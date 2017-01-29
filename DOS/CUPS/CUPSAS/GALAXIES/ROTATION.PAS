    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1994.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program to animate the rotation of a          *)
    (*       galaxy and to display its velocity            *)
    (*       curve.                                        *)
    (*                                                     *)
    (*******************************************************)

Unit Rotation;
{$M 64000,0,655360}
{$O+}
{$F+}
interface
procedure RunRotation;

implementation
USES Crt, Graph, CUPSMupp, CUPS, CupsGui, CUPSproc, CUPSFunc;

{procedure RunRotation;}
{Units: Mass:   10^9 solar masses.
        Length: Kiloparsec.
        Time:   10^6 years.}
Type
  Sbase = ARRAY[1..1] OF single;
  SbasePtr =^Sbase;

  SVector = OBJECT
    VecPtr   : SbasePtr;               {This points to the DVector}
    Size     : INTEGER;                {This is the number of elements used}
    MemBlock : LongInt;
    PROCEDURE Init( NumberElements: INTEGER);
    PROCEDURE Free;                                {This releases the memory}
    PROCEDURE Put( Index: INTEGER;     X: REAL);   {Store X at Index}
    FUNCTION  Value( Index: INTEGER) : REAL;       {Returns value at Index}
  END;

PROCEDURE SVector.Init (NumberElements: INTEGER);
 VAR
    i: INTEGER;
    MemReq,MaxBlock: String[10];
BEGIN
   Size := NumberElements;
   MemBlock:= longint(Size)*SizeOf(single);
   IF ((MemBlock > MaxAvail) OR (MemBlock >= 65521)) THEN
   BEGIN
     Str(MemBlock:0,MemReq);
     Str(MaxAvail:0,MaxBlock);
     Error('SVector.Init: Request for '+ MemReq +
               ' exceeds free memory ' + MaxBlock + 'or 65521');
     MemBlock := 0;
     VecPtr   := NIL;
     Size     := 0;
   END
   ELSE
     IF size=0 THEN VecPtr:=nil ELSE
     BEGIN
       GetMem(VecPtr,MemBlock);
       FOR i:=1 TO Size DO VecPtr^[i]:=0.0
     END
 END;

PROCEDURE SVector.Free;
BEGIN
   FreeMem(VecPtr,MemBlock);
   MemBlock := 0;
   VecPtr := NIL;
   Size:= 0;
END;

PROCEDURE SVector.Put(Index   : INTEGER;  {input}
                         X     : REAL   {input});
BEGIN
{$IFOPT D+}
   IF (Index>Size) OR (Index<1) THEN
     Error('SVector.Put: Index exceeds length of the vector');
{$ENDIF}
   VecPtr^[Index]:=X;
END;

FUNCTION  SVector.Value(Index: INTEGER): REAL;
BEGIN
{$IFOPT D+}
   IF (Index>Size) OR (Index<1) THEN
     Error('SVector.Value: Index exceeds length of the vector');
{$ENDIF}
   Value:=VecPtr^[Index]
END;





procedure RunRotation;
TYPE
   VelVector = Array[1..100] of Real;
   DatVector = Array[1..2200] of Single;
   IntVector = Array[1..100] of Integer;


VAR
   G : Real; {Constant of gravitation.}
   Aspect : Real; {Parameter for horizontal and vertical display.}
   Dt : Real; {Time interval between frames for rotation.}
   Tick          : Longint;  {Useless parameter for Mac Delay.}
   CircVel, VisibleCircVel, CircRad     : VelVector;
   CosDel, SinDel                       : VelVector;
   Rad, Angle, CosAngle, SinAngle, Speed: SVector;
   Num                                  : IntVector;
   SphVis                               : Array[1..5] OF Integer;
   SphHomogeneous                       : Array[1..5] OF Boolean;
   SphMaj, SphMin, SphMass              : Array[1..5] OF Real;
   RotateColors                         : Array[1..2200] OF Word;
   CMass                                : Real; {Mass of center.}
   GalRadius                            : Real; {Radius of the visible galaxy.}
   nStar                                : Integer; {Number of visible stars.}
   i                                    : Integer;
   NumSph                               : Integer; {Number of spheroids.}
   SphNum                               : Real;
   RotateMenu                           : TMenu;
   GalaxyMade                           : Boolean;
   HotKeys                              : THotKeys;
   CUPSKeys                             : THotKeys;
   ShowCUPSKeys                         : Boolean;
   Key                                  : Byte;
   Rotate                               : Boolean;
   Action                               : Boolean;
   Start                                : Boolean; {True only during initial demo StartUp.}
   ExitRequest                          : Boolean;
   LightMenu                            : Boolean;
   Picture                              : TFrames;

{-------------------------------------------------------------------------}

PROCEDURE SetUpRotateMenu;
BEGIN
   {New(RotateMenu);}
   With RotateMenu DO
   BEGIN
      Init;
      Column(1, 'File');
       row(1,1, 'About CUPS');
       row(1,2, 'About Program');
       row(1,3, 'Configuration');
       row(1,4, 'Exit this module');
      Column(2, 'Galaxy');
       row(2,1, 'Enter data for a galaxy');
       row(2,2, 'Replot the galaxy');
       row(2,3, 'Plot default galaxy');
      Column(3, 'Rotate');
       row(3,1, 'See the rotation of the galaxy');
      Column(4, 'Profile');
       row(4,1, 'See the galactic velocity profile');
   END;
   RotateMenu.Display;
END;

{---------------------------------------------------------------}

PROCEDURE SetUpHotKeys;
BEGIN
   {New(HotKeys);}
   WITH HotKeys DO
   BEGIN
      HotKeys.Init(2);
      Key[1] := 'F2-Stop';
      Key[2] := 'F10-Menu';
   END;
   HotKeys.Display;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleHotKeys(key: Byte);
BEGIN
   CASE Key OF
      1: BEGIN
            IF Action THEN
            BEGIN
               WITH HotKeys DO Key[1] := 'F2-Run';
               HotKeys.BackColor := Red;
               picture.pause;
               HotKeys.Display;
            END
            ELSE
            BEGIN
               WITH HotKeys DO Key[1] := 'F2-Stop';
               HotKeys.BackColor := Blue;
               HotKeys.Display;
               picture.continue;
            END;
            action := not action;
         END;
      2: BEGIN
            Rotate := False;
            Action := False;
            Picture.Close;
            HotKeys.Clear;
            ShowCUPSKeys := True;
            CUPSKeys.Display;
            LightMenu := True;
            ShowMouse;
         END;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpCUPSKeys;
BEGIN
    WITH CUPSKeys DO
    BEGIN
       CUPSKeys.Init(2);
       Key[1] := 'F1-Help';
       Key[2] := 'F10-Menu';
    END;
END;

{---------------------------------------------------------------}

{PROCEDURE HandleHotKeys(key: Byte);
BEGIN
   CASE Key OF
      1: Rotate := False;
   END;
END;}

{------------------------------------------------------------------------}

{---------Procedure for setting up a galaxy-------------}

Procedure MakeGalaxy(VAR CircVel, CircRad: VelVector;
                     VAR Rad, Angle, Speed: SVector;
                     VAR GalRadius: Real;
                     VAR nStar: Integer;
                     VAR Num: IntVector;
                     VAR GalaxyMade: Boolean);

TYPE
   GalVector = Array[1..5] of Real;
   VisVector = Array[1..5] of Integer;

VAR
   VisMass                  : Real; {Mass of the visible elements.}
   i                        : Integer;
   DataOK                   : Boolean;
   InputAccepted            : Boolean;

FUNCTION GalData1 : Boolean;
VAR
   GalDat1 : TInputScreen;
BEGIN
   {New(GalDat1);}
   ClearMuppetPort;
   WITH GalDat1 DO
   BEGIN
      Init;
      DefineInputPort(0.05, 0.95, 0.1, 0.85);
      LoadLine('                  Data for making a galaxy.');
      LoadLine('The galaxy can be constructed from a central mass, and up to five');
      LoadLine('homogeneous spheroids. A spheroid can be made up of either visible');
      LoadLine('invisible matter, and can be homogeneous or non-homogeneous.');
      LoadLine('          The unit of mass is 10^9 solar masses.');
      LoadLine('          The unit of length is one kiloparsec.');
      LoadLine('          The unit of time is 10^6 years.');
      LoadLine('          Enter the central mass: {      } >= 0');
      LoadLine('          Enter the number of spheroids: 1 <= {  } <= 5');
      LoadLine('');
      LoadLIne('                  [  OK  ]             [Cancel]');
      SetNumber(1, CMass);
      SetNumberLimits(1, 0, 10000);
      SetNumber(2, SphNum);
      SetNumberLimits(2, 1, 5);
      AcceptScreen;
      GalData1 := NOT Canceled;
      IF NOT Canceled THEN
      BEGIN
         InputAccepted := True;
         CMass := GetNumber(1);
         SphNum := GetNumber(2);         {Real.}
         NumSph := Trunc(SphNum + 0.001);    {Integer.}
      END;
      Done;
   END;
   {Dispose(GalDat1);}
END;


Procedure GalData2;
VAR
   RBNames: String ;
   i: integer;
   GalDat2 : TInputScreen;

BEGIN
   {New(GalDat2);}
   RBNames := '123456789';
   WITH GalDat2 DO
   BEGIN
      Init;
      DefineInputPort(0.05, 0.95, 0.44 - 0.088*NumSph, 0.56 + 0.088*NumSph);
      LoadLine(' Data for the spheroids.    Major axes should exceed minor axes.');
      LoadLine(' Spheroid 1:  Semimajor axis = {     }>=1 Semiminor axis = {     }');
      LoadLine('       Mass = {     }>0    Visible #1  Invisible #1 ');
      LoadLine('       Non-homogeneous?        Yes #6         No #6 ');
      SetNumber(1, SphMaj[1]);
      SetNumberLimits(1, 1, 10000);
      SetNumber(2, SphMin[1]);
      SetNumberLimits(2, 1, 10000);
      SetNumber(3, SphMass[1]);
      SetNumberLimits(3, 0, 10000);
      IF SphNum > 1 THEN
      BEGIN
         LoadLine(' Spheroid 2:  Semimajor axis = {     }>=1 Semiminor axis = {     }');
         LoadLine('       Mass = {     }>0    Visible #2  Invisible #2 ');
         LoadLine('       Non-homogeneous?        Yes #7         No #7 ');
         SetNumber(8, SphMaj[2]);
         SetNumberLimits(8, 1, 10000);
         SetNumber(9, SphMin[2]);
         SetNumberLimits(9, 1, 10000);
         SetNumber(10, SphMass[2]);
         SetNumberLimits(10, 0.0001, 10000);
      END;
         IF SphNum > 2 THEN
      BEGIN
         LoadLine(' Spheroid 3:  Semimajor axis = {     }>=1 Semiminor axis = {     }');
         LoadLine('       Mass = {     }>0    Visible #3  Invisible #3 ');
         LoadLine('       Non-homogeneous?        Yes #8         No #8 ');
         SetNumber(15, SphMaj[3]);
         SetNumberLimits(15, 1, 10000);
         SetNumber(16, SphMin[3]);
         SetNumberLimits(16, 1, 10000);
         SetNumber(17, SphMass[3]);
         SetNumberLimits(17, 0.0001, 10000);
      END;
      IF SphNum > 3 THEN
      BEGIN
         LoadLine(' Spheroid 4:  Semimajor axis = {     }>=1 Semiminor axis = {     }');
         LoadLine('       Mass = {     }>0    Visible #4  Invisible #4 ');
         LoadLine('       Non-homogeneous?        Yes #9         No #9 ');
         SetNumber(22, SphMaj[4]);
         SetNumberLimits(22, 1, 10000);
         SetNumber(23, SphMin[4]);
         SetNumberLimits(23, 1, 10000);
         SetNumber(24, SphMass[4]);
         SetNumberLimits(24, 0.0001, 10000);
      END;
      IF SphNum > 4 THEN
      BEGIN
         LoadLine(' Spheroid 5:  Semimajor axis = {     }>=1 Semiminor axis = {     }');
         LoadLine('       Mass = {     }>0    Visible #5  Invisible #5 ');
         LoadLine('       Non-homogeneous?        Yes #A         No #A ');
         SetNumber(29, SphMaj[5]);
         SetNumberLimits(29, 1, 10000);
         SetNumber(30, SphMin[5]);
         SetNumberLimits(30, 1, 10000);
         SetNumber(31, SphMass[5]);
         SetNumberLimits(31, 0.0001, 10000);
      END;
      LoadLine('                [  OK  ]          [Cancel]');
      AcceptScreen;
      IF Canceled THEN InputAccepted := False
      ELSE
      BEGIN
         InputAccepted := True;
         VisMass := 0;
         GalRadius := 0;
         FOR i := 1 TO NumSph DO
         BEGIN
            SphMaj[i] := GetNumber(7*i-6);
            SphMin[i] := GetNumber(7*i-5);
            SphMass[i] := GetNumber(7*i-4);
            IF GetBoolean(7*i-3) = True THEN
            SphHomogeneous[i] := False ELSE SphHomogeneous[i] := True;
            IF GetRadioButton(RBNames[i]) = 1 THEN
            BEGIN
               SphVis[i] := 1;
               VisMass := VisMass + SphMass[i];
            END
            ELSE SphVis[i] := 0;
            IF (SphMaj[i] > GalRadius) AND (SphVis[i] = 1)
               THEN GalRadius := SphMaj[i];
         END;
         GalRadius := 1.1*GalRadius;
      END;
      Done;
   END;
   {Dispose(GalDat2);}
END;


PROCEDURE TestData2;
VAR
   i: Integer;
BEGIN
   DataOK := True;
   FOR i := 1 TO NumSph DO
   BEGIN
      IF (SphMaj[i] < 0)
      OR (SphMin[i] >= SphMaj[i])
      OR (SphMass[i] < 0)
      THEN DataOK := False;
   END;
END;

Procedure Initialize;
VAR
   nRad, n, j                    : Integer;
   NewColor                      : Integer;
   Radius, dRadius, CPrime       : Real;
   dMass                         : Real;
   Force                         : Real;
   Th0                           : Real;
   Inside                        : Boolean;
   Density, Eccentricity         : Array[1..5] OF Real;

FUNCTION HomForce(Ecc, Dens, Sma, Rad : Real) : Real;
VAR
   Factor  : Real;
   Alpha   : Real;
BEGIN
   Factor := G*2*Pi*(Sqrt(1 - Sqr(Ecc))/(Ecc*Ecc*Ecc))*Rad*Dens;
   IF Inside THEN Alpha := Ecc ELSE Alpha := Sma*Ecc/Rad;
   HomForce := Factor*
      (ArcTan(Alpha/Sqrt(1-Alpha*Alpha)) - Alpha*Sqrt(1 - Alpha*Alpha));
END;

FUNCTION NonHomForce(Ecc, Dens, Sma, Rad : Real) : Real;
VAR
   Factor  : Real;
   Alpha   : Real;
BEGIN
   Factor := G*4*Pi*(Sqrt(1 - Sqr(Ecc))/(Ecc*Ecc*Ecc))*Rad*Dens;
   IF Inside THEN Alpha := Ecc ELSE Alpha := Sma*Ecc/Rad;
   NonHomForce := Factor*
      (((- Sma*Ecc/Rad) + (Rad/(Sma*Ecc)))*Sqrt(1 - Alpha*Alpha)
       - (Rad/(3*Sma*Ecc))*Sqrt(1 - Alpha*Alpha)*(1 - Alpha*Alpha)
       + ((Sma*Ecc/Rad) - (Rad/(Sma*Ecc))) + Rad/(3*Sma*Ecc));
END;

BEGIN
   {Find densities and eccentricities of the spheroids.}
   FOR j := 1 TO NumSph DO
   BEGIN
      Eccentricity[j] := Sqrt(1.00001 - Sqr(SphMin[j]/SphMaj[j]));
      Density[j] := SphMass[j]/(Pi*SphMaj[j]*SphMaj[j]*SphMin[j]);
      IF SphHomogeneous[j] THEN Density[j] := 0.75*Density[j];
   END;
   nStar := 0;
   dRadius := GalRadius*(1/101);
   FOR nRad := 1 TO 100 DO
   BEGIN
      Radius := dRadius*(nRad + 0.5);
      CircRad[nRad] := Radius;
{Find the number of visible stars between Radius and (Radius + dRadius).}
         Num[nRad] := 0;
         FOR j := 1 TO NumSph DO
         BEGIN
            IF SphVis[j] = 1 THEN
            BEGIN
               IF Radius < SphMaj[j] THEN
               BEGIN
                  IF SphHomogeneous[j] THEN
                  dMass := (3)*(Radius/Sqr(SphMaj[j]))
                                *Sqrt(1 - Sqr(Radius/SphMaj[j]))*dRadius
                                *SphMass[j]
                  ELSE
                  dMass := 4*Pi*Density[j]*Radius*dRadius*SphMin[j]
                           *((1 - 0.5*Sqr(Radius/SphMaj[j]))
                             *Ln((Sqrt(1.00001 - Sqr(Radius/SphMaj[j])) + 1)
                                  /(Radius/SphMaj[j]))
                             -0.5*Sqrt(1 - Sqr(Radius/SphMaj[j])));
                  Num[nRad] := Num[nRad] + Trunc((dMass/VisMass)*2200);
               END;
            END;
         END;
{Find the radial force at distance Radius, and the circular velocity.}
      Force := G*CMass/Sqr(Radius); {Component due to the central mass.}
      FOR j := 1 TO NumSph DO
      BEGIN
         IF Radius < SphMaj[j] THEN Inside := True ELSE Inside := False;
         IF SphHomogeneous[j] THEN Force := Force +
                                HomForce(Eccentricity[j], Density[j],
                                         SphMaj[j], Radius)
                              ELSE Force := Force +
                                NonHomForce(Eccentricity[j], Density[j],
                                         SphMaj[j], Radius);
      END;
      CircVel[nRad] := Sqrt(Force/Radius); {Angular velocity.}
      CosDel[nRad] := Cos(Dt*CircVel[nRad]);
      SinDel[nRad] := Sin(Dt*CircVel[nRad]);
{Find initial conditions.}
      Th0 := Random*2*Pi;
      {Th0 := Random_*2*Pi;}
      FOR j := 1 TO Num[nRad] DO
      BEGIN
         Rad.put(nStar + j, Radius);
         Speed.put(nStar + j, CircVel[nRad]);
         Angle.put(nStar + j, Th0 + j*2*3.14159/Num[nRad]);
         NewColor := Trunc(Angle.value(nStar + j)*(32/Pi));
         NewColor := NewColor - 16*Trunc(NewColor/16);
         RotateColors[nStar + j] := NewColor;
         CosAngle.put(nStar + j, Radius*Cos(Angle.value(nStar + j)) );
         SinAngle.put(nStar + j, Radius*Sin(Angle.value(nStar + j)) );
      END;
      nStar := nStar + Num[nRad];
   END; {nRad loop.}
   DT := Pi/(100*CircVel[50]);
   FOR nRad := 1 TO 100 DO
   BEGIN
      CosDel[nRad] := Cos(Dt*CircVel[nRad]);
      SinDel[nRad] := Sin(Dt*CircVel[nRad]);
   END;
END; {Procedure Initialize.}


BEGIN {Main program for MakeGalaxy.}
IF NOT Start THEN
BEGIN
   Message('');
      IF NOT GalData1 THEN
      BEGIN
         GalaxyMade := False;
         Exit;
         {Exit(MakeGalaxy);}
      END;
   IF NumSph > 0 THEN
   BEGIN
      Message('');
      REPEAT
         GalData2;
         IF NOT InputAccepted THEN
         BEGIN
            GalaxyMade := False;
            Exit;
            {Exit(MakeGalaxy);}
         END;
         TestData2;
         IF DataOK = False THEN
         BEGIN
            Beep;
            Message('Try again');
         END;
      UNTIL DataOK = True;
   END;
END
ELSE
BEGIN
   CMass := 10;
   SphNum := 5;      {Real.}
   NumSph := 5; {Integer.}
   FOR i := 1 TO NumSph DO
   BEGIN
      SphMaj[i] := 2*i;
      SphMin[i] := i;
      SphMass[i] := i;
      SphVis[i] := 1;
   END;
   VisMass := 20;
   GalRadius := 11;
END;
   Initialize;
   GalaxyMade := True;
   DefineScale(1, -1.2*GalRadius, 1.2*GalRadius,
                          (-1.2*GalRadius)*Aspect, (1.2*Galradius)*Aspect);
END;

{----------------End of procedure for setting up a galaxy---------------}


{-----------------Procedure for rotating a galaxy-----------------------}

PROCEDURE StarPlot(xg, yg: real; GalColor: integer);
var x,y : integer;
BEGIN
   Map(xg,yg,x,y);
   PutPixel(x,y,galColor);
   PutPixel(x+1,y,galColor);
   PutPixel(x-1,y,galColor);
   PutPixel(x,y+1,galColor);
   PutPixel(x,y-1,galColor);
END;

{-----------------------------------------------------------------------}

Procedure MarkScale;
VAR
   nS, nScale, nMark: Integer;
   DelScale         : Integer;
   Scalen: String;
   xCenter, yCenter: Integer;
   dx, dy: Real;
BEGIN
   Map(0, 0, xCenter, yCenter);
   dx := xCenter/(GalRadius)*Aspect;
   dy := yCenter/(GalRadius); 
   nMark := Trunc(GalRadius/10) + 1;
   SetColor(Cyan);
   Line(5, 0, 5, GetMaxY);
   {LineTP(5, 0, 5, GetMaxY);}
   nScale := Trunc(GalRadius);
   DelScale := 1;
   IF nScale > 10 THEN DelScale := 2;
   IF nScale > 20 THEN DelScale := 5;
   IF nScale > 50 THEN DelScale := 10;
   IF nScale > 100 THEN DelScale := 100;
   nScale := Trunc(GalRadius/DelScale);
   dy := dy*DelScale;
   Line(5, yCenter, 10, yCenter);
   {LineTP(5, yCenter, 10, yCenter);}
   OutTextxy(30, yCenter, '0');
   For nS := 1 TO nScale DO
   BEGIN
      {IF (nS/nMark) = Trunc(nS/nMark) THEN}
      BEGIN
         Line(5, yCenter - Trunc(nS*dy), 10, yCenter - Trunc(nS*dy));
         {LineTP(5, yCenter - Trunc(nS*dy), 10, yCenter - Trunc(nS*dy));}
         Scalen := NumStr(nS*DelScale, 3, 0);
         OutTextxy(15, yCenter - Trunc(nS*dy), Scalen);
         Line(5, yCenter + Trunc(nS*dy), 10, yCenter + Trunc(nS*dy));
         {LineTP(5, yCenter + Trunc(nS*dy), 10, yCenter + Trunc(nS*dy));}
         Scalen := NumStr(-nS*DelScale, 3, 0);
         OutTextxy(15, yCenter + Trunc(nS*dy), Scalen);
      END;
      IF nS = nScale THEN OutTextxy(40, yCenter + Trunc((nS-0.5)*dy), 'Kpc');
   END;
END;


Procedure PlotRotatingGalaxy;
VAR       
   i, j, k, StarColor: Integer;
   DelAngle: Real;
   Save: Real;
   SinTHeta, CosTheta: Real;
   xCenter, yCenter: Integer;
BEGIN
 {  RotateMenu.Display;}
   HotKeys.Display;
   HideMouse;
   SetColor(Blue);
   GraphBackColor := Blakk;
   OpenViewPort(1);
   Map(0, 0, xCenter, yCenter);
   MarkScale;
   SetColor(Blue);
   SetFillStyle(1, Blue);
   FillEllipse(xCenter, yCenter, 5, 4);
   StarColor := 0;
   k := 0;
   FOR i := 1 TO 100 DO
   BEGIN
      FOR j := 1 TO Num[i] DO
      BEGIN
         k := k + 1;
         Save := CosAngle.value(k);
         CosAngle.put(k, CosAngle.value(k)*CosDel[i]
                       - SinAngle.value(k)*SinDel[i] );
         SinAngle.put(k, SinAngle.value(k)*CosDel[i] + Save*SinDel[i] );
         {StarColor := StarColor + 1; IF StarColor = 16 THEN StarColor := 1;}
         SetColor(RotateColors[k]);
         {PlotSymbol(CosAngle[k], SinAngle[k], '.');}
         StarPlot(CosAngle.value(k), SinAngle.value(k), RotateColors[k]);
      END;
   END;
   SetColor(Blue);
   SetFillStyle(1, Blue);
   FillEllipse(xCenter, yCenter, 5, 4);
   ShowMouse;
END;

PROCEDURE PrepareForRotation;
BEGIN
   ShowCUPSKeys := False;
   CUPSKeys.Clear;
   SetUpHotKeys;
   HideMouse;
   Message('');
   SelectScale(1);
   Picture.Select(1);
   Rotate := True;
   Action := True;
   {RotateMenu.Display;}
   Picture.SwitchAndClean;
END;

{--------------------End of procedure for rotating a galaxy-------------}

Procedure PlotRV(CircVel, CircRad: VelVector; Num: IntVector; GalRadius: Real);

VAR
   MaxRV, MinRV : Real;
   NumData      : Integer;
   i            : Integer;
   x, y         : DataVector;
   xSc, ySc     : Integer;
   YTick, XTick : Integer;

BEGIN
   MaxRV := 0;
   FOR i := 3 TO 100 DO
   BEGIN
      x[i] := CircRad[i];
      y[i] := 977*CircRad[i]*CircVel[i];
      IF MaxRV < y[i] THEN MaxRV := y[i];
   END;
   MinRV := MaxRV;
   FOR i := 3 TO 100 DO IF MinRV > y[i] THEN MinRV := y[i];
   YTick := 10;
   IF MaxRV - MinRV > 190 THEN YTick := 100;
   IF MaxRV - MinRV > 1900 THEN YTick := 1000;
   IF MaxRV - MinRV > 19000 THEN YTick := 10000;
   MinRV := YTick*Trunc(MinRV/YTick);
   XTick := 5;
   IF GalRadius > 50 THEN Tick := 10;
   IF GalRadius > 190 THEN XTick := 100;
   DefineViewPort(2, 0.5, 1, 0.15, 0.9);
   DefineScale(2, 0, 1.2*GalRadius,
               YTick*Trunc(MinRV/YTick), MaxRV + (MaxRV - MinRV)/10);
   GraphBackColor := Blue;
   HideMouse;
   SelectViewPort(2);
   SelectScale(2);
   SetColor(Yellow);
   Map(x[3]/2, MaxRV+5, xSc, ySc);
   OutTextxy(xSc, ySc, 'km/sec');
   Map(x[90], MinRV, xSc, ySc);
   OutTextxy(xSc, ySc-20, 'Kpc');
   Map(x[50], MaxRV+5, xSc, ySc);
   OutTextxy(xSc, ySc, 'Velocity profile');
   Axis(0, YTick*Trunc(MinRV/YTick), xTick, yTick);
   SetColor(Whitte);
   PlotLine(0, 0, 0, MaxRV);
   SetColor(Yellow);
   FOR i := 4 TO 100 DO
      IF CircRad[i] < GalRadius/1.1 THEN PlotLine(x[i-1], y[i-1], x[i], y[i]);
   ShowMouse;
   LightMenu := True;
END;

{------------------------------------------------------------------------}

Procedure PlotGalaxy;

VAR
   i, StarColor, xSc, ySc: Integer;
BEGIN
   StarColor := 0;
   GraphBackColor := Blakk;
   HideMouse;
   SetColor(Blue);
   OpenViewPort(1);
   SelectScale(1);
   ShowCUPSKeys := True;
   CUPSKeys.Display;
   FOR i := 1 TO nStar DO
   BEGIN
      StarColor := StarColor + 1; IF StarColor = 16 THEN StarColor := 1;
      SetColor(RotateColors[i]);
      PlotSymbol(CosAngle.value(i), SinAngle.value(i), '.');
   END;
   SetFillStyle(1, Blue);
   Map(0, 0, xSc, ySc);
   FillEllipse(xSc, ySc, 5, 4);
   ShowMouse;
END;

{------------------------------------------------------------------------}

PROCEDURE ReAllocateColors;
VAR
   Th0          : Real;
   Radius       : Real;
   Angle        : Real;
   nRad, j      : Integer;
   NewColor     : Integer;
   StarCount    : Integer;
BEGIN
   StarCount := 0;
   FOR nRad := 1 TO 100 DO
   BEGIN
      Radius := GalRadius*(1/101)*(nRad + 0.5);
      Th0 := Random*2*Pi;
      FOR j := 1 TO Num[nRad] DO
      BEGIN
         StarCount := StarCount + 1;
         Angle := Th0 + j*2*Pi/Num[nRad];
         NewColor := Trunc(Angle*(32/Pi));
         NewColor := NewColor - 16*Trunc(NewColor/16);
         RotateColors[StarCount] := NewColor;
         CosAngle.put(StarCount, Radius*Cos(Angle) );
         SinAngle.put(StarCount, Radius*Sin(Angle) );
      END;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE PlotDefaultGalaxy;
var i : integer;
BEGIN
   NumSph := 5;
   SphNum := 5;
   CMass := 10;
   FOR i := 1 TO 5 DO
   BEGIN
      SphMaj[i] := 2*i;
      SphMin[i] := i;
      SphMass[i] := i;
      SphVis[i] := 1;
      SphHomogeneous[i] := False;
   END;
   Start := True;
   MakeGalaxy(CircVel, CircRad, Rad, Angle, Speed, GalRadius, nStar, Num,
               GalaxyMade);
   RotateMenu.RowActivate(2,2, True);
   Rotatemenu.RowActivate(3,1, True);
   RotateMenu.RowActivate(4,1, True);
   PlotGalaxy;
   Start := False;
   LightMenu := True;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleRotateMenu;

BEGIN
   With RotateMenu DO
   CASE ColChosen OF
      1: CASE RowChosen OF
            1: BEGIN
                  AboutCUPS;
                  LightMenu := True;
               END;
            2: BEGIN
                  ShowHelp('HelpGals.HLP', 'Rotation Hello');
                  LightMenu := True;
               END;
            3: BEGIN
                  Configuration;
                  LightMenu := True;
               END;
            4: ExitRequest := True;
         END;
      2: CASE RowChosen OF
            1: BEGIN
                  MakeGalaxy(CircVel, CircRad, Rad, Angle, Speed, GalRadius,
                     nStar, Num, GalaxyMade);
                  RotateMenu.Display;
                  LightMenu := True;
                  IF GalaxyMade THEN
                  BEGIN
                     RotateMenu.RowActivate(2,2, True);
                     Rotatemenu.RowActivate(3,1, True);
                     RotateMenu.RowActivate(4,1, True);
                     PlotGalaxy;
                  END
                  ELSE
                  BEGIN
                     Announce('Data must be complete for galaxy to be shown');
                     RotateMenu.RowActivate(2,2, False);
                     Rotatemenu.RowActivate(3,1, False);
                     RotateMenu.RowActivate(4,1, False);
                     Exit; {Exit(HandleRotateMenu);}
                  END;
              END;
            2: BEGIN
                  ReAllocateColors;
                  PlotGalaxy;
               END;
            3: PlotDefaultGalaxy;
         END;
      3: CASE RowChosen OF
            1: PrepareForRotation;
         END;
      4: CASE RowChosen OF
            1: PlotRV(CircVel, CircRad, Num, GalRadius);
         END;
    END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleCUPSKeys(Key: Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HelpGals.HLP', 'Rotation Hello');
      2: IF RotateMenu.Chosen THEN HandleRotateMenu;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE StartUp;
VAR
   i : Integer;
BEGIN
   DefineViewPort(1, 0, 1, 0.08, 0.93);
   G := 0.00449897; {Constant of gravitation.}
   Aspect := 0.7; {Parameter for horizontal and vertical display.}
   Dt := 0.1; {Time interval between frames for rotation.}
   SetUpRotateMenu;
   SetUpCUPSKeys;
   CUPSKeys.Display;
   ShowCUPSKeys := True;
   NumSph := 5;
   SphNum := 5;
   CMass := 10;
   FOR i := 1 TO 5 DO
   BEGIN
      SphMaj[i] := 2*i;
      SphMin[i] := i;
      SphMass[i] := i;
      SphVis[i] := 1;
      SphHomogeneous[i] := False;
   END;
   Start := True; {Only true for this procedure.}
   Rad.init(2200);
   Angle.init(2200);
   CosAngle.init(2200);
   SinAngle.init(2200);
   Speed.init(2200);
   MakeGalaxy(CircVel, CircRad, Rad, Angle, Speed, GalRadius, nStar, Num,
               GalaxyMade);
   PlotGalaxy;
   Start := False;
   Rotate := False;
   ExitRequest := False;
   LightMenu := True;
END;

Procedure CleanUp;
begin
   Rad.free;
   Angle.free;
   CosAngle.free;
   SinAngle.free;
   Speed.free;
end;


{------------------------------------------------------------------------}

BEGIN   {Main Program.}
   {New(Picture);}
   Picture.Init;
   SetGraphMode(VGAmed);
{  CUPSinit;}
   ShowMouse;
   StartUp;
   Delay(1000);
   {Delay(3000 div 62,Tick);}
   ShowHelp('HelpGals.HLP', 'Rotation Hello');
   REPEAT
      CheckForEvents;
      IF ShowCUPSKeys THEN
              IF CUPSKeys.Pressed(Key) THEN HandleCUPSKeys(Key);
      IF Rotate AND Action THEN
      BEGIN
         PlotRotatingGalaxy;
         Picture.Switch;
      END;
      IF Rotate THEN
         IF HotKeys.Pressed(Key) THEN HandleHotKeys(Key);
      IF LightMenu THEN
      BEGIN
         LightMenu := False;
         IF RotateMenu.Chosen THEN HandleRotateMenu;
      END
      ELSE
         IF RotateMenu.Activated THEN HandleRotateMenu;
   UNTIL ExitRequest;
   RotateMenu.Done;
{  CUPSDone;}
   SetGraphMode(VGAhi);
   CleanUp;
   ShowMouse;
   {Dispose(RotateMenu);}
   {Dispose(HotKeys);}
   {Dispose(Picture);}
end;

BEGIN
END.