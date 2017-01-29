    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1994.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program to display the differential           *)
    (*       rotation and velocity fields near the         *)
    (*       "Sun", with the variations of radial          *)
    (*       velocities and proper motions with distance.  *)
    (*       These are compared with the linear            *)
    (*       approximations and the Oort constants.        *)
    (*                                                     *)
    (*******************************************************)

Unit OortCons;
{$M 64000,0,327680}
{$O+}
{$F+}
interface
procedure RunOortCons;

implementation
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;


{Units: Mass:   10^9 solar masses.
        Length: Kiloparsec.
        Time:   10^6 years.}

procedure RunOortCons;
TYPE
   VelVector = Array[1..100] of Real;
   DatVector = Array[1..2200] of Single;
   IntVector = Array[1..100] of Integer;
   ArmArray = Array[1..5, 0..50] of Single;
   MaxArm = Array[1..5] of Integer;

VAR
   G : Real; {Constant of gravitation.}
   Aspect : Real; {Adjusts for vertical and horizontal scales on screen.}
   Tick          : Longint;  {Useless parameter for Mac Delay.}
   CircVel, CircRad: VelVector;
   Rad, Angle, CosAngle, SinAngle, Speed: DatVector;
   GalRadius                            :  Real; {Radius of the visible galaxy.}
   R0: Real; {Distance of the Sun from the galactic center.}
   nStar: Integer; {Number of visible stars.}
   Num                                  : IntVector;
   SphVis                               : Array[1..5] OF Integer;
   SphHomogeneous                       : Array[1..5] OF Boolean;
   SphMaj, SphMin, SphMass              : Array[1..5] OF Real;
   CMass                                : Real; {Mass of center.}
   NumSph                               : Integer; {Number of spheroids.}
   SphNum                               : Real;
   xSun, ySun                           : Real;
   LocalRad                             : Real;
   dx, dy                               : Real;
   xCenter, yCenter                     : Integer;
   GalaxyMade, SunLocated               : Boolean;
   OortMenu                             : TMenu;
   Picture                              : TFrames;
   HotKeys, FieldHotKeys                : THotKeys;
   CUPSKeys                             : THotKeys;
   Key                                  : Byte;
   ShowCUPSKeys                         : Boolean;
   Start                                : Boolean;
   Action                               : Boolean;
   SeeRotation                          : Boolean;
   LightMenu                            : Boolean;
   ExitRequest                          : Boolean;
   ShowConstants                        : Boolean;
   FieldNumber                          : Integer;

PROCEDURE HandleOortMenu; FORWARD;
PROCEDURE HandleFieldHotKeys(Key: Byte); FORWARD;

{------------------------------------------------------------------------}


PROCEDURE SetUpOortMenu;
BEGIN
   {New(OortMenu);}
   With OortMenu DO
   BEGIN
      Init;
      Column(1, 'File');
       row(1,1, 'About CUPS');
       row(1,2, 'About Program');
       row(1,3, 'Configuration');
       row(1,4, 'General information');
       row(1,5, 'Exit this module');
      Column(2, 'Galaxy');
       row(2,1, 'Enter data for a galaxy');
       row(2,2, 'See the galaxy for current data');
       row(2,3, 'See the default galaxy');
      Column(3, 'Location');
       row(3,1, 'Choose a local region');
       row(3,2, 'See differential rotation');
       row(3,3, 'See velocity fields');
      Column(4, 'Oort');
       row(4,1, 'Radial velocities');
       row(4,2, 'Proper motions');
   END;
   OortMenu.Display;
END;
{-------------------------------------------------------}

PROCEDURE SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      HotKeys.Init(2);
      Key[1] := 'F2-Stop';
      Key[2] := 'F10-Menu';
   END;
   HotKeys.Display;
END;

{-------------------------------------------------------------------------}

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
            SeeRotation := False;
            Action := False;
            ShowMouse;
         END;
   END;
END;

{-------------------------------------------------------------------------}

PROCEDURE SetUpFieldHotKeys;
BEGIN
   {New(FieldHotKeys);}
   WITH FieldHotKeys DO
   BEGIN
      Init(6);
      Key[1] := 'F1-Help';
      Key[2] := 'F2-Velocity';
      Key[3] := 'F3-Radial';
      Key[4] := 'F4-Transverse';
      Key[5] := 'F5-Pr-Motion';
      Key[6] := 'F10-Menu';
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

{-------------------------------------------------------------------------}
{----------------Procedure for setting up a galaxy------------------------}

Procedure MakeGalaxy(VAR CircVel, CircRad: VelVector;
                     VAR Rad, Angle, Speed: DatVector;
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
   WITH GalDat1 DO
   BEGIN
      Init;
      DefineInputPort(0.05, 0.95, 0.22, 0.78);
      LoadLine('');
      LoadLine('                      Data for making a galaxy.');
      LoadLine(' The galaxy can be constructed from a central mass, and up to five');
      LoadLine(' homogeneous spheroids. A spheroid can be made up of either visible');
      LoadLine(' invisible matter, and can be homogeneous or non-homogeneous.');
      LoadLine('          The unit of mass is 10^9 solar masses.');
      LoadLine('          The unit of length is one kiloparsec.');
      LoadLine('          The unit of time is 10^6 years.');
      LoadLine('          Enter the central mass: {      } >= 0');
      LoadLine('          Enter the number of spheroids: 1 <= {  } <= 5');
      LoadLine('');
      LoadLIne('                    [  OK  ]             [Cancel]');
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
      DefineInputPort(0.05, 0.95, 0.4 - 0.068*NumSph, 0.6 + 0.068*NumSph);
      LoadLine('                    Data for the spheroids.');
      LoadLine(' Major axes should exceed minor axes.');
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
      LoadLine('');
      LoadLine('                   [  OK  ]          [Cancel]');
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
{Find initial conditions.}
      Th0 := Random*2*Pi;
      {Th0 := Random_*2*Pi;}
      FOR j := 1 TO Num[nRad] DO
      BEGIN
         Rad[nStar + j] := Radius;
         Speed[nStar + j] := CircVel[nRad];
         Angle[nStar + j] := Th0 + j*2*3.14159/Num[nRad];
         CosAngle[nStar + j] := Radius*Cos(Angle[nStar + j]);
         SinAngle[nStar + j] := Radius*Sin(Angle[nStar + j]);
      END;
      nStar := nStar + Num[nRad];
   END; {nRad loop.}
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


Procedure PlotGalaxy;

VAR
   i, StarColor, xSc, ySc: Integer;
BEGIN
   StarColor := 0;
   GraphBackColor := Blakk;
   HideMouse;
   CloseViewPort(3);
   ShowCUPSKeys := True;
   CUPSKeys.Display;
   OpenViewPort(2);
   OpenViewPort(1);
   SelectScale(1);
   SetColor(Blue);
   SetFillStyle(1, Blue);
   Map(0, 0, xSc, ySc);
   FillEllipse(xSc, ySc, 5, 4);
   FOR i := 1 TO nStar DO
   BEGIN
      StarColor := StarColor + 1; IF StarColor = 16 THEN StarColor := 1;
      SetColor(StarColor);
      StarPlot(CosAngle[i], SinAngle[i], Starcolor);
      {PlotSymbol(CosAngle[i], SinAngle[i], '.');}
   END;
   ShowMouse;
END;

{-----------------------------------------------------------------------}

PROCEDURE PlotGalaxyAndRegion;
VAR
   xOld, yOld, xNew, yNew             : Real;
   DelTheta, CosDeltheta, SinDelTheta : Real;
   i                                  : Integer;
BEGIN
   PlotGalaxy;
   SetColor(Yellow);
   DelTheta := Pi/20;
   CosDelTheta := Cos(DelTheta); SinDelTheta := Sin(DelTheta);
   xOld := xSun + LocalRad;
   yOld := ySun;
   FOR i := 1 TO 40 DO
   BEGIN
      xNew := xSun + (xOld - xSun)*CosDelTheta
                   - (yOld - ySun)*SinDelTheta;
      yNew := ySun + (yOld - ySun)*CosDelTheta
                   + (xOld - xSun)*SinDelTheta;
      PlotLine(xOld, yOld, xNew, yNew);
      xOld := xNew; yOld := yNew;
   END;
   PlotSymbol(xSun, ySun, '*');
   xOld := GalRadius/1.1;
   yOld := 0;
   FOR i := 1 TO 40 DO
   BEGIN
      xNew := xOld*CosDelTheta - yOld*SinDelTheta;
      yNew := yOld*CosDelTheta + xOld*SinDelTheta;
      PlotLine(xOld, yOld, xNew, yNew);
      xOld := xNew; yOld := yNew;
   END;
END;

{------------------------------------------------------------------------}

Procedure DefineLocalRegion(GalRadius: Real;
                            VAR xSun, ySun, R0, LocalRad: Real);
VAR
   XCoord, YCoord: Real;
   xm, ym: Integer;
   Ins: Boolean;
   Button    : Integer;
   Clicked   : Boolean;
   PositionOK: Boolean;
   xSc, ySc: Integer;
   i       : Integer;
   ArmStr, SegStr: String;
   xOld, yOld, xNew, yNew: Real;
   ROld                  : Real;
   MaxRad: Real;
   DelTheta, CosDeltheta, SinDelTheta: Real;

PROCEDURE MakeCircle(xSun, yXun, LocalRad: Real);
VAR
   i: Integer;
BEGIN
      SetColor(Yellow);
      DelTheta := Pi/20;
      CosDelTheta := Cos(DelTheta); SinDelTheta := Sin(DelTheta);
      FOR i := 1 TO 40 DO
      BEGIN
         xNew := xSun + (xOld - xSun)*CosDelTheta
                      - (yOld - ySun)*SinDelTheta;
         yNew := ySun + (yOld - ySun)*CosDelTheta
                      + (xOld - xSun)*SinDelTheta;
         PlotLine(xOld, yOld, xNew, yNew);
         xOld := xNew; yOld := yNew;
      END;
END;

BEGIN
   GraphBackColor := Blue;
   SetColor(White);
   OpenViewPort(2);
   HideMouse;
   Delay(500);
   {Delay(500 div 62, Tick);}
   SelectScale(2);
   Map(0, 1.8, xSc, ySc);
   OutTextxy(xSc, ySc,
   '  Click on the central point, at least one AU from the galactic center,');
   Map(0, 0.8, xSc, ySc);
   OutTextxy(xSc, ySc,'  and inside the galaxy.');
     ShowMouse;
   SetColor(yellow);
   SelectScale(1);
   SelectViewPort(1);
   DelTheta := Pi/20;
   xOld := GalRadius/1.1;
   yOld := 0;
   CosDelTheta := Cos(DelTheta); SinDelTheta := Sin(DelTheta);
   SetColor(Yellow);
   FOR i := 1 TO 40 DO
   BEGIN
      xNew := xOld*CosDelTheta - yOld*SinDelTheta;
      yNew := yOld*CosDelTheta + xOld*SinDelTheta;
      PlotLine(xOld, yOld, xNew, yNew);
      xOld := xNew; yOld := yNew;
   END;
   xOld := 1;
   yOld := 0;
   CosDelTheta := Cos(DelTheta); SinDelTheta := Sin(DelTheta);
   SetColor(Yellow);
   FOR i := 1 TO 40 DO
   BEGIN
      xNew := xOld*CosDelTheta - yOld*SinDelTheta;
      yNew := yOld*CosDelTheta + xOld*SinDelTheta;
      PlotLine(xOld, yOld, xNew, yNew);
      xOld := xNew; yOld := yNew;
   END;
   SetColor(Whitte);
   PositionOK := True;
   Clicked := False;
   REPEAT
      {WaitOnMouseClick;}
      MouseGlobalPosn(xSc, ySc, Button);
      MousePosn(xSun, ySun, 1, 1, Ins);
      R0 := Sqrt(xSun*xSun + ySun*ySun);
      IF (R0 > (GalRadius/1.1))
      OR  (R0 < 1) THEN
          BEGIN
             Beep;
             PositionOK := False;
             RubOutXY(0.8, 0.88, 10, Black);
             PrintXY(0.8, 0.88, 'Go away');
          END
          ELSE
          BEGIN
             IF Abs(xOld - xSun) + Abs(yOld - ySun) > 0.01 THEN
             BEGIN
                RubOutXY(0.8, 0.88, 10, Black);
                PrintXY(0.8, 0.88, 'R0 = '+NumStr(R0, 4, 2));
                xOld := xSun;
                yOld := ySun;
             END;
             PositionOK := True;
          END;
      IF PositionOK AND (Button <> 0) THEN
      BEGIN
         REPEAT
            MouseGlobalPosn(xSc, ySc, Button);
            MousePosn(xSun, ySun, 1, 1, Ins);
         UNTIL PositionOK AND (Button = 0);
         Clicked := True;
      END;
   UNTIL PositionOK AND Clicked;
   R0 := Sqrt(xSun*xSun + ySun*ySun);
   GraphBackColor := Blue;
   SetColor(Yellow);
   PlotSymbol(xSun, ySun, '*');
   OpenViewPort(2);
   SelectScale(2);
   SetColor(white);
   Map(0, 1.8, xSc, ySc);
   OutTextxy(xSc, ySc,
   '  Click at a point on the circumference. Don''t include the galactic center');
   Map(0, 0.8, xSc, ySc);
   OutTextxy(xSc, ySc,
   '  The entire region must be inside the galaxy. Minimum radius is 0.1 Kpc.');

   SelectScale(1);
   SelectViewPort(1);
   SetColor(Whitte);
   PositionOK := True;
      SetWriteMode(XORPut);
      REPEAT
         Button := 0;
         MouseGlobalPosn(xm, ym, Button);
         MousePosn(xOld, yOld, 1, 1, Ins);
         PlotLine(xSun, ySun, xOld, yOld);
         LocalRad := Sqrt(Sqr(xSun - xOld) + Sqr(ySun - yOld));
         IF LocalRad < 0.1 THEN LocalRad := 0.1;
         MakeCircle(xSun, ySun, LocalRad);
         Delay(10);
         {Delay(1, Tick);}
         PlotLine(xSun, ySun, xOld, yOld);
         MakeCircle(xSun, ySun, LocalRad);
         IF Abs(ROld - LocalRad) > 0.05 THEN
         BEGIN
            RubOutXY(0.75, 0.88, 14, Black);
            PrintXY(0.75, 0.88, 'Radius = '+NumStr(LocalRad, 4, 2));
            ROld := LocalRad;
         END;
         MaxRad := R0 + LocalRad;
         IF MaxRad > Abs(GalRadius/1.1) THEN
         BEGIN
            RubOutXY(0.75, 0.88, 14, Black);
            PrintXY(0.75, 0.88, '   Go away');
            Beep;
            PositionOK := False;
         END
         ELSE PositionOK := True;
         IF LocalRad > R0-0.5 THEN
         BEGIN
            RubOutXY(0.75, 0.88, 14, Black);
            PrintXY(0.75, 0.88, '   Go away');
            Beep;
            PositionOK := False;
         END;
      UNTIL (Button <> 0) AND (PositionOK = True);
      SetWriteMode(CopyPut);
      MakeCircle(xSun, ySun, LocalRad);
      RubOutXY(0.75, 0.88, 14, Black);
      ShowMouse;
      IF LocalRad < 0.1 THEN LocalRad := 0.1;
END;

{------------------------------------------------------------------------}
   
Procedure Arrow(xa, ya, xb, yb: Real; ArrowColor: Word);
VAR
   x, y: Real;
   Fraction: Real;
   StoreColor: Word;
BEGIN
   StoreColor := GetColor;
   SetColor(ArrowColor);
   Fraction := 0.2;
   PlotLine(xa, ya, xb, yb);
   x := (xb - Fraction*(xb - xa) + Fraction*(yb - ya));
   y := (yb - Fraction*(yb - ya) - Fraction*(xb - xa));
   PlotLine(xb, yb, x, y);
   x := (xb - Fraction*(xb - xa) - Fraction*(yb - ya));
   y := (yb - Fraction*(yb - ya) + Fraction*(xb - xa));
   PlotLine(xb, yb, x, y);
   SetColor(StoreColor);
END;


{------------------------------------------------------------------------}      

Procedure LocalRotation(R0, dR, GalRadius: Real; CircVel: VelVector);

VAR
   Aspect : Real;
   dt: Real;
   LocalSpeeds, RandTh: VelVector;
   Size: Word;
   dx, dy: Real;
   dTheta: Real;
   CosSun, SinSun: Real;
   NumRad: Integer;


Procedure FindRelativeSpeeds(R0, dR: Real; VAR LocalSpeeds, RandTh: VelVector);
VAR
   V0: Real;
   nRad, j: Integer;
   Radius, dRadius, CPrime, Force: Real;


Function FindCircVel(R, GalRadius: Real; CircVel: VelVector): Real;
VAR
   i: Integer;
BEGIN
   i := Trunc(101*R/GalRadius + 0.5);
   FindCircVel := CircVel[i] +
                  (CircVel[i+1] - CircVel[i])*(R - GalRadius*(i + 0.5)/101)
                  /(GalRadius/101);
END;

BEGIN
   Aspect := 0.8;
   dRadius := 2*dR*(1/NumRad);
{Find angular speed of Sun.}
   V0 := FindCircVel(R0, GalRadius, CircVel);
   FOR nRad := 1 TO NumRad DO
   BEGIN
      Radius := R0 - dR + dRadius*(nRad);
{Find the circular speed relative to the Sun.}
      LocalSpeeds[nRad] := FindCircVel(Radius, GalRadius, CircVel) - V0;
{Find random angular displacement. Delete "*Random" to see initial radial
"spokes" in the same colors.}
      RandTh[nRad] := 2*dTheta{*Random} - dTheta;
      {RandTh[nRad] := 2*dTheta*Random_ - dTheta;}
   END; {nRad loop.}
END;



Procedure PlotLocalStars(R0, dR, dTheta, dT: Real; LocalSpeeds: VelVector;
                         VAR Randth: VelVector);
VAR
   nRad, i, n1: Integer;
   Radius, dRadius, dTh, Angle: Real;
   StarColor: Word;
   xSc, ySc: Integer;
  

Procedure PlotStar(R, Th: Real; StarColor, Size: Word);
VAR
   xSc, ySc: Integer;
   Save, xRel, yRel: Real;
BEGIN
   xRel := (R*Cos(Th) - R0);
   yRel := (R*Sin(Th));
   Save := xRel;
   xRel := xRel*CosSun - yRel*SinSun;
   yRel := Save*SinSun + yRel*CosSun;
  { Map(xRel, yRel, xSc, ySc);
   SetColor(StarColor);
   Circle(xSc, ySc, Size);}
   StarPlot(xRel, yRel,StarColor);
END;


BEGIN
   StarColor := 0;
   HotKeys.Display;
   HideMouse;
   PrintXY(0.2, 0.9, 'Rotating reference system. Origin at the Sun.');
   dRadius := 2*dR*(1/NumRad);
   FOR nRad := 1 TO NumRad DO
   BEGIN
      dTh := dTheta/15;
      Radius := R0 - dR + dRadius*(nRad);
      StarColor := 0;
      FOR i := 1 TO 30 DO
      BEGIN
         StarColor := StarColor + 1; IF StarColor > 16 THEN StarColor := 1;
         Angle := RandTh[nRad] + dTh*i;
         IF Angle > dTheta THEN Angle := Angle - 2*dTheta;
         IF R0*R0 + Radius*Radius - 2*R0*Radius*Cos(Angle) < dR*dR THEN
                 PlotStar(Radius, Angle, StarColor, 2);
      END;
      PlotStar(R0, 0, Whitte, 4);
      PlotStar(R0, 0, Whitte, 3);
      PlotStar(R0, 0, Whitte, 2);
      RandTh[nRad] := RandTh[nRad] + dT*LocalSpeeds[nRad];
      IF RandTh[nRad] > dTheta THEN
         RandTh[nRad] := RandTh[nRad] - 2*dTheta;
      IF RandTh[nRad] < - dTheta THEN
         RandTh[nRad] := RandTh[nRad] + 2*dTheta;
   END;
   Arrow(0, 0, -1.2*dR*CosSun, -1.2*dR*SinSun, Whitte);
   Map(-1.2*dR*CosSun, -1.2*dR*SinSun, xSc, ySc);
   OutTextXY(xSc+5, ySc, 'Galactic center');
   PlotSymbol(-R0*CosSun, -R0*SinSun, 'O');
   ShowMouse;
END;


{------------------------------------------------------------------------}

{Main program for LocalRotation.}
BEGIN
   ShowCUPSKeys := False;
   CUPSKeys.Clear;
{  CloseGraph;
   CUPSDone;}
   Picture.Init;
{  CUPSInit; }
   SetGraphMode(VGAmed);
   NumRad := 30;
   dTheta := dR/R0;
   Action := True;
   HideMouse;
   FindRelativeSpeeds(R0, dR, LocalSpeeds, RandTh);
   GraphBackColor := Blakk;
   DefineViewPort(4, 0.03, 0.97, 0.07, 0.93);
   DefineScale(4, -2*dR, 2*dR, -2*dR*Aspect, 2*dR*Aspect);
   OpenViewPort(4);
   SetUpHotKeys;
   SelectScale(4);
   Picture.Select(4);
   dT := 1;
   CosSun := xSun/R0;
   SinSun := ySun/R0;       
   SeeRotation := True;
   REPEAT
      IF Action THEN
      BEGIN
         PlotLocalStars(R0, dR, dTheta, dT, LocalSpeeds, Randth);
         Picture.SwitchAndClean;
      END;
      CheckForEvents;
      IF HotKeys.Pressed(Key) THEN HandleHotKeys(Key);
   UNTIL SeeRotation = False;
   Picture.Close;
{  CloseGraph;
   CUPSDone;
   GraphDriver := Detect;
   CUPSInit;  }
   SetGraphMode(VGAhi);
   OortMenu.Display;
   OortMenu.RowActivate(3,1, False);
   LightMenu := True;
   ShowMouse;
END;

{------------------------------------------------------------------------}

PROCEDURE PlotLocalVelocityField;
VAR
   dTh: Real;
   CosSun, SinSun: Real;
   NumRad: Integer;
   Radius, dRadius: Real;
   SunSpeed: Real;
   i, nRad: Integer;
   Angle: Real;
   LastAnglePlot: Real;
   LocalSpeeds, CAngle, SAngle: VelVector;
   ScaleFactor : Real;
   Magnitude: Real;
   XVector, YVector: Real;
   xHead, yHead, xTail, yTail: Real;
   xCenter, yCenter: Real;
   Distance: Real;
   StopAndLook: Boolean;
   xSc, ySc: Integer;

PROCEDURE FindSpeeds(R0, dR: Real;
                     VAR LocalSpeeds: VelVector; VAR SunSpeed: Real);
VAR
   nRad: Integer;
   Radius, dRadius: Real;


FUNCTION FindCircVel(R, GalRadius: Real; CircVel: VelVector): Real;
VAR
   i: Integer;
BEGIN
   i := Trunc(101*R/GalRadius + 0.5);
   FindCircVel := CircVel[i] +
                  (CircVel[i+1] - CircVel[i])*(R - GalRadius*(i + 0.5)/101)
                  /(GalRadius/101);
END;

BEGIN
   dRadius := 2*LocalRad*(1/NumRad);
{Find speed of Sun.}
   SunSpeed := R0*FindCircVel(R0, GalRadius, CircVel);
   FOR nRad := 1 TO NumRad DO
   BEGIN
      Radius := R0 - LocalRad + dRadius*(nRad);
{Find the circular speed relative to the Sun.}
      LocalSpeeds[nRad] := Radius*(FindCircVel(Radius, GalRadius, CircVel));
   END; {nRad loop.}
END;

Procedure PlotVelocityArrow(R, CosTh, SinTh, Length: Real);
VAR
   xSc, ySc: Integer;
   Save, xRel, yRel: Real;
   xHead, yHead, xTail, yTail: Real;
   Delx, Dely: Real;
BEGIN
   xRel := (R*CosTh - R0);
   yRel := (R*SinTh);
   Save := xRel;
   xRel := xRel*CosSun - yRel*SinSun;
   yRel := Save*SinSun + yRel*CosSun;
   Delx := 0.5*Length*(SinTh*CosSun + CosTh*SinSun);
   Dely := 0.5*Length*(CosTh*CosSun - SinTh*SinSun);
   xHead := xRel - Delx;
   xTail := xRel + Delx;
   yHead := yRel + Dely;
   yTail := yRel - Dely;
   Arrow(xTail, yTail, xHead, yHead, Yellow);
   PlotSymbol(xRel, yRel, '.');
END;

{------------------------------------------------------------------------}

PROCEDURE Rotate(xIn, yIn: Real; VAR xOut, yOut: Real);
BEGIN
   xOut := xIn*CosSun - yIn*SinSun;
   yOut := xIn*SinSun + yIn*CosSun;
END;

{------------------------------------------------------------------------}

FUNCTION FindScaleFactor : Real;
VAR
   Length, MaxLength  : Real;
   i, n               : Integer;
BEGIN
   MaxLength := 0;
   FOR n := 1 TO NumRad DO
   BEGIN
      Radius := R0 - LocalRad + dRadius*(n);
      LastAnglePlot := - 10*dTh;
      FOR i := 1 TO 20 DO
      BEGIN
         Angle := (i - 10)*dTh;
       {  Print(5,5, NumStr(Radius, 10,8));
         Print(5,6, NumStr(R0, 10,8));
         Print(5,7, NumStr(LocalRad, 10,8));
         Print(5,8, NumStr(R0*R0 + Radius*Radius - 2*R0*Radius*CAngle[i],10,8));
         Print(5,9, NumStr(CAngle[i],10,8));StaticPause;Halt; }
         IF R0*R0 + Radius*Radius - 2*R0*Radius*CAngle[i]
                     < LocalRad*LocalRad THEN
{The point is within the circle to be plotted.}
         BEGIN
            Magnitude := Abs(LocalSpeeds[n] - SunSpeed);
            BEGIN
               LastAnglePlot := Angle;
               Distance := (Sqr(Radius) + Sqr(R0) - 2*Radius*R0*CAngle[i]);
               IF Distance < 0.00001 THEN Distance := 0.01;
               Distance := Sqrt(Distance);
               CASE FieldNumber OF
               1: BEGIN
                      XVector := - LocalSpeeds[n]*SAngle[i];
                      YVector := LocalSpeeds[n]*CAngle[i] - SunSpeed;
                 END;
               2: BEGIN
                     Magnitude := SAngle[i]
                                  *(R0*LocalSpeeds[n] - Radius*SunSpeed);
                     Magnitude := Magnitude/Distance;
                     XVector := (-R0 + Radius*CAngle[i])*Magnitude;
                     YVector := Radius*SAngle[i]*Magnitude;
                  END;
               3: BEGIN
                     Magnitude := LocalSpeeds[n]*(R0*CAngle[i] - Radius)
                                   + SunSpeed*(Radius*CAngle[i] - R0);
                     Magnitude := Magnitude/(Distance);
                     XVector := (Radius*SAngle[i])*Magnitude;
                     YVector := (R0 - Radius*CAngle[i])*Magnitude;
                  END;
               4: BEGIN
                     Magnitude := LocalSpeeds[n]*(R0*CAngle[i] - Radius)
                                   + SunSpeed*(Radius*CAngle[i] - R0);
                     Magnitude := Magnitude/(Distance*Sqrt(Distance));
                     XVector := (Radius*SAngle[i])*Magnitude;
                     YVector := (R0 - Radius*CAngle[i])*Magnitude;
                  END;
               END;
               Rotate(XVector, YVector, XVector, YVector);
               Length := Sqrt(XVector*XVector + YVector*Yvector);
               IF MaxLength < Length THEN MaxLength := Length;
            END;
         END;
      END;
   END;
   FindScaleFactor := 0.15*LocalRad/MaxLength;
END;

{------------------------------------------------------------------------}

BEGIN    {Main program for local velocity field.}
   CloseViewPort(2);
   NumRad := 30;
   dTh := (LocalRad/R0)/10;
   CosSun := xSun/R0;
   SinSun := ySun/R0;
   FOR i := 1 TO 20 DO
   BEGIN
      Angle := (i - 10)*dTh;
      CAngle[i] := Cos(Angle);
      SAngle[i] := Sin(Angle);
   END;    
   FindSpeeds(R0, LocalRad, LocalSpeeds, SunSpeed);
   GraphBackColor := Blakk;
   DefineViewPort(4, 0.03, 0.97, 0.07, 0.93);
   DefineScale(4, -2*LocalRad, 2*LocalRad,
                  -2*LocalRad*Aspect, 2*LocalRad*Aspect);
   FieldNumber := 1;
   REPEAT
   HideMouse;
   OpenViewPort(4);
   ShowCUPSKeys := False;
   CUPSKeys.Clear;
   FieldHotKeys.Display;
   SelectScale(4);
   SetColor(Yellow);
   PrintXY(0.05, 0.85, 'Non-rotating reference system');
   PrintXY(0.05, 0.81, 'Origin at the Sun');
   CASE FieldNumber OF
      1: PrintXY(0.2, 0.9, '             VELOCITY FIELD');
      2: PrintXY(0.2, 0.9, '      COMPONENTS OF RADIAL VELOCITY');
      3: PrintXY(0.2, 0.9, '    COMPONENTS OF TANGENTIAL VELOCITY');
      4: PrintXY(0.2, 0.9, '            PROPER MOTIONS');
   END; 
   dRadius := 2*LocalRad*(1/NumRad);
   ScaleFactor := FindScaleFactor;
   FOR nRad := 1 TO NumRad DO
   BEGIN
      Radius := R0 - LocalRad + dRadius*(nRad);
      LastAnglePlot := - 10*dTh;
      FOR i := 1 TO 20 DO
      BEGIN
         Angle := (i - 10)*dTh;
         IF R0*R0 + Radius*Radius - 2*R0*Radius*CAngle[i]
                     < LocalRad*LocalRad THEN
{The point is within the circle to be plotted.}
         BEGIN
            BEGIN
               LastAnglePlot := Angle;
               Distance := (Sqr(Radius) + Sqr(R0) - 2*Radius*R0*CAngle[i]);
               IF Distance < 0.00001 THEN Distance := 0.01;
               Distance := Sqrt(Distance);
               CASE FieldNumber OF
               1: BEGIN
                      XVector := - LocalSpeeds[nRad]*SAngle[i];
                      YVector := LocalSpeeds[nRad]*CAngle[i] - SunSpeed;
                 END;  
               2: BEGIN
                     Magnitude := SAngle[i]
                                  *(R0*LocalSpeeds[nRad] - Radius*SunSpeed);
                     Magnitude := Magnitude/Distance;
                     XVector := (-R0 + Radius*CAngle[i])*Magnitude;
                     YVector := Radius*SAngle[i]*Magnitude;
                  END;
               3: BEGIN
                     Magnitude := LocalSpeeds[nRad]*(R0*CAngle[i] - Radius)
                                   + SunSpeed*(Radius*CAngle[i] - R0);
                     Magnitude := Magnitude/(Distance);
                     XVector := (Radius*SAngle[i])*Magnitude;
                     YVector := (R0 - Radius*CAngle[i])*Magnitude;
                  END;
               4: BEGIN
                     Magnitude := LocalSpeeds[nRad]*(R0*CAngle[i] - Radius)
                                   + SunSpeed*(Radius*CAngle[i] - R0);
                     Magnitude := Magnitude/(Distance*Sqrt(Distance));
                     XVector := (Radius*SAngle[i])*Magnitude;
                     YVector := (R0 - Radius*CAngle[i])*Magnitude;
                  END;
               END;
               Rotate(XVector, YVector, XVector, YVector);
               Rotate(Radius*CAngle[i] - R0, Radius*SAngle[i], xCenter, yCenter);
               xHead := xCenter + ScaleFactor*XVector/2;
               yHead := yCenter + ScaleFactor*YVector/2;
               xTail := xCenter - ScaleFactor*XVector/2;
               yTail := yCenter - ScaleFactor*YVector/2;
               Arrow(xTail, yTail, xHead, yHead, Yellow);
            END;
         END;
      END;
   END;
   PlotSymbol(0, 0, 'o');
   PlotSymbol(-R0*CosSun, -R0*SinSun, 'O');
   Arrow(0, 0, -1.2*LocalRad*CosSun, -1.2*LocalRad*SinSun, Whitte);
   Map(-1.2*LocalRad*CosSun, -1.2*LocalRad*SinSun, xSc, ySc);
   SetColor(Whitte);
   {OutTextXY(xSc+Trunc(20*(-Abs(CosSun) + Abs(SinSun))),
             ySc+Trunc(20*(CosSun - SinSun)), 'Galactic center');}
   IF CosSun>0 THEN OutTextXY(xSc-60, ySc, 'Galactic center')
               ELSE OutTextXY(xSc, ySc, 'Galactic center');
   StopAndLook := True;
   ShowMouse;
      REPEAT
         CheckForEvents;
         IF FieldHotKeys.Pressed(Key) THEN
         BEGIN
            StopAndLook := False;
            HandleFieldHotKeys(Key);
         END;
         If Oortmenu.Activated then
         begin
            StopAndLook := False;
            FieldNumber := 5;
            PlotGalaxyAndRegion;
            HandleOortMenu;
         end;
      UNTIL NOT StopAndLook;
   UNTIL FieldNumber = 5;
END;

{------------------------------------------------------------------------}

Procedure  PlotRadialVelocities(R0, LocalRad, GalRadius:Real;
                                CircVel: VelVector);
VAR
   OortA: Real;



Function FindCircVel(R, GalRadius: Real; CircVel: VelVector): Real;
VAR
   i: Integer;
BEGIN
   i := Trunc(101*R/GalRadius + 0.5);
   FindCircVel := CircVel[i] +
                  (CircVel[i+1] - CircVel[i])*(R - GalRadius*(i + 0.5)/101)
                  /(GalRadius/101);
END;


Function FindRadVel(LocalRad, R0, GalRadius, GalLong: Real;
                         CircVel: VelVector): Real;
VAR
   R: Real;
BEGIN
   R := Sqrt(R0*R0 + LocalRad*LocalRad - 2*R0*LocalRad*Cos(GalLong));
   FindRadVel := R0*Sin(GalLong)*(FindCircVel(R, GalRadius, CircVel)
                                 - FindCircVel(R0, GalRadius, CircVel));
END;

Function FindOortA(R0, GalRadius: Real; CircVel: VelVector): Real;
VAR
   dR: Real;
BEGIN
   dR := 0.1;
   FindOortA := - 0.5*R0*(FindCircVel((R0 + dR), GalRadius, CircVel)
               - FindCircVel((R0 - dR), GalRadius, CircVel))/(2*dR);
END;

Procedure  PlotRadVel(LocalRad, R0, GalRadius: Real; CircVel: VelVector);
VAR
   GalLong: Real;   {Galactic longitude.}
   RV, RVOld, RVMax: Real;
   RVMaxInt: Integer;
   dx, dy: Real;
   i, xSc, ySc: Integer;
   RVLabel: String;
BEGIN
   HideMouse;
   OpenViewPort(3);
   GalLong:= 0;
   RVMax := 0;
   FOR i := 1 TO 50 DO
   BEGIN    {Find the maximum radial velocity.}
      GalLong := GalLong + 2*3.14159/50;
      RV := FindRadVel(LocalRad, R0, GalRadius, GalLong, CircVel);
      IF Abs(RV) > RVMax THEN RVMax := Abs(RV);
   END;
   DefineScale(3, -20, 380, -2*RVMax, 1.5*RVMax);
   SelectScale(3);
   SetColor(Yellow);
   PlotLine(0, 0, 360, 0);
   PlotLine(0, -RVMax, 0, RVMax);
   FOR i := 1 TO 12 DO
   BEGIN
      PlotLine(30*i, 0, 30*i, RVMax/20);
      Map(30*i, -RVMax/20, xSc, ySc);
      RVLabel := NumStr(30*i, 3, 0);
      OutTextxy(xSc-5, ySc, RVLabel);
   END;
   Map(280, -0.2*RVMax, xSc, ySc);
   OutTextxy(xSc, ySc, 'Galactic Longitude');
   Map(5, 1.1*RVMax, xSc, ySc);
   OutTextxy(xSc, ySc, 'km/sec');
   RVMaxInt := Trunc(977*RvMax);
   FOR i := RVMaxInt TO RVMaxInt DO
   BEGIN
      Map(-10, i/977, xSc, ySc);
      PlotLine(0, i/977, 10, i/977);
      RVLabel := NumStr(i, 3, 0);
      OutTextxy(xSc, ySc, RVLabel);
      Map(-15, -i/977, xSc, ySc);
      PlotLine(0, -i/977, 10, -i/977);
      RVLabel := NumStr(-i, 3, 0);
      OutTextxy(xSc, ySc, RVLabel);
   END;
   Map(-10, 0, xSc, ySc);
   OutTextxy(xSc, ySc, '0');
   GalLong := 0;
   RVOld := FindRadVel(LocalRad, R0, GalRadius, GalLong, CircVel);
   FOR i := 1 TO 180 DO
   BEGIN
      GalLong := i*2*3.14159/180;
      RV := FindRadVel(LocalRad, R0, GalRadius, GalLong, CircVel);
      PlotLine(2*i-2, RVOld, 2*i, RV);
      RVOld := RV;
   END;
   Map(20, -1.3*RVMax, xSc, ySc);
   OutTextxy(xSc, ySc,
          'Radial velocity as a function of galactic longitude.');
   RVLabel := NumStr(LocalRad, 4, 2);
   RVLabel := Concat('The distance from the Sun is ' , RVLabel , 'kpc');
   Map(20, -1.5*RVMax, xSc, ySc);                       OutTextxy(xSc, ySc, RVLabel);
   OortA := FindOortA(R0, GalRadius, CircVel);
   SetColor(Whitte);
   GalLong := 0;
   RVOld := 0;
   FOR i := 1 TO 180 DO
   BEGIN
      GalLong := i*2*3.14159/180;
      RV := OortA*LocalRad*Sin(2*GalLong);
      PlotLine(2*i-2, RVOld, 2*i, RV);
      RVOld := RV;
   END;
   RVLabel := NumStr(977*OortA, 6, 3);
   RVLabel :=  Concat('Linear approximation, Adsin(2L). Oort''s constant A ='
                , RVLabel , '(km/sec)/kpc)');
   Map(20, -1.7*RVMax, xSc, ySc);
   OutTextxy(xSc, ySc, RVLabel);
   ShowMouse;
END;

{------------------------------------------------------------------------}

BEGIN  {Main program for procedure PlotRadialVelocities.}
         ShowConstants := True;
         PlotRadVel(LocalRad, R0, GalRadius, CircVel);
         LightMenu := False;
END;

{------------------------------------------------------------------------}                  

Procedure  PlotProperMotions(R0, LocalRad, GalRadius:Real;
                             CircVel: VelVector);
VAR
   OortA, OortB: Real;


Function FindCircVel(R, GalRadius: Real; CircVel: VelVector): Real;
VAR
   i: Integer;
BEGIN
   i := Trunc(101*R/GalRadius + 0.5);
   FindCircVel := CircVel[i] +
                  (CircVel[i+1] - CircVel[i])*(R - GalRadius*(i + 0.5)/101)
                  /(GalRadius/101);
END;


Function FindPrMot(LocalRad, R0, GalRadius, GalLong: Real; CircVel: VelVector):
                    Real;
VAR
   R: Real;
BEGIN
   R := Sqrt(R0*R0 + LocalRad*LocalRad - 2*R0*LocalRad*Cos(GalLong));
   FindPrMot := R0*Cos(GalLong)*(FindCircVel(R, GalRadius, CircVel)
                                 - FindCircVel(R0, GalRadius, CircVel))/LocalRad
                           - FindCircVel(R, GalRadius, CircVel);
END;

Function FindOortA(R0, GalRadius: Real; CircVel: VelVector): Real;
VAR
   dR: Real;
BEGIN
   dR := 0.1;
   FindOortA := - 0.5*R0*(FindCircVel((R0 + dR), GalRadius, CircVel)
               - FindCircVel((R0 - dR), GalRadius, CircVel))/(2*dR);
END;

Function FindOortB(OortA, R0, GalRadius: Real; CircVel: VelVector): Real;
BEGIN
   FindOortB := OortA - FindCircVel(R0, GalRadius, CircVel);
END;

Procedure  PlotPrMot(LocalRad, R0, GalRadius: Real; CircVel: VelVector);
VAR
   GalLong: Real;   {Galactic longitude.}
   PM, PMOld, PMMax, PMMin, PMRange: Real;
   PMMaxInt, PMMinInt: Integer;
   dx, dy: Real;
   i, xSc, ySc: Integer;
   PMLabel: String;
BEGIN
   HideMouse;
   OpenViewPort(3);
   GalLong:= 0;
   PMMax := 0; PMMin := 0;
   FOR i := 1 TO 50 DO
   BEGIN    {Find the extremes of proper motion.}
      GalLong := GalLong + 2*3.14159/50;
      PM := FindPrMot(LocalRad, R0, GalRadius, GalLong, CircVel);
      IF PM > PMMax THEN PMMax := PM;
      IF PM < PMMin THEN PMMin := PM;
   END;
   PMRange := PMMax - PMMin;
   DefineScale(3, -20, 380, PMMin - 0.5*PMRange, PMMax + 0.25*PMRange);
   SelectScale(3);
   SetColor(Yellow);
   PlotLine(0, 0, 360, 0);
   PlotLine(0, PMMin, 0, PMMax);
   FOR i := 1 TO 12 DO
   BEGIN
      PlotLine(30*i, 0, 30*i, PMRange/40);
      Map(30*i, -PMRange/40, xSc, ySc);
      PMLabel := NumStr(30*i, 3, 0);
      OutTextxy(xSc-5, ySc, PMLabel);
   END;
   Map(280, -0.1*PMRange, xSc, ySc);
   OutTextxy(xSc, ySc, 'Galactic Longitude');
   Map(5, 0.12*PMRange + PMMax, xSc, ySc);
   OutTextxy(xSc, ySc, 'units of 0.001 seconds of arc per year');
   PMMaxInt := Trunc(977*PMMax/4.74);
   PMMinInt := Trunc(977*PMMin/4.74);
   FOR i := PMMaxInt TO PMMaxInt DO
   BEGIN
      Map(-10, i*4.74/977, xSc, ySc);
      PlotLine(0, i*4.74/977, 10, i*4.74/977);
      PMLabel := NumStr(i, 3, 0);
      OutTextxy(xSc, ySc, PMLabel);
   END;
   Map(-10, 0, xSc, ySc);
   OutTextxy(xSc, ySc, '0');
   FOR i := - PMMinInt TO - PMMinInt DO
   BEGIN
      Map(-15, -i*4.74/977, xSc, ySc);
      PlotLine(0, -i*4.74/977, 10, -i*4.74/977);
      PMLabel := NumStr(-i, 3, 0);
      OutTextxy(xSc, ySc, PMLabel);
   END;
   GalLong := 0;
   PMOld := FindPrMot(LocalRad, R0, GalRadius, GalLong, CircVel);
   FOR i := 1 TO 180 DO
   BEGIN
      GalLong := i*2*3.14159/180;
      PM := FindPrMot(LocalRad, R0, GalRadius, GalLong, CircVel);
      PlotLine(2*i-2, PMOld, 2*i, PM);
      PMOld := PM;
   END;
   Map(0, PMMin - 0.1*PMRange, xSc, ySc);
   OutTextxy(xSc, ySc,
          'Proper motion as a function of galactic longitude.');
   PMLabel := NumStr(LocalRad, 4, 2);
   PMLabel := Concat('The distance from the Sun is ' , PMLabel , 'kpc');
   Map(0, PMMin - 0.2*PMRange, xSc, ySc);
   OutTextxy(xSc, ySc, PMLabel);
   OortA := FindOortA(R0, GalRadius, CircVel);
   OortB := FindOortB(OortA, R0, GalRadius, CircVel);
   SetColor(Whitte);
   GalLong := 0;
   PMOld := OortA + OortB;
   FOR i := 1 TO 180 DO
   BEGIN
      GalLong := i*2*3.14159/180;
      PM := OortA*Cos(2*GalLong) + OortB;
      PlotLine(2*i-2, PMOld, 2*i, PM);
      PMOld := PM;
   END;
   Map(0, PMMin - 0.3*PMRange, xSc, ySc);
   PMLabel := NumStr(977*OortA, 6, 3);
   PMLabel :=  Concat('Linear approximation, Acos(2L) + B. Oort''s constants A ='
                , PMLabel , '(km/sec)/kpc)');
   OutTextxy(xSc, ySc, PMLabel);
   Map(0, PMMin - 0.4*PMRange, xSc, ySc);
   PMLabel := NumStr(977*OortB, 6, 3);
   PMLabel :=  Concat('                                                     B ='
                , PMLabel , '(km/sec)/kpc');
   OutTextxy(xSc, ySc, PMLabel);
   ShowMouse;
   LightMenu := True;
END;
      
{------------------------------------------------------------------------}

BEGIN   {Main Program for Procedure PlotProperMotions.}
         ShowConstants := True;
         LightMenu := False;
         PlotPrMot(LocalRad, R0, GalRadius, CircVel);
         LightMenu := True;
END;

{------------------------------------------------------------------------}

PROCEDURE PlotDefaultGalaxy;
VAR
   i   : Integer;

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
   G := 0.00449897; {Constant of gravitation.}
   Aspect := 0.7; {Adjusts for vertical and horizontal scales on screen.}
   Start := True;
   ShowConstants := False;
   MakeGalaxy(CircVel, CircRad, Rad, Angle, Speed, GalRadius, nStar, Num,
               GalaxyMade);
   PlotGalaxy;
   Start := False;
   OortMenu.RowActivate(2,2, True);
   OortMenu.RowActivate(3,1, True);
   OortMenu.RowActivate(3,2, False);
   OortMenu.RowActivate(3,3, False);
   OortMenu.RowActivate(4,1, False);
   OortMenu.RowActivate(4,2, False);
   LightMenu := True;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleOortMenu;

BEGIN
   With OortMenu DO
   CASE ColChosen OF
      1: CASE RowChosen OF
            1: BEGIN
                  AboutCUPS;
                  LightMenu := True;
               END;
            2: BEGIN
                  ShowHelp('HELPGals.HLP', 'Oort Hello');
                  LightMenu := True;
               END;
            3: BEGIN
                  Configuration;
                  LightMenu := True;
               END;
            4: BEGIN
                  ShowHelp('HELPGals.HLP', 'Oort GeneralA');
                  ShowHelp('HELPGals.HLP', 'Oort GeneralB');
                  ShowHelp('HELPGals.HLP', 'Oort GeneralC');
                  LightMenu := True;
               END;
            5: ExitRequest := True;
         END;
      2: CASE RowChosen OF
            1: BEGIN
                  MakeGalaxy(CircVel, CircRad, Rad, Angle, Speed, GalRadius, nStar, Num,
                     GalaxyMade);
                  IF GalaxyMade = False THEN
                  BEGIN
                     ClearMuppetPort;
                     OortMenu.Display;
                     Announce('Data must be complete for galaxy to be shown');
                     OortMenu.RowActivate(2,2, False);
                     OortMenu.RowActivate(3,1, False);
                     OortMenu.RowActivate(3,2, False);
                     OortMenu.RowActivate(3,3, False);
                     OortMenu.RowActivate(4,1, False);
                     OortMenu.RowActivate(4,2, False);
                     LightMenu := True;
                     Exit;
                     {Exit(HandleOortMenu);}
                  END;
                  DefineScale(1, -1.2*GalRadius, 1.2*GalRadius,
                          (-1.2*GalRadius)*Aspect, (1.2*Galradius)*Aspect);
                  ClearMuppetPort;
                  OortMenu.Display;
                  OortMenu.Rowactivate(2,2, True);
                  OortMenu.RowActivate(3,1, True);
                  OortMenu.RowActivate(3,2, False);
                  OortMenu.RowActivate(3,3, False);
                  OortMenu.RowActivate(4,1, False);
                  OortMenu.RowActivate(4,2, False);
                  PlotGalaxy;
               END;
            2: BEGIN
                  PlotGalaxy;
                  OortMenu.RowActivate(3,1, True);
                  OortMenu.RowActivate(3,2, False);
                  OortMenu.RowActivate(3,3, False);
                  OortMenu.RowActivate(4,1, False);
                  OortMenu.RowActivate(4,2, False);
               END;
            3: PlotDefaultGalaxy;
         END;
      3: CASE RowChosen OF
            1: BEGIN
                  DefineLocalRegion(GalRadius, xSun, ySun, R0, LocalRad);
                  OortMenu.Display;
                  OortMenu.RowActivate(3,1, False);
                  OortMenu.RowActivate(3,2, True);
                  OortMenu.RowActivate(3,3, True);
                  OortMenu.RowActivate(4,1, True);
                  OortMenu.RowActivate(4,2, True);
                  GraphBackColor := Blakk;
                  OpenViewPort(2);
                  LightMenu := True;
               END;
            2: BEGIN
                  LocalRotation(R0, LocalRad, GalRadius, CircVel);
                  PlotGalaxyAndRegion;
               END;
            3: PlotLocalVelocityField;
         END;
      4: CASE RowChosen OF
            1: PlotRadialVelocities(R0, LocalRad, GalRadius, CircVel);
            2: PlotProperMotions(R0, LocalRad, GalRadius, CircVel);
         END;
    END;
END;

{-------------------------------------------------------------------------}

PROCEDURE HandleFieldHotKeys(Key: Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HELPGals.HLP', 'Oort Hello');
      2: FieldNumber := 1;
      3: FieldNumber := 2;
      4: FieldNumber := 3;
      5: FieldNumber := 4;
      6: BEGIN
            FieldNumber := 5;
            PlotGalaxyAndRegion;
         END;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleCUPSKeys(Key: Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HELPGals.HLP', 'Oort Hello');
      2: BEGIN
            IF ShowConstants THEN
            BEGIN
               ShowConstants := False;
               PlotGalaxyAndRegion;
            END;
            IF OortMenu.Chosen THEN HandleOortMenu;
         END;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE StartUp;
VAR i  : Integer;
BEGIN
   DefineViewPort(1, 0.03, 0.97, 0.07, 0.9);
   DefineViewPort(2, 0.03, 0.97, 0.9,0.95);
   DefineViewPort(3, 0.03, 0.97, 0.07, 0.93);
   DefineScale(2, 0, 1, 0, 2);
   SetUpOortMenu;
   SetUpHotKeys;
   SetUpFieldHotKeys;
   SetUpCUPSKeys;
   ShowCUPSKeys := True;
   CUPSKeys.Display;
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
   G := 0.00449897; {Constant of gravitation.}
   Aspect := 0.7; {Adjusts for vertical and horizontal scales on screen.}
   Start := True;
   ShowConstants := False;
   MakeGalaxy(CircVel, CircRad, Rad, Angle, Speed, GalRadius, nStar, Num,
               GalaxyMade);
   PlotGalaxy;
   Start := False;
  { Delay(2000);}
   {Delay(2000 div 62, Tick);}
   ShowHelp('HELPGals.HLP', 'Oort Hello');
   OortMenu.RowActivate(3,2, False);
   OortMenu.RowActivate(3,3, False);
   OortMenu.RowActivate(4,1, False);
   OortMenu.RowActivate(4,2, False);
   LightMenu := True;
   ExitRequest := False;
END;


{------------------------------------------------------------------------}

BEGIN
{  CUPSinit;}
   StartUp;
   REPEAT
      CheckForEvents;
      IF ShowCUPSKeys THEN
              IF CUPSKeys.Pressed(Key) THEN HandleCUPSKeys(Key);
      IF LightMenu THEN
      BEGIN
         LightMenu := False;
         IF OortMenu.Chosen THEN HandleOortMenu;
      END
      ELSE
         IF OortMenu.Activated THEN HandleOortMenu;
   UNTIL ExitRequest;
   OortMenu.Done;
{  CUPSDone;}
   {Dispose(OortMenu);}
   {Dispose(HotKeys);}
   {Dispose(FieldHotKeys);}
end;

BEGIN
END.