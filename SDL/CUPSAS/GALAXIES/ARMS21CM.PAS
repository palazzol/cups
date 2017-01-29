    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1994.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program to display profiles of 21cm           *)
    (*       emission lines due to the spiral arms         *)
    (*       of a galaxy.                                  *)
    (*                                                     *)
    (*******************************************************)

Unit Arms21cm;  {Arms21cm.PAS}
{$M 64000,0,327680}
{$O+}
{$F+}
interface
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;

procedure RunArms21cm;

implementation

procedure RunArms21cm;
{Units: Mass:   10^9 solar masses.
        Length: Kiloparsec.
        Time:   10^6 years.}

TYPE
   VelVector = Array[1..100] of Real;
   DatVector = Array[1..2200] of Single;
   IntVector = Array[1..100] of Integer;
   GalVector = Array[1..5] of Real;
   ArmArray = Array[1..5, 0..50] of Single;
   MaxArm = Array[1..5] of Integer;

VAR
   G                                    : Real; {Constant of gravitation.}
   Aspect                               : Real;{Parameter fo adjust horizontal and vertical scales.}
   Tick                                 : Longint;  {Useless parameter for Mac Delay.}
   CircVel, CircRad                     : VelVector;
   Rad, Angle, CosAngle, SinAngle, Speed: DatVector;
   GalRadius                            : Real; {Radius of visible the galaxy.}
   R0                                   : Real; {Distance of the Sun from the galactic center.}
   nStar                                : Integer; {Number of visible stars.}
   Num                                  : IntVector;
   SphVis                               : Array[1..5] OF Integer;
   SphHomogeneous                       : Array[1..5] OF Boolean;
   NumSph                               : Integer; {Number of spheroids.}
   SphMaj, SphMin, SphMass              : GalVector; {Dimensions and masses of the spheroids.}
   SphNum                               : Real;
   CMass                                : Real;   {Central mass.}
   xArm, yArm                           : ArmArray;
   xSun, ySun                           : Real;
   dx, dy                               : Real;
   xSc, ySc                             : Integer;
   xCenter, yCenter                     : Real;
   Segments                             : MaxArm;
   NArm                                 : Integer;
   GalaxyMade, ArmsMade, SunLocated     : Boolean;
   DefaultArmsDrawn                     : Boolean;
   Start                                : Boolean;
   LightMenu                            : Boolean;
   SeeProfiles                          : Boolean;
   ExitRequest                          : Boolean;
   ChooseLong                           : Boolean;
   GalLong, GalLongDegrees              : Real;
   GalLongTemp                          : Real;
   xRV, yRV, x21cm, y21cm               : Real;
   SolarDistance                        : Real;
   GalMenu                              : TMenu;
   Sliders                              : TSliders;
   HotKeys                              : THotKeys;
   CUPSKeys                             : THotKeys;
   ShowCUPSKeys                         : Boolean;
   Key                                  : Byte;

PROCEDURE SetUpDefaultInput; FORWARD;
PROCEDURE StartUp; FORWARD;

{------------------------------------------------------------------------}


PROCEDURE SetUpGalMenu;
BEGIN
   {New(GalMenu);}
   With GalMenu DO
   BEGIN
      Init;
      Column(1, 'File');
       row(1,1, 'About Program');
       row(1,2, 'About CUPS');
       row(1,3, 'Configuration');
       row(1,4, 'Exit this module');
      Column(2, 'Help');
       row(2,1, 'General information');
       row(2,2, '21 cm line profiles');
       row(2,3, 'Rotation curve');
      Column(3, 'Galaxy');
       row(3,1, 'Enter data for a galaxy');
       row(3,2, 'Plot the galaxy using current data');
       row(3,3, 'Plot default galaxy and arms');
       row(3,4, 'Store rotation curve data in a file');
      Column(4, 'Arms');
       row(4,1, 'Draw a spiral arm');
       row(4,2, 'Draw the default arms');
      Column(5, 'ViewPoint');
       row(5,1, 'Click on position of viewer');
       row(5,2, 'Select new position of viewer');
      Column(6, 'Profiles');
       row(6,1, 'See 21 cm profiles');
   END;
   GalMenu.Display;
END;

{-----------------------------------------------------------------------}

PROCEDURE SetUpHotKeys;
BEGIN
   {New(HotKeys);}
   WITH HotKeys DO
   BEGIN
      HotKeys.Init(5);
      Key[1] := 'F1-Help';
      Key[2] := 'F2-Next';
      Key[3] := 'F3-Preceding';
      Key[4] := 'F4-Select';
      Key[5] := 'F10-Menu';
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
{-----------------------------------------------------------------------}

PROCEDURE SetUpGalMenu1;  {Galactic data OK. Sun not selected.}
VAR
   i: Integer;
BEGIN
   GalMenu.RowActivate(3,2, True);
   GalMenu.RowActivate(3,4, True);
   GalMenu.RowActivate(4,1, True);
   GalMenu.RowActivate(4,2, True);
   GalMenu.RowActivate(5,1, True);
   GalMenu.RowActivate(5,2, False);
   GalMenu.RowActivate(6,1, False);
   GalMenu.Display;
END;

{-----------------------------------------------------------------------}

PROCEDURE SetUpGalMenu2;  {No galactic data.}
VAR
   i: Integer;
BEGIN
   GalMenu.RowActivate(3,2, False);
   GalMenu.RowActivate(3,4, False);
   GalMenu.RowActivate(4,1, False);
   GalMenu.RowActivate(4,2, False);
   GalMenu.RowActivate(5,1, False);
   GalMenu.RowActivate(5,2, False);
   GalMenu.RowActivate(6,1, False);
   {GalMenu.Display; }
END;

{-----------------------------------------------------------------------}

{----------------Procedure for setting up a galaxy----------------------}

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
   ClearMuppetPort;
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
   xOld, yOld, xNew, yNew: Real;
   DelTheta, CosDeltheta, SinDelTheta: Real;
BEGIN
   StarColor := 0;
   GraphBackColor := Blakk;
   HideMouse;
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
      StarPlot(CosAngle[i], SinAngle[i], StarColor);
      {PlotSymbol(CosAngle[i], SinAngle[i], '.');}
   END;
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
   SetColor(Blue);
   SetFillStyle(1, Blue);
   Map(0, 0, xSc, ySc);
   FillEllipse(xSc, ySc, 5, 4);
   ShowMouse;
   DefaultArmsDrawn := False;
END;

{------------------------------------------------------------------------}

PROCEDURE PlotGalaxyAndArms;
var i,j : integer;
BEGIN
   IF NOT DefaultArmsDrawn THEN
      PlotGalaxy
   ELSE
   BEGIN
      PlotGalaxy;
      DefaultArmsDrawn := True;
   END;
   IF SunLocated THEN
   BEGIN
      SetColor(Yellow);
      SetFillStyle(1, Yellow);
      Map(xSun, ySun, xSc, ySc);
      FillEllipse(xSc, ySc, 5, 4);
   END;
   SetColor(Whitte);
   SetLineStyle(Solidln, 0, ThickWidth);
   FOR i := 1 TO NArm DO
   FOR j := 1 TO Segments[i] DO
   BEGIN
      PlotLine(xArm[i, j-1], yArm[i, j-1], xArm[i, j], yArm[i, j]);
   END;
   SetLineStyle(Solidln, 0, NormWidth);
   IF SunLocated THEN
   BEGIN
      GalMenu.Rowactivate(5,1, False);
      GalMenu.Rowactivate(5,2, True);
   END
   ELSE
   BEGIN
      GalMenu.Rowactivate(5,1, True);
      GalMenu.Rowactivate(5,2, False);
   END;
   IF NArm = 5 THEN
      GalMenu.RowActivate(4,1, False)
   ELSE
      GalMenu.RowActivate(4,1, True);
   IF DefaultArmsDrawn OR (nArm > 3) THEN
      GalMenu.RowActivate(4,2, False)
   ELSE
      GalMenu.RowActivate(4,2, True);
   IF SunLocated AND (NArm > 0) THEN
      GalMenu.RowActivate(6,1, True);
   {LightMenu := True;}
END;

{--------------------Procedures for making galactic arms-----------------}

PROCEDURE DrawDefaultArms;
VAR
   dTheta, Theta: Real;
   j,i : integer;
BEGIN
   DefaultArmsDrawn := True;
   nArm := nArm + 2;
   dTheta := 3.14159/10;
   Theta := 0;
   Segments[1] := 30;
   Segments[2] := 30;
   FOR j := 0 to 30 DO
   BEGIN
      xArm[1,j] := 0.9*(1 + Theta)*Cos(Theta);
      yArm[1,j] := 0.9*(1 + Theta)*Sin(Theta);
      xArm[2,j] := - xArm[1,j];
      yArm[2,j] := - yArm[1,j];
      Theta := Theta + dTheta;
   END;
   SetColor(Whitte);
   SetLineStyle(SolidLn, 1, ThickWidth);
   FOR i := 1 TO 2 DO
   BEGIN
      FOR j := 1 TO 30 DO
         PlotLine(xArm[i,j-1], yarm[i,j-1], xArm[i,j], yarm[i,j]);
   END;
   SetLineStyle(SolidLn, 1, NormWidth);
END;

{------------------------------------------------------------------------}

Procedure MakeArm(GalRadius: Real;
                   NArmIn: Integer;
                   VAR xArm, yArm: ArmArray;
                   VAR NArm: Integer;
                   VAR Segments: MaxArm);

VAR
   xSC, ySc: Integer;
   xOld, yOld, xNew, yNew: Real;
   Button: Integer;
   Segment: Integer;
   ArmNade, SunLocated: Boolean;

{------------------------------------------------------------------------}

Procedure DrawArm(GalRadius: Real; NArm: Integer;
                  VAR xArm, yArm: ArmArray;
                  VAR Segment: Integer);
VAR
   XCoord, YCoord: Real;
   xm, ym: Integer;
   Twitch: Real;
   Ins: Boolean;
   PositionOK: Boolean;
   xSc, ySc: Integer;
   ArmStr, SegStr: String;

BEGIN
   GraphBackColor := Blue;
   SetColor(White);
   HideMouse;
   OpenViewPort(2);
   SelectScale(2);
   IF nArm = 5 THEN
      BEGIN
         Map(0.1, 0.8, xSc, ySc);
         OutTextxy(xSc, ySc, 'This is your final arm');
      END
      ELSE
      BEGIN
         Map(0.1, 1.8, xSc, ySc);
         OutTextxy(xSc, ySc,
              '     Click on arm segments. Double click to end an arm.');
         Map(0.1, 0.8, xSc, ySc); 
         OutTextxy(xSc, ySc,
              '          Maximum of 50 clicks. Maximum of 5 arms.');
      END;
   ShowMouse;
   SelectScale(1);
   SelectViewPort(1);
   ArmStr := NumStr(NArm, 1, 0);
   Segment := 0;
   Twitch := 0.05;
   SetColor(Whitte);
   SetLineStyle(Solidln, 0, ThickWidth);
   PositionOK := True;
   REPEAT
      WaitOnMouseClick;
      MousePosn(xOld, yOld, 1, 1, Ins);
      IF xOld*xOld + yOld*yOld > Sqr(GalRadius/1.1) THEN
          BEGIN
             Beep;
             PositionOK := False;
          END
          ELSE PositionOK := True;
   UNTIL PositionOK = True;
   xArm[nArm, Segment] := xOld; yArm[nArm, Segment] := yOld;
   REPEAT
      SetWriteMode(XORPut);
      REPEAT
         {WaitOnMouseClick;}
         Button := 0;
         MouseGlobalPosn(xm, ym, Button);
         MousePosn(xNew, yNew, 1, 1, Ins);
         PlotLine(xOld, yOld, xNew, yNew);
         IF xNew*xNew + yNew*yNew > Sqr(GalRadius/1.1) THEN Beep;
         Delay(10);
         {Delay(1, Tick);}
         PlotLine(xOld, yOld, xNew, yNew);
      UNTIL (Button <> 0) AND (xNew*xNew + yNew*yNew < Sqr(GalRadius/1.1));
      SetWriteMode(CopyPut);
      StaticPause;
      IF (Abs(xOld - xNew) + Abs(yOld - yNew)  < Twitch) OR
              Event.DoubleClicked THEN
      BEGIN
         GraphBackColor := Blue;
         OpenViewPort(2);
         SelectViewPort(1);
         SetLineStyle(Solidln, 0, NormWidth);
         Exit;
         {Exit(DrawArm);}
      END;
         Segment := Segment + 1;
         SegStr := NumStr(Segment+1, 2, 0);
         xArm[nArm, Segment] := xNew; yArm[nArm, Segment] := yNew;
         PlotLine(xOld, yOld, xNew, yNew);
         xOld := xNew; yOld := yNew;
         OpenViewPort(2);
         SelectScale(2);
         SetColor(Yellow);
         Map(0.2, 0.8, xSc, ySc);
         OutTextxy(xSc, ySc,
              'Maximum of 50 clicks. Maximum of 5 arms.');
         Map(0.2, 1.8, xSc, ySc);
         OutTextxy(xSc, ySc, Concat('        Arm  ', ArmStr,
            '       Segment   ', SegStr ,'                 '));
         SelectViewPort(1);
         SelectScale(1);
         SetColor(Whitte);
   UNTIL (Segment = 50);
   IF Segment = 50 THEN Beep;
   SetLineStyle(Solidln, 0, NormWidth);
   GraphBackColor := Blue;
   OpenViewPort(2);
   SelectViewPort(2);
END;


{------------------------------------------------------------------------}      

BEGIN  {Main Program for Procedure MakeArm.}
   Delay(1000);
   {Delay(1000 div 62, Tick);}
   nArm := NArmIn;
               IF nArm = 5 THEN BEGIN Beep; Exit; {Exit(MakeArm);} END
               ELSE
               BEGIN
                  nArm := nArm + 1; 
                  DrawArm(GalRadius, nArm, xArm, yArm, Segment);
                  Segments[nArm] := Segment;
               END;
END;

{-----------------End of procedure for making arms------------------}
{-----------------Procedure for locating the "Sun"------------------}

Procedure LocateSun(VAR xSun, ySun: Real; VAR R0: Real);
VAR
   nRay: Integer;
   Phi, xSlope, ySlope: Real;
   t, t1, t2: Real;
   Ins: Boolean;
   MaxX, MinX, MaxY, MinY: Real;
   xSc, ySc, xSc1, ySc1: Integer;

BEGIN
   OpenViewPort(2);
   SelectScale(2);
   SetColor(Yellow);
   Map(0.1, 1.3, xSc, ySc); 
   OutTextxy(xSc, ySc, 'Click on a position in the galaxy');
   Delay(1000);
   {Delay(1000 div 62, Tick);}
   SelectScale(1);
   SelectViewPort(1);
   MinX :=  -GalRadius-1;
   MaxX :=  GalRadius+1;
   MinY := (-GalRadius-1)*Aspect;
   MaxY := (Galradius+1)*Aspect;
   SetLineStyle(DottedLn, 0, NormWidth);
   SunLocated := False;
   REPEAT
      WaitOnMouseClick;
      MousePosn(xSun, ySun, 1, 1, Ins);
      R0 := Sqrt(xSun*xSun + ySun*ySun);
      IF R0 < GalRadius/1.1 THEN SunLocated := True ELSE Beep;
   UNTIL SunLocated = True;
   HideMouse;
   SetColor(Yellow);
   SetFillStyle(1, Yellow);
   Map(xSun, ySun, xSc, ySc);
   FillEllipse(xSc, ySc, 5, 4);
   FOR nRay := 0 TO 23 DO
   BEGIN
      Phi := nRay*3.14159/12;
      Map(xSun, ySun, xSc, ySc);
      xSlope := (xSun*Cos(Phi) - ySun*Sin(Phi));
      ySlope := (ySun*Cos(Phi) + xSun*Sin(Phi));
      IF xSlope > 0 THEN t1 := (MaxX - xSun)/xSlope
                    ELSE t1 := (MinX - xSun)/xSlope;
      IF ySlope > 0 THEN t2 := (MaxY - ySun)/ySlope
                    ELSE t2 := (MinY - ySun)/ySlope;
      IF t1 < t2 THEN t := t1 Else t := t2;
      Map(xSun + t*xSlope, ySun + t*ySlope, xSc1, ySc1);
      Line(xSc, ySc, xSc1, ySc1);
      {LineTP(xSc, ySc, xSc1, ySc1);}
   END;
   ShowMouse;
   OpenViewPort(2);
   SelectViewPort(1);
   GalMenu.Display;
END;


{-------------End of procedure for locating the "Sun----------------}
{-------Start of procedure for showing radial velocity profiles-----}
{----------------------and 21 cm line profiles.---------------------}

Function FindCircVel(R, GalRadius: Real; CircVel: VelVector): Real;
VAR
   i: Integer;
BEGIN
   i := Trunc(101*R/GalRadius + 0.5);
   if i<1 then i:=1; {last minute correction by Jarek T.}
   FindCircVel := CircVel[i] +
                  (CircVel[i+1] - CircVel[i])*(R - GalRadius*(i + 0.5)/101)
                  /(GalRadius/101);
END;


{-------------------------------------------------------------------}

Procedure RVProfile(R0, GalRadius, GalLong: Real; CircVel: VelVector);
VAR
   Dist, MaxDist, DelDist: Real;
   R: Real;
   RV: Real;
   nRV: Integer;
   i: Integer;
   xTick                   : Integer;
   AngVelSun, AngVel: Real;
   x, y: DataVector;
   NumData: Word;
   MaxRV, MinRV: Real;
   xSc, ySc: Integer;
   RVString: String;

BEGIN
   ClearMuppetPort;
   AngVelSun := FindCircVel(R0, GalRadius, CircVel);
   MaxDist := R0*Cos(GalLong) + Sqrt(Sqr(GalRadius/1.1) - Sqr(R0*Sin(GalLong)));
   DelDist := MaxDist/100;
   Dist := 0;
   MaxRV := 0; MinRV := 0;
   FOR i := 1 TO 100 DO
   BEGIN
      Dist := Dist + DelDist;
      R := Sqrt(R0*R0 + Dist*Dist - 2*R0*Dist*Cos(GalLong));
      AngVel := FindCircVel(R, GalRadius, CircVel);
      RV := 977*R0*Sin(GalLong)*(AngVel - AngVelSun);
      IF RV > MaxRV THEN MaxRV := RV;
      IF RV < MinRV THEN MinRV := RV;
      x[i] := Dist;
      y[i] := RV;
   END;
      GraphBackColor := Red;
      SetColor(Yellow);
      HideMouse;
      OpenViewPort(2);
      SelectScale(2);
      SetBkColor(Blakk);
      IF GalLong > 2*Pi THEN GalLong := GalLong - 2*Pi;
      RVString := NumStr(Trunc(180*GalLong/Pi + 0.001), 4, 0);
      RVString := Concat('Galactic longitude ' , RVString , ' degrees');
      Map(0.3, 1, xSc, ySc);
      OutTextxy(xSc, ySc, RVString);
      NumData := 100;
      GraphBackColor := Black;
      HideMouse;
      OpenViewPort(3);
      ShowMouse;
      DefineScale(3, 0, MaxDist, MinRV-5, MaxRV+10);
      SelectScale(3);
      Map(x[50], MaxRV+5, xSc, ySc);
      PrintXY(0.68, 0.87, 'Radial Velocity Profile.');
      ShowMouse;
      XTick := Trunc(GalRadius/5);
      Axis(0.0, 0.0, xTick, 20);
      FOR nRV := 2 TO NumData DO
                PlotLine(x[nRV-1], y[nRV-1], x[nRV], y[nRV]);
      Map(x[90], -5, xSc, ySc);
      OutTextxy(xSc, ySc, 'Kpc'); 
      Map(x[1], MaxRv+5, xSc, ySc); 
      OutTextxy(xSc, ySc, 'km/sec'); 
END;


   
{------------------------------------------------------------------------}



{------------------------------------------------------------------------}

Procedure H21cmProfile(R0, GalRadius, GalLong: Real;
                       nArm: Integer;
                       CircVel: VelVector;
                       xArm, yArm: ArmArray;
                       xSun, ySun: Real;
                       Segments: MaxArm);

VAR
   RVCross: VelVector;
   nCross: Integer;
   xSlope, ySlope: Real;

Function FindCircVel(R, GalRadius: Real; CircVel: VelVector): Real;
VAR
   i: Integer;
BEGIN
   i := Trunc(101*R/GalRadius + 0.5);
   FindCircVel := CircVel[i] +
                  (CircVel[i+1] - CircVel[i])*(R - GalRadius*(i + 0.5)/101)
                  /(GalRadius/101);
END;

{------------------------------------------------------------------------}

Function Sign(x, y, xSlope, ySlope: Real; xSun, ySun: Real): Real;
BEGIN
   Sign := (y - ySun)*xSlope - (x - xSun)*ySlope;
END;

{------------------------------------------------------------------------}

Procedure FindRVCross(nA, nSeg: Integer;
                      Cross1, Cross2, GalRadius, R0, GalLong: Real;
                      CircVel: VelVector;
                      VAR RV: Real;
                      VAR X1, Y1: Real);
VAR
   C1, C2, R, AngVel, AngVelSun: Real;

BEGIN
   C1 := Abs(Cross1);
   C2 := Abs(Cross2);
   X1 := (C2*xArm[nA, nSeg-1] + C1*xArm[nA, nSeg])/(C2 + C1);
   Y1 := (C2*yArm[nA, nSeg-1] + C1*yArm[nA, nSeg])/(C2 + C1);
   R := Sqrt(Sqr(X1) + Sqr(Y1));
   AngVelSun := FindCircVel(R0, GalRadius, CircVel);
   AngVel := FindCircVel(R, GalRadius, CircVel);
   RV := 977*R0*Sin(GalLong)*(AngVel - AngVelSun);
END;

{------------------------------------------------------------------------}

Procedure FindArmCross(xArm, yArm: ArmArray; Segments: MaxArm; nArm: Integer;
                       xSun, ySun: Real; 
                       R0, GalLong: Real;
                       CircVel: VelVector;
                       VAR nCross: Integer; VAR RVCross: VelVector);
VAR
   nA, nSeg: Integer;
   Cross1, Cross2: Real;
   X1, Y1: Real;
   RVelocity: Real;

BEGIN
   xSlope := xSun*Cos(GalLong) - ySun*Sin(GalLong);
   ySlope := xSun*Sin(GalLong) + ySun*Cos(GalLong);
   nCross := 0;
   FOR nA := 1 TO nArm DO
   BEGIN
      Cross1 := Sign(xArm[nA, 0], yArm[nA, 0], xSlope, ySlope, xSun, ySun);
      FOR nSeg := 1 TO Segments[nA] DO
      BEGIN
         Cross2 :=  Sign(xArm[nA, nSeg], yArm[nA, nSeg], xSlope, ySlope, xSun, ySun);
         IF (Cross1*Cross2 < 0) THEN
         BEGIN
            FindRVCross(nA, nSeg, Cross1, Cross2, GalRadius, R0, GalLong,
                           CircVel, RVelocity, x1, y1);
            IF (Y1 - ySun)*ySlope + (X1- xSun)*xSlope < 0
            THEN
               BEGIN
                  nCross := nCross + 1;
                  RVCross[nCross] := RVelocity;
               END;
         END;
         Cross1 := Cross2;
      END;
   END;
END;

{------------------------------------------------------------------------}

Procedure Plot21cmLine(nCross: Integer; RVCross: VelVector);
VAR
   H: Real;
   x, y: DataVector;
   Line, MaxLine: Real;
   xSc, ySc: Integer;
   LineString: String;
   RV, MinRV, MaxRV, DelRV: Real;
   nRV, nC, NumData: Integer;
   TickMark: Integer;
   TickLength: Real;
   TickString: String;

BEGIN
   MinRV := -200; MaxRV := 200;
   H := 5; {Parameter for the broadening of the lines.}
   MaxLine := 0;
   DelRV := (MaxRV - MinRV)/100;
   FOR nRV := 1 TO 100 DO
   BEGIN
      Line := 0;
      RV := MinRV + nRV*DelRV;
      IF nCross > 0 THEN
      BEGIN
         FOR nC := 1 TO nCross DO
         BEGIN
            IF Abs(RV - RVCross[nc]) < 2*H THEN
               Line := Line + Exp(-Sqr(RV - RVCross[nC])/Sqr(H));
         END;
      END;
      x[nRV] := RV;
      y[nRV] := Line;
      IF MaxLine < Line THEN MaxLine := Line;
   END;
      NumData := 100;
      GraphBackColor := DarkGray;
      DefineViewPort(6, 0, 1, 0, 0.1);
      DefineScale(6, MinRV/0.9, MaxRV/0.9, 0, 2);
      OpenViewPort(4);
      PrintXY(0.28, 0.43, 'Press the mouse to show coordinates');
      DefineScale(4, MinRV, MaxRV, 0, MaxLine+2);
      SelectScale(4);   
      Map(x[60], MaxLine+1.5, xSc, ySc);
      OutTextxy(xSc, ySc, '21 cm Line Profile.');
      Axis(0.0, 0.0, 40, 10); 
      {SetColor(Whitte);
      PlotLine(MinRv, 0,MaxRv, 0);
      PlotLine(0, 0, 0, MaxLine+2);
      SelectViewPort(6);
      SelectScale(6);
      Map(0, 1.5, xSc, ySc);
      OutTextxy(xSc, ySc, '0');
      SelectViewPort(4);
      SelectScale(4);
      TickMark := 40;
      TickLength := (MaxLine + 2)/20;
      REPEAT
         PlotLine(TickMark, 0, TickMark, TickLength);
         SelectViewPort(6);
         SelectScale(6);
         Map(TickMark, 1.5, xSc, ySc);
         Str(TickMark, TickString);
         OutTextxy(xSc-10, ySc, TickString);
         SelectViewPort(4);
         SelectScale(4);
         TickMark := TickMark + 40;
      UNTIL TickMark > MaxRV;
      TickMark := - 40;
      REPEAT
         PlotLine(TickMark, 0, TickMark, TickLength);
         SelectViewPort(6);
         SelectScale(6);
         Map(TickMark, 1.5, xSc, ySc);
         Str(TickMark, TickString);
         OutTextxy(xSc-10, ySc, TickString);
         SelectViewPort(4);
         SelectScale(4);
         TickMark := TickMark - 40;
      UNTIL TickMark < MinRV;  }
      Map(x[90], 0.1, xSc, ySc);
      OutTextxy(xSc, ySc-10, 'km/sec');
      SetColor(Yellow);
      FOR nRV := 2 TO NumData DO PlotLine(x[nRV-1], y[nRV-1], x[nRV], y[nRV]);
END;

{------------------------------------------------------------------------}

BEGIN   {Main program for Procedure H21cmProfile.}
   FindArmCross(xArm, yArm, Segments, nArm, xSun, ySun,
                  R0, GalLong, CircVel, nCross, RVCross);  
   Plot21cmLine(nCross, RVCross);
END;

{------------------------------------------------------------------------}

{------------------------------------------------------------------------}



PROCEDURE DrawRay(GalLongTemp: Real; RayColor: Word);
VAR
   xSc, ySc, xSc1, ySc1: Integer;
   xSlope, ySlope: Real;
   t1, t2, t: Real;
   MinX, MaxX, MinY, MaxY: Real;
BEGIN
   SelectViewPort(5);
   SelectScale(1);
   SetColor(RayColor);
   Map(xSun, ySun, xSc, ySc);
   MinX :=  -GalRadius-1;
   MaxX :=  GalRadius+1;
   Miny := (-GalRadius-1)*Aspect;
   MaxY := (Galradius+1)*Aspect;
   xSlope := (xSun*Cos(GalLongTemp + Pi) -
              ySun*Sin(GalLongTemp + Pi));
   ySlope := (ySun*Cos(GalLongTemp + Pi) +
              xSun*Sin(GalLongTemp + Pi));
   IF xSlope > 0 THEN t1 := (MaxX - xSun)/xSlope
                 ELSE t1 := (MinX - xSun)/xSlope;
   IF ySlope > 0 THEN t2 := (MaxY - ySun)/ySlope
                 ELSE t2 := (MinY - ySun)/ySlope;
   IF t1 < t2 THEN t := t1 Else t := t2;
   Map(xSun + t*xSlope, ySun + t*ySlope, xSc1, ySc1);
   Line(xSc, ySc, xSc1, ySc1);
   {LineTP(xSc, ySc, xSc1, ySc1);}
END;

{------------------------------------------------------------------------}

PROCEDURE PlotArms(GalLong: Real; RayColor: Word);
VAR
   xSc, ySc, xSc1, ySc1: Integer;
   xSlope, ySlope: Real;
   t1, t2, t: Real;
   MinX, MaxX, MinY, MaxY: Real;
   i, j: Integer;

BEGIN
   GraphBackColor := Blakk;
   SetBkColor(Blakk);
   OpenViewPort(5);
   SelectScale(1);
   PrintXY(0.05, 0.86, 'Solar         distance');
   DrawRay(GalLong, RayColor);
   SetColor(Green);
   SetFillStyle(1, Green);
   Map(0, 0, xSc, ySc);
   FillEllipse(xSc, ySc, 5, 4); 
   SetColor(Yellow);
   SetFillStyle(1, Yellow);
   Map(xSun, ySun, xSc, ySc);
   FillEllipse(xSc, ySc, 5, 4);
   SetColor(Whitte);
   SetLineStyle(Solidln, 0, ThickWidth);
   FOR i := 1 TO NArm DO
      FOR j := 1 TO Segments[i] DO
      BEGIN
         PlotLine(xArm[i, j-1], yArm[i, j-1], xArm[i, j], yArm[i, j]);
      END;
   SetLineStyle(Solidln, 0, NormWidth);
END;

{------------------------------------------------------------------------}

PROCEDURE SelectLongitude;
VAR
   GalLongTemp: Real;
   LongChosen: Boolean;
BEGIN
   GalLongDegrees := 180*GalLong/3.14159;
   Sliders.Create(1, 0, 360, GalLongDegrees, 0, 0.92, 1, 1,
                                   '0', '360', 'Galactic longitude', False);
   CloseViewPort(2);
   Message('Use the slider to select different longitudes. Press F4-Observe, when ready.');
   Sliders.DrawAll;
   PlotArms(GalLong, GetBkColor);
   ShowMouse;
   SetWriteMode(XORPut);
   {DrawRay(GalLong, Whitte);}
END;

{------------------------------------------------------------------------}


Procedure ShowProfiles;

BEGIN   {Main Program for Procedure ShowProfiles.}
   SeeProfiles := True;
   GalLong := Pi/12;
   RVProfile(R0, GalRadius, GalLong, CircVel);
   H21cmProfile(R0, GalRadius, GalLong, nArm,
                       CircVel, xArm, yArm, xSun, ySun, Segments);
   PlotArms(GalLong, Yellow);
END;

{------------------------------------------------------------------------}

PROCEDURE RecordMouse;
VAR
   InsideRV, Inside21cm, InsideGal     : Boolean;
   xMouse, yMouse                      : Real;
   Distance                            : Real;
   xint, yint                          : Integer;
   RecordPosn                          : Boolean;
   Button                              : Integer;

BEGIN
   MouseGlobalPosn(xint, yint, Button);
   IF Button = 0 THEN Exit;
   IF event.MouseClicked{(xint, yint)} THEN
      RecordPosn := True  Else RecordPosn := False;
   RecordPosn := False;
   MousePosn(xMouse, yMouse, 3, 3, InsideRV);
   IF InsideRV AND ((Abs(xRV-xMouse) > 0.1) OR (Abs(yRV-yMouse) > 1)) THEN
   BEGIN
      SelectScale(3);
      RubOutXY(0.76, 0.85, 18, Black);
      IF RecordPosn THEN
      BEGIN
        RubOutXY(0.76, 0.82, 18, Black);
        PrintXY(0.76, 0.82, '('+Numstr(xMouse, 6, 2)+', '+
                                          NumStr(yMouse, 6, 2)+')');
        Delay(1000); ClearMKBuffers;
      END
      ELSE
      PrintXY(0.76, 0.85, '('+Numstr(xMouse, 6, 2)+', '+
                                          NumStr(yMouse, 6, 2)+')');
      xRV := xMouse; yRV := yMouse;
   END;
   MousePosn(xMouse, yMouse, 4, 4, Inside21cm);
   IF Inside21cm AND (Abs(x21cm - xMouse) > 0.1) THEN
   BEGIN
      SelectScale(4);
      RubOutXY(0.2, 0.35, 10, DarkGray);
      IF RecordPosn THEN
      BEGIN
         RubOutXY(0.2, 0.32, 10, DarkGray);
         PrintXY(0.1, 0.32, 'Rad vel: '+Numstr(xMouse, 6, 2));
         Delay(1000);  ClearMKBuffers;
      END
      ELSE
      PrintXY(0.1, 0.35, 'Rad vel: '+Numstr(xMouse, 6, 2));
      x21cm := xMouse;
   END;
   MousePosn(xMouse, yMouse, 5, 1, InsideGal);
   Distance := Sqrt(Sqr(xSun-xMouse) + Sqr(ySun-yMouse));
   IF InsideGal AND (Abs(Distance - SolarDistance) > 0.1) THEN
   BEGIN
      SelectScale(1);HideMouse;
      RubOutXY(0.27, 0.84, 6, Black);
      IF RecordPosn THEN
      BEGIN
         RubOutXY(0.27, 0.81, 6, Black);
         PrintXY(0.27, 0.81, NumStr(Distance, 4,1));  ShowMouse;
         Delay(1000); ClearMKBuffers;
      END
      ELSE
      PrintXY(0.27, 0.84, NumStr(Distance, 4,1));  ShowMouse;
      SolarDistance := Distance;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE RecordRotationCurve;

TYPE
   PointRecord = Record
                    x, y : Real;
                 END;

VAR
   Filename               : String;
   F1                     : File of PointRecord;
   Points                 : PointRecord;
   i                      : Integer;
   Radius, RV             : Real;

FUNCTION MakeFile : Boolean;
VAR
   SaveRotationCurve : TInputScreen;
BEGIN
   WITH SaveRotationCurve DO
   BEGIN
      Init;
      DefineInputPort(0.2, 0.8, 0.4, 0.7);
      LoadLine('');
      LoadLine('  Data for the rotation curve will be');
      LoadLine('  stored in a file.');
      LoadLine('      File name: "Rotate.DAT"');
      LoadLine('');
      LoadLine('       [  OK  ]       [Cancel]');
      AcceptScreen;
      FileName := GetString(1);
      Done;
   END;
END;

BEGIN
   IF (MakeFile) THEN
   BEGIN
      Assign(F1, Filename);
      ReWrite(F1);
      FOR i := 1 TO 100 DO
      BEGIN
         Radius := (i + 0.5)*GalRadius/101;
         RV := 966*Radius*CircVel[i];
         Points.x := Radius;
         Points.y := RV;
         Write(F1, Points);
      END;
      Close(F1);
   END;
END;


{------------------------------------------------------------------------}

PROCEDURE HandleGalMenu;
VAR
   i,j: Integer;
BEGIN
   With GalMenu DO
   CASE ColChosen OF
      1: CASE RowChosen OF
            1: BEGIN
                  ShowHelp('HELPGals.HLP', 'Spiral Hello');
                  LightMenu := True;
               END;
            2: BEGIN
                  AboutCUPS;
                  LightMenu := True;
               END;
            3: Configuration;
            4: ExitRequest := True;
         END;
      2: CASE RowChosen OF
            1: BEGIN
                  ShowHelp('HELPGals.HLP', 'Spiral GeneralA');
                  ShowHelp('HELPGals.HLP', 'Spiral GeneralB');
                  LightMenu := True;
               END;
            2: BEGIN
                  ShowHelp('HELPGals.HLP', 'Spiral GeneralC');
                  ShowHelp('HELPGals.HLP', 'Spiral GeneralD');
                  LightMenu := True;
               END;
            3: BEGIN
                  ShowHelp('HELPGals.HLP', 'Spiral RotationCurve');
                  LightMenu := True;
               END;
         END;
      3: CASE RowChosen OF
            1:  BEGIN
                   MakeGalaxy(CircVel, CircRad, Rad, Angle, Speed, GalRadius, nStar, Num,
                           GalaxyMade);
                   IF GalaxyMade = False THEN
                   BEGIN
                      SetUpGalMenu2;
                      Announce('Data must be complete for galaxy to be shown');
                      LightMenu := True;
                      Exit;
                      {Exit(HandleGalMenu);}
                   END;
                   ClearMuppetPort;
                   SetUpGalMenu1;
                   PlotGalaxy;
                   nArm := 0;
                   SunLocated := False;
                   LightMenu := True;
                END;
            2: BEGIN
                  PlotGalaxy;
                  nArm := 0;
                  SunLocated := False;
                  DefaultArmsDrawn := False;
                  GalMenu.RowActivate(5,1, True);
                  GalMenu.RowActivate(5,2, False);
                  GalMenu.RowActivate(3,2, True);
                  GalMenu.RowActivate(3,4, True);
                  GalMenu.RowActivate(4,1, True);
                  GalMenu.RowActivate(4,2, True);
                  GalMenu.RowActivate(6,1, False);
                  LightMenu := True;
               END;
            3: BEGIN
                  SetUpDefaultInput;
                  StartUp;
               END;
            4: RecordRotationCurve;
         END;
      4: CASE RowChosen OF
            1: BEGIN
                  MakeArm(GalRadius, NArm, xArm, yArm, NArm, Segments);
                  ShowMouse;
                  IF SunLocated = True THEN
                  BEGIN
                     GalMenu.RowActivate(6,1, True);
                     GalMenu.RowActivate(5,1, False);
                     GalMenu.RowActivate(5,2, True);
                     IF nArm > 3 THEN
                        GalMenu.RowActivate(4,2, False);
                     IF nArm = 5 THEN
                        GalMenu.RowActivate(4,1, False);
                  END
                  ELSE
                  BEGIN
                     GalMenu.RowActivate(6,1, False);
                     GalMenu.RowActivate(5,1, True);
                     GalMenu.RowActivate(5,2, False);
                     IF nArm > 3 THEN
                        GalMenu.RowActivate(4,2, False);
                     IF nArm = 5 THEN
                        GalMenu.RowActivate(4,1, False);
                  END;
                  LightMenu := True;
               END;
            2: BEGIN
                  IF (nArm > 3) OR (DefaultArmsDrawn) THEN Beep
                  ELSE
                  BEGIN
                     DrawDefaultArms;
                     GalMenu.RowActivate(4,2, False);
                     ShowMouse;
                     IF SunLocated = True THEN
                     BEGIN
                        GalMenu.RowActivate(5,1, False);
                        GalMenu.RowActivate(5,2, True);
                        GalMenu.RowActivate(6,1, True);
                        IF nArm = 5 THEN
                           GalMenu.RowActivate(4,1, False);
                     END
                     ELSE
                     BEGIN
                        GalMenu.RowActivate(5,1, True);
                        GalMenu.RowActivate(5,2, False);
                        GalMenu.RowActivate(6,1, False);
                        IF nArm = 5 THEN
                           GalMenu.RowActivate(4,1, False);
                     END;
                  END;
                  LightMenu := True;
               END;
            END;
      5: CASE RowChosen OF
         1: BEGIN
               LocateSun(xSun, ySun, R0);
               GalMenu.RowActivate(5,1, False);
               GalMenu.RowActivate(5,2, True);
               IF NArm > 0 THEN
               BEGIN
                  GalMenu.RowActivate(6,1, True);
                  IF nArm = 5 THEN
                  BEGIN
                     GalMenu.RowActivate(4,1, False);
                     GalMenu.RowActivate(4,2, False);
                  END;
               END
               ELSE
               BEGIN
                  GalMenu.RowActivate(4,1, True);
                  GalMenu.RowActivate(4,2, True);
                  GalMenu.RowActivate(6,1, False);
               END;
               LightMenu := True;
            END;
         2: BEGIN
               IF NOT DefaultArmsDrawn THEN
                  PlotGalaxy
               ELSE
               BEGIN
                  PlotGalaxy;
                  DefaultArmsDrawn := True;
               END;
               SetColor(Whitte);
               SetLineStyle(Solidln, 0, ThickWidth);
               FOR i := 1 TO NArm DO
                  FOR j := 1 TO Segments[i] DO
                     BEGIN
                        PlotLine(xArm[i, j-1], yArm[i, j-1], xArm[i, j], yArm[i, j]);
                     END;
               SetLineStyle(Solidln, 0, NormWidth);
               LocateSun(xSun, ySun, R0);
               IF nArm > 0 THEN RowActivate(6, 1, True);
               GalMenu.RowActivate(5,1, False);
            END;
         END;
      6: BEGIN
            SetUpGalMenu2;
            ShowProfiles;
            SeeProfiles := True;
            ChooseLong := False;
            ShowCUPSKeys := False;
            CUPSKeys.Clear;
            WITH HotKeys DO Key[4] := 'F4-Select';
            HotKeys.BackColor := Blue;
            HotKeys.Display;
            Message('               Use the hot keys to observe different longitudes.');
         END;
      END; {Row CASE.}
 END;

{------------------------------------------------------------------------}

PROCEDURE HandleHotKeys(key: Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HELPGals.HLP', 'Spiral ArmHelp');
      2: BEGIN
            IF NOT ChooseLong THEN
            BEGIN
               GalLong := GalLong + Pi/12;
               IF GalLong > 2*Pi
                  THEN GalLong := GalLong - 2*Pi;
               RVProfile(R0, GalRadius, GalLong, CircVel);
               H21cmProfile(R0, GalRadius, GalLong, nArm, CircVel,
                                    xArm, yArm, xSun, ySun, Segments);
               PlotArms(GalLong, Yellow);
               HotKeys.Display;
            END;
         END;
      3: BEGIN
            IF NOT ChooseLong THEN
            BEGIN

               GalLong := GalLong - Pi/12;
               IF GalLong < 0
                  THEN GalLong := GalLong + 2*Pi;
               RVProfile(R0, GalRadius, GalLong, CircVel);
               H21cmProfile(R0, GalRadius, GalLong, nArm, CircVel,
                                    xArm, yArm, xSun, ySun, Segments);
               PlotArms(GalLong, Yellow);
               HotKeys.Display;
            END;
         END;
      4: BEGIN
            IF ChooseLong THEN
            BEGIN
               ChooseLong := False;
               SetWriteMode(CopyPut);
               WITH HotKeys DO Key[4] := 'F4-Select';
               HotKeys.BackColor := Blue;
               Sliders.Delete(1);
               RVProfile(R0, GalRadius, GalLong, CircVel);
               H21cmProfile(R0, GalRadius, GalLong, nArm, CircVel,
                                 xArm, yArm, xSun, ySun, Segments);
               PLotArms(GalLong, Yellow);
               HotKeys.Display;
            END
            ELSE
            BEGIN
               ChooseLong := True;
               WITH HotKeys DO Key[4] := 'F4-Observe';
               HotKeys.BackColor := Red;
               HotKeys.Display;
               SelectLongitude;
            END;
         END;
      5: BEGIN
            IF ChooseLong THEN
            BEGIN
               SetWriteMode(CopyPut);
               ChooseLong := False;
            END;
            SeeProfiles := False;
            HotKeys.Clear;
            CloseViewPort(3);
            CloseViewPort(4);
            CloseViewPort(5);
            ClearMuppetPort;
            PlotGalaxyAndArms;
            IF GalMenu.Chosen THEN HandleGalMenu;
         END;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleCUPSKeys(Key: Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HELPGals.HLP', 'Spiral Hello');
      2: IF GalMenu.Chosen THEN HandleGalMenu;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE StartUp;
VAR
   i, j: Integer;
   Theta, dTheta: Real;
   xSc, ySc: Integer;
BEGIN
   Start := True;
   MakeGalaxy(CircVel, CircRad, Rad, Angle, Speed, GalRadius, nStar, Num,
               GalaxyMade);
   PlotGalaxy;
   DefaultArmsDrawn := True;
   nArm := 2;
   dTheta := 3.14159/10;
   Theta := 0;
   Segments[1] := 30;
   Segments[2] := 30;
   FOR j := 0 to 30 DO
   BEGIN
      xArm[1,j] := 0.9*(1 + Theta)*Cos(Theta);
      yArm[1,j] := 0.9*(1 + Theta)*Sin(Theta);
      xArm[2,j] := - xArm[1,j];
      yArm[2,j] := - yArm[1,j];
      Theta := Theta + dTheta;
   END;
   SetColor(Whitte);
   SetLineStyle(SolidLn, 1, ThickWidth);
   FOR i := 1 TO 2 DO
   BEGIN
      FOR j := 1 TO 30 DO
         PlotLine(xArm[i,j-1], yarm[i,j-1], xArm[i,j], yarm[i,j]);
   END;
   SetLineStyle(SolidLn, 1, NormWidth);
   SetColor(Yellow);
   SetFillStyle(1, Yellow);
   xSun := 5; ySun := 3;
   R0 := Sqrt(xSun*xSun + ySun*ySun);
   Map(xSun, ySun, xSc, ySc);
   FillEllipse(xSc, ySc, 5, 4);
   Start := False;
   GalMenu.Display;
   SunLocated := True;
   GalMenu.Rowactivate(4,1, True);
   GalMenu.RowActivate(4,2, False); {Default arms drawn.}
   GalMenu.RowActivate(5,1, False); {Sun selected.}
   GalMenu.RowActivate(5,2, True);
   GalMenu.RowActivate(6,1, True);
   LightMenu := True;
END;

{------------------------------------------------------------------------}

PROCEDURE InitializeGraphics;
BEGIN
   DefineViewPort(1, 0.03, 0.97, 0.07, 0.9);
   DefineViewPort(2, 0.03, 0.97, 0.9,0.95);
   DefineViewPort(3, 0.4, 1, 0.5, 0.88);
   DefineViewPort(4, 0.05, 0.95, 0.1, 0.4);
   DefineScale(2, 0, 1, 0, 2);
   DefineViewPort(5, 0, 0.35, 0.5, 0.88);
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpDefaultInput;
VAR
   i: Integer;
BEGIN
   G := 0.00449897; {Constant of gravitation.}
   Aspect := 0.7;  {Parameter to adjust horizontal and vertical scales.}
   CMass := 10;
   SphNum := 5;
   NumSph := 5;
   FOR i := 1 TO 5 DO
   BEGIN
      SphMaj[i] := 2*i;
      SphMin[i] := i;
      SphMass[1] := i;
      SphVis[i] := 1;
      SphHomogeneous[i] := False;
   END;
   SetUpCUPSKeys;
   ShowCUPSKeys := True;
   SetUpGalMenu1;
   ExitRequest := False;
   ChooseLong := False;
END;

{------------------------------------------------------------------------}
BEGIN
{   CUPSinit;}
   {New(Sliders);}
   Sliders.Init;
   InitializeGraphics;
   SetUpGalMenu;
   SetUpHotKeys;
   SetUpGalMenu1;
   SetUpDefaultInput;
   StartUp;
   Delay(1000);
   CUPSKeys.Display;
   {Delay(2000 div 62, Tick);}
   ShowHelp('HELPGals.HLP', 'Spiral Hello');
   REPEAT
      CheckForEvents;

      IF SeeProfiles THEN RecordMouse;
      IF SeeProfiles AND HotKeys.Pressed(Key) THEN HandleHotKeys(Key);
      IF ShowCUPSKeys AND CUPSKeys.Pressed(Key) THEN HandleCUPSKeys(Key);
      IF LightMenu THEN
      BEGIN
         LightMenu :=False;
         IF GalMenu.Chosen THEN HandleGalMenu;
      END
      ELSE
      IF GalMenu.Activated THEN HandleGalMenu;
      IF ChooseLong THEN
      BEGIN
         GalLongTemp := Sliders.Value(1)*3.14159/180;
         SetColor(Whitte);
         DrawRay(GalLongTemp, Whitte);
         Delay(10);
         {Delay(1, Tick);}
         DrawRay(GalLongTemp, Whitte);
         IF Sliders.Changed THEN GalLong := Sliders.Value(1)*(Pi/180);
      END;

   UNTIL  ExitRequest;

   Sliders.Done;
   GalMenu.Done;
{  CUPSDone;}
   {Dispose(GalMenu);}
   {Displose(Sliders);}
end;


BEGIN
END.

(* PROGRAM ReadStarData;
   {This program will read and print a file written by the program Arms21cm.PAS.
    This is entitled "The Spiral Structure of a Galaxy."
    The file contains data for the rotation curve of a galaxy.
    These are points (R, V) where R is distance from the galactic center
    in Kpc, and V is the circular velocity in km/sec.
    The user must change the string "FileName" as appropriate.
    Note: The program is in conventional Pascal, and does not use
    any CUPS utilities. So the command "WriteLn" can be used.
    The instruction ReadLn means that you need to press a key to
    see successive entries. Delete it if you do not wish to do this.}

USES DOS, Crt;

TYPE
   PointRecord = Record
                    x, y : Double;
                 END;

VAR
   Filename           : String;
   F1                 : File of PointRecord;
   Points             : PointRecord;
   Radius, RadVel     : Real;

BEGIN
   FileName := 'Rotate,DAT'; {Change this to match the relevant file.}
   Assign(F1,FileName);
   Reset(F1);
   REPEAT
      IF NOT Eof(F1) THEN
      BEGIN
         Read(F1, Points);
         Radius := Points.x;
         RadVel := Points.y;
         WriteLn(Radius,'   ',RadVel);  readln;
      END;
   UNTIL Eof(F1);
   Close(F1);
   Readln;
END.*)