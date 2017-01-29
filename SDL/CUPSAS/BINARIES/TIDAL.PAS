    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1994.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program to follow the orbital evolution       *)
    (*       of a binary under the influence of tidal      *)
    (*       forces.                                       *)
    (*                                                     *)
    (*******************************************************)

unit Tidal;
{$M 64000,0,327680}
{$O+}
{$F+}
interface
procedure RunTidal;

implementation
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;

CONST
   Gks = 0.00029591221; {Square of the Gaussian gravitational constant.}
  
   Amc = 0.1; {Apsidal motion constant. Reduce for higher central concentration.}
   MIFactor = 0.01; {Factor relating to the moment of inertia of the primary star.
                MIFactor = 2/5 for a homogeneous sphere.}

TYPE
  
   ElementVector = Array[1..6] of Real;
   ElementArray = Array[1..600] of ElementVector;

VAR
   TideMenu                                    : TMenu;
   Mass1, Radius1, Rotation1, MeanRotation, Lag: Real;
   Radius1Input                                : Real;
   Mass2, Sma, Ecc, Inc, LongNode, ArgPastron  : Real;
   IncDeg, LongNodeDeg, ArgPastronDeg          : Real;
   Mu                                          : Real;
   aMax, aMin, eMax, eMin, iMax, iMin          : Real;
   NodeMax, NodeMin, AphMax, AphMin            : Real;
   RotMax, RotMin                              : Real;
   ElementTime                                 : Real;
                              {Time interval over which elements are plotted.}
   InputChosen                                 : Boolean;
   PlotOrbit                                   : Boolean;
   FirstPlot                                   : Boolean;
   InputAccepted                               : Boolean;
   Averaged                                    : Boolean;
   LightMenu                                   : Boolean;
   ExitRequest                                 : Boolean;
   Elements                                    : ElementArray;
   HotKeys                                     : THotKeys;
   CUPSKeys                                    : THotKeys;
   ShowCUPSKeys                                : Boolean;
   Key                                         : Byte;

PROCEDURE HandleTideMenu; FORWARD;

{------------------------------------------------------------------------}

PROCEDURE SetUpTideMenu;
BEGIN
   With TideMenu DO
   BEGIN
      Init;
      Column(1, 'File');
       row(1,1, 'About CUPS');
       row(1,2, 'About Program');
       row(1,3, 'Configuration');
       row(1,4, 'Exit this module');
      Column(2, 'Help');
       row(2,1, 'General information');
      Column(3, 'Data');
       row(3,1, 'Enter data');
      Column(4, 'Output');
       row(4,1, 'Not averaged. See each revolution');
       row(4,2, 'Averaged');
      Column(5, 'Elements');
       row(5,1, 'Semimajor axis');
       row(5,2, 'Eccentricity');
       row(5,3, 'Argument of periastron');
       row(5,4, 'Inclination');
       row(5,5, 'Longitude of ascending node');
       row(5,6, 'Rate of rotation of primary');
       row(5,7, 'Extend the time');
       row(5,8, 'Return to the full menu');
       AutoChecking(4,1);
       AutoChecking(5,1);
   END;
   TideMenu.Display;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      HotKeys.Init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F2-Stop';
      Key[3] := 'F10-Menu';
   END;
   HotKeys.Display;
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

{------------------------------------------------------------------------}
 
PROCEDURE HandleHotKeys(key: Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HELPBins.HLP', 'Tidal Hello');
      2: BEGIN
            PlotOrbit := False;
            HotKeys.Clear;
            ShowCUPSKeys := True;
            CUPSKeys.Display;
            IF TideMenu.Chosen THEN HandleTideMenu;
         END;
      3: BEGIN
            PlotOrbit := False;
            HotKeys.Clear;
            ShowCUPSKeys := True;
            CUPSKeys.Display;
            IF TideMenu.Chosen THEN HandleTideMenu;
         END;
   END;
END;

{------------------------------------------------------------------------}
 
PROCEDURE HandleCUPSKeys(key: Byte);
var i: integer;
BEGIN
   CASE Key OF
      1: ShowHelp('HELPBins.HLP', 'Tidal Hello');
      2: BEGIN
            TideMenu.RowActivate(3,1, True);
            TideMenu.RowActivate(4,1, True);
            TideMenu.RowActivate(4,2, True);
            FOR i := 1 TO 8 DO
            TideMenu.RowActivate(5,i, False);
            CloseViewPort(2);
            IF TideMenu.Chosen THEN HandleTideMenu;
         END;
   END;
END;

{------------------------------------------------------------------------}

Procedure TideData1(VAR Mass1, Mass2, Mu, MeanRotation,
                        Sma, Ecc, Inc, LongNode, ArgPAstron: Real);
VAR
   TideDat1  : TInputScreen;
BEGIN
   SetColor(Black); CloseViewPort(2);
   WITH TideDat1 DO
   BEGIN
      Init;
      DefineInputPort(0.05, 0.95, 0.07, 0.92);
      LoadLine('');
      LoadLine('                 The Tidal History of a Binary.            ');
      LoadLine('                                                           ');
      LoadLine('     A secondary star revolves around a primary.  The primary ');
      LoadLine('  is distorted due to the tidal action of the secondary,     ');
      LoadLine('  which is spherical. The axis of rotation of the primary    ');
      LoadLine('  is along the z-axis.                                        ');
      LoadLine('                                                            ');
      LoadLine('     The Mass of the primary star = {      } solar masses     ');
      LoadLine('                                                            ');
      LoadLine('     Enter information about the secondary star and its orbit:');
      LoadLine('                             Mass = {      } solar masses     ');
      LoadLine('                   Semimajor axis = {      }  astronomical units');
      LoadLine('      Eccentricity (0 <= e < 0.5) = {      }                   ');
      LoadLine('      Inclination (less than 10)  = {      } degrees           ');
      LoadLIne('  Longitude of the ascending node = {      } degrees           ');
      LoadLine('           Argument of periastron = {      } degrees           ');
      LoadLine('                                                             ');
      LoadLine('                    [  OK  ]             [Cancel]            ');
      SetNumber(1, 1);
      SetNumberLimits(1, 0.000001, 1000);
      SetNumber(2, 1);
      SetNumberLimits(2, 0.000001, 1000);
      SetNumber(3, 0.1);
      SetNumberLimits(3, 0.000001, 1000);
      SetNumber(4, 0.1);
      SetNumberLimits(4, 0.00001, 0.5);
      SetNumber(5, 5);
      SetNumberLimits(5, 0, 10);
      SetNumber(6, 180);
      SetNumberLimits(6, 0, 360);
      SetNumber(7, 180);
      SetNumberLimits(7, 0, 360);
      AcceptScreen;
      IF Canceled THEN InputAccepted := False ELSE InputAccepted := True;
      Mass1 := GetNumber(1);
      Mass2 := GetNumber(2);
      Mu := Gks*(Mass1 + Mass2);
      Sma := GetNumber(3);
      Ecc := GetNumber(4);
      IncDeg := GetNumber(5);
      Inc := IncDeg*3.14159/180;
      LongNodeDeg := GetNumber(6);
      IF LongNodeDeg = 360 THEN LongNodeDeg := 0;
      LongNode := LongNodeDeg*3.14159/180;
      ArgPAstronDeg := GetNumber(7);
      IF ArgPAstronDeg = 360 THEN ArgPAstronDeg := 0;
      ArgPAstron := ArgPAstronDeg*3.14159/180;
      MeanRotation := Sqrt((Mu)/(Sma*Sma*Sma))*Cos(Inc);
      Done;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE TideData2(MeanRotation: Real; VAR Radius1, Rotation1, Lag: Real);
VAR
   RotString  : String;
   TideDat2   : TInputScreen;
BEGIN
   HideCursor; SetColor(Black); {OpenViewPort(2);} ShowCursor;
   {DefineViewPort(1, 0, 1, 0.8, 0.9);
   SelectViewPort(1);}
   HideCursor;
   SetColor(White);
   Str(MeanRotation: 6: 3, RotString);
   Print(1, 3,'       The mean orbital angular rate is ' + RotString + ' radians per day');
   ShowCursor;
   WITH TideDat2 DO
   BEGIN
      Init;
      DefineInputPort(0.05, 0.95, 0.15, 0.85);
      LoadLine('');
      LoadLine('                 The Tidal History of a Binary.');
      LoadLine('');
      LoadLine('   The radius of the primary star  =  {      } solar radii');
      LoadLine('The angle of the tidal lag (in radians) is a constant, tau, multiplied');
      LoadLine('by the difference between the angular velocity of the primary, and');
      LoadLine('the z-component of the orbital angular velocity of the secondary.');
      LoadLine('The mean value of this latter quantity is shown above. The tidal');
      LoadLine('lag should not be large. Definitely, less than 1.');
      LoadLine('');
      LoadLine('                                tau = {      } days');
      LoadLine('');
      LoadLine('The angular velocity of the primary = {      } radians per day');
      LoadLine(''); 
      LoadLine('                [  OK  ]               [Cancel]');
      SetNumber(1, Radius1Input);
      SetNumberLimits(1, 0.00001, 1000);
      SetNumber(2, Lag);
      SetNumberLimits(2, 0.00001, 1);
      SetNumber(3, Rotation1); 
      AcceptScreen;
      IF Canceled THEN InputAccepted := False ELSE InputAccepted := True;
      Radius1Input := GetNumber(1);
      Radius1 := Radius1Input*(6.96265/1495.97870); {Convert to astronomical units.}
      Lag := GetNumber(2);
      Rotation1 := GetNumber(3);
      Done;
   END;
END;

{-------------------------------------------------------------------------}

Procedure ElementInfo(Sma: Real; VAR ElementTime: Real);
VAR
   i            : Integer;
   Period       : Real;
   PerString    : String;
   ElementDat   : TInputScreen;
BEGIN
   HideCursor;
   GraphBackColor := Black; SetColor(Black); CloseViewPort(2);
   ShowCursor;
   SelectViewPort(1);
   HideCursor;
   Period := 2*3.14159*Sqrt((Sma*Sma*Sma)/(Mu));
   Str(Period: 8: 2, PerString);
   SetColor(White);
   Print(1, 2,'         The period of the binary orbit is ' + PerString + ' days');
   ShowCursor;
   PLotOrbit := False;
   WITH ElementDat DO
   BEGIN
      Init;
      DefineInputPort(0.05, 0.95, 0.35, 0.8);
      LoadLine('             Information for Computing Elements');
      LoadLine('');
      LoadLine('   Values of the elements will be computed at 600 regularly spaces');
      LoadLine('times over an interval that you must specify. The period of the');
      LoadLine('binary orbit is given above.');
      LoadLine('   Time interval for finding elements: {      } days');
      LoadLine('   During the computation, the eccentricity will be plotted. ');
      LoadLine('');
      LoadLine('             [  OK  ]                  [Cancel]');
      SetNumber(1, ElementTime);
      SetNumberLimits(1, 1, 1000000);
      AcceptScreen;
      IF Canceled THEN InputAccepted := False ELSE InputAccepted := True;
      ElementTime := GetNumber(1);
      Done;
   END;
   IF InputAccepted THEN
   BEGIN
     
      FirstPlot := True;
   END;
END;


{-------------------------------------------------------------------------}

PROCEDURE PromptElementChoice;
var ViewPort : ViewPortType;
    x1, y1, x2,y2,midx,midy : integer;
begin
   HideCursor;
   PlotOrbit := False;
   SetColor(Black);
   GraphBackColor := Black;
   SelectViewPort(2);
   GetViewSettings (ViewPort);
   with ViewPort do begin midx := (x1+x2) div 2; midy := (y1+y2) div 2; end;
   x1 := midx-200; y1 := midy-50; x2 := midx+200; y2 := midy;
   SetTextJustify(LeftText, TopText);
   SetFillStyle(SolidFill,blue);
   SetColor(Yellow);
   Bar3D(x1,y1+5,x2,y2+5,5,true);  
   OutTextXY(x1+20,y1+20,'Make a selection from the menu: "Elements"');
   OutTextXY(x1+20,y1+40,'to see plots of this or other elements.');
   ShowCursor;
END;

{-----------------------------------------------------------------------}

PROCEDURE DrawAxes(xMin, xMax, yMin, yMax: Real);
VAR
   i, nTick: Integer;
   Tick, dTick, TickSize: Real;
   xSc, ySc, xSd, ySd: Integer;
   Mark: String;
BEGIN
   Map(0, yMin, xSc, ySc); Map(0, yMax, xSd, ySd);
   Line(xSc, ySc, xSd, ySd);                    {Draw the y-axis.}
   Map(xMax, yMin, xSd, ySd);
   Line(xSc, ySc, xSd, ySd);                    {Draw the x-axis.}
   TickSize := (xMax - xMin)/100;
  { nTick := 4 - Trunc(ln((yMax - yMin)*1000/3)/(ln(10)) + 0.1);
   IF nTick > 4 THEN nTick := 4; }
   dTick := 100;
   nTick := -2;
   REPEAT dTick := dTick/10; nTick := nTick + 1; UNTIL dTick < (yMax - yMin)/2;
 {  FOR i := 1 TO nTick DO dTick := dTick/10;} 
   Tick := Trunc((yMin/dTick) + 1.0000001)*dTick;
   REPEAT
      Map(0, Tick, xSc, ySc); Map(TickSize, Tick, xSd, ySd);
      Line(xSc, ySc, xSd, ySd);
      IF nTick > 0 THEN
         Mark := NumStr(Tick, 3, nTick)
      ELSE
         Mark := NumStr(Tick, 4, 0);
      OutTextxy(xSd+2, ySd-2, Mark);
      Tick := Tick + dTick;
   UNTIL Tick > yMax;
   TickSize := (yMax - yMin)/100;
   nTick := 4 - Trunc(ln((xMax - xMin)*1000/3)/(ln(10)) + 0.1);
   dTick := 10;
   IF nTick > 0 THEN
      FOR i := 1 TO nTick DO dTick := dTick/10;
   IF nTick < 0 THEN
      FOR i := 1 TO -nTick DO dTick := dTick*10;       
   Tick := Trunc((xMin/dTick) + 1.0000001)*dTick;
   REPEAT
      Map(Tick, yMin, xSc, ySc); Map(Tick, yMin + TickSize, xSd, ySd);
      Line(xSc, ySc, xSd, ySd);
      IF nTick > 0 THEN
         Str(Tick:3:nTick-1, Mark)
      ELSE
         Str(Tick:5:0, Mark);
      OutTextxy(xSd-20, ySd-10, Mark);
      Tick := Tick + dTick;
   UNTIL Tick > xMax;

END;

{-----------------------------------------------------------------------}

Procedure AScreen(aMax, aMin, ElementTime: Real);
VAR
   xSc, ySc: Integer;
BEGIN
   SetColor(Yellow);
   GraphBackColor := DarkGray;
   HideCursor;
   OpenViewPort(2);
   DefineScale(2, 0, ElementTime, aMin, aMax);
   SelectScale(2);
   DrawAxes(0, ElementTime, aMin, aMax);
   Map(0.94*ElementTime, aMin, xSc, ySc);
   OutTextxy(xSc, ySc-20, 'Days');
   Map(0.1*ElementTime, aMax, xSc, ySc);
   OutTextxy(xSc, ySc+5, 'Astronomical units');
   Map(0.5*ElementTime, aMax, xSc, ySc);
   OutTextxy(xSc, ySc+5, 'Semimajor axis');
   ShowCursor;
END;

{-----------------------------------------------------------------------}

Procedure EScreen(eMax, eMin, ElementTime: Real);
VAR
   xSc, ySc: Integer;
BEGIN
   SetColor(Yellow);
   GraphBackColor := DarkGray;
   HideCursor;
   IF FirstPlot = True THEN OpenViewPort(3)
                       ELSE OpenViewPort(2);
   DefineScale(2, 0, ElementTime, eMin, eMax);
   SelectScale(2);
   DrawAxes(0, ElementTime, eMin, eMax);
   Map(0.94*ElementTime, eMin, xSc, ySc);
   OutTextxy(xSc, ySc-20, 'Days');
   Map(0.5*ElementTime, eMax, xSc, ySc);
   OutTextxy(xSc, ySc+5, 'Eccentricity');
   ShowCursor;
END;

{-----------------------------------------------------------------------}

Procedure IScreen(iMax, iMin, ElementTime: Real);
VAR
   xSc, ySc: Integer;
BEGIN
   SetColor(Yellow);
   GraphBackColor := DarkGray;
   HideCursor;
   OpenViewPort(2);
   DefineScale(2, 0, ElementTime, iMin, iMax);
   SelectScale(2);
   DrawAxes(0, ElementTime, iMin, iMax);
   Map(0.1*ElementTime, iMax, xSc, ySc);
   OutTextxy(xSc, ySc+5, 'Degrees');
   Map(0.94*ElementTime, iMin, xSc, ySc);
   OutTextxy(xSc, ySc-20, 'Days');
   Map(0.5*ElementTime, iMax, xSc, ySc);
   OutTextxy(xSc, ySc+5, 'Inclination');
   ShowCursor;
END;

{-----------------------------------------------------------------------}

Procedure AphScreen(AphMax, AphMin, ElementTime: Real);
VAR
   xSc, ySc: Integer;
BEGIN
   SetColor(Yellow);
   GraphBackColor := DarkGray;
   HideCursor;
   OpenViewPort(2);
   DefineScale(2, 0, ElementTime, AphMin, AphMax);
   SelectScale(2);
   DrawAxes(0, ElementTime, AphMin, AphMax);
   Map(0.1*ElementTime, AphMax, xSc, ySc);
   OutTextxy(xSc, ySc+5, 'Degrees');
   Map(0.94*ElementTime, AphMin, xSc, ySc);
   OutTextxy(xSc, ySc-20, 'Days');
   Map(0.5*ElementTime, AphMax, xSc, ySc);
   OutTextxy(xSc, ySc+5, 'Argument of Periastron');
   ShowCursor;
END;

{-----------------------------------------------------------------------}

Procedure NodeScreen(NodeMax, NodeMin, ElementTime: Real);
VAR
   xSc, ySc: Integer;
BEGIN
   SetColor(Yellow);
   GraphBackColor := DarkGray;
   HideCursor;
   OpenViewPort(2);
   DefineScale(2, 0, ElementTime, NodeMin, NodeMax);
   SelectScale(2);
   DrawAxes(0, ElementTime, NodeMin, NodeMax);
   Map(0.1*ElementTime, NodeMax, xSc, ySc);
   OutTextxy(xSc, ySc+5, 'Degrees');
   Map(0.94*ElementTime, NodeMin, xSc, ySc);
   OutTextxy(xSc, ySc-20, 'Days');
   Map(0.3*ElementTime, NodeMax, xSc, ySc);
   OutTextxy(xSc, ySc+5, 'Longitude of the ascending node');
   ShowCursor;
END;

{-----------------------------------------------------------------------}

Procedure RotScreen(RotMax, RotMin, ElementTime: Real);
VAR
   xSc, ySc: Integer;
BEGIN
   SetColor(Yellow);
   GraphBackColor := DarkGray;
   HideCursor;
   OpenViewPort(2);
   DefineScale(2, 0, ElementTime, RotMin, RotMax);
   SelectScale(2);
   DrawAxes(0, ElementTime, RotMin, RotMax); 
   Map(0.1*ElementTime, RotMax, xSc, ySc);
   OutTextxy(xSc, ySc+5, 'Radians per day');
   Map(0.94*ElementTime, RotMin, xSc, ySc);
   OutTextxy(xSc, ySc-20, 'Days');
   Map(0.4*ElementTime, RotMax, xSc, ySc);
   OutTextxy(xSc, ySc+5, 'Angular velocity of the primary');
   ShowCursor;
END;

{-----------------------------------------------------------------------}  

Procedure DrawElement(n: Integer);
VAR
   xSc, ySc: Integer;
   i: Integer;
   xMouse, yMouse  : Real;
   Inside          : Boolean;
   xOld, yOld      : Real;

BEGIN
   HideCursor;
   FOR i := 1 TO 600 DO
   BEGIN
      Map(i*(ElementTime/600), Elements[i, n], xSc, ySc);
      IF i = 1 THEN  MoveTo(xSc, ySc) ELSE LineTo(xSc, ySc);
   END;
   {FOR i := 1 TO 8 DO  TideMenu.RowActivate(5, i, True);}
   ShowCursor;
   SetColor(Yellow);
   Message('Move mouse outside window to activate menu');
   REPEAT
      MousePosn(xMouse, yMouse, 2, 2, Inside);
      IF Inside THEN
      BEGIN
         IF Abs(xOld-xMouse) + Abs(yOld-yMouse) > 0.01 THEN
         BEGIN
            RubOutXY(0.7, 0.8, 15, DarkGray);
            PrintXY(0.7, 0.8, '('+NumStr(xMouse, 6,1)+', '
                                     +NumStr(yMouse, 5,2)+')');
            xOld := xMouse;
            yOld := yMouse;
         END;
      END;
   UNTIL NOT Inside;
   RubOutXY(0.7, 0.8, 15, DarkGray);
   Message('');
   LightMenu := True;
END;

{-----------------------------------------------------------------------}

Procedure SeeElements;

CONST
   Neq = 7;

TYPE
   Vector = Array[1..Neq] of Real;

VAR
   More: Boolean;
   i, j: Integer;
   Y: Vector;


Procedure RotateAxes(nr: Integer; th, xin, yin, zin: Real;
                         VAR xout, yout, zout: Real);
BEGIN
   CASE nr OF
   1:  BEGIN
{Rotation through th about the x-axis.}
          yout := yin*cos(th) + zin*sin(th);
          zout := - yin*sin(th) + zin*cos(th);
          xout := xin;
       END;
   2:  BEGIN
{Rotation through th about the y-axis.}
          xout := xin*cos(th) - zin*sin(th);
          zout := xin*sin(th) + zin*cos(th);
          yout := yin;
       END;
   3:  BEGIN
{Rotation through th about the z-axis.}
          xout := xin*cos(th) + yin*sin(th);
          yout := - xin*sin(th) + yin*cos(th);
          zout := zin;
       end;
   end; {Case.}
end;


Procedure InitialConditions(Mass1, Mass2, Rotation1,
                                 Sma, Ecc, Inc, LongNode, ArgPAstron: Real;
                                 VAR State: Vector);
VAR
   x, y, z, vx, vy, vz: real;
   th: real;
   nr: integer;

BEGIN
   x := Sma*(1 - Ecc);
   y := 0;
   z := 0;
   vx := 0;
   vy :=  Sqrt((Mu/Sma)*((1 + Ecc)/(1 - Ecc)));
   vz := 0;
   nr := 3; th := - ArgPAstron; RotateAxes(nr, th, x, y, z, x, y, z);
   RotateAxes(nr, th, vx, vy, vz, vx, vy, vz);
   nr := 1; th := - Inc; RotateAxes(nr, th, x, y, z, x, y, z);
   RotateAxes(nr, th, vx, vy, vz, vx, vy, vz);
   nr := 3; th := - LongNode; RotateAxes(nr, th, x, y, z, x, y, z);
   RotateAxes(nr, th, vx, vy, vz, vx, vy, vz);
   State[1] := x;
   State[2] := y;
   State[3] := z;
   State[4] := vx;
   State[5] := vy;
   State[6] := vz;
   State[7] := Rotation1;
END;



Procedure GravForce(MassTerm, x1, y1, z1, x2, y2, z2: Real;
                    VAR gfx, gfy, gfz: Real);
{Force exerted by a particle at (x2, y2, z2) on a particle at (x1, y1, z1).}

VAR
   Dist2, Dist3: Real;
BEGIN
   Dist2 := (x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2) + (z1 - z2)*(z1 - z2);
   Dist3 := Sqrt(Dist2)*Dist2;
   gfx := MassTerm*(x2 - x1)/Dist3;
   gfy := MassTerm*(y2 - y1)/Dist3;
   gfz := MassTerm*(z2 - z1)/Dist3;
END;



Procedure Fun(x: real; y: Vector; VAR z: Vector);
VAR
   r, r2, r3, r5: Real;
   Rotz: Real;
   LagAngle, Cla, Sla: Real;
   rLag: Real;
   TidalMass: Real;
   MassTerm: Real;
   hz: Real;
   xA, yA, zA: Real;
   Tfx, Tfy, Tfz, TRot: Real;
BEGIN
   r2 := y[1]*y[1] + y[2]*y[2] + y[3]*y[3];
   r := Sqrt(r2);
   r3 := r2*r;
   r5 := r3*r2;
   z[1] := y[4];  z[2] := y[5]; z[3] := y[6];

   hz := y[1]*y[5] - y[2]*y[4];
   Rotz := y[7] - hz/(y[1]*y[1] + y[2]*y[2]);  {Relative angular velocity.}
   LagAngle := Lag*Rotz;
   Cla := Cos(Lagangle);
   Sla := Sin(LagAngle);
   rLag := r - Lag*(y[1]*y[4] + y[2]*y[5] + y[3]*y[6])/r;
   TidalMass := 0.5*Amc*Mass2*(Radius1*Radius1*Radius1/(rLag*rLag*rLag));
   MassTerm := Mu*(1 - 2*TidalMass/Mass1);
   z[4] := - MassTerm*y[1]/r3; {                     }
   z[5] := - MassTerm*y[2]/r3; {Inverse square terms.}
   z[6] := - MassTerm*y[3]/r3; {                     }

   xA := (Cla*y[1] - Sla*y[2])*(Radius1/r);
   yA := (Sla*y[1] + Cla*y[2])*(Radius1/r);
   zA := y[3]*(Radius1/r);
   MassTerm := Mu*TidalMass/Mass1;
   GravForce(MassTerm, y[1], y[2], y[3], xA, yA, zA, Tfx, Tfy, Tfz);
   z[4] := z[4] + Tfx;  {            }
   z[5] := z[5] + Tfy;  {Tidal terms.}
   z[6] := z[6] + Tfz;  {            }
   GravForce(MassTerm, y[1], y[2], y[3], -xA, -yA, -zA, Tfx, Tfy, Tfz);
   z[4] := z[4] + Tfx;  {            }
   z[5] := z[5] + Tfy;  {Tidal terms.}
   z[6] := z[6] + Tfz;  {            }

   z[7] := (Mass2/(Mass1 + Mass2))*(y[2]*z[4] - y[1]*z[5])/
   (Sqr(Radius1*MIFactor));
END;

{---------------------------------------------------------------------}

Procedure Step(xin, hin, tol: real; yin: Vector;
               VAR xout, hout: real; VAR yout: Vector);

VAR
   a, ch, ct:                      array[1..6] of real;
   b:                              array[1..6,1..5] of real;
   f:                              array[1..6] of vector;
   y, ytemp, te:                          Vector;
   x, xtemp, h, htemp, temax:                   real;
   k, l, n:                        integer;

begin
{Parameters for the Runge-Kutta-Fehlberg method of
order 4/5.}
   a[1]   := 0     ;    a[2]   := 2/9     ;    a[3]   := 1/3;
   a[4]   := 3/4   ;    a[5]   := 1       ;    a[6]   := 5/6;
   ch[1]  := 47/450;    ch[2]  := 0       ;    ch[3]  := 12/25;
   ch[4]  := 32/225;    ch[5]  := 1/30    ;    ch[6]  := 6/25;
   ct[1]  := -1/150;    ct[2]  := 0       ;    ct[3]  := 3/100;
   ct[4]  := -16/75;    ct[5]  := -1/20   ;    ct[6]  := 6/25;
   b[2,1] := 2/9   ;    b[3,1] := 1/12    ;    b[3,2] := 1/4;
   b[4,1] := 69/128;    b[4,2] := -243/128;    b[4,3] := 135/64;
   b[5,1] := -17/12;    b[5,2] := 27/4    ;    b[5,3] := -27/5;
   b[5,4] := 16/15 ;    b[6,1] := 65/432  ;    b[6,2] := -5/16;
   b[6,3] := 13/16 ;    b[6,4] := 4/27    ;    b[6,5] := 5/144;

   xtemp := xin;
   h := hin;
   Fun(xin, yin, f[1]);
   ytemp := yin;
   repeat {until temax < tol}
      for k := 2 to 6 do
         begin
            x := xtemp + a[k]*h;
            y := ytemp;
            for n := 1 to neq do
               begin
                  for l := 1 to k-1 do
                      y[n] := y[n] + h*b[k,l]*f[l,n];
               end;
            Fun(x, y, f[k]);
         end; {k loop}
      y := ytemp;
      for n := 1 to neq do
         begin
            te[n] := 0;
               for k := 1 to 6 do
                  begin
                     y[n] := y[n] + h*ch[k]*f[k,n];
                     te[n] := te[n] + h*ct[k]*f[k,n];
                  end;  {k loop}
               te[n] := abs(te[n]);
         end; {n loop}
      temax := tol/10;
      for n := 1 to neq do
         if temax < te[n] then temax := te[n];
      htemp := h;
      h := 0.9*h*exp(ln(tol/temax)/5);
   until temax < tol; 
   yout := y;
   xout := xtemp + htemp;
   hout := h;
end;



{---------------------------------------------------------------------}

Function Atan2(s,c: Real): Real;
{Finds the angle between 0 and 2*pi for which the sine is proportional
to s and the cosine proportional to c.  Equivalent to the FORTRAN
function of the same name.}
BEGIN
   IF s < 0 THEN
   BEGIN
      IF c = 0 THEN atan2 := 1.5*pi
      ELSE
         BEGIN
            IF c > 0 THEN Atan2 := 2*pi + ArcTan(s/c)
            ELSE Atan2 := pi + ArcTan(s/c);
         END;
   END
   ELSE
   BEGIN
      IF c = 0 THEN Atan2 := pi/2
      ELSE
      BEGIN
         Atan2 := ArcTan(s/c);
      END;
      IF c < 0 THEN Atan2 := pi + ArcTan(s/c);
   END;
END;

{------------------------------------------------------------------------}

Procedure FindElements(y: Vector; VAR Vec: ElementVector);
{Calculates elements, given components of position and velocity.}
VAR
   x, yy, z, xv, yv, zv, r: Real;
   hx, hy, hz, hh: Real;
   ex, ey, ez, u, s, c: Real;
BEGIN
   x := y[1];  yy := y[2];  z := y[3];
   xv := y[4]; yv := y[5]; zv := y[6];
   hx := yy*zv - z*yv;
   hy := z*xv - x*zv;
   hz := x*yv - yy*xv;
   hh := Sqrt(hx*hx + hy*hy + hz*hz);
   r := Sqrt(x*x + yy*yy + z*z);
   ex := (yv*hz - zv*hy)/Mu - x/r;
   ey := (zv*hx - xv*hz)/Mu - yy/r;
   ez := (xv*hy - yv*hx)/Mu - z/r;
   u := x*xv + yy*yv + z*zv;
   s := hx;                  c := -hy;
   Vec[5] := (180/3.14159)*Atan2(s,c);   {Longitude of the ascending node.}
   s := Sqrt(hx*hx + hy*hy); c := hz;
   Vec[3] := (180/3.14159)*Atan2(s,c);   {Inclination.}
   Vec[6] := y[7];
   IF s < 0.000001 THEN  {The orbit is planar.}
   BEGIN
      s := ey; c := ex; Vec[4] := (180/3.14159)*Atan2(s,c);
   END
   ELSE
   BEGIN
      s := ez; c := (-ex*hy + ey*hx)/hh;
      Vec[4] := (180/3.14159)*Atan2(s,c);  {Argument of perihelion.}
   END;
   Vec[2] := Sqrt(ex*ex + ey*ey + ez*ez);  {Eccentricity.}
   Vec[1] := 1/(2/r - (xv*xv + yv*yv + zv*zv)/Mu);  {Semimajor axis.}
end;

{------------------------------------------------------------------------}

Procedure FillElementArray(ElementTime: Real; y: Vector;
                           VAR Elements: ElementArray; VAR yOut: Vector);
VAR
   Dt, tOut, h: Real;
   ElementVec: ElementVector;
   i, j: Integer;
   xOld, yOld, xNew, yNew   : Integer;
   xMouse, yMouse            : Real;
   xSave, ySave              : Real;
   Inside                    : Boolean;
   xSc, ySc: Integer;

BEGIN
   FOR i := 1 TO 600 DO
   BEGIN
      FOR j := 1 TO 6 DO Elements[i, j] := 0;
   END;
   PlotOrbit := True;
   SelectViewPort(3);
   SelectScale(2);
   ShowCUPSKeys := False;
   CUPSKeys.Clear;
   SetUpHotKeys;
   eMin := 0;
   eMax := Ecc+ 0.1;
   eScreen(eMax, eMin, ElementTime);
   ShowCursor;
   Dt := ElementTime/600;
   FindElements(y, ElementVec);
   aMax := ElementVec[1]; aMin := AMax;
   eMax := ElementVec[2]; eMin := eMax;
   iMax := ElementVec[3]; iMin := iMax;
   AphMax := ElementVec[4]; AphMin := AphMax;
   NodeMax := ElementVec[5]; NodeMin := NodeMax;
   RotMax := ElementVec[6]; RotMin := RotMax;
   i := 0;
   REPEAT
      i := i + 1;
      tOut := 0;
      REPEAT
         Step(tOut, Dt - tOut, 0.0000001, y, tOut, h, y);
      UNTIL Abs(Dt - tout) < 0.001;
      FindElements(y, ElementVec);  
      IF aMax < ElementVec[1] THEN aMax := ElementVec[1];
      IF aMin > ElementVec[1] THEN aMin := ElementVec[1];
      IF eMax < ElementVec[2] THEN eMax := ElementVec[2];
      IF eMin > ElementVec[2] THEN eMin := ElementVec[2];
      IF iMax < ElementVec[3] THEN iMax := ElementVec[3];
      IF iMin > ElementVec[3] THEN iMin := ElementVec[3];
      IF AphMax < ElementVec[4] THEN AphMax := ElementVec[4];
      IF AphMin > ElementVec[4] THEN AphMin := ElementVec[4];
      IF NodeMax < ElementVec[5] THEN NodeMax := ElementVec[5];
      IF NodeMin > ElementVec[5] THEN NodeMin := ElementVec[5];
      IF RotMax < ElementVec[6] THEN RotMax := ElementVec[6];
      IF RotMin > ElementVec[6] THEN RotMin := ElementVec[6]; 
      Map(i*(ElementTime/600), ElementVec[2], xSc, ySc);
      HideCursor;
      {IF i = 1 THEN  MoveTo(xSc, ySc) ELSE LineTo(xSc, ySc);}
      IF i = 1 THEN
      BEGIN
         xOld := xSc;
         yOld := ySc;
      END
      ELSE
      BEGIN
         xNew := xSc;
         yNew := ySc;
         Line(xOld, yOld, xNew, yNew);
         xOld := xNew;
         yOld := yNew;
      END;
      ShowCursor;
      FOR j := 1 TO 6 DO
         Elements[i, j] := ElementVec[j];
      CheckForEvents;              
      IF Hotkeys.Pressed(Key) THEN HandleHotKeys(Key);
   UNTIL (i = 600) OR (NOT PlotOrbit) OR (y[2] > 0.7);
   IF PlotOrbit = False THEN
   BEGIN
      ShowCUPSKeys := True;
      CUPSKeys.Display;
      Exit;
   END;
   FirstPlot := False;
   Sma := Elements[600, 1];
   Ecc := Elements[600, 2];
   Inc := Elements[600, 3]*Pi/180;
   ArgPAstron := Elements[600, 4];
   LongNode := Elements[600, 5];
   Rotation1 := Elements[600, 6];
   SetColor(Yellow);
   Message('Move mouse outside window to activate menu');
   REPEAT
      MousePosn(xMouse, yMouse, 2, 2, Inside);
      IF Inside THEN
      BEGIN
         IF Abs(xSave-xMouse) + Abs(ySave-yMouse) > 0.01 THEN
         BEGIN
            RubOutXY(0.7, 0.8, 15, DarkGray);
            PrintXY(0.7, 0.8, '('+NumStr(xMouse, 6,1)+', '
                                     +NumStr(yMouse, 5,2)+')');
            xSave := xMouse;
            ySave := yMouse;
         END;
      END;
   UNTIL NOT Inside;
   RubOutXY(0.7, 0.8, 15, DarkGray);
   Message('');
END;

BEGIN   {Main Program for Procedure SeeElements.}
      ElementTime := 600;
      FOR i := 1 TO 8 DO  TideMenu.RowActivate(5, i, False);
      TideMenu.RowActivate(3,1, True);
      TideMenu.RowActivate(4,1, True);
      TideMenu.RowActivate(4,2, True);
      InitialConditions(Mass1, Mass2, Rotation1,
                     Sma, Ecc, Inc, LongNode, ArgPAstron, Y);
      ElementInfo(Sma, ElementTime);
      IF NOT InputAccepted THEN
      BEGIN
         RubOut(1,2, 70, Black);
         LightMenu := True;
         Exit;
      END;
      FillElementArray(ElementTime, y, Elements, y);
      TideMenu.Display;
      ShowCUPSKeys := True;
      HotKeys.Clear;
      CUPSKeys.Display;
      IF PlotOrbit THEN
      BEGIN
         FOR i := 1 TO 8 DO  TideMenu.RowActivate(5, i, True);
         IF ABS(Elements[1, 3]) < 0.001 THEN
         BEGIN
            TideMenu.RowActivate(5,4, False);
            TideMenu.RowActivate(5,5, False);
         END;
        { TideMenu.RowActivate(3,1, False);
         TideMenu.RowActivate(4,1, False);
         TideMenu.RowActivate(4,2, False);}
         PromptElementChoice;
      END;
END;

{----------------------------------------------------------------------}


Procedure SeeAveragedElements;

CONST
   Neq = 4;

TYPE
   Vector = Array[1..Neq] of Real;

VAR
   More: Boolean;
   i, j: Integer;
   Y: Vector;
   MassRatio, Factor: Real;

{----------------------------------------------------------------------}

Procedure Fun(x: real; y: Vector; VAR z: Vector);
VAR
   MeanMotion: Real;
   e1, e2, e3, f1, f2, f3, f4, f5: Real;
   Factor1, Factor2, Factor3, Factor4, Factor5: Real;

Function Func1(x: Real): Real;
BEGIN
   Func1 := 1 + x*(15.5 + x*(31.875 + x*(11.5626 + x*0.390625)));
END;

Function Func2(x: Real): Real;
BEGIN
   Func2 := 1 + x*(7.5 + x*(5.625 + x*0.3125));
END;

Function Func3(x: Real): Real;
BEGIN
   Func3 := 1 + x*(3.75 + x*(1.875 + x*0.078125));
END;

Function Func4(x: Real): Real;
BEGIN
   Func4 := 1 + x*(1.5 + x*0.125);
END;

Function Func5(x: Real): Real;
BEGIN
   Func5 := 1 + x*(3 + x*0.375);
END;

BEGIN
   IF y[2] > 0.99 THEN y[2] := 0.99;
   e2 := y[2]*y[2];
   e1 := Sqrt(1 - e2);
   e3 := e1*e1*e1;
   f1 := Func1(e2); f2 := Func2(e2); f3 := Func3(e2);
   f4 := Func4(e2); f5 := Func5(e2);
   Factor1 := Sqr(Radius1/(y[1]*(1 - e2)));
   MeanMotion := Sqrt(Mu/(y[1]*y[1]*y[1]));
   Factor2 := y[4]/MeanMotion;
   Factor3 := Factor*MassRatio*(1 + MassRatio)
                    *Factor1*Factor1*Factor1*Factor1;
   Factor4 := 3*Factor*(MassRatio*MassRatio/MIFactor)
                    *Factor1*Factor1*Factor1;
   Factor5 := 0.5*(1 - MIFactor*(1 + 1/MassRatio)
              *(Radius1*Radius1/(y[1]*y[1]))*Factor2/e1);
   z[1] := -6*Factor3*y[1]*e1*(f1 - e3*f2*Factor2);
   z[2] := -27*Factor3*y[2]*e3*(f3 - (11/18)*f4*Factor2);
   z[3] := - Factor4*(y[3]/Factor2)*(f2 - Factor5*e3*f5*Factor2);
   z[4] := Factor4*MeanMotion*(f2 - e3*f5*Factor2);
END;

{---------------------------------------------------------------------}

Procedure Step(xin, hin, tol: real; yin: Vector;
               VAR xout, hout: real; VAR yout: Vector);

VAR
   a, ch, ct:                      array[1..6] of real;
   b:                              array[1..6,1..5] of real;
   f:                              array[1..6] of vector;
   y, ytemp, te:                          Vector;
   x, xtemp, h, htemp, temax:                   real;
   k, l, n:                        integer;

begin
{Parameters for the Runge-Kutta-Fehlberg method of
order 4/5.}
   a[1]   := 0     ;    a[2]   := 2/9     ;    a[3]   := 1/3;
   a[4]   := 3/4   ;    a[5]   := 1       ;    a[6]   := 5/6;
   ch[1]  := 47/450;    ch[2]  := 0       ;    ch[3]  := 12/25;
   ch[4]  := 32/225;    ch[5]  := 1/30    ;    ch[6]  := 6/25;
   ct[1]  := -1/150;    ct[2]  := 0       ;    ct[3]  := 3/100;
   ct[4]  := -16/75;    ct[5]  := -1/20   ;    ct[6]  := 6/25;
   b[2,1] := 2/9   ;    b[3,1] := 1/12    ;    b[3,2] := 1/4;
   b[4,1] := 69/128;    b[4,2] := -243/128;    b[4,3] := 135/64;
   b[5,1] := -17/12;    b[5,2] := 27/4    ;    b[5,3] := -27/5;
   b[5,4] := 16/15 ;    b[6,1] := 65/432  ;    b[6,2] := -5/16;
   b[6,3] := 13/16 ;    b[6,4] := 4/27    ;    b[6,5] := 5/144;

   xtemp := xin;
   h := hin;
   Fun(xin, yin, f[1]);
   ytemp := yin;
   repeat {until temax < tol}
      for k := 2 to 6 do
         begin
            x := xtemp + a[k]*h;
            y := ytemp;
            for n := 1 to neq do
               begin
                  for l := 1 to k-1 do
                      y[n] := y[n] + h*b[k,l]*f[l,n];
               end;
            Fun(x, y, f[k]);
         end; {k loop}
      y := ytemp;
      for n := 1 to neq do
         begin
            te[n] := 0;
               for k := 1 to 6 do
                  begin
                     y[n] := y[n] + h*ch[k]*f[k,n];
                     te[n] := te[n] + h*ct[k]*f[k,n];
                  end;  {k loop}
               te[n] := abs(te[n]);
         end; {n loop}
      temax := tol/10;
      for n := 1 to neq do
         if temax < te[n] then temax := te[n];
      htemp := h;
      h := 0.9*h*exp(ln(tol/temax)/5);
   until temax < tol; 
   yout := y;
   xout := xtemp + htemp;
   hout := h;
end;



{---------------------------------------------------------------------}

Procedure FillElementArray(ElementTime: Real; y: Vector;
                           VAR Elements: ElementArray; VAR yOut: Vector);
VAR
   Dt, tOut, h: Real;
   ElementVec: ElementVector;
   i, j: Integer;
   xOld, yOld, xNew, yNew : Integer;
   xMouse, yMouse            : Real;
   xSave, ySave              : Real;
   Inside                    : Boolean;
   xSc, ySc: Integer;
BEGIN
   SelectScale(2);
   ShowCUPSKeys := False;
   CUPSKeys.Clear;
   SetUpHotKeys;
   PlotOrbit := True;
   eMin := 0;
   eMax := Ecc+ 0.1;
   eScreen(eMax, eMin, ElementTime);
   ShowCursor;
   Dt := ElementTime/600;
   aMax := Y[1]; aMin := AMax;
   eMax := Y[2]; eMin := eMax;
   iMax := Y[3]; iMin := iMax;
   FOR i := 1 TO 600 DO
      BEGIN
         FOR j := 1 TO 4 DO Elements[i, j] := 0;
      END;
   RotMax := Y[4]; RotMin := RotMax;
   i := 0;    
   REPEAT
      i := i + 1;
      tOut := 0;
      REPEAT
         Step(tOut, (Dt - tOut), 0.000001, y, tOut, h, y);
      UNTIL Abs(Dt - tout) < 0.001;
      IF aMax < Y[1] THEN aMax := Y[1];
      IF aMin > Y[1] THEN aMin := Y[1];
      IF eMax < Y[2] THEN eMax := Y[2];
      IF eMin > Y[2] THEN eMin := Y[2];
      IF iMax < Y[3] THEN iMax := Y[3];
      IF iMin > Y[3] THEN iMin := Y[3];
      IF RotMax < Y[4] THEN RotMax := Y[4];
      IF RotMin > Y[4] THEN RotMin := Y[4];
      Map(i*(ElementTime/600), Y[2], xSc, ySc);
      HideCursor;
     { IF i = 1 THEN  MoveTo(xSc, ySc) ELSE LineTo(xSc, ySc);}
      IF i = 1 THEN
      BEGIN
         xOld := xSc;
         yOld := ySc;
      END
      ELSE
      BEGIN
         xNew := xSc;
         yNew := ySc;
         Line(xOld, yOld, xNew, yNew);
         xOld := xNew;
         yOld := yNew;
      END;
      ShowCursor;
      FOR j := 1 TO 3 DO
         Elements[i, j] := Y[j];
      Elements[i, 6] := Y[4];
      CheckForEvents;              
      IF Hotkeys.Pressed(Key) THEN HandleHotKeys(Key);
   UNTIL (i = 600) OR (NOT PlotOrbit) OR (y[2] > 0.7);
   IF PlotOrbit = False THEN
   BEGIN
      ShowCUPSKeys := True;
      CUPSKeys.Display;
      Exit;
   END;
   FirstPlot := False;
   Sma := y[1]; Ecc := y[2]; Inc := y[3]*Pi/180; Rotation1 := y[4];
   SetColor(Yellow);
   Message('Move mouse outside window to activate menu');
   REPEAT
      MousePosn(xMouse, yMouse, 2, 2, Inside);
      IF Inside THEN
      BEGIN
         IF Abs(xSave-xMouse) + Abs(ySave-yMouse) > 0.01 THEN
         BEGIN
            RubOutXY(0.7, 0.8, 15, DarkGray);
            PrintXY(0.7, 0.8, '('+NumStr(xMouse, 6,1)+', '
                                     +NumStr(yMouse, 5,2)+')');
            xSave := xMouse;
            ySave := yMouse;
         END;
      END;
   UNTIL NOT Inside;
   RubOutXY(0.7, 0.8, 15, DarkGray);
   Message('');
END;

{---------------------------------------------------------------------}

BEGIN   {Main Program for Procedure SeeAveragedElements.}
      FOR i := 1 TO 8 DO  TideMenu.RowActivate(5, i, False);
      TideMenu.RowActivate(3,1, True);
      TideMenu.RowActivate(4,1, True);
      TideMenu.RowActivate(4,2, True);
      ElementTime := 1000;
      y[1] := Sma; y[2] := Ecc; y[3] := Inc; y[4] := Rotation1;
      MassRatio := Mass2/Mass1;
      Factor := Gks*Mass1*Lag*Amc/(Radius1*Radius1*Radius1);
      ElementInfo(Sma, ElementTime);
      IF NOT InputAccepted THEN
      BEGIN
         RubOut(1,2, 70, Black);
         LightMenu := True;
         Exit;
      END;
      FillElementArray(ElementTime, y, Elements, y);
      ShowCUPSKeys := True;
      CUPSKeys.Display;
      IF PlotOrbit THEN
      BEGIN
         TideMenu.Display;
             FOR i := 1 TO 8 DO  TideMenu.RowActivate(5, i, True);
         TideMenu.RowActivate(5,3, False);
         TideMenu.RowActivate(5,5, False);
         IF ABS(Elements[1, 3]) < 0.001 THEN
         BEGIN
            TideMenu.RowActivate(5,4, False);
            TideMenu.RowActivate(5,5, False);
         END;
        { TideMenu.RowActivate(3,1, False);
         TideMenu.RowActivate(4,1, False);
         TideMenu.RowActivate(4,2, False);}
         PromptElementChoice;
      END;
END;

{-------------------------------------------------------------------------}

PROCEDURE PromptChoice;
var ViewPort : ViewPortType;
    x1, y1, x2,y2,midx,midy : integer;
begin
   HideCursor;
   SetColor(Black);
   GraphBackColor := Black;
   OpenViewPort(2);
   GetViewSettings (ViewPort);
   with ViewPort do begin midx := (x1+x2) div 2; midy := (y1+y2) div 2; end;
   x1 := midx-200; y1 := midy-100; x2 := midx+200; y2 := midy;
   SetTextJustify(LeftText, TopText);
   SetFillStyle(SolidFill,blue);
   SetColor(Yellow);
   Bar3D(x1,y1+5,x2,y2+5,5,true);  
   OutTextXY(x1+20,y1+20,'  Make a selection from the menu: "Output"');
   OutTextXY(x1+20,y1+40,'   either see short-period perturbations');
   OutTextXY(x1+20,y1+60,'                    or');
   OutTextXY(x1+20,y1+80,'        see averaged perturbations');
   ShowCursor;
end;  

{----------------------------------------------------------------------}

PROCEDURE GetStarted;
VAR
   i: Integer;
BEGIN
   GraphBackColor := Black;
   TideData1(Mass1, Mass2, Mu, MeanRotation,
             Sma, Ecc, Inc, LongNode, ArgPAstron); 
   IF NOT InputAccepted THEN
   BEGIN
      Announce('Data must be complete to see the animation');
      FOR i := 1 TO 8 DO  TideMenu.RowActivate(5, i, False);
      FOR i := 1 TO 2 DO  TideMenu.RowActivate(4, i, False);
      Exit;
   END;
   TideData2(MeanRotation, Radius1, Rotation1, Lag);
   IF NOT InputAccepted THEN
   BEGIN
      RubOut(1, 3, 70, Black);
      Announce('Data must be complete to see the animation');
      FOR i := 1 TO 8 DO  TideMenu.RowActivate(5, i, False);
      FOR i := 1 TO 2 DO  TideMenu.RowActivate(4, i, False);
      Exit;
   END;
   InputChosen := True;
   ShowCursor;
   ShowCUPSKeys := True;
   CUPSKeys.Display;
   PromptChoice;
   FOR i := 1 TO 2 DO  TideMenu.RowActivate(4, i, True);
   FOR i := 1 TO 8 DO  TideMenu.RowActivate(5, i, False);
  { IF TideMenu.Chosen THEN HandleTideMenu;}
END;

{----------------------------------------------------------------------}
 

PROCEDURE HandleTideMenu;
var i : integer;
BEGIN
   With TideMenu DO
   CASE ColChosen OF
       1: CASE RowChosen OF
             1: BEGIN
                   AboutCUPS;
                   Lightmenu := True;
                END;
             2: BEGIN
                   ShowHelp('HELPBins.HLP', 'Tidal Hello');
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
                  ShowHelp('HELPBins.HLP', 'Tidal GeneralA');
                  ShowHelp('HELPBins.HLP', 'Tidal GeneralB');
                  LightMenu := True;
               END;
         END;
      3: BEGIN
            GetStarted;
            ShowCursor;
            ShowCUPSKeys := True;
            CUPSKeys.Display;
            IF InputChosen = False THEN
            BEGIN
               FOR i := 1 TO 8 DO  TideMenu.RowActivate(5, i, False);
               FOR i := 1 TO 2 DO  TideMenu.RowActivate(4, i, False);
            END;
            LightMenu := True;
         END;
      4: CASE RowChosen OF
            1: BEGIN
                  Averaged := False;
                  SeeElements;
               END;
            2: BEGIN
                  Averaged := True;
                  SeeAveragedElements;
               END;
         END;
      5: CASE RowChosen OF
            1: BEGIN
                  AScreen(aMax, aMin, ElementTime); DrawElement(1);
               END;
            2: BEGIN
                  EScreen(eMax, eMin, ElementTime); DrawElement(2);
               END;
            3: BEGIN
                  AphScreen(AphMax, AphMin, ElementTime); DrawElement(4);
               END;
            4: BEGIN
                  IScreen(iMax, iMin, ElementTime); DrawElement(3);
               END;
            5: BEGIN
                  NodeScreen(NodeMax, NodeMin, ElementTime); DrawElement(5);
               END;
            6: BEGIN
                  RotScreen(RotMax, RotMin, ElementTime); DrawElement(6);
               END;
            7: BEGIN
                  IF Averaged THEN SeeAveragedElements
                              ELSE SeeElements;
               END;
            8: BEGIN
                  TideMenu.RowActivate(3,1, True);
                  TideMenu.RowActivate(4,1, True);
                  TideMenu.RowActivate(4,2, True);
                  FOR i := 1 TO 8 DO
                  TideMenu.RowActivate(5,i, False);
                  OpenViewPort(2);
                  IF TideMenu.Chosen THEN HandleTideMenu;
               END;
         END;
    END;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpDefaultInput;
var i: integer;
BEGIN
   Mass1 := 1;
   Mass2 := 1;
   Sma := 0.1;
   Ecc := 0.5;
   IncDeg := 5;
   LongNodeDeg := 180;
   ArgPAstronDeg := 180;
   Radius1Input := 5.0;
   Lag := 0.05;
   Rotation1 := 1;
   DefineViewPort(1, 0, 1, 0.8, 0.9);
   DefineViewPort(2, 0, 1, 0.07, 0.95);
   DefineViewPort(3, 0, 1, 0.07, 0.95);
   InputChosen := False;
   ExitRequest := False;
   ShowCUPSKeys := True;
   SetUpCUPSKeys;
   CUPSKeys.Display;
   GraphBackColor := Black;
   SetUpTideMenu;
   ShowHelp('HELPBins.HLP', 'Tidal Hello');
   FOR i := 1 TO 8 DO  TideMenu.RowActivate(5, i, False);
END;

{-------------------------------------------------------------------------}
procedure RunTidal;
var i: integer;
BEGIN
{   CUPSInit;}
   SetUpDefaultInput;
   GetStarted;
   ShowCursor;
   IF InputChosen = False THEN
   BEGIN
      FOR i := 1 TO 8 DO  TideMenu.RowActivate(5, i, False);
      FOR i := 1 TO 2 DO  TideMenu.RowActivate(4, i, False);
      ShowCUPSKeys := True;
      CUPSKeys.Display;
   END;
   LightMenu := True;
   REPEAT
      CheckForEvents;
      IF ShowCUPSKeys THEN
              IF CUPSKeys.Pressed(Key) THEN HandleCUPSKeys(Key);
      IF LightMenu THEN
      BEGIN
         LightMenu := False;
         IF TideMenu.Chosen THEN HandleTideMenu;
      END
      ELSE
         IF TideMenu.Activated THEN HandleTideMenu;
   UNTIL ExitRequest;
   TideMenu.Done;
{  CUPSDone;}
END;

END.