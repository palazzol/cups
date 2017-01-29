    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1994.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program to determine and animate the          *)
    (*       true orbit of a visual binary.                *)
    (*                                                     *)
    (*******************************************************)

unit Visual2;
{$M 64000,0,327680}
{$O+}
{$F+}
interface
procedure RunVisual2;

implementation
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;

procedure RunVisual2;
VAR
   VBMenu                             : TMenu;
   HotKeys                            : THotKeys;
   Key                                : Byte;
   ApEcc, a, b                        : Real;  {Parameters for the
                                                apparent ellipse.}
   Sma, Ecc, Inc, LongNode, ArgPAstron: Real;  {True orbnital elements.}
   xA, yA                             : real;  {Coordinates of the primary.}
   VBDataOK                           : Boolean;
   ASelected                          : Boolean;
   LightMenu                          : Boolean;
   ExitRequest                        : Boolean;
   ObservationsMade                   : Boolean;
   Sliders                            : TSliders;

PROCEDURE HandleVBMenu; FORWARD;

{------------------------------------------------------------------------}

PROCEDURE SetUpVBMenu;
BEGIN
   With VBMenu DO
   BEGIN
      Init;
      Column(1, 'File');
       row(1,1, 'About CUPS');
       row(1,2, 'About Program');
       row(1,3, 'Configuration');
       row(1,4, 'Exit this module');
      Column(2, 'Help');
       row(2,1, 'General information');
       row(2,2, 'Orbital elements');
       row(2,3, 'Observations');
      Column(3, 'Run');
       row(3,1, 'Select eccentricity');
       row(3,2, 'Click on the location of star A');
       row(3,3, 'Make "observations"');
       row(3,4, 'See three-D animation');
   END;
   VBMenu.AutoChecking(3,1);
   VBMenu.Display;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      HotKeys.Init(2);
      Key[1] := 'F1-Help';
      Key[2] := 'F10-Menu';
   END;
   HotKeys.Display;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleHotKeys(Key: Byte);
BEGIN
      CASE Key OF
         1: ShowHelp('HelpBins.HLP', 'Visual2 Hello');
         2: IF VBMenu.Chosen THEN HandleVBMenu;
      END;
END;

{------------------------------------------------------------------------}

Procedure DrawEllipse(a, b: real; EllipseColor: Word);
VAR
   xSc, ySc, axSc, aySc, bxSc, bySc: integer;
   xOld, yOld, xNew, yNew, T, CosT, SinT: Real;
   i: Integer;
BEGIN
   SetColor(EllipseColor);
   Map(0, 0, xSc, ySc);
   Map(a, 0, axSc, aySc);
   Map(0, b, bxSc, bySc);
   HideMouse;
   Ellipse(xSc, ySc, 0, 360, axSc - xSc, ySc - bySc);
   ShowMouse;
   {IF MousePresent THEN VBMenu.RowActivate(4,1, True);}
END;

{------------------------------------------------------------------------}

Procedure DrawScale;
VAR
   i: integer;
   xSc, ySc: Integer;
   VBString: String;
BEGIN
   PlotLine(-5, -3, 5, -3);
   FOR i := 1 TO 9 DO
   BEGIN
      PlotLine(i-5, -3, i-5, -2.9);
      Map(i-5, -3.2, xSc, ySc);
      VBString := NumStr(i-5, 2, 0);
      OutTextxy(xSc-5, ySc, VBString);
   END;
   Map(-2, -3.5, xSc, ySc);
   OutTextxy(xSc, ySc, 'Scale for the semimajor axis');
END;

{------------------------------------------------------------------------}

Procedure LocateStarA(VAR xA, yA: real);
VAR
   Inside, Ins: Boolean;
   xSc, ySc: integer;
   xm, ym: Word;
   Button: Word;


BEGIN
   Map(-2.5, 3.9, xSc, ySc);
   HideCursor;
   Print(18, 3, 'Click inside the ellipse to locate Star A.');
   ShowCursor;
   REPEAT
      WaitOnMouseClick;
      MousePosn(xA, yA, 1, 1, Ins);
      If (xA*xA/(a*a) + yA*yA/(b*b) > 1) THEN
                      BEGIN
                         Sound(440);
                         Delay(100);
                         NoSound;
                      END
                      ELSE Inside := True;
   UNTIL Inside = True;
   rubOut(18, 3, 43, Black);
   ASelected := True;
   VBMenu.RowActivate(3,3, True);
   VBMenu.RowActivate(3,4, True);
   VBMenu.RowActivate(3,2, False);
END;

{------------------------------------------------------------------------}

Function Atan2(si, co: real): real;
{Finds the angle between 0 and 2*Pi of which
the sine is proportional to si and the cosine is proportional to co.
equivalent to the FORTRAN function.}
VAR
   Pi: real;
begin
   Pi := 3.141592653589793;
   if si < 0 then
      begin
         if co = 0 then Atan2 := 1.5*Pi
         else
            if co > 0 then Atan2 := 2*Pi + ArcTan(si/co)
            else Atan2 := Pi + ArcTan(si/co);
      end
   else
      begin
         if co = 0 then Atan2 := Pi/2
                   else Atan2 := ArcTan(si/co);
         if co < 0 then Atan2 := Pi + ArcTan(si/co);
      end;
end;

{------------------------------------------------------------------------}

Procedure NumToStr(Num: real; i, j: integer; VAR OutStr: String);
VAR
   Tempstr: String;
BEGIN
   Str(Num: i: j, Tempstr);
   OutStr := TempStr;
END;

{------------------------------------------------------------------------}
   
Procedure TrueOrbit(xA, yA, Appa, Appb: real;
                     VAR  Sma, Ecc, Inc, Node, PAstron: real);
VAR
   A, B, F, G: real;
   Z1, Z2: real;
   n: integer;
   xSc, ySc: integer;
   PrtElement, ElString: String;

{------------------------------------------------------------------------}

Procedure PlotTrueEllipse;
VAR
   x, y: real;
   n: integer;
   t: real;

Function Tan(Ang: real): real;
BEGIN
  Tan := Sin(Ang)/Cos(Ang);
END;

BEGIN
   SetColor(7);
   For n := 1 TO 100 DO
   BEGIN
      x := 2*Cos(n*(2*Pi/100));
      y := 2*Sqrt(1 - Sqr(ApEcc))*Sin(n*(2*Pi/100));
      t := ((x - xA)*cos(Node) - (y - yA)*Sin(Node))*(1/Cos(Inc) - 1);
      x := x + t*Cos(Node);
      y := y - t*Sin(Node);
      PlotSymbol(x, y, '.');
   END;
   x := xA/Ecc;
   y := yA/Ecc;
   t := ((x - xA)*cos(Node) - (y - yA)*Sin(Node))*(1/Cos(Inc) - 1);
   x := x + t*Cos(Node);
   y := y - t*Sin(Node);
   PlotLine(xA, yA, x, y);  {Draw line to periastron.}
   Map(x, y, xSc, ySc);
   OutTextxy(xSc, ySc, 'Periastron');
END;

BEGIN
   HideMouse;
   PlotSymbol(xA, yA, '*');
   Ecc := Sqrt(Sqr(xA)/Sqr(Appa) + Sqr(yA)/Sqr(Appb));
   ElString := NumStr(Ecc, 5, 4);
   PrtElement := Concat('Eccentricity = ', ElString);
   Map(-5, 3.4, xSc, ySc);
   OutTextxy(xSc, ySc, PrtElement);
   A := - 1/(Sqr(Appb)*(1 - Sqr(Ecc)));
   B := - 1/(Sqr(Appa)*(1 - Sqr(Ecc)));
   G :=  ya*A;
   F :=  xA*B;
   Node := (360/(2*Pi))*(1/2)*ATan2(-2*F*G, Sqr(F) - Sqr(G) + A - B);
   ElString := NumStr(Node, 7, 4);
   PrtElement := Concat('Longitude of Node = ', ElString);
   Map(-5, 2.9, xSc, ySc);
   OutTextxy(xSc, ySc, PrtElement);
   Node := (2*Pi/360)*Node;
   Z1 := Sqrt(Sqr(2*F*G) + Sqr(Sqr(F) - Sqr(G) + A - B)); {Sqr(tani/p).}
   Z2 := Sqr(F) + Sqr(G) - A - B - Z1; {2/Sqr(p).}
   Z1 := Sqrt(2*Z1/Z2);    {tani.}
   Inc := Arctan(Z1);
   Sma := Sqrt(2/Z2)/(1 - Sqr(Ecc));
   Pastron := (360/(2*Pi))*ATan2((- F*Cos(Node) + G*Sin(Node))*Cos(Inc),
                                (- G*cos(Node) - F*sin(Node)));
   Inc := (360/(2*Pi))*Inc;
   Elstring := NumStr(Inc, 7, 4);
   PrtElement := Concat('Inclination angle  = ', ElString);
   Map(-5, 2.4, xSc, ySc);
   OutTextxy(xSc, ySc, PrtElement);
   ElString := NumStr(Pastron, 7, 4);
   PrtElement := Concat('Argument of Periastron = ', ElString);
   Map(-5, 1.9, xSc, ySc);
   OutTextxy(xSc, ySc, PrtElement);
   Elstring := NumStr(Sma, 5, 4);
   PrtElement := Concat('Semimajor axis = ', ElString);
   Map(-5, 1.4, xSc, ySc);
   OutTextxy(xSc, ySc, PrtElement);
   SetColor(7);
   Inc := (2*Pi/360)*Inc;
   Pastron := (2*Pi/360)*Pastron;
   PlotLine(xA - 3*Sin(Node), yA - 3*Cos(Node),
            xA + 3*Sin(Node), yA + 3*Cos(Node));    {Line of Nodes.}
   Map(xA + 2*Sin(Node), yA + 2*Cos(Node), xSc, ySc);
   OutTextxy(xSc, ySc, 'Line of nodes');
   PlotLine(xA, yA, xA, 3.5);
   PlotLine(xA, 3.5, xA - 0.2, 3.3);
   PlotLine(xA, 3.5, xA + 0.2, 3.3);
   Map(xA + 0.3, 3.5, xSc, ySc);
   OutTextxy(xSc, ySc, 'North');
   PlotTrueEllipse;
   ShowMouse;
END;

{------------------------------------------------------------------------}

PROCEDURE FindTrueOrbit;
BEGIN
   a := 2; b := a*Sqrt(1 - ApEcc*ApEcc);
   GraphBackColor := Black;
   OpenViewPort(1);
   SelectScale(1);
   SetColor(Yellow);
   DrawEllipse(a, b, Yellow);
   DrawScale;
   LocateStarA(xA, yA);
   TrueOrbit(xA, yA, a, b, Sma, Ecc, Inc, LongNode, ArgPAstron);
   ShowCursor;
   LightMenu := True;
   ObservationsMade := False;
END;

{------------------------------------------------------------------------}

PROCEDURE RestoreScreen;
BEGIN
   HotKeys.Display;
   VBMenu.Display;
   a := 2; b := a*Sqrt(1 - ApEcc*ApEcc);
   GraphBackColor := Black;
   OpenViewPort(1);
   SelectScale(1);
   SetColor(Yellow);
   DrawEllipse(a, b, Yellow);
   DrawScale;
   {LocateStarA(xA, yA);}
   TrueOrbit(xA, yA, a, b, Sma, Ecc, Inc, LongNode, ArgPAstron);
   ShowCursor;
   LightMenu := True;
END;

{------------------------------------------------------------------------}

PROCEDURE MakeObservations;

VAR
   v0, v1, dv, dE, t, Time: Real;
   x, y, r, theta: Real;
   xGlobal, yGlobal: Integer;
   Observing: Boolean;
   TimeStr, RStr, ThetaStr: String;
   xSc, ySc: Integer;
   Times, Radii, PositionAngles: Array[1..30] OF Real;
   ObsNumber: Integer;

PROCEDURE RecordObservations;
CONST
   RBNames: String = '12345';

TYPE
   PointRecord = Record
                    x, y, z : Real;
                 END;

VAR
   Filename               : String;
   MakeFile               : Boolean;
   F1                     : File of PointRecord;
   Points                 : PointRecord;

VAR
   SaveObs : TInputScreen;
   i       : Integer;

BEGIN
   WITH SaveObs DO
   BEGIN
      Init;
      DefineInputPort(0.2, 0.8, 0.4, 0.7);
      LoadLine('');
      LoadLine('Store results in a file? No #1  Yes #1');
      LoadLine('                  File name: "VisualA.POS"');
      LoadLine('');
      LoadLine('     [  OK  ]           [Cancel]');
      AcceptScreen;
      IF GetRadioButton(RBNames[1]) = 1 THEN MakeFile := False
                                        ELSE MakeFile := True;
      FileName := GetString(3);
      Done;
   END;
   IF (MakeFile) THEN
   BEGIN
      Assign(F1, Filename);
      ReWrite(F1);
      FOR i := 1 TO ObsNumber-1 DO
      BEGIN
         Points.x := Times[i];
         Points.y := Radii[i];
         Points.z := PositionAngles[i];
         Write(F1, Points);
      END;
      Close(F1);
   END;
END;    

PROCEDURE Rescale(xGlobal, yGlobal: Integer; VAR x, y: Real);
VAR
   XFactor, YFactor: REAL;
BEGIN
   WITH Views[1] DO
      WITH GraphScales[1] DO
      BEGIN
         xFactor := (xmax - xmin) / (Vx2 - Vx1);
         yFactor := (ymax - ymin) / (Vy2 - Vy1);
         X       := (xFactor * (xGlobal - Vx1)) + xmin;
         Y       :=-(yFactor * (yGlobal - Vy1)) + ymax;
      END;
END;

BEGIN
   IF ObservationsMade THEN RestoreScreen;
   SelectScale(1);
   SetColor(Yellow);
   ObsNumber := 1;
   x := xA/Ecc;
   y := yA/Ecc;
   t := ((x - xA)*cos(LongNode) - (y - yA)*Sin(LongNode))*(1/Cos(Inc) - 1);
   x := x + t*Cos(LongNode);
   y := y - t*Sin(LongNode);
   v0 := ATan2(y-yA, x-xA);
   Map(-4.5, -2, xSc, ySc);
   OutTextxy(xSc, ySc, 'Click on the orbit to "observe"');
   Map(-4.5, -2.2, xSc, ySc);
   OutTextxy(xSc, ySc, 'Up to 30 observations are possible');
   Map(-4.5, -2.4, xSc, ySc);
   OutTextxy(xSc, ySc, 'Click on the menu to quit');
   Map(2.1, 3.5, xSc, ySc);
   OutTextxy(xSc, ySc, 'time      r    theta');
   Observing := True;
   REPEAT
      IF MouseClicked(xGlobal, yGlobal) THEN
      BEGIN
         Rescale(xGlobal, yGlobal, x, y);
         IF y < 4 THEN
         BEGIN
            PlotSymbol(x, y, '*');
            R := Sqrt((x-xA)*(x-xA) + (y-yA)*(y-yA));
            Theta := (180/Pi)*ATan2(y-yA, x-xA);
            Theta := 90 - Theta;
            IF Theta < 0 THEN Theta := 360 + Theta;
            t := ((x - xA)*cos(LongNode) - (y - yA)*Sin(LongNode))*(1/Cos(Inc) - 1);
            x := x + t*Cos(LongNode);
            y := y - t*Sin(LongNode);
            v1 := ATan2(y-yA, x-xA);
            dv := v1 - v0;
            IF dv < 0 THEN
            BEGIN
               dv := dv + 2*Pi;
               v0 := v0 - 2*Pi;
            END;
            dE := 2*ArcTan(Sqrt((1-Ecc)/(1+Ecc))*Sin(dv/2)/Cos(dv/2));
            Time := (dE - Ecc*Sin(dE))/(2*Pi);
            IF Time < 0 THEN Time := 1 + Time;
            Times[ObsNumber] := Time;
            Radii[ObsNumber] := R;
            PositionAngles[ObsNumber] := Theta;
            TimeStr := NumStr(Time,6,4);
            RStr := NumStr(R,4,2);
            ThetaStr := NumStr(Theta,6,1);
            Map(2.0, 3.5- ObsNumber*0.2, xSc, ySc);
            OutTextxy(xSc, ySc, Concat(TimeStr,'   ',Rstr,'  ',ThetaStr));
            ObsNumber := ObsNumber + 1;
         END
         ELSE
         BEGIN
            Observing := False;
           { LightMenu := True;}
         END;
      END;
      IF ObsNumber > 30 THEN BEGIN Observing := False; Beep; END;
   UNTIL NOT Observing;
   IF ObsNumber > 3 THEN RecordObservations;
   IF ObsNumber > 0 THEN ObservationsMade := True;
   LightMenu := True;
END;


{------------------------------------------------------------------------}

PROCEDURE ThreeDDemo(Sma, Ecc, Inc, LongNode, ArgPAstron: Real);

VAR
   t, Dt                                      : Real;
   Radius, Azimuth, Altitude                  : Real;
   Caz, Saz, Calt, Salt                       : Real;
   DemHotKeys                                 : THotKeys;
   DemLag                                     : Integer;
   Key                                        : Byte;
   AnimationOff                               : Boolean;
   Picture                                    : TFrames;
   Period                                     : Real;
   xInit, yInit, zInit, xvInit, yvInit, zvInit: Real;
   ax, ay, az, bx, by, bz                     : Real;

{------------------------------------------------------------------------}

PROCEDURE SetUpDemHotKeys;
BEGIN
   WITH DemHotKeys DO
   BEGIN
      DemHotKeys.Init(7);
      Key[1] := 'F5-Faster';
      Key[2] := 'F6-Slower';
      Key[3] := 'RIGHT-Right';
      Key[4] := 'LEFT-Left';
      Key[5] := 'UP-Up';
      Key[6] := 'DOWN-Down';
      Key[7] := 'F10-Quit';
   END;
   DemHotKeys.Display;
END;


PROCEDURE HandleDemHotKeys(key: Byte; VAR AnimationOff: Boolean);
CONST
   Increment = Pi/36;
   DelLag = 10;
BEGIN
   CASE Key OF
      1: IF DemLag > DelLag THEN DemLag := DemLag - DelLag;
      2: DemLag := DemLag + DelLag;
      3: Azimuth := Azimuth + Increment;
      4: Azimuth := Azimuth - Increment;
      5: Altitude := Altitude + Increment;
      6: Altitude := Altitude - Increment;
      7: AnimationOff := True;
   END;
END;


PROCEDURE View(x, y, z: Real; VAR XView, YView: Real);
BEGIN
   XView := (x*Caz - z*Saz);
   YView := (y*Calt - (z*Caz + x*Saz)*Salt);
END;

PROCEDURE Arrow ( x0, y0, z0, x1, y1, z1 : REAL;
                                   barbs : BOOLEAN);
   {Draws an arrow from (x0,y0,z0) to (x1,y1,z1) on the active
    viewport using perspective projection.  Barbs are 1/10 length
    of arrow and assumed to lie in the y-z plane.}

CONST   BarbSize = 0.1;
        range = 8;

VAR
   xorg, yorg   : REAL ;
   xend, yend   : REAL ;
   zbarb,xbarb  : REAL ;
   temp         : REAL ; {Intermediate Variable}

BEGIN
   View(x0,y0,z0,xorg,yorg);   { Plot line from origin to terminus.}
   View(x1,y1,z1,xend,yend);
   IF abs(xend) > range THEN
   BEGIN          {Replace by intersection with x-boundary.}
      temp := range * xend / abs(xend);
      yend := yorg + (yend - yorg) * (temp - xorg) / (xend - xorg);
      xend := temp
   END;
   IF abs(yend) > range THEN
   BEGIN          {Replace by intersection with y-boundary.}
      temp := range * yend / abs(yend);
      xend := xorg + (xend - xorg) * (temp - yorg) / (yend - yorg);
      yend := temp
   END;
   PlotLine(xorg,yorg,xend,yend);
   IF barbs THEN             {Add barbs at terminus.}
      BEGIN
      zbarb := z1 + BarbSize*(-z1+z0+(x1-x0)/2);  {Plot barb lines.}
      xbarb := x1 + BarbSize*(-x1+x0-(z1-z0)/2);
      xorg := xend ; yorg := yend ;
      View(xbarb,y1,zbarb,xend,yend);
      Plotline(xorg,yorg,xend,yend);
      zbarb := z1 + BarbSize*(-z1+z0-(x1-x0)/2);
      xbarb := x1 + BarbSize*(-x1+x0+(z1-z0)/2);
      View(xbarb,y1,zbarb,xend,yend);
      Plotline(xorg,yorg,xend,yend);
      END;
END; {Arrow}

PROCEDURE SetUpInitialVectors(Sma, Ecc, LongNode, ArgPAstron, Inc: Real;
                       VAR xInit, yInit, zInit, xvInit, yvInit, zvInit: Real;
                       VAR ax, ay, az, bx, by, bz: Real);
VAR
   x, y, z, xv, yv, zv: Real;
   Temp: Real;

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
   END; {Case.}
END;

{------------------------------------------------------------------------}

Procedure Project(LongNode, ArgPAstron, Inc, xin, yin, zin: Real;
      VAR xout, yout, zout: Real);
VAR
   nr: Integer;
   th: Real;
BEGIN
   nr := 3; th := - ArgPAstron; RotateAxes(nr, th, xin, yin, zin,
   xout, yout, zout);
   xin := xout; yin := yout; zin := zout;
   nr := 1; th := - Inc; RotateAxes(nr, th, xin, yin, zin,
   xout, yout, zout);
   xin := xout; yin := yout; zin := zout;
   nr := 3; th := - LongNode; RotateAxes(nr, th, xin, yin, zin,
   xout, yout, zout);
END;

BEGIN
   x := Sma*(1 - Ecc); xv := 0;
   y := 0; yv := Sqrt((1 + Ecc)/x);
   z := 0; zv := 0;
   Project(LongNode, ArgPAstron, Inc, x, y, z, xInit, yInit, zInit);
   Project(LongNode, ArgPAstron, Inc, xv, yv, zv, xvInit, yvInit, zvInit);
   Temp := Sqrt(xInit*xInit + yInit*yInit + zInit*zInit);
   ax := Sma*xInit/Temp; ay := Sma*yInit/Temp; az := Sma*zInit/Temp;
   Temp := Sqrt(xvInit*xvInit + yvInit*yvInit + zvInit*zvInit);
   Temp := Sma*Sqrt(1 - Ecc*Ecc)/Temp;
   bx := xvInit*Temp; by := yvInit*Temp; bz := zvInit*Temp;
END;


PROCEDURE DrawOrbits(ax, ay, az, bx, by, bz: Real);
VAR
   Angle, DelAngle, CAngle, SAngle, CDelAngle, SDelAngle, Spare: Real;
   xOld, yOld, xNew, yNew: Real;
   xAppOld, yAppOld, xAppNew, yAppNew: Real;
   x, y, z: Real;
   i: Integer;
BEGIN  
   DelAngle := 3.14159265/20;
   CDelAngle := Cos(DelAngle);
   SDelAngle := Sin(DelAngle);
   CAngle := 1;
   SAngle := 0;
   x := ax*(CAngle - Ecc) + bx*SAngle;
   y := ay*(CAngle - Ecc) + by*SAngle;
   z := az*(CAngle - Ecc) + bz*SAngle;
   View(x, y, z, xOld, yOld);
   View(x, y, 0, xAppOld, yAppOld);
   SetColor(Yellow);
   FOR i := 1 TO 40 DO
   BEGIN
      Spare := Cangle*CDelAngle - SAngle*SDelAngle;
      Sangle := SAngle*CDelAngle + CAngle*SDelAngle;
      CAngle := Spare;
      x := ax*(CAngle - Ecc) + bx*SAngle;
      y := ay*(CAngle - Ecc) + by*SAngle;
      z := az*(CAngle - Ecc) + bz*SAngle;
      View(x, y, z, xNew, yNew);
      View(x, y, 0, xAppNew, yAppNew);
      SetColor(LightBlue);
      PlotLine(xOld, yOld, xNew, yNew);
      SetColor(Yellow);
      PlotLine(xAppOld, yAppOld, xAppNew, yAppNew);
      xOld := xNew; yOld := yNew;
      xAppOld := xAppNew; yAppOld := yAppNew;
   END;
END;


PROCEDURE TrigFunctions;
BEGIN
   Calt := Cos(Altitude);
   Salt := Sin(Altitude);
   Caz := Cos(Azimuth);
   Saz := Sin(Azimuth);
END;

PROCEDURE PrepareGraph;
VAR Problem: Real;
BEGIN
   GraphBackColor := Black;
   SetBkColor(Black);
   DefineViewPort(5, 0.1, 0.9, 0.15, 0.9);
   DefineScale(5, -2, 2, -2, 2);
   SelectScale(5);
END;



PROCEDURE PlotPoints(Time, Sma, Ecc,
                     xInit, yInit, zInit, xvInit, yvInit, zvInit: Real);
VAR
   n, m, c, s, f, g: Real;
   x, y, z, xView, yView: Real;

Procedure Kepler(m, Ecc: Real; VAR x, c, s: Real);
{Solves Kepler's equation for elliptic motion. Starts at pericenter.}
VAR
   f, fp, fpp, fppp, dx: Real;
   ms, sigma, k: Real;
begin
   k := 0.85;
   ms := m - Int(m/(2*pi))*(2*pi);
   IF Sin(ms) < 0 THEN sigma := -1 ELSE sigma := 1;
   x := ms+ sigma*k*Ecc;
   repeat
      s := sin(x); c := cos(x);
      f := x - Ecc*s - m;
      fp := 1 - Ecc*c;
      fpp := Ecc*s;
      fppp := Ecc*c;
      dx := - f/fp;
      dx := - f/(fp + dx*fpp/2);
      dx := - f/(fp + dx*fpp/2 + dx*dx*fppp/6);
      x := x + dx;
   until abs(dx) < 0.0001;
end;

{------------------------------------------------------------------------}

BEGIN
   n := 1/(Sma*Sqrt(Sma));
   m := n*Time;
   Kepler(m, Ecc, x, c, s);
   f := (c - 1)/(1 - Ecc) + 1;
   g := Time + (s - x)/n;
   x := xInit*f + xvInit*g;
   y := yInit*f + yvInit*g;
   z := zInit*f + zvInit*g;
   View(x, y, z, xView, yView);
   SetColor(LightBlue);
   PlotSymbol(xView, yView, '*');
   View(x, y, 0, xView, yView);
   SetColor(Yellow);
   PlotSymbol(xView, yView, '*');
end;



PROCEDURE PlotFigure(t: Real);
VAR
   xOld, yOld, xNew, yNew: Real;
   i: Integer;
   xSc, ySc: Integer;
   x, y, z: Real;
BEGIN    
   HideCursor;
   OpenViewPort(5);
   DemHotKeys.Display;
   DrawOrbits(ax, ay, az, bx, by, bz);
   PlotPoints(t, Sma, Ecc,
                     xInit, yInit, zInit, xvInit, yvInit, zvInit);
   SetColor(White);
   View(0, 0, 0, xNew, yNew);
   PlotSymbol(xNew, yNew, '*');
   Arrow(0, 0, 0, 0, 0, -1.4, True);
   View(0, 0, -1.6, xNew, yNew);
   Map(xNew, yNew, xSc, ySc);
   OutTextxy(xSc, ySc, 'Observer');
   Arrow(0, 0, 0, 1.4, 0, 0, True);
   View(1.6, 0, 0, xNew, yNew);
   Map(xNew, yNew, xSc, ySc);
   OutTextxy(xSc, ySc, 'North');
   Radius := Sma*(1 - Ecc*Ecc)/(1 + Ecc*Cos(ArgPAstron));
   View(Radius*Cos(LongNode), Radius*Sin(LongNode), 0, xNew, yNew);
   Radius := Sma*(1 - Ecc*Ecc)/(1 - Ecc*Cos(ArgPAstron));
   View(-Radius*Cos(LongNode), -Radius*Sin(LongNode), 0, xOld, yOld);
   PLotLine(xNew, yNew, xOld, yOld);
   Map(xNew, yNew, xSc, ySc);
   View(1.1*Cos(LongNode), 1.1*Sin(LongNode), 0, xNew, yNew);
   OutTextxy(xSc, ySc, 'Node');
   View(1.6*ax, 1.6*ay, 1.6*az, xOld, yOld);
   SetColor(LightBlue);
   Arrow(0, 0, 0, 1.5*ax, 1.5*ay, 1.5*az, True);
   Map(xOld, yOld, xSc, ySc);
   OutTextxy(xSc, ySc, 'PeriAstron');
   Map(-1.8, 1.9, xSc, ySc);
   SetColor(LightBlue);
   OutTextxy(xSc, ySc, 'True orbit');
   Map(-1.8, 1.7, xSc, ySc);
   SetColor(Yellow);
   OutTextxy(xSc, ySc, 'Apparent orbit');
END;



PROCEDURE AnimateMotion;
BEGIN
   SetUpDemHotKeys;
   AnimationOff := False;
   REPEAT
      CheckForEvents;
      IF DemHotKeys.Pressed(Key) THEN HandleDemHotKeys(Key, AnimationOff);
      TrigFunctions;
      t := t + Dt;
      IF t > Period THEN t := t - Period;
      PlotFigure(t);
      Delay(DemLag);
      Picture.SwitchAndClean;
   UNTIL (AnimationOff = True);
END;


BEGIN
   CloseGraph;
   CUPSDone;
   Picture.Init;
   CUPSInit;
   DemLag := 0;
   t := 0;
   Dt := 0.1;
   Sma := 1;
   Period := 2*3.14159*Sma*Sqrt(Sma);
   Radius := 5;
   Azimuth := 0;
   Altitude := -3*Pi/8;
   TrigFunctions;
   SetUpInitialVectors(Sma, Ecc, LongNode, ArgPAstron, Inc,
                       xInit, yInit, zInit, xvInit, yvInit, zvInit,
                       ax, ay, az, bx, by, bz);
   PrepareGraph;
   AnimateMotion;
   Picture.Close;
   CloseGraph;
   CUPSDone;
   GraphDriver := Detect;
   CUPSInit;
   RestoreScreen;
END;

{------------------------------------------------------------------------}

 PROCEDURE GetData;
 BEGIN
    HotKeys.Display;
    VBMenu.RowActivate(3,2, True);
    VBMenu.RowActivate(3,3, False);
    VBMenu.RowActivate(3,4, False);
    GraphBackColor := Black;
    DefineViewPort(2, 0, 1, 0.07, 0.95);
    DefineScale(1, -5, 5, -4, 4);
    OpenViewPort(2);
    ShowCursor;
    SetColor(White);
    Print(5, 3, 'When ready, select "Run: Click on the location of star A" from the menu.');
    Sliders.Create(1, 0, 0.99, 0.5, 0.1, 0.2, 0.8, 3, '0', '0.99', 'Eccentricity', True);
    Sliders.DrawAll;
    ApEcc := Sliders.Value(1);
    ApEcc := 0.5;
    a := 2;
    b := a*Sqrt(1 - ApEcc*ApEcc);
    DrawEllipse(a, b, Yellow);
    SetColor(Yellow);
    Print(15, 20, 'Select the eccentricity of the apparent orbit');
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpInitialParameters;
BEGIN
   DefineViewPort(1, 0, 1, 0.07, 0.95);
   DefineScale(1, -5, 5, -4, 4);
   SetBkColor(Black);
   Sma := 1;
   Ecc := 0.5;
   Inc := 0.5;
   LongNode := 1;
   ArgPAstron := 1;
   ASelected := False;
   ExitRequest := False;
   LightMenu := False;
   ObservationsMade := False;
   SetUpVBMenu;
   SetUpHotKeys;
   ShowHelp('HelpBins.HLP', 'Visual2 Hello');
   VBMenu.RowActivate(3,2, False);
   VBMenu.RowActivate(3,3, False);
   VBMenu.RowActivate(3,4, False);
END;

{------------------------------------------------------------------------}

PROCEDURE HandleVBMenu;
BEGIN
   With VBMenu DO
   CASE ColChosen OF
      1: CASE RowChosen OF
            1: BEGIN
                  AboutCUPS;
                  LightMenu := True;
               END;
            2:
            BEGIN
                  ShowHelp('HelpBins.HLP', 'Visual2 Hello');
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
                  ShowHelp('HelpBins.HLP', 'Visual2 General');
                  LightMenu := True;
               END;
            2: BEGIN
                  ShowHelp('HelpBins.HLP', 'Visual2 Elements1');
                  ShowHelp('HelpBins.HLP', 'Visual2 Elements2');
                  LightMenu := True;
               END;
            3: BEGIN
                  ShowHelp('HelpBins.HLP', 'Visual2 Observations');
                  LightMenu := True;
               END;
         END;
      3: CASE RowChosen OF
            1: BEGIN
                  GetData;
                  ASelected := False;
               END;
            2: FindTrueOrbit;
            3: MakeObservations;
            4: ThreeDDemo(Sma, Ecc, Inc, LongNode, ArgPAstron);
         END;
    END;
END;


{------------------------------------------------------------------------}

BEGIN   {Main Program.}
{  CUPSInit;}
   Sliders.Init;
   SetUpInitialParameters;
   GetData;
   REPEAT
      CheckForEvents;
      IF HotKeys.Pressed(Key) THEN HandleHotKeys(Key);
      IF LightMenu THEN
      BEGIN
         LightMenu := False;
         IF VBMenu.Chosen THEN HandleVBMenu;
      END
      ELSE
         IF VBMenu.Activated THEN HandleVBMenu;
      IF Sliders.Changed THEN
      BEGIN
          ApEcc := Sliders.Value(1);
          DrawEllipse(a, b, Black);
          b := a*Sqrt(1 - ApEcc*ApEcc);
          DrawEllipse(a, b, Yellow);
      END;
   UNTIL ExitRequest;
{   CUPSDone;}
   Sliders.Done;
   VBMenu.done;
END;

END.

(* PROGRAM ReadStarData;
   {This program will read and print a file written by the program Visual2.PAS.
    This is entitled "The True Orbit of a Visual Binary."
    The file contains recorded "observations." These are:
    The time, which is a fraction of the orbital period, measured
    from periastron.
    The separation, measured in units to be defined by the user.
    The position angle.
    The user must change the string "FileName" as appropriate.
    Note: The program is in conventional Pascal, and does not use
    any CUPS utilities. So the command "WriteLn" can be used.}

USES DOS, Crt;

TYPE
   PointRecord = Record
                    x, y, z : Double;
                 END;

VAR
   Filename: String;
   F1: File of PointRecord;
   Points: PointRecord;
   Time, Radius, PositionAngle: Real;

BEGIN
   FileName := 'VisualA.POS'; {Change this to match the relevant file.}
   Assign(F1,FileName);
   Reset(F1);
   REPEAT
      IF NOT Eof(F1) THEN
      BEGIN
         Read(F1, Points);
         Time := Points.x;
         Radius := Points.y;
         PositionAngle := Points.z;
         WriteLn(Time,'   ',Radius,'  ', PositionAngle);  readln;
      END;
   UNTIL Eof(F1);
   Close(F1);
   Readln;
END. *)