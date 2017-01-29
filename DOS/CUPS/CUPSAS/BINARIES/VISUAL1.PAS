    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1994.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program to display the animated motion        *)
    (*       of both members of a visual binary            *)
    (*       relative to the moving center of mass.        *)
    (*                                                     *)
    (*******************************************************)

unit Visual1;
{$M 64000,0,327680}
{$O+}
{$F+}
interface
procedure RunVisual1;

implementation
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;

procedure RunVisual1;
CONST
   dt = 0.1;   {Time between calculated positions.             }

PROCEDURE HandleVBMenu; FORWARD;

VAR
   VBMenu        : TMenu;
   VBDat         : TInputScreen;
   HotKeys       : THotKeys;
   CUPSKeys      : THotKeys;
   Key           : Byte;
   ShowCUPSKeys  : Boolean;
   MassRat, Ecc, ArgNode, ArgPcenter, Inc, Speed: Real; {Orbital elements.}
   ArgNodeDeg, ArgPCenterdeg, IncDeg: Real; {Stored elements for input.}
   Lag           : Integer;  {Delay, in milliseconds, between plotted points.}
   InputAccepted : Boolean;
   DataIn        : Boolean;
   LightMenu     : Boolean; {Menu highlighted when true.}
   Action        : Boolean;
   PlotOrbit     : Boolean;
   RelativeOrbit : Boolean;
   ExitRequest   : Boolean;
   Axo, Ayo, Bxo, Byo, Cxo, Cyo: Real; {Screen coordinates.}
   x, y, z, vx, vy: Real; {Spatial coordinates of A.}
   xSave, ySave  : Array[0..10] OF Real; {Stored points for relative orbit.}
   NumberSaved   : Integer;

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
       row(2,2, 'Parameters');
       row(2,3, 'Three-D demonstration');
      Column(3, 'Data');
       row(3,1, 'Enter data');
      Column(4, 'Plot');
       row(4,1, 'See animated proper motion');
       row(4,2, 'See animated relative motion');
       VBMenu.AutoChecking(4,1);
      Display;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpHotKeys;
BEGIN
   ShowCUPSKeys := False;
   CUPSKeys.Clear;
   WITH HotKeys DO
   BEGIN
      HotKeys.Init(5);
      Key[1] := 'F1-Help';
      Key[2] := 'F2-Stop';
      Key[3] := 'F5-Faster';
      Key[4] := 'F6-Slower';
      Key[5] := 'F10-Menu';
   END;
   HotKeys.BackColor := Blue;
   HotKeys.HotKeyColor := Yellow;
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

PROCEDURE VBData(VAR MassRat, Ecc, ArgNode, ArgPcenter, Inc, Speed: Real);
BEGIN
   Action := False;
   WITH VBDat DO
   BEGIN
      Init;
      DefineInputPort(0.15, 0.85, 0.15, 0.85);
      LoadLine('            Data for a Visual Binary');
      LoadLine('');
      LoadLine(' Mass ratio (between 0 and 1)    = {       }        ');
      LoadLine('');
      LoadLine(' Eccentricity,  e  (0 <= e < 1)  = {       }        ');
      LoadLine('');
      LoadLine(' Longitude of the line of nodes  = {       } degrees');
      LoadLine('');
      LoadLine(' Argument of Periastron          = {       } degrees');
      LoadLine('');
      LoadLine(' Inclination, i  (0 <= i <= 90)  = {       } degrees');
      LoadLine('');
      LoadLine(' Speed across the screen ( >= 0) = {       } units  ');
      LoadLine('');
      LoadLine('       [  OK  ]       [Cancel]       [ Help ]      ');
      SetHelpFile('HelpBins.HLP', 'Visual1 Input help');
      SetNumber(1, MassRat);
      SetNumberLimits(1, 0.000001, 0.999999);
      SetNumber(2, Ecc);
      SetNumberLimits(2, 0, 0.999999);
      SetNumber(3, ArgNodeDeg);
      SetNumberLimits(3, 0, 360);
      SetNumber(4, ArgPCenterDeg);
      SetNumberLimits(4, 0, 360);
      SetNumber(5, IncDeg);
      SetNumberLimits(5, 0, 90);
      SetNumber(6, Speed);
      SetNumberLimits(6, 0, 100);
      AcceptScreen;
      IF Canceled THEN InputAccepted := False ELSE InputAccepted := True;
      MassRat := GetNumber(1);
      Ecc := GetNumber(2);
      ArgNodeDeg :=  GetNumber(3);
      ArgNode := ArgNodeDeg*3.14159/180;
      ArgPcenterDeg :=  GetNumber(4);
      ArgPcenter := ArgPcenterDeg*3.14159/180;
      IncDeg := GetNumber(5);
      Inc := IncDeg*3.14159/180;
      Speed := GetNumber(6);
      DataIn := True;
      Done;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleHotKeys(key: Byte);
VAR
   DelLag: Integer;
BEGIN
   DelLag := 20;
   CASE Key OF
      1: BEGIN
            ShowHelp('HelpBins.HLP', 'Visual1 Hello');
         END;
      2: BEGIN
            IF Action = True THEN
            BEGIN
               Action := False;
               WITH HotKeys DO Key[2] := 'F2-Run';
               HotKeys.BackColor := Red;
               HotKeys.Display;
               IF NOT RelativeOrbit THEN
               BEGIN
                  SetColor(LightGray);
                  PlotLine(Axo, Ayo, Bxo, Byo);
               END;
            END
            ELSE
            BEGIN
               Action := True;
               WITH HotKeys DO Key[2] := 'F2-Stop';
               HotKeys.BackColor := Blue;
               HotKeys.Display;
            END;
         END;
      3: IF Lag > DelLag THEN Lag := Lag - DelLag;
      4: Lag := Lag + DelLag;
      5: BEGIN PlotOrbit := False;
               Action := False;
               LightMenu := True;
               HotKeys.Clear;
               CUPSKeys.Display;
               ShowCUPSKeys := True;
         END;
   END;
END;

{------------------------------------------------------------------------}

Procedure Kepler(m, ec0, es0: Real; VAR x, c, s, fp: Real);
{Solves Kepler's equation for elliptic motion.}
{Uses accelerated Newton's method with quartic convergence.}
VAR
   f, fpp, fppp, dx: Real;
begin
   x := m;
   repeat
      s := sin(x); c := cos(x);
      f := x - ec0*s + es0*(1 - c) - m;
      fp := 1 - ec0*c + es0*s;
      fpp := ec0*s + es0*c;
      fppp := ec0*c - es0*s;
      dx := - f/fp;
      dx := - f/(fp + dx*fpp/2);
      dx := - f/(fp + dx*fpp/2 + dx*dx*fppp/6);
      x := x + dx;
   until abs(dx) < 0.000001;
end;

{------------------------------------------------------------------------}

Procedure Ivp(x0, y0, vx0, vy0: Real; VAR x1, y1, vx1, vy1: Real);
{Solves the initial value problem in two dimensions.}
VAR
   r0, a, n, m, f, g, vf, vg, ec0, es0, x, c, s, fp: Real;
BEGIN
   r0 := sqrt(x0*x0 + y0*y0);
   a := 1/(2/r0 - vx0*vx0 - vy0*vy0);
   n := 1/(a*sqrt(a));
   ec0 := 1 - r0/a;
   es0 := (x0*vx0 + y0*vy0)/(n*a*a);
   m := dt*n;
   Kepler(m, ec0, es0, x, c, s, fp);
   f := (a/r0)*(c - 1) + 1;
   g := dt + (s - x)/n;
   vf := - a/(fp*r0)*n*s;
   vg := (1/fp)*(c - 1) + 1;
   x1 := x0*f + vx0*g;
   y1 := y0*f + vy0*g;
   vx1 := x0*vf + vx0*vg;
   vy1 := y0*vf + vy0*vg;
   vx0 := vx1; vy0 := vy1;
END;

{------------------------------------------------------------------------}

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

Procedure Project(ArgNode, ArgPcenter, Inc, xin, yin, zin: Real;
      VAR xout, yout, zout: Real);
VAR
   nr: Integer;
   th: Real;
BEGIN
   nr := 3; th := - ArgPcenter; RotateAxes(nr, th, xin, yin, zin,
   xout, yout, zout);
   xin := xout; yin := yout; zin := zout;
   nr := 1; th := - Inc; RotateAxes(nr, th, xin, yin, zin,
   xout, yout, zout);
 {  xin := xout; yin := yout; zin := zout;
   nr := 3; th := - ArgNode; RotateAxes(nr, th, xin, yin, zin,
   xout, yout, zout);}
END;

{------------------------------------------------------------------------}

Procedure SetUpOrbits(MassRat, Ecc, ArgNode, ArgPcenter, Inc, Speed: Real);
VAR
   xt, yt, zt: Real;
BEGIN
   GraphBackColor := Black;
   Message('');
   OpenViewPort(1);
   ShowCursor;
   SelectScale(1);
   SetUpHotKeys;
   SetColor(LightGreen);
   OutTextxy(10, 10, 'PROPER MOTION OF A VISUAL BINARY');
   SetColor(Yellow);
   OutTextxy(10, 30, 'Motion of the primary, star A');
   SetColor(Green);
   OutTextxy(10, 50, 'Motion of the secondary, star B');
   SetColor(White);
   OutTextxy(10, 70, 'Motion of the center of mass'); 
   PlotOrbit := True;
   Action := True;
   x := (1 - Ecc); vx := 0;
   y := 0; vy := Sqrt((1 + Ecc)/(1 - Ecc));
   z := 0;
   Project(ArgNode, ArgPcenter, Inc, x, y, z, xt, yt, zt);
{Initial coordinates of the center of mass and the stars.}
   Cxo := 0.5; Cyo := 0;
   IF Abs(Speed) < 0.00001 THEN Cxo := 5;
   Axo := Cxo + (1 - MassRat)*xt; Ayo := (1 - MassRat)*yt;
   Bxo := Cxo - MassRat*xt; Byo := - MassRat*yt;
END;

{------------------------------------------------------------------------}

Procedure PlotOrbits(MassRat, Ecc, ArgNode, ArgPcenter, Inc, Speed: Real);
VAR
   Axn, Ayn, Bxn, Byn, Cxn, Cyn: Real;
   xt, yt, zt: Real;
BEGIN
         Ivp(x, y, vx, vy, x, y, vx, vy);
         z := 0;
         Project(ArgNode, ArgPcenter, Inc, x, y, z, xt, yt, zt);
         Cxn := Cxo + dt*Speed; Cyn := 0;
         Axn := Cxn + (1 - MassRat)*xt;
         Ayn := Cyn + (1 - MassRat)*yt;
         Bxn := Cxn - MassRat*xt;
         Byn := Cyn - MassRat*yt;
         SetColor(Yellow); PlotLine(Axo, Ayo, Axn, Ayn);
         SetColor(Green); PlotLine(Bxo, Byo, Bxn, Byn);
         SetColor(White); PlotLine(Cxo, Cyo, Cxn, Cyn);
         Axo := Axn; Ayo := Ayn;
         Bxo := Bxn; Byo := Byn;
         Cxo := Cxn; Cyo := Cyn;
      Delay(Lag);
      IF (Axo + Bxo > 22) THEN
      BEGIN
         PlotOrbit := False;
         Action := False;
         LightMenu := True;
         HotKeys.Clear;
         CUPSKeys.Display;
         ShowCUPSKeys := True;
      END;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpRelativeOrbit(MassRat, Ecc, ArgNode, ArgPcenter, Inc, Speed: Real);
VAR
   xt, yt, zt: Real;
BEGIN
   GraphBackColor := Black;
   Message('');
   OpenViewPort(1);
   ShowCursor;
   SelectScale(1);
   SetUpHotKeys;
   SetColor(LightGreen);
   OutTextxy(10, 10, 'RELATIVE MOTION OF A VISUAL BINARY');
   SetColor(Yellow);
   OutTextxy(10, 30, 'Primary, star A');
   SetColor(Green);
   OutTextxy(10, 50, 'Secondary, star B');
   PlotOrbit := True;
   Action := True;
   x := (1 - Ecc); vx := 0;
   y := 0; vy := Sqrt((1 + Ecc)/(1 - Ecc));
   z := 0;
   Project(ArgNode, ArgPcenter, Inc, x, y, z, xt, yt, zt);
{Initial coordinates of the star B.}
   Bxo := 5; Byo := 0;
   SetColor(Yellow);
   PlotSymbol(Bxo, Byo, '*');
   Axo := Bxo + 2*xt;
   Ayo := Byo + 2*yt;
   xSave[0] := Axo;
   ySave[0] := Ayo;
   NumberSaved := 0;
END;

{------------------------------------------------------------------------}

Procedure PlotRelativeOrbit(MassRat, Ecc, ArgNode, ArgPcenter, Inc, Speed: Real);
VAR
   Axn, Ayn, Bxn, Byn, Cxn, Cyn: Real;
   xt, yt, zt: Real;
   i: Integer;
BEGIN
      IF NumberSaved <= 5 THEN NumberSaved := NumberSaved + 1;
      Ivp(x, y, vx, vy, x, y, vx, vy);
      z := 0;
      Project(ArgNode, ArgPcenter, Inc, x, y, z, xt, yt, zt);
      Axn := Bxo + 2*xt; 
      Ayn := Byo + 2*yt;
      SetColor(GraphBackColor);
      PlotSymbol(Axo, Ayo, '*');
      SetColor(Green);
      PlotSymbol(Axn, Ayn, '*');
      {Re-plot a trail behind the moving star.}
      IF NumberSaved <= 5 THEN
      BEGIN
         xSave[NumberSaved] := Axn;
         ySave[NumberSaved] := Ayn;
         FOR i := 1 TO NumberSaved DO
            BEGIN
               SetColor(LightGray);
               PlotLine(xSave[i-1], ySave[i-1], xSave[i], ySave[i]);
            END;
      END
      ELSE
      BEGIN
         FOR i := 0 TO 4 DO
         BEGIN
            xSave[i] := xSave[i+1];
            ySave[i] := ySave[i+1];
         END;
         xSave[5] := Axn;
         ySave[5] := Ayn;
         FOR i := 1 TO 5 DO
         BEGIN
            SetColor(LightGray);
            PlotLine(xSave[i-1], ySave[i-1], xSave[i], ySave[i]);
         END;
      END;
      Axo := Axn; Ayo := Ayn;
      Delay(Lag);
END;


{-------------------------------------------------------------------------}

PROCEDURE ThreeDDemo;

VAR
   t, Dt                                       : Real;
   Radius, Azimuth, Altitude                   : Real;
   Caz, Saz, Calt, Salt                        : Real;
   DemHotKeys                                  : THotKeys;
   DemLag                                      : Integer;
   Key                                         : Byte;
   AnimationOff                                : Boolean;
   Picture                                     : TFrames;
   Sma, Ecc, Inc, ArgNode, ArgPCenter, Period  : Real;
   xInit, yInit, zInit, xvInit, yvInit, zvInit : Real;
   ax, ay, az, bx, by, bz                      : Real;

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

PROCEDURE SetUpInitialVectors(Sma, Ecc, ArgNode, ArgPCenter, Inc: Real;
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

Procedure Project(ArgNode, ArgPcenter, Inc, xin, yin, zin: Real;
      VAR xout, yout, zout: Real);
VAR
   nr: Integer;
   th: Real;
BEGIN
   nr := 3; th := - ArgPcenter; RotateAxes(nr, th, xin, yin, zin,
   xout, yout, zout);
   xin := xout; yin := yout; zin := zout;
   nr := 1; th := - Inc; RotateAxes(nr, th, xin, yin, zin,
   xout, yout, zout);
   xin := xout; yin := yout; zin := zout;
   nr := 3; th := - ArgNode; RotateAxes(nr, th, xin, yin, zin,
   xout, yout, zout);
END;

BEGIN
   x := Sma*(1 - Ecc); xv := 0;
   y := 0; yv := Sqrt((1 + Ecc)/x);
   z := 0; zv := 0;
   Project(ArgNode, ArgPcenter, Inc, x, y, z, xInit, yInit, zInit);
   Project(ArgNode, ArgPcenter, Inc, xv, yv, zv, xvInit, yvInit, zvInit);
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
      Line(mapx(xold), mapy(yold),mapx(xnew),mapy(ynew));
      {PlotLine(xOld, yOld, xNew, yNew);}
      SetColor(Yellow);
      {PlotLine(xAppOld, yAppOld, xAppNew, yAppNew);}
      Line(mapx(xAppOld), mapy(yAppOld), mapx(xAppNew),mapy(yAppNew));
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
   Radius := Sma*(1 - Ecc*Ecc)/(1 + Ecc*Cos(ArgPCenter));
   View(Radius*Cos(ArgNode), Radius*Sin(ArgNode), 0, xNew, yNew);
   Radius := Sma*(1 - Ecc*Ecc)/(1 - Ecc*Cos(ArgPCenter));
   View(-Radius*Cos(ArgNode), -Radius*Sin(ArgNode), 0, xOld, yOld);
   PLotLine(xNew, yNew, xOld, yOld);
   Map(xNew, yNew, xSc, ySc);
   View(1.1*Cos(ArgNode), 1.1*Sin(ArgNode), 0, xNew, yNew);
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

PROCEDURE DrawArc(x, y, z, x1, y1, z1, x2, y2, z2: Real);
VAR
   Temp: Real;
   x3, y3, z3, x4, y4, z4, x5, y5, z5: Real;
   Theta, DelTheta: Real;
   xOld, yOld, xNew, yNew: Real;
   Fraction: Real;
BEGIN
   Fraction := 0.2;
   Temp := Sqrt(x1*x1 + y1*y1 + z1*z1);
   x1 := x1/Temp; y1 := y1/Temp; z1 := z1/Temp;
   Temp := Sqrt(x2*x2 + y2*y2 + z2*z2);
   x2 := x2/Temp; y2 := y2/Temp; z2 := z2/Temp;
   x3 := y1*z2 - y2*z1; y3 := z1*x2 - z2*x1; z3 := x1*y2 - x2*y1;
   Temp := Sqrt(x3*x3 + y3*y3 + z3*z3);
   x3 := x3/Temp; y3 := y3/Temp; z3 := z3/Temp;
   x4 := y3*z1 - y1*z3; y4 := z3*x1 - z1*x3; z4 := x3*y1 - x1*y3;
   Theta := 0;  DelTheta := 0.1;
   x5 := x + (x1*Cos(Theta) + x4*Sin(Theta))*Fraction;
   y5 := y + (y1*Cos(Theta) + y4*Sin(Theta))*Fraction;
   z5 := z + (z1*Cos(Theta) + z4*Sin(Theta))*Fraction;
   View(x5, y5, z5, xOld, yOld);
   REPEAT
      Theta := Theta + DelTheta;
      x5 := x + (x1*Cos(Theta) + x4*Sin(Theta))*Fraction;
      y5 := y + (y1*Cos(Theta) + y4*Sin(Theta))*Fraction;
      z5 := z + (z1*Cos(Theta) + z4*Sin(Theta))*Fraction;
      View(x5, y5, z5, xNew, yNew);
      {PlotLine(xOld, yOld, xNew, yNew); }
      Line(mapx(xOld), mapy(yOld), mapx(xNew), mapy(yNew));
      xOld := xNew; yOld := yNew;
   UNTIL Temp*Cos(Theta) - (x1*x2+y1*y2+z2*z2)*Sin(Theta) < 0;
END;


PROCEDURE ExplainGeometry;
VAR
   ViewX, ViewY: Real;
   xOld, yOld, xNew, yNew: Real;
   xv, yv, zv: Real;
   xSc, ySc: Integer;


BEGIN
   OpenViewPort(5);
   SetColor(White);
   View(0, 0, 0, ViewX, ViewY);
   PlotSymbol(ViewX, ViewY, '*');
   Map(ViewX, ViewY+0.2, xSc, ySc);
   OutTextxy(xSc, ySc, 'B');
   Map(-1.8, 1.8, xSc, ySc);
   SetColor(Yellow);
   OutTextxy(xSc, ySc, 'Location of the secondary star, B');
   Print(5,13, 'Click to progress');
   StaticPause; 
   OpenViewPort(5);
   SetColor(White);   
   View(0, 0, 0, ViewX, ViewY);
   PlotSymbol(ViewX, ViewY, '*'); 
   Map(ViewX, ViewY+0.2, xSc, ySc);
   OutTextxy(xSc, ySc, 'B');
   Arrow(0, 0, 0, 0, 0, -1.4, True);
   View(0, 0, -1.6, xNew, yNew);
   Map(xNew, yNew, xSc, ySc);
   OutTextxy(xSc, ySc, 'Observer'); 
   Map(-1.8, 1.8, xSc, ySc);
   SetColor(Yellow);
   OutTextxy(xSc, ySc, 'Direction to the observer');
   Print(5,13, 'Click to progress');
   StaticPause;
   {Picture.SwitchAndClean;}
   OpenViewPort(5);
   SetColor(White);
   View(0, 0, 0, ViewX, ViewY);
   PlotSymbol(ViewX, ViewY, '*');
   Arrow(0, 0, 0, 0, 0, -1.4, True);
   View(0, 0, -1.6, xNew, yNew);
   Map(xNew, yNew, xSc, ySc);
   OutTextxy(xSc, ySc, 'Observer');
   SetColor(Green);
   View(1, 1, 0, xOld, yOld);
   View(-1, 1, 0, xNew, yNew);
   PlotLine(xOld, yOld, xNew, yNew);
   View(-1, -1, 0, xOld, yOld);
   PlotLine(xOld, yOld, xNew, yNew);
   View(1, -1, 0, xNew, yNew);
   PlotLine(xOld, yOld, xNew, yNew);
   View(1, 1, 0, xOld, yOld);
   PlotLine(xOld, yOld, xNew, yNew);
   Map(-1.8, 1.8, xSc, ySc);
   SetColor(Yellow);
   OutTextxy(xSc, ySc, 'Square in the plane of the sky');
   Map(-1.8, 1.6, xSc, ySc);
   OutTextxy(xSc, ySc, 'The rectangular shape is caused by the angle of viewing');
   Print(5,13, 'Click to progress');
   StaticPause;
   OpenViewPort(5);
   SetColor(White);
   View(0, 0, 0, ViewX, ViewY);
   PlotSymbol(ViewX, ViewY, '*');
   Arrow(0, 0, 0, 0, 0, -1.4, True);
   View(0, 0, -1.6, xNew, yNew);
   Map(xNew, yNew, xSc, ySc);
   OutTextxy(xSc, ySc, 'Observer');
   SetColor(Green);
   View(1, 1, 0, xOld, yOld);
   View(-1, 1, 0, xNew, yNew);
   PlotLine(xOld, yOld, xNew, yNew);
   View(-1, -1, 0, xOld, yOld);
   PlotLine(xOld, yOld, xNew, yNew);
   View(1, -1, 0, xNew, yNew);
   PlotLine(xOld, yOld, xNew, yNew);
   View(1, 1, 0, xOld, yOld);
   PlotLine(xOld, yOld, xNew, yNew);
   SetColor(White);
   Arrow(0, 0, 0, 1.4, 0, 0, True);
   View(1.6, 0, 0, xNew, yNew);
   Map(xNew, yNew, xSc, ySc);
   OutTextxy(xSc, ySc, 'North');
   Map(-1.8, 1.8, xSc, ySc);
   SetColor(Yellow);
   OutTextxy(xSc, ySc, 'North, in the plane of the sky, as seen by the observer');
   Print(5,13, 'Click to progress');
   StaticPause;
   OpenViewPort(5);
   SetColor(White);
   View(0, 0, 0, ViewX, ViewY);
   PlotSymbol(ViewX, ViewY, '*');
   Arrow(0, 0, 0, 0, 0, -1.4, True);
   View(0, 0, -1.6, xNew, yNew);
   Map(xNew, yNew, xSc, ySc);
   OutTextxy(xSc, ySc, 'Observer');
   SetColor(Green);
   View(1, 1, 0, xOld, yOld);
   View(-1, 1, 0, xNew, yNew);
   PlotLine(xOld, yOld, xNew, yNew);
   View(-1, -1, 0, xOld, yOld);
   PlotLine(xOld, yOld, xNew, yNew);
   View(1, -1, 0, xNew, yNew);
   PlotLine(xOld, yOld, xNew, yNew);
   View(1, 1, 0, xOld, yOld);
   PlotLine(xOld, yOld, xNew, yNew);
   SetColor(White);
   Arrow(0, 0, 0, 1.4, 0, 0, True);
   View(1.6, 0, 0, xNew, yNew);
   Map(xNew, yNew, xSc, ySc);
   OutTextxy(xSc, ySc, 'North');
   Radius := Sma*(1 - Ecc*Ecc)/(1 + Ecc*Cos(ArgPCenter));
   View(Radius*Cos(ArgNode), Radius*Sin(ArgNode), 0, xNew, yNew);
   Radius := Sma*(1 - Ecc*Ecc)/(1 - Ecc*Cos(ArgPCenter));
   View(-Radius*Cos(ArgNode), -Radius*Sin(ArgNode), 0, xOld, yOld);
   PLotLine(xNew, yNew, xOld, yOld);
   DrawArc(0, 0, 0, 1, 0, 0, Cos(ArgNode), Sin(ArgNode), 0);
   Map(xNew, yNew, xSc, ySc);
   View(1.1*Cos(ArgNode), 1.1*Sin(ArgNode), 0, xNew, yNew);
   OutTextxy(xSc, ySc, 'Node');
   SetColor(Yellow);
   Map(-1.8, 1.8, xSc, ySc);
   OutTextxy(xSc, ySc, 'The line of nodes; The longitude of the node is 60 degrees');
   Map(-1.8, 1.6, xSc, ySc);
   OutTextxy(xSc, ySc, 'No distinction can be made between ascending and descending');
   Map(-1.8, 1.4, xSc, ySc);
   OutTextxy(xSc, ySc, 'nodes; The longitude of the node lies between 0 and 180');
   Map(-1.8, 1.2, xSc, ySc);
   OutTextxy(xSc, ySc, 'degrees. It is measured clockwise from the North, seen');
   Map(-1.8, 1.0, xSc, ySc);
   OutTextxy(xSc, ySc, 'by the observer');
   Print(5,13, 'Click to progress');
   StaticPause;
   OpenViewPort(5);
   SetColor(White);
   View(0, 0, 0, ViewX, ViewY);
   PlotSymbol(ViewX, ViewY, '*');
   Arrow(0, 0, 0, 0, 0, -1.4, True);
   View(0, 0, -1.6, xNew, yNew);
   Map(xNew, yNew, xSc, ySc);
   OutTextxy(xSc, ySc, 'Observer');
   SetColor(White);
   Arrow(0, 0, 0, 1.4, 0, 0, True);
   View(1.6, 0, 0, xNew, yNew);
   Map(xNew, yNew, xSc, ySc);
   OutTextxy(xSc, ySc, 'North');
   Radius := Sma*(1 - Ecc*Ecc)/(1 + Ecc*Cos(ArgPCenter));
   View(Radius*Cos(ArgNode), Radius*Sin(ArgNode), 0, xNew, yNew);
   Radius := Sma*(1 - Ecc*Ecc)/(1 - Ecc*Cos(ArgPCenter));
   View(-Radius*Cos(ArgNode), -Radius*Sin(ArgNode), 0, xOld, yOld);
   PLotLine(xNew, yNew, xOld, yOld);
   DrawArc(0, 0, 0, 1, 0, 0, Cos(ArgNode), Sin(ArgNode), 0);
   Map(xNew, yNew, xSc, ySc);
   View(1.1*Cos(ArgNode), 1.1*Sin(ArgNode), 0, xNew, yNew);
   OutTextxy(xSc, ySc, 'Node');
   DrawOrbits(ax, ay, az, bx, by, bz);
   SetColor(Yellow);
   Map(-1.8, 1.8, xSc, ySc);
   OutTextxy(xSc, ySc, 'The true (blue) and apparent (yellow) orbits');
   Map(-1.8, 1.6, xSc, ySc);
   OutTextxy(xSc, ySc, 'The apparent orbit is in the plane of the sky');
   Map(-1.8, 1.4, xSc, ySc);
   OutTextxy(xSc, ySc, 'The eccentricity of the true orbit is 0.5');
   Print(5,13, 'Click to progress');
   StaticPause;
   OpenViewPort(5);
   SetColor(White);
   View(0, 0, 0, ViewX, ViewY);
   PlotSymbol(ViewX, ViewY, '*');
   Arrow(0, 0, 0, 0, 0, -1.4, True);
   View(0, 0, -1.6, xNew, yNew);
   Map(xNew, yNew, xSc, ySc);
   OutTextxy(xSc, ySc, 'Observer');
   SetColor(White);
   Arrow(0, 0, 0, 1.4, 0, 0, True);
   View(1.6, 0, 0, xNew, yNew);
   Map(xNew, yNew, xSc, ySc);
   OutTextxy(xSc, ySc, 'North');
   Radius := Sma*(1 - Ecc*Ecc)/(1 - Ecc*Cos(ArgPCenter));
   View(-Radius*Cos(ArgNode), -Radius*Sin(ArgNode), 0, xOld, yOld);
   Radius := Sma*(1 - Ecc*Ecc)/(1 + Ecc*Cos(ArgPCenter));
   View(Radius*Cos(ArgNode), Radius*Sin(ArgNode), 0, xNew, yNew);
   PLotLine(xNew, yNew, xOld, yOld);
   DrawArc(0, 0, 0, 1, 0, 0, Cos(ArgNode), Sin(ArgNode), 0);
   Map(xNew, yNew, xSc, ySc);
   View(1.1*Cos(ArgNode), 1.1*Sin(ArgNode), 0, xNew, yNew);
   OutTextxy(xSc, ySc, 'Node');
   DrawOrbits(ax, ay, az, bx, by, bz);
   xv := - Sin(ArgNode)*(ax*by - ay*bx);
   yv := Cos(ArgNode)*(ax*by - ay*bx);
   zv := Sin(ArgNode)*(ay*bz - az*by) - Cos(ArgNode)*(az*bx - ax*bz);
   SetColor(Yellow);
   DrawArc(Radius*Cos(ArgNode), Radius*Sin(ArgNode), 0,
         - Sin(ArgNode), Cos(ArgNode), 0, xv, yv, zv);
   SetColor(Yellow);
   Map(-1.8, 1.8, xSc, ySc);
   OutTextxy(xSc, ySc, 'The inclination is 45 degrees');
   Print(5,13, 'Click to progress');
   StaticPause;
   OpenViewPort(5);
   SetColor(White);
   View(0, 0, 0, ViewX, ViewY);
   PlotSymbol(ViewX, ViewY, '*');
   Arrow(0, 0, 0, 0, 0, -1.4, True);
   View(0, 0, -1.6, xNew, yNew);
   Map(xNew, yNew, xSc, ySc);
   OutTextxy(xSc, ySc, 'Observer');
   SetColor(White);
   Arrow(0, 0, 0, 1.4, 0, 0, True);
   View(1.6, 0, 0, xNew, yNew);
   Map(xNew, yNew, xSc, ySc);
   OutTextxy(xSc, ySc, 'North');
   Radius := Sma*(1 - Ecc*Ecc)/(1 - Ecc*Cos(ArgPCenter));
   View(-Radius*Cos(ArgNode), -Radius*Sin(ArgNode), 0, xOld, yOld);
   Radius := Sma*(1 - Ecc*Ecc)/(1 + Ecc*Cos(ArgPCenter));
   View(Radius*Cos(ArgNode), Radius*Sin(ArgNode), 0, xNew, yNew);
   PLotLine(xNew, yNew, xOld, yOld);
   DrawArc(0, 0, 0, 1, 0, 0, Cos(ArgNode), Sin(ArgNode), 0);
   Map(xNew, yNew, xSc, ySc);
   View(1.1*Cos(ArgNode), 1.1*Sin(ArgNode), 0, xNew, yNew);
   OutTextxy(xSc, ySc, 'Node');
   DrawOrbits(ax, ay, az, bx, by, bz);
   xv := - Sin(ArgNode)*(ax*by - ay*bx);
   yv := Cos(ArgNode)*(ax*by - ay*bx);
   zv := Sin(ArgNode)*(ay*bz - az*by) - Cos(ArgNode)*(az*bx - ax*bz);
   SetColor(White);
   DrawArc(Radius*Cos(ArgNode), Radius*Sin(ArgNode), 0,
         - Sin(ArgNode), Cos(ArgNode), 0, xv, yv, zv);
   SetColor(LightBlue);
   View(1.6*ax, 1.6*ay, 1.6*az, xOld, yOld);
   Arrow(0, 0, 0, 1.5*ax, 1.5*ay, 1.5*az, True);
   Map(xOld, yOld, xSc, ySc);
   OutTextxy(xSc, ySc, 'PeriAstron');
   SetColor(Green);
   DrawArc(0, 0, 0, Cos(ArgNode), Sin(ArgNode), 0,
                    ax, ay, az);
   SetColor(Yellow);
   Map(-1.8, 1.8, xSc, ySc);
   OutTextxy(xSc, ySc, 'The argument of periastron is 60 degrees');
   Print(5,13, 'Click to progress');
   StaticPause;
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
      Delay(DemLag);
      PlotFigure(t);
      Picture.SwitchAndClean;
   UNTIL AnimationOff;
END;


BEGIN
   CloseGraph;
   CUPSdone;
   Picture.Init;
   CUPSInit;
   DemLag := 0;
   t := 0;
   Dt := 0.1;
   Sma := 1;
   Ecc := 0.5;
   ArgNode := 3.14159/3;
   ArgPCenter := 3.14159/3;
   Inc := 3.14159/4;
   Period := 2*3.14159*Sma*Sqrt(Sma);
   Radius := 5;
   Azimuth := 0;
   Altitude := -3*Pi/8;
   TrigFunctions;
   SetUpInitialVectors(Sma, Ecc, ArgNode, ArgPCenter, Inc,
                       xInit, yInit, zInit, xvInit, yvInit, zvInit,
                       ax, ay, az, bx, by, bz);
   PrepareGraph;
   ExplainGeometry;
   AnimateMotion;
   Picture.Close;
   CloseGraph;
   CUPSdone;;
   GraphDriver := Detect;
   CUPSInit;
   VBMenu.Display;
   Action := False;
   LightMenu := True;
   ShowCUPSKeys := True;
   CUPSKeys.Display;
END;

{-------------------------------------------------------------------------}

PROCEDURE PromptChoice;
var ViewPort : ViewPortType;
    x1, y1, x2,y2,midx,midy : integer;
begin
   HideCursor;
   GetViewSettings (ViewPort);
   with ViewPort do begin midx := (x1+x2) div 2; midy := (y1+y2) div 2; end;
   x1 := midx-190; y1 := midy-100; x2 := midx+190; y2 := midy;
   SetTextJustify(LeftText, TopText);
   SetFillStyle(SolidFill,blue);
   SetColor(Yellow);
   Bar3D(x1,y1+5,x2,y2+5,5,true);  
   OutTextXY(x1+20,y1+20,' Make a selection from the menu item Plot:');
   OutTextXY(x1+20,y1+40,'        See animated proper motion');
   OutTextXY(x1+20,y1+60,'                   or');
   OutTextXY(x1+20,y1+80,'       See animated relative motion');
   ShowCursor;
end;  


{-------------------------------------------------------------------------}

PROCEDURE GetData;
BEGIN
   VBMenu.Display;
      VBData(MassRat, Ecc, ArgNode, ArgPcenter, Inc, Speed);
   IF NOT InputAccepted THEN
   BEGIN
      VBMenu.RowActivate(4,1, False);
      VBMenu.RowActivate(4,2, False);
   END
   ELSE
   BEGIN
      VBMenu.RowActivate(4,1, True);
      VBMenu.RowActivate(4,2, True);
   END;
   Action := False;
   PlotOrbit := False;
   IF InputAccepted THEN PromptChoice;
   LightMenu := True;
   ShowCUPSKeys := True;
   CUPSKeys.Display;
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
                  ShowHelp('HelpBins.HLP', 'Visual1 Hello');
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
                  ShowHelp('HelpBins.HLP', 'Visual1 General');
                  LightMenu := True;
               END;
            2: BEGIN
                  ShowHelp('HelpBins.HLP', 'Visual1 Params1');
                  ShowHelp('HelpBins.HLP', 'Visual1 Params2');
                  LightMenu := True;
               END;
            3: ThreeDDemo;
         END;
      3: BEGIN
            ClearMuppetPort;
            GetData;
         END;
      4: CASE RowChosen OF
            1: BEGIN
                  RelativeOrbit := False;
                  SetUpOrbits(MassRat, Ecc, ArgNode, ArgPcenter, Inc, Speed);
               END;
            2: BEGIN
                  RelativeOrbit := True;
                  SetUpRelativeOrbit(MassRat, Ecc, ArgNode, ArgPcenter, Inc, Speed);
               END;
         END;
    END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleCUPSKeys(Key: Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HelpBins.HLP', 'Visual1 Hello');
      2: IF VBMenu.Chosen THEN HandleVBMenu;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE OpeningView;

Procedure PlotOrbits0(MassRat, Ecc, ArgNode, ArgPcenter, Inc, Speed: Real);
VAR
   Axo, Ayo, Bxo, Byo, Cxo, Cyo: Real;
   Axn, Ayn, Bxn, Byn, Cxn, Cyn: Real;
   x, y, z, vx, vy: Real;
   xt, yt, zt: Real;
   t: Real;
   nr: Integer;
   th: Real;

BEGIN
   GraphBackColor := Black;
   OpenViewPort(1);
   SelectScale(1);
   SetColor(LightGreen);
   OutTextxy(10, 10, 'PROPER MOTION OF A VISUAL BINARY');
   SetColor(Yellow);
   OutTextxy(10, 30, 'Motion of star A');
   SetColor(Green);
   OutTextxy(10, 50, 'Motion of star B');
   SetColor(White);
   OutTextxy(10, 70, 'Motion of the center of mass');
   x := (1 - Ecc); vx := 0;

   y := 0; vy := Sqrt((1 + Ecc)/(1 - Ecc));
   z := 0;
   t := 0;
   Project(ArgNode, ArgPcenter, Inc, x, y, z, xt, yt, zt);
{Initial coordinates of the center of mass and the stars.}
   Cxo := 0.5; Cyo := 0;
   IF Abs(Speed) < 0.00001 THEN Cxo := 5;
   Axo := Cxo + (1 - MassRat)*xt; Ayo := (1 - MassRat)*yt;
   Bxo := Cxo - MassRat*xt; Byo := - MassRat*yt;
   REPEAT
      Ivp(x, y, vx, vy, x, y, vx, vy);
      z := 0;
      Project(ArgNode, ArgPcenter, Inc, x, y, z, xt, yt, zt);
      Cxn := Cxo + dt*Speed; Cyn := 0;
      Axn := Cxn + (1 - MassRat)*xt; 
      Ayn := Cyn + (1 - MassRat)*yt;
      Bxn := Cxn - MassRat*xt; 
      Byn := Cyn - MassRat*yt;
      SetColor(Green); PlotLine(Axo, Ayo, Axn, Ayn);
      SetColor(Yellow); PlotLine(Bxo, Byo, Bxn, Byn);
      SetColor(White); PlotLine(Cxo, Cyo, Cxn, Cyn);
      Axo := Axn; Ayo := Ayn;
      Bxo := Bxn; Byo := Byn;
      Cxo := Cxn; Cyo := Cyn;
      Delay(Lag);
   UNTIL  (axn + bxn > 20);
   ShowCursor;
END;

BEGIN
   Lag := 50;
   PlotOrbits0(MassRat, Ecc, ArgNode, ArgPcenter, Inc, Speed);
   Lag := 200;
END;

{------------------------------------------------------------------------}

PROCEDURE SetDefaultParameters;
BEGIN
   MassRat := 0.7;
   Ecc := 0.5;
   ArgNodedeg :=  90;
   ArgNode := ArgNodedeg*3.14159/180;
   ArgPcenterdeg :=  45;
   ArgPcenter := ArgPcenterdeg*3.14159/180;
   Incdeg := 45;
   Inc := Incdeg*3.14159/180;
   Speed := 1;
   Lag := 100;
   DataIn := False;
   DefineViewPort(1, 0.02, 0.98, 0.07, 0.93);
   DefineScale(1, 0, 10, -3, 3);
   SetBkColor(Black);
   LightMenu := False;
   ExitRequest := False;
   SetUpCUPSKeys;
   ShowCUPSKeys := False;
END;

{------------------------------------------------------------------------}

BEGIN  {Main Program.}
{  CUPSinit;}
   SetDefaultParameters; 
   OpeningView; {Give demonstration.}
   ShowCUPSKeys := True;
   CUPSKeys.Display;
   SetUpVBMenu;
   ShowHelp('HelpBins.HLP', 'Visual1 Hello');
   GetData; {Gets input data.}
   REPEAT
      CheckForEvents;
      IF ShowCUPSKeys THEN
              IF CUPSKeys.Pressed(Key) THEN HandleCUPSKeys(Key);
      IF HotKeys.Pressed(Key) THEN
         HandleHotKeys(key);
      IF (Action) AND (NOT LightMenu) THEN
      BEGIN
         IF RelativeOrbit THEN
            PlotRelativeOrbit(MassRat, Ecc, ArgNode, ArgPcenter, Inc, Speed)
         ELSE
            PlotOrbits(MassRat, Ecc, ArgNode, ArgPcenter, Inc, Speed);
      END;
      IF LightMenu THEN
      BEGIN
         LightMenu := False;
         IF VBMenu.Chosen THEN HandleVBMenu;
      END
      ELSE
         IF VBMenu.Activated THEN HandleVBMenu;
   UNTIL ExitRequest;
   VBMenu.Done;
{  CUPSdone; }
END;

END.
