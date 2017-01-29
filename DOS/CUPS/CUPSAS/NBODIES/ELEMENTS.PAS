    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1994.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program to demonstrate the geometrical        *)
    (*       elements of a planetary orbit, and to         *)
    (*       animate the orbital motion.                   *)
    (*                                                     *)
    (*******************************************************)

unit Elements;
{$M 64000,0,327680}
{$O+}
{$F+}
interface
procedure RunElements;

implementation
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;

TYPE
   TGeometry = object
      AnimationOff: Boolean;
      TwistAngle, Altitude: Real;
      PROCEDURE init;
      function  ReadElementData : boolean;
      PROCEDURE AnimateOrbit;
  private
      Radius, Azimuth : Real;
      t, Dt: Real;
      Caz, Saz, Calt, Salt, CTwist, STwist: Real;
      Sma, Ecc, Inc, LongNode, ArgPCenter, Period: Real;
      SmaInput: Real; {In the program, the semimajor axis is one unit.}
      IncDeg, LongNodeDeg, ArgPCenterDeg: Real;
      xInit, yInit, zInit, xvInit, yvInit, zvInit: Real;
      ax, ay, az, bx, by, bz: Real;
      PROCEDURE SetUpDefaultData;
      PROCEDURE SetUpFigure;
      PROCEDURE SetUpInitialVectors;
      PROCEDURE TrigFunctions;
      PROCEDURE View(x, y, z: Real; VAR XView, YView: Real);
      PROCEDURE Arrowx( x0, y0, z0, x1, y1, z1 : REAL);
      PROCEDURE Arrowy( x0, y0, z0, x1, y1, z1 : REAL);
      PROCEDURE Arrowz( x0, y0, z0, x1, y1, z1 : REAL);
      PROCEDURE DrawSun;
      PROCEDURE DrawNorth;
      PROCEDURE DrawEcliptic;
      PROCEDURE DrawVernalEquinox;
      PROCEDURE DrawNodes;
      PROCEDURE DrawOrbitPlane;
      PROCEDURE DrawPerihelion;
      PROCEDURE DrawOrbit;
      PROCEDURE PlotOrbitPoint;
      PROCEDURE DrawFigure;
  end;

VAR
   ShowCUPSKeys: Boolean;
   HotKeys     : THotKeys;
   CUPSKeys    : THotKeys;
   Picture     : TFrames;

PROCEDURE TGeometry.init;
BEGIN
   TwistAngle := 0;
   Azimuth    := Pi/6;
   Altitude   := Pi/6;
   Sma        := 1;
   Period     := 2*Pi*Sma*Sqrt(Sma);
END;

{------------------------------------------------------------------------}

PROCEDURE TGeometry.SetUpDefaultData;
BEGIN
   SmaInput := 1;
   Ecc := 0.5;
   IncDeg := 45;
   LongNodeDeg := 60;
   ArgPCenterDeg := 60;
END;

{------------------------------------------------------------------------}

function TGeometry.ReadElementData : boolean;
Var
   ElementDat: TInputScreen;
BEGIN
   WITH ElementDat DO
   BEGIN
      Init;
      DefineInputPort(0.05, 0.95, 0.07, 0.9);
      LoadLine('                  THE ELEMENTS OF A PLANETARY ORBIT');
      LoadLine('  Only elliptic orbits are considered here.');
      LoadLine('  The SEMIMAJOR AXIS (positive)          = {      } astronomical units');
      LoadLine('  The ECCENTRICITY must be positive and less than one. It is zero');
      LoadLine('  for a circular orbit. The eccentricity = {      }');
      LoadLine('  The INCLINATION, i,  lies between 0 and 90 degrees for a DIRECT');
      LoadLine('  orbit, and between 90 < i 180, for a RETROGRADE orbit.');
      LoadLine('                                       i = {      } degrees');
      LoadLine('  The LONGITUDE OF THE ASCENDING NODE lies between 0 and 360 degrees');
      LoadLine('         Longitude of the ascending node = {      } degrees');
      LoadLine('  The ARGUMENT OF PERICENTER lies between 0 and 360 degrees');
      LoadLine('                  Argement of pericenter = {      } degrees');
      LoadLine('');
      LoadLine('                [  OK  ]                  [Cancel]');
      SetNumber(1, SmaInput);
      SetNumberLimits(1, 0.000001, 10000);
      SetNumber(2, Ecc);
      SetNumberLimits(2, 0, 0.99999);
      SetNumber(3, IncDeg);
      SetNumberLimits(3, 0, 179.999);
      SetNumber(4, LongNodeDeg);
      SetNumberLimits(4, 0, 359.999);
      SetNumber(5, ArgPCenterDeg);
      SetNumberLimits(5, 0, 359.999);
      Accept;
      SmaInput := GetNumber(1);
      Sma := 1;
      Ecc := GetNumber(2);
      IncDeg := GetNumber(3);
      Inc := Pi*IncDeg/180;
      LongNodeDeg := GetNumber(4);
      LongNode := Pi*LongNodeDeg/180;
      ArgPcenterDeg := GetNumber(5);
      ArgPcenter := Pi*ArgPcenterDeg/180;
      ReadElementData := not canceled;
      If not Canceled then
      begin
         TrigFunctions;
         SetUpInitialVectors;
         CUPSKeys.Clear;
         ShowCUPSKeys := False;
         SetUpFigure;
      end;
      ClearInputPort;
      Done;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE TGeometry.TrigFunctions;
BEGIN
   Calt := Cos(Altitude);
   Salt := Sin(Altitude);
   Caz := Cos(Azimuth);
   Saz := Sin(Azimuth);
   CTwist := Cos(TwistAngle);
   STwist := Sin(TwistAngle);
END;

{------------------------------------------------------------------------}

PROCEDURE TGeometry.View(x, y, z: Real; VAR XView, YView: Real);
VAR
   Spare: Real;
BEGIN
   XView := (y*Caz - x*Saz);
   YView := (z*Calt - (x*Caz + y*Saz)*Salt);
   Spare := XView*CTwist - YView*STwist;
   YView := XView*STwist + YView*CTwist;
   XView := Spare;
END;

{------------------------------------------------------------------------}


PROCEDURE TGeometry.Arrowx( x0, y0, z0, x1, y1, z1 : REAL);

CONST   BarbSize = 0.1;

VAR
   xorg, yorg   : REAL ;
   xend, yend   : REAL ;
   ybarb,zbarb  : REAL ;

BEGIN
   View(x0,y0,z0,xorg,yorg);   { Plot line from origin to terminus.}
   View(x1,y1,z1,xend,yend);
   PlotLine(xorg,yorg,xend,yend);
      ybarb := y1 + BarbSize*(-y1+y0+(z1-z0)/2);  {Plot barb lines.}
      zbarb := z1 + BarbSize*(-z1+z0-(y1-y0)/2);
      xorg := xend ; yorg := yend ;
      View(x1,ybarb,zbarb,xend,yend);
      Plotline(xorg,yorg,xend,yend);
      zbarb := z1 + BarbSize*(-z1+z0+(y1-y0)/2);
      ybarb := y1 + BarbSize*(-y1+y0-(z1-z0)/2);
      View(x1,ybarb,zbarb,xend,yend);
      Plotline(xorg,yorg,xend,yend);
END; {Arrow}

{------------------------------------------------------------------------}

PROCEDURE TGeometry.Arrowy( x0, y0, z0, x1, y1, z1 : REAL);

CONST   BarbSize = 0.1;

VAR
   xorg, yorg   : REAL ;
   xend, yend   : REAL ;
   xbarb,zbarb  : REAL ;

BEGIN
   View(x0,y0,z0,xorg,yorg);   { Plot line from origin to terminus.}
   View(x1,y1,z1,xend,yend);
   PlotLine(xorg,yorg,xend,yend);
      xbarb := x1 + BarbSize*(-x1+x0+(z1-z0)/2);  {Plot barb lines.}
      zbarb := z1 + BarbSize*(-z1+z0-(x1-x0)/2);
      xorg := xend ; yorg := yend ;
      View(xbarb,y1,zbarb,xend,yend);
      Plotline(xorg,yorg,xend,yend);
      zbarb := z1 + BarbSize*(-z1+z0+(x1-x0)/2);
      xbarb := x1 + BarbSize*(-x1+x0-(z1-z0)/2);
      View(xbarb,y1,zbarb,xend,yend);
      Plotline(xorg,yorg,xend,yend);
END; {Arrow}

{------------------------------------------------------------------------}

PROCEDURE TGeometry.Arrowz( x0, y0, z0, x1, y1, z1 : REAL);

CONST   BarbSize = 0.1;

VAR
   xorg, yorg   : REAL ;
   xend, yend   : REAL ;
   xbarb,ybarb  : REAL ;

BEGIN
   View(x0,y0,z0,xorg,yorg);   { Plot line from origin to terminus.}
   View(x1,y1,z1,xend,yend);
   PlotLine(xorg,yorg,xend,yend);
      xbarb := x1 + BarbSize*(-x1+x0+(y1-y0)/2);  {Plot barb lines.}
      ybarb := y1 + BarbSize*(-y1+y0-(x1-x0)/2);
      xorg := xend ; yorg := yend ;
      View(xbarb,ybarb,z1,xend,yend);
      Plotline(xorg,yorg,xend,yend);
      ybarb := y1 + BarbSize*(-y1+y0+(x1-x0)/2);
      xbarb := x1 + BarbSize*(-x1+x0-(y1-y0)/2);
      View(xbarb,ybarb,z1,xend,yend);
      Plotline(xorg,yorg,xend,yend);
END; {Arrow}


{------------------------------------------------------------------------}

PROCEDURE TGeometry.DrawSun;
VAR
   ViewX, ViewY: Real;
BEGIN
   SelectViewPort(1);
   SetColor(Yellow);
   View(0, 0, 0, ViewX, ViewY);
   PlotSymbol(ViewX, ViewY, '*');
END;

{------------------------------------------------------------------------}

PROCEDURE TGeometry.DrawNorth;
VAR
   xSc, ySc: Integer;
   xView, yView: Real;
BEGIN
   SelectViewPort(1);
   SetColor(Cyan);
   Arrowx(0, 0, 0, 0, 0, 1.8);
   View(0, 0, 2, xView, yView);
   Map(xView, yView, xSc, ySc);
   OutTextxy(xSc, ySc, 'N');
END;

{------------------------------------------------------------------------}

PROCEDURE TGeometry.DrawEcliptic;
VAR
   i: Integer;
   Theta, dTheta: Real;
   xOld, yOld, xNew, yNew: Real;
   CTheta, STheta, CdTheta, SdTheta, Spare: Real;
BEGIN
    SelectViewPort(1);
    dTheta := Pi/20;;
    CTheta := 1; STheta := 0;
    CdTheta := Cos(dTheta); SdTheta := Sin(dTheta);
    View(CTheta, STheta, 0, xOld, yOld);
    FOR i := 1 TO 40 DO
    BEGIN
       Spare := CTheta*CdTheta - STheta*SdTheta;
       STheta := STheta*CdTheta + CTheta*SdTheta;
       CTheta := Spare;
       View(CTheta, STheta, 0, xNew, yNew);
       PlotLine(xOld, yOld, xNew, yNew);
       xOld := xNew; yOld := yNew;
    END;
END;

{------------------------------------------------------------------------}

PROCEDURE TGeometry.DrawVernalEquinox;
VAR
   xSc, ySc: Integer;
   xView, yView: Real;

   PROCEDURE DrawSign(x, y: Integer);
   BEGIN
      Ellipse(x-5, y+5, 0, 110, 5, 10);
      Ellipse(x+5, y+5, 70, 180, 5, 10);
   END;


BEGIN
   SelectViewPort(1);
   Arrowz(0, 0, 0, 1.8, 0, 0);
   View(2, 0, 0, xView, yView);
   Map(xView, yView, xSc, ySc);
   DrawSign(xSc, ySc);
END;

{------------------------------------------------------------------------}


PROCEDURE TGeometry.DrawNodes;
VAR
   x1, y1, x2, y2: Real;
   ArcSize, Theta, dTheta: Real;
   i, NArcs: Integer;
   xSc, ySc: Integer;
BEGIN
   SelectViewPort(1);
   ArcSize := 0.2;
   View(Cos(LongNode), Sin(LongNode), 0, x1, y1);
   View(-Cos(LongNode), -Sin(LongNode), 0, x2, y2);
   PLotLine(x1, y1, x2, y2);
   View(1.1*Cos(LongNode), 1.1*Sin(LongNode), 0, x1, y1);
   Map(x1, y1, xSc, ySc);
   OutTextxy(xSc, ySc, 'Node');
   NArcs := Trunc(5*LongNode/Pi) + 2;
   dTheta := LongNode/NArcs;
   Theta := 0;
   View(ArcSize*Cos(Theta), ArcSize*Sin(Theta), 0, x1, y1);
   FOR i := 1 TO NArcs DO
   BEGIN
      Theta := Theta + dTheta;
      View(ArcSize*Cos(Theta), ArcSize*Sin(Theta), 0, x2, y2);
      PlotLine(x1, y1, x2, y2);
      x1 := x2; y1 := y2;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE TGeometry.DrawOrbitPlane;
VAR
   i: Integer;
   Theta, dTheta: Real;
   xOld, yOld, xNew, yNew: Real;
   CTheta, STheta, CdTheta, SdTheta, Spare: Real;
   a11, a12, a13, a21, a22, a23, a31, a32, a33: Real;
   x, y, z: Real;
   ArcSize, CNode, SNode, CA, SA: Real;
   NArc: Integer;
BEGIN
   SelectViewPort(1);
   CNode := Cos(LongNode);
   SNode := Sin(LongNode);
   a11 := Cos(LongNode);
   a12 := - SNode*Cos(Inc);
   a13 := SNode*Sin(Inc);
   a21 := SNode;
   a22 := CNode*Cos(Inc);
   a23 := - Cos(LongNode)*Sin(Inc);
   a31 := 0;
   a32 := Sin(Inc);
   a33 := Cos(Inc);
   dTheta := Pi/20;;
    CTheta := 1; STheta := 0;
    CdTheta := Cos(dTheta); SdTheta := Sin(dTheta);
    x := a11*CTheta + a12*STheta;
    y := a21*CTheta + a22*STheta;
    z := a31*CTheta + a32*STheta;
    View(x, y, z, xOld, yOld);
    FOR i := 1 TO 40 DO
    BEGIN
       Spare := CTheta*CdTheta - STheta*SdTheta;
       STheta := STheta*CdTheta + CTheta*SdTheta;
       CTheta := Spare;
       x := a11*CTheta + a12*STheta;
       y := a21*CTheta + a22*STheta;
       z := a31*CTheta + a32*STheta;
       View(x, y, z, xNew, yNew);
       PlotLine(xOld, yOld, xNew, yNew);
       xOld := xNew; yOld := yNew;
    END;
    {Plot the angle arc.}
    ArcSize := 0.2;
    CA := Cos(ArcSize);
    SA := Sin(ArcSize);
    NArc := Trunc(5*Inc/Pi) + 2;
    dTheta := Inc/NArc;
    Theta := 0;
    x := CA*CNode - SA*SNode;
    y := CA*SNode + SA*CNode;
    z := 0;
    View(x, y, z, xOld, yOld);
    FOR i := 1 TO NArc DO
    BEGIN
       Theta := Theta + dTheta;
       x := CA*CNode - SA*SNode*Cos(Theta);
       y := CA*SNode + SA*CNode*Cos(Theta);
       z := SA*Sin(Theta);
       View(x, y, z, xNew, yNew);
       PlotLine(xOld, yOld, xNew, yNew);
       xOld := xNew; yOld := yNew;
    END;
END;

{------------------------------------------------------------------------}

PROCEDURE TGeometry.SetUpInitialVectors;
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

Procedure Project( xin,  yin,  zin : Real;
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
   nr := 3; th := - LongNode; RotateAxes(nr, th, xin, yin, zin,
   xout, yout, zout);
END;

BEGIN
   x := Sma*(1 - Ecc); xv := 0;
   y := 0; yv := Sqrt((1 + Ecc)/x);
   z := 0; zv := 0;
   Project( x,  y,  z,  xInit,  yInit,  zInit);
   Project( xv, yv, zv, xvInit, yvInit, zvInit);
   Temp := Sqrt(xInit*xInit + yInit*yInit + zInit*zInit);
   ax := Sma*xInit/Temp; ay := Sma*yInit/Temp; az := Sma*zInit/Temp;
   Temp := Sqrt(xvInit*xvInit + yvInit*yvInit + zvInit*zvInit);
   Temp := Sma*Sqrt(1 - Ecc*Ecc)/Temp;
   bx := xvInit*Temp; by := yvInit*Temp; bz := zvInit*Temp;
END;

{------------------------------------------------------------------------}

PROCEDURE TGeometry.DrawPerihelion;
VAR
   i, NArc: Integer;
   Theta, dTheta: Real;
   CTheta, STheta, CdTheta, SdTheta, Spare: Real;
   ArcSize: Real;
   xOld, yOld, xNew, yNew: Real;
   a11, a12, a13, a21, a22, a23, a31, a32, a33: Real;
   x, y, z: Real;
   xSc, ySc: Integer;
   xView, yView: Real;
BEGIN
   SelectViewPort(1);
   a11 := Cos(LongNode);
   a12 := - Sin(LongNode)*Cos(Inc);
   a13 := Sin(LongNode)*Sin(Inc);
   a21 := Sin(LongNode);
   a22 := Cos(LongNode)*Cos(Inc);
   a23 := - Cos(LongNode)*Sin(Inc);
   a31 := 0;
   a32 := Sin(Inc);
   a33 := Cos(Inc);
   ArcSize := 0.2;
   NArc := Trunc(5*ArgPcenter/Pi) + 2;
   dTheta := ArgPcenter/NArc;
    CTheta := ArcSize; STheta := 0;
    CdTheta := Cos(dTheta); SdTheta := Sin(dTheta);
    x := a11*CTheta + a12*STheta;
    y := a21*CTheta + a22*STheta;
    z := a31*CTheta + a32*STheta;
    View(x, y, z, xOld, yOld);
    FOR i := 1 TO NArc DO
    BEGIN
       Spare := CTheta*CdTheta - STheta*SdTheta;
       STheta := STheta*CdTheta + CTheta*SdTheta;
       CTheta := Spare;
       x := a11*CTheta + a12*STheta;
       y := a21*CTheta + a22*STheta;
       z := a31*CTheta + a32*STheta;
       View(x, y, z, xNew, yNew);
       PlotLine(xOld, yOld, xNew, yNew);
       xOld := xNew; yOld := yNew;
    END;
   Arrowx(0, 0, 0, 1.5*ax, 1.5*ay, 1.5*az);
   View(1.6*ax, 1.6*ay, 1.6*az, xView, yView);
   Map(xView, yView, xSc, ySc);
   OutTextxy(xSc, ySc, 'P');
END;

{------------------------------------------------------------------------}

PROCEDURE TGeometry.DrawOrbit;
VAR
   Angle, DelAngle, CAngle, SAngle, CDelAngle, SDelAngle, Spare: Real;
   xOld, yOld, xNew, yNew: Real;
   x, y, z: Real;
   i: Integer;
BEGIN
   SelectViewPort(1); 
   DelAngle := 3.14159265/20;
   CDelAngle := Cos(DelAngle);
   SDelAngle := Sin(DelAngle);
   CAngle := 1;
   SAngle := 0;
   x := ax*(CAngle - Ecc) + bx*SAngle;
   y := ay*(CAngle - Ecc) + by*SAngle;
   z := az*(CAngle - Ecc) + bz*SAngle;
   View(x, y, z, xOld, yOld);
   FOR i := 1 TO 40 DO
   BEGIN
      Spare := Cangle*CDelAngle - SAngle*SDelAngle;
      Sangle := SAngle*CDelAngle + CAngle*SDelAngle;
      CAngle := Spare;
      x := ax*(CAngle - Ecc) + bx*SAngle;
      y := ay*(CAngle - Ecc) + by*SAngle;
      z := az*(CAngle - Ecc) + bz*SAngle;
      View(x, y, z, xNew, yNew);
      PlotLine(xOld, yOld, xNew, yNew);
      xOld := xNew; yOld := yNew;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE TGeometry.PlotOrbitPoint;
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
   m := n*t;
   Kepler(m, Ecc, x, c, s);
   f := (c - 1)/(1 - Ecc) + 1;
   g := t + (s - x)/n;
   x := xInit*f + xvInit*g;
   y := yInit*f + yvInit*g;
   z := zInit*f + zvInit*g;
   View(x, y, z, xView, yView);
   SetColor(Yellow);
   PlotSymbol(xView, yView, '*');
end;


PROCEDURE TGeometry.DrawFigure;
BEGIN
   HideCursor;
   SetColor(Yellow);  
   DrawSun;
   SetColor(Cyan);
   DrawNorth;
   DrawEcliptic;
   DrawVernalEquinox;
   DrawNodes;
   SetColor(LightRed);
   DrawOrbitPlane;
   DrawPerihelion;
   DrawOrbit;
   ShowCursor;
END;


PROCEDURE TGeometry.AnimateOrbit;
BEGIN
      AnimationOff := False;
      TrigFunctions;
      t := t + Dt;
      IF t > Period THEN t := t - Period;
      DrawFigure;
      PlotOrbitPoint
END;

{------------------------------------------------------------------------}

PROCEDURE TGeometry.SetUpFigure;
VAR
   Progress: Boolean;
   ViewPort : ViewPortType;
   x1, y1, x2, y2 : integer;


BEGIN
   OpenViewPort(1);
   ShowMouse;
   Print(2, 15, 'Click or Press any key to procede');
   SetColor(Yellow);
   DrawSun;
   Message('The North pole of the ecliptic');
   StaticPause;
   SetColor(Cyan);
   DrawNorth;
   Message('The North pole of the ecliptic');
   StaticPause;
   DrawEcliptic;
   Message('The plane of the ecliptic');
   StaticPause;
   DrawVernalEquinox;
   Message('The direction of the vernal equinox');
   StaticPause;
   DrawNodes;
   Message(Concat('Line of nodes. The longitude of the ascending node = ',
           NumStr(180*LongNode/Pi+ 0.001,3,0),' degrees'));
   StaticPause;
   SetColor(LightRed);
   DrawOrbitPlane;
   Message(Concat('The orbital plane. The inclination = ',
            NumStr(180*Inc/Pi+ 0.001,3,0),' degrees'));
   StaticPause;
   DrawPerihelion;
   Message(concat('Direction of perihelion. The argument of perihelion = ',
                   NumStr(Trunc(180*ArgPcenter/Pi+ 0.001),3,0),' degrees'));
   StaticPause;
   DrawOrbit;
   Message(concat('The eccentricity of the orbit is ', NumStr(Ecc,6,4)));
   StaticPause;
   AnimationOff := False;
   t := 0;
   Dt := 0.1;
   message('');
   HideMouse;
   HotKeys.Display;
   Picture.Select(1);
END;


procedure RunElements;
VAR
   Key         : Byte;
   ElementMenu : TMenu;
   ExitRequest : Boolean;
   geo         : TGeometry;

PROCEDURE HandleElementMenu; FORWARD;

PROCEDURE SetUpElementMenu;
BEGIN
   With ElementMenu DO
   BEGIN
      Init;
      Column(1, 'File');
       row(1,1, 'About CUPS');
       row(1,2, 'About Program');
       row(1,3, 'Configuration');
       row(1,4, 'Exit this module');
      Column(2, 'Data');
       row(2,1, 'Enter a set of orbital elements');
       row(2,2, 'Continue animation');
   END;
   ElementMenu.Display;
END;

{------------------------------------------------------------------------}


{------------------------------------------------------------------------}

{------------------------------------------------------------------------}

PROCEDURE SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      HotKeys.Init(5);
      Key[1] := 'RIGHT-Right';
      Key[2] := 'LEFT-Left';
      Key[3] := 'UP-Up';
      Key[4] := 'DOWN-Down';
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

{------------------------------------------------------------------------}

PROCEDURE HandleHotKeys(key: Byte);
CONST
   Increment = Pi/36;
BEGIN
   geo.AnimationOff := True;
   with geo do
   CASE Key OF
      1: BEGIN TwistAngle := TwistAngle - Increment;
            AnimationOff := False;
         END;
      2: BEGIN TwistAngle := TwistAngle + Increment;
            AnimationOff := False;
         END;
      3: BEGIN Altitude := Altitude - Increment;
            AnimationOff := False;
         END;
      4: BEGIN Altitude := Altitude + Increment;
            AnimationOff := False;
         END;
      5: BEGIN
            AnimationOff := True;
            ShowCUPSKeys := True;
            SetActivePage(0); CUPSKeys.Display;
            SetActivePage(1); CUPSKeys.Display;
         END;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleCUPSKeys(Key: Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HELPNbod.HLP', 'Elements Hello');
      2: begin
            IF ElementMenu.Chosen THEN HandleElementMenu;
            message('');
         end;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE GetData;
BEGIN
   If not geo.ReadElementData THEN
   BEGIN
      ElementMenu.RowActivate(2,2, False);
      IF ElementMenu.Chosen THEN HandleElementMenu;
      message('');
   END
   ELSE ElementMenu.RowActivate(2,2, True);
END;


{------------------------------------------------------------------------}

PROCEDURE HandleElementMenu;
BEGIN
   With ElementMenu DO
   CASE ColChosen OF
      1: CASE RowChosen OF
            1: AboutCUPS;
            2: ShowHelp('HELPNbod.HLP', 'Elements Hello');
            3: Configuration;
            4: ExitRequest := True;
         END;
      2: CASE RowChosen OF
            1: GetData;
            2: BEGIN
                  Geo.AnimationOff := False;
                  HideMouse;
                  ShowCUPSKeys := False;
                  CUPSKeys.Clear;
                  HotKeys.Display;
                  message('');
                  Picture.Select(1);
                  Geo.t := Geo.t - Geo.Dt;
               END;
         END;
    END;
END;


{------------------------------------------------------------------------}

BEGIN
   Picture.Init;
   SetGraphMode(vgamed);
   ShowMouse;
{  CUPSInit; }
{   new(geo); Mac}
   geo.init;
   geo.SetUpDefaultData;
   DefineViewPort(1, 0.05, 0.95, 0.07, 0.93);
   DefineScale(1, -2, 2, -2, 2);
   DefineViewPort(2, 0.05, 0.95, 0.8, 1);
   GraphBackColor := Black;
   ExitRequest := False;
   {OpenViewPort(1);}
   SelectScale(1);
   SetUpHotKeys;
   SetUpCUPSKeys;
   ShowCUPSKeys := True;
   CUPSKeys.Display;
   SetUpElementMenu;
   ShowHelp('HELPNbod.HLP', 'Elements Hello');
   GetData;
   REPEAT
      CheckForEvents;
      IF ShowCUPSKeys THEN
      BEGIN
         IF CUPSKeys.Pressed(Key) THEN HandleCUPSKeys(Key);
      END
      ELSE
      BEGIN
         IF HotKeys.Pressed(Key) THEN HandleHotKeys(Key);
         IF NOT geo.AnimationOff THEN
         BEGIN
            geo.AnimateOrbit;
            Picture.SwitchAndClean;
         END
         ELSE
         BEGIN
            geo.t := geo.t - geo.Dt;
            geo.AnimateOrbit;
            HotKeys.Clear;
            Picture.Close;
            IF ElementMenu.Chosen THEN HandleElementMenu;
            message('');
         END;
      END;
   UNTIL  ExitRequest;
{  CUPSDone;}
   SetGraphMode(vgahi);
   ElementMenu.Done;
END;

END.
