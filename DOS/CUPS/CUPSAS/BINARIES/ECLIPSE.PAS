    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1994.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program to display the light curve of an      *)
    (*       eclipsing binary, with the option of also     *)
    (*       viewing the animated orbital motion, or       *)
    (*       animated eclipses.                            *)
    (*                                                     *)
    (*******************************************************)

unit Eclipse;
{$M 64000,0,327680}
{$O+}
{$F+}
interface
procedure RunEclipse;

implementation
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;

procedure RunEclipse;
VAR
   dt    : Real;  {Time between calculated positions.            }
   xMin  : Real;  {                                              }
   xMax  : Real;  { Maximum and minimum coordinates.             }
   yMin  : Real;  {                                              }
   yMax  : Real;  {                                              }
   Sc    : Real;  {Scale factor for the animated graphics.       }


VAR
   EBMenu         : TMenu;
   CUPSKeys       : THotKeys;
   HotKeys        : THotKeys;
   Picture        : TFRames;
   Key            : Byte;
   EBDataOK       : Boolean;
   ShowCUPSKeys   : Boolean;

VAR
   Ecc, ArgPcenter, Inclin    : Real; {Orbital elements.}
   ArgPCenterDeg, InclinDeg   : Real;
   MagA, MagB                 : Real; {Magnitudes of the stars.}
   MagMax, MagMin             : Real; {Greatest and least combined magnitudes.}
   LA, LB                     : Real; {Maximum luminosities of the stars.}
   LA0, LB0                   : Real; {Mean luminosities, per unit area.}
   ColorA, ColorB             : Word;
   DarkA, DarkB               : Real; {Measures of limb darkening. 0 for no darkening.}
   Aa, Ab, Ba, Bb             : Real; {Dimensions of the stars.}
   AaInput, AbInput, BaInput, BbInput: Real; {Stored input values.}
   x, y, z, vx, vy, vz        : Real;
   t                          : Real;
   Tick                       : LongInt; {Useless quantity used in Mac Delay function.}
   Lag                        : Integer;  {Delay. in milliseconds between plotted points.}
   Cx, Cy                     : Real;
   Action, PlotOrbit          : Boolean;
   RepeatOrbit                : Boolean;
   ExitRequest                : Boolean;
   LightMenu                  : Boolean;   {Menu highlighted if true.}
   P0Oldx, P0Oldy, P0Newx, P0Newy: Real;
   P1Oldx, P1Oldy, P1Newx, P1Newy: Real;
   Axo, Ayo, Axn, Ayn         : Real; {old and new orbital coordinates.}
   xLArc, yLArc               : Array[0..10] of Real; {Preceding coordinates for the light curve.}
   LArcNum                    : Integer;
   xArc, yArc                 : Array[0..10] of Real; {Preceding coordinates for the orbit.}
   ArcNum                     : Integer;
   ViewOrbit                  : Boolean;

PROCEDURE HandleEBMenu; FORWARD;

{------------------------------------------------------------------------}


PROCEDURE SetUpEBMenu;
BEGIN
   {New(EBMenu);}
   With EBMenu DO
   BEGIN
      Init;
      Column(1, 'File');
       row(1,1, 'About CUPS');
       row(1,2, 'About Program');
       row(1,3, 'Configuration');
       row(1,4, 'Exit this module');
      Column(2, 'Help');
       row(2,1, 'Parameters');
      Column(3, 'Data');
       row(3,1, 'Enter data');
      Column(4, 'Orbit');
       row(4,1, 'See orbital motion');
      Column(5, 'Eclipses');
       row(5,1, 'See eclipses');
      RowActivate(4,1, False);
      RowActivate(5,1, False);
      Display;
   END;
END;
{------------------------------------------------------------------------}

PROCEDURE SetUpHotKeys;
BEGIN
{   New(HotKeys);}
   WITH HotKeys DO
   BEGIN
      Init(5);
      Key[1] := 'F1-Help';
      Key[2] := 'F2-Stop';
      Key[3] := 'F5-Faster';
      Key[4] := 'F6-Slower';
      Key[5] := 'F10-Menu';
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpCUPSKeys;
BEGIN
   WITH CUPSKeys DO
   BEGIN
      Init(2);
      Key[1] := 'F1-Help';
      Key[2] := 'F10-Menu';
   END;
END;

{------------------------------------------------------------------------}
 
PROCEDURE HandleHotKeys(key: Byte;  VAR PlotOrbit, Action: Boolean);
VAR
   DelLag: Integer;
BEGIN
   DelLag := 20;
   PlotOrbit := True;
   CASE Key OF
      1: ShowHelp('HELPBins.HLP', 'Eclipse Hello');
      2: BEGIN
            IF Action THEN
            BEGIN
               WITH HotKeys DO Key[2] := 'F2-Run';
               HotKeys.BackColor := Red;
               IF NOT ViewOrbit THEN picture.pause;
               HotKeys.Display;
            END
            ELSE
            BEGIN
               WITH HotKeys DO Key[2] := 'F2-Stop';
               HotKeys.BackColor := Blue;
               HotKeys.Display;
               IF NOT ViewOrbit THEN picture.continue;
            END;
            action := not action;
         END;
      3: IF Lag > DelLag THEN Lag := Lag - DelLag;
      4: Lag := Lag + DelLag;
      5: BEGIN
            IF NOT ViewOrbit THEN Picture.Close;
            Action    := False;
            PlotOrbit := False;
            LightMenu := True;
            HotKeys.Clear;
            CUPSKeys.Display;
            ShowCUPSKeys := True;
         END;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleCUPSKeys(Key : Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HELPBins.HLP', 'Eclipse Hello');
      2: IF EBMenu.Chosen THEN HandleEBMenu;
   END;
END;

{------------------------------------------------------------------------}

FUNCTION StarColor(Rad, Mag: Real): Word;
VAR
   ColorTest : Real;
BEGIN
   ColorTest := 0.1*(4 - Mag) - 0.5*Ln(10*Rad);
   ColorTest := Exp(ColorTest*Ln(10));
   IF ColorTest <= 0.4 THEN StarColor := Red;
   IF (0.3 < ColorTest) AND (ColorTest <= 0.7) THEN StarColor := Yellow;
   IF (0.7 < ColorTest) AND (ColorTest < 1.5) THEN StarColor := Green;
   IF ColorTest >= 1.5 THEN StarColor := Blue;
END;

{------------------------------------------------------------------------}

Procedure EBData1(VAR Aa, Ab, MagA, Darka, LA, LA0: Real);
VAR
   EBDat1: TInputScreen;
   AaTemp, AbTemp: Real;
BEGIN
   WITH EBDat1 DO
   BEGIN
      Init;
      DefineInputPort(0.05, 0.95, 0.15, 0.85);
      LoadLine('                   DATA FOR AN ECLIPSING BINARY');
      LoadLine('');
      LoadLine('The semimajor axis of the relative orbit is one unit of length:');
      LoadLine('Enter data for the shape of star A:');
      LoadLine('Semimajor axis of star A, Aa = {      } units');
      LoadLine('Semiminor axis of star A, Ab = {      } units (not larger than Aa)');
      LoadLine('Magnitude of star A:     -5 <= {      } <= 5.');
      LoadLine('The limb-darkening coefficient should lie between zero and one.');
      LoadLine('Limb-darkening coefficient of star A: {    } (Zero for no darkening.)');
      LoadLine('');
      LoadLine('                 [  OK  ]                 [Cancel]');
      SetNumber(1, AaInput);
      SetNumberLimits(1, 0.0001, 0.99);
      SetNumber(2, AbInput);
      SetNumberLimits(2, 0.0001, 0.99);
      SetNumber(3, MagA);
      SetNumberLimits(3, -5, 5);
      SetNumber(4, DarkA);
      SetNumberLimits(4, 0, 1);
      REPEAT
         AcceptScreen;
         AaTemp := GetNumber(1);
         AbTemp := GetNumber(2);
         IF AaTemp < AbTemp THEN Announce('Aa >= Ab. Try again.');
      UNTIL (Canceled) OR (AaTemp >= AbTemp);
      IF NOT Canceled THEN
      BEGIN
         AaInput := AaTemp;
         AbInput := AbTemp;
         Aa := Sc*AaInput;
         Ab := Sc*AbInput;
         MagA := GetNumber(3);
         DarkA := GetNumber(4);
         LA := Exp(- ln(100)*MagA/5);
         LA0 := LA/((3.14159*Aa*Ab)*(1 - DarkA/3));
         ColorA := StarColor(Aa, MagA);
      END
      ELSE EBDataOK := False;
      Done;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE EBData2(VAR Ba, Bb, MagB, DarkB, LB, LB0, Ecc, ArgPcenter, Inclin: Real);
VAR
   EBDat2: TInputScreen;
   BaTemp, BbTemp: Real;
BEGIN
   WITH EBDat2 DO
   BEGIN
      Init;
      DefineInputPort(0.05, 0.95, 0.13, 0.87);
      LoadLine('                   DATA FOR AN ECLIPSING BINARY.');
      LoadLine('');
      LoadLine('Semimajor axis of star B, Ba = {      } units. Make Aa + Ba < 1');
      LoadLine('Semiminor axis of star B, Bb = {      } units (not larger than Ba)');
      LoadLine('Magnitude of star B:     -5 <= {      } <= 5.');
      LoadLine('The limb-darkening coefficient should lie between zero and one.');
      LoadLine('Limb-darkening coefficient of star B: {    } (Zero for no darkening.)');
      LoadLine('    Orbital eccentricity   e = {      } 0 <= e < 1');
      LoadLine('    Argument of Periastron   = {      } degrees');
      LoadLine('    Inclination (near 90)    = {      } degrees');
      LoadLine('');
      LoadLine('                 [  OK  ]                 [Cancel]');
      SetNumber(1, BaInput);
      SetNumberLimits(1, 0.0001, 0.99);
      SetNumber(2, BbInput);
      SetNumberLimits(2, 0.0001, 0.99);
      SetNumber(3, MagB);
      SetNumberLimits(3, -5, 5);
      SetNumber(4, DarkB);
      SetNumberLimits(4, 0, 1);
      SetNumber(5, Ecc);
      SetNumberLimits(5, 0, 0.99999);
      SetNumber(6, ArgPCenterDeg);
      SetNumberLimits(6, 0, 360);
      SetNumber(7, InclinDeg);
      SetNumberLimits(7, 0, 180);
      REPEAT
         AcceptScreen;
         BaTemp := GetNumber(1);
         BbTemp := GetNumber(2);
         IF BaTemp < BbTemp THEN Announce('Ba >= Bb. Try again.');
         IF (AaInput + BaTemp > 1) THEN
            Announce ('Ba > 1 - Aa = '+NumStr(1-AaInput, 3,2)+', try again');
      UNTIL (Canceled) OR ((BaTemp >= BbTemp) AND (AaInput + BaTemp < 1));
      IF NOT Canceled THEN
      BEGIN
         BaInput := BaTemp;
         BbInput := BbTemp;
         Ba := Sc*BaInput;
         Bb := Sc*BbInput;
         MagB := GetNumber(3);
         DarkB := GetNumber(4);
         LB := Exp(- ln(100)*MagB/5);
         LB0 := LB/((3.14159*Ba*Bb)*(1 - DarkB/3));
         Ecc := GetNumber(5);
         ArgPcenterDeg :=  GetNumber(6);
         ArgPcenter := ArgPcenterDeg*3.14159/180;
         InclinDeg := GetNumber(7);
         Inclin := InclinDeg*3.14159/180;
         ColorB := StarColor(Ba, MagB);
      END
      ELSE EBDataOK := False;
      Done;
   END;
END;

{------------------------------------------------------------------------}


PROCEDURE FindExtremeMagnitudes(VAR MagMax, MagMin: Real);
VAR
   ASize, BSize, LMin: Real;
BEGIN
   MagMax := - (5/ln(100))*Ln(LA*(1 + Darka/2) + LB*(1 + DarkB/2));
   ASize := 3.14159*Ab*Ab; {Minimum cross-sectional areas,}
   BSize := 3.14159*Bb*Bb; {as viewed during an eclipse.}
   IF Asize > BSize
   THEN  BEGIN
            IF LA0 > LB0 THEN LMin := LA0*ASize - (LA0 - LB0)*BSize
                         ELSE LMin := LA0*ASize;
         END
   ELSE  BEGIN
            IF LA0 > LB0 THEN LMin := LB0*BSize
                         ELSE LMIn := LB0*BSize - (LB0 - LA0)*ASize;
         END;
   MagMin := -(5/ln(100))*Ln(LMin);
   {Truncate to fit inside scale.}
   MagMax := Trunc(10*MagMax)/10;
   MagMin := Trunc(10*MagMin)/10 + 0.1;
END;

{------------------------------------------------------------------------}

PROCEDURE PrepareGraphics;
BEGIN
   DefineViewPort(1, 0, 1, 0.08, 0.545);
   DefineScale(1, -5, 5, -4, 1);
   DefineViewPort(2, 0, 1, 0.555, 0.93);
   DefineScale(2, -5, 5, 0.5, 4);
   DefineViewPort(3, 0, 1, 0.08, 0.93);
   DefineScale(3, -5, 5, -4, 4);
END;

{------------------------------------------------------------------------}

Procedure Kepler(m, ec0, es0: Real; VAR xx, c, s, fp: Real);
{Solves Kepler's equation for elliptic motion.}
VAR
   f, fpp, fppp, dx: Real;
begin
   xx := m;
   repeat
      s := sin(xx); c := cos(xx);
      f := xx - ec0*s + es0*(1 - c) - m;
      fp := 1 - ec0*c + es0*s;
      fpp := ec0*s + es0*c;
      fppp := ec0*c - es0*s;
      dx := - f/fp;
      dx := - f/(fp + dx*fpp/2);
      dx := - f/(fp + dx*fpp/2 + dx*dx*fppp/6);
      xx := xx + dx;
   until abs(dx) < 0.000001;
end;

{------------------------------------------------------------------------}

Procedure Ivp(x0, y0, vx0, vy0: Real; VAR x1, y1, vx1, vy1: Real);
{Solves the initial value problem.}
VAR
   r0, a, n, m, f, g, vf, vg, ec0, es0, xx, c, s, fp: Real;
begin
   r0 := sqrt(x0*x0 + y0*y0);
   a := 1/(2/r0 - vx0*vx0 - vy0*vy0);
   n := 1/(a*sqrt(a));
   ec0 := 1 - r0/a;
   es0 := (x0*vx0 + y0*vy0)/(n*a*a);
   m := dt*n;
   Kepler(m, ec0, es0, xx, c, s, fp);
   f := (a/r0)*(c - 1) + 1;
   g := dt + (s - xx)/n;
   vf := - a/(fp*r0)*n*s;
   vg := (1/fp)*(c - 1) + 1;
   x1 := x0*f + vx0*g;
   y1 := y0*f + vy0*g;
   vx1 := x0*vf + vx0*vg;
   vy1 := y0*vf + vy0*vg;
   vx0 := vx1; vy0 := vy1;
end;

{------------------------------------------------------------------------}

Procedure RotateAxes(nr: Integer; th, xin, yin, zin: Real; VAR xout, yout, zout
: Real);
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

{------------------------------------------------------------------------}

Procedure Project(ArgPcenter, Inclin, xin, yin, zin: Real;
      VAR xout, yout, zout: Real);
VAR
   nr: Integer;
   th: Real;
BEGIN
   nr := 3; th := - ArgPcenter; RotateAxes(nr, th, xin, yin, zin,
   xout, yout, zout);
   xin := xout; yin := yout; zin := zout;
   nr := 1; th := - Inclin; RotateAxes(nr, th, xin, yin, zin,
   xout, yout, zout);
END;

{------------------------------------------------------------------------}

Procedure ScaleEllipse(a, b, th: real; VAR Sa, Sb: Integer);
VAR
   dx, dy: Real;
BEGIN
   a := Sqrt(Sqr(a*Cos(th)) + Sqr(b*Sin(th)));
   dx := GetMaxX/(xMax - xMin);
   dy := GetMaxY/(yMax - yMin);
   Sa := Trunc(a*dx);
   Sb := Trunc(b*dy);
END;

{------------------------------------------------------------------------}

Function LimbDarkening(Rad, Dark: Real):Real;
BEGIN
   LimbDarkening := Dark*Sqrt(1 - Rad*Rad) + 1 - Dark;
END;

Procedure Eclipse(a1, b1, a2, b2, x2, y2, z2: Real;
                  VAR Total: Boolean; VAR Bite: Real);
VAR
   xLower, xUpper, yLower, yUpper: Real; {Limits for the eclipsed area.}
   x, y, dx, dy: Real;
   xi, yj, i, j: Integer;
   ay1, ay2, by1, by2: Real;
   r: Real;
   Covered: Boolean;
   Dark: Real; {Darkening parameter for the eclipsed star.}
BEGIN
   If z2 > 0 Then Dark := DarkB ELSE Dark := DarkA;
   Bite := 0;
   Total := False;
   {Test for totality.}
   IF  (Sqr((a1 - x2)/a2) + Sqr(y2/b2) < 1)
   AND (Sqr((a1 + x2)/a2) + Sqr(y2/b2) < 1)
   AND (Sqr(x2/a2) + Sqr((b1 - y2)/b2) < 1)
   AND (Sqr(x2/a2) + Sqr((b1 + y2)/b2) < 1)
   THEN Total := True
   ELSE
   BEGIN
      IF x2 - a2 < - a1 THEN xLower := -a1 ELSE xLower := x2 - a2;
      IF x2 + a2 > a1 THEN xUpper := a1 ELSE xUpper := x2 + a2;
      xi := 5 + Trunc((xUpper - xLower)/0.01);
      dx := (xUpper - xLower)/xi;
      FOR i := 1 TO xi DO
      BEGIN
         x := xLower + dx*(i - 0.5);
         Covered := False;
         ay1 := b1*Sqrt(1 - Sqr(x/a1));
         ay2 := - ay1;
         by1 := y2 + b2*Sqrt(1 - Sqr((x - x2)/a2));
         by2 := 2*y2 - by1; {y-coordinates of intersections of the x-strip.}
         IF by2 < ay1 THEN
         BEGIN
            IF by2 > ay2 THEN
            BEGIN
               IF by1 > ay1 THEN
               BEGIN
                  yLower := by2;
                  yUpper := ay1;
                  Covered := True;
               END
               ELSE
               BEGIN
                  yLower := by2;
                  yUpper := by1;
                  Covered := True;
               END;
            END
            ELSE
            BEGIN
               IF by1 > ay1 THEN
               BEGIN
                  yLower := ay2;
                  yUpper := ay1;
                  Covered := True;
               END
               ELSE
               BEGIN
                  IF by1 > ay2 THEN
                  BEGIN
                     yLower := ay2;
                     yUpper := by1;
                     Covered := True;
                  END;
               END;
            END;
         END;
         IF Covered = True THEN
         BEGIN
            IF Dark = 0 THEN Bite := Bite + dx*(yUpper - yLower)
            ELSE
            BEGIN
               yj := 5 + Trunc((yUpper - yLower)/0.01);
               dy := (yUpper - yLower)/yj;
               FOR j := 1 TO yj DO
               BEGIN
                  y := yLower + dy*(j - 0.5);
                  r := Sqrt(Sqr(x/a1) + Sqr(y/b1));
                  Bite := Bite + dx*dy*LimbDarkening(r, Dark);
               END; {j loop.}
            END;
         END; {Test for Covered.}
      END; {i loop.}
   END;
END;

{------------------------------------------------------------------------}

Procedure Magnitude(th: Real; xB, yB, zB: Real; VAR Mag: Real);
VAR
   Luminosity: Real;
   Bite: Real; {Area affected by an eclipse.}
   AaProj, BaProj: Real; {Observed major axes.}
   Total: Boolean;
BEGIN
   Bite := 0;
   AaProj := Sqrt(Sqr(Aa*Cos(th)) + Sqr(Ab*Sin(th)));
   BaProj := Sqrt(Sqr(Ba*Cos(th)) + Sqr(Bb*Sin(th)));
   If Abs(xB) < AaProj + BaProj THEN  {An eclipse is possible.}
   BEGIN   
      IF zB > 0 {Star B is eclipsed.}
      THEN Eclipse(BaProj, Bb, AaProj, Aa, xB, yB, zB, Total, Bite)
      ELSE {Star A is eclipsed.}
           Eclipse(AaProj, Ab, BaProj, Bb, xB, yB, zB, Total, Bite);
   END;
   IF zb > 0 THEN
   BEGIN
      IF Total = True THEN
                      Luminosity :=  LA0*Pi*AaProj*Ab
                      ELSE
                      Luminosity :=  LA0*Pi*AaProj*Ab
                                   + LB0*(Pi*BaProj*Bb - Bite);
   END
   ELSE
   BEGIN
      IF Total = True THEN
                      Luminosity :=  LB0*Pi*BaProj*Bb
                      ELSE
                      Luminosity :=  LB0*Pi*BaProj*Bb
                                   + LA0*(Pi*AaProj*Ab - Bite);
   END;
   IF Luminosity > 0
   THEN
      Mag := - Ln(Luminosity)*5/Ln(100)
   ELSE
   BEGIN
      IF zb < 0 THEN Mag := MagB ELSE Mag := MagA;
   END;
END;

{------------------------------------------------------------------------}

Procedure MagScale; {Draws a magnitude scale on the screen.}
VAR
   xp, yp: Integer;
   MagStr: String;
   Width, Dec: Integer;
BEGIN
   GraphBackColor := DarkGray;
   SelectViewPort(1);
   SelectScale(1);
   SetColor(Whitte);
   PlotLine(-4.9, 0.5, -4.9, -3.5);
   PlotLine(-4.9, 0.5, -4.8, 0.5);
   PlotLine(-4.9, -3.5, -4.8, -3.5);
   Map(-4.8, -3.7, xp, yp);
   OutTextxy(xp, yp, 'Magnitude');
   Width := 3; Dec := 1;
   MagStr := NumStr(MagMax, Width, Dec);
   Map(-4.8, 0.9, xp, yp);
   OutTextxy(xp, yp, MagStr);
   MagStr := NumStr(MagMin, Width, Dec);
   Map(-4.8, -3.51, xp, yp);
   OutTextxy(xp, yp, MagStr);
END;

{------------------------------------------------------------------------}

FUNCTION MagMap(Mag: Real): Real; {Scales the magnitude for plotting.}
BEGIN
   MagMap := 0.5 - 4*(Mag - MagMax)/(MagMin - MagMax);
END;

{------------------------------------------------------------------------}

PROCEDURE InitializeEclipseScreen;
BEGIN
   CloseViewPort(3);
   HotKeys.Display;
   message('');
   HideMouse;
{Initialize animation.}
   Action := True;
   PlotOrbit := True;
   RepeatOrbit := False;
   LArcNum := 0;
   t := 0;
{Initialize screen for light curve.}
 {  GraphBackColor := Red;
   OpenViewPort(1);
   SelectScale(1); }
   FindExtremeMagnitudes(MagMax, MagMin);
  { MagScale;
   SetColor(Whitte);
   PrintXY(0.8, 0.4, 'Light Curve'); }
{Select eclipse screen for switching.}
   Picture.Select(2);
END;

{------------------------------------------------------------------------}

Procedure RenewScreen;
{Used when the light curve animation has completed one revolution.}
VAR
   Mag: Real;
   xt, yt, zt: Real; {Projected orbital coordinates.}
   xp, yp: Integer;  {Screen coordinates.}
BEGIN
   LArcNum := 0;                          
  { FindExtremeMagnitudes(MagMax, MagMin);}
   x := (1 - Ecc); vx := 0;
   y := 0; vy := Sqrt((1 + Ecc)/(1 - Ecc));
   z := 0; vz := 0;
   t := 0;
   Cx := 0; Cy := 2; {Center Coordinates for animation.}
   Magnitude(ArgPcenter, Cx + x, Cy + y, 1, Mag);
   xLArc[0] := -4.9;
   yLArc[0] := MagMap(Mag);
END;


{------------------------------------------------------------------------}

PROCEDURE DrawLArc;
VAR
   i: Integer;
BEGIN
   FOR i := 1 TO LArcNum DO PlotLine(xLArc[i-1], yLArc[i-1], xLArc[i], yLArc[i]);
END;

{------------------------------------------------------------------------}

Procedure MakeEclipseScreen;
VAR
   xt, yt, zt: Real; {Projected orbital coordinates.}
   xp, yp: Integer;  {Screen coordinates.}
   th: Real;
   Sa, Sb: Integer;
   Mag: Real;
   i: Integer;

{------------------------------------------------------------------------}

PROCEDURE PlotStars;
BEGIN
   GraphBackColor := Blakk;
   OpenViewPort(2); 
   SelectScale(2);
   Project(ArgPcenter, Inclin, x, y, z, xt, yt, zt);
   HideMouse;
   IF zt > 0 THEN  {Star A is in front.}
   BEGIN
      Map(Cx + Sc*xt, Cy + Sc*yt, xp, yp);
      SetColor(ColorB); SetFillStyle(1, ColorB);
      ScaleEllipse(Ba, Bb, th, Sa, Sb);
      FillEllipse(xp, yp, Sa, Sb);
      SetColor(ColorA); SetFillStyle(1, ColorA);
      Map(Cx, Cy, xp, yp);
      ScaleEllipse(Aa, Ab, th, Sa, Sb);
      FillEllipse(xp, yp, Sa, Sb);
   END
   ELSE  {Star B is in front.}
   BEGIN
      SetColor(ColorA); SetFillStyle(1, ColorA);
      Map(Cx, Cy, xp, yp);
      ScaleEllipse(Aa, Ab, th, Sa, Sb);
      FillEllipse(xp, yp, Sa, Sb);
      Map(Cx + Sc*xt, Cy + Sc*yt, xp, yp);
      SetColor(ColorB); SetFillStyle(1, ColorB);
      ScaleEllipse(Ba, Bb, th, Sa, Sb);
      FillEllipse(xp, yp, Sa, Sb);
   END;
   ShowMouse;
END;

{---------------------------------------------------------------------}

BEGIN
   HideMouse;
   IF (t = 0) AND (NOT RepeatOrbit) THEN RenewScreen;
   Ivp(x, y, vx, vy, x, y, vx, vy);
   z := 0;
   t := t + dt;
   th := ArgPcenter + t; {Orientation of the major axis relative to the viewer.}
   IF t > 2*Pi THEN {Light curve animation must be repeated.}
   BEGIN
      RepeatOrbit := True;
      RenewScreen;
   END;
   
{Plot eclipsing stars.}
   PlotStars;
{Light curve.}
      SelectViewPort(1);
      SelectScale(1);
      IF (LArcNum <= 1) AND (NOT RepeatOrbit) THEN
      BEGIN
         GraphBackColor := DarkGray;
         OpenViewPort(1);

         MagScale;
         SetColor(Whitte);
         PrintXY(0.8, 0.2, 'Light Curve');
         HotKeys.Display;
      END;
      IF (LArcNum = 0) AND (RepeatOrbit) THEN
{Start repeating light curve. Erase asterisks at the end of the light curve.}
      BEGIN
         SelectViewPort(1);
         SelectScale(1);
   {Erase the final asterisk.}
         SetColor(DarkGray);
         PlotSymbol(xLArc[9], yLArc[9], '*');
         SetColor(LightBlue);
   {Finish the light curve uniformly between screens.}
         PlotLine(xLArc[8], yLArc[8], xLArc[9], yLArc[9]);
         PlotLine(xLArc[9], yLArc[9], xLArc[10], yLArc[10]);
      END;
      IF (LArcNum = 1) AND (RepeatOrbit) THEN
      BEGIN
         SelectViewPort(1);
         SelectScale(1);
   {Erase the final asterisk.}
         SetColor(DarkGray);
         PlotSymbol(xLArc[10], yLArc[10], '*');
         SetColor(LightBlue);
         PlotLine(xLArc[8], yLArc[8], xLArc[9], yLArc[9]);
         PlotLine(xLArc[9], yLArc[9], xLArc[10], yLArc[10]);
      END;
{Calculate the combined magnitude.}
   Magnitude(th, Sc*xt, Sc*yt, Sc*zt, Mag);
   IF LArcNum < 10 THEN
{Build up the array containing ten preceding coordinates of the light curve.}
      BEGIN
            SetColor(Whitte);
            PlotLine(-4.9, 0.5, -4.9, -3.5);
            PlotLine(-4.9, 0.5, -4.8, 0.5);
            SetColor(LightBlue);
            LArcNum := LArcNum + 1;
            xLArc[LArcNum] := -4.9 + 1.4*t;
            yLArc[LArcNum] := MagMap(Mag);
      END
      ELSE
      BEGIN
      {Update the array for the light curve.}
            FOR i := 1 TO 10 DO
            BEGIN
               xLArc[i-1] := xLArc[i];
               yLArc[i-1] := yLarc[i];
            END;
            xLArc[10] := -4.9 + 1.4*t;
            yLArc[10] := MagMap(Mag);
      END;
      SetColor(DarkGray);
      IF LArcNum > 1 THEN
{Erase preceding asterisk.}
         PlotSymbol(xLArc[LArcNum-2], yLArc[LArcNum-2], '*');
      SetColor(LightBlue);
{Re-plot light curve over erased region.}
      DrawLArc;
{Plot the next asterisk.}
      PlotSymbol(xLArc[LArcNum], yLArc[LArcNum], '*');
      ShowMouse;
END;

{------------------------------------------------------------------------}
{----------------Procedures for viewing orbital motion-------------------}

Procedure Arrow(Cx, Cy, Angle, Long: Real);
VAR
   x1, y1, x2, y2: Real;
BEGIN
   SetColor(10);
   x1 := Cx + Long*cos(Angle);
   y1 := Cy + Long*sin(Angle);
   PlotLine(Cx, Cy, x1, y1);
   x2 := x1 - {0.1*Long}0.2*cos(Angle + 0.785);
   y2 := y1 - {0.1*Long}0.2*sin(Angle + 0.785);
   PlotLine(x1, y1, x2, y2);
   x2 := x1 - {0.1*Long}0.2*cos(Angle - 0.785);
   y2 := y1 - {0.1*Long}0.2*sin(Angle - 0.785);
   PlotLine(x1, y1, x2, y2);
END;

{------------------------------------------------------------------------}

Procedure InitializeOrbitScreen;
VAR
   Mag: Real;
   xt, yt, zt: Real; {Projected orbital coordinates.}
   xp, yp: Integer;  {Screen coordinates.}
   Luminosity: Real;
   nr: Integer;
   th: Real;
   Long: Real;
BEGIN
     CloseViewPort(3);
     GraphBackColor := DarkGray;
     OpenViewPort(1);
     GraphBackColor := Black;
     OpenViewPort(2);
     HotKeys.Display;
     message('');
     SetColor(LightCyan);
     PrintXY(0.05, 0.9, 'Relative Orbit');
     SetColor(Yellow);
     PrintXY(0.05, 0.85, 'Primary. Star A');
     SelectScale(2);
     Arrow(-4.5, 2.2, 0, 1.3);
     PrintXY(0.19, 0.75, 'Viewer');
     SetColor(Whitte);
     PrintXY(0.05, 0.8, 'Secondary. Star B');
     Action := True;
     PlotOrbit := True;
     ArcNum := 0;
     LArcNum := 0;
     Lag := 200;

   SetColor(1);

{Orbital initialization.}
   SelectViewPort(2);
   SelectScale(2);
   x := (1 - Ecc); vx := 0;
   y := 0; vy := Sqrt((1 + Ecc)/(1 - Ecc));
   z := 0; vz := 0;
   t := 0;
   nr := 3; th := - ArgPcenter; RotateAxes(nr, th, x, y, z, xt, yt, zt);
{Initial coordinates of the center of mass and the stars.}
   Cx := 0; Cy := 2.0;
   Long := 1.5;
   Arrow(Cx, Cy, - ArgPcenter+ 3*3.14159/2, Long);
   Axo := Cx + x; Ayo := Cy;
   SetColor(14); PlotSymbol(Cx, Cy, 'o');
{Light curve initialization.}   
   SelectViewPort(1); SelectScale(1);
   FindExtremeMagnitudes(MagMax, MagMin);
   MagScale; 
   SetColor(Whitte);
   PrintXY(0.8, 0.2, 'Light Curve');
   Magnitude(ArgPcenter, Cx + x, Cy + y, 1, Mag);
   P0Oldx := -4.9; P0Oldy := MagMap(Mag);
   P0Newx := -4.9; P0Newy := MagMap(Mag);
   xArc[0] := Axo; yArc[0] := Ayo;
   xLArc[0] := P0Oldx; yLArc[0] := P0Oldy;
END;

{------------------------------------------------------------------------}

Procedure MakeOrbitScreen;
VAR
   xt, yt, zt, vxt, vyt, vzt: Real;
   Long: Real;
   nr: Integer;
   th: Real;
   Mag: Real;
   i: Integer;

{------------------------------------------------------------------------}

PROCEDURE DrawArc;
VAR
   i: Integer;
BEGIN
   FOR i := 1 TO ArcNum DO PlotLine(xArc[i-1], yArc[i-1], xArc[i], yArc[i]);
END;

PROCEDURE DrawLArc;
VAR
   i: Integer;
BEGIN
   FOR i := 1 TO LArcNum DO PlotLine(xLArc[i-1], yLArc[i-1], xLArc[i], yLArc[i]);
END;

{------------------------------------------------------------------------}

BEGIN
   Delay(Lag);
   {Delau(Lag div 62, Tick);}
   t := t + dt;   
{Update orbit.}
      SelectViewPort(2); SelectScale(2);
      GraphBackColor := Black;
      Ivp(x, y, vx, vy, x, y, vx, vy);
      z := 0; vz := 0;
      Axn := Cx + x; Ayn := Cy + y;
      IF ArcNum < 10 THEN
      BEGIN
         ArcNum := ArcNum + 1;
         xArc[ArcNum] := Axn; yArc[ArcNum] := Ayn;
      END
      ELSE
      BEGIN
         FOR i := 1 TO 10 DO
         BEGIN
            xArc[i-1] := xArc[i];
            yArc[i-1] := yarc[i];
         END;
         xArc[10] := Axn; yArc[10] := Ayn;
      END;
      DrawArc;
      HideMouse;
      SetColor(GraphBackColor); PlotSymbol(Axo, Ayo, '*');
      SetColor(Whitte);
      PlotSymbol(Axn, Ayn, '*');
      ShowMouse;
      Axo := Axn; Ayo := Ayn;
{Update light curve.}     
      SelectViewPort(1); SelectScale(1);
      GraphBackColor := DarkGray;
      th := ArgPcenter + t; {Orientation of the major axis relative to the viewer.}
      IF t > 2*3.14159 THEN
      BEGIN
         t := t - 2*3.14159;
         SetColor(GraphBackColor);
         HideMouse;
         PlotSymbol(P0Oldx, P0Oldy, '*');
         ShowMouse;
         Project(ArgPcenter, Inclin, x, y, z, xt, yt, zt);
         Magnitude(th, Sc*xt, Sc*yt, zt, Mag);
         LArcNum := 0;
         P0Oldx := -4.9;  P0Oldy := P0Newy;
         P0Newx := -4.9; 
         P1Oldx := -4.9;  P1Oldy := P0Newy;
         P1Newx := -4.9;  P1Newy := p0Newy;
         xLArc[0] := P0Oldx; yLArc[0] := P0Oldy;
      END;
      Project(ArgPcenter, Inclin, x, y, z, xt, yt, zt);
      Magnitude(th, Sc*xt, Sc*yt, Sc*zt, Mag);
      SetColor(GraphBackColor);
      P0Newx := -4.9 + 1.4*t; P0Newy := MagMap(Mag);
      HideMouse;
      PlotSymbol(P0Oldx, P0Oldy, '*');
      SetColor(LightCyan);
      PlotSymbol(P0Newx, P0Newy, '*');
      ShowMouse;
     { PlotLine(P0Oldx, P0Oldy, P0Newx, P0Newy);}
      IF LArcNum < 10 THEN
      BEGIN
         LArcNum := LArcNum + 1;
         xLArc[LArcNum] := P0Newx; yLArc[LArcNum] := P0Newy;
      END
      ELSE
      BEGIN
         FOR i := 1 TO 10 DO
         BEGIN
            xLArc[i-1] := xLArc[i];
            yLArc[i-1] := yLarc[i];
         END;
         xLArc[10] := P0Newx; yLArc[10] := P0Newy;
      END;
      DrawLArc;
      P0Oldx := P0Newx; P0Oldy := P0Newy;
END;

{------------------------------------------------------------------------}
{-----------------End of procedures for viewing orbital motion-------------}

{PROCEDURE Whistle;
VAR
   i: Integer;
BEGIN
   FOR i := 1 TO 20 DO
   BEGIN
      Sound(50 + 50*i);
      Delay(20);
      NoSound;
   END;
END;  }

{-------------------------------------------------------------------------}

PROCEDURE PromptChoice;
var ViewPort : ViewPortType;
    x1, y1, x2,y2,midx,midy : integer;
begin
   HideMouse;
   DefineViewPort(5, 0, 1, 0.06, 0.9);
{   GraphBackColor := Blakk;}
   SetColor(Blakk);
   SelectViewPort(5);
   GetViewSettings (ViewPort);
   with ViewPort do begin midx := (x1+x2) div 2; midy := (y1+y2) div 2; end;
   x1 := midx-190; y1 := midy-100; x2 := midx+190; y2 := midy;
   SetTextJustify(LeftText, TopText);
   SetFillStyle(SolidFill,blue);
   SetColor(Yellow);
   Bar3D(x1,y1+5,x2,y2+5,5,true);  
   OutTextXY(x1+20,y1+20,'Make a selection from the menu:');
   OutTextXY(x1+20,y1+40,'See the orbital motion and the light curve');
   OutTextXY(x1+20,y1+60,'      or');
   OutTextXY(x1+20,y1+80,'See the eclipses and the light curve');
   ShowMouse;
end;  

{-------------------------------------------------------------------------}

PROCEDURE GetData;
BEGIN
   Action := False;
   PlotOrbit := False;
   ClearMuppetPort;
   EBDataOK := True;
   EBData1(Aa, Ab, MagA, DarkA, LA, LA0);
   IF NOT EBDataOK THEN
   BEGIN
      Announce('Data must be complete to see animation');
      EBMenu.RowActivate(4,1, False);
      EBMenu.RowActivate(5,1, False);
      Exit;
   END;
   EBData2(Ba, Bb, MagB, DarkB, LB, LB0, Ecc, ArgPcenter, Inclin);
   IF EBDataOK THEN
   BEGIN
      PromptChoice;
      EBMenu.RowActivate(4,1, True);
      EBMenu.RowActivate(5,1, True);
   END
   ELSE
   BEGIN
      Announce('Data must be complete to see animation');
      EBMenu.RowActivate(4,1, False);
      EBMenu.RowActivate(5,1, False);
   END;
   LightMenu := True;
   CUPSKeys.Display;
   ShowCUPSKeys := True;
END;

{--------------------------------------------------------------------------}

PROCEDURE SetUpDefaultInput;
BEGIN
   AaInput := 0.1;
   AbInput := 0.1;
   BaInput := 0.1;
   BbInput := 0.1;
   MagA := 0;
   MagB := 2.0;
   DarkA := 0.0;
   DarkB := 0.0;
   Ecc := 0.5;
   ArgPCenterDeg := 45;
   InclinDeg := 90;
   Lag := 100;
   dt := 0.05;  {Time between calculated positions.            }
   xMin := -5;  {                                              }
   xMax := 5;   { Maximum and minimum coordinates.             }
   yMin := -4;  {                                              }
   yMax := 4;   {                                              }
   Sc := 3;     {Scale factor for the animated graphics.       }
   ExitRequest := False;
   LightMenu := False;
   ShowCUPSKeys := False;
   {New(Picture);}
END;

{--------------------------------------------------------------------------}

PROCEDURE HandleEBMenu;

BEGIN
   With EBMenu DO
   CASE ColChosen OF
      1: CASE RowChosen OF
            1: BEGIN
                  ShowHelp('HELPBins.HLP', 'About CUPS');
                  LightMenu := True;
               END;
            2: BEGIN
                  ShowHelp('HELPBins.HLP', 'Eclipse Hello');
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
                  ShowHelp('HELPBins.HLP', 'Eclipse General');
                  LightMenu := True;
               END;
         END;
      3: GetData;
      4: BEGIN
            ShowCUPSKeys := False;
            ViewOrbit := True;
            InitializeOrbitScreen;
         END;
      5: BEGIN
            ShowCUPSKeys := False;
            ViewOrbit := False;
            InitializeEclipseScreen;
         END;
      
    END;
END;


{------------------------------------------------------------------------}

 
BEGIN  {Main Program.}
   Picture.Init;
   SetGraphMode(VGAmed);
{  CUPSinit;}
   SetUpEBMenu;
   SetUpHotKeys;
   SetUpCUPSKeys;
   CUPSKeys.Display;
   ShowCUPSKeys := True;
   ShowMouse;
   ShowHelp('HELPBins.HLP', 'Eclipse Hello');
   PrepareGraphics;
   SetUpDefaultInput;
   GetData;
   REPEAT
      CheckForEvents;
      IF ShowCUPSKeys THEN
             IF CUPSKeys.Pressed(Key) THEN HANDLECUPSKeys(Key);
      IF Hotkeys.Pressed(Key) THEN
            HandleHotKeys(Key, PlotOrbit, Action);
      IF Action AND PlotOrbit THEN
      BEGIN
         IF ViewOrbit THEN MakeOrbitScreen
         ELSE
         BEGIN
            MakeEclipseScreen;
            Delay(Lag);
            {Delay(Lag div 62, Tick);}
            Picture.Switch;
         END;
      END;
      IF LightMenu AND EBMenu.Chosen THEN HandleEBMenu;
      LightMenu := not PlotOrbit;
   UNTIL ExitRequest;
{  CUPSDone;}
   EBMenu.done;
   SetGraphMode(VGAhi);
   {Dispose(EBMenu);}
   {Dispose{HotKeys);}
   {Dispose(Picture);}
END;

END.


