    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1994.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program to display the velocity curve         *)
    (*       of one member of a spectroscopic binary       *)
    (*       together with the animated orbital motion     *)
    (*       and the moving spectral line.                 *)
    (*                                                     *)
    (*******************************************************)

unit Spectro;
{$M 64000,0,327680}
{$O+}
{$F+}
interface
procedure RunSpectro;

implementation
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;

procedure RunSpectro;
CONST
   dt = 0.06283185307;  {Time between calculated positions.}
   Gk = 0.01720209895;

VAR
   SBMenu                       : TMenu;
   SBDat                        : TInputScreen;
   HotKeys                      : THotKeys;
   CUPSKeys                     : THotKeys;
   Key                          : Byte;
   Ecc, ArgPcenter, Inc         : Real;
   ArgPCenterDeg, IncDeg        : Real;
   MassA, MassB, SmAxis, Period : Real;
   Lag                          : Integer;  {Delay, in milliseconds,
                                             between plotted points.}
   ShowCUPSKeys                 : Boolean;
   DataIn                       : Boolean;
   InputAccepted                : Boolean;

   Cx, Cy                       : Real; {Coordinates of A in orbit animation.}
   Axo, Ayo                     : Real; {"Old" orbital coordinates.}
   xArc, yArc                   : Array[0..10] of Real;
                                  {Coordinates of preceding orbital arc.}
   OrbitArcNum                  : Integer; {Current number in these arrays.}
   ArcColor                     : Array[0..10] OF Word;
   xVArc, yVArc                 : Array[0..10] of Real; {Preceding velocity curve.}
   VArcNum                      : Integer; {Current number in these arrays.}
   x, y, z, vx, vy, vz          : Real;  {Orbital coordinates.}
   RVMax, RVMin, DelRV          : Real; {Radial velocity parameters.}
   RVx, RVo                     : Real; {Radial velocity coordinates.}
   Action                       : Boolean;
   PlotOrbit                    : Boolean;
   LightMenu                    : Boolean; {Menu highlighted when true.}
   ExitRequest                  : Boolean;

PROCEDURE HandleSBMenu; FORWARD;

{------------------------------------------------------------------------}

PROCEDURE SetUpSBMenu;
BEGIN
   With SBMenu DO
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
      Column(3, 'Data');
       row(3,1, 'Enter data');
      Column(4, 'Orbits');
       row(4,1, 'See more animation');
   END;
   SBMenu.Display;
END;
{------------------------------------------------------------------------}

PROCEDURE SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      HotKeys.Init(5);
      Key[1] := 'F1-Help';
      Key[2] := 'F2-Stop';
      Key[3] := 'F5-Faster';
      Key[4] := 'F6-Slower';
      Key[5] := 'F10-Menu';
   END;
 {  HotKeys.BackColor := Blue;
   HotKeys.Display; }
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

Procedure SBData(VAR Ecc, ArgPcenter, Inc: Real);
BEGIN
   WITH SBDat DO
   BEGIN
      Init;
      DefineInputPort(0.1, 0.9, 0.1, 0.9);
      LoadLine('');
      LoadLine('             Data for a Spectroscopic Binary');
      LoadLine('');
      LoadLine('   The velocity curve for star A will be plotted.');
      LoadLine('');
      LoadLine('   The mass of star A          = {      } solar masses');
      LoadLine('   The mass of star B          = {      } solar masses');
      LoadLine('');
      LoadLine('   The semimajor axis of the ');
      LoadLine('              relative orbit   = {      } astronomical units');
      LoadLine('');
      LoadLine('The eccentricity, e:        0 <= {      } < 1');
      LoadLine('');
      LoadLine('The argument of periastron: 0 <= {      } < 360 degrees');
      LoadLine('');
      LoadLine('The inclination, i:         0 <  {      } <= 90 degrees');
      LoadLine('');
      LoadLine('               [  OK  ]              [Cancel]      ');
      SetNumber(1, MassA);
      SetNumberLimits(1, 0, 10000);
      SetNumber(2, MassB);
      SetNumberLimits(2, 0, 10000);
      SetNumber(3, SmAxis);
      SetNumberLimits(3, 0.00001, 100000);
      SetNumber(4, Ecc);
      SetNumberLimits(4, 0, 0.9999);
      SetNumber(5, ArgPCenterDeg);
      SetNumberLimits(5, 0, 359.999);
      SetNumber(6, IncDeg);
      SetNumberLimits(6, 0.0001, 90);
      AcceptScreen;
      IF Canceled THEN InputAccepted := False ELSE InputAccepted := True;
      MassA := GetNumber(1);
      MassB := GetNumber(2);
      SmAxis := GetNumber(3);
      Ecc := GetNumber(4);
      ArgPcenterDeg := GetNumber(5);
      ArgPcenter := ArgPcenterDeg*3.14159/180;
      IncDeg := GetNumber(6);
      Inc := IncDeg*3.14159/180;
      Period := 2*Pi*(SmAxis/Gk)*Sqrt(SmAxis/(MassA + MassB));
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
      1: ShowHelp('HELPBins.HLP', 'Spectro Hello');
      2: BEGIN
            IF Action = True THEN
            BEGIN
               Action := False;
               WITH HotKeys DO Key[2] := 'F2-Run';
               HotKeys.BackColor := Red;
               HotKeys.Display;
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
               HotKeys.Clear;
               ShowCUPSKeys := True;
               CUPSKeys.Display;
               IF SBMenu.Chosen THEN HandleSBMenu;
         END;
   END;
END;

{------------------------------------------------------------------------}


Procedure Kepler(m, ec0, es0: Real; VAR x, c, s, fp: Real);
{Solves Kepler's equation for elliptic motion.}
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
{Solves the initial value problem.}
VAR
   r0, a, n, m, f, g, vf, vg, ec0, es0, x, c, s, fp: Real;
begin
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

Procedure Project(ArgPcenter, Inc, xin, yin, zin: Real;
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
END;

{------------------------------------------------------------------------}

Procedure Arrow(Cx, Cy, Angle, Long: Real);
VAR
   x1, y1, x2, y2: Real;
   xSc, ySc: Integer;
BEGIN
   SetColor(White);
   x1 := Cx + Long*cos(Angle);
   y1 := Cy + Long*sin(Angle);
   PlotLine(Cx, Cy, x1, y1);
   x2 := x1 - 0.1*Long*cos(Angle + 0.785);
   y2 := y1 - 0.1*Long*sin(Angle + 0.785);
   PlotLine(x1, y1, x2, y2);
   x2 := x1 - 0.1*Long*cos(Angle - 0.785);
   y2 := y1 - 0.1*Long*sin(Angle - 0.785);
   PlotLine(x1, y1, x2, y2);
   Map(x1, y1, xSc, ySc);
   OutTextxy(xSc+3, ySc, 'Viewer');
END;

{------------------------------------------------------------------------}

Procedure SetUpSpectrum(RVMin, RVMax: Real);
VAR
   ZeroPoint: Real;
   xSc, ySc: Integer;
   ScaleFactor: Real;
   RVCoord: Real;
   MassFunction: Real;
   MaxPoint, MinPoint: Integer;
   RVString: String;
BEGIN
   SetColor(Yellow);
   HideCursor;
   PlotLine(0.2, 1, 4.2, 1);
   PlotLine(0.2, 2, 4.2, 2);
   ZeroPoint := 4 - 3.6*RVMax/(RVMax - RVMin);
   PlotLine(ZeroPoint, 0.7, ZeroPoint, 1);
   PlotLine(ZeroPoint, 2, ZeroPoint, 2.3);
   Map(1.5, 2.6, xSc, ySc);
   OutTextxy(xSc, ySc, 'Spectral line');
   Map(0.5, 2.2, xSc, ySc);
   SetColor(LightRed);
   OutTextxy(xSc, ySc, 'Red');
   Map(3.5, 2.2, xSc, ySc);
   SetColor(LightBlue);
   OutTextxy(xSc, ySc, 'Blue');
   SetColor(White); 
   PlotLine(0.2, 0.5, 4.2, 0.5);
   ScaleFactor := 29.785*MassB/Sqrt(SmAxis*(MassA + MassB));
   MaxPoint := Trunc(RVMax*ScaleFactor);
   RVCoord := 4 + (- RVMax + MaxPoint/ScaleFactor)*3.6/(RVMax - RVMin);
   PlotLine(RVCoord, 0.5, RVCoord, 0.6);
   Map(RVCoord-1, 0.4, xSc, ySc);
   RVString := NumStr(MaxPoint, 5, 0);
   OutTextxy(xSc, ySc, Concat(RVString, ' km/sec'));
   MinPoint := Trunc(RVMin*ScaleFactor);
   RVCoord := 4 + (- RVMax + MinPoint/ScaleFactor)*3.6/(RVMax - RVMin);
   PlotLine(RVCoord, 0.5, RVCoord, 0.6);
   Map(RVCoord-0.5, 0.4, xSc, ySc);
   RVString := NumStr(MinPoint, 5, 0);
   OutTextxy(xSc, ySc, RVString);
   MassFunction := MassB*Sin(Inc)*Sqr(MassB*Sin(Inc)/(MassA + MassB));
   RVString := NumStr(MassFunction, 7, 4);
   Map(3.5, 3, xSc, ySc);
   OutTextxy(xSc, ySc, Concat('Mass function = ', RVString));
   ShowCursor;
END;

{------------------------------------------------------------------------}

Procedure SpectralLine(RVMin, RVMax, RadV: Real; NColor: Integer);
VAR
   RVCoord: Real;
BEGIN
   SetColor(NColor);
   RVCoord := 4 + (- RVMax + RadV)*3.6/(RVMax - RVMin);
   Plotline(RVCoord, 1.05, RVCoord, 1.95);
END;

{------------------------------------------------------------------------}

PROCEDURE FindRVRange(Ecc, ArgPcenter, Inc: Real; VAR RVMin, RVMax: Real);
VAR
   xt, yt, zt, vxt, vyt, vzt: Real;
   t, x, y, z, vx, vy, vz: Real;
   nr: Integer;
   th: Real;
BEGIN
   x := (1 - Ecc); vx := 0;
   y := 0; vy := Sqrt((1 + Ecc)/(1 - Ecc));
   z := 0; vz := 0;
   t := 0;
   Project(ArgPcenter, Inc, vx, vy, vz, vxt, vyt, vzt);
   nr := 3; th := - ArgPcenter; RotateAxes(nr, th, x, y, z, xt, yt, zt);
   RVMax := vzt;
   RVMin := RVMax;
   REPEAT
      t := t + dt;
      Ivp(x, y, vx, vy, x, y, vx, vy);
      z := 0; vz := 0;
      Project(ArgPcenter, Inc, vx, vy, vz, vxt, vyt, vzt);
      IF RVMax < vzt THEN RVMax := vzt;
      IF RVMin > vzt THEN RVMin := vzt;
   UNTIL t > 6.3;
END;

{------------------------------------------------------------------------}

PROCEDURE DrawLightCurveAxis(RVMin, RVMax, DelRV: Real);
VAR
   xAxis: Real;
   xSc, ySc: Integer;
   RVString: String;
   ScaleFactor, MaxPoint, RVCoord, MinPoint: Real;
BEGIN
   HideCursor;
   SetColor(Yellow);
   xAxis := - RVMax*DelRV;
   PlotLine(0, xAxis, 10, xAxis);
   PlotLine(2*Pi, xAxis, 2*Pi, xAxis + 0.2);
   Map(2*Pi- 1.1, xAxis - 0.2, xSc, ySc);
   RVString := NumStr(Period, 7, 3);
   OutTextxy(xSc, ySc, 'Period:' + RVString + ' days');
   Map(2.5, 0, xSc, ySc);
   OutTextxy(xSc, ySc, 'RADIAL VELOCITY CURVE');
   PlotLine(0.1, 0, 0.1, -3);
   ScaleFactor := 29.785*MassB/Sqrt(SmAxis*(MassA + MassB));
   MaxPoint := Trunc(RVMax*ScaleFactor);
   RVCoord := (- RVMax + MaxPoint/ScaleFactor)*DelRV;
   PlotLine(0.1, RVCoord, 0.2, RVCoord);
   Map(0.2, RVCoord, xSc, ySc);
   RVString := NumStr(MaxPoint, 5, 0);
   OutTextxy(xSc, ySc, Concat(RVString, ' km/sec'));
   MinPoint := Trunc(RVMin*ScaleFactor);
   RVCoord := (- RVMax + MinPoint/ScaleFactor)*DelRV;
   PlotLine(0.1, RVCoord, 0.2, RVCoord);
   Map(0.2, RVCoord, xSc, ySc);
   RVString := NumStr(MinPoint, 5, 0);
   OutTextxy(xSc, ySc, RVString);
   ShowCursor;
END;
   

{------------------------------------------------------------------------}

PROCEDURE DrawOrbitArc;
VAR
   i: Integer;
BEGIN
   FOR i := 1 TO OrbitArcNum DO
   BEGIN
      SetColor(ArcColor[i]);
      PlotLine(xArc[i-1], yArc[i-1], xArc[i], yArc[i]);
   END;
END;

PROCEDURE DrawVCurveArc;
VAR
   i: Integer;
BEGIN
   FOR i := 1 TO VArcNum DO PlotLine(xVArc[i-1], yVArc[i-1], xVArc[i], yVArc[i]);
END;
   

PROCEDURE SetUpScreens;
VAR
   Long: Real;
   nr: Integer;
   xSc, ySc: Integer;
   th: Real;
   xt, yt, zt, vxt, vyt, vzt: Real;
BEGIN
   SBMenu.Display;
   WITH HotKeys DO Key[2] := 'F2-Stop';
               HotKeys.BackColor := Blue;
               HotKeys.Display;
   SetBkColor(Black);
   GraphBackColor := Black;
   OpenViewPort(1);
   GraphBackColor := DarkGray;
   OpenViewPort(2);
   SelectScale(1);
   SelectViewPort(1);
   SetColor(White);
   HideCursor;
   Map(6, 0.2 ,xSc, ySc);
   OutTextxy(xSc, ySc, 'Relative view of the orbit');
   PlotOrbit := True;
   Action := True;
   LightMenu := False;
   OrbitArcNum := 0;
   VArcNum := 0;
   Lag := 200;
   FindRVRange(Ecc, ArgPcenter, Inc, RVMin, RVMax);
   DelRV := 2.8/(RVMax - RVMin);
   SetUpSpectrum(RVMin, RVMax);
   x := (1 - Ecc); vx := 0;
   y := 0; vy := Sqrt((1 + Ecc)/(1 - Ecc));
   z := 0; vz := 0;
   Project(ArgPcenter, Inc, vx, vy, vz, vxt, vyt, vzt);
   nr := 3; th := - ArgPcenter; RotateAxes(nr, th, x, y, z, xt, yt, zt);
   RVo := vzt;
   Rvx := 0.1;
   SpectralLine(RVMin, RVMax, RVo, LightRed);
{Initial coordinates of the center of mass and the stars.}
   Cx := 7; Cy := 1.7;   {Coordinates of star A.}
   Long := 1.5;
   Arrow(Cx, Cy, - ArgPcenter+ 3.14159/2, Long);
   Axo := Cx + x; Ayo := Cy;
   xArc[0] := Axo; yArc[0] :=Ayo; ArcColor[0] := LightRed;
   xVArc[0] :=  RVx; yVArc[0] := (RVo-RVMax)*DelRV;
   SetColor(Yellow); PlotSymbol(Cx, Cy, 'o');
   SelectViewPort(2); SelectScale(2);
   DrawLightCurveAxis(RVMin, RVMax, DelRV);
   ShowCursor;
END;

{----------------------------------------------------------------------}

PROCEDURE UpDateScreens;
VAR
   xt, yt, zt, vxt, vyt, vzt: Real;
   Axn, Ayn, RVn: Real;
   i: Integer;
BEGIN
  { Repeat }
      IF Action THEN
      BEGIN
        RVx := RVx + dt;
         SetColor(Black);
         Ivp(x, y, vx, vy, x, y, vx, vy);
         z := 0; vz := 0;
         Project(ArgPcenter, Inc, vx, vy, vz, vxt, vyt, vzt);
         RVn := vzt;
         SelectViewPort(1); SelectScale(1);
   {Draw spectral line.}
         SpectralLine(RVMin, RVMax, RVo, Black);
         IF RVn < 0 THEN SpectralLine(RVMin, RVMax, RVn, LightRed)
                    ELSE SpectralLine(RVMin, RVMax, RVn, LightBlue);
         IF RVx >= 0.1+2*3.14159 THEN
         BEGIN
   {Renew radial velocity curve.}
            SelectScale(2);
            SelectViewPort(2); SetColor(DarkGray);
            PlotSymbol(RVx-dt, (RVo-RVMax)*DelRV, '*');
            SetColor(White);
            VArcNum := 0;
            RVx := RVx - 2*Pi;
            xVArc[0] :=  RVx-dt; yVArc[0] := (RVo-RVMax)*DelRV;
         END;
   {Plot radial velocity curve.}
            SelectViewPort(2); SelectScale(2);
            SetColor(DarkGray);
            PlotSymbol(RVx-dt, (RVo-RVMax)*DelRV, '*');
            SetColor(Yellow);
            PlotLine(RVx-dt, (RVo-RVMax)*DelRV, RVx, (RVn-RVMax)*DelRV);
            PlotSymbol(RVx, (RVn-RVMax)*DelRV, '*');
            RVo := RVn;
            IF VArcNum < 10 THEN
            BEGIN
               VArcNum := VArcNum + 1;
               xVArc[VArcNum] := RVx; yVArc[VArcNum] := (RVn-RVMax)*DelRV;
            END
            ELSE
            BEGIN
               FOR i := 1 TO 10 DO
               BEGIN
                  xVArc[i-1] := xVArc[i];
                  yVArc[i-1] := yVarc[i];
               END;
               xVArc[10] := RVx; yVArc[10] := (RVn-RVMax)*DelRV;
            END;
         DrawVCurveArc;
         Axn := Cx + x; Ayn := Cy + y;
         IF OrbitArcNum < 10 THEN
         BEGIN
            OrbitArcNum := OrbitArcNum + 1;
            xArc[OrbitArcNum] := Axn; yArc[OrbitArcNum] := Ayn;
           IF RVn < 0 THEN ArcColor[VArcNum] := LightRed
                      ELSE ArcColor[VArcNum] := LightBlue;
         END
         ELSE
         BEGIN
            FOR i := 1 TO 10 DO
            BEGIN
               xArc[i-1] := xArc[i];
               yArc[i-1] := yarc[i];
               ArcColor[i-1] := ArcColor[i];
            END;
            xArc[10] := Axn; yArc[10] := Ayn;
           IF RVn < 0 THEN ArcColor[10] := LightRed
                      ELSE ArcColor[10] := LightBlue;
         END;
   {Plot orbit.}
         SelectViewPort(1); SelectScale(1);
         SetColor(Black); PlotSymbol(Axo, Ayo, '*');
         SetColor(LightBlue); PlotSymbol(Axn, Ayn, '*');
         DrawOrbitArc;
         Axo := Axn; Ayo := Ayn;
      END;
      Delay(Lag);
      LightMenu := False;
END;

{------------------------------------------------------------------------}

PROCEDURE GetData;
BEGIN
   SetBkColor(Black);
   SBData(Ecc, ArgPcenter, Inc);
   IF InputAccepted THEN
   BEGIN
      SBMenu.RowActivate(4,1, True);
      ShowCUPSKeys := False;
      SetUpScreens;
   END
   ELSE
   BEGIN
      SBMenu.RowActivate(4,1, False);
      LightMenu := True;
      CUPSKeys.Display;
      ShowCUPSKeys := True;
   END;
END;

{------------------------------------------------------------------------}
 
PROCEDURE HandleSBMenu;

BEGIN
   With SBMenu DO
   CASE ColChosen OF
      1: CASE RowChosen OF
            1: BEGIN
                  AboutCUPS;
                  LightMenu := True;
               END;
            2: BEGIN
                  ShowHelp('HELPBins.HLP', 'Spectro Hello');
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
                  ShowHelp('HELPBins.HLP', 'Spectro GeneralA');
                  LightMenu := True;
               END;
            2: BEGIN
                  ShowHelp('HELPBins.HLP', 'Spectro GeneralB');
                  LightMenu := True;
               END;
         END;
      3: BEGIN
            GetData;
          {  IF InputAccepted THEN
            BEGIN
               SBMenu.RowActivate(4,1, True);
               ShowCUPSKeys := False;
               SetUpScreens;
            END
            ELSE SBMenu.RowActivate(4,1, False);
            LightMenu := True;
            CUPSKeys.Display;
            ShowCUPSKeys := True; }
         END;
      4: BEGIN
            Action := True;
            PlotOrbit := True;
            ShowCUPSKeys := False;
            CUPSKeys.Clear;
            WITH HotKeys DO Key[2] := 'F2-Stop';
            HotKeys.BackColor := Blue;
            HotKeys.Display;
         END;
      
    END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleCUPSKeys(Key : Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HELPBins.HLP', 'Spectro Hello');
      2: IF SBMenu.Chosen THEN HandleSBMenu;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpDefaultInput;
BEGIN
   DefineViewPort(1, 0, 1, 0.56, 0.94);
   DefineViewPort(2, 0, 1, 0.06, 0.54);
   DefineScale(1, 0, 10, 0, 3.2);
   DefineScale(2, 0, 7, -3.2, 0.2);
   DataIn := False;
   LightMenu := False;
   Action := False;
   SetBkColor(Black);
   MassA := 1;
   MassB := 1;
   SmAxis := 0.1;
   Ecc := 0.5;
   ArgPCenterDeg := 45;
   IncDeg := 90;
   ExitRequest := False;
   SetUpCUPSKeys;
   ShowCUPSKeys := True;
END;

{------------------------------------------------------------------------}

BEGIN   {Main Program.}
{  CUPSinit;}
   SetUpSBMenu;
   SetUpHotKeys;
   SetUpDefaultInput;
   CUPSKeys.Display;
   ShowHelp('HELPBins.HLP', 'Spectro Hello');
   GetData;
   REPEAT
      CheckForEvents;
      IF HotKeys.Pressed(Key) THEN HandleHotKeys(Key);
      IF ShowCUPSKeys THEN
            IF CUPSKeys.Pressed(Key) THEN HandleCUPSKeys(Key);
      IF (Action) AND (PlotOrbit) THEN UpDateScreens;
      IF LightMenu THEN
      BEGIN
         LightMenu := False;
         IF SBMenu.Chosen THEN HandleSBMenu;
      END
      ELSE
         IF SBMenu.Activated THEN HandleSBMenu;
   UNTIL ExitRequest;
   SBMenu.done;
{  CUPSdone;}
END;

END.