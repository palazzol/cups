    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1994.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program to display the animated motion        *)
    (*       ofthe "Sun, "Jupiter" and up to 100           *)
    (*       "asteroids."                                  *)
    (*                                                     *)
    (*******************************************************)

unit Astroids;

{$M 64000,0,327680}
{$O+}
{$F+}
interface
procedure RunAstroids;

implementation
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;


CONST
   MassSun = 1;  {Mass of the Sun.}
   Nmax = 5;  {Number of terms in the power series for Jupiter.}
   Neq = 6;    {Number of differential equations for each asteroid.}
   NeqA = 10;  {Number of equations for one asteroid and Jupiter.}
   dt = 0.5;   {Time between plotted positions.}
   Eps = 0.0001;  {Distance within which the inverse square law is replaced
                   by a constant force.}
  

TYPE
   Vector = Array[1..6] OF Real;
   Vector1 = ARRAY[0..12] of real;
   Vector3 = ARRAY[1..3] of real;
   Vector5 = Array[1..5] of Real;
   VectorA = Array[1..NeqA] of Real;
   Matrix = Array[0..100] of Vector;
   ElementArray = Array[1..500, 1..5] of Real;

TYPE
   PointRecord = Record
                    x, y, z : integer;
                 END;

VAR
   Filename, FileDirectory: String;
   MakeFile, FileOpen: Boolean;
   F1: File of PointRecord;
   Points: PointRecord;
                                                                             
VAR
   i                                 : Integer;
   AstMenu                           : TMenu;
   MassJup                           : Real;   {Mass of Jupiter.}
   PeriodJup                         : Real;   {Period of Jupiter.}
   nStates                           : Matrix;   {Matrix storing the States of all bodies.}
   yA                                : VectorA;
   y                                 : Vector;
   nA                                : Integer; {Total number of asteroids.}
   Jupa, Jupe, Jupf0, Jupf0Deg       : real;  {Orbital elements of Jupiter.}
   Da, De, Di, DiDeg, Asta           : Real; {Orbital bounds for elements of the asteroids.}
   aMax, aMin, eMax, eMin, iMax, iMin: Real;  {Bounds for the elements}
   NodeMax, NodeMin, AphMax, AphMin  : Real;    {of one asteroid.}
   ElementMax                        : Real;
   Elements                          : ElementArray;
   ElementTime                       : Real; {Time interval for plotting elements.}
   SeeAllAsteroids                   : Boolean;
   LightMenu                         : Boolean;
   ExitRequest                       : Boolean;
   HotKeys                           : THotKeys;
   CUPSKeys                          : THotKeys;
   Key                               : Byte;
   ShowCUPSKeys                      : Boolean;
   Action, PlotOrbit                 : Boolean;
   AstDataOK                         : Boolean;
   InputCanceled                     : Boolean;
   DrawMars                          : Boolean;
   MarsRadius                        : Real;
   PResonance, QResonance            : Real;

PROCEDURE HandleAstMenu; FORWARD;

{------------------------------------------------------------------------}

PROCEDURE SetUpASTMenu;
BEGIN
   With ASTMenu DO
   BEGIN
      Init;
      Column(1, 'File');
       row(1,1, 'About CUPS');
       row(1,2, 'About the program');
       row(1,3, 'Configuration');
       row(1,4, 'General help');
       row(1,5, 'Exit this module');
      Column(2, 'Data');
       row(2,1, 'Enter data');
      Column(3, 'See orbits');
       row(3,1, 'See the asteroid motion');
      Column(4, 'Asteroid');
       row(4,1, 'Select an asteroid');
      Column(5, 'Elements');
       row(5,1, 'Semimajor axis');
       row(5,2, 'Eccentricity');
       row(5,3, 'Argument of perihelion');
       row(5,4, 'Inclination');
       row(5,5, 'Longitude of ascending node');
       row(5,6, 'Extend the time');
   END;
   {ASTMenu.Display;}
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpAstMenu1;
VAR i: Integer;
BEGIN
  { AstMenu.Display;}
  { AstMenu.RowActivate(3,1, False);}
   AstMenu.RowActivate(4,1, False);
   FOR i := 1 TO 7 DO
   AstMenu.RowActivate(5,i, False);
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpAstMenu2;
VAR i: Integer;
BEGIN
   AstMenu.Display;
  { AstMenu.RowActivate(2,1, False);}
   {AstMenu.RowActivate(3,1, False);}
   AstMenu.RowActivate(4,1, False);
     FOR i := 1 TO 7 DO
   AstMenu.RowActivate(5,i, True);
   AstMenu.RowActivate(5,4, False);
   AstMenu.RowActivate(5,5, False);
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpAstMenu3;
VAR i: Integer;
BEGIN
   AstMenu.Display;
   AstMenu.RowActivate(4,1, True);
   FOR i := 1 TO 7 DO
   AstMenu.RowActivate(5,i, False);
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpAstMenu4;
VAR i: Integer;
BEGIN
   AstMenu.Display;
  { AstMenu.RowActivate(2,1, False); }
   {AstMenu.RowActivate(3,1, False);}
   AstMenu.RowActivate(4,1, False);
   FOR i := 1 TO 7 DO
   AstMenu.RowActivate(5,i, True);
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      HotKeys.Init(2);
      Key[1] := 'F2-Pause';
      Key[2] := 'F10-Menu';
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

{-------------------------------------------------------------------------}

PROCEDURE HandleHotKeys(key: Byte; ActionIn: Boolean; VAR PlotOrbit, Action: Boolean);
BEGIN
   CASE Key OF
      1: BEGIN
            IF ActionIn = True THEN
            BEGIN
               Action := False;
               WITH HotKeys DO Key[1] := 'F2-Run';
               HotKeys.BackColor := Red;
               HotKeys.Display;
            END
            ELSE
            BEGIN
               Action := True;
               WITH HotKeys DO Key[1] := 'F2-Pause';
               HotKeys.BackColor := Blue;
               HotKeys.Display;
            END;
         END;
      2: BEGIN
            PlotOrbit := False;
            HotKeys.Clear;
            ShowCUPSKeys := True;
            CUPSKeys.Display;
            LightMenu := True;
            SetUpAstMenu3;
         END;
   END;
END;

{-------------------------------------------------------------------------}

Procedure AstData1(VAR MassJup, Jupa, Jupe, Jupf0: Real);
CONST
   RBNames: String = '12345';
VAR
   AstDat1: TInputScreen;
BEGIN
   ClearMuppetPort;
   InputCanceled := False;
   WITH AstDat1 DO
   BEGIN
      Init;
      DefineInputPort(0.05, 0.95, 0.05, 0.95);
      LoadLine('             N Asteroids in Resonance with Jupiter.');
      LoadLine('');
      LoadLine('The model consists of the Sun, ''Jupiter'',  and n asteroids, which');
      LoadLine('which have negligible mass.');
      LoadLine('');
      LoadLine('The mass of the Sun is 1 unit of mass.');
      LoadLine('              Enter the mass of Jupiter: {      } solar masses');
      LoadLine('');
      LoadLine('Enter the orbital elements of Jupiter:');
      LoadLine('The major axis will be horizontal on the screen, therefore the');
      LoadLine('inclination of Jupiter''s orbit is zero.');
      LoadLine('        Semimajor axis (greater than 1): {      } astronomical units');
      LoadLine('                   Eccentricity e:  0 <= {      } < 1');
      LoadLine('True anomaly at the start of the motion: {      } degrees');
      LoadLine('');
      LoadLine('Would you like to see a reference circular orbit plotted?');
      LoadLine('It might, for instance, represent the orbit of Mars.');
      LoadLine('      No #1  Yes #1   Radius {      } astronomical units.');
      LoadLine('');
      LoadLine('                 [  OK  ]            [Cancel]');
      SetNumber(1, MassJup);
      SetNumberLimits(1, 0, 10000);
      SetNumber(2, Jupa);
      SetNumberLimits(2, 1, 10000);
      SetNumber(3, Jupe);
      SetNumberLimits(3, 0, 0.9999);
      SetNumber(4, Jupf0Deg);
      SetNumberLimits(4, 0, 359.999);
      SetNumber(7, MarsRadius);
      SetNumberLimits(7, 0, 100);
      Accept;
      IF Canceled THEN InputCanceled := True
      ELSE
      BEGIN
         MassJup := GetNumber(1);
         Jupa := GetNumber(2);
         Jupe := GetNumber(3);
         Jupf0Deg := GetNumber(4);
         Jupf0 := 3.14159*Jupf0Deg/180;     {Convert to radians.}
         IF GetRadioButton(RBNames[1]) = 1 THEN DrawMars := False
                                           ELSE DrawMars := True;
         MarsRadius := GetNumber(7);
         PeriodJup := Sqrt(Jupa*Jupa*Jupa/(1+MassJup));
      END;
      ClearInputPort;
      Done;
   END;
END;

{-------------------------------------------------------------------------}

Procedure AstData2(MassSun, MassJup, Jupa: Real;
                  VAR nA: Integer;
                  VAR Asta, De, Di: Real);
CONST
   RBNames: String = '12345';
VAR
   p, q, RealnA: Real;
   Query, FileName: String;
   AstDat2: TInputScreen;
BEGIN
   InputCanceled := False;
   WITH AstDat2 DO
   BEGIN
      Init;
      DefineInputPort(0.1, 0.95, 0.05, 0.9);
      LoadLine('                     Data for the Asteroids');
      LoadLine('');
      LoadLine('         Number of asteroids: 1 <= {      } <= 100');
      LoadLine('');
      LoadLine('      For a resonance ''p/q'', p revolutions of an asteroid will');
      LoadLine('      take the same time as q revolutions of Jupiter.');
      LoadLine('      Enter positive integers, p = {      }');
      LoadLine('                               q = {      }');
      LoadLine('');
      LoadLine('      Eccentricities will lie at random between 0 and De');
      LoadLine('                    Enter De: 0 <= {      } < 1');
      LoadLine('');
      LoadLine('      Inclinations will lie at random between 0 and Di');
      LoadLine('                    Enter Di: 0 <= {      } < 180 degrees');
      LoadLine('      Do you want to store the animated motion in a file for');
      LoadLine('      later playback?    No #1   Yes #1');
      LoadLine('                       File name: "Astroid1" ');
      LoadLine('');
      LoadLine('                 [  OK  ]                [Cancel]');
      SetNumber(1, NA);
      SetNumberLimits(1, 1, 100); 
      SetNumber(2, PResonance);
      SetNumberLimits(2, 1, 10000);
      SetNumber(3, QResonance);
      SetNumberLimits(3, 1, 10000);
      SetNumber(4, De);
      SetNumberLimits(4, 0, 0.9999);
      SetNumber(5, DiDeg);
      SetNumberLimits(5, 0, 179.999);
      Accept;
      IF Canceled THEN InputCanceled := True
      ELSE
      BEGIN
         RealnA := GetNumber(1);
         nA := Trunc(RealnA + 0.001);
         pResonance := GetNumber(2);
         qResonance := GetNumber(3);
         Asta := Jupa*Exp(Ln((qResonance*qResonance)
                            /(pResonance*pResonance))/3);
         De := GetNumber(4);
         DiDeg := GetNumber(5);
         Di := DiDeg*Pi/180;
         IF GetRadioButton(RBNames[1]) = 1 THEN MakeFile := False
                                           ELSE MakeFile := True;
         FileName := GetString(8);
         IF MakeFile THEN
         BEGIN
            Assign(F1, Concat(Filename, '.POS'));
            ReWrite(F1);
         END;
      END;
      ClearInputPort;
      Done;
   END;
END;

{----------------------------------------------------------------------}

Procedure ElementInfo(VAR ElementTime, da: Real);
VAR
   AstDat3: TInputScreen;
BEGIN
   InputCanceled := False;
   WITH AstDat3 DO
   BEGIN
      Init;
      DefineInputPort(0.05, 0.95, 0.2, 0.8);
      LoadLine('                Information for Computing Elements');
      LoadLine('');
      LoadLine('   Values of the elements will be computed at 500 regularly spaces');
      LoadLine('times over an interval that you must specify.');
      LoadLine('');
      LoadLine('      Time interval for finding elements: {      } years');
      LoadLine('');
      LoadLine('   During the computation, the semimajor axis will be plotted. The');
      LoadLine('plotting range about its mean value has the default value of 0.3');
      LoadLine('astronomical units. To change this, enter an alternative value:');
      LoadLine('Range in the semimajor axis for plotting: {      } astronomical units');
      LoadLine('');
      LoadLine('               [  OK  ]                [Cancel]');
      SetNumber(1, ElementTime);
      SetNumberLimits(1, 1, 100000);
      SetNumber(2, dA);
      SetNumberLimits(2, 0, 10);
      Accept;
      IF Canceled THEN InputCanceled := True
      ELSE
      BEGIN
         ElementTime := GetNumber(1);
         da := GetNumber(2);
      END;
      ClearInputPort;
      Done;
   END;
END;


{----------------------------------------------------------------------}

Procedure InitialConditions(MassSun, MassJup, Jupa, Jupe, Jupf0,
                                Asta, De, Di: Real;
                            VAR nStates: Matrix);
VAR
   i: Integer;

Procedure StartJupiter(MassSun, MassJup, Jupa, Jupe, Jupf0: Real;
                       VAR State: Vector);
VAR
   Param: Real;
BEGIN
   Param := Jupa*(1 - Sqr(Jupe));
   State[1] := Param*Cos(Jupf0)/(1 + Jupe*Cos(Jupf0));
   State[2] := Param*Sin(Jupf0)/(1 + Jupe*Cos(Jupf0));
   State[3] := 0;
   State[4] := - Sqrt((MassSun + MassJup)/Param)*Sin(Jupf0);
   State[5] := Sqrt((MassSun + MassJup)/Param)*(Jupe + Cos(Jupf0));
   State[6] := 0;
END;

{-------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------}

Procedure StartAsteroid(MassSun, Asta, De, Di: Real;
                        VAR State: Vector);
VAR
   Ecc, Inc, Node, Aph, f0, Param: Real;
   xa, ya, za, vxa, vya, vza: Real;
   nr: Integer;
   Th: Real;

BEGIN
   Ecc := Random*De;
   Inc := Random*Di;
   Node := Random*2*3.14159;
   Aph := Random*2*3.14159;
   f0 := Random*2*3.14159;
   Param := Asta*(1 - Sqr(Ecc));
   xa := Param*Cos(f0)/(1 + Ecc*Cos(f0));
   ya := Param*Sin(f0)/(1 + Ecc*Cos(f0));
   za := 0;
   vxa := - Sqrt((MassSun)/Param)*Sin(f0);
   vya := Sqrt((MassSun)/Param)*(Ecc + Cos(f0));
   vza := 0;
   nr := 3; th := - Aph;
   RotateAxes(nr, th, xa, ya, za, xa, ya, za);
   RotateAxes(nr, th, vxa, vya, vza, vxa, vya, vza);
   nr := 1; th := - Inc;
   RotateAxes(nr, th, xa, ya, za, xa, ya, za);
   RotateAxes(nr, th, vxa, vya, vza, vxa, vya, vza);
   nr := 3; th := - Node;
   RotateAxes(nr, th, xa, ya, za, xa, ya, za);
   RotateAxes(nr, th, vxa, vya, vza, vxa, vya, vza);
   State[1] := xa;
   State[2] := ya;
   State[3] := za;
   State[4] := vxa;
   State[5] := vya;
   State[6] := vza;
END;

{-------------------------------------------------------------------------}

BEGIN   {Main Program for Procedure InitialConditions.}
   StartJupiter(MassSun, MassJup, Jupa, Jupe, Jupf0, nStates[0]);
   FOR i := 1 TO nA DO
      StartAsteroid(MassSun, Asta, De, Di, nStates[i]);
END;


{------------------------------------------------------------------------}

Procedure Advance;  {For asteroids.}

VAR
   r, v: Vector3;
   rAst, vAst: Vector3;
   f, g: Vector1;
   mu, t: real;
   n: integer;
   xOld, yOld, xNew, yNew: Real;
   Angle: Real;
   AstColor: Word;
   PossibleTime: Real; {Maximum time for the f and g series.}
   RemainingTime: Real; {Time left in the interval dt.}
   MoveDone: Boolean; {True when time dt is complete.}

{---------------------------------------------------------------------}
 
Procedure FandG(rin, vin: Vector3; mu: real;
                VAR f, g: Vector1; VAR PossibleTime: Real);
CONST
   TruncError = 0.00001; {Greatest trunctation error of the series.}
VAR
   hs: real;
   r, s: ARRAY[0..20] of real;
   n, k: integer;
BEGIN
   hs := Sqr(rin[2]*vin[3] - rin[3]*vin[2])
             + Sqr(rin[3]*vin[1] - rin[1]*vin[3])
             + Sqr(rin[1]*vin[2] - rin[2]*vin[1]);
{Initialize the series.}
   r[0] := Sqrt(Sqr(rin[1]) + Sqr(rin[2]) + Sqr(rin[3]));
   r[1] := (rin[1]*vin[1] + rin[2]*vin[2]
           + rin[3]*vin[3])/r[0];
   f[0] := 1; f[1] := 0;
   g[0] := 0; g[1] := 1;
   s[0] := 1/(r[0]*r[0]*r[0]);
   FOR n := 2 to Nmax + 2 DO
   BEGIN
      f[n] := 0; g[n] := 0;
      r[n] := hs*s[n-2];
      s[n-1] := 0;
      FOR k := 0 TO n-2 DO
      BEGIN
         f[n] := f[n] + f[k]*s[n-2-k];
         g[n] := g[n] + g[k]*s[n-2-k];
         r[n] := r[n] + mu*r[k]*s[n-2-k];
      END;
      f[n] := - mu*f[n]/(n*(n-1));
      g[n] := - mu*g[n]/(n*(n-1));
      r[n] := r[n]/(n*(n-1));
      FOR k := 0 to n-2 DO
         s[n-1] := s[n-1] + (n-1-k)*(r[k]*s[n-1-k] + 3*s[k]*r[n-1-k]);
      s[n-1] := - s[n-1]/(r[0]*(n-1));
   END;
   PossibleTime := Exp((Ln(TruncError) -
                   Ln(Abs(f[Nmax + 1]) + Abs(f[Nmax + 2]) +
                      Abs(g[Nmax + 1]) + Abs(g[Nmax + 2])))/(Nmax+1));
END;

{---------------------------------------------------------------------}

Procedure JupState(Rin, Vin: Vector3; Time: Real;
                    VAR Stateout: Vector);
VAR
   Fsum, Gsum, FVsum, GVsum: real;
   k: integer;
BEGIN
   Fsum := f[Nmax]; Gsum := g[Nmax];
   FOR k := 1 TO Nmax DO
   BEGIN
      Fsum := Time*Fsum + f[Nmax - k];
      Gsum := Time*Gsum + g[Nmax - k];
   END;
   FVsum := Nmax*f[Nmax]; GVsum := Nmax*g[Nmax];
   FOR k := 1 to Nmax-1 DO
   BEGIN
      FVsum := Time*FVsum + (Nmax - k)*f[Nmax - k];
      GVsum := Time*GVsum + (Nmax - k)*g[Nmax - k];
   END;
   FOR k := 1 TO 3 DO
   BEGIN
      Stateout[k] := Fsum*Rin[k] + Gsum*Vin[k];
      Stateout[k + 3] := FVsum*Rin[k] + GVsum*Vin[k];
   END;
END;

{---------------------------------------------------------------------}

Procedure JupPosition(Rin, Vin: Vector3; Time: Real;
                    VAR Rout: Vector3);
VAR
   Fsum, Gsum: real;
   k: integer;
BEGIN
   Fsum := f[Nmax]; Gsum := g[Nmax];
   FOR k := 1 TO Nmax DO
   BEGIN
      Fsum := Time*Fsum + f[Nmax - k];
      Gsum := Time*Gsum + g[Nmax - k];
   END;
   FOR k := 1 TO 3 DO
      Rout[k] := Fsum*Rin[k] + Gsum*Vin[k];
END;

{---------------------------------------------------------------------}

Procedure Fun(x: real; y: Vector; VAR z: Vector);
VAR
   n: integer;
   rStar, r1: Vector3;
   Dist1, Dist2, Dist3: real;
BEGIN
   JupPosition(rAst, vAst, x, r1);
   Dist1 := 0; Dist2 := 0; Dist3 := 0;
      FOR n := 1 TO 3 DO
      BEGIN
         rStar[n] := y[n];
         Dist1 := Dist1 + Sqr(rStar[n]);
         Dist2 := Dist2 + Sqr(rStar[n] - r1[n]);
         Dist3 := Dist3 + Sqr(r1[n]);
      END;
      Dist1 := Sqrt(Dist1)*Dist1; IF Dist1 < Eps THEN Dist1 := Eps;
      Dist2 := Sqrt(Dist2)*Dist2; IF Dist2 < Eps THEN Dist2 := Eps;
      Dist3 := Sqrt(Dist3)*Dist3; IF Dist3 < Eps THEN Dist3 := Eps;
      FOR n := 1 TO 3 DO
      BEGIN
         z[n] := y[n+3];
         z[n+3] := - MassSun*rStar[n]/Dist1
                + MassJup*((r1[n] - rStar[n])/Dist2
                            - r1[n]/Dist3);
      END;
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

Procedure JupPlot(xg, yg: real; GalColor: integer);
BEGIN
   SetColor(GalColor);
   PlotSymbol(xg, yg, 'J');
END;

Procedure SunPlot(xg, yg: real; GalColor: integer);
BEGIN
   SetColor(GalColor);
   PlotSymbol(xg, yg, 'S');
END;

Procedure StarPlot(xg, yg: real; GalColor: integer);
var x,y : integer;
BEGIN
   Map(xg,yg,x,y);
   PutPixel(x,y,galColor);
   PutPixel(x+1,y,galColor);
   PutPixel(x-1,y,galColor);
   PutPixel(x,y+1,galColor);
   PutPixel(x,y-1,galColor);
END;

{---------------------------------------------------------------------}

Procedure JupMove(RequiredTime: Real;
                  VAR PossibleTime: Real; VAR MoveDone: Boolean);
VAR
   n: integer;
BEGIN
   FOR n := 1 TO 3 DO
   BEGIN
      rAst[n] := nStates[0, n];
      vAst[n] := nStates[0, n+3];
   END;
   JupPlot(rAst[1], rAst[2], GraphBackColor);
   FandG(rAst, vAst, MassSun + MassJup, f, g, PossibleTime);
   IF PossibleTime >= RemainingTime THEN
   BEGIN
      PossibleTime := RemainingTime;
      MoveDone := True;
   END
   ELSE
      RemainingTime := RemainingTime - PossibleTime;
   JupState(rAst, vAst, PossibleTime, nStates[0]);
   JupPlot(nStates[0, 1], nStates[0, 2], 2);
END;

{---------------------------------------------------------------------}

Procedure AstMove(nStar: integer; PossibleTime: Real);
VAR
   tout, h: real;
BEGIN
{   SetColor(GraphBackColor);
   PlotSymbol(nStates[nStar, 1], nStates[nStar, 2], '.');}
   StarPlot(nStates[nStar, 1], nStates[nStar, 2], GraphBackColor );
   tout := 0;
   REPEAT
      Step(tout, PossibleTime - tout, 0.001,
           nStates[nStar], tout, h, nStates[nStar]);
   UNTIL Abs(PossibleTime - tout) < 0.001;
{   SetColor(AstColor);
   PlotSymbol(nStates[nStar, 1], nStates[nStar, 2], '.'); }
   StarPlot(nStates[nStar, 1], nStates[nStar, 2], AstColor);
END;

{---------------------------------------------------------------------}
VAR
   xSc, ySc: Integer;
BEGIN  {Main program for procedure advance.}
   MoveDone := False;
   RemainingTime := Dt;
   SunPlot(0, 0, Yellow);
   REPEAT
      JupMove(RemainingTime, PossibleTime, MoveDone);
      IF (MakeFile = True) AND (MoveDone = True) THEN
      BEGIN
         Points.x := Trunc(1000*nStates[0, 1]);
         Points.y := Trunc(1000*nStates[0, 2]);
         Points.z := Trunc(1000*nStates[0, 3]);
         Write(F1, Points);
      END;
      AstColor := 0;
      HideMouse;
      FOR n := 1 TO nA DO
      BEGIN
         AstColor := AstColor + 1; IF AstColor > 16 THEN AstColor := 1;
         AstMove(n, PossibleTime);
         IF (MakeFile = True) AND (MoveDone = True) THEN
         BEGIN
            Points.x := Trunc(1000*nStates[n, 1]);
            Points.y := Trunc(1000*nStates[n, 2]);
            Points.z := Trunc(1000*nStates[n, 3]);
            Write(F1, Points);
         END;
      END;
      ShowMouse;
   UNTIL MoveDone = True;
END;


{------------------------------------------------------------------------}

Procedure AnimateAsteroids(Jupa: Real);
VAR
   Scalex, Scaley: Integer;
   X1, Y1, X2, Y2: Integer;
   i: Integer;
   xOld, yOld, xNew, yNew, Angle: Real;
BEGIN
   SeeAllAsteroids := True;
   IF Jupa > Asta THEN
   BEGIN
      Scalex := Trunc((7/5)*(Jupa + 1));
      Scaley := Trunc(Jupa + 1);
   END
   ELSE
   BEGIN
      Scalex := Trunc((7/5)*(Asta + 1));
      Scaley := Trunc(Asta + 1);
   END;
      DefineScale(1, -Scalex, Scalex, -Scaley, Scaley);
      IF MakeFile THEN
      BEGIN
         Rewrite(F1);
         FileOpen := True;
         Points.x := Trunc(-1000*Scalex);
         Points.y := Trunc(1000*Scalex);
         Points.z := Trunc(-1000*Scaley);
         Write(F1, Points);
         Points.x := Trunc(1000*Scaley);
         Points.y := Trunc(nA);
         Points.z := Trunc(nA);
      Write(F1, Points);
   END;
      GraphBackColor := Black;
      {SetUpAstMenu3;}
      OpenViewPort(1);
      SelectScale(1);
      IF DrawMars THEN
      BEGIN
         SetColor(LightRed);
        { Map(0, 0, X1, Y1);
         Map(MarsRadius, 0, X2, Y2);
         Circle(X1, Y1, Abs(X2 - X1));}
         Angle := 0;
         xOld := MarsRadius; yOld := 0;
         FOR i := 1 TO 50 DO
         BEGIN
            Angle := Angle + Pi/25;
            xNew := MarsRadius*Cos(Angle);
            yNew := MarsRadius*Sin(Angle);
            PlotLine(xOld, yOld, xNew, yNew);
            xOld := xNew; yOld := yNew;
         END; 
      END;
      ShowCUPSKeys := False;
      CUPSKeys.Clear;
      SetUpHotKeys;
      PlotOrbit := True;
      Action := True;
END;

{--------------------------------------------------------------------}

Procedure ChooseAsteroid(Jupa: Real; nA: Integer; VAR yA: VectorA);
VAR
   xMouse, yMouse: Real;
   Inside: Boolean;
   xSc, ySc: Integer;
   nClose: Integer;
   Dist, MinDist: Real;
   i: Integer;
BEGIN
   SeeAllAsteroids := False;
   MinDist := 10;
   SelectViewPort(1);
   SelectScale(1);
   Message('                              Click on an asteroid');
   REPEAT
      WaitOnMouseClick;
      MousePosn(xMouse, yMouse, 1, 1, Inside);
      FOR i := 1 TO nA DO
      BEGIN
         Dist := Sqr(xMouse - nStates[i, 1]) + Sqr(yMouse - nStates[i, 2]);
         IF MinDist > Dist THEN
         BEGIN
            MinDist := Dist;
            nClose := i;
         END;
      END;
      IF MinDist >= Asta*Asta/100 THEN
      BEGIN
         Message('                Please click closer to an asteroid');
         Beep;
         Delay(500);
         Message('                              Click on an asteroid');
      END;
   UNTIL MinDist < Asta*Asta/100;
   FOR i := 1 TO 3 DO
   BEGIN
      yA[i] := nStates[nClose, i];
      yA[i+3] := nStates[nClose, i+3];
   END;
   FOR i := 1 TO 2 DO
   BEGIN
      yA[6+i] := nStates[0, i];
      yA[8+i] := nStates[0, i+3];
   END;
   Message('');
END;

{-------------------------------------------------------------------------}

Procedure FunA(x: real; yA: VectorA; VAR z: VectorA);
VAR
   n: integer;
   rStar, r1: Vector3;
   Dist1, Dist2, Dist3: real;
BEGIN
   Dist1 := yA[1]*yA[1] + yA[2]*yA[2] + yA[3]*yA[3];
   Dist2 := Sqr(yA[1] - yA[7]) + Sqr(yA[2] - yA[8]) + yA[3]*yA[3];
   Dist3 := yA[7]*yA[7] + yA[8]*yA[8];
   Dist1 := Dist1*Sqrt(Dist1);  IF Dist1 < Eps THEN Dist1 := Eps;
   Dist2 := Dist2*Sqrt(Dist2);  IF Dist2 < Eps THEN Dist2 := Eps;
   Dist3 := Dist3*Sqrt(Dist3);  IF Dist3 < Eps THEN Dist3 := Eps;
   z[1] := yA[4];  z[2] := yA[5]; z[3] := yA[6];
   z[4] := - MassSun*yA[1]/Dist1 + MassJup*((yA[7] - yA[1])/Dist2 - yA[7]/Dist3);
   z[5] := - MassSun*yA[2]/Dist1 + MassJup*((yA[8] - yA[2])/Dist2 - yA[8]/Dist3);
   z[6] := - MassSun*yA[3]/Dist1 + MassJup*(       - yA[3])/Dist2;
   z[7] := yA[9];  z[8] := yA[10];
   z[9] := - (MassSun + MassJup)*yA[7]/Dist3;
   z[10] := - (MassSun + MassJup)*yA[8]/Dist3;
END;

{-------------------------------------------------------------------------}


Procedure Step(xin, hin, tol: real; yin: VectorA;
               VAR xout, hout: real; VAR yout: VectorA);

VAR
   a, ch, ct:                      array[1..6] of real;
   b:                              array[1..6,1..5] of real;
   f:                              array[1..6] of vectorA;
   y, ytemp, te:                          VectorA;
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
   FunA(xin, yin, f[1]);
   ytemp := yin;
   repeat {until temax < tol}
      for k := 2 to 6 do
         begin
            x := xtemp + a[k]*h;
            y := ytemp;
            for n := 1 to neqA do
               begin
                  for l := 1 to k-1 do
                      y[n] := y[n] + h*b[k,l]*f[l,n];
               end;
            FunA(x, y, f[k]);
         end; {k loop}
      y := ytemp;
      for n := 1 to neqA do
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
      for n := 1 to neqA do
         if temax < te[n] then temax := te[n];
      htemp := h;
      h := 0.9*h*exp(ln(tol/temax)/5);
   until temax < tol; 
   yout := y;
   xout := xtemp + htemp;
   hout := h;
end;


{-------------------------------------------------------------------------}

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

Procedure FindElements(yA: VectorA; VAR Vec: Vector5);
{Calculates elements, given components of position and velocity.}
VAR
   x, yy, z, xv, yv, zv, r: Real;
   hx, hy, hz, hh: Real;
   ex, ey, ez, u, s, c: Real;
   Convert: Real;
BEGIN
   Convert := 360/(2*3.14159);
   x := yA[1];  yy := yA[2];  z := yA[3];
   xv := yA[4]; yv := yA[5]; zv := yA[6];
   hx := yy*zv - z*yv;
   hy := z*xv - x*zv;
   hz := x*yv - yy*xv;
   hh := Sqrt(hx*hx + hy*hy + hz*hz);
   r := Sqrt(x*x + yy*yy + z*z);
   ex := (yv*hz - zv*hy)/MassSun - x/r;
   ey := (zv*hx - xv*hz)/MassSun - yy/r;
   ez := (xv*hy - yv*hx)/MassSun - z/r;
   u := x*xv + yy*yv + z*zv;
   s := hx;                  c := -hy;
   Vec[5] := Convert*Atan2(s,c);   {Longitude of the ascending node.}
   s := Sqrt(hx*hx + hy*hy); c := hz;
   Vec[3] := Convert*Atan2(s,c);   {Inclination.}
   IF Abs(Vec[3]) < 0.1 THEN  {The orbit is planar.}
   BEGIN
      s := ey; c := ex; Vec[4] := Convert*Atan2(s,c);
   END
   ELSE
   BEGIN
      s := ez; c := (-ex*hy + ey*hx)/hh;
      Vec[4] := Convert*Atan2(s,c);  {Argument of perihelion.}
   END;
   Vec[2] := Sqrt(ex*ex + ey*ey + ez*ez);  {Eccentricity.}
   Vec[1] := 1/(2/r - (xv*xv + yv*yv + zv*zv)/MassSun);  {Semimajor axis.}
end;

{------------------------------------------------------------------------}

Procedure DrawAxes(xMin, xMax, yMin, yMax: Real);
VAR
   i, nTick: Integer;
   Tick, dTick, TickSize  : Real;
   MarkPeriods            : Real;
   xSc, ySc, xSd, ySd: Integer;
   Mark: String;
BEGIN
   HideCursor;
   SetColor(Yellow);
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
      {FOR i := 1 TO nTick DO} dTick := dTick/10;
   IF nTick < 0 THEN
      {FOR i := 1 TO -nTick DO} dTick := dTick*10;
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
   SetColor(White);
   MarkPeriods := xMin + PeriodJup;
   nTick := 1;
   IF MarkPeriods < xMax THEN
   REPEAT
        {  PlotLine(MarkPeriods, yMin - (yMax - yMin)/40,
               MarkPeriods, yMin + (yMax - yMin)/40);}
      PlotLine(MarkPeriods, yMax,
               MarkPeriods, yMax - (yMax - yMin)/50);
      SelectViewPort(4);
      Line(MapX(MarkPeriods), MapY(yMin)-25,
           MapX(MarkPeriods), MapY(yMin)-40);
      IF (xMax - xMin)/PeriodJup < 20
      THEN
         OutTextxy(MapX(MarkPeriods)-10, MapY(yMin)-22, NumStr(NTick, 2,0))
      ELSE
         IF Trunc(NTick/10) = NTick/10 THEN
         OutTextxy(MapX(MarkPeriods)-10, MapY(yMin)-22, NumStr(NTick, 2,0));
      SelectViewPort(2);
      NTick := NTick + 1;
      MarkPeriods := MarkPeriods + PeriodJup;
   UNTIL MarkPeriods > xMax;
   SelectViewPort(4);
   OutTextxy(MapX(xMax*0.5), MapY(yMin)-10, 'Jupiter revolutions');
   SelectViewPort(2);
   SetColor(Yellow);
   ShowCursor;
END;

{-------------------------------------------------------------------------}

Procedure AScreen(aMax, aMin, ElementTime: Real);
VAR
   i                    : Integer;
   xSc, ySc, xSd, ySd   : Integer;
   Tick                 : Real;
   Mark                 : String;
   xMouse, yMouse       : Real;
   Inside               : Boolean;

BEGIN
   IF aMax < Asta + 0.1 THEN aMAx := Asta + 0.1;
   IF aMin > Asta - 0.1 THEN aMin := Asta - 0.1;
   ElementMax := aMax;
   SetColor(Yellow);
   HideCursor; OpenViewPort(2);
   DefineScale(2, 0, ElementTime, aMin, aMax);
   SelectScale(2);
   DrawAxes(0, ElementTime, aMin, aMax);
   Map(0, Asta, xSc, ySc); Map(ElementTime, Asta, xSd, ySd);
   Line(xSc, ySc, xSd, ySd);
   Map(0.9*ElementTime, aMin, xSc, ySc);
   OutTextxy(xSc, ySc-40, 'Years');
   Map(0, aMax, xSc, ySc);
   OutTextxy(xSc+40, ySc+10, 'Astronomical units');
   Map(0.5*ElementTime, aMax, xSc, ySc);
   OutTextxy(xSc, ySc+10, 'Semimajor axis');
   ShowCursor;
END;

{-------------------------------------------------------------------------}

Procedure EScreen(eMax, eMin, ElementTime: Real);
VAR
   i: Integer;
   xSc, ySc, xSd, ySd: Integer;
   Tick: Real;
   Mark: String;
BEGIN
   SetColor(Yellow);
   HideCursor; OpenViewPort(2);
   ElementMax := eMax;
   DefineScale(2, 0, ElementTime, eMin, eMax);
   SelectScale(2);
   DrawAxes(0, ElementTime, eMIn, eMax);
   Map(0.9*ElementTime, eMin, xSc, ySc);
   OutTextxy(xSc, ySc-40, 'Years');
   Map(0.5*ElementTime, eMax, xSc, ySc);
   OutTextxy(xSc, ySc+10, 'Eccentricity');
   ShowCursor;
END;

{-------------------------------------------------------------------------}

Procedure IScreen(iMax, iMin, ElementTime: Real);
VAR
   i: Integer;
   xSc, ySc, xSd, ySd: Integer;
   Tick: Real;
   TickDegrees: Real;
   Mark: String;
BEGIN
   SetColor(Yellow);
   HideCursor; OpenViewPort(2);
   DefineScale(2, 0, ElementTime, iMin, iMax);
   ElementMax := iMax;
   SelectScale(2);
   DrawAxes(0, ElementTime, iMin, iMax);
   Map(0.9*ElementTime, iMin, xSc, ySc);
   OutTextxy(xSc, ySc-40, 'Years');
   Map(0.1*ElementTime, iMax, xSc, ySc);
   OutTextxy(xSc, ySc+10, 'Degrees');
   Map(0.5*ElementTime, iMax, xSc, ySc);
   OutTextxy(xSc, ySc+10, 'Inclination');
   ShowCursor;
END;

{-------------------------------------------------------------------------}

Procedure AphScreen(AphMax, AphMin, ElementTime: Real);
VAR
   i: Integer;
   xSc, ySc, xSd, ySd: Integer;
   Tick: Real;
   TickDegrees: Integer;
   Mark: String;
BEGIN
   SetColor(Yellow);
   HideCursor; OpenViewPort(2); 
   DefineScale(2, 0, ElementTime, AphMin, AphMax);
   ElementMax := AphMax;
   SelectScale(2);
   DrawAxes(0, ElementTime, AphMin, AphMax);
   Map(0.9*ElementTime, AphMin, xSc, ySc);
   OutTextxy(xSc, ySc-40, 'Years');
   Map(0.1*ElementTime, AphMax, xSc, ySc);
   OutTextxy(xSc, ySc+10, 'Degrees');
   Map(0.5*ElementTime, AphMax, xSc, ySc);
   OutTextxy(xSc, ySc+10, 'Argument of Perihelion');
   ShowCursor;
END;

{-------------------------------------------------------------------------}

Procedure NodeScreen(NodeMax, NodeMin, ElementTime: Real);
VAR
   i: Integer;
   xSc, ySc, xSd, ySd: Integer;
   Tick: Real;
   TickDegrees: Integer;
   Mark: String;
BEGIN
   SetColor(Yellow);
   HideCursor; OpenViewPort(2); 
   DefineScale(2, 0, ElementTime, NodeMin, NodeMax);
   ElementMax := NodeMax;
   SelectScale(2);
   DrawAxes(0, ElementTime, NodeMin, NodeMax);
   Map(0.9*ElementTime, NodeMin, xSc, ySc);
   OutTextxy(xSc, ySc-40, 'Years');
   Map(0.1*ElementTime, NodeMax, xSc, ySc);
   OutTextxy(xSc, ySc+10, 'Degrees');
   Map(0.3*ElementTime, NodeMax, xSc, ySc);
   OutTextxy(xSc, ySc+10, 'Longitude of the ascending node');
   ShowCursor;
END;

{-------------------------------------------------------------------------}

Procedure DrawElement(n: Integer);
VAR
   xSc, ySc        : Integer;
   i               : Integer;
   xMouse, yMouse  : Real;
   Inside          : Boolean;
   xOld, yOld      : Real;
BEGIN
   HideCursor;
   SetColor(White);
   FOR i := 1 TO 500 DO
   BEGIN
      Map(i*(ElementTime/500), Elements[i, n], xSc, ySc);
      IF i = 1 THEN  MoveTo(xSc, ySc) ELSE LineTo(xSc, ySc);
   END;
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
END;

{-------------------------------------------------------------------------}

Procedure SetUpElementArray(ElementTime: Real; yA: VectorA;
                           VAR yOut: VectorA; VAR Elements: ElementArray);
VAR
   Dt, tOut, h               : Real;
   ElementVec                : Vector5;
   i, j                      : Integer;
   xSc, ySc                  : Integer;
   xOld, yOld, xNew, yNew    : Real;
   xMouse, yMouse            : Real;
   Inside                    : Boolean;

BEGIN
   FOR i := 1 TO 500 DO
   BEGIN
      FOR j := 1 TO 5 DO Elements[i, j] := 0;
   END;
   aMin := Asta - da;
   aMax := Asta + da;
   SetBkColor(Black);
   ClearMuppetPort;
   CUPSKeys.Display;
   ShowCUPSKeys := True;
   AstMenu.Display;
   SetBkColor(Black);
   AScreen(aMax, aMin, ElementTime);
   Action := True;
   SetColor(White);
   Dt := ElementTime/500*(2*3.14159);
   FindElements(yA, ElementVec);
   aMax := Asta; aMin := Asta;
   eMax := ElementVec[2]; eMin := eMax;
   iMax := ElementVec[3]; iMin := iMax;
   AphMax := ElementVec[4]; AphMin := AphMax;
   NodeMax := ElementVec[5]; NodeMin := NodeMax;
   i := 0;
   HideMouse;
   REPEAT
         i := i + 1;
         tOut := 0;
         REPEAT
            Step(tOut, Dt - tOut, 0.00001, yA, tOut, h, yA);
         UNTIL Abs(Dt - tout) < 0.001;
         FindElements(yA, ElementVec);
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
         xNew := i*(ElementTime/500);
         yNew := ElementVec[1];
         IF i > 1 THEN  PlotLine(xOld, yOld, xNew, yNew);
         xOld := xNew; yOld := yNew;
         FOR j := 1 TO 5 DO
            Elements[i, j] := ElementVec[j];
   UNTIL (i = 500);
   ShowMouse;
   SetColor(Yellow);
   Message('Move mouse outside window to activate menu');
   REPEAT
      MousePosn(xMouse, yMouse, 2, 2, Inside);
      IF Inside THEN
      BEGIN
         IF Abs(xOld-xMouse) + Abs(yOld-yMouse) > 0.01 THEN
         BEGIN
            RubOutXY(0.7, 0.8, 16, DarkGray);
            PrintXY(0.7, 0.8, '('+NumStr(xMouse, 6,1)+', '
                                     +NumStr(yMouse, 4,2)+')');
            xOld := xMouse;
            yOld := yMouse;
         END;
      END;
   UNTIL NOT Inside;
   RubOutXY(0.7, 0.8, 16, DarkGray);
   Message('');
      yOut := yA;
      FOR j := 1 TO 7 DO
         AstMenu.RowActivate(5,j, True);
      IF Abs(Elements[1,3]) < 0.001 THEN
      BEGIN
         AstMenu.RowActivate(5,4, False);
         AstMenu.RowActivate(5,5, False);
      END;
END;                                  

{------------------------------------------------------------------------}

PROCEDURE UpDateElements;
BEGIN
   ElementInfo(ElementTime, da);
   IF InputCanceled THEN 
   BEGIN
      SetUpAstMenu3;
      {AstMenu.RowActivate(4,1, False);}
      LightMenu := True;
      Exit;
   END;
   GraphBackColor := DarkGray;
   SetUpElementArray(ElementTime, yA, yA, Elements);
   IF ABS(Elements[1, 3]) > 0.001 THEN SetUpAstMenu4
                                         ELSE SetUpAstMenu2;
   LightMenu := True;
END;

{-------------------------------------------------------------------------}

PROCEDURE GetData;
BEGIN
     AstMenu.RowActivate(3,1, False);
     AstDataOK := False;
     MakeFile := False;
     IF FileOpen THEN Close(F1);
     FileOpen := False;
        SetUpAstMenu1;
        AstData1(MassJup, Jupa, Jupe, Jupf0);
        If InputCanceled THEN Exit;
        AstData2(MassSun, MassJup, Jupa, nA, Asta, De, Di);
        IF InputCanceled THEN BEGIN MakeFile := False; Exit; END;
     ShowCursor;
     AstDataOK := True;
     AstMenu.RowActivate(3,1, True);
END;

{-------------------------------------------------------------------------}
 

PROCEDURE HandleAstMenu;

BEGIN
   With AstMenu DO
   CASE ColChosen OF
      1: CASE RowChosen OF
            1: BEGIN
                  AboutCUPS;
                  LightMenu := True;
               END;
            2: BEGIN
                  ShowHelp('HELPNbod.HLP', 'Asteroids Hello');
                  LightMenu := True;
               END;
            3: Configuration;
            4: BEGIN
                  ShowHelp('HELPNbod.HLP', 'Asteroids GeneralA');
                  ShowHelp('HELPNbod.HLP', 'Asteroids GeneralB');
                  ShowHelp('HELPNbod.HLP', 'Asteroids GeneralC');
                  LightMenu := True;
               END;

            5: ExitRequest := True;
         END;

      2: BEGIN
            AstMenu.RowActivate(3,1, False);
            GetData;   
            IF AstDataOK THEN
            BEGIN
               AstMenu.RowActivate(3,1, True);
               InitialConditions(MassSun, MassJup, Jupa, Jupe, Jupf0,
                                Asta, De, Di, nStates);
               Message('');AnimateAsteroids(Jupa);
            END
            ELSE
            BEGIN
               Announce('Data must be complete to see the animation');
               LightMenu := True;
            END;
         END;

      3: BEGIN
            {InitialConditions(MassSun, MassJup, Jupa, Jupe, Jupf0,
                                Asta, De, Di, nStates);}
         Message(''); AnimateAsteroids(Jupa);
         END;
      4: CASE RowChosen OF
         1: BEGIN
               IF FileOpen THEN Close(F1);
               ChooseAsteroid(Jupa, nA, yA);
               UpDateElements;
            END;
         END;
         5: CASE RowChosen OF
                  1: BEGIN
                        AScreen(aMax, aMin, ElementTime);
                        DrawElement(1);
                        LightMenu := True;
                     END;
                  2: BEGIN
                        EScreen(eMax, eMin, ElementTime);
                        DrawElement(2);
                        LightMenu := True;
                     END;
                  3: BEGIN
                        AphScreen(AphMax, AphMin, ElementTime);
                        DrawElement(4);
                        LightMenu := True;
                     END;
                  4: BEGIN
                        IScreen(iMax, iMin, ElementTime);
                        DrawElement(3);
                        LightMenu := True;
                     END;
                  5: BEGIN
                        NodeScreen(NodeMax, NodeMin, ElementTime);
                        DrawElement(5);
                        LightMenu := True;
                     END;
                  6: UpDateElements;
               END;
    END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleCUPSKeys(Key: Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HELPNbod.HLP', 'Asteroids Hello');
      2: IF AstMenu.Chosen THEN HandleAstMenu;
   END;
END;

{-------------------------------------------------------------------------}

PROCEDURE GetSetUp;
BEGIN
   SeeAllAsteroids := False;
   FileOpen := False;
   LightMenu := False;
   ExitRequest := False;
   MassJup := 0.001;
   Jupa := 5.2;
   Jupe := 0;
   Jupf0Deg := 0;
   MarsRadius := 1.4;
   NA := 50;
   PResonance := 2;
   QResonance := 1;
   De := 0;
   DiDeg := 0;
   ElementTime := 200;
   Da := 0.3;
   SetBkColor(Black);
   DefineViewPort(1, 0, 1, 0.07, 0.94);
   DefineViewPort(2, 0.05, 0.95, 0.14, 0.94);
   DefineViewPort(4, 0.05, 0.95, 0.07, 1);
   ShowCUPSKeys := True;
   SetUpCUPSKeys;
END;

{-------------------------------------------------------------------------}

procedure RunAstroids;
BEGIN    {Main Program.}
{  CUPSinit;}
   GetSetUp;
   ShowHelp('HELPNbod.HLP', 'Asteroids Hello');
   SetUpAstMenu;
   CUPSKeys.Display;
   GetData;
   IF AstDataOK THEN
   BEGIN
      InitialConditions(MassSun, MassJup, Jupa, Jupe, Jupf0,
                                Asta, De, Di, nStates);
      Message(''); AnimateAsteroids(Jupa);
   END
   ELSE
   BEGIN
      Announce('Data must be complete to see the animation');
      LightMenu := True;
   END;
   REPEAT
      CheckForEvents;
      IF ShowCUPSKeys THEN
              IF CUPSKeys.Pressed(Key) THEN HandleCUPSKeys(Key);
      IF (SeeAllAsteroids) THEN
      BEGIN
           IF HotKeys.Pressed(Key) THEN
                            HandleHotKeys(Key, Action, PlotOrbit, Action);
           IF (Action) AND (PlotOrbit) THEN Advance;
           IF (NOT PlotOrbit) THEN
           BEGIN
              IF FileOpen THEN Close(F1);
              FileOpen := False;
              SeeAllAsteroids := False;
           END;
      END;
      IF LightMenu THEN
      BEGIN
         LightMenu := False;
         IF AstMenu.Chosen THEN HandleAstMenu;
      END;
     { ELSE
         IF AstMenu.Activated THEN HandleAstMenu;}
   UNTIL  ExitRequest;
   IF FileOpen THEN Close(F1);
{  CUPSDone;}
   AstMenu.done;
END;

END.
