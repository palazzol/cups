    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1994.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program to display the animated motion        *)
    (*       of n mutually attracting bodies               *)
    (*       simulating a "solar system."                  *)
    (*                                                     *)
    (*******************************************************)

unit Planets;

{$M 64000,0,327680}
{$O+}
{$F+}
interface
procedure RunPlanets;

implementation
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;

procedure RunPlanets;
CONST
   MaxNBodies = 11;
   MaxNeq = 66;    {Number of differential equations.}
   dt = 0.5;   {Time between plotted positions.}
   Eps = 0.001;  {Distance within which the inverse square law is replaced
                   by a constant force.}

TYPE
   Vector = Array[1..MaxNeq] OF Real;
   Vector5 = Array[1..5] OF Real;
   Vector6 = Array[1..6] OF Real;
   MassVector = Array[1..MaxNBodies] of Real;

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
   PlanetDat2                     : TInputScreen;
   Action                         : Boolean;
   Mass                           : MassVector;
   StoreSma, StoreEcc, StoreInc   : MassVector;
   StoreNode, StoreAph, Storef0   : MassVector;
   y                              : Vector;
   MaxDist                        : Real;
   Jupa, Jupe, Jupf0Deg           : Real;
   DataOK                         : Boolean;
   LightMenu                      : Boolean;
   Neq, NBodies                   : Integer;
   NPLanets                       : Integer;
   NBMenu                         : TMenu;
   HotKeys1, HotKeys2             : THotKeys;
   CUPSKeys                       : THotKeys;
   ShowCUPSKeys                   : Boolean;
   Key                            : Byte;
   DataComplete                   : Boolean;
   PlotOrbit                      : Boolean;
   SeeTraces                      : Boolean;
   ExitRequest                    : Boolean;
   SeeOrbits                      : Boolean;
   Lag                            : Integer;
   OldCoordinates                 : Array[1..11, 1..2] of Real;
   PColors                         : Array[1..15] of Integer;

PROCEDURE HandleNBMenu; FORWARD;

{---------------------------------------------------------------------}

PROCEDURE SetUpNBMenu;
BEGIN
   With NBMenu DO
   BEGIN
      Init;
      Column(1, 'File');
       row(1,1, 'About CUPS');
       row(1,2, 'About Program');
       row(1,3, 'Configuration');
       row(1,4, 'General information');
       row(1,5, 'Exit this module');
      Column(2, 'Run');
       row(2,1, 'Enter data');
       row(2,2, 'Animate some more');
   END;
   NBMenu.Display;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpHotKeys1;
BEGIN
   WITH HotKeys1 DO
   BEGIN
      HotKeys1.Init(5);
      Key[1] := 'F1-Help';
      Key[2] := 'F2-Pause';
      Key[3] := 'F3-Faster';
      Key[4] := 'F4-Slower';
      Key[5] := 'F10-Menu';
   END;
   HotKeys1.BackColor := Blue;
   HotKeys1.HotKeyColor := Yellow;
   HotKeys1.Display;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpHotKeys2;
BEGIN
   WITH HotKeys2 DO
   BEGIN
      HotKeys2.Init(6);
      Key[1] := 'F1-Help';
      Key[2] := 'F2-Pause';
      Key[3] := 'F3-Faster';
      Key[4] := 'F4-Slower';
      Key[5] := 'F5-CleanUp';
      Key[6] := 'F10-Menu';
   END;
   HotKeys2.BackColor := Blue;
   HotKeys2.HotKeyColor := Yellow;
   HotKeys2.Display;
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

{---------------------------------------------------------------------}

Procedure GetData(VAR Mass: MassVector; VAR y: Vector);
VAR
   Sma, Ecc, Inc, Node, APh, f0: Real;
   State: Vector6;
   DataOK: Boolean;
   xSc, ySc: Integer;
   NPlanet: Integer;
   NumStr: String;
   InputCanceled  : Boolean;

FUNCTION PlanetData1 : Boolean;
CONST
   RBNames: String = '12345';
VAR
   Jupf0, Param     : Real;
   i                : Integer;
   Query, FileName  : String;
   PlanetDat1       : TInputScreen;
BEGIN
   IF FileOpen THEN BEGIN Close(F1); FileOpen := False; END;
   WITH PlanetDat1 DO
   BEGIN
      Init;
      DefineInputPort(0.03, 0.97, 0.05, 0.95);
      LoadLine('                     MAKE YOUR OWN SOLAR SYSTEM');
      LoadLine('');
      LoadLine('The model consists of the Sun, ''Jupiter'',  and up to nine other planets.');
      LoadLine('');
      LoadLine('Enter the total number of planets:         {   } (between two and ten)');
      LoadLine('');
      LoadLine('The mass of the Sun is one unit of mass.');
      LoadLine('');
      LoadLIne('Enter data for Jupiter:');
      LoadLine('                       Mass of Jupiter : {       } solar masses');
      LoadLine('             Semimajor axis (positive) : {       } astronomical units');
      LoadLIne('             Eccentricity (0 <= e < 1) : {       }');
      LoadLIne('True anomaly at the start of the motion: {       } degrees');
      LoadLine('');
      LoadLine('            See orbit traces?  No #2   Yes #2');
      LoadLine('');
      LoadLIne('     Store results in a file?  No #1   Yes #1');
      LoadLIne('                             File name : "Planets1"');
      LoadLIne('');
      LoadLIne('                [  OK  ]           [Cancel]');
      SetNumber(1, NPlanets);
      SetNumberLimits(1, 2, 10);
      SetNumber(2, Mass[2]);
      SetNumberLimits(2, 0, 1000);
      SetNumber(3, Jupa);
      SetNumberLimits(3, 0.001, 10000);
      SetNumber(4, Jupe);
      SetNumberLimits(4, 0, 0.9999);
      SetNumber(5, Jupf0Deg);
      Accept;
      PlanetData1 := NOT Canceled;
      IF Canceled THEN Exit;
      Mass[1] := 1;
      NPlanets := Trunc(GetNumber(1) + 0.00001);
      NBodies := NPlanets + 1;
      Neq := 6*NBodies;
      Mass[2] := GetNumber(2);
      Jupa := GetNumber(3);
      StoreSma[1] := Jupa;
      Jupe := GetNumber(4);
      StoreEcc[1] := Jupe;
      StoreInc[1] := 0;
      StoreNode[1] := 0;
      StoreAph[1] := 0;
      Jupf0Deg := GetNumber(5);
      Storef0[1] := Jupf0Deg;
      Jupf0 := Pi*Jupf0Deg/180;     {Convert to radians.}
      MaxDist := Jupa*(1 + Jupe);
      Param := Jupa*(1 - Sqr(Jupe));
      FOR i := 1 TO 6 DO y[i] := 0;
      y[7] := Param*Cos(Jupf0)/(1 + Jupe*Cos(Jupf0));
      y[8] := Param*Sin(Jupf0)/(1 + Jupe*Cos(Jupf0));
      y[9] := 0;
      y[10] := - Sqrt((Mass[1] + Mass[2])/Param)*Sin(Jupf0);
      y[11] := Sqrt((Mass[1] + Mass[2])/Param)*(Jupe + Cos(Jupf0));
      y[12] := 0;
      IF GetRadioButton(RBNames[2]) = 1 THEN SeeTraces := False
                                        ELSE SeeTraces := True;
      IF GetRadioButton(RBNames[1]) = 1 THEN MakeFile := False
                                        ELSE MakeFile := True;
      FileName := GetString(10);
      IF MakeFile THEN
      BEGIN
         Assign(F1, Concat(Filename, '.POS'));
         ReWrite(F1);
      END;
      ClearInputPort;
      Done;
   END;
END;

{---------------------------------------------------------------------}

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

{---------------------------------------------------------------------}

Procedure StartPlanet(Sma, Ecc, Inc, Node, APh, f0, MassPlanet: Real;
                        VAR State: Vector6);
VAR
   Param: Real;
   xa, ya, za, vxa, vya, vza: Real;
   nr: Integer;
   Th: Real;

BEGIN
   Param := Sma*(1 - Sqr(Ecc));
   xa := Param*Cos(f0)/(1 + Ecc*Cos(f0));
   ya := Param*Sin(f0)/(1 + Ecc*Cos(f0));
   za := 0;
   vxa := - Sqrt((Mass[1] + MassPlanet)/Param)*Sin(f0);
   vya := Sqrt((Mass[1] + MassPlanet)/Param)*(Ecc + Cos(f0));
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

{---------------------------------------------------------------------}


Procedure PlanetData2(NPlanet: Integer);
VAR
   i             : Integer;
   PlanetDat2    : TInputScreen;
BEGIN
   WITH PlanetDat2 DO
   BEGIN
      Init;
      DefineInputPort(0, 1, 0.46, 0.88);
      LoadLIne('                                   Mass : {       } solar masses');
      LoadLine('              Semimajor axis (a >= 0.1) : {       } astronomical units');
      LoadLine('              Eccentricity (0 <= e < 1) : {       }');
      LoadLine('             Inclination (0 <= i < 180) : {       } degrees');
      LoadLIne('        Longitude of the ascending node : {       } degrees');
      LoadLine('                 Argument of perihelion : {       } degrees');
      LoadLine('True anomaly at the start of the motion : {       } degrees');
      LoadLine('');
      LoadLine('                 [  OK  ]             [Cancel]');
      SetNumber(1, Mass[NPLanet+1]);
      SetNumberLimits(1, 0, 10000);
      SetNumber(2, StoreSma[NPlanet]);
      SetNumberLimits(2, 0.1, 10000);
      SetNumber(3, StoreEcc[NPlanet]);
      SetNumberLimits(3, 0, 0.9999);
      SetNumber(4, StoreInc[NPlanet]);
      SetNumberLimits(4, 0, 179.999);
      SetNumber(5, StoreNode[NPlanet]);
      SetNumberLimits(5, 0, 359.999);
      SetNumber(6, StoreAph[NPlanet]);
      SetNumberLimits(6, 0, 359.999);
      SetNumber(7, Storef0[NPlanet]);
      SetNumberLimits(7, 0, 359.999);
     { CASE NPlanet OF
         2: BEGIN
               SetNumber(2, 2);
               SetNumber(7, 80);
            END;
         3: BEGIN
               SetNumber(2, 2.5);
               SetNumber(7, 160);
            END;
         4: BEGIN
               SetNumber(2, 3);
               SetNumber(7, 240);
            END;
         5: BEGIN
               SetNumber(2, 3.5);
               SetNumber(7, 320);
            END;
         6: BEGIN
               SetNumber(2, 4);
               SetNumber(7, 40);
            END;
         7: BEGIN
               SetNumber(2, 4.5);
               SetNumber(7, 120);
            END;
         8: BEGIN
               SetNumber(2, 6);
               SetNumber(7, 200);
            END;
         9: BEGIN
               SetNumber(2, 6.5);
               SetNumber(7, 280);
            END;
         10: BEGIN
               SetNumber(2, 7);
               SetNumber(7, 340);
            END;
      END;   }
      Accept;
      InputCanceled := Canceled;
     { DataOK := True;}
      Mass[NPlanet + 1] := GetNumber(1);
      Sma := GetNumber(2);
      StoreSma[NPlanet] := Sma;
      Ecc := GetNumber(3);
      StoreEcc[NPlanet] := Ecc;
      Inc := GetNumber(4);
      StoreInc[NPlanet] := Inc;
      Inc := Pi*Inc/180;     {Convert to radians.}
      Node := GetNumber(5);
      StoreNode[NPlanet] := Node;
      Node := Pi*Node/180;     {Convert to radians.}
      Aph := GetNumber(6);
      StoreAph[NPlanet] := Aph;
      Aph := Pi*Aph/180;     {Convert to radians.}
      f0 := GetNumber(7);
      Storef0[NPlanet] := f0;
      f0 := Pi*f0/180;     {Convert to radians.}
      IF MaxDist < Sma*(1 + Ecc) THEN MaxDist := Sma*(1 + Ecc);
      DefineScale(3, -2*MaxDist, 2*MaxDist, -1.5*MaxDist, 1.5*MaxDist);
      StartPlanet(Sma, Ecc, Inc, Node, APh, f0, Mass[NPlanet], State);
      FOR i := 1 TO 6 DO
         y[6*NPlanet + i] := State[i];
      ClearInputPort;
      Done;
   END;
END;

{---------------------------------------------------------------------}

PROCEDURE InputSoFar(NPlanet: Integer);
VAR
   xSc, ySc: Integer;
   i: Integer;
   PlStr, aStr, eStr, iStr, nStr, pStr, fStr: String;
BEGIN
   GraphBackColor := Blue;
   SetColor(White);
   HideCursor;
   OpenViewPort(2);
   SelectScale(2);
   Map(0.4, 12, xSc, ySc);
   OutTextxy(xSc, ySc, 'INPUT SOFAR');
   Map(0, 11, xSc, ySc);
   OutTextxy(xSc, ySc, ' Planet   SmAxis   Eccentricity   Inclination  Node   P-Helion   True Anom ');
   FOR i := 1 TO NPlanet - 1 DO
   BEGIN
      Str(i, plStr);
      Str(StoreSma[i]: 6: 2, aStr);
      Str(StoreEcc[i]: 6: 4, eStr);
      Str(StoreInc[i]: 5: 1, iStr);
      Str(StoreNode[i]: 5: 1, nStr);
      Str(StoreAph[i]: 5: 1, pStr);
      Str(Storef0[i]: 5: 1, fStr);
      Map(0, 11 - i, xSc, ySc);
      plStr := Concat(plStr+'    '+aStr+'       '+eStr+'        '+iStr);
      plStr := Concat('    '+plStr+'     '+nStr+'    '+pStr+'       '+fStr);
      OutTextxy(xSc, ySc, plStr);
   END;
   ShowCursor;
END;

{---------------------------------------------------------------------}

BEGIN    {Main Program for Procedure GetData.}
   DataComplete := False;
   InputCanceled := False;
   Action := False;
   PlotOrbit := False;
   NBMenu.RowActivate(3,1, False);
   NBMenu.Display;
      IF NOT PlanetData1 THEN
      BEGIN
         MakeFile := False;
         LightMenu := True;
         ClearMuppetPort;
         NBMenu.Display;
         Exit;
      END
      ELSE
      BEGIN
         NPlanet := 1;
         REPEAT
            NPlanet := NPlanet + 1;
            GraphBackColor := Blue;
            HideCursor;
            OpenViewPort(1);
            SelectScale(1);
            SetColor(White);
            Map(0.3, 0.5, xSc, ySc);
            CASE NPlanet OF
               2: NumStr := 'second';
               3: NumStr := 'third';
               4: NumStr := 'fourth';
               5: NumStr := 'fifth';
               6: NumStr := 'sixth';
               7: NumStr := 'seventh';
               8: NumStr := 'eighth';
               9: NumStr := 'ninth';
               10: NumStr := 'tenth';
            END;
            OutTextxy(xSc, ySc, Concat('Data for the ' + NumStr + ' planet'));
            InputSoFar(NPlanet);
            ShowCursor;
            PlanetData2(NPlanet);
         UNTIL (NPLanet = NBodies-1) OR (InputCanceled);
      END;
      IF (InputCanceled) THEN
      BEGIN
         MakeFile := False;
         LightMenu := True;
         ClearMuppetPort;
         NBMenu.Display;
      END
      ELSE
      BEGIN
         GraphBackColor := Red;
         OpenViewPort(1);
         SelectScale(1);
         SetColor(White);
         Map(0.35, 0.5, xSc, ySc);
         OutTextxy(xSc, ySc, 'Data complete');
         DataComplete := True;
         NBMenu.RowActivate(3,1, True);
         Delay(1000);
      END;
END;

{---------------------------------------------------------------------}

Procedure Fun(x: Real; y: Vector; VAR z: Vector);
VAR
   Dist: Real;
   i, j, n: Integer;
BEGIN
   FOR n := 1 TO NBodies DO
   BEGIN
      FOR j := 1 TO 3 DO
      BEGIN
         z[6*n - 6 + j] := y[6*n - 3 + j];
         z[6*n - 3 + j] := 0;
      END;
   END; {Initialization.}
   FOR n := 2 TO NBodies DO
   BEGIN
      FOR i := 1 TO n-1 DO
      BEGIN
         Dist := (Sqr(Y[6*n-6+1] - Y[6*i-6+1])
                        + Sqr(Y[6*n-6+2] - Y[6*i-6+2])
                        + Sqr(Y[6*n-6+3] - Y[6*i-6+3]));
         IF Dist < Eps THEN Dist := Eps;
         Dist := Dist*Sqrt(Dist);
         FOR j := 1 TO 3 DO
         BEGIN
            z[6*n - 3 + j] := z[6*n - 3 + j] +
                              Mass[i]*(Y[6*i-6+j] - Y[6*n-6+j])/Dist;
            z[6*i - 3 + j] := z[6*i - 3 + j] +
                              Mass[n]*(Y[6*n-6+j] - Y[6*i-6+j])/Dist;
         END;  {j loop.}
      END;  {i loop.}
   END; {n loop.}
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
END;


{---------------------------------------------------------------------}

PROCEDURE Advance;
VAR
   i, j: Integer;
   Tout, h: Real;
BEGIN
      Tout := 0;
      REPEAT
         Step(Tout, Dt - Tout, 0.0001, Y,
                Tout, h, Y);
      UNTIL Abs(Dt - Tout) < 0.001;
      Delay(Lag);
END;

{---------------------------------------------------------------------}

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

Procedure ShowBodies;
VAR
   n: Integer;
   
BEGIN
   IF MakeFile THEN BEGIN ReWrite(F1); FileOpen := True; END;
   PColors[1] := 14;
   PColors[2] := 15;
   PColors[3] := 11;
   PColors[4] := 2;
   PColors[5] := 3;
   PColors[6] := 14;
   PColors[7] := 7;
   PColors[8] := 9;
   PColors[9] := 10;
   PColors[10] := 12;
   PColors[11] := 13;
   IF MakeFile THEN
   BEGIN
      ReWrite(F1);
      Points.x := Trunc(-2000*MaxDist);
      Points.y := Trunc(2000*MaxDist);
      Points.z := Trunc(-1500*MaxDist);
      Write(F1, Points);
      Points.x := Trunc(1500*MaxDist);
      Points.y := nBodies - 2;
      Points.z := 1;    {A positive value here records the two galaxies.}
      Write(F1, Points);
   END;
   Action := True;
   PlotOrbit := True;
   ClearMuppetPort;
   GraphBackColor := Black;
   OpenViewPort(3);
   SelectScale(3);
   {NBMenu.Display;}
   ShowCUPSKeys := False;
   CUPSKeys.Clear;
   IF SeeTraces THEN SetUpHotKeys2 ELSE SetUpHotKeys1;
   FOR n := 1 TO nBodies DO
   BEGIN
      SetColor(PColors[n]);
      IF n = 1 THEN PlotSymbol(y[6*n - 5] - y[1], y[6*n-4] - y[2], 'O');
      IF n = 2 THEN PlotSymbol(y[6*n - 5] - y[1], y[6*n-4] - y[2], 'o');
      IF n > 2 THEN StarPlot(y[6*n - 5] - y[1], y[6*n-4] - y[2], PColors[n]);
      {IF n > 2 THEN PlotSymbol(y[6*n - 5] - y[1], y[6*n-4] - y[2], '.');}
      OldCoordinates[n, 1] := y[6*n-5];
      OldCoordinates[n, 2] := y[6*n-4];
   END;
   IF MakeFile THEN
   BEGIN
      FOR n := 2 TO nBodies DO
      BEGIN
         Points.x := Trunc(1000*(y[6*n - 5] - y[1]));
         Points.y := Trunc(1000*(y[6*n - 4] - y[2]));
         Points.z := Trunc(1000*(y[6*n - 3] - y[3]));
         Write(F1, Points);
      END;
   END;
END;

PROCEDURE TakeStep;
VAR
   n: Integer;
 BEGIN
    Advance;
    SetColor(GraphBackColor);
    FOR n := 2 TO nBodies DO
       IF n = 2 THEN
            PlotSymbol(OldCoordinates[n, 1], OldCoordinates[n, 2], 'o')
            ELSE
            IF n > 2 THEN StarPlot(OldCoordinates[n, 1], OldCoordinates[n, 2],
                                GraphBackColor);
            {PlotSymbol(OldCoordinates[n, 1], OldCoordinates[n, 2], '.');}
    SetColor(Yellow);
    PlotSymbol(0, 0, 'O');
      PColors[1] := 14;
   PColors[2] := 15;
   PColors[3] := 11;
   PColors[4] := 2;
   PColors[5] := 3;
   PColors[6] := 14;
   PColors[7] := 7;
   PColors[8] := 9;
   PColors[9] := 10;
   PColors[10] := 12;
   PColors[11] := 13;
    FOR n := 2 TO NBodies DO
    BEGIN
       SetColor(PColors[n]);
       IF n = 2 THEN
          PlotSymbol(y[6*n - 5]-  y[1], y[6*n-4] - y[2], 'o')
       ELSE
          IF n > 2 THEN StarPlot(y[6*n - 5] - y[1], y[6*n-4] - y[2], PColors[n]);
         { PlotSymbol(y[6*n - 5]-  y[1], y[6*n-4] - y[2], '.');}
       IF SeeTraces THEN
       BEGIN
          SetColor(Brown);
          PlotLine(OldCoordinates[n, 1], OldCoordinates[n, 2],
                       y[6*n - 5]-  y[1], y[6*n-4] - y[2]);
       END;
       OldCoordinates[n, 1] := y[6*n-5] - y[1];
       OldCoordinates[n, 2] := y[6*n-4] - y[2];
    END;
    IF MakeFile THEN
    BEGIN
       FOR n := 2 TO nBodies DO
       BEGIN
          Points.x := Trunc(1000*(y[6*n - 5] - y[1]));
          Points.y := Trunc(1000*(y[6*n - 4] - y[2]));
          Points.z := Trunc(1000*(y[6*n - 3] - y[3]));
          Write(F1, Points);
       END;
    END;
 END;

{---------------------------------------------------------------------}

PROCEDURE HandleNBMenu;

BEGIN
   With NBMenu DO
   CASE ColChosen OF
      1: CASE RowChosen OF
            1: BEGIN
                  AboutCUPS;
                  LightMenu := True;
               END;
            2: BEGIN
                  ShowHelp('HELPNbod.HLP', 'SSystem Hello');
                  LightMenu := True;
               END;
            3: BEGIN
                  Configuration;
                  LightMenu := True;
               END;
            4: BEGIN
                  ShowHelp('HELPNbod.HLP', 'SSystem GeneralA');
                  LightMenu := True;
               END;
            5: ExitRequest := True;
         END;
      2: CASE RowChosen OF
            1: BEGIN
                  ClearMuppetPort;
                  NBMenu.Display;
                  GetData(Mass, y);
                  IF DataComplete THEN
                  BEGIN
                     NBMenu.RowActivate(2,2, True);
                     Message('');
                     ShowBodies;
                  END
                  ELSE
                  BEGIN
                     Announce('Data must be complete for animation to be seen');
                     ShowCUPSKeys := True;
                     CUPSKeys.Display;
                  END;
               END;
            2: BEGIN
                  PlotOrbit := True;
                  Action := True;
                  Message('');
                  IF SeeTraces THEN SetUpHotKeys2 ELSE SetUpHotKeys1;
               END;
         END;
    END;
END;

{---------------------------------------------------------------------}

PROCEDURE HandleHotKeys1(Key: Byte);
VAR
   DelLag: Integer;
BEGIN
   DelLag := 20;
   CASE Key OF
      1: ShowHelp('HELPNbod.HLP', 'SSystem Hello');
      2: BEGIN
            IF Action = True THEN
            BEGIN
               Action := False;
               WITH HotKeys1 DO Key[2] := 'F2-Run';
               HotKeys1.BackColor := Red;
               HotKeys1.Display;
            END
            ELSE
            BEGIN
               Action := True;
               WITH HotKeys1 DO Key[2] := 'F2-Pause';
               HotKeys1.BackColor := Blue;
               HotKeys1.Display;
            END;
         END;
      3: IF Lag > DelLag THEN Lag := Lag - DelLag;
      4: Lag := Lag + DelLag;
      5: BEGIN
            PlotOrbit := False;
            HotKeys1.Clear;
            ShowCUPSKeys := True;
            CUPSKeys.Display;
            IF NBMenu.Chosen THEN HandleNBMenu;
         END;
   END;
END;

{---------------------------------------------------------------------}

PROCEDURE HandleHotKeys2(Key: Byte);
VAR
   DelLag: Integer;
BEGIN
   DelLag := 20;
   CASE Key OF
      1: ShowHelp('HELPNbod.HLP', 'SSystem Hello');
      2: BEGIN
            IF Action = True THEN
            BEGIN
               Action := False;
               WITH HotKeys2 DO Key[2] := 'F2-Run';
               HotKeys2.BackColor := Red;
               HotKeys2.Display;
            END
            ELSE
            BEGIN
               Action := True;
               WITH HotKeys2 DO Key[2] := 'F2-Pause';
               HotKeys2.BackColor := Blue;
               HotKeys2.Display;
            END;
         END;
      3: IF Lag > DelLag THEN Lag := Lag - DelLag;
      4: Lag := Lag + DelLag;
      5: OpenViewPort(3);
      6: BEGIN
            HotKeys2.Clear;
            PlotOrbit := False;
            ShowCUPSKeys := True;
            CUPSKeys.Display;
            IF NBMenu.Chosen THEN HandleNBMenu;
         END;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleCUPSKeys(Key: Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HELPNbod.HLP', 'SSystem Hello');
      2: IF NBMenu.Chosen THEN HandleNBMenu;
   END;
END;

{---------------------------------------------------------------------}

PROCEDURE SetUpParameters;
VAR
   i   : Integer;
BEGIN
   SetBkColor(Black);
   DefineViewPort(1, 0.03, 0.97, 0.9, 0.95);
   DefineScale(1, 0, 1, 0, 1);
   DefineViewPort(2, 0, 1, 0, 0.43);
   DefineScale(2, 0, 1, 0, 13);
   DefineViewPort(3, 0, 1, 0.07, 0.93);
   FileOpen := False;
   LightMenu := False;
   ExitRequest := False;
   Action := False;
   PlotOrbit := False;
   ShowCUPSKeys := True;
   SetUpCUPSKeys;
   NPLanets := 6;
   Mass[2] := 0.005;
   Jupa := 5.2;
   Jupe := 0;
   Jupf0Deg := 0;
   FOR i := 2 TO 10 DO
   BEGIN
      Mass[i+1] := 0.001;
      StoreSma[i] := -0.4+0.8*i;
      IF i > 6 THEN StoreSma[i] := 0.4+0.8*i;
      StoreEcc[i] := 0;
      StoreInc[i] := 0;
      StoreNode[i] := 0;
      StoreAph[i] := 0;
      StoreF0[i] := 40*i-40;
   END;
   Lag := 200;
   CUPSKeys.Display;
   SetUpNBMenu;
   ShowHelp('HELPNbod.HLP', 'SSystem Hello');
END;

{---------------------------------------------------------------------}

BEGIN   {Main Program.}
{  CUPSInit;}
   SetUpParameters;
   GetData(Mass, y);
   IF DataComplete THEN BEGIN Message(''); ShowBodies; END
   ELSE
   BEGIN
      LightMenu := True;
      Announce('Data must be complete for the animation to be seen');
      NBMenu.RowActivate(2,2, False);
      ShowCUPSKeys := True;
      CUPSKeys.Display;
   END;
   REPEAT
      CheckForEvents;
      IF PlotOrbit THEN
      BEGIN
         IF SeeTraces THEN
         BEGIN
            IF HotKeys2.Pressed(Key) THEN HandleHotKeys2(Key);
         END
         ELSE
            IF HotKeys1.Pressed(Key) THEN HandleHotKeys1(Key);
      END;
      IF ShowCUPSKeys THEN
              IF CUPSKeys.Pressed(Key) THEN HandleCUPSKeys(Key);
      IF LightMenu THEN
      BEGIN
         LightMenu := False;
         IF NBMenu.Chosen THEN HandleNBMenu;
      END
      ELSE
         IF NBMenu.Activated THEN HandleNBMenu;
      IF (Action) AND (PlotOrbit) THEN TakeStep;
   UNTIL  ExitRequest;
   IF FileOpen THEN Close(F1);
   NBMenu.done;
{  CUPSDone;}
END;

END.
