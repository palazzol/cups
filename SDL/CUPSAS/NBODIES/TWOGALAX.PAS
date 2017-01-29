    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1994.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program to display the animated motion        *)
    (*       of two "galaxies" and up to 500 "stars."      *)
    (*                                                     *)
    (*******************************************************)

unit TwoGalax;

{$M 64000,0,163840}
{$O+}
{$F+}
interface
procedure RunTwoGalax;

implementation
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;

CONST
   NmaxS = 3;  {Number of terms in the power series for Galaxy 2,
                when moving a star.}
   NmaxG = 6;  {Number of terms in the power series for Galaxy 2,
                when moving the galaxy.}
   Neq = 6;    {Number of differential equations for each star.}
   dt = 0.5;   {Time between plotted positions.}
   Eps = 0.001;  {Distance within which the inverse square law is replaced
                   by a constant force.}
   TruncError = 0.00001; {Greatest truncation error of the series.}
   TooClose = 0.05;  {If a star approches a galaxy more closely
                      than this distance, then it is moved in an arc
                      of Keplerian motion relative to the galaxy.}
   xMin = -7;
   xMax = 7;
   yMin = -5;
   yMax = 5;





TYPE
   Vector = Array[1..6] OF Real;
   Vector1 = ARRAY[0..12] of real;
   Vector2 = ARRAY[1..3] of real;
   Matrix = Array[0..500] of Vector;

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
   NBMenu                             : TMenu;
   HotKeys                            : THotKeys;
   CUPSKeys                           : THotKeys;
   ShowCUPSKeys                       : Boolean;
   Sliders                            : TSliders;
   Key                                : Byte;
   Action, PlotOrbit                  : Boolean;
   NBDataOK                           : Boolean;
   InputAccepted                      : Boolean;
   SplitScreen                        : Boolean;
   ExitRequest                        : Boolean;
   LightMenu                          : Boolean;
   Sliding                            : Boolean;
   Zooming, ZoomIn                    : Boolean;
   ZoomFactor                         : Real;
   NRings, NRings2                    : Real;  {Number of rings of stars.}
   IntNRings, IntNRings2              : Integer;
   MassGal1                           : Real;   {Mass of the first galaxy.}
   MassGal2                           : Real;   {Mass of the second galaxy.}
   Rad, Rad2                          : Vector1;      {Array of radii of rings.}
   FloatNR, FloatNR2: Vector1;  {Array of numbers of stars in the rings.}
   nStates: Matrix;   {Matrix storing the States of all bodies.}
   ns: Integer;       {Total number of stars.}
   nS2: Integer;     {Number of stars of second galaxy.}
   Galpc, Gale, Gali, Galw, Galf0: real;  {Orbital elements of the second
                                          galaxy about the first.}
   GaliDeg, GalwDeg, Galf0Deg: Real;
   Theta, Phi: Real; {Orientation angle for rings of galaxy 2.}
   ThetaDeg, PhiDeg: Real;
   CxMin, CxMax, CyMin, CyMax    : Real; {Graph limits.}


{-------------------------------------------------------------------------}


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
      Column(2, 'Data');
       row(2,1, 'Enter data for the galaxies');
       row(2,2, 'Use default parameters');
      Column(3, 'Orbits');
       row(3,1, 'Start animation in the x-y plane');
       row(3,2, 'Start animation in x-y and x-z planes');
       row(3,3, 'Continue animation in the x-y plane');
       row(3,4, 'Continue animation in x-y and x-z planes');
   END;
   NBMenu.Display;
END;
{------------------------------------------------------------------------}

PROCEDURE SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      HotKeys.Init(5);
      Key[1] := 'F1-Help';
      Key[2] := 'F2-Stop';
      Key[3] := 'F3-ZoomIn';
      Key[4] := 'F4-ZoomOut';
      Key[5] := 'F10-Menu';
   END;
   HotKeys.BackColor := Blue;
   HotKeys.HotKeyColor := Yellow;
   {HotKeys.Display;}
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

Procedure NBData1;
CONST
   RBNames  : String = '12345';
VAR
   NBDat1   : TInputScreen;
BEGIN
   WITH NBDat1 DO
   BEGIN
      Init;
      DefineInputPort(0.03, 0.97, 0.08, 0.95);
      LoadLine('');
      LoadLine('                 DATA FOR THE INTERACTION OF TWO GALAXIES');
      LoadLine('');
      LoadLine('A galaxy contains rings of stars revolving in circular orbits in a plane.');
      LoadLine('');
      LoadLine('A second galaxy passes by the first, perturbing the stars.');
      LoadLIne('');
      LoadLIne('The two galaxies move in Keplerian orbits relative to one another.');
      LoadLine('');
      LoadLine('Store results in a file? No #1  Yes #1');
      LoadLine('                    File name: "Galaxy1"');
      LoadLine('');
      LoadLIne('Enter the number of rings of stars (10 or fewer) = {   } ');
      LoadLine('');
      LoadLine('If you run the program with only a few stars, you may want to slow');
      LoadLine('down the animation. Enter a delay time in milliseconds:');
      LoadLine('                          0 <= {    } < 1000.');
      LoadLine('');
      LoadLine('                    [  OK  ]               [Cancel]');
      SetNumber(4, NRings);
      SetNumberLimits(4, 1, 10);
      SetNumber(5, DelayTime);
      SetNumberLimits(5, 0, 1000);
      AcceptScreen;
      IF NBDat1.Canceled THEN InputAccepted := False
                         ELSE InputAccepted := True;
      IF GetRadioButton(RBNames[1]) = 1 THEN MakeFile := False
                                        ELSE MakeFile := True;
      FileName := GetString(3);    
      NRings := GetNumber(4);
      DelayTime := Trunc(GetNumber(5)+0.001);
      IF NRings > 10 THEN NRings := 10;
      IF NRings < 1 THEN NRings := 1;
      IntNRings := Trunc(NRings + 0.001);
      IF MakeFile THEN
      BEGIN
         Assign(F1, Concat(Filename, '.POS'));
         ReWrite(F1);
      END;
      Done;
   END;
END;

{-------------------------------------------------------------------------}

Procedure NBData2;
VAR
   i       : integer;
   NBDat2  : TInputScreen;

BEGIN
   WITH NBDat2 DO
   BEGIN
      Init;
      DefineInputPort(0.15, 0.85, 0.5-0.15-0.022*IntNRings,
                                  0.5+0.15+0.022*IntNRings);
      LoadLine('              DATA FOR RINGS OF STARS ');
      LoadLine('');
      LoadLine('Radii should not exceed 5 units or be less than 0.1.');
      LoadLine('The total number of stars must not exceed 500.');
      LoadLine('');
      LoadLine('Ring 1.  Radius = {       } Number of stars = {    }');
      IF IntNRings > 1 THEN
         LoadLIne('Ring 2.  Radius = {       } Number of stars = {    }');
      IF IntNRings > 2 THEN
         LoadLIne('Ring 3.  Radius = {       } Number of stars = {    }');
      IF IntNRings > 3 THEN
         LoadLine('Ring 4.  Radius = {       } Number of stars = {    }');
      IF IntNRings > 4 THEN
         LoadLine('Ring 5.  Radius = {       } Number of stars = {    }');
      IF IntNRings > 5 THEN
         LoadLine('Ring 6.  Radius = {       } Number of stars = {    }');
      IF IntNRings > 6 THEN
         LoadLine('Ring 7.  Radius = {       } Number of stars = {    }');
      IF IntNRings > 7 THEN
         LoadLine('Ring 8.  Radius = {       } Number of stars = {    }');
      IF IntNRings > 8 THEN
         LoadLine('Ring 9.  Radius = {       } Number of stars = {    }');
      IF IntNRings > 9 THEN
         LoadLine('Ring 10. Radius = {       } Number of stars = {    }');
      LoadLine('');
      LoadLIne('               [  OK  ]           [Cancel]');
      FOR i := 1 TO IntNRings DO
      BEGIN
         SetNumber(2*i - 1, Rad[i]);
         SetNumberLimits(2*i - 1, 0.1, 5);
         SetNumber(2*i, FloatNR[i]);
         SetNumberLimits(2*i, 1, 500);
      END;
      AcceptScreen;
      IF NBDat2.Canceled THEN InputAccepted := False
                         ELSE InputAccepted := True;
      nS := 0;
      FOR i := 1 TO IntNRings DO
      BEGIN
         Rad[i] := GetNumber(2*i-1);
         FloatNR[i] := GetNumber(2*i);
         nS := nS + Trunc(FloatNR[i]+ 0.001);
      END;
      Done;
   END;
END;

{-------------------------------------------------------------------------}

PROCEDURE TestNBData(VAR NBData: Boolean);

BEGIN
   NBDataOK := True;
   IF nS > 500 THEN
   BEGIN
      NBDataOK := False;
      Message('Too many stars; the maximum number is 500. Click to try again');
      StaticPause;
      NBMenu.Display;
   END;
END;

{----------------------------------------------------------------------}

PROCEDURE NBData3;
CONST
   RBNames  : String = '12345';
VAR
   NBDat3  : TInputScreen;
BEGIN
   WITH NBDat3 DO
   BEGIN
      Init;
      DefineInputPort(0.05, 0.95, 0.05, 0.9);
      LoadLine('');
      LoadLine('                    DATA FOR THE SECOND GALAXY. ');
      LoadLIne('');
      LoadLine('The mass of the first galaxy is 1 unit of mass. Do you want to');
      LoadLine('change this during the running of the program?  No #1 Yes #1');
      LoadLine('');
      LoadLine('    Enter the mass of the second galaxy: {      } > 0.001');
      LoadLIne('');
      LoadLIne('Enter the orbital elements of the second galaxy relative to the first:');
      LoadLIne('The line of nodes will be horizontal on the screen.');
      LoadLine('                Pericenter distance =    {      } (0.5 to 5)');
      LoadLIne('                    Eccentricity    =    {      }');
      LoadLine('                     Inclination    =    {      } degrees');
      LoadLine('          Argument of pericenter    =    {      } degrees');
      LoadLIne('True anomaly at the start of the motion: {      } degrees');
      LoadLIne('');
      LoadLine('The number of rings of stars (5 or fewer) = {   }');
      LoadLine('');
      LoadLine('                 [  OK  ]             [Cancel]');
      SetNumber(3, MassGal2);
      SetNumberLimits(3, 0.001, 10000);
      SetNumber(4, Galpc);
      SetNumberLimits(4, 0.5, 5);
      SetNumber(5, Gale);
      SetNumberLimits(5, 0, 10000);
      SetNumber(6, GaliDeg);
      SetNumberLimits(6, 0, 179.999);
      SetNumber(7, GalwDeg);
      SetNumberLimits(7, 0, 359.999);
      SetNumber(8, Galf0Deg);
      SetNumberLimits(8, -360, 360);
      SetNumber(9, NRings2);
      SetNumberLimits(9, 0, 5);
      AcceptScreen;
      IF NBDat3.Canceled THEN InputAccepted := False
                         ELSE InputAccepted := True;
      IF InputAccepted THEN
      BEGIN
         IF GetRadioButton(RBNames[1]) = 1 THEN Sliding := False
                                           ELSE Sliding := True;
         IF Sliding THEN SplitScreen := False;
         MassGal2 := GetNumber(3);
         Galpc := GetNumber(4);
         Gale := GetNumber(5);
         GaliDeg := GetNumber(6);
         Gali := GaliDeg*Pi/180;
         GalwDeg := GetNumber(7);
         Galw := GalwDeg*Pi/180;
         Galf0Deg := GetNumber(8);
         Galf0 := Galf0Deg*Pi/180;
         NRings2 := GetNumber(9);
         IntNRings2 := Trunc(NRings2 + 0.001);
      END;
      Done;
   END;
END;

{-------------------------------------------------------------------------}

Procedure NBData4;
VAR
   i        : integer;
   NBDat4   : TINputScreen;

BEGIN
   WITH NBDat4 DO
   BEGIN
      Init;
      DefineInputPort(0.12, 0.88, 0.5-0.282-0.022*IntNRings2,
                                  0.5+0.282+0.022*IntNRings2);
      LoadLine('    DATA FOR RINGS OF STARS FOR THE SECOND GALAXY ');
      LoadLine('');
      LoadLine('Radii should not exceed 3 units or be less than 0.1.');
      LoadLine('The total number of stars must not exceed 500.');
      LoadLine('');
      LoadLine('Ring 1.  Radius = {       } Number of stars = {    }');
      IF IntNRings2 > 1 THEN
         LoadLIne('Ring 2.  Radius = {       } Number of stars = {    }');
      IF IntNRings2 > 2 THEN
         LoadLIne('Ring 3.  Radius = {       } Number of stars = {    }');
      IF IntNRings2 > 3 THEN
         LoadLine('Ring 4.  Radius = {       } Number of stars = {    }');
      IF IntNRings2 > 4 THEN
         LoadLine('Ring 5.  Radius = {       } Number of stars = {    }');
      LoadLine('');
      LoadLine('The plane of rings intersects the x-y plane in a line');
      LoadLine('making the angle theta with the x-axis, and is inclined');
      LoadLine('to the x-y plane by the angle phi.');
      LoadLine('             0 <= theta < 360,           0 <= phi <= 90.');
      LoadLine('          theta = {       }            phi = {      }');
      LoadLIne('');
      LoadLIne('               [  OK  ]         [Cancel]');
      FOR i := 1 TO IntNRings2 DO
      BEGIN
         SetNumber(2*i - 1, Rad2[i]);
         SetNumberLimits(2*i - 1, 0.1, 3);
         SetNumber(2*i, FloatNR2[i]);
         SetNumberLimits(2*i, 1, 500-nS);
      END;
      SetNumber(2*IntNRings2+1, 90);
      SetNumber(2*IntNRings2+2, 45);
      SetNumberLimits(2*IntNRings2+1, 0, 359.9);
      SetNumberLimits(2*IntNRings2+1, 0, 90);
      AcceptScreen;
      IF NBDat4.Canceled THEN InputAccepted := False
                         ELSE InputAccepted := True;
      nS2 := 0;
      FOR i := 1 TO IntNRings2 DO
      BEGIN
         Rad2[i] := GetNumber(2*i-1);
         FloatNR2[i] := GetNumber(2*i);
         nS2 := nS2 + Trunc(FloatNR2[i]+ 0.001);
      END;
      ThetaDeg := GetNumber(2*IntNRings2+1);
      Theta := ThetaDeg*Pi/180;
      PhiDeg := GetNumber(2*IntNRings2+2);
      Phi := PhiDeg*Pi/180;
      Done;
      nS := nS + nS2;
   END;
END;

{-------------------------------------------------------------------------}

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

PROCEDURE Galaxy2Rings;

BEGIN
   REPEAT
      NBData4;
      IF nS > 500 THEN
      BEGIN
         Message('Too many stars; the maximum number is 500. Click to try again');
         StaticPause;
         NBMenu.Display;
      END;
   UNTIL (nS <= 500) OR (NOT InputAccepted);
   {IF NOT InputAccepted THEN
   BEGIN
      LightMenu := True;
   END; }
END;

{------------------------------------------------------------------------}

Procedure Star_Initial_Conditions(Rad, FloatNR: Vector1;
VAR nStates: Matrix);
VAR
   i, j, n: Integer;
   th: Real;
   StarColor: Word;
BEGIN
   StarColor := 0;
   FOR i := 1 TO IntNRings DO
   BEGIN
      Randomize;
      th := Random * 2*Pi;
      n := Trunc(FloatNR[i]+0.0001);
      FOR j := 1 to n DO
      BEGIN
         ns := ns + 1;
         nStates[ns, 1] := Rad[i]*Cos(th);
         nStates[ns, 2] := Rad[i]*Sin(th);
         nStates[ns, 3] := 0;
         StarColor := StarColor + 1; IF StarColor > 16 THEN StarColor := 1;
         SetColor(StarColor);
         IF SplitScreen = False THEN
            StarPlot(nStates[ns, 1], nStates[ns, 2], StarColor)
            {PlotSymbol(nStates[ns, 1], nStates[ns, 2], '.')}
         ELSE
         BEGIN
            SelectViewPort(3);
            SelectScale(3);
            {PlotSymbol(nStates[ns, 1], nStates[ns, 2], '.');}
            StarPlot(nStates[ns, 1], nStates[ns, 2], StarColor);
            SelectViewPort(4);
            SelectScale(4);
            {PlotSymbol(nStates[ns, 1], nStates[ns, 3], '.');}
            StarPlot(nStates[ns, 1], nStates[ns, 3], StarColor);
         END;
         nStates[ns, 4] := - Sin(th)/Sqrt(Rad[i]/MassGal1);
         nStates[ns, 5] := Cos(th)/Sqrt(Rad[i]/MassGal1);
         nStates[ns, 6] := 0;
         th := th + 2*Pi/n;
      END; {j}
   END; {i}
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

Procedure Gal_Initial_Conditions(MassGal1, MassGal2,
                                 Galpc, Gale, Gali, Galw, Galf0: Real);
VAR
   xg, yg, zg, vxg, vyg, vzg: real;
   th: real;
   nr: integer;

BEGIN
   xg := Galpc*(1 + Gale)*cos(Galf0)/(1 + Gale*cos(Galf0));
   yg := Galpc*(1 + Gale)*sin(Galf0)/(1 + Gale*cos(Galf0));
   zg := 0;
   vxg := - Sqrt((MassGal1 + MassGal2)/(Galpc*(1 + Gale)))*sin(Galf0);
   vyg := Sqrt((MassGal1 + MassGal2)/(Galpc*(1 + Gale)))*
                                (Gale + cos(Galf0));
   vzg := 0;
   nr := 3; th := - Galw; RotateAxes(nr, th, xg, yg, zg, xg, yg, zg);
   RotateAxes(nr, th, vxg, vyg, vzg, vxg, vyg, vzg);
   nr := 1; th := - Gali; RotateAxes(nr, th, xg, yg, zg, xg, yg, zg);
   RotateAxes(nr, th, vxg, vyg, vzg, vxg, vyg, vzg);
   nStates[0,1] := xg;
   nStates[0,2] := yg;
   nStates[0,3] := zg;
   IF SplitScreen = False THEN
   BEGIN
      SetColor(Green); PlotSymbol(nStates[0, 1], nStates[0, 2], 'o');
      SetColor(LightGreen); PlotSymbol(0, 0, '0');
   END
   ELSE
   BEGIN
      SelectViewPort(3);
      SelectScale(3);
      SetColor(Green);
      PlotSymbol(nStates[0, 1], nStates[0, 2], 'o');
      SetColor(LightGreen);
      PlotSymbol(0, 0, '0');
      SelectViewPort(4);
      SelectScale(4);
      SetColor(Green);
      PlotSymbol(nStates[0, 1], nStates[0, 3], 'o');
      SetColor(LightGreen);
      PlotSymbol(0, 0, '0');
   END;
   nStates[0,4] := vxg;
   nStates[0,5] := vyg;
   nStates[0,6] := vzg;
END;

{-------------------------------------------------------------------------}

Procedure Star_Initial_Conditions2(Rad2, FloatNR2: Vector1);
VAR
   i, j, n: Integer;
   th: Real;
   StarColor: Word;
   xS, yS: Real;
   Ct, St, Cp, Sp: Real;
BEGIN
   StarColor := 0;
   Ct := Cos(Theta);
   St := Sin(Theta);
   Cp := Cos(Phi);
   Sp := Sin(Phi);
   FOR i := 1 TO IntNRings2 DO
   BEGIN
      Randomize;
      th := Random * 2*3.14159265359;
      n := Trunc(FloatNR2[i]+0.0001);
      FOR j := 1 to n DO
      BEGIN
         ns := ns + 1;
         xS := Rad2[i]*Cos(th);
         yS := Rad2[i]*Sin(th);
         nStates[ns, 1] := xS*Ct + yS*Cp*St + nStates[0, 1];
         nStates[ns, 2] := - xS*St + yS*Cp*Ct + nStates[0, 2];  
         nStates[ns, 3] := - yS*Sp + nStates[0, 3];
         StarColor := StarColor + 1; IF StarColor > 16 THEN StarColor := 1;
         SetColor(StarColor);
         IF SplitScreen = False THEN
            {PlotSymbol(nStates[ns, 1], nStates[ns, 2], '.')}
            StarPlot(nStates[ns, 1], nStates[ns, 2], StarColor)
         ELSE
         BEGIN
            SelectViewPort(3);
            SelectScale(3);
            {PlotSymbol(nStates[ns, 1], nStates[ns, 2], '.');}
            StarPlot(nStates[ns, 1], nStates[ns, 2], StarColor);
            SelectViewPort(4);
            SelectScale(4);
            {PlotSymbol(nStates[ns, 1], nStates[ns, 3], '.');}
            StarPlot(nStates[ns, 1], nStates[ns, 3], StarColor);
         END;
         xS := - Sin(th)/Sqrt(Rad2[i]/MassGal2);
         yS := Cos(th)/Sqrt(Rad2[i]/MassGal2);
         nStates[ns, 4] := xS*Ct + yS*Cp*St + nStates[0, 4];
         nStates[ns, 5] := - xS*St + yS*Cp*Ct + nStates[0, 5];
         nStates[ns, 6] := - yS*Sp + nStates[0, 6];
         th := th + 2*Pi/n;
      END; {j}
   END; {i}
END;

{-------------------------------------------------------------------------}

Procedure Advance;     {Takes one step. Used in the Two-Galaxy program.}

VAR
   r, v: Vector2;
   rGal, vGal: Vector2;
   f, g: Vector1;
   mu, t: real;
   n: integer;
   StarColor: Word;
   PossibleTime: Real; {Maximum time for the f and g series.}
   RemainingTime: Real; {Time left in the interval dt.}
   MoveDone: Boolean; {True when time dt is complete.}

{------------------------------------------------------------------------}

PROCEDURE IVPUV(x0, y0, z0, xv0, yv0, zv0, DelTime, Mu: Real;
                  VAR x, y, z, xv, yv, zv: Real);

CONST Pi = 3.1415926535898;

VAR
   r0, v0s, u, r1, ds, f, g, fdot, gdot: Real;
   c0, c1, c2, c3, s, c:                             real;
   Sma, Ecc, alpha, ec, es:        Real;

PROCEDURE Stumpff(x: real; VAR c0, c1, c2, c3: Real);
{With input x, calculates the Stumpff functions c0, c1, c2 and
c3 using the method described in the flowchart on page 173.}
VAR
    n: integer;
BEGIN
   n := 0;
   WHILE abs(x) > 0.1 DO
      BEGIN
         n := n + 1;
         x := x/4;
      END;
   c2 := (1-x*(1-x*(1-x*(1-x*(1-x*(1-x/182)/132)/90)/56)/30)/12)/2;
   c3 := (1-x*(1-x*(1-x*(1-x*(1-x*(1-x/210)/156)/110)/72)/42)/20)/6;
   c1 := 1 - x*c3;
   c0 := 1 - x*c2;
   WHILE n > 0 do
      BEGIN
         n := n - 1;
         c3 := (c2 + c0*c3)/4;
         c2 := c1*c1/2;
         c1 := c0*c1;
         c0 := 2*c0*c0 - 1;
         x := x*4;
      END;
END;

{------------------------------------------------------------------------}

Procedure Kepler(r0, r0rdot0, Mu, Sma, Ecc, DelTime: Real;
                 VAR DelTimeNew, c0, c1, c2, c3, r1: Real);
{Solution of Kepler's equation in universal variables.
Newton's method, accelerated to give quartic convergence,
is used. If this fails, Laguerre's method is available
as a backup.}
{For the initial guess, see Celestial Mechanics, volume 40,
pp 303-312, 1987.}

VAR
   Alpha, en, ec, es, Sigma, x, y, z: Real;
   r02, r03, t1, t2, u1, r12, r13, tol, st:            real;
    ch, sh, fp, fpp, fppp, dm:               real;
   nc:                                                     integer;
BEGIN
   tol := 1e-12;
   Alpha := Mu/Sma;
   if alpha > 0 then
{Elliptic orbit.}
      BEGIN
         en := sqrt(mu/(Sma*Sma*Sma));
         ec := 1 - r0/Sma; es := r0rdot0/(en*Sma*Sma);
         DelTime := DelTime - int(en*DelTime/(2*pi))*(2*pi)/en;
         DelTimeNew := DelTime;
         y := en*DelTime - es;
         z := es*cos(y) + ec*sin(y);
         if z = 0 then sigma := 1 else sigma := abs(z)/z;
         x := y + 0.85*sigma*Ecc;
         s := x/sqrt(alpha);
{Initial guess for elliptic orbit.}
      END {alpha > 0}
   else
      begin
         r02 := r0*r0; r03 := r02*r0;
         if (abs(DelTime*u/r02) < 1) and (abs(DelTime*DelTime*(alpha/r02 + mu/r03)) < 3) then
            begin
               t1 := 0.75*DelTime; t2 := 0.25*DelTime;
               s := t1/r0 - 0.5*t1*t1*u/r03
                    + t1*t1*t1*(alpha - mu/r0 + 3*u*u/r02)/(6*r03)
                    + t1*t1*t1*t1*(u/(r02*r03))
                    *(-3*alpha/8 - 5*u*u/(8*r02) + 5*mu/(12*r0));
               r1 := r0*(1 - alpha*s*s/2) + u*S*(1 - alpha*s*s/6) + mu*s*s/2;
               u1 := (-r0*alpha + mu)*s*(1 - s*s*alpha/6)+u*(1 - alpha*s*s/2);
               r12 := r1*r1; r13 := r1*r12;
               s := s + t2/r1 - 0.5*t2*t2*u1/r13
                      + t2*t2*t2*(alpha - mu/r1 + 3*u1*u1/r12)/(6*r13)
                      + t2*t2*t2*t2*(u1/(r12*r13))
                      *(-3*alpha/8 - 5*u1*u1/(8*r12) + 5*mu/(12*r1));
            end
         else
            begin
               en := sqrt(- mu/(Sma*Sma*Sma));
               ch := 1 - r0/Sma; sh := u/sqrt(- Sma*mu);
               Ecc := sqrt(ch*ch - sh*sh);
               dm := en*DelTime;
               if dm > 0 then
                  s := ln((2*dm + 1.8*Ecc)/(ch + sh))/sqrt(-alpha)
               else
                  s := - ln((-2*dm + 1.8*Ecc)/(ch - sh))/sqrt(-alpha);
            end;
{Initial guess for hyperbolic orbit.}
      end;
{Start Newton's method.}
      nc := 0;
      st := s;
{Store initial guess in case it is needed for Laguerre's method.}
      repeat
         x := s*s*alpha;
         Stumpff(x, c0, c1, c2, c3);
         c1 := c1*s; c2 := c2*s*s; c3 := c3*s*s*s;
         f := r0*c1 + u*c2 + mu*c3 - DelTime;
         fp := r0*c0 + u*c1 + mu*c2; r1 := fp;
         fpp := (- r0*alpha + mu)*c1 + u*c0;
         fppp := (- r0*alpha + mu)*c0 - u*alpha*c1;
         ds := - f/fp;
         ds := - f/(fp + ds*fpp/2);
         ds := - f/(fp + ds*fpp/2 + ds*ds*fppp/6);
         s := s + ds;
         nc := nc + 1;
      until (abs(ds) < tol) or (nc = 10);
      if nc = 10 then
{Newton's method has failed. Laguerre to the rescue.}
         begin  
            nc := 0;
            s := st;
            repeat
               x := s*s*alpha;
               Stumpff(x, c0, c1, c2, c3);
               c1 := c1*s; c2 := c2*s*s; c3 := c3*s*s*s;
               f := r0*c1 + u*c2 + mu*c3 - DelTime;
               fp := r0*c0 + u*c1 + mu*c2;  r1 := fp;
               fpp := (- r0*alpha + mu)*c1 + u*c0;
               ds := 16*fp*fp - 20*f*fpp;
{The Laguerre index is taken here to equal 5.}
               if fp > 0 then
                            ds := - 5*f/(fp + sqrt(abs(ds)))
                         else
                            ds := - 5*f/(fp - sqrt(abs(ds)));
               s := s + ds;
               nc := nc + 1;
            until (abs(ds) < tol) or (nc = 20);
       end; {Laguerre.}
end;

BEGIN
      r0 := sqrt(x0*x0 + y0*y0 + z0*z0);
      v0s := xv0*xv0 + yv0*yv0 + zv0*zv0;
      u := x0*xv0 + y0*yv0 + z0*zv0;
      alpha := 2*mu/r0 - v0s;
      Sma := mu/alpha;
      Ecc := sqrt((1 - r0/Sma)*(1 - r0/Sma) + u*u/(mu*Sma));
      Kepler(r0, u, Mu, Sma, Ecc, DelTime, DelTime, c0, c1, c2, c3, r1);
      f := 1 - (mu/r0)*c2;   
      g := DelTime - mu*c3;  
      fdot := - (mu/(r1*r0))*c1;
      gdot := 1 - (mu/r1)*c2;
      x := x0*f + xv0*g;   
      y := y0*f + yv0*g;   
      z := z0*f + zv0*g;
      xv := x0*fdot + xv0*gdot;
      yv := y0*fdot + yv0*gdot;
      zv := z0*fdot + zv0*gdot;
END;


{------------------------------------------------------------------------}

Procedure FandG(rin, vin: Vector2; mu: real;
                VAR f, g: Vector1; VAR PossibleTime: Real);

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
   FOR n := 2 to NmaxG + 2 DO
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
                   Ln(Abs(f[NmaxG + 1]) + Abs(f[NmaxG + 2]) +
                      Abs(g[NmaxG + 1]) + Abs(g[NmaxG + 2])))/(NmaxG+1));
END;

{------------------------------------------------------------------------}

Procedure GalState(Rin, Vin: Vector2; Time: Real;
                    VAR Stateout: Vector);
VAR
   Fsum, Gsum, FVsum, GVsum: real;
   k: integer;
BEGIN
   Fsum := f[NmaxG]; Gsum := g[NmaxG];
   FOR k := 1 TO NmaxG DO
   BEGIN
      Fsum := Time*Fsum + f[NmaxG - k];
      Gsum := Time*Gsum + g[NmaxG - k];
   END;
   FVsum := NmaxG*f[NmaxG]; GVsum := NmaxG*g[NmaxG];
   FOR k := 1 to NmaxG-1 DO
   BEGIN
      FVsum := Time*FVsum + (NmaxG - k)*f[NmaxG - k];
      GVsum := Time*GVsum + (NmaxG - k)*g[NmaxG - k];
   END;
   FOR k := 1 TO 3 DO
   BEGIN
      Stateout[k] := Fsum*Rin[k] + Gsum*Vin[k];
      Stateout[k + 3] := FVsum*Rin[k] + GVsum*Vin[k];
   END;
END;

{------------------------------------------------------------------------}

Procedure GalPosition(Rin, Vin: Vector2; Time: Real;
                    VAR Rout: Vector2);
VAR
   Fsum, Gsum: real;
   k: integer;
BEGIN
   Fsum := f[NmaxS]; Gsum := g[NmaxS];
   FOR k := 1 TO NmaxS DO
   BEGIN
      Fsum := Time*Fsum + f[NmaxS - k];
      Gsum := Time*Gsum + g[NmaxS - k];
   END;
   FOR k := 1 TO 3 DO
      Rout[k] := Fsum*Rin[k] + Gsum*Vin[k];
END;

{------------------------------------------------------------------------}

Procedure Fun(x: real; y: Vector; VAR z: Vector);
VAR
   n: integer;
   rStar, r1: Vector2;
   Dist1, Dist2, Dist3: real;
BEGIN
   GalPosition(rGal, vGal, x, r1);
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
         z[n+3] := - MassGal1*rStar[n]/Dist1
                + MassGal2*((r1[n] - rStar[n])/Dist2
                            - r1[n]/Dist3);
      END;
END;

{------------------------------------------------------------------------}

Procedure Step(xin, hin, tol: real; yin: Vector;
               VAR xout, hout: real; VAR yout: Vector);

VAR
   a, ch, ct:                      array[1..4] of real;
   b:                              array[1..4,1..3] of real;
   f:                              array[1..5] of vector;
   y, ytemp, te:                          Vector;
   x, xtemp, h, htemp, temax:                   real;
   k, l, n:                        integer;

BEGIN
{Parameters for the Runge-Kutta-Fehlberg method of order 2/3.}

   a[1]   := 0        ;    a[2]   := 1/4     ;     a[3]   := 27/40     ;
   a[4]   := 1 ;       
   ch[1]  := 533/2106   ;    ch[2]  := 0       ;     ch[3]  := 800/1053 ;
   ch[4]  := - 1/78 ;      
   ct[1]  := 23/1782   ;    ct[2]  := -1/33       ;  ct[3]  := 3.50/115.83  ;
   ct[4]  := -1/78    ;    
   b[2,1] := 1/4      ;
   b[3,1] := -189/800     ;    b[3,2] := 729/800   ;
   b[4,1] := 214/891    ;    b[4,2] := 1/33;     b[4,3] := 650/891;
   xtemp := xin;
   h := hin;
   Fun(xin, yin, f[1]);
   ytemp := yin;
   repeat {until temax < tol}
      for k := 2 to 4 do
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
               for k := 1 to 4 do
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
      h := 0.9*h*exp(ln(tol/temax)/4);
   until temax < tol;
   yout := y;
   xout := xtemp + htemp;
   hout := h;
end;



{------------------------------------------------------------------------}

Procedure GalPlot(xg, yg, zg: real; GalColor: integer);
BEGIN
   SetColor(GalColor);
   IF SplitScreen = True THEN
   BEGIN
      SelectViewPort(3);
      SelectScale(3);
      PlotSymbol(xg, yg, 'o');
      SetColor(LightGreen); PlotSymbol(0, 0, '0');
      SelectViewPort(4);
      SelectScale(4);
      SetColor(GalColor);
      PlotSymbol(xg, zg, 'o');
      SetColor(LightGreen); PlotSymbol(0, 0, '0');
   END
   ELSE
   PlotSymbol(xg, yg, 'o');
   SetColor(LightGreen); PlotSymbol(0, 0, '0');
END;

{------------------------------------------------------------------------}

Procedure GalMove(RequiredTime: Real;
                  VAR PossibleTime: Real; VAR MoveDone: Boolean);
VAR
   n: integer;
BEGIN
   FOR n := 1 TO 3 DO
   BEGIN
      rGal[n] := nStates[0, n];
      vGal[n] := nStates[0, n+3];
   END;
   GalPlot(rGal[1], rGal[2], rGal[3], GraphBackColor); 
   FandG(rGal, vGal, MassGal1 + MassGal2, f, g, PossibleTime);
   IF PossibleTime >= RemainingTime THEN
   BEGIN
      PossibleTime := RemainingTime;
      MoveDone := True;
   END
   ELSE
      RemainingTime := RemainingTime - PossibleTime;
   GalState(rGal, vGal, PossibleTime, nStates[0]);
   GalPlot(nStates[0, 1], nStates[0, 2], nStates[0, 3], Green);
END;

{------------------------------------------------------------------------}

Procedure StarMove(nStar: integer; PossibleTime: Real);
VAR
   tout, h: real;
   GalDistance1, GalDistance2: Real;
   x , y, z, xv, yv, zv: Real;
   MoveDone: Boolean;
BEGIN
   MoveDone := False;
{Erase previous image.}
   SetColor(GraphBackColor);
   IF SplitScreen = True THEN
   BEGIN
      SelectViewPort(3);
      SelectScale(3);
      {PlotSymbol(nStates[nStar, 1], nStates[nStar, 2], '.');}
      StarPlot(nStates[nStar, 1], nStates[nStar, 2], GraphBackColor);
      SelectViewPort(4);
      SelectScale(4);
      {PlotSymbol(nStates[nStar, 1], nStates[nStar, 3], '.');}
      StarPlot(nStates[nStar, 1], nStates[nStar, 3], GraphBackColor);
   END
   ELSE
   {PlotSymbol(nStates[nStar, 1], nStates[nStar, 2], '.');}
   StarPlot(nStates[nStar, 1], nStates[nStar, 2], GraphBackColor);
{Test for close approach to first galaxy.}
   x := nStates[nStar, 1];
   y := nStates[nStar, 2];
   z := nStates[nStar, 3];
   GalDistance1 := Sqrt(x*x + y*y + z*z);
   IF GalDistance1 < (MassGal1/MassGal2)*TooClose THEN 
{Use a Keplerian arc around the first galaxy.}   BEGIN
      xv := nStates[nStar, 4];
      yv := nStates[nStar, 5];
      zv := nStates[nStar, 6];
      IVPUV(x, y, z, xv, yv, zv, PossibleTime, MassGal1, x, y, z, xv, yv, zv);
      nStates[nStar, 1] := x;
      nStates[nStar, 2] := y;
      nStates[nStar, 3] := z;
      nStates[nStar, 4] := xv;
      nStates[nStar, 5] := yv;
      nStates[nStar, 6] := zv;
      MoveDone := True;
   END;
{Test for close aproach to second galaxy.}
   x := nStates[nStar, 1] - rGal[1];
   y := nStates[nStar, 2] - rGal[2];
   z := nStates[nStar, 3] - rGal[3];
   GalDistance2 := Sqrt(x*x + y*y + z*z);
   IF GalDistance2 < TooClose THEN
{Use a Keplerian arc around the second galaxy.}
   BEGIN
      xv := nStates[nStar, 4] - vGal[1];
      yv := nStates[nStar, 5] - vGal[2];
      zv := nStates[nStar, 6] - vGal[3];
      IVPUV(x, y, z, xv, yv, zv, PossibleTime, MassGal2, x, y, z, xv, yv, zv);
      nStates[nStar, 1] := nStates[0, 1] + x;
      nStates[nStar, 2] := nStates[0, 2] + y;
      nStates[nStar, 3] := nStates[0, 3] + z;
      nStates[nStar, 4] := nStates[0, 4] + xv;
      nStates[nStar, 5] := nStates[0, 5] + yv;
      nStates[nStar, 6] := nStates[0, 6] + zv;
   MoveDone := True;
   END;

   IF NOT MoveDone THEN
   BEGIN
      tout := 0;
      REPEAT
         Step(tout, PossibleTime - tout, 0.00001, nStates[nStar], tout, h, nStates[nStar]);
      UNTIL Abs(PossibleTime - tout) < 0.00001;
   END;

   SetColor(StarColor);
   IF SplitScreen = True THEN
   BEGIN
      SelectViewPort(3);
      SelectScale(3);
      {PlotSymbol(nStates[nStar, 1], nStates[nStar, 2], '.');}
      StarPlot(nStates[nStar, 1], nStates[nStar, 2], StarColor);
      SelectViewPort(4);
      SelectScale(4);
      {PlotSymbol(nStates[nStar, 1], nStates[nStar, 3], '.');}
      StarPlot(nStates[nStar, 1], nStates[nStar, 3], StarColor);
   END
   ELSE
   {PlotSymbol(nStates[nStar, 1], nStates[nStar, 2], '.'); }
   StarPlot(nStates[nStar, 1], nStates[nStar, 2], StarColor);
END;

{------------------------------------------------------------------------}

BEGIN   {Main Program for Procedure Advance.}
   IF NOT Sliding THEN HideMouse;
   MoveDone := False;
   RemainingTime := Dt;
   REPEAT
      GalMove(RemainingTime, PossibleTime, MoveDone);
      IF (MakeFile = True) AND (MoveDone = True) THEN
      BEGIN
         Points.x := Trunc(1000*nStates[0, 1]);
         Points.y := Trunc(1000*nStates[0, 2]);
         Points.z := Trunc(1000*nStates[0, 3]);
         Write(F1, Points);
      END;
      StarColor := 0;
      FOR n := 1 TO nS DO
      BEGIN
         StarColor := StarColor + 1; IF StarColor > 16 THEN StarColor := 1;
         StarMove(n, PossibleTime);
         IF (MakeFile = True) AND (MoveDone = True) THEN
         BEGIN
            Points.x := Trunc(1000*nStates[n, 1]);
            Points.y := Trunc(1000*nStates[n, 2]);
            Points.z := Trunc(1000*nStates[n, 3]);
            Write(F1, Points);
         END;
      END;
   UNTIL MoveDone = True;
   IF NOT Sliding THEN ShowMouse;
END;

{-------------------------------------------------------------------------}

PROCEDURE SelectDefaultScales;
BEGIN
   DefineScale(1, xMin, xMax, yMin, yMax);
   DefineScale(3, yMin, yMax, 1.2*yMin, 1.2*yMax);
   DefineScale(4, yMin, yMax, 1.2*yMin, 1.2*yMax);
   CxMin := xMin;
   CxMax := xMax;
   CyMin := yMin;
   CyMax := yMax;
END;

{-------------------------------------------------------------------------}

PROCEDURE AnimateEverything;
VAR
   Key: Byte;
   xSc, ySc: Integer;
BEGIN
   SelectDefaultScales;
      GraphBackColor := Black;
      HideCursor;
      IF MakeFile THEN
      BEGIN
         ReWrite(F1);
         FileOpen := True;
         Points.x := Trunc(1000*xMin);
         Points.y := Trunc(1000*xMax);
         Points.z := Trunc(1000*yMin);
         Write(F1, Points);
         Points.x := Trunc(1000*yMax);
         Points.y := Trunc(nS);
         Points.z := 1;    {A positive value here records the two galaxies.}
         Write(F1, Points);
      END;
      IF SplitScreen = True THEN
      BEGIN
         CloseViewPort(1);
         OpenViewPort(3);
         SelectScale(3);
         SetColor(White);
         PrintXY(0.2, 0.14, 'x-y plane');
         {Map(-1, -5, xSc, ySc);
         OutTextxy(xSc, ySc, 'x-y plane');}
         OpenViewPort(4);
         SelectScale(4);
         PrintXY(0.7, 0.14, 'x-z plane');
         {OutTextxy(xSc, ySc, 'x-z plane');}
      END
      ELSE
      BEGIN
         OpenViewPort(1);
         SelectScale(1);
         MassGal1 := 1;
         IF Sliding THEN
         BEGIN
            Sliders.Create(1, 0.1, 4,  1.000, 0.05, 0.15, 0.85, 2, '0.1', '4',
                         'Mass', True);
            Sliders.DrawAll;
         END;
      END;
      ShowCursor;
      ShowCUPSKeys := False;
      CUPSKeys.Clear;
      HotKeys.Display;
      Message('');
      PlotOrbit := True;
      Action := True;
      Ns := 0;
      Gal_Initial_Conditions(MassGal1, MassGal2,
                                 Galpc, Gale, Gali, Galw, Galf0);
      IF IntNRings2 > 0 THEN
                 Star_Initial_Conditions2(Rad2, FloatNR2);
      Star_Initial_Conditions(Rad, FloatNR, nStates);
END;

{-------------------------------------------------------------------------}

PROCEDURE RePlot;
VAR
   nStar      : Integer;
   StarColor: Word;
BEGIN
   IF SplitScreen THEN
   BEGIN
      OpenViewPort(3);
      SelectScale(3);
      SetColor(White);
      PrintXY(0.2, 0.14, 'x-y plane');
      OpenViewPort(4);
      SelectScale(4);
      PrintXY(0.7, 0.14, 'x-z plane');
   END
   ELSE
   BEGIN
      OpenViewPort(1);
      SelectScale(1);
      IF Sliding THEN Sliders.DrawAll;
   END;
   StarColor := 0;
   FOR nStar := 1 TO nS DO
   BEGIN
      StarColor := StarColor + 1;
      IF StarColor > 16 THEN StarColor := 1;
      SetColor(StarColor);
      IF SplitScreen = True THEN
      BEGIN
         SelectViewPort(3);
         SelectScale(3);
         {PlotSymbol(nStates[nStar, 1], nStates[nStar, 2], '.');}
         StarPlot(nStates[nStar, 1], nStates[nStar, 2], StarColor);
         SelectViewPort(4);
         SelectScale(4);
         {PlotSymbol(nStates[nStar, 1], nStates[nStar, 3], '.');}
         StarPlot(nStates[nStar, 1], nStates[nStar, 3], StarColor);
      END
      ELSE
      {PlotSymbol(nStates[nStar, 1], nStates[nStar, 2], '.'); }
      StarPlot(nStates[nStar, 1], nStates[nStar, 2], StarColor);
   END;
   IF SplitScreen = False THEN
   BEGIN
      SelectScale(1);
      SetColor(Green);
      PlotSymbol(nStates[0, 1], nStates[0, 2], 'o');
      SetColor(LightGreen); PlotSymbol(0, 0, '0');
   END
   ELSE
   BEGIN
      SelectViewPort(3);
      SelectScale(3);
      SetColor(Green); PlotSymbol(nStates[0, 1], nStates[0, 2], 'o');
      SetColor(LightGreen); PlotSymbol(0, 0, '0');
      SelectViewPort(4);
      SelectScale(4);
      SetColor(Green);
      PlotSymbol(nStates[0, 1], nStates[0, 3], 'o');
      SetColor(LightGreen);
      PlotSymbol(0, 0, '0');
   END;
END;

{-------------------------------------------------------------------------}

PROCEDURE Zoom;
VAR
   xRange, yRange      : Real;
   xCenter1, yCenter1  : Real;
   xCenter3, yCenter3  : Real;
   xCenter5, yCenter5  : Real;
   ClickOK             : Boolean;
   Ins1, Ins3, Ins5    : Boolean;

FUNCTION ChangeZoomFactor : Boolean;
VAR
   ZoomData  : TInputScreen;
BEGIN
   WITH ZoomData DO
   BEGIN
      Init;
      DefineInputPort(0.27, 0.73, 0.7, 0.9);
      LoadLine('');
      LoadLine(' Zoom factor: 1 <= {      } <= 5.');
      LoadLine('');
      LoadLine('        [  OK  ]  [Cancel]');
      SetNumber(1, ZoomFactor);
      SetNumberLimits(1, 1, 5);
      AcceptScreen;
      ChangeZoomFactor := NOT Canceled;
      IF NOT Canceled THEN ZoomFactor := GetNumber(1);
      Done;
   END;
END;


BEGIN
   GraphBackColor := Blue;
   OpenViewPort(5);
   SelectScale(5);
   SetColor(White);
   IF ZoomIn THEN OutTextxy(MapX(0.4), MapY(0.9), 'Zooming in')
             ELSE OutTextxy(MapX(0.4), MapY(0.9), 'Zooming out');
   IF ChangeZoomFactor THEN
   BEGIN
      HideCursor;
      OutTextxy(MapX(0.1), MapY(0.5),
          'Click on a new center in the x-y plane.');
      OutTextxy(MapX(0.1), MapY(0.2),
          'Click on this bar to return to default scale.');
      ShowCursor;
      ClickOK := False;
      Delay(100);
      REPEAT
         WaitOnMouseClick;
         MousePosn(xCenter1, yCenter1, 1, 1, Ins1);
         MousePosn(xCenter3, yCenter3, 3, 1, Ins3);
         MousePosn(xCenter5, yCenter5, 5, 5, Ins5);
         IF (Ins1 AND NOT SplitScreen) OR (Ins3 AND SplitScreen) THEN
         BEGIN
            ClickOK := True;
            IF ZoomIn THEN
            BEGIN
               xRange := (CxMax - CxMin)/ZoomFactor;
               yRange := (CyMax - CyMin)/ZoomFactor;
            END
            ELSE
            BEGIN
               xRange := (CxMax - CxMin)*ZoomFactor;
               yRange := (CyMax - CyMin)*ZoomFactor;
            END;
            IF SplitScreen THEN
            BEGIN
               CxMin := xCenter3 - xRange/2;
               CxMax := xCenter3 + xRange/2;
               CyMin := yCenter3 - yRange/2;
               CyMax := yCenter3 + yRange/2;
            END
            ELSE
            BEGIN
               CxMin := xCenter1 - xRange/2;
               CxMax := xCenter1 + xRange/2;
               CyMin := yCenter1 - yRange/2;
               CyMax := yCenter1 + yRange/2;
            END;

         END;
         IF Ins5 THEN
         BEGIN
            ClickOK := True;
            CxMin := xMin;
            CxMax := xMax;
            CyMin := yMin;
            CyMax := yMax;
         END;
         IF ClickOK THEN
         BEGIN
            DefineScale(1, CxMin, CxMax, CyMin, CyMax);
            DefineScale(3, CyMin, CyMax, CyMin*1.2, CyMax*1.2);
            DefineScale(4, CyMin, CyMax, CyMin*1.2, CyMax*1.2);
         END
         ELSE Announce('Please click INSIDE a window');
      UNTIL ClickOK;
   END;
   GraphBackColor := Black;
   CloseViewPort(5);
   RePlot;
END;

{-------------------------------------------------------------------------}

PROCEDURE GetData;
VAR
   i   : Integer;
BEGIN
   SelectDefaultScales;
   ClearMuppetPort;
   FOR i := 1 TO 4 DO
     NBMenu.RowActivate(3, i, False);
   IF FileOpen THEN BEGIN Close(F1); FileOpen := False; END;
   NBData1;
   IF NOT InputAccepted THEN
   BEGIN
      MakeFile := False;
      LightMenu := True;
      Announce('Data must be complete for animation to be shown. Try again');
      Exit;
   END;
   REPEAT
      NBData2;
      IF InputAccepted THEN TestNBData(NBDataOK);
   UNTIL (NBDataOK = True) OR (NOT InputAccepted);
   IF NOT InputAccepted THEN
   BEGIN
      MakeFile := False;
      LightMenu := True;
      Announce('Data must be complete for animation to be shown. Try again');
      Exit;
   END;
   NBData3;
   IF NOT InputAccepted THEN
   BEGIN
      MakeFile := False;
      LightMenu := True;
      Announce('Data must be complete for animation to be shown. Try again');
      Exit;
   END;

   IF IntNRings2 > 0 THEN Galaxy2Rings;
   IF NOT InputAccepted THEN
   BEGIN
      MakeFile := False;
      LightMenu := True;
      Announce('Data must be complete for animation to be shown. Try again');
      Exit;
   END;
   FOR i := 1 TO 4 DO
     NBMenu.RowActivate(3, i, True);
END;

{-------------------------------------------------------------------------}

PROCEDURE ContinueAnimation;
BEGIN
   PlotOrbit := True;
   Action := True;
   CUPSKeys.Clear;
   ShowCUPSKeys := False;
   WITH HotKeys DO Key[2] := 'F2-Pause';
   HotKeys.BackColor := Blue;
   HotKeys.Display;
   Message('');
   LightMenu := False;
   RePlot;
END;

{-------------------------------------------------------------------------}

PROCEDURE UseDefaultInput;
VAR
   i    : Integer;

BEGIN
   MassGal1 := 1;
   CxMin := xMin;
   CxMax := xMax;
   CyMin := yMin;
   CyMax := yMax;
   SplitScreen := False;
   Zooming := False;
   ZoomFactor := 2;
   MassGal2 := 0.2;
   NRings := 5;
   IntNRings := 5;
   FOR i := 1 TO 10 DO
   BEGIN
      Rad[i] := 1.5 + 0.1*i;
      FloatNR[i] := 20;
   END;
   Galpc := 3;
   Gale := 0.3;
   GaliDeg := 30;
   Gali := GaliDeg*Pi/180;
   GalwDeg := 0;
   Galw := 0;
   Galf0Deg := -75;
   Galf0 := Galf0Deg*Pi/180;
   NRings2 := 0;
   IntNRings2 := 0;
   FOR i := 1 TO 5 DO
   BEGIN
      Rad2[i] := 0.3 + 0.2*i;
      FloatNR2[i] := 20;
   END;
   ThetaDeg := 90;
   Theta := ThetaDeg*Pi/180;
   PhiDeg := 45;
   Phi := PhiDeg*Pi/180;
   MakeFile := False;
   FileOpen := False;
   Sliding := False;
   ShowCUPSKeys := False;
   GraphBackColor := Black;
   OpenViewPort(1);
   SelectScale(1);
   CUPSKeys.Clear;
   Message('');
   HotKeys.Display;
   FOR i := 1 TO 4 DO
     NBMenu.RowActivate(3, i, True);
   ns := 0;
   Star_Initial_Conditions(Rad, FloatNR, nStates);
   Gal_Initial_Conditions(MassGal1, MassGal2,
                                 Galpc, Gale, Gali, Galw, Galf0);
   Action := True;
   PlotOrbit := True;
END;

{-------------------------------------------------------------------------}

PROCEDURE HandleNBMenu;

BEGIN
   PlotOrbit := True;
   With NBMenu DO
   CASE ColChosen OF
      1: CASE RowChosen OF
            1: BEGIN
                  AboutCUPS;
                  LightMenu := True;
               END;
            2: BEGIN
                  ShowHelp('HELPNbod.HLP', 'Two Galaxies Hello');
                  LightMenu := True;
               END;
            3: BEGIN
                  Configuration;
                  LightMenu := True;
               END;
            4: BEGIN
                  ShowHelp('HELPNbod.HLP', 'Two Galaxies GeneralA');
                  ShowHelp('HELPNbod.HLP', 'Two Galaxies GeneralB');
                  ShowHelp('HELPNbod.HLP', 'Two Galaxies GeneralC');
                  ShowHelp('HELPNbod.HLP', 'Two Galaxies GeneralD');
                  LightMenu := True;
               END;
            5: ExitRequest := True;
         END;
      2: CASE RowChosen OF
            1: BEGIN
                  GetData;
                  IF InputAccepted THEN AnimateEverything
                                   ELSE PlotOrbit := False;
               END;
            2: UseDefaultInput;
         END;
      3: CASE RowChosen OF
            1: BEGIN
                  SplitScreen := False;
                  AnimateEverything;
               END;
            2: BEGIN
                  SplitScreen := True;
                  AnimateEverything;
               END;
            3: BEGIN
                  SplitScreen := False;
                  ContinueAnimation;
               END;
            4: BEGIN
                  SplitScreen := True;
                  CloseViewPort(1);
                  ContinueAnimation;
               END;
         END;
    END;
END;

{-------------------------------------------------------------------------}

PROCEDURE HandleHotKeys(key: Byte);
VAR
   DelLag: Integer;
BEGIN
   DelLag := 20;
   CASE Key OF
      1: ShowHelp('HELPNbod.HLP', 'Two Galaxies Hello');
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
               WITH HotKeys DO Key[2] := 'F2-Pause';
               HotKeys.BackColor := Blue;
               HotKeys.Display;
            END;
         END;
      3: BEGIN
            Zooming := True;
            ZoomIn := True;
            Zoom;
         END;
      4: BEGIN
            Zooming := True;
            ZoomIn := False;
            Zoom;
         END;
      5: BEGIN
            PlotOrbit := False;
            Action := False;
            HotKeys.Clear;
            IF Sliding THEN Sliders.Erase(1, GraphBackColor);
            ShowCUPSKeys := True;
            CUPSKeys.Display;
            LightMenu := True;
         END;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleCUPSKeys(Key: Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HELPNbod.HLP', 'Two Galaxies Hello');
      2: IF NBMenu.Chosen THEN HandleNBMenu;
   END;
END;

{-------------------------------------------------------------------------}

PROCEDURE StartUp;
VAR
   i, Count  : Integer;
   xSc, ySc  : Integer;
   xm, ym    : Integer;
BEGIN
   IntNRings := 14;
   DefineViewPort(1, 0, 1, 0.07, 0.95);
   DefineScale(1, xMin, xMax, yMin, yMax);
   DefineViewPort(3, 0, 0.49, 0.07, 0.95);
   DefineScale(3, yMin, yMax, 1.2*yMin, 1.2*yMax);
   DefineScale(4, yMin, yMax, 1.2*yMin, 1.2*yMax);
   DefineViewPort(4, 0.51, 1, 0.07, 0.95);
   DefineViewPort(5, 0.15, 0.85, 0.85, 0.95);
   DefineScale(5, 0, 1, 0, 1);
   MassGal1 := 1;
   Sliders.Create(1, 0.1, 4,  MassGal1, 0.05, 0.15, 0.85, 2, '0.1', '4',
                      'Mass', True);
   CxMin := xMin;
   CxMax := xMax;
   CyMin := yMin;
   CyMax := yMax;
   SplitScreen := False;
   Zooming := False;
   ZoomFactor := 2;
   MassGal2 := 0.2;
   NRings := 5;
   IntNRings := 5;
   FOR i := 1 TO 10 DO
   BEGIN
      Rad[i] := 1.5 + 0.1*i;
      FloatNR[i] := 20;
   END;
   Galpc := 3;
   Gale := 0.3;
   GaliDeg := 30;
   Gali := GaliDeg*Pi/180;
   GalwDeg := 0;
   Galw := 0;
   Galf0Deg := -75;
   Galf0 := Galf0Deg*Pi/180;
   NRings2 := 0;
   IntNRings2 := 0;
   FOR i := 1 TO 5 DO
   BEGIN
      Rad2[i] := 0.3 + 0.2*i;
      FloatNR2[i] := 20;
   END;
   ThetaDeg := 90;
   Theta := ThetaDeg*Pi/180;
   PhiDeg := 45;
   Phi := PhiDeg*Pi/180;
   MakeFile := False;
   FileOpen := False;
   Sliding := False;
   ShowCUPSKeys := False;
   SetUpCUPSKeys;
   SetUpHotKeys;
   GraphBackColor := Black;
   HideCursor;
   OpenViewPort(1);
   SelectScale(1);
   Map(-2, 4, xSc, ySc);
   ns := 0;
   SetColor(White);
   OutTextxy(xSc, ySc, 'TWO INTERACTING GALAXIES');
   OutTextxy(MapX(-2.5), MapY(-4), 'Click or press <ENTER> to continue');
   Star_Initial_Conditions(Rad, FloatNR, nStates);
   Gal_Initial_Conditions(MassGal1, MassGal2,
                                 Galpc, Gale, Gali, Galw, Galf0);
   Count := 0;
   REPEAT
      Count := Count + 1;
      Advance;
   UNTIL (KeyPressed) OR (MouseClicked(xm, ym));
   ClearMKBuffers;
   PlotOrbit := False;
   ShowCUPSKeys := True;
   CUPSKeys.Display;
  { OpenViewPort(1); }
END;

{-------------------------------------------------------------------------}
procedure RunTwoGalax;

BEGIN   {Main Program.}
{  CUPSinit;}
   Sliders.Init;
   StartUp;
   ExitRequest := False;
   LightMenu := False;
   DelayTime := 0;
   SetUpNBMenu;
   CUPSKeys.Display;
   ShowCUPSKeys := True;
   ShowHelp('HELPNbod.HLP', 'Two Galaxies Hello');
   LightMenu := True;
   ShowCursor;
   REPEAT
      CheckForEvents;
      IF ShowCUPSKeys THEN
              IF CUPSKeys.Pressed(Key) THEN HandleCUPSKeys(Key);
      IF HotKeys.Pressed(Key) THEN HandleHotKeys(Key);
      IF LightMenu THEN
      BEGIN
         LightMenu := False;
         IF NBMenu.Chosen THEN HandleNBMenu;
      END
      ELSE
         IF NBMenu.Activated THEN HandleNBMenu;
      IF Sliding THEN
          IF Sliders.Changed THEN MassGal1 := Sliders.Value(1);
      IF (Action) AND (PlotOrbit) AND (NOT ExitRequest) THEN Advance;
      IF NOT PlotOrbit THEN
      BEGIN
         Action := False;
         IF FileOpen THEN BEGIN Close(F1); FileOpen := False; END;
         HotKeys.Clear;
      END;
   UNTIL  ExitRequest;
   IF FileOpen THEN Close(F1);
   DelayTime := 0;
   NBMenu.Done;
   Sliders.Done;
{  CUPSdone;}
END;

END.