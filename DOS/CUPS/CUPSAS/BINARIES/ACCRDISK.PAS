    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1994.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State Univerity.               *)
    (*                                                     *)
    (*       Program to follow the formation of an         *)
    (*       accretion disk for a close binary system.     *)
    (*                                                     *)
    (*******************************************************)

unit AccrDisk;     {Accrdisk.PAS}
{$M 64000,0,327680}
{$O+}
{$F+}
interface
procedure RunAccrDisk;

implementation
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;

procedure RunAccrDisk;
CONST
   ColorA = LightBlue;
   ColorB = Red;

VAR
   Mu                             : real;
   fnX, fnY                       : real;
   nX, nY                         : integer;
   i                              : Integer;
   Energy                         : real;
   Energies                       : Array[0..50, 0..50] of real;
   xMax, yMax, xMin, yMin         : Real;
   L1x, L1Energy                  : Real;
   Cx, Cy                         : Integer;
   MaxRadius                      : Real;
   AA, BB, AAA, BBB, xCrit, yCrit : Real; {Parameters for a approximation to the shape
   of the orbit of stellar matter into the left lobe.}
   xHot, yHot                     : Real; {Coordinates of the hot spot.}
   Info                           : String;
   Proceed                        : Boolean;
   LightMenu                      : Boolean;
   DiskMenu                       : TMenu;
   HotKeys                        : THotKeys;
   Key                            : Byte;
   ExitRequest                    : Boolean;
   Image                          : TImage;
   DemoNumber                     : Integer;

PROCEDURE HandleDiskMenu;  FORWARD;

{-----------------------------------------------------------------------}

PROCEDURE SetUpDiskMenu;
BEGIN
   DiskMenu.Init;
   With DiskMenu DO
   BEGIN
      Init;
      Column(1, 'File');
       row(1,1, 'About CUPS');
       row(1,2, 'About Program');
       row(1,3, 'Configuration');
       row(1,4, 'Exit this module');
      Column(2, 'Data');
       row(2,1, 'Enter the mass ratio');
   END;
   DiskMenu.Display;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpHotKeys;
BEGIN
    WITH HotKeys DO
    BEGIN
       HotKeys.Init(3);
       Key[1] := 'F1-Help';
       Key[2] := 'F2-Next';
       Key[3] := 'F10-Menu';
       Display;
    END;
END;

{------------------------------------------------------------------------}

FUNCTION RocheData : Boolean;
VAR
   RocheDat  : TInputScreen;
BEGIN
   WITH RocheDat DO
   BEGIN
      Init;
      DefineInputPort(0.19, 0.81, 0.2, 0.85);
      LoadLine('                                            ');
      LoadLine('        The Formation of an Accretion       ');
      LoadLine('            Disk in a Close Binary          ');
      LoadLine('                                            ');
      LoadLine('      Enter the mass ratio. This is the mass');
      LoadLine('   of the primary divided by the total mass ');
      LoadLine('   of the system. It should between 0.1 and ');
      LoadLine('   0.9.                                     ');
      LoadLine('      Mass ratio: 0.1 <= {      } <= 0.9.   ');
      LoadLine('                                            ');
      LoadLine('      To follow stages in the evolution of  ');
      LoadLine('   the disk, press the hot key F2-Next.     ');
      LoadLine('                                            ');
      LoadLine('           [  OK  ]         [Cancel]        ');
      SetNumber(1, Mu);
      SetNumberLimits(1, 0.1, 0.9);
      AcceptScreen;
      RocheData := NOT Canceled;
      IF NOT Canceled THEN Mu := GetNumber(1);
      Done;
   END;
END;

{------------------------------------------------------------------------}

Procedure FindL1(VAR L1x: Real);
VAR
   Approximation, Correction: Real;

Function F(x: Real): Real;
BEGIN
   F := x - (1 - Mu)/Sqr(x + Mu) + Mu/Sqr(x + Mu - 1);
END;

Function FPrime(x: Real): Real;
BEGIN
   FPrime := 1 + 2*(1 - Mu)/(Sqr(x + Mu)*(x + Mu))
               - 2*Mu/(Sqr(x + Mu - 1)*(x + Mu - 1));
END;

BEGIN
   Approximation := 0.5 - Mu;
   REPEAT
      Correction := - F(Approximation)/FPrime(Approximation);
      Approximation := Approximation + Correction;
   UNTIL Abs(Correction) < 0.00001;
   L1x := Approximation;
END;

{------------------------------------------------------------------------}

Procedure FindExtremeCoordinates;
VAR
   Approximation, Correction: Real;
   xTemp: Real;
   MuHigh: Boolean;

Function FLeft(x: Real): Real;
BEGIN
   FLeft := x*x + Mu*(1 - Mu) - 2*(1 - Mu)/(x + Mu) - 2*Mu/(x - 1 + Mu);
END;

Function FCenter(x: Real): Real;
BEGIN
   FCenter := x*x + Mu*(1 - Mu) + 2*(1 - Mu)/(x + Mu) - 2*Mu/(x - 1 + Mu);
END;

Function FRight(x: Real): Real;
BEGIN
   FRight := x*x + Mu*(1 - Mu) + 2*(1 - Mu)/(x + Mu) + 2*Mu/(x - 1 + Mu);
END;

Function FLeftPrime(x: Real): Real;
BEGIN
   FLeftPrime := 2*x + 2*(1 - Mu)/Sqr(x + Mu)
                   + 2*Mu/Sqr(x - 1 + Mu);
END;

Function FRightPrime(x: Real): Real;
BEGIN
   FRightPrime := 2*x - 2*(1 - Mu)/Sqr(x + Mu)
                   - 2*Mu/Sqr(x - 1 + Mu);
END;

BEGIN
   IF Mu > 0.5 THEN
   BEGIN
      Mu := 1 - Mu;
      MuHigh := True;
   END
   ELSE MuHigh := False;
   FindL1(L1x);
   L1Energy := FCenter(L1x);
   Approximation := - 2*Mu - L1x;
   REPEAT
      Correction := - (FLeft(Approximation) - L1Energy)
                             /FLeftPrime(Approximation);
      Approximation := Approximation + Correction;
   UNTIL Abs(Correction) < 0.00001;
   xMin := Approximation;
   Approximation := 1;
   REPEAT
      Correction := - (FRight(Approximation) - L1Energy)
                             /FRightPrime(Approximation);
      Approximation := Approximation + Correction;
   UNTIL Abs(Correction) < 0.00001;
   xMax := Approximation;
   IF MuHigh = True THEN
   BEGIN
      L1x := - L1x;
      xTemp := xMax;
      xMax := - xMin;
      xMin := - xTemp;
      Mu := 1 - Mu;
   END;
   L1Energy := FCenter(L1x);
   xMax := xMax + 0.2;
   xMin := xMin - 0.2;
   yMax := 0.35*(xMax - xMin);
   yMin := - yMax;
END;

{------------------------------------------------------------------------}

Procedure SetUpScreen;
VAR
   xSc, ySc: integer;
BEGIN
   GraphBackColor := Black;
   SetColor(Black);
   HideMouse;
   OpenViewPort(1);
   SelectScale(1);
   {SetUpDiskMenu1;}
   SetBkColor(Black);
   SetColor(ColorA);  SetFillStyle(1, ColorA);
   Map(-Mu, 0, xSc, ySc);
   FillEllipse(xSc, ySc, 5, 4);
   SetColor(ColorB);  SetFillStyle(1, ColorB);
   Map(1 - Mu, 0, xSc, ySc);
   FillEllipse(xSc, ySc, 10, 8);
   ShowMouse;
END;

{------------------------------------------------------------------------}

Function EnergyFun(xf, yf, mu: real): real;
VAR
   rho1, rho2, val: real;
BEGIN
   rho1 := Sqrt((xf + mu)*(xf + mu) + yf*yf);
   rho2 := Sqrt((xf - 1 + mu)*(xf - 1 + mu) + yf*yf);
   IF (rho1 < (1 - mu)/500) OR (rho2 < mu/500)
   THEN EnergyFun := 50
   ELSE
   BEGIN
      val := xf*xf + yf*yf + mu*(1 - mu) + 2*((1 - mu)/rho1 + mu/rho2);
      if val > 50 then val := 50;
      EnergyFun := val;
   END;
END;

{------------------------------------------------------------------------}
 
Procedure FindEnergies;
VAR
   i, j: Integer;
   xc, yc: Real;
BEGIN
   FOR i := 0 TO Nx DO
   BEGIN
      FOR j := 0 TO Ny DO
      BEGIN
         xc := xMin + i*(xMax - xMin)/fNx;
         yc := yMin + j*(yMax - yMin)/fNy;
         Energies[i, j] := EnergyFun(xc, yc, Mu);
      END;
   END;
END;

{------------------------------------------------------------------------}

Procedure Coordinates(Ci, Cj: integer; VAR xc, yc, zc: real);
{Calculates x, y and z coordinates, given the i and j coordinates
in the base grid.}
BEGIN
   xc := xMin + Ci*(xMax - xMin)/fNx;
   yc := yMin + Cj*(yMax - yMin)/fNy;
   zc := Energies[Ci, Cj];
END;

{------------------------------------------------------------------------}

Procedure TriCTest(nx1, ny1, nx2, ny2: integer; tx3, ty3, tz3, Cz: real);
VAR
   Pc:  integer;
   tc:  real;
   tx1, ty1, tz1, tx2, ty2, tz2: real;
   xCross1, yCross1, xCross2, yCross2: real;
   xC1, yC1, xC2, yC2: Integer;
BEGIN
   Coordinates(nx1, ny1, tx1, ty1, tz1);
   Coordinates(nx2, ny2, tx2, ty2, tz2);
   Pc := 0;
   IF tz1 <> tz2 THEN
      BEGIN
         tc := (cz - tz1)/(tz2 - tz1);
         IF (tc >= 0) AND (tc <= 1) THEN
            BEGIN
               Pc := Pc + 1;
               xCross1 := tx1 + (tx2 - tx1)*tc;
               yCross1 := ty1 + (ty2 - ty1)*tc;
            END;
     END;
  IF tz2 <> tz3 THEN
     BEGIN
        tc := (cz - tz2)/(tz3 - tz2);
        IF (tc >= 0) AND (tc <= 1) THEN
           BEGIN
              Pc := Pc + 1;
             
              IF Pc = 1 THEN
                 BEGIN
                    xCross1 := tx2 + (tx3 - tx2)*tc;
                    yCross1 := ty2 + (ty3 - ty2)*tc;
                 END
              ELSE
                 BEGIN
                    xCross2 := tx2 + (tx3 - tx2)*tc;
                    yCross2 := ty2 + (ty3 - ty2)*tc;
                 END;
           END;
     END;
  IF (Pc = 1) AND (tz3 <> tz1) THEN
     BEGIN
        tc := (cz - tz3)/(tz1 - tz3);
        IF (tc >= 0) AND (tc <= 1) THEN
           BEGIN
              Pc := Pc + 1;
              xCross2 := tx3 + (tx1 - tx3)*tc;
              yCross2 := ty3 + (ty1 - ty3)*tc;
           END;
     END;
  IF Pc = 2 THEN
  BEGIN
     IF (xCross1 > xMin + 0.19) AND (xCross2 > xMin + 0.19) AND
        (xCross1 < xMax - 0.19) AND (xCross2 < xMax - 0.19) THEN
     BEGIN
        Map(xCross1, yCross1, xC1, yC1);
        Map(xCross2, yCross2, xC2, yC2);
        Line(xC1, yC1, xC2, yC2);
     END;
  END;
END;

{------------------------------------------------------------------------}

Procedure Contour(Cz: real);
{Draws a contour for the selected energy, Cz.}
VAR
   i, j: integer;
   tx1, ty1, tz1, tx2, ty2, tz2, tx3, ty3, tz3: real;
BEGIN
   SetColor(14);
   Cz := Cz + 0.00001;
   FOR i := 1 TO Nx DO
      BEGIN 
         FOR j := 1 TO Ny DO
            BEGIN
               Coordinates(i-1, j-1, tx1, ty1, tz1);
               Coordinates(i, j, tx2, ty2, tz2);
               tx3 := (tx1 + tx2)/2;
               ty3 := (ty1 + ty2)/2;
               tz3 := EnergyFun(tx3, ty3, Mu);
               TriCTest(i-1, j-1, i, j-1, tx3, ty3, tz3, Cz);
               TriCTest(i-1, j-1, i-1, j, tx3, ty3, tz3, Cz);
               TriCTest(i, j, i-1, j, tx3, ty3, tz3, Cz);
               TriCTest(i, j, i, j-1, tx3, ty3, tz3, Cz);
            END;  {j loop.}
      END; {i loop.}
END;

{------------------------------------------------------------------------}

Procedure SealL1;
VAR
   xSc, ySc: Integer;
BEGIN
   Map(L1x, 0, xSc, ySc);
   SetColor(14);
   SetFillStyle(1, 14);
   FillEllipse(xSc, ySc, 3, 2);
END;

{------------------------------------------------------------------------}

PROCEDURE Messagexy(xMessage, yMessage: Real; New: Boolean; Message: String);
VAR
   xSc, ySc: Integer;
   StoreColor: Word;
BEGIN
   StoreColor := GetColor;
   IF New THEN
   BEGIN
      GraphBackColor := Blue;
      SetColor(Yellow);
      OpenViewPort(2);
      SelectScale(2);
      Map(xMessage, yMessage, xSc, ySc);
      HideMouse;
      OutTextxy(xSc, ySc, Message);
      ShowMouse;
      SelectViewPort(1);
      SelectScale(1);
      GraphBackColor := Black;
   END
   ELSE
   BEGIN
      SelectViewPort(2);
      SelectScale(2);
      SetColor(Yellow);
      Map(xMessage, yMessage, xSc, ySc);
      HideMouse;
      OutTextxy(xSc, ySc, Message);
      ShowMouse;
      SelectViewPort(1);
      SelectScale(1);
   END;
   SetColor(StoreColor);
END;
   

{------------------------------------------------------------------------}

Procedure PlotRocheLobes;
BEGIN
   FindExtremeCoordinates;
   DefineScale(1, xMin, xMax, yMin, yMax);
   FindEnergies;
   GraphBackColor := Black;
   SetUpScreen;
   Messagexy(0.05, 1.4, True,
             'The Roche lobes meet at the Lagrangian point L1.');
   Messagexy(0.05, 0.8, False,
             'The primary, star A, a white dwarf, is on the left.');
   Energy := L1Energy;
   SetColor(3);
   SetLineStyle(SolidLn, 0, ThickWidth);
   HideCursor;
   Contour(Energy);
   SealL1;
   ShowCursor;
END;




{------------------------------------------------------------------------}

Procedure FillLobeB;
VAR
   Radius, dRadius, Angle, dAngle: Real;
   xOld, yOld, xNew, yNew: Real;
   Energy, Energy1, Energy2, DelEnergy: Real;
   i: Integer;
   xSc, ySc: Integer;
BEGIN
   Messagexy(0.01, 1, True,
   'The material of the secondary star, B, expands until it fills its lobe.');
   SetLineStyle(SolidLn, 0, ThickWidth);
   SetColor(Red);
   Energy := EnergyFun(L1x, 0, Mu);
   Radius := 0.03; dRadius := 0.00005;
   Angle := 0;
   dAngle := 0.1;
   xOld := 1 - Mu + Radius; yOld := 0;
   HideCursor;
   REPEAT
      Radius := Radius + dRadius;
      Angle := Angle + dAngle;
      xNew := 1 - Mu + Radius*Cos(angle);
      yNew := Radius*Sin(Angle);
      IF    (EnergyFun(xOld, yOld, Mu) > L1Energy)
      AND   (EnergyFun(xNew, yNew, Mu) > L1Energy)
      THEN   PlotLine(xOld, yOld, xNew, yNew);
      xOld := xNew; yOld := yNew;
   UNTIL 1 - Mu - Radius < L1x;
   Image.Get(Trunc(0.09*GetMaxX), 50, Trunc(0.91*GetMaxX), GetMaxy);
   ShowMouse;
  { Contour(L1Energy);}
END;

{------------------------------------------------------------------------}

Function Shape(x, xCrit: Real): Real;
BEGIN
   IF x > xCrit THEN
         Shape := AA*(x - L1x) + BB*(x - L1x)*(x - L1x)
      ELSE
         Shape := yCrit + AAA*(x - xCrit) + BBB*(x - xCrit)*(x - xCrit);
END;
Procedure HotSpot(xH, yH: Real; HotColor: Word);
VAR
   xSc, ySc: Integer;
BEGIN
   Map(xH, yH, xSc, ySc);
   SetFillStyle(1, HotColor);
   SetColor(HotColor);
   FillEllipse(xSc, ySc, 5, 4);
END;



{------------------------------------------------------------------------}

Procedure LeakOutOfLobe;   

CONST
   Neq = 4;
   DelayTime = 200;

TYPE
   Vector = Array[1..4] of Real;

VAR
   Time, h: Real;
   State: Vector;
   xOld, yOld, xNew, yNew: Integer;
   S2, S3, S4, S5, S6, Dx, Temp: Real;
   i: Integer;

Procedure Fun(x: real; y: Vector; VAR z: Vector);
VAR
   Term1, Term2: real;
{y[1] and y[1] are components of position; y[2] and y[4] are components of velocity,}
BEGIN
   Term1 := Sqr(y[1]+Mu) + Sqr(y[3]);
   Term2 := Sqr(y[1]-1+Mu) + Sqr(y[3]);
   Term1 := (1-Mu)/(Term1 * Sqrt(Term1)) ;
   Term2 := MU/(Term2 * Sqrt(Term2)) ;
   z[1] := y[2];
   z[3] := y[4];
   z[2] := 2*y[4]+y[1]-(y[1]+Mu)*Term1 - (y[1]-1+Mu)*Term2 ;
   z[4] := -2*y[2]+y[3]*(1 - Term1 - Term2);
END;


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



BEGIN
   SetColor(ColorB);
   SetLineStyle(SolidLn, 0, ThickWidth);
   State[1] := L1x;
   State[3] := 0;
   State[2] := - 0.1;
   State[4] := 0;
   Time := 0;
   h := 0.1;
   Messagexy(0.01, 1.9, True,
              'Matter can only leave the full lobe through L1.');
   Messagexy(0.01, 1.4, False,
   'All orbital motion in lobe A is planar and must lie within the lobe.');
{Start the entry into the left lobe and find a function approximating
the shape of the curve.}
   S2 := 0; S3 := 0; S4 := 0; S5 := 0; S6 := 0;
   Map(State[1], State[3], xOld, yOld);
   Map(L1x + 0.05, 0, xNew, yNew);
   SetColor(ColorB);
   Line(xOld, yOld, xNew, yNew); {Fill out the L1 region.}
   FOR i := 1 TO 15 DO
   BEGIN
      Step(Time, h, 0.000001, State, Time, h, State);
      Map(State[1], State[3], xNew, yNew);
      Line(xOld, yOld, xNew, yNew);
      xOld := xNew; yOld := yNew;
      Dx := (State[1] -L1x);
      S5 := S5 + Dx*State[3];
      Temp := Dx*Dx;
      S2 := S2 + Temp;
      S6 := S6 + Temp*State[3];
      Temp := Temp*Dx;
      S3 := S3 + Temp;
      S4 := S4 + Temp*Dx;
      Delay(DelayTime);
   END;
   AA := (S5*S4 - S3*S6)/(S2*S4 - S3*S3);
   BB := (S2*S6 - S3*S5)/(S2*S4 - S3*S3);
   xCrit := State[1]; yCrit := State[3];
   S2 := 0; S3 := 0; S4 := 0; S5 := 0; S6 := 0;
   FOR i := 1 TO 15 DO
   BEGIN
      Step(Time, h, 0.000001, State, Time, h, State);
      Map(State[1], State[3], xNew, yNew);
      Line(xOld, yOld, xNew, yNew);
      xOld := xNew; yOld := yNew;
      Dx := (State[1] - xCrit);
      S5 := S5 + Dx*(State[3] - yCrit);
      Temp := Dx*Dx;
      S2 := S2 + Temp;
      S6 := S6 + Temp*(State[3] - yCrit);
      Temp := Temp*Dx;
      S3 := S3 + Temp;
      S4 := S4 + Temp*Dx;
      Delay(DelayTime);
   END;
   AAA := (S5*S4 - S3*S6)/(S2*S4 - S3*S3);
   BBB := (S2*S6 - S3*S5)/(S2*S4 - S3*S3);
   REPEAT
      Step(Time, h, 0.000001, State, Time, h, State);
      Map(State[1], State[3], xNew, yNew);
      Line(xOld, yOld, xNew, yNew);
      xOld := xNew; yOld := yNew;
      Delay(DelayTime);
   UNTIL Shape(State[1], xCrit) < State[3];
   xHot := State[1]; yHot := State[3];
   Messagexy(0.01, 0.9, False,
   'When the material meets further matter streaming in, it heats up to ');
   Messagexy(0.01, 0.4, False, 'to form a hot spot.');
   HotSpot(xHot, yHot, LightRed);
END;

{------------------------------------------------------------------------}

Procedure ClearLobeA;
VAR
   xSc, ySc: Integer;
BEGIN
   SelectViewPort(1);
   SelectScale(1);
   SealL1;
   Map(-Mu, 0, xSc, ySc);
   SetFillStyle(1, Black);
   FloodFill(xSc, ySc, Yellow); 
   SetColor(ColorA);  SetFillStyle(1, ColorA);
   Map(-Mu, 0, xSc, ySc);
   FillEllipse(xSc, ySc, 5, 4); 
END;

{------------------------------------------------------------------------}

Procedure L1ToRing(RadMax: Real; VAR xHot, yHot: Real);
VAR
   xOld, yOld, xNew, yNew: Integer;
   Dx, x, y: Real;
BEGIN
   SetColor(ColorB);
   Map(L1x, 0, xOld, yOld);
   x := L1x;
   Dx := 0.002;
   REPEAT
      x := x - Dx;
      y := Shape(x, xCrit);
      Map(x, y, xNew, yNew);
      Line(xOld, yOld, xNew, yNew);
      xOld := xNew; yOld := yNew;
   UNTIL Sqrt(Sqr(x + Mu) + y*y) < RadMax;
   HotSpot(x, y, LightRed);
   xHot := x; yHot := y;
END;



PROCEDURE GrowRing(VAR MaxRadius: Real);
VAR
   Radius1, Radius2, DelRadius1, DelRadius2: Real;
   Angle, DelAngle, CosAngle, SinAngle: Real;
   k: integer;
   x1Old, y1Old, x1New, y1New: Integer;
   x2Old, y2Old, x2New, y2New: Integer;
BEGIN
   ClearLobeA;
   Messagexy(0.05, 1.6, True,
   'Through collisions, some matter loses orbital energy, spiralling ');
   Messagexy(0.05, 1.0, False, 'downward to the surface of the primary.');
   Messagexy(0.05, 0.4, False,
   'Total angular momentum of the disk is conserved, so it must expand.');
   SetColor(ColorB);
   Radius1 := Sqrt(Sqr(xHot + Mu) + yHot*yHot);
   Radius2 := Radius1;
   Angle := 0;
   Map(-Mu + Radius1, 0, x1Old, y1Old);
   x2Old := x1Old; y2Old := y1Old;
   DelRadius1 := 0.00001;
   DelRadius2 := 0.0000075;
   DelAngle := 0.1;
   SetLineStyle(SolidLn, 0, ThickWidth);
   L1ToRing(Radius2, xHot, yHot);
   SetLineStyle(SolidLn, 0, NormWidth);
   REPEAT
      SetColor(ColorB);
      Angle := Angle + DelAngle;
      CosAngle := Cos(Angle); SinAngle := Sin(Angle);
      Radius1 := Radius1 - DelRadius1;
      Radius2 := Radius2 + DelRadius2;
      Map( -Mu + Radius1*CosAngle, Radius1*SinAngle, x1New, y1New);
      Map( -Mu + Radius2*CosAngle, Radius2*SinAngle, x2New, y2New);
      Line(x1Old, y1Old, x1New, y1New);
      Line(x2Old, y2Old, x2New, y2New);
      x1Old := x1New; y1Old := y1New;
      x2Old := x2New; y2Old := y2New;
      IF Angle > 4*3.14159 THEN
      BEGIN
         Angle := Angle - 4*3.14159;
         HotSpot(xHot, yHot, ColorB);
         L1ToRing(Radius2, xHot, yHot);
      END;
   UNTIL Radius1 < 0.02;
   MaxRadius := Radius2;
END;

{------------------------------------------------------------------------}

Procedure HeatDisk(MaxRadius: Real);
VAR
   Radius, DelRadius, Angle, DelAngle, Rho1: Real;
   k: integer;
   xOld, yOld, xNew, yNew: Integer;
BEGIN
   Messagexy(0.05, 1.6, True,
   'When the disk meets the surface of the primary, it starts to heat up.');
   Messagexy(0.05, 1.0, False,
   'This temperature close to the surface may reach 10^8 K causing a');
   Messagexy(0.05, 0.4, False,
   '"nuclear runaway."');
   SetColor(15);
   SetLineStyle(SolidLn, 0, NormWidth);
   Rho1 := Sqrt(Sqr(xHot + Mu) + yHot*yHot);
   Radius := 0.02;
   Angle := 0;
   Map(-Mu + Radius, 0, xOld, yOld);
   DelRadius := 0.00001;
   DelAngle := 0.1;
   REPEAT
      Angle := Angle + DelAngle;
      Radius := Radius + DelRadius;
      Map( -Mu + Radius*Cos(Angle), Radius*Sin(Angle), xNew, yNew);
      Line(xOld, yOld, xNew, yNew);
      xOld := xNew; yOld := yNew;
   UNTIL Radius > MaxRadius;
END;

PROCEDURE Nova;
BEGIN
   Messagexy(0.1, 1, True, 'A');
   Delay(1000);
   SetTextStyle(DefaultFont,HorizDir,2);
   Messagexy(0.1, 1, False, '  A');
   Delay(1000);
   SetTextStyle(DefaultFont,HorizDir,3);
   Messagexy(0.1, 1, False, '    A');
   Delay(1000);
   SetTextStyle(DefaultFont,HorizDir,4);
   Messagexy(0.1, 1, False, '      A');
   Delay(2000);
   Messagexy(0.1, 1, False, '         CHOO');
   SetTextStyle(DefaultFont,HorizDir,1);
   Delay(1000);
   GraphBackColor := White;
   HideCursor;
   OpenViewPort(3);
   Delay(3000);
END;

{------------------------------------------------------------------------}

Procedure BackToNormal;
BEGIN
   ClearMuppetPort;
   SelectViewPort(1);
   Image.Put(Trunc(0.09*GetMaxX), 50);
   HotKeys.Display;
   DiskMenu.Display;
   Messagexy(0.1, 1, True,
   'Ready to start again! It was only a nova! Click to continue.');
   ShowMouse;
   DynamicPause;
   Proceed := False;
   GraphBackColor := Black;
   SetColor(Black);
   CloseViewPort(2);
   SetColor(Blue);
   CloseViewPort(1);
   LightMenu := True;
END;

{------------------------------------------------------------------------}

PROCEDURE GetData;
BEGIN
   IF NOT RocheData THEN
   BEGIN
      LightMenu := True;
      Proceed := False;
   END
   ELSE
   BEGIN
      Proceed := True;
      DemoNumber := 1;
      PlotRocheLobes;
      LightMenu := False;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE Advance;
BEGIN
   CASE DemoNumber OF
      1: BEGIN FillLobeB; DemoNumber := DemoNumber + 1; END;
      2: BEGIN LeakOutofLobe; DemoNumber := DemoNumber + 1; END;
      3: BEGIN GrowRing(MaxRadius); DemoNumber := DemoNumber + 1; END;
      4: BEGIN HeatDisk(MaxRadius); DemoNumber := DemoNumber + 1; END;
      5: BEGIN Nova; BackToNormal; DemoNumber := 0; END;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleDiskMenu;

BEGIN
   With DiskMenu DO
   CASE ColChosen OF
       1: CASE RowChosen OF
            1: BEGIN
                  AboutCUPS;
                  LightMenu := True;
               END; 
            2: BEGIN
                  ShowHelp('HelpBins.HLP', 'Accretion disk');
                  LightMenu := True;
               END;
            3: BEGIN
                  Configuration;
                  LightMenu := True;
               END;
            4: ExitRequest := true;
          END;
       2: BEGIN
             GetData;
             LightMenu := True;
          END;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleHotKeys(Key: Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HelpBins.HLP', 'Accretion disk');
      2: BEGIN
            IF Proceed THEN
               Advance
            ELSE Announce('Enter a mass ratio to get started');
         END;
      3: BEGIN
            DemoNumber := 0;
            Proceed := False;
            IF DiskMenu.Chosen THEN HandleDiskMenu;
         END;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE GetSetUp;
BEGIN
   SetBkColor(Black);
   ExitRequest := False;
   {LightMenu := False;}
   Mu := 0.5;
   fnX := 50;
   Nx := 50;  {Number of grid points along the x-axis.}
   fnY :=  50;
   Ny := 50;  {Number of grid points along the y-axis.}
   DefineViewPort(1, 0, 1, 0.1, 0.94);
   DefineViewPort(2, 0.05, 0.95, 0.06, 0.2);
   DefineScale(2, 0, 1, 0, 2);
   DefineViewPort(3, 0, 1, 0, 1);
END;

{------------------------------------------------------------------------}

BEGIN   {Main Program.}
{  CUPSinit;}
   GetSetUp;
   SetUpHotKeys;
   SetUpDiskMenu;
   ShowHelp('HelpBins.HLP', 'Accretion disk');
   GetData;

   REPEAT
      CheckForEvents;
      IF HotKeys.Pressed(Key) THEN HandleHotKeys(Key);
      IF LightMenu THEN
      BEGIN
         LightMenu := False;
         IF DiskMenu.Chosen THEN HandleDiskMenu;
      END
      ELSE
         IF DiskMenu.Activated THEN HandleDiskMenu;
   UNTIL  ExitRequest;
   DiskMenu.done;
{  CUPSDone;}
END;

END.
