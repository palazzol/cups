    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1993.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program to display the animated motion        *)
    (*       of n mutually attracting bodies.              *)
    (*       Quantities relevant to the virial theorem,    *)
    (*       such as the kinetic and potential energy      *)
    (*       are displayed.                                *)
    (*                                                     *)
    (*******************************************************)

unit NBody;

{$M 64000,0,327680}
{$O+}
{$F+}
interface
procedure RunNBody;

implementation
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;

procedure RunNBody;
CONST
   MaxNeq = 120;
   MaxNBodies = 20;    
   Eps = 0.001;  {Distance within which the inverse square law is replaced
                   by a constant force.}
   Dt = 0.1; {Time lapse between plotted points.}


TYPE
   Vector = Array[1..MaxNeq] of Real;

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
   y                : Vector;
   MassSum          : Real;
   NBodies          : Integer; {Number of bodies.}
   TotalEnergy      : Real;
   Mass             : Array[1..MaxNBodies] of Real;
   NBMenu           : TMenu;
   NBDat            : TInputScreen;
   HotKeys          : THotKeys;
   CUPSKeys         : THotKeys;
   ShowCUPSKeys     : Boolean;
   Key              : Byte;
   DataIn           : Boolean;
   NBDataOK         : Boolean;
   Action           : Boolean;
   LightMenu        : Boolean;
   ExitRequest      : Boolean;
   ZoomIn           : Boolean;
   ZoomFactor       : Real;
   neq              : Integer;
   Lag              : Integer;
   GetData, SetUpGraphics, PlotOrbit: Boolean;
   OldCoordinates: Array[1..MaxNBodies, 1..2] of Real;
   h                : Real;   {Stepsize.}
   NBxMin, NBxMax, NByMin, NByMax: Real;

PROCEDURE HandleNBMenu; FORWARD;

{------------------------------------------------------------------------}

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
       row(2,1, 'Enter data');
      Column(3, 'Animate');
       row(3,1, 'Animate some more');
       row(3,2, 'Remove escaped bodies');
   END;
   NBMenu.Display;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      HotKeys.Init(6);
      Key[1] := 'F2-Pause';
      Key[2] := 'F3-Faster';
      Key[3] := 'F4-Slower';
      Key[4] := 'F6-Zoom in';
      Key[5] := 'F7-Zoom out';
      Key[6] := 'F10-Menu';
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

FUNCTION NBData : Boolean;
CONST
   RBNames: String = '12345';
VAR
   FloatNBodies: Real;
   FileName: String;
BEGIN
   WITH NBDat DO
   BEGIN
      Init;
      DefineInputPort(0.17, 0.83, 0.15, 0.85);
      LoadLine('        DATA FOR THE MOTION OF N BODIES');
      LoadLine('');
      LoadLine('The number of bodies should not exceed 20.');
      LoadLine('');
      LoadLIne('     Enter the number of bodies (3 or more): {   }');
      LoadLine('');
      LoadLine('If the system is to remain bounded, it is');
      LoadLIne('necessary, although not sufficient, that');
      LoadLIne('the total energy be negative.');
      LoadLine('');
      LoadLIne('     Enter the total energy of the system: {      }');
      LoadLine('');
      LoadLine('Store the animation in a file?  No #1   Yes #1');
      LoadLine('             File name: "Nbodies1"');
      LoadLine('');
      LoadLine('           [  OK  ]           [Cancel]');
      SetNumber(1, 6);
      SetNumberLimits(1, 3, 20);
      SetNumber(2, -0.1);
      AcceptScreen;
      NBData := NOT Canceled;
      IF NOT Canceled THEN
      BEGIN
         FloatNBodies := GetNumber(1);
         NBodies := Trunc(FloatNBodies + 0.0001);
         Neq := 6*NBodies;
         TotalEnergy := GetNumber(2);
         IF GetRadioButton(RBNames[1]) = 1 THEN MakeFile := False
                                           ELSE MakeFile := True;
         FileName := GetString(5);

         IF MakeFile THEN
         BEGIN
            Assign(F1, Concat(Filename, '.POS'));
            ReWrite(F1);
         END;
      END;
      Done;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE Zoom;
VAR
   xRange, yRange  : Real;
   xC, yC          : Real;
   Inside          : Boolean;

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
   IF ChangeZoomFactor THEN
   BEGIN
      IF ZoomIn
         THEN
            Message('  Zooming in:             Click on a new center.')
         ELSE
            Message('  Zooming out:            Click on a new center.');
      REPEAT
         WaitOnMouseClick;
         MousePosn(xC, yC, 1, 1, Inside);
         IF NOT Inside THEN
         BEGIN
            Message('         Please click INSIDE the window.');
            Beep;
            Delay(500);
            IF ZoomIn
               THEN
                  Message('  Zooming in:             Click on a new center.')
               ELSE
                  Message('  Zooming out:            Click on a new center.');
         END;
      UNTIL Inside;
      IF ZoomIn THEN
      BEGIN
         xRange := (NBxMax - NBxMin)/ZoomFactor;
         yRange := (NByMax - NByMin)/ZoomFactor;
      END
      ELSE
      BEGIN
         xRange := (NBxMax - NBxMin)*ZoomFactor;
         yRange := (NByMax - NByMin)*ZoomFactor;
      END;
      NBxMin := xC - xRange/2;
      NBxMax := xC + xRange/2;
      NByMin := yC - yRange/2;
      NByMax := yC + yRange/2;
      DefineScale(1, NBxMin, NBxMax, NByMin, NByMax);
      SelectScale(1);
      GraphBackColor := Black;
      OpenViewPort(1);
      NBMenu.Display;
   END;
END;

{-------------------------------------------------------------------------}

PROCEDURE HandleHotKeys(key: Byte; ActionIn: Boolean; VAR PlotOrbit, Action: Boolean);
VAR
   DelLag: Integer;
BEGIN
   DelLag := 10;
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
      2: IF Lag > DelLag THEN Lag := Lag - DelLag;
      3: Lag := Lag + DelLag;
      4: BEGIN
            ZoomIn := True;
            Zoom;
         END;
      5: BEGIN
            ZoomIn := False;
            Zoom;
         END;
      6: BEGIN
            HotKeys.Clear;
            ShowCUPSKeys := True;
            CUPSKeys.Display;
            IF NBMenu.Chosen THEN HandleNBMenu;
         END;
   END;
END;


{--------------------------------------------------------------------}

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
   FOR n := 1 TO NBodies DO
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

{--------------------------------------------------------------------}

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


{--------------------------------------------------------------------}

Procedure Advance;
VAR
   i, j: Integer;
   Tout: Real;
   h: Real;
BEGIN
      Tout := 0;
      h := Dt;
      Delay(Lag);
      REPEAT
         Step(Tout, h, 0.00001, Y, Tout, h, Y);
      UNTIL Tout > Dt;
      Step(Tout, Dt - Tout, 0.00001, Y, Tout, h, Y);
END;

{--------------------------------------------------------------------}

Function InertiaFunction: Real;
VAR
   InF: Real;
   n: Integer;
BEGIN
   InF := 0;
   FOR n := 1 TO NBodies DO
      InF := InF + Mass[n]*(Sqr(y[6*n-5]) + Sqr(y[6*n-4]) + Sqr(y[6*n-3]));
   InertiaFunction := InF/2;
END;

{--------------------------------------------------------------------}

Function KineticEnergy: Real;
VAR
   KE: Real;
   n: Integer;
BEGIN
   KE := 0;
   FOR n := 1 TO NBodies DO
      KE := KE + Mass[n]*(Sqr(y[6*n-2]) + Sqr(y[6*n-1]) + Sqr(y[6*n]));
   KineticEnergy := KE/2;
END;

{--------------------------------------------------------------------}

Function Potential: Real;
VAR
   i, j, n: Integer;
   Pot: Real;
   Dist: Real;
BEGIN
   Pot := 0;
   FOR n := 2 TO NBodies DO
   BEGIN
      FOR i := 1 TO n-1 DO
      BEGIN
         Dist := Sqrt(Sqr(Y[6*n-6+1] - Y[6*i-6+1])
                        + Sqr(Y[6*n-6+2] - Y[6*i-6+2])
                        + Sqr(Y[6*n-6+3] - Y[6*i-6+3]));
         IF Dist < Eps THEN Dist := Eps;
         Pot := Pot + Mass[n]*Mass[i]/Dist;
      END;  {i loop.}
   END; {n loop.}
   Potential := Pot;
END;

{--------------------------------------------------------------------}

Procedure InitialConditions;
VAR
   i, n: Integer;
   R, Theta, SinPhi: Real;
   ScaleFactor: Real;
   V: Real;
   Pi: Real;
   CMassVel: Array[1..3] OF Real;
BEGIN
   Randomize;
   Pi := 3.1415926536;
   MassSum := 0;
   FOR n := 1 TO NBodies DO
   BEGIN
      Mass[n] := 0.1;
      MassSum := MassSum + Mass[n];
      R := Sqrt(Random);
      Theta := 2*Pi*Random;
      SinPhi := Random;
      Y[6*n - 6 + 1] := R*Cos(Theta)*SinPhi; {x-coordinate.}
      Y[6*n - 6 + 2] := R*Sin(Theta)*SinPhi; {y-coordinate.}
      Y[6*n - 6 + 3] := R*Sqrt(1 - SinPhi*SinPhi); {z-coordinate.}
      R := Random;
      IF R > 0.5 THEN
         Y[6*n - 6 + 3] := - Y[6*n - 6 + 3]; {Ensures positive and
                                                    negative values.}
   END;
   IF TotalEnergy < 0 THEN
   {Scale distances so that (Potential + TotalEnergy) > 0.}
   BEGIN
      ScaleFactor := - 0.5*Potential/TotalEnergy;
      FOR i := 1 TO 3 DO
         FOR n := 1 TO NBodies DO
            Y[6*n - 6 + i] := Y[6*n - 6 + i]*Scalefactor;
   END; 
   V := Sqrt(2*(Potential + TotalEnergy)/MassSum);
   FOR n := 1 TO NBodies DO
   BEGIN
      Theta := 2*Pi*Random;
      SinPhi := Random;
      Y[6*n - 6 + 4] := V*Cos(Theta)*SinPhi; {x-velocity.}
      Y[6*n - 6 + 5] := V*Sin(Theta)*SinPhi; {y-velocity.}
      Y[6*n - 6 + 6] := V*Sqrt(1 - SinPhi*SinPhi); {z-velocity.}
      R := Random;
      IF R > 0.5 THEN
         Y[6*n] := - Y[6*n]; {Ensures positive and negative values.}
   END;
   {Find the velocity components of the center of mass, and then
   subtract these from the components just found so that the amended
   components of the velocity of the center of mass are zero.}
   FOR i := 1 TO 3 DO
   BEGIN
      CMassVel[i] := 0;
      FOR n := 1 TO NBodies DO
         CMassVel[i] := CMassVel[i] + Mass[n]*Y[6*n - 3 + i];
      CMassVel[i] := CMassVel[i]/MassSum;
      FOR n := 1 TO NBodies DO
         Y[6*n - 3 + i] := Y[6*n - 3 + i] - CMassVel[i];
   END;
   {Scale velocity components to fit total energy.}
   ScaleFactor := Sqrt((Potential + TotalEnergy)/KineticEnergy);
   FOR i := 1 TO 3 DO
      FOR n := 1 TO NBodies DO
         Y[6*n - 3 + i] := Y[6*n - 3 + i]*Scalefactor;
END;

{--------------------------------------------------------------------}

PROCEDURE GetSetUp;
VAR
   MaximumX, MaximumY: Real;
   n: Integer;
   InfoStr: String;
BEGIN
   MaximumX := 0;
   MaximumY := 0;
      FOR n := 1 TO NBodies DO
      BEGIN
         IF MAximumX < Abs(Y[6*n - 6 + 1])
                   THEN MAximumX := Abs(Y[6*n - 6 + 1]);
         IF MAximumY < Abs(Y[6*n - 6 + 2])
                   THEN MAximumY := Abs(Y[6*n - 6 + 2]);
      END;
   NBxMin := -3*MaximumX;
   NBxMax := - NBxMin;
   NByMin := -3*MaximumY;
   NByMax := - NByMin;
   DefineScale(1, NBxMin, NBxMax, NByMin, NByMax);
   IF MakeFile THEN
   BEGIN
      ReWrite(F1);
      FileOpen := True;
      Points.x := Trunc(-3000*MaximumX);
      Points.y := Trunc(3000*MaximumX);
      Points.z := Trunc(-3000*MaximumY);
      Write(F1, Points);
      Points.x := Trunc(3000*MaximumY);
      Points.y := Trunc(nBodies);
      Points.z := -1;
      {A negative value here prompts PlayBack not to plot two galaxies.}    
      Write(F1, Points);
   END;
   GraphBackColor := Black;
   HideCursor;
   OpenViewPort(1);
   SelectScale(1);
   DefineScale(2, 0, 1, 0, 1);
   GraphBackColor := Red;
   OpenViewPort(2);
   SetColor(white);
   InfoStr := ('2T =            V =         '+
               '   T + V =            I =         ');
   print(10,1,InfoStr);
   h := 0.05;
   SelectViewPort(1);
   SelectScale(1);
   GraphBackColor := Black;
   ShowCUPSKeys := False;
   CUPSKeys.Clear;
   SetUpHotKeys;
   SetColor(Yellow);     
   FOR n := 1 TO NBodies DO
   BEGIN
      HideCursor; PlotSymbol(y[6*n - 5], y[6*n-4], '.'); ShowCursor;
      OldCoordinates[n, 1] := y[6*n-5];
      OldCoordinates[n, 2] := y[6*n-4];
   END;
   Action := True;
   PlotOrbit := True;
   ShowCursor;
   SetUpGraphics := False;
   NBMenu.RowActivate(3, 1, True);
END;

{--------------------------------------------------------------------}

PROCEDURE TakeAStep;
VAR
   n : Integer;
BEGIN
      Advance;
      SetColor(GraphBackColor);
      FOR n := 1 TO NBodies DO
         BEGIN
            HideCursor;
            PlotSymbol(OldCoordinates[n, 1], OldCoordinates[n, 2], '.');
            ShowCursor;
         END;
      SetColor(Yellow);
      FOR n := 1 TO NBodies DO
      BEGIN
         HideCursor; PlotSymbol(y[6*n - 5], y[6*n-4], '.'); ShowCursor;
         OldCoordinates[n, 1] := y[6*n-5];
         OldCoordinates[n, 2] := y[6*n-4];
         IF MakeFile THEN
         BEGIN
            Points.x := Trunc(1000*y[6*n - 5]);
            Points.y := Trunc(1000*y[6*n - 4]);
            Points.z := Trunc(1000*y[6*n - 3]);
            Write(F1, Points);
         END;
      END;
      SelectViewPort(2);
      SetColor(white);
      rubOut(15,1,8,red);
      print (15,1,NumStr(2*KineticEnergy,8,4));
      rubOut(30,1,8,red);
      print (30,1,NumStr(-Potential,8,4));
      rubOut(48,1,8,red);
      print (48,1,NumStr(KineticEnergy - Potential,8,4));  
      rubOut(63,1,8,red);
      print (63,1,NumStr(InertiaFunction,8,4));
      SelectViewPort(1);
      SelectScale(1);
      GraphBackColor := Black;
END;

{--------------------------------------------------------------------}

PROCEDURE RemoveEscapedBodies;
VAR
   i, j, nEscaped: Integer;
   Escaped: Boolean;

PROCEDURE TestForEscaped(i: Integer; VAR Escaped: Boolean);
VAR
   Potential, KineticEnergy, Distance: Real;
   n: Integer;
BEGIN
   Potential := 0;
   FOR n := 1 TO NBodies DO
   BEGIN
   IF n <> i THEN
      BEGIN
         Distance := Sqrt(Sqr(Y[6*n-6+1] - Y[6*i-6+1])
                        + Sqr(Y[6*n-6+2] - Y[6*i-6+2])
                        + Sqr(Y[6*n-6+3] - Y[6*i-6+3]));
         Potential := Potential + Mass[n]*Mass[i]/Distance;
      END;
   END; {n loop.}
   KineticEnergy := 0.5*Mass[i]*(Sqr(y[6*i - 2]) + Sqr(y[6*i - 1])
                                                 + Sqr(y[6*i - 0]));
   Distance := Sqrt(Sqr(y[6*i - 5]) + Sqr(y[6*i - 4]) + Sqr(y[6*i - 3]));
   IF  (KineticEnergy - Potential > 0) AND (Distance > 3)
        THEN Escaped := True
        ELSE Escaped := False;
END;

BEGIN
   nEscaped := 0;
   FOR i := 1 TO NBodies DO
   BEGIN
      TestForEscaped(i, Escaped);
      IF Escaped = True THEN nEscaped := nEscaped + 1
      ELSE
      BEGIN
         FOR j := 1 TO 6 DO
            y[6*(i - nEscaped - 1) + j] := y[6*(i-1) + j];
      END;
   END;
   NBodies := NBodies - nEscaped;
   Neq := 6*NBodies;
   OpenViewPort(1);
END;

{--------------------------------------------------------------------}

PROCEDURE InputData;
BEGIN
   IF FileOpen THEN Close(F1);
   HotKeys.Clear;
   CloseViewPort(1);
   CloseViewPort(2);
   FileOpen := False;
   PlotOrbit := False;
   Action := False;
   ZoomFactor := 2;
   IF NOT NBData THEN
   BEGIN
      MakeFile := False;
      NBMenu.RowActivate(3, 1, False);
      NBMenu.RowActivate(4, 1, False);
      LightMenu := True;
      GetData := False;
      ShowCUPSKeys := True;
      CUPSKeys.Display;
   END
   ELSE
   BEGIN
      NBMenu.RowActivate(3, 1, True);
      NBMenu.RowActivate(4, 1, True);
      GetData := False;
      SetUpGraphics := True;
   END;
END;

{--------------------------------------------------------------------}

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
                  ShowHelp('HELPNbod.HLP', 'NBodies Hello');
                  LightMenu := True;
               END;
            3: BEGIN
                  Configuration;
                  LightMenu := True;
               END;
            4: BEGIN
                  ShowHelp('HELPNbod.HLP', 'NBodies GeneralA');
                  ShowHelp('HELPNbod.HLP', 'NBodies GeneralB');
                  LightMenu := True;
               END;
            5: ExitRequest := True;
         END;
      2: BEGIN
            GetData := True;
         END;
      3: CASE RowChosen OF
            1: BEGIN
                  CupsKeys.Clear;
                  ShowCUPSKeys := False;
                  HotKeys.Display;
               END;
            2: BEGIN
                  RemoveEscapedBodies;
                  IF MakeFile THEN BEGIN Close(F1); FileOpen := False; END;
               END;
         END;
    END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleCUPSKeys(Key: Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HELPNbod.HLP', 'NBodies Hello');
      2: IF NBMenu.Chosen THEN HandleNBMenu;
   END;
END;

{--------------------------------------------------------------------}      

BEGIN   {Main Program.}
{  CUPSInit;}
   DataIn := False;
   MakeFile := False;
   FileOpen := False;
   ExitRequest := False;
   LightMenu := False;
   ShowCUPSKeys := True;
   SetUpCUPSKeys;
   Lag := 50;
   SetBkColor(Black);
   DefineViewPort(1, 0, 1, 0.07, 0.88);
   DefineViewPort(2, 0, 1, 0.89, 0.94);
   CUPSKeys.Display;
   SetUpNBMenu;
   ShowHelp('HELPNbod.HLP', 'NBodies Hello');
   InputData;
   REPEAT
      CheckForEvents;
      IF ShowCUPSKeys THEN
              IF CUPSKeys.Pressed(Key) THEN HandleCUPSKeys(Key);
      IF HotKeys.Pressed(Key) THEN
            HandleHotKeys(Key, Action, PlotOrbit, Action);
      IF LightMenu THEN
      BEGIN
         LightMenu := False;
         IF NBMenu.Chosen THEN HandleNBMenu;
      END
      ELSE
         IF NBMenu.Activated THEN HandleNBMenu;
      IF GetData THEN InputData;
      IF SetUpGraphics THEN
      BEGIN
         InitialConditions;
         GetSetUp;
      END;
      IF (PlotOrbit) AND (Action) THEN TakeAStep;
   UNTIL  ExitRequest;
   IF FileOpen THEN Close(F1);
   NBMenu.done;
{  CUPSDone;}
END;

END.

