    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1994.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program to plot curves of zero velocity       *)
    (*       in the restricted problem of three bodies.    *)
    (*       (As applied to a close binary, these are      *)
    (*       called "Roche curves.")                       *)
    (*       Radiation pressure can be included.           *)
    (*       The program also plots orbits and Poincare    *)
    (*       maps.                                         *)
    (*                                                     *)
    (*******************************************************)

Unit RocheRad;
{$M 64000,0,327680}
{$O+}
{$F+}
interface
procedure RunRocheRad;

implementation
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;

procedure RunRocheRad;
CONST
   neq  = 4;

TYPE
   Vector = Array[1..4] of Real;

VAR
   RocheMenu         : TMenu;
   OrbitHotKeys      : THotKeys;
   PoincareHotKeys   : THotKeys;
   CUPSKeys          : THotKeys;
   ShowCUPSKeys     : Boolean;
   Key               : Byte;
   MassRatio         : real;
   RP1, RP2, p, q    : Real;   {Parameters for radiation pressure.}
   Lag               : Integer;
   fnX, fnY          : real;
   nX, nY            : integer;
   Energy            : Real;
   Choice            : Integer;
   Energies          : Array[0..70, 0..70] of single;
   CStr              : String[10];
   PrintEnergy       : Boolean;
   x0, y0, xv0, yv0  : Real;
   xOld, yOld        : Real;
   State             : Vector;
   ScreenReady       : Boolean;
   MassRatioChosen   : Boolean;
   EnergyChosen      : Boolean;
   PlotOrbit         : Boolean;
   Action            : Boolean;
   DataIn            : Boolean;
   exitRequest       : Boolean;
   Poincare          : Boolean;
   LightMenu         : Boolean;
   CrossSign         : Integer;  {Changes sign at each Poincare mapped point.}
   ZoomIn            : Boolean;
   ZoomFactor        : Real;
   MapColor          : Integer;
   CxMin, CxMax, CyMin, CyMax  : Real;  {Graph limits for contours and orbits.}
   PxMin, PxMax, PyMin, PyMax  : Real;  {Graph limits for the Poincare maps.}
   Time, h           : Real;
   InputAccepted     : Boolean;
   Colors            : Array[1..10] OF Word;
   CurrentColor      : Word;

PROCEDURE HandleRocheMenu  ; FORWARD;
PROCEDURE Contour(Cz: Real); FORWARD;
PROCEDURE FindEnergies     ; FORWARD;
PROCEDURE SetUpOrbitPlot(x0, y0, xv0, yv0, Mu: Real); FORWARD;

{------------------------------------------------------------------------}

PROCEDURE SetUpRocheMenu;
BEGIN
   With RocheMenu DO
   BEGIN
      Init;
      Column(1, 'File');
       row(1,1, 'About CUPS');
       row(1,2, 'About Program');
       row(1,3, 'Configuration');
       row(1,4, 'Exit this module');
      Column(2, 'Help');
       row(2,1, 'General information');
       row(2,2, 'Photo-gravitation model');
       row(2,3, 'Poincare Maps');
       row(2,4, 'Poincare Demonstration');
      Column(3, 'Data');
       row(3,1, 'Enter data');
      Column(4, 'New Energy');
       row(4,1, 'Enter energy for new Roche curve');
       row(4,2, 'Click for the curve through a point');
       row(4,3, 'Zoom in');
       row(4,4, 'Zoom out');
      Column(5, 'Orbit');
       row(5,1, 'See an orbit for this energy');
       row(5,2, 'See Poincare maps');
      Column(6, 'CleanUp');
       row(6,1, 'Whole screen');
       row(6,2, 'Keep last curve');
   END;
   RocheMenu.Display;
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


Procedure RocheData(VAR MassRatio: real; VAR fnX, fnY: real;
                    VAR Nx, Ny: integer);
VAR
   RocheDat  : TInputScreen;
BEGIN
   WITH RocheDat DO
   BEGIN
      Init;
      DefineInputPort(0.0, 0.9, 0.08, 0.95);
      LoadLine('             DATA FOR ROCHE CURVES OF A CLOSE BINARY ');
      LoadLine('');
      LoadLine('  Enter the mass ratio; it should be between 0 and 1.  ');
      LoadLine('                      Mass ratio = {     }');
      LoadLine('');
      LoadLine('  The stars may exert radiation pressure on the small body.');
      LoadLine('  If it is distant r1 from the mass (1-mu) and r2 from the mass');
      LoadLine('  mu, then the potential is modified to be');
      LoadLine('  (1 - Rad1)(1 - mu)/r1 + (1 - Rad2)mu/r3,  0 <= Rad1, Rad2 < 1. ');
      LoadLine('  Enter values for Rad1 = {      }, Rad2 = {      }.');
      LoadLine('');
      LoadLine('  Enter extreme coordinates for the contours:');
      LoadLine('xMin = {      }, xMax = {      }, yMin = {      }, yMax = {      }');
      LoadLine('  The potential function is sampled for a grid of points in the');
      LoadLine('  orbital plane. The amount of detail can be adjusted by changing');
      LoadLine('  the numbers of grids.');
      LoadLine('     Grid points along the x-axis: {     } (30 <= nx <= 70.)');
      LoadLine('     Grid points along the y-axis: {     } (30 <= ny <= 70.)');
      LoadLine('');
      LoadLine('                [  OK  ]              [Cancel]');
      SetNumber(1, MassRatio);
      SetNumberLimits(1, 0.000001, 0.999999);
      SetNumber(2, RP1);
      SetNumberLimits(2, 0, 0.99999);
      SetNumber(3, RP2);
      SetNumberLimits(3, 0, 0.99999);
      SetNumber(4, -2);
      SetNumber(5, 2);
      SetNumber(6, -2);
      SetNumber(7, 2);
      SetNumber(8, fnX);
      SetNumberLimits(8, 30, 70);
      SetNumber(9, fnY);
      SetNumberLimits(9, 30, 70);
      AcceptScreen;
      IF Canceled THEN InputAccepted := False ELSE InputAccepted := True;
      MassRatio := GetNumber(1);
      RP1 := GetNumber(2);
      RP2 := GetNumber(3);
      CxMin := GetNumber(4);
      CxMax := GetNumber(5);
      CyMin := GetNumber(6);
      CyMax := GetNumber(7);
      fnX := GetNumber(8);
      Nx := Trunc(fnX + 0.0001);
      fnY := GetNumber(9);
      Ny := Trunc(fnY + 0.0001);
      p := Exp(Ln(1 - RP1)/3);
      q := Exp(Ln(1 - RP2)/3);
      Done;
   END;
   IF NOT InputAccepted THEN Exit;
END;

{----------------------Graphics Procedures--------------------------------}

{------------------------------------------------------------------------}

PROCEDURE Cross(xCross, yCross: Real);
VAR
   xSize, ySize: Real;
BEGIN
   xSize := (CxMax - CxMin)/300;
   ySize := (CyMax - CyMin)/200;
   PlotLine(xCross - xSize, yCross, xCross + xSize, yCross);
   PlotLine(xCross, yCross - ySize, xCross, yCross + ySize);
END;

{------------------------------------------------------------------------}

PROCEDURE FindL1(Mu: Real; VAR xL1: Real);
VAR
   fun, dfun: Real;
   delx, x1, x2: Real;
BEGIN
   xL1 := 0.5 - Mu;
   REPEAT
      x1 := xL1 + Mu;
      x2 := xL1 - 1 + Mu;
      fun := xL1*x1*x1*x2*x2 - (1 - RP1)*(1 - Mu)*x2*x2 + (1 - RP2)*Mu*x1*x1;
      dfun := x1*x1*x2*x2 + xL1*2*x1*x2*x2 + xL1*x1*x1*2*x2
                 - 2*(1 - RP1)*(1 - Mu)*x2 + 2*(1 - RP2)*Mu*x1;
      delx := - fun/dfun;
      xL1 := xL1 + delx;
   UNTIL Abs(delx) < 0.001;
END;

{------------------------------------------------------------------------}

PROCEDURE FindL2(Mu: Real; VAR xL2: Real);
VAR
   fun, dfun: Real;
   delx, x1, x2: Real;
BEGIN
   xL2 := 1 - Mu + 0.5;
   REPEAT
      x1 := xL2 + Mu;
      x2 := xL2 - 1 + Mu;
      fun := xL2*x1*x1*x2*x2 - (1 - RP1)*(1 - Mu)*x2*x2 - (1 - RP2)*Mu*x1*x1;
      dfun := x1*x1*x2*x2 + xL2*2*x1*x2*x2 + xL2*x1*x1*2*x2
                 - 2*(1 - RP1)*(1 - Mu)*x2 - 2*(1 - RP2)*Mu*x1;
      delx := - fun/dfun;
      xL2 := xL2 + delx;
   UNTIL Abs(delx) < 0.001;
END;

{------------------------------------------------------------------------}

PROCEDURE FindL3(Mu: Real; VAR xL3: Real);
VAR
   fun, dfun: Real;
   delx, x1, x2: Real;
BEGIN
   xL3 := - 0.5 - Mu;
   REPEAT
      x1 := xL3 + Mu;
      x2 := xL3 - 1 + Mu;
      fun := xL3*x1*x1*x2*x2 + (1 - RP1)*(1 - Mu)*x2*x2 + (1 - RP2)*Mu*x1*x1;
      dfun := x1*x1*x2*x2 + xL3*2*x1*x2*x2 + xL3*x1*x1*2*x2
                 + 2*(1 - RP1)*(1 - Mu)*x2 + 2*(1 - RP2)*Mu*x1;
      delx := - fun/dfun;
      xL3 := xL3 + delx;
   UNTIL Abs(delx) < 0.001;
END;

{------------------------------------------------------------------------}

PROCEDURE DrawL4L5;
VAR
   x, y: Real;
BEGIN
   IF p + q < 1 THEN Exit; {L3 and L4 do not exist.}
   x := - MassRatio + (p*p - q*q + 1)/2;
   IF p*p - (x + MassRatio)*(x + MassRatio) < 0 THEN Exit;
   y := Sqrt(p*p - (x + MassRatio)*(x + MassRatio));
   Cross(x, y);
   Cross(x, -y);
END;
  
{------------------------------------------------------------------------}

PROCEDURE SetUpGraphics;
BEGIN
   SetColor(Yellow);
   DefineViewPort(1, 0, 1, 0.07, 0.9);
   DefineScale(1, CxMin, CxMax, CyMin, CyMax);
   DefineViewPort(2, 0, 1, 0.9, 0.95);
   DefineScale(2, 0, 1, 0, 2);
   DefineViewPort(3, 0, 1, 0, 0.95);
   DefineViewPort(4, 0, 1, 0, 0.05);
   DefineScale(4, 0, 2, -1, 1);
   DefineViewPort(5, 0, 0.5, 0.07, 0.95);
   DefineScale(5, CxMin, CxMax, CyMin, CyMax);
   DefineViewPort(6, 0.5, 1, 0.07, 0.95);
   SelectScale(1);
END;

{------------------------------------------------------------------------}

Procedure SetUpScreen;
VAR
   xSc, ySc: integer;
   xL1, xL2, xL3: Real;
BEGIN
   HideCursor;
   ScreenReady := True;
   GraphBackColor := Black;
   OpenViewPort(2);
  { OpenViewPort(4); }
   OpenViewPort(1);
   SetColor(2);
   PlotSymbol(- MassRatio, 0, 'o');
   Map(- MassRatio, -(CyMax - CyMin)/80, xSc, ySc);
   OutTextXY(xSc-2, ySc, 'A');
   PlotSymbol(1 - MassRatio, 0, 'o');
   Map(1 - MassRatio, -(CyMax - CyMin)/80, xSc, ySc);
   OutTextXY(xSc-2, ySc, 'B');
   DrawL4L5;
   FindL1(MassRatio, xL1);
   Cross(xL1, 0);
   FindL2(MassRatio, xL2);
   Cross(xL2, 0);
   FindL3(MassRatio, xL3);
   Cross(xL3, 0);
   Map(CxMin, CyMin, xSc, ySc);
   OutTextxy(xSc, ySc-10, ' + Lagrangian points +');
   ShowCursor;
   RocheMenu.RowActivate(4,1, True);
   RocheMenu.RowActivate(4,2, True);
   RocheMenu.RowActivate(4,3, True);
   RocheMenu.RowActivate(4,4, True);
   IF NOT EnergyChosen THEN
   BEGIN
      RocheMenu.RowActivate(5,1, False);
      RocheMenu.RowActivate(5,2, False);
   END
   ELSE
   BEGIN
      RocheMenu.RowActivate(5,1, True);
      RocheMenu.RowActivate(5,2, True);
   END;
   RocheMenu.Display;
END;

{------------------------------------------------------------------------}

PROCEDURE Zoom;
VAR
   xSc, ySc       : integer;
   xC, yC         : Real;
   Ins            : Boolean;
   xRange, yRange : Real;
   PlotEnergy     : Boolean;
   ClickOK        : Boolean;

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
      LoadLine('             [  OK  ]  ');
      SetNumber(1, ZoomFactor);
      SetNumberLimits(1, 1, 5);
      AcceptScreen;
      ChangeZoomFactor := NOT Canceled;
      IF NOT Canceled THEN ZoomFactor := GetNumber(1);
      Done;
   END;
END;


BEGIN
   IF EnergyChosen THEN PlotEnergy := True ELSE PlotEnergy := False;
   GraphBackColor := Black;
   OpenViewPort(2);
   SelectScale(2);
   SetColor(White);
   IF ZoomIn THEN OutTextxy(MapX(0.4), MapY(1), 'Zooming in')
             ELSE OutTextxy(MapX(0.4), MapY(1), 'Zooming out');
   IF ChangeZoomFactor THEN
   BEGIN
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
      OpenViewPort(2);
      SelectScale(2);
      Map(0.1, 1, xSc, ySc);
      HideCursor;
      OutTextxy(xSc, ySc, 'Click on a new center. Click on this bar to return to default screen');
      ShowCursor;
      ClickOK := False;
      SelectViewPort(1);
      SelectScale(1);
      Delay(100);
      REPEAT
         WaitOnMouseClick;
         MousePosn(xC, yC, 1, 1, Ins);
         IF (yC < CyMax) AND Ins THEN
         BEGIN
            ClickOK := True;
            CxMin := xC - xRange/2;
            CxMax := xC + xRange/2;
            CyMin := yC - yRange/2;
            CyMax := yC + yRange/2;
         END;
         MousePosn(xC, yC, 2, 2, Ins);
         IF Ins THEN
         BEGIN
            ClickOK := True;
            CxMin := -2;
            CxMax := 2;
            CyMin := -2;
            CyMax := 2;
         END;
         IF NOT ClickOK THEN Beep;
      UNTIL ClickOK;
      DefineScale(1, CxMin, CxMax, CyMin, CyMax);
      SelectScale(1);
      SetUpScreen;
      IF PlotEnergy THEN
      BEGIN
         EnergyChosen := True;
         RocheMenu.RowActivate(5,1, True);
         RocheMenu.RowActivate(5,2, True);
      END;
      FindEnergies;
      SetColor(Colors[CurrentColor]);
      IF EnergyChosen THEN
      BEGIN
         Contour(Energy);
         RocheMenu.RowActivate(6,2, True);
      END;
   END;
END;

{------------------------------------------------------------------------}
{-----------Procedures for selecting energy and plotting contours---------}


PROCEDURE ChooseEnergy;
VAR
   EnergyDat   : TInputScreen;
BEGIN
   PlotOrbit := False;
   IF CurrentColor = 7 THEN CurrentColor := 1
                        ELSE CurrentColor := CurrentColor + 1;
   SetColor(Colors[CurrentColor]);
   WITH EnergyDat DO
   BEGIN
      Init;
      DefineInputPort(0, 1, 0.9, 0.95);
      LoadLine('For a Roche curve, C > 3.  C = {        }    [  OK  ]   [Cancel]');
      SetNumber(1, Energy); 
      AcceptScreen;
      IF Canceled THEN InputAccepted := False ELSE InputAccepted := True;
      Energy := GetNumber(1);
      Cstr := NumStr(Energy, 5, 2);
      Done;
   END;
   IF InputAccepted THEN PrintEnergy := True;
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
      val := xf*xf + yf*yf + mu*(1 - mu) +
             2*((1 - RP1)*(1 - mu)/rho1 + (1 - RP2)*mu/rho2)
             + 3*(1 - p*p*(1 - mu) - q*q*mu);
      if val > 50 then val := 50;
      EnergyFun := val;
   END;
END;

 
PROCEDURE FindEnergies;
VAR
   i, j: Integer;
   xc, yc: Real;
BEGIN
   FOR i := 0 TO Nx DO
   BEGIN
      FOR j := 0 TO Ny DO
      BEGIN
         xc := CxMin + i*(CxMax - CxMin)/fNx;
         yc := CyMin + j*(CyMax - CyMin)/fNy;
         Energies[i, j] := EnergyFun(xc, yc, MassRatio);
      END;
   END;
END;
{------------------------------------------------------------------------}   

Procedure Coordinates(Ci, Cj: integer; VAR xc, yc, zc: real);
{Calculates x, y and z coordinates, given the i and j coordinates
in the base grid.}
BEGIN
   xc := CxMin + Ci*(CxMax - CxMin)/fNx;
   yc := CyMin + Cj*(CyMax - CyMin)/fNy;
   zc := Energies[Ci, Cj];
END;

{------------------------------------------------------------------------}

Procedure TriCTest(nx1, ny1, nx2, ny2: integer; tx3, ty3, tz3, Cz: real);
VAR
   Pc:  integer;
   tc:  real;
   tx1, ty1, tz1, tx2, ty2, tz2: real;
   xCross1, yCross1, xCross2, yCross2: real;
   XPrint, YPrint: Integer;
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
     PlotLine(xCross1, yCross1, xCross2, yCross2);
     IF PrintEnergy THEN
     BEGIN
        Map(xCross1, yCross1, XPrint, YPrint);
        OutTextxy(XPrint-20, YPrint, Cstr);
        PrintEnergy := False;
     END;
  END;
END;

{------------------------------------------------------------------------}

PROCEDURE Contour(Cz: Real);
{Draws a contour for the selected energy, Cz.}
VAR
   i, j: integer;
   tx1, ty1, tz1, tx2, ty2, tz2, tx3, ty3, tz3: real;
BEGIN
   IF Poincare THEN
   BEGIN
      SelectViewPort(5); SelectScale(1);
   END
   ELSE
   BEGIN
     SelectViewPort(1); SelectScale(1);
   END;
   HideCursor;
   Cz := Cz + 0.00001; 
   FOR i := 1 TO Nx DO
      BEGIN         
         FOR j := 1 TO Ny DO
            BEGIN
               Coordinates(i-1, j-1, tx1, ty1, tz1);
               Coordinates(i, j, tx2, ty2, tz2);
               tx3 := (tx1 + tx2)/2;
               ty3 := (ty1 + ty2)/2;
               tz3 := (tz1 + tz2)/2; {EnergyFun(tx3, ty3, MassRatio);}
               TriCTest(i-1, j-1, i, j-1, tx3, ty3, tz3, Cz);
               TriCTest(i-1, j-1, i-1, j, tx3, ty3, tz3, Cz);
               TriCTest(i, j, i-1, j, tx3, ty3, tz3, Cz);
               TriCTest(i, j, i, j-1, tx3, ty3, tz3, Cz);
            END;  {j loop.}
      END; {i loop.}  
   ShowCursor;
   RocheMenu.Rowactivate(5,1, True);
   RocheMenu.RowActivate(5,2, True);
   RocheMenu.RowActivate(6,2, True);
END;

{------------------------------------------------------------------------}

PROCEDURE FindNewEnergy;
VAR
   xSc, ySc: Integer;
BEGIN
      OpenViewPort(2);
     { OpenViewPort(4);}
      PlotOrbit := False;
      Action := False;
      OrbitHotKeys.BackColor := Blue;
      OrbitHotKeys.Clear;
      CUPSKeys.Display;
      ChooseEnergy;
      If NOT InputAccepted THEN
      BEGIN
         OpenViewPort(2);
         LightMenu := True;
         Exit;
      END;
      IF Energy <= 3 THEN
      BEGIN
         HideCursor;
         OpenViewPort(2);
         SelectScale(2);
         Map(0.1, 1, xSc, ySc);
         OutTextxy(xSc, ySc, '     No Roche curve for this energy, C = ' + Cstr);
         ShowCursor;
      END
      ELSE Contour(Energy);
      EnergyChosen := True;
      RocheMenu.RowActivate(5,1, True);
      RocheMenu.RowActivate(5,2, True);
      RocheMenu.RowActivate(6,2, True);
      LightMenu := True;
      ShowCursor;
END;

{------------------------------------------------------------------------}

PROCEDURE ClickForEnergy;
VAR
   xSc, ySc: integer;
   xm, ym: Word;
   xC, yC: Real;
   Button: Word;
   Ins: Boolean;

BEGIN
   OpenViewPort(2);
   SelectScale(2);
   PlotOrbit := False;
   Action := False;
   OrbitHotKeys.BackColor := Blue;
   OrbitHotKeys.Clear;
   CUPSKeys.Display;
   IF CurrentColor = 7 THEN CurrentColor := 1
                        ELSE CurrentColor := CurrentColor + 1;
   SetColor(Colors[CurrentColor]);
   Map(0.1, 1, xSc, ySc);
   HideCursor;
   OutTextxy(xSc, ySc, 'Click on a point to see the Roche curve through that point');
   ShowCursor;
   SelectViewPort(1);
   SelectScale(1);
   Delay(100);
   WaitOnMouseClick;
   MousePosn(xC, yC, 1, 1, Ins);
   Energy := EnergyFun(xC, yC, MassRatio);
   CStr := NumStr(Energy, 5, 2);
   PrintEnergy := True;
   Contour(Energy);
   OpenViewPort(2);
   SelectViewPort(1);
   EnergyChosen := True;
   RocheMenu.RowActivate(5,1, True);
   RocheMenu.RowActivate(5,2, True);
   RocheMenu.RowActivate(6,2, True);
   LightMenu := True;
   ShowCursor;
END;

{-------End of procedures for choosing energy and plotting contours------}

{-------------------Procedures for integrating orbits--------------------}

Procedure Fun(x: real; y: Vector; VAR z: Vector);
VAR
   Term1, Term2: real;
{y[1] and y[3] are components of position; y[2] and y[4] are components of velocity,}
BEGIN
   Term1 := Sqr(y[1]+MassRatio) + Sqr(y[3]);
   Term2 := Sqr(y[1]-1+MassRatio) + Sqr(y[3]);
   Term1 := (1 - RP1)*(1-MassRatio)/(Term1 * Sqrt(Term1)) ;
   Term2 := (1 - RP2)*MassRatio/(Term2 * Sqrt(Term2)) ;
   z[1] := y[2];
   z[3] := y[4];
   z[2] := 2*y[4]+y[1]-(y[1]+MassRatio)*Term1 - (y[1]-1+MassRatio)*Term2 ;
   z[4] := -2*y[2]+y[3]*(1 - Term1 - Term2);
END;

{-----------------------------------------------------------------------}

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

{--------------End of procedures for integrating orbits-----------------}
{------------------Procedures for plotting orbits-----------------------}

Procedure ChooseOrbit(Energy, Mu: Real; VAR x0, y0, xv0, yv0: Real);
VAR
   Inside, Ins: Boolean;
   x1, y1, r1, v1: Real;
   xSc, ySc: Integer;
   i: Integer;
   Button: Integer;
   xm, ym: Integer;
   
Procedure Arrow(xa, ya, xb, yb: Real);
VAR
   x, y: Real;
   Fraction: Real;
BEGIN
   SetColor(White);
   Fraction := 0.2;
   PlotLine(xa, ya, xb, yb);
   x := (xb - Fraction*(xb - xa) + Fraction*(yb - ya));
   y := (yb - Fraction*(yb - ya) - Fraction*(xb - xa));
   PlotLine(xb, yb, x, y);
   x := (xb - Fraction*(xb - xa) - Fraction*(yb - ya));
   y := (yb - Fraction*(yb - ya) + Fraction*(xb - xa));
   PlotLine(xb, yb, x, y);
END;


BEGIN
   GraphBackColor := Blue;
   HideCursor;
   ClearMKBuffers;
   OpenViewPort(2);
   SelectScale(2);
   Inside := False;
   Map(0, 1.8, xSc, ySc);
   SetColor(Yellow);
   OutTextxy(xSc, ySc, '     Click on initial point, consistent with the energy.');
   Map(0, 0.8, xSc, ySc);
   OutTextxy(xSc, ySc, '     Click outside the window to quit.');
   SelectViewPort(1);
   SelectScale(1);
   ShowCursor;
   REPEAT
      WaitOnMouseClick;
      MousePosn(x0, y0, 1, 1, Ins);
      IF (EnergyFun(x0, y0, Mu) < Energy) THEN
                      BEGIN
                         Sound(440);
                         Delay(100);
                         NoSound;
                      END
                      ELSE Inside := True;
   UNTIL (Inside = True) OR (Ins = False);
   IF NOT Ins THEN
   BEGIN
      LightMenu := True;
      DataIn := False;
      OpenViewPort(2);
      SelectViewPort(1);
      SelectScale(1);
      Exit;
   END;
   DataIn := True;
   OpenViewPort(2);
   SelectScale(2);
   Inside := False;
   Map(0, 1.8, xSc, ySc);
   SetColor(Yellow);
   HideMouse;
   OutTextxy(xSc, ySc, '     Click on initial point, consistent with the energy.');
   Map(0, 0.8, xSc, ySc);
   OutTextxy(xSc, ySc, '     Click again at a point in the direction of the velocity.');
   SelectViewPort(1);
   SelectScale(1);
   ShowMouse;
   SetWriteMode(XORPut);
   x1 := x0; y1 := y0;
   REPEAT
      MouseGlobalPosn(xm, ym, Button);
      MousePosn(x1, y1, 1, 1, Ins);
      Arrow(x0, y0, x1, y1);
      Delay(10);
      Arrow(x0, y0, x1, y1);
   UNTIL Button <> 0;
   SetWriteMode(CopyPut);
   {WaitOnMouseClick;
   MousePosn(x1, y1, 1, 1, Ins);}
   r1 := Sqrt(Sqr(x1 - x0) + Sqr(y1 - y0));
   x1 := (x1 - x0)/r1; y1 := (y1 - y0)/r1; {Unit vector for velocity.}
   v1 := Sqrt(EnergyFun(x0, y0, Mu) - Energy); {Initial speed.}
   xv0 := v1*x1; yv0 := v1*y1;
   GraphBackColor := Blue;
   HideCursor;
   OpenViewPort(2);
   ShowCursor;
   SelectViewPort(1);
   SelectScale(1);
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpOrbitHotKeys;
BEGIN
   WITH OrbitHotKeys DO
   BEGIN
      OrbitHotKeys.Init(6);
      Key[1] := 'F2-Stop';
      Key[2] := 'F3-Faster';
      Key[3] := 'F4-Slower';
      Key[4] := 'F6-Zoom In';
      Key[5] := 'F7-Zoom Out';
      Key[6] := 'F10-Menu';
   END;
   OrbitHotKeys.BackColor := Blue;
   OrbitHotKeys.HotKeyColor := Yellow;
   {OrbitHotKeys.Display;}
END;

{------------------------------------------------------------------------}

PROCEDURE HandleOrbitHotKeys(key: Byte);
VAR
   DelLag: Integer;
BEGIN
   DelLag := 20;
      CASE Key OF
      1: BEGIN
            IF Action = True THEN
            BEGIN
               Action := False;
               WITH OrbitHotKeys DO Key[1] := 'F2-Run';
               OrbitHotKeys.BackColor := Red;
               OrbitHotKeys.Display;
               SetColor(Yellow);
            END
            ELSE
            BEGIN
               Action := True;
               WITH OrbitHotKeys DO Key[1] := 'F2-Stop';
               OrbitHotKeys.BackColor := Blue;
               OrbitHotKeys.Display;
               SetColor(Yellow);
            END;
         END;
      2: IF Lag > DelLag THEN Lag := Lag - DelLag;
      3: Lag := Lag + DelLag;
      4: BEGIN
            ZoomIn := True;
            Zoom;
            OrbitHotKeys.Display;
         END;
      5: BEGIN
            ZoomIn := False;
            Zoom;
            OrbitHotKeys.Display;
         END;
      6: BEGIN
               PlotOrbit := False;
               Action := False;
               OrbitHotKeys.BackColor := Blue;
               OrbitHotKeys.Clear;
               LightMenu := False;
               ChooseOrbit(Energy, MassRatio, x0, y0, xv0, yv0);
               IF DataIn THEN SetUpOrbitPlot(x0, y0, xv0, yv0, MassRatio)
               ELSE
               BEGIN
                  ShowCUPSKeys := True;
                  CUPSKeys.Display;
                  LightMenu :=True;
               END;
              { GraphBackColor := Blue;
               OpenViewPort(2);}
         END;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpOrbitPlot(x0, y0, xv0, yv0, Mu: Real);
VAR
   xSc, ySc: Integer;
BEGIN
   SelectViewPort(1);
   SelectScale(1);
   State[1] := x0;
   State[2] := xv0;
   State[3] := y0;
   State[4] := yv0;
   Time := 0;
   h := 0.1;
   xOld := x0; yOld := y0;
   Lag := 100;
   CUPSKeys.Clear;
   ShowCUPSKeys := False;
   WITH OrbitHotKeys DO Key[1] := 'F2-Stop';
               OrbitHotKeys.BackColor := Blue;
   OrbitHotKeys.Display;
   GraphBackColor := Red;
   SetColor(Yellow);
   HideCursor;
   OpenViewPort(2);
   Map(-1.5, 1, xSc, ySc);
   OutTextxy(xSc, ySc, 'WARNING! You are in a rotating reference system!');
   ShowCursor;
   SetColor(Colors[CurrentColor]);
   GraphBackColor := Black;
   SelectViewPort(1);
   SelectScale(1);
   ShowCursor;
   PlotOrbit := True;
   Action := True;
   Lightmenu := False;
END;

{-----------------------------------------------------------------------}

PROCEDURE TakeOrbitStep;
VAR
   xNew, yNew: Real;
BEGIN
     Delay(Lag);
     Step(Time, h, 0.000001, State, Time, h, State);
     xNew := State[1]; yNew := State[3];
     PlotLine(xOld, yOld, xNew, yNew);
     xOld := xNew; yOld := yNew;
END;

{------------------End of procedures for plotting orbits------------------}
{------------------Procedures for plotting Poincare maps------------------}

PROCEDURE MakeMapScreen;
VAR
   xSc, ySc: Integer;
   BoundStr: String;
   x, Delx : Real;
   Test    : Real;
   i       : Integer;
   Region  : Boolean;
   yOld, xOld    : Real;
BEGIN
   GraphBackColor := Black;
{   SetBkColor(Black);}
   HideCursor;
   OpenViewPort(6);
   SelectScale(6);
   SetColor(Yellow);
   IF PyMin*PyMax < 0 THEN
   BEGIN
      PlotLine(PxMin, 0, PxMax, 0);
      Map(PxMin, 0, xSc, ySc);
      Str(PxMin:3:1, BoundStr);
      OutTextxy(xSc, ySc-10, BoundStr);
      Map(PxMax, 0, xSc, ySc);
      Str(PxMax:3:1, BoundStr);
      OutTextxy(xSc-30, ySc-10, BoundStr);
      OutTextxy(xSc-15, ySc, 'x');
   END
   ELSE
   BEGIN
      Map(PxMin, 0.5*(PyMin + PyMax), xSc, ySc);
      Str(PxMin:3:1, BoundStr);
      OutTextxy(xSc, ySc, BoundStr);
      Map(PxMax, 0.5*(PyMin + PyMax), xSc, ySc);
      Str(PxMax:3:1, BoundStr);
      OutTextxy(xSc-30, ySc, BoundStr);
      OutTextxy(xSc-15, ySc+10, 'x');
   END;
   IF PxMin*PxMax < 0 THEN
   BEGIN
      PlotLine(0, PyMin, 0, PyMax);
      Map(0, PyMin, xSc, ySc);
      Str(PyMin:4:1, BoundStr);
      OutTextxy(xSc+5, ySc-10, BoundStr);
      Map(0, PyMax, xSc, ySc);
      Str(PyMax:4:1, BoundStr);
      OutTextxy(xSc+5, ySc+10, BoundStr);
      OutTextxy(xSc+5, ySc+20, 'xDot');
   END
   ELSE
   BEGIN
      Map(0.5*(PxMin + PxMax), PyMin, xSc, ySc);
      Str(PyMin:4:1, BoundStr);
      OutTextxy(xSc, ySc-10, BoundStr);
      Map(0.5*(PxMin + PxMax), PyMax, xSc, ySc);
      Str(PyMax:4:1, BoundStr);
      OutTextxy(xSc, ySc+10, BoundStr);
      OutTextxy(xSc, ySc+20, 'xDot');
   END;
   SetColor(LightGray);
   Delx := (PxMax - PxMin)/100;
   Region := False;
   FOR i := 0 TO 100 DO
   BEGIN
      x := PxMin + i*Delx;
      Test := EnergyFun(x, 0, MassRatio) - Energy;
      IF (i = 0) AND (Test > 0) THEN
      BEGIN
         yOld := Sqrt(Test);
         xOld := x;
         Region := True;
      END;
      IF (NOT Region) AND (Test > 0) THEN
      BEGIN
         yOld := Sqrt(Test);
         xOld := x;
         Region := True;
         Line(MapX(x), MapY(yOld), MapX(x), MapY(-yOld));
      END;
      IF (Region) AND (Test < 0) THEN
      BEGIN
         Region := False;
         Line(MapX(xOld), MapY(yOld), MapX(xOld), MapY(-yOld));
      END;
      IF Region AND (Test > 0) THEN
      BEGIN
         Test := Sqrt(Test);
         Line(MapX(xOld), MapY(yOld), MapX(x), MapY(Test));
         Line(MapX(xOld), MapY(-yOld), MapX(x), MapY(-Test));
         xOld := x;
         yOld := Test;
         {PutPixel(MapX(x), MapY(Test), LightGray);
         PutPixel(MapX(x), MapY(-Test), LightGray);}
      END;
   END;
   ShowCursor;
END;

{------------------------------------------------------------------------}

PROCEDURE SetNewMapLimits;
VAR
   i: Integer;
   MapDat  : TInputScreen;
BEGIN
   ShowCursor;
   WITH MapDat DO
   BEGIN
      Init;
      DefineInputPort(0, 1, 0.9, 0.95);
      LoadLine('Min x = {     } Max x = {     } Min y = {     } Max y = {     } [  OK  ][Cancel]');
      SetNumber(1, PxMin);
      SetNumber(2, PxMax);
      SetNumber(3, PyMin);
      SetNumber(4, PyMax);
      REPEAT
         AcceptScreen;
         IF Canceled THEN InputAccepted := False ELSE InputAccepted := True;
         PxMin := GetNumber(1);
         PxMax := GetNumber(2);
         PyMin := GetNumber(3);
         PyMax := GetNumber(4);
         IF (PxMin > PxMax)OR (PyMin > PyMax) THEN Beep;
      UNTIL (PxMin < PxMax) AND (PyMin < PyMax);
      done;
   END;
   DefineScale(6, PxMin, PxMax, PyMin, PyMax);
   IF InputAccepted THEN MakeMapScreen;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpPoincareHotKeys;
BEGIN
   WITH PoincareHotKeys DO
   BEGIN
      PoincareHotKeys.Init(5);
      Key[1] := 'F1-Help';
      Key[2] := 'F2-Stop';
      Key[3] := 'F3-NewMap';
      Key[4] := 'F4-ReScale';
      Key[5] := 'F10-Menu';
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE StartPoincareMap;
VAR
   Inside: Boolean;
   VSquare: Real;
   xSc, ySc: Integer;

PROCEDURE ChangeMapColor(ColorIn: Integer; VAR ColorOut: Integer);
BEGIN
   ColorOut := ColorIn - 1;
   IF ColorOut = 8 THEN ColorOut := 7;
   IF ColorOut = 6 THEN ColorOut := 3;
   IF ColorOut = 0 THEN ColorOut := 15;
END;


BEGIN
   y0 := 0;
   OpenViewPort(5);
   ChangeMapColor(MapColor, MapColor);
   SetColor(white);
   SelectScale(1);
   Contour(Energy);
   HideCursor;
   GraphBackColor := Blue;
   OpenViewPort(4);
   SelectScale(4);
   Map(0.5, 0, xSc, ySc);
   OutTextxy(xSc, ySc, 'Click on a starting point inside a permitted region');
   ShowCursor;
   SetColor(MapColor);
   MouseBorders(Trunc(GetMaxX/2), Trunc(0.93*GetMaxY),
                      GetMaxX, Trunc(0.05*GetMaxY));
   SelectViewPort(6);
   SelectScale(6);
   REPEAT
      WaitOnMouseClick;
      MousePosn(x0, xv0, 6, 6, Inside);
      IF xV0 > PyMax THEN
      BEGIN
         MouseBorders(0, GetMaxY, GetMaxX, 0);
         LightMenu := True;
         Exit;
      END;
      VSquare := EnergyFun(x0, y0, MassRatio) - Energy - xv0*xv0;
      IF VSquare < 0 THEN
      BEGIN
         Sound(440);
         Delay(100);                       
         NoSound;
      END;
   UNTIL VSquare > 0;
   yv0 := Sqrt(VSquare);
   SetColor(MapColor);
   PlotSymbol(x0, xv0, '.');
   MouseBorders(0, GetMaxY, GetMaxX, 0);
   State[1] := x0;
   State[2] := xv0;
   State[3] := y0;
   State[4] := yv0;
   Time := 0;
   h := 0.1;
   xOld := x0; yOld := y0;
   SelectViewPort(5);
   SelectScale(1);
   CrossSign := 1;
   CUPSKeys.Clear;
   ShowCUPSKeys := False;
   PoincareHotKeys.Display;
   PlotOrbit := True;
END;

{------------------------------------------------------------------------}

PROCEDURE FindPoincareCrossing(LocalState: Vector);
VAR
   StepSize: Real;
   Counter: Integer;
   StoreColor: Integer;
BEGIN
   Counter := 0;
   REPEAT
      Counter := Counter + 1;
      StepSize := - LocalState[3]/LocalState[4];
      Step(Time, StepSize, 0.00001, LocalState, Time, StepSize, LocalState);
   UNTIL (Abs(LocalState[3]) < 0.0001) OR (Counter = 10);
   IF Counter < 10 THEN
   BEGIN
      SelectViewPort(6);
      SelectScale(6);
      StoreColor := GetColor;
      SetColor(MapColor);
      PlotSymbol(LocalState[1], LocalState[2], '.');
      SetColor(StoreColor);
      SelectViewPort(5);
      SelectScale(1);
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE TakePoincareStep;
VAR
   xNew, yNew: Real;
BEGIN
   Step(Time, h, 0.000001, State, Time, h, State);
   IF CrossSign*State[3] < 0 THEN
   BEGIN
      FindPoincareCrossing(State);
      CrossSign := - CrossSign;  
   END;
   xNew := State[1]; yNew := State[3];
   PlotLine(xOld, yOld, xNew, yNew);
   xOld := xNew; yOld := yNew;
END;

{-----------------------------------------------------------------------}

PROCEDURE HandlePoincareHotKeys(Key: Byte);
BEGIN
            CASE Key OF
               1: ShowHelp('HelpBins.HLP', 'Roche Poincare');
               2: BEGIN
                     PlotOrbit := False;
                     Poincare := False;
                     OpenViewPort(1); 
                     SelectScale(1);
                     PoincareHotKeys.Clear;
                     SetUpScreen;
                     ShowCUPSKeys := True;
                     CUPSKeys.Display;
                     Contour(Energy);
                     EnergyChosen := True;
                     RocheMenu.RowActivate(5,1, True);
                     RocheMenu.RowActivate(5,2, True);
                     RocheMenu.RowActivate(6,2, True);
                     LightMenu := True;
                  END;
               3: BEGIN
                     PlotOrbit := False;
                     StartPoincareMap;
                  END;
               4: BEGIN
                     PlotOrbit := False;
                     SetNewMapLimits;
                     StartPoincareMap;
                  END;
               5: BEGIN
                     Poincare := False;
                     PlotOrbit := False;
                     PoincareHotKeys.Clear;
                     ClearMuppetPort;
                     SetUpScreen;
                     Contour(Energy);
                     EnergyChosen := True;
                     RocheMenu.RowActivate(5,1, True);
                     RocheMenu.RowActivate(5,2, True);
                     RocheMenu.RowActivate(6,2, True);
                     LightMenu := True;
                     CUPSKeys.Display;
                     ShowCUPSKeys := True;
                  END;
            END;
END;


{-----------------------------------------------------------------------}

PROCEDURE MakePoincareMap;
BEGIN
   Poincare := True;
   HideCursor;
   PxMin := -2;
   PxMax := 2;
   PyMin := -5;
   PyMax := 5;
   DefineScale(6, PxMin, PxMax, PyMin, PyMax);
   MakeMapScreen;
   OpenViewPort(5);
   MapColor := 2;
   ShowCursor;
   StartPoincareMap;
END;

{--------------End of procedures for plottting Poincare maps- -----------}

{------------------------------------------------------------------------}

PROCEDURE GetData;
VAR
   LimitsOK: Boolean;
BEGIN
   ClearMuppetPort;
   RocheMenu.Display;
   EnergyChosen := False;
   RocheMenu.RowActivate(4,1, False);
   RocheMenu.RowActivate(4,2, False);
   RocheMenu.RowActivate(4,3, False);
   RocheMenu.RowActivate(4,4, False);
   RocheMenu.RowActivate(5,1, False);
   RocheMenu.RowActivate(5,2, False);
   RocheMenu.RowActivate(6,1, False);
   RocheMenu.RowActivate(6,2, False);
   Poincare := False;
{      SetBkColor(Black);}
      GraphBackColor := Black;
      REPEAT
         RocheData(MassRatio, fnX, fnY, Nx, Ny);
         IF (CxMax <= CxMin+0.00001) OR (CyMax <= CyMin+0.00001) THEN
         BEGIN
            Message('            Error in extreme coordinates');
            LimitsOK := False;
         END
         ELSE
         BEGIN
            LimitsOK := True;
            DefineScale(1, CxMin, CxMax, CyMin, CyMax);
         END;
      UNTIL (LimitsOK) OR (NOT InputAccepted);
      IF NOT InputAccepted THEN
      BEGIN
         IF ScreenReady = False THEN
         BEGIN
            RocheMenu.RowActivate(4,1, False);
            RocheMenu.RowActivate(4,2, False);
            RocheMenu.RowActivate(4,3, False);
            RocheMenu.RowActivate(4,4, False);
         END;
         IF MassRatioChosen = False THEN
         BEGIN
            RocheMenu.RowActivate(6,1, False);
            RocheMenu.RowActivate(6,2, False);
         END;
         RocheMenu.Display;
         LightMenu := True;
         ShowCUPSKeys := True;
         CUPSKeys.Display;
         Exit;
      END;
   RocheMenu.RowActivate(6,1, True);
   IF EnergyChosen THEN RocheMenu.RowActivate(6,2, True)
                   ELSE RocheMenu.RowActivate(6,2, False);
   MassRatioChosen := True;
   SetUpScreen;
   RocheMenu.Display;
   ShowCUPSKeys := True;
   CUPSKeys.Display;
   FindEnergies;
   FindNewEnergy;
      RocheMenu.RowActivate(4,1, True);
   LightMenu := True;
   END;

{------------------------------------------------------------------------}

PROCEDURE RunPoincareDemo;

VAR
   ThreeDHotKeys: THotKeys;
   Lag       : Integer;
   State: Vector;
   Action           : Boolean;
   Demo: Boolean;
   Radius, Azimuth, TwistAngle, Altitude: Real;
   Caz, Saz, Calt, Salt, CTwist, STwist: Real;
   SaveMassRatio: Real;

PROCEDURE HandleThreeDHotKeys(Key: Byte; VAR Action: Boolean); FORWARD;

{------------------------------------------------------------------------}

PROCEDURE SetUpThreeDHotKeys;
BEGIN
   WITH ThreeDHotKeys DO
   BEGIN
      ThreeDHotKeys.Init(6);
      Key[1] := 'F1-Help';
      Key[2] := 'F2-Dem1';
      Key[3] := 'F3-Dem2';
      Key[4] := 'F4-Dem3';
      Key[5] := 'F5-Dem4';
      Key[6] := 'F6-Quit';
   END;
   ThreeDHotKeys.BackColor := Blue;
   ThreeDHotKeys.HotKeyColor := Yellow;
   ThreeDHotKeys.Display;
END;

{-----------------------------------------------------------------------}

PROCEDURE TrigFunctions;
BEGIN
   Calt := Cos(Altitude);
   Salt := Sin(Altitude);
   Caz := Cos(Azimuth);
   Saz := Sin(Azimuth);
   CTwist := Cos(TwistAngle);
   STwist := Sin(TwistAngle);
END;

{------------------------------------------------------------------------}

PROCEDURE View(x, y, z: Real; VAR XView, YView: Real);
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

PROCEDURE Arrowx( x0, y0, z0, x1, y1, z1 : REAL; CoordStr: String);


VAR
   xorg, yorg   : REAL ;
   xend, yend   : REAL ;
   ybarb,zbarb  : REAL ;
   xSc, ySc: Integer;
   BarbSize, Length: Real;
BEGIN
   Length := Sqrt(Sqr(x1-x0)+Sqr(y1-y0)+Sqr(z1-z0));
   BarbSize := 0.1/Length;
   View(x0,y0,z0,xorg,yorg);   { Plot line from origin to terminus.}
   View(x1,y1,z1,xend,yend);
   PlotLine(xorg,yorg,xend,yend);
   Map(xend+(xend-xorg)/10, yend+(yend-yorg)/10, xSc, ySc);
   OutTextxy(xsc, ysc, CoordStr);
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

PROCEDURE Arrowy( x0, y0, z0, x1, y1, z1 : REAL; CoordStr: String);


VAR
   xorg, yorg   : REAL ;
   xend, yend   : REAL ;
   xbarb,zbarb  : REAL ;
   xSc, ySc: Integer;
   BarbSize, Length: Real;
BEGIN
   Length := Sqrt(Sqr(x1-x0)+Sqr(y1-y0)+Sqr(z1-z0));
   BarbSize := 0.1/Length;
   View(x0,y0,z0,xorg,yorg);   { Plot line from origin to terminus.}
   View(x1,y1,z1,xend,yend);
   PlotLine(xorg,yorg,xend,yend);
   Map(xend+(xend-xorg)/10, yend+(yend-yorg)/10, xSc, ySc);
   OutTextxy(xsc, ysc, CoordStr);
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

PROCEDURE Arrowz( x0, y0, z0, x1, y1, z1 : REAL; CoordStr: String);


VAR
   xorg, yorg   : REAL ;
   xend, yend   : REAL ;
   xbarb,ybarb  : REAL ;
    xSc, ySc: Integer;
   BarbSize, Length: Real;
BEGIN
   Length := Sqrt(Sqr(x1-x0)+Sqr(y1-y0)+Sqr(z1-z0));
   BarbSize := 0.1/Length;
   View(x0,y0,z0,xorg,yorg);   { Plot line from origin to terminus.}
   View(x1,y1,z1,xend,yend);
   PlotLine(xorg,yorg,xend,yend);
   Map(xend+(xend-xorg)/10, yend+(yend-yorg)/10, xSc, ySc);
   OutTextxy(xsc, ysc, CoordStr);
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

{-------------------------------------------------------------------------}

PROCEDURE FindPoincareCrossing(LocalState: Vector; MapColor: Word);
VAR
   StepSize: Real;
   Counter: Integer;
   StoreColor: Integer;
   xCross, yCross: Real;
BEGIN
   Counter := 0;
   REPEAT
      Counter := Counter + 1;
      StepSize := - (LocalState[3])/LocalState[4];
      Step(Time, StepSize, 0.00001, LocalState, Time, StepSize, LocalState);
   UNTIL (Abs(LocalState[3]) < 0.0001) OR (Counter = 10);
   IF Counter < 10 THEN
   BEGIN
      HideCursor;
      SetColor(Yellow);
      View(LocalState[1], LocalState[2], LocalState[3], xCross, yCross);
      PlotSymbol(xCross, yCross, '.');
      ShowCursor;
   END;
END;

{-----------------------------------------------------------------------}

PROCEDURE ThreeDMotion;
VAR
  
   xOld, yOld, xNew, yNew: Real;
   CrossSign: Integer;
   MaxX, MaxY, MaxZ: Real;
   
BEGIN
   CrossSign := 1;
   MaxX := 1;
   MaxY := 5;
   MaxZ := 1;
   Time := 0;
   lag := 10;
   h := 0.1;
   DefineViewPort(7, 0, 1, 0.05, 0.95);
   DefineScale(7, -MaxX*1.2, MaxX*1.2, -MaxZ*1.2, MaxZ*1.2);
   GraphBackColor := Black;
   OpenViewPort(7);
   SelectScale(7);
   Altitude := 0.1;
   Azimuth := 1.5;
   TwistAngle := 0.0;
   TrigFunctions;
   SetColor(LightBlue);
   Arrowy(0, 0, 0, MaxX, 0, 0, 'X');
   Arrowz(0, 0, 0, 0, MaxY, 0, 'Xdot');
   Arrowy(0, 0, 0, 0, 0, MaxZ, 'Y');
   View(State[1], State[2], State[3], xOld, yOld);
   Action := True;
   REPEAT
      Delay(lag);
      Step(Time, h, 0.000001, State, Time, h, State);
      IF CrossSign*State[3] < 0.0 THEN
         BEGIN
            FindPoincareCrossing(State, Yellow);
            CrossSign := - CrossSign;
         END;
      View(State[1], State[2], State[3], xNew, yNew);
      HideCursor;
      SetColor(lightGray);
      PlotLine(xOld, yOld, xNew, yNew);  
      ShowCursor;
      xOld := xNew; yOld := yNew;
      CheckForEvents;
      IF ThreeDHotKeys.Pressed(Key) THEN
                          HandleThreeDHotKeys(Key, Action);
   UNTIL (Not Action);
END;

{-----------------------------------------------------------------------}

PROCEDURE RunDem1;
BEGIN
   MassRatio := 0.2;
   Energy := 4;
   State[1] := 0.08;
   State[2] := 0;
   State[3] := 0;
   State[4] := EnergyFun(State[1], State[3], MassRatio) - Energy
                     - State[2]*State[2];
   State[4] := Sqrt(State[4]);
   ThreeDMotion;
END;

{-----------------------------------------------------------------------}

PROCEDURE RunDem2;
BEGIN
   MassRatio := 0.2;
   Energy := 4;
   State[1] := 0.03;
   State[2] := 0;
   State[3] := 0;
   State[4] := EnergyFun(State[1], State[3], MassRatio) - Energy
                     - State[2]*State[2];
   State[4] := Sqrt(State[4]);
   ThreeDMotion;
END;

{-----------------------------------------------------------------------}

PROCEDURE RunDem3;
BEGIN
   MassRatio := 0.2;
   Energy := 4;
   State[1] := 0;
   State[2] := 0.5;
   State[3] := 0;
   State[4] := EnergyFun(State[1], State[3], MassRatio) - Energy
                     - State[2]*State[2];
   State[4] := Sqrt(State[4]);
   ThreeDMotion;
END;

{-----------------------------------------------------------------------}

PROCEDURE RunDem4;
BEGIN
   MassRatio := 0.2;
   Energy := 3.7;
   State[1] := 0;
   State[2] := 0;
   State[3] := 0;
   State[4] := EnergyFun(State[1], State[3], MassRatio) - Energy
                     - State[2]*State[2];
   State[4] := Sqrt(State[4]);
   ThreeDMotion;
END;


{-----------------------------------------------------------------------}

PROCEDURE HandleThreeDHotKeys(Key: Byte; VAR Action: Boolean);
BEGIN
      CASE Key OF
      1: ShowHelp('HelpBins.HLP', 'Roche PoincDem');
      2: BEGIN Action := False; RunDem1; END;
      3: BEGIN Action := False; RunDem2; END;
      4: BEGIN Action := False; RunDem3; END;
      5: BEGIN Action := False; RunDem4; END;
      6: BEGIN
            Action := False;
            Demo := False;
            ThreeDHotKeys.Clear;
            OpenViewPort(7);
         END;
   END;
END;

{------------------------------------------------------------------------}

BEGIN {Main program for Procedure PoincareDemo.}
   DefineViewPort(7, 0, 1, 0.05, 0.95);
   SetUpThreeDHotKeys;
   RP1 := 0;
   RP2 := 0;
   SaveMassRatio := MassRatio;
   ShowHelp('HelpBins.HLP', 'Roche PoincDem');
   Demo := True;
   REPEAT
      CheckForEvents;
      IF ThreeDHotKeys.Pressed(Key) THEN
                          HandleThreeDHotKeys(Key, Action);
   UNTIL NOT Demo;
   MassRatio := SaveMassRatio;
   ShowCursor;
   ShowCUPSKeys := True;
   CUPSKeys.Display;
END;


{------------------------------------------------------------------------}

PROCEDURE HandleRocheMenu;

BEGIN
   With RocheMenu DO
   CASE ColChosen OF
      1: CASE RowChosen OF
            1: BEGIN
                  AboutCUPS;
                  LightMenu := True;
               END;
            2: BEGIN
                  ShowHelp('HelpBins.HLP', 'Roche Hello');
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
                  ShowHelp('HelpBins.HLP', 'Roche Info1');
                  ShowHelp('HelpBins.HLP', 'Roche Info2');
                  LightMenu := True;
               END;
            2: BEGIN
                  ShowHelp('HelpBins.HLP', 'Roche Info3');
                  LightMenu := True;
               END;
            3: BEGIN
                  ShowHelp('HelpBins.HLP', 'Roche Poincare');
                  LightMenu := True;
               END;
            4: BEGIN
                  RunPoincareDemo;
                  RocheMenu.RowActivate(4,1, False);
                  RocheMenu.RowActivate(4,2, False);
                  RocheMenu.RowActivate(4,3, False);
                  RocheMenu.RowActivate(4,4, False);
                  RocheMenu.RowActivate(5,1, False);
                  RocheMenu.RowActivate(5,2, False);
                  SelectScale(1);
                  LightMenu := True;
               END;
         END;
      3: BEGIN
            GetData;
         END;
      4: CASE RowChosen OF
            1: FindNewEnergy;
            2: ClickForEnergy;
            3: BEGIN
                  ZoomIn := True;
                  Zoom;
                  LightMenu := True;
               END;
            4: BEGIN
                  ZoomIn := False;
                  Zoom;
                  LightMenu := True;
               END;
         END;
      5: CASE RowChosen OF
            1: BEGIN
                  ChooseOrbit(Energy, MassRatio, x0, y0, xv0, yv0);
                  IF DataIn THEN
                            SetUpOrbitPlot(x0, y0, xv0, yv0, MassRatio)
                            ELSE LightMenu := True;
               END;
            2: BEGIN
                  RocheMenu.RowActivate(4,1, False);
                  RocheMenu.RowActivate(4,2, False);
                  RocheMenu.RowActivate(4,3, False);
                  RocheMenu.RowActivate(4,4, False);
                  RocheMenu.RowActivate(5,1, False);
                  RocheMenu.RowActivate(5,2, True);
                  MakePoincareMap;
               END;
         END;
      6: CASE RowChosen OF
            1: BEGIN
                  Poincare := False;
                  SetUpScreen;
                  RocheMenu.RowActivate(4,1, True);
                  RocheMenu.RowActivate(4,2, True);
                  RocheMenu.RowActivate(4,3, True);
                  RocheMenu.RowActivate(4,4, True);
                  RocheMenu.RowActivate(5,1, False);
                  RocheMenu.RowActivate(5,2, False);
                  LightMenu := True;
               END;
            2: BEGIN
                  Poincare := False;
                  SetUpScreen;
                  Contour(Energy);
                  EnergyChosen := True;
                  RocheMenu.RowActivate(4,1, True);
                  RocheMenu.RowActivate(4,2, True);
                  RocheMenu.RowActivate(4,3, True);
                  RocheMenu.RowActivate(4,4, True);
                  RocheMenu.RowActivate(5,1, True);
                  RocheMenu.RowActivate(5,2, True);
                  RocheMenu.RowActivate(6,1, True);
                  RocheMenu.RowActivate(6,2, True);
                  LightMenu := True;
               END;
         END;
    END;
END;

{------------------------------------------------------------------------}

PROCEDURE HandleCUPSKeys(Key: Byte);
BEGIN
   CASE Key OF
      1: ShowHelp('HelpBins.HLP', 'Roche Hello');
      2: IF RocheMenu.Chosen THEN HandleRocheMenu;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE SetUpDefaultInput;
VAR
   i: Integer;
BEGIN
   MassRatio := 0.5;
   RP1 := 0;
   RP2 := 0;
   fnX := 50;
   fnY := 50;
   Energy := 4;
   PxMin := -2;
   PxMax := 2;
   PyMin := -5;
   PyMax := 5;
   CxMin := -2;
   CxMax := 2;
   CyMin := -2;
   CyMax := 2;
   FOR i := 1 TO 6 DO
      Colors[i] := 16 - i;
   Colors[7] := 7;
   CurrentColor := 1;
   ZoomFactor := 2;
END;

{------------------------------------------------------------------------}

PROCEDURE GetStarted;
BEGIN
   ExitRequest := false;
   PlotOrbit := False;
   Action := False;
   EnergyChosen := False;
   ScreenReady := False;
   MassRatioChosen := False;
   Poincare := False;
   ShowCUPSKeys := True;
   SetUpDefaultInput;
   SetUpGraphics;
   Lightmenu := False;
   ShowCursor;
   SetUpCUPSKeys;
   CUPSKeys.Display;
   SetUpOrbitHotKeys;
   SetUpPoincareHotKeys;
   SetUpRocheMenu;
   ShowHelp('HelpBins.HLP', 'Roche Hello');
   RocheMenu.RowActivate(4,1, False);
   RocheMenu.RowActivate(4,2, False);
   RocheMenu.RowActivate(5,1, False);
   RocheMenu.RowActivate(5,2, False);
END;

{------------------------------------------------------------------------}

BEGIN   {Main Program.}
{  CUPSinit;}
   GetStarted;
   GetData;
   REPEAT
      CheckForEvents;
      IF ShowCUPSKeys THEN
              IF CUPSKeys.Pressed(Key) THEN HandleCUPSKeys(Key);
      IF (PlotOrbit) AND (NOT Poincare) THEN
      BEGIN
         IF OrbitHotKeys.Pressed(Key) THEN
               HandleOrbitHotKeys(key);
         IF Action THEN TakeOrbitStep;
      END;
      IF (Poincare) AND (PlotOrbit) THEN
      BEGIN
         IF PoincareHotKeys.Pressed(Key) THEN
                     HandlePoincareHotKeys(Key);
         TakePoincareStep;
      END;
      IF LightMenu THEN
      BEGIN
         LightMenu := False;
         IF RocheMenu.Chosen THEN HandleRocheMenu;
      END
      ELSE
         IF RocheMenu.Activated THEN HandleRocheMenu;
   UNTIL  ExitRequest;
   RocheMenu.done;
{  CUPSDone;}
END;

END.
