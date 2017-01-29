           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.7 (95/06/27) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)


UNIT S_WAVE;
{$N+}
                    {*********************************}
                    {*    Two-Nucleon Simulation     *}
                    {*          S_WAVE.PAS           *}
                    {*   Written by John Philpott    *}
                    {*        July XX, 1993.         *}
                    {*   Copyright (c) 1992, 1993    *}
                    {*********************************}
 
INTERFACE
 
USES
      crt, 
      graph, CUPS, CUPSmupp, CUPSfunc,
      CUPSproc, CUPSgui, CUPSgrph, Consts, Master;

{ ------------------ Global Variables Used Only by SWAVE ------------------ }

VAR
   MyPotl       : Integer;
   CoreRad      : Float;
   PotlDepth    : Float;
   PotlRange    : Float;
   SqWellLocalV : Float;
 
VAR
   SwaveMinEgy  : Float;
   SwaveMaxEgy  : Float;
   SwaveNumEgy  : Integer;

PROCEDURE InitializeSwaveConstants;
PROCEDURE GetStartingParams(MyPotl: Integer;
          VAR CoreRad, PotlRange, PotlDepth: Float);
FUNCTION Vcnt(MyPotl: Integer; r: Float): Float;

{ ---------------- Derivative Procedures Used Only by SWAVE --------------- }

PROCEDURE Derivs(x: Real; VAR yv, dydx: DVector);
PROCEDURE SqWellDerivs(x: Real; VAR yv, dydx: DVector);

PROCEDURE SWave;

PROCEDURE SWaveInit;

IMPLEMENTATION

{ ----------------------------- Physics Data ------------------------------ }

PROCEDURE InitializeSwaveConstants;
BEGIN
   SwaveMinEgy   :=   0.0;
   SwaveMaxEgy   := 200.0;
   SwaveNumEgy   :=  41;
END;

PROCEDURE GetStartingParams(MyPotl: Integer;
          VAR CoreRad, PotlRange, PotlDepth: Float);

   PROCEDURE SetParams(arg1, arg2, arg3: Float);
   BEGIN
      CoreRad := arg1;
      PotlRange := arg2;
      PotlDepth := arg3;
   END;

BEGIN
   CASE MyPotl OF
{                   Potential    Core          Potl         Potl     }
{                     Type     Radius (fm)   Range (fm)  Depth (MeV) }
      1: SetParams( {Square}      0.0    ,     2.10    ,    33.7     );
      2: SetParams( {Gauss }      0.0    ,     1.57    ,    65.9     );
      3: SetParams( {Exponl}      0.0    ,     0.70    ,   176.5     );
      4: SetParams( {Yukawa}      0.0    ,     1.60    ,    40.6     );
      5: SetParams( {Square}      0.2    ,     1.90    ,    48.0     );
      6: SetParams( {Gauss }      0.3    ,     1.25    ,   150.0     );
      7: SetParams( {Exponl}      0.35   ,     0.45    ,   824.0     );
      8: SetParams( {Yukawa}      0.4    ,     0.57    ,  1010.0     );
   END; { CASE }
END;
 
{ ------------------------------ Potentials ------------------------------- }

FUNCTION FormOfPotl: PotlForm;
BEGIN
   CASE ((MyPotl - 1) MOD 4) + 1 OF
      1: FormOfPotl := Square;
      2: FormOfPotl := Gauss;
      3: FormOfPotl := Exponential;
      4: FormOfPotl := Yukawa;
   END;
END;
 
FUNCTION PotlType(MyPotl: Integer): String;
VAR
   itype : Integer;
BEGIN
   itype := (MyPotl - 1) MOD 4 + 1;
   CASE itype OF
      1:  PotlType := 'Square';
      2:  PotlType := 'Gauss';
      3:  PotlType := 'Exponential';
      4:  PotlType := 'Yukawa';
   END; { CASE }
END;
 
FUNCTION Vcnt(MyPotl: Integer; r: Float): Float;
VAR
   index : Integer;
BEGIN
   Index := ((MyPotl - 1) MOD 4) + 1;
   CASE Index OF
1: BEGIN { Square }
      IF r > PotlRange      THEN Vcnt := 0
      ELSE IF r = PotlRange THEN Vcnt := -PotlDepth/2
      ELSE                       Vcnt := -PotlDepth;
   END;
2: Vcnt := -PotlDepth*exp(-sqr(r/PotlRange)); { Gauss }
3: Vcnt := -PotlDepth*exp(-r/PotlRange); { Exponential }
4: Vcnt := -PotlDepth*Yuk(r/PotlRange); { Yukawa }
   END; { CASE }
END;
 
{ ---------------- Derivative Procedures Used Only by SWAVE --------------- }

PROCEDURE Derivs(x: Real; VAR yv, dydx: DVector);
{ Uses GLOBAL VARIABLES: Energy, muftr }
BEGIN
   dydx.Put(1,yv.Value(2));
   dydx.Put(2,muftr*(Vcnt(MyPotl, x) - Energy)*yv.Value(1));
END;
 
PROCEDURE SqWellDerivs(x: Real; VAR yv, dydx: DVector);
{ Uses GLOBAL VARIABLES: Energy, muftr }
BEGIN
   dydx.Put(1,yv.Value(2));
   dydx.Put(2,muftr*(SqWellLocalV - Energy)*yv.Value(1));
END;

PROCEDURE SWave;

{ ------------------ Global Variables Used Only by SWAVE ------------------ }

VAR
   MinEgy   : Float;
   MaxEgy   : Float;
   NumEgy   : Integer;

   ScattLength : Float;
   ScattRange  : Float;

   Escape : Boolean;
   xv, yv : DVector;
   Nmatch : Integer;
   Nmax   : Integer;
 
PROCEDURE RestoreSwaveConstants;
BEGIN
   MinEgy := SwaveMinEgy;
   MaxEgy := SwaveMaxEgy;
   NumEgy := SwaveNumEgy;
END;

PROCEDURE SaveSwaveConstants;
BEGIN
   SwaveMinEgy := MinEgy;
   SwaveMaxEgy := MaxEgy;
   SwaveNumEgy := NumEgy;
END;

{ ---------------------- Wavefunction Calculations ------------------------ }

PROCEDURE SWaveStep(
                    Derivs: DFproc;                                   
                    VAR r, dr: Real; Var tempv: DVector);
{ Trap the call by SwaveFunction to CUPS procedure StepRK4
  in order to correctly treat a square well potential }
VAR
   rend, rmin, rmax : Float;
   drtemp : Real;
BEGIN
   IF ((MyPotl - 1) MOD 4) > 0 THEN StepRK4(Derivs, r, dr, tempv)
   ELSE
      BEGIN { Special procedure for square well }
         rend := r + dr;
         IF dr > 0 THEN BEGIN rmin := r;     rmax := rend; END
                   ELSE BEGIN rmin := rend;  rmax := r;    END;
         IF (rmin < PotlRange) AND (rmax > PotlRange) THEN
            BEGIN { Discontinuity falls within integration step }
               drtemp := PotlRange - r;
               SqWellLocalV := Vcnt(MyPotl, r + 0.5*drtemp);
               StepRK4(SqWellDerivs, r, drtemp, tempv);
               drtemp := rend - PotlRange;
               SqWellLocalV := Vcnt(MyPotl, rend - 0.5*drtemp);
               StepRK4(SqWellDerivs, r, drtemp, tempv);
            END
         ELSE
            BEGIN { Discontinuity not within integration step }
               SqWellLocalV := Vcnt(MyPotl, r + 0.5*dr);
               StepRK4(SqWellDerivs, r, dr, tempv);
            END;
      END;  { Special procedure for square well }
END;
 
PROCEDURE SwaveFunction(E: Float; ibeg, iend, imatch: Integer;
                        xv: DVector; VAR yv: DVector; VAR dydx: Float);
{ Resets GLOBAL VARIABLE: Energy }
VAR
   i1, i2, di, i, ii     : Integer;
   tempv : DVector;
   r, dr : Real;
   expftr, kappa : Float;
BEGIN
   Energy := E;   { Needed in Derivs     }
   Request1DVector(tempv, 2); { Request temp DVector }
   IF ibeg < iend THEN { Prepare for outward integration }
      BEGIN
         i1 := ibeg;  i2 := iend;  di :=  1;
         { Initialize radial wavefunction to zero value
           and unit slope at origin or core radius }
         tempv.Put(1, 0.0);
         tempv.Put(2, 1.0);
      END
   ELSE IF ibeg > iend THEN { Prepare for inward integration }
      BEGIN
         i1 := iend;  i2 := ibeg;  di := -1;
         { Initialize to exp function at the outer radius.  }
         { ASSUMES E is NEGATIVE (bound state wavefunction) }
         kappa := sqrt(-muftr*E);
         expftr := exp(-kappa*xv.Value(ibeg));
         tempv.Put(1, expftr);
         tempv.Put(2, -kappa*expftr);
      END;
   r := xv.Value(ibeg);
   dr := xv.Value(ibeg + di) - r;
   i := ibeg;
   yv.Put(i,tempv.Value(1));
   FOR ii := i1 + 1 TO i2 DO
      BEGIN
         SWaveStep(Derivs, r, dr, tempv);
         i := i + di;
         yv.Put(i,tempv.Value(1));
         IF i = imatch THEN dydx := tempv.Value(2);
      END;
   Release1DVector(tempv);    { Release temp DVector }
END;
 
PROCEDURE ZeroEgyNorm(Nmax: Integer;
                      xv: DVector; VAR yv: DVector;
                      VAR ScattLength, ScattRange: Float;
                      VAR NumBoundStates: Integer);
VAR
   slope : Float;
   norm  : Float;
   i     : Integer;
BEGIN
   { Calculate the wavefunction }
   SWaveFunction(0.0, 1, Nmax, Nmax, xv, yv, slope);
   norm := yv.Value(Nmax) - xv.Value(Nmax)*slope;
   IF norm <> 0 THEN
      BEGIN
         norm := 1/norm;
         FOR i := 1 TO Nmax DO yv.Put(i,norm*yv.Value(i));
         slope := norm*slope;
      END;
   { Determine scattering length, effective range and number of nodes }
   ScattLength := -1/slope;
   ScattRange := 2.0*(LineInt(slope, 1.0, xv.Value(Nmax))
                      - LineInt(slope, 1.0, 0.0)
                      - NormInt(Rstep, 1, Nmax, yv));
   NumBoundStates := Nodes(yv, 1, Nmax);
   IF (yv.Value(Nmax-1)*yv.Value(Nmax) >= 0) AND
      (ScattLength > Rmax - Rstep/2) THEN Inc(NumBoundStates);
END;
 
PROCEDURE Normalize(BE, dr: Float; Nmax: Integer; VAR yv: DVector);
VAR
   norm : Float;
   i    : Integer;
BEGIN
   norm := NormInt(dr, 1, Nmax, yv)
           + SWaveTailInt(BE, Nmax, yv);
   norm := 1/Sqrt(norm);
   FOR i := 1 TO Nmax DO yv.Put(i,norm*yv.Value(i));
END;
 
PROCEDURE MakeApproxBoundWaveFnctn{ For s-wave deuteron }
          (BE: Float; Nmax, Nmatch: Integer;
           xv: DVector; VAR yv: DVector;
           VAR DerivLeft, DerivRight: Float);
VAR
   i                 : Integer;
   dxtik, dytik      : Float;
   dydx, ftr, norm   : Float;
   IntLeft, IntRight : Float;
BEGIN
{ Calculate left branch of wavefunction }
   SWaveFunction(-BE, 1, Nmatch, Nmatch, xv, yv, dydx);
   ftr := yv.Value(Nmatch);
   DerivLeft := dydx;
   IntLeft := NormInt(Rstep, 1, Nmatch, yv);
{ Calculate right branch of wavefunction }
   SWaveFunction(-BE, Nmax, Nmatch, Nmatch, xv, yv, dydx);
   ftr := ftr/yv.Value(Nmatch);
   DerivRight := dydx;
   IntRight := NormInt(Rstep, Nmatch, Nmax, yv)
               + SWaveTailInt(BE, Nmax, yv);
{ Normalize the wavefunction }
   norm := 1/Sqrt(IntLeft + Sqr(ftr)*IntRight);
   FOR i := 1 TO Nmatch - 1 DO yv.Put(i,norm*yv.Value(i));
   DerivLeft := norm*DerivLeft;
   norm := ftr*norm;
   FOR i := Nmatch TO Nmax DO yv.Put(i,norm*yv.Value(i));
   DerivRight := norm*DerivRight;
END;
 
PROCEDURE AnalyzeApproxBoundWaveFnctn(xv, yv: DVector; Nmax, Nmatch: Integer;
                           DerivLeft, DerivRight: Float;
                           VAR DeltaBE: Float; VAR NodeCount: Integer);
{ Analyze an approximate bound state wavefunction to determine the
  number of nodes and the estimate for the needed shift in binding
  energy.   The input wavefunction should be normalized such that
  its squared integral is unity. }
{ Uses GLOBAL VARIABLE muftr }
BEGIN
{ Calculate the estimated change needed in BE }
   DeltaBE := yv.Value(Nmatch)*(DerivRight - DerivLeft)/muftr;
{ Find number of nodes }
   NodeCount := Nodes(yv, 1, Nmax);
   IF DeltaBE > 0 THEN Inc(NodeCount);
END;

{ ----------------------- Phase Shift Calculations ------------------------ }

FUNCTION EffRangeDelta(ScattLength, ScattRange, Egy: Float): Float;
{ Uses GLOBAL VARIABLE muftr }
VAR
   kout, delta : Float;
BEGIN
{ Calculate the phase shift from the effective range expansion }
   kout := sqrt(muftr*Egy);
   delta := Arctan2(0.5*ScattRange*sqr(kout) - 1/ScattLength, kout);
   EffRangeDelta := (180/pi)*delta;
END;
 
FUNCTION PhaseShift(y, dydx, Rmatch, Egy: Float): Float;
{ Uses GLOBAL VARIABLE muftr }
VAR
   kout, delta : Float;
BEGIN
{ Calculate the phase shift from the boundary condition }
   kout := sqrt(muftr*Egy);
   delta := Arctan2(dydx, kout*y) - kout*Rmatch;
   delta := delta/pi;
   delta := delta - trunc(delta);
   IF delta < 0 THEN delta := delta + 1;
   PhaseShift := 180*delta;
END;
 
{ -------------------------- Interface Routines --------------------------- }

PROCEDURE ShowResultsScreen(VAR ScattLength, ScattRange: Float;
                            VAR NumBoundStates: Integer);
BEGIN
   DefineViewPort(10, 0.0, 0.35, 0.13, 0.93);
   OpenViewPort(10);
   Print(1,02,'   POTENTIAL PARAMETERS:');
   Print(1,04,Concat('  Potl Type:  ', PotlType(MyPotl)));
   Print(1,05,Concat('  Core Radius: ', NumStr(CoreRad,5,2),   ' fm'));
   Print(1,06,Concat('  Potl Range:  ', NumStr(PotlRange,5,2), ' fm'));
   Print(1,07,Concat('  Potl Depth:  ', NumStr(PotlDepth,5,2), ' MeV'));
   SetColor(Yellow);
   Print(1,09,'   CALCULATED RESULTS:');
   Print(1,11,Concat('  Scatt Length:', NumStr(ScattLength,7,2), ' fm'));
   Print(1,12,Concat('  Eff. Range:  ', NumStr(ScattRange,7,2),  ' fm'));
   Print(1,14,Concat('     # bound states ', NumStr(NumBoundStates,2,0)));
   SetColor(Whitte);
END;

CONST
   potlmin = -80;
   potlmax = +20;

PROCEDURE PlotPotlFunction(xv: DVector; Nmax: Integer);
VAR
   index, i : Integer;
   yv       : DVector;
BEGIN
   SelectViewPort(1);
   SelectScale(1);
   SetColor(LightGreen);
   Print(9,1,'Potential versus Radius');
   Index := ((MyPotl - 1) MOD 4) + 1;
   IF index = 1 THEN { Square potential plotted between corners }
      BEGIN
         PlotLine(CoreRad,-PotlDepth,PotlRange,-PotlDepth);
         PlotLine(PotlRange,-PotlDepth,PotlRange,0);
         PlotLine(PotlRange,0,RmaxInit,0);
         IF CoreRad > 0 THEN PlotLine(CoreRad,-PotlDepth,CoreRad,potlmax);
      END
   ELSE
      BEGIN          { Other shapes plotted between grid points }
         Request1DVector(yv, Nmax);
         FOR i := 1 TO Nmax DO
            BEGIN
               yv.Put(i,Vcnt(MyPotl,xv.Value(i)));
            END;
         IF CoreRad > 0 THEN PlotLine(CoreRad,yv.Value(1),CoreRad,potlmax);
         PlotDvtr(xv, yv, 1, Nmax);
         Release1DVector(yv);
      END;
   SetColor(whitte);
END;
 
PROCEDURE ZeroEnergy;
VAR
   ScattLength, ScattRange : Float;
   ymin, ymax, BE, dydx    : Float;
   NumBoundStates          : Integer;
BEGIN { ZeroEnergy }
   ClearMUPPETport;
   Request2DVectors(xv, yv, Npoints);
   SetGrid(CoreRad, RmaxInit, Rstep, xv, Nmatch, Nmax);
   ZeroEgyNorm(Nmax, xv, yv, ScattLength, ScattRange, NumBoundStates);
   ShowResultsScreen(ScattLength,ScattRange,NumBoundStates);
   PreparePotlPlot(potlmin, potlmax);
   PlotPotlFunction(xv, Nmax);
   ymax := 1.2*MaxF(1.0,ABS(yv.Value(Nmax)));
   ymin := -ymax;
   PrepareWaveFuncPlot(ymin, ymax);
   SetColor(Yellow);
   PlotDvtr(xv, yv, 1, Nmax);
   Print(9,1,'Zero Energy Wavefunction');
   SetColor(whitte);
   PlotLine(0.0,1.0,xv.Value(Nmax),yv.Value(Nmax));
   Release2DVectors(xv, yv);
   SelectViewPort(10);
   IF NumBoundStates = 1 THEN
      BEGIN
         EstimateBE(ScattLength, ScattRange, CoreRad, BE);
         IF BE > 0 THEN
            BEGIN
               Print(3,16,'Effective range theory');
               Print(3,17,'predicts a bound state');
               Print(3,18,Concat('near BE =', NumStr(BE,9,3), ' MeV'));
            END
         ELSE
            BEGIN
               Print(3,16,'No useful estimate for');
               Print(3,17,'the bound state energy');
               Print(3,18,'found.');
            END;
      END;
END;  { ZeroEnergy }
 
FUNCTION GetGoodParams(VAR CoreRad, PotlRange, PotlDepth: Float) : boolean;
VAR
   Escape, GoodParams : Boolean;
   Core, Range, Depth : Float;
   TheScreen : TInputScreen;
BEGIN
   OpenScrn(TheScreen);
   with TheScreen do
   BEGIN
      init;
      DefineInputport(0.18, 0.82, 0.18, 0.82); 
      LoadLine('    View and Adjust the potential parameters ');
      LoadLine('');
      LoadLine('');
      LoadLine('    Hard core radius             {    0} fm ');
      LoadLine('');
      LoadLine('    Attractive potential range   {    0} fm ');
      LoadLine('');
      LoadLine('    Attractive potential depth   {    0} MeV   ');
      LoadLine('');
      LoadLine('    NOTE:  Any changes made here will be lost  ');
      LoadLine('           if you re-select an interaction.    ');
      LoadLine('           Use Help for info on valid entries. ');
      LoadLine('');
      LoadLine('     [  Ok  ]      [Cancel]      [ Help ]      ');
      SetNumber(1,CoreRad);
      SetNumber(2,PotlRange);
      SetNumber(3,PotlDepth);
      SetNumberLimits(1, 0, 2.0);
      SetNumberLimits(2, 0.1, 5);
      SetNumberLimits(3, 0, 5000);
      SetHelpFile('twobody.hlp','SWavePotlParams');
      GoodParams := FALSE;
      REPEAT
         AcceptScreen;
         Core  := GetNumber(1);
         Range := GetNumber(2);
         Depth := GetNumber(3);
         IF (FormOfPotl = Square) AND (Range < Core) THEN
            announce('Core radius exceeds potential range.  Try again...')
         ELSE
            BEGIN
               CoreRad :=   Core;
               PotlRange := Range;
               PotlDepth := Depth;
               GoodParams := TRUE;
            END;
      UNTIL Canceled OR GoodParams;
      GetGoodParams := Not Canceled;
      done;
   END;
   CloseScrn(TheScreen);
END;

PROCEDURE DisplayApproxBoundWaveFnctn{ For s-wave deuteron }
          (BE: Float; Nmax, Nmatch: Integer;
           xv: DVector; VAR yv: DVector;
           VAR DerivLeft, DerivRight: Float);
CONST
   overlap = 1.0; { fm }
VAR
   imin, imax, iend, i : Integer;
   ymin, ymax          : Real;
   dydx, ynorm, norm   : Float;
   ysavev              : DVector;
BEGIN
{ Calculate both branches up to the matching radius }
   MakeApproxBoundWaveFnctn(BE, Nmax, Nmatch, xv, yv,
                            DerivLeft, DerivRight);
   yv.MinMax(imin, ymin, imax, ymax);
   ymin := 1.1*ymin;
   ymax := 1.1*ymax;
   PrepareWaveFuncPlot(ymin, ymax);
{ Save normalized wavefunction value from previous calculation }
   ynorm := yv.Value(Nmatch);
{ Calculate and plot left branch of wavefunction }
   iend := Nmatch + round(overlap/Rstep);
   IF iend > Nmax THEN iend := Nmax;
   SWaveFunction(-BE, 1, iend, Nmatch, xv, yv, dydx);
   norm := ynorm/yv.Value(Nmatch);
   FOR i := 1 TO iend DO yv.Put(i,norm*yv.Value(i));
   SetColor(Yellow);
   PlotDvtr(xv, yv, 1, iend);
   SetColor(Whitte);
{ Calculate and plot right branch of wavefunction }
   iend := Nmatch - round(overlap/Rstep);
   IF iend < 1 THEN iend := 1;
{ Save original left branch in overlap region }
   Request1DVector(ysavev, Nmatch);
   FOR i := iend TO Nmatch DO ysavev.Put(i, yv.Value(i));
   SWaveFunction(-BE, Nmax, iend, Nmatch, xv, yv, dydx);
   norm := ynorm/yv.Value(Nmatch);
   FOR i := Nmax DOWNTO iend DO yv.Put(i,norm*yv.Value(i));
   SetColor(Yellow);
   PlotDvtr(xv, yv, Nmax, iend);
   SetColor(Whitte);
{ Restore original left branch in overlap region }
   FOR i := iend TO Nmatch DO yv.Put(i, ysavev.Value(i));
   Release1DVector(ysavev);
END;
 
PROCEDURE PlotWaveFunction(BE, Rstep: Float; Nmax: Integer;
                           xv, yv: DVector);
VAR
   ymin, ymax : Real;
   imin, imax : Integer;
BEGIN
   Normalize(BE, Rstep, Nmax, yv);
   yv.MinMax(imin, ymin, imax, ymax);
   ymin := 1.1*ymin;
   ymax := 1.1*ymax;
   PrepareWaveFuncPlot(ymin, ymax);
   SetColor(Yellow);
   PlotDvtr(xv, yv, 1, Nmax);
   SetColor(Whitte);
END;

TYPE
   ComparisonPhase = (Singlet, Triplet);

PROCEDURE PreparePhasePlot(MyComparison: ComparisonPhase; MinEgy, MaxEgy: Float);
VAR
   dxtik, dytik : Float;
BEGIN
   DefineViewport(2, 0.45, 0.95, 0.13, 0.52);
   OpenViewPort(2);
   DefineScale(2, MinEgy,MaxEgy, 0.0,180.0);
   SelectScale(2);
   dxtik := TickSpace(MaxEgy - MinEgy);
   dytik := TickSpace(180.0);
   Axis(0.0, 0.0, dxtik, dytik);
   PutLabel(bottom,'cm Energy (MeV)');
   PutLabel(left,'Phase Shift (deg)');
   CASE MyComparison OF
      Singlet:  Print(9,1,'1S0 empirical phase shift');
      Triplet:  Print(9,1,'3S1 empirical phase shift');
   END;
   SetColor(Yellow);
   Print(9,2,'Calculated phase shift');
   SetColor(LightRed);
   Print(9,3,'Effective Range Theory');
   SetColor(whitte);
END;
 
PROCEDURE PlotEmpiricalPhases(MyComparison: ComparisonPhase;
                              MinEgy, MaxEgy: Float; NumEgy: Integer);
CONST
   breakvalue = 90; { degrees }
   eps        = 1E-6;
VAR
   Phases          : PhaseType;
   PhasesFound     : Boolean;
   DEgy, dydx, ftr : Float;
   Egy, OldEgy     : Float;
   Delta, OldDelta : Float;
   StartDelta      : Float;
   i, jphase       : Integer;
BEGIN
   ftr := 180/pi;
   CASE MyComparison OF
      Singlet: BEGIN  jphase := 4;  StartDelta :=   0;  END;
      Triplet: BEGIN  jphase := 1;  StartDelta := 180;  END;
   END;
   SelectViewPort(2);
   SelectScale(2);
   DEgy := (MaxEgy - MinEgy)/(NumEgy - 1);
   IF MinEgy < 0.5*MinimumTabulatedLabKE THEN
      MinEgy := 0.5*MinimumTabulatedLabKE;
   IF MaxEgy > 0.5*MaximumTabulatedLabKE THEN
      MaxEgy := 0.5*MaximumTabulatedLabKE;
   NumEgy := Round((MaxEgy - MinEgy)/DEgy) + 1;
   Egy := MinEgy;
   OldEgy := Egy;
   IF Egy < eps THEN OldDelta := StartDelta
   ELSE
      BEGIN
         GetPhases(2*Egy, PhasesFound, Phases);
         OldDelta := ftr*Phases[1,jphase];
         IF OldDelta < 0 THEN OldDelta := OldDelta + 180;
      END;
   FOR i := 2 TO NumEgy DO
      BEGIN
         Egy := Egy + DEgy;
         GetPhases(2*Egy, PhasesFound, Phases);
         Delta := ftr*Phases[1,jphase];
         IF Delta < 0 THEN Delta := Delta + 180;
         IF ABS(Delta - OldDelta) < breakvalue THEN
            BEGIN
               Plotline(OldEgy, OldDelta, Egy, Delta);
            END;
         OldEgy := Egy;
         OldDelta := Delta;
      END;
   SetColor(whitte);
END;
 
PROCEDURE PlotEffRangePhases(MinEgy, MaxEgy: Float; NumEgy: Integer);
CONST
   breakvalue = 90; { degrees }
   eps        = 1E-6;
VAR
   DEgy, dydx      : Float;
   Egy, OldEgy     : Float;
   Delta, OldDelta : Float;
   i               : Integer;
BEGIN
   SelectViewPort(2);
   SelectScale(2);
   SetColor(LightRed);
   DEgy := (MaxEgy - MinEgy)/(NumEgy - 1);
   Egy := MinEgy;
   OldEgy := Egy;
   IF Egy < eps THEN OldDelta := 180
   ELSE OldDelta := EffRangeDelta(ScattLength, ScattRange, Egy);
   FOR i := 2 TO NumEgy DO
      BEGIN
         Egy := Egy + DEgy;
         Delta := EffRangeDelta(ScattLength, ScattRange, Egy);
         IF ABS(Delta - OldDelta) < breakvalue THEN
            BEGIN
               Plotline(OldEgy, OldDelta, Egy, Delta);
            END;
         OldEgy := Egy;
         OldDelta := Delta;
      END;
   SetColor(whitte);
END;
 
PROCEDURE PlotPhases(MinEgy, MaxEgy: Float; NumEgy: Integer);
CONST
   breakvalue = 90; { degrees }
   eps        = 1E-6;
VAR
   DEgy, dydx      : Float;
   Egy, OldEgy     : Float;
   Delta, OldDelta : Float;
   i               : Integer;
BEGIN
   SelectViewPort(2);
   SelectScale(2);
   SetColor(Yellow);
   DEgy := (MaxEgy - MinEgy)/(NumEgy - 1);
   Egy := MinEgy;
   OldEgy := Egy;
   IF Egy < eps THEN OldDelta := 180
   ELSE
      BEGIN
         SWaveFunction(Egy, 1, Nmax, Nmax, xv, yv, dydx);
         OldDelta := PhaseShift(yv.Value(Nmax), dydx, xv.Value(Nmax), Egy);
      END;
   i := 1;
   REPEAT
      i := i + 1;
      Egy := Egy + DEgy;
      SWaveFunction(Egy, 1, Nmax, Nmax, xv, yv, dydx);
      Delta := PhaseShift(yv.Value(Nmax), dydx, xv.Value(Nmax), Egy);
      IF ABS(Delta - OldDelta) < breakvalue THEN
         BEGIN
            Plotline(OldEgy, OldDelta, Egy, Delta);
         END;
      OldEgy := Egy;
      OldDelta := Delta;
   UNTIL (i = NumEgy) OR break;
   SetColor(whitte);
END;
 
{ ------------------------- Program Organization -------------------------- }

PROCEDURE ChooseInteraction;
{ Purpose:  Select one of several pre-defined potential shapes }
{ Resets and Restores GLOBAL VARIABLE: muftr }
VAR
   ViewScreen : TInputScreen;
   m : integer;
BEGIN { ChooseInteraction (SWave) }
   OpenScrn(ViewScreen);
   WITH ViewScreen DO
   BEGIN
      init;
      DefineInputport(0.19, 0.81, 0.3, 0.7);
      LoadLine('Select potential shape:');
      LoadLine('');
      LoadLine('  #1 Square       #1 Square with hard core');
      LoadLine('  #1 Gauss        #1 Gauss  with hard core ');
      LoadLine('  #1 Exponential  #1 Exponential with hard core');
      LoadLine('  #1 Yukawa       #1 Yukawa with hard core');
      LoadLine('  ');
      LoadLine('');
      LoadLine('        [  Ok  ]  [Cancel]       ');
      m := MyPotl;
      m := 2*((m-1) mod 4) +1 +(m-1) div 4;
      SetRadioButton('1',m);
      AcceptScreen;
      IF NOT Canceled THEN
         BEGIN
            m := GetRadioButton('1')+1;
            MyPotl := m div 2 +4*(m mod 2);
            GetStartingParams(MyPotl, CoreRad, PotlRange, PotlDepth);
            SetMuftr(2);        { Temporarily set to np system }
            ZeroEnergy;
            SetMuftr(MySystem); { Restore to original system   }
         END;
      done;
   END;
   CloseScrn(ViewScreen);
END;  { ChooseInteraction (SWave) }
 
PROCEDURE AdjustInteraction;
{ Resets and Restores GLOBAL VARIABLE: muftr }
BEGIN
   IF GetGoodParams(CoreRad, PotlRange, PotlDepth) THEN
      BEGIN
         SetMuftr(2);        { Temporarily set to np system }
         ZeroEnergy;
         SetMuftr(MySystem); { Restore to original system   }
      END;
END;

PROCEDURE AutoBound;
{ Resets and Restores GLOBAL VARIABLE: muftr }
VAR
   HighBE, LowBE : Float;
 
   FUNCTION InRange(BE: Float): Boolean;
   BEGIN
      InRange := (HighBE < BE) AND (BE < LowBE);
   END;
 
VAR
   xv, yv         : DVector;
   Nmatch, Nmax   : Integer;
   BE, DeltaBE    : Float;
   OldDeltaBE     : Float;
   ymin, ymax     : Real;
   NumBoundStates : Integer;
   trialnumber    : Integer;
   DerivLeft, DerivRight : Float;
   imin, imax, NodeCount : Integer;
   arbLongInt     : LongInt;
 
BEGIN { AutoBound (SWave) }
   ClearMUPPETport;
   SetMuftr(2);        { Temporarily set to np system }
   Request2DVectors(xv, yv, Npoints);
   SetGrid(CoreRad, RmaxInit, Rstep, xv, Nmatch, Nmax);
   ZeroEgyNorm(Nmax, xv, yv, ScattLength, ScattRange, NumBoundStates);
   IF NumBoundStates = 1 THEN
      BEGIN
         ShowResultsScreen(ScattLength,ScattRange,NumBoundStates);
         PreparePotlPlot(potlmin, potlmax);
         PlotPotlFunction(xv, Nmax);
         EstimateBE(ScattLength, ScattRange, CoreRad, BE);
         HighBE := 0;
       { Find a lower bound for the bound state energy }
         LowBE := 4; { MeV }
         REPEAT
            MakeApproxBoundWaveFnctn(LowBE, Nmax, Nmatch, xv, yv,
                                     DerivLeft, DerivRight);
            AnalyzeApproxBoundWaveFnctn(xv, yv, Nmax, Nmatch,
                         DerivLeft, DerivRight, DeltaBE, NodeCount);
            LowBE := 2*LowBE;
         UNTIL (NodeCount = 0) OR (LowBE > 1000 { MeV });
         IF NodeCount = 0 THEN
            BEGIN
               trialnumber := 0;
               DeltaBE := 0;
               REPEAT
                  OldDeltaBE := DeltaBE;
                  IF NOT InRange(BE) THEN BE := 0.5*(LowBE + HighBE);
                  MakeApproxBoundWaveFnctn(BE, Nmax, Nmatch, xv, yv,
                                           DerivLeft, DerivRight);
                  AnalyzeApproxBoundWaveFnctn(xv, yv, Nmax, Nmatch,
                             DerivLeft, DerivRight, DeltaBE, NodeCount);
                  PlotWaveFunction(BE, Rstep, Nmax, xv, yv);
                  Inc(trialnumber);
                  Print(9,1,Concat('Trial Wavefunction #',
                     NumStr(trialnumber,2,0)));
                  Delay(50);            
                  IF NodeCount = 0 THEN LowBE := BE
                                   ELSE HighBE := BE;
                  BE := BE + DeltaBE;
               UNTIL ((LowBE - HighBE) < 1e-5*(LowBE + HighBE)) OR
                     ((ABS(DeltaBE) < ABS(OldDeltaBE)) AND
                     (ABS(DeltaBE) < 1e-5*BE));
             { Normalize and plot the bound state wavefunction }
               PlotWaveFunction(BE, Rstep, Nmax, xv, yv);
               NodeCount := Nodes(yv, 1, Nmax);
               Print(9,1,'Bound state Wavefunction');
               Print(11,2,Concat('at BE = ', NumStr(BE,8,3), ' MeV'));
               SelectViewPort(10);
               SetColor(Yellow);
               Print(3,16,Concat('   # Nodes = ', NumStr(NodeCount,4,0)));
               Print(3,17,Concat('rms radius = ',
                  NumStr(sqrt(SWaveRsqME(BE, Nmax, xv, yv)),7,2), ' fm'));
               SelectViewPort(2);
               SetColor(Whitte);
            END
         ELSE
            BEGIN
               { Don't expect to get here for any reasonable potential }
               Announce(
      'Potential too deep.  Press <Enter> or click mouse to continue.');
            END;
      END
   ELSE
      BEGIN
         ZeroEnergy;
         IF NumBoundStates = 0 THEN Announce('There is no bound state.')
         ELSE Announce('More than one bound state - Use Manual Search');
      END;
   Release2DVectors(xv, yv);
   SetMuftr(MySystem); { Restore to original system   }
END;  { AutoBound (SWave) }
 
PROCEDURE ManualBound;
{ Resets and Restores GLOBAL VARIABLE: muftr }
VAR
   Escape : Boolean;
   xv, yv : DVector;
   Nmatch : Integer;
   Nmax   : Integer;
   ymin   : Float;
   ymax   : Float;
   dydx   : Float;
   BE     : Float;
   DerivLeft, DerivRight : Float;
   DeltaBE, OldDeltaBE: Float;
   FoundIt, LostIt : Boolean;
   NumBoundStates, NodeCount : Integer;
   OldNodeCount : Integer;
 
BEGIN { ManualBound (SWave) }
   ClearMUPPETport;
   OldNodeCount := 0;
   SetMuftr(2);        { Temporarily set to np system }
   Request2DVectors(xv, yv, Npoints);
   SetGrid(CoreRad, RmaxInit, Rstep, xv, Nmatch, Nmax);
   ZeroEgyNorm(Nmax, xv, yv, ScattLength, ScattRange, NumBoundStates);
   BE := 0;
   IF NumBoundStates = 0 THEN
      BEGIN
         ZeroEnergy;
         Announce(
         'No bound state.  Press <Enter> or click mouse to continue...');
      END
   ELSE
      BEGIN { NumBoundStates > 0 }
         ShowResultsScreen(ScattLength,ScattRange,NumBoundStates);
         PreparePotlPlot(potlmin, potlmax);
         PlotPotlFunction(xv, Nmax);
         IF NumBoundStates = 1 THEN
            EstimateBE(ScattLength, ScattRange, CoreRad, BE);
         IF GetGoodEnergy(BE) THEN
            BEGIN { a starting energy was entered }
IF BE > 0 THEN
   BEGIN { Manual search for bound state }
      IF BE < 1e-6 THEN
         BEGIN
            Announce('Binding energy too small - Increased to 10^-6 MeV.');
            BE := 1e-6;
         END;
      OldDeltaBE := Large;
      FoundIt := FALSE;
      LostIt := FALSE;
      yv.Put(Nmatch,0.5);
      REPEAT
         DisplayApproxBoundWaveFnctn(BE, Nmax, Nmatch, xv, yv,
                                     DerivLeft, DerivRight);
         AnalyzeApproxBoundWaveFnctn(xv, yv, Nmax, Nmatch,
                      DerivLeft, DerivRight, DeltaBE, NodeCount);
         IF ABS(DeltaBE) < 1E-3*BE THEN
            BEGIN { Display info: Prepare to quit search }
               FoundIt := TRUE;
               NodeCount := Nodes(yv, 1, Nmax);
               Print(9,1,'Bound state Wavefunction');
               Print(11,2,Concat('at BE = ', NumStr(BE,8,3), ' MeV'));
               SelectViewPort(10);
               SetColor(GraphBackColor);
               Print(3,16,Concat('   # Nodes = ', NumStr(OldNodeCount,4,0)));
               SetColor(Yellow);
               Print(3,16,Concat('   # Nodes = ', NumStr(NodeCount,4,0)));
               Print(3,17,Concat('rms radius = ',
                  NumStr(sqrt(SWaveRsqME(BE, Nmax, xv, yv)),7,2), ' fm'));
               SelectViewPort(2);
               SetColor(Whitte);
            END   { Display info: Prepare to quit search }
         ELSE
            BEGIN { Display info: Request instructions }
               Print(9,1,'Approximate Bound state');
               Print(9,2,Concat('near BE = ', NumStr(BE,8,3), ' MeV'));
               BE := BE + DeltaBE;
               SetColor(LightRed);
               Print(7,3,Concat('(Next guess is ', NumStr(BE,8,3), ' MeV)'));
               SelectViewPort(10);
               SetColor(GraphBackColor);
               Print(3,16,Concat('   # Nodes = ', NumStr(OldNodeCount,4,0)));
               OldNodeCount := NodeCount;
               SetColor(Yellow);
               Print(3,16,Concat('   # Nodes = ', NumStr(NodeCount,4,0)));
               SelectViewPort(2);
               SetColor(Whitte);
               IF (ABS(DeltaBE) < 5*OldDeltaBE) AND (BE > 0) THEN
                  BEGIN
                     OldDeltaBE := ABS(DeltaBE);
                     Message(
   'Press <Enter> or click mouse to continue search for bound state.');
                  END
               ELSE
                  BEGIN
                     LostIt := TRUE;
                     Announce('Bound state search not converging.');
                     Message(
   'Guess a new BE - or press <Enter> or click mouse to quit search.');
                  END;
                  WaitForInstructions(BE, OldDeltaBE, LostIt);
            END;  { Display info: Request instructions }
      UNTIL FoundIt OR LostIt;
   END   { Manual search for bound state }
ELSE
   BEGIN
      ClearMUPPETport;
      Announce(
      'Guessed binding energy is zero.   Manual search aborted!');
   END;
            END   { a starting energy was entered }
            ELSE Announce('No starting energy...  Manual search aborted!');
      END;  { NumBoundStates > 0 }

   Release2DVectors(xv, yv);
   SetMuftr(MySystem); { Restore to original system   }
END;  { ManualBound (SWave) }


PROCEDURE Phases(MyComparison: ComparisonPhase);
{ Resets and Restores GLOBAL VARIABLE: muftr }
VAR
   Escape : Boolean;
   dydx   : Float;
   NumBoundStates : Integer;
BEGIN { Phases (SWave) }
   IF GetEgyRange(MinEgy, MaxEgy, NumEgy) THEN
      BEGIN
         ClearMUPPETport;
         SetMuftr(2);        { Temporarily set to np system }
         Request2DVectors(xv, yv, Npoints);
         SetGrid(CoreRad, RmaxInit, Rstep, xv, Nmatch, Nmax);
         ZeroEgyNorm(Nmax, xv, yv, ScattLength, ScattRange, NumBoundStates);
         ShowResultsScreen(ScattLength,ScattRange,NumBoundStates);
         PreparePotlPlot(potlmin, potlmax);
         PlotPotlFunction(xv, Nmax);
         PreparePhasePlot(MyComparison, MinEgy, MaxEgy);
         PlotEmpiricalPhases(MyComparison, MinEgy, MaxEgy, NumEgy);
         PlotEffRangePhases(MinEgy, MaxEgy, 100);
         PlotPhases(MinEgy, MaxEgy, NumEgy);
         Release2DVectors(xv, yv);
         SetMuftr(MySystem); { Restore to original system   }
      END;
END;  { Phases (SWave) }
 

BEGIN
   RestoreSwaveConstants;
   WITH MasterMenu DO
      CASE RowChosen OF
    1: ShowHelp('twobody.hlp','AboutSWave');
    2: ChooseInteraction;
    3: AdjustInteraction;
    4: AutoBound;
    5: ManualBound;
    6: Phases(Triplet);
    7: Phases(Singlet);
      END; {CASE}
   SaveSwaveConstants;
END;

PROCEDURE SwaveInit;
BEGIN

 { Set up initial potential parameters for S_WAVE calculations }

   InitializeSwaveConstants;

   MyPotl    := 1;
   GetStartingParams(MyPotl, CoreRad, PotlRange, PotlDepth);

END;

END.  { Unit S_WAVE.PAS }
