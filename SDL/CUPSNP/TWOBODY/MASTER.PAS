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


UNIT Master;
{$N+}
                    {*********************************}
                    {*    Two-Nucleon Simulation     *}
                    {*          MASTER.PAS           *}
                    {*   Written by John Philpott    *}
                    {*        July XX, 1993.         *}
                    {*   Copyright (c) 1992, 1993    *}
                    {*********************************}
 
 
                                 INTERFACE
 
 
USES
      crt, 
      graph, CUPS, CUPSmupp, CUPSfunc,
      CUPSproc, CUPSgui, CUPSgrph, Consts;


{ ------------------------- Program Organization -------------------------- }

VAR
   MasterMenu :TMenu;    { Master control menu   }
   Hotkeys    :THotKeys; { General hot keys      }
 
PROCEDURE SetUpMasterMenu;
PROCEDURE SetUpHotKeys;

{ ----------------------------- Math Routines ----------------------------- }

FUNCTION Even(J: Integer): Boolean;
FUNCTION MinF(x, y: Float): Float;
FUNCTION MaxF(x, y: Float): Float;
FUNCTION Atan2(x,y : FLOAT): FLOAT;
PROCEDURE Request1DVector(VAR av: DVector; Npoints: Integer);
PROCEDURE Release1DVector(VAR av: DVector);
PROCEDURE Request2DVectors(VAR av, bv: DVector; Npoints: Integer);
PROCEDURE Release2DVectors(VAR av, bv: DVector);
PROCEDURE Request3DVectors(VAR av, bv, cv: DVector; Npoints: Integer);
PROCEDURE Release3DVectors(VAR av, bv, cv: DVector);
PROCEDURE Request1IDVector(VAR iv: IDVector; Npoints: Integer);
PROCEDURE Release1IDVector(VAR iv: IDVector);
PROCEDURE Request1DMatrix(VAR amat: DMatrix; Nrow, Ncol: Integer);
PROCEDURE Release1DMatrix(VAR amat: DMatrix);
PROCEDURE Request2DMatrices(VAR amat, bmat: DMatrix; Nrow, Ncol: Integer);
PROCEDURE Release2DMatrices(VAR amat, bmat: DMatrix);
PROCEDURE Request1CXVector(VAR av: CXVector; Npoints: Integer);
PROCEDURE Release1CXVector(VAR av: CXVector);
PROCEDURE RequestCXVectors(VAR av, bv, cv, ev, fv: CXVector;
                           Npoints: Integer);
PROCEDURE ReleaseCXVectors(VAR av, bv, cv, ev, fv: CXVector);

{ -------------------- Physics (and Math-Phys) Routines ------------------- }

PROCEDURE SetGrid(CoreRad, RmaxInit, Rstep: Float;
   VAR xv : DVector; VAR Nmatch, Nmax: Integer);
FUNCTION Geti(CoreRad, r: Float; Nmax: Integer): Integer;
PROCEDURE SetMuftr(MySystem: Integer);
PROCEDURE CoulombFunctions(eta, xk, r: FLOAT; Lmax: INTEGER;
                       VAR CoulSig, F, G, FP, GP: CoulVtr);

{ ------------------------ Calculate  Potentials ------------------------- }

FUNCTION Yuk(x: Float): Float;
FUNCTION VcSing(r: Float): Float;
FUNCTION VcTrip(r: Float): Float;
FUNCTION Vls(r: Float): Float;
FUNCTION VTens(r: Float): Float;

FUNCTION Zftr(x: Float): Float;
FUNCTION Vc(x, Yuk: Float): Float;
FUNCTION Vt(x, Yuk: Float): Float;
FUNCTION VlsHJ(x, Yuk: Float): Float;
FUNCTION Vll(x, Yuk: Float): Float;

{ ----------------------- Derivatives Calculations ------------------------ }

PROCEDURE DerivsSinglet(r: Real; VAR yv, DyDx: DVector);
PROCEDURE Derivs3P0(r: Real; VAR yv, DyDx: DVector);
PROCEDURE DerivsTriplet(r: Real; VAR yv, DyDx: DVector);
PROCEDURE DerivsCoup(r: Real; VAR yv, DyDx: DVector);
 
PROCEDURE DerivsSingletHJ(r: Real; VAR yv, DyDx: DVector);
PROCEDURE Derivs3P0HJ(r: Real; VAR yv, DyDx: DVector);
PROCEDURE DerivsTripletHJ(r: Real; VAR yv, DyDx: DVector);
PROCEDURE DerivsCoupHJ(r: Real; VAR yv, DyDx: DVector);
 
{ ---------------------- Wavefunction Calculations ------------------------ }

PROCEDURE Wavefunction(E: Float; ibeg, iend, imatch: Integer;
                       Derivs: DFproc;                                  
                       xv: DVector; VAR yv, ysavev: DVector);
PROCEDURE CoupWavefunction(E: Float; ibeg, iend, imatch, itype: Integer;
                       Derivs: DFproc;                                  
                       xv: DVector; VAR uv, wv, ysavev: DVector);
FUNCTION NormInt(dr: Float; ibeg, iend: Integer; yv: DVector): Float;
FUNCTION LineInt(m, c, x: Float): Float;
FUNCTION Nodes(yv: DVector; ibeg, iend: Integer): Integer;

{ *** Routines to calculate tail region integrals involving the s-wave *** }

FUNCTION SWaveTailInt(BE: Float; icalc: Integer; yv: DVector): Float;
FUNCTION SWaveRsqInt(ibeg, iend: Integer; xv, yv: DVector): Float;
FUNCTION SWaveRsqTailInt(BE: Float; icalc: Integer; xv, yv: DVector): Float;
FUNCTION SWaveRsqME(BE: Float; Nmax: Integer; xv, yv: DVector): Float;

{ *** Routines to calculate tail region integrals involving the d-wave *** }
 
FUNCTION ExponIntE1(x: Float): Float;
PROCEDURE ExponInts(x: Float; VAR alpha: alphatype; VAR capE: capEtype);
PROCEDURE DWaveTailInt1(xk, r: Float; VAR Wint: Float);
PROCEDURE AsympVandW(x : Float; VAR v, w: Float);
PROCEDURE DWaveTailInt2(xk, r: Float; VAR Wint: Float);
FUNCTION DWaveTailInt(BE: Float; icalc: Integer; xv, wv: DVector): Float;
PROCEDURE QpoleTailints1(xk, r: Float;
                         VAR Vr2Vint, Vr2Wint, Wr2Wint: Float);
PROCEDURE QpoleTailints2(xk, r: Float;
                         VAR Vr2Vint, Vr2Wint, Wr2Wint: Float);
PROCEDURE QpoleTailInts(BE: Float; icalc: Integer; xv, uv, wv: DVector;
                        VAR Vr2Vint, Vr2Wint, Wr2Wint: Float);
PROCEDURE SDRsqInts(ibeg, iend: Integer; xv, uv, wv: DVector;
                    VAR Vr2Vint, Vr2Wint, Wr2Wint: Float);
PROCEDURE SDWaveMEs(BE: Float; Nmax: Integer;
                    xv, uv, wv: DVector; VAR RsqME, QpoleME: Float);

FUNCTION ModScattRange(ScattLength, ScattRange, CoreRad: Float): Float;
PROCEDURE EstimateBE(ScattLength, ScattRange, CoreRad: Float;
                     VAR BE: Float);

{ ------------------- Access to Tabulated Phase Shifts -------------------- }

PROCEDURE GetPhases(LabKE: Float; VAR PhasesFound: Boolean;
                    VAR Phases: PhaseType);

{ ----------------------- Phase Shift Calculations ------------------------ }

FUNCTION Phase(ysave: DVector; L: Integer): Float;
PROCEDURE FindKay(ysave: DVarray; J: Integer; VAR Kv, Kw, Kc: FLOAT);
PROCEDURE MakePhases(Kv,Kw,Kc: FLOAT; VAR dv,dw,eps: FLOAT);

PROCEDURE SingletPhase(cmEgy: Float; xv, yv: Dvector; Nmax: Integer;
                       VAR CalcPhase: Float);
PROCEDURE DiagTripletPhase(cmEgy: Float; xv, yv: Dvector; Nmax: Integer;
                           VAR CalcPhase: Float);
PROCEDURE TripletP0Phase(cmEgy: Float; xv, yv: Dvector; Nmax: Integer;
                         VAR CalcPhase: Float);
PROCEDURE CoupledPhase(cmEgy: Float; xv, uv, wv: Dvector; Nmax: Integer;
                       VAR vPhase, wPhase, couple: Float);
PROCEDURE NuclearBarPhases(cmEgy: Float; Npoints, Nmax, Nmatch,
                  Jmax: Integer; xv: DVector; VAR phases: PhaseType);

PROCEDURE SingletPhaseHJ(cmEgy: Float; xv, yv: Dvector; Nmax: Integer;
                         VAR CalcPhase: Float);
PROCEDURE DiagTripletPhaseHJ(cmEgy: Float; xv, yv: Dvector; Nmax: Integer;
                             VAR CalcPhase: Float);
PROCEDURE TripletP0PhaseHJ(cmEgy: Float; xv, yv: Dvector; Nmax: Integer;
                           VAR CalcPhase: Float);
PROCEDURE CoupledPhaseHJ(cmEgy: Float; xv, uv, wv: Dvector; Nmax: Integer;
                         VAR vPhase, wPhase, couple: Float);
PROCEDURE NuclearBarPhasesHJ(cmEgy: Float; Npoints, Nmax, Nmatch,
                  Jmax: Integer; xv: DVector; VAR phases: PhaseType);

{ ------------------- Scattering Amplitude Calculations ------------------- }

PROCEDURE ShowAmps(theta: Float; a, b, c, e, f, g: Complex);
PROCEDURE Alphacalc(Phases: PhaseType; LmaxP:Integer);
PROCEDURE IncludeCoulPhases(eta:Float; LmaxP:Integer);
PROCEDURE Coeff(wk:Float; LmaxP, select:integer);
PROCEDURE PutWolfAmps(theta: Float; a, b, c, e, f: Complex; i: Integer);
PROCEDURE GetWolfAmps(VAR theta: Real;
                      VAR a, b, c, e, f: Complex; i: Integer);
PROCEDURE MakeWolf(Theta: Float; Amps: CXarray; i: Integer);
PROCEDURE ScattAmp(LabKE, eta, Thetmin, dThet: Float;
                   LmaxP, numang, select: Integer);
PROCEDURE MakeScattAmps(Phases: PhaseType;
                        Thetmin, Thetmax: Float; numang: Integer);

{ ---------------------- Cross Section Calculations ----------------------- }

PROCEDURE MakeUnpolSigma(i: Integer; VAR theta: real; VAR sig: Float);
PROCEDURE Sigmas1(i: integer; VAR theta: Real; VAR sig, sigP, sigD :Float);
PROCEDURE Sigmas2(i: integer; VAR theta: Real;
                              VAR sig, sigxx, sigxz, sigzz :Float);


{ -------------------------- Interface Routines --------------------------- }

FUNCTION Break: Boolean;
PROCEDURE OpenScrn(VAR Screen: TInputScreen);
PROCEDURE CloseScrn(VAR Screen: TInputScreen);
PROCEDURE PlotPoint(x1, y1: Float);
PROCEDURE Display(i,j: Integer; myLabel: String; value: Float);
PROCEDURE DisplayInt(i,j: Integer; myLabel: String; ival: LongInt);
PROCEDURE PlotDvtr(xv, yv: DVector; ibeg, iend: Integer);
PROCEDURE PreventAdjustments;
PROCEDURE AllowAdjustments;
FUNCTION ThisPotlOK(ThisPotl: PotlConsts): Boolean;
FUNCTION SelectInteraction: Boolean;
PROCEDURE LoadNumbers(VAR ViewScreen: TInputScreen; ThisPotl: PotlConsts);
PROCEDURE GetNumbers(ViewScreen: TInputScreen; VAR ThisPotl: PotlConsts);
FUNCTION Center(inString: String): String;
FUNCTION GetNewPotlParams: Boolean;
PROCEDURE ShowBlankRectangle(x1, x2, y1, y2: Float);
PROCEDURE DrawBlankRectangle(x1, x2, y1, y2: Float);
PROCEDURE PreparePotlPlot(ymin, ymax: Float);
PROCEDURE PrepareWaveFuncPlot(ymin, ymax: Float);
FUNCTION GetGoodEnergy(VAR BE: Float) : Boolean;
PROCEDURE WaitForInstructions(VAR BE, OldDeltaBE: Float;
   VAR LostIt: Boolean);
Function GetEgyRange(VAR MinEgy, MaxEgy: Float;
                     VAR NumEgy: Integer) : Boolean;
PROCEDURE FindReasonableScale(Numang: Integer; VAR Sigmax: Float);

PROCEDURE GeneralInit;

                               IMPLEMENTATION


{ ------------------------- Program Organization -------------------------- }


PROCEDURE SetUpMasterMenu;
BEGIN
   MasterMenu.Init;
   WITH MasterMenu DO
   BEGIN
      column(1, 'File');
         row(1,1, 'About CUPS');
         row(1,2, 'About Program');
         row(1,3, 'Configuration');
         row(1,4, '-------------');
         row(1,5, 'Exit Program');
         rowActivate(1,4,false);
      column(2, 'S-Wave');
         row(2,1, 'About S-Wave Calculations');
         row(2,2, 'Choose the potential');
         row(2,3, 'Adjust the potential');
         row(2,4, 'Automatic search for bound state');
         row(2,5, 'Manual search for bound state');
         row(2,6, 'Compare to 3S1 phase shifts');
         row(2,7, 'Compare to 1S0 phase shifts');
      column(3, 'Deuteron');
         row(3,1, 'About Deuteron Calculations');
         row(3,2, 'Choose the interaction');
         row(3,3, 'Adjust the interaction');
         row(3,4, 'Automatic search for bound state');
         row(3,5, 'Manual search for bound state');
      column(4, 'Phases');
         row(4,1, 'About Phase Calculations');
         row(4,2, 'Choose the interaction');
         row(4,3, 'Adjust the interaction');
         row(4,4, 'Choose the energy range');
         row(4,5, 'Choose and display phases');
      column(5, 'CrossSect');
         row(5,1, 'About Cross Section Calcs');
         row(5,2, 'Choose the system');
         row(5,3, 'Choose the lab energy');
         row(5,4, 'Empirical phase shifts');
         row(5,5, 'Choose the interaction');
         row(5,6, 'Adjust the interaction');
         row(5,7, 'Calculate phase shifts');
         row(5,8, 'Choose the angular range');
         row(5,9, 'Display Cross sections - 1');
         row(5,10, 'Display Cross sections - 2');
      column(6, 'Spin-Lab');
         row(6,1, 'About Spin-Lab');
         row(6,2, 'Choose Setup 1');
         row(6,3, 'Choose Setup 2');
         row(6,4, 'Display Responses');
   END;
END;
 
PROCEDURE SetUpHotKeys;
BEGIN
   WITH HotKeys DO BEGIN
      init(2);
      key[1] := 'F1-Help';
      key[2] := 'F10-Menu';
   END;
END;
 
{ ----------------------------- Math Routines ----------------------------- }

FUNCTION Even(J: Integer): Boolean;
BEGIN
   Even := NOT Odd(J);
END;

FUNCTION MinF(x, y: Float): Float;
BEGIN
   IF x < y THEN MinF := x
            ELSE MinF := y;
END;
 
FUNCTION MaxF(x, y: Float): Float;
BEGIN
   IF x > y THEN MaxF := x
            ELSE MaxF := y;
END;
 
FUNCTION Atan2(x,y : FLOAT): FLOAT;
{ Atan2 yields an angle (in radians) in the range -pi -> pi }
VAR
  theta : FLOAT;
BEGIN
   theta := ArcTan2(x,y);
   IF theta > pi THEN theta := theta - 2*pi;
   Atan2 := theta;
END;
 
PROCEDURE Request1DVector(VAR av: DVector; Npoints: Integer);
BEGIN
   av.Init(Npoints);
END;

PROCEDURE Release1DVector(VAR av: DVector);
BEGIN
   av.Free;
END;

PROCEDURE Request2DVectors(VAR av, bv: DVector; Npoints: Integer);
BEGIN
   av.Init(Npoints);
   bv.Init(Npoints);
END;

PROCEDURE Release2DVectors(VAR av, bv: DVector);
BEGIN
   bv.Free;
   av.Free;
END;

PROCEDURE Request3DVectors(VAR av, bv, cv: DVector; Npoints: Integer);
BEGIN
   av.Init(Npoints);
   bv.Init(Npoints);
   cv.Init(Npoints);
END;

PROCEDURE Release3DVectors(VAR av, bv, cv: DVector);
BEGIN
   cv.Free;
   bv.Free;
   av.Free;
END;

PROCEDURE Request1IDVector(VAR iv: IDVector; Npoints: Integer);
BEGIN
   iv.Init(Npoints);
END;

PROCEDURE Release1IDVector(VAR iv: IDVector);
BEGIN
   iv.Free;
END;

PROCEDURE Request1DMatrix(VAR amat: DMatrix; Nrow, Ncol: Integer);
BEGIN
   amat.Init(Nrow, Ncol);
END;

PROCEDURE Release1DMatrix(VAR amat: DMatrix);
BEGIN
   amat.Free;
END;

PROCEDURE Request2DMatrices(VAR amat, bmat: DMatrix; Nrow, Ncol: Integer);
BEGIN
   amat.Init(Nrow, Ncol);
   bmat.Init(Nrow, Ncol);
END;

PROCEDURE Release2DMatrices(VAR amat, bmat: DMatrix);
BEGIN
   bmat.Free;
   amat.Free;
END;

PROCEDURE Request1CXVector(VAR av: CXVector; Npoints: Integer);
BEGIN
   Request2DVectors(av.RealVec, av.ImagVec, Npoints);
END;

PROCEDURE Release1CXVector(VAR av: CXVector);
BEGIN
   Release2DVectors(av.RealVec, av.ImagVec);
END;

PROCEDURE RequestCXVectors(VAR av, bv, cv, ev, fv: CXVector;
                           Npoints: Integer);
BEGIN
   Request2DVectors(av.RealVec, av.ImagVec, Npoints);
   Request2DVectors(bv.RealVec, bv.ImagVec, Npoints);
   Request2DVectors(cv.RealVec, cv.ImagVec, Npoints);
   Request2DVectors(ev.RealVec, ev.ImagVec, Npoints);
   Request2DVectors(fv.RealVec, fv.ImagVec, Npoints);
END;


PROCEDURE ReleaseCXVectors(VAR av, bv, cv, ev, fv: CXVector);
BEGIN
   Release2DVectors(fv.RealVec, fv.ImagVec);
   Release2DVectors(ev.RealVec, ev.ImagVec);
   Release2DVectors(cv.RealVec, cv.ImagVec);
   Release2DVectors(bv.RealVec, bv.ImagVec);
   Release2DVectors(av.RealVec, av.ImagVec);
END;

{ -------------------- Physics (and Math-Phys) Routines ------------------- }

PROCEDURE SetGrid(CoreRad, RmaxInit, Rstep: Float;
   VAR xv : DVector; VAR Nmatch, Nmax: Integer);
{ Sets GLOBAL VARIABLES Rmatch, Rmax }
VAR
   i : Integer;
   x : Float;
BEGIN
   Rmatch := CoreRad + 2.0 { Fm };
   Nmatch := 2*round(0.5*(Rmatch - CoreRad)/Rstep) + 1;
   Nmax := 2*round(0.5*(RmaxInit - CoreRad)/Rstep) + 1;
   IF Nmatch < 3 THEN Nmatch := 3;
   IF Nmatch > Nmax - 2 THEN Nmatch := Nmax - 2;
   x := CoreRad;
   FOR i := 1 TO Nmax DO
      BEGIN
         xv.Put(i,x);
         x := x + Rstep;
      END;
   Rmatch := xv.Value(Nmatch);
   Rmax   := xv.Value(Nmax);
END;
 
FUNCTION Geti(CoreRad, r: Float; Nmax: Integer): Integer;
{ Returns meshpoint that lies closest to the indicated r value }
BEGIN
   Geti := Round((Nmax-1)*(r - CoreRad)/(Rmax - CoreRad)) + 1;
END;
 
PROCEDURE SetMuftr(MySystem: Integer);
VAR
   mucsq : Float;
BEGIN
   mucsq := neutron*proton/(neutron + proton);
   CASE MySystem OF
1:   muftr := proton/sqr(hbarc);
2:   muftr := 2*mucsq/sqr(hbarc);
3:   muftr := 2*mucsq/sqr(hbarc);
4:   muftr := neutron/sqr(hbarc);
   END;
END;

PROCEDURE MakeSigLs(eta, xk: FLOAT; Lmax: INTEGER; VAR CoulSig: CoulVtr);
{ Calculate Coulomb phase shifts from formulae 14.5.6, 14.5.7
  and 6.1.41 (See also 6.1.40) in Abramowitz and Stegun.
  The Asymptotic series is used for the highest L.   Sigma(L)'s
  for lower L are obtained by recursion.}
 
VAR
   rho, rhosq : FLOAT;
   ftr, phase : FLOAT;
   L          : INTEGER;
   sin1, sin3, sin5, sin7, sin9 : FLOAT;
 
BEGIN { MakeSigLs }
   rhosq := SQR(Lmax + 1) + SQR(eta);
   rho := SQRT(rhosq);
   ftr := 1/rhosq;
   phase := ArcTan(eta/(Lmax + 1));
 
{  The following compound statement led to an 8087 stack overflow:
 
   CoulSig[Lmax] := (Lmax + 0.5)*phase + eta*(ln(rho) - 1)
      - (sin(phase)/12 - ftr*(sin(3*phase)/360
      - ftr*(sin(5*phase)/1260 - ftr*(sin(7*phase)/1680
      - ftr*(sin(9*phase)/1188)))))/rho;
 
   It has been replaced by an equivalent set of shorter statements }
 
   sin1 := sin(phase);
   sin3 := sin(3*phase);
   sin5 := sin(5*phase);
   sin7 := sin(7*phase);
   sin9 := sin(9*phase);
 
   CoulSig[Lmax] := (Lmax + 0.5)*phase + eta*(ln(rho) - 1)
      - (sin1/12 - ftr*(sin3/360
      - ftr*(sin5/1260 - ftr*(sin7/1680
      - ftr*(sin9/1188)))))/rho;
 
   FOR L := Lmax DOWNTO 1 DO
      BEGIN
         CoulSig[L-1] := CoulSig[L] - ArcTan(eta/L);
      END;
END;  { MakeSigLs }
 
PROCEDURE TryMakeG(r, xk, eta, SigZero: FLOAT;
               VAR Gzero, GPzero: FLOAT; VAR Gmade: Boolean);
{ Attempt to calculate G and GP (for L = 0) from the asymptotic
  series given in Abramowitz and Stegun, section 14.5
  Gmade is set to TRUE if the calculation is successful }
 
CONST
   NumSum = 50;  { Max number of terms in the f, g, f*, g* recursions }
   eps  = 1e-7;  { Desired accuracy level }
 
VAR
   rho, tworho, etasq           : FLOAT;
   f, g, fp, gp, c, s           : FLOAT;
   fterm, gterm, fpterm, gpterm : FLOAT;
   ak, bk, den, temp, W, theta  : FLOAT;
   k, kp  : INTEGER;
 
BEGIN { TryMakeG }
   Gmade := FALSE;
   rho := r*xk;   tworho := 2*rho;   etasq := SQR(eta);
   f     := 1;  g     := 0;  fp     := 0;   gp     := 1 - eta/rho;
   fterm := f;  gterm := g;  fpterm := fp;  gpterm := gp;
   k := 0;
   REPEAT
      kp := k + 1;  den := kp*tworho;
      ak := (k + kp)*eta/den;  bk := (etasq - k*kp)/den;
      temp  := fterm;
      fterm := ak*fterm - bk*gterm;
      gterm := ak*gterm + bk*temp;
      temp   := fpterm;
      fpterm := ak*fpterm - bk*gpterm - fterm/rho;
      gpterm := ak*gpterm +  bk*temp  - gterm/rho;
      k := kp;
      f  := f  +  fterm;  g  := g  +  gterm;
      fp := fp + fpterm;  gp := gp + gpterm;
      W  := ABS(f*gp-g*fp-1);
   UNTIL (k = NumSum) OR ( W < eps);
   IF W < eps THEN
      BEGIN
         theta := rho - eta*ln(tworho) + SigZero;
         c := cos(theta);  s := sin(theta);
         Gzero  := f*c - g*s;  GPzero := xk*(fp*c - gp*s);
         Gmade := TRUE;
      END;
END;  { TryMakeG }
 
PROCEDURE MakeGzero(eta, xk, SigZero, ra: FLOAT; VAR Gzero, GPzero: FLOAT);
{ Calculate G and GP for L = 0.   Use the asymptotic series at various
  radii.    If necessary, use the Coulomb differential equation to
  propagate G and GP inwards. }
 
CONST
   Num2h  = 10;  { Number of 2h steps taken when increasing r }
   h   = 0.005;  { Length of integration step for Gzero }
 
VAR
   r, Gzprime, dummy : FLOAT;
   Gmade             : Boolean;
   Nstep, i          : INTEGER;
   fivsix, twelth    : FLOAT;
   xksq, num, sum    : FLOAT;
   ftr1, ftr2, ftr3  : FLOAT;
   val1, val2, val3  : FLOAT;
 
BEGIN { MakeGzero }
   Gmade := FALSE;
   Nstep := -2*Num2h;
   WHILE NOT Gmade DO
      BEGIN
         Nstep := Nstep + 2*Num2h;
         r := ra + Nstep*h;
         TryMakeG( {Input} r, xk, eta, SigZero,
                  {Output} Gzero, GPzero, Gmade);
         IF Gmade AND (Nstep <> 0) THEN
            BEGIN
               TryMakeG( {Input} r + h, xk, eta, SigZero,
                        {Output} Gzprime, dummy, Gmade);
            END;
      END;
   IF Nstep <> 0 THEN
      BEGIN { Inward integration (Numerov) }
         xksq := SQR(xk);  num := 2*eta*xk;
         fivsix := 5*h*h/6; twelth := fivsix/10;
         ftr1 := xksq - num/(r+h);  val1 := Gzprime;
         ftr2 := xksq - num/r;  val2 := Gzero;
         sum := ftr2*val2;
         Nstep := Round(Nstep/2);
         FOR i := Nstep DOWNTO 1 DO
            BEGIN
               r := r - h;  ftr3 := xksq - num/r;
               val3 := ( (2 - fivsix*ftr2)*val2 - (1 + twelth*ftr1)*val1 )/
                         (1 + twelth*ftr3);
               r := r - h;  ftr1 := xksq - num/r;
               val1 := ( (2 - fivsix*ftr3)*val3 - (1 + twelth*ftr2)*val2 )/
                         (1 + twelth*ftr1);
               sum := 2*ftr1*val1 + 4*ftr3*val3 + sum;
               ftr2 := ftr1;  val2 := val1;
               ftr1 := ftr3;  val1 := val3;
            END;
         Gzero  := val2;
         GPzero := GPzero + h*(sum - ftr2*val2)/3;
      END;
END;  { MakeGzero }
 
PROCEDURE MakeFLs(eta, xk, r, Gzero, GPzero: FLOAT; Lmax: INTEGER;
              VAR F: CoulVtr; VAR FPzero: FLOAT);
{ Calculate unnormalized FL's by downward resursion (Abramowitz
  and Stegun, 14.2.3)   Determine the proper normalization from
  the Wronskian of F and G for L = 0, ibid. 14.2.4 }
VAR
   rho, etasq  : FLOAT;
   oldftr, ftr : FLOAT;
   L           : INTEGER;
 
BEGIN { MakeFLs }
   etasq := SQR(eta);
   rho := r*xk;
   L := LCoul + 6;
   F[ L ] := 0;
   oldftr := SQRT(SQR(L) + etasq)/L;
   F[ L-1 ] := 1e-36;
   FOR L := LCoul + 5 DOWNTO 1 DO
      BEGIN
         ftr := SQRT(SQR(L) + etasq)/L;
         F[ L-1 ] := ( (2*L+1)*(eta/(L*(L+1)) + 1/rho)*F[ L ]
                           - oldftr*F[ L+1 ] )/ftr;
         oldftr := ftr;
      END;
   FPzero := xk*( (eta + 1/rho)*F[0] - SQRT(1 + etasq)*F[1] );
   ftr := xk/(FPzero*Gzero-F[0]*GPzero);
   FOR L := 0 to Lmax DO
      BEGIN
         F[ L ] := ftr*F[ L ];
      END;
   FPzero := ftr*FPzero;
END;  { MakeFLs }
 
PROCEDURE MakeGLs(eta, xk, r: FLOAT; Lmax: INTEGER;
              VAR F, G, FP, GP: CoulVtr);
{ Calculate the FPL's from the FL's and the GL's and GPL's by
  upward resursion (Abramowitz and Stegun, 14.2.1 and 14.2.2) }
CONST
   eps  = 1e-7;  { Desired accuracy level }
VAR
   etasq, rho : FLOAT;
   ftr1, ftr2 : FLOAT;
   W          : FLOAT;
   L          : INTEGER;
 
BEGIN { MakeGLs }
   etasq := SQR(eta);
   rho := r*xk;
   FOR L := 1 TO Lmax DO
      BEGIN
         ftr1 := SQRT(SQR(L) + etasq)/L;
         ftr2 := (L/rho) + (eta/L);
         FP[ L ] := (ftr1*F[ L-1 ] - ftr2*F[ L ])*xk;
         G[  L ] := (ftr2*G[ L-1 ] - GP[ L-1 ]/xk)/ftr1;
         GP[ L ] := (ftr1*G[ L-1 ] - ftr2*G[ L ])*xk;
         W := ABS( (G[L]*FP[L]-F[L]*GP[L])/xk - 1 );
         IF W > eps THEN
            BEGIN
               Writeln('Trouble in MakeGLs.  L = ',L,'   W = ',W:15:8);
            END;
      END;
END;  { MakeGLs }
 
PROCEDURE CoulombFunctions(eta, xk, r: FLOAT; Lmax: INTEGER;
                       VAR CoulSig, F, G, FP, GP: CoulVtr);
{ Calculate Coulomb wave functions for a positive energy.
  Inputs are  eta     = Coulomb parameter
              xk      = wavenumber
              Rmatch  = matching radius
              Lmax    = Largest L value needed
  Outputs are CoulSig = Coulomb phase shifts
              and the Coulomb functions, F, G, FP, GP. }
 
VAR
   SigZero : FLOAT;
   Gzero, Fzero, GPzero, FPzero : FLOAT;
   L : INTEGER;
 
BEGIN { CoulombFunctions }
 
   FOR L := 0 TO LCoulPlus DO
      BEGIN
         Coulsig[L] := 0;
         F[L]       := 0;
         G[L]       := 0;
         FP[L]      := 0;
         GP[L]      := 0;
      END;
 
   MakeSigLs(eta, xk, Lmax, CoulSig);
   SigZero := CoulSig[0];
   MakeGzero(eta, xk, SigZero, r, Gzero, GPzero);
   MakeFLs(eta, xk, r, Gzero, GPzero, Lmax, F, FPzero);
   G[0] := Gzero;  GP[0] := GPzero;  FP[0] := FPzero;
   MakeGLs(eta, xk, r, Lmax, F, G, FP, GP);
 
END;  { CoulombFunctions }


{ ------------------------ Potential Calculations ------------------------- }

FUNCTION Yuk(x: Float): Float;
{ Yukawa radial form ( x = r/PotlRange ) }
BEGIN
   IF x = 0 THEN Yuk := Large
   ELSE Yuk := exp(-x)/x;
END;
 
FUNCTION VcSing(r: Float): Float;
VAR
   temp : Float;
BEGIN
   WITH SingletPotl DO
      BEGIN
         IF CNdepth <> 0 THEN
              temp := -CNdepth*Yuk(r/CNrange)
         ELSE temp := 0;
      END;
 { Add OPEP term, if requested }
   IF OPEP THEN temp := temp + VcOPEP*Yuk(r/OPEPrange);
 { Add Coulomb term, if necessary }
   IF MySystem = 1 THEN temp := temp + esq/r;
   VcSing := temp;
END;
 
FUNCTION VcTrip(r: Float): Float;
VAR
   temp : Float;
BEGIN
   WITH TripletPotl DO
      BEGIN
         IF CNdepth <> 0 THEN
              temp := -CNdepth*Yuk(r/CNrange)
         ELSE temp := 0;
      END;
 { Add OPEP term, if requested }
   IF OPEP THEN temp := temp + VcOPEP*Yuk(r/OPEPrange);
 { Add Coulomb term, if necessary }
   IF MySystem = 1 THEN temp := temp + esq/r;
   VcTrip := temp;
END;
 
FUNCTION Vls(r: Float): Float;
BEGIN
   WITH TripletPotl DO
      BEGIN
         IF LSdepth <> 0 THEN
              Vls := -LSdepth*Yuk(r/LSrange)
         ELSE Vls := 0;
      END;
END;
 
FUNCTION VTens(r: Float): Float;
VAR
   temp, x : Float;
BEGIN
   WITH TripletPotl DO
      BEGIN
         IF TNdepth <> 0 THEN
              temp := -TNdepth*Yuk(r/TNrange)
         ELSE temp := 0;
      END;
 { Add OPEP term, if requested }
   IF OPEP THEN
      BEGIN
         x := r/OPEPrange;
         temp := temp + VtOPEP*Zftr(x)*Yuk(x);
      END;
   VTens := temp;
END;

{ Potentials taken from HAMADA and JOHNSTON,
    Nuclear Physics 34 (1962), p382 }

FUNCTION Zftr(x: Float): Float;
BEGIN
     Zftr := 1 + (3 + 3/x)/x;
END;
 
FUNCTION Vc(x, Yuk: Float): Float;
VAR
   temp, t2 :Float;
BEGIN
   WITH PotlHJ DO
      BEGIN
         t2   := 1 + (Ac + Bc*Yuk)*Yuk;
         temp := Vcen*t2*Yuk;
      END;
 { Add Coulomb term, if necessary }
   IF MySystem = 1 THEN temp := temp + esq/(x*pionCompLen);
   Vc   := temp;
END;
 
FUNCTION Vt(x, Yuk: Float): Float;
VAR
   temp, t2 :Float;
BEGIN
   WITH PotlHJ DO
      BEGIN
         temp := Vten*Zftr(x);
         t2   := 1 + (At + Bt*Yuk)*Yuk;
         Vt   := temp*t2*Yuk;
      END;
END;
 
FUNCTION VlsHJ(x, Yuk: Float): Float;
BEGIN
   WITH PotlHJ DO
      BEGIN
         VlsHJ := Vlsz*(1 + Bls*Yuk)*Sqr(Yuk);
      END;
END;
 
FUNCTION Vll(x, Yuk: Float): Float;
VAR
   temp, t2 :Float;
BEGIN
   WITH PotlHJ DO
      BEGIN
         temp := Zftr(x);
         temp := Vllz*temp;
         temp := temp/sqr(x);
         t2   := 1 + (All + Bll*Yuk)*Yuk;
         Vll  := temp*t2*Yuk;
      END;
END;

{ ---------------------- Derivatives Calculations ------------------------- }

PROCEDURE DerivsSinglet(r: Real; VAR yv, DyDx: DVector);
{ Uses GLOBAL VARIABLES: CapJ, Energy, muftr }
BEGIN
   DyDx.Put(1,yv.Value(2));
   DyDx.Put(2,(VcSing(r) - Energy)*yv.Value(1));
   DyDx.Put(2,muftr*DyDx.Value(2) + CapJ*(CapJ+1)/sqr(r)*yv.Value(1));
END;
 
PROCEDURE Derivs3P0(r: Real; VAR yv, DyDx: DVector);
{ Uses GLOBAL VARIABLES: CapJ, Energy, muftr }
VAR
   Cent, Tens, SpinOrb, Vpotl: Float;
   Jp1,Jp2 :FLoat;
BEGIN
   Cent := VcTrip(r);
   Tens := VTens(r);
   SpinOrb := Vls(r);
   Jp1:=CapJ+1; Jp2:=CapJ+2;
   Vpotl := Cent - Jp2*SpinOrb - 2*Jp2/(2*CapJ+1)*Tens;
   DyDx.Put(1,yv.Value(2));
   DyDx.Put(2,(Vpotl - Energy)*yv.Value(1));
   DyDx.Put(2,muftr*DyDx.Value(2) + Jp1*Jp2/sqr(r)*yv.Value(1));
END;
 
PROCEDURE DerivsTriplet(r: Real; VAR yv, DyDx: DVector);
{ Uses GLOBAL VARIABLES: CapJ, Energy, muftr }
VAR
   Cent, Tens, SpinOrb, Vpotl: Float;
   Jp1 :FLoat;
BEGIN
   Cent := VcTrip(r);
   Tens := VTens(r);
   SpinOrb := Vls(r);
   Jp1:=CapJ+1;
   Vpotl := Cent - SpinOrb + 2*Tens;
   DyDx.Put(1,yv.Value(2));
   DyDx.Put(2,(Vpotl - Energy)*yv.Value(1));
   DyDx.Put(2,muftr*DyDx.Value(2) + CapJ*Jp1/sqr(r)*yv.Value(1));
END;
 
PROCEDURE DerivsCoup(r: Real; VAR yv, DyDx: DVector);
{ Uses GLOBAL VARIABLES: CapJ, Energy, muftr }
{ Assumes  v : J-1,  w : J+1 }
VAR
   ftr : Float;
   Cent, Tens, SpinOrb    : Float;
   Vpotl, Wpotl, Coupling : Float;
   Jm1, Jp2               : Float;
BEGIN
   ftr := 2*CapJ + 1;
   Coupling := 6*sqrt(CapJ*(CapJ+1))/ftr;
   Cent := VcTrip(r);
   Tens := VTens(r);
   SpinOrb := Vls(r);
   Jm1:=CapJ-1;  Jp2:=CapJ+2;
   Vpotl := Cent + Jm1*SpinOrb - (2*Jm1/ftr)*Tens;
   Wpotl := Cent - Jp2*SpinOrb - (2*Jp2/ftr)*Tens;
   DyDx.Put(1,yv.Value(2));
   DyDx.Put(3,yv.Value(4));
   DyDx.Put(2,(Vpotl - Energy)*yv.Value(1) + Coupling*Tens*yv.Value(3));
   DyDx.Put(4,(Wpotl - Energy)*yv.Value(3) + Coupling*Tens*yv.Value(1));
   DyDx.Put(2,muftr*DyDx.Value(2) +  CapJ  * Jm1/sqr(r)*yv.Value(1));
   DyDx.Put(4,muftr*DyDx.Value(4) +(CapJ+1)*Jp2/sqr(r)*yv.Value(3));
END;
 

PROCEDURE DerivsSingletHJ(r: Real; VAR yv, DyDx: DVector);
{ This version includes all four potentials.  Assumes  v : J }
{ Uses GLOBAL VARIABLES: CapJ, Energy, muftr }
VAR
   x, Yukawa, Cent, Tens, QSpinOrb, Vpotl: Float;
BEGIN
   x := r/pionCompLen;
   Yukawa := Yuk(x);
   Cent := Vc(x,Yukawa);
   QSpinOrb := Vll(x,Yukawa);
   Vpotl := Cent - 2*CapJ*(CapJ+1)*QSpinOrb;
   DyDx.Put(1,yv.Value(2));
   DyDx.Put(2,(Vpotl - Energy)*yv.Value(1));
   DyDx.Put(2,muftr*DyDx.Value(2) + CapJ*(CapJ+1)/sqr(r)*yv.Value(1));
END;
 
PROCEDURE Derivs3P0HJ(r: Real; VAR yv, DyDx: DVector);
{ This version includes all four potentials.  Assumes  v : J+1 }
{ Uses GLOBAL VARIABLES: CapJ, Energy, muftr }
VAR
   x, Yukawa, Cent, Tens, SpinOrb, QSpinOrb, Vpotl: Float;
   Jp1,Jp2 :FLoat;
BEGIN
   x := r/pionCompLen;
   Yukawa := Yuk(x);
   Cent := Vc(x,Yukawa);
   Tens := Vt(x,Yukawa);
   SpinOrb := VlsHJ(x,Yukawa);
   QSpinOrb := Vll(x,Yukawa);
   Jp1:=CapJ+1; Jp2:=CapJ+2;
   Vpotl := Cent - Jp2*SpinOrb - 2*Jp2/(2*CapJ+1)*Tens - 2*Jp2*QSpinOrb;
   DyDx.Put(1,yv.Value(2));
   DyDx.Put(2,(Vpotl - Energy)*yv.Value(1));
   DyDx.Put(2,muftr*DyDx.Value(2) + Jp1*Jp2/sqr(r)*yv.Value(1));
END;
 
PROCEDURE DerivsTripletHJ(r: Real; VAR yv, DyDx: DVector);
{ This version includes all four potentials.  Assumes  v : J }
{ Uses GLOBAL VARIABLES: CapJ, Energy, muftr }
VAR
   x, Yukawa, Cent, Tens, SpinOrb, QSpinOrb, Vpotl: Float;
   Jp1 :FLoat;
BEGIN
   x := r/pionCompLen;
   Yukawa := Yuk(x);
   Cent := Vc(x,Yukawa);
   Tens := Vt(x,Yukawa);
   SpinOrb := VlsHJ(x,Yukawa);
   QSpinOrb := Vll(x,Yukawa);
   Jp1:=CapJ+1;
   Vpotl := Cent - SpinOrb + 2*Tens + (2*CapJ*Jp1-1)*QSpinOrb;
   DyDx.Put(1,yv.Value(2));
   DyDx.Put(2,(Vpotl - Energy)*yv.Value(1));
   DyDx.Put(2,muftr*DyDx.Value(2) + CapJ*Jp1/sqr(r)*yv.Value(1));
END;
 
PROCEDURE DerivsCoupHJ(r: Real; VAR yv, DyDx: DVector);
{ This version includes all four potentials.  Assumes  v : J-1,  w : J+1 }
{ Uses GLOBAL VARIABLES: CapJ, Energy, muftr }
VAR
   x, Yukawa, ftr, QSpinOrb : Float;
   Cent, Tens, SpinOrb      : Float;
   Vpotl, Wpotl, Coupling   : Float;
   Jm1, Jp2                 : Float;
BEGIN
   ftr := 2*CapJ + 1;
   Coupling := 6*sqrt(CapJ*(CapJ+1))/ftr;
   x := r/pionCompLen;
   Yukawa := Yuk(x);
   Cent := Vc(x,Yukawa);
   Tens := Vt(x,Yukawa);
   SpinOrb := VlsHJ(x,Yukawa);
   QSpinOrb := Vll(x,Yukawa);
   Jm1:=CapJ-1;  Jp2:=CapJ+2;
   Vpotl := Cent + Jm1*SpinOrb - (2*Jm1/ftr)*Tens + Jm1*QSpinOrb;
   Wpotl := Cent - Jp2*SpinOrb - (2*Jp2/ftr)*Tens - Jp2*QSpinOrb;
   DyDx.Put(1,yv.Value(2));
   DyDx.Put(3,yv.Value(4));
   DyDx.Put(2,(Vpotl - Energy)*yv.Value(1) + Coupling*Tens*yv.Value(3));
   DyDx.Put(4,(Wpotl - Energy)*yv.Value(3) + Coupling*Tens*yv.Value(1));
   DyDx.Put(2,muftr*DyDx.Value(2) +  CapJ  * Jm1/sqr(r)*yv.Value(1));
   DyDx.Put(4,muftr*DyDx.Value(4) +(CapJ+1)*Jp2/sqr(r)*yv.Value(3));
END;

{ ----------------------- Wavefunction Calculations ----------------------- }

PROCEDURE Wavefunction(E: Float; ibeg, iend, imatch: Integer;
                       Derivs: DFproc;                                  
                       xv: DVector; VAR yv, ysavev: DVector);
{ Resets GLOBAL VARIABLE: Energy }
{ Calculate the radial wavefunction at energy E.  Integration
  can run in either direction using starting values described
  below.   Inputs are a pointer to a "derivatives" procedure
  (Derivs) and a grid of pivot points (xv).   Outputs are the
  values of the wavefunction at the pivot points as well as
  the value and derivative of the wavefunction at a matching
  radius. }
VAR
   i1, i2, di, i, ii, j  : Integer;
   tempv : DVector;
   r, dr : Real;
   expftr, kappa : Float;
BEGIN
   Energy := E;   { Needed in Derivs     }
   Request1DVector(tempv, 2);
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
         expftr := exp(-kappa*xv.Value(iend));
         tempv.Put(1, expftr);
         tempv.Put(2, -kappa*expftr);
      END;
   r := xv.Value(ibeg);
   dr := xv.Value(ibeg + di) - r;
   i := ibeg;
   yv.Put(i,tempv.Value(1));
   FOR ii := i1 + 1 TO i2 DO
      BEGIN
         StepRK4(Derivs, r, dr, tempv);
         i := i + di;
         yv.Put(i,tempv.Value(1));
         IF i = imatch THEN FOR j := 1 TO 2 DO
            ysavev.Put(j, tempv.Value(j));
      END;
   Release1DVector(tempv);
END;

PROCEDURE CoupWavefunction(E: Float; ibeg, iend, imatch, itype: Integer;
                       Derivs: DFproc;                                  
                       xv: DVector; VAR uv, wv, ysavev: DVector);
{ Resets GLOBAL VARIABLE: Energy }
{ Calculate coupled radial wavefunctions of type itype at energy E.
  Integration can run in either direction using starting values
  described below.   Inputs are a pointer to a "derivatives"
  procedure (Derivs) and a grid of pivot points (xv).   Outputs are
  the values of the wavefunction at the pivot points as well as the
  value and derivative of the wavefunction at a matching radius. }

   PROCEDURE StartLeft(Aone, Atwo: Float; VAR yv: DVector);
   BEGIN
      yv.Put(1, Aone*0); {  v   }
      yv.Put(2, Aone*1); { dvdr }
      yv.Put(3, Atwo*0); {  w   }
      yv.Put(4, Atwo*1); { dwdr }
   END;

   PROCEDURE StartRight(Bone, Btwo, kappa, r : Float; VAR yv: DVector);
   VAR
      x, expftr : Float;
   BEGIN
      x    := kappa*r;
      expftr := exp(-x);
      yv.Put(1, Bone*expftr);                                  {  v   }
      yv.Put(2, Bone*(-kappa)*expftr);                         { dvdr }
      yv.Put(3, Btwo*(1 + (3 + 3/x)/x)*expftr);                {  w   }
      yv.Put(4, Btwo*(-kappa)*(1 + (3 + (6+6/x)/x)/x)*expftr); { dwdr }
   END;

VAR
   i1, i2, di    : Integer;
   i, ii, j      : Integer;
   tempv         : DVector;
   r, dr         : Real;
   expftr, kappa : Float;
   Aone, Atwo    : Float;
BEGIN
   Energy := E;   { Needed in Derivs     }
   Request1DVector(tempv, 4);
   CASE itype OF
      1: BEGIN  Aone := 1;  Atwo := 0;  END;
      2: BEGIN  Aone := 0;  Atwo := 1;  END;
   END;

   r := xv.Value(ibeg);
   IF ibeg < iend THEN { Prepare for outward integration }
      BEGIN
         i1 := ibeg;  i2 := iend;  di :=  1;
         { Initialize radial wavefunction to zero value
           and unit slope at origin or core radius }
         StartLeft(Aone,Atwo,tempv);
      END

   ELSE IF ibeg > iend THEN { Prepare for inward integration }
      BEGIN
         i1 := iend;  i2 := ibeg;  di := -1;
         { Initialize to exp functions at the outer radius. }
         { ASSUMES E is NEGATIVE (bound state wavefunction) }
         kappa := sqrt(-muftr*E);
         StartRight(Aone,Atwo,kappa,r,tempv);
      END;

   dr := xv.Value(ibeg + di) - r;
   i := ibeg;
   uv.Put(i,tempv.Value(1));
   wv.Put(i,tempv.Value(3));
   FOR ii := i1 + 1 TO i2 DO
      BEGIN
         StepRK4(Derivs, r, dr, tempv);
         i := i + di;
         uv.Put(i,tempv.Value(1));
         wv.Put(i,tempv.Value(3));
         IF i = imatch THEN FOR j := 1 TO 4 DO
            ysavev.Put(j, tempv.Value(j));
      END;
   Release1DVector(tempv);
END;

FUNCTION NormInt(dr: Float; ibeg, iend: Integer; yv: DVector): Float;
{ Calculate the integral of the squared wavefunction between two radii.
  Note:  If  iend - ibeg  is not a positive, even integer, then
         the calculation fails and NormInt is set to zero. }
VAR
   i, idiff   : Integer;
   imax, ival : Integer;
   sum1, sum2 : Float;
BEGIN
   idiff := iend - ibeg;
   imax := round(idiff/2);
   IF idiff = 2*imax THEN
      BEGIN { Simpson's rule integration }
         sum1 := 0.0;  sum2 := 0.0;
         ival := ibeg;
         FOR i := 1 TO imax DO
            BEGIN
               ival := ival + 2;
               sum1 := sum1 + Sqr(yv.Value(ival-1));
               sum2 := sum2 + Sqr(yv.Value(ival));
            END;
         sum1 := 4.0*sum1;  sum2 := 2.0*sum2;
         NormInt := (Sqr(yv.Value(ibeg)) + sum1 + sum2
                      - Sqr(yv.Value(iend)))*dr/3.0;
      END ELSE NormInt := 0;
END;
 
FUNCTION LineInt(m, c, x: Float): Float;
{ Indefinite integral wrt x of the function sqr(m*x + c). }
BEGIN
   LineInt := x*(sqr(m*x)/3 + c*(m*x + c));
END;
 
FUNCTION Nodes(yv: DVector; ibeg, iend: Integer): Integer;
 
   FUNCTION Isign(x: Float): Integer;
   BEGIN
      IF x >= 0 THEN Isign := 1
                ELSE Isign := -1;
   END;
 
VAR
   i1, i2, ii, i, di  : Integer;
   ThisSign, PrevSign : Integer;
   NodeCount          : Integer;
BEGIN
   NodeCount := 0;
   IF ibeg <> iend THEN
      BEGIN
         IF ibeg < iend THEN
            BEGIN
               i1 := ibeg;  i2 := iend;  di := 1;
            END ELSE
            BEGIN
               i1 := iend;  i2 := ibeg;  di := -1
            END;
         i := ibeg;
         IF i = 1 THEN PrevSign := Isign(yv.Value(2))
                  ELSE PrevSign := Isign(yv.Value(i));
         FOR ii := i1 + 1 TO i2 DO
            BEGIN
               i := i + di;
               ThisSign := Isign(yv.Value(i));
               IF ThisSign <> PrevSign THEN
                  BEGIN
                     Inc(NodeCount);
                     PrevSign := ThisSign;
                  END;
            END;
      END;
   Nodes := NodeCount;
END;
 
{ *** Routines to calculate tail region integrals involving the s-wave *** }

FUNCTION SWaveTailInt(BE: Float; icalc: Integer; yv: DVector): Float;
{ Uses the known asymptotic form of the s-wave function at a given
  binding energy to obtain the contribution to the normalization
  integral for radii beyond that specified by icalc }
{ Uses GLOBAL VARIABLE muftr }
BEGIN
   SWaveTailInt := sqr(yv.Value(icalc))/(2*sqrt(muftr*BE));
END;
 
FUNCTION SWaveRsqInt(ibeg, iend: Integer; xv, yv: DVector): Float;
{ Calculate the integral of the r^2 times the squared wavefunction
  between two radii.
  Note:  If  iend - ibeg  is not a positive, even integer, then
         the calculation fails and SWaveRsqInt is set to zero. }
VAR
   i, idiff, imax, ival : Integer;
   sum1, sum2, dr       : Float;
BEGIN
   dr := xv.Value(2) - xv.Value(1);
   idiff := iend - ibeg;
   imax := round(idiff/2);
   IF idiff = 2*imax THEN
      BEGIN { Simpson's rule integration }
         sum1 := 0.0;  sum2 := 0.0;
         ival := ibeg;
         FOR i := 1 TO imax DO
            BEGIN
               ival := ival + 2;
               sum1 := sum1 + Sqr(xv.Value(ival-1)*yv.Value(ival-1));
               sum2 := sum2 + Sqr(xv.Value(ival)*yv.Value(ival));
            END;
         sum1 := 4.0*sum1;  sum2 := 2.0*sum2;
         SWaveRsqInt := (Sqr(xv.Value(ibeg)*yv.Value(ibeg)) + sum1 + sum2
                      - Sqr(xv.Value(iend)*yv.Value(iend)))*dr/3.0;
      END
   ELSE SWaveRsqInt := 0;
END;
 
FUNCTION SWaveRsqTailInt(BE: Float; icalc: Integer; xv, yv: DVector): Float;
{ Uses the known asymptotic form of the s-wave function at a given
  binding energy to obtain the contribution to the expectation value of
  r^2 for radii beyond that specified by icalc }
{ Uses GLOBAL VARIABLE muftr }
VAR
   xk, x : Float;
BEGIN
   xk := sqrt(muftr*BE);
   x := 2*xk*xv.Value(icalc);
   SWaveRsqTailInt := sqr(yv.Value(icalc))*((x + 2)*x + 2)/Pwr(2*xk,3);
END;
 
FUNCTION SWaveRsqME(BE: Float; Nmax: Integer; xv, yv: DVector): Float;
{ Calculate the matrix element of r^2 in a bound s-wave radial function
 (Factor of (1/2)^2 accounts for difference between the separation of
  the nucleons and their distance from the deuteron's center of mass) }
BEGIN
   SWaveRsqME := 0.25*(SWaveRsqInt(1, Nmax, xv, yv)
                 + SWaveRsqTailInt(BE, Nmax, xv, yv));
END;

{ *** Routines to calculate tail region integrals involving the d-wave *** }
 
FUNCTION ExponIntE1(x: Float): Float;
 
   FUNCTION ExponInt1: Float;
   { Approximate fromula for E1(x) valid for  0 <= x <= 1
     See Abramowitz and Stegun, formula 5.1.53, p 231 }
   VAR
      poly : Float;
   BEGIN
      poly := -0.57721566 + x*( 0.99999193 + x*(-0.24991055 + x*
              (0.05519968 + x*(-0.00976004 + x*  0.00107857))));
      ExponInt1 := poly - ln(x);
   END;
 
   FUNCTION ExponInt2: Float;
   { Approximate fromula for E1(x) valid for  1 <= x <= inf.
     See Abramowitz and Stegun, formula 5.1.56, p 231 }
   VAR
      num, den : Float;
   BEGIN
      num := (((x + 8.5733287401)*x + 18.0590169730)*x
                  + 8.6347608925)*x + 0.2677737343;
      den := ((((x + 9.5733223454)*x + 25.6329561486)*x
                   + 21.0996530827)*x + 3.9584969228)*x;
      ExponInt2 := num*exp(-x)/den;
   END;
 
BEGIN
   IF x < 1 THEN ExponIntE1 := ExponInt1
            ELSE ExponIntE1 := ExponInt2;
END;
 
PROCEDURE ExponInts(x: Float; VAR alpha: alphatype; VAR capE: capEtype);
{ Calculate the exponential integrals needed for s- and d-wave tail
  region normalization integrals.   The upward recurrence procedure
  for the capE integrals is valid only for x < about 5. }
VAR
   expftr : Float;
   n      : Integer;
BEGIN
   expftr := exp(-x);
   alpha[0] := expftr/x;
   alpha[1] := (x + 1)*alpha[0]/x;
   alpha[2] := ((x + 2)*x + 2)*alpha[0]/sqr(x);
   capE[1] := ExponIntE1(x);
   FOR n := 1 TO 3 DO
      BEGIN
         capE[n+1] := (expftr - x*capE[n])/n;
      END;
END;
 
PROCEDURE DWaveTailInt1(xk, r: Float; VAR Wint: Float);
{ Calculates the normalization integral from r to infinity
  from approximate representations of exponential integrals }
VAR
   x, ftr : Float;
   alpha  : alphatype;
   capE   : capEtype;
BEGIN
   x := 2*xk*r;
   IF x < 5 THEN
      BEGIN
         ExponInts(x, alpha, capE);
         ftr := 1/(xk*r);
         Wint := ((9*capE[4]*ftr + 18*capE[3])*ftr + 15*capE[2])*ftr;
         Wint := r*((Wint + 6*capE[1])*ftr + alpha[0]);
      END
   ELSE
      BEGIN
         Wint := 0;
      END;
END;
 
PROCEDURE AsympVandW(x : Float; VAR v, w: Float);
VAR
   expftr : Float;
BEGIN
   expftr := exp(-x);
   v := expftr;
   w := (1 + (3 + 3/x)/x)*expftr;
END;
 
PROCEDURE DWaveTailInt2(xk, r: Float; VAR Wint: Float);
{ Calculates the normalization integral from r to infinity
  by brute force direct integration over the tail region }
 
CONST
   dx = 0.01;
   eps = 1e-6;
VAR
   x, v, w, wsq : Float;
   sumW1, sumW2 : Float;
BEGIN
   x := xk*r;
   AsympVandW(x, v, w);
   sumW1 := 0;  sumW2 := w*w/2;
   REPEAT
      x := x + dx;
      AsympVandW(x, v, w);
      sumW1 := sumW1 + w*w;
      x := x + dx;
      AsympVandW(x, v, w);
      wsq := w*w;
      sumW2 := sumW2 + wsq;
   UNTIL wsq <= eps*sumW2;
   Wint := (4*sumW1 + 2*sumW2 - wsq)*dx/(3*xk);
END;
 
FUNCTION DWaveTailInt(BE: Float; icalc: Integer; xv, wv: DVector): Float;
{ Purpose:  Use the known asymptotic form of the d-wave function at a
  given binding energy to obtain the contribution to the normalization
  integral for radii beyond that specified by icalc }
VAR
   xk, r, v, w, Wint: Float;
BEGIN
   xk := sqrt(muftr*BE);  r := xv.Value(icalc);
   { If exponential integral method works, use it
     Otherwise use brute force integration method }
   DWaveTailInt1(xk, r, Wint);
   IF Wint = 0 THEN DWaveTailInt2(xk, r, Wint);
   { Normalize to the actual asymptotic value }
   AsympVandW(xk*r, v, w);
   DWaveTailInt := sqr(wv.Value(icalc)/w)*Wint;
END;
 
PROCEDURE QpoleTailints1(xk, r: Float;
                         VAR Vr2Vint, Vr2Wint, Wr2Wint: Float);
{ Calculates BOTH the r^2 and quadrupole moment overlap integrals
  from r to infinity from approximate representations of exponential
  integrals }
VAR
   x, ftr : Float;
   alpha  : alphatype;
   capE   : capEtype;
BEGIN
   x := 2*xk*r;
   IF x < 5 THEN
      BEGIN
         ExponInts(x, alpha, capE);
         ftr := 1/(xk*r);
         Vr2Vint := r*sqr(r)*alpha[2];
         Vr2Wint := r*sqr(r)*(3*(alpha[0]*ftr + alpha[1])*ftr + alpha[2]);
         Wr2Wint := ((9*capE[2]*ftr + 18*capE[1])*ftr + 15*alpha[0])*ftr;
         Wr2Wint := r*sqr(r)*((Wr2Wint + 6*alpha[1])*ftr + alpha[2]);
      END
   ELSE
      BEGIN
         Vr2Vint := 0;  Vr2Wint := 0;  Wr2Wint := 0;
      END;
END;
 
PROCEDURE QpoleTailints2(xk, r: Float;
                         VAR Vr2Vint, Vr2Wint, Wr2Wint: Float);
{ Calculates BOTH the r^2 and quadrupole moment overlap integrals
  from r to infinity by direct integration over the tail region }
 
CONST
   dx = 0.01;
   eps = 1e-8;
VAR
   rp, drp, v, w          : Float;
   vr2v, vr2w, wr2w       : Float;
   sumVV1, sumVW1, sumWW1 : Float;
   sumVV2, sumVW2, sumWW2 : Float;
BEGIN
   drp := dx/xk;  rp := r;
   AsympVandW(xk*rp, v, w);
   sumVV1 := 0;            sumVW1 := 0;             sumWW1 := 0;
   sumVV2 := sqr(v*rp)/2;  sumVW2 := v*sqr(rp)*w/2; sumWW2 := sqr(w*rp)/2;
   REPEAT
      rp := rp + drp;
      AsympVandW(xk*rp, v, w);
      sumVV1 := sumVV1 + sqr(v*rp);
      sumVW1 := sumVW1 + v*sqr(rp)*w;
      sumWW1 := sumWW1 + sqr(w*rp);
      rp := rp + drp;
      AsympVandW(xk*rp, v, w);
      vr2v := sqr(v*rp);  vr2w := v*sqr(rp)*w;  wr2w := sqr(w*rp);
      sumVV2 := sumVV2 + vr2v;
      sumVW2 := sumVW2 + vr2w;
      sumWW2 := sumWW2 + wr2w;
   UNTIL (vr2v <= eps*sumVV2) AND (wr2w <= eps*sumWW2);
   Vr2Vint := (4*sumVV1 + 2*sumVV2 - vr2v)*drp/3;
   Vr2Wint := (4*sumVW1 + 2*sumVW2 - vr2w)*drp/3;
   Wr2Wint := (4*sumWW1 + 2*sumWW2 - wr2w)*drp/3;
END;
 
PROCEDURE QpoleTailInts(BE: Float; icalc: Integer; xv, uv, wv: DVector;
                        VAR Vr2Vint, Vr2Wint, Wr2Wint: Float);
{ Uses the known asymptotic form of the s- and d-wave functions to
  obtain the contributions to the r^2-weighted overlap integrals for
  radii beyond that specified by icalc }
VAR
   xk, r, v, w  : Float;
   coefv, coefw : Float;
BEGIN
   xk := sqrt(muftr*BE);  r := xv.Value(icalc);
   { If exponential integral method works, use it
     Otherwise use brute force integration method }
   QpoleTailints1(xk, r, Vr2Vint, Vr2Wint, Wr2Wint);
   IF (Vr2Vint = 0) AND (Wr2Wint = 0) THEN
      QpoleTailints2(xk, r, Vr2Vint, Vr2Wint, Wr2Wint);
   { Normalize to the actual asymptotic values }
   AsympVandW(xk*r, v, w);
   coefv := uv.Value(icalc)/v;  coefw := wv.Value(icalc)/w;
   Vr2Vint := sqr(coefv)*Vr2Vint;
   Vr2Wint := coefv*coefw*Vr2Wint;
   Wr2Wint := sqr(coefw)*Wr2Wint;
END;
 
PROCEDURE SDRsqInts(ibeg, iend: Integer; xv, uv, wv: DVector;
                    VAR Vr2Vint, Vr2Wint, Wr2Wint: Float);
{ Calculate the radial overlaps of r^2 between two radii.
  Note:  If  iend - ibeg  is not a positive, even integer, then
         the calculation fails and RsqInt is set to zero. }
VAR
   dr         : Float;
   i, idiff   : Integer;
   imax, ival : Integer;
   sumVV1, sumVV2 : Float;
   sumVW1, sumVW2 : Float;
   sumWW1, sumWW2 : Float;
BEGIN
   dr := xv.Value(2) - xv.Value(1);
   idiff := iend - ibeg;
   imax := round(idiff/2);
   IF idiff = 2*imax THEN
      BEGIN { Simpson's rule integration }
         sumVV1 := 0.0;  sumVV2 := 0.0;
         sumVW1 := 0.0;  sumVW2 := 0.0;
         sumWW1 := 0.0;  sumWW2 := 0.0;
         ival := ibeg;
         FOR i := 1 TO imax DO
            BEGIN
               ival := ival + 2;
               sumVV1 := sumVV1 + Sqr(xv.Value(ival-1)*uv.Value(ival-1));
               sumVW1 := sumVW1
               + uv.Value(ival-1)*Sqr(xv.Value(ival-1))*wv.Value(ival-1);
               sumWW1 := sumWW1 + Sqr(xv.Value(ival-1)*wv.Value(ival-1));
               sumVV2 := sumVV2 + Sqr(xv.Value(ival)*uv.Value(ival));
               sumVW2 := sumVW2
                 + uv.Value(ival)*Sqr(xv.Value(ival))*wv.Value(ival);
               sumWW2 := sumWW2 + Sqr(xv.Value(ival)*wv.Value(ival));
            END;
         sumVV1 := 4.0*sumVV1;  sumVV2 := 2.0*sumVV2;
         sumVW1 := 4.0*sumVW1;  sumVW2 := 2.0*sumVW2;
         sumWW1 := 4.0*sumWW1;  sumWW2 := 2.0*sumWW2;
         Vr2Vint := (Sqr(xv.Value(ibeg)*uv.Value(ibeg)) + sumVV1 + sumVV2
            - Sqr(xv.Value(iend)*uv.Value(iend)))*dr/3.0;
         Vr2Wint := (uv.Value(ibeg)*Sqr(xv.Value(ibeg))*wv.Value(ibeg)
            + sumVW1 + sumVW2
            - uv.Value(iend)*Sqr(xv.Value(iend))*wv.Value(iend))*dr/3.0;
         Wr2Wint := (Sqr(xv.Value(ibeg)*wv.Value(ibeg)) + sumWW1 + sumWW2
            - Sqr(xv.Value(iend)*wv.Value(iend)))*dr/3.0;
      END
   ELSE
      BEGIN
         Vr2Vint := 0;  Vr2Wint := 0;  Wr2Wint := 0;
      END;
END;
 
PROCEDURE SDWaveMEs(BE: Float; Nmax: Integer;
                    xv, uv, wv: DVector; VAR RsqME, QpoleME: Float);
{ Calculate the matrix elements of r^2 and Q(r) in an s- and d-wave
  coupled bound state.  (The factor of (1/2)^2 in RsqME accounts for
  the difference between the separation of the nucleons and their
  distance from the deuteron's center of mass) }
VAR
   Vr2Vint, Vr2Wint, Wr2Wint    : Float;
   Vr2Vtail, Vr2Wtail, Wr2Wtail : Float;
BEGIN
{ Set up the radial integrals }
   SDRsqInts(1, Nmax, xv, uv, wv, Vr2Vint, Vr2Wint, Wr2Wint);
   QpoleTailInts(BE, Nmax, xv, uv, wv, Vr2Vtail, Vr2Wtail, Wr2Wtail);
   Vr2Vint := Vr2Vint + Vr2Vtail;
   Vr2Wint := Vr2Wint + Vr2Wtail;
   Wr2Wint := Wr2Wint + Wr2Wtail;
{ Calculate the matrix elements }
   RsqME := 0.25*(Vr2Vint + Wr2Wint);
   QpoleME := Vr2Wint/sqrt(50) - Wr2Wint/20;
END;
 
FUNCTION ModScattRange(ScattLength, ScattRange, CoreRad: Float): Float;
{ Subtract the integral up to the core radius and renormalize }
VAR
 slope : Float;
BEGIN
   slope := -1/ScattLength;
   ModScattRange := sqr(1/(1 - CoreRad/ScattLength))
   *(ScattRange - 2*(LineInt(slope,1,CoreRad) - LineInt(slope,1,0)));
END;
 
PROCEDURE EstimateBE(ScattLength, ScattRange, CoreRad: Float;
                     VAR BE: Float);
{ Uses GLOBAL VARIABLE muftr }
VAR
   root, ModRange, ModLength : Float;
BEGIN
   IF CoreRad = 0 THEN
      BEGIN
         ModLength := ScattLength;
         ModRange := ScattRange;
      END
   ELSE
      BEGIN
         ModLength := ScattLength - CoreRad;
         ModRange := ModScattRange(ScattLength, ScattRange, CoreRad);
      END;
   IF (ModLength > 0) AND (ModLength > 2*ModRange) THEN
      BEGIN
         root := sqrt(modLength*(modLength - 2*ModRange));
         BE := 1/(muftr*sqr((ModLength + root)/2));
      END ELSE BE := 0;
END;


{ ------------------- Access to Tabulated Phase Shifts -------------------- }

PROCEDURE GetPhases(LabKE: Float; VAR PhasesFound: Boolean;
                    VAR Phases: PhaseType);
{ User supplies the kinetic energy in MeV.   GetPhases returns PhasesFound }
{ and Phases.   If PhasesFound is TRUE, then the Phases array holds inter- }
{ polated empirical phase shifts matching the requested kinetic energy.    }
{ Otherwise, the array is undefined.                                       }

VAR
   J, K, Lplus : Integer;
   x1, x2      : Float;
 
BEGIN
   IF NOT ( (LabKE < MinimumTabulatedLabKE) OR
            (LabKE > MaximumTabulatedLabKE) ) THEN
      BEGIN
         { Find the energy interval in the table of phase shifts }
         { into which the requested kinetic energy falls         }
         J := PhasesJmax;
         REPEAT
            J := J - 1;
         UNTIL LabKE >= PhaseArray[J].Energy;
         { Find weight factors based on a linear interpolation }
         { of the logarithm of the energies                    }
         x1 := (Ln(PhaseArray[J+1].Energy) - Ln(LabKE))/
               (Ln(PhaseArray[J+1].Energy) - Ln(PhaseArray[ J ].Energy));
         x2 := (Ln(LabKE) - Ln(PhaseArray[ J ].Energy))/
               (Ln(PhaseArray[J+1].Energy) - Ln(PhaseArray[ J ].Energy));
         { Divide out the 100 factor and convert to radians }
         x1 := x1 * pi / 18000;
         x2 := x2 * pi / 18000;
         { Interpolate the phase shifts using these weight factors }
         FOR Lplus := 1 TO PhasesLmax DO
            BEGIN
               FOR K := 1 TO PhasesKmax DO
               Phases[Lplus,K] := x1*PhaseArray[ J ].Phase[Lplus,K] +
                                  x2*PhaseArray[J+1].Phase[Lplus,K];
            END;
         FOR Lplus := PhasesLmax + 1 TO PhasesLbig DO
            BEGIN
               FOR K := 1 TO PhasesKmax DO
               Phases[Lplus,K] := 0;
            END;
         PhasesFound := TRUE;
      END
   ELSE PhasesFound := FALSE;
END;

{ ----------------------- Phase Shift Calculations ------------------------ }

FUNCTION Phase(ysave: DVector; L: Integer): Float;
{ Uses GLOBAL VARIABLES: F[L], G[L], FP[L], GP[L] }
VAR
   Value, Deriv : Float;
BEGIN
   Value := ysave.Value(1);
   Deriv := ysave.Value(2);
   Phase := Atan2(G[L]*Deriv  -  Value*GP[L], Value*FP[L] -  F[L]*Deriv);
END;


PROCEDURE FindKay(ysave: DVarray; J: Integer; VAR Kv, Kw, Kc: FLOAT);
{ Find the desired K-matrix solutions as linear combinations of two
  independent solutions of the coupled equations.   Extract the phase
  shifts from the resulting K-matrix. }
CONST
   n = 4;
VAR
   amat       : DMatrix;
   vtr1, vtr2 : DVector;
   indx       : IDVector;
   i          : Integer;
   d          : Real;

BEGIN
   Request1DMatrix(amat, n, n);
   Request2DVectors(vtr1, vtr2, n);
   Request1IDVector(indx, n);
   WITH amat DO
      BEGIN
         FOR i := 1 TO n DO
            BEGIN
               Put(i, 1, ysave[1].Value(i));
               Put(i, 2, ysave[2].Value(i));
            END;
         Put(1, 3, -G[J-1]);
         Put(2, 3, -GP[J-1]);
         Put(3, 4, -G[J+1]);
         Put(4, 4, -GP[J+1]);
      END; { WITH amat }
   LUdecomp(amat, d, indx);

   { Find solution that has an F-wave in the v channel }
   vtr1.Put(1, F[J-1]);
   vtr1.Put(2, FP[J-1]);
   LUsolve(amat, vtr1, indx);
   Kv := vtr1.Value(3);
   Kc := vtr1.Value(4);

   { Find solution that has an F-wave in the w channel }
   vtr2.Put(3, F[J+1]);
   vtr2.Put(4, FP[J+1]);
   LUsolve(amat, vtr2, indx);
   Kc := vtr2.Value(3);
   Kw := vtr2.Value(4);

   Release1IDVector(indx);
   Release2DVectors(vtr1, vtr2);
   Release1DMatrix(amat);
END;

 
PROCEDURE MakePhases(Kv,Kw,Kc: FLOAT; VAR dv,dw,eps: FLOAT);
{ Calculate the phase shifts from K-matrix elements
  for a two-channels coupled channels problem }
VAR
   deltsum, deltdif : FLOAT;
BEGIN
   deltsum := Atan2(1 - Kv*Kw + Kc*Kc,Kv + Kw);
   deltdif := Atan2(1 + Kv*Kw - Kc*Kc,Kv - Kw);
   dv  := 0.5*(deltsum + deltdif);
   dw  := 0.5*(deltsum - deltdif);
   eps := 0.5*Atan2(SQRT(SQR(1 + Kv*Kw - Kc*Kc) + SQR(Kv - Kw)),2*Kc);
END;
 
PROCEDURE SingletPhase(cmEgy: Float; xv, yv: Dvector; Nmax: Integer;
                       VAR CalcPhase: Float);
VAR
   L : Integer;
   ysave  : DVector;
BEGIN
   Request1DVector(ysave, 2);
   WITH ThisPotl DO
      BEGIN { Singlet }
         L := CapJ;
         IF Even(L) THEN SingletPotl:=SE_CONSTS
                    ELSE SingletPotl:=SO_CONSTS;
         IF Even(L) THEN SetOPEPStrengths(SEvn, VcOPEP, VtOPEP)
                    ELSE SetOPEPStrengths(SOdd, VcOPEP, VtOPEP);
         Wavefunction(cmEgy, 1, Nmax, Nmax, DerivsSinglet, xv, yv, ysave);
         CalcPhase := Phase(ysave, L);
      END;  { Singlet }
   Release1DVector(ysave);
END;

PROCEDURE DiagTripletPhase(cmEgy: Float; xv, yv: Dvector; Nmax: Integer;
                           VAR CalcPhase: Float);
VAR
   L : Integer;
   ysave  : DVector;
BEGIN
   Request1DVector(ysave, 2);
   WITH ThisPotl DO
      BEGIN { Triplet, L = J }
         L := CapJ;
         IF Even(L) THEN TripletPotl := TE_CONSTS
                    ELSE TripletPotl := TO_CONSTS;
         IF Even(L) THEN SetOPEPStrengths(TEvn, VcOPEP, VtOPEP)
                    ELSE SetOPEPStrengths(TOdd, VcOPEP, VtOPEP);
         Wavefunction(cmEgy, 1, Nmax, Nmax, DerivsTriplet, xv, yv, ysave);
         CalcPhase := Phase(ysave, L);
      END;  { Triplet, L = J }
   Release1DVector(ysave);
END;

PROCEDURE TripletP0Phase(cmEgy: Float; xv, yv: Dvector; Nmax: Integer;
                       VAR CalcPhase: Float);
VAR
   L : Integer;
   ysave  : DVector;
BEGIN
   Request1DVector(ysave, 2);
   WITH ThisPotl DO
      BEGIN { 3P0 }
         L := 1;
         TripletPotl := TO_CONSTS;
         SetOPEPStrengths(TOdd, VcOPEP, VtOPEP);
         Wavefunction(cmEgy, 1, Nmax, Nmax, Derivs3P0, xv, yv, ysave);
         CalcPhase := Phase(ysave, L);
      END;  { 3P0 }
   Release1DVector(ysave);
END;

PROCEDURE CoupledPhase(cmEgy: Float; xv, uv, wv: Dvector; Nmax: Integer;
                       VAR vPhase, wPhase, couple: Float);
VAR
   L, itype : Integer;
   ysave  : DVArray;
   Kv, Kw, Kc : Float;
BEGIN
   Request2DVectors(ysave[1], ysave[2], 4);
{ Triplet,  L = J + 1 and L = J - 1 }
   L := CapJ - 1;
   IF Even(L) THEN TripletPotl := ThisPotl.TE_CONSTS
              ELSE TripletPotl := ThisPotl.TO_CONSTS;
   IF Even(L) THEN SetOPEPStrengths(TEvn, VcOPEP, VtOPEP)
              ELSE SetOPEPStrengths(TOdd, VcOPEP, VtOPEP);
   { Create two linearly independent solutions }
   FOR itype := 1 TO 2 DO CoupWavefunction(cmEgy, 1, Nmax, Nmax,
                          itype, DerivsCoup, xv, uv, wv, ysave[itype]);
   { Find K-matrix elements and then the nuclear bar phases }
   FindKay(ysave, CapJ, Kv, Kw, Kc);
   MakePhases(Kv, Kw, Kc, vPhase, wPhase, couple);
   Release2DVectors(ysave[1], ysave[2]);
END;

PROCEDURE NuclearBarPhases(cmEgy: Float; Npoints, Nmax, Nmatch,
                  Jmax: Integer; xv: DVector; VAR phases: PhaseType);
{ Uses GLOBAL VARIABLES: CapJ, muftr, MySystem }

{
             Storage pattern for nucleon-nucleon phases

             |    1         2         3         4         5
             |
       ------|------------------------------------------------
             |
        1    |   3S1        -         -        1S0        -
        2    |   3P2       3P1       3P0       1P1       Eps1
        3    |   3D3       3D2       3D1       1D2       Eps2
        4    |   3F4       3F3       3F2       1F3       Eps3
        5    |   3G5       3G4       3G3       1G4       Eps4
        6    |   3H6       3H5       3H4       1H5       Eps5
        ......................  etc.  .......................
}
VAR
   CapJsave : Integer;
   L, J     : Integer;
   uv, wv   : DVector;
   xk, eta  : Float;
   WaitMessage : TWaitMessage;
BEGIN
   CapJsave := CapJ;
   FOR L := 1 TO PhasesLbig DO
   FOR J := 1 TO PhasesKmax DO
   Phases[L,J] := 0;

   xk := SQRT(muftr*cmEgy);
   IF MySystem = 1 THEN
      eta := esq*muftr/(2.0*xk)
   ELSE eta := 0;

   CoulombFunctions( {Input} eta, xk, xv.Value(Nmax), Jmax+1,
                     {Output} CoulSig, F, G, Fp, Gp);
 
   Request2DVectors(uv, wv, Npoints);
   WaitMessage.Show;

   CapJ := 0;
   Print(9,6,Concat('Calculating J =', NumStr(CapJ,3,0), ' phases...'));
   SingletPhase(cmEgy, xv, uv, Nmax, Phases[1,4]);
   TripletP0Phase(cmEgy, xv, uv, Nmax, Phases[2,3]);

   FOR J := 1 TO Jmax DO
      BEGIN
         WaitMessage.Update;
         CapJ := J;
         IF J <> 5 THEN
            Print(9,J+6,Concat(
               'Calculating J =', NumStr(CapJ,3,0), ' phases...'))
         ELSE
            Print(9,J+6,'Calculating J');
         SingletPhase(cmEgy, xv, uv, Nmax, Phases[J+1,4]);
         DiagTripletPhase(cmEgy, xv, uv, Nmax, Phases[J+1,2]);
         WaitMessage.Update;
         CoupledPhase(cmEgy, xv, uv, wv, Nmax,
                      Phases[J,1], Phases[J+2,3], Phases[J+1,5]);
      END;

   WaitMessage.Hide;
   Release2DVectors(uv, wv);
   CapJ := CapJsave;
END;

PROCEDURE SingletPhaseHJ(cmEgy: Float; xv, yv: Dvector; Nmax: Integer;
                         VAR CalcPhase: Float);
VAR
   L : Integer;
   ysave  : DVector;
BEGIN
   Request1DVector(ysave, 2);
   L := CapJ;
   IF Even(L) THEN PotlHJ := SE_CONSTS
              ELSE PotlHJ := SO_CONSTS;
   Wavefunction(cmEgy, 1, Nmax, Nmax, DerivsSingletHJ, xv, yv, ysave);
   CalcPhase := Phase(ysave, L);
   Release1DVector(ysave);
END;

PROCEDURE DiagTripletPhaseHJ(cmEgy: Float; xv, yv: Dvector; Nmax: Integer;
                             VAR CalcPhase: Float);
VAR
   L : Integer;
   ysave  : DVector;
BEGIN
   Request1DVector(ysave, 2);
   L := CapJ;
   IF Even(L) THEN PotlHJ := TE_CONSTS
              ELSE PotlHJ := TO_CONSTS;
   Wavefunction(cmEgy, 1, Nmax, Nmax, DerivsTripletHJ, xv, yv, ysave);
   CalcPhase := Phase(ysave, L);
   Release1DVector(ysave);
END;

PROCEDURE TripletP0PhaseHJ(cmEgy: Float; xv, yv: Dvector; Nmax: Integer;
                       VAR CalcPhase: Float);
VAR
   L : Integer;
   ysave  : DVector;
BEGIN
   Request1DVector(ysave, 2);
   L := 1;
   PotlHJ := TO_CONSTS;
   Wavefunction(cmEgy, 1, Nmax, Nmax, Derivs3P0HJ, xv, yv, ysave);
   CalcPhase := Phase(ysave, L);
   Release1DVector(ysave);
END;

PROCEDURE CoupledPhaseHJ(cmEgy: Float; xv, uv, wv: Dvector; Nmax: Integer;
                         VAR vPhase, wPhase, couple: Float);
VAR
   L, itype : Integer;
   ysave  : DVArray;
   Kv, Kw, Kc : Float;
BEGIN
   Request2DVectors(ysave[1], ysave[2], 4);
   L := CapJ - 1;
   IF Even(L) THEN PotlHJ := TE_CONSTS
              ELSE PotlHJ := TO_CONSTS;
   { Create two linearly independent solutions }
   FOR itype := 1 TO 2 DO CoupWavefunction(cmEgy, 1, Nmax, Nmax,
                          itype, DerivsCoupHJ, xv, uv, wv, ysave[itype]);
   { Find K-matrix elements and then the nuclear bar phases }
   FindKay(ysave, CapJ, Kv, Kw, Kc);
   MakePhases(Kv, Kw, Kc, vPhase, wPhase, couple);
   Release2DVectors(ysave[1], ysave[2]);
END;

PROCEDURE NuclearBarPhasesHJ(cmEgy: Float; Npoints, Nmax, Nmatch,
                  Jmax: Integer; xv: DVector; VAR phases: PhaseType);
{ USES GLOBAL VARIABLE: CapJ, muftr, MySystem }

{
             Storage pattern for nucleon-nucleon phases

             |    1         2         3         4         5
             |
       ------|------------------------------------------------
             |
        1    |   3S1        -         -        1S0        -
        2    |   3P2       3P1       3P0       1P1       Eps1
        3    |   3D3       3D2       3D1       1D2       Eps2
        4    |   3F4       3F3       3F2       1F3       Eps3
        5    |   3G5       3G4       3G3       1G4       Eps4
        6    |   3H6       3H5       3H4       1H5       Eps5
        ......................  etc.  .......................
}
VAR
   CapJsave : Integer;
   L, J     : Integer;
   uv, wv   : DVector;
   xk, eta  : Float;
   WaitMessage : TWaitMessage;
BEGIN
   CapJsave := CapJ;
   FOR L := 1 TO PhasesLbig DO
   FOR J := 1 TO PhasesKmax DO
   Phases[L,J] := 0;

   xk := SQRT(muftr*cmEgy);
   IF MySystem = 1 THEN
      eta := esq*muftr/(2.0*xk)
   ELSE eta := 0;

   CoulombFunctions( {Input} eta, xk, xv.Value(Nmax), Jmax+1,
                     {Output} CoulSig, F, G, Fp, Gp);
 
   Request2DVectors(uv, wv, Npoints);
   WaitMessage.Show;

   CapJ := 0;
   Print(9,6,Concat('Calculating J =', NumStr(CapJ,3,0), ' phases...'));
   SingletPhaseHJ(cmEgy, xv, uv, Nmax, Phases[1,4]);
   TripletP0PhaseHJ(cmEgy, xv, uv, Nmax, Phases[2,3]);

   FOR J := 1 TO Jmax DO
      BEGIN
         WaitMessage.Update;
         CapJ := J;
         IF J <> 5 THEN
            Print(9,J+6,Concat(
               'Calculating J =', NumStr(CapJ,3,0), ' phases...'))
         ELSE
            Print(9,J+6,'Calculating J');
         SingletPhaseHJ(cmEgy, xv, uv, Nmax, Phases[J+1,4]);
         DiagTripletPhaseHJ(cmEgy, xv, uv, Nmax, Phases[J+1,2]);
         WaitMessage.Update;
         CoupledPhaseHJ(cmEgy, xv, uv, wv, Nmax,
                      Phases[J,1], Phases[J+2,3], Phases[J+1,5]);
      END;

   WaitMessage.Hide;
   Release2DVectors(uv, wv);
   CapJ := CapJsave;
END;

{ ------------------- Scattering Amplitude Calculations ------------------- }

PROCEDURE ShowAmps(theta: Float; a, b, c, e, f, g: Complex);
CONST
   xstrt = 17;
   ystrt = 4;
BEGIN
   ClearMUPPETport;
   DefineViewPort(1, 0.10,0.9,  0.2,0.85);
   OpenViewPort(1);
   Print(xstrt+7,ystrt,Concat('Theta = ', Numstr(theta,6,2)));
   Print(xstrt,ystrt+2,Concat(
      'a = ', Numstr(a.re,12,6), '  ', Numstr(a.im,12,6)));
   Print(xstrt,ystrt+3,Concat(
      'b = ', Numstr(b.re,12,6), '  ', Numstr(b.im,12,6)));
   Print(xstrt,ystrt+4,Concat(
      'c = ', Numstr(c.re,12,6), '  ', Numstr(c.im,12,6)));
   Print(xstrt,ystrt+5,Concat(
      'e = ', Numstr(e.re,12,6), '  ', Numstr(e.im,12,6)));
   Print(xstrt,ystrt+6,Concat(
      'f = ', Numstr(f.re,12,6), '  ', Numstr(f.im,12,6)));
   Print(xstrt,ystrt+7,Concat(
      'g = ', Numstr(g.re,12,6), '  ', Numstr(g.im,12,6)));
   Pause;
END;

PROCEDURE Alphacalc(Phases: PhaseType; LmaxP:Integer);
{ Calculate nucleon-nucleon alphas from phase shifts.
  Input:  nucleon-nucleon phases (in phases array)
  Output: alpha's (in cxamp array) }

{                Storage pattern for nucleon-nucleon alpha's                }
{           ( Outgoing channel on the left, ingoing on the right )          }
{                                                                           }
{           |    1         2         3         4         5         6        }
{           |                                                               }
{     ------|---------------------------------------------------------      }
{           |                                                               }
{      1    | 3S1-3S1      -         -      3S1-3D1      -        1S0       }
{      2    | 3P2-3P2   3P1-3P1   3P0-3P0   3P2-3F2      -        1P1       }
{      3    | 3D3-3D3   3D2-3D2   3D1-3D1   3D3-3G3   3D1-3S1     1D2       }
{      4    | 3F4-3F4   3F3-3F3   3F2-3F2   3F4-3H4   3F2-3P2     1F3       }
{      5    | 3G5-3G5   3G4-3G4   3G3-3G3   3G5-3I5   3G3-3D3     1G4       }
{      6    | 3H6-3H6   3H5-3H5   3H4-3H4   3H6-3J6   3H4-3F4     1H5       }
{           |                                                               }
{      ---------------------------------------------------------------      }

VAR
lp : Integer;
 
   PROCEDURE AlphaDiag(delta, epsJ: Float; VAR alpha: Complex);
   VAR
      ftr : Float;
   BEGIN
      ftr := 0.5*cos(2*epsJ);
      alpha.re := +ftr*sin(2*delta);
      alpha.im := 0.5 - ftr*cos(2*delta);
   END;
 
   PROCEDURE AlphaCoupl(sumdeltas, epsJ: Float; VAR alpha: Complex);
   VAR
      ftr : Float;
   BEGIN
      ftr := -0.5*sin(2*epsJ);
      alpha.re := ftr*cos(sumdeltas);
      alpha.im := ftr*sin(sumdeltas);
   END;
 
BEGIN
   { Alpha's for l = 0 }
   AlphaDiag(phases[1,1], phases[2,5], cxamp[1,1]);
   cxamp[1,2] := zero;
   cxamp[1,3] := zero;
   AlphaCoupl(phases[1,1] + phases[3,3], phases[2,5], cxamp[1,4]);
   AlphaDiag(phases[1,4],0.0, cxamp[1,6]);
   { Alpha's for l > 0 }
   FOR lp := 2 TO LmaxP DO
      BEGIN          { Triplet - Diagonal terms }
   AlphaDiag(phases[lp,1], phases[lp+1,5], cxamp[lp,1]);  { J = L + 1 }
   AlphaDiag(phases[lp,2],      0.0      , cxamp[lp,2]);  { J =   L   }
   AlphaDiag(phases[lp,3], phases[lp-1,5], cxamp[lp,3]);  { J = L - 1 }
                     { Triplet - Coupling terms }
   AlphaCoupl(phases[lp,1] + phases[lp+2,3], phases[lp+1,5], cxamp[lp,4]);
                          { Singlet - term }
   AlphaDiag(phases[lp,4],     0.0     , cxamp[lp,6]);
      END;
   { Second set of triplet coupling terms }
   cxamp[1,5] := zero;
   cxamp[2,5] := zero;
   FOR lp := 3 TO LmaxP DO cxamp[lp,5] := cxamp[lp-2,4];
END;

PROCEDURE IncludeCoulPhases(eta:Float; LmaxP:Integer);
{ Include Coulomb phases in the nucleon-nucleon alpha's.
  Input:   original alpha's (in cxamp array)
  Output:  modified alpha's (in cxamp array) }

VAR
   lp, i         : Integer;
   omegal        : Float;
   cxftr, cxftr2 : Complex;
   Coulphaseftr  : CXVector;
 
BEGIN
   { Set up the Coulomb phase factors }
   Request1CXVector(Coulphaseftr, LmaxP + 2);
   omegal := 0;
   Coulphaseftr.RealVec.Put(1, 1);
   Coulphaseftr.ImagVec.Put(1, 0);
   FOR lp := 2 TO LmaxP + 2 DO
      BEGIN
         omegal := omegal + arctan(eta/(lp - 1));
         cxftr.re := cos(omegal);
         cxftr.im := sin(omegal);
         Coulphaseftr.RealVec.Put(lp,cxftr.re);
         Coulphaseftr.ImagVec.Put(lp,cxftr.im);
      END;
   { Perform the multiplications }
   { Alpha's for l = 0 }
   Coulphaseftr.RealVec.Get(3,cxftr2.re);
   Coulphaseftr.ImagVec.Get(3,cxftr2.im);
   CxOp(cxftr2,'*',cxamp[1,4],cxamp[1,4]);
   { Alpha's for l > 0 }
   FOR lp := 2 TO LmaxP DO
      BEGIN
         Coulphaseftr.RealVec.Get(lp,cxftr.re);
         Coulphaseftr.ImagVec.Get(lp,cxftr.im);
         CxOp(cxftr,'*',cxftr,cxftr2);
         FOR i := 1 TO 3 DO
            CxOp(cxftr2,'*',cxamp[lp,i],cxamp[lp,i]);
         CxOp(cxftr2,'*',cxamp[lp,6],cxamp[lp,6]);
         Coulphaseftr.RealVec.Get(lp+2,cxftr2.re);
         Coulphaseftr.ImagVec.Get(lp+2,cxftr2.im);
         CxOp(cxftr,'*',cxamp[lp,4],cxamp[lp,4]);
         CxOp(cxftr2,'*',cxamp[lp,4],cxamp[lp,4]);
      END;
   FOR lp := 3 TO LmaxP DO cxamp[lp,5] := cxamp[lp-2,4];
   Release1CXVector(Coulphaseftr);
END;


PROCEDURE Coeff(wk:Float; LmaxP, select:integer);
{ Calculate coefficients of the Legendre expansion
     of the scattering amplitudes.
  Input:   alphas (in cxamp array).
  Output:  Legendre coefficients (in cxamp array) }
VAR
   Amps            : ARRAY[1..6] of complex;
   rthalf,ftr      : Float;
   i,xlm,xl,xlp,lp : Integer;
 
   PROCEDURE DoSum(lp: Integer; ftr, B1, B2, B3, B4, B5: Float;
                   VAR Amp: Complex);
   BEGIN
      Amp.Re := B1*CxAmp[lp,1].Re + B2*CxAmp[lp,2].Re
              + B3*CxAmp[lp,3].Re + B4*CxAmp[lp,4].Re + B5*CxAmp[lp,5].Re;
      Amp.Im := B1*CxAmp[lp,1].Im + B2*CxAmp[lp,2].Im
              + B3*CxAmp[lp,3].Im + B4*CxAmp[lp,4].Im + B5*CxAmp[lp,5].Im;
      Amp.Re := ftr*Amp.Re;
      Amp.Im := ftr*Amp.Im;
   END;
 
BEGIN
   rthalf:=sqrt(0.5);
   FOR i:=1 TO 6 DO Amps[i]:=zero;
 
   {** Lprime = 0 **}
   DoSum(1, 1.0, 1.0, 0.0, 0.0, rthalf, 0.0, Amps[1]);
   DoSum(1, 1.0, 1.0, 0.0, 0.0, -1/rthalf, 0.0, Amps[5]);
   Amps[6] := CxAmp[1,6]; { Singlet }
   FOR i:=1 TO 6 DO CxAmp[1,i]:=Amps[i];
 
   {** Lprime = 1 **}
   DoSum(2, 1.0, 1.5, 1.5, 0.0, sqrt(6)/2, 0.0, Amps[1]);
   DoSum(2, 1.0, rthalf, 0.0, -rthalf, -sqrt(0.75), 0.0, Amps[2]);
   DoSum(2, 1.0, -1.5*rthalf, 1.5*rthalf, 0.0, -sqrt(0.75), 0.0, Amps[4]);
   DoSum(2, 1.0, 2.0, 0.0, 1.0, -sqrt(6.0), 0.0, Amps[5]);
   rcprod(3, CxAmp[2,6], Amps[6]); { Singlet }
   FOR i:=1 TO 6 DO CxAmp[2,i]:=Amps[i];
 
   {** Lprime = 2 TO Lmax **}
   xl:=1;
   FOR lp:=3 TO LmaxP DO
      BEGIN
         xl:=xl+1;  xlm := xl-1;  xlp := xl+1;
         ftr := 1/(xl*xlp);
         DoSum(lp, 0.5, xl+2, xl+xlp, xlm,
            sqrt((xlp)*(xlp+1)), sqrt(xlm*xl), Amps[1]);
         DoSum(lp, rthalf, 1.0, 0.0, -1.0,
            -sqrt((xlp+1)/(xlp)), sqrt(xlm/xl), Amps[2]);
         DoSum(lp, 0.5*ftr, xl, -(xl+xlp), xlp,
            xl*sqrt(xlp/(xlp+1)), xlp*sqrt(xl/xlm) , Amps[3]);
         DoSum(lp, rthalf, -(xlp+1)/xlp, (xl+xlp)*ftr, xlm/xl,
            -sqrt((xlp+1)/xlp), sqrt(xlm/xl), Amps[4]);
         DoSum(lp, 1.0, xlp, 0.0, xl,
            -sqrt((xlp)*(xlp+1)), -sqrt(xlm*xl), Amps[5]);
         rcprod(2*xl+1, CxAmp[lp,6], Amps[6]);
         FOR i:=1 TO 6 DO CxAmp[lp,i] := Amps[i];
      END;
END;


PROCEDURE PutWolfAmps(theta: Float; a, b, c, e, f: Complex; i: Integer);
BEGIN
   thetav.Put(i,theta);
   av.RealVec.Put(i,a.re);
   bv.RealVec.Put(i,b.re);
   cv.RealVec.Put(i,c.re);
   ev.RealVec.Put(i,e.re);
   fv.RealVec.Put(i,f.re);
   av.ImagVec.Put(i,a.im);
   bv.ImagVec.Put(i,b.im);
   cv.ImagVec.Put(i,c.im);
   ev.ImagVec.Put(i,e.im);
   fv.ImagVec.Put(i,f.im);
END;


PROCEDURE GetWolfAmps(VAR theta: Real;
                      VAR a, b, c, e, f: Complex; i: Integer);
BEGIN
   thetav.Get(i,theta);
   av.RealVec.Get(i,a.re);
   bv.RealVec.Get(i,b.re);
   cv.RealVec.Get(i,c.re);
   ev.RealVec.Get(i,e.re);
   fv.RealVec.Get(i,f.re);
   av.ImagVec.Get(i,a.im);
   bv.ImagVec.Get(i,b.im);
   cv.ImagVec.Get(i,c.im);
   ev.ImagVec.Get(i,e.im);
   fv.ImagVec.Get(i,f.im);
END;

PROCEDURE MakeWolf(Theta: Float; Amps: CXarray; i: Integer);
{ Form Wolfenstein scatt amps.
  Input:   Triplet and singlet scatt amps (in Amps array)
  Output:  Wolfenstein amps (in av..fv CXVector arrays) }
 
   PROCEDURE DoSum(ftr1, ftr2, ftr3: Float;
                   amp1, amp2, amp3: Complex; VAR Amp: Complex);
   BEGIN
      Amp.Re := ftr1*amp1.Re + ftr2*amp2.Re + ftr3*amp3.Re;
      Amp.Im := ftr1*amp1.Im + ftr2*amp2.Im + ftr3*amp3.Im;
   END;
 
VAR
   ftr, csq, ssq    : Float;
   arg, sintheta    : Float;
   a, b, c, e, f, g : Complex;
   ebar, fbar, gbar : Complex;
 
BEGIN
   ftr := 1/sqrt(8);
   { Form coefficient amplitudes in the Madison convention frame }
   DoSum(0.50,  0.25,  0.25, Amps[1], Amps[5], Amps[6], a);
   DoSum(0.25, -0.25, -0.50, Amps[5], Amps[6], Amps[3], b);
   DoSum( ftr,  -ftr,  0.00, Amps[2], Amps[4],   zero , c);
   { Mult by i } arg := c.re;  c.re := -c.im;  c.im := arg;
   DoSum(0.25, -0.25,  0.50, Amps[5], Amps[6], Amps[3], ebar);
   DoSum(0.50, -0.25, -0.25, Amps[1], Amps[5], Amps[6], fbar);
   DoSum( ftr,   ftr,  0.00, Amps[2], Amps[4],   zero , gbar);
   { Form coefficient amplitudes in rotated reference frame }
   arg := pi*Theta/180;
   csq := cos(0.5*arg);
   ssq := sin(0.5*arg);
   sintheta := 2*csq*ssq;
   csq := sqr(csq);
   ssq := sqr(ssq);
   DoSum(csq,  ssq, -sintheta, ebar, fbar, gbar, e);
   DoSum(ssq,  csq,  sintheta, ebar, fbar, gbar, f);
   { Save the resulting Wolfenstein amplitudes }
   PutWolfAmps(theta, a, b, c, e, f, i);
   { The following amplitude should vanish! }
   ftr := 0.5*sintheta;
   DoSum(-ftr, ftr, ssq - csq, ebar, fbar, gbar, g);
   { De-comment the following statement to see the Wolfenstein amps }
   { ShowAmps(theta, a, b, c, e, f, g); }
END;


PROCEDURE ScattAmp(LabKE, eta, Thetmin, dThet: Float;
                   LmaxP, numang, select: Integer);
{ Form singlet and triplet scatt amps.
  Call MakeWolf to calculate and save Wolfenstein amplitudes.
  Input:  Modified alpha's (in cxamp array)
  Output: Saved Wolfenstein amps (in av..fv CXVector arrays)
  Storage pattern for triplet and singlet amplitudes:
  M(1,1), M(1,0), M(1,-1), M(0,1) M(0,0) in Amps[1..5].
  Singlet amplitude in Amps[6] }
TYPE
   evenodd = (even, odd);
VAR
   parity            : evenodd;
   Amps              : ARRAY[1..6,evenodd] OF Complex;
   plm               : ARRAY[1..6] of Float;
   Mamps             : CXArray;
   acoul, cxtem      : Complex;
   Theta, arg, arg2  : Float;
   eps, x, y, p0, p1 : Float;
   wk, OneOverK, ftr : Float;
   i, j, xl, lp      : Integer;
   TripFtr, SingFtr  : Integer;
   Exchng, NoExch    : Integer;
   SpinSpace         : Integer;
 
BEGIN
   wk := sqrt(muftr*LabKE/2); { NOTE: conversion to cm energy! }
   eps := 0.001;
   OneOverK := 1/wk;
   Theta := Thetmin - dThet;
FOR j := 1 TO numang DO
   BEGIN { Loop over angles }
 
      Theta := Theta + dThet;
 
      { Amplitude contributions for Lprime = 0 }
      Amps[1,even] := CxAmp[1,1];
      FOR i := 2 TO 4 DO Amps[i,even] := zero;
      Amps[5,even] := CxAmp[1,5];
      Amps[6,even] := CxAmp[1,6];
 
      { Legendre Polynomials for Lprime = 1 }
      arg := pi*Theta/180;
      x := cos(arg);
      y := sin(arg);
      p0 := 1;
      p1 := x;
      plm[2] := y;
      plm[3] := 0;
 
      { Amplitude contributions for Lprime = 1 }
      rcprod(  p1  ,CxAmp[2,1],Amps[1,odd]);
      rcprod(plm[2],CxAmp[2,2],Amps[2,odd]);
      Amps[3,odd] := zero;
      rcprod(plm[2],CxAmp[2,4],Amps[4,odd]);
      rcprod(  p1  ,CxAmp[2,5],Amps[5,odd]);
      rcprod(  p1  ,CxAmp[2,6],Amps[6,odd]);
 
      xl := 1;
      FOR lp := 3 TO LmaxP DO
         BEGIN
 
            { Legendre Polynomials for Lprime >= 2 }
            xl := xl + 1;
            plm[1] := ((2*xl-1)*x*p1 - (xl-1)*p0)/xl;
            plm[3] := x*plm[3] + (xl+1)*y*plm[2];
            plm[2] := x*plm[2] + xl*y*p1;
            plm[4] := plm[2];
            plm[5] := plm[1];
            plm[6] := plm[1];
            p0:=p1;
            p1:=plm[1];
 
            IF lp MOD 2 = 1 THEN parity := even
               ELSE parity := odd;
 
            { Amplitude contributions for Lprime >= 2 }
            FOR i := 1 TO 6 DO
               BEGIN
                  rcprod(plm[i],CxAmp[lp,i],cxtem);
                  CxOp(cxtem,'+',Amps[i,parity],Amps[i,parity]);
               END;
         END;  { Loop over lp }
 
      IF Select = 1 THEN
         BEGIN { Add direct and exchange Coulomb amplitudes }
             IF arg < eps THEN
                BEGIN
                   acoul.re := Large;
                   acoul.im := 0;
                END ELSE
                BEGIN
                   ftr := -0.25*eta/sqr(sin(0.5*arg));
                   acoul.re := ftr*cos(2*eta*ln(sin(0.5*arg)));
                   acoul.im :=-ftr*sin(2*eta*ln(sin(0.5*arg)));
                END;
             CxOp(Amps[1, odd],'+',acoul,Amps[1, odd]);
             CxOp(Amps[5, odd],'+',acoul,Amps[5, odd]);
             CxOp(Amps[6,even],'+',acoul,Amps[6,even]);
 
             arg2 := pi - arg;
             IF arg2 < eps THEN
                BEGIN
                   acoul.re := Large;
                   acoul.im := 0;
                END ELSE
                BEGIN
                   ftr := -0.25*eta/sqr(sin(0.5*arg2));
                   acoul.re := ftr*cos(2*eta*ln(sin(0.5*arg2)));
                   acoul.im :=-ftr*sin(2*eta*ln(sin(0.5*arg2)));
                END;
             CxOp(Amps[1, odd],'-',acoul,Amps[1, odd]);
             CxOp(Amps[5, odd],'-',acoul,Amps[5, odd]);
             CxOp(Amps[6,even],'+',acoul,Amps[6,even]);
         END;  { Add direct and exchange Coulomb amplitudes }
 
      { Combine even and odd contributions and divide by wavenumber }
      NoExch := 1; { NoExch term present for all but n-p exchange scatt }
      Exchng := 1; { Exchng term present for all but n-p direct scatt   }
      IF Select = 2 THEN Exchng := 0;
      IF Select = 3 THEN NoExch := 0;
      { Triplet Even and Singlet Odd have spin*space exchng = +1 }
      SpinSpace := +1;
      ftr := OneOverK*(NoExch - Exchng*SpinSpace);
      FOR i := 1 TO 5 DO
         rcprod(ftr,Amps[i,even],Amps[i,even]);
      rcprod(ftr,Amps[6, odd],Amps[6, odd]);
      { Singlet Even and Triplet Odd have spin*space exchng = -1 }
      SpinSpace := -1;
      ftr := OneOverK*(NoExch - Exchng*SpinSpace);
      FOR i := 1 TO 5 DO
         rcprod(ftr,Amps[i, odd],Amps[i, odd]);
      rcprod(ftr,Amps[6,even],Amps[6,even]);
      FOR i := 1 TO 6 DO CxOp(Amps[i,even],'+',Amps[i,odd],Mamps[i]);
 
      { Calculate and save the Wolfenstein amplitudes }
      MakeWolf(Theta, Mamps, j);
   END;  { Loop over angles }
END;


PROCEDURE MakeScattAmps(Phases: PhaseType;
                        Thetmin, Thetmax: Float; numang: Integer);
VAR
   wk, eta, dThet : Float;
BEGIN
   alphacalc(Phases, LmaxP);
   wk := sqrt(muftr*0.5*LabKE);
   IF MySystem = 1 THEN
      BEGIN
         eta := esq*muftr/(2.0*wk);
         IncludeCoulPhases(eta, LmaxP);
      END ELSE eta := 0;
   Coeff(wk, LmaxP, MySystem);
   dThet := (Thetmax - Thetmin)/(numang - 1);
   ScattAmp(LabKE, eta, Thetmin, dThet, LmaxP, numang, MySystem);
END;

{ ---------------------- Cross Section Calculations ----------------------- }

PROCEDURE MakeUnpolSigma(i: Integer; VAR theta: real; VAR sig: Float);

   FUNCTION ModSqr(cx: complex): Float;
   BEGIN
      ModSqr := Sqr(cx.re) + Sqr(cx.im);
   END;

VAR
   norm : Float;
   a, b, c, e, f : Complex;
BEGIN
   norm := 10;
   GetWolfAmps(theta, a, b, c, e, f, i);
   sig := ModSqr(a) + ModSqr(b) + 2*Modsqr(c) + ModSqr(e) + ModSqr(f);
   sig := norm*sig;
END;

PROCEDURE Sigmas1(i: integer; VAR theta: Real; VAR sig, sigP, sigD :Float);
{ Calculate some observables from the Wolfenstein amplitudes.
  - Projectile and detected spin polarizations perpendicular
    to the scattering plane.  (Unpolarized target) }
 
   PROCEDURE Conj(a:COMPLEX; VAR c:COMPLEX);
   BEGIN
      c.re:=a.re;
      c.im:=-a.im;
   END;
 
VAR
   norm          : Float;
   t1, t2, t3    : Complex;
   a, b, c, e, f : Complex;
   siga, sigb    : Float;
   sigc, sige    : Float;
   sigf          : Float;
BEGIN
   norm := 10;
   GetWolfAmps(theta, a, b, c, e, f, i);
   { De-comment the following statement to see the Wolfenstein amps }
   { ShowAmps(theta, a, b, c, e, f, temp); }
   Conj(a,t1);  CxOp(a,'*',t1,t3);  siga := norm*t3.re;
   Conj(b,t1);  CxOp(b,'*',t1,t3);  sigb := norm*t3.re;
   Conj(c,t1);  CxOp(c,'*',t1,t3);  sigc := norm*t3.re;
   Conj(e,t1);  CxOp(e,'*',t1,t3);  sige := norm*t3.re;
   Conj(f,t1);  CxOp(f,'*',t1,t3);  sigf := norm*t3.re;
   sig := siga + sigb + 2*sigc + sige + sigf;
   Conj(c,t1);  CxOp(a,'+',b,t2);  CxOp(t1,'*',t2,t3);
   sigP := 2*norm*t3.re;
   sigD := sig - 2*(sige + sigf);
END;

PROCEDURE Sigmas2(i: integer; VAR theta: Real;
                              VAR sig, sigxx, sigxz, sigzz :Float);
{ Calculate some observables from the Wolfenstein amplitudes.
  - Projectile and detected spin polarizations parallel to the
    scattering plane.  (Unpolarized target) }
 
   PROCEDURE Conj(a:COMPLEX; VAR c:COMPLEX);
   BEGIN
      c.re:=a.re;
      c.im:=-a.im;
   END;
 
VAR
   norm          : Float;
   t1, t2, t3    : Complex;
   a, b, c, e, f : Complex;
   siga, sigb    : Float;
   sigc, sige    : Float;
   sigf          : Float;
BEGIN
   norm := 10;
   GetWolfAmps(theta, a, b, c, e, f, i);
   { De-comment the following statement to see the Wolfenstein amps }
   { ShowAmps(theta, a, b, c, e, f, temp); }
   Conj(a,t1);  CxOp(a,'*',t1,t3);  siga := norm*t3.re;
   Conj(b,t1);  CxOp(b,'*',t1,t3);  sigb := norm*t3.re;
   Conj(c,t1);  CxOp(c,'*',t1,t3);  sigc := norm*t3.re;
   Conj(e,t1);  CxOp(e,'*',t1,t3);  sige := norm*t3.re;
   Conj(f,t1);  CxOp(f,'*',t1,t3);  sigf := norm*t3.re;
   sig := siga + sigb + 2*sigc + sige + sigf;
   sigxx := siga - sigb + sige - sigf;
   Conj(c,t1);  CxOp(a,'-',b,t2);  CxOp(t1,'*',t2,t3);
   sigxz := 2*norm*t3.im;
   sigzz := siga - sigb - sige + sigf;
END;


{ -------------------------- Interface Routines --------------------------- }

FUNCTION Break: Boolean;
BEGIN
   CheckForEvents;
   WITH Event DO Break := KeyPressed OR MouseClicked;
END;
 
PROCEDURE OpenScrn(VAR Screen: TInputScreen);
BEGIN
END;

PROCEDURE CloseScrn(VAR Screen: TInputScreen);
BEGIN
END;

PROCEDURE PutBigPixel(x, y, color: Integer);
VAR
   i, j : Integer;
BEGIN
   FOR i := -1 TO 1 DO
   FOR j := -1 TO 1 DO
   PutPixel(x + i, y + j, color);
END;

PROCEDURE PlotPoint( x1, y1: Float);
{ color and size can be changed.}
   VAR
      x, y: integer;
      LineInfo : LineSettingsType;
BEGIN
   Map(x1, y1, x, y);
   GetLineSettings(LineInfo);
   WITH LineInfo DO
      BEGIN
         IF Thickness = ThickWidth THEN
            PutBigPixel(x, y, getcolor)
         ELSE PutPixel(x, y, getcolor);
      END;
END;

PROCEDURE Display(i,j: Integer; myLabel: String; value: Float);
{ Use the MUPPET Print function to display a numerical
  value together with a descriptive label }
BEGIN
   Print(i,j,Concat(myLabel, NumStr(value,10,5)));
END;
 
PROCEDURE DisplayInt(i,j: Integer; myLabel: String; ival: LongInt);
{ Use the MUPPET Print function to display an integer
  value together with a descriptive label }
BEGIN
   Print(i,j,Concat(myLabel, NumStr(ival,10,0)));
END;
 
 
PROCEDURE PlotDvtr(xv, yv: DVector; ibeg, iend: Integer);
{ Plots subset of DVector elements either forwards or backwards }
VAR
   i1, i2, di, ii, i, x1, y1 : Integer;
BEGIN
   HideMouse;
   IF ibeg <> iend THEN
   BEGIN
      IF scaleNumber=0 THEN autoScaling(0,xv,yv);
      IF ibeg < iend THEN BEGIN i1:=ibeg; i2:=iend; di:= 1 END
      ELSE                BEGIN i1:=iend; i2:=ibeg; di:=-1 END;
         i := ibeg;
         Map(xv.Value(i),yv.Value(i),x1,y1);
         MoveTo(x1,y1);
         FOR ii := i1 + 1 TO i2 DO
         BEGIN
            i := i + di;
            Map(xv.Value(i),yv.Value(i),x1,y1);
            LineTo(x1,y1);
         END;
      END;
   ShowMouse;
END;

PROCEDURE PreventAdjustments;
BEGIN
   WITH MasterMenu DO
      BEGIN
         RowActivate(3,3,FALSE);
         RowActivate(4,3,FALSE);
         RowActivate(5,6,FALSE);
      END;
END;

PROCEDURE AllowAdjustments;
BEGIN
   WITH MasterMenu DO
      BEGIN
         RowActivate(3,3,TRUE);
         RowActivate(4,3,TRUE);
         RowActivate(5,6,TRUE);
      END;
END;

FUNCTION DropLeadingBlanks(ThisString: String): String;
VAR
   i : Integer;
BEGIN
   i := 1;
   WHILE Copy(ThisString,i,1) = ' ' DO Inc(i);
   DropLeadingBlanks := Copy(ThisString,i,80);
END;

FUNCTION GetRestofLine(thisString: String): String;
VAR
   mystring : String;
BEGIN
   mystring := DropLeadingBlanks(thisString);
   GetRestofLine := Copy(mystring,Pos(' ',mystring),80);
END;

FUNCTION GetNumber(thisString: String): Float;
{ Return the numerical value of the first entry in the input string.
  Returns zero if the first entry is not a valid number. }
VAR
   mystring : String;
   myvalue  : Real;
   i, code  : Integer;
BEGIN
   mystring := DropLeadingBlanks(thisString);
   i := Pos(' ',mystring);
   IF i = 0 THEN Val(mystring,myvalue,code)
            ELSE Val(Copy(mystring,1,i-1),myvalue,code);
   IF code = 0 THEN GetNumber := myvalue
               ELSE GetNumber := 0;
END;

PROCEDURE GetTitleIndex(VAR InFile: Text);
{ Purpose: Read the file that contains user-defined sets of interaction
           parameters, display a numbered list of the titles and request
           the user to select one of them.  Return the number of the
           selected title.  Return a zero if request is cancelled. }
VAR
   ViewScreen : TInputScreen;
   icount     : Integer;
   thisline, thisTitle : String;
BEGIN
   OpenScrn(ViewScreen);
   WITH ViewScreen DO
   BEGIN
      init;
      DefineInputport(0.1, 0.9, 0.1, 0.9);
      LoadLine( Center('Select one of the nucleon-nucleon') );
      LoadLine( Center('interactions listed below:-') );
      LoadLine('');
      LoadLine(''); 
      Reset(InFile);
      icount := 0;
      WHILE NOT EOF(InFile) AND NOT (icount > 10) DO
         BEGIN
            Readln(InFile,thisline);
            IF Copy(thisline,1,6) = 'Title:' THEN
               BEGIN
                  Inc(icount);
                  thisTitle := DropLeadingBlanks(Copy(thisline,7,80));
                  LoadLine(Concat(' #1', NumStr(icount,3,0), '. ', thisTitle));
               END;
         END;
      LoadLine('');
      LoadLine(''); 
      LoadLine('                 [  Ok  ]           [Cancel]       ');
      IF UserIndex = 0 THEN UserIndex := 1;
      SetRadioButton('1',UserIndex);
      AcceptScreen;
      IF NOT Canceled THEN UserIndex := GetRadioButton('1')
                      ELSE UserIndex := 0;
      done;
   END;
   CloseScrn(ViewScreen);
END;

FUNCTION ThisPotlOK(ThisPotl: PotlConsts): Boolean;
CONST
   Vmax = 1e7;  Rmin = 0.1;  Rmax = 3.5;
VAR
   check : Boolean;
BEGIN
   check := TRUE;
   WITH ThisPotl DO
      BEGIN
         IF (core < 0.1) OR (core > 2) THEN check := FALSE;
         WITH SE_Consts DO
            BEGIN
               IF ABS(CNdepth) > Vmax THEN check := FALSE;
               IF (CNdepth <> 0) AND ((CNrange < Rmin) OR (CNrange > Rmax))
                  THEN check := FALSE;
            END;
         WITH SO_Consts DO
            BEGIN
               IF ABS(CNdepth) > Vmax THEN check := FALSE;
               IF (CNdepth <> 0) AND ((CNrange < Rmin) OR (CNrange > Rmax))
                  THEN check := FALSE;
            END;
         WITH TE_Consts DO
            BEGIN
               IF ABS(CNdepth) > Vmax THEN check := FALSE;
               IF (CNdepth <> 0) AND ((CNrange < Rmin) OR (CNrange > Rmax))
                  THEN check := FALSE;
               IF ABS(LSdepth) > Vmax THEN check := FALSE;
               IF (LSdepth <> 0) AND ((LSrange < Rmin) OR (LSrange > Rmax))
                  THEN check := FALSE;
               IF ABS(TNdepth) > Vmax THEN check := FALSE;
               IF (TNdepth <> 0) AND ((TNrange < Rmin) OR (TNrange > Rmax))
                  THEN check := FALSE;
            END;
         WITH TO_Consts DO
            BEGIN
               IF ABS(CNdepth) > Vmax THEN check := FALSE;
               IF (CNdepth <> 0) AND ((CNrange < Rmin) OR (CNrange > Rmax))
                  THEN check := FALSE;
               IF ABS(LSdepth) > Vmax THEN check := FALSE;
               IF (LSdepth <> 0) AND ((LSrange < Rmin) OR (LSrange > Rmax))
                  THEN check := FALSE;
               IF ABS(TNdepth) > Vmax THEN check := FALSE;
               IF (TNdepth <> 0) AND ((TNrange < Rmin) OR (TNrange > Rmax))
                  THEN check := FALSE;
            END;
      END;
   ThisPotlOK := check;
END;

FUNCTION ReadThisPotl(VAR InFile: Text; UserIndex: Integer;
                      VAR ThisPotl: PotlConsts): Boolean;
{ Purpose: Read the requested set of interaction parameters }
VAR
   CoreFound    : Boolean;
   SingletFound : Boolean;
   TripletFound : Boolean;
   SpinOrbFound : Boolean;
   TensorFound  : Boolean;

   PROCEDURE SetFalse;
   BEGIN
      CoreFound := FALSE;
      SingletFound := FALSE;
      TripletFound := FALSE;
      SpinOrbFound := FALSE;
      TensorFound  := FALSE;
   END;

   FUNCTION AllFound: Boolean;
   BEGIN
      AllFound := CoreFound AND
                  SingletFound AND TripletFound AND
                  SpinOrbFound AND TensorFound;
   END;

VAR
   quit : Boolean;
   icount : Integer;
   key    : String[6];
   thisline, thispiece : String;
BEGIN
 { Reset the file and read down to the desired title line }
   Reset(InFile);
   icount := 0;
   REPEAT
      Readln(InFile,thisline);
      IF Copy(thisline,1,6) = 'Title:' THEN Inc(icount);
   UNTIL icount = UserIndex;
 { Read the interaction parameters }
   quit := FALSE;
   SetFalse;
   WITH ThisPotl DO
      BEGIN { WITH ThisPotl }
         Title := DropLeadingBlanks(Copy(thisline,7,80));
         OPEPswitch := FALSE;
         Form := Yukawa;
         REPEAT
               Readln(InFile,thisline);
               key := Copy(thisline,1,6);
               IF key = 'Includ' THEN OPEPswitch := TRUE;
               IF key = 'Core r' THEN
                  BEGIN
                     Core := GetNumber(Copy(thisline,13,80));
                     CoreFound := TRUE;
                  END;
               IF key = 'Single' THEN
                  BEGIN
                     WITH SE_Consts DO
                        BEGIN
                           thispiece := Copy(thisline,8,80);
                           CNdepth := GetNumber(thispiece);
                           thispiece := GetRestofLine(thispiece);
                           CNrange := GetNumber(thispiece);
                        END;
                     WITH SO_Consts DO
                        BEGIN
                           thispiece := GetRestofLine(thispiece);
                           CNdepth := GetNumber(thispiece);
                           thispiece := GetRestofLine(thispiece);
                           CNrange := GetNumber(thispiece);
                        END;
                     SingletFound := TRUE;
                  END;
               IF key = 'Triple' THEN
                  BEGIN
                     WITH TE_Consts DO
                        BEGIN
                           thispiece := Copy(thisline,8,80);
                           CNdepth := GetNumber(thispiece);
                           thispiece := GetRestofLine(thispiece);
                           CNrange := GetNumber(thispiece);
                        END;
                     WITH TO_Consts DO
                        BEGIN
                           thispiece := GetRestofLine(thispiece);
                           CNdepth := GetNumber(thispiece);
                           thispiece := GetRestofLine(thispiece);
                           CNrange := GetNumber(thispiece);
                        END;
                     TripletFound := TRUE;
                  END;
               IF key = 'Spin-O' THEN
                  BEGIN
                     WITH TE_Consts DO
                        BEGIN
                           thispiece := Copy(thisline,9,80);
                           LSdepth := GetNumber(thispiece);
                           thispiece := GetRestofLine(thispiece);
                           LSrange := GetNumber(thispiece);
                        END;
                     WITH TO_Consts DO
                        BEGIN
                           thispiece := GetRestofLine(thispiece);
                           LSdepth := GetNumber(thispiece);
                           thispiece := GetRestofLine(thispiece);
                           LSrange := GetNumber(thispiece);
                        END;
                     SpinOrbFound := TRUE;
                  END;
               IF key = 'Tensor' THEN
                  BEGIN
                     WITH TE_Consts DO
                        BEGIN
                           thispiece := Copy(thisline,7,80);
                           TNdepth := GetNumber(thispiece);
                           thispiece := GetRestofLine(thispiece);
                           TNrange := GetNumber(thispiece);
                        END;
                     WITH TO_Consts DO
                        BEGIN
                           thispiece := GetRestofLine(thispiece);
                           TNdepth := GetNumber(thispiece);
                           thispiece := GetRestofLine(thispiece);
                           TNrange := GetNumber(thispiece);
                        END;
                     TensorFound  := TRUE;
                  END;
               quit := AllFound;
               IF EOF(InFile) OR (key = 'Title:') THEN quit := TRUE;
         UNTIL quit;
      END;  { WITH ThisPotl }
   ReadThisPotl := AllFound AND ThisPotlOK(ThisPotl);
END;

FUNCTION ReadFile(VAR ThisPotl: PotlConsts): Boolean;
{ Purpose: Organize the reading of user-defined potential parameters
  from a file.  ReadFile returns TRUE if successful, FALSE if not.
  If successful, potential parameters will be returned via ThisPotl. }
VAR
   InFile   : Text;
   thisline : String;
   filename : String;
   newparamsfound : Boolean;
BEGIN
   newparamsfound := FALSE;
   filename := OpenFile('*.INT');
   IF filename <> '' THEN
      BEGIN
         Assign(InFile, filename);
         {$I-}
         Reset(Infile);
         {$I+}
         IF IOResult = 0 THEN
            BEGIN
               GetTitleIndex(InFile);
               IF UserIndex > 0 THEN { Valid title found and selected }
                  BEGIN
                     IF ReadThisPotl(InFile, UserIndex, ThisPotl) THEN
                        newparamsfound := TRUE;
                  END;
            END;
      END;
   IF NOT newparamsfound THEN
      Announce('Read file error.  Potential parameters unchanged');
   ReadFile := newparamsfound;
END;

FUNCTION SelectInteraction: Boolean;
{ Purpose:  Select one of several pre-defined interaction models }
VAR
   ViewScreen     : TInputScreen;
   i, TempIndex   : Integer;
   TempPotl       : PotlConsts;
   NewParamsFound : Boolean;
BEGIN
   OpenScrn(ViewScreen);
   WITH ViewScreen DO
   BEGIN
      init;
      DefineInputport(0.1, 0.9, 0.14, 0.86);
      LoadLine( Center('Select one of the nucleon-nucleon') );
      LoadLine( Center('interactions listed below:-') );
      LoadLine('');
      LoadLine(''); 
      FOR i := 1 TO 10 DO LoadLine(Concat(' #1',
         NumStr(i,3,0), '. ', PotlParams[i].Title));
      LoadLine('');
      LoadLine(''); 
      LoadLine('                 [  Ok  ]           [Cancel]       ');
      SetRadioButton('1',PotlIndex);
      AcceptScreen;
      NewParamsFound := FALSE;
      IF NOT Canceled THEN
         BEGIN
            NewParamsFound := TRUE;
            TempIndex := GetRadioButton('1');
            TempPotl := PotlParams[TempIndex];
            IF TempPotl.Form = UserDefined THEN
               NewParamsFound := ReadFile(TempPotl);
            IF NewParamsFound THEN
               BEGIN
                  PotlIndex := TempIndex;
                  ThisPotl := TempPotl;
                  OPEP := ThisPotl.OPEPswitch;
                  IF ThisPotl.Form = HJ THEN
                     BEGIN
                        ThisPotl.Core := RADMin;
                        PreventAdjustments;
                     END
                  ELSE AllowAdjustments;
                  TheoryDone := FALSE;
               END;
         END;
      SelectInteraction := NewParamsFound;
      done;
   END;
   CloseScrn(ViewScreen);
END;

PROCEDURE LoadNumbers(VAR ViewScreen: TInputScreen; ThisPotl: PotlConsts);
CONST
   MinRange =    0;   MaxRange = 3.5;
   MinPotl  = -1e7;   MaxPotl  = 1e7;
BEGIN
   WITH ViewScreen DO
   WITH ThisPotl DO
   BEGIN
      SetNumber(1,Core);
      SetNumberLimits(1, 0.1, 2.0);
      WITH SE_Consts DO
      BEGIN
         SetNumber(2,CNdepth);
         SetNumber(3,CNrange);
         SetNumberLimits(2,MinPotl,MaxPotl);
         SetNumberLimits(3,MinRange,MaxRange);
      END;
      WITH TE_Consts DO
      BEGIN
         SetNumber( 6,CNdepth);
         SetNumber( 7,CNrange);
         SetNumberLimits(6,MinPotl,MaxPotl);
         SetNumberLimits(7,MinRange,MaxRange);
         SetNumber(10,LSdepth);
         SetNumber(11,LSrange);
         SetNumberLimits(10,MinPotl,MaxPotl);
         SetNumberLimits(11,MinRange,MaxRange);
         SetNumber(14,TNdepth);
         SetNumber(15,TNrange);
         SetNumberLimits(14,MinPotl,MaxPotl);
         SetNumberLimits(15,MinRange,MaxRange);
      END;
      WITH SO_Consts DO
      BEGIN
         SetNumber(4,CNdepth);
         SetNumber(5,CNrange);
         SetNumberLimits(4,MinPotl,MaxPotl);
         SetNumberLimits(5,MinRange,MaxRange);
      END;
      WITH TO_Consts DO
      BEGIN
         SetNumber( 8,CNdepth);
         SetNumber( 9,CNrange);
         SetNumberLimits(8,MinPotl,MaxPotl);
         SetNumberLimits(9,MinRange,MaxRange);
         SetNumber(12,LSdepth);
         SetNumber(13,LSrange);
         SetNumberLimits(12,MinPotl,MaxPotl);
         SetNumberLimits(13,MinRange,MaxRange);
         SetNumber(16,TNdepth);
         SetNumber(17,TNrange);
         SetNumberLimits(16,MinPotl,MaxPotl);
         SetNumberLimits(17,MinRange,MaxRange);
      END;
   END;
END;

PROCEDURE GetNumbers(ViewScreen: TInputScreen; VAR ThisPotl: PotlConsts);
BEGIN
   WITH ViewScreen DO
   WITH ThisPotl DO
      BEGIN
         Core := GetNumber(1);
         WITH SE_Consts DO
         BEGIN
            CNdepth := GetNumber(2);
            CNrange := GetNumber(3);
         END;
         WITH TE_Consts DO
         BEGIN
            CNdepth := GetNumber(6);
            CNrange := GetNumber(7);
            LSdepth := GetNumber(10);
            LSrange := GetNumber(11);
            TNdepth := GetNumber(14);
            TNrange := GetNumber(15);
         END;
         WITH SO_Consts DO
         BEGIN
            CNdepth := GetNumber(4);
            CNrange := GetNumber(5);
         END;
         WITH TO_Consts DO
         BEGIN
            CNdepth := GetNumber(8);
            CNrange := GetNumber(9);
            LSdepth := GetNumber(12);
            LSrange := GetNumber(13);
            TNdepth := GetNumber(16);
            TNrange := GetNumber(17);
         END;
   END; { WITH }
END;
 
FUNCTION Center(inString: String): String;
VAR
   tempStr : String;
BEGIN
   tempStr := '                               ';
   Insert(inString, tempStr, 31-Round(Length(inString)/2));
   Center := tempStr;
END;

FUNCTION GetNewPotlParams: boolean;
VAR
   tempPotl   : PotlConsts;
   ViewScreen : TInputScreen;
BEGIN
   OpenScrn(ViewScreen);
   WITH ViewScreen DO
   BEGIN
      init;
      DefineInputport(0.1, 0.9, 0.07, 0.93); 
      LoadLine( Center('VIEW AND ADJUST THE INTERACTION PARAMETERS') );
      LoadLine('');
      LoadLine( Center(ThisPotl.Title) );
      IF OPEP THEN
         LoadLine( Center('(Includes OPEP)') )
      ELSE
         LoadLine( Center('(Does not include OPEP)') );
      LoadLine('');
      LoadLine('               Hard core radius = {   0} fm       ');
      LoadLine('');
   LoadLine('                 EVEN Parity              ODD  Parity     ');
   LoadLine('               Depth     Range          Depth     Range   ');
   LoadLine('               (MeV)     (fm)           (MeV)     (fm)    ');
   LoadLine('   Singlet    {      0}  {    0}       {      0}  {    0} ');
   LoadLine('   Triplet    {      0}  {    0}       {      0}  {    0} ');
   LoadLine('   Spin-Orb   {      0}  {    0}       {      0}  {    0} ');
   LoadLine('   Tensor     {      0}  {    0}       {      0}  {    0} ');
      LoadLine('');
      LoadLine('          NOTE:  Any changes made here will be lost    ');
      LoadLine('                 if you re-select an interaction.      ');
      LoadLine('                 Use Help for info on valid entries.   ');
      LoadLine('');
      LoadLine('         [  Ok  ]        [Cancel]        [ Help ]      ');
      tempPotl := ThisPotl;
      Loadnumbers(ViewScreen, tempPotl);
      SetHelpFile('twobody.hlp','InteractionParams');
      AcceptScreen;
      GetNumbers(ViewScreen, tempPotl);
      GetNewPotlParams := FALSE;
      IF NOT Canceled THEN
         BEGIN
            IF ThisPotlOK(tempPotl) THEN
               BEGIN
                  ThisPotl := tempPotl;
                  TheoryDone := FALSE;
                  GetNewPotlParams := TRUE;
               END
            ELSE
               BEGIN
                  Announce('Invalid parameters.  Try again...');
               END;
         END;
      done;
   END;
   CloseScrn(ViewScreen);
END;

PROCEDURE ShowBlankRectangle(x1, x2, y1, y2: Float);
VAR
   Vx1, Vx2, Vy1, Vy2 : Integer;
BEGIN
   Vx1:=Round(x1 * GetMaxX);
   Vx2:=Round(x2 * GetMaxX);
   Vy1:=Round((1.0-y2) * GetMaxY);
   Vy2:=Round((1.0-y1) * GetMaxY);
   SetFillStyle(SolidFill, blakk);
   Bar3D(Vx1, Vy1, Vx2, Vy2, 0, Topoff);
END;
 
PROCEDURE DrawBlankRectangle(x1, x2, y1, y2: Float);
VAR
   Vx1, Vx2, Vy1, Vy2 : Integer;
BEGIN
   Vx1:=Round(x1 * GetMaxX);
   Vx2:=Round(x2 * GetMaxX);
   Vy1:=Round((1.0-y2) * GetMaxY);
   Vy2:=Round((1.0-y1) * GetMaxY);
   SetFillStyle(SolidFill, blakk);
   Bar(Vx1, Vy1, Vx2, Vy2);
END;
 
PROCEDURE PreparePotlPlot(ymin, ymax: Float);
VAR
   dxtik, dytik : Float;
BEGIN
   DefineViewport(1, 0.45, 0.95, 0.60, 0.93);
   OpenViewPort(1);
   DefineScale(1, 0.0,RmaxInit, ymin,ymax);
   SelectScale(1);
   dxtik := TickSpace(RmaxInit);
   dytik := TickSpace(ymax - ymin);
   Axis(0.0, 0.0, dxtik, dytik);
   PutLabel(bottom,'Radius (fm)');
   PutLabel(left,'Potential (MeV)');
END;
 
PROCEDURE PrepareWaveFuncPlot(ymin, ymax: Float);
VAR
   dxtik, dytik : Float;
BEGIN
   SelectViewport(0);
   DrawBlankRectangle(0.39, 0.45, 0.10, 0.55);
   DefineViewport(2, 0.45, 0.95, 0.13, 0.52);
   OpenViewPort(2);
   DefineScale(2, 0.0,RmaxInit, ymin,ymax);
   SelectScale(2);
   dxtik := TickSpace(RmaxInit);
   dytik := TickSpace(ymax - ymin);
   Axis(0.0, 0.0, dxtik, dytik);
   PutLabel(bottom,'Radius (fm)');
   PutLabel(left,'Wavefunction');
END;

FUNCTION GetGoodEnergy(VAR BE: Float) : Boolean;
VAR
   EnergyScreen : TInputScreen;
BEGIN
   OpenScrn(EnergyScreen);
   WITH EnergyScreen DO
   BEGIN
      init;
      DefineInputport(0.45, 0.95, 0.13, 0.52); 
      LoadLine(' Accept or change the current guess');
      LoadLine(' at the binding energy ');
      LoadLine('');
      LoadLine('           BE = {      0} ');
      LoadLine('');
      LoadLine('( Guessed value must lie in the range');
      LoadLine('      1e-6 MeV <= BE <= 500 MeV )    ');
      LoadLine('');
      LoadLine('        [  Ok  ]       [Cancel]  ');
      SetNumber(1,BE);
      SetNumberLimits(1, 0, 500);
      AcceptScreen;
      BE := GetNumber(1);
      GetGoodEnergy := NOT canceled;
      done;
   END;
   CloseScrn(EnergyScreen);
END;
 
PROCEDURE WaitForInstructions(VAR BE, OldDeltaBE: Float;
                              VAR LostIt: Boolean);
VAR
   key  : Byte;
   quit : Boolean;
   HotKeys1 : THotKeys;
BEGIN
   WITH HotKeys1 DO BEGIN
      init(4);
      key[1] := 'F1-Help';
      key[2] := 'F2-Continue';
      key[3] := 'F3-Guess BE';
      key[4] := 'F10-Menu';
   END;
   HotKeys1.Display;
   quit := FALSE;
   REPEAT
      CheckForEvents;
      IF HotKeys1.Pressed(key) THEN
         BEGIN
            quit := TRUE;
            CASE key OF
           1: BEGIN
                 ShowHelp('twobody.hlp','AboutEgySearch');
                 quit := FALSE;
              END;
           3: BEGIN
                 OldDeltaBE := Large;
                 LostIt := NOT GetGoodEnergy(BE);
              END;
           4: LostIt := TRUE; { Returns to menu }
            END; { CASE }
         END
      ELSE IF Event.KeyPressed OR Event.MouseClicked THEN quit := TRUE;
   UNTIL quit;
   CheckForEvents;
END;
 
FUNCTION GetEgyRange(VAR MinEgy, MaxEgy: Float;
                     VAR NumEgy: Integer) : Boolean;
VAR
   GoodRange : Boolean;
   EnergyScreen : TInputScreen;
   Number : Real;
BEGIN
   OpenScrn(EnergyScreen);
   WITH EnergyScreen DO
   BEGIN
      init;
      DefineInputport(0.28, 0.72, 0.25, 0.70); 
      LoadLine('     Enter an energy range      ');
      LoadLine('');
      LoadLine(' Min Energy         = {     0}  ');
      LoadLine(' Max Energy         = {     0}  ');
      LoadLine('(Energies in range 0 to 400 MeV)');
      LoadLine('');
      LoadLine('Number of Energies  = {     0}  ');
      LoadLine('(An integer in range 2 to 250)  ');
      LoadLine('');
      LoadLine('      [  Ok  ]   [Cancel]       ');
      SetNumber(1,MinEgy);
      SetNumber(2,MaxEgy);
      SetNumber(3,NumEgy);
      SetNumberLimits(1,0,400);
      SetNumberLimits(2,0,400);
      SetNumberLimits(3,2,250);
      GoodRange := FALSE;
      REPEAT
         AcceptScreen;
         MinEgy := GetNumber(1);
         MaxEgy := GetNumber(2);
         Number := GetNumber(3);
         NumEgy := round(Number);
         IF MaxEgy <= MinEgy THEN
            Announce('Max Energy must exceed Min Energy.  Try again...')
         ELSE IF Number <> NumEgy THEN
            Announce('Integer number of energies, please.  Try again...')
         ELSE GoodRange := TRUE;
      UNTIL GoodRange;
      GetEgyRange := NOT Canceled;
      done;
   END;
   CloseScrn(EnergyScreen);
END;
 
PROCEDURE FindReasonableScale(Numang: Integer; VAR Sigmax: Float);
{ Present version not definitive! }
VAR
   i      : Integer;
   theta  : Real;
   sig, maxval, minval : Float;
BEGIN
   minval := Large;
   maxval := 0;
   FOR i := 1 TO Numang DO
      BEGIN
         MakeUnpolSigma(i, theta, sig);
         IF sig > maxval THEN maxval := sig;
         IF sig < minval THEN minval := sig;
      END;
   IF MySystem = 1 THEN Sigmax := 2.5*minval
                   ELSE Sigmax := 1.1*maxval;
END;

PROCEDURE GeneralInit;
BEGIN { General initialization actions }

 { Set # points available for radial grid.   }
 { Npoints is never changed from this value. }
   Npoints := 2*Round( RmaxInit/(2.0*Rstep) ) + 1;
 
  SetComplexConsts;

 { Set up initial parameters for general calculations }

   MySystem := 2;
   LabKE    := 200; { MeV }
   Thetmin  := 0;
   Thetmax  := 180;
   Numang   := 91;
   SetMuftr(MySystem);
   SetPotlConsts;
   SetPotlConstsHJ;
   PotlIndex := 7;
   UserIndex := 1;
   ThisPotl := PotlParams[PotlIndex];
   OPEP := ThisPotl.OPEPswitch;
   SetupTabulatedPhases;

 { Declare initial absence of theory phases for cross section calculations }
   TheoryDone := FALSE;

END;  { General initialization actions }

END.  { Unit MASTER.PAS }
