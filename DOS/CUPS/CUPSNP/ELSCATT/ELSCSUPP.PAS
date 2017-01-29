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

                    {*********************************}
                    {*  Electron Scattering Program  *}
                    {*         ElScSupp.PAS          *}
                    {*        R. J. Philpott         *}
                    {*       Copyright @ 1994        *}
                    {*********************************}

UNIT ElScSupp;
{$N+}

                                INTERFACE

USES
     crt, 
   graph, CUPSmupp, CUPS, CUPSfunc, CUPSproc, CUPSgui, CUPSgrph;

(* Special DVectors that may be introduced for testing *)
(* VAR                                                 *)
(*    testJv, testJM1v, testJP1v : DVector;            *)

TYPE
   Float = Extended;
CONST
   hbarc  = 197.327; { MeV fm }
   amu    = 931.49;  { MeV }
   Rmax   =  10.0;   { fm }
   Rstep  =   0.05;  { fm }
   Qmax   =   5.0;   { inverse fm }
   Qstep  =   0.05;  { inverse fm }
   glprot =   1.00;
   gsprot =   5.587;
   glneut =   0.00;
   gsneut =  -3.826;

VAR
   NumR   : Integer; { Size of array for functions of r }
   NumQ   : Integer; { Size of array for functions of q }

VAR
   MasterMenu : TMenu;    { Master control menu   }
   Hotkeys    : THotKeys; { General hot keys      }

VAR { Used by Inelastic and Response sections }
   Iselect          : Integer;
   TargetSelector   : Integer;
   Nparticle        : Integer;
   HoleSelector     : Integer;
   ParticleSelector : Integer;
   Jspin            : Integer;

FUNCTION MaxI(n1, n2: Integer): Integer;
FUNCTION MinI(n1, n2: Integer): Integer;
FUNCTION MaxF(x1, x2: Float): Float;
FUNCTION Even(n: Integer): Boolean;
FUNCTION Phase(n: Integer): Integer;

CONST
   MaxFactorial = 20;
   MaxDoubleFac = 41;
   NumOscCoeff  = 25; { Set to value MaxDeltaN + 2*MaxNhole + 1 }
   JfacMax      = 61;
VAR
   rootpiby4 : Float;

VAR
   f    : ARRAY[0..MaxFactorial] OF Float;
   df   : ARRAY[0..MaxDoubleFac] OF Float;
   Jfac : ARRAY[0..JfacMax] OF Float;

PROCEDURE MakeFactorial;
PROCEDURE MakeDoubleFac;
PROCEDURE MakeJFacts;

VAR
   lchar : ARRAY[0..20] OF Char;

PROCEDURE SetupLchar;

CONST
   MaxNumsp = 7;
TYPE
   SinglPclRec =
      RECORD
         nsp, lsp, jjsp : Integer;
      END;
VAR
   SinglPclArray : ARRAY[1..MaxNumsp] OF SinglPclRec;

PROCEDURE SetupSinglPclArray;
FUNCTION ProjectileRestEnergy(ProjectileSelector: Integer): Float;
FUNCTION ProjectileName(ProjectileSelector: Integer): String;
FUNCTION TargetName(TargetSelector: Integer): String;
FUNCTION DensityName(DensitySelector: Integer): String;
FUNCTION MakeTargetString(itarg: Integer): String;
FUNCTION TargetMassNumber(itarg: Integer): Integer;
FUNCTION Aosc(itarg: Integer): Float;
PROCEDURE MakeHoleQuantumNumbers(NumOrbit: Integer;
                  VAR nh, lh, jjh: Integer);
FUNCTION MakeHoleString(NumOrbit: Integer): String;
PROCEDURE MakeParticleQuantumNumbers(Nparticle, NumOrbit: Integer;
                  VAR np, lp, jjp: Integer);
FUNCTION MakeParticleString(Nparticle, NumOrbit: Integer): String;
FUNCTION InCore(Nparticle, NumOrbit: Integer): Boolean;

{ -------------------- Some Generally Useful Procedures ------------------- }

PROCEDURE OpenScrn(VAR Screen: TInputScreen);
PROCEDURE CloseScrn(VAR Screen: TInputScreen);
PROCEDURE OpenMenu(VAR Menu: TBarMenu);
PROCEDURE CloseMenu(VAR Menu: TBarMenu);
PROCEDURE Request1DVector(VAR av: DVector; Npoints: Integer);
PROCEDURE Release1DVector(VAR av: DVector);
PROCEDURE Request2DVectors(VAR av, bv: DVector; Npoints: Integer);
PROCEDURE Release2DVectors(VAR av, bv: DVector);
PROCEDURE Request3DVectors(VAR av, bv, cv: DVector; Npoints: Integer);
PROCEDURE Release3DVectors(VAR av, bv, cv: DVector);
PROCEDURE Request1DMatrix(VAR matrix: DMatrix; Nrow, Ncol: Integer);
PROCEDURE Release1DMatrix(VAR matrix: DMatrix);
PROCEDURE SetGrid(Npoints: Integer; Xstep: Float; VAR xv : DVector);
FUNCTION IsInt(x: Float): Boolean;
FUNCTION Triang(a, b, c: Float): Boolean;
FUNCTION CLBH(a, b, c: Float): Float;
FUNCTION CJcoeff(jj1, jj2, L, J: Integer): Float;
FUNCTION YxSrme(l1, jj1, l2, jj2, L, S, J: Integer): Float;
PROCEDURE CofUU(n1, l1, n2, l2: Integer;
                VAR cnv: DVector; VAR kpmin, kpmax: Integer);
PROCEDURE CofUUP(n1, l1, n2, l2: Integer;
                 VAR cnv: DVector; VAR kpmin, kpmax: Integer);
FUNCTION ConfHyp(a, b, z: Float): Float;
PROCEDURE Qden(L, itype: Integer; Aosc: Float;
               cnv: DVector; kpmin, kpmax: Integer;
               qv: DVector; NumQ: Integer; VAR denv: DVector);
FUNCTION LimCoulbyq(Aosc: Float; cnv: DVector; kpmin, kpmax: Integer): Float;
PROCEDURE MomDensity(n1, l1, jj1, n2, l2, jj2, J: Integer; Aosc: Float;
                     qv: DVector; VAR rhoQv : DVector);
PROCEDURE ElasticFormFactor(qv: DVector; VAR rhoQv : DVector);
PROCEDURE RadialDensity(n1, l1, n2, l2: Integer; aosc: Float;
                            rv: DVector; VAR rhoRv: DVector);
PROCEDURE DerivDensity(n1, l1, n2, l2: Integer; aosc: Float;
                           rv: DVector; VAR drhoRv: DVector);
PROCEDURE DerivOfProduct(n1, l1, n2, l2: Integer; aosc: Float;
                             rv: DVector; VAR rhoRv: DVector);
PROCEDURE RaddenN(n1, l1, jj1, n2, l2, jj2, J: Integer; Aosc: Float;
   rv: DVector; VAR chargev, convJM1v, convJP1v, spinJM1v, spinJP1v: DVector);
PROCEDURE RaddenU(n1, l1, jj1, n2, l2, jj2, J: Integer; Aosc: Float;
                  rv: DVector; VAR convJv, spinJv: DVector);
PROCEDURE MomdenN(n1, l1, jj1, n2, l2, jj2, J: Integer; Aosc: Float;
                  qv: DVector; VAR Coulv, Lperpv, DiagSv: DVector);
PROCEDURE MomdenU(n1, l1, jj1, n2, l2, jj2, J: Integer; Aosc: Float;
                  qv: DVector; VAR Sperpv, DiagLv: DVector);

{---------------- General Display and plotting procedures ------------------}

PROCEDURE PlotDvectorsJP(xv, yv: DVector; ibeg, iend: Integer);
PROCEDURE PlotAbsDvtr(xv, yv: DVector; ibeg, iend: Integer);
PROCEDURE PlotX(x, y : Integer);

PROCEDURE SetUpMasterMenu;
PROCEDURE SetUpHotKeys;

                              IMPLEMENTATION

FUNCTION MaxI(n1, n2: Integer): Integer;
BEGIN
   IF n1 > n2 THEN MaxI := n1
   ELSE MaxI := n2;
END;

FUNCTION MinI(n1, n2: Integer): Integer;
BEGIN
   IF n1 < n2 THEN MinI := n1
   ELSE MinI := n2;
END;

FUNCTION MaxF(x1, x2: Float): Float;
BEGIN
   IF x1 > x2 THEN MaxF := x1
   ELSE MaxF := x2;
END;

FUNCTION Even(n: Integer): Boolean;
BEGIN
   Even := ABS(n MOD 2) = 0;
END;

FUNCTION Phase(n: Integer): Integer;
BEGIN
   IF Even(n)
      THEN Phase := 1
   ELSE Phase := -1;
END;

PROCEDURE MakeFactorial;
{ Set up the factorial array
  such that f[n] contains n! }
VAR
   j : Integer;
BEGIN
   f[0] := 1;
   FOR j := 1 TO MaxFactorial DO
      f[j] := j*f[j-1];
END;

PROCEDURE MakeDoubleFac;
{ Set up the double factorial array
  such that df[n] contains n!! }
VAR
   j : Integer;
BEGIN
   df[0] := 1;  df[1] := 1;
   FOR j := 2 TO MaxDoubleFac DO
      df[j] := j*df[j-2];
END;

PROCEDURE MakeJFacts;
{ Set up the factorial array such
  that Jfac[n] contains sqrt(sqrt(n!)) }
VAR
   j : Integer;
BEGIN
   Jfac[0] := 1;
   FOR j := 1 TO JfacMax DO
      Jfac[j] := sqrt(sqrt(j))*Jfac[j-1];
END;

PROCEDURE SetupLchar;
BEGIN
   lchar[ 0] := 's';
   lchar[ 1] := 'p';
   lchar[ 2] := 'd';
   lchar[ 3] := 'f';
   lchar[ 4] := 'g';
   lchar[ 5] := 'h';
   lchar[ 6] := 'i';
   lchar[ 7] := 'j';
   lchar[ 8] := 'k';
   lchar[ 9] := 'l';
   lchar[10] := 'm';
   lchar[11] := 'n';
   lchar[12] := 'o';
   lchar[13] := 'p';
   lchar[14] := 'q';
   lchar[15] := 'r';
   lchar[16] := 's';
   lchar[17] := 't';
   lchar[18] := 'u';
   lchar[19] := 'v';
   lchar[20] := 'w';
END;

PROCEDURE SetupSinglPclArray;

   PROCEDURE SetupArray (i, n, l, j: Integer);
   BEGIN
        WITH SinglPclArray[i] DO
         BEGIN
            nsp := n;
            lsp := l;
            jjsp := j;
         END;
   END;

BEGIN
   SetupArray(1, 1, 0, 1);  { 1s 1/2 }
   SetupArray(2, 1, 1, 3);  { 1p 3/2 }
   SetupArray(3, 1, 1, 1);  { 1p 1/2 }
   SetupArray(4, 1, 2, 5);  { 1d 5/2 }
   SetupArray(5, 2, 0, 1);  { 2s 1/2 }
   SetupArray(6, 1, 2, 3);  { 1d 3/2 }
   SetupArray(7, 1, 3, 7);  { 1f 7/2 }
END;

FUNCTION ProjectileRestEnergy(ProjectileSelector: Integer): Float;
CONST
   ElectronRestEnergy = 0.511  { MeV };
   MuonRestEnergy     = 105.7  { MeV };
   TauonRestEnergy    = 1782.0 { MeV };
BEGIN
   CASE ProjectileSelector OF
      1:   ProjectileRestEnergy := ElectronRestEnergy;
      2:   ProjectileRestEnergy := MuonRestEnergy;
      3:   ProjectileRestEnergy := TauonRestEnergy;
   END;
END;

FUNCTION ProjectileName(ProjectileSelector: Integer): String;
BEGIN
   CASE ProjectileSelector OF
      1:   ProjectileName := 'Electron';
      2:   ProjectileName := 'Muon';
      3:   ProjectileName := 'Tauon';
   END;
END;

FUNCTION TargetName(TargetSelector: Integer): String;
BEGIN
   IF TargetSelector = 1 THEN TargetName := 'nucleon'
   ELSE IF TargetSelector = 2 THEN TargetName := 'deuteron'
   ELSE TargetName := Concat('A = ', NumStr(TargetSelector,0,0));
END;

FUNCTION DensityName(DensitySelector: Integer): String;
BEGIN
   CASE DensitySelector OF
      1: DensityName := 'Square';
      2: DensityName := 'Gaussian';
      3: DensityName := 'Fermi';
      4: DensityName := 'Oscillator model'
   END;
END;

FUNCTION MakeTargetString(itarg: Integer): String;
BEGIN
   CASE itarg OF
      1:  MakeTArgetString := ' 4 He';
      2:  MakeTArgetString := '12 C';
      3:  MakeTArgetString := '16 O';
      4:  MakeTArgetString := '28 Si';
      5:  MakeTArgetString := '32 S';
      6:  MakeTArgetString := '40 Ca';
   END; { CASE }
END;

FUNCTION TargetMassNumber(itarg: Integer): Integer;
BEGIN
   CASE itarg OF
      1:  TargetMassNumber := 4;
      2:  TargetMassNumber := 12;
      3:  TargetMassNumber := 16;
      4:  TargetMassNumber := 28;
      5:  TargetMassNumber := 32;
      6:  TargetMassNumber := 40;
   END; { CASE }
END;

FUNCTION Aosc(itarg: Integer): Float;
BEGIN
   CASE itarg OF
      1:  Aosc := 1.31;
      2:  Aosc := 1.64;
      3:  Aosc := 1.76;
      4:  Aosc := Pwr(28,1/6);
      5:  Aosc := Pwr(32,1/6);
      6:  Aosc := Pwr(40,1/6);
   END; { CASE }
END;

PROCEDURE MakeHoleQuantumNumbers(NumOrbit: Integer;
                  VAR nh, lh, jjh: Integer);
BEGIN
   WITH SinglPclArray[NumOrbit] DO
      BEGIN
         nh  := nsp;
         lh  := lsp;
         jjh := jjsp;
      END;
END;

FUNCTION MakeHoleString(NumOrbit: Integer): String;
VAR
   nh, lh, jjh : Integer;
BEGIN
   WITH SinglPclArray[NumOrbit] DO
      MakeHoleString := Concat(NumStr(nsp,1,0), lchar[lsp],
                          Numstr(jjsp,2,0), '/2');
END;

PROCEDURE MakeParticleQuantumNumbers(Nparticle, NumOrbit: Integer;
                  VAR np, lp, jjp: Integer);
VAR
   Nhole : Integer;
BEGIN
   jjp := 2*NumOrbit - 1;
   lp := Round((jjp - 1)/2);
   IF NOT Even(Nparticle + lp) THEN Inc(lp);
   np := Round((Nparticle - lp)/2) + 1;
END;

FUNCTION MakeParticleString(Nparticle, NumOrbit: Integer): String;
VAR
   np, lp, jjp : Integer;
BEGIN
   MakeParticleQuantumNumbers(Nparticle, NumOrbit, np, lp, jjp);
   MakeParticleString := Concat(NumStr(np,1,0), lchar[lp],
                           Numstr(jjp,2,0), '/2');
END;

FUNCTION InCore(Nparticle, NumOrbit: Integer): Boolean;
{ Determine whether proposed particle state is already occupied }
{*** USES TargetSelector ***}
VAR
   PcleString : String;
   isp, Nhole : Integer;
   IsInCore   : Boolean;
BEGIN
   IsInCore := FALSE;
   PcleString := MakeParticleString(Nparticle, NumOrbit);
   isp := TargetSelector + 1;
   REPEAT
      Dec(isp);
      IsInCore := PcleString = MakeHoleString(isp);
      WITH SinglPclArray[isp] DO Nhole := 2*(nsp - 1) + lsp;
   UNTIL IsInCore OR (Nhole < Nparticle);
   InCore := IsInCore;
END;


{ -------------------- Some Generally Useful Procedures ------------------- }

PROCEDURE OpenScrn(VAR Screen: TInputScreen);
BEGIN
END;

PROCEDURE CloseScrn(VAR Screen: TInputScreen);
BEGIN
END;

PROCEDURE OpenMenu(VAR Menu: TBarMenu);
BEGIN
   Menu.init;
END;

PROCEDURE CloseMenu(VAR Menu: TBarMenu);
BEGIN
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

PROCEDURE Request1DMatrix(VAR matrix: DMatrix; Nrow, Ncol: Integer);
BEGIN
   matrix.Init(Nrow, Ncol);
END;

PROCEDURE Release1DMatrix(VAR matrix: DMatrix);
BEGIN
   matrix.Free;
END;

PROCEDURE SetGrid(Npoints: Integer; Xstep: Float; VAR xv : DVector);
VAR
   i : Integer;
   x : Float;
BEGIN
   x := 0;
   FOR i := 1 TO Npoints DO
   BEGIN
      xv.Put(i,x);
      x := x + Xstep;
   END;
END;

FUNCTION IsInt(x: Float): Boolean;
BEGIN
   IsInt := ABS(x - Round(x)) = 0;
END;

FUNCTION Triang(a, b, c: Float): Boolean;
{ Assumes that a, b, c are all non-
  negative integers or half-integers }
BEGIN
   IF IsInt(a + b + c) AND
      NOT ((c < ABS(a - b)) OR (c > a + b))
      THEN Triang := TRUE
   ELSE Triang := FALSE;
END;

FUNCTION CLBH(a, b, c: Float): Float;
VAR
   j : Integer;
   sum, ftr, k : Float;
BEGIN
   IF IsInt(a + 0.5) AND IsInt(b + 0.5) AND IsInt(c) AND Triang(a, b, c) THEN
      BEGIN
         sum := a + b + c;
         IF Even(Round(sum)) THEN k := c ELSE k := c + 1;
         ftr := Jfac[Round((a + b + k)/2)]/
              ( Jfac[Round((a + b - k)/2)]*Jfac[Round((a + k - b - 1)/2)]
                       *Jfac[Round((b + k - a - 1)/2)] );
         ftr := ftr*Jfac[Round(a + b - c)]*Jfac[Round(a + c - b)]*
                Jfac[Round(b + c - a)]/Jfac[Round(sum + 1)]*ftr;
         ftr := 2*ftr*sqrt((2*c+1)/((2*a+1)*(2*b+1)))*ftr;
         CLBH := Phase(Round((a + b - k)/2))*ftr;
      END
   ELSE CLBH := 0;
END;

FUNCTION CJcoeff(jj1, jj2, L, J: Integer): Float;
BEGIN
   CJcoeff := 0.5*Phase(L + Round((jj1 - 1)/2))*sqrt((jj2 + 1)/pi)
                    *CLBH(0.5*jj1,0.5*jj2,J);
END;

FUNCTION YxSrme(l1, jj1, l2, jj2, L, S, J: Integer): Float;
{ Calculate reduced matrix element of [Y^L x Sigma^S]^J}
{   NOTE:  Assumes all arguments are non-negative    }
VAR
   Error        : Boolean;
   ll1, ll2     : Integer;
   g1, g2, BLSJ : Float;
BEGIN
   YxSrme := 0;
   Error := NOT (Even(l1 + l2 + L) AND Triang(l1, l2, L)
                         AND Triang(L, S, J));
   IF NOT Error THEN
   BEGIN
      ll1 := 2*l1;
      IF jj1 = ll1 + 1 THEN g1 := l1  ELSE
      IF jj1 = ll1 - 1 THEN g1 := -(l1 + 1)  ELSE Error := TRUE;
      ll2 := 2*l2;
      IF jj2 = ll2 + 1 THEN g2 := l2  ELSE
      IF jj2 = ll2 - 1 THEN g2 := -(l2 + 1)  ELSE Error := TRUE;
   END;
   IF NOT Error THEN
   BEGIN
      IF (S = 0) AND (L = J) THEN BLSJ := 1
      ELSE IF (S = 1) THEN
      BEGIN
         IF L = J - 1 THEN
            BLSJ := (g1 + g2 + J + 2)/sqrt(J*(2*J + 1))
         ELSE IF (L = J) AND (L > 0) THEN
            BLSJ := (g1 - g2)/sqrt(J*(J + 1))
         ELSE IF L = J + 1 THEN
            BLSJ := (g1 + g2 - J + 1)/sqrt(L*(2*J + 1))
         ELSE Error := TRUE;
      END
      ELSE Error := TRUE;
   END;
   IF NOT Error THEN
      BEGIN
         YxSrme := BLSJ*CJcoeff(jj1, jj2, L, J);
      END;
END;


PROCEDURE CofUU(n1, l1, n2, l2: Integer;
                VAR cnv: DVector; VAR kpmin, kpmax: Integer);
{ Calculate polynomial coefficients for the product of two
  oscillator radial functions }
VAR
   k, k1, k2 : Integer;
   num, den  : Float;
BEGIN
   kpmin := l1 + l2 + 1;
   kpmax := kpmin + 2*(n1 + n2 - 2);
   FOR k := kpmin TO kpmax DO cnv.Put(k,0);
   num := Pwr(2, n1 + l1 + n2 + l2 + 6);
   num := num*df[2*(n1+l1)-1]*df[2*(n2+l2)-1]/pi;
   num := Sqrt(num*f[n1-1]*f[n2-1]);
   FOR k1 := 0 TO n1 - 1 DO
    FOR k2 := 0 TO n2 - 1 DO
    BEGIN
       den := Pwr(2,n1+n2-k1-k2)*f[k1]*f[n1-k1-1]*f[k2]*f[n2-k2-1]
              *df[2*(k1+l1)+1]*df[2*(k2+l2)+1];
       k := kpmin + 2*(k1 + k2);
       cnv.Put(k, cnv.Value(k) + Phase(k1 + k2)*num/den);
    END;
END;

PROCEDURE CofUUP(n1, l1, n2, l2: Integer;
                 VAR cnv: DVector; VAR kpmin, kpmax: Integer);
{ Calculate polynomial coefficients for the product of an oscillator
  radial function and the derivative of an oscillator radial function }
VAR
   k, k1, k2 : Integer;
   num, den, term  : Float;
BEGIN
   kpmin := l1 + l2;
   kpmax := kpmin + 2*(n1 + n2 - 1);
   IF kpmin = 0 THEN kpmin := 2;
   FOR k := kpmin TO kpmax DO cnv.Put(k,0);
   num := Pwr(2, n1 + l1 + n2 + l2 + 6);
   num := num*df[2*(n1+l1)-1]*df[2*(n2+l2)-1]/pi;
   num := Sqrt(num*f[n1-1]*f[n2-1]);
   FOR k1 := 0 TO n1 - 1 DO
   FOR k2 := 0 TO n2 - 1 DO
   BEGIN
      den := Pwr(2,n1+n2-k1-k2)*f[k1]*f[n1-k1-1]*f[k2]*f[n2-k2-1]
             *df[2*(k1+l1)+1]*df[2*(k2+l2)+1];
      term := Phase(k1 + k2)*num/den;
      k := l1 + l2 + 2*(k1 + k2);
      IF k > 0 THEN
         cnv.Put(k, cnv.Value(k) + (l2 + 2*k2)*term);
      cnv.Put(k + 2, cnv.Value(k + 2) - term);
   END;
END;

FUNCTION ConfHyp(a, b, z: Float): Float;
CONST
   eps = 1e-8;
   MaxIter = 100;
VAR
   am, bm, sum, term, s : Float;
BEGIN
   am := a - 1;  bm := b - 1;
   sum := 1;  term := 1;
   s := 0;
   WHILE (ABS(term) > eps*ABS(sum)) AND (s <= MaxIter) DO
      BEGIN
         s := s + 1;
         term := term*(am + s)*z/((bm + s)*s);
         sum := sum + term;
      END;
   IF s >= MaxIter THEN announce('WARNING  -  ConfHyp sum not converged');
   ConfHyp := sum;
END;

PROCEDURE Qden(L, itype: Integer; Aosc: Float;
               cnv: DVector; kpmin, kpmax: Integer;
               qv: DVector; NumQ: Integer; VAR denv: DVector);
{ Evaluation of SUM( cn(kp)*I(kp) ) where
  cn(kp) contains polynomial coeffs for products of osc. radial fns,
  I(kp) = INTEGRAL( exp(-x*x)*x^(kp-itype)*jl(Aosc*q*x)*x**2 )dx  and
  qv contains a list of q-values.   Results are returned in denv. }
VAR
   iq, kp, mu, istart, ai, a : Integer;
   rati, temi, b   : Float;
   q, arg, argsq : Float;
   ratio, temp, sum : Float;
   rtwoalp, power : Float;
BEGIN
   rtwoalp := 0.5*Aosc;
   FOR iq := 1 TO NumQ DO denv.Put(iq,0);
   mu := kpmin - itype;
   istart := mu - L;
   IF NOT Even(istart) THEN
     BEGIN
        Announce('Warning  -  Parity error in Qden');
     END
   ELSE
   BEGIN
      istart := Round(istart/2);
      ai := - istart;
      rati := 1;
      temi := L + 0.5;
      b := temi + 1;
      IF istart > 0 THEN
       BEGIN
          FOR kp := 1 TO istart DO
          BEGIN
             temi := temi + 1;
             rati := rati*temi;
          END
       END
      ELSE IF istart < 0 THEN
       BEGIN
          istart := -istart;
          FOR kp := 1 TO istart DO
          BEGIN
             rati := rati*temi;
             temi := temi - 1;
          END;
          rati := 1/rati;
       END;
      FOR iq := 1 TO NumQ DO
         BEGIN
            q := qv.Value(iq);
            arg := q*rtwoalp;
            argsq := sqr(arg);
            a := ai;
            ratio := rati;
            temp := temi;
            sum := 0;
            FOR kp := kpmin TO kpmax DO
            BEGIN
               IF Even(kp - kpmin) THEN
               BEGIN
                  sum := sum
                     + cnv.Value(kp)*ratio*ConfHyp(a, b, argsq);
                  a := a - 1;
                  temp := temp + 1;
                  ratio := ratio*temp;
               END;
            END;
            power := 1;
            IF L > 0 THEN power := pwr(arg,L);
            denv.Put(iq,rootpiby4*power*exp(-argsq)*sum);
         END;
   END;
END;

FUNCTION LimCoulbyq(Aosc: Float; cnv: DVector; kpmin, kpmax: Integer): Float;
{ Evaluate q --> 0 limit of (1/q)*Coulomb momentum density }
VAR
   kp, istart, ai, a : Integer;
   rati, ratio     : Float;
   temi, temp, sum : Float;
BEGIN
   istart := kpmin - 2;
   IF NOT Even(istart) THEN
     BEGIN
        Announce('Warning  -  Parity error in LimCoulbyq');
     END
   ELSE
   BEGIN
      istart := Round(istart/2);
      ai := - istart;
      rati := 1;
      temi := 1.5;
      IF istart > 0 THEN
       BEGIN
          FOR kp := 1 TO istart DO
          BEGIN
             temi := temi + 1;
             rati := rati*temi;
          END
       END
      ELSE IF istart < 0 THEN
       BEGIN
          istart := -istart;
          FOR kp := 1 TO istart DO
          BEGIN
             rati := rati*temi;
             temi := temi - 1;
          END;
          rati := 1/rati;
       END;
      a := ai;
      ratio := rati;
      temp := temi;
      sum := 0;
      FOR kp := kpmin TO kpmax DO
      BEGIN
         IF Even(kp - kpmin) THEN
         BEGIN
            sum := sum + cnv.Value(kp)*ratio;
            a := a - 1;
            temp := temp + 1;
            ratio := ratio*temp;
         END;
      END;
      LimCoulbyq := rootpiby4*0.5*Aosc*sum;
   END;
END;

PROCEDURE MomDensity(n1, l1, jj1, n2, l2, jj2, J: Integer; Aosc: Float;
                     qv: DVector; VAR rhoQv : DVector);
VAR
   kpmin, kpmax : Integer;
   cnv  : DVector;
   angftr  : Float;
   L, S, itype : Integer;
BEGIN
   Request1DVector(cnv, NumOscCoeff);
   CofUU(n1, l1, n2, l2, cnv, kpmin, kpmax);
   L := J;  S := 0;  itype := 1;
   angftr := YxSrme(l1, jj1, l2, jj2, L, S, J);
   Qden(L, itype, Aosc, cnv, kpmin, kpmax, qv, NumQ, rhoQv);
   rhoQv.MultScalar(angftr);
   Release1DVector(cnv);
END;

PROCEDURE ElasticFormFactor(qv: DVector; VAR rhoQv : DVector);
{*** USES TargetSelector ***}
VAR
   isp, J, number : Integer;
   nh, lh, jjh : Integer;
   rhotemQv : DVector;
   root4pi : Float;
BEGIN
   Request1DVector(rhotemQv, NumQ);
   rhoQv.Fill(0);
   J := 0;
   FOR isp := 1 TO TargetSelector DO
      BEGIN
         MakeHoleQuantumNumbers(isp, nh, lh, jjh);
         number := (jjh + 1); { Number of PROTONS }
         MomDensity(nh, lh, jjh, nh, lh, jjh, J,
                    Aosc(TargetSelector), qv, rhotemQv);
         rhoQv.AddVector(number, rhotemQv);
      END;
   root4pi := Sqrt(4*pi);
   rhoQv.MultScalar(root4pi);
   Release1DVector(rhotemQv);
END;

PROCEDURE RadialDensity(n1, l1, n2, l2: Integer; aosc: Float;
                            rv: DVector; VAR rhoRv: DVector);
{ Calculate product of two oscillator radial wavefunctions }
VAR
   kp, kpmin, kpmax, i : Integer;
   rho, rhosq, sum, ftr : Float;
   norm : Float;
   cnv  : DVector;
BEGIN
   norm := 1/Pwr(aosc,3);
   Request1DVector(cnv, NumOscCoeff);
   CofUU(n1, l1, n2, l2, cnv, kpmin, kpmax);
   FOR i := 1 TO NumR DO
      BEGIN
         sum := 0;
         rho := rv.Value(i)/Aosc;
         rhosq := sqr(rho);
         ftr := Pwr(rho, kpmin - 1);
         FOR kp := kpmin TO kpmax DO
            IF Even(kp - kpmin) THEN
               BEGIN
                  sum := sum + cnv.Value(kp)*ftr;
                  ftr := ftr*rhosq;
               END;
         rhoRv.Put(i,norm*sum*exp(-rhosq));
      END;
   Release1DVector(cnv);
END;

PROCEDURE DerivDensity(n1, l1, n2, l2: Integer; aosc: Float;
                           rv: DVector; VAR drhoRv: DVector);
{ Calculate product of an oscillator wavefunction with
  the radial derivative of an oscillator wavefunction }
VAR
   kp, kpmin, kpmax, i : Integer;
   rho, rhosq, sum, ftr : Float;
   norm : Float;
   cnv  : DVector;
BEGIN
   norm := 1/Pwr(aosc,4);
   Request1DVector(cnv, NumOscCoeff);
   CofUUP(n1, l1, n2, l2, cnv, kpmin, kpmax);
   FOR i := 1 TO NumR DO
      BEGIN
         sum := 0;
         rho := rv.Value(i)/Aosc;
         rhosq := sqr(rho);
         ftr := Pwr(rho, kpmin - 1);
         FOR kp := kpmin TO kpmax DO
            IF Even(kp - kpmin) THEN
               BEGIN
                  sum := sum + cnv.Value(kp)*ftr;
                  ftr := ftr*rhosq;
               END;
         drhoRv.Put(i,norm*sum*exp(-rhosq));
      END;
   Release1DVector(cnv);
END;

PROCEDURE DerivOfProduct(n1, l1, n2, l2: Integer; aosc: Float;
                             rv: DVector; VAR rhoRv: DVector);
{ Calculate derivative of product of two oscillator radial wavefunctions }
VAR
   kp, kpmin, kpmax, i : Integer;
   rho, rhosq, sum, ftr : Float;
   norm : Float;
   cnv  : DVector;
BEGIN
   norm := 1/Pwr(aosc,4);
   Request1DVector(cnv, NumOscCoeff);
   CofUU(n1, l1, n2, l2, cnv, kpmin, kpmax);
   { Make new coefficients for the derivative }
   IF NOT (kpmin = 1) THEN
      cnv.Put(kpmin - 1, (kpmin - 1)*cnv.Value(kpmin));
   FOR kp := kpmin + 1 TO kpmax - 1 DO
      IF Even(kp - kpmin + 1) THEN
         BEGIN
            cnv.Put(kp, kp*cnv.Value(kp + 1) - 2*cnv.Value(kp - 1) );
         END;
   cnv.Put(kpmax + 1, -2*cnv.Value(kpmax));
   IF kpmin = 1 THEN Inc(kpmin) ELSE Dec(kpmin);
   Inc(kpmax);
   FOR i := 1 TO NumR DO
      BEGIN
         sum := 0;
         rho := rv.Value(i)/Aosc;
         rhosq := sqr(rho);
         ftr := Pwr(rho, kpmin - 1);
         FOR kp := kpmin TO kpmax DO
            IF Even(kp - kpmin) THEN
               BEGIN
                  sum := sum + cnv.Value(kp)*ftr;
                  ftr := ftr*rhosq;
               END;
         rhoRv.Put(i,norm*sum*exp(-rhosq));
      END;
   Release1DVector(cnv);
END;

PROCEDURE RaddenN(n1, l1, jj1, n2, l2, jj2, J: Integer; Aosc: Float;
   rv: DVector; VAR chargev, convJM1v, convJP1v, spinJM1v, spinJP1v: DVector);
{ Natural parity single-particle radial transition densities }
{ ---  rhoJ and L<>J convection and spin currents            }
VAR
   L, S, i      : Integer;
   val, temp, sftr, gftr : Float;
   rhov, Ldrhov, Rdrhov  : DVector;
   diffdrhov, sumdrhov   : DVector;
BEGIN
   chargev.Fill(0);
   convJM1v.Fill(0);
   convJP1v.Fill(0);
   spinJM1v.Fill(0);
   spinJP1v.Fill(0);
   IF Even(l1 + l2 + J) AND Triang(l1, l2, J) THEN
   BEGIN
      Request1DVector(rhov, NumR);
      RadialDensity(n1, l1, n2, l2, Aosc, rv, rhov);
      IF J > 0 THEN
         BEGIN
            Request2DVectors(Rdrhov, Ldrhov, NumR);
            DerivDensity(n1, l1, n2, l2, Aosc, rv, Rdrhov);
            DerivDensity(n2, l2, n1, l1, Aosc, rv, Ldrhov);
            Request2DVectors(diffdrhov, sumdrhov, NumR);
            diffdrhov.SumOfVectors(1, Rdrhov, -1, Ldrhov);
            sumdrhov.SumOfVectors(1, Rdrhov, 1, Ldrhov);
         END;
      L := J;  S := 0;
      val := YxSrme(l1, jj1, l2, jj2, L, S, J);
      { Calculate the charge density (if present) }
      IF val <> 0 THEN chargev.Equate(val, rhov);
      IF J > 0 THEN
         BEGIN
            rhov.Put(1,0.0);
            FOR i := 2 TO NumR DO
               rhov.Put(i,rhov.Value(i)/rv.Value(i));
            IF val <> 0 THEN
               BEGIN { Calculate the convection current densities }
                  val := val/sqrt(2*J + 1);
                  temp := l1*(l1 + 1) - l2*(l2 + 1);
                  sftr := sqrt(J);
                  gftr := -val*temp/sftr;
                  sftr := val*sftr;
                  convJM1v.SumOfVectors(sftr, diffdrhov, gftr, rhov);
                  sftr := sqrt(J + 1);
                  gftr := -val*temp/sftr;
                  sftr := -val*sftr;
                  convJP1v.SumOfVectors(sftr, diffdrhov, gftr, rhov);
               END;  { Calculate the convection current densities }
            S := 1;
            val := YxSrme(l1, jj1, l2, jj2, L, S, J);
            IF val <> 0 THEN
               BEGIN { Calculate the spin current densities }
                  val := 0.5*val/sqrt(2*J + 1);
                  sftr := sqrt(J + 1)*val;
                  gftr := sftr*(J + 1);
                  spinJM1v.SumOfVectors(sftr, sumdrhov, gftr, rhov);
                  sftr := sqrt(J)*val;
                  gftr := -sftr*J;
                  spinJP1v.SumOfVectors(sftr, sumdrhov, gftr, rhov);
               END;  { Calculate the spin current densities }
            Release2DVectors(diffdrhov, sumdrhov);
            Release2DVectors(Rdrhov, Ldrhov);
         END;
      Release1DVector(rhov);
   END
   ELSE Writeln('No transition densities found');
END;

PROCEDURE RaddenU(n1, l1, jj1, n2, l2, jj2, J: Integer; Aosc: Float;
                  rv: DVector; VAR convJv, spinJv: DVector);
{ Unnatural parity single-particle radial transition densities }
{ ---  L=J convection and spin currents                        }
VAR
   L, S, i : Integer;
   val, ftr, ftrp   : Float;
   sftr, gam1, gam2 : Float;
   rhov, drhov : DVector;
BEGIN
   convJv.Fill(0);
   spinJv.Fill(0);
   IF (NOT Even(l1 + l2 + J)) AND (J > 0) THEN
   BEGIN
      S := 1;
      L := J - 1;
      val := CJcoeff(jj1, jj2, L, J);
      IF val <> 0 THEN
      BEGIN
         Request2DVectors(rhov, drhov, NumR);
         RadialDensity(n1, l1, n2, l2, Aosc, rv, rhov);
         rhov.Put(1,0.0);
         FOR i := 2 TO NumR DO
            rhov.Put(i,rhov.Value(i)/rv.Value(i));
         DerivOfProduct(n1, l1, n2, l2, Aosc, rv, drhov);
         sftr := sqrt(J*(J + 1));
         IF jj1 > 2*l1 THEN gam1 := l1 ELSE gam1 := -(l1 + 1);
         IF jj2 > 2*l2 THEN gam2 := l2 ELSE gam2 := -(l2 + 1);
         { Calculate the Diag-L transition density }
         ftr := (gam1 + gam2 + 1 - J)*(gam1 + gam2 + 2 + J);
         ftr := -ftr*val/sftr;
         convJv.Equate(ftr,rhov);
         { Calculate the S-perp transition density }
         ftr := 0.5*val*(gam1 + gam2 + 2 - J*(J + 1))/sftr;
         ftrp := 0.5*val*(gam1 + gam2 + 2)/sftr;
         spinJv.SumOfVectors(ftr, rhov, ftrp, drhov);
         Release2DVectors(rhov, drhov);
      END;
   END
   ELSE Writeln('No transition densities found');
END;

PROCEDURE MomdenN(n1, l1, jj1, n2, l2, jj2, J: Integer; Aosc: Float;
                  qv: DVector; VAR Coulv, Lperpv, DiagSv: DVector);
{ Natural parity momentum densities }
{ ---  Coulomb, L-perp and Diag-S   }
VAR
   L, S, i, DeltaN : Integer;
   kpmin, kpmax : Integer;
   val, sftr, gftr  : Float;
   ftrL, ftrR{, temp} : Float;
   gam1, gam2 : Float;
   cnv  : DVector;
   denJv, derivRv, derivLv : DVector;
BEGIN
   Coulv.Fill(0);
   Lperpv.Fill(0);
   DiagSv.Fill(0);
   IF Even(l1 + l2 + J) AND Triang(l1, l2, J) THEN
   BEGIN
      L := J;
      Request1DVector(cnv, NumOscCoeff);
      Request1DVector(denJv, NumQ);
      CofUU(n1, l1, n2, l2, cnv, kpmin, kpmax);
      Qden(J, 1, Aosc, cnv, kpmin, kpmax, qv, NumQ, denJv);
      S := 0;
      val := YxSrme(l1, jj1, l2, jj2, L, S, J);
      IF val <> 0 THEN
      BEGIN
         { Calculate the Coulomb transition density }
         Coulv.Equate(val, denJv);
         IF J > 0 THEN
         BEGIN
            { Calculate the L-perp transition density }
(*            Specific evaluation for q = 0                            *)
(*            IF J = 1 THEN                                            *)
(*               BEGIN                                                 *)
(*                  DeltaN := 2*(n1 - n2) + l1 - l2;                   *)
(*                  temp := -Sqrt(2)*(DeltaN/Sqr(Aosc))                *)
(*                           *val*LimCoulbyq(Aosc, cnv, kpmin, kpmax); *)
(*               END ELSE temp := 0;                                   *)
            sftr := sqrt(J*(J + 1));
            IF jj1 > 2*l1 THEN gam1 := l1 ELSE gam1 := -(l1 + 1);
            IF jj2 > 2*l2 THEN gam2 := l2 ELSE gam2 := -(l2 + 1);
            gftr := (gam1 - gam2)*(gam1 + gam2 + 1)/sftr;
            ftrL := (gftr - sftr)*val/2;
            ftrR := (gftr + sftr)*val/2;
            Request2DVectors(derivLv, derivRv, NumQ);
            CofUUP(n2, l2, n1, l1, cnv, kpmin, kpmax);
            Qden(J, 2, Aosc, cnv, kpmin, kpmax, qv, NumQ, derivLv);
            CofUUP(n1, l1, n2, l2, cnv, kpmin, kpmax);
            Qden(J, 2, Aosc, cnv, kpmin, kpmax, qv, NumQ, derivRv);
            Lperpv.SumOfVectors(ftrL, derivLv, ftrR, derivRv);
(*            Lperpv.Put(1,temp);                                      *)
            FOR i := 1 TO NumQ DO
               Lperpv.Put(i,Lperpv.Value(i)/sqr(Aosc*qv.Value(i)));
            Release2DVectors(derivLv, derivRv);
         END;
      END;
      IF J > 0 THEN
      BEGIN
         S := 1;
         val := YxSrme(l1, jj1, l2, jj2, L, S, J);
         IF val <> 0 THEN
         BEGIN
            { Calculate the Diag-S transition density }
            DiagSv.Equate(val, denJv);
         END;
      END;
      Release1DVector(denJv);
      Release1DVector(cnv);
   END
   ELSE Writeln('No transition densities found');
END;

PROCEDURE MomdenU(n1, l1, jj1, n2, l2, jj2, J: Integer; Aosc: Float;
                  qv: DVector; VAR Sperpv, DiagLv: DVector);
{ Unnatural parity momentum densities }
{ ---  S-perp and Diag-L              }
VAR
   L, S : Integer;
   kpmin, kpmax : Integer;
   valm, valp : Float;
   ftrM, ftrP : Float;
   sftr, gam1, gam2 : Float;
   cnv  : DVector;
   denJM1v, denJP1v : DVector;
BEGIN
   Sperpv.Fill(0);
   DiagLv.Fill(0);
   IF (NOT Even(l1 + l2 + J)) AND (J > 0) THEN
   BEGIN
      S := 1;
      L := J - 1;
      valm := 0;
      IF Triang(l1, l2, L) THEN
         BEGIN
            valm := YxSrme(l1, jj1, l2, jj2, L, S, J);
         END;
      L := J + 1;
      valp := 0;
      IF Triang(l1, l2, L) THEN
      BEGIN
         valp := YxSrme(l1, jj1, l2, jj2, L, S, J);
      END;
      IF (valm <> 0) OR (valp <> 0) THEN
      BEGIN
         Request1DVector(cnv, NumOscCoeff);
         Request2DVectors(denJM1v, denJP1v, NumQ);
         CofUU(n1, l1, n2, l2, cnv, kpmin, kpmax);
         IF (valm <> 0) THEN
            Qden(J-1, 1, Aosc, cnv, kpmin, kpmax, qv, NumQ, denJM1v);
         IF (valp <> 0) THEN
            Qden(J+1, 1, Aosc, cnv, kpmin, kpmax, qv, NumQ, denJP1v);
         { Calculate the Diag-L transition density }
         sftr := sqrt(J*(J + 1));
         IF jj1 > 2*l1 THEN gam1 := l1 ELSE gam1 := -(l1 + 1);
         IF jj2 > 2*l2 THEN gam2 := l2 ELSE gam2 := -(l2 + 1);
         ftrM := (gam1 + gam2 + 1 - J)*valm/2;
         ftrP := (gam1 + gam2 + 2 + J)*valp/2;
         ftrM := -sqrt((  J  )/(2*J + 1))*ftrM/sftr;
         ftrP := -sqrt((J + 1)/(2*J + 1))*ftrP/sftr;
         DiagLv.SumOfVectors(ftrM, denJM1v, ftrP, denJP1v);
         { Calculate the S-perp transition density }
         ftrM :=  sqrt((J + 1)/(2*J + 1))*valm;
         ftrP := -sqrt((  J  )/(2*J + 1))*valp;
         Sperpv.SumOfVectors(ftrM, denJM1v, ftrP, denJP1v);
         Release2DVectors(denJM1v, denJP1v);
         Release1DVector(cnv);
      END;
   END
   ELSE Writeln('No transition densities found');
END;


{---------------- General Display and plotting procedures ------------------}

PROCEDURE PlotDvectorsJP(xv, yv: DVector; ibeg, iend: Integer);
{ Plots subset of DVector elements either forwards or backwards }
VAR
   i1, i2, di, ii, i, x1, y1 : Integer;
BEGIN
   IF ibeg <> iend THEN
   BEGIN
      IF scaleNumber=0 THEN autoScaling(0,xv,yv);
      IF ibeg < iend THEN BEGIN i1:=ibeg; i2:=iend; di:= 1 END
      ELSE                BEGIN i1:=iend; i2:=ibeg; di:=-1 END;
         i := ibeg;
         Map(xv.Value(i),yv.Value(i),x1,y1);
         HideMouse;
         MoveTo(x1,y1);
         FOR ii := i1 + 1 TO i2 DO
         BEGIN
            i := i + di;
            Map(xv.Value(i),yv.Value(i),x1,y1);
            LineTo(x1,y1);
         END;
         ShowMouse;
      END;
END;

PROCEDURE PlotAbsDvtr(xv, yv: DVector; ibeg, iend: Integer);
{ Plots subset of DVector elements either forwards or backwards }
VAR
   i1, i2, di, ii, i, x1, y1 : Integer;
BEGIN
   IF ibeg <> iend THEN
   BEGIN
      IF scaleNumber=0 THEN autoScaling(0,xv,yv);
      IF ibeg < iend THEN BEGIN i1:=ibeg; i2:=iend; di:= 1 END
      ELSE                BEGIN i1:=iend; i2:=ibeg; di:=-1 END;
         i := ibeg;
         Map(xv.Value(i),ABS(yv.Value(i)),x1,y1);
         HideMouse;
         MoveTo(x1,y1);
         FOR ii := i1 + 1 TO i2 DO
         BEGIN
            i := i + di;
            Map(xv.Value(i),ABS(yv.Value(i)),x1,y1);
            LineTo(x1,y1);
         END;
         ShowMouse;
      END;
END;

PROCEDURE PlotX(x, y : Integer);
CONST
   l = 3;
BEGIN
   SetLineStyle(SolidLn,0,3);
   moveTo (x-l,y-l); lineRel(2*l,2*l);
   moveTo (x-l,y+l); lineRel(2*l,-2*l);
   SetLineStyle(SolidLn,0,1);
END;

PROCEDURE SetUpMasterMenu;
BEGIN
   WITH MasterMenu DO
   BEGIN
      init;
      column(1, 'File');
         row(1,1, 'About CUPS');
         row(1,2, 'About Program');
         row(1,3, 'Configuration');
         row(1,4, '-------------');
         row(1,5, 'Exit Program');
      column(2, 'Kinematics');
         row(2,1, 'About Kinematics');
         row(2,2, 'Choose Parameters');
         row(2,3, 'Transfer Parameters');
         row(2,4, 'Lorentz Scalars');
      column(3, 'Elastic');
         row(3,1, 'About Elastic Scattering');
         row(3,2, 'Coulomb Scattering');
      column(4, 'Inelastic');
         row(4,1, 'About Inelastic Scattering');
         row(4,2, 'Choose Type of Excitation');
         row(4,3, 'Choose Hole State');
         row(4,4, 'Choose Particle State');
         row(4,5, 'Choose Spin');
         row(4,6, 'Numerical results');
      column(5, 'Response');
         row(5,1, 'About Coulomb Response');
         row(5,2, 'Choose q and Target');
         row(5,3, 'Fixed q response');
      ColSize := 11;
      rowactivate(1,4,false);
      display;
   END;
END;

PROCEDURE SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      init(2);
      key[1] := 'F1-Help';
      key[2] := 'F10-Menu';
      display;
   END;
END;

END.  { SUPPORT }

{ ************************************************************************** }
