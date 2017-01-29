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


UNIT DEUTCALC;
{$N+}
                    {*********************************}
                    {*    Two-Nucleon Simulation     *}
                    {*         DEUTCALC.PAS          *}
                    {*   Written by John Philpott    *}
                    {*        July XX, 1993.         *}
                    {*   Copyright (c) 1992, 1993    *}
                    {*********************************}
 
INTERFACE
 
USES
      crt, 
      graph, CUPS, CUPSmupp, CUPSfunc,
      CUPSproc, CUPSgui, CUPSgrph, Consts, Master;

PROCEDURE Deuteron;

IMPLEMENTATION
 
PROCEDURE Deuteron;

{ ---------------------- Wavefunction Calculations ------------------------ }

PROCEDURE MakeZeroEnergyWaveFnctn{ For s + d-wave deuteron }
          (Nmax: Integer; xv: DVector; VAR uv1, wv1: DVector;
           VAR Slope: Float);
 
   PROCEDURE GetLinComb(R: Float; yv: DVarray;
             VAR Aone, Atwo, Bone, Btwo: Float);

   CONST
      n = 4;
   VAR
      amat     : DMatrix;
      bvec     : DVector;
      indx     : IDVector;
      i        : Integer;
      d        : Real;
      rm2, rm3 : Float;
 
   BEGIN
      Request1DMatrix(amat, n, n);
      Request1DVector(bvec, n);
      Request1IDVector(indx, n);
   { Set up matrix and vector of boundary values }
      rm2 := 1/sqr(R);  rm3 := rm2/R;
      WITH amat DO
         BEGIN

            Put(1, 1,  yv[1].Value(1));  Put(1, 2,  yv[2].Value(1));
            Put(2, 1,  yv[1].Value(2));  Put(2, 2,  yv[2].Value(2));
            Put(3, 1, -yv[1].Value(3));  Put(3, 2, -yv[2].Value(3));
            Put(4, 1, -yv[1].Value(4));  Put(4, 2, -yv[2].Value(4));

            Put(1, 3, -R);               Put(3, 4, -rm2);
            Put(2, 3, -1);               Put(4, 4,  2*rm3);

         END; { WITH amat DO }
      bvec.Put(1, 1);
   { Solve linear equations to match the ingoing and outgoing functions }
      LUdecomp(amat, d, indx);
      LUsolve(amat, bvec, indx);
      Aone:=bvec.Value(1);  Atwo:=bvec.Value(2);
      Bone:=bvec.Value(3);  Btwo:=bvec.Value(4);
      Release1IDVector(indx);
      Release1DVector(bvec);
      Release1DMatrix(amat);
   END;
 
VAR
   ysave      : DVarray;
   uv2, wv2   : DVector;
   j          : Integer;
   Aone, Atwo : Float;
   Dnorm      : Float;
BEGIN { MakeZeroEnergyWaveFnctn }

   CapJ := 1;
   Request2DVectors(ysave[1], ysave[2], 4);
   Request2DVectors(ysave[3], ysave[4], 4);
   Request2DVectors(uv2, wv2, Npoints);
 
{ Create two linearly independent solutions
  from the origin to the maximum radius }
   IF ThisPotl.Form = HJ THEN
      BEGIN
         PotlHJ := TE_CONSTS;
         CoupWavefunction(0.0, 1, Nmax, Nmax, 1, DerivsCoupHJ,
                              xv, uv1, wv1, ysave[1]);
         CoupWavefunction(0.0, 1, Nmax, Nmax, 2, DerivsCoupHJ,
                              xv, uv2, wv2, ysave[2]);
      END
   ELSE
      BEGIN
         TripletPotl := ThisPotl.TE_Consts;
         SetOPEPStrengths(TEvn, VcOPEP, VtOPEP);
         CoupWavefunction(0.0, 1, Nmax, Nmax, 1, DerivsCoup,
                              xv, uv1, wv1, ysave[1]);
         CoupWavefunction(0.0, 1, Nmax, Nmax, 2, DerivsCoup,
                              xv, uv2, wv2, ysave[2]);
      END;

{ Form the appropriate linear combination }
   GetLinComb(xv.Value(Nmax), ysave, Aone, Atwo, Slope, Dnorm);
   FOR j := 1 TO Nmax DO
      BEGIN
         uv1.put(j,Aone*uv1.Value(j) + Atwo*uv2.Value(j));
         wv1.put(j,Aone*wv1.Value(j) + Atwo*wv2.Value(j));
      END;
 
   Release2DVectors(uv2, wv2);
   Release2DVectors(ysave[3], ysave[4]);
   Release2DVectors(ysave[1], ysave[2]);
END;  { MakeZeroEnergyWaveFnctn }


PROCEDURE ZeroEgyNorm{ For s + d-wave deuteron }
                 (Nmax: Integer;
                 xv: DVector; VAR uv1, wv1: DVector;
                 VAR ScattLength, ScattRange: Float;
                 VAR NumBoundStates: Integer);
VAR
   slope : Float;
   Wint  : Float;
BEGIN
{ Calculate the wavefunction }
   MakeZeroEnergyWaveFnctn(Nmax, xv, uv1, wv1, slope);
{ Determine scattering length, effective range and number of nodes }
   ScattLength := -1/slope;
   Wint := NormInt(Rstep, 1, Nmax, wv1) +
              xv.Value(Nmax)*sqr(wv1.Value(Nmax))/3;
   ScattRange := 2.0*(LineInt(slope, 1.0, xv.Value(Nmax))
                      - LineInt(slope, 1.0, 0.0)
                      - NormInt(Rstep, 1, Nmax, uv1) - Wint);
   NumBoundStates := Nodes(uv1, 1, Nmax);
   IF (uv1.Value(Nmax-1)*uv1.Value(Nmax) >= 0) AND
      (ScattLength > Rmax - Rstep/2) THEN Inc(NumBoundStates);
END;
 

PROCEDURE MakeApproxBoundWaveFnctn{ For s + d-wave deuteron }
          (BE: Float; Nmax, Nmatch: Integer;
           VAR DerivLeft, DerivRight, Dprob: Float;
           xv: DVector; VAR uv1, wv1, uv3, wv3: DVector);
{ Calculate overlapping left and right branches of the wavefunction }
 
   Procedure GetLinComb(yv: DVarray; VAR Bone, Btwo, Atwo: Float);
   CONST
      n = 3;
   VAR
      amat : DMatrix;
      bvec : DVector;
      indx : IDVector;
      i    : Integer;
      d    : Real;
   BEGIN
      Request1DMatrix(amat, n, n);
      Request1DVector(bvec, n);
      Request1IDVector(indx, n);
      { Set up matrix and vector of boundary values }
      WITH amat DO
         BEGIN
            Put(1, 1, yv[3].Value(1));
            Put(2, 1, yv[3].Value(3));
            Put(3, 1, yv[3].Value(4));
            Put(1, 2, yv[4].Value(1));
            Put(2, 2, yv[4].Value(3));
            Put(3, 2, yv[4].Value(4));
            Put(1, 3, -yv[2].Value(1));
            Put(2, 3, -yv[2].Value(3));
            Put(3, 3, -yv[2].Value(4));
         END;
      bvec.Put(1, yv[1].Value(1));
      bvec.Put(2, yv[1].Value(3));
      bvec.Put(3, yv[1].Value(4));
{ Solve linear equations to match the ingoing and outgoing functions }
      LUdecomp(amat, d, indx);
      LUsolve(amat, bvec, indx);
      Bone := bvec.Value(1);
      Btwo := bvec.Value(2);
      Atwo := bvec.Value(3);
      Release1IDVector(indx);
      Release1DVector(bvec);
      Release1DMatrix(amat);
   END;

CONST
   overlap = 1.0; { fm }
VAR
   ysave                  : DVarray;
   uv2, wv2, uv4, wv4     : DVector;
   NmaxLeft, NminRight, j : Integer;
   Aone, Atwo, Bone, Btwo : Float;
   Lderiv, Rderiv         : Float;
   Vint, Wint, Normftr    : Float;
BEGIN

   CapJ := 1;
   Request2DVectors(ysave[1], ysave[2], 4);
   Request2DVectors(ysave[3], ysave[4], 4);
   Request2DVectors(uv2, wv2, Npoints);
   Request2DVectors(uv4, wv4, Npoints);
 
{ Create two linearly independent solutions
  from the origin to beyond the matching radius }
   NmaxLeft := Geti(ThisPotl.Core, Rmatch + overlap,Nmax);
   IF ThisPotl.Form = HJ THEN
      BEGIN
         PotlHJ := TE_CONSTS;
         CoupWavefunction(-BE, 1, NmaxLeft, Nmatch, 1, DerivsCoupHJ,
                              xv, uv1, wv1, ysave[1]);
         CoupWavefunction(-BE, 1, NmaxLeft, Nmatch, 2, DerivsCoupHJ,
                              xv, uv2, wv2, ysave[2]);
      END
   ELSE
      BEGIN
         TripletPotl := ThisPotl.TE_Consts;
         SetOPEPStrengths(TEvn, VcOPEP, VtOPEP);
         CoupWavefunction(-BE, 1, NmaxLeft, Nmatch, 1, DerivsCoup,
                              xv, uv1, wv1, ysave[1]);
         CoupWavefunction(-BE, 1, NmaxLeft, Nmatch, 2, DerivsCoup,
                              xv, uv2, wv2, ysave[2]);
      END;
{ Create two linearly independent solutions from
  the outer radius to beyond the matching radius }
   NminRight := Geti(ThisPotl.Core, Rmatch - overlap,Nmax);
   IF ThisPotl.Form = HJ THEN
      BEGIN
         CoupWavefunction(-BE, Nmax, NminRight, Nmatch, 1, DerivsCoupHJ,
                              xv, uv3, wv3, ysave[3]);
         CoupWavefunction(-BE, Nmax, NminRight, Nmatch, 2, DerivsCoupHJ,
                              xv, uv4, wv4, ysave[4]);
      END
   ELSE
      BEGIN
         CoupWavefunction(-BE, Nmax, NminRight, Nmatch, 1, DerivsCoup,
                              xv, uv3, wv3, ysave[3]);
         CoupWavefunction(-BE, Nmax, NminRight, Nmatch, 2, DerivsCoup,
                              xv, uv4, wv4, ysave[4]);
      END;
{ Form the appropriate linear combinations }
   Aone := 1;
   GetLinComb(ysave, Bone, Btwo, Atwo);
   FOR j := 1 TO NmaxLeft DO
      BEGIN
         uv1.put(j,Aone*uv1.Value(j) + Atwo*uv2.Value(j));
         wv1.put(j,Aone*wv1.Value(j) + Atwo*wv2.Value(j));
      END;
   Lderiv := Aone*ysave[1].Value(2) + Atwo*ysave[2].Value(2);
   FOR j := Nmax DOWNTO NminRight DO
      BEGIN
         uv3.put(j,Bone*uv3.Value(j) + Btwo*uv4.Value(j));
         wv3.put(j,Bone*wv3.Value(j) + Btwo*wv4.Value(j));
      END;
   Rderiv := Bone*ysave[3].Value(2) + Btwo*ysave[4].Value(2);
{ Normalize the total wavefunction }
   Vint := NormInt(Rstep, 1, Nmatch, uv1) +
           NormInt(Rstep, Nmatch, Nmax, uv3) +
           SWaveTailInt(BE, Nmax, uv3);
   Wint := NormInt(Rstep, 1, Nmatch, wv1) +
           NormInt(Rstep, Nmatch, Nmax, wv3) +
           DWaveTailInt(BE, Nmax, xv, wv3);
   Normftr := 1/(Vint + Wint);
   Dprob   := Normftr*Wint;
   Normftr := sqrt(Normftr);
   FOR j := 1 TO NmaxLeft DO
      BEGIN
         uv1.put(j,Normftr*uv1.Value(j));
         wv1.put(j,Normftr*wv1.Value(j));
      END;
   DerivLeft := Normftr*Lderiv;
   FOR j := Nmax DOWNTO NminRight DO
      BEGIN
         uv3.put(j,Normftr*uv3.Value(j));
         wv3.put(j,Normftr*wv3.Value(j));
      END;
   DerivRight := Normftr*Rderiv;
 
   Release2DVectors(uv4, wv4);
   Release2DVectors(uv2, wv2);
   Release2DVectors(ysave[3], ysave[4]);
   Release2DVectors(ysave[1], ysave[2]);
END;
 
PROCEDURE AnalyzeApproxBoundWaveFnctn{ For s + d-wave deuteron }
          (uv1, uv2: DVector; Nmax, Nmatch: Integer;
           DerivLeft, DerivRight: Float;
           VAR DeltaBE: Float; VAR NodeCount: Integer);
BEGIN
{ Calculate the estimated change needed in BE }
   DeltaBE := uv1.Value(Nmatch)*(DerivRight - DerivLeft)/muftr;
{ Determine the node count }
   NodeCount := Nodes(uv1, 1, Nmatch);
   NodeCount := NodeCount + Nodes(uv2, Nmatch, Nmax);
   IF uv2.value(Nmatch)*DerivRight > uv1.Value(Nmatch)*DerivLeft
      THEN Inc(NodeCount);
END;
 

{ -------------------------- Interface Routines --------------------------- }

PROCEDURE ShowResultsScreen(VAR ScattLength, ScattRange: Float;
                            VAR NumBoundStates: Integer);
var
   str : string;
BEGIN
   DefineViewPort(10, 0.0, 0.35, 0.13, 0.93);
   OpenViewPort(10);
   Print(1,01,' POTENTIAL PARAMETERS:');
   WITH ThisPotl DO BEGIN WITH TE_Consts DO BEGIN
   IF Form = HJ THEN
     BEGIN
       Print(1,04,'     HAMADA-JOHNSTON');
       Print(1,05,'      S + D channel');
       Print(1,06,'       Potentials');
     END
   ELSE
     BEGIN
       IF OPEP THEN
          Print(1,03,Concat(' OPEP-Yes   Core', NumStr(Core,7,3), ' fm'))
       ELSE
          Print(1,03,Concat(' OPEP--No   Core', NumStr(Core,7,3), ' fm'));
       Print(1,05,'            Depth    Range');
       Print(1,06,'            (MeV)    (fm)');
       if CNdepth<1e5 then str:=NumStr(CNdepth,10,2)
       else str:='  '+ScNumStr(CNdepth,4);
       Print(1,07,Concat(' Centrl ', Str, NumStr(CNrange,8,3)));
       Print(1,08,Concat(' Spin-O ', NumStr(LSdepth,10,2), NumStr(LSrange,8,3)));
       Print(1,09,Concat(' Tensor ', NumStr(TNdepth,10,2), NumStr(TNrange,8,3)));
     END;
   END; { WITH ThisPotl  }  END; { WITH TE_Consts }
   SetColor(Yellow);
   Print(1,11,' CALCULATED RESULTS:');
   Print(1,13,Concat(' Scatt Length:', NumStr(ScattLength,7,2), ' fm'));
   Print(1,14,Concat(' Eff. Range:  ', NumStr(ScattRange,7,2), ' fm'));
   Print(1,16,Concat('    # bound states ', NumStr(NumBoundStates,2,0)));
   SetColor(Whitte);
END;

CONST
   potlmin = -250;
   potlmax =   50;

PROCEDURE PlotSDPotls(xv: DVector; Nmax: Integer);
VAR
   index, i        : Integer;
   yv1, yv2, yv3   : DVector;
   x, Yukawa, Cent : Float;
   Tens, SpinOrb   : Float;
   root8, QspinOrb : Float;
   Vpotl, Wpotl    : Float;
BEGIN
   SelectViewPort(1);
   SelectScale(1);
   SetColor(LightGreen);
   Print(9,1,'Potential versus Radius');
   Request3DVectors(yv1, yv2, yv3, Nmax);
   root8 := sqrt(8);
   FOR i := 1 TO Nmax DO
      BEGIN
         IF thisPotl.Form = HJ THEN
            BEGIN
               x := xv.Value(i)/pionCompLen;
               Yukawa := Yuk(x);
               Cent := Vc(x,Yukawa);
               Tens := Vt(x,Yukawa);
               SpinOrb := VlsHJ(x,Yukawa);
               QSpinOrb := Vll(x,Yukawa);
            END
         ELSE
            BEGIN
               x := xv.Value(i);
               Cent := VcTrip(x);
               Tens := VTens(x);
               SpinOrb := Vls(x);
               QSpinOrb := 0;
            END;
         Vpotl := Cent;
         Wpotl := Cent - 3*SpinOrb - 2*Tens - 3*QSpinOrb;
         yv1.Put(i,Vpotl);
         yv2.Put(i,Wpotl);
         yv3.Put(i,root8*Tens);
      END;
   IF ThisPotl.Core > 0 THEN
      PlotLine(ThisPotl.Core, potlmin, ThisPotl.Core, potlmax);
   Print(12,5,'S-Wave Potential');
   PlotDvtr(xv, yv1, 1, Nmax);
   SetColor(LightRed);
   Print(12,6,'D-Wave Potential');
   PlotDvtr(xv, yv2, 1, Nmax);
   SetColor(whitte);
   Print(12,7,'Coupling Potential');
   PlotDvtr(xv, yv3, 1, Nmax);
   Release3DVectors(yv1, yv2, yv3);
END;
 
PROCEDURE ZeroEnergy(Nmax: Integer; xv: DVector);{ For s + d-wave deuteron }
VAR
   {Rmax,} BE    : Float;
   ymin, ymax  : Float;
   uv1, wv1    : DVector;
   ScattLength : Float;
   ScattRange  : Float;
   NumBoundStates : Integer;
BEGIN
   Request2DVectors(uv1, wv1, Npoints);
   ZeroEgyNorm(Nmax, xv, uv1, wv1, ScattLength, ScattRange, NumBoundStates);
   ShowResultsScreen(ScattLength,ScattRange,NumBoundStates);
   PreparePotlPlot(potlmin, potlmax);
   PlotSDPotls(xv, Nmax);
   ymax := 1.2*MaxF(1.0,ABS(uv1.Value(Nmax)));
   ymin := -ymax;
   PrepareWaveFuncPlot(ymin, ymax);
   SetColor(Yellow);
   Print(9,1,'Zero Energy Wavefunction');
   PlotDvtr(xv, uv1, 1, Nmax);
   SetColor(LightRed);
   PlotDvtr(xv, wv1, 1, Nmax);
   SetColor(whitte);
   PlotLine(0.0, 1.0, xv.Value(Nmax), uv1.Value(Nmax));
   SelectViewPort(10);
   IF NumBoundStates = 1 THEN
      BEGIN
         EstimateBE(ScattLength, ScattRange, ThisPotl.Core, BE);
         IF BE > 0 THEN
            BEGIN
               Print(3,17,'Effective range theory');
               Print(3,18,'predicts a bound state');
               Print(3,19,Concat('near BE =', NumStr(BE,9,3), ' MeV'));
            END
         ELSE
            BEGIN
               Print(3,17,'No useful estimate for');
               Print(3,18,'the bound state energy');
               Print(3,19,'found.');
            END;
      END;
   SetColor(whitte);
   Release2DVectors(uv1, wv1);
END;
 
PROCEDURE LoadNumbers(VAR ViewScreen: TInputScreen; ThisPotl: PotlConsts);
CONST
   MinRange =    0;   MaxRange = 3.5;
   MinPotl  = -1e7;   MaxPotl  = 1e7;
BEGIN
   with ViewScreen do
   WITH ThisPotl DO
   BEGIN
      SetNumber(1,Core);
      SetNumberLimits(1,0.1,2.0);
      WITH TE_Consts DO
      BEGIN
         SetNumber( 2,CNdepth);
         SetNumber( 3,CNrange);
         SetNumberLimits(2,MinPotl,MaxPotl);
         SetNumberLimits(3,MinRange,MaxRange);
         SetNumber(4,LSdepth);
         SetNumber(5,LSrange);
         SetNumberLimits(4,MinPotl,MaxPotl);
         SetNumberLimits(5,MinRange,MaxRange);
         SetNumber(6,TNdepth);
         SetNumber(7,TNrange);
         SetNumberLimits(6,MinPotl,MaxPotl);
         SetNumberLimits(7,MinRange,MaxRange);
      END;
   END; { WITH ThisPotl }
END;

PROCEDURE GetNumbers(ViewScreen: TInputScreen; VAR ThisPotl: PotlConsts);
BEGIN
   with ViewScreen do
   WITH ThisPotl DO
      BEGIN
         Core := GetNumber(1);
         WITH TE_Consts DO
         BEGIN
            CNdepth := GetNumber(2);
            CNrange := GetNumber(3);
            LSdepth := GetNumber(4);
            LSrange := GetNumber(5);
            TNdepth := GetNumber(6);
            TNrange := GetNumber(7);
         END;
   END; { WITH ThisPotl }
END;
 
FUNCTION GetNewPotlParams: boolean;
VAR
   tempPotl   : PotlConsts;
   ViewScreen : TInputScreen;
BEGIN
   OpenScrn(ViewScreen);
   WITH ThisPotl DO
   WITH ViewScreen DO
   BEGIN
      init;
      DefineInputport(0.1, 0.9, 0.09, 0.91); 
      LoadLine( Center('VIEW AND ADJUST THE INTERACTION PARAMETERS') );
      LoadLine('');
      LoadLine( Center(Title) );
      IF OPEP THEN
         LoadLine( Center('(This interaction includes OPEP)') )
      ELSE
         LoadLine( Center('(This interaction does not include OPEP)') );
      LoadLine('');
      LoadLine('               Hard core radius = {   0} fm          ');
      LoadLine('');
   LoadLine('                               EVEN Parity              ');
   LoadLine('                             Depth     Range            ');
   LoadLine('                             (MeV)     (fm)             ');
   LoadLine('                 Central    {      0}  {    0}          ');
   LoadLine('                 Spin-Orb   {      0}  {    0}          ');
   LoadLine('                 Tensor     {      0}  {    0}          ');
      LoadLine('');
      LoadLine('        NOTE:  Any changes made here will be lost'    );
      LoadLine('               if you re-select an interaction.      ');
      LoadLine('               Use Help for info on valid entries.   ');
      LoadLine('');
      LoadLine('              [  Ok  ]     [Cancel]     [ Help ]     ');
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

PROCEDURE FindWaveFnctnScale(uv1, wv1, uv2, wv2: DVector;
                             Nmatch, Nmax: Integer;
                             VAR ymin, ymax: Real);
{ Find min and max of coupled wavefunction }
VAR
   i : Integer;
   temp : Float;
BEGIN
   ymin := 0;  ymax := 0;
   FOR i := 1 TO Nmatch DO
      BEGIN
         temp := uv1.Value(i);
         IF ymin > temp THEN ymin := temp;
         IF ymax < temp THEN ymax := temp;
         temp := wv1.Value(i);
         IF ymin > temp THEN ymin := temp;
         IF ymax < temp THEN ymax := temp;
      END;
   FOR i := Nmatch TO Nmax DO
      BEGIN
         temp := uv2.Value(i);
         IF ymin > temp THEN ymin := temp;
         IF ymax < temp THEN ymax := temp;
         temp := wv2.Value(i);
         IF ymin > temp THEN ymin := temp;
         IF ymax < temp THEN ymax := temp;
      END;
END;
 
PROCEDURE PlotWavefunction(xv, uv1, wv1, uv2, wv2: DVector;
                           Nmatch, Nmax: Integer);
CONST
   overlap = 1;  { fm }
VAR
   ymin, ymax : Real;
   NmaxLeft   : Integer;
   NminRight  : Integer;
BEGIN
   FindWaveFnctnScale(uv1, wv1, uv2, wv2, Nmatch, Nmax, ymin, ymax);
   ymin := 1.1*ymin;  ymax := 1.1*ymax;
   PrepareWaveFuncPlot(ymin, ymax);
   NmaxLeft := Geti(ThisPotl.Core, Rmatch + overlap,Nmax);
   NminRight := Geti(ThisPotl.Core, Rmatch - overlap,Nmax);
   SetColor(Yellow);
   PlotDvtr(xv, uv1, 1, NmaxLeft);
   PlotDvtr(xv, uv2, NminRight, Nmax);
   SetColor(LightRed);
   PlotDvtr(xv, wv1, 1, NmaxLeft);
   PlotDvtr(xv, wv2, NminRight, Nmax);
   SetColor(whitte);
END;


{ ------------------------- Program Organization -------------------------- }

PROCEDURE ChooseInteraction;
{ Purpose:  Select one of several pre-defined interaction models }
{ Resets and Restores GLOBAL VARIABLE: muftr }
VAR
   xv           : DVector;
   Nmatch, Nmax : Integer;
   SaveMySystem : Integer;
BEGIN { ChooseInteraction (Deuteron) }
   IF SelectInteraction THEN
      BEGIN
         ClearMUPPETport;
         SaveMySystem := MySystem;
         MySystem := 2;             { Temporarily set to np system }
         SetMuftr(MySystem);
         Request1DVector(xv, Npoints);
         SetGrid(ThisPotl.Core, RmaxInit, Rstep, xv, Nmatch, Nmax);
         ZeroEnergy(Nmax, xv);
         Release1DVector(xv);
         MySystem := SaveMySystem;  { Restore to original system   }
         SetMuftr(MySystem);
      END;
END;  { ChooseInteraction (Deuteron) }

PROCEDURE AdjustInteraction;
{ Purpose:  View and possibly change the interaction parameters }
{ Resets and Restores GLOBAL VARIABLE: muftr }
VAR
   xv           : DVector;
   Nmatch, Nmax : Integer;
   SaveMySystem : Integer;
BEGIN { AdjustInteraction (Deuteron) }
   IF GetNewPotlParams THEN
      BEGIN
         ClearMUPPETport;
         SaveMySystem := MySystem;
         MySystem := 2;             { Temporarily set to np system }
         SetMuftr(MySystem);
         Request1DVector(xv, Npoints);
         SetGrid(ThisPotl.Core, RmaxInit, Rstep, xv, Nmatch, Nmax);
         ZeroEnergy(Nmax, xv);
         Release1DVector(xv);
         MySystem := SaveMySystem;  { Restore to original system   }
         SetMuftr(MySystem);
      END;
END;  { AdjustInteraction (Deuteron) }
 
PROCEDURE AutoBound;
VAR
   HighBE, LowBE : Float;
 
   FUNCTION InRange(BE: Float): Boolean;
   BEGIN
      InRange := (HighBE < BE) AND (BE < LowBE);
   END;
 
VAR
   xv, uv, wv : DVector;
   uv1, wv1, uv2, wv2  : DVector;
   Nmatch, Nmax, i : Integer;
   BE, DeltaBE : Float;
   OldDeltaBE  : Float;
   DerivLeft, DerivRight, Dprob : Float;
   imin, imax : Integer;
   umin, umax : Real;
   wmin, wmax : Real;
   ymin, ymax : Real;
   NodeCount  : Integer;
   trialnumber : Integer;
   ScattLength  : Float;
   ScattRange   : Float;
   NumBoundStates : Integer;
   RsqME, QpoleME : Float;
   SaveMySystem : Integer;
BEGIN { AutoBound (Deuteron) }
{ Resets and Restores GLOBAL VARIABLE: muftr }
   ClearMUPPETport;
   SaveMySystem := MySystem;
   MySystem := 2;             { Temporarily set to np system }
   SetMuftr(MySystem);
   Request3DVectors(xv, uv1, wv1, Npoints);
   Request2DVectors(uv2, wv2, Npoints);
   SetGrid(ThisPotl.Core, RmaxInit, Rstep, xv, Nmatch, Nmax);
   ZeroEgyNorm(Nmax, xv, uv1, wv1, ScattLength, ScattRange, NumBoundStates);
   ShowResultsScreen(ScattLength,ScattRange,NumBoundStates);
   PreparePotlPlot(potlmin, potlmax);
   PlotSDPotls(xv, Nmax);
   BE := 0;
   IF NumBoundStates = 0 THEN Announce('There is no bound state.')
   ELSE IF NumBoundStates = 1 THEN
      BEGIN
         EstimateBE(ScattLength, ScattRange, ThisPotl.Core, BE);
         HighBE := 0;
       { Find a lower bound for the bound state energy }
         LowBE := 4; { MeV }
         trialnumber := 0;
         REPEAT
            MakeApproxBoundWaveFnctn(LowBE, Nmax, Nmatch,
               DerivLeft, DerivRight, Dprob, xv, uv1, wv1, uv2, wv2);
            AnalyzeApproxBoundWaveFnctn(uv1, uv2, Nmax, Nmatch,
               DerivLeft, DerivRight, DeltaBE, NodeCount);
          { Plot the current guess at the bound state wavefunction }
            Inc(trialnumber);
            PlotWavefunction(xv, uv1, wv1, uv2, wv2, Nmatch, Nmax);
            Print(9,1,Concat('Trial Wavefunction #',
               NumStr(trialnumber,2,0)));
            LowBE := 2*LowBE;
         UNTIL (NodeCount = 0) OR (LowBE > 1000 { MeV });
         IF NodeCount = 0 THEN
            BEGIN
               DeltaBE := 0;
               REPEAT
                  OldDeltaBE := DeltaBE;
                  IF NOT InRange(BE) THEN BE := 0.5*(LowBE + HighBE);
                  MakeApproxBoundWaveFnctn(BE, Nmax, Nmatch,
                     DerivLeft, DerivRight, Dprob, xv, uv1, wv1, uv2, wv2);
                  AnalyzeApproxBoundWaveFnctn(uv1, uv2, Nmax, Nmatch,
                     DerivLeft, DerivRight, DeltaBE, NodeCount);
                  IF NodeCount = 0 THEN LowBE := BE
                                   ELSE HighBE := BE;
                  BE := BE + DeltaBE;
                { Plot the current guess at the bound state wavefunction }
                  Inc(trialnumber);
                  PlotWavefunction(xv, uv1, wv1, uv2, wv2, Nmatch, Nmax);
                  Print(9,1,Concat('Trial Wavefunction #',
                     NumStr(trialnumber,2,0)));
               UNTIL ((LowBE - HighBE) < 1e-5*(LowBE + HighBE)) OR
                     ((ABS(DeltaBE) < ABS(OldDeltaBE)) AND
                     (ABS(DeltaBE) < 1e-5*BE));
               NodeCount := Nodes(uv1, 1, Nmatch);
               NodeCount := NodeCount + Nodes(uv2, Nmatch, Nmax);
               Request2DVectors(uv, wv, Npoints);
               FOR i := 1 TO Nmatch DO
                  BEGIN
                     uv.Put(i, uv1.Value(i));  wv.Put(i, wv1.Value(i));
                  END;
               FOR i := Nmatch TO Nmax DO
                  BEGIN
                     uv.Put(i, uv2.Value(i));  wv.Put(i, wv2.Value(i));
                  END;
               SDWaveMEs(BE, Nmax, xv, uv, wv, RsqME, QpoleME);
               Release2DVectors(uv, wv);
               PlotWavefunction(xv, uv1, wv1, uv2, wv2, Nmatch, Nmax);
               Print(9,1,'Bound state Wavefunction');
               Print(11,2,Concat('at BE = ', NumStr(BE,8,3), ' MeV'));
               SelectViewPort(10);
               SetColor(Yellow);
               Print(3,17,Concat('  # Nodes = ', NumStr(NodeCount,7,0)));
               Print(2,18,Concat('rms radius = ', NumStr(sqrt(RsqME),8,3), ' fm'));
               Print(2,19,Concat('Q-pole mom = ', NumStr(QpoleME,8,3), ' fm^2'));
               SelectViewPort(2);
               SetColor(Whitte);
            END
         ELSE { For NodeCount <> 0 }
            BEGIN
               { Don't expect to get here for any reasonable potential }
               Announce('Bound state too deeply bound');
            END;
      END
   ELSE
      BEGIN { For NumBoundStates > 1 }
         Announce('More than one bound state - Use Manual Search.');
      END;
   Release2DVectors(uv2, wv2);
   Release3DVectors(xv, uv1, wv1);
   MySystem := SaveMySystem;  { Restore to original system }
   SetMuftr(MySystem);
END;  { AutoBound (Deuteron) }
 
PROCEDURE ManualBound;
{ Resets and Restores GLOBAL VARIABLE: muftr }
VAR
   xv, uv, wv : DVector;
   uv1, wv1, uv2, wv2  : DVector;
   i, Nmatch, Nmax : Integer;
   BE, DeltaBE, OldDeltaBE  : Float;
   DerivLeft, DerivRight, Dprob : Float;
   ScattLength  : Float;
   ScattRange   : Float;
   NumBoundStates : Integer;
   LostIt, FoundIt : Boolean;
   NodeCount : Integer;
   OldNodeCount : Integer;
   RsqME, QpoleME : Float;
   SaveMySystem : Integer;

BEGIN { ManualBound (Deuteron) }
   ClearMUPPETport;
   OldNodeCount := 0;
   SaveMySystem := MySystem;
   MySystem := 2;             { Temporarily set to np system }
   SetMuftr(MySystem);
   Request3DVectors(xv, uv1, wv1, Npoints);
   Request2DVectors(uv2, wv2, Npoints);
   SetGrid(ThisPotl.Core, RmaxInit, Rstep, xv, Nmatch, Nmax);
   ZeroEgyNorm(Nmax, xv, uv1, wv1, ScattLength, ScattRange, NumBoundStates);
   BE := 0;
   IF NumBoundStates = 0 THEN
      BEGIN
         ZeroEnergy(Nmax, xv);
         Announce('There is no bound state.');
      END
   ELSE
      BEGIN { NumBoundStates > 0 }
         ShowResultsScreen(ScattLength,ScattRange,NumBoundStates);
         PreparePotlPlot(potlmin, potlmax);
         PlotSDPotls(xv, Nmax);
         IF NumBoundStates = 1 THEN
            EstimateBE(ScattLength, ScattRange, ThisPotl.Core, BE);
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
      REPEAT
         MakeApproxBoundWaveFnctn(BE, Nmax, Nmatch,
            DerivLeft, DerivRight, Dprob, xv, uv1, wv1, uv2, wv2);
         AnalyzeApproxBoundWaveFnctn(uv1, uv2, Nmax, Nmatch,
            DerivLeft, DerivRight, DeltaBE, NodeCount);
       { Plot the bound state wavefunction }
         PlotWavefunction(xv, uv1, wv1, uv2, wv2, Nmatch, Nmax);
         IF ABS(DeltaBE) < 1E-3*BE THEN
            BEGIN { Display info: ready to quit search }
               FoundIt := TRUE;
               Print(9,1,'Bound state Wavefunction');
               Print(11,2,Concat('at BE = ', NumStr(BE,8,3), ' MeV'));
               NodeCount := Nodes(uv1, 1, Nmatch);
               NodeCount := NodeCount + Nodes(uv2, Nmatch, Nmax);
               Request2DVectors(uv, wv, Npoints);
               FOR i := 1 TO Nmatch DO
                  BEGIN
                     uv.Put(i, uv1.Value(i));  wv.Put(i, wv1.Value(i));
                  END;
               FOR i := Nmatch TO Nmax DO
                  BEGIN
                     uv.Put(i, uv2.Value(i));  wv.Put(i, wv2.Value(i));
                  END;
               SDWaveMEs(BE, Nmax, xv, uv, wv, RsqME, QpoleME);
               Release2DVectors(uv, wv);
               SelectViewPort(10);
               SetColor(GraphBackColor);
               Print(3,17,Concat('  # Nodes = ', NumStr(OldNodeCount,7,0)));
               SetColor(Yellow);
               Print(3,17,Concat('  # Nodes = ', NumStr(NodeCount,7,0)));
               Print(2,18,Concat('rms radius = ', NumStr(sqrt(RsqME),8,3), ' fm'));
               Print(2,19,Concat('Q-pole mom = ', NumStr(QpoleME,8,3), ' fm^2'));
               SelectViewPort(2);
               SetColor(Whitte);
            END   { Display info: ready to quit search }
         ELSE
            BEGIN { Display info: wait for instructions }
               Print(9,1,'Wavefunction calculated');
               Print(9,2,Concat('at BE = ', NumStr(BE,8,3), ' MeV'));
               BE := BE + DeltaBE;
               SetColor(LightRed);
               Print(7,3,Concat('(Next guess is ', NumStr(BE,8,3), ' MeV'));
               SelectViewPort(10);
               SetColor(GraphBackColor);
               Print(3,17,Concat('  # Nodes = ', NumStr(OldNodeCount,7,0)));
               OldNodeCount := NodeCount;
               SetColor(Yellow);
               Print(3,17,Concat('  # Nodes = ', NumStr(NodeCount,7,0)));
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
               WaitForInstructions(BE, OldDeltaBE, Lostit);
            END;  { Display info: wait for instructions }
      UNTIL FoundIt OR LostIt;
   END   { Manual search for bound state }
ELSE
   BEGIN
{      ClearMUPPETport; }
      Announce(
      'Guessed binding energy is zero.  Manual search aborted!');
   END;
            END   { a starting energy was entered }
            ELSE Announce('No starting energy.  Manual search aborted!');
      END;  { NumBoundStates > 0 }
   Release2DVectors(uv2, wv2);
   Release3DVectors(xv, uv1, wv1);
   MySystem := SaveMySystem;  { Restore to original system }
   SetMuftr(MySystem);
END;  { ManualBound (Deuteron) }
 

BEGIN
 WITH MasterMenu DO
    CASE RowChosen OF
  1: ShowHelp('twobody.hlp','AboutDeuteron');
  2: ChooseInteraction;
  3: AdjustInteraction;
  4: AutoBound;
  5: ManualBound;
    END; {CASE}
END;

END.  { Unit DEUTCALC.PAS }

