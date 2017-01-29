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


UNIT PHASCALC;
{$N+}
                    {*********************************}
                    {*    Two-Nucleon Simulation     *}
                    {*        PHASCALC.PAS           *}
                    {*   Written by John Philpott    *}
                    {*        July XX, 1993.         *}
                    {*   Copyright (c) 1992, 1993    *}
                    {*********************************}

INTERFACE

USES
      crt, 
      graph, CUPS, CUPSmupp, CUPSfunc,
      CUPSproc, CUPSgui, CUPSgrph, Consts, Master;

{ -------------------- Global Constants and Variables --------------------- }

VAR
   lchar : ARRAY[0..20] OF Char;

VAR
   MinEgy    : Float;
   MaxEgy    : Float;
   NumEgy    : Integer;
   MyChannel : Integer;
   MySpin    : Integer;

PROCEDURE InitializeOAMlabels;
PROCEDURE InitializePhascalcConstants;
PROCEDURE Phases;
PROCEDURE PhasesInit;

IMPLEMENTATION

PROCEDURE InitializeOAMlabels;
{ OAM stands for orbital angular momentum }
BEGIN
   lchar[ 0] := 'S';
   lchar[ 1] := 'P';
   lchar[ 2] := 'D';
   lchar[ 3] := 'F';
   lchar[ 4] := 'G';
   lchar[ 5] := 'H';
   lchar[ 6] := 'I';
   lchar[ 7] := 'J';
   lchar[ 8] := 'K';
   lchar[ 9] := 'L';
   lchar[10] := 'M';
   lchar[11] := 'N';
   lchar[12] := 'O';
   lchar[13] := 'P';
   lchar[14] := 'Q';
   lchar[15] := 'R';
   lchar[16] := 'S';
   lchar[17] := 'T';
   lchar[18] := 'U';
   lchar[19] := 'V';
   lchar[20] := 'W';
END;

PROCEDURE InitializePhascalcConstants;
BEGIN
   MinEgy    :=   0.0;
   MaxEgy    := 400.0;
   NumEgy    :=  41;
   MyChannel := 1;
   MySpin    := 0;
END;

PROCEDURE Phases;

{ -------------------- Physics (and Math-Phys) Routines ------------------- }

FUNCTION MySpinOK: Boolean;
BEGIN
   MySpinOK := NOT ((MyChannel = 2) AND (MySpin = 0));
END;

FUNCTION IsospinOK: Boolean;
BEGIN
   IF (MySystem = 1) OR (MySystem = 4) THEN
      BEGIN
         IF MyChannel = 2 THEN IsospinOK := NOT Even(MySpin)
                          ELSE IsospinOK := Even(MySpin);
      END
   ELSE IsoSpinOK := TRUE;
END;

{ ----------------------- Phase Shift Calculations ------------------------ }

PROCEDURE GetEmpiricalPhases(LabKE: Float; MySpin : Integer;
                             VAR vPhase, wPhase, couple: Float;
                             VAR PhasesFound: Boolean);
VAR
   lp : Integer;
   Phases : PhaseType;
BEGIN
   GetPhases(LabKE, PhasesFound, Phases);
   CASE MyChannel OF
1: vPhase := Phases[MySpin + 1,4]; { Singlet }
2: vPhase := Phases[MySpin + 1,2]; { Triplet, L = J }
3: BEGIN
      IF MySpin = 0 THEN
         BEGIN
            wPhase := Phases[2,3]; { 3P0 }
         END
      ELSE
         BEGIN
            vPhase := Phases[MySpin,    1]; { Triplet, L = J - 1 }
            wPhase := Phases[MySpin + 2,3]; { Triplet, L = J + 1 }
            couple := Phases[MySpin + 1,5]; { Triplet,   epsJ    }
         END;
   END;
   END; { CASE }
END;

PROCEDURE CalcPhase(LabKE: Float; Nmax: Integer; xv, uv, wv: DVector;
                    VAR vPhase, wPhase, couple: Float);
{ USES GLOBAL VARIABLE CapJ }
VAR
   cmEgy, xk, eta : Float;
   Kv, Kw, Kc : Float;
BEGIN
   cmEgy := 0.5*LabKE;
   xk := SQRT(muftr*cmEgy);
   IF MySystem = 1 THEN
      eta := esq*muftr/(2.0*xk)
   ELSE eta := 0;

   CoulombFunctions( {Input} eta, xk, xv.Value(Nmax), CapJ+1,
                     {Output} CoulSig, F, G, Fp, Gp);

   CASE MyChannel OF
1: BEGIN { Singlet }
      SingletPhase(cmEgy, xv, uv, Nmax, vPhase);
   END;
2: BEGIN { Triplet, L = J }
      DiagTripletPhase(cmEgy, xv, uv, Nmax, vPhase);
   END;
3: BEGIN { Triplet, coupled }
      IF CapJ = 0 THEN { 3P0 }
         TripletP0Phase(cmEgy, xv, uv, Nmax, wPhase)
      ELSE
         BEGIN
            CoupledPhase(cmEgy, xv, uv, wv, Nmax, vPhase, wPhase, couple);
         END;
   END;
   END; { CASE }
END;

PROCEDURE CalcPhaseHJ(LabKE: Float; Nmax: Integer; xv, uv, wv: DVector;
                      VAR vPhase, wPhase, couple: Float);
{ USES GLOBAL VARIABLE CapJ }
VAR
   cmEgy, xk, eta : Float;
   Kv, Kw, Kc : Float;
BEGIN
   cmEgy := 0.5*LabKE;
   xk := SQRT(muftr*cmEgy);
   IF MySystem = 1 THEN
      eta := esq*muftr/(2.0*xk)
   ELSE eta := 0;

   CoulombFunctions( {Input} eta, xk, xv.Value(Nmax), CapJ+1,
                     {Output} CoulSig, F, G, Fp, Gp);

   CASE MyChannel OF
1: BEGIN { Singlet }
      SingletPhaseHJ(cmEgy, xv, uv, Nmax, vPhase);
   END;
2: BEGIN { Triplet, L = J }
      DiagTripletPhaseHJ(cmEgy, xv, uv, Nmax, vPhase);
   END;
3: BEGIN { Triplet, L = coupled }
      IF CapJ = 0 THEN { 3P0 }
         TripletP0PhaseHJ(cmEgy, xv, uv, Nmax, wPhase)
      ELSE
         BEGIN { Triplet, L <> J, J > 0 }
            CoupledPhaseHJ(cmEgy, xv, uv, wv, Nmax, vPhase, wPhase, couple);
         END;
   END;
   END; { CASE }
END;

PROCEDURE GetCalcPhase(LabKE: Float; Nmax: Integer; xv, uv, wv: DVector;
                       VAR vPhase, wPhase, couple: Float);
BEGIN
   IF ThisPotl.Form = HJ THEN
      CalcPhaseHJ(LabKE, Nmax, xv, uv, wv, vPhase, wPhase, couple)
   ELSE CalcPhase(LabKE, Nmax, xv, uv, wv, vPhase, wPhase, couple);
END;

{ -------------------------- Interface Routines --------------------------- }

FUNCTION MakePhaseLabel(MyChannel, MySpin, L: Integer): String;
VAR
   len     : Integer;
   jstring : String;
BEGIN
   IF MySpin > 9 THEN len := 2
                 ELSE len := 1;
   jstring := NumStr(MySpin,len,0);
   IF MyChannel = 1 THEN MakePhaseLabel := Concat('1', lchar[L], jstring)
                    ELSE MakePhaseLabel := concat('3', lchar[L], jstring);
END;

PROCEDURE PreparePhasePlot(MinEgy, MaxEgy: Float;
                           MyChannel, MySpin: Integer);
VAR
   dxtik, dytik : Float;
   L, len       : Integer;
BEGIN

   DefineViewport(1, 0.00, 0.97, 0.84, 0.93);
   OpenViewPort(1);
   CASE MySystem OF
1: Print(7,1,'Proton on Proton Phase Shifts');
2: Print(7,1,'Proton on Neutron Phase Shifts');
3: Print(7,1,'Proton on Neutron Phase Shifts');
4: Print(7,1,'Neutron on Neutron Phase Shifts');
   END; { CASE }
   Print(50,1,'o o o o o = observed');
   Print(50,2,'--------- = calculated ');
   SetColor(Yellow);
   IF NOT (MyChannel = 3) THEN
      BEGIN
         L := MySpin;
         Print(20,2,MakePhaseLabel(MyChannel, MySpin, L));
      END
   ELSE
      BEGIN
         IF MySpin = 0 THEN
            BEGIN { 3P0 }
               L := MySpin + 1;
               SetColor(LightRed);
               Print(20,2,MakePhaseLabel(MyChannel, MySpin, L));
            END   { 3P0 }
         ELSE
            BEGIN
               L := MySpin - 1;
               Print(10,2,MakePhaseLabel(MyChannel, MySpin, L));
               L := L + 2;
               SetColor(LightRed);
               Print(20,2,MakePhaseLabel(MyChannel, MySpin, L));
               SetColor(LightBlue);
               IF MySpin > 9 THEN len := 2
                             ELSE len := 1;
               Print(30,2,Concat('eps', NumStr(MySpin,len,0)));
            END;
      END;
   SetColor(whitte);
   DefineViewport(2, 0.07, 0.97, 0.15, 0.82);
   OpenViewPort(2);
   DefineScale(2, MinEgy, MaxEgy, 0.0, 180.0);
   SelectScale(2);
   dxtik := TickSpace(MaxEgy - MinEgy);
   dytik := TickSpace(180.0);
   Axis(0.0, 0.0, dxtik, dytik);
   PutLabel(bottom,'Lab Energy (MeV)');
   PutLabel(left,'Phase Shift (deg)');
END;
 
PROCEDURE PlotEmpiricalPhases(MinEgy, MaxEgy: Float;
                              NumEgy, MySpin: Integer);

   VAR
      ftr : Float;

   FUNCTION Fixup(Phase: Float): Float;
   VAR
      Delta : Float;
   BEGIN
      Delta := ftr*Phase;
      IF Delta < 0 THEN Delta := Delta + 180;
      Fixup := Delta;
   END;

VAR
   i : Integer;
   PhasesFound : Boolean;
   vPhase, wPhase, couple : Float;
   Egy, DEgy, dydx        : Float;
   vDelta, wDelta, epsJ   : Float;
BEGIN
   ftr := 180/pi;
   SelectViewPort(2);
   SelectScale(2);
   DEgy := (MaxEgy - MinEgy)/(NumEgy - 1);
   IF MinEgy < MinimumTabulatedLabKE THEN
      MinEgy := MinimumTabulatedLabKE;
   IF MaxEgy > MaximumTabulatedLabKE THEN
      MaxEgy := MaximumTabulatedLabKE;
   NumEgy := Round((MaxEgy - MinEgy)/DEgy) + 1;
   Egy := MinEgy - DEgy;
   i := 0;
   REPEAT
      Inc(i);
      Egy := Egy + DEgy;
      GetEmpiricalPhases(Egy, MySpin, vPhase, wPhase, couple, PhasesFound);
      IF PhasesFound THEN
         BEGIN
            IF NOT (MyChannel = 3) THEN
               BEGIN
                  vDelta := Fixup(vPhase);
                  SetColor(yellow);
                  PlotSymbol(Egy, vDelta, 'o');
               END
            ELSE
               BEGIN
                  IF MySpin = 0 THEN
                     BEGIN { 3P0 }
                        wDelta := Fixup(wPhase);
                        SetColor(LightRed);
                        PlotSymbol(Egy, wDelta, 'o');
                     END   { 3P0 }
                  ELSE
                     BEGIN
                        vDelta := Fixup(vPhase);
                        SetColor(yellow);
                        PlotSymbol(Egy, vDelta, 'o');
                        SetColor(LightRed);
                        wDelta := Fixup(wPhase);
                        PlotSymbol(Egy, wDelta, 'o');
                        SetColor(LightBlue);
                        epsJ := Fixup(couple);
                        PlotSymbol(Egy, epsJ, 'o');
                     END;
               END;
         END;
   UNTIL (i = NumEgy) OR NOT PhasesFound;
   SetColor(whitte);
END;
 
PROCEDURE PlotSegment(OldEgy: Float; VAR OldDelta: Float;
                      Egy, Delta: Float);
CONST
   breakvalue = 90; { degrees }
BEGIN
   IF ABS(Delta - OldDelta) < breakvalue THEN
      Plotline(OldEgy, OldDelta, Egy, Delta);
   OldDelta := Delta;
END;

PROCEDURE PlotPhases(MinEgy, MaxEgy: Float; NumEgy, MySpin: Integer);

   VAR
      ftr : Float;

   FUNCTION Fixup(Phase: Float): Float;
   VAR
      Delta : Float;
   BEGIN
      Delta := ftr*Phase;
      IF Delta < 0 THEN Delta := Delta + 180;
      Fixup := Delta;
   END;

CONST
   breakvalue = 90; { degrees }
   eps        = 1E-6;
VAR
   i : Integer;
   Egy, OldEgy, DEgy : Float;
   vDelta, OldvDelta : Float;
   wDelta, OldwDelta : Float;
   epsJ, OldepsJ     : Float;
   xv, uv, wv   : DVector;
   Nmatch, Nmax : Integer;
   vPhase, wPhase, couple : Float;
BEGIN
   ftr := 180/pi;
   SelectViewPort(2);
   SelectScale(2);
   SetColor(Yellow);
   Request3DVectors(xv, uv, wv, Npoints);
   SetGrid(ThisPotl.Core, RmaxInit, Rstep, xv, Nmatch, Nmax);
   Nmatch := Nmax;
   DEgy := (MaxEgy - MinEgy)/(NumEgy - 1);
   Egy := MinEgy;
   OldEgy := Egy;
   IF Egy < eps THEN
      BEGIN
         OldvDelta := 0;  OldwDelta := 0;  OldepsJ := 0;
         IF (MyChannel = 3) AND (CapJ = 1) THEN OldvDelta := 180;
      END
   ELSE
      BEGIN
         GetCalcPhase(Egy, Nmax, xv, uv, wv, vPhase, wPhase, couple);
         IF NOT (MyChannel = 3) THEN OldvDelta := Fixup(vPhase)
         ELSE
            BEGIN { Triplet, coupled }
               IF MySpin = 0 THEN OldwDelta := Fixup(wPhase) { 3P0 }
               ELSE
                  BEGIN
                     OldvDelta := Fixup(vPhase);
                     OldwDelta := Fixup(wPhase);
                     OldepsJ := Fixup(couple);
                  END;
            END;
      END;
   i := 1;
   REPEAT
      Inc(i);
      Egy := Egy + DEgy;
      GetCalcPhase(Egy, Nmax, xv, uv, wv, vPhase, wPhase, couple);
      IF NOT (MyChannel = 3) THEN
         BEGIN
            SetColor(yellow);
            vDelta := Fixup(vPhase);
            PlotSegment(OldEgy, OldvDelta, Egy, vDelta);
         END
      ELSE
         BEGIN
            IF MySpin = 0 THEN
               BEGIN { 3P0 }
                  SetColor(LightRed);
                  wDelta := Fixup(wPhase);
                  PlotSegment(OldEgy, OldwDelta, Egy, wDelta);
               END   { 3P0 }
            ELSE
               BEGIN
                  SetColor(yellow);
                  vDelta := Fixup(vPhase);
                  PlotSegment(OldEgy, OldvDelta, Egy, vDelta);
                  SetColor(LightRed);
                  wDelta := Fixup(wPhase);
                  PlotSegment(OldEgy, OldwDelta, Egy, wDelta);
                  SetColor(LightBlue);
                  epsJ := Fixup(couple);
                  PlotSegment(OldEgy, OldepsJ, Egy, epsJ);
               END;
         END;
      OldEgy := Egy;
   UNTIL (i = NumEgy) OR break;
   Release3DVectors(xv, uv, wv);
   SetColor(whitte);
END;
 
FUNCTION ChooseChannel: Boolean;
{ Purpose:  Get input to determine which phase shift to display }
VAR
   ViewScreen : TInputScreen;
BEGIN
   OpenScrn(ViewScreen);
   WITH ViewScreen DO
   BEGIN
      init;
      DefineInputport(0.25, 0.75, 0.15, 0.80);
      LoadLine('');
      LoadLine('  Choose the quantum numbers that'  );
      LoadLine('  label the desired phase shift'    );
      LoadLine('');
      LoadLine('         #1  Singlet'               );
      LoadLine('         #1  Triplet, L = J'        );
      LoadLine('         #1  Triplet, coupled'      );
      LoadLine('');
      LoadLine('  Choose the total angular momentum');
      LoadLine('');
      LoadLine('             J = {  }'              );
      LoadLine('    (An integer in range 0 to 6)'   );
      LoadLine('');
      LoadLine('         [  Ok  ]     [Cancel]'     );
      SetRadioButton('1',MyChannel);
      SetNumber(4,MySpin);
      SetNumberLimits(4,0,6);
      AcceptScreen;
      MyChannel := GetRadioButton('1');
      MySpin := Round(GetNumber(4));
      ChooseChannel := NOT Canceled;
      done;
   END; { WITH ViewScreen }
   CloseScrn(ViewScreen);
END;

{ ------------------------- Program Organization -------------------------- }

PROCEDURE ChooseInteraction;
{ Purpose:  Select one of several pre-defined interaction models }
BEGIN { ChooseInteraction (Phases) }
   IF SelectInteraction THEN
      BEGIN
      END;
END;  { ChooseInteraction (Phases) }

PROCEDURE AdjustInteraction;
{ Purpose:  View and possibly change the interaction parameters }
VAR
   xv           : DVector;
   Nmatch, Nmax : Integer;
BEGIN { AdjustInteraction (Phases) }
   IF GetNewPotlParams THEN
      BEGIN
      END;
END;  { AdjustInteraction (Phases) }
 
PROCEDURE EnergyRange;
VAR
   NewEgyRange : Boolean;
BEGIN { EnergyRange (Phases) }
   NewEgyRange := GetEgyRange(MinEgy, MaxEgy, NumEgy);
END;  { EnergyRange (Phases) }

PROCEDURE ShowPhases;
{ ALTERS GLOBAL VARIABLE: CapJ }
VAR
   Nmatch, Nmax : Integer;
   xv, yv : DVector;
   done : Boolean;
BEGIN { ShowPhases (Phases) }
   REPEAT
      done := TRUE;
      IF ChooseChannel THEN
         BEGIN
            ClearMUPPETport;
            CapJ := MySpin;
            PreparePhasePlot(MinEgy, MaxEgy, MyChannel, MySpin);
            IF MySpinOK AND IsospinOK THEN
               BEGIN
                  PlotEmpiricalPhases(MinEgy, MaxEgy, NumEgy, MySpin);
                  PlotPhases(MinEgy, MaxEgy, NumEgy, MySpin);
               END
            ELSE
               BEGIN
                  Announce(
      'This channel is forbidden for identical nucleons.  Try again...');
                  done := FALSE;
               END;
         END;
   UNTIL done;
END;  { ShowPhases (Phases) }

BEGIN
 WITH MasterMenu DO
    CASE RowChosen OF
  1: ShowHelp('twobody.hlp','AboutPhases');
  2: ChooseInteraction;
  3: AdjustInteraction;
  4: EnergyRange;
  5: ShowPhases;
    END; {CASE}
END;

PROCEDURE PhasesInit;
BEGIN

   InitializeOAMlabels;
   InitializePhascalcConstants;

END;

END.  { Unit PHASCALC.PAS }
