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


UNIT ALL_WAVE;
{$N+}
                    {*********************************}
                    {*    Two-Nucleon Simulation     *}
                    {*         ALL_WAVE.PAS          *}
                    {*   Written by John Philpott    *}
                    {*        July XX, 1993.         *}
                    {*   Copyright (c) 1992, 1993    *}
                    {*********************************}
 
                                 INTERFACE
 
USES
      crt, 
      graph, CUPS, CUPSmupp, CUPSfunc,
      CUPSproc, CUPSgui, CUPSgrph, Consts, Master;

PROCEDURE AllWaves;

                               IMPLEMENTATION
 
{ -------------------------- Interface Routines --------------------------- }

PROCEDURE ShowPhasesScreen(LabKE : Float; Phases: PhaseType);
VAR
   i, j : Integer;
   ftr  : Float;
   s : array[1..23] of string[20];
BEGIN
   ftr := 180/pi;
   s[1] := NumStr(LabKE,5,1);
   s[2] := NumStr(ftr*Phases[1,4],7,2);
   s[3] := NumStr(ftr*Phases[2,3],7,2);
   i := 3;
   FOR j := 2 TO 5 DO
   BEGIN
      s[i+1] := NumStr(ftr*Phases[ j ,4],7,2);
      s[i+2] := NumStr(ftr*Phases[j-1 ,1],7,2);
      s[i+3] := NumStr(ftr*Phases[ j ,2],7,2);
      s[i+4] := NumStr(ftr*Phases[j+1,3],7,2);
      s[i+5] := NumStr(ftr*Phases[ j ,5],7,2);
      i := i + 5;
   END;
   DefineViewPort(1, 0.05, 0.95, 0.06, 0.94);
   OpenViewPort(1);

   Print( 1, 2,Concat('                Phase Shifts at Lab Energy ',
                      s[1], ' MeV '));

   SetColor(LightRed);
   Print( 1, 4,
      '          Singlet   ................ Triplet ................');
   Print( 1, 5,
      '           L = J    L = J - 1    L = J    L = J + 1  Coupling');
   SetColor(Whitte);

   Print( 1, 7,'            1S0                              3P0');
   SetColor(LightRed);
   Print( 1, 8,' J = 0');
   SetColor(Yellow);
   Print(10, 8,Concat(s[2], '                          ', s[3]));
   SetColor(Whitte);

   Print(1,10,'            1P1        3S1        3P1        3D1       Eps1');
   SetColor(LightRed);
   Print(1,11,' J = 1');
   SetColor(Yellow);
   Print(10,11,Concat(s[4], '    ', s[5], '    ', s[6], '    ',
                      s[7], '    ', s[8]));
   SetColor(Whitte);

   Print(1,13,'            1D2        3P2        3D2        3F2       Eps2');
   SetColor(LightRed);
   Print(1,14,' J = 2');
   SetColor(Yellow);
   Print(10,14,Concat(s[9], '    ', s[10], '    ', s[11], '    ',
                     s[12], '    ', s[13]));
   SetColor(Whitte);

   Print(1,16,'            1F3        3D3        3F3        3G3       Eps3');
   SetColor(LightRed);
   Print(1,17,' J = 3');
   SetColor(Yellow);
   Print(10,17,Concat(s[14], '    ', s[15], '    ', s[16], '    ',
                     s[17], '    ', s[18]));
   SetColor(Whitte);

   Print(1,19,'            1G4        3F4        3G4        3H4       Eps4');
   SetColor(LightRed);
   Print(1,20,' J = 4');
   SetColor(Yellow);
   Print(10,20,Concat(s[19], '    ', s[20], '    ', s[21], '    ',
                     s[22], '    ', s[23]));
   SetColor(Whitte);
END;

PROCEDURE Prepare2upXSectsPlot(MySystem: Integer;
                 LabKE, Thetmin, Thetmax, Sigmax: Float);
VAR
   dxtik, dytik : Float;
BEGIN
   ClearMUPPETport;
 
   DefineViewport(1, 0.00, 0.97, 0.885, 0.93);
   OpenViewPort(1);
   CASE MySystem OF
1: Print(18,1,Concat(
         'Proton on Proton Scattering at', NumStr(LabKE,8,2), ' MeV'));
2: Print(19,1,Concat(
         'P + N Elastic Scattering at', NumStr(LabKE,8,2), ' MeV'));
3: Print(16,1,Concat(
         'P + N Charge Exchange Scattering at', NumStr(LabKE,8,2), ' MeV'));
4: Print(22,1,Concat(
         'Neutron on Neutron Scattering at', NumStr(LabKE,8,2), ' MeV'));
   END; { CASE }
 
   DefineViewport(1, 0.07, 0.47, 0.15, 0.85);
   OpenViewPort(1);
   DefineScale(1, Thetmin, Thetmax, 0.0, Sigmax);
   SelectScale(1);
   dxtik := TickSpace(Thetmax - Thetmin);
   dytik := TickSpace(Sigmax);
   Axis(0.0, 0.0, dxtik, dytik);
   PutLabel(bottom,'Theta (deg)');
   PutLabel(left,'d(Sigma)/d(omega)');
   Print(4,1,'Unpolarized Cross Section');
   IF TheoryDone = TRUE THEN Print(6,2,'Solid line is theory');

   DefineViewport(2, 0.57, 0.97, 0.15, 0.85);
   OpenViewPort(2);
   DefineScale(2, Thetmin, Thetmax, 0.0, Sigmax);
   SelectScale(2);
   dxtik := TickSpace(Thetmax - Thetmin);
   dytik := TickSpace(Sigmax);
   Axis(0.0, 0.0, dxtik, dytik);
   PutLabel(bottom,'Theta (deg)');
   PutLabel(left,'d(Sigma)/d(omega)');
   SetColor(LightGreen);
   Print(5,1,'Spin  Up  Cross Section');
   SetColor(LightRed);
   Print(5,2,'Spin Down Cross Section');
   SetColor(whitte);
   IF TheoryDone = TRUE THEN Print(5,3,'Solid lines are theory');
END;

PROCEDURE Prepare4upXSectsPlot(MySystem: Integer;
                 LabKE, Thetmin, Thetmax, Sigmax: Float);
VAR
   dxtik, dytik : Float;
BEGIN
   ClearMUPPETport;
 
   DefineViewport(1, 0.00, 0.97, 0.885, 0.93);
   OpenViewPort(1);
   CASE MySystem OF
1: Print(18,1,Concat(
      'Proton on Proton Scattering at', NumStr(LabKE,8,2), ' MeV'));
2: Print(19,1,Concat(
      'P + N Elastic Scattering at', NumStr(LabKE,8,2), ' MeV'));
3: Print(16,1,Concat(
      'P + N Charge Exchange Scattering at', NumStr(LabKE,8,2), ' MeV'));
4: Print(22,1,Concat(
      'Neutron on Neutron Scattering at', NumStr(LabKE,8,2), ' MeV'));
   END; { CASE }
 
   DefineViewport(1, 0.07, 0.47, 0.53, 0.85);
   OpenViewPort(1);
   DefineScale(1, Thetmin, Thetmax, 0.0, Sigmax);
   SelectScale(1);
   dxtik := TickSpace(Thetmax - Thetmin);
   dytik := TickSpace(Sigmax);
   Axis(0.0, 0.0, dxtik, dytik);
   PutLabel(bottom,'Theta (deg)');
   PutLabel(left,'d(Sigma)/d(omega)');
   SetColor(LightGreen);  Print(10,3,'Left');
   SetColor(LightRed);  Print(18,3,'Right');
   SetColor(whitte);
   Print(2,1,'Initial spin parallel to beam');
   Print(7,2,'Measured spin perp');
   IF TheoryDone = TRUE THEN Print(5,4,'Solid lines are theory');

   DefineViewport(2, 0.57, 0.97, 0.53, 0.85);
   OpenViewPort(2);
   DefineScale(2, Thetmin, Thetmax, 0.0, Sigmax);
   SelectScale(2);
   dxtik := TickSpace(Thetmax - Thetmin);
   dytik := TickSpace(Sigmax);
   Axis(0.0, 0.0, dxtik, dytik);
   PutLabel(bottom,'Theta (deg)');
   PutLabel(left,'d(Sigma)/d(omega)');
   SetColor(LightGreen);  Print(10,3,'Left');
   SetColor(LightRed);  Print(18,3,'Right');
   SetColor(whitte);
   Print(4,1,'Initial spin perp to beam');
   Print(7,2,'Measured spin perp');
   IF TheoryDone = TRUE THEN Print(5,4,'Solid lines are theory');

   DefineViewport(3, 0.07, 0.47, 0.13, 0.45);
   OpenViewPort(3);
   DefineScale(3, Thetmin, Thetmax, 0.0, Sigmax);
   SelectScale(3);
   dxtik := TickSpace(Thetmax - Thetmin);
   dytik := TickSpace(Sigmax);
   Axis(0.0, 0.0, dxtik, dytik);
   PutLabel(bottom,'Theta (deg)');
   PutLabel(left,'d(Sigma)/d(omega)');
   SetColor(LightGreen);  Print(5,3,'Parallel');
   SetColor(LightRed);  Print(17,3,'Antiparallel');
   SetColor(whitte);
   Print(2,1,'Initial spin parallel to beam');
   Print(5,2,'Measured spin parallel');
   IF TheoryDone = TRUE THEN Print(5,4,'Solid lines are theory');

   DefineViewport(4, 0.57, 0.97, 0.13, 0.45);
   OpenViewPort(4);
   DefineScale(4, Thetmin, Thetmax, 0.0, Sigmax);
   SelectScale(4);
   dxtik := TickSpace(Thetmax - Thetmin);
   dytik := TickSpace(Sigmax);
   Axis(0.0, 0.0, dxtik, dytik);
   PutLabel(bottom,'Theta (deg)');
   PutLabel(left,'d(Sigma)/d(omega)');
   SetColor(LightGreen);  Print(5,3,'Parallel');
   SetColor(LightRed);  Print(17,3,'Antiparallel');
   SetColor(whitte);
   Print(4,1,'Initial spin perp to beam');
   Print(5,2,'Measured spin parallel');
   IF TheoryDone = TRUE THEN Print(5,4,'Solid lines are theory');
END;


{ ------------------------- Program Organization -------------------------- }

PROCEDURE ChooseSystem;
{ Resets GLOBAL VARIABLES: muftr, TheoryDone }
VAR
   ViewScreen : TInputScreen;
BEGIN { ChooseSystem (AllWaves) }
   OpenScrn(ViewScreen);
   WITH ViewScreen DO
   BEGIN
      init;
      DefineInputport(0.1, 0.9, 0.2, 0.8);
      LoadLine(
         Center('Select one of the scattering processes listed below:-') );
      LoadLine('');
      LoadLine(''); 
      LoadLine('          #1  Proton on proton elastic scattering');
      LoadLine('          #1  Proton on neutron elastic scattering');
      LoadLine('          #1  Ditto - charge exchange scattering');
      LoadLine('          #1  Neutron on neutron elastic scattering');
      LoadLine('');
      LoadLine(''); 
      LoadLine('                 [  Ok  ]           [Cancel]       ');
      SetRadioButton('1',MySystem);
      AcceptScreen;
      IF NOT Canceled THEN
         BEGIN
            MySystem := GetRadioButton('1');
            SetMuftr(MySystem);
            TheoryDone := FALSE;
         END;
      done;
   END;
   CloseScrn(ViewScreen);
END;  { ChooseSystem (AllWaves) }

PROCEDURE GetLabKE;
{ Resets GLOBAL VARIABLES: LabKE, TheoryDone }
VAR
   KEScreen : TInputScreen;
BEGIN
   OpenScrn(KEScreen);
   WITH KEScreen DO
   BEGIN
      init;
      DefineInputport(0.25, 0.75, 0.25, 0.75); 
      LoadLine('');
      LoadLine('Enter the kinetic energy of the    ');
      LoadLine('projectile in the laboratory frame:');
      LoadLine('');
      LoadLine(' Lab Kinetic Energy = {   0} MeV   ');
      LoadLine('');
      LoadLine('Note:  Energies outside the range  ');
      LoadLine(' 24 MeV to 400 MeV will be ignored.');
      LoadLine('');
      LoadLine('          [  Ok  ]  [Cancel]       ');
      SetNumber(1,LabKE);
      SetNumberLimits(1,24,400);
      AcceptScreen;
      LabKE := GetNumber(1);
      IF NOT canceled THEN TheoryDone := FALSE;
      done;
   END;
   CloseScrn(KEScreen);
END;

PROCEDURE ShowEmpiricalPhases;
VAR
   Phases       : PhaseType;
   PhasesFound  : Boolean;
BEGIN { ShowEmpiricalPhases (AllWaves) }
   ClearMUPPETport;
   DefineViewPort(1, 0.05, 0.95, 0.06, 0.94);
   OpenViewport(1);
   GetPhases(LabKE, PhasesFound, Phases);
   ClearMUPPETport;
   ShowPhasesScreen( LabKE, Phases);
END;  { ShowEmpiricalPhases (AllWaves) }


PROCEDURE ChooseInteraction;
{ Purpose:  Select one of several pre-defined interaction models }
BEGIN { ChooseInteraction (AllWaves) }
   IF SelectInteraction THEN
      BEGIN
      END;
END;  { ChooseInteraction (AllWaves) }


PROCEDURE AdjustInteraction;
{ Purpose:  View and possibly change the interaction parameters }
BEGIN
   IF GetNewPotlParams THEN
      BEGIN
      END;
END;

PROCEDURE CalcPhases;
VAR
   cmEGY : Float;
   Zproj, Ztarg : Float;
   xk           : Float;
   ZZPesq, eta  : Float;
   PhasesFound  : Boolean;
   xv           : DVector;
   Nmatch, Nmax : Integer;
   Jmax         : Integer;
BEGIN { CalcPhases (AllWaves) }
   ClearMUPPETport;
   DefineViewPort(1, 0.05, 0.95, 0.06, 0.94);
   OpenViewport(1);
 
   Request1DVector(xv, Npoints);
   SetGrid(ThisPotl.Core, RmaxInit, Rstep, xv, Nmatch, Nmax);
   cmEgy := 0.5*LabKE;
   Jmax := 7;
   IF ThisPotl.Form = HJ THEN
      NuclearBarPhasesHJ(cmEgy, Npoints, Nmax, Nmatch,
                         Jmax, xv, TheoryPhases)
   ELSE
      NuclearBarPhases(cmEgy, Npoints, Nmax, Nmatch,
                       Jmax, xv, TheoryPhases);
   Release1DVector(xv);
   ShowPhasesScreen( LabKE, TheoryPhases);
   TheoryDone := TRUE;
END;  { CalcPhases (AllWaves) }


PROCEDURE GetAngularRange;
{ Resets GLOBAL VARIABLES: Thetmin, Thetmax, NumAng }
VAR
   RangeScreen : TInputScreen;
   Number      : real;
   GoodRange   : boolean;
BEGIN
   OpenScrn(RangeScreen);
   WITH RangeScreen DO
   BEGIN
      init;
      DefineInputport(0.28, 0.72, 0.25, 0.7);   
      LoadLine('     Enter an angular range    ');
      LoadLine('');
      LoadLine(' Min Angle (in deg) = {     0} ');
      LoadLine(' Max Angle (in deg) = {     0} ');
      LoadLine(' (Angles in range 0 to 180 deg)');
      LoadLine('');
      LoadLine(' Number of angles   = {     0} ');
      LoadLine(' (An integer in range 2 to 250)');
      LoadLine('');
      LoadLine('       [  Ok  ]    [Cancel]    ');
      SetNumber(1,Thetmin);
      SetNumberLimits(1,0,180);
      SetNumber(2,Thetmax);
      SetNumberLimits(2,0,180);
      SetNumber(3,NumAng);
      SetNumberLimits(3,2,250);
      GoodRange := FALSE;
      REPEAT
         AcceptScreen;
         Thetmin := GetNumber(1);
         Thetmax := GetNumber(2);
         Number  := GetNumber(3);
         NumAng := round(Number);
         IF Thetmax <= Thetmin THEN
          announce('Max angle must exceed Min angle.  Try again...') ELSE
         IF Number <> NumAng THEN
          announce('Integer number of angles, please.  Try again...') ELSE
         GoodRange := TRUE;
      UNTIL GoodRange OR Canceled;
      done;
   END;
   CloseScrn(RangeScreen);
END;

PROCEDURE CrossSects1;
{ Uses GLOBAL VARIABLES:  Thetmin, Thetmax, Numang }
VAR
   Theta        : Real;
   Phases       : PhaseType;
   PhasesFound  : Boolean;
   i            : Integer;
   sig, sigmax  : Float;
   sigP, sigD   : Float;
   sigup, sigdn : Float;
   sigmav1, sigmav2, sigmav3   : DVector;
 
BEGIN { CrossSects1 (AllWaves) }
   ClearMUPPETport;
   Request1DVector(thetav, Numang);
   RequestCXVectors(av, bv, cv, ev, fv, Numang);
   GetPhases(LabKE, PhasesFound, Phases);
   MakeScattAmps(Phases, Thetmin, Thetmax, Numang);
   FindReasonableScale(Numang, Sigmax);
   Prepare2upXSectsPlot(MySystem, LabKE, Thetmin, Thetmax, Sigmax);
{  Plot cross sections obtained from the empirical phase shifts }
   HideMouse;
   FOR i := 1 TO numang DO
      BEGIN
         Sigmas1(i, theta, sig, sigP, sigD);
         SelectViewPort(1);
         SelectScale(1);
         PlotPoint(Theta,Sig);
         SelectViewPort(2);
         SelectScale(2);
         SetColor(LightGreen);
         sigup := 0.5*(sig + sigP);
         PlotPoint(Theta,sigup);
         SetColor(LightRed);
         sigdn := 0.5*(sig - sigP);
         PlotPoint(Theta,sigdn);
         SetColor(whitte);
      END;
   ShowMouse;
{  Plot cross sections obtained from the calculated phase shifts }
   IF TheoryDone = TRUE THEN
      BEGIN
         MakeScattAmps(TheoryPhases, Thetmin, Thetmax, numang);
         Request3DVectors(sigmav1, sigmav2, sigmav3, Numang);
         FOR i := 1 TO numang DO
            BEGIN
               Sigmas1(i, theta, sig, sigP, sigD);
               sigmav1.Put(i,sig);
               sigmav2.Put(i, 0.5*(sig + sigP));
               sigmav3.Put(i, 0.5*(sig - sigP));
            END;
         SelectViewPort(1);
         SelectScale(1);
         PlotDvtr(thetav, sigmav1, 1, numang);
         SelectViewPort(2);
         SelectScale(2);
         SetColor(LightGreen);
         PlotDvtr(thetav, sigmav2, 1, numang);
         SetColor(LightRed);
         PlotDvtr(thetav, sigmav3, 1, numang);
         SetColor(whitte);
         Release3DVectors(sigmav1, sigmav2, sigmav3);
      END;
   ReleaseCXVectors(av, bv, cv, ev, fv);
   Release1DVector(thetav);
END;  { CrossSects1 (AllWaves) }


PROCEDURE CrossSects2;
{ Uses GLOBAL VARIABLES:  Thetmin, Thetmax, Numang }

   PROCEDURE PlotSigPoints(Theta, Sig, dSig: Float);
   VAR
      Left, Right : Float;
   BEGIN
      SetColor(LightGreen);
      Left := 0.5*(Sig + dSig);
{      PlotLine(Theta,Left,Theta,Left); }
      PlotPoint(Theta,Left);
      SetColor(LightRed);
      Right := 0.5*(Sig - dSig);
{      PlotLine(Theta,Right,Theta,Right); }
      PlotPoint(Theta,Right);
      SetColor(whitte);
   END;
 
VAR
   Theta       : Real;
   arg, c, s   : Float;
   Phases      : PhaseType;
   PhasesFound : Boolean;
   i           : Integer;
   wk, eta, dThet, sig, sigmax : Float;
   sigxx, sigxz, sigzx, sigzz  : Float;
   sigLL, sigLS, sigSL, sigSS  : Float;
   sigmav1, sigmav2            : DVector;
BEGIN { CrossSects2 (AllWaves) }
   ClearMUPPETport;
   Request1DVector(thetav, Numang);
   RequestCXVectors(av, bv, cv, ev, fv, Numang);
   GetPhases(LabKE, PhasesFound, Phases);
   MakeScattAmps(Phases, Thetmin, Thetmax, numang);
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
   FindReasonableScale(Numang, Sigmax);
   Prepare4upXSectsPlot(MySystem, LabKE, Thetmin, Thetmax, Sigmax);
{  Plot cross sections obtained from the empirical phase shifts }
   HideMouse;
   FOR i := 1 TO numang DO
      BEGIN
         Sigmas2(i, theta, sig, sigxx, sigxz, sigzz);
         sigzx := - sigxz;
         arg := (pi/180)*0.5*theta;
         c := cos(arg);  s := sin(arg);
         SelectViewPort(1);
         SelectScale(1);
         sigSL := c*sigxz - s*sigxx;
         PlotSigPoints(Theta, Sig, sigSL);
         SelectViewPort(2);
         SelectScale(2);
         sigSS := c*sigxx + s*sigxz;
         PlotSigPoints(Theta, Sig, sigSS);
         SelectViewPort(3);
         SelectScale(3);
         sigLL := c*sigzz - s*sigzx;
         PlotSigPoints(Theta, Sig, sigLL);
         SelectViewPort(4);
         SelectScale(4);
         sigLS := c*sigzx + s*sigzz;
         PlotSigPoints(Theta, Sig, sigLS);
      END;
   ShowMouse;
{  Plot cross sections obtained from the calculated phase shifts }
   IF TheoryDone = TRUE THEN
      BEGIN
         MakeScattAmps(TheoryPhases, Thetmin, Thetmax, numang);
         Request2DVectors(sigmav1, sigmav2, Numang);
         FOR i := 1 TO numang DO
            BEGIN
               Sigmas2(i, theta, sig, sigxx, sigxz, sigzz);
               arg := (pi/180)*0.5*theta;
               c := cos(arg);  s := sin(arg);
               sigSL := c*sigxz - s*sigxx;
               sigmav1.Put(i,0.5*(sig + sigSL));
               sigmav2.Put(i,0.5*(sig - sigSL));
            END;
         SelectViewPort(1);
         SelectScale(1);
         SetColor(LightGreen);
         PlotDvtr(thetav, sigmav1, 1, numang);
         SetColor(LightRed);
         PlotDvtr(thetav, sigmav2, 1, numang);
         SetColor(whitte);
 
         FOR i := 1 TO numang DO
            BEGIN
               Sigmas2(i, theta, sig, sigxx, sigxz, sigzz);
               arg := (pi/180)*0.5*theta;
               c := cos(arg);  s := sin(arg);
               sigSS := c*sigxx + s*sigxz;
               sigmav1.Put(i,0.5*(sig + sigSS));
               sigmav2.Put(i,0.5*(sig - sigSS));
            END;
         SelectViewPort(2);
         SelectScale(2);
         SetColor(LightGreen);
         PlotDvtr(thetav, sigmav1, 1, numang);
         SetColor(LightRed);
         PlotDvtr(thetav, sigmav2, 1, numang);
         SetColor(whitte);

         FOR i := 1 TO numang DO
            BEGIN
               Sigmas2(i, theta, sig, sigxx, sigxz, sigzz);
               sigzx := - sigxz;
               arg := (pi/180)*0.5*theta;
               c := cos(arg);  s := sin(arg);
               sigLL := c*sigzz - s*sigzx;
               sigmav1.Put(i,0.5*(sig + sigLL));
               sigmav2.Put(i,0.5*(sig - sigLL));
            END;
         SelectViewPort(3);
         SelectScale(3);
         SetColor(LightGreen);
         PlotDvtr(thetav, sigmav1, 1, numang);
         SetColor(LightRed);
         PlotDvtr(thetav, sigmav2, 1, numang);
         SetColor(whitte);

         FOR i := 1 TO numang DO
            BEGIN
               Sigmas2(i, theta, sig, sigxx, sigxz, sigzz);
               sigzx := - sigxz;
               arg := (pi/180)*0.5*theta;
               c := cos(arg);  s := sin(arg);
               sigLS := c*sigzx + s*sigzz;
               sigmav1.Put(i,0.5*(sig + sigLS));
               sigmav2.Put(i,0.5*(sig - sigLS));
            END;
         SelectViewPort(4);
         SelectScale(4);
         SetColor(LightGreen);
         PlotDvtr(thetav, sigmav1, 1, numang);
         SetColor(LightRed);
         PlotDvtr(thetav, sigmav2, 1, numang);
         SetColor(whitte);
         Release2DVectors(sigmav1, sigmav2);
      END;

   ReleaseCXVectors(av, bv, cv, ev, fv);
   Release1DVector(thetav);
END;  { CrossSects2 (AllWaves) }

PROCEDURE AllWaves;
BEGIN
 WITH MasterMenu DO
    CASE RowChosen OF
  1: ShowHelp('twobody.hlp','AboutCrossSect');
  2: ChooseSystem;
  3: GetLabKE;
  4: ShowEmpiricalPhases;
  5: ChooseInteraction;
  6: AdjustInteraction;
  7: CalcPhases;
  8: GetAngularRange;
  9: CrossSects1;
  10: CrossSects2;
    END; {CASE}
END;
 
END.  { Unit ALL_WAVE.PAS }
