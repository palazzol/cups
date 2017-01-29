PROGRAM QMTime;

{ Quantal time development in one dimension. }
{ By Dan Styer (dstyer@physics.oberlin.edu).  Version 1.0.  29 August 1994. }
{ This version uses the CUPS utilities of 7 July 1994, including workarounds
  for bugs in that release. }

{ Contents of program QMTime:

  Introduction (including notes on modifying the program).
  Constants.

  Global variables that will not be altered.
  Global variables that specify the profile.
  Global variables that specify the state.
  Global variables for time stepping.
  Global variables for flow control.
  Global variables for the user interface.

  Subroutines for the user to modify:
     UserDefinedPotential, UserDefinedWF.
  Time development:
     PrepareToRun, PrepareWithDriving, StepWF, StepWFNaive.
  Mean values:
     MeanE, MeanX, FindMeanXP.
  Change of basis:
     FindMomentumWF, FindEnergySpectrum.
  Output (Plots):
     SetUpPlot, ColorSlice, DrawWheel, ShowDisplayStyle,
     Interpolate, FloodWithColor, FloodWithColorM,
     PlotWFandPotl, PlotMomentumWF, PlotEnergySpectrum, Plot.
  Process the potential:
     BuildPotl.
  Process the wave function:
     NormalizeWF, BuildWFPacket, BuildWFCombo, BuildWF.
  Alter the wave function:
     ShiftWF, StretchWF, ReflectWF, ConjugateWF.
  Input:
     GetPotlPara, GetDrivingPara, GetWFPara, ProcessDisplay.
  Action on global variables:
     SetStaticVariables, SetDefaultProfile, GenerateState.
  Interface:
     UpdateMenu, SetUpInterface, ShowKeyBanner.
  Help:
     GiveWelcome, AboutQMTime, GiveHints.
  File handling:
     GetFileName, GetComment, ReadProfileFile, SaveProfileFile.
  Flow control:
     ProcessKeys, ProcessMenu, HaltOnEdgeWallCollision, MouseClickedInMenu.
  Main program. }

{---Introduction (including notes on modifying the program)------------------}

{ Welcome to program QMTime!  I am glad that you have decided to look
  at the program, because you will learn more about quantum mechanics
  by mucking about in the code than you will by just watching the
  simulation.  This program bears a copyright at the insistence of
  the distributer, John Wiley & Sons, Inc.  The copyright is not
  intended to stop you from looking at or modifying the code, which
  I encourage.  If you do modify the code then you MUST:  1) change the
  name of the program, 2) change the introductory comments in the first
  four lines of the program, and 3) change the screen in procedure
  GiveWelcome.  You may not sell a modification of this program, but
  I encourage you to distribute it by sending a copy to the anonymous
  ftp site top.physics.oberlin.edu.  If your modification is sufficiently
  interesting, I will post it there under directory /pub/QMTime.   (I am
  also interested in distributing interesting profile files in this manner.)
  If you distribute a modified program, either through this ftp site or
  in any other way, then that program must have similar restrictions on
  its modification and distribution.  For examples, see the programs
  QMBeach and QMValue available through top.physics.oberlin.edu.

              Happy trails,
              Dan Styer. }

{$N+ Generate code for numeric coprocessor. }

USES DOS, CRT, Graph, CUPS, CUPSmupp, CUPSgui, CUPSfunc, CUPSgrph;

CONST
   NPoints = 256;   { Number of grid points.  If the momentum wave function
      is to be found, this must be an integral power of 2 greater than 4.
      Must be less than or equal to the CUPSmupp constant MaxNumData. }

   { Although the value of the wave function is calculated at all of the
     NPoints points described above, the display window is typically
     smaller.  Here and in procedure SetStaticVariables we set the
     display region to be the middle half of the calculated region.
     This hides spurious edge effects. }
   NumDP = (NPoints DIV 2) + 1;   { The number of displayed points. }

   nTop = 5;   { Highest eigenfunction used when finding energy spectra.
      This is the only thing you need to change to display more energy
      projection amplitudes. }
   NumColors = 8;   { The number of colors used in color-for-phase display. }
   NumPotls = 14;   { The number of time-independent potential energy
      functions (i.e. including "User defined" but excluding "Driving"). }

VAR
{---Global variables that will not be altered--------------------------------}
   { These variables are set in procedure SetStaticVariables. }
   xmin, xmax, Dx: Real;
      { Edges of the universe, grid spacing. }
      { Throughout the program, all positions are in units of nanometers. }
   DtSI, Dt: Real;
      { Time step in femtoseconds and in "scaled" units:
           scaled time = time*hbar/m in nm^2
        where m is the electron's mass. }

   FirstDP: Integer;
      { The number of the first grid point in the display window. }
   xleft, xright: Real;
      { The frames of the display window, in nanometers, of course. }

   Flat, InfWell, SHO: Integer;
      { Names for some special potentials. }
   Infinity: Real;
      { Code value for infinite potential. }
   CeVtoScaled: Real;
      { A "scaled energy" is energy*(m/hbar^2) -- where m is
        is the electron's mass -- in units of nm^(-2).  The variable
        CeVtoScaled holds m/hbar^2 in units of nm^(-2)/eV, so that
        scaled energy (in nm^(-2)) = energy (in eV) * CeVtoScaled. }

   xValue: DataVector;   { A CUPSmupp data type }
      { List of grid of displayed x values. }
   pValue: DataVector;   { A CUPSmupp data type }
      { Grid of values where the momentum wave function is evaluated and
        plotted.  Data are actually momenta/hbar in units of nm^(-1). }

   ColorOf: ARRAY[1..NumColors] OF Integer;
      { Wheel of hue used in color for phase display style. }
   RealColor, ImagColor: Integer;
      { Colors used in real and imaginary parts display style. }
   ProbColor: Integer;
      { Color used in probability density display style. }

{---Global variables that specify the profile--------------------------------}
   { In this program the "profile" and the "state" are different
     things.  The profile can be changed only by choices from the
     menu or hot keys.  The state can be changed by choices from the
     menu/keys or through time stepping. }
   { The profile variables listed here are all initially set in
     procedure SetDefaultProfile. }
   { Actually, I like the term "configuration" better than "profile",
     but after I had introduced and used the term "configuration", the
     CUPS directors preempted the word to use for what should be called
     "preferences"! }
   ActivePotl: Integer;
      { The number, from 1 to NumPotls, of the potential currently in use. }
   PotlPara: ARRAY[1..NumPotls, 1..3] OF Real;
      { Allow up to three parameters for each of NumPotls types of potential.
        The meaning of each parameter is given by comments in procedure
        SetDefaultProfile. }
   ForceF, Forcew: Real;
      { Amplitude (in eV/nm) and frequency (in fsec^(-1)) of the driving
        force. }

   ActiveInitWF: Integer;
      { The number, from 1 to 5, of the most recent initial wave function. }
      { Note that, in contrast to the situation for potential, there is
        a difference between the "active initial wave function" and the
        "current wave function." }
   InitWFPara: ARRAY[1..NumPotls, 1..3] OF Real;
      { Parameters for the initial wave function when it is a wave packet.
        Allow three parameters for each of NumPotls different potentials
        (using the same values for each of the three different wave packet
        types). }
   InitWFCombo: ARRAY[1..2, 1..6] OF Real;
      { Parameters for the initial wave function when it is a linear
        combination of energy eigenfunctions.  Allow six parameters for
        each of potentials InfWell and SHO. }

   DisplayStyle: Integer;
      { How to show wave function: 1 is color for phase,
        2 is real and imaginary parts, 3 is square magnitude. }
   DisplayContent: Integer;
      { An integer from 0 to 7 telling which combination of three possible
        graphs to display.  (Bit 1 means display, bit 0 means don't.)
        Rightmost bit controls display of position wave function.
        Middle bit controls display of momentum wave function.
        Leftmost bit controls display of energy spectrum. }
   ShowMeans, ShowUncertainties: Integer;
      { If ShowMeans is positive then means of position and momentum
        are displayed.  Otherwise they aren't.  Similarly for
        ShowUncertainties. }
   AnimationSpeed: Integer;
      { The one profile variable that is changed through
        the hot keys rather than the menu.  If negative, amount of
        pause between frames in half-seconds.  If positive, number of
        steps taken without redrawing the screen. }

{---Global variables that specify the state----------------------------------}
   { These variables are initially set in procedure GenerateState.  They
     can be changed through the menu, and (except for V, VD, Jstart, and
     Jstop) they change as time evolves. }
   Itime: Integer;
      { Current time in units of Dt. }

   V: ARRAY[1..NPoints] OF Real;
      { V[J] is the value of the scaled potential energy function at
        the Jth grid point.  The program assumes that that there are
        infinite potential values located on bands of finite width
        located at each edge, and that these bands hold the only
        infinite potential values.  Note the shorthand for infinity
        given in procedure SetStaticVariables. }
   VD: DataVector;   { A CUPSmupp data type }
      { Array of potential values within the display window, in eV. }
   Jstart, Jstop: Integer;
      { The noninfinite potential values run from Jstart to Jstop
        inclusive. }

   Psi: ARRAY[1..NPoints, 1..2] OF Real;
      { Psi[J,1] is the real part of Psi at point J, Psi[J,2] is the
        imaginary part of Psi there.  Units of (nm)^(-1/2). }
   PsiTilda: ARRAY[1..NPoints, 1..2] OF Real;
      { Values of the momentum wave function.  Units of (nm)^(+1/2). }
   Coeff: ARRAY[0..nTop, 1..2] OF Real;
      { Values of the energy expansion coefficients.  Dimensionless. }

{---Global variables for time stepping---------------------------------------}
   { These variables are set in procedure PrepareToRun or
     PrepareWithDriving and used by procedure StepWF. }
   DRe: ARRAY[1..NPoints] OF Real;
   DIm: Real;
      { The Jth diagonal element is D[J] = DRe[J] + i DIm. }
   U: ARRAY[1..NPoints, 1..2] OF Real;
      { The Jth upper diagonal element is U[J] = U[J,1] + i U[J,2]. }

{---Global variables for flow control----------------------------------------}
   QuitNow: Boolean;
      { If TRUE, quit at next opportunity. }
   Running: Boolean;
      { TRUE in run mode, FALSE in command (or menu) mode. }
   Stepping: Boolean;
      { TRUE if single stepping, FALSE if going continuously. }
   IDirection: Integer;
      { If +1, time flows forward; if -1, backwards. }

{---Global variables for the user interface----------------------------------}
   { All are CUPSgui object variables. }
   Menu: TMenu;
   Keys: THotKeys;
   InputScreen: TInputScreen;



{---Subroutines for the user to modify---------------------------------------}

FUNCTION UserDefinedPotential (x: Real): Real;
{ You may use this function to investigate your own potential energy
  function without fear of damaging any other part of the program.
  Insert code producing a potential energy in units of eV.
  If your function has regions of infinite potential, use the code
  value for infinity established in procedure SetStaticVariables below.
  The potential may be infinite only on two unbroken bands adjacent to
  either edge of the grid. }

BEGIN
   { Replace this line with any function of x. }
   UserDefinedPotential := Abs (x);
END; {of FUNCTION UserDefinedPotential}



PROCEDURE UserDefinedWF (x: Real; VAR RePsi, ImPsi: Real);
{ You may use this procedure to insert your own initial wave function
  without fear of damaging any other part of the program.  Simply calculate
  the real part of Psi (RePsi) and the imaginary part of Psi (ImPsi) at the
  point x.  The wave function doesn't need to be normalized. }

BEGIN
   RePsi := Exp (-x*x);
   ImPsi := 0.0;
END; {of PROCEDURE UserDefinedWF}



{---Time development---------------------------------------------------------}

PROCEDURE PrepareToRun;
{ Set up potential-dependent variables for use by procedure StepWF. }
{ This subroutine alters the global variables used in time stepping. }

VAR
   J: Integer;
   Re, Im: Real;

BEGIN

   DIm := 4*IDirection * Dx*Dx/Dt;

   U[Jstart-1,1] := 0.0; U[Jstart-1,2] := 0.0;

   FOR J := Jstart TO Jstop DO
   BEGIN
      DRe[J] := - 2.0 * Dx*Dx*V[J] - 2.0;

      { U[J] := 1/(D[J] - U[J-1]); }
      Re := DRe[J] - U[J-1,1]; Im := DIm - U[J-1,2];
      U[J,1] :=   Re/(Re*Re + Im*Im);
      U[J,2] := - Im/(Re*Re + Im*Im);
   END; {of FOR J}

END; {of PROCEDURE PrepareToRun}



PROCEDURE PrepareWithDriving;
{ Set up (potential and driving force)-dependent variables for use by
  procedure StepWF. }
{ This subroutine alters the global variables DRe and U. }

VAR
   J: Integer;
   x, AveF, Re, Im: Real;

BEGIN

   AveF := CeVtoScaled * ForceF
     * 0.5 * (Sin (Forcew*DtSI*(Itime-IDirection)) + Sin (Forcew*DtSI*Itime));

   x := xmin + Dx*(Jstart-1);
   U[Jstart-1,1] := 0.0; U[Jstart-1,2] := 0.0;
   FOR J := Jstart TO Jstop DO
   BEGIN
         DRe[J] := - 2.0 * Dx*Dx*(V[J] - AveF*x) - 2.0;

         { U[J] := 1/(D[J] - U[J-1]); }
         Re := DRe[J] - U[J-1,1]; Im := DIm - U[J-1,2];
         U[J,1] :=   Re/(Re*Re + Im*Im);
         U[J,2] := - Im/(Re*Re + Im*Im);

         x := x + Dx;
   END; {of FOR J}

END; {of PROCEDURE PrepareWithDriving}



PROCEDURE StepWF;
{ Replace the wave function Psi with an approximation for its value
  a time Dt later, using the Crank-Nicholson algorithm. }
{ This subroutine alters the state global variables Itime and Psi. }

VAR
   J: Integer;
   Re, Im: Real;

BEGIN
   Itime := Itime + IDirection;

   IF (ForceF <> 0.0) THEN PrepareWithDriving;

   J := Jstart;

   { Psi[J-1] := -(Psi[J+1] + ccD[J]*Psi[J]) * U[J];
     where ccD[J] is the complex conjugate of D[J]. }
   Re := Psi[J+1,1] + DRe[J]*Psi[J,1] + DIm*Psi[J,2];
   Im := Psi[J+1,2] + DRe[J]*Psi[J,2] - DIm*Psi[J,1];
   Psi[J-1,1] := - Re*U[J,1] + Im*U[J,2];
   Psi[J-1,2] := - Re*U[J,2] - Im*U[J,1];

   FOR J := Jstart+1 TO Jstop DO
   BEGIN

      { Psi[J-1] := -(Psi[J+1] + ccD[J]*Psi[J] + Psi[J-1] + Psi[J-2])*U[J]; }
      Re := Psi[J+1,1] + DRe[J]*Psi[J,1] + DIm*Psi[J,2] + Psi[J-1,1]
            + Psi[J-2,1];
      Im := Psi[J+1,2] + DRe[J]*Psi[J,2] - DIm*Psi[J,1] + Psi[J-1,2]
            + Psi[J-2,2];
      Psi[J-1,1] := - Re*U[J,1] + Im*U[J,2];
      Psi[J-1,2] := - Re*U[J,2] - Im*U[J,1];

   END; {of FOR J}

   FOR J := Jstop DOWNTO Jstart DO
   BEGIN

      { Psi[J] := Psi[J-1] - U[J]*Psi[J+1]; }
      Psi[J,1] := Psi[J-1,1] - U[J,1]*Psi[J+1,1] + U[J,2]*Psi[J+1,2];
      Psi[J,2] := Psi[J-1,2] - U[J,1]*Psi[J+1,2] - U[J,2]*Psi[J+1,1];

   END; {of FOR J}

   Psi[Jstart-1,1] := 0.0; Psi[Jstart-1,2] := 0.0;

END; {of PROCEDURE StepWF}



PROCEDURE StepWFNaive;
{ Replace the wave function Psi with an approximation for its value
  a time Dt later, using the naive "forward time center space" method.
  It assumes that the driving force is zero. }
{ This subroutine alters the state global variables Itime and Psi. }

VAR
   J: Integer;
   DD, Temp, OldPsiRe, OldPsiIm, TempPsiRe, TempPsiIm: Real;

BEGIN
   Itime := Itime + IDirection;

   DD := Dt/(2.0*Dx*Dx);

   J := Jstart;

   { OldPsi := Psi[J]; }
   OldPsiRe := Psi[J,1]; OldPsiIm := Psi[J,2];

   { Psi[J] := Psi[J] + i DD (Psi[J+1] - 2Psi[J]) - i Dt V[J] Psi[J]; }
   Temp     := Psi[J,1] - DD*(Psi[J+1,2] - 2*Psi[J,2]) + Dt*V[J]*Psi[J,2];
   Psi[J,2] := Psi[J,2] + DD*(Psi[J+1,1] - 2*Psi[J,1]) - Dt*V[J]*Psi[J,1];
   Psi[J,1] := Temp;

   FOR J := Jstart+1 TO Jstop DO
   BEGIN

      { TempPsi := Psi[J]; }
      TempPsiRe := Psi[J,1]; TempPsiIm := Psi[J,2];

      { Psi[J] := Psi[J] + i DD (Psi[J+1] - 2Psi[J] + OldPsi)
                     - i Dt V[J] Psi[J]; }
      Temp     := Psi[J,1] - DD*(Psi[J+1,2] - 2*Psi[J,2] + OldPsiIm)
                     + Dt*V[J]*Psi[J,2];
      Psi[J,2] := Psi[J,2] + DD*(Psi[J+1,1] - 2*Psi[J,1] + OldPsiRe)
                     - Dt*V[J]*Psi[J,1];
      Psi[J,1] := Temp;

      { OldPsi := TempPsi; }
      OldPsiRe := TempPsiRe; OldPsiIm := TempPsiIm;

   END; {of FOR J}

END; {of PROCEDURE StepWFNaive}



{---Mean values--------------------------------------------------------------}

FUNCTION MeanE: Real;
{ Find mean value of scaled energy for current wave function and
  potential. }
{ This subroutine does not alter any global variable. }

VAR
   J: Integer;
   KE, PE: Real;

BEGIN
   KE := 0.0;
   PE := 0.0;
   FOR J := Jstart TO Jstop DO
   BEGIN
      KE := KE + Psi[J,1]*Psi[J-1,1] + Psi[J,2]*Psi[J-1,2];
      PE := PE + V[J] * (Sqr (Psi[J,1]) + Sqr (Psi[J,2]));
   END;
   MeanE := 1/(Dx*Dx) - KE/Dx + PE*Dx;
END; {of FUNCTION MeanE}



FUNCTION MeanX: Real;
{ Find mean value of position for current wave function. }
{ This subroutine does not alter any global variable. }

VAR
   J: Integer;
   Temp, x : Real;

BEGIN
   Temp := 0.0;
   x := xmin;
   FOR J := 1 TO NPoints DO
   BEGIN
      Temp := Temp + x*(Sqr (Psi[J,1]) + Sqr (Psi[J,2]));
      x := x + Dx;
   END;
   MeanX := Temp*Dx;
END; {of FUNCTION MeanX}



PROCEDURE FindMeanXP (VAR xave, delx, pave, delp: Real);
{ Find mean and uncertainty of position and momentum for the current
  wave function.  Actually, we don't use momentum, but instead the
  wave number k = p/hbar. }
{ This subroutine does not alter any global variable. }
{ Dependencies: ArcTan2 is a CUPSfunc function.
                Pi is a Turbo function. }

{ This procedure uses two different algorithms to find the mean and
  uncertainty for momentum.  Algorithm A relies on linear interpolation
  of the real and imaginary parts of Psi between grid points.  Algorithm B
  relies upon linear interpolation of the magnitude and phase of Psi.
  Neither one is particularly good (look at a Lorentzian wave packet, for
  example).  Algorithm A is definitely superior for excited pure energy
  eigenfunctions, because in these cases the phase jumps by Pi when the
  magnitude is zero, while algorithm B seems to be superior in other cases.
  Whatever, it has been my observation that whichever algorithm gives a lower
  momentum uncertainty is more likely to be correct.  So I calculate it
  both ways and then report the smaller value.  I recommend this to you as
  a research problem. }

VAR
   J: Integer;
   paveA, paveB, p2aveA, p2aveB, delpA, delpB: Real;
   M2jm1, M2j, M2jp1, Phjm1, Phj, Phjp1, x, x2ave: Real;

BEGIN

   xave := 0.0;
   x2ave := 0.0;
   paveA := 0.0;
   p2aveA := 0.0;
   paveB := 0.0;
   p2aveB := 0.0;

   J := Jstart;
   x := xmin + (J-1)*Dx;

   M2jm1 := 0.0;
   M2j   := Sqr (Psi[J,1]) + Sqr (Psi[J,2]);

   Phjm1 := 0.0;
   Phj   := ArcTan2 (Psi[J,1], Psi[J,2]);

   FOR J := Jstart TO Jstop DO
   BEGIN

      { Find new magnitudes and phases. }
      M2jp1 := Sqr (Psi[J+1,1]) + Sqr (Psi[J+1,2]);
      Phjp1 := ArcTan2 (Psi[J+1,1], Psi[J+1,2]);
         { Arrange for all three phases to be on the same branch. }
      IF (Phjm1 < 0.5*Pi) AND (Phjp1 > 1.5*Pi) THEN
      BEGIN
         Phjm1 := Phjm1 + 2*Pi;
         IF (Phj < Pi) THEN Phj := Phj + 2*Pi;
      END
      ELSE IF (Phjp1 < 0.5*Pi) AND (Phjm1 > 1.5*Pi) THEN
      BEGIN
         Phjm1 := Phjm1 - 2*Pi;
         IF (Phj > Pi) THEN Phj := Phj - 2*Pi;
      END;

      { Update position averages. }
      xave  := xave  +   x*M2j;
      x2ave := x2ave + x*x*M2j;
      x := x + Dx;

      { Update momentum averages...real and imaginary part interpolation. }
      paveA  := paveA  + Psi[J,1]*Psi[J+1,2] - Psi[J,2]*Psi[J+1,1];
      p2aveA := p2aveA + Psi[J,1]*Psi[J+1,1] + Psi[J,2]*Psi[J+1,2];

      { Update momentum averages...magnitude and phase interpolation. }
      paveB  := paveB  + M2j*(Phjp1 - Phjm1);
      p2aveB := p2aveB + M2j*Sqr(Phjp1 - Phjm1) - 8.0*Sqrt(M2j*M2jm1);

      { Update magnitudes and phases. }
      IF (Phj < 0) THEN Phj := Phj + 2*Pi
         ELSE IF (Phj > 2*Pi) THEN Phj := Phj - 2*Pi;
      M2jm1 := M2j;
      M2j := M2jp1;
      Phjm1 := Phj;
      Phj := Phjp1;

   END; {of FOR J}

   xave := xave*Dx;
   x2ave := x2ave*Dx;
   delx := Sqrt (x2ave - Sqr(xave));

   p2aveA := 2.0/Sqr(Dx) - (2.0/Dx)*p2aveA;
   delpA := Sqrt (p2aveA - Sqr(paveA));

   paveB := 0.5*paveB;
   p2aveB := 2.0/Sqr(Dx) + p2aveB/(4.0*Dx);
   delpB := Sqrt (p2aveB - Sqr(paveB));

   IF (delpB < delpA) THEN
   BEGIN
      pave := paveB;
      delp := delpB;
   END
   ELSE
   BEGIN
      pave := paveA;
      delp := delpA;
   END;

END; {of PROCEDURE FindMeanXP}



{---Change of basis----------------------------------------------------------}

PROCEDURE FindMomentumWF;
{ Find the momentum wave function. }
{ This subroutine alters the state global variable PsiTilda. }
{ This procedure calculates a discrete Fourier transform using the
  decimation-in-space (Cooley-Tukey) FFT algorithm. }
{ Dependencies: Pi is a Turbo function. }

VAR
   I, J, L, Lrev, probe, Nhat, Istart: Integer;
   Qr, Qi, QPr, QPi, Tempr, Tempi: Real;

BEGIN

   { Copy from Psi into PsiTilda, with the suitable change of sign. }
   J := 1;
   WHILE (J < NPoints) DO
   BEGIN
      PsiTilda[J,1] := Psi[J,1];
      PsiTilda[J,2] := Psi[J,2];
      J := J + 1;
      PsiTilda[J,1] := - Psi[J,1];
      PsiTilda[J,2] := - Psi[J,2];
      J := J + 1;
   END; {of WHILE J}

   { Rearrange into bit reversed sequence. }
   Lrev := 0;
   FOR L := 0 TO NPoints - 3 DO
   BEGIN
      IF (Lrev > L) THEN
      BEGIN
         Tempr := PsiTilda[L+1,1];
         PsiTilda[L+1,1] := PsiTilda[Lrev+1,1];
         PsiTilda[Lrev+1,1] := Tempr;
         Tempi := PsiTilda[L+1,2];
         PsiTilda[L+1,2] := PsiTilda[Lrev+1,2];
         PsiTilda[Lrev+1,2] := Tempi;
      END; {of IF Lrev}

      probe := NPoints DIV 2;
      WHILE (Lrev >= probe) DO
      BEGIN
         Lrev := Lrev - probe;
         probe := probe DIV 2;
      END; {of WHILE Lrev}
      Lrev := Lrev + probe;
   END; {of FOR L}

   { Build up the discrete Fourier transform. }
   Nhat := 1;
   WHILE (Nhat < NPoints) DO
   BEGIN

      Qr := + Cos (Pi/Nhat);
      Qi := - Sin (Pi/Nhat);
      QPr := 1.0;
      QPi := 0.0;

      FOR Istart := 1 TO Nhat DO
      BEGIN
         I := Istart;
         WHILE (I < NPoints) DO
         BEGIN
            J := I + Nhat;
            Tempr := QPr*PsiTilda[J,1] - QPi*PsiTilda[J,2];
            Tempi := QPr*PsiTilda[J,2] + QPi*PsiTilda[J,1];
            PsiTilda[J,1] := PsiTilda[I,1] - Tempr;
            PsiTilda[J,2] := PsiTilda[I,2] - Tempi;
            PsiTilda[I,1] := PsiTilda[I,1] + Tempr;
            PsiTilda[I,2] := PsiTilda[I,2] + Tempi;
            I := I + 2*Nhat;
         END; {of WHILE I}

         Tempr := QPr;
         QPr := Qr*QPr - Qi*QPi;
         QPi := Qr*QPi + Qi*Tempr;
      END; {of FOR Istart}

      Nhat := 2*Nhat;
   END; {of WHILE Nhat}

   { Normalize and fix up the sign. }
   Tempr := Dx/Sqrt (2.0*Pi);
   J := 1;
   WHILE (J < NPoints) DO
   BEGIN
      PsiTilda[J,1] := Tempr*PsiTilda[J,1];
      PsiTilda[J,2] := Tempr*PsiTilda[J,2];
      J := J + 1;
      PsiTilda[J,1] := - Tempr*PsiTilda[J,1];
      PsiTilda[J,2] := - Tempr*PsiTilda[J,2];
      J := J + 1;
   END; {of WHILE J}

END; {of PROCEDURE FindMomentumWF}



PROCEDURE FindEnergySpectrum;
{ Find energy eigenfunction expansion coefficients:
    n-th coefficient = Coeff[n,1] + i Coeff[n,2] = <u_n|Psi>,
  where |u_n> represents the n-th energy eigenfunction.
  Used for infinite square well and harmonic oscillator potentials only. }
{ This subroutine alters the state global variable Coeff. }
{ Dependencies: Pi is a Turbo function. }

VAR
   J, n: Integer;
   x, temp, factor: Real;
   xi, u, u1, u2: Real;
   { These are used for the harmonic oscillator potential.
     xi is the scaled position variable Sqrt (omega*m/hbar) * x.
     u  is the value of the nth energy eigenfunction at the current x.
     u1 is the value of the (n-1)th energy eigenfunction at the current x.
     u2 is the value of the (n-2)th energy eigenfunction at the current x.
     The above values are unnormalized:
        u(normalized) = [(omega*m)/(pi*hbar)]^(1/4) * u(here). }

BEGIN

   IF (ActivePotl = InfWell) THEN
   BEGIN   { Infinite square well }

      FOR n := 0 TO nTop DO
      BEGIN
         Coeff[n,1] := 0.0;
         Coeff[n,2] := 0.0;
      END; {of FOR n}

      x := xmin;
      FOR J := 1 TO NPoints DO
      BEGIN
         temp := Pi*(x - PotlPara[InfWell,1])/PotlPara[InfWell,2];

         FOR n := 1 TO nTop DO
         BEGIN
            Coeff[n,1] := Coeff[n,1] + Sin(n*temp)*Psi[J,1];
            Coeff[n,2] := Coeff[n,2] + Sin(n*temp)*Psi[J,2];
         END; {of FOR n}

         x := x + Dx;
      END; {of FOR J}

      FOR n := 1 TO nTop DO
      BEGIN
         Coeff[n,1] := (Dx * Sqrt (2.0/PotlPara[InfWell,2]) ) * Coeff[n,1];
         Coeff[n,2] := (Dx * Sqrt (2.0/PotlPara[InfWell,2]) ) * Coeff[n,2];
      END; {of FOR n}

   END
   ELSE
   BEGIN   { Harmonic oscillator }

      FOR n := 0 TO nTop DO
      BEGIN
         Coeff[n,1] := 0.0;
         Coeff[n,2] := 0.0;
      END; {of FOR n}

      factor := Sqrt (Sqrt (CeVtoScaled*PotlPara[SHO,1]));

      x := xmin;
      FOR J := 1 TO NPoints DO
      BEGIN
         xi := factor*x;

         { Here u2 holds the value of u_0. }
         u2 := Exp ( -0.5 * xi*xi);
         Coeff[0,1] := Coeff[0,1] + u2*Psi[J,1];
         Coeff[0,2] := Coeff[0,2] + u2*Psi[J,2];

         { Here u1 holds the value of u_1. }
         u1 := Sqrt (2.0) * xi * u2;
         Coeff[1,1] := Coeff[1,1] + u1*Psi[J,1];
         Coeff[1,2] := Coeff[1,2] + u1*Psi[J,2];

         { Use the recurrence relation for SHO energy eigenfunctions. }
         FOR n := 2 TO nTop DO
         BEGIN
            { Here u holds the value of u_n. }
            u := xi * Sqrt (2/n) * u1  -  Sqrt ((n-1)/n) * u2;
            Coeff[n,1] := Coeff[n,1] + u*Psi[J,1];
            Coeff[n,2] := Coeff[n,2] + u*Psi[J,2];

            u2 := u1;
            u1 := u;
         END; {of FOR n}

         x := x + Dx;
      END; {of FOR J}

      factor := Dx * Sqrt (Sqrt ( Sqrt(CeVtoScaled*PotlPara[SHO,1])/Pi ));
      FOR n := 0 TO nTop DO
      BEGIN
         Coeff[n,1] := factor * Coeff[n,1];
         Coeff[n,2] := factor * Coeff[n,2];
      END; {of FOR n}

   END;

END; {of PROCEDURE FindEnergySpectrum}



{---Output (Plots)-----------------------------------------------------------}

PROCEDURE SetUpPlot;
{ This subroutine does not alter any global variable. }
{ Dependencies: Blakk is a CUPS unit constant.
                GraphBackColor is a CUPS unit variable.
                All subroutines are from CUPSmupp. }

BEGIN
   GraphBackColor := Blakk;
   ClearMUPPETPort;

   CASE DisplayContent OF

      1: BEGIN   { Display Psi(x). }
            DefineViewPort (1, 0.10, 0.95, 0.15, 0.90);
         END;

      2: BEGIN   { Display PsiTilda(p). }
            DefineViewPort (2, 0.10, 0.95, 0.15, 0.90);
         END;

      3: BEGIN   { Display Psi(x) and PsiTilda(p). }
            DefineViewPort (1, 0.10, 0.95, 0.57, 0.90);
            DefineViewPort (2, 0.10, 0.95, 0.15, 0.48);
         END;

      4: BEGIN   { Display energy spectrum. }
            DefineViewPort (3, 0.10, 0.95, 0.15, 0.90);
         END;

      5: BEGIN   { Display Psi(x) and energy spectrum. }
            DefineViewPort (1, 0.10, 0.95, 0.57, 0.90);
            DefineViewPort (3, 0.10, 0.95, 0.15, 0.48);
         END;

      6: BEGIN   { Display PsiTilda(p) and energy spectrum. }
            DefineViewPort (2, 0.10, 0.95, 0.57, 0.90);
            DefineViewPort (3, 0.10, 0.95, 0.15, 0.48);
         END;

      7: BEGIN   { Display Psi(x), PsiTilda(p), and energy spectrum. }
            DefineViewPort (1, 0.10, 0.95, 0.57, 0.90);
            DefineViewPort (2, 0.10, 0.67, 0.15, 0.48);
            DefineViewPort (3, 0.75, 0.95, 0.15, 0.48);
         END;

   END; {of CASE DisplayContent}

   DefineScale (1, xleft, xright, -1.0, +10.0);
      { If you change the last two numbers in the above call to
        DefineScale, be sure to change the corresponding numbers
        in the second call to PlotLine in procedure PlotWFandPotl. }
   DefineScale (2, -30.0, +30.0, -0.5, +0.5);
   IF (DisplayStyle <> 3) THEN DefineScale (3, 0.0, nTop + 1, -0.5, +0.5)
      ELSE DefineScale (3, 0.0, nTop + 1, 0.0, 1.0);

END; {of PROCEDURE SetUpPlot}



FUNCTION ColorSlice (Re, Im: Real; VAR Phase: Real): Integer;
{ Return the number of the slice of phase associated with the given complex
  number.  In turn, ColorOf[ColorSlice (Re, Im, Phase)] gives the associated
  VGA color.  ColorSlice also returns the phase of the complex number. }
{ This subroutine neither uses nor alters any global variable. }
{ Dependencies: ArcTan2 is a CUPSfunc function.
                Pi is a Turbo function. }

VAR
   Itemp: Integer;

BEGIN

   Phase := ArcTan2 (Re, Im);
   Itemp := Trunc (NumColors*Phase/Pi);
   IF (Itemp >= 2*NumColors - 1) THEN Itemp := -1;
   ColorSlice := (Itemp + 3) DIV 2;

END; {of FUNCTION ColorSlice}



PROCEDURE DrawWheel;
{ Draw a sample color wheel. }
{ This subroutine neither uses nor alters any global variable. }
{ Dependencies: GetMaxX, GetMaxY, SetFillStyle, SetColor, PieSlice,
                   and OutTextXY are Turbo Graph subroutines.
                SolidFill is a Turbo Graph constant.
                Whitte is a CUPS unit constant. }

VAR
   X, Y, Radius, Seg: Integer;
   HalfSlice: Real;

BEGIN

   X := Round (0.85*GetMaxX);
   Y := Round (0.23*GetMaxY);
   Radius := 20;
   HalfSlice := 180.0/NumColors;

   { First half slice. }
   SetFillStyle (SolidFill, ColorOf[1]);
   SetColor (ColorOf[1]);
   PieSlice (X, Y, 0, Round (HalfSlice), Radius);

   { All full slices. }
   FOR Seg := 2 TO NumColors DO
   BEGIN
      SetFillStyle (SolidFill, ColorOf[Seg]);
      SetColor (ColorOf[Seg]);
      PieSlice (X, Y, Round ((2*Seg - 3)*HalfSlice),
                      Round ((2*Seg - 1)*HalfSlice), Radius);
   END;

   { Last half slice. }
   SetFillStyle (SolidFill, ColorOf[1]);
   SetColor (ColorOf[1]);
   PieSlice (X, Y, Round ((2*NumColors - 1)*HalfSlice), 360, Radius);

   { Zero mark. }
   SetColor (Whitte);
   OutTextXY (X + Radius - 2, Y - 3, '-0');

END; {of PROCEDURE DrawWheel}



PROCEDURE ShowDisplayStyle;
{ Present a sample of the display style in the upper right corner. }
{ This subroutine does not alter any global variable. }
{ Dependencies: GetMaxX, GetMaxY, SetColor, Line, and
                   OutTextXY are Turbo Graph subroutines.
                Whitte is a CUPS unit constant.
                SelectMUPPETPort is a CUPSmupp subroutine. }

VAR
   X, Y: Integer;

BEGIN

   SelectMUPPETPort;

   CASE DisplayStyle OF

      1: BEGIN   { Color for phase }
            PrintXY (0.61, 0.81, 'Color for phase');
            DrawWheel;
         END;

      2: BEGIN   { Real and Imaginary parts }
            X := Round (0.75*GetMaxX);
            Y := Round (0.20*GetMaxY);
            SetColor (RealColor);
            Line (X, Y, X+38, Y);
            OutTextXY (X+48, Y-3, 'Real');
            SetColor (ImagColor);
            Line (X, Y+13, X+38, Y+13);
            OutTextXY (X+48, Y+10, 'Imaginary');
         END;

      3: BEGIN   { Probability }
            X := Round (0.75*GetMaxX);
            Y := Round (0.20*GetMaxY);
            SetColor (ProbColor);
            Line (X, Y, X+38, Y);
            OutTextXY (X+48, Y-3, 'Prob');
         END;

   END; {of CASE DisplayStyle}

   SetColor (Whitte);

END; {of PROCEDURE ShowDisplayStyle}



PROCEDURE Interpolate (Y1, Y2, Mag1, Mag2, Ph1, Ph2: Real;
                       ISlice: Integer;
                       VAR BdyY, BdyMag, BdyPhase: Real;
                       VAR NextISlice: Integer);
{ Find location of color boundary between two grid points of
  different colors. }
{ This subroutine neither uses nor alters any global variable. }
{ Dependencies: Pi is a Turbo function. }

{ A complex function of variable Y (which will usually be either
  position x or momentum p) has magnitude and phase Mag1 and Ph1 at Y1
  and Mag2 and Ph2 at Y2.  The phases fall into different color slices,
  the one at Y1 being ISlice.  This routine interpolates to
  find the boundary between current and new colors.  (If there are
  several color boundaries between the two points, it finds the one
  closest to Y1).  Output is the estimated location of the boundary and
  magnitude and phase of the function there, and the color value on the
  other side of the boundary.

  We interpolate between points Y1 and Y2 by assuming that
       phase(Y) = (phase(Y2) - phase(Y1))*(Y-Y1)/(Y2-Y1)  + phase(Y1),
  and similarly for magnitude.  This is NOT the same as linear interpolation
  of real and imaginary parts.

  We assume that the change in phase between Y1 and Y2 is less than or
  equal to pi, i.e., that it is the minimum possible. }

VAR
   Phase1, Phase2, temp: Real;

BEGIN

   Phase1 := Ph1;
   Phase2 := Ph2;

   { If the phase crosses through zero, adjust to make angles continuous
     (i.e. make Phase1, Phase2, and BdyPhase all on the same branch). }
   IF (Abs (Phase1-Phase2) > Pi) THEN
      IF (Phase1 > Pi) THEN
         IF (ISlice <> 1) THEN Phase2 := Phase2 + 2*Pi
            ELSE Phase1 := Phase1 - 2*Pi
      ELSE
         IF (ISlice <> 1) THEN Phase2 := Phase2 - 2*Pi
            ELSE Phase1 := Phase1 + 2*Pi;

   { Find boundary phase where one color stops and the next starts. }
   IF (Phase2 > Phase1) THEN
   BEGIN   { Increasing phase }
      IF (ISlice <> NumColors) THEN NextISlice := ISlice + 1
         ELSE NextISlice := 1;
      BdyPhase := Pi*(2*ISlice - 1)/NumColors;
   END
   ELSE
   BEGIN   { Decreasing phase }
      IF (ISlice <> 1) THEN NextISlice := ISlice - 1
         ELSE NextISlice := NumColors;
      BdyPhase := Pi*(2*NextISlice - 1)/NumColors;
   END;

   { Find location of phase boundary. }
   IF (Phase1 = Phase2) THEN temp := 1.0
      ELSE temp := (BdyPhase - Phase1)/(Phase2 - Phase1);
   BdyY := temp*(Y2 - Y1) + Y1;

   { Find magnitude of Psi at boundary. }
   BdyMag := temp*(Mag2 - Mag1) + Mag1;

END; {of PROCEDURE Interpolate}



PROCEDURE FloodWithColor (Eave, scale: Real);
{ Plot position space wave function using color for phase. }
{ Note: If two adjacent grid points are of different color, we interpolate
  to estimate the proper boundary between colors. }
{ This subroutine does not alter any global variable. }
{ Dependencies: FillPoly, SetFillStyle, and SetColor are Turbo
                   Graph subroutines.
                SolidFill is a Turbo Graph constant.
                GraphBackColor is a CUPS unit variable.
                MapX and MapY are CUPSmupp subroutines. }

VAR
   JD, J, NVertex, ISlice, NewISlice, NextISlice: Integer;
   Point: ARRAY[1..NumDP+2,1..2] OF Integer;
   x, Newx, Bdyx, Magnitude, NewMag, BdyMag,
                  Phase, NewPhase, BdyPhase: Real;

BEGIN
   SetColor (GraphBackColor);

   JD := 1;
   J := FirstDP;
   Magnitude := Sqrt (Sqr (Psi[J,1]) + Sqr (Psi[J,2]));
   ISlice := ColorSlice (Psi[J,1], Psi[J,2], Phase);
   x := xValue[JD];
   Point[1,1] := MapX (x);
   Point[1,2] := MapY (Eave);
   Point[2,1] := MapX (x);
   Point[2,2] := MapY (Eave + scale*Magnitude);
   NVertex := 2;

   FOR JD := 2 TO NumDP DO
   BEGIN

      J := J + 1;
      NewMag := Sqrt (Sqr (Psi[J,1]) + Sqr (Psi[J,2]));
      NewISlice := ColorSlice (Psi[J,1], Psi[J,2], NewPhase);
      Newx := xValue[JD];

      WHILE (ISlice <> NewISlice) DO
      BEGIN
         Interpolate (x, Newx, Magnitude, NewMag, Phase, NewPhase, ISlice,
                      Bdyx, BdyMag, BdyPhase, NextISlice);

         { Close off old color polygon. }
         NVertex := NVertex + 1;
         Point[NVertex,1] := MapX (Bdyx);
         Point[NVertex,2] := MapY (Eave + scale*BdyMag);
         NVertex := NVertex + 1;
         Point[NVertex,1] := MapX (Bdyx);
         Point[NVertex,2] := MapY (Eave);
         SetFillStyle (SolidFill, ColorOf[ISlice]);
         FillPoly (NVertex, Point);

         { Make next color current, begin new polygon. }
         Magnitude := BdyMag;
         ISlice := NextISlice;
         Phase := BdyPhase;
         x := Bdyx;
         Point[1,1] := MapX (x);
         Point[1,2] := MapY (Eave);
         Point[2,1] := MapX (x);
         Point[2,2] := MapY (Eave + scale*Magnitude);
         NVertex := 2;

      END; {of WHILE ISlice}

      Magnitude := NewMag;
      Phase := NewPhase;
      x := Newx;
      NVertex := NVertex + 1;
      Point[NVertex,1] := MapX (x);
      Point[NVertex,2] := MapY (Eave + scale*Magnitude);

   END; {of FOR JD}

   { Close up at last point. }
   NVertex := NVertex + 1;
   Point[NVertex,1] := MapX (xValue[NumDP]);
   Point[NVertex,2] := MapY (Eave);
   SetFillStyle (SolidFill, ColorOf[ISlice]);
   FillPoly (NVertex, Point);

END; {of PROCEDURE FloodWithColor}



PROCEDURE FloodWithColorM;
{ Plot momentum space wave function using color for phase.
  Analogous to FloodWithColor. }
{ This subroutine does not alter any global variable. }
{ Dependencies: FillPoly, SetFillStyle, and SetColor are Turbo
                   Graph subroutines.
                SolidFill is a Turbo Graph constant.
                GraphBackColor is a CUPS unit variable.
                MapX and MapY are CUPSmupp subroutines. }

VAR
   JD, J, NVertex, ISlice, NewISlice, NextISlice: Integer;
   Point: ARRAY[1..NPoints+2,1..2] OF Integer;
   p, Newp, Bdyp, Magnitude, NewMag, BdyMag,
                  Phase, NewPhase, BdyPhase: Real;

BEGIN
   SetColor (GraphBackColor);

   J := 1;
   Magnitude := Sqrt (Sqr (PsiTilda[J,1]) + Sqr (PsiTilda[J,2]));
   ISlice := ColorSlice (PsiTilda[J,1], PsiTilda[J,2], Phase);
   p := pValue[J];
   Point[1,1] := MapX (p);
   Point[1,2] := MapY (0.0);
   Point[2,1] := MapX (p);
   Point[2,2] := MapY (Magnitude);
   NVertex := 2;

   FOR J := 2 TO NPoints DO
   BEGIN

      NewMag := Sqrt (Sqr (PsiTilda[J,1]) + Sqr (PsiTilda[J,2]));
      NewISlice := ColorSlice (PsiTilda[J,1], PsiTilda[J,2], NewPhase);
      Newp := pValue[J];

      WHILE (ISlice <> NewISlice) DO
      BEGIN
         Interpolate (p, Newp, Magnitude, NewMag, Phase, NewPhase, ISlice,
                      Bdyp, BdyMag, BdyPhase, NextISlice);

         { Close off old color polygon. }
         NVertex := NVertex + 1;
         Point[NVertex,1] := MapX (Bdyp);
         Point[NVertex,2] := MapY (BdyMag);
         NVertex := NVertex + 1;
         Point[NVertex,1] := MapX (Bdyp);
         Point[NVertex,2] := MapY (0.0);
         SetFillStyle (SolidFill, ColorOf[ISlice]);
         FillPoly (NVertex, Point);

         { Make next color current, begin new polygon. }
         Magnitude := BdyMag;
         ISlice := NextISlice;
         Phase := BdyPhase;
         p := Bdyp;
         Point[1,1] := MapX (p);
         Point[1,2] := MapY (0.0);
         Point[2,1] := MapX (p);
         Point[2,2] := MapY (Magnitude);
         NVertex := 2;

      END; {of WHILE ISlice}

      Magnitude := NewMag;
      Phase := NewPhase;
      p := Newp;
      NVertex := NVertex + 1;
      Point[NVertex,1] := MapX (p);
      Point[NVertex,2] := MapY (Magnitude);

   END; {of FOR J}

   { Close up at last point. }
   NVertex := NVertex + 1;
   Point[NVertex,1] := MapX (pValue[NPoints]);
   Point[NVertex,2] := MapY (0.0);
   SetFillStyle (SolidFill, ColorOf[ISlice]);
   FillPoly (NVertex, Point);

END; {of PROCEDURE FloodWithColorM}



PROCEDURE PlotWFandPotl (xave, delx: Real);
{ Plot the wave function and the potential energy function. }
{ This subroutine does not alter any global variable. }
{ Dependencies: DataVector is a CUPSmupp data type.
                Whitte is a CUPS unit constant.
                Red, LightBlue, Green, SolidLn, ThickWidth, etc. are
                   Turbo Graph constants.
                SetColor and SetLineStyle are Turbo Graph subroutines.
                All other subroutines are from CUPSmupp. }

VAR
   JD: Integer;
   MeanEValue, scale: Real;
   WFlist: DataVector;

BEGIN

   OpenViewPort (1);
   SelectScale (1);
   Axis (xleft, -1.0, 1.0, 1.0);
   PutLabel (Bottom, 'Position (nm)');
   PutLabel (Left, 'Potential (eV)');
   scale := 1.0;
   IF (DisplayContent <> 1) THEN scale := 2.0;

   { Draw wave function axis. }
   MeanEValue := MeanE/CeVtoScaled;
   SetColor (LightBlue);
   IF (MeanEValue > 10.0) OR (MeanEValue < -1.0) THEN
   BEGIN
      MeanEValue := 0.0;
      SetColor (Red);
   END;
   PlotLine (xleft, MeanEValue, xright, MeanEValue);

   { Draw wave function. }
   CASE DisplayStyle OF

      1: FloodWithColor (MeanEValue, scale);   { Color for phase }

      2: BEGIN   { Real and imaginary parts of psi }
            FOR JD := 1 TO NumDP DO
               WFlist[JD] := scale*Psi[JD+FirstDP-1,1] + MeanEValue;
            SetColor (RealColor);
            PlotData (xValue, WFlist, NumDP);
            FOR JD := 1 TO NumDP DO
               WFlist[JD] := scale*Psi[JD+FirstDP-1,2] + MeanEValue;
            SetColor (ImagColor);
            PlotData (xValue, WFlist, NumDP);
         END;

      3: BEGIN   { Square magnitude of psi (probability density) }
            FOR JD := 1 TO NumDP DO
               WFlist[JD] := scale*(Sqr (Psi[JD+FirstDP-1,1])
                                  + Sqr (Psi[JD+FirstDP-1,2])) + MeanEValue;
            SetColor (ProbColor);
            PlotData (xValue, WFlist, NumDP);
         END;

   END; {of CASE DisplayStyle}

   { Draw potential. }
   SetColor (Green);
   SetLineStyle (SolidLn, 0, ThickWidth);
   PlotData (xValue, VD, NumDP);
   SetLineStyle (SolidLn, 0, NormWidth);

   { Display mean and uncertainty of position. }
   IF (ShowMeans > 0) THEN
   BEGIN
      { Draw vertical line at mean position. }
      SetColor (LightGray);
      PlotLine (xave, -1.0, xave, +10.0);
   END;
   IF (ShowUncertainties > 0) THEN
   BEGIN
      { Draw fat horizontal line to represent uncertainty. }
      SetColor (LightGray);
      SetLineStyle (SolidLn, 0, ThickWidth);
      PlotLine (xave - delx, MeanEValue, xave + delx, MeanEValue);
      SetLineStyle (SolidLn, 0, NormWidth);
   END;

   { Tidy up. }
   SetColor (Whitte);

END; {of PROCEDURE PlotWFandPotl}



PROCEDURE PlotMomentumWF (pave, delp: Real);
{ Plot the momentum representation of the wave function. }
{ This subroutine does not alter any global variable. }
{ Dependencies: DataVector is a CUPSmupp data type.
                Whitte is a CUPS unit constant.
                LightBlue, SolidLn, ThickWidth, etc. are
                   Turbo Graph constants.
                SetColor and SetLineStyle are Turbo Graph subroutines.
                All other subroutines are from CUPSmupp. }

VAR
   J: Integer;
   WFlist: DataVector;

BEGIN

   OpenViewPort (2);
   SelectScale (2);
   Axis (-30.0, -0.5, 10.0, 0.5);
   PutLabel (Bottom, 'Momentum/hbar (nm^-1)');

   { Draw wave function axis. }
   SetColor (LightBlue);
   PlotLine (-30.0, 0.0, +30.0, 0.0);

   { Draw wave function. }
   CASE DisplayStyle OF

      1: FloodWithColorM;   { Color for phase }

      2: BEGIN   { Real and imaginary parts of psi }
            FOR J := 1 TO NPoints DO
               WFlist[J] := PsiTilda[J,1];
            SetColor (RealColor);
            PlotData (pValue, WFlist, NPoints);
            FOR J := 1 TO NPoints DO
               WFlist[J] := PsiTilda[J,2];
            SetColor (ImagColor);
            PlotData (pValue, WFlist, NPoints);
         END;

      3: BEGIN   { Square magnitude of psi (probability density) }
            FOR J := 1 TO NPoints DO
               WFlist[J] := Sqr (PsiTilda[J,1]) + Sqr (PsiTilda[J,2]);
            SetColor (ProbColor);
            PlotData (pValue, WFlist, NPoints);
         END;

   END; {of CASE DisplayStyle}

   { Display mean and uncertainty of momentum. }
   IF (ShowMeans > 0) THEN
   BEGIN
      { Draw vertical line at mean momentum. }
      SetColor (LightGray);
      PlotLine (pave, -0.5, pave, +0.5);
   END;
   IF (ShowUncertainties > 0) THEN
   BEGIN
      { Draw fat horizontal line to represent uncertainty. }
      SetColor (LightGray);
      SetLineStyle (SolidLn, 0, ThickWidth);
      PlotLine (pave - delp, 0.0, pave + delp, 0.0);
      SetLineStyle (SolidLn, 0, NormWidth);
   END;

   SetColor (Whitte);

END; {of PROCEDURE PlotMomentumWF}



PROCEDURE PlotEnergySpectrum;
{ Plot the energy spectrum of the wave function. }
{ This subroutine does not alter any global variable. }
{ Dependencies: SolidFill is a Turbo Graph constant.
                SetFillStyle and Bar are Turbo Graph subroutines.
                All other subroutines are from CUPSmupp. }

VAR
   n: Integer;
   Magnitude, Phase, Prob: Real;

BEGIN

   OpenViewPort (3);
   SelectScale (3);
   IF (DisplayStyle <> 3) THEN Axis (0.0, -0.5, 1.0, 0.5)
      ELSE Axis (0.0, 0.0, 1.0, 0.5);
   PutLabel (Bottom, 'Energy state');

   { Plot bars corresponding to energy expansion coefficients. }
   CASE DisplayStyle OF

      1: BEGIN   { Color for phase }
            FOR n := 0 TO nTop DO
            BEGIN
               Magnitude := Sqrt (Sqr (Coeff[n,1]) + Sqr (Coeff[n,2]));
               SetFillStyle (SolidFill,
                  ColorOf[ColorSlice (Coeff[n,1], Coeff[n,2], Phase)]);
               Bar (MapX(n), MapY(0.0), MapX(n+1), MapY(Magnitude));
            END; {of FOR n}
         END;

      2: BEGIN   { Real and imaginary parts of psi }
            FOR n := 0 TO nTop DO
            BEGIN
               SetFillStyle (SolidFill, RealColor);
               Bar (MapX(n), MapY(0.0), MapX(n+0.5), MapY(Coeff[n,1]));
               SetFillStyle (SolidFill, ImagColor);
               Bar (MapX(n+0.5), MapY(0.0), MapX(n+1), MapY(Coeff[n,2]));
            END; {of FOR n}
         END;

      3: BEGIN   { Square magnitude of psi (probability) }
            FOR n := 0 TO nTop DO
            BEGIN
               Prob := Sqr (Coeff[n,1]) + Sqr (Coeff[n,2]);
               SetFillStyle (SolidFill, ProbColor);
               Bar (MapX(n), MapY(0.0), MapX(n+1), MapY(Prob));
            END; {of FOR n}
         END;

   END; {of CASE DisplayStyle}

END; {of PROCEDURE PlotEnergySpectrum}



PROCEDURE Plot;
{ This subroutine does not alter any global variable. }
{ Dependencies: HideMouse, ShowMouse, and NumStr are CUPS unit subroutines.
                GraphBackColor is a CUPS unit constant.
                RubOutXY and PrintXY are CUPSmupp subroutines. }

VAR
   xave, delx, pave, delp: Real;

BEGIN

   IF (ShowMeans > 0) OR (ShowUncertainties > 0) THEN
      FindMeanXP (xave, delx, pave, delp);

   IF (NOT Running) THEN HideMouse;

   IF (DisplayContent = 0) THEN    { Clear old time and uncertainites. }
   BEGIN
      RubOutXY (0.61, 0.85, 16, GraphBackColor);
      IF (ShowUncertainties > 0) THEN
      BEGIN
         RubOutXY (0.14, 0.81, 31, GraphBackColor);
         RubOutXY (0.14, 0.77, 31, GraphBackColor);
         RubOutXY (0.14, 0.73, 31, GraphBackColor);
      END;
   END;

   IF Odd(DisplayContent) THEN   { Plot Psi(x). }
      PlotWFandPotl (xave, delx);

   IF Odd(DisplayContent DIV 2) THEN   { Find and plot PsiTilda(p). }
   BEGIN
      FindMomentumWF;
      PlotMomentumWF (pave, delp);
   END;

   IF Odd(DisplayContent DIV 4) THEN   { Find and plot energy spectrum. }
   BEGIN
      FindEnergySpectrum;
      PlotEnergySpectrum;
   END;

   { Output the time and a sample of the complex number display style. }
   PrintXY (0.61, 0.85, Concat ('Time: ', NumStr (DtSI*Itime,10,3), ' fsec'));
   ShowDisplayStyle;
   { Possibly output uncertainties and driving force message. }
   IF (ForceF <> 0.0) THEN PrintXY (0.14, 0.85, 'Driving force is ON');
   IF (ShowUncertainties > 0) THEN
   BEGIN
      PrintXY (0.14, 0.81,
         Concat ('Delta x:                 ', NumStr (delx, 6, 2)));
      PrintXY (0.14, 0.77,
         Concat ('(Delta p)/hbar:          ', NumStr (delp, 6, 2)));
      PrintXY (0.14, 0.73,
         Concat ('(Delta x)(Delta p)/hbar: ', NumStr (delx*delp, 6, 2)));
   END;

   IF (NOT Running) THEN ShowMouse;

END; {of PROCEDURE Plot}



{---Process the potential----------------------------------------------------}

PROCEDURE BuildPotl;
{ Fill the array V[J] with values of the active potential. }
{ This subroutine alters the state global variables V, VD, Jstart,
  and Jstop. }

VAR
   J, JD: Integer;
   x: Real;

BEGIN
   x := xmin;
   FOR J := 1 TO NPoints DO
   BEGIN

      CASE ActivePotl OF
         1: { Flat }
            V[J] := PotlPara[1,1];
         2: { Ramp }
            V[J] := (PotlPara[2,1]/(xright-xleft))*(x-xleft);
         3: { Step }
            IF (x < PotlPara[3,2]) THEN V[J] := 0.0
               ELSE V[J] := PotlPara[3,1];
         4: { Square barrier }
            IF (Abs(x) > 0.5*PotlPara[4,2]) THEN V[J] := 0.0
               ELSE V[J] := PotlPara[4,1];
         5: { Notch barrier }
            IF (Abs(x) >= 0.5*PotlPara[5,2]) THEN V[J] := 0.0
               ELSE IF (Abs(x) > PotlPara[5,2]*(0.5 - PotlPara[5,3])) THEN
                  V[J] := - (PotlPara[5,1]/PotlPara[5,3])
                            * (Abs(x)/PotlPara[5,2] - 0.5)
               ELSE V[J] := PotlPara[5,1];
         6: { Gaussian barrier }
            V[J] := PotlPara[6,1] * Exp (-x*x);
         7: { Two square barriers }
            IF  (Abs(x - 1.0) > 0.5*PotlPara[7,2])
            AND (Abs(x + 1.0) > 0.5*PotlPara[7,2]) THEN V[J] := 0.0
                    ELSE V[J] := PotlPara[7,1];
         8: { Infinite square well }
            IF (x > PotlPara[8,1]) AND
                  (x < PotlPara[8,1] + PotlPara[8,2]) THEN V[J] := 0.0
               ELSE V[J] := Infinity;
         9: { Finite square well }
            IF (Abs(x) > 0.5*PotlPara[9,2]) THEN V[J] := PotlPara[9,1]
               ELSE V[J] := 0.0;
         10: { Notch well }
            IF (Abs(x) >= 0.5*PotlPara[10,2]) THEN V[J] := PotlPara[10,1]
               ELSE IF (Abs(x) > PotlPara[10,2]*(0.5 - PotlPara[10,3])) THEN
                  V[J] := (PotlPara[10,1]/PotlPara[10,3])
                            * (Abs(x)/PotlPara[10,2] + PotlPara[10,3] - 0.5)
               ELSE V[J] := 0.0;
         11: { Double square well }
            IF (Abs(x) > 1.5) THEN V[J] := Infinity
               ELSE IF (Abs(x) < 0.5) THEN V[J] := PotlPara[11,1]
                  ELSE V[J] := 0.0;
         12: { Harmonic oscillator }
            V[J] := 0.5*PotlPara[12,1]*Sqr(x);
         13: { Quartic oscillator }
            V[J] := 0.25*PotlPara[13,1]*Sqr(Sqr(x));
         14: { User defined }
            V[J] := UserDefinedPotential(x);
      END; {of CASE ActivePotl}

      x := x + Dx;
   END; {of FOR J}

   { Place infinite barriers at edges..."edge walls". }
   V[1]       := Infinity;
   V[NPoints] := Infinity;

   { Transfer values within the display window into VD. }
   FOR JD := 1 TO NumDP DO
      VD[JD] := V[JD+FirstDP-1];

   { Find first and last points of finite potential. }
   FOR J := 1 TO NPoints DO
   BEGIN
      IF (V[J] <> Infinity) THEN
      BEGIN
         IF (V[J-1] = Infinity) THEN Jstart := J;
         IF (V[J+1] = Infinity) THEN Jstop := J;
      END;
   END; {of FOR J}

   { Convert V to scaled units. }
   FOR J := Jstart TO Jstop DO
      V[J] := CeVtoScaled * V[J];

END; {of PROCEDURE BuildPotl}



{---Process the wave function------------------------------------------------}

PROCEDURE NormalizeWF;
{ Clip the current wave function where the potential is infinite, then
  normalize it. }
{ This subroutine alters the state global variable Psi. }

VAR
   J: Integer;
   temp: Real;

BEGIN

   { Clip. }
   FOR J := 1 TO NPoints DO
   BEGIN
      IF (V[J] = Infinity) THEN
      BEGIN
         Psi[J,1] := 0.0; Psi[J,2] := 0.0;
      END;
   END;

   { Normalize. }
   temp := 0.0;
   FOR J := 1 TO NPoints DO
   BEGIN
      temp := temp + Sqr (Psi[J,1]) + Sqr (Psi[J,2]);
   END;
   temp := Sqrt (temp*Dx);
   IF (temp <> 0.0) THEN
   BEGIN
      FOR J := 1 TO NPoints DO
      BEGIN
         Psi[J,1] := Psi[J,1]/temp;  Psi[J,2] := Psi[J,2]/temp;
      END;
   END; {of IF temp}

END; {of PROCEDURE NormalizeWF}



PROCEDURE BuildWFPacket;
{ Fill Psi[J,1..2] with a Gaussian, Lorentzian, or tent wave packet. }
{ This subroutine alters the state global variable Psi. }
{ Dependencies: Announce is a CUPS unit subroutine. }

VAR
   J: Integer;
   x, xave, delx, Eave, E0, pave, Ltent: Real;

BEGIN

   xave := InitWFPara[ActivePotl,1];
   delx := InitWFPara[ActivePotl,2];
   Eave := InitWFPara[ActivePotl,3] * CeVtoScaled;
   IF (ActiveInitWF = 3) THEN Ltent := Sqrt(10.0) * delx;

   { Fill Psi with wave function of the desired type and with the desired
     xave and delx, but with mean momentum zero. }
   x := xmin;
   FOR J := 1 TO NPoints DO
   BEGIN
      CASE ActiveInitWF OF
         1: { Gaussian }
            Psi[J,1] := Exp ( - (x-xave)*(x-xave) / (4.0*delx*delx) );
         2: { Lorentzian }
            Psi[J,1] := 1.0 / ( (x-xave)*(x-xave) + delx*delx );
         3: { tent }
            IF (Abs(x-xave) < Ltent) THEN Psi[J,1] := Ltent - Abs(x-xave)
               ELSE Psi[J,1] := 0.0;
      END; {of CASE ActiveInitWF}
      Psi[J,2] := 0.0;
      x := x + Dx;
   END; {of FOR J}

   NormalizeWF;

   { Find pave of the desired wave function using E(p) = E(0) + p^2/2m,
     then, if possible, fill Psi with the desired wave function. }
   E0 := MeanE;
   IF (E0 <= Eave) THEN
   BEGIN

      pave := Sqrt (2.0*(Eave-E0));
         { You could also take the negative square root. }

      IF (pave > 1.0/Dx) THEN
      BEGIN   { Requested energy is too high. }
         Announce (Concat ('This wave packet varies too rapidly to be ',
                           'accurately approximated on the grid.'));
         { Better would be 'The wave packet with this energy varies...',
           but that doesn't fit on the screen. }
         Announce (Concat ('The most energetic wave packet that can be ',
                           'well-approximated is used instead.'));
         pave := 1.0/Dx;
         InitWFPara[ActivePotl,3] := (E0 + Sqr(pave)/2.0)/CeVtoScaled;
      END; {of IF pave}

      x := xmin;
      FOR J := 1 TO NPoints DO
      BEGIN
         Psi[J,2] := Psi[J,1] * Sin (pave*x);
         Psi[J,1] := Psi[J,1] * Cos (pave*x);
         x := x + Dx;
      END; {of FOR J}

   END
   ELSE
   BEGIN

      { For the given potential, wave packet type, xave, and delx,
        there is no wave packet of the desired energy...it is too
        low.  So we substitute a wave packet with the minimum
        possible energy, i.e. one with zero momentum.  This happens
        to be the wave packet we've just computed! }

      InitWFPara[ActivePotl,3] := E0/CeVtoScaled;
      Announce ('There is no wave packet with the desired energy.');
      Announce (Concat ('A wave packet of the minimum possible ',
                        'energy is used instead.'));

   END;

END; {of PROCEDURE BuildWFPacket}



PROCEDURE BuildWFCombo;
{ Fill Psi[J,1..2] with a linear combination of energy eigenfunctions.
  Used for infinite square well and harmonic oscillator potentials only. }
{ This subroutine alters the state global variable Psi. }
{ Dependencies: Pi is a Turbo function. }

VAR
   J: Integer;
   x, xi, temp: Real;

BEGIN

   IF (ActivePotl = InfWell) THEN
   BEGIN   { Infinite square well }

      x := xmin;
      FOR J := 1 TO NPoints DO
      BEGIN
         temp := Pi*(x - PotlPara[InfWell,1])/PotlPara[InfWell,2];
         Psi[J,1] := InitWFCombo[1,1] * Sin (1*temp)
                   + InitWFCombo[1,3] * Sin (2*temp)
                   + InitWFCombo[1,5] * Sin (3*temp);
         Psi[J,2] := InitWFCombo[1,2] * Sin (1*temp)
                   + InitWFCombo[1,4] * Sin (2*temp)
                   + InitWFCombo[1,6] * Sin (3*temp);
         x := x + Dx;
      END; {of FOR J}

   END
   ELSE
   BEGIN   { Harmonic oscillator }

      x := xmin;
      FOR J := 1 TO NPoints DO
      BEGIN
         xi := Sqrt (Sqrt (CeVtoScaled*PotlPara[SHO,1])) * x;
         temp := Exp ( -0.5 * xi*xi);
         { The energy eigenfunctions are unnormalized because
           normalization will be taken care of by NormalizeWF. }
         Psi[J,1] :=
              InitWFCombo[2,1] * temp
            + InitWFCombo[2,3] * Sqrt(2.0) * xi * temp
            + InitWFCombo[2,5] * (2.0*xi*xi - 1.0) * temp / Sqrt(2.0);
         Psi[J,2] :=
              InitWFCombo[2,2] * temp
            + InitWFCombo[2,4] * Sqrt(2.0) * xi * temp
            + InitWFCombo[2,6] * (2.0*xi*xi - 1.0) * temp / Sqrt(2.0);
         x := x + Dx;
      END; {of FOR J}

   END;

   NormalizeWF;

END; {of PROCEDURE BuildWFCombo}



PROCEDURE BuildWF;
{ Fill Psi[J,1..2] with the desired initial wave function. }
{ This subroutine alters the state global variable Psi. }

VAR
   J: Integer;
   x: Real;

BEGIN

   CASE ActiveInitWF OF
      1,2,3: BuildWFPacket;
      4: BuildWFCombo;
      5: BEGIN  { User defined wave function }
            x := xmin;
            FOR J := 1 TO NPoints DO
            BEGIN
               UserDefinedWF (x, Psi[J,1], Psi[J,2]);
               x := x + Dx;
            END;

            NormalizeWF;
         END;
   END; {of CASE ActiveInitWF}

END; {of PROCEDURE BuildWF}



{---Alter the wave function--------------------------------------------------}

PROCEDURE ShiftWF;
{ Get a distance from the screen and then translate the wave function by
  that distance.  Use periodic boundary conditions. }
{ This subroutine alters the state global variable Psi. }
{ Dependencies: Input screen subroutines are from CUPSgui. }

VAR
   NShift, J: Integer;
   ShiftedPsi: ARRAY[1..NPoints, 1..2] OF Real;

BEGIN
WITH InputScreen DO
BEGIN

   Init;
   DefineInputPort (0.15, 0.85, 0.30, 0.60);

   LoadLine ('  ');
   LoadLine (' Shift the wave function right by {     } nm. ');
   LoadLine ('  ');
   LoadLine ('  [  Ok  ]   [Cancel] ');

   SetNumber (1, -1.0);

   AcceptScreen;

   IF (NOT Canceled) THEN
   BEGIN

      NShift := Round (GetNumber(1)/Dx);
      NShift := NShift MOD NPoints;
      IF (NShift < 0) THEN NShift := NShift + NPoints;

      FOR J := 1 TO NPoints - NShift DO
      BEGIN
         ShiftedPsi[J+NShift,1] := Psi[J,1];
         ShiftedPsi[J+NShift,2] := Psi[J,2];
      END; {of FOR J}

      FOR J := NPoints - NShift + 1 TO NPoints DO
      BEGIN
         ShiftedPsi[J+NShift-NPoints,1] := Psi[J,1];
         ShiftedPsi[J+NShift-NPoints,2] := Psi[J,2];
      END; {of FOR J}

      FOR J := 1 TO NPoints DO
      BEGIN
         Psi[J,1] := ShiftedPsi[J,1];
         Psi[J,2] := ShiftedPsi[J,2];
      END; {of FOR J}

      NormalizeWF;
      Plot;

   END;

   Done;

END; {of WITH InputScreen}
END; {of PROCEDURE ShiftWF}



PROCEDURE StretchWF;
{ Get a stretch factor from the screen and then stretch the wave function
  about position "xCenter" by that factor.  For a compression, use a stretch
  factor less than one.  For a reflection, use a stretch factor of -1. }
{ This subroutine alters the state global variable Psi. }
{ Dependencies: Input screen subroutines are from CUPSgui.
                Macheps is a CUPS unit variable.
                Announce is a CUPS unit subroutine. }

VAR
   J, Jfloor: Integer;
   xCenter, factor, constant, Jexact, d: Real;
   StretchedPsi: ARRAY[1..NPoints, 1..2] OF Real;

BEGIN
WITH InputScreen DO
BEGIN

   xCenter := MeanX;

   Init;
   DefineInputPort (0.15, 0.85, 0.30, 0.60);

   LoadLine ('  ');
   LoadLine (' Stretch the wave function by a factor of  {     } ');
   LoadLine ('  ');
   LoadLine ('  [  Ok  ]   [Cancel] ');

   SetNumber (1, 0.5);

   AcceptScreen;

   IF (NOT Canceled) THEN
   BEGIN

      factor := GetNumber(1);
      IF (Abs(factor) <= Macheps) THEN
         Announce ('Cannot stretch by zero.')
      ELSE
      BEGIN

         constant := (xmin/Dx - 1.0)/factor
                     + (1.0 - 1.0/factor)*(xCenter/Dx) - xmin/Dx + 1.0;
         FOR J := Jstart TO Jstop DO
         BEGIN
            Jexact := constant + J/factor;
            Jfloor := Trunc(Jexact);
            IF (1 <= Jfloor) AND (Jfloor <= NPoints - 1) THEN
            BEGIN
               d := Jexact - Jfloor;
               StretchedPsi[J,1] := (1.0-d)*Psi[Jfloor,1] + d*Psi[Jfloor+1,1];
               StretchedPsi[J,2] := (1.0-d)*Psi[Jfloor,2] + d*Psi[Jfloor+1,2];
            END
            ELSE
            BEGIN
               StretchedPsi[J,1] := 0.0;
               StretchedPsi[J,2] := 0.0;
            END;
         END; {of FOR J}

         FOR J := Jstart TO Jstop DO
         BEGIN
            Psi[J,1] := StretchedPsi[J,1];
            Psi[J,2] := StretchedPsi[J,2];
         END; {of FOR J}

         NormalizeWF;
         Plot;

      END; {of IF Abs(factor)}

   END; {of IF NOT Canceled}

   Done;

END; {of WITH InputScreen}
END; {of PROCEDURE StretchWF}



PROCEDURE ReflectWF;
{ Reflect the wave function about position "xCenter". }
{ This subroutine alters the state global variable Psi. }

VAR
   Jpivot, Jleft, Jright: Integer;
   xCenter, swap: Real;

BEGIN

   xCenter := MeanX;

   Jpivot := Round ((xCenter - xmin)/Dx) + 1;

   IF (Jstop-Jpivot > Jpivot-Jstart) THEN
   BEGIN

      FOR Jright := Jpivot+1 TO Jstop DO
      BEGIN
         Jleft := 2*Jpivot - Jright;
         IF (Jleft >= Jstart) THEN
         BEGIN
            swap := Psi[Jleft,1];
            Psi[Jleft,1] := Psi[Jright,1];
            Psi[Jright,1] := swap;
            swap := Psi[Jleft,2];
            Psi[Jleft,2] := Psi[Jright,2];
            Psi[Jright,2] := swap;
         END
         ELSE
         BEGIN
            Psi[Jright,1] := 0.0;
            Psi[Jright,2] := 0.0;
         END;
      END; {of FOR Jright}

   END
   ELSE
   BEGIN

      FOR Jleft := Jstart TO Jpivot-1 DO
      BEGIN
         Jright := 2*Jpivot - Jleft;
         IF (Jright <= Jstop) THEN
         BEGIN
            swap := Psi[Jleft,1];
            Psi[Jleft,1] := Psi[Jright,1];
            Psi[Jright,1] := swap;
            swap := Psi[Jleft,2];
            Psi[Jleft,2] := Psi[Jright,2];
            Psi[Jright,2] := swap;
         END
         ELSE
         BEGIN
            Psi[Jleft,1] := 0.0;
            Psi[Jleft,2] := 0.0;
         END;
      END; {of FOR Jleft}

   END;

   NormalizeWF;
   Plot;

END; {of PROCEDURE ReflectWF}



PROCEDURE ConjugateWF;
{ Replace the wave function with its complex conjugate. }
{ This subroutine alters the state global variable Psi. }

VAR
   J: Integer;

BEGIN

   FOR J := Jstart TO Jstop DO
      Psi[J,2] := - Psi[J,2];

   Plot;

END; {of PROCEDURE ConjugateWF}



{---Input--------------------------------------------------------------------}

PROCEDURE GetPotlPara (PotlNum: Integer; VAR DoIt: Boolean);
{ Produce and read pop up screen for the chosen potential. }
{ This subroutine alters the profile global variable PotlPara. }
{ Dependencies: All subroutines are from CUPSgui. }

BEGIN
WITH InputScreen DO
BEGIN

   Init;
   DefineInputPort (0.15, 0.85, 0.30, 0.60);

   CASE PotlNum OF

      1: BEGIN
            LoadLine ('  ');
            LoadLine (' Energy of flat potential:   {     }');
            LoadLine ('  ');
            LoadLine ('  [  Ok  ]   [Cancel] ');
            SetNumber (1, PotlPara[PotlNum,1]);
         END;

      2: BEGIN
            LoadLine ('  ');
            LoadLine (' Energy height of ramp at right edge:   {     }');
            LoadLine ('  ');
            LoadLine ('  [  Ok  ]   [Cancel] ');
            SetNumber (1, PotlPara[PotlNum,1]);
         END;

      3: BEGIN
            LoadLine ('    Step potential   ');
            LoadLine ('                     ');
            LoadLine (' Energy height:   {     }   Position:   {     } ');
            LoadLine ('  [  Ok  ]   [Cancel] ');
            SetNumber (1, PotlPara[PotlNum,1]);
            SetNumber (2, PotlPara[PotlNum,2]);
         END;

      4: BEGIN
            LoadLine ('    Square barrier   ');
            LoadLine ('                     ');
            LoadLine (' Energy height:   {     }   Width:   {     } ');
            LoadLine ('  [  Ok  ]   [Cancel] ');
            SetNumber (1, PotlPara[PotlNum,1]);
            SetNumber (2, PotlPara[PotlNum,2]);
         END;

      5: BEGIN
            LoadLine ('    Notch barrier    ');
            LoadLine (' Energy height:   {     }   Width:   {     } ');
            LoadLine (' Walls slope over {     } of width.          ');
            LoadLine ('  [  Ok  ]   [Cancel] ');
            SetNumber (1, PotlPara[PotlNum,1]);
            SetNumber (2, PotlPara[PotlNum,2]);
            SetNumber (3, PotlPara[PotlNum,3]);
         END;

      6: BEGIN
            LoadLine ('  ');
            LoadLine (' Energy height of Gaussian barrier:   {     }');
            LoadLine ('  ');
            LoadLine ('  [  Ok  ]   [Cancel] ');
            SetNumber (1, PotlPara[PotlNum,1]);
         END;

      7: BEGIN
            LoadLine ('    Two square barriers, each with   ');
            LoadLine ('  ');
            LoadLine (' Energy height:   {     }   Width:   {     } ');
            LoadLine ('  [  Ok  ]   [Cancel] ');
            SetNumber (1, PotlPara[PotlNum,1]);
            SetNumber (2, PotlPara[PotlNum,2]);
         END;

      8: BEGIN
            LoadLine ('    Infinite square well    ');
            LoadLine ('                            ');
            LoadLine (' Position of left side:  {     }    Width:  {     }');
            LoadLine ('  [  Ok  ]   [Cancel] ');
            SetNumber (1, PotlPara[PotlNum,1]);
            SetNumber (2, PotlPara[PotlNum,2]);
            SetNumberLimits (1, xleft, xright);
            SetNumberLimits (2, 0.1, xright-xleft);
         END;

      9: BEGIN
            LoadLine ('    Finite square well      ');
            LoadLine ('                            ');
            LoadLine (' Energy depth:   {     }   Width:   {     } ');
            LoadLine ('  [  Ok  ]   [Cancel] ');
            SetNumber (1, PotlPara[PotlNum,1]);
            SetNumber (2, PotlPara[PotlNum,2]);
         END;

      10: BEGIN
             LoadLine ('    Notch well       ');
             LoadLine (' Energy depth:   {     }   Width:   {     } ');
             LoadLine (' Walls slope over {     } of width.         ');
             LoadLine ('  [  Ok  ]   [Cancel] ');
             SetNumber (1, PotlPara[PotlNum,1]);
             SetNumber (2, PotlPara[PotlNum,2]);
             SetNumber (3, PotlPara[PotlNum,3]);
          END;

      11: BEGIN
             LoadLine ('    Double square well   ');
             LoadLine ('                         ');
             LoadLine (' Energy height of central barrier:   {     }');
             LoadLine ('  [  Ok  ]   [Cancel] ');
             SetNumber (1, PotlPara[PotlNum,1]);
          END;

      12: BEGIN
             LoadLine ('    V(x) = (K/2) x^2    ');
             LoadLine ('                        ');
             LoadLine (' Stiffness (K) of harmonic oscillator:   {     }');
             LoadLine ('  [  Ok  ]   [Cancel] ');
             SetNumber (1, PotlPara[PotlNum,1]);
          END;

      13: BEGIN
             LoadLine ('    V(x) = (K/4) x^4    ');
             LoadLine ('                        ');
             LoadLine (' Stiffness (K) of quartic oscillator:   {     }');
             LoadLine ('  [  Ok  ]   [Cancel] ');
             SetNumber (1, PotlPara[PotlNum,1]);
          END;

   END; {of CASE PotlNum}

   AcceptScreen;

   DoIt := NOT Canceled;
   IF (DoIt) THEN
   BEGIN
      PotlPara[PotlNum,1] := GetNumber (1);
      IF (PotlPara[PotlNum,2] <> 0.0) OR (PotlNum = 3) THEN
         PotlPara[PotlNum,2] := GetNumber (2);
      IF (PotlNum = 5) OR (PotlNum = 10) THEN
         PotlPara[PotlNum,3] := GetNumber (3);
   END;

   Done;

END; {of WITH InputScreen}
END; {of PROCEDURE GetPotlPara}



PROCEDURE GetDrivingPara;
{ Produce and read pop up screen for driving parameters. }
{ This subroutine alters the profile global variables ForceF and
  Forcew. }
{ Dependencies: All subroutines are from CUPSgui. }

BEGIN
WITH InputScreen DO
BEGIN

   Init;
   DefineInputPort (0.15, 0.85, 0.30, 0.80);

   LoadLine ('                                     ');
   LoadLine (' Driving force is  F sin(wt) where   ');
   LoadLine ('                                     ');
   LoadLine ('    Amplitude F is         {     }eV/nm   (try 5.0) ');
   LoadLine ('    Angular frequency w is {     }fsec^-1           ');
   LoadLine ('                                     ');
   LoadLine ('  [  Ok  ]   [Cancel]                ');

   SetNumber (1, ForceF);
   SetNumber (2, Forcew);
   SetNumberLimits (2, -2.0/Dt, +2.0/Dt);

   AcceptScreen;

   IF (NOT Canceled) THEN
   BEGIN
      ForceF := GetNumber (1);
      Forcew := GetNumber (2);
   END;

   Done;

END; {of WITH InputScreen}
END; {of PROCEDURE GetDrivingPara}



PROCEDURE GetWFPara (WFNum: Integer; VAR DoIt: Boolean);
{ Produce and read pop up screen for the chosen initial wave function type. }
{ This subroutine alters the profile global variables InitWFPara
  and InitWFCombo. }
{ Dependencies: All subroutines are from CUPSgui. }

VAR
   I, IP: Integer;

BEGIN
WITH InputScreen DO
BEGIN

   Init;

   IF (WFNum <= 3) THEN
   BEGIN

      DefineInputPort (0.15, 0.85, 0.30, 0.80);
      LoadLine ('                                              ');
      CASE WFNum OF
         1: LoadLine (' Initial wave packet will be Gaussian with   ');
         2: LoadLine (' Initial wave packet will be Lorentzian with ');
         3: LoadLine (' Initial wave packet will be a tent with     ');
      END;
      LoadLine ('                                              ');
      LoadLine ('    Mean position:             {    }         ');
      LoadLine ('    Uncertainty in position:   {    }         ');
      LoadLine ('    Mean energy:               {    }         ');
      LoadLine ('                                              ');
      LoadLine ('  [  Ok  ]   [Cancel]                         ');

      FOR I := 1 TO 3 DO
         SetNumber (I, InitWFPara[ActivePotl,I]);
      SetNumberLimits (1, xmin, xmax);
      SetNumberLimits (2, 0.1, xmax-xmin);

      AcceptScreen;

      DoIt := NOT Canceled;
      IF (DoIt) THEN
      BEGIN
         FOR I := 1 TO 3 DO
            InitWFPara[ActivePotl,I] := GetNumber (I);
      END; {of IF DoIt}

   END
   ELSE
   BEGIN

      DefineInputPort (0.15, 0.90, 0.18, 0.88);
      LoadLine ('                                                         ');
      LoadLine (' Initial wave function will be a linear combination      ');
      LoadLine (' of the three lowest lying energy eigenfunctions.        ');
      LoadLine ('                                                         ');
      LoadLine (' Enter the complex expansion coefficients below.         ');
      LoadLine (' (They do not need to be normalized.)                    ');
      LoadLine ('                                                         ');
      LoadLine ('    Ground state coefficient:          {    } + i {    } ');
      LoadLine ('    First excited state coefficient:   {    } + i {    } ');
      LoadLine ('    Second excited state coefficient:  {    } + i {    } ');
      LoadLine ('                                                         ');
      LoadLine ('  [  Ok  ]   [Cancel]                                    ');

      IF (ActivePotl = InfWell) THEN IP := 1
         ELSE IP := 2;

      FOR I := 1 TO 6 DO
         SetNumber (I, InitWFCombo[IP,I]);

      AcceptScreen;

      DoIt := NOT Canceled;
      IF (DoIt) THEN
      BEGIN
         FOR I := 1 TO 6 DO
            InitWFCombo[IP,I] := GetNumber (I);
      END; {of IF DoIt}

   END;

   Done;

END; {of WITH InputScreen}
END; {of PROCEDURE GetWFPara}



PROCEDURE ProcessDisplay (RowChosen: Integer);
{ Produce and read pop up screen for Display. }
{ This subroutine alters the profile global variables DisplayStyle,
  DisplayContent, ShowMeans, and ShowUncertainties. }
{ Dependencies: Announce is a CUPS unit subroutine.
                All other subroutines are from CUPSgui. }

VAR
   ITest: Integer;
   NoFFT: Boolean;

BEGIN
WITH InputScreen DO
BEGIN

   CASE RowChosen OF

      1: BEGIN   { Plot how/Display style }

            Init;
            DefineInputPort (0.15, 0.85, 0.30, 0.80);
            LoadLine ('                                 ');
            LoadLine (' How should complex wave functions be displayed? ');
            LoadLine ('                                 ');
            LoadLine ('   Color for phase          #1   ');
            LoadLine ('   Real and imaginary parts #1   ');
            LoadLine ('   Probability density      #1   ');
            LoadLine ('                                 ');
            LoadLine ('  [  Ok  ]   [Cancel]            ');
            SetRadioButton ('1', DisplayStyle);

            AcceptScreen;

            IF (NOT Canceled) THEN DisplayStyle := GetRadioButton ('1');
            Done;

         END;

      2: BEGIN   { Plot what/Display content }

            Init;
            DefineInputPort (0.15, 0.85, 0.30, 0.80);
            LoadLine ('                                        ');
            LoadLine (' What should be shown?                  ');
            LoadLine ('                                        ');
            LoadLine ('    The position space wave function #F ');
            LoadLine ('    The momentum space wave function #F ');
            IF (ActivePotl = InfWell) OR (ActivePotl = SHO) THEN
            LoadLine ('    The energy spectrum              #F ')
            ELSE
            LoadLine ('                                        ');
            LoadLine ('                                        ');
            LoadLine (' [  Ok  ]   [Cancel]                    ');

            SetBoolean (1, Odd(DisplayContent));
            SetBoolean (2, Odd(DisplayContent DIV 2));
            IF (ActivePotl = InfWell) OR (ActivePotl = SHO) THEN
               SetBoolean (3, Odd(DisplayContent DIV 4));

            AcceptScreen;

            IF (NOT Canceled) THEN
            BEGIN
               DisplayContent := 0;

               IF (GetBoolean (1)) THEN
                  DisplayContent := DisplayContent + 1;

               IF (GetBoolean (2)) THEN
               BEGIN

                  { First check to see whether the FFT can be taken, which
                    requires NPoints to be an integral power of 2 greater
                    than 4. }
                  NoFFT := FALSE;
                  IF (NPoints <= 4) THEN
                     NoFFT := TRUE
                  ELSE
                  BEGIN
                     ITest := NPoints;
                     WHILE (ITest > 1) DO
                     BEGIN
                        IF (Odd(ITest)) THEN NoFFT := TRUE;
                        ITest := ITest DIV 2;
                     END;
                  END;

                  IF (NoFFT) THEN
                     Announce (Concat ('Cannot find momentum space wave ',
                                       'function...bad value of NPoints.'))
                  ELSE
                     DisplayContent := DisplayContent + 2;

               END;   {of IF GetBolean (2)}

               IF (ActivePotl = InfWell) OR (ActivePotl = SHO) THEN
                  IF (GetBoolean (3)) THEN
                     DisplayContent := DisplayContent + 4;

            END;
            Done;

         END;

      3: ShowMeans := - ShowMeans;   { Show mean values }

      4: ShowUncertainties := - ShowUncertainties;   { Show uncertainties }

   END; {of CASE RowChosen}

END; {of WITH InputScreen}
END; {of PROCEDURE ProcessDisplay}



{---Action on global variables-----------------------------------------------}

PROCEDURE SetStaticVariables;
{ Set the global variables that will not be altered as the program runs. }
{ Dependencies: Pi is a Turbo function.
                LightRed, Yellow, etc. are Turbo Graph constants. }

VAR
   J, JD: Integer;
   pmin, Dp: Real;

BEGIN

   xmin := - 6.0;
      { Left edge of the universe.  Must be negative. }
   Dx := - (2*xmin)/NPoints;
      { Grid spacing...selected so that x = 0 falls on the grid. }
   xmax := xmin + Dx*(NPoints-1);
      { Right edge of the universe. }
   DtSI := 0.025;
      { Time step in femtoseconds...found by experiment. }
   Dt := 0.11576765 * DtSI;
      { The "scaled" time step: time*hbar/m in nm^2,
        where m is the electron's mass. }

   { Although the value of the wave function is calculated at all of
     the NPoints grid points, the display window is typically smaller.
     Here we set the display region to be the middle half of
     the calculated region.  This hides spurious edge effects. }
   FirstDP := (NPoints DIV 4) + 1;
      { The number of the first display point. }
   xleft  := xmin + (FirstDP-1)*Dx;
   xright := xmin + (FirstDP+NumDP-2)*Dx;
      { Frames of the display window. }

   Flat := 1; InfWell := 8; SHO := 12;
      { Names for some special potentials. }
   Infinity := 1066.0;
      { Code value for infinite potential. }
   CeVtoScaled := 13.123415;
      { A "scaled energy" is energy*(m/hbar^2) -- where m is the
        electron's mass -- in units of nm^(-2).  This number is
        m/hbar^2 in units of nm^(-2)/eV, so that
      scaled energy (in nm^(-2)) = energy (in eV) * CeVtoScaled. }

   { List of displayed x values...not calculated x values. }
   FOR JD := 1 TO NumDP DO
      xValue[JD] := xleft + Dx*(JD-1);

   { Grid of values where the momentum wave function is evaluated and
     plotted.  Data are actually momenta/hbar in units of nm^(-1). }
   pmin := - Pi/Dx;
   Dp := 2.0*Pi/(NPoints*Dx);
   FOR J := 1 TO NPoints DO
      pValue[J] := pmin + Dp*(J-1);

   { Elements of a continuous wheel of hue. }
   ColorOf[1] := LightRed;
   ColorOf[2] := Yellow;
   ColorOf[3] := LightGreen;   
   ColorOf[4] := LightCyan;
   ColorOf[5] := LightBlue;
   ColorOf[6] := Blue;
   ColorOf[7] := Magenta;
   ColorOf[8] := LightMagenta;

   { Colors for real part, imaginary part, and square magnitude of
        a complex number. }
   RealColor := ColorOf[1];
   ImagColor := ColorOf[(NumColors DIV 4) + 1];
   ProbColor := Yellow;

END; {of PROCEDURE SetStaticVariables}



PROCEDURE SetDefaultProfile;
{ Set the global variables that specify the profile. }

VAR
   I, IP: Integer;

BEGIN

   ActivePotl := 1;

   FOR I := 1 TO NumPotls DO
      FOR IP := 1 TO 3 DO
         PotlPara[I,IP] := 0.0;

   PotlPara[ 1,1] :=  0.00;   { Flat potential height, in eV }
   PotlPara[ 2,1] :=  9.00;   { Ramp height at right edge, in eV }
   PotlPara[ 3,1] :=  4.00;   { Step height, in eV }
   PotlPara[ 3,2] :=  0.00;   { Step position, in nm }
   PotlPara[ 4,1] :=  4.00;   { Square barrier height, in eV }
   PotlPara[ 4,2] :=  1.00;   { Square barrier width, in nm }
   PotlPara[ 5,1] :=  4.00;   { Notch barrier height, in eV }
   PotlPara[ 5,2] :=  1.00;   { Notch barrier width, in nm }
   PotlPara[ 5,3] :=  0.20;   { Fraction of notch barrier width over which
                                the walls slope. }
   PotlPara[ 6,1] :=  4.00;   { Gaussian barrier height, in eV }
   PotlPara[ 7,1] :=  8.00;   { Two square barrier height, in eV }
   PotlPara[ 7,2] :=  0.50;   { Width of each square barrier, in nm }
   PotlPara[ 8,1] := -0.75;   { Infinite square well left side, in nm }
   PotlPara[ 8,2] := +1.50;   { Infinite square well width, in nm }
   PotlPara[ 9,1] :=  4.00;   { Finite square well depth, in eV }
   PotlPara[ 9,2] :=  2.00;   { Finite square well width, in nm }
   PotlPara[10,1] :=  4.00;   { Notch well depth, in eV }
   PotlPara[10,2] :=  1.00;   { Notch well width, in nm }
   PotlPara[10,3] :=  0.20;   { Fraction of notch well width over which
                                the walls slope. }
   PotlPara[11,1] :=  1.00;   { Double square well barrier height, in eV }
   PotlPara[12,1] :=  2.50;   { Harmonic oscillator K, in eV/nm^2 }
   PotlPara[13,1] :=  2.50;   { Quartic oscillator K, in eV/nm^4 }

   ForceF := 0.0;
   Forcew := 5.0;

   ActiveInitWF := 1;

   FOR I := 1 TO NumPotls DO
   BEGIN
      InitWFPara[I,1] := -2.0;   { Mean position, in nm }
      InitWFPara[I,2] := +0.2;   { Uncertainty in position, in nm }
      InitWFPara[I,3] :=  3.0;   { Mean energy, in eV }
   END;

   InitWFPara[ 3,2] := +0.4;   { Step }
   InitWFPara[ 7,1] :=  0.0;   { Two square barriers }
   InitWFPara[ 8,1] :=  0.0;   { Infinite square well }
   InitWFPara[ 9,1] :=  0.0;   { Finite square well }
   InitWFPara[10,1] :=  0.0;   { Notch well }
   InitWFPara[11,1] := -1.0;   { Double square well }
   InitWFPara[11,3] :=  1.0;
   InitWFPara[12,3] :=  6.0;   { Harmonic oscillator }
   InitWFPara[13,1] := -1.0;   { Quartic oscillator }
   InitWFPara[13,3] :=  6.0;

   FOR IP := 1 TO 2 DO
   BEGIN
      FOR I := 1 TO 6 DO
         InitWFCombo[IP,I] := 0.0;   { Expansion coefficients in terms }
      InitWFCombo[IP,1] := +1.0;     { of the three lowest lying energy }
      InitWFCombo[IP,4] := -0.5;     { eigenfunctions. }
   END;

   DisplayStyle := 1;
   DisplayContent := 1;
   ShowMeans := -1;
   ShowUncertainties := -1;
   AnimationSpeed := 0;

END; {of PROCEDURE SetDefaultProfile}



PROCEDURE GenerateState;
{ Given a profile, set the additional global variables needed to
  specify the state. }

BEGIN

   Itime := 0;
   BuildPotl;
   BuildWF;

END; {of PROCEDURE GenerateState}



{---Interface----------------------------------------------------------------}

PROCEDURE UpdateMenu;
{ Update menu checks and activates. }
{ This subroutine alters the global object variable Menu. }
{ Dependencies: Menu is a CUPSgui object variable. }

VAR
   I: Integer;

BEGIN

   FOR I := 1 TO NumPotls DO
      Menu.RowChecked (2, I, FALSE);
   Menu.RowChecked (2, ActivePotl, TRUE);
   IF (ForceF <> 0.0) THEN
      Menu.RowChecked (2, NumPotls+2, TRUE)
   ELSE
      Menu.RowChecked (2, NumPotls+2, FALSE);

   FOR I := 1 TO 5 DO
      Menu.RowChecked (3, I, FALSE);
   Menu.RowChecked (3, ActiveInitWF, TRUE);

   IF (ShowMeans < 0) THEN
      Menu.RowChecked (4, 3, FALSE)
   ELSE
      Menu.RowChecked (4, 3, TRUE);
   IF (ShowUncertainties < 0) THEN
      Menu.RowChecked (4, 4, FALSE)
   ELSE
      Menu.RowChecked (4, 4, TRUE);

   IF (ActivePotl = InfWell) OR (ActivePotl = SHO) THEN
      Menu.RowActivate (3, 4, TRUE)
   ELSE
      Menu.RowActivate (3, 4, FALSE);

END; {of PROCEDURE UpdateMenu}



PROCEDURE SetUpInterface;
{ Prepare command mode menu and display it.
  Prepare run mode keys but don't display them. }
{ This subroutine alters the global object variables Menu and Keys. }
{ Dependencies: Menu and Keys are CUPSgui object variables. }

BEGIN

   WITH Menu DO
   BEGIN
      Init;
      Column (1, 'File');
         Row (1, 1, 'About CUPS');
         Row (1, 2, 'About Program');   { I prefer "About QMTime". }
         Row (1, 3, 'Things to Notice');
         Row (1, 4, 'Configuration');
         Row (1, 5, 'Open...');
         Row (1, 6, 'Save as...');
         Row (1, 7, 'Exit Program');
      Column (2, 'Potential');
         Row (2, 1, 'Flat');
         Row (2, 2, 'Ramp');
         Row (2, 3, 'Step');
         Row (2, 4, 'Square Barrier');
         Row (2, 5, 'Notch Barrier');
         Row (2, 6, 'Gaussian Barrier');
         Row (2, 7, 'Two Square Barriers');
         Row (2, 8, 'Infinite Square Well');
         Row (2, 9, 'Finite Square Well');
         Row (2,10, 'Notch Well');
         Row (2,11, 'Double Square Well');
         Row (2,12, 'Harmonic Oscillator');
         Row (2,13, 'Quartic Oscillator');
         Row (2, NumPotls,   'User Defined');
         Row (2, NumPotls+1, '--------------------');
         RowActivate (2, NumPotls+1, FALSE);
         Row (2, NumPotls+2, 'Driving');
      Column (3, 'Wave func');
         Row (3, 1, 'Gaussian');
         Row (3, 2, 'Lorentzian');
         Row (3, 3, 'Tent');
         Row (3, 4, 'Combination of Energy States');
         Row (3, 5, 'User Defined');
         Row (3, 6, '----------------------------');
         RowActivate (3, 6, FALSE);
         Row (3, 7, 'Shift');   { or Translate }
         Row (3, 8, 'Stretch');
         Row (3, 9, 'Reflect');
         Row (3,10, 'Conjugate');
      Column (4, 'Display');
         Row (4, 1, 'Plot How');    { Display style }
         Row (4, 2, 'Plot What');   { Display content }
         Row (4, 3, 'Show Mean Values');
         Row (4, 4, 'Show Uncertainties');
      Column (5, 'Reset');
         Row (5, 1, 'Reset this Run');
         Row (5, 2, 'Reset to Defaults');
      Column (6, 'Run');
       { Row (6, 1, 'Run Continuously'); }
       { Row (6, 2, 'Run by Steps');     }
       { To enable stepping from the menu, uncomment these two lines
         and one line in ProcessMenu. }
      UpdateMenu;
      Display;
   END; {of WITH Menu}

   WITH Keys DO
   BEGIN
      Init (6);
      Key[1] := 'F2-Run';   { Can change to 'F2-Stop'. }
      Key[2] := 'F3-Step';
      Key[3] := 'F4-Backward';   { Can change to 'F4-Forward'. }
      Key[4] := 'F5-Slower';
      Key[5] := 'F6-Faster';
      Key[6] := 'F10-Menu';
   END; {of WITH Keys}

END; {of PROCEDURE SetUpInterface}



PROCEDURE ShowKeyBanner (VAR Frames: TFrames);
{ Display the hot key banner strip at the bottom of the screen. }
{ This subroutine alters the global object variable Keys. }
{ Dependencies: SetActivePage is a Turbo Graph subroutine.
                Keys is a CUPSgui object variable.
                TFrames is a CUPSgui object type. }

BEGIN

   IF (Stepping) THEN
      Keys.Key[1] := 'F2-Run'
   ELSE
      Keys.Key[1] := 'F2-Stop';

   IF (IDirection > 0) THEN
      Keys.Key[3] := 'F4-Backward'
   ELSE
      Keys.Key[3] := 'F4-Forward';

   IF (Frames.FirstPage) THEN
   BEGIN
      SetActivePage (1);
      Keys.Display;
      SetActivePage (0);
      Keys.Display;
   END
   ELSE
   BEGIN
      SetActivePage (0);
      Keys.Display;
      SetActivePage (1);
      Keys.Display;
   END;

END; {of PROCEDURE ShowKeyBanner}



{---Help---------------------------------------------------------------------}

PROCEDURE GiveWelcome;
{ Show a welcoming screen. }
{ Dependencies: HelpScrType is a CUPSgui type.
                Help is a CUPSgui subroutine. }

VAR
  Screen: HelpScrType;

BEGIN

   Screen[01] := '                                                  ';
   Screen[02] := '       QUANTUM MECHANICAL TIME DEVELOPMENT        ';
   Screen[03] := '                                                  ';
   Screen[04] := '                        by                        ';
   Screen[05] := '                                                  ';
   Screen[06] := '                   Daniel Styer                   ';
   Screen[07] := '                  Oberlin College                 ';
   Screen[08] := '           (dstyer@physics.oberlin.edu)           ';
   Screen[09] := '                                                  ';
   Screen[10] := '           Version 1.0   29 August 1994           ';
   Screen[11] := '        (c) 1995, John Wiley & Sons, Inc.         ';
   Screen[12] := '                                                  ';
   Screen[13] := '                                                  ';
   Screen[14] := '     This program demonstrates quantal time       ';
   Screen[15] := '  development in one dimension.  The potential    ';
   Screen[16] := '  energy function and initial wave function are   ';
   Screen[17] := '  under your control.  So are the style and       ';
   Screen[18] := '  content of the display.  Simply choose "Run"    ';
   Screen[19] := '  to see how the initial wave function that you   ';
   Screen[20] := '  have chosen changes with time.                  ';
   Screen[21] := '                                                  ';
   Screen[22] := '                                                  ';
   Screen[23] := '                                                  ';
{  Screen[24] := '    Press any key or click mouse to continue.     '; }
   Screen[24] := '                                                  ';
   Screen[25] := '                                                  ';

   Help (Screen);

END; {of PROCEDURE GiveWelcome}



PROCEDURE AboutQMTime;
{ Show the welcoming screen and a page of information about the display. }
{ Dependencies: HelpScrType is a CUPSgui type.
                Help is a CUPSgui subroutine. }

VAR
  Screen: HelpScrType;

BEGIN

   GiveWelcome;

   Screen[01] := ' Understanding the Display in Program QMTime:     ';
   Screen[02] := '                                                  ';
   Screen[03] := ' DISPLAY OF COMPLEX-VALUED WAVE FUNCTIONS         ';
   Screen[04] := '    The physically significant representation of  ';
   Screen[05] := ' a wave function is as an magnitude and a phase,  ';
   Screen[06] := ' not as real and imaginary parts.  The default    ';
   Screen[07] := ' display style for this program represents a      ';
   Screen[08] := ' wave function''s magnitude at a point by the      ';
   Screen[09] := ' height of a line, its phase by the color of that ';
   Screen[10] := ' line.  The correspondence of color to phase is   ';
   Screen[11] := ' given by the wheel in the upper right corner:    ';
   Screen[12] := ' light red for pure positive real, light green    ';
   Screen[13] := ' for pure positive imaginary, etc.  This display  ';
   Screen[14] := ' style can be changed through the menu entry      ';
   Screen[15] := ' "Display|Plot How".                              ';
   Screen[16] := '                                                  ';
   Screen[17] := ' DISPLAY OF POTENTIAL ENERGY FUNCTION             ';
   Screen[18] := '    The potential energy as a function of         ';
   Screen[19] := ' position is shown by a thick green line.         ';
   Screen[20] := '                                                  ';
   Screen[21] := ' DISPLAY OF MEAN ENERGY                           ';
   Screen[22] := '    The thin blue line that is the psi(x) = 0     ';
   Screen[23] := ' axis is drawn at the expectation energy for the  ';
   Screen[24] := ' wave function.                                   ';
   Screen[25] := '                                                  ';

   Help (Screen);

END; {of PROCEDURE AboutQMTime}



PROCEDURE GiveHints;
{ Suggest things to look for. }
{ Dependencies: HelpScrType is a CUPSgui type.
                Help is a CUPSgui subroutine. }

VAR
  Screen: HelpScrType;

BEGIN

   Screen[01] := '                                                  ';
   Screen[02] := '  Things to notice about the free particle:       ';
   Screen[03] := '                                                  ';
   Screen[04] := '  1. The hump in the wave function spreads out    ';
   Screen[05] := '     as time goes on.  If the initial wave packet ';
   Screen[06] := '     was Gaussian, the hump remains symmetric     ';
   Screen[07] := '     about its center as it moves; if it was      ';
   Screen[08] := '     Lorentzian or a tent, it does not.           ';
   Screen[09] := '                                                  ';
   Screen[10] := '  2. The hump moves faster than the colors do.    ';
   Screen[11] := '     (Group velocity is greater than phase        ';
   Screen[12] := '     velocity.)                                   ';
   Screen[13] := '                                                  ';
   Screen[14] := '  3. The colors bunch up to the right of the      ';
   Screen[15] := '     hump.  (Local wavelength decreases toward    ';
   Screen[16] := '     the front...dispersion.)                     ';
   Screen[17] := '                                                  ';
   Screen[18] := '                                                  ';
   Screen[19] := '                                                  ';
   Screen[20] := '                                                  ';
   Screen[21] := '                                                  ';
   Screen[22] := '                                                  ';
   Screen[23] := '                                                  ';
   Screen[24] := '                                                  ';
   Screen[25] := '                                                  ';

   Help (Screen);

END; {of PROCEDURE GiveHints}



{---File handling------------------------------------------------------------}

PROCEDURE GetFileName (VAR FileName: String);
{ Utility: Ask for and read in a file name.  On input, FileName holds
  the default file name; on output it holds the file name given by the
  user or, if the user canceled his request, it holds ''. }
{ This procedure doesn't test to see whether the name read is that
  of an existing file.  Nor does it check for illegal MS-DOS file names,
  such as those with two periods or with an asterisk. }
{ This subroutine neither uses nor alters any global variable. }
{ Dependencies: String is a Turbo data type.
                All subroutines are from CUPSgui. }

BEGIN
WITH InputScreen DO
BEGIN

   Init;
   DefineInputPort (0.15, 0.85, 0.30, 0.60);

   LoadLine ('                             ');
   LoadLine (' File name:  "            "  ');
   LoadLine ('                             ');
   LoadLine ('  [  Ok  ]   [Cancel]        ');
   SetString (1, FileName);

   AcceptScreen;

   IF (Canceled) THEN
      FileName := ''
   ELSE
      FileName := GetString (1);

   Done;

END; {of WITH InputScreen}
END; {of PROCEDURE GetFileName}



PROCEDURE GetComment (VAR Comment: String);
{ Utility: Ask for and read in a comment. }
{ This subroutine neither uses nor alters any global variable. }
{ Dependencies: String is a Turbo data type.
                All strange subroutines are from CUPSgui. }

BEGIN
WITH InputScreen DO
BEGIN

   Init;
   DefineInputPort (0.12, 0.92, 0.30, 0.60);

   LoadLine (' Enter a comment (optional):                                 ');
   LoadLine ('  "                                                        " ');
   LoadLine ('                                                             ');
   LoadLine ('  [  Ok  ]   [Cancel]                                        ');

   AcceptScreen;

   IF (Canceled) THEN
      Comment := ' *'
   ELSE
      Comment := Concat (' * ', GetString (1));

   Done;

END; {of WITH InputScreen}
END; {of PROCEDURE GetComment}



PROCEDURE ReadProfileFile (FirstTime: Boolean);
{ Read an existing profile file. }
{ This subroutine alters all of the profile global variables. }
{ Dependencies: String is a Turbo data type.
                FSearch is a Turbo DOS unit subroutine.
                OpenFile is a CUPSgui subroutine.
                Announce is a CUPS unit subroutine. }

VAR
   FileName, Title: String;
   InputFile: Text;
   I: Integer;

BEGIN

   { If this is at startup, look for an INIT profile file.
     Otherwise read in a file name. }
   IF (FirstTime) THEN
      FileName := FSearch ('QMTInit.pfl', ' ')
   ELSE
      FileName := OpenFile ('*.pfl');

   IF (FileName <> '') THEN
   BEGIN

      Assign (InputFile, FileName);
      Reset (InputFile);

      ReadLn (InputFile, Title);
      IF (Title = ' Profile file produced by program QMTime') THEN
      BEGIN

         ReadLn (InputFile);   { Ignore the comment line. }

         ReadLn (InputFile, ActivePotl);
         FOR I := 1 TO NumPotls DO
            ReadLn (InputFile, PotlPara[I,1], PotlPara[I,2], PotlPara[I,3]);
         ReadLn (InputFile, ForceF, Forcew);

         ReadLn (InputFile, ActiveInitWF);
         FOR I := 1 TO NumPotls DO
            ReadLn (InputFile, InitWFPara[I,1], InitWFPara[I,2],
                    InitWFPara[I,3]);
         FOR I := 1 TO 6 DO
            ReadLn (InputFile, InitWFCombo[1,I], InitWFCombo[2,I]);

         ReadLn (InputFile, DisplayStyle, DisplayContent,
                            ShowMeans, ShowUncertainties, AnimationSpeed);

      END
      ELSE

         Announce (Concat ('File ', FileName,
                           ' is not a QMTime profile file.'));

      Close (InputFile);

   END; {of IF FileName}

END; {of PROCEDURE ReadProfileFile}



PROCEDURE SaveProfileFile;
{ Save the current profile in a file. }
{ This subroutine does not alter any global variable. }
{ Dependencies: String is a Turbo data type. }

VAR
   FileName, Comment: String;
   OutputFile: Text;
   I: Integer;

BEGIN

   FileName := 'qmt00.pfl';
   GetFileName (FileName);
   IF (FileName <> '') THEN
   BEGIN

      Assign (OutputFile, FileName);
      Rewrite (OutputFile);

      WriteLn (OutputFile, ' Profile file produced by program QMTime');

      GetComment (Comment);
      WriteLn (OutputFile, Comment);

      WriteLn (OutputFile, ActivePotl:3);
      FOR I := 1 TO NumPotls DO
         WriteLn (OutputFile, ' ', PotlPara[I,1], ' ', PotlPara[I,2],
                  ' ', PotlPara[I,3]);
      WriteLn (OutputFile, ' ', ForceF, ' ', Forcew);

      WriteLn (OutputFile, ActiveInitWF:3);
      FOR I := 1 TO NumPotls DO
         WriteLn (OutputFile, ' ', InitWFPara[I,1], ' ', InitWFPara[I,2],
                  ' ', InitWFPara[I,3]);
      FOR I := 1 TO 6 DO
         WriteLn (OutputFile, ' ', InitWFCombo[1,I], ' ', InitWFCombo[2,I]);

      WriteLn (OutputFile, DisplayStyle:3, DisplayContent:3,
                     ShowMeans:3, ShowUncertainties:3, AnimationSpeed:3);

      Close (OutputFile);

   END; {of IF FileName}

END; {of PROCEDURE SaveProfileFile}



{---Flow control-------------------------------------------------------------}

PROCEDURE ProcessKeys (KeyNum: Byte; VAR Frames: TFrames);
{ Process pressing of a hot key. }
{ The global variables directly altered by this subroutine are
  AnimationSpeed, Stepping, Running, IDirection, and Keys.  The routine
  can call procedure PrepareToRun, which alters the global variables used
  in time stepping. }
{ Dependencies: Keys is a CUPSgui object variable.
                TFrames is a CUPSgui object type. }

BEGIN
   CASE KeyNum OF

      1: { F2-Run/Stop: Change from continuous going to single stepping
           or vice versa. }
         BEGIN
            Stepping := NOT Stepping;
            ShowKeyBanner (Frames);
         END;

      2: { F3-Step: Move forward a single step. }
         IF (NOT Stepping) THEN
         BEGIN
            Stepping := TRUE;
            ShowKeyBanner (Frames);
         END;

      3: { F4-Backward/Forward: Change direction that time is running. }
         BEGIN
            IDirection := - IDirection;
            PrepareToRun;
            ShowKeyBanner (Frames);
         END;

      4: { F5-Slower }
         AnimationSpeed := AnimationSpeed - 1;

      5: { F6-Faster }
         AnimationSpeed := AnimationSpeed + 1;

      6: { F10-Menu: Change from run mode to command mode. }
         BEGIN
            Running := FALSE;
            Stepping := FALSE;
            { IDirection := +1; }
            { Uncomment the above line if you want a choice from the menu
              to change time to forward going. }
            Frames.Close;
            Keys.Clear;
         END;

   END; {of CASE KeyNum}

END; {of PROCEDURE ProcessKeys}



PROCEDURE ProcessMenu (VAR Frames: TFrames);
{ Process choice from command menu. }
{ This subroutine can alter any of the profile (except for
  AnimationSpeed), state, or flow control global variables.  It can also
  call the procedure UpdateMenu, which alters the global object variable
  Menu, and or the procedure ShowKeyBanner, which alters the global
  object variable Keys. }
{ Dependencies: SetVisualPage is a Turbo Graph subroutine.
                Menu and Keys are CUPSgui object variables.
                TFrames is a CUPSgui object type.
                Configuration is a CUPSgui subroutine. }

VAR
   DoIt: Boolean;
      { This variable is TRUE if the user asks to make changes,
        FALSE if he changes his mind and asks to escape.
        Although perhaps a strange name, it is a favorite word
        of my two-year-old son. }

BEGIN

   CASE Menu.ColChosen OF

      1: {File}
         CASE Menu.RowChosen OF
            1: AboutCUPS;
            2: AboutQMTime;
            3: GiveHints;   { Things to notice }
            4: Configuration;
            5: BEGIN
                  ReadProfileFile (FALSE);
                  GenerateState;
                  UpdateMenu;
                  SetUpPlot;
                  Plot;
               END;
            6: SaveProfileFile;
            7: QuitNow := TRUE;
         END; {of CASE Menu.RowChosen}

      2: BEGIN   { Potential }
            IF (Menu.RowChosen <= NumPotls) THEN
            BEGIN

               IF (Menu.RowChosen = NumPotls) THEN
                  DoIt := TRUE
               ELSE
                  GetPotlPara (Menu.RowChosen, DoIt);
               IF (DoIt) THEN
               BEGIN
                  ActivePotl := Menu.RowChosen;
                  BuildPotl;
                     { Possibly clip the current wave function. }
                  IF (ActivePotl = InfWell) OR (ActivePotl = 9)
                     OR (ActivePotl = NumPotls) THEN NormalizeWF;
                  IF (ActivePotl <> InfWell) AND (ActivePotl <> SHO) THEN
                  BEGIN   { Eliminate energy spectrum display. }
                     IF (DisplayContent > 4) THEN
                        DisplayContent := DisplayContent - 4;
                     IF (DisplayContent = 4) THEN
                        DisplayContent := 1;
                     SetUpPlot;
                  END;
                  Itime := 0;
                  UpdateMenu;
                  Plot;
               END; {of IF DoIt}

            END
            ELSE
            BEGIN   { Driving }

               GetDrivingPara;
               UpdateMenu;
               Plot;

            END;
         END;

      3: { Wave function }
         CASE Menu.RowChosen OF

            1..4: BEGIN   { Initial wave function }
                     GetWFPara (Menu.RowChosen, DoIt);
                     IF (DoIt) THEN
                     BEGIN
                        ActiveInitWF := Menu.RowChosen;
                        Itime := 0;
                        BuildWF;
                        UpdateMenu;
                        Plot;
                     END; {of IF DoIt}
                  END;

            5: BEGIN   {User defined initial wave function }
                  ActiveInitWF := Menu.RowChosen;
                  Itime := 0;
                  BuildWF;
                  UpdateMenu;
                  Plot;
               END;

            7: ShiftWF;   { Alter wave function }
            8: StretchWF;
            9: ReflectWF;
            10: ConjugateWF;

         END; {of CASE Menu.RowChosen}

      4: BEGIN   { Display }
            ProcessDisplay (Menu.RowChosen);
            UpdateMenu;
            SetUpPlot;
            Plot;
         END;

      5: { Reset }
         CASE Menu.Rowchosen OF

            1: BEGIN   { Reset this run. }
                  Itime := 0;
                  BuildWF;
                  Plot;
               END;

            2: BEGIN   { Reset to defaults. }
                  SetDefaultProfile;
                  GenerateState;
                  UpdateMenu;
                  SetUpPlot;
                  Plot;
               END;

         END; {of CASE Menu.RowChosen}

      6: BEGIN   { Run }
            { IF (Menu.Rowchosen = 2) THEN Stepping := TRUE; }
            { To enable stepping from the menu, uncomment this line
              and two lines in SetUpInterface. }
            Running := TRUE;
            PrepareToRun;
            ShowKeyBanner (Frames);
            Frames.Select (1);
            SetVisualPage (1);
         END;

   END; {of CASE Menu.ColChosen}

END; {of PROCEDURE ProcessMenu}



PROCEDURE HaltOnEdgeWallCollision (VAR Frames: TFrames);
{ Switch to command mode if wave function collides with boundary wall. }
{ This subroutine can alter the Running, Stepping, and Keys global
  variables. }
{ Dependencies: Keys is a CUPSgui object variable.
                TFrames is a CUPSgui object type.
                Announce is a CUPS unit subroutine. }

VAR
   eps: Real;

BEGIN
   eps := 1.0e-4;
   IF (Sqr (Psi[2,1]) + Sqr (Psi[2,2]) > eps) OR
      (Sqr (Psi[NPoints-1,1]) + Sqr (Psi[NPoints-1,2]) > eps) THEN
   BEGIN

      { Act as if key F10 had been pressed. }
      ProcessKeys (6, Frames);
      { Give warning. }
      Announce ('The wave packet has collided with one of the edge walls.');

   END;

END; {of PROCEDURE HaltOnEdgeWallCollision}



FUNCTION MouseClickedInMenu: Boolean;
{ Utility: True iff the mouse has been clicked in the menu area. }
{ Dependencies: Event is a global record of unit CUPS. }

BEGIN
   MouseClickedInMenu := Event.MouseClicked AND (Event.Y < 22)
END; {of FUNCTION MouseClickedInMenu}



{---Main program-------------------------------------------------------------}

{ Dependencies: CUPSinit, CUPSdone, HideMouse, ShowMouse, and
                   DynamicPause are CUPS unit subroutines.
                CheckForEvents is a CUPSgui subroutine.
                Delay is a Turbo CRT subroutine. }

VAR
   Frames: TFrames;   { CUPSgui object variable for display. }
   KeyNum: Byte;
   IStep: Integer;

BEGIN
   Frames.Init;
   CUPSinit;
   SetStaticVariables;
   SetDefaultProfile;
   ReadProfileFile (TRUE);
   GenerateState;
   SetUpPlot;
   Plot;
   SetUpInterface;
   GiveWelcome;

   QuitNow := FALSE;
   Running := FALSE;
   Stepping := FALSE;
   IDirection := +1;

   WHILE (NOT QuitNow) DO
   BEGIN

      IF (Running) THEN
      BEGIN   { Run mode }

         IF (Stepping) THEN
         BEGIN
            IF (Frames.FirstPage) THEN   { Some MS-DOS mouse drivers show }
            BEGIN                        { the pointer only on alternate  }
               Plot;                     { frames during animation.  This }
               HideMouse;                { IF clause gets rid of that     }
               Frames.Switch;            { defect during stepping, which  }
               ShowMouse;                { is when it's really noticable. }
            END;
            DynamicPause;
         END;

         IF (AnimationSpeed < 0) THEN
            Delay (-500*AnimationSpeed)
         ELSE
            FOR IStep := 1 TO AnimationSpeed DO
               StepWF;

         { Step the wave function. }
         StepWF;
         Plot;
         HideMouse;
         Frames.Switch;
         ShowMouse;

         CheckForEvents;
         IF Keys.Pressed (KeyNum) THEN ProcessKeys (KeyNum, Frames)
         ELSE IF MouseClickedInMenu THEN
         BEGIN
            ProcessKeys (6, Frames);
            IF Menu.Chosen THEN ProcessMenu (Frames);
         END;
         { I.e.  IF mouse is clicked in menu area, THEN
                    1) act as if key F10 had been pressed,
                    2) read and process request from menu. }
         HaltOnEdgeWallCollision (Frames);

      END
      ELSE
      BEGIN   { Command (menu) mode }

         CheckForEvents;
         IF Menu.Chosen THEN ProcessMenu (Frames);

      END;

   END; {of WHILE NOT QuitNow}

   Menu.Done;
   CUPSdone;
END. {of PROGRAM QMTime}
