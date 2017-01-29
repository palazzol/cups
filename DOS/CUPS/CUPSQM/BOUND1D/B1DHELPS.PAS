UNIT B1Dhelps;

{$D-}
{$N+}

INTERFACE

USES
   Graph, CUPSmupp, CUPS, CUPSgui;

TYPE
   HelpScreens = (progHS,
                  partHS,part1aHS,part1bHS,
                  tryEnergyHS,examineHS,
                  part2HS,part2aHS,part2bHS,
                  part3HS,part3aHS,part3bHS);

PROCEDURE SetUpTryEnergyHS(VAR A:HelpScrType);
PROCEDURE DisplayHelpScreen(thisHS:HelpScreens);

IMPLEMENTATION


{ ------------ PROCEDURES FOR HELP SCREENS -------------- }

PROCEDURE SetUpProgHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := '';
      A[2]  := '          ONE DIMENSIONAL BOUND STATES ';
      A[3]  := '';
      A[4]  := '                 Ian D.Johnston';
      A[5]  := '         University of Sydney, Australia';
      A[6]  := '';
      A[7]  := '                  version 1.00            ';
      A[8]  := '        (c) 1995 John Wiley and Sons, Inc.';
      A[9]  := '';
      A[10] := '';
      A[11] := '';
      A[12] := '  This program explores the properties of the ';
      A[13] := '  bound state wave functions of an electron in';
      A[14] := '  a one-dimensional potential well.';
      A[15] := '  (1) It solves the Schroedinger equation for a';
      A[16] := '      range of different wells.';
      A[17] := '  (2) It studies properties of eigenfunctions';
      A[18] := '      by calculating various overlap integrals.';
      A[19] := '  (3) It shows the time development of general ';
      A[20] := '      states made up of these eigenfunctions.';
      A[21] := '';
      A[22] := '';
      A[23] := '  Press <Enter> or click the mouse to continue.';
   END;

PROCEDURE SetUpPartHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := '  This program is in three different parts,';
      A[2]  := '  which you get to with the PARTS.. menu.';
      A[3]  := '';
      A[4]  := '';
      A[5]  := '  FINDING EIGENVALUES  allows you to choose';
      A[6]  := '        one of a range of potential wells';
      A[7]  := '        and to find the eigenfunctions and ';
      A[8]  := '        eigenvalues of an electron in the well.';
      A[9] := '';
      A[10] := '';
      A[11] := '  WAVEFUNCTION PROPERTIES  allows you to study';
      A[12] := '        properties of eigenfunctions found in the';
      A[13] := '        first part: orthogonality, normalization';
      A[14] := '        and a wide range of overlap integrals.';
      A[15]  := '';
      A[16] := '';
      A[17] := '  TIME DEVELOPMENT  allows you to investigate how';
      A[18] := '        a general state, made up of eigenfunctions';
      A[19] := '        found in part 1, develop with time.';
      A[21] := '';
      A[22] := '  CAUTION!! If you choose a new potential you must';
      A[23] := '  find its spectrum before going to parts 2 or 3.';
   END;

PROCEDURE SetUpPart1aHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' PARTS.. Choose one of';
      A[2]  := '         PART 1: FINDING EIGENVALUES';
      A[3]  := '         PART 2: WAVEFUNCTION PROPERTIES  ';
      A[4]  := '         PART 3: TIME DEVELOPMENT  ';
      A[5]  := '';
      A[6]  := '';
      A[7]  := ' POTENTIAL.. Select a well from a number of ';
      A[8]  := '                standard shapes.';
      A[9]  := '';
      A[10] := '';
      A[11] := ' PARAMETERS  VARY WELL PARAMETERS  Input ';
      A[12] := '                different values for the well.';
      A[13] := '';
      A[14] := '         ADD A PERTURBATION  Include a small';
      A[15] := '                extra term in the potential.';
      A[16]  := '';
      A[17] := '';
      A[18]  := ' METHOD..TRY ENERGY (WITH MOUSE) ';
      A[19]  := '         TRY ENERGY (FROM KEYBOARD)';
      A[20]  := '                Observe solution of the wave';
      A[21]  := '                equation for different EB';
      A[22]  := '';
      A[23]  := '';
      A[24] := '         Press <Enter> for the next screen....';
   END;

PROCEDURE SetUpPart1bHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' METHOD..HUNT FOR ZERO   Use a binary search';
      A[2]  := '                method to find an energy with';
      A[3]  := '                correct asymptotic behaviour.';
      A[4]  := '';
      A[5]  := '         EXAMINE SOLUTION   Read details of the';
      A[6] := '               solution from the screen using';
      A[7] := '               the mouse';
      A[8] := '';
      A[9] := '';
      A[10] := ' SPECTRUM.. FIND EIGENVALUES   Automatically';
      A[11] := '                find all the eigenvalues for';
      A[12] := '                the well.';
      A[13] := '';
      A[14] := '            SEE WAVEFUNCTIONS   ';
      A[15] := '            SEE WFS AND PROBS   Display each';
      A[16] := '                of the eigenfunctions (and';
      A[17] := '                probabilities) on request.';
      A[18] := '';
      A[19] := '            EXAMINE SOLUTION   As above';
      A[20] := '';
      A[21] := '            SOUND  Toggle between on and off.';
      A[22] := '';
      A[23] := '';
      A[24] := '                           Press <Enter> to resume';
   END;

PROCEDURE SetUpTryEnergyHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' ';
      A[2]  := ' Try choosing a value for the binding energy';
      A[3]  := ' (either with the mouse or from the keyboard).';
      A[4]  := ' For each energy you choose, the program will';
      A[5]  := ' solve the wave equation and draw the solution';
      A[6]  := ' on the corresponding level of the graph.';
      A[7]  := ' ';
      A[8]  := ' Observe the behaviour of this solution at ';
      A[9]  := ' large values of x.  In most cases it diverges';
      A[10] := ' either up or down.  Such solutions do not';
      A[11] := ' correspond to an eigenvalue of the energy.';
      A[12] := ' If however the solution seems to approach ';
      A[13] := ' zero asymptotically, the energy you chose is';
      A[14] := ' very close to an eigenvalue, and the solution';
      A[15] := ' is very close to an eigenfunction.';
      A[16] := '';
      A[17] := ' In most cases the best you will be able to ';
      A[18] := ' do is to find two values of energy which span';
      A[19] := ' an eigenvalue.  You can then use the next';
      A[20] := ' menu choice (HUNT FOR ZERO) to find the exact';
      A[21] := ' value of the binding energy between these two';
      A[22] := ' bounds which produces an exact eigenfunction.';
      A[23] := '';
      A[24] := '                       Press <Enter> to resume';
   END;

PROCEDURE SetUpExamineHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := '';
      A[2]  := '  Use this facility to examine values of the wave';
      A[3]  := '  function for various values of x.';
      A[4]  := '  Note that, irrespective of where you click the';
      A[5]  := '  mouse (so long as it is within the graph) the';
      A[6]  := '  program will simply return the x co-ordinate of';
      A[7]  := '  the point at which you clicked, and the value ';
      A[8]  := '  of the wave function (plotted in pink) at that';
      A[9]  := '  x value. ';
      A[10] := '';
      A[11] := '  Obviously, you can only access this facility if';
      A[12] := '  you have just calculated a wave function, either';
      A[13] := '  with the HUNT FOR ZERO or SEE WAVE FUNCTIONS';
      A[14] := '  options.';
      A[15] := '';
      A[16] := '';
      A[17] := '  To return to the main menu when you are finished ';
      A[18] := '  examining the function, select <F10>.';
      A[19] := '';
      A[20] := '';
      A[21] := '';
      A[22] := '';
      A[23] := '';
      A[24] := '                       Press <Enter> to resume';
      A[25] := '';
   END;

PROCEDURE SetUpPart2HS(VAR A:HelpScrType);
   BEGIN
      A[2]  := ' This part of the program will allow you to';
      A[3]  := ' explore the properties of real wave functions';
      A[4]  := ' by calculating various overlap intregrals.';
      A[5]  := ' ';
      A[6]  := ' It works with the potential well you chose in';
      A[7]  := ' part 1, and its set of (bound) eigenstates.';
      A[8]  := ' If you did not go through part 1, it will use';
      A[9]  := ' a square well which has 6 eigenstates.';
      A[10] := ' ';
      A[11] := ' It will allow you to integrate integrands of';
      A[12] := ' the form:';
      A[13] := '          psi1 * operator * psi2';
      A[14] := ' where psi1 and psi2 are eigenfunctions of the';
      A[15] := ' system, or more general states constructed ';
      A[16] := ' from real linear combinations of eigenstates.';
      A[17] := ' ';
      A[18] := ' ';
      A[19] := ' ';
      A[20] := ' ';
      A[21] := ' ';
      A[22] := ' ';
      A[23] := '         Press <Enter> for the next screen....';
   END;

PROCEDURE SetUpPart2aHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' PARTS.. Choose one of';
      A[2]  := '         PART 1: FINDING EIGENVALUES';
      A[3]  := '         PART 2: WAVEFUNCTION PROPERTIES  ';
      A[4]  := '         PART 3: TIME DEVELOPMENT  ';
      A[5]  := '';
      A[6]  := '';
      A[7]  := ' PSI1..  EIGENSTATE, n = .. Choose one of the ';
      A[8]  := '               eigenfunctions as the first ';
      A[9]  := '                part of the overlap integrand.';
      A[10] := ' ';
      A[11] := '         GENERAL STATE   Choose a real linear';
      A[12] := '                combination of eigenfunctions as';
      A[13] := '                the first part of the integrand.';
      A[14]  := '';
      A[15]  := '';
      A[16] := ' OPERATOR..     Choose an operator to operate on';
      A[17] := '                a second function to form the ';
      A[18] := '                second part of the integrand.';
      A[19] := '                The operators available are:';
      A[20] := '                   1, X, D/DX, X^2, D^2/DX^2,';
      A[21] := '                   V, E, X.D/DX, D/DX.X';
      A[22] := '';
      A[23] := '';
      A[24] := '         Press <Enter> for the next screen....';
   END;

PROCEDURE SetUpPart2bHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' PSI2..  EIGENSTATE   Choose one of the eigen-';
      A[2] := '                functions to be the operand for';
      A[3] := '                the second part of the integrand.';
      A[4] := ' ';
      A[5] := '         GENERAL STATE   Choose a real linear';
      A[6] := '                combination of eigenfunctions.';
      A[7] := '';
      A[8] := '         OTHER FUNCTION  A facility is available ';
      A[9] := '                to specify your own function.  ';
      A[9] := '                Consult manual.';
      A[10] := '';
      A[11] := '';
      A[12] := ' INTEGRATE      Calculates and displays the product';
      A[13] := '                    psi1 * operator * psi2';
      A[14] := '                Then integrates over all x.';
      A[15] := ' ';
      A[16] := ' ';
      A[17] := ' ';
      A[18] := ' ';
      A[19] := ' ';
      A[20] := ' ';
      A[21] := '                        Press <Enter> to resume.';
   END;


PROCEDURE SetUpPart3HS(VAR A:HelpScrType);
   BEGIN
      A[2]  := ' This part of the program will allow you to';
      A[3]  := ' explore the time development of general state';
      A[4]  := ' wave functions made up of linear combinations';
      A[5]  := ' of eigenfunctions, by using the sinusoidal ';
      A[6]  := ' time variation of the individual eigenstates.';
      A[7]  := ' ';
      A[8]  := ' It works with the potential well you chose in';
      A[9]  := ' part 1, and its set of (bound) eigenstates.';
      A[10] := ' If you did not go through part 1, it will use';
      A[11] := ' a square well which has 6 eigenstates.';
      A[12] := ' ';
      A[13] := ' The time-varying wave function is represented';
      A[14] := ' either by plotting amplitude as a function of ';
      A[15] := ' x, and representing the phase by color; or by';
      A[16] := ' plotting real and imaginary parts separately.';
      A[17] := ' ';
      A[18] := ' ';
      A[19] := ' ';
      A[20] := ' ';
      A[21] := ' ';
      A[22] := ' ';
      A[23] := '         Press <Enter> for the next screen....';
   END;

PROCEDURE SetUpPart3aHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' PARTS.. Choose one of';
      A[2]  := '         PART 1: FINDING EIGENVALUES';
      A[3]  := '         PART 2: WAVEFUNCTION PROPERTIES  ';
      A[4]  := '         PART 3: TIME DEVELOPMENT  ';
      A[5]  := '';
      A[6]  := '';
      A[7]  := ' WAVE FUNC.. CHOOSE A WAVE FUNCTION';
      A[8]  := '         Choose a linear combination of ';
      A[9]  := '         eigenfunctions of the well from ';
      A[10]  := '         part 1 to form wave function at t=0';
      A[11] := '         Coefficients may be real or complex.';
      A[12] := '';
      A[13] := '         RUN  Animate using current values of';
      A[14] := '              time (t) and time step (dt).';
      A[15] := '              Default values t = 0, dt = .0001';
      A[16] := '              Note: the units are fs.';
      A[17] := '              During animation choices are:';
      A[18] := '              STOP, REVERSE, SLOWER, FASTER';
      A[19] := '              RESTART, ESCAPE';
      A[20] := '';
      A[21] := '         BEGIN OVER  Set time to 0 and step to 1.';
      A[22] := ' ';
      A[23] := ' ';
      A[24] := '            Press <Enter> for the next screen....';
   END;

PROCEDURE SetUpPart3bHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' MEASURE..  POSITION   Calculate and display <x>';
      A[2]  := '                at the current value of time.';
      A[3]  := ' ';
      A[4]  := '         MOMENTUM   Calculate and display <d/dx>';
      A[5]  := '                at the current value of time.';
      A[6]  := ' ';
      A[7]  := '         USER DEFINED  A facility is available for';
      A[8]  := '                your own code here. Consult manual.';
      A[9]  := '  ';
      A[10] := '         SET TIME   Enter a new value of the time.';
      A[11] := '                and recalculate wave function.';
      A[12] := '  ';
      A[13] := '  ';
      A[14] := ' PLOT HOW..  Choose between';
      A[15] := '          AMPLITUDE AND PHASE';
      A[16] := '          REAL AND IMAGINARY';
      A[17] := '  ';
      A[18] := '  ';
      A[19] := '  ';
      A[20] := '  ';
      A[21] := '                  Press <Enter> to resume';
   END;

PROCEDURE DisplayHelpScreen(thisHS:HelpScreens);
   VAR
      HS : HelpScrType;
      OK : Boolean;
      i  : Integer;
   BEGIN
      FOR i:=1 TO 25 DO HS[i] := '';
      OK := true;
      CASE thisHS OF
         progHS      : SetUpProgHS(HS);
         partHS      : SetUpPartHS(HS);
         part1aHS    : SetUpPart1aHS(HS);
         part1bHS    : SetUpPart1bHS(HS);
         tryEnergyHS : SetUpTryEnergyHS(HS);
         examineHS   : SetUpExamineHS(HS);
         part2HS     : SetUpPart2HS(HS);
         part2aHS    : SetUpPart2aHS(HS);
         part2bHS    : SetUpPart2bHS(HS);
         part3HS     : SetUpPart3HS(HS);
         part3aHS    : SetUpPart3aHS(HS);
         part3bHS    : SetUpPart3bHS(HS);
         ELSE      OK := false;
      END;   {case}
      IF OK THEN
         Help(HS);
   END;

BEGIN
END.
