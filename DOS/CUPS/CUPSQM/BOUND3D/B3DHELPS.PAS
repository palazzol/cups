UNIT B3Dhelps;

{$D-}
{$N+}

INTERFACE

USES
   Graph, CUPSmupp, CUPS, CUPSgui;

TYPE
   HelpScreens = (progHS,
                  partHS,part1aHS,part1bHS,part1cHS,
                  angMomHS,tryEnergyHS,examineHS,cloudHS,
                  part2HS,part2aHS,part2bHS);

PROCEDURE SetUpTryEnergyHS(VAR A:HelpScrType);
PROCEDURE DisplayHelpScreen(thisHS:HelpScreens);

IMPLEMENTATION


{ ------------ PROCEDURES FOR HELP SCREENS -------------- }

PROCEDURE SetUpProgHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := '';
      A[2]  := '         THREE DIMENSIONAL BOUND STATES ';
      A[3]  := '';
      A[4]  := '                 Ian D.Johnston';
      A[5]  := '         University of Sydney, Australia';
      A[6]  := '';
      A[7]  := '                  version 1.00            ';
      A[8]  := '        (c) 1995 John Wiley and Sons, Inc.';
      A[9]  := '';
      A[10] := '';
      A[11] := '';
      A[12] := '  This program solves the Schroedinger equation ';
      A[13] := '  for bound states of several different particles';
      A[14] := '  in a range of spherically symmetric, three-.';
      A[15] := '  dimensional potential wells.';
      A[16] := '  It also studies the properties of these eigen';
      A[17] := '  functions, by calculations of a wide range of';
      A[18] := '  overlap integrals.';
      A[19] := '';
      A[20] := '';
      A[21] := '';
      A[22] := '';
      A[23] := '  Press <Enter> or click the mouse to continue.';
   END;

PROCEDURE SetUpPartHS(VAR A:HelpScrType);
   BEGIN
      A[2]  := '  This program is in two different parts,';
      A[3]  := '  which you get to with the PARTS.. menu.';
      A[4]  := '';
      A[5]  := '';
      A[6]  := '  FINDING EIGENVALUES  allows you to choose';
      A[7]  := '        one of a range of spherically symm-';
      A[8]  := '        etric potentials and to find the ';
      A[9]  := '        radial eigenvalues of a particle in';
      A[10] := '        the well. The angular part of the';
      A[11] := '        wave functions are assumed to be';
      A[12] := '        spherical harmonics.';
      A[13] := '';
      A[14] := '';
      A[15] := '  WAVEFUNCTION PROPERTIES  allows you to study';
      A[16] := '        properties of eigenfunctions found in the';
      A[17] := '        first part: orthogonality, normalization';
      A[18] := '        and a wide range of overlap integrals.';
      A[19] := '';
      A[20] := '';
      A[21] := '';
      A[23] := '                 Press <Enter> to start program.';
   END;

PROCEDURE SetUpPart1aHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' PARTS.. Choose one of';
      A[2]  := '         PART 1: FINDING EIGENVALUES';
      A[3]  := '         PART 2: WAVEFUNCTION PROPERTIES  ';
      A[4]  := '';
      A[5]  := '';
      A[6]  := ' ANG MOM.. SET ORBITAL ANG MOM, l';
      A[7]  := '           SET MAGNETIC ANG MOM, m';
      A[8]  := '                Choose appropriate values';
      A[9]  := '';
      A[10] := '           DISPLAY SPHERICAL HARMONIC.  Draws';
      A[11] := '                Y(l,m) with chsen values.';
      A[12] := '';
      A[13] := '';
      A[14] := ' POTENTIAL.. CHOOSE POTENTIAL SHAPE Select from';
      A[15] := '                a number of standard shapes.';
      A[16] := '';
      A[17] := '         VARY WELL PARAMETERS  Input different';
      A[18] := '                values for the chosen well.';
      A[19] := '';
      A[20] := '         ADD A PERTURBATION  Include a small';
      A[21] := '                extra term in the potential.';
      A[22] := '';
      A[23] := '';
      A[24] := '         Press <Enter> for the next screen....';
   END;

PROCEDURE SetUpPart1bHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' METHOD..TRY ENERGY (WITH MOUSE) ';
      A[2]  := '         TRY ENERGY (FROM KEYBOARD)';
      A[3]  := '                Solve for different EB';
      A[4]  := '';
      A[5]  := '         HUNT FOR ZERO   Use a binary search';
      A[6]  := '                method to find an eigenvalue.';
      A[7]  := '';
      A[8]  := '         EXAMINE SOLUTION   Read details of the';
      A[9]  := '               solution from the screen.';
      A[10] := '';
      A[11] := '';
      A[12] := ' SPECTRUM.. FIND EIGENVALUES   Automatically';
      A[13] := '                find all the eigenvalues.';
      A[14] := '';
      A[14] := '            SEE WAVEFUNCTIONS   ';
      A[15] := '            SEE WFS AND PROBS   Display eigen-';
      A[16] := '                functions (and probabilities).';
      A[18] := '';
      A[19] := '            PROBABILITY CLOUD  Show probability';
      A[20] := '                density in the x-z plane.';
      A[21] := '';
      A[22] := '';
      A[23] := '';
      A[24] := '         Press <Enter> for the next screen....';
   END;

PROCEDURE SetUpPart1cHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' UNITS..  UNITS  Allows you to choose one ';
      A[2]  := '                of: atomic, molecular or';
      A[3]  := '                nuclear lengths and masses.';
      A[4]  := '';
      A[5]  := '         MASS  Allows you to change the mass.';
      A[6]  := '';
      A[7]  := '         r AXIS  Allows you to change the ';
      A[8]  := '               maximum value of r on the axis.';
      A[9]  := '';
      A[10] := '';
      A[11] := '';
      A[12] := '';
      A[13] := '';
      A[14] := '';
      A[14] := '';
      A[15] := '';
      A[16] := '';
      A[18] := '';
      A[19] := '';
      A[20] := '';
      A[21] := '';
      A[22] := '';
      A[23] := '';
      A[24] := '                      Press <Enter> to resume';
   END;

PROCEDURE SetUpAngMomHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' ';
      A[2]  := ' There are two diagrammatic representations';
      A[3]  := ' of the spherical harmonics available.';
      A[4]  := ' ';
      A[5]  := ' (1)  2D PLOT';
      A[6]  := ' ';
      A[7]  := ' Shows a polar plot of the magnitude of Y(l,m)';
      A[8]  := ' on the x=0 plane.';
      A[9]  := ' ';
      A[10] := ' (2)  3D PLOT';
      A[11] := ' ';
      A[12] := ' Shows a three dimensional representation of';
      A[13] := ' the same thing.  Note that you may alter';
      A[14] := '   the PERSPECTIVE, by adjusting the left';
      A[15] := '       hand slider to change the angle in ';
      A[16] := '       the y-z plane; and';
      A[17] := '   the SCALE by adjusting the right hand';
      A[18] := '       slider, labelled "size".';
      A[19] := ' ';
      A[20] := ' ';
      A[21] := ' ';
      A[22] := ' ';
      A[23] := '';
      A[24] := '  Press <Enter> or click the mouse to continue.';
   END;

PROCEDURE SetUpTryEnergyHS(VAR A:HelpScrType);
   BEGIN
      A[2]  := ' Try choosing a value for the binding energy';
      A[3]  := ' (either with the mouse or from the keyboard).';
      A[4]  := ' For each energy you choose, the program will';
      A[5]  := ' solve the wave equation and draw the solution';
      A[6]  := ' on the corresponding level of the graph.';
      A[7]  := ' ';
      A[8]  := ' Observe the behaviour of this solution at ';
      A[9]  := ' large values of r.  In most cases it diverges';
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
      A[24] := '  Press <Enter> or click the mouse to continue.';
   END;

PROCEDURE SetUpExamineHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := '';
      A[2]  := '  Use this facility to examine values of the wave';
      A[3]  := '  function for various values of r.';
      A[4]  := '  Note that, irrespective of where you click the';
      A[5]  := '  mouse (so long as it is within the graph) the';
      A[6]  := '  program will simply return the r co-ordinate of';
      A[7]  := '  the point at which you clicked, and the value ';
      A[8]  := '  of the wave function (plotted in pink) at that';
      A[9]  := '  r value. ';
      A[10] := '';
      A[11] := '  Obviously, you can only access this facility if';
      A[12] := '  you have just calculated a wave function, either';
      A[13] := '  with the HUNT FOR ZERO or SEE WAVE FUNCTIONS';
      A[14] := '  options.';
      A[15] := '';
      A[16] := '';
      A[17] := '  Select <F10-Menu> when you are finished examining';
      A[18] := '  the function and want to return to the main menu.';
      A[19] := '';
      A[20] := '';
      A[21] := '';
      A[22] := '';
      A[23] := '';
      A[24] := '  Press <Enter> or click the mouse to continue.';
      A[25] := '';
   END;

PROCEDURE SetUpCloudHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := '';
      A[2]  := '  This part of the program plots the probability';
      A[3]  := '  density of finding the particle at a distance';
      A[4]  := '  r from the origin as a scatter diagram.';
      A[5]  := '  ';
      A[6]  := '  RUN/STOP  allows you to start or stop plotting.';
      A[7]  := '  ';
      A[8]  := '  FASTER/SLOWER  controls the rate at which the ';
      A[9]  := '            plotting occurs. ';
      A[10] := '';
      A[11] := '  RESTART   clears the plot and restarts.';
      A[12] := '  ';
      A[13] := '  NEW N     Selects a different energy state';
      A[14] := '  ';
      A[15] := '  MENU      finish plotting and return to the';
      A[16] := '            main menu.';
      A[17] := '';
      A[18] := '';
      A[19] := '';
      A[20] := '';
      A[21] := '';
      A[22] := '';
      A[23] := '';
      A[24] := '  Press <Enter> or click the mouse to continue.';
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
      A[13] := '        r*psi1 * operator * r*psi2';
      A[14] := ' where psi1 and psi2 are eigenfunctions of the';
      A[15] := ' system, or more general states constructed ';
      A[16] := ' from real linear combinations of eigenstates.';
      A[17] := ' ';
      A[18] := ' ';
      A[19] := ' ';
      A[20] := ' ';
      A[21] := ' ';
      A[22] := ' ';
      A[23] := '  ';
      A[24] := '  Press <Enter> or click the mouse to continue.';
   END;

PROCEDURE SetUpPart2aHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' PARTS.. FINDING EIGENVALUES  Choose potential';
      A[2]  := '                well and find the energy levels.';
      A[3]  := '';
      A[4]  := '         WAVEFUNCTION PROPERTIES  Investigate';
      A[5]  := '                eigenfunctions found in part 1.';
      A[6]  := '';
      A[7]  := '';
      A[8]  := '         QUIT   Exit from the whole program.';
      A[9]  := '';
      A[10] := '';
      A[11] := '';
      A[12] := '';
      A[13]  := ' PSI1..  EIGENSTATE   Choose one of the eigen-';
      A[14]  := '                functions as the first part of ';
      A[15] := '                the overlap integrand.';
      A[16] := ' ';
      A[17] := '         GENERAL STATE   Choose a real linear';
      A[18] := '                combination of eigenfunctions as';
      A[19] := '                the first part of the integrand.';
      A[20] := '';
      A[21] := '';
      A[22] := ' ';
      A[23] := ' ';
      A[24] := '  <Enter> or click mouse for next screen .....';
   END;

PROCEDURE SetUpPart2bHS(VAR A:HelpScrType);
   BEGIN
      A[1] := ' OPERATOR..     Choose an operator to operate on';
      A[2] := '                a second function to form the ';
      A[3] := '                second part of the integrand.';
      A[4] := '                The operators available are:';
      A[5] := '                   1, R, D/DX, R^2, D^2/DX^2,';
      A[6] := '                   V, E, R.D/DX, D/DX.R';
      A[7] := '';
      A[8]  := '';
      A[9]  := ' PSI2..  EIGENSTATE   Choose one of the eigen-';
      A[10] := '                functions to be the operand for';
      A[11] := '                the second part of the integrand.';
      A[12] := ' ';
      A[13] := '         GENERAL STATE   Choose a real linear';
      A[14] := '                combination of eigenfunctions.';
      A[15] := '';
      A[16] := '         USER   A facility is available to specify';
      A[17] := '                your own function.  Consult manual.';
      A[18] := '';
      A[19] := '';
      A[20] := ' INTEGRATE      Calculates and displays the product';
      A[21] := '                    r*psi1 * operator * r*psi2';
      A[22] := '                Then integrates over all r.';
      A[23] := ' ';
      A[24] := ' ';
      A[25] := '  Press <Enter> or click the mouse to continue.';
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
         part1cHS    : SetUpPart1cHS(HS);
         angMomHS    : SetUpAngMomHS(HS);
         tryEnergyHS : SetUpTryEnergyHS(HS);
         examineHS   : SetUpExamineHS(HS);

         cloudHS     : SetUpCloudHS(HS);
         part2HS     : SetUpPart2HS(HS);
         part2aHS    : SetUpPart2aHS(HS);
         part2bHS    : SetUpPart2bHS(HS);
         ELSE      OK := false;
      END;   {case}
      IF OK THEN
         Help(HS);
   END;


BEGIN
END.
