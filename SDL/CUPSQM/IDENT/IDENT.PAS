PROGRAM Ident;

{ Identical particles in quantum mechanics:  Time development of two
  noninteracting identical particles in a one-dimensional infinite
  square well. }
{ By Dan Styer (dstyer@physics.oberlin.edu).  Version 1.0.  29 August 1994. }
{ This version uses the CUPS utilities of 7 July 1994, including workarounds
  for bugs in that release. }

{ Contents of program Ident:

  Introduction (including notes on modifying the program).
  Constants.

  Global variables that will not be altered.
  Global variables that specify the profile.
  Global variables that specify the state.
  Global variables for plotting and printing.
  Global variables for time stepping.
  Global variables for flow control.
  Global variables for the user interface.

  Subroutines for the user to modify:
     UserDefinedWF, UserDefinedWFSample.
  Time development:
     StepWF.
  Find probability densities:
     FindJointPD, FindReducedPD.
  Output (Plots):
     InitializePlots, PlotProbDensity, PlotCompare2, PlotCompare3, Plot.
  Process the wave function:
     NormalizeWF, BuildWFGaussian, BuildWFEigen, BuildWFCombo, BuildWF.
  Input:
     GetWFParaGaussian, GetWFParaEigen, GetWFParaCombo.
  Action on global variables:
     SetStaticVariables, SetSteppingVariables, SetDefaultProfile.
  Interface:
     UpdateMenu, SetUpInterface, ShowKeyBanner.
  Help:
     GiveWelcome, AboutIdent, GiveHints.
  File handling:
     GetFileName, GetComment, ReadProfileFile, SaveProfileFile,
     StartPrintingS.
  Flow control:
     ProcessKeys, ProcessMenu, MouseClickedInMenu.
  Main program. }

{---Introduction (including notes on modifying the program)------------------}

{ Welcome to program Ident!  I am glad that you have decided to look
  at the program, because you will learn more about quantum mechanics
  by mucking about in the code than you will by just watching the
  simulation.  This program bears a copyright at the insistence of
  the distributer, John Wiley & Sons, Inc.  The copyright is not
  intended to stop you from looking at or modifying the code, which
  I encourage.  If you do modify the code then you MUST:  1) change the
  name of the program, 2) change the introductory comments in the first
  six lines of the program, and 3) change the screen in procedure
  GiveWelcome.  You may not sell a modification of this program, but
  I encourage you to distribute it by sending a copy to the anonymous
  ftp site top.physics.oberlin.edu.  If your modification is sufficiently
  interesting, I will post it there under directory /pub/Ident.   (I am
  also interested in distributing interesting profile files in this manner.)
  If you distribute a modified program, either through this ftp site or
  in any other way, then that program must have similar restrictions on
  its modification and distribution.  For examples, see the programs
  QMBeach and QMValue available through top.physics.oberlin.edu.

              Happy trails,
              Dan Styer. }

{$N+ Generate code for numeric coprocessor. }

USES DOS, CRT, Graph, CUPS, CUPSmupp, CUPSgui, CUPSproc, CUPSgrph;

CONST
   NPoints = 21;   { Number of grid points in one direction.
      Must be less than or equal to the CUPSmupp constant MaxNumData. }
   NContours = 9;   { The maximum number of contour lines, from 1 to 14.
      A higher number gives better graphs and slower speed. }

VAR
{---Global variables that will not be altered--------------------------------}
   { These variables are set in procedure SetStaticVariables. }
   xmin, xmax, Dx: Real;
      { Edges of the box, grid spacing.  All distances are in nanometers. }
   DtSI, Dt: Real;
      { Time step in femtoseconds and in "scaled" units:
           scaled time = time*hbar/m in nm^2
        where m is the electron's mass. }

   xValue: DataVector;   { A CUPSmupp data type }
   sValue: DataVector;   { A CUPSmupp data type }
      { List of grid of x and s values.  Variable s represents the
        separation between two points, s = Abs(x1-x2), so it ranges
        from 0 to xmax-xmin. }

{---Global variables that specify the profile--------------------------------}
   { In this program the "profile" and the "state" are different
     things.  The profile can be changed only by choices from the
     menu or hot keys.  The state can be changed by choices from the
     menu/keys or through time stepping. }
   { The profile variables listed here are all initially set in
     procedure SetDefaultProfile. }
   ActiveInitWF: Integer;
      { The number, from 1 to 4, of the most recent initial wave function. }
   InitWFPara: ARRAY[1..3, 1..6] OF Real;
      { Allow up to six parameters for each of three different kinds of
        built in (i.e., not user defined) wave functions. }

   SymNum: Integer;
      { The symmetrization to show: 1 for nonsymmetrized, 2 for symmetrized,
        3 for antisymmetrized, 4 for compare symm. and antisymm.,
        5 for compare all three. }
   PlotWhat: Integer;
      { Which reduced single-variable probability density to plot:
        1 for separation probability density,
        2 for single probability density. }
   ShowS: Integer;
      { If ShowS is positive and the separation probability density
        is displayed, the mean value of s is shown.  Otherwise it isn't. }
   AnimationSpeed: Integer;
      { The one profile variable that is changed through
        the hot keys rather than the menu.  If negative, amount of
        pause between frames in half-seconds.  If positive, number of
        steps taken without redrawing the screen. }

{---Global variables that specify the state----------------------------------}
   { Psi and Zsa are initially set in procedure BuildWF.  Psi changes
     with time. }
   Itime: Integer;
      { Current time in units of Dt. }
   Psi: ARRAY [1..NPoints, 1..NPoints, 1..2] OF Real;
      { Array Psi always stores a normalized, nonsymmetrized wave function.
        The computation of probability densities corresponding to
        symmetrized or antisymmetrized wave functions is done in the
        procedure FindJointPD. }
      { Psi[J1,J2,1] is the real part of Psi at grid point J1, J2,
        Psi[J1,J2,2] is the imaginary part of Psi there. }
   Zsa: Real;
      { A variable used to normalize the symmetrized and
        antisymmetrized wave functions. }

{---Global variables for plotting and printing-------------------------------}
   WJointPD: DMatrix;   { A CUPSproc object type }
      { Note well!  WJointPD at (NPoints+1-J2, J1) corresponds to
        JointPD[J1,J2].  Hence the name "Weird Joint Probability Density".
        You may not like it and I may not like it, but that's the form
        demanded by the plotting routine. }
   ContourPlot: TContour;   { A CUPSgrph object type }
      { Data necessary for contour plots. }
   PrintS: Boolean;
      { TRUE if mean values of separation are to be printed. }
   PrintSFile: Text;
      { File into which those mean values are put. }

{---Global variables for time stepping---------------------------------------}
   { These variables are set in procedure SetSteppingVariables
     and used by procedure StepWF. }
   DIm: Real;
   U: ARRAY[1..NPoints, 1..2] OF Real;
      { Matrix elements used in time stepping: diagonal elements are
        D = -2 + iDIm, upper diagonal elements are U[J] = U[J,1] + iU[J,2]. }

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

PROCEDURE UserDefinedWF (x1, x2: Real; VAR RePsi, ImPsi: Real);
{ You may use this procedure to insert your own initial wave function
  without fear of damaging any other part of the program.  Simply calculate
  the real part of Psi (RePsi) and the imaginary part of Psi (ImPsi) at the
  point x1, x2.  The wave function doesn't need to be normalized or
  symmetrized. }

BEGIN
   RePsi := Exp ( - x1*x1 - x2*x2 );
   ImPsi := 0.0;
END; {of PROCEDURE UserDefinedWF}



PROCEDURE UserDefinedWFSample (x1, x2: Real; VAR RePsi, ImPsi: Real);
{ This sample demonstrates how you could set up a user defined wave function.
  The function chosen has the interesting property that
          <s>_S  >  <s>_N  >  <s>_A.   }

VAR
   PDS, PDA: Real;

BEGIN
   PDS := Exp ( -( Sqr(x1+0.5) + Sqr(x2-0.5) )/0.25 )
        + Exp ( -( Sqr(x1-0.5) + Sqr(x2+0.5) )/0.25 );
   PDA := Exp ( -( Sqr(x1+0.1) + Sqr(x2-0.1) )/0.25 )
        + Exp ( -( Sqr(x1-0.1) + Sqr(x2+0.1) )/0.25 );
   IF (x1 >= x2) THEN
      RePsi := Sqrt(PDS) + Sqrt(PDA)
   ELSE
      RePsi := Sqrt(PDS) - Sqrt(PDA);
   ImPsi := 0.0;
END; {of PROCEDURE UserDefinedWFSample}



{---Time development---------------------------------------------------------}

PROCEDURE StepWF;
{ Replace the wave function Psi with an approximation for its value
  a time Dt later, using the Crank-Nicholson algorithm and operator
  splitting. }
{ This subroutine alters the state global variables Itime and Psi. }

VAR
   J, M: Integer;
   Re, Im: Real;
   STO: ARRAY[1..NPoints, 1..2] OF Real;

BEGIN
   Itime := Itime + IDirection;

   { First half of operator splitting. }
   FOR J := 2 TO NPoints-1 DO
   BEGIN
      STO[J,1] := 0.0; STO[J,2] := 0.0;
   END;

   FOR M := 2 TO NPoints-1 DO
   BEGIN

      FOR J := 2 TO NPoints-1 DO
      BEGIN

         { temp := STO[J] + ccD*Psi[J,M] + Psi[J,M+1] + Psi[J-1,M];
           STO[J] := Psi[J,M];
           Psi[J,M] := - temp*U[J];
           where ccD is the complex conjugate of D. }
         Re := STO[J,1] - 2*Psi[J,M,1] + DIm*Psi[J,M,2] + Psi[J,M+1,1]
               + Psi[J-1,M,1];
         Im := STO[J,2] - 2*Psi[J,M,2] - DIm*Psi[J,M,1] + Psi[J,M+1,2]
               + Psi[J-1,M,2];
         STO[J,1] := Psi[J,M,1];
         STO[J,2] := Psi[J,M,2];
         Psi[J,M,1] := - Re*U[J,1] + Im*U[J,2];
         Psi[J,M,2] := - Re*U[J,2] - Im*U[J,1];

      END; {of FOR J}

      FOR J := NPoints-1 DOWNTO 2 DO
      BEGIN

         { Psi[J,M] := Psi[J,M] - U[J]*Psi[J+1,M]; }
         Psi[J,M,1] := Psi[J,M,1] - U[J,1]*Psi[J+1,M,1] + U[J,2]*Psi[J+1,M,2];
         Psi[J,M,2] := Psi[J,M,2] - U[J,1]*Psi[J+1,M,2] - U[J,2]*Psi[J+1,M,1];

      END; {of FOR J}

   END; {of FOR M}

   { Second half of operator splitting. }
   FOR M := 2 TO NPoints-1 DO
   BEGIN
      STO[M,1] := 0.0; STO[M,2] := 0.0;
   END;

   FOR J := 2 TO NPoints-1 DO
   BEGIN

      FOR M := 2 TO NPoints-1 DO
      BEGIN

         { temp := STO[M] + ccD*Psi[J,M] + Psi[J+1,M] + Psi[J,M-1];
           STO[M] := Psi[J,M];
           Psi[J,M] := - temp*U[M]; }
         Re := STO[M,1] - 2*Psi[J,M,1] + DIm*Psi[J,M,2] + Psi[J+1,M,1]
               + Psi[J,M-1,1];
         Im := STO[M,2] - 2*Psi[J,M,2] - DIm*Psi[J,M,1] + Psi[J+1,M,2]
               + Psi[J,M-1,2];
         STO[M,1] := Psi[J,M,1];
         STO[M,2] := Psi[J,M,2];
         Psi[J,M,1] := - Re*U[M,1] + Im*U[M,2];
         Psi[J,M,2] := - Re*U[M,2] - Im*U[M,1];

      END; {of FOR M}

      FOR M := NPoints-1 DOWNTO 2 DO
      BEGIN

         { Psi[J,M] := Psi[J,M] - U[M]* Psi[J,M+1]; }
         Psi[J,M,1] := Psi[J,M,1] - U[M,1]*Psi[J,M+1,1] + U[M,2]*Psi[J,M+1,2];
         Psi[J,M,2] := Psi[J,M,2] - U[M,1]*Psi[J,M+1,2] - U[M,2]*Psi[J,M+1,1];

      END; {of FOR M}

   END; {of FOR J}

END; {of PROCEDURE StepWF}



{---Find probability densities-----------------------------------------------}

PROCEDURE FindJointPD (ISym: Integer);
{ Given a normalized, nonsymmetrized wave function stored in the global
  variable Psi, find the joint probability density and put it in the global
  variable WJointPD.  The probability density produced will correspond
  to the nonsymmetrized, symmetrized, or antisymmetrized wave function
  when ISym is set to 1, 2, or 3. }
{ This subroutine alters the global variable WJointPD. }
{ Dependencies: WJointPD.Put is a CUPSproc subroutine. }

VAR
   J1, J2: Integer;
   temp, Value: Real;

BEGIN

   CASE (ISym) OF

      1: BEGIN   { Nonsymmetrized }

            FOR J1 := 1 TO NPoints DO
            BEGIN
            FOR J2 := 1 TO NPoints DO
            BEGIN

               Value := Sqr (Psi[J1,J2,1]) + Sqr (Psi[J1,J2,2]);
               WJointPD.Put (NPoints+1-J2, J1, Value);
               { i.e. JointPD[J1,J2] := Value; }

            END; {of FOR J2}
            END; {of FOR J1}

         END;

      2: BEGIN   { Symmetrized }

            IF (2.0 + Zsa = 0.0) THEN temp := 0.0 ELSE
               temp := 1.0/(2.0 + Zsa);

            FOR J1 := 1 TO NPoints DO
            BEGIN
            FOR J2 := 1 TO NPoints DO
            BEGIN

               Value := temp * (  Sqr (Psi[J1,J2,1] + Psi[J2,J1,1])
                                + Sqr (Psi[J1,J2,2] + Psi[J2,J1,2]) );
               WJointPD.Put (NPoints+1-J2, J1, Value);
               { i.e. JointPD[J1,J2] := Value; }

            END; {of FOR J2}
            END; {of FOR J1}

         END;

      3: BEGIN   { Antisymmetrized }

            IF (2.0 - Zsa = 0.0) THEN temp := 0.0 ELSE
               temp := 1.0/(2.0 - Zsa);

            FOR J1 := 1 TO NPoints DO
            BEGIN
            FOR J2 := 1 TO NPoints DO
            BEGIN

               Value := temp * (  Sqr (Psi[J1,J2,1] - Psi[J2,J1,1])
                                + Sqr (Psi[J1,J2,2] - Psi[J2,J1,2]) );
               WJointPD.Put (NPoints+1-J2, J1, Value);
               { i.e. JointPD[J1,J2] := Value; }

            END; {of FOR J2}
            END; {of FOR J1}

         END;

   END; {of CASE ISym}

END; {of PROCEDURE FindJointPD}



PROCEDURE FindReducedPD (VAR ReducedPD: DataVector; VAR MeanS: Real);
{ Given a joint probability density stored in the global variable
  WJointPD, find a reduced probability density: either the
  separation PD or the single PD, depending upon the global variable
  PlotWhat.  If the separation PD is found, then find also the mean
  value of separation. }
{ This subroutine does not alter any global variable. }
{ Dependencies: WJointPD.Value is a CUPSproc subroutine. }

VAR
   J1, Js, J: Integer;
   temp, s: Real;

BEGIN

   IF (PlotWhat = 1) THEN
   BEGIN

      { Generate the separation probability density. }
      s := 0.0;
      MeanS := 0.0;

      FOR Js := 1 TO NPoints DO
      BEGIN
         temp := 0.0;
         FOR J1 := 1 TO NPoints - (Js-1) DO
            temp := temp + WJointPD.Value(NPoints+1-J1-(Js-1),J1)
                           { i.e. JointPD[J1,J1+(Js-1)] }
                         + WJointPD.Value(NPoints+1-J1,J1+(Js-1))
                           { i.e. JointPD[J1+(Js-1),J1] };
         ReducedPD[Js] := Dx * temp;

         MeanS := MeanS + Dx*s*ReducedPD[Js];
         s := s + Dx;
      END; {of FOR Js}
      ReducedPD[1] := 0.5 * ReducedPD[1];

   END
   ELSE
   BEGIN

      { Generate the single probability density. }
      FOR J := 1 TO NPoints DO
      BEGIN
         temp := 0.0;
         FOR J1 := 1 TO NPoints DO
            temp := temp + WJointPD.Value(NPoints+1-J,J1)
                           { i.e. JointPD[J1,J] }
                         + WJointPD.Value(NPoints+1-J1,J)
                           { i.e. JointPD[J,J1] };
         ReducedPD[J] := (0.5 * Dx) * temp;
      END; {of FOR J}

   END;

END; {of PROCEDURE FindReducedPD}



{---Output (Plots)-----------------------------------------------------------}

PROCEDURE InitializePlots;
{ This subroutine does not alter any global variable. }
{ Dependencies:  DefineViewPort and DefineScale are CUPSmupp subroutines.
                 GraphBackColor is a CUPS unit variable.
                 Blakk is a CUPS unit constant.
                 WJointPD.Init is a CUPSproc subroutine.
                 ContourPlot.Init and .SetHeights are CUPSgrph subroutines.
                 HeightType is a CUPSgrph data type. }

VAR
   I: Integer;
   ContourInterval, Ratio, Height, Gap, Base: Real;
   ContourElevations: HeightType;

BEGIN

{ CUPSmupp initializations. }
   GraphBackColor := Blakk;

   Ratio := 480.0/640.0;

   { For plots of nonsymmetrized, symmetrized, or antisymmetrized through
     procedure PlotProbDensity. }
   Height := 0.55;
   { Square plot of joint probability density vs. (x1,x2). }
   DefineViewPort (1, 0.143, 0.143 + Ratio*Height, 0.35, 0.35 + Height);
   { Plot of reduced probability density vs. x. }
   DefineViewPort (2, 0.143, 0.143 + Ratio*Height, 0.15, 0.25);

   { For comparison of symmetrized and antisymmetrized through
     procedure PlotCompare2. }
   Height := 0.50;
   { Joint probability density, symmetrized. }
   DefineViewPort (3, 0.1, 0.1 + Ratio*Height, 0.35, 0.35 + Height);
   { Reduced probability density, symmetrized. }
   DefineViewPort (4, 0.1, 0.1 + Ratio*Height, 0.15, 0.25);
   { Joint probability density, antisymmetrized. }
   DefineViewPort (5, 0.55, 0.55 + Ratio*Height, 0.35, 0.35 + Height);
   { Reduced probability density, antisymmetrized. }
   DefineViewPort (6, 0.55, 0.55 + Ratio*Height, 0.15, 0.25);

   { For three comparison plots of reduced probability
     density through procedure PlotCompare3. }
   Height := 0.21; Gap := 0.06;
   Base := 0.15;
   FOR I := 9 DOWNTO 7 DO
   BEGIN
      DefineViewPort (I, 0.10, 0.69, Base, Base + Height);
      Base := Base + Height + Gap;
   END;

   { Define scales for joint, separation, and single probability density
     plots, respectively. }
   DefineScale (1, xmin, xmax, xmin, xmax);
   DefineScale (2, 0.0, xmax-xmin, 0.0, 6.0);
   DefineScale (3, xmin, xmax, 0.0, 3.0);

{ CUPSproc initializations. }
   WJointPD.Init (NPoints, NPoints);

{ CUPSgrph initializations. }
   ContourInterval := 10.0/(NContours*Sqr (xmax-xmin));
   FOR I := 1 TO NContours DO
      ContourElevations[I] := ContourInterval*I;

   ContourPlot.Init;
   ContourPlot.SetHeights (NContours, ContourElevations);

END; {of PROCEDURE InitializePlots}



PROCEDURE PlotProbDensity;
{ Make a contour plot of joint probability density vs. (x1,x2) and
  an ordinary plot of separation probability density vs. s or of
  single probability density vs. x. }
{ This subroutine alters the global variable WJointPD through its call
  to FindJointPD. }
{ Dependencies: SetColor is a Turbo Graph subroutine.
                Yellow, LightMagenta, and LightGray are Turbo Graph constants.
                Whitte is a CUPS unit constant.
                OpenViewPort, SelectScale, Axis, PutLabel, PlotData,
                    and PlotLine are CUPSmupp subroutines.
                DataVector is a CUPSmupp data type.
                ContourPlot.DrawMatrix is a CUPSgrph subroutine. }

VAR
   Value, MeanS: Real;
   ReducedPD: DataVector;

BEGIN

   { Find and plot joint probability density. }
   FindJointPD (SymNum);
   OpenViewPort (1);
   SelectScale (1);
   Axis (xmin, xmin, 1.0, 1.0);
   PutLabel (Bottom, 'x1 (nm)');
   PutLabel (Left,   'x2 (nm)');
   ContourPlot.DrawMatrix (WJointPD);
   IF (SymNum <> 1) THEN PlotLine (xmin, xmin, xmax, xmax);

   { Find and plot separation or single probability density. }
   FindReducedPD (ReducedPD, MeanS);
   OpenViewPort (2);
   IF (PlotWhat = 1) THEN
   BEGIN   { Separation probability density vs. s. }

      SelectScale (2);
      Axis (0.0, 0.0, 1.0, 6.0);
      PutLabel (Bottom, 's (nm)');
      PutLabel (Left, 'PD_sep');
      SetColor (LightMagenta);
      PlotData (sValue, ReducedPD, NPoints);

      IF (ShowS > 0) THEN
      BEGIN   { Draw vertical line at mean value of s. }
         SetColor (LightGray);
         PlotLine (MeanS, 0.0, MeanS, 6.0);
      END;

      IF (PrintS) THEN
         WriteLn (PrintSFile, DtSI*Itime:9:3, MeanS);

   END
   ELSE
   BEGIN   { Single probability density vs. x. }

      SelectScale (3);
      Axis (xmin, 0.0, 1.0, 3.0);
      PutLabel (Bottom, 'x (nm)');
      PutLabel (Left, 'PD_single');
      SetColor (Yellow);
      PlotData (xValue, ReducedPD, NPoints);

   END;
   SetColor (Whitte);

END; {of PROCEDURE PlotProbDensity}



PROCEDURE PlotCompare2;
{ Make joint and reduced probability density plots for both
  symmetrized and antisymmetrized wave functions. }
{ This subroutine alters the global variable WJointPD through its call
  to FindJointPD. }
{ Dependencies: SetColor is a Turbo Graph subroutine.
                Yellow and LightMagenta are Turbo Graph constants.
                Whitte is a CUPS unit constant.
                OpenViewPort, SelectScale, Axis, PutLabel, PlotData,
                    and PlotLine are CUPSmupp subroutines.
                DataVector is a CUPSmupp data type.
                ContourPlot.DrawMatrix is a CUPSgrph subroutine. }

VAR
   Value, temp, MeanSs, MeanSa: Real;
   ReducedPD: DataVector;

BEGIN

   { Symmetrized first. }

   { Find and plot joint probability density. }
   FindJointPD (2);
   OpenViewPort (3);
   SelectScale (1);
   Axis (xmin, xmin, 1.0, 1.0);
   PutLabel (Bottom, 'x1 (nm)');
   PutLabel (Left,   'x2 (nm)');
   PutLabel (Top, 'Symmetrized');
   ContourPlot.DrawMatrix (WJointPD);
   PlotLine (xmin, xmin, xmax, xmax);

   { Find and plot reduced probability density. }
   FindReducedPD (ReducedPD, MeanSs);
   OpenViewPort (4);
   IF (PlotWhat = 1) THEN
   BEGIN   { Separation probability density vs. s. }

      SelectScale (2);
      Axis (0.0, 0.0, 1.0, 6.0);
      PutLabel (Bottom, 's (nm)');
      PutLabel (Left, 'PD_sep');
      SetColor (LightMagenta);
      PlotData (sValue, ReducedPD, NPoints);

      IF (ShowS > 0) THEN
      BEGIN   { Draw vertical line at mean value of s. }
         SetColor (LightGray);
         PlotLine (MeanSs, 0.0, MeanSs, 6.0);
      END;

   END
   ELSE
   BEGIN   { Single probability density vs. x. }

      SelectScale (3);
      Axis (xmin, 0.0, 1.0, 3.0);
      PutLabel (Bottom, 'x (nm)');
      PutLabel (Left, 'PD_single');
      SetColor (Yellow);
      PlotData (xValue, ReducedPD, NPoints);

   END;
   SetColor (Whitte);

   { Antisymmetrized second. }

   { Find and plot joint probability density. }
   FindJointPD (3);
   OpenViewPort (5);
   SelectScale (1);
   Axis (xmin, xmin, 1.0, 1.0);
   PutLabel (Bottom, 'x1 (nm)');
   PutLabel (Top, 'Antisymmetrized');
   ContourPlot.DrawMatrix (WJointPD);
   PlotLine (xmin, xmin, xmax, xmax);

   { Find and plot reduced probability density. }
   FindReducedPD (ReducedPD, MeanSa);
   OpenViewPort (6);
   IF (PlotWhat = 1) THEN
   BEGIN   { Separation probability density vs. s. }

      SelectScale (2);
      Axis (0.0, 0.0, 1.0, 6.0);
      PutLabel (Bottom, 's (nm)');
      SetColor (LightMagenta);
      PlotData (sValue, ReducedPD, NPoints);

      IF (ShowS > 0) THEN
      BEGIN   { Draw vertical line at mean value of s. }
         SetColor (LightGray);
         PlotLine (MeanSa, 0.0, MeanSa, 6.0);
      END;

      IF (PrintS) THEN
         WriteLn (PrintSFile, DtSI*Itime:9:3, MeanSs, MeanSa);

   END
   ELSE
   BEGIN   { Single probability density vs. x. }

      SelectScale (3);
      Axis (xmin, 0.0, 1.0, 3.0);
      PutLabel (Bottom, 'x (nm)');
      SetColor (Yellow);
      PlotData (xValue, ReducedPD, NPoints);

   END;
   SetColor (Whitte);

END; {of PROCEDURE PlotCompare2}



PROCEDURE PlotCompare3;
{ Make reduced probability density plots for nonsymmetrized,
  symmetrized, and antisymmetrized wave functions. }
{ This subroutine alters the global variable WJointPD through its call
  to FindJointPD. }
{ Dependencies: SetColor is a Turbo Graph subroutine.
                Yellow and LightMagenta are Turbo Graph constants.
                Whitte is a CUPS unit constant.
                OpenViewPort, SelectScale, Axis, PutLabel, PlotData,
                    and PlotLine are CUPSmupp subroutines.
                DataVector is a CUPSmupp data type. }

VAR
   ISym: Integer;
   Value, temp: Real;
   MeanS: ARRAY[1..3] OF Real;
   ReducedPD: DataVector;

BEGIN

   FOR ISym := 1 TO 3 DO
   BEGIN

      { Generate the joint probability density. }
      FindJointPD (ISym);

      { Find and plot separation or single probability density. }
      FindReducedPD (ReducedPD, MeanS[ISym]);
      OpenViewPort (ISym + 6);
      IF (PlotWhat = 1) THEN
      BEGIN   { Separation probability density vs. s. }

         SelectScale (2);
         Axis (0.0, 0.0, 1.0, 6.0);
         CASE ISym OF
            1: PutLabel (Left, 'Nonsymm.');
            2: PutLabel (Left, 'Symm.');
            3: PutLabel (Left, 'Antisymm.');
         END;
         IF (ISym = 3) THEN PutLabel (Bottom, 's (nm)');
         SetColor (LightMagenta);
         PlotData (sValue, ReducedPD, NPoints);

         IF (ShowS > 0) THEN
         BEGIN   { Draw vertical line at mean value of s. }
            SetColor (LightGray);
            PlotLine (MeanS[ISym], 0.0, MeanS[ISym], 6.0);
         END;

      END
      ELSE
      BEGIN   { Single probability density vs. x. }

         SelectScale (3);
         Axis (xmin, 0.0, 1.0, 3.0);
         CASE ISym OF
            1: PutLabel (Left, 'Nonsymm.');
            2: PutLabel (Left, 'Symm.');
            3: PutLabel (Left, 'Antisymm.');
         END;
         IF (ISym = 3) THEN PutLabel (Bottom, 'x (nm)');
         SetColor (Yellow);
         PlotData (xValue, ReducedPD, NPoints);

      END;
      SetColor (Whitte);

   END; {of FOR ISym}

   IF (PrintS) AND (PlotWhat = 1) THEN
      WriteLn (PrintSFile, DtSI*Itime:9:3, MeanS[1], MeanS[2], MeanS[3]);

END; {of PROCEDURE PlotCompare3}



PROCEDURE Plot;
{ This subroutine does not alter any global variable. }
{ Dependencies: HideMouse, ShowMouse, and NumStr are CUPS unit subroutines.
                GraphBackColor is a CUPS unit variable.
                RubOutXY and PrintXY are CUPSmupp subroutines. }

BEGIN

   IF (NOT Running) THEN HideMouse;

   { Do major plotting. }
   CASE (SymNum) OF
      1,2,3: PlotProbDensity;
      4: PlotCompare2;
      5: PlotCompare3;
   END;

   { Output time and status information. }
   PrintXY (0.72, 0.83, 'Time: ');
   IF (SymNum <> 4) THEN RubOutXY (0.72, 0.80, 10, GraphBackColor);
   PrintXY (0.72, 0.80, Concat(NumStr(DtSI*Itime,10,3), ' fsec'));

   CASE (SymNum) OF
      1: PrintXY (0.72, 0.75, 'Nonsymmetrized');
      2: PrintXY (0.72, 0.75, 'Symmetrized');
      3: PrintXY (0.72, 0.75, 'Antisymmetrized');
   END;

   IF (NOT Running) THEN ShowMouse;

END; {of PROCEDURE Plot}



{---Process the wave function------------------------------------------------}

PROCEDURE NormalizeWF;
{ Normalize the active wave function and find the constant Zsa that
  enables later normalization of symmetric and antisymmetric wave functions. }
{ This subroutine alters the state global variables Psi and Zsa. }

VAR
   J, J1, J2: Integer;
   temp: Real;

BEGIN

   { Assure that wave function vanishes on boundaries. }
   FOR J := 1 TO NPoints DO
   BEGIN
      Psi[J,1,1] := 0.0;       Psi[J,1,2] := 0.0;
      Psi[J,NPoints,1] := 0.0; Psi[J,NPoints,2] := 0.0;
      Psi[1,J,1] := 0.0;       Psi[1,J,2] := 0.0;
      Psi[NPoints,J,1] := 0.0; Psi[NPoints,J,2] := 0.0;
   END;

   { Find normalization constant. }
   temp := 0.0;
   FOR J1 := 2 TO NPoints-1 DO
   BEGIN
   FOR J2 := 2 TO NPoints-1 DO
   BEGIN
      temp := temp + Sqr (Psi[J1,J2,1]) + Sqr (Psi[J1,J2,2]);
   END;
   END;

   { Normalize. }
   temp := Dx * Sqrt (temp);
   IF (temp <> 0.0) THEN
   BEGIN
      FOR J1 := 2 TO NPoints-1 DO
      BEGIN
      FOR J2 := 2 TO NPoints-1 DO
      BEGIN
         Psi[J1,J2,1] := Psi[J1,J2,1]/temp;
         Psi[J1,J2,2] := Psi[J1,J2,2]/temp;
      END;
      END;
   END; {of IF temp}

   { Calculate Zsa. }
   temp := 0.0;
   FOR J1 := 2 TO NPoints-1 DO
   BEGIN
   FOR J2 := 2 TO NPoints-1 DO
   BEGIN
      temp := temp + Psi[J1,J2,1]*Psi[J2,J1,1]
                   + Psi[J1,J2,2]*Psi[J2,J1,2];
   END;
   END;
   Zsa := 2 * Sqr (Dx) * temp;

END; {of PROCEDURE NormalizeWF}



PROCEDURE BuildWFGaussian;
{ Fill Psi[J1,J2,1..2] with a bivariate Gaussian wave packet. }
{ This subroutine alters the state global variable Psi. }

VAR
   J1, J2, J: Integer;
   x1ave, x2ave, k1, k2, g, d1, d2: Real;
   X1, X2, Magnitude, Phase: Real;

BEGIN

   x1ave := InitWFPara[1,1];
   x2ave := InitWFPara[1,2];
   k1 := InitWFPara[1,5];
   k2 := - k1;
   g := InitWFPara[1,6];
   d1 := InitWFPara[1,3] * (2.0 * Sqrt(1.0 - g*g));
   d2 := InitWFPara[1,4] * (2.0 * Sqrt(1.0 - g*g));

   { Build interior wave function. }
   FOR J1 := 2 TO NPoints-1 DO
   BEGIN
   FOR J2 := 2 TO NPoints-1 DO
   BEGIN
      X1 := (xValue[J1] - x1ave)/d1;
      X2 := (xValue[J2] - x2ave)/d2;
      Magnitude := Exp ( - X1*X1 + 2*g*X1*X2 - X2*X2 );
      Phase := k1*xValue[J1] + k2*xValue[J2];
      Psi[J1,J2,1] := Magnitude * Cos (Phase);
      Psi[J1,J2,2] := Magnitude * Sin (Phase);
   END; {of FOR J2}
   END; {of FOR J1}

   NormalizeWF;

END; {of PROCEDURE BuildWFGaussian}



PROCEDURE BuildWFEigen;
{ Fill Psi[J1,J2,1..2] with an energy eigenfunction. }
{ This subroutine alters the state global variable Psi. }
{ Dependencies: Pi is a Turbo function. }

VAR
   J1, J2, J: Integer;
   n, m, x1, x2, sin1: Real;
   sin2: ARRAY[1..NPoints] OF Real;

BEGIN

   n := InitWFPara[2,1];
   m := InitWFPara[2,2];

   x2 := xmin + Dx;
   FOR J2 := 2 TO NPoints-1 DO
   BEGIN
      sin2[J2] := Sin (m*Pi*(x2-xmin)/(xmax-xmin));
      x2 := x2 + Dx;
   END;

   { Build interior wave function. }
   x1 := xmin + Dx;
   FOR J1 := 2 TO NPoints-1 DO
   BEGIN

      sin1 := Sin (n*Pi*(x1-xmin)/(xmax-xmin));
      FOR J2 := 2 TO NPoints-1 DO
      BEGIN
         Psi[J1,J2,1] := sin1*sin2[J2];
         Psi[J1,J2,2] := 0.0;
      END; {of FOR J2}
      x1 := x1 + Dx;

   END; {of FOR J1}

   NormalizeWF;

END; {of PROCEDURE BuildWFEigen}



PROCEDURE BuildWFCombo;
{ Fill Psi[J1,J2,1..2] with a linear combination of energy eigenfunctions. }
{ This subroutine alters the state global variable Psi. }
{ Dependencies: Pi is a Turbo function. }

VAR
   J1, J2, J, I: Integer;
   n, m, x1, x2, sin1: Real;
   sin2: ARRAY[1..NPoints] OF Real;

BEGIN

   { Zero the wave function array. }
   FOR J1 := 2 TO NPoints-1 DO
   BEGIN
      FOR J2 := 2 TO NPoints-1 DO
      BEGIN
         Psi[J1,J2,1] := 0.0;
         Psi[J1,J2,2] := 0.0;
      END; {of FOR J2}
   END; {of FOR J1}

   FOR I := 1 TO 3 DO  { Loop over the three energy states being combined. }
   BEGIN

      { Select which energy eigenfunction is being used in this pass. }
      { If you want to make combinations with energy eigenfunctions
        different from those built in--namely (2,1), (1,3), and (3,2)--you
        only need to change statements in this block. }
      IF (I = 1) THEN BEGIN n := 2; m := 1; END;
      IF (I = 2) THEN BEGIN n := 1; m := 3; END;
      IF (I = 3) THEN BEGIN n := 3; m := 2; END;

      { Find and store values of u_m(x2). }
      x2 := xmin + Dx;
      FOR J2 := 2 TO NPoints-1 DO
      BEGIN
         sin2[J2] := Sin (m*Pi*(x2-xmin)/(xmax-xmin));
         x2 := x2 + Dx;
      END;

      { Build interior wave function. }
      x1 := xmin + Dx;
      FOR J1 := 2 TO NPoints-1 DO
      BEGIN

         sin1 := Sin (n*Pi*(x1-xmin)/(xmax-xmin));
         FOR J2 := 2 TO NPoints-1 DO
         BEGIN
            Psi[J1,J2,1] := Psi[J1,J2,1] +
                               InitWFPara[3,I+I-1] * (sin1 * sin2[J2]);
            Psi[J1,J2,2] := Psi[J1,J2,2] +
                               InitWFPara[3,I+I] * (sin1 * sin2[J2]);
         END; {of FOR J2}
         x1 := x1 + Dx;

      END; {of FOR J1}

   END; {of FOR I}

   NormalizeWF;

END; {of PROCEDURE BuildWFCombo}



PROCEDURE BuildWF;
{ Fill Psi[J1,J2,1..2] with the desired initial wave function. }
{ This subroutine alters the state global variable Psi. }

VAR
   J1, J2: Integer;

BEGIN

   CASE ActiveInitWF OF
      1: BuildWFGaussian;
      2: BuildWFEigen;
      3: BuildWFCombo;
      4: BEGIN  { User defined wave function }

            FOR J1 := 2 TO NPoints-1 DO
            BEGIN
            FOR J2 := 2 TO NPoints-1 DO
            BEGIN
               UserDefinedWF (xValue[J1], xValue[J2],
                              Psi[J1,J2,1], Psi[J1,J2,2]);
            END; {of FOR J2}
            END; {of FOR J1}

            NormalizeWF;

         END;
   END; {of CASE ActiveInitWF}

END; {of PROCEDURE BuildWF}



{---Input--------------------------------------------------------------------}

PROCEDURE GetWFParaGaussian (VAR DoIt:Boolean);
{ Produce and read pop up screen for a bivariate Gaussian wave packet. }
{ This subroutine alters the profile global variable InitWFPara. }
{ Dependencies: All subroutines are from CUPSgui. }

VAR
   I: Integer;

BEGIN
WITH InputScreen DO
BEGIN

   Init;
   DefineInputPort (0.17, 0.95, 0.03, 0.93);
   LoadLine ('                                                      ');
   LoadLine ('    The (unnormalized) nonsymmetrized initial         ');
   LoadLine ('    wave function is the bivariate Gaussian           ');
   LoadLine (' Psi(x1,x2) = exp ( - (X1^2 - 2*g*X1*X2 + X2^2)       ');
   LoadLine ('                    + i (k*x1 - k*x2) )               ');
   LoadLine (' where X1 = (x1 - <x1>)/d1;   X2 = (x2 - <x2>)/d2.    ');
   LoadLine ('                                                      ');
   LoadLine ('    Mean of x1:                 {     }               ');
   LoadLine ('    Mean of x2:                 {     }               ');
   LoadLine ('    Uncertainty in x1:          {     }               ');
   LoadLine ('    Uncertainty in x2:          {     }               ');
   LoadLine ('    Mean momentum k:            {     }               ');
   LoadLine ('    Correlation coefficient g:  {     }  (-1 < g < 1) ');
   LoadLine ('                                                      ');
   LoadLine ('  [  Ok  ]   [Cancel]                                 ');

   FOR I := 1 TO 6 DO
      SetNumber (I, InitWFPara[1,I]);
   SetNumberLimits (1, -3.0, +3.0);
   SetNumberLimits (2, -3.0, +3.0);
   SetNumberLimits (3, 0.1, 4.0);
   SetNumberLimits (4, 0.1, 4.0);
   SetNumberLimits (5, -1.0/Dx, +1.0/Dx);
   SetNumberLimits (6, -0.9, +0.9);

   AcceptScreen;

   DoIt := NOT Canceled;
   IF (DoIt) THEN
   BEGIN
      FOR I := 1 TO 6 DO
         InitWFPara[1,I] := GetNumber (I);
   END;
   Done;

END; {of WITH InputScreen}
END; {of PROCEDURE GetWFParaGaussian}



PROCEDURE GetWFParaEigen (VAR DoIt:Boolean);
{ Produce and read pop up screen for an energy eigenfunction. }
{ This subroutine alters the profile global variable InitWFPara. }
{ Dependencies: All subroutines are from CUPSgui. }

BEGIN
WITH InputScreen DO
BEGIN

   Init;
   DefineInputPort (0.17, 0.85, 0.20, 0.80);
   LoadLine ('                                                ');
   LoadLine ('    The (unnormalized) nonsymmetrized initial   ');
   LoadLine ('    wave function is the energy eigenfunction   ');
   LoadLine ('       Psi(x1,x2) = u_n(x1) * u_m(x2),          ');
   LoadLine ('    where u_i(x) is the ith eigenfunction of    ');
   LoadLine ('    the one-particle problem.                   ');
   LoadLine ('                                                ');
   LoadLine ('        n:  {   }       m:   {   }   (1-5)      ');
   LoadLine ('                                                ');
   LoadLine ('  [  Ok  ]   [Cancel]                           ');

   SetNumber (1, InitWFPara[2,1]);
   SetNumber (2, InitWFPara[2,2]);
   SetNumberLimits (1, 1, 5);
   SetNumberLimits (2, 1, 5);

   AcceptScreen;

   DoIt := NOT Canceled;
   IF (DoIt) THEN
   BEGIN
      InitWFPara[2,1] := GetNumber (1);
      InitWFPara[2,2] := GetNumber (2);
   END;
   Done;

END; {of WITH InputScreen}
END; {of PROCEDURE GetWFParaEigen}



PROCEDURE GetWFParaCombo (VAR DoIt:Boolean);
{ Produce and read pop up screen for a linear combination of energy
  eigenfunctions. }
{ This subroutine alters the profile global variable InitWFPara. }
{ Dependencies: All subroutines are from CUPSgui. }

VAR
   I: Integer;

BEGIN
WITH InputScreen DO
BEGIN

   Init;

   DefineInputPort (0.17, 0.95, 0.03, 0.93);
   LoadLine ('                                                          ');
   LoadLine (' The (unnormalized) nonsymmetrized initial wave function  ');
   LoadLine (' is a linear combination of three energy eigenfunctions   ');
   LoadLine ('                                                          ');
   LoadLine ('          Psi(x1,x2) =   a_A * u_2(x1) * u_1(x2)          ');
   LoadLine ('                       + a_B * u_1(x1) * u_3(x2)          ');
   LoadLine ('                       + a_C * u_3(x1) * u_2(x2),         ');
   LoadLine ('                                                          ');
   LoadLine (' where u_n(x) is the nth energy eigenfunction of the      ');
   LoadLine (' one-particle problem.                                    ');
   LoadLine ('                         a_A:   {    } + i {    }         ');
   LoadLine ('                         a_B:   {    } + i {    }         ');
   LoadLine ('                         a_C:   {    } + i {    }         ');
   LoadLine ('                                                          ');
   LoadLine ('  [  Ok  ]   [Cancel]                                     ');

   FOR I := 1 TO 6 DO
      SetNumber (I, InitWFPara[3,I]);

   AcceptScreen;

   DoIt := NOT Canceled;
   IF (DoIt) THEN
   BEGIN
      FOR I := 1 TO 6 DO
         InitWFPara[3,I] := GetNumber (I);
   END; {of IF DoIt}
   Done;

END; {of WITH InputScreen}
END; {of PROCEDURE GetWFParaCombo}



{---Action on global variables-----------------------------------------------}

PROCEDURE SetStaticVariables;
{ Set the global variables that will not be altered as the program runs. }

VAR
   J: Integer;

BEGIN

   xmin := - 1.0;  xmax := + 1.0;   { The edges of the box. }
   Dx := (xmax - xmin)/(NPoints - 1);   { Grid spacing. }
   DtSI := 0.025;   { Time step in femtoseconds. }
   Dt := 0.11576765 * DtSI;   { The "scaled" time step: time*hbar/m in nm^2,
      where m is the electron's mass. }

   FOR J := 1 TO NPoints DO
   BEGIN
      xValue[J] := Dx*(J-1) + xmin;
      sValue[J] := Dx*(J-1);
   END;

END; {of PROCEDURE SetStaticVariables}



PROCEDURE SetSteppingVariables;
{ Set the global variables that will be used by procedure StepWF to
  step the wave function. }

VAR
   J: Integer;
   Re, Im: Real;

BEGIN

   DIm := 4*IDirection * Dx*Dx/Dt;

   U[1,1] := 0.0; U[1,2] := 0.0;
   FOR J := 2 TO NPoints-1 DO
   BEGIN

      { U[J] := 1/(D - U[J-1]); }
      Re := - 2.0 - U[J-1,1]; Im := DIm - U[J-1,2];
      U[J,1] :=   Re/(Re*Re + Im*Im);
      U[J,2] := - Im/(Re*Re + Im*Im);

   END; {of FOR J}

END; {of PROCEDURE SetSteppingVariables}



PROCEDURE SetDefaultProfile;
{ Set the global variables that specify the default profile. }

BEGIN

   ActiveInitWF := 1;

   InitWFPara[1,1] := -0.3;   { Mean of x1, in nm }
   InitWFPara[1,2] := +0.3;   { Mean of x2, in nm }
   InitWFPara[1,3] := 0.15;   { Uncertainty in x1, in nm }
   InitWFPara[1,4] := 0.10;   { Uncertainty in x2, in nm }
   InitWFPara[1,5] := 5.00;   { Mean momentum k (actually momentum/hbar),
                                in nm^-1 }
   InitWFPara[1,6] := 0.50;   { Correlation coefficient }

   InitWFPara[2,1] := 1.0;
   InitWFPara[2,2] := 2.0;

   InitWFPara[3,1] :=  1.0;   { Expansion coefficients }
   InitWFPara[3,2] :=  0.0;
   InitWFPara[3,3] :=  0.0;
   InitWFPara[3,4] := -0.5;
   InitWFPara[3,5] :=  1.0;
   InitWFPara[3,6] :=  0.0;

   SymNum := 1;
   PlotWhat := 1;
   ShowS := -1;
   AnimationSpeed := 0;

END; {of PROCEDURE SetDefaultProfile}



{---Interface----------------------------------------------------------------}

PROCEDURE UpdateMenu;
{ Update menu checks. }
{ This subroutine alters the global object variable Menu. }
{ Dependencies: Menu is a CUPSgui object variable. }

VAR
   I: Integer;

BEGIN

   FOR I := 1 TO 4 DO
      Menu.RowChecked (2, I, FALSE);
   Menu.RowChecked (2, ActiveInitWF, TRUE);

   FOR I := 1 TO 5 DO
      Menu.RowChecked (3, I, FALSE);
   Menu.RowChecked (3, SymNum, TRUE);

   FOR I := 1 TO 2 DO
      Menu.RowChecked (4, I, FALSE);
   Menu.RowChecked (4, PlotWhat, TRUE);
   IF (ShowS < 0) THEN
      Menu.RowChecked (4, 3, FALSE)
   ELSE
      Menu.RowChecked (4, 3, TRUE);

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
         Row (1, 2, 'About Program');   { I prefer "About Ident". }
         Row (1, 3, 'Things to Notice');
         Row (1, 4, 'Configuration');
         Row (1, 5, 'Open...');
         Row (1, 6, 'Save as...');
         Row (1, 7, 'Log Mean Separation');
         Row (1, 8, 'Exit Program');
      Column (2, 'Wave func');
         Row (2, 1, 'Bivariate Gaussian');
         Row (2, 2, 'Energy Eigenstate');
         Row (2, 3, 'Combination of Energy States');
         Row (2, 4, 'User Defined');
      Column (3, 'Symmetry');
         Row (3, 1, 'Nonsymmetrized');
         Row (3, 2, 'Symmetrized');
         Row (3, 3, 'Antisymmetrized');
         Row (3, 4, 'Compare Symm. and Antisymm.');
         Row (3, 5, 'Compare all Three');
      Column (4, 'Plot What');
         Row (4, 1, 'Separation Probability Density');
         Row (4, 2, 'Single Probability Density');
         Row (4, 3, 'Show Mean Separation');
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
   Screen[02] := '     IDENTICAL PARTICLES IN QUANTUM MECHANICS     ';
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
   Screen[13] := '     Time development of two noninteracting       ';
   Screen[14] := '  identical particles in a one-dimensional        ';
   Screen[15] := '  infinite square well.                           ';
   Screen[16] := '                                                  ';
   Screen[17] := '     The initial wave function is controlled      ';
   Screen[18] := '  through menu entry "Wave func".  The inter-     ';
   Screen[19] := '  change symmetry and display are controlled      ';
   Screen[20] := '  through entries "Symmetry" and "Plot What".     ';
   Screen[21] := '  To start the animation, simply choose "Run".    ';
   Screen[22] := '                                                  ';
   Screen[23] := '                                                  ';
{  Screen[24] := '    Press any key or click mouse to continue.     '; }
   Screen[24] := '                                                  ';
   Screen[25] := '                                                  ';

   Help (Screen);

END; {of PROCEDURE GiveWelcome}



PROCEDURE AboutIdent;
{ Show three pages of information. }
{ Dependencies: HelpScrType is a CUPSgui type.
                Help is a CUPSgui subroutine. }

VAR
  Screen: HelpScrType;

BEGIN

   GiveWelcome;

   Screen[01] := '                                                  ';
   Screen[02] := '              --- Wave functions ---              ';
   Screen[03] := '                                                  ';
   Screen[04] := ' If Psi(x1,x2) is a wave function (such as        ';
   Screen[05] := ' the initial wave function entered through the    ';
   Screen[06] := ' "Wave func" screen), then                        ';
   Screen[07] := '                                                  ';
   Screen[08] := ' the nonsymmetrized wave function is              ';
   Screen[09] := '          A_n * Psi(x1,x2),                       ';
   Screen[10] := '                                                  ';
   Screen[11] := ' the symmetrized wave function is                 ';
   Screen[12] := '          A_s * [Psi(x1,x2) + Psi(x2,x1)],        ';
   Screen[13] := '                                                  ';
   Screen[14] := ' and the antisymmetrized wave function is         ';
   Screen[15] := '          A_a * [Psi(x1,x2) - Psi(x2,x1)],        ';
   Screen[16] := '                                                  ';
   Screen[17] := ' where the A''s are normalization constants.       ';
   Screen[18] := '                                                  ';
   Screen[19] := '                                                  ';
   Screen[20] := '                                                  ';
   Screen[21] := '                                                  ';
   Screen[22] := '                                                  ';
   Screen[23] := '                                                  ';
   Screen[24] := '                                                  ';
   Screen[25] := '                                                  ';

   Help (Screen);

   Screen[01] := '                                                  ';
   Screen[02] := '          --- Probability Densities ---           ';
   Screen[03] := '                                                  ';
   Screen[04] := ' The joint probability density is shown on a      ';
   Screen[05] := ' contour plot.                                    ';
   Screen[06] := '                                                  ';
   Screen[07] := '   PD_joint(x1,x2) * dx1 * dx2 is the probability ';
   Screen[08] := '   of finding a particle in interval dx1 about    ';
   Screen[09] := '   x1 and a particle in interval dx2 about x2.    ';
   Screen[10] := '                                                  ';
   Screen[11] := ' The separation probability density is shown in   ';
   Screen[12] := ' magenta on an ordinary plot.                     ';
   Screen[13] := '                                                  ';
   Screen[14] := '   PD_sep(s) * ds is the probability of           ';
   Screen[15] := '   finding the two particles separated by a       ';
   Screen[16] := '   distance in the interval ds about s.           ';
   Screen[17] := '                                                  ';
   Screen[18] := ' The single probability density is shown in       ';
   Screen[19] := ' yellow on an ordinary plot.                      ';
   Screen[20] := '                                                  ';
   Screen[21] := '   PD_single(x) * dx is the probability of        ';
   Screen[22] := '   finding a particle in interval dx about x.     ';
   Screen[23] := '                                                  ';
   Screen[24] := '                                                  ';
   Screen[25] := '                                                  ';

   Help (Screen);

END; {of PROCEDURE AboutIdent}



PROCEDURE GiveHints;
{ Suggest things to look for. }
{ Dependencies: HelpScrType is a CUPSgui type.
                Help is a CUPSgui subroutine. }

VAR
  Screen: HelpScrType;

BEGIN

   Screen[01] := '                                                  ';
   Screen[02] := '  Things to notice:                               ';
   Screen[03] := '                                                  ';
   Screen[04] := ' 1. This program always displays a probability    ';
   Screen[05] := '    density, never a wave function.               ';
   Screen[06] := '                                                  ';
   Screen[07] := ' 2. To investigate the properties of symmetric    ';
   Screen[08] := '    and antisymmetric wave functions, try the     ';
   Screen[09] := '    bivariate Gaussian both with the default      ';
   Screen[10] := '    values and with x1ave = -0.05, x2ave =        ';
   Screen[11] := '    +0.05.  What happens if the nonsymmetrized    ';
   Screen[12] := '    wave function is already symmetric?           ';
   Screen[13] := '                                                  ';
   Screen[14] := ' 3. One way to think of the time development      ';
   Screen[15] := '    part of this program is simply as a means     ';
   Screen[16] := '    of making many different and varied non-      ';
   Screen[17] := '    symmetric wave functions to be symmetrized    ';
   Screen[18] := '    or antisymmetrized.                           ';
   Screen[19] := '                                                  ';
   Screen[20] := '                                                  ';
   Screen[21] := '                                                  ';
   Screen[22] := '                                                  ';
   Screen[23] := '                                                  ';
   Screen[24] := '                                                  ';
   Screen[25] := '                                                  ';

{  Screen[20] := ' 4. A sidelight: If you choose "Symmetry|         '; }
{  Screen[21] := '    Nonsymmetrized", you may interpret what you   '; }
{  Screen[22] := '    see as the time development of a single       '; }
{  Screen[23] := '    particle in a two-dimensional square well,    '; }
{  Screen[24] := '    which has nothing to do with identical        '; }
{  Screen[25] := '    particles but is interesting in its own right.'; }
{ Remark 4 is true, but given that one of the main aims of the program is
  to stop students from thinking of PD_joint(x1,x2) as a function in (x,y)
  space, it is counterproductive. }

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
      FileName := FSearch ('IdtInit.pfl', ' ')
   ELSE
      FileName := OpenFile ('*.pfl');

   IF (FileName <> '') THEN
   BEGIN

      Assign (InputFile, FileName);
      Reset (InputFile);

      ReadLn (InputFile, Title);
      IF (Title = ' Profile file produced by program Ident') THEN
      BEGIN

         ReadLn (InputFile);   { Ignore the comment line. }
         ReadLn (InputFile, ActiveInitWF);
         FOR I := 1 TO 6 DO
            ReadLn (InputFile, InitWFPara[1,I]);
         ReadLn (InputFile, InitWFPara[2,1]);
         ReadLn (InputFile, InitWFPara[2,2]);
         FOR I := 1 TO 6 DO
            ReadLn (InputFile, InitWFPara[3,I]);
         ReadLn (InputFile, SymNum, PlotWhat, ShowS, AnimationSpeed);

      END
      ELSE

         Announce (Concat('File ', FileName,
                          ' is not an Ident profile file.'));

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

   FileName := 'idt00.pfl';
   GetFileName (FileName);
   IF (FileName <> '') THEN
   BEGIN

      Assign (OutputFile, FileName);
      Rewrite (OutputFile);

      WriteLn (OutputFile, ' Profile file produced by program Ident');

      GetComment (Comment);
      WriteLn (OutputFile, Comment);

      WriteLn (OutputFile, ActiveInitWF:3);
      FOR I := 1 TO 6 DO
         WriteLn (OutputFile, ' ', InitWFPara[1,I]);
      WriteLn (OutputFile, ' ', InitWFPara[2,1]);
      WriteLn (OutputFile, ' ', InitWFPara[2,2]);
      FOR I := 1 TO 6 DO
         WriteLn (OutputFile, ' ', InitWFPara[3,I]);
      WriteLn (OutputFile, SymNum:3, PlotWhat:3, ShowS:3, AnimationSpeed:3);

      Close (OutputFile);

   END; {of IF FileName}

END; {of PROCEDURE SaveProfileFile}



PROCEDURE StartPrintingS;
{ Start printing the mean values of separation. }
{ This subroutine does not alter any global variable. }

BEGIN

   Rewrite (PrintSFile);

   WriteLn (PrintSFile, ' Data file produced by program Ident');

   IF (SymNum = 1) THEN
      WriteLn (PrintSFile, '    time  <s(t)>:Nonsymm.' );
   IF (SymNum = 2) THEN
      WriteLn (PrintSFile, '    time  <s(t)>:Symm.' );
   IF (SymNum = 3) THEN
      WriteLn (PrintSFile, '    time  <s(t)>:Antisymm.' );
   IF (SymNum = 4) THEN
      WriteLn (PrintSFile, Concat('    time  <s(t)>:Symm.           ',
                           '<s(t)>:Antisymm.' ));
   IF (SymNum = 5) THEN
      WriteLn (PrintSFile, Concat('    time  <s(t)>:Nonsymm.        ',
                           '<s(t)>:Symm.           <s(t)>:Antisymm.' ));

   IF (PlotWhat = 2) THEN
   BEGIN
      WriteLn (PrintSFile, ' In order to get any data into this file, you');
      WriteLn (PrintSFile, ' must first choose "Plot What|Separation". ');
   END;

END; {of PROCEDURE StartPrintingS}



{---Flow control-------------------------------------------------------------}

PROCEDURE ProcessKeys (KeyNum: Byte; VAR Frames: TFrames);
{ Process pressing of a hot key. }
{ The global variables directly altered by this subroutine are PrintS,
  AnimationSpeed, Stepping, Running, IDirection, and Keys.  The routine
  can call SetSteppingVariables, which alters the global variables used
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
            SetSteppingVariables;
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
          { IF (IDirection = -1) THEN
            BEGIN
               IDirection := +1;
               SetSteppingVariables;
            END; }
            { Uncomment the above lines if you want any choice from the menu
              to insure that time is going forward. }
            Frames.Close;
            Keys.Clear;
            IF (PrintS) THEN
            BEGIN
               Close (PrintSFile);
               PrintS := FALSE;
            END;
         END;

   END; {of CASE KeyNum}

END; {of PROCEDURE ProcessKeys}



PROCEDURE ProcessMenu (VAR Frames: TFrames);
{ Process choice from command menu. }
{ This subroutine can alter any of the profile (except for
  AnimationSpeed), state, or flow control global variables, and PrintS
  as well.  It can also call the procedure UpdateMenu, which alters the
  global object variable Menu, or the procedure ShowKeyBanner, which
  alters the global object variable Keys. }
{ Dependencies: String is a Turbo data type.
                SetVisualPage is a Turbo Graph subroutine.
                Menu and Keys are CUPSgui object variables.
                TFrames is a CUPSgui object type.
                Configuration is a CUPSgui subroutine.
                ClearMUPPETPort is a CUPSmupp subroutine. }

VAR
   FileName: String;
   DoIt: Boolean;
      { This variable is returned TRUE if the user asks to make changes,
        FALSE if he changes his mind and asks to escape. }

BEGIN

   CASE Menu.ColChosen OF

      1: { File }
         CASE Menu.RowChosen OF
            1: AboutCUPS;
            2: AboutIdent;
            3: GiveHints;   { Things to notice }
            4: Configuration;
            5: BEGIN
                  ReadProfileFile (FALSE);
                  Itime := 0;
                  BuildWF;
                  UpdateMenu;
                  ClearMUPPETPort;
                  Plot;
               END;
            6: SaveProfileFile;
            7: BEGIN   { Log <s(t)> }
                  FileName := 'idt00.dat';
                  GetFileName (FileName);
                  IF (FileName <> '') THEN
                  BEGIN
                     PrintS := TRUE;
                     Assign (PrintSFile, FileName);
                     Rewrite (PrintSFile);
                  END;
               END;
            8: QuitNow := TRUE;
         END; {of CASE Menu.RowChosen}

      2: BEGIN   { Initial wave function }

            CASE Menu.RowChosen OF
               1: GetWFParaGaussian (DoIt);
               2: GetWFParaEigen (DoIt);
               3: GetWFParaCombo (DoIt);
               4: DoIt := TRUE;
            END; {of CASE Menu.RowChosen}

            IF (DoIt) THEN
            BEGIN
               ActiveInitWF := Menu.RowChosen;
               Itime := 0;
               BuildWF;
               UpdateMenu;
               ClearMUPPETPort;
               Plot;
            END;

         END;

      3: BEGIN   { Interchange symmetry }
            SymNum := Menu.RowChosen;
            UpdateMenu;
            ClearMUPPETPort;
            Plot;
         END;

      4: BEGIN   { Plot what }
            CASE Menu.RowChosen OF
               1: PlotWhat := 1;   { Plot separation PD }
               2: PlotWhat := 2;   { Plot single PD }
               3: ShowS := - ShowS;   { Show <s(t)> }
            END; {of CASE Menu.RowChosen}
            UpdateMenu;
            ClearMUPPETPort;
            Plot;
         END;

      5: { Reset }
         CASE Menu.RowChosen OF

            1: BEGIN   { Reset this run }
                  Itime := 0;
                  BuildWF;
                  Plot;
               END;

            2: BEGIN   { Reset to defaults }
                  SetDefaultProfile;
                  Itime := 0;
                  BuildWF;
                  UpdateMenu;
                  ClearMUPPETPort;
                  Plot;
               END;

         END; {of CASE Menu.Rowchosen}

      6: BEGIN   { Run }
            { IF (Menu.Rowchosen = 2) THEN Stepping := TRUE; }
            { To enable stepping from the menu, uncomment this line
              and two lines in SetUpInterface. }
            Running := TRUE;
            ShowKeyBanner (Frames);
            Frames.Select (1);
            SetVisualPage (1);
            IF (PrintS) THEN StartPrintingS;
         END;

   END; {of CASE Menu.ColChosen}

END; {of PROCEDURE ProcessMenu}



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
   PrintS := FALSE;
   ReadProfileFile (TRUE);
   Itime := 0;
   BuildWF;
   InitializePlots;
   Plot;
   SetUpInterface;
   GiveWelcome;

   QuitNow := FALSE;
   Running := FALSE;
   Stepping := FALSE;
   IDirection := +1;
   SetSteppingVariables;

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
         ELSE IF (MouseClickedInMenu) THEN
         BEGIN
            ProcessKeys (6, Frames);
            IF Menu.Chosen THEN ProcessMenu (Frames);
         END;

      END
      ELSE
      BEGIN   { Command (menu) mode }

         CheckForEvents;
         IF Menu.Chosen THEN ProcessMenu (Frames);

      END;

   END; {of IF NOT QuitNow}

   Menu.Done;
   CUPSDone;
END. {of PROGRAM Ident}
