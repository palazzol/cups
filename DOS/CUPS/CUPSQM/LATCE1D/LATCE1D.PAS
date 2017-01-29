{Program title: Electrons in a crystal lattice                        }
{Author: Ian D. Johnston             Copyright 1993                   }
{Version: 3.0                        25 April 1994                    }
{                                                                     }
{Computes energy eigenfunctions for an electron in a potential made up}
{of a number (<=10) of simple wells, which can be square, parabolic or}
{Coulombic.  The potential is assumed to be zero to the left of Xmin, }
{and to the right of Xmax.  The wave functions are computed by "hunt  }
{and shoot" methods.   One part of the program is designed to show the}
{student how to solve the problem in principle, by choosing a value of}
{the binding energy, calculating  from left to right and seeing if the}
{resulting function behaves like a decreasing exponential at x=Xmax.  }
{In another part of the program the eigenvalues are computed atuomat- }
{ically, by integrating from the left and the right and matching the  }
{solutions at the centre of the lattice.                              }
{The first part of the program deals with regular lattices, where all }
{wells are the same and spaced at regular intervals.  The second part }
{allows the user to change the width, depth or spacing of any of the  }
{wells to mimic an irregular lattice, or to allow an external electric}
{to be applied across the lattice.  In both parts the eigenvalues, the}
{eigenfunctions and their probability functions may be examined.      }
{Facility is provided for the user to incorporate their own procedures}
{to calculate wells, lattice  arrangements or external fields of their}
{own choosing.                                                        }
{                                                                     }
{Utilities: Crt, Graph, MUPPET, CUPS, CUPSfunc, CUPSproc, CUPSgui     }
{Library files: The parts of the code responsible for the SEARCH FOR  }
{               ZERO option is in a pre-compiled unit SEARCH.TPU      }
{Algorithms: Half-step integration and/or Numerov                                           }
{                                                                     }


PROGRAM LATCE1D;

{$F+}
{$N+}
{$M 32000,0,655360}

USES
   Crt,Graph,CUPSmupp,
   CUPS, CUPSgui, CUPSfunc, CUPSproc, CUPSgrph,
   Search;

CONST
   h    = 6.626E-34;       {J.s}
   hbar = 1.055E-34;       {J.s}
   e    = 1.602E-19;       {C}
   me   = 9.109E-31;       {kg}
   a0   = 5.292E-11;       {m}
   a1   = 1.000E-09;       {m  - unit of length on graphs}
   eps0 = 8.854E-12;       {F.m-1}
   energyConvert = 2*me*e*a1*a1/hbar/hbar;

   inpBackColor   : integer = blue;
   psiColor   : word = white;
   potlColor1 : word = lightblue;
   potlColor2 : word = lightgray;
   probColor1 : word = yellow;
   probColor2 : word = lightgreen;
   engyColor  : word = cyan;
   tailColor  : word = lightmagenta;
   dullColor  : word = lightgray;

   MUPbackColor : word = black;

TYPE
   wellParams = RECORD
                   wellType : Integer;
                   depth    : Real;
                   depth1   : Real;
                   width    : Real;
                   width1   : Real;
                   maxDepth : Real;
                END;
   cellParams = RECORD
                   well   : WellParams;
                   center : Real;
                   width  : Real;
                END;
   cellArray = ARRAY[1..12] OF cellParams;
   lattice = RECORD
                numCells : Integer;
                cells    : cellArray;
             END;

   ValuList = ARRAY [1..40] OF Real;
   eValList = RECORD
                 numLevels : Integer;
                 eigenVals : ValuList;
              END;
   HelpScreens = (progHS,cupsHS,
                  introHS,tryEnergyHS,tailsHS,sumProbsHS,
                  mainAHS,mainBHS);
   Str1       = String[1];

   FieldRecord = RECORD
                    identifier   : String;
                    fileWell     : WellParams;
                    fileLattice  : Lattice;
                    fileSpectrum : eValList;
                    fileFound    : Boolean;
                    fileVoltage  : Real;
                    fileModel    : Integer;
                 END;

   DiskFile = FILE OF FieldRecord;


CONST
   numDataMax = 399;
   lowestX = 0;
   defNumCells = 6;
   maxNumCells = 12;
   cellWidth = +0.1;      {nm}
   latticeOffset = +0.05; {nm}
   solnAmp: Real = 2;

   fileIdentifier = 'File for LATCE1D.PAS';
   first: Boolean = true;
   same: Boolean = true;

VAR
  {Global variables concerned with graphs}
   numData          : Integer;
   highestX,deltaX  : Real;
   highestV,lowestV : Real;
   X,V              : dVector;

  {Global Dvectors, used for many purposes}
   psi,prob,sumProb,temp: dVector;

  {Global variables, used to select part of the program}
   partCode    : Integer;
   quitFlag    : Boolean;

  {Global flags associated with the various choices}
   soundOn     : Boolean;
   regularFlag : Boolean;

  {Global variable associated with the user-defined procedures}
   userFlag  : Boolean;   {true if there are special user procedures}
   userItem1, userItem2 : String;  {for putting in the menus by user}
   userItem3            : String;

  {Global variable used in main part of program}
   cChoice,rChoice : Integer;
   energy          : Real;
   escape,finisht  : Boolean;
   mainMenu        : TMenu;

  {Global variables that may be saved to disk}
   thisWell        : WellParams;
   thisLattice     : Lattice;
   voltage         : Real;
   spectrum        : eValList;
   modelCode       : Integer;
   spectrumFound   : Boolean;
   currentFileName : String;

{*** NAMES OF USER DEFINED PROCEDURES ***}

PROCEDURE SetDefaultUser(VAR thisWell:wellParams);
   FORWARD;
PROCEDURE SetScreenLinesUser(thisWell:wellParams;
                            VAR thisScreen:TInputScreen);
   FORWARD;
PROCEDURE ReadParamsUser(VAR thisScreen:TInputScreen;
                            VAR thisWell:wellParams);
   FORWARD;
FUNCTION UserPotential(VAR thisWell:wellParams; x:Real): Real;
   FORWARD;
PROCEDURE SetLatticeUser(VAR thisLattice:Lattice);
   FORWARD;
PROCEDURE DoCalculationUser(VAR X,V,psi:DVector; thisSpectrum:eValList);
   FORWARD;



{ ------------------- HELP SCREENS --------------------- }

PROCEDURE SetUpProgHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := '';
      A[2]  := '      ELECTRON STATES IN A ONE DIMENSIONAL ';
      A[3]  := '                    LATTICE';
      A[4]  := '';
      A[5]  := '                       by';
      A[6]  := '';
      A[7]  := '                Ian D.Johnston';
      A[8]  := '        University of Sydney, Australia';
      A[9]  := '';
      A[10] := '                  version 1.00            ';
      A[11] := '        (c) 1995 John Wiley and Sons, Inc.';
      A[12] := '';
      A[13] := '';
      A[14] := '  This program solves the Schroedinger equation ';
      A[15] := '  for a finite one dimensional lattice consisting ';
      A[16] := '  of up to twelve simple potential wells.  ';
      A[17] := '  It is designed to show properties of energy';
      A[18] := '  bands, and the effect of impurities in the';
      A[19] := '  lattice.';
      A[20] := '';
      A[21] := '';
      A[22] := '';
      A[23] := '    ..... Press <Enter> or click mouse to resume.';
      A[24]  := '';
      A[25]  := '';
   END;

PROCEDURE SetUpIntroHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := '';
      A[2]  := '  This program allows you to explore energy,';
      A[3]  := '  eigenstates of an electron in a lattice';
      A[4]  := '  made up of a small number of potential wells.';
      A[5]  := '';
      A[6]  := '';
      A[7]  := '  You may explore a REGULAR LATTICE where the';
      A[8]  := '  number of wells is less than or equal to 10,';
      A[9]  := '  by solving the Schroedinger equation for an';
      A[10] := '  electron moving in the field.';
      A[11] := '';
      A[12] := '';
      A[13] := '  You may explore an IRREGULAR LATTICE in the ';
      A[14] := '  same way by introducing one of many different';
      A[15] := '  kinds of irregularity - by making some wells ';
      A[16] := '  wider or deeper than the rest, at different';
      A[17]  := ' spacings, or with an electric field.';
      A[18] := '';
      A[19] := '';
      A[20] := '';
      A[21] := '';
      A[22] := '';
      A[23] := '    ..... Press <Enter> or click mouse to resume.';
      A[24]  := '';
      A[25]  := '';
   END;

PROCEDURE SetUpTryEnergyHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := '';
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
      A[24] := '    ..... Press <Enter> or click mouse to resume.';
      A[25]  := '';
   END;

PROCEDURE SetUpTailsHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' ';
      A[2]  := '  The energy levels for a lattice are, in';
      A[3]  := '  general, very close together.  It is very  ';
      A[4]  := '  difficult to guess an energy that will';
      A[5]  := '  separate two eigenvalues.';
      A[6]  := ' ';
      A[7]  := '  This facility solves the Schroedinger equation';
      A[8]  := '  for a number of different energies within a ';
      A[9]  := '  RANGE which you specify.  For each solution ';
      A[10] := '  it notes the value of the solution at the';
      A[11] := '  extreme values of x.  If the energy being used';
      A[12] := '  is an eigenvalue, this extreme value will be ';
      A[13] := '  zero.  If not, it will be above or below the' ;
      A[14] := '  axis, depending on whether the energy is above';
      A[15] := '  or below the eigenvalue.';
      A[16] := '  The program plots the extreme values of the';
      A[17] := '  solution against EB, and you can see where the';
      A[18] := '  eigenvalues are, by seeing where this curve2';
      A[19] := '  crosses the x axis.';
      A[20] := ' ';
      A[21] := '  By selecting <F2-Range> you get to the input';
      A[22] := '  screen, which enables you to input values for';
      A[23] := '  the energy range and the step size.';
      A[24] := '';
      A[25] := '    ..... Press <Enter> or click mouse to resume.';
   END;

PROCEDURE SetUpSumProbsHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' ';
      A[2]  := '  An interesting property of energy bands is';
      A[3]  := '  that they can become "filled", which means';
      A[4]  := '  that if there is a different electron in';
      A[5]  := '  each state no electrical conduction will';
      A[6]  := '  result from those electrons.';
      A[7]  := '  ';
      A[8]  := '  This part of the program allows you to compute';
      A[9]  := '  the total probability of finding an electron';
      A[10] := '  somewhere, if more than one state has an ';
      A[11] := '  electron in it.  You can select a RANGE of ';
      A[12] := '  states by entering the quantum number for the';
      A[13] := '  first and last state in the range, and pressing' ;
      A[14] := '  <Enter>.  The probability distribution for each';
      A[15] := '  of those states in turn will be calculated, and';
      A[16] := '  drawn in the upper graph (in green).  When all';
      A[17] := '  have been calculated their SUM will appear on';
      A[18] := '  that same graph (in yellow).';
      A[19] := '  ';
      A[20] := '  The interesting feature to notice is that, for';
      A[21] := '  most lattices, if the range of levels chosen is';
      A[22] := '  one whole band, the total probability density';
      A[23] := '  is the same about each lattice site.';
      A[24] := '';
      A[25] := '    ..... Press <Enter> or click mouse to resume.';
   END;

PROCEDURE SetUpMainAHS(VAR A:HelpScrType);
   BEGIN
      A[2]  := ' WELLS.. NUMBER OF WELLS  Choose the number of';
      A[3]  := '                individual wells in the lattice.';
      A[4]  := '';
      A[5]  := '         SHAPE OF EACH WELL  Choose between ';
      A[6]  := '                 different shapes of the wells.';
      A[7]  := '';
      A[8]  := '         WELL PARAMETERS  Vary the parameters of';
      A[9]  := '                the wells already chosen.';
      A[10] := '';
      A[11] := '';
      A[12] := ' LATTICE.. REGULAR LATTICE  Revert to using a';
      A[13] := '                regular array of potential wells.';
      A[14] := '';
      A[15] := '         IRREGULAR LATTICE   Choose from a';
      A[16] := '                selection of irregularities.';
      A[17] := '';
      A[18] := '         ADJUST PARAMETERS  Vary parameters of ';
      A[19] := '                individual wells in the lattice.';
      A[20] := '';
      A[21]  := '         APPLY ELECTRIC FIELD  Add a linear';
      A[22]  := '                potential to the whole lattice.';
      A[23]  := '';
      A[24] := 'Press <Enter> or click mouse for the next screen.';
      A[25]  := '';
   END;

PROCEDURE SetUpMainBHS(VAR A:HelpScrType);
   BEGIN
      A[1]  := ' METHOD..TRY ENERGY (WITH MOUSE) ';
      A[2]  := '                    (FROM KEYBD)  Observe ';
      A[3]  := '                solution for various energies.';
      A[4]  := '';
      A[5]  := '         HUNT FOR ZERO   Use binary search for';
      A[6]  := '                solution with correct asymptote.';
      A[7] := '';
      A[8] := '         RANGE OF ENERGY  Calculate asymptotic';
      A[9] := '               behaviour for various energies.';
      A[10] := '';
      A[11] := '';
      A[12] := ' SPECTRUM.. FIND EIGENVALUES   Find all the';
      A[13] := '                eigenvalues for the lattice.';
      A[14] := '';
      A[15] := '            SEE WAVEFUNCTIONS   ';
      A[16] := '            SEE WFS AND PROBS   Display eigen-';
      A[17] := '                functions (and probabilities).';
      A[18] := '';
      A[19]  := '           PROBABILITIES  Display the sum of';
      A[20]  := '               probabilities for states.';
      A[21] := '';
      A[22] := '            SOUND  Toggle between on and off.';
      A[23]  := '';
      A[24] := '    ..... Press <Enter> or click mouse to resume.';
      A[25]  := '';
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
         introHS     : SetUpIntroHS(HS);
         tryEnergyHS : SetUpTryEnergyHS(HS);
         tailsHS     : SetUpTailsHS(HS);
         sumProbsHS  : SetUpSumProbsHS(HS);
         mainAHS     : SetUpMainAHS(HS);
         mainBHS     : SetUpMainBHS(HS);
         ELSE      OK := false;
      END;   {case}
      IF OK THEN
         Help(HS);
   END;

{ ------------ GENERAL PROCEDURES FOR POTENTIAL WELLS -------------- }

PROCEDURE SayError(thisStr:String; VAR flag:Boolean);
   BEGIN
      flag := true;
      Message(thisStr);
      Beep;
   END;

{-------------------- SQUARE WELL ------------------------}

PROCEDURE SetDefaultSquare(VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth    := 300;
            width    := 0.075;
            maxDepth := 400;
         END;
   END;

PROCEDURE SetScreenLinesSquare(thisWell:wellParams;
                               VAR thisScreen:TInputScreen);
   VAR
      thisMaxWidth : Real;
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('          Depth of each well ( <',
                            NumStr(thisWell.maxDepth,5,2),
                            ' ) = {      }eV'));
            SetNumber(2,thisWell.depth);
            SetNumberLimits(2, 0, thisWell.maxDepth);

            thisMaxWidth := cellWidth;
            LoadLine(concat('          Width of each well ( <',
                            NumStr(thisMaxWidth,5,2),
                            ' )  = {      }nm'));
            SetNumber(3,thisWell.width);
            SetNumberLimits(3,0,thisMaxWidth);
         END;    {with thisScreen}
   END;

PROCEDURE ReadParamsSquare(VAR thisScreen:TInputScreen;
                           VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth := thisScreen.getNumber(2);
            width := thisScreen.getNumber(3);
         END;
   END;

FUNCTION SquarePotntl(thisWell:wellParams; x:Real): Real;
   VAR
      V: Real;
   BEGIN
      V := 0;
      WITH thisWell DO
         BEGIN
            IF x>=-width/2 THEN V := -depth;
            IF x>width/2  THEN V := 0;
         END;
      SquarePotntl := V;
   END;

{-------------------- PARABOLIC WELL ------------------------}

PROCEDURE SetDefaultParabolic(VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth := 300;
            width := 0.11;
            maxDepth := 400;
         END;
   END;

PROCEDURE SetScreenLinesParabolic(thisWell:wellParams;
                               VAR thisScreen:TInputScreen);
   VAR
      thisMaxWidth : Real;
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('          Depth of each well ( <',
                            NumStr(thisWell.maxDepth,5,2),
                            ' ) = {      }eV'));
            SetNumber(2,thisWell.depth);
            SetNumberLimits(2, 0, thisWell.maxDepth);

            thisMaxWidth := 2*cellWidth;
            LoadLine(concat('          Width at the top   ( <',
                            NumStr(thisMaxWidth,5,2),
                            ' )  = {      }nm'));
            SetNumber(3,thisWell.width);
            SetNumberLimits(3,0,thisMaxWidth);
         END;    {with thisScreen}
   END;

PROCEDURE ReadParamsParabolic(VAR thisScreen:TInputScreen;
                              VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth := thisScreen.getNumber(2);
            width := thisScreen.getNumber(3);
         END;
   END;

FUNCTION ParabolicPotntl(thisWell:wellParams; x:Real): Real;
   VAR
      V: Real;
   BEGIN
      V := 0;
      WITH thisWell DO
         BEGIN
            IF x>=-width/2
               THEN V := depth*(sqr(2*x/width)-1);
            IF x>width/2 THEN V := 0;
         END;
      ParabolicPotntl := V;
   END;

{-------------------- COULOMBIC WELL ------------------------}

PROCEDURE SetDefaultCoulombic(VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth  := 150;
            depth1 :=  10;
            width  := 0.1;   {an arbitrary measure of width}
            maxDepth := 150;
         END;
   END;

PROCEDURE SetScreenLinesCoulombic(thisWell:wellParams;
                               VAR thisScreen:TInputScreen);
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('          Depth at the bottom ( <',
                            NumStr(thisWell.maxDepth,5,2),
                            ' ) = {      }eV'));
            SetNumber(2,thisWell.depth);
            SetNumberLimits(2, 0, thisWell.maxDepth);

            LoadLine(concat('          Depth at point (x=',
                            NumStr(thisWell.width,2,1),
                            'nm )       = {      }eV'));
            SetNumber(3,thisWell.depth1);
            SetNumberLimits(3,0,thisWell.maxDepth);
         END;    {with thisScreen}
   END;


PROCEDURE ReadParamsCoulombic(VAR thisScreen:TInputScreen;
                              VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth := thisScreen.getNumber(2);
            depth1:= thisScreen.getNumber(3);
         END;
   END;

FUNCTION CoulombicPotntl(thisWell:wellParams; x:Real): Real;
   VAR
      V: Real;
   BEGIN
      V := 0;
      WITH thisWell DO
         BEGIN
            IF abs(x)*depth>(depth1*width)
               THEN V := -width*depth1/abs(x)
               ELSE V := -depth;
         END;
      CoulombicPotntl := V;
   END;

{------------- GENERAL PROCEDURES FOR ALL WELLS ----------}

PROCEDURE SetDefaultWellParameters(thisWellType:Integer;
                                   VAR thisWell:wellParams);
   BEGIN
      thisWell.welltype := thisWellType;
      CASE thisWellType OF
         1 : SetDefaultSquare({setting} thisWell);
         2 : SetDefaultParabolic({setting} thisWell);
         3 : SetDefaultCoulombic({setting} thisWell);
         4 : SetDefaultUser({setting} thisWell);
      END;   {with case}
   END;

FUNCTION WellPotntl(thisWell:wellParams; x:Real): Real;
   BEGIN
     CASE thisWell.wellType OF
         1 : WellPotntl := SquarePotntl({using} thisWell,x);
         2 : WellPotntl := ParabolicPotntl({using} thisWell,x);
         3 : WellPotntl := CoulombicPotntl({using} thisWell,x);
         4 : WellPotntl := UserPotential({using} thisWell,x);
        END;   {with case}
   END;

PROCEDURE SetParticularScreenLines(thisWell:wellParams;
                                   VAR thisScreen:TInputScreen);
   BEGIN
      CASE thisWell.wellType OF
       1 : SetScreenLinesSquare(thisWell,thisScreen);
       2 : SetScreenLinesParabolic(thisWell,thisScreen);
       3 : SetScreenLinesCoulombic(thisWell,thisScreen);
       4 : SetScreenLinesUser(thisWell,thisScreen);
      END;    {case}
   END;

PROCEDURE ReadParticularParams(VAR thisScreen:TInputScreen;
                    VAR thisWell:wellParams);
   BEGIN
      CASE thisWell.wellType OF
       1 : ReadParamsSquare({using} thisScreen, {giving} thisWell);
       2 : ReadParamsParabolic({using} thisScreen, {giving} thisWell);
       3 : ReadParamsCoulombic({using} thisScreen, {giving} thisWell);
       4 : ReadParamsUser({using} thisScreen, {giving} thisWell);
      END;    {case}
   END;

{----------------- GENERAL UTILITIES -----------------------------}

PROCEDURE SwapReal(VAR a,b:Real);
   VAR
      c: Real;
   BEGIN
      c := a;  a := b;  b := c;
   END;

PROCEDURE SwapInteger(VAR a,b:Integer);
   VAR
      c: Integer;
   BEGIN
      c := a;  a := b;  b := c;
   END;

PROCEDURE SoundNote(E:Real; soundOn:Boolean);
   VAR
      f: Integer;
   BEGIN
      IF soundOn THen
         BEGIN
            f := Round(770*(-E-lowestV)/(highestV-lowestV) + 110);
            Sound(f);
            Delay(200);
            NoSound;
         END;
   END;

PROCEDURE OpenColoredViewport (Vnum: integer);
BEGIN
   SelectViewPort(vNum);
   HideCursor;
   SetFillStyle(SolidFill, MUPBackColor);
   WITH Views[vnum] DO bar(0,0,Vx2-Vx1,Vy2-Vy1);
   WITH Views[vnum] DO rectangle(0,0,Vx2-Vx1,Vy2-Vy1);
   ShowCursor;
END;

{ ------------------ MANAGEMENT PROCEDURES -------------------- }

PROCEDURE SetDefaultGlobalValues;
   VAR
      i: Integer;
   BEGIN
      numData  :=  (defNumCells+1) * (numDataMax div (defNumCells+1));
      highestX :=  lowestX + cellWidth*defNumCells + 2*latticeOffset;
      deltaX   := (highestX-lowestX)/numData;
      DefineScale(1, lowestX,highestX, -315,315);
      quitFlag    := false;
      soundOn     := true;
      regularFlag := true;
      first       :=  true;
      currentFileName := '';

      X.init(numdataMax+1);
      V.init(numDataMax+1);
      psi.init(numDataMax+1);
      prob.init(numDataMax+1);
      sumProb.init(numDataMax+1);
      temp.init(numDataMax+1);
      FOR i:=1 TO numData+1 DO X.Put(i,lowestX + (i-1)*deltaX);
   END;

PROCEDURE ChangeGlobalVariables(thisLattice:lattice);
   VAR
      i: Integer;
   BEGIN
      WITH thisLattice DO
         BEGIN
   {** NOTE:  The number of integration steps must be chosen so that **}
   {** the regularity of the lattice is not disrupted intentionally. **}
            numData  := (numCells+1) * (numDataMax div (numCells+1));
            highestX :=  cells[numCells].center + (cells[1].center - lowestX);
         END;
      deltaX   := (highestX-lowestX)/numData;
      FOR i:=1 TO numData+1 DO X.Put(i,lowestX + (i-1)*deltaX);
   END;

PROCEDURE SetScaleForV;
   VAR
      offsetV : Real;
   BEGIN
      offsetV := (highestV - lowestV)/10;
    { Note electric fields of 20% will be allowed }
      DefineScale(1, lowestX,highestX, lowestV-offsetV,highestV+offsetV);
   END;

PROCEDURE ZoomScaleForV(EB:Real);
   VAR
      offsetV, center    : Real;
      thisLowV,thisHighV : Real;
   BEGIN
      offsetV := (highestV - lowestV)/20;
      IF (-EB<=(highestV+3*lowestV)/4) THEN
         center := (highestV+3*lowestV)/4;
      IF (-EB>=(3*highestV+lowestV)/4) THEN
         center := (3*highestV+lowestV)/4;
      IF (-EB>(highestV+3*lowestV)/4) AND (-EB<(3*highestV+lowestV)/4) THEN
         center := -EB;
      thisLowV  := center - (highestV-lowestV)/4;
      thisHighV := center + (highestV-lowestV)/4;
      DefineScale(1, lowestX,highestX, thisLowV-offsetV,thisHighV+offsetV);
   END;

{ --------------- PROCEDURES FOR PLOTTING DETAILS ----------------- }

PROCEDURE SetPlottingDetails;
   BEGIN
      DefineViewport(1, 0.10,0.90, 0.50,0.90);  {for potential well}
      DefineViewport(2, 0.10,0.90, 0.20,0.90);  {for well + functions}
      DefineViewport(3, 0.10,0.90, 0.15,0.45);  {for the hunting process}
      DefineViewport(4, 0.10,0.90, 0.18,0.55);  {for the energy range}
      DefineViewport(5, 0.00,1.00, 0.00,0.56);  {masks viewport 4}
      DefineViewport(6, 0.10,0.90, 0.60,0.90);  {for sum of probabilities}
      DefineViewport(7, 0.05,0.95, 0.07,0.12);  {for message re energy range}
   END;

PROCEDURE ColoredAutoScale(sNum:Integer; VAR x,y:Datavector;
                           numdata:Integer; thisColor:Integer);
VAR
   ticX,ticY : Real;
BEGIN
   AutoScale(sNum,x,y,numData);
   WITH GraphScales[sNum] DO
      BEGIN
         ticX := TickSpace(xMax - xMin);
         ticY := TickSpace(yMax - yMin);
      END;
   DrawAxis(0,0,ticX,ticY,thisColor);
END;

PROCEDURE ShowEnergyLevels(VAR spectrum:eValList);
   VAR
      i:Integer;
   BEGIN
      SelectViewport(4);
      SelectScale(1);
      SetColor(engyColor);
      WITH spectrum DO
         FOR i:=1 TO numLevels DO
            PlotLine(lowestX,-eigenvals[i], highestX,-eigenVals[i]);
   END;

{ ------------ GENERAL PROCEDURES FOR WAVE EQUATION -------------- }

PROCEDURE SolveWaveEquation(EB:real; VAR X,V,psi:dVector);
  {This procedure uses the Numerov algorithm as in CUPSmath.tpu.}
   VAR
      psi0,k,dydx: real;
      i: integer;
      A: Dvector;
   BEGIN
      psi0 := 200;           {sets arbitrary amplitude of the solution}
    { Initial conditions of integration: psi must be an increasing    }
    { exponential for values of X to the left of the potential well.  }
      k := sqrt(abs(energyConvert*EB));  {wave number outside the well}
      psi.Put(1,psi0 * exp(k*X.value(1)));
      psi.Put(2,psi0 * exp(k*X.value(2)));
    { Setting up for the numerov method of integration }
      A.init(NumDataMax+1);
      FOR i:=1 TO numData+1 DO
         A.Put(i,energyConvert*(EB+V.value(i))*deltaX*deltaX/12.0);
      Numerov(1,numData+1,deltaX,A,psi);
      A.Free;
   END;

PROCEDURE SolveTwoHalfEquations(EB:Real; VAR X,V,psi:dVector;
                                VAR mismatch,coeff1,coeff2: Real);
 { This uses the numerov method. }
   VAR
      psi0,k,ksq,a1     : Real;
      midVal1,midSlope1 : Real;
      midVal2,midSlope2 : Real;
      amplt1,amplt2     : Real;
      i,n               : Integer;
      A                 : DVector;
   BEGIN
      psi0 := 1;                      {sets amplitude of the solution}
      n := numData div 2;
    { Setting up boundary conditions outside the well}
      k := sqrt(abs(energyConvert*EB));

    { Setting up for the numerov method of integration }
      A.init(NumDataMax+1);
      FOR i:=1 TO numData+1 DO
         A.Put(i,energyConvert*(EB+V.value(i))*deltaX*deltaX/12.0);

    { Initial conditions of first half of integration: psi is increasing }
    { exponential for values of X to the left of the potential well.     }
      psi.Put(1,psi0 * exp(k*X.value(1)));
      psi.Put(2,psi0 * exp(k*X.value(2)));
      Numerov(1,n+3,deltaX,A,psi);
      midVal1   := k*psi.VecPtr^[n+1];
      midSlope1 := NumerovDeriv(n+1,deltaX,A,psi);

    { Initial conditions of second half of integration: psi is decreasing }
    { exponential for values of X to the right of the potential well.     }
    { They are the same (to  + or -1) as first two values on left side.   }
      psi.Put(numData+1,psi0 * exp(k*X.value(1)));
      psi.Put(numData  ,psi0 * exp(k*X.value(2)));
      Numerov(numData+1,n-1,deltaX,A,psi);
      midVal2   := k*psi.VecPtr^[n+1];
      midSlope2 := NumerovDeriv(n+1,deltaX,A,psi);
      A.Free;

    { Setting up for boundary matching at halfway point }
      ksq := abs(energyConvert*(EB+V.value(n+1)));
      amplt1 := sqrt(ksq*midVal1*midVal1 + midSlope1*midSlope1);
      amplt2 := sqrt(ksq*midVal2*midVal2 + midSlope2*midSlope2);
      misMatch := (midVal1*midSlope2 - midVal2*midSlope1)/(amplt1*amplt2);
      IF (abs(midVal1)>abs(midSlope1)) THEN
         BEGIN
            coeff1 := midVal2;
            coeff2 := midVal1;
         END
      ELSE
          BEGIN
             coeff1 := midSlope2;
             coeff2 := midSlope1;
          END;
   END;

PROCEDURE FitHalvesTogether(coeff1,coeff2:Real; VAR psi:DVector);
   VAR
      n,i : Integer;
   BEGIN
      n := numData div 2;
      FOR i:=1 TO n-2 DO
         psi.Put(i,psi.VecPtr^[i]*coeff1);
      FOR i:=n-1 TO numData+1 DO
         psi.Put(i,psi.VecPtr^[i]*coeff2);
   END;

FUNCTION EndCorrection(y1,y2:Real):Real;
  {** If the integrand (y) is of the form: A exp(kx) for **}
  {** values of x less than x1, then the integral of y   **}
  {** from -infinity to x1 is equal to:                  **}
  {**   y1/k  or equivalently,  y1*deltaX/ln(y2/y1)      **}
   VAR
      temp,temp1: Real;
   BEGIN
      temp := 0;
      IF y1<>0 THEN
         IF (y2/y1)>0 THEN temp1 := ln(y2/y1);
         IF temp1>0   THEN temp := y1*deltaX/temp1;
     {Note: if anything wrong, correction is left as zero }
      endCorrection := temp;
   END;

PROCEDURE NormalizeTheWaveFunction(VAR psi:dVector);
   VAR
      normFactor: Real;
      area: Real;
      temp1,temp2: Real;
      i: Integer;
   BEGIN
      area := endCorrection(psi.value(1)*psi.value(1),
                            psi.value(2)*psi.value(2));
      FOR i:=2 TO numdata+1 DO
         BEGIN
            temp1 := psi.value(i)*psi.value(i);
            temp2 := psi.value(i-1)*psi.value(i-1);
            area := area + (temp1+temp2)*deltaX/2;
         END;
      area := area + endCorrection(psi.value(numdata+1)*psi.value(numdata+1),
                                   psi.value(numdata)*psi.value(numdata));
      IF area>macheps
         THEN normfactor := 1/sqrt(area);
      psi.MultScalar(normFactor);
   END;

PROCEDURE CalculateProbabilityFunction(VAR psi,prob:Dvector);
   VAR
      temp : Real;
      i    : Integer;
   BEGIN
            FOR i:=1 TO numdata+1 DO
               BEGIN
                  temp := psi.value(i)*psi.value(i);
                  prob.Put(i,temp);
               END;
   END;


{ ------------ GENERAL PROCEDURES FOR LATTICES -------------- }

PROCEDURE ConstructRegularLattice(n:Integer; thisWell:WellParams;
                                  VAR thisLattice:Lattice;
                                  VAR spectrum:eValList);
   VAR
      i: Integer;
   BEGIN
      thisLattice.numCells := n;
      WITH thisLattice DO
         BEGIN
            FOR i:=1 TO numCells DO
               BEGIN
                  cells[i].well   := thisWell;
                  cells[i].width  := cellWidth;
                  cells[i].center := latticeOffset + lowestX + (i-0.5)*cells[i].width;
               END;
         END;    {with}
      spectrum.numLevels := 0;  {spectrum still has to be calculated}
   END;

VAR x0,V0 : ARRAY[1..500] OF Real;

PROCEDURE CalculateLatticeDVector(VAR thisLattice:Lattice;
                                  VAR V:dVector;
                                  VAR spectrumFound:Boolean);
   VAR
      i,j: Integer;
     x1,dV : Real;
   BEGIN
      FOR i:=1 TO numData+1 DO
         V.Put(i,0);

      WITH thisLattice DO
         BEGIN
            FOR j:=1 TO numCells DO
               BEGIN
                  FOR i:=1 TO numData+1 DO
                     BEGIN
                        x1 := X.value(i) - cells[j].center;
                        dV := WellPotntl(cells[j].well,x1);
                        V.Put(i,V.value(i)+dV);
                     END;          {for i}
               END;                {for j}
         END;                      {with}

      V.Put(1,0);
      V.Put(numdata+1,0);
      lowestV  := 0;
      highestV := 0;
      FOR i:=1 TO numData+1 DO
         BEGIN
           IF V.value(i)<lowestV  THEN lowestV  := V.value(i);
            IF V.value(i)>highestV THEN highestV := V.value(i);
         END;
      spectrumFound := false;

      FOR i:=1 TO numData+1 DO
         BEGIN
            X0[i] := X.value(i);
            V0[i] := V.value(i);
         END;
   END;


{------- PROCEDURES CONCERNING SOLVING SCHROEDINGER EQN -------}

PROCEDURE DrawThePotentialWellAsBackground(VAR X,V:Dvector);
   BEGIN
      ClearMUPPETport;
      graphBackColor := black;
      SetColor(dullColor);
      OpenColoredViewport(2);
      SelectScale(1);
      DrawAxis(0,0, 0.1,50, dullColor);
      PutLabel(right,'energy (eV)');
      PutLabel(bottom,'distance (nm)');
      SetColor(potlColor1);
      PlotDVectors(X,V,1,numData+1);
   END;

PROCEDURE ClearThePotentialPlot(VAR X,V:Dvector);
   BEGIN
      SetColor(dullColor);
      OpenColoredViewport(2);
      SelectScale(1);
      DrawAxis(0,0, 0.1,50, dullColor);
      SetColor(potlColor1);
      PlotDVectors(X,V,1,numData+1);
   END;

PROCEDURE PrepareToFindWaveFunctions(VAR X,V:dVector;
                                     VAR EB:Real);
   BEGIN
      DrawThePotentialWellAsBackground(X,V);
      EB := -0.8*lowestV;    {default value of the binding energy}
   END;

PROCEDURE ChooseMeansOfInput(rowNum:Integer; VAR withMouse:Boolean);
   BEGIN
      IF rowNum=1 THEN withMouse := true
                  ELSE withMouse := false;
   END;

PROCEDURE LocalMousePosn(xGlobal,yGlobal:Real; VAR X,Y:REAL;
                         VNum,SNum:integer; VAR Inside:boolean);
   VAR
      XFactor,YFactor: REAL;
   BEGIN
      WITH Views[VNum] DO
           WITH GraphScales[SNum] DO
                BEGIN xFactor := (xmax-xmin)/(Vx2-Vx1);
                      X := (xFactor*(xGlobal-Vx1)) + xmin;
                      yFactor := (ymax-ymin)/(Vy2-Vy1);
                      Y := -(yFactor*(yGlobal-Vy1)) + ymax;
                      Inside := false;
                      IF (xGlobal>=Vx1) AND (xGlobal<=Vx2)
                         AND (yGlobal>=Vy1) AND (yGlobal<=Vy2)
                         THEN Inside := true;
                END;
END;

PROCEDURE InputEnergyMouse(VAR EB:Real;
                           VAR erase,finisht,zoom:Boolean);
   VAR
      x,y        : Real;
      inside     : Boolean;
      theseKeys  : ThotKeys;
      kNum       : Byte;
      tempColor  : Word;

   PROCEDURE WriteInstruction(thisColor:Integer);
      BEGIN
         SetColor(MUPbackColor);
         OpenColoredViewport(7);
         SetColor(thisColor);
         Print(16,1,'Click within the graph to choose an energy.');
      END;

   BEGIN
      WITH theseKeys DO
         BEGIN
            Init(4);
            key[1] := 'F1-Help';
            key[2] := 'F2-Clear';
            IF zoom THEN
               key[3] := 'F3-Zoom Out'
            ELSE
               key[3] := 'F3-Zoom In';
            key[4] := 'F10-Menu';
         END;   {with}

      WriteInstruction(white);
      finisht := false;
      erase := false;
      theseKeys.Display;
      SelectViewport(2);
      SelectScale(1);
      CheckForEvents;
      REPEAT
         DynamicPause;
         CheckForEvents;
         inside := false;
         IF theseKeys.Pressed(knum) THEN
            BEGIN
               IF kNum=1 THEN DisplayHelpScreen(tryEnergyHS);
               IF kNum=2 THEN erase := true;
               IF kNum=3 THEN
                  BEGIN
                     ClearMUPPETport;
                     zoom := NOT zoom;
                     erase := true;
                     IF zoom THEN
                        ZoomScaleForV(EB)
                     ELSE
                        SetScaleForV;
                  END;
               IF kNum=4 THEN finisht := true;
            END
         ELSE
            BEGIN
               LocalMouseposn(event.X,event.Y,x,y,2,1,inside);
               IF inside THEN EB := -y;
            END;
      UNTIL inside OR erase OR finisht;
      WriteInstruction(graphbackColor);
   END;

PROCEDURE ClearUpTheDisplay;
   VAR
      dummyKeys : Thotkeys;
   BEGIN
      ClearMKbuffers;
      dummyKeys.Clear;
   END;

PROCEDURE InputEnergyKeybd(VAR EB:Real; VAR erase,finished:Boolean);
   VAR
      thisScreen: TInputScreen;
      thisHS    : HelpScrType;
   BEGIN
      WITH thisScreen DO
         BEGIN
            init;
            DefineInputPort(0.05,0.95, 0.05,0.10);
            LoadLine(concat('Choose EB (+ve) {      }eV',
                            ':   [ Help ]  [Clear ]  [  Ok  ]  [Cancel]'));
            SetNumber(1,EB);
            SetNumberLimits(1, -graphScales[1].Ymax,-graphScales[1].Ymin);
            SetUpTryEnergyHS(thisHS);
            SetHelpScreen(thisHS);
            finished := false;
            erase := false;
            SetColor(white);
            AcceptScreen;
            IF Canceled
               THEN finished := true
            ELSE
               IF GetBoolean(3)
                  THEN erase := true
                  ELSE EB := thisScreen.getNumber(1);
            done;
         END;   {with thisScreen}
   END;

PROCEDURE ShowTheEnergyLevel(EB:Real);
   BEGIN
      SelectViewport(2);
      SelectScale(1);
      SetColor(engyColor);
      PlotLine(lowestX,-EB, highestX,-EB);
   END;

PROCEDURE DrawWaveFunctionOnEnergyLevel(EB:Real; VAR X,psi:dVector);
   VAR
      i: Integer;
      scaleFactor: Real;
   BEGIN
      scaleFactor := 0.08*abs(lowestV)*sqrt(highestX);
      FOR i:=1 TO numData+1 DO
         temp.Put(i,-EB + scaleFactor*psi.value(i));
         SelectViewport(2);
         SelectScale(1);
         PlotDVectors(X,temp,1,numData+1);
   END;

PROCEDURE DisplayProbabilityFunction(spectrum:eValList; n:Integer;
                               VAR X,prob:Dvector);
   VAR
      EB : Real;
      i  : Integer;
      scaleFactor : Real;
   BEGIN
      scaleFactor := 0.04*abs(lowestV)*highestX;
      EB := spectrum.eigenVals[n];
      SetColor(probColor1);
      FOR i:=1 TO numData+1 DO
         temp.Put(i,-EB + scaleFactor*prob.value(i));
      SelectViewport(2);
      SelectScale(1);
      PlotDVectors(X,temp,1,numData+1);
   END;

PROCEDURE PrepareToHunt;
   BEGIN
      DefineScale(2, lowestX,highestX, -1.5,+1.5);
      DefineScale(3, lowestX,highestX, -1.0,+1.0);
      Message('');
   END;

PROCEDURE DisplayIntermediateSolution(VAR psi:DVector);
   BEGIN
      SetColor(dullColor);
      IF ViewportNumber=3
         THEN SelectViewport(3)
         ELSE OpenColoredViewport(3);
      SelectScale(3);
      PlotLine(lowestX,0, highestX,0);
      SetColor(psiColor);
      PlotDVectors(X,psi,1,numData+1);
   END;

PROCEDURE DisplayResultOfSearch(EB:Real; VAR X,psi:Dvector);
   BEGIN
      DrawThePotentialWellAsBackground(X,V);
      ShowTheEnergyLevel(EB);
      SetColor(psiColor);
      DrawWaveFunctionOnEnergyLevel(EB,X,psi);
      PutLabel(inside,concat('Binding energy = ',
                             NumStr(EB,7,2)));
   END;

FUNCTION NumberOfNodes(EB:Real; VAR psi:Dvector):Integer;
   VAR
      i,n: Integer;
      k: Real;
   BEGIN
      n := 0;
      FOR i:=2 TO numData+1 DO
       { When psi changes sign this signifies a node }
         IF ((psi.value(i-1)>=0) AND (psi.value(i)<0))
            OR ((psi.value(i-1)<0) AND (psi.value(i)>=0))
             THEN n := n+1;
       { But there might also be one beyond x = highestX }
         k := sqrt(abs(energyConvert*EB));  { wave number outside the well }
         IF abs(psi.value(numData))>abs(exp(k*deltaX)*psi.value(numData+1))
            THEN n := n + 1;
      numberOfNodes := n;
   END;

FUNCTION Yvalue(Xvalue:Real; VAR nValue:Integer):Real;
 { This is the function for the Search procedure }
   VAR
      energy,k: Real;
   BEGIN
      energy := Xvalue;
      SolveWaveEquation(energy,X,V,psi);
      NormalizeTheWaveFunction(psi);
      DisplayIntermediateSolution(psi);
      nValue := NumberOfNodes(energy,psi);
      k := sqrt(abs(energyConvert*energy));  { wave number outside the well }
      Yvalue := psi.value(numdata) - exp(k*deltaX)*psi.value(numData+1);
    { This means the last two points are in the same ratio as the first two }
   END;

FUNCTION FastYvalue(Xvalue:Real):Real;
 { This is the function for the FastSearch procedure }
   VAR
      energy,k      : Real;
      mismatch,coeff1,coeff2 : Real;
   BEGIN
      energy := Xvalue;
      SolveTwoHalfEquations(energy,X,V,psi,mismatch,coeff1,coeff2);
      FastYvalue := mismatch;
   END;

PROCEDURE DisplayEnergyAndNum(EB:Real; VAR psi:dVector);
   VAR
      n : Integer;
   BEGIN
      n := NumberOfNodes(EB,psi);
      Message(concat('  EB := ',
                     NumStr(EB,5,2)
                     ,'  :  number of nodes = ',
                     NumStr(n,2,0)));
   END;

PROCEDURE FindNumberOfEigenvalues(VAR X,V,psi:Dvector; VAR spectrum:eValList);
   VAR
      EB: Real;
      n : Integer;
   BEGIN
      EB := 0;
      SolveWaveEquation({using} EB,X,V, {giving} psi);
      spectrum.numLevels := NumberOfNodes({using} EB,psi);
    { It is assumed here that number of eigenvalues is one greater }
    { then the number of nodes for the highest energy level, which }
    { is one fewer than the number for the  solution with E = 0.   }
      Message(concat('Number of levels = ',NumStr(spectrum.numLevels,2,0)));
   END;

PROCEDURE FindLevelRecursively(firstEnergy,secondEnergy:Real;
                               secondNumNodes:Integer;
                               soundOn:Boolean;
                               VAR X,V,psi:Dvector;
                               VAR spectrum:eValList);
   VAR
      firstNumNodes,n: Integer;
      newEnergy,eigenEnergy: Real;
      i,j: Integer;
   BEGIN
      SolveWaveEquation({using} firstEnergy,X,V, {giving} psi);
      firstNumNodes := NumberOfNodes({using} firstEnergy,psi);

      CASE abs(firstNumNodes-secondNumNodes) OF
       0 : {no eigenvalues in this range}
           EXIT;
       1 : {one eigenvalue in here, go and find it}
           BEGIN
              eigenEnergy := (firstEnergy+secondEnergy)/2;
              FuncFastYValue := FastYValue;
              FastSearchForZero(firstEnergy,secondEnergy,eigenEnergy);
              ShowTheEnergyLevel(eigenEnergy);
              IF firstNumNodes<secondNumNodes
                 THEN n := secondNumNodes
                 ELSE n := firstNumNodes;
              Message(concat('Level number ',
                             Numstr(n,2,0),
                             ':   Binding energy = ',
                             Numstr(eigenEnergy,7,2)));
              spectrum.eigenVals[n] := eigenEnergy;
              SoundNote(eigenEnergy,soundOn);
              EXIT;
           END;
       ELSE {more than one eigenvalue here, subdivide further};
           BEGIN
              newEnergy := (firstEnergy+secondEnergy)/2;
              IF abs(newEnergy-firstEnergy)<=(1.0E06 * macheps) THEN
               { levels are degenerate within accuracy of the computer}
                 BEGIN
                    IF firstNumNodes<secondNumNodes
                       THEN
                          BEGIN
                             i := firstNumNodes;
                             j := secondNumNodes;
                          END
                       ELSE
                          BEGIN
                             i := secondNumNodes;
                             j := firstNumNodes;
                          END;
                    FOR n:=i+1 TO j DO
                       BEGIN
                          Message(concat('Level number ',
                                         Numstr(n,2,0),
                                         ':   Binding energy = ',
                                         Numstr(newEnergy,7,2)));
                          spectrum.eigenVals[n] := newEnergy;
                          SoundNote(newEnergy,soundOn);
                       END;
                    EXIT
                 END;     {if energy difference}
              FindLevelRecursively(newEnergy,firstEnergy,firstNumNodes,
                                   soundOn,X,V,psi, {adding to} spectrum);
              FindLevelRecursively(newEnergy,secondEnergy,secondNumNodes,
                                   soundOn,X,V,psi, {adding to} spectrum);
           END;      {else}
      END;    {case}
   END;

PROCEDURE FindAllEigenvalues(VAR X,V,psi:Dvector;
                             VAR spectrum:eValList;
                             VAR spectrumFound:Boolean);
   VAR
      lowEnergy,highEnergy: Real;
      lowNumNodes,highNumNodes: Integer;
   BEGIN
      highEnergy := abs(lowestV);
      highNumNodes := 0;
      lowEnergy  := 0.001;
    { Note: cannot use EB = 0 else solution is zero everywhere}
      FindLevelRecursively({using} lowEnergy,highEnergy,highNumNodes,
                           {using} soundOn,X,V, {making} psi, {completing} spectrum);
      spectrumFound := true;
      Message('');
   END;

PROCEDURE PrepareToDisplayWaveFunctions(VAR n:integer);
   BEGIN
      DrawThePotentialWellAsBackground(X,V);
      n := 1;
   END;

PROCEDURE LabelTheGraph(probFlag:Boolean);
   BEGIN
      SelectViewport(2);
      SetColor(psiColor);
      IF probFlag THEN
         BEGIN
            PutLabel(top,'wave function                          ');
            SetColor(probColor1);
            PutLabel(top,'                    probability density');
         END
      ELSE
         PutLabel(top,'wave function');
   END;

PROCEDURE InputALevelNumber(VAR spectrum:eValList;
                            VAR n:integer; VAR finisht:boolean);
   VAR
      thisScreen: TInputScreen;
      numFound: boolean;
   BEGIN
      WITH thisScreen DO
         BEGIN
            init;
            DefineInputPort(0.05,0.95, 0.05,0.10);
            IF spectrum.numLevels<1 THEN
               BEGIN
                  beep;
                  LoadLine('You have not calculated the spectrum yet.  Better do it now.   [  Ok  ]');
                  Loadline('');    {dummy lines, don't actually appear}
                  LoadLine('[Cancel]');
                  AcceptScreen;
                  finisht := true;
               END
            ELSE
               BEGIN
                  LoadLine(concat(' Choose a level number (<=',
                                  numStr(spectrum.numLevels,2,0),
                                  ')  {   }',
                                  ':   [clear ]   [  Ok  ]   [Cancel]'));
                  SetNumber(1,n);
                  SetNumberLimits(1, 1,spectrum.numLevels);
                  REPEAT
                     AcceptScreen;
                     finisht := false;
                     numFound := false;
                     IF GetBoolean(2) THEN
                        PrepareToDisplayWaveFunctions({setting} n);
                     IF GetBoolean(3) THEN
                        BEGIN
                           n := round(thisScreen.getNumber(1));
                           IF n<=spectrum.numLevels
                              THEN numFound := true;
                        END;
                     IF GetBoolean(4)
                        THEN finisht := true;
                  UNTIL numFound OR finisht;
               END;     {if numlevels}
         END;   {with thisScreen}
   END;

PROCEDURE DisplayEigenFunction(spectrum:eValList; n:Integer;
                               VAR X,V,psi,prob:Dvector);
   VAR
      energy,temp   : Real;
      i             : Integer;
      mismatch      : Real;
      coeff1,coeff2 : Real;
   BEGIN
      WITH spectrum DO
         BEGIN
            IF n>numLevels THEN EXIT;
            energy := eigenVals[n];
            ShowTheEnergyLevel({using} energy);
            SolveTwoHalfEquations({using} energy,X,V,
                                        {giving} psi,mismatch,coeff1,coeff2);
            FitHalvesTogether({using} coeff1,coeff2, {changing} psi);
            NormalizeTheWaveFunction({using} psi);
            CalculateProbabilityFunction({using} psi, {giving} prob);
            SetColor(psiColor);
            DrawWaveFunctionOnEnergyLevel({using} energy,X,psi);
            Message(concat('Level number ',
                           Numstr(n,2,0),
                           ':   Binding energy = ',
                           Numstr(energy,7,2)));
         END;    {with}
   END;


{------------- PROCEDURES SHOWING METHODS OF SOLUTION ------------}

PROCEDURE StartToPlotAsymptotes(maxY,Emax,Emin: Real);
   VAR
      Xscale,Yscale: dataVector;
   BEGIN
      SetColor(MUPbackColor);
      OpenColoredViewport(5);
      SetColor(dullColor);
      OpenColoredViewport(4);
      Xscale[1] := Emin;
      Xscale[2] := Emax;
      Yscale[1] := 1.2 * abs(maxY);
      Yscale[2] := - Yscale[1];
      ColoredAutoScale(4,Xscale,Yscale,2,dullColor);
      PutLabel(bottom,'EB (eV)');
      SetColor(tailColor);
      PutLabel(inside,'coefficient of diverging exp at large x');
   END;


PROCEDURE IntegrateAndPlotAsymptotes(Emax,Emin,dE:Real; n:Integer;
                                     VAR X,V,tails:Dvector);
   VAR
      lastE,lastY,EB,Y,k: Real;
      i: Integer;
      misMatch,mismatch0: Real;
   BEGIN
      if n>numDataMax THEN
         n := numDataMax;       {number of tails can't be too big}
      lastE := Emax;
      EB := Emax;
      SolveWaveEquation({using} EB,X,V, {giving} psi);
      NormalizeTheWaveFunction({changing} psi);
      k := sqrt(abs(energyConvert*EB));  { wave number outside the well }
      lastY := psi.value(numdata) - exp(k*deltaX)*psi.value(numData+1);
      StartToPlotAsymptotes(lastY,Emax,Emin);
      FOR i:=1 TO n+1 DO
         BEGIN
            EB := Emax - (i-1)*dE;
            SolveWaveEquation({using} EB,X,V, {giving} psi);
            NormalizeTheWaveFunction({changing} psi);
            k := sqrt(abs(energyConvert*EB));  { wave number outside the well }
            Y := psi.value(numdata) - exp(k*deltaX)*psi.value(numData+1);
            tails.Put(i,Y);
            IF errorFound=true THEN
               i:= n+2;    {exit from the loop}
            PlotLine(lastE,lastY, EB,Y);
            lastE := EB;
            lastY := Y;
         END;
   END;

PROCEDURE ReadEnergiesFromPlot(VAR again:Boolean);
   VAR
      thisScreen     : TInputScreen;
      x,y,EB         : Real;
      inside,finisht : Boolean;
      theseKeys      : ThotKeys;
      kNum           : Byte;
      tempColor      : Word;
   BEGIN
      WITH theseKeys DO
         BEGIN
            Init(3);
            key[1] := 'F1-Help';
            key[2] := 'F2-Range';
            key[3] := 'F10-Menu';
         END;   {with}

      SetColor(MUPbackColor);
      OpenColoredViewport(7);
      SetColor(lightmagenta);
      Print(1,1,
        'Click within the graph to measure EB.  Select <F2> for different range.');

      SelectViewport(10);           {These erase bits of the input screen}
      SetColor(blue);
      Print(3,4,'Please specify:');
      SetFillStyle(SolidFill, blue);
      HideMouse;
      Bar(10,159,  200,132);
      Rectangle(10,159,  200,132);
      ShowMouse;

      finisht := false;
      again   := false;
      theseKeys.Display;
      SelectViewport(4);
      SelectScale(4);
      CheckForEvents;
      REPEAT
         DynamicPause;
         CheckForEvents;
         IF theseKeys.Pressed(knum) THEN
            BEGIN
               IF kNum=1 THEN DisplayHelpScreen(TailsHS);
               IF kNum=2 THEN again := true;
               IF kNum=3 THEN finisht := true;
            END
         ELSE
            BEGIN
               LocalMouseposn(event.X,event.Y,x,y,4,4,inside);
               IF inside
                  THEN Message(concat('  Binding energy = ',
                                      NumStr(x,7,2),
                                      ' eV'));
            END;
      UNTIL again OR finisht;
      theseKeys.clear;
      SetColor(MUPbackColor);
      OpenColoredViewport(7);
      ClearViewport;
   END;

PROCEDURE ExploreARangeOfEnergies(VAR X,V:DVector);
   VAR
      rangeScreen  : TinputScreen;
      Emax,Emin,dE : Real;
      again,OK     : Boolean;
      numE,i       : Integer;
      minV,temp    : Real;
   BEGIN
      WITH rangeScreen DO
         BEGIN
            Init;
            DefineInputport(0.1,0.9, 0.60,0.94);
            DefineViewport(10,0.1,0.9, 0.60,0.94); {used to hide inputport}
            minV := int(abs(lowestV));
            LoadLine(' For various values of binding energy the wave function ');
            LoadLine(' will be integrated.  The coefficient of the diverging');
            LoadLine(' exponential part of psi at large x will be plotted.    ');
            LoadLine(' Please specify:                            ');
            LoadLine(concat('        Lowest binding energy (0<=EB<=',
                            NumStr(minV,3,0),
                            ') =  {        }eV  '));
            LoadLine(concat('       Highest binding energy (0<=EB<=',
                            NumStr(minV,3,0),
                            ') =  {        }eV  '));
            LoadLine(concat('                          Step size =  ',
                            '{       }eV  '));
            LoadLine(' [  Ok  ]    [Cancel]');
            SetNumber(1,0);
            SetNumber(2,minV);
            SetNumber(3,abs(minV/100));
            SetNumberLimits(1, 0,minV);
            SetNumberLimits(2, 0,minV);
            SetNumberLimits(3, 0.00001,minV);

            REPEAT
               IF viewportNumber<>4 THEN ClearMUPPETport;
               OK := false;
               REPEAT
                  Accept;
                  IF Canceled
                     THEN escape := true
                  ELSE
                     BEGIN
                        escape := false;
                        Emax := getNumber(1);
                        Emin := getNumber(2);
                        IF Emin>Emax THEN SwapReal(Emin,Emax);
                        dE   := getNumber(3);
                        numE := Trunc((Emax-Emin)/dE);
                        IF (Emax>=0) AND (Emax<=abs(lowestV))
                                     AND (Emin>=0) AND (Emin<=abs(lowestV))
                                     AND (Emax<>Emin)
                                     AND (dE<=(Emax-Emin))
                           THEN OK := true
                        ELSE
                           BEGIN
                              Message('Not acceptable values.  Please try again. <esc> will abort.');
                              beep;
                           END;
                     END;
            UNTIL escape OR OK;
            Message('');

               IF (NOT finisht) AND (NOT escape) THEN
                  BEGIN
                     IntegrateAndPlotAsymptotes({using} Emax,Emin,dE,numE,
                                                 X,V, {giving} prob);
                     ReadEnergiesFromPlot({giving} again);
                  END;
            UNTIL (NOT again) OR finisht OR escape;
            done;
         END;     {with rangeScreen}
   END;

PROCEDURE ExploreMethodOfSolution(code:Integer; energy:Real;
                                  VAR X,V:Dvector);
   VAR
      finisht,erase   : Boolean;
      zoom            : Boolean;
      misMatch,coeff  : Real;
      withMouse       : Boolean;
   BEGIN
      zoom := false;
      Message('');
      CASE code OF
         1..2: {Solve for a particular energy }
            BEGIN
               PrepareToFindWaveFunctions({using} X,V, {setting} energy);
               ChooseMeansOfInput({using} code, {giving} withMouse);
               REPEAT
                  IF withMouse
                     THEN InputEnergyMouse
                                     ({giving} energy,erase,finisht,zoom)
                     ELSE InputEnergyKeybd({giving} energy,erase,finisht);
                  IF NOT finisht THEN
                     IF NOT erase THEN
                        BEGIN
                           ShowTheEnergyLevel({using} energy);
                           SolveWaveEquation({using} energy,X,V,
                                           {giving} psi);
                           NormalizeTheWaveFunction({changing} psi);
                           DisplayEnergyAndNum({using} energy,psi);
                           SetColor(psiColor);
                           DrawWaveFunctionOnEnergyLevel
                                           ({using} energy,X,psi);
                        END
                     ELSE
                        ClearThePotentialPlot({using} X,V);
               UNTIL finisht;
               SetScaleForV;
               ClearUpTheDisplay;
            END;
         3: {Show asymptote for a range of energies}
            BEGIN
               ExploreARangeOfEnergies({using} X,V);
               DrawThePotentialWellAsBackground(X,V);
            END;
         4: {Hunt for an eigenvalue}
            BEGIN
               PrepareToHunt;
               FuncYValue := YValue;
               SearchForZero('asymptote (diverging exponential)',
                             'energy', lowestV, energy);
               IF NOT abortedSearch THEN
                  BEGIN
                     Announce
                     ('       Press <Enter> or click mouse to continue.      ');
                     NormalizeTheWaveFunction({changing} psi);
                     DisplayResultOfSearch({using} energy,X,psi);
                  END
               ELSE
                  DrawThePotentialWellAsBackground(X,V);
            END;
      END;   {case code}
   END;

{------- PROCEDURES CONCERNING SUMS OF PROBABILITIES -------}

PROCEDURE RequestQuantumNumbers(spectrum:eValList;
                                VAR n1,n2:Integer; VAR escape:Boolean);
   VAR
      thisScreen : TInputScreen;
      thisHS    : HelpScrType;
   BEGIN
      WITH thisScreen DO
         BEGIN
            init;
            DefineInputPort(0.02,0.98, 0.05,0.10);
            IF spectrum.numLevels<1 THEN
               BEGIN
                  Announce
                   ('You have not calculated the spectrum yet.  Better do it now.');
                  finisht := true;
               END
            ELSE
               BEGIN
                  LoadLine(concat(' Sum probabilities, levels {   }',
                                  'thru {   } (<=',
                                  numStr(spectrum.numLevels,2,0),
                                  ')  [  Ok  ] [Cancel] [Help]'));

                  SetUpSumProbsHS(thisHS);
                  SetHelpScreen(thisHS);

                  SetNumber(1,n1);
                  SetNumber(2,n2);
                  SetNumberLimits(1,1,spectrum.numLevels);
                  SetNumberLimits(2,1,spectrum.numLevels);
                  escape := false;
                  AcceptScreen;
                  IF Canceled
                     THEN escape := true
                     ELSE
                        BEGIN
                           n1 := round(thisScreen.getNumber(1));
                           n2 := round(thisScreen.getNumber(2));
                           IF n1>n2 THEN SwapInteger(n1,n2);
                        END;
                  ClearViewport;
               END;    {ELSE}
            Done;
         END;         {with thisScreen}
   END;

PROCEDURE PrepareDisplayProbs(spectrum:eValList; VAR X,V:Dvector);
   VAR
      maxprob : Real;
   BEGIN
      ClearMUPPETport;
      SetColor(dullColor);
      OpenColoredViewport(4);
      SelectScale(1);
      DrawAxis(0,0, 0.1,50, dullColor);
      PutLabel(right,'energy (eV)');
      PutLabel(bottom,'distance (nm)');
      SetColor(potlColor1);
      PlotDVectors(X,V,1,numData+1);
      ShowEnergyLevels(spectrum);
      SetColor(dullColor);
      OpenColoredViewport(6);
      maxProb := 2/cellWidth;
      DefineScale(2, lowestX,highestX, -0.25*maxProb,+1.25*maxProb);
      SelectScale(2);
      DrawAxis(0,0, 0.1,10, dullColor);
      PutLabel(right,'prob density (1/nm)');
   END;

PROCEDURE DrawProbs(lowN,highN:Integer; spectrum:eValList; soundOn:Boolean;
                    VAR X,V,psi,prob,sumprob:Dvector);
   VAR
      i,j           : Integer;
      EB            : Real;
      mismatch      : Real;
      coeff1,coeff2 : Real;
   BEGIN
      SetColor(probColor2);
      FOR j:=1 TO numData+1 DO
         sumProb.Put(j,0);
      FOR i:=lowN TO highN DO
         BEGIN
            EB := spectrum.eigenvals[i];
            SelectViewport(4);
            SelectScale(1);
            PlotLine(lowestX,-EB, highestX,-EB);
            SolveTwoHalfEquations({using} EB,X,V,
                                        {giving} psi,mismatch,coeff1,coeff2);
            FitHalvesTogether({using} coeff1,coeff2, {changing} psi);
            NormalizeTheWaveFunction({changing} psi);
            CalculateProbabilityFunction(psi,prob);
            SoundNote(EB,soundOn);
            SelectViewport(6);
            SelectScale(2);
            PlotDVectors(X,prob,1,numData+1);
            FOR j:=1 TO numData+1 DO
               sumProb.Put(j,sumProb.value(j)+prob.value(j));
         END;
   END;

PROCEDURE DrawSumOfProbs(VAR X,sumProb:Dvector);
   BEGIN
      Delay(500);
      SelectViewport(6);
      SelectScale(2);
      SetColor(probColor1);
      PlotDVectors(X,sumProb,1,numData+1);
   END;

PROCEDURE ExploreTheSpectrum(code:Integer; numCells:Integer;
                             VAR soundOn:Boolean;
                             VAR spectrum: eValList;
                             VAR X,V:Dvector);
   VAR
      energy          : Real;
      finisht,escape  : Boolean;
      showProb        : Boolean;
      n,lowN,highN    : Integer;
   BEGIN
      CASE code OF
       1 : {Calculate the eigenvalue spectrum}
           BEGIN
              DrawThePotentialWellAsBackground(X,V);
              FindNumberOfEigenvalues({using} X,V,psi,
                                      {giving} spectrum);
              FindAllEigenvalues({using} X,V,psi,
                                 {completing} spectrum,spectrumFound);
           END;
    2..3 : {Show individual eigenfunctions}
           BEGIN
              IF code=3 THEN showProb := true
                        ELSE showProb := false;
              PrepareToDisplayWaveFunctions({setting} n);
              LabelTheGraph(showProb);
              REPEAT
                 InputALevelNumber({using} spectrum,
                                   {giving} n,finisht);
                 IF NOT finisht THEN
                    BEGIN
                       LabelTheGraph(showProb);
                       DisplayEigenFunction({using}
                                            spectrum,n,X,V,psi,prob);
                       IF code=3 THEN
                          DisplayProbabilityFunction({using}
                                            spectrum,n,X,prob);
                    END;
              UNTIL finisht;
              Message('');
           END;
       4 : {display sum of probabilities}
           BEGIN
              lowN := 1;
              highN := numCells;
              PrepareDisplayProbs({using} spectrum,X,V);
              REPEAT
                 RequestQuantumNumbers({using}spectrum,
                                       {giving} lowN,highN,escape);
                 IF NOT escape THEN
                    BEGIN
                       PrepareDisplayProbs({using} spectrum,X,V);
                       DrawProbs({using} lowN,highN,spectrum,soundOn,X,V,
                                 {giving} psi,prob, sumprob);
                       DrawSumOfProbs({using} X,sumProb);
                    END;
              UNTIL escape;
              DrawThePotentialWellAsBackground(X,V);
           END;
       6 : {toggle the flag for sound on}
           IF soundOn THEN soundOn := false
                      ELSE soundOn := true;
       7 : {Do some user defined calculation}
            DoCalculationUser({using} X,V,psi,spectrum);
      END;    {case rChoice}
   END;

{---------------- PROCEDURES USED FOR REGULAR LATTICES ----------------}

PROCEDURE InputNumberWellsFromScreen(VAR numCells:Integer;
                                      VAR escape: Boolean);
   VAR
      thisScreen  : TInputScreen;
   BEGIN
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputPort(0.05,0.95, 0.05,0.10);
            LoadLine(concat('Enter number of wells in lattice ( <=',
                            NumStr(maxNumCells,2,0),
                            ' ) : {   }  [  OK  ]  [Cancel]'));
            SetNumber(1,numCells);
            SetNumberLimits(1, 1,maxNumCells);
            AcceptScreen;
            escape := true;
            IF NOT Canceled THEN
               BEGIN
                  escape   := false;
                  numCells := Round(GetNumber(1));
               END;
            Done;
         END;    {with thisScreen}
   END;

PROCEDURE InputLatticeShapeFromScreen(VAR thisWellType:Integer;
                                      VAR escape: Boolean);
   VAR
      i,n         : Integer;
      thisScreen  : TInputScreen;
   BEGIN
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputport(0.05,0.95, 0.06,0.43);
            IF userFlag
               THEN n := 4
               ELSE n := 3;
            LoadLine('    Choose the shape of the individual wells in the lattice');
            LoadLine('             #1   square well');
            LoadLine('             #1   parabolic well');
            LoadLine('             #1   coulombic well');
            IF userFlag THEN
               LoadLine(Concat('             #1   ',userItem1));
            LoadLine('                                   [OK]       [Cancel]');
            SetBoolean(thisWellType,true);
            AcceptScreen;
            escape := true;
            IF NOT Canceled THEN
               BEGIN
                  escape := false;
                  thisWellType := 1;
                  FOR i:=1 TO n DO
                     IF GetBoolean(i) THEN
                        thisWellType := i;
               END;
            Done;
      END;   {with thisScreen}
   END;

PROCEDURE RequestAndAccept(VAR numCells:Integer; VAR thisWell:wellParams;
                           VAR finisht,escape:Boolean);
   VAR
      thisScreen : TInputScreen;
      n          : Integer;
   BEGIN
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputport(0.02,0.98, 0.06,0.65);
            LoadLine('');
            LoadLine('  You may change the number of wells in the lattice.');
            LoadLine(concat('          Number of wells (<=',
                            NumStr(maxNumCells,1,0),
                            ') = {   }'));
            SetNumber(1,numCells);
            SetNumberLimits(1, 1,maxNumCells);
            LoadLine('');

            LoadLine('  or change some of the parameters of the individual wells.');
            SetParticularScreenLines({using} thisWell, {changing} thisScreen);
            LoadLine('');
            LoadLine('  Select <  Ok  > when you are satisfied and want the program to proceed.');
            LoadLine('         <Cancel> if you want to exit without having made any changes.');
            LoadLine('         < View > to see the effect of the changes before proceeding.');
            LoadLine('');
            LoadLine('                       [  Ok  ]     [Cancel]     [ View ]');

            AcceptScreen;
           {Selecting <Cancel> means exit without any changes}
            IF Canceled
               THEN escape := true
               ELSE escape := false;
           {Selecting <View> means do not proceed but look at changes}
            IF GetBoolean(6)
               THEN finisht := false
               ELSE finisht := true;
            IF NOT escape THEN
               BEGIN
                  numCells := Round(thisScreen.getNumber(1));
                  ReadParticularParams(thisScreen, thisWell);
               END;
            Done;
         END;    {with thisScreen}
   END;

PROCEDURE AskIfThisIsAcceptible(VAR acceptFlag:Boolean);
   VAR
      tempScreen : TInputScreen;
   BEGIN
      WITH tempScreen DO
         BEGIN
            Init;
            DefineInputPort(0.05,0.95, 0.05,0.10);
            LoadLine(concat(' Accept this [  Ok  ]          Try another [Cancel]'));
            AcceptScreen;
            acceptFlag := NOT Canceled;
            Done;
         END;
   END;

PROCEDURE AskIfAnyChanges(VAR thisLattice:Lattice; VAR escape:Boolean);
   VAR
      finisht    : Boolean;
      tempLatt   : Lattice;
   BEGIN
      tempLatt := thisLattice;
      REPEAT
         WITH tempLatt DO
            BEGIN
               RequestAndAccept(numCells,cells[1].well,finisht,escape);
               ConstructRegularLattice({using} numCells,cells[1].well,
                                       {giving} tempLatt,spectrum);
            END;
         IF NOT escape THEN
            BEGIN
               ChangeGlobalVariables({using} tempLatt);
               CalculateLatticeDVector({using} tempLatt,
                                       {giving} V,spectrumFound);
               SetScaleForV;
               IF NOT finisht THEN
                  BEGIN
                     DrawThePotentialWellAsBackground(X,V);
                     AskIfThisIsAcceptible(finisht);
                  END;
            END;
      UNTIL finisht OR escape;
      IF NOT escape
         THEN thisLattice := tempLatt;
   END;

PROCEDURE SetUpRegularLattice(VAR thisLattice:Lattice;
                              VAR spectrumFound:Boolean);
   BEGIN
      ConstructRegularLattice
                    ({using} thisLattice.numCells,thisLattice.cells[1].well,
                     {giving} thisLattice,spectrum);
      ChangeGlobalVariables({using} thisLattice);
      CalculateLatticeDVector({using} thisLattice,
                              {giving} V,spectrumFound);
      SetScaleForV;
      spectrumFound := false;
      regularFlag   := true;
   END;

PROCEDURE InputLatticeParameters(code:Integer; VAR first:Boolean;
                                 VAR thisLattice:Lattice;
                                 VAR spectrumFound:Boolean);
   VAR
      escape : Boolean;
   BEGIN
      WITH thisLattice DO
         WITH cells[1] DO
            BEGIN
               CASE code OF
                1 : InputNumberWellsFromScreen
                                         ({giving} numCells,escape);
                2 : BEGIN
                       InputLatticeShapeFromScreen
                                    ({giving} well.wellType,escape);
                       SetDefaultWellParameters
                                    ({using} well.wellType,{giving} well);
                    END;
                3 : AskIfAnyChanges({changing} thisLattice,escape);
               END;   {case}

               SetUpRegularLattice({changing} thisLattice,spectrumFound);
            END;
   END;

{---------------- PROCEDURES USED FOR IRREGULAR LATTICES ----------------}

PROCEDURE SelectModelIrregularLattice(thisLattice:Lattice;
                                      VAR code:Integer; VAR escape:Boolean);
   VAR
      i,n        : Integer;
      thisScreen : TInputScreen;
   BEGIN
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputport(0.05,0.95, 0.06,0.50);
            LoadLine('       You may change this lattice to one of these irregular models:');
            LoadLine('');
            LoadLine('             #1   a regular lattice with one shallow well');
            LoadLine('             #1   a regular lattice with one narrow well');
            LoadLine('             #1   a regular lattice with one well displaced');
            LoadLine('             #1   a junction of two different lattices');
            LoadLine('             #1   an amorphous lattice');
            IF userFlag THEN
               LoadLine(Concat('             #1   ',userItem2));
            LoadLine('');
            LoadLine('                 [  Ok ]    [Cancel]');
            SetBoolean(code,true);
            AcceptScreen;

            escape := true;
            IF NOT Canceled
               THEN
                  BEGIN
                     escape := false;
                     code := 1;
                     IF userFlag THEN n := 6
                                 ELSE n := 5;
                     FOR i:=1 TO n DO
                        IF getBoolean(i) THEN code := i;
                  END;
            Done;
         END;   {with thisScreen}
   END;

PROCEDURE SetModelIrregularLattice(modelCode:Integer;
                        VAR thisLattice:Lattice; VAR spectrum:eValList);
   VAR
      n,i      : Integer;
      thisWell : WellParams;
      dV       : Real;
   BEGIN
      WITH thisLattice DO
         BEGIN
           {make a regular lattice first}
            SetDefaultWellParameters({using} thisLattice.cells[1].well.wellType,
                                     {giving} thisWell);
            ConstructRegularLattice({using} thisLattice.numCells,thisWell,
                                    {giving} thisLattice,spectrum);
           {now introduce irregularity}
            n := trunc(numCells*0.7);  {different well 2/3 way along}
            CASE modelCode OF
             1 : {nth well made shallower}
                 WITH cells[n].well DO
                    depth := depth*0.90;
             2 : {nth well made narrower}
                 WITH cells[n].well DO
                    width := width*0.80;
             3 : {nth well displaced}
                 WITH cells[n] DO
                    center := center - width/5;
             4 : {two different lattices}
                 BEGIN
                    n := trunc(numCells/2);
                    FOR i:=1 TO n DO
                       WITH cells[i].well DO
                          depth := depth - 15;
                 END;
             5 : {well centers randomly arranged}
                 BEGIN
                   WITH cells[1] DO center := center + width*0.17;
                   WITH cells[2] DO center := center - width*0.04;
                   WITH cells[3] DO center := center - width*0.10;
                   WITH cells[4] DO center := center + width*0.02;
                   WITH cells[5] DO center := center - width*0.18;
                   WITH cells[6] DO center := center + width*0.10;
                   WITH cells[7] DO center := center - width*0.02;
                   WITH cells[8] DO center := center + width*0.05;
                   WITH cells[9] DO center := center + width*0.00;
                 END;
             6 : {user defined lattice}
                 SetLatticeUser(thisLattice);
            END;   {case}
            regularFlag := false;
         END;
   END;

PROCEDURE CheckCenter(thisCenter:Real; i:Integer; thisLattice:Lattice;
                      VAR errorFlag:Boolean);
   BEGIN
      WITH thisLattice DO
         BEGIN
            IF i=1 THEN
              IF thisCenter<lowestX THEN
                 SayError('Well center 1 is outside available range.'
                          ,errorFlag);
            IF i>1 THEN
               IF thisCenter<=cells[i-1].center THEN
                 SayError(concat('Well center ',
                                 NumStr(i,2,0),
                                 ' is out of correct order.')
                          ,errorFlag);
            IF i=numCells THEN
              IF thisCenter>highestX THEN
                 SayError(concat('Well center ',
                                 NumStr(i,2,0),
                                 ' is outside available range.')
                          ,errorFlag);
         END;
   END;

PROCEDURE RequestNewParameters(VAR thisLattice:Lattice; VAR escape:Boolean);
   VAR
      thisScreen  : TInputScreen;
      i           : Integer;
      tempLattice : Lattice;
      errorFlag   : Boolean;
   BEGIN
      WITH thisScreen DO
         BEGIN
            Init;
            tempLattice := thisLattice;
            DefineInputport(0.00,1.00, 0.05,0.94);
            LoadLine('');
            LoadLine('       You are at liberty to change any or all of the following parameters ');
            LoadLine('       which define the individual wells in the lattice. ');
            LoadLine('');
            LoadLine('        well       center (nm)       width (nm)       depth (eV) ');
            LoadLine(concat('         no.                                              <= ',
                            NumStr(thisLattice.cells[1].well.maxDepth,3,0)));
            LoadLine('');
            WITH tempLattice DO
               BEGIN
                  FOR i:=1 TO numCells DO
                     WITH cells[i] DO
                        BEGIN
                           LoadLine(concat('         ',
                                           numStr(i,2,0),
                                           ':         {     }',
                                           '          {      }',
                                           '          {     }'));
                           SetNumber(3*(i-1)+1,center);
                           SetNumber(3*(i-1)+2,well.width);
                           SetNumber(3*(i-1)+3,well.depth);
                           SetNumberLimits(3*(i-1)+2,0,highestX-lowestX);
                           SetNumberLimits(3*(i-1)+3,0,well.maxDepth);
                        END;
               END;
            LoadLine('');
            LoadLine('                                 [  Ok  ]     [Cancel]');

            REPEAT
               errorFlag := false;
               AcceptScreen;
              {Pressing <Cancel> means exit without recording choices}
               IF Canceled
                  THEN escape := true
                  ELSE escape := false;
               WITH tempLattice DO
                  BEGIN
                     FOR i:=1 TO numCells DO
                        WITH cells[i] DO
                           BEGIN
                              center := thisScreen.getNumber(3*i-2);
                              CheckCenter(center,i,tempLattice,errorFlag);
                              well.width := thisScreen.getNumber(3*i-1);
                              well.depth := thisScreen.getNumber(3*i);
                           END;
                  END;
            UNTIL escape OR NOT errorFlag;
            IF NOT escape AND NOT errorFlag THEN
               BEGIN
                  thisLattice := tempLattice;
                  regularFlag := false;
               END;
            Done;
         END;    {with thisScreen}
   END;

{---------------- PROCEDURES USED WITH ELECTRIC FIELDS ----------------}

PROCEDURE RequestVoltage(maxVolt:Real; VAR voltage:Real;
                         VAR finisht,escape:Boolean);
   VAR
      thisScreen : TInputScreen;
      i          : Integer;
   BEGIN
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputport(0.05,0.95, 0.06,0.43);
            LoadLine('   You can now apply a voltage across the whole lattice. ');
            LoadLine('   Enter the magnitude of the voltage drop. ');
            LoadLine('');
            LoadLine(concat('             Voltage = {    }',
                            'V      (between ',
                            NumStr(-maxVolt,1,0),
                            ' and +',
                            NumStr(maxVolt,1,0),
                            ') '));
            SetNumber(1,voltage);
            LoadLine('');
            LoadLine('                       [  Ok  ]     [Cancel]     [ View ]');
            SetNumber(1,voltage);
            SetNumberLimits(1,-maxVolt,maxVolt);
            AcceptScreen;
           {Pressing <Cancel> means exit without changing things}
           IF Canceled
              THEN escape := true
              ELSE escape := false;
          {Pressing <View> means do not proceed but look at changes}
           IF GetBoolean(4)
              THEN finisht := false
              ELSE finisht := true;
           IF NOT escape THEN
              voltage := thisScreen.getNumber(1);
            Done;
         END;    {with thisScreen}
   END;

PROCEDURE ApplyFieldToLatticeDVector(voltage:Real; VAR V:Dvector);
   VAR
      i  : Integer;
      dV : Real;
   BEGIN
      dV := voltage/numData;
    { Note: voltage is applied from -V/2 to +V/2 }
      FOR i:=1 TO numData+1 DO
         V.Put(i,V.Value(i)-voltage/2+(i-1)*dV);
      V.Put(1,0);
      V.Put(numData+1,0);
   END;

PROCEDURE SubtractFieldFromLatticeDVector(voltage:Real; VAR V:Dvector);
   VAR
      i  : Integer;
      dV : Real;
   BEGIN
      dV := voltage/numData;
      FOR i:=1 TO numData+1 DO
         V.Put(i,V.Value(i)+voltage/2-(i-1)*dV);
      V.Put(1,0);
      V.Put(numData+1,0);
   END;

PROCEDURE InputVoltage(VAR voltage:Real; VAR escape:Boolean);
   VAR
      finisht      : Boolean;
      oldVoltage   : Real;
      firstVoltage : Real;
      maxVolt      : Real;
   BEGIN
      maxVolt := round(abs(highestV-lowestV)/5);
    { Note: the allowed electric field no bigger than 20% of Vmax }
      firstVoltage := voltage;
      oldVoltage   := voltage;
      REPEAT
         SubtractFieldFromLatticeDVector(oldVoltage,V);
         RequestVoltage(maxVolt,voltage,finisht,escape);
         IF NOT escape THEN
            BEGIN
               ApplyFieldToLatticeDVector(voltage,V);
               oldVoltage := voltage;
               IF NOT finisht THEN
                  BEGIN
                     DrawThePotentialWellAsBackground(X,V);
                     AskIfThisIsAcceptible(finisht);
                  END;
            END;
      UNTIL finisht OR escape;
      IF escape THEN
         BEGIN
            voltage := firstVoltage;
            ApplyFieldToLatticeDVector(voltage,V);
         END;
   END;

{ ------------------ FILE HANDLING PROCEDURES -------------------- }

{ ** These procedures save and read in global variables ** }

PROCEDURE SetUpOutputRecord(VAR thisFieldRecord:FieldRecord);
   BEGIN
      WITH thisFieldRecord DO
         BEGIN
            identifier   := fileIdentifier;
            fileWell     := thisWell;
            fileLattice  := thisLattice;
            fileSpectrum := spectrum;
            fileFound    := spectrumFound;
            fileVoltage  := voltage;
            fileModel    := modelCode;
         END;
   END;

PROCEDURE InterpretInputRecord(thisFieldRecord:FieldRecord);
   BEGIN
      WITH thisFieldRecord DO
         BEGIN
            thisWell      := fileWell;
            thisLattice   := fileLattice;
            ChangeGlobalVariables({using} thisLattice);
            CalculateLatticeDvector(thisLattice,V,spectrumFound);
            SetScaleForV;
            DrawThePotentialWellAsBackground(X,V);
            spectrum      := fileSpectrum;
            spectrumFound := fileFound;
            voltage       := fileVoltage;
            modelCode     := fileModel;
         END;
   END;

PROCEDURE AskForFileName(newFlag: Boolean; VAR fileName:String;
                            VAR escape:Boolean);
    VAR
       thisScreen : TInputScreen;
       nameFound  : Boolean;
       i,n        : Integer;
       ithChar    : String;
   BEGIN
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputPort(0.20,0.90, 0.52,0.80);
            LoadLine('');
            IF newFlag THEN
               LoadLine('        Save file as')
            ELSE
               LoadLine('        File name');
            LoadLine('        "            "               [  Ok  ]  ');
            LoadLine('');
            LoadLine('                                     [Cancel]');
            REPEAT
               SetString(1,fileName);
               AcceptScreen;
               escape := true;
               nameFound := true;
               IF NOT Canceled THEN
                  BEGIN
                     escape := false;
                     fileName := GetString(1);
                   { Take out all spaces }
                     n := length(fileName);
                     FOR i:=1 TO n DO
                        BEGIN
                           ithChar :=  Copy(fileName,n+1-i,1);
                           IF ithChar=' ' THEN
                              Delete(fileName,n+1-i,1);
                        END;
                     IF fileName='' THEN
                        BEGIN
                           beep;
                           nameFound := false;
                        END;
                  END;
            UNTIL escape OR nameFound;
            Done;
         END;    {with thisScreen}
   END;

PROCEDURE AskForNewFileName(VAR fileName:String;
                            VAR escape:Boolean);
   BEGIN
      AskForFileName(true,fileName,escape);
   END;

FUNCTION FileIsOnDisk(thisFileName:String; VAR thisFile:DiskFile) : Boolean;
   BEGIN
      Assign(thisFile,thisFileName);
      {$I-}                        {check file is on the disk}
      Reset(thisFile);
      {$I+}
      IF IOResult=0
         THEN
            BEGIN
               FileIsOnDisk := true;
               Close(thisFile);
            END
         ELSE FileIsOnDisk := false;
   END;

PROCEDURE CheckOKtoProceed(fileName:String; VAR proceed:Boolean);
    VAR
       thisScreen   : TInputScreen;
   BEGIN
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputPort(0.30,0.80, 0.10,0.40);
            LoadLine('              Warning     ');
            LoadLine('');
            LoadLine(Concat('   File ',
                            fileName,
                            ' already exists.'));
            LoadLine('   Overwrite?');
            LoadLine('');
            LoadLine('         [  Ok  ]     [Cancel]');
            AcceptScreen;
            IF Canceled
               THEN proceed := false
               ELSE proceed := true;
            Done;
         END;    {with thisScreen}
   END;

PROCEDURE FailedTryAnother(fileName:String; VAR another:Boolean);
    VAR
       thisScreen   : TInputScreen;
   BEGIN
      beep;
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputPort(0.30,0.80, 0.10,0.40);
            LoadLine('              Warning     ');
            LoadLine('');
            LoadLine(Concat('   File ',
                            fileName,
                            ' is not on the disk.'));
            LoadLine('   Try another?');
            LoadLine('');
            LoadLine('         [  Ok  ]     [Cancel]');
            AcceptScreen;
            IF Canceled
               THEN another := false
               ELSE another := true;
            Done;
         END;    {with thisScreen}
   END;

PROCEDURE WriteFileToDisk(thisFieldRec:FieldRecord; fileName:String;
                          VAR thisFile:DiskFile);
   BEGIN
      Assign(thisFile,fileName);         {Open a file on disk}
      Rewrite(thisFile);      {Set file to write at beginning}
      Write(thisFile,thisFieldRec);
      ClearMUPPETport;
      Close(thisFile);
   END;

PROCEDURE ReadFileFromDisk(VAR thisFieldRecord:FieldRecord;
                           VAR fileName:String; VAR thisfile:DiskFile);
   BEGIN
      Assign(thisFile,fileName);         {Open a file on disk}
      Reset(thisFile);         {Set file to read at beginning}
      Read(thisFile,thisFieldRecord);
      Close(thisFile);
      IF thisFieldRecord.identifier<>fileIdentifier THEN
         BEGIN
            beep;
            Message('ERROR. This is not an acceptable file!    Press <Enter> to continue.');
            Pause;
            EXIT;
         END;
   END;

PROCEDURE OpenFromDisk(VAR fileName:String);
   VAR
      tempFileName    : String;
      another         : Boolean;
      thisFound       : Boolean;
      thisFile        : DiskFile;
      thisFieldRecord : FieldRecord;
   BEGIN
      tempFileName := fileName;
      REPEAT
         tempFilename := OpenFile('*.*');
         IF NOT (tempFilename='') THEN
            BEGIN
               fileName := tempFileName;
               another := false;
               ReadFileFromDisk(thisFieldRecord,fileName,thisFile);
               InterpretInputRecord(thisFieldRecord);
            END
         ELSE
            FailedTryAnother(tempFileName,another);
      UNTIL NOT another;
   END;

PROCEDURE SaveAsOnDisk(VAR fileName:String);
   VAR
      tempFileName    : String;
      escape,proceed  : Boolean;
      thisFile        : DiskFile;
      thisFieldRecord : FieldRecord;
   BEGIN
      tempFileName := '';
      SetUpOutputRecord(thisFieldRecord);
      REPEAT
         AskForNewFileName(tempFileName,escape);
         proceed := true;
         IF NOT escape THEN
                      IF FileIsOnDisk(tempFileName,thisFile) THEN
                         CheckOKtoProceed(tempFileName,proceed);
      UNTIL escape OR proceed;
      IF NOT escape THEN
         BEGIN
            fileName := tempFileName;
            WriteFileToDisk(thisFieldRecord,fileName,thisFile);
         END;
   END;

PROCEDURE SaveOnDisk(VAR fileName:String);
   VAR
      tempFileName    : String;
      escape,proceed  : Boolean;
      thisFile        : DiskFile;
      thisFieldRecord : FieldRecord;
   BEGIN
      IF fileName='' THEN
         SaveAsOnDisk(fileName)
      ELSE
         BEGIN
            SetUpOutputRecord(thisFieldRecord);
            WriteFileToDisk(thisFieldRecord,fileName,thisFile);
         END;
   END;



{ ************ ALL USER WRITTEN PROCEDURES ARE HERE ************ }
{ *                                                            * }
{ *   You may change or rewrite any of the following seven     * }
{ *   procedures.  Just follow the templates already coded.    * }
{ *   The sample coding below solves the Matthieu problem.     * }
{ *                                                            * }
{ ************************************************************** }

PROCEDURE SetUserFlag;
   BEGIN
      userFlag := false;
   END;

PROCEDURE SetUserMenuItems;
 { This puts names of user defined items in the menus.    }
 { Currently they have descriptions of the "user defined" }
 { procedures written below.  When you write your own,    }
 { you may, if you like, change these names accordingly.  }
   BEGIN
    { DEFAULT : DO NOT HAVE USER MENU ITEMS }
      userItem1 := 'User defined';     {goes in WELL SHAPE menu}
      userItem2 := 'User defined';     {goes in CHOOSE LATTICE }
      userItem3 := 'User defined';     {goes in SPECTRUM menu  }
(*
    { ALTERNATIVE : THESE USER MENU ITEMS RELATE TO AUTHOR'S TEMPLATES}
      userItem1 := 'sinusoidal well';  {goes in WELL SHAPE menu}
      userItem2 := 'one deep well';    {goes in CHOOSE LATTICE }
      userItem3 := 'calculate <x>';    {goes in SPECTRUM menu  }
*)
   END;

{---------------- USER DEFINED POTENTIAL WELL -------------------}

PROCEDURE SetDefaultUser(VAR thisWell:wellParams);
 { This sets the depth and width of your well when first chosen  }
   BEGIN
      Announce('This is the default well.  Write the code for your choice.');
      WITH thisWell DO
         BEGIN
            depth := 300;
            width := 0.10;
            maxDepth := 300;
         END;
   END;

PROCEDURE SetScreenLinesUser(thisWell:wellParams;
                             VAR thisScreen:TInputScreen);
 {  This writes information to the input screen.         }
 {  Your well must input two and only two parameters.    }
 {  You needn't change this, but you might like to set   }
 {  up a more elaboarte input screen, in which case you  }
 {  should look at the procedure "SetScreenLinesSquare"  }
 {  to get an idea of what you can do.                   }
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine('        Read in parameter 1 = {      }');
            LoadLine('        Read in parameter 2 = {      }');
            SetNumber(2,thisWell.depth);
            SetNumberLimits(2, 0,thisWell.maxDepth);
     {Note: for the Matthieu problem, only one parameter is adjustable}
         END;    {with thisScreen}
   END;

PROCEDURE ReadParamsUser(VAR thisScreen:TInputScreen;
                         VAR thisWell:wellParams);
 { This reads information from the input screen.  Again note }
 { for the Matthieu problem, only one parameter is adjustable}
   BEGIN
      WITH thisWell DO
         BEGIN
            depth := thisScreen.getNumber(2);
         END;
   END;

FUNCTION UserPotential(VAR thisWell:wellParams; x:Real): Real;
 { This calculates the potential DVector. Currently it is a }
 { sinusoidal well, which is used in the Matthieu problem.  }
   VAR
      V : Real;
   BEGIN
       V := 0;
       WITH thisWell DO
          BEGIN
            IF x>=-width/2 THEN
               V := -depth*(cos(2*pi*x/width)+1)/2;
            IF x>width/2  THEN
               V := 0;
          END;
       UserPotential := V;
   END;

{--------------- USER DEFINED LATTICE PROCEDURES ----------------}

PROCEDURE SetLatticeUser(VAR thisLattice:Lattice);
 { This sets the parameters for a particular lattice.  Currently }
 { it changes a regular lattice to one with a single deeper well.}
   VAR
      n : Integer;
   BEGIN
      Announce('This is the default lattice.  Write the code for your choice.');
      WITH thisLattice DO
         BEGIN
            n := trunc(numCells*0.7);  {different well 2/3 way along}
            WITH cells[n].well DO
               depth := depth*1.10;
         END;
   END;

{-------------------- USER DEFINED CALCULATIONS -----------------------}

PROCEDURE DoCalculationUser(VAR X,V,psi:DVector; thisSpectrum:eValList);
 {  This allows the user to do some unforeseen calculation.  Currently }
 {  it calculates and displays <x> for a chosen eigenfunction.         }
   VAR
      i,n       : Integer;
      sum, norm : Real;
      finisht   : Boolean;
   BEGIN
      Announce('This is the default calculation.  Write the code for your choice.');
      PrepareToDisplayWaveFunctions({setting} n);
      REPEAT
         InputALevelNumber({using} spectrum, {giving} n,finisht);
         IF NOT finisht THEN
            BEGIN
               DisplayEigenFunction({using} spectrum,n,X,V,psi,prob);
               sum := 0;
               norm := 0;
               FOR i:=1 TO numData+1 DO
                  BEGIN
                     sum := sum + sqr(psi.value(i))*X.value(i);
                     norm := norm + sqr(psi.value(i));
                  END;
               IF norm>macheps THEN sum := sum/norm;
               Message(concat('      <x> = ',
                              NumStr(sum,5,4),
                              ' nm.'));
            END;
      UNTIL finisht;
      Message('');
   END;

{ ************ END OF USER WRITTEN PROCEDURES ************ }


{ ------------ GENERAL PROCEDURES FOR MENUS AND HOT-KEYS-------------- }

PROCEDURE SetUpMainMenu(VAR thisMenu:Tmenu);
   BEGIN
      WITH thismenu DO
         BEGIN
            Init;

            column(1,'File');
            row(1,1,'About CUPS');
            row(1,2,'About Program');
            row(1,3,'Configuration');
            row(1,4,'Open...');
            row(1,5,'Save');
            row(1,6,'Save as...');
            row(1,7,'Exit Program');


            column(2,'Wells');
            row(2,1,'Number of Wells');
            row(2,2,'Shape of Each Well');
            row(2,3,'Well Parameters');

            column(3,'Lattice');
            row(3,1,'Regular Lattice');
            row(3,2,'Irregular Lattice');
            row(3,3,'--------------------');
            row(3,4,'Adjust Parameters');
            row(3,5,'Apply Electric Field');
            RowActivate(3,3,false);
            RowChecked(3,1,regularFlag);
            RowChecked(3,2,NOT regularFlag);

            column(4,'Method');
            row(4,1,'Try Energy (with mouse)');
            row(4,2,'Try Energy (from keyboard)');
            row(4,3,'Solve Range of Energies');
            row(4,4,'Hunt for Zero');

            column(5,'Spectrum');
            row(5,1,'Find Eigenvalues');
            row(5,2,'See Wave Functions');
            row(5,3,'See Wfs and Probs');
            row(5,4,'Sum Probabilities');
            row(5,5,'------------------');
            row(5,6,'Sound');
            IF userFlag THEN
               row(5,7,userItem3);
            RowActivate(5,5,false);
         END;
   END;

PROCEDURE DisposeOfMainMenu(thisMenu:TMenu);
   BEGIN
      thisMenu.done;
   END;

PROCEDURE ChooseFromMainMenu(VAR thisMenu:TMenu;
                             soundOn,spectrumFound:Boolean;
                             VAR colNum,rowNum:integer);
   VAR
      found : Boolean;
      i     : Integer;
   BEGIN
      WITH thisMenu DO
         BEGIN
            RowActivate(5,2,spectrumFound);
            RowActivate(5,3,spectrumFound);
            RowActivate(5,4,spectrumFound);

            RowChecked (5,6,soundOn);
            RowChecked(3,1,regularFlag);
            RowChecked(3,2,NOT regularFlag);

            REPEAT
               found := chosen
            UNTIL found;
            colNum := colChosen;
            rowNum := rowChosen;
         END;
   END;

PROCEDURE InitializeUserProcedures;
   BEGIN
      SetUserFlag;
      SetUserMenuItems;
   END;


{*----------------------- MAIN PROGRAM --------------------------*}

PROCEDURE IntroduceProgram;
   VAR
      thisMenu : Tmenu;
   BEGIN
      SetUpMainMenu(thismenu);
      thisMenu.Display;
      DisplayHelpScreen(progHS);
      thisMenu.Done;
   END;

PROCEDURE SetUpDefaultLattice(VAR thisLattice:Lattice;
                              VAR voltage:Real;
                              VAR spectrum:EValList;
                              VAR spectrumFound:Boolean;
                              VAR V:DVector);
   VAR
      thisWell : WellParams;
   BEGIN
      WITH thisLattice DO
         BEGIN
            numCells := 6;
            cells[1].well.wellType := 1;
         END;
      voltage   := 0;             {no applied field to start}
      modelCode := 1;             {for default irregular lattice}
      SetDefaultWellParameters({using} thisLattice.cells[1].well.wellType,
                               {giving} thisWell);
      ConstructRegularLattice({using} thisLattice.numCells,thisWell,
                              {giving} thisLattice,spectrum);
      ChangeGlobalVariables({using} thisLattice);
      CalculateLatticeDVector({using} thisLattice, {giving} V,spectrumFound);
      SetScaleForV;
   END;


BEGIN
   CUPSinit;
   InitializeUserProcedures;
   SetDefaultGlobalValues;
   SetUpMainMenu(mainMenu);
   SetPlottingDetails;
   SetUpDefaultLattice({setting} thisLattice,voltage,
                                 spectrum,spectrumFound,V);
   SetScaleForV;
   IntroduceProgram;
   DrawThePotentialWellAsBackground(X,V);

   REPEAT
      ChooseFromMainMenu(mainMenu,soundOn,spectrumFound,cChoice,rChoice);
      CASE cChoice OF
         1 : {where to go to now?}
             BEGIN
                CASE rChoice OF
                 1 : AboutCUPS;                   {describe CUPS}
                 2 : {show information about program}
                     BEGIN
                        DisplayHelpScreen(progHS);
                        DisplayHelpScreen(mainAHS);
                        DisplayHelpScreen(mainBHS);
                     END;
                 3 : Configuration;          {various facilities}
                 4 : {Open old file}
                     OpenFromDisk(currentFileName);
                 5 : {Save}
                     SaveOnDisk(currentFileName);
                 6 : {Save as}
                     SaveAsOnDisk(currentFileName);
                 7 : {Quit the whole program}
                       quitFlag := true;
                END;  {case rchoice}
             END;

         2 : {Select shape and number of wells in the lattice}
             BEGIN
                InputLatticeParameters(rChoice,first,
                                       thisLattice,spectrumFound);
                DrawThePotentialWellAsBackground(X,V);
             END;

         3 : {Set the kind of lattice}
             BEGIN
                CASE rChoice OF
                 1 : BEGIN
                        first := true;
                        SetUpRegularLattice({changing} thisLattice,spectrumFound);
                     END;
                 2 : BEGIN
                        SelectModelIrregularLattice(thisLattice,modelcode,escape);
                        IF NOT escape THEN
                           SetModelIrregularLattice({using} modelCode,
                                              {giving} thisLattice,spectrum);
                     END;
                 4 : BEGIN
                        RequestNewParameters({giving} thisLattice,escape);
                     END;
                 5 : BEGIN
                        InputVoltage({giving} voltage,escape);
                     END;
                END;    {case}

                first := false;
                ChangeGlobalVariables({using} thisLattice);
                CalculateLatticeDVector({using} thisLattice,
                                        {giving} V,spectrumFound);
                SetScaleForV;
                ApplyFieldToLatticeDVector({using} voltage,
                                           {giving} V);
                DrawThePotentialWellAsBackground(X,V);
             END;

         4 : {Demonstrate the method of solving the problem}
             BEGIN
                first := true;
                ExploreMethodOfSolution({using} rChoice,energy,X,V);
             END;

         5 : {solve wave eqn and examine solutions}
             BEGIN
                first := true;
                ExploreTheSpectrum({using} rChoice,thisLattice.numCells,
                                           soundOn,spectrum,X,V);
             END;
      END;   {case cChoice};
   UNTIL quitFlag;

   CUPSdone;
END.


