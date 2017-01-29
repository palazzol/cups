{Program title: BOUND STATE WAVE FUNCTIONS IN ONE DIMENSION           }
{Author: Ian D. Johnston             Copyright 1993                   }
{Version: 3.01                       23 August 1994                   }
{                                                                     }
{Computes energy eigenfunctions for an electron in various potential  }
{wells, which can be square, parabolic, ramped, asymmetric,double or  }
{Coulombic.  The potential is assumed to be zero to the left of Xmin, }
{and to the right of Xmax.  The wave functions are computed by "hunt  }
{and shoot" methods.   One part of the program is designed to show the}
{student how to solve the problem in principle, by choosing a value of}
{the binding energy, calculating  from left to right and seeing if the}
{resulting function behaves like a decreasing exponential at x=Xmax.  }
{In another part of the program the eigenvalues are computed atuomat- }
{ically, by counting the number of nodes to determine where the eigen-}
{values occur.                                                        }
{The first part of the program deals with simply finding the eigen-   }
{values and eigenfunctions of different wells.  The second part looks }
{at properties of eigenfunctions - normalization, orthogonality and   }
{the evaluation of many kinds of overlap integrals.  The third part of}
{the program examines time development of bound state wave functions. }
{Facility is provided for users to incorporate their own procedures to}
{specify different potential wells or different overlap integrals.    }
{                                                                     }
{Utilities: Crt, Graph, MUPPET, CUPS, CUPSfunc, CUPSproc, CUPSgui     }
{Library files: The search procedures are in the unit Search.tpu and  }
{               all the help screens are in the unit B1Dhelps.tpu.    }
{Algorithms:    Numerov                                               }
{                                                                     }

{$F+}
{$N+}
{$D-}

PROGRAM Bound1D;

USES
   Crt, Graph, CUPSmupp,
   CUPS, CUPSfunc, CUPSproc, CUPSgui, CUPSgrph, CUPSpars,
   Search,b1dhelps;

VAR
   partCode : Integer;
   quitFlag : Boolean;

{ --------------- ITEMS COMMON TO ALL PARTS ----------------- }

CONST
   h    = 6.626E-34;       {J.s}
   hbar = 1.055E-34;       {J.s}
   e    = 1.602E-19;       {C}
   me   = 9.109E-31;       {kg}
   a0   = 5.292E-11;       {m}
   nm   = 1.000E-09;       {m - unit for measuring lengths}
   fs   = 1.000E-15;       {s - unit for measuring times}
   eV   = 1.602E-19;       {J - unit for measuring energies}
   eps0 = 8.854E-12;       {F.m-1}
   energyConvert = 2*me*eV*nm*nm/hbar/hbar;
   timeConvert   = hbar/eV/fs;
   maxNumLevels  = 13;

   psiColor   : word = white;
   potlColor1 : word = lightblue;
   potlColor2 : word = lightgray;
   probColor  : word = yellow;
   engyColor  : word = cyan;
   realColor  : word = lightred;
   imagColor  : word = lightgreen;
   dullColor  : word = lightgray;
   dullerColor: word = darkgray;
   braColor   : word = lightgreen;
   ketColor   : word = lightgreen;
   prodColor  : word = lightgreen;
   intgColor  : word = green;

   inpBackColor : Integer = blue;
   MUPbackColor : Integer = black;

TYPE
   wellParams = RECORD
                   wellType : Integer;
                   depth1   : Real;
                   depth2   : Real;
                   depth3   : Real;
                   depth4   : Real;
                   posn1    : Real;
                   posn2    : Real;
                   posn3    : Real;
                   posn4    : Real;
                   posn5    : Real;
                   maxDepth : Real
                END;
   Perturbn = RECORD
                 perturbnType : Integer;
                 coefficient  : Real;
              END;
   ValuList = ARRAY [1..20] OF real;
   eValList = RECORD
                 numLevels : integer;
                 eigenVals : ValuList;
              END;
   psiParam = RECORD
                 quantNum : Integer;
                 coeffs   : ARRAY[1..maxNumLevels] OF Real;
                 funcStr  : String;
              END;
   overlap  = RECORD
                 bra,ket     : DVector;
                 psi2,braket : DVector;
                 braParams   : PsiParam;
                 ketParams   : PsiParam;
                 braDrawn    : Boolean;
                 ketDrawn    : Boolean;
                 operatorNum : Integer;
              END;
   cxVector = RECORD
                 Re,Im: Dvector;
              END;
   fnArray = ARRAY [1..maxNumLevels] OF Dvector;
   ampArray= ARRAY [1..maxNumLevels] OF real;
   ampsType= RECORD
                Re,Im: ampArray;
             END;
   Str1    = String[1];
   FieldRecord = RECORD
                    identifier   : String;
                    fileWell     : WellParams;
                    filePerturbn : Perturbn;
                    fileSpectrum : eValList;
                    fileFound    : Boolean;
                    fileBraParam : PsiParam;
                    fileKetParam : PsiParam;
                    fileCxAmps   : AmpsType;
                 END;

   DiskFile = FILE OF FieldRecord;


CONST
   numData = 200;
   fileIdentifier = 'File for BOUND1D.PAS';
   first : Boolean = true;
   same  : Boolean = true;

VAR
   highestX,lowestX,deltaX : Real;
   highestV,lowestV        : Real;
   X,V,psi,prob,temp       : dVector;
   energy, area            : Real;

   cxPsi     : cxVector;
   eigFns    : fnArray;
   thisAmps  : ampsType;
   nAmps     : Integer;

 { These variables are global }
   thisWell          : WellParams;
   thisPerturbn      : Perturbn;
   spectrum          : eValList;
   spectrumFound     : Boolean;
   thisOverlap       : Overlap;
   initialCxAmps     : ampsType;
   currentFileName   : String;
   operators         : ARRAY[1..12] of String;
   psi1Name,psi2Name : String;

   thisData         : FieldRecord;

   escape,pure      : Boolean;
   realFlag         : Boolean;
   key,key1         : Byte;
   n,choice,chosen  : Integer;
   finished,goBack  : Boolean;
   soundOn          : Boolean;
   developKeys      : Thotkeys;

{ ------------ PROCEDURES AND TYPES FOR USER PROGRAMMING -------------- }

PROCEDURE ConstructKetUser1(VAR X,V,bra,ket:DVector);
   FORWARD;
PROCEDURE ConstructKetUser2(VAR X,V,bra,ket:DVector);
   FORWARD;
PROCEDURE SetDefaultUser(VAR thisWell:wellParams);
   FORWARD;
PROCEDURE SetScreenLinesUser(VAR thisWell:wellParams; VAR n:Integer;
                             VAR thisScreen:TInputScreen);
   FORWARD;
PROCEDURE ReadParamsUser(VAR thisScreen:TInputScreen;
                         VAR thisWell:wellParams);
   FORWARD;
PROCEDURE CalculateDVectorUser(VAR thisWell:wellParams; VAR V:dVector);
   FORWARD;
FUNCTION PerturbationUser(a,x:Real):Real;
   FORWARD;
FUNCTION MeasurementUser(VAR X:dVector; VAR cxPsi:cxVector):Real;
   FORWARD;

VAR
   userFlag  : Boolean;   {true if there is a special user potential}
   userItem1, userItem2 : String;  {for putting in the menus by user}
   userItem3, userItem4 : String;
   userItem5, userItem6 : String;



{ ------------ GENERAL PROCEDURES FOR POTENTIAL WELLS -------------- }

PROCEDURE SayError(thisStr:string; VAR flag:Boolean);
   BEGIN
      flag := true;
      Message(thisStr);
      Beep;
   END;

PROCEDURE CheckDepth(thisDepth,thisMaxDepth:Real; VAR flag:Boolean);
   BEGIN
      IF (thisDepth<0) THEN
         SayError('Depths cannot be negative.',flag);
      IF (thisDepth>thisMaxDepth) THEN
         SayError(concat('Depths cannot be greater than ',
                         NumStr(thisMaxDepth,3,0),
                         '.')
                  ,flag);
   END;

PROCEDURE CheckWidth(thisWidth:Real; VAR flag:Boolean);
   BEGIN
      IF (thisWidth<0) THEN
         SayError('Widths cannot be negative.',flag);
      IF (thisWidth>(highestX-lowestX)) THEN
         SayError('Widths cannot be greater than space available.'
                  ,flag);
   END;

{-------------------- SQUARE WELL ------------------------}

PROCEDURE SetDefaultSquare(VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := 300;
            posn1 := -0.1;
            posn2 := +0.1;
            maxDepth := 400;
         END;
   END;

PROCEDURE SetScreenLinesSquare(thisWell:wellParams; VAR n:Integer;
                               VAR thisScreen:TInputScreen);
   VAR
      thisMaxWidth : Real;
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('          Depth of the well in eV (<',
                            numStr(thisWell.maxDepth,3,0),
                            ') = {      }'));
            SetNumber(1,thisWell.depth1);
            SetNumberLimits(1, 0, thisWell.maxDepth);

            thisMaxWidth := highestX-lowestX;
            LoadLine(concat('          Width of the well in nm',
                            '        = {      }'));
            SetNumber(2,thisWell.posn2-thisWell.posn1);
            SetNumberLimits(2,0,thisMaxWidth);
         END;    {with thisScreen}
         n := 2;                 {there are two items to be read in}
   END;

PROCEDURE ReadParamsSquare(VAR thisScreen:TInputScreen;
                           VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := thisScreen.GetNumber(1);
            posn2  := abs(thisScreen.GetNumber(2)/2);
            posn1 := - posn2;
         END;
   END;

PROCEDURE CalculateDVectorSquare(VAR thisWell:wellParams;
                                 VAR V:dVector);
   VAR
      i: integer;
   BEGIN
      WITH thisWell DO
         BEGIN
            FOR i:=1 TO numData+1 DO
               BEGIN
                  V.Put(i,0);
                  IF X.value(i)>=posn1 THEN V.Put(i,-depth1);
                  IF X.value(i)>posn2  THEN V.Put(i,0);
               END;
         END;
   END;

{-------------------- RAMPED WELL ------------------------}

PROCEDURE SetDefaultRamped(VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := 300;
            depth2 := 100;
            posn1 := -0.1;
            posn2 := +0.1;
            maxDepth := 400;
         END;
   END;

PROCEDURE SetScreenLinesRamped(VAR thisWell:wellParams; VAR n:Integer;
                               VAR thisScreen:TInputScreen);
   VAR
      thisMaxWidth : Real;
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('  Left depth in eV (<',
                            numStr(thisWell.maxDepth,3,0),
                            ') = {      }',
                            ' Right depth in eV (<',
                            numStr(thisWell.maxDepth,3,0),
                            ') = {      }'));
            SetNumber(1,thisWell.depth1);
            SetNumberLimits(1, 0, thisWell.maxDepth);
            SetNumber(2,thisWell.depth2);
            SetNumberLimits(2, 0, thisWell.maxDepth);

            thisMaxWidth := highestX-lowestX;
            LoadLine(concat('          Width of the well in nm',
                            '        = {      }'));
            SetNumber(3,thisWell.posn2-thisWell.posn1);
            SetNumberLimits(3,0,thisMaxWidth);
         END;    {with thisScreen}
         n := 3;              {there are three items to be read in}
   END;

PROCEDURE ReadParamsRamped(VAR thisScreen:TInputScreen;
                           VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
          BEGIN
             depth1 := thisScreen.GetNumber(1);
             depth2 := thisScreen.GetNumber(2);
             posn2  := abs(thisScreen.GetNumber(3)/2);
             posn1 := - posn2;
          END;
    END;

 PROCEDURE CalculateDVectorRamped(VAR thisWell:wellParams;
                                VAR V:dVector);
    VAR
       i: integer;
    BEGIN
       WITH thisWell DO
          BEGIN
             FOR i:=1 TO numData+1 DO
                BEGIN
                    V.Put(i,0);
                    IF X.value(i)>=posn1 THEN
                       IF posn2>posn1
                          THEN V.Put(i,(-depth1 + (depth1-depth2)*(X.value(i)-posn1)/(posn2-posn1)))
                          ELSE V.Put(i,-depth1);
                    IF X.value(i)>posn2 THEN V.Put(i,0);
                 END;
          END;
    END;

 {-------------------- ASYMMETRIC WELL ------------------------}

 PROCEDURE SetDefaultAsymmetric(VAR thisWell:wellParams);
    BEGIN
       WITH thisWell DO
          BEGIN
             depth1 := 300;
             depth2 := 100;
             posn1 := -0.1;
             posn2 :=  0.0;
             posn3 := +0.1;
             maxDepth := 400;
          END;
    END;

 PROCEDURE SetScreenLinesAsymmetric(VAR thisWell:wellParams; VAR n:Integer;
                                VAR thisScreen:TInputScreen);
    VAR
       thisMaxWidth : Real;
    BEGIN
       WITH thisScreen DO
          BEGIN
             LoadLine(concat('  Left depth in eV (<',
                             numStr(thisWell.maxDepth,3,0),
                             ') = {      }',
                             '  Right depth in eV (<',
                             numStr(thisWell.maxDepth,3,0),
                             ') = {      }'));
             SetNumber(1,thisWell.depth1);
             SetNumberLimits(1, 0, thisWell.maxDepth);
             SetNumber(2,thisWell.depth2);
             SetNumberLimits(2, 0, thisWell.maxDepth);

             thisMaxWidth := highestX-lowestX;
             LoadLine(concat('  Total width in nm',
                             '       = {      }',
                             '  Central wall in nm',
                             '       = {      }'));
             SetNumber(3,thisWell.posn3-thisWell.posn1);
             SetNumberLimits(3,0,thisMaxWidth);
             SetNumber(4,thisWell.posn2);
             SetNumberLimits(4,0,thisMaxWidth);
         END;    {with thisScreen}
         n := 4;                {there are four items to be read in}
   END;

PROCEDURE ReadParamsAsymmetric(VAR thisScreen:TInputScreen;
                               VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
          BEGIN
             depth1 := thisScreen.GetNumber(1);
             depth2 := thisScreen.GetNumber(2);
             posn3  := abs(thisScreen.GetNumber(3)/2);
             posn1  := - posn3;
             posn2  := thisScreen.getNumber(4);
          END;
   END;

PROCEDURE CalculateDVectorAsymmetric(VAR thisWell:wellParams;
                               VAR V:dVector);
   VAR
      i: integer;
   BEGIN
      WITH thisWell DO
         BEGIN
            FOR i:=1 TO numData+1 DO
                BEGIN
                   V.Put(i,0);
                   IF X.value(i)>=posn1 THEN V.Put(i,-depth1);
                   IF X.value(i)>posn2  THEN V.Put(i,-depth2);
                   IF X.value(i)>posn3  THEN V.Put(i,0);
                END;
         END;
   END;

{-------------------- DOUBLE SQUARE WELL ------------------------}

PROCEDURE SetDefaultDoubleSquare(VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := 300;
            depth2 := 50;
            posn1 := +0.10;
            posn2 := +0.05;
            posn3 := (highestX+lowestX)/2;
            maxDepth := 400;
         END;
   END;

PROCEDURE SetScreenLinesDoubleSquare(VAR thisWell:wellParams; VAR n:Integer;
                               VAR thisScreen:TInputScreen);
   VAR
      thisMaxWidth : Real;
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('  Depth at rim in eV (<',
                            numStr(thisWell.maxDepth,3,0),
                            ') = {      }',
                            '  at center in eV (>-',
                            numStr(0.25*thisWell.maxDepth,3,0),
                            ') = {      }'));
            SetNumber(1,thisWell.depth1);
            SetNumberLimits(1, 0, thisWell.maxDepth);
            SetNumber(2,thisWell.depth2);
            SetNumberLimits(2, -0.25*thisWell.maxDepth, thisWell.maxDepth);

            thisMaxWidth := highestX-lowestX;
            LoadLine(concat('  Width of outer well in nm',
                            ' = {      }',
                            '  of central hump in nm',
                            '   = {      }'));
            SetNumber(3, 2*thisWell.posn1);
            SetNumberLimits(3,0,thisMaxWidth);
            SetNumber(4, 2*thisWell.posn2);
            SetNumberLimits(4,0,thisMaxWidth);
         END;    {with thisScreen}
         n := 4;                {there are four items to be read in}
   END;

PROCEDURE ReadParamsDoubleSquare(VAR thisScreen:TInputScreen;
                                 VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := thisScreen.GetNumber(1);
            depth2 := thisScreen.GetNumber(2);
            posn1  := abs(thisScreen.GetNumber(3)/2);
            posn2  := abs(thisScreen.GetNumber(4)/2);
         END;
   END;

PROCEDURE CalculateDVectorDoubleSquare(VAR thisWell:wellParams;
                               VAR V:dVector);
   VAR
      i: integer;
   BEGIN
      WITH thisWell DO
         BEGIN
            FOR i:=1 TO numData+1 DO
                BEGIN
                   V.Put(i,0);
                   IF X.value(i)>=posn3-posn1 THEN V.Put(i,-depth1);
                   IF X.value(i)>=posn3-posn2 THEN V.Put(i,-depth2);
                   IF X.value(i)>posn3+posn2  THEN V.Put(i,-depth1);
                   IF X.value(i)>posn3+posn1  THEN V.Put(i,0);
                END;
         END;
   END;

{-------------------- PARABOLIC WELL ------------------------}

PROCEDURE SetDefaultParabolic(VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := 300;
            posn1 := 0.1;
            maxDepth := 900;
         END;
   END;

PROCEDURE SetScreenLinesParabolic(VAR thisWell:wellParams; VAR n:Integer;
                                  VAR thisScreen:TInputScreen);
   VAR
      thisMaxWidth : Real;
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('          Depth of the well in eV (<',
                            numStr(thisWell.maxDepth,3,0),
                            ') = {      }'));
            SetNumber(1,thisWell.depth1);
            SetNumberLimits(1, 0, thisWell.maxDepth);

            thisMaxWidth := highestX-lowestX;
            LoadLine(concat('          Width at the top in nm',
                            '         = {      }'));
            SetNumber(2, 2*abs(thisWell.posn1));
            SetNumberLimits(2,0,thisMaxWidth);
         END;    {with thisScreen}
         n := 2;                 {there are two items to be read in}
   END;

PROCEDURE ReadParamsParabolic(VAR thisScreen:TInputScreen;
                              VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := thisScreen.GetNumber(1);
            posn1  := abs(thisScreen.GetNumber(2)/2);
         END;
   END;

PROCEDURE CalculateDVectorParabolic(VAR thisWell:wellParams;
                               VAR V:dVector);
   VAR
      i: integer;
   BEGIN
      WITH thisWell DO
         BEGIN
             FOR i:=1 TO numData+1 DO
                BEGIN
                   V.Put(i,0);
                   IF X.value(i)>=-posn1
                      THEN V.Put(i,depth1*(sqr(X.value(i)/posn1)-1));
                   IF X.value(i)>posn1 THEN V.Put(i,0);
                END;
         END;
   END;

{-------------------- COULOMBIC WELL ------------------------}

PROCEDURE SetDefaultCoulombic(VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 :=  20;
            depth2 := 300;
            posn1  := 0.1;
            maxDepth := 300;
         END;
   END;

PROCEDURE SetScreenLinesCoulombic(VAR thisWell:wellParams; VAR n:Integer;
                               VAR thisScreen:TInputScreen);
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('        Depth at the bottom in eV (<',
                            numStr(thisWell.maxDepth,3,0),
                            ') = {      }'));
            SetNumber(1,thisWell.depth2);
            SetNumberLimits(1, 0, thisWell.maxDepth);

            LoadLine(concat('          Depth at (x=',
                            NumStr(thisWell.posn1,2,1),
                            ') in eV (< ',
                            numStr(thisWell.maxDepth,3,0),
                            ') = {      }'));
            SetNumber(2, thisWell.depth1);
            SetNumberLimits(2,0,thisWell.maxDepth);
         END;    {with thisScreen}
         n := 2;                 {there are two items to be read in}
   END;

PROCEDURE ReadParamsCoulombic(VAR thisScreen:TInputScreen;
                              VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := abs(thisScreen.GetNumber(2));
            depth2 := abs(thisScreen.GetNumber(1));
         END;
   END;

PROCEDURE CalculateDVectorCoulombic(VAR thisWell:wellParams;
                                    VAR V:dVector);
   VAR
      i: integer;
   BEGIN
      WITH thisWell DO
         BEGIN
             FOR i:=1 TO numData+1 DO
                IF abs(X.value(i))*depth2>depth1*posn1
                   THEN V.Put(i,-depth1*posn1/abs(X.value(i)))
                   ELSE V.Put(i,-depth2);
         END;
   END;

{------------- GENERAL PROCEDURES FOR ALL WELLS ----------}

PROCEDURE SetDefaultWellParameters(VAR thisWell:wellParams);
   BEGIN
      CASE thisWell.wellType OF
         1 : SetDefaultSquare({setting} thisWell);
         2 : SetDefaultRamped({setting} thisWell);
         3 : SetDefaultAsymmetric({setting} thisWell);
         4 : SetDefaultDoubleSquare({setting} thisWell);
         5 : SetDefaultParabolic({setting} thisWell);
         6 : SetDefaultCoulombic({setting} thisWell);
         7 : SetDefaultUser({setting} thisWell);
      END;   {with case}
   END;

PROCEDURE CalculateWellDVector(VAR thisWell:wellParams;
                               VAR V:dVector; VAR spectrumFound:Boolean);
   VAR
      i: integer;
   BEGIN
     CASE thisWell.wellType OF
         1 : CalculateDVectorSquare({using} thisWell, {giving} V);
         2 : CalculateDVectorRamped({using} thisWell, {giving} V);
         3 : CalculateDVectorAsymmetric({using} thisWell, {giving} V);
         4 : CalculateDVectorDoubleSquare({using} thisWell, {giving} V);
         5 : CalculateDVectorParabolic({using} thisWell, {giving} V);
         6 : CalculateDVectorCoulombic({using} thisWell, {giving} V);
         7 : CalculateDVectorUser({using} thisWell, {giving} V);
        END;   {with case}

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
   END;


PROCEDURE SetParticularScreenLines(thisWell:wellParams; VAR n:Integer;
                                   VAR thisScreen:TInputScreen);
   BEGIN
      CASE thisWell.wellType OF
       1 : SetScreenLinesSquare(thisWell,n,thisScreen);
       2 : SetScreenLinesRamped(thisWell,n,thisScreen);
       3 : SetScreenLinesAsymmetric(thisWell,n,thisScreen);
       4 : SetScreenLinesDoubleSquare(thisWell,n,thisScreen);
       5 : SetScreenLinesParabolic(thisWell,n,thisScreen);
       6 : SetScreenLinesCoulombic(thisWell,n,thisScreen);
       7 : SetScreenLinesUser(thisWell,n,thisScreen);
      END;    {case}
   END;

PROCEDURE ReadParticularParams(VAR thisScreen:TInputScreen;
                               VAR thisWell:wellParams);
   BEGIN
      CASE thisWell.wellType OF
       1 : ReadParamsSquare({using} thisScreen, {giving} thisWell);
       2 : ReadParamsRamped({using} thisScreen, {giving} thisWell);
       3 : ReadParamsAsymmetric({using} thisScreen, {giving} thisWell);
       4 : ReadParamsDoubleSquare({using} thisScreen, {giving} thisWell);
       5 : ReadParamsParabolic({using} thisScreen, {giving} thisWell);
       6 : ReadParamsCoulombic({using} thisScreen, {giving} thisWell);
       7 : ReadParamsUser({using} thisScreen, {giving} thisWell);
      END;    {case}
   END;


{--------------------- PERTURBATIONS -----------------}

PROCEDURE ListTypesOfPerturbation(VAR n:Integer;
                                  VAR thisScreen:TInputScreen);
   BEGIN
      WITH thisScreen DO
         BEGIN
            IF userFlag THEN n := 6
                        ELSE n := 5;
            LoadLine('       Choose which kind of perturbation you want to work with');
            LoadLine(concat('             #1   Constant  (A)'));
            LoadLine(concat('             #1   Linear    (A.x)'));
            LoadLine(concat('             #1   Quadratic (A.x^2)'));
            LoadLine(concat('             #1   Cubic     (A.x^3)'));
            LoadLine(concat('             #1   Quartic   (A.x^4)'));
            IF userFlag THEN
               LoadLine(concat('             #1   ',userItem4));
         END;    {with thisScreen}
   END;

PROCEDURE CheckSizePerturbn(thisPerturbn:Perturbn; VAR flag:Boolean);
   VAR
      maxP : Real;
      maxL : Real;
   BEGIN
      maxL := (highestX - lowestX)/2;
      maxP := abs(highestV - lowestV)/10;
      CASE thisPerturbn.perturbnType OF
       1 : maxP := maxP;
       2 : maxP := maxP/maxL;
       3 : maxP := maxP/maxL/maxL;
       4 : maxP := maxP/maxL/maxL/maxL;
       5 : maxP := maxP/maxL/maxL/maxL/maxL;
       6 : ;
      END;   {with case}
      IF (thisPerturbn.coefficient>maxP) THEN
         SayError
            ('Maximum perturbation cannot be greater than 10% well depth.',
             flag);
   END;

PROCEDURE AddPerturbationToDVector(thisPerturbn:Perturbn; VAR V:dVector);
   VAR
      i,j,n  : Integer;
      deltaV : Real;
      Xi,Vi  : Real;
   BEGIN
      n := thisPerturbn.PerturbnType;
      FOR i:=1 TO numData+1 DO
         BEGIN
            deltaV := thisPerturbn.coefficient;
            Xi := X.value(i);
            Vi := V.value(i);
            CASE thisPerturbn.perturbnType OF
             1 : deltaV := deltaV;
             2 : deltaV := deltaV*Xi;
             3 : deltaV := deltaV*Xi*Xi;
             4 : deltaV := deltaV*Xi*Xi*Xi;
             5 : deltaV := deltaV*Xi*Xi*Xi*Xi;
             6 : deltaV := PerturbationUser(deltaV,Xi);
            END;   {with case}
         V.Put(i,Vi+deltaV);
         END;
     V.Put(1,0);
     V.Put(numdata+1,0);

     lowestV  := 0;
     highestV := 0;
     FOR i:=1 TO numData+1 DO
        BEGIN
          IF V.value(i)<lowestV  THEN lowestV  := V.value(i);
          IF V.value(i)>highestV THEN highestV := V.value(i);
        END;
   END;

{----------------- GENERAL -----------------------------}

PROCEDURE OpenHiddenViewport (Vnum: integer);
BEGIN
   SetColor(MUPbackColor);
   SelectViewPort(vNum);
   HideCursor;
   SetFillStyle(SolidFill, MUPBackColor);
   WITH Views[vnum] DO bar(0,0,Vx2-Vx1,Vy2-Vy1);
   WITH Views[vnum] DO rectangle(0,0,Vx2-Vx1,Vy2-Vy1);
   ShowCursor;
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

PROCEDURE SetGlobalValues;
   VAR
      i: integer;
   BEGIN
      highestX := +0.16;           {scale on x axis is in nm}
      lowestX  := -0.16;
      deltaX   := (highestX-lowestX)/numData;
      soundOn := true;
      energy  := 0;
    { in order that the scales have something in them at the start }
      DefineScale(1,lowestX,highestX,-1.0,1.0);
      DefineScale(2,lowestX,highestX,-1.0,1.0);
      DefineScale(3,lowestX,highestX,-1.0,1.0);
      DefineScale(4,lowestX,highestX,-1.0,1.0);

      X.init(numdata+1);
      V.init(numdata+1);
      psi.init(numdata+1);
      prob.init(numdata+1);
      temp.init(numdata+1);
      FOR i:=1 TO numData+1 DO X.Put(i,lowestX + (i-1)*deltaX);
   END;

{ --------------- PROCEDURES FOR PLOTTING DETAILS ----------------- }

PROCEDURE SetPlottingDetails;
   BEGIN
      graphBackColor := black;
      DefineViewport(1,  0.10,0.90, 0.50,0.90);  {for potential well}
      DefineViewport(2,  0.10,0.90, 0.18,0.88);  {for well + functions}
      DefineViewport(3,  0.10,0.90, 0.15,0.45);  {for the hunting process}
      DefineViewport(4,  0.10,0.90, 0.15,0.50);  {for on the study screen}
      DefineViewport(5,  0.06,0.68, 0.69,0.92);  {for the bra function}
      DefineViewport(6,  0.06,0.68, 0.40,0.63);  {for the ket function}
      DefineViewport(7,  0.06,0.68, 0.09,0.32);  {for the braket integral}
      DefineViewport(8,  0.75,0.99, 0.09,0.32);  {for the integration result}
      DefineViewport(9,  0.05,0.95, 0.41,0.87);  {for animation}
      DefineViewport(10, 0.05,0.70, 0.12,0.36);  {for measurements}
      DefineViewport(11, 0.75,0.95, 0.12,0.40);  {for color wheel}
      DefineViewport(12, 0.05,0.95, 0.07,0.12);  {for message re find energy}
      DefineViewport(13, 0.01,0.99, 0.35,0.65);  {for error message}
   END;

PROCEDURE SpecialScale(s:Integer; VAR Y:DataVector);
   VAR
      i    : Integer;
      maxY, ticY : Real;
   BEGIN
      maxY := 0;
      FOR i:=1 TO numData+1 DO
         IF abs(Y[i])>maxY THEN maxY := abs(Y[i]);
      DefineScale(s, lowestX,highestX, -maxY,maxY);
      ticY := trunc(maxY);
      SetColor(lightgray);
      Axis(0,0, 0.10,ticY);
   END;

PROCEDURE SemiAutoScale(snum:integer; VAR psi:Dvector);
   VAR
      n       : Integer;
      min,max : Real;
      ticY    : Real;
   BEGIN
    { Erase old tic marks first }
      max := graphScales[snum].yMax;
      IF max>1
         THEN ticY := trunc(max)
         ELSE ticY := trunc(10*max)/10;
      SelectScale(snum);
      IF ticY>0 THEN
         DrawAxis(0,0, 0.10,ticY,MUPbackColor);
    { Then set the new scale and draw axes }
      psi.MinMax(n,min,n,max);
      IF abs(min)>abs(max)
         THEN max := 1.02*abs(min)
         ELSE max := 1.02*abs(max);
      IF max<0.102 THEN max := 0.102;
      DefineScale(snum,lowestX,highestX,-max,max);
      IF max>1
         THEN ticY := trunc(max)
         ELSE ticY := trunc(10*max)/10;
      DrawAxis(0,0, 0.10,ticY,dullColor);
   END;

PROCEDURE SemiAutoScaleOld(snum:integer; VAR psi:Dvector);
   VAR
      n: integer;
      min,max: real;
   BEGIN
      psi.MinMax(n,min,n,max);
      IF min>0 THEN min := 0;
      IF max<0 THEN max := 0;
      DefineScale(snum,lowestX,highestX,min,max);
      PlotLine(lowestX,0, highestX,0);
   END;

PROCEDURE ShowEnergyLevels(VAR spectrum:eValList);
   VAR
      i:integer;
   BEGIN
      SelectViewport(4);
      SelectScale(1);
      SetColor(engyColor);
      WITH spectrum DO
         FOR i:=1 TO numLevels DO
            PlotLine(lowestX,-eigenvals[i], highestX,-eigenVals[i]);
   END;

PROCEDURE DrawBar(x,y:real);
   BEGIN
      SetLineStyle(0,0,thickwidth);
      PlotLine(x-deltaX/2,0, x-deltaX/2,y);
      SetLineStyle(0,0,normwidth);
   END;

{ ------------ GENERAL PROCEDURES FOR WAVE EQUATION -------------- }

PROCEDURE SolveWaveEquationOld(EB:real; VAR X,V,psi:dVector);
  {This is a simple "half-step" integrator.}
   VAR
      psi0,k: real;
      i: integer;
   BEGIN
      psi0 := 200;                      {sets amplitude of the solution}
      k := sqrt(abs(energyConvert*EB));  {wave number outside the well}
    { Initial conditions of integration: psi must be an increasing    }
    { exponential for values of X to the left of the potential well.  }
      psi.Put(1,psi0 * exp(k*X.value(1)));
      psi.Put(2,psi0 * exp(k*X.value(2)));
      FOR i:=3 TO numData+1 DO
         psi.Put(i,2*psi.value(i-1) - psi.value(i-2)
                   + energyConvert*(EB+V.value(i-1))*psi.value(i-1)*deltaX*deltaX);
   END;

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
      A.init(NumData+1);
      FOR i:=1 TO numData+1 DO
         A.Put(i,energyConvert*(EB+V.value(i))*deltaX*deltaX/12.0);
      Numerov(1,numData+1,deltaX,A,psi);
      A.Free;
   END;

{ ------------------------------------------------------------------- }

FUNCTION EndCorrection(y1,y2:real):real;
  {** If the integrand (y) is of the form: A exp(kx) for **}
  {** values of x less than x1, then the integral of y   **}
  {** from -infinity to x1 is equal to:                  **}
  {**   y1/k  or equivalently,  y1*deltaX/ln(y2/y1)      **}
   VAR
      temp   : Real;
      factor : Real;
   BEGIN
      temp := 0;
      IF abs(y1)>macheps THEN
         IF (y2/y1)>macheps THEN
            BEGIN
               factor := ln(y2/y1);
               IF factor>macheps THEN
                  temp := y1*deltaX/ln(y2/y1);
            END;
     {Note: if anything wrong, correction is left as zero }
      endCorrection := temp;
   END;

PROCEDURE NormalizeTheWaveFunction(VAR psi:dVector);
   VAR
      normFactor: real;
      area: real;
      temp1,temp2: real;
      i: integer;
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
      normfactor := 1/sqrt(area);
      psi.MultScalar(normFactor);
   END;

PROCEDURE CalculateProbabilityFunction(VAR psi,prob:Dvector);
   VAR
      temp: real;
      i: integer;
   BEGIN
            FOR i:=1 TO numdata+1 DO
               BEGIN
                  temp := psi.value(i)*psi.value(i);
                  prob.Put(i,temp);
               END;
   END;

PROCEDURE SetADefaultWell(VAR thisWell:wellParams; VAR thisPert:Perturbn;
                          VAR V:Dvector; VAR spectrum:eValList;
                          VAR spectrumFound:Boolean);
   BEGIN
      thisWell.wellType := 1;    {choose a square well for default}
      SetDefaultWellParameters({giving} thisWell);
      thisPert.perturbnType := 1;
      thisPert.coefficient  := 0;
      CalculateWellDvector(thisWell,V,spectrumFound);
      WITH spectrum DO
         BEGIN
            numLevels := 6;
            eigenVals[1] := 292.409570;
            eigenVals[2] := 269.719901;
            eigenVals[3] := 232.204175;
            eigenVals[4] := 180.440292;
            eigenVals[5] := 115.668716;
            eigenVals[6] :=  41.429625;
            DefineScale(1, lowestX,highestX, -315,+15);
         END;
      spectrumFound := true;
   END;


PROCEDURE RecordAllEigenFunctions(spectrum:eValList; VAR X,V:Dvector;
                                  VAR eigFns:fnArray);
   VAR
      i,n: integer;
      energy: real;
   BEGIN
      n := spectrum.numLevels;
      IF n>maxNumLevels THEN n := maxNumLevels;
      FOR i:=1 TO n DO
         BEGIN
            energy := spectrum.eigenVals[i];
            SolveWaveEquation({using} energy,X,V, {giving} eigFns[i]);
            NormalizeTheWaveFunction({changing} eigFns[i]);
         END;
   END;

{ --------------- PROCEDURES ESPECIALLY FOR PART 1 ----------------- }

{ ------------ GENERAL PROCEDURES FOR POTENTIAL WELLS -------------- }

PROCEDURE SetPotentialPlottingScale(VAR V:dVector);
   VAR
      offsetV: real;
   BEGIN
      offsetV := (highestV - lowestV)/20;
      DefineScale(1, lowestX,highestX, lowestV-offsetV,highestV+offsetV);
   END;

PROCEDURE ZoomPotentialPlottingScale(EB:Real; VAR V:dVector);
   VAR
      offsetV, center    : Real;
      thisLowV,thisHighV : Real;
   BEGIN
      offsetV := (highestV - lowestV)/40;
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

PROCEDURE ChooseAPotentialWell(n:Integer;
                               VAR thisWell:wellParams;
                               VAR V:dVector;
                               VAR spectrum:eValList;
                               VAR spectrumFound:Boolean);
   BEGIN
      thisWell.wellType := n;
      SetDefaultWellParameters({altering} thisWell);
      CalculateWellDVector({using} thisWell, {giving} V,spectrumFound);
      spectrum.numLevels := 0;  {spectrum still has to be calculated}
      spectrumFound := false;
   END;

PROCEDURE RequestAndAccept(VAR thisWell:wellParams;
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
            LoadLine('You may care to change some of the parameters of this well.');
            LoadLine('');
            SetParticularScreenLines({using} thisWell,
                                     {changing} n,thisScreen);
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
            IF GetBoolean(n+3)
               THEN finisht := false
               ELSE finisht := true;
            IF NOT escape THEN
               ReadParticularParams(thisScreen, thisWell);
            Done;
         END;    {with thisScreen}
   END;


PROCEDURE DrawThePotentialWellAsBackground(VAR X,V:Dvector;
                                           VAR examine:Boolean);
   BEGIN
      ClearMUPPETport;
      examine := false;
      SetColor(dullColor);
      OpenColoredViewport(2);
      SelectScale(1);
      DrawAxis(0,0, 0.05,50, dullColor);
      PutLabel(right,'energy (eV)');
      PutLabel(bottom,'distance (nm)');
      SetColor(potlColor1);
      PlotDVectors(X,V,1,numData+1);
   END;

PROCEDURE AskIfThisIsAcceptible(VAR acceptFlag:Boolean);
   VAR
      tempScreen : TInputScreen;
   BEGIN
      WITH tempScreen DO
         BEGIN
            Init;
            DefineInputPort(0.05,0.95, 0.05,0.10);
            LoadLine(concat(' Accept this  [  Ok  ]          Try another  [Cancel]'));
            AcceptScreen;
            acceptFlag := NOT Canceled;
            Done;
         END;
   END;

PROCEDURE MakeAnyChanges(VAR thisWell:wellParams);
   VAR
      finisht  : Boolean;
      tempWell : WellParams;
   BEGIN
      tempWell := thisWell;
      Message('');
      REPEAT
         RequestAndAccept(tempWell,finisht,escape);
         IF NOT escape THEN
            BEGIN
               CalculateWellDVector({using} tempWell,
                                    {giving} V,spectrumFound);
               IF NOT finisht THEN
                  BEGIN
                     DrawThePotentialWellAsBackground(X,V,finisht);
                     AskIfThisIsAcceptible(finisht);
                  END
               ELSE
                  SetPotentialPlottingScale(V);
            END;
      UNTIL finisht OR escape;
      IF NOT escape THEN
         thisWell := tempwell;
   END;

PROCEDURE RequestPerturbation(thisWell:WellParams;
                              VAR thisPerturbn:Perturbn;
                              VAR finisht,escape:boolean);
   VAR
      thisScreen : TInputScreen;
      i,n        : Integer;
      errorFlag  : Boolean;
   BEGIN
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputport(0.03,0.95, 0.06,0.73);
            ListTypesOfPerturbation(n,thisScreen);
            SetBoolean(thisPerturbn.perturbnType, true);
            LoadLine('');
            LoadLine(concat('       and input the coefficient (A) = ',
                            '{     } (units of eV/m',
                            '^',
                            Numstr(i,1,0),
                            ')'));
            LoadLine('');
            LoadLine('  Select <  Ok  > when you are satisfied and want the program to proceed.');
            LoadLine('         <Cancel> if you want to exit without having made any changes.');
            LoadLine('         < View > to see the effect of the changes before proceeding.');
            LoadLine('');
            LoadLine('                       [  Ok  ]     [Cancel]     [ View ]');
            SetNumber(n+1,thisPerturbn.coefficient);
            SetNumberLimits(n+1, -100, 100);

            REPEAT
               errorFlag := false;
               AcceptScreen;
              {Pressing <escape> means exit without any changes}
               IF Canceled
                  THEN escape := true
                  ELSE escape := false;
              {Selecting <View> means do not proceed but look at changes}
               IF GetBoolean(n+4)
                  THEN finisht := false
                  ELSE finisht := true;
               IF NOT escape THEN
                  BEGIN
                     FOR i:=1 TO n DO
                        IF GetBoolean(i) THEN
                           thisPerturbn.perturbnType:= i;
                     thisPerturbn.coefficient := GetNumber(n+1);
                     CheckSizePerturbn(thisPerturbn,errorFlag);
                  END;
             UNTIL escape OR NOT errorFlag;
             Done;
         END;    {with thisScreen}
   END;

PROCEDURE ChoosePerturbation(thisWell:wellParams;
                             VAR thisPerturbn:Perturbn);
   VAR
      finisht      : Boolean;
      tempPerturbn : Perturbn;
   BEGIN
      tempPerturbn := thisPerturbn;
      REPEAT
         RequestPerturbation(thisWell,tempPerturbn,finisht,escape);
         IF NOT escape THEN
            BEGIN
               CalculateWellDVector({using} thisWell,
                                    {giving} V,spectrumFound);
               AddPerturbationToDVector({using} tempPerturbn, {giving} V);
               IF NOT finisht THEN
                  BEGIN
                     DrawThePotentialWellAsBackground(X,V,finisht);
                     AskIfThisIsAcceptible(finisht);
                  END
               ELSE
                  SetPotentialPlottingScale(V);
            END;
      UNTIL finisht OR escape;
      IF NOT escape THEN
         thisPerturbn := tempPerturbn;
   END;

PROCEDURE ChangePotentialAxis;
   BEGIN
   END;

PROCEDURE ClearThePotentialPlot(VAR X,V:Dvector; VAR examine:Boolean);
   BEGIN
      SetColor(dullColor);
      OpenColoredViewport(2);
      SelectScale(1);
      DrawAxis(0,0, 0.05,50, dullColor);
      SetColor(potlColor1);
      PlotDVectors(X,V,1,numData+1);
      examine := false;
   END;

PROCEDURE PrepareToFindWaveFunctions(VAR X,V:dVector;
                                     VAR EB:real; VAR examine:Boolean);
   BEGIN
      DrawThePotentialWellAsBackground(X,V,examine);
      EB := -0.8*lowestV;    {default value of the binding energy}
      choice := 1;           {default setting of choice index}
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
            BEGIN
               xFactor := (xmax-xmin)/(Vx2-Vx1);
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

      OpenHiddenViewport(12);
      SetColor(white);
      Print(16,1,'Click within the graph to choose an energy.');

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
                        ZoomPotentialPlottingScale(EB,V)
                     ELSE
                        SetPotentialPlottingScale(V);
                  END;
               IF kNum=4 THEN finisht := true;
            END
         ELSE
            BEGIN
               LocalMouseposn(event.X,event.Y,x,y,2,1,inside);
               IF inside THEN EB := -y;
            END;
      UNTIL inside OR erase OR finisht;
      inpBackColor := tempColor;
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
      thisHS: helpScrType;
   BEGIN
      WITH thisScreen DO
         BEGIN
            init;
            DefineInputPort(0.05,0.95, 0.05,0.10);
            LoadLine(concat('Choose EB (+ve) {      }eV',
                            ':   [ help ]  [clear ]  [  Ok  ]  [Cancel]'));
            SetNumber(1,EB);
            SetNumberLimits(1, 0,abs(graphScales[1].Ymin));
            SetUpTryEnergyHS(thisHS);
            SetHelpScreen(thisHS);
            finished := false;
            erase := false;
            SetColor(white);
            AcceptScreen;
            IF GetBoolean(3) THEN
               erase := true;
            IF GetBoolean(5) THEN
               finished := true;
            IF (NOT erase) AND (NOT finished) THEN
               EB := abs(thisScreen.getNumber(1));
            done;
         END;   {with thisScreen}
   END;


PROCEDURE ShowTheEnergyLevel(EB:real);
   BEGIN
      SelectViewport(2);
      SelectScale(1);
      SetColor(engyColor);
      PlotLine(lowestX,-EB, highestX,-EB);
   END;

PROCEDURE DrawWaveFunctionOnEnergyLevel(EB:real; VAR X,psi:dVector);
   VAR
      i: integer;
      scaleFactor: real;
   BEGIN
      scaleFactor := 0.1*abs(lowestV)*sqrt(highestX);
      FOR i:=1 TO numData+1 DO
         temp.Put(i,-EB + scaleFactor*psi.value(i));
      SelectViewport(2);
      SelectScale(1);
      PlotDVectors(X,temp,1,numData+1);
   END;

PROCEDURE DisplayProbabilityFunction(spectrum:eValList; n:integer;
                               VAR X,prob:Dvector);
   VAR
      i  : Integer;
      EB : Real;
      scaleFactor: real;
   BEGIN
      scaleFactor := 0.1*abs(lowestV)*highestX;
      EB := spectrum.eigenVals[n];
      SetColor(probColor);
      FOR i:=1 TO numData+1 DO
         temp.Put(i,-EB + scaleFactor*prob.value(i));
      SelectViewport(2);
      SelectScale(1);
      PlotDVectors(X,temp,1,numData+1);
   END;

PROCEDURE ExamineGraphWithMouse(EB:Real; VAR X,psi:DVector);
   VAR
      inside,finisht    : Boolean;
      theseKeys         : THotKeys;
      x0,y0,x1,y1,x2,y2 : Real;
      oldX0             : Real;
      amp               : Real;
      i                 : Integer;
      kNum              : Byte;
      tempColor         : Word;
   BEGIN
      WITH theseKeys DO
         BEGIN
            Init(2);
            key[1] := 'F1-Help';
            key[2] := 'F10-Menu';
         END;   {with}
      Message('');

      OpenHiddenViewport(12);
      SetColor(lightmagenta);
      Print(13,1,'Click within the graph to measure x and psi(x).');

      finisht := false;
      oldX0 := lowestX;
      theseKeys.Display;
      SelectViewport(2);
      SelectScale(1);
      SetColor(lightmagenta);
      DrawWaveFunctionOnEnergyLevel(EB,X,psi);
      CheckForEvents;
      REPEAT
         DynamicPause;
         CheckForEvents;
         IF theseKeys.Pressed(knum) THEN
            BEGIN
               IF (kNum=1) OR (kNum=2) THEN
                  IF kNum=1 THEN
                     DisplayHelpScreen(examineHS);{describe program};
                  IF kNum=2 THEN
                     BEGIN
                        finisht := true;
                        Message('');
                     END;
            END
         ELSE
            BEGIN
               LocalMouseposn(event.X,event.Y,x0,y0,2,1,inside);
               IF inside THEN
                  BEGIN
                     i := 1;
                     SetColor(dullerColor);
                     PlotLine(oldX0,lowestV,oldX0,highestV);
                     SetColor(white);
                     PlotLine(x0,lowestV,x0,highestV);
                     oldX0 := x0;
                     WHILE x0>X.value(i) DO i := i+1;
                     y2 := psi.value(i);
                     y1 := psi.value(i-1);
                     x2 := X.value(i);
                     x1 := X.value(i-1);
                     amp := y1 + (x0-x1)*(y2-y1)/(x2-x1);
                     Message(concat('     x = ',
                                    NumStr(x0,4,4),
                                    ' (nm);      psi(x) = ',
                                    NumStr(amp,6,4),
                                    ' (nm-1/2)'));
                  END;
            END;
      UNTIL finisht;
      theseKeys.clear;
      inpBackColor := tempColor;
   END;

PROCEDURE PrepareToHunt;
   VAR
      highestPsi : Real;
   BEGIN
      highestPsi := 8*sqrt(highestX-lowestX);
      DefineScale(2, lowestX,highestX, -highestPsi,+highestPsi);
      Message('');
   END;

PROCEDURE DisplayIntermediateSolution(VAR psi:DVector);
   BEGIN
      SetColor(dullColor);
      IF ViewportNumber=3
         THEN SelectViewport(3)
         ELSE OpenColoredViewport(3);
      SelectScale(2);
      PlotLine(lowestX,0, highestX,0);
      SetColor(psiColor);
      PlotDVectors(X,psi,1,numData+1);
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

FUNCTION FastYvalue(Xvalue:real):real;
 { This is the function for the FastSearch procedure }
   VAR
      k: real;
   BEGIN
      energy := Xvalue;
      SolveWaveEquation(energy,X,V,psi);
      k := sqrt(abs(energyConvert*energy));  { wave number outside the well }
      FastYvalue := psi.value(numdata) - exp(k*deltaX)*psi.value(numData+1);
    { This means the last two points are in the same ratio as the first two }
   END;

PROCEDURE DisplayResultOfSearch(EB:real; VAR X,psi:Dvector;
                                         VAR examine:Boolean);
   BEGIN
      DrawThePotentialWellAsBackground(X,V,examine);
      ShowTheEnergyLevel(EB);
      SetColor(psiColor);
      DrawWaveFunctionOnEnergyLevel(EB,X,psi);
      PutLabel(inside,concat('Binding energy = ',
                             NumStr(EB,7,2),
                             ' eV'));
   END;

PROCEDURE PrepareToDisplaySpectrum(VAR examine:Boolean);
   BEGIN
      DrawThePotentialWellAsBackground(X,V,examine);
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

PROCEDURE FindNumberOfEigenvalues(VAR X,V,psi:Dvector; VAR spectrun:eValList);
   VAR
      EB: real;
      n : integer;
   BEGIN
      EB := 0;
      SolveWaveEquation({using} EB,X,V, {giving} psi);
      spectrum.numLevels := NumberOfNodes({using} EB,psi);
    { It is assumed here that number of eigenvalues is one greater }
    { then the number of nodes for the highest energy level, which }
    { is one fewer than the number for the  solution with E = 0.   }
      Message(concat('Number of levels = ',NumStr(spectrum.numLevels,2,0)));
   END;

PROCEDURE SoundNote(E:Real; soundOn:Boolean);
   VAR
      f: Integer;
   BEGIN
      IF soundOn THEN
         BEGIN
            f := Round(770*(-E-lowestV)/(highestV-lowestV) + 110);
            Sound(f);
            Delay(200);
            NoSound;
         END;
   END;


PROCEDURE FindLevelRecursively(firstEnergy,secondEnergy:real;
                               secondNumNodes:integer;
                               soundOn:Boolean;
                               VAR X,V,psi:Dvector;
                               VAR spectrum:eValList);
   VAR
      firstNumNodes,n: integer;
      newEnergy,eigenEnergy: real;
   BEGIN
      SolveWaveEquation({using} firstEnergy,X,V, {giving} psi);
      firstNumNodes := NumberOfNodes({using} firstEnergy,psi);

      CASE abs(firstNumNodes-secondNumNodes) OF
       0 : {no eigenvalues in this range}
           EXIT;
       1 : {one eigenvalue in here, go and find it}
           BEGIN
              FuncFastYValue := FastYValue;     {Set up for the search}
              FastSearchForZero(firstEnergy,secondEnergy,eigenEnergy);
              ShowTheEnergyLevel(eigenEnergy);
              IF firstNumNodes<secondNumNodes
                 THEN n := secondNumNodes
                 ELSE n := firstNumNodes;
              Message(concat('    Level number ',
                             Numstr(n,2,0),
                             '   :   Binding energy = ',
                             Numstr(eigenEnergy,7,2),
                             ' eV'));
              spectrum.eigenVals[n] := eigenEnergy;
              SoundNote(eigenEnergy,soundOn);
              EXIT;
           END;
       ELSE {more than one eigenvalue here, subdivide further};
           BEGIN
              newEnergy := (firstEnergy+secondEnergy)/2;
              IF abs(newEnergy-firstEnergy)<macheps THEN
               { levels are degenerate within accuracy of the computer}
                 BEGIN
                    EXIT
                 END;
              FindLevelRecursively(newEnergy,firstEnergy,firstNumNodes,
                                   soundOn,X,V,psi, {adding to} spectrum);
              FindLevelRecursively(newEnergy,secondEnergy,secondNumNodes,
                                   soundOn,X,V,psi, {adding to} spectrum);
           END;
      END;    {case}
   END;

PROCEDURE FindAllEigenvalues(soundOn:Boolean; VAR X,V,psi:Dvector;
                             VAR spectrum:eValList;
                             VAR spectrumFound:Boolean);
   VAR
      lowEnergy,highEnergy: real;
      lowNumNodes,highNumNodes: integer;
   BEGIN
      highEnergy := abs(lowestV);
      highNumNodes := 0;
      lowEnergy  := 0;
      FindLevelRecursively({using} lowEnergy,highEnergy,highNumNodes,
                           {using} soundOn,X,V, {making} psi,
                           {completing} spectrum);
      Message('');
      spectrumFound := true;    {signifies you can ask to see the eigenfunctions}
   END;

PROCEDURE PrepareToDisplayWaveFunctions(VAR n:integer; VAR examine:Boolean);
   BEGIN
      DrawThePotentialWellAsBackground(X,V,examine);
      n := 1;
   END;

PROCEDURE LabelTheGraph(probFlag:Boolean);
   BEGIN
      SelectViewport(2);
      SetColor(psiColor);
      IF probFlag THEN
         BEGIN
            PutLabel(top,'wave function                          ');
            SetColor(probColor);
            PutLabel(top,'                    probability density');
         END
      ELSE
         PutLabel(top,'wave function');
   END;

PROCEDURE InputALevelNumber(VAR spectrum:eValList;
                            VAR n:Integer;
                            VAR finisht,examine:Boolean);
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
                  finisht := false;
                  REPEAT
                     AcceptScreen;
                     finisht := false;
                     numFound := false;
                     IF GetBoolean(2) THEN
                        PrepareToDisplayWaveFunctions({setting} n,examine);
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


PROCEDURE DisplayEigenFunction(spectrum:eValList; n:integer;
                               VAR X,V,psi,prob:Dvector);
   VAR
      energy,temp:real;
      i: integer;
   BEGIN
      WITH spectrum DO
         BEGIN
            IF n>numLevels THEN EXIT;
            energy := eigenVals[n];
            ShowTheEnergyLevel({using} energy);
            SolveWaveEquation({using} energy,X,V, {giving} psi);
            NormalizeTheWaveFunction({using} psi);
            CalculateProbabilityFunction({using} psi, {giving} prob);
            SetColor(psiColor);
            DrawWaveFunctionOnEnergyLevel({using} energy,X,psi);
            Message(concat('    Level number ',
                           Numstr(n,2,0),
                           '   :   Binding energy = ',
                           Numstr(energy,7,2),
                           ' eV'));
         END;    {with}
   END;

PROCEDURE ExploreThisPotential(VAR colNum,rowNum:Integer;
                               VAR first,examine,spectrumFound:Boolean);
   VAR
      i,exitcode     : Integer;
      withMouse,zoom : Boolean;
      erase,finisht  : Boolean;
      showProb       : Boolean;
   BEGIN
      CASE colNum OF
         3 : {Choose a well to work with}
             BEGIN
                ChooseAPotentialWell({using} rowNum,
                                 {giving} thisWell,V,spectrum,spectrumFound);
                SetPotentialPlottingScale({using} V);
                DrawThePotentialWellAsBackground({using} X,V,examine);
             END;
         4 : {Change the well parameters}
             BEGIN
                CASE rowNum OF
                 1 : MakeAnyChanges({changing} thisWell);
                 2 : ChoosePerturbation({using} thisWell,
                                        {giving} thisPerturbn);
                END;       {case rowNum}
                CalculateWellDVector({using} thisWell,
                                     {giving} V,spectrumFound);
                AddPerturbationToDVector({using} thisPerturbn, {giving} V);
                DrawThePotentialWellAsBackground({using} X,V,examine);
             END;
         5 : {Show method of solution}
             BEGIN
                first := true;
                CASE rowNum OF
                  1..2 : {Solve for a particular energy}
                      BEGIN
                         zoom := false;
                         DrawThePotentialWellAsBackground(X,V,examine);
                         ChooseMeansOfInput({using} rowNum,
                                            {giving} withMouse);
                         REPEAT
                            IF withMouse
                               THEN InputEnergyMouse
                                     ({giving} energy,erase,finisht,zoom)
                               ELSE InputEnergyKeybd
                                          ({giving} energy,erase,finisht);
                            IF NOT finisht THEN
                               IF NOT erase THEN
                                  BEGIN
                                     ShowTheEnergyLevel({using} energy);
                                     SolveWaveEquation({using} energy,X,V,
                                                       {giving} psi);
                                     DisplayEnergyAndNum({using} energy,psi);
                                     SetColor(psiColor);
                                     NormalizeTheWaveFunction({changing} psi);
                                     DrawWaveFunctionOnEnergyLevel
                                                     ({using} energy,X,psi);
                                     examine := true;
                                  END
                               ELSE
                                  ClearThePotentialPlot({using} X,V,examine);
                         UNTIL finisht;
                         SetPotentialPlottingScale({using} V);
                         ClearUpTheDisplay;
                         DrawThePotentialWellAsBackground({using} X,V,examine);
                      END;
                  3 : {Hunt for an eigenvalue}
                       BEGIN
                          PrepareToHunt;
                          FuncYValue := YValue;    {Set up for the search}
                          SearchForZero('asymptote (increasing exponential)',
                                        'energy', lowestV, energy);
                          IF NOT abortedSearch THEN
                             BEGIN
                                Announce
                                ('       Press <Enter> or click mouse to continue.      ');
                                NormalizeTheWaveFunction({changing} psi);
                                DisplayResultOfSearch
                                          ({using} energy,X,psi,examine);
                                examine := true;
                             END;
                          {NOTE: the display is NOT cleared}
                       END;
                  4 : {Examine solution}
                       BEGIN
                          ExamineGraphWithMouse({using} energy,X,psi);
                          examine := false;
                          ClearMUPPETport;
                          DrawThePotentialWellAsBackground(
                                                      {using} X,V,examine);
                       END;
                END;    {case rowNum}
             END;
         6 : {Calculate spectrum}
                BEGIN
                   examine := false;
                   first := true;
                   CASE rowNum OF
                     1 : {Calculate the eigenvalue spectrum}
                         BEGIN
                            PrepareToDisplaySpectrum(examine);
                            FindNumberOfEigenvalues({using} X,V,psi,
                                                    {giving} spectrum);
                            FindAllEigenvalues({using} soundOn,X,V,psi,
                                       {completing} spectrum,spectrumFound);
                         END;
                  2..3 : {Show individual eigenfunctions}
                         BEGIN
                            IF rowNum=3 THEN showProb := true
                                        ELSE showProb := false;
                            PrepareToDisplayWaveFunctions
                                                     ({setting} n,examine);
                            LabelTheGraph(showProb);
                            REPEAT
                               InputALevelNumber({using} spectrum,
                                                {giving} n,finisht,examine);
                               IF NOT finisht THEN
                                  BEGIN
                                     LabelTheGraph(showProb);
                                     DisplayEigenFunction({using}
                                                   spectrum,n,X,V,psi,prob);
                                     IF showProb THEN
                                        DisplayProbabilityFunction(
                                                 {using} spectrum,n,X,prob);
                                     examine := true;
                                     energy := spectrum.eigenVals[n];
                                  END;
                            UNTIL finisht;
                            Message('');
                         END;
                     4 : {examine the current wave function}
                         BEGIN
                            ExamineGraphWithMouse({using} energy,X,psi);
                            examine := false;
                            ClearMUPPETport;
                            DrawThePotentialWellAsBackground(
                                                         {using} X,V,examine);
                         END;
                     6 : {toggle the sound on/of facility}
                         IF soundOn THEN soundOn := false
                                    ELSE soundOn := true;
                   END;    {case rowNum}
                END;
         END;   {case colNum};
   END;

{ --------------- PROCEDURES ESPECIALLY FOR PART 2 ----------------- }

FUNCTION SpectrumHasBeenCalculated(spectrum:eValList) : Boolean;
   VAR
      thisScreen : TInputScreen;
   BEGIN
      IF spectrum.numLevels<1 THEN
         BEGIN
            OpenViewport(13);
            HideCursor;
            SetColor(white);
            SetFillStyle(SolidFill, blue);
            WITH Views[13] DO bar(0,0,Vx2-Vx1,Vy2-Vy1);
            WITH Views[13] DO rectangle(0,0,Vx2-Vx1,Vy2-Vy1);
            ShowCursor;
            beep;
            Print(2,2,concat('  You have changed the potential, but ',
                             'have not yet calculated the spectrum.'));
            Print(2,3,concat('  The rest of the program therefore has ',
                             'no functions to work with.  '));
            Print(2,4,concat('  Please go back and find the eigen ',
                             'functions of your potential.'));
            Print(2,6,'               Press <Enter> to return to Part 1.');
            Pause;
            SpectrumHasBeenCalculated := false;
         END
      ELSE
         SpectrumHasBeenCalculated := true;
   END;

PROCEDURE SetPart2DVectors(VAR thisOverlap:Overlap);
   BEGIN
      WITH thisOverlap DO
         BEGIN
            bra.init(numdata+1);
            ket.init(numdata+1);
            psi2.init(numdata+1);
            braket.init(numdata+1);
         END;
   END;

PROCEDURE UnsetPart2DVectors(VAR thisOverlap:Overlap);
   BEGIN
      WITH thisOverlap DO
         BEGIN
            bra.free;
            ket.free;
            psi2.free;
            braket.free;
         END;
   END;

PROCEDURE SetDefaultsForOverlaps(VAR thisOverlap:Overlap);
   VAR
      i : Integer;

   PROCEDURE SetStandardParams(VAR thisParams:PsiParam);
      VAR
         i : Integer;
      BEGIN
         WITH thisParams DO
            BEGIN
               quantNum := 1;
               coeffs[1] := 0.7071;
               coeffs[2] := 0.7071;
               FOR i:=3 TO maxNumLevels DO
                  coeffs[i] := 0;
            END;    {with}
      END;

   BEGIN
      WITH thisOverlap DO
         BEGIN
            energy := spectrum.eigenVals[1];
            SolveWaveEquation({using} energy,X,V, {giving} bra);
            NormalizeTheWaveFunction({changing} bra);
            operatorNum := 1;
            FOR i:=1 TO numData+1 DO
               ket.Put(i,bra.value(i));
            IF currentFileName='' THEN
               BEGIN
                  SetStandardParams(braParams);
                  SetStandardParams(ketParams);
                  ketParams.funcStr := 'H(x-0.03)*(1-H(x-0.09))';
               END;
            braDrawn := false;
            ketDrawn := false;
            psi1Name := 'psi1';
            psi2Name := 'psi2';
            n := 1;
         END;    {with}
   END;

{ ------------ GENERAL PROCEDURES FOR MENUS AND HOT-KEYS-------------- }

PROCEDURE PrepareToDrawBra(VAR X,V:DVector);
   BEGIN
      SetColor(dullColor);
      OpenColoredViewport(5);
      SelectScale(1);
      PlotDvectors(X,V,1,numData+1);
      Print(1,1,psi1Name);
   END;

PROCEDURE PrepareToDrawKet(VAR X,V:DVector);
   BEGIN
      SetColor(dullColor);
      OpenColoredViewport(6);
      SelectScale(1);
      PlotDvectors(X,V,1,numData+1);
      Print(1,1,concat(operators[thisOverlap.operatorNum],
                       '  * ',
                       psi2Name));
   END;

PROCEDURE PrepareToDrawBraKet(VAR X,V:DVector);
   BEGIN
      SetColor(dullColor);
      OpenColoredViewport(7);
      SelectScale(1);
      PlotDvectors(X,V,1,numData+1);
      Print(1,1,concat(psi1Name,
                       ' * ',
                       operators[thisOverlap.operatorNum],
                       ' * ',
                       psi2Name));
   END;

PROCEDURE PrepareToWriteResult;
   BEGIN
      SetColor(dullColor);
      OpenColoredViewport(8);
      Print(1,1,'Integration result');
   END;

PROCEDURE PrepareToStudyWaveFunctions(VAR X,V:Dvector);
   BEGIN
      SetColor(dullColor);
      ClearMUPPETport;
      PrepareToDrawBra(X,V);
      PrepareToDrawKet(X,V);
      PrepareToDrawBraKet(X,V);
      DefineScale(2,lowestX,highestX,-1.0,1.0);
      DefineScale(3,lowestX,highestX,-1.0,1.0);
      DefineScale(4,lowestX,highestX,-1.0,1.0);
   END;

PROCEDURE ChooseAnEigenFunction(n:Integer; spectrum:eValList;
                                VAR X,V,psi:Dvector;
                                VAR psiParams:PsiParam;
                                VAR psiName:String;
                                VAR escape:Boolean);
   BEGIN
      energy := spectrum.eigenVals[n];
      SolveWaveEquation({using} energy,X,V, {giving} psi);
      NormalizeTheWaveFunction({changing} psi);
      psiParams.quantNum := n;
      psiName := Concat('u',Numstr(n,1,0));
      escape := false;
   END;

PROCEDURE ChooseACompoundState(spectrum:eValList;
                               VAR R,V,chi:Dvector;
                               VAR psiParams:PsiParam;
                               VAR escape:boolean);
   VAR
      i,j,n       : Integer;
      amp         : ARRAY[1..maxNumLevels] OF Real;
      total,dummy : Real;
      thisScreen  : TInputScreen;
      numLines    : Integer;
      temp        : DVector;
   BEGIN
      temp.init(numData+1);
      Message('');
      n := spectrum.numLevels;
      IF n>maxNumLevels THEN n := maxNumLevels;
    { Note: maxNumLevels eigenstates is all the screen has room for }
      numLines := 6 + 2*n;
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputPort(0.69,0.99, 0.07,0.92);
            LoadLine('Input a coefficient');
            LoadLine('for each eigenstate');
            LoadLine('(real numbers only).');
            LoadLine(' None must be >1.');
            LoadLine('');
            FOR i:=1 TO n DO
               BEGIN
                  LoadLine(concat('',
                                  numStr(i,1,0),
                                  ': coeff = {       }'));
                  SetNumber(i,psiParams.coeffs[i]);
                  SetNumberLimits(i,-1,1);
               END;
            LoadLine('[Cancel] to abort.');
            LoadLine('[  Ok  ] if satisfied.');
            REPEAT
               AcceptScreen;
               total := 0;
               IF Canceled
                  THEN
                     escape := true
                  ELSE
                     BEGIN
                        escape := false;
                        FOR i:=1 TO n DO
                           BEGIN
                              amp[i] := GetNumber(i);  {read an amplitude}
                              total := total + amp[i]*amp[i]
                           END;
                     END;
            UNTIL escape OR (total>0);
            IF NOT escape THEN
               BEGIN
                  FOR i:=1 TO numdata+1 DO
                     chi.Put(i,0);
                  FOR j:=1 TO n DO
                     BEGIN
                        psiParams.coeffs[j] := amp[j];
                        energy := spectrum.eigenVals[j];
                        SolveWaveEquation({using} energy, X,V,
                                          {giving} temp);
                        NormalizeTheWaveFunction({changing} temp);
                        IF abs(amp[j])>macheps THEN
                           FOR i:=1 TO numdata+1 DO
                              chi.Put(i,(chi.value(i) + amp[j]*temp.value(i)));
                     END;
               END;
            temp.free;
            Done;
         END;    {with thisScreen}
   END;

PROCEDURE InputFuncString (VAR fString:string; VAR escape:Boolean);
   VAR
      thisScreen          : TInputScreen;
      i,j,len          : Integer;
      tempStr,lastChar : String;
   BEGIN
      len := length(fString);
      tempStr := fString;
      i := len;                        {remove trailing spaces}
      lastChar := copy(tempStr,i,1);
      WHILE (i>0) AND (lastChar=' ') DO
         BEGIN
            Delete(tempStr,i,1);
            i := i-1;
            lastChar := copy(tempStr,i,1);
         END;
      IF i<50 THEN                   {now add trailing '@'}
         FOR j:=1 TO 50-i DO
            tempStr := concat(tempStr,' ');
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputPort (0.05, 0.95, 0.36, 0.67);
            LoadLine('                   Input a FUNCTION   ');
            LoadLine('  You can use: ^, abs, sqr, sqrt, ln, exp, pi, H(unit step fn),');
            LoadLine('  all trigonometric/hyperbolic functions and their inverses.');
            LoadLine('');
            LoadLine(concat('      f(r) = "',tempStr,'"'));
            LoadLine('');
            LoadLine('                 [  Ok  ] to accept,     [Cancel] to abort.');
            AcceptScreen;
            escape := Canceled;
            IF NOT escape THEN
               fString := GetString(1);
            Done;
         END;    {with thisScreen}
   END;

PROCEDURE ChooseSomeOtherFunction(VAR X,psi2:Dvector;
                                  VAR psiParams:PsiParam;
                                  VAR psiName:String;
                                  VAR escape:boolean);
   VAR
      Fstring  : String;
      success  : Boolean;
      myParser : Tparser;
      i        : Integer;
   BEGIN
      myParser.init;
      Fstring := psiParams.funcStr;
      REPEAT
         InputFuncString(Fstring,escape);
         IF NOT escape THEN
            BEGIN
               success := myParser.Parse('x','y','z','t',Fstring);
               IF success THEN
                  BEGIN
                     FOR i:=1 TO numData+1 DO
                        psi2.Put(i,myParser.F(X.value(i),0,0,0));
                     psiParams.funcStr := Fstring;
                     psiName := 'function';
                  END
               ELSE
                  BEGIN
                     Message('Error: Cannot evaluate this expression');
                     Beep;
                  END;
            END;
      UNTIL escape OR success;
   END;

PROCEDURE DrawTheBraFunction(VAR X,V,bra:Dvector);
   BEGIN
      PrepareToDrawBra(X,V);
      SemiAutoScale(2,bra);
      SetColor(braColor);
      plotDvectors(X,bra,1,numData+1);
   END;

PROCEDURE DrawTheKetFunction(VAR X,V,ket:Dvector);
   BEGIN
      PrepareToDrawKet(X,V);
      SemiAutoScale(3,ket);
      SetColor(ketColor);
      plotDvectors(X,ket,1,numData+1);
   END;

PROCEDURE FormAndDisplayProduct(VAR X,bra,ket,braket:Dvector);
   VAR
      i: integer;
   BEGIN
      FOR i:=1 TO numdata+1 DO
         braket.Put(i,bra.value(i)*ket.value(i));
      SetColor(dullColor);
      PrepareToDrawBraket(X,V);
      SemiAutoScale(4,braket);
      SetColor(prodColor);
      PlotDvectors(X,braket,1,numData+1);
      PrepareToWriteResult;
   END;

PROCEDURE WriteArea(area,areaOld:real);
   BEGIN
      SelectViewport(8);
      SetColor(black);
      Print(8,3,numStr(areaOld,5,4));
      SetColor(prodColor);
      Print(8,3,numStr(area,5,4));
   END;

PROCEDURE IntegrateAndDisplay(VAR X,braket:Dvector; VAR area:real);
   VAR
      i: integer;
      areaOld: real;
   BEGIN
      PrepareToWriteResult;
      area :=  EndCorrection(braket.value(1),braket.value(2));
      FOR i:=2 TO numdata+1 DO
         BEGIN
            areaOld := area;
            area := area + (braket.value(i)+braket.value(i-1))*deltaX/2;
            SelectViewport(7);
            SetColor(intgColor);
            DrawBar(X.value(i),(braket.value(i)+braket.value(i-1))/2);
            WriteArea(area,areaOld);
         END;
      areaOld := area;
      area :=  area + EndCorrection(braket.value(numdata+1),braket.value(numdata));
      WriteArea(area,areaOld);
   END;

FUNCTION BeforeFirstValue(VAR psi:DVector):Real;
  {It is assumed that psi behaves like exp(+kx) at Xmin}
   BEGIN
      IF psi.Value(2)=0
         THEN BeforeFirstValue := 0
         ELSE BeforeFirstValue := psi.Value(1)*psi.Value(1)/psi.Value(2);
   END;

FUNCTION AfterLastValue(VAR psi:DVector):Real;
  {It is assumed that psi behaves like exp(-kx) at Xmax}
   BEGIN
      IF psi.Value(numData)=0
         THEN AfterLastValue := 0
         ELSE AfterLastValue := psi.Value(numData+1)
                               *psi.Value(numData+1)/psi.Value(numData);
   END;

PROCEDURE CalculateFirstDeriv(VAR psi2,temp:DVector);
   VAR
      i : Integer;
      y : Real;
   BEGIN
      WITH psi2 DO
         BEGIN
            temp.Put(1,(value(2)-beforeFirstValue(psi2)));
            FOR i:=2 TO numdata DO
                     temp.Put(i,(value(i+1)-value(i-1)));
           {For final value assume psi2 goes as exp(-kx)}
            IF abs(value(numData+1))>macheps
               THEN y := abs(value(numData+1)/value(numData))
               ELSE y := 0;
            temp.Put(numData+1,temp.value(numData)*y);
         END;   {with}
   END;

PROCEDURE CalculateSecondDeriv(VAR psi2,temp:DVector);
   VAR
      i : Integer;
      y : Real;
   BEGIN
      WITH psi2 DO
         BEGIN
            temp.Put(1,(value(2)-2*value(1)+beforeFirstValue(psi2)));
            FOR i:=2 TO numdata DO
               temp.Put(i,(value(i+1)-2*value(i)+value(i-1)));
           {For final value assume psi2 goes as exp(-kx)}
            IF value(numData+1)<>0
               THEN y := abs(value(numData+1)/value(numData))
               ELSE y := 0;
            temp.Put(numData+1,temp.value(numData)*y);
         END;   {with}
   END;

PROCEDURE OperateToFormKetFunction(opNum:integer; VAR X,V,psi2,ket:Dvector);
   VAR
      i    : Integer;
      temp : DVector;
      k    : Real;
   BEGIN
      temp.Init(numData+1);
      CASE opNum OF
       1 : {1}
           FOR i:=1 TO numData+1 DO
              ket.Put(i,psi2.value(i));
       2 : {x}
            FOR i:=1 TO numdata+1 DO
                 ket.Put(i,(X.value(i)*psi2.value(i)));
       3 : {d/dx}
            BEGIN
               CalculateFirstDeriv(psi2,temp);
               FOR i:=1 TO numData+1 DO
                  ket.Put(i,temp.value(i)/(2*deltaX));
            END;
       4 : {x^2}
            FOR i:=1 TO numdata+1 DO
                 ket.Put(i,(X.Value(i)*X.value(i)*psi2.value(i)));
       5 : {d^2/dx^2}
            BEGIN
               CalculateSecondDeriv(psi2,temp);
               FOR i:=1 TO numData+1 DO
                    ket.Put(i,temp.value(i)/deltaX/deltaX);
            END;
       6 : {V(x)}
           BEGIN
              FOR i:=1 TO numdata+1 DO
                 ket.Put(i,(V.value(i)*psi2.value(i)));
           END;
       7 : {total energy}
           BEGIN
              CalculateSecondDeriv(psi2,temp);
              FOR i:=1 TO numData+1 DO
                 ket.Put(i,V.value(i)*psi2.value(i)
                          -temp.value(i)/deltaX/deltaX/energyConvert);
           END;
       8 : {x *  d/dx}
           BEGIN
              OperateToFormKetFunction(3, X,V,psi2,temp);
              OperateToFormKetFunction(2, X,V,temp,ket);
           END;
       9 : {d/dx * x}
           BEGIN
              OperateToFormKetFunction(2, X,V,psi2,temp);
              OperateToFormKetFunction(3, X,V,temp,ket);
           END;
      10  : {user defined}
           BEGIN
              ConstructKetUser1(X,V,psi2,ket);
           END;
      11  : {user defined}
           BEGIN
              ConstructKetUser2(X,V,psi2,ket);
           END;
      END;    {case}
      temp.free;
   END;


{------ THIS IS THE MAIN PART OF THE PROGRAM FOR PART 2 ---------}


PROCEDURE CalculateOverlapIntegrals(cNum,rNum:Integer;
                                    VAR first:Boolean;
                                    VAR thisOverlap:Overlap);
   BEGIN
      WITH thisOverlap DO
         BEGIN
            IF first THEN
               BEGIN
                  first := false;
               END;

         CASE cNum OF
            3 : {specify psi1}
                BEGIN
                   IF (rNum>0) AND (rNum<=spectrum.numLevels) THEN
                      ChooseAnEigenFunction(rNum,spectrum,X,V,bra,
                                            braParams,psi1Name,escape);
                   IF rNum=spectrum.numLevels+2 THEN
                      BEGIN
                         ChooseACompoundState(spectrum,X,V,bra,
                                              braParams,escape);
                         psi1Name := 'psi1';
                      END;
                   IF NOT escape
                      THEN
                         BEGIN
                            DrawTheBraFunction({using} X,V,bra);
                            braDrawn := true;
                            IF ketDrawn THEN
                               FormAndDisplayProduct({using} X,bra,ket,
                                                     {giving} braket);
                         END;
                END;
            4 : {specify operator}
                BEGIN
                   operatorNum := rNum;
                   IF ketDrawn THEN
                      BEGIN
                         OperateToFormKetFunction({using} operatorNum,
                                         X,V,psi2,{giving} ket);
                         DrawTheKetFunction({using} X,V,ket);
                         IF braDrawn THEN
                            FormAndDisplayProduct({using} X,bra,ket,
                                                  {giving} braket);
                      END
                   ELSE
                      PrepareToDrawKet(X,V);
                END;
            5 : {specify psi2}
                BEGIN
                   IF (rNum>0) AND (rNum<=spectrum.numLevels) THEN
                      ChooseAnEigenFunction(rNum,spectrum,X,V,psi2,
                                            ketParams,psi2Name,escape);
                   IF rNum=spectrum.numLevels+2 THEN
                      BEGIN
                         ChooseACompoundState(spectrum,X,V,psi2,
                                              ketParams,escape);
                         psi2Name := 'psi2';
                      END;
                   IF rNum=spectrum.numLevels+3 THEN
                      ChooseSomeOtherFunction(X,psi2,
                                              ketParams,psi2Name,escape);
                   IF NOT (escape)
                      THEN
                         BEGIN
                            OperateToFormKetFunction({using} operatorNum,
                                                  X,V,psi2,{giving} ket);
                            DrawTheKetFunction({using} X,V,ket);
                            ketDrawn := true;
                            IF braDrawn THEN
                               FormAndDisplayProduct({using} X,bra,ket,
                                                     {giving} braket);
                         END;
                END;
            6 : {perform the overlap integration}
                BEGIN
                   IF braDrawn AND ketDrawn THEN
                      IntegrateAndDisplay({using} X,braket,{giving} area);
                END;
         END;   {case cNum};

         END;    {with this overlap}
   END;

{ --------------- PROCEDURES ESPECIALLY FOR PART 3 ----------------- }

{ ------------ GENERAL PROCEDURES FOR MENUS AND HOT-KEYS-------------- }

PROCEDURE SetUpDevelopKeys(VAR developKeys:Thotkeys);
   BEGIN
      WITH developKeys DO
         BEGIN
            Init(6);
            key[1] := 'F2-Stop';
            key[2] := 'F4-Reverse';
            key[3] := 'F5-Slower';
            key[4] := 'F6-Faster';
            key[5] := 'F7-Restart';
            key[6] := 'F10-Menu';
         END;   {with}
   END;

{ ------------ PROCEDURES FOR COLOR CODING (from Hiller) -------------- }

FUNCTION ColorWheel(val:Real; VAR numColors:Integer) : Word;
  {Convert val to color on color wheel.}
   CONST
      range = 7;
      wheel : ARRAY [0..range] OF Word
                    = (lightred, yellow, lightgreen, lightcyan,
                       lightblue, blue, magenta, lightmagenta);
   VAR
      index : Integer;
   BEGIN
      numColors := range+1;
      val := frac(val/(2*Pi)+0.0625);
      IF val<0.0
         THEN val := val+1.0;
      index := trunc(numColors*val);
      IF index=numColors
         THEN index:=0;
      ColorWheel := Wheel[index]
   END;      {ColorWheel}

PROCEDURE DisplayLegendColorWheel;
{Display legend for plot of wave function.}
   VAR
      numColors,n,sx,sy,temp : Integer;
      color : Word;
      delta : Real;
   BEGIN
      OpenHiddenViewport(11);
      Color := ColorWheel(0.0,numColors);
      delta := 180.0/NumColors;
      DefineScale(8,0,1,0,1);
      SelectScale(8);
      Map(0.49,0.475,sx,sy);
      FOR n:=1 TO 2*numColors DO
         BEGIN
            Color := ColorWheel(2*Pi*(n-1)*delta/360,temp);
            SetFillStyle(SolidFill,Color);
            SetColor(Color);
            PieSlice(sx,sy,round((n-1)*delta),round(n*delta),35)
         END;
      SetColor(dullColor);
      Print(1,2,'phase:');
      Print(13,4,'-0')
   END;

{ ------------ GENERAL PROCEDURES -------------- }

PROCEDURE SetDefaultCoefficients(VAR cxAmps:AmpsType);
   VAR
      i : Integer;
   BEGIN
      IF currentFileName='' THEN
         BEGIN
            FOR i:=1 TO maxNumLevels DO
               BEGIN
                  cxAmps.Re[i] := 0;
                  cxAmps.Im[i] := 0;
               END;
(*          { Set default at equal mixture of ground and first excited states }
            cxAmps.Re[1] := 0.7171;
            cxAmps.Re[2] := 0.7171;
*)
          { Set default at the ground state }
            cxAmps.Re[1] := 1.0000;
            n := spectrum.numLevels;
         END;
   END;

PROCEDURE SetInitialTimes(VAR t,dt:Real);
 { Note all times are in fs }
   BEGIN
      t  := 0;
      dt := 0.001;
   END;

PROCEDURE SetPart3Vectors(VAR eigFns:fnArray; VAR cxPsi:cxVector;
                          VAR realFlag:Boolean);
   VAR
      i:integer;
   BEGIN
      FOR i:=1 TO maxNumLevels DO eigFns[i].init(numdata+1);
      cxPsi.Re.init(numdata+1);
      cxPsi.Im.init(numdata+1);
      realFlag := false;  {default display method is amp/phase}
   END;

PROCEDURE UnsetPart3Vectors(VAR eigFns:fnArray; VAR cxPsi:cxVector);
   VAR
      i:integer;
   BEGIN
      FOR i:=1 TO maxNumLevels DO eigFns[i].free;
      cxPsi.Re.free;
      cxPsi.Im.free;
   END;

PROCEDURE SelectCodedColor(argt:real);
   VAR
      n : integer;
   BEGIN
      SetColor(colorWheel(argt,n));
   END;

PROCEDURE RubOutBar(x,y1,y2:real);
   BEGIN
      SetColor(GraphBackColor);
      SetLineStyle(0,0,thickwidth);
      IF y2<y1 THEN
         PlotLine(x-deltaX/2,y2, x-deltaX/2,y1);
      SetLineStyle(0,0,normwidth);
   END;

PROCEDURE CalculateCompoundState(n:integer; cxAmps:ampsType;
                                 VAR eigFns:fnArray; VAR cxPsi:cxVector);
   VAR
      i,j:integer;
   BEGIN
      FOR j:=1 TO numdata+1 DO
         BEGIN
            cxPsi.Re.Put(j,0);
            cxPsi.Im.Put(j,0);
         END;
      FOR i:=1 TO n DO
         BEGIN
            IF abs(CxAmps.Re[i])>macheps THEN
               FOR j:=1 TO numdata+1 DO
                  cxPsi.Re.Put(j, cxPsi.Re.value(j) +
                                  CxAmps.Re[i]*eigFns[i].value(j));
            IF abs(CxAmps.Im[i])>macheps THEN
               FOR j:=1 TO numdata+1 DO
                  cxPsi.Im.Put(j, cxPsi.Im.value(j) +
                                  CxAmps.Im[i]*eigFns[i].value(j));
         END;
   END;

PROCEDURE SetUpPlottingScales(VAR X,V,prob:DVector; VAR cxPsi:cxVector);
   VAR
      i : Integer;
      z : complex;
   BEGIN
      FOR i:=1 TO numdata+1 DO
         BEGIN
            z.Re := cxPsi.Re.value(i);
            z.Im := cxPsi.Im.value(i);
            prob.Put(i,cabs(z));
         END;
      SemiAutoScale(4,prob);                {for plotting amplitudes}
      DefineScale(5,lowestX,highestX,       {for plotting real and imaginary}
                    -graphScales[4].Ymax,graphScales[4].Ymax);
   END;

PROCEDURE DisplayCompoundState(vnum:integer; VAR X,V,psi,prob:Dvector;
                               VAR cxPsi:cxVector);
   VAR
      i: integer;
      z: complex;
   BEGIN
      SelectViewport(vnum);
      FOR i:=1 TO numdata+1 DO
         BEGIN
            z.Re := cxPsi.Re.value(i);
            z.Im := cxPsi.Im.value(i);
            prob.Put(i,cabs(z));
            psi.Put(i,arg(z));
         END;
      FOR i:=1 TO numdata+1 DO
         BEGIN
            IF i>1 THEN
               IF realFlag THEN
                  BEGIN
                     SelectScale(5);
                     PlotLine(lowestX,0, highestX,0);
                     SetColor(realColor);
                     PlotLine(X.value(i-1),cxPsi.Re.value(i-1),
                                 X.value(i),cxPsi.Re.value(i));
                     SetColor(imagColor);
                     PlotLine(X.value(i-1),cxPsi.Im.value(i-1),
                                 X.value(i),cxPsi.Im.value(i));
                  END
               ELSE
                  BEGIN
                     SelectScale(4);
                     SelectCodedColor(psi.value(i));
                     DrawBar(X.value(i),prob.value(i));
                  END;
         END;
   END;

PROCEDURE ShowTime(t:real; color:integer);
   BEGIN
      SelectViewport(10);
      RubOut(1,3,50,GraphBackcolor);
      RubOut(1,4,50,GraphBackcolor);
      RubOut(1,5,50,GraphBackcolor);
      SetColor(dullColor);
      Print(1,1,'Time =           (fs)');
      SetColor(color);
      Print(8,1,numstr(t,8,4));
   END;

PROCEDURE PrepareForEvolution(t:Real);
   BEGIN
      ClearMUPPETport;
      SetColor(dullColor);
      OpenColoredViewport(9);
      OpenHiddenViewport(10);
      ShowTime(t,white);
   END;

PROCEDURE SetUpForEvolution(t:Real; VAR X,V,prob:Dvector;
                                    VAR xcPsi:cxVector);
   BEGIN
      PrepareForEvolution(t);
      DisplayCompoundState(9, {using} X,V, {giving} psi,prob,cxPsi);
      SelectScale(1);
      SetColor(potlColor2);
      PlotDvectors(X,V,1,numData+1);
      IF realFlag THEN
         BEGIN
            SelectViewport(9);
            SetColor(realColor);
            PutLabel(top,'Real part of psi                        ');
            SetColor(imagColor);
            PutLabel(top,'                   Imaginary part of psi');
         END
      ELSE
         BEGIN
            DisplayLegendColorWheel;
            SelectViewport(9);
            SetColor(dullColor);
            PutLabel(top,'Amplitude of wave function');
         END;
   END;

PROCEDURE ChooseCompoundState(spectrum:eValList; VAR X,V:Dvector;
                              VAR n:Integer; VAR cxAmps:AmpsType;
                              VAR cxPsi:cxVector; VAR escape:Boolean);
   VAR
      i,j         : Integer;
      thisScreen  : TInputScreen;
      ampRe,ampIm : Real;
      numLines    : Integer;
      found       : Boolean;
      thisCxAmps  : AmpsType;
   BEGIN
      ClearMUPPETport;
      thisCxAmps := cxAmps;
      n := spectrum.numLevels;
      IF n>maxNumLevels THEN n := maxNumLevels;
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputport(0.00,1.00,  0.01,0.99);
          { Note: maxNumLevels eigenstates is all the screen has room for }
            numLines := 6 + 2*n;
            LoadLine(concat('   Construct a general state, at time t=0, ',
                            'by assigning coefficients to the'));
            LoadLine(concat('  ',numStr(spectrum.numLevels,2,0),
                            ' energy eigenstates.  ',
                            'Note: this time you may use complex coefficients.'));
            LoadLine('');
            FOR i:=1 TO n DO
               BEGIN
                  LoadLine(concat('    ',numStr(i,1,0),
                                  ':     Coefficient:',
                                  '     (Re) = {        }',
                                  '     (Im) = {        }'));
                  SetNumber(2*i-1,thisCxAmps.Re[i]);
                  SetNumber(2*i,  thisCxAmps.Im[i]);
                  SetNumberLimits(2*i-1,-1,1);
                  SetNumberLimits(2*i,  -1,1);
               END;
            LoadLine('');
            LoadLine('');
            LoadLine('  Select <  Ok  > when you are satisfied and want the program to proceed.');
            LoadLine('         <Cancel> if you want to exit without having made any changes.');
            LoadLine('         < View > to see the effect of the changes before proceeding.');
            LoadLine('');
            LoadLine('                       [  Ok  ]     [Cancel]     [ View ]');
            REPEAT
               AcceptScreen;
               IF Canceled
                  THEN escape := true
                  ELSE
                     BEGIN
                        escape := false;
                        FOR i:=1 TO n DO
                           BEGIN
                              thisCxAmps.Re[i] := GetNumber(2*i-1);
                              thisCxAmps.Im[i] := GetNumber(2*i);
                           END;
                        IF GetBoolean(2*n+3)
                          THEN found := false
                          ELSE found := true;
                        CalculateCompoundState({using} n,thisCxAmps,eigFns,
                                               {giving} cxPsi);
                       {Selecting <View> means do not proceed but look at changes}
                        IF NOT found THEN
                           BEGIN
                              SetUpPlottingScales(X,V,prob,cxPsi);
                              SetUpForEvolution({using} 0,X,V,prob,cxPsi);
                              AskIfThisIsAcceptible(found);
                           END;
                     END;
            UNTIL found OR escape;
            IF NOT escape THEN
                  cxAmps := thisCxAmps;
            CalculateCompoundState({using} n,cxAmps,eigFns,
                                   {giving} cxPsi);
            Done;
         END;    {with ThisScreen}
   END;

PROCEDURE ShowPosition(VAR X:Dvector; VAR cxPsi:cxVector);
   VAR
      i         : Integer;
      probDens  : Real;
      area,norm : Real;
   BEGIN
      SelectViewport(10);
    { Calculate  and print <x> }
      area := 0;
      norm := 0;
      FOR i:=1 TO numdata+1 DO
         BEGIN
            WITH cxPsi DO
               probDens := sqr(Re.value(i)) + sqr(Im.value(i));
            area := area + probDens*X.value(i);
            norm := norm + probDens;
         END;
      IF norm>macheps THEN
         area := area/norm;
      SetColor(dullColor);
      Print(1,3, '  <x>    =            (nm)');
      SetColor(white);
      Print(12,3,numstr(area,9,5));
   END;

PROCEDURE ShowMomentum(VAR X:Dvector; VAR cxPsi:cxVector);
   VAR
      i         : Integer;
      integrand : Real;
      probdens  : Real;
      area,norm : Real;
      dPsi     : Complex;
   BEGIN
      SelectViewport(10);
    { Calculate  and print <p>.  It is equal to the integral of }
    {  -i* hbar * PsiStar * d(Psi)/dx.  This should be real.    }
      WITH cxPsi DO
         BEGIN
            area := 0;
            norm := 0;
            FOR i:=2 TO numdata DO
               BEGIN
                  dPsi.Re := Re.value(i+1) - Re.Value(i-1);
                  dPsi.Im := Im.value(i+1) - Im.Value(i-1);
                { Correct value of dPsi/dx is this over 2*deltaX }
                  area := area + (Re.value(i)*dPsi.Im)
                               - (Im.value(i)*dPsi.Re);
                { No point in calculating the real part }
                  norm := norm + sqr(Re.value(i)) + sqr(Im.value(i));
               END;
         END;
      IF norm>macheps THEN
         area := area/norm/2/deltaX;
      SetColor(dullColor);
      Print(1,4, '-i<d/dx> =            (nm^-1)');
      SetColor(white);
      Print(13,4,numstr(area,8,2));
   END;

PROCEDURE ShowQuantity(quantity:Real);
   BEGIN
      SelectViewport(10);
    { Print the user calculated quantity }
      SetColor(dullColor);
      Print(1,5, concat(userItem6,
                        '  =           '));
      SetColor(white);
      Print(19,5,numstr(quantity,6,5));
   END;

PROCEDURE IncrementAndShowTime(dt:real; VAR t:real);
   BEGIN
      ShowTime(t,graphBackcolor);
      t := t + dt;
      ShowTime(t,white);
   END;

PROCEDURE InputNewTime(VAR t,dt:real);
   VAR
      thisScreen : TInputScreen;
   BEGIN
      ShowTime(t,GraphBackColor);
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputport(0.10,0.90, 0.20,0.40);
            LoadLine(concat('Choose new values:   time = {      }',
                            '   time step = {      }'));
            LoadLine('');
            LoadLine('              [  Ok  ]               [Cancel]');
            SetNumber(1,t);
            SetNumberLimits(1, -1000, 1000);
            SetNumber(2,dt);
            SetNumberLimits(1, -1000, 1000);
            AcceptScreen;
            IF NOT Canceled THEN
               BEGIN
                  t  := GetNumber(1);
                  dt := GetNumber(2);
               END;
            Done;
         END;    {with thisScreen}
      ShowTime(t,white);
   END;

PROCEDURE EvolveAmplitudes(t:real; spectrum:eValList; n:integer;
                           cxAmps:ampstype; VAR thisAmps:ampsType);
   VAR
      i: integer;
      sinwt,coswt: real;
   BEGIN
      FOR i:=1 TO n DO
         BEGIN
            sinwt := sin((1+spectrum.eigenVals[i]/timeConvert)*t);
            coswt := cos((1+spectrum.eigenVals[i]/timeConvert)*t);
            thisAmps.Re[i] := cxAmps.Re[i]*coswt - cxAmps.Im[i]*sinwt;
            thisAmps.Im[i] := cxAmps.Im[i]*coswt + cxAmps.Re[i]*sinwt;
         END;
   END;

PROCEDURE CalculateAndRedrawCompoundState(n:integer; thisAmps:ampstype;
                                     VAR eigFns:fnArray; VAR X,V:Dvector;
                                     VAR prob:Dvector; VAR cxPsi:cxVector);
   VAR
      i,j                : Integer;
      new,old            : Complex;
      newAbs,oldAbs,argt : Real;
      maxV,minV,maxPsi   : Real;
   BEGIN
      maxV := graphScales[1].Ymax;
      minV := graphScales[1].Ymin;
      maxPsi := graphScales[4].Ymax;
      SelectViewport(9);
      HideCursor;      {necessary so as not to slow down the animation}
      FOR j:=1 TO numdata+1 DO
         BEGIN
            new.Re := 0;
            new.Im := 0;
            FOR i:=1 TO n DO
               BEGIN
                  new.Re := new.Re + thisAmps.Re[i]*eigFns[i].value(j);
                  new.Im := new.Im + thisAmps.Im[i]*eigFns[i].value(j);
               END;
            newAbs  := cabs(new);
            argt := arg(new);
            IF j>1 THEN
               BEGIN
                  IF realFlag THEN
                     BEGIN
                        SelectScale(5);
                        SetColor(GraphBackColor);
                        PlotLine(X.value(j-1),cxPsi.Re.value(j-1),
                                 X.value(j),cxPsi.Re.value(j));
                        PlotLine(X.value(j-1),cxPsi.Im.value(j-1),
                                 X.value(j),cxPsi.Im.value(j));
                        SetColor(realColor);
                        PlotLine(X.value(j-1),old.Re, X.value(j),new.Re);
                        SetColor(imagColor);
                        PlotLine(X.value(j-1),old.Im, X.value(j),new.Im);
                     END
                  ELSE
                     BEGIN
                        SelectScale(4);
                        RubOutBar(X.value(j),prob.value(j),newAbs);
                        SelectCodedColor(argt);
                        DrawBar(X.value(j),newAbs);
                     END;

                  SetColor(dullColor);
                  IF realFlag THEN
                     IF j>2 THEN
                        PlotLine(X.value(j-2),0,X.value(j-1),0);
                  SelectScale(1);
                  IF j>2 THEN
                     PlotLine(X.value(j-2),V.value(j-2),
                              X.value(j-1),V.value(j-1));
                  PlotLine(X.value(j-1),maxV, X.value(j),maxV);
                  PlotLine(X.value(j-1),minV, X.value(j),minV);
                  cxPsi.Re.Put(j-1,old.Re);
                  cxPsi.Im.Put(j-1,old.Im);
                  prob.Put(j-1,oldAbs);
               END;
            old.Re := new.Re;
            old.Im := new.Im;
            oldAbs := newAbs;
         END;
      ShowCursor;
      cxPsi.Re.Put(numdata+1,new.Re);
      cxPsi.Im.Put(numdata+1,new.Im);
   END;

{------------------------------------------------------------------------}

PROCEDURE StartSystemDeveloping(VAR cNum,rNum:Integer;
                                VAR first,initFlag:Boolean;
                                VAR cxAmps:AmpsType; VAR t,dt:Real);
   VAR
      key       : Byte;
      startFlag : Boolean;

   PROCEDURE MakeKeyChoice(n:Integer; VAR t,dt:Real;
                           VAR startFlag:Boolean);
      BEGIN
         CASE n OF
          1 : {F2 = Run or stop}
              BEGIN
                 startFlag := NOT startFlag;
                 IF startFlag THEN
                    developKeys.key[1] := 'F2-Stop'
                 ELSE
                    developKeys.key[1] := 'F2-Run';
                 developKeys.Display;
              END;
          2 : {F4 = Reverse}
              BEGIN
                 dt := - dt;
                  IF dt>0 THEN
                     developKeys.key[2] := 'F4-Reverse'
                  ELSE
                     developKeys.key[2] := 'F4-Forward';
                  developKeys.Display;
              END;
          3 : {F5 = Slower}
               IF dt/2>macheps THEN
                  dt := dt / 2;
          4 : {F6 = Faster}
               dt := dt * 2;
          5 : {F7 = Restart at t=0}
               BEGIN
                  CalculateCompoundState({using} nAmps,cxAmps,
                                           eigFns, {giving} cxPsi);
                  t := 0;
                  SetUpForEvolution({using} t,X,V,prob,cxPsi);
                  developKeys.Display;
               END;
         END;   {case}
      END;

   BEGIN
         CASE cNum OF
            3 : {Work with wave function}
                BEGIN
                   CASE rNum of
                     1 : {Choose an initial state}
                         BEGIN
                            ChooseCompoundState({using} spectrum,X,V,
                                         {giving} nAmps,cxAmps,cxPsi,escape);
                            SetUpPlottingScales(X,V,prob,cxPsi);
                            SetUpForEvolution({using} t,X,V,prob,cxPsi);
                            first := true;
                            initFlag := false;
                         END;
                     2..3 : {Run the animation}
                         BEGIN
                            finished := false;
                            startFlag := true;
                            IF rNum=2 THEN   {begin over}
                               BEGIN
                                  ShowTime(t,graphBackcolor);
                                  SetInitialTimes({giving} t,dt);
                                  EvolveAmplitudes({using} t,spectrum,nAmps,
                                              cxAmps, {giving} thisAmps);
                                  CalculateAndRedrawCompoundState
                                    ({using} nAmps,thisAmps,eigFns,X,V,
                                     {giving} prob,cxPsi);
                               END;
                            SetUpDevelopKeys(developKeys);
                            developKeys.Display;
                            Message('');
                            REPEAT
                               IF startFlag THEN
                                  BEGIN
                                     IncrementAndShowTime({using} dt,
                                                          {giving} t);
                                     EvolveAmplitudes({using} t,spectrum,
                                                      nAmps,cxAmps,
                                                      {giving} thisAmps);
                                     CalculateAndRedrawCompoundState
                                           ({using} nAmps,thisAmps,eigFns,X,V,
                                            {giving} prob,cxPsi);
                                  END;
                               CheckForEvents;
                               IF developKeys.Pressed(key) THEN
                                  BEGIN
                                     IF key=6 THEN finished := true;
                                     MakeKeyChoice(key,t,dt,startFlag);
                                  END;
                            UNTIL finished;
                            developKeys.Clear;
                         END;
                   END;    {case}
                END;
            4 : {Explore the present state}
                BEGIN
                   IF NOT userFlag THEN
                      IF rNum=4 THEN
                         rNum := 5;
                   CASE rNum OF
                    1: {calculate position}
                        ShowPosition(X,cxPsi);
                    2: {calculate momentum}
                        ShowMomentum(X,cxPsi);
                    3: {User defined measurement}
                       ShowQuantity(MeasurementUser(X,cxPsi));
                    5: {Set the time}
                        BEGIN
                           InputNewTime(t,dt);
                           IF NOT initFlag THEN
                              BEGIN
                                 EvolveAmplitudes({using} t,spectrum,nAmps,
                                               cxAmps, {giving} thisAmps);
                                 CalculateAndRedrawCompoundState
                                      ({using} nAmps,thisAmps,eigFns,X,V,
                                       {giving} prob,cxPsi);
                              END;
                        END;
                   END;   {case}
                   SelectViewport(9);  {in case of change display}
                END;
            5 : {Set the mode of display}
                BEGIN
                   IF rNum=1 THEN realFlag := false
                             ELSE realFlag := true;
                   IF (NOT initFlag) AND (viewportNumber=9) THEN
                      SetUpForEvolution({using} t,X,V,prob,cxPsi);
                END;
         END;   {case cNum};
   END;

{ ------------------ FILE HANDLING PROCEDURES -------------------- }

{ ** These procedures save and read in global variables ** }

PROCEDURE SetUpOutputRecord(VAR thisFieldRecord:FieldRecord);
   BEGIN
      WITH thisFieldRecord DO
         BEGIN
            identifier   := fileIdentifier;
            fileWell     := thisWell;
            filePerturbn := thisPerturbn;
            fileSpectrum := spectrum;
            fileFound    := spectrumFound;
            fileBraParam := thisOverlap.braParams;
            fileKetParam := thisOverlap.ketParams;
            fileCxAmps   := initialCxAmps;
         END;
   END;

PROCEDURE InterpretInputRecord(thisFieldRecord:FieldRecord);
   BEGIN
      WITH thisFieldRecord DO
         BEGIN
            thisWell     := fileWell;
            thisPerturbn := filePerturbn;
            CalculateWellDvector(thisWell,V,spectrumFound);
            spectrum      := fileSpectrum;
            spectrumFound := fileFound;
            thisOverlap.braParams := fileBraParam;
            thisOverlap.ketParams := fileKetParam;
            initialCxAmps := fileCxAmps;
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
      dummy           : Boolean;
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
               thisFound := spectrumFound;
               CalculateWellDVector({using} thisWell,
                                    {giving} V,spectrumFound);
               SetPotentialPlottingScale({using} V);
               DrawThePotentialWellAsBackground(X,V,dummy);
               spectrumFound := thisFound;
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




{ ********* ALL USER WRITTEN PROCEDURES ARE HERE ********* }
{ *                                                      * }
{ * You may change or rewrite any of the following seven * }
{ * procedures.  Just follow the templates already coded * }
{ *                                                      * }
{ ******************************************************** }

PROCEDURE SetUserFlag;
   BEGIN
      userFlag := false;
   END;

PROCEDURE SetMenuItemsUser;
 { This puts names of user defined items in the menus.    }
 { Currently they have descriptions of the "user defined" }
 { procedures written below.  When you write your own,    }
 { you may, if you like, change these names accordingly.  }
   BEGIN
    { DEFAULT : THESE DO NOT HAVE SPECIAL USER MENU ITEMS }
      userItem1 := 'User defined';     {goes in WELL SHAPE menu}
      userItem2 := 'user1';            {goes in OPERATOR menu  }
      userItem3 := 'user2';            {and so does this       }
      userItem4 := 'User defined';     {goes in PERTURBATION   }
      userItem5 := 'User defined';     {goes in MEASURE menu   }
      userItem6 := 'user defined';     {goes in MEASURE result }
(*
    { ALTERNATIVE : THESE USER MENU ITEMS RELATE TO AUTHOR'S SUGGESTIONS }
      userItem1 := 'Triangular well';  {goes in WELL SHAPE menu}
      userItem2 := 'a.x+b.d/dx';       {goes in OPERATOR menu  }
      userItem3 := 'a.x-b.d/dx';       {and so does this       }
      userItem4 := 'A.abs(x)';         {goes in PERTURBATION   }
      userItem5 := '<x^2>';            {goes in MEASURE menu   }
      userItem6 := '<x^2>';            {goes in MEASURE result }
*)
   END;

{---------------- USER DEFINED POTENTIAL WELL -------------------}

PROCEDURE SetDefaultUser(VAR thisWell:wellParams);
 { This sets the depth and width of your well when it is first chosen }
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := 300;
            posn1 := 0.10;
            maxDepth := 600;
         END;
   END;

PROCEDURE SetScreenLinesUser(VAR thisWell:wellParams; VAR n:Integer;
                             VAR thisScreen:TInputScreen);
 { This writes information to the input screen.  If your well }
 { only needs two parameters, you won't have to change this.  }
   VAR
      thisMaxWidth : Real;
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('     Read in your parameter(s) here ',
                            '= {      }'));
            SetNumber(1,thisWell.depth1);
            SetNumberLimits(1, 0, thisWell.maxDepth);

            thisMaxWidth := highestX-lowestX;
            LoadLine(concat('                                    ',
                            '= {      }'));
            SetNumber(2,thisWell.posn1);
            SetNumberLimits(2,0,thisMaxWidth);
         END;    {with thisScreen}
         n := 2;                 {there are two items to be read in}
   END;

PROCEDURE ReadParamsUser(VAR thisScreen:TInputScreen;
                         VAR thisWell:wellParams);
 { This reads the parameters from the input screen.  If your well }
 { only needs two parameters you won't have to change this.       }
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := thisScreen.GetNumber(1);
            posn1  := thisScreen.GetNumber(2);
         END;
   END;

PROCEDURE CalculateDVectorUser(VAR thisWell:wellParams;
                               VAR V:dVector);
 { This calculates the potential DVector. }
 { Currently it is a triangular well.  }
   VAR
      i: integer;
   BEGIN
      Announce('This is a default potential.  Modify the code to get your own.');
      WITH thisWell DO
         BEGIN
            FOR i:=1 TO numData+1 DO
               BEGIN
                  V.Put(i,0);
                  IF abs(X.value(i))<=posn1 THEN
                     V.Put(i,-depth1*(1-abs(X.value(i))/posn1));
                  IF abs(X.value(i))>posn1  THEN V.Put(i,0);
               END;
         END;
   END;

{------------ USER DEFINED PERTURBATION ---------------}

FUNCTION PerturbationUser(a,x:Real):Real;
 { This calculates the change deltaV as a function of x. }
 { It must be defined in term of one parameter only: a.  }
 { Currently it is a linear perturbation.                    }
   BEGIN
      Announce('This is a default perturbation.  Modify the code to get your own.');
      PerturbationUser := a * abs(x);
   END;

{------------ USER DEFINED OPERATORS AND/OR KETS ---------------}

PROCEDURE ConstructKetUser1(VAR X,V,bra,ket:DVector);
 { This operates on the "bra" function to form the "ket".  }
 { Currently the operator is (x + const*d/dx) - which is a }
 { raising operator for harmonic oscillator wave functions. }
   VAR
      i           : integer;
      temp1,temp2 : DVector;
      constnt     : Real;
      sqrt2       : Real;
   BEGIN
      WITH thisWell DO
         constnt := sqrt(sqrt(depth1*energyConvert/posn1/posn1));
      sqrt2 := sqrt(2);
      temp1.init(numData+1);
      temp2.init(numData+1);
      OperateToFormKetFunction(2,X,V,bra,temp1);
    { This is the second menu choice, it multiplies by x }
      OperateToFormKetFunction(3,X,V,bra,temp2);
    { This is the third choice, it takes the first derivative }
      FOR i:=1 TO numdata+1 DO
         BEGIN
            ket.Put(i,(-constnt*temp1.value(i)+temp2.value(i)/constnt)/sqrt2);
         END;
      temp1.free;
      temp2.free;
   END;

PROCEDURE ConstructKetUser2(VAR X,V,bra,ket:DVector);
 { This operates on the "bra" function to form the "ket".  }
 { Currently the operator is (x - const*d/dx) - which is a }
 { lowering operator for harmonic oscillator wave functions. }
   VAR
      i           : integer;
      temp1,temp2 : DVector;
      constnt     : Real;
      sqrt2       : Real;
   BEGIN
      WITH thisWell DO
         constnt := sqrt(sqrt(depth1*energyConvert/posn1/posn1));
      sqrt2 := sqrt(2);
      temp1.init(numData+1);
      temp2.init(numData+1);
      OperateToFormKetFunction(2,X,V,bra,temp1);
    { This is the second menu choice, it multiplies by x }
      OperateToFormKetFunction(3,X,V,bra,temp2);
    { This is the third choice, it takes the first derivative }
      FOR i:=1 TO numdata+1 DO
         BEGIN
            ket.Put(i,(-constnt*temp1.value(i)-temp2.value(i)/constnt)/sqrt2);
         END;
      temp1.free;
      temp2.free;
   END;

{------------ USER DEFINED MEASUREMENT ---------------}

FUNCTION MeasurementUser(VAR X:DVector; VAR cxPsi:cxVector):Real;
 { This calculates some measureable quantity using Psi. }
 { Currently it is calculating <x^2>.                     }
   VAR
      area, norm : Real;
      probDens   : Real;
      i          : Integer;
   BEGIN
      area := 0;
      norm := 0;
      FOR i:=1 TO numdata+1 DO
         BEGIN
            WITH cxPsi DO
               probDens := sqr(Re.value(i)) + sqr(Im.value(i));
            area := area + probDens*X.value(i)*X.Value(i);
            norm := norm + probDens;
         END;
      IF norm>macheps THEN
         MeasurementUser := area/norm;
   END;


{ ************ END OF USER WRITTEN PROCEDURES ************ }



{----------------------------------------------------------------}

PROCEDURE InitializeUserProcedures;
   BEGIN
 {**** Set this flag to be true if you want your own potential ****}
     SetUserFlag;
     SetMenuItemsUser;

(*
     SetDefaultUser       := SetUserDefaults;
     SetScreenLinesUser   := SetUserScreenLines;
     ReadParamsUser       := ReadUserParams;
     CalculateDVectorUser := CalculateUserDVector;

     PerturbationUser     := Userperturbation;

     ConstructKetUser1    := ConstructUserKet1;

     ConstructKetUser2    := ConstructUserKet2;

     MeasurementUser      := UserMeasurement;
*)
  END;

{*--------------------- MAIN PROGRAM MANAGEMENT --------------------*}

{*----------------------- PART 1 --------------------------*}

PROCEDURE SetUpPart1Menu(VAR thisMenu:TMenu);
   BEGIN
      WITH thisMenu DO
         BEGIN
            init;

            column(1,'File');
            row(1,1,'About CUPS');
            row(1,2,'About Program');
            row(1,3,'Configuration');
            row(1,4,'Open...');
            row(1,5,'Save');
            row(1,6,'Save as...');
            row(1,7,'Exit Program');

            column(2,'Parts');
            row(2,1,'About Part 1');
            row(2,2,'Part 1: Finding Eigenvalues');
            row(2,3,'Part 2: Wavefunction Properties');
            row(2,4,'Part 3: Time Development');

            column(3,'Potential');
            row(3,1,'Square Well');
            row(3,2,'Ramped Well');
            row(3,3,'Asymmetric Well');
            row(3,4,'Double Square Well');
            row(3,5,'Parabolic Well');
            row(3,6,'Coulombic Well');
            IF userFlag THEN
               row(3,7,userItem1);

            column(4,'Parameters');
            row(4,1,'Vary Well Parameters');
            row(4,2,'Add a Perturbation');

            column(5,'Method');
            row(5,1,'Try Energy (with mouse)');
            row(5,2,'Try Energy (from keyboard)');
            row(5,3,'Hunt for Zero');
            row(5,4,'Examine Solution');

            column(6,'Spectrum');
            row(6,1,'Find Eigenvalues');
            row(6,2,'See Wave Functions');
            row(6,3,'See Wfs and Probs');
            row(6,4,'Examine Solution');
            row(6,5,'------------------');
            row(6,6,'Sound');
            RowActivate(6,5,false);
      END;
   END;

PROCEDURE DisposeOf(VAR thisMenu:TMenu);
   BEGIN
      thisMenu.done;
   END;

PROCEDURE ChooseFromPart1Menu(VAR thisMenu:TMenu;
                              examine,spectrumFound:Boolean;
                              VAR partCode:Integer;
                              VAR colNum,rowNum:integer);
   VAR
      found : Boolean;
      i     : Integer;
   BEGIN
      WITH thisMenu DO
         BEGIN
            thisMenu.RowActivate(5,4,examine);
            thisMenu.RowActivate(6,2,spectrumFound);
            thisMenu.RowActivate(6,3,spectrumFound);
            thisMenu.RowActivate(6,4,examine);

            thisMenu.RowChecked(2,2,true);
            FOR i:=1 TO 7 DO
               thisMenu.RowChecked(3,i,false);
            thisMenu.RowChecked(3,thisWell.welltype,true);
            thisMenu.RowChecked(6,6,soundOn);

            REPEAT
               found := chosen
            UNTIL found;
            colNum := colChosen;
            rowNum := rowChosen;
         END;
   END;

PROCEDURE FindEigenvaluesAndEigenfunctions1(VAR partCode:Integer);
{* This is the main part of the program for part 1 *}
   VAR
      colNum,rowNum : Integer;
      escape,first  : Boolean;
      examine       : Boolean;
      exitCode      : Integer;
      part1Menu     : TMenu;
   BEGIN
      SetUpPart1Menu(part1Menu);
      first := true;
      examine := false;
      DrawThePotentialWellAsBackground({using} X,V,examine);
      REPEAT
        ChooseFromPart1menu(part1Menu,examine,spectrumFound,partCode,
                            colNum,rowNum);
        escape := false;
        CASE colNum OF
         1 : CASE rowNum OF
               1 : AboutCUPS;                   {describe CUPS}
               2 : BEGIN
                      DisplayHelpScreen(progHS);{describe program}
                      DisplayHelpScreen(partHS);{describe part structure}
                   END;
               3 : Configuration;          {various facilities}
               4 : {Open old file}
                   OpenFromDisk(currentFileName);
               5 : {Save}
                   SaveOnDisk(currentFileName);
               6 : {Save as}
                   SaveAsOnDisk(currentFileName);
               7 : BEGIN
                      partCode := 4;       {means EXIT PROGRAM}
                      escape := true;
                   END;
              END;    {case rowNum}
         2 : BEGIN
                CASE rowNum OF
                  1 : BEGIN                  {show help screen}
                         DisplayHelpScreen(part1aHS);
                         DisplayHelpScreen(part1bHS);
                      END;
                  2..4 : BEGIN  {go to another part of program}
                            partCode := rowNum-1;
                            ClearMUPPETport;
                            escape := true;
                         END;
                END;    {case rowNum}
             END;
         3..6 : ExploreThisPotential(colNum,rowNum,first,
                                     examine,spectrumFound);
        END;    {case colNum}
      UNTIL escape;
      DisposeOf(part1Menu);
   END;

{*----------------------- PART 2 --------------------------*}

PROCEDURE SetVariousDefaults(VAR fileName:String;
                             VAR ovrlap:Overlap);
   BEGIN
      ovrlap.operatorNum := 1;
      fileName := '';

      operators[1] := '1';
      operators[2] := 'x';
      operators[3] := 'd/dx';
      operators[4] := 'x^2';
      operators[5] := 'd^2/dx^2';
      operators[6] := 'V';
      operators[7] := 'E';
      operators[8] := 'x.d/dx';
      operators[9] := 'd/dx.x';
      operators[10] := userItem2;
      operators[11] := userItem3;
   END;

PROCEDURE SetUpPart2Menu(VAR thisMenu:TMenu);
   VAR
      i : Integer;
   BEGIN
      WITH thisMenu DO
         BEGIN
            init;

            column(1,'File');
            row(1,1,'About CUPS');
            row(1,2,'About Program');
            row(1,3,'Configuration');
            row(1,4,'Open...');
            row(1,5,'Save');
            row(1,6,'Save as...');
            row(1,7,'Exit Program');
            RowActivate(1,4,false);

            column(2,'Parts');
            row(2,1,'About part 2');
            row(2,2,'Part 1: Finding Eigenvalues');
            row(2,3,'Part 2: Wavefunction Properties');
            row(2,4,'Part 3: Time Development');

            column(3,'Psi 1');
            FOR i:=1 TO spectrum.numLevels DO
               row(3,i,concat('Eigenstate, n=',NumStr(i,1,0)));
            row(3,i+1,'---------------');
            RowActivate(3,i+1,false);
            row(3,i+2,'General State');

            column(4,'Operator');
            FOR i:=1 TO 9 DO
               row(4,i,operators[i]);
            IF userFlag THEN
               FOR i:=10 TO 11 DO
                  row(4,i,operators[i]);

            column(5,'Psi 2');
            FOR i:=1 TO spectrum.numLevels DO
               row(5,i,concat('Eigenstate, n=',NumStr(i,1,0)));
            row(5,i+1,'------------------');
            RowActivate(5,i+1,false);
            row(5,i+2,'General state');
            row(5,i+3,'Some Other Function');

            column(6,'Integrate');
      END;
   END;

PROCEDURE ChooseFromPart2Menu(VAR thisMenu:TMenu;
                              thisOverlap:Overlap;
                              VAR partCode:Integer;
                              VAR colNum,rowNum:integer);
   VAR
      found : Boolean;
      i     : Integer;
   BEGIN
      WITH thisMenu DO
         BEGIN
            RowChecked(2,3,true);
            FOR i:=1 TO 11 DO
               RowChecked(4,i,false);
            RowChecked(4,thisOverlap.operatorNum,true);
            REPEAT
               found := chosen
            UNTIL found;
            colNum := colChosen;
            rowNum := rowChosen;

            IF colNum=3 THEN
               BEGIN
                  FOR i:=1 TO spectrum.numLevels+2 DO
                     RowChecked(3,i,false);
                  RowChecked(3,rowNum,true);
               END;
            IF colNum=5 THEN
               BEGIN
                  FOR i:=1 TO spectrum.numLevels+5 DO
                     RowChecked(5,i,false);
                  RowChecked(5,rowNum,true);
               END;
         END;
   END;

PROCEDURE DemonstratePropertiesOfWaveFunctions1(VAR partCode:Integer);
{* This is the main part of the program for part 2 *}
   VAR
      colNum,rowNum : Integer;
      first,escape  : Boolean;
      part2Menu     : TMenu;
   BEGIN
      SetUpPart2Menu(part2Menu);
      SetPart2DVectors({initializing} thisOverlap);
      SetDefaultsForOverlaps({setting} thisOverlap);
      PrepareToStudyWaveFunctions({using} X,V);
      REPEAT
        ChooseFromPart2menu(part2Menu,thisOverlap,partCode,
                            colNum,rowNum);
        escape := false;
        CASE colNum OF
         1 : CASE rowNum OF
               1 : AboutCUPS;                   {describe CUPS}
               2 : BEGIN
                      DisplayHelpScreen(progHS);{describe program}
                      DisplayHelpScreen(partHS);{describe part structure}
                   END;
               3 : Configuration;          {various facilities}
               4 : {Open old file}
                   OpenFromDisk(currentFileName);
               5 : {Save}
                   SaveOnDisk(currentFileName);
               6 : {Save as}
                   SaveAsOnDisk(currentFileName);
               7 : BEGIN
                      partCode := 4;       {means EXIT PROGRAM}
                      escape := true;
                   END;
              END;    {case rowNum}
         2 : BEGIN
                CASE rowNum OF
                  1 : BEGIN                  {show help screen}
                         DisplayHelpScreen(part2HS);
                         DisplayHelpScreen(part2aHS);
                         DisplayHelpScreen(part2bHS);
                      END;
                  2..4 : BEGIN  {go to another part of program}
                            partCode := rowNum-1;
                            escape := true;
                         END;
                END;    {case rowNum}
             END;
         3..6 :
             CalculateOverlapIntegrals(colNum,rowNum,
                                       first,thisOverlap);
        END;    {case colNum}
      UNTIL escape;
      UnsetPart2DVectors({closing} thisOverlap);
      ClearMUPPETport;
      DisposeOf(part2Menu);
   END;

{*----------------------- PART 3 --------------------------*}

PROCEDURE SetUpPart3Menu(VAR thisMenu:TMenu);
   VAR
      i : Integer;
   BEGIN
      WITH thisMenu DO
         BEGIN
            init;

            column(1,'File');
            row(1,1,'About CUPS');
            row(1,2,'About Program');
            row(1,3,'Configuration');
            row(1,4,'Open...');
            row(1,5,'Save');
            row(1,6,'Save as...');
            row(1,7,'Exit Program');
            RowActivate(1,4,false);

            column(2,'Parts');
            row(2,1,'About part 3');
            row(2,2,'Part 1: Finding Eigenvalues');
            row(2,3,'Part 2: Wavefunction Properties');
            row(2,4,'Part 3: Time Development');

            column(3,'Wave Func');
            row(3,1,'Choose Wave Function');
            row(3,2,'Show Time Development');
            row(3,3,'Begin Over');

            column(4,'Measure');
            row(4,1,'Position');
            row(4,2,'Momentum');
            row(4,3, userItem5);
            IF userFlag THEN i := 4
                        ELSE i := 3;
            row(4,i,'--------');
            row(4,i+1,'Set Time');

            column(5,'Plot How');
            row(5,1,'Amplitude & Phase');
            row(5,2,'Real & Imaginary');

      END;
   END;

PROCEDURE ChooseFromPart3Menu(VAR thisMenu:TMenu;
                              initFlag:Boolean; VAR partCode:Integer;
                              VAR colNum,rowNum:integer);
   VAR
      found    : Boolean;
      i        : Integer;
   BEGIN
      WITH thisMenu DO
         BEGIN
            FOR i:=2 TO 3 DO
               RowActivate(3,i,NOT initFlag);
            FOR i:=1 TO 3 DO
               RowActivate(4,i,NOT initFlag);
            IF userFlag THEN
               RowActivate(4,4,false)
            ELSE
               RowActivate(4,3,false);

            RowChecked(2,4,true);
            RowChecked(5,1, NOT realFlag);
            RowChecked(5,2,realFlag);

            REPEAT
               found := chosen
            UNTIL found;
            colNum := colChosen;
            rowNum := rowChosen;
         END;
   END;

PROCEDURE DemonstrateTimeDevelopment1(VAR partCode:Integer);
{* This is the main part of the program for part 3 *}
   VAR
      colNum,rowNum : Integer;
      escape,first  : Boolean;
      initFlag      : Boolean;
      exitCode      : Integer;
      t,dt          : Real;
      part3Menu     : TMenu;
   BEGIN
      SetUpPart3Menu(part3Menu);
      initFlag := true;
      SetPart3Vectors({setting} eigFns,cxPsi,realFlag);
      RecordAllEigenfunctions({using} spectrum,X,V,{giving} eigFns);
      SetDefaultCoefficients({setting} initialCxAmps);
      SetInitialTimes({setting} t,dt);
      PrepareForEvolution(t);

      REPEAT
         ChooseFromPart3menu(part3Menu,initFlag,partCode,colNum,rowNum);
         escape := false;
         CASE colNum OF
          1 : CASE rowNum OF
                1 : AboutCUPS;                   {describe CUPS}
                2 : BEGIN
                       DisplayHelpScreen(progHS);{describe program}
                       DisplayHelpScreen(partHS);{describe part structure}
                    END;
                3 : Configuration;          {various facilities}
                4 : {Open old file}
                    OpenFromDisk(currentFileName);
                5 : {Save}
                    SaveOnDisk(currentFileName);
                6 : {Save as}
                    SaveAsOnDisk(currentFileName);
                7 : BEGIN
                       partCode := 4;       {means EXIT PROGRAM}
                       escape := true;
                    END;
               END;    {case rowNum}
           2 : BEGIN
                 CASE rowNum OF
                   1 : BEGIN                  {show help screen}
                          DisplayHelpScreen(part3HS);
                          DisplayHelpScreen(part3aHS);
                          DisplayHelpScreen(part3bHS);
                       END;
                  2..4 : BEGIN  {go to another part of program}
                            partCode := rowNum-1;
                            escape := true;
                         END;
                 END;    {case rowNum}
              END;
          3..5 :
              StartSystemDeveloping(colNum,rowNum,first,initFlag,
                                    initialCxAmps,t,dt);
          END;    {case colNum}
      UNTIL escape;
      UnsetPart3Vectors({clearing} eigFns,cxPsi);
      DisposeOf(part3Menu);
   END;

{*----------------------- MAIN PROGRAM --------------------------*}

PROCEDURE IntroduceProgram(VAR partCode:Integer);
   VAR
      thisMenu : Tmenu;
      rNum     : Integer;
   BEGIN
      SetUpPart1Menu(thisMenu);
      thisMenu.Display;
      DisplayHelpScreen(progHS);
      partCode := 1;
      thisMenu.done;
   END;

BEGIN
   CUPSinit;
   InitializeUserProcedures;
   SetGlobalValues;
   SetPlottingDetails;
   SetADefaultWell(thisWell,thisPerturbn,V,spectrum,spectrumFound);
   SetVariousDefaults(currentFileName,thisOverlap);
   IntroduceProgram(partCode);

   REPEAT
      CASE partCode OF
       1 : {Part 1}
           FindEigenvaluesAndEigenfunctions1(partCode);
       2 : {Part 2}
           IF spectrumHasBeenCalculated(spectrum) THEN
              DemonstratePropertiesOfWaveFunctions1(partCode)
           ELSE
              partCode := 1;
       3 : {Part 3}
           IF spectrumHasBeenCalculated(spectrum) THEN
              DemonstrateTimeDevelopment1(partCode)
           ELSE
              partCode := 1;
      END;
   UNTIL (partCode=4);

   CUPSdone;
END.



