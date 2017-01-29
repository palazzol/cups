{Program title: BOUND STATE WAVE FUNCTIONS IN THREE DIMENSIONS        }
{Author: Ian D. Johnston             Copyright 1993                   }
{Version: 3.0                        25 April 1994                    }
{                                                                     }
{Computes energy eigenfunctions for an particle in a spherically sym- }
{metric potential well, which can be square, parabolic, or Coulombic. }
{The potential is assumed to be zero outside the range r<rMax.        }
{The angular part of the wave functions are assumed to be spherical   }
{harmonics, and the radial part computed by "hunt and shoot" methods. }
{                                                                     }
{One part of the program is designed to show the student how to solve }
{the problem in principle, for a given angular momentum quantum number}
{by choosing a value of the binding energy, and calculating from the  }
{origin outwards, and seeing if the resulting function behaves like a }
{decreasing exponential at r=Rmax.  In another part of the program the}
{eigenvalues are computed automatically, by counting the number of    }
{nodes to determine where the eigenvalues occur.                      }
{The first part of the program deals with simply finding the eigen-   }
{values and eigenfunctions of different wells.  The second part looks }
{at properties of eigenfunctions - normalization, orthogonality and   }
{the evaluation of many kinds of overlap integrals.                   }
{Facility is provided for users to incorporate their own procedures to}
{specify different potential wells or different overlap integrals.    }
{                                                                     }
{Utilities: Crt, Graph, MUPPET, CUPS, CUPSfunc, CUPSproc, CUPSgui     }
{Library files: The parts of the code responsible for the SEARCH FOR  }
{               ZERO option is in a pre-compiled unit SEARCH.TPU      }
{Algorithms: Half-step integration and/or Numerov                                           }
{                                                                     }

{$F+}
{$N+}

PROGRAM Bound3D;

USES
   Crt, Graph, CUPSmupp,
   CUPS, CUPSfunc, CUPSproc, CUPSgui, CUPSgrph, CUPSpars,
   Search, SphHar,B3DHelps;

VAR
   partCode : Integer;
   quitFlag : Boolean;

{ --------------- ITEMS COMMON TO ALL PARTS ----------------- }

CONST
   h    = 6.626E-34;       {J.s}
   hbar = 1.055E-34;       {J.s}
   e    = 1.602E-19;       {C}
   me   = 9.109E-31;       {kg - mass of electron}
   mp   = 1.673E-27;       {kg - a.m.u.}
   a0   = 5.292E-11;       {m - Bohr radius}
   nm   = 1.000E-09;       {m - unit for atomic/molecular lengths}
   fm   = 1.000E-15;       {m - unit for nuclear lengths}
   eV   = 1.602E-19;       {J - unit for atomic/molecular energies}
   MeV  = 1.602E-13;       {J - unit for nuclear energies}
   eps0 = 8.854E-12;       {F.m-1}

   psiColor   : word = white;
   angColor   : word = lightmagenta;
   potlColor1 : word = lightblue;
   potlColor2 : word = lightgray;
   probColor  : word = yellow;
   engyColor  : word = cyan;
   realColor  : word = lightred;
   imagColor  : word = lightgreen;
   dullColor  : word = lightgray;
   dullerColor: word = darkgray;
   posColor   : word = lightblue;
   negColor   : word = yellow;
   braColor   : word = lightgreen;
   ketColor   : word = lightgreen;
   prodColor  : word = lightgreen;
   intgColor  : word = lightgreen;


   inpBackColor   : integer = blue;
   MUPbackColor : word = black;

TYPE
   wellParams = RECORD
                   wellType : Integer;
                   depth1   : Real;
                   depth2   : Real;
                   posn1    : Real;
                   posn2    : Real;
                   maxDepth : Real
                END;
   UnitsType = (atomic,molecular,nuclear);
   UnitRec  = RECORD
                 code      : UnitsType;
                 mass      : Real;
                 massRatio : Real;
                 length    : Real;
                 energy    : Real;
                 nameL     : String;
                 nameE     : String;
              END;
   Perturbn = RECORD
                 perturbnType : Integer;
                 coefficient  : Real;
              END;
   ValuList = ARRAY [1..20] OF real;
   eValList = RECORD
                 l, m      : Integer; {ang mom quantum nos}
                 numLevels : Integer;
                 eigenVals : ValuList;
              END;
   psiParam = RECORD
                 quantNum : Integer;
                 coeffs   : ARRAY[1..7] OF Real;
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
   fnArray = ARRAY [1..7] OF Dvector;
   Str1    = String[1];

   FieldRecord = RECORD
                    identifier   : String;
                    fileUnits    : UnitRec;
                    fileHighestR : Real;
                    fileLowestR  : Real;
                    fileDeltaR   : Real;
                    fileTicR     : Real;
                    fileWell     : WellParams;
                    filePerturbn : Perturbn;
                    fileSpectrum : eValList;
                    fileFound    : Boolean;
                    fileBraParam : PsiParam;
                    fileKetParam : PsiParam;
                 END;

   DiskFile = FILE OF FieldRecord;

CONST
   numData = 200;
   fileIdentifier = 'File for BOUND_3D.PAS';
   lMax    = 5;               {largest value of orb ang momm.}
   first: boolean = true;
   same: boolean = true;

TYPE
   Coeffnts = ARRAY[1..(2*lMax+1)] OF Real;

VAR
   R,V,chi,prob,temp : dVector;
   energy, area      : Real;
   eigFns            : fnArray;
   ticV              : Real;
   highestV,lowestV  : Real;

   atomicUnits      : UnitRec;
   nuclearUnits     : UnitRec;
   molecularUnits   : UnitRec;
   energyConvert    : Real;

 { These variables are global }
   globalUnits       : UnitRec;
   highestR,lowestR  : Real;
   deltaR,ticR       : Real;
   thisWell          : WellParams;
   thisPerturbn      : Perturbn;
   spectrum          : eValList;
   spectrumFound     : Boolean;
   thisOverlap       : Overlap;
   currentFileName   : String;
   operators         : ARRAY[1..12] of String;
   psi1Name,psi2Name : String;

   escape,pure     : Boolean;
   realFlag        : Boolean;
   key,key1        : Byte;
   n,choice,chosen : Integer;
   nextN,soundOn   : Boolean;
   cloudKeys       : Thotkeys;

   userFlag  : Boolean;   {true if there are special user procedures}
   userItem1, userItem2 : String;  {for putting in the menus by user}
   userItem3, userItem4 : String;


{*** NAMES OF USER DEFINED PROCEDURES ***}

PROCEDURE ConstructKetUser1(VAR R,V,bra,ket:DVector);
   FORWARD;
PROCEDURE ConstructKetUser2(VAR R,V,bra,ket:DVector);
   FORWARD;
PROCEDURE SetDefaultUser(VAR thisWell:wellParams;
                         VAR highR,stepR:Real);
   FORWARD;
PROCEDURE SetScreenLinesUser(VAR thisWell:wellParams;
                         VAR thisScreen:TInputScreen);
   FORWARD;
PROCEDURE ReadParamsUser(VAR thisScreen:TInputScreen;
                         VAR thisWell:wellParams);
   FORWARD;
PROCEDURE CalculateDVectorUser(VAR thisWell:wellParams; VAR V:dVector);
   FORWARD;
FUNCTION PerturbationUser(a,r:Real):Real;
   FORWARD;



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
      IF (thisWidth>(highestR-lowestR)) THEN
         SayError('Widths cannot be greater than space available.'
                  ,flag);
   END;

PROCEDURE SetNewScaleOnXAxis(highR,stepR:Real; VAR V:Dvector;
                             VAR spectrumFound:Boolean);FORWARD;


{-------------------- SQUARE WELL ------------------------}

PROCEDURE SetDefaultSquare(VAR thisWell:wellParams;
                           VAR highR,stepR:Real);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1   := 300;
            posn1    := +0.05;
            maxDepth := 600;
            highR    := 0.16;
            stepR    := 0.05;
            ticV     := 50;
         END;
      globalUnits := atomicUnits;
   END;

PROCEDURE SetScreenLinesSquare(thisWell:wellParams;
                               VAR thisScreen:TInputScreen);
   VAR
      thisMaxWidth : Real;
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('          Depth of the well in ',
                            globalUnits.nameE,
                            ' (<',
                            numStr(thisWell.maxDepth,3,0),
                            ') = {      }'));
            SetNumber(1,thisWell.depth1);
            SetNumberLimits(1, 0, thisWell.maxDepth);

            thisMaxWidth := highestR-lowestR;
            LoadLine(concat('          Radius of the well in ',
                            globalUnits.nameL,
                            '       = {      }'));
            SetNumber(2,thisWell.posn1);
            SetNumberLimits(2,0,thisMaxWidth);
         END;    {with thisScreen}
   END;

PROCEDURE ReadParamsSquare(VAR thisScreen:TInputScreen;
                           VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := thisScreen.GetNumber(1);
            posn1 := abs(thisScreen.GetNumber(2));
         END;
   END;

PROCEDURE CalculateDVectorSquare(VAR thisWell:wellParams;
                               VAR V:dVector);
   VAR
      i: integer;
   BEGIN
      WITH thisWell DO
         FOR i:=1 TO numData+1 DO
            IF R.value(i)<=posn1 THEN V.Put(i,-depth1)
                                 ELSE V.Put(i,0);
   END;

{-------------------- PARABOLIC WELL ------------------------}

PROCEDURE SetDefaultParabolic(VAR thisWell:wellParams;
                              VAR highR,stepR:Real);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1   := 300;
            posn1    := 0.05;
            maxDepth := 600;
            highR    := 0.16;
            stepR    := 0.05;
            ticV     := 50;
         END;
      globalUnits := atomicUnits;
   END;

PROCEDURE SetScreenLinesParabolic(thisWell:wellParams;
                               VAR thisScreen:TInputScreen);
   VAR
      thisMaxWidth : Real;
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('          Depth of the well in ',
                      globalUnits.nameE,
                      '(<',
                      numStr(thisWell.maxDepth,3,0),
                      ') = {      }'));
            SetNumber(1,thisWell.depth1);
            SetNumberLimits(1, 0, thisWell.maxDepth);

            thisMaxWidth := highestR-lowestR;
            LoadLine(concat('          Radius at the top in ',
                            globalUnits.nameL,
                            '       = {      }'));
            SetNumber(2,thisWell.posn1);
            SetNumberLimits(2,0,thisMaxWidth);
         END;    {with thisScreen}
   END;

PROCEDURE ReadParamsParabolic(VAR thisScreen:TInputScreen;
                              VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := thisScreen.GetNumber(1);
            posn1 := abs(thisScreen.GetNumber(2));
         END;

   END;

PROCEDURE CalculateDVectorParabolic(VAR thisWell:wellParams;
                               VAR V:dVector);
   VAR
      i: integer;
   BEGIN
      WITH thisWell DO
          FOR i:=1 TO numData+1 DO
             IF R.value(i)<=posn1
                THEN V.Put(i,depth1*(sqr(R.value(i)/posn1)-1))
                ELSE V.Put(i,0);
   END;

{-------------------- COULOMBIC WELL ------------------------}

PROCEDURE SetDefaultCoulombic(VAR thisWell:wellParams;
                              VAR highR,stepR:Real);
   BEGIN
      WITH thisWell DO
         BEGIN
            posn1  := a0/nm;
            depth1 :=  e*e/(4*pi*eps0*posn1*nm)/eV;
            depth2 := 600;     {well is truncated at this depth}
            maxDepth := 600;
            highR    := 0.32;
            stepR    := 0.1;
            ticV     := 50;
         END;
      globalUnits := atomicUnits;
   END;

PROCEDURE SetScreenLinesCoulombic(VAR thisWell:wellParams;
                                  VAR thisScreen:TInputScreen);
   VAR
      thisMaxWidth : Real;
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('          Depth at bottom in ',
                      globalUnits.nameE,
                      '(<',
                      numStr(thisWell.maxDepth,3,0),
                      ') = {      }'));
            SetNumber(1,thisWell.depth2);
            SetNumberLimits(1, 0, thisWell.maxDepth);

            thisMaxWidth := highestR-lowestR;
            LoadLine(concat('          Depth at (r=',
                            NumStr(thisWell.posn1,4,3),
                            globalUnits.nameL,
                            ') in ',
                            globalUnits.nameE,
                            '  = {      }'));
            SetNumber(2,thisWell.depth1);
            SetNumberLimits(2,0,thisWell.maxDepth);
         END;    {with thisScreen}
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
                IF abs(R.value(i))*depth2>depth1*posn1
                   THEN V.Put(i,-depth1*posn1/abs(R.value(i)))
                   ELSE V.Put(i,-depth2);
         END;
   END;

{-------------------- Lennard-JONES WELL------------------------}

PROCEDURE SetDefaultLennardJones(VAR thisWell:wellParams;
                              VAR highR,stepR:Real);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1   := 10.00;
            depth2   := -10.00;
            posn1    := 0.05;
            maxDepth := 20.00;
            highR    := 0.10;
            stepR    := 0.05;
            ticV     := 5.0;
         END;
      globalUnits := molecularUnits;
   END;

PROCEDURE SetScreenLinesLennardJones(VAR thisWell:wellParams;
                                    VAR thisScreen:TInputScreen);
   VAR
      thisMaxWidth : Real;
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('          Depth of the well in ',
                      globalUnits.nameE,
                      '(<',
                      numStr(thisWell.maxDepth,3,0),
                      ') = {      }'));
            SetNumber(1,thisWell.depth1);
            SetNumberLimits(1, 0, thisWell.maxDepth);

            thisMaxWidth := highestR-lowestR;
            LoadLine(concat('          Radius at the minimum in ',
                            globalUnits.nameL,
                            '   = {      }'));
            SetNumber(2,thisWell.posn1);
            SetNumberLimits(2,0,thisMaxWidth);
         END;
   END;

PROCEDURE ReadParamsLennardJones(VAR thisScreen:TInputScreen;
                              VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := thisScreen.GetNumber(1);
            posn1 := abs(thisScreen.GetNumber(2));
         END;
   END;

PROCEDURE CalculateDVectorLennardJones(VAR thisWell:wellParams;
                               VAR V:dVector);
   VAR
      i    : Integer;
      rSix : Real;
   BEGIN
      WITH thisWell DO
         BEGIN
            V.Put(1,-depth2);
            FOR i:=2 TO numData+1 DO
               BEGIN
                  rSix := r.Value(i)*r.Value(i)/posn1/posn1;
                  rSix := rSix*rSix*rSix;
                  rSix := rSix*rSix;
                  V.put(i,
                     depth1*((1/rSix - 2)/rSix));
                  IF V.value(i)>-depth2 THEN V.put(i,-depth2);
               END;
         END;
   END;

{-------------------- DUMBELL WELL------------------------}

PROCEDURE SetDefaultDumbell(VAR thisWell:wellParams;
                              VAR highR,stepR:Real);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1   := 10.00;
            depth2   := -10.00;
            posn1    := 0.05;
            posn2    := 0.0065;
            maxDepth := 20.00;
            highR    := 0.10;
            stepR    := 0.05;
            ticV     := 5.0;
         END;
      globalUnits := molecularUnits;
   END;

PROCEDURE SetScreenLinesDumbell(VAR thisWell:wellParams;
                                VAR thisScreen:TInputScreen);
   VAR
      thisMaxWidth : Real;
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('          Depth of the well in ',
                            globalUnits.nameE,
                            ' (<',
                            numStr(thisWell.maxDepth,3,0),
                            ') = {      }'));
            SetNumber(1,thisWell.depth1);
            SetNumberLimits(1, 0, thisWell.maxDepth);

            thisMaxWidth := highestR-lowestR;
            LoadLine(concat('          Radius at the minimum in ',
                            globalUnits.nameL,
                            '    = {      }'));
            SetNumber(2,thisWell.posn1);
            SetNumberLimits(2,0,thisMaxWidth);
         END;    {with thisScreen}
   END;

PROCEDURE ReadParamsDumbell(VAR thisScreen:TInputScreen;
                              VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := thisScreen.GetNumber(1);
            posn1 := abs(thisScreen.GetNumber(2));
         END;
   END;

PROCEDURE CalculateDVectorDumbell(VAR thisWell:wellParams;
                               VAR V:dVector);
   VAR
      i  : Integer;
      r0 : Real;
   BEGIN
      WITH thisWell DO
         BEGIN
            FOR i:=1 TO numData+1 DO
               BEGIN
                  IF (r.value(i)<posn1-posn2)
                  OR (r.value(i)>posn1+posn2) THEN
                     V.put(i,-depth2)
                  ELSE
                     BEGIN
                        r0 := abs(r.value(i) - posn1);
                        V.put(i,-depth1+(depth1-depth2)*sqr(r0/posn2));
                     END;
               END;
         END;
   END;

{-------------------- YUKAWA WELL ------------------------}

PROCEDURE SetDefaultYukawa(VAR thisWell:wellParams;
                           VAR highR,stepR:Real);
   BEGIN
      WITH thisWell DO
         BEGIN
            posn1  := 1.0;     {range of well}
            depth1 := 200;
            depth2 := 600;   {well is truncated at this depth}
            maxDepth := 600;
            highR    := 3.20;
            stepR    := 0.50;
            ticV     := 50;
         END;
      globalUnits := nuclearUnits;
   END;

PROCEDURE SetScreenLinesYukawa(VAR thisWell:wellParams;
                               VAR thisScreen:TInputScreen);
   VAR
      thisMaxWidth : Real;
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('          Depth V0 in ',
                            globalUnits.nameE,
                            ' (<',
                            numStr(thisWell.maxDepth,3,0),
                            ') = {      }'));
            SetNumber(1,thisWell.depth1);
            SetNumberLimits(1, 0, thisWell.maxDepth);

            thisMaxWidth := highestR-lowestR;
            LoadLine(concat('          Range of well in ',
                            globalUnits.nameL,
                            '    = {      }'));
            SetNumber(2,thisWell.posn1);
            SetNumberLimits(2,0,thisMaxWidth);
         END;    {with thisScreen}
   END;

PROCEDURE ReadParamsYukawa(VAR thisScreen:TInputScreen;
                           VAR thisWell:wellParams);
   BEGIN
      WITH thisWell DO
         BEGIN
            depth1 := thisScreen.GetNumber(1);
            posn1 := abs(thisScreen.GetNumber(2));
         END;
   END;

PROCEDURE CalculateDVectorYukawa(VAR thisWell:wellParams;
                               VAR V:dVector);
   VAR
      i: integer;
   BEGIN
      WITH thisWell DO
         BEGIN
            V.Put(1,-depth2);
            FOR i:=2 TO numData+1 DO
               BEGIN
                  V.Put(i,
                       -depth1*posn1/abs(R.value(i)){*exp(-0*R.value(i)/posn1)});
                  IF (V.Value(i)<-depth2)
                     THEN V.Put(i,-depth2);
               END;
         END;
   END;


{------------- GENERAL PROCEDURES FOR ALL WELLS ----------}

PROCEDURE CalculateEnergyConvert;
   BEGIN
      WITH GlobalUnits DO
         energyConvert := 2*massRatio*mass*sqr(length)*energy/hbar/hbar;
   END;

PROCEDURE SetDefaultWellParameters(VAR thisWell:wellParams);
   VAR
      highR,stepR : Real;
   BEGIN
      CASE thisWell.wellType OF
         1 : SetDefaultSquare({setting} thisWell, highR,stepR);
         2 : SetDefaultParabolic({setting} thisWell, highR,stepR);
         3 : SetDefaultCoulombic({setting} thisWell, highR,stepR);
         4 : SetDefaultLennardJones({setting} thisWell, highR,stepR);
         5 : SetDefaultDumbell({setting} thisWell, highR,stepR);
         6 : SetDefaultYukawa({setting} thisWell, highR,stepR);
         7 : SetDefaultUser({setting} thisWell,highR,stepR);
      END;   {with case}
      CalculateEnergyConvert;
      SetNewScaleOnXAxis(highR,stepR,V,spectrumFound);
   END;

PROCEDURE SetPotentialPlottingScale(VAR V:dVector);
   VAR
      offsetV: real;
   BEGIN
      offsetV := (highestV - lowestV)/20;
      DefineScale(1, lowestR,highestR,
                     lowestV-offsetV,highestV+8*offsetV);
   END;

PROCEDURE CalculateWellDVector(VAR thisWell:wellParams;
                               VAR V:dVector; VAR spectrumFound:Boolean);
   VAR
      i: integer;
   BEGIN
     CASE thisWell.wellType OF
         1 : CalculateDVectorSquare({using} thisWell, {giving} V);
         2 : CalculateDVectorParabolic({using} thisWell, {giving} V);
         3 : CalculateDVectorCoulombic({using} thisWell, {giving} V);
         4 : CalculateDVectorLennardJones({using} thisWell, {giving} V);
         5 : CalculateDVectorDumbell({using} thisWell, {giving} V);
         6 : CalculateDVectorYukawa({using} thisWell, {giving} V);
         7 : CalculateDVectorUser({using} thisWell, {giving} V);
        END;   {with case}

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


PROCEDURE SetParticularScreenLines(thisWell:wellParams;
                                   VAR thisScreen:TInputScreen);
   BEGIN
      CASE thisWell.wellType OF
       1 : SetScreenLinesSquare(thisWell,thisScreen);
       2 : SetScreenLinesParabolic(thisWell,thisScreen);
       3 : SetScreenLinesCoulombic(thisWell,thisScreen);
       4 : SetScreenLinesLennardJones(thisWell,thisScreen);
       5 : SetScreenLinesDumbell(thisWell,thisScreen);
       6 : SetScreenLinesYukawa(thisWell,thisScreen);
       7 : SetScreenLinesUser(thisWell,thisScreen);
      END;    {case}
   END;

PROCEDURE ReadParticularParams(VAR thisScreen:TInputScreen;
                               VAR thisWell:wellParams);
   BEGIN
      CASE thisWell.wellType OF
       1 : ReadParamsSquare({using} thisScreen, {giving} thisWell);
       2 : ReadParamsParabolic({using} thisScreen, {giving} thisWell);
       3 : ReadParamsCoulombic({using} thisScreen, {giving} thisWell);
       4 : ReadParamsLennardJones({using} thisScreen, {giving} thisWell);
       5 : ReadParamsDumbell({using} thisScreen, {giving} thisWell);
       6 : ReadParamsYukawa({using} thisScreen, {giving} thisWell);
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
            LoadLine(concat('             #1   Linear    (A.r)'));
            LoadLine(concat('             #1   Quadratic (A.r^2)'));
            LoadLine(concat('             #1   Cubic     (A.r^3)'));
            LoadLine(concat('             #1   Quartic   (A.r^4)'));
            IF userFlag THEN
               LoadLine(concat('             #1   ', userItem2));
         END;    {with thisScreen}
   END;

PROCEDURE CheckSizePerturbn(thisPerturbn:Perturbn; VAR flag:Boolean);
   VAR
      maxP : Real;
      maxL : Real;
   BEGIN
      maxL := (highestR - lowestR)/2;
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
            Xi := R.value(i);
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

PROCEDURE SetVariousUnits;
   BEGIN
      WITH atomicUnits DO
         BEGIN
            code   := atomic;
            mass   := me;
            massRatio := 1.00;
            length := nm;
            energy := eV;
            nameL  := 'nm';
            nameE  := 'eV';
         END;
      WITH molecularUnits DO
         BEGIN
            code   := molecular;
            mass   := mp;
            massRatio := 1.00;
            length := nm;
            energy := eV;
            nameL  := 'nm';
            nameE  := 'eV';
         END;
      WITH nuclearUnits DO
         BEGIN
            code   := nuclear;
            mass   := mp;
            massRatio := 1.00;
            length := fm;
            energy := MeV;
            nameL  := 'fm';
            nameE  := 'MeV';
         END;
      globalUnits := atomicUnits;
      CalculateEnergyConvert;
   END;

PROCEDURE ChangeGlobalUnits(VAR spectrumFound:Boolean);
   VAR
      i, n         : Integer;
      screen1      : TInputScreen;
      highR, stepR : Real;
   BEGIN
      WITH screen1 DO
         BEGIN
            Init;
            DefineInputPort(0.00,0.98, 0.50,0.95);
            LoadLine('Choose the set of units you want to work with');
            LoadLine('');
            LoadLine('    #1:  Atomic units');
            LoadLine('           Lengths measured in nm, energies measured in eV.');
            LoadLine('    #1:  Molecular units');
            LoadLine('           Length measured in nm, energies measured in eV.');
            LoadLine('    #1:  Nuclear units');
            LoadLine('           Lengths measured in fm, energies measured in MeV.');
            LoadLine('');
            LoadLine('                                 [ Ok ]    [Cancel]');
            CASE globalUnits.code OF
              atomic    : SetBoolean(1,true);
              molecular : SetBoolean(2,true);
              nuclear   : SetBoolean(3,true);
            END;    {case}
            AcceptScreen;
            FOR i:=1 TO 3 DO
               IF GetBoolean(i) THEN n := i;
            escape := Canceled;
            done;
         END;    {with thisScreen}

      IF NOT escape THEN
         BEGIN
            CASE n OF
             1 : BEGIN
                    globalUnits := atomicUnits;
                    highR := 0.16;
                    stepR := 0.05;
                 END;
             2 : BEGIN
                    globalUnits := molecularUnits;
                    highR := 0.10;
                    stepR := 0.05;
                 END;
             3 : BEGIN
                    globalUnits := nuclearUnits;
                    highR := 1.60;
                    stepR := 0.50;
                 END;
            END;    {case}
            SetNewScaleOnXAxis(highR,stepR,V,spectrumFound);
            CalculateEnergyConvert;
            spectrumFound := false;
         END;   {if not escape}
   END;

PROCEDURE ChangeMass(VAR spectrumFound:Boolean);
   VAR
      i,n             : Integer;
      thisMassRatio   : Real;
      screen2         : TInputScreen;
      nameStr,massStr : String;
   BEGIN
      CASE globalUnits.code OF
       atomic    : BEGIN
                      nameStr := 'atomic';
                      massStr := 'electron mass';
                   END;
       molecular : BEGIN
                      nameStr := 'molecular';
                      massStr := 'a.m.u.';
                   END;
       nuclear   : BEGIN
                      nameStr := 'nuclear';
                      massStr := 'a.m.u.';
                   END;
      END;    {case}

      WITH screen2 DO
         BEGIN
            Init;
            DefineInputPort(0.00,0.98, 0.50,0.95);
            LoadLine(Concat('You are currently working with ',
            nameStr,' units.'));
            LoadLine('');
            LoadLine('You may change the mass of the particle:');
            LoadLine(Concat('       mass = {',
                            NumStr(globalUnits.massRatio,4,3),
                            '} * ',
                            massStr,
                            '.'));
            LoadLine('');
            LoadLine('                                 [ Ok ]    [Cancel]');
            AcceptScreen;
            escape := Canceled;
            thisMassRatio := GetNumber(1);
            Done;
         END;    {with screen2}

         IF NOT escape THEN
            BEGIN
               globalUnits.massRatio := thisMassRatio;
               CalculateEnergyConvert;
               spectrumFound := false;
            END;
   END;

PROCEDURE SetGlobalValues;
   VAR
      i: integer;
   BEGIN
      highestR := +0.16;           {scale on r axis is in nm}
      lowestR  :=  0.00;
      ticR     :=  0.05;           {default tic marks}
      ticV     :=  50;
      n        := 1;               {first level number to be tried}
      deltaR   := (highestR-lowestR)/numData;
      soundOn := true;
      Randomize;

      R.init(numdata+1);
      V.init(numdata+1);
      chi.init(numdata+1);
      prob.init(numdata+1);
      temp.init(numdata+1);
      FOR i:=1 TO numData+1 DO R.Put(i,lowestR + (i-1)*deltaR);
   END;

PROCEDURE InputNewScale(VAR highX:Real; VAR escape:Boolean);
   VAR
      thisScreen: TInputScreen;
   BEGIN
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputPort(0.05,0.95, 0.05,0.10);
            LoadLine(concat('Maximum value on r axis ',
                            '{    } ',
                            globalUnits.nameL,
                            ' : [  Ok  ]  [Cancel] '));
            SetNumber(1,highX);
            SetNumberLimits(1,0,100);
            escape := false;
            AcceptScreen;
            IF Canceled
               THEN escape := true
               ELSE highX := GetNumber(1);
            Done;
         END;    {with thisScreen}
   END;

PROCEDURE SetNewScaleOnXAxis(highR,stepR:Real; VAR V:Dvector;
                             VAR spectrumFound:Boolean);
   VAR
      i      : Integer;
   BEGIN
      highestR := highR;
      ticR     := stepR;
      deltaR   := (highestR-lowestR)/numData;
      FOR i:=1 TO numData+1 DO R.Put(i,lowestR + (i-1)*deltaR);
      CalculateWellDvector(thisWell,V,spectrumFound);
      SetPotentialPlottingScale(V);
   END;

PROCEDURE ChangeScaleOnXAxis(VAR V:Dvector; VAR spectrumFound:Boolean);
   VAR
      highR,stepR  : Real;
      escape       : Boolean;
      factor       : Real;
   BEGIN
      highR := highestR;
      InputNewScale(highR,escape);
      IF (NOT escape) AND (highR>0) THEN
         BEGIN
            IF highR>=highestR THEN
               factor := round(highR/highestR)
            ELSE
               factor := 1/(round(highestR/highR));
            stepR := factor*ticR;
            SetNewScaleOnXAxis(highR,stepR,V,spectrumFound);
         END;
   END;

{ --------------- PROCEDURES FOR PLOTTING DETAILS ----------------- }

PROCEDURE SetPlottingDetails;
   VAR
      fract1,fract2   : Real;
      xCenter,yOffset : Real;
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
    { prepare a square viewport for the spherical harmonics}
         fract1  := 0.285; fract2  := 1.36 * fract1;
         xCenter := 0.30;  yOffset := 0.12;
      DefineViewport(9,  xCenter-fract1,xCenter+fract1,
                         yOffset,yOffset+2*fract2);
      DefineViewport(10, 0.05,0.95, 0.07,0.12);  {for message re find energy}
      DefineViewport(11, 0.01,0.99, 0.35,0.65);  {for error message}
      DefineViewport(12, 0.05,0.95, 0.07,0.12);  {for message re examine}
         fract1  := 0.280; fract2  := 1.36 * fract1;
         xCenter := 0.50;  yOffset := 0.16;
      DefineViewport(13, xCenter-fract1,xCenter+fract1,
                     yOffset,yOffset+2*fract2);  {for the probability cloud}
   END;

PROCEDURE SpecialScale(s:Integer; VAR Y:DataVector);
   VAR
      i    : Integer;
      maxY, ticY : Real;
   BEGIN
      maxY := 0;
      FOR i:=1 TO numData+1 DO
         IF abs(Y[i])>maxY THEN maxY := abs(Y[i]);
      DefineScale(s, lowestR,highestR, -maxY,maxY);
      ticY := trunc(maxY);
      SetColor(lightgray);
      Axis(0,0, ticR,ticY);
   END;

PROCEDURE SemiAutoScale(snum:integer; VAR chi:Dvector);
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
      DrawAxis(0,0, ticR,ticY,MUPbackColor);
    { Then set the new scale and draw axes }
      chi.MinMax(n,min,n,max);
      IF abs(min)>abs(max)
         THEN max := 1.02*abs(min)
         ELSE max := 1.02*abs(max);
      IF max<0.102 THEN max := 0.102;
      DefineScale(snum,lowestR,highestR,-max,max);
      IF max>1
         THEN ticY := trunc(max)
         ELSE ticY := trunc(10*max)/10;
      DrawAxis(0,0, ticR,ticY,dullColor);
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
            PlotLine(lowestR,-eigenvals[i], highestR,-eigenVals[i]);
   END;

PROCEDURE DrawBar(r,y:real);
   BEGIN
      SetLineStyle(0,0,thickwidth);
      PlotLine(r-deltaR/2,0, r-deltaR/2,y);
      SetLineStyle(0,0,normwidth);
   END;

{ ------------ GENERAL PROCEDURES FOR WAVE EQUATION -------------- }

{** The radial wave equation will be solved in the form: **}
{**    d2/dr2(chi) = ( l(l+1)/r2 - 2m/hbar^2(E-V)) chi   **}
{** or, using binding energy explicitely positive        **}
{**    d2/dr2(chi) = ( l(l+1)/r2 + 2m/hbar^2(EB+V)) chi  **}
{** The function chi is related to the usual psi by      **}
{**          psi(r)  =  chi(r) / r                       **}
{**                                                      **}
{ ** Initial conditions are:      near r = 0             **}
{ **         chi(r) = r^(l+1),   d/dr(chi) = r^l         **}

PROCEDURE SolveWaveEquation(EB:real; l:Integer; VAR R,V,chi:dVector);
  {This procedure uses the Numerov algorithm as in CUPSmath.tpu.}
   VAR
      psi0,dydx : Real;
      i         : Integer;
      A         : Dvector;
   BEGIN
      psi0 := 200;           {sets arbitrary amplitude of the solution}
    { Initial conditions of integration: chi must be an increasing    }
    { exponential for values of R to the left of the potential well.  }
      chi.Put(1,0);
      chi.Put(2,psi0 * exp((l+1)*ln(deltaR)));
    { Setting up for the numerov method of integration }
      A.init(NumData+1);
      A.Put(1,( (l*(l+1)/sqr(deltaR))
                   + energyConvert*(EB+V.value(1)) )
                * deltaR*deltaR/12.0);  { an approximation!! }
      FOR i:=2 TO numData+1 DO
         A.Put(i,( (l*(l+1)/sqr(R.value(i)))
                   + energyConvert*(EB+V.value(i)) )
                * deltaR*deltaR/12.0);
      Numerov(1,numData+1,deltaR,A,chi);
      A.Free;
   END;

PROCEDURE SolveWaveEquationInTwoParts(EB:real; l:Integer;
                                      VAR R,V,chi:dVector;
                                      VAR mismatch:Real);
  {This procedure integrates from both ends, aiming to match}
  {solutions at the furtherest point for which E=V.  It uses}
  {the Numerov algorithm as in CUPSmath.tpu.                }
   VAR
      psi0,dydx : Real;
      i,n       : Integer;
      A         : Dvector;
      k         : Real;
      midVal1,midSlope1 : Real;
      midVal2,midSlope2 : Real;
      nextVal           : Real;
   BEGIN
      A.init(NumData+1);
      A.Put(1,( (l*(l+1)/sqr(deltaR))
                   + energyConvert*(EB+V.value(1)) )
                * deltaR*deltaR/12.0);  { an approximation!! }
      FOR i:=2 TO numData+1 DO
         A.Put(i,( (l*(l+1)/sqr(R.value(i)))
                   + energyConvert*(EB+V.value(i)) )
                * deltaR*deltaR/12.0);
    { Find the last point at which the potential is less than -EB. }
      n := numData+1;
      WHILE (V.value(n)>-EB) AND (n>3) DO n := n-1;
    {  WHILE (V.value(n)>=0) AND (n>0) DO n := n-1;  }
      IF n>=numData-2 THEN n := numData-2;

      psi0 := 200;           {sets arbitrary amplitude of the solution}
      k := sqrt(abs(energyConvert*energy));  { wave number outside the well }
    { Boundary conditions of integration: chi must be an decreasing   }
    { exponential for values of R to the right of the potential well. }
      chi.Put(numData+1,psi0 * exp(-k*R.value(numData+1)));
      chi.Put(numData,psi0 * exp(-k*R.value(numData)));
    { Setting up for the numerov method of integration }
      Numerov(numData+1,n-1,deltaR,A,chi);
      midVal1   := chi.VecPtr^[n];
      midSlope1 := (chi.VecPtr^[n+1]-chi.VecPtr^[n-1])/deltaR;
      nextVal   := chi.VecPtr^[n+1];

    { Initial conditions of integration: chi must be an increasing    }
    { exponential for values of R to the left of the potential well.  }
      chi.Put(1,0);
      chi.Put(2,psi0 * exp((l+1)*ln(deltaR)));
    { Setting up for the numerov method of integration }
      Numerov(1,n+1,deltaR,A,chi);
      midVal2   := chi.VecPtr^[n];
      midSlope2 := (chi.VecPtr^[n+1]-chi.VecPtr^[n-1])/deltaR;
      chi.Put(n+1,nextVal);

      IF midSlope1<0 THEN
         BEGIN
            FOR i:=1 TO n DO chi.Put(i,chi.value(i)*midVal1);
            FOR i:=n+1 TO numData+1 DO chi.Put(i,chi.value(i)*midVal2);
         END
      ELSE
         BEGIN
            FOR i:=1 TO n DO chi.Put(i,-chi.value(i)*midVal1);
            FOR i:=n+1 TO numData+1 DO chi.Put(i,-chi.value(i)*midVal2);
         END;
      mismatch := midVal1*midSlope2 - midVal2*midSlope1;

      A.Free;
   END;

{ ------------------------------------------------------------------- }

FUNCTION EndCorrection(yLast,yPrev:Real):Real;
  {**  If the integrand (y) is of the form: A exp(-kr) for    **}
  {**  values of r greater than r(N), then the integral of y  **}
  {**  from r(N) to infinity is equal to:                     **}
  {**  y(N)/k  or equivalently,  y(N)*deltaR/ln(y(N-1)/y(N))  **}
   VAR
      temp   : Real;
      factor : Real;
   BEGIN
      temp := 0;
      IF abs(yLast)>macheps THEN
         IF (yPrev/yLast)>macheps THEN
            BEGIN
               factor := ln(yPrev/yLast);
               IF factor>macheps THEN
                  temp := yLast*deltaR/ln(yPrev/yLast);
            END;
    { Note: if anything wrong, correction is left as zero }
      endCorrection := temp;
   END;

PROCEDURE NormalizeTheWaveFunction(VAR chi:dVector);
   VAR
      normFactor: real;
      area: real;
      temp1,temp2: real;
      i: integer;
   BEGIN
      area := 0;
      FOR i:=2 TO numdata+1 DO
         BEGIN
            temp1 := chi.value(i)*chi.value(i);
            temp2 := chi.value(i-1)*chi.value(i-1);
            area := area + (temp1+temp2)*deltaR/2;
         END;
      area := area + endCorrection(chi.value(numdata+1)*chi.value(numdata+1),
                                   chi.value(numdata)*chi.value(numdata));
      normfactor := 1/sqrt(area);
      chi.MultScalar(normFactor);
   END;

PROCEDURE CalculateProbabilityFunction(VAR chi,prob:Dvector);
   VAR
      temp: real;
      i: integer;
   BEGIN
            FOR i:=1 TO numdata+1 DO
               BEGIN
                  temp := chi.value(i)*chi.value(i);
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
            l := 0;          {orb ang mom = 0}
            m := 0;          {mag ang mom = 0}
            numLevels := 1;
            eigenVals[1] := 201.68019086;
         END;
      SetPotentialPlottingScale(V);
      spectrumFound := true;
   END;


PROCEDURE RecordAllEigenFunctions(spectrum:eValList; VAR R,V:Dvector;
                                  VAR eigFns:fnArray);
   VAR
      i,n    : Integer;
      energy : Real;
      dummy  : Real;
   BEGIN
      n := spectrum.numLevels;
      IF n>7 THEN n := 7;
      FOR i:=1 TO n DO
         BEGIN
            energy := spectrum.eigenVals[i];
            SolveWaveEquationInTwoParts({using} energy,spectrum.l,R,V,
                                        {giving} eigFns[i],dummy);
            NormalizeTheWaveFunction({changing} eigFns[i]);
         END;
   END;

{ --------------- PROCEDURES ESPECIALLY FOR PART 1 ----------------- }

{ ------------ GENERAL PROCEDURES FOR ANGULAR MOMENTUM -------------- }

PROCEDURE ChooseOrbitalAngMomm(VAR l,m:Integer;
                                VAR spectrumFound:Boolean);
   VAR
      thisScreen : TInputScreen;
      lTemp      : Real;
      escape     : Boolean;
   BEGIN
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputPort(0.00,0.62, 0.71,0.95);
            LoadLine('Choose orbital angular momentum quantum number;');
            LoadLine(Concat('(must be <= ',
                            NumStr(lMax,1,0),
                            ')'));
            LoadLine('                 l = {  }');
            LoadLine('');
            LoadLine('                              [ Ok ]   [Cancel]');
            SetNumber(1,l);
            SetNumberLimits(1,0,5);
            AcceptScreen;
            lTemp  := GetNumber(1);
            escape := Canceled;
            done;
         END;    {with screen1}
      IF NOT escape THEN
         BEGIN
            l := trunc(lTemp);
            spectrumFound := false;
         END;
      IF abs(m)>l THEN m := l;
   END;

PROCEDURE ChooseMagneticAngMomm(l: Integer; VAR m:Integer);
   VAR
      thisScreen : TInputScreen;
      mTemp      : Real;
      escape     : Boolean;
   BEGIN
      WITH thisScreen DO
         BEGIN
            Init;
            DefineInputPort(0.00,0.62, 0.71,0.95);
            LoadLine('Choose orbital angular momentum quantum number;');
            LoadLine(Concat('(must be >= ',
                            NumStr(-l,1,0),
                            ' and <= ',
                            NumStr(l,1,0),
                            ')'));
            LoadLine('                 m = {  }');
            LoadLine('');
            LoadLine('                              [ Ok ]   [Cancel]');
            SetNumber(1,m);
            SetNumberLimits(1,-l,l);
            AcceptScreen;
            mTemp  := GetNumber(1);
            escape := Canceled;
            done;
         END;    {with screen1}
      IF NOT escape THEN
            m := trunc(mTemp);
   END;

PROCEDURE CalculateLegendreCoefficients(l,m:Integer; VAR A:Coeffnts);
   VAR
      i,j,m1 : Integer;
      norm   : Real;
      norm1  : Real;
   BEGIN
      A[2*l+1] := 1;
      IF l>0 THEN
         BEGIN
          { first calculate coefficients of (z^2-1)^l }
            FOR j:=1 TO 2*l DO
               A[j] := 0;
            FOR i:=1 TO l DO
               A[2*l-2*i+1] := -(l+1-i)*A[2*l-2*i+3]/i;
             { now coefficients of d^l/dz^l }
               FOR i:=1 TO l DO
                  BEGIN
                     FOR j:=1 TO 2*l DO
                        A[j] := (j) * A[j+1];
                     A[2*l+1] := 0;
                  END;
         END;
    { now calculate associated legendre coefficients }
      m1 := abs(m);
      IF m1>0 THEN
       { coefficients of d^m/dz^m }
         FOR i:=1 TO m1 DO
            BEGIN
               FOR j:=1 TO 2*l DO
                  A[j] := (j) * A[j+1];
               A[2*l+1] := 0;
            END;
    { now get the normalization factor }
      norm  := 1;
      norm1 := 2*l + 1;
      IF l>0 THEN
         BEGIN
          { normalization for P_l }
            FOR i:=1 TO l DO
               norm := norm * 2 * i;
          { normalization for Y^m_l }
            FOR i:=1 TO l+m1 DO
               norm1 := norm1 / i;
            IF m1<l THEN
               FOR i:=1 TO l-m1 DO
                  norm1 := norm1 * i;
         END;
      norm1 := sqrt(norm1);
    { combine both normalizations }
      FOR j:=1 TO 2*l+1 DO
         A[j] := A[j] * norm1 / norm;
   END;

PROCEDURE CalculateSphericalHarmonic(l,m:Integer; VAR X,Z,P:DVector);
   VAR
      A       : Coeffnts;  {defined as ARRAY[1..2*lMax+1] of Integer}
      i,j     : Integer;
      dTheta  : Real;
      thisCos : Real;
      thisSin : Real;
      cosToL  : Real;
      sinToM  : Real;
   BEGIN
      dTheta := 2 * pi / numData;
      CalculateLegendreCoefficients(l,m,A);
      FOR i:=1 TO numData+1 DO
         BEGIN
            thisCos := cos((i-1)*dTheta);
            thisSin := sin((i-1)*dTheta);
            cosToL  := 1;
            P.put(i,0);
            FOR j:=1 TO (2*l+1) DO
               BEGIN
                  P.put(i,P.value(i) + A[j]*cosToL);
                  cosToL := cosToL * thisCos;
               END;
            sinToM  := 1;
            IF abs(m)>0 THEN
               FOR j:=1 TO abs(m) DO
                  sinToM := sinToM * thisSin;
            P.put(i,P.value(i)*sinToM);
            Z.put(i, abs(P.value(i)) * thisCos);
            X.put(i, abs(P.value(i)) * thisSin);
         END;
   END;

PROCEDURE DisplaySphericalHarmonic(l,m:Integer);
   VAR
      X,Z,P   : DVector;
      axisLen : Real;
      i       : Integer;
   BEGIN
      X.init(numData+1);
      Z.init(numData+1);
      P.init(numData+1);
      ClearMUPPETport;
      SetColor(dullColor);
      OpenColoredViewport(9);
      axisLen := 1.10 * sqrt(2*l+1);
      DefineScale(5, -axisLen,axisLen, -axisLen,axisLen);
      DrawAxis(0,0, 0.5,0.5,dullColor);
      PutLabel(left,  'Z');
      PutLabel(bottom,'X');
      SetColor(angColor);
      Print(3,2,Concat('l = ',
                       NumStr(l,1,0),
                       ';  m = ',
                       NumStr(m,1,0)));
      CalculateSphericalHarmonic(l,m,X,Z,P);
      FOR i:=2 TO numData+1 DO
         BEGIN
            IF P.value(i)>=0 THEN
               SetColor(posColor)
            ELSE
               SetColor(negColor);
            PlotLine(X.value(i-1),Z.value(i-1), X.value(i),Z.value(i));
         END;
      X.free;
      Z.free;
      P.free;
   END;

PROCEDURE Display3DSphericalHarmonic(l,m:Integer);
   VAR
      X,Z,P   : DVector;
      axisLen,min,max : Real;
      i       : Integer;
      key     : byte;
      str     : string;
      hotKeys : THotKeys;
      spherical : TSphericalHar;
      NewData : boolean;
      ch : char;

   procedure Draw;
   begin
      if m=0 then ch:=' ' else ch:=chr(241);
      str := Concat('l = ', NumStr(l,1,0), ';  m = ',ch, NumStr(m,1,0));
      axisLen := 1.10 * sqrt(2*l+1);
      DefineScale(5, -axisLen,axisLen, -axisLen,axisLen);
      CalculateSphericalHarmonic(l,m,X,Z,P);
      Spherical.PlotFigure(X,Z,P,str);
      NewData := false;
   end;

   BEGIN
      X.init(numData+1);
      Z.init(numData+1);
      P.init(numData+1);
      ClearMUPPETport;
      HotKeys.init(2);
      HotKeys.Key[1] := 'F1-Help';
      HotKeys.Key[2] := 'F10-Menu';
      HotKeys.Display;
      Spherical.init(9,l,m);
{      Spherical.init(9); }
      Draw;
      REPEAT
         checkForEvents;
         IF HotKeys.Pressed(key) THEN
            IF key=1 THEN
               DisplayHelpScreen(angMomHS);
         IF Spherical.Changed(l,m) THEN Draw;
      UNTIL (key=2);
      Spherical.done;
      HotKeys.Clear;
      X.free;
      Z.free;
      P.free;
   END;

{ -------- GENERAL PROCEDURES FOR PROBABILITY CLOUD PLOTTING ------- }

PROCEDURE WriteAngMomQuantumNumbers;
   BEGIN
      SetColor(angColor);
      Print( 3,1,Concat('l = ',
                        NumStr(spectrum.l,1,0),
                        ';  m = ',
                        NumStr(spectrum.m,1,0)));
   END;

PROCEDURE PrepareToPlotProbCloud;
   VAR
      i       : Integer;
      axisLen : Real;
   BEGIN
      ClearMUPPETport;
      SetColor(dullColor);
      OpenColoredViewport(13);
      axisLen := highestR/1.414;
      DefineScale(5, -axisLen,axisLen, -axisLen,axisLen);
      DrawAxis(0,0, ticR,ticR,dullColor);
      PutLabel(bottom, Concat('X (',
                              'nm',
                              ')'));
      PutLabel(left,   Concat('Z (',
                              'nm',
                              ')'));
      SetColor(graphBackColor);
      PlotLine(-0.90*axisLen,0, 0.90*axisLen,0);
      PlotLine(0,-0.90*axisLen, 0,0.90*axisLen);
      WriteAngMomQuantumNumbers;
   END;

PROCEDURE PrepareForRandomRadius(deltaProb:Real; VAR R,chi:DVector;
                                 VAR radProb:DVector);
   VAR
      i,j,jOld : Integer;
      intProb  : DVector;
      nextprob : Real;
      temp     : Real;
   BEGIN
      intProb.init(numData+1);
    { first find the normalized integrated probability }
      intProb.Put(1,0);
      FOR i:=2 TO numData+1 DO
         intProb.Put(i,intProb.value(i-1)+sqr(chi.value(i))/R.value(i));
      FOR i:=2 TO numData+1 DO
         intProb.Put(i,intProb.value(i)/intProb.value(numData+1));

    { now find a radial scale corresponding to linear integrated prob }
      radProb.Put(1, 0.0);
      j := 1;
      FOR i:=2 TO numData DO
         BEGIN
            nextProb := (i-1) * deltaProb;
            jOld := j;
            REPEAT
               j := j+1;
            UNTIL nextProb<=intProb.value(j);
          { now calculate equivalent radius using jOld and j }
            IF nextProb=intProb.value(j) THEN
               radProb.Put(i, R.value(j))
            ELSE
               BEGIN
                  temp := R.value(jOld) + (nextProb - intProb.value(jOld))
                                 * (R.value(j) - R.value(jOld))
                                 / (intProb.value(j) - intProb.value(jOld));
                  radProb.Put(i,temp);
                  j := j-1;
               END;
         END;
      radProb.Put(numData+1,R.value(numData+1));
    { now clean up }
      intProb.free;
   END;

PROCEDURE PrepareForRandomAngle(deltaProb:Real; l,m:Integer;
                                VAR angProb:DVector);
   VAR
      i,j,jOld : Integer;
      intProb  : DVector;
      sphHarm  : DVector;
      dTheta   : Real;
      nextprob : Real;
      tempry   : Real;
   BEGIN
      sphHarm.init(numData+1);
      intProb.init(numData+1);
    { First calculate the spherical harmonic using l and m }
      CalculateSphericalHarmonic(l,m,
                {dummy} temp, {dummy} intProb, {genuine} sphHarm);
    { now find the normalized integrated probability }
      intProb.Put(1,0);
      FOR i:=2 TO numData+1 DO
         intProb.Put(i,intProb.value(i-1)+sqr(sphHarm.value(i)));
      FOR i:=2 TO numData+1 DO
         intProb.Put(i,intProb.value(i)/intProb.value(numData+1));

    { now find a angular scale corresponding to linear integrated prob }
      dTheta := 2 * pi / numData;
      angProb.Put(1, 0.0);
      j := 1;
      FOR i:=2 TO numData DO
         BEGIN
            nextProb := (i-1) * deltaProb;
            jOld := j;
            REPEAT
               j := j+1;
            UNTIL nextProb<=intProb.value(j);
          { now calculate equivalent radius using jOld and j }
            IF nextProb=intProb.value(j) THEN
               angProb.Put(i, (j-1)*dTheta)
            ELSE
               BEGIN
                  tempry :=  (jOld-1)*dTheta
                                 + (nextProb - intProb.value(jOld))
                                 * (j - jOld)*dTheta
                                 / (intProb.value(j) - intProb.value(jOld));
                  angProb.Put(i,tempry);
                  j := j-1;
               END;
         END;
      angProb.Put(numData+1,(numData*dTheta));
    { now clean up }
      intProb.free;
      sphHarm.free;
   END;

PROCEDURE PrepareToCalculateCloud(l,m:Integer;
                                  VAR R,chi:DVector;
                                  VAR deltaProb:Real;
                                  VAR radProb,angProb:DVector);
   BEGIN
      radprob.init(numData+1);
      angProb.init(numData+1);
      deltaProb := 1/numData;
      PrepareForRandomRadius(deltaProb,R,chi,radProb);
      PrepareForRandomAngle(deltaProb,l,m,angProb);
   END;

PROCEDURE FinishCalculatingCloud(VAR radProb,angProb:DVector);
   BEGIN
      radProb.free;
      angProb.free;
   END;


PROCEDURE ChooseARandomPoint(axisLen:Real; VAR x,z:Real);
   BEGIN
      x := axisLen*(2*random - 1);
      z := axisLen*(2*random - 1);
   END;

PROCEDURE ChooseARandomCoordinate(VAR Prob:DVector; VAR coord:Real);
   VAR
      rNum : Real;
      rInt : Integer;
   BEGIN
      rNum := numData * random;
      rInt := trunc(rNum)+1;
      coord := Prob.Value(rInt);
      IF frac(rNum)>0 THEN
         coord := coord
                + frac(rNum)*(Prob.Value(rInt+1)-Prob.value(rInt));
   END;

PROCEDURE MarkARandomPoint(VAR radProb,angProb:DVector);
   VAR
      xP,zP     : Real;
      xSc,zSc   : Integer;
      rad,theta : Real;
   BEGIN
      SelectViewport(13);
      SelectScale(5);
      ChooseARandomCoordinate(radProb,rad);
      ChooseARandomCoordinate(angProb,theta);
      xP := rad * sin(theta);
      zP := rad * cos(theta);
      Map(xP,zP,xSc,zSc);
      PutPixel(xSc,zSc,probColor);
   END;

PROCEDURE SetUpcloudKeys(VAR cloudKeys:Thotkeys; VAR fastCount:Integer);
   BEGIN
      fastCount := 50;     {large number means slow plotting}
      WITH cloudKeys DO
         BEGIN
            Init(6);
            key[1] := 'F1-Help';
            key[2] := 'F2-Stop';
            key[3] := 'F5-Faster';
            key[4] := 'F7-Restart';
            key[5] := 'F8-New n';
            key[6] := 'F10-Menu';
         END;   {with}
   END;

PROCEDURE MakeKeyChoice(n:Integer; VAR fastCount:Integer;
                        VAR cloudKeys:THotKeys;
                        VAR startFlag,nextN,finished:Boolean);
      BEGIN
         CASE n OF
          1 : {F1 = Help}
              DisplayHelpScreen(cloudHS);
          2 : {F2 = Run or stop}
              BEGIN
                 startFlag := NOT startFlag;
                 IF startFlag THEN
                    cloudKeys.key[2] := 'F2-Stop'
                 ELSE
                    cloudKeys.key[2] := 'F2-Run';
                 cloudKeys.Display;
              END;
          3 : {F5 = Fast/Slow}
              BEGIN
                 IF fastCount>1 THEN
                    BEGIN
                       fastCount := 1;   {small means fast}
                       cloudKeys.key[3] := 'F5-Slower';
                    END
                 ELSE
                    BEGIN
                       fastCount := 50;
                       cloudKeys.key[3] := 'F5-Faster';
                    END;
                 cloudKeys.Display;
              END;
          4 : {F7 = Restart}
               BEGIN
                  PrepareToPlotProbCloud;
                  startFlag := true;
                  cloudKeys.key[2] := 'F2-Stop';
                  cloudKeys.Display;
               END;
          5 : {F8 = New n}
               nextN := true;
          6 : {Esc = finished}
               finished := true;
         END;   {case}
      END;



{ ------------ GENERAL PROCEDURES FOR POTENTIAL WELLS -------------- }

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
      DefineScale(1, lowestR,highestR, thisLowV-offsetV,thisHighV+offsetV);
   END;

PROCEDURE ChooseAPotentialWell(n:Integer;
                               VAR thisWell:wellParams;
                               VAR V:dVector;
                               VAR spectrum:eValList);
   BEGIN
      thisWell.wellType := n;
      SetDefaultWellParameters({altering} thisWell);
      CalculateWellDVector({using} thisWell, {giving} V,spectrumFound);
      spectrum.l := 0;
      spectrum.m := 0;
      spectrum.numLevels := 0;  {spectrum still has to be calculated}
   END;

PROCEDURE DrawThePotentialWellAsBackground(VAR R,V:Dvector;
                                           VAR examine:Boolean);
   BEGIN
      ClearMUPPETport;
      examine := false;
      SetColor(dullColor);
      OpenColoredViewport(2);
      SelectScale(1);
      DrawAxis(0,0, ticR,ticV,dullColor);
      PutLabel(right,concat('energy (',
                            globalUnits.nameE,
                            ')'));
      PutLabel(bottom,concat('radial distance (',
                             globalUnits.nameL,
                             ')'));
      SetColor(potlColor1);
      PlotDVectors(R,V,1,numData+1);
      WriteAngMomQuantumNumbers;
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
            IF GetBoolean(5)
               THEN finisht := false
               ELSE finisht := true;
            IF NOT escape THEN
               ReadParticularParams(thisScreen, thisWell);
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
                     DrawThePotentialWellAsBackground(R,V,finisht);
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
            DefineInputport(0.03,0.95, 0.06,0.84);
            ListTypesOfPerturbation(n,thisScreen);
            SetBoolean(thisPerturbn.perturbnType,true);
            LoadLine('');
            LoadLine(concat('       and input the coefficient (A) = ',
                            '{     } (units of ',
                            globalUnits.nameE,
                            '/',
                            globalUnits.nameL,
                            '^i)'));
            LoadLine('                      (where i is power of r in the perturbation.)');
            LoadLine(' ');
            LoadLine('  NOTE : A must be chosen small enough that the perturbation is ');
            LoadLine('         never greater than 10% of the maximum depth of the well.');
            LoadLine(' ');
            LoadLine('  Select <  Ok  > when you are satisfied and want to proceed.');
            LoadLine('         <Cancel> if you want to exit without having made any changes.');
            LoadLine('         < View > to see the effect of the changes before proceeding.');
            LoadLine('');
            LoadLine('                       [  Ok  ]     [Cancel]     [ View ]');
            SetNumber(n+1,thisPerturbn.coefficient);
            SetNumberLimits(n+1,-100,100);

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
                     DrawThePotentialWellAsBackground(R,V,finisht);
                     AskIfThisIsAcceptible(finisht);
                  END
               ELSE
                  SetPotentialPlottingScale(V);
            END;
      UNTIL finisht OR escape;
      IF NOT escape THEN
         thisPerturbn := tempPerturbn;
   END;

PROCEDURE ClearThePotentialPlot(VAR R,V:Dvector; VAR examine:Boolean);
   BEGIN
      SetColor(dullColor);
      OpenColoredViewport(2);
      SelectScale(1);
      DrawAxis(0,0, ticR,50,dullColor);
      SetColor(potlColor1);
      PlotDVectors(R,V,1,numData+1);
      examine := false;
      WriteAngMomQuantumNumbers;
   END;

PROCEDURE PrepareToFindWaveFunctions(VAR R,V:dVector;
                                     VAR EB:real; VAR examine:Boolean);
   BEGIN
      DrawThePotentialWellAsBackground(R,V,examine);
      EB := -0.8*lowestV;    {default value of the binding energy}
      choice := 1;           {default setting of choice index}
   END;

PROCEDURE ChooseMeansOfInput(rowNum:Integer; VAR withMouse:Boolean);
   BEGIN
      IF rowNum=1 THEN withMouse := true
                  ELSE withMouse := false;
   END;

PROCEDURE LocalMousePosn(xGlobal,yGlobal:Real; VAR R,Y:REAL;
                         VNum,SNum:integer; VAR Inside:boolean);
   VAR
      XFactor,YFactor: REAL;
   BEGIN
      WITH Views[VNum] DO
         WITH GraphScales[SNum] DO
            BEGIN
               xFactor := (xmax-xmin)/(Vx2-Vx1);
               R := (xFactor*(xGlobal-Vx1)) + xmin;
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
      thisScreen : TInputScreen;
      r,y        : Real;
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

      OpenHiddenViewport(10);
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
         inside := false;
         CheckForEvents;
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
               LocalMouseposn(event.X,event.Y,r,y,2,1,inside);
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
   BEGIN
      WITH thisScreen DO
         BEGIN
            init;
            DefineInputPort(0.05,0.95, 0.05,0.10);
            LoadLine(concat(' Choose a binding energy  {      }',
                            globalUnits.nameE,
                            ':   [clear ]   [  Ok  ]   [Cancel]'));
            SetNumber(1,EB);
            SetNumberLimits(1, -graphScales[1].Ymax,-graphScales[1].Ymin);
            finished := false;
            erase := false;
            SetColor(white);
            AcceptScreen;
            IF GetBoolean(2)
                     THEN erase := true;
            IF GetBoolean(4)
               THEN finished := true;
            IF (NOT erase) AND (NOT finished) THEN
               EB := thisScreen.getNumber(1);
            done;
         END;   {with thisScreen}
   END;

PROCEDURE ShowTheEnergyLevel(EB:real);
   BEGIN
      SelectViewport(2);
      SelectScale(1);
      SetColor(engyColor);
      PlotLine(lowestR,-EB, highestR,-EB);
   END;

PROCEDURE DrawWaveFunctionOnEnergyLevel(EB:real; VAR R,chi:dVector);
   VAR
      i: integer;
      scaleFactor: real;
   BEGIN
      scaleFactor := 0.1*abs(lowestV)*sqrt(highestR);
      FOR i:=1 TO numData+1 DO
         temp.Put(i,-EB + scaleFactor*chi.value(i));
      SelectViewport(2);
      SelectScale(1);
      PlotDVectors(R,temp,1,numData+1);
   END;

PROCEDURE DisplayProbabilityFunction(spectrum:eValList; n:integer;
                               VAR R,prob:Dvector);
   VAR
      i  : Integer;
      EB : Real;
      scaleFactor: real;
   BEGIN
      scaleFactor := 0.1*abs(lowestV)*highestR;
      EB := spectrum.eigenVals[n];
      SetColor(probColor);
      FOR i:=1 TO numData+1 DO
         temp.Put(i,-EB + scaleFactor*prob.value(i));
      SelectViewport(2);
      SelectScale(1);
      PlotDVectors(R,temp,1,numData+1);
   END;

PROCEDURE ExamineGraphWithMouse(EB:Real; VAR R,chi:DVector);
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
      Print(13,1,'Click within the graph to measure r and u(r).');

      finisht := false;
      oldX0 := lowestR;
      theseKeys.Display;
      SelectViewport(2);
      SelectScale(1);
      SetColor(lightmagenta);
      DrawWaveFunctionOnEnergyLevel(EB,R,chi);
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
                     WHILE x0>R.value(i) DO i := i+1;
                     y2 := chi.value(i);
                     y1 := chi.value(i-1);
                     x2 := R.value(i);
                     x1 := R.value(i-1);
                     amp := y1 + (x0-x1)*(y2-y1)/(x2-x1);
                     Message(concat('     r = ',
                                    NumStr(x0,4,4),
                                    ' (units of ',
                                    globalUnits.nameL,
                                    ');      u(r) = ',
                                    NumStr(amp,6,4)))
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
      highestPsi := 8*sqrt(highestR-lowestR);
      DefineScale(2, lowestR,highestR, -highestPsi,+highestPsi);
      Message('');
   END;

PROCEDURE DisplayIntermediateSolution(VAR chi:DVector);
   BEGIN
      SetColor(dullColor);
      IF ViewportNumber=3
         THEN SelectViewport(3)
         ELSE OpenColoredViewport(3);
      SelectScale(2);
      PlotLine(lowestR,0, highestR,0);
      SetColor(psiColor);
      PlotDVectors(R,chi,1,numData+1);
   END;

FUNCTION NumberOfNodes(EB:Real; VAR chi:Dvector):Integer;
   VAR
      i,n: Integer;
      k: Real;
   BEGIN
      n := 0;
      FOR i:=2 TO numData+1 DO
       { When chi changes sign this signifies a node }
         IF ((chi.value(i-1)>=0) AND (chi.value(i)<0))
            OR ((chi.value(i-1)<0) AND (chi.value(i)>=0))
             THEN n := n+1;
       { But there might also be one beyond r = highestR }
         k := sqrt(abs(energyConvert*EB));  { wave number outside the well }
         IF abs(chi.value(numData))>abs(exp(k*deltaR)*chi.value(numData+1))
            THEN n := n + 1;
      numberOfNodes := n;
   END;

FUNCTION Yvalue(Xvalue:Real; VAR nValue:Integer):Real;
 { This is the function for the Search procedure }
   VAR
      energy,k: Real;
   BEGIN
      energy := Xvalue;
      SolveWaveEquation(energy,spectrum.l,R,V,chi);
      NormalizeTheWaveFunction(chi);
      DisplayIntermediateSolution(chi);
      nValue := NumberOfNodes(energy,chi);
      k := sqrt(abs(energyConvert*energy));  { wave number outside the well }
      Yvalue := chi.value(numdata) - exp(k*deltaR)*chi.value(numData+1);
    { This means the last two points are in the same ratio as the first two }
   END;

FUNCTION FastYvalue(Xvalue:real):real;
 { This is the function for the FastSearch procedure }
   VAR
      k,mismatch : Real;
   BEGIN
      energy := Xvalue;
      SolveWaveEquationInTwoParts(energy,spectrum.l,R,V,chi,mismatch);
      FastYvalue := mismatch;
   END;

PROCEDURE DisplayResultOfSearch(EB:real; VAR R,chi:Dvector;
                                         VAR examine:Boolean);
   BEGIN
      DrawThePotentialWellAsBackground(R,V,examine);
      ShowTheEnergyLevel(EB);
      SetColor(psiColor);
      DrawWaveFunctionOnEnergyLevel(EB,R,chi);
      PutLabel(inside,concat('Binding energy = ',
                             NumStr(EB,7,2),
                             ' ',
                             globalUnits.nameE));
   END;

PROCEDURE PrepareToDisplaySpectrum(VAR examine:Boolean);
   BEGIN
      DrawThePotentialWellAsBackground(R,V,examine);
   END;

PROCEDURE DisplayEnergyAndNum(EB:Real; VAR chi:dVector);
   VAR
      n : Integer;
   BEGIN
      n := NumberOfNodes(EB,chi);
      Message(concat('  EB := ',
                     NumStr(EB,5,2)
                     ,'  :  number of nodes = ',
                     NumStr(n,2,0)));
   END;

PROCEDURE FindNumberOfEigenvalues(VAR R,V,chi:Dvector;
                                  VAR spectrun:eValList);
   VAR
      EB: real;
      n : integer;
   BEGIN
      EB := 0;
      SolveWaveEquation({using} EB,spectrum.l,R,V, {giving} chi);
      spectrum.numLevels := NumberOfNodes({using} EB,chi);
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
                               VAR R,V,chi:Dvector;
                               VAR spectrum:eValList);
   VAR
      firstNumNodes,n       : Integer;
      newEnergy,eigenEnergy : Real;
   BEGIN
      SolveWaveEquation({using} firstEnergy,spectrum.l,R,V, {giving} chi);
      firstNumNodes := NumberOfNodes({using} firstEnergy,chi);

      CASE abs(firstNumNodes-secondNumNodes) OF
       0 : {no eigenvalues in this range}
           EXIT;
       1 : {one eigenvalue in here, go and find it}
           BEGIN
              FuncFastYValue := FastYValue;    {Set up for search}
              FastSearchForZero(firstEnergy,secondEnergy,eigenEnergy);
              ShowTheEnergyLevel(eigenEnergy);
              IF firstNumNodes<secondNumNodes
                 THEN n := secondNumNodes
                 ELSE n := firstNumNodes;
              Message(concat('    Level number ',
                             Numstr(n,2,0),
                             '   :   Binding energy = ',
                             Numstr(eigenEnergy,7,2),
                             ' ',
                             globalUnits.nameE));
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
                                   soundOn,R,V,chi, {adding to} spectrum);
              FindLevelRecursively(newEnergy,secondEnergy,secondNumNodes,
                                   soundOn,R,V,chi, {adding to} spectrum);
           END;
      END;    {case}
   END;

PROCEDURE FindAllEigenvalues(soundOn:Boolean; VAR R,V,chi:Dvector;
                             VAR spectrum:eValList; VAR spectrumFound:Boolean);
   VAR
      lowEnergy,highEnergy: real;
      lowNumNodes,highNumNodes: integer;
   BEGIN
      highEnergy := abs(lowestV);
      highNumNodes := 0;
      lowEnergy  := 0;
      FindLevelRecursively({using} lowEnergy,highEnergy,highNumNodes,
                           {using} soundOn,R,V, {making} chi,
                           {completing} spectrum);
      Message('');
      spectrumFound := true;    {signifies you can ask to see the eigenfunctions}
   END;

PROCEDURE PrepareToDisplayWaveFunctions(VAR n:integer; VAR examine:Boolean);
   BEGIN
      DrawThePotentialWellAsBackground(R,V,examine);
      n := 1;
   END;

PROCEDURE LabelTheGraph(probFlag:Boolean);
   BEGIN
      SelectViewport(2);
      SetColor(psiColor);
      IF NOT probFlag THEN
         PutLabel(top,'r * wave function')
      ELSE
         BEGIN
            PutLabel(top,'r * wave function                      ');
            SetColor(probColor);
            PutLabel(top,'                    probability density');
         END;
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
            done;
         END;   {with thisScreen}
   END;

PROCEDURE InputNewLevelNumber(VAR spectrum:eValList;
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
                                  ':   [  Ok  ]     [Cancel]'));
                  SetNumber(1,n);
                  SetNumberLimits(1, 1,spectrum.numLevels);
                  REPEAT
                     AcceptScreen;
                     finisht := false;
                     numFound := false;
                     IF GetBoolean(2) THEN
                        BEGIN
                           n := round(thisScreen.getNumber(1));
                           IF n<=spectrum.numLevels
                              THEN numFound := true;
                        END;
                     IF GetBoolean(3)
                        THEN finisht := true;
                  UNTIL numFound OR finisht;
               END;     {if numlevels}
            done;
         END;   {with thisScreen}
   END;

PROCEDURE DisplayEigenFunction(spectrum:eValList; n:integer;
                               VAR R,V,chi,prob:Dvector);
   VAR
      energy,temp,dummy : Real;
      i                 : Integer;
   BEGIN
      WITH spectrum DO
         BEGIN
            IF n>numLevels THEN EXIT;
            energy := eigenVals[n];
            ShowTheEnergyLevel({using} energy);
            SolveWaveEquationInTwoParts({using} energy,l,R,V,
                                        {giving} chi,dummy);
            NormalizeTheWaveFunction({using} chi);
            CalculateProbabilityFunction({using} chi, {giving} prob);
            SetColor(psiColor);
            DrawWaveFunctionOnEnergyLevel({using} energy,R,chi);
            Message(concat('    Level number ',
                           Numstr(n,2,0),
                           '   :   Binding energy = ',
                           Numstr(energy,7,2),
                           ' ',
                           globalUnits.nameE));
         END;    {with}
   END;

PROCEDURE ExploreThisPotential(colNum,rowNum:Integer;
                     VAR first,examine,spectrumFound,eraseFirst:Boolean);
   VAR
      i,exitcode      : Integer;
      fastCount       : Integer;
      withMouse,zoom  : Boolean;
      erase,finisht   : Boolean;
      startFlag       : Boolean;
      showProb        : Boolean;
      radProb,angProb : DVector;
      deltaProb       : Real;
      dummy           : Real;
   BEGIN
      zoom := false;
      CASE colNum OF
         3 : {Choose a well to work with}
             BEGIN
                ChooseAPotentialWell({using} rowNum,
                                     {giving} thisWell,V,spectrum);
                SetPotentialPlottingScale({using} V);
             END;
         4 : {Choose a well to work with}
             BEGIN
                CASE rowNum OF
                 1..7 : {Choose a well shape}
                     BEGIN
                        ChooseAPotentialWell({using} rowNum,
                                             {giving} thisWell,V,spectrum);
                        SetPotentialPlottingScale({using} V);
                     END;
                 8 : MakeAnyChanges({changing} thisWell);
                 9 : ChoosePerturbation({using} thisWell,
                                        {giving} thisPerturbn);
                END;       {case rowNum}
                CalculateWellDVector({using} thisWell,
                                     {giving} V,spectrumFound);
                AddPerturbationToDVector({using} thisPerturbn, {giving} V);
             END;
         5 : {Show method of solution}
             BEGIN
                first := true;
                CASE rowNum OF
                  1..2 : {Solve for a particular energy}
                      BEGIN
                         zoom := false;
                         DrawThePotentialWellAsBackground(R,V,examine);
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
                                     SolveWaveEquation({using} energy,
                                                               spectrum.l,R,V,
                                                       {giving} chi);
                                     DisplayEnergyAndNum({using} energy,chi);
                                     SetColor(psiColor);
                                     NormalizeTheWaveFunction({changing} chi);
                                     DrawWaveFunctionOnEnergyLevel
                                                     ({using} energy,R,chi);
                                     examine := true;
                                  END
                               ELSE
                                  ClearThePotentialPlot({using} R,V,examine);
                         UNTIL finisht;
                         SetPotentialPlottingScale({using} V);
                         ClearUpTheDisplay;
                      END;
                  3 : {Hunt for an eigenvalue}
                       BEGIN
                          PrepareToHunt;
                          FuncYValue := YValue;    {Set up for search}
                          SearchForZero('asymptote (increasing exponential)',
                                        'energy', lowestV, energy);
                          IF NOT abortedSearch THEN
                             BEGIN
                                Announce
                                ('       Press <Enter> or click mouse to continue.      ');
                                NormalizeTheWaveFunction({changing} chi);
                                DisplayResultOfSearch
                                          ({using} energy,R,chi,examine);
                                examine := true;
                             END;
                          eraseFirst := false;  {don't erase the screen}
                       END;
                  4 : {Examine solution}
                       BEGIN
                          ExamineGraphWithMouse({using} energy,R,chi);
                          examine := false;
                          ClearMUPPETport;
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
                            FindNumberOfEigenvalues({using} R,V,chi,
                                                    {giving} spectrum);
                            FindAllEigenvalues({using} soundOn,R,V,chi,
                                       {completing} spectrum,spectrumFound);
                            eraseFirst := false;
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
                                                   spectrum,n,R,V,chi,prob);
                                     IF rowNum=3 THEN
                                        DisplayProbabilityFunction
                                                ({using} spectrum,n,R,prob);
                                     examine := true;
                                     energy := spectrum.eigenVals[n];
                                     eraseFirst := false; {don't erase screen}
                                  END;
                            UNTIL finisht;
                            Message('');
                         END;
                     4 : {examine the current wave function}
                         BEGIN
                            ExamineGraphWithMouse({using} energy,R,chi);
                            examine := false;
                            ClearMUPPETport;
                         END;
                     5 : {Plot the probability cloud}
                         REPEAT
                            InputNewLevelNumber({using} spectrum,
                                              {giving} n,finisht,examine);
                            IF NOT finisht THEN
                               BEGIN
                                  SolveWaveEquationInTwoParts(
                                        {using} spectrum.eigenVals[n],
                                                spectrum.l,R,V,
                                        {giving} chi,dummy);
                                  NormalizeTheWaveFunction({using} chi);
                                  PrepareToCalculateCloud(
                                        {using} spectrum.l,spectrum.m, R,chi,
                                        {giving} deltaProb,radProb,angProb);
                                  PrepareToPlotProbCloud;
                                  SetUpCloudKeys({setting} cloudKeys,fastCount);
                                  cloudKeys.Display;
                                  Message('');
                                  finisht   := false;
                                  nextN     := false;
                                  startFlag := true;
                                  i := 0;
                                  REPEAT
                                     i := (i + 1) mod fastCount;
                                     IF startFlag THEN
                                        IF i=0 THEN
                                           MarkARandomPoint({using} radProb,angProb);
                                     CheckForEvents;
                                     IF cloudKeys.Pressed(key) THEN
                                        MakeKeyChoice(key,fastCount,cloudKeys,
                                             startFlag,nextN,finisht);
                                  UNTIL (finisht OR nextN);
                                  cloudKeys.Clear;
                                  FinishCalculatingCloud({clearing} radProb,angProb);
                                  eraseFirst := false;
                               END;
                         UNTIL finisht;
                     6 : ; {this choice is blank}
                     7 : {toggle the sound on/of facility}
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
            OpenViewport(11);
            HideCursor;
            SetColor(white);
            SetFillStyle(SolidFill, blue);
            WITH Views[11] DO bar(0,0,Vx2-Vx1,Vy2-Vy1);
            WITH Views[11] DO rectangle(0,0,Vx2-Vx1,Vy2-Vy1);
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
      i     : Integer;
      dummy : Real;

   PROCEDURE SetStandardParams(VAR thisParams:PsiParam);
      VAR
         i : Integer;
      BEGIN
         WITH thisParams DO
            BEGIN
               quantNum := 1;
               coeffs[1] := 0.7071;
               coeffs[2] := 0.7071;
               FOR i:=3 TO 7 DO
                  coeffs[i] := 0;
            END;    {with}
      END;

   BEGIN
      WITH thisOverlap DO
         BEGIN
            energy := spectrum.eigenVals[1];
            SolveWaveEquationInTwoParts({using} energy,spectrum.l,R,V,
                                        {giving} bra,dummy);
            NormalizeTheWaveFunction({changing} bra);
            operatorNum := 1;
            FOR i:=1 TO numData+1 DO
               ket.Put(i,bra.value(i));
            IF currentFileName='' THEN
               BEGIN
                  SetStandardParams(braParams);
                  SetStandardParams(ketParams);
                  ketParams.funcStr := 'H(r-0.03)*(1-H(r-0.09))';
               END;
            braDrawn := false;
            ketDrawn := false;
            psi1Name := 'psi1';
            psi2Name := 'psi2';
            n := 1;
         END;    {with}
   END;

{ ------------ GENERAL PROCEDURES FOR MENUS AND HOT-KEYS-------------- }

PROCEDURE PrepareToDrawBra(VAR R,V:DVector);
   BEGIN
      SetColor(dullColor);
      OpenColoredViewport(5);
      SelectScale(1);
      PlotDvectors(R,V,1,numData+1);
      Print(1,1,psi1name);
   END;

PROCEDURE PrepareToDrawKet(VAR R,V:DVector);
   BEGIN
      SetColor(dullColor);
      OpenColoredViewport(6);
      SelectScale(1);
      PlotDvectors(R,V,1,numData+1);
      Print(1,1,concat(operators[thisOverlap.operatorNum],
                       '  * ',
                       psi2Name));
   END;

PROCEDURE PrepareToDrawBraKet(VAR R,V:DVector);
   BEGIN
      SetColor(dullColor);
      OpenColoredViewport(7);
      SelectScale(1);
      PlotDvectors(R,V,1,numData+1);
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

PROCEDURE PrepareToStudyWaveFunctions(VAR R,V:Dvector);
   BEGIN
      SetColor(dullColor);
            ClearMUPPETport;
            PrepareToDrawBra(R,V);
            PrepareToDrawKet(R,V);
            PrepareToDrawBraKet(R,V);
            DefineScale(2,lowestR,highestR,-1.0,1.0);
            DefineScale(3,lowestR,highestR,-1.0,1.0);
            DefineScale(4,lowestR,highestR,-1.0,1.0);
   END;

PROCEDURE ChooseAnEigenFunction(n:Integer; spectrum:eValList;
                                VAR R,V,chi:Dvector;
                                VAR psiParams:PsiParam;
                                VAR psiName:String;
                                VAR escape:Boolean);
   VAR
      dummy : Real;
   BEGIN
      energy := spectrum.eigenVals[n];
      SolveWaveEquationInTwoParts({using} energy,spectrum.l,R,V,
                                  {giving} chi,dummy);
      NormalizeTheWaveFunction({changing} chi);
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
      amp         : ARRAY[1..7] OF Real;
      total,dummy : Real;
      thisScreen  : TInputScreen;
      numLines    : Integer;
      temp        : DVector;
   BEGIN
      temp.init(numData+1);
      Message('');
      n := spectrum.numLevels;
      IF n>7 THEN n := 7;
    { Note: 7 eigenstates is all the screen has room for }
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
                  LoadLine('');
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
                        SolveWaveEquationInTwoParts({using} energy,spectrum.l,R,V,
                                                    {giving} temp,dummy);
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

PROCEDURE ChooseSomeOtherFunction(VAR R,psi2:Dvector;
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
               success := myParser.Parse('r','y','z','t',Fstring);
               IF success THEN
                  BEGIN
                     FOR i:=1 TO numData+1 DO
                        psi2.Put(i,myParser.F(R.value(i),0,0,0));
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

PROCEDURE DrawTheBraFunction(VAR R,V,bra:Dvector);
   BEGIN
      PrepareToDrawBra(R,V);
      SemiAutoScale(2,bra);
      SetColor(braColor);
      plotDvectors(R,bra,1,numData+1);
   END;

PROCEDURE DrawTheKetFunction(VAR R,V,ket:Dvector);
   BEGIN
      PrepareToDrawKet(R,V);
      SemiAutoScale(3,ket);
      SetColor(ketColor);
      plotDvectors(R,ket,1,numData+1);
   END;

PROCEDURE FormAndDisplayProduct(VAR R,bra,ket,braket:Dvector);
   VAR
      i: integer;
   BEGIN
      FOR i:=1 TO numdata+1 DO
         braket.Put(i,bra.value(i)*ket.value(i));
      SetColor(dullColor);
      PrepareToDrawBraket(R,V);
      SemiAutoScale(4,braket);
      SetColor(prodColor);
      PlotDvectors(R,braket,1,numData+1);
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

PROCEDURE IntegrateAndDisplay(VAR R,braket:Dvector; VAR area:real);
   VAR
      i: integer;
      areaOld: real;
   BEGIN
      PrepareToWriteResult;
      area :=  0;
      FOR i:=2 TO numdata+1 DO
         BEGIN
            areaOld := area;
            area := area + (braket.value(i)+braket.value(i-1))*deltaR/2;
            SelectViewport(7);
            SetColor(intgColor);
            DrawBar(R.value(i),(braket.value(i)+braket.value(i-1))/2);
            WriteArea(area,areaOld);
         END;
      areaOld := area;
      area :=  area + EndCorrection(braket.value(numdata+1),braket.value(numdata));
      WriteArea(area,areaOld);
   END;

FUNCTION AfterLastValue(VAR chi:DVector):Real;
  {It is assumed that chi behaves like exp(-kx) at Xmax}
   BEGIN
      IF chi.Value(numData)=0
         THEN AfterLastValue := 0
         ELSE AfterLastValue := chi.Value(numData+1)
                               *chi.Value(numData+1)/chi.Value(numData);
   END;

PROCEDURE CalculateFirstDeriv(VAR psi2,temp:DVector);
   VAR
      i : Integer;
      y : Real;
   BEGIN
      WITH psi2 DO
         BEGIN
            temp.Put(1,0);
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
            temp.Put(1,0);
            FOR i:=2 TO numdata DO
               temp.Put(i,(value(i+1)-2*value(i)+value(i-1)));
           {For final value assume psi2 goes as exp(-kx)}
            IF value(numData+1)<>0
               THEN y := abs(value(numData+1)/value(numData))
               ELSE y := 0;
            temp.Put(numData+1,temp.value(numData)*y);
         END;   {with}
   END;

PROCEDURE OperateToFormKetFunction(opNum:integer; VAR R,V,psi2,ket:Dvector);
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
       2 : {r}
            FOR i:=1 TO numdata+1 DO
                 ket.Put(i,(R.value(i)*psi2.value(i)));
       3 : {d/dx}
            BEGIN
               CalculateFirstDeriv(psi2,temp);
               FOR i:=1 TO numData+1 DO
                  ket.Put(i,temp.value(i)/(2*deltaR));
            END;
       4 : {r^2}
            FOR i:=1 TO numdata+1 DO
                 ket.Put(i,(R.Value(i)*R.value(i)*psi2.value(i)));
       5 : {d^2/dx^2}
            BEGIN
               CalculateSecondDeriv(psi2,temp);
               FOR i:=1 TO numData+1 DO
                    ket.Put(i,temp.value(i)/deltaR/deltaR);
            END;
       6 : {V(r)}
           BEGIN
              FOR i:=1 TO numdata+1 DO
                 ket.Put(i,(V.value(i)*psi2.value(i)));
           END;
       7 : {total energy}
           BEGIN
              CalculateSecondDeriv(psi2,temp);
              FOR i:=1 TO numData+1 DO
                 ket.Put(i,V.value(i)*psi2.value(i)
                          -temp.value(i)/deltaR/deltaR/energyConvert);
           END;
       8 : {r *  d/dx}
           BEGIN
              OperateToFormKetFunction(3, R,V,psi2,temp);
              OperateToFormKetFunction(2, R,V,temp,ket);
           END;
       9 : {d/dx * r}
           BEGIN
              OperateToFormKetFunction(2, R,V,psi2,temp);
              OperateToFormKetFunction(3, R,V,temp,ket);
           END;
      10  : {user defined}
           BEGIN
              ConstructKetUser1(R,V,psi2,ket);
           END;
      11  : {user defined}
           BEGIN
              ConstructKetUser2(R,V,psi2,ket);
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
                      ChooseAnEigenFunction(rNum,spectrum,R,V,bra,
                                            braParams,psi1Name,escape);
                   IF rNum=spectrum.numLevels+2 THEN
                      BEGIN
                         ChooseACompoundState(spectrum,R,V,bra,
                                              braParams,escape);
                         psi1Name := 'psi1';
                      END;
                   IF NOT escape
                      THEN
                         BEGIN
                            DrawTheBraFunction({using} R,V,bra);
                            braDrawn := true;
                            IF ketDrawn THEN
                               FormAndDisplayProduct({using} R,bra,ket,
                                                     {giving} braket);
                         END;
                END;
            4 : {specify operator}
                BEGIN
                   operatorNum := rNum;
                   IF ketDrawn THEN
                      BEGIN
                         OperateToFormKetFunction({using} operatorNum,
                                         R,V,psi2,{giving} ket);
                         DrawTheKetFunction({using} R,V,ket);
                         IF braDrawn THEN
                            FormAndDisplayProduct({using} R,bra,ket,
                                                  {giving} braket);
                      END
                   ELSE
                      PrepareToDrawKet(R,V);
                END;
            5 : {specify psi2}
                BEGIN
                   IF (rNum>0) AND (rNum<=spectrum.numLevels) THEN
                      ChooseAnEigenFunction(rNum,spectrum,R,V,psi2,
                                            ketParams,psi2Name,escape);
                   IF rNum=spectrum.numLevels+2 THEN
                      BEGIN
                         ChooseACompoundState(spectrum,R,V,psi2,
                                              ketParams,escape);
                         psi2Name := 'psi2';
                      END;
                   IF rNum=spectrum.numLevels+3 THEN
                      ChooseSomeOtherFunction(R,psi2,
                                              ketParams,psi2Name,escape);
                   IF NOT (escape)
                      THEN
                         BEGIN
                            OperateToFormKetFunction({using} operatorNum,
                                                  R,V,psi2,{giving} ket);
                            DrawTheKetFunction({using} R,V,ket);
                            ketDrawn := true;
                            IF braDrawn THEN
                               FormAndDisplayProduct({using} R,bra,ket,
                                                     {giving} braket);
                         END;
                END;
            6 : {perform the overlap integration}
                BEGIN
                   IF braDrawn AND ketDrawn THEN
                      IntegrateAndDisplay({using} R,braket,{giving} area);
                END;
         END;   {case cNum};

         END;    {with this overlap}
   END;

{ ------------------ FILE HANDLING PROCEDURES -------------------- }

{ ** These procedures save and read in global variables ** }

PROCEDURE SetUpOutputRecord(VAR thisFieldRecord:FieldRecord);
   BEGIN
      WITH thisFieldRecord DO
         BEGIN
            identifier   := fileIdentifier;
            fileUnits    := globalUnits;
            fileHighestR := highestR;
            fileLowestR  := lowestR;
            fileDeltaR   := deltaR;
            fileTicR     := ticR;
            fileWell     := thisWell;
            filePerturbn := thisPerturbn;
            fileSpectrum := spectrum;
            fileFound    := spectrumFound;
            fileBraParam := thisOverlap.braParams;
            fileKetParam := thisOverlap.ketParams;
         END;
   END;

PROCEDURE InterpretInputRecord(thisFieldRecord:FieldRecord);
   BEGIN
      WITH thisFieldRecord DO
         BEGIN
            globalUnits  := fileUnits;
            highestR     := fileHighestR;
            lowestR      := fileLowestR;
            deltaR       := fileDeltaR;
            ticR         := fileTicR;
            SetNewScaleOnXAxis(highestR,ticR,V,spectrumFound);
            CalculateEnergyConvert;
            thisWell     := fileWell;
            thisPerturbn := filePerturbn;
            CalculateWellDvector(thisWell,V,spectrumFound);
            spectrum      := fileSpectrum;
            spectrumFound := fileFound;
            thisOverlap.braParams := fileBraParam;
            thisOverlap.ketParams := fileKetParam;
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

PROCEDURE AskForOldFileName(VAR fileName:String; 
                            VAR escape:Boolean);
   BEGIN
      AskForFileName(false,fileName,escape);
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
               thisFound := spectrumFound;
               CalculateWellDVector({using} thisWell,
                                    {giving} V,spectrumFound);
               SetPotentialPlottingScale({using} V);
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

{------------------------------------------------------------------------}


{ ********* ALL USER WRITTEN PROCEDURES ARE HERE ********* }
{ *                                                      * }
{ * You may change or rewrite any of the following seven * }
{ * procedures.  Just follow the templates already coded * }
{ *                                                      * }
{ ******************************************************** }

PROCEDURE SetUserFlag;
   BEGIN
{ ** Set this flag true if you want your own code included **}
      userFlag := false;
   END;

PROCEDURE SetUserMenuItems;
 { This puts names of user defined items in the menus.    }
 { Currently they have descriptions of the "user defined" }
 { procedures written below.  When you write your own,    }
 { you may, if you like, change these names accordingly.  }
   BEGIN
    { DEFAULT : DO NOT HAVE USER MENU ITEMS }
      userItem1 := 'Exponential';      {goes in POTENTIAL menu }
      userItem2 := 'User defined';     {goes in PERTURBATION   }
      userItem3 := 'user1';            {goes in OPERATOR menu  }
      userItem4 := 'user2';            {and so does this       }
(*
    { ALTERNATIVE : THESE USER MENU ITEMS RELATE TO AUTHOR'S SUGGESTIONS }
      userItem1 := 'Exponential';      {goes in POTENTIAL menu }
      userItem2 := 'A.l';              {goes in PERTURBATION   }
      userItem3 := 'r';                {goes in OPERATOR menu  }
      userItem4 := 'd/dr';             {and so does this       }
*)
   END;

{---------------- USER DEFINED POTENTIAL WELL -------------------}

PROCEDURE SetDefaultUser(VAR thisWell:wellParams;
                           VAR highR,stepR:Real);
 { This sets the depth and width of your well when it is first chosen }
   BEGIN
      WITH thisWell DO
         BEGIN
            posn1  := 1.0;     {range of well}
            depth1 := 200;
            maxDepth := 600;
            highR    := 3.20;
            stepR    := 0.50;
            ticV     := 50;
         END;
      globalUnits := nuclearUnits;
   END;


PROCEDURE SetScreenLinesUser(VAR thisWell:wellParams;
                             VAR thisScreen:TInputScreen);
 { This writes information to the input screen.  If your well }
 { only needs two parameters, you won't have to change this.  }
   BEGIN
      WITH thisScreen DO
         BEGIN
            LoadLine(concat('     Read in your parameter(s) here = ',
                            '{      }'));
            LoadLine(concat('                                      ',
                            '{      }'));
            SetNumber(1,thisWell.depth1);
            SetNumberLimits(1,0,thisWell.maxDepth);
            SetNumber(2,thisWell.posn1);
            SetNumberLimits(2,0,2*thisWell.posn1);
         END;
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
 { Currently it is a simple exponential well.  }
   VAR
      i: integer;
   BEGIN
      WITH thisWell DO
         BEGIN
            V.Put(1,-depth1);
            FOR i:=2 TO numData+1 DO
               V.Put(i,-depth1*exp(-R.value(i)/posn1));
         END;
   END;

{------------ USER DEFINED PERTURBATION ---------------}

FUNCTION PerturbationUser(a,r:Real):Real;
 { This calculates the change deltaV as a function of r. }
 { It must be defined in term of one parameter only: a.  }
 { Currently it is a perturbation proportional to l.     }
   BEGIN
      PerturbationUser := a * spectrum.l;
   END;

{------------ USER DEFINED OPERATORS AND/OR KETS ---------------}

PROCEDURE ConstructKetUser1(VAR R,V,bra,ket:DVector);
 { This operates on the "bra" function to form the "ket".  }
 { Currently the operator is r.                            }
   VAR
      i           : integer;
      temp1,temp2 : DVector;
   BEGIN
      temp1.init(numData+1);
      temp2.init(numData+1);
      OperateToFormKetFunction(2,R,V,bra,ket);
    { This is the second menu choice, it multiplies by r }
      temp1.free;
      temp2.free;
   END;

PROCEDURE ConstructKetUser2(VAR R,V,bra,ket:DVector);
 { This operates on the "bra" function to form the "ket".  }
 { Currently the operator is d^/dr^.                     }
   VAR
      i           : integer;
      temp1,temp2 : DVector;
   BEGIN
      temp1.init(numData+1);
      temp2.init(numData+1);
      OperateToFormKetFunction(3,R,V,bra,ket);
    { This is the third choice, it takes the first derivative }
      temp1.free;
      temp2.free;
   END;

{ ************ END OF USER WRITTEN PROCEDURES ************ }


PROCEDURE InitializeUserProcedures;
   BEGIN
     SetUserFlag;
     SetUserMenuItems;
  END;

{*--------------------- MAIN PROGRAM MANAGEMENT --------------------*}

{*----------------------- PART 1 --------------------------*}

PROCEDURE SetUpPart1Menu(VAR thisMenu:TMenu);
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

            column(2,'Parts');
            row(2,1,'About Part 1');
            row(2,2,'Part 1: Finding Eigenvalues');
            row(2,3,'Part 2: Wavefunction Properties');

            column(3,'Ang Mom');
            row(3,1,'Set Orbital Ang Mom,  l');
            row(3,2,'Set Magnetic Ang Mom, m');
            row(3,3,'Display Spherical Harmonic');

            column(4,'Potential');
            row(4,1,'Square Well');
            row(4,2,'Parabolic');
            row(4,3,'Coulombic');
            row(4,4,'Lennard-Jones');
            row(4,5,'Dumbell');
            row(4,6,'Yukawa');
            row(4,7,userItem1);
            IF userFlag THEN i := 7
                        ELSE i := 6;
            row(4,i+1,'--------------------');
            RowActivate(4,i+1,false);  {blank line to separate parts of menu}
            row(4,i+2,'Vary Well Parameters');
            row(4,i+3,'Add a Perturbation');

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
            row(6,5,'Probability Cloud');
            row(6,6,'------------------');
            row(6,7,'Sound');
            RowActivate(6,6,false);

            column(7,'Units');
            row(7,1,'Units');
            row(7,2,'Mass');
            row(7,3,'r Axis');
      END;
   END;

PROCEDURE DisposeOfpart1Menu(VAR thisMenu:TMenu);
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
            RowActivate(5,4,examine);
            RowActivate(6,2,spectrumFound);
            RowActivate(6,3,spectrumFound);
            RowActivate(6,4,examine);
            RowActivate(6,5,spectrumFound);

            FOR i:=1 TO 7 DO
               RowChecked(4,i,false);
            RowChecked(4,thisWell.wellType,true);
            RowChecked(2,2,true);
            RowChecked(6,7,soundOn);
            REPEAT
               found := chosen
            UNTIL found;
            colNum := colChosen;
            rowNum := rowChosen;
         END;
   END;

PROCEDURE FindEigenvaluesAndEigenfunctions(VAR partCode:Integer);
{* This is the main part of the program for part 1 *}
   VAR
      colNum,rowNum,newRow  : Integer;
      escape,first          : Boolean;
      eraseFirst            : Boolean;
      examine,spectrumFound : Boolean;
      exitCode              : Integer;
      part1Menu             : TMenu;
   BEGIN
      SetUpPart1Menu(part1Menu);
      eraseFirst := true;
      first := true;
      examine := false;
      REPEAT
         IF eraseFirst THEN
            DrawThePotentialWellAsBackground({using} R,V,examine)
         ELSE
            eraseFirst := true;
         ChooseFromPart1menu(part1Menu,examine,spectrumFound,
                            partCode,colNum,rowNum);
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
                          DisplayHelpScreen(part1cHS);
                       END;
                   2..4 : BEGIN  {go to another part of program}
                             partCode := rowNum-1;
                             ClearMUPPETport;
                             escape := true;
                          END;
                 END;    {case rowNum}
              END;
          3 : { Deal with the angular parts of the problem }
              BEGIN
                 eraseFirst := false;
                 IF rowNum=1 THEN
                    ChooseOrbitalAngMomm({setting} spectrum.l,spectrum.m,
                                                   spectrumFound);
                 IF rownum=2 THEN
                    ChooseMagneticAngMomm({using} spectrum.l,
                                             {giving} spectrum.m);
                 IF rownum=3 THEN
                    Display3DSphericalHarmonic({using} spectrum.l,
                                                     spectrum.m);
                 DrawThePotentialWellAsBackground({using} R,V,examine)
              END;
          4..6 :
              BEGIN
            { Note: if the userFlag is set there is an extra menu item!}
                 IF (colNum=4) AND (rowNum>7) AND userFlag THEN
                    newRow := rowNum-1
                 ELSE
                    newRow := rowNum;
            { Now go and do all that the user wants }
                 ExploreThisPotential(colNum,newRow,first,
                                      examine,spectrumFound,eraseFirst);
              END;
          7 : { Change the scale and measuring units }
              CASE rowNum OF
               1 : ChangeGlobalUnits(spectrumFound);
               2 : ChangeMass(spectrumFound);
               3 : ChangeScaleOnXAxis(V,spectrumFound);
              END;
         END;    {case colNum}
      UNTIL escape;
      DisposeOfPart1Menu(part1Menu);
   END;

{*----------------------- PART 2 --------------------------*}

PROCEDURE SetVariousDefaults(VAR fileName:String;
                             VAR ovrlap:Overlap);
   BEGIN
      ovrlap.operatorNum := 1;
      fileName := '';

      operators[1] := '1';
      operators[2] := 'r';
      operators[3] := 'd/dr';
      operators[4] := 'r^2';
      operators[5] := 'd^2/dr^2';
      operators[6] := 'V';
      operators[7] := 'E';
      operators[8] := 'r.d/dr';
      operators[9] := 'd/dr.r';
      operators[10] := userItem3;
      operators[11] := userItem4;
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
            row(2,1,'About Part 2');
            row(2,2,'Part 1: Finding Eigenvalues');
            row(2,3,'Part 2: Wavefunction Properties');

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
            row(5,i+1,'-------------------');
            RowActivate(5,i+1,false);
            row(5,i+2,'General State');
            row(5,i+3,'Some Other Function');

            column(6,'Integrate');
      END;
   END;

PROCEDURE DisposeOfPart2Menu(VAR thisMenu:TMenu);
   BEGIN
      thisMenu.Done;
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
      PrepareToStudyWaveFunctions({using} R,V);
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
      DisposeOfPart2Menu(part2Menu);
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
   SetVariousUnits;
   SetGlobalValues;
   SetPlottingDetails;
   SetADefaultWell(thisWell,thisPerturbn,V,spectrum,spectrumFound);
   SetVariousDefaults(currentFileName,thisOverlap);
   IntroduceProgram(partCode);

   REPEAT
      CASE partCode OF
       1 : {Part 1}
           FindEigenvaluesAndEigenfunctions(partCode);
       2 : {Part 2}
           IF spectrumHasBeenCalculated(spectrum) THEN
              DemonstratePropertiesOfWaveFunctions1(partCode)
           ELSE
              partCode := 1;
      END;
   UNTIL (partCode=4);

   CUPSdone;
END.
