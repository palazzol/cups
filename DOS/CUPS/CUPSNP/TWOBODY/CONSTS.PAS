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

UNIT Consts;
{$N+}
                    {*********************************}
                    {*    Two-Nucleon Simulation     *}
                    {*          CONSTS.PAS           *}
                    {*   Written by John Philpott    *}
                    {*        July XX, 1993.         *}
                    {*   Copyright (c) 1992, 1993    *}
                    {*********************************}
 
 
                                 INTERFACE
 
 
USES
      crt, 
      graph, CUPS, CUPSmupp, CUPSfunc, CUPSproc, CUPSgui, CUPSgrph;
 
{ ---------------- Various Constants and Type Declarations ---------------- }

TYPE
   Float     = Extended;

CONST
   Large    = 1E10;
   hbarc    = 197.327; { MeV fm }
   esq      = 1.44;    { MeV fm }
   neutron  = 939.566; { MeV }
   proton   = 938.272; { MeV }
   RmaxInit = 10;      { fm }
   Rstep    = 0.10;    { fm }
   LmaxP    = 7;       { < 12  }

VAR
   Zero : Complex;

PROCEDURE SetComplexConsts;

TYPE
   CXVector = RECORD
         RealVec : DVector;
         ImagVec : DVector;
      END;

CONST
   LCoul     = 42;   { Largest L value for Coulomb functions   }
   LCoulPlus = LCoul + 6;

CONST
   PhasesLbig = 13;  { Maximum Lprime+1 in the PhaseType Array }
   PhasesKmax =  5;  { Number of phases for each Lprime        }
TYPE
   PhaseType = ARRAY[1..PhasesLbig,1..PhasesKmax] of Float;
   CXVec     = ARRAY[1..PhasesLbig,1..6] of COMPLEX;

TYPE
   DVarray   = ARRAY[1..4] OF DVector;
   CXarray   = ARRAY[1..6] of Complex;
 
TYPE
   alphatype = ARRAY[0..2] OF Float; { Used in d-wave tail }
   capEtype  = ARRAY[1..4] OF Float; { region integrals    }
 
{ ------------------- Some Sets of Potential Parameters ------------------- }

TYPE
   PotlForm = (Square, Gauss, Exponential, Yukawa, HJ, UserDefined);
 
   TripletConsts =
      RECORD
         CNdepth, CNrange : Float;
         LSdepth, LSrange : Float;
         TNdepth, TNrange : Float;
      END;
   SingletConsts =
      RECORD
         CNdepth, CNrange : Float;
      END;
   PotlConsts =
      RECORD
         Title      : String;
         OPEPswitch : Boolean;
         Form   : PotlForm;
         Core   : Float;
         TE_Consts, TO_Consts : TripletConsts;
         SE_Consts, SO_Consts : SingletConsts;
      END;
 
VAR
   PotlParams : ARRAY[1..10] OF PotlConsts;

PROCEDURE SetPotlConsts;

            { Potentials taken from HAMADA and JOHNSTON,
                 Nuclear Physics 34 (1962), p382 }
 
CONST
   pionCompLen = 1.415;    { fm }
   RADMin      = 0.485345; { fm }
 
TYPE
   PotlConstsHJ =
      RECORD
         Vcen, Ac, Bc,   Vten, At, Bt   : Float;
         Vlsz, Bls,      Vllz, All, Bll : Float;
      END;
 
VAR
   TE_Consts, TO_Consts : PotlConstsHJ;
   SE_Consts, SO_Consts : PotlConstsHJ;

PROCEDURE SetPotlConstsHJ;

{ OPEP Potential constants and variables }

CONST
   OPEPstrength = 3.7173;
   OPEPrange    = 1.415;

VAR
   OPEP           : Boolean; { OPEP flag }
   VcOPEP, VtOPEP : Float;   { OPEP Central and tensor strengths }

TYPE
   Channel = (SEvn, SOdd, TEvn, TOdd);

PROCEDURE SetOPEPStrengths(ThisChannel: Channel; VAR VcOPEP, VtOPEP: Float);


{ --------------------- Set Up Tabulated Phase Shifts --------------------- }

CONST
   MinimumTabulatedLabKE = 24;  { MeV }
   MaximumTabulatedLabKE = 400; { MeV }

CONST
   PhasesJmax =  27;  { Number of tabulated energies }
   PhasesLmax =   6;  { Maximum tabulated Lprime+1 }
TYPE
   PhaseRec =
      RECORD
         Energy: Integer;
         Phase : ARRAY[1..PhasesLmax,1..PhasesKmax] OF Integer;
      END;
 
VAR
   PhaseArray : ARRAY[1..PhasesJmax] of PhaseRec;

PROCEDURE SetupTabulatedPhases;


{ --------------------------- Global Variables ---------------------------- }

VAR
   quitprog : Boolean;

VAR
   Rmatch   : Float;   { fm }
   Rmax     : Float;   { fm }
   MySystem : Integer;
   LabKE    : Float;   { MeV }

   Npoints  : Integer; { Size of array for the }
                         { radial wavefunction   }

   PotlIndex   : Integer;
   UserIndex   : Integer;
   ThisPotl    : PotlConsts;
   TripletPotl : TripletConsts;
   SingletPotl : SingletConsts;
 
   PotlHJ      : PotlConstsHJ;

   muftr    : Float;
   Energy   : Float;
   CapJ     : Integer;

TYPE
   CoulVtr   = ARRAY[0..LCoulPlus] of FLOAT;
VAR
   CoulSig       : CoulVtr;   { Absolute Coulomb phases  }
   F, G, Fp, Gp  : CoulVtr;   { Coulomb radial functions }
   TheoryDone    : Boolean;   { Test whether calculated phases exist }
   TheoryPhases  : PhaseType; { Calculated phase shifts  }

   Thetmin  : Float;
   Thetmax  : Float;
   Numang   : Integer;

   thetav     : DVector;
   av, bv     : CXVector;
   cv, ev, fv : CXVector;
   cxamp      : CXVec;


                               IMPLEMENTATION

{ --------------------- Set Up Interaction Constants ---------------------- }

PROCEDURE SetComplexConsts;
BEGIN
   WITH Zero DO
      BEGIN
         Re := 0.0;
         Im := 0.0;
      END;
END;
 
PROCEDURE SetPotlConsts;

   PROCEDURE SetSinglet(arg1, arg2, arg3, arg4: Float);
   BEGIN
      WITH ThisPotl DO
         BEGIN
            SE_Consts.CNdepth := arg1;
            SE_Consts.CNrange := arg2;
            SO_Consts.CNdepth := arg3;
            SO_Consts.CNrange := arg4;
         END;
   END;

   PROCEDURE SetTriplet(arg1, arg2, arg3, arg4: Float);
   BEGIN
      WITH ThisPotl DO
         BEGIN
            TE_Consts.CNdepth := arg1;
            TE_Consts.CNrange := arg2;
            TO_Consts.CNdepth := arg3;
            TO_Consts.CNrange := arg4;
         END;
   END;

   PROCEDURE SetSpinOrb(arg1, arg2, arg3, arg4: Float);
   BEGIN
      WITH ThisPotl DO
         BEGIN
            TE_Consts.LSdepth := arg1;
            TE_Consts.LSrange := arg2;
            TO_Consts.LSdepth := arg3;
            TO_Consts.LSrange := arg4;
         END;
   END;

   PROCEDURE SetTensor_(arg1, arg2, arg3, arg4: Float);
   BEGIN
      WITH ThisPotl DO
         BEGIN
            TE_Consts.TNdepth := arg1;
            TE_Consts.TNrange := arg2;
            TO_Consts.TNdepth := arg3;
            TO_Consts.TNrange := arg4;
         END;
   END;

BEGIN
   WITH ThisPotl DO
      BEGIN { WITH ThisPotl }

{ --- 1 --- }
   Title      := 'Central interaction - No exchange';
   OPEPswitch := FALSE;
   Form       := Yukawa;
   Core       := 0.5;
{                   EVEN Parity             ODD  Parity      }
{                Depth       Range       Depth       Range   }
   SetSinglet( 1.070e6   ,   0.126   , 1.070e6   ,   0.126   );
   SetTriplet( 1.070e6   ,   0.126   , 1.070e6   ,   0.126   );
   SetSpinOrb(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   SetTensor_(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   PotlParams[1] := ThisPotl;

{ --- 2 --- }
   Title      := 'Central interaction with spin exchange';
   OPEPswitch := FALSE;
   Form       := Yukawa;
   Core       := 0.5;
{                   EVEN Parity             ODD  Parity      }
{                Depth       Range       Depth       Range   }
   SetSinglet( 1.030e6   ,   0.117   , 1.030e6   ,   0.117   );
   SetTriplet( 1.081e6   ,   0.126   , 1.081e6   ,   0.126   );
   SetSpinOrb(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   SetTensor_(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   PotlParams[2] := ThisPotl;

{ --- 3 --- }
   Title      := 'Central interaction with space exchange';
   OPEPswitch := FALSE;
   Form       := Yukawa;
   Core       := 0.5;
{                   EVEN Parity             ODD  Parity      }
{                Depth       Range       Depth       Range   }
   SetSinglet( 14235.0   ,   0.303   , 12916.0   ,   0.206   );
   SetTriplet( 14235.0   ,   0.303   , 12916.0   ,   0.206   );
   SetSpinOrb(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   SetTensor_(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   PotlParams[3] := ThisPotl;

{ --- 4 --- }
   Title      := 'Central with spin and space exchange';
   OPEPswitch := FALSE;
   Form       := Yukawa;
   Core       := 0.5;
{                   EVEN Parity             ODD  Parity      }
{                Depth       Range       Depth       Range   }
   SetSinglet( 1.319e6   ,   0.125   ,-2.341e6   ,   0.196   );
   SetTriplet( 3.774e5   ,   0.150   ,    0.00   ,   0.000   );
   SetSpinOrb(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   SetTensor_(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   PotlParams[4] := ThisPotl;

{ --- 5 --- }
   Title      := 'Central and spin-orbit interaction';
   OPEPswitch := FALSE;
   Form       := Yukawa;
   Core       := 0.5;
{                   EVEN Parity             ODD  Parity      }
{                Depth       Range       Depth       Range   }
   SetSinglet( 1.319e6   ,   0.123   ,-2.340e6   ,   0.190   );
   SetTriplet( 3.761e5   ,   0.151   ,   30.50   ,   1.002   );
   SetSpinOrb( 1284.20   ,   0.346   ,  225.70   ,   0.563   );
   SetTensor_(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   PotlParams[5] := ThisPotl;

{ --- 6 --- }
   Title      := 'Central and tensor interaction';
   OPEPswitch := FALSE;      { Gammel, Christian and Thaler, }
   Form       := Yukawa;     {   Phys. Rev. 105, 311 (1957)  }
   Core       := 0.5;
{                   EVEN Parity             ODD  Parity      }
{                Depth       Range       Depth       Range   }
   SetSinglet(  905.00   ,   0.588   , -113.00   ,   1.000   );
   SetTriplet( 6395.00   ,   0.3405  ,  150.00   ,   0.6667  );
   SetSpinOrb(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   SetTensor_(   45.00   ,   1.362   ,  -57.50   ,   0.893   );
   PotlParams[6] := ThisPotl;

{ --- 7 --- }
   Title      := 'Test interaction 1 - No OPEP';
   OPEPswitch := FALSE;
   Form       := Yukawa;
   Core       := 0.5;
{                   EVEN Parity             ODD  Parity      }
{                Depth       Range       Depth       Range   }
   SetSinglet( 5023.62   ,   0.362   ,-1.750e6   ,   0.150   );
   SetTriplet( 5157.69   ,   0.353   , 1779.85   ,   0.283   );
   SetSpinOrb(  204.17   ,   0.425   ,12995.58   ,   0.244   );
   SetTensor_(  100.41   ,   0.999   ,  -50.58   ,   0.908   );
   PotlParams[7] := ThisPotl;

{ --- 8 --- }
   Title      := 'Test interaction 2 - WITH OPEP';
   OPEPswitch := TRUE;
   Form       := Yukawa;
   Core       := 0.5;
{                   EVEN Parity             ODD  Parity      }
{                Depth       Range       Depth       Range   }
   SetSinglet( 8162.89   ,   0.312   ,-8.370e5   ,   0.143   );
   SetTriplet( 1759.66   ,   0.327   ,   59.70   ,   0.710   );
   SetSpinOrb(  227.15   ,   0.536   ,19159.65   ,   0.229   );
   SetTensor_(   -4.25   ,   1.630   ,  147.55   ,   0.505   );
   PotlParams[8] := ThisPotl;

{ --- 9 --- }
   Title      := 'Hamada-Johnston interaction';
   OPEPswitch := FALSE;       { T. Hamada and I. D. Johnston }
   Form       := HJ;          { Nucl. Phys. 34, (1962) p382  }
   Core       := 0.0;
{                   EVEN Parity             ODD  Parity      }
{                Depth       Range       Depth       Range   }
   SetSinglet(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   SetTriplet(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   SetSpinOrb(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   SetTensor_(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   PotlParams[9] := ThisPotl;

{ --- 10 --- }
   Title      := 'User-defined interaction from file TWOBODY.INT';
   OPEPswitch := FALSE;
   Form       := UserDefined;
   Core       := 0.0;
{                   EVEN Parity             ODD  Parity      }
{                Depth       Range       Depth       Range   }
   SetSinglet(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   SetTriplet(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   SetSpinOrb(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   SetTensor_(    0.00   ,   0.000   ,    0.00   ,   0.000   );
   PotlParams[10] := ThisPotl;

      END;  { WITH ThisPotl }
END;

PROCEDURE SetPotlConstsHJ;

   PROCEDURE SetVcenHJ(arg1, arg2, arg3: Float);
   BEGIN
      WITH PotlHJ DO
         BEGIN
            Vcen := arg1;
            Ac   := arg2;
            Bc   := arg3;
         END;
   END;

   PROCEDURE SetVtenHJ(arg1, arg2, arg3: Float);
   BEGIN
      WITH PotlHJ DO
         BEGIN
            Vten := arg1;
            At   := arg2;
            Bt   := arg3;
         END;
   END;

   PROCEDURE SetVlszHJ(arg1, arg2, arg3: Float);
   BEGIN
      WITH PotlHJ DO
         BEGIN
            Vlsz := arg1;
            Bls  := arg3;
         END;
   END;

   PROCEDURE SetVllzHJ(arg1, arg2, arg3: Float);
   BEGIN
      WITH PotlHJ DO
         BEGIN
            Vllz := arg1;
            All  := arg2;
            Bll  := arg3;
         END;
   END;

BEGIN

{ --- TE --- }
   SetVcenHJ(-11.1519   ,  6.0  , -1.0  );
   SetVtenHJ(-11.1519   , -0.5  ,  0.2  );
   SetVlszHJ( 10.3574   ,  0.0  , -0.1  );
   SetVllzHJ(  0.3722   ,  1.8  , -0.4  );
   TE_Consts := PotlHJ;

{ --- TO --- }
   SetVcenHJ(  3.7173   , -9.07 ,  3.48 );
   SetVtenHJ(  3.7173   , -1.29 ,  0.55 );
   SetVlszHJ( 27.33634  ,  0.0  , -7.12 );
   SetVllzHJ( -0.124205 , -7.26 ,  6.92 );
   TO_Consts := PotlHJ;

{ --- SE --- }
   SetVcenHJ(-11.1519   ,  8.7  , 10.6  );
   SetVtenHJ(  0.0      ,  0.0  ,  0.0  );
   SetVlszHJ(  0.0      ,  0.0  ,  0.0  );
   SetVllzHJ( -0.124205 ,  0.2  , -0.2  );
   SE_Consts := PotlHJ;

{ --- SO --- }
   SetVcenHJ( 33.4557   , -8.0  , 12.0  );
   SetVtenHJ(  0.0      ,  0.0  ,  0.0  );
   SetVlszHJ(  0.0      ,  0.0  ,  0.0  );
   SetVllzHJ( -0.3722   ,  2.0  ,  6.0  );
   SO_Consts := PotlHJ;

END;

PROCEDURE SetOPEPStrengths(ThisChannel: Channel; VAR VcOPEP, VtOPEP: Float);
BEGIN
   CASE ThisChannel OF
SEvn: BEGIN
         VcOPEP := -3*OPEPstrength;
      END;
SOdd: BEGIN
         VcOPEP := 9*OPEPstrength;
      END;
TEvn: BEGIN
         VcOPEP := -3*OPEPstrength;
         VtOPEP := VcOPEP;
      END;
TOdd: BEGIN
         VcOPEP := OPEPstrength;
         VtOPEP := VcOPEP;
      END;
   END; { CASE }
END;

{ --------------------- Set Up Tabulated Phase Shifts --------------------- }

{                Storage pattern for nucleon-nucleon phases                 }
{                                                                           }
{                |    1         2         3         4         5             }
{                |                                                          }
{          ------|------------------------------------------------          }
{                |                                                          }
{           1    |   3S1        -         -        1S0        -             }
{           2    |   3P2       3P1       3P0       1P1       Eps1           }
{           3    |   3D3       3D2       3D1       1D2       Eps2           }
{           4    |   3F4       3F3       3F2       1F3       Eps3           }
{           5    |   3G5       3G4       3G3       1G4       Eps4           }
{           6    |   3H6       3H5       3H4       1H5       Eps5           }
{                |                                                          }
{           ------------------------------------------------------          }

PROCEDURE SetupTabulatedPhases;
{ Note:   Phase shifts are tabulated as integers.  Each integer is          }
{            100 times the empirical phase shift (in degrees)               }
VAR
   j : Integer;

   PROCEDURE SetEnergy(arg: Integer);
   BEGIN
      PhaseArray[j].Energy := arg;
   END;

   PROCEDURE SetPhases(Lplus, arg1, arg2, arg3, arg4, arg5: Integer);
   BEGIN
      PhaseArray[j].Phase[Lplus,1] := arg1;
      PhaseArray[j].Phase[Lplus,2] := arg2;
      PhaseArray[j].Phase[Lplus,3] := arg3;
      PhaseArray[j].Phase[Lplus,4] := arg4;
      PhaseArray[j].Phase[Lplus,5] := arg5;
   END;

BEGIN

   j := 1;
   SetEnergy( 24);
   SetPhases(1,   7895,      0,      0,   4971,      0 );
   SetPhases(2,    223,   -423,    700,    -39,    535 );
   SetPhases(3,     27,    407,   -229,     70,    -83 );
   SetPhases(4,      3,    -22,     10,    -38,     50 );
   SetPhases(5,      0,     15,     -5,      4,     -4 );
   SetPhases(6,      0,     -1,      0,     -3,      3 );

   j := 2;
   SetEnergy( 32);
   SetPhases(1,   7369,      0,      0,   4597,      0 );
   SetPhases(2,    329,   -542,    947,   -112,    492 );
   SetPhases(3,     44,    602,   -344,    102,   -118 );
   SetPhases(4,      6,    -37,     18,    -60,     81 );
   SetPhases(5,     -1,     29,    -10,      7,     -8 );
   SetPhases(6,      0,     -3,      1,     -6,      7 );

   j := 3;
   SetEnergy( 40);
   SetPhases(1,   6874,      0,      0,   4247,      0 );
   SetPhases(2,    436,   -652,   1118,   -207,    425 );
   SetPhases(3,     63,    791,   -460,    135,   -149 );
   SetPhases(4,     10,    -52,     27,    -82,    114 );
   SetPhases(5,     -1,     45,    -16,     11,    -13 );
   SetPhases(6,      1,     -5,      2,    -10,     12 );

   j := 4;
   SetEnergy( 48);
   SetPhases(1,   6415,      0,      0,   3931,      0 );
   SetPhases(2,    542,   -758,   1220,   -317,    353 );
   SetPhases(3,     82,    969,   -574,    167,   -176 );
   SetPhases(4,     15,    -68,     36,   -102,    147 );
   SetPhases(5,     -1,     63,    -23,     15,    -18 );
   SetPhases(6,      1,     -8,      3,    -15,     19 );

   j := 5;
   SetEnergy( 56);
   SetPhases(1,   5991,      0,      0,   3647,      0 );
   SetPhases(2,    643,   -858,   1267,   -439,    286 );
   SetPhases(3,    102,   1134,   -683,    199,   -200 );
   SetPhases(4,     20,    -85,     45,   -122,    180 );
   SetPhases(5,     -1,     83,    -31,     20,    -24 );
   SetPhases(6,      1,    -11,      4,    -20,     26 );

   j := 6;
   SetEnergy( 64);
   SetPhases(1,   5599,      0,      0,   3391,      0 );
   SetPhases(2,    739,   -955,   1273,   -568,    229 );
   SetPhases(3,    121,   1285,   -787,    230,   -220 );
   SetPhases(4,     27,   -100,     54,   -140,    211 );
   SetPhases(5,     -1,    103,    -39,     25,    -29 );
   SetPhases(6,      2,    -15,      5,    -25,     33 );

   j := 7;
   SetEnergy( 80);
   SetPhases(1,   4900,      0,      0,   2940,      0 );
   SetPhases(2,    913,  -1138,   1200,   -838,    147 );
   SetPhases(3,    158,   1542,   -979,    290,   -250 );
   SetPhases(4,     41,   -130,     70,   -171,    270 );
   SetPhases(5,      0,    145,    -57,     35,    -41 );
   SetPhases(6,      4,    -23,      9,    -37,     50 );

   j := 8;
   SetEnergy( 96);
   SetPhases(1,   4295,      0,      0,   2550,      0 );
   SetPhases(2,   1063,  -1308,   1064,  -1113,    111 );
   SetPhases(3,    191,   1748,  -1151,    347,   -271 );
   SetPhases(4,     58,   -157,     85,   -197,    325 );
   SetPhases(5,      2,    186,    -76,     45,    -51 );
   SetPhases(6,      6,    -32,     13,    -48,     67 );

   j := 9;
   SetEnergy(112);
   SetPhases(1,   3768,      0,      0,   2204,      0 );
   SetPhases(2,   1190,  -1466,    901,  -1383,    116 );
   SetPhases(3,    219,   1908,  -1305,    402,   -284 );
   SetPhases(4,     75,   -181,     97,   -219,    376 );
   SetPhases(5,      4,    227,    -96,     55,    -61 );
   SetPhases(6,      9,    -42,     17,    -59,     85 );

   j := 10;
   SetEnergy(128);
   SetPhases(1,   3304,      0,      0,   1888,      0 );
   SetPhases(2,   1296,  -1614,    732,  -1645,    153 );
   SetPhases(3,    244,   2031,  -1443,    454,   -291 );
   SetPhases(4,     94,   -203,    107,   -237,    422 );
   SetPhases(5,      8,    267,   -115,     64,    -70 );
   SetPhases(6,     11,    -52,     23,    -69,    103 );

   j := 11;
   SetEnergy(144);
   SetPhases(1,   2892,      0,      0,   1594,      0 );
   SetPhases(2,   1383,  -1751,    565,  -1845,    218 );
   SetPhases(3,    264,   2122,  -1568,    504,   -295 );
   SetPhases(4,    113,   -221,    114,   -252,    465 );
   SetPhases(5,     11,    305,   -135,     73,    -77 );
   SetPhases(6,     15,    -63,     28,    -78,    120 );

   j := 12;
   SetEnergy(160);
   SetPhases(1,   2524,      0,      0,   1320,      0 );
   SetPhases(2,   1454,  -1879,    404,  -2133,    306 );
   SetPhases(3,    281,   2188,  -1680,    553,   -296 );
   SetPhases(4,    133,   -238,    119,   -265,    505 );
   SetPhases(5,     15,    342,   -154,     82,    -83 );
   SetPhases(6,     18,    -74,     34,    -87,    137 );

   j := 13;
   SetEnergy(176);
   SetPhases(1,   2194,      0,      0,   1060,      0 );
   SetPhases(2,   1510,  -1998,    248,  -2358,    411 );
   SetPhases(3,    295,   2233,  -1783,    599,   -295 );
   SetPhases(4,    153,   -252,    122,   -276,    543 );
   SetPhases(5,     20,    378,   -173,     90,    -88 );
   SetPhases(6,     22,    -85,     40,    -95,    154 );

   j := 14;
   SetEnergy(192);
   SetPhases(1,   1896,      0,      0,    813,      0 );
   SetPhases(2,   1555,  -2110,     98,  -2570,    531 );
   SetPhases(3,    306,   2262,  -1877,    643,   -293 );
   SetPhases(4,    173,   -264,    124,   -285,    578 );
   SetPhases(5,     24,    412,   -192,     97,    -92 );
   SetPhases(6,     26,    -96,     46,   -103,    170 );

   j := 15;
   SetEnergy(208);
   SetPhases(1,   1625,      0,      0,    577,      0 );
   SetPhases(2,   1588,  -2216,    -50,  -2769,    662 );
   SetPhases(3,    314,   2276,  -1963,    686,   -291 );
   SetPhases(4,    193,   -274,    124,   -292,    611 );
   SetPhases(5,     29,    445,   -210,    104,    -95 );
   SetPhases(6,     30,   -107,     52,   -109,    186 );

   j := 16;
   SetEnergy(224);
   SetPhases(1,   1379,      0,      0,    351,      0 );
   SetPhases(2,   1612,  -2315,   -197,  -2957,    802 );
   SetPhases(3,    320,   2279,  -2043,    727,   -289 );
   SetPhases(4,    212,   -283,    122,   -299,    641 );
   SetPhases(5,     33,    477,   -228,    110,    -96 );
   SetPhases(6,     34,   -118,     59,   -116,    201 );

   j := 17;
   SetEnergy(240);
   SetPhases(1,   1154,      0,      0,    134,      0 );
   SetPhases(2,   1628,  -2408,   -346,  -3133,    950 );
   SetPhases(3,    324,   2273,  -2118,    767,   -286 );
   SetPhases(4,    232,   -291,    119,   -304,    671 );
   SetPhases(5,     38,    508,   -246,    116,    -97 );
   SetPhases(6,     38,   -129,     65,   -122,    216 );

   j := 18;
   SetEnergy(256);
   SetPhases(1,    947,      0,      0,    -75,      0 );
   SetPhases(2,   1638,  -2497,   -499,  -3299,   1103 );
   SetPhases(3,    327,   2259,  -2187,    806,   -284 );
   SetPhases(4,    251,   -298,    116,   -309,    699 );
   SetPhases(5,     42,    537,   -263,    122,    -96 );
   SetPhases(6,     42,   -140,     72,   -127,    231 );

   j := 19;
   SetEnergy(272);
   SetPhases(1,    757,      0,      0,   -276,      0 );
   SetPhases(2,   1641,  -2580,   -656,  -3455,   1260 );
   SetPhases(3,    327,   2240,  -2252,    843,   -283 );
   SetPhases(4,    270,   -303,    111,   -312,    725 );
   SetPhases(5,     46,    566,   -280,    127,    -95 );
   SetPhases(6,     47,   -151,     78,   -132,    245 );

   j := 20;
   SetEnergy(288);
   SetPhases(1,    581,      0,      0,   -470,      0 );
   SetPhases(2,   1639,  -2660,   -820,  -3602,   1421 );
   SetPhases(3,    327,   2215,  -2313,    878,   -282 );
   SetPhases(4,    288,   -308,    106,   -316,    750 );
   SetPhases(5,     51,    593,   -296,    131,    -93 );
   SetPhases(6,     51,   -162,     84,   -136,    258 );

   j := 21;
   SetEnergy(304);
   SetPhases(1,    419,      0,      0,   -658,      0 );
   SetPhases(2,   1633,  -2735,   -992,  -3739,   1584 );
   SetPhases(3,    325,   2187,  -2371,    913,   -282 );
   SetPhases(4,    307,   -311,    100,   -318,    774 );
   SetPhases(5,     55,    620,   -312,    135,    -91 );
   SetPhases(6,     55,   -172,     91,   -140,    271 );

   j := 22;
   SetEnergy(320);
   SetPhases(1,    268,      0,      0,   -839,      0 );
   SetPhases(2,   1623,  -2807,  -1173,  -3869,   1750 );
   SetPhases(3,    322,   2156,  -2426,    946,   -282 );
   SetPhases(4,    324,   -314,     94,   -320,    797 );
   SetPhases(5,     59,    646,   -328,    139,    -87 );
   SetPhases(6,     59,   -183,     97,   -144,    284 );

   j := 23;
   SetEnergy(336);
   SetPhases(1,    129,      0,      0,  -1014,      0 );
   SetPhases(2,   1610,  -2876,  -1362,  -3991,   1916 );
   SetPhases(3,    318,   2123,  -2478,    979,   -283 );
   SetPhases(4,    342,   -317,     87,   -322,    819 );
   SetPhases(5,     63,    671,   -343,    142,    -84 );
   SetPhases(6,     64,   -193,    103,   -148,    296 );

   j := 24;
   SetEnergy(352);
   SetPhases(1,     -2,      0,      0,  -1183,      0 );
   SetPhases(2,   1594,  -2942,  -1562,  -4105,   2083 );
   SetPhases(3,    313,   2088,  -2528,   1010,   -285 );
   SetPhases(4,    359,   -319,     80,   -323,    840 );
   SetPhases(5,     66,    695,   -359,    145,    -79 );
   SetPhases(6,     68,   -203,    110,   -151,    308 );

   j := 25;
   SetEnergy(368);
   SetPhases(1,   -123,      0,      0,  -1346,      0 );
   SetPhases(2,   1576,  -3005,  -1771,  -4213,   2250 );
   SetPhases(3,    307,   2051,  -2575,   1040,   -288 );
   SetPhases(4,    376,   -320,     72,   -324,    860 );
   SetPhases(5,     70,    719,   -373,    148,    -74 );
   SetPhases(6,     72,   -213,    116,   -154,    320 );

   j := 26;
   SetEnergy(384);
   SetPhases(1,   -236,      0,      0,  -1504,      0 );
   SetPhases(2,   1556,  -3065,  -1990,  -4315,   2417 );
   SetPhases(3,    301,   2014,  -2621,   1070,   -291 );
   SetPhases(4,    392,   -321,     65,   -325,    880 );
   SetPhases(5,     73,    741,   -388,    150,    -69 );
   SetPhases(6,     76,   -222,    122,   -156,    331 );

   j := 27;
   SetEnergy(400);
   SetPhases(1,   -342,      0,      0,  -1658,      0 );
   SetPhases(2,   1533,  -3123,  -2220,  -4411,   2583 );
   SetPhases(3,    294,   1976,  -2664,   1098,   -295 );
   SetPhases(4,    409,   -322,     57,   -325,    899 );
   SetPhases(5,     76,    764,   -402,    153,    -64 );
   SetPhases(6,     80,   -232,    128,   -159,    342 );

END;

END.  { Unit CONSTS.PAS }
