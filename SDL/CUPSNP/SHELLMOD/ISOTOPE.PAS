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

Unit Isotope;
Interface
   Uses
      cups, cupsfunc, cupsproc, cupsmupp;
   Type
      stateType = (gs, first, second, none);
      Str20 = String[20];
      messageArray = Array[1..2] Of String;
      elementStrArrayType = Array[0..103] Of Str20;
      NumOfIsotopesArrayType = Array[0..103] Of integer;
      FirstAarrayType = Array[0..103] Of integer;
      spinParityArrayType = Array[0..103, 1..7] Of Str20;
      StabilityArrayType = Array[0..103, 1..7] Of Str20;
      Array4 = Array[1..4] Of Str20;
      level2array = Array[1..2, 1..30] Of real;
      level2Str = Array[1..2, 1..30] Of Str20;
      levelType = Record
            degeneracy: Array[1..30] Of integer;
            sumdegen: Array[1..30] Of longint;
            energy: Array[1..30] Of real;
            levelLabel: Array[1..30] Of Str20;
            spinParity: Array[1..30] Of Str20;
         End;
      elementData = Record
            elementStr: Str20;
            Stability: Str20;
            spinparity: Array[1..3] Of Str20;
         End;
      SMNucleiType = Record
            neut, prot, A, lengthp, lengthn, lengthMNp, lengthMNn: integer;
            state: statetype;
            SubNlevels, SubPlevels: leveltype;
            subMNlevels: level2array;
            subMN: level2Str;
            element: elementdata;
         End;{SMNucleiType}

   Procedure GetIsotopeData (Var SMNucData: SMNucleiType;
                           Var errorIsotopeData: boolean;
                           Var messageStr: messageArray);
   Procedure GetNeutronLevels (Var minE, maxE: real;
                           Var SMNucData: SMNucleiType);
   Procedure GetProtonLevels (Var minE, maxE: real;
                           Var SMNucData: SMNucleiType);
   Procedure GetMNlevels (minEp, maxEp, minEn, maxEn: real;
                           Var SMNucData: SMNucleiType);


Implementation
   Function FillElementStr ({SMNucData: SMNucleiType}
                           prot: integer): Str20;
      Var
         elementStrArray: elementStrArrayType;
   Begin
      elementStrArray[0] := 'n ';
      elementStrArray[1] := 'H ';
      elementStrArray[2] := 'He';
      elementStrArray[3] := 'Li';
      elementStrArray[4] := 'Be';
      elementStrArray[5] := 'B ';
      elementStrArray[6] := 'C ';
      elementStrArray[7] := 'N ';
      elementStrArray[8] := 'O ';
      elementStrArray[9] := 'F ';
      elementStrArray[10] := 'Ne';
      elementStrArray[11] := 'Na ';
      elementStrArray[12] := 'Mg';
      elementStrArray[13] := 'Al';
      elementStrArray[14] := 'Si';
      elementStrArray[15] := 'P  ';
      elementStrArray[16] := 'S  ';
      elementStrArray[17] := 'Cl ';
      elementStrArray[18] := 'Ar ';
      elementStrArray[19] := 'K  ';
      elementStrArray[20] := 'Ca';
      elementStrArray[21] := 'Sc ';
      elementStrArray[22] := 'Ti';
      elementStrArray[23] := 'V ';
      elementStrArray[24] := 'Cr';
      elementStrArray[25] := 'Mn';
      elementStrArray[26] := 'Fe';
      elementStrArray[27] := 'Co';
      elementStrArray[28] := 'Ni';
      elementStrArray[29] := 'Cu';
      elementStrArray[30] := 'Zn';
      elementStrArray[31] := 'Ga';
      elementStrArray[32] := 'Ge';
      elementStrArray[33] := 'As';
      elementStrArray[34] := 'Se';
      elementStrArray[35] := 'Br';
      elementStrArray[36] := 'Kr';
      elementStrArray[37] := 'Rb';
      elementStrArray[38] := 'Sr';
      elementStrArray[39] := 'Y';
      elementStrArray[40] := 'Zr';
      elementStrArray[41] := 'Nb';
      elementStrArray[42] := 'Mo';
      elementStrArray[43] := 'Tc';
      elementStrArray[44] := 'Ru';
      elementStrArray[45] := 'Rh';
      elementStrArray[46] := 'Pd';
      elementStrArray[47] := 'Ag';
      elementStrArray[48] := 'Cd';
      elementStrArray[49] := 'In';
      elementStrArray[50] := 'Sn';
      elementStrArray[51] := 'Sb';
      elementStrArray[52] := 'Te';
      elementStrArray[53] := 'I ';
      elementStrArray[54] := 'Xe';
      elementStrArray[55] := 'Cs';
      elementStrArray[56] := 'Ba';
      elementStrArray[57] := 'La';
      elementStrArray[58] := 'Ce';
      elementStrArray[59] := 'Pr';
      elementStrArray[60] := 'Nd';
      elementStrArray[61] := 'Pm';
      elementStrArray[62] := 'Sm';
      elementStrArray[63] := 'Eu';
      elementStrArray[64] := 'Gd';
      elementStrArray[65] := 'Tb';
      elementStrArray[66] := 'Dy';
      elementStrArray[67] := 'Ho';
      elementStrArray[68] := 'Er';
      elementStrArray[69] := 'Tm';
      elementStrArray[70] := 'Yb';
      elementStrArray[71] := 'Lu';
      elementStrArray[72] := 'Hf';
      elementStrArray[73] := 'Ta';
      elementStrArray[74] := 'W ';
      elementStrArray[75] := 'Re';
      elementStrArray[76] := 'Os';
      elementStrArray[77] := 'Ir';
      elementStrArray[78] := 'Pt';
      elementStrArray[79] := 'Au';
      elementStrArray[80] := 'Hg';
      elementStrArray[81] := 'Tl';
      elementStrArray[82] := 'Pb';
      elementStrArray[83] := 'Bi';
      elementStrArray[84] := 'Po';
      elementStrArray[85] := 'At';
      elementStrArray[86] := 'Rn';
      elementStrArray[87] := 'Fr';
      elementStrArray[88] := 'Ra';
      elementStrArray[89] := 'Ac';
      elementStrArray[90] := 'Th';
      elementStrArray[91] := 'Pa';
      elementStrArray[92] := 'U ';
      elementStrArray[93] := 'Np';
      elementStrArray[94] := 'Pu ';
      elementStrArray[95] := 'Am';
      elementStrArray[96] := 'Cm';
      elementStrArray[97] := 'Bk';
      elementStrArray[98] := 'Cf';
      elementStrArray[99] := 'Es';
      elementStrArray[100] := 'Fm';
      elementStrArray[101] := 'Md';
      elementStrArray[102] := 'No';
      elementStrArray[103] := 'Lr';
{with SMNucData do}
      FillelementStr := elementStrArray[prot];
   End;{FillelementStr}

   Function FillNumOfIsotopes ({SMNucData: SMNucleiType}
                           prot: integer): integer;
      Var
         NumOfIsotopesArray: NumOfIsotopesArrayType;
   Begin
      NumOfIsotopesArray[0] := 1;
      NumOfIsotopesArray[1] := 2;
      NumOfIsotopesArray[2] := 1;
      NumOfIsotopesArray[3] := 1;
      NumOfIsotopesArray[4] := 2;
      NumOfIsotopesArray[5] := 2;
      NumOfIsotopesArray[6] := 2;
      NumOfIsotopesArray[7] := 2;
      NumOfIsotopesArray[8] := 3;
      NumOfIsotopesArray[9] := 2;
      NumOfIsotopesArray[10] := 3;
      NumOfIsotopesArray[11] := 3;
      NumOfIsotopesArray[12] := 3;
      NumOfIsotopesArray[13] := 2;
      NumOfIsotopesArray[14] := 2;
      NumOfIsotopesArray[15] := 2;
      NumOfIsotopesArray[16] := 4;
      NumOfIsotopesArray[17] := 4;
      NumOfIsotopesArray[18] := 4;
      NumOfIsotopesArray[19] := 5;
      NumOfIsotopesArray[20] := 3;
      NumOfIsotopesArray[21] := 5;
      NumOfIsotopesArray[22] := 4;
      NumOfIsotopesArray[23] := 4;
      NumOfIsotopesArray[24] := 4;
      NumOfIsotopesArray[25] := 3;
      NumOfIsotopesArray[26] := 4;
      NumOfIsotopesArray[27] := 3;
      NumOfIsotopesArray[28] := 5;
      NumOfIsotopesArray[29] := 4;
      NumOfIsotopesArray[30] := 5;
      NumOfIsotopesArray[31] := 3;
      NumOfIsotopesArray[32] := 4;
      NumOfIsotopesArray[33] := 3;
      NumOfIsotopesArray[34] := 5;
      NumOfIsotopesArray[35] := 5;
      NumOfIsotopesArray[36] := 6;
      NumOfIsotopesArray[37] := 4;
      NumOfIsotopesArray[38] := 3;
      NumOfIsotopesArray[39] := 2;
      NumOfIsotopesArray[40] := 5;
      NumOfIsotopesArray[41] := 2;
      NumOfIsotopesArray[42] := 4;
      NumOfIsotopesArray[43] := 1;
      NumOfIsotopesArray[44] := 4;
      NumOfIsotopesArray[45] := 1;
      NumOfIsotopesArray[46] := 3;
      NumOfIsotopesArray[47] := 5;
      NumOfIsotopesArray[48] := 6;
      NumOfIsotopesArray[49] := 5;{?????}
      NumOfIsotopesArray[50] := 5;
      NumOfIsotopesArray[51] := 6;
      NumOfIsotopesArray[52] := 7;
      NumOfIsotopesArray[53] := 6;
      NumOfIsotopesArray[54] := 4;
      NumOfIsotopesArray[55] := 7;
      NumOfIsotopesArray[56] := 4;
      NumOfIsotopesArray[57] := 1;
      NumOfIsotopesArray[58] := 4;
      NumOfIsotopesArray[59] := 2;
      NumOfIsotopesArray[60] := 5;
      NumOfIsotopesArray[61] := 4;
      NumOfIsotopesArray[62] := 4;
      NumOfIsotopesArray[63] := 0;
      NumOfIsotopesArray[64] := 1;
      NumOfIsotopesArray[65] := 0;
      NumOfIsotopesArray[66] := 1;
      NumOfIsotopesArray[67] := 0;
      NumOfIsotopesArray[68] := 0;
      NumOfIsotopesArray[69] := 0;
      NumOfIsotopesArray[70] := 0;
      NumOfIsotopesArray[71] := 0;
      NumOfIsotopesArray[72] := 0;
      NumOfIsotopesArray[73] := 0;{Ta}
      NumOfIsotopesArray[74] := 0;
      NumOfIsotopesArray[75] := 0;
      NumOfIsotopesArray[76] := 0;
      NumOfIsotopesArray[77] := 0;
      NumOfIsotopesArray[78] := 3;
      NumOfIsotopesArray[79] := 5;
      NumOfIsotopesArray[80] := 5;
      NumOfIsotopesArray[81] := 6;
      NumOfIsotopesArray[82] := 6;
      NumOfIsotopesArray[83] := 5;
      NumOfIsotopesArray[84] := 5;
      NumOfIsotopesArray[85] := 1;
{element[86].NumOfIsotopesArray[] := '0';}
{element[87].NumOfIsotopesArray[] := '0';}
{element[88].NumOfIsotopesArray[] := '0';}
{element[89].NumOfIsotopesArray[] := '0';}
{element[90].NumOfIsotopesArray[] := '0';}
{element[91].NumOfIsotopesArray[] := '0';}
{element[92].NumOfIsotopesArray[] := '0';}
{element[93].NumOfIsotopesArray[] := '0';}
{element[94].NumOfIsotopesArray[] := '0';}
{element[95].NumOfIsotopesArray[] := '0';}
{element[96].NumOfIsotopesArray[] := '0';}
{element[97].NumOfIsotopesArray[] := '0';}
{element[98].NumOfIsotopesArray[] := '0';}
{element[99].NumOfIsotopesArray[] := '0';}
{element[100].NumOfIsotopesArray[] := '0';}
{element[101].NumOfIsotopesArray[] := '0';}
{element[102].NumOfIsotopesArray[] := '0';}
{element[103].NumOfIsotopesArray[] := '0';}
{with SMNucData do}
      FillNumOfIsotopes := NumOfIsotopesArray[prot];
   End;{FillNumOfIsotopes}

   Function FillFirstA ({SMNucData: SMNucleiType}
                           prot: integer): integer;
      Var
         FirstAarray: FirstAarrayType;
   Begin
      FirstAarray[0] := 1;
      FirstAarray[1] := 1;
      FirstAarray[2] := 3;
      FirstAarray[3] := 7;
      FirstAarray[4] := 7;
      FirstAarray[5] := 11;
      FirstAarray[6] := 11;
      FirstAarray[7] := 13;
      FirstAarray[8] := 15;
      FirstAarray[9] := 17;
      FirstAarray[10] := 19;
      FirstAarray[11] := 21;
      FirstAarray[12] := 23;
      FirstAarray[13] := 25;
      FirstAarray[14] := 27;
      FirstAarray[15] := 29;
      FirstAarray[16] := 31;
      FirstAarray[17] := 33;
      FirstAarray[18] := 35;
      FirstAarray[19] := 37;
      FirstAarray[20] := 39;
      FirstAarray[21] := 41;
      FirstAarray[22] := 45;
      FirstAarray[23] := 47;
      FirstAarray[24] := 49;
      FirstAarray[25] := 51;
      FirstAarray[26] := 53;
      FirstAarray[27] := 55;
      FirstAarray[28] := 57;
      FirstAarray[29] := 61;
      FirstAarray[30] := 61;
      FirstAarray[31] := 67;
      FirstAarray[32] := 69;
      FirstAarray[33] := 71;
      FirstAarray[34] := 75;
      FirstAarray[35] := 77;
      FirstAarray[36] := 77;
      FirstAarray[37] := 81;
      FirstAarray[38] := 87;
      FirstAarray[39] := 89;
      FirstAarray[40] := 89;
      FirstAarray[41] := 93;
      FirstAarray[42] := 93;
      FirstAarray[43] := 99;
      FirstAarray[44] := 97;
      FirstAarray[45] := 103;
      FirstAarray[46] := 101;
      FirstAarray[47] := 103;
      FirstAarray[48] := 105;
      FirstAarray[49] := 109;
      FirstAarray[50] := 113;
      FirstAarray[51] := 115;
      FirstAarray[52] := 117;
      FirstAarray[53] := 123;
      FirstAarray[54] := 129;
      FirstAarray[55] := 125;
      FirstAarray[56] := 131;
      FirstAarray[57] := 139;
      FirstAarray[58] := 137;
      FirstAarray[59] := 141;
      FirstAarray[60] := 141;
      FirstAarray[61] := 143;
      FirstAarray[62] := 143;
      FirstAarray[63] := 0;
      FirstAarray[64] := 153;
      FirstAarray[65] := 0;
      FirstAarray[66] := 155;
      FirstAarray[67] := 0;
      FirstAarray[68] := 0;
      FirstAarray[69] := 0;
      FirstAarray[70] := 0;
      FirstAarray[71] := 0;
      FirstAarray[72] := 0;
      FirstAarray[73] := 0;
      FirstAarray[74] := 0;
      FirstAarray[75] := 0;
      FirstAarray[76] := 0;
      FirstAarray[77] := 0;
      FirstAarray[78] := 195;
      FirstAarray[79] := 191;
      FirstAarray[80] := 197;
      FirstAarray[81] := 195;
      FirstAarray[82] := 199;
      FirstAarray[83] := 199;
      FirstAarray[84] := 201;
      FirstAarray[85] := 211;
{element[86].FirstAarray[] := '0';}
{element[87].FirstAarray[] := '0';}
{element[88].FirstAarray[] := '0';}
{element[89].FirstAarray[] := '0';}
{element[90].FirstAarray[] := '0';}
{element[91].FirstAarray[] := '0';}
{element[92].FirstAarray[] := '0';}
{element[93].FirstAarray[] := '0';}
{element[94].FirstAarray[] := '0';}
{element[95].FirstAarray[] := '0';}
{element[96].FirstAarray[] := '0';}
{element[97].FirstAarray[] := '0';}
{element[98].FirstAarray[] := '0';}
{element[99].FirstAarray[] := '0';}
{element[100].FirstAarray[] := '0';}
{element[101].FirstAarray[] := '0';}
{element[102].FirstAarray[] := '0';}
{element[103].FirstAarray[] := '0';}
{with SMNucData do}
      FillFirstA := FirstAarray[prot];
   End;{FillFirstA}

   Function FillSpinParitygs ({SMNucData: SMNucleiType;}
                           prot, offset: integer): Str20;
{finished and checked. there are no values for nonspherical nuclei}
      Var
         spinParityArraygs: spinParityArrayType;
         i, j: integer;
   Begin
      spinParityArraygs[0, 1] := '1/2+ ';
      spinParityArraygs[1, 1] := '1/2+';
      spinParityArraygs[1, 2] := '1/2+ ';
      spinParityArraygs[2, 1] := '1/2+ ';
      spinParityArraygs[3, 1] := '3/2- ';
      spinParityArraygs[4, 1] := '3/2- ';
      spinParityArraygs[4, 2] := '3/2- ';
      spinParityArraygs[5, 1] := '3/2- ';
      spinParityArraygs[5, 2] := '3/2- ';
      spinParityArraygs[6, 1] := '3/2- ';
      spinParityArraygs[6, 2] := '1/2- ';
      spinParityArraygs[7, 1] := '1/2- ';
      spinParityArraygs[7, 2] := '1/2- ';
      spinParityArraygs[8, 1] := '1/2- ';
      spinParityArraygs[8, 2] := '5/2+ ';
      spinParityArraygs[8, 3] := '5/2+ ';
      spinParityArraygs[9, 1] := '5/2+ ';
      spinParityArraygs[9, 2] := '1/2+ ';
      spinParityArraygs[10, 1] := '1/2+ ';
      spinParityArraygs[10, 2] := '3/2+ [211]3/2 ';
      spinParityArraygs[10, 3] := '5/2+ ';
      spinParityArraygs[11, 1] := '3/2+ [211]3/2 ';
      spinParityArraygs[11, 2] := '3/2+ ';
      spinParityArraygs[11, 3] := '5/2+ ';
      spinParityArraygs[12, 1] := '3/2+ ';
      spinParityArraygs[12, 2] := '5/2+ ';
      spinParityArraygs[12, 3] := '1/2+ ';
      spinParityArraygs[13, 1] := '5/2+ ';
      spinParityArraygs[13, 2] := '5/2+ ';
      spinParityArraygs[14, 1] := '5/2+ ';
      spinParityArraygs[14, 2] := '1/2+ ';
      spinParityArraygs[15, 1] := '1/2+ ';
      spinParityArraygs[15, 2] := '1/2+ ';
      spinParityArraygs[16, 1] := '1/2+ ';
      spinParityArraygs[16, 2] := '3/2+ ';
      spinParityArraygs[16, 3] := '3/2+ ';
      spinParityArraygs[16, 4] := '5/2,7/2- ';
      spinParityArraygs[17, 1] := '3/2+ ';
      spinParityArraygs[17, 2] := '3/2+ ';
      spinParityArraygs[17, 3] := '3/2+ ';
      spinParityArraygs[17, 4] := '3/2+ ';
      spinParityArraygs[18, 1] := '3/2+ ';
      spinParityArraygs[18, 2] := '3/2+ ';
      spinParityArraygs[18, 3] := '7/2- ';
      spinParityArraygs[18, 4] := '7/2- ';
      spinParityArraygs[19, 1] := '3/2+ ';
      spinParityArraygs[19, 2] := '3/2+ ';
      spinParityArraygs[19, 3] := '3/2+ ';
      spinParityArraygs[19, 4] := '3/2+ ';
      spinParityArraygs[19, 5] := '3/2+ ';
      spinParityArraygs[20, 1] := '3/2+ ';
      spinParityArraygs[20, 2] := '7/2- ';
      spinParityArraygs[20, 3] := '7/2- ';
      spinParityArraygs[21, 1] := '7/2- ';
      spinParityArraygs[21, 2] := '7/2- ';
      spinParityArraygs[21, 3] := '7/2- ';
      spinParityArraygs[21, 4] := '7/2- ';
      spinParityArraygs[21, 5] := '(7/2)- ';
      spinParityArraygs[22, 1] := '7/2- ';
      spinParityArraygs[22, 2] := '5/2- ';{??}
      spinParityArraygs[22, 3] := '7/2- ';
      spinParityArraygs[22, 4] := '3/2- ';
      spinParityArraygs[23, 1] := '3/2- ';{??}
      spinParityArraygs[23, 2] := '7/2- ';
      spinParityArraygs[23, 3] := '7/2- ';
      spinParityArraygs[23, 4] := '7/2- ';{deformed?}
      spinParityArraygs[24, 1] := '5/2- ';{??}
      spinParityArraygs[24, 2] := '7/2- ';
      spinParityArraygs[24, 3] := '3/2- ';
      spinParityArraygs[24, 4] := '3/2- ';
      spinParityArraygs[25, 1] := '5/2- ';{deformed?}
      spinParityArraygs[25, 2] := '7/2- ';
      spinParityArraygs[25, 3] := '5/2- ';{??}
      spinParityArraygs[26, 1] := '7/2- ';
      spinParityArraygs[26, 2] := '3/2- ';
      spinParityArraygs[26, 3] := '1/2- ';{??}
      spinParityArraygs[26, 4] := '3/2- ';{??}
      spinParityArraygs[27, 1] := '7/2- ';
      spinParityArraygs[27, 2] := '7/2- ';
      spinParityArraygs[27, 3] := '7/2- ';
      spinParityArraygs[28, 1] := '3/2- ';
      spinParityArraygs[28, 2] := '3/2- ';
      spinParityArraygs[28, 3] := '3/2- ';
      spinParityArraygs[28, 4] := '1/2- ';
      spinParityArraygs[28, 5] := '5/2- ';
      spinParityArraygs[29, 1] := '3/2- ';
      spinParityArraygs[29, 2] := '3/2- ';
      spinParityArraygs[29, 3] := '3/2- ';
      spinParityArraygs[29, 4] := '3/2- ';
      spinParityArraygs[30, 1] := '3/2- ';
      spinParityArraygs[30, 2] := '3/2- ';
      spinParityArraygs[30, 3] := '5/2- ';
      spinParityArraygs[30, 4] := '5/2- ';
      spinParityArraygs[30, 5] := '1/2- ';
      spinParityArraygs[31, 1] := '3/2- ';
      spinParityArraygs[31, 2] := '3/2- ';
      spinParityArraygs[31, 3] := '3/2- ';
      spinParityArraygs[32, 1] := '5/2- ';
      spinParityArraygs[32, 2] := '1/2- ';
      spinParityArraygs[32, 3] := '9/2+ ';
      spinParityArraygs[32, 4] := '1/2- ';
      spinParityArraygs[33, 1] := '5/2- ';
      spinParityArraygs[33, 2] := '3/2- ';
      spinParityArraygs[33, 3] := '3/2- ';
      spinParityArraygs[34, 1] := '5/2+ ';{deformed}
      spinParityArraygs[34, 2] := '1/2- ';
      spinParityArraygs[34, 3] := '7/2+ ';{??}
      spinParityArraygs[34, 4] := '(1/2)- ';
      spinParityArraygs[34, 5] := '(9/2)+ ';
      spinParityArraygs[35, 1] := '3/2- ';
      spinParityArraygs[35, 2] := '3/2- ';
      spinParityArraygs[35, 3] := '3/2- ';
      spinParityArraygs[35, 4] := '(3/2)- ';
      spinParityArraygs[35, 5] := '3/2- ';
      spinParityArraygs[36, 1] := '(5/2+) ';{deformed}
      spinParityArraygs[36, 2] := '1/2- ';
      spinParityArraygs[36, 3] := '7/2+ ';{??}
      spinParityArraygs[36, 4] := '9/2+ ';
      spinParityArraygs[36, 5] := '9/2+ ';
      spinParityArraygs[36, 6] := '(5/2)+ ';
      spinParityArraygs[37, 1] := '3/2- ';
      spinParityArraygs[37, 2] := '5/2- ';
      spinParityArraygs[37, 3] := '5/2- ';
      spinParityArraygs[37, 4] := '3/2- ';
      spinParityArraygs[38, 1] := '9/2+ ';
      spinParityArraygs[38, 2] := '5/2+ ';
      spinParityArraygs[38, 3] := '5/2+ ';
      spinParityArraygs[39, 1] := '1/2- ';
      spinParityArraygs[39, 2] := '1/2- ';
      spinParityArraygs[40, 1] := '9/2+ ';
      spinParityArraygs[40, 2] := '5/2+ ';
      spinParityArraygs[40, 3] := '5/2+ ';
      spinParityArraygs[40, 4] := '5/2+ ';
      spinParityArraygs[40, 5] := '1/2+ ';
      spinParityArraygs[41, 1] := '9/2+ ';
      spinParityArraygs[41, 2] := '9/2+ ';
      spinParityArraygs[42, 1] := '5/2+ ';
      spinParityArraygs[42, 2] := '5/2+ ';
      spinParityArraygs[42, 3] := '5/2+ ';
      spinParityArraygs[42, 4] := '1/2+ ';
      spinParityArraygs[43, 1] := '9/2+ ';
      spinParityArraygs[44, 1] := '5/2+ ';
      spinParityArraygs[44, 2] := '5/2+ ';
      spinParityArraygs[44, 3] := '5/2+ ';
      spinParityArraygs[44, 4] := '5/2+ ';
      spinParityArraygs[45, 1] := '1/2- ';
      spinParityArraygs[46, 1] := '(5/2)+ ';
      spinParityArraygs[46, 2] := '5/2+ ';
      spinParityArraygs[46, 3] := '5/2+ ';
      spinParityArraygs[47, 1] := '7/2+ ';{??}
      spinParityArraygs[47, 2] := '1/2- ';
      spinParityArraygs[47, 3] := '1/2- ';
      spinParityArraygs[47, 4] := '1/2- ';
      spinParityArraygs[47, 5] := '1/2- ';
      spinParityArraygs[48, 1] := '5/2+ ';
      spinParityArraygs[48, 2] := '5/2+ ';
      spinParityArraygs[48, 3] := '5/2+ ';
      spinParityArraygs[48, 4] := '1/2+ ';
      spinParityArraygs[48, 5] := '1/2+ ';
      spinParityArraygs[48, 6] := '1/2+ ';
      spinParityArraygs[49, 1] := '9/2+ ';
      spinParityArraygs[49, 2] := '9/2+ ';
      spinParityArraygs[49, 3] := '9/2+ ';
      spinParityArraygs[49, 4] := '9/2+ ';
      spinParityArraygs[49, 5] := '9/2+ ';
      spinParityArraygs[50, 1] := '1/2+ ';
      spinParityArraygs[50, 2] := '1/2+ ';
      spinParityArraygs[50, 3] := '1/2+ ';
      spinParityArraygs[50, 4] := '1/2+ ';
      spinParityArraygs[50, 5] := '3/2+ ';
      spinParityArraygs[51, 1] := '5/2+ ';
      spinParityArraygs[51, 2] := '5/2+ ';
      spinParityArraygs[51, 3] := '5/2+ ';
      spinParityArraygs[51, 4] := '5/2+ ';
      spinParityArraygs[51, 5] := '7/2+ ';
      spinParityArraygs[51, 6] := '7/2+ ';
      spinParityArraygs[52, 1] := '1/2+ ';
      spinParityArraygs[52, 2] := '1/2+ ';
      spinParityArraygs[52, 3] := '1/2+ ';
      spinParityArraygs[52, 4] := '1/2+ ';
      spinParityArraygs[52, 5] := '1/2+ ';
      spinParityArraygs[52, 6] := '3/2+ ';
      spinParityArraygs[52, 7] := '3/2+ ';
      spinParityArraygs[53, 1] := '5/2+ ';
      spinParityArraygs[53, 2] := '5/2+ ';
      spinParityArraygs[53, 3] := '7/2+ ';
      spinParityArraygs[53, 4] := '7/2+ ';
      spinParityArraygs[53, 5] := '7/2+ ';
      spinParityArraygs[53, 6] := '7/2+ ';
      spinParityArraygs[54, 1] := '1/2+ ';
      spinParityArraygs[54, 2] := '3/2+ ';
      spinParityArraygs[54, 3] := '3/2+ ';
      spinParityArraygs[54, 4] := '3/2+ ';
      spinParityArraygs[55, 1] := '1/2+ ';
      spinParityArraygs[55, 2] := '1/2+ ';
      spinParityArraygs[55, 3] := '1/2+ ';
      spinParityArraygs[55, 4] := '5/2+ ';
      spinParityArraygs[55, 5] := '1/2+ ';
      spinParityArraygs[55, 6] := '7/2+ ';
      spinParityArraygs[55, 7] := '7/2+ ';
      spinParityArraygs[56, 1] := '1/2+ ';
      spinParityArraygs[56, 2] := '1/2+ ';
      spinParityArraygs[56, 3] := '3/2+ ';
      spinParityArraygs[56, 4] := '3/2+ ';
      spinParityArraygs[57, 1] := '7/2+ ';
      spinParityArraygs[58, 1] := '3/2+ ';
      spinParityArraygs[58, 2] := '3/2+ ';
      spinParityArraygs[58, 3] := '7/2- ';
      spinParityArraygs[58, 4] := '3/2- ';
      spinParityArraygs[59, 1] := '5/2+ ';
      spinParityArraygs[59, 2] := '7/2+ ';
      spinParityArraygs[60, 1] := '3/2+ ';
      spinParityArraygs[60, 2] := '7/2- ';
      spinParityArraygs[60, 3] := '7/2- ';
      spinParityArraygs[60, 4] := '5/2- ';
      spinParityArraygs[60, 5] := '5/2- ';
      spinParityArraygs[61, 1] := '5/2+ ';
      spinParityArraygs[61, 2] := '5/2+ ';
      spinParityArraygs[61, 3] := '7/2+ ';
      spinParityArraygs[61, 4] := '7/2+ ';
      spinParityArraygs[62, 1] := '3/2+ ';
      spinParityArraygs[62, 2] := '7/2- ';
      spinParityArraygs[62, 3] := '7/2- ';
      spinParityArraygs[62, 4] := '7/2- ';
      spinParityArraygs[63, 1] := ' ';
      spinParityArraygs[64, 1] := '3/2- ';
      spinParityArraygs[65, 1] := ' ';
      spinParityArraygs[66, 1] := '3/2- [521]3/2 ';
      spinParityArraygs[67, 1] := ' ';
      spinParityArraygs[68, 1] := ' ';
      spinParityArraygs[69, 1] := ' ';
      spinParityArraygs[70, 1] := ' ';
      spinParityArraygs[71, 1] := ' ';
      spinParityArraygs[72, 1] := ' ';
      spinParityArraygs[73, 1] := ' ';
      spinParityArraygs[74, 1] := ' ';
      spinParityArraygs[75, 1] := ' ';
      spinParityArraygs[76, 1] := ' ';
      spinParityArraygs[77, 1] := ' ';
      spinParityArraygs[78, 1] := '1/2- ';
      spinParityArraygs[78, 2] := '1/2- ';
      spinParityArraygs[78, 3] := '(5/2-) ';
      spinParityArraygs[79, 1] := '3/2+ ';
      spinParityArraygs[79, 2] := '3/2+ ';
      spinParityArraygs[79, 3] := '3/2+ ';
      spinParityArraygs[79, 4] := '3/2+ ';
      spinParityArraygs[79, 5] := '3/2+ ';
      spinParityArraygs[80, 1] := '1/2- ';
      spinParityArraygs[80, 2] := '1/2- ';
      spinParityArraygs[80, 3] := '3/2- ';
      spinParityArraygs[80, 4] := '5/2- ';
      spinParityArraygs[80, 5] := '1/2- ';
      spinParityArraygs[81, 1] := '1/2+ ';
      spinParityArraygs[81, 2] := '1/2+ ';
      spinParityArraygs[81, 3] := '1/2+ ';
      spinParityArraygs[81, 4] := '1/2+ ';
      spinParityArraygs[81, 5] := '1/2+ ';
      spinParityArraygs[81, 6] := '1/2+ ';
      spinParityArraygs[82, 1] := '5/2- ';
      spinParityArraygs[82, 2] := '5/2- ';
      spinParityArraygs[82, 3] := '5/2- ';
      spinParityArraygs[82, 4] := '5/2- ';
      spinParityArraygs[82, 5] := '1/2- ';
      spinParityArraygs[82, 6] := '9/2+ ';
      spinParityArraygs[83, 1] := '9/2- ';
      spinParityArraygs[83, 2] := '9/2- ';
      spinParityArraygs[83, 3] := '9/2- ';
      spinParityArraygs[83, 4] := '9/2- ';
      spinParityArraygs[83, 5] := '9/2- ';
      spinParityArraygs[84, 1] := '3/2(-) ';
      spinParityArraygs[84, 2] := '5/2- ';
      spinParityArraygs[84, 3] := '5/2- ';
      spinParityArraygs[84, 4] := '5/2- ';
      spinParityArraygs[84, 5] := '1/2- ';
      spinParityArraygs[85, 1] := '9/2- ';

      FillSpinParitygs := spinParityArraygs[prot, offset];
   End;{FillSpinParitygs}

   Function FillspinParityfirst ({SMNucData: SMNucleiType;}
                           prot, offset: integer): Str20;
      Var
         spinParityArrayfirst: spinParityArrayType;
   Begin
      spinParityArrayfirst[0, 1] := 'unbound ';
      spinParityArrayfirst[1, 1] := 'unbound ';
      spinParityArrayfirst[1, 2] := 'unbound ';
      spinParityArrayfirst[2, 1] := 'unbound ';
      spinParityArrayfirst[3, 1] := '1/2- ';
      spinParityArrayfirst[4, 1] := '1/2- ';
      spinParityArrayfirst[4, 2] := '1/2+ ';
      spinParityArrayfirst[5, 1] := '1/2- ';
      spinParityArrayfirst[5, 2] := '(1/2,3/2,5/2)+ ';
      spinParityArrayfirst[6, 1] := '1/2- ';
      spinParityArrayfirst[6, 2] := '1/2+ ';
      spinParityArrayfirst[7, 1] := '1/2+ ';
      spinParityArrayfirst[7, 2] := '5/2+ ';
      spinParityArrayfirst[8, 1] := '1/2+ ';
      spinParityArrayfirst[8, 2] := '1/2+ ';
      spinParityArrayfirst[8, 3] := '3/2+ ';
      spinParityArrayfirst[9, 1] := '1/2+ ';
      spinParityArrayfirst[9, 2] := '1/2- ';
      spinParityArrayfirst[10, 1] := '5/2+ ';
      spinParityArrayfirst[10, 2] := '1/2- 1/2[101]';
      spinParityArrayfirst[10, 3] := '1/2+ ';
      spinParityArrayfirst[11, 1] := '1/2+ 1/2[211] ';
      spinParityArrayfirst[11, 2] := '5/2+ ';
      spinParityArrayfirst[11, 3] := '3/2,5/2+ ';
      spinParityArrayfirst[12, 1] := '5/2+ ';
      spinParityArrayfirst[12, 2] := '1/2+ ';
      spinParityArrayfirst[12, 3] := '3/2+ ';
      spinParityArrayfirst[13, 1] := '1/2+ ';
      spinParityArrayfirst[13, 2] := '1/2+ ';
      spinParityArrayfirst[14, 1] := '1/2+ ';
      spinParityArrayfirst[14, 2] := '3/2+ ';
      spinParityArrayfirst[15, 1] := '3/2+ ';
      spinParityArrayfirst[15, 2] := '3/2+ ';
      spinParityArrayfirst[16, 1] := '3/2+ ';
      spinParityArrayfirst[16, 2] := '1/2+ ';
      spinParityArrayfirst[16, 3] := '1/2+ ';
      spinParityArrayfirst[16, 4] := '? ';
      spinParityArrayfirst[17, 1] := '1/2+ ';
      spinParityArrayfirst[17, 2] := '1/2+ ';
      spinParityArrayfirst[17, 3] := '1/2+ ';
      spinParityArrayfirst[17, 4] := '1/2+ ';
      spinParityArrayfirst[18, 1] := '1/2+ ';
      spinParityArrayfirst[18, 2] := '1/2+ ';
      spinParityArrayfirst[18, 3] := '3/2- ';
      spinParityArrayfirst[18, 4] := '(7/2-) ';
      spinParityArrayfirst[19, 1] := '1/2+ ';
      spinParityArrayfirst[19, 2] := '1/2+ ';
      spinParityArrayfirst[19, 3] := '1/2+ ';
      spinParityArrayfirst[19, 4] := '1/2+ ';
      spinParityArrayfirst[19, 5] := '1/2+ ';
      spinParityArrayfirst[20, 1] := '1/2+ ';
      spinParityArrayfirst[20, 2] := '3/2- ';
      spinParityArrayfirst[20, 3] := '5/2- ';
      spinParityArrayfirst[21, 1] := '3/2- ';
      spinParityArrayfirst[21, 2] := '3/2+ ';
      spinParityArrayfirst[21, 3] := '3/2+ ';
      spinParityArrayfirst[21, 4] := '3/2+ ';
      spinParityArrayfirst[21, 5] := '1/2+ ';
      spinParityArrayfirst[22, 1] := '(3/2)- ';
      spinParityArrayfirst[22, 2] := '7/2- ';{??}
      spinParityArrayfirst[22, 3] := '3/2- ';
      spinParityArrayfirst[22, 4] := '1/2- ';
      spinParityArrayfirst[23, 1] := '5/2- ';{??}
      spinParityArrayfirst[23, 2] := '5/2- ';
      spinParityArrayfirst[23, 3] := '5/2- ';
      spinParityArrayfirst[23, 4] := '(5/2)- ';{deformed?}
      spinParityArrayfirst[24, 1] := '7/2- ';{??}
      spinParityArrayfirst[24, 2] := '3/2- ';
      spinParityArrayfirst[24, 3] := '1/2- ';
      spinParityArrayfirst[24, 4] := '(1/2)- ';
      spinParityArrayfirst[25, 1] := '7/2- ';{deformed?}
      spinParityArrayfirst[25, 2] := '5/2- ';
      spinParityArrayfirst[25, 3] := '7/2- ';{??}
      spinParityArrayfirst[26, 1] := '3/2- ';
      spinParityArrayfirst[26, 2] := '1/2- ';
      spinParityArrayfirst[26, 3] := '3/2- ';{??}
      spinParityArrayfirst[26, 4] := '1/2- ';{??}
      spinParityArrayfirst[27, 1] := '3/2- ';
      spinParityArrayfirst[27, 2] := '9/2- ';
      spinParityArrayfirst[27, 3] := '3/2- ';
      spinParityArrayfirst[28, 1] := '5/2- ';
      spinParityArrayfirst[28, 2] := '5/2- ';
      spinParityArrayfirst[28, 3] := '5/2- ';
      spinParityArrayfirst[28, 4] := '5/2- ';
      spinParityArrayfirst[28, 5] := '1/2- ';
      spinParityArrayfirst[29, 1] := '1/2- ';
      spinParityArrayfirst[29, 2] := '1/2- ';
      spinParityArrayfirst[29, 3] := '1/2- ';
      spinParityArrayfirst[29, 4] := '? ';
      spinParityArrayfirst[30, 1] := '? ';
      spinParityArrayfirst[30, 2] := '5/2- ';
      spinParityArrayfirst[30, 3] := '1/2- ';
      spinParityArrayfirst[30, 4] := '1/2- ';
      spinParityArrayfirst[30, 5] := '9/2+ ';
      spinParityArrayfirst[31, 1] := '1/2- ';
      spinParityArrayfirst[31, 2] := '1/2- ';
      spinParityArrayfirst[31, 3] := '1/2- ';
      spinParityArrayfirst[32, 1] := '1/2- ';
      spinParityArrayfirst[32, 2] := '5/2- ';
      spinParityArrayfirst[32, 3] := '5/2+ ';
      spinParityArrayfirst[32, 4] := '7/2+ ';
      spinParityArrayfirst[33, 1] := '? ';
      spinParityArrayfirst[33, 2] := '5/2- ';
      spinParityArrayfirst[33, 3] := '1/2- ';
      spinParityArrayfirst[34, 1] := '7/2+ ';{deformed}
      spinParityArrayfirst[34, 2] := '7/2+ ';
      spinParityArrayfirst[34, 3] := '1/2- ';{??}
      spinParityArrayfirst[34, 4] := '(7/2)+ ';
      spinParityArrayfirst[34, 5] := '(1/2)- ';
      spinParityArrayfirst[35, 1] := '9/2+ ';
      spinParityArrayfirst[35, 2] := '9/2+ ';
      spinParityArrayfirst[35, 3] := '5/2- ';
      spinParityArrayfirst[35, 4] := '(5/2)- ';
      spinParityArrayfirst[35, 5] := '(5/2)- ';
      spinParityArrayfirst[36, 1] := '(3/2-) ';{deformed}
      spinParityArrayfirst[36, 2] := '7/2+ ';
      spinParityArrayfirst[36, 3] := '(9/2)+ ';{??}
      spinParityArrayfirst[36, 4] := '7/2(+) ';
      spinParityArrayfirst[36, 5] := '1/2- ';
      spinParityArrayfirst[36, 6] := '1/2+ ';
      spinParityArrayfirst[37, 1] := '3/2- ';{*****************}
      spinParityArrayfirst[37, 2] := '5/2- ';
      spinParityArrayfirst[37, 3] := '5/2- ';
      spinParityArrayfirst[37, 4] := '3/2- ';
      spinParityArrayfirst[38, 1] := '9/2+ ';
      spinParityArrayfirst[38, 2] := '5/2+ ';
      spinParityArrayfirst[38, 3] := '5/2+ ';
      spinParityArrayfirst[39, 1] := '1/2- ';
      spinParityArrayfirst[39, 2] := '1/2- ';
      spinParityArrayfirst[40, 1] := '9/2+ ';
      spinParityArrayfirst[40, 2] := '5/2+ ';
      spinParityArrayfirst[40, 3] := '5/2+ ';
      spinParityArrayfirst[40, 4] := '5/2+ ';
      spinParityArrayfirst[40, 5] := '1/2+ ';
      spinParityArrayfirst[41, 1] := '9/2+ ';
      spinParityArrayfirst[41, 2] := '9/2+ ';
      spinParityArrayfirst[42, 1] := '5/2+ ';
      spinParityArrayfirst[42, 2] := '5/2+ ';
      spinParityArrayfirst[42, 3] := '5/2+ ';
      spinParityArrayfirst[42, 4] := '5/2+ ';
      spinParityArrayfirst[43, 1] := '9/2+ ';
      spinParityArrayfirst[44, 1] := '5/2+ ';
      spinParityArrayfirst[44, 2] := '5/2+ ';
      spinParityArrayfirst[44, 3] := '5/2+ ';
      spinParityArrayfirst[44, 4] := '5/2+ ';
      spinParityArrayfirst[45, 1] := '1/2- ';
      spinParityArrayfirst[46, 1] := '7/2+ ';
      spinParityArrayfirst[46, 2] := '5/2+ ';
      spinParityArrayfirst[46, 3] := '5/2+ ';
      spinParityArrayfirst[47, 1] := '7/2+ ';{??}
      spinParityArrayfirst[47, 2] := '1/2- ';
      spinParityArrayfirst[47, 3] := '1/2- ';
      spinParityArrayfirst[47, 4] := '1/2- ';
      spinParityArrayfirst[47, 5] := '1/2- ';
      spinParityArrayfirst[48, 1] := '5/2+ ';
      spinParityArrayfirst[48, 2] := '5/2+ ';
      spinParityArrayfirst[48, 3] := '5/2+ ';
      spinParityArrayfirst[48, 4] := '1/2+ ';
      spinParityArrayfirst[48, 5] := '1/2+ ';
      spinParityArrayfirst[48, 6] := '1/2+ ';
      spinParityArrayfirst[49, 1] := '9/2+ ';
      spinParityArrayfirst[49, 2] := '9/2+ ';
      spinParityArrayfirst[49, 3] := '9/2+ ';
      spinParityArrayfirst[49, 4] := '1/2- ';
      spinParityArrayfirst[49, 5] := '9/2+ ';
      spinParityArrayfirst[50, 1] := '7/2+ ';
      spinParityArrayfirst[50, 2] := '(3/2)+ ';
      spinParityArrayfirst[50, 3] := '3/2+ ';
      spinParityArrayfirst[50, 4] := '3/2+ ';
      spinParityArrayfirst[50, 5] := '11/2- ';
{******************************}
      spinParityArrayfirst[51, 1] := '(7/2+) ';
      spinParityArrayfirst[51, 2] := '7/2+ ';
      spinParityArrayfirst[51, 3] := '(7/2)+ ';
      spinParityArrayfirst[51, 4] := '7/2+ ';
      spinParityArrayfirst[51, 5] := '5/2+ ';
      spinParityArrayfirst[51, 6] := '5/2+ ';
      spinParityArrayfirst[52, 1] := '5/2+ ';
      spinParityArrayfirst[52, 2] := '5/2+ ';
      spinParityArrayfirst[52, 3] := '5/2+ ';
      spinParityArrayfirst[52, 4] := '1/2+ ';
      spinParityArrayfirst[52, 5] := '1/2+ ';
      spinParityArrayfirst[52, 6] := '1/2+ ';
      spinParityArrayfirst[52, 7] := '1/2+ ';
      spinParityArrayfirst[53, 1] := '5/2+ ';
      spinParityArrayfirst[53, 2] := '5/2+ ';
      spinParityArrayfirst[53, 3] := '5/2+ ';
      spinParityArrayfirst[53, 4] := '1/2+ ';
      spinParityArrayfirst[53, 5] := '1/2+ ';
      spinParityArrayfirst[53, 6] := '1/2+ ';
      spinParityArrayfirst[54, 1] := '5/2+ ';
      spinParityArrayfirst[54, 2] := '5/2+ ';
      spinParityArrayfirst[54, 3] := '5/2+ ';
      spinParityArrayfirst[54, 4] := '1/2+ ';
      spinParityArrayfirst[55, 1] := '5/2+ ';
      spinParityArrayfirst[55, 2] := '5/2+ ';
      spinParityArrayfirst[55, 3] := '5/2+ ';
      spinParityArrayfirst[55, 4] := '1/2+ ';
      spinParityArrayfirst[55, 5] := '1/2+ ';
      spinParityArrayfirst[55, 6] := '1/2+ ';
      spinParityArrayfirst[55, 7] := '1/2+ ';
      spinParityArrayfirst[56, 1] := '5/2+ ';
      spinParityArrayfirst[56, 2] := '5/2+ ';
      spinParityArrayfirst[56, 3] := '5/2+ ';
      spinParityArrayfirst[56, 4] := '1/2+ ';
      spinParityArrayfirst[57, 1] := '5/2+ ';
      spinParityArrayfirst[58, 1] := '5/2+ ';
      spinParityArrayfirst[58, 2] := '5/2+ ';
      spinParityArrayfirst[58, 3] := '5/2+ ';
      spinParityArrayfirst[58, 4] := '1/2+ ';
      spinParityArrayfirst[59, 1] := '5/2+ ';
      spinParityArrayfirst[59, 2] := '5/2+ ';
      spinParityArrayfirst[60, 1] := '5/2+ ';
      spinParityArrayfirst[60, 2] := '5/2+ ';
      spinParityArrayfirst[60, 3] := '5/2+ ';
      spinParityArrayfirst[60, 4] := '1/2+ ';
      spinParityArrayfirst[60, 5] := '1/2+ ';

{with SMNucData do}
      FillSpinParityfirst := spinParityArrayfirst[prot, offset];
   End;{FillSpinParityfirst}

   Function FillspinParitySecond ({SMNucData: SMNucleiType;}
                           prot, offset: integer): Str20;
      Var
         spinParityArraysecond: spinParityArrayType;
   Begin
      spinParityArraysecond[0, 1] := 'unbound ';
      spinParityArraysecond[1, 1] := 'unbound ';
      spinParityArraysecond[1, 2] := 'unbound ';
      spinParityArraysecond[2, 1] := 'unbound ';
      spinParityArraysecond[3, 1] := '7/2- ';
      spinParityArraysecond[4, 1] := '7/2- ';
      spinParityArraysecond[4, 2] := '5/2- ';
      spinParityArraysecond[5, 1] := '5/2- ';
      spinParityArraysecond[5, 2] := '(1/2,5/2,7/2)- ';
      spinParityArraysecond[6, 1] := '5/2- ';
      spinParityArraysecond[6, 2] := '3/2- ';
      spinParityArraysecond[7, 1] := '3/2- ';
      spinParityArraysecond[7, 2] := '1/2+ ';
      spinParityArraysecond[8, 1] := '5/2+ ';
      spinParityArraysecond[8, 2] := '1/2- ';
      spinParityArraysecond[8, 3] := '1/2+ ';
      spinParityArraysecond[9, 1] := '1/2- ';
      spinParityArraysecond[9, 2] := '5/2+ ';
      spinParityArraysecond[10, 1] := '1/2- ';
      spinParityArraysecond[10, 2] := '1/2+ 1/2[211] ';
      spinParityArraysecond[10, 3] := '7/2 ';
      spinParityArraysecond[11, 1] := '1/2- 1/2[101] ';
      spinParityArraysecond[11, 2] := '7/2+ ';
      spinParityArraysecond[11, 3] := '1/2+ ';
      spinParityArraysecond[12, 1] := '7/2+ ';
      spinParityArraysecond[12, 2] := '3/2+ ';
      spinParityArraysecond[12, 3] := '5/2+ ';
      spinParityArraysecond[13, 1] := '3/2+ ';
      spinParityArraysecond[13, 2] := '3/2+ ';
      spinParityArraysecond[14, 1] := '3/2+ ';
      spinParityArraysecond[14, 2] := '5/2+ ';
      spinParityArraysecond[15, 1] := '5/2+ ';
      spinParityArraysecond[15, 2] := '5/2+ ';
      spinParityArraysecond[16, 1] := '5/2+ ';
      spinParityArraysecond[16, 2] := '5/2+ ';
      spinParityArraysecond[16, 3] := '5/2,7/2- ';
      spinParityArraysecond[16, 4] := '? ';
      spinParityArraysecond[17, 1] := '5/2+ ';
      spinParityArraysecond[17, 2] := '5/2+ ';
      spinParityArraysecond[17, 3] := '(5/2+) ';
      spinParityArraysecond[17, 4] := '? ';
      spinParityArraysecond[18, 1] := '5/2,3/2+ ';
      spinParityArraysecond[18, 2] := '7/2- ';
      spinParityArraysecond[18, 3] := '3/2+ ';
      spinParityArraysecond[18, 4] := '3/2- ';
      spinParityArraysecond[19, 1] := '5/2,7/2- ';
      spinParityArraysecond[19, 2] := '7/2- ';
      spinParityArraysecond[19, 3] := '7/2- ';
      spinParityArraysecond[19, 4] := '7/2- ';
      spinParityArraysecond[19, 5] := '? ';
      spinParityArraysecond[20, 1] := '7/2- ';
      spinParityArraysecond[20, 2] := '3/2+ ';
      spinParityArraysecond[20, 3] := '3/2- ';
      spinParityArraysecond[21, 1] := '(3/2)+ ';
      spinParityArraysecond[21, 2] := '3/2- ';
      spinParityArraysecond[21, 3] := '3/2- ';
      spinParityArraysecond[21, 4] := '3/2- ';
      spinParityArraysecond[21, 5] := '3/2+ ';
      spinParityArraysecond[22, 1] := '(5/2)- ';
      spinParityArraysecond[22, 2] := '9/2- ';{??}
      spinParityArraysecond[22, 3] := '? ';
      spinParityArraysecond[22, 4] := '7/2- ';
      spinParityArraysecond[23, 1] := '7/2- ';{??}
      spinParityArraysecond[23, 2] := '3/2- ';
      spinParityArraysecond[23, 3] := '3/2- ';
      spinParityArraysecond[23, 4] := '(3/2)- ';{deformed?}
      spinParityArraysecond[24, 1] := '9/2- ';{??}
      spinParityArraysecond[24, 2] := '1/2- ';
      spinParityArraysecond[24, 3] := '5/2- ';
      spinParityArraysecond[24, 4] := '(5/2)- ';
      spinParityArraysecond[25, 1] := '9/2- ';{deformed?}
      spinParityArraysecond[25, 2] := '3/2- ';
      spinParityArraysecond[25, 3] := '9/2- ';{??}
      spinParityArraysecond[26, 1] := '9/2- ';
      spinParityArraysecond[26, 2] := '5/2- ';
      spinParityArraysecond[26, 3] := '5/2- ';{??}
      spinParityArraysecond[26, 4] := '5/2- ';{??}
      spinParityArraysecond[27, 1] := '3/2- ';
      spinParityArraysecond[27, 2] := '3/2- ';
      spinParityArraysecond[27, 3] := '9/2- ';
      spinParityArraysecond[28, 1] := '1/2- ';
      spinParityArraysecond[28, 2] := '1/2- ';
      spinParityArraysecond[28, 3] := '1/2- ';
      spinParityArraysecond[28, 4] := '3/2- ';
      spinParityArraysecond[28, 5] := '(3/2)- ';
      spinParityArraysecond[29, 1] := '5/2- ';
      spinParityArraysecond[29, 2] := '5/2- ';
      spinParityArraysecond[29, 3] := '5/2- ';
      spinParityArraysecond[29, 4] := '? ';
      spinParityArraysecond[30, 1] := '? ';
      spinParityArraysecond[30, 2] := '? ';
      spinParityArraysecond[30, 3] := '3/2- ';
      spinParityArraysecond[30, 4] := '3/2- ';
      spinParityArraysecond[30, 5] := '5/2- ';
      spinParityArraysecond[31, 1] := '5/2- ';
      spinParityArraysecond[31, 2] := '5/2- ';
      spinParityArraysecond[31, 3] := '5/2- ';
      spinParityArraysecond[32, 1] := '3/2- ';
      spinParityArraysecond[32, 2] := '9/2+ ';
      spinParityArraysecond[32, 3] := '1/2- ';
      spinParityArraysecond[32, 4] := '9/2+ ';
      spinParityArraysecond[33, 1] := '(3/2)- ';
      spinParityArraysecond[33, 2] := '(3/2)- ';
      spinParityArraysecond[33, 3] := '3/2- ';
      spinParityArraysecond[34, 1] := '9/2+ ';{deformed}
      spinParityArraysecond[34, 2] := '9/2+ ';
      spinParityArraysecond[34, 3] := '5/2- ';{??}
      spinParityArraysecond[34, 4] := '(9/2)+ ';
      spinParityArraysecond[34, 5] := '(1/2+) ';
      spinParityArraysecond[35, 1] := '(5/2+) ';
      spinParityArraysecond[35, 2] := '5/2- ';
      spinParityArraysecond[35, 3] := '9/2+ ';
      spinParityArraysecond[35, 4] := '? ';
      spinParityArraysecond[35, 5] := '? ';
      spinParityArraysecond[36, 1] := '(7/2+) ';{deformed}
      spinParityArraysecond[36, 2] := '5/2- ';
      spinParityArraysecond[36, 3] := '1/2- ';{??}
      spinParityArraysecond[36, 4] := '1/2(-) ';
      spinParityArraysecond[36, 5] := '(3/2-) ';
      spinParityArraysecond[36, 6] := '(5/2+) ';
      spinParityArraysecond[37, 1] := '3/2- ';{*************}
      spinParityArraysecond[37, 2] := '5/2- ';
      spinParityArraysecond[37, 3] := '5/2- ';
      spinParityArraysecond[37, 4] := '3/2- ';
      spinParityArraysecond[38, 1] := '9/2+ ';
      spinParityArraysecond[38, 2] := '5/2+ ';
      spinParityArraysecond[38, 3] := '5/2+ ';
      spinParityArraysecond[39, 1] := '1/2- ';
      spinParityArraysecond[39, 2] := '1/2- ';
      spinParityArraysecond[40, 1] := '9/2+ ';
      spinParityArraysecond[40, 2] := '5/2+ ';
      spinParityArraysecond[40, 3] := '5/2+ ';
      spinParityArraysecond[40, 4] := '5/2+ ';
      spinParityArraysecond[40, 5] := '1/2+ ';
      spinParityArraysecond[41, 1] := '9/2+ ';
      spinParityArraysecond[41, 2] := '9/2+ ';
      spinParityArraysecond[42, 1] := '5/2+ ';
      spinParityArraysecond[42, 2] := '5/2+ ';
      spinParityArraysecond[42, 3] := '5/2+ ';
      spinParityArraysecond[42, 4] := '5/2+ ';
      spinParityArraysecond[43, 1] := '9/2+ ';
      spinParityArraysecond[44, 1] := '5/2+ ';
      spinParityArraysecond[44, 2] := '5/2+ ';
      spinParityArraysecond[44, 3] := '5/2+ ';
      spinParityArraysecond[44, 4] := '5/2+ ';
      spinParityArraysecond[45, 1] := '1/2- ';
      spinParityArraysecond[46, 1] := '7/2+ ';
      spinParityArraysecond[46, 2] := '5/2+ ';
      spinParityArraysecond[46, 3] := '5/2+ ';
      spinParityArraysecond[47, 1] := '7/2+ ';{??}
      spinParityArraysecond[47, 2] := '1/2- ';
      spinParityArraysecond[47, 3] := '1/2- ';
      spinParityArraysecond[47, 4] := '1/2- ';
      spinParityArraysecond[47, 5] := '1/2- ';
      spinParityArraysecond[48, 1] := '5/2+ ';
      spinParityArraysecond[48, 2] := '5/2+ ';
      spinParityArraysecond[48, 3] := '5/2+ ';
      spinParityArraysecond[48, 4] := '1/2+ ';
      spinParityArraysecond[48, 5] := '1/2+ ';
      spinParityArraysecond[48, 6] := '1/2+ ';
      spinParityArraysecond[49, 1] := '9/2+ ';
      spinParityArraysecond[49, 2] := '9/2+ ';
      spinParityArraysecond[49, 3] := '9/2+ ';
      spinParityArraysecond[49, 4] := '1/2- ';
      spinParityArraysecond[49, 5] := '9/2+ ';
      spinParityArraysecond[50, 1] := '(5/2+) ';
      spinParityArraysecond[50, 2] := '(7/2+) ';
      spinParityArraysecond[50, 3] := '11/2- ';
      spinParityArraysecond[50, 4] := '11/2- ';
      spinParityArraysecond[50, 5] := '1/2+ ';
{******************************}
      spinParityArraysecond[51, 1] := '(1/2+) ';
      spinParityArraysecond[51, 2] := '1/2+ ';
      spinParityArraysecond[51, 3] := '1/2+ ';
      spinParityArraysecond[51, 4] := '1/2+ ';
      spinParityArraysecond[51, 5] := '3/2+ ';
      spinParityArraysecond[51, 6] := '(3/2)+ ';
      spinParityArraysecond[52, 1] := '5/2+ ';
      spinParityArraysecond[52, 2] := '5/2+ ';
      spinParityArraysecond[52, 3] := '5/2+ ';
      spinParityArraysecond[52, 4] := '1/2+ ';
      spinParityArraysecond[52, 5] := '1/2+ ';
      spinParityArraysecond[52, 6] := '1/2+ ';
      spinParityArraysecond[52, 7] := '1/2+ ';
      spinParityArraysecond[53, 1] := '5/2+ ';
      spinParityArraysecond[53, 2] := '5/2+ ';
      spinParityArraysecond[53, 3] := '5/2+ ';
      spinParityArraysecond[53, 4] := '1/2+ ';
      spinParityArraysecond[53, 5] := '1/2+ ';
      spinParityArraysecond[53, 6] := '1/2+ ';
      spinParityArraysecond[54, 1] := '5/2+ ';
      spinParityArraysecond[54, 2] := '5/2+ ';
      spinParityArraysecond[54, 3] := '5/2+ ';
      spinParityArraysecond[54, 4] := '1/2+ ';
      spinParityArraysecond[55, 1] := '5/2+ ';
      spinParityArraysecond[55, 2] := '5/2+ ';
      spinParityArraysecond[55, 3] := '5/2+ ';
      spinParityArraysecond[55, 4] := '1/2+ ';
      spinParityArraysecond[55, 5] := '1/2+ ';
      spinParityArraysecond[55, 6] := '1/2+ ';
      spinParityArraysecond[55, 7] := '1/2+ ';
      spinParityArraysecond[56, 1] := '5/2+ ';
      spinParityArraysecond[56, 2] := '5/2+ ';
      spinParityArraysecond[56, 3] := '5/2+ ';
      spinParityArraysecond[56, 4] := '1/2+ ';
      spinParityArraysecond[57, 1] := '5/2+ ';
      spinParityArraysecond[58, 1] := '5/2+ ';
      spinParityArraysecond[58, 2] := '5/2+ ';
      spinParityArraysecond[58, 3] := '5/2+ ';
      spinParityArraysecond[58, 4] := '1/2+ ';
      spinParityArraysecond[59, 1] := '5/2+ ';
      spinParityArraysecond[59, 2] := '5/2+ ';
      spinParityArraysecond[60, 1] := '5/2+ ';
      spinParityArraysecond[60, 2] := '5/2+ ';
      spinParityArraysecond[60, 3] := '5/2+ ';
      spinParityArraysecond[60, 4] := '1/2+ ';
      spinParityArraysecond[60, 5] := '1/2+ ';

{with SMNucData do}
      FillSpinParitySecond := spinParityArraysecond[prot, offset];
   End;{FillSpinParitySecond}

   Function FillStability ({SMNucData: SMNucleiType;}
                           prot, offset: integer): Str20;
      Var
         stability: StabilityArrayType;
   Begin
      stability[0, 1] := '10.6 m á- ';
      stability[1, 1] := '99.985% ';
      stability[1, 2] := '12.3 y á- ';
      stability[2, 1] := '1.4e-4% ';
      stability[3, 1] := '92.5% ';
      stability[4, 1] := '53.3 d e ';
      stability[4, 2] := '100% ';
      stability[5, 1] := '80.2% ';
      stability[5, 2] := '17.4 ms á-,á-n ';
      stability[6, 1] := '20.4 m e ';
      stability[6, 2] := '1.11% ';
      stability[7, 1] := '10.0 m e ';
      stability[7, 2] := '0.366% ';
      stability[8, 1] := '122 s e ';
      stability[8, 2] := '0.038% ';
      stability[8, 3] := '26.9 s á- ';
      stability[9, 1] := '64.5 s e ';
      stability[9, 2] := '100% ';
      stability[10, 1] := '17.3 s e ';
      stability[10, 2] := '0.27% ';
      stability[10, 3] := '37.6 s á- ';
      stability[11, 1] := '22.5 s e ';
      stability[11, 2] := '100% ';
      stability[11, 3] := '60 s á- ';
      stability[12, 1] := '11.3 s e ';
      stability[12, 2] := '10.00% ';
      stability[12, 3] := '9.46 m á- ';
      stability[13, 1] := '7.18 s e ';
      stability[13, 2] := '100% ';
      stability[14, 1] := '4.1 s á+ ';{*******************}
      stability[14, 2] := '4.67% ';
      stability[15, 1] := '4.1 s á+ ';
      stability[15, 2] := '100% ';
      stability[16, 1] := '2.6 s á+ ';
      stability[16, 2] := '0.75% ';
      stability[16, 3] := '87.4 d á- ';
      stability[16, 4] := '5.0 m á- ';
      stability[17, 1] := '2.5 s á+ ';
      stability[17, 2] := '75.8% ';
      stability[17, 3] := '24.2% ';
      stability[17, 4] := '56.2 m á- ';
      stability[18, 1] := '1.8 s á+ ';
      stability[18, 2] := '35.0 d ec ';
      stability[18, 3] := '269 y á- ';
      stability[18, 4] := '1.8 h á- ';
      stability[19, 1] := '1.2 s á+ ';
      stability[19, 2] := '93.26% ';
      stability[19, 3] := '6.73%- ';
      stability[19, 4] := '22.3 h á- ';
      stability[19, 5] := '20 m á- ';
      stability[20, 1] := '0.86 s á+ ';
      stability[20, 2] := '1.0x10^5 y ec ';
      stability[20, 3] := '0.135% ';
      stability[21, 1] := '0.60 s á+ ';
      stability[21, 2] := '3.89 h á+,ec ';
      stability[21, 3] := '100% ';
      stability[21, 4] := '3.4 d á- ';
      stability[21, 5] := '57.0 m á- ';
      stability[22, 1] := '3.1 h á+,ec ';
      stability[22, 2] := '7.4% ';{??}
      stability[22, 3] := '5.4% ';
      stability[22, 4] := '5.8 m á- ';
      stability[23, 1] := '32.6 m á+,ec ';{??}
      stability[23, 2] := '330 d ec ';
      stability[23, 3] := '99.75% ';
      stability[23, 4] := '1.6 m á- ';{deformed?}
      stability[24, 1] := '41.9 m á+,ec ';{??}
      stability[24, 2] := '27.7 d ec ';
      stability[24, 3] := '9.50% ';
      stability[24, 4] := '3.6 m á- ';
      stability[25, 1] := '46.2 m á+,ec ';{deformed?}
      stability[25, 2] := '3.7x10^6 y ec ';
      stability[25, 3] := '100% ';{??}
      stability[26, 1] := '8.51 m á+,ec ';
      stability[26, 2] := '2.7 y ec ';
      stability[26, 3] := '2.15% ';{??}
      stability[26, 4] := '44.6 d á- ';{??}
      stability[27, 1] := '17.5 h á+,ec ';
      stability[27, 2] := '271 d ec ';
      stability[27, 3] := '100% ';
      stability[28, 1] := '36.0 h á+,ec ';
      stability[28, 2] := '7.5x10^4 y á+,ec ';
      stability[28, 3] := '1.13% ';
      stability[28, 4] := '100 y á- ';
      stability[28, 5] := '2.5 h á- ';
      stability[29, 1] := '3.4 h á+,ec ';
      stability[29, 2] := '69.2% ';
      stability[29, 3] := '30.8% ';
      stability[29, 4] := '61.9 h á- ';
      stability[30, 1] := '89.1 s á+,ec ';
      stability[30, 2] := '38.1 m á+,ec ';
      stability[30, 3] := '244.1 d á+,ec ';
      stability[30, 4] := '4.10% ';
      stability[30, 5] := '56 m á- ';
      stability[31, 1] := '78.3 h ec ';
      stability[31, 2] := '60.1% ';
      stability[31, 3] := '39.9% ';
      stability[32, 1] := '39.0 h á+,ec ';
      stability[32, 2] := '11.2 d ec ';
      stability[32, 3] := '7.8% ';
      stability[32, 4] := '82.8 m á- ';
      stability[33, 1] := '61 h á+,ec ';
      stability[33, 2] := '80.3 d ec ';
      stability[33, 3] := '100% ';
      stability[34, 1] := '118.5 d ec ';{deformed}
      stability[34, 2] := '7.6% ';
      stability[34, 3] := '<=6.5x10^4 y á- ';{??}
      stability[34, 4] := '18.5 m á- ';
      stability[34, 5] := '22.5 m á- ';
      stability[35, 1] := '57.0 h á+,ec ';
      stability[35, 2] := '50.69% ';
      stability[35, 3] := '49.31% ';
      stability[35, 4] := '2.4 h m á- ';
      stability[35, 5] := '2.9 m á- ';
      stability[36, 1] := '75 h á+,ec ';{deformed}
      stability[36, 2] := '35.0 h á+,ec ';
      stability[36, 3] := '2.1x10^5 y ec ';{??}
      stability[36, 4] := '11.5% ';
      stability[36, 5] := '10.7 y á- ';
      stability[36, 6] := '76 m á- ';
      stability[37, 1] := '4.6 h á+,ec ';{*************}
      stability[37, 2] := '86.2 d ec ';
      stability[37, 3] := '72.17% ';
      stability[37, 4] := '4.8x10^10 y á- ';
      stability[38, 1] := '7.0% ';
      stability[38, 2] := '50.5 d á- ';
      stability[38, 3] := '9.5 h á- ';
      stability[39, 1] := '100% ';
      stability[39, 2] := '58.5 d á- ';
      stability[40, 1] := '58.4 h á+,ec ';
      stability[40, 2] := '11.2% ';
      stability[40, 3] := '1.5x10^6 y á- ';
      stability[40, 4] := '64.0 d á- ';
      stability[40, 5] := '16.9 h á- ';
      stability[41, 1] := '100% ';
      stability[41, 2] := '35.0 d á- ';
      stability[42, 1] := '13.6 y ec ';
      stability[42, 2] := '15.9% ';
      stability[42, 3] := '9.6% ';
      stability[42, 4] := '66.0 h á- ';
      stability[43, 1] := '2.1x10^5 y á- ';
      stability[44, 1] := '2.88 d ec ';
      stability[44, 2] := '12.7% ';
      stability[44, 3] := '17.0% ';
      stability[44, 4] := '39.4 d á- ';
      stability[45, 1] := '100% ';
      stability[46, 1] := '8.5 h á+,ec ';
      stability[46, 2] := '17.0 d ec ';
      stability[46, 3] := '22.2% ';
      stability[47, 1] := '1.1 h á+,ec ';{??}
      stability[47, 2] := '41.3 d á+,ec ';
      stability[47, 3] := '51.83% ';
      stability[47, 4] := '48.17% ';
      stability[47, 5] := '48.6 m á- ';
      stability[48, 1] := '56.0 m á+,ec ';
      stability[48, 2] := '6.5 h á+,ec ';
      stability[48, 3] := '453 d,ec ';
      stability[48, 4] := '12.8% ';
      stability[48, 5] := '9x10^15 y á- ';
      stability[48, 6] := '53.4 h á- ';
      stability[49, 1] := '4.2 h á+,ec ';
      stability[49, 2] := '2.8 d ec ';
      stability[49, 3] := '4.3% ';
      stability[49, 4] := '5.1x10^14 y á- ';
      stability[49, 5] := '42 m á- ';
      stability[50, 1] := '115.1 d EC ';
      stability[50, 2] := '0.38% ';
      stability[50, 3] := '7.75% ';
      stability[50, 4] := '8.6% ';
      stability[50, 5] := '27.1 h  á-';
      stability[51, 1] := '31.8 m ';{EC67%,á+33% }
      stability[51, 2] := '2.8 h ';{EC 97.5%, á+ 2.5% }
      stability[51, 3] := '38 h EC ';
      stability[51, 4] := '57.3% ';
      stability[51, 5] := '42.7% ';
      stability[51, 6] := '2.7 y á- ';
{******************************}
      stability[52, 1] := '5/2+ ';
      stability[52, 2] := '5/2+ ';
      stability[52, 3] := '5/2+ ';
      stability[52, 4] := '1/2+ ';
      stability[52, 5] := '1/2+ ';
      stability[52, 6] := '1/2+ ';
      stability[52, 7] := '1/2+ ';
      stability[53, 1] := '5/2+ ';
      stability[53, 2] := '5/2+ ';
      stability[53, 3] := '5/2+ ';
      stability[53, 4] := '1/2+ ';
      stability[53, 5] := '1/2+ ';
      stability[53, 6] := '1/2+ ';
      stability[53, 7] := '1/2+ ';
      stability[54, 1] := '5/2+ ';
      stability[54, 2] := '5/2+ ';
      stability[54, 3] := '5/2+ ';
      stability[54, 4] := '1/2+ ';
      stability[55, 1] := '5/2+ ';
      stability[55, 2] := '5/2+ ';
      stability[55, 3] := '5/2+ ';
      stability[55, 4] := '1/2+ ';
      stability[55, 5] := '1/2+ ';
      stability[55, 6] := '1/2+ ';
      stability[55, 7] := '1/2+ ';
      stability[56, 1] := '5/2+ ';
      stability[56, 2] := '5/2+ ';
      stability[56, 3] := '5/2+ ';
      stability[56, 4] := '1/2+ ';
      stability[57, 1] := '5/2+ ';
      stability[58, 1] := '5/2+ ';
      stability[58, 2] := '5/2+ ';
      stability[58, 3] := '5/2+ ';
      stability[58, 4] := '1/2+ ';
      stability[59, 1] := '5/2+ ';
      stability[59, 2] := '5/2+ ';
      stability[60, 1] := '5/2+ ';
      stability[60, 2] := '5/2+ ';
      stability[60, 3] := '5/2+ ';
      stability[60, 4] := '1/2+ ';
      stability[60, 5] := '1/2+ ';
      stability[61, 1] := '5/2+ ';
      stability[61, 2] := '5/2+ ';
      stability[61, 3] := '7/2+ ';
      stability[61, 4] := '7/2+ ';
      stability[62, 1] := '3/2+ ';
      stability[62, 2] := '7/2- ';
      stability[62, 3] := '7/2- ';
      stability[62, 4] := '7/2- ';
      stability[63, 1] := ' ';
      stability[64, 1] := '3/2- ';
      stability[65, 1] := ' ';
      stability[66, 1] := '3/2- [521]3/2 ';
      stability[67, 1] := ' ';
      stability[68, 1] := ' ';
      stability[69, 1] := ' ';
      stability[70, 1] := ' ';
      stability[71, 1] := ' ';
      stability[72, 1] := ' ';
      stability[73, 1] := ' ';
      stability[74, 1] := ' ';
      stability[75, 1] := ' ';
      stability[76, 1] := ' ';
      stability[77, 1] := ' ';
      stability[78, 1] := '1/2- ';
      stability[78, 2] := '1/2- ';
      stability[78, 3] := '(5/2-) ';
      stability[79, 1] := '3/2+ ';
      stability[79, 2] := '3/2+ ';
      stability[79, 3] := '3/2+ ';
      stability[79, 4] := '3/2+ ';
      stability[79, 5] := '3/2+ ';
      stability[80, 1] := '1/2- ';
      stability[80, 2] := '1/2- ';
      stability[80, 3] := '3/2- ';
      stability[80, 4] := '5/2- ';
      stability[80, 5] := '1/2- ';
      stability[81, 1] := '1/2+ ';
      stability[81, 2] := '1/2+ ';
      stability[81, 3] := '1/2+ ';
      stability[81, 4] := '1/2+ ';
      stability[81, 5] := '1/2+ ';
      stability[81, 6] := '1/2+ ';
      stability[82, 1] := '5/2- ';
      stability[82, 2] := '5/2- ';
      stability[82, 3] := '5/2- ';
      stability[82, 4] := '5/2- ';
      stability[82, 5] := '1/2- ';
      stability[82, 6] := '9/2+ ';
      stability[83, 1] := '9/2- ';
      stability[83, 2] := '9/2- ';
      stability[83, 3] := '9/2- ';
      stability[83, 4] := '9/2- ';
      stability[83, 5] := '9/2- ';
      stability[84, 1] := '3/2(-) ';
      stability[84, 2] := '5/2- ';
      stability[84, 3] := '5/2- ';
      stability[84, 4] := '5/2- ';
      stability[84, 5] := '1/2- ';
      stability[85, 1] := '9/2- ';

      FillStability := stability[prot, offset];
   End;{FillStability}

   Procedure GetIsotopeData; {(SMNucData:SMNucleiType; var errorIsotopeData:boolean;var messageStr:messageArray)}
      Var
         i, Arange, FirstA, NumOfIsotopes, offset: integer;
         ArangeReal, FirstAReal: real;
         FirstAStr, ArangeStr: Str20;
   Begin
      With SMNucData Do
         Begin
            errorIsotopeData := false;
            If prot > 85 Then
               Begin
                  errorIsotopeData := true;
                  messageStr[1] := 'Nonspherical nuclei for Z>86';
                  messageStr[2] := '';
                  exit;
               End;{if Z>85}
            FirstA := FillFirstA(prot);
            FirstAReal := FirstA * 1.0;
            NumOfIsotopes := FillNumOfIsotopes(prot);
            Arange := FirstA + 2 * (NumOfIsotopes - 1);
            ArangeReal := Arange * 1.0;
            If (A < FirstA) Or (A > Arange) Then
               Begin
                  errorIsotopeData := true;
                  FirstAStr := NumStr(FirstAReal, 3, 0);
                  ArangeStr := NumStr(ArangeReal, 3, 0);
                  messageStr[1] := 'Allowed A range: ';
                  messageStr[2] := concat(FirstAStr, ' - ', ArangeStr);
                  exit;
               End;
            offset := round((A - FirstA) / 2) + 1;
            messageStr[1] := '';
            messageStr[2] := '';
            With element Do
               Begin
                  elementStr := FillelementStr(prot);
                  Stability := FillStability(prot, offset);
                  spinParity[1] := FillspinParitygs(prot, offset);
                  spinParity[2] := FillspinParityfirst(prot, offset);
                  spinParity[3] := FillspinParitysecond(prot, offset);
               End;{with element}
         End;{with SMNucData}
   End;{GetIsotopeData}

{{$S Levels}

   Procedure GetNeutronLevels; {(var minE, maxE: real; var SMNucData:SMNucleiType)}

      Var
         i, j, index, index1n, index2n: integer;
         sum: longint;
         Nlevels: levelType;
   Begin
      Nlevels.degeneracy[1] := 2;
      Nlevels.degeneracy[2] := 4;
      Nlevels.degeneracy[3] := 2;
      Nlevels.degeneracy[4] := 6;
      Nlevels.degeneracy[5] := 2;
      Nlevels.degeneracy[6] := 4;
      Nlevels.degeneracy[7] := 8;
      Nlevels.degeneracy[8] := 4;
      Nlevels.degeneracy[9] := 6;
      Nlevels.degeneracy[10] := 2;
      Nlevels.degeneracy[11] := 10;
      Nlevels.degeneracy[12] := 6;
      Nlevels.degeneracy[13] := 8;
      Nlevels.degeneracy[14] := 12;
      Nlevels.degeneracy[15] := 4;
      Nlevels.degeneracy[16] := 2;
      Nlevels.degeneracy[17] := 8;
      Nlevels.degeneracy[18] := 10;
      Nlevels.degeneracy[19] := 6;
      Nlevels.degeneracy[20] := 4;
      Nlevels.degeneracy[21] := 14;
      Nlevels.degeneracy[22] := 2;
      Nlevels.degeneracy[23] := 10;
      Nlevels.degeneracy[24] := 12;
      Nlevels.degeneracy[25] := 6;
      Nlevels.degeneracy[26] := 8;
      Nlevels.degeneracy[27] := 4;
      sum := 0;
      For i := 1 To 27 Do
         Begin
            sum := sum + Nlevels.degeneracy[i];
            Nlevels.sumdegen[i] := sum;
         End;
      Nlevels.energy[1] := 0.00;
      Nlevels.energy[2] := 1.60;
      Nlevels.energy[3] := 1.82;
      Nlevels.energy[4] := 3.17;
      Nlevels.energy[5] := 3.35;
      Nlevels.energy[6] := 3.80;
      Nlevels.energy[7] := 4.69;
      Nlevels.energy[8] := 5.53;
      Nlevels.energy[9] := 5.70;
      Nlevels.energy[10] := 5.79;
      Nlevels.energy[11] := 6.00;
      Nlevels.energy[12] := 7.28;
      Nlevels.energy[13] := 7.40;
      Nlevels.energy[14] := 7.89;
      Nlevels.energy[15] := 7.98;
      Nlevels.energy[16] := 8.07;
      Nlevels.energy[17] := 9.64;
      Nlevels.energy[18] := 9.72;
      Nlevels.energy[19] := 10.54;
      Nlevels.energy[20] := 10.80;
      Nlevels.energy[21] := 10.90;
      Nlevels.energy[22] := 10.99;
      Nlevels.energy[23] := 12.38;
      Nlevels.energy[24] := 12.98;
      Nlevels.energy[25] := 13.60;
      Nlevels.energy[26] := 13.79;
      Nlevels.energy[27] := 14.22;
      Nlevels.levelLabel[1] := '1s1/2';
      Nlevels.levelLabel[2] := '1p3/2';
      Nlevels.levelLabel[3] := '1p1/2';
      Nlevels.levelLabel[4] := '1d5/2';
      Nlevels.levelLabel[5] := '2s1/2';
      Nlevels.levelLabel[6] := '1d3/2';
      Nlevels.levelLabel[7] := '1f7/2';
      Nlevels.levelLabel[8] := '2p3/2';
      Nlevels.levelLabel[9] := '1f5/2';
      Nlevels.levelLabel[10] := '2p1/2';
      Nlevels.levelLabel[11] := '1g9/2';
      Nlevels.levelLabel[12] := '2d5/2';
      Nlevels.levelLabel[13] := '1g7/2';
      Nlevels.levelLabel[14] := '1h11/2';
      Nlevels.levelLabel[15] := '2d3/2';
      Nlevels.levelLabel[16] := '3s1/2';
      Nlevels.levelLabel[17] := '2f7/2';
      Nlevels.levelLabel[18] := '1h9/2';
      Nlevels.levelLabel[19] := '2f5/2';
      Nlevels.levelLabel[20] := '3p3/2';
      Nlevels.levelLabel[21] := '1i13/2';
      Nlevels.levelLabel[22] := '3p1/2';
      Nlevels.levelLabel[23] := '2g9/2';
      Nlevels.levelLabel[24] := '1i11/2';
      Nlevels.levelLabel[25] := '3d5/2';
      Nlevels.levelLabel[26] := '2g7/2';
      Nlevels.levelLabel[27] := '3d3/2';
      Nlevels.spinParity[1] := '1/2+';
      Nlevels.spinParity[2] := '3/2-';
      Nlevels.spinParity[3] := '1/2-';
      Nlevels.spinParity[4] := '5/2+';
      Nlevels.spinParity[5] := '1/2+';
      Nlevels.spinParity[6] := '3/2+';
      Nlevels.spinParity[7] := '7/2-';
      Nlevels.spinParity[8] := '3/2-';
      Nlevels.spinParity[9] := '5/2-';
      Nlevels.spinParity[10] := '1/2-';
      Nlevels.spinParity[11] := '9/2+';
      Nlevels.spinParity[12] := '5/2+';
      Nlevels.spinParity[13] := '7/2+';
      Nlevels.spinParity[14] := '11/2-';
      Nlevels.spinParity[15] := '3/2+';
      Nlevels.spinParity[16] := '1/2+';
      Nlevels.spinParity[17] := '7/2-';
      Nlevels.spinParity[18] := '9/2-';
      Nlevels.spinParity[19] := '5/2-';
      Nlevels.spinParity[20] := '3/2-';
      Nlevels.spinParity[21] := '13/2+';
      Nlevels.spinParity[22] := '1/2-';
      Nlevels.spinParity[23] := '9/2+';
      Nlevels.spinParity[24] := '11/2+';
      Nlevels.spinParity[25] := '5/2+';
      Nlevels.spinParity[26] := '7/2+';
      Nlevels.spinParity[27] := '3/2+';
      With SMNucData Do
         Begin
            i := 1;
            index := 1;
            While (i < 28) And (Nlevels.sumdegen[i] < neut) Do
               Begin
                  index := i;
                  inc(i);
               End;
            index1n := index - 2;
{            write(index, index1n); }
            If index1n < 1 Then
               index1n := 1;
            index2n := index + 4;
{writeln(index2n);}
            If index2n > 27 Then
               index2n := 27;
            j := 0;
            For i := index1n To index2n Do
               Begin
                  j := j + 1;
                  subNlevels.degeneracy[j] := Nlevels.degeneracy[i];
                  subNlevels.sumdegen[j] := Nlevels.sumdegen[i];
                  subNlevels.energy[j] := Nlevels.energy[i];
                  subNlevels.levelLabel[j] := Nlevels.levelLabel[i];
                  subNlevels.spinParity[j] := Nlevels.spinParity[i];
               End;
            minE := Nlevels.energy[index1n];
            maxE := Nlevels.energy[index2n];
            lengthn := j;
{writeln(minE, maxE);}
{    writeln('lengthn,energyn(lengthn)', lengthn, subNlevels.energy[lengthn]);}
         End;{with SMNucData}
   End;{GetNeutronLevels}

   Procedure GetProtonLevels; {(var minE, maxE: real; var SMNucData:SMNucleiType)}
      Var
         i, j, sum, index, index1p, index2p: integer;
         Plevels: levelType;
   Begin
      Plevels.degeneracy[1] := 2;
      Plevels.degeneracy[2] := 4;
      Plevels.degeneracy[3] := 2;
      Plevels.degeneracy[4] := 6;
      Plevels.degeneracy[5] := 2;
      Plevels.degeneracy[6] := 4;
      Plevels.degeneracy[7] := 8;
      Plevels.degeneracy[8] := 4;
      Plevels.degeneracy[9] := 6;
      Plevels.degeneracy[10] := 2;
      Plevels.degeneracy[11] := 10;
      Plevels.degeneracy[12] := 8;
      Plevels.degeneracy[13] := 6;
      Plevels.degeneracy[14] := 12;
      Plevels.degeneracy[15] := 4;
      Plevels.degeneracy[16] := 2;
      Plevels.degeneracy[17] := 10;
      Plevels.degeneracy[18] := 8;
      Plevels.degeneracy[19] := 6;
      Plevels.degeneracy[20] := 14;
      Plevels.degeneracy[21] := 4;
      Plevels.degeneracy[22] := 2;
      Plevels.degeneracy[23] := 12;
      sum := 0;
      For i := 1 To 23 Do
         Begin
            sum := sum + Plevels.degeneracy[i];
            Plevels.sumdegen[i] := sum;
         End;
      Plevels.energy[1] := 0.00;
      Plevels.energy[2] := 1.62;
      Plevels.energy[3] := 1.82;
      Plevels.energy[4] := 3.21;
      Plevels.energy[5] := 3.38;
      Plevels.energy[6] := 3.80;
      Plevels.energy[7] := 4.69;
      Plevels.energy[8] := 5.60;
      Plevels.energy[9] := 5.70;
      Plevels.energy[10] := 5.84;
      Plevels.energy[11] := 5.93;
      Plevels.energy[12] := 7.30;
      Plevels.energy[13] := 7.45;
      Plevels.energy[14] := 7.78;
      Plevels.energy[15] := 7.99;
      Plevels.energy[16] := 8.09;
      Plevels.energy[17] := 9.60;
      Plevels.energy[18] := 9.69;
      Plevels.energy[19] := 10.58;
      Plevels.energy[20] := 10.68;
      Plevels.energy[21] := 10.80;
      Plevels.energy[22] := 11.00;
      Plevels.energy[23] := 12.70;
      Plevels.levelLabel[1] := '1s1/2';
      Plevels.levelLabel[2] := '1p3/2';
      Plevels.levelLabel[3] := '1p1/2';
      Plevels.levelLabel[4] := '1d5/2';
      Plevels.levelLabel[5] := '2s1/2';
      Plevels.levelLabel[6] := '1d3/2';
      Plevels.levelLabel[7] := '1f7/2';
      Plevels.levelLabel[8] := '2p3/2';
      Plevels.levelLabel[9] := '1f5/2';
      Plevels.levelLabel[10] := '2p1/2';
      Plevels.levelLabel[11] := '1g9/2';
      Plevels.levelLabel[12] := '1g7/2';
      Plevels.levelLabel[13] := '2d5/2';
      Plevels.levelLabel[14] := '1h11/2';
      Plevels.levelLabel[15] := '2d3/2';
      Plevels.levelLabel[16] := '3s1/2';
      Plevels.levelLabel[17] := '1h9/2';
      Plevels.levelLabel[18] := '2f7/2';
      Plevels.levelLabel[19] := '2f5/2';
      Plevels.levelLabel[20] := '1i13/2';
      Plevels.levelLabel[21] := '3p3/2';
      Plevels.levelLabel[22] := '3p1/2';
      Plevels.levelLabel[23] := '1i11/2';
      Plevels.spinParity[1] := '1/2+';
      Plevels.spinParity[2] := '3/2-';
      Plevels.spinParity[3] := '1/2-';
      Plevels.spinParity[4] := '5/2+';
      Plevels.spinParity[5] := '1/2+';
      Plevels.spinParity[6] := '3/2+';
      Plevels.spinParity[7] := '7/2-';
      Plevels.spinParity[8] := '3/2-';
      Plevels.spinParity[9] := '5/2-';
      Plevels.spinParity[10] := '1/2-';
      Plevels.spinParity[11] := '9/2+';
      Plevels.spinParity[12] := '7/2+';
      Plevels.spinParity[13] := '5/2+';
      Plevels.spinParity[14] := '11/2-';
      Plevels.spinParity[15] := '3/2+';
      Plevels.spinParity[16] := '1/2+';
      Plevels.spinParity[17] := '9/2-';
      Plevels.spinParity[18] := '7/2-';
      Plevels.spinParity[19] := '5/2-';
      Plevels.spinParity[20] := '13/2+';
      Plevels.spinParity[21] := '3/2-';
      Plevels.spinParity[22] := '1/2-';
      Plevels.spinParity[23] := '11/2+';
      With SMNucData Do
         Begin
            i := 1;
            index := 1;
            While (i < 23) And (Plevels.sumdegen[i] < prot) Do
               Begin
                  index := i;
                  inc(i);
               End;
            index1p := index - 2;
 {           write(index, index1p);  }
            If index1p < 1 Then
               index1p := 1;
            index2p := index + 4;
{    writeln(index2p);}
            If index2p > 23 Then
               index2p := 23;
            j := 0;
            For i := index1p To index2p Do
               Begin
                  j := j + 1;
                  subPlevels.degeneracy[j] := Plevels.degeneracy[i];
                  subPlevels.sumdegen[j] := Plevels.sumdegen[i];
                  subPlevels.energy[j] := Plevels.energy[i];
                  subPlevels.levelLabel[j] := Plevels.levelLabel[i];
                  subPlevels.spinParity[j] := Plevels.spinParity[i];
               End;
            minE := Plevels.energy[index1p];
            maxE := Plevels.energy[index2p];
{    writeln(mine, maxe);}
            lengthp := j;
         End;{with SMNucData}
   End;{GetProtonLevels}

   Procedure GetMNlevels; {(minEp, maxEp, minEn, maxEn: real; var SMNucData:SMNucleiType)}
      Var
         i, j, indexa, indexb: integer;
         MNlevels: Array[1..7] Of real;
         MN: Array[1..7] Of Str20;
   Begin
      MNlevels[1] := 0.80;
      MNlevels[2] := 2.50;
      MNlevels[3] := 4.30;
      MNlevels[4] := 5.10;
      MNlevels[5] := 6.60;
      MNlevels[6] := 8.85;
      MNlevels[7] := 11.80;
      MN[1] := '2';
      MN[2] := '8';
      MN[3] := '20';
      MN[4] := '28';
      MN[5] := '50';
      MN[6] := '82';
      MN[7] := '126';
      With SMNucData Do
         Begin
            i := 1;
            indexa := 1;
            While (i <= 7) And (MNlevels[i] <= minEp) Do Begin
               indexa := i;
               inc(i);
             End;
            i := 1;
            While (i <= 7) And (MNlevels[i] <= maxEp) Do
               inc(i);
            If (i = 7) Then
               indexb := 7
             Else
               indexb := i - 1;
            If indexb < indexa then
               indexb := indexa;
            j := 0;
            For i := indexa To indexb Do
               Begin
                  j := j + 1;
                  subMNlevels[1, j] := MNlevels[i];
                  subMN[1, j] := MN[i];
               End;
            lengthMNp := j;
            i := 1;
            While (i <= 7) And (MNlevels[i] <= minEn) Do Begin
               indexa := i;
               inc(i);
             End;
            i := 1;
            While (i <= 7) And (MNlevels[i] <= maxEn) Do
               inc(i);
            If (i = 7) Then
               indexb := 7
             Else
               indexb := i - 1;
            j := 0;
            For i := indexa To indexb Do
               Begin
                  j := j + 1;
                  subMNlevels[2, j] := MNlevels[i];
                  subMN[2, j] := MN[i];
               End;
            lengthMNn := j;
         End;{with SMNucData}
   End;{GetMNlevels}


End.{unit IsotopeData}