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

Unit NucInter;

Interface
   Uses
      Cups, Cupsmupp, CupsProc;
   Const
      maxcollisions = 2000;
   Type
      Str25 = String[25];
      PhotoConstList = Array[1..4] Of extended;
      Array59 = Array[1..2, 1..59] Of real;
      vector59 = Array[1..59] Of real;
      MaterialType = Record
            a, b, c, p: PhotoConstList;
            singlematerialname: Str25;
            Z: array[1..3] of integer;
            Zeff: real;
            N: array[1..3] of real;
            Neff: real;
            M: array[1..3] of real;
            Meff: real;
            sigmax: array[1..3] of real;
            Iave: array[1..3] of real;
            IaveEff: real;
            density: array[1..3] of real;
            densityeff: real;
            Lrad: array[1..3] of real;
            d1: array[1..3] of real;
            f: array[1..3, 1..4] of real;
            h: array[1..3, 1..4] of real;
            w: array[1..3] of real;
            EnergyPairArray: Array59;
            Photoarray: array[1..59] of real;
            ComptonArray: array[1..59] of real;
         End;{MaterialType}
         MyPointType = record
            x: real;
            y: real;
            z: real;
          end;
         PointVectorType = array[1..maxcollisions] of MyPointType;
         sourceType = (photon, electron, proton, muon, multiple, pseudo);
         materialsType = (air, plastic, aluminum, silcon, lead, NaImat);
         photonInteractionType = (compton, photoelectric, pairProduction, total);
         Str80 = string[80];
         RadType = record
            source: array[1..5] of sourceType;
            sourcename: array[1..5] of Str25;
            sourceStr: Str25;
            multisources: integer;
            energy: real;
            energyStr: Str25;
            material: array[1..4] of materialsType;
            materialname: array[1..4] of Str25;
            materialStr: Str25;
            multimaterials: integer;
            thickness: real;
            thicknessStr: Str25;
            range: boolean;
            MonteCarlo: integer;
            MCtotalStr: Str25;
            MCtogoStr: Str25;
         end;{RadType}
      NaIType = record
            Esource: real;
            energyStr: Str25;
            thickness: real;
            thicknessStr: Str25;
            Detdia: real;
            DetdiaStr: Str25;
            DetRadius: real;
            detsourceDist: real;
            detsourceDistStr: Str25;
            MonteCarlo: integer;
            MCtotalStr: Str25;
            MCtogoStr: Str25;
         end;{NaIType}

   Procedure GetConstantArrays (Var thisMaterial: MaterialType);
   Procedure GetAngleDistr(var AngleDistr: Dvector);
   Procedure GetPtvec(i:integer; var lastcollision:integer;var Ptvec: PointVectorType);
   Procedure GetTransDistr(var TransDistr: dVector);
   procedure ChooseMaterial(num: integer; var Rad: RadType; var thisMaterial: MaterialType);

Implementation

   Procedure GetConstantArrays; {(var thisMaterial:MaterialType)}
      Var
         i, index: integer;
         CompositeArray: Array[1..6, 1..59] Of real;{An array of acceptable energy values}
{                                                                           and pair cross sections for Pb & Al & NaI}
         NaIArray: Array[1..3, 1..59] Of real;
   Begin
      With thisMaterial Do Begin
            a[1] := 1.6268e-9;{a,b,c,p are constants used to calculate the photoelectric cross section}
            a[2] := 1.5274e-9;
            a[3] := 1.1330e-9;
            a[4] := -9.12e-11;
            b[1] := -2.683e-12;
            b[2] := -5.110e-13;
            b[3] := -2.177e-12;
            b[4] := 0.0;
            c[1] := 4.173e-2;
            c[2] := 1.027e-2;
            c[3] := 2.013e-2;
            c[4] := 0.0;
            p[1] := 1;
            p[2] := 2;
            p[3] := 3.5;
            p[4] := 4;
            CompositeArray[1, 1] := 0.010;{MeV}
            CompositeArray[1, 2] := 0.015;
            CompositeArray[1, 3] := 0.020;
            CompositeArray[1, 4] := 0.030;
            CompositeArray[1, 5] := 0.03319;
            CompositeArray[1, 6] := 0.0332;
            CompositeArray[1, 7] := 0.040;
            CompositeArray[1, 8] := 0.050;
            CompositeArray[1, 9] := 0.060;
            CompositeArray[1, 10] := 0.080;
            CompositeArray[1, 11] := 0.100;
            CompositeArray[1, 12] := 0.150;
            CompositeArray[1, 13] := 0.200;
            CompositeArray[1, 14] := 0.300;
            CompositeArray[1, 15] := 0.4;
            CompositeArray[1, 16] := 0.5;
            CompositeArray[1, 17] := 0.6;
            CompositeArray[1, 18] := 0.8;
            CompositeArray[1, 19] := 1.0;
            CompositeArray[1, 20] := 1.5;
            CompositeArray[1, 21] := 2.0;
            CompositeArray[1, 22] := 3.0;
            CompositeArray[1, 23] := 4.0;
            CompositeArray[1, 24] := 5.0;
            CompositeArray[1, 25] := 6.0;
            CompositeArray[1, 26] := 8.0;
            CompositeArray[1, 27] := 10.0;
            CompositeArray[1, 28] := 15.0;
            CompositeArray[1, 29] := 20.0;
            CompositeArray[1, 30] := 30.0;
            CompositeArray[1, 31] := 40.0;
            CompositeArray[1, 32] := 50.0;
            CompositeArray[1, 33] := 60.0;
            CompositeArray[1, 34] := 80.0;
            CompositeArray[1, 35] := 100.0;
            CompositeArray[1, 36] := 150.0;
            CompositeArray[1, 37] := 200.0;
            CompositeArray[1, 38] := 300.0;
            CompositeArray[1, 39] := 400.0;
            CompositeArray[1, 40] := 500.0;
            CompositeArray[1, 41] := 600.0;
            CompositeArray[1, 42] := 800.0;
            CompositeArray[1, 43] := 1000.0;
            CompositeArray[1, 44] := 1500.0;
            CompositeArray[1, 45] := 2000.0;
            CompositeArray[1, 46] := 3000.0;
            CompositeArray[1, 47] := 4000.0;
            CompositeArray[1, 48] := 5000.0;
            CompositeArray[1, 49] := 6000.0;
            CompositeArray[1, 50] := 8000.0;
            CompositeArray[1, 51] := 10000.0;
            CompositeArray[1, 52] := 15000.0;
            CompositeArray[1, 53] := 20000.0;
            CompositeArray[1, 54] := 30000.0;
            CompositeArray[1, 55] := 40000.0;
            CompositeArray[1, 56] := 50000.0;
            CompositeArray[1, 57] := 60000.0;
            CompositeArray[1, 58] := 80000.0;
            CompositeArray[1, 59] := 100000.0;{100 GeV}
            CompositeArray[2, 1] := 0;{ pair cross section for Air in barns}
            CompositeArray[2, 2] := 0;
            CompositeArray[2, 3] := 0;
            CompositeArray[2, 4] := 0;
            CompositeArray[2, 5] := 0;
            CompositeArray[2, 6] := 0;
            CompositeArray[2, 7] := 0;
            CompositeArray[2, 8] := 0;
            CompositeArray[2, 9] := 0;
            CompositeArray[2, 10] := 0;
            CompositeArray[2, 11] := 0;
            CompositeArray[2, 12] := 0;
            CompositeArray[2, 13] := 0;
            CompositeArray[2, 14] := 0;
            CompositeArray[2, 15] := 0;
            CompositeArray[2, 16] := 0;
            CompositeArray[2, 17] := 0;
            CompositeArray[2, 18] := 0;
            CompositeArray[2, 19] := 0;
            CompositeArray[2, 20] := 0.0476;
            CompositeArray[2, 21] := 0.1896;
            CompositeArray[2, 22] := 0.5496;
            CompositeArray[2, 23] := 0.9046;
            CompositeArray[2, 24] := 1.2308;
            CompositeArray[2, 25] := 1.5207;
            CompositeArray[2, 26] := 2.0197;
            CompositeArray[2, 27] := 2.4307;
            CompositeArray[2, 28] := 3.2180;
            CompositeArray[2, 29] := 3.7931;
            CompositeArray[2, 30] := 4.6126;
            CompositeArray[2, 31] := 5.1877;
            CompositeArray[2, 32] := 5.6015;
            CompositeArray[2, 33] := 5.9962;
            CompositeArray[2, 34] := 6.5111;
            CompositeArray[2, 35] := 6.8576;
            CompositeArray[2, 36] := 7.5409;
            CompositeArray[2, 37] := 7.9500;
            CompositeArray[2, 38] := 8.4312;
            CompositeArray[2, 39] := 8.7295;
            CompositeArray[2, 40] := 8.9172;
            CompositeArray[2, 41] := 9.0472;
            CompositeArray[2, 42] := 9.2878;
            CompositeArray[2, 43] := 9.4177;
            CompositeArray[2, 44] := 9.6198;
            CompositeArray[2, 45] := 9.7016;
            CompositeArray[2, 46] := 9.8412;
            CompositeArray[2, 47] := 9.9086;
            CompositeArray[2, 48] := 9.9711;
            CompositeArray[2, 49] := 9.9856;
            CompositeArray[2, 50] := 10.048;
            CompositeArray[2, 51] := 10.058;
            CompositeArray[2, 52] := 10.067;
            CompositeArray[2, 53] := 10.125;
            CompositeArray[2, 54] := 10.130;
            CompositeArray[2, 55] := 10.135;
            CompositeArray[2, 56] := 10.140;
            CompositeArray[2, 57] := 10.140;
            CompositeArray[2, 58] := 10.144;
            CompositeArray[2, 59] := 10.144;
            CompositeArray[3, 1] := 0;{ pair cross section for Plastic (CH2)x in barns}
            CompositeArray[3, 2] := 0;
            CompositeArray[3, 3] := 0;
            CompositeArray[3, 4] := 0;
            CompositeArray[3, 5] := 0;
            CompositeArray[3, 6] := 0;
            CompositeArray[3, 7] := 0;
            CompositeArray[3, 8] := 0;
            CompositeArray[3, 9] := 0;
            CompositeArray[3, 10] := 0;
            CompositeArray[3, 11] := 0;
            CompositeArray[3, 12] := 0;
            CompositeArray[3, 13] := 0;
            CompositeArray[3, 14] := 0;
            CompositeArray[3, 15] := 0;
            CompositeArray[3, 16] := 0;
            CompositeArray[3, 17] := 0;
            CompositeArray[3, 18] := 0;
            CompositeArray[3, 19] := 0;
            CompositeArray[3, 20] := 0.0339;
            CompositeArray[3, 21] := 0.1355;
            CompositeArray[3, 22] := 0.3953;
            CompositeArray[3, 23] := 0.6552;
            CompositeArray[3, 24] := 0.8999;
            CompositeArray[3, 25] := 1.1188;
            CompositeArray[3, 26] := 1.4970;
            CompositeArray[3, 27] := 1.8081;
            CompositeArray[3, 28] := 2.4108;
            CompositeArray[3, 29] := 2.8565;
            CompositeArray[3, 30] := 3.4886;
            CompositeArray[3, 31] := 3.9404;
            CompositeArray[3, 32] := 4.2757;
            CompositeArray[3, 33] := 4.5505;
            CompositeArray[3, 34] := 4.9651;
            CompositeArray[3, 35] := 5.2725;
            CompositeArray[3, 36] := 5.8081;
            CompositeArray[3, 37] := 6.1202;
            CompositeArray[3, 38] := 6.5580;
            CompositeArray[3, 39] := 6.8142;
            CompositeArray[3, 40] := 6.9632;
            CompositeArray[3, 41] := 7.0936;
            CompositeArray[3, 42] := 7.2892;
            CompositeArray[3, 43] := 7.3777;
            CompositeArray[3, 44] := 7.5827;
            CompositeArray[3, 45] := 7.6665;
            CompositeArray[3, 46] := 7.8062;
            CompositeArray[3, 47] := 7.8314;
            CompositeArray[3, 48] := 7.8994;
            CompositeArray[3, 49] := 7.9134;
            CompositeArray[3, 50] := 7.9739;
            CompositeArray[3, 51] := 7.98;
            CompositeArray[3, 52] := 8.00;
            CompositeArray[3, 53] := 8.06;
            CompositeArray[3, 54] := 8.07;
            CompositeArray[3, 55] := 8.07;
            CompositeArray[3, 56] := 8.07;
            CompositeArray[3, 57] := 8.08;
            CompositeArray[3, 58] := 8.08;
            CompositeArray[3, 59] := 8.08;
            CompositeArray[4, 1] := 0;{ pair cross section for Al in barns}
            CompositeArray[4, 2] := 0;
            CompositeArray[4, 3] := 0;
            CompositeArray[4, 4] := 0;
            CompositeArray[4, 5] := 0;
            CompositeArray[4, 6] := 0;
            CompositeArray[4, 7] := 0;
            CompositeArray[4, 8] := 0;
            CompositeArray[4, 9] := 0;
            CompositeArray[4, 10] := 0;
            CompositeArray[4, 11] := 0;
            CompositeArray[4, 12] := 0;
            CompositeArray[4, 13] := 0;
            CompositeArray[4, 14] := 0;
            CompositeArray[4, 15] := 0;
            CompositeArray[4, 16] := 0;
            CompositeArray[4, 17] := 0;
            CompositeArray[4, 18] := 0;
            CompositeArray[4, 19] := 0;
            CompositeArray[4, 20] := 0.00768;
            CompositeArray[4, 21] := 0.0303;
            CompositeArray[4, 22] := 0.0870;
            CompositeArray[4, 23] := 0.1422;
            CompositeArray[4, 24] := 0.1912;
            CompositeArray[4, 25] := 0.2355;
            CompositeArray[4, 26] := 0.3101;
            CompositeArray[4, 27] := 0.3722;
            CompositeArray[4, 28] := 0.4886;
            CompositeArray[4, 29] := 0.5733;
            CompositeArray[4, 30] := 0.6924;
            CompositeArray[4, 31] := 0.7769;
            CompositeArray[4, 32] := 0.8396;
            CompositeArray[4, 33] := 0.8891;
            CompositeArray[4, 34] := 0.9631;
            CompositeArray[4, 35] := 1.0163;
            CompositeArray[4, 36] := 1.099;
            CompositeArray[4, 37] := 1.155;
            CompositeArray[4, 38] := 1.223;
            CompositeArray[4, 39] := 1.258;
            CompositeArray[4, 40] := 1.294;
            CompositeArray[4, 41] := 1.328;
            CompositeArray[4, 42] := 1.341;
            CompositeArray[4, 43] := 1.365;
            CompositeArray[4, 44] := 1.378;
            CompositeArray[4, 45] := 1.391;
            CompositeArray[4, 46] := 1.402;
            CompositeArray[4, 47] := 1.403;
            CompositeArray[4, 48] := 1.404;
            CompositeArray[4, 49] := 1.415;
            CompositeArray[4, 50] := 1.416;
            CompositeArray[4, 51] := 1.417;
            CompositeArray[4, 52] := 1.417;
            CompositeArray[4, 53] := 1.428;
            CompositeArray[4, 54] := 1.428;
            CompositeArray[4, 55] := 1.428;
            CompositeArray[4, 56] := 1.428;
            CompositeArray[4, 57] := 1.428;
            CompositeArray[4, 58] := 1.428;
            CompositeArray[4, 59] := 1.428;
            CompositeArray[5, 1] := 0;{ pair cross section for Si in barns}
            CompositeArray[5, 2] := 0;
            CompositeArray[5, 3] := 0;
            CompositeArray[5, 4] := 0;
            CompositeArray[5, 5] := 0;
            CompositeArray[5, 6] := 0;
            CompositeArray[5, 7] := 0;
            CompositeArray[5, 8] := 0;
            CompositeArray[5, 9] := 0;
            CompositeArray[5, 10] := 0;
            CompositeArray[5, 11] := 0;
            CompositeArray[5, 12] := 0;
            CompositeArray[5, 13] := 0;
            CompositeArray[5, 14] := 0;
            CompositeArray[5, 15] := 0;
            CompositeArray[5, 16] := 0;
            CompositeArray[5, 17] := 0;
            CompositeArray[5, 18] := 0;
            CompositeArray[5, 19] := 0;
            CompositeArray[5, 20] := 0.00894;
            CompositeArray[5, 21] := 0.0352;
            CompositeArray[5, 22] := 0.1006;
            CompositeArray[5, 23] := 0.164;
            CompositeArray[5, 24] := 0.221;
            CompositeArray[5, 25] := 0.274;
            CompositeArray[5, 26] := 0.359;
            CompositeArray[5, 27] := 0.429;
            CompositeArray[5, 28] := 0.564;
            CompositeArray[5, 29] := 0.661;
            CompositeArray[5, 30] := 0.799;
            CompositeArray[5, 31] := 0.895;
            CompositeArray[5, 32] := 0.971;
            CompositeArray[5, 33] := 1.024;
            CompositeArray[5, 34] := 1.11;
            CompositeArray[5, 35] := 1.166;
            CompositeArray[5, 36] := 1.265;
            CompositeArray[5, 37] := 1.332;
            CompositeArray[5, 38] := 1.400;
            CompositeArray[5, 39] := 1.445;
            CompositeArray[5, 40] := 1.469;
            CompositeArray[5, 41] := 1.492;
            CompositeArray[5, 42] := 1.516;
            CompositeArray[5, 43] := 1.539;
            CompositeArray[5, 44] := 1.563;
            CompositeArray[5, 45] := 1.576;
            CompositeArray[5, 46] := 1.599;
            CompositeArray[5, 47] := 1.601;
            CompositeArray[5, 48] := 1.612;
            CompositeArray[5, 49] := 1.613;
            CompositeArray[5, 50] := 1.624;
            CompositeArray[5, 51] := 1.624;
            CompositeArray[5, 52] := 1.625;
            CompositeArray[5, 53] := 1.626;
            CompositeArray[5, 54] := 1.636;
            CompositeArray[5, 55] := 1.637;
            CompositeArray[5, 56] := 1.637;
            CompositeArray[5, 57] := 1.637;
            CompositeArray[5, 58] := 1.637;
            CompositeArray[5, 59] := 1.637;
            CompositeArray[6, 1] := 0;{pair cross section for Pb in barns}
            CompositeArray[6, 2] := 0;
            CompositeArray[6, 3] := 0;
            CompositeArray[6, 4] := 0;
            CompositeArray[6, 5] := 0;
            CompositeArray[6, 6] := 0;
            CompositeArray[6, 7] := 0;
            CompositeArray[6, 8] := 0;
            CompositeArray[6, 9] := 0;
            CompositeArray[6, 10] := 0;
            CompositeArray[6, 11] := 0;
            CompositeArray[6, 12] := 0;
            CompositeArray[6, 13] := 0;
            CompositeArray[6, 14] := 0;
            CompositeArray[6, 15] := 0;
            CompositeArray[6, 16] := 0;
            CompositeArray[6, 17] := 0;
            CompositeArray[6, 18] := 0;
            CompositeArray[6, 19] := 0;
            CompositeArray[6, 20] := 0.566;
            CompositeArray[6, 21] := 1.70;
            CompositeArray[6, 22] := 3.943;
            CompositeArray[6, 23] := 5.784;
            CompositeArray[6, 24] := 7.327;
            CompositeArray[6, 25] := 8.581;
            CompositeArray[6, 26] := 10.57;
            CompositeArray[6, 27] := 12.296;
            CompositeArray[6, 28] := 15.65;
            CompositeArray[6, 29] := 18.29;
            CompositeArray[6, 30] := 22.04;
            CompositeArray[6, 31] := 24.58;
            CompositeArray[6, 32] := 26.41;
            CompositeArray[6, 33] := 27.84;
            CompositeArray[6, 34] := 29.97;
            CompositeArray[6, 35] := 31.40;
            CompositeArray[6, 36] := 33.74;
            CompositeArray[6, 37] := 35.27;
            CompositeArray[6, 38] := 36.90;
            CompositeArray[6, 39] := 37.83;
            CompositeArray[6, 40] := 38.44;
            CompositeArray[6, 41] := 38.85;
            CompositeArray[6, 42] := 39.47;
            CompositeArray[6, 43] := 39.78;
            CompositeArray[6, 44] := 40.39;
            CompositeArray[6, 45] := 40.60;
            CompositeArray[6, 46] := 41.01;
            CompositeArray[6, 47] := 41.11;
            CompositeArray[6, 48] := 41.22;
            CompositeArray[6, 49] := 41.32;
            CompositeArray[6, 50] := 41.42;
            CompositeArray[6, 51] := 41.52;
            CompositeArray[6, 52] := 41.52;
            CompositeArray[6, 53] := 41.63;
            CompositeArray[6, 54] := 41.63;
            CompositeArray[6, 55] := 41.63;
            CompositeArray[6, 56] := 41.73;
            CompositeArray[6, 57] := 41.73;
            CompositeArray[6, 58] := 41.73;
            CompositeArray[6, 59] := 41.73;
            NaIArray[1, 1] := 0;{ pair cross section for NaI in barns}
            NaIArray[1, 2] := 0;
            NaIArray[1, 3] := 0;
            NaIArray[1, 4] := 0;
            NaIArray[1, 5] := 0;
            NaIArray[1, 6] := 0;
            NaIArray[1, 7] := 0;
            NaIArray[1, 8] := 0;
            NaIArray[1, 9] := 0;
            NaIArray[1, 10] := 0;
            NaIArray[1, 11] := 0;
            NaIArray[1, 12] := 0;
            NaIArray[1, 13] := 0;
            NaIArray[1, 14] := 0;
            NaIArray[1, 15] := 0;
            NaIArray[1, 16] := 0;
            NaIArray[1, 17] := 0;
            NaIArray[1, 18] := 0;
            NaIArray[1, 19] := 0;
            NaIArray[1, 20] := 0.1335;{0.1831;}
            NaIArray[1, 21] := 0.4523;{0.6196;}
            NaIArray[1, 22] := 1.1790;{1.5950;}
            NaIArray[1, 23] := 1.8487;{2.4714;}
            NaIArray[1, 24] := 2.4096;{3.2305;}
            NaIArray[1, 25] := 2.8830;{3.8885;}
            NaIArray[1, 26] := 3.6536;{4.9565;}
            NaIArray[1, 27] := 4.3061;{5.8472;}
            NaIArray[1, 28] := 5.5736;{7.5561;}
            NaIArray[1, 29] := 6.4845;{8.8323;}
            NaIArray[1, 30] := 7.8037;{10.595;}
            NaIArray[1, 31] := 8.6487;{11.797;}
            NaIArray[1, 32] := 9.4047;{12.692;}
            NaIArray[1, 33] := 9.8638;{13.409;}
            NaIArray[1, 34] := 10.627;{14.412;}
            NaIArray[1, 35] := 11.1676;{15.131;}
            NaIArray[1, 36] := 16.288;
            NaIArray[1, 37] := 16.984;
            NaIArray[1, 38] := 17.873;
            NaIArray[1, 39] := 18.281;
            NaIArray[1, 40] := 18.567;
            NaIArray[1, 41] := 18.803;
            NaIArray[1, 42] := 19.114;
            NaIArray[1, 43] := 19.274;
            NaIArray[1, 44] := 19.562;
            NaIArray[1, 45] := 19.721;
            NaIArray[1, 46] := 19.878;
            NaIArray[1, 47] := 19.958;
            NaIArray[1, 48] := 20.012;
            NaIArray[1, 49] := 20.065;
            NaIArray[1, 50] := 20.092;
            NaIArray[1, 51] := 20.119;
            NaIArray[1, 52] := 20.172;
            NaIArray[1, 53] := 20.199;
            NaIArray[1, 54] := 20.224;
            NaIArray[1, 55] := 20.226;
            NaIArray[1, 56] := 20.251;
            NaIArray[1, 57] := 20.251;
            NaIArray[1, 58] := 20.276;
            NaIArray[1, 59] := 20.276;
            NaIArray[2, 1] := 22510;{3.38e+4;{ photoelectric cross section for NaI in barns}
            NaIArray[2, 2] := 7064.7;{1.14e+4;}
            NaIArray[2, 3] := 3120.1;{5250.1;}
            NaIArray[2, 4] := 950.99;{1667.1;}
            NaIArray[2, 5] := 702.45;{1251.6;}
            NaIArray[2, 6] := 5567.4;{7539.2;}
            NaIArray[2, 7] := 3332.0;{4677.8;}
            NaIArray[2, 8] := 1828.7;{2562.8;}
            NaIArray[2, 9] := 1110.7;{1562.6;}
            NaIArray[2, 10] := 501.96;{714.11;}
            NaIArray[2, 11] := 267.37;{378.20;}
            NaIArray[2, 12] := 83.933;{118.44;}
            NaIArray[2, 13] := 37.141;{52.003;}
            NaIArray[2, 14] := 11.980;{16.621;}
            NaIArray[2, 15] := 5.3515;{7.7134;}
            NaIArray[2, 16] := 2.8997;{4.4041;}
            NaIArray[2, 17] := 1.8588;{2.8365;}
            NaIArray[2, 18] := 0.9668;{1.4631;}
            NaIArray[2, 19] := 0.6246;{0.9107;}
            NaIArray[2, 20] := 0.3049;{0.4130;}
            NaIArray[2, 21] := 0.1934;{0.2538;}
            NaIArray[2, 22] := 0.1190;{0.1359;}
            NaIArray[2, 23] := 0.0818;{0.0898;}
            NaIArray[2, 24] := 0.0595;{0.0672;}
            NaIArray[2, 25] := 0.0521;{0.0525;}
            NaIArray[2, 26] := 0.0372;{0.0366;}
            NaIArray[2, 27] := 0.0298;{0.0281;}
            NaIArray[2, 28] := 0.0223;{0.0176;}
            NaIArray[2, 29] := 0.0149;{0.0128;}
            NaIArray[2, 30] := 0.00824;
            NaIArray[2, 31] := 0.00610;
            NaIArray[2, 32] := 0.00483;
            NaIArray[2, 33] := 0.00398;
            NaIArray[2, 34] := 0.00296;
            NaIArray[2, 35] := 0.00237;
            NaIArray[2, 36] := 0.0000;
            NaIArray[2, 37] := 0.0000;
            NaIArray[2, 38] := 0.0000;
            NaIArray[2, 39] := 0.0000;
            NaIArray[2, 40] := 0.0000;
            NaIArray[2, 41] := 0.0000;
            NaIArray[2, 42] := 0.0000;
            NaIArray[2, 43] := 0.0000;
            NaIArray[2, 44] := 0.0000;
            NaIArray[2, 45] := 0.0000;
            NaIArray[2, 46] := 0.0000;
            NaIArray[2, 47] := 0.0000;
            NaIArray[2, 48] := 0.0000;
            NaIArray[2, 49] := 0.0000;
            NaIArray[2, 50] := 0.0000;
            NaIArray[2, 51] := 0.0000;
            NaIArray[2, 52] := 0.0000;
            NaIArray[2, 53] := 0.0000;
            NaIArray[2, 54] := 0.0000;
            NaIArray[2, 55] := 0.0000;
            NaIArray[2, 56] := 0.0000;
            NaIArray[2, 57] := 0.0000;
            NaIArray[2, 58] := 0.0000;
            NaIArray[2, 59] := 0.0000;
            NaIArray[3, 1] := 30.368;{629.51;{ Compton cross section for NaI in barns}
            NaIArray[3, 2] := 29.850;{420.50;}
            NaIArray[3, 3] := 29.328;{305.05;}
            NaIArray[3, 4] := 28.294;{186.12;}
            NaIArray[3, 5] := 27.998;{164.22;}
            NaIArray[3, 6] := 27.998;{164.22;}
            NaIArray[3, 7] := 27.405;{131.38;}
            NaIArray[3, 8] := 26.592;{102.02;}
            NaIArray[3, 9] := 25.849;{83.603;}
            NaIArray[3, 10] := 24.518;{61.956;}
            NaIArray[3, 11] := 23.328;{50.261;}
            NaIArray[3, 12] := 21.039;{37.074;}
            NaIArray[3, 13] := 19.261;{30.853;}
            NaIArray[3, 14] := 16.740;{24.857;}
            NaIArray[3, 15] := 15.038;{21.249;}
            NaIArray[3, 16] := 13.713;{19.258;}
            NaIArray[3, 17] := 12.678;{17.716;}
            NaIArray[3, 18] := 11.142;{15.352;}
            NaIArray[3, 19] := 10.017;{13.660;}
            NaIArray[3, 20] := 8.1327;{11.072;}
            NaIArray[3, 21] := 6.9402;{9.4053;}
            NaIArray[3, 22] := 5.4589;{7.3899;}
            NaIArray[3, 23] := 4.5627;{6.1707;}
            NaIArray[3, 24] := 3.9255;{5.3247;}
            NaIArray[3, 25] := 3.4661;{4.7027;}
            NaIArray[3, 26] := 2.8369;{3.8318;}
            NaIArray[3, 27] := 2.4146;{3.2844;}
            NaIArray[3, 28] := 1.7850;{2.4210;}
            NaIArray[3, 29] := 1.4332;{1.9433;}
            NaIArray[3, 30] := 1.0411;{1.4158;}
            NaIArray[3, 31] := 0.8273;{1.1247;}
            NaIArray[3, 32] := 0.6903;{0.9380;}
            NaIArray[3, 33] := 0.5947;{0.8087;}
            NaIArray[3, 34] := 0.4681;{0.6370;}
            NaIArray[3, 35] := 0.3888;{0.5300;}
            NaIArray[3, 36] := 0.3782;
            NaIArray[3, 37] := 0.2961;
            NaIArray[3, 38] := 0.2085;
            NaIArray[3, 39] := 0.1640;
            NaIArray[3, 40] := 0.1359;
            NaIArray[3, 41] := 0.1165;
            NaIArray[3, 42] := 0.0982;
            NaIArray[3, 43] := 0.0746;
            NaIArray[3, 44] := 0.0523;
            NaIArray[3, 45] := 0.0403;
            NaIArray[3, 46] := 0.0281;
            NaIArray[3, 47] := 0.0217;
            NaIArray[3, 48] := 0.0177;
            NaIArray[3, 49] := 0.0150;
            NaIArray[3, 50] := 0.0116;
            NaIArray[3, 51] := 0.00946;
            NaIArray[3, 52] := 0.00654;
            NaIArray[3, 53] := 0.00503;
            NaIArray[3, 54] := 0.00346;
            NaIArray[3, 55] := 0.00266;
            NaIArray[3, 56] := 0.00216;
            NaIArray[3, 57] := 0.00183;
            NaIArray[3, 58] := 0.00136;
            NaIArray[3, 59] := 0.00114;

            If singleMaterialname = 'NaI' Then
               For i := 1 To 59 Do Begin
                     EnergyPairArray[1, i] := CompositeArray[1, i];
                     EnergyPairArray[2, i] := NaIArray[1, i];
                     PhotoArray[i] := NaIArray[2, i];
                     ComptonArray[i] := NaIArray[3, i];
                  End {if NaI}
            Else Begin
                  If singleMaterialname = 'Air' Then
                     index := 2
                  Else If singleMaterialname = 'Plastic' Then
                     index := 3
                  Else If singleMaterialname = 'Aluminum' Then
                     index := 4
                  Else If singleMaterialname = 'Silcon' Then
                     index := 5
                  Else If singleMaterialname = 'Lead' Then
                     index := 6
                  Else If singleMaterialname = 'NaImat' Then
                     index := 6;
                  For i := 1 To 59 Do Begin
                        EnergyPairArray[1, i] := CompositeArray[1, i];
                       if singleMaterialname = 'NaImat' then
                          EnergyPairArray[2, i] := NaIArray[1, i]
                       else
                        EnergyPairArray[2, i] := CompositeArray[index, i];
                     End;{for i}
               End;{else if NaI}
         End;{with thisMaterial}
   End;{GetConstantArrays}

   Procedure GetAngleDistr(var AngleDistr: Dvector);
   begin
      AngleDistr.put(1,98);
      AngleDistr.put(2,115);
      AngleDistr.put(3,113);
      AngleDistr.put(4,104);
      AngleDistr.put(5,103);
      AngleDistr.put(6,97);
      AngleDistr.put(7,85);
      AngleDistr.put(8,93);
      AngleDistr.put(9,90);
      AngleDistr.put(10,84);
      AngleDistr.put(11,100);
      AngleDistr.put(12,91);
      AngleDistr.put(13,101);
      AngleDistr.put(14,98);
      AngleDistr.put(15,69);
      AngleDistr.put(16,88);
      AngleDistr.put(17,79);
      AngleDistr.put(18,66);
      AngleDistr.put(19,86);
      AngleDistr.put(20,72);
      AngleDistr.put(21,78);
      AngleDistr.put(22,65);
      AngleDistr.put(23,55);
      AngleDistr.put(24,69);
      AngleDistr.put(25,74);
      AngleDistr.put(26,55);
      AngleDistr.put(27,70);
      AngleDistr.put(28,68);
      AngleDistr.put(29,50);
      AngleDistr.put(30,48);
      AngleDistr.put(31,55);
      AngleDistr.put(32,46);
      AngleDistr.put(33,54);
      AngleDistr.put(34,51);
      AngleDistr.put(35,49);
      AngleDistr.put(36,48);
      AngleDistr.put(37,34);
      AngleDistr.put(38,41);
      AngleDistr.put(39,42);
      AngleDistr.put(40,46);
      AngleDistr.put(41,46);
      AngleDistr.put(42,44);
      AngleDistr.put(43,38);
      AngleDistr.put(44,39);
      AngleDistr.put(45,40);
      AngleDistr.put(46,32);
      AngleDistr.put(47,42);
      AngleDistr.put(48,34);
      AngleDistr.put(49,42);
      AngleDistr.put(50,41);
      AngleDistr.put(51,45);
      AngleDistr.put(52,46);
      AngleDistr.put(53,43);
      AngleDistr.put(54,42);
      AngleDistr.put(55,39);
      AngleDistr.put(56,48);
      AngleDistr.put(57,39);
      AngleDistr.put(58,44);
      AngleDistr.put(59,34);
      AngleDistr.put(60,51);
      AngleDistr.put(61,39);
      AngleDistr.put(62,62);
      AngleDistr.put(63,50);
      AngleDistr.put(64,40);
      AngleDistr.put(65,40);
      AngleDistr.put(66,39);
      AngleDistr.put(67,54);
      AngleDistr.put(68,61);
      AngleDistr.put(69,57);
      AngleDistr.put(70,43);
      AngleDistr.put(71,43);
      AngleDistr.put(72,58);
      AngleDistr.put(73,52);
      AngleDistr.put(74,42);
      AngleDistr.put(75,42);
      AngleDistr.put(76,66);
      AngleDistr.put(77,54);
      AngleDistr.put(78,54);
      AngleDistr.put(79,62);
      AngleDistr.put(80,69);
      AngleDistr.put(81,41);
      AngleDistr.put(82,67);
      AngleDistr.put(83,45);
      AngleDistr.put(84,71);
      AngleDistr.put(85,54);
      AngleDistr.put(86,66);
      AngleDistr.put(87,83);
      AngleDistr.put(88,61);
      AngleDistr.put(89,51);
      AngleDistr.put(90,51);
   end;{GetAngleDistr}

   Procedure GetPtvec;{(i:integer; var lastcollision:integer;Ptvec: PointVectorType);}
   begin
      case i of
      1: begin
         lastcollision:= 3;
         Ptvec[1].x := 0.0;
         Ptvec[1].z := 0.0;
         Ptvec[2].x := 0.0;
         Ptvec[2].z := 7.564;
         Ptvec[3].x := -0.231;
         Ptvec[3].z := 18.889;
       end;{1}
      2: begin
         lastcollision:= 2;
         Ptvec[1].x := 0.0;
         Ptvec[1].z := 0.0;
         Ptvec[2].x := 0.0;
         Ptvec[2].z := 31.818;
       end;{2}
      3: begin
         lastcollision:= 5;
         Ptvec[1].x := 0.0;
         Ptvec[1].z := 0.0;
         Ptvec[2].x := 0.0;
         Ptvec[2].z := 1.032;
         Ptvec[3].x := 0.939;
         Ptvec[3].z := 0.727;
         Ptvec[4].x := 1.045;
         Ptvec[4].z := 0.581;
         Ptvec[5].x := 7.425;
         Ptvec[5].z := -8.693;
       end;{3}
      4: begin
         lastcollision:= 4;
         Ptvec[1].x := 0.0;
         Ptvec[1].z := 0.0;
         Ptvec[2].x := 0.0;
         Ptvec[2].z := 2.201;
         Ptvec[3].x := 0.121;
         Ptvec[3].z := 2.771;
         Ptvec[4].x := 11.166;
         Ptvec[4].z := 4.750;
       end;{4}
      5: begin
         lastcollision:= 3;
         Ptvec[1].x := 0.0;
         Ptvec[1].z := 0.0;
         Ptvec[2].x := 0.0;
         Ptvec[2].z := 0.259;
         Ptvec[3].x := 1.906;
         Ptvec[3].z := -0.216;
       end;{5}
      6: begin
         lastcollision:= 3;
         Ptvec[1].x := 0.0;
         Ptvec[1].z := 0.0;
         Ptvec[2].x := 0.0;
         Ptvec[2].z := 2.702;
         Ptvec[3].x := -6.224;
         Ptvec[3].z := 11.623;
       end;{6}
      7: begin
         lastcollision:= 3;
         Ptvec[1].x := 0.0;
         Ptvec[1].z := 0.0;
         Ptvec[2].x := 0.0;
         Ptvec[2].z := 2.137;
         Ptvec[3].x := 0.148;
         Ptvec[3].z := 2.037;
       end;{7}
      8: begin
         lastcollision:= 5;
         Ptvec[1].x := 0.0;
         Ptvec[1].z := 0.0;
         Ptvec[2].x := 0.0;
         Ptvec[2].z := 1.098;
         Ptvec[3].x := 0.16;
         Ptvec[3].z := 0.894;
         Ptvec[4].x := 0.549;
         Ptvec[4].z := 0.095;
         Ptvec[5].x := 1.253;
         Ptvec[5].z := -1.373;
       end;{8}
      9: begin
         lastcollision:= 4;
         Ptvec[1].x := 0.0;
         Ptvec[1].z := 0.0;
         Ptvec[2].x := 0.0;
         Ptvec[2].z := 3.83;
         Ptvec[3].x := -0.132;
         Ptvec[3].z := 4.01;
         Ptvec[4].x := -0.554;
         Ptvec[4].z := 4.65;
       end;{9}
      10: begin
         lastcollision:= 3;
         Ptvec[1].x := 0.0;
         Ptvec[1].z := 0.0;
         Ptvec[2].x := 0.0;
         Ptvec[2].z := 0.04;
         Ptvec[3].x := -1.706;
         Ptvec[3].z := -0.431;
       end;{10}
     end;{case}
   end;{GetPtvec}

   procedure GetTransDistr;
   begin
      TransDistr.put(1, 935);
      TransDistr.put(2, 844);
      TransDistr.put(3, 730);
      TransDistr.put(4, 613);
      TransDistr.put(5, 455);
      TransDistr.put(6, 400);
      TransDistr.put(7, 279);
      TransDistr.put(8, 131);
      TransDistr.put(9, 96);
      TransDistr.put(10, 56);
      TransDistr.put(11, 8);
   end;{gettransdistr}

   procedure ChooseMaterial;{ (num: integer;
                var Rad: RadType;
                var thisMaterial: MaterialType);}
      var
         mat: materialsType;
   begin
      with Rad do
         with thisMaterial do begin
            singleMaterialname := materialname[num];
            mat := material[num];
            case mat of
               air: begin
                  Z[1] := 7;
                  Z[2] := 8;
                  Z[3] := 18;
                  Zeff := 15;
                  N[1] := 2.5e19;
                  N[2] := 2.7e19;
                  N[3] := 2.7e19;
                  Neff := N[1];
                  sigmax[1] := 20;
                  M[1] := 14.52;
                  M[2] := 15.9994;
                  M[3] := 39.948;
                  Meff := 70.4674;
                  Iave[1] := 85.7e-6;{MeV, from Leo}
                  Iave[2] := 85.7e-6;{MeV, from Leo}
                  Iave[3] := 85.7e-6;{MeV, from Leo}
                  IaveEff := 85.7e-6;{MeV, from Leo}
                  density[1] := 0.001205;
                  density[2] := 0.001429;
                  density[3] := 0.001784;
                  densityeff := 0.001205;
                  Lrad[1] := 30050;
                  d1[1]:= 6.5870;
                  d1[2]:= 6.5293;
                  d1[3]:= 6.4305;
                  f[1,1]:= 0.22719;
                  f[1,2]:= 0.14932;
                  f[1,3]:= 0.6823e-2;
                  f[1,4]:= -0.853e-3;
                  f[2,1]:= 0.25813;
                  f[2,2]:= 0.16034;
                  f[2,3]:= 1.1139e-2;
                  f[2,4]:= -0.5261e-3;
                  f[3,1]:= 0.39574;
                  f[3,2]:= 0.21966;
                  f[3,3]:= 3.2532e-2;
                  f[3,4]:= 1.3867e-3;
                  h[1,1]:= -0.06371;
                  h[1,2]:= 0.05702;
                  h[1,3]:= 0.0893e-2;
                  h[1,4]:= -0.1851e-3;
                  h[2,1]:= -0.02805;
                  h[2,2]:= 0.05792;
                  h[2,3]:= 0.2306e-2;
                  h[2,4]:= -0.0745e-3;
                  h[3,1]:= 0.14439;
                  h[3,2]:= 0.07730;
                  h[3,3]:= 0.8471e-2;
                  h[3,4]:= 0.7610e-3;
                  w[1]:= 0.755;
                  w[2]:= 0.232;
                  w[3]:= 0.013;
                end;
               plastic: begin
                  Z[1] := 1;
                  Z[2] := 6;
                  Zeff := 8;
                  N[1] := 2.68e19;
                  N[2] := 1.13e23;
                  Neff := 3.95e22;
                  M[1] := 1.00797;
                  M[2] := 12.01115;
                  Meff := 14.02709;
                  sigmax[1] := 20;
                  Iave[1] := 65e-6;{MeV, from Leo actually for plastic scintillator}
                  Iave[2] := 78e-6;{MeV, from Leo actually for plastic scintillator}
                  IaveEff := 57.4e-6;{MeV}
                  density[1] := 0.8988e-4;
                  density[2] := 2.25;
                  densityeff := 0.92;
                  Lrad[1] := 42.9;
                  d1[1]:= 9.8520;
                  d1[2]:= 6.6696;
                  d1[3]:= 0.0;
                  f[1,1]:= 0.41384;
                  f[1,2]:= 0.19703;
                  f[1,3]:= 2.7608e-2;
                  f[1,4]:= 1.9841e-3;
                  f[2,1]:= 0.22127;
                  f[2,2]:= 0.14523;
                  f[2,3]:= 0.5969e-2;
                  f[2,4]:= -0.7762e-3;
                  f[3,1]:= 0.0;
                  f[3,2]:= 0.0;
                  f[3,3]:= 0.0;
                  f[3,4]:= 0.0;
                  h[1,1]:= -0.00881;
                  h[1,2]:= 0.06893;
                  h[1,3]:= 0.1158e-2;
                  h[1,4]:= 0.6558e-3;
                  h[2,1]:= -0.07644;
                  h[2,2]:= 0.05754;
                  h[2,3]:= 0.0359e-2;
                  h[2,4]:= -0.1501e-3;
                  h[3,1]:= 0.0;
                  h[3,2]:= 0.0;
                  h[3,3]:= 0.0;
                  h[3,4]:= 0.0;
                  w[1]:= 0.1437;
                  w[2]:= 0.8563;
                  w[3]:= 0.00;
                end;
               aluminum: begin
                  Z[1] := 13;
                  Zeff := Z[1];
                  N[1] := 6.02e22;
                  Neff := N[1];
                  M[1] := 26.98;
                  Meff := M[1];
                  sigmax[1] := 8.45;
                  Iave[1] := 166e-6;{MeV, from Leo}
                  IaveEff := Iave[1];
                  density[1] := 2.70;
                  densityeff := density[1];
                  Lrad[1] := 8.9;
                  d1[1]:= 6.4185;
                  d1[2]:= 0.0;
                  d1[3]:= 0.0;
                  f[1,1]:= 0.37066;
                  f[1,2]:= 0.2045;
                  f[1,3]:= 2.875e-2;
                  f[1,4]:= 1.0984e-3;
                  f[2,1]:= 0.0;
                  f[2,2]:= 0.0;
                  f[2,3]:= 0.0;
                  f[2,4]:= 0.0;
                  f[3,1]:= 0.0;
                  f[3,2]:= 0.0;
                  f[3,3]:= 0.0;
                  f[3,4]:= 0.0;
                  h[1,1]:= 0.10366;
                  h[1,2]:= 0.06827;
                  h[1,3]:= 0.7722e-2;
                  h[1,4]:= 0.5543e-3;
                  h[2,1]:= 0.0;
                  h[2,2]:= 0.0;
                  h[2,3]:= 0.0;
                  h[2,4]:= 0.0;
                  h[3,1]:= 0.0;
                  h[3,2]:= 0.0;
                  h[3,3]:= 0.0;
                  h[3,4]:= 0.0;
                  w[1]:= 1.00;
                  w[2]:= 0.00;
                  w[3]:= 0.00;
                end;
               silcon: begin
                  Z[1] := 14;
                  Zeff := Z[1];
                  N[1] := 8.5e22;
                  Neff := N[1];
                  M[1] := 28.086;
                  Meff := M[1];
                  sigmax[1] := 45;
                  Iave[1] := 173e-6;{MeV, from Leo}
                  IaveEff := Iave[1];
                  density[1] := 2.42;
                  densityeff := density[1];
                  Lrad[1] := 8.9;{using Al for the moment}
                  d1[1]:= 6.4158;
                  d1[2]:= 0.0;
                  d1[3]:= 0.0;
                  f[1,1]:= 0.37512;
                  f[1,2]:= 0.20792;
                  f[1,3]:= 2.9722e-2;
                  f[1,4]:= 1.1044e-3;
                  f[2,1]:= 0.0;
                  f[2,2]:= 0.0;
                  f[2,3]:= 0.0;
                  f[2,4]:= 0.0;
                  f[3,1]:= 0.0;
                  f[3,2]:= 0.0;
                  f[3,3]:= 0.0;
                  f[3,4]:= 0.0;
                  h[1,1]:= 0.11117;
                  h[1,2]:= 0.07045;
                  h[1,3]:= 0.7656e-2;
                  h[1,4]:= 0.5828e-3;
                  h[2,1]:= 0.0;
                  h[2,2]:= 0.0;
                  h[2,3]:= 0.0;
                  h[2,4]:= 0.0;
                  h[3,1]:= 0.0;
                  h[3,2]:= 0.0;
                  h[3,3]:= 0.0;
                  h[3,4]:= 0.0;
                  w[1]:= 1.00;
                  w[2]:= 0.00;
                  w[3]:= 0.00;
                end;
               lead: begin
                  Z[1] := 82;
                  Zeff := Z[1];
                  N[1] := 3.3e22;
                  Neff := N[1];
                  M[1] := 207.19;
                  Meff := M[1];
                  sigmax[1] := 80;
                  Iave[1] := 823e-6;{MeV, from Leo}
                  IaveEff := Iave[1];
                  density[1] := 11.34;
                  densityeff := density[1];
                  Lrad[1] := 0.56;
                  d1[1]:= 7.572;
                  d1[2]:= 0.0;
                  d1[3]:= 0.0;
                  f[1,1]:= 0.20234;
                  f[1,2]:= 0.11091;
                  f[1,3]:= 2.3613e-2;
                  f[1,4]:= 1.4037e-3;
                  f[2,1]:= 0.0;
                  f[2,2]:= 0.0;
                  f[2,3]:= 0.0;
                  f[2,4]:= 0.0;
                  f[3,1]:= 0.0;
                  f[3,2]:= 0.0;
                  f[3,3]:= 0.0;
                  f[3,4]:= 0.0;
                  h[1,1]:= 0.11764;
                  h[1,2]:= 0.07059;
                  h[1,3]:= 0.8275e-2;
                  h[1,4]:= 1.0978e-3;
                  h[2,1]:= 0.0;
                  h[2,2]:= 0.0;
                  h[2,3]:= 0.0;
                  h[2,4]:= 0.0;
                  h[3,1]:= 0.0;
                  h[3,2]:= 0.0;
                  h[3,3]:= 0.0;
                  h[3,4]:= 0.0;
                  w[1]:= 1.00;
                  w[2]:= 0.00;
                  w[3]:= 0.00;
                end;
               NaImat: begin
                  Z[1] := 11;
                  Z[2] := 53;
                  Zeff := 64;
                  N[1] := 2.5e22;
                  N[2] := 2.3e22;
                  Neff := 1.47e22;
                  M[1] := 22.9898;
                  M[2] := 126.9044;
                  Meff := 149.8942;
                  sigmax[1] := 20;
                  Iave[1] := 65e-6;{MeV, from Leo actually for plastic scintillator}
                  Iave[2] := 65e-6;{MeV, from Leo actually for plastic scintillator}
                  IaveEff := 452e-6;
                  density[1] := 0.971;
                  density[2] := 4.94;
                  densityeff := 3.667;
                  Lrad[1] := 42.9;
                  d1[1]:= 6.4357;
                  d1[2]:= 7.0175;
                  d1[3]:= 0.0;
                  f[1,1]:= 0.30421;
                  f[1,2]:= 0.18604;
                  f[1,3]:= 1.8046e-2;
                  f[1,4]:= -0.0445e-3;
                  f[2,1]:= 0.34856;
                  f[2,2]:= 0.19525;
                  f[2,3]:= 3.3656e-2;
                  f[2,4]:= 1.6683e-3;
                  f[3,1]:= 0.0;
                  f[3,2]:= 0.0;
                  f[3,3]:= 0.0;
                  f[3,4]:= 0.0;
                  h[1,1]:= 0.02861;
                  h[1,2]:= 0.06669;
                  h[1,3]:= 0.4096e-2;
                  h[1,4]:= 0.1421e-3;
                  h[2,1]:= 0.197;
                  h[2,2]:= 0.09562;
                  h[2,3]:= 0.9936e-2;
                  h[2,4]:= 1.1735e-3;
                  h[3,1]:= 0.0;
                  h[3,2]:= 0.0;
                  h[3,3]:= 0.0;
                  h[3,4]:= 0.0;
                  w[1]:= 0.1437;
                  w[2]:= 0.8563;
                  w[3]:= 0.00;
                end;
               end;{case}
            GetConstantArrays(thisMaterial);
         end;{with thisMaterial}
   end;{ChooseMaterial}

End.{unit InteractionData}
