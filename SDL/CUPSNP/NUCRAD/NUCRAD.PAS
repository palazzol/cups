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

program NuclearRadiation;
{$M 65520,0,655360}

   uses
       Graph, Cups, CupsMupp, Cupsfunc, CupsGui, CupsGrph, CupsProc,
       NucInter, NucGraph;
   const
      RBNames = '123';{way of storing ID names of radio-buttons}
   type
      CrossSectionType = record
            sigtotal: real;
            sigPhoto: real;
            sigCompton: real;
            sigPair: real;
         end;
      OmegaType = record      {direction cosines}
            alpha: real;
            beta: real;
            gamma: real;
         end;{OmegaType}
      table = array[1..numAngles] of real;
      modeType = (MainMode, RadMode, NaIMode );
   var
      MainMenu : TMenu;
      MainHotKeys, RadHotKeys, NaIHotKeys : THotKeys;
      idumconst{, transmit, reflect, absorb, out}: integer;
      Eprob, Eav: real;
      QuitFlag, OneStepRad, OneStepNaI: boolean;
      Mode : ModeType;
      Rad : RadType;
      NaI : NaIType;


{******************** Menu, HotKeys, and Help Procedures *******************}

   procedure SetUpMainMenu;
   begin
      with MainMenu do
         begin
            init;
            column(1, 'File');
            row(1, 1, 'About CUPS');
            row(1, 2, 'About the program');
            row(1, 3, 'Configuration');
            row(1, 4, '-----------------');
            row(1, 5, 'Exit');
            column(2, 'Radiation');
            row(2, 1, 'Proton');
            row(2, 2, 'Muon');
            row(2, 3, 'Electron');
            row(2, 4, 'Photon');
            column(3,'NaI');
            row(3, 1, 'Photons in NaI');
            rowactivate(1,4,false);
            display;
         end;
   end;{SetUpMainMenu}

   procedure SetUpRadHotKeys;
   begin
      with RadHotKeys do
         begin
            init(2);
            key[1] := 'F1-Help';
      {     key[2] := 'F6-One';
            key[3] := 'F7-Run';
            key[4] := 'F8-Refresh'; }
            key[2] := 'ESC-Stop';
            Display;
         end;
   end;{SetUpRadHotKeys}

   procedure SetUpNaIHotKeys;
   begin
      with NaIHotKeys do
         begin
            init(2);
            key[1] := 'F1-Help';
            key[2] := 'ESC-Stop';
            Display;
         end;
   end;{SetUpNaIHotKeys}

   procedure SetUpMainHotKeys;
   begin
{new(MainHotKeys);}
      with MainHotKeys do
         begin
            init(2);
            key[1] := 'F1-Help';
            key[2] := 'F10-Menu';
            Display;
         end;
   end;{SetUpMainHotKeys}

   procedure OpenMess;
     var helpScr: HelpScrType;
   begin
      helpScr[1] := '                                                  ';
      helpScr[2] := '                                                  ';
      helpScr[3] := '          Radiation Interaction in Matter         ';
      helpScr[4] := '                                                  ';
      helpScr[5] := '               by Roberta A. Bigelow              ';
      helpScr[6] := '               Willamette University              ';
      helpScr[7] := '         (c) 1995 John Wiley & Sons, Inc.         ';
      helpScr[8] := '                                                  ';
      helpScr[9] := '                                                  ';
      helpScr[10] := '  This simulation explores the interaction of     ';
      helpScr[11] := '  alpha particles, muons, electrons, and photons  ';
      helpScr[12] := '  with matter. You will develop an understanding  ';
      helpScr[13] := '  of how ranges, energy losses, and random par-   ';
      helpScr[14] := '  ticle paths depend on materials, radiation, and ';
      helpScr[15] := '  incident energy. As a specific application, you ';
      helpScr[16] := '  can explore photon and electron interactions    ';
      helpScr[17] := '  in a sodium iodide crystal which determines     ';
      helpScr[18] := '  the energy response of a radiation detector.    ';
      helpScr[19] := '                                                  ';
      helpScr[20] := ' Menu choices are:                                ';
      helpScr[21] := '   File      - Project and program information    ';
      helpScr[22] := '   Radiation - General radiation exploration      ';
      helpScr[23] := '   NaI       - Photons in a NaI detector          ';
      helpScr[24] := '                                                  ';
      helpScr[25] := '                                                  ';
      help(helpScr);
   end;{OpenMess}


{************************* Graphing Procedures **************************}

{S Interaction}
{************************* Interaction Procedures **************************}

   function InputText1 (var Rad: RadType): string;
      var
         radiation: sourcetype;
   begin
      with Rad do begin
            radiation := source[1];
            case radiation of
               proton: 
                  InputText1 := '    for a PROTON Source   ';
               muon: 
                  InputText1 := '    for a MUON Source   ';
               electron: 
                  InputText1 := '    for a ELECTRON Source   ';
               photon:
                  InputText1 := '    for a PHOTON Source   ';
            end;{radiation}
         end;{with Rad}
   end;{InputText1}

   function InputText2 (var Rad: RadType): string;
      var
         radiation: sourcetype;
   begin
      with Rad do begin
            radiation := source[1];
            case radiation of
               proton:
                  InputText2 := '    (Limits:  0.5 - 100 MeV)   ';
               muon:
                  InputText2 := '    (Limits:  0.5 - 1000 MeV)  ';
               electron:
                  InputText2 := '    (Limits:  0.02 - 0.4 MeV)   ';
               photon:
                  InputText2 := '    (Limits:  0.02 - 100 MeV)   ';
            end;{radiation}
         end;{with Rad}
   end;{InputText2}

   procedure SourceInput (var Rad: RadType);
      var
         i, numviews, matter: integer;
         tempreal: real;
         noerror: boolean;
         SourceScreen: TInputScreen;

   begin
{new(SourceScreen);}
      with Rad do begin
         with SourceScreen do begin
            Init;
               DefineInputport(0.07, 0.60, 0.1, 0.9);
               LoadLine(' EXPERIMENTAL SETUP INFORMATION       ');
               LoadLine(InputText1(Rad));
               LoadLine('  ');
               LoadLine(' Beam Energy = {       }MeV ');
               LoadLine(InputText2(Rad));
               LoadLine('  ');
               LoadLine('   ');
               LoadLine('   #2 Air              #2 Silcon ');
               LoadLine('   #2 Plastic(CH2)x    #2 Lead  ');
               LoadLine('   #2 Aluminum         #2 NaI');
               LoadLine('  ');
               LoadLine(' Material Thickness = {      } cm');
               LoadLine('   #T Range or Attenuation Calculation');
               LoadLine('  ');
               LoadLine(' Number of particles {     }');
               LoadLine('  ');
               LoadLine('     [  Ok  ]   [ Help ]   [Cancel]');
   {        end;{else}
{moving your data to the input screen}
            SetNumber(1, energy);
   {        for i := 1 to 6 do
               SetBoolean(i + 1, false);    }
               case material[1] of
                  Air: SetRadioButton('2', 1);
                  Plastic: SetRadioButton('2', 3);
                  Aluminum: SetRadioButton('2', 5);
                  Silcon: SetRadioButton('2', 2);
                  Lead: SetRadioButton('2', 4);
                  NaImat: SetRadioButton('2', 6);
                end;{case material}
               SetNumber(8, thickness);
               if Rad.range then
                  SetBoolean(9, true)
               else
                  SetBoolean(9, false);
               SetNumber(10, MonteCarlo);
               SetHelpFile('NUCRAD.HLP', 'HelpScreen1');
               repeat   {until noerror or canceled}
                  noerror := true;
                  case source[1] of
                  proton: begin
                     sourceStr := 'proton';
                     SetNumberLimits(1, 0.5, 100);
                   end;
                  muon: begin
                     sourceStr := 'muon';
                     SetNumberLimits(1, 0.5, 1000);
                   end;
                  electron: begin
                     sourceStr := 'electron';
                     SetNumberLimits(1, 0.02, 0.4);
                   end;
                  photon: begin
                     sourceStr := 'photon';
                     SetNumberLimits(1, 0.02, 100);
                   end;
                end;{case}
               SetNumberLimits(8, 1.0e-5, 1000);
               SetNumberLimits(10, 1, 100000);
               AcceptScreen;
{Assign energy}
               energy := GetNumber(1);
               energyStr := NumStr(Rad.energy, 6, 3);
{Assign material}
               multimaterials := 1;
               Matter := GetRadioButton('2');
               case matter of
                  1{air}: begin
                     materialname[multimaterials] := 'Air';
                     material[1] := air;
                     materialStr := 'Air';
                   end;
                  3{Plastic}: begin
                     materialname[multimaterials] :=  'Plastic';
                     material[1] :=  Plastic;
                     materialStr := 'Plastic';
                   end;
                  5{Aluminum}: begin
                     materialname[multimaterials] := 'Aluminum';
                     material[1] := aluminum;
                     materialStr := 'Aluminum';
                   end;
                  2{Silcon}: begin
                     materialname[multimaterials] := 'Silcon';
                     material[1] := silcon;
                     materialStr := 'Silcon';
                   end;
                  4{Lead}: begin
                     materialname[multimaterials] := 'Lead';
                     material[1] := lead;
                     materialStr := 'Lead';
                   end;
                  6{NaImat}: begin
                     materialname[multimaterials] := 'NaI';
                     material[1] := NaImat;
                     materialStr := 'NaI';
                   end;
                end;{case material}
{Assign thickness}
               thickness := GetNumber(8);
               thicknessStr := NumStr(Rad.thickness, 7, 5);
               Range:= GetBoolean(9);
{Calculate the number of viewports}
               numviews := multimaterials * multisources + 2;
               if numviews > 4 then begin
                  announce('The number of materials times the number of sources can not be greater than 2.');
                  noerror := false;
                end;
{Assign the number of MonteCarlo runs}
               tempReal := GetNumber(10);
               MonteCarlo := round(tempReal);
               MCtotalStr := NumStr(MonteCarlo, 6, 0);
            until noerror or canceled;
            done;
{dispose(SourceScreen);}
         end;{with SourceScreen}
      end;{with Rad}
     CloseViewPort(10);
   end;{SourceInput}

   function SimpleRandom (var idum: integer): real;
   begin
      SimpleRandom := Urand(idum);
   end;{SimpleRandom}

   procedure InitialRadValues (var Rad: RadType;
               var thispt: MyPointType;
               var thisOmega: OmegaType;
               var Einitial: real);
{Assume a collimated beam of particles with a beam radius equal to 0.}
      var
         Rphi, phi, beamsize: real;

   begin
      idumconst := 1;
      beamsize := 0.0 * Rad.thickness;
      Rphi := SimpleRandom(idumconst) * beamSize;
      phi := 2 * pi * SimpleRandom(idumconst);
      with thisPt do
         begin
            thisPt.x := Rphi * cos(phi);
            thisPt.y := Rphi * sin(phi);
            thisPt.z := 0;
         end;{with thisPt}
      with thisomega do
         begin
            gamma := 1; {cos(theta)}
            alpha := 0; {sin(theta)cos(phi)}
            beta := 0;  {sin(theta)sin(phi)}
         end;{with thisomega}
      Einitial := Rad.energy;
   end;{InitialRadValues}

   procedure CalcSigCompton (E: real;
                  var thisMaterial: MaterialType;
                  var allSigma: CrossSectionType);
      const
         c1 = 1.651035;
         c2 = 9.340220;
         c3 = -8.325004;
         d1 = 12.501332;
         d2 = -14.200407;
         d3 = 1.699075;
      var
         x, term1, term2, term3, eta: real;

   begin
      with allsigma do
         begin
            x := E / mec2;
            eta := 1 + 0.222037 * x;
{           term1 := (1 + x) / sqr(1 + 2 * x);
            term2 := 2 / sqr(x);
            term3 := (sqr(x) - 2 * x - 2) * ln(1 + 2 * x) / (2 * pwr(x, 3));
            sigCompton := 2 * pi * sqr(re) * (term1 + term2 + term3);}
            term1 := c1*sqr(eta)+c2*eta+c3;
            term2 := pwr(eta,3)+d1*sqr(eta)+d2*eta+d3;
            sigCompton := pi *sqr(re) *(term1/term2);
            sigCompton := thisMaterial.Zeff * sigCompton * 1e24;{b/atom}
         end;{with allSigma}
   end;{CalcSigCompton}

   procedure CalcSigPhoto (E: real;
                var thisMaterial: materialType;
                var allSigma: CrossSectionType);
      var
         sigK, factor: real;
         i: integer;
         tooSmall: boolean;

   begin
      tooSmall := false;
      with thisMaterial do
         with allSigma do begin
            if E < 0.08 then
               sigPhoto := sigmax[1] * 2
            else  begin
               sigK := 0.0;
               i := 1;
               while (i <= 4) and (not tooSmall) do begin
                  factor := pwr(E, -p[i]);
                  sigK := sigK + factor * (a[i] + b[i] * Zeff) / (1 + c[i] * Zeff);
                  if factor < 1e-12 then
                     tooSmall := true;
                  inc(i);
                end;{for i}
                sigK := pwr(Zeff, 5) * sigK;{b/atom}
                if Zeff < 30 then
                   sigPhoto := 9 * sigK / 8
                else
                   sigPhoto := 5 * sigK / 4;
             end;{else}
      end;{with allSigma}
   end;{CalcSigPhoto}

   procedure CalcSigPair (E: real;
                index: integer;
                var EnergyPairArray: Array59;
                interp: boolean;
                var allSigma: CrossSectionType);
      var
         offset, numerator, denominator: real;
   begin
      with allSigma do begin
         if E < 1.5 then  {Pair cross section data begins at 1.5 MeV}
            sigPair := 0
         else if interp then  begin
            numerator := (E - EnergyPairArray[1, index - 1])
                      * (EnergyPairArray[2, index] - EnergyPairArray[2, index - 1]);
            denominator := (EnergyPairArray[1, index] - EnergyPairArray[1, index - 1]);
            offset := numerator / denominator;
            sigPair := EnergyPairArray[2, index - 1] + offset;
          end
         else
            sigPair := EnergyPairArray[2, index];{b/atom}
         end;{with allSigma}
   end;{CalcSigPair}

   procedure Calculatelambda (var thisMaterial: MaterialType;
                var thissource: sourceType;
                E: real;
                var lambda, zeta: real;
                var allSigma: CrossSectionType);
      const
         ao = 0.0529e+6;{Bohr radius in fm}
         hbarc = 197;{MeV fm}
      var
         i, index: integer;
         sigK, term1, term2, numerator, denominator: real;
         interp, leavebool: boolean;
   begin
      with thisMaterial do begin
         case thissource of
            electron: begin      {AJP 54,no.3,March 1986}
               term1 := pwr(Zeff, 1 / 3) / (0.885 * ao);
               term2 := hbarc / sqrt(sqr(E + mec2) - sqr(mec2));
               zeta := 0.25 * sqr(1.12 * term1 * term2);
               numerator := 1.02 * Na * zeta * (1 + zeta) * sqr(E) * 1e2;{last factor needed to make}
               denominator := Na * densityeff * Zeff * (Zeff + 1) / Meff;                    {lambda come out in cm}
               lambda := numerator / denominator;{cm}
               with allsigma do begin
                  sigtotal := 0.0;
                  sigcompton := 0.0;
                  sigphoto := 0.0;
                  sigpair := 0.0;
                end;{with allsigma}
             end;{electron}
            photon: begin
               i := 1;
               leavebool := false;
               while (i <= 59) and (not leavebool) do begin
                  if (EnergyPairArray[1, i] - E) >= 0 then begin
                     index := i;
                     leavebool := true;
                   end;{if}
                  if i = 59 then begin
                     message('Energy too big, it has been set to Emax = 100GeV');
                     index := 59;
                   end;{if i=59}
                  inc(i);
                end;{for i}
               interp := true;

               CalcSigCompton(E, thisMaterial, allSigma);
               CalcSigPhoto(E, thisMaterial, allSigma);
               CalcSigPair(E, index, EnergyPairArray, interp, allSigma);

               with allSigma do  begin
                  sigTotal := sigCompton + sigPhoto + sigPair;{b/atom}
                  lambda := 1 / (Neff * sigTotal * 1e-24);{cm}
                end;{with allSigma}
               zeta := 0.0;
            end;{photon}
         end;{case thissource}
      end;{with thisMaterial}
   end;{Calculatelambda}

   procedure Angledensity (E: real;
               var subsum, cosines: table);
{ This Procedure calculates the cross section per unit solid angle }
{"SigKN" according to the Klein-Nishina formula, and also calculates}
{the area of sin» * SigKN for randomly selecting the angle later on.}
      var
         x, y, theta, SigKN: real;
         i: integer;
         sines, Escatter: table;
   begin
      x := E / mec2;{E is in MeV}
      for i := 1 to Numangles do begin
         theta := DegToRad * (round(i * 180 / Numangles));
         sines[i] := sin(theta);
         cosines[i] := cos(theta);
         y := 1 - cosines[i];      {temporary for 1 - cos(theta)}
         SigKN := (1 + sqr(cosines[i]) + sqr(x * y) / (1 + x * y)) / sqr(1 + x * y);
         SigKN := SigKN * sqr(re) * 1e24 / 2;{b/atom}
         subsum[i] := (SigKN {* sines[i]});   {temp for sigma times sin(theta)}
         if i > 1 then
            subsum[i] := subsum[i] + subsum[i - 1];
       end; {for i}
      for i := 1 to Numangles do
         subsum[i] := subsum[i] / subsum[Numangles];
   end;{Angledensity}

   function WhatAngleBin (var subsum: table): integer;
{ This Function randomly selects the bin for an angle,}
{  proportional to sin»*SigKN.}
      var
         p: real;
         i: integer;
         j: integer;
   begin
      p := SimpleRandom(idumconst);
      j:= 1;
      for i:= 1 to Numangles - 1 do
         if (subsum[i] < p) and (subsum[i+1] > p) then
            j:= i + 1;
      WhatAnglebin := j;
   end; {WhatAngleBin}

   procedure calcEscatterPhoton (E, cosScatt: real;
                  var morecollisions: boolean;
                  var Escatter: real);
      const
         energyCutOff = 0.01;{MeV; data begins at 0.01 MeV}
   begin
      Escatter := E / (1 + E * (1 - cosScatt));
      if energyCutOff > Escatter then
         begin
            absorb := absorb + 1;
            {writeln('absorbed - energycutoff');  }
            morecollisions := false;
         end;
   end;{calcEscatterPhoton}

   procedure calcsigmaRad (Rad: RadType;
                Eo: real;
                viewsnum: integer;
                var thisMaterial: MaterialType;
                var dEdxrad: real);
{from Heitler p251-2 eq. 29,30,31, esqr = 1.44e-13 MeVcm, e charge sqr,
 mec2 = 0.511 MeV      }
 {Try Seltzer and Beger Int.J.Appl.Radiat.Isot.Vol33, pp1219-1226, 1982}
      var
         i: integer;
         numerator, denominator, T, sigmabar, sigmarad: real;
         mat: materialsType;
   begin
      with thisMaterial do begin
         T:= Eo - mec2;
         if viewsnum =1 then
            mat:= Rad.material[1]
         else
            mat:= Rad.material[2];
         dEdxrad:= 0.0;
         case mat of
            air: begin
               for i:= 1 to 3 do begin
                  numerator:= 1 + f[i,1]*ln(T) + f[i,2]*sqr(ln(T)) + f[i,3]*pwr(ln(T),3) + f[i,4]*pwr(ln(T),4);
                  denominator:= 1 + h[i,1]*ln(T) + h[i,2]*sqr(ln(T)) + h[i,3]*pwr(ln(T),3) + h[i,4]*pwr(ln(T),4);
                  sigmabar := sqr(Z[i] * re) * N[i] / 137;
                  sigmarad := sigmabar * d1[i] * numerator / denominator;
                  dEdxrad := dEdxrad + w[i] * Eo * sigmaRad / density[i];
                end;
             end;{air}
            plastic: begin
               for i:= 1 to 2 do begin
                  numerator:= 1 + f[i,1]*ln(T) + f[i,2]*sqr(ln(T)) + f[i,3]*pwr(ln(T),3) + f[i,4]*pwr(ln(T),4);
                  denominator:= 1 + h[i,1]*ln(T) + h[i,2]*sqr(ln(T)) + h[i,3]*pwr(ln(T),3) + h[i,4]*pwr(ln(T),4);
                  sigmabar := sqr(Z[i] * re) * N[i] / 137;
                  sigmarad := sigmabar * d1[i] * numerator / denominator;
                  dEdxrad := dEdxrad + w[i] * Eo * sigmaRad / density[i];
                end;
             end;{plastic}
            NaImat: begin
               for i:= 1 to 2 do begin
                  numerator:= 1 + f[i,1]*ln(T) + f[i,2]*sqr(ln(T)) + f[i,3]*pwr(ln(T),3) + f[i,4]*pwr(ln(T),4);
                  denominator:= 1 + h[i,1]*ln(T) + h[i,2]*sqr(ln(T)) + h[i,3]*pwr(ln(T),3) + h[i,4]*pwr(ln(T),4);
                  sigmabar := sqr(Z[i] * re) * N[i] / 137;
                  sigmarad := sigmabar * d1[i] * numerator / denominator;
                  dEdxrad := dEdxrad + w[i] * Eo * sigmaRad / density[i];
                end;
             end;{NaImat}
            else begin
               numerator:= 1 + f[1,1]*ln(T) + f[1,2]*sqr(ln(T)) + f[1,3]*pwr(ln(T),3) + f[1,4]*pwr(ln(T),4);
               denominator:= 1 + h[1,1]*ln(T) + h[1,2]*sqr(ln(T)) + h[1,3]*pwr(ln(T),3) + h[1,4]*pwr(ln(T),4);
               sigmabar := sqr(Zeff * re) * Neff * densityeff / (137 * Meff);
               sigmarad := sigmabar * d1[1] * numerator / denominator;
               dEdxrad := dEdxrad + w[1] * Eo * sigmaRad / densityeff;
             end;{Al,Pb}
         end;{case}
      end;{with thisMaterial}
   end;{calcsigmaRad}

   procedure calcEscatterElectron (E: real;
                viewsnum: integer;
                var thismaterial: materialtype;
                distance, thick: real;
                var morecollisions: boolean;
                 var Escatter: real);
      const
         energyCutOff = 0.005;{MeV}
         {esqr = 1.44e-13 MeVcm, electron charge squared}
      var
         gamma, beta, beta2, dEdx, dEdxrad, sigmaRad, sigmabar, Eo: real;
         term1, term2, term3, term4, term5: real;
   begin
      with thismaterial do
         begin
            gamma := (E / mec2) + 1;{ratio from special relativity}
            beta2 := 1 - (1 / sqr(gamma));
            beta := sqrt(beta2);
            term1 := 2 * pi * Na * densityeff * sqr(esqr) * Zeff / (mec2 * beta2 * Meff);
            term2 := mec2 * beta2 / (2 * sqr(IaveEff) * (1 - beta2));
            term3 := (2 * sqrt(1 - beta2) - 1 + beta2) * ln(2);
            term4 := 1 - beta2;
            term5 := sqr(1 - sqrt(1 - beta2)) / 8;
            dEdx := term1 * (ln(term2 * E) - term3 + term4 + term5);{MeV/cm}

            Eo := E + mec2;
            calcsigmaRad(Rad, Eo, viewsnum, thisMaterial, dEdxrad);
            dEdx := dEdx + dEdxrad * densityeff;
            Escatter := E - abs(dEdx) * distance;
            if energyCutOff > Escatter then
               begin
                  morecollisions := false;
                  absorb := absorb + 1;
                  {writeln('energycutoff');}
               end;
         end;{with thismaterial}
   end;{calcEscatterElectron}

   procedure electronPath (E, zeta, distance, thick: real;
                 viewsnum: integer;
                 var thismaterial: materialtype;
                 var morecollisions: boolean;
                 var cosScatt, Efinal: real);
      var
         numerator, denominator: real;
         Rscatt: real;

   begin
      RScatt := SimpleRandom(idumconst);
      numerator := 2 * zeta * Rscatt;
      denominator := 1 + zeta - Rscatt;
      cosScatt := 1 - (numerator / denominator);
      calcEscatterElectron(E, viewsnum, thismaterial, distance, thick, morecollisions, Efinal);
   end;{electronPath}

   procedure NextCollision (j, collisions, viewsnum: integer;
               E, thick: real;
               var thisMaterial: MaterialType;
               var thissource: sourceType;
               var lastPt: MyPointType;
               var Omega: OmegaType;
               var morecollisions: boolean;
               var thisPt: MyPointType;
               var allSigma: CrossSectionType;
               var zeta, distance: real;
               var ETrans: DVector);
      var
         Rdistance, lambda, temp, zmax, s, n, cosscatt, Efinal, newdistance: real;

   begin
      Calculatelambda(thisMaterial, thissource, E, lambda, zeta, allSigma);
      Rdistance := SimpleRandom(idumconst);
      if Rdistance < 0.0000001 then
         Rdistance := 0.0000001;{prevents floating point errors when simplerandom returns a 0}
      distance := -lambda * ln(Rdistance);{cm}
      with Omega do
         with lastPt do begin
            thisPt.x := x + alpha * distance;
            thisPt.y := y + beta * distance;
            thisPt.z := z + gamma * distance;
          end;{with lastPt}
      if Omega.gamma >= 0.0 then
         zmax := thick - thisPt.z
      else
         zmax := thisPt.z;
      if (lambda <= 0.001 * zmax) and (collisions >= 1999) then begin
         morecollisions := false;
         absorb := absorb + 1;
       end;
      if collisions = (maxcollisions - 1) then begin
         morecollisions := false;
         n:= 1.265 - 0.0954 * ln(E);
         s := 0.75 * (412 * pwr(E, n) * 1e-3 / thismaterial.densityeff);{cm}
         if s >= (zmax/abs(omega.gamma)) then
            if Omega.gamma >= 0.01 then begin
               transmit := transmit + 1;
               electronPath(E, zeta, distance, thick, viewsnum, thismaterial,
                     morecollisions, cosScatt, Efinal);
               ETrans.Put(transmit, Efinal);
 {              announce('transmit - collisions');  }
               temp := (thick * 1.2) - thisPt.z;
               with omega do begin
                  newdistance := distance + temp / gamma;
                  with lastpt do begin
                     thisPt.x := x + alpha * newdistance;
                     thisPt.y := y + beta * newdistance;
                     thisPt.z := z + gamma * newdistance;
                   end;
               end;
             end
            else
               with omega do begin
                  if gamma <= -0.01 then begin
                     reflect := reflect + 1;
                     temp := (thick * -0.1) - thisPt.z;
                     newdistance := distance + abs(temp / gamma);
                     with lastpt do begin
                        thisPt.x := x + alpha * newdistance;
                        thisPt.y := y + beta * newdistance;
                        thisPt.z := z + gamma * newdistance;
                      end;
                  end;
                 { announce('reflect - collisions'); }
                end
         else begin
            absorb := absorb + 1;
 {           announce(concat('absorb - collisions',numstr(e,6,4),numstr(s,6,4),numstr(zmax,6,4)));}
          end;
       end;
      if morecollisions then begin
         if thisPt.z <= 0 then begin {particle has escaped out the front}
            morecollisions := false;
            {writeln('backscatter'); }
            reflect := reflect + 1;
            with omega do begin
               temp := (thick * -0.1) - thisPt.z;
               newdistance := distance + abs(temp / gamma);
               with lastpt do begin
                  thisPt.x := x + alpha * newdistance;
                  thisPt.y := y + beta * newdistance;
                  thisPt.z := z + gamma * newdistance;
                end;
             end;
          end;
         if thisPt.z >= thick then begin {particle has escaped out the back}
            morecollisions := false;
         if (thissource = photon) and((thisPt.x > 0.05) or (thisPt.y > 0.05)) then begin
            morecollisions := false;
            out:= out + 1;
          end
         else begin
            {writeln('transmitted'); }
            transmit := transmit + 1;
            ETrans.Put(transmit, E);
            temp := (thick * 1.2) - thisPt.z;
               with omega do begin
                  newdistance := distance + abs(temp / gamma);
                  with lastpt do begin
                     thisPt.x := x + alpha * newdistance;
                     thisPt.y := y + beta * newdistance;
                     thisPt.z := z + gamma * newdistance;
                   end;
               end;
          end;
       end;
      if (abs(thisPt.y) > 10 * thick) or (abs(thisPt.x) > 10 * thick) then begin
         morecollisions := false;
         out:= out + 1;
            {writeln('out sides'); }
       end;
      end;{if morecollisions}
   end;{NextCollision}

   procedure NextCollisionHeavy (E, thick: real;
                 viewsnum: integer;
                 var Rad: RadType;
                 var thisMaterial: MaterialType;
                 var thissource: sourceType;
                 var lastPt: MyPointType;
                 var Omega: OmegaType;
                 var morecollisions: boolean;
                 var thisPt: MyPointType;{}
                 var Efinal, distance: real;
                 var Etrans: DVector);
(*  Instead of calulating a mean free path then using the exponential
distribution for finding the distance that the particle traveled, this
procedure a logarithmically spaced pathlength.  On average the energy
of the particle is reduced by a constant factor k per step, where k
will be 2^(1/m), where m is an integer and the particle will lose
half its energy in m steps    *)
      const
         m = 64;
         energyCutOff = 0.0050;{MeV}
      var
         Rdistance, lambda, k, deltaE, mc2, temp, deltaEmean, Erms: real;
         gamma, gamma2, beta, beta2, term1, term2, term3, term4, eta: real;
         dEdx, tau, denominator, Wmax: real;

   begin

      if thisSource = proton then
         mc2 := mpc2
      else if thisSource = muon then
         mc2 := mmc2;
      k := pwr(2, -1 / m);
      tau := E / mc2;
      gamma := tau + 1;{ratio from special relativity}
      gamma2 := sqr(gamma);
      beta2 := 1 - (1 / gamma2);
      beta := sqrt(beta2);
      eta := beta * gamma;
      with thismaterial do begin
         term1 := 4 * pi * Na * densityeff * sqr(re) * Zeff * mec2 / (beta2 * Meff);
         term2 := 2 * mec2 * beta2 * gamma2 / IaveEff;
         term3 := (0.422377*pwr(eta,-2)+0.0304043*pwr(eta,-4)-0.00038106*pwr(eta,-6))*1e-6*sqr(IaveEff);
         term4 := term3 + (3.85019*pwr(eta,-2)-0.1667989*pwr(eta,-4)+0.00157955*pwr(eta,-6))*1e-9*pwr(IaveEff,3);
         dEdx := 0.5*term1 * (ln(term2) - beta2 - (term4 / Zeff));{MeV/cm inc shell corrections}
         distance := E * (1 - k) / abs(dEdx);
         deltaEmean := abs(dEdx) * distance;
            {Erms := sqrt(4*pi*z^2*e^4*NZ*1e-26   MeV^2/cm}
         Erms := 0.02*sqrt(4 * pi * sqr(esqr) * Na * densityeff * Zeff / Meff);
       end;{with thismaterial}
      deltaE := Erms * sqrt(-2 * ln(1 - SimpleRandom(idumconst))) * cos(2 * pi * simpleRandom(idumconst)) + deltaEmean;
      Efinal := E - deltaE;
      if energyCutOff > Efinal then begin
            morecollisions := false;
            {writeln('energycutoff');}
            absorb := absorb + 1;
         end;
      with Omega do
         with lastPt do begin
               thisPt.x := x + alpha * distance;
               thisPt.y := y + beta * distance;
               thisPt.z := z + gamma * distance;
            end;{with lastPt}
      if thisPt.z <= 0 then begin {particle has escaped out the front}
            morecollisions := false;
            {writeln('backscatter');}
            reflect := reflect + 1;
         end;
      if thisPt.z >= thick then  {particle has escaped out the back}
         begin
            morecollisions := false;
            {writeln('transmitted');}
            transmit := transmit + 1;
            ETrans.Put(transmit,E);
            temp := (thick * 1.2) - thisPt.z;
            if temp > 0.0 then begin
               thisPt.z := thick * 1.2;
               thisPt.x := thisPt.x + (sqrt(1 - sqr(Omega.gamma)) / Omega.gamma) * temp;
             end;
         end;
      if (abs(thisPt.y) > 4 * thick) or (abs(thisPt.x) > 4 * thick) then
         begin
            morecollisions := false;
            out:= out + 1;
            {writeln('out sides');}
         end;
   end;{NextCollisionHeavy}

   procedure DirectionAfterScatt (lastomega: OmegaType;
                                  cosScatt: real;
                                  var thisomega: OmegaType);
      const
         n = 8;
      var
         Rphi, phi, cosphi, sinphi:real;
         sinScatt, sinfactor, singamma, numerator: real;

   begin
      sinScatt := sqrt(abs(1 - sqr(cosScatt)));{prevents SANE floating point error with -0.00}
      Rphi := SimpleRandom(idumconst);
      phi := 2 * pi * Rphi;
      cosphi := cos(phi);
      sinphi := sin(phi);
      with lastOmega do begin
         if (1 - abs(gamma)) < pwr(2, -n) then  begin
            thisOmega.alpha := sinScatt * cosphi;
            thisOmega.gamma := cosScatt * gamma / abs(gamma);
            thisOmega.beta := sinScatt * sinphi;
          end
         else  begin
            singamma := sqrt(1 - sqr(gamma));
            sinfactor := sinScatt / singamma;
            numerator := (cosphi * gamma * alpha) - (sinphi * beta);
            thisOmega.alpha := cosScatt * alpha + sinfactor * numerator;
            numerator := (cosphi * gamma * beta) + (sinphi * alpha);
            thisOmega.gamma := cosScatt * gamma - sinfactor
                             * sqr(singamma) * cosphi;
            thisOmega.beta := cosScatt * beta + sinfactor * numerator;
          end;
       end;{with lastOmega}
   end;{DirectionAfterScatt}

   procedure photonPath (collisions: integer; E: real;
                var allSigma: CrossSectionType;
                var morecollisions: boolean;
                var cosScatt, Efinal: real;
                var AngleBin: integer);
      var
         PCompton, PPair, PPhoto: real;
         x: real;
         subsum, cosines: table;
   begin
      with allSigma do begin
         PPhoto := sigPhoto / sigTotal;
         PCompton := sigCompton / sigTotal;
         PPair := sigPair / sigTotal;
{This part selects which interaction took place.}
         x := SimpleRandom(idumconst);
         if x <= PCompton then   begin
            AngleDensity(E, subsum, cosines);
            AngleBin := WhatAngleBin(subsum);
            cosScatt := cosines[Anglebin];
            calcEscatterPhoton(E, cosScatt, morecollisions, Efinal);
          end
         else if x <= (PCompton + PPair) then   begin
            morecollisions := false;
            {writeln('absorbed - pair');}
            absorb := absorb + 1;
          end
         else  begin
            morecollisions := false;
            absorb := absorb + 1;
            {writeln('absorbed - photoelectric');}
          end;
     end;{with allSigma}
   end;{photonPath}

   procedure GaussianAngularDistr (distance, E: real;
                var thisMaterial: MaterialType;
                var thisSource: SourceType;
                var cosScatt: real);
      var
         particlez: integer;
         Eo, pc, mc2, beta, RMSangle, theta1, theta, thetarad: real;
         term1, term2, term3: real;
   begin
      if thisSource = electron then
         mc2 := mec2
      else if thisSource = proton then
         mc2 := mpc2
      else if thisSource = muon then
         mc2 := mmc2;
      particlez := 1;
      with thisMaterial do begin
         Eo := E + mc2;
         pc := sqrt(sqr(Eo) - sqr(mc2));
         beta := pc / Eo;
         term1 := particlez * (20.0 / (pc * beta));
         term2 := sqrt(distance / Lrad[1]);
         term3 := 1.0 + log10(abs(distance) / Lrad[1]) / 9.0;
         RMSangle := term1 * term2 * term3;
         theta := RMSangle * sqrt(-2 * ln(1 - SimpleRandom(idumconst))) * cos(2 * pi * simpleRandom(idumconst));
         cosScatt := cos(theta);
       end;{with thisMaterial}
   end;{GaussianAngularDistr}

   procedure PhotonSigma (var Rad: RadType;
                var thisMaterial: MaterialType;
                viewsnum: integer);
      var
         j, k, num: integer;
         E, logE, CompCount, PairCount, PhotoCount, max: real;
         PPhoto, PCompton, PPair: real;
         interp, multi: boolean;
         r: real;
         logEarray, SigmaTheoryPhoto, SigmaTheoryCompton, SigmaTheoryPair, SigmaTheoryTotal: DVector;
         allSigma: CrossSectionType;
   begin
{    new(logEarray);}
      logEarray.init(59);
{new(SigmaTheoryPhoto);}
      SigmaTheoryPhoto.init(59);
{    new(SigmaTheoryCompton);}
      SigmaTheoryCompton.init(59);
{    new(SigmaTheoryPair);}
      SigmaTheoryPair.init(59);
{    new(SigmaTheoryTotal);}
      SigmaTheoryTotal.init(59);
      {if Rad.materialStr = 'multiple' then }
         multi := true;
      {else
         multi := false;}
      with thisMaterial do begin
         for j := 1 to 59 do begin {energy step}
            E := EnergyPairArray[1, j];
            logE := log10(E);
            logEarray.put(j, logE);
            PhotoCount := 0;
            CompCount := 0;
            PairCount := 0;
            interp := false;
            CalcSigCompton(E, thisMaterial, allSigma);
            CalcSigPhoto(E, thisMaterial, allSigma);
            CalcSigPair(E, j, EnergyPairArray, interp, allSigma);

            with allSigma do  begin
               sigTotal := sigCompton + sigPhoto + sigPair;{b/atom}
               if sigPhoto<1e-10 then
                  sigPhoto:=1e-10;
               SigmaTheoryPhoto.Put(j, log10(sigPhoto));
               SigmaTheoryCompton.Put(j, log10(sigCompton));
               if sigPair<1e-10 then
                  sigPair:=1e-10;
               SigmaTheoryPair.Put(j, log10(sigPair));
                     SigmaTheoryTotal.Put(j, log10(sigTotal));
          end;{with allSigma}
       end;{for j}
      if viewsnum = 1 then
         begin
            num := 1;
            InitializePlotSigma(Rad, thisMaterial, max, multi, sigmatheorytotal);
         end
      else if viewsnum = 5 then
         num := 2;
       PlotSigmaTheory(logEarray, SigmaTheoryPhoto, SigmaTheoryCompton,
                            SigmaTheoryPair, SigmaTheoryTotal, thisMaterial,
                            multi, num, max);
       end;{with thisMaterial}
      logEarray.free;
      SigmaTheoryPhoto.free;
      SigmaTheoryCompton.free;
      SigmaTheoryPair.free;
      SigmaTheoryTotal.free;
{dispose(logEarray);}
{dispose(SigmaTheoryPhoto);}
{dispose(SigmaTheoryCompton);}
{dispose(SigmaTheoryPair);}
{dispose(SigmaTheoryTotal);}
   end;{PhotonSigma}

{S main}

   procedure HandleRadHotKeys (key: byte;
                   var Rad: RadType;
                   var esckey: boolean);
   forward;

   procedure UpDateData (j, k, viewsnum: integer;
               var thisSource: sourcetype;
               var thisMaterial: MaterialType;
               var Rad: RadType;
               thick: real);
      var
         MCtogo: integer;
   begin
      SelectViewPort(7);
      SelectScale(7);
      EraseXY(MapX(8.5), MapY(5.5), 10, 1, DarkGray);
      EraseXY(MapX(5.0), MapY(5.3), 10, 1, DarkGray);
      EraseXY(MapX(6.3), MapY(5.3), 5, 1, DarkGray);
      MCtogo := Rad.MonteCarlo - j;
      SetColor(whitte);
      OutTextXY(MapX(8.8), MapY(5.5), NumStr(MCtogo, 6, 0));
      OutTextXY(MapX(5.0), MapY(5.3), NumStr(thick, 6,4));
      if rad.range then
         OutTextXY(MapX(6.3), MapY(5.3), NumStr(k, 3, 0));
      PrintData(viewsnum, j, thisSource, thisMaterial);
   end;{UpDateData}

   procedure ZeroPointVector (lastcollision: integer;
                 var PointVector: PointVectorType);
      var
         i: integer;
   begin
      for i := 1 to lastcollision do
         begin
            PointVector[i].x := 0.0;
            PointVector[i].y := 0.0;
            PointVector[i].z := 0.0;
         end;{for i}
   end;{ZeroPointVector}

   procedure calcAtten (Rad: RadType;
               thismaterial: MaterialType;
               var atten: real);
     var
       i, index: integer;
       leavebool, interp: boolean;
       sigma: crosssectionType;
   begin
      CalcSigCompton(Rad.Energy, thisMaterial, Sigma);
      CalcSigPhoto(Rad.Energy, thisMaterial, Sigma);
      with thismaterial do begin
         if Rad.Energy > 1.5 then begin
            i := 1;
            leavebool := false;
            while (i <= 59) and (not leavebool) do begin
               if (EnergyPairArray[1, i] - Rad.Energy) >= 0 then begin
                  index := i;
                  leavebool := true;
                end;{if}
               if i = 59 then begin
                  message('Energy too big, it has been set to Emax = 100GeV');
                  index := 59;
                end;{if i=59}
               inc(i);
             end;{for i}
            interp := true;
            CalcSigPair(Rad.Energy, index, EnergyPairArray, interp, Sigma);
          end
         else
         Sigma.sigpair := 0.0;
         with sigma do
            atten:= (sigphoto + sigcompton + sigpair) * Neff * 1e-24;{cm-1}
      end;{thismaterial}
   end;{calcAtten}

   procedure PhotonRangeCalc (var Rad: RadType;
              var thisMaterial: MaterialType;
              viewsnum: integer);
      var
         i, j, k, collisions, lastcollision, MCtogo, degree, AngleBin: integer;
         scalefactor, atten: real;
         unitStr: Str25;
         morecollisions, esckey: boolean;
         lastPt, thisPt: MyPointType;
         lastomega, thisomega: OmegaType;
         E, Einitial, Efinal, cosScatt, zeta, distance, thick: real;
         allSigma: CrossSectionType;
         thissource: sourcetype;
         PointVector: PointVectorType;
         deltaT, TransDistr, AngleDistr, ETrans: Dvector;
         keybd: char;
         key: byte;

      procedure CollisionLoop;
      begin
         collisions := collisions + 1;
         CheckforEvents;
         if RadHotKeys.Pressed(key) then
            HandleRadHotKeys(key, Rad, esckey);
         if event.keypressed then
            begin
               beep;
               esckey := (event.ReadKey = chr(27));{look for escape key}
            end;
         if (not esckey) then begin
            lastPt := thisPt;
            lastomega := thisomega;
            NextCollision(j, collisions, viewsnum, E, thick, thisMaterial, thissource,
                    lastPt, lastomega, morecollisions, thisPt, allSigma,
                    zeta, distance, ETrans);
            if morecollisions then
               photonPath(collisions, E, allSigma, morecollisions, cosScatt,
                              Efinal, AngleBin);
 {if no morecollisions and collisions = 1 then anglebin is undefined!!!}
            if (collisions = 1) and (morecollisions) then
               AngleDistr.Put(AngleBin, AngleDistr.value(AngleBin) + 1); {Distribution of the first collision}
            PointVector[collisions] := lastPt;
            if not morecollisions then
               PointVector[collisions + 1] := thisPt;
            PlotPath(lastPt, thisPt, viewsnum, thisSource, scalefactor);
            E := Efinal;
            if morecollisions then
               DirectionAfterScatt(lastomega, cosScatt, thisomega);
        end;{if not esckey}
    end;{CollisionLoop}

   begin
{    new(AngleDistr);}
      AngleDistr.init(180);
      AngleDistr.fill(0.0);
      ETrans.init(Rad.MonteCarlo);
      deltaT.init(11);
      TransDistr.init(11);
      calcAtten(Rad, thismaterial, atten);
      thissource := photon;
      esckey := false;
      InitializeRangePlot(Rad, thisMaterial, atten, deltaT);
      InitPlotAngle(Rad);
      with Rad do begin
         k:= 1;
         while (k <= 11) and (not esckey) do begin
            thick:= deltaT.value(k);
            InitializePlotPath(thick, Rad, thisMaterial, thisSource, viewsnum, scalefactor, unitStr);
            PrintExpInfo(Rad, thick, scalefactor, unitStr);
            transmit := 0;
            reflect := 0;
            absorb := 0;
            out := 0;
            MCtogo := MonteCarlo;
            SetColor(whitte);
            SelectViewPort(7);
            SelectScale(7);
            EraseXY(MapX(8.5), MapY(5.5), 10, 1, DarkGray);
            OutTextXY(MapX(8.8), MapY(5.5), NumStr(MCtogo, 6, 0));
            j := 1;
            while (j <= MonteCarlo) and (not esckey) do begin
               UpDateData(j, k, viewsnum, thisSource, thisMaterial, Rad, thick);
               InitialRadValues(Rad, thisPt, thisomega, Einitial);
               SelectViewPort(viewsnum);
               SetColor(blakk);
               SelectScale(1);
               PlotLine(-0.1*scalefactor*thick, 0.0, 0.0, 0.0);
               E := Einitial;
               morecollisions := true;
               collisions := 0;
               while (morecollisions) and (not esckey) do
                  CollisionLoop;
               lastcollision := collisions + 1;
               setcolor(yellow);
               SelectViewPort(7);
               SelectScale(7);
               if viewsnum = 1 then
                  OutTextXY(MapX(4.3), MapY(2.3), numStr(atten, 6, 3))
               else
                  OutTextXY(MapX(8.0), MapY(2.3), numStr(atten, 6, 3));
               inc(j);
               SelectViewPort(viewsnum);
               SetColor(photonColor);
               SelectScale(1);
               PlotLine(-0.1*scalefactor*thick, 0.0, 0.0, 0.0);
               RedrawPath(viewsnum, lastcollision, thisSource, PointVector, scalefactor);
               RedrawPath(viewsnum, lastcollision, thisSource, PointVector, scalefactor);
               if j <> MonteCarlo then
                  ZeroPointVector(lastcollision, PointVector);
             end;{for j}
            RedrawPath(viewsnum, lastcollision, thisSource, PointVector, scalefactor);
            TransDistr.put(k, transmit);
            inc(k);
            PlotAngle(Rad, AngleDistr,lastcollision,viewsnum);
            PlotRangePoints(Rad, atten, TransDistr, deltaT);
         end;{while k<=11}
       end;{with Rad}
      if (not esckey) then
         PlotRange(Rad, atten, TransDistr, deltaT);
      if esckey then MainHotKeys.display;
      TransDistr.free;
      deltaT.free;
      AngleDistr.free;
      ETrans.free;
   end;{PhotonRangeCalc}

   procedure PathCalcPhoton (var Rad: RadType;
              var thisMaterial: MaterialType;
              viewsnum: integer);
      var
         i, j, k, collisions, collisionAv, lastcollision, MCtogo, degree, AngleBin: integer;
         scalefactor, atten: real;
         unitStr: Str25;
         morecollisions, esckey: boolean;
         lastPt, thisPt: MyPointType;
         lastomega, thisomega: OmegaType;
         E, Einitial, Efinal, cosScatt, zeta, distance: real;
         allSigma: CrossSectionType;
         thissource: sourcetype;
         PointVector: PointVectorType;
         AngleDistr, ETrans: Dvector;
         keybd: char;
         key: byte;

      procedure CollisionLoop;
      begin
         collisions := collisions + 1;
         CheckforEvents;
         if RadHotKeys.Pressed(key) then
            HandleRadHotKeys(key, Rad, esckey);
         if event.keypressed then
            begin
               beep;
               esckey := (event.ReadKey = chr(27));{look for escape key}
            end;
         if (not esckey) then begin
            lastPt := thisPt;
            lastomega := thisomega;
            NextCollision(j, collisions, viewsnum, E, Rad.thickness, thisMaterial, thissource,
                         lastPt, lastomega, morecollisions, thisPt, allSigma,
                         zeta, distance, ETrans);
            if morecollisions then
               photonPath(collisions, E, allSigma, morecollisions, cosScatt,
                              Efinal, AngleBin);
 {if no morecollisions and collisions = 1 then anglebin is undefined!!!}
            if (collisions = 1) and (morecollisions) then
               AngleDistr.Put(AngleBin, AngleDistr.value(AngleBin) + 1); {Distribution of the first collision}
            PointVector[collisions] := lastPt;
            if not morecollisions then
               PointVector[collisions + 1] := thisPt;
            PlotPath(lastPt, thisPt, viewsnum, thisSource, scalefactor);
            E := Efinal;
            if morecollisions then
               DirectionAfterScatt(lastomega, cosScatt, thisomega);
        end;{if not esckey}
      end;{CollisionLoop}

   begin
{    new(AngleDistr);}
      AngleDistr.init(180);
      AngleDistr.fill(0.0);
      ETrans.init(Rad.MonteCarlo);
      thissource := photon;
      calcAtten(Rad, thismaterial, atten);
      InitializePlotPath(rad.thickness, Rad, thisMaterial, thisSource, viewsnum, scalefactor, unitStr);
      PrintExpInfo(Rad, rad.thickness, scalefactor, unitStr);
      if not(rad.materialstr = 'multiple') then
         PhotonSigma(Rad, thisMaterial, viewsnum);
      InitPlotAngle(Rad);
      esckey := false;
      transmit := 0;
      reflect := 0;
      absorb := 0;
      out := 0;
      with Rad do begin
         MCtogo := MonteCarlo;
         SetColor(whitte);
         SelectViewPort(7);
         SelectScale(7);
         EraseXY(MapX(8.5), MapY(5.5), 10, 1, DarkGray);
         EraseXY(MapX(8.5), MapY(5.3), 10, 1, DarkGray);
         OutTextXY(MapX(8.8), MapY(5.3), NumStr(MCtogo, 6, 0));
         j := 1;
         CollisionAv:=0;
         while (j <= MonteCarlo) and (not esckey) do begin
            UpDateData(j, j, viewsnum, thisSource, thisMaterial, Rad, rad.thickness);
            InitialRadValues(Rad, thisPt, thisomega, Einitial);
            SelectViewPort(viewsnum);
            SetColor(blakk);
            SelectScale(1);
            PlotLine(-0.1*scalefactor*rad.thickness, 0.0, 0.0, 0.0);
            E := Einitial;
            morecollisions := true;
            collisions := 0;
            while (morecollisions) and (not esckey) do
               CollisionLoop;
            lastcollision := collisions + 1;
            SelectViewPort(7);
            SelectScale(7);
            setcolor(yellow);
            if viewsnum = 1 then
               OutTextXY(MapX(4.3), MapY(2.3), numStr(atten, 6, 3))
            else
               OutTextXY(MapX(8.0), MapY(2.3), numStr(atten, 6, 3));
            inc(j);
            collisionAv:=collisionAv+lastcollision;
            SelectViewPort(viewsnum);
            SetColor(photonColor);
            SelectScale(1);
            PlotLine(-0.1*scalefactor*rad.thickness, 0.0, 0.0, 0.0);
            RedrawPath(viewsnum, lastcollision, thisSource, PointVector, scalefactor);
            if j <> MonteCarlo then
               ZeroPointVector(lastcollision, PointVector);
          end;{for j}
         RedrawPath(viewsnum, lastcollision, thisSource, PointVector, scalefactor);
         collisionAv:=round(collisionAv / montecarlo);
       end;{with Rad}
      UpDateData(j, j, viewsnum, thisSource, thisMaterial, Rad, rad.thickness);
      if (not esckey) then begin
         PlotAngle(Rad, AngleDistr, collisionAv, viewsnum);
      end;
      if esckey then MainHotKeys.display;
      AngleDistr.free;
      ETrans.free;
   end;{PathCalcPhoton}

   {S Charge}

   procedure PlotEtrans(rad: radType;
                thismaterial: materialType;
                thick, maxfreq: real;
                maxfreqIndex, viewsnum: integer;
                EDistr, frequency, EDistr1, frequency1: dvector;
                maxfreq1: real);
   begin
      with Rad do begin
         with thismaterial do begin
            if range then begin
               SelectViewPort(0);
               EraseXY(314, 82, 4, 21, blakk);
               EraseXY(320, 246, 40, 1, blakk);
             end;
            OpenViewPort(2);
            SetColor(whitte);
            putLabel(bottom, 'E, MeV');
            putLabel(left, 'Rel. Freq. per MeV');
            SetColor(lightMagenta);
            autoscaling(9, EDistr, frequency);
            SelectScale(9);
            PlotDVectors(EDistr, frequency, 1, 101);
            SetColor(whitte);
            if range then begin
               with graphscales[9] do begin
                  OutTextXY(MapX(xmin) + 2, MapY(maxfreq), concat('thick=', numStr(thick,6,4),' cm'));
                  OutTextXY(MapX(xmin) + 2, MapY(maxfreq) + 9, concat('Eprob=', numstr(eprob,6,4),' MeV'));
                  OutTextXY(MapX(xmin) + 2, MapY(maxfreq) + 18, concat('Eav  =', numstr(eav,6,4),' MeV'));
                end;
             end
            else begin
               SelectViewPort(3);
               SelectScale(7);
               if viewsnum = 1 then begin
                  OutTextXY(MapX(1.2), MapY(3.3), 'E probable');
                  OutTextXY(MapX(1.2), MapY(2.3), 'E average');
                  SetColor(yellow);
                  OutTextXY(MapX(4.3), MapY(3.3), NumStr(Eprob, 6, 4));
                  OutTextXY(MapX(4.3), MapY(2.3), NumStr(Eav, 6, 4));
                end
               else begin
                  SetColor(yellow);
                  OutTextXY(MapX(8.0), MapY(3.3), NumStr(Eprob, 6, 4));
                  OutTextXY(MapX(8.0), MapY(2.3), NumStr(Eav, 6, 4));
               end;
            end;
           end;{with thismaterial}
      end;{with rad}
   end;{PlotEtrans}

   procedure ETransmit (thick: real;
               viewsnum: integer;
               Rad: RadType;
               thismaterial: MaterialType;
               ETrans: DVector;
           var frequency1, EDistr1: DVector;
           var maxfreq1: real);
     Var
      i, j, iEmin, iEmax, maxfreqIndex, ibeg, icount: integer;
      Emin, Emax, binsize, anglesize, maxfreq, temp, ticksx, ticksy: real;
      binned, dothis: boolean;
      frequency, EDistr: DVector;

   begin
     if viewsnum = 1 then begin
        OpenViewPort(2);
        setcolor(lightmagenta);
      end
     else begin
        SelectViewPort(2);
        setcolor(lightblue);
      end;
     if transmit = 0 then
        PutLabel(inside, 'No Transmission')
     else begin
        frequency.init(101);
        EDistr.init(101);
        frequency.fill(0.0);
        Emin := ETrans.value(1);
        Emax := ETrans.value(1);
        for i:= 2 to transmit do
          If ETrans.value(i) < Emin then
            Emin := ETrans.value(i)
          else if ETrans.value(i) >= Emax then
            Emax := ETrans.value(i);
        binsize := (Emax - Emin) / 100.0;
        EDistr.put(1, Emin);
        for i:= 2 to 101 do
          EDistr.put(i, EDistr.value(i-1) + binsize);
        for j:= 1 to transmit do begin
          i:=1;
          binned:= false;
          while (i<=100) and (not binned) do begin
            if (ETrans.value(j) >= EDistr.value(i))
               and (ETrans.value(j) < EDistr.value(i+1)) then begin
              binned := true;
              frequency.put(i, frequency.value(i) + 1);
             end;
            inc(i);
           end;{while}
         end;{for j}
        Eav:= frequency.value(1) * EDistr.value(1);
        maxfreqIndex:= 1;
        maxfreq := frequency.value(1);
        for i:= 2 to 101 do begin
          if frequency.value(i) >= maxfreq then begin
            maxfreq := frequency.value(i);
            maxfreqIndex:= i;
           end;
          Eav:= Eav + frequency.value(i) * EDistr.value(i);
         end;
        Eav:= Eav/transmit;
        Eprob:= EDistr.value(maxfreqIndex);
        for i:= 1 to 101 do
           frequency.put(i, frequency.value(i) / transmit);
        maxfreq := maxfreq / transmit;
        PlotEtrans(rad, thismaterial, thick, maxfreq, maxfreqIndex, viewsnum,
                   EDistr, frequency, EDistr1, frequency1, maxfreq1);
        EDistr.free;
        frequency.free;
      end;{else}
   end;{ETransmit}

   procedure dEdxCalcElectron (var Rad: RadType; var thismaterial: materialType; viewsnum: integer);
      const
         energycutoff = 0.005;{MeV}
         maxenergy = 1.00e5;{MeV}
         numbins = 36;
      var
         i, icutoff, num: integer;
         interval, binenergy, E, Elog, Eo, sigmaRad, sigmabar: real;
         gamma, beta, beta2, dEdx: real;
         term1, term2, term3, term4, term5, temp: real;
         multi: boolean;
         logE, logdEdx, logdEdxtot: DVector;
   begin
{    new(logE);}
{    new(logdEdx);}
{    new(logdEdxtot);}
      logE.init(numbins);
      logdEdx.init(numbins);
      logdEdxtot.init(numbins);
      if (Rad.materialStr = 'multiple'){ or (Rad.sourceStr = 'multiple')} then
         multi := true
      else
         multi := false;
      if viewsnum = 1 then
         begin
            num := 1;
            InitializePlotdEdxElectron(Rad, thisMaterial);
         end
      else if viewsnum = 5 then
         num := 2;
      with thismaterial do
         begin
            interval := log10(maxenergy / energycutoff);{interval := maxenergy - energycutoff;}
            binenergy := interval / numbins;
            Elog := Log10(energycutoff) - binenergy;{E := energycutoff - binenergy;}
            icutoff := 1;
            for i := 1 to numbins do
               begin
                  Elog := Elog + binenergy;{E := E + binenergy;}
                  E := pwr(10, Elog);
                  gamma := (E / mec2) + 1;
                  beta2 := 1 - (1 / sqr(gamma));
                  beta := sqrt(beta2);
                  term1 := 2 * pi * Na * densityeff * sqr(esqr) * Zeff / (mec2 * beta2 * Meff);
                  term2 := mec2 * beta2 / (2 * sqr(IaveEff) * (1 - beta2));
                  term3 := (2 * sqrt(1 - beta2) - 1 + beta2) * ln(2);
                  term4 := 1 - beta2;
                  term5 := sqr(1 - sqrt(1 - beta2)) / 8;
                  dEdx := term1 * (ln(term2 * E) - term3 + term4 + term5)  / densityeff;  {MeVcm^2/g}
                  logdEdxtot.Put(i, dEdx);
                  logE.Put(i, log10(E));
                  logdEdx.Put(i, log10(abs(dEdx)));
               end;{for i}
{if not multi then}
            num := lightred;
            PlotdEdxElectronTheory(logE, logdEdx, icutoff, numbins, num, thisMaterial);
{dEdx due to Bremsstrahlung}
            icutoff := round(log10(1 / energycutoff) / binenergy);{icutoff := round((1.00 - energycutoff) / binenergy);}
            Elog := -binenergy;{E := 1.00 - binenergy;}
            for i := icutoff to numbins do
               begin
                  Elog := Elog + binenergy;{E := E + binenergy;}
                  E := pwr(10, Elog);
                  Eo := E + mec2;
                  calcsigmaRad(Rad, Eo, viewsnum, thisMaterial, dEdx);
                  logdEdxtot.Put(i, logdEdxtot.value(i) + dEdx);
                  logdEdx.Put(i, log10(dEdx));
               end;{for i}
         end;{with thismaterial}
{if not multi then}
      num := lightblue;
      PlotdEdxElectronTheory(logE, logdEdx, icutoff, numbins, num, thisMaterial);
      for i := 1 to numbins do
         logdEdxtot.Put(i, log10(logdEdxtot.value(i)));
      num := whitte;
      PlotdEdxElectronTheory(logE, logdEdxtot, 1, numbins, num, thisMaterial);
      logE.free;
      logdEdx.free;
      logdEdxtot.free;
{dispose(logE);}
{dispose(logdEdx);}
{dispose(logdEdxtot);}
   end;{dEdxCalcElectron}

   procedure TheoElectronRange (Rad: RadType;
                thisMaterial: MaterialType;
                var RangeDist: dVector);
     var
       ExpRange, n: real;
   begin
      if Rad.Energy < 2.0 then begin
         n:= 1.265 - 0.0954 * ln(Rad.Energy);
         ExpRange := 412 * pwr(Rad.Energy, n);{mg/cm2}
       end
      else
         ExpRange := 530 * Rad.Energy - 106;{mg/cm2}
      ExpRange := ExpRange / (thisMaterial.densityeff * 1e3);{cm}
      RangeDist.put(1, ExpRange * 0.05);
      RangeDist.put(2, ExpRange * 0.10);
      RangeDist.put(3, ExpRange * 0.20);
      RangeDist.put(4, ExpRange * 0.30);
      RangeDist.put(5, ExpRange * 0.40);
      RangeDist.put(6, ExpRange * 0.50);
      RangeDist.put(7, ExpRange * 0.65);
      RangeDist.put(8, ExpRange * 0.8);
      RangeDist.put(9, ExpRange * 0.95);
      RangeDist.put(10, ExpRange * 1.0);
      RangeDist.put(11, ExpRange * 1.1);
   end;{TheoElectronRange}

   procedure ElectronRangeCalc (var Rad: RadType;
               var thisMaterial: MaterialType;
               viewsnum: integer);
      var
         j, k, collisions, lastcollision, MCtogo: integer;
         scalefactor: real;
         morecollisions, esckey: boolean;
         unitStr: Str25;
         lastPt, thisPt: MyPointType;
         lastomega, thisomega: OmegaType;
         E, Einitial, Efinal, cosScatt, zeta, distance, thick, maxfreq1: real;
         allSigma: CrossSectionType;
         thissource: sourceType;
         PointVector: PointVectorType;
         ETrans, RangeFreq, ReflectFreq, RangeDist, frequency1, Edistr1: DVector;
         keybd: char;
         key: byte;

   procedure CollisionLoop;
   begin
     collisions := collisions + 1;
     if collisions > maxcollisions then
        announce('Too many Collisions');
     CheckforEvents;
     if RadHotKeys.Pressed(key) then
       HandleRadHotKeys(key, Rad, esckey);
     if event.keypressed then begin
       beep;
       esckey := (event.ReadKey = chr(27));   {look for escape key}
      end;
     if (not esckey) then begin
       lastPt := thisPt;
       lastomega := thisomega;
       NextCollision(j,collisions, viewsnum, E, thick, thisMaterial, thissource, lastPt,
                     lastomega, morecollisions, thisPt, allSigma, zeta,
                     distance, ETrans);
       if morecollisions then begin
         electronPath(E, zeta, distance, thick, viewsnum, thismaterial,
                     morecollisions, cosScatt, Efinal);
        end;
       PointVector[collisions] := lastPt;
       if not morecollisions then
         PointVector[collisions + 1] := thisPt;
       PlotPath(lastPt, thisPt, viewsnum, thisSource, scalefactor);
{GaussianAngularDistr(distance, E, thisMaterial, thissource, cosScatt);}
{calcEscatterElectron(E, thismaterial, distance, morecollisions, Efinal);}
       E := Efinal;
       if morecollisions then
         DirectionAfterScatt(lastomega, cosScatt, thisomega);
      end;{if not esckey}
   end;{CollisionLoop}

   begin
      ETrans.init(Rad.MonteCarlo);
      RangeFreq.init(11);
      ReflectFreq.init(11);
      RangeDist.init(11);
      frequency1.init(101);
      EDistr1.init(101);
      TheoElectronRange(Rad, thisMaterial, RangeDist);
      InitializeChargeRangePlot(Rad, thisMaterial, RangeDist);
      thissource := electron;
      esckey := false;
      with Rad do begin
         k := 1;
         while (k <= 11) and (not esckey) do begin
            thick := RangeDist.value(k);
            InitializePlotPath(thick, Rad, thisMaterial, thissource, viewsnum, scalefactor, unitStr);
            PrintExpInfo(Rad, thick, scalefactor, unitStr);
            transmit := 0;
            reflect := 0;
            absorb := 0;
            out := 0;
            MCtogo := MonteCarlo;
            SetColor(whitte);
            SelectViewPort(7);
            SelectScale(7);
            EraseXY(MapX(8.5), MapY(5.3), 10, 1, DarkGray);
            OutTextXY(MapX(8.5), MapY(5.3), NumStr(MCtogo, 6, 0));
            j := 1;
            while (j <= MonteCarlo) and (not esckey) do begin
               UpDateData(j, k, viewsnum, thisSource, thisMaterial, Rad, thick);
               InitialRadValues(Rad, thisPt, thisomega, Einitial);
               SelectViewPort(viewsnum);
               SetColor(blakk);
               SelectScale(1);
               PlotLine(-0.1*scalefactor*thick, 0.0, 0.0, 0.0);
               E := Einitial;
               morecollisions := true;
               collisions := 0;
               while (morecollisions) and (not esckey) do
                  CollisionLoop;
               lastcollision := collisions + 1;
               inc(j);
               SelectViewPort(viewsnum);
               SetColor(electroncolor);
               SelectScale(1);
               PlotLine(-0.1*scalefactor*thick, 0.0, 0.0, 0.0);
               RedrawPath(viewsnum, lastcollision, thisSource, PointVector, scalefactor);
               if j <> MonteCarlo then
                  ZeroPointVector(lastcollision, PointVector);
             end;{for j}
            RedrawPath(viewsnum, lastcollision, thisSource, PointVector, scalefactor);
            ETransmit(thick, viewsnum, Rad, thismaterial, ETrans, frequency1, EDistr1, maxfreq1);
            RangeFreq.put(k, transmit / MonteCarlo);
            ReflectFreq.put(k, reflect / MonteCarlo);
            PlotChargeRangePoints(k, RangeFreq, ReflectFreq, RangeDist);
            inc(k);
          end;{while k <= 11}
       end;{with Rad}
      if (not esckey) then begin
        PlotChargeRange(RangeFreq, ReflectFreq, RangeDist);
        {dEdxCalcElectron(Rad, thismaterial, viewsnum);}
      end;
      UpDateData(j, k, viewsnum, thisSource, thisMaterial, Rad, thick);
      if esckey then MainHotKeys.display;
      ETrans.free;
      RangeDist.free;
      RangeFreq.free;
      ReflectFreq.free;
      frequency1.free;
      EDistr1.free;
   end;{ElectronRangeCalc}

   procedure PathCalcElectron (var Rad: RadType;
               var thisMaterial: MaterialType;
               viewsnum: integer);
      var
         j, collisions, lastcollision, MCtogo: integer;
         scalefactor: real;
         morecollisions, esckey: boolean;
         unitStr: Str25;
         lastPt, thisPt: MyPointType;
         lastomega, thisomega: OmegaType;
         E, Einitial, Efinal, cosScatt, zeta, distance, maxfreq1: real;
         allSigma: CrossSectionType;
         thissource: sourceType;
         PointVector: PointVectorType;
         ETrans, frequency1, EDistr1: DVector;
         keybd: char;
         key: byte;

   procedure CollisionLoop;
   begin
     collisions := collisions + 1;
     if collisions > maxcollisions then
        announce('too many collisions');
{    writeln('collisions', collisions);}
     CheckforEvents;
     if RadHotKeys.Pressed(key) then
       HandleRadHotKeys(key, Rad, esckey);
     if event.keypressed then begin
       beep;
       esckey := (event.ReadKey = chr(27));   {look for escape key}
      end;
     if (not esckey) then begin
       lastPt := thisPt;
       lastomega := thisomega;
       NextCollision(j, collisions, viewsnum, E, Rad.thickness, thisMaterial, thissource, lastPt,
                     lastomega, morecollisions, thisPt, allSigma, zeta,
                     distance, ETrans);
       if morecollisions then
         electronPath(E, zeta, distance, Rad.thickness, viewsnum, thismaterial,
                     morecollisions, cosScatt, Efinal);
       PointVector[collisions] := lastPt;
       if not morecollisions then
         PointVector[collisions + 1] := thisPt;
       PlotPath(lastPt, thisPt, viewsnum, thisSource, scalefactor);
{GaussianAngularDistr(distance, E, thisMaterial, thissource, cosScatt);}
{calcEscatterElectron(E, thismaterial, distance, morecollisions, Efinal);}
       E := Efinal;
       if morecollisions then
         DirectionAfterScatt(lastomega, cosScatt, thisomega);
      end;{if not esckey}
   end;{CollisionLoop}

   begin
      ETrans.init(Rad.MonteCarlo);
      frequency1.init(101);
      Edistr1.init(101);
      thissource := electron;
      InitializePlotPath(rad.thickness, Rad, thisMaterial, thisSource, viewsnum, scalefactor, unitStr);
      PrintExpInfo(Rad, rad.thickness, scalefactor, unitStr);
      if (rad.materialstr<>'multiple') then
         dEdxCalcElectron(Rad, thismaterial, viewsnum);
      esckey := false;
      transmit := 0;
      reflect := 0;
      absorb := 0;
      out := 0;
      with Rad do begin
        MCtogo := MonteCarlo;
        SetColor(whitte);
        SelectViewPort(7);
        SelectScale(7);
        EraseXY(MapX(8.5), MapY(5.3), 10, 1, DarkGray);
        OutTextXY(MapX(8.5), MapY(5.3), NumStr(MCtogo, 6, 0));
        j := 1;
        while (j <= MonteCarlo) and (not esckey) do begin
          UpDateData(j, j, viewsnum, thisSource, thisMaterial, Rad, rad.thickness);
          InitialRadValues(Rad, thisPt, thisomega, Einitial);
          SelectViewPort(viewsnum);
          SetColor(blakk);
          SelectScale(1);
          PlotLine(-0.1*scalefactor*rad.thickness, 0.0, 0.0, 0.0);
          E := Einitial;
          morecollisions := true;
          collisions := 0;
          while (morecollisions) and (not esckey) do
            CollisionLoop;
          lastcollision := collisions + 1;
          inc(j);
          SelectViewPort(viewsnum);
          SetColor(electroncolor);
          SelectScale(1);
          PlotLine(-0.1*scalefactor*rad.thickness, 0.0, 0.0, 0.0);
          RedrawPath(viewsnum, lastcollision, thisSource, PointVector, scalefactor);
          if j <> MonteCarlo then
            ZeroPointVector(lastcollision, PointVector);
           end;{for j}
          RedrawPath(viewsnum, lastcollision, thisSource, PointVector, scalefactor);
         end;{with Rad}
      if (not esckey) then
        ETransmit(Rad.thickness, viewsnum, Rad, thismaterial, ETrans, frequency1, EDistr1, maxfreq1);
      UpDateData(j, j, viewsnum, thisSource, thisMaterial, Rad, rad.thickness);
      if esckey then MainHotKeys.display;
      ETrans.free;
      frequency1.free;
      Edistr1.free;
   end;{PathCalcElectron}

   procedure dEdxCalcHeavy (var Rad: RadType; var thismaterial: materialType; var thisSource: SourceType;{}
                           viewsnum: integer);
      const
         energycutoff = 0.6;{MeV}
         maxenergy = 1.00e5;{MeV}
         numbins = 36;
      var
         i, icutoff, num: integer;
         interval, binenergy, E, Elog, Eo, mc2: real;{, sigmaRad, sigmabar}
         gamma, gamma2, beta, beta2, dEdx, term1, term2, tau: real;
         multi: boolean;
         logE, logdEdx: DVector;
   begin
{    new(logE);}
{    new(logdEdx);}
      logE.init(numbins);
      logdEdx.init(numbins);
      if (Rad.materialStr = 'multiple') {or (Rad.sourceStr = 'multiple')} then
         multi := true
      else
         multi := false;
      if viewsnum = 1 then
         begin
            num := 1;
            InitializePlotdEdxHeavy(Rad, thisMaterial, thisSource);
         end
      else if viewsnum = 5 then
         num := 2;
      if thisSource = proton then
         mc2 := mpc2
      else if thisSource = muon then
         mc2 := mmc2;
      with thismaterial do
         begin
            interval := log10(maxenergy / energycutoff);
            binenergy := interval / numbins;
            Elog := Log10(energycutoff) - binenergy;
            icutoff := 1;
            for i := 1 to numbins do
               begin
                  Elog := Elog + binenergy;
                  E := pwr(10, Elog);
                  tau := E / mc2;
                  gamma := tau + 1;{ratio from special relativity}
                  gamma2 := sqr(gamma);
                  beta2 := 1 - (1 / gamma2);
                  beta := sqrt(beta2);
                  term1 := 4 * pi * Na * densityeff * sqr(re) * Zeff * mec2 / (beta2 * Meff);
                  term2 := 2 * mec2 * beta2 * gamma2 / IaveEff;
                  dEdx := term1 * (ln(term2) - beta2) / densityeff;{MeVcm2/g}
                  logE.Put(i, log10(E));
                  logdEdx.Put(i, log10(abs(dEdx)));
               end;{for i}
         end;{with thismaterial}
      PlotdEdxHeavyTheory(logE, logdEdx, icutoff, numbins, num, thisMaterial, thisSource);
      logE.free;
      logdEdx.free;
{dispose(logE);}
{dispose(logdEdx);}
   end;{dEdxCalcHeavy}

   procedure TheoHeavyRange (Rad: RadType;
                thissource:sourceType;
                thisMaterial: MaterialType;
                var RangeDist: dVector);
     var
       ExpRange, n, Rpair: real;
   begin
      Rpair:= 100 * pwr((Rad.Energy/9.3),1.8);{R for p in cm of air}
      with thismaterial do
         if thissource = proton then
            ExpRange := 3.2e-4 * sqrt(Meff) * Rpair / densityeff
         else
            ExpRange := 3.2e-4 * (mmc2/mpc2) * sqrt(Meff) * Rpair / densityeff;
      RangeDist.put(1, ExpRange * 0.05);
      RangeDist.put(2, ExpRange * 0.10);
      RangeDist.put(3, ExpRange * 0.20);
      RangeDist.put(4, ExpRange * 0.30);
      RangeDist.put(5, ExpRange * 0.40);
      RangeDist.put(6, ExpRange * 0.50);
      RangeDist.put(7, ExpRange * 0.65);
      RangeDist.put(8, ExpRange * 0.8);
      RangeDist.put(9, ExpRange * 0.95);
      RangeDist.put(10, ExpRange * 1.1);
      RangeDist.put(11, ExpRange * 1.2);
   end;{TheoHeavyRange}

   procedure HeavyRangeCalc (var Rad: RadType;
               var thisMaterial: MaterialType;
               thissource:sourceType;
               viewsnum: integer);
      var
         j, k, collisions, lastcollision, MCtogo: integer;
         scalefactor: real;
         morecollisions, esckey, wait: boolean;
         unitStr: Str25;
         lastPt, thisPt: MyPointType;
         lastomega, thisomega: OmegaType;
         E, Einitial, Efinal, cosScatt, zeta, distance, thick, maxfreq1: real;
         allSigma: CrossSectionType;
         PointVector: PointVectorType;
         ETrans, RangeFreq, ReflectFreq, RangeDist, frequency1, EDistr1: DVector;
         keybd: char;
         key: byte;

   procedure CollisionLoop;
   begin
     collisions := collisions + 1;
     repeat
        CheckforEvents;
        if RadHotKeys.Pressed(key) then
           HandleRadHotKeys(key, Rad, esckey);
        if (key= 3) or (key= 2) then wait:= false;
        if event.keypressed then begin
           beep;
           esckey := (event.ReadKey = chr(27));   {look for escape key}
         end;
     until (not wait) or esckey;
     if (not esckey) then begin
        lastPt := thisPt;
        lastomega := thisomega;
        NextCollisionHeavy(E,thick, viewsnum, Rad, thisMaterial,thissource, lastPt,
                         lastOmega, morecollisions, thisPt, Efinal, distance,
                         Etrans);
        PointVector[collisions] := lastPt;
        if not morecollisions then
           PointVector[collisions + 1] := thisPt;
        PlotPath(lastPt, thisPt, viewsnum, thisSource, scalefactor);
        GaussianAngularDistr(distance, E, thisMaterial, thissource, cosScatt);
        E := Efinal;
       if morecollisions then
         DirectionAfterScatt(lastomega, cosScatt, thisomega);
      end;{if not esckey}
   end;{CollisionLoop}

   begin
      ETrans.init(Rad.MonteCarlo);
      RangeFreq.init(11);
      ReflectFreq.init(11);
      RangeDist.init(11);
      frequency1.init(101);
      EDistr1.init(101);
      TheoHeavyRange(Rad, thissource, thisMaterial, RangeDist);
      InitializeChargeRangePlot(Rad, thisMaterial, RangeDist);
      esckey := false;
      with Rad do begin
         k := 1;
         while (k <= 11) and (not esckey) do begin
            thick := RangeDist.value(k);
            InitializePlotPath(thick, Rad, thisMaterial, thisSource, viewsnum, scalefactor, unitStr);
            PrintExpInfo(Rad, thick, scalefactor, unitStr);
            transmit := 0;
            reflect := 0;
            absorb := 0;
            out := 0;
            MCtogo := MonteCarlo;
            SetColor(whitte);
            SelectViewPort(7);
            SelectScale(7);
            EraseXY(MapX(8.5), MapY(5.3), 10, 1, DarkGray);
            OutTextXY(MapX(8.5), MapY(5.3), NumStr(MCtogo, 6, 0));
            j := 1;
            while (j <= MonteCarlo) and (not esckey) do begin
               UpDateData(j, k, viewsnum, thisSource, thisMaterial, Rad, thick);
               InitialRadValues(Rad, thisPt, thisomega, Einitial);
               SelectViewPort(viewsnum);
               SetColor(blakk);
               SelectScale(1);
               PlotLine(-0.1*scalefactor*thick, 0.0, 0.0, 0.0);
               E := Einitial;
               morecollisions := true;
               collisions := 0;
               while (morecollisions) and (not esckey) do
                  CollisionLoop;
               lastcollision := collisions + 1;
               inc(j);
               SelectViewPort(viewsnum);
               if thissource = proton then
                  SetColor(protoncolor)
               else
                  SetColor(muoncolor);
               SelectScale(1);
               PlotLine(-0.1*scalefactor*thick, 0.0, 0.0, 0.0);
               RedrawPath(viewsnum, lastcollision, thisSource, PointVector, scalefactor);
               if j <> MonteCarlo then
                  ZeroPointVector(lastcollision, PointVector);
             end;{for j}
            RedrawPath(viewsnum, lastcollision, thisSource, PointVector, scalefactor);
            RangeFreq.put(k, transmit / MonteCarlo);
            ReflectFreq.put(k, reflect / MonteCarlo);
            PlotChargeRangePoints(k, RangeFreq, ReflectFreq, RangeDist);
            ETransmit(thick, viewsnum, Rad, thismaterial, ETrans, frequency1, EDistr1, maxfreq1);
            inc(k);
          end;{while k <= 11}
       end;{with Rad}
      if (not esckey) then
        PlotChargeRange(RangeFreq, ReflectFreq, RangeDist);
      if esckey then MainHotKeys.display;
      UpDateData(j, k, viewsnum, thisSource, thisMaterial, Rad, thick);
      ETrans.free;
      RangeDist.free;
      RangeFreq.free;
      ReflectFreq.free;
      frequency1.free;
      Edistr1.free;
   end;{HeavyRangeCalc}

   procedure PathCalcHeavy (var Rad: RadType;
                var thisMaterial: MaterialType;
                var thisSource: SourceType;
                viewsnum: integer);
      var
         j, collisions, lastcollision, MCtogo: integer;
         scalefactor: real;
         unitStr: Str25;
         morecollisions,wait , esckey: boolean;
         lastPt, thisPt: MyPointType;
         lastomega, thisomega: OmegaType;
         E, Einitial, Efinal, cosScatt, zeta, distance, maxfreq1: real;
         allSigma: CrossSectionType;
         PointVector: PointVectorType;
         Etrans, frequency1, EDistr1: DVector;
         keybd: char;
         key: byte;
   procedure CollisionLoop;
   begin
      collisions := collisions + 1;
      repeat  {until not wait}
         CheckforEvents;
         if RadHotKeys.Pressed(key) then
            HandleRadHotKeys(key, Rad, esckey);
         if (key= 3) or (key= 2) then wait:= false;
         if event.keypressed then begin
            beep;
            esckey := (event.ReadKey = chr(27));   {look for escape key}
          end;
         until (not wait) or esckey;
         if (not esckey) then begin
            lastPt := thisPt;
            lastomega := thisomega;
            NextCollisionHeavy(E, Rad.thickness, viewsnum, Rad, thisMaterial,thissource, lastPt,
                         lastOmega, morecollisions, thisPt, Efinal, distance,
                         Etrans);
            PointVector[collisions] := lastPt;
            if not morecollisions then
               PointVector[collisions + 1] := thisPt;
            PlotPath(lastPt, thisPt, viewsnum, thisSource, scalefactor);
            GaussianAngularDistr(distance, E, thisMaterial, thissource, cosScatt);
            E := Efinal;
            if morecollisions then
            DirectionAfterScatt(lastomega, cosScatt, thisomega);
          end;{if not esckey}
   end;{CollisionLoop}

   begin
      ETrans.init(Rad.MonteCarlo);
      frequency1.init(101);
      EDistr1.init(101);
      InitializePlotPath(rad.thickness, Rad, thisMaterial, thisSource, viewsnum, scalefactor, unitStr);
      PrintExpInfo(Rad, rad.thickness, scalefactor, unitStr);
      if (rad.materialstr<>'multiple') then
         dEdxCalcHeavy(Rad, thismaterial, thisSource, viewsnum);
      esckey := false;
      wait:= false;
      transmit := 0;
      reflect := 0;
      absorb := 0;
      out :=0;
      with Rad do begin
         MCtogo := MonteCarlo;
         SetColor(whitte);
         SelectViewPort(7);
         SelectScale(7);
         EraseXY(MapX(8.5), MapY(5.3), 10, 1, DarkGray);
         OutTextXY(MapX(8.8), MapY(5.3), NumStr(MCtogo, 6, 0));
         j := 1;
         while (j <= MonteCarlo) and (not esckey) do begin
            UpDateData(j, j, viewsnum, thisSource, thisMaterial, Rad,rad.thickness);
            InitialRadValues(Rad, thisPt, thisomega, Einitial);
            SelectViewPort(viewsnum);
            SetColor(blakk);
            SelectScale(1);
            PlotLine(-0.1*scalefactor*rad.thickness, 0.0, 0.0, 0.0);
            E := Einitial;
            morecollisions := true;
            collisions := 0;
            while (morecollisions) and (not esckey) do
               CollisionLoop;
            if OneStepRad then wait:= true;
            lastcollision := collisions + 1;
            inc(j);
            SelectViewPort(viewsnum);
            if thissource = proton then
               SetColor(protoncolor)
            else
               SetColor(muoncolor);
            SelectScale(1);
            PlotLine(-0.1*scalefactor*rad.thickness, 0.0, 0.0, 0.0);
            RedrawPath(viewsnum, lastcollision, thisSource, PointVector, scalefactor);
            if j <> MonteCarlo then
               ZeroPointVector(lastcollision, PointVector);
          end;{for j}
         RedrawPath(viewsnum, lastcollision, thisSource, PointVector, scalefactor);
       end;{with Rad}
      if (not esckey) then
         ETransmit(Rad.thickness, viewsnum, Rad, thismaterial, ETrans, frequency1, EDistr1, maxfreq1);
      UpDateData(j, j, viewsnum, thisSource, thisMaterial, Rad,rad.thickness);
      if esckey then MainHotKeys.display;
      ETrans.free;
      frequency1.free;
      Edistr1.free;
   end;{PathCalcHeavy}

   procedure RadiationCalc (numviews: integer; var Rad: RadType);
      var
         j, k, viewsnum: integer;
         radiation: sourceType;
         thismaterial: materialType;
         thisSource: SourceType;
   begin
      with Rad do begin
         viewsnum := 1;
         j := 1;
         Choosematerial(j, Rad, thisMaterial);
         with thisMaterial do
            OpenViewPort(viewsnum);
            radiation := source[1];
            case radiation of
               proton: begin
                  thisSource := proton;
                  if Rad.range then
                        HeavyRangeCalc(Rad,thismaterial, thisSource, viewsnum)
                  else
                        pathCalcHeavy(Rad, thismaterial, thisSource, viewsnum);
                end;{case proton}
               muon: begin
                     thisSource := muon;
                     if Rad.range then
                        HeavyRangeCalc(Rad,thismaterial, thisSource, viewsnum)
                     else
                        pathCalcHeavy(Rad, thismaterial, thisSource, viewsnum);
                end;{case muon}
               electron:
                     if Rad.range then
                        ElectronRangeCalc(Rad, thismaterial, viewsnum)
                     else
                        pathCalcElectron(Rad, thismaterial, viewsnum);
               photon:
                     if Rad.range then
                        PhotonRangeCalc(Rad, thismaterial, viewsnum)
                     else
                        pathCalcPhoton(Rad, thismaterial, viewsnum);
                end;{case radiation}
       end;{with Rad}
   end;{RadiationCalc}


{S NaI}

{******************** NaI Procedures *****************}

   function NaIinput (var NaI: NaIType) : boolean;
      var
         noerror: boolean;
         NaIScreen: TInputScreen;
         tempReal: real;

   begin
      {noerror := true;}
      with NaIScreen do
         begin
            init;
            DefineInputport(0.25, 0.75, 0.3, 0.7);
            LoadLine('   EXPERIMENTAL SETUP INFORMATION       ');
            LoadLine('');
            LoadLine('Detector-Source Distance D = {      }cm');
            LoadLine('Source Energy =              {      }MeV ');
            LoadLine('Number of Source Photons =   {      }');
            LoadLine('The Detector Diameter is 3 inches ');
            LoadLine('The Detector Thickness is 3 inches ');
            LoadLine('');
            LoadLine('   [  Ok  ]  [ Help ]  [Cancel]');
            with NaI do
               begin
                  SetNumber(1, detsourceDist);
                  SetNumber(2, Esource);
                  SetNumber(3, MonteCarlo);
                  SetHelpFile('NucRad .HLP', 'HelpScreen1');
                  repeat   {until noerror or canceled}
                     AcceptScreen;
                     noerror := true;
{Assign detector-source distance}
                     DetSourceDist := GetNumber(1);
                     DetSourceDistStr := NumStr(DetSourceDist, 3, 0);
                     if (DetSourceDist < 0) or (DetSourceDist > 100) then
                        begin
                           noerror := false;
                           announce('Detector-Source Distance must be  between 0 and 100 cm.');
                        end;
{Assign the number of MonteCarlo runs}
                     tempReal := GetNumber(3);
                     MonteCarlo := round(tempReal);
                     MCtotalStr := NumStr(MonteCarlo, 6, 0);
                     if MonteCarlo = 0 then
                        begin
                           announce('Number of Monte Carlo calculations   must be > zero.');
                           noerror := false;
                        end;
                     if MonteCarlo > 1000 then
                           announce('This will take a long  time with this number of M C calculations.');
{noerror := false;}
{Assign Source energy}
                     Esource := GetNumber(2);
                     energyStr := NumStr(Esource, 5, 3);
                     if (Esource < 0.3) or (Esource > 2.5) then
                        begin
                           noerror := false;
                           announce('energy must be between 0.3 and 2.5 MeV.');
                        end;
                     DetDia := 7.62;{cm = 3"}
                     DetDiaStr := NumStr(DetDia, 3, 1);
                     DetRadius := DetDia / 2.0;
                     thickness := 7.62;{cm = 3"}
                     thicknessStr := NumStr(DetDia, 3, 1);
                  until noerror or canceled;
               end;{with NaI}
               NaIInput := not Canceled;
            done;
{dispose(NaIScreen);}
         end;{with NaIScreen}
     CloseViewPort(10);
   end;{{NaIinput}

   procedure AssignMaterialconstants (var thisMaterial: MaterialType);
   begin
      with thisMaterial do
         begin
            singleMaterialname := 'NaI';
            Zeff := 64;
            Neff := 1.99e22;
            sigmax[1] := 20.3;
            Iaveeff := 452e-6;{MeV from Leo p26}
         end;{with thisMaterial}
      GetConstantArrays(thisMaterial);
   end;{AssignMaterialconstants}

   procedure PrepareResponseSpectrum (Esource: real;
                           var ResponseSpect: Dmatrix;
                           var EnergyPerChan: real;
                           var peakchan, ComptonChan: integer;
                           var ComptonEnergy: real);
      var
         i: integer;
         energyinterval, pulseHeight, Emc2: real;
   begin
      energyinterval := Esource * 1.1;
      EnergyPerChan := energyinterval / numChan;
      peakchan := round(Esource / EnergyPerChan);
      Emc2 := Esource / mec2;
      ComptonEnergy:= Esource * 2 * Emc2 / (1 + 2 * Emc2);
      ComptonChan := round(ComptonEnergy / EnergyPerChan);
      pulseHeight := 0.0;
      for i := 1 to numChan do
         begin
            pulseHeight := pulseHeight + EnergyPerChan;
            ResponseSpect.Put(1, i, pulseHeight);
         end;{for i}
   end;{PrepareResponseSpectrum}

   procedure InitialNaIValues (var NaI: NaIType;
                 gammao: real; MCdone: integer;
                 var thispt: MyPointType;
                 var thisOmega: OmegaType;
                 var Einitial: real);
{Assume a point source located on the detector symmetry axis.}
      var
         Rgamma: real;

   begin
      with NaI do begin       {sample the cosine distribution}
         with thisomega do
            with thisPt do begin
               Rgamma := SimpleRandom(idumconst);
               gamma := gammao + (1 - gammao) * (MCdone - Rgamma) / MonteCarlo;
               alpha := sqrt(abs(1 - sqr(gamma)));
               beta := 0.0;
               x := detSourceDist * alpha / gamma;
               y := 0;{because of azimuthal symmetry it is sufficient}
               z := 0;{to allow the photon to intercept on x-axis}
             end;{with thisPt}
         Einitial := Esource;
       end;{with NaI}
   end;{InitialNaIValues)}

   procedure calcDistanceXtal (var NaI: NaItype;
                               var Pt: MyPointType;
                               var Omega: OmegaType;
                               var distXtal: real);
      var
         ratio, K, A, B, C, temp, sintheta: real;
         {gammaCrit1, gammaCrit2, gammaCrit3, gammaCrit4: real;}
         x1, x2, y1, y2, xprime, yprime, length, ztemp, zmax: real;
(*Calculate the distance of line from the collision point to the edge
of the detector in the direction of the photon. First find the
intersection points for the line projected into the x,y plane then
given the cosine of the angle from the z-axis find the intersection
with the detector.*)
   begin
      with NaI do begin
         with Pt do begin
            with Omega do begin
               if abs(alpha) < 0.00001 then begin
                  x1 := x;
                  x2 := x;
                  if (sqr(DetRadius) - sqr(x1)) < 0.0 then
                     announce('DetRadius < x1');
                  y1 := sqrt(sqr(DetRadius) - sqr(x1));
                  y2 := y2;
                end
               else begin
                  ratio := beta / alpha;
                  K := y - x * ratio;
                  A := 1 + sqr(ratio);
                  B := 2 * ratio * K;
                  C := sqr(K) - sqr(DetRadius);
                  temp := sqr(B) - 4 * A * C;
                  if temp < 0.0 then
                     announce('temp is negative');
                  x1 := (-B + sqrt(temp)) / (2 * A);
                  x2 := (-B - sqrt(temp)) / (2 * A);
                  y1 := ratio * x1 + K;
                  y2 := ratio * x2 + K;
                end;
               if alpha > 0.0 then
                  if x1 > x then begin
                     xprime := x1;
                     yprime := y1;
                   end
                  else begin
                     xprime := x2;
                     yprime := y2;
                   end
                else if x1 > x then begin
                   xprime := x2;
                   yprime := y2;
                 end
                else begin
                   xprime := x1;
                   yprime := y1;
                 end;
               length := sqrt(sqr(xprime - x) + sqr(yprime - y));
               if gamma > 0.0 then
                  zmax := thickness - z
               else
                  zmax := z;
               sintheta:= sqrt(abs(1 - sqr(gamma)));
                  ztemp := abs(length * gamma/ sintheta);{opp/tan(theta)}
                  if ztemp >= zmax then {out end}
                     distXtal := abs(zmax / gamma)
                  else
                     distXtal := abs(length * sintheta);
            end;{with Omega}
         end;{with Pt}
      end;{with NaI}
   end;{calcDistanceXtal}

   procedure CalcSigma (E: real;
               var thisMaterial: MaterialType;
               var allsigma: CrossSectionType);
      var
         i, index: integer;
         numerator, denominator, offset: real;
         leavebool: boolean;
   begin
      with thisMaterial do begin
         i := 1;
         leavebool := false;
         while (i <= 59) and (not leavebool) do begin
            if (EnergyPairArray[1, i] - E) >= 0 then  begin
               index := i;
               leavebool := true;
             end;{if}
            if i = 59 then begin
               message('Energy too big, it has been set to  Emax = 100GeV');
               index := 59;
             end;{if i=59}
            inc(i);
          end;{for i}

         with allSigma do begin
            if index = 1 then begin
               sigCompton := ComptonArray[1];
               sigPhoto := PhotoArray[1];
               sigPair := EnergyPairArray[2,1];
             end;
{    CalcSigCompton(E, index, thisMaterial, allSigma);}
              numerator := (E - EnergyPairArray[1, index - 1])
                        * (ComptonArray[index] - ComptonArray[index - 1]);
              denominator := (EnergyPairArray[1, index]
                           - EnergyPairArray[1, index - 1]);
              offset := numerator / denominator;
              sigCompton := ComptonArray[index - 1] + offset;

              numerator := (E - EnergyPairArray[1, index - 1])
                             * (PhotoArray[index] - PhotoArray[index - 1]);
              offset := numerator / denominator;
              sigPhoto := PhotoArray[index - 1] + offset;

              if E < 1.5 then  {Pair cross section data begins at 1.5 MeV}
                 sigPair := 0
              else   begin
                 numerator := (E - EnergyPairArray[1, index - 1])
                    * (EnergyPairArray[2, index] - EnergyPairArray[2, index - 1]);
                 offset := numerator / denominator;
                 sigPair := EnergyPairArray[2, index - 1] + offset;
               end;
              sigTotal := sigCompton + sigPhoto + sigPair;{b/atom}
           end;{with allsigma}
     end;{with thisMaterial}
   end;{CalcSigma}

   procedure PhotonDirection (var photonOmega: OmegaType);
{direction cosines of an isotropic vector}
      var
         x, y, e, p, pfactor, d, R1, R2, R3, R4: real;
   begin
      with photonOmega do
         begin
            repeat   {until d<=1}
               R1 := SimpleRandom(idumconst);
               R2 := SimpleRandom(idumconst);
               x := 2 * R1 - 1;
               y := R2;
               d := sqr(x) + sqr(y);
            until d <= 1;
            repeat   {until e<=pfactor}
               R3 := SimpleRandom(idumconst);
               R4 := SimpleRandom(idumconst);
               e := sqr(R3) + sqr(R4);
               p := 2 * R3 * R4 / e;
               pfactor := 8 * p / (3 * sqrt(3));
            until e <= pfactor;
            alpha := p * (sqr(x) - sqr(y)) / d;
            beta := p * (2 * x * y / d);
            gamma := (sqr(R3) - sqr(R4)) / e;
         end;{with photonOmega}
   end;{PhotonDirection}

   procedure ResponseSpectrum (Edeposit, counts, EnergyPerChan: real;
                              var ResponseSpect: Dmatrix);
      var
         ChanNum: integer;
         newcounts: real;
   begin
      ChanNum := round(Edeposit / EnergyPerChan);
      if ChanNum = 0 then
        ChanNum := 1;
      newcounts := ResponseSpect.value(2, ChanNum) + counts;
      ResponseSpect.Put(2, ChanNum, newcounts);
   end;{ResponseSpectrum}

   procedure PhotoElectricInt (e:real; NaI: NaIType;
                weight, EnergyPerChan: real;
                var allsigma: CrossSectionType;
                var ResponseSpect: DMatrix);
      const
         Eloss = 0.0;
      var
         counts, Edeposit: real;
   begin
      counts := weight * allsigma.sigphoto / allsigma.sigtotal;
      Edeposit := NaI.Esource - Eloss;
      ResponseSpectrum(Edeposit, counts, EnergyPerChan, ResponseSpect);
   end;{PhotoElectricInt}

   procedure PairProductionInt (E, weight, capSigma, energyPerChan: real;
                                  var NaI: NaItype;
                                  var thisPt: MyPointType;
                                  var allsigma: CrossSectionType;
                                  var ResponseSpect: Dmatrix);
      var
         base, distXtal1, distXtal2, counts, Edeposit: real;
         probescape1, probescape2, probnoEscape, probOfOneEscape, probOfTwoEscape: real;
         omega1, omega2: OmegaType;
   begin
      with NaI do begin
         base := weight * allsigma.sigpair / allsigma.sigtotal;

         PhotonDirection(omega1);
         with omega1 do begin
            omega2.alpha := -alpha;
            omega2.beta := -beta;
            omega2.gamma := -gamma;
          end;{with omega1}

         calcDistanceXtal(NaI, thisPt, Omega1, distXtal1);
         probescape1 := exp(-capSigma * distXtal1);
         calcDistanceXtal(NaI, thisPt, Omega2, distXtal2);
         probescape2 := exp(-capSigma * distXtal2);
         probnoEscape := (1 - probescape1) * (1 - probescape2);{prob. 1 survives*prob. 2 survives}
         counts := base * probnoEscape;
         Edeposit := Esource;
         ResponseSpectrum(Edeposit, counts, EnergyPerChan, ResponseSpect);
         probOfOneEscape := probescape1 * (1 - probescape2) + probescape2 * (1 - probescape1);
         counts := base * probOfOneEscape;
         Edeposit := Esource - 0.511;{1st escape peak}
         ResponseSpectrum(Edeposit, counts, EnergyPerChan, ResponseSpect);
         probOfTwoEscape := 1 - probNoEscape - probOfOneEscape;
         counts := base * probOfTwoEscape;
         Edeposit := Esource - 1.022;{2nd escape peak}
         ResponseSpectrum(Edeposit, counts, EnergyPerChan, ResponseSpect);
      end;{with NaI}
   end;{PairProductionInt}

   procedure calcComptonScatt (E: real; var Efinal, cosScatt: real);
{Zerby fig 11 p111}
      var
         Rscatt1, Rp2, Rscatt3: real;
         Emec2, y: real;
         notdone: boolean;
   begin
      notdone := true;
      Emec2 := E / mec2;
      while notdone do begin
         Rscatt1 := SimpleRandom(idumconst);
         if Rscatt1 <= (2 * Emec2 + 1) / (2 * Emec2 + 9) then begin
            Rp2 := SimpleRandom(idumconst);
            y := 1 + 2 * Emec2 * Rp2;
            Rscatt3 := SimpleRandom(idumconst);
            if Rscatt3 <= 4 * ((1 / y) - (1 / sqr(y))) then begin
               cosScatt := 1 - 2 * Rp2;
               notdone := false;
             end
            else
               notdone := true;
          end
         else begin
            Rp2 := SimpleRandom(idumconst);
            y := (2 * Emec2 + 1) / (1 + 2 * Emec2 * Rp2);
            cosScatt := 1 - (y - 1) / Emec2;
            Rscatt3 := SimpleRandom(idumconst);
            if Rscatt3 <= 0.5 * ((1 / y) + sqr(cosScatt)) then
               notdone := false
            else
               notdone := true;
          end;{else}
         end;{while}
      Efinal := E / y;
   end;{calcComptonScatt}

   procedure ComptonInt (var NaI: NaIType;
               var thisMaterial: MaterialType;
               EnergyPerChan: real;
               var thisPt: MyPointType;
               var thisOmega, lastOmega: OmegaType;
               var allsigma: CrossSectionType;
               var expfactor, weight, E, capSigma, distXtal: real;
               var ResponseSpect: Dmatrix;
               var morecollisions: boolean);
      const
         energycutoff = 0.010;{MeV}
      var
         Rdistance, distance: real;
         Edeposit, Efinal, Elost, cosScatt, weightcutoff, counts: real;
   begin
      weightcutoff := 0.001 * (1 / NaI.MonteCarlo);
      weight := weight * allsigma.sigcompton / allsigma.sigtotal;
      calcComptonScatt(E, Efinal, cosScatt);
   {   E := Efinal;    }
      if Efinal <= energyCutOff then
         morecollisions := false;
      If morecollisions then begin
         lastOmega := thisOmega;
         DirectionAfterScatt(lastOmega, cosScatt, thisOmega);
         Calcsigma(Efinal, thisMaterial, allsigma);
         calcDistanceXtal(NaI, thisPt, thisOmega, distXtal);
         capSigma := thisMaterial.Neff * allsigma.sigTotal * 1e-24;{cm-1}
         expfactor := exp(-capSigma * distXtal);
         counts := weight * expfactor;
         Elost := 0.0;
         Edeposit := NaI.Esource - Efinal - Elost;
         ResponseSpectrum(Edeposit, counts, EnergyPerChan, ResponseSpect);
       end;{if morecollisions}
       weight:=weight*(1-expfactor);
       if weight < weightcutoff then
          morecollisions := false;
       if not morecollisions then begin
          Edeposit := E;
          counts := weight;
          ResponseSpectrum(Edeposit, counts, EnergyPerChan, ResponseSpect);
        end;
       E := Efinal;
   end;{ComptonInt}

   procedure CalcPeaktoTotal (var NaI: NaItype;
                var total: Dvector; eff: real;
                var PeaktoTotal, SDPeaktoTotal: real);
      var
         T, SDT, Tsqr, temp: real;
         i: integer;
         peak:dvector;
   begin
      with NaI do begin
         peak.init(montecarlo);
         peak.put(1,total.value(1));
         for i:=2 to montecarlo do
            peak.put(i,total.value(i)-total.value(i-1));
         T := peak.value(1);
         for i := 2 to MonteCarlo do
            T := T + peak.value(i);
         Tsqr := sqr(T);
         SDT := 0.0;
         for i := 1 to MonteCarlo do
            SDT := SDT + sqr(peak.value(i));
         SDT := SDT - Tsqr;
      (*   T := total.value(1);
         for i := 2 to MonteCarlo do
            T := T + total.value(i);
         T := T / MonteCarlo;
         Tsqr := sqr(T);
         SDT := 0.0;
         for i := 1 to MonteCarlo do
            SDT := SDT + sqr(total.value(i)) - Tsqr;*)
         SDT := sqrt(abs(SDT)/montecarlo);
         PeaktoTotal := T / eff;
         SDPeaktoTotal := SDT / eff;
      end;{with NaI}
   end;{CalcPeakToTotal}

   procedure CalcBroadenSpect (Esource, EnergyPerChan: real;
                var ResponseSpect: Dmatrix;
                var BroadenSpect: Dmatrix);
{Gaussian broadening}
      const
         C1 = 0.01703;{(MeV^.5) from Zerby}
         C2 = 0.01147;{(MeV)  from Zerby}
      var
         i, j, ChanNum: integer;
         E, Eprime, integral, SD, term1, term2, Gaussian, max: real;
   begin
      E := 0.0;
      for i := 1 to numChan do begin
         E := E + abs(EnergyPerChan);
         Eprime := 0.0;
         integral := 0.0;
         for j := 1 to numChan do begin
            Eprime := Eprime + EnergyPerChan;
            SD := C1 * sqrt(Eprime) + C2 * Eprime;
            term1 := 1 / (sqrt(pi) * SD);
            term2 := sqr((E - Eprime) / SD) / 2;
            if term2 < 123 then
               Gaussian := term1 * exp(-term2)
            else
               Gaussian := 0.0;
            if (j = 1) or (j = numChan) then
               integral := 0.5 * (ResponseSpect.value(2, j) * Gaussian) + integral
            else
               integral := ResponseSpect.value(2, j) * Gaussian + integral;
          end;{for j (Eprime)}
         BroadenSpect.Put(2, i, integral * EnergyPerChan);
         BroadenSpect.Put(1, i, ResponseSpect.value(1, i));
       end;{for i (E)}
      ChanNum := round(Esource / EnergyPerChan);
      max := BroadenSpect.value(2, ChanNum - 16);
      for i := ChanNum - 15 to NumChan do
         if BroadenSpect.value(2, i) > max then
            max := BroadenSpect.value(2, i);
      for i := 1 to NumChan do
         BroadenSpect.put(2, i, BroadenSpect.value(2, i) / max);{normalize BroadenSpect to a max of 1}
   end;{CalcBroadenSpect}

   procedure HandleNaIHotKeys (key: byte;
                     var NaI: NaIType;
                     var esckey: boolean);
   forward;

   procedure NaIcalc (var NaI: NaIType);
      var
         i, MCtogo, drawcount, peakchan, ComptonChan: integer;
         weightCutOff, gammao, weight, capSigma, eff, zeroInter, distXtal: real;
         E, Esource, Einitial, EnergyPerChan, Edeposit, counts,ComptonEnergy: real;
         expfactor, PeaktoTotal,peaktototal1,peaktototal2,peaktototal3, SDPeaktoTotal, dsigmadE_factor: real;
         broaden, morecollisions, esckey: boolean;
         ResponseSpect, BroadenSpect: Dmatrix;
         total: dvector;
         thisPt, lastPt: MyPointType;
         thisOmega, lastOmega: OmegaType;
         allsigma: CrossSectionType;
         thisMaterial: MaterialType;
         text: Str80;
         keybd: char;
         key: byte;

   procedure FirstCollision;
   (* Select a photon from the source and then force the first collision
      to be inside the detector. The weight is adjusted to account for the
      probability that the sample photon remained in the detector and
      therefore in the calculation.*)
      var
        Rdistance, distance: real;
   begin
      InitialNaIValues(NaI, gammao, i, thisPt, thisomega, Einitial);
      E := Einitial;
      Calcsigma(E, thisMaterial, allsigma);
      calcDistanceXtal(NaI, thisPt, thisOmega, distXtal);
      capSigma:= thisMaterial.Neff * allsigma.sigTotal * 1e-24;{cm-1}
      expfactor := exp(-capSigma * distXtal);
      weight := weight * (1 - expfactor);
      eff := eff + 1 - expfactor; {average detector efficiency}
      zeroInter := zeroInter + distXtal * expfactor;  {Analytic Zero Intercept}

   {Calculate the first Collision point}
      Rdistance:= SimpleRandom(idumconst);
      distance := -ln(1 - Rdistance * (1 - expfactor)) / capSigma;
      lastpt := thisPt;
      with thisPt do
         with thisOmega do begin
            x := lastPt.x + alpha * distance;
            y := lastPt.y + beta * distance;
            z := lastPt.z + gamma * distance;
          end; {with thisOmega}
   end;{FirstCollision}

   procedure NextCollisionPoint;
      var
        Rdistance, distance: real;
   begin
      Rdistance:= SimpleRandom(idumconst);
      distance := -ln(1 - Rdistance * (1 - expfactor)) / capSigma;
      lastpt := thisPt;
      with thisPt do
         with thisOmega do begin
            x := lastPt.x + alpha * distance;
            if abs(x) > NaI.DetRadius then
                announce('x > DetRadius');
            y := lastPt.y + beta * distance;
            if abs(y) > NaI.DetRadius then
               announce('y > DetRadius');
            z := lastPt.z + gamma * distance;
            if sqrt(sqr(x) + sqr(y)) > NaI.DetRadius then
               announce('sqr(x) + sqr(y) > DetRadius');
          end; {with thisOmega}
   end;{NextCollisionPoint}

   begin
{    new(ResponseSpect);}
{    new(BroadenSpect);}
{    new(total);}
      ResponseSpect.init(2, numChan);
      BroadenSpect.init(2, numChan);
      total.init(NaI.MonteCarlo);
      esckey := false;
      idumconst := 1;
      InitializePlotSpect(NaI);
      with NaI do begin
         gammao := DetSourceDist / sqrt(sqr(DetSourceDist)  + sqr(detRadius));{calculate the minimum cosine wrt the z-axis}
         eff := 0.0;
         zeroInter := 0.0;
         weightcutoff := 0.001 * (1 / MonteCarlo);
         AssignMaterialConstants(thisMaterial);
         PrepareResponseSpectrum(Esource, ResponseSpect, EnergyPerChan, peakchan, ComptonChan,ComptonEnergy);
         broaden := false;
         MCtogo := MonteCarlo;
         drawcount := 0;
         i := 1;
         while (i <= MonteCarlo) and (not esckey) do  begin
            morecollisions := true;
            drawcount := drawcount + 1;
            weight := 1 / MonteCarlo;
            FirstCollision;
            repeat         {until (Not morecollisions) Or (esckey)}
               CheckForEvents;
               if NaIHotKeys.Pressed(key) then
                  HandleNaIHotKeys(key, NaI, esckey);
               if event.keypressed then begin
                  beep;
                  esckey := (event.ReadKey = chr(27));   {look for escape key}
                end;
               if (not esckey) then begin
{****    first photoelectric then pair if E>1.5MeV then compton  ******}
                  PhotoElectricInt(E, NaI, weight, EnergyPerChan, allsigma, ResponseSpect);
                  if E >= 1.5 then
                     PairProductionInt(E, weight, capSigma, energyPerChan, NaI, thisPt, allsigma, ResponseSpect);
                {  weight := weight * (1 - expfactor);  }
                  if weight < weightcutoff then begin
                     morecollisions := false;
                     Edeposit := E;
                     counts := weight;
                     ResponseSpectrum(Edeposit, counts, EnergyPerChan, ResponseSpect);
                   end
                  else begin
                     ComptonInt(NaI, thisMaterial, EnergyPerChan, thisPt,
                         thisOmega, lastOmega, allsigma, expfactor, weight, E,
                         capSigma, distXtal, ResponseSpect, morecollisions);
                     NextCollisionPoint;
                   end;
                end;{if not esckey}
            until (not morecollisions) or (esckey);
            if i > 1 then
               total.Put(i, ResponseSpect.value(2, peakchan) + ResponseSpect.value(2, peakchan - 1){  - total.value(i - 1)})
            else
               total.Put(i, ResponseSpect.value(2, peakchan) + ResponseSpect.value(2, peakchan - 1));

            SelectViewPort(8);
            SelectScale(7);
            EraseXY(MapX(5.3), MapY(3.5), 10, 1, DarkGray);
            MCtogo := MonteCarlo - i;
            SetColor(whitte);
            OutTextXY(MapX(5.5), MapY(3.5), NumStr(MCtogo, 6, 0));
            broaden := false;
            PlotResponseSpect(NaI, ResponseSpect, broaden, peakchan, comptonchan,ComptonEnergy);
            CalcBroadenSpect(Einitial, EnergyPerChan, ResponseSpect, BroadenSpect);
            broaden := true;
            PlotResponseSpect(NaI, BroadenSpect, broaden, peakchan, ComptonChan,ComptonEnergy);
            inc(i);
          end;{for i}
         if (not esckey) then begin
            CalcBroadenSpect(Einitial, EnergyPerChan, ResponseSpect, BroadenSpect);
            broaden := true;
            PlotResponseSpect(NaI, BroadenSpect, broaden, peakchan, ComptonChan,ComptonEnergy);
            dsigmadE_factor := 2 * pi * sqr(re) * thisMaterial.Neff
                                 / sqr(Esource / mec2);{(dsigma/dE)/No for straightahead scattering}
            peaktototal1:=ResponseSpect.value(2,peakchan);
            eff := eff / MonteCarlo;
            peaktototal2:=responseSpect.value(2,peakchan)/eff;
            zeroInter := zeroInter * dsigmadE_factor / MonteCarlo;
            CalcPeaktoTotal(NaI, total, eff, PeaktoTotal, SDPeaktoTotal);
            selectviewport(8);
            SelectScale(7);
            Setcolor(whitte);
            OutTextXY(MapX(8.8), MapY(9.2), NumStr(peaktototal, 6, 4));
            OutTextXY(MapX(8.8), MapY(6.5), NumStr(SDpeaktototal, 6, 4));
            OutTextXY(MapX(8.8), MapY(3.5), NumStr(eff, 6, 4));
            selectViewPort(12);
            SetColor(yellow);
            PlotLine(-energyperchan, log10(zeroInter), energyperchan, log10(zeroInter));
            PlotLine(0.0, log10(zeroInter-0.5*zeroInter), 0.0, log10(zeroInter+0.5*zeroInter));
          end;{if not esckey}
       end;{with NaI}
      if esckey then MainHotKeys.display;
      ResponseSpect.free;
      BroadenSpect.free;
      total.free;
   end;{NaIcalc}


{**************Menu and Hotkey handling routines**********}


   procedure HandleMainMenu;
   forward;

   procedure NaIRadiation;
   begin
{     MainMenu.done;}
      if NaIinput(NaI) then
      begin
         OneStepNaI := false;
         PrintNaIinfo(NaI);
         SetUpNaIHotKeys;
         NaIcalc(NaI);
         mode := NaIMode;
      end;
   end;{NaIScreen}

   procedure HandleNaIHotKeys; {(key: byte; var NaI: NaIType; var restartNaI: boolean)}
      var
         i: integer;
   begin
      case key of
         1:{Help}
            ShowHelp('NucRad .hlp', 'HelpScreen3');{DisplayNaIHelp;}
(*       2:{Stop}
            begin
               message('Hit any key to resume');
               DynamicPause;
               message('');
            end;
         3: OneStepNaI := true; {New Data}
         4:{Add}
            begin
               message('Not available yet. Hit any key to resume');
               DynamicPause;
               message('');
            end;{ReturntoSetup}            *)
         2: begin {Stop}
              beep;
              esckey := true;
            end;
      end;{key}
{     ClearMKBuffers; }
   end;{HandleNaIHotKeys}

   procedure InitRadSource (thisSource: sourceType; var Rad: RadType);
   begin
      with Rad do
         case ThisSource of
            proton:  begin
               if source[1] <> proton then begin
                  source[1] := proton;
                  sourcename[1] := 'proton';
                  energy := 10.000;   {MeV}
                  multimaterials := 1;
                  material[1] := Aluminum;
                  thickness := 0.04;  {cm}
                  range:= false;
                  MonteCarlo := 200;
                 end;{if}
               multisources := 1;
               end;{proton}
            muon:  begin
               if source[1] <> muon then begin
                  source[1] := muon;
                  sourcename[1] := 'muon';
                  energy := 5.000;   {MeV}
                  material[1] := Aluminum;
                  multimaterials := 1;
                  thickness := 0.08;  {cm}
                  range:= false;
                  MonteCarlo := 200;
                 end;{if}
               multisources := 1;
               end;{muon}
            electron:  begin
               if source[1] <> electron then begin
                  source[1] := electron;
                  sourcename[1] := 'electron';
                  energy := 0.050;   {MeV}
                  material[1] := Aluminum;
                  multimaterials := 1;
                  thickness := 0.0005;  {cm}
                  range:= false;
                  MonteCarlo := 150;
                 end;{if}
               multisources := 1;
               end;{Electron}
            photon:  begin
               if source[1] <> photon then begin
                  source[1] := photon;
                  sourcename[1] := 'photon';
                  energy := 0.662;   {MeV}
                  material[1] := Aluminum;
                  multimaterials := 1;
                  thickness := 2.0;  {cm}
                  range:= false;
                  MonteCarlo := 500;
                 end;{if}
               multisources := 1;
               end;{photon}
              end;{case}
            SourceInput(Rad);
   end;{InitRadSource}

   procedure InitNaI(var NaI: NaIType);
   begin
      with NaI do
      begin
         detsourceDist := 10.0;
         Esource := 0.662;
         MonteCarlo := 1000;
      end;{with NaI}
   end;{InitNaI}

   procedure GeneralRadiation;
      var
         numviews: integer;
   begin
      OneStepRad:= false;
      SetUpRadHotKeys;
      numviews := Rad.multimaterials * Rad.multisources + 2;
      RadiationCalc(numviews, Rad);
      Mode := RadMode;
   end;{GeneralRadiation}

   procedure HandleRadHotKeys;
   begin
      case key of
         1:{Help}
            ShowHelp('NucRad .hlp', 'HelpScreen2');
         2: begin   {Stop}
             beep;
             esckey := true;
            end;
      end;{key}
{     ClearMKBuffers;  }
   end;{HandleRadHotKeys}

   procedure HandleMainHotKeys(key : integer);
   begin
      case key of
       1 : ShowHelp('NucRad .hlp', 'HelpScreen1');
       2 : if MainMenu.Chosen then HandleMainMenu;
      end;
   end;

   procedure HandleMainMenu;
   var
      ThisSource: sourceType;
   begin
      with MainMenu do
         case colChosen of
            1: case rowChosen of
                1: AboutCUPS;
                2: ShowHelp('NucRad .hlp', 'HelpScreen1');
                3: Configuration;
                5: QuitFlag := True;
               end; {case of column 1}
            2: begin
                case rowChosen of
                   1: ThisSource := proton;
                   2: ThisSource := muon;
                   3: ThisSource := electron;
                   4: ThisSource := photon;
           {        5: ThisSource := multiple;     }
                end;{case column 2}
                InitRadSource(ThisSource, Rad);
                GeneralRadiation;
               end;
            3: case rowChosen of
                1: NaIRadiation;
               end;{case Column 3}
         end;{case colChosen}
   end;{HandleMainMenu}

   procedure OpeningScreen;
   var
     thisSource: sourceType;
     thisMaterial: materialType;
     i,lastcollision, viewsnum, collisionAv: integer;
     atten, scalefactor: real;
     AngleDistr, deltaT, TransDistr :DVector;
     Ptvec: PointVectorType;
     unitStr: str25;
   begin
      AngleDistr.init(numangles);
      deltaT.init(11);
      TransDistr.init(11);
      thisSource := photon;
      viewsnum:= 1;
      collisionAv := 3;
      with Rad do begin
         source[1] := photon;
         sourcename[1] := 'photon';
         sourceStr := 'photon';
         energy := 0.080;   {MeV}
         energyStr := numStr(energy, 5, 3);
         material[1] := Aluminum;
         materialStr := 'Aluminum';
         materialname[1] := 'Aluminum';
         multimaterials := 1;
         thickness := 9.801;  {cm}
         thicknessStr := numStr(thickness, 6, 4);
         range:= true;
         MonteCarlo := 1000;
         MCtotalStr := numStr(MonteCarlo, 6, 0);
         multisources := 1;
       end;{with Rad}
      ChooseMaterial(1, Rad, thismaterial);
      InitializePlotPath(rad.thickness, Rad, thisMaterial, thisSource, viewsnum, scalefactor, unitStr);
      PrintExpInfo(Rad, rad.thickness, scalefactor, unitstr);
      GetAngleDistr(AngleDistr);
      PlotAngle(Rad, AngleDistr, collisionAv, viewsnum);
      InitializePrintdata(thisSource);
      transmit := 8;
      reflect := 76;
      absorb := 911;
      out := 5;
      UpdateData(Rad.MonteCarlo, 11, viewsnum, thissource, thismaterial, Rad, Rad.thickness);
      calcAtten(Rad, thismaterial, atten);
      OutTextXY(MapX(4.3), MapY(2.3), numStr(atten, 6, 3));
      InitializeRangePlot(Rad, thisMaterial, atten, deltaT);
      GetTransDistr(TransDistr);
      PlotRangePoints(Rad, atten, TransDistr, deltaT);
      PlotRange(Rad, atten, TransDistr, deltaT);
      SelectViewPort(1);
      SelectScale(1);
      SetColor(yellow);
      OutTextXY(MapX(0.01*scalefactor*rad.thickness), MapY(1.9*scalefactor*rad.thickness),'10 reprentative photon');
      OutTextXY(MapX(0.01*scalefactor*rad.thickness), MapY(1.65*scalefactor*rad.thickness),'     paths out of 1000');
      PlotLine(-0.1*scalefactor*rad.thickness,0.0,0.0,0.0);
      for i := 1 to 10 do begin
         GetPtvec(i, lastcollision, Ptvec);
         RedrawPath(viewsnum, lastcollision, thisSource, Ptvec, scalefactor);
       end;
      rad.source[1] := pseudo;
      OpenMess;
      AngleDistr.free;
      deltaT.free;
      TransDistr.free;
   end;{OpeningScreen}

   procedure ProgramInit;
   begin
      SetUpMainMenu;
      SetUpMainHotKeys;
      Rad.source[1]:= pseudo;{InitRad(Rad);}
      InitNaI(NaI);
     {ShowHelp('NucRad .hlp', 'HelpScreen1');{DisplayMainHelp;}
      DefineViewport(1, 0.07, 0.47, 0.50, 0.82);  {upper left}
      DefineViewport(2, 0.55, 0.95, 0.50, 0.82);  {upper right}
      DefineViewport(3, 0.55, 0.95, 0.11, 0.43);  {lower right}
      DefineViewport(5, 0.07, 0.47, 0.11, 0.43);  {lower left}
      DefineViewport(7, 0.10, 0.90, 0.83, 0.95);  {Experimental information}
      DefineViewPort(9, 0.01, 0.99, 0.01, 0.95);
      DefineViewPort(10, 0, 1, 0.05, 0.95);  {full screen, excluding menu and hotkey bars}
      DefineViewport(11, 0.07, 0.47, 0.12, 0.855);{NaI left}
      DefineViewport(12, 0.55, 0.99, 0.12, 0.855);{NaI right}
      DefineViewport(8, 0.07, 0.99, 0.87, 0.95); {NaI information}
      SelectViewPort(9);
      SetColor(whitte);
      QuitFlag := false;
      Mode := MainMode;
   end;{ProgramInit}

{************************* Main Program **************************}

var
   key : byte;
   esckey: boolean;
begin
   CUPSinit;
   ProgramInit;
   OpeningScreen;
   repeat   {until false}
      CheckForEvents;
      if MainMenu.Activated then HandleMainMenu;
      case mode of
       MainMode: if MainHotKeys.Pressed(key) then HandleMainHotKeys(key);
       RadMode:  if RadHotKeys. Pressed(key) then HandleRadHotKeys (key, Rad, esckey);
       NaIMode:  if NaIHotKeys. Pressed(key) then HandleNaIHotKeys (key, NaI, esckey);
      end;
   until quitflag;
   mainMenu.done;
{dispose(mainmenu);}
   cupsdone;

end.{Program exp}
