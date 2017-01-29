unit Pmenus;
  {Unit for Stellar Atmosphere; C. A. Whitney}
  {sets up menus and help screens; handles them}
   {modified June 21, 1994}
interface

	uses
		crt, dos, graph, printer, CUPSMUPP, CUPS, CUPSfunc, CUPSproc, cupsgui, CUPSGrph, PCommon, Poutput, PInput, PBuild, pdynprocs;
	procedure SetUpInterface;
	procedure HandleMenuSel;
	procedure UpdateMenus;    {and hot keys}
	procedure ResetGenHotKeys;
	procedure MFile;
	procedure MStaticModel;{build and display static model}
	procedure MPlotWhat; {choose data to be animated}
	procedure MRun; {start animation}
  { PROCEDURE mMethods;}
	procedure MHelp;
  {--------------------------------------------}


implementation

   {=============Utilities======================}

	procedure SetUpInterface;
	begin
		MyMenu.Init;
		with MyMenu do
			begin
				column(1, 'File');
				Row(1, 1, 'About Cups');
				Row(1, 2, 'About program');
				Row(1, 3, 'Configuration');
				Row(1, 4, 'Save Model Data');
				Row(1, 5, 'Save Graph Setup');
				Row(1, 6, 'Get Graph Setup');
				Row(1, 7, '------------------ ');
				Row(1, 8, 'Exit program');

				Column(2, 'Model');
				row(2, 1, 'Specify/Build');
				Row(2, 2, 'List Model');
				{Row(2, 3, 'List More Model'); }
				Row(2, 3, 'Plot Structure');
				Row(2, 4, 'Plot More Structure');
				Row(2, 5, 'Show Modes');

				column(3, 'Plot-What');
				row(3, 1, 'Set Plot Types');
				row(3, 2, 'Select Variables');
				row(3, 3, 'Show/Clear Plots');
				{row(3, 4, 'See Mode Phasors');}

				column(4, 'Run');
				row(4, 1, 'Set Modes');
            row(4, 2, 'Set Displacements');
            row(4, 3, 'Start');
				row(4, 4, 'Stop');
				row(4, 5, 'Continue');


				column(5, 'Help');
				row(5, 1, 'Get Started');
				row(5, 2, 'Model');
            row(5, 3, 'Plot-what');
            row(5, 4, 'Starting from Displacements');
				row(5, 5, 'Modes');
				row(5, 6, 'Methods');

				rowactivate(1, 7, false);
				{rowactivate(3, 4, false);  }

				Display; {make sure menu is up}

			end; {with mymenu}

		with GenHotKeys do
			begin
				init(6);
				key[1] := 'F1-Help';
				key[2] := 'F2-Start';
				key[3] := 'F3-Continue';
				key[4] := 'F4-Faster';
				key[5] := 'F5-Slower';
				key[6] := 'F10-Menu';

			end;

      With MoveHotKeys do
         begin
           init(6);
				key[1] := 'F1-Help';
				key[2] := 'Left-Left';
				key[3] := 'Right-Right';
				key[4] := 'Down-Decrease';
				key[5] := 'Up-Increase';
				key[6] := 'F10-Menu';
         end;
      updatemenus;
	end; {setupinterface}

 {===================================}

	procedure UpDateMenus;
		var
			i: integer;
	begin


		for i := 1 to 7 do
			if modelbuilt then
				MyMenu.RowActivate(4, i, TRUE)
			else
				mymenu.RowActivate(4, i, FALSE);

		if modelbuilt  then
			GenHotKeys.display
		else
			genhotkeys.clear;   {cannot run the pulsation w/o model}


		for i := 3 to 5 do
			if ModelBuilt then {Something ready to be plotted}
				MyMenu.RowActivate(2, i, TRUE)
			else
				MyMenu.RowActivate(2, i, FALSE);

         if runstate=stopped then
            genhotkeys.key[2]:='F2-Start'
            else
            genHotkeys.key[2]:='F2-Stop';

         genhotkeys.display;
 end; {UpDateMenus}

 {===========================}
	procedure HandleMenuSel;
	begin
		with mymenu do
			case colChosen of
				1: 
					MFile;
				2:
					begin   {static model}
						gOpMode := specifymode;
						MStaticModel;
					end;
				3: 
					begin   {plot-what}
						MPlotwhat;
        { gOpMode:=graphmode;}
					end;
				4:
					begin {Run}

						mRun;
					end;

				5:
					begin {help}
						MHelp;
					end;

			end; {CASE}

     if runstate=stopped then
            genhotkeys.key[2]:='F2-Start'
            else
            genHotkeys.key[2]:='F2-Stop';

       if not (gOpMode=setdisplacementsMode) then
                   genhotkeys.display;
	end;  {HandleMenu}

 {================================================}

	procedure ResetGenHotKeys;

	begin
		genHotKeys.display;
	end;


 {=================  Menu Response handling Procedures ================}

	procedure MFILE;
		type
			reco = file of setupRec;
		var
			fname, tstring: string;
			datafile: text;
			f, i, j: integer;
			stpfile: reco;
			q: real;

	begin

		with MyMenu do
			case rowChosen of
				1: 
					AboutCups;

				2: 
					ShowHelp('pulse.hlp', 'ABOUTTHISPROGRAM');

				3: 
					Configuration;

				4:
					begin {save to disc }
						fname := concat(Modelname, '.pas');
						ASSIGN(datafile, fname);
						REWRITE(datafile);

						WRITELN(datafile, concat('Data for stellar model in file: ', fname));

						writeln(datafile, 'Model name: ' + modelname);
						writeln(datafile, 'Solar units: Mass = ' + ScNumStr(Starmass, 1) + '; Radius = ' + ScNumstr(Starrad, 1) +
                                                 ';Luminosity = ' + scNumStr(starlum, 1));
						writeln(datafile, 'Surface temperature(Te) = ' + numstr(star.teff, 7, 0) + ' K; Surface gravity = '
                                                 + num2str(star.grav, 5) + ' cm/sec^2');
						writeln(datafile, 'Abundances by number: ' + 'X(H) = ' + scNumstr(habund, 3) + '; Y(He) = ' + scNumStr(heabund, 3) +
                                                 ' Z(Met)= ' + ScNumStr(metabund, 3));
						case star.densitylaw of
							1: 
								tstring := 'Constant density';
							2: 
								tstring := 'Radiative/convective';
							3: 
								tstring := 'Radiative';
							4: 
								tstring := 'Convective';
						end;
						writeln(datafile, 'Fractional envelope mass = ' + scnumstr(envelmass, 3) + '; Type of model: ' + tstring);
						writeln(datafile);

						writeln(datafile, ' Shell  r(cm)  Mr(gm)  T(K)   P(dy/cm^2) V(cm^3/gm)  Lum   kappa');
						writeln(datafile, '        dM1    dM2      mu       gamma        H+     He+   He++');

						i := numshells;
						repeat
							with star.eq[i] do
								begin
									writeln(datafile, NumStr(i, 3, 0), ' ', scNumStr(vari[radi], 2), ' ', scNumStr(intmass, 2), ' ',
                                                                         scNumStr(vari[temp], 2), ' ',
                                                                         scNumStr(vari[pres], 2), ' ',
                                                                         scNumStr(vari[volu], 2), ' ',
                                                                          scNumStr(vari[flux], 2), ' ',
                                                                          scNumStr(vari[kapp], 2));

									writeln(datafile, '    ', scNumStr(dm1, 2), ' ', scNumStr(dm2, 2), ' ', scNumStr(vari[lcmu], 2), ' ',
                                                                        scNumStr(vari[gamm], 2), ' ',
                                                                        scNumStr(vari[Hion], 2), ' ',
                                                                        scNumStr(vari[Heion], 2), ' ',
                                                                         scNumStr(vari[Hepion], 2));
								end; {with star}
							i := i - 1;
						until (i < 1);


						Q := 2 * pi / mode[0].freq / 86400 / 1.187 * sqrt(starmass / (starrad * starrad * starrad));  {pulsation constant}
						writeln(datafile);
						writeln(datafile, concat('Pulsation constant: ' + NumStr(Q, 6, 4)));
						writeln(datafile);
						writeln(datafile, 'EigenFrequencies:');
						writeln(datafile, '  Overtone      Period(days)');
						writeln(datafile, '            Real      Damping');
						for i := 1 to NumShells do
							begin

								if heatTrans = adiabatic then
									if i = 1 then
										writeln(datafile, NumStr(i - 1, 3, 0), '        ', concat(scnumStr(2 * pi / 8.64e4 / mode[i - 1].freq, 2) +
                                                                                 '     No damping'))
									else
										writeln(datafile, NumStr(i - 1, 3, 0), '        ', concat(scnumStr(2 * pi / 8.64e4 / mode[i - 1].freq, 2) +
                                                                                 '         " '));
							end;
						CLOSE(datafile);

					end;


				5: 
					begin     {Save Graph setup}
						with SetUPData do {Load record with setup parameters. Note,}
{
   with a proper definition of the record, this translation would}
{
   not be necessary. Fix some day.}
							begin
								for f := 1 to 4 do
									begin
										fkind[f] := frame[f].kind;
										sXvar[f] := frame[f].xvariable;
										sYvar[f] := frame[f].yvariable;
									end;
								sShowShell := showshell;
								sPointScale := MassPointScale;
								sIsZoomed := IsZoomed;
								sZoomedFrame := ZoomedFrame;
								sDensityLaw := star.DensityLaw;
								sNumShells := Numshells;
								sEquation := Equation;
								sHeatTrans := HeatTrans;
								sModelName := ModelName;
								sLum := StarLum;
								sRad := StarRad;
								sMass := StarMass;
								sHabund := Habund;
								sHeAbund := HeAbund;
								sMetAbund := MetAbund;
								sCoreMass := coreMass;
								sEnvelMass := EnvelMass;
								for i := 0 to 3 do
									begin
										sMActive[i] := mode[i].active;
										sMAmp[i] := mode[i].amp;
									end;
							end;{with setupdata}
						fname := concat(Modelname, '.stp');
						ASSIGN(stpfile, fname);
						REWRITE(stpfile);

						WRITE(stpfile, setUpData);

						CLOSE(stpfile);
						{Message('The setup data were written to file: ' + fname + '     <anykey> to continue');
						Pause;}

					end;

				6: 
					begin  {Read Graph setup}
                  fname:= openfile('*.stp');{ This shows directories but not files.}
{						fname := 'sun.stp'; }   {This works and reads successfully}
                  if fname<>'' then
                  begin
						   assign(stpfile, fname);
						   reset(stpfile);
						   read(stpfile, setupdata);
						   with SetUPData do {Read parameters}
							   begin
								   for f := 1 to 4 do
									   begin
										   frame[f].kind := fkind[f];
										   frame[f].xvariable := sXvar[f];
										   frame[f].yvariable := sYvar[f];
									   end;
								   ShowShell := sShowshell;
								   MassPointScale := sPointScale;
								   IsZoomed := sIsZoomed;
								   ZoomedFrame := sZoomedFrame;
								   star.DensityLaw := sdensityLaw;
								   NumShells := sNumshells;
								   Equation := sEquation;
								   HeatTrans := sHeatTrans;
								   ModelName := sModelName;
								   StarLum := sLum;
								   StarRad := sRad;
								   StarMass := sMass;
								   Habund := sHabund;
								   HeAbund := sHeAbund;
								   MetAbund := sMetAbund;
								   CoreMass := scoreMass;
								   EnvelMass := sEnvelMass;
								   for i := 0 to 3 do
									   begin
										   mode[i].active := sMactive[i];
										   mode[i].amp := sMAmp[i];
									   end;
							   end;{with setupdata}
       {now read data from the record}
						   close(stpfile);
                  end;
					end;


				8: 
					begin {exit from program}
						QuitNow := true;
					end;

			end; {CASE}
         if (mymenu.rowchosen in [4,5]) then
                 begin
                  Message('The data were written to file: ' + fname + '     <anykey> to continue');
						Pause;
                  mymenu.display;
                 end;
	end;{MfILE}
 {=================================================================}
	procedure mStaticModel;
		var
			okay, changed: boolean;
	begin
  {HideCursor;}
		CloseViewPort(9); {whole screen}
  {ShowCursor;}
		with MyMenu do
			case rowChosen of

				1:
					begin  {specify/build}
						doSpecifyAModel;
						{showhelp('pulse.hlp', 'GETSTARTED');}
						gOpMode := specifyMode;
						if buildtridiagmat then
							begin
								FindLinearModes;
							end;
					end;

				2:
					begin  {List}
						gDoingMore := false;
						doListModel;
						gOpMode := listMode;
					end;


				3:
					begin  {Graph StaticModel}
						gdoingMore := FALSE;
						DoGraphModel(gDoingMore);
						gOpMode := GraphMode;
					end;

				4:
					begin  {Graph more Staticmodel}
						gdoingMore := TRUE;
						DoGraphModel(gDoingMore);
						gOpMode := GraphMode;
					end;

				5:
					begin
                  gOpMode := ListFreqMode;
						{startcalc; }
           			DoShowModefreqs;

					end;
			end; {CASE menu}

	end; {mStaticModel}

 {=====================================================}

	procedure MPlotWhat;
		var
			fname: string;
			datafile: text;
			i: integer;

	begin
  {HideCursor;}
		CloseViewPort(9); {whole screen}
  {ShowCursor;}

		with MyMenu do
			case rowChosen of

				1: 
					begin  {Set Type of plot in each frame}
						OpenTypeSelector;
						openVariableSelector;
						openPlots;
						gOpMode := PlotMode;
					end;

				2: 
					begin  {Select variable in each frame}
						OpenVariableSelector;
						openPlots;
						gOpMode := PlotMode;
					end;

				3: 
					begin  {}
						OpenPlots;
						gOpMode := plotMode;
					end;

				4: 
					begin  {show phasors}
       {OpenModePhasors;}
						genHotkeys.display;

					end;

			end; {CASE menu}


	end; {mPlotWhat}
 {=================================}
	procedure mRun;
   var s:integer;
	begin

		with MyMenu do
			case rowChosen of
				1:
					begin {Phasors}
                  openviewport(9);
                  closeviewport(9);
						gopMode := phasorMode;
						doSetModes;
					end;

            2:      begin {set Displacements}
                    openviewport(9);
                    closeviewport(9);
                    gopMode:=SetDisplacementsMode;
                    for s:=1 to numshells do
                        tempVector[s]:=star.dyn[s].vari[disp];
                    MoveHotkeys.display;
                    ShowDisplacements;
                    rowactivate(4,5,false);
                    end;

            3: begin {start}
                  openviewport(9);
                  closeviewport(9);
                  openplots;
                  startcalc;
						RunState := running;
                  gopMode:=runMode;


               end;

            4:  begin {stop}
                      runstate:=stopped;
                      genhotkeys.key[2]:='F2-Start';
                      genhotkeys.display;
                end;

				5: begin  {continue}
                  closeviewport(DispSetScrn);
                  closeviewport(escBut);
                  closeViewport(AcceptBut);
                  openplots;
						RunState := running;
                  gopMode:=runMode;
					end;
   		end; {case}


	end;{MRun}
 {===================================}
 {procedure mMethods;
	begin

		with MyMenu do
			case rowChosen of
				1:
					begin
                 HeatTrans:=adiabatic;
					end;

            2:  begin
                    HeatTrans:=Nonadiabatic;
                end;

				3:	begin
                    equation:=linear_Syn;
					end;
				4:
					begin
                    equation:=Linear_FDE;
               end;
           { 5: begin
                    equation:=nonlinear;  }
              { end;}
			{end;} {case}
                     {   UpdateMenus;
	end;{MMethods}
 {===================================}

 {===================================}
	procedure MHelp;
		var
			Savedmode: integer;

	begin

		savedMode := gOpMode;
		gOpMode := HelpMode;
		resetGenHotkeys;
		with MyMenu do
			case rowChosen of

				1:
					ShowHelp('pulse.hlp', 'GETSTARTED');
				2: 
					ShowHelp('pulse.hlp', 'STATICMODEL');
            3:
					ShowHelp('pulse.hlp', 'PLOTS');
				4:
               ShowHelp('pulse.hlp', 'DISPADJUST');
            5:
					ShowHelp('pulse.hlp', 'MODES');
				6:
					ShowHelp('pulse.hlp', 'METHODS');

			end; {CASE}
		gOpMode := savedMode;
	end;   {MHelp}

end.