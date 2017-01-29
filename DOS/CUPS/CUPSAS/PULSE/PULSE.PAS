
program PulsePC;


 {******************************************************************}
 {*    STELLAR PULSATION PROGRAM                                   *}
 {*    Constructs a stellar model in hydrostatic equilibrium       *}
 {*    and then computes the  linear radial modes. Permits         *}
 {*    displaying superpostion of modes.                           *}
 {*                                                                *}
 {*     C.A. Whitney    Copyright @ 1994                           *}
 {*        CUPS Project. PulsePC1 version 1.0, June 21, 1994        *}
 {******************************************************************}



	uses
		crt, dos, graph, printer, CUPSMUPP, CUPS, CUPSfunc, CUPSproc, cupsgui, CUPSGrph, PCommon, Pmenus, Pbuild, POutPut, PInPut,
                PdynProcs;
 {*-----               Initialization                                  -----*}
 {---------------------------------------------------------------------------}


	procedure InitModel;      {default values for solar model}

		var
			i,s: Integer;
			okay: boolean;
	begin
      manualDisp:=false; {defalut is to find from modes}
		ModelBuilt := false;
		QuitNow := false;
		gOpMode := graphmode;
		star.densitylaw := DRadCon;
     { star.densitylaw := DHom;}
		ModelName := 'Sun'; {user-defined name of the current model}
		starLum := 1;    { solar units}
		starRad := 1;
		StarMass := 1;
		HAbund := 0.7;
		MetAbund := 0.02; {abundances by number}
		HeABund := 1 - HAbund - MetAbund;
		Numshells := 20; {number of shells in model}
		equation := Linear_syn; {linear_Syn,linear_FDE,nonlinear}
      skipframe:=1;
		heatTrans := adiabatic;
		coremass := 0.2;{fraction of star}
		envelmass := 1 - coremass;
		unitsSelected := solar;
		mode[0].amp := 1;
		mode[1].amp := 1;
		runstate := stopped;
      for s:=1 to numshells do
         begin
         mode[i-1].active:=false;
         initDisp[s]:=0;
         end;
      mode[0].active := true;
		BuildModel(okay);
		if okay then
			modelbuilt := true
		else
			modelbuilt := false;
		if buildtridiagmat then
			begin
				FindLinearModes;
			end;
	end;{INit}
 {-----------------------------------------------}
  {========================================}
	procedure InitPlots; {create selection buttons, define labels,}
{
 and set defaults}
		var
			f, t, shellInc: integer;
			x, y: real;
	begin
		kindlabel[stripK] := 'Strip Chart';
		kindlabel[LinePlotk] := 'Profile Plot';
		kindlabel[phaseDiagk] := 'Phase Diagram';
		kindlabel[AnimDiagk] := 'Animation';
                kindLabel[nullK]:=' ';
 {now set up dictionary of labels}
		LogMyLabel[pres] := 'Log(P-d/cm^2)';
		LogMyLabel[temp] := 'Log(T-K)';
		LogMyLabel[dens] := 'Log(Den.-g/cm^3)';
		LogMyLabel[kapp] := 'Log(Opacity-cm^2/g)';
		NatMyLabel[pres] := 'P-d/cm^2';
		NatMyLabel[temp] := 'T-K';
		NatMyLabel[dens] := 'Dens.-g/cm^3';
		NatMyLabel[Hion] := 'H+';
		NatMyLabel[Heion] := 'He+';
		NatMyLabel[HePion] := 'He++';
		NatMyLabel[gamm] := 'Ad. dlnT/dlnP';
		NatMyLabel[flux] := 'Rad. Flux';
		NatMyLabel[velo] := 'Veloc.-cm/s';
		NatMyLabel[kapp] := 'Opacity-cm^2/g';
		NatMyLabel[disp] := 'Displacement-cm';
		NatMyLabel[radgrad] := 'Act. dlnT/dlnP';
		NatMyLabel[BolMag] := '2.5 log(Brightness)';
 {default frame data}
		masspointscale := true;
		structureData.logarithmic := true;
		frame[1].kind := animDiagK;
		frame[1].yvariable := pres;
		frame[2].kind := LinePlotK;
		frame[2].yvariable := disp;
		frame[3].kind := PhaseDiagK;
		frame[3].yvariable := velo;
		frame[3].xvariable := disp;
		frame[4].kind := stripK;
		frame[4].yvariable := disp;
		findShowShells;
      shellSel:=numshells div 2; { for setting displacement}
	end;
 {=================================}

 {--------------------------------------------}
	procedure ProcessHotKeys (key: byte);

	begin
		case key of
			1:{HELP}
				begin
					if gOpMode = specifyMode then
						showHelp('pulse.hlp', 'GETSTARTED')
					else if gOpMode = RunMode then
						showHelp('pulse.hlp', 'RUNNING')
					else if gOpMode = PhasorMode then
						showHelp('pulse.hlp', 'MODES')
					else  if gopmode= plotMode then
						showhelp('pulse.hlp', 'PLOTS')
               else if gopmode=setDisplacementsMode then
                    showhelp('pulse.hlp','DISPADJUST')
               else showHelp('pulse.hlp', 'GETSTARTED');

				end;
			2:{STOP}
				begin

					if runstate = stopped then
						begin
                     closeviewport(9);
							openplots;
							startcalc;  {initialize the pulsation}
							runstate := running;
                     genhotkeys.key[2]:='F2-Stop';
						end
					else
                  begin
                       genhotkeys.key[2]:='F2-Start';
						     RunState := stopped;  {sets end condition}
                  end;
                  genhotkeys.display;
				end;
			3:{CONTINUE}
				begin
               {closeviewport(DispSetScrn);
               closeviewport(escBut);
               closeViewport(AcceptBut);}
               openplots;
					runState := running;
               genhotkeys.key[2]:='F2-Stop';
               genhotkeys.display;
				end;
			4:{FASTER}
				begin
					dt[equation] := 2 * dt[equation];
					runstate := running;
				end;
			5:{SLOWER}
				begin
					dt[equation] := dt[equation] / 2;
					runstate := running;
				end;
			6:{MENU}
				begin
               if mymenu.chosen then HandleMenuSel;
				end;
		end;
	end; {ProcessKeys}
   {---------------------------------------------}

   procedure HandleMoveHotkeys(k:byte);
   Begin
       if k= 1 then
          	begin
					if gOpMode = specifyMode then
						showHelp('pulse.hlp', 'GETSTARTED')
					else if gOpMode = RunMode then
						showHelp('pulse.hlp', 'RUNNING')
					else if gOpMode = PhasorMode then
						showHelp('pulse.hlp', 'MODES')
					else  if gopmode= plotMode then
						showhelp('pulse.hlp', 'PLOTS')
               else if gopmode=setDisplacementsMode then
                    showhelp('pulse.hlp','DISPADJUST')
               else showHelp('pulse.hlp', 'GETSTARTED');


				end;
       if k = 6 then
       begin
          if mymenu.chosen then HandleMenuSel;
       end;

        if gOpMode=setDisplacementsMode then
         begin
              case k of
              2:  begin {Select another shell}
                  shellSel:=shellSel-1;
                  if shellSel<1 then shellSel:=numshells;
                  end;
              3:  begin
                  shellSel:=ShellSel+1;
                  if ShellSel>numshells then ShellSel:=1;
                  end;
              4:  begin
                  tempVector[shellsel]:=tempvector[shellsel]*0.98;
                  end;
              5:  begin
                  tempVector[shellsel]:=tempvector[shellsel]/0.98;
                  end;
              end;{case}
         showDisplacements;
         end;{if}
   end;
 {-------------------------------------------------}
   procedure HandleMouseClicked;
   var
      recalcModel,doEsc,DoAccept:boolean;
      xr,yr:real;
      s:integer;
   Begin
        if gopMode=setdisplacementsMode then
        begin
            { for s:=1 to numshells do
                      tempVector[s]:=star.dyn[s].vari[disp]; done in menu handler }
            ReCalcModel:=false;
            doEsc:=false;
            mouseposn(xr,yr,escbut,escBut,doEsc);     {see if escape was hit}
                 if DoEsc then manualDisp:=false;

            mousePosn(xr,yr,AcceptBut, AcceptBut, ReCalcModel);{ see if accept was hit}
            if ReCalcModel then
               begin
                   for s:=1 to numshells do
                   begin
                     initDisp[s]:=tempVector[s];
                     star.dyn[s].vari[disp] := tempVector[s];
                   end;
               ManualDIsp:=true;

               findFAmplitudes;
               end;
                closeviewport(DispSetScrn);
                closeviewport(escBut);
                closeViewport(AcceptBut);
                openviewport(9);
                closeviewport(9);
                genhotkeys.key[2]:='F2-Start';
					 {RunState := stopped;}  {sets end condition}
                genhotkeys.display;

         end;

   end;
 {===========================================================================}
 (*-----              ****    MAIN PROGRAM    ****                     -----*)
 {---------------------------------------------------------------------------}

 {This program controls the flow of the main menu}
	var
		Error,x,y: Integer;
		key: Byte;
		keychar: char;
      okay:boolean;

begin {Main}
	CUPSinit;
	InitModel;
	initPlots;
	SetUpInterface;
	SetUpviewports;

	showhelp('pulse.hlp', 'ABOUTTHISPROGRAM');
	doSpecifyAModel;
	startcalc;
   FindshowShells;

   runstate:=running;
   genhotkeys.key[2]:='F2-Stop';
   genhotkeys.display;
   openplots;

	repeat
		CheckForEvents;
      if MyMenu.activated then
			begin
				runstate := stopped;
				HandleMenuSel;
			end;

      if (gOpMode=setdisplacementsMode) then
       begin
         if MoveHotKeys.Pressed(key) then HandleMoveHotKeys(key);
         if mouseClicked(x,y) then HandleMouseClicked;
       end;

		if not (gOpMode=setDisplacementsMode) then
         if GenHotKeys.pressed(key) then processHotKeys(key);

		if runstate = running then DoOneStep;

	until quitNow;

	MyMenu.done;
	CUPSdone;

end.