
unit POutput;
 {---------------------------------------------------------------------------}
 {----            Output and Help Screen  Procedures                      -----*)
 {-------------------Modified May 1994 ----------------------------}
 {---------------------------------------------------------------------------}


interface

	uses
		crt, dos, graph, printer, CUPsMupp, CUPS, CUPSfunc, CUPSProc, cupsgui, CUPSGrph, PCommon;
	procedure OpenTypeSelector;
	procedure OpenVariableSelector;
	procedure OpenFrame (f: integer);
	procedure OpenPlots;
	procedure setUpViewPorts;
	procedure InitLabels (f: integer);
	procedure PutLabels (f: integer);
	procedure SetMyAxes (f: integer);

	procedure OpenGraphModel (DoingMore: boolean);
	procedure OpenListModel;
	procedure OpenModeFreqs;

	procedure DoGraphModel (DoingMore: boolean);
	procedure DoListModel;
	procedure DoshowModeFreqs;

	procedure DoPlotPerturbation (ft: integer);
	procedure DoAnimation (f: integer);
	procedure DoPhaseDiagram (f: integer);
	procedure DoStripChart (f: integer);
 {PROCEDURE DoModePhasors;  }
	procedure PlotFrame (f: integer);
	procedure PlotFrames;

	procedure CloseButtons;

implementation

 {---------------------------------------------------------------------------}
 {===================  Open Screen Procedures  ======================}
 {---------------------------------------------------------------------------}


 {---------------------------------------------------------------------------}
	procedure OpenTypeSelector;     {draws buttons for selecting type of}
{
plot in each frame. Check in main loop for button changed and handle}
{
the result there.}

		var
			f: integer;
			screen: Tinputscreen;
	begin
		with screen do
			begin
				init;
				defineInputPort(0.0, 0.9, 0.55, 0.95);
				loadline('Select plot type for each frame');
				loadline(' ');
				Loadline('Type:   No Plot  Strip     Profile     Phase Diag.   Animation');
				loadline('Frame 1   #1       #1         #1            #1           #1');
				loadline('Frame 2   #2       #2         #2            #2           #2');
				loadline('Frame 3   #3       #3         #3            #3           #3');
				loadline('Frame 4   #4       #4         #4            #4           #4');
				loadline('Select x-variable:    #5  Mass point index   #5  Radius');
				loadline(' [ OK ]      [Cancel]');
				setRadioButton('1', frame[1].kind);
				setRadioButton('2', frame[2].kind);
				setRadioButton('3', frame[3].kind);
				setRadioButton('4', frame[4].kind);
				if masspointscale then
					setRadioButton('5', 1)
				else
					setRadioButton('5', 2);
				acceptscreen;

				if not canceled then
					begin
						frame[1].kind := getRadioButton('1');
						frame[2].kind := getRadioButton('2');
						frame[3].kind := getRadioButton('3');
						frame[4].kind := getRadioButton('4');
						for f := 1 to 4 do
							if GetRadioButton('5') = 1 then
								masspointscale := true
							else
								masspointscale := false;
						if GetRAdioButton('5') = 1 then
							masspointscale := true
						else
							masspointscale := false;

					end;
				done;
			end;{WITH SCREEN}

	end;  {open Type selector}
 {==========================================}

	procedure OpenVariableSelector;
		var
			f, s, st: integer;
			screen: Tinputscreen;
 {---------------------------------}
		function FindVarID (v: integer): integer;
 {finds frame id for variable to be plotted; used for radio buttons}
		begin
 {case}
			case v of
				pres: 
					FindVarId := 1;
				temp: 
					FindVarId := 2;
				dens: 
					FindVarId := 3;
				velo: 
					FindVarId := 4;
				disp: 
					FindVarId := 5;
				{flux:
					FindVarId := 6;}
				BolMag:
					FindVarId := 6;
			end; {CASE}
		end;

		function GetVarId (id: integer): integer;
 {find variable from frame Id; used for radio buttons}
		begin
			case ID of
				1: 
					GetVarId := pres;
				2: 
					GetVarId := temp;
				3: 
					GetVarId := dens;
				4: 
					GetVarId := velo;
				5: 
					GetVarId := disp;
				{6:
					GetVarId := flux;}
				6:
					GetVarId := BolMag;
			end;{CASE}
		end;
 {-------------main}
	begin
		with screen do
			begin
				init;
				defineInputPort(0.0, 0.95, 0.15, 0.95);

				LoadLine('Input indexes of shells to be displayed in Phase diagram and Strip chart:');
				LoadLine(' {   } {   }  {   }  {   }  {   }');

				for st := 1 to 5 do   {clear}
					begin
						setNumber(st, 0);
						setnumberLimits(st, 0, numshells);
					end;
				st := 0;
				for s := 1 to numshells do   {update}
					if s in showshell then
						begin
							st := st + 1;
							SetNumber(st, s);
						end;
				loadline(' ');
				loadline('Select variables for each frame:');

				LoadLine('           Press. Temp.  Dens.  Veloc.  Displace.   Mag. ');
				loadLine(concat('Frame 1 -- ' + kindlabel[frame[1].kind]));
				loadLine('Y-variable: #1     #1     #1     #1       #1       #1');
				if frame[1].kind = phasediagk then
					loadLine('X-variable: #2     #2     #2     #2       #2        #2');

				loadLine(concat('Frame 2 -- ' + kindlabel[frame[2].kind]));
				loadLine('Y-variable: #3     #3     #3     #3       #3       #3');
				if frame[2].kind = phasediagk then
					loadLine('X-variable: #4     #4     #4     #4       #4       #4');

				loadLine(concat('Frame 3 -- ' + kindlabel[frame[3].kind]));
				loadLine('Y-variable: #5     #5     #5     #5       #5       #5');
				if frame[3].kind = phasediagk then
					loadLine('X-variable: #6     #6     #6     #6       #6       #6');

				loadLine(concat('Frame 4 -- ' + kindlabel[frame[4].kind]));
				loadLine('Y-variable: #7     #7     #7     #7       #7       #7');
				if frame[4].kind = phasediagk then
					loadLine('X-variable: #8     #8     #8     #8       #8       #8');
         {set radio buttons to current values}
				SetRadioButton('1', findVarID(frame[1].yvariable));
				if frame[1].kind = PhaseDiagK then
					SetRadioButton('2', findVarID(frame[1].xvariable));
				SetRadioButton('3', findVarID(frame[2].yvariable));
				if frame[2].kind = PhaseDiagK then
					SetRadioButton('4', findVarID(frame[2].xvariable));
				SetRadioButton('5', findVarID(frame[3].yvariable));
				if frame[3].kind = PhaseDiagK then
					SetRadioButton('6', findVarID(frame[3].xvariable));
				SetRadioButton('7', findVarID(frame[4].yvariable));
				if frame[4].kind = PhaseDiagK then
					SetRadioButton('8', findVarID(frame[4].xvariable));

				loadline(' ');

				LoadLine(' [ ok ]       [cancel]');
				acceptScreen;
				if not canceled then        {Get variables from radio buttons}
					begin
						frame[1].yvariable := GetVarId(GetRadioButton('1'));
						frame[2].yvariable := GetVarId(GetRadioButton('3'));
						frame[3].yvariable := GetVarId(GetRadioButton('5'));
						frame[4].yvariable := GetVarId(GetRadioButton('7'));

						if frame[1].kind = phaseDiagK then
							frame[1].xvariable := GetVarId(GetRadioButton('2'));
						if frame[2].kind = phaseDiagK then
							frame[2].xvariable := GetVarId(GetRadioButton('4'));
						if frame[3].kind = phaseDiagK then
							frame[3].xvariable := GetVarId(GetRadioButton('6'));
						if frame[4].kind = phaseDiagK then
							frame[4].xvariable := GetVarId(GetRadioButton('8'));
						showshell := [0];
						for st := 1 to 5 do
							showShell := Showshell + [trunc(getNumber(st))];
					end;


				DONE;
			end;{WITH SCREEN}

	end;
 {==========================================}

	procedure SetUpViewPorts;
	begin

		DefineViewport(3, 0.10, 0.45, 0.15, 0.47);  {bottom left}
		DefineViewport(1, 0.10, 0.45, 0.58, 0.90);  {top left}
		DefineViewport(4, 0.60, 0.95, 0.15, 0.47);  {bottom right}
		DefineViewport(2, 0.60, 0.95, 0.58, 0.90);  {top right}
      DefineViewport(7, 0.10, 0.45, 0.15, 0.47);  {bottom left}
		DefineViewport(5, 0.10, 0.45, 0.58, 0.90);  {top left}
		DefineViewport(8, 0.60, 0.95, 0.15, 0.47);  {bottom right}
		DefineViewport(6, 0.60, 0.95, 0.58, 0.90);  {top right}
		DefineViewport(9, 0.00, 1.00, 0.07, 0.93);  {whole screen}
		DefineViewport(10, 0.0, 1.0, 0.35, 0.95);  {Upper portion of screen}
		DefineViewport(11, 0.0, 0.6, 0.1, 0.7);  {Far top right}

	end;
 {=====================================}
	procedure InitLabels (f: integer);
		var
			x, xe, y, ye: integer;
	begin
		with frame[f] do
			begin
				case kind of
					stripK: 
						begin
							axisLabel[2, f] := 'Time Step';
							axisLabel[1, f] := NatMyLabel[yvariable];
						end;
					linePlotK: 
						begin
							if masspointScale then
								AxisLabel[2, f] := 'Mass Point'
							else
								AxisLabel[2, f] := 'Radial Dist. (r/R)';
							axisLabel[1, f] := NatMyLabel[yvariable];
							selectviewport(f);
							with views[f] do
								begin
									x := 0;
									xe := vx2 - vx1;
									y := (-vy1 + vy2) div 2;
									ye := y;
									moveto(x, y);
									lineto(xe, ye);
								end;

						end;
					PhaseDiagK:
						begin

							selectviewport(f);
							axisLabel[2, f] := natMyLabel[xvariable];
							axisLabel[1, f] := natMyLabel[yvariable];
							with views[f] do
								begin
									x := 0;
									xe := vx2 - vx1;
									y := (-vy1 + vy2) div 2;
									ye := y;
									moveto(x, y);
									lineto(xe, ye);
									x := (vx2 - vx1) div 2;
									xe := x;
									y := 0;
									ye := vy2 - vy1;
									moveto(x, y);
									lineto(xe, ye);
								end;
						end;
					AnimDiagK: 
						begin
							axisLabel[1, f] := ' ';
							axisLabel[2, f] := ' ';
						end;
					structureK:
						begin
							if masspointscale then
								AxisLabel[2, f] := 'Mass Point'
							else
								AxisLabel[2, f] := 'Radial Dist. (r/R)';
          {do y-axis}
							if (not gdoingmore) then
                        begin
                         if  structureData.logarithmic then
								   axisLabel[1, f] := LogMyLabel[yvariable]
							    else
								   axisLabel[1, f] := NatMyLabel[yvariable];
                        end
                        else   {doingmore}
                         axisLabel[1, f] := NatMyLabel[yvariable];
						end;{structureK}
				end;{CASE}
			end;{with}

	end;
 {========================================}

	procedure PutLabels (f: integer);
	begin
		if not (frame[f].kind = nullk) then
                   begin
                    PutLabel(left, axisLabel[1, f]);
		              putLabel(bottom, AxisLabel[2, f]);
                   end;
  {add labels to frame box top}
		setcolor(white);
		if not ((frame[f].kind = structureK) or (frame[f].kind = animDiagK)) then
			print(8, 1, concat(numstr(f,1,0)+'. '+kindlabel[frame[f].kind]));
		{if frame[f].kind = animDiagK then }
			{print(4, 1, concat(numstr(f,1,0)+'. Animation ' : ' + NatMyLabel[frame[f].yvariable] ));   }
		setcolor(lightgreen);
	end;

 {==================================}



	procedure SetMyAxes (f: integer);
		var
			xint, yint: real;
	begin
		selectviewport(f);
		with GraphScales[ScaleNumber] do
			Axis(0.0, 0.0, TickSpace(xmax - xmin), TickSpace(ymax - ymin));
	end;

 {-----------------------------------}

 {=================================}
	procedure GetXScale (f: integer);     {for structure plot}
	begin
		xl := 0;
 { if ((frame[f].kind = structurek) or (frame[f].kind=lineplotk)) then}
		if MassPointscale then
			xr := numshells
		else
		{	xr := star.eq[numshells].vari[radi]; }
         xr:=1.1;

	end;

 {==================================}
	procedure GetYScale (f: integer); {for structure plot}
		var
			temp1, temp2: real;
			i: integer;
	begin

		if frame[f].kind = Structurek then
			begin
 {scan for largest value}
				temp1 := 0;
				for i := 1 to numshells do
					if star.eq[i].vari[frame[f].yvariable] > temp1 then
						temp1 := star.eq[i].vari[frame[f].yvariable];

 {scan for smallest value}
				temp2 := 1e25;
				for i := 1 to numshells do
					if star.eq[i].vari[frame[f].yvariable] < temp2 then
						temp2 := star.eq[i].vari[frame[f].yvariable];

				if (structureData.logarithmic and (not gdoingmore)) then
					begin
						if temp1 > 0 then
							st[f] := Log10(temp1) +0.1{find upper scale}
						else
							st[f] := 0;

						if temp2 > 0 then
							begin
								sb[f] := Log10(temp2)-0.1;
								if sb[f] < 0 then
									sb[f] := sb[f] - 1;
							end
						else
							sb[f] := 0;
					end
				else  {linear or doingMore}
					begin
						st[f] := temp1*1.1; {find upper scale}
						sb[f] := temp2*0.9;
					end;

				if (frame[f].yvariable = Heion) or (frame[f].yvariable = Hepion) then
					begin
						st[f] := 1.1;
						sb[f] := 0;
					end
				else
					begin
        { st[f]:=trunc(st[f]+1);trouble}
        { sb[f]:=trunc(sb[f]);}
					end;

				if ((frame[f].yvariable = gamm) or (frame[f].yvariable = radgrad)) then
					begin
						sb[f] := 0;
					end;

			end; {if structurek}
 {handle other cases}

	end;   {getYScale}

 {=======================================}

	procedure OpenFrame (f: integer);

 {-------------------------------}
	begin {main OpenFrame}
		getxScale(f);
		GraphBackColor := black;
		setColor(lightGreen);
		SetLineStyle(0, 0, 1);
		OpenViewPort(f);
     { setmyaxes(f); }
		InitLabels(f);
		putlabels(f);

	end;{OpenFrame}
 {========================================}
	procedure OpenPlots;
		var
			frame: integer;
	begin
		justcleared := true;
		for frame := 1 to 4 do
			OpenFrame(frame);
	end;
 {=========================================}


	procedure OpenGraphModel (DoingMore: boolean);
		var
			f: integer;
			j: integer;

	begin

		frame[5].yvariable := pres;  {tell program which data to plot in each frame}
		frame[6].yvariable := temp;
		frame[7].yvariable := dens;
		frame[8].yvariable := kapp;
		if doingMore then
			begin
				frame[5].yvariable := gamm;
				frame[6].yvariable := radgrad;
				frame[7].yvariable := Heion;
				frame[8].yvariable := Hepion;
			end;


		for j := 5 to 8 do
			begin
				st[J] := 0;
				sb[J] := 0;
			end;
		GraphBackColor := darkGray;
		SetLineStyle(0, 0, 1);
		SetColor(LightGreen);

		for f := 5 to 8 do
			begin
				frame[f].kind := structureK;
   { lineplot[f].masspointscale:=true; }
				InitLabels(f);
				GetYScale(f);
				if st[f] = sb[f] then   {variable is constant across plot}
					begin
						if structuredata.logarithmic and (not gdoingmore) then
							begin
								with star do   {use log value at bottom}
									st[f] := int(log10(eq[numshells].vari[frame[f].yvariable]) + 1);
								sb[f] := st[f] - 1;
							end
						else
							begin
								sb[f] := 1;
								with star do   {use value at bottom}
									st[f] := 2 * eq[numshells].vari[frame[f].yvariable];
							end;
					end;
			end;

		GetXScale(5); {Loads xr; same x-scale for all frames}

		for f := 5 to 8 do
			begin
				OpenViewPort(f);
				DefineScale(f, 0, xr, sb[f], st[f]);
				SetMyAxes(f);
				selectScale(f);
				PutLabels(f);
			end; {for f}

		SetColor(White);
	end;

 {---------------------------------------------------------------------------}

	procedure OpenListModel;

	begin
		GraphBackColor := DarkGray;
		SetColor(White);
		Openviewport(9);

	end;

 {---------------------------------------------------------------------------}

	procedure OpenModeFreqs;

	begin
		GraphBackColor := DarkGray;
		SetColor(White);
		Openviewport(9);

	end;
 {---------------------------------------------------------------------------}


 {=========================Output Handling Procedures =======================}

	procedure DOPARAMETERLIST;
		var
			tstring: string;
	begin
		Print(3, 2, 'Model name: ' + modelname);
		Print(3, 3, 'Solar units: Mass = ' + ScNumStr(Starmass, 1) + '; Radius = ' + ScNumstr(Starrad, 1) + '; Luminosity = '
                + scNumStr(starlum, 1));
		Print(3, 4, 'Surface temperature(Te) = ' + numstr(star.teff, 7, 0) + ' K; Surface gravity = ' + num2str(star.grav, 5)
                + ' cm/sec^2');
		Print(3, 5, 'Abundances by number: ' + 'X(H) = ' + scNumstr(habund, 3) + '; Y(He) = ' + scNumStr(heabund, 3) + ' Z(Met)= '
                + ScNumStr(metabund, 3));
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
		Print(3, 6, 'Fractional envelope mass = ' + scnumstr(envelmass, 3) + '; Type of model: ' + tstring);

		Print(3, 7, ' Shell  r(cm)  Mr(gm)  T(K)   P(dy/cm^2) V(cm^3/gm)  Lum   kappa');
      if not (gOpMode = listfreqmode) then
		Print(3, 8, '        dM1    dM2      mu       gamma        H+     He+   He++');
	

        end;

        {==============================================================}

        procedure DoListModel;
		var
			i, Myx, MyY, TopLine: integer;
			secondPage: boolean;
	begin
		OpenListmodel;
		graphbackcolor := DarkGray;
		DoParameterList;
		topLIne := 8;

                	myY:=topline;

		FOR I:=1 to numshells do
			with star.eq[i] do
				begin
					{myY := (2 * ((numshells - i) mod 7) + topline; }
                                        myY:=mYY+1;
					myX := 4;
					print(myX, myY, NumStr(i, 3, 0));
					myX := 8;
					print(myX, myY, scNumStr(vari[radi], 2));
					myX := 17;
					print(myX, myY, scNumStr(intmass, 2));
					myX := 26;
					print(myX, myY, scNumStr(vari[temp], 2));
					myX := 35;
					print(myX, myY, scNumStr(vari[pres], 2));
					myX := 44;
					print(myX, myY, scNumStr(vari[volu], 2));
					myX := 53;
					print(myX, myY, scNumStr(vari[flux], 2));
					myX := 62;
					print(myX, myY, scNumStr(vari[kapp], 2));

					myY := myY + 1;
					myX := 8;
					print(myX, myY, scNumStr(dm1, 2));
					myX := 17;
					print(myX, myY, scNumStr(dm2, 2));
					myX := 26;
					print(myX, myY, scNumStr(vari[lcmu], 2));
					myX := 35;
					print(myX, myY, scNumStr(vari[gamm], 2));
					myX := 44;
					print(myX, myY, scNumStr(vari[Hion], 2));
					myX := 53;
					print(myX, myY, scNumStr(vari[Heion], 2));
					myX := 62;
					print(myX, myY, scNumStr(vari[Hepion], 2));
                                if (i mod 6) =  0 then
                                   BEGIN
                                        MESSAGE('Any key or mouse button for more');
                                        staticpause;
                                        openListModel;
                                        doparameterlist;
                                        myY:=topline;
                                   END;

				end; {for i , with star}

                mymenu.display;
	end;
 {======================================}

	procedure GraphStructureData (f: integer);
		var
			s: integer;
			xb, yb, xe, ye: real;
	begin
		SelectViewport(f);
		SelectScale(f);
		setcolor(lightgreen);
		with star do
			begin
				for s := 1 to numshells - 1 do
					begin
 {find x coordinates}
						if masspointscale then
							begin
								xb := s;
								xe := s + 1;
							end
						else
							begin
								xb := eq[s].vari[radi]/eq[numshells].vari[radi];
								xe := eq[s + 1].vari[radi]/eq[numshells].vari[radi];
							end;
 {now find y coordinates}
						if (structureData.logarithmic and (not gdoingmore)) then
							begin
								if ((eq[s].vari[frame[f].yvariable] > 0) and (eq[s + 1].vari[frame[f].yvariable] > 0)) then
									begin
										yb := log10(eq[s].vari[frame[f].yvariable]);
										ye := log10(eq[s + 1].vari[frame[f].yvariable]);
									end
								else   {can't plot with log scale}
									begin
										yb := 0;
										ye := 0;
										{beep;}
									end;
							end {if log}
						else
							begin
								yb := (eq[s].vari[frame[f].yvariable]);
								ye := (eq[s + 1].vari[frame[f].yvariable]);
							end;
 {draw the line}
						plotline(xb, yb, xe, ye);
					end; {for s}
			end; {with star}
		setcolor(white);
	end;
 {==================================}
	procedure DoGraphModel (doingMore: boolean);
		var
			f: integer;
	begin
		OpenGraphModel(doingMore);
		for f := 5 to 8 do
 {if not (( star.densitylaw=DHom) and (frame[f].yvariable=kapp)) then }
			graphstructureData(f);
	end;
 {------------------------------------------  }
	procedure DoShowModeFreqs;
		var
			max, i, TopLine, myX, myY: integer;
			Q: real;

	begin
		OpenmodeFreqs;
		{doParameterList; }
		max := numshells-1;
		{if max > 12 then
			max := 12; {display only the first 12 frequencies}
	{	TopLine := 8;}
   topline:=1;
		myx := 4;
		myY := topline;
		Q := 2 * pi / mode[0].freq / 86400 / 1.187 * sqrt(starmass / (starrad * starrad * starrad));  {pulsation constant}
		print(myX, myY, concat('Pulsation constant: ' + NumStr(Q, 6, 4)));
		myY := topline + 1;
		print(myX, myY, 'Overtone      Period(days)              Rel. Amp. ');
		myY := topLIne + 2;
		print(myX, myY, '            Real      Damping');
		for i := 0 to max do
			if mode[i].active then
          begin
				MyX := 4;
				{myY := topline + i + 3; }
            myY:=myY+1;
				print(myX, myY, NumStr(i, 3, 0));
				myX := 15;
 { print(myX,myY,scnumStr(8.64e4*mode[i-1].freq/2/pi,4));}
				myX := 15;
				if heatTrans = adiabatic then
					if i = 0 then
						print(myX, myY, concat(scnumStr(2 * pi / 8.64e4 / mode[i].freq, 2) + '  No damping'))
					else
						print(myX, myY, concat(scnumStr(2 * pi / 8.64e4 / mode[i].freq, 2) + '      " '));
           myX :=45;
                  print(myX,myY, scnumStr(mode[i].amp,4));
			  end;
         repeat
         until mouseclicked(myx,myy);
                 closeviewport(9);
	end;

 {===============These are the animated graphs===============}
 {They are called in CarryOn, depending on the mode that has been selected}


	procedure DoPlotPerturbation (ft: integer);
		const
			old = 1;
			new = 2;
		var
			s, st: integer;
			x, y: real;
 {---------------------------------}
		procedure Drawline (f, color: integer; myData: integer);
			var
				xb, xe, yb, ye: real;
				ts: integer;
		begin
 {selectviewport(f);}
 {selectScale(f); }
			setcolor(color);
			with star, LinePlot[f] do
				for ts := 1 to numshells - 1 do
					begin
      {find x-coordinate}
						if masspointscale then
							begin
								xb := ts;
								xe := ts + 1;
							end
						else
							begin
								xb := eq[ts].vari[radi];
								xe := eq[ts + 1].vari[radi];
							end;
      {find y-coordinate}
						if mydata = old then
							begin
								yb := oldData[ts];
								ye := oldData[ts + 1];
							end
						else if mydata = new then
							begin
								yb := newData[ts];
								ye := newData[ts + 1];
							end;
						plotline(xb, yb, xe, ye);
					end;  {for ts}
		end;
 {---------------------------------}

	begin {main}

		st := 0;
		selectviewport(ft);
		selectscale(ft);
      setwriteMode(xorput);
		with LinePlot[ft] do
			for s := numshells downto 1 do    {Get data and draw vertical trace}
				begin

					Newdata[s] := star.dyn[s].vari[frame[ft].yvariable];
					if (FrameCount = 1) and (s in showshell) then
						begin
							st := st + 1;
							if masspointscale then
								x := s
							else
								x := star.eq[s].vari[radi];
							y := graphscales[ft].ymin;
							setcolor(st+1);
							plotline(x, y, x, -y);
                     print(1,st+1,Numstr(s,2,0));
						end; {IF S}
				end;  {FOR S}
 {erase old line}

		if FrameCount > 1 then
			if not justCleared then
				Drawline(ft, frame[ft].yvariable, old);
 {draw new line}
		DrawLine(ft, frame[ft].yvariable, new);   {need a better way to select color}
 {reset old data}
		with LinePlot[ft] do
			for s := 1 to numshells do
				OldData[s] := Newdata[s];
		setcolor(white);
		setwritemode(copyput);
	end;    {doLinePlot}
 {======================================}
	procedure DoStripChart (f: integer);
		const
			old = 1;
			new = 2;
         myRect:array[1..4] of pointType=((x:30;y:25),(x:180;y:25),
         (x:180;y:36),(x:30;y:36));
		var
			s, st, tt: integer;
 {------------------------------}
		procedure DrawStripLine (f, myset, s, ttt: integer);
			var
				t, tn: integer;
				xb, yb, xe, ye,fact: real;
		begin
			setcolor(s + 1);
         {fact:=dt[equation]*skipframe/8.64e4;}

         fact:=1;
			if frame[f].yvariable = bolmag then
				setcolor(2);
			with stripchart[f] do
				begin
					for t := ttt - 1 downto ttt - maxNumPoints + 2 do
						begin
							if t < 1 then
								tn := t + maxnumPoints
							else
								tn := t;
							if tn = maxnumpoints then
								tn := 1;{avoids empty slot}
							xb := maxNumPoints - (ttt - t);
							xe := xb - 1;
							if myset = old then
								begin
             {setcolor(graphbackcolor); }
									yb := olddata[s, tn];
									ye := oldData[s, tn - 1];
								end
							else
								begin
									yb := newdata[s, tn];
									ye := newdata[s, tn - 1];
								end;

							PlotLine(xb*fact, yb, xe*fact, ye);
						end;
				end;{WITH STRIPCHART}
			setcolor(white);
		end;

 {-------------------------}
	begin {Main DoStripChart}
		selectviewport(f);
		selectscale(f);
		setWriteMode(xorput);
		st := 0;
		with stripchart[f] do
			begin
				for s := numshells downto 1 do
					if s in showshell then
						if (frame[f].yvariable <> bolmag) or (s = numshells) then
							begin
								st := st + 1; {increment plotting shell counter}
								if st < 6 then      {can only plot 5 shells in strip chart}
									begin
										tt := ((FrameCount - 1) mod (maxNumpoints)); {data-plot index }
          {note tt never reaches maxNumPoints}
										newdata[st, tt] := star.dyn[s].vari[frame[f].yvariable];
										if FrameCount > 1 then {skip this the first time}
											begin
												if not justcleared then
													drawstripLine(f, old, st, tt - 1);
												drawStripLine(f, new, st, tt);
											end
										else
											begin
												drawstripline(f, new, st, tt);
											end; {if counter}
										olddata[st, tt] := newdata[st, tt];
									end; {IF ST<6 }

							end; {for s, if s}
            fillpoly(4,myRect);
				setwriteMode(copyput);
            setcolor(black);
            print(5,2,concat('dt = '+scNumStr(dt[equation]*skipframe/8.64e4,2)+' days'));
            setcolor(white);
			end;{WITH STRIPCHART}

	end;
 {=====================================}
 {==========================================}

	procedure DoPhaseDiagram (f: integer);
		var
			xb, yb, xe, ye: real;
			s, c: integer;
	begin
		selectviewport(f);
		SelectScale(f);
      {setWriteMode(xorput);}
		with PhaseDiag[f], star do
			begin
				c := 0;
				for s := numshells downto 1  do
					begin
						if s in showshell then
							if ((frame[f].xvariable <> bolmag) and (frame[f].yvariable <> bolmag)) or (s = numshells) then
								begin
									NewXdata[s] := dyn[s].vari[frame[f].xvariable];
									newYdata[s] := dyn[s].vari[frame[f].yvariable];
									if Framecount > 1 then
										begin
											c := c + 1;
											xb := oldXdata[s];
											yb := oldYdata[s];
											xe := newXdata[s];
											ye := newYdata[s];
											setcolor(c + 1);
											plotline(xb, yb, xe, ye);
                                  print(1,c+1,Numstr(s,2,0));
										end;{if counter}
									oldxdata[s] := newXdata[s];
									OldYdata[s] := newYdata[s];

								end;{if s in}
					end;{for s}
        {setwritemode(copyput);}

			end;{with star}
		setcolor(white);
	end;
 {-------------------------------------------------}


	procedure DoAnimation (f: integer);
		var
			s, c, xc, yc,i: integer;
			rad: word;
      {-----------------------------}
      function findFillColor(top,bot,val:real):integer;
                var c,j:integer;
                begin
                     j:=trunc((val+top)/(2*top)*4)+1;
                     case j of
                    { 1: c:=2;   {green}
                    { 2: c:=10;  {light green}
                    { 3: c:= 14;  {yellow}
                   {  else c:= 15;}
                     1:c:=15;
                     2:c:=14;
                     3:c:=10;
                     else c:=2;
                     end;
                     findFillColor:=c;
                end;{findFillColor}
      {-----------------------------}
	begin
		openviewport(f); {refresh}
		selectviewport(f);
      setcolor(white);
      {print(6, 1, concat(numstr(f,1,0)+'. Animation ')); }
      print(4, 1, concat(numstr(f,1,0)+'. Animation: ' + NatMyLabel[frame[f].yvariable] )) ;
		selectscale(f);
      {setwritemode(xorput);}
		xc := getMaxX div 6;
		yc := getMaxY div 6;
		c := 0;
		for s := numshells downto 1 do
			if s in showshell then
				begin
					c := c + 1;
					setcolor(c + 1);
					rad := trunc(0.9*xc / graphscales[f].xmax * star.dyn[s].vari[radi] / 1.5);
                print(1,c+1,Numstr(s,2,0));
               setLineStyle(solidLn,0,thickwidth); {thickness of shell lines}
              { ellipse(xc, yc, 0, 360, rad, rad); }
              i:=frame[f].yvariable;
               setfillstyle(solidfill,findFillColor(maxrelVal[i],MinRelval[i],star.dyn[s].vari[i]));
               fillellipse(xc, yc,rad, rad);
               setLineStyle(solidLn,0,normwidth);
				end;
      setfillstyle(solidfill,white);
		setcolor(white);
      {setwritemode(copyput);}
	end;
 {========================================}

	procedure PlotFrame (f: integer);
	begin
		case frame[f].kind of
			StripK: 
				doStripChart(f);
			lineplotK: 
				DoPlotPerturbation(f);
			PhaseDiagk: 
				DoPhaseDiagram(f);
			AnimDiagK: 
				doAnimation(f);
		end;
	end;

 {========================================}
	procedure PlotFrames;
		var
			f: integer;
	begin
		for f := 1 to 4 do
			PlotFrame(f);
	end;
 {===================================}
	procedure CLOSEBUTTONS;
		var
			f: integer;
	begin
		for f := 1 to 4 do
			begin
				FTypeButton[f].done;
				FVarButton[f].done;
			end;
	end;

 {-------------------------------------------------}
end.