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

Program ShellModels;
{$M 65520,0,655360}
	Uses
		Graph, CupsMupp, CUPS, CupsProc, CupsFunc, CupsGui, CupsGrph, Isotope;

	Const
		RBNames = '12345678';{way of storing ID names of radio-buttons}
		proton = true;
		neutron = false;
		TitleFont = 3;
		LabelFont = 3;
		fntsize = 9;

      Models2o_Output : String = 'models2o.dat';

	Type
		potlType = (SW, HO, WS);{Square Well, Harmonic Oscillator, Woods-Saxon}
		NucleiType = (Al, Eu, Dy, Tm, Hf, Pu);
		Str80 = String[80];
      Str6 = string[6];
		ShellModelType = Record
				potl: potlType;
				potStr: Str80;
				depth: real;
				d: real;
				neutrons: integer;
				protons: integer;
				A: integer;
				radius: real;
				lambda: real;
				drawbool: boolean;
			End;

       DataArray = Record
           iso : Array[1..72] of Integer;
           energy : Array[1..72] of real;
           level_str : Array[1..72] of Str6;
           degeneracy : Array[1..72] of Integer;
       End;

		BasisType = Record
				HO_N: integer;
				L: integer;
				lambda: real;
				sigma: real;
			End;
		InitialStatesMatrix = Record
				energy: Array[1..15] Of real;
				count: Array[1..15] Of integer;
			End;
		stateStrType = Array[1..72] Of Str20;
		Strarray = Array[1..16] Of Str20;
		LevelRecordType = Record
				energy: Array[1..72] Of real;
				stateStr: stateStrType;
			End;
		InitialLabelType = Record
				xpos: Array[1..16] Of integer;
				ypos: Array[1..16] Of integer;
				oddN: Array[1..16] Of boolean;
				InitialStr: Strarray;
			End;{InitialLabelType}
		LevelIndexType = Record
				xpos: integer;
				ypos: integer;
				LevelIndexStr: Str80;
			End;{LevelIndexType}
		LevelStorageType = Record
				reducedE: Array[1..72, 1..31] Of real;
				stateStr: stateStrType;
				InitialLabel: InitialLabelType;
				LevelColor: Array[1..72] Of integer;
				LevelIndexArray: Array[1..72] Of LevelIndexType;
			End;{LevelStorageType}
		statesStrType = Array[1..3] Of Str20;
		extraType = Record
				prot: integer;
				neut: integer;
				states: stateType;
				statesStr: statesStrType;
			End;{extra record}
		IsoParamType = Record
				Nmin: integer;
				Nmax: integer;
				A: integer;
				SingleDelta: real;
				yscalemin: real;
				yscalemax: real;
				kappa: real;
				mu: real;
				mufactor: real;
				title: Str80;
				NucleonsBelow: integer;
				OddNumNucleons: integer;
				MNlabel: Array[1..3] Of Str20;
				MNcoord: Array[1..3] Of real;
				extra: extraType;
			End;{IsoParamType record}
		NucPosType = Record
				xpos: integer;
				ypos: integer;
				filled: boolean;
			End;{NucPosType}
		DrawStateRecord = Record
				NucPos: Array[1..129] Of NucPosType;
				ModelExcitedStr: Str20;
				ExpExcitedStr: Str20;
			End;{DrawStateRecord}
		DrawType = Record
				groundState: DrawStateRecord;
				firstState: DrawStateRecord;
				secondState: DrawStateRecord;
				NumNucleons: integer;
			End;{DrawType}
		arrayBasis = Array[1..8] Of BasisType;
		labelmatrix = Array[1..2, 1..76] Of integer;
		LevelArrayType = Array[1..8, 1..31] Of real;
		Realarray4 = Array[1..4] Of real;
		Str1 = String[1];
      ModeType = ( Main, dNuc, dPot, sNuc, sPot );
type
      TShellPotential = object
         procedure Init;
         procedure SetUp;
      private
         r,InitV   : DVector;
			potential : ShellModelType;
	      ro, mnc2, hbarc, Lconst, dr : real;
         NPts : integer;
      end;

      TShellNuclei = object
         active : boolean;
         procedure Init;
         procedure Free;
         procedure SetUp;
         procedure Control;
      private
         SMNucData : SMNucleiType;
			noerror: boolean;
         buttons : TButtons;
      end;

      TDeformedPotential = object
         active : boolean;
         procedure Init;
         procedure Free;
         procedure SetUp;
         procedure Control;
      private
   		IsoParam: IsoParamType;
		   LevelStorage: LevelStorageType;
		   TotalStateCount, InitialStateCount: integer;
         pixeldelta: integer;
      end;

      TDeformedNuclei = object
         active : boolean;
         procedure Init;
         procedure Free;
         procedure SetUp;
         procedure Control;
      private
   		IsoParam: IsoParamType;
         DrawRecord : DrawType;
         buttons : TButtons;
   		ExcitedState: stateType;
      end;

	Var
   	scrx1, scrx2, scrx3, scrx4, scrx5, scrx6: integer;
   	OffSetnL, OffSetnLj, OffSetnum, OffSet2L, OffSetj: integer;
		delta, smpi, Omega, Sigma, theta, Mu, lambda: char;
		labelheight : integer;

      icount,iicount : integer;
      Outdata :DataArray;

      hbarwo, esqr: real;
		linelength : integer;

		MainMenu: TMenu;
		ShellModelHotKeys, NilssonHotKeys, MainHotKeys: THotKeys;
      ShellPot    : TShellPotential;
      ShellNuc    : TShellNuclei;
      DeformedPot : TDeformedPotential;
      DeformedNuc : TDeformedNuclei;
      mode : ModeType;
      QuitFlag, escDef: boolean;

   procedure HandleMainMenu; forward;

   procedure WhereIsMouse(vNum,sNum : integer; var x,y: real);
   var
      xFactor, yFactor : real;
   begin
      WITH Views[vNum] DO
         WITH GraphScales[sNum] DO
         BEGIN
            xFactor := (xmax - xmin) / (Vx2 - Vx1);
            yFactor := (ymax - ymin) / (Vy2 - Vy1);
            X       := (xFactor * (event.x - Vx1)) + xmin;
            Y       :=-(yFactor * (event.y - Vy1)) + ymax;
         END;
   end;

	Procedure EraseXY (x, y: integer;
									num, BkColor: word);
 { It erase num characters, starting from location x,y.}
		Var
			col, row, color, h, w: integer;
			fill: FillSettingsType;
			text: TextSettingsType;
	Begin
		color := GetColor;
		SetColor(BkColor);
		GetFillSettings(fill);
		GetTextSettings(text);
		SetFillStyle(SolidFill, BkColor);
		h := TextHeight('W');
		w := num * TextWidth('W');
		Col := Round(x - w * text.horiz / 2);
		Row := Round(y - h * (2 - text.vert) / 2);
		Bar(Col, Row, Col + w, Row + h);
		SetColor(Color);
		With fill Do
			SetFillStyle(Pattern, Color);
	End;

   procedure DrawCircle (x,y : real; r, Color: integer);
   var sx,sy : integer;
   begin
      Map(x,y, sx,sy);
      Setfillstyle(solidfill, color);
      SetColor(Color);
      fillellipse(sx, sy, r, r);
   end;

{********Menus, HotKeys, and Help Screens********}

	Procedure SetUpMainMenu;
	Begin
		MainMenu.Init;
		With MainMenu Do Begin
				column(1, 'File');
				Row(1, 1, 'About CUPS');
				Row(1, 2, 'About Program');
				Row(1, 3, 'Configuration');
            Row(1, 4, '-------------');
				Row(1, 5, 'Exit');
				column(2, 'Potential');
				row(2, 1, 'Spherical');
            row(2, 2, 'Deformed:');
				row(2, 3, '  2-20   for N or Z');
				row(2, 4, '  50-82  for Z');
				row(2, 5, '  82-126 for N');
				row(2, 6, '  Z > 82');
				row(2, 7, '  N > 126');
				column(3, 'Nuclei');
				row(3, 1, 'Spherical');
            row(3, 2, 'Deformed:');
				row(3, 3, '   25 Al');
				row(3, 4, '  153 Eu');
				row(3, 5, '  161 Dy');
				row(3, 6, '  169 Tm');
				row(3, 7, '  177 Hf');
				row(3, 8, '  239 Pu');
				Display;
            RowActivate(1,4,false);
            RowActivate(2,2,false);
            RowActivate(3,2,false);
			End;
	End;{SetUpMainMenu}

   procedure EraseMenu;
   begin
      SetFillStyle(SolidFill,blakk);
      bar(0,0,GetMaxX,20);
   end;

	Procedure SetUpShellModelHotKeys (text: Str20);
	Begin
		With ShellModelHotKeys Do Begin
				init(4);
				key[1] := 'F1-Help';
				key[2] := text;
				key[3] := 'F7-Pause';
				key[4] := 'ESC-Stop';
				Display;
			End;
	End;{SetUpShellModelHotKeys}

	Procedure SetUpNilssonHotKeys;
	Begin
		With NilssonHotKeys Do Begin
				init(2);
				key[1] := 'F1-Help';
				key[2] := 'ESC-Stop';
				Display;
			End;
	End;{SetUpNilssonHotKeys}

	Procedure SetUpMainHotKeys;
	Begin
		With MainHotKeys Do Begin
				init(2);
				key[1] := 'F1-Help';
				key[2] := 'F10-Menu';
				Display;
			End;
	End;{SetUpMainHotKeys}

   procedure SetUpButtons(var Buttons : TButtons);
   begin
	   With Buttons Do
      Begin
		   init;                            {initialize object}
		   create(1, 0.014, 0.136,'Ground State');  {define position of each button ...}
		   create(2, 0.014, 0.101,'First Exited State');  {... and its positions}
		   create(3, 0.014, 0.065,'Second Exited State');
		   number := 1;                     {initialy button 1 will be chosen}
		   DrawAll;                         {draw all buttons}
	   End;
   end;

   procedure OpenMess;
     var helpScr: HelpScrType;
   begin
      helpScr[1] := '                                                  ';
      helpScr[2] := '                                                  ';
      helpScr[3] := '                Nuclear Shell Model               ';
      helpScr[4] := '                                                  ';
      helpScr[5] := '               by Roberta A. Bigelow              ';
      helpScr[6] := '               Willamette University              ';
      helpScr[7] := '         (c) 1995 John Wiley & Sons, Inc.         ';
      helpScr[8] := '                                                  ';
      helpScr[9] := '                                                  ';
      helpScr[10] := '   This program  calculates energy levels for     ';
      helpScr[11] := '   spherical and deformed nuclei using the single ';
      helpScr[12] := '   particle shell model. You can explore how the  ';
      helpScr[13] := '   nuclear potential shape, the spin-orbit        ';
      helpScr[14] := '   interaction, and deformation affect both the   ';
      helpScr[15] := '   order and spacing of nuclear energy levels. In ';
      helpScr[16] := '   addition, you will learn how to predict the    ';
      helpScr[17] := '   spin and parity for single particle states.    ';
      helpScr[18] := '                                                  ';
      helpScr[19] := '                                                  ';
      helpScr[20] := ' Menu choices are:                                ';
      helpScr[21] := '   File      - Project and program information    ';
      helpScr[22] := '   Potential - Spherical and Deformed potentials  ';
      helpScr[23] := '   Nuclei    - Filling of energy levels           ';
      helpScr[24] := '                                                  ';
      helpScr[25] := '                                                  ';
      help(helpScr);
   end;{OpenMess}

{******** end Menus, HotKeys, and Help Screens********}

   procedure CleanMemory;
   begin
      If ShellNuc.active then ShellNuc.Free;
      If DeformedNuc.active then DeformedNuc.Free;
      If DeformedPot.active then DeformedPot.Free;
   end;



	Procedure SetGreek;
	Begin
		delta := chr(235);
		smpi  := chr(227);
		Omega := chr(234);
		Sigma := chr(229);
		theta := chr(233);
		Mu    := chr(230);
      lambda:= chr(239)
	End;{SetGreek}



{*****************Shell Model Potential Routines******************}



	Procedure InitialShellModelParam (Var potential: ShellModelType);
	Begin
		With potential Do Begin
				potl := WS;
				potStr := concat('Woods Saxon Potential, A=', NumStr(A, 3, 0), ', d=', NumStr(d, 3, 2));
				depth := 57;
				d := 0.65;
				neutrons := 21;
				protons := 20;
				A := neutrons + protons;
				lambda := -0.7;
				drawbool := true;
			End;{with potential}
	End;{Initialparameters}

	function PotentialInput (Var potential: ShellModelType) : boolean;
		Var
			V0, V0corr: real;
			temp: integer;
			PotentialName: integer;
			MScreen: TInputScreen;
	Begin
		With MScreen Do Begin
				init;
				DefineInputPort(0.04, 0.95, 0.1, 0.9);
				LoadLine('           NUCLEAR POTENTIAL INPUT FOR THE SPHERICAL SHELL MODEL');
				LoadLine('');
				LoadLine(' Nuclear Potential  ');
				LoadLine('  #1 Square Well         : V(r)=-V(0) for r<R and V(r)=0 for r>R');
				LoadLine('  #1 Harmonic Oscillator : V(r)=-V(0)(1-(r/R)^2) ');
				LoadLine('  #1 Woods-Saxon         : V(r)=-V(0)/(1+exp((r-R)/d))  ');
				LoadLine('');
				LoadLine(' Potential Depth                      V(0) = {     }MeV (10 to 100)');
				LoadLine(' Diffusiveness of Woods-Saxon potential  d = {     }fm >0');
				LoadLine(' Number of Neutrons for nucleus          N = {     } (0 to 226)');
				LoadLine(' Number of Protons  for nucleus          Z = {     } (1 to 115)');
				LoadLine(' The sum of N and Z must be odd.');
				LoadLine('');
				LoadLine(' Spin-Orbit Potential has the form  Vso = cf(r) = c (1/r)(dV/dr)l'+chr(249)+'s');
				LoadLine(' where c={      }is the strength of the spin-orbit potential ');
				LoadLine('');
				LoadLine('  #T  Draw the intermediate solutions');{in addition to the final numeric solutions}
				LoadLine('            [  Ok  ]   [ Help ]   [Cancel]');
				With potential Do Begin
{moving your data to the input screen}
						SetBoolean(9, drawbool);
						Case potl Of
							SW: SetRadioButton('1', 1);
							HO: SetRadioButton('1', 2);
							WS: SetRadioButton('1', 3);
						End;{case}
						SetNumber(4, depth);
						SetNumber(5, d);
						SetNumber(6, neutrons);
						SetNumber(7, protons);
						SetNumber(8, lambda);
						SetHelpFile('ShellMod.hlp ', 'HelpScreen6');
                  SetNumberLimits(4, 10.0, 100.0);
                  SetNumberLimits(5, 0, 1e20);
                  SetNumberLimits(6, 0, 226);
                  SetNumberLimits(7, 1, 115);
                  SetNumberLimits(8, -2.0, 2.0);
						Repeat{until odd A}
							AcceptScreen;
							neutrons := trunc(GetNumber(6));
							protons  := trunc(GetNumber(7));
							A := neutrons + protons;
							If not odd(A) Then
								announce('An Odd A nucleus must be entered.');
						Until odd(A) Or canceled;
						potentialname := GetRadioButton('1');
						radius := shellPot.ro * pwr(A, 1 / 3);
						Case potentialname Of
							1:  Begin
									potl := SW;
									potStr := concat('Square Well Potential, A = ', NumStr(A, 3, 0), ', no s-o term');
									d := 0.0;
								End;
							2:  Begin
									potl := HO;
									potStr := concat('Harmonic Oscillator Potential, A=', NumStr(A, 3, 0));
									d := 0.0;
								End;
							3:  Begin
									potl := WS;
									d := GetNumber(5);
                           if d <= 0.0 then
                              d := 0.65;
									potStr := concat('Woods Saxon Potential, A=', NumStr(A, 3, 0), ', d=', NumStr(d, 3, 2));
								End;
						End;{case}
						V0 := abs(GetNumber(4));
						depth := V0; {+ V0corr}
						lambda := GetNumber(8);
						drawbool := GetBoolean(9);
					End;{with potential}
            PotentialInput := not Canceled;
				done;
			End;{with MScreen}
	End;{PotentialInput}

	Procedure GetSMDVectors (Var Veff, NucleonCount1, NucleonCount2: DMatrix;
									Var A1, u1: DVector;
									Var labelcheck1: DMatrix;
									Var labelcheck2: IDVector;
                           NPts : integer);
	Begin
		A1.Init(NPts + 1);
		u1.Init(NPts + 1);
		Veff.Init(3, NPts + 1);
		labelcheck1.init(2, 72);
		labelcheck2.init(72);
		NucleonCount1.init(2, 72);
		NucleonCount2.init(2, 72);
	End;{GetDVectors}

	Procedure InitDVector (Var a: dVector;
									dSize: integer);
	Begin
		a.init(dSize);
	End;{InitDVector}

	Procedure FreeDVector (Var a: dVector);
	Begin
		a.free;
	End;{FreeDVector}

	Procedure FreeSMDVectors (Var Veff, NucleonCount1, NucleonCount2: DMatrix;
									Var A1, u1: DVector;
									Var labelcheck1: DMatrix;
									Var labelcheck2: IDVector);
	Begin
		Veff.free;
		NucleonCount1.free;
		NucleonCount2.free;
		A1.free;
		u1.free;
		labelcheck1.free;
		labelcheck2.free;
	End;{FreeSMDVectors}

	Procedure CalcPosition (dx: real;
									Var x: DVector);
		Var
			i: integer;
	Begin
		x.Put(1, 0.0);
		For i := 2 To x.size Do
			x.Put(i, x.value(i - 1) + dx);
	End;{CalcPosition}

	Procedure CalcVcoul (var potential: ShellModelType;
									Var r: DVector;
									Var Vcoul: dvector);
		Var
			i: integer;
			term, qsqr, qsqrOverR, esqr: real;
	Begin
	   esqr := 1.44;{Mev.fm}
		With potential Do Begin
				qsqr := protons * esqr;
				qsqrOverR := (qsqr / radius);
				For i := 1 To r.size Do Begin
						If r.Value(i) < radius Then Begin
								term := (qsqrOverR) * (1 + 0.5 * (1 - sqr(r.Value(i) / radius)));
								Vcoul.Put(i, term);
							End
						Else Begin
								term := qsqr / r.Value(i);
								Vcoul.Put(i, term);
							End;
					End;{for}
			End;{with potential}
	End;{CalcVcoul}

	Procedure CalcSqrWell (dx: real;
									var potential: ShellModelType;
									Var x, y: DVector);
		Var
			i, Rindex, temp: integer;
			Vcoul: DVector;
	Begin
		InitDVector(Vcoul, y.size);
		With potential Do Begin
				Rindex := 2 * round(radius / (2 * dx)) + 1;
				CalcPosition(dx, x);
				temp := 2 * round(protons / 2);
				If temp <> protons Then
					CalcVcoul(potential, x, Vcoul)
				Else
					Vcoul.fill(0.0);
				For i := 1 To Rindex Do
					y.Put(i, -1.0);
				For i := Rindex + 1 To y.size Do
					y.Put(i, 0.0);
			End;{with potential}
		FreeDVector(Vcoul);
	End;{CalcSqrWell}

	Procedure CalcHarOsc (dx, mnc2: real;
									var potential: ShellModelType;
									Var x, y: DVector);
		Var
			i, tempint: integer;
			w2, temp: real;
			R2: real;
			Vcoul: DVector;
	Begin
		InitDVector(Vcoul, y.size);
		With potential Do Begin
				R2 := sqr(radius);
				CalcPosition(dx, x);
				tempint := 2 * round(protons / 2);
				If tempint <> protons Then
					CalcVcoul(potential, x, Vcoul)
				Else
					Vcoul.fill(0.0);
				w2 := depth * 2 / (mnc2 * R2);
				For i := 1 To y.size Do Begin
						temp := (0.5 * mnc2 * w2 * (sqr(x.Value(i)) - R2) + Vcoul.Value(i)) / depth;
						y.Put(i, temp);
					End;
			End;{with potential}
		FreeDVector(Vcoul);
	End;{CalcHarOsc}

	Procedure CalcWS (dx: real;
									var potential: ShellModelType;
									Var x, y: DVector);
		Var
			i, temp: integer;
			denominator: real;
			Vcoul: DVector;
	Begin
		InitDVector(Vcoul, y.size);
		With potential Do Begin
				CalcPosition(dx, x);
				temp := 2 * round(protons / 2);
				If temp <> protons Then
					CalcVcoul(potential, x, Vcoul)
				Else
					Vcoul.fill(0.0);
				For i := 1 To y.size Do Begin
						denominator := 1 + exp((x.Value(i) - radius) / d);
						y.Put(i, ((-1 / denominator) + (Vcoul.Value(i) / depth)));
					End;
			End;{with potential}
		FreeDVector(Vcoul);
	End;{CalcWS}

	Procedure CalcVso (var potential: ShellModelType;
									L: integer;
									Var r: DVector;
									Var Vso: dmatrix);
		Var
			temp, dVdr, jplus, jminus: real;
			w2: real;
			i: integer;
	Begin
		With potential Do Begin
				jplus := L * 0.5;
				jminus := -0.5 * (L + 1);
				Case potl Of
					HO:  Begin
							w2 := 2 / (ShellPot.mnc2 * sqr(radius));
							dVdr := ShellPot.mnc2 * w2;
							For i := 1 To r.size Do Begin
									Vso.Put(1, i, jplus * lambda * dVdr);
									Vso.Put(2, i, jminus * lambda * dVdr);
								End;
						End;
					WS:  Begin
							Vso.Put(1, 1, 1e10 / depth);
							Vso.Put(2, 1, 1e10 / depth);
							For i := 2 To r.size Do Begin
									temp := exp((r.Value(i) - radius) / d);
									dVdr := temp / (d * sqr(1 + temp));
									Vso.Put(1, i, jplus * lambda * dVdr / r.Value(i));
									Vso.Put(2, i, jminus * lambda * dVdr / r.Value(i));
								End;
						End;{WS}
				End;{case pot}
			End;{with potential}
	End;{CalcVso}

	Procedure CalcVeff (var potential: ShellModelType;
									Var initV, r: DVector;
									L, NPts: integer;
									Var Veff: dmatrix);
		Var
			i: integer;
			Lterm: real;
			Vso: dmatrix;
	Begin
		With potential Do Begin
				Vso.Init(2, NPts);
				For i := 1 To NPts Do
					Veff.Put(1, i, initV.Value(i));
				If (L > 0) Then Begin
						Veff.Put(1, 1, 3e6 / depth);
						Veff.Put(2, 1, 3e7 / depth);
						Veff.Put(3, 1, 3e7 / depth);
					End;
				If (L > 0) And (potential.potl <> SW) Then
					CalcVso(potential, L, r, Vso);
				If potl = SW Then
					Vso.fill(0);
				For i := 2 To NPts Do Begin
						Lterm := L * (L + 1) / (ShellPot.LConst * sqr(r.Value(i)));{angular momentum term}
						Veff.Put(1, i, initV.Value(i) + Lterm / depth);{effective potential, scaled to depth}
						Veff.Put(2, i, initV.Value(i) + Vso.value(1, i) + Lterm / depth);{effective potential, scaled to depth}
						Veff.Put(3, i, initV.Value(i) + Vso.value(2, i) + Lterm / depth);
					End;{for i}
				Vso.free;
			End;{with potential}
	End;{CalcVeff}

	Procedure Getfirstvalues (L, iso: integer;
									Var Veff: dmatrix;
									Var ifirst: integer;
									Var E: real);
		Var
			i: integer;
	Begin
		i := 2;
		While (i < VEff.NCols) And (Veff.value(iso, i) < Veff.value(iso, i - 1)) Do
			inc(i);

		ifirst := 2;{index + 1;}
		E := Veff.value(iso, i - 1);  {set E equal to the smallest value of Veff}
	End;{Getfirstvalues}

	Procedure MakeA1DVector (E, dr: real;
									var potential: ShellModelType;
									NPts, iso: integer;
									Var Veff: dmatrix;
									Var A1: DVector;
									Var imatch: integer);
		Var
			i: integer;
			ksqr, temp: real;
			match: boolean;
	Begin
		With potential Do Begin
				A1.Put(1, 0.0);
				For i := 2 To NPts Do	{fill A1 DVector for forward integration}
					Begin
						match := false;
						ksqr := (E - Veff.value(iso, i)) * (-ShellPot.LConst) * depth;
						A1.Put(i, ksqr * sqr(dr) / 12);
						If (A1.value(i - 1) * A1.Value(i) <= 0) And (A1.value(i - 1) < 0) Then
							If Not match Then Begin
									match := true;
									imatch := i;
								End;
					End;{for i}
			End;{with potential}
	End;{MakeA1DVector}

	Procedure StartLeft ({n,}
									L, ifirst: integer;
									Var y: DVector);
		Var
			tempint: integer;
			sign: real;
	Begin
		sign := 1;
		If L = 1 Then
			y.Put(1, sign * 3e-8)
		Else
			y.Put(1, 0.0);
		y.Put(2, sign * 1.0);
	End;{StartLeft}

	Procedure StartRight (E: real;
									var potential: ShellModelType;
									NPts, n, m: Integer;
									Var x: DVector;
									Var Veff: dmatrix;
									Var y: DVector);
		Var
			temp: real;
			sign: real;
			tempint: integer;
	Begin
		With potential Do Begin
				tempint := trunc(n / 2) * 2;
				If tempint = n Then
					sign := -1
				Else
					sign := 1;
				temp := sqrt(abs((E - Veff.value(m, NPts)) * (-ShellPot.LConst) * depth));
				temp := sign * exp(-temp * x.value(Npts));
				y.Put(NPts, temp);
				temp := sqrt(abs((E - Veff.value(m, NPts - 1)) * (-ShellPot.LConst) * depth));
				temp := sign * exp(-temp * x.value(Npts - 1));
				y.Put(NPts - 1, temp);
			End;{with potential}
	End;{StartRight}

	Procedure InitDrawWaveFnct (Var r, initV: Dvector);
		Var
			x1, x2, y1, y2: integer;
	Begin
		OpenViewPort(2);
		SelectScale(1);{for E and initV}
		setColor(whitte);
		Axis(0, 0, 2, 0.1);
		PutLabel(Left, 'E/Vo');
		PutLabel(bottom, 'r (fm)');
		setColor(lightmagenta);
		PlotDVectors(r, initV, 1, r.size);
		Map(0.5, -1.01, x1, y1);
		Map(14.5, -1.16, x2, y2);
		rectangle(x1, y1, x2, y2);
		Map(0.7, -1.01, x1, y1);
		OutTextXY(x1, y1 + labelheight + 4, 'L=0 Pot___');
		setcolor(whitte);
		OutTextXY(x1, y1 + 2 * labelheight + 6, 'Pot without Spin-Orbit__ ');
		setcolor(lightgreen);
		OutTextXY(x1, y1 + 3 * labelheight + 8, 'Pot with Spin-Orbit___');
		setcolor(lightblue);
		OutTextXY(x1, y1 + 4 * labelheight + 10, 'Wavefunction (Scaled)___');
	End;{InitDrawWaveFnct}

	function findLStr (L: integer) : Str20;
   var LStr : str20;
	Begin
		Case L Of
			0: LStr := 's';
			1: LStr := 'p';
			2: LStr := 'd';
			3: LStr := 'f';
			4: LStr := 'g';
			5: LStr := 'h';
			6: LStr := 'i';
		End;{case}
      findLStr := LStr;
	End;{findLStr}

	Procedure DrawApproxWaveFunc (E: real;
									NPts, ibeg, iend, iso, n, L, nodes: integer;
									redraw: boolean;
									Var r, initV: DVector;
									Var Veff: dmatrix;
									Var u1: DVector);
		Var
			i, ii, iiend, scrx, scry, j: integer;
			umax: real;
			LStr, statestr: Str20;
			text: Str80;
			Vplot, uplot: dVector;
	Begin
		InitDVector(Vplot, NPts);
      HideMouse;
		If nodes <> 0 Then Begin
				InitDVector(uplot, NPts);
				umax := abs(u1.value(ibeg));
				For i := 2 To iend Do
					If umax < abs(u1.value(i)) Then
						umax := abs(u1.value(i));
				umax := umax * 30;
				For i := ibeg To iend Do
					uplot.Put(i, (u1.Value(i) / umax) + E);
			End;
		If redraw Then Begin
				OpenViewPort(2);
				SetColor(lightmagenta);
				SelectScale(1);
				InitDrawWaveFnct(r, initV);
				If iso > 1 Then
					iiend := 3
				Else
					iiend := 1;
				For ii := 1 To iiend Do Begin
						For i := 1 To NPts Do Begin
								Vplot.Put(i, Veff.value(ii, i));
							End;
						If ii = 1 Then
							SetColor(whitte)
						Else
							SetColor(lightgreen);
						PlotDVectors(r, Vplot, 1, NPts);
					End;
			End
		Else
			SelectViewPort(2);
		scrx := MapX(6.0);
		scry := MapY(-0.99);
		LStr := findLStr(L);
		If iso > 1 Then Begin
				If iso = 2 Then
					j := round((L + 0.5) * 2)
				Else
					j := round((L - 0.5) * 2);
				stateStr := concat(LStr, NumStr(j, 2, 0), '/2');
			End
		Else
			stateStr := LStr;
		EraseXY(scrx, scry, 15, darkgray);
		If iso > 1 Then
			SetColor(lightgreen)
		Else
			SetColor(whitte);
		If n = 0 Then Begin
				text := concat('Calculating max. number');
				text := concat('Nodes: ', stateStr);
				OutTextXY(scrx, scry, text);
			End
		Else Begin
				text := concat('Calculating the energy');
				text := concat('Energy: ', NumStr(n, 1, 0), stateStr);
				OutTextXY(scrx, scry, text);
			End;
		PlotLine(0, E, 15, E);
		If nodes <> 0 Then Begin
				SetColor(lightblue);
				PlotDVectors(r, uplot, ibeg, iend);
				FreeDVector(uplot);
			End;
      ShowMouse;
		FreeDVector(Vplot);
	End;{DrawApproxWaveFunc}

	Function Isign (x: real): Integer;
	Begin
		If x >= 0 Then Isign := 1 Else Isign := -1;
	End;{Isign}

	Procedure Crossing (NPts, iso: integer;
									var Veff: dmatrix;
									Var icross: integer);
		Var
			i: integer;
			stopflag: boolean;
	Begin
		stopflag := false;
		icross := 0;{there was no crossing}
		i := 1;
		While (i <= NPts) And (Not stopflag) Do Begin
				inc(i);
				If (Veff.value(iso, i) >= -1.0e-4) And (Veff.value(iso, i - 1) < 0.0) Then Begin
						icross := i;
						stopflag := true;
					End;{if}
			End;{while}
	End;{Crossing}

	Procedure CountNodes (ibeg, iend: integer;
									var u1: dvector;
									Var nodes, index: integer);
		Var
			i, NodeCount, ThisSign, PrevSign: integer;
	Begin
		NodeCount := 0;
		PrevSign := Isign(u1.value(ibeg + 1));
		For i := ibeg + 2 To iend Do Begin
				ThisSign := Isign(u1.value(i));
				If ThisSign <> PrevSign Then Begin
						index := i;
						NodeCount := NodeCount + 1;
					End;
				PrevSign := ThisSign;
			End;
		nodes := nodeCount;
	End;{CountNodes}

	Procedure FindMaxNodes ({j,}
									L, NPts, iso: integer;
									var potential: ShellModelType;
									var r, initV: dvector;
									var Veff: dmatrix;
                           dr : real;
									Var maxnodes: integer);
		Const
			deltaE = 0.07;
		Var
			range, oldE: real;
			redraw: boolean;
			A2, u2: dvector;
			imatch, ifirst, ibeg, iend, icross, nNodes, nodes, index, Isign: integer;
	Begin
		InitDVector(A2, NPts);
		InitDVector(u2, NPts);
		MakeA1DVector(0.0, dr, potential, NPts, iso, Veff, A2, imatch);
		StartLeft(L, ifirst, u2);{j,}
		Crossing(NPts, iso, Veff, icross);
		If icross = 0 Then
			nodes := 0
		Else Begin
				ibeg := 1;
				iend := abs(icross) + 100;
				If iend > NPts Then
					iend := NPts;
				Numerov(ibeg, iend, dr, A2, u2);
				CountNodes(ibeg, iend, u2, nodes, index);
			End;{else of if icross=0}
		redraw := true;
		If potential.drawbool Then
			DrawApproxWavefunc(0.0, NPts, ibeg, iend, iso, 0, L, nodes, redraw, r, initV, Veff, u2);
		maxnodes := nodes;
		FreeDVector(A2);
		FreeDVector(u2);
	End;{FindMaxNodes}

	Procedure HandleShellModelHotKeys (key: byte;
									Var potential: ShellModelType;
                           var esckey: boolean);
	forward;



	Procedure ApproxELevel (j, L, NPts, iso: integer;
									var potential: ShellModelType;
									var r, initV: dvector;
									var Veff: dmatrix;
									Var E, dE, Einterval, dr: real;
									Var foundit, esckey: boolean);
		Const
			deltaE = 0.03;
		Var
			range, oldE: real;
			redraw, exitbool: boolean;
			A2, u2: dvector;
			imatch, ifirst, ibeg, iend, nNodes, nodes, index, Isign: integer;
			keybd: char;
			key: byte;

		Procedure block1;
		Begin
			comment(0.0, 0.95, '|');
			MakeA1DVector(E, dr, potential, NPts, iso, Veff, A2, imatch);
			StartLeft(L, ifirst, u2);{j,}
			ibeg := 1;
			iend := imatch + 50;
			If iend > NPts Then
				iend := NPts;
			comment(0.0, 0.95, '/');
			Numerov(ibeg, iend, dr, A2, u2);
			CountNodes(ibeg, iend, u2, nodes, index);
			comment(0.0, 0.95, '-');
			If potential.drawbool Then
				DrawApproxWavefunc(E, NPts, ibeg, iend, iso, j, L, 1, redraw, r, initV, Veff, u2);
			comment(0.0, 0.95, '\');
			oldE := E;
			range := range / 2;
			If nodes > nNodes Then
				E := oldE - range
			Else If nodes < nNodes Then
				E := oldE + range
			Else If nodes = nNodes Then Begin
					If index >= imatch Then {last node is outside the potential}
						E := oldE + range
					Else
						E := oldE - range;
				End;{if nodes=nNodes}
{			writeln('oldE', oldE, 'newE', E);}
		End;{block1}

	Begin
		InitDVector(A2, NPts);
		InitDVector(u2, NPts);
		range := abs(E / 2);
		E := E + range;
		nNodes := j;
		exitbool := false;
		foundit := false;
		If E >= 0.0 Then Begin
				foundit := false;
				exitbool := true;
			End;
		redraw := false;
		If (Not esckey) And (Not exitbool) Then
			Repeat      {(range < deltaE) and (nodes = nNodes)}
				CheckforEvents;
				If ShellModelHotKeys.Pressed(key) Then
					HandleShellModelHotKeys(key, potential, esckey);
				If event.keypressed Then Begin
						beep;
						esckey := (event.ReadKey = chr(27));
					End;
            exitbool := esckey;
				If Not escKey Then
					block1;

				If E >= 0.0 Then Begin
						exitbool := true;
						foundit := false;
					End;

				If (Not exitbool) And (Not esckey) Then Begin
						If (range < 0.007) And (nodes <> nNodes) Then
							If (E < 0.1) And (abs(nodes - nNodes) = 1) Then Begin
									dE := 0.04;
									E := E - deltaE;
									Einterval := 2 * deltaE;
									foundit := true;
									exitbool := true;
								End
							Else Begin
									exitbool := true;
									foundit := false;
								End;
					End;{if foundit and not esckey}
			Until ((range < deltaE) And (nodes = nNodes)) Or (exitbool);

		If Not exitbool Then Begin
				If (index >= imatch) Then
					dE := 0.04
				Else
					dE := -0.04;
				foundit := true;
				E := E - deltaE;
				Einterval := 2 * deltaE;{Einterval := deltaE;{range}
			End;{if}
		comment(0.0, 0.95, '   ');
		FreeDVector(A2);
		FreeDVector(u2);
	End;{ApproxELevel}



	Procedure GetLabelPositions (Var scrx1, scrx2, scrx3, scrx4, scrx5, scrx6, OffSetnL,{}
									OffSetnLj, OffSetnum, OffSet2L, OffSetj: integer);
	Begin
		OpenViewPort(3);
		SelectScale(1);
		scrx1 := MapX(0.125);
		scrx2 := MapX(1.75);
		scrx3 := MapX(1.80);
		scrx4 := MapX(4.95);
		scrx5 := MapX(10.25);
		scrx6 := MapX(10.3);
		OffSetnL := TextWidth('4s') - 2;
		OffSetnLj := TextWidth('4s1/23') - 1;
		OffSetnum := TextWidth('26');
		OffSet2L := TextWidth('2(2L+1)');
		OffSetj := TextWidth('(2j+1)') + 2;
	End;{GetLabelPostions}

	Procedure MNLabel (NumOfStates: integer;
									Var NucleonCount: DMatrix);
		Var
			i, j, scrx, scry, scry1: integer;
			MN: Array[1..8] Of integer;
	Begin
		MN[1] := 2;
		MN[2] := 8;
		MN[3] := 20;
		MN[4] := 28;
		MN[5] := 50;
		MN[6] := 82;
		MN[7] := 126;
		MN[8] := 186;
		SelectViewPort(3);
		SelectScale(1);
		SetColor(lightcyan);
		j := 1;
		For i := 1 To NumOfStates Do Begin
				If j < 8 Then
					If (NucleonCount.value(2, i) >= MN[j]) And (NucleonCount.value(2, i) < MN[j + 1]) Then Begin
							scry := MapY(NucleonCount.value(1, i)) + 2;{MapY((NucleonCount.value(1, i) + NucleonCount.value(1, i + 1)) / 2);}
							scrx := MapX(13.75);
							OutTextXY(scrx, scry, NumStr(MN[j], 3, 0));
							j := j + 1;
						End;
			End;{for i}
	End;{MNLabel}

	Procedure SortNucleonCount (NumOfStates: integer;
									Var NucleonCount: DMatrix);
		Var
			i, j: integer;
			tempenergy, tempcount: real;
			insert: boolean;
	Begin
		insert := false;
		For j := 2 To NumOfStates Do Begin
				tempenergy := NucleonCount.value(1, j);
				tempcount := NucleonCount.value(2, j);
				i := j - 1;    {look for place to insert it}
				While (i >= 1) And (NucleonCount.value(1, i) > tempenergy) Do Begin
						NucleonCount.put(1, i + 1, NucleonCount.value(1, i));
						NucleonCount.put(2, i + 1, NucleonCount.value(2, i));
						dec(i);
					End;
				insert := (NucleonCount.value(1, i) <= tempenergy);
				If Not insert Then
					i := 0;
				NucleonCount.put(1, i + 1, tempenergy);
				NucleonCount.put(2, i + 1, tempcount);		{insert it}
			End;{for j}
		For i := 2 To NumOfStates Do
			NucleonCount.Put(2, i, NucleonCount.value(2, i) + NucleonCount.value(2, i - 1));
	End;{SortNucleonCount}

	Procedure SortDataArray (icount: integer;
			         Var Outdata : DataArray);
	 Var
	    i, j, tempiso, tempdeg : integer;
	    tempenergy : real;
            templevel_str : STR6;
	    insert: boolean;
	 Begin
	   insert := false;
           For j := 2 To icount Do Begin
              tempiso := OutData.iso[j];
	           tempenergy := ABS(OutData.energy[j]);
              templevel_str := OutData.level_str[j];
              tempdeg := OutData.degeneracy[j];
              i := j - 1;    {look for place to insert it}
	      While (i >= 1) And (ABS(OutData.energy[i]) > tempenergy) Do Begin
                 OutData.iso[i+1] := OutData.iso[i];
                 OutData.energy[i+1] := OutData.energy[i];
                 OutData.level_str[i+1] := OutData.level_str[i];
                 OutData.degeneracy[i+1] := OutData.degeneracy[i];
		           dec(i);
	      End;  {While (i >= 1) ... }
              insert := (ABS(OutData.energy[i]) <= tempenergy);
	      If Not insert Then i := 0;
              OutData.iso[i+1] := tempiso;
              OutData.energy[i+1] := -tempenergy;
              OutData.level_str[i+1] := templevel_str;
              OutData.degeneracy[i+1] := tempdeg;
	   End;{for j}

        End;{SortDataArray}

	Procedure NucleonCountLabel (var potential: ShellModelType;
									NumOfStates, iso: integer;
									Var NucleonCount: DMatrix);
		Var
			i, scrx, scry, scry1, scry2: integer;
	Begin
		SelectViewPort(3);
		SelectScale(1);
		SortNucleonCount(NumOfStates, NucleonCount);
		scry := MapY(NucleonCount.value(1, 1) - 0.1);
		If potential.potl <> SW Then Begin
				setColor(lightgreen);
				OutTextXY(scrx6 + OffSetj + 5, scry, 'sum');
				setColor(whitte);
				OutTextXY(scrx3 - 5 + OffSet2L, scry, 'sum');
			End
		Else Begin
				SetColor(whitte);
				OutTextXY(scrx6 + OffSet2L - 10, scry, 'sum');
			End;
		SetColor(lightcyan);
		OutTextXY(MapX(14.3), scry, 'MN');
		scry := MapY(NucleonCount.value(1, 1));
		scry1 := MapY(NucleonCount.value(1, 2));
		If iso > 1 Then Begin
				scrx := scrx6 + OffSetj;
				SetColor(lightgreen);
			End
		Else Begin
				If potential.potl <> SW Then
					scrx := scrx3 + OffSet2L - 10
				Else
					scrx := scrx6 + OffSet2L - 10;
				SetColor(whitte);
			End;
		OutTextXY(scrx, scry, NumStr(NucleonCount.value(2, 1), 2, 0));
		OutTextXY(scrx, scry1, NumStr(NucleonCount.value(2, 2), 2, 0));
		For i := 3 To NumOfStates Do Begin
				scry := MapY(NucleonCount.value(1, i));
				scry1 := MapY(NucleonCount.value(1, i - 1));
				scry2 := MapY(NucleonCount.value(1, i - 2));
				If abs(scry - scry1) < labelheight Then
					If abs(scry1 - scry2) < labelheight Then
						OutTextXY(scrx + 2 * OffSetnum, scry, NumStr(NucleonCount.value(2, i), 2, 0))
					Else
						OutTextXY(scrx + OffSetnum, scry, NumStr(NucleonCount.value(2, i), 2, 0))
				Else
					OutTextXY(scrx, scry, NumStr(NucleonCount.value(2, i), 2, 0));
			End;{for i}
		If (iso = 2) Or (potential.potl = SW) Then
			MNLabel(NumOfStates, NucleonCount);
	End;{NucleonCountLabel}

	Procedure Draw_SqrWellWaveFnct (E: real;
									u1max: real;
									L, n, NPts: integer;
									Var r, initV: DVector;
									Var Veff: dmatrix;
									Var u1: DVector;
									Var labelcheck2: IDVector;
									Var labelindex2: integer);
		Var
			i, k, j, scry, templabel, horizOffset1, horizOffset2: integer;
			umax: real;
			Vplot, uplot: dVector;
			LStr, LevelStr: Str20;
	Begin
		InitDVector(Vplot, NPts);
		InitDVector(uplot, NPts);
      HideMouse;
		SelectViewPort(3);
		SelectScale(1);
		umax := u1max * 30;
		For i := 1 To NPts Do Begin
				uplot.Put(i, (u1.Value(i) / umax) + E);
				Vplot.Put(i, Veff.value(1, i));
			End;
		InitDrawWaveFnct(r, initV);
		SetColor(whitte);
		PlotDVectors(r, Vplot, 1, NPts);
		PlotLine(0, E, 15, E);
		SetColor(lightblue);
		PlotDVectors(r, uplot, 1, NPts);
{****************  Energy Levels Only  ***********}
		setColor(whitte);
		If (L = 0) And (n = 1) Then Begin
				OpenViewPort(3);
				SelectScale(1);
				scry := MapY(E - 0.1);
				OutTextXY(scrx5 - 18, scry, '2(2L+1)');
			End
		Else Begin
				SelectViewPort(3);
				SelectScale(1);
			End;
		lStr := findLStr(L);
		horizOffset1 := 0;
		horizOffset2 := 0;
		scry := MapY(E);
		For i := 1 To labelindex2 Do Begin
				templabel := labelcheck2.Value(i);
				If abs(scry - templabel) <= labelheight Then Begin
						horizOffset1 := OffSetnL;
						horizOffset2 := OffSetnum;
					End;{if}
			End;{for i}
		labelindex2 := labelindex2 + 1;
		labelcheck2.put(labelindex2, scry);
		Line(scrx1 + horizOffset1 + OffSetnL, scry, scrx5, scry);
		levelStr := concat(NumStr(n, 1, 0), LStr);
		OutTextXY(scrx1 + horizOffset1, scry, levelStr);
		OutTextXY(scrx6 + horizOffset2 - 2, scry, NumStr(2 * (2 * L + 1), 2, 0));
      ShowMouse;
		FreeDVector(Vplot);
		FreeDVector(uplot);
	End;{Draw_SqrWellWaveFnct}

	Procedure Draw_SWaveFnct (E: real;
									u1max: real;
									n, NPts{, iso}
									: integer;
									repeat1: boolean;
									Var r, initV: DVector;
									Var Veff: dmatrix;
									Var u1: DVector;
									Var labelcheck2: IDVector;
									Var SLevels: Realarray4;
									Var labelindex2, Scount: integer);
		Var
			i, k, j, scry, templabel, horizOffset: integer;
			umax: real;
			Vplot, uplot: dVector;
			LStr, LevelStr: Str20;
	Begin
      HideMouse;
		SelectViewPort(3);
		SelectScale(1);
		If repeat1 Then Begin
				SetColor(lightgreen);
				scry := MapY(SLevels[1] - 0.1);
				OutTextXY(scrx6 - 6, scry, '(2j+1)');
				For i := 1 To Scount Do Begin
						scry := MapY(SLevels[i]);
						Line  (scrx4 + OffSetnLj, scry, scrx5, scry);
						levelStr := concat(NumStr(i, 1, 0), 's 1/2');
						OutTextXY(scrx4, scry, levelStr);
						OutTextXY(scrx6, scry, '2');
					End;{for i}
			End{if iso>1}
		Else Begin
				InitDVector(Vplot, NPts);
				InitDVector(uplot, NPts);
				umax := u1max * 30;
				For i := 1 To NPts Do Begin
						uplot.Put(i, (u1.Value(i) / umax) + E);
						Vplot.Put(i, Veff.value(1, i));
					End;
				InitDrawWaveFnct(r, initV);
				SetColor(whitte);
				PlotLine(0, E, 15, E);
				SetColor(lightblue);
				PlotDVectors(r, uplot, 1, NPts);
{****************  Energy Levels Only  ***********}
				If n = 1 Then Begin
						OpenViewPort(3);
						SelectScale(1);
						scry := MapY(E - 0.1);
						setColor(whitte);
						OutTextXY(scrx3 - 18, scry, '2(2L+1)');
					End
				Else Begin
						SelectViewPort(3);
						SelectScale(1);
					End;
				SetColor(whitte);
				scry := MapY(E);
				Line  (scrx1 + OffSetnL, scry, scrx2, scry);
				levelStr := concat(NumStr(n, 1, 0), 's');
				OutTextXY(scrx1, scry, levelStr);
				OutTextXY(scrx3, scry, '2');
				SetColor(DarkGray);
				SetLineStyle(dottedLn, 0, NormWidth);
				Line  (scrx3, scry, scrx4, scry);
				SetColor(whitte);
				SetLineStyle(solidLn, 0, NormWidth);
				labelindex2 := labelindex2 + 1;
				labelcheck2.put(labelindex2, scry);
				FreeDVector(Vplot);
				FreeDVector(uplot);
			End;{else}
     ShowMouse;
	End;{Draw_SWaveFnct}

	Procedure DrawWaveFnct (E: real;
									u1max: real;
									n, L, NPts, iso: integer;
									Var r, initV: DVector;
									Var Veff: dmatrix;
									Var u1: DVector;
									var potential: ShellModelType;
									Var labelcheck1: DMatrix;
									Var labelcheck2: IDVector;
									Var labelindex1, labelindex2: integer);
		Var
			i, ii, iiend, k, j, scry, templabel, templabel1, horizOffset1, horizOffset2, numOffSet, itemp: integer;
			umax, temp: real;
			Vplot, uplot: dVector;
			LStr, LevelStr: Str20;
	Begin
		InitDVector(Vplot, NPts);
		InitDVector(uplot, NPts);
      HideMouse;
		umax := u1max * 30;
		For i := 1 To NPts Do
			uplot.Put(i, (u1.Value(i) / umax) + E);
		InitDrawWaveFnct(r, initV);
		If iso > 1 Then
			iiend := 3
		Else
			iiend := 1;
		For ii := 1 To iiend Do Begin
				For i := 1 To NPts Do
					Vplot.Put(i, Veff.value(ii, i));
				If ii = 1 Then
					SetColor(whitte)
				Else
					SetColor(lightgreen);
				PlotDVectors(r, Vplot, 1, NPts);
			End;
		PlotLine(0, E, 15, E);
		SetColor(lightblue);
		PlotDVectors(r, uplot, 1, NPts);
{****************  Energy Levels Only  ***********}
		SelectViewPort(3);
		SelectScale(1);
		scry := MapY(E);
		LStr := findLStr(L);
		horizOffset1 := 0;
		horizOffset2 := 0;
		If iso > 1 Then Begin
				SetColor(lightgreen);
				numOffSet := 0;
				For i := 1 To labelindex1 Do Begin
						templabel := round(labelcheck1.Value(1, i));
						If abs(scry - templabel) <= labelheight Then Begin
								itemp := round(labelcheck1.value(2, i));
								Case itemp Of
									0: numOffset := 1;
									1: numOffSet := 2;
									2: numOffSet := 1;
								End;{case}
								horizOffset1 := numOffSet * OffSetnLj;
								horizOffset2 := numOffSet * OffSetnum;
							End;
					End;{for i}
				labelindex1 := labelindex1 + 1;
				labelcheck1.put(1, labelindex1, scry);
				labelcheck1.put(2, labelindex1, numOffSet);
				If iso = 2 Then
					j := round((L + 0.5) * 2)
				Else
					j := round((L - 0.5) * 2);
				Line  (scrx4 + OffSetnLj + horizOffset1, scry, scrx5, scry);
				levelStr := concat(NumStr(n, 1, 0), LStr, NumStr(j, 2, 0), '/2');
				OutTextXY(scrx4 + horizOffset1, scry, levelStr);
				OutTextXY(scrx6 + horizOffset2, scry, NumStr(j + 1, 2, 0));
			End
		Else Begin
				setcolor(whitte);
				For i := 1 To labelindex2 Do Begin
						templabel := labelcheck2.Value(i);
						If abs(scry - templabel) <= labelheight Then Begin
								horizOffset1 := OffSetnL;
								horizOffset2 := OffSetnum;
							End;{if}
					End;{for i}
				labelindex2 := labelindex2 + 1;
				labelcheck2.put(labelindex2, scry);
				Line  (scrx1 + horizOffset1 + OffSetnL, scry, scrx2, scry);
				SetColor(DarkGray);
				SetLineStyle(dottedLn, 0, NormWidth);
				Line  (scrx3, scry, scrx4, scry);
				SetColor(whitte);
				SetLineStyle(solidLn, 0, NormWidth);
				levelStr := concat(NumStr(n, 1, 0), LStr);
				OutTextXY(scrx1 + horizOffset1, scry, levelStr);
				OutTextXY(scrx3 + horizOffset2 - 2, scry, NumStr(2 * (2 * L + 1), 2, 0));
			End;
      ShowMouse;
		FreeDVector(Vplot);
		FreeDVector(uplot);
	End;{DrawWaveFnct}

	Procedure FindEnergyLevels (var potential: ShellModelType;
								       Var r, initV: DVector; NPts : integer;
                               dr : real);

		Const
			maxn = 4;{maximum number of levels with the same L value}
                        sstring : String[1] = 's';
		Var
			L, i, j, k, m, iso, iso_max, index, nodes, kbeg, ibeg, iend, imatch, ifirst, c,
         maxnodes, iterationUp, iterationDown: integer;

         sum1,sum2,start1,start2 : integer;
         slstring : String[1];
         degeneracy : Array[1..2,1..72] OF Integer;

			labelindex1, labelindex2, NumOfStates1, NumOfStates2, Scount: integer;
			u1max, norm, h: real;
			E, Efirst, Eold, Einterval, Eupper, Elower, dE, dEold, temp: real;{, eigenE}
			du1drmatchLeft, du1drmatchRight, u1norm: real;
			tolf, f, fold, fstart: real;
			secant, start, foundit, leavebool, esckey, NoMoreE, cycle_iso, repeat1: boolean;
			key: byte;
			keybd: char;
			SLevels: Realarray4;
			Veff, NucleonCount1, NucleonCount2: dmatrix;
			A1, u1: DVector;
			labelcheck1: DMatrix;
			labelcheck2: IDvector;

		Procedure WhileLoop;
			Var
				i: integer;
		Begin
			Elower := E - Einterval;
			Eupper := E + Einterval;
			If Eupper > 0.0 Then
				Eupper := 0.0;
			iterationUp := 0;
			iterationDown := 0;
			If (nomoreE) Or (Not foundit) Then
				leavebool := true;
			While (abs(fold) >= tolf) And (Not leavebool) And (Not esckey) Do Begin
{check for events here, e.g. hot key to quit iterations}
					CheckforEvents;
					If ShellModelHotKeys.Pressed(key) Then
						HandleShellModelHotKeys(key, potential, esckey);
					If event.keypressed Then Begin
							beep;
							esckey := (event.ReadKey = chr(27));
						End;
					If (Not escKey) Then Begin
							comment(0.0, 0.95, '-');
							MakeA1DVector(E, dr, potential, NPts, iso, Veff, A1, imatch);
{**********************************}
							StartLeft(L, ifirst, u1);{j,}
							ibeg := ifirst;
							iend := imatch + 2;
							If iend > NPts Then
								iend := NPts;
							comment(0.0, 0.95, '\');
							Numerov(ibeg, iend, dr, A1, u1);{forward integration}
							u1norm := u1.value(imatch);
							du1drmatchleft := NumerovDeriv(imatch, dr, A1, u1);{derivative of u1 at imatch}
{**********************************}
							StartRight(E, potential, NPts, j, iso, r, Veff, u1);
							ibeg := NPts;
							iend := imatch - 2;
							If iend < ifirst Then
								iend := ifirst;
							comment(0.0, 0.95, '|');
							Numerov(ibeg, iend, dr, A1, u1);{backward integration}
							norm := u1norm / u1.value(imatch);
							comment(0.0, 0.95, '/');
							For i := ibeg Downto iend Do Begin
									u1.Put(i, u1.Value(i) * abs(norm));
								End;
							If norm < 0 Then Begin
									For i := ibeg Downto iend Do
										u1.Put(i, -u1.Value(i));
								End;
							du1drmatchright := NumerovDeriv(imatch, dr, A1, u1);{derivative of u1 at imatch}

							u1max := abs(u1.value(1));{find the maximum value of u1}
							For i := 2 To NPts Do
								If abs(u1.Value(i)) > u1max Then
									u1max := abs(u1.Value(i));
{*********************************}
							f := (du1drmatchRight - du1drmatchLeft) / u1max;
							If start Then
								fstart := f;
							If f * fstart < 0 Then
								secant := true;
							If f <> fold Then
								If secant Then
									dE := -f * (E - Eold) / (f - fold);{secant search method}
							Eold := E;
							fold := f;
							E := E + dE;
							If E > Eupper Then Begin
									If E > 0.0 Then
										E := 0.0;
									iterationUp := iterationUp + 1;
									If iterationUp > 1 Then Begin
											Eupper := Eupper + Einterval;
                                 iterationUp := 0;
											If Eupper > 0.0 Then Begin
													dE := -dE;
													Eupper := 0.0;
												End;
										End
									Else
										dE := -dE;
								End;
							If E < Elower Then Begin
									If E < Efirst Then
										E := Efirst;
									iterationDown := iterationDown + 1;
									If iterationDown > 1 Then Begin
											Elower := Elower - Einterval;
                                 iterationDown := 0;
											If Elower < Efirst Then Begin
													dE := -dE;
													Elower := Efirst;
												End;
										End
									Else
										dE := -dE;
								End;
							start := false;
						End; {if not escKey}
				End;{while}
			CountNodes(ibeg, iend, u1, nodes, index);
{			writeln('nodes, imatch, index', nodes, imatch, index);}
		End; {procedure}

		Procedure block1;
		Begin
         Inc(icount);
			If (L = 0) And (potential.potl <> SW) Then Begin
					SLevels[j] := E;
					Scount := j;
					Draw_SWaveFnct(Eold, u1max, j, NPts, repeat1, r, initV, Veff, u1, labelcheck2, SLevels, labelindex2, Scount);{ iso, }
				End
			Else If Potential.potl <> SW Then
				DrawWaveFnct(Eold, u1max, j, L, NPts, iso, r, initV, Veff, u1, potential,
            labelcheck1, labelcheck2, labelindex1, labelindex2);
			If potential.potl = SW Then
				Draw_SqrWellWaveFnct(Eold, u1max, L, j, NPts, r, initV, Veff, u1, labelcheck2, labelindex2);

          OutData.iso[icount] := iso;
          OutData.energy[icount] := Eold;
          IF iso =1 THEN BEGIN
           OutData.level_str[icount] := concat(numstr(j,1,0),findlstr(L),'    ');
           OutData.degeneracy[icount] := 4*L+2;
          END
          ELSE
          IF iso = 2 THEN BEGIN
           OutData.level_str[icount] := concat(numstr(j,1,0),findlstr(L),numstr(2*L+1,2,0),'/2');
           OutData.degeneracy[icount] := 2*L+2;
          END
          ELSE BEGIN
           OutData.level_str[icount] := concat(numstr(j,1,0),findlstr(L),numstr(2*L-1,2,0),'/2');
           OutData.degeneracy[icount] := 2*L;
          END;

         If (iso > 1) And (potential.potl <> SW) Then Begin
		NumOfStates1 := NumOfStates1 + 1;
		NucleonCount1.put(1, NumOfStates1, Eold);
		If iso = 2 Then
		   NucleonCount1.put(2, NumOfStates1, 2.0 * L + 2)
		Else
		    NucleonCount1.put(2, NumOfStates1, 2.0 * L);
		End
		Else Begin
	            NumOfStates2 := NumOfStates2 + 1;
                    NucleonCount2.put(1, NumOfStates2, Eold);
        	    NucleonCount2.put(2, NumOfStates2, 2.0 * (2.0 * L + 1));
		End;
                dE := 0.01;{abs(Eold / 50)}
			E := Eold + dE;
		End;{block1}

		Procedure Block2;
		Begin
			j := 1;
			While (j <= maxn) And (maxnodes >= j) Do {And foundit }
				Begin
					secant := false;
					start := true;
					fold := 1e10;
					ApproxELevel(j, L, NPts, iso, potential, r, initV,
                            Veff, E, dE, Einterval, dr, foundit, esckey);
					If Not nomoreE Then
						WhileLoop;
					If Not escKey Then
						block1;
					inc(j);
				End;{while j, next s-o state}
		End; {block2}

	Begin
		NumOfStates1 := 0;
		NumOfStates2 := 0;
		If potential.potl = SW Then Begin
				tolf := 0.005;
				iso_max := 1;
			End
		Else Begin
				tolf := 0.00012;
				iso_max := 3;
			End;
{    eigenE := -1;minimum value for E, where E is energy/depth}
		GetSMDVectors(Veff, NucleonCount1, NucleonCount2, A1, u1,
                    labelcheck1, labelcheck2, NPts);
		labelindex2 := 0;
      leavebool := false;
		esckey := false;
		repeat1 := false;
		keybd := ' ';
		L := 0;
		While (L <= 6) And (Not EscKey) Do   {loop over angular momentum values 0 to 6}
			Begin
				cycle_iso := false;
				NoMoreE := false;{%%%%%%%%%%%%%}

				iso := 1;
				While (iso <= iso_max) And (Not escKey) And (Not NoMoreE) Do {loop over w/o s-o, w/ s-o high j, and w/ s-o low j}
					Begin
						If (L = 0) And (iso > 1) Then
							cycle_iso := true;
						CalcVeff(potential, initV, r, L, NPts, Veff);
						Getfirstvalues(L, iso, Veff, ifirst, Efirst);
						E := 0.99 * Efirst;
						If E >= 0.0 Then{cycle iso}
							cycle_iso := true;
						If Not cycle_iso Then Begin
								FindMaxNodes(L, npts, iso, potential, r, initV,
                                     Veff, dr, maxnodes);
								NoMoreE := ((E >= 0.0) Or (maxnodes = 0));
								If (Not NoMoreE) Then
									block2;
							End;{if not cycle_iso}
						If Not EscKey Then Begin
								If (L = 0) And (iso = 1) And (potential.potl <> SW) Then Begin
										repeat1 := true;
										Draw_SWaveFnct(E, u1max, j, NPts, repeat1, r, initV, Veff, u1, labelcheck2, SLevels, labelindex2, Scount);{ iso}
										For i := 1 To Scount Do Begin
												labelcheck1.Put(1, i, labelcheck2.Value(i));
												labelcheck1.Put(2, i, 0);
												nucleonCount1.Put(1, i, NucleonCount2.value(1, i));
												nucleonCount1.Put(2, i, NucleonCount2.value(2, i));
											End;{for i=1 to Scount}
										labelindex1 := Scount;
										NumOfStates1 := Scount;
									End{if (L=0) and (iso=1) and (potential.potl<>SW)}
							End;{if not esckey}
						inc(iso);
					End;{for iso, next L level}

				inc(L);
			End;{for L}
		comment(0.0, 0.95, ' sums');
		If potential.potl <> SW Then
			NucleonCountLabel(potential, NumOfStates1, 2, NucleonCount1);
		NucleonCountLabel(potential, NumOfStates2, 1, NucleonCount2);
		comment(0.0, 0.95, '     ');

      writeln(output);
      writeln(output,'------------------------------------------------------');
      writeln(output);
      writeln(output,' Spherical Shell Model');
      IF (potential.potl =   SW) Then
      writeln(output,' Nuclear Potential : Square Well')
      ELSE
      IF (potential.potl = HO) THEN
      writeln(output,' Nuclear Potential : Harmonic Oscillator')
      ELSE
      writeln(output,' Nuclear Potential : Woods-Saxon');

      writeln(output,'  Well Depth : ',potential.depth:7:2,' Mev');
      If (potential.potl = WS) THEN
         writeln('  Diffusioness (d) : ',potential.d:7:3,' fm');
      writeln(output,' Number of Neutrons : ',potential.neutrons:3);
      writeln(output,' Number of Protons  : ',potential.protons:3);
      writeln(output,' Spin-Orbit Strength (c) : ',potential.lambda:4:2);

      SortDataarray(icount,OutData);

      FOR start1 := 1 TO 2 DO
         FOR iicount := 1 TO 72 DO
            degeneracy[start1,iicount] := 0;

      FOR iicount := 1 TO icount DO BEGIN
       slstring := Copy(OutData.level_str[iicount],2,1);
       IF (slstring = sstring) THEN BEGIN
          degeneracy[1,iicount] := OutData.degeneracy[iicount];
          degeneracy[2,iicount] := OutData.degeneracy[iicount];
       END
       ELSE
       IF (OutData.iso[iicount] = 1) THEN
          degeneracy[1,iicount] := OutData.degeneracy[iicount]
       ELSE
          degeneracy[2,iicount] := OutData.degeneracy[iicount];
      END;

      writeln(output);
      writeln(output);
      writeln(output,' Energy    W/O S-O Coup.        W/ S-O Coup.');
      writeln(output);
      For iicount := 1 TO icount DO BEGIN
         slstring := Copy(OutData.level_str[iicount],2,1);
         IF (slstring=sstring) THEN BEGIN
           sum1 := 0;
           sum2 := 0;
           FOR start1 := icount DownTo iicount DO BEGIN
             sum1 := sum1 + degeneracy[1,start1];
             sum2 := sum2 + degeneracy[2,start1];
           END;
           IF (potential.potl <> SW) THEN
              writeln(Output,' ',OutData.energy[iicount]:6:3,'   ',
               Copy(OutData.level_str[iicount],1,2),'_____',
               OutData.degeneracy[iicount]:2,'   ',sum1:3,
               '      ',Concat(Copy(OutData.level_str[iicount],1,2),' 1/2'),
               '_____',OutData.degeneracy[iicount]:2,'   ',sum2:3)
           ELSE
              writeln(Output,' ',OutData.energy[iicount]:6:3,'   ',
               Copy(OutData.level_str[iicount],1,2),'_____',
               OutData.degeneracy[iicount]:2,'   ',sum1:3);
         END
         ELSE
         IF (OutData.iso[iicount] = 1) THEN BEGIN
            sum1 := 0;
              FOR start1 := icount DownTo iicount DO BEGIN
                sum1 := sum1 + degeneracy[1,start1];
              END;
            writeln(Output,' ',OutData.energy[iicount]:6:3,'   ',
             Copy(OutData.level_str[iicount],1,2),'_____',
             OutData.degeneracy[iicount]:2,'   ',sum1:3);
         END
         ELSE
         BEGIN
            sum2 := 0;
              FOR start1 := icount DownTo iicount DO BEGIN
                sum2 := sum2 + degeneracy[2,start1];
              END;
            writeln(Output,' ',OutData.energy[iicount]:6:3,
             '                        ',
             OutData.level_str[iicount],'_____',
             OutData.degeneracy[iicount]:2,'   ',sum2:3);
         END;
     END;

		FreeSMDVectors(Veff, NucleonCount1, NucleonCount2, A1, u1, labelcheck1, labelcheck2);
	End;{FindEnergyLevels}

   Procedure PrintTitle (var potential: ShellModelType);
	Begin
		OpenViewPort(1);
		SelectScale(6);
		setcolor(whitte);
		SetTextJustify(0, 0);
		OutTextXY(MapX(1.4), MapY(3.0), 'Spherical Shell Model Potentials');
		OutTextXY(MapX(1.3), MapY(1.2), potential.potstr);
		SelectScale(1);
		OpenViewPort(2);
		OpenViewPort(3);
	End;{PrintTitle}

   procedure TShellPotential.Init;
   var
      RMax : real;
   begin
	   ro := 1.25;{fm}
	   mnc2 := 939.573;
	   hbarc := 197.329;{Mev.fm}
	   Lconst := 2 * mnc2 / sqr(hbarc);
	   dr   := 0.03;
	   Rmax := 15;
	   NPts := 2 * Round(Rmax / (2.0 * dr)) + 1;
		InitialShellModelParam(potential);
   end;

   procedure TShellPotential.SetUp;
   var
      text : Str20;
   begin
		if PotentialInput(potential) then
      begin
         CleanMemory;
         mode := sPot;
   	   InitDVector(r, NPts);
		   InitDVector(initV, NPts);
		   DefineViewPort(1, 0.25, 0.75, 0.88, 0.95);{1screen title}
		   DefineViewPort(2, 0.07, 0.40, 0.11, 0.88);{2 Shell Model potentials}
		   DefineViewPort(3, 0.41, 0.99, 0.11, 0.88);{3 Shell Model energy levels}
		   DefineViewport(5, 0.25, 0.75, 0.06, 0.13);{4 port title - shell model}
		   DefineScale(1, 0, 15, -1.2, 0.05);{for E and initV in Shell Model}
         EraseMenu;
         CloseViewPort(10);
		   GetLabelPositions(scrx1, scrx2, scrx3, scrx4, scrx5, scrx6,
                           OffSetnL, OffSetnLj, OffSetnum, OffSet2L, OffSetj);
		   OpenViewPort(3);
		   OpenViewPort(2);
		   SelectScale(1);
		   OpenViewPort(1);
		   PrintTitle(potential);
		   Case potential.potl Of
			   SW: CalcSqrWell(dr, potential, r, initV);
			   HO: CalcHarOsc(dr, mnc2, potential, r, initV);
			   WS: CalcWS(dr, potential, r, initV);
		   End;{case potl}
		   If potential.drawbool Then
			   text := 'F6-No Draw'
		   Else
			   text := 'F6-Draw';
		   SetUpShellModelHotKeys(text);
                   icount := 0;
		   FindEnergyLevels(potential, r, initV, NPts, dr);
         MainMenu.Display;
		   MainHotKeys.Display;
		   FreeDVector(r);
		   FreeDVector(initV);
      end;
   end;


{******************end Shell Model Potential routines*************}

{****************** Shell Model Populate routines***************}

	function MyInput (Var SMNucData: SMNucleiType;
									Var noerror: boolean;
									Var messageIsotopeData: messageArray) : boolean;
		Var
			MyScreen: TInputscreen;
			i, x1, x2, y1, y2, temp: integer;
			oddA, ok, help, cancel, errorIsotopeData: boolean;
			states: Array[1..4] Of boolean;
			tempreal: real;

	Begin
		With MyScreen Do Begin
				init;
				oddA := false;
				noerror := true;
				DefineInputPort(0.25, 0.75, 0.25, 0.75);
				LoadLine(' POPULATE LEVELS for the SINGLE PARTICLE ');
				LoadLine('               SHELL MODEL       ');
				LoadLine('');
				LoadLine('     Enter an odd A=(N+Z) Nucleus    ');
				LoadLine(' Number of neutrons, N = {    }');
				LoadLine(' Number of protons,  Z = {    }');
				LoadLine('');
				LoadLine('    [  Ok  ]   [ Help ]   [Cancel]');
				With SMNucData Do Begin
						tempreal := neut * 1.0;
						SetNumber(1, tempreal);
						tempreal := prot * 1.0;
						SetNumber(2, tempreal);
						SetHelpFile('ShellMod.hlp ', 'HelpScreen7');
{Repeat {until no errorIsotopeData}
						Repeat {until odd A}
							AcceptScreen;
							neut := trunc(GetNumber(1));
							prot := trunc(GetNumber(2));
							A := neut + prot;
							oddA := (2 * round(A / 2) <> A);
							If Not oddA Then
								announce('An Odd A nucleus must be entered.');
						Until oddA Or canceled;
						state := gs;
						GetIsotopeData(SMNucData, errorIsotopeData, messageIsotopeData);
						noerror := not errorIsotopeData;
					End;{with SMNucData}
            MyInput := Not Canceled;
				done;
			End;{with MyScreen}
	End;{MyInput}

	Function nuclide (A, prot: integer;
									element: elementData): Str20;
		Var
			index: integer;
			nucleons: Str20;
	Begin
		If A < 10 Then
			index := 1
		Else
			index := 2;
		With element Do Begin
				nucleons := NumStr(A, index, 0);
				nuclide := concat(nucleons, elementStr);
			End;{with element}
	End;{nuclide}

	Procedure DrawLevels (var SMNucData: SMNucleiType);
		Const
			x1 = 0.0;
			x2 = 5.0;
			large = 20;
		Var
			i, Plevely, Nlevely, MNy: integer;
			screenx1, screenx2, scry, scry1: integer;
			y1n, y2n, y1p, y2p: real;
	Begin
      HideMouse;
		With SMNucData Do Begin
				SelectViewport(5);
				y1p := subPlevels.energy[1] - 0.15;
				y2p := subPlevels.energy[lengthp] + 0.2;
				DefineScale(2, x1, x2, y1p, y2p);
				SelectScale(2);
				SetColor(lightgreen);
				screenx1 := MapX(x1);
				screenx2 := MapX(2.4);
				scry1 := large;
				OutTextXY(MapX(0.8), MapY(y1p), 'Protons');
				For i := 1 To lengthp Do Begin
						scry := MapY(subPlevels.energy[i]);
						Line  (screenx1, scry, screenx2, scry);
						OutTextXY(screenx1 + 2, scry, subPlevels.levelLabel[i]);
						scry1 := scry;
					End;
				y1n := subNlevels.energy[1] - 0.15;
				y2n := subNlevels.energy[lengthn] + 0.2;
				DefineScale(3, x1, x2, y1n, y2n);
				SelectScale(3);
				OutTextXY(MapX(3.3), MapY(y1n), 'Neutrons');
				screenx1 := MapX(2.6);
				screenx2 := MapX(x2);
				scry1 := large;
				For i := 1 To lengthn Do Begin
						scry := MapY(subNlevels.energy[i]);
						Line  (screenx1, scry, screenx2, scry);
						OutTextXY(screenx1 + 2, scry, subNlevels.levelLabel[i]);
						scry1 := scry;
					End;
				SetColor(lightcyan);
				selectscale(2);
				screenx1 := MapX(x1);
				screenx2 := MapX(2.4);
				For i := 1 To lengthMNp Do Begin
						scry := MapY(subMNlevels[1, i]);
						OutTextXY(screenx1 + 2, scry, subMN[1, i]);
					End;
				selectscale(3);
				screenx1 := MapX(2.6);
				screenx2 := MapX(x2);
				For i := 1 To lengthMNn Do Begin
						scry := MapY(subMNlevels[2, i]);
						OutTextXY(screenx1 + 2, scry, subMN[2, i]);
					End;
			End;{with SMNucData}
         ShowMouse;
	End;{DrawLevels}

	Procedure Drawprotons (var SMNucData: SMNucleiType;
									Var spinParitylevelp: integer);
		Const
			pr = 4;
			xint = 0.74;
			solid = 1;
			dot = 11;
		Var
			i, j, rad, netprotOld, netprot, scrx, scry, numb, numfill, numUnfill, temp, offset, excited: integer;
			x, scal: real;
			neg: boolean;
	Begin
		SelectViewport(5);
		setColor(lightblue);
		With SMNucData Do Begin
				With subPlevels Do Begin
						neg := false;
						netprotOld := prot - sumdegen[1] + degeneracy[1];
{******Ground state*********}
						For i := 1 To lengthp Do Begin
								numb := degeneracy[i];
								If Not neg Then Begin
										netprot := netprotOld - numb;
										If netprot > 0 Then Begin
												numfill := numb;
												numUnfill := 0;
											End
										Else Begin
												neg := true;
												spinParitylevelp := i;
												numfill := netprotOld;
												numUnfill := abs(netprot);
											End;
										netprotOld := netprot;
									End
								Else Begin
										numfill := 0;
										numUnfill := numb;
									End;
								scal := abs(xint - 2.4) / numb;
								x := xint - scal;
								selectscale(2);
								For j := 1 To numfill Do Begin
										x := x + scal;
                              DrawCircle(x, energy[i], pr, lightred);
									End;
								For j := 1 To numUnfill Do Begin
										x := x + scal;
                              DrawCircle(x, energy[i], pr, white);
									End;
							End;{for i :=1 to lengthp}
{************Excited States************}
						temp := 2 * round(prot / 2);
						If (state <> gs) And (temp <> prot) Then Begin
								If state = first Then
									offset := 1
								Else
									offset := 2;
								excited := spinParitylevelp + offset;
								x := xint;
                        DrawCircle(x, energy[excited-offset], pr, white);
                        DrawCircle(x, energy[excited], pr, lightRed);
								spinParitylevelp := excited;
							End;{if state<>gs}
					End;{with subPlevels}
			End;{with SMNucData}
	End;{DrawProtons}

	Procedure Drawneutrons (var SMNucData: SMNucleiType;
									Var spinParityleveln: integer);
		Const
			nr = 4;
			xint = 3.34;
			solid = 1;
			dot = 11;
		Var
			i, j, rad, netneutOld, netneut, scrx, scry, numb, numfill, numUnfill, temp, excited, offset: integer;
			x, scal: real;
			neg: boolean;
	Begin
		SelectViewport(5);
		setColor(lightblue);
		With SMNucData Do Begin
				With subNlevels Do Begin
						neg := false;
						netneutOld := neut - sumdegen[1] + degeneracy[1];
{*******Ground state******}
						For i := 1 To lengthn Do Begin
								numb := degeneracy[i];
								If Not neg Then Begin
										netneut := netneutOld - numb;
										If netneut > 0 Then Begin
												numfill := numb;
												numUnfill := 0;
											End
										Else Begin
												neg := true;
												spinParityleveln := i;
												numfill := netneutOld;
												numUnfill := abs(netneut);
											End;
										netneutOld := netneut;
									End
								Else Begin
										numfill := 0;
										numUnfill := numb;
									End;
								scal := abs(xint - 5.0) / numb;
								x := xint - scal;
								selectscale(3);
								For j := 1 To numfill Do Begin
										x := x + scal;
                              DrawCircle( x, energy[i], nr, lightBlue);
									End;
								For j := 1 To numUnfill Do Begin
										x := x + scal;
                              DrawCircle( x, energy[i], nr, white);
									End;
							End;
{************Excited States************}
						temp := 2 * round(neut / 2);
						If (state <> gs) And (temp <> neut) Then Begin
								If state = first Then
									offset := 1
								Else
									offset := 2;
								excited := spinParityleveln + offset;
								x := xint;
                        DrawCircle( x, energy[Excited-offset], nr, white);
                        DrawCircle( x, energy[Excited], nr, lightBlue);
								spinParityleveln := excited;
							End;{if state<>gs}
					End;{with subNlevels}
			End;{with SMNucData}
	End;{DrawNeutrons}

	Procedure InitializeShowResult (noerror: boolean;
									var messageIsotopeData: messageArray;
									Var SMNucData: SMNucleiType);
		Var
			index1, index2: integer;
			minEp, maxEp, minEn, maxEn: real;
			text: Str80;
			subMNlevels: level2array;
			subMN: level2Str;
	Begin
		OpenViewPort(6);
		SetTextJustify(1, 0);
		SelectScale(6);
		OutTextXY(MapX(3.0), MapY(1.7), 'Single Particle States in Nuclei');
		SetTextJustify(0, 0);
		OpenViewPort(2);
		SelectScale(7);
		SetColor(lightgreen);
		With SMNucData Do
		   With element Do Begin
            OutTextXY(MapX(1.4), MapY(8.0), '   GROUND STATE for');
				text := concat(NumStr(prot, 3, 0), ' Protons');
				OutTextXY(MapX(3.5), MapY(7.0), text);
				text := concat(NumStr(neut, 3, 0), ' Neutrons');
				OutTextXY(MapX(3.5), MapY(6.5), text);
				OutTextXY(MapX(1.9), MapY(2.5), concat('I',smpi,':'));
            OutTextXY(MapX(1.2), MapY(2.0), 'Shell Model  =');
		         If noerror Then Begin
                  OutTextXY(MapX(1.2), MapY(7.0), nuclide(A, prot, element));
      		      OutTextXY(MapX(1.15), MapY(5.5), 'Abundance/half-life: ');
						OutTextXY(MapX(1.8), MapY(5.0), Stability);
						OutTextXY(MapX(1.2), MapY(1.5), 'Experimental =');
					  End
					Else Begin
					   OutTextXY(MapX(0.15), MapY(5.5), 'The requested isotope is not');
						OutTextXY(MapX(0.15), MapY(5.0), '   in the look-up table.');
						OutTextXY(MapX(0.15), MapY(4.0), concat(messageIsotopeData[1], messageIsotopeData[2]));
					  End;
				OpenViewport(5);
				SelectScale(7);
				GetNeutronLevels(minEn, maxEn, SMNucData);
            GetProtonLevels(minEp, maxEp, SMNucData);
            GetMNLevels(minEp, maxEp, minEn, maxEn, SMNucData);
				DrawLevels(SMNucData);
				SetAspectRatio(1, 1);
			End;{with SMNucData}
	End;{InitializeShowResult}

	Procedure FindSpinParity (noerror: boolean;
									var SMNucData: SMNucleiType);
		Var
			i, temp, spinParitylevelp, spinParityleveln: integer;
			SpinParitySM: Str20;
		Var
			scrx, scry, scrx1, scry1: integer;
	Begin
		With SMNucData Do Begin
				SelectViewPort(5);
				Drawprotons(SMNucData, spinParitylevelp);
				Drawneutrons(SMNucData, spinParityleveln);
				SelectViewPort(2);
				SelectScale(7);
				scrx := MapX(5.6);
				scry := MapY(1.5);
				scry1 := MapY(2.0);
				EraseXY(scrx, scry, 10, DarkGray);
				EraseXY(scrx, scry1, 10, DarkGray);
				SetColor(whitte);
				If noerror Then Begin
						With element Do Begin
								Case state Of
									gs:
										OutTextXY(scrx, scry, SpinParity[1]);
									first:
										OutTextXY(scrx, scry, SpinParity[2]);
									second:
										OutTextXY(scrx, scry, SpinParity[3]);
								End;{case state}
							End;{with element}
					End;{if}
				temp := 2 * round(neut / 2);
				If neut <> temp Then
					SpinParitySM := subNlevels.spinParity[spinParityleveln]
				Else
					SpinParitySM := subPlevels.spinParity[spinParitylevelp];
				OutTextXY(scrx, scry1, SpinParitySM);
			End;{with SMNucData}
	End;{FindSpinParity}


	Procedure HandleSMNucleiButtons (key: byte;
									Var noerror: boolean;
									Var SMNucData: SMNucleiType);
		Var
			messageIsotopeData: messageArray;
         text : string;
	Begin
		With SMNucData Do Begin
		 Case key Of
		  1: {gs}
			  Begin
				  state := gs;
              text  := '   GROUND STATE for';
			  End;
		  2: {1st excited}
			  Begin
				  state := first;
              text  := 'FIRST EXCITED STATE for';
			  End;
		  3: {2nd excited}
			  Begin
				  state := second;
              text  := 'SECOND EXCITED STATE for';
		     End;
		 End;{case key}
		 SelectViewPort(2);
		 EraseXY(MapX(1.4), MapY(8.0), 24, DarkGray);
		 SetColor(lightgreen);
		 OutTextXY(MapX(1.4), MapY(8.0), text);
		 FindSpinParity(noerror, SMNucData);
	 End;{with SMNucData}
	End;{HandleSMNucleiButtons}


   procedure TShellNuclei.Init;
   begin
      active := false;
		With SMNucData Do Begin
	      neut := 21;
			prot := 20;
      end;
   end;

   procedure TShellNuclei.Free;
   begin
      active := false;
      CloseViewPort(7);
      buttons.done;
   end;

   procedure TShellNuclei.SetUp;
   var
     messageIsotopeData: messageArray;
   begin
		if MyInput(SMNucData, noerror, messageIsotopeData) then
      begin
         mode := sNuc;
		   DefineViewPort(2, 0.05, 0.42, 0.25, 0.88);{2 upper left port in screen 2}
		   DefineViewport(5, 0.44, 0.99, 0.05, 0.88);{4 Energy levels in screen 2}
		   DefineViewPort(6, 0.28, 0.74, 0.89, 0.95);
		   DefineViewPort(7, 0.05, 0.43, 0.07, 0.24);
         CloseViewPort(10);
         SelectMuppetPort;
			setColor(white);
         HideMouse;
         SetUpButtons(Buttons);
			InitializeShowResult(noerror, messageIsotopeData, SMNucData);
			FindSpinParity(noerror, SMNucData);
         ShowMouse;
         MainHotKeys.Display;
      end;
   end;

   procedure TShellNuclei.Control;
   begin
      If buttons.changed Then
			HandleSMNucleiButtons(buttons.number, noerror, SMNucData);
   end;

{****************end Shell Model Populate rountines************}

{*****************Deformed Shell Model routines**************}
	Procedure SetUpNilssonScreen (var IsoParam: IsoParamType);
		Var
			scrx, scry: integer;
			text: Str80;
	Begin
		OpenViewPort(1);
		SetColor(whitte);
		SetTextJustify(0, 0);
		SelectScale(6);
		text := 'Deformed Shell Model Levels';
		OutTextXY(MapX(1.3), MapY(2.95), text);
      OutTextXY(MapX(2.3), MapY(1.55), IsoParam.title);
		SetUpNilssonHotKeys;
	End;{SetUpNilssonScreen}

	Procedure SetUpDefNucleiScreen (var IsoParam: IsoParamType);
		Var
			scrx, scry: integer;
			text: Str80;
	Begin
		OpenViewPort(1);
		SetColor(whitte);
		SetTextJustify(0, 0);
		SelectScale(6);
		text := 'Populate Deformed Nuclei Levels';
		OutTextXY(MapX(1.1), MapY(2.95), text);
		OutTextXY(MapX(1.9), MapY(1.55),IsoParam.title);
		OpenViewport(5);
		SelectScale(7);
		OutTextXY(MapX(1.9), MapY(6.3), 'Nuclear Spin and Parity');
		OutTextXY(MapX(0.25), MapY(3.0), concat('[N nz ',lambda,']',omega,smpi));
		OutTextXY(MapX(3.5), MapY(3.0), 'model');
		OutTextXY(MapX(0.25), MapY(0.5), concat('[N nz ',lambda,']',omega,smpi));
		OutTextXY(MapX(3.5), MapY(0.5), 'experiment');
      OpenViewPort(6);
      SelectScale(7);
      SetAspectRatio(1, 1);
      OutTextXY(MapX(2.5), MapY(5.9), 'filled');
      OutTextXY(MapX(2.5), MapY(2.3), 'unfilled');
      SetColor(lightblue);
      DrawCircle( 1, 6.4, 3, lightBlue);
      DrawCircle( 1, 2.9, 3, white);
		OpenViewPort(2);
		OpenViewPort(3);
		SetUpNilssonHotKeys;
	End;{SetUpDefNucleiScreen}

	Procedure AssignLevelParam (LevelGroup: integer;
									Var IsoParam: IsoParamType);
	Begin
		With IsoParam Do Begin
				Case LevelGroup Of
					1: {2 <NorZ< 20}
						Begin
							Nmin := 1;
							Nmax := 3;
							yscalemin := 2.2;
							yscalemax := 4.0;
							A := 35;
							kappa := 0.08;
							mu := 0.0;
							mufactor := 0.0;
							MNlabel[1] := '2';
							MNcoord[1] := 2.3;
							MNlabel[2] := '8';
							MNcoord[2] := 3.0;
							MNlabel[3] := '20';
							MNcoord[3] := 3.85;
							title := '2<[N or Z]<20';
						End;{for case 1}
					2: {50<Z<82}
						Begin
							Nmin := 4;
							Nmax := 5;
							yscalemin := 4.8;
							yscalemax := 5.7;
							A := 165;
							kappa := 0.0637;
							mu := 0.60;
							mufactor := 2.4;
							MNlabel[1] := '50';
							MNcoord[1] := 4.9;
							MNlabel[2] := '82';
							MNcoord[2] := 5.6;
							MNlabel[3] := '';
							MNcoord[3] := 0.0;
							title := '50<Z<82';
						End;{for case 2}
					3: {82<N<126}
						Begin
							Nmin := 5;
							Nmax := 6;
							yscalemin := 5.7;
							yscalemax := 6.7;
							A := 181;
							kappa := 0.0637;
							mu := 0.42;
							mufactor := 2.4;
							MNlabel[1] := '82';
							MNcoord[1] := 5.85;
							MNlabel[2] := '126';
							MNcoord[2] := 6.65;
							MNlabel[3] := '';
							MNcoord[3] := 0.0;
							title := '82<N<126';
						End;{for case 3}
					4: {Z>82}
						Begin
							Nmin := 5;
							Nmax := 6;
							yscalemin := 5.5;
							yscalemax := 6.7;
							A := 237;
							kappa := 0.05;
							mu := 0.70;
							mufactor := 2.45;
							MNlabel[1] := '82';
							MNcoord[1] := 5.62;
							MNlabel[2] := '126';
							MNcoord[2] := 6.65;
							MNlabel[3] := '';
							MNcoord[3] := 0.0;
							title := 'Z>82';
						End;{for case 4}
					5: {N>126}
						Begin
							Nmin := 6;
							Nmax := 7;
							yscalemin := 6.4;
							yscalemax := 7.07;
							A := 247;
							kappa := 0.06;
							mu := 0.43;
							mufactor := 2.22;
							MNlabel[1] := '126';
							MNcoord[1] := 6.55;
							MNlabel[2] := '';
							MNcoord[2] := 0.0;
							MNlabel[3] := '';
							MNcoord[3] := 0.0;
							title := 'N>126';
						End;{for case 5}
				End;{case}
			End;{with IsoParam}
	End;{AssignLevelParam}


	Procedure AssignIsoParam (Nuclei: NucleiType;
									Var IsoParam: IsoParamType);
	Begin
		With IsoParam Do Begin
				Case Nuclei Of
					Al:  Begin
							Nmin := 2;
							Nmax := 3;
							NucleonsBelow := 8;
							OddNumNucleons := 13;
							singleDelta := 0.30;
							A := 25;
							yscalemin := 2.95;
							yscalemax := 3.97;
							kappa := 0.08;
							mu := 0.00;
							mufactor := 0.0;
							MNlabel[1] := '8';
							MNcoord[1] := 3.0;
							MNlabel[2] := '20';
							MNcoord[2] := 3.85;
							MNlabel[3] := '';
							MNcoord[3] := 0.0;
							title := concat('25 Al  ',delta,' = 0.30');
							With extra Do Begin
									Prot := 13;
									Neut := 12;
									states := gs;{default to gs}
									statesStr[1] := '[202] 5/2+';
									statesStr[2] := '[211] 1/2+';
									statesStr[3] := '[200] 1/2+';
								End;{with extra}
						End;{Al}
					Eu:  Begin
							Nmin := 4;
							Nmax := 5;
							NucleonsBelow := 40;
							OddNumNucleons := 63;
							singleDelta := 0.30;
							A := 153;
							yscalemin := 4.80;
							yscalemax := 5.60;
							kappa := 0.0637;
							mu := 0.60;
							mufactor := 2.4;
							MNlabel[1] := '50';
							MNcoord[1] := 4.9;
							MNlabel[2] := '82';
							MNcoord[2] := 5.59;
							MNlabel[3] := '';
							MNcoord[3] := 0.0;
							title := concat('153 Eu  ',delta,' = 0.30');{Mac}
							With extra Do Begin
									Prot := 63;
									Neut := 90;
									states := gs;{default to gs}
									statesStr[1] := '[413] 5/2+';
									statesStr[2] := '[532] 5/2-';
									statesStr[3] := '[411] 3/2+';
								End;{with extra}
						End;{Eu}
					Dy:  Begin
							Nmin := 5;
							Nmax := 6;
							NucleonsBelow := 70;
							OddNumNucleons := 95;
							singleDelta := 0.31;
							A := 161;
							yscalemin := 5.70;
							yscalemax := 6.25;
							kappa := 0.0637;
							mu := 0.42;
							mufactor := 2.4;
							MNlabel[1] := '82';
							MNcoord[1] := 5.85;
							MNlabel[2] := '';
							MNcoord[2] := 0.0;
							MNlabel[3] := '';
							MNcoord[3] := 0.0;
                     title := concat('161 Dy  ', delta, ' = 0.31');{PC}
							With extra Do Begin
									Prot := 66;
									Neut := 95;
									states := gs;{default to gs}
									statesStr[1] := '[642] 5/2+';
									statesStr[2] := '[523] 5/2-';
									statesStr[3] := '[521] 3/2-';
								End;{with extra}
						End;{Dy}
					Tm:  Begin
							Nmin := 4;
							Nmax := 5;
							NucleonsBelow := 40;
							OddNumNucleons := 69;
							singleDelta := 0.29;
							A := 169;
							yscalemin := 4.80;
							yscalemax := 5.75;
							kappa := 0.0637;
							mu := 0.60;
							mufactor := 2.4;
							MNlabel[1] := '50';
							MNcoord[1] := 4.9;
							MNlabel[2] := '82';
							MNcoord[2] := 5.6;
							MNlabel[3] := '';
							MNcoord[3] := 0.0;
                     title := concat('169 Tm  ', delta, ' = 0.29');{PC}
							With extra Do Begin
									Prot := 69;
									Neut := 100;
									states := gs;{default to gs}
									statesStr[1] := '[411] 1/2+';
									statesStr[2] := '[404] 7/2+';
									statesStr[3] := '[523] 7/2-';
								End;{with extra}
						End;{Tm}
					Hf:  Begin
							Nmin := 5;
							Nmax := 6;
							NucleonsBelow := 70;
							OddNumNucleons := 105;
							singleDelta := 0.27;
							A := 177;
							yscalemin := 5.85;
							yscalemax := 6.50;
							kappa := 0.0637;
							mu := 0.42;
							mufactor := 2.4;
							MNlabel[1] := '82';
							MNcoord[1] := 5.93;
							MNlabel[2] := '';
							MNcoord[2] := 0.0;
							MNlabel[3] := '';
							MNcoord[3] := 0.0;
                     title := concat('177 Hf  ', delta, ' = 0.27');{PC}
							With extra Do Begin
									Prot := 72;
									Neut := 105;
									states := gs;{default to gs}
									statesStr[1] := '[514] 7/2-';
									statesStr[2] := '[624] 9/2+';
									statesStr[3] := '[633] 7/2+';
								End;{with extra}
						End;{Hf}
					Pu:  Begin
							Nmin := 6;
							Nmax := 7;
							NucleonsBelow := 112;
							OddNumNucleons := 145;
							singleDelta := 0.26;
							A := 239;
							yscalemin := 6.40;
							yscalemax := 7.10;
							kappa := 0.06;{35}
							mu := 0.43;{0.325}
							mufactor := 2.22;
							MNlabel[1] := '126';
							MNcoord[1] := 6.65;
							MNlabel[2] := '';
							MNcoord[2] := 0.0;
							MNlabel[3] := '';
							MNcoord[3] := 0.0;
                     title := concat('239 Pu  ', delta, ' = 0.26');{PC}
							With extra Do Begin
									Prot := 94;
									Neut := 195;
									states := gs;{default to gs}
									statesStr[1] := '[631] 1/2+';
									statesStr[2] := '[622] 5/2+';
									statesStr[3] := '[743] 7/2-';
								End;{with extra}
						End;{Pu}
				End;{case Nuclei}
			End;{with IsoParam}
	End;{AssignIsoParam}

{S CalcSeg}

	Procedure BasisStates (N: integer;
									Omega: real;
									Var BasisState: arrayBasis;
									Var BasisStateNum: integer);
		Var
			i, k, count, angmom: integer;
			tempj, templambda, testomega: real;
			EqOmega, LTzero: boolean;
	Begin
		count := 0;
		angmom := N + 2;
		LTzero := false;
		For i := 1 To 7 Do Begin
				angmom := angmom - 2;
				LTzero := (angmom < 0);
				If Not LTzero Then Begin
						EqOmega := false;
						templambda := angmom + 1;
						k := 1;
						While (k <= angmom + 1) And (Not EqOmega) Do Begin
								templambda := templambda - 1;
								testomega := templambda + 0.5000;
								If (testomega = Omega) Then Begin
										count := count + 1;
										BasisState[count].HO_N := N;
										BasisState[count].L := angmom;
										BasisState[count].lambda := templambda;
										BasisState[count].sigma := 0.5000;
										EqOmega := true;
									End;{If (testomega = Omega)}
								inc(k);
							End;{for k}
						EqOmega := false;
						templambda := angmom + 1;
						k := 1;
						While (k <= angmom + 1) And (Not EqOmega) Do Begin
								templambda := templambda - 1;
								testomega := templambda - 0.5000;
								If (testomega = Omega) Then Begin
										count := count + 1;
										BasisState[count].HO_N := N;
										BasisState[count].L := angmom;
										BasisState[count].lambda := templambda;
										BasisState[count].sigma := -0.5000;
										EqOmega := true;
									End;{If (testomega = Omega)}
								inc(k);
							End;{for k}
					End;{if not LTzero}
			End;{for i}
		BasisStateNum := count;
	End;{BaseStates}

	Procedure CalcCG (L, Lprime: integer;
									lambda, lambdaPrime: real;
									Var resultCG: real);
		Var
			differL: integer;
			numerator, denominator, CG1, CG2: real;
	Begin
		differL := L - Lprime;
		If (abs(differL) > 2) Or (L = 0) Or (lambda <> lambdaPrime) Then Begin
				CG1 := 0.0;
				CG2 := 0.0;
			End
		Else If Lprime = L + 2 Then Begin
				numerator := 3 * (L - lambdaPrime + 2) * (L - lambdaPrime + 1) * (L + lambdaPrime + 2) * (L + lambdaPrime + 1);
				denominator := (2 * L + 1) * (2 * L + 2) * (2 * L + 3) * (L + 2);
				CG1 := sqrt(numerator / denominator);
				numerator := 3 * (L + 2) * (L + 1) * (L + 2) * (L + 1);
				CG2 := sqrt(numerator / denominator);
			End
		Else If Lprime = L + 1 Then Begin
				CG1 := 1.0; {this is arbitrary because CG2=0 which makes resultCG=0}
				CG2 := 0.0;
			End
		Else If Lprime = L Then Begin
				numerator := 3 * sqr(lambdaPrime) - L * (L + 1);
				denominator := (2 * L - 1) * L * (L + 1) * (2 * L + 3);
				CG1 := numerator / sqrt(denominator);
				numerator := -L * (L + 1);
				CG2 := numerator / sqrt(denominator);
			End
		Else If Lprime = L - 1 Then Begin
				CG1 := 1.0; {this is arbitrary because CG2=0 which makes CGresult=0}
				CG2 := 0;
			End
		Else If Lprime = L - 2 Then Begin
				numerator := 3 * (L - lambdaPrime) * (L - lambdaPrime - 1) * (L + lambdaPrime) * (L + lambdaPrime - 1);
				denominator := (2 * L - 2) * (2 * L - 1) * L * (2 * L + 1);
				CG1 := sqrt(numerator / denominator);
				numerator := 3 * sqr(L) * (L - 1) * (L - 1);
				CG2 := sqrt(numerator / denominator);
			End;
		resultCG := CG1 * CG2;
	End;{CalcCG}

	Function max (i, ii: integer): integer;
	Begin
		If i >= ii Then
			max := i
		Else
			max := ii;
	End;{max}

	Procedure CalcMatrixElements (N, BasisStateNum: integer;
									var BasisState: arrayBasis;
									delta: real;
									var Isoparam: Isoparamtype;
									Var deformedMatrix: dmatrix;
									Var deltafactor: real);
		Var
			i, h, tempL, tempLprime, littlen: integer;
			templambda, templambdaprime, eta, temp: real;
			rsqr, resultCG, Y20, LdotS, etaU, muLsqr, corrLsqr, U: real;
	Begin
		With IsoParam Do Begin
				deltafactor := 1 - (4 / 3) * sqr(delta) - (16 / 27) * pwr(delta, 3);
				eta := (delta / kappa) * pwr(deltafactor, -1 / 6);
				For i := 1 To BasisStateNum Do Begin
						For h := i To BasisStateNum Do Begin
								tempL := BasisState[i].L;
								templambda := BasisState[i].lambda;
								tempLprime := BasisState[h].L;
								templambdaPrime := BasisState[h].lambda;
{calculate r^2}
								If tempLprime = tempL Then
									rsqr := N + 1.5000
								Else If abs(tempLprime - tempL) = 2 Then Begin
										littlen := round((N - max(tempL, tempLprime)) / 2);
										rsqr := 2 * sqrt((littlen + 1) * (littlen + tempL + 0.5000));
									End
								Else
									rsqr := 0.0;
{calculate Y20}
								CalcCG(tempL, tempLprime, templambda, templambdaPrime, resultCG);
								Y20 := sqrt(5 / (4 * pi)) * sqrt((2 * tempL + 1) / (2 * tempLprime + 1)) * resultCG;
{calculate eta*U}
								U := (-4 / 3) * sqrt(pi / 5) * rsqr * Y20;
								etaU := eta * U;
{calculate L.S}
								If tempL <> tempLprime Then
									LdotS := 0.0
								Else If templambda = templambdaPrime Then
									If BasisState[i].sigma = 0.5000 Then
										LdotS := templambda / 2
									Else
										LdotS := -templambda / 2
								Else Begin
										If templambdaPrime = templambda + 1 Then
											LdotS := sqrt((tempL - templambda) * (tempL + templambda + 1)) / 2
										Else If templambdaPrime = templambda - 1 Then
											LdotS := sqrt((tempL + templambda) * (tempL - templambda + 1)) / 2;
									End;
{calculate muL^2}
								If (tempLprime <> tempL) Or (templambdaPrime <> templambda) Then
									muLsqr := 0.0
								Else Begin
										If (N = Nmax) Then
											muLsqr := mufactor * mu * (tempL * (tempL + 1) - (N * (N + 3) / 2))
										Else
											muLsqr := mu * tempL * (tempL + 1);
									End;
{calculate matrix element}
								deformedMatrix.put(i, h, etaU - 2 * LdotS - muLsqr);
								If h <> i Then
									deformedMatrix.put(h, i, deformedMatrix.value(i, h));
							End;{for h}
					End;{for i}
			End;{with IsoParam}
	End;{CalcMatrixElements}

	Procedure SortEnergy (numb: integer;
									Var energy: dvector);
		Var
			i, j: integer;
			temp: real;
			insert, bool1: boolean;
	Begin
		insert := false;
		For j := 2 To numb Do Begin
				temp := energy.value(j);
				i := j - 1;    {look for place to insert it}
            bool1 := false;
				While (i >= 1) And (energy.value(i) > temp) and (not Bool1) Do
               Begin
						energy.put(i + 1, energy.value(i));
						if i=1 then bool1 := true else dec(i);
					End;
				insert := (energy.value(i) <= temp);
				If Not insert Then
					i := 0;
				energy.put(i + 1, temp)		{insert it}
			End;
	End;{SortEnergy}

	Function equal (a, b: real): boolean;
	Begin
		equal := (abs(a - b) < 0.0001)
	End;

	Procedure SortStates (numb: integer;
									var energies: dvector;
									Var statecount: integer;
									Var InitialStates: InitialStatesMatrix);
		Var
			i, h, states, tempcount: integer;
			tempenergy: real;
			insert, bool1: boolean;
	Begin
{count the number of times a particular ¶=0 energy occurs}
		With InitialStates Do Begin
				If statecount = 0 Then Begin
						energy[1] := energies.value(1);
						count[1] := 1;
						statecount := 1;
					End
				Else Begin
						For i := 1 To numb Do Begin
								h := 1;
								While (h < stateCount) And (Not equal(energies.value(i), energy[h])) Do
									inc(h);
								If equal(energies.value(i), energy[h]) Then
									inc(count[h])
								Else Begin
										inc(statecount);
										energy[stateCount] := energies.value(i);
										count[stateCount] := 1;
									End;{if energies}
							End;{for i}

{Sort InitialStates on count}
						insert := false;
						For h := 2 To statecount Do Begin
								bool1 := false;
								tempcount := count[h];
								tempenergy := energy[h];
								i := h - 1;		{look for place to insert it}
								While (i >= 1) And (count[i] > tempcount) And (Not bool1) Do Begin
										count[i + 1] := count[i];
										energy[i + 1] := energy[i];
										If i = 1 Then
											bool1 := true
										Else
											dec(i);
									End;{for i}
								insert := (count[i] <= tempcount);
								If Not insert Then
									i := 0;
								count[i + 1] := tempcount;
								energy[i + 1] := tempenergy;		{insert it}
							End;{for h}
					End;{else}
			End;{with InitialStates}
	End;{SortStates}

	Procedure SortLevelRecord (numb: integer;
									Var LevelRecord: LevelRecordType);
		Var
			i, j: integer;
			temp: real;
			tempStr: Str20;
			insert, bool1: boolean;
	Begin
		With LevelRecord Do Begin
				insert := false;
				For j := 2 To numb Do Begin
						bool1 := false;
						temp := energy[j];
						tempStr := stateStr[j];
						i := j - 1;		{look for place to insert it}
						While (i >= 1) And (energy[i] > temp) And (Not bool1) Do Begin
								energy[i + 1] := energy[i];
								stateStr[i + 1] := stateStr[i];
								If i = 1 Then
									bool1 := true
								Else
									dec(i);
							End;
						insert := (energy[i] <= temp);
						If Not insert Then
							i := 0;
						energy[i + 1] := temp;
						stateStr[i + 1] := tempStr;		{insert it}
					End;{for j}
			End;{with LevelRecord}
	End;{SortLevelRecord}

	Procedure SortLabelCheck (labelIndex: integer;
									Var LabelCheck: labelmatrix);
		Var
			i, j: integer;
			temp, tempOffset: integer;
			insert, bool1: boolean;
	Begin
		insert := false;
		For j := 2 To labelIndex Do Begin
				bool1 := false;
				temp := LabelCheck[1, j];
				tempOffset := LabelCheck[2, j];
				i := j - 1;		{look for place to insert it}
				While (i >= 1) And (LabelCheck[1, i] > temp) And (Not bool1) Do Begin
						LabelCheck[1, i + 1] := LabelCheck[1, i];
						LabelCheck[2, i + 1] := LabelCheck[2, i];
						If i = 1 Then
							bool1 := true
						Else
							dec(i);
					End;{if}
				insert := (LabelCheck[1, i] <= temp);
				If Not insert Then
					i := 0;
				LabelCheck[1, i + 1] := temp;		{insert it}
				LabelCheck[2, i + 1] := tempOffset;
			End;{for j}
	End;{SortLabelCheck}

	Function Odd (aInt: integer): boolean;
		Var
			temp: integer;
	Begin
		temp := 2 * round(aInt / 2);
		If temp = aInt Then
			Odd := false
		Else
			Odd := true;
	End;{Odd}

	Procedure AsymptoticStates (N, BasisStateNum: integer;
									Omega: real;
									Var finalStateStr: Strarray);
		Var
			i, tempnz, temp: integer;
			templambda, tempparity, areal: real;
			text: Str20;
	Begin
		tempnz := -1;
		For i := BasisStateNum Downto 1 Do Begin
				text := '';
				text := concat('[', NumStr(N, 1, 0));
				tempnz := tempnz + 1;
				text := concat(text, NumStr(tempnz, 1, 0));
				If Odd(N - tempnz) Then
					If Odd(round(Omega + 0.5000)) Then Begin
							templambda := Omega + 0.5000;
							If templambda > 9.0 Then
								text := concat(text, NumStr(templambda, 2, 0), ']')
							Else
								text := concat(text, NumStr(templambda, 1, 0), ']');
						End
					Else Begin
							templambda := Omega - 0.5000;
							If templambda > 9.0 Then
								text := concat(text, NumStr(templambda, 2, 0), ']')
							Else
								text := concat(text, NumStr(templambda, 1, 0), ']');
						End
				Else If Odd(round(Omega + 0.5000)) Then Begin
						templambda := Omega - 0.5000;
						If templambda > 9.0 Then
							text := concat(text, NumStr(templambda, 2, 0), ']')
						Else
							text := concat(text, NumStr(templambda, 1, 0), ']');
					End
				Else Begin
						templambda := Omega + 0.5000;
						If templambda > 9.0 Then
							text := concat(text, NumStr(templambda, 2, 0), ']')
						Else
							text := concat(text, NumStr(templambda, 1, 0), ']');
					End;
				areal := 2 * Omega;
				text := concat(text, NumStr(areal, 2, 0), '/2');
				tempparity := pwr(-1, N);
				If tempparity >= 0 Then
					text := concat(text, '+')
				Else
					text := concat(text, '-');
				finalStateStr[i] := text;
			End;
	End;{AsymptoticStates}

	Procedure BuildLevelArray (var eigenvalues: dvector;
									N, index, BasisStateNum: integer;
									delta, deltafactor: real;
									var IsoParam: IsoParamtype;
									Var LevelArray: LevelArrayType;
									Var hbarwdelta: real);
		Var
			i, k: integer;
			term1, term2, matrixelement: real;
	Begin
		hbarwdelta := hbarwo * pwr(deltafactor, -1 / 6);
		For i := 1 To BasisStateNum Do Begin
				term1 := (N + (3 / 2));
				term2 := (IsoParam.kappa * hbarwo / hbarwdelta) * eigenvalues.value(i);
				matrixelement := term1 + term2;
				LevelArray[i, index] := matrixelement;
			End;
	End;{BuildLevelArray}


	Procedure LabelInitialStates (N, Nmax, statecount: integer;
									hbarwdelta: real;
									var IsoParam: IsoParamtype;
									var InitialStates: InitialStatesMatrix;
									Var InitLabel: InitialLabelType);
		Var
			j: real;
			term1, term2: real;
			i, L, Ltemp, index, xscr, yscr, offset, little_n: integer;
			InitialStateStr: Strarray;
			LStr: Array[0..7] Of Str1;
			Nodd: boolean;
	Begin
		SelectViewPort(2);
		SelectScale(4);
		Nodd := odd(N);
		If Nodd Then
			SetColor(lightgreen)
		Else
			SetColor(lightmagenta);
		LStr[0] := 's';
		LStr[1] := 'p';
		LStr[2] := 'd';
		LStr[3] := 'f';
		LStr[4] := 'g';
		LStr[5] := 'h';
		LStr[6] := 'i';
		LStr[7] := 'j';
		With InitialStates Do Begin
				L := N;
				index := statecount;
				While (L > 0) And (Index > 0) Do Begin
						little_n := round((N - L) / 2) + 1;
						j := 2 * count[index] - 1;
						InitialStateStr[index] := concat(NumStr(little_n, 1, 0), LStr[L], NumStr(j, 2, 0), '/2');
						If index - 1 > 0 Then Begin
								j := 2 * count[index - 1] - 1;
								InitialStateStr[index - 1] := concat(NumStr(little_n, 1, 0), LStr[L], NumStr(j, 2, 0), '/2');
							End;
						L := L - 2;
						index := index - 2;
					End;{for i}
				If (L = 0) Then Begin
						little_n := round((N - L) / 2) + 1;
						j := 2 * count[index] - 1;
						InitialStateStr[index] := concat(NumStr(little_n, 1, 0), LStr[L], NumStr(j, 2, 0), '/2');
					End;
				term1 := (N + (3 / 2));
				For i := 1 To statecount Do Begin
						term2 := (IsoParam.kappa * hbarwo / hbarwdelta) * energy[i];
						Map(0, term1 + term2, xscr, yscr);
						offset := 0;
						OutTextXY(xscr + 3, yscr + offset, InitialStateStr[i]);
						With InitLabel Do Begin
								xpos[i] := xscr + 3;
								ypos[i] := yscr + offset;
								oddN[i] := Nodd;
								InitialStr[i] := InitialStateStr[i];
							End;{with InitLabel}
					End;{for i}
			End;{with InitialStates}
	End;{LabelInitialStates}

	Procedure MNLabel1 (y: real;
									numb: integer;
									plus_minus: boolean);
		Const
			x = 4.5;
		Var
			xscr, yscr: integer;
			areal: real;
	Begin
		SelectViewPort(3);
		SelectScale(5);
		SetColor(lightcyan);
		Map(x, y, xscr, yscr);
		If plus_minus Then
			OutTextXY(xscr, yscr + 5, NumStr(numb, 3, 0))
		Else
			OutTextXY(xscr, yscr + 15, NumStr(numb, 3, 0));
	End;{MNLabel1}

	Procedure MNLabelDefLevels (var IsoParam: IsoParamType);
		Const
			x = 0.01;
		Var
			i, xscr, yscr: integer;
			bool3: boolean;
	Begin
		SelectViewPort(2);
		SelectScale(4);
		SetColor(lightcyan);
		bool3 := false;
		With isoParam Do Begin
				i := 1;
				While (i <= 3) And (MNcoord[i] <> 0.0) And (Not bool3) Do Begin
						Map(x, MNcoord[i], xscr, yscr);
						OutTextXY(xscr, yscr, MNLabel[i]);
						If i = 3 Then
							bool3 := true
						Else
							inc(i);
					End;
			End;{with IsoParam}
	End;{MNLabelDefLevels}

	Procedure RLine (a, b: pointtype);  far;
	Begin
      HideMouse;
		Line(b.x,{ b.y - linelength}0, b.x, {b.y + linelength}GetMaxY);
      ShowMouse;
	End;{RLine}

	Procedure DrawSingleDelta (var IsoParam: IsoParamType);
		Var
			x, y: integer;
			apoint: pointtype;
	Begin
		SelectScale(4);
		With IsoParam Do Begin
				x := MapX(singleDelta);
				y := MapY(yscalemax);
				apoint.x := x;
				apoint.y := y;
				If mode=dNuc Then SetColor(whitte)
            Else SetColor(lightgray);
     	   		RLine(apoint, apoint);
			End;{with IsoParam}
	End;{DrawSingleDelta}

	Procedure PrepareDeformedEnergyLevel (singleDelta: real;
									var IsoParam: IsoParamType);
		Const
			x1 = 0.0;
			x2 = 0.405;
		Var
			XTicks, YTicks: real;
	Begin
		If mode=dNuc Then
			SetUpDefNucleiScreen(IsoParam)
		Else
			SetUpNilssonScreen(IsoParam);
		With IsoParam Do Begin
				OpenViewPort(2);
				setColor(white);
            PutLabel(bottom,concat('Deformation Parameter ',delta));
				DefineScale(4, x1, x2, yscalemin, yscalemax);
				XTicks := Tickspace(x2 - x1);
				YTicks := Tickspace(yscalemax - yscalemin) / 2;
				SelectScale(4);
				Axis(0, yscalemin, XTicks, YTicks);
				DrawSingleDelta(IsoParam);
            defineViewPort(11,0,0.027,0.20,0.88);
            closeViewPort(11);
            SetColor(white);
            SetTextStyle(DefaultFont,VertDir,1);
            PrintXY(0.02,0.4,concat('E/hbarw(',delta,')'));
            SetTextStyle(DefaultFont,HorizDir,1);
      		OpenViewPort(3);
				Axis(-100, yscalemin, 200, YTicks);
            SetColor(white);
            if mode=dNuc then
               defineViewPort(12,0.5,0.98,0.17,0.20)
            else
               defineViewPort(12,0.5,0.98,0.07,0.115);
               closeViewPort(12);
            if mode=dNuc then
               PrintXY(0.65,0.18,concat('E/hbarw(',delta,') for ',delta,'=',NumStr(singledelta,4,2)))
            else
               PrintXY(0.65,0.08,concat('E/hbarw(',delta,') for ',delta,'=',NumStr(singledelta,4,2)));
			End;{with IsoParam}
	End;{PrepareDeformedEnergyLevel}

	Procedure DrawDeformedEnergyLevel (var LevelArray: LevelArrayType;
									N, numb: integer;
									Omega: real;
									var IsoParam: IsoParamType;
									var finalStateStr: Strarray;
									Var labelcheck1: labelmatrix;
									Var labelindex1, TotalStateCount, LevelIndex: integer;
									Var LevelStorage: LevelStorageType);
		Var
			temp, x, y: real;
			i, j, k, ii, xscr, yscr, offset, horizOffset, tempoffset, index: integer;
			Eplot, deltaplot: dVector;

		Procedure Block1;
			Var
				j: integer;
				done: boolean;
		Begin
			j := 1;
			done := false;
			While (j <= labelindex1) And (Not done) Do Begin
					If (yscr >= labelcheck1[1, j]) And (yscr <= labelcheck1[1, j] + labelheight) Then Begin
							If j + 1 = labelindex1 Then Begin
									If labelcheck1[2, j] = 0 Then
										horizOffset := 1
									Else If labelcheck1[2, j] = 1 Then
										horizOffset := 2
									Else
										horizOffset := 0
								End{if j+1 = labelindex1}
							Else Begin
									If labelcheck1[2, j] = 0 Then
										If labelcheck1[2, j + 1] = 1 Then
											horizOffset := 2
										Else
											horizOffset := 1
									Else If labelcheck1[2, j] = 1 Then
										If labelcheck1[2, j + 1] = 0 Then
											horizOffset := 2
										Else
											horizOffset := 0;
									done := true;
								End;{if j+1 < labelindex1}
						End;  { if (yscr>= labelcheck1[1,j])}
{is the label above too close?}
					If (yscr <= labelcheck1[1, j]) And (yscr >= labelcheck1[1, j] - labelheight) And (Not done) Then Begin
							If labelcheck1[2, j] = 0 Then
								horizOffset := 1
							Else
								horizOffset := 0;
							done := true;
						End; {if yscr<=labelcheck1[1,j]}
					inc(j);
				End;{while j}
		End; {procedure}

	Begin
		InitDVector(Eplot, 31);
		InitDVector(deltaplot, 31);
		With IsoParam Do Begin
				SelectViewPort(2);
				SelectScale(4);
				temp := -0.011;
				For k := 1 To 31 Do Begin
						temp := temp + 0.011;
						deltaplot.Put(k, temp);
					End;
				For i := 1 To numb Do Begin
						For k := 1 To 31 Do
							Eplot.Put(k, LevelArray[i, k]);
						If odd(N) Then
							SetColor(lightgreen)
						Else
							SetColor(lightmagenta);
						PlotDVectors(deltaplot, Eplot, 1, 31);
						With LevelStorage Do Begin
                     hidemouse;
						   If ((Eplot.value(1) > yscalemin) And
                        (Eplot.value(1) < yscalemax)) Or
                        ((Eplot.value(31) < yscalemax) And
                        (Eplot.value(31) > yscalemin)) Then Begin
								   LevelIndex := LevelIndex + 1;
									LevelIndexArray[TotalStateCount + i].LevelIndexStr := NumStr(LevelIndex, 2, 0);
                           If Eplot.value(31) > yscalemax Then Begin
									   index := 31;
										For k := 31 Downto 1 Do
										   If Eplot.value(k) > yscalemax Then
											   index := k;
										setColor(whitte);
										LevelIndexArray[TotalStateCount + i].xpos := MapX(deltaplot.value(index));
										LevelIndexArray[TotalStateCount + i].ypos := MapY(yscalemax) + 12;
										OutTextXY(MapX(deltaplot.value(index))-5, MapY(yscalemax) + 12, numstr(LevelIndex, 2, 0));
										End
									Else If Eplot.value(31) < yscalemin Then Begin
									   index := 1;
										For k := 1 To 31 Do
										   If Eplot.value(k) > yscalemin Then
											   index := k;
										setColor(whitte);
										LevelIndexArray[TotalStateCount + i].xpos := MapX(deltaplot.value(index));
										LevelIndexArray[TotalStateCount + i].ypos := MapY(yscalemin);
										OutTextXY(MapX(deltaplot.value(index)), MapY(yscalemin), numstr(LevelIndex, 2, 0));
										End
									Else If (Eplot.value(31) < yscalemax) And (Eplot.value(31) > yscalemin) Then Begin
									   xscr := MapX(deltaplot.value(31));
										yscr := MapY(Eplot.value(31));
										horizOffset := 0;
										block1;

                              Offset := MapX(0.025);
										SetColor(whitte);
										LevelIndexArray[TotalStateCount + i].xpos := xscr + MapX(horizOffset * Offset);
										LevelIndexArray[TotalStateCount + i].ypos := MapY(yscr);
										OutTextXY(xscr + horizOffset * Offset, yscr, numstr(LevelIndex, 2, 0));
										labelindex1 := labelindex1 + 1;
										labelcheck1[1, labelindex1] := yscr;
										labelcheck1[2, labelindex1] := horizOffset;
                              SortLabelCheck(labelindex1, labelcheck1);
                          End;{if (Eplot.value(31) < yscalemax) And (Eplot.value(31) > yscalemin)}
                          End{if}
								Else Begin
								   LevelIndexArray[TotalStateCount + i].LevelIndexStr := NumStr(0, 2, 0);
									LevelIndexArray[TotalStateCount + i].xpos := MapX(-1);
									LevelIndexArray[TotalStateCount + i].ypos := MapY(yscalemin - 20);
								End;
                    showmouse;
							End;{with LevelStorage}
					End;{for i}
				FreeDVector(deltaplot);
				FreeDVector(Eplot);
			End;{with IsoParam}
	End;{DrawDeformedEnergyLevel}

	Procedure DrawLevelStorage (var IsoParam: IsoParamType;
									var LevelStorage: LevelStorageType;
									TotalStateCount: integer;
									AddTo: boolean);
		Const
			x1 = 0.0;
			x2 = 6.5;
		Var
			i, j, index, horizOffset, templabel, tempenergy, levely, MNy: integer;
			scrx1, scry1, scrx2, scry2, labelcheckcount, Offset, horiOffset: integer;
			E, YTicks: real;
			labelcheck: labelmatrix;
			text: Str80;

		Procedure block1;
			Var
				j: integer;
				done: boolean;
		Begin
			j := 1;
			done := false;
			While (j <= labelcheckcount) And (Not done) Do Begin
					If (tempenergy >= labelcheck[1, j]) And (tempenergy <= labelcheck[1, j] + labelheight) Then Begin
							If j + 1 = labelcheckcount Then Begin
									If labelcheck[2, j] = 0 Then
										horizOffset := 1
									Else If labelcheck[2, j] = 1 Then
										horizOffset := 2
									Else
										horizOffset := 0
								End{if j+1 = labelindex}
							Else Begin
									If labelcheck[2, j] = 0 Then
										If labelcheck[2, j + 1] = 1 Then
											horizOffset := 2
										Else
											horizOffset := 1
									Else If labelcheck[2, j] = 1 Then
										If labelcheck[2, j + 1] = 0 Then
											horizOffset := 2
										Else
											horizOffset := 0;
									done := true;
								End;{if j+1< labelindex}
						End;{if(tempenergy>=labelcheck[1,j])}
					If (tempenergy <= labelcheck[1, j]) And (tempenergy >= labelcheck[1, j] - labelheight) And (Not done) Then Begin
							If labelcheck[2, j] = 0 Then
								horizOffset := 1
							Else
								horizOffset := 0;
							done := true;
						End;{if tempenergy<=labelcheck[1,j]}
					inc(j);
				End;{for j}
		End;{block1}

	Begin
		With IsoParam Do Begin
				With LevelStorage Do Begin
						SetColor(whitte);
                  if mode=dNuc then
                    defineViewPort(12,0.5,0.98,0.17,0.20)
                  else
                    defineViewPort(12,0.5,0.98,0.07,0.115);
                  closeViewPort(12);
                  if mode=dNuc then
                     PrintXY(0.65,0.18,concat('E/hbarw(',delta,') for ',delta,'=',NumStr(singledelta,4,2)))
                  else
                     PrintXY(0.65,0.08,concat('E/hbarw(',delta,') for ',delta,'=',NumStr(singledelta,4,2)));
						If AddTo Then
							SelectViewPort(3)
						Else
							OpenViewPort(3);
						DefineScale(5, x1, x2, yscalemin, yscalemax);
						SelectScale(5);
{calculate the array element that corresponds to the singledelta value}
						index := trunc(singledelta / 0.011);
						If index * 0.011 < singledelta Then
                     if (A >= 177) And (mode = dnuc) then
							   index := index + 2
                     Else
                       index := index + 1;
						labelcheckcount := 0;
						For i := 1 To TotalStateCount Do Begin
								SetColor(LevelColor[i]);
								horizOffset := 0;
								E := reducedE[i, index];
								tempenergy := MapY(E);
								If (E < yscalemax) And (E > yscalemin) Then Begin
										block1;

										labelcheckcount := labelcheckcount + 1;
										labelcheck[1, labelcheckcount] := tempenergy;
										labelcheck[2, labelcheckcount] := horizOffset;
										SortLabelCheck(labelcheckcount, labelcheck);
										Line(MapX(x1), tempenergy, MapX(0.85), tempenergy);
										Offset := Mapx(1.92);
										OutTextXY(MapX(0.97) + horizOffset * Offset, tempenergy, StateStr[i]);
										SetColor(whitte);
										OutTextXY(MapX(0.97) + horizOffset * Offset - MapX(0.3), tempenergy, levelindexarray[i].levelindexstr);
									End;{if}
							End;{for i}
					End;{with LevelStorage}
			End;{with IsoParam}
	End;{DrawLevelStorage}

	Procedure DrawNucleons (NumOfLevels: integer;
									var IsoParam: IsoParamType;
									var LevelRecord: LevelRecordType;
									esckey: boolean;
									Var DrawRecord: DrawType);
		Const
			nuc_r = 3;
			xint = 0.2;
			degeneracy = 2;
			solid = 1;
			dot = 11;
			plus = true;
			minus = false;
		Var
			i, j, temp, offset, LastLevel, netNucOld, netNuc, scrx, scry, numfill, numUnfill: integer;
			excited, NucCount, lastnucleonfilled, lastlevelfilled, firstlevelunfilled, countfilled, countunfilled, count: integer;
			x, scale, yMN: real;
			neg, plus_minus: boolean;
			expLevelStr, LevelStr: Str20;
         MN : Set Of 1..126;
	Begin
		HideMouse;
      MN := [2, 8, 20, 28, 50, 82, 126];
		SelectViewPort(3);
		SelectScale(5);
		SetAspectRatio(1, 1);
		setColor(lightblue);
		neg := false;
		count := 0;
		countfilled := 0;
		countunfilled := 0;
		With IsoParam Do Begin
				NucCount := NucleonsBelow;
				With LevelRecord Do Begin
						If NucCount In MN Then Begin
								yMN := energy[1];
								plus_minus := minus;
								setColor(lightblue);
							End;{if}
						netNucOld := OddNumNucleons - NucleonsBelow;
						With DrawRecord Do Begin
{******Ground state*********}
								With groundstate Do Begin
										For i := 1 To NumOfLevels Do Begin
												NucCount := NucCount + degeneracy;
												If NucCount In MN Then Begin
														yMN := energy[i];
														plus_minus := plus;
														setColor(lightblue);
													End;{if}
												If Not neg Then Begin
														netNuc := netNucOld - degeneracy;{each level has a degeneracy of 2 nucleons}
														If netNuc > 0 Then Begin
																numfill := degeneracy;
																numUnfill := 0;
																countfilled := countfilled + 1;
															End
														Else Begin
																neg := true;
																LevelStr := stateStr[i];
																LastLevel := i;
																numfill := netNucOld;
																numUnfill := abs(netNuc);
																countfilled := countfilled + 1;
																countunfilled := countunfilled + 1;
																lastnucleonfilled := count + 1;
																lastlevelfilled := countfilled;
																firstlevelunfilled := countunfilled;
															End;
														netNucOld := netNuc;
													End
												Else Begin
														numfill := 0;
														numUnfill := degeneracy;
														countunfilled := countunfilled + 1;
													End;
												scale := abs(xint - 1.0) / degeneracy;
												x := xint - scale;
												For j := 1 To numfill Do Begin
														count := count + 1;
														x := x + scale;
														Map(x, energy[i], scrx, scry);
														With nucpos[count] Do Begin
																xpos := scrx;
																ypos := scry;
																filled := true;
															End;{with nucpos}
                                          DrawCircle(x, energy[i], nuc_r,lightBlue);
													End;{for j fill}
												For j := 1 To numUnfill Do Begin
														count := count + 1;
														x := x + scale;
														Map(x, energy[i], scrx, scry);
														With nucpos[count] Do Begin
																xpos := scrx;
																ypos := scry;
																filled := false;
															End;{with nucpos}
                                          DrawCircle(x, energy[i], nuc_r, white);
													End;{for j unfill}
											End;{for i :=1 to NumOfLevels}
										NumNucleons := count;
										SelectViewPort(5);
										SelectScale(7);
										EraseXY(MapX(6.3), MapY(3.0), 10, darkgray);
										EraseXY(MapX(6.3), MapY(0.5), 10, darkgray);
										SetColor(whitte);
										If Not esckey Then Begin
												ModelExcitedStr := LevelStr;
												OutTextXY(MapX(6.3), MapY(3.0), levelstr);
											End;
										ExpExcitedStr := extra.statesStr[1];
										OutTextXY(MapX(6.3), MapY(0.5), ExpExcitedStr);
									End;{with groundstate}
								SelectViewPort(3);
								SetColor(lightblue);
{************Excited States************}
								If Not esckey Then Begin
										With FirstState Do Begin
												ExpExcitedStr := extra.statesStr[2];
												For i := 1 To count Do
													nucpos[i] := groundstate.nucpos[i];
												nucpos[lastnucleonfilled].filled := false;
												nucpos[lastnucleonfilled + 2].filled := true;
												ModelExcitedStr := stateStr[LastLevel + 1];
											End;{with FirstState}
										With SecondState Do Begin
												ExpExcitedStr := extra.statesStr[3];
												For i := 1 To count Do
													nucpos[i] := groundstate.nucpos[i];
												nucpos[lastnucleonfilled].filled := false;
												nucpos[lastnucleonfilled + 4].filled := true;
												ModelExcitedStr := stateStr[LastLevel + 2];
											End;{with SecondState}
									End;{if not esckey}
							End;{with DrawRecord}
					End;{with LevelRecord}
      	End;{with IsoParam}
         ShowMouse;
	End;{DrawNucleons}

	Procedure RedrawChangeStates (var DrawRecord: DrawType;
                                 var ExcitedState : StateType);
		Const
			nuc_r = 3;
			solid = 1;
			dot = 11;
		Var
			i: integer;
	Begin
	   SelectViewPort(5);
		SelectScale(7);
      setcolor(whitte);
		EraseXY(MapX(6.2), MapY(3.0), 12, darkgray);
		EraseXY(MapX(6.2), MapY(0.5), 12, darkgray);
		if escDef then begin
		   OutTextXY(MapX(6.2), MapY(3.0), 'Escaped Calc.');
         case excitedState of
            gs:
				   OutTextXY(MapX(6.2), MapY(0.5), DrawRecord.GroundState.ExpExcitedStr);
            first:
				   OutTextXY(MapX(6.2), MapY(0.5), 'Escaped Calc.');
            second:
				   OutTextXY(MapX(6.2), MapY(0.5), 'Escaped Calc.');
          end;{case}
       end
      else begin
         With DrawRecord Do Begin
				SetAspectRatio(1, 1);
				Case ExcitedState Of
					gs:  Begin
							With GroundState Do Begin
									OutTextXY(MapX(6.3), MapY(3.0), ModelExcitedStr);
									OutTextXY(MapX(6.3), MapY(0.5), ExpExcitedStr);
									setcolor(lightblue);
									SelectViewPort(3);
									For i := 1 To NumNucleons Do Begin
											With nucpos[i] Do Begin
													If filled = false Then Begin
															Setfillstyle(solid,  white);
                                             SetColor(white);
															fillellipse(xpos, ypos, nuc_r, nuc_r);
														End
													Else Begin
															Setfillstyle(solid, lightblue);
                                             SetColor(lightBlue);
															fillellipse(xpos, ypos, nuc_r, nuc_r);
														End;{else}
												End;{with nucpos}
										End;{for i}
								End;{with GroundState}
						End;{gs}
					first:  Begin
							With FirstState Do Begin
									OutTextXY(MapX(6.3), MapY(3.0), ModelExcitedStr);
									OutTextXY(MapX(6.3), MapY(0.5), ExpExcitedStr);
									setcolor(lightblue);
									SelectViewPort(3);
									For i := 1 To NumNucleons Do Begin
											With nucpos[i] Do Begin
													If filled = false Then Begin
															Setfillstyle(solid, white);
                                             SetColor(white);
															fillellipse(xpos, ypos, nuc_r, nuc_r);
														End
													Else Begin
															Setfillstyle(solid, lightblue);
                                             SetColor(lightBlue);
															fillellipse(xpos, ypos, nuc_r, nuc_r);
														End;{else}
												End;{with nucpos}
										End;{for i}
								End;{with FirstState}
						End;{First}
					second:  Begin
							With SecondState Do Begin
									OutTextXY(MapX(6.3), MapY(3.0), ModelExcitedStr);
									OutTextXY(MapX(6.3), MapY(0.5), ExpExcitedStr);
									setcolor(lightblue);
									SelectViewPort(3);
									For i := 1 To NumNucleons Do Begin
											With nucpos[i] Do Begin
													If filled = false Then Begin
															Setfillstyle(solid, white);
                                             SetColor(white);
															fillellipse(xpos, ypos, nuc_r, nuc_r);
														End
													Else Begin
															Setfillstyle(solid, lightblue);
                                             SetColor(lightBlue);
															fillellipse(xpos, ypos, nuc_r, nuc_r);
														End;{else}
												End;{with nucpos}
										End;{for i}
								End;{with SecondState}
						End;{second}
				End;{case ExcitedState}
			End;{with DrawRecord}
      end;{else}
	End;{RedrawChangeStates}

	Procedure InitDefDVector (Var deformedMatrix, dummyMatrix: dmatrix;
									Var diagonalElement, offdiagonalElement, eigenvalues, energies: dVector);
	Begin
		diagonalElement.init(8);
		offdiagonalElement.init(8);
		eigenvalues.init(8);
		energies.init(8);
		deformedMatrix.init(8, 8);
		dummyMatrix.init(1, 1);
	End;

	Procedure FreeDefDVector (Var deformedMatrix, dummyMatrix: dmatrix;
									Var diagonalElement, offdiagonalElement, eigenvalues, energies: dVector);
	Begin
		diagonalElement.free;
		offdiagonalElement.free;
		eigenvalues.free;
		energies.free;
		deformedMatrix.free;
		dummyMatrix.free;
   End;

	Procedure HandleNilssonHotKeys (key: byte;
                           var esckey: boolean);
	forward;

	Procedure CalcLevels (var IsoParam: isoParamType;
									Var LevelStorage: LevelStorageType;
									Var TotalStateCount, InitialStateCount: integer;
                           var DrawRecord: DrawType);

		Var
			s: real;
         LevelRecord: LevelRecordType;
         NumOfLevels : integer;
			Omega, deltafactor, delta, eta, term1, term2, hbarwdelta: real;
			BasisStateNum, NumOmega, N, k, p, pp, i, ii, kk, index, statecount, labelindex1, labelindex2, LevelIndex: integer;
			AddTo, esckey, evecbool: boolean;
			keybd: char;
			finalStateStr: Strarray;
			BasisState: arrayBasis;
			deformedMatrix, dummymatrix: dmatrix;
			diagonalElement, offdiagonalElement, eigenvalues, energies: dVector;
			LevelStr: Str20;
			LevelArray: LevelArrayType;
			InitialStates: InitialStatesMatrix;
			InitLabel: InitialLabelType;
			labelcheck1, labelcheck2: labelmatrix;
         key : byte;

		Procedure Block1;
			Var
				k, kk: integer;
		Begin
			With IsoParam Do Begin
					comment(0.0, 0.95, '/');
					Omega := Omega - 1.000;
					BasisStates(N, Omega, BasisState, BasisStateNum);
					delta := -0.011;
					For k := 1 To 31 Do Begin
							comment(0.0, 0.95, '-');
							delta := delta + 0.011;
							evecbool := false;
							CalcMatrixElements(N, BasisStateNum, BasisState, delta, Isoparam, deformedMatrix, deltafactor);
							SYM2TRID(BasisStateNum, deformedMatrix, diagonalElement, offdiagonalElement, evecbool);
							Tridiag(BasisStateNum, diagonalelement, offdiagonalelement, evecbool, dummymatrix);
							s := 1.0;
							eigenvalues.equate(s, diagonalelement);
							comment(0.0, 0.95, '|');
							SortEnergy(BasisStateNum, eigenvalues);
							If delta < 0.001 Then
								AsymptoticStates(N, BasisStateNum, Omega, finalStateStr);
							BuildLevelArray(eigenvalues, N, k, BasisStateNum, delta, deltafactor, IsoParam, LevelArray, hbarwdelta);

							If abs(IsoParam.singledelta - delta) <= 0.0055 Then Begin
								hbarwdelta := hbarwo * pwr(deltafactor, -1 / 6);
									For kk := 1 To BasisStateNum Do Begin
											term1 := (N + (3 / 2));
											term2 := (IsoParam.kappa * hbarwo / hbarwdelta) * eigenvalues.value(kk);
											energies.put(kk, term1 + term2);
											index := index + 1;
											With LevelRecord Do Begin
													energy[index] := energies.value(kk);
													stateStr[index] := finalStateStr[kk];
												End;{with LevelRecord}
										End;
								End;{if abs(singledelta - delta)<0.005}

							If (k = 1) Then Begin
									SortStates(BasisStateNum, eigenvalues, statecount, InitialStates);
								End;
						End;{for k}
					comment(0.0, 0.95, '|');
					If (N = IsoParam.Nmin) And (i = 1) Then
						PrepareDeformedEnergyLevel(singleDelta, IsoParam);
					DrawDeformedEnergyLevel(LevelArray, N, BasisStateNum, Omega, IsoParam,
                finalStateStr, labelcheck1, labelindex1, TotalStateCount, LevelIndex, LevelStorage);
					With LevelStorage Do Begin
							For kk := 1 To BasisStateNum Do Begin
									If odd(N) Then
										LevelColor[TotalStateCount + kk] := lightgreen
									Else
										LevelColor[TotalStateCount + kk] := lightmagenta;
									stateStr[TotalStateCount + kk] := finalStateStr[kk];
									For k := 1 To 31 Do
										reducedE[TotalStateCount + kk, k] := LevelArray[kk, k];
								End;{for kk}
						End;{with LevelStorage}
					comment(0.0, 0.95, '|');
					TotalStateCount := TotalStateCount + BasisStateNum;
					DrawLevelStorage(Isoparam, LevelStorage, TotalStateCount, AddTo);
				End;{with IsoParam}
		End;

	Begin
		InitDefDVector(deformedMatrix, dummymatrix, diagonalElement, offdiagonalElement, eigenvalues, energies);
		index := 0;
		labelindex1 := 0;
		labelindex2 := 0;
		LevelIndex := 0;
		esckey := false;
      escDef := false;
		AddTo := true;
		comment(0.0, 0.95, '|');
		With IsoParam Do Begin
				If (extra.states = gs) Or (extra.states = none) Then Begin
						TotalStateCount := 0;
						InitialStatecount := 0;
						N := Nmin;
						While (N <= Nmax) And (Not EscKey) Do Begin
								statecount := 0;
								NumOmega := N + 1;
								Omega := N + 1.50;
								i := 1;
								While (i <= NumOmega) And (Not EscKey) Do Begin
										CheckForEvents;
                              if NilssonHotKeys.pressed(key) then
                                 HandleNilssonHotKeys(key, esckey);
										If event.keypressed Then Begin
												beep;
												escKey := (event.ReadKey = chr(27));
											End;
										If Not EscKey Then
											Block1;
										inc(i);
									End;{for i}
								If (Not esckey) Or (N > Nmin) Then Begin
										LabelInitialStates(N, Nmax, statecount, hbarwdelta, IsoParam, InitialStates, InitLabel);
										For i := InitialStateCount + 1 To InitialStateCount + statecount Do Begin
												LevelStorage.InitialLabel.InitialStr[i] := InitLabel.InitialStr[i - InitialStateCount];
												LevelStorage.InitialLabel.xpos[i] := InitLabel.xpos[i - InitialStateCount];
												LevelStorage.InitialLabel.ypos[i] := InitLabel.ypos[i - InitialStateCount];
												LevelStorage.InitialLabel.oddN[i] := InitLabel.oddN[i - InitialStateCount];
											End;
									End;
								InitialStateCount := InitialStateCount + statecount;
								inc(N);
							End;{for N}
						SortLevelRecord(index, LevelRecord);
						NumOfLevels := index;
					End;{if states=gs}
			If NumOfLevels <> 0 Then Begin
						If mode=dNuc Then
							DrawNucleons(NumOfLevels, IsoParam, LevelRecord, esckey, DrawRecord);
						MNLabelDefLevels(IsoParam);
					End;{if}
			End;{with IsoParam}
		comment(0.0, 0.95, ' ');
      if esckey = true then begin
         escDef := true;
         MainHotKeys.display;
       end;
		FreeDefDVector(deformedMatrix, dummymatrix, diagonalElement, offdiagonalElement, eigenvalues, energies);
	End;{CalcLevels}

	Procedure HandleNilssonHotKeys (key: byte;
									var esckey: boolean);
		Var
		   i, k: integer;
			temp: real;
			Eplot, deltaplot: dVector;
			text: str80;
	Begin
		Case key Of
			1:{Help}
				ShowHelp('ShellMod.hlp ', 'HelpScreen4');{DisplayNilssonHelp;}
         2: begin   {Stop}
              beep;
              esckey := true;
             end;
		End;{case key}
	End;{HandleNilssonHotKeys}

	Procedure HandleMouseClick (Var IsoParam: IsoParamType;
									var LevelStorage: LevelStorageType;
									TotalStateCount: integer;
                           var PixelDelta : integer);
		Var
			a, olda, b: pointtype;
			x, y: integer;
			ratiopixel: real;
			AddTo: boolean;
			v: view;
	Begin
		SelectViewPort(2);
		SelectScale(4);
		v := views[viewPortNumber];
		x := MapX(IsoParam.singledelta);{deltavalue}
		y := MapY(IsoParam.yscalemax);
		a.x := x;
		a.y := y;
		olda.x := x;
		olda.y := y;
		SetColor(lightgray xor DarkGray);
      HideMouse;
		SetWriteMode(xorPut);
		RLine(a, a);
		SetWriteMode(CopyPut);
      ShowMouse;
		SetColor(white xor DarkGray);
		DrawRubberBand(Rline, a, b, DarkGray, true);
		b.y := y;
{constrain the line to be >= (delta=0.0) and <= (delta=0.33)}
		If b.x < 1 Then
			b.x := 1;
		If b.x > 214 Then
			b.x := 214;
		setColor(lightgray Xor DarkGray);
      HideMouse;
		SetWriteMode(xorPut);
		RLine(a, b);
		SetWriteMode(CopyPut);
      ShowMouse;
{*******convert pixels to scaled value*****}
		ratiopixel := b.x / (MapX(0.4) - MapX(0.0));
		pixeldelta := b.x;
		IsoParam.singledelta := 0.4 * ratiopixel;
		AddTo := false;
		DrawLevelStorage(IsoParam, LevelStorage, TotalStateCount, AddTo);
	End;{handlemouseclick}

	Procedure HandleDefNucleiButtons (key: byte; var DrawRecord: DrawType;
                                     var ExcitedState : StateType);
	Begin
		Case key Of
		 1: ExcitedState := gs;
		 2: ExcitedState := first;
		 3: ExcitedState := second;
		End;
      RedrawChangeStates(DrawRecord, ExcitedState);
	End;{HandleDefNucleibuttons}

   procedure TDeformedPotential.Init;
   begin
      active := false;
		With IsoParam Do Begin
				singledelta := 0.26;{deltavalue}
				OddNumNucleons := 0;
				With extra Do Begin
						prot := 0;
						neut := 0;
						states := none;
					End;{with extra}
			End;{with IsoParam}
   end;

   procedure TDeformedPotential.Free;
   begin
      active := false;
   end;

   procedure TDeformedPotential.SetUp;
	Var
      DrawRecord: DrawType;
   begin
      mode := dPot;
      closeViewPort(10);
		DefineViewPort(1, 0.3, 0.7, 0.885, 0.95);{1screen title}
		DefineViewPort(2, 0.028, 0.440, 0.12,  0.87);{2 energy levels vs ¶}
		DefineViewPort(3, 0.50, 0.995, 0.12,  0.87);{3 energy levels for one ¶}
		AssignLevelParam(MainMenu.rowchosen-2, IsoParam);
		hbarwo := 41 * pwr(IsoParam.A, -1 / 3);
		CalcLevels(IsoParam, LevelStorage,
                 TotalStateCount, InitialStateCount, DrawRecord);
      comment(0.0, 0.95, concat('Drag line to change ', delta));
      MainHotKeys.Display;
		selectviewport(2);
		pixeldelta := mapx(IsoParam.singledelta);{deltavalue}
   end;

   procedure TDeformedPotential.Control;
   var
     GraphEdge : integer;
     key : byte;
   begin
		graphedge := views[2].vx1;
      with event do with views[2] do
       If mouseclicked And (y>vy1) and (y<vy2) and
          (abs(x - pixeldelta - graphedge) <= 10) Then
				handleMouseClick(IsoParam, LevelStorage, TotalStateCount,pixeldelta);
   end;



   procedure TDeformedNuclei.Init;
   begin
      active := false;
   end;

   procedure TDeformedNuclei.Free;
   begin
      active := false;
      buttons.done;
   end;

   procedure TDeformedNuclei.SetUp;
   var
			LevelStorage: LevelStorageType;
			TotalStateCount, InitialStateCount: integer;
   begin
      mode := dNuc;
      closeViewPort(10);
		DefineViewPort(1, 0.26, 0.7, 0.885,0.95);{1screen title}
		DefineViewPort(2, 0.028, 0.44,  0.21, 0.88);{2 energy levels vs ¶}
		DefineViewPort(3, 0.50, 0.995, 0.21, 0.88);{3 energy levels for one ¶}
		DefineViewport(5, 0.50, 0.995, 0.06, 0.165);{port title - deformed shell model}
		DefineViewport(6, 0.32, 0.49, 0.06, 0.135);{port title - deformed shell model}
		excitedstate := gs;
      SetUpButtons(Buttons);
		Case MainMenu.rowChosen Of
			3:{Al} assignIsoParam(Al, IsoParam);
			4:{Eu} assignIsoParam(Eu, IsoParam);
			5:{Dy} assignIsoParam(Dy, IsoParam);
			6:{Tm} assignIsoParam(Tm, IsoParam);
			7:{Hf} assignIsoParam(Hf, IsoParam);
			8:{Pu} assignIsoParam(Pu, IsoParam);
		End;{case rowChosen}
		With IsoParam Do
			hbarwo := 41 * pwr(A, -1 / 3);
		CalcLevels(IsoParam, LevelStorage,
                 TotalStateCount, InitialStateCount,DrawRecord);
      MainHotKeys.Display;
   end;

   procedure TDeformedNuclei.Control;
   begin
      If buttons.changed Then
			HandleDefNucleiButtons(buttons.number, DrawRecord, ExcitedState);
   end;


{***************end Deformed Shell Model rountines*************}


{**************Menu and Hotkey handling routines**********}

	Procedure HandleShellModelHotKeys (key: byte;
									Var potential: ShellModelType;
                           var esckey: boolean);
		Var
			text: Str20;
			r, initV: dvector;
	Begin
		Case key Of
			1: {Help}
				ShowHelp('ShellMod.hlp ', 'HelpScreen2');{DisplayShellModelHelp;}
			2:  {DRAW/NO DRAW}
				Begin
					If potential.Drawbool Then Begin
							potential.Drawbool := false;
							text := 'F6-Draw';
							SetUpShellModelHotKeys(text);
						End
					Else If Not potential.Drawbool Then Begin
							potential.Drawbool := true;
							text := 'F6-No Draw';
							SetUpShellModelHotKeys(text);
						End;
				End;
			3: {pause}
				PressKeymessage;
         4: begin   {Stop}
             beep;
             esckey := true;
            end;

		End;{case key}
		ClearMKBuffers;
	End;{HandleShellModelHotKeys}


	Procedure HandleMainMenu;
	Begin
		With MainMenu Do
		 Case colChosen Of
		  1: Case rowChosen Of
				  1: AboutCUPS;
				  2: OpenMess;
				  3: Configuration;
				  5: QuitFlag := True;
			  End;
		  2: case rowChosen Of
				  1: shellPot.SetUp; {SphericalPotential;}
			   else DeformedPot.SetUp; {DeformedPotential;}
			  End;
		  3: case rowChosen Of
				  1: shellNuc.SetUp; {SphericalNuclei;}
			   else DeformedNuc.SetUp; {DeformedNucleus;}
			  End;
       End;
	End;{HandleMainMenu}

   procedure HandleMainHotKeys(key : integer);
   begin
      case key of
       1: case mode of
           main: ShowHelp('ShellMod.hlp ', 'HelpScreen1');{DisplayMainHelp;}
           sPot: ShowHelp('ShellMod.hlp ', 'HelpScreen2');{DisplayMainHelp;}
           sNuc: ShowHelp('ShellMod.hlp ', 'HelpScreen3');{DisplayMainHelp;}
           dPot: ShowHelp('ShellMod.hlp ', 'HelpScreen4');{DisplayMainHelp;}
           dNuc: ShowHelp('ShellMod.hlp ', 'HelpScreen5');{DisplayMainHelp;}
          end;
       2: if MainMenu.chosen then HandleMainMenu;
      end;
   end;


   procedure initialize;
   begin
	   quitflag := false;
      mode := main;
      shellPot.init;
      shellNuc.init;
      DeformedPot.init;
      DeformedNuc.init;

	   DefineScale(6, 1, 5, 1, 5);{for screen title and port title}
	   DefineScale(7, 0, 9, 0, 9);{for info ports in Shell Model Nuclei and Deformed Nuclei}
		DefineViewport(10, 0, 1, 0.05, 0.956);{screen size}
      SetGreek;{PC}
	   labelheight := TextHeight('[Set up]');
	   SetUpMainMenu;
      SetUpMainHotKeys;
      OpenMess;
   end;

var
   Key : byte;
Begin
	CUPSinit;
   initialize;

   ASSIGN(Output,Models2o_Output);
   REWRITE(Output);

	Repeat {until quit}
		CheckForEvents;
		If MainMenu.Activated Then handleMainMenu;
      if MainHotKeys.Pressed(key) then HandleMainHotKeys(key);
      case mode of
       sNuc : ShellNuc.Control;
       dNuc : DeformedNuc.Control;
       dPot : DeformedPot.Control;
      end;
	Until quitflag;
	mainmenu.done;
   CLOSE(Output);
	CUPSdone;
End.{program Models}