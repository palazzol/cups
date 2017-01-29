unit Pcommon;

 {---------------------------------------------------------------------------}
 {*-----        Declare all global variables, constants, and controls-------*}
 {-----------------------For Stellar Pulsation-------------------------------}
 {---------------------------------------------------------------------------}

 {Modified June 21, 1994}

interface

	uses
		crt, dos, graph, printer, CupsMUPP, CUPS, CUPSfunc, CupsProc, cupsgui, CUPSGrph;

	const
		radToDeg = 57.295;
		MaxUnsignedInt = 65535;
		maxNumShells = 20;
		maxnumPoints = 50;
		fourPi = 12.5663708;
		solarmass = 1.989e33; {gm, for computing dimensionless quantities for computation}
		solarLum = 3.827e33; {eg/sec}
		solarRad = 6.960e10; {cm}
		SolarDen = 1.4084; {mean density: gm/cm3}
		BolConst = 4.95;{mbol=-10logT/Tsun-5logR/Rsun}
		Gas = 8.314e07;{Gas constant: erg/deg/mole}
		Gravcon = 6.67e-08;{G ravitation constant: dyn cm2/gm2}
		Dhom = 1; {densitylaw}
		Dradcon = 2;
		DRad = 3;
		DCon = 4;
		adiabatic = 1; {heattrans}
		quasiadiabatic = 2;
		nonadiabatic = 3;
		Linear_Syn = 1; {equation}
		Linear_FDE = 2;
		nonlinear = 3;

 {physical variables}
		radi = 1;
		pres = 2;
		temp = 3;
		dens = 4;
		Hion = 5;
		Heion = 6;
		HepIon = 7;
		gamm = 8;{adiabatic gradient, dLnT/dLnP}
		flux = 9;
		volu = 10;
		velo = 11;
		W = 12;
		kapp = 13;
		lcmu = 14;
		Pe = 15;
		MRad = 16;
		disp = 17;
		RadGrad = 18; { dlnT / dLnP in radiative model , evaluated in Getfluxes }
		Entr = 19; {entropy}
		Bolmag = 20;  {bolometric magnitude, computed from R^2*T^4}
		none = 21;

		aboveRange = 1;
		BelowRange = 2;
		InRange = 3;

 {items for mode frequency window, reduced by 1}
		f_fund = 10;
		f_1 = 11;
		f_2 = 12;
		f_3 = 13;
		d_fund = 14;
		d_1 = 15;
		d_2 = 16;
		d_3 = 17;
		name_of_model2 = 6;
		p_fund = 19;
		p_1 = 20;
		p_2 = 21;
		p_3 = 22;

		SpecifyMode = 1;
		GraphMode = 2;
		runMode = 3;
		PhasorMode = 4;
		ListMode = 5;
		PlotMode = 6;
		Helpmode = 7;
      setDisplacementsMode=8;
      ListFreqMode = 9;

      NullK=1; {empty plot}
		stripk = 2;   {identifies tyope of plot in frame}
		linePlotk = 3;
		PhaseDiagk = 4;
		AnimDiagk = 5;
      PoincareDiag = 6;
		Structurek = 7;
      DispSetScrn=11; {ViewPort}
      acceptBut=12;
      escBut=13;
  {------------------------------------}
	type
 		{ColorType = (red, orange, yellow, green, blue); }
		unitsType = (solar, cgs);
		showShellType = set of 0..maxNumshells; {which  shells have been selected for plotting on strip and phase diagram;}
 {held in windowRec and used by graphUtils at plot  time; user may select as many}
 {shells as desired, up to numshells, by clicking on the icon in the appropriate window}

		variType = array[1..20] of real;
		RunstateType = (running, stopped);
		shelltype = record
				vari: variType;
				dm1, dm2, intMass: real; {mass of shell and mass of all shells interior to it }
				KeyGamma, nKap, sKap: real;
				SType: integer; {color for animation plot}
			end;

		modeType = record
				freq, I_freq, Lam, amp, phase: real; {real and imaginary (damping) frequency of mode,}
 {eigen value, mode amplitude, constant initial phase}
				Rshape: array[1..maxNumshells] of real; { RShape: displacement of each shell relative to surface; derived}
 {from eigenvector, XM, after removing normlization factor sqrt M}
				{SShape: array[1..maxNumshells] of real;} {Entropy displacements derived from SM}
				active: boolean; {TRUE if this mode is in the fourier composition?}
			end;

		ModelType = record
				densityLaw: integer;
				teff, grav: real; {effective temperature, computed in Build}
				Eq, Dyn: array[0..maxNumshells] of shelltype; {for equilibrium,dynamical, and starting conditions}
				ER1, ER2: array[1..maxNumshells] of real; {related to DR of Castorl, for finding density in linear theory}
			end;
 {refer to variable when plotting as, eq[i].vari[windowRec.frame.variable] and}
 {when computing as, eq[i].vari[pres]}


		VectorType = record
				E: array[1..maxNumShells] of extended;
				size: integer;{number of elements}
			end;

		SqMatrixtype = record
				E: array[1..maxNumShells, 1..maxNumShells] of extended;
				Lam: array[1..maxNumshells] of real;{eigen value}
				size: integer;
				EigVect: array[1..maxNumshells] of vectortype;
			end;
		dataVectorType = array[1..maxNumShells] of real;

		frameRec = record
				kind: integer;
				xvariable, yvariable: integer;

			end;
		stripRec = record
				olddata, newdata: array[1..5, 0..maxnumpoints] of real;

			end;
		LinePlotRec = record
				olddata, newData: dataVectorType;
				logarithmic: boolean;
			end;
		animDiagRec = record
				data, radData: dataVectorType;

			end;
		PhaseDiagRec = record
				oldXData, oldYData, newXdata, newYData: dataVectorType;

			end;
		structurerec = record
				logarithmic: boolean;
			end;
 {====================}
		SetUpRec = record
				fkind, sXvar, sYvar: array[1..4] of integer;
				sSHowShell: showShellType;
				sPointScale, sIsZoomed: boolean;
				sZoomedframe, sdensityLaw, sNumshells, sequation, sHeatTrans: integer;
				sModelName: string[30];
				sLum, sRad, sMass, sHabund, sHeAbund, sMetabund, sCoreMass, sEnvelMass: real;
				sMActive: array[0..maxnumshells] of boolean;
				sMAmp: array[0..maxnumshells] of real;
			end;

     {================================}
	var
		GenHotkeys,moveHotKeys: THotKeys; {hot keys for general status}
                               {and for select stars mode}
		modelBuilt: boolean; {is the model ready for plotting?}
		gOpMode: integer;

		quitNow: boolean; {exit program}
		MyMenu: Tmenu;

 { these variables control the operation of the program and are set by the user}
		Synthesize: boolean;{do Fourier synthesis using modes}

 {output}
		unitsSelected: unitsType;    {used in display of model parameters}

 {these variables describe a particular model and are set by user}
		ModelName: string[30]; {user-defined name of the current model}
		starLum, starRad, StarMass, starMeanDen: real;{luminosity,surface gravity,radius, mass in solar units}
		HAbund, HeABund, MetAbund: real; {abundances by number}
		Numshells: integer; {number of shells in model}
                { Modsel        :Integer;}
 {desgnates model being built}
		equation: integer; {linear_Syn,linear_FDE,nonlinear}
		heatTrans: integer;
		coremass, envelmass: real;
		mode: array[0..maxNumshells] of modetype; {mode[i].R_freq; mode[i].I_freq; mode[i].ampl[j]; mode[i].phase[j]}
		Star: modelType;

 {control}
		time: real;  {current integration time, reset by StartCalc}
		dt: array[linear_Syn..nonlinear] of real; {time increments for diff. methods}
		runstate: runstatetype;
		calcdone: boolean;
		counter, framecount: integer;       {keep track of time steps}
		building: boolean;

 {the following are used for linear analysis; Z is square}
 {see Castor Ap J 166,109, 1971}
		zMat: SqMatrixType; {  z is eigenvector array}
		dVect, eVect: vectortype;
		XM, XMT: array[1..maxNumShells] of real; {linear mode displacements, normalized by sqrt m; and temp array}
		G1, Aij: array[1..maxNumshells, 1..3] of real;{Aij is tri-diagonal derived from G1}

 {display/output }
		maxAbsVal, maxRelVal, minRelVal: array[1..20] of real;{For scales on graphs; evaluated in Static Model and getScales}
		degreeunits: boolean;
		logYscale: real;{used for log plots in ShowStatic; set in GetShowStaticScales}
		skipframe: integer;     {for writing to screen}
		outfileName: string; {name of images file to be written}

		DataValue: integer; {for parsing input data}

		IsPrinting: boolean;
		colorscale: real;
		gDoingMore: boolean;
		IsZoomed: boolean;

		SpecifyScrn, ModeSetScrn: tInputScreen;

                 {ploting stuff}

                 {gVarID:array[1..8] of integer; }
{identify data for frames}
		sb, st: array[1..8] of real;  {vertical scale limits}
		xr, xl: real;  {horizontal scale limits}
		axisLabel: array[1..2, 1..8] of string[20];
		logMyLabel, NatMyLabel: array[1..20] of string[20];
		myRight, myLower, myUpper: array[1..8] of real; {for axes of each frame}
		frame: array[1..8] of framerec;     {5-8 are for structure}
		StripChart: array[1..4] of striprec;
		LinePlot: array[1..4] of LinePlotRec;
		PhaseDiag: array[1..4] of PhasediagRec;
		AnimDiag: array[1..4] of AnimDiagRec;
		ZoomedFrame: integer;
		varLabel, kindLabel: array[1..6] of string[15];
		FTypeButton:array[1..4] of TButtons;
                FVarButton: array[1..4] of TButtons;
		structureData: structureRec;
		justCleared: boolean;
		showshell: showshellType;
		SetUpData: setuprec;
		massPointScale: Boolean;
      ShellSel:integer;
      tempVector,initDisp,initVelo,PhX,PhY:array[1..maxnumshells] of real;
      ManualDisp:boolean;{true if initial displacements were set manually}
  {---------------------------------------------------------------------------}

implementation
end.