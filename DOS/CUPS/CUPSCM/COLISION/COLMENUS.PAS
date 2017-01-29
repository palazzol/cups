           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.5 (95/02/13) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

	 {**********************************************}
	 {**********************************************}
	 {**           Unit ColMenus.Pas              **}
	 {**          Program Colision.Pas            **}
	 {**          Two Body Collisions             **}
	 {**        Written by Bruce Hawkins          **}
	 {**           March 22, 1993                 **}
	 {**        Last change:  2/ 1/95             **}
	 {**             Version 1.00                 **}
	 {**                                          **}
	 {**      (c) 1995 John Wiley & Sons, Inc.    **}
	 {**             CUPS Project                 **}
	 {**                                          **}
	 {**     Uses 12/94 MUPPET & CUPS Utilities   **}
	 {**********************************************}
	 {**********************************************}

{         Contains the menus and hotkeys and handling routines      }
{$F+}
UNIT ColMenus;

INTERFACE

USES	Dos, CupsMupp, Graph, Cups, CupsFunc, CupsProc, CupsGui, CupsGrph,
		ColPhy, ColTutor;

TYPE
	WhichPlot = (CofM, Body1, Body2, Lab, NoPlot);
VAR
	MainMenu, HelpMenu : TMenu;
	HelpKeys : THotKeys;
	ArithmeticProg : Boolean;
	OpenScreen : HelpScrType;
	EnoughForces : Integer;

PROCEDURE ShowTrajectories;
PROCEDURE QuitRun;
PROCEDURE SetUpMenus;
PROCEDURE SetUpMainKeys;  {Function keys }
PROCEDURE DoMainKeys (key : byte);  {Act on the function key presses}
PROCEDURE SetUpRunKeys;
PROCEDURE HandleMenu;     {Main menu routine does all the work}
PROCEDURE HandleHelpMenu;
PROCEDURE SpreadIt(VAR RepCount : Integer);
PROCEDURE PreviousForce(SwapForces : Boolean);

IMPLEMENTATION

CONST
	MyVersion = 1.00;
	SpreadConstant = 1.1;
	ChangeOthers : Boolean = True;
	Vary_Second : Boolean = False;

VAR
	dataScreen : TInputScreen;
	SaveImpactPar, Savekc, Savekw, Saveks, Saveky,
		Savea, SaveR0, SaveEnergy, SaveM : Real;

PROCEDURE SetUpAMenu(VAR Menu : TMenu);
BEGIN
	Menu.Init;
	WITH Menu DO
	BEGIN
	  column(1, 'Files');
		 row(1,1, 'About Collisions');
		 row(1,2, 'About Cups');
		 row(1,3, 'Configure');
		 row(1,4, '------------------');
		 row(1,5, 'Read Cross Section File');
		 row(1,6, 'Save Cross Section File');
		 row(1,7, 'Read Collision History File');
		 row(1,8, '     Read History Options');
		 row(1,9, 'Save Collision History File');
		 row(1,10, 'Exit Program (Alt-x)');
	  column(2, 'Plots&Zoom');
		 row(2,1, 'Choose One Window');
		 row(2,2, 'Trajectories Only');
		 row(2,3, 'Vectors Only');
		 row(2,4, 'Trajectories & Vectors');
		 row(2,5, 'Zoom In');
		 row(2,6, 'Zoom Out');
		 row(2,7, 'Default Scales');
		 row(2,8, 'Display Saved Cross Sections');
	  column(3, 'Forces');
		 row(3,1, 'Pure Coulomb');
		 row(3,2, 'Coulomb with Shielding');
		 row(3,3, 'Coulomb truncated near r=0');
		 row(3,4, 'Other Power Law');
		 row(3,5, 'Hard Sphere');
		 row(3,6, 'Soft Sphere');
		 row(3,7, 'Yukawa');
		 row(3,8, 'Woods-Saxon');
		 row(3,9, 'User Defined');
		 row(3,10,'Previous Force');
	  column(4, 'HELP!!');
		 row(4,1,'Help');
		 row(4,2,'Tutorial')
	END;
END; {SetUpAMenu}

PROCEDURE SetUpMenus;
BEGIN
	SetUpAMenu(MainMenu);
	SetUpAMenu(HelpMenu);
	MainMenu.RowActivate(1,6,False);
	MainMenu.RowActivate(1,9,False);
	MainMenu.RowActivate(2,7,False);
	MainMenu.RowActivate(2,8,False);
	HelpMenu.column(5, 'Quit Help');
	HelpMenu.column(4, 'More');
	HelpMenu.row(4, 2, 'Main F-keys');
	HelpMenu.row(4, 3, 'Collision Types');
	HelpMenu.row(4, 4, 'Collision Conditions');
	HelpMenu.row(4, 5, 'Vary: Impact Parameter');
	HelpMenu.row(4, 6, 'Vary: Energy, Force, etc.');
	HelpMenu.row(4, 7, 'Plot Cross Section');
	HelpMenu.row(4, 8, 'Coulomb Choices');
	HelpMenu.row(4, 9, 'Cross Section Display List');
	HelpMenu.row(4, 10, 'Cross Section Plot Changes');
	MainMenu.Display
END; {SetUpMenus}

PROCEDURE Restart;
BEGIN

END;

{----------- Set up the various function key displays, ----------}
{-----------  Each is followed by its action routine   ----------}

{---------- Here are the actions taken when menu items are picked ----------}
PROCEDURE OpenHelpPort(Color : Byte);
BEGIN
	GraphBackColor := Color; SetColor(White);
	OpenMyViewPort(AllPort);
	SetTextStyle(SmallFont, HorizDir, {Size=} 4);
	SetUserCharSize(11,8,7,4);
END; {OpenHelpPort}

PROCEDURE AboutCollisions;
BEGIN
	 OpenHelpPort(Blue);
	 Print(28,1,'About Collisions');

	 Print (5,3,'A simulation of two body collisions.');
	 Print (5,4,'You may find more information about the various types of collisions');
	 Print (5,5,'from the Help menu.');

	 Print (5,7,'Trajectories are displayed in the center of mass and lab systems,');
	 Print (5,8,'and velocity and momentum vector diagrams are also shown.');

	 Print (5,10,'Cross sections are calulated, with a tutorial on the cross section concept.');


	 SetTextStyle(TriplexFont,HorizDir,4); SetUserCharSize(1,2,3,5);
	 Print (21,16,'Written by Bruce Hawkins');
	 Print (15,17,'Smith College Physics Department, 1992.');
	SetTextStyle(DefaultFont, HorizDir, 1)
END; {AboutCollisions}

PROCEDURE ShowOne;
BEGIN
	SetAllFalse;
	WITH DataScreen DO
	BEGIN
		Init;
		LoadLine ('    Choose a Window');
		Loadline ('');
		LoadLine ('#1 Laboratory Trajectories');
		LoadLine ('#1 Center of Mass Trajectories');
		LoadLine ('#1 Laboratory Vectors');
		LoadLine ('#1 Center of Mass Vectors');
		LoadLine ('');
		SetHelpFile('colision.hlp','OneFrameHelp');
		LoadLine ('[ OK ]  [Cancel] [Help]');
		DefineInputPort(0.06,0.46,0.30,0.65);
		AcceptScreen;
		IF Not Canceled THEN
		BEGIN
			CASE GetRadioButton('1') OF
				1 : PlotLab := True;
				2 : PlotCofM := True;
				3 : BEGIN PlotLabVectors := True; Vectors := True END;
				4 : BEGIN PlotCofMVectors := True; Vectors := True END
			END; {Case}
			Plots := 1;
			HideCursor;
			SetPlots;
			ClearPlots(True);
			ShowCursor
		END; {Not Canceled}
		Done;
	END;
END; {ShowOne}

PROCEDURE ShowVectors;
BEGIN
	SetAllFalse;
	Vectors := True;
	PlotLabVectors := True;
	PlotCofMVectors := True;
	Plots := 2;
	SetPlots;
	ClearPlots(True)
END; {ShowVectors}

PROCEDURE ShowTrajectories;
BEGIN
	SetAllFalse;
	PlotLab := True;
	PlotCofM := True;
	Plots := 2;
	SetPlots;
	ClearPlots(True)
END; {ShowTrajectories}

PROCEDURE ShowBoth;
BEGIN
	SetAllFalse;
	Vectors := True;
	PlotLabVectors := True;
	PlotCofMVectors := True;
	PlotLab := True;
	PlotCofM := True;
	Plots := 4;
	SetPlots;
	ClearPlots(True)
END; {ShowBoth}

PROCEDURE NoHelp;
BEGIN
	MildAnnounce ('Sorry, this topic is not yet available.')
END; {NoHelp}

 PROCEDURE SetSoft(VAR Screen : TInputScreen; Line : Integer);
 BEGIN
	Screen.LoadLine ('Soft Sphere Radius: {     }');
	Screen.LoadLine ('Force strength K : {     }');
	Screen.SetNumber (Line,R0);
	Screen.SetNumber (Line+1,ks);
 END;

 PROCEDURE GetSoft(VAR Screen : TInputScreen; Line : Integer);
 BEGIN
	R0 := Screen.GetNumber(Line);
	ks := Screen.GetNumber(Line+1);
	R0_2 := Sqr(R0)
 END;

 PROCEDURE SetHard(VAR Screen : TInputScreen; Line : Integer);
 BEGIN
	Screen.LoadLine ('Hard Sphere Radius: {     }');
	Screen.SetNumber (Line,R0);
 END;

 PROCEDURE GetHard(VAR Screen : TInputScreen; Line : Integer);
 BEGIN
	R0 := Screen.GetNumber(Line);
	R0_2 := Sqr(R0)
 END;

 PROCEDURE SetYukawa(VAR Screen : TInputScreen; Line : Integer);
 BEGIN
	Screen.LoadLine ('   Well Depth Vo : {     }');
	Screen.LoadLine ('  Well Radius Ro : {     }');
	Screen.SetNumber (Line,ky);
	Screen.SetNumber (Line+1,R0);
 END;

 PROCEDURE GetYukawa(VAR Screen : TInputScreen; Line : Integer);
 BEGIN
	ky := Screen.GetNumber (Line);
	R0 := Screen.GetNumber (Line+1);
 END;

 PROCEDURE SetWSaxon(VAR Screen : TInputScreen; Line : Integer);
 BEGIN
	Screen.LoadLine ('   Well Depth Vo : {     }');
	Screen.LoadLine ('  Well Radius Ro : {     }');
	Screen.LoadLine ('Well Steepness a : {     }');
	Screen.SetNumber (Line,kw);
	Screen.SetNumber (Line+1,R0);
	Screen.SetNumber (Line+2,a)
 END;

 PROCEDURE GetWSaxon(VAR Screen : TInputScreen; Line : Integer);
 BEGIN
	kw := Screen.GetNumber (Line);
	R0 := Screen.GetNumber (Line+1);
	a := Screen.GetNumber (Line+2)
 END;

PROCEDURE SetSecondForce;
VAR DataScreen2 : TInputScreen;
BEGIN
	WITH DataScreen2 DO {Must NOT be DataScreen1, or trouble!}
	BEGIN
		Init;
		Loadline (' Second Force Parameters');
		Loadline ('  '+ForceName);
		Loadline ('Coulomb k is '+NumStr(kc,5,3));
		LoadLine ('');
		Case SecondForce OF
			HardSphere : SetHard(DataScreen2, 1);
			SoftSphere : SetSoft(DataScreen2, 1);
			Yukawa : SetYukawa(DataScreen2, 1);
			WSaxon : SetWSaxon(DataScreen2, 1);
		END; {Case SecondForce}
		Loadline ('');
		LoadLine (' [ OK ]   [Cancel] [Help]');
		SetHelpFile('colision.hlp','Second Force Help');
		DefineInputPort (0.08,0.47,0.35,0.77);
		AcceptScreen;
		If Not Canceled THEN
		BEGIN
		Case SecondForce OF
			HardSphere : GetHard(DataScreen2, 1);
			SoftSphere : GetSoft(DataScreen2, 1);
			Yukawa : GetYukawa(DataScreen2, 1);
			WSaxon : GetWSaxon(DataScreen2, 1);
		END; {Case SecondForce}
		END; {Not Canceled}
		Done
	END {With DataScreen}
END; {SetSecondForce}

PROCEDURE GetInitialConditions;
VAR
	DataScreen1 : TInputScreen;
	SavKc, SavKs, SavKy, SavKw, SavR0, SavA, SavPower : Real;
	SetSecond : Boolean;

 PROCEDURE SetCoulomb;
 BEGIN
	DataScreen1.LoadLine ('Force Strength K: {     }');
	IF TwoForces THEN
	 DataScreen1.Loadline('#F Set 2nd Force Parameters');
	DataScreen1.SetNumber(10,kc)
 END;

 PROCEDURE GetCoulomb(VAR SetSecond : Boolean);
  BEGIN
	kc := DataScreen1.GetNumber(10);
	IF TwoForces AND DataScreen1.GetBoolean(11) THEN SetSecond := True
 END;

 PROCEDURE SetPower;
 BEGIN
	SetCoulomb;
	DataScreen1.LoadLine (' Force Law Power : {     }');
	DataScreen1.SetNumber (11,Power)
 END;

 PROCEDURE GetPower;
 BEGIN
	GetCoulomb(SetSecond);
	Power := DataScreen1.GetNumber(11)
 END;

 PROCEDURE SetShield;
 BEGIN
	SetCoulomb;
	DataScreen1.LoadLine ('Shielding Radius : {     }');
	DataScreen1.SetNumber (11,R0);
 END;

 PROCEDURE GetShield;
 BEGIN
	GetCoulomb(SetSecond);
	R0 := DataScreen1.GetNumber(11);
	R0_2 := Sqr(R0)
 END;

 PROCEDURE SetTrunc;
 BEGIN
	SetCoulomb;
	DataScreen1.LoadLine ('Truncation Radius: {     }');
	DataScreen1.SetNumber (11,R0);
 END;

 PROCEDURE GetTrunc;
 BEGIN
	GetCoulomb(SetSecond);
	R0 := DataScreen1.GetNumber(11);
	R0_2 := Sqr(R0)
 END;

 PROCEDURE SetUser;
 BEGIN
	DataScreen1.LoadLine ('User Constant U1 : {     }');
	DataScreen1.LoadLine ('User Constant U2 : {     }');
	DataScreen1.LoadLine ('User Constant U3 : {     }');
	DataScreen1.LoadLine ('User Constant U4 : {     }');
	DataScreen1.SetNumber (10,U1);
	DataScreen1.SetNumber (11,U2);
	DataScreen1.SetNumber (12,U3);
	DataScreen1.SetNumber (13,U4);
 END;

 PROCEDURE GetUser;
 BEGIN
	U1 := DataScreen1.GetNumber (10);
	U2 := DataScreen1.GetNumber (11);
	U3 := DataScreen1.GetNumber (12);
	U4 := DataScreen1.GetNumber (13)
 END;

BEGIN {GetInitialConditions}
	SavKc := kc; SavKs := ks; SavKy := ky; SavKw := kw;
	SavR0 := R0; SavA := a; SavPower := Power;
	SetSecond := False;
	With DataScreen1 DO
	BEGIN
		Init;
		IF Varying AND (Vary_What = FType) THEN
		BEGIN
			LoadLine('Will show one collision');
			LoadLine('  for each force law.')
		END
      ELSE
      BEGIN
			LoadLine ('   Collision Conditions');
		   LoadLine ('')
      END;
		LoadLine ('#T Show Center of Mass');			{Field 1}
		LoadLine ('#T Show Impact Parameter');			{Field 2}
      Loadline ('#F Show Effective Potential');    {Field 3}
		LoadLine ('#1 Show Object Now');					{Field 4}
		LoadLine ('#1 Show Object History ');			{Field 5}
		LoadLine ('#1 Show Paths Only');					{Field 6}
		LoadLine ('Impact Parameter : {     }');		{Field 7}
		LoadLine ('Mass Ratio m2/m1 : {    3}');		{Field 8}
		LoadLine ('          Energy : {     }');		{Field 9}
		SetBoolean(1,ShowCofM);
		SetBoolean(2,ShowImpactP);
		SetBoolean(3,ShowEffective);
		SetBoolean(5,Not ErasePast);
		SetBoolean(6,ShowPaths);
		SetNumber (7,b);
		SetNumber (8,M[2]/M[1]);
		SetNumber (9, Energy);
		SetHelpFile('colision.hlp','Set Condition Help');
		CASE WhichForce OF
			Coulomb : SetCoulomb;
			FPower : SetPower;
			HardSphere : SetHard(DataScreen1, 10);
			SoftSphere : SetSoft(DataScreen1, 10);
			ShieldedC : SetShield;
			TruncatedC : SetTrunc;
			Yukawa : SetYukawa(DataScreen1, 10);
			WSaxon : SetWSaxon(DataScreen1, 10);
			User : SetUser
		END; {Case}
		LoadLine ('');
		IF Varying THEN
		BEGIN
			Loadline ('You can''t change the');
			LoadLine ('parameter that is varying.')
		END;
		LoadLine (' [ OK ]   [Cancel] [Help]');
		DefineInputPort (0.08,0.47,0.10,0.87);
		AcceptScreen;
		IF Canceled THEN Quit := True
		ELSE BEGIN
			ShowCofM := GetBoolean(1);
			ShowImpactP := GetBoolean(2);
         ShowEffective := GetBoolean(3);
			ErasePast := GetBoolean(4);
			ShowPaths := GetBoolean(6);
			b := GetNumber (7);
			M[2] := GetNumber (8)*M[1];
			Energy := GetNumber(9);
			CASE WhichForce OF
				Coulomb : GetCoulomb(SetSecond);
				FPower : GetPower;
				HardSphere : GetHard(DataScreen1, 10);
				SoftSphere : GetSoft(DataScreen1, 10);
				ShieldedC : GetShield;
				TruncatedC : GetTrunc;
				Yukawa : GetYukawa(DataScreen1, 10);
				WSaxon : GetWSaxon(DataScreen1, 10);
				User : GetUser
			END; {Case}
		END;
		Done
	END; {With DataScreen}
	IF SetSecond THEN SetSecondForce;
	IF ((Abs(SavKc-Kc)/SavKc) > 0.1) OR ((Abs(SavKs-Ks)/SavKs) > 0.1)
		OR ((Abs(SavKy-Ky)/SavKy) > 0.1) OR ((Abs(SavKw-Kw)/SavKw) > 0.1)
		OR ((Abs(SavR0-R0)/SavR0) > 0.1) OR ((Abs(SavA-A)/SavA) > 0.1)
		OR ((Abs(SavPower-Power)/SavPower) > 0.1) THEN
			IF Vary_What = None THEN DisplayPotential(True)
END; {GetInitialConditions}

PROCEDURE SetXPlot;
BEGIN
	SetAllFalse;
	Plots := 2;
	PlotLab := True;
	PlotXSection := True;
	SetPlots;
	ClearPlots(True)
END; {SetXPlot;}

CONST Concentration : Integer = 1;

PROCEDURE SetImpactRange;
VAR
	DataScreen2, DataScreen3 : TInputScreen;
	SaveShow : Boolean;
	i, SaveInterval : Integer;
	temp1, temp2, tempScale, PMult, Angle, OldAngle : Real;
BEGIN
	With DataScreen2 DO
	BEGIN
		Init;
		IF Vary_What = XSect THEN
			BEGIN
				LoadLine ('Cross Section Parameters');
				SetHelpFile('colision.hlp','XSect Help')
			END
			ELSE
			BEGIN 
				LoadLine ('Impact parameter range');
				SetHelpFile('colision.hlp','Range Help')
			END;
		LoadLine ('');
		LoadLine ('{ 10} Impact Parameters');
		LoadLine ('#1 Evenly Distributed');
		LoadLine ('#1 Concentrated');
		LoadLine ('#1 Heavily Concentrated');
		LoadLine ('#F Change other parameters');
		IF Vary_What = XSect THEN
		BEGIN
			LoadLine ('#2 Save   #2 Don''t Save');
			SetRadioButton('2',2)
		END;
		SetRadioButton('1',Concentration);
		SetBoolean(5,ChangeOthers);
		LoadLine ('[ OK ]  [Cancel] [Help]');
		SetNumber(1,NumberOfReps);
		SetNumberLimits(1,2,100);
		DefineInputPort (0.10,0.45,0.28,0.72);
		AcceptScreen;
		IF Canceled THEN
		BEGIN Quit := True; Vary_What := None END
		ELSE BEGIN
			ShowPaths := True; ErasePast := False;
			ChangeOthers := GetBoolean(5);
			IF ChangeOthers THEN GetInitialConditions;
			NumberOfReps := Trunc(GetNumber(1));
			DeltaMult := 1.0;
			IF WhichForce in [HardSphere,SoftSphere] THEN
				TempScale := (1-0.5/NumberOfReps)*R0
			ELSE TempScale := (1-0.5/NumberOfReps)*YScale;
			Concentration := GetRadioButton('1');
			Case Concentration OF
				1 : DeltaB :=  TempScale/(NumberOfReps-0.5);
				2 : DeltaMult := SpreadConstant;
				3 : DeltaMult := Sqr(SpreadConstant);
			END; {Case}
			Case Concentration OF
				1 : BEGIN
						Temp1 := TempScale;
						Temp2 := DeltaB;
						ProbNorm := Temp1*DeltaB
					 END;
				2, 3 : BEGIN
							temp1 := 0.5;
							temp2 := 1.0;
							FOR i := 1 to NumberOfReps-1 DO
							BEGIN
								temp1 := temp1 + temp2;
								temp2 := temp2*DeltaMult
							END;
							DeltaB := TempScale/temp1;{Initial value}
							temp1 := Temp1*DeltaB;
							Temp2 := temp2*DeltaB/DeltaMult
						END {Case 2,3}
			END; {Second Case}
			Saving := False;
			IF Vary_What = XSect THEN
			BEGIN
				IF (GetRadioButton('2') = 1) THEN
				BEGIN
					IF (XSaved >= MaxXSects) THEN
						MildAnnounce(Concat('You can only save ',
							NumStr(MaxXSects,2,0),'cross sections'))
					ELSE
					With DataScreen3 DO
					BEGIN
						Init;
						LoadLine ('Describe this cross section somehow:');
						LoadLine ('"                         "');
						LoadLine ('You will use the description to');
						Loadline ('choose plots to display.');
						LoadLine ('      [ OK ]     [Cancel]');
						DefineInputPort(0.05,0.53,0.4,0.65);
						AcceptScreen;
						IF Canceled THEN
						BEGIN
							Quit := True; Varying := False; Vary_What := None
						END
						ELSE
						BEGIN
							Saving := True;
                     MainMenu.RowActivate(2,8,True);
                     MainMenu.RowActivate(1,6,True);
							XSaved := XSaved + 1;
							XDescription[XSaved] := GetString(1)+': '+ForceName;
							SavForceT[XSaved] := WhichForce;
							SavProb[XSaved].ReSize(NumberOfReps);
							SavAngle[XSaved].ReSize(NumberOfReps)
						END;
						Done
					END {With}
				END; {IF Cross Section}
				IF Not Quit THEN
				BEGIN
				 SaveInterval := DotInterval; SaveShow := ShowImpactP;
				 DotInterval := 10000; ShowImpactP := False; {Don't Display graph}
				 Comment(0.2,0.9,'One moment, please, collecting preliminary data');
				 b := Temp1-Temp2; InitializeParameters;
				 Running := True; ShowOrbit(OldAngle, True);
				 b := Temp1; InitializeParameters;
				 Running := True; ShowOrbit(Angle,True);
				 RunKeys.Display;
				 ProbNorm := b*Temp2/(Abs(Sin(Angle))*Abs(OldAngle-Angle));
				 IF Saving THEN SavProb[XSaved].Put(1,ProbNorm);
				 DotInterval := SaveInterval; ShowImpactP := SaveShow;
				 Comment (0.2,0.9,'                                            ')
				END {Not Quit}
			END; {XSect}
			IF Not Quit THEN
			BEGIN
				b := DeltaB/2;
				IF Vary_What = XSect THEN SetXPlot
				ELSE ShowTrajectories;
				Running := True;
				SelectMyViewPort(ParamPort);
				ClearViewPort;
				SetColor(White);
				Print(1,1,ConCat('Impact parameter = ',	NumStr(b,7,2)))
			END {Not Quit}
		END; {Not Canceled}
		Done
	END {With DataScreen}
END; {SetImpactRange}

PROCEDURE SetEnergyRange;
VAR
	DataScreen2 : TInputScreen;
BEGIN
	With DataScreen2 DO
	BEGIN
		Init;
		LoadLine ('    Energy Range');
		LoadLine ('');
		LoadLine ('{  6} Energies');
		LoadLine ('First Energy {   1.0}');
		LoadLine ('Last Energy  {  10.0}');
		LoadLine ('#F Change other parameters');
		SetHelpFile('colision.hlp','Energy Range Help');
		LoadLine ('[ OK ]  [Cancel] [Help]');
		SetNumber(1,NumberOfReps);
		SetNumberLimits(1,2,100);
		SetBoolean(4,ChangeOthers);
		DefineInputPort (0.10,0.45,0.33,0.7);
		AcceptScreen;
		IF Canceled THEN Quit := True
		ELSE BEGIN
			ShowPaths := True; ErasePast := False;
			Energy := GetNumber(2);
			ChangeOthers := GetBoolean(4);
         IF ChangeOthers THEN GetInitialConditions;
			NumberOfReps := Trunc(GetNumber(1));
			DeltaB := (GetNumber(3)-Energy)/(NumberOfReps-1);
			ShowTrajectories;
			SelectMyViewPort(ParamPort);
			ClearViewPort;
			SetColor(White);
			Print(1,1,ConCat('Energy = ',	NumStr(Energy,7,2)))
	END; {Not Canceled}
		Done
	END {With DataScreen}
END; {SetEnergyRange}

PROCEDURE SetRangeSpread;
VAR
	DataScreen2 : TInputScreen;
BEGIN
	With DataScreen2 DO
	BEGIN
		Init;
		LoadLine ('Vary Range of Force');
		LoadLine ('');
		LoadLine ('{  6} Ranges');
		LoadLine ('First Range  {      }');
		LoadLine ('Last Range   {      }');
		LoadLine ('#F Change other parameters');
		SetHelpFile('colision.hlp','Energy Range Help');
		LoadLine ('[ OK ]  [Cancel] [Help]');
		IF WhichForce = TruncatedC THEN
		BEGIN
			Loadline ('You will see no effect at');
			LoadLine ('large impact parameters.')
		END;
		SetNumber(2,R00/3.3333);
		SetNumber(3,R00*3);
		SetNumber(1,NumberOfReps);
		SetNumberLimits(1,2,100);
      SetBoolean(4,ChangeOthers);
		IF WhichForce = TruncatedC THEN
			DefineInputPort (0.10,0.45,0.28,0.7)
		ELSE DefineInputPort (0.10,0.45,0.33,0.7);
		AcceptScreen;
		IF Canceled THEN Quit := True
		ELSE BEGIN
			ShowPaths := True; ErasePast := False;
			r0 := GetNumber(2);
			ChangeOthers := GetBoolean(4);
         IF ChangeOthers THEN GetInitialConditions;
			NumberOfReps := Trunc(GetNumber(1));
			DeltaB := (GetNumber(3)-r0)/(NumberOfReps-1);
			ShowTrajectories;
			SelectMyViewPort(ParamPort);
			ClearViewPort;
			SetColor(White);
			Print(1,1,ConCat('R0 = ',	NumStr(r0,7,2)))
	END; {Not Canceled}
		Done
	END {With DataScreen}
END; {SetRangeSpread}

PROCEDURE SetSteepnessSpread;
VAR
	DataScreen2 : TInputScreen;
BEGIN
	With DataScreen2 DO
	BEGIN
		Init;
		LoadLine ('Vary Steepness of Force');
		LoadLine ('');
		LoadLine ('{  6} Steepness');
		LoadLine ('First Steepness  {      }');
		LoadLine ('Last Steepness   {      }');
		LoadLine ('#F Change other parameters');
		SetHelpFile('colision.hlp','Energy Range Help');
		LoadLine ('[ OK ]  [Cancel] [Help]');
		SetNumber(1,NumberOfReps);
		SetNumberLimits(1,2,100);
		SetNumber(2,a0/3.3333);
		SetNumber(3,a0*3);
      SetBoolean(4,ChangeOthers);
		DefineInputPort (0.10,0.45,0.33,0.7);
		AcceptScreen;
		IF Canceled THEN Quit := True
		ELSE BEGIN
			ShowPaths := True; ErasePast := False;
			a := GetNumber(2);
			ChangeOthers := GetBoolean(4);
         IF ChangeOthers THEN GetInitialConditions;
			NumberOfReps := Trunc(GetNumber(1));
			DeltaB := (GetNumber(3)-a)/(NumberOfReps-1);
			ShowTrajectories;
			SelectMyViewPort(ParamPort);
			ClearViewPort;
			SetColor(White);
			Print(1,1,ConCat('a= ',	NumStr(a,7,2)))
	END; {Not Canceled}
	Done
	END; {With DataScreen}
END; {SetSteepnessSpread;}

PROCEDURE SetStrengthSpread;
VAR
	DataScreen2 : TInputScreen;
	UseForce : ForceType;
	kset : Real;
	ShowK : String[2];
BEGIN
	With DataScreen2 DO
	BEGIN
		Init;
		IF Vary_Second THEN BEGIN UseForce := SecondForce; kset := k00 END
			ELSE BEGIN UseForce := WhichForce; kset := k0 END;
			IF UseForce = Yukawa THEN
				ShowK := 'Vo'
				ELSE IF UseForce = SoftSphere THEN
					ShowK := 'k'
					ELSE IF UseForce = WSaxon THEN
						ShowK := 'Vo'
						ELSE
						ShowK := 'k';
		LoadLine ('Force Strength Range');
		LoadLine ('');
		LoadLine (Concat('{  6} Strengths (',ShowK,')'));
		LoadLine (Concat('First ',ShowK,' {      }'));
		LoadLine (Concat('Last  ',ShowK,' {      }'));
		LoadLine ('#F Change other parameters');
		SetHelpFile('colision.hlp','Energy Range Help');
		LoadLine ('[ OK ]  [Cancel] [Help]');
		SetNumber(1,NumberOfReps);
		SetNumberLimits(1,2,100);
		SetNumber(2,kset/3.1623);
		SetNumber(3,kset*3.1623);
		SetBoolean(4,ChangeOthers);
		DefineInputPort (0.10,0.45,0.33,0.7);
		AcceptScreen;
		IF Canceled THEN Quit := True
		ELSE BEGIN
			ShowPaths := True; ErasePast := False;
			ChangeOthers := GetBoolean(4);
			IF ChangeOthers THEN GetInitialConditions;
			NumberOfReps := Trunc(GetNumber(1));
			IF UseForce = Yukawa THEN
				ky := GetNumber(2)
				ELSE IF UseForce = SoftSphere THEN
					ks := GetNumber(2)
					ELSE IF UseForce = WSaxon THEN
						kw := GetNumber(2)
						ELSE
						kc := GetNumber(2);
			DeltaB := (GetNumber(3)-GetNumber(2))/(NumberOfReps-1);
			ShowTrajectories;
			SelectMyViewPort(ParamPort);
			ClearViewPort;
			SetColor(White);
			Print(1,1,ConCat('k = ',	NumStr(GetNumber(2),7,2)))
		END; {Not Canceled}
		Done
	END {With DataScreen}
END; {SetStrengthSpread}

PROCEDURE SetMassRatioRange;
VAR
	DataScreen2 : TInputScreen;
BEGIN
	With DataScreen2 DO
	BEGIN
		Init;
		LoadLine ('    MassRatio Range');
		LoadLine ('');
		LoadLine ('{ 11} Mass Ratios');
		LoadLine ('First MassRatio {   0.1}');
		LoadLine ('Last MassRatio  {  10.0}');
		LoadLine ('#1 Arithmetic Progression');
		LoadLine ('#1 Geometric Progression');
		LoadLine ('#F Change other parameters');
		SetHelpFile('colision.hlp','Mass Ratio Range Help');
		LoadLine ('[ OK ]  [Cancel] [Help]');
		SetNumber(1,NumberOfReps);
		SetNumberLimits(1,2,100);
		SetBoolean(5,Not ArithmeticProg);
      SetBoolean(6,ChangeOthers);
		DefineInputPort (0.10,0.45,0.30,0.73);
		AcceptScreen;
		IF Canceled THEN Quit := True
		ELSE BEGIN
			ShowPaths := True; ErasePast := False;
			M[2] := GetNumber (2)*M[1];
			ChangeOthers := GetBoolean(6);
         IF ChangeOthers THEN GetInitialConditions;
			NumberOfReps := Trunc(GetNumber(1));
			ArithmeticProg := GetBoolean(4);
			IF ArithmeticProg THEN DeltaB := (GetNumber(3)-M[2])/(NumberOfReps-1)
				ELSE DeltaB := Pwr(GetNumber(3)/M[2],1/(NumberOfReps-1));
			ShowTrajectories;
			SelectMyViewPort(ParamPort);
			ClearViewPort;
			SetColor(White);
			Print(1,1,ConCat('Mass Ratio   /   = ',	NumStr(M[2],7,2)));
			SetColor(LightBlue);
			Print (12,1,'M2');
			SetColor(Yellow);
			Print (15,1,'M1')
		END; {Not Canceled}
		Done
	END {With DataScreen}
END; {SetMassRatioRange}

PROCEDURE SetForceRange;
BEGIN
	Varying :=True; {Needed specially here to inform next call}
	GetInitialConditions;
	WhichForce := Coulomb;
	ForceName := 'Coulomb';
	SetAccel;
	Power := -2;
	SaveNReps := NumberOfReps;
	NumberOfReps := 8;
	ShowTrajectories;
	ErasePast := False;
	ShowPaths := True;
	DisplayPotential(True);
	Message('Cycling through all Force types.')
END;

PROCEDURE Single;
BEGIN
	IF PlotXSection THEN ShowTrajectories;
	GetInitialConditions
END;

PROCEDURE Choose_Second;
VAR DataScreen2 : TInputScreen;
	 it : Integer;
BEGIN
	WITH DataScreen2 DO
	BEGIN
		Init;
		LoadLine ('Vary Second Force Parameters');
		Loadline ('');
		IF SecondForce IN [HardSphere,SoftSphere,Yukawa,WSaxon]	THEN
			LoadLine ('#1 Vary Second Force Range');
		IF SecondForce in [SoftSphere,Yukawa,WSaxon] THEN
			LoadLine ('#1 Vary Second Force Strength');
		IF SecondForce = WSaxon THEN
			LoadLine ('#1 Vary Second Force Steepness');
		LoadLine ('');
		SetHelpFile('colision.hlp','Collision Type Help');
		LoadLine ('[ OK ]  [Cancel] [Help]');
		DefineInputPort (0.08,0.47,0.45,0.77);
		AcceptScreen;
		IF Canceled THEN
      BEGIN
			  Varying := False;
           Running := False;
           Quit := True
      END
		ELSE
		BEGIN
			CASE GetRadioButton('1') OF
				1 : BEGIN Vary_What := Range; SetRangeSpread END;
				2 : BEGIN Vary_What := Strength; SetStrengthSpread END;
				3 : BEGIN Vary_What := Steepness; SetSteepnessSpread END
			END
		END; {Not Canceled}
		Done
	END {With DataScreen2}
END; {Choose_Second;}

PROCEDURE WhatRun;
VAR
	i : Integer;
BEGIN
	SaveImpactPar := b;
	SaveEnergy := Energy;
	SaveR0 := r0;
	Savea := a;
	Savekc := kc;
	Savekw := kw;
	Saveks := ks;
	Saveky := ky;
	SaveM := m[2];
	WITH DataScreen DO
	BEGIN
		Init;
		LoadLine ('    Collision Type');
		LoadLine ('#1 Single Collision');
		LoadLine ('#1 Vary Impact Parameter');
		LoadLine ('#1 Plot Cross Section');
		LoadLine ('#1 Vary Energy');
		LoadLine ('#1 Vary Force Type');
		LoadLine ('#1 Vary Mass Ratio');
		IF WhichForce IN [Coulomb,FPower,SoftSphere,ShieldedC,TruncatedC,
			Yukawa,WSaxon] THEN LoadLine ('#1 Vary Force Strength');
		IF WhichForce IN [HardSphere,SoftSphere,ShieldedC,TruncatedC,
			Yukawa,WSaxon] THEN LoadLine ('#1 Vary Force Range');
		IF WhichForce = WSaxon THEN LoadLine ('#1 Vary Force Steepness');
		IF TwoForces THEN LoadLine ('#1 Vary 2nd Force Parameters');
		LoadLine ('');
		SetHelpFile('colision.hlp','Collision Type Help');
		LoadLine ('[ OK ]  [Cancel] [Help]');
		IF (WhichForce <> WSaxon) AND (Ord(Vary_What) = Ord(Steepness)) THEN
			Vary_What := Pred(Vary_What);
		IF (WhichForce in [Coulomb,HardSPhere])
			AND (Ord(Vary_What) = Ord(Range)) THEN
			Vary_What := Pred(Vary_What);
		SetRadioButton ('1',Ord(Vary_What)+1);
		IF TwoForces and Vary_Second THEN SetRadioButton ('1',8);
		DefineInputPort (0.08,0.47,0.25,0.77);
		AcceptScreen;
		IF Canceled THEN Quit := True
		ELSE BEGIN
			Vary_What := None; Vary_Second := False;
			FOR i := 1 to GetRadioButton('1')-1 DO Inc(Vary_What);
			IF (WhichForce = HardSphere) AND (Vary_What = Strength) THEN
				Inc(Vary_What);
			Varying := Vary_What <> None;
			IF TwoForces and (Vary_What = Range) THEN Vary_Second := True
			ELSE
			Case Vary_What OF
				None      : Single;
				ImpactP   : SetImpactRange;
				XSect     : SetImpactRange;
				VEnergy   : SetEnergyRange;
				FType     : SetForceRange;
				MassRatio : SetMassRatioRange;
				Strength  : SetStrengthSpread;
				Range     : SetRangeSpread;
				Steepness : SetSteepnessSpread;
			END; {Case}
		END; {Not Canceled}
		IF Varying THEN
		BEGIN
			SelectMyViewPort(ParamPort);
			SetColor(White);
			Print(1,2,Concat('Force: ', ForceName))
		END;
		Done;
		IF Quit THEN
		BEGIN Vary_What := None; Varying := False;
			Running := False; Vary_Second := False
		END;
		IF Vary_Second THEN Choose_Second
	END; {With DataScreen}
END; {WhatRun}

VAR
	ShowIt : Array[1..MaxXSects] of Boolean;

PROCEDURE PrintLegend(Logs : Boolean);
  {Adapted from the contour legend in CupsGrph}
  VAR
	  x,y,n,nd,Ht,NumLen, Skipped : integer;
	  increment, Low : Real;
	  strng : string;
	  Viewport : ViewPortType;
  BEGIN
	  SetTextJustify(LeftText,TopText);
	  GetViewSettings(viewPort);
	  NumLen := 9 * TextWidth('8');
	  Ht := TextHeight('8');
	  Low := 0.0;
	  WITH ViewPort DO BEGIN x:=x2-x1-(7*NumLen) DIV 2; y:=y1+Ht  END;
	  SetColor(white);
	  IF Logs THEN OutTextXY (x-32,y-15, 'Log');
	  OutTextXy (x,y-15, 'Cross Sections:');
	  Increment := 0.2;
	  nd := 0; Skipped := 0;
	  FOR n := 1 TO XSaved DO
          IF ShowIt[n] THEN
	       BEGIN
             IF n > 6 THEN nd := -13;
		       SetColor(rainbow[0].colors[n+9+nd]);
		       line(x-14,y+(n-skipped)*10,x-1,y+(n-skipped)*10);
	       END
          ELSE Skipped := Skipped+1;
	  SetColor(white);
	  rectangle (x-15,y+(XSaved-Skipped+1)*10,x,y);
     Skipped := 0;
	  FOR n := 1 TO XSaved DO
        IF ShowIt[n] THEN
			  OutTextXY(x+5,y+(n-skipped)*10-Ht DIV 2,XDescription[n])
        ELSE Skipped := Skipped+1
  END; {PrintLegend}

PROCEDURE ShowSavedXSections;
VAR
	j, k, nd, SavePlots : Integer;
   QuitAll, QuitThis, ShowLegend : Boolean;
   SavLab, SaveCM, SaveLabV, SaveCMV, SaveV : Boolean;
	TempX, TempY : DVector;
	Screen : TInputScreen;
BEGIN   {Menu item inactive if no cross sections saved}
	ShowLegend := True;
	SavLab := PlotLab;
	SaveCM := PlotCofM;
	SaveLabV := PlotLabVectors;
   SaveV := Vectors;
	SaveCMV := PlotCofMVectors;
	SavePlots := Plots;
   QuitAll := False;
   ShowCursor; {Hidden in Menu}
	Logs := False;
	FOR j := 1 To XSaved DO
		ShowIt[j] := True;
   REPEAT {Until QuitAll}
		With Screen DO
		BEGIN
			Init;
			LoadLine ('Cross Section Display Choices');
			LoadLine ('');
			LoadLine ('Display Which Cross Sections:');
			FOR j := 1 To XSaved DO
			BEGIN
				LoadLine (Concat('#T ',NumStr(j,2,0),': ',XDescription[j]));
            SetBoolean(j, ShowIt[j])
         END;
			LoadLine ('#1 Show Values  #1 Show Logarithms');
         IF Logs THEN SetRadioButton('1',2);
			LoadLine ('');
			SetHelpFile('colision.hlp','XSext Display Help');
			LoadLine ('[ OK ]    [Cancel]   [Help]');
			DefineInputPort(0.10,0.55,0.58-0.042*XSaved,0.85);
			AcceptScreen;
			IF Canceled THEN QuitAll := True
			ELSE
			BEGIN
				IF GetRadioButton('1') = 2 THEN Logs := True;
				FOR j := 1 To XSaved DO
					ShowIt[j] := GetBoolean(j)
			END; {Not Canceled}
			Done
		END; {With Screen}
		If Not QuitAll THEN
      BEGIN
		SetAllFalse;
		Plots := 1;
		PlotXSection := True;
      QuitThis := False;
      REPEAT {Until QuitThis}
		HideCursor;
		SetPlots;
		ClearPlots(True);
		FOR j := 1 to XSaved DO IF Showit[j] THEN
		 IF (SavProb[j].Length < 2) OR (SavProb[j].Value(2) <= 0.0) THEN
			MildAnnounce ('Two few values in data set '+Numstr(j,2,0)+': '+
			XDescription[j])
		 ELSE
		 BEGIN
			TempX.Init(SavProb[j].Length); TempY.Init(SavProb[j].Length);
			IF Logs THEN
			BEGIN
				DefineScale(LnSNum,0,180,XScalMinLog,XScalMaxLog);
				SelectScale(LnSNum);
				FOR k := 2 To TempY.Length DO
					TempY.Put(k,Ln(SavProb[j].Value(k)))
			END {If Logs}
			ELSE
			BEGIN
            DefineScale(XSectSNum,0,180,0.0,1.0);
				SelectScale(XSectSNum);
				TempY.Equate(1/SavProb[j].Value(1),SavProb[j])
			END; {Not Logs}
			TempX.Equate(180/Pi,SavAngle[j]);
			nd := 0;
			IF j > 6 THEN nd := -13;
			  SetColor(rainbow[0].colors[j+9+nd]);
			PlotDVectors(TempX, TempY, 2, TempX.Length);
			TempX.Free; TempY.Free
		END; {For j DO IF}
		IF ShowLegend THEN PrintLegend(Logs);
		ShowCursor;
		With Screen DO
		BEGIN
			Init;
			Loadline ('     Change Plots');
			Loadline ('');
			LoadLine ('#1 Show Values');
         Loadline ('#1 Show Logarithms');
			LoadLine ('#F New Choice of Plots');
         Loadline ('#F Quit Plotting');
         Loadline ('#T Show Legend');
			SetHelpFile('colision.hlp','New XSect Help');
			LoadLine ('[ OK ]  [Cancel] [Help]');
         Loadline ('');
			Loadline ('#F Discard All XSects');
			Loadline ('      & quit Plotting');
			Loadline ('#F Discard Unselected ');
			Loadline ('      Cross Sections');
			If Logs THEN SetRadioButton('1',2);
			SetBoolean(5, ShowLegend);
			DefineInputPort(0.68,1.00,0.30,0.89);
			AcceptScreen;
			IF Canceled THEN BEGIN QuitThis := True; QuitAll := True END
			ELSE
			BEGIN
				IF GetRadioButton('1') = 1 THEN Logs := False;
				IF GetRadioButton('1') = 2 THEN Logs := True;
            IF GetBoolean(3) THEN QuitThis := True;
            IF GetBoolean(4) THEN
            BEGIN
               QuitThis := True;
               QuitAll := True
            END;
				IF GetBoolean(5) THEN ShowLegend := True
               ELSE ShowLegend := False;
				IF GetBoolean(9) THEN
            BEGIN
					XSaved := 0;
					XScalMaxLog := -1E6; XScalMinLog := 1E6;
					XScalMax := -1E6; XScalMin := 1E6;
					MainMenu.RowActivate(2,8,False);
					MainMenu.RowActivate(1,6,False);
               ShowCursor;
               QuitThis := True;
               QuitAll := True
				END;
				IF GetBoolean(10) THEN
				BEGIN
					For j := 1 to XSaved DO
						IF Not ShowIt[j] THEN
						BEGIN
							For k := j + 1 to XSaved DO
							BEGIN
								SavProb[k-1] := SavProb[k];
								SavAngle[k-1] := SavAngle[k];
								XDescription[k-1] := XDescription[k];
								SavForceT[k-1] := SavForceT[k];
								ShowIt[k-1] := ShowIt[k]
							END;
							XSaved := XSaved - 1
						END
				END;
			END; {Not Canceled}
			Done
		END; {With Screen}
      UNTIL QuitThis
      END {If Not QuitAll}
	UNTIL QuitAll;
	PlotLab := SavLab;
	PlotCofM := SaveCM;
	PlotLabVectors := SaveLabV;
	Vectors := SaveV;
	PlotCofMVectors := SaveCMV;
	Plots := SavePlots;
   PlotXSection := False;
	IF (Plots = 2) THEN
	BEGIN
		PlotCofM := PlotLab;
		PlotCofMVectors := PlotLabVectors
	END;
	SetPlots;
   ClearPlots(True)
END; {ShowSavedXSections}

FUNCTION GetFName(Extension : String) : String;
VAR
	DirInfo : SearchRec;

BEGIN
	ShowCursor;
	FindFirst ('*'+Extension, AnyFile, DirInfo);
	IF DosError = 18 THEN
	BEGIN
		MildAnnounce
		  ('There are No files of the type requested in this directory.');
		GetFName := ''
	END
	ELSE
		GetFName := OpenFile('*'+Extension);
END; {GetFName}

FUNCTION PutFName(Extension : String) : String;
VAR
	FName, Blanks : String[12];
	Action : Integer;
	DirInfo : SearchRec;
	Exists : Boolean;
	Describe : TInputScreen;

BEGIN
	Exists := False;
   ShowCursor;
	Action := 0;
	DosError := 100; {Signals no file has been sought}
	FName := '';
	REPEAT {Until File is new or overwrite OK}
		With Describe DO
		REPEAT {Until filename is not blank}
			Init;
			LoadLine ('         Get a File Name');
			LoadLine ('');
			LoadLine ('');
			LoadLine ('');
			LoadLine ('The name of the file to write is ');
			LoadLine ('');
			LoadLine ('        "        "'+Extension);
			SetString (1,FName);
			IF DosError = 0 THEN {File Exists}
			BEGIN
				LoadLine ('There is already a file by that name;');
				IF Extension = '.par' THEN
				BEGIN
					LoadLine ('#1 Add to it');
					LoadLine ('#1 Change name (please do it NOW)');
					LoadLine ('#1 Don''t write a file after all');
					SetRadioButton('1',2)
				END
				ELSE
				BEGIN
					LoadLine ('#1 Write over it');
					LoadLine ('#1 Change name (please do it NOW)');
					LoadLine ('#1 Don''t write a file after all');
					SetRadioButton('1',2)
				END
			END; {File Exists}
			LoadLine ('');
			LoadLine ('       [ OK ]    [Cancel]');
			DefineInputPort(0.09, 0.62, 0.35, 0.92);
			Message('Type a file name and press <ENTER>');
			AcceptScreen;
         IF Canceled THEN Action := 3
         ELSE
         BEGIN
         	FName := GetString(1);
			   If DosError = 0 THEN Action := GetRadioButton('1');
            IF Exists THEN
            If Pos('.',FName) > 0 THEN Delete (FName, Pos('.',FName), 12);
			   While Pos(' ',FName) > 0 DO Delete (FName, Pos(' ',FName), 1);
         END;
			Describe.Done;
		UNTIL (Length (FName) > 0) OR (Action = 3);
		FName := FName + Extension;
		FindFirst (FName, AnyFile, DirInfo);
		IF (DosError = 0) AND (Action in [0,2]) THEN
			BEGIN
			Exists := True;
			Delete (FName, Pos('.',FName), 12);
			END; {If DosError}
	UNTIL (DosError = 18) OR (Action in [1,3]);
	IF  (DosError = 0) AND (Action = 1) AND (Extension = '.par') THEN
	BEGIN
		Delete (FName, Pos('.',FName)+1, 12);
		FName := FName + 'ex' {tells caller file exists, other caller don't care}
	END; {File Exists}
	IF (Action = 3) THEN
		FName := 'ABORT';
	MainMenu.Display;
	PutFName := FName
END; {PutFName}

PROCEDURE SaveXSection(Fname:String);
VAR
	Screen : TInputScreen;
	XSectFile : Text;
	i, j : integer;
BEGIN
	IF (FName = 'ABORT') OR (Copy(Fname, Pos('.',FName)+1,2) = 'ex') THEN
	BEGIN
		Message ('File not written');
		StaticPause;
		MainMenu.Display;
		Exit
	END;
	Assign(XSectFile, FName);
	ReWrite(XSectFile);
	With Screen DO
	BEGIN
		Init;
		LoadLine (ConCat('        Creating Cross Section File ',FName));
		LoadLine (' ');
		LoadLine ('    Please type a description of the whole set:');
		LoadLine ('"                                                        "');
		LoadLine (' ');
		LoadLine ('       [ OK ]    [Cancel]');
		DefineInputPort(0.10, 0.85, 0.58, 0.85);
		AcceptScreen;
		If Not Canceled THEN
      BEGIN
			Writeln (XSectFile,'Differential Cross Section File');
			Writeln (XSectFile, GetString(1))
      END
		ELSE BEGIN Done; Exit END;
		Done
	END;
	Writeln (XSectFile,XSaved,' Number of Cross Sections');
	Writeln (XSectFile, XScalMinLog:14,' ',XScalMaxLog:14,' Log Scale Min & Max');
	FOR i := 1 to XSaved Do
	BEGIN
	   Writeln (XSectFile,Ord(SavForceT[i]), ' Force is ',
				  ForceNames[Ord(SavForceT[i])+1]);
		Writeln (XSectFile, XDescription[i]);
		Writeln (XSectFile, SavProb[i].Length,' number of angles');
		Writeln (XSectFile, '     Angle,        Probability');
		FOR j := 1 to SavProb[i].Length DO
			Writeln (XSectFile, SavAngle[i].Value(j):15,'  ',
						SavProb[i].Value(j):14)
	END;
   HideCursor;
	Close(XSectFile);
END; {SaveXSection}

PROCEDURE ReadXSection(FName:String);
VAR
	XSectFile : Text;
	Description : String;
	i, j, f : Integer;
	Angle, Probability : Real;
BEGIN
	IF FName = '' THEN Exit;
	Assign(XSectFile, FName);
	ReSet(XSectFile);
	IF Eof(XSectFile) THEN
	BEGIN
		Message('File is empty. Click or ENTER.');
		StaticPause;
   	MainMenu.Display;
      Exit
	END
	ELSE
	BEGIN
		Readln (XSectFile); {Discard Header}
		Readln (XSectFile, Description);
		Message('Cross Section file Description: '+Description);
		Readln (XSectFile, XSaved); {Number of Cross Sections}
		Readln (XSectFile, XScalMinLog, XScalMaxLog);
		FOR i := 1 to XSaved DO
		BEGIN
			Readln (XSectFile, f); {Ordinal number of Force}
         SavForceT[i] := Coulomb; {First Force Type}
         FOR j := 1 to f DO
             SavForceT[i] := Succ(SavForceT[i]);
			Readln (XSectFile, XDescription[i]);
			Readln (XSectFile, f); {Number of datum points}
			SavProb[i].ReSize(f);
			SavAngle[i].ReSize(f);
			Readln (XSectFile); {Discard Header}
			FOR j := 1 to f DO
			BEGIN
				Readln (XSectFile, Angle, Probability);
				SavAngle[i].Put(j,Angle);
				SavProb[i].Put(j,Probability)
			END
		END {For i to XSaved}
	END; {File Exists}
	Close(XSectFile);
	MainMenu.RowActivate(2,8,True);
	ShowSavedXSections;
	HideCursor;
	MainMenu.Display
END; {ReadXSection}

PROCEDURE SaveHistory(FName:String);
VAR
	Screen : TInputScreen;
	i, j : integer;
BEGIN
	IF (FName = 'ABORT') OR (Copy(Fname, Pos('.',FName)+1,2) = 'ex') THEN
	BEGIN
		Message ('File not written');
		StaticPause;
		MainMenu.Display;
		Exit
	END;
	With Screen DO
	BEGIN
		Init;
		LoadLine (ConCat('        Creating History File ',FName));
		LoadLine (' ');
		LoadLine ('This will create a history of the most recent collision');
		LoadLine (' ');
		LoadLine ('    Please type a description of the collision:');
		LoadLine ('"                                                        "');
		LoadLine (' ');
		LoadLine ('       [ OK ]    [Cancel]');
		DefineInputPort(0.10, 0.85, 0.38, 0.85);
		AcceptScreen;
		If Not Canceled THEN
		BEGIN
			Assign(HistFile, FName);
			ReWrite(HistFile);
			Writeln (HistFile,'Collision History File');
			Writeln (HistFile, GetString(1));
		END
		ELSE BEGIN
			Done;
			Message ('File not written');
			StaticPause;
			MainMenu.Display;
			Exit
		END;
		Done
	END;
	Writeln (Histfile, Ord(WhichForce),'  ',ForceNames[Ord(WhichForce)+1],
				' force');
	IF TwoForces THEN
	BEGIN
		Writeln(Histfile,'True');
		Writeln(Histfile, Ord(SecondForce),'  ',ForceNames[Ord(SecondForce)+1],
				' second force');
		Writeln(Histfile, ForceName)
	END
	ELSE Writeln(Histfile,'False: No second Force');
	Writeln (Histfile, kc:15,'  Coulomb force Constant');
	Writeln (Histfile, ks:15,'  Soft sphere force constant');
	Writeln (Histfile, kw:15,'  Woods-Saxon Vo');
	Writeln (Histfile, ky:15,'  Yukawa Vo');
	Writeln (Histfile, a:15,'  Woods-Saxon a');
	Writeln (Histfile, R0:15,'  range or radius parameter for several forces');
	Writeln (HistFile, M[1]:15, M[2]:15, '  Object Masses');
	Writeln (HistFIle, AngleHistory:15, '  Scattering Angle');
	Writeln (Histfile, HistoryCount-1,'  Number of time steps');
	For i := 1 to HistoryCount-1 DO {HistoryCount is 1 more than steps taken}
	BEGIN
		Writeln(Histfile, tHistory[I]:15,' time: x, y,            Vx,',
				  '           Vy: for Bodies 1 & 2');
		FOR j := 1 to 2 DO
			Writeln(Histfile, xHistory[I,j]:15,' ', yHistory[I,j]:15,' ',
									VxHistory[I,j]:15,' ', VyHistory[I,j]:15);
	END;
	Close(HistFile)
END; {SaveHistory}

PROCEDURE ResetScales(Clear : Boolean) ; FORWARD;

PROCEDURE ChangeSystem(Which : ForceType; NewConditions : Boolean);
Var i : Integer;
BEGIN
	ResetScales({Clear:} False);
	HistoryCount := 0;
	MainMenu.RowActivate(1,9,False);
	TwoForces := False;
	For i := 1 to NumForces Do
	MainMenu.RowChecked(3,i, False);
	OldForce := Which;
	Case Which OF
		Coulomb : CoulombParameters(NewConditions);
		FPower : PowerParameters;
		ShieldedC : ShieldedParameters(NewConditions);
		TruncatedC : TruncatedParameters(NewConditions);
		HardSphere : HardSphereParameters;
		SoftSphere : SoftSphereParameters;
		WSaxon : Woods_Saxon_Parameters;
		Yukawa : Yukawa_Parameters;
		User : User_Parameters(NewConditions)
	END; {Case}
	MainMenu.RowChecked(3,Ord(WhichForce)+1, True);
	IF NewConditions THEN
	BEGIN
		GetInitialConditions;
		PreviousForce({SwapForces:} False);
		ClearPlots(True)
	END
END; {ChangeSystem}

PROCEDURE ReplayHistory;
VAR
	Count,i,j,xt,yt,xh,yh, xh2,yh2 : Integer;
	UseColor : Array[1..2] of Byte;
	Px1, Py1, Px2, Py2, Pmx1, Pmy1, Pmx2, Pmy2 : Real;
	XCom0, YCom0 : Real;
BEGIN
	IF HistoryCount < 2 THEN Exit;
	UseColor[1] := Yellow; UseColor[2] := LightCyan;
	IF PlotLab THEN
	BEGIN
		SelectMyViewPort(LabPort);
		SelectScale(XYSNum);
		Count := 0;
		For i := 2 to HistoryCount-1 DO
			FOR j := 1 to 2 DO
         BEGIN
				SetColor(UseColor[j]);
				IF ShowPaths THEN
				PlotLine( xHistory[i-1,j], yHistory[i-1,j],
							 xHistory[i,j], yHistory[i,j])
				ELSE IF (Count MOD DotInterval) = 0 THEN
					DrawCircle (xHistory[i,j], yHistory[i,j],3,UseColor[j]);
			END;
         Count := Count + 1
	END;
	IF PlotCofM THEN
	BEGIN
		SelectMyViewPort(CofMPort);
		SelectScale(XYSNum);
		XCom0 := (M[1]*xHistory[1,1] + M[2]*xHistory[1,2])/TotalM;
		YCom0 := (M[1]*yHistory[1,1] + M[2]*yHistory[1,2])/TotalM;
		Count := 0;
		For i := 2 to HistoryCount-1 DO
      BEGIN
			XCofM := (M[1]*xHistory[i,1] + M[2]*xHistory[i,2])/TotalM;
			YCofM := (M[1]*yHistory[i,1] + M[2]*yHistory[i,2])/TotalM;
			FOR j := 1 to 2 DO
         BEGIN
				SetColor(UseColor[j]);
				IF ShowPaths THEN
					PlotLine( xHistory[i-1,j]-XCom0, yHistory[i-1,j]-YCom0,
							 xHistory[i,j]-XCofM, yHistory[i,j]-YCofM)
				ELSE IF (Count MOD DotInterval) = 0 THEN
					DrawCircle (xHistory[i,j]-XCofM,
									yHistory[i,j]-YCofM,3,UseColor[j]);
      	END;
			XCom0 := XCofM;
			YCom0 := YCofM;
			Count := Count + 1
		END
	END; {PlotCofM}
   IF Vectors THEN
	BEGIN
		VCofM := VxHistory[1,1]*m[1]/(M[1]+M[2]);
		FOR i := 1 to HistoryCount-1 DO
		BEGIN
			Px1:= VxHistory[i,1]*M[1]; Py1 := VyHistory[i,1]*M[1];
			Px2 := VxHistory[i,1]*M[1]+VxHistory[i,2]*M[2];
			Py2 := VyHistory[i,1]*M[1]+VyHistory[i,2]*M[2];
			Pmx1 := (VxHistory[i,1]-VCofm)*M[1]; Pmy1 := VyHistory[i,1]*M[1];
			Pmx2 := (VxHistory[i,2]-VCofM)*M[2]; Pmy2 := VyHistory[i,2]*M[2];
			IF PlotLabVectors THEN
			BEGIN
				SelectMyViewPort(LabVectorPort);
				SelectScale(PSNum);
				SetColor(UseColor[1]);
				Map (0.0,0.0, xt,yt);
				Map (Px1,Py1, xh, yh);
				Vector (xt,yt, xh,yh);
				SetColor(UseColor[2]);
				Map (Px2,Py2, xh2,yh2);
				Vector(xh,yh, xh2,yh2)
			END; {PlotLabVectors}
			IF PlotCofMVectors THEN
			BEGIN
				SelectMyViewPort(CofMVectorPort);
				SelectScale(PSNum);
				SetColor(UseColor[1]);
				Map (0.0,0.0, xt,yt);
				Map (Pmx1,Pmy1, xh,yh);
				Vector(xt,yt,xh,yh);
				SetColor(UseColor[2]);
				Map (Pmx2,Pmy2, xh,yh);
				Vector(xt,yt,xh,yh)
			END {PlotCofMVectors}
		END {For i = 1 to HistoryCount DO}
	END;
	SelectMyViewPort(AnglePort);
	ClearViewPort;
	SetColor(White);
	IF AngleHistory > Pi THEN AngleHistory := AngleHistory - 2*Pi;
	Print(1,1,ConCat('Scattering Angle = ',
			NumStr(AngleHistory*180/Pi,7,2)));
	SelectMyViewPort(InitDistPort1);
	ClearViewPort;
END; {ReplayHistory}

CONST
	ClearRead : Boolean = True;

PROCEDURE ReadHistory(FName : String);
VAR Description : String;
	 TrueTrue : String[4];
	 GetForceName : String[20];
	 FirstForce : ForceType;
	 i,j : Integer;
BEGIN
	IF FName = '' THEN Exit;
	Assign(HistFile, FName);
	ReSet(HistFile);
	IF Eof(HistFile) THEN
	BEGIN
		Message('File is empty. Click or ENTER.');
		StaticPause;
   	MainMenu.Display;
      Exit
	END;
	Readln (HistFile,Description); {Header Line}
	Readln (HistFile, Description); {Description is here}
	Readln (Histfile, j);
	WhichForce := Coulomb;
	FOR I := 1 to j DO WhichForce := Succ(WhichForce);
	Readln (Histfile, truetrue);
	IF TrueTrue = 'True' THEN
	BEGIN
		Readln (Histfile, j);
		SecondForce := Coulomb;
		FirstForce := WhichForce;
		FOR I := 1 to j DO SecondForce := Succ(SecondForce);
		ChangeSystem(SecondForce, {ChangeConditions=} False);
		WhichForce := FirstForce;
		SecondAccel := TheAccel;
		SecondPotential := ThePotential;
		Readln (Histfile, GetForcename);
	END ELSE TwoForces := False;
	ChangeSystem(WhichForce, {ChangeConditions=} False);
	IF TrueTrue = 'True' THEN
	BEGIN
		TwoForces := True;
		ForceName := GetForceName
	END;
	Readln (Histfile, kc);
	Readln (Histfile, ks);
	Readln (Histfile, kw);
	Readln (Histfile, ky);
	Readln (Histfile, a);
	Readln (Histfile, R0);
	Readln (HistFile, M[1], M[2]);
	IF ClearRead THEN  ClearPlots(True); {Changesystem(False) doesn't clear}
	Readln (HistFIle, AngleHistory);
	Readln (Histfile, HistoryCount);
	For i := 1 to HistoryCount DO
	BEGIN
		Readln(Histfile, tHistory[I]);
		FOR j := 1 to 2 DO
		BEGIN
			Readln(HistFile, xHistory[I,j], yHistory[I,j],
									VxHistory[I,j], VyHistory[I,j]);
      END
	END;
	Close(HistFile);
	Message ('History File Read: '+Description);
	ReplayHistory;
	StaticPause;
   MainMenu.Display
END; {ReadHistory}

PROCEDURE ReadHistOptions;
BEGIN
	WITH DataScreen DO
	BEGIN
		Init;
		Loadline ('   History Read Options');
		Loadline ('');
		LoadLine ('#1 Show Object History ');
		LoadLine ('#1 Show Paths Only');
		Loadline ('#T Don''t Clear Screen');
		SetBoolean(2,ShowPaths);
		SetBoolean(3,Not ClearRead);
		LoadLine ('');
		SetHelpFile('Colision.hlp','ReadHistOptionsHelp');
		LoadLine ('[ OK ]  [Cancel] [Help]');
		DefineInputPort (0.10,0.45,0.55,0.87);
		ShowCursor;
		AcceptScreen;
		HideCursor;
		IF Not Canceled THEN
		BEGIN
			ShowPaths := GetBoolean(2);
			ClearRead := Not GetBoolean(3);
		END; {not Canceled}
		Done;
	END {With DataScreen}
END; {ReadHistOptions}

CONST ZoomFactor : Real = 3.0;

PROCEDURE ZoomScreen(VAR Quit : Boolean);
BEGIN
	With DataScreen DO
	BEGIN
		Init;
		Loadline ('      Zoom Choices');
		Loadline ('');
		Loadline ('Zoom Factor:    {      }');
		Loadline ('Time Step:      {      }');
		Loadline ('Image Interval: {      }');
		Loadline ('');
		Loadline ('[ OK ]  [Cancel] [Help]');
		DefineInputPort(0.06,0.4,0.35,0.65);
		SetNumber(1,ZoomFactor);
		SetNumber(2,TimeStep);
		SetNumber(3,DotInterval);
		SetNumberLimits(1,1.0,1000.0);
		SetNumberLimits(3,1.0,1000.0);
		SetHelpFile('colision.hlp','ZoomHelp');
		AcceptScreen;
		If Canceled THEN Quit := True
		ELSE
		BEGIN
			ZoomFactor := GetNumber(1);
			TimeStep := GetNumber(2);
			DotInterval := Trunc(GetNumber(3))
		END; {Not Canceled}
		Done
	END {With DataScreen}
END; {ZoomScreen}

Procedure ZoomIn;
VAR Quit : Boolean;
BEGIN
	If Not Zoomed THEN
	BEGIN
		XScaleDef := XScale;
		YScaleDef := YScale;
		PxScaleDef := PxScale;
		PyScaleDef := PyScale;
		DotIntDef := DotInterval;
		TimeStepDef := TimeStep
	END;
	Quit := False;
	ZoomScreen(Quit);
	IF Not Quit THEN
	BEGIN
		XScale := XScale/ZoomFactor;
		YScale := YScale/ZoomFactor;
		Zoomed := True;
		MainMenu.RowActivate(2,7,True);
		SetPlots;
		ClearPlots({NotTutorial:} True);
		ReplayHistory
	END
END; {ZoomIn}

PROCEDURE ZoomOut;
VAR Quit : Boolean;
BEGIN
	If Not Zoomed THEN
	BEGIN
		XScaleDef := XScale;
		YScaleDef := YScale;
		PxScaleDef := PxScale;
		PyScaleDef := PyScale;
		DotIntDef := DotInterval;
		TimeStepDef := TimeStep
	END;
	Quit := False;
	ZoomScreen(Quit);
	IF Not Quit THEN
	BEGIN
		XScale := XScale*ZoomFactor;
		YScale := YScale*ZoomFactor;
		Zoomed := True;
		MainMenu.RowActivate(2,7,True);
		SetPlots;
		ClearPlots({NotTutorial:} True);
		ReplayHistory
	END
END; {ZoomOut}

PROCEDURE ResetScales(Clear : Boolean);
BEGIN
	XScale := XScaleDef;
	YScale := YScaleDef;
	PxScale := PxScaleDef;
	PyScale := PyScaleDef;
	DotInterval := DotIntDef;
	TimeStep := TimeStepDef;
	Zoomed := False;
	ZoomFactor := 3;
	MainMenu.RowActivate(2,7,False);
	IF Clear THEN
	BEGIN
		SetPlots;
		ClearPlots({NotTutorial:} True);
		ReplayHistory
	END {IF Clear}
END; {ResetScales}

VAR
	kcp, ksp, kwp, kyp, ap, R0p : Array[0..1] of Real;
	WhichForcep : Array[0..1] of ForceType;
	InPtr, OutPtr : Integer;

PROCEDURE PreviousForce(SwapForces : Boolean);
VAR Savb : Real;
BEGIN
	IF Not SwapForces THEN
	BEGIN
		InPtr := OutPtr;
		OutPtr := (OutPtr + 1) MOD 2;
		EnoughForces := EnoughForces +  1
	END
	ELSE IF EnoughForces < 2 THEN
	BEGIN
		MIldAnnounce('There is no previous force.');
		Exit
	END;
	WhichForcep[InPtr] := WhichForce;
	kcp[InPtr] := kc;
	ksp[InPtr] := ks;
	kwp[InPtr] := kw;
	kyp[InPtr] := ky;
	ap[InPtr]  :=  a;
	R0p[InPtr] := R0;

	IF SwapForces THEN {And change to the saved force}
	BEGIN
		Savb := b;
		WhichForce := WhichForcep[OutPtr];
		ChangeSystem(WhichForce, {ChangeConditions=} False);
		kc := kcp[OutPtr];
		ks := ksp[OutPtr];
		kw := kwp[OutPtr];
		ky := kyp[OutPtr];
		a  := ap[OutPtr];
		R0 := R0p[OutPtr];
		InPtr := OutPtr;
		OutPtr := (OutPtr + 1) MOD 2;
		SelectMyViewPort(ClearNamePort);
		ClearViewPort;
		SetColor(White);
		SelectMyViewPort(NamePort);
		IF Plots = 1 THEN PutLabel(Right,ForceName)
			ELSE PutLabel(Top,ForceName);
		b := Savb;
		DisplayPotential(True)
	END
END; {PreviousForce}

PROCEDURE HandleMenu;

	PROCEDURE MarkCofMass;
	BEGIN
		ShowCofM := Not ShowCofM;
		IF ShowCofM THEN MainMenu.row(3,6, 'Don''t Mark C of M')
		ELSE MainMenu.row(3,6, 'Mark Cent of Mass')
	END; {MarkCofMass}

{------------- Main Menu routine is mostly procedure calls -----------------}

BEGIN {HandleMenu}
	WITH MainMenu DO
	CASE colChosen OF
	 1 : Case rowChosen of
			1 : Help(OpenScreen);
			2 : AboutCups;
			3 : Configuration;
			5 : ReadXSection(GetFName('.xsc'));
			6 : SaveXSection(PutFname('.xsc'));
			7 : ReadHistory(GetFName('.chf'));
			8 : ReadHistOptions;
			9 : SaveHistory(PutFName('.chf'));
			10: BEGIN MainMenu.Done; CupsDone; Halt END
		  END; {case rowChosen in Column 1}
	 2 : CASE rowChosen OF
			1 : BEGIN ShowOne; ReplayHistory; END;
			2 : BEGIN HideCursor; ShowTrajectories; ReplayHistory;ShowCursor END;
			3 : BEGIN HideCursor; ShowVectors; ReplayHistory; ShowCursor END;
			4 : BEGIN HideCursor; ShowBoth; ReplayHistory; ShowCursor END;
			5 : ZoomIn;
			6 : ZoomOut;
			7 : ResetScales({Clear:} True);
			8 : ShowSavedXSections;
		  END; {case rowChosen in Column 3}
	 3 : CASE rowChosen OF
			1 : ChangeSystem(Coulomb, {ChangeConditions=} True);
			2 : ChangeSystem(ShieldedC, True);
			3 : ChangeSystem(TruncatedC, True);
			4 : ChangeSystem(FPower, True);
			5 : ChangeSystem(HardSphere, True);
			6 : ChangeSystem(SoftSphere, True);
			7 : ChangeSystem(Yukawa, True);
			8 : ChangeSystem(WSaxon, True);
			9 : ChangeSystem(User, True);
			10: PreviousForce({SwapPointer= } True)
		  END; {case rowChosen in Column 4}
	 4 : Case RowChosen OF
			1 : HandleHelpMenu;
			2 : BEGIN
					RowChecked(3,Ord(WhichForce)+1, False);
					Tutorial;
					RowChecked(3,Ord(WhichForce)+1, True);
					ShowTrajectories;
					MainMenu.Display END;
			END {Column 4}
	END; {case colChosen}
	ShowCursor;
	Debounce
END; {HandleMenu}

PROCEDURE QuitRun;
BEGIN
	MainKeys.Display;
	MainMenu.Display;
	Quit := False;
	Running := False
END; {QuitRun}

{----------- Set up the various function key displays, ----------}
{-----------  Each is followed by its action routine   ----------}

PROCEDURE SetUpMainKeys;
BEGIN
	WITH MainKeys DO BEGIN
		Init(6);
		key[1] := 'F1-Help';
		key[2] := 'F2-Go';
		Key[3] := 'F3-New Data';
		key[4] := 'F5-Clear';
		Key[5] := 'F6-Speed';
		key[6] := 'F10-Menu';
	END;
	MainKeys.Display;
END; {SetUpMainKeys}

PROCEDURE DoMainKeys (key : byte);
BEGIN
	CASE key OF
	 1 : HandleHelpMenu;
	 2 : BEGIN
			  RunKeys.Display;
			  IF PlotXSection THEN ShowTrajectories;
			  Message(''); {Menu unavailable}
			  IF (Not ShowPaths) AND (DelayTime < MaxTime) AND (Plots=2)THEN
			  BEGIN
					Comment (0.05,0.92,
 'Apparent Slowdown at close approach is in part a computational artifact.');
				  CommentOn := True
			  END;
			  Running := True
		  END;
	 3 : BEGIN
			  RunKeys.Display;
			  Message(''); {Menu unavailable}
			  Running := True;
			  WhatRun
		  END;
	 4 : BEGIN
           IF PlotXSection THEN
           BEGIN
				  IF Plots = 4 THEN ShowBoth
				  ELSE IF Vectors THEN ShowVectors
						 ELSE ShowTrajectories
           END
			  ELSE Begin ClearPlots(True); HistoryCount := 0 END;
		  END;
	 5 : ReadDelayTime;
	 6 : BEGIN event.ReadKey := Chr(68);IF MainMenu.Chosen THEN HandleMenu END;
	END; {case}
	IF key <> 6 THEN Debounce
END; {DoMainKeys}

PROCEDURE SetUpRunKeys;
BEGIN
	WITH RunKeys DO BEGIN
		Init(3);
		key[1] := 'F3-Stop';
		key[2] := 'F4-Pause,Step';
		key[3] := 'F5-Continue';
	END
END; {SetUpRunKeys}

PROCEDURE DoHelpMenu(VAR Finished : Boolean);
BEGIN {DoHelpMenu}
	Finished := False;
	WITH HelpMenu DO
	CASE colChosen OF
	 1 : Case rowChosen of
			1 : Help(OpenScreen);
			2 : AboutCups;
			3 : ShowHelp('Colision.hlp','ConfigureHelp');
			5 : ShowHelp('Colision.hlp','ReadXsHelp');
			6 : ShowHelp('Colision.hlp','SaveXSHelp');
			7 : ShowHelp('Colision.hlp','ReadHistHelp');
			8 : ShowHelp('Colision.hlp','ReadHistOptionsHelp');
			9 : ShowHelp('Colision.hlp','SaveHistHelp');
		  END; {case rowChosen in Column 1}
	 2 : CASE rowChosen OF
			1 : ShowHelp('Colision.hlp','OneFrameHelp');
			2 : ShowHelp('Colision.hlp','TrajectoryHelp');
			3 : ShowHelp('Colision.hlp','VectorHelp');
			4 : ShowHelp('Colision.hlp','Traj&VectHelp');
			5 : ShowHelp('Colision.hlp','ZoomHelp');
			6 : ShowHelp('Colision.hlp','ZoomHelp');
			7 : ShowHelp('Colision.hlp','Default Scale Help');
			8 : ShowHelp('Colision.hlp','Display Saved Cross Sections');
		  END; {case rowChosen in Column 2}
	 3 : CASE rowChosen OF
			1 : ShowHelp('Colision.hlp','CoulombHelp');
			2 : ShowHelp('Colision.hlp','CoulombHelp');
			3 : ShowHelp('Colision.hlp','CoulombHelp');
			4 : ShowHelp('Colision.hlp','PowerHelp');
			5 : ShowHelp('Colision.hlp','HardHelp');
			6 : ShowHelp('Colision.hlp','SoftHelp');
			7 : ShowHelp('Colision.hlp','YukawaHelp');
			8 : ShowHelp('Colision.hlp','WSHelp');
			9 : ShowHelp('Colision.hlp','User Force Help');
			10: ShowHelp('Colision.hlp','Previous Force Help');
		  END; {case rowChosen in Column 3}
	 4 : CASE rowChosen OF
			1 : ShowHelp('Colision.hlp','IntroHelp');
			2 : ShowHelp('Colision.hlp','F-key Help');
			3 : ShowHelp('Colision.hlp','Collision Type Help');
			4 : ShowHelp('Colision.hlp','Set Condition Help');
			5 : ShowHelp('Colision.hlp','Range Help');
			6 : ShowHelp('Colision.hlp','Energy Range Help');
			7 : ShowHelp('Colision.hlp','XSect Help');
			8 : ShowHelp('Colision.hlp','Coulomb Choice Help');
			9 : ShowHelp('Colision.hlp','XSext Display Help');
			10: ShowHelp('Colision.hlp','New XSect Help');
		  END; {case rowChosen in Column 4}
	 5 : Finished := True
	END; {case colChosen}
	Debounce
END; {DoHelpMenu}

PROCEDURE HandleHelpMenu;
VAR Finished : Boolean;
	 key : byte;
BEGIN
	MainKeys.Clear;
	HelpMenu.BackColor := Red;
	HelpMenu.Display;
	ShowHelp('Colision.hlp','IntroHelp');
	ShowCursor; Finished := False;
	REPEAT
		CheckForEvents;
		IF Event.keypressed or Event.Mouseclicked THEN Finished := True;
		IF HelpMenu.Activated THEN DoHelpMenu(Finished)
	UNTIL Finished;
	HideCursor;
	Debounce;
	MainKeys.Display;
	ClearPlots(True);
	MainMenu.Display; ShowCursor
END; {HandleHelpMenu}

PROCEDURE SpreadIt(VAR RepCount : Integer);
VAR UseForce : ForceType;
BEGIN
	CASE Vary_What OF
		XSect : IF RepCount <= NumberOfReps THEN DrawXSection(RepCount)
	END; {Case}
	IF RepCount < NumberOfReps THEN
	BEGIN
		SelectMyViewPort(ParamPort);
		ClearViewPort;
		SetColor(White);
		CASE Vary_What OF
			ImpactP, XSect : BEGIN
				b := b + DeltaB;
				Print(1,1,ConCat('Impact parameter = ',	NumStr(b,7,2)));
				DeltaB := DeltaB*DeltaMult;
				END;
			VEnergy : BEGIN
				Energy := Energy + DeltaB;
				Print(1,1,ConCat('Energy = ',	NumStr(Energy,7,2)))
				END;
			Range : BEGIN
				R0 := R0 + DeltaB;
				R0_2 := R0*R0;
				Print(1,1,ConCat('R0 = ',	NumStr(r0,7,2)));
				DisplayPotential(True)
				END;
			Steepness : BEGIN
				a := a + DeltaB;
				Print(1,1,ConCat('a = ',	NumStr(a,7,2)));
				DisplayPotential(True)
				END;
			Strength : BEGIN
				IF Vary_Second THEN UseForce := SecondForce
					ELSE UseForce := WhichForce;
				IF UseForce IN [Coulomb,FPower,ShieldedC,TruncatedC] THEN
					BEGIN
					kc := kc + DeltaB;
					Print(1,1,ConCat('k = ',	NumStr(kc,7,2)))
					END;
				IF UseForce = SoftSphere THEN
					BEGIN
					ks := ks + DeltaB;
					Print(1,1,ConCat('k = ',	NumStr(ks,7,2)))
					END;
				IF UseForce = Yukawa THEN
					BEGIN
					ky := ky + DeltaB;
					Print(1,1,ConCat('k = ',	NumStr(ky,7,2)));
					END;
				IF UseForce = WSaxon THEN
					BEGIN
					kw := kw + DeltaB;
					Print(1,1,ConCat('k = ',	NumStr(kw,7,2)))
					END;
				DisplayPotential(True);
				END;
			FType    : BEGIN
					WhichForce := Succ(WhichForce);
					ForceName := ForceNames[RepCount+1];
					SetAccel;
					DisplayPotential(True)
				END;
			MassRatio : BEGIN
				IF ArithmeticProg THEN M[2] := M[2] + DeltaB
				ELSE M[2] := M[2] * DeltaB;
				Print(1,1,ConCat('Mass Ratio   /   = ',	NumStr(M[2],7,2)));
				SetColor(LightBlue);
				Print (12,1,'M2');
				SetColor(Yellow);
				Print (15,1,'M1')
				END;
		END; {Case}
		SelectMyViewPort(ParamPort);
		SetColor(White);
		Print(1,2,Concat('Force: ', ForceName));
		RepCount := RepCount + 1;
		Running := True
	END {RepCount < NumberOfReps}
	ELSE
	BEGIN
		RepCount := 1;
		Varying := False;
		HistoryCount := 0;
		MainMenu.RowActivate(1,9,False);
		Saving := False;
		MainKeys.Display;
		IF Vary_What = FType THEN NumberOfReps := SaveNReps;
		b := SaveImpactPar;
		kc := Savekc;
		kw := Savekw;
		ks := Saveks;
		ky := Saveky;
		a := Savea;
		R0 := SaveR0;
		Energy := SaveEnergy;
		M[2] := SaveM;
		MainMenu.Display
	END
END;{SpreadIt}

BEGIN {Always set Version to that of the unit with the largest Version number}
	If MyVersion > Version THEN Version := MyVersion;
	Vary_What := None; Varying := False;
	ArithmeticProg := False;
	InPtr := 0; OutPtr := 1;
	EnoughForces := 0;

	OpenScreen[2] := '                Two Body Collisions';
	OpenScreen[3] := '';
	OpenScreen[4] := '                   Bruce Hawkins';
	OpenScreen[5] := '                   Smith College';
   OpenScreen[6] := '    Consortium for Upper-level Physics Software';
   OpenScreen[7] := '         (c) 1995 John Wiley & Sons, Inc.';

   OpenScreen[9] := '--Includes both atomic and nuclear collisions.';
   OpenScreen[10]:= '  Information about each is in the Help system.';

   OpenScreen[12]:= '--Either trajectories or momentum vectors';
	OpenScreen[13]:= '  may be displayed: use the "Plots & Zoom" menu.';

   OpenScreen[15]:= '--The "New Data" hotkey gives access to all the';
   OpenScreen[16]:= '  collision parameters, Differential Cross Section';
   OpenScreen[17]:= '  plots, display choices, and more.';

   OpenScreen[19]:= '--Use the Speed hotkey if the collsion happens';
   OpenScreen[20]:= '  too fast to see.';

   OpenScreen[24]:= '>> To start a collision, click in the LabSystem'
                   {'  For line length control                         '}
END.
