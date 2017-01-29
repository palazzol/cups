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
	 {**           Unit OrbMenus.Pas              **}
	 {**          Program CentralF.Pas            **}
	 {**          Central Force Orbits            **}
	 {**        Written by Bruce Hawkins          **}
	 {**           January 21, 1992               **}
	 {**        Last change:  2/10/95             **}
	 {**             Version 1.00                 **}
	 {**                                          **}
	 {**      B. Hawkins Copyright @ 1992         **}
	 {**             CUPS Project                 **}
	 {**                                          **}
	 {**     Uses 12/95 MUPPET & CUPS Utilities   **}
	 {**********************************************}
	 {**********************************************}

{         Contains the menus and hotkeys and handling routines      }
{$F+}
UNIT OrbMenus;

INTERFACE

USES	CupsMupp, Graph, Cups, CupsFunc, CupsProc, CupsGui, CupsGrph, OrbPhy,
      OrbPar, OrbHelp;

TYPE
	WhichPlot = (CofM, Body1, Body2, Body3, Universe, Rotate,
					 Poincare, Done, CFactor, None);
	OrbSystem = (SunEMoon, Solar, Shuttle, SunJupComet,
					 Lagrange, Binary, Retrograde, SunJupMoons);

VAR
	ReplayKeys : THotKeys;

PROCEDURE SetUpRunKeys;
PROCEDURE DoRunKeys (key : byte);
PROCEDURE SetPlotsA;
PROCEDURE SetUpMenus;
PROCEDURE Replay(Temporary : Boolean);
PROCEDURE ChangeSystem(WhichSystem : OrbSystem);
PROCEDURE HandleMenu;     {Main menu routine does all the work}
PROCEDURE ExitSystem; {Set to Zero all normal parameters}
PROCEDURE DoSettings;
Procedure SetMyColor (Color:Word);
Procedure YTics(Yint, TicY : Real);
PROCEDURE MyAutoScaling (sNum : integer; VAR x,y : DVector);
PROCEDURE MenuInit;

IMPLEMENTATION

{****************************Internal Procedures***************************
PROCEDURE PlotButtons;
PROCEDURE ChoosePlot(VAR Flag : WhichPlot);
PROCEDURE FullScreen;
PROCEDURE ShowN(N : Integer);        Displays chosen N frames to Plot
PROCEDURE NextFrame(NoShowData : Boolean);
PROCEDURE ShowSix;         Displays all six

Within HandleMenu:
  PROCEDURE SetUpSlider(VAR Param : Real; ThisPort : Integer);  Draw a slider 
  PROCEDURE SelectValue(VAR Param : Real; ThisPort : Integer);  Uses a slider to select a parameter
		PROCEDURE ShowValueChosen;  Moves the slider following mouse 
  PROCEDURE Clear_Continue;
  PROCEDURE MarkCofMass;
  PROCEDURE ZoomOut;
  PROCEDURE ZoomIn;
  PROCEDURE ChangeParameters;
  PROCEDURE ReadSystem(GetFName);

**************************************************************************}

{-----------------------------------------------------------
Within DoSettings
	  PROCEDURE SetUpNameKeys;
	  PROCEDURE DoNameKeys (VAR Done : Boolean; key : byte);
	  PROCEDURE SetUpWhenKeys;
	  PROCEDURE DoWhenKeys (VAR Done : Boolean; key : byte);
	  PROCEDURE SetUpNumKeys;
	  PROCEDURE DoNumKeys (VAR Done : Boolean; key : byte);
	  PROCEDURE AskWhichBody;
	  PROCEDURE AskWhen;
	  PROCEDURE ChangeSpreadFactor;
	  PROCEDURE AskHowMany;
	  PROCEDURE AskCoordinate;
	  PROCEDURE ManyProbesInit;
--------------------------------------------------------------}
 
USES Crt, Dos;

CONST
	MyVersion = 1.00;
VAR
	CreatingSystem : Boolean;
	CurrentColor, NearestBody : Integer;

 {-----         Display one, two, or six graphs chosen by user         ------}
 {-----   choosing is done in later procedures, this sets the windows  ------}
 
PROCEDURE SetPlotsA;
VAR
	i, j, k, NPlotsIndex, SavRotN : Integer;
BEGIN
	SavRotN := RotNumber;
	IF Zoomed THEN PortTable[1,1] := OneFull ELSE PortTable[1,1] := One;
	IF Plots < 6 THEN
		FrameNameTable[6] := Concat('Rotating Frame (' , Nametable[R1NameIndex],
									  ' & ' , Nametable[R2NameIndex] , ')')
	 ELSE
		FrameNameTable[6] := Concat('Rot. (' , Nametable[R1NameIndex],
									  ' & ' , NameTable[R2NameIndex] , ')');

	NPlotsIndex := (Plots DIV 2) + 1;             {Turns 1,2,4,6 into 1,2,3,4}
	j := 1; k := 1;

	For i := 1 to Plots DO BEGIN
		If (j = 7) AND (RotNumber > 1) THEN
		BEGIN j := j - 1; RotNumber := RotNumber - 1 END;
		While Not PlotList[j] DO j := j + 1;
		PortsUsed[i] := PortTable[NPlotsIndex, k];
		ScaleUsed[i] := ScaleTable[j];
		NamesUsed[i] := FrameNameTable[j];
		FrameUsed[i] := j;
		XCenterUsed[i] := CenterTableX[j]; YCenterUsed[i] := CenterTableY[j];
		j := j + 1; k := k +1
	END; {For i}
	RotNumber := SavRotN
END; {SetPlotsA}

PROCEDURE SetPlots;
BEGIN
	PlotList[1] := PlotCofM; PlotList[2] := PlotBody1;
	PlotList[3] := PlotBody2; PlotList[4] := PlotBody3;
	PlotList[5] := PlotUniverse; PlotList[6] := PlotRotating;
	PlotList[7] := PlotPoincare;
	SetPlotsA;
END; {SetPlots}

{-------------- Select graphs and set up to display them  ------------}

PROCEDURE SetUpPoincare;
VAR JScreen : TInputScreen;
BEGIN
	IF PoinBody = 0 THEN 
	BEGIN
		MildAnnounce ('Poincar‚ plot not available for this system.');
		PlotPoincare := False
	END;
	IF SysName = 'Jupiter''s Moons' THEN
		With JScreen DO
		BEGIN
			Init;
			LoadLine ('    Jupiter''s Moons');
			LoadLine ('     Poincar‚ Plot');
			LoadLine ('');
			LoadLine (Concat('1: ',NameTable[3]));
			LoadLine (Concat('2: ',NameTable[4]));
			LoadLine (Concat('3: ',NameTable[5]));
			LoadLine (Concat('4: ',NameTable[6]));
			LoadLine ('Use moon {   }');
			SetNumber (1, PoinBody-2);
			SetNumberLimits (1, 1, 4);
			LoadLine ('');
			LoadLine('  [OK]    [Cancel]');
			DefineInputPort(0.1, 0.4, 0.25, 0.75);
			AcceptScreen;
			IF Not Canceled THEN
				PoinBody := Trunc(GetNumber(1)+2);
			Done;
			FrameNameTable[7] := 'Poincar‚ of '+NameTable[PoinBody]
										+' (Vx vs X at Y=0)';
			Case PoinBody of
         	3 : VPScale := 0.05;
         	4 : VPScale := 0.1;
         	5 : VPScale := 0.2;
         	6 : VPScale := 0.4
			END {Case}
		END
END; {SetUpPoincare}

PROCEDURE PlotButtons(N : Integer; VAR CancelIt : Boolean);
{ CONST  ColWidth   : integer =  8;
		 RowHt      : integer = 20; in Muppet}
VAR
	i, j, ILast : Integer;
	Screen : TInputScreen;
	Finished, Repeating : Boolean;
BEGIN
{M	HideCursor;}
	Finished := False; Repeating := False; CancelIt := False;
	j := 0;
	With Screen DO
	REPEAT
		Init;
		LoadLine('   View Selection');
		LoadLine('');
      IF PoinBody = 0 THEN Ilast := 6 ELSE ILast := 7;
		For i := 1 to ILast DO
		BEGIN
			LoadLine(ConCat('#F',FrameNameTable[i]));
			Case i of
				1 : SetBoolean(i,PlotCofM);
				2 : SetBoolean(i,PlotBody1);
				3 : SetBoolean(i,PlotBody2);
				4 : SetBoolean(i,PlotBody3);
				5 : SetBoolean(i,PlotUniverse);
				6 : SetBoolean(i,PlotRotating);
				7 : SetBoolean(i,PlotPoincare);
			END; {Case Flag of}
		END; {For i}
		If N > 1 THEN LoadLine('#F Second Rotating Window');
		If RotNumber = 2 THEN SetBoolean(8,True);
		LoadLine('');
		LoadLine(ConCat('Mark Exactly',NumStr(N,2,0),' windows.'));
		LoadLine('');
		IF Repeating THEN
			Announce(ConCat('Please mark exactly',NumStr(N,2,0),' windows.'));
		LoadLine('[OK]    [Cancel][Help]');
      SetHelpScreen(WindowsHelp);
		DefineInputPort(0.5, 0.98, 0.11, 0.91);
		AcceptScreen;
		IF Canceled THEN Begin Finished := True; CancelIt := True END
		ELSE
		BEGIN
			PlotCofM := GetBoolean(1);
			PlotBody1 := GetBoolean(2);
			PlotBody2  := GetBoolean(3);
			PlotBody3 := GetBoolean(4);
			PlotUniverse := GetBoolean(5);
			PlotRotating := GetBoolean(6);
			IF PoinBody > 0 THEN PlotPoincare := GetBoolean(7);
			IF PlotPoincare THEN SetUpPoincare;
			j := 0;
			For i := 1 to ilast DO IF GetBoolean(i) THEN j := j + 1;
			IF PlotRotating THEN
				RotNumber := 1;
			IF N > 1 THEN
					IF Getboolean(ilast+1) THEN
					BEGIN
						RotNumber := RotNumber + 1 ;
						j := j + 1;
						PlotRotating := True
					END;
			IF j = N THEN Finished := True
			ELSE Repeating := True
		END; {Not Canceled}
		Done;
	UNTIL Finished
{M	ShowCursor;}
END; {PlotButtons}

PROCEDURE FullScreen(Change : Boolean);
VAR Flag : WhichPlot;
	 CancelIt : Boolean;
BEGIN
	If Zoomed AND Change THEN BEGIN
		Zf := 0.92;
		MainMenu.row(3,7, 'Full Screen')
		END
	ELSE IF Change THEN BEGIN
		Zf := 1.5;
		MainMenu.row(3,7, 'Part Screen')
		END; {If Zoomed}
	IF Change THEN Zoomed := Not Zoomed;
	If (Plots > 1) AND Zoomed THEN BEGIN
		SaveThePlots; SetAllFalse;
		PlotButtons(1, CancelIt);
	   If CancelIt THEN
		   BEGIN RestorePlots; Exit END;
		Plots := 1;
		MainKeys.Display;
		MainMenu.Display

	END; {IF Plots  > 1}
	SetPlots;
	If AutoReplay AND ((HistoryCount > 1) OR Wrapped) THEN Replay({Temporary:} False)
		ELSE ClearPlots({Temporary:} False, {ShowInit:} True)
END; {FullScreen}

PROCEDURE ShowN(N : Integer);
VAR Flag : WhichPlot;
	 CancelIt : Boolean;
BEGIN
	SaveThePlots; SetAllFalse;
	If N > 1 Then Zoomed := False;
	MainMenu.row(3,7, 'Full Screen');
	RotNumber :=  0;  {Set up default choice}
	PlotButtons(N, CancelIt);
	If CancelIt THEN
		BEGIN RestorePlots; Exit END;
	Plots := N;
	If (Plots = 4) OR Zoomed THEN Zf := 1.5 ELSE Zf := 0.92;
{M	HideCursor;}
	MainKeys.Display;
	MainMenu.Display;
	CloseMyViewPort(ChangePort);
{M	ShowCursor;}
	IF Zoomed THEN FullScreen({Change =} False)
	ELSE BEGIN
		SetPlots;
		If AutoReplay AND ((HistoryCount > 1) OR Wrapped) THEN Replay({Temporary:} False)
			ELSE ClearPlots({Temporary:} False, {ShowInit:} True)
	END
END; {ShowN}

PROCEDURE NextFrame(NoShowData : Boolean);
BEGIN
	IF Plots > 1 THEN BEGIN
			SetAllFalse; Plots := 1; PlotCofM := True END
	ELSE IF PlotCofM THEN BEGIN PlotCofM := False; PlotBody1 := True END
	ELSE IF PlotBody1 THEN BEGIN PlotBody1 := False; PlotBody2 := True END
	ELSE IF PlotBody2 THEN BEGIN PlotBody2 := False; PlotBody3 := True END
	ELSE IF PlotBody3 THEN BEGIN PlotBody3 := False; PlotUniverse := True END
	ELSE IF PlotUniverse THEN BEGIN PlotUniverse := False; 
											  PlotRotating := True END
	ELSE IF PlotRotating THEN
	BEGIN
		PlotRotating := False;
		IF PoinBody = 0 THEN PlotCofM := True
		ELSE PlotPoincare := True
	END
	ELSE If PlotPoincare THEN
	BEGIN PlotPoincare := False; PlotCofM := True END;
{M	HideCursor;}
	MainKeys.Display;
	MainMenu.Display;
	CloseMyViewPort(ChangePort);
{M	ShowCursor;}
	If Zoomed THEN Zf := 1.5 ELSE Zf := 0.92;
	IF Zoomed THEN FullScreen({Change:} False)
	ELSE BEGIN
		SetPlots;
		If AutoNextReplay AND ((HistoryCount > 1) OR Wrapped) THEN Replay({Temporary:} False)
			ELSE ClearPlots({Temporary:} NoShowData, {ShowInit:} True)
		END;
END; {NextFrame}

{--------------- Sets up the menu display contents --------------}

VAR
	 dataScreen    : TInputScreen;   { input screen               }

PROCEDURE SetUpAMenu(VAR Menu : TMenu);
BEGIN
	Menu.Init;
	WITH Menu DO
	BEGIN
	  column(1, 'Files');
		 row(1,1, 'About Orbiter');
		 row(1,2, 'About Cups');
		 row(1,3, 'Configure');
		 row(1,4, '------------------');
		 row(1,5, 'Read System from File');
		 row(1,6, 'Save System to File');
		 row(1,7, 'Exit Program (Alt-x)');
	  RowActivate(1,4,False);
	  column(2, 'Choices');
		 row(2,1, 'Clear & Continue');
		 row(2,2, 'Restart');
		 row(2,3, 'Replay');
		 row(2,4, 'Replicate a Body');
		 row(2,5, 'Settings');
		 row(2,6, 'Reverse Time');
		 row(2,7, 'Change parameters');
		 row(2,8, 'Move Body (Mouse)');
		 row(2,9, 'Add a Body');
		 row(2,10, 'Allow Thrust');
	  column(3, 'Plots&Zoom');
		 row(3,1, 'Choose One Window');
		 row(3,2, 'Next One Window');
		 row(3,3, 'Choose Two Windows');
		 row(3,4, 'Choose Four Windows');
		 row(3,5, 'Choose Six Windows');
		 row(3,6, 'Mark Center of Mass');
		 row(3,7, 'Full Screen');
		 row(3,8, 'Zoom In');
		 row(3,9, 'Zoom Out');
		 row(3,10, 'Default Scales');
	  column(4, 'Systems');
		 row(4,1, 'Sun Earth Moon');
		 row(4,2, 'Solar System');
		 row(4,3, 'Shuttle Docking');
		 row(4,4, 'Sun Jupiter Comet');
		 row(4,5, 'Sun Jupiter & Moons');
		 row(4,6, 'Lagrangian Points');
		 row(4,7, 'Binary Star & Comet');
		 row(4,8, 'Retrograde Motion');
		 row(4,9, 'Create New System');
	  column(5, 'HELP!!');
	AutoChecking(4,2)
	END;
END; {SetUpAMenu}

PROCEDURE SetUpMenus;
BEGIN
	SetUpAMenu(MainMenu);
	SetUpAMenu(HelpMenu);
	HelpMenu.column(6, 'Quit Help');
	UseManyHelp := False;
	MainMenu.Display
END; {SetUpMenus}

{----------- Set up the various function key displays, ----------}
{-----------  Each is followed by its action routine   ----------}

PROCEDURE SetUpReplayKeys;
BEGIN
	WITH ReplayKeys DO BEGIN
		Init(4);
		key[1] := 'F5-Slower';
		key[2] := 'F6-Pause';
		key[3] := 'F7-Faster';
		key[4] := 'F8-Backup';
	END;
	ReplayKeys.Display;
END; {SetUpReplayKeys}

PROCEDURE DoReplayKeys (VAR DelayT : Integer; VAR Back : Boolean; key : byte);
BEGIN
	CASE key OF
	 1 : If DelayT = 0 THEN DelayT := 100 ELSE DelayT := DelayT + 10;
	 2 : BEGIN
			  Message('Press any key to continue.');
			  DynamicPause;
			  MainMenu.Display
			 END;
	 3 : BEGIN DelayT := DelayT - 30; If DelayT < 0 THEN DelayT := 0 END;
	 4 : BEGIN
			  If Back THEN ReplayKeys.key[4] := 'F8-Backup'
				  ELSE  ReplayKeys.key[4] := 'F8-Forward';
			  Back := Not Back;
			  ReplayKeys.Display
		  END;
	END; {case}
	Debounce
END; {DoReplayKeys}

{------- This one uses the keys to choose graphs displayed if no mouse ------}

PROCEDURE Replay(Temporary : Boolean);
VAR ihistory : Integer;
	 iplot, iframe, i,j, k, count, DupCount : Integer;
	 SaveWrap : Boolean;
	 DelayT : Integer; Back : Boolean; key : byte;
BEGIN
 IF (HistoryCount > 1) OR Wrapped THEN
 BEGIN
	 DelayT := 0;
	 Back := False;
	 SetupReplayKeys;
	 SaveWrap := Wrapped;
	 ReWrap;
	 t := SaveT - HistoryCount*TimeStep;
	 IF Wrapped THEN t := SaveT - HistoryLength*TimeStep;
	 ClearPlots(Temporary, {ShowInit:} False);
	 TempV.init(2*N2Bodies);
	 ihistory := 1;
	 IF Wrapped THEN ihistory := HistoryCount;
	 DupCount := RotNumber;
	 For iplot := 1 to Plots DO BEGIN
		 iframe := FrameUsed[iplot];
		 IF (iframe <> 6) {Rotating} OR (DupCount = RotNumber) THEN
		 BEGIN
			 SavePos[iframe].init(2*N2Bodies);
			 Position0[iframe].init(2*N2Bodies);
			 Positions[iframe].init(2*N2Bodies);
			 ToFrame(iframe, Positions[iframe],History[ihistory]);
			 SavePos[iframe].Equate(1.0, Positions[iframe]);
			 Position0[iframe].Equate(1.0, Positions[iframe]);
			 IF iframe = 6 THEN DupCount := DupCount - 1
		 END
	 END; {For iplot}
{M	 HideCursor;}
	 If (HistoryCount > 1) OR Wrapped THEN
		 Count := (HistoryCount MOD DotInterval);
	 While ((ihistory < HistoryCount)
			 OR (Wrapped AND (ihistory < HistoryLength)))  DO
		BEGIN
		CheckForEvents;
		If ReplayKeys.Pressed(key) THEN DoReplayKeys (DelayT, Back, key);
		Delay(DelayT);
		FOR iplot := 1 to Plots Do {Draw the trajectories}
		BEGIN
			iframe := FrameUsed[iplot];
         IF iframe <> 7 THEN
         BEGIN
				ToFrame(iframe, Positions[iframe],History[Ihistory]);
				i := 1; j := 1; k := 2;  {Used to minimize calculations in inner loop}
				SelectMyViewPort(PortsUsed[iplot]);
				SelectScale(iplot);
				REPEAT
					SetColor(ColorB[i]);
					If Back THEN SetColor(Black);
					PlotLine (Position0[iframe].Value(j),Position0[iframe].Value(k),
								 Positions[iframe].Value(j),Positions[iframe].Value(k));
					i := i + 1; j := j + 2; k := k+2
				UNTIL i > NBodies;
				Position0[iframe].Equate(1.0, Positions[iframe]);  {Set equal to first half of Y (length of X0 governs}
         END
		END; {FOR iplot}

	  t := t + TimeStep;
	  If ShowBodies AND (Count >= DotInterval) THEN
	  BEGIN   {Leave a DotGap*timestep gap in the track}
		  ihistory := ihistory + DotGap;
		  t := t + DotGap*TimeStep;
		  IF (ihistory < HistoryCount)
			  OR (Wrapped and (ihistory < HistoryLength)) THEN
			  FOR iplot := 1 to Plots DO BEGIN         {Set beginning of next line}
				  iframe := FrameUsed[iplot];
				  ToFrame(iframe, Position0[iframe],History[iHistory]) END;
		  Count := DotGap {Make lines eight timesteps long}
	  END; {If Count}
	  Count := Count + 1;
	  IF Back THEN ihistory := ihistory - 1 ELSE ihistory := ihistory + 1;
	  IF ihistory >= HistoryLength THEN BEGIN
		  ihistory := 1;
		  Wrapped := False END;
	  IF (Ihistory <= 0) AND SaveWrap THEN {This can only happen if Back true}
	  BEGIN
		  ihistory := HistoryLength-1;
		  Wrapped := True
	  END;
	  IF Back and Wrapped and (ihistory <= historycount) THEN
		  ihistory := HistoryLength {terminates loop}
	 END; {While ihistory}
	 Wrapped := SaveWrap;
	 t := SaveT;
	 If Not Temporary THEN ShowData(True);
	 DupCount := RotNumber;
	 For iplot := 1 to Plots DO BEGIN
		 iframe := FrameUsed[iplot];
		 IF (iframe <> 6) {Rotating} OR (DupCount = RotNumber) THEN
		 BEGIN
			 SavePos[iframe].free;
			 Position0[iframe].Free;
			 Positions[iframe].Free;
			 IF iframe = 6 THEN DupCount := DupCount - 1
		END
	 END;
	 Y.Equate(1.0, SaveY);
	 TempV.free;
	 MainKeys.Display
 END {IF HistoryCount > 1}
 ELSE BEGIN
	 Message ('No Replay history available (Restart clears it) Click Mouse or ENTER.');
	 StaticPause;
	 MainMenu.Display END;
{M	 ShowCursor}
END; {Replay}
	
PROCEDURE ChangeSystem(WhichSystem : OrbSystem);
VAR i : Integer;
BEGIN
	Case WhichSystem of
		SunEMoon, Lagrange, Binary, Retrograde, 
		 Shuttle, SunJupMoons, SunJupComet, Solar :
		 BEGIN
			 ExitSystem;
			 RotNumber := 1;
			 Replicated := False;
			 NewPicked := False;
			 FreeReplayMemory 
		 END
	ELSE
		BEGIN
		 Message ('System not available yet.  Click Mouse to continue');
		 StaticPause;
		 MainMenu.Display
		END {ELSE}
	END; {Case}
	If Zoomed THEN BEGIN
		Zf := 0.92;
		MainMenu.row(3,7, 'Full Screen')
	END;
	FOR i := 1 to NumNames DO
		NameOrderTable[i] := i;
	Case WhichSystem of
		 SunEMoon : BEGIN EMSParameters; ThreeBodyInit END;
		 Lagrange : BEGIN LagrangeParameters; GeneralInit END;
		 Binary   : BEGIN BinaryParameters; ThreeBodyInit END;
		 Retrograde : BEGIN RetrogradeParameters; ThreeBodyInit END;
		 Shuttle : BEGIN ShuttleParameters; GeneralInit END;
		 SunJupMoons : BEGIN JupiterMoonsParameters; GeneralInit END;
		 SunJupComet :  BEGIN JupiterCometParameters; GeneralInit END;
		 Solar :  BEGIN SolarSystemParameters; GeneralInit;
						FrameNameTable[1] := 'Outer Planets Window';
						FrameNameTable[2] :=  'Inner Planets Window';
					 END
	ELSE 
		BEGIN
		 Message ('System not available yet.  Click Mouse to continue');
		 StaticPause;
		 MainMenu.Display
		END {ELSE}
	END; {Case}
	MainMenu.Display;
	SetPlots;
	ClearPlots({Temporary:} False, {ShowInit:} True);
	If WhichSystem = Shuttle THEN
	BEGIN
		MainMenu.row(2, 10, 'Refuse Thrust');
		MildAnnounce (Concat('You may apply thrust to Atlantis with ',
								Chr(24),Chr(25),Chr(26),Chr(27),' keys'));
	END
END; {ChangeSystem}

FUNCTION GetNearestR(VAR WhichBody : Integer; XChosen, YChosen : Real;
					  N1, N2 : Integer) : Real;
VAR i, No : Integer;
	 R, RTemp : Real;
BEGIN
	R := 1E20;
	No := WhichBody;
	FOR i := N1 to N2 DO
   IF i <> No THEN {Don't pick the body itself}
		BEGIN
			RTemp := Sqrt(Sqr(XChosen-Y.Value(2*i-1))
						 + Sqr(YChosen-Y.Value(2*i)));
			IF (R > RTemp) AND (RTemp > 0.0) THEN
			BEGIN
				WhichBody := i; R := RTemp
			END
		END; {FOR i}
	GetNearestR := R;
END; {GetNearestR}

PROCEDURE LookForClicks(VAR XChosen, YChosen, VCx, VCy : Real;
								VAR N : Integer;
								VAR Cancel : Boolean);
VAR
	dx, dy, FirstBody, LastBody  : Integer;
	x2, y2, X0, Y0, R, Xr, Yr, VCirc, mass : Real;
	ThisView : View;
	xnow, ynow, button : Integer; key : byte;
	EventFound, PickCentralBody : Boolean;
	VKeys : THotKeys;

	PROCEDURE DrawCircularV(N : Integer);
	VAR S1 : Array [1..2] of String[5];
		 S2 : Array [1..2] of String[8];
	VAR j : Integer;
	BEGIN
		S1[1] := 'large'; S1[2] := 'small';
		S2[1] := 'increase'; S2[2] := 'decrease';
		SelectMyViewPort(PortsUsed[1]);
		IF N > MaxHeavy then mass := 0 ELSE mass := M[N];
		VCirc := Sqrt(G*(M[NearestBody]+mass)/R);
		VCx := -ScaleUsed[1]*Yr/(Pwr(R,1.5)*8);
		VCy := ScaleUsed[1]*Xr/(Pwr(R,1.5)*8);
		SetColor(Yellow);
{M		HideCursor;}
		PlotLine(XChosen, YChosen,  XChosen + VCx, YChosen + VCy);
		SelectMyViewPort(PortsUsed[1]); {CofM is always first port on list}
		SetColor(ColorB[N]);
		DrawCircle(XChosen,YChosen,DotSize,ColorB[N]);
		OpenMyViewPort(ChangePort);
		SetColor(Yellow);
		Print (1,6,'Circular Velocity relative');
		Print (2,7,ConCat(' to ',Nametable[NearestBody],' shown;'));
		SetColor(White);
		Print (2,8,'change with the mouse.');
		Print (12,9,'OR');
		Print (2,10,'F1-Help');
		Print (2,11,'F3-Use Circular Velocity');
		  Print(5,12,'as shown.');
		Print (2,13,'F4-Use Circular Velocity');
		  Print (5,14,'in opposite direction.');
		Print (2,15,'F5-Pick body to relate to.');
		Print (2,16,'F9-Don''t add (move) body.');
		IF CreatingSystem THEN
			IF (AllNormScale > 6.0) OR (AllNormScale < 0.5) THEN
			BEGIN
				IF (AllNormScale > 6.0) THEN j := 1 ELSE j := 2;
				Print (2,15,ConCat('With such a ',S1[j],' scale'));
				Print (2,16,ConCat('you may wish to ',S2[j]));
				Print (2,17,'the time step.');
				Print (2,18,'Use Change Parameters:');
				Print (2,19,'System (Main Menu)');
			END; {Creating with large or small Scale}
		VKeys.Init(5);
		VKeys.key[1] := 'F1- Help';
		VKeys.key[2] := 'F3- +V Circ';
		VKeys.key[3] := 'F4- -V Circ';
		VKeys.key[4] := 'F5- Rel. Body';
		VKeys.key[5] := 'F9- Cancel';
		VKeys.Display;
{M		ShowCursor;}
		X0 := XChosen + VCx; Y0 := YChosen + VCy;
		ClearMyViewPort (ClearTimePort);
		SetColor(White);
		SelectMyViewPort(AllPort);
		Print(5,21,ConCat('V/VCircular relative to ',Nametable[NearestBody],' = '));
		Print(46,21,'1.00');
	END; {DrawCircularV}

	PROCEDURE FindPlace(VAR N : Integer);
	BEGIN
		IF N = 0 THEN {Choose a Body}
	   BEGIN
			REPEAT CheckForEvents UNTIL Event.MouseClicked;
			WITH Event DO WITH ThisView DO
         BEGIN
            dx := (vx2-vx1);
            x2:=(x-vx1) /dx;
			   XChosen := GraphScales[1].xmin +
									  (GraphScales[1].Xmax-GraphScales[1].Xmin)*x2;
			   dy := (vy2-vy1);
			   y2:=(vy2-y) /dy;
			   YChosen := GraphScales[1].ymin +
									  (GraphScales[1].Ymax-GraphScales[1].Ymin)*y2;
         END;
			R := GetNearestR(N, XChosen, YChosen, 1, NBodies);{N is 0, any body}
			SelectMyViewPort(PortsUsed[1]); {CofM is always first port on list}
         HideCursor;
			SetColor(Black);
			DrawCircle(Y.Value(2*N-1), Y.Value(2*N),DotSize,Black);
         ShowCursor;
         SelectMyViewPort(ChangePort);
         SetColor(White);
		END {Choosing Body}
		ELSE DynamicPause;
		Xr := 0; Yr := 0.1; {Initialize, not at c of m since sun likely there}
		WITH ThisView DO {Look for events in Port}
		REPEAT {Drag Mouse}
			MouseGlobalPosn(xnow, ynow, button);
			dx := (vx2-vx1);
			x2:=(xnow-vx1) /dx;
			dy := (vy2-vy1);
			y2:=(vy2-ynow) /dy;
			XChosen := GraphScales[1].xmin +
									  (GraphScales[1].Xmax-GraphScales[1].Xmin)*x2;
			YChosen := GraphScales[1].ymin +
									  (GraphScales[1].Ymax-GraphScales[1].Ymin)*y2;
			SelectMyViewPort(PortsUsed[1]); {CofM is always first port on list}
			SetColor(Black);
			HideCursor;
			DrawCircle(Xr, Yr, DotSize,Black);
			SetColor(ColorB[N]);
			DrawCircle(XChosen, YChosen, DotSize,ColorB[N]);
			ShowCursor;
			Xr := XChosen; Yr := YChosen;
			R := Sqrt(Sqr(XChosen-Y.Value(1))
					  + Sqr(YChosen-Y.Value(2)));
			IF PickCentralBody THEN
				R := Sqrt(Sqr(XChosen-Y.Value(2*NearestBody-1))
						 + Sqr(YChosen-Y.Value(2*NearestBody)))
			ELSE
			BEGIN
				NearestBody := N; {Don't pick this body}
				R := GetNearestR(NearestBody, XChosen, YChosen,
					{FirstBody:} 1, {LastBody:} NHeavy)
			END;
{M			HideCursor;}
			SelectMyViewPort(ChangePort);
			SetColor(White);
			Print(2,6,'Position and velocity are');
			Print(2,7,'relative to '+NameTable[NearestBody]+'.');
			Print(2,8,'Change with F-5');
			ClearMyViewPort(ClearVCircPort);
			SetColor(White);
			SelectMyViewPort(AllPort);
			Print(45,21,ConCat(Nametable[NearestBody],' = ',Numstr(R,5,2)));
{M			ShowCursor}
	  UNTIL Button = 0; {Mouse Released}
	END; {FindPlace}

	PROCEDURE PlaceBody(VAR N : Integer);
	BEGIN
{M		HideCursor;}
		MainKeys.Clear; Message ('');
		ClearMyViewPort (ClearTimePort);
		ClearMyViewPort(ChangePort);
		SetColor(White);
		IF N = 0 THEN
		BEGIN
			Print(2,3,'Choose body and');
			Print(2,4,'move body with the mouse.')
		END
		ELSE
			Print(2,4,'Place body with the mouse');
		SelectMyViewPort(AllPort);
		Print(30,21,'R (Au) to  ');
{M		ShowCursor;}
		FindPlace(N);
      PickCentralBody := False;
		Xr := XChosen-Y.Value(2*NearestBody-1); Yr := YChosen-Y.Value(2*NearestBody);
	END; {PlaceBody}

	PROCEDURE PickVel;
	VAR
		Xp, Yp : Real;
		Again : Boolean;
	BEGIN
   	Xp := X0; Yp := Y0; {Save for drawing ghost circular velocity}
		SelectMyViewPort(PortsUsed[1]);
		REPEAT
		 Again := False;
		 DynamicPause;
		 CheckForEvents;
		 IF VKeys.Pressed(key) THEN
		 BEGIN
		  CASE key of
			1 : BEGIN ShowHelp('orbiter.hlp','PickCircularHelp');
					Again := True END;
			2 : ; {Do nothing accepts circular velocity}
			3 : VCirc := - VCirc;
			4 : BEGIN PickCentralBody := True; Exit END;
			5 : Cancel := True
		  END; {Case}
		  VCx := -VCirc*Yr/R;
		  VCy := VCirc*Xr/R;
		 END {VKeys}
		 ELSE WITH ThisView DO {Look for events in Port}
		 REPEAT {Drag Mouse}
			MouseGlobalPosn(xnow, ynow, button);
			x2:=(Xnow-vx1) /dx;
			y2:=(vy2-ynow) /dy;
			X2 := GraphScales[1].xmin +
									  (GraphScales[1].Xmax-GraphScales[1].Xmin)*x2;
			Y2 := GraphScales[1].ymin +
									  (GraphScales[1].Ymax-GraphScales[1].Ymin)*y2;
{M			HideCursor;}
			SetColor(Black);
			PlotLine(XChosen, YChosen,  X0, Y0);
			SetColor(LightGray);
			PlotLine(XChosen, YChosen,  Xp, Yp);
			SetColor(White);
			PlotLine(XChosen, YChosen,  X2, Y2);
			X0 := X2 ; Y0 := Y2;
			VCx := VCirc*(X2-XChosen)*8*Pwr(R,0.5)/ScaleUsed[1];
			VCy := VCirc*(Y2-YChosen)*8*Pwr(R,0.5)/ScaleUsed[1];
			ClearMyViewPort(ClearVCircPort);
			SetColor(White);
			SelectMyViewPort(AllPort);
			Print(30+15,21,Numstr(Sqrt(Sqr(Vcx)+Sqr(Vcy))/VCirc,5,2));
{M			ShowCursor;}
			SelectMyViewPort(PortsUsed[1]);
		 UNTIL Button = 0; {Mouse is released}
		UNTIL Not Again;
		EventFound := True;
		VCx := VCx + Y.Value(N2Bodies+2*NearestBody-1);
		Vcy := VCy + Y.Value(N2Bodies+2*NearestBody);
	END; {PickVel}

   PROCEDURE ChooseCentralBody;
   VAR Screen : TInputScreen;
   	 i : Integer;
   BEGIN
   	WITH Screen DO
		BEGIN
			Init;
			Loadline('      Pick Body');
			Loadline('');
         Loadline('(Massive Bodies only)');
         Loadline('');
         For i := 1 to NHeavy DO
         	IF i <> N THEN {Avoid the body itself}
		         Loadline('#1 '+NameTable[i]);
			Loadline('');
			Loadline('[OK]    [Cancel] [Help]');
			SetHelpFile('Orbiter.hlp','CenterBodyHelp');
			DefineInputPort(0.1,0.45,0.4,0.8);
			AcceptScreen;
			IF Not Canceled THEN NearestBody := GetRadioButton('1');
			Done
      END
   END;

BEGIN {LookForClicks  (CofM frame has been displayed)}
	Message ('');
	EventFound := False;
	If PortsUsed[1] < SwapPort THEN ThisView := Views[PortsUsed[1]]
		ELSE ThisView := MyViews[PortsUsed[1]];
	SelectScale(1);
   PickCentralBody := False;
   REPEAT
		PlaceBody(N);
		DrawCircularV(N);
		PickVel;
      IF PickCentralBody THEN
      BEGIN
			ChooseCentralBody;
			IF PickCentralBody THEN
   	   BEGIN
         	HideCursor;
				SetColor(Black);
				DrawCircle(XChosen, Ychosen, DotSize,Black);
				PlotLine(XChosen, YChosen,  X0, Y0);
            ShowCursor
			END
      END
   UNTIL Not PickCentralBody;
   VKeys.Clear
END; {LookForClicks}

PROCEDURE PickBody(N : Integer);
VAR 
	X, Y, Vx, Vy, SaveZf : Real;
	SaveList : Array [1..7] of Boolean;
	PlotSav, i : Integer;
	Saved, AutoSav, Cancel : Boolean;
BEGIN
	Saved := False; Cancel := False;
	IF (Not PlotCofM) OR (Plots > 1) THEN 
	BEGIN
		PlotSav := Plots;
		SaveZf := Zf;
		Saved := True;
		FOR i := 1 to 7 DO SaveList[i] := PlotList[i];
		AutoSav := AutoNextReplay; AutoNextReplay := False;
      OpenMyViewPort (AllPort);
		NextFrame({NoShowData:} True); {Will be CofM}
		GraphBackColor := Blue;
		AutoNextReplay := AutoSav
	END;
	N := 0;  {0 means ask which body}
	LookForClicks(X, Y, Vx, Vy, N, Cancel);
	IF Not Cancel THEN
	BEGIN
		SetNewPlace(X, Y, Vx, Vy, N);
		NewPicked := True;
	END;
	Debounce;
	MainMenu.Display; MainKeys.Display;
	IF Saved THEN
	BEGIN
		Plots := PlotSav;
		Zf := SaveZf;
		FOR i := 1 to 7 DO PlotList[i] := SaveList[i];
		SetPlotsA;
	END;
END; {PickBody}

PROCEDURE AddBody;
VAR
	SaveList : Array [1..7] of Boolean;
	PlotSav, i, N : Integer;
	XPos, YPos, Vx, Vy : Real;
	Saved, Cancel : Boolean;
BEGIN
	MainKeys.Clear; Message ('');
	Saved := False; Cancel := False;
	AddingBody := True;
	N := NBodies+1;
	IF N > 1 THEN ColorB[N] := ColorB[N-1]+1;
	IF ColorB[N] > 15 THEN ColorB[N] := 9;
	Nametable[N] := ' ';
	IF N = 1 THEN InputModeCartesian := True
	ELSE
	With DataScreen DO
	BEGIN
		Init;
		LoadLine('Parameter Input Mode');
		Loadline('');
		LoadLine('#1 Polar Coordinates');
		LoadLine('#1 Orbital Elements');
		LoadLine('');
		LoadLine ('[  OK  ]   [Cancel]');
		IF NOT InputModeCartesian THEN SetRadioButton ('1',2);
		DefineInputPort(0.1,0.43,0.5,0.8);
		AcceptScreen;
		IF Not Canceled THEN
			IF GetRadioButton('1') = 2 THEN InputModeCartesian := False
			ELSE InputModeCartesian := True;
		Done;
	END;
	IF NBodies = 0 THEN M[N] := 1.0
	ELSE IF N <= MaxHeavy THEN
		IF Nheavy < NBodies-1 THEN M[N] := 0.0
		ELSE M[N] := M[NBodies];
	IF MousePresent AND (NBodies > 0) THEN
	BEGIN
		IF (NHeavy = NBodies) AND (N <= MaxHeavy) THEN
			GetMass(N);
		IF (Not PlotCofM) OR (Plots > 1) THEN
		BEGIN
			PlotSav := Plots;
			Saved := True;
			FOR i := 1 to 7 DO SaveList[i] := PlotList[i];
         OpenMyViewPort (AllPort);
			NextFrame({NoShowData:} True); {Will be CofM}
			GraphBackColor := Blue;
			MainKeys.clear;
			ClearPlots(False,True); {Because NextFrame Replays}
		END;
		LookForClicks(XPos, YPos, Vx, Vy, N, Cancel)
	END
	ELSE  {First body in New System}
	BEGIN
		XPos := 0.0; YPos := 0.0; Vx := 0.0; Vy := 0.0; M[1] := 1.0
	END;
	IF Not Cancel THEN
	BEGIN
	NBodies := NBodies + 1; N2Bodies := 2* Nbodies; NBodiesOld := NBodies;
	IF NBodies > 1 THEN InitY.Equate(1.0, Y);
	Y.ReSize(4*NBodies); YP.ReSize(4*NBodies);
	FOR i := 1 TO N2Bodies - 2 DO
	BEGIN
		Y.Put(i,InitY.Value(i)); Y.Put(N2Bodies+i,InitY.Value(N2Bodies+i-2))
	END;
	IF (NHeavy = NBodies - 1) AND (NBodies <= MaxHeavy) AND (M[N] > 0.0) THEN
		 NHeavy := NBodies;
	IF MousePresent OR (NBodies = 1) THEN SetNewPlace(XPos, YPos, Vx, Vy, N)
	ELSE  {Duplicate last Body}
	  FOR i := N2Bodies-1 TO N2Bodies DO
	  BEGIN
		  Y.Put(i,InitY.Value(i-2));
		  Y.Put(N2Bodies+i,InitY.Value(N2Bodies+i-2))
	  END;
	IF InputModeCartesian THEN
		SetCartesianBodyParameters(NBodies, NearestBody, Cancel)
	ELSE
		SetBodyElements(NBodies, NearestBody, {NewBody: }NBodies = 1, Cancel);
	IF Cancel THEN
		MildAnnounce(
			  'Sorry, it was too late to cancel the body. X has been added.');
	IF NBodies <= MaxHeavy THEN
		IF (Nheavy < NBodies-1) OR (M[NBodies] = 0.0) THEN M[NBodies] := 0.0
		ELSE NHeavy := NBodies;
	IF (NHeavy = NBOdies) AND (M[NBodies] = 0.0) THEN NHeavy := NBodies - 1;
	IF NHeavy > MaxHeavy THEN NHeavy := MaxHeavy;
	NHeavyOld := NHeavy; N2Heavy := 2*NHeavy;
	ThrustBody := NBodies;  {Allow thrust to be applied to last body added}
	FOR i := 1 to NHeavy DO BEGIN
		Xh[i].Resize(NBodies); Yh[i].Resize(NBodies); Rh_3[i].Resize(NBodies) END;
	InitY.Resize(4*NBodies);
	InitY.Equate(1.0, Y);
	N_Body_Force(t, Y, YP);
	END; {Not Cancel}
	Replicated := False; NewPicked := False; AddingBody := False;
	Debounce;
	MainMenu.Display; MainKeys.Display;
	IF Saved THEN
	BEGIN
		Plots := PlotSav;
		If (Plots = 4) OR Zoomed THEN Zf := 1.5 ELSE Zf := 0.92;
		FOR i := 1 to 7 DO PlotList[i] := SaveList[i];
		SetPlotsA
	END;
END; {AddBody}

PROCEDURE CreateSystem;
VAR CKeys : THotKeys;
	 key : Byte;
	 Done, AddAnother : Boolean;
	 i : Integer;
BEGIN
	LagrangeParameters; {To reset distance and time scales}
	CreatingSystem := True;
	AllNormScale := 1;
   DataBody := 2; DataCenterBody := 1; ShowEnergyBody :=2;
	ExitSystem;
	TimeStep := 0.001;
	MainKeys.Clear; Message(' ');
	FreeReplayMemory;
	FOR i := 1 to 15 DO NameTable[i] := ' ';
	FOR i := 1 to MaxHeavy DO M[i] := 0.0;
	FOR i := 1 to 6 DO
	BEGIN
		XCenterUsed[i] := 0.0; YCenterUsed[i] := 0.0;
		CenterTableX[i] := 0.0; CenterTabley[i] := 0.0
	END;
	CenterTableX[7] := 0.0; CenterTabley[7] := 0.0;
	CKeys.Init(3);
	CKeys.key[1] := 'F1-Help';
	CKeys.key[2] := 'F3-Add Body';
	CKeys.key[3] := 'F4-Done';
	Done := False;
   DrawSightLine := False;
	NBodies := 0; NHeavy := 1; N2Heavy := 2;
	SetSystem;
   AddAnother := True;
	REPEAT {Until Done}
      IF AddAnother OR (NBodies < 2) Then
      BEGIN
			CKeys.Clear;
			AddBody;
         MainKeys.Clear;
{M			HideCursor;}
			Message(' ');
			GraphBackColor := Black;
			OpenMyViewPort(AllPort);
			GraphBackColor := Blue;
			CKeys.Display;
			AddAnother := False
      END;
{M		ShowCursor;}
		IF NBodies > 1 THEN
		BEGIN
			MildANnounce ('Select an F-key');
			REPEAT CheckForEvents; UNTIL Ckeys.Pressed(Key);
			Case key OF
				 1 : BEGIN
						ShowHelp('Orbiter.hlp','AddAnotherHelp');
						StaticPause;
						SetTextStyle(DefaultFont, HorizDir, 1);
						GraphBackColor := Black;
						OpenMyViewPort(AllPort);
						GraphBackColor := Blue
				     END;
				 2 : AddAnother := True;
				 3 : Done := True;
			END {Case}
		END {IF Nbodies > 1}
	UNTIL Done;
	MainKeys.Display; MainMenu.Display;
	SetFrameNameTable;
	SetPlotsA;
	CreatingSystem := False;
	LightEnergyName := Concat(NameTable[2],' En.');
	RestartIt;
END; {CreateSystem}

{---------- Here are the actions taken when menu items are picked ----------}
PROCEDURE HandleMenu;

	PROCEDURE MarkCofMass;
	BEGIN
		ShowCofM := Not ShowCofM;
		IF ShowCofM THEN MainMenu.row(3,6, 'Don''t Mark C of M')
		ELSE MainMenu.row(3,6, 'Mark Cent of Mass')
	END; {MarkCofMass}

	TYPE
		Str3 = String[3];
		WhichFrame = 0..9;
	CONST
		Rot = 6; ZDone = 7; CFactor = 8;

	PROCEDURE ZoomButtons(InStr : Str3; VAR Flag : WhichFrame);
	{ CONST  ColWidth   : integer =  8;
			 RowHt      : integer = 20; in Muppet}
	VAR i : Integer;
		 DirString : String[3];
		 FillChar : Char;
		 Screen : TInputScreen;
	BEGIN
{M		HideCursor;}
		WITH Screen DO
		BEGIN
			Init;
			LoadLine(ConCat('    Zoom ',InStr,' Select'));
			LoadLine('');
			For i := 1 TO Plots DO
			BEGIN
				LoadLine(ConCat('#1 ',NamesUsed[i]));
			END; {For i}
			LoadLine('');
			LoadLine('#1 Done');
			LoadLine('');
			LoadLine('Zoom Factor {    }');
         SetRadioButton('1', Plots+1);
			SetNumber(Plots+2,ZFact);
			LoadLine('');
			LoadLine('[ OK ]  [Cancel][Help]');
         IF InStr = 'In' THEN
				SetHelpScreen(ZoomInHelp) ELSE SetHelpScreen(ZoomOutHelp);
			DefineInputPort(0.66, 0.98, 0.11, 0.91);
			AcceptScreen;
			IF Canceled THEN Flag := ZDone
			ELSE
			BEGIN
				Flag := GetRadioButton('1');
				IF GetBoolean(Plots+1) THEN Flag := ZDone;
				ZFact := GetNumber(Plots+2)
			END; {Not Canceled}
			Done;
		END {With Screen}
{M		ShowCursor;}
	END; {ZoomButtons}

	PROCEDURE ZoomOut;
	VAR
		Flag : WhichFrame;
		Ch : Char;
	BEGIN
	 IF Plots > 1 THEN
	 BEGIN
		REPEAT
			ZoomButtons('Out',Flag);
			IF Flag <> ZDone THEN ScaleUsed[Flag]  := ScaleUsed[Flag]*ZFact;
		UNTIL Flag = ZDone
	 END {Plots > 1}
	 ELSE ScaleUsed[1]  := ScaleUsed[1]*ZFact;
	 MainMenu.Display;
	 If AutoReplay AND ((HistoryCount > 1) OR Wrapped) THEN
	 BEGIN
		Replay({Temporary:} False);
		ShowInitialPositions
		END
		ELSE ClearPlots({Temporary:} False, {ShowInit:} True)
	END; {ZoomOut}

	Procedure PickCenter(Flag : WhichFrame; VAR XCenter, YCenter : Real);
	VAR
		PlotSav, dx, dy : Integer;
		x2, y2, ZfSav : Real;
	BEGIN
		PlotSav := Plots;  ZfSav := Zf;
		SetAllFalse;
      OpenMyViewPort(ChangePort);
		Message ('Click mouse where you want to zoom in to.');
		SelectMyViewPort(ChangePort);
		SetColor(Yellow);
		Print(4,12,'Click mouse in plot');
		Print(4,13,'At desired center.');
      SetColor(White);
		Plots := 1; IF Zoomed THEN Zf := 1.5 ELSE Zf := 0.92;
		Case FrameUsed[Flag] of 
			1 : PlotCofM := True;
			2 : PlotBody1 := True;
			3 : PlotBody2 := True;
			4 : PlotBody3 := True;
			5 : PlotUniverse := True;
			6 : PlotRotating := True;
			7 : PlotPoincare := True
		END; {Case Flag}

{----- Call to ClearPlots (direct or via Replay) with Plots = 1 sets
		 PortsUsed[1] and GraphScales[1]
		 Temporary  avoids clearing button window
		 Replay is essential here -------}

		PortsUsed[1] := One;
		NamesUsed[1] := NamesUsed[Flag];
		FrameUsed[1] := FrameUsed[Flag];
		PlotList[1] := PlotList[Flag];
		ScaleUsed[1] := ScaleUsed[Flag];
		XCenterUsed[1] := XCenterUsed[flag];
		YCenterUsed[1] := YCenterUsed[flag];
		If ((HistoryCount > 1) OR Wrapped) THEN Replay({Temporary:} True)
			ELSE ClearPlots({Temporary:} True, {ShowInit:} True);
		WITH event DO WITH views[PortsUsed[1]] DO {Look for mousclick in window}
		REPEAT
			CheckForEvents;
			  IF mouseClicked AND (x>vx1) AND (x<vx2) AND (y>vy1) AND (y<vy2) THEN
			  BEGIN {Look for position chosen}
				  dx := (vx2-vx1);
				  x2:=(x-vx1) /dx;
				  XCenter := GraphScales[1].xmin +
											 (GraphScales[1].Xmax-GraphScales[1].Xmin)*x2;
				  dy := (vy2-vy1);
				  y2:=(vy2-y) /dy;
				  YCenter := GraphScales[1].ymin +
											 (GraphScales[1].Ymax-GraphScales[1].Ymin)*y2;
			 END; {WITH event & Views, IF Mouseclicked in window}
		UNTIL Event.MouseClicked AND (x>vx1) AND (x<vx2) AND (y>vy1) AND (y<vy2);
		Debounce;
		Plots := PlotSav; Zf := ZfSav
	END; {PickCenter}

	PROCEDURE ZoomIn;
	VAR 
		Flag : WhichFrame;
		Ch : Char;
		iPort, jPort, j : Integer;
		SavPorts, SavFrames : Array[1..6] of Integer;
		SavScales, SavXCenter, SavYCenter : Array[1..6] of Real;
		SavPlotList : Array[1..7] of Boolean;
		SavNames : Array[1..6] of String[35];
		XCenter, YCenter : Real;
	BEGIN
		FOR iPort := 1 to 6 DO
		BEGIN
			SavPorts[iPort] := PortsUsed[iPort];
			SavScales[iPort] := ScaleUsed[iPort];
			SavNames[iPort] := NamesUsed[iPort];
			SavFrames[iPort] := FrameUsed[iPort];
			SavXcenter[iPort] := XCenterUsed[iPort];
			SavYCenter[iPort] := YCenterUsed[iPort];
			SavPlotList[iPort] := PlotList[iPort]
		END; {FOR iPort}
		SavPlotList[7] := PlotList[7];
		IF Plots = 1 THEN Flag := 1 ELSE Flag := 0;
		REPEAT
			ZoomButtons('In',Flag);
			IF MousePresent AND NOT ((Flag = ZDone) OR (Flag = CFactor))THEN
				PickCenter (Flag, XCenter, YCenter);
			IF (Not MousePresent) AND (FrameUsed[Flag] = Rot) THEN
			BEGIN 
				Message (ConCat('Center Plot on: 1-',Body1Name,', 2-',
						Body2Name,', or 3-',Body3Name,'? (Press 1,2,or3)'));
				REPEAT Ch := ReadKey UNTIL Ch in ['1','2','3'];
				Case Ch of
					'1' : BEGIN 
								XCenter := 0.0;
								YCenter := 0.0 
							END;
					'2' : BEGIN 
								XCenter := R12;
								YCenter := 0.0
							END;
					'3' : BEGIN 
								XCenter := R12*Cos(60*Pi/180);
								YCenter := R12*Sin(60*Pi/180)
							END;
					END; {Case Ch}
				MainMenu.Display
			END; {No Mouse}
			Case Flag of
				1,2,3,4,5,6 : BEGIN
						 SavScales[Flag]  := SavScales[Flag]/ZFact;
						 SavXcenter[Flag] := XCenter;
						 SavYcenter[Flag] := YCenter
					 END {Case 1,2,3,4,5,6}
			END; {Case of Flag}
			FOR iPort := 1 to 6 DO
			BEGIN
				PortsUsed[iPort] := SavPorts[iPort];
				ScaleUsed[iPort] := SavScales[iPort];
				NamesUsed[iPort] := SavNames[iPort];
				FrameUsed[iPort] := SavFrames[iPort];
				XCenterUsed[iPort] := SavXcenter[iPort];
				YCenterUsed[iPort] := SavYCenter[iPort];
				PlotList[iPort] := SavPlotList[iPort]
			END; {FOR iPort}
			PlotList[7] := SavPlotList[7];
			PlotCofM := PlotList[1]; PlotBody1 := PlotList[2];
			PlotBody2 := PlotList[3];  PlotBody3 := PlotList[4];
			PlotUniverse := PlotList[5]; PlotRotating := PlotList[6];
			PlotPoincare := PlotList[7];
			If (Flag = ZDone) AND AutoReplay
				AND ((HistoryCount > 1) OR Wrapped) THEN
				BEGIN
					Replay({Temporary:} False);
					ShowInitialPositions
					END
					ELSE ClearPlots({Temporary:} False, {ShowInit:} True);
			IF Flag <> ZDone THEN Flag := 0;
			Debounce
		UNTIL Flag = ZDone;
		MainMenu.Display
	END; {ZoomIn;}

	PROCEDURE SystemParameters;
	VAR  
		i : Integer;
		TRev : Boolean;
		Screen : TInputScreen;
	BEGIN
	  WITH Screen DO
	  BEGIN
		Init;
		IF TimeSTep < 0.0 THEN TRev := True ELSE TRev := False;
		TimeStep := Abs(TimeStep);
		DefineInputPort(0.02, 0.98, 0.19, 0.83);
		LoadLine ('         SYSTEM PARAMETERS:');
		LoadLine ('');
		LoadLine (ConCat(' G         = {               } ',GName));
		LoadLine (ConCat(' time step = {       } ',TimeName));
		Loadline (Num2Str(NHeavy,2)+' massive bodies');
		For i := 1 to NHeavy Do
		BEGIN
			Loadline (NameTable[i]+' mass: {        }');
			SetNumber(2+i,M[i])
		END;
		LoadLine ('');
		LoadLine ('       [  OK  ]    [Cancel]');
		SetNumber (1,G);
		SetNumber(2,TimeStep);
		SetNumberLimits(1, 0.00001, 1000);
		SetNumberLimits(2, 0.0001, 100);
		Message('Press <ENTER> to accept changes, <ESC> to leave unchanged');
		AcceptScreen;
		If Not Canceled THEN
			BEGIN
			G    := GetNumber(1);
			TimeStep := GetNumber(2);
			For i := 1 to NHeavy DO
				M[i] := GetNumber(2+i);
			ReInit({Ask:} True);
			N_Body_Force(t, Y, YP)
		END; {Not Escaped}
		IF TRev THEN TimeStep := -TimeStep;
		Done;
	  END {With Screen}
	END; {SystemParameters;}

	PROCEDURE ChangeParameters;
	VAR
		 Fini : Boolean;
		 i : Integer;
		 temp : Real;
	BEGIN
		WITH DataScreen DO
		REPEAT
			Init;
			DefineInputPort(0.1, 0.42, 0.01, 0.99);
			LoadLine ('Change Parameters of:');
			LoadLine ('');
			Loadline ('#2 System & Masses');
			FOR i := 1 to NBodies DO LoadLine(ConCat('#2 ',Nametable[i]));
			LoadLine('#2 Done');
			LoadLine ('           Use:');
			LoadLine ('#1 Polar Coordinates');
			LoadLine ('#1 Orbital Elements');
			LoadLine ('');
			LoadLine ('[  OK  ]   [Cancel]');
			IF NOT InputModeCartesian THEN SetRadioButton ('1',2);
			SetRadioButton ('2', Nbodies+2);
			Fini := False;
			AcceptScreen;
			NearestBody := 1; {Until program ready to handle something more}
			IF Canceled THEN Fini := True
			ELSE
			BEGIN
				IF GetRadioButton('1') = 2 THEN InputModeCartesian := False
					ELSE InputModeCartesian := True;
				IF GetRadioButton('2') = 1 THEN SystemParameters
				ELSE IF GetRadioButton('2') = (NBodies + 2) THEN Fini := True
				ELSE
				BEGIN
					i := GetRadioButton('2')-1;
					IF InputModeCartesian THEN
						SetCartesianBodyParameters(i, NearestBody, Fini)
					ELSE
						SetBodyElements(i, NearestBody, {NewBody: }False, Fini);
				END
			END {Not Canceled}
		UNTIL Fini;
		DataScreen.Done;
		MainMenu.Display;
		ClearPlots({Temporary:} False, {ShowInit:} True)
	END; {ChangeParameters}

	FUNCTION GetFName : String;
	VAR
		Extension : String[4];
	BEGIN
		Message ('Press ESC to cancel.'); MainKeys.Clear;
		Extension := '.orb';
			GetFName := OpenFile('*'+Extension);
		SetColor(White);
		MainMenu.Display; MainKeys.Display {In Case Canceled}
	END; {GetFName}

	PROCEDURE ReadSystem(FName : String);
	VAR
		SystemFile : Text;
		Temp : String[80];
		RTemp : Real;
		i : Integer;

		PROCEDURE Swallow(N : Integer);
		VAR
			i : Integer;
			C : Char;
		BEGIN
			For i := 1 to N DO
				Read(SystemFile, C);
		END; {Swallow}

	BEGIN
		IF Fname =  '' THEN Exit;
		Assign(SystemFile, FName);
		ReSet(SystemFile);
		IF Eof(SystemFile) THEN
		BEGIN
			Message('File is empty. Click or ENTER.');
			StaticPause
		END
		ELSE
		BEGIN
			Readln (SystemFile, SysName);
			Swallow (19); Read (SystemFile, NBodies); Read (SystemFile, NHeavy);
			Readln (SystemFile, Temp);
			Swallow (4); Read (SystemFile, G);
			Swallow (13); Readln (SystemFile, TimeStep);
			N2Bodies := 2*NBodies;
			Y.Resize(4*NBodies); YP.Resize(4*NBodies); UnivVel.init(N2Bodies);
			FOR i := 1 to NBodies DO
			BEGIN
			  Swallow (7); Read(SystemFile,NameOrderTable[i]);
			  Swallow(18);
			  IF i <= NHeavy THEN
			  BEGIN Read(SystemFile,M[i]); Swallow (8) END;
			  Readln(SystemFile,NameTable[i]);
			  Swallow (5); Read(SystemFile, Rtemp); Y.Put(2*i-1, Rtemp);
			  Swallow (5); Readln(SystemFile, Rtemp); Y.Put(2*i, Rtemp);
			  Swallow (5); Read(SystemFile, Rtemp); Y.Put(N2Bodies+2*i-1, Rtemp);
			  Swallow (5); Readln(SystemFile, Rtemp); Y.Put(N2Bodies+2*i, Rtemp);
			END; {Read Body Params}
			Swallow (16); Readln(SystemFile, AllNormScale);
			Swallow (22); Read(SystemFile, Rtemp); UnivVel.Put(1, Rtemp);
			Swallow (6); Readln(SystemFile, Rtemp); UnivVel.Put(2, Rtemp);
			ExtendVect(UnivVel);
			Read(SystemFile, Plots); Swallow (33); Readln(SystemFile, Zf);
			Swallow (17); Readln(SystemFile, ShowEnergyBody);
			Swallow (11); Readln(SystemFile, DataBody);
			Swallow (17); Readln(SystemFile, DataCenterBody);
			FOR i := 1 TO 7 DO
			BEGIN
			  Swallow (16); Read(SystemFile, ScaleTable[i]);
			  Swallow (8); Readln(SystemFile, FrameNametable[i]);
			  Swallow (12); Read(SystemFile, CenterTableX[i]);
			  Swallow (5); Readln(SystemFile, CenterTableY[i]);
			  Swallow (6); Readln(SystemFile, Temp);
			  IF Temp[1] = 'T' THEN PlotList[i] := True
				  ELSE PlotList[i] := False
			END; {Read Frame Params};
			Close(SystemFile);
			InitY.Resize(4*NBodies);
			InitY.Equate(1.0, Y);
			FOR i := 1 to NHeavy DO
			BEGIN
				Xh[i].Resize(NBodies); Yh[i].Resize(NBodies);
				Rh_3[i].Resize(NBodies)
			END;
			NBodiesOld := NBodies; NHeavyPrime := NHeavy; NHeavyOld := NHeavy;
			FreeReplayMemory;
			SetPlotsA; ClearPlots (False, True)
		END; {Else System File Not empty}
	END; {ReadSystem(GetFName)}

	FUNCTION PutFName : String;
	VAR
		FName : String[12];
		ConChar : String;
		Extension : String[4];
		F : Text;
		FileExists, WriteOver, Cancel : Boolean;
		Screen : TInputScreen;
	BEGIN
		FName := ' ';
		Extension := '.orb';
		FileExists := False; Cancel := False; WriteOver := False;
		WITH Screen DO
		REPEAT {Until File is new or overwrite OK}
			REPEAT {Until filename is not blank}
				Init;
				LoadLine ('Save system, body, & frame parameters');
				LoadLine ('          in a file.');
				LoadLine ('');
				LoadLine ('If any frame is zoomed out or in,');
				LoadLine ('those scales will be the ones');
				LoadLine ('written to the file.');
				LoadLine ('');
				LoadLine ('The name of the file to write is ');
				LoadLine (ConCat('        "        " ',Extension));
				SetString(1,FName);
				IF FileExists THEN
				BEGIN
					LoadLine ('There is already a file by that name;');
				LoadLine ('');
				LoadLine ('[  OK  ]    [Cancel]  [WriteOver]');
				END
				ELSE
				BEGIN
					LoadLine ('');
					LoadLine ('       [  OK  ]    [Cancel]');
				END;
				DefineInputPort(0.01, 0.5, 0.26, 0.78);
				Message('Type a file name and press <ENTER>');
				AcceptScreen;
				IF Canceled THEN Cancel := True ELSE
				BEGIN
					FName := GetString(1);
					If FileExists THEN
						WriteOver := GetBoolean(4);
					IF Pos('.',FName) > 0 THEN Delete (FName, Pos('.',FName), 12);
					WHILE Pos(' ',FName) > 0 DO Delete (FName, Pos(' ',FName), 1)
				END; {Not Canceled}
				Screen.Done;
			UNTIL Cancel OR (Length (FName) > 0);
			FName := FName + Extension;
			{$I-}
			Assign(F, FName);
			Reset(F);
			{$I+}
			FileExists := (IOResult = 0) and (FName <> '') AND (Not Eof(F));
			IF FileExists and (Not Writeover) THEN
			BEGIN
				Delete (FName, Pos('.',FName), 12);
				Close(F)
			END;
		UNTIL (Not FileExists) OR WriteOver OR Cancel;
		IF Cancel THEN FName := '';
		MainMenu.Display;
		PutFName := FName;
	END; {PutFName}

	PROCEDURE SaveSystem(FName : String);
	VAR
		i, j : Integer;
		SysFile : Text;
		FStr : String[10];
	BEGIN
		IF FName = '' THEN Exit;
		Assign(SysFile, FName);
		ReWrite(SysFile);
		Writeln(SysFile, SysName);
		Writeln(SysFile, 'Number of Bodies = ', NBodies:2,' ',NHeavy,' Massive');
		Writeln(SysFile, 'G = ',G,' Time Step = ',Abs(TimeStep));
		FOR i := 1 to NBodies DO
		BEGIN
		  IF i <= NHeavy THEN
			  Writeln(SysFile,'Body ',i:2,NameOrderTable[i]:3,
				' in order; Mass = ',M[i],' Name = ',NameTable[i])
		  ELSE
			  Writeln(SysFile,'Body ',i:2,NameOrderTable[i]:3,
					' in order; Name = ',NameTable[i]);
		  Writeln(SysFile,'X  = ',Y.Value(2*i-1),' Y  = ',Y.Value(2*i));
		  Writeln(SysFile,'Vx = ',Y.Value(N2Bodies+2*i-1),' Vy = ',Y.Value(N2Bodies+2*i));
		END; {Write Body Params}
		Writeln(SysFile, 'Default Scale = ',AllNormScale);
		Writeln(SysFile, 'C of M Velocity: Vx = ',UnivVel.Value(1),' Vy = ',UnivVel.Value(2));
		Writeln(SysFile, Plots, ' Windows open; X/Y Scale Ratio = ',Zf);
		Writeln(SysFile, 'ShowEnergyBody = ',ShowEnergyBody);
		Writeln(SysFile, 'DataBody = ',DataBody);
		Writeln(SysFile, 'DataCenterBody = ',DataCenterBody);
		j := 1;
		FOR i := 1 TO 7 DO
		BEGIN
		IF FrameUsed[j] = i THEN
		BEGIN
		  Writeln(SysFile, 'Frame ',i,' Scale = ',ScaleUsed[j],' Name = ',FrameNametable[i]);
		  Writeln(SysFile, 'Center: X = ',XCenterUsed[j],' Y = ',YCenterUsed[j]);
		  j := j + 1
		 END
		 ELSE
		 BEGIN
		  Writeln(SysFile, 'Frame ',i,' Scale = ',ScaleTable[i],' Name = ',FrameNametable[i]);
		  Writeln(SysFile, 'Center: X = ',CenterTableX[i],' Y = ',CenterTableY[i]);
		 END; {FrameUsed}
		 Writeln(SysFile, 'Used: ',PlotList[i]);
		END; {Write Frame Params};
		Close(SysFile);
		ClearPlots({Temporary:} False, {ShowInit:} True);
		MainMenu.Display;
{M		ShowCursor}
	END; {SaveSystem}

{------------- Main Menu routine is mostly procedure calls -----------------}
BEGIN {HandleMenu}
	WITH MainMenu DO
	CASE colChosen OF
	 1 : Case rowChosen of
			1 : Help(OpenScreen);
			2 : AboutCups;
			3 : Configuration;
			5 : ReadSystem(GetFName);
			6 : SaveSystem(PutFname);
			7 : BEGIN MainMenu.Done;
              CupsDone; Halt END
		  END; {case rowChosen in Column 1}
	 2 : CASE rowChosen OF
			1 : ClearPlots({Temporary:} False, {ShowInit:} True);
			2 : RestartIt;
			3 : Replay({Temporary:} False);
			4 : StartMany;
			5 : DoSettings;
			6 : BEGIN
					TimeStep := - TimeStep;
					IF TimeStep >= 0.0 THEN Row(2,6,'Reverse Time')
					ELSE Row(2,6,'Time Forward')
				 END;
			7 : ChangeParameters;
			8 : IF MousePresent THEN
				 BEGIN PickBody(0); ClearPlots(False, True) END
				 ELSE Message('Not available without Mouse');
			9 : BEGIN AddBody; RestartIt END;
			10 : IF ThrustBody > 0 THEN
				  BEGIN
					  ThrustAllowed := Not ThrustAllowed;
					  IF ThrustAllowed THEN 
					  BEGIN
					  	 row(2, 10, 'Refuse Thrust');
						 IF Plots <= 2 THEN
						 	Message(ConCat('You may apply thrust to ',
								Nametable[Thrustbody]))
                   ELSE
						 	Message(ConCat('You may apply thrust to ',
								Nametable[Thrustbody],
								'; choose 1 or 2 windows to see thrust data.'))
					  END
					  ELSE Row(2,10, 'Allow Thrust');
					  VThrust := 0.0; HThrust := 0.0;
					  SavVT := VThrust; SavHT := HThrust;
					  ClearPlots(False, True);
					  ShowData(False)
				  END
				  ELSE Message('No body with engines in this system');
		  END; {case rowChosen in Column 2}
	 3 : CASE rowChosen OF
			1 : ShowN(1);
			2 : NextFrame({NoShowData:} False);
			3 : ShowN(2);
			4 : ShowN(4);
			5 : ShowN(6);
			6 : MarkCofMass;
			7 : FullScreen({Change:} True);
			8 : ZoomIn;
			9 : ZoomOut;
			10: BEGIN SetPlotsA;
					If AutoReplay AND ((HistoryCount > 1) OR Wrapped) THEN
						Replay({Temporary:} False)
						ELSE ClearPlots({Temporary:} False, {ShowInit:} True)
				 END;
		  END; {case rowChosen in Column 3}
	 4 : CASE rowChosen OF
			1 : ChangeSystem(SunEMoon);
			2 : ChangeSystem(Solar);
			3 : ChangeSystem(Shuttle);
			4 : ChangeSystem(SunJupComet);
			5 : ChangeSystem(SunJupMoons);
			6 : ChangeSystem(Lagrange);
			7 : ChangeSystem(Binary);
			8 : ChangeSystem(Retrograde);
			9 : CreateSystem;
		  END; {case rowChosen in Column 4}
	 5 : HandleHelpMenu
	END; {case colChosen}
	Debounce
END; {HandleMenu}

VAR
	 WhenKeys, NameKeys, NumKeys, CoordKeys : THotKeys;

PROCEDURE ExitSystem; {Set to Zero all normal parameters}
BEGIN
	CofMNormScale := 0.0; Body1NormScale := 0.0;
	Body2NormScale := 0.0; Body3NormScale := 0.0;
	UnivNormScale := 0.0; DataNormScale := 0.0;
	PoincareNormScale := 0.0; RotateNormScale := 0.0;
	RotNormCenterX := 0.0; RotNormCenterY := 0.0;
	UnivNormCenterX := 0.0; UnivNormCenterY := 0.0;
	RelativeError := 1.0e-6;
	AbsError := 1.0e-6;
	DotSize := 1;
	ShowBodies := True;
	MainMenu.Row(2,10, 'Allow Thrust');
	ThrustAllowed := False
END; {ExitSystem}

PROCEDURE DoSettings;
VAR
	 SetScreen    : TInputScreen;
	 AutoRep, AutoNextRep, Track, Body : Char;
	 TempSt : String[1];
BEGIN {DoSettings}
	WITH SetScreen DO
	BEGIN
		Init;
		DefineInputPort(0.02, 0.88, 0.25, 0.83);
		LoadLine ('                    System Setting Choices');
		LoadLine (Concat('                      Time step {       } ',TimeName));
		LoadLine (Concat('              Largest Time Step {       } ',TimeName));
		LoadLine ('#T Automatic Replay after changing number of plots');
		LoadLine ('#T Automatic Replay when choosing next plots');
		LoadLine ('    Size of Dots marking present body positions  {   }');
		LoadLine ('#T Show body positions');
		LoadLine ('#T Show past track of bodies');
		LoadLine ('Display orbital elements of body {   } about body {   }');
      LoadLine ('             Show energy of body {   }');
		LoadLine ('      Absolute Integration Error {       }');
		LoadLine ('      Relative Integration Error {       }');
		LoadLine ('      [OK]              [Cancel]           [Help]');
      SetHelpScreen(SettingHelp);
		SetNumber(1,TimeStep);
		SetNumber(2,MaxStep);
		SetBoolean(3,AutoReplay);
		SetBoolean(4,AutoNextReplay);
		SetNumber(5,DotSize);
		SetBoolean(6,ShowBodies);
		SetBoolean(7,Tracking);
		SetNumber(8,DataBody);
		SetNumber(9,DataCenterBody);
      SetNumber(10, ShowEnergyBody);
		SetNumber(11,AbsError);
		SetNumber(12,RelativeError);
		SetNumberLimits(1,0.0001,100);
		SetNumberLimits(2,0.0001,100);
		SetNumberLimits(5,1,5);
		SetNumberLimits(8,1,NBodies);
		SetNumberLimits(9,1,NHeavy);
		SetNumberLimits(10,1,NBodies);
		SetNumberLimits(11,1E-10,0.001);
		SetNumberLimits(12,1E-10,0.001);
		Message('Press <ENTER> to change parameters, <ESC> to leave unchanged');
		AcceptScreen;
{M		ShowCursor;}
		If Not Canceled THEN
		BEGIN
			TimeStep := GetNumber(1);
			MaxStep := GetNumber(2);
			AutoReplay := GetBoolean(3);
			AutoNextReplay := GetBoolean(4);
			DotSize := Trunc(GetNumber(5)+0.1);
			ShowBodies := GetBoolean(6);
			Tracking := GetBoolean(7);
			DataBody := Trunc(GetNumber(8));
			DataCenterBody := Trunc(GetNumber(9));
         ShowEnergyBody := Trunc(GetNumber(10));
			AbsError := GetNumber(11);
			RelativeError := GetNumber(12);
         LightEnergyName := Concat(NameTable[ShowEnergyBody],' En');
		END; {Not Escaped}
		Done;
	END; {With Screen}
	MainMenu.Display;
	ClearPlots({Temporary:} False, {ShowInit:} True)
END; {SettingsMenu}
	
Procedure SetMyColor (Color:Word);
BEGIN
	CurrentColor := Color;
	SetColor(Color)
END;

{------- To Put tics on the Vertical Axis only ---------}

Procedure YTics(Yint, TicY : Real);
VAR y, TicSize : Real;
	 Xs,  Ys       :integer;
	 L :string[5];

  PROCEDURE LabelTicY;
  BEGIN
    SelectViewPort(ViewPortNumber);
	 IF  TicY >=100                   THEN L := NumStr(y,4,0);
	 IF (TicY>=10) and (TicY<100)     THEN L := NumStr(y,4,0);
	 IF (TicY>=1) and (TicY<=10)      THEN L := NumStr(y,4,0);
	 IF (TicY>=0.1) and (TicY<1)      THEN L := NumStr(y,4,1);
	 IF (TicY>=0.01) and (TicY<0.1)   THEN L := NumStr(y,4,2);
	 IF (TicY>=0.001) and (TicY<0.01) THEN L := NumStr(y,4,3);
	 Map(GraphScales[ScaleNumber].xmin,y,Xs,Ys);
	 SetTextJustify(RightText,CenterText);
	 OutTextXY(Xs-5,Ys,L);
	 SetTextJustify(LeftText,TopText);
  END;

BEGIN
	with GraphScales[ScaleNumber] do BEGIN
	  TicSize := 0.02*(xmax-xmin);
	  y := Yint;
	  While y <= ymax do BEGIN                    {plotting along y axis}
		 PlotLine(xmin,y,xmin+ticsize,y);
		 LabelTicY;
		 PlotLine(xmax,y,xmax-ticsize,y);
		 y := y + ticY;
		 END;  {while}
	 END {With}
END; {YTics}
{**** Define my own scaling routine to avoid unwanted legends ***}

PROCEDURE MyAutoScaling (sNum : integer; VAR x,y : DVector);
VAR
	min,max,Tick,CInt : point2D;
	n : integer;
BEGIN
	x.MinMax(n,min[1],n,max[1]);
	y.MinMax(n,min[2],n,max[2]);
	FOR n := 1 TO 2 DO
		IF (0<=max[n]) AND (0>=min[n]) THEN CInt[n]:=0 ELSE CInt[n]:=min[n];
	DefineScale(snum,min[1],max[1],min[2],max[2]);
	PlotLine(CInt[1],Min[2],CInt[1],Max[2]);        { Plotting y axis }
	PlotLine(Min[1],CInt[2],Max[1],CInt[2]);         { Plotting x axis }
END;

PROCEDURE SetUpRunKeys;
BEGIN
	WITH RunKeys DO BEGIN
		Init(3);
		key[1] := 'F3-Stop';
		key[2] := 'F5-Clear&Cont';
		key[3] := 'F10-Menu'
	END
END; {SetUpRunKeys}

PROCEDURE DoRunKeys (key : byte);
BEGIN
	CASE key OF
	 1, 3 : BEGIN
				Running := False;
				MainKeys.Display;
			  END;
	 2 : BEGIN Running := False; ClearIt := True END;
	END; {case}
	Debounce
END; {DoRunKeys}

PROCEDURE MenuInit;
BEGIN {Always set Version to that of the unit with the largest Version number}
	CreatingSystem := False;
	AddingBody := False;
	UseManyHelp := False;
	InputModeCartesian := True;
	If MyVersion > Version THEN Version := MyVersion
END;

END.
