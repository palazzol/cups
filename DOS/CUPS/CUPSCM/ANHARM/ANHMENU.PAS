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
	 {**           Unit AnhMenu.Pas               **}
	 {**           Program AnHarm.Pas             **}
	 {**      Non-Linear Oscillator Behaviour     **}
	 {**        Written by Bruce Hawkins          **}
	 {**           November 28, 1991              **}
	 {**        Last change:  2/15/95             **}
	 {**             Version 1.00                 **}
	 {**                                          **}
	 {**      B. Hawkins Copyright @ 1991         **}
	 {**             CUPS Project                 **}
	 {**                                          **}
	 {**     Uses 12/94 MUPPET & CUPS Utilities   **}
	 {**********************************************}
	 {**********************************************}

{         Contains the menus and hotkeys and handling routines      }

UNIT AnhMenu;

INTERFACE

USES Graph, CupsProc, CupsGui;

{$IFDEF CPU87}
	TYPE
		REAL = DOUBLE;
{$ELSE}
	TYPE
		SINGLE   = REAL;
		DOUBLE   = REAL;
		EXTENDED = REAL;
		COMP     = REAL;
{$ENDIF}

CONST
	Version : Real = 1.00;
	AlwaysContinuous : Boolean = False;
	PeriodPause : Boolean = True;
	FreezeScales : Boolean = False; FreezeTimeScale : Boolean = False;
	LeftPort = 1; UpperLeft = 2;  AllPort = 19;
	LowerLeft =3; RightPort = 10;
	UpperRight = 8; MidRight = 17; ClearURY = 31;
	ClearLLX = 12; ClearLY = 13; {Port Numbers needed by Fourier}
	ClearLMidX = 15; ClearLUY = 30;
	Plots : Integer = 2;
	DemoPoincare : Boolean = False;
	InTutorial : Boolean = False;
	HistFileOpen : Boolean = False;
	{Resonance Curve Parameters}
	NPerCurves : Integer = 5;
	ESteps : Integer = 25;
	DeltaE : Real = 0.002;
	EOne : Real = 0.002;
	BaseVal : Real = 0.4; DeltaW : Real = 0.08;
	NResPtsInit = 30; NResCurves : Integer = 2;
	DBeg : Real = 0.1; DInc : Real = 0.1;
	Omega0 : Real = 1.0; OmegaRange : Real = 0.5;
	OmegaRangeDown : Real = 0.5; OmegaRangeUp : Real = 0.5;
	ResStepSize : Real=1.0;

	{Define Scale Numbers}
	VSNum = 1;
	PhaseSNum = 2; PositionSNum = 3;
	PoincareSNum = 4; ReturnSNum = 5; PeriodSnum = 6;
	SelectSNum = 7; TimeSNum = 8; VelSNum = 9; ResonanceSnum = 10;
	Exact      : boolean = True;

	Animating  : boolean = True;
	CirclePresent : Boolean = False;
	TimePlotInverted : Boolean = False;
	ReadSearch : Boolean = False;
	MasterDelayTime : Integer = 100;
	DelayTime : Integer = 50;
	BothSides : boolean = False;

   NewEnergyScale : Boolean = False;
	Continuous : boolean = False;
	TimeLimit  : Boolean = False;
	Searching  : Boolean = False;
	Resonating : Boolean = False;
	PhasePicked : Boolean = False;
	DoErase    : Boolean = True;
	ShowMenu   : Boolean = True;
	PlotTime   : boolean = True;
	PlotVel    : boolean = False;
	PlotPhase  : boolean = True;
	PlotPoincare : boolean = False;
	PlotReturn : boolean = False;
	HidePoincare : Boolean = False;

	HaveResonances: boolean = False;
	HavePeriods: boolean = False; 
	HavePerTabSpace : Boolean = False;
	HaveResTabSpace : Boolean = False;
	NColor : Integer = 1;
	ShowPeriods : Integer = 2;
	x0Old : Real = 0.01; v0Old : Real = 0.0; t0Old : Real = 0.0;


VAR
	MainMenu, HelpMenu : TMenu;
	ShoKeys, WallKeys, PendKeys, VDPKeys, CubKeys,
	  HelpKeys, ZoomKeys, FourKeys, FlipKeys   : THotKeys;

	VPort, PhasePort, PositionPort, VelocityPort, ButtonPort, WallPort,
		PoincarePort, ReturnPort, TimePort : Integer;
	WallSNum : Integer;
	NResPts : Array[1..3] of Integer;

	CurrentColor : Integer;
	TempColor : Integer;
	PScale, EScale, FScale, AScale : Real;
	Plus : Boolean;

	HistFile : Text;

PROCEDURE SelectMyViewport(Port:Integer);
PROCEDURE OpenMyViewPort (Port : Integer);
PROCEDURE CloseMyViewPort (Port : Integer);
PROCEDURE SetMyColor (Color:Word);
PROCEDURE MildAnnounce(text : string);
PROCEDURE DrawCircle(x, y: Real; r: Integer; thisColor: Word);
PROCEDURE MyAutoScaling (sNum : integer; VAR x,y : DVector);
PROCEDURE SetUpMenus;
PROCEDURE SetUpSystemKeys;
PROCEDURE SetUpZoomKeys;  {Function keys to choose graphs to be displayed}
PROCEDURE SetUpFlipKeys;
PROCEDURE ChooseWalls;    {Switches to spring between walls simulation}
PROCEDURE ChoosePendulum; {Switches to pendulum simulation}
Procedure ToggleExactAccel;{Switches from exact <--> approximate Accel}
Procedure ChooseCubic;
Procedure ChooseSHO;
Procedure ChooseVanDePol;
PROCEDURE NewSystem(Clear : Boolean);
PROCEDURE ChooseSystem(Clear : Boolean);
Procedure DisplayMainMenu;
PROCEDURE HandleMenu;     {Main menu routine does all the work}
PROCEDURE DoSystemKeys;
PROCEDURE DoZoomKeys (key : byte);
PROCEDURE DoFlipKeys (key : byte; VAR IBegin, DTime : Integer;
			 VAR Done, Pausing : Boolean);
PROCEDURE SetPlots; {Called to place the different graphs in windows as requested}
PROCEDURE Debounce;
PROCEDURE SetAllFalse;
PROCEDURE ShowXandV;
PROCEDURE ShowXVP;
PROCEDURE ShowXandPhase;
PROCEDURE SetUpPorts;
Procedure SelectLeftPort;
Procedure CloseAllPort;
Procedure OpenAllPort;
PROCEDURE ShowPoinAlone;   {Poincare Map as only graph}
PROCEDURE displayChoice;  {Displays parameter values}
PROCEDURE displayDissip;  {More parameter values (not always displayed}
PROCEDURE RunInstruct;
PROCEDURE ClearDissipPort;
PROCEDURE ClearLegend;
PROCEDURE displayPeriod(t : Real); {Displays result for period of motion}
FUNCTION AxisRound(TickSize: Real; Sig : Integer): Real;
PROCEDURE DisplayPotential (ShowBoth, HoldScale: Boolean); {Displays them}
PROCEDURE DrawSprings (x : Real; Color, MassColor : Integer); {Used by animation}
PROCEDURE DrawPendulum (Theta : Real; TempColor : Integer);
PROCEDURE DrawGeneral(xsize : Real; Color : Integer);
PROCEDURE DrawSystem;    {Displays whichever system is being simulated}
PROCEDURE ClearPhasePort; {Clears all the graphs}
PROCEDURE ShowPeriod(GType : Char);     {Displays the results of period vs. energy calculation}
PROCEDURE ShowFour(Return : Boolean);        {Displays all four graphs}
PROCEDURE OpenTwoPoincare;

IMPLEMENTATION

{****************************Internal Procedures***************************
Procedure ShowPhaseAlone;  Sets up phase plot as only graph 
Procedure ShowTimeAlone;   x against time as only graph 
Procedure ShowReturnAlone;  Return Map: v(phi=0) vs. previous v(phi=0) only
Procedure ShowTwo;         Displays chosen two of above 
Within HandleMenu:
  Procedure ToggleBothSides;  Displays trajectories with same energies in both sides of double well 
  Procedure SelectEnergyScale;    Uses a slider to select maximum energy graphed
Procedure DrawWalls;  Draws the two walls in the correct position
  PROCEDURE OneWall (Position : Real; Above : Integer);
**************************************************************************}

USES Dos, Crt, Cups, CupsFunc, CupsMupp, CupsGrph, AnhPhy, AnhHelp, AnhTutor;

CONST
	LastPlots  : Integer = 2;
	Zero : Integer = 0;
	Stretched  : boolean = True;
	{Define ViewPort Numbers}
	LeftUpperLeft = 4; RightUpperLeft = 5;
	LeftLowerLeft = 6; RightLowerLeft = 7;
	LowerRight = 9;
	SwapPort = 20;
	ClearLRY = 11; ClearLRX = 14;
	ClearLMidY = 16;
	SomePort = 18; {AllPort = 19; }
	ClearPeriod = 21; ClearDissip = 22; ClearElegent = 23;
	GetEnergy = 24; GetR = 25; GetDrive = 26; GetF = 27;
	ClearUR = 28; PLegend = 29; {ClearURY := 31; is largest}
	AnimString : String[3] = 'Off';

	BlankPoincare : Boolean = False;

VAR
	MyViews : Array [21..40] of View;

Procedure DefineMyViewPort (Port : Integer; VPX1,VPX2,VPY1,VPY2: real);
BEGIN
	IF Port < SwapPort THEN DefineViewPort (Port, VPX1,VPX2,VPY1,VPY2)
	  ELSE
	  BEGIN
	  DefineViewPort (SwapPort, VPX1,VPX2,VPY1,VPY2);
	  MyViews[Port] := Views[SwapPort]
	  END
END; {DefineMyViewPort}

Procedure SelectMyViewport(Port:Integer);
BEGIN
	IF Port < SwapPort THEN SelectViewPort (Port)
	  ELSE
	  BEGIN
	  Views[SwapPort] := MyViews[Port];
	  SelectViewPort (SwapPort)
	  END
END; {SelectMyViewPort}

Procedure OpenMyViewPort (Port : Integer);
BEGIN
	IF Port < SwapPort THEN OpenViewPort (Port)
	  ELSE
	  BEGIN
	  Views[SwapPort] := MyViews[Port];
	  OpenViewPort (SwapPort)
	  END
END; {OpenMyViewPort}

Procedure CloseMyViewPort (Port : Integer);
BEGIN
	IF Port < SwapPort THEN
	  BEGIN
		 CloseViewPort (Port);
		 SelectViewPort (Port)
	  END
	  ELSE
	  BEGIN
		  Views[SwapPort] := MyViews[Port];
		  CloseViewPort (SwapPort);
		  SelectViewPort (SwapPort) {Because we often want to clear a port and use it}
	  END
END; {CloseMyViewPort}

Procedure SetMyColor (Color:Word);
BEGIN
	CurrentColor := Color;
	SetColor(Color)
END;

PROCEDURE MildAnnounce(text : string);
VAR
   dx,dy,cx,cy,a,n : integer;
   image           : TImage;
   z               : char;
   envir           : TEnvironment;
BEGIN
	envir.Save;
{	Message('Click or Press ENTER, then follow instructions.');  }
	envir.Standardize;
	SetFillStyle(SolidFill,Brown);
	dx := TextWidth(text)+13;
   dy := RowHt + 3;
   cx := (GetMaxX-dx) DIV 2;
   cy := (GetMaxY-dy) DIV 2;
   Image.Get(cx,cy,cx+dx,cy+dy);
   HideMouse;
   Bar3D(cx,cy+3,cx+dx-3,cy+dy,3,true);
   ShowMouse;
	ClearMKBuffers;
	SetColor(white);
	HideMouse;
	OutTextXY(cx+6,cy+8,Text);
	ShowMouse;
	ClearMKBuffers;
	DynamicPause;
	image.put(cx,cy);
	envir.Reset;
END;

PROCEDURE DrawCircle(x, y: Real; r: Integer; thisColor: Word);
VAR
	xSc, ySc, x2Sc, y2Sc: integer;
BEGIN
	Map(x,y, xsc,ysc);
	SetColor(ThisColor);
	SetFillStyle(solidfill,ThisColor);
	FillEllipse(xSc,ySc,r,r);
END;

{------- To Put tics on the Vertical Axis only ---------}

Procedure YTics(Yint, TicY : Real);
VAR y, TicSize : Real;
	 Xs,  Ys       :integer;
	 L :string[5];

  PROCEDURE LabelTicY;
	 BEGIN
	 WITH Views[ViewPortNumber] do
		BEGIN
		  SetViewPort(Vx1,Vy1,Vx2,Vy2,False);
		END;
	 IF  TicY >=100                   THEN Str(y:4:0,L);
	 IF (TicY>=10) and (TicY<100)     THEN Str(y:4:0,L);
	 IF (TicY>=1) and (TicY<=10)      THEN Str(y:4:0,L);
	 IF (TicY>=0.1) and (TicY<1)      THEN Str(y:4:1,L);
	 IF (TicY>=0.01) and (TicY<0.1)   THEN Str(y:4:2,L);
	 IF (TicY>=0.001) and (TicY<0.01) THEN Str(y:4:3,L);
	 Map(GraphScales[ScaleNumber].xmin,y,Xs,Ys);
	 SetTextJustify(RightText,CenterText);
	 OutTextXY(Xs-5,Ys,L);
	 SetTextJustify(LeftText,TopText);
  END;

BEGIN
	SetTextStyle(DefaultFont,HorizDir,1);
	with GraphScales[ScaleNumber] do
	BEGIN
	  TicSize := 0.02*(xmax-xmin);
	  y := Yint;
	  While y <= ymax do
	  BEGIN                    {plotting along y axis}
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
END; {MyAutoScaling}

{-----         Display one, two, or four graphs chosen by user         ------}
{-----   choosing is done in following routines, this sets the windows ------}

Procedure SetPlots;
VAR i : Integer;
BEGIN
	i := 0;
	IF PlotPoincare AND (Plots > 1)
		AND (Not InTutorial) AND (F0overM > 0.0) AND (Not ReadSearch) THEN
		MildAnnounce ('Be aware that Poincar‚ Maps are fastest without another plot.');
	Case Plots of
	 1 : BEGIN
			  IF PlotPhase THEN BEGIN PhasePort := LeftPort; Exit END;
			  IF PlotTime  THEN BEGIN TimePort  := LeftPort; Exit END;
			  IF PlotVel  THEN BEGIN VelocityPort := LeftPort; Exit END;
			  IF PlotPoincare THEN BEGIN PoincarePort  := LeftPort; Exit END;
			  IF PlotReturn THEN ReturnPort  := LeftPort
		  END; {Case 1}
	 2 : BEGIN
			  IF PlotPhase THEN BEGIN PhasePort := UpperLeft; i := i + 1 END;
			  IF PlotVel THEN
			  BEGIN
				  VelocityPort  := UpperLeft;
				  i := i + 1;
				  IF i = Plots THEN BEGIN VelocityPort := LowerLeft; Exit END
			  END; {PlotVel}
			  IF PlotTime  THEN
			  BEGIN
				  TimePort  := UpperLeft;
				  i := i + 1;
				  IF i = Plots THEN BEGIN TimePort := LowerLeft; Exit END
			  END; {PlotTime}
			  IF PlotPoincare THEN BEGIN 
				  PoincarePort  := UpperLeft; 
				  i := i + 1;
				  IF i = Plots THEN BEGIN PoincarePort := LowerLeft; Exit END
				  END; {PlotPoincare}
			  IF PlotReturn THEN ReturnPort  := LowerLeft
		  END; {Case 2}
	 4 : BEGIN
			  PhasePort := LeftUpperLeft;
			  TimePort  := LeftLowerLeft; 
			  PoincarePort  := RightLowerLeft;
			  IF PlotReturn THEN ReturnPort  := RightUpperLeft
			  ELSE VelocityPort := RightUpperLeft
		  END; {Case 4}
	END; {Cases}
END; {SetPlots}

PROCEDURE Debounce;
BEGIN
	REPEAT CheckForEvents UNTIL Not Event.MouseClicked;
	Event.keyPressed := False
END; {Debounce}

{------------ Set up Phase Diagram (v vs. x) as only graph -------------}

PROCEDURE SetAllFalse;
BEGIN
	PlotPhase := False;
	PlotTime  := False;
	PlotVel := False;
	PlotPoincare := False;
	PlotReturn := False;
	BlankPoincare := False
END; {SetAllFalse}

PROCEDURE ShowXandV;
BEGIN
	SetAllFalse;
	Plots := 2;
	PlotTime := True;
	PlotVel := True;
	SetPlots;
	ClearPhasePort;
END; {ShowXandV}

PROCEDURE ShowXVP;
BEGIN
	SetAllFalse;
	Plots := 4;
	PlotTime := True;
	PlotVel := True;
	PlotPhase := True;
	PlotPoincare := True; {Canceled if no drive}
	SetPlots;
	ClearPhasePort;
END; {ShowXVP}

PROCEDURE ShowXandPhase;
BEGIN
	SetAllFalse;
	Plots := 2;
	PlotTime := True;
	PlotPhase := True;
	SetPlots;
	ClearPhasePort;
END; {ShowXandV}

Procedure ShowPhaseAlone;
BEGIN
	Plots := 1;
	SetAllFalse;
	PlotPhase := True;
	SetPlots;
	ClearPhasePort
END; {ShowPhaseAlone}

{------------ Set up position against time plot as only graph -------------}

Procedure ShowTimeAlone;
BEGIN
	Plots := 1;
	SetAllFalse;
	PlotTime := True;
	SetPlots;
	ClearPhasePort
END; {ShowTimeAlone}

Procedure ShowVelAlone;
BEGIN
	Plots := 1;
	SetAllFalse;
	PlotVel := True;
	SetPlots;
	ClearPhasePort
END; {ShowVelAlone}

PROCEDURE FailMess;
BEGIN
	MildAnnounce('Not a reasonable choice if Oscillator isn''t driven');
END; {FailMess}

{----- Set up Poincare Map (v vs. x at zero drive phase) as only graph -----}

Procedure ShowPoinAlone;
BEGIN
	IF F0overM = 0.0 THEN
		BEGIN FailMess; StaticPause; MainMenu.Display END
	ELSE BEGIN
		Plots := 1;
		SetAllFalse;
		PlotPoincare := True;
		SetPlots;
		ClearPhasePort
	END {ELSE: Drive not zero}
END; {ShowPoinAlone}

{----- Set up Return Map (v at zero phase vs. previous v at zero phase) -----}

Procedure ShowReturnAlone;
BEGIN
	IF F0overM = 0.0 THEN
		BEGIN FailMess; StaticPause; MainMenu.Display END
	ELSE BEGIN
		Plots := 1;
		SetAllFalse;
		PlotReturn := True;
		SetPlots;
		ClearPhasePort
	END {ELSE: Drive not zero}
END; {ShowReturnAlone}

{-------------- Select any two graphs and set up to display them  ------------}


Procedure ShowTwo;
VAR key : Byte;
BEGIN
	SetAllFalse;
	SetupZoomKeys;
	Plots := 0;             {Set up default choice}
	REPEAT
		CheckForEvents;
		IF ZoomKeys.Pressed(key) THEN DoZoomKeys(key);
		IF PlotPoincare AND (F0overM <= 0.0) THEN
			BEGIN PlotPoincare := False; Plots := Plots - 1; FailMess END;
		IF PlotReturn AND (F0overM <= 0.0) THEN
			BEGIN PlotReturn := False; Plots := Plots - 1; FailMess END;
	UNTIL Plots = 2;
	SetPlots;
	HideMouse;
	SetUpSystemKeys; MainMenu.Display;
	CloseMyViewPort(ClearUR);
	DrawSystem;
	ShowMouse;
	ClearPhasePort
END; {ShowTwo}

{--------------- Set up all four graphs --------------------}

PROCEDURE ShowFour(Return : Boolean);

	PROCEDURE DoFourKeys (key : byte);
	BEGIN
		CASE key OF
		 1 : HandleHelpMenu;
		 2 : PlotVel := True;
		 3 : PlotReturn := True
		END; {case}
		Debounce
	END; {DoFourKeys}
	
	PROCEDURE SetUpFourKeys;
	BEGIN
		Message('Choice of including v vs. t or Return Map'
				  +' with the other three graphs.');
		WITH FourKeys DO
		BEGIN
			Init(3);
			key[1] := 'F1-Help';
			Key[2] := 'F2-v vs. t';
			key[3] := 'F3-Return Map'
		END;
		FourKeys.Display;
	END; {SetUpFourKeys}

VAR key : Byte;
BEGIN {ShowFour}
		Plots := 4;
		PlotPhase := True;
		PlotTime := True;
		IF f0overM <> 0.0 THEN
		BEGIN
			PlotPoincare := True;
			PlotVel := False
		END
		ELSE
		BEGIN
			PlotPoincare := False;
			PlotVel := True;
			MildAnnounce ('No Poincar‚ or Return Maps when system undriven.');
		END;
		PlotReturn := Return;
		If (Not Return) AND (F0overM <> 0.0) THEN
		BEGIN
			SetUpFourKeys;
			REPEAT CheckForEvents UNTIL FourKeys.Pressed(key);
			DoFourKeys(key);
		END; {Not Return}
		HideMouse;
		SetPlots;
		SetUpSystemKeys;
		MainMenu.Display;
		ClearPhasePort;
		ShowMouse
END; {ShowFour}

{--------------- Sets up the menu display contents --------------}

CONST
	BothWellStr1 = 'Plot in Both Wells';
	BothWellStr2 = 'Plot in One Well';
	BothWellStr : String[25] = 'Plot in Both Wells';

PROCEDURE SetUpAMenu(VAR Menu : TMenu);
BEGIN
	Menu.Init;
	WITH Menu DO
	BEGIN
	  column(1, 'Files');
		 row(1, 1, 'About ANHARM');
		 row(1, 2, 'About CUPS');
		 row(1, 3, 'Configure');
		 row(1, 4, '------------------');
		 row(1, 5, 'Save Parameters');
		 row(1, 6, 'Read Parameters');
		 row(1, 7, 'Save Poincar‚ Maps');
		 row(1, 8, 'Read Poincar‚ Maps');
		 row(1, 9, 'Compare Two P Maps');
		 row(1, 10, 'Read Search file');
		 Row(1, 11, 'Save History');
		 Row(1, 12, 'Save Resonance Plot');
		 Row(1, 13, 'Read Resonance Plot');
		 row(1, 14, '------------------');
		 row(1, 15, 'Exit Program (Alt-x)');
	  column(2, 'Choices');
		 row(2, 1, 'Clear');
		 row(2, 2, 'Clear & Rescale');
		 row(2, 3, BothWellStr);
		 row(2, 4, 'Animate '+AnimString);
		 row(2, 5, 'Change Potential Range');
		 row(2, 6, '----------------------');
		 row(2, 7, 'Plot Period');
		 row(2, 8, 'Plot Resonance');
		 row(2, 9, 'Frequency Analysis');
		 row(2, 10, 'Poincar‚ Map Movie');
		 row(2, 11, 'Return Map Movie');
		 row(2, 12, 'Demonstrate Phase Plot');
	  column(3, 'Display');
		 row(3, 1, 'x vs. t');
		 row(3, 2, 'v vs. t');
		 row(3, 3, 'v vs. x (Phase Diagram)');
		 row(3, 4, 'Poincar‚ Map');
		 row(3, 5, 'Return Map');
		 row(3, 6, '-----------------------');
		 row(3, 7, 'Display Two Graphs');
		 row(3, 8, 'Display Four Graphs');
		 row(3, 9, '------------------------');
		 row(3, 10, 'Display Period Graph');
		 row(3, 11, 'Display Resonance Graph');
		 row(3, 12, '-----------------------');
		 row(3, 13, 'Display Options');
	  column(4, 'Forces');
		 row(4, 1, 'Asymmetric');
		 row(4, 2, 'Cubic (Duffing)');
		 row(4, 3, 'Pendulum');
		 row(4, 4, 'Simple Harmonic');
		 row(4, 5,'Two Walls');
		 row(4, 6, 'Van Der Pol Oscillator');
		 row(4, 7, '----------------------------');
		 row(4, 8, 'Explain Current Force');
		 row(4, 9, 'Drive, Damping, & Parameters');
		 row(4, 10, 'Interesting Parameters');
		 row(4, 11, 'Search Parameter Space');
		 row(4, 12, 'Series Approximation');
	  column(5, 'HELP!!');
		 row(5, 1, 'Help');
		 row(5, 2, 'Tutorial');
	  RowActivate(1,4,False);
	  RowActivate(1,12,False);
	  RowActivate(1,14,False);
	  RowActivate(2,6,False);
	  RowActivate(3,6,False);
	  RowActivate(3,9,False);
	  RowActivate(3,12,False);
	  RowActivate(4,7,False)
	END;
	ShowMouse
END; {SetUpAMenu}

PROCEDURE SetUpMenus;
BEGIN
	SetUpAMenu(MainMenu);
	SetUpAMenu(HelpMenu);
	HelpMenu.row(1,12, '');
	HelpMenu.row(1, 1, 'How to RUN');
	HelpMenu.column(6,'Quit Help');
	HelpMenu.RowActivate(1,13,False);
	HelpMenu.RowActivate(4, 8,False);
END; {SetUpMenus}

{----------- Set up the various function key displays, ----------}
{-----------  Each is followed by its action routine   ----------}

{------ This one changes the wall parameters: separation and intial energy ---}

PROCEDURE SetUpWallKeys;
BEGIN
	IF ReadSearch THEN Exit;
	WITH WallKeys DO
	BEGIN
		Init(6);
		key[1] := 'F1-Help';
		Key[2] := 'F2-Wall Up';
		key[3] := 'F4-Wall Down';
		key[4] := 'F6-More Ener';
		key[5] := 'F8-Less Ener';
		key[6] := 'F10-Menu';
  END;
	WallKeys.Display;
END; {SetUpWallKeys}

PROCEDURE DoWallKeys (key : byte; ShowM : Boolean);
BEGIN
	CASE key OF
	 1 : HandleHelpMenu;
	 2 : BEGIN HalfWallSep:=HalfWallSep+0.01; FindPotential(True);DisplayChoice;
			DrawSystem;DisplayPotential(False, False) END;
	 3 : BEGIN HalfWallSep:=HalfWallSep - 0.01; DrawSystem;DisplayChoice;
			FindPotential(True); DisplayPotential(False,False) END;
	 4 : BEGIN Energy := Energy + 0.001; DisplayChoice END;
	 5 : BEGIN Energy := Energy - 0.001; DisplayChoice END;
	 6 : BEGIN CheckForEvents; IF MainMenu.Chosen THEN HandleMenu END
	END; {case}
	IF HalfWallSep < 0.01 THEN HalfWallSep := 0.01;
	IF Energy < 0.001 THEN Energy := 0.0;
	IF ShowM THEN
	CASE Key OF
	 4,5 : Message ('Press enter to plot orbit for this energy');
	 1,2,3,6 : MainMenu.Display
	END; {Case}
	Debounce
END; {DoWallKeys}

PROCEDURE SetUpAsKeys;
BEGIN
	IF ReadSearch THEN Exit;
	WITH WallKeys DO
	BEGIN
		Init(6);
		key[1] := 'F1-Help';
		Key[2] := 'F2-A2 Up';
		key[3] := 'F4-A2 Down';
		key[4] := 'F6-More Ener';
		key[5] := 'F8-Less Ener';
		key[6] := 'F10-Menu';
  END;
	WallKeys.Display;
END; {SetUpWallKeys}

PROCEDURE DoAsKeys (key : byte; ShowM : Boolean);
BEGIN
	CASE key OF
	 1 : HandleHelpMenu;
	 2 : BEGIN AsCoef3 := AsCoef3+0.05; FindPotential(True);DisplayChoice;
			DrawSystem;DisplayPotential(False, False) END;
	 3 : BEGIN AsCoef3 := AsCoef3  - 0.05; DrawSystem;DisplayChoice;
			FindPotential(True); DisplayPotential(False,False) END;
	 4 : BEGIN Energy := Energy + 0.001; DisplayChoice END;
	 5 : BEGIN Energy := Energy - 0.001; DisplayChoice END;
	 6 : BEGIN CheckForEvents; IF MainMenu.Chosen THEN HandleMenu END
	END; {case}
	IF HalfWallSep < 0.01 THEN HalfWallSep := 0.01;
	IF Energy < 0.001 THEN Energy := 0.0;
	IF ShowM THEN
	CASE Key OF
	 4,5 : Message ('Press enter to plot orbit for this energy');
	 1,2,3,6 : MainMenu.Display
	END; {Case}
	Debounce
END; {DoWallKeys}

{------------- These change the pendulum parameters: g and energy --------}

PROCEDURE SetUpPendKeys;
BEGIN
	IF ReadSearch THEN Exit;
	WITH PendKeys DO
	BEGIN
		Init(6);
		key[1] := 'F1-Help';
		Key[2] := 'F2-More g';
		key[3] := 'F4-Less g';
		key[4] := 'F6-More Ener';
		key[5] := 'F8-Less Ener';
		key[6] := 'F10-Menu';
  END;
	PendKeys.Display;
END; {SetUpPendKeys}

PROCEDURE DoPendKeys (key : byte);
BEGIN
	CASE key OF
	 1 : HandleHelpMenu;
	 2 : BEGIN gOverL := gOverL + 1.0; DisplayChoice;
			FindPotential(True); DisplayPotential(False,False) END;
	 3 : BEGIN gOverL := gOverL - 1.0; DisplayChoice;
			FindPotential(True); DisplayPotential(False,False) END;
	 4 : BEGIN Energy := Energy + 0.5; DisplayChoice END;
	 5 : BEGIN Energy := Energy - 0.5; DisplayChoice END;
	 6 : BEGIN CheckForEvents; IF MainMenu.Chosen THEN HandleMenu END
	END; {case}
	Debounce
END; {DoPendKeys}

PROCEDURE SetUpVDPKeys;
BEGIN
	IF ReadSearch THEN Exit;
	WITH VDPKeys DO
	BEGIN
		Init(6);
		key[1] := 'F1-Help';
		Key[2] := 'F2-More D';
		key[3] := 'F4-Less D';
		key[4] := 'F6-Inc. xi';
		key[5] := 'F8-Dec. xi';
		key[6] := 'F10-Menu';
  END;
	VDPKeys.Display;
END; {SetUpVDPKeys}

PROCEDURE DoVDPKeys (key : byte);
BEGIN
	CASE key OF
	 1 : HandleHelpMenu;
	 2 : BEGIN DoverM := DoverM + 0.2; DisplayChoice;
			FindPotential(True); DisplayPotential(False,False) END;
	 3 : BEGIN DoverM := DoverM - 0.2; DisplayChoice;
			FindPotential(True); DisplayPotential(False,False) END;
	 4 : BEGIN Energy := Energy + 0.2; DisplayChoice END;
	 5 : BEGIN Energy := Energy - 0.2; DisplayChoice END;
	 6 : BEGIN CheckForEvents; IF MainMenu.Chosen THEN HandleMenu END
	END; {case}
	Debounce
END; {DoVDPKeys}

PROCEDURE SetUpSHOKeys;
BEGIN
	IF ReadSearch THEN Exit;
	WITH SHOKeys DO
	BEGIN
		Init(6);
		key[1] := 'F1-Help';
		Key[2] := 'F2-More k';
		key[3] := 'F4-Less k';
		key[4] := 'F6-More Ener';
		key[5] := 'F8-Less Ener';
		key[6] := 'F10-Menu';
  END;
	SHOKeys.Display;
END; {SetUpSHOKeys}

PROCEDURE DoSHOKeys (key : byte);
BEGIN
	CASE key OF
	 1 : HandleHelpMenu;
	 2 : BEGIN koverM := koverM + 0.1; DisplayChoice;
			FindPotential(True); DisplayPotential(False,False) END;
	 3 : BEGIN koverM := koverM - 0.1; DisplayChoice;
			FindPotential(True); DisplayPotential(False,False) END;
	 4 : BEGIN Energy := Energy + 0.001; DisplayChoice END;
	 5 : BEGIN Energy := Energy - 0.001; DisplayChoice END;
	 6 : BEGIN CheckForEvents; IF MainMenu.Chosen THEN HandleMenu END
	END; {case}
	IF Energy < 0.001 THEN Energy := 0.001;
	IF KoverM < 0.0 THEN KoverM := 0.0;
	Debounce
END; {DoSHOKeys}

PROCEDURE SetUpCubKeys;
BEGIN
	IF ReadSearch THEN Exit;
	WITH CubKeys DO
	BEGIN
		Init(6);
		key[1] := 'F1-Help';
		Key[2] := 'F2-More C3';
		key[3] := 'F4-Less C3';
		key[4] := 'F6-More Ener';
		key[5] := 'F8-Less Ener';
		key[6] := 'F10-Menu';
  END;
	CubKeys.Display;
END; {SetUpCubKeys}

PROCEDURE DoCubKeys (key : byte);
BEGIN
	CASE key OF
	 1 : HandleHelpMenu;
	 2 : BEGIN C3overM := C3overM + 0.1; DisplayChoice;
			FindPotential(True); DisplayPotential(False,False) END;
	 3 : BEGIN C3overM := C3overM - 0.1; DisplayChoice;
			FindPotential(True); DisplayPotential(False,False) END;
	 4 : BEGIN Energy := Energy + 0.001; DisplayChoice END;
	 5 : BEGIN Energy := Energy - 0.001; DisplayChoice END;
	 6 : BEGIN CheckForEvents; IF MainMenu.Chosen THEN HandleMenu END
	END; {case}
	IF Energy < 0.001 THEN Energy := 0.001;
	IF C3overM < 0.0 THEN C3overM := 0.0;
	Debounce
END; {DoCubKeys}

PROCEDURE SetUpSystemKeys;
BEGIN
	CASE WhichForce OF
		Walls : SetupWallKeys;
		Asymmetric : SetupAsKeys;
		Pendulum : SetUpPendKeys;
		VanDePol : SetUpVDPKeys;
		SHO  : SetUpSHOKeys;
		Cubic : SetUpCubKeys
	END; {Case}
END; {SetUpSystemKeys}

PROCEDURE DoSystemKeys;
VAR Key : Byte;
BEGIN
	Case WhichForce of
		Walls : IF WallKeys.Pressed(key) THEN
			BEGIN DoWallKeys(key, True); RunInstruct END;
		Asymmetric : IF WallKeys.Pressed(key) THEN
			BEGIN DoAsKeys(key, True); RunInstruct END;
		Pendulum : IF PendKeys.Pressed(key) THEN
			BEGIN DoPendKeys(key); RunInstruct END;
		VanDePol : IF VDPKeys.Pressed(key) THEN
			BEGIN DoVDPKeys(key); RunInstruct END;
		SHO : IF SHOKeys.Pressed(key) THEN
			BEGIN DoSHOKeys(key); RunInstruct END;
		Cubic : IF CubKeys.Pressed(key) THEN
			BEGIN DoCubKeys(key); RunInstruct END
	END; {Case}
END; {DoSystemKeys}

{------- This one uses the keys to choose graphs displayed if no mouse ------}

PROCEDURE DoZoomKeys (key : byte);
BEGIN
	CASE key OF
	 1 : HandleHelpMenu;
	 2 : BEGIN 
			If Not PlotTime THEN Plots := Plots + 1;
			PlotTime := True;
			Message ('x vs. t selected; select another')
		  END;
	 3 : BEGIN 
			If Not PlotVel THEN Plots := Plots + 1;
			PlotVel := True;
			Message ('v vs. t selected; select another')
		  END;
	 4 : BEGIN 
			If Not PlotPhase THEN Plots := Plots + 1;
			PlotPhase := True;
			Message ('v vs. x (phase diagram) selected; select another')
		  END;
	 5 : BEGIN 
			If Not PlotPoincare THEN Plots := Plots + 1;
			PlotPoincare:= True;
			Message ('Poincar‚ map selected; select another')
		  END;
	 6 : BEGIN 
			If Not PlotReturn THEN Plots := Plots + 1;
			PlotReturn := True;
			Message ('Return map selected; select another')
		  END;
	END; {case}
	Debounce
END; {DoZoomKeys}

PROCEDURE SetUpZoomKeys;
BEGIN
	Message('Choose graphs by pressing F keys or clicking mouse in F key area.');
	WITH ZoomKeys DO
	BEGIN
		IF F0overM <> 0.0 THEN
			Init(6)
		ELSE
			Init(4);
		key[1] := 'F1-Help';
		Key[2] := 'F2-x vs. t';
		key[3] := 'F3-v vs. t';
		key[4] := 'F4-v vs. x';
		IF F0overM <> 0.0 THEN
		BEGIN
			key[5] := 'F5-Poincar‚ M';
			key[6] := 'F6-Return M'
		END
  END;
	ZoomKeys.Display;
END; {SetUpZoomKeys}

PROCEDURE DoFlipKeys (key : byte; VAR IBegin, DTime : Integer;
			 VAR Done, Pausing : Boolean);
BEGIN
	CASE key OF
	 1 : BEGIN
			ShowHelp('anharm.hlp','FlipHelp;');
			CloseMyViewPort(AllPort);
			GraphBackColor := DarkGray;
			SetTextStyle(DefaultFont, HorizDir, {Size=} 1);
			DisplayPotential(False, False);
			DisplayChoice;
			ClearPhasePort;
			DrawSystem
		  END;
	 2 : IBegin := IBegin + 10;
	 3 : DTime:= DTime + 100;
	 4 : DTime:= DTime - 100;
	 5 : BEGIN
			  Pausing := Not Pausing;
			  IF Pausing THEN FlipKeys.key[5] := 'F5-Go'
				  ELSE 
				  BEGIN 
					  FlipKeys.key[5] := 'F5-Pause';
					  Message ('Use F keys to control as indicated.')
				  END;
			  FlipKeys.Display
		  END;
	 6 : BEGIN DTime := 0; Done := True END;
	END; {case}
	DeBounce
END; {DoFlipKeys}

PROCEDURE SetUpFlipKeys;
BEGIN
	WITH FlipKeys DO
	BEGIN
		Init(6);
		key[1] := 'F1-Help';
		Key[2] := 'F2-Truncate';
		key[3] := 'F3-Slower';
		key[4] := 'F4-Faster';
		key[5] := 'F5-Pause';
		key[6] := 'F7-Finished';
  END;
  FlipKeys.Display;
END; {SetUpFlipKeys}

Procedure ToggleExactAccel;
BEGIN 
  If Exact THEN
	  BEGIN
	  Exact := False; BothV := True;
	  IF Not (WhichForce in [Walls, Pendulum]) THEN
			 BEGIN
				 EnterMessage('No approximation appropriate. ');
				 MainMenu.Display
			 END;
	  Emin := EminA;
	  MainMenu.Row(4, 12, 'Exact Force');
	  FindPotential(False); {Don't change Scale}
	  SetMyColor(LightCyan);
	  SelectScale(VSNum);
	  SelectMyViewPort (VPort);
	  DisplayPotential(BothV, HoldVScale);
	  END
	  ELSE BEGIN
		 Exact := True; BothV := False;
	  Emin := EminX;
	  MainMenu.Row(4, 12, 'Series Approximation');
	  FindPotential(False);
	  SetMyColor(White);
	  SelectScale(VSNum);
	  SelectMyViewPort (VPort);
	  DisplayPotential(BothV, HoldVScale);
  END
END; {ToggleExactAccel}

{----------- Select two walls simulation with exact force -------------}

Procedure ChooseWalls;
BEGIN
	MainMenu.RowChecked(4,Ord(WhichForce)+1, False);
	IF WhichForce <> Walls THEN
	Begin HavePeriods := False; HaveResonances := False;
		MainMenu.RowActivate(1,12,False)
	 END;
	If Not Exact THEN ToggleExactAccel;
	WhichForce := Walls;
	MaxPos := 0.5;
	Energy := 0.02;
	MaxEnergy := 2*Energy;
	kOverM := 1.0;
	DoverM := 0.0;
	DelayTime := 0;
	IF Animating THEN Continuous := True;
	DeltaE := 0.002;
	EOne := 0.002;
	BaseVal := 0.4; DeltaW := 0.08;
	DoErase := True;
	SetUpWallKeys;
	MainMenu.RowChecked(4,Ord(WhichForce)+1, True)
END; {ChooseWalls}

{----------- Select pendulum simulation with exact force --------------}

Procedure ChoosePendulum;
BEGIN
	MainMenu.RowChecked(4,Ord(WhichForce)+1, False);
	If Not Exact THEN ToggleExactAccel;
	IF WhichForce <> Pendulum THEN
		Begin HavePeriods := False; HaveResonances := False;
		MainMenu.RowActivate(1,12,False)
	END;
	WhichForce := Pendulum;
	MaxPos := Pi;
	Energy := 10.0;
	MaxEnergy := 2*Energy;
	DelayTime := MasterDelayTime;
	kOverM := 1.0;
	DoverM := 0.0;
	Mass := 1.0;
	Equilibrium := 0.0;
	IF Animating THEN Continuous := True;
	DeltaE := PendL/8;
	EOne := PendL/8;
	BaseVal := 2.0; DeltaW := 2.0;
	DoErase := True;
	SetUpPendKeys;
	MainMenu.RowChecked(4,Ord(WhichForce)+1, True)
END; {ChoosePendulum}

Procedure ChooseVanDePol;
BEGIN
	MainMenu.RowChecked(4,Ord(WhichForce)+1, False);
	If Not Exact THEN ToggleExactAccel;
	IF WhichForce <> VanDePol THEN
		Begin HavePeriods := False; HaveResonances := False;
			MainMenu.RowActivate(1,12,False)
	END;
	WhichForce := VanDePol;
	MaxPos := 4.0;
	XStart := 0.5;
	Energy := 0.5; Emin := 0.0;
	MaxEnergy := 2*Energy;
	kOverM := 1.0;
	DoverM := 1.0;
	Equilibrium := 0.0;
	DelayTime := MasterDelayTime DIV 2;
	DeltaE := 0.5;
	EOne := 0.5;
	BaseVal := 0.4; DeltaW := 0.4;
	DoErase := True;
	NColor := 10; SetMyColor(NColor);
	SetUpVDPKeys;
	MainMenu.RowChecked(4,Ord(WhichForce)+1, True)
END; {ChooseVanDePol}

Procedure ChooseSHO;
BEGIN
	MainMenu.RowChecked(4,Ord(WhichForce)+1, False);
	If Not Exact THEN ToggleExactAccel; {No approximations exist}
	IF WhichForce <> SHO THEN
		Begin HavePeriods := False; HaveResonances := False;
			MainMenu.RowActivate(1,12,False)
	END;
	WhichForce := SHO;
	MaxPos := 0.5;
	Energy := 0.06;
	MaxEnergy := 2*Energy;
	kOverM := 1.0;
	Equilibrium := 0.0;
	DelayTime := MasterDelayTime DIV 2;
	IF Animating THEN Continuous := True;
	DeltaE := 0.01;
	EOne := 0.01;
	BaseVal := 2.0; DeltaW := 2.0;
	DoErase := True;
	NColor := 10; SetMyColor(NColor);
	SetUpSHOKeys;
	MainMenu.RowChecked(4,Ord(WhichForce)+1, True)
END; {ChooseSHO}

Procedure ChooseCubic;
BEGIN
	MainMenu.RowChecked(4,Ord(WhichForce)+1, False);
	If Not Exact THEN ToggleExactAccel;
	IF WhichForce <> Cubic THEN
		Begin HavePeriods := False; HaveResonances := False;
			MainMenu.RowActivate(1,12,False)
	END;
	WhichForce := Cubic;
	MaxPos := 0.5;
	Energy  := 0.01;
	MaxEnergy := 2*Energy;
	KoverM := 0.0;
	DoverM := 0.0;
	Equilibrium := 0.0;
	DelayTime := 0; {MasterDelayTime DIV 10;}
	IF Animating THEN Continuous := True;
	DeltaE := 0.0015;
	EOne := 0.0015;
	BaseVal := 1.0; DeltaW := 3.0;
	DoErase := True;
	NColor := 10; SetMyColor(NColor);
	SetUpCubKeys;
	MainMenu.RowChecked(4,Ord(WhichForce)+1, True)
END; {ChooseCubic}

Procedure ChooseAsymmetric;
BEGIN
	MainMenu.RowChecked(4,Ord(WhichForce)+1, False);
	If Not Exact THEN ToggleExactAccel;
	IF WhichForce <> Asymmetric THEN
		Begin HavePeriods := False; HaveResonances := False;
		MainMenu.RowActivate(1,12,False)
	END;
	WhichForce := Asymmetric;
	MaxPos := 1.0;
	Energy  := 0.06;
	MaxEnergy := 2*Energy;
	KoverM := 1.0;
	AsCoef3 := -0.40;
	AsCoef4 := 0.10;
	DoverM := 0.0;
	Equilibrium := 0.0;
	DelayTime := 0; {MasterDelayTime DIV 10;}
	IF Animating THEN Continuous := True;
	DeltaE := 0.03;
	EOne := 0.03;
	BaseVal := -0.36; DeltaW := - 0.02;
	DoErase := True;
	NColor := 10; SetMyColor(NColor);
	SetUpAsKeys;
	MainMenu.RowChecked(4,Ord(WhichForce)+1, True)
END; {ChooseAsymmetric}

PROCEDURE NewSystem(Clear : Boolean);
VAR
	ChangePars : TInputScreen;
	ScrBot, ScrRt : Real;
BEGIN
	IF Not ReadSearch THEN
	IF not Intutorial THEN
	IF (F0overM <> 0.0)
		OR ((DoverM <> 0.0) AND (Not (WhichForce = VanDePol))) THEN
	With ChangePars DO
	BEGIN
		Init;
		ScrBot := 0.67; ScrRt := 0.52;
		LoadLine ('#1 Keep present drive and damping');
		LoadLine ('#1 No driving force, no friction');
		IF PlotPoincare OR PlotReturn THEN
		BEGIN
			LoadLine ('');
			LoadLine ('Poincar‚ or Return Plots will not make');
			Loadline ('sense if you turn off drive & damping;');
			Loadline ('you will need to change the display.');
			Loadline ('');
			ScrBot := ScrBot - 0.25; ScrRt := ScrRt + 0.1
		END;
		If Animating THEN
		BEGIN
			LoadLine ('');
			LoadLine ('#2 Animation off to increase speed');
			LoadLine ('#2 Keep animation');
			LoadLine ('');
			ScrBot := ScrBot - 0.2
		END;
		LoadLine ('    [  OK  ]    [Cancel]');
		DefineInputPort(0.07, ScrRt, ScrBot, 0.80);
		Accept;
		IF Not Canceled THEN
		BEGIN
			If (GetRadioButton('1') = 2) THEN
			BEGIN F0overM := 0.0; DoverM := 0.0;
				If WhichForce = VanDePol THEN DoverM := 1.0;
				AlwaysContinuous := False; Continuous := False;
			END;
			FreezeScales := False; FreezeTimeScale := False;
			If Animating AND (GetRadioButton('2') = 1) THEN
			BEGIN
				Animating := False;
				AnimString := '';
					MainMenu.row(2, 4, 'Animate '+AnimString);
				MainMenu.Display
			END
		END; {Not Canceled}
		Done
	END; {With ChangePars}
	IF WhichForce = VanDePol THEN
      BEGIN AlwaysContinuous := True; Continuous := True END;
	HideMouse;
	If Clear THEN ClearPhasePort;
	DrawSystem;
	FindPotential(True);
	DisplayPotential(False, False);
	DisplayChoice;
	RunInstruct;
	IF WhichForce = VanDePol THEN
		MildAnnounce ('Click in Van Der Pol picture (Top right) or Phase Plot to launch motion.');
	If DoverM > 0.0 THEN DisplayDissip;
	ShowMouse
END; {NewSystem}

PROCEDURE ChooseSystem(Clear : Boolean);
	BEGIN
	Case WhichForce OF
		Asymmetric : ChooseAsymmetric;
		Walls : ChooseWalls;
		Cubic : ChooseCubic;
		Pendulum : ChoosePendulum;
		VanDePol : ChooseVanDePol;
		SHO : ChooseSHO
	END; {CASE}
	NewSystem(Clear)
END; {ChooseSystem}

{---------- Here are the actions taken when menu items are picked ----------}
Procedure DisplayMainMenu;
VAR Driven : Boolean;
BEGIN
	IF F0overM <> 0.0 THEN Driven := True Else Driven := False;
	MainMenu.RowActivate(3,4,Driven);
	MainMenu.RowActivate(3,5,Driven);
	MainMenu.Display
END;

PROCEDURE HandleMenu;
VAR
	dy : Integer; {Used in Parameter selection routines}
	y2, ys : Real;
	Done : Boolean; 

{------------ Display trajectory in both wells or only one ----------}

  Procedure ToggleBothSides;
  BEGIN
	 IF BothSides THEN
	 BEGIN
		 BothSides := False;
		 BothWellStr := BothWellStr1
	 END
	 ELSE BEGIN
		 BothSides := True;
		 BothWellStr := BothWellStr2
	 END;
	 MainMenu.row(2, 3, BothWellStr);
  END; {ToggleBothSides}

{------------ Use exact force or the fourth order approximation ------------}

	FUNCTION GetFName(Extension : String) : String;
	VAR
		DirInfo : SearchRec;

	BEGIN
		FindFirst ('*'+Extension, AnyFile, DirInfo);
		IF DosError = 18 THEN
		BEGIN
			MildAnnounce
			  ('There are No files of the type requested in this directory.');
			GetFName := ''
		END
		ELSE
      BEGIN
      	Message ('Press ESC if you do not want any of these files.');
			GetFName := OpenFile('*'+Extension)
		END;
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
		Action := 0;
		DosError := 100; {Signals no file has been sought}
		FName := '';
		REPEAT {Until File is new or overwrite OK}
			With Describe DO
			REPEAT {Until filename is not blank}
				Init;
				IF Extension = '.smp' THEN
				BEGIN
					LoadLine ('      Search Parameter Space');
					LoadLine ('');
					LoadLine (' Writes many Poincar‚ maps to a file');
					LoadLine (' for later review by Read Search Maps.');
					SetHelpFile('Anharm.hlp','SearchHelp;');
				END
				ELSE
				BEGIN
					LoadLine ('    Writing A File');
					SetHelpFile('Anharm.hlp','FileHelp')
				END;
				LoadLine ('');
				LoadLine ('  The name of the file is ');
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
				LoadLine ('    [  OK  ]    [Cancel]   [Help]');
				DefineInputPort(0.09, 0.62, 0.3, 0.92);
				Message('Type a file name and press <ENTER>');
				Accept;
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
		DisplayMainMenu;
		ClearPhasePort;
		PutFName := FName
	END; {PutFName}

TYPE
  ParMapRec = Record
		Drag, Driv, Omeg : Real;
		Description : String[60];

		CASE Force : ForceType OF
			Walls, SHO, Cubic : (fk, fm, separation : Real);
			Asymmetric : (am, fak, k3, k4 : Real);
			Pendulum : (fg, Plength : Real);
			VanDePol : (vk, vm, VX0 : Real);
	END; {PMapRec}

	PROCEDURE SavePars(FName : String);
	VAR
		i, j : Integer;
		ParFile : File of ParMapRec;
		ParMap : ParMapRec;
		FStr : String[11];
		ParsIn : TInputScreen;

	BEGIN
		IF FName = 'ABORT' THEN BEGIN Message ('File not written'); Exit END;
		Message('Press <ENTER> to accept description, <ESC> to leave blank.');
		Case WhichForce OF
			Asymmetric : FStr := 'Asymm';
			Walls    : FStr := 'Two Walls';
			Pendulum : FStr := 'Pendulum';
			Cubic    : FStr := 'Cubic';
			SHO      : FStr := 'SHO';
			VanDePol : Fstr := 'Van Der Pol';
		END; {Case}
		With ParMap DO With ParsIn DO
		BEGIN
			Init;
			LoadLine ('System Parameters. Current force: '+FStr);
			LoadLine (' ');
			LoadLine ('    Please type a description:');
			LoadLine ('"                                                        "');
			LoadLine (' ');
			LoadLine ('          Wall Separation = '+NumStr(HalfWallSep*2,5,2)+' m');
			LoadLine ('               Pendulum L = '+NumStr(PendL,7,4)+' m');
			LoadLine ('      Limit Cycle Size X0 = '+NumStr(VdepX0,7,4)+' rad/sec');
			LoadLine ('  k = '+NumStr(kOverM*mass,5,2)+' N/m;     C3 = '+NumStr(C3overM*mass,5,2)+' N/m^3');
			LoadLine ('  m = '+NumStr(mass,5,2)+' kg;       g = '+NumStr(gOverL*PendL,5,2)+' m/sec^2');
			LoadLine ('Fo sin(omega t):       Fo = '+NumStr(F0overM*mass,7,4)+' m/sec^2');
			LoadLine ('                    omega = '+NumStr(omega,7,4)+' rad/sec');
			LoadLine ('          Drag Coeficient = '+NumStr(DoverM*mass,7,4)+' sec^-1  ');
			LoadLine ('');
			LoadLine ('    [  OK  ]    [Cancel]  [Help]');
			SetHelpFile('Anharm.hlp','FileHelp');
			DefineInputPort(0.01, 0.99, 0.12, 0.92);
			AcceptScreen;
			If Not Canceled THEN
         BEGIN
         	IF Copy(Fname, Pos('.',FName)+1,2) = 'ex' THEN
				BEGIN {File Exists}
			      Delete (FName, Pos('.',FName), 12);
			      FName := FName + '.par';
			      Assign(ParFile, FName);
			      Reset(ParFile);
			      REPEAT
				      Read(ParFile, ParMap)
			      UNTIL Eof(ParFile)
		      END {File Exists}
		      ELSE
		      BEGIN
			      Assign(ParFile, FName);
			      ReWrite(ParFile)
		      END; {Else File Didn't Exist}
				Description := GetString(1);
				Force := WhichForce;
			   CASE WhichForce OF
					  Walls, SHO, Cubic : BEGIN fk := koverm; fm := mass; Separation := HalfWallSep END;
				     Pendulum : BEGIN fg := gOverL; PLength := PendL END;
				     VanDePol : BEGIN vm := mass; vk := koverm; vX0 := VDepX0 END;
					  Asymmetric : BEGIN am := mass; fak := koverm; k3 := AsCoef3; k4 := AsCoef4 END;
			   END; {Case WhichForce}
				Drag := DoverM;
			   Driv := F0overM;
			   Omeg := Omega;
			   Write (ParFile, ParMap);
            Close(ParFile)
         END;
			Done
		END; {With ParMap}
		CloseMyViewPort(AllPort);
		ClearPhasePort;
		NewSystem({Clear: } True);
		DisplayChoice;
		If DoverM > 0.0 THEN DisplayDissip;
		DisplayMainMenu;
		Event.ReadKey := Chr(0);
		ShowMouse
	END; {SavePars}

PROCEDURE ReadPars(FName : String);
VAR
	ParFile : File of ParMapRec;
	Pars : ParMapRec;
	ParsIn : TInputScreen;
	FStr : String[10];
	Action : Integer;
	kSav, mSav, SeparationSav, gSav, PLengthSav, k3sav, k4sav,
			vmSav, vkSav, VX0Sav, DragSav, DrivSav, OmegSav : Real;
	ForceSav : ForceType;
BEGIN
	IF FName = '' THEN Exit;
	ShowMouse; Action := 0;
	ForceSav := WhichForce;
	kSav := koverm; mSav := mass; SeparationSav := HalfWallSep;
	gSav := gOverL; PLengthSav := PendL;
	k4sav := AsCoef4; k3sav := AsCoef3;
	vmSav := mass; vkSav := koverm; vX0Sav := VDepX0;
	DragSav := DoverM; DrivSav := F0overM; OmegSav := Omega;
	Assign(ParFile, FName);
	ReSet(ParFile);
	IF Eof(ParFile) THEN
	BEGIN
		Message('File is empty. Click or ENTER.');
		StaticPause
	END
	ELSE
	WHILE ((Not Eof(ParFile)) AND (Not (Action in [1,3]))) DO {Record picked or EOF}
	With Pars DO
	With ParsIn DO
	BEGIN
		Init;
		Read (ParFile, Pars);
		MainMenu.RowChecked(4,Ord(WhichForce)+1, False);
		WhichForce := Force;
		MainMenu.RowChecked(4,Ord(WhichForce)+1, True);
		CASE WhichForce OF
			Walls, SHO, Cubic :
				BEGIN koverm := fk; mass := fm; HalfWallSep := Separation END;
			Pendulum : BEGIN gOverL := fg; PendL := PLength END;
			VanDePol : BEGIN mass := vm; koverm := vk; VDepX0 := VX0 END;
			Asymmetric : BEGIN koverm := fak; AsCoef3 := k3; AsCoef4 := k4; Mass := am END;
		END; {Case WhichForce}
		DoverM := Drag;
		F0overM := Driv;
		Omega := Omeg;
		Case WhichForce OF
			Asymmetric : FStr := 'Asymm';
			Walls    : FStr := 'Two Walls';
			Pendulum : FStr := 'Pendulum';
			Cubic    : FStr := 'Cubic';
			SHO      : FStr := 'SHO';
			VanDePol : Fstr := 'Van Der Pol';
		END; {Case}
		LoadLine ('System Parameters; Current force: '+FStr);
		LoadLine (''+Description);
		LoadLine ('          Wall Separation = '+NumStr(HalfWallSep*2,5,2)+' m');
		LoadLine ('               Pendulum L = '+NumStr(PendL,7,4)+' m');
		LoadLine ('      Limit Cycle Size X0 = '+NumStr(VdepX0,7,4)+' rad/sec');
		LoadLine ('  k = '+NumStr(kOverM*mass,5,2)+' N/m;     C3 = '+NumStr(C3overM*mass,5,2)+' N/m^3');
		LoadLine ('  m = '+NumStr(mass,5,2)+' kg;       g = '+NumStr(gOverL*PendL,5,2)+' m/sec^2');
		LoadLine ('Fo sin(omega t):       Fo = '+NumStr(F0overM*mass,7,4)+' m/sec^2');
		LoadLine ('                    omega = '+NumStr(omega,7,4)+' rad/sec');
		LoadLine ('         Drag Coefficient = '+NumStr(DoverM*mass,7,4)+' sec^-1  ');
		LoadLine (' ');
		LoadLine ('#1 Use these parameters');
		LoadLine ('#1 Look for another set in this file)');
		LoadLine ('#1 Quit looking in this file');
		LoadLine ('');
		LoadLine (' [  OK  ]    [Cancel]   [Help]');
		SetHelpFile('Anharm.hlp','FileHelp');
		DefineInputPort(0.01, 0.99, 0.12, 0.92);
		Accept;
		If Canceled THEN
			Action := 3
		ELSE
			Action := GetRadioButton('1');
		Done;
	END; {ELSE File Exists}
	IF (Action = 3) OR ((Action = 2) AND Eof(ParFile)) THEN
	BEGIN {No record accepted; restore original parameters}
		WhichForce := ForceSav;
		koverm := kSav; mass := mSav; HalfWallSep := SeparationSav; 
		gOverL := gSav; PendL := PLengthSav;
		AsCoef4 := k4sav; AsCoef3 := k3sav;
		mass := vmSav; koverm := vkSav; VDepX0 := VX0Sav;
		DoverM := DragSav; F0overM := DrivSav; Omega := OmegSav;
		ClearPhasePort
	END;
	CloseMyViewPort(AllPort);
	ReadSearch :=True; {Don't display drive & Damping inquiry}
	ChooseSystem({Clear: } True);
	DisplayChoice;
	ReadSearch :=False;
	If DoverM > 0.0 THEN DisplayDissip;
	Close(ParFile);
	Event.ReadKey := Chr(0);
END; {ReadPars}

PROCEDURE SaveHist;
VAR Screen : TInputScreen;
	 FStr : String[10];
	 FName : String;
	 CDAType : Boolean;
	 i : integer;
BEGIN
		With Screen Do
		BEGIN
{MAC		New(Screen);}
			Init;
			LoadLine(' What Type of File?');
			LoadLine('');
			LoadLine('#1 Full History : t,x,v');
			LoadLine('#1 x only for Chaos Data Analyzer');
			LoadLine('');
			LoadLine('  [ OK ]    [Cancel]');
			DefineInputPort(0.1,0.55,0.37,0.65);
			AcceptScreen;
			IF Canceled THEN BEGIN Done; Exit END
			ELSE
			BEGIN
				CDAType := GetBoolean(2);
				IF CDAType THEN FName := PutFname('.dat')
				ELSE FName := PutFname('.ohs')
			END; {Not Canceled}
			Done;
{MAC		Dispose(Screen);}
		END; {With Screen}
		IF (FName = 'ABORT') OR (Copy(Fname, Pos('.',FName)+1,2) = 'ex') THEN
		BEGIN
			Message ('File not written');
			StaticPause;
			DisplayMainMenu;
			Exit
		END;
		Assign(HistFile, FName);
		HistFileOpen := True;
		ReWrite(HistFile);
		IF Not CDAType THEN {Write Header}
		BEGIN
			Case WhichForce OF
				Asymmetric : FStr := 'Asymmetric';
				Walls    : FStr := 'Two Walls';
				Pendulum : FStr := 'Pendulum';
				Cubic    : FStr := 'Cubic';
				SHO      : FStr := 'SHO';
				VanDePol : Fstr := 'Van Der Pol';
			END; {Case}
			Message('Press <ENTER> to accept description, <ESC> to leave blank.');
			With Screen DO
			BEGIN
				Init;
				LoadLine ('History File; Current force: '+FStr);
				LoadLine (' ');
				LoadLine ('    Please type a description:');
				LoadLine ('"                                                        "');
				LoadLine (' ');
				LoadLine ('       [  OK  ]    [Cancel]');
				DefineInputPort(0.10, 0.85, 0.58, 0.85);
				AcceptScreen;
				Writeln (HistFile, 'Oscillator History File: '+FStr);
				If Not Canceled THEN
					Writeln (HistFIle, GetString(1))
				ELSE
					Writeln (HistFile, ' ');
				Writeln (HistFile, '    T                   X                 V');
				Done
			END
		END; {Write Header}
		IF CDAType AND (WhichForce = Pendulum) THEN
			Comment(0.1,0.7,
				'Writing Velocity data for beter analysis for Pendulum');
		IF NOT Wrapped THEN {Points start at zero}
		BEGIN
			FOR i := 1 TO IHist DO
				IF Not CDATYPE THEN
					Writeln (HistFile,Ts[i], Xs[i],Vs[i])
					ELSE IF WhichForce = Pendulum THEN
						Writeln (HistFile,Vs[i])
						ELSE Writeln (HistFile,Xs[i])
		END
		ELSE
		BEGIN
			FOR i := iHist+1 to HistoryCount DO
				IF Not CDATYPE THEN
					Writeln (HistFile,Ts[i], Xs[i],Vs[i])
					ELSE IF WhichForce = Pendulum THEN
						Writeln (HistFile,Vs[i])
						ELSE Writeln (HistFile,Xs[i]);
			FOR i := 1 to iHist DO
				IF Not CDATYPE THEN
					Writeln (HistFile,Ts[i], Xs[i],Vs[i])
					ELSE IF WhichForce = Pendulum THEN
						Writeln (HistFile,Vs[i])
						ELSE Writeln (HistFile,Xs[i])
		END; {wrapped}
		Close(HistFile);
		HistFileOpen := False
END;

PROCEDURE SaveResonance(Fname : String);
VAR
	FStr : String[10];
	i, j, k : Integer;
	Screen : TInputScreen;
	ResFile : Text;
BEGIN
	IF FName = 'ABORT' THEN BEGIN Message ('File not written'); Exit END;
	Message('Press <ENTER> to accept description, <ESC> to leave blank.');
	Case WhichForce OF
		Asymmetric : FStr := 'Asymmetric';
		Walls    : FStr := 'Two Walls';
		Pendulum : FStr := 'Pendulum';
		Cubic    : FStr := 'Cubic';
		SHO      : FStr := 'SHO';
		VanDePol : Fstr := 'Van Der Pol';
	END; {Case}
	With Screen DO
	BEGIN
		Init;
		LoadLine ('Resonance Plot File; Current force: '+FStr);
		LoadLine (' ');
		LoadLine ('    Please type a description:');
		LoadLine ('"                                                        "');
		LoadLine (' ');
		LoadLine ('       [  OK  ]    [Cancel]');
		DefineInputPort(0.10, 0.85, 0.58, 0.85);
		AcceptScreen;
		If Canceled THEN Exit;
		Assign(ResFile, FName);
		ReWrite(ResFile);
		Writeln (ResFile, 'Resonance Plot File: ');
		Writeln (ResFile, GetString(1));
		Done
	END;
	Writeln (ResFile);
	Writeln (Resfile, Ord(WhichForce),'  Force: ',FStr);
	Writeln (Resfile, mass, ' Mass');
	Writeln (Resfile, koverm*mass, ' Force constant k');
	CASE WhichForce OF
		  Walls : Writeln (Resfile, 2*HalfWallSep, ' Wall Separation');
		  Pendulum :
			BEGIN
				Writeln (Resfile, gOverL*PendL, ' g');
				Writeln (Resfile, PendL, ' Pendulum Length')
			END;
		  VanDePol : Writeln (Resfile, VDepX0, 'Limit Cycle Size X0');
		  Asymmetric :
			BEGIN
				Writeln (Resfile, AsCoef3, ' Asymmetric Cubic Coeficient');
				Writeln (Resfile, AsCoef4, ' Asymmetric Quartic Coeficient')
			END;
	END; {Case WhichForce}
	Writeln (ResFile, Omega0-OmegaRangeDown, ' Lowest Angular Frequency');
	Writeln (ResFile, Omega0+OmegaRangeUp, ' Highest Angular Frequency');
	Writeln (ResFile, NResCurves, ' Number of Curves');
	Writeln (ResFile, NResPts[1],' ', NResPts[2],' ', NResPts[3],
							' # of Points in curve segments');
	Writeln (ResFile, DBeg, ' Lowest Damping');
	Writeln (ResFile, DBeg + (NResCurves-1)*DInc, ' Highest Damping');
	Writeln (ResFile, AScale, ' Vertical Scale');
	FOR i := 1 to NResCurves DO
	BEGIN
		Writeln (ResFile, 'Points in Curve ',i);
		Writeln (ResFile, NumberOfResPlots[i], ' # of segments in this curve');
		Writeln (ResFile, 'Angular Frequency    Amplitude');
		FOR j := 1 to 3 DO
		BEGIN
		 For k := 1 to NResPts[j] DO
			Writeln (ResFile,Frequencies[i,j].Value(k),Amplitudes[i,j].Value(k));
		 Writeln (ResFile)
		END
	END;
	Close (Resfile)
END; {SaveResonance}

PROCEDURE ReadResonance(Fname : String);
VAR
	ResFile : Text;
	i, j, k : Integer;
	Description : String;
	Freq, Ampl : Real;
BEGIN
	IF FName = '' THEN Exit;
		IF HaveResTabSpace THEN
			For i := 1 TO NResCurves DO
				BEGIN
				Amplitudes[i,1].free; Frequencies[i,1].free;
				Amplitudes[i,2].free; Frequencies[i,2].free;
				Amplitudes[i,3].free; Frequencies[i,3].free;
				HaveResTabSpace := False
				END;
	Assign(ResFile, FName);
	ReSet(ResFile);
	IF Eof(ResFile) THEN
	BEGIN
		Message('File is empty. Click or ENTER.');
		StaticPause
	END
	ELSE
	BEGIN
		Readln(ResFile);
		Readln(ResFile, Description);
		Message(Description);
		Readln(ResFile);
		Readln(ResFile, i);
		WhichFOrce := Asymmetric;
		While i > 0 DO 
		BEGIN
			WhichForce := Succ(WhichForce);
			i := i - 1
		END;
		Readln(ResFile, mass);
		Readln(ResFile, koverm); koverm := koverm/mass;
		CASE WhichForce OF
			  Walls : 
					BEGIN
						Readln (Resfile, HalfWallSep);
						HalfWallSep := HalfWallSep/2
					END;
			  Pendulum :
					BEGIN
						Readln (Resfile, PendL);
						Readln (Resfile, gOverL); 
						gOverL := gOverL/PendL
					END;
			  VanDePol : Readln (Resfile, VDepX0);
			  Asymmetric :
					BEGIN
						Readln (Resfile, AsCoef3);
						Readln (Resfile, AsCoef4)
					END;
		END; {Case WhichForce}
		Readln(ResFile, OmegaRangeDown);
		Readln(ResFile, OmegaRangeUp);
		Omega0 := (OmegaRangeUp + OmegaRangeDown)/2;
		OmegaRangeUp := OmegaRangeUp - Omega0;
		OmegaRangeDown := Omega0 - OmegaRangeDown;
		Readln (ResFile, NResCurves);
		Readln (ResFile, NResPts[1], NResPts[2], NResPts[3]);
		i := 1;
		While i <=  NResCurves DO
		BEGIN
			IF 2*(NResPts[1]+NResPts[2]+NResPts[3])*SizeOf(Real)+1600 < Maxavail THEN
			BEGIN
				Amplitudes[i,1].init(NResPts[1]+1);
				Frequencies[i,1].init(NResPts[1]+1);
				Amplitudes[i,2].init(NResPts[2]+1);
				Frequencies[i,2].init(NResPts[2]+1);
				Amplitudes[i,3].init(NResPts[3]+1);
				Frequencies[i,3].init(NResPts[3]+1);
				HaveResTabSpace := True
			END
			ELSE
			BEGIN
				Announce('Number of curves reduced to '+NumStr(i-1,2,0)
					+' because of lack of memory.');
				NResCurves := i-1
			END;
			i := i + 1
		END;
		Readln (ResFile, DBeg);
		Readln (ResFile, DInc);
		IF NresCurves > 1 THEN
			DInc := (DInc-DBeg)/(NResCurves-1)
		ELSE
			DInc := (DInc-DBeg);
		Readln (ResFile, AScale);
		FOR i := 1 to NResCurves DO
		BEGIN
			Readln (ResFile);
			Readln (ResFile, NumberOfResPlots[i]);
			Readln (ResFile);
			FOR j := 1 to 3 DO
			BEGIN
			 For k := 1 to NResPts[j] DO
			 BEGIN
				Readln (ResFile, Freq, Ampl);
				Frequencies[i,j].Put(k,Freq);
				Amplitudes[i,j].Put(k,Ampl)
			 END;
			 {Now signal to display routine that curve is complete}
			 Frequencies[i,j].Put(NResPts[j]+1,Freq);
			 Amplitudes[i,j].Put(NResPts[j]+1,Ampl);
			 Readln (ResFile)
			END;
		END;
		{Complete signal to display routine that curve is complete}
		FOR j := 1 to 3 DO
			Nrespts[j] := NResPts[j] + 1;
		Close (Resfile)
	END;
	HaveResonances := True;
	DefineScale(ResonanceSNum,
			Omega0-OmegaRangeDown,Omega0+OmegaRangeUp,0.0,AScale);
	ShowPeriod('R')
END; {ReadResonance}

PROCEDURE QueryAnimation;
VAR
	InScreen : TInputScreen;
BEGIN   {Only called if Animating is true}
	IF NOT InTUtorial THEN WITH InScreen DO
	BEGIN
		Init;
		LoadLine ('#2 Animation off to increase speed');
		LoadLine ('#2 Keep animation');
		DefineInputPort (0.10, 0.60, 0.67, 0.80);
		LoadLine ('    [  OK  ]    [Cancel]');
		Accept;
		If (Not Canceled) AND (GetRadioButton('2') = 1) THEN
		BEGIN 
			Animating := False; 
			AnimString := '';
			MainMenu.row(2, 4, 'Animate '+AnimString);
			DisplayMainMenu
		END;
		Done
	END
END; {QueryAnimation}

	Function ChgNum(Num:REAL; i,j,l:integer):string;
	VAR Temp : String;
	BEGIN  {insert '+' at beginning of number for input screen parsing}
		temp := NumStr(Num, i, j);
		While temp[1] = ' ' DO Delete (Temp, 1, 1);
		While Length(temp) < l DO temp := '+'+temp;
		ChgNum := temp
	END; {ChgNum}

{-----------------   Change the scale of the energy diagram        ----------}

	PROCEDURE ExactParameters(VAR Search : Boolean);
	VAR TempE, TempA, OrigA, SavL, SavM : Real;
		 Launch : Boolean;
		 Nf : Integer;
		 FStr : String[10];
		 InPar : TInputScreen;
	BEGIN
	Launch := False; SavL := PendL; SavM := Mass;
	Message('Press <ENTER> to change parameters, <ESC> to leave unchanged');
	Case WhichForce OF
		Asymmetric : FStr := 'Asymm';
		Walls    : FStr := 'Two Walls';
		Pendulum : FStr := 'Pendulum';
		Cubic    : FStr := 'Cubic';
		SHO      : FStr := 'SHO';
		VanDePol : Fstr := 'Van Der Pol';
	END; {Case}
	With InPar DO
	BEGIN
		Init;
		DefineInputPort(0.00, 0.65, 0.08, 0.92);
		LoadLine ('System Parameters * * * Current force: '+FStr+' ');
		LoadLine ('          Wall Separation = {       } m');
		LoadLine ('               Pendulum L = {       } m');
		LoadLine ('      Limit Cycle Size X0 = {       } rad/sec');
		LoadLine ('  k = {     } N/m;     C3 = {       } N/m^3');
		LoadLine ('  m = {     } kg;       g = {       } m/sec^2');
		LoadLine ('Fo sin(omega t):       Fo = {       } kg m/sec^2');
		LoadLine ('                    omega = {       } rad/sec');
		LoadLine ('         Drag Coefficient = {       } sec^-1  ');
		SetNumber(1,2*HalfWallSep);
		SetNumber(2,PendL);
		SetNumber(3,VdepX0);
		SetNumber(4,kOverM*mass); SetNumber(5,C3overM*mass);
			{Change limits in next line when GetX and scaling can
			 handle negative k (which is OK in cubic if C3 > 0}
		SetNumberLimits(4,0.0,50.0);
		SetNumber(6,mass); SetNumber(7,gOverL*PendL);
		SetNumber(8,F0overM*mass);
		SetNumber(9,omega);
		SetNumber(10,DoverM*mass);
		SetNumberLimits(10,0.0,10.0);
		IF Search THEN
		BEGIN
			LoadLine ('Search starting from above values & changing:');
			LoadLine ('  Drive Ampl by  {0.100}   {     } times');
			LoadLine (' Drive Omega by  {0.100}   {     } times');
			LoadLine (' Resist Coef by  {0.100}   {     } times');
			LoadLine ('');
			SetNumber(12,NDrive);
			SetNumber(14,NOmega);
			SetNumber(16,NResist);
			SetHelpFile('Anharm.hlp','SearchHelpII;')
		END
		ELSE
		BEGIN
			LoadLine ('           User Parameter = {       }');
			LoadLine ('Approximate potential polynomial');
			LoadLine (' coefficients: A1 = '+NumStr(VCoefA,7,4)+' A3 = '
							+NumStr(VCoefB,7,4));
			LoadLine ('');
			Loadline ('Asymmetric potential polynomial');
			Loadline (' coefficients: A2 = {       } A3 = {      }');
			Loadline ('');
			LoadLine ('***** Initial Conditions ******* ');
			IF WhichForce = Pendulum THEN
			BEGIN
				LoadLine ('Initial position = {       } rad');
				LoadLine ('        Velocity = {       } rad/sec')
			END
			ELSE
			BEGIN
				LoadLine ('Initial position = {       } m');
				LoadLine ('        Velocity = {       } m/sec')
			END;
			SetNumber(11,UserPar);
			SetNumber(12,AsCoef3); SetNumber(13,AsCoef4);
			SetNumber(14,x0Old);
			SetNumber(15,v0Old);
			SetHelpFile('Anharm.hlp','ExactParHelp;')
		END; {If Search ELSE}
		LoadLine ('       [  OK  ]       [Cancel]      [Help]');
		Accept;
		If Canceled THEN Search := False
		ELSE
		BEGIN
			HalfWallSep := GetNumber(1)/2;
			mass  := GetNumber(6);
			IF Mass > 0.0 THEN
			BEGIN
				koverm := GetNumber(4)/mass;
				C3overM := GetNumber(5)/mass;
				F0overM := GetNumber(8)/mass;
				DoverM := GetNumber(10)/mass;
			END; {Mass > 0}
			PendL := GetNumber(2);
			IF PendL > 0.0 THEN gOverL := GetNumber(7)/PendL;
			VdepX0:= GetNumber(3);
			omega := GetNumber(9);
			If Not Search THEN
			BEGIN
				UserPar := GetNumber(11);
				AsCoef3 := GetNumber(12); AsCoef4 := GetNumber(13);
				TempA  := GetNumber(14);
				IF Abs(TempA - Round(1.0E4*x0Old)/1.0E4) > 0.00005  THEN
					BEGIN x0Old := TempA; Launch := True END;
				TempE  := GetNumber(15);
				IF Abs(TempE - v0Old) > 0.00005 THEN
					BEGIN v0Old := TempE; Launch := True END;
				IF Launch THEN PhasePicked := True
			END {Not Search}
			ELSE
			BEGIN
				 DDrive := GetNumber( 11);
				 NDrive := Trunc(GetNumber( 12));
				 DOmega := GetNumber( 13);
				 NOmega := Trunc(GetNumber( 14));
				 DResist := GetNumber( 15);
				 NResist := Trunc(GetNumber( 16))
			END; {ELSE Search}
		END; {Not Escaped From}
		Done
	 END; {With InPar}
	 IF Animating AND ((F0overM <> 0.0) OR (DoverM <> 0.0)) THEN
		QueryAnimation;
	 CloseMyViewport(AllPort);
	 DisplayChoice;
	 DisplayDissip;
	 DrawSystem;
	 FindPotential(True);
	 DisplayPotential(Not Exact, HoldVScale);
	 IF (DoverM > 0.0) OR Animating THEN Continuous := True
		 ELSE Continuous := AlwaysContinuous;
	 DisplayMainMenu;
	 ClearPhasePort;
	 If Launch THEN ShowTrajectory;
	 Launch := False; PhasePicked := False
	END; {ExactParameters}

  Procedure SelectEnergyScale;
  VAR
	  EnIn : TInputScreen;
	  TMaxPos : Real;
	  NRoots : Integer;
  BEGIN
	With EnIn DO
	BEGIN
	  Init;
	  LoadLine ('   Select New Energy Scale');
	  LoadLine ('');
	  LoadLine ('Largest Energy: {      } joules');
	  LoadLine ('');
	  LoadLine ('       [  OK  ]    [Cancel]');
	  SetNumber(1,MaxEnergy);
	  SetNumberLimits(1, 1e-6, 1e4);
	  DefineInputPort(0.06, 0.65, 0.12, 0.92);
	  Accept;
	  MaxEnergy := GetNumber(1);
	  If (WhichForce = Pendulum) AND (MaxEnergy > 2.0*gOverL*Pendl) THEN
			TMaxPos := 4.0
		ELSE
		BEGIN
			GetX(TMaxPos, MaxEnergy+Emin);
			IF TMaxPos = 0.0 THEN {Happens if asymmetric potentialhas only local minimum}
			BEGIN
				GetX2(TMaxPos, MaxEnergy+Emin);
				TMaxPos := Abs(TMaxPos)
			END
		END;
	  IF TMaxPos > 0.0 THEN MaxPos := TMaxPos
	  ELSE MildANnounce('I can''t handle this range.');
     If Not Canceled THEN NewEnergyScale := True;
	  CloseMyViewPort(AllPort);
	  DefineScale(VSNum, -MaxPos, MaxPos, 0.0, MaxEnergy);
	  HoldVScale := True;
	  FindPotential({SetScale =} False);
	  DefineScale(VSNum, -MaxPos, MaxPos, 0.0, MaxEnergy);
	  DisplayPotential({ShowApprox =} Not Exact, HoldVScale);
	  DrawSystem;
	  DisplayChoice;
	  RunInstruct;
	  ClearPhasePort;
	  DisplayMainMenu;
	  Done;
	  ShowMouse
	END {With DataScreen}
  END; {SelectEnergyScale}

	PROCEDURE SetOptions;
	VAR 
		 SaveTs, SavePos, SaveVel, SavNPhases : Real;
		 Error : Boolean;
		 DisIn : TInputScreen;
		 MemBlock : Integer;
	BEGIN
		With DisIn DO
		REPEAT {Until no error}
			Init;
			DefineInputPort(0.08, 0.65, 0.20, 0.92);
			Error := False;
			LoadLine ('       Display and Calculation Options');
			LoadLine ('');
			LoadLine ('  Animation Delay Time = {     } ms');
			LoadLine ('    Undriven Time Step = {     } sec');
			LoadLine ('      Driven Time Step = {     } per Period');
			LoadLine ('      Graph Time Scale = {     } sec');
			LoadLine ('         Graph X Scale = {     } m');
			LoadLine ('         Graph V Scale = {     } m/sec');
			LoadLine ('Number of movie frames = {     } per cycle');
			SetNumber (1,MasterDelayTime);
			SetNumber (2,TimeStep);
			SetNumber (3,DrivenTimeFactor);
			SetNumber (4,TimeScale);
			SetNumber (5,MaxPos);
			SetNumber (6,MaxVel);
			SetNumber (7,NPhases);
			SetNumberLimits (7,1,32);
			LoadLine ('Stop after two periods  = #1 Yes #1 No');
			LoadLine ('Write over time graphs  =  #2 Yes #2 No');
			LoadLine ('Mark Poincar‚ in Phase  =  #3 Yes #3 No');
			LoadLine ('Large dots in Poincar‚  =  #5 Yes #5 No');
			LoadLine ('Pause in Period & Reson =  #4 Yes #4 No');
			If AlwaysContinuous OR Continuous THEN SetRadioButton('1',2);
			If DoErase THEN SetRadioButton('2',2);
			If Not DemoPoincare THEN SetRadioButton('3',2);
			If Not LargePoincare THEN SetRadioButton('5',2);
			IF Not PeriodPause THEN SetRadioButton('4',2);
			LoadLine ('');
			LoadLine (' [  OK  ]    [Cancel]   [Help]');
			SetHelpFile('Anharm.hlp','OptionHelp;');
			Message('Press <ENTER> to change parameters, <ESC> to leave unchanged');
			Accept;
			If Not Canceled THEN
			BEGIN
				SaveTs := TimeScale; SavePos := MaxPos; SaveVel := MaxVel;
				SavNPhases := NPhases;
				MasterDelayTime := Trunc(GetNumber(1));
				TimeStep  := GetNumber(2);
				DrivenTimeFactor := GetNumber(3);
				TimeScale := GetNumber(4);
				MaxPos := GetNumber(5);
				MaxVel := GetNumber(6);
				NPhases := Trunc(GetNumber(7));
				IF Nphases < SavNPhases THEN
				BEGIN
					MemBlock := PMapSize*SizeOf(Integer);
					REPEAT
					FreeMem(XMapPtr[MapsUsed], MemBlock);
					FreeMem(VMapPtr[MapsUsed], MemBlock);
					MapsUsed := MapsUsed - 1
					UNTIL MapsUsed = NPhases
				END;
				IF Animating OR (DoverM > 0.0) THEN
					Continuous := GetBoolean(9)
				ELSE
				BEGIN
					AlwaysContinuous := GetBoolean(9);
					Continuous := AlwaysContinuous
				END;
				DoErase := GetBoolean(11);
				DemoPoincare := GetBoolean(12);
				LargePoincare := GetBoolean(14);
				PeriodPause := GetBoolean(16);
			END; {Not Escaped}
			Done
		UNTIL Not Error;
		IF (Abs( SavePos-MaxPos) > 0.00005)
			OR (Abs( SaveVel-MaxVel) > 0.00005) THEN
			  BEGIN FreezeScales := False; FreezeEnergy := True END;
		IF (Abs(SaveTs-TimeScale) > 0.00005) THEN  FreezeTimeScale := True;
		DisplayMainMenu;
		ClearPhasePort; FreezeEnergy := False
	END; {SetOptions}


	PROCEDURE GetInteresting;
	VAR
		InCondMenu : TMenu;
	BEGIN
		WITH InCondMenu DO
		BEGIN
			init;
			column(1, 'Cubic');
			row(1, 1, 'Ueda Case A');
			row(1, 2, 'Ueda Case B');
			row(1, 3, 'Ueda Case C');
			row(1, 4, 'Undriven, undamped');
			row(1, 5, 'Default Values');
			row(1, 6, 'Quit without changing');
			column(2, 'Pendulum');
			row(2, 1, 'Chaos');
			row(2, 2, 'Undriven, undamped');
			row(2, 3, 'Default Values');
			row(2, 4, 'Quit without changing');
			column(3, 'SHO');
			row(3, 1, 'Undriven, undamped');
			row(3, 2, 'Default Values');
			row(3, 3, 'Quit without changing');
			column(4, 'Two Walls');
			row(4, 1, 'Short Limit Cycle');
			row(4, 2, 'Long Limit Cycle');
			row(4, 3, 'Chaotic');
			row(4, 4, 'Undriven, undamped');
			row(4, 5, 'Default Values');
			row(4, 6, 'Quit without changing');
			column(5, 'Van Der Pol');
			row(5, 1, 'Birkhoff-Shaw Attractor');
			row(5, 2, 'Undriven, undamped');
			row(5, 3, 'Default Values');
			row(5, 4, 'Quit without changing');
			display;
			DynamicPause;
			if Chosen THEN
			 CASE colChosen OF
			  1 : BEGIN
				  ChooseCubic;
				  CASE RowChosen OF
				  1 : BEGIN
						kOverM := 0.0; Mass := 1.0; C3overM := 1.0;
						F0overM := 7.5; omega := 1.0; DoverM := 0.05;
						MaxPos := 4.0; MaxVel := 8.0; FreezeScales := False;
						FreezeEnergy := True
						END;
				  2 : BEGIN
						kOverM := 0.0; Mass := 1.0; C3overM := 1.0;
						F0overM := 8.5; omega := 1.0; DoverM := 0.25;
						MaxPos := 4.0; MaxVel := 8.0; FreezeScales := False;
						FreezeEnergy := True
						END;
				  3 : BEGIN
						kOverM := 0.0; Mass := 1.0; C3overM := 1.0;
						F0overM := 12.0; omega := 1.0; DoverM := 0.1;
						MaxPos := 4.0; MaxVel := 10.0; FreezeScales := False;
						FreezeEnergy := True
						END;
				  4 : BEGIN
						F0overM := 0.0; omega := 1.0;
						DoverM := 0.0; ShowPeriods := 2
						END;
				  5 : BEGIN
						kOverM := 0.0; Mass := 1.0; C3overM := 1.0;
						F0overM := 0.0; omega := 1.0; DoverM := 0.0
						END;
				  6 : ; {No Action}
			  End; {Case Row}
			  END; {Column 1}
			  2 : BEGIN
				  ChoosePendulum;
				  CASE RowChosen OF
				  1 : BEGIN
						gOverL := 1.0; PendL := 1;
						F0overM := 0.7; omega := 0.7; DoverM := 0.16;
						MaxPos := 4.0; MaxVel := 3.0; FreezeScales := False;
						FreezeEnergy := True
						END;
				  2 : BEGIN
						F0overM := 0.0; omega := 1.0; DoverM := 0.0
						END;
				  3 : BEGIN
						gOverL := 9.8; PendL := 1.0;
						F0overM := 0.0; omega := 1.0;
						DoverM := 0.0; ShowPeriods := 2
						END;
				  4 : ; {No Action}
			  End; {Case Row}
			  END; {Column 2}
			  3 : BEGIN
				  ChooseSHO;
				  CASE RowChosen OF
				  1 : BEGIN
						F0overM := 0.0; omega := 1.0; DoverM := 0.0;
							IF kOverM = 0.0 THEN kOverM := 1.0 {otherwise don't chg}
						END;
				  2 : BEGIN
						kOverM := 1.0; Mass := 1.0;
						F0overM := 0.0; omega := 1.0;
						DoverM := 0.0; ShowPeriods := 2
						END;
				  2 : ; {No Action}
			  End; {Case Row}
			  END; {Column 3}
			  4 : BEGIN
				  ChooseWalls; 
				  CASE RowChosen OF
				  1 : BEGIN
							HalfWallSep := 0.4; DoverM := 0.1;
							F0overM := 0.3; omega := 0.45; koverm := 1; Mass := 1;
							MaxPos := 1.0; MaxVel := 0.6; FreezeScales := False;
						FreezeEnergy := True
						END;
				  2 : BEGIN
							HalfWallSep := 0.4; DoverM := 0.00001;
							F0overM := 0.2; omega := 0.45; koverm := 1; Mass := 1;
							MaxPos := 1.0; MaxVel := 0.6; FreezeScales := False;
						FreezeEnergy := True
						END;
				  3 : BEGIN
							HalfWallSep := 0.25; DoverM := 0.2;
							F0overM := 0.2; omega := 0.45; koverm := 1; Mass := 1;
							MaxPos := 1.0; MaxVel := 0.6; FreezeScales := False;
						FreezeEnergy := True
						END;
				  4 : BEGIN
							F0overM := 0.0; omega := 1.0; DoverM := 0.0;
							IF kOverM = 0.0 THEN kOverM := 1.0 {otherwise don't chg}
						END;
				  5 : BEGIN
							HalfWallSep := 0.5; DoverM := 0.0; ShowPeriods := 2;
							F0overM := 0.0; omega := 1.0; koverm := 1.0; Mass := 1.0
						END;
				  6 : ; {No Action}
			  END; {Case Row}
			  END; {Column4}
			  5 : BEGIN
				  ChooseVanDePol; Plus := True;
				  CASE RowChosen OF
				  1 : BEGIN
						F0overM := 0.25; omega := 1.57; DoverM := 10.0;
						VdepX0 := Sqrt(0.1); ShowPeriods := 2;
						kOverM := 0.7;
						MaxPos := 1.0; MaxVel := 1.2; FreezeScales := False;
						FreezeEnergy := True
						END;
				  2 : BEGIN
						F0overM := 0.0; omega := 1.0; DoverM := 0.0;
						ShowPeriods := 2;
						IF kOverM = 0.0 THEN kOverM := 1.0 {otherwise don't chg}
						END;
				  3 : BEGIN
						kOverM := 1.0; Mass := 1.0; VDepX0 := 1.0;
						F0overM := 0.0; omega := 1.0;
						DoverM := 1.0
						END;
				  4 : ; {No Action}
			  End; {Case Row}
			  END; {Column 5}
			END;{If Chosen}
			Done   {Release Menu Space}
		END; {With Menu}
		Debounce;
		IF Animating AND ((F0overM <> 0.0) OR (DoverM <> 0.0)) THEN
			QueryAnimation;
		CloseMyViewport(ClearUR);
		DisplayChoice; 
		DrawSystem;
		FindPotential(True);
		Energy := MaxEnergy/2; {Prevents futile energy scaling}
		DisplayPotential(Not Exact, HoldVScale);
		IF (DoverM > 0.0) THEN Continuous := True
			ELSE Continuous := AlwaysContinuous;
		DisplayMainMenu;
		ClearPhasePort;
		RunInstruct;
		ShowMouse
	END; {GetInteresting}
	
	PROCEDURE FlipPMaps(Return : Boolean);
	BEGIN   
		IF MapLength < 10 THEN
		BEGIN   
			Message ('Not enough data stored; click mouse or press ENTER.');
			StaticPause; DisplayMainMenu
		END
		ELSE FlipEm({Slow:} False, Return)
	END; {FlipPMaps}

	PROCEDURE SearchParams(FName : String);
	VAR
		i, j, k, temp, count : Integer;
		Hour1, Hour, Min1, Min, Sec1, Sec, Sec100A, Sec100 : Word;
		Din, Oin, Rin : Real;
		First, Second, Search : Boolean;
		SFile : SFileTyp;

	  PROCEDURE CalcTime(Temp: Integer);
	  VAR Sec2, Min2, Hour2 : Integer;
	  BEGIN
		  GetTime(Hour, Min, Sec, Sec100);
		  Hour := Hour-Hour1; Min := Min-Min1+60; Sec := Sec-Sec1+60;
		  Min := Min-1 + Sec DIV 60;
		  Hour := Hour-1 + Min DIV 60;
		  Sec := (Sec MOD 60)*Temp;
		  Min := (Min MOD 60)*Temp + Sec DIV 60;
		  Hour := Hour*Temp + Min DIV 60;
		  Sec := Sec MOD 60; Min := Min MOD 60;
		  Sec2 := Sec + Sec1; Min2 := Min1 + Min + Sec2 DIV 60;
		  Hour2 := (Hour1 + Hour + Min2 DIV 60) Mod 24;
		  Sec2 := Sec2 MOD 60; Min2 := Min2 MOD 60;
		  Message ('Time will be '+NumStr(Hour,2,0)+'h '+NumStr(Min,2,0)
						+'m till '+NumStr(Hour2,2,0)+'h '+NumStr(Min2,2,0)
						+'m. Press Q to Abort, G to go on to next');
		  IF Hour = 0 THEN
			  Message ('Time will be '+NumStr(Min,2,0)+'m '+NumStr(Sec,2,0)
			 +'s till '+NumStr(Hour2,2,0)+'h ' +NumStr(Min2,2,0)
			 +'m. Press Q to Abort, G to go on to next');
	  END; {CalcTime}

LABEL 1;
	BEGIN
		IF FName = 'ABORT' THEN BEGIN Message ('File not written'); Exit END;
		Search := True;
		ExactParameters(Search);
		IF Not Search THEN Exit;
		Assign(SFile, FName);
		ReWrite(SFile);
		If F0overM = 0.0 THEN F0overM := 0.00001; {Otherwise no Poincare diagram)
		Debounce;
		Event.ReadKey := Chr(0); {Need to forget last event}
		CloseMyViewport(ClearUR);
		Continuous := True; {So Scales will be set appropriately}
		Searching := True; Animating := False;
		ShowPoinAlone;
		SetPlots;
		Continuous := False; {So Searching will control number of points}
		Message ('Press Q to Abort, G to go on to next');
		Rin := DoverM; Din := F0overM; Oin := Omega;
		GetTime(Hour1, Min1, Sec1, Sec100A);
		First := True; Second := True;
		Count := 1;
		DrawSystem;
		FindPotential(False);
		DisplayPotential(False, False);
		For i := 1 to NDrive DO
		BEGIN
		 For j := 1 to NResist DO
		 BEGIN
		  For k := 1 to NOmega DO
		  BEGIN
			  DisplayChoice;
			  DisplayDissip;
			  CloseMyViewPort(PoincarePort);
			  Plus := True;
			  ShowTrajectory;
			  SaveSMap(SFile, Count);
			  Count := Count + 1;
			  IF Event.ReadKey in ['Q','q'] THEN GoTo 1;
			  IF First THEN
			  BEGIN
				  CalcTime(NDrive*NResist*NOmega);
				  First := False
			  END; {If First}
			  Omega := Omega + DOmega
		  END; {Vary Omega}
		  IF Second THEN
		  BEGIN
			  CalcTime(NDrive*NResist);
			  Second := False
		  END; {If First}
		  Omega := Oin;
		  DoverM := DoverM + DResist
		 END; {Vary DoverM}
		 DoverM := Rin;
		 F0overM := F0overM + DDRive
		END; {Vary Driving Amplitude}
1:    Close(SFile);
		Searching := False;
		Continuous := True;
		F0overM := Din      
	END; {SearchParams}

{------------- Main Menu routine is mostly procedure calls -----------------}
VAR Error, Search, Driven : Boolean; {Only needed as a parameter; not used here}
BEGIN {HandleMenu}
	Search := False;
	WITH MainMenu DO
	CASE colChosen OF
	 1 : BEGIN CASE rowChosen OF
		   1 : Help(OpenScreen);
			2 : AboutCups;
			3 : Configuration;
			5 : SavePars(PutFName('.par'));
			6 : ReadPars(GetFName('.par'));
			7 : SavePMap(PutFName('.pmp'));
			8 : BEGIN
				 ReadPMap(GetFName('.pmp'), {Flip=} True, Error, Zero,MapLength);
				 Zero := 0 {Has to be reset here}
				 END;
			9 : Read2PMaps(GetFName('.pmp'),GetFName('.pmp'));
			10: ReadSMap(GetFName('.smp'));
			11 : SaveHist;
			12 : SaveResonance(PutFname('.res'));
			13 : ReadResonance(GetFname('.res'));
			15 : BEGIN HelpMenu.Done; MainMenu.Done; CupsDone; Halt END;
		 END; {Case Row}
		 DisplayMainMenu
		 END; {Column 1}
	 2 : CASE rowChosen OF
			1 : BEGIN FreezeScales := True;
					FreezeEnergy := True;
					ClearPhasePort;
					DisplayPotential(BothV, HoldVScale)
				 END;
			2 : BEGIN FreezeScales := False;
					FreezeEnergy := False;
					ClearPhasePort;
					DisplayPotential(BothV, HoldVScale)
				 END;
			3 : ToggleBothSides;
			4 : BEGIN
				 Animating := (Not (Animating));
				 If Animating THEN AnimString := 'Off' ELSE AnimString := '';
				 Continuous := Animating OR AlwaysContinuous;
				 MainMenu.row(2, 4, 'Animate '+AnimString);
				 DisplayMainMenu
				 END;
			5 : SelectEnergyScale;
			7 : MakePeriodTable;
			8 : MakeResonanceTable;
			9 : IF (Not Wrapped) THEN
				 BEGIN
					IF IHist < 32 THEN MildANnounce('Not Enough Data')
					ELSE IF IHist < MaxSub THEN
						FourierTransform(IHist+1, IHist, Wrapped)
					ELSE  FourierTransform(MaxSub, IHist, Wrapped)
				 END
				 ELSE  FourierTransform(MaxSub, IHist, Wrapped);
			10: FlipPMaps({Return:} False);
			11 : FlipPMaps({Return:} True);
			12 : IF TimePlotInverted THEN
					BEGIN TimePlotInverted := False; ShowXandPhase;
					  rowChecked(2,12, False) END
					ELSE
					BEGIN TimePlotInverted := True; ShowXVP;
					  rowChecked(2,12, True) END;
		  END; {case rowChosen in Column 1}
	 3 : BEGIN
			FreezeScales := True; FreezeEnergy := True;
			CASE rowChosen OF
			1 : ShowTimeAlone;
			2 : ShowVelAlone;
			3 : ShowPhaseAlone;
			4 : ShowPoinAlone;
			5 : ShowReturnAlone;
			7 : ShowTwo;
			8 : ShowFour({Return :} False);
			10: ShowPeriod('P');
			11: ShowPeriod('R');
			13: SetOptions;
			END {case rowChosen in Column 3}
		  END;
	 4 : CASE rowChosen OF
			1 : BEGIN ChooseAsymmetric; NewSystem({Clear: } True) END;
			2 : BEGIN ChooseCubic; NewSystem({Clear: } True) END;
			3 : BEGIN ChoosePendulum; NewSystem({Clear: } True) END;
			4 : BEGIN ChooseSHO; NewSystem({Clear: } True) END;
			5 : BEGIN ChooseWalls; NewSystem({Clear: } True) END;
			6 : BEGIN ChooseVanDePol; NewSystem({Clear: } True) END;

			8 : Case WhichForce of
					Walls : ShowHelp('anharm.hlp','Walls;');
					Asymmetric : ShowHelp('anharm.hlp','Asymmetric;');
					Pendulum : ShowHelp('anharm.hlp','Pendulum;');
					VanDePol : ShowHelp('anharm.hlp','VanDePol;');
					SHO  : ShowHelp('anharm.hlp','SHO;');
					Cubic : ShowHelp('anharm.hlp','Cubic;');
				 END;
			9 : ExactParameters(Search);
			10: GetInteresting;
			11: BEGIN SearchParams(PutFName('.smp')); DisplayMainMenu END;
			12: ToggleExactAccel
		  END; {case rowChosen in Column 4}
	 5 : Case RowChosen OF
			1 : HandleHelpMenu;
			2 : Tutorial(False);
		  END {case rowChosen in Column 5}
	END; {case colChosen}
END; {HandleMenu}

Procedure SetUpPorts;
BEGIN
	DefineMyViewPort (LeftPort, 0.07, 0.65, 0.12, 0.92);  {MyViewPort allows > 10}
	DefineMyViewPort (LowerLeft, 0.07, 0.64, 0.12, 0.46);
	DefineMyViewPort (UpperLeft, 0.07, 0.64, 0.54, 0.92);
	DefineMyViewPort (LeftUpperLeft, 0.07, 0.33, 0.54, 0.92);
	DefineMyViewPort (RightUpperLeft, 0.38, 0.64, 0.54, 0.92);
	DefineMyViewPort (LeftLowerLeft, 0.07, 0.33, 0.12, 0.46);
	DefineMyViewPort (RightLowerLeft, 0.38, 0.64, 0.12, 0.46);
	DefineMyViewPort (RightPort, 0.67, 1.00, 0.12, 0.92);
	DefineMyViewPort (UpperRight, 0.75, 0.97, 0.64, 0.92);
	DefineMyViewPort (ClearUR, 0.66, 0.97, 0.63, 0.93);
	DefineMyViewPort (MidRight, 0.7, 0.99, 0.425, 0.635);
	DefineMyViewPort (LowerRight, 0.75, 0.97, 0.12, 0.41);
	DefineMyViewPort (ClearLLX, 0.0, 0.70, 0.06,0.115); {These are needed to}
	DefineMyViewPort (ClearLY, 0.0, 0.064, 0.08, 0.94);   {clear axis legends}
	DefineMyViewPort (ClearLUY, 0.0, 0.064, 0.5, 0.94);
	DefineMyViewPort (ClearLRY, 0.651, 0.76, 0.11, 0.44);
	DefineMyViewPort (ClearLRX, 0.68, 0.99, 0.06, 0.11);
	DefineMyViewPort (ClearURY, 0.651, 0.76, 0.5,0.93);
	DefineMyViewPort (ClearPeriod, 0.7, 0.99, 0.53, 0.58);
	DefineMyViewPort (ClearDissip, 0.66, 0.99, 0.425, 0.52);
	DefineMyViewPort (ClearLMidX, 0.0, 0.667, 0.463, 0.540);
	DefineMyViewPort (ClearLMidY, 0.331, 0.379, 0.08, 0.94);
	DefineMyViewPort (PLegend, 0.7, 0.99, 0.380, 0.600);
	DefineMyViewPort (GetEnergy, 0.72, 0.75, 0.12, 0.73);
	DefineMyViewPort (GetR, 0.74, 0.77, 0.64, 0.88);
	DefineMyViewPort (GetDrive, 0.84, 0.87, 0.64, 0.88);
	DefineMyViewPort (GetF, 0.94, 0.97, 0.64, 0.88);
	DefineMyViewPort (AllPort, 0.0, 1.0, 0.07, 0.93);
	VPort := LowerRight; ButtonPort := MidRight;
	WallPort := UpperRight; WallSNum := PositionSNum
END; {SetUpPorts}

Procedure SelectLeftPort;
BEGIN
	SelectMyViewport(LeftPort)
END;

Procedure CloseAllPort;
BEGIN
	CloseMyViewPort(AllPort)
END;

Procedure OpenAllPort;
BEGIN
	OpenMyViewPort(AllPort)
END;

{------------ Displays wall separation and energy values -----------------}

PROCEDURE displayChoice;
VAR
	text : TextSettingsType;
	h : integer;
BEGIN
	CloseMyViewPort(ButtonPort);
	SetColor(White);
	h := TextHeight('O')+4;
	HideMouse;
	Case WhichForce OF
		Walls : OutTextXY(0,5, 'Wall Separation = '+numStr(2*HalfWallSep,3,2));
		Pendulum : OutTextXY(0,5, 'g = '+numStr(gOverL*PendL,4,1));
		VanDePol : OutTextXY(0,5, 'Drag = '+numStr(DoverM*Mass,4,1));
		Sho : OutTextXY(0,5, 'k = '+numStr(KoverM*Mass,5,2));
		Asymmetric   : OutTextXY(0,5, 'A2 = '+numStr(Ascoef3,5,2));
		Cubic : OutTextXY(0,5, 'C3 = '+numStr(C3OverM*Mass,5,2))
	END; {Case}
	IF WhichForce <> VanDePol THEN
		OutTextXY(0,5+h,'Energy =  '+numStr(Energy,5,3))
	ELSE
		OutTextXY(0,5+h,'Initial x =  '+numStr(XStart,5,3));
	IF DoverM > 0.0 THEN DisplayDissip;
	ShowMouse;
END; {displayChoice}

{------------ Displays friction and driving force params when non-zero --------}

PROCEDURE displayDissip;
VAR
	text : TextSettingsType;
	h : integer;
BEGIN
	CloseMyViewPort (ClearDissip);
	SetColor(White);
	h := TextHeight('O')+4;
	HideMouse;
	OutTextXY(25,1,'Drag/M = '+numStr(DoverM,5,2));
	OutTextXY(25,1+h,'F0/M = '+numStr(F0overM,5,2));
	OutTextXY(25,1+2*h,'Omega = '+numStr(Omega,6,3));
	ShowMouse;
END; {displayDissip}

PROCEDURE RunInstruct;
VAR
	text : TextSettingsType;
	h : integer;
BEGIN
	If DoverM = 0.0 THEN
	BEGIN
	CloseMyViewPort (ClearDissip);
	SetColor(White);
	h := TextHeight('O')+4;
	HideMouse;
	OutTextXY(1,1,'To RUN: click in v vs. x');
	OutTextXY(1,1+h,'OR Potential vs. x diagram');
	OutTextXY(1,1+2*h,'OR press ENTER.');
	ShowMouse
	END {If DoverM}
END; {RunInstruct}

PROCEDURE ClearDissipPort;
BEGIN
	SelectMyViewPort(ClearDissip);
	ClearViewPort
END; {ClearDissipPort}

PROCEDURE ClearLegend;
BEGIN
	SelectMyViewPort(PLegend);
	ClearViewPort
END; {ClearLegend}

{------------ Displays result of period calculation ----------------}

PROCEDURE displayPeriod(t : Real);
VAR
	text : TextSettingsType;
	h : integer;
BEGIN
	CloseMyViewPort (ClearPeriod);
	SelectMyViewPort(ButtonPort);
	SetColor(White);
	h := TextHeight('O')+4;
	HideMouse;
	OutTextXY(0,5+2*h,'Period = '+numStr(t,5,2));
	ShowMouse
END; {displayPeriod}

{ ------ Selects # of Significant ficures to display on labels ---------}

FUNCTION AxisRound(TickSize: Real; Sig : Integer): Real;
VAR
	RTick : Real;
	ITick : Integer;
BEGIN
	IF TickSize < Pwr(10.0,-Sig) THEN AxisRound := TickSize
	ELSE
	BEGIN
		Case Sig Of
			3 : If TickSize < 0.02 THEN ITick := 1000 ELSE ITick := 100;
			2 : If TickSize < 0.2 THEN ITick := 100;  {Set scaling}
		END; {Case}
		If TickSize >= 0.2 Then ITick := 10;
		If TickSize >= 2.0 Then ITick := 1;
		IF TickSize < 0.02 THEN ITick := 2 * ITick;
		RTick := Trunc(ITick*TickSize/2); {Reduces to 2 significan figures}
		RTIck := RTick/ITick;
		IF TickSize < 0.02 THEN RTick := 2 * RTick;
		AxisRound := Rtick
	END {Else}
END; {AxisRound}

{----- Redraws the potential display as needed -------}

Procedure DisplayPotential (ShowBoth, HoldScale: Boolean);
VAR
	MaxV, RTick, min, max : Real;
	TempV :Dvector;
	h, VTick, imin, imax : Integer;
BEGIN
 HideMouse;
 IF WhichForce = VanDePol THEN
 BEGIN
	CloseMyViewPort(ClearLRY);
	CloseMyViewPort (ClearLRX);
	ClearDissipPort;
	CloseMyViewPort (VPort);
	h := TextHeight('O')+4;
	OutTextXY(1,1,'  Doesn''t have');
	OutTextXY(1,1+h,'  a potential.');
	OutTextXY(1,1+3*h,'   Click in');
	OutTextXY(1,1+4*h,'  Van Der Pol');
	OutTextXY(1,1+5*h,' picture above');
	OutTextXY(1,1+6*h,'or Phase diagram');
	OutTextXY(1,1+7*h,'to launch motion.')
 END ELSE
 BEGIN
	SetColor(White);
	CloseMyViewPort(ClearLRY);
	CloseMyViewPort (ClearLRX);
	OpenMyViewPort (VPort);
	SelectScale(VSNum);
	PutLabel(Inside,'Potential vs. x');
	IF WhichForce = Pendulum THEN PutLabel(Bottom,'Angle (rad)')
	ELSE PutLabel(Bottom,'x (m)');
	PutLabel(Left,'V (joules)');
	IF Exact THEN MaxV := xactpotential.value(41)
	ELSE MaxV := potential.value(41);
	IF (Not HoldScale) AND (xactpotential.value(21) > MaxV) THEN
	BEGIN
		MaxV := xactpotential.value(21);
		GetX(MaxPos, MaxV+Emin);
		IF MaxPos = 0.0 THEN {Happens if asymmetric potentialhas only local minimum}
		BEGIN
			GetX2(MaxPos, MaxV+Emin);
			MaxPos := Abs(MaxPos)
		END;
		FindPotential({SetScale =} True);
		IF xactpotential.value(41) > MaxV THEN {Could be less after change}
			MaxV := xactpotential.value(41);
		END;
	If (Not HoldScale) AND (WhichForce = Walls) AND (Not Exact) THEN
		DefineScale(VsNum,-MaxPos,MaxPos, 0.0, potential.value(41));
	If (WhichForce = Pendulum) AND (Not HoldScale) THEN
	BEGIN
		TempV.Init(41);
      IF MaxPos >= Pi THEN TempV.Equate(1.5, xactpotential)
		ELSE TempV.Equate(1.0, xactpotential);
		MyAutoScaling(VSNum, xpos, TempV);
		TempV.Free
	END;
	xactPotential.MinMax(IMin, Min, IMax, Max);
	Axis(0.0, 0.0, AxisRound(2*xpos.value(41),3), AxisRound(Max,3));
	PlotDVectors(xpos, xactpotential,1,41);
	IF ShowBoth THEN
	BEGIN
		SetColor(LightCyan);
		PlotDVectors(xpos, potential,1,41)
	END
 END; {ELSE Not VanDePol}
 ShowMouse
END; {DisplayPotential}

{------------ Draws the springs at the current x position as needed -------}
{------------     Used by system drawing and animation routines     -------}

Procedure DrawSprings (x : Real; Color, MassColor : Integer);
BEGIN
	SetColor(Color);
	Plotline (0.0,0.0, x, HalfWallSep);
	Plotline (0.0,2*HalfWallSep, x, HalfWallSep);
	DrawCircle (x ,HalfWallSep, 4, MassColor);
END; {DrawSprings}

{---------- Main loop calculates trajectory and draws all graphs -----------}
{----------   Handles animation and puts energy on energy graph  ----------- }

{------------ Draws the two walls in the correct position ---------------}

Procedure DrawWalls;
CONST
	Above = 1; Below = -1;

	PROCEDURE OneWall (Position : Real; Above : Integer);
	VAR OtherSide : Real; i : Integer;
	BEGIN
		SetColor (LightRed);
		OtherSide := Position + Above*0.08;
		PlotLine (-0.45, Position,   0.45, Position);
		PlotLine (-0.45, OtherSide,  0.45, OtherSide);
		PlotLine (-0.45, Position,  -0.45, OtherSide);
		PlotLine (0.45, Position,  0.45, OtherSide);
		For i := 1 to 9 DO
		  PlotLine (-0.49+0.09*i, Position,  -0.41+0.09*i, OtherSide)
	END; {OneWall}

BEGIN
	CloseMyViewport(ClearUR);
	SelectMyViewPort(WallPort);
	DefineScale (WallSNum, -0.5, 0.5,  -0.1, 1.1);
	SelectScale (WallSNum);
	YTics(0.0, 0.5);
	OneWall (2*HalfWallSep, Above);
	OneWall (0.0, Below);
	If HalfWallSep >= RestLength THEN
		BEGIN
			Equilibrium := 0.0;
			Stretched := True;
			TempColor := Green
		END
		ELSE BEGIN
		  Equilibrium := Sqrt (Sqr(RestLength) - Sqr(HalfWallSep));
		  Stretched := False;
		  TempColor := LightMagenta
		END;
	If Not Plus THEN Equilibrium := -Equilibrium;
	DrawSprings (Equilibrium, TempColor, Red);
	GraphBackColor := DarkGray;
END; {DrawWalls}

{--- Draw the pendulum at the current position for DrawSystem or animation ----}

Procedure DrawPendulum (Theta : Real; TempColor : Integer);
VAR
	x, y : Real;
	Ix, Iy : Integer;
BEGIN
	SetColor (TempColor);
	x := PendL*Sin(Theta); y := -PendL*Cos(Theta);
	PlotLine (0.0, 0.0,  x, y);
	Map (X, y,  ix, iy);
	Circle (ix, iy, 3)
END; {DrawPendulum}

PROCEDURE ShowPendulum (Theta : Real; TempColor : Integer);
VAR x :Real;
BEGIN
	GraphBackColor := Blue;
	CloseMyViewport(ClearUR);
	OpenMyViewPort(WallPort); 
	x := 1.1*PendL;
	DefineScale (WallSNum, -x, x,  -x, x);
	SelectScale (WallSNum);
	YTics(-1.0*PendL, 0.5*PendL);
	DrawPendulum (Theta, TempColor);
	GraphBackColor := DarkGray
END; {ShowPendulum}

PROCEDURE DrawGeneral(xsize : Real; Color : Integer);
VAR i : Integer;
	 dx : Real;
BEGIN
	GraphBackColor := Color;
	CloseMyViewport(ClearUR);
	OpenMyViewPort(WallPort); 
	DefineScale (WallSNum, -xsize, xsize,  -xsize, xsize);
	SelectScale (WallSNum);
	SetColor (White);
	PlotLine (-xsize,-0.125*xsize, xsize,-0.125*xsize);
	dx := xsize/11;
	For i := 0 to 9 DO
	  PlotLine (-xsize+dx+2*dx*i, -0.125*xsize,  -xsize+3*dx+2*dx*i, -xsize);
	PlotLine (-xsize, -0.55*xsize,  -xsize+dx, -Xsize);
	PlotLine (xsize-dx, -0.125*xsize,  xsize, -0.55*xsize);
	DrawCircle (0.0, 0.0, 5, White);
	Case WhichForce of
		VanDePol : PutLabel(Inside,'Van Der Pol');
		Asymmetric : PutLabel(Inside,'Asymmetric');
		SHO      : PutLabel(Inside,'Simple Harmonic');
		Cubic    : PutLabel(Inside,'Cubic Force')
	END; {Case}
	GraphBackColor := DarkGray
END; {DrawGeneral}

{-------- Displays whichever system is being simulated ---------}

Procedure DrawSystem;
BEGIN
	Case WhichForce of
	  Walls : DrawWalls;
	  Pendulum : ShowPendulum (0.0, White);
	  VanDePol : DrawGeneral (MaxPos, Red);
	  SHO      : DrawGeneral (MaxPos, Brown);
	  Cubic : DrawGeneral (MaxPos, Magenta);
	  Asymmetric : DrawGeneral (MaxPos, Blue)
	END; {Case}
	SetColor(White)
END; {DrawSystem}

{-------- Clears all the graphs ---------------}

Procedure ClearPhasePort;
VAR
	MaxX, MaxV : Real;
	UsePeriod, TTicks, i, ie : Integer;
	PosString : String[1]; PosUnits : String[5];
	VelString : String[1]; VelUnits : String[9];

BEGIN {ClearPhasePort}
	IF PlotPoincare THEN BlankPoincare := False;
	UsePeriod := ShowPeriods; {A large number avoids message from continuous}
	If UsePeriod > 10 THEN UsePeriod := 2; {To Set Scale Properly}
	PosString := 'x'; PosUnits := '(m)';
	VelString := 'v'; VelUnits := '(m/sec)';
	HideMouse;
	IF Plots = 4 THEN TTicks := 2 ELSE TTicks := 4;
	CloseMyViewPort (ClearLLX);
	CloseMyViewPort (ClearLY);
	IF (LastPlots > 1) OR (Plots > 1) THEN CloseMyViewPort (ClearLMidX);
	IF (LastPlots > 2) OR (Plots > 2) THEN CloseMyViewPort (ClearLMidY);
	CloseMyViewPort (LeftPort);
	LastPlots := Plots;
	IF WhichForce = Pendulum THEN
   BEGIN
		PosString := Chr(233) ; PosUnits := '(rad)';
		VelString := 'w'; VelUnits := '(rad/sec)'
   END;
	IF FreezeEnergy THEN MaxX := MaxPos
	ELSE
	BEGIN
		IF F0overM > 0.0 THEN {Use time history to determine scale}
		BEGIN
			If Wrapped THEN ie := HistoryCount ELSE ie := iHist;
			MaxX := 0.0; MaxV := 0.0;
			For i := 0 to ie DO
			BEGIN
				IF Abs(Vs[i]) > MaxV THEN MaxV := Abs(Vs[i]);
				IF Abs(Xs[i]) > MaxX THEN MaxX := Abs(Xs[i]);
			END;
			MaxX := 2*MaxX;
			IF MaxV > 0.0 THEN MaxVel := 2*MaxV; {Otherwise don't change}
			If MaxX > 0.0 THEN MaxPos := MaxX ELSE MaxX := MaxPos
		END {Scale from time history}
		ELSE {Get scale from energy last used}
		BEGIN
		IF WhichForce <> VanDePol THEN
			IF Exact THEN
				CASE WhichForce of
					Asymmetric : 
						MaxVel := Sqrt(4*Abs(AsymmetricPotential(MaxPos)-Emin)/Mass);
					Cubic : 
						MaxVel := Sqrt(4*(CubicPotential(MaxPos)-Emin)/Mass);
					Pendulum : 
						MaxVel := Sqrt(4*(ExactPendPotential(MaxPos)-Emin)/Mass);
					SHO : 
						MaxVel := Sqrt(4*(SHOPotential(MaxPos)-Emin)/Mass);
					Walls : 
						MaxVel := Sqrt(4*(ExactWallPotential(MaxPos)-Emin)/Mass);
					VanDePol : {No potential};
				END {Case Ignore i}
			ELSE
				Case WhichForce of
					Pendulum : 
						MaxVel := Sqrt(4*(ApproxPendPotential(MaxPos)-Emin)/Mass);
					Walls : 
						MaxVel := Sqrt(4*(ApproxWallPotential(MaxPos)-Emin)/Mass)
				{The other forces have no approximate expressions}
				END; {Case}

		CASE WhichForce OF
		  Cubic : IF Not Continuous THEN
						MaxX := MaxPos
					 ELSE IF F0overM = 0.0 THEN
						MaxX := MaxPos
					 ELSE BEGIN 
						MaxX := 4.0;
						MaxVel := 8.0
					 END;
		  Walls : IF Not Continuous THEN
						MaxX := MaxPos
					 ELSE IF F0overM = 0.0 THEN
						MaxX := MaxPos
					 ELSE BEGIN
						MaxX := 1.5;
						MaxVel := 2
					 END;
		  Pendulum : BEGIN
							 IF Not Continuous THEN
								 MaxX := MaxPos
							 ELSE IF F0overM = 0.0 THEN
								 MaxX := MaxPos
							 ELSE  MaxX := 4.0
						 END; {Pendulum}
		  VanDePol : BEGIN 
							MaxX := MaxPos;
							MaxVel := Sqrt(4*(SHOPotential(MaxPos)-Emin)/Mass)
						 END;
		  SHO, Asymmetric : MaxX := MaxPos
		END; {Case}
		MaxX := Abs(MaxX);
		MaxPos := MaxX
		END {Get scale from Energy}
	END; {Not FreezeScales}
	IF (PlotTime OR PlotVel) AND Not FreezeTimeScale THEN
	BEGIN
		CASE WhichForce OF
		  Walls, VanDePol :
					IF Animating OR Not Continuous OR (DoverM = 0.0) THEN
						TimeScale := 24*Sqrt(1/kOverM)*UsePeriod
					 ELSE IF F0overM = 0.0 THEN
						TimeScale := 6*Sqrt(1/kOverM)*5/Pwr(DoverM+0.01,0.6)
					 ELSE TimeScale := 6*Sqrt(1/kOverM)*10*UsePeriod;
			SHO, Asymmetric :
					IF Animating OR Not Continuous OR (DoverM = 0.0) THEN
						TimeScale := 1.01*TwoPi*Sqrt(1/kOverM)*UsePeriod
					 ELSE IF F0overM = 0.0 THEN
						TimeScale := 30*Sqrt(1/kOverM)/Pwr(DoverM+0.01,0.6)
					 ELSE TimeScale := 6*Sqrt(1/kOverM)*10*UsePeriod;
		  Cubic :
					IF Animating OR Not Continuous OR (DoverM = 0.0) THEN
						TimeScale := 36*Sqrt(1/(C3overM+kOverM))*UsePeriod
					 ELSE IF F0overM = 0.0 THEN
						TimeScale := 6*Sqrt(1/(C3overM+kOverM))*5/Pwr(DoverM+0.01,0.6)
					 ELSE TimeScale := 6*Sqrt(1/(C3overM+kOverM))*10*UsePeriod;
		  Pendulum : IF Animating OR Not Continuous OR (DoverM = 0.0) THEN
							TimeScale := 12*Sqrt(1/gOverL)*UsePeriod
						  ELSE IF F0overM = 0.0 THEN
							 TimeScale := 6*Sqrt(1/gOverL)*5/Pwr(DoverM+0.01,0.8)
						  ELSE TimeScale := 6*Sqrt(1/gOverL)*10*UsePeriod;
		END; {Case}
	END; {IF Time or Velocity}
	SetColor(White);
		IF Not FreezeScales THEN
		BEGIN {Define all scales to be available when frozen}
			DefineScale(TimeSNum, 0.0, TimeScale, -1.1*MaxX, 1.1*MaxX);
			IF TimePlotInverted THEN
				DefineScale(TimeSNum, -1.1*MaxX, 1.1*MaxX, 0.0, TimeScale);
			DefineScale(PhaseSNum, -1.1*MaxX, 1.1*MaxX, -MaxVel, MaxVel);
			DefineScale(VelSNum, 0.0, TimeScale, -1.1*MaxVel, 1.1*MaxVel);
			DefineScale(ReturnSNum, -MaxVel, MaxVel, -MaxVel, MaxVel);
		END;
	IF PlotTime THEN
	BEGIN
		OpenMyViewPort(TimePort);
		IF TimePlotInverted THEN
		BEGIN
			PutLabel(Inside,'t vs. '+PosString);
			PutLabel(Left,'t (sec)');
			PutLabel(Bottom,PosString+' '+PosUnits)
		END
		ELSE
		BEGIN
			PutLabel(Inside,PosString+' vs. t');
			PutLabel(Bottom,'t (sec)');
			PutLabel(Left,PosString+' '+PosUnits)
		END;
		SelectScale(TimeSNum);
		IF TimePlotInverted THEN
			Axis (0.0, 0.0, AxisRound(MaxX,2),AxisRound(TimeScale/TTicks,2))
		ELSE
			Axis (0.0, 0.0, AxisRound(TimeScale/TTicks,2), AxisRound(MaxX,2))
	END;
	IF PlotVel THEN
	BEGIN
		OpenMyViewPort(VelocityPort);
		PutLabel(Inside,VelString+' vs. t');
		PutLabel(Bottom,'t (sec)');
		IF Plots < 4 THEN PutLabel(Left,VelString+' '+VelUnits);
		SelectScale(VelSNum);
		Axis (0.0, 0.0, AxisRound(TimeScale/TTicks,2), AxisRound(MaxVel,2));
	END;
	IF PlotPhase THEN
	BEGIN
		OpenMyViewPort(PhasePort);
		SelectScale(PhaseSNum);
		PutLabel(Inside,'Phase Plot: '+VelString+' vs. '+PosString);
		PutLabel(Left,VelString+' '+VelUnits);
		PUtLabel(Bottom,PosString+' '+PosUnits);
		Axis (0.0, 0.0, AxisRound(MaxX,2), AxisRound(MaxVel,2));
	END;
	IF PlotPoincare OR BlankPoincare THEN {when balked, check if Fo changed}
	BEGIN
		CloseMyViewPort(PoincarePort);
		SelectScale(PhaseSNum);
		IF (Not HidePoincare) AND (F0overM <> 0.0) THEN
		BEGIN
			PutLabel(Inside,'Poincar‚ Map ('+VelString+' vs. '+PosString+')');
			IF Plots < 4 THEN PutLabel(Left,VelString+' '+VelUnits);
			PutLabel(Bottom,PosString+' '+PosUnits);
			Axis (0.0, 0.0, AxisRound(MaxX,2), AxisRound(MaxVel,2));
			BlankPoincare := False; PlotPoincare := True
		END {We do want to use the port}
		ELSE
		BEGIN
			IF Not HidePoincare THEN
				PutLabel(Inside,'Not available, system undriven');
			PlotPoincare := False; {And don't plot in it}
(*			BlankPoincare := True*)
		END
	END; {PlotPoincare}
	IF PlotReturn THEN
	BEGIN
		CloseMyViewPort(ReturnPort);
		SelectScale(ReturnSNum);
		IF F0overM <> 0.0 THEN
		BEGIN
			PutLabel(Inside,'Return Map');
			IF Plots < 4 THEN PutLabel(Left,VelString+'2 '+VelUnits);
			PutLabel(Bottom,VelString+'1 '+VelUnits);
			Axis (0.0, 0.0, AxisRound(MaxVel,2), AxisRound(MaxVel,2))
		END
		ELSE
		BEGIN
			PutLabel(Inside,'Not available, system undriven');
			PlotReturn := False
		END
	END;
	NColor := 10;
	FreezeEnergy := False;
	ShowMouse
END; {ClearPhasePort}

PROCEDURE OpenTwoPoincare;
BEGIN
		CloseMyViewPort(AllPort);
		CloseMyViewPort(UpperLeft);
		PutLabel(Inside,'Comparing Poincar‚ Maps');
		SelectScale(PhaseSNum);
		Axis (0.0, 0.0, AxisRound(MaxPos,2), AxisRound(MaxVel,2));
		ClearViewPort;
		CloseMyViewPort(LowerLeft);
		Axis (0.0, 0.0, AxisRound(MaxPos,2), AxisRound(MaxVel,2));
		ClearViewPort
END; {TwoPoincare}

{----- Displays the previously calculated period vs. energy as only graph -----}

Procedure ShowPeriod(GType : Char);
VAR i, j, SNum, Curves : Integer;

PROCEDURE PrintLegend(GType : Char);
  {Adapted from the contour legend in CupsGrph}
  VAR
	  x,y,n,NumLen : integer;
	  increment, Low : Real;
	  strng : string;
	  Viewport : ViewPortType;
  BEGIN
	  SetTextJustify(LeftText,TopText);
	  GetViewSettings(viewPort);
	  NumLen := 9 * TextWidth('8');
	  Low := 0.0;
	  WITH ViewPort DO BEGIN x:=x2-x1-NumLen-5; y:=y2-y1-5  END;
	  SetColor(white);
	  CASE GType OF
	  'P' :
	  BEGIN
		Increment := DeltaW;
		Low := BaseVal-DeltaW;
		Case WhichForce OF
		  Walls :
			  OutTextXy (x-45,y-70+10*(5-Curves), 'Wall Separation');
		  SHO :
			  OutTextXy (x+20,y-70+10*(5-Curves), 'k');
		  Pendulum :
			  OutTextXy (x+20,y-70+10*(5-Curves), 'g');
		  VanDePol :
			  OutTextXy (x-80,y-70+10*(5-Curves), 'Limit Cycle Size Xo');
		  Asymmetric :
			  OutTextXy (x-5,y-70+10*(5-Curves), 'AsCoef3');
		  Cubic :
			  OutTextXy (x+10,y-70+10*(5-Curves), 'C3');
		END; {Case WhichForce}
	  END; {'P'}
	  'R' : BEGIN
				OutTextXY (x,y-70+10*(5-Curves),'Drag');
				Increment := DInc;
				Low := DBeg-DInc;
			  END
	  END; {Case GType}
	  rectangle (x-15,y-6*10+10*(5-Curves),x,y);
	  FOR n := 1 TO Curves DO
	  BEGIN
		  SetColor(rainbow[0].colors[((n-1) Mod 6)+10]);
		  line(x-14,y-n*10,x-1,y-n*10);
	  END;
	  SetColor(white);
	  FOR n := 1 TO Curves DO
	  OutTextXY(x+2,y-n*10-TextHeight('8') DIV 2, NumStr(Increment*n+Low,6,3));
  END; {PrintLegend}

BEGIN
	CASE GType OF
	  'P' : If Not HavePeriods THEN
		  BEGIN
			  Message ('Choose ''Plot Period'' First; Click Mouse or press Enter');
			  StaticPause;
			  DisplayMainMenu;
			  Exit
		  END;
	  'R' : IF Not HaveResonances THEN
		  BEGIN
			  Message ('Choose ''Plot Resonance'' First; Click Mouse or press Enter');
			  StaticPause;
			  DisplayMainMenu;
			  Exit
		  END
	  END; {CASE}
	HideMouse;
	CloseMyViewPort (ClearLLX);
	CloseMyViewPort (ClearLY);
	CloseMyViewPort (ClearLMidX);
	SetColor(White); {For plot borders}
	OpenMyViewPort(LeftPort);
	CASE GType OF
		'P' : BEGIN
				 IF WhichForce = VanDePol THEN PutLabel(Inside,'Period vs. Drag')
				 ELSE PutLabel(Inside,'Period vs. Energy');
				 SNum := PeriodSNum;
				 Curves := NPerCurves
				END;
		'R' : BEGIN
				  PutLabel(Inside, 'Amplitude vs. Omega');
				  SNum := ResonanceSNum;
				  Curves := NResCurves;
				  IF (Frequencies[1,1].Value(NResPts[1])
						<> Frequencies[1,1].Value(NResPts[1]-1))
					OR (Frequencies[1,2].Value(NResPts[2])
						<> Frequencies[1,2].Value(NResPts[2]-1))
					OR (Frequencies[1,3].Value(NResPts[3])
						<> Frequencies[1,3].Value(NResPts[3]-1)) THEN
					Announce('Display Memory Full, curves probably not complete.');
				END;
	END; {Case}
	SelectScale (SNum);
	Axis (GraphScales[Snum].XMin/2, GraphScales[Snum].YMin/2,
			AxisRound(GraphScales[Snum].XMax/2,2),
			AxisRound(GraphScales[Snum].YMax/2,2));
	PrintLegend(GType);
	NColor := 10;
	For i := 1 to Curves DO
	BEGIN
		SetMyColor(rainbow[0].colors[Ncolor]);
		CASE GType OF
			'P' : PlotDVEctors (Energies[i], Periods[i],1,ESteps);
			'R' : FOR j := 1 TO NumberOfResPlots[i] DO
						PlotDVEctors (Frequencies[i,j],
								Amplitudes[i,j],1,NResPts[j]);

		END; {Case}
		IF (GType = 'R') AND (NumberOfResPlots[i] = 2) THEN {Approximation was used}
			 PlotLine(Frequencies[i,2].Value(NResPts[2]),
						 Amplitudes[i,2].Value(NResPts[2]),
						 Frequencies[i,1].Value(NResPts[1]),
						 Amplitudes[i,1].Value(NResPts[1]));
		NColor := NColor + 1;
		IF NColor > 15 THEN NColor := 10
	END;
	StaticPause; Debounce;
	SetColor(White);
	ClearPhasePort;
	DrawSystem;
	ShowMouse;
	DisplayMainMenu
End; {ShowPeriod}

BEGIN
END. {UNIT AnhMenus}
