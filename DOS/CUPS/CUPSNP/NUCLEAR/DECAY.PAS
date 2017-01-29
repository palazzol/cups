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

 {$A-}
 {$N+}
 {$R+}
 {$M 65520,0,655360}

USES
	printer, crt, dos, graph, CupsMupp, CUPS, CUPSproc, Cupsfunc, CUPSgrph, CUPSgui, CUPSpars, CupsScrn;

 (*******   DEFINE GLOBAL CONSTANTS, TYPES AND VARIABLES *******)

TYPE
	Str80 = STRING[80];
	Str30 = STRING[30]; {y-axis vertical title}

CONST {EXTENDED KEY CODES}
	ArrowL = chr(75);
	CtrlArrowL = chr(115);
	ArrowR = chr(77);
	CtrlArrowR = chr(116);
	PgDn = chr(81);
	CtrlPgUp = chr(132);
	PgUp = chr(73);
	CtrlPgDn = chr(118);
	HomeKey = chr(71);
	ArrowUp = chr(72);
	EndKey = chr(79);
	ArrowDn = chr(80);
	AltD = chr(32);
	AltA = chr(30);
	Ins = chr(82);
	Del = chr(83);
	ESC = chr(27);
	RET = chr(13);
	TAB = chr(9);
	AP = chr(39);{apostrophe}
	F1key = chr(59);
	F2key = chr(60);
	F3key = chr(61);
	F4key = chr(62);
	F5key = chr(63);
	F6key = chr(64);
	F7key = chr(65);
	F8key = chr(66);
	F9key = chr(67);
	F10key = chr(68);
	ScUpA = chr(24);
	ScDnA = chr(25);
	ScRtA = chr(26);
	ScLfA = chr(27);

	SelectKeys = ['E', 'e', 'L', 'l'];
	ActionKeys = [ArrowR, ArrowL, ArrowUp, ArrowDn, PgUp, PgDn, HomeKey, EndKey, Ins, Del]; { action in E, L, P, or S mode }
	BarMenuKeys = [ArrowR, ArrowL, ArrowUp, ArrowDn, PgUp, PgDn, HomeKey, EndKey, RET];
	Blok = chr(219) + chr(219) + chr(219); { three solid-block characters }

CONST
	Bk: INTEGER = DarkGray;     {Background color for plots.}
	GMClr: integer = lightblue;    { GMT counts bar color }
	IdClr: integer = cyan;         { Ideal counts bar color }
	txt:   integer = yellow;
	GraphBoxColor:	INTEGER = LightGreen;   {Axes and LegEND color.}
	AxisLabelColor: INTEGER = White;
	KeyCl = cyan;                   {color of keys on menus}
	CCl = lightred;  {color of stuff which changes}
	ACl = lightgray; {lightblue}
  {color of XOR'ed active cursor}
	PCl = red; {light}
 {ditto for passive cursor}
	FshCl = 6;   {this xored w/ scr bkg gives yellow for poisson curve}
	QuitFlag:	BOOLEAN = FALSE;
	MainInfo: BOOLEAN = TRUE;
	CountInfo: BOOLEAN = TRUE;
	RatesInfo: BOOLEAN = TRUE;

CONST
	MAXNUM = 1936;
	FullVP = 0;  {Full screen viewport nr}
	ReplotVP1 = 1;
	PlotVP = 2;  {smaller area for plot}
	BotVP = 3;  {bottom 10%}
	TopVP = 4;  {top 10%}
	TitleVP = 5;  {for title line}
	InputVP = 6;  {for numeric inputs}
	StatusVP = 7;
	MainVP1 = 8;
	HelpTextVP = 9;
	GraphVP = 10; {incl titles & stuff}
	YAxisVP1 = 11;
	XAxisVP1 = 12;
	ColorPlotVP1 = 14;
	ColorReplotVP1 = 15;
	MaxNrVPs = 20; {Number of viewports - maximum of 20}
	vpl = 0.12; {left edge of plotvp; adjust for y ticks}
	heading = white;
	lgy = lightgray;
	FileSign = 'Files';

TYPE
	NuclearRec = RECORD
			Z, N: byte;
			mass: longint;
			mode: char;
			life: single;
		END; {record}
	Locator = RECORD
			x, y: integer;
		END; {record}
	Str12 = STRING[12];
	curs = ARRAY[1..4] OF Locator;
 { The data file is organized by increasing Atomic Mass Number A}
{
   for a given A, there are increasing Z's                      }

VAR
	HS: HelpScrType;  { global help screen }
	RScreen: TInputScreen;
{	TopMenu: TMenu;}
	VPList: ARRAY[1..MaxNrVPs] OF ViewPortType;
	StatX: ARRAY[1..10] OF real;
	StatY: ARRAY[1..38] OF real;
	CurrentVP: integer; {number of currently chosen viewport}
	lf, rt     {plot array limits}	: integer;
	OriginalRight: integer;
	MSel: char;
   HotKeysType : char;
	Cr: curs;                      {fit cursors}
	EqLbl: ARRAY[0..4] OF STRING[16]; {for fit equations}
	f: text;
	fname: STRING[80];
	ch: char;
	NuclideNr: integer;
	MaxZNr, MaxANr: integer;
	MaxNNr: integer;
	inside: boolean; {inside the viewport?}
	Rx, Ry: real;    {x,y real mouse position in in VPort}
	GMousX, GMousY: integer; {x,y integers; mouse global position}
	PlotVPX, PlotVPY: integer; {x,y starting coords of Plot VP }
	PCVersion, ReturnFlag: boolean;
	XScale, YScale: real;
	blanc, noir: word;
	ValidPlot: boolean; {is there a valid plot on the screeen?}
	RePlotFlag: boolean; {should module do a re-plot?}
	MouseShowing: boolean; {is mouse cursor showing on screen?}
	StatusShowing: boolean; {is status area showing on screen?}
{	HotKShowing: boolean; { hotkeys showing? }
{	TopMShowing: boolean; { top menu showing? }
	Cursor  {left, right edit cursors}	: ARRAY[1..2] OF integer;
	ci      {cursor index =1,2}       	: integer;
	Ac      {index of active cursor}  	: integer;
	MouseButton: integer;
	CInc: ARRAY[1..3] OF integer;
	SStr: ARRAY[1..4] OF STRING[7];
   section : byte;

 {==========================  Utility Routines  =============================}
 {==========================  Utility Routines  =============================}
 {Procedure HandleTopMenu; forward;}

PROCEDURE ChooseVP (VPNr: integer); {track current VP}
BEGIN{ChooseVP}
	CurrentVP := VPNr;
	SelectViewPort(VPNr);
END; {ChooseVP}


PROCEDURE MLine (x1, y1, x2, y2: integer; hue: word);
	VAR
		oldvp: integer;
BEGIN {MLine}
	OldVP := CurrentVP;
	ChooseVP(FullVP);
	SetColor(hue);
 { do line on full screen in screen-rel coordinates }
	line(round(x1 * XScale), round(y1 * YScale), round(x2 * XScale), round(y2 * YScale));
	ChooseVP(OldVP);
END; {MLine}

PROCEDURE MBar (x1, y1, x2, y2: integer; hue: word);
	VAR
		OldVP: integer;
BEGIN {MBar}
 { do Bar on full screen in screen-rel coordinates }
	OldVP := CurrentVP;
	ChooseVP(FullVP);
	SetFillStyle(SolidFill, hue);
	Bar(round(x1 * XScale), round(y1 * YScale), round(x2 * XScale), round(y2 * YScale));
	ChooseVP(OldVP);
END; {MBar}

PROCEDURE MouseOff;
BEGIN {MouseOff}
	IF MouseShowing THEN
	BEGIN
		HideCursor;
		MouseShowing := FALSE;
	END;
END;  {MouseOff}

PROCEDURE MouseOn;
	VAR
		SaveVP: integer;
BEGIN {MouseOn}
	IF (NOT MouseShowing) THEN
	BEGIN
		SaveVP := CurrentVP;
		ChooseVP(FullVP);
		ShowCursor;
		MouseShowing := TRUE;
		ChooseVP(SaveVP);
	END;
END;  {MouseOn}


PROCEDURE WriteGr (XScreen, YScreen: real; msg: Str80; hue: integer);
	VAR
		x, y, OldVP: integer;
 { write to x,y 0..1 in screen-relative coords}
BEGIN {WriteGr}
	OldVP := CurrentVP;
	ChooseVP(FullVP);
	SetTextJustify(LeftText, TopText);
	RuboutXY(XScreen, YScreen, length(msg), noir);
	setcolor(hue);
	PrintXY(XScreen, YScreen, msg);
	ChooseVP(OldVP);
END;{WriteGr}

PROCEDURE LeaveVP;
BEGIN
	MouseOff;
	ClearViewPort;
END; {LeaveVP}

PROCEDURE SetInput (Nr: integer; num, Min, Max: real);
BEGIN {SetInput}
      {Sets both number and number limits for input variable}
	RScreen.SetNumber(Nr, num);
	RScreen.SetNumberLimits(Nr, Min, Max);
END;  {SetInput}

PROCEDURE YTitle (st: str80);
	VAR
		OldVP, x, y: integer;
BEGIN {YTitle}
 { Y-title on full screen in screen-rel coordinates }
	OldVP := CurrentVP;
	ChooseVP(FullVP);
	x := round(XScale * 12);
	y := round(YScale * (230 - 4 * length(st)));
	SetTextStyle(DefaultFont, VertDir, 1);
	SetTextJustify(LeftText, TopText);
	setcolor(blanc);
	Outtextxy(x, y, st);
	SetTextStyle(DefaultFont, HorizDir, 1);
	ChooseVP(OldVP);
END; {YTitle}

PROCEDURE ClearAxisY; {clears off Axis routine y-labels}
BEGIN {ClearAxisY}
 { Because some Axis routine y-labels are re-done }
	ChooseVP(YAxisVP1);
	ClearViewPort;
	ChooseVP(PlotVP);
END; {ClearAxisY}

PROCEDURE ClearAxisX; {clears off Axis routine x-labels}
BEGIN {ClearAxisX}
 { Because some Axis routine x-labels are re-done }
	ChooseVP(XAxisVP1);
	ClearViewPort;
	ChooseVP(PlotVP);
END; {ClearAxisX}


PROCEDURE SetHelpGraphVP;
BEGIN{SetHelpGraphVP}
	DefineViewport(ReplotVP1, 0.0, 0.76, 0.35, 0.95);
	DefineViewport(PlotVP, vpl, 0.75, 0.4, 0.92);
	PlotVPx := round(vpl * GetMaxX);
	PlotVPy := round(0.08 * GetMaxY);
END; {SetHelpGraphVP}

PROCEDURE SetGraphVP;
BEGIN{SetGraphVP}
	DefineViewport(ReplotVP1, 0.0, 0.78, 0.10, 0.95);
	DefineViewport(PlotVP, vpl, 0.75, 0.15, 0.92);
	PlotVPx := round(vpl * GetMaxX);
	PlotVPy := round(0.08 * GetMaxY);
END; {SetGraphVP}


 {===================== INFORMATION ROUTINES ===============================}
 {===================== INFORMATION ROUTINES ===============================}

PROCEDURE OpeningScreen;
	VAR
		i: integer;
BEGIN{OpeningScreen}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[2] := '         Radioactivity Simulation  II ';
	HS[3] := '     Counting, Activation, and Decay Rates';
	HS[6] := ' Mike Moloney, Rose-Hulman Institute of Technology';
	HS[7] := '              Copyright (c) 1994';
	HS[10] := ' This program has a section on nuclear rates,    ';
	HS[11] := ' and a section on nuclear counting.';
	HS[13] := ' The nuclear rates section deals with production';
	HS[14] := ' and decay of ''activated'' nuclides via neutron';
	HS[15] := ' bombardment. It also simulates the decay of one';
	HS[16] := ' species into a second which decays into a third';
	HS[17] := ' species.';
	HS[19] := ' The nuclear counting section simulates counting';
	HS[20] := ' with ''dead time''';
	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
   SetRGBPalette(green, $00, $18, $7);
END;{OpeningScreen}

PROCEDURE CountModuleInfo;
	VAR
		i: integer;
BEGIN{CountModuleInfo}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
        {12345678901234567890123456789012345678901234567890}
	HS[2] := ' This section simulates decays from a source with';
	HS[3] := ' a constant decay rate. Arriving counts are  ';
	HS[4] := ' modeled via the ''interval'' distribution. The   ';
	HS[5] := ' user may select a Geiger-Muller tube (GMT) dead';
	HS[6] := ' time, or set it to zero.';

	HS[8] := ' Both ''Ideal'' and ''GMT'' counts displayed. ';
	HS[9] := ' ''Ideal'' counts are those found by an ''ideal''';
	HS[10] := ' detector, one which has no ''dead time''. ''GMT''';
	HS[11] := ' counts are less than ''Ideal'' counts because ';
	HS[12] := ' some are missed while the GMT is recovering';
	HS[13] := ' from a previous pulse, during its ''dead time''.';

	HS[15] := ' A ''walk-through'' simulation of counting is';
	HS[16] := ' available (and recommended) under ''Section 1''';

	HS[18] := ' A poisson distribution is available to compare';
	HS[19] := ' with the accumulated results. The user may';
	HS[20] := ' vary the poisson parameter E for the best fit.';
	HS[20] := ' One may adjust:    average decay rate ';
	HS[21] := '                    Geiger-Muller tube dead time';
	HS[22] := '                    time for each sample ';
	HS[23] := '                    the total number of samples';
	Help(HS);
   SetRGBPalette(green, $00, $18, $7);
END;{CountModuleInfo}


	PROCEDURE ShowFrontInfo;
		VAR
			i: integer;
	BEGIN{ShowFrontInfo}
		FOR i := 1 TO 25 DO
			HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
		HS[2] := ' Information  -  Nuclear Rates Section     ';
		HS[5] := ' This section simulates two nuclear rate pro-';
		HS[6] := ' cesses. One process is the production and';
		HS[7] := ' decay of ''activated'' nuclides via neutron';
		HS[8] := ' bombardment. Two species, with abundances,';
		HS[9] := ' half-lives, and neutron capture cross-sections';
		HS[10] := ' can be used. With a neutron flux selected,';
		HS[11] := ' the buildup and decay of each species will be';
		HS[12] := ' followed. The flux may be turned off, which';
		HS[13] := ' will lead to the decay of the activated nuc-';
		HS[14] := ' lides as one would see in a neutron howitzer';
		HS[15] := ' experiment.';
		HS[17] := 'The other process is the series decay of one';
		HS[18] := 'species into a second which decays into a third.';
		HS[19] := 'The user supplies the initial amount of the';
		HS[20] := 'first species, and the half-lives of all three.';
		HS[22] := 'A "speedup" factor is available which increases';
		HS[23] := 'the simulation speed, but with some loss in';
		HS[24] := 'fidelity to the true statistical behavior.';
		HS[25] := ' Press <Enter> or click mouse to continue.  ';
		Help(HS);
      SetRGBPalette(green, $00, $18, $7);
	END;{ShowFrontInfo}

PROCEDURE ReminderInfo;
	VAR
		i: integer;
BEGIN{ReminderInfo}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[2] := '         Reminders for CUPS Users ';

	HS[4] := ' Use of keys in CUPS programs. ';

	HS[6] := '  Menu items: ';
	HS[8] := '     <F10> takes you to the ''top menu''. ';
	HS[9] := '     <Enter> selects a menu item. ';
	HS[10] := '     Arrow keys move vertically thru menus. ';
	HS[15] := '  Slider manipulation: ';
	HS[17] := '     <Tab> selects the next slider.';
	HS[18] := '     Arrow keys increase/decrease slider values';
	HS[19] := '     <Home> selects the smallest slider value';
	HS[20] := '     <End>  selects the largest  slider value';

	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
   SetRGBPalette(green, $00, $18, $7);
END;{ReminderInfo}

PROCEDURE SliderUseInfo;
	VAR
		i: integer;
BEGIN{SliderUseInfo}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[2] := '                 Use of Sliders ';
	HS[4] := ' Getting from one slider to the next:';
	HS[6] := '      TAB key gets from one slider to the next.';
	HS[7] := '      The ''active'' slider has a shaded area';
	HS[8] := '          on its button.';
	HS[11] := ' Moving to either end of the slider:';
	HS[13] := '      HOME key gets to low end of slider.';
	HS[14] := '      END  key gets to high end of slider.';
	HS[17] := ' The slider value may be changed by placing the';
	HS[18] := ' mouse over the slider button, pressing the mouse';
	HS[19] := ' button and ''dragging'' the mouse. Holding the';
	HS[20] := ' mouse button down over the triangular symbol at';
	HS[21] := ' either end of the slider also changes the slider';
	HS[22] := ' value and moves the slider button.';
	HS[25] := '   Press <Enter> or click mouse to continue.   ';
	Help(HS);
   SetRGBPalette(green, $00, $18, $7);
END;{SliderUseInfo}


PROCEDURE KeyHelp;
	VAR
		i: integer;
BEGIN{KeyHelp}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[2] := '         Keys Available and their Uses ';
	HS[4] := ' In EDIT mode there is are two cursors, one';
	HS[5] := ' ''active'' and one ''passive''. The active cursor';
	HS[6] := ' points to one data point. This cursor advances';
	HS[7] := ' via the right arrow or up arrow keys, & retreats';
	HS[8] := ' via the left arrow and down arrow keys. The PgUp';
	HS[9] := ' key takes you to the next cursor, swapping the';
	HS[10] := ' roles of active and passive cursors. The step';
	HS[11] := ' size in advancing or retreating is changed via';
	HS[12] := ' the PgDn key. In EDIT mode, one may ''Zoom In''';
	HS[13] := ' on data between the cursors via the <End> key,';
	HS[14] := ' and may ''Zoom Out'' via the <Home> key.';
	HS[16] := ' In LINE mode, there are two cursors which can';
	HS[17] := ' move anywhere on the screen via arrow keys or';
	HS[18] := ' be dragged with the mouse. <PgUp> and <PgDn>';
	HS[19] := ' have the same function in LINE mode that they';
	HS[20] := ' do in EDIT mode. To see the equation of the';
	HS[21] := ' line, press, <End>. ';
	HS[23] := '   To enter LINE mode, press <L> or <l>.';
	HS[24] := '   To enter EDIT mode, press <E> or <e>.';
	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
   SetRGBPalette(green, $00, $18, $7);
END;{KeyHelp}

 {===========================================================================}
 {============== N U C L E A R   R A T E S    M O D U L E ===================}
 {===========================================================================}
PROCEDURE Rates (InfoScreen: Boolean);

	CONST
		MaxNum = 500; {Max points in plot array}
		PlotPtsNr = 100; {Nr of points on the time axis of graph}
		AvNr = 200; {Av Nr created in a time interval DT   }
		CCl = lightred; {color of stuff that changes}
		Y1Cl = lightgreen; {species 1}
		Y2Cl = cyan;       {species 2}
		Y3Cl = lightgray;  {species 3}
		TotCl = white;      {sp1+sp2}
		Act = 1; {activation plot selected}
		Ser = 2; {series plot selected}
		txt = yellow;
		lgy = lightgray;
		heading = white;
		SetParms = 2; {menu column for Set Parameters}
		MaxStatus = 4;
		waiting = 1;
		running = 2;
		editing = 3;
		data = 4;
		norm = 1;
		decay = 2;
		log = 3; {Graph Type}
		New = 1;
		UpDate = 2; {for fit routine info}
		Mode = 1;
		CStatus = 4;
		CKeys = 12;
		OKeys = 17;
		Gr = 24; {lines for status box}
       {New=1;}
		Edit = 2;
		Lin = 3;
		wait = 4;
		run = 5;
		min = 1;
		max = 2; {for minmaxvalue function}
   TYPE
      VectType = ARRAY[1..MaxNum] OF real;
	VAR
		key: BYTE;
		TopMenu: TMenu;
		HotKeys: THotKeys;
		SSTr: ARRAY[1..4] OF STRING[10];
		ScaleX, ScaleY: real;
		XLow, YLow: integer; {lower corner screen plot coords}
		XHigh, YHigh: integer; {upper corner   " }
		xlo, ylo, xhi, yhi: real;    {min,max values of plot variables}
		Cs, Frac: ARRAY[1..2] OF real;
		HLife, SLife, lambda: ARRAY[1..3] OF real;
		PRate, DRate {production,decay}		: ARRAY[1..2] OF real;
		Nr: ARRAY[1..3] OF longint;
		NrOne: real;
		GrTime1, GrTime2: real;
		GraphTime, BarnRate: real;
		DT {basic time interval}  : real;
		XArr, Y1Arr, Y2Arr, Y3Arr, TotArr: VectType;
		ItNr, slice, ff, ff1, ff2 {speedup factors} 	: integer;
		VMin, VMax, HMin, HMax: real;
		TSlice: real;
		option: integer;
		ReturnFlag, FluxOn: boolean;
		Fresh1, Fresh2: boolean;
		Graphing, paused: boolean;
		ch: char;
		ModeStatus: integer;
		GraphType {normal, decay, log(decay) }		: integer;
		LastCall {to status routine}          		: integer;
		OmitY  {what to plot on y-axis}       		: integer;
		CursCl {color for y=cursor values}    		: integer;
		YCurNr {number of y-cursor species }  		: integer;

      Procedure SetEditHotKeys; forward;
      Procedure SetLineHotKeys; forward;
      Procedure SetRunHotKeys; forward;

 {================  S T A R T   O F   P R O C E D U R E S ================}
 {================  S T A R T   O F   P R O C E D U R E S ================}
 {================  S T A R T   O F   P R O C E D U R E S ================}

	PROCEDURE ClearScreen;
	BEGIN{ClearScreen}
		ChooseVP(FullVP);
		LeaveVP;
		StatusShowing := FALSE;
	END; {ClearScreen}



 {======================  H   E   L   P  ==================================}


	PROCEDURE ShowHelp1;
		VAR
			i: integer;
	BEGIN{ShowHelp1}
		FOR i := 1 TO 25 DO
			HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
		HS[5] := 'Initially one does either a neutron activation';
		HS[6] := 'run, or a decay-series run. At the conclusion of';
		HS[7] := 'a run, one enters Edit mode. From here one may';
		HS[8] := 'focus on data of interest, and line-fit the data.';

		HS[11] := 'When Edit or Line mode is active, the ''Pick''';
		HS[12] := 'Plot'' top menu item has become active. This lets';
		HS[13] := 'one choose population,  decay rate or log of decay';
		HS[14] := 'rate vs time to be plotted.';

		HS[17] := '  If the simulation runs too slowly, go to the';
		HS[18] := 'parameters menu and enlarge the ''speedup'' factor.';
		HS[19] := '  If the simulation runs rapidly, it is good';
		HS[20] := 'to reduce the speedup factor and get more fidelity';
		HS[21] := 'to the true dynamics of activation and decay.';
		HS[25] := ' Press <Enter> or click mouse to continue.  ';
		Help(HS);
      SetRGBPalette(green, $00, $18, $7);
	END;{ShowHelp1}

	PROCEDURE ShowHelp2;
		VAR
			i: integer;
	BEGIN{ShowHelp2}
		FOR i := 1 TO 25 DO
			HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
		HS[3] := 'In ''Edit'' mode for any plot, the vertical cursor';
		HS[4] := 'represents one time, but several different y      ';
		HS[5] := 'values (e. g. species 1, species 2, and total).   ';

		HS[7] := 'In the upper righthand corner of the screen is    ';
		HS[8] := 'displayed the cursor information. Only one of     ';
		HS[9] := 'the cursor y-values is shown, in the same color   ';
		HS[10] := 'as on the graph.';

		HS[12] := 'The <F5> key (''Y-Values'') changes which of the   ';
		HS[13] := 'y-values is displayed. The color changes each time';
		HS[14] := 'to reflect the color of the item on the graph.    ';

		HS[25] := ' Press <Enter> or click mouse to continue.  ';
		Help(HS);
      SetRGBPalette(green, $00, $18, $7);
	END;{ShowHelp2}


 {=====================  S  T  A  T  U  S    ==============================}
 {=====================  S  T  A  T  U  S    ==============================}

	PROCEDURE ShowFluxStatus;
	BEGIN{ShowFluxStatus}
		IF FluxOn THEN
			WriteGr(StatX[1], StatY[31], 'Flux:  ON', txt)
		ELSE
			WriteGr(StatX[1], StatY[31], 'Flux: OFF', txt);
	END; {ShowFluxStatus}

	PROCEDURE ShowCalcStatus;
	BEGIN{ShowCalcStatus}
		WriteGr(StatX[4], StatY[33], NumStr(slice, 3, 0), txt);
	END; {ShowCalcStatus}

	PROCEDURE SetCalcStatus;
	BEGIN{SetCalcStatus}
		WriteGr(StatX[1], StatY[33], 'Step', lgy);
		ShowCalcStatus;
		WriteGr(StatX[7], StatY[33], Concat('of ', NumStr(PlotPtsNr, 3, 0)), lgy);
	END; {SetCalcStatus}


	PROCEDURE ShowStatus (Call: integer);
		VAR
			NewCall: boolean;       {ShowStatus}
		VAR
			KStr: STRING[6];
			i: integer;
			ACur: ARRAY[1..2] OF STRING[6];
			CurInc: ARRAY[1..3] OF STRING[3];

		PROCEDURE SetConstants;
		BEGIN{SetConstants}
			ACur[1] := 'Left ';
			ACur[2] := 'Right';
			CurInc[1] := '  1';
			CurInc[2] := ' 10';
			CurInc[3] := '100';
		END; {SetConstants}

		PROCEDURE Generics; {Common Status box stuff}
			CONST
				CK = 3;
				CS = 15;
				Gr = 19;
		BEGIN{Generics}
			StatusShowing := TRUE;
			ClearViewPort;
			WriteGr(StatX[1], StatY[Ck+1], 'Present Mode is', Heading);
		   WriteGr(StatX[1], StatY[Ck+4], 'Y-cursor value: ', txt);
		   WriteGr(StatX[1], StatY[Ck+5], ' Total     ', totCl);
		   WriteGr(StatX[1], StatY[Ck+6], ' Species 1 ', y1cl);
		   WriteGr(StatX[1], StatY[Ck+7], ' Species 2 ', y2cl);
         IF (option = ser)	then
         WriteGr(StatX[1], StatY[Ck+8], ' Species 3 ', y3cl);
		   WriteGr(StatX[1], StatY[Ck+9], 'X-cursor value: ', txt);

			WriteGr(StatX[1], StatY[CS], 'Cursor Status', Heading);
			WriteGr(StatX[2], StatY[CS + 1], '   Active', lgy);
			WriteGr(StatX[2], StatY[CS + 2], 'Step Size', lgy);
			WriteGr(StatX[8], StatY[CS + 1], ACur[Ac], txt);
			WriteGr(StatX[8], StatY[CS + 2], CurInc[ci], txt);

			WriteGr(StatX[1], StatY[Gr], 'Graph Information', Heading);
			IF option = ser THEN
				WriteGr(StatX[1], StatY[Gr + 1], '  Series of Three', txt)
			ELSE
				WriteGr(StatX[1], StatY[Gr + 1], '  Activation', txt);
		END;{Generics}

	BEGIN{ShowStatus}
		SetConstants;
		ChooseVP(StatusVP);
		NewCall := FALSE;
		IF (NOT StatusShowing) THEN Generics;
		IF Call <> LastCall THEN
		BEGIN
			LastCall := Call;
			NewCall := TRUE;
		END;
		IF NewCall THEN
			CASE Call OF
				Edit:	WriteGr(StatX[1], StatY[5], '       E D I T ', txt);
				Lin: 	WriteGr(StatX[1], StatY[5], '       L I N E ', txt);
				run:  WriteGr(StatX[1], StatY[5], ' R U N N I N G ', txt);
				wait: WriteGr(StatX[1], StatY[5], ' W A I T I N G ', txt);
			END;{Case}
		ShowFluxStatus;
    {update cursor status}
		IF (Call = lin) OR (Call = Edit) THEN
		BEGIN
			WriteGr(StatX[8], StatY[16], ACur[Ac], txt);
			WriteGr(StatX[8], StatY[17], CurInc[ci], txt);
		END;
		ChooseVP(MainVP1);
	END;{ShowStatus}

 {======================== E   D   I   T  =================================}
	PROCEDURE XORLine (x1, y1, x2, y2: integer);
	BEGIN{XORLine}
		SetWriteMode(XORPut);
		Line(x1, y1, x2, y2);
		SetWriteMode(CopyPut);
	END; {XORLine}

	PROCEDURE ColorPlot;
	forward;

	PROCEDURE PlotCursor (Nr, hue: integer); {Cursor Nr,color}
		VAR
			i, i1, j1, SaveClr: integer; {cursor: XORed vertical line}
	BEGIN{PlotCursor}
		ChooseVP(PlotVP); {plot area only}
		SetColor(hue);
		i := Cursor[Nr];  {get cursor's spot on the plot list}
		Map(XArr[i], Y1Arr[i], i1, j1);
		XORLine(i1, 0, i1, GetMaxY);
	END;{PlotCursor}

	FUNCTION PlotValue (Index, typ: integer): real;
	forward;
  { returns a value to be plotted }
  { type tells if 1st..3rd, or total }

	PROCEDURE ShowCursorValues;
		VAR
			v: STRING[18];
			x1,x2,x3,x4: real;
			m,ck: integer;
	BEGIN{ShowCursorValues}
      m := Cursor[ac];
		x1 := PlotValue(m, 1);
		x2 := PlotValue(m, 2);
      IF (option = ser)	then x3 := PlotValue(m, 3);
		x4 := PlotValue(m, 4);
      ck := 3;
		WriteGr(StatX[1], StatY[ck+5], ' Total     '+NumStr(x4, 6, 3), totCl);
		WriteGr(StatX[1], StatY[ck+6], ' Species 1 '+NumStr(x1, 6, 3), y1cl);
		WriteGr(StatX[1], StatY[ck+7], ' Species 2 '+NumStr(x2, 6, 3), y2cl);
      IF (option = ser)	then
      WriteGr(StatX[1], StatY[ck+8], ' Species 3 '+NumStr(x3, 6, 3), y3cl);
		WriteGr(StatX[1], StatY[ck+10],'           '+NumStr(XArr[m], 6, 3), white);
	END; {ShowCursorValues}

	PROCEDURE InitEdit;
	BEGIN{InitEdit}
      SetEditHotKeys;
		ci := 2;
		MSel := 'E';
		Cursor[1] := lf;
		Cursor[2] := rt;
		IF rt - lf > 4 THEN
		BEGIN
			Cursor[1] := lf + 2;
			Cursor[2] := rt - 2;
		END;
		PlotCursor(1, ACl);     {left is active cursor}
		PlotCursor(2, PCl);
		Ac := 1;
		ShowCursorValues;
		ChooseVP(MainVP1);
	END; {InitEdit}

	PROCEDURE EditAction; { Ac indicates 'active cursor' }
            {EditAction}

		PROCEDURE CursorForward;
		BEGIN {CursorForward}
			PlotCursor(Ac, ACl);{erase old}
			Cursor[Ac] := Cursor[Ac] + CInc[ci];
			IF Cursor[1] >= Cursor[2] THEN
				Cursor[1] := Cursor[2] - 1;
			IF Cursor[2] > rt THEN
				Cursor[2] := rt;
			PlotCursor(Ac, ACl);{write new}
		END; {CursorForward}

		PROCEDURE CursorBack;
		BEGIN {CursorBack}
			PlotCursor(Ac, ACl);{erase old}
			Cursor[Ac] := Cursor[Ac] - CInc[ci];
			IF Cursor[2] <= Cursor[1] THEN
				Cursor[2] := Cursor[1] + 1;
			IF Cursor[1] < lf THEN
				Cursor[Ac] := lf;
			PlotCursor(Ac, ACl);{write new}
		END; {CursorBack}

   { Ac = active cursor indicator }
	BEGIN {EditAction}
		CASE event.readkey OF
			ArrowR, ArrowUp:
				CursorForward;
			ArrowL, ArrowDn:
				CursorBack;
			PgUp: { new active cursor }
			BEGIN
				PlotCursor(Ac, ACl); {erase}
				PlotCursor(Ac, PCl);{ new color}
				IF Ac = 1 THEN Ac := 2
				ELSE Ac := 1;
				PlotCursor(Ac, PCl); {erase}
				PlotCursor(Ac, ACl); {new color}
			END;
			PgDn: {dec. cursor increment }
			BEGIN
				dec(ci);
				IF ci = 0 THEN	ci := 3;
			END;
			HomeKey: { zoom out to original limits }
			BEGIN
				lf := 1;
				rt := OriginalRight;
				Cursor[1] := lf;
				Cursor[2] := rt;
				ColorPlot;
			END;
			EndKey: { zoom in to present cursors  }
			BEGIN
				lf := Cursor[1];
				rt := Cursor[2];
				ColorPlot;
			END;
		END;{CASE}
		ShowCursorValues;
	END;{EditAction}

	PROCEDURE ExitEdit;
		VAR
			i: integer;
	BEGIN{ExitEdit}
		PlotCursor(Ac, ACl);
		PlotCursor(3 - Ac, PCl);
		SelectViewPort(StatusVP);
		FOR i := 3 TO 6 DO
			WriteGr(CStatus + i, 2, '                 ', lgy);
		SelectViewPort(MainVP1);
	END; {ExitEdit}

 {================= F I T   R O U T I N E   S T U F F ========================}

	PROCEDURE CrossedCursorTest (cur: integer);
		VAR
			ch: char;
	BEGIN{CrossedCursorTest}
		IF (Cr[1].x >= Cr[2].x) THEN
		BEGIN
			CASE cur OF
				1:
					Cr[1].x := Cr[2].x - 1;
				2:
					IF Cr[2].x >= Cr[3].x THEN
						Cr[2].x := Cr[3].x - 1
					ELSE
						Cr[2].x := Cr[1].x + 1;
				3:
					Cr[3].x := Cr[2].x + 1
			END;{case}
		END;
	END;{CrossedCursorTest}

	PROCEDURE FitCursor (Nr, hue: integer); {Plot Fit Cursor; solid 9x9 }
		CONST
			d = 4; {2d+1 x 2d+1 pixels}
		VAR
			i, i1, j1, s: integer;
	BEGIN{FitCursor}
		s := GetColor;
		SetColor(hue + 128);
		i1 := Cr[Nr].x;
		j1 := Cr[Nr].y;
		SetLineStyle(SolidLn, 0, NormWidth);
		FOR i := i1 - d TO i1 + d DO
			XORLine(i, j1 - d, i, j1 + d);
		SetColor(s);   {restore saved color}
	END;{FitCursor}

	PROCEDURE CursorUp;
	BEGIN {CursorUp}
		FitCursor(Ac, ACl);{erase old}
		Cr[Ac].y := Cr[Ac].y - CInc[ci];
		IF Cr[Ac].y > YHigh THEN
			Cr[Ac].y := YHigh;
		IF Cr[Ac].y < YLow THEN
			Cr[Ac].y := YLow;
		FitCursor(Ac, ACl);{write new}
	END; {CursorUp}

	PROCEDURE CursorDn;
	BEGIN {CursorDn}
		CInc[ci] := -CInc[ci];
		CursorUp;
		Cinc[ci] := -CInc[ci];
	END;  {CursorDn}

	PROCEDURE CursorR;
	BEGIN {CursorR}
		FitCursor(Ac, ACl);{erase old}
		Cr[Ac].x := Cr[Ac].x + CInc[ci];
		IF (Cr[1].x >= Cr[2].x) OR (Cr[2].x > Cr[3].x) THEN
			Cr[Ac].x := Cr[Ac].x - CInc[ci]; {undo the move}
		IF Cr[Ac].x > XHigh THEN
			Cr[Ac].x := XHigh;
		IF Cr[Ac].x < XLow THEN
			Cr[Ac].x := XLow;
		FitCursor(Ac, ACl);{write new}
	END; {CursorR}

	PROCEDURE CursorL;
	BEGIN {CursorL}
		CInc[ci] := -CInc[ci];
		CursorR;
		CInc[ci] := -CInc[ci];
	END;  {CursorL}

	PROCEDURE OptStr (m: real; VAR v: STRING);
		VAR
			x: real;
	BEGIN {OptStr}
		x := abs(m);
		IF x >= 0.1 THEN
			v := NumStr(m, 8, 3);
		IF (x < 0.1) AND (x >= 0.001) THEN
			v := NumStr(m, 8, 5);
		IF x < 0.001 THEN
			v := NumStr(m, 10, 7);
	END; {OptStr}


	PROCEDURE DragCursor (NrCursors: integer);              {DragCursor}
		CONST
			dist = 30;
		VAR
			i, bx, by: integer;
		PROCEDURE PopCursor (j: integer);
		BEGIN{PopCursor}
			IF j = Ac THEN
				FitCursor(j, ACl)
			ELSE
				FitCursor(j, PCl);
		END; {PopCursor}

	BEGIN{DragCursor}
		bx := GMousX;
		by := GMousY;
		bx := bx - PlotVPX;
		by := by - PlotVPY;
		FOR i := 1 TO NrCursors DO {mouse near cursor?}
			IF  (abs(bx - Cr[i].x) <= dist)
         AND (abs(by - Cr[i].y) <= dist) AND (MouseButton <> 0) THEN {enter dragging mode}
				REPEAT           {while button down}
					MouseGlobalPosn(GMousX, GMousY, MouseButton);
					bx := GMousX;
					by := GMousY;
					bx := bx - PlotVPX;
					by := by - PlotVPY;
					MouseOff;
					PopCursor(i); {erase old}
					Cr[i].x := bx;
					Cr[i].y := by;
           { keep cursors from crossing }
					CrossedCursorTest(i);
					PopCursor(i);   {draw new}
					MouseOn;
					Delay(25);
				UNTIL MouseButton = 0; {button up}
	END; {DragCursor}

 {============================== L I N E ====================================}

	PROCEDURE InitLine;
	BEGIN{InitLine}
      SetLineHotKeys;
		ShowStatus(Lin);
		ChooseVP(PlotVP);
		ci := 2;
		Ac := 1;
		Cr[1].x := XLow + 10;
		Cr[1].y := YHigh - 10;
		Cr[2].x := XHigh - 10;
		Cr[2].y := YLow + 10;
		Cr[3].x := XHigh + 1;
		FitCursor(1, ACl);
		FitCursor(2, PCl);
		SetColor(KeyCl);
		MouseOff;
		XORLine(Cr[1].x, Cr[1].y, Cr[2].x, Cr[2].y);    {Fresh Line}
		MouseOn;
	END; {InitLine}

	PROCEDURE LineEquation; {calc eqn & put on screen}
		VAR
			px1, py1, px2, py2, m, b: extended;
			ms, bs, eqs: STRING;
			lhs: ARRAY[1..3] OF STRING[10];
	BEGIN{LineEquation}
		lhs[1] := '# of nuc ';
		lhs[2] := 'decay\s  ';
		lhs[3] := 'ln(dec/s)';
		px1 := (Cr[1].x - XLow) * ScaleX + HMin;
		py1 := (YHigh - Cr[1].y) * ScaleY + VMin;
		px2 := (Cr[2].x - XLow) * ScaleX + HMin;
		py2 := (YHigh - Cr[2].y) * ScaleY + VMin;
		IF px2 - px1 = 0 THEN
		BEGIN
			OptStr(px1, ms);
			eqs := Concat('  ', lhs[GraphType], ' = ', ms);
		END
		ELSE
		BEGIN
			m := (py2 - py1) / (px2 - px1);
			OptStr(m, ms);
			b := (py1 * px2 - py2 * px1) / (px2 - px1);
			OptStr(b, bs);
			eqs := Concat(lhs[GraphType], ' = ( ', ms, ')t + (', bs, ')');
		END;
		announce('Line Equation: '+eqs);
	END;{LineEquation}

	PROCEDURE LineAction;  { Ac = active cursor indicator }
	BEGIN{LineAction}
		ChooseVP(PlotVP);
		SetColor(KeyCl);
		MouseOff; {cursor gone when erasing/writing line}
		XORLine(Cr[1].x, Cr[1].y, Cr[2].x, Cr[2].y);  {erase the line}
		MouseOn;
		delay(25); {give click time to come up}
		IF MouseButton <> 0 THEN {drag cursor}
		BEGIN
			DragCursor(2);
{			Event.Readkey := INS;}
		END;
		ch := event.ReadKey;
		CASE ch OF
			ArrowR:	CursorR;
			ArrowUp:	CursorUp;
			ArrowL: 	CursorL;
			ArrowDn:	CursorDn;
			PgUp: { new active cursor }
			BEGIN
				FitCursor(Ac, ACl); {erase}
				FitCursor(Ac, PCl);{ new color}
				inc(Ac);
				IF Ac > 2 THEN Ac := 1;
				FitCursor(Ac, PCl); {erase}
				FitCursor(Ac, ACl); {new color}
			END;
			PgDn: {dec. cursor increment }
			BEGIN
				dec(ci);
				IF ci = 0 THEN
					ci := 3;
			END;
			EndKey: LineEquation;
		END;{CASE}
		MouseOff;          {don't show cursor when line is redrawn}
		SetColor(KeyCl);
		XORLine(Cr[1].x, Cr[1].y, Cr[2].x, Cr[2].y);
		MouseOn;
		ShowStatus(lin);
		ChooseVP(MainVP1);
	END; {LineAction}

	PROCEDURE ExitLine; {erase the evidence}
		VAR
			i: integer;
	BEGIN{ExitLine}
		ChooseVP(PlotVP);
		FitCursor(Ac, ACl);
		FitCursor(3 - Ac, PCl);
		SetColor(KeyCl);
		XORLine(Cr[1].x, Cr[1].y, Cr[2].x, Cr[2].y);
		SetLineStyle(SolidLn, 0, NormWidth);
		SelectViewPort(StatusVP);
		FOR i := 3 TO 6 DO
			WriteGr(CStatus + i, 2, '                 ', lgy);
		ChooseVP(MainVP1);
	END; {ExitLine}


 {========================================================================}

	PROCEDURE Screen1Data; {VAR RScreen : TInputScreen;}
		PROCEDURE SetInput (Nr: integer; num, Min, Max: real);
		BEGIN
			RScreen.SetNumber(Nr, num);
			RScreen.SetNumberLimits(Nr, Min, Max);
		END;
	BEGIN {Screen1Data}
		WITH RScreen DO
		BEGIN
			init;
			DefineInputPort(0.1, 0.9, 0.15, 0.90);
			LoadLine('  Numeric parameter input:   Nuclear Rates Section ');
			LoadLine('');
			LoadLine(' 1st species half-life (1E-6 to 1E10)  = {       }sec ');
			LoadLine(' 2nd species half-life (1E-6 to 1E10)  = {       }sec ');
			LoadLine('');
			LoadLine(' 1st species fractional abundance      = {       } (0 to 1)');
			LoadLine(' 2nd species fractional abundance      = {       } (0 to 1)');
			LoadLine('');
			LoadLine(' 1st species capture c.s (1E-4 to 1E4) = {       }barns');
			LoadLine(' 2nd species capture c.s (1E-4 to 1E4) = {       }barns');
			LoadLine('');
			LoadLine(' Production Rate  (0.01 to 1E4)        = {       }/barn-sec ');
			LoadLine(' Total time on the graph (1 to 1E5)    = {       }sec ');
			LoadLine(' Speed-up factor F for decay rates     = {       } (1 to 1000)');
			LoadLine('   (nuclei produced/decay F at a time) ');
			LoadLine('');
			LoadLine('                    [  Ok  ]     [Cancel] ');
			SetInput(1, HLife[1], 1E-6, 1E10);
			SetInput(3, Frac[1], 0.0, 1.0);
			SetInput(2, HLife[2], 1E-6, 1E10);
			SetInput(4, Frac[2], 0.0, 1.0);
			SetInput(5, Cs[1], 1E-4, 1E4);
			SetInput(7, BarnRate, 1E-4, 1E4);
			SetInput(6, Cs[2], 1E-4, 1E4);
			SetInput(8, GrTime1, 1.0, 1E5);
			SetInput(9, ff1, 1.0, 1000);
			AcceptScreen;
			HLife[1] := GetNumber(1);
			Frac[1] := GetNumber(3);
			HLife[2] := GetNumber(2);
			Frac[2] := GetNumber(4);
			Cs[1] := GetNumber(5);
			BarnRate := GetNumber(7);
			Cs[2] := GetNumber(6);
			GrTime1 := GetNumber(8);
			ff1 := round(GetNumber(9));
			done;
		END;{with}
	END;{Screen1Data}

	PROCEDURE NewXYGraph (xlo, xhi, ylo, yhi: REAL);
	forward;

	PROCEDURE Screen2Data; {VAR RScreen : TInputScreen;}
	BEGIN {Screen2Data}
		WITH RScreen DO
		BEGIN
			init;
			DefineInputPort(0.1, 0.9, 0.15, 0.85);
			LoadLine('  Numeric parameter input:   Nuclear Rates Section ');
			LoadLine('');
			LoadLine(' Species 1 -> Species 2 -> -> Species 3. ');
			LoadLine('     Only Species 1 present at t=0.');
			LoadLine('');
			LoadLine('      For Three-Nucide Decay Chain :   ');
			LoadLine(' Species 1 half-life  (1E-6 to 1E10)  = {       }sec');
			LoadLine(' Species 2 half-life  (1E-6 to 1E10)  = {       }sec');
			LoadLine(' Species 3 half-life  (1E-6 to 1E10)  = {       }sec');
			LoadLine('');
			LoadLine(' Species 1 initial nr (0.01 to 20)    = {       }millions');
			LoadLine(' Total time on the graph (0.1 to 1E5) = {       }sec');
			LoadLine(' Speed-up factor F for decay rates    = {       } (1 to 1000)');
			LoadLine('   (nuclei produced/decay F at a time) ');
			LoadLine('');
			LoadLine('                   [  Ok  ]     [Cancel] ');
			SetInput(1, SLife[1], 1E-6, 1E10);
			SetInput(4, NrOne, 0.01, 20);
			SetInput(2, SLife[2], 1E-6, 1E10);
			SetInput(5, GrTime2, 0.1, 1E5);
			SetInput(3, SLife[3], 1E-6, 1E10);
			SetInput(6, ff2, 1, 1000);
			AcceptScreen;
			SLife[1] := GetNumber(1);
			NrOne := GetNumber(4);
			SLife[2] := GetNumber(2);
			GrTime2 := GetNumber(5);
			SLife[3] := GetNumber(3);
			ff2 := round(GetNumber(6));
			done;
		END;{with}
	END;{Screen2Data}

	PROCEDURE GraphLimits;
		VAR
			root: real;
			i: integer;
	BEGIN{GraphLimits}
{   set graph limits; also initialize populations.}
{   Max population at top; lam1*n1=Pr1; n1max=Pr1/lam1; n2max=pr2/lam2}
{   total time for x-axis}
{   only rescale when gross parameter change, not always}
		IF option = act THEN  {activation}
		BEGIN
			FluxOn := True;
			PRate[1] := Cs[1] * Frac[1] * BarnRate;
			PRate[2] := Cs[2] * Frac[2] * BarnRate;
			DT := AvNr / PRate[1];     {time to produce av nr of species 1}
			lambda[1] := ln(2) / Hlife[1];   {decay constants}
			lambda[2] := ln(2) / HLife[2];
			VMax := 1.05E-6 * ((PRate[1] / lambda[1]) + (PRate[2] / lambda[2]));
			Nr[1] := 0;
			GraphTime := GrTime1;
			ff := ff1;
		END
		ELSE  {series of 3 decays}
		BEGIN
			VMax := 1.05 * NrOne;
			Nr[1] := round(1.0E6 * NrOne);
			GraphTime := GrTime2;
			ff := ff2;
			FOR i := 1 TO 3 DO
				lambda[i] := ln(2) / SLife[i];
		END;
		Vmin := 0.0;
		Hmin := 0.0;
		HMax := GraphTime;
		slice := 0;
		TSlice := GraphTime / PlotPtsNr; { time interval per slice on plot }
 {  ItNr   := round(TSlice/DT);}
 {iterations to do inside the calc routine}
		Nr[2] := 0;
		Nr[3] := 0;
		GraphType := norm;
		lf := 1; {original left end of data array}
		rt := 0; {bogus right end of data array}
	END;{GraphLimits}

   PROCEDURE PlotPt (x, y, hue: integer);
		VAR
			m: integer;
		CONST
			d = 1;
	BEGIN{PlotPt}
		SetWriteMode(CopyPut);
		setcolor(hue);
		FOR m := x - d TO x + d DO
			line(m, y - d, m, y + d);
	END; {PlotPt}


   procedure PDLine( var XArr, yArr : vectType;
                     k : integer; color : integer);
   var x1,y1,x2,y2,m: integer;
   begin
      SetColor(color);
		Map(XArr[k], YArr[k], x1, y1);
		FOR m := x1-1 TO x1+1 DO line(m, y1-1, m, y1+1);
      if k>1 then begin
   		Map(XArr[k-1], YArr[k-1], x2, y2);
         line(x2, y2, x1, y1);
      end;
   end;


	PROCEDURE PDSlice (k: integer); {does one time 'slice' of P/D:      PDSlice }
		CONST
			d = 1; {point plots as 2d+1 x 2d+1 pixels}
                { production and decay of nuclei }
			LoopNr = 50; {number of subloops in this time slice}
		VAR
			time, tiny, t, tot, tpav1, tpav2, tdav1, tdav2, tpav3, tdav3: real;
			p3, d3, p1, d1, p2, d2: longint;
			i, j, m, ck, x1,y1,x2,y2,x3,y3,xt,yt: integer;
	BEGIN{PDSlice}
 {IF k=1 THEN InitMouse(20,50);}
		ShowStatus(Run);
		IF k = 1 THEN
			SetCalcStatus;
		ShowCalcStatus;
 { set up average decay, production times }
 { ff = speedup factor }
		IF ff < 1 THEN
			ff := 1;
		IF ff > 1000 THEN
			ff := 1000;
		time := TSlice / ff; {total time for this slice of computation}
		tiny := time / LoopNr;
		FOR m := 1 TO LoopNr DO
		BEGIN
			tpav1 := 0.0;
			tpav2 := 0.0;
			tpav3 := 0.0;
			tdav1 := 0.0;
			tdav2 := 0.0;
			tdav3 := 0.0;
    {reset decay rates}
			IF (Nr[1] > 0) AND (lambda[1] > 0) THEN
				tdav1 := 1.0 / (Nr[1] * lambda[1]);
			IF (Nr[2] > 0) AND (lambda[2] > 0) THEN
				tdav2 := 1.0 / (Nr[2] * lambda[2]);
			IF option = ser THEN
				IF (Nr[3] > 0) AND (lambda[3] > 0) THEN
					tdav3 := 1.0 / (Nr[3] * lambda[3]);
    {reset production rates}
			IF (FluxOn) AND (option = act) THEN
			BEGIN
				tpav1 := 1.0 / PRate[1];
				tpav2 := 1.0 / PRate[2];
			END;
    {init nrs produced in this subslice}
			p1 := 0;
			p2 := 0;
			p3 := 0;
			d1 := 0;
			d2 := 0;
			d3 := 0;
   {decay}
			t := 0;
			IF Nr[1] > 0 THEN
				REPEAT {reduce species 1 }
					t := t - tdav1 * ln(1 - random);
					inc(d1);
				UNTIL t > tiny;
			dec(d1);
			t := 0;
			IF Nr[2] > 0 THEN
				REPEAT {reduce species 2 }
					t := t - tdav2 * ln(1 - random);
					inc(d2);
				UNTIL t > tiny;
			dec(d2);
			t := 0;
			IF (Nr[3] > 0) AND (tdav3 > 0) THEN
			BEGIN
				REPEAT
					t := t - tdav3 * ln(1 - random);
					inc(d3);
				UNTIL t > tiny;
				dec(d3);
			END;
 {produce}
			t := 0;
			IF option = act THEN
			BEGIN
				IF FluxOn THEN
					REPEAT
						t := t - tpav1 * ln(1 - random);
						inc(p1);
					UNTIL t > tiny;
				dec(p1);
				t := 0;
				IF FluxOn THEN
					REPEAT {make species 2 }
						t := t - tpav2 * ln(1 - random);
						inc(p2);
					UNTIL t > tiny;
				dec(p1);
			END
			ELSE { series of 3}
			BEGIN
				p3 := d2;
				p2 := d1;
			END;
   {update the numbers}
			Nr[1] := Nr[1] + (p1 - d1) * ff;
			Nr[2] := Nr[2] + (p2 - d2) * ff;
			Nr[3] := Nr[3] + (p3 - d3) * ff;
    {don't disappear completely}
			FOR i := 1 TO 2 DO
				IF Nr[i] < 1 THEN
					Nr[i] := 1;
			IF option = ser THEN
				IF Nr[3] < 1 THEN
					Nr[3] := 1;
		END;{loop}
		tot := 1.0E-6 * (Nr[1] + Nr[2] + Nr[3]);
   {fill the arrays}
		XArr[k] := k * TSlice;
		Y1Arr[k] := 1.0E-6 * Nr[1];
		Y2Arr[k] := 1.0E-6 * Nr[2];
		Y3Arr[k] := 1.0E-6 * Nr[3];
		TotArr[k] := tot;
		rt := k;
		OriginalRight := rt;
		SelectScale(PlotVP);
		ChooseVP(PlotVP);
   {plot 3 or 4 points}
      ck := 3;
		WriteGr(StatX[8], StatY[ck+5], NumStr(TotArr[k],6, 3), totCl);
		WriteGr(StatX[8], StatY[ck+6], NumStr(Y1Arr[k], 6, 3), y1cl);
		WriteGr(StatX[8], StatY[ck+7], NumStr(Y2Arr[k], 6, 3), y2cl);
      IF option = ser THEN
		WriteGr(StatX[8], StatY[ck+8], NumStr(Y3Arr[k], 6, 3), y3cl);
      WriteGr(StatX[8], StatY[ck+10],NumStr(XArr[k],  6, 3), white);

      PDLine(XArr,y1Arr,k,y1cl);
      PDLine(XArr,y2Arr,k,y2cl);
		IF option = ser THEN PDLine(XArr,y3Arr,k,y3cl);
      PDLine(XArr,totArr,k,totcl);

{		Map(XArr[k], Y1Arr[k], i, j);
		PlotPt(i, j, Y1Cl);
		Map(XArr[k], Y2Arr[k], i, j);
		PlotPt(i, j, Y2Cl);
		Map(XArr[k], tot, i, j);
		PlotPt(i, j, TotCl);
		IF option = ser THEN
		BEGIN
			Map(XArr[k], Y3Arr[k], i, j);
			PlotPt(i, j, Y3Cl);
		END;}

	END;{PDSlice}

	FUNCTION MinMaxValue (i, which: integer): real;
{ returns a value from which max or min can be gotten}
{ OmitY       series            activation}
{  1      omit species 1      omit species 1}
{  2      omit species 2      omit species 2}
{  3      omit species 3      omit species 3 (N/A)}
{  4      omit total          omit total}
		VAR
			s, x, y, z: real;
	BEGIN{MinMaxValue}
    {for population values}
		x := Y1Arr[i];
		y := Y2Arr[i];
		IF option = ser THEN z := Y3Arr[i] else z := 0;
    {for decay rate or log of decay rate}
		IF GraphType <> norm THEN
		BEGIN
			x := x * lambda[1];
			y := y * lambda[2];
			z := z * lambda[3];
		END;
    {assign s to one of the first two}
		IF (OmitY <> 1) THEN s := x ELSE s := y;
    {then test the rest}
		IF which = min THEN      {get the smallest}
		BEGIN
			IF (OmitY <> 2) AND (s > y) THEN s := y;
			IF (OmitY <> 3) AND (option = ser) AND (s > z) THEN s := z;
		END
		ELSE                   {get the largest}
		BEGIN
			IF (OmitY <> 2) AND (s < y) THEN s := y;
			IF (OmitY <> 3) AND (s < z) THEN s := z;
			IF (OmitY <> 4) THEN
			BEGIN
				s := 0;
				IF (OmitY <> 1) THEN s := s + x;
				IF (OmitY <> 2) THEN s := s + y;
				IF (OmitY <> 3) THEN s := s + z;
			END;
		END;
		IF GraphType <> norm THEN s := s * 1.0E6;
		MinMaxValue := s;
	END; {MinMaxValue}

	FUNCTION PlotValue (Index, typ: integer): real;
  { returns a value to be plotted }
  { type tells if 1st..3rd, or total }
		VAR
			i, j: integer;
			x: real;
	BEGIN{PlotValue}
      {needed for all types}
		CASE typ OF
			1: x := Y1Arr[Index];
			2: x := Y2Arr[Index];
			3: x := Y3Arr[Index];
			4: x := TotArr[Index];
		END;{case}
		IF GraphType <> norm THEN
		BEGIN
			IF typ < 4 THEN
				x := x * lambda[typ]
			ELSE
			BEGIN
				x := Y1Arr[Index] * lambda[1] + Y2Arr[Index] * lambda[2];
				IF option = SER THEN
					x := x + Y3Arr[Index] * lambda[3];
			END;
			x := x * 1.0E6;
		END;
		IF GraphType = log THEN
			x := ln(x);
		PlotValue := x;
	END; {PlotValue}

	PROCEDURE NewXYGraph (xlo, xhi, ylo, yhi: REAL);
		VAR
			YTitle: ARRAY[1..3] OF Str80;
			XTitle: Str80;
	BEGIN {NewXYGraph}
		YTitle[1] := 'Millions of Nuclei ';
		YTitle[2] := 'Decay Rate counts/s';
		YTitle[3] := 'Ln  Decay Rate     ';
		XTitle := 'time (sec)';
		ChooseVP(ReplotVP1);
		ClearViewPort;
		DefineScale(PlotVP, xlo, xhi, ylo, yhi);
		GraphBackColor := bk;
		SetColor(GraphBoxColor);
		SelectScale(PlotVP);
		OpenViewPort(PlotVP);
		ChooseVP(PlotVP);
		Map(xlo, ylo, XLow, YHigh);     {lower screen corner coords}
		Map(xhi, yhi, XHigh, YLow);     {upper screen corner coords}
		ScaleX := (xhi - xlo) / (XHigh - XLow);
		ScaleY := (yhi - ylo) / (YHigh - YLow);
		setcolor(white);
      axis(0,0,0,0);
      PutLabel(bottom, XTitle);
      PutLabel(left, YTitle[GraphType]);
	END; {NewXYGraph}

	PROCEDURE PlotYValue (k, w: integer; color: integer);
		VAR
			x1, y1, x2, y2, m: integer;
	BEGIN{PlotYValue}
		IF OMITY <> w THEN
		BEGIN
         SetColor(color);
		   Map(XArr[k], PlotValue(k, w), x1, y1);
		   FOR m := x1-1 TO x1+1 DO line(m, y1-1, m, y1+1);
         if k>1 then begin
   		   Map(XArr[k-1], PlotValue(k-1, w), x2, y2);
            line(x2, y2, x1, y1);
         end;
		END;
	END; {PlotYValue}

	PROCEDURE ColorPlot;
		VAR
			i, k: integer;
			x: real;
	BEGIN{ColorPlot}
		HMin := TSlice * (lf - 1);
		HMax := TSlice * (rt);
   { get vertical max & min for all possible options }
		VMax := MinMaxValue(lf, max);
		VMin := MinMaxValue(lf, min);
		FOR i := lf + 1 TO rt DO
		BEGIN
			x := MinMaxValue(i, max);
			IF x > VMax THEN VMax := x;
			x := MinMaxValue(i, min);
			IF x < VMin THEN VMin := x;
		END;
		IF GraphType = log THEN
		BEGIN
			VMax := ln(VMax);
			VMin := ln(VMin);
		END;
		ylo := VMin;
		yhi := 1.05*VMax;
		NewXYGraph(Hmin, HMax, VMin, VMax);
		SelectScale(PlotVP);
		ChooseVP(PlotVP);
 {  SetTextJustify(centertext,centertext);}
      MouseOff;
		FOR k := lf TO rt DO {plot 3 or 4 points}
		BEGIN
			PlotYValue(k, 1, Y1Cl);
			PlotYValue(k, 2, Y2Cl);
			PlotYValue(k, 4, TotCl);
			IF option = ser THEN PlotYValue(k, 3, Y3Cl);

{        PDLine(XArr,y1Arr,k,y1cl);
         PDLine(XArr,y2Arr,k,y2cl);
		   IF option = ser THEN PDLine(XArr,y3Arr,k,y3cl);
         PDLine(XArr,totArr,k,totcl);}
		END;
      MouseOn;
		SetTextJustify(LeftText, TopText);
		InitEdit;
	END; {ColorPlot}

	PROCEDURE TakeAction;   {take action based on mode}
	BEGIN{TakeAction}
		IF (Event.Readkey IN ActionKeys) OR (MouseButton <> 0) THEN
			CASE MSel OF
				'E':
				BEGIN
					EditAction;
					ShowStatus(edit);
				END;
				'L':
				BEGIN
					LineAction;
					ShowStatus(lin);
				END;
			END;{CASE}
		ChooseVP(MainVP1);
	END; {TakeAction}

	PROCEDURE SelectMode;
		VAR
			ch: char;
	BEGIN{SelectMode}
		ch := upcase(event.readkey);
		IF ch <> MSel THEN      { new selection }
		BEGIN
			MSel := ch;  {open this selection}
			CASE ch OF
				'E':
				BEGIN
					ExitLine;
					InitEdit;
					ShowStatus(Edit);
				END;
				'L':
				BEGIN
					ExitEdit;
					InitLine;
					ShowStatus(Lin);
				END;
			END;{CASE}
		END;
	END;{SelectMode}

	PROCEDURE CursorSelect; { selects which values y-cursor shows }
 { set y cursor color, y cursor number }
		VAR
			CursColor: ARRAY[1..4] OF word;
	BEGIN{CursorSelect}
		CursColor[1] := Y1Cl;
		CursColor[2] := Y2Cl;
		CursColor[3] := Y3Cl;
		CursColor[4] := TotCl;
		inc(YCurNr);                        {get next}
		IF OMitY = YCurNr THEN
			inc(YCurNr);   {skip it?}
		IF (option = act) AND (YCurNr = 3) THEN
			YCurNr := 4; {no 3rd in activation}
		IF YCurNr > 4 THEN
			YCurNr := 1;         {wrap around?}
		CursCl := CursColor[YCurNr];
		ShowCursorValues;
	END; {CursorSelect}


 {===================== M E N U     S T U F F ==============================}
	PROCEDURE SetRunHotKeys;
	BEGIN{SetHotKeys}
      HotKeysType := 'R';
		WITH HotKeys DO
		BEGIN
         clear;
			init(5);
			key[1] := 'F1-Help';
			key[2] := 'F2-KeyHelp';
			key[3] := 'F3-Stop/Go';
			key[4] := 'F4-FluxOff/On';
			key[5] := 'F10-Menu';
         active[3] := false;
         active[4] := false;
         Display;
		END;
	END; {SetHotKeys}

	PROCEDURE SetLineHotKeys;
	BEGIN{SetHotKeys}
      HotKeysType := 'L';
		WITH HotKeys DO
		BEGIN
         Clear;
			Init(11);
			key[1] := 'F1-Help';
			key[2] := 'F2-KeyHelp';
			key[3] := 'E-Edit';
			key[4] := 'F5-Equation';
         key[5] := 'Tab-NewCursor';
         key[6] := 'PgDn-NewStep';
         key[7] := 'up-Move Up';
         key[8] := 'Down-Move Down';
         key[9] := 'Left-Move Left';
         key[10] := 'Right-Move Right';
         key[11] := 'F10-Menu';
         BackColor := green;
         Display;
		END;
	END; {SetHotKeys}

	PROCEDURE SetEditHotKeys;
	BEGIN{SetEditHKeys}
      HotKeysType := 'E';
		WITH HotKeys DO
		BEGIN
         Clear;
			Init(11);
			key[1] := 'F1-Help';
			key[2] := 'F2-KeyHelp';
			key[3] := 'L-Line';
			key[4] := 'F4-FluxOff/On';
			key[5] := 'End-ZoomIn';
			key[6] := 'Home-ZoomOut';
         key[7] := 'Tab-NewCursor';
         key[8] := 'PgDn-NewStep';
         key[9] := 'Left-Move Left';
         key[10] := 'Right-Move Right';
         key[11] := 'F10-Menu';
			Display;
		END;
	END; {SetEditHKeys}

	PROCEDURE HandleTopMenu;
	forward;


	PROCEDURE HandleEditHKeys(key : byte);
	BEGIN{HandleEditHKeys1}
		CASE key OF
			1: BEGIN
				   ShowHelp1;
   				ShowHelp2;
	   		END;
			2:	KeyHelp;
			3: Event.Readkey := 'L';
			4: begin
               FluxOn := not FluxOn;
         		ShowFluxStatus;
         		ChooseVP(PlotVP);
            end;
			5: Event.Readkey := EndKey;
			6: Event.Readkey := HomeKey;
			7: Event.Readkey := PgUp;
			8: Event.Readkey := PgDn;
			9: Event.Readkey := ArrowL;
			10: Event.Readkey := ArrowR;
         11: If TopMenu.Chosen then HandleTopMenu;
		END; {case}
	END; {HandleEditHKeys1}

	PROCEDURE HandleRunHKeys(key : byte);
	BEGIN{HandleHotKeys}
		CASE key OF
			1: BEGIN
				   ShowHelp1;
   				ShowHelp2;
	   		END;
			2:	KeyHelp;
			3: Paused := not Paused;
			4:	FluxOn := not FluxOn;
			5:	IF TopMenu.Chosen THEN HandleTopMenu;
		END;{case}
	END; {HandleHotKeys}

	PROCEDURE HandleLineHKeys(key : byte);
	BEGIN{HandleLineHKeys}
		CASE key OF
			1: BEGIN
				   ShowHelp1;
   				ShowHelp2;
	   		END;
			2: KeyHelp;
			3: Event.Readkey := 'E';
			4: Event.Readkey := EndKey;
			5: Event.Readkey := PgUp;
			6: Event.Readkey := PgDn;
			7: Event.Readkey := ArrowUp;
			8: Event.Readkey := ArrowDn;
			9: Event.Readkey := ArrowL;
			10: Event.Readkey := ArrowR;
         11: If TopMenu.Chosen then HandleTopMenu;
		END; {case}
	END; {HandleLineHKeys}


	PROCEDURE SetTopMenu (YesNo: boolean);
   var i : integer;
	BEGIN{SetTopMenu}
		TopMenu.Init;
		WITH TopMenu DO
		BEGIN
			column(1, 'File');
			row(1, 1, 'About CUPS    ');
			row(1, 2, 'About Program ');
			row(1, 3, 'Configuration');
			row(1, 4, '----------------------------');
			row(1, 5, 'About Geiger Tube Counting');
			row(1, 6, 'About Decay and Growth Rates');
			row(1, 7, '----------------------------');
			row(1, 8, 'Exit Program  ');
			column(2, 'Sections');
			row(2, 1, 'Section 1 - Geiger Tube Counting  ');
			row(2, 2, 'Section 2 - Decay and Growth Rates');
         rowactivate(1,4,false);
         rowactivate(1,7,false);
         autochecking(2,2);
			column(3, 'Run');
			row(3, 1, 'Activation');
			row(3, 2, 'Series of 3');
			column(4, 'Pick Plot');
			row(4, 1, 'Population vs. time');
			row(4, 2, 'Decay Rate vs. time');
			row(4, 3, 'Ln(Decay Rate) vs. time');
			row(4, 4, '-- Y-Axis parameters --');
			row(4, 5, 'Plot all');
			row(4, 6, 'Plot all but species 1');
			row(4, 7, 'Plot all but species 2');
			row(4, 8, 'Plot all but species 3');
			row(4, 9, 'Plot all but total');
			for i := 1 to 9 do RowActivate(4, i, YesNo);
			RowActivate(4, 4, FALSE);
         RowChecked(4,1,true);
         RowChecked(4,5,true);
         ColSize := 11;
			OmitY := 5;
			GraphType := 1;
         Display;
		END;
	END; {SetTopMenu}


	PROCEDURE SniffKeys;
	BEGIN{SniffKeys}
		CheckForEvents;
		MouseGlobalPosn(GMousX, GMousY, MouseButton);
		IF Hotkeys.pressed(key) THEN
      case HotKeysType of
       'E':	HandleEditHKeys(key);
       'L': HandleLineHKeys(key);
       'R': HandleRunHKeys(key);
      end;
{		IF Event.DoubleClicked THEN Event.Readkey := EndKey; }
		IF (Event.ReadKey IN SelectKeys) AND (NOT Event.ExtendedKey) THEN
			SelectMode;
		IF (Event.ReadKey IN ActionKeys) OR (MouseButton <> 0) THEN
			TakeAction;
		IF TopMenu.Activated THEN
			HandleTopMenu;
	END; {SniffKeys}

	PROCEDURE HandleTopMenu;
	VAR
			i: integer;
	BEGIN{HandleTopMenu}
		WITH TopMenu DO
			CASE colChosen OF
				1:
					CASE rowchosen OF
						1: begin AboutCups; SetRGBPalette(green, $00, $18, $7); end;
						2: OpeningScreen;
						3: Configuration;
						5: CountModuleInfo;
						6: ShowFrontInfo;
						8: begin QuitFlag := TRUE; ReturnFlag := true; end;
					END; {case}
				2:
				BEGIN
					ReturnFlag := True;
               section := rowChosen;
				END;
				3: {set activation, series parms}
				BEGIN
            	SetRunHotKeys;
					CASE rowchosen OF
						1: begin Screen1Data; option := act; end;
						2: begin Screen2Data; option := ser; end;
					END;{Case}
               HotKeys.active[3] := true;
               HotKeys.active[4] := true;
               HotKeys.Display;
					paused := False;
			      StatusShowing := FALSE; {to reflect new option}
			      ShowStatus(run);
			      GraphLimits;
      			NewXYGraph(HMin, HMax, 0, VMax);
		         WHILE (PlotPtsNr > slice) AND (NOT ReturnFlag) DO
		         BEGIN
			         inc(slice);
			         PDSlice(slice);
			         MouseOn;
			         SniffKeys;
                  If Paused then DynamicPause;
		         END;
		         Graphing := TRUE;
				END;
				4:
				BEGIN
					slice := PlotPtsNr;
					IF rowchosen<4 THEN
               begin
                  GraphType := rowchosen;
   					FOR i := 1 TO 3 DO RowChecked(4, i, FALSE);
               end else	BEGIN
						OmitY := rowchosen - 5;
						FOR i := 5 TO 9 DO RowChecked(4, i, FALSE);
					END;
					RowChecked(4, rowchosen, TRUE);
					IF (rt > lf) THEN ColorPlot;
				END;
			END;{CASE}
	END;{HandleTopMenu}



	PROCEDURE DataDefaults;
	BEGIN{DataDefaults}
		HLife[1] := 144.0;
		HLife[2] := 24.5;
		Frac[1] := 0.5;
		Frac[2] := 0.5;
		Cs[1] := 35.0;
		Cs[2] := 84.0;
		BarnRate := 1000;
		GrTime1 := 600.0;
		ff1 := 100;
		SLife[1] := 100.0;
		SLife[2] := 10.0;
		SLife[3] := 2E7;
		NrOne := 1.0;
		GrTime2 := 600.0;
		ff2 := 10;
		YCurNr := 1;
		CursCl := Y1Cl;
		lf := 1;
		rt := 1;
      ac := 1;
      ci := 2;
	END; {DataDefaults}

	PROCEDURE SetDefaults;
	BEGIN{SetDefaults}
		DataDefaults;
		ReturnFlag := false;
		FluxOn := true;
		Option := act;
		StatusShowing := FALSE;
		GraphType := norm;
		SetGraphVP;
		paused := FALSE;
		Graphing := FALSE;
	END; {SetDefaults}

 {=================== MAIN RATES MODULE PROGRAM ================}
var
   i : integer;
BEGIN{Rates}
  	SetRunHotKeys;
   SetTopMenu(FALSE);
	SetDefaults;
	NewXYGraph(0.0, 10.0, 0.0, 10.0);
	REPEAT
		SniffKeys;
		IF (Graphing) AND (NOT paused) THEN
		BEGIN
         {just left the plotting loop}
			Graphing := FALSE;
			for i := 1 to 9 do TopMenu.RowActivate(4, i, true);
			TopMenu.RowActivate(4, 4, FALSE);
         {show 'new' menu w/extra items on it}
			InitEdit;
			ShowStatus(Edit);
		END;
		MouseOn;
	UNTIL ReturnFlag;
	MouseOff;
{	TopMenu.Done;}
END;{Rates}

 {===========================================================================}
 {=========  E N D   N U C L E A R   R A T E S    M O D U L E ===============}
 {===========================================================================}




 {===========================================================================}
 {=========== N U C L E A R   C O U N T I N G    M O D U L E ================}
 {===========================================================================}
PROCEDURE Counting (InfoScreen: Boolean);

	CONST
		MaxCount = 1000; {Max Count in any sample}
		ReplotNr = 200; {Nr of trials done before redoing histogram}
		CCl = lightred; {color of stuff that changes}
		BotVP = 3; {for bottom 0.1 vertically; about 3 lines }
		TopVP = 4; {for top    0.1 vertically; about 3 lines }

	CONST
		mode = 2;
		Pd = 6;
		PStatus = 11;
		Keys = 16;
		Gr = 23;
		lgy = lightgray;
		Heading = white;
		MaxStatus = 4;
		wait = 1;
		run = 2;
		pois = 3;
		data = 4;

	VAR
		key: BYTE;
		TopMenu: TMenu;
		HotKeys: THotKeys;
		TopDisplay: boolean;
		St9: STRING[9];
		StatusShowing {mode status} 		: integer;
		SStr: ARRAY[1..MaxStatus] OF STRING[10];
	VAR
		Nuclei, Tdead, Hlife, TSamp: real;
		Fishy {poisson parameter} : real;
		AvgRate: real;
		total: real;
		NSamp, ig, samples: integer;
		HMin, Hmax, Vmax, TAvg, Avg: real;
		ValidPlot, ReturnFlg: boolean;
		GMHist, IdealHist: ARRAY[0..MaxCount] OF integer;
		HelpIndex: integer;

	PROCEDURE ClearScreen;
	BEGIN{ClearScreen}
		ChooseVP(FullVP);
		LeaveVP;
		TopDisplay := FALSE;
	END; {ClearScreen}


	PROCEDURE ShowModeStatus (Nr: integer);
	BEGIN{ShowModeStatus}
		IF (Nr <> StatusShowing) AND (Nr <= MaxStatus) THEN
		BEGIN
			StatusShowing := Nr;
			WriteGr(StatX[1], StatY[20], SStr[Nr], txt);
		END;
	END;{ShowModeStatus}


	PROCEDURE PoissonInfo;
		VAR
			i: integer;
	BEGIN{PoissonInfo}
		FOR i := 1 TO 25 DO
			HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}

		HS[2] := ' A ''walk-through'' simulation of counting is';
		HS[3] := ' available (and recommended) under ''Section 1''';

		HS[5] := ' If the counts are very slow in showing up,';
		HS[6] := ' you may have a large number of counts in ';
		HS[7] := ' each sample, and an extremely large number';
		HS[8] := ' overall. If so, you can <F10> to the menu,';
		HS[9] := ' reset the parameters, and restart.';

		HS[15] := ' One may observe the closeness of the results';
		HS[16] := ' to a POISSON distribution by selecting that';
		HS[17] := ' option from the main menu. One may THEN vary';
		HS[18] := ' the poisson parameter with ARROW KEYS, or by';
		HS[19] := ' dragging near the center with the MOUSE. The ';
		HS[20] := ' ideal counts and GMT counts are shifted by half';
		HS[21] := ' a box each so both can be displayed. The user';
		HS[22] := ' should remain aware of this half-box offset.';

		HS[25] := ' Press <Enter> or click mouse to continue.  ';
		Help(HS);
      SetRGBPalette(green, $00, $18, $7);
	END;{PoissonInfo}

	PROCEDURE PoissonHelp;
		VAR
			i: integer;
	BEGIN{PoissonHelp}
		FOR i := 1 TO 25 DO
			HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}

		HS[3] := ' Active Keys:';
		HS[5] := ' Arrow Keys will move the poisson parameter E,';
		HS[6] := ' and replot the poisson curve at the same time.';
		HS[7] := ' The PageUp and PageDown keys will change the';
		HS[8] := ' increment by which E is moved. You can see the';
		HS[9] := ' value of E and the increment in E displayed';
		HS[10] := ' in the status panel on the right of the screen.';

		HS[20] := ' Ideal counts and GMT counts are shifted by half';
		HS[21] := ' a count each so both can be displayed. The user';
		HS[22] := ' should remain aware of this half-count offset.';

		HS[25] := ' Press <Enter> or click mouse to continue.  ';
		Help(HS);
      SetRGBPalette(green, $00, $18, $7);
	END;{PoissonHelp}

   Procedure SetParameterDefaults;
   forward;

	PROCEDURE Screen1Data;
   var Default : boolean;
	BEGIN {Screen1Data}
		ShowModeStatus(data);
		WITH RScreen DO
		BEGIN
			init;
			DefineInputPort(0.07, 0.93, 0.3, 0.7);
			LoadLine(' Numeric parameter input:   Nuclear Counting Section ');
			LoadLine('');
			LoadLine(' Average count rate from decays    = {       }/sec (0.01 to 10,000)');
			LoadLine(' Counting interval for each sample = {       }sec (0.01 to 1000)');
			LoadLine(' Total number of samples           = {       } (1 to 10,000)');
			LoadLine(' Geiger-Muller Tube Dead Time      = {       }sec');
			LoadLine('  (ideal: 0;  generic: 3E-4;  fast: 5E-5  sec');
			LoadLine('');
			LoadLine('       [  Ok  ]  [Cancel]  [Default]');
			SetInput(1, AvgRate, 0.01, 1E4);
			SetInput(2, TSamp, 0.01, 1000);
			SetInput(3, NSamp, 1.0, 10000);
			SetInput(4, TDead, 0, 1E-3);
         repeat
			   Accept;
            Default := getBoolean(7);
            if Default then begin
               SetParameterDefaults;
			      SetInput(1, AvgRate, 0.01, 1E4);
			      SetInput(2, TSamp, 0.01, 1000);
			      SetInput(3, NSamp, 1.0, 10000);
			      SetInput(4, TDead, 0, 1E-3);
            end;
         until not Default;
         ClearInputPort;
			AvgRate := GetNumber(1);
			TSamp := GetNumber(2);
			NSamp := round(GetNumber(3));
			Tdead := GetNumber(4);
			done;
		END;{with}
	END;{Screen1Data}

	PROCEDURE ClearHistoArrays;
		VAR
			i: integer;
	BEGIN{ClearHistoArrays}
		FOR i := 0 TO MaxCount DO
		BEGIN
			GMHist[i] := 0;
			IdealHist[i] := 0;
		END;
	END; {ClearHistoArrays}

	PROCEDURE GraphLimits;
		VAR
			root: real;
	BEGIN{GraphLimits}
 {}
{  1.   Find avg nr of expected counts                    TSamp ln 2/HL N}
{  2.   sigma is root of N, so go 3 sigma to either side  3 root N}
{  3.   height is nr of samples div sigma, roughly        NSamp / root N}
{}
		Avg := TSamp * AvgRate;           {expected avg counts in TSamp}
		fishy := 0.9 * Avg;                 {initial poisson parameter}
		root := sqrt(Avg);               { std dev of avg counts }
		Tavg := 1.0 / AvgRate;             {avg interval between counts}
		VMax := NSamp / (3.0 * root);
		Hmin := avg - 5.0 * root; {expected lo x value}
		IF Hmin < 0.0 THEN
			Hmin := -0.5;
		HMax := avg + 7.0 * root; {expected hi x value}
		ValidPlot := true;
		ClearHistoArrays;
		Samples := 0;
      HotKeys.active[3] := true;
      HotKeys.Display;
	END;{GraphLimits}

	PROCEDURE DoCountTrials;
		VAR
			GMNr, IdealNr, trial: integer;
			TimeOfNextCount: real;
			EndOfDeadTime: real;

		PROCEDURE RegisterCount;  {register ideal and maybe GM count}
		BEGIN{RegCnt}
			Inc(IdealNr);
			IF TimeOfNextCount > EndOfDeadTime THEN
			BEGIN
				Inc(GMNr);    { & update dead time expiration}
				EndOfDeadTime := TimeOfNextCount + TDead;
			END;
			TimeOfNextCount := TimeOfNextCount - TAvg * ln(1.0 - random);
		END; {RegCnt}

	BEGIN{DoCountTrials}
		ShowModeStatus(run);
		GMNr := 0;
		IdealNr := 0;
		trial := 0;
		TimeOfNextCount := -Tavg * ln(1.0 - random); {initial time interval}
		EndOfDeadTime := 0.0;                  {no initial dead time}
		WHILE ReplotNr > trial DO {counting trials}
			IF TimeOfNextCount > TSamp THEN
			BEGIN
				inc(GMHist[GMNr]); { save last sample }
				inc(IdealHist[IdealNr]);
				inc(trial);
				GMNr := 0;
				IdealNr := 0;
				TimeOfNextCount := TimeOfNextCount - TSamp;
				EndOfDeadTime := EndOfDeadTime - TSamp;
			END
			ELSE
				RegisterCount;
	END;{DoCountTrials}

	PROCEDURE GenericStatus; {Common Status box stuff}
		CONST
			Gr = 22;
		VAR
			AvStr: STRING[16];
	BEGIN{GenericStatus}
		ChooseVP(StatusVP);
		ClearViewPort;
		WriteGr(StatX[1], StatY[Gr-3], 'Present Mode is', Heading);
		WriteGr(StatX[1], StatY[Gr], 'Graph Information', Heading);
		WriteGr(StatX[1], StatY[Gr + 2], '  GMT Counts:', lgy);
		WriteGr(StatX[1], StatY[Gr + 3], 'Ideal Counts:', lgy);
		MBar(605, 288, 625, 296, GMClr);
		MBar(605, 300, 625, 308, IdClr);
		WriteGr(StatX[1], StatY[Gr + 5], Concat('Av Counts/s:', NumStr(Avg, 5, 1)), txt);
		WriteGr(StatX[1], StatY[Gr + 9], 'Tmax=', lgy);
		WriteGr(StatX[4], StatY[Gr + 9], NumStr(TSamp * NSamp, 8, 1), txt);
		WriteGr(StatX[10], StatY[Gr + 9], 's', lgy);
	END;{GenericStatus}

	PROCEDURE ShowHistoGraph (xlo, xhi, YRange: REAL);
		CONST
			XAxLabel:
			str80 = 'Counts';
			YAxLabel:
			str80 = 'Number of Occurrences';
		VAR
			xlow, xhigh, ylow, yhigh: integer;
			ylo, yhi: real;
	BEGIN {ShowHistoGraph}
		MouseOff;       {otherwise its screen area stays!}
		ChooseVP(ReplotVP1);
		ClearViewPort;
		ylo := 0.0;
		yhi := yrange;
		DefineScale(PlotVP, xlo, xhi, ylo, yhi);
		GraphBackColor := bk;
		SetColor(GraphBoxColor);
		SelectScale(PlotVP);
		OpenViewPort(PlotVP);
		Map(xlo, ylo, XLow, YHigh);     {lower screen corner coords}
		Map(xhi, yhi, XHigh, YLow);     {upper screen corner coords}

		Axis(xlo, 0, 0, 0);
		Setcolor(White);
		PutLabel(Bottom, 'Counts');
		PutLabel(Left, 'Number of Occurrences');
		IF HelpIndex<>1 THEN GenericStatus;
		ChooseVP(PlotVP);
      MouseOn;
	END; {ShowHistoGraph}

	PROCEDURE PlotHistogram;
		VAR
			i, MaxIndex, MinIndex, MaxC: integer;
			i1, i2, j1, j2: integer;
	BEGIN{PlotHistogram}
		i := MaxCount;
		REPEAT
			MaxIndex := i;
			dec(i);
		UNTIL IdealHist[i] <> 0;
		i := -1;
		REPEAT
			MinIndex := i;
			inc(i);
		UNTIL GMHist[i] <> 0;
		IF MinIndex < 0 THEN
			MinIndex := 0;
		MaxC := 0;
		total := 0;
		FOR i := MinIndex TO MaxIndex DO
		BEGIN
			total := total + IdealHist[i];
			i1 := IdealHist[i];
			i2 := GMHist[i];
			IF i1 > MaxC THEN
				MaxC := i1;
			IF i2 > MaxC THEN
				MaxC := i2;
		END;
   {see if rescaling needed}
		IF (MaxC > round(0.95 * Vmax)) OR (MinIndex < round(Hmin)) OR (MaxIndex + 1 > round(Hmax)) THEN    { H & or V rescale}
		BEGIN
			IF MaxC > Round(0.95 * Vmax) THEN
				Vmax := VMax * 4.0;
			IF MinIndex - 1 < Hmin THEN
				Hmin := MinIndex - 5;
			IF HMin < 0 THEN
				HMin := -0.5;
			IF MaxIndex + 1 > Hmax THEN
				HMax := MaxIndex + 12;
			ShowModeStatus(wait);    {so 'run' will reappear}
			ShowHistoGraph(HMin, HMax, VMax);
		END;
		SelectScale(PlotVP);
		SelectViewPort(PlotVP);
		FOR i := MinIndex TO MaxIndex DO
		BEGIN
			Map(i - 0.5, 0.0, i1, j1);
			Map(i, GMHist[i], i2, j2);
			SetFillStyle(SolidFill, GMClr);
			Bar(i1, j1, i2, j2);
			Map(i, 0.0, i1, j1);
			Map(i + 0.5, IdealHist[i], i2, j2);
			SetFillStyle(SolidFill, IdClr);
			Bar(i1, j1, i2, j2);
		END;
		SelectViewPort(StatusVP);
		St9 := NumStr(RePlotNr * TSamp * samples, 8, 1);
		WriteGr(StatX[1], StatY[Gr + 7], 't=', txt);
		WriteGr(StatX[3], StatY[Gr + 7], St9, CCl);
		WriteGr(StatX[9], StatY[Gr + 7], 'sec', txt);
		SelectViewPort(PlotVP);
		ShowModeStatus(run);
	END;{PlotHistogram}

   procedure HandleTopMenu; forward;

	PROCEDURE PoissFit;                                              {PoissFit}
		VAR
			poiss: ARRAY[0..1000] OF real;
			ilabl: ARRAY[1..6] OF STRING[4];
			incval: ARRAY[1..6] OF real;
			m1, m2, m3, m4, m5, m6: integer;
			ii, k, i1, i2, j1, j2: integer;
			lf, rt, bx, by: integer;
			ReturnFlag, MenuFlag : boolean;
			OldFish: real;
			q, ch, sv: char;
			hdr1, hdr2, hdr3, eqform, v: STRING;
			PHotkeys: THotkeys;
			key: byte;
		CONST
         n = 8;
 {rows down for Poiss eqn print }

		PROCEDURE SetConstantArrays;
			VAR
				i: integer;
		BEGIN {SetConstArrays}
			ilabl[1] := '.001';
			ilabl[2] := ' .01';
			ilabl[3] := ' .10';
			ilabl[4] := '   1';
			ilabl[5] := '  10';
			ilabl[6] := ' 100';
			incval[1] := 0.001;
			FOR i := 2 TO 6 DO
				incval[i] := 10.0 * incval[i - 1];
		END;  {SetConstArrays}

		PROCEDURE FreshFishFit; forward;

		PROCEDURE UpIncr;
		BEGIN
			inc(ii);
			IF ii > 6 THEN
				ii := 1;
		END;     {UpIncr}

		PROCEDURE DnIncr;
		BEGIN
			dec(ii);
			IF ii < 1 THEN
				ii := 6;
		END;     {DnIncr}

		PROCEDURE IncrE;
		BEGIN  {IncrE}
			Fishy := Fishy + incval[ii];
			IF Fishy < 0.0 THEN
				Fishy := 0.0;
			FreshFishFit;
		END;   {IncrE}

		PROCEDURE DecrE;
		BEGIN  {DecrE}
			Fishy := Fishy - incval[ii];
			IF Fishy < 0.0 THEN
				Fishy := 0.0;
			FreshFishFit;
		END;   {DecrE}

		PROCEDURE SetPHotKeys;  {poisson hotkeys}
		BEGIN {SetPHotKeys}
			WITH PHotKeys DO
			BEGIN
				init(6);
				key[1] := 'F1-Help';
				key[2] := 'RIGHT-Incr E';
				key[3] := 'LEFT-Decr E';
				key[4] := 'PgUp-StepUp';
				key[5] := 'PgDn-StepDn';
				key[6] := 'Esc-Exit';
            BackColor := green;
       {Display;}
			END;
		END; {SetPHotKeys}

		PROCEDURE UpdatePoissStatus;
		forward;

		PROCEDURE HandlePHotKeys;
		BEGIN{HandlePHotKeys}
			CASE key OF
				1:	PoissonHelp;
				2:	IncrE;
				3:	DecrE;
				4:	UpIncr;
				5:	DnIncr;
				6:	ReturnFlag := true;
			END;{CASE}
			if key in [2,3,4,5] then UpdatePoissStatus;
		END;{HandlePHotKeys}

		PROCEDURE StaticPoissStatus;
		BEGIN
			WriteGr(StatX[1], StatY[n+1], 'Poisson ', heading);
			WriteGr(StatX[1], StatY[n+2], 'Distribution', heading);
			WriteGr(StatX[1], StatY[n+3], 'Pn = ', lgy);
			WriteGr(StatX[1], StatY[n+4], 'exp(-E) (E)^n /n!', lgy);
			WriteGr(StatX[1], StatY[n+7], 'Poisson Status', Heading);
			WriteGr(StatX[1], StatY[n+8], '        E:', lgy);
			WriteGr(StatX[1], StatY[n+9], 'Step Size:', lgy);
		END;{StaticPoissStatus}

		PROCEDURE UpdatePoissStatus;
		BEGIN {UpdatePoissStatus}
			WriteGr(StatX[9], StatY[n+9], ilabl[ii], txt);
			WriteGr(StatX[8], StatY[n+8], NumStr(Fishy, 6, 3), txt);
		END;{UpdatePoissStatus}

		PROCEDURE Fish (parm: real);
			VAR
				i: integer;
		BEGIN{Fish}
			poiss[0] := total * exp(-parm);
			FOR i := 1 TO rt DO
				poiss[i] := poiss[i - 1] * parm / i;
		END;{Fish}

		PROCEDURE PlotPoiss (E: real; couleur: integer);
			VAR
				k, SaveClr: integer;
		BEGIN{PlotPoiss}
			SelectViewPort(PlotVP);
			SelectScale(PlotVP);
			SaveClr := GetColor;
			SetColor(couleur);
			SetWriteMode(XORPut);
			Fish(E);{all poisson pieces}
			lf := 0;
			Map(lf + 0.01, Poiss[lf], i1, j1);
			FOR k := lf + 1 TO rt DO
			BEGIN
				Map(k + 0.01, Poiss[k], i2, j2);
				Line(i1, j1, i2, j2);
				i1 := i2;
				j1 := j2;
			END;
			SetWriteMode(CopyPut);
			SetColor(SaveClr);
		END;{PlotPoiss}

		PROCEDURE FreshFishFit;
		BEGIN{FreshFishFit}
			WriteGr(StatX[8], StatY[n+8], NumStr(Fishy, 6, 3), txt);
			PlotPoiss(OldFish, FshCl);
			OldFish := Fishy;
			PlotPoiss(Fishy, FshCl);
		END;{FreshFishFit}

	BEGIN  {PoissFit}
		SetConstantArrays;
{		HotKeys.Clear;}
		ReturnFlag := false;
		SetPHotkeys;
		PHotkeys.display;
		ChooseVp(TopVP);
{		ClearViewPort;            }
    { take out top menu, we hope }
		MouseOn;
		StaticPoissStatus;
		ShowModeStatus(pois);
		lf := round(Hmin);
		rt := round(Hmax);
		setcolor(txt);
		ii := 3;
		PlotPoiss(Fishy, FshCl);
		OldFish := Fishy;
		UpDatePoissStatus;
      MenuFlag := false;
		REPEAT
{			ch := INS;}
			SelectViewPort(PlotVP);
			SelectScale(PlotVP);
			MouseGlobalPosn(GMousX, GMousY, MouseButton);
			MousePosn(Rx, Ry, PlotVP, PlotVP, inside);
			IF (MouseButton <> 0) AND (abs(Rx - Fishy) < 10) THEN
			BEGIN
				REPEAT           {while button down}
					MouseGlobalPosn(GMousX, GMousY, MouseButton);
					MousePosn(Rx, Ry, PlotVP, PlotVP, inside);
					IF inside THEN
					BEGIN
						MouseOff;
						Fishy := Rx;
						FreshFishFit;
						ChooseVP(PlotVP);
						MouseOn;
						Delay(25);
					END;
				UNTIL MouseButton = 0; {button up}
			END;
			CheckForEvents;
			IF PHotkeys.pressed(key) THEN HandlePHotkeys;
    		IF TopMenu.Activated THEN
         begin
            if topMenu.ColChosen=1 then
             case topMenu.RowChosen of
					 1: begin AboutCups; SetRGBPalette(green, $00, $18, $7); end;
					 2: OpeningScreen;
					 3: Configuration;
					 5: CountModuleInfo;
					 6: ShowFrontInfo;
					 8: MenuFlag := true;
				 END {case}
            else MenuFlag := true;
         end;
		UNTIL ReturnFlag or MenuFlag;
		PlotPoiss(Fishy, FshCl);     {erase last curve & leave}
		ChooseVP(StatusVP);
		ClearViewPort; {clear poisson stuff}
		ShowModeStatus(wait);
		SelectViewPort(PlotVP);
		GenericStatus;
		Hotkeys.Display;
      if MenuFlag then HandleTopMenu;
{		TopMenu.Display;}
	END;{PoissFit}

	PROCEDURE Restart; { restart entire histogram }
	BEGIN{Restart}
		GraphLimits;
		ShowHistoGraph(HMin, HMax, VMax);
		ClearHistoArrays;
		Samples := 0;
	END; {Restart}

	PROCEDURE TapeSetup;
	BEGIN {TapeSetup}
		MBar(505, 50, 555, 55, noir);
		MBar(575, 50, 625, 55, noir);
		MBar(505, 406, 625, 470, noir);
		MLine(556, 405, 574, 405, noir);
		WriteGr(StatX[2], StatY[1], 'Ideal', yellow);
		WriteGr(Statx[2], StatY[2], 'counts', yellow);
		WriteGr(StatX[7], StatY[1], 'G-M', yellow);
		WriteGr(StatX[7], StatY[2], 'counts', yellow);
		MBar(505, 55, 555, 405, IdClr);
		MBar(575, 55, 625, 405, GMClr);
		MLine(556, 55, 574, 55, lightred);
	END;{TapeSetup}

	PROCEDURE SimulatedDataRun (VAR IDealNr, GMNr: integer);
		VAR
			TimeNextCount, i: integer;
			TimeOfNextCount: real;
			EndOfDeadTime: real;

		PROCEDURE RegisterCount;  {register ideal and maybe GM count}
		BEGIN{RegCnt}
			Inc(IdealNr);
			IF i > round(EndOfDeadTime) THEN
			BEGIN
				Inc(GMNr);    { & update dead time expiration}
				MBar(575, i + 55, 625, i + 55 + round(TDead), lgy);
				MBar(597, i + 52, 603, i + 58, blanc);
				EndOfDeadTime := i + TDead;
			END;
			TimeOfNextCount := -100.0 * TAvg * ln(1.0 - random);
			TimeNextCount := i + round(TimeOfNextCount);
		END; {RegCnt}

	BEGIN{SimulatedDataRun}
		TapeSetup;
		randomize;
		GMNr := 0;
		IdealNr := 0;
		TDead := 40.0;
    {350 ticks }
		TimeNextCount := round(-100.0 * Tavg * ln(1.0 - random));
		EndOfDeadTime := 0.0;
		SetFillStyle(SolidFill, white);
		FOR i := 1 TO 350 DO
		BEGIN
			delay(20);
			MLine(556, 54 + i, 574, 54 + i, noir);
			MLine(556, 55 + i, 574, 55 + i, lightred); {move time-line}
			IF i >= TimeNextCount THEN
			BEGIN
          { plot ideal count; get a new time }
				beep;
				MBar(527, i + 52, 533, i + 58, blanc);
				RegisterCount;
			END;
		END;
		inc(GMHist[GMNr]);
		inc(IdealHist[IdealNr]);
	END;{SimulatedDataRun}

	PROCEDURE AddData;
		VAR
			i, i1, i2, j1, j2: integer;
	BEGIN{AddData}

		SelectScale(PlotVP);
		SelectViewPort(PlotVP);
		FOR i := 0 TO 20 DO
		BEGIN
			Map(i - 0.5, 0.0, i1, j1);
			Map(i, GMHist[i], i2, j2);
			SetFillStyle(SolidFill, GMClr);
			Bar(i1, j1, i2, j2);
			Map(i, 0.0, i1, j1);
			Map(i + 0.5, IdealHist[i], i2, j2);
			SetFillStyle(SolidFill, IdClr);
			Bar(i1, j1, i2, j2);
		END;
	END;{AddData}

	PROCEDURE Tutorial;
		VAR
			st: Str80;                  {Tutorial}
			i, IdealNr, GMNr, NSampSave: integer;
			AvRateSave, TDeadSave, TSampSave: real;
  { put up graph, and several sets of text }
		CONST
			Edlines = 8;
		VAR
			EditPage: ARRAY[1..Edlines] OF STRING[60];
	BEGIN{Tutorial}
      HotKeys.Clear;
		EditPage[1] := 'This tutorial will ''walk through'' some simulated';
		EditPage[2] := 'data taking by an ''ideal'' detector, and a Geiger-';
		EditPage[3] := 'Muller tube with finite dead time.';
		EditPage[4] := '';
		EditPage[5] := '   Press ''s'' to start a simulated data run.';
		EditPage[6] := '   Press ''a'' when the run is done to add data to graph';
		EditPage[7] := '   Press ''x'' to exit this tutorial (when you have done';
		EditPage[8] := '    enough runs and added enough data to the histogram.)';
		IF HelpIndex = 1 THEN {initial tutorial setup}
		BEGIN
			ChooseVP(FullVP);
			ClearViewPort;
			DefineViewport(HelpTextVP, 0.02, 0.75, 0.05, 0.34);
			SetHelpGraphVP;
			AvRateSave := AvgRate;
			AvgRate := 2.5;
			TSampSave := TSamp;
			TSamp := 4.0;
			TDeadSave := TDead;
			TDead := 0.4;
			NSampSave := NSamp;
			NSamp := 1;
			GraphLimits;
			ShowHistoGraph(0.0, 20.0, 5.0);
		END;
		ChooseVP(HelpTextVP);
		FOR i := 1 TO EdLines DO
			WriteGr(0.01, StatY[28 + i], EditPage[i], yellow);
		ClearHistoArrays;
		TapeSetup;
		REPEAT
			ch := readkey;
			CASE ch OF
				's', 'S':
					SimulatedDataRun(IdealNr, GMNr);
				'a', 'A':
					AddData;
			END;{case}
		UNTIL (ch = 'x') OR (ch = 'X');
		ChooseVP(FullVP);
		ClearViewPort;
		TopDisplay := FALSE;
		MouseOn;
		HelpIndex := 0;
		ValidPlot := FALSE;
		AvgRate := AvRateSave;
		TSamp := TSampSave;
		NSamp := NSampSave;
		TDead := TDeadSave;
	END; {Tutorial}

 {===================== M E N U     S T U F F ==============================}
	PROCEDURE SetTopMenu;
	BEGIN {SetTopMenu}
		TopMenu.Init;
		WITH TopMenu DO
		BEGIN
			column(1, 'File');
			row(1, 1, 'About CUPS    ');
			row(1, 2, 'About Program ');
			row(1, 3, 'Configuration');
			row(1, 4, '----------------------------');
			row(1, 5, 'About Geiger Tube Counting');
			row(1, 6, 'About Decay and Growth Rates');
			row(1, 7, '----------------------------');
			row(1, 8, 'Exit Program  ');
			column(2, 'Sections');
			row(2, 1, 'Section 1 - Geiger Tube Counting  ');
			row(2, 2, 'Section 2 - Decay and Growth Rates');
         rowactivate(1,4,false);
         rowactivate(1,7,false);
         autochecking(2,1);
			column(3, 'Set Params');
			row(3, 1, 'Set Counting Parameters');
			column(4, 'Tutorial');
			row(4, 1, 'Counting Tutorial ''Walk-Through''');
         ColSize := 11;
		END;
	END;{SetTopMenu}

	PROCEDURE HandleTopMenu;
	BEGIN{HandleTopMenu}
		WITH TopMenu DO
			CASE colChosen OF
				1:
					CASE rowchosen OF
						1: begin AboutCups; SetRGBPalette(green, $00, $18, $7); end;
						2: OpeningScreen;
						3: Configuration;
						5: CountModuleInfo;
						6: ShowFrontInfo;
						8: begin QuitFlag := TRUE; ReturnFlg := true; end;
					END; {case}
				2:
				BEGIN
					ReturnFlg := True;
               section := rowChosen;
				END;
				3: CASE RowChosen OF
						1:
						BEGIN
							Screen1Data;
							GraphLimits;
							ShowHistograph(HMin, HMax, VMax);
						END;
					END; {case}
             4: if RowChosen=1 then begin
                   HelpIndex := 1;
                   Tutorial;
                end;
			END;{CASE}
	END;{HandleTopMenu}

	PROCEDURE SetHotKeys;
	BEGIN {SetHotKeys}
		WITH HotKeys DO
		BEGIN
			init(5);
			key[1] := 'F1-Help';
			key[2] := 'F2-Start Run';
			key[3] := 'F3-Stop/Go';
			key[4] := 'F4-Poisson';
			key[5] := 'F10-Menu';
		END;
	END; {SetHotKeys}

	PROCEDURE HandleHotKeys;
	BEGIN{HandleHotKeys}
		CASE key OF
			1:	PoissonInfo;
			2:
			BEGIN
				GraphLimits;
				ShowHistograph(HMin, HMax, VMax);
			END;
			3:  {toggle between run pause}
				IF ValidPlot THEN
				BEGIN
					ShowModeStatus(wait);
					ValidPlot := false;
				END
				ELSE
				BEGIN
					ShowModeStatus(run);
					ValidPlot := true;
            END;
			4:
				IF Samples > 1 THEN
				BEGIN
					MouseOff;
					PoissFit;
				END
				ELSE Beep;
			5:
				IF TopMenu.Chosen THEN
					HandleTopMenu;
		END;{CASE}
	END;{HandleHotKeys}

	PROCEDURE SniffKeys;
	BEGIN{SniffKeys}
		CheckForEvents;
		IF Hotkeys.pressed(key) THEN
			HandleHotKeys;
		IF TopMenu.Activated THEN
			HandleTopMenu;
	END; {SniffKeys}

	PROCEDURE SetParameterDefaults;
	BEGIN{SetParameterDefaults}
		AvgRate := 250.0;
		TSamp := 0.10;
		NSamp := 10000;
		TDead := 2E-4;
	END; {SetParameterDefaults}

	PROCEDURE SetDefaults;
	BEGIN{SetDefaults}
		SStr[1] := 'Waiting';
		SStr[2] := 'Running';
		SStr[3] := 'Poisson';
		SStr[4] := '       '; {had been 'Data  '}
     {mode status string}
		ReturnFlg := false;
		StatusShowing := pois;
		TopDisplay := FALSE;
		MouseOff;
		SetParameterDefaults;
		Samples := 1;
		SetGraphVP;
		HelpIndex := 0;
		ValidPlot := false;
		Avg := 0.0;
	END; {SetDefaults}

 {================= MAIN COUNTING MODULE PROGRAM ============}
BEGIN{Counting}
	SetTopMenu;
	SetHotKeys;
	SetDefaults;
	ClearMUPPETport;
 {IF InfoScreen THEN ShowFrontInfo; {Show Info Screen if asked}
	REPEAT
		SelectViewPort(FullVP);
		MouseOn;
		IF HelpIndex = 0 THEN
			SetGraphVP
		ELSE
			SetHelpGraphVP;
		IF (NOT TopDisplay) THEN
		BEGIN
			Hotkeys.Display;
			TopDisplay := TRUE;
			TopMenu.Display;
		END;
		WHILE (NSamp > ReplotNr * Samples) AND (NOT ReturnFlg) AND (ValidPlot) DO
		BEGIN
			inc(Samples);
			DoCountTrials;
			PlotHistogram;
			SniffKeys;
		END;
		IF (NSamp <= ReplotNr * Samples) AND (ValidPlot) THEN begin
			ShowModeStatus(wait);
         HotKeys.active[3] := false;
         HotKeys.Display;
         VAlidPlot := false;
      end;
		SniffKeys;
	UNTIL ReturnFlg;
	ChooseVP(MainVP1);
	LeaveVP;
	TopMenu.Done;
END;{Counting}
 {===========================================================================}
 {===  E N D   N U C L E A R   C O U N T I N G    M O D U L E ===============}
 {===========================================================================}

 {===========================================================================}
 {=============== M A I N    M E N U     M O D U L E ========================}
 {===========================================================================}
PROCEDURE SetGlobalDefaults;
	VAR
		i: integer;
BEGIN {SetGlobalDefaults}
	XScale := GetMaxX / 639;
	YScale := GetMaxY / 479;
	SStr[1] := 'Waiting';
	SStr[2] := 'Running';
	SStr[3] := 'E D I T';
	SStr[4] := 'Data   ';   {mode status string}
 (******* PC version ***)
	blanc := white;
	noir := black;
	PCVersion := true;
	StatX[1] := 0.783;
	FOR i := 2 TO 10 DO
		StatX[i] := StatX[i - 1] + 0.02;
	StatY[1] := 0.97;
	FOR i := 2 TO 38 DO
		StatY[i] := StatY[i - 1] - 0.025;
	CInc[1] := 1;
	CInc[2] := 10;
	CInc[3] := 100;
   section := 0;
END;  {SetGlobalDefaults}

PROCEDURE MainMenu (InfoScreen: Boolean);
	VAR
		BigMenu: TMenu;
		FrontDisplay: boolean;
		FrontKeys: THotKeys;
		key : byte;
		ReturnFlag: Boolean;

 {===================== M E N U     S T U F F ==============================}

	PROCEDURE SetFrontKeys;
	BEGIN{SetFrontKeys}
		WITH FrontKeys DO
		BEGIN
			Init(2);
			key[1] := 'F1-Help';
			key[2] := 'F10-Menu';
		END;
	END; {SetFrontKeys}

	PROCEDURE HandleBigMenu;
	forward;

	PROCEDURE HandleFrontKeys;
	BEGIN{HandleFrontKeys}
		CASE key OF
			1: ReminderInfo;
			2: IF BigMenu.Chosen THEN HandleBigMenu;
		END; {case}
	END; {HandleFrontKeys}

	PROCEDURE SetBigMenu;
	BEGIN{SetBigMenu}
		BigMenu.Init;
		WITH BigMenu DO
		BEGIN
			column(1, 'File');
			row(1, 1, 'About CUPS    ');
			row(1, 2, 'About Program ');
			row(1, 3, 'Configuration');
			row(1, 4, '----------------------------');
			row(1, 5, 'About Geiger Tube Counting  ');
			row(1, 6, 'About Decay and Growth Rates');
			row(1, 7, '----------------------------');
			row(1, 8, 'Exit Program  ');
			column(2, 'Sections');
			row(2, 1, 'Section 1 - Geiger Tube Counting  ');
			row(2, 2, 'Section 2 - Decay and Growth Rates');
         rowactivate(1,4,false);
         rowactivate(1,7,false);
         ColSize := 11;
		END;
	END;{SetBigMenu}

	PROCEDURE HandleBigMenu;
	BEGIN{HandleBigMenu}
		WITH BigMenu DO
			CASE colChosen OF
				1:
					CASE rowchosen OF
						1: begin AboutCups; SetRGBPalette(green, $00, $18, $7); end;
						2: OpeningScreen;
						3: Configuration;
						5: CountModuleInfo;
						6: ShowFrontInfo;
						8: QuitFlag := TRUE;
					END; {case}
				2: begin
                  section := rowChosen;
				      repeat
					      CASE section OF
						      1: Counting(CountInfo);
						      2: Rates(RatesInfo);
					      END; {case}
				      until QuitFlag;
               end;
			END;{CASE}
	END;{HandleBigMenu}

	PROCEDURE ModuleChoice;
	BEGIN{ModuleChoice}
		SetBigMenu;
		BigMenu.Display;
		SetFrontKeys;
		FrontKeys.Display;
		OpeningScreen;
		ReturnFlag := false;
		FrontDisplay := True;
		REPEAT
			MouseOn;
			CheckForEvents;
			IF NOT FrontDisplay THEN
			BEGIN
				FrontDisplay := true;
				Frontkeys.Display;
				BigMenu.Display;
			END;
			IF FrontKeys.pressed(key) THEN
				HandleFrontKeys;
			IF BigMenu.Activated THEN
				HandleBigMenu;
		UNTIL QuitFlag;
		BigMenu.Done;
	END;{ModuleChoice}

 {==================== MainMenu PROGRAM ============}
BEGIN{MainMenu}
	CupsInit;
	DefineViewport(FullVP, 0.0, 1.0, 0.0, 1.0);  {full screen}
	DefineViewPort(MainVP1, 0.0, 1.0, 0.05, 0.95); {all except top}
	DefineViewport(GraphVP, 0.0, 0.8, 0.0, 0.95); {incl. titles &c}
	DefineViewport(StatusVP, 0.78, 1.0, 0.15, 0.95); {for status box}
	DefineViewport(BotVP, 0.0, 1.0, 0.0, 0.10); {bottom 3 lines}
	DefineViewport(TopVP, 0.0, 1.0, 0.94, 1.0); {top    2 lines}
	DefineViewport(TitleVP, 0.0, 1.0, 0.89, 0.94); {title    line }
	DefineViewport(YAxisVP1, 0.0, 0.11, 0.05, 0.95); {incl. titles &c}
	DefineViewport(XAxisVP1, 0.0, 0.8, 0.05, 0.11); {incl. titles &c}
	DefineViewport(InputVP, 0.0, 1.0, 0.0, 0.45); {Numeric input}
	DefineViewport(ColorPlotVP1, 0.12, 0.75, 0.12, 0.92);
	DefineViewport(ColorReplotVP1, 0.10, 0.76, 0.05, 0.95);
	SetGlobalDefaults;
	ModuleChoice;
	CupsDone;
END;{MainMenu}

BEGIN {Mainline}
	MainMenu(MainInfo);
END.{MainLine}
 {===========================================================================}
 {============  E N D   M A IN   M E N U     M O D U L E ====================}
 {===========================================================================}
