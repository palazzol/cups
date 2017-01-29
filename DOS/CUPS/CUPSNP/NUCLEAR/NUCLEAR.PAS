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
	printer, crt, dos, graph, CupsMupp, CUPS, CUPSproc, Cupsfunc, CUPSgrph,
   CUPSgui, CUPSpars, CupsScrn;

 (*******   DEFINE GLOBAL CONSTANTS, TYPES AND VARIABLES *******)

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

	SelectKeys = ['E', 'e', 'L', 'l', 'P', 'p', F1Key, F10Key];
	ActionKeys = [ArrowR, ArrowL, ArrowUp, ArrowDn, Tab, PgDn,
   HomeKey, EndKey, Ins]; { action in E, L, P, or S mode }

CONST
	Bk = DarkGray;     {Background color for plots.}
	GMClr = lightblue;    { GMT counts bar color }
	IdClr = cyan;         { Ideal counts bar color }
	txt = yellow;
	GraphBoxColor = LightGreen;   {Axes and Legend color.}
	KeyCl = cyan;         {color of keys on menus}
	CCl = lightred;  {color of stuff which changes}
	ACl = lightgray; {lightblue}
  {color of XOR'ed active cursor}
	PCl = lightblue; {brown;}
 {ditto for passive cursor}
	lgy = lightgray;


CONST
	MAXNUM = 1936;
	FullVP = 0;  {Full screen viewport nr}
	ReplotVP = 1;
	PlotVP = 2;  {smaller area for plot}
	BotVP = 3;  {bottom 10%}
	TopVP = 4;  {top 10%}
	TitleVP = 5;  {for title line}
	InputVP = 6;  {for numeric inputs}
	StatusVP = 7;
	MainVP = 8;
	HelpTextVP = 9;
	GraphVP = 10; {incl titles & stuff}
	VPNr = 10; {Number of viewports - maximum of 10}
	vpl = 0.12; {For HelpGraphVP}

TYPE
	Str80 = STRING[80];
	Str30 = STRING[30];

TYPE
	NuclearRec = RECORD
			Z, N: byte;
			mass: longint;
			mode: char;
			life: real;
		END; {record}
	Locator = RECORD
			x, y: integer;
		END; {record}
	Str12 = STRING[12];
	RealArr = ARRAY[1..MaxNum] OF real;
	IntArr = ARRAY[1..MaxNum] OF integer;
	curs = ARRAY[1..4] OF Locator;

 { The data file is organized by increasing Atomic Mass Number A}
{
   for a given A, there are increasing Z's                      }

VAR
	HS: HelpScrType;  { global help screen }
	RScreen: TInputScreen;
	TopMenu: TMenu;
	FrontKeys: THotKeys;

	NDat: ARRAY[1..MaxNum] OF NuclearRec; {By Z's; incr A's}
	Abbr: STRING[210];   {element abbreviations, by 2's}
	AList: ARRAY[1..MaxNum] OF integer; {list of A's incr w/Z}
	NList: ARRAY[1..MaxNum] OF integer; {list of N's incr w/Z}
	ZStartList: ARRAY[1..105] OF integer;  {where Z's start in NDat}
	AStartList: ARRAY[1..262] OF integer; {where A's start in AList}
	AStopList: ARRAY[1..262] OF integer; {where A's stop  in AList}
	NStartList: ARRAY[0..157] OF integer; {where N's start in NList}
	NStopList: ARRAY[0..157] OF integer; {where N's stop in NList}
	VPList: ARRAY[1..VPNr] OF ViewPortType;
	QList: RealArr;
	CurrentVP: integer; {number of currently chosen viewport}
	CInc: ARRAY[1..3] OF integer;

	lf, rt     {plot array limits}	: integer;
	OriginalRight: integer;
	MSel: char;

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

	MouseShowing: boolean; {is mouse cursor showing on screen?}

	Cursor : ARRAY[1..2] OF integer;   {left, right edit cursors}
	ci : integer; {cursor index =1,2}
	Ac : integer; {index of active cursor}
	MouseButton: integer;
	heading: word;

	XScale, YScale: real;
	XPix, Ypix: real;
	StatX: ARRAY[1..10] OF real;
	StatY: ARRAY[1..38] OF real;
	blanc, noir: word;
	KeyFlag: integer; {Hotkey levels}

 {==========================  Utility Routines  =============================}
 {==========================  Utility Routines  =============================}
 {==========================  Utility Routines  =============================}

PROCEDURE OpeningScreen;
	VAR
		i: integer;
BEGIN{OpeningScreen}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[2] := '         Radioactivity Simulation Part I';
	HS[3] := '              Properties of Nuclei';
	HS[6] := ' Mike Moloney, Rose-Hulman Institute of Technology';
	HS[7] := '               Copyright (c) 1994';

	HS[10] := '    Nuclear mass or binding energy per nucleon';
	HS[11] := '       vs. Z, N, or A. ';

	HS[13] := '    Semi-empirical mass formula (SEMF)';
	HS[14] := '       Colors show if the experimental BE/A';
	HS[15] := '       & the formula BE/A agree within limits.';

	HS[17] := '    ln(half-life) vs alpha particle energy';

	HS[19] := '    Nuclides which have the energy to emit alphas';
	HS[20] := '       or betas above a given energy';
	HS[25] := '   Press <Enter> or click mouse to continue.   ';
	Help(HS);
   SetRGBPalette(green, $00, $18, $7);
END;{OpeningScreen}


PROCEDURE ProgramInfo;
	VAR
		i: integer;
BEGIN{ProgramInfo}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[2] := ' This program shows mass and binding energy of ';
	HS[3] := ' known nuclei, and shows the predictions of the';
	HS[4] := ' ''semi-empirical mass formula''.';

	HS[6] := ' It displays half-lives vs alpha energy for';
	HS[7] := ' alpha emitters. The two plots look the same,';
	HS[8] := ' but in one, the user advances the cursor in';
	HS[9] := ' order of increasing Z, and in the other, the';
	HS[10] := ' cursor advances in order of increasing energy.';

	HS[12] := ' It also shows nuclei energeticaly capable of';
	HS[13] := ' alpha and beta decays at or above a given energy.';

	HS[15] := ' One may also study neutron emission using a ';
	HS[16] := ' negative emission energy, to find binding';
	HS[17] := ' energies of particular neutrons. These energies';
	HS[18] := ' tend to be markedly lower after closed shells.';

	HS[20] := ' One may also notice a tendency toward less';
	HS[21] := ' binding energy when the neutron number is odd,';
	HS[22] := ' reflecting the influence of pairing energy';

	HS[25] := '   Press <Enter> or click mouse to continue.   ';
	Help(HS);
   SetRGBPalette(green, $00, $18, $7);
END;{ProgramInfo}

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


procedure Swap(var a,b: real);
var t : real;
begin
   t := a;
   a := b;
   b := t;
end;

procedure iSwap(var a,b: integer);
var t : integer;
begin
   t := a;
   a := b;
   b := t;
end;

PROCEDURE ChooseVP (VPNr: integer); {track current VP}
BEGIN{ChooseVP}
	CurrentVP := VPNr;
	SelectViewPort(VPNr);
END; {ChooseVP}

PROCEDURE MBar (x1, y1, x2, y2: integer; hue: word);
	VAR
		OldVP: integer;
BEGIN {MBar}
 { do Bar on full screen in screen-rel coordinates }
	OldVP := CurrentVP;
	ChooseVP(FullVP);
	SetFillStyle(SolidFill, hue);
	Bar(round(x1 * XScale), round(y1 * YScale),
       round(x2 * XScale), round(y2 * YScale));
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
{		SaveVP := CurrentVP;
		ChooseVP(FullVP);}
      ShowCursor;
		MouseShowing := TRUE;
{		ChooseVP(SaveVP); }
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

PROCEDURE SetHelpGraphVP;
BEGIN{SetHelpGraphVP}
	DefineViewport(ReplotVP, 0.0, 0.76, 0.35, 0.95);
	DefineViewport(PlotVP, vpl, 0.75, 0.4, 0.92);
	PlotVPx := round(vpl * GetMaxX);
	PlotVPy := round(0.08 * GetMaxY);
END; {SetHelpGraphVP}

PROCEDURE DefineGraphArea (vpl: real);
BEGIN{DefineGrArea}
    { vpl is the fraction of GetMaxX where PlotVP starts}
    { can redefine PlotVP on the fly; more flexible than SetGraphVP }
	DefineViewport(ReplotVP, 0.01, 0.78, 0.08, 0.95);
	DefineViewport(PlotVP, vpl, 0.75, 0.15, 0.92);
	PlotVPx := round(vpl * GetMaxX);
	PlotVPy := round(0.08 * GetMaxY);
END; {DefineGrArea}



 {===========================================================================}
 {============== M A S S \ B E    P L O T     M O D U L E =================}
 {===========================================================================}

PROCEDURE MassBEPlot;
	TYPE
		St50 = STRING[50];
	CONST
		MeV_per_amu = 931.4943;
		c11 = 1.073544e-6; {converts MeV/amu x 1000 to amu}
		Mp = 1.0072766;
		MH = 1.007825;
		Mn = 1.008665;
		Ma = 4.001506;  {alpha}
		MT = 3.016049; {tritium}
		Md = 2.014102; {deuteron}
		Me = 5.486e-4;
		RxNr = 14;  {types of nuclear reactions}

		HCl = lightred;   { SEMF too hi color }
		RCl = lightgreen; { SEMF ok color }
		LCl = lightblue;  { SEMF too low color}

		All = 1;
		StableOnly = 2;
		MBP = 1;
		SEP = 2;   {Plot options}
		RxP = 3;
		AlphaP = 4;
		EditKeys = [ArrowR, ArrowL, Tab, PgDn, HomeKey, EndKey];
		New = 1;
		UpDate = 2; {for fit routine info}
		Thr = 6;
		Err = 7; {for real real input}
		Integ = 8;  {for constant integer input}
		C1 = 1;
		C2 = 2;
		C3 = 3;
		C4 = 4;
		C5 = 5; {for semf constants}
		VL = 2;
		Mode = 11;
		CS = 14;
		CK = 18;
		OK = 23;
      Gr = 23;
{		Gr = 31; {lines for status box}
       {New=1;}
		Edit = 2;
		Lin = 3;
		Para = 4;
		Energy = 2;
		ZValue = 1;
		Root = 2;
		ZRoot = 3; {for sorting alpha list}
	VAR
		TopMenu: TMenu;
    {Edit}
		HKeys : THotKeys;
		key: byte;
		RxLabel: STRING[17];
		XLbl: ARRAY[1..6, 1..2] OF STRING[3];
		ReturnFlag: boolean;

		XSel, YSel, CSel, WhatNuc, KNr: integer;
      kNum, reac : array[1..4] of Integer;
		option: integer;
		LastCall	: integer; {last mode when status called}
		HelpIndex : integer; {to which page in help}

		YArr, XArr {Array of reals to be plotted}	: RealArr;
		DatColor  {color of stuff plotted!}      	: IntArr;
		MPList    {master plot list of NDat nrs} 	: IntArr;
		NNam      { name string for nuclide}     	: Str12;
		Z, A, N, i, j, k, m: integer;
		mode1, mode2: integer;
		LastActionKey, ch: char;
		q               { Q of reaction }        	: real;
		Thresh1, Thresh2 { Reaction thresholds } 	: real;
		start, stop: integer;
		Fresh, FirstPass: boolean;
		us, ys, RYs: STRING[3];
		Rw, Cl: integer;
		opt: integer;
		CVol, CSurf, CIso {SEMF constants}       	: real;
		CCoul, CPair: real;
		ErrMar    {SEMF error margin in MeV/amu} 	: real;
		XAxLabel, YAxLabel {axis labels in PlotPrep}	: Str30;
		ScaleX, ScaleY   { fit equation scales }  : real;
		XLow, YLow: integer; {lower corner screen plot coords}
		XHigh, YHigh: integer; {upper corner   " }
		xlo, ylo, xhi, yhi: real;    {min,max values of plot variables}
		ener, sort: integer; {sorting parameter on alpha plot}
		XDNr, YDNr: integer; {# of places to right of decimal on axes}

 {============ S T A R T   O F   P R O C E D U R E S =====================}
 {============ S T A R T   O F   P R O C E D U R E S =====================}

	PROCEDURE ClearAllButTopMenu;
	BEGIN{ClearAllButTopMenu}
		ChooseVP(MainVP);
		LeaveVP;
      ShowMouse;
	END; {ClearAllButTopMenu}

	PROCEDURE DoPlot;
	forward;
	PROCEDURE ExitPara;
	forward;
	PROCEDURE ExitLine;
	forward;
   PROCEDURE PickPlot(option_ : integer);
   forward;

 {================ I N F O ,  H E L P,  T  U T O R I A L S ===============}

	PROCEDURE ShowEditHelp;
		VAR
			i: integer;
	BEGIN{ShowEditHelp}
		FOR i := 1 TO 25 DO
			HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
 {  HS[01]:='Help Screen:  -  Mass-Binding Energy Module  ';}
		HS[2] := 'All plots can be edited and ''fitted''. EDIT mode';
		HS[3] := ' identifies the x & y values of the active cursor.';

		HS[5] := ' Initially, the active cursor is on the lowest ';
		HS[6] := ' point in the data set, and called the ''left''';
		HS[7] := ' cursor. Striking <Tab> switches active cur-';
		HS[8] := ' sors, so the ''right'' cursor becomes active. ';
		HS[9] := ' Cursors are moved by left and right arrow keys,';
		HS[10] := ' or by dragging with the mouse.';

		HS[12] := ' The ''right'' cursor is always right of the ''left''';
		HS[13] := ' cursor in the data set. (Cursors CANNOT CROSS';
		HS[14] := ' in the horizontal direction.)';

		HS[16] := ' ''Zooming In'' replots points between the cursors.';
		HS[17] := ' ''Zooming Out'' replots the full data set.';

		HS[19] := ' LINE mode shows a line on the screen between the';
		HS[20] := ' cursors. Mouse or arrow keys move the cursors.';
		HS[21] := ' This is not a true ''fit''; the user decides if the';
		HS[22] := ' line describes data on the screen. The <End> key';
		HS[23] := ' gives the equation of the line.  PARABOLA mode';
		HS[24] := ' behaves in the same way as LINE, but it has';
		HS[25] := ' three cursors instead of two.';
		Help(HS);
      SetRGBPalette(green, $00, $18, $7);
	END;{ShowEditHelp}

	PROCEDURE MouseEditHelp;
		VAR
			i: integer;
	BEGIN{MouseEditHelp}
		FOR i := 1 TO 25 DO
			HS[i] := ' '; {clear it}
           {12345678901234567890123456789012345678901234567890}
		HS[2] := ' The mouse can be used to ''drag'' either cursor';
		HS[3] := ' to one of the data points. The destination must';
		HS[4] := ' be within 5 pixels of the center of a data point,';
		HS[5] := ' or else the cursor position will not be moved.';

		HS[7] := ' A sound indicates that the cursor has been';
		HS[8] := ' selected, and the original cursor is erased, but';
		HS[9] := ' no visible cursor is shown with the mouse. When';
		HS[10] := ' the mouse is released, the new cursor position ';
		HS[11] := ' will be shown.';

		HS[14] := ' A low sound will indicate that no data point was ';
		HS[15] := ' within 5 pixels of the release point, and the user';
		HS[16] := ' must try ''dragging'' again.';

		HS[18] := ' A cursor can be grabbed from a distance of 30 ';
		HS[19] := ' pixels, so one can move the cursor to nearby';
		HS[20] := ' data points by clicking, and releasing.';

		Help(HS);
      SetRGBPalette(green, $00, $18, $7);
	END;{MouseEditHelp}

	PROCEDURE ShowAlphaInfo;
		VAR
			i: integer;
	BEGIN{ShowAlphaInfo}
		FOR i := 1 TO 25 DO
			HS[i] := ' '; {clear it}
           {12345678901234567890123456789012345678901234567890}
		HS[1] := ' Information/Help Screen:  -  Alpha Decay Graphs ';
		HS[3] := ' Beryllium-8 is an outlier on the alpha graphs. ';
		HS[4] := ' To remove it by ''zooming'' in, set the step size';
		HS[5] := ' to 1 with <PgDn>, press an up arrow or right';
		HS[7] := ' arrow, and press <End>. This will rescale the';
		HS[8] := ' graph without its ''first'' data point, Be-8.';

		HS[10] := ' The alpha emitters involved in ''natural radio-';
		HS[11] := ' activity'' from uranium to lead are shown in ';
		HS[12] := ' light red. All 4 of the decay series are repre-';
		HS[13] := ' sented.';
		Help(HS);
      SetRGBPalette(green, $00, $18, $7);
	END;{ShowAlphaInfo}

	PROCEDURE Tutorial;
		VAR
			i: integer;
			st: Str80;                  {Tutorial}
  { put up graph, and several sets of text }
		CONST
			EdPgs = 4;
			EdLines = 8;
		VAR
			Page: ARRAY[1..EdPgs, 1..EdLines] OF STRING[60];
	BEGIN{Tutorial}
		Page[1, 1] := ' There are two ''edit''cursors on the graph. Try hitting';
		Page[1, 2] := ' the right arrow (or up arrow) key. You should see the ';
		Page[1, 3] := ' cursor on the left move to the right.';
		Page[1, 4] := ' ';
		Page[1, 5] := ' Try hitting the the PgDn key. This changes the cursor';
		Page[1, 6] := ' step sizes between 10, 1, & 100. You should see the';
		Page[1, 7] := ' step size change. Try moving the left cursor at diff-';
		Page[1, 8] := ' erent step sizes.';
		Page[2, 1] := ' The Tab key changes cursors. Try hitting Tab to make';
		Page[2, 2] := ' the right cursor active. Try moving it to the left with';
		Page[2, 3] := ' arrow keys. Move each cursor in from the edge of the   ';
		Page[2, 4] := ' graph. Hit the <End> key.This replots the graph between';
		Page[2, 5] := ' the cursors. Hit the <Home> key. This restores the     ';
		Page[2, 6] := ' original plot.  This is ''zooming'' in & out.          ';
		Page[2, 7] := ' Play around with Tab, PgDn, End, Home, and arrow keys. ';
		Page[2, 8] := ' Note that the panel gives a lot of cursor information. ';
		Page[3, 1] := ' Hit ''L'' to enter LINE mode. The left cursor is active  ';
		Page[3, 2] := ' and can be moved with arrow keys. Try PgDn to change   ';
		Page[3, 3] := ' step size, & then move the cursor again. Tab gets a   ';
		Page[3, 4] := ' new cursor. Move the right cursor with arrow keys. Now ';
		Page[3, 5] := ' hold the mouse button down & put the mouse near one of ';
		Page[3, 6] := ' the cursors. Keep the button down, & when the cursor   ';
		Page[3, 7] := ' jumps onto the mouse arrow, ''drag'' the cursor then let';
		Page[3, 8] := ' the button up. This ''drags'' the line to a new spot. ';
		Page[4, 1] := ' Hit F5 to see the equation of the line. Hitting ''E''';
		Page[4, 2] := ' you back to EDIT mode, while hitting ''P'' takes you to  ';
		Page[4, 3] := ' PARABOLA mode, where you will find three cursors. The  ';
		Page[4, 4] := ' left one is initially active, and Tab will make the   ';
		Page[4, 5] := ' center one active next. Cursor movement & step size are';
		Page[4, 6] := ' like before with arrows and PgDn. As in LINE, F5    ';
		Page[4, 7] := ' gives the equation of the curve. ';
		Page[4, 8] := ' Double-clicking the mouse equals hitting the F5 key.';
		IF HelpIndex = 1 THEN {initial tutorial setup}
		BEGIN
			SelectViewPort(MainVP);
			ClearViewPort;
			DefineViewport(HelpTextVP, 0.02, 0.76, 0.10, 0.35);
			SetHelpGraphVP;
			DoPlot;
		END;

		IF HelpIndex < 5 THEN   {tutorial in progress}
		BEGIN
			ChooseVP(HelpTextVP);
			ClearViewPort;
			IF HelpIndex = 1 THEN
				WriteGr(0.03, 0.33, 'Tutorial Instructions:', white);
			FOR i := 1 TO EdLines DO
			BEGIN
				st := Page[HelpIndex, i];
				WriteGr(0.04, 0.32 - 0.025 * i, st, lightgray);
			END;
			IF HelpIndex < 4 THEN begin
				WriteGr(0.77, 0.22, 'For more tutorial', lightred);
				WriteGr(0.77, 0.19, 'hit <Return>.', lightred);
			end ELSE begin
				WriteGr(0.77, 0.22, 'End of tutorial', lightred);
				WriteGr(0.77, 0.19, 'hit <Return> or', lightred);
         end;
			WriteGr(0.77, 0.16, 'Leave tutorial', lightred);
			WriteGr(0.77, 0.13, 'via F10-Menu.', lightred);
		END
		ELSE {tutorial over, exit.}
		BEGIN
			HelpIndex := 0;
         MSel := 'X';
			ClearAllButTopMenu;
         TopMenu.Display;
         FrontKeys.Display;
		END;
	END; {Tutorial}

 {===================== P H Y S I C S  R O U T I N E S =====================}
 {===================== P H Y S I C S  R O U T I N E S =====================}

	FUNCTION EmpBEA (j: integer): real;
		VAR
			N, Z, A: real;
			EvenN, EvenZ: boolean;
			x, A3, Am2: real;
			m, y: integer;
    { calculates semi-empirical  BE/A  }
	BEGIN{EmpBEA}
		m := NDat[j].N;
		y := NDat[j].Z;
		IF m = 2 * (m DIV 2) THEN
			EvenN := true
		ELSE
			EvenN := false;
		IF y = 2 * (y DIV 2) THEN
			EvenZ := true
		ELSE
			EvenZ := false;
		N := m;
		Z := y;
		A := N + Z;
		A3 := exp(-0.333333 * ln(A));
		Am2 := 1.0 / sqr(A);
		x := CVol + CSurf * A3;
		x := x + CIso * sqr(N - Z) * Am2;
		x := x + CCoul * Z * (Z - 1) * A3 / A;
		IF (EvenN) AND (EvenZ) THEN
			x := x + CPair * Am2;
		IF (NOT EvenN) AND (NOT EvenZ) THEN
			x := x - CPair * Am2;
		EmpBEA := x;
	END; {EmpBEA}

	FUNCTION AMass (j: integer): real; { atomic mass }
		VAR
			n, z: integer;
			x: real;
	BEGIN{AMass}
		n := NDat[j].N;
		z := NDat[j].Z;
		x := NDat[j].mass * 1e-6;
 { writeln('z,n,x',z:4,n:4,x:12:6); ch:=readkey;}
		IF x < 0.5 THEN
			AMass := n + z + x
		ELSE
			AMass := n + z + x - 1.0;
	END; {Amass}

	FUNCTION NMass (j: integer): real; { Matomic - electron masses}
	BEGIN{NMass}
		NMass := AMass(j) - NDat[j].Z * Me;
	END; {NMass}

	FUNCTION BEA (j: integer): real; {BE/nucleon, in MeV}
		VAR
			x: real;
	BEGIN {BEA}
		x := 1.0 / (NDat[j].N + NDat[j].Z);
		BEA := (Mn * NDat[j].N + Mp * NDat[j].Z - NMass(j)) * x * MeV_per_Amu;
	END;  {BEA}

	PROCEDURE NameString (j: integer; VAR name: Str12);
		VAR
			i, z: integer;    { name string for j-th nuclide }
	BEGIN{NameString}
		z := NDat[j].Z;
		name := NumStr(z + NDat[j].N, 3, 0);
		name := Concat(Abbr[2 * z - 1], Abbr[2 * z], name);
	END; {NameString}

	PROCEDURE GetQ (mode: integer; i: integer; VAR Q: real);
 { Gets Q of alpha, beta decays, & other reactions }
 { Q is (initial-final) mass-energy;  IF Q>0 THEN energy released }
		VAR
			zf, nf, f, zi, ni, lo, hi, k, row, col: integer;
			Mf {mass of extra 'final' particle}
			: real;
	BEGIN{GetQ}
   { i for initial state, f for final state }
		Q := -100.0;          {default if no Q found}
		zi := NDat[i].Z;
		ni := NDat[i].N;
		zf := zi;
		nf := ni;
   {6 rows, 6 cols;  decode into row and column}
		row := 1 + (mode - 1) DIV 6;
		col := mode - 6 * (row - 1);
   {mode:=col+(row-1)*6}
		IF row = 1 THEN  {row 1: gamma in}
			CASE col OF
				1: {emit beta-}
				BEGIN
					zf := zi + 1;
					nf := ni - 1;
					Mf := 0;
				END;
				2: {emit beta+}
				BEGIN
					zf := zi - 1;
					nf := ni + 1;
					Mf := 2 * Me;
				END;
				3: {emit p}
				BEGIN
					zf := zi - 1;
					nf := ni;
					Mf := MH;
				END;
				4: {emit n}
				BEGIN
					zf := zi;
					nf := ni - 1;
					Mf := Mn;
				END;
				5: {emit alpha}
				BEGIN
					zf := zi - 2;
					nf := ni - 2;
					Mf := Ma;
				END;
				6: {delayed n}
				BEGIN
					zf := zi + 1;
					nf := ni - 2;
					Mf := Mn;
				END;
			END{case}
		ELSE {row>1}
		BEGIN
			CASE row OF
				2: {proton in}
				BEGIN
					inc(zf);
					Mf := -MH;
				END;
				3: {neutron in}
				BEGIN
					inc(nf);
					Mf := -Mn;
				END;
				4: {deuteron in}
				BEGIN
					inc(zf);
					inc(nf);
					Mf := -Md;
				END;
				5: {tritium  in}
				BEGIN
					inc(zf);
					nf := nf + 2;
					Mf := -MT;
				END;
				6: {alpha in}
				BEGIN
					zf := zf + 2;
					nf := nf + 2;
					Mf := -Ma;
				END;
			END;{case}
			CASE col OF
      {1:  gamma out : no change }
				2: {proton out}
				BEGIN
					dec(zf);
					Mf := Mf + MH;
				END;
				3: {neutron out}
				BEGIN
					dec(nf);
					Mf := Mf + Mn;
				END;
				4: {deuteron out}
				BEGIN
					dec(zf);
					dec(nf);
					Mf := Mf + Md;
				END;
				5: {tritium  out}
				BEGIN
					dec(zf);
					nf := nf - 2;
					Mf := Mf + MT;
				END;
				6: {alpha out}
				BEGIN
					zf := zf - 2;
					nf := nf - 2;
					Mf := Mf + Ma;
				END;
			END;{case}
		END;
		IF (zf > 0) AND (zf <= MaxZNr - 1) THEN
		BEGIN
			lo := ZStartList[zf];
			hi := ZStartList[zf + 1] - 1;
			IF (lo > 0) AND (hi > 0) AND (lo < NuclideNr) AND (hi <= NuclideNr) THEN
				FOR f := lo TO hi DO        {get Q in amu}
					IF NDat[f].N = nf THEN
						Q := Amass(i) - Mf - Amass(f);
		END;
	END; {GetQ}


 {------------------- End of Physics Routines -----------------------------}

 {===================  S  T  A  T  U  S   ==================================}
 {===================  S  T  A  T  U  S   ==================================}

	PROCEDURE ShowStatus (Call: integer);
		VAR
			NewCall: boolean;       {ShowStatus}

		VAR
			KStr: STRING[6];
			i: integer;
			ACur: ARRAY[1..2] OF STRING[6];
			PCur: ARRAY[1..3] OF STRING[6];
			CurInc: ARRAY[1..3] OF STRING[3];

		PROCEDURE AddEdit;

			VAR
				nuclide: Str12;
				XVStr: STRING[4];
				YVStr: STRING[8];
				i, z, n: integer;
		BEGIN
			i := MPList[Cursor[Ac]]; {get true index from Master Plot List}
			NameString(i, Nuclide);
			z := NDat[i].Z;
			n := NDat[i].N;
			IF option = AlphaP THEN
			BEGIN
				XVStr := NumStr(XArr[Cursor[Ac]], 8, 3);
				YVStr := NumStr(YArr[Cursor[Ac]], 8, 3);
			END
			ELSE
			BEGIN
				IF option = RxP THEN
					YVStr := NumStr(QList[i], 8, 3)
				ELSE
					YVStr := NumStr(YArr[Cursor[Ac]], 8, 3);
				XVStr := NumStr(round(XArr[Cursor[Ac]]), 3, 0);
			END;

			SetTextStyle(DefaultFont, HorizDir, 2);
			WriteGr(StatX[2], StatY[VL], Nuclide, CCl);
			WriteGr(StatX[2], StatY[VL + 2], 'Z=', txt);
			WriteGr(StatX[5], StatY[VL + 2], NumStr(z, 3, 0), txt);
			WriteGr(StatX[2], StatY[VL + 4], 'N=', txt);
			WriteGr(StatX[5], StatY[VL + 4], NumStr(n, 3, 0), txt);
			SetTextStyle(DefaultFont, HorizDir, 1);

			WriteGr(StatX[1], StatY[VL + 6], 'x-value', lgy);
			WriteGr(StatX[8], StatY[VL + 6], XVStr, txt);
			IF option = RxP THEN
				WriteGr(StatX[1], StatY[VL + 7], 'Q-value', lgy)
			ELSE
				WriteGr(StatX[1], StatY[VL + 7], 'y-value', lgy);
			WriteGr(StatX[6], StatY[VL + 7], YVStr, txt);

		END; {AddEdit}


		PROCEDURE Generics; {Common Status box stuff}
      var d : integer;
		BEGIN{Generics}
			ClearViewPort;
			WriteGr(StatX[1], StatY[Mode], 'Present Mode is', Heading);

			WriteGr(StatX[1], StatY[CS], 'Cursor Status', Heading);
			WriteGr(StatX[1], StatY[CS + 1], '   Active', lgy);
			WriteGr(StatX[1], StatY[CS + 2], 'Step Size', lgy);

			WriteGr(StatX[1], StatY[Gr], 'Graph Information', Heading);

			IF WhatNuc = All THEN
				WriteGr(StatX[1], StatY[Gr + 1], ' All Nuclides', txt)
			ELSE
				WriteGr(StatX[1], StatY[Gr + 1], ' Stable Nuclides', txt);

         d := round(0.2*GetMaxY);
 {now set up for various graphs, once only}
			CASE option OF
				MBP: {mass-BE plot}
				BEGIN{MBP}
					WriteGr(StatX[1], StatY[Gr + 2], '@ Fixed: ', lgy);
					KStr := NumStr(KNr, 3, 0);
					CASE CSel OF
						1: KStr := 'Z= ' + KStr;
						2: KStr := 'N= ' + KStr;
						3: KStr := 'A= ' + KStr;
						4: KStr := 'none';
					END;{case}
					WriteGr(StatX[7], StatY[Gr + 2], KStr, txt);
					IF WhatNuc = all THEN
					BEGIN
						MBar(523, 409-d, 548, 416-d, CCl);
						WriteGr(StatX[5], StatY[Gr + 3], ' stable', lgy);
						MBar(523, 422-d, 548, 429-d, txt);
						WriteGr(StatX[5], StatY[Gr + 4], ' unstable', lgy);
					END
					ELSE
					BEGIN
						MBar(523, 409-d, 548, 416-d, txt);
						WriteGr(StatX[3], StatY[Gr + 3], ' stable', lgy);
					END;

				END;{MBP}

				SEP: {SEMF}
				BEGIN
					WriteGr(StatX[1], StatY[Gr + 2], 'Err Margin ', lgy);
					WriteGr(StatX[8], StatY[Gr + 2], NumStr(ErrMar, 5, 3), txt);
					WriteGr(StatX[1], StatY[Gr + 3], ' Mass Formula is:', lgy);
					MBar(523, 420-d, 548, 427-d, lightred);
					MBar(523, 433-d, 548, 440-d, lightgreen);
					MBar(523, 446-d, 548, 453-d, lightblue);
					WriteGr(StatX[6], StatY[Gr + 4], 'too high', txt);
					WriteGr(StatX[6], StatY[Gr + 5], ' OK', txt);
					WriteGr(StatX[6], StatY[Gr + 6], 'too low', txt);
				END;{SEP}

				RxP: {alpha, beta decay stuff}
				BEGIN
					WriteGr(StatX[2], StatY[Gr + 2], RxLabel, txt);
					WriteGr(StatX[1], StatY[Gr + 3], 'Min E:', lgy);
					WriteGr(StatX[6], StatY[Gr + 3], NumStr(Thresh1, 6, 2), txt);
					WriteGr(StatX[2], StatY[Gr + 4], 'Above Min E', lgy);
					MBar(610, 420-d, 630, 428-d, CCl);
					MBar(610, 432-d, 630, 440-d, txt);
					WriteGr(StatX[2], StatY[Gr + 5], 'Below Min E', lgy);
				END;{RxP}
				AlphaP: {plot of ln half-life vs alpha E}
				BEGIN
					WriteGr(StatX[1], StatY[Gr + 2], ' Other Nuclides', txt);
					WriteGr(StatX[1], StatY[Gr + 1], ' Decay Chains', CCl);
				END;
			END;{case}
		END;{Generics}

	BEGIN{ShowStatus}
		ACur[1] := 'Left  ';
		ACur[2] := 'Right ';
		PCur[1] := 'Left  ';
		PCur[2] := 'Center';
		PCur[3] := 'Right ';
		CurInc[1] := '  1';
		CurInc[2] := ' 10';
		CurInc[3] := '100';


		SelectViewPort(StatusVP);
		NewCall := FALSE;
		IF Call <> LastCall THEN
		BEGIN
			LastCall := Call;
			NewCall := TRUE;
		END;
		IF Call = New THEN Generics; {write common stuff}
		IF (Call = Edit) {or (Call = New)} THEN AddEdit
      else closeViewPort(20);
		CASE Call OF
			New, Edit: 
				WriteGr(StatX[1], StatY[Mode + 1], '       E D I T ', txt);
			Lin:
				WriteGr(StatX[1], StatY[Mode + 1], '       L I N E ', txt);
			Para: 
				WriteGr(StatX[1], StatY[Mode + 1], 'P A R A B O L A', txt);
		END;{Case}

    {update cursor status}
		IF Call = Para THEN
			WriteGr(StatX[7], StatY[CS + 1], PCur[Ac], txt)
		ELSE
			WriteGr(StatX[7], StatY[CS + 1], ACur[Ac], txt);
		WriteGr(StatX[7], StatY[CS + 2], CurInc[ci], txt);

		SelectViewPort(MainVP);
	END;{ShowStatus}


 {=========== C H E C K   T H E   P L O T   D A T A =======================}

	PROCEDURE PlotListCheck (j: integer;{spot in NDat list}
									stable: boolean;{stable nuclides only?}
									c1, c2: boolean;{need to satisfy q's?}
									rxmode1, rxmode2: integer;{reaction modes}
									th1, th2: real;   {reaction thresholds}
									VAR good: boolean;{ok to add to list?}
									VAR q1: real    {returns 1st q value}
									);

  { checks c1,c2 to satisfy one,two reaction modes above threshold(s) }

		VAR
			q2: real; { q value of 2nd reaction }
			StabilityOK: boolean;

		PROCEDURE QOK (k, mode: integer; thr: real; VAR qnet: real);
		BEGIN{QOK}
     { returns q value of reaction mode, in MeV }
			GetQ(mode, k, qnet);
			qnet := qnet * Mev_per_Amu;
			qnet := qnet - thr;
		END;{QOK}

	BEGIN{PListCk}
		Good := False;
   {  q1:=0.0; }
		IF (j < 0) OR (j > NuclideNr) THEN
		BEGIN
			directvideo := false;
			MoveTo(1, 1);
			write('j= ', j : 8);
			ch := readkey;
		END;

		IF ((NDat[j].life < 0.0) AND (stable)) OR (NOT stable) THEN
			StabilityOK := true
		ELSE
			StabilityOk := false;
		IF StabilityOK THEN { check c1, etc.  }
		BEGIN
			IF c1 THEN {don't do 2nd constraint if 1st = nil}
			BEGIN
				QOK(j, rxmode1, th1, q1);
				IF (q1 > 0.0) AND (NOT c2) THEN
					Good := True;
				IF (q1 > 0.0) AND (c2) THEN
				BEGIN
					QOK(j, rxmode2, th2, q2);
					IF q2 > 0.0 THEN
						Good := True;
				END;
			END
			ELSE
				Good := True; {stable & c1 false; didn't check c2}
		END;
	END;{PListCk}

	FUNCTION NucIndex (k: integer): integer; {Returns j for NDat[j] }
		VAR
			j: integer;
	BEGIN{NucIndex}
		IF option = MBP THEN  { mass or BE plot}
		BEGIN
           {pull j off the correct list}
			CASE CSel OF
				1: 	j := k;           {constant Z}
				2: 	j := NList[k];    {constant N}
				3: 	j := AList[k];    {constant A}
				4: 	j := k;           {nothing}
			END;{CASE}
		END
		ELSE {always from big list}
			j := k;
		NucIndex := j;
	END;{NucIndex}

	PROCEDURE GetXYVar (j, k: integer; VAR RealOut: real);
	BEGIN{GetyXYVar}
  {return Z, N or A desired}
		CASE k OF
			1: RealOut := NDat[j].Z;
			2: RealOut := NDat[j].N;
			3: RealOut := NDat[j].Z + NDat[j].N;
		END;{case}
	END; {GetXYVar}

	PROCEDURE PlotPrep;
		VAR
			i, m: integer;
			tf1, tf2, tf3, ItsOK: boolean;
			q1: real;
			XLabel: ARRAY[1..6] OF STRING[24];
			YLabel: ARRAY[1..4] OF STRING[16];
			XCl: ARRAY[1..4] OF STRING[1];
			YCl: ARRAY[1..4] OF STRING[4];
          { generate MPList, fill arrays; hold color for plots}

		PROCEDURE MBE; {mass-BE plot}
			VAR
				j, k, start, stop: integer;
		BEGIN{MBE}
			XDNr := 0;
			YDNr := 0;
			IF YSel > 1 THEN YDNr := 1;
			YAxLabel := YLabel[YSel];
			EqLbl[0] := YCl[Ysel];
			IF (CSel = 1) AND (KNr > MaxZNr) THEN
			BEGIN
				KNr := MaxZNr - 5;
				Beep;
				Beep;
			END;
			IF (CSel = 2) AND (KNr > MaxNNr) THEN
			BEGIN
				KNr := MaxNNr - 5;
				Beep;
				Beep;
			END;
			IF (CSel = 3) AND (KNr > MaxANr) THEN
			BEGIN
				KNr := MaxANr - 5;
				Beep;
				Beep;
			END;
			IF KNr < 1 THEN
				KNr := 5;
      {get limits from what is constant}
			CASE CSel OF
				1:  { Z constant }
				BEGIN
					start := ZStartList[KNr];
					stop := ZStartList[KNr + 1] - 1;
				END;
				2:  { N constant }
				BEGIN
					start := NStartList[KNr];
					stop := NStopList[KNr];
				END;
				3:  { A constant }
				BEGIN
					start := AStartList[KNr];
					stop := AStopList[KNr];
				END;
				4:  { nothing constant }
				BEGIN
					start := 1;
					stop := NuclideNr;
				END;
			END;{CASE}

			i := 0;
			IF (start > 0) AND (stop > 0) THEN
				FOR k := start TO stop DO {fill arrays}
				BEGIN
					j := NucIndex(k);
					IF (CSel = 4) AND (XSel = 2) THEN
						j := NList[j];
					IF (CSel = 4) AND (XSel = 3) THEN
						j := AList[j];
					PlotListCheck(j, true, false, false, CSel, XSel, thresh1, thresh2, ItsOk, q1);

					IF (WhatNuc = All) OR ItsOk THEN
					BEGIN
						Inc(i);
						MPList[i] := j;
						GetXYVar(j, XSel, Xarr[i]);
						CASE YSel OF
							1:	YArr[i] := AMass(j);
							2:	YArr[i] := BEA(j);
						END; {CASE}
						DatColor[i] := txt;
						IF (WhatNuc = All) AND (ItsOK) THEN
							DatColor[i] := CCl;
					END;
				END;
		END;{MBE}

		PROCEDURE RxPlot; { reaction plots, decay modes}
       { i MUST REMAIN GLOBAL TO PLOTPREP!}
			VAR
				j, m: integer;
				w: real; {reaction plots}
		BEGIN{RxPlot}
			XDNr := 0;
			YDNr := 0;
			IF WhatNuc = StableOnly THEN
				tf1 := true
			ELSE
				tf1 := false;
			i := 0;
			mode2 := 1;  {not checking for mode 2}
			FOR j := 1 TO NuclideNr DO
			BEGIN
				m := j;
      { plot list order depends on x-variable }
				CASE XSel OF
					2: 		m := NList[j];
					3: 		m := AList[j];
				END;{case}
				PlotListCheck(m, tf1, false, false, mode1, mode2, thresh1, thresh2, ItsOk, q1);
				GetQ(mode1, m, w);
				w := w * MeV_per_Amu;
				IF abs(w) > 100 THEN
					w := 99.9;
				QList[m] := w;   {raw Q of 1st reaction}
				IF ItsOK THEN  {passes stability check}
				BEGIN
					inc(i);       {now check for decay}
					PlotListCheck(m, tf1, true, false, mode1, mode2, thresh1, thresh2, ItsOk, q1);
					IF ItsOK THEN
						DatColor[i] := CCl
					ELSE
						DatColor[i] := txt;
					GetXYVar(m, XSel, XArr[i]);
					GetXYVar(m, YSel, YArr[i]);
					MPList[i] := m;
				END;
			END;
		END;{RxPlot}

		PROCEDURE SEMF; {semi-empirical mass formula plot setup}
        {SEMF}
       { i MUST REMAIN GLOBAL TO PLOTPREP - CANT BE LOCAL }
			VAR
				j, m: integer;
				x, y: real;
		BEGIN{SEMF}
			XDNr := 0;
			YDNr := 0;
			IF YSel = 4 THEN
			BEGIN
				YAxLabel := 'BE/A (MeV)';
				YDNr := 1;
			END;
			IF WhatNuc = StableOnly THEN
				tf1 := true
			ELSE
				tf1 := false;
			tf2 := false;
			tf3 := false;
			i := 0;
			FOR j := 1 TO NuclideNr DO
			BEGIN
				m := j; { plot list order depends on x-variable }
				CASE XSel OF
					2: 		m := NList[j];
					3: 		m := AList[j];
				END;{case}
				PlotListCheck(m, tf1, tf2, tf3, mode1, mode2, thresh1, thresh2, ItsOk, q1);
				IF ItsOK THEN {decide colors}
				BEGIN
					x := EmpBEA(m); {SEMF BE/A}
					y := BEA(m);    {actual BE/A}
					inc(i);
					IF YSel < 4 THEN
						GetXYVar(m, YSel, YArr[i])
					ELSE
						YArr[i] := y;
					GetXYVar(m, XSel, XArr[i]);
					DatColor[i] := RCl;
					IF x > y + ErrMar THEN
						DatColor[i] := HCl;
					IF x < y - ErrMar THEN
						DatColor[i] := LCl;
					MPList[i] := m;
				END;
			END;
		END;{SEMF}


		PROCEDURE ShellSort (n: integer; VAR x, y: RealArr; VAR z: IntArr);
   {slight adaptation from Numerical Recipes f/ Pascal, p. 257}
   {sort on x, dragging others along}
			CONST
				aln2i = 1.442695022;
				tiny = 1.0e-5;
			VAR
				lognb2, f, h, i, j, k, l, m, nn: integer;
				t: real;
		BEGIN{ShellSort}
			lognb2 := trunc(ln(n) * aln2i + tiny);
			m := n;
			FOR nn := 1 TO lognb2 DO
			BEGIN
				m := m DIV 2;
				k := n - m;
				FOR j := 1 TO k DO
				BEGIN
					i := j;
					REPEAT
						l := i + m;
						f := 0;
						IF x[l] < x[i] THEN
						BEGIN
							swap(x[i], x[l]);
							swap(y[i], y[l]);
						   iswap(z[i], z[l]);
                     iswap(DatColor[i], DatColor[l]);
							i := i - m;
							f := 1;
						END;
					UNTIL (f = 0) OR (i < 1);
				END;
			END;
		END;{ShellSort}

		PROCEDURE Alpha; { ln of half-life vs. decay energy }
       { i MUST REMAIN GLOBAL TO PLOTPREP - CANT BE LOCAL }
			VAR
				j, k: integer;
				q, w: real;
				ch: char;
				Nuke: Str12;
		BEGIN{Alpha}
			XDNr := 1;
			YDNr := 0; {auto-scale x,y decimals}
			YAxLabel := YLabel[AlphaP];
			k := Ener - ZValue + AlphaP;
			XAxLabel := XLabel[k];
			i := 0;
			EqLbl[0] := YCl[AlphaP];
			FOR j := 1 TO 2 DO
				EqLbl[j] := XLbl[k, j];
			FOR j := 1 TO NuclideNr DO
				IF NDat[j].mode = 'a' THEN
				BEGIN
					GetQ(5, j, q);
					IF q > 0 THEN
					BEGIN
						inc(i);
						YArr[i] := ln(NDat[j].life);
						w := q * MeV_per_Amu;
						XArr[i] := w;
						CASE ener OF
							Root: 	XArr[i] := 1.0 / sqrt(w);
							ZRoot: 	XArr[i] := (NDat[j].Z - 2) / sqrt(w);
						END;{case}
						IF (NDat[j].Z >= 84) AND (NDat[j].Z <= 92) THEN
            {color 4 main decay series}
							DatColor[i] := CCl
						ELSE
							DatColor[i] := txt;
						MPList[i] := j;
					END;
				END;
			IF Sort = Energy THEN
				ShellSort(i, XArr, Yarr, MPList);
		END;{Alpha}

	BEGIN{PlotPrep}
		XLabel[1] := 'Atomic  Number (Z)';
		XLabel[2] := 'Neutron Number (N)';
		XLabel[3] := 'At Mass Number (A)';
		XLabel[4] := 'Alpha Energy(MeV) ';
		XLabel[5] := ' x = E^(-1/2)     ';
		XLabel[6] := ' x = Z E^(-1/2)   ';
		YLabel[1] := 'Mass (amu)     ';
		YLabel[2] := 'BE/A (MeV)     ';
		YLabel[3] := 'BE/A (MeV)     ';
		YLabel[4] := 'ln Half-Life(s)';
		XCl[1] := 'Z';
		XCl[2] := 'N';
		XCl[3] := 'A';
		XCl[4] := 'E';
		YCl[1] := 'Mass';
		YCl[2] := 'BE/A';
		YCl[3] := 'BE/A';
		YCl[4] := 'ln t';
   { put plot labels; MBE, alpha will overwrite some}
		XAxLabel := XLabel[XSel];
		YAxLabel := XLabel[YSel];
		EqLbl[0] := XCl[Ysel];
		FOR m := 1 TO 2 DO
			EqLbl[m] := XLbl[XSel, m];
		CASE option OF
			MBP: MBE;
			RxP: RxPlot;
			SEP: SEMF;
			AlphaP: Alpha;
		END;{CASE}

		lf := 1;
		rt := i;
		OriginalRight := rt;
		Cursor[1] := lf;
		Cursor[2] := rt;
		IF (rt < 2) OR (XArr[lf] = XArr[rt]) THEN	Beep;
	END; {PlotPrep}

	PROCEDURE InitEdit;
	forward;

	PROCEDURE ColorPlot;
		CONST
			d = 1; { 2d+1 x 2d+1 pixel area plotted }
			tr = 5;
			tc = 18; {for data just above plot}
		VAR
			i, j, k, m: integer;
	BEGIN {ColorPlot}
		IF HelpIndex = 0 THEN
			DefineGraphArea(0.12)
		ELSE
			SetHelpGraphVP;
		ChooseVP(RePlotVP);
		MouseOff;
		ClearViewPort;
    {find max & min}
		xlo := XArr[lf];
		xhi := XArr[lf];
		ylo := YArr[lf];
		yhi := YArr[lf];
		FOR i := lf + 1 TO rt DO
		BEGIN
			IF XArr[i] < xlo THEN
				xlo := XArr[i];
			IF XArr[i] > xhi THEN
				xhi := XArr[i];
			IF YArr[i] < ylo THEN
				ylo := YArr[i];
			IF YArr[i] > yhi THEN
				yhi := YArr[i];
		END;

		DefineScale(PlotVP, xlo, xhi, ylo, yhi);
		GraphBackColor := bk;
		SetColor(GraphBoxColor);
		SelectScale(PlotVP);
		OpenViewPort(PlotVP);
		Map(xlo, ylo, XLow, YHigh);     {lower screen corner coords}
		Map(xhi, yhi, XHigh, YLow);     {upper screen corner coords}
		ScaleX := (xhi - xlo) / (XHigh - XLow);
		ScaleY := (yhi - ylo) / (YHigh - YLow);
		Axis(0, -100, 0, 0);
		Setcolor(White);
		PutLabel(Bottom, XAxLabel);
		PutLabel(Left, YAxLabel);

		SetWriteMode(CopyPut);
		FOR i := lf TO rt DO {plot pts as squares}
		BEGIN
			Map(XArr[i], YArr[i], j, k);
			SetColor(DatColor[i]);
			FOR m := j - d TO j + d DO
				line(m, k - d, m, k + d);
		END;
		InitEdit;
		ShowStatus(new);
		MouseOn;
	END; {ColorPlot}

	PROCEDURE DoPlot;
	BEGIN{DoPlot}
		PlotPrep; {which can reset ValidPlot}
		ColorPlot;
	END;{DoPlot}

	PROCEDURE HandleTopMenu;
	forward;

 {======================== E   D   I   T  =================================}

	PROCEDURE PlotCursor (Nr, hue: integer); {Cursor Nr,color}
		VAR
			i, i1, j1, SaveClr: integer; {9x9 pixels XORed over point }
	BEGIN{PlotCursor}
		SelectViewPort(PlotVP); {plot area only}
		SetColor(hue);
		SetWriteMode(XORPut);
		i := Cursor[Nr];  {get cursor's spot on the plot list}
		Map(XArr[i], YArr[i], i1, j1);
		FOR i := i1 - 4 TO i1 + 4 DO
			line(i, j1 - 4, i, j1 + 4);
		FOR i := i1 - 2 TO i1 + 2 DO
			line(i, j1 - 2, i, j1 + 2); {put hole in middle}
		setwritemode(CopyPut);
	END;{PlotCursor}

	PROCEDURE PlotCursor1 (Nr, hue: integer); {Plot Fit Cursor; solid 9x9 }
		CONST
			d = 4; {2d+1 x 2d+1 pixels}
		VAR
			i, i1, j1, s: integer;
	BEGIN{FitCursor}
		SelectViewPort(PlotVP);
		SetWriteMode(XORPut);
		s := GetColor;
		SetColor(hue);
		i1 := Cr[Nr].x;
		j1 := Cr[Nr].y;
		SetLineStyle(SolidLn, 0, NormWidth);
		FOR i := i1 - d TO i1 + d DO
			line(i, j1 - d, i, j1 + d);
		FOR i := i1 - 2 TO i1 + 2 DO
			line(i, j1 - 2, i, j1 + 2); {put hole in middle}
		SetColor(s);
	END;{FitCursor}

	PROCEDURE NewEditCursor (NumCursors: integer);
	BEGIN
		PlotCursor(Ac, ACl); {erase}
		PlotCursor(Ac, PCl);{ new color}
		inc(Ac);
		IF Ac > numCursors THEN	Ac := 1;
		PlotCursor(Ac, PCl); {erase}
		PlotCursor(Ac, ACl); {new color}
	END;

	PROCEDURE SetEditHKeys;
	BEGIN{SetEditHKeys}
		KeyFlag := 1;
		WITH HKeys DO
		BEGIN
			Init(12);
			key[1] := 'F1-Help';
			key[2] := 'F2-MouseHelp';
			key[3] := 'L-Line';
			key[4] := 'P-Parabola';
			key[5] := 'End-ZoomIn';
			key[6] := 'Home-ZoomOut';
			key[7] := 'N-New Data';
         key[8] := 'Tab-NewCursor';
         key[9] := 'PgDn-NewStep';
         key[10] := 'Left-Move Left';
         key[11] := 'Right-Move Right';
         key[12] := 'F10-Menu';
			Display;
		END;
	END; {SetEditHKeys}


	PROCEDURE HandleEditHKeys;
	BEGIN{HandleEditHKeys1}
		CASE key OF
			1: ShowEditHelp;
			2: MouseEditHelp;
			3: Event.Readkey := 'L';
			4: Event.Readkey := 'P';
			5: Event.Readkey := EndKey;
			6: Event.Readkey := HomeKey;
			7: PickPlot(option);
			8: Event.Readkey := Tab;
			9: Event.Readkey := PgDn;
			10: Event.Readkey := ArrowL;
			11: Event.Readkey := ArrowR;
         12: If TopMenu.Chosen then HandleTopMenu;
		END; {case}
      if key in [5,6,8,9,10,11] then Event.ExtendedKey := true;
	END; {HandleEditHKeys1}


	PROCEDURE InitEdit;
	BEGIN{InitEdit}
		ci := 1;
		MSel := 'E';
		PlotCursor(1, ACl);     {left is active cursor}
		PlotCursor(2, PCl);
		Ac := 1;
		SelectViewPort(MainVP);
		SetEditHkeys;          {EditHKeys.display;}
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

		PROCEDURE DragEditCursor;                                  {DragEditCursor}
			CONST
				Dist = 15;
				DCap = 4;
			VAR
				n, i, j, k, Cur, bx, by, obx, oby, xlf, ylf, xrt, yrt: integer;
            min, dis : real;
				x, y: ARRAY[1..2] OF integer;
				Nearby: boolean;

			PROCEDURE PopCursor (j: integer);
			BEGIN{PopCursor}
				IF j = Ac THEN PlotCursor(j, ACl)
				ELSE PlotCursor(j, PCl);
			END; {PopCursor}

		   PROCEDURE PopCursor1 (j: integer);
		   BEGIN{PopCursor}
			   IF j = Ac THEN PlotCursor1(j, ACl)
			   ELSE PlotCursor1(j, PCl);
		   END; {PopCursor}

		BEGIN{DragEditCursor}
			SelectViewPort(PlotVP); {plot area only}
			SelectScale(PlotVP); {plot area only}
         PlotVPX := views[PlotVp].vx1;
			PlotVPY := views[PlotVp].vy1;
			nearby := false;
			bx := GMousX;
			by := GMousY;
			obx := bx - PlotVPX;
			oby := by - PlotVPY;
			i := Cursor[1];
			Map(XArr[i], YArr[i], x[1], y[1]);
			i := Cursor[2];
			Map(XArr[i], YArr[i], x[2], y[2]);
			FOR i := 1 TO 2 DO {mouse near cursor?}
				IF (abs(obx - x[i]) <= dist) AND (abs(oby - y[i]) <= dist)
            AND (MouseButton <> 0) THEN {enter dragging mode}
            begin
               if ac<>i then NewEditCursor(2);
					Cr[i].x := x[i];
					Cr[i].y := y[i];
				   MouseOff;
				   PopCursor1(i); {erase old}
				   MouseOn;
					REPEAT           {while button down}
						IF NOT nearby THEN
						BEGIN
							nearby := true;
							Cur := i;
							MouseOff;
							PopCursor(i);
							MouseOn;
						END;
						MouseGlobalPosn(GMousX, GMousY, MouseButton);
					   bx := GMousX;
					   by := GMousY;
					   bx := bx - PlotVPX;
					   by := by - PlotVPY;
					   MouseOff;
					   PopCursor1(i); {erase old}
					   Cr[i].x := bx;
					   Cr[i].y := by;
					   PopCursor1(i);               {draw new}
					   MouseOn;
					   Delay(25);
					UNTIL MouseButton = 0; {button up}
				   MouseOff;
				   PopCursor1(i); {erase old}
				   MouseOn;
			      IF nearby THEN
			      BEGIN
				      MouseOff;
				      Map(XArr[lf], YArr[lf], xlf, ylf);
				      Map(XArr[rt], YArr[rt], xrt, yrt);
{                 min := 10000;
                  for n := lf to rt do
                  begin
					      Map(XArr[n], YArr[n], j, k);
                     dis := sqrt( sqr((j - bx)/1000) + sqr((k - by)/1000) );
                     if dis<min then min:=dis;
                  end;
				      n := lf - 1;
				      REPEAT
					      inc(n);
					      Map(XArr[n], YArr[n], j, k);
                     dis := sqrt( sqr((j - bx)/1000) + sqr((k - by)/1000) );
				      UNTIL dis=min;}
				      n := lf - 1;
				      REPEAT
					      inc(n);
					      Map(XArr[n], YArr[n], j, k);
				      UNTIL ((abs(j - bx) + abs(k - by)) <= DCap) OR (n = rt);
				      IF (n=rt) or ((Cur = 1) AND (n >= Cursor[2]))
                            OR ((Cur = 2) AND (n <= Cursor[1])) THEN beep
				      ELSE Cursor[Cur] := n;
				      PopCursor(Cur); {new c position}
				      MouseOn;
			      END;
            end;
		END; {DragEditCursor}

   { Ac = active cursor indicator }
	BEGIN {EditAction}

		MouseGlobalPosn(GMousX, GMousY, MouseButton);
		IF MouseButton <> 0 THEN {drag cursor}
		BEGIN
			DragEditCursor;
			Event.Readkey := INS;
		END;
 { find nearest cursor to mouse position }
		CASE event.readkey OF
			ArrowR, ArrowUp: CursorForward;
			ArrowL, ArrowDn: CursorBack;
			Tab: newEditCursor(2);
			PgDn: {dec. cursor increment }
			BEGIN
				dec(ci);
				IF ci = 0 THEN
					ci := 3;
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
				IF (XArr[Cursor[1]] = XArr[Cursor[2]]) THEN
					beep
				ELSE
				BEGIN
					lf := Cursor[1];
					rt := Cursor[2];
					ColorPlot;
				END;
			END;
		END;{CASE}
	END;{EditAction}

	PROCEDURE ExitEdit;
	BEGIN{ExitEdit}
		PlotCursor(Ac, ACl);
		PlotCursor(3 - Ac, PCl);
		HKeys.clear;
	END; {ExitEdit}


 {================= F I T   R O U T I N E   S T U F F ========================}

	PROCEDURE FitCursor (Nr, hue: integer); {Plot Fit Cursor; solid 9x9 }
		CONST
			d = 4; {2d+1 x 2d+1 pixels}
		VAR
			i, i1, j1, s: integer;
	BEGIN{FitCursor}
		SelectViewPort(PlotVP);
		s := GetColor;
		SetColor(hue);
		i1 := Cr[Nr].x;
		j1 := Cr[Nr].y;
		SetLineStyle(SolidLn, 0, NormWidth);
		FOR i := i1 - d TO i1 + d DO
			line(i, j1 - d, i, j1 + d);
		SetColor(s);
	END;{FitCursor}

	PROCEDURE NewCursor (NumCursors: integer);
	BEGIN
		FitCursor(Ac, ACl); {erase}
		FitCursor(Ac, PCl);{ new color}
		inc(Ac);
		IF Ac > numCursors THEN	Ac := 1;
		FitCursor(Ac, PCl); {erase}
		FitCursor(Ac, ACl); {new color}
	END;

	PROCEDURE CrossedCursorTest (cur: integer);
	BEGIN{CrossedCursorTest}
		IF ((MSel = 'P') AND (Cr[2].x >= Cr[3].x)) THEN
    {Cursor 'cur' moved, so move it back}
			IF cur = 3 THEN
				Cr[3].x := Cr[2].x + 1
			ELSE
				Cr[2].x := Cr[3].x - 1;
		IF (Cr[1].x >= Cr[2].x) THEN
			IF cur = 2 THEN
				Cr[2].x := Cr[1].x + 1
			ELSE
				Cr[1].x := Cr[2].x - 1;
	END;{CrossedCursorTest}

	PROCEDURE CursorUp;
	BEGIN {CursorUp}
		FitCursor(Ac, ACl);{erase old}
		Cr[Ac].y := Cr[Ac].y - CInc[ci];
		IF Cr[Ac].y > YHigh THEN Cr[Ac].y := YHigh;
		IF Cr[Ac].y < YLow  THEN Cr[Ac].y := YLow;
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
		IF (Cr[1].x >= Cr[2].x) OR (Cr[2].x >= Cr[3].x) THEN
			Cr[Ac].x := Cr[Ac].x - CInc[ci]; {undo the move}
		IF Cr[Ac].x > XHigh THEN Cr[Ac].x := XHigh;
		IF Cr[Ac].x < XLow  THEN Cr[Ac].x := XLow;
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

	PROCEDURE DragCursor (NrCursors: integer);                       {DragCursor}
		CONST
			dist = 20;
		VAR
			i, bx, by: integer;
		PROCEDURE PopCursor (j: integer);
		BEGIN{PopCursor}
			IF j = Ac THEN FitCursor(j, ACl)
			ELSE FitCursor(j, PCl);
		END; {PopCursor}

	BEGIN{DragCursor}
		bx := GMousX;
		by := GMousY;
		bx := bx - PlotVPX;
		by := by - PlotVPY;
		FOR i := 1 TO NrCursors DO {mouse near cursor?}
			IF (abs(bx - Cr[i].x) <= dist) AND (abs(by - Cr[i].y) <= dist)
         AND (MouseButton <> 0) THEN {enter dragging mode}
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
					CrossedCursorTest(i);
					PopCursor(i);               {draw new}
					MouseOn;
					Delay(25);
				UNTIL MouseButton = 0; {button up}
	END; {DragCursor}

 {============================== L I N E ====================================}

	PROCEDURE SetLineHKeys;
	BEGIN{SetLineHKeys}
		WITH HKeys DO
		BEGIN
			Init(12);
			key[1] := 'F1-Help';
			key[2] := 'F2-MouseHelp';
			key[3] := 'E-Edit';
			key[4] := 'P-Parabola';
			key[5] := 'F5-Equation';
         key[6] := 'Tab-NewCursor';
         key[7] := 'PgDn-NewStep';
         key[8] := 'up-Move Up';
         key[9] := 'Down-Move Down';
         key[10] := 'Left-Move Left';
         key[11] := 'Right-Move Right';
         key[12] := 'F10-Menu';
         BackColor := green;
         Display;
		END;
	END; {SetLineHKeys}

	PROCEDURE HandleLineHKeys;
	BEGIN{HandleLineHKeys}
		CASE key OF
			1: ShowEditHelp;
			2: MouseEditHelp;
			3: Event.Readkey := 'E';
			4: Event.Readkey := 'P';
			5: Event.Readkey := EndKey;
			6: Event.Readkey := tab;
			7: Event.Readkey := PgDn;
			8: Event.Readkey := ArrowUp;
			9: Event.Readkey := ArrowDn;
			10: Event.Readkey := ArrowL;
			11: Event.Readkey := ArrowR;
         12: If TopMenu.Chosen then HandleTopMenu;
		END; {case}
      if (key>4) and (key<12) then Event.ExtendedKey := true;
	END; {HandleLineHKeys}

	PROCEDURE InitLine;
	BEGIN{InitLine}
		ShowStatus(Lin);
		SelectViewPort(PlotVP);
		SetWriteMode(XORPut);
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
		LastActionKey := ArrowR;
		MouseOff;
		Line(Cr[1].x, Cr[1].y, Cr[2].x, Cr[2].y);    {Fresh Line}
		SetLineHKeys;
		MouseOn;
	END; {InitLine}


	PROCEDURE LineEquation; {calc eqn & put on screen}
		VAR
			px1, py1, px2, py2, m, b: real;
			ms, bs, eqs: STRING;
	BEGIN{LineEquation}
		px1 := (Cr[1].x - XLow) * ScaleX + xlo;
		py1 := (YHigh - Cr[1].y) * ScaleY + ylo;
		px2 := (Cr[2].x - XLow) * ScaleX + xlo;
		py2 := (YHigh - Cr[2].y) * ScaleY + ylo;
		IF px2 - px1 = 0 THEN
		BEGIN
			OptStr(px1, ms);
			eqs := Concat('  ', EqLbl[0], ' = ', ms);
		END
		ELSE
		BEGIN
			m := (py2 - py1) / (px2 - px1);
			OptStr(m, ms);
			b := (py1 * px2 - py2 * px1) / (px2 - px1);
			OptStr(b, bs);
			eqs := Concat('  ', EqLbl[0], ' = (', ms, ')', EqLbl[1], ' + (', bs, ')');
		END;
      Announce('Equation is :'+eqs);
	END;{LineEquation}

	PROCEDURE LineAction;  { Ac = active cursor indicator }
	BEGIN{LineAction}
		SelectViewPort(PlotVP);
		SetColor(KeyCl);
		MouseOff; {cursor gone when erasing/writing line}
		Line(Cr[1].x, Cr[1].y, Cr[2].x, Cr[2].y);  {erase the line}
		MouseOn;
		delay(25); {give click time to come up}
		MouseGlobalPosn(GMousX, GMousY, MouseButton);
		IF MouseButton <> 0 THEN {drag cursor}
		BEGIN
			DragCursor(2);
			Event.Readkey := INS;
		END;
		ch := event.ReadKey;
		CASE ch OF
			ArrowR: CursorR;
			ArrowUp: CursorUp;
			ArrowL: CursorL;
			ArrowDn: CursorDn;
			Tab: newCursor(2);
			PgDn: {dec. cursor increment }
			BEGIN
				dec(ci);
				IF ci = 0 THEN
					ci := 3;
			END;
			EndKey: LineEquation;
		END;{CASE}
		MouseOff;          {don't show cursor when line is redrawn}
		SelectViewPort(PlotVP);
		SetColor(KeyCl);
		Line(Cr[1].x, Cr[1].y, Cr[2].x, Cr[2].y);
		MouseOn;
		ShowStatus(lin);
		SelectViewPort(MainVP);
	END; {LineAction}

	PROCEDURE ExitLine; {erase the evidence}
	BEGIN{ExitLine}
		SelectViewPort(PlotVP);
		FitCursor(Ac, ACl);
		FitCursor(3 - Ac, PCl);
		SetColor(KeyCl);
		Line(Cr[1].x, Cr[1].y, Cr[2].x, Cr[2].y);
		SetWriteMode(CopyPut);
    {erase equation of line when leaving}
		WriteGr(0.30, 0.98, '                                                         ', txt);
		HKeys.clear;
		SetLineStyle(SolidLn, 0, NormWidth);
		SelectViewPort(MainVP);
	END; {ExitLine}

 {================= P A R A B O L A    F I T =============================}

	FUNCTION Df (dum: curs; i: integer): extENDed;
		VAR
			r: extENDed;
	BEGIN {Df(i)}
		r := (dum[i].y - dum[i - 1].y) / (dum[i].x - dum[i - 1].x);
		Df := r;
	END;  {Df(i)}

	PROCEDURE Warmup (VAR al, be, albe, al2be, et: extENDed);
	BEGIN{Warmup}
		be := (yhi - ylo) / (YLow - YHigh);
		al := (XHigh - XLow) / (xhi - xlo);
		albe := al * be;
		al2be := al * albe;
		et := (XLow / al) - xlo;
	END;{Warmup}

	PROCEDURE EqParab (dum: curs; VAR a, b, c: real);
		VAR
			f3, f2, f1: extended;
	BEGIN{EqParab}
		f3 := dum[3].x;
		f2 := dum[2].x;
		f1 := dum[1].x;
		a := (Df(dum, 3) - Df(dum, 2)) / (f3 - f1);
		b := Df(dum, 3) - a * (f3 + f2);
		c := dum[1].y - f1 * (b + a * f1);
	END;{EqParab}

	PROCEDURE PlParab (rcl: curs);
		VAR
			a, b, c: real;
			i, j: integer;
	BEGIN{PlParab}
		SelectViewPort(PlotVP);
		EqParab(rcl, a, b, c);
		FOR i := XLow TO XHigh DO
		BEGIN
			j := round(c + i * (b + a * i));
			IF (j >= YLow) AND (j <= YHigh) THEN
				PutPixel ( i , j , GetPixel ( i , j ) xor KeyCl{txt}
				);
		END;
	END;{PlParab}

	PROCEDURE WrEqParab;
		VAR
			a, b, c, wa, wb, wc: real; {abc pure numbers, wa,wb,wc for x vs t }
			al, be, albe, al2be, et, ga, de, ks: extENDed;
			at, bt, ct, eqs: STRING;
	BEGIN{WrEqParab}
		EqParab(Cr, a, b, c);
		Warmup(al, be, albe, al2be, et);
		ks := ylo + (c - YHigh) * be;
		ga := a * al2be;
		de := b * albe;
		wa := ga;
		wb := de + 2 * ga * et;
		wc := ks + de * et + ga * et * et;
		OptStr(wa, at);
		OptStr(wb, bt);
		OptStr(wc, ct);
		eqs := Concat('  ', EqLbl[0], ' =(', at, ')', EqLbl[2], ' + (', bt, ')', EqLbl[1], ' + (', ct, ')');
      announce('Parabola Equation:'+eqs);
	END;{WrEqParab}

	PROCEDURE SetParaHKeys;
	BEGIN{SetLineHKeys}
		WITH HKeys DO
		BEGIN
			Init(12);
			key[1] := 'F1-Help';
			key[2] := 'F2-MouseHelp';
			key[3] := 'E-Edit';
			key[4] := 'L-Line';
			key[5] := 'F5-Equation';
         key[6] := 'Tab-NewCursor';
         key[7] := 'PgDn-NewStep';
         key[8] := 'up-Move Up';
         key[9] := 'Down-Move Down';
         key[10] := 'Left-Move Left';
         key[11] := 'Right-Move Right';
         key[12] := 'F10-Menu';
         BackColor := green;
         Display;
		END;
	END; {SetLineHKeys}

	PROCEDURE HandleParaHKeys;
	BEGIN{HandleLineHKeys}
		CASE key OF
			1: ShowEditHelp;
			2: MouseEditHelp;
			3: Event.Readkey := 'E';
			4: Event.Readkey := 'L';
			5: Event.Readkey := EndKey;
			6: Event.Readkey := tab;
			7: Event.Readkey := PgDn;
			8: Event.Readkey := ArrowUp;
			9: Event.Readkey := ArrowDn;
			10: Event.Readkey := ArrowL;
			11: Event.Readkey := ArrowR;
         12: If TopMenu.Chosen then HandleTopMenu;
		END; {case}
      if (key>4) and (key<12) then Event.ExtendedKey := true;
	END; {HandleLineHKeys}

	PROCEDURE InitPara;
	BEGIN {InitPara}
		ShowStatus(Para);
		SelectViewPort(PlotVP);
		Ac := 1;
		ci := 2;
		Cr[1].x := XLow;
		Cr[1].y := YHigh;
		Cr[3].x := XHigh;
		Cr[3].y := YLow;
		Cr[2].x := (XLow + XHigh) DIV 2;
		Cr[2].y := (YLow + YHigh) DIV 4;
		SetWriteMode(XORPut);
		LastActionKey := ArrowR;
		FitCursor(1, ACl);
		FitCursor(2, PCl);
		FitCursor(3, PCl);
		PlParab(Cr); {initial parab}
		SetParaHKeys;
		SelectViewPort(PlotVP);
	END;  {InitPara}

	PROCEDURE ParaAction;
		VAR
			ch: char;
	BEGIN {ParaAction}
		SelectViewPort(PlotVP);
		SetColor(KeyCl);
		MouseOff;
		PlParab(Cr);
		MouseOn;
		MouseGlobalPosn(GMousX, GMousY, MouseButton);
		IF MouseButton <> 0 THEN {drag cursor}
		BEGIN
			DragCursor(3);
			Event.Readkey := INS;
		END;
		ch := event.ReadKey;
		CASE ch OF
			ArrowR: CursorR;
			ArrowUp: CursorUp;
			ArrowL: CursorL;
			ArrowDn: CursorDn;
			Tab: newCursor(3);
			PgDn: {dec. cursor increment }
			BEGIN
				dec(ci);
				IF ci = 0 THEN	ci := 3;
			END;
			EndKey: WrEqParab;
		END;{CASE}
		MouseOff;
		SetColor(KeyCl);
		PlParab(Cr);
		MouseOn;
	END;{ParaAction}

	PROCEDURE ExitPara;
		VAR
			i, cl: integer; {erase evidence}
	BEGIN{ExitPara}
		SelectViewPort(PlotVP);
		FOR i := 1 TO 3 DO
		BEGIN
			cl := ACl;
			IF i <> Ac THEN
				cl := PCl;
			FitCursor(i, cl);
		END;
		PlParab(Cr);  {erase parab}
		SetWriteMode(CopyPut);
		WriteGr(0.30, 0.98, '                                                         ', txt);  {erase equation}
    {Para}
		HKeys.clear;
		SelectViewPort(MainVP);
	END; {ExitPara}

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
				'P': 
				BEGIN
					ParaAction;
					ShowStatus(para);
				END;
			END;{CASE}
		SelectViewPort(MainVP);
	END; {TakeAction}

	PROCEDURE SelectMode;
		VAR
			ch: char;
	BEGIN{SelectMode}
		ch := upCase(event.readkey);
		IF ch <> MSel THEN      { new selection }
		BEGIN
                     {exit prev selection}
			CASE MSel OF
				'E': 	ExitEdit;
				'L': 	ExitLine;
				'P': 	ExitPara;
			END;{CASE}
			MSel := ch;  {open this selection}
			CASE ch OF
				'E': 
				BEGIN
					InitEdit;
					ShowStatus(New);
				END;
				'L':
			  	BEGIN
					InitLine;
					ShowStatus(Lin);
				END;
				'P': 
				BEGIN
					InitPara;
					ShowStatus(para)
				END;
			END;{CASE}
		END;
	END;{SelectMode}

 {===================== M E N U     S T U F F ==============================}

	PROCEDURE SEMFDefaults;
	BEGIN{SEMFDefaults}
  		ErrMar := 0.01;
		CVol := 15.70;
		CSurf := -17.80;
		CIso := -23.60;
		CCoul := -0.712;
		CPair := 132.0;
	END;{SEMFDefaults}

	PROCEDURE MBESetUp;
		VAR
			OK : boolean;
         screen : TInputScreen;
         ZNA : string;
	BEGIN{MBESetUp}
      ZNA := 'ZNA';
      kNum[4] := 0;
      With Screen do
      begin
         Init;
         DefineInputPort(0.28,0.72,0.22,0.78);
		   LoadLine(' Plot parameters for nuclear mass');
         LoadLine('  or binding energy per nucleon ');
         LoadLine('');
		   LoadLine(' y-axis  : #1Mass  #1BE/nucleon ');
		   LoadLine(' x-axis  : #2Z     #2N     #2A   ');
		   LoadLine(' nuclides: #3All   #3Stable only  ');
		   LoadLine(' keep constant:');
         LoadLine('   #4Z = {      } (1 to 103)');
         LoadLine('   #4N = {      } (1 to 160)');
         LoadLine('   #4A = {      } (1 to 260)');
         LoadLine('   #4nothing ');
         LoadLine('');
		   LoadLine('      [  Ok  ]   [Cancel]');
		   SetRadioButton('1',YSel);
		   SetRadioButton('2',XSel);
		   SetRadioButton('3',WhatNuc);
		   SetRadioButton('4',CSel);
         SetNumber(9 ,kNum[1]); SetNumberLimits(9, 1,103);
         SetNumber(11,kNum[2]); SetNumberLimits(11,1,160);
         SetNumber(13,kNum[3]); SetNumberLimits(13,1,260);
         repeat
            AcceptScreen;
{            ClearInputPort;}
		      YSel := GetRadioButton('1');
		      XSel := GetRadioButton('2');
		      WhatNuc := GetRadioButton('3');
		      CSel := GetRadioButton('4');
            kNum[1] := round(GetNumber(9));
            kNum[2] := round(GetNumber(11));
            kNum[3] := round(GetNumber(13));
            kNr := kNum[cSel];
            OK := (CSel<>XSel);
            if not OK then
             announce('You cannot keep constant '+ZNA[xSel]+
                      ' and plot it on the x-axis');
         until OK;
         Done;
     	   if OKPressed then DoPlot;
      end;
	END; {MBESetUp}

	PROCEDURE SEMFSetUp;
		VAR
			OK : boolean;
         screen : TInputScreen;
         ZNA : string;
	BEGIN{MBESetUp}
      ZNA := 'ZNA';
      With Screen do
      begin
         Init;
         DefineInputPort(0.1,0.9,0.15,0.85);
			LoadLine(' Semi-Empirical Mass Formula (SEMF) Parameters ');
         LoadLine('');
		   LoadLine(' y-axis  : #1Z   #1N   #1A  #1BE/A');
		   LoadLine(' x-axis  : #2Z   #2N   #2A          ');
		   LoadLine(' nuclides: #3All #3Stable only  ');
			LoadLine(' Error margin:                M = {       }MeV (0.001 to 1)');
			LoadLine(' BE/A values within M of the true BE/A will show in green.');
			LoadLine('');
			LoadLine(' Volume term    =                 {       } (10 to 20)');
			LoadLine(' Surface term   =       A^(-1/3) *{       } (-25 to -10)');
			LoadLine(' Isotope effect =    ((N-Z)/A)^2 *{       } (-50 to -10)');
			LoadLine(' Coulomb energy = Z(Z-1)/A^(4/3) *{       } (-2 to -0.1)');
			LoadLine(' Pairing energy term =      A^-2 *{       } (1 to 1000)');
         LoadLine('');
		   LoadLine('            [  Ok  ]   [Cancel]   [Defaults]');
         if (xSel=ySel) then inc(ySel);
		   SetRadioButton('1',YSel);
		   SetRadioButton('2',XSel);
		   SetRadioButton('3',WhatNuc);
         repeat
            SetNumber(10, ErrMar);  SetNumberLimits(10, 0.001,1);
			   SetNumber(11, CVol);    SetNumberLimits(11, 10, 20);
			   SetNumber(12, CSurf);   SetNumberLimits(12, -25, -10);
			   SetNumber(13, CIso);    SetNumberLimits(13, -50, -10);
			   SetNumber(14, CCoul);   SetNumberLimits(14, -2.0, -0.1);
			   SetNumber(15, CPair);   SetNumberLimits(15, 1, 1000);
            AcceptScreen;
		      YSel := GetRadioButton('1');
		      XSel := GetRadioButton('2');
            OK := (YSel<>XSel);
            if not OK then
             announce('You cannot plot '+ZNA[xSel]+' on both axis');
            if GetBoolean(18) then begin
               SEMFDefaults;
               OK := false;
            end;
         until OK;
		   WhatNuc := GetRadioButton('3');
         ErrMar := GetNumber(10);
			CVol   := GetNumber(11);
			CSurf  := GetNumber(12);
			CIso   := GetNumber(13);
			CCoul  := GetNumber(14);
			CPair  := GetNumber(15);
         Done;
     	   if OKPressed then DoPlot;
      end;
	END;


	PROCEDURE DecaySetUp;
		VAR
			OK : boolean;
         screen : TInputScreen;
         Decay, Reacn, ZNA : string[10];
	BEGIN{MBESetUp}
      Decay := '··pn‡ ';
      Reacn := 'ÁpndT‡';
      ZNA   := 'ZNA';
      With Screen do
      begin
         Init;
         DefineInputPort(0.15,0.85,0.15,0.85);
			LoadLine(' Nuclear Decay/Reactions Plot Parameters ');
         LoadLine('');
		   LoadLine(' y-axis  : #1Z     #1N     #1A ');
		   LoadLine(' x-axis  : #2Z     #2N     #2A ');
		   LoadLine(' nuclides: #3All   #3Stable only  ');
			LoadLine(' Particle Minimum Energy = {     }MeV (-10 to 20) ');
			LoadLine(' Process:');
         LoadLine('  #4Decay with emission of:');
			LoadLine('      #5·-  #5·+  #5p   #5n   #5‡   #5late n ');
         LoadLine('  #4Reaction (A,B): ');
			LoadLine('    A(in):        #6p   #6n   #6d   #6T   #6‡');
			LoadLine('    B(out): #7Á   #7p   #7n   #7d   #7T   #7‡');
			LoadLine('');
		   LoadLine('            [  Ok  ]     [Cancel]');
         if (xSel=ySel) then if xSel=3 then dec(ySel) else inc(ySel);
		   SetRadioButton('1',YSel);
		   SetRadioButton('2',XSel);
		   SetRadioButton('3',WhatNuc);
         SetNumber(9 ,Thresh1);  SetNumberLimits(9, -10, 20);
		   SetRadioButton('4',reac[1]);
		   SetRadioButton('5',reac[2]);
		   SetRadioButton('6',reac[3]);
		   SetRadioButton('7',reac[4]);
         repeat
            AcceptScreen;
{            ClearInputPort;}
		      YSel := GetRadioButton('1');
		      XSel := GetRadioButton('2');
            OK := (YSel<>XSel);
            if not OK then
             announce('You cannot plot '+ZNA[xSel]+' on both axis');
         until OK;
		   WhatNuc := GetRadioButton('3');
   		Thresh1 := GetNumber(9);
		   reac[1] := GetRadioButton('4');
		   reac[2] := GetRadioButton('5');
		   reac[3] := GetRadioButton('6');
		   reac[4] := GetRadioButton('7');
         if reac[1]=1 then mode1:=reac[2]
         else mode1:=reac[3]*6+ reac[4] + 7;
         case reac[1] of
          1: begin
                RxLabel := Decay[reac[2]];
                case reac[2] of
                 1: RxLabel := RxLabel+'+';
                 2: RxLabel := RxLabel+'-';
                 6: RxLabel := 'late n';
                end;
                RxLabel := RxLabel+' emition'
             end;
          2: RxLabel := '('+reacn[reac[3]+1]+','+reacn[reac[4]]+') reaction';
         end;
         Done;
     	   if OKPressed then DoPlot;
      end;
	END;

	PROCEDURE HalfLifeSetUp;
		VAR
			OK : boolean;
         screen : TInputScreen;
	BEGIN
      With Screen do
      begin
         Init;
         DefineInputPort(0.15,0.85,0.35,0.65);
			LoadLine(' ln(half-life) vs. alpha particle energy');
         LoadLine('');
		   LoadLine(' y-axis   : ln(half-life) ');
		   LoadLine(' x-axis   : #1E        #1E^(-1/2)  #1Z*E^(-1/2) ');
			LoadLine(' sorted by: #2Z value  #2energy');
			LoadLine('');
		   LoadLine('           [  Ok  ]     [Cancel]');
		   SetRadioButton('1',ener);
		   SetRadioButton('2',sort);
         AcceptScreen;
{         ClearInputPort;}
         ener := GetRadioButton('1');
	      sort := GetRadioButton('2');
         Done;
     	   if OKPressed then DoPlot;
      end;
	END;

	PROCEDURE SetTopMenu;
		VAR
			i: integer;
	BEGIN{SetTopMenu}
		MSel := 'X';
		TopMenu.Init;
		WITH TopMenu DO
		BEGIN
			column(1, 'File');
			row(1, 1, 'About CUPS    ');
			row(1, 2, 'About Program ');
			row(1, 3, 'Configuration');
			row(1, 4, '---------------------');
			row(1, 5, 'More About Program');
			row(1, 6, 'Some Reminders');
			row(1, 7, 'Tutorial Walk-Through');
			row(1, 8, '---------------------');
			row(1, 9, 'Exit Program');
			column(2, 'Pick Plot');
			row(2, 1, 'Experimental Mass or Binding Energy ');
			row(2, 2, 'Semi-Empirical Mass Formula');
			row(2, 3, 'Nuclear Decays/Reactions');
			row(2, 4, 'Alpha Decay: Lifetime vs. Energy');
			AutoChecking(2, 1);
			RowActivate(1, 4, FALSE);
			RowActivate(1, 8, FALSE);
		END;
	END;{SetTopMenu}

   PROCEDURE PickPlot(option_ : integer);
	BEGIN
		option := option_;
		CASE option OF
			1: MBESetUp;
			2: SEMFSetUp;
			3: DecaySetUp;
			4: HalfLifeSetUp;
		END;{case}
	END;

	PROCEDURE HandleTopMenu;
		VAR
			i: integer;
	BEGIN{HandleTopMenu}
      SetColor(white);
		IF (HelpIndex<>0) and (TopMenu.ColChosen > 1) THEN
		BEGIN
			HelpIndex := 0;
			DefineGraphArea(0.12);
		END;
		WITH TopMenu DO
			CASE colChosen OF
				1: { Files }
					CASE rowchosen OF
						1: begin AboutCups; SetRGBPalette(green, $00, $18, $7); end;
						2: OpeningScreen;
						3: Configuration;
						5: ProgramInfo;
						6: ReminderInfo;
						7:	BEGIN
							   HelpIndex := 1;
   							Tutorial;
	   					END;
						9: ReturnFlag := TRUE;
					END; {case}
				2:  PickPlot( rowchosen);
			END;{CASE}
	END; {HandleTopMenu}

	PROCEDURE SetFrontKeys;
	BEGIN{SetFrontKeys}
		WITH FrontKeys DO
		BEGIN
			Init(2);
			key[1] := 'F1-Help';
			key[2] := 'F10-Menu';
         Display;
		END;
	END; {SetFrontKeys}

	PROCEDURE HandleFrontKeys;
	BEGIN{HandleFrontKeys}
		CASE key OF
			1: ReminderInfo;
			2: IF TopMenu.Chosen THEN
					HandleTopMenu;
		END; {case}
	END; {HandleFrontKeys}


	PROCEDURE SetDefaults;
		CONST
			NextLbl = 'More ' + ScRtA;
			LastLbl = ScLfA + ' More';

		VAR
			i: integer;
	BEGIN {SetDefaults}
		XSel := 1;
		YSel := 1;
		CSel := 4;
		WhatNuc := all;
		KNr := 123;
      for i := 1 to 4 do kNum[i] := 1;
      for i := 1 to 4 do reac[i] := 1;
		option := 1;
		lf := 1;
		rt := 1;
		ci := 1;
		mode1 := 2;
		Ac := 1; {active cursor}
		MSel := 'X';
		FirstPass := TRUE;
		ReturnFlag := FALSE;

		SEMFDefaults;
		HelpIndex := 0;
		Sort := ZValue;
      ener := zValue;
		Thresh1 := 0.0;

		XLbl[1, 1] := ' Z ';   		XLbl[1, 2] := 'Z^2';
		XLbl[2, 1] := ' N ';   		XLbl[2, 2] := 'N^2';
		XLbl[3, 1] := ' Z ';   		XLbl[3, 2] := 'Z^2';
		XLbl[4, 1] := ' E ';   		XLbl[4, 2] := 'E^2';
		XLbl[5, 1] := ' x ';   		XLbl[5, 2] := 'x^2';
		XLbl[6, 1] := ' x ';   		XLbl[6, 2] := 'x^2';
		CInc[1] := 1;
		CInc[2] := 10;
		CInc[3] := 100;

		XScale := GetMaxX / 639;
		Xpix := 1.0 / GetMaxX; {one x-pixel as a fraction of x-screen}
		YScale := GetMaxY / 479;
		YPix := 1.0 / GetMaxY; {one y-pixel as a fraction of y-screen}

		StatX[1] := 0.783;
		FOR i := 2 TO 10 DO
			StatX[i] := StatX[i - 1] + 0.02;
		StatY[1] := 0.97;
		FOR i := 2 TO 38 DO
			StatY[i] := StatY[i - 1] - 0.025;

		ChooseVP(FullVP);
		ClearViewPort;
		SetTopMenu;
		MouseShowing := true;
		TopMenu.Display;
		SetFrontKeys;
		blanc := white;
		noir := black;
		heading := blanc;
	END; {SetDefaults}

 {=============== MAIN MBE MODULE PROGRAM ==================}
BEGIN{MassBEPlot}
	SetDefaults;
	OpeningScreen;
	REPEAT
		MouseOn;
		CheckForEvents;
		IF Event.DoubleClicked THEN
			Event.Readkey := EndKey;
		MouseGlobalPosn(GMousX, GMousY, MouseButton);
		IF (MSel = 'E') AND (HKeys.pressed(key)) THEN HandleEditHkeys;
		IF (MSel = 'L') AND (HKeys.pressed(key)) THEN HandleLineHkeys;
		IF (MSel = 'P') AND (HKeys.pressed(key)) THEN HandleParaHkeys;
		IF (MSel = 'X') AND (FrontKeys.pressed(key)) THEN HandleFrontKeys;
		IF TopMenu.Activated THEN HandleTopMenu;
      IF (Event.ReadKey IN SelectKeys) AND (NOT Event.ExtendedKey) THEN
	     SelectMode;
		IF ((Event.ReadKey IN ActionKeys) OR (MouseButton <> 0)) THEN
        TakeAction;
		IF (HelpIndex > 0) AND (Event.Readkey = RET) THEN
		BEGIN
			Inc(HelpIndex);
			Tutorial;
		END;
	UNTIL ReturnFlag;
	LeaveVP;
	TopMenu.Done;
END;{MassBEPlot}
 {===========================================================================}
 {=========  E N D   M A S S  \ B E    P L O T    M O D U L E ===============}
 {===========================================================================}

PROCEDURE LoadFiles;
forward;
 {===========================================================================}
 {=======================   M A I N     P R O G R A M    ====================}
 {===========================================================================}
PROCEDURE MainPgm;
 {==================== Main PROGRAM ============}
BEGIN{MainPgm}
	LoadFiles;
	CupsInit;
	DefineViewport(FullVP, 0.0, 1.0, 0.0, 1.0);  {full screen}
	DefineViewPort(MainVP, 0.0, 1.0, 0.0, 0.95); {all except top}
	DefineViewport(GraphVP, 0, 0.8, 0, 0.95); {incl. titles &c}
	DefineViewport(StatusVP, 0.78, 1.0, 0.1, 0.95); {for status box}
	DefineViewport(BotVP, 0.0, 1.0, 0.0, 0.10); {bottom 3 lines}
	DefineViewport(TopVP, 0.0, 1.0, 0.94, 1.0); {top    2 lines}
	DefineViewport(TitleVP, 0.0, 1.0, 0.89, 0.94); {title    line }
   DefineViewPort(20, 0.78, 1.0, 0.75, 0.95);
	MassBEPlot;
	CupsDone;
END;{MainPgm}
 {===========================================================================}
 {=================  E N D     M A I N     P R O G R A M  ==================}
 {===========================================================================}

PROCEDURE LoadFiles;  { read in nuclear data }
	VAR
		i, j, k, m: integer;
		f: text;
		s: STRING[2];
		fname: str80;
		ch: char;
BEGIN{LoadFiles}
	clrscr;
	gotoxy(1, 1);
	writeln(' Reading in nuclear data.');
	writeln(' This will take around 30 sec from a floppy.');
	fname := 'Nuclear.dat';
	assign(f, fname);
	{$I-} reset(f); {$I+}  { for reading }
   If IOResult<>0 then
    Error('File NUCLEAR.DAT not found in current directory');

	readln(f, NuclideNr);
	FOR j := 1 TO NuclideNr DO
	BEGIN
		readln(f, NDat[j].mode, NDat[j].N, NDat[j].Z, NDat[j].mass, NDat[j].life);
		IF NDat[j].life < -200.0 THEN  {error occurred}
		BEGIN
			writeln(NDat[j].N : 4, NDat[j].Z : 4, Ndat[j].mass : 8, NDat[j].life : 24 : 3);
			ch := readkey;
		END;
	END;
	close(f);
	writeln(' Nuclear data read in');

	fname := 'nucelem.dat';
	assign(f, fname);
	{$I-} reset(f); {$I+}  { for reading }
   If IOResult<>0 then
    Error('File NucElem.Dat not found in current directory');
	readln(f, MaxZNr);
	FOR j := 1 TO MaxZNr DO
	BEGIN
		readln(f, s);
		Abbr[2 * j - 1] := s[1];
		Abbr[2 * j] := s[2];
	END;
	close(f);
	writeln(' Element abbreviations read in');

	fname := 'NucLists.dat';
	assign(f, fname);
	reset(f);   { for reading }
	{$I-} reset(f); {$I+}  { for reading }
   If IOResult<>0 then
    Error('File NucLists.Dat not found in current directory');
	readln(f, k);
	FOR j := 1 TO k DO
		readln(f, ZStartList[j]);

	readln(f, MaxANr);
	readln(f, m);

	FOR j := 1 TO MaxANr DO
	BEGIN
		readln(f, i, k);
		AStartList[j] := i;
		AStopList[j] := k;
	END;
	FOR j := 1 TO m DO
		readln(f, AList[j]);
	readln(f, MaxNNr);
	readln(f, m);

	FOR j := 0 TO MaxNNr DO
	BEGIN
		readln(f, i, k);
		NStartList[j] := i;
		NStopList[j] := k;
	END;
	FOR j := 1 TO m DO
		readln(f, NList[j]);

	close(f);
	writeln(' Nuclear list data read in from file.');
END;{LoadFiles}

BEGIN{Nominal Mainline}
	MainPgm;
END.{Nominal Mainline}