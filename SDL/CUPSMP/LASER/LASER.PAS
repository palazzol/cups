           (*************************************************)
           (*************************************************)
           (**                                             **)
           (**  Consortium of Upper-level Physics Software **)
           (**              ( CUPS Project )               **)
           (**      ( c) 1994 by John Wiley & Sons         **)
           (**  Compiled with Utilities Ver. 1.9 (95/09/10)**)
           (**                                             **)
           (*************************************************)
           (*************************************************)

 {$A-}
 {$N+}
 {$R+}
 {$M 65520,0,655360}

USES
	printer, crt, dos, graph, CupsMupp, CUPS, CUPSproc, Cupsfunc, CUPSgrph, CUPSgui, CUPSpars;

 (*******   DEFINE GLOBAL CONSTANTS, TYPES AND VARIABLES *******)

TYPE
	Str80 = STRING[80];
	Str30 = STRING[30]; {for y-axis title, done vertically}

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
	AltA = chr(30);
	AltS = chr(31);
	AltD = chr(32);
	AltF = chr(33);
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

	SelectKeys  = ['E', 'e', 'L', 'l', 'P', 'p', F1Key, F10Key];
	ActionKeys  = [ArrowR, ArrowL, ArrowUp, ArrowDn, PgUp, PgDn, HomeKey, EndKey, Ins, Del]; { action in E, L, P, or S mode }
	BarMenuKeys = [ArrowR, ArrowL, ArrowUp, ArrowDn, PgUp, PgDn, HomeKey, EndKey, RET];
CONST
	Bk:	INTEGER = DarkGray;     {Background color for plots.}
	GMClr:	integer = lightblue;    { GMT counts bar color }
	IdClr:	integer = cyan;         { Ideal counts bar color }
	txt:  	integer = yellow;
	KCl = cyan;    {color of keys on menus}
	CCl = lightred;  {color of stuff which changes}
	ACl = lightgray; {lightblue}
  {color of XOR'ed active cursor}
	PCl = lightblue; {brown;}
 {ditto for passive cursor}
	ExitFlg:	BOOLEAN = FALSE;
	MainInfo:	BOOLEAN = TRUE;
	MAXNUM = 500;
	FullVP = 0;  {Full screen viewport nr}
	ColorReplotVP = 1;
	ColorPlotVP = 2;  {smaller area for plot}
	YAxisVP = 3;  {bottom 10%}
	XAxisVP = 4;  {top 10%}
	MirrReplotVP = 5;  {mirror graphs}
	MirrPlotVP = 6;
	InputVP = 7;  {for numeric inputs}
	MainVP = 8;
 {       HelpTextVP     = 9;}
	GraphVP = 10; {incl titles & stuff}
	VPNr = 10; {Number of viewports - maximum of 10}
	vpl = 0.08; {left edge of plotvp; adjust for ticks}

	lgy = lightgray;
	Ntot = 400.0;
	e = 0.05;
	XLeft = 20.0; {for mirror stuff}
	rand = 1;
	select = 2;
	LnTen = 2.302581;
	top = 1;
	left = 2;
	bottom = 3;
	right = 5;

	YGC = 231; {FullVP centerline for ray plots}
	YC = 130; {ColorPlotVP centerline for ray plots}

TYPE
	Locator = RECORD
			x, y: integer;
		END; {record}
	Str12 = STRING[12];
	RealArr = ARRAY[1..MaxNum] OF real;
	IntArr = ARRAY[1..MaxNum] OF integer;
	curs = ARRAY[1..4] OF Locator;

VAR
	RScreen: TInputScreen;
	sliders: TSliders;
	HS: HelpScrType;  { global help screen }
	Hotkeys: THotkeys;
	FrontKeys: THotkeys;    {keys for opening screen}
	TopMenu: TMenu;
	key: byte;
	StatX: ARRAY[1..10] OF real;
	StatY: ARRAY[1..40] OF real;

	CurrentVP: integer; {number of currently chosen viewport}
	XArr, Y1Arr, Y2Arr: RealArr;
	DatColor: IntArr;
	lf, rt: integer;  { limits of plot array }
	OriginalRight: integer;

	ch: char;
	inside: boolean; {inside the viewport?}
	Rx, Ry: real;    {x,y real mouse position in in VPort}
	GMousX, GMousY: integer; {x,y integers; mouse global position}
	ColorPlotVPx, ColorPlotVPy: integer;
	MirrPlotVPx, MirrPlotVPy: integer; {x,y starting coords of Plot VP }
	XAxLabel, YAxLabel: Str80;

	W, PumpVal, TempVal, A10Val: extended;
	A21Val, A20Val: extended;
	Trans: real;
	n2, n1, n0, delta: extended;
	E10Val, E21Val, E20Val: extended; { in terms of Emn/kT at room temp }
	st: STRING[20];
	lase21: boolean; {lasing between 2 and 1 ?}
	ValidPlot: boolean; {is there a valid plot on the screeen?}
	RePlotFlag: boolean; {should module do a re-plot?}
	MouseShowing: boolean; {is mouse cursor showing on screen?}
	StatusShowing: boolean; {is status area showing on screen?}
	ReturnFlag: boolean; { exit the program? }
	TopDisplay: boolean; { top menu showing? }
	TopCall: boolean; { delayed call to top menu?}
	MoreRays: boolean; {return to rays fm beamwidth?}
	MouseButton: integer;
	RayMinAng, RayMaxAng: real;
	RayInitAng: real;
	XInit, YMag: real;
	CInc: ARRAY[1..3] OF integer;

	CLeft, CRight {center of curvature}  	: real;
	RLeft, RRight {radius of curvature}  	: real;
	MirrDist     {distance betw. mirrors}	: real;
	NrBounces    {max pairs of bounces}  	: integer;
	VMin, VMax, HMin, HMax: real;
	PlotFlag, Flows: boolean;
	XScale, YScale, XPix, YPix: real;

	blanc, noir, heading: word;
	GraphBoxColor, AxisLabelColor: word;
	PCVersion: boolean;

 {==========================  Utility Routines  =============================}
 {==========================  Utility Routines  =============================}
Function MouseInVP(n : integer; x,y : integer) : boolean;
BEGIN
   with Views[n] do
   MouseInVP := (x>=vx1) and (x<=vx2) and (y>=vy1) and (y<=vy2);
END;    {ClearVP}

PROCEDURE ClearVP;
BEGIN
   HideMouse;
	ClearViewPort;
   ShowMouse;
END;    {ClearVP}

PROCEDURE ChooseVP (VPNr: integer); {track current VP}
BEGIN{ChooseVP}
	CurrentVP := VPNr;
	SelectViewPort(VPNr);
END; {ChooseVP}

PROCEDURE DLay (amount: integer);
BEGIN{DLay}
	delay(amount);
END; {DLay}

PROCEDURE MBar (x1, y1, x2, y2: integer; hue: word);
	VAR
		OldVP: integer;
BEGIN {MBar}
 { do Bar on full screen in screen-rel coordinates }
	SetFillStyle(SolidFill, hue);
	OldVP := CurrentVP;
	ChooseVP(FullVP);
   HideMouse;
	Bar(round(x1 * XScale), round(y1 * YScale), round(x2 * XScale), round(y2 * YScale));
   ShowMouse;
	ChooseVP(OldVP);
END; {MBar}

PROCEDURE YTitle (st: str80);
	VAR
		SaveVP, x, y: integer;
BEGIN {YTitle}
 { do Bar on full screen in screen-rel coordinates }
	SaveVP := CurrentVP;
	ChooseVP(FullVP);
	x := round(XScale * 12);
	y := round(YScale * (230 - 4 * length(st)));
	SetTextStyle(DefaultFont, VertDir, 1);
	Outtextxy(x, y, st);
	SetTextStyle(DefaultFont, HorizDir, 1);
	ChooseVP(SaveVP);
END; {YTitle}

FUNCTION dg (strg: Str80): STRING;
BEGIN { add degrees to a string }
	dg := concat(strg, chr(248));
END; {dg}


PROCEDURE CheckEvents;
BEGIN{CheckEvents}
	CheckForEvents;
	IF TopMenu.Activated THEN
		TopCall := true;
END; {CheckEvents}

PROCEDURE WriteGr (XScreen, YScreen: real; msg: Str80; hue: integer);
	VAR
		SaveVP: integer;
  {write to x,y 0..1 in screen-relative coords}
BEGIN {WriteGr}
	SaveVP := CurrentVP;
	ChooseVP(FullVP);
	RuboutXY(XScreen, YScreen, length(msg), noir);
	setcolor(hue);
	PrintXY(XScreen, YScreen, msg);
	ChooseVP(SaveVP);
END;{WriteGr}

PROCEDURE DrawArrow (x1, y1, x2, y2: integer; hue: word);
  { draw arrow from x1,y1 to x2,y2 }
  { initially for arrows longer than 5 units. }
  { Come back 5 from tip and go left and right 2 each. }
  { Then draw lines from there to tip. }
  { line is dx,dy.     perpendicular to line is dy,-dx. }

	VAR
		dx, dy, l, ha, hb, hc, hx, hy, hx1, hx2, hy1, hy2: real;
	VAR
		OldVP: integer;
		SaveColor: word;
 { do arrow on full screen in screen-rel coordinates }
BEGIN{DrawArrow}
	SaveColor := GetColor;
	SetColor(hue);
	OldVP := CurrentVP;
	ChooseVP(FullVP);
	dx := x2 - x1;
	dy := y2 - y1;
	l := sqrt(dx * dx + dy * dy); { get line length }
   HideMouse;
	line(x1, y1, x2, y2);                          { draw the line}
	IF l > 6 THEN {if length>6 draw tip of arrow}
	BEGIN
		ha := 5.0 / l;
		hx := x2 - ha * dx;
		hy := y2 - ha * dy;            { back 5 from tip}
		ha := 2.0 / l;
		hb := ha * dy;
		hc := -ha * dx;
		hx1 := hx - hb;
		hy1 := hy - hc;              { 2 to each side }
		hx2 := hx + hb;
		hy2 := hy + hc;
		line(round(hx1), round(hy1), x2, y2);
		line(round(hx2), round(hy2), x2, y2);
	END;
   ShowMouse;
	ChooseVP(OldVP);
	SetColor(SaveColor);
END; {DrawArrow}

 {==============  S T A R T   G L O B A L   P R O C E D U R E S ============}
 {==============  S T A R T   G L O B A L   P R O C E D U R E S ============}
 {==============  S T A R T   G L O B A L   P R O C E D U R E S ============}

PROCEDURE OpeningScreen;
	VAR
		i: integer;
BEGIN{OpeningScreen}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[2] := '              Laser Simulation';
	HS[4] := '           Written by Mike Moloney';
	HS[5] := '      Rose-Hulman Institute of Technology';
	HS[6] := '             Copyright (c) 1994                   ';
	HS[8] := ' The atoms in this laser are supposed to contain';
	HS[9] := ' three levels. The model begins at thermal equil-';
	HS[10] := ' ibrium when no pump power is applied. The user';
	HS[11] := ' may observe the transition from thermal equil-';
	HS[12] := ' ibrium to population inversion as pump power is';
	HS[13] := ' increased.';
	HS[15] := ' The user may change energy level locations, ';
	HS[16] := ' Einstein coefficients, pump power, and temp-';
	HS[17] := ' erature.';
	HS[19] := ' Cavity stability may be examined by changing its';
	HS[20] := ' shape and passing rays back and forth between';
	HS[21] := ' mirrors. For stable cavities, gaussian beam ';
	HS[22] := ' parameters may be studied.';
	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
END;{OpeningScreen}

PROCEDURE Announcement;
	VAR
		i: integer;
BEGIN{Announcement}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[7] := ' The plot you requested cannot be made, or would';
	HS[8] := ' provide no information if it were done.';
	HS[12] := ' Please recheck the lasing conditions before';
	HS[13] := ' requesting another plot.';
	HS[23] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
END;{Announcement}

PROCEDURE AboutPlots;
	VAR
		i: integer;
BEGIN{AboutPlots}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[2] := ' Plots of population or coherent intensity vs. ';
	HS[3] := ' pump power begin with the value of pump power';
	HS[4] := ' selected in the lasing scheme, and end with a';
	HS[5] := ' pump power 10 times as great. These plots are';
	HS[6] := ' all done with no mirror transmission.';
	HS[9] := ' Plots of population or coherent intensity vs.';
	HS[10] := ' mirror transmission are done with the selected';
	HS[11] := ' value of pump power over a range of 0.0 to 0.10';
	HS[14] := ' If laser coherent intensity has not been set up';
	HS[15] := ' in the lasing scheme, the last two plots will';
	HS[16] := ' only beep.';
	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
END;{AboutPlots}

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
	HS[25] := '   Press any key or click mouse to continue...   ';
	Help(HS);
END;{SliderUseInfo}

PROCEDURE FlowRateInfo;
	VAR
		i: integer;
BEGIN{FlowRateInfo}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[2] := '      Flow Rates Between Atomic Energy Levels ';
	HS[4] := ' There are two screens which show the flow';
	HS[5] := ' rates between the three atomic energy levels, ';
	HS[6] := ' for either 2 -> 1 lasing or for 1 -> 0 lasing.';

	HS[8] := ' For each level, spontaneous emission is shown ';
	HS[9] := ' on the left, and stimulated emission on the ';
	HS[10] := ' right.';

	HS[12] := ' The rates are all artificially low due to';
	HS[13] := ' ''400'' atoms in the laser. The user should';
	HS[14] := ' mentally scale up the rates and populations.';

	HS[16] := ' A choice was made to show the flow rate ';
	HS[17] := ' physics clearly for moderate pump power.';

	HS[19] := ' No attempt was made to artifically truncate';
	HS[20] := ' the rate displays, so that very large pump';
	HS[21] := ' power can cause the flow displays to run ';
	HS[22] := ' together. This should not be cause for alarm.';

	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
END;{FlowRateInfo}

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
END;{ReminderInfo}

PROCEDURE ShowCavityHelp;
	VAR
		i: integer;
BEGIN{ShowCavityHelp}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[2] := '       Help screen -  Laser Cavity Module';
	HS[5] := ' The simulation involves two mirrors. The user';
	HS[6] := ' may insert a ray into the cavity and watch it for';
	HS[7] := ' several bounces or until its angle to the hori-';
	HS[8] := ' zontal becomes too great. A collection of rays';
	HS[9] := ' serves to indicate the stability or lack of it';
	HS[10] := ' for a given cavity.';
	HS[12] := ' One may choose a magnification of 10 for the';
	HS[13] := ' y-axis. This shows small-angle motion more';
	HS[14] := ' clearly by exaggerating y motion of the rays.';
	HS[15] := ' Gaussian wavefronts are only spherical when ';
	HS[16] := ' y^2 << z^2, which is the case using 10x magni-';
	HS[17] := ' fication along the y direction.';
	HS[19] := ' Mirror shape is not replotted when 10x y mag-';
	HS[20] := ' nification is chosen. Both mirrors would look';
	HS[21] := ' rather flat with this magnification.';
	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
END; {ShowCavityHelp}

PROCEDURE RayScreenHelp;
	VAR
		i: integer;
BEGIN{RayScreenHelp}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[2] := ' A ray is inserted into the cavity by pressing';
	HS[3] := ' F2 (Insert). The ray bounces back and forth, and';
	HS[4] := ' continues until it completes the assigned bounces';
	HS[5] := ' or the ray angle becomes too large. Each time the';
	HS[6] := ' ray passes over a pixel, the color increases by';
	HS[7] := ' one, from black (0) to white (15). After that the';
	HS[8] := ' color stays white.';

	HS[10] := ' For more precise study of beam shapes, one ';
	HS[11] := ' may launch from very low angles, and use a';
	HS[12] := ' vertical magnification (via the F5 key) of 10.';

	HS[14] := ' F5 toggles between magnifications of 1 and 10.';

	HS[16] := ' Arrow keys move the line left/right.';
	HS[17] := ' <Alt-A> or <Ins> keys increase beam height.';
	HS[18] := ' <Alt-S> or <Del> keys decrease beam height.';
	HS[19] := ' The horizontal step is adjusted with PgUp/PgDn.';

	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
END;{RayScreenHelp}

PROCEDURE BeamWidthHelp;
	VAR
		i: integer;
BEGIN{BeamWidthHelp}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[4] := ' The beam width routine uses a vertical line';
	HS[5] := ' which the user adjusts to measure gaussian beam';
	HS[6] := ' parameters.';

	HS[8] := ' The mouse can drag the line around when near it';
	HS[9] := ' in the x-direction. If mouse is below the line';
	HS[10] := ' center, it will only drag the line, but if the';
	HS[11] := ' mouse is above the center of the line, it will';
	HS[12] := ' change the line height as well as dragging it';
	HS[13] := ' in the horizontal direction.';

	HS[15] := ' Arrow keys move the line left/right.';
	HS[16] := ' <Alt-A> or <Ins> keys increase beam height.';
	HS[17] := ' <Alt-S> or <Del> keys decrease beam height.';
	HS[18] := ' The horizontal step is adjusted with PgUp/PgDn.';
	HS[20] := ' The beam may be mouse-dragged and resized from';
	HS[21] := ' the top end of the vertical bar.';

	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
END;{BeamWidthHelp}

PROCEDURE CavityParmScreen; { for setting cavity parameters }
	VAR
		RScreen: TInputScreen;
		n: byte;
BEGIN {CavityParmScreen}
	WITH RScreen DO
	BEGIN
		init;
		DefineInputPort(0.1, 0.9, 0.3, 0.7);
		LoadLine('                   Cavity Parameters');
		LoadLine('');
		LoadLine(' Left  Mirror Radius of Curvature = {     }mm (-9999 to 9999)');
		LoadLine(' Right Mirror Radius of Curvature = {     }mm (-9999 to 9999)');
		LoadLine(' (Converging mirrors have positive radii of curvature)');
		LoadLine(' Mirror Separation                = {     }mm (  300 to  500)');
		LoadLine(' Max Pairs of bounces per ray     = {     }   (    2 to  500)');
		LoadLine('');
		LoadLine('                    [  Ok  ]    [Cancel] ');
		SetNumber(1, RLeft);
		SetNumberLimits(1, -9999.0, 9999.0);
		SetNumber(2, RRight);
		SetNumberLimits(2, -9999.0, 9999.0);
		SetNumber(3, MirrDist);
		SetNumberLimits(3, 300.0, 500.0);
		SetNumber(4, NrBounces);
		SetNumberLimits(4, 2.0, 500.0);
		AcceptScreen;
		RLeft     := GetNumber(1);
		RRight    := GetNumber(2);
		MirrDist  := GetNumber(3);
		NrBounces := round(GetNumber(4));
		IF Abs(RLeft) < 110 THEN
			RLeft := Rleft * 110.0 / abs(RLeft);
		IF Abs(RRight) < 110 THEN
			RRight := Rright * 110.0 / abs(RRight);
		done;
	END;{with}
	CLeft  := XLeft + RLeft;            {centers of curvature}
	CRight := XLeft + MirrDist - RRight;
END; {CavityParmScreen}

PROCEDURE SetColorPlotVPs;
	CONST
		vpl = 0.12;
BEGIN{SetColorPlotVPs}
	DefineViewport(ColorRePlotVP, 0.01, 0.76, 0.08, 0.95);
	DefineViewport(ColorPlotVP, vpl, 0.75, 0.1, 0.92);
	ColorPlotVPx := round(vpl * GetMaxX);
	ColorPlotVPy := round(0.08 * GetMaxY);
END; {SetColorPlotVPs}

PROCEDURE xSetMirrPlotVPs;
	CONST
		vpl = 0.08;
BEGIN{SetMirrPlotVPs}
	MirrPlotVPx := round(vpl * GetMaxX);
	MirrPlotVPy := round(0.08 * GetMaxY);
END; {SetMirrPlotVPs}

PROCEDURE SetInput (Nr: integer; num, Min, Max: real);
BEGIN {SetInput}
      {Sets both number and number limits for input variable}
	RScreen.SetNumber(Nr, num);
	RScreen.SetNumberLimits(Nr, Min, Max);
END;  {SetInput}

PROCEDURE ClearAxisY; {clears off Axis routine y-labels}
BEGIN {ClearAxisY}
 { Because some Axis routine y-labels are re-done }
	ChooseVP(YAxisVP);
	ClearVP;
	ChooseVP(ColorPlotVP);
END; {ClearAxisY}

PROCEDURE YTickLabels (VAR title: str80; VAR ymax, ymin: real);
	CONST
		LnTen = 2.3026;
	VAR
		i, N: integer;
		v: STRING[8];
		x, y0, dy: real;
BEGIN {YTickLabels}
	ChooseVP(GraphVP);
    {Find power of 10, assuming positive values}
	N := trunc(ln(ymax) / LnTen);
	x := exp(-N * LnTen);
	dy := x * (ymax - ymin) / 4;
	y0 := x * ymax;
    {Put out five labels, four equal spacings}
	FOR i := 0 TO 4 DO
		WriteGr(0.05, 0.93 - i * 0.20, NumStr(y0 - i * dy, 4, 2), blanc);
	v := NumStr(N, 3, 0);
	YTitle(concat(title, concat(' x 10^', v)));
	ChooseVP(ColorPlotVP);
END; {YTickLabels}

PROCEDURE ClearAxisX; {clears off Axis routine x-labels}
BEGIN {ClearAxisX}
 { Because some Axis routine x-labels are re-done }
	ChooseVP(XAxisVP);
	ClearVP;
	ChooseVP(ColorPlotVP);
END; {ClearAxisX}

PROCEDURE XTickLabels (VAR s: str80; VAR xmax, xmin: real);
	CONST
		LnTen = 2.3026;
	VAR
		i, N, m: integer;
		v: STRING[8];
		x, y0, dy: real;
BEGIN {XTickLabels}
	ChooseVP(GraphVP);
    {Find power of 10, assuming positive values}
	N := trunc(ln(xmax) / LnTen);
	x := exp(-N * LnTen);
	dy := x * (xmax - xmin) / 4;
	y0 := x * xmin;
    {Put out five labels, four equal spacings}
	FOR i := 0 TO 4 DO
		WriteGr(0.09 + i * 0.16, 0.10, NumStr(y0 + i * dy, 4, 2), blanc); {values}
	WriteGr(0.45 - 0.01 * length(s), 0.07, concat(s, '  x 10^', NumStr(N, 3, 0)), blanc);                    {title}
	ChooseVP(ColorPlotVP);
END; {XTickLabels}

PROCEDURE YVertTitle (VAR s: Str30);
	VAR
		i, m: integer;
BEGIN {YVertTitle}
    { Write the y-axis label vertically }
	m := 10 - Length(s) DIV 2;  {rough vertical centering}
	ChooseVP(GraphVP);
	setcolor(blanc);
	FOR i := 1 TO Length(s) DO
		Print(1, i + m, s[i]);
	ChooseVP(ColorPlotVP);
END;  {YVertTitle}


PROCEDURE EnterClearVP (NewVP: integer);
BEGIN{EnterVP}
	ChooseVP(NewVP);
	ClearVP;
END; {EnterVP}


PROCEDURE ClearAllButTopMenu;
BEGIN{ClearAllButTopMenu}
	ChooseVP(MainVP);
	ValidPlot := FALSE;
	ClearVP;
END; {ClearAllButTopMenu}


PROCEDURE WriteMirrorParms (st: integer); { mirror parameters in status viewport }
	VAR
		v: Str80;
BEGIN{WriteMirrorParms}
	v := concat('   D  =', NumStr(round(MirrDist), 4, 0));
	WriteGr(StatX[1], StatY[1], v, yellow);
	v := concat('RLeft =', NumStr(round(Rleft), 4, 0));
	WriteGr(StatX[1], StatY[2], v, yellow);
	v := concat('RRight=', NumStr(round(RRight), 4, 0));
	WriteGr(StatX[1], StatY[3], v, yellow);
	WriteGr(StatX[1], StatY[4], concat('YMag= ', NumStr(YMag, 4, 0)), yellow);
END; {WriteMirrorParms}

PROCEDURE ShowParameters; {put lasing parameters in statusVP}
	VAR
		v: STRING[12];
BEGIN{ShowParameters}
	v := concat(NumStr(E21Val * 0.0259, 5, 3), ' eV');
	WriteGr(StatX[1], StatY[1], concat('E21=', v), yellow);

	v := concat(NumStr(E10Val * 0.0259, 5, 3), ' eV');
	WriteGr(StatX[1], StatY[2], concat('E10=', v), yellow);

	v := concat(NumStr(TempVal, 4, 0), ' K');
	WriteGr(StatX[1], StatY[3], concat('Temp=', v), yellow);

	WriteGr(StatX[1], StatY[4], 'Log(10) of Amn ', blanc);

	WriteGr(StatX[1], StatY[5], 'log A20:', yellow);
	WriteGr(StatX[6], StatY[5], NumStr(0.4343 * ln(A20Val), 5, 3), yellow);
	WriteGr(StatX[1], StatY[6], 'log A21:', yellow);
	WriteGr(StatX[6], StatY[6], NumStr(0.4343 * ln(A21Val), 5, 3), yellow);
	WriteGr(StatX[1], StatY[7], 'log A10:', yellow);
	WriteGr(StatX[6], StatY[7], NumStr(0.4343 * ln(A10Val), 5, 3), yellow);
END; {ShowParameters}


 {=====================  P H Y S I C S    R O U T I N E S ===================}

PROCEDURE SolveRay (x0, m, cc, r: extended; VAR x, y, x1, gam: extended);
{solve for x,y intersection of ray with mirror in mirror cc coords}
{mirror coordinates : (each mirror has its own coords)}
{ y is + upward, zero along the axis.}
{ x is zero at mirror cc.}
{ x is positive from cavity toward mirror, so}
{ convex mirrors (bulging toward cavity) have negative x's in cavity}
{ concave mirrors (bulging away fm cavity) have positive x's in cavity}
{ cc is + for concave mirrors facing center of cavity}
{}
{ m is positive for rays rising while going toward mirror}
{ m is dx/dy, the reciprocal of the usual y = mx + b slope}
{}
{ x1 is solved for in mirror coordinates, converted to next mirror}
{ outside this SolveRay routine.}
{}
{ alpha is the angle made by a ray approaching the mirror;}
{ tan(alpha) = 1/m; so alpha is negative when the slope is negative}
{}
{ gamma is the angle of the emerging ray.}
{ when the slope of the emerging ray is the same sign as that of the}
{ incoming ray, gamma will have the same sign as alpha}
{}
{ rays rising toward mirror have positive alpha}
{ rays falling going away from mirror have positive gamma}
{}
{ gamma will be used outside this routine to get the slope toward the}
{ next mirror}
{}
{ the general form is   gamma =  2 beta - alpha}
{ the distance between x and x1 is y cot(gamma)}
{ when gamma and y are both negative, and again when both are positive}
{ the reflected ray crosses the axis in real territory}
{ when gamma and y are of opposite signs, the reflected ray comes from}
{ behind the mirror}
{}
{ y^2 (m^2 + 1) + y (2 x0 m) + (x0)^2 - r^2 = 0;  m= slope}
{ x1 is intersection of reflected ray in this coord system}
{ gam is the angle of the reflected ray in this coord system}
{}
 	VAR
		a, b, c, d, e, f, yp, yn, yg, yl: extended;
		l, alpha, beta: extended;
		ch: char;

	PROCEDURE ErrMessage;
	BEGIN
		directvideo := false;
		moveto(1, 1);
		writeln('negative discriminant');
		sound(500);
		delay(1500);
		nosound;
		beep;
		beep;
		halt;
	END;

BEGIN {SolveRay}
	a := (m * m + 1.0);
	b := 2.0 * x0 * m;
	c := (x0 + r) * (x0 - r);
	IF b <> 0.0 THEN
	BEGIN
		d := b / (2.0 * a);
		e := 1.0 - 4.0 * a * c / (b * b);
		IF e < 0 THEN
			ErrMessage;

		f := sqrt(e);  {set up 2 solutions}
		yp := -d + d * f;
		yn := -d - d * f;
	END
	ELSE
	BEGIN
		d := -4.0 * a * c;
		IF d >= 0.0 THEN
		BEGIN
			yp := +sqrt(d);
			yn := -sqrt(d);
		END
		ELSE
			ErrMessage;
	END;

	IF (yp > yn) THEN
	BEGIN   {find greater and}
		yg := yp;
		yl := yn;
	END     {lesser soln's}
	ELSE
	BEGIN
		yg := yn;
		yl := yp;
	END;

	IF (r > 0) AND (m >= 0.0) THEN
		y := yg;
	IF (r > 0) AND (m < 0.0) THEN
		y := yl;
	IF (r < 0) AND (m >= 0.0) THEN
		y := yl;
	IF (r < 0) AND (m < 0.0) THEN
		y := yg;

	x := x0 + m * y; {intersection point on the mirror}

	alpha := arctan(1.0 / m);
	beta := arctan(y / x);
	gam := 2.0 * beta - alpha;         { new angle, ray from mirr }
	x1 := x - y * cos(gam) / sin(gam);   { new x-axis ray intercept }
END; {SolveRay}

PROCEDURE PlotMirrors;
forward;

PROCEDURE InsertRay;                             {ray near axis   InsertRay}
	VAR
		x01, x02, x, y, s, m: extended;
		v: STRING[6]; {Str80;}
		ch: char;
		LXOld, LYOld, RXOld, RYOld, i: integer;
		x1, alpha, gam: extended;
		yp, yn, yg, yl: extended;
		key: byte;
		DeltaY: integer;

	PROCEDURE EraseRays;
	forward;

	PROCEDURE BeamWidth; {vertical bar; total nr of rays}
           {BeamWidth}
		CONST
			HStart = 50;
			HStop = 500;
 {      st=20; { for beam width status stuff }
		VAR
			xpos {x value of vertical line}			      : integer;
			ylo  {y value when at 10% of bounces}			: integer;
			yhi  {y value when at 90% of bounces}			: integer;
			BounceNr, ii: integer;
			v: STRING[20];
			VStart, VStop {V Line extremes}      			: integer;
			IncVal: ARRAY[1..3] OF integer;

		PROCEDURE HStepSize;
		BEGIN{HStepSize}
			WriteGr(StatX[1], StatY[6], 'Hor Step Size ', blanc);
			WriteGr(StatX[2], StatY[7], NumStr(IncVal[ii], 3, 0), txt);
		END; {HStepSize}

		PROCEDURE BeamWidthStatus; {Common Status box stuff}
			VAR
				v: STRING[3];
		BEGIN{BeamWidthStatus}
			WriteMirrorParms(6);
			WriteGr(statx[1], staty[21], 'Beam bar can be', KCl);
			WriteGr(statx[1], staty[22], 'mouse-dragged', KCl);
			HStepSize;
		END;{BeamWidthStatus}

		PROCEDURE BeamZandWidth;
			VAR
				v1, v2: str80;
		BEGIN {BeamZandWidth}
			v1 := concat('Beam width = ', NumStr(VStop - VStart, 3, 0));
			v2 := concat('  z = ', NumStr(round(XPos - CLeft + RLeft - 30), 3, 0));
			WriteGr(0.3, 0.25, concat(v1, v2), lightred);
		END;  {BeamZandWidth}

		PROCEDURE LineVertStuff; {erases old, puts new}
		BEGIN{LineVertStuff}
			SetColor(blanc);
         HideMouse;
			Line(XPos, Vstart, XPos, Vstop); {erase last}
         ShowMouse;
			VStart := VStart + DeltaY;
			VStop := VStop - DeltaY;
			Line(XPos, VStart, XPos, Vstop); {write new}
			SetColor(blanc);
			BeamZandWidth;
		END; {LineVertStuff}

		PROCEDURE LineHorStuff;  {XOR's one vertical line}
		BEGIN{LineHorStuff}
			BeamZandWidth;
			SetColor(blanc);
         HideMouse;
			Line(XPos, Vstart, XPos, Vstop); {write once}
         ShowMouse;
			SetColor(blanc);
		END; {LineHorStuff}

		PROCEDURE IncrStepSize;
		BEGIN{IncrStepSize}
			inc(ii);
			IF ii > 3 THEN	ii := 1;
			HStepSize;
		END; {IncrStepSize}

		PROCEDURE MoveLineRight;
		BEGIN{MoveLineRight}
			LineHorStuff;
			XPos := XPos + IncVal[ii];
			IF XPos > HStop THEN
				XPos := HStop;
			LineHorStuff;
		END; {MoveLineRight}

		PROCEDURE MoveLineLeft;
		BEGIN{MoveLineLeft}
			LineHorStuff;
			XPos := XPos - IncVal[ii];
			IF XPos < HStart THEN
				XPos := HStart;
			LineHorStuff;
		END; {MoveLineLeft}


		PROCEDURE BiggerLine;
		BEGIN{BiggerLine}
			DeltaY := -1;
			LineVertStuff;
		END; {BiggerLine}

		PROCEDURE SmallerLine;
		BEGIN{SmallerLine}
			DeltaY := 1;
			LineVertStuff;
		END; {SmallerLine}

		PROCEDURE SetHotKeys;
		BEGIN{SetHotKeys/BeamWidth}
			WITH HotKeys DO
			BEGIN
				init(7);
				key[1] := 'F1-Help';
				key[2] := 'right-MoveRt';
				key[3] := 'left-MoveLeft';
				key[4] := 'up-IncWidth';
				key[5] := 'down-DecWidth';
				key[6] := 'PgUp-Step';
				key[7] := 'F9-Return';
				Display;
			END;
		END; {SetHotKeys}

		PROCEDURE HandleHotKeys;
		BEGIN{HandleHotKeys/BeamWidth}
			CASE key OF
				1:	BeamWidthHelp;
				2:	MoveLineRight;
				3:	MoveLineLeft;
				4: BiggerLine;
				5: SmallerLine;
				6:	IncrStepSize;
				7:	MoreRays := true;
			END;{case}
		END; {HandleHotKeys/BeamWidth}

	BEGIN{BeamWidth}
      DefineViewPort(19, 0.03, 0.75, 0.23, 0.82);
		IncVal[1] := 1;
		IncVal[2] := 10;
		IncVal[3] := 100;
		Vstart := YGC - 60;
		Vstop := YGC + 60;
		ii := 2;
		BeamWidthStatus;
		ChooseVP(FullVP);
		SetHotkeys;
		XPos := 150;
		SetWriteMode(XORPut);
		LineHorStuff; {initial line}
		MoreRays := false;
		REPEAT
			ch := INS;
			MouseGlobalPosn(GMousX, GMousY, MouseButton);
			IF (MouseButton <> 0) AND (abs(GMousX - XPos) < 30)
         and (GMousY>=VStart) and (GMousY<=VStop) THEN
			BEGIN
				REPEAT           {while button down}
					MouseGlobalPosn(GMousX, GMousY, MouseButton);
					IF  (GMousX >= Hstart) AND (GMousX <= HStop)
               and MouseInVP(19,GMousX, GMousY) THEN
					BEGIN
                 { mouse adjustment of line }
						LineHorStuff;
						IF GMousY < YGC THEN
						BEGIN
							VStart := GMousY;
							VStop  := 2 * YGC - GMousY;
						END;
						XPos := GMousX;
						LineHorStuff;
						Delay(25);
					END;
				UNTIL MouseButton = 0; {button up}
			END;

			CheckEvents;
			IF (Hotkeys.pressed(key)) AND (key <> 0) THEN
				HandleHotKeys;
		UNTIL (TopCall) OR (MoreRays);
		LineHorStuff;
		Hotkeys.Clear;
		SetWriteMode(CopyPut);
	END; {BeamWidth}

	PROCEDURE AddRays;
		VAR
			i, j, lo, hi, ylo, hue: integer;
			m: real;
	BEGIN{AddRays}
		setcolor(lightred);
		m := (LYOld - RYOld) / (LXOld - RXOld);
		IF RXOld > LXOld THEN
		BEGIN
			lo := LXOld;
			hi := RXOld;
			ylo := LYOld;
		END
		ELSE
		BEGIN
			lo := RXOld;
			hi := LXOld;
			ylo := RYOld;
		END;
		FOR i := lo TO hi DO
		BEGIN
			j := ylo + round(m * (i - lo));
			hue := GetPixel(i, j);
			IF hue < 15 THEN
				inc(hue);
			PutPixel(i, j, hue);
		END;
	END;{AddRays}

	PROCEDURE RayDisplay;
		VAR
			v1, v2, v3: STRING[6];
	BEGIN
		HotKeys.Display;
		randomize;
		textcolor(txt);
		ChooseVP(MirrPlotVP);

		sliders.init;
		v1 := NumStr(CLeft - RLeft, 5, 1);
		v2 := NumStr(CRight + RRight - 1, 5, 1);
		sliders.create(1, CLeft - RLeft, CRight + RRight - 1, XInit, 0.1, 0.115, 0.7, 3, v1, v2, 'ray x-value', false);
		v1 := NumStr(RayMinAng, 4, 2);
		v2 := NumStr(RayMaxAng, 4, 2);
		sliders.create(2, RayMinAng, RayMaxAng, RayInitAng, 0.1, 0.18, 0.7, 3, v1, v2, 'ray launch angle', false);
		sliders.drawAll;
		ChooseVP(MirrPlotVP);
	END;{RayDisplay}

	PROCEDURE EraseRays;
	BEGIN{EraseRays}
		PlotMirrors;
		RayDisplay;
	END; {EraseRays}

	PROCEDURE SetHotKeys;
	BEGIN{SetHotKeys/InsertRay}
		WITH HotKeys DO
		BEGIN
			init(6);
			key[1] := 'F1-Help';
			key[2] := 'F2-Insert';
			key[3] := 'F3-BmWidth';
			key[4] := 'F4-Erase';
			key[5] := 'F5-Magnify';
			key[6] := 'F10-Menu';
			Display;
		END;
	END; {SetHotKeys}

	PROCEDURE HandleHotKeys;
	BEGIN{HandleHotKeys/InsertRay}
		CASE key OF
			1:	RayScreenHelp;
			2:	Event.Readkey := 'i';
			3:
			BEGIN
				Hotkeys.Clear;
				BeamWidth;
			END;
			4: EraseRays;
			5: {toggle bet. 1x and 10x y magn}
			BEGIN
				IF YMag = 1.0 THEN
					YMag := 10.0
				ELSE
					YMag := 1.0;
				WriteGr(StatX[1], StatY[4], concat('YMag= ', NumStr(YMag, 4, 0)), yellow);
				ChooseVP(MirrPlotVP);
			END;
			6: TopCall := true;
		END;{case}
	END; {HandleHotKeys}

BEGIN {InsertRay}
	SetHotKeys;
	RayDisplay;
	x := XInit;
	gam := RayInitAng;
	REPEAT
		MoreRays := false;
		IF Sliders.Changed THEN
		BEGIN
		END; { keep this !! }
     { get slider values whether or not sliders change }
		x := Sliders.Value(1);
		gam := Sliders.Value(2);

		IF (Event.Readkey = 'i') OR (Event.Readkey = 'I') THEN {insert a ray}
		BEGIN

			x02 := CLeft - x; {pretend to be in left mirror coords}
			m := -cos(gam) / sin(gam);
			LXOld := round(CLeft - x02); {back to screen coords}
			LYOld := YC;
			i := 0;
			WHILE (i < NrBounces) AND (abs(gam) <= RayMaxAng) DO
			BEGIN
				inc(i);
				WriteGr(statx[1], staty[30], '  Bounce', yellow);
				v := concat(NumStr(i, 3, 0), ' of');
				WriteGr(statx[1], staty[31], concat(v, NumStr(NrBounces, 4, 0)), yellow);
				ChooseVP(MirrPlotVP);
     {switch to coordinates of right mirror}
				x01 := -CRight + CLeft - x02;
     {change sign of slope with change of coordinates}
				m := -cos(gam) / sin(gam);
				SolveRay(x01, m, CRight, RRight, x, y, x02, gam); {var}
				RXOld := round(x + CRight);
				RYOld := round(YC - y * YMag);
				AddRays;
 { now for new coordinates, based on left mirror }
 { new slope is negative of old slope }
				IF abs(gam) <= RayMaxAng THEN
				BEGIN
					m := -cos(gam) / sin(gam);
					x01 := -CRight + CLeft - x02;
					SolveRay(x01, m, CLeft, RLeft, x, y, x02, gam); {var}
					LXOld := round(CLeft - x);
					LYOld := round(YC - y * YMag);
					AddRays;
				END;
			END;
			IF (abs(gam) > RayMaxAng) THEN
				WriteGr(3, 12, 'Ray is headed out of bounds.', yellow);
		END;
		CheckEvents;
		IF (Hotkeys.pressed(key) AND (key <> 0)) THEN HandleHotKeys;
		IF MoreRays THEN SetHotkeys;
	UNTIL TopCall;
	Sliders.Done;
	PlotFlag := False;
	HotKeys.Clear;
END; {InsertRay}


PROCEDURE TwoLevels;                                             {TwoLevels}
	CONST
		Pump = 1;
		Temp = 2;
		A10 = 3;
		E10 = 4;
	CONST
		Ntot = 450.0;
		L1b = 61;
		L1e = 77;
		L2b = 97;
		L2e = 113;
	VAR
		ch: char;
		PumpVal, TempVal, A10Val, n1, n0, E10Val: real;

	PROCEDURE TwoLevelPops (temp, pump: real);
		VAR
			x, n1_n0: extended;
	BEGIN{TwoLevelPops}
		x := exp(300 * E10Val / temp) - 1.0;
		n1_n0 := pump / (x + pump);
		n0 := Ntot / (1.0 + n1_n0);
		n1 := n0 * n1_n0;
	END; {TwoLevelPops}

BEGIN{TwoLevels}
	ChooseVP(MainVP);
	ClearVP;
 { let a = E10/kT = 10.0 at 300K }
 { then a = 10*300/T }

	PumpVal := 1.0;
	TempVal := 300.0;
	A10Val := exp(-15.0);
	E10Val := 10.0;
	TwoLevelPops(TempVal, PumpVal);

	sliders.init;
	sliders.create(Pump, 0.0, 20.0, 0.0, 0.1, 0.2, 0.8, 2, 'Off', 'High', 'Log(Pump Power)', false);
	sliders.create(Temp, 10.0, 2000.0, TempVal, 0.1, 0.3, 0.8, 0, 'Low', 'High', 'Kelvin Temp', false);
	sliders.create(A10, -30.0, -10.0, ln(A10Val), 0.1, 0.4, 0.8, 2, 'Low', 'High', 'Log(A10)', false);
	sliders.create(E10, 0.0259, 3.0, E10Val * 0.0259, 0.1, 0.5, 0.8, 3, 'Low', 'High', 'Level 1 Energy (eV)', false);
	sliders.drawAll;

	ChooseVP(MainVP);
	WriteGr(0.03, 0.86, 'Level 1 population', yellow);
	WriteGr(0.03, 0.785, 'Level 0 population', yellow);

	MBar(180, L1b, 180 + round(n1), L1e, lightgray);
	MBar(180, l2b, 180 + round(n0), L2e, lightgray);
	REPEAT
		CheckForEvents;
		IF sliders.changed THEN
		BEGIN
			PumpVal := sliders.value(Pump);
			PumpVal := exp(PumpVal);
			TempVal := Sliders.value(Temp);
			A10Val := Sliders.value(A10);
			A10Val := exp(A10Val);
			E10Val := Sliders.value(E10) / 0.0259;
			TwoLevelPops(TempVal, PumpVal);

			MBar(180, L1b, 640, L1e, noir);
			MBar(180, L1b, 180 + round(n1), L1e, lightgray);
			MBar(180, L2b, 640, L2e, noir);
			MBar(180, L2b, 180 + round(n0), L2e, lightgray);
		END;
		IF TopMenu.Activated THEN
			TopCall := true;
	UNTIL TopCall;
	sliders.done;
	ChooseVP(MainVP);
	ClearVP;
END;{TwoLevels}

FUNCTION Xmn (a, e: real): extended;
BEGIN
	Xmn := a / (exp(e) - 1);
END;{Xmn}


PROCEDURE ThreeLevelPops (temp, pump: extended);                 {ThreeLevelPops}
	VAR
		x21, x10, x20, c21, c12, c22, c11, c01, c02: extended;
		f, g, p, alpha, beta, D, CoherentFactor: extended;
		x, y: real;

	PROCEDURE Populations (Z: extended); {solve for n0, n1, n2, given Xmn, Amn}

 {}
{get general solution for n0, n1, n2 using dn0/dt and dn1/dt, and sum = Ntot}
{}
{Bmn rhomn = Xmn}
{}
{dn0/dt = 0 = A20 n2 + A10 n1 +X10(n1-n0) + X20(n2-n0) = 0;}
{dn1/dt = 0 = A21 n2 - A10 n1 -X10(n1-n0) + X21(n2-n1) = 0;}
{         N = n2 + n1 + n0;}
{}
{dn0/dt   0 = n2 (A20+X20) + n1 ( A10+X10)     + n0 (-X10-X20) = 0;}
{dn1/dt   0 = n2 (A21+X21) + n1 (-A10-X10-X21) + n0 (+X10    ) = 0;}
{         N = n2           + n1                + n0            = N;}
{}
{sub for n0 in dn0/dt = 0;}
{     n2 (A20+X20) + n1 (A10+X10) = (X10+X20)(N-n1-n2);}
{     n2 [A20+X20+X10+X20] + n1 [A10+X10+X10+X20] = N(X10+X20)}
{     n2 c02               + n1 c01               = K0;}
{     n2 c02/C01           + n1                   = K0/C01;}
{}
{sub for n0 in dn1/dt = 0;}
{     n2 (A21+X21)        + n1 (-A10-X10-X21)      = -X10 (N-n1-n2);}
{     n2 (A21+X21-X10)    + n1 (-A10-X10-X21-X10)  = -N X10;}
{     n2 C12              + n1 C11                 = K1;}
{     n2 C12/C11          + n1                     = K1/C11;}
{}
{solve for n2, eliminating n1; then get n1, then n0:}
{}
{         n2 = (K0/C01 - K1/C11)/(C02/C01 - C12/C11);}
{         n1 = K1/C11 - n2 C12/C11;}
{         n0 = N - n1 - n2;}
{}
{         K0  = N(X10+X20);          K1  = -N X10;}
{         C01 = A10 +2 X10 + X20;    C11 = -A10 - 2 X10 - X21;}
{         C02 = A20 +2 X20 + X10;    C12 =  A21 +   X21 - X10;}
{}
{Pumping between 0 and 2 makes X20 become P X20.}
{}
{Lasing  between 2 and 1 makes X21 become Z X21.}
{}
{Lasing  between 1 and 0 makes X10 become Z X10.}
{}
{}
{}
	VAR
			C01, C02, C11, C12, K0, K1: extended;
	BEGIN{Populations}
		X21 := Xmn(A21Val, E21Val * 300 / temp);
		X20 := Xmn(A20Val, E20Val * 300 / temp);
		X10 := Xmn(A10Val, E10Val * 300 / temp);
		X20 := pump * X20;
		p := X20;
		IF Lase21 THEN
			X21 := Z * X21
		ELSE
			X10 := Z * X10;
		K0 := Ntot * (X10 + X20);
		K1 := -Ntot * X10;
		C01 := 1.0 / (A10Val + 2 * X10 + X20);
		C02 := A20Val + 2 * X20 + X10;
		C11 := 1.0 / (-A10Val - 2 * X10 - X21);
		C12 := A21Val + X21 - X10;
		n2 := (K0 * C01 - K1 * C11) / (C02 * C01 - C12 * C11);
		n1 := K1 * C11 - n2 * C12 * C11;
		n0 := Ntot - n1 - n2;
	END; {Populations}

	PROCEDURE NoLasingPops;
	BEGIN
	END;
BEGIN {ThreeLevelPops}
	W := 0;
	CoherentFactor := 1;
	Populations(CoherentFactor);
	delta := e * Ntot * (0.02 + trans) / 0.02;  {n2-n1 for lasing}
	IF ((n2 - n1 > delta) AND Lase21) OR ((n1 - n0 > delta) AND NOT Lase21) THEN  {lasing -get cohererent density}
	BEGIN{lasing}

		IF Lase21 THEN   { assume 2->1 lasing }
		BEGIN{2->1}
 { 2->1 lasing:}
{}
{  First find n1, n2, independent of W.}
{}
{  Solve dn0/dt = 0  ( it doesn't contain W ) and find}
{  dn0/dt = A20 n2 + X20(n2-n0) + A10 n1 + X10(n1-n0) = 0}
{}
{  n1 = alpha n0 - beta*delta}
{}
{  alpha = (X20*P+X10)/D;}
{  beta =  (X20*P+A20)/D;}
{  D = A10+A20+X20*P+X10;}
{}
{  Ntot = N0(1+2*alpha) +delta*(1-2*beta)}
{  Solve for N0, then for N1, then add delta for N2.}
{}
{  These results depend on pump rate, but not on value of W.}
{}
{  If lasing,     W>0  and n2-n1>delta.}
{  If not lasing, W=0, and n2-n1<delta.}
{}
{  See if n2-n1>delta; if not, W=0, if so, solve for W.}
{  ( 2->1 lasing transition; solve dn2/dt = 0 )}
{}
{  W/(rho21) = ((N0*P*X20-n2*(A20+A21+X20*P))/(X21*delta)) - 1}
{  If W<0, we are not lasing.}
{}
			D := A10Val + A20Val + X20 + X10;
			alpha := (X20 + X10) / D;
			beta := (X20 + A20Val) / D;
			n0 := (Ntot - delta * (1.0 - 2.0 * beta)) / (1.0 + 2.0 * alpha);
			n1 := n0 * alpha - beta * delta;
			n2 := n1 + delta;

			W := ((n0 * p - n2 * (A20Val + A21Val + p)) / (X21 * delta)) - 1.0;
		END{2->1}

		ELSE { assume lasing 1 -> 0 }
		BEGIN{1->0}
{    n1-n0 = delta, by assumption}
{}
{  dn2/dt = 0 => n2 (A20 + PX20 + A21 + X21) = n1 X21 + n0 PX20}
{}
{        n2 = [ n0 (X21+PX20) + X21 delta ]/D; D=A20+PX20+A21+X21;}
{        N  = n0 + n1 + n2 = 2 n0 + n2 + delta;}
{        n0 = (N - delta -delta X21/D)/(2 + (X21+PX20)/D )}
{        n1 = n0 + delta}
{}
{  dn0/dt = 0 => W delta X10 = -n2(A20+PX20) +n0 PX20 +n1 A21}
{}
	   	D := 1.0 / (A20val + A21val + p + X21);
			n0 := (Ntot - delta * (1.0 + X21 * D)) / (2.0 + (X21 + p) * D);
			n1 := n0 + delta;
			n2 := D * (n1 * X21 + n0 * p);
			W := (-n2 * (A20val + p) + n0 * p - n1 * A10val) / (delta * X10);

		END;{1->0}
		IF W > 1.0E19 THEN
			W := 1.0E19;
	END;{lasing}
END;{ThreeLevelPops}

PROCEDURE ThreeLevels;                                           {ThreeLevels}
	CONST
		Pump = 1;
		Temp = 2;
		Mirr = 3;
		A10 = 4;
		A21 = 5;
		A20 = 6;
		E10 = 7;
		E21 = 8;
		L1b = 50;
		L1e = 70;
		L2b = 85;
		L2e = 105;
		L3b = 120;
		L3e = 140;
		L4b = 155;
		L4e = 175;
		L5b = 190;
		L5e = 210;

	VAR
		ch: char;
		FirstTime: boolean;
		r: real;

	PROCEDURE SlidersAndBars;
	BEGIN {SlidersAndBars}
		IF (NOT FirstTime) THEN
		BEGIN
			PumpVal := LnTen * sliders.value(Pump);
			PumpVal := exp(PumpVal);
			TempVal := Sliders.value(Temp);
			Trans := Sliders.value(Mirr);
			E10Val := Sliders.value(E10) / 0.0259;
			E21Val := Sliders.value(E21) / 0.0259;
			E20Val := E21Val + E10Val;
			A10Val := LnTen * Sliders.value(A10);
			A10Val := exp(A10Val);
			A21Val := LnTen * Sliders.value(A21);
			A21Val := exp(A21Val);
			A20Val := LnTen * Sliders.value(A20);
			A20Val := exp(A20Val);
		END
		ELSE
			FirstTime := False;

		ThreeLevelPops(TempVal, PumpVal);
		ChooseVP(MainVP);

		MBar(180, L1b, 640, L1e, noir);
		MBar(180, L1b, 180 + round(n2), L1e, lightgray);

		WriteGr(StatX[5], StatY[5], NumStr(n2, 11, 8), yellow);

		MBar(180, L2b, 640, L2e, noir);
		MBar(180, L2b, 180 + round(n1), L2e, lightgray);

		WriteGr(StatX[7], StatY[8], NumStr(n1, 7, 4), yellow);

		MBar(180, L3b, 640, L3e, noir);
		MBar(180, L3b, 180 + round(n0), L3e, lightgray);

		WriteGr(StatX[8], StatY[11], NumStr(n0, 4, 1), yellow);

		MBar(180, L4b, 640, L4e, noir);
		SetFillStyle(SolidFill, lightred);
		IF W > 0.0 THEN  {coherent cavity intensity}
			MBar(180, L4b, 180 + round(10.0 * ln(W)), L4e, lightred)
		ELSE
			MBar(180, L4b, 180, L4e, lightred);
		MBar(180, L5b, 640, L5e, noir);
		IF Lase21 THEN {output intensity}
			MBar(180, L5b, 180 + round(Trans * W * 1e-4), L5e, lightred)
		ELSE
			MBar(180, L5b, 180 + round(Trans * W * 1e-4), L5e, lightred);
		ChooseVP(MainVP);
	END; {SlidersAndBars}

	PROCEDURE SlidersAndFlows;
		VAR
			x: real;
			ix: integer;
		CONST
			a1 = 100;
			a2 = 180;
			a3 = 280;
			a4 = 360;
			a5 = 460;
			a6 = 540;
		VAR
			X21, X20, X10: real;
			hue: integer;
	BEGIN {SlidersAndFlows}
		IF (NOT FirstTime) THEN
		BEGIN
			PumpVal := LnTen * sliders.value(Pump);
			PumpVal := exp(PumpVal);
			TempVal := Sliders.value(Temp);
			Trans := Sliders.value(Mirr);
			E10Val := Sliders.value(E10) / 0.0259;
			E21Val := Sliders.value(E21) / 0.0259;
			E20Val := E21Val + E10Val;
			A10Val := LnTen * Sliders.value(A10);
			A10Val := exp(A10Val);
			A21Val := LnTen * Sliders.value(A21);
			A21Val := exp(A21Val);
			A20Val := LnTen * Sliders.value(A20);
			A20Val := exp(A20Val);
		END
		ELSE
			FirstTime := False;

		ThreeLevelPops(TempVal, PumpVal);
		ChooseVP(MainVP);

		x := 300 / TempVal;
		X21 := Xmn(A21Val, E21Val * x);
		X20 := Xmn(A20Val, E20Val * x);
		X10 := Xmn(A10Val, E10Val * x);

		X20 := PumpVal * X20;
		IF W > 0.0 THEN
			IF lase21 THEN
				X21 := X21 + W * X21
			ELSE
				X10 := X10 + W * X10;
		x := 300.0 / (Ntot * A20val);
		MBar(a1 - 4, L1b, a6 + 50, L3e, noir); {blank out old stuff}

 { A20 n2 180-230 }
		ix := round(A20val * n2 * x);
		IF ix > 0 THEN
		BEGIN
			MBar(a1, L1b, a1 + ix, L3e, lightgreen);
			DrawArrow(a1, L1b, a1, L3e, lightgreen);
			DrawArrow(a1 + ix, L1b, a1 + ix, L3e, lightgreen);
		END
		ELSE
			MBar(a1, L1b, a1, L3e, lightgray);

 { X20 (n2-n0) a2-300 }
		MBar(a2, L1b, a2, L3e, lightgray);
		ix := round(X20 * (n2 - n0) * x);
		IF ix < 0 THEN
		BEGIN
			MBar(a2, L1b, a2 - ix, L3e, lightgreen);
			DrawArrow(a2, L3e, a2, L1b, lightgreen);
			DrawArrow(a2 - ix, L3e, a2 - ix, L1b, lightgreen);
		END;
		IF ix > 0 THEN
		BEGIN
			MBar(a2, L1b, a2 + ix, L3e, lightgreen);
			DrawArrow(a2, L1b, a2, L3e, lightgreen);
			DrawArrow(a2 + ix, L1b, a2 + ix, L3e, lightgreen);
		END;


 { A21 n2  a3-380 }
		MBar(a3, L1b, a3, L2e, lightgray);
		ix := round(x * A21val * n2);
		IF ix > 0 THEN
		BEGIN
			MBar(a3, L1b, a3 + ix, L2e, lightgreen);
			DrawArrow(a3, L1b, a3, L2e, lightgreen);
			DrawArrow(a3 + ix, L1b, a3 + ix, L2e, lightgreen);
		END;

 { X21(n2-n1) a4-460 }
		ix := round(x * X21 * (n2 - n1));
		MBar(a4, L1b, a4, L2e, lightgray);
		IF ix < 0 THEN
		BEGIN
			MBar(a4, L1b, a4 - ix, L2e, lightgreen);
			DrawArrow(a4, L2e, a4, L1b, lightgreen);
			DrawArrow(a4 - ix, L2e, a4 - ix, L1b, lightgreen);
		END;
		IF ix > 0 THEN
		BEGIN
			IF (lase21) AND (W > 0.0) THEN
				hue := lightred
			ELSE
				hue := lightgreen;
			MBar(a4, L1b, a4 + ix, L2e, hue);
			DrawArrow(a4, L1b, a4, L2e, hue);
			DrawArrow(a4 + ix, L1b, a4 + ix, L2e, hue);
		END;


 { A10 n1  a5-540 }
		ix := round(A10val * x * n1);
		IF ix > 0 THEN
		BEGIN
			MBar(a5, L2b, a5 + ix, L3e, lightgreen);
			DrawArrow(a5, L2b, a5, L3e, lightgreen);
			DrawArrow(a5 + ix, L2b, a5 + ix, L3e, lightgreen);
		END
		ELSE
			MBar(a5, L2b, a5, L3e, lightgray);

 { X10 (n1-n0) a6-620 }
		MBar(a6, L2b, a6, L3e, lightgray);
		ix := round(X10 * x * (n1 - n0));
		IF ix < 0 THEN
		BEGIN
			MBar(a6, L2b, a6 - ix, L3e, lightgreen);
			DrawArrow(a6, L3e, a6, L2b, lightgreen);
			DrawArrow(a6 - ix, L3e, a6 - ix, L2b, lightgreen);
		END;
		IF ix > 0 THEN
		BEGIN
			IF (NOT lase21) AND (W > 0.0) THEN
				hue := lightred
			ELSE
				hue := lightgreen;
			MBar(a6, L2b, a6 + ix, L3e, hue);
			DrawArrow(a6, L2b, a6, L3e, hue);
			DrawArrow(a6 + ix, L2b, a6 + ix, L3e, hue);
		END;


		WriteGr(StatX[5], StatY[5], NumStr(n2, 11, 8), yellow);
		WriteGr(StatX[7], StatY[8], NumStr(n1, 7, 4), yellow);
		WriteGr(StatX[8], StatY[11], NumStr(n0, 4, 1), yellow);

		hue := cyan;
		x := A20val * n2;
		WriteGr(0.15, 0.68, NumStr(x, 11, 8), hue);
		x := X20 * (n2 - n0);
		WriteGr(0.15, 0.655, NumStr(x, 11, 8), hue);

		x := A21val * n2;
		WriteGr(0.40, 0.625, NumStr(x, 14, 8), hue);
		x := X21 * (n2 - n1);
		WriteGr(0.40, 0.60, NumStr(x, 14, 8), hue);

		x := A10val * n1;
		WriteGr(0.68, 0.57, NumStr(x, 14, 8), hue);
		x := X10 * (n1 - n0);
		WriteGr(0.68, 0.545, NumStr(x, 14, 8), hue);

		hue := lightcyan;
		x := A20val * n2 + X20 * (n2 - n0);
		WriteGr(0.15, 0.515, NumStr(x, 11, 8), hue);
		x := A21val * n2 + X21 * (n2 - n1);
		WriteGr(0.40, 0.515, NumStr(x, 14, 8), hue);
		x := A10val * n1 + X10 * (n1 - n0);
		WriteGr(0.68, 0.515, NumStr(x, 14, 8), hue);

		ChooseVP(MainVP);
	END; {SlidersAndFlows}

BEGIN{ThreeLevels}
	ChooseVP(MainVP);
	ClearVP;
	FirstTime := TRUE;

 { let a = E10/kT = 10.0 at 300K }
 { then a = 10*300/T }
 { let E10 = E21; equally spaced levels }
 { get same factor for E21 as for E10, twice as big for E20 }

	ChooseVP(MainVP);
	SetBkColor(noir);
	sliders.init;
	r := 1.0 / LnTen;
	sliders.create(Pump, 0.0, 20.0, r * ln(PumpVal), 0.1, 0.05, 0.8, 2, '0.0', '20', 'Log(Pump Power)', false);
	sliders.create(Temp, 10.0, 1200.0, TempVal, 0.1, 0.13, 0.8, 0, 'Low', 'High', 'Kelvin Temp', false);
	sliders.create(Mirr, 0.0, 0.20, Trans, 0.1, 0.21, 0.8, 3, '0.0', '0.2', 'Mirror Transmission', false);
	sliders.create(A10, -10.0, -3.0, r * ln(A10Val), 0.02, 0.3, 0.38, 2, '-10', '-3', 'Log(A10)', false);
	sliders.create(E10, 0.0259, 2.0, E10Val * 0.0259, 0.45, 0.3, 0.98, 3, '.026', '2.0', '(E1 - E0) (eV)', false);

	sliders.create(A21, -10.0, -3.0, r * ln(A21Val), 0.02, 0.37, 0.38, 2, 'Low', 'High', 'Log(A21)', false);
	sliders.create(E21, 0.0259, 2.0, E21Val * 0.0259, 0.45, 0.37, 0.98, 3, '.026', '2.0', '(E2 - E1) (eV)', false);

	sliders.create(A20, -10.0, -3.0, r * ln(A20Val), 0.02, 0.44, 0.38, 2, 'Low', 'High', 'Log(A20)', false);
	sliders.drawAll;

	WriteGr(0.01, 0.88, 'Level 2 ', yellow);
	WriteGr(0.01, 0.805, 'Level 1 ', yellow);
	WriteGr(0.01, 0.73, 'Level 0 ', yellow);
	IF (NOT Flows) THEN
	BEGIN
		WriteGr(0.11, 0.88, 'population ', yellow);
		WriteGr(0.11, 0.805, 'population ', yellow);
		WriteGr(0.11, 0.73, 'population ', yellow);
		WriteGr(0.01, 0.655, 'Coherent intensity', yellow);
		WriteGr(0.01, 0.595, 'Output beam intensity', yellow);
		WriteGr(0.01, 0.57, '  (not to scale)', yellow);
	END
	ELSE  {Flow rate stuff}
	BEGIN
		WriteGr(0.86, 0.92, 'Populations', lightgray);
		WriteGr(0.01, 0.68, 'A20 n2', lightgray);
		WriteGr(0.01, 0.655, 'X20(n2-n0)', lightgray);
		WriteGr(0.01, 0.625, 'A21 n2', lightgray);
		WriteGr(0.01, 0.60, 'X21(n2-n1)', lightgray);
		WriteGr(0.01, 0.57, 'A10 n1', lightgray);
		WriteGr(0.01, 0.545, 'X10(n1-n0)', lightgray);
		WriteGr(0.01, 0.515, 'Net Flows', lightgray);
	END;
	SetTextStyle(DefaultFont, HorizDir, 2); {set large lettering}
	IF Lase21 THEN
		WriteGr(0.60, 0.99, ' 2 -> 1 Lasing', blanc)
	ELSE
		WriteGr(0.60, 0.99, ' 1 -> 0 Lasing', blanc);
	SetTextStyle(DefaultFont, HorizDir, 1); {set normal lettering}

	IF Flows THEN
		SlidersAndFlows
	ELSE
		SlidersAndBars;

	REPEAT
		TopCall := false;
		CheckForEvents;
		IF sliders.changed THEN
			IF Flows THEN
				SlidersAndFlows
			ELSE
				SlidersAndBars;
		IF TopMenu.Activated THEN
			TopCall := true;
	UNTIL TopCall;
	WriteGr(0.80, 0.95, '              ', noir);
	sliders.done;
	ClearVP;
END;{ThreeLevels}

 { =================== P L O T    R O U T I N E S ==========================}

PROCEDURE PlotMirrors;
	VAR
		v: Str80;
		i: integer;            {PlotMirrors}

	PROCEDURE MirrPlot (c, r: real; hue: integer; RtMirr: boolean);
		VAR
			x, y, cc: real;
		PROCEDURE RtPl; {Rt mirr, R>0}
		BEGIN{RtPl}
			PutPixel(round(cc + x), round(YC - y), hue);
			PutPixel(round(cc + x), round(YC + y), hue);
		END; {RtPl}
		PROCEDURE LfPl; {Left mirr, R>0}
		BEGIN{LfPl}
			PutPixel(round(cc - x), round(YC - y), hue);
			PutPixel(round(cc - x), round(YC + y), hue);
		END; {LfPl}
	BEGIN {MirrPlot}
		y := 0.0;
		cc := c;
		x := abs(r);
		WHILE y <= 100.0 DO
		BEGIN
			y := sqrt((r - x) * (r + x));
			IF (RtMirr) AND (r > 0) THEN RtPl;
			IF (RtMirr) AND (r < 0) THEN LfPl;
			IF (NOT RtMirr) AND (r > 0) THEN	LfPl;
			IF (NOT RtMirr) AND (r < 0) THEN	RtPl;
			x := x - 1.0;
		END;
	END; {MirrPlot}

BEGIN{PlotMirrors}
	ChooseVP(MainVP);
	ClearVP;
	WriteMirrorParms(8);
	ChooseVP(MirrPlotVP);
	IF (NOT PlotFlag) THEN
	BEGIN
		PlotFlag := TRUE;
		ClearVP;
	END;
	MirrPlot(CLeft, RLeft, cyan, false);
	MirrPlot(CRight, RRight, lightgreen, true);

	ChooseVP(FullVP);
	WriteGr(0.02, 0.91, '# passes', lightgray);
	FOR i := 1 TO 15 DO
	BEGIN
		WriteGr(0.11 + XPix * i * 23, 0.91, NumStr(i, 3, 0), lightgray);
		MBar(75 + i * 23, 75, 97 + i * 23, 90, i);
	END;
	v := NumStr(round(CLeft - RLeft), 3, 0);
	WriteGr(0.05, 0.523, v, lightgray);
	v := NumStr(round(CRight + Rright), 3, 0);
	WriteGr(0.75 * (CRight + RRight + 30) / 450, 0.523, v, lightgray);
	ChooseVP(MirrPlotVP);
END; {PlotMirrors}

PROCEDURE ColorPlot;                                            {ColorPlot}
	CONST
		d = 1; { 2d+1 x 2d+1 pixel area plotted }
		tr = 5;
		tc = 18; {for data just above plot}
	VAR
		i, j, k, m: integer;
		XLow, XHigh, YLow, YHigh: integer;
		xlo, xhi, ylo, yhi: real;
		PlotScaleX, PlotScaleY: real;
		g, f: real;
		N: integer;
		v: STRING[3];
BEGIN {ColorPlot}
    {this defines both plotvp and replotvp}
 {   ChooseVP(ColorReplotVP);   ClearVP;}
	ShowParameters;  {lines 1 thru 7 }
    {find max & min}
	xlo := XArr[lf];
	xhi := XArr[lf];
	ylo := Y2Arr[lf];
	yhi := Y2Arr[lf];
	FOR i := lf TO rt DO
	BEGIN
		IF XArr[i] < xlo THEN xlo := XArr[i];
		IF XArr[i] > xhi THEN xhi := XArr[i];
		IF Y1Arr[i] < ylo THEN ylo := Y1Arr[i];
		IF Y1Arr[i] > yhi THEN yhi := Y1Arr[i];
		IF Y2Arr[i] < ylo THEN ylo := Y2Arr[i];
		IF Y2Arr[i] > yhi THEN yhi := Y2Arr[i];
	END;

	IF (xhi <> xlo) AND (yhi <> ylo) THEN
	BEGIN

		DefineScale(ColorPlotVP, xlo, xhi, ylo, yhi);
		GraphBackColor := noir;
		SetColor(GraphBoxColor);
		SelectScale(ColorPlotVP);
		OpenViewPort(ColorPlotVP);

		Map(xlo, ylo, XLow, YHigh);     {lower screen corner coords}
		Map(xhi, yhi, XHigh, YLow);     {upper screen corner coords}

		Axis(xlo, ylo, (xhi - xlo) / 4, (yhi - ylo) / 4);
		ClearAxisY;
		YTickLabels(YAxLabel, yhi, ylo);
 {   YVertTitle(YAxLabel);}
 {   Setcolor(White);  PutLabel(Bottom, 'Counts');}
 {   YTitle(YAxLabel);}
 {                     PutLabel(Left,   'Nr of Occurrences');}

		ClearAxisX;
		XTickLabels(XAxLabel, xhi, xlo);

		SetWriteMode(CopyPut);
      HideMouse;
		FOR i := lf TO rt DO {plot pts as squares}
		BEGIN
			Map(XArr[i], Y1Arr[i], j, k);
			SetColor(lightgreen);
			FOR m := j - d TO j + d DO
				line(m, k - d, m, k + d);
			Map(XArr[i], Y2Arr[i], j, k);
			SetColor(lightred);
			FOR m := j - d TO j + d DO
				line(m, k - d, m, k + d);
		END;
      ShowMouse;
		ValidPlot := TRUE;
	END
	ELSE
	BEGIN
		beep;
		Announcement;
	END;
END; {ColorPlot}

PROCEDURE BigLasingLabel;
	VAR
		v1, v2: STRING[30];
BEGIN{BigLasingLabel}
	IF Lase21 THEN
		v1 := '2 -> 1 '
	ELSE
		v1 := '1 -> 0 ';
	SetTextStyle(DefaultFont, HorizDir, 2);
	WriteGr(statx[1], staty[12], v1, blanc);
	SetTextStyle(DefaultFont, HorizDir, 1);
END; {BigLasingLabel}

PROCEDURE LasingLabels;
	VAR
		v1, v2: STRING[30];
BEGIN{LasingLabels}
	BigLasingLabel;
	MBar(580, 270, 610, 285, lightred);
	MBar(580, 295, 610, 310, lightgreen);
	IF Lase21 THEN
	BEGIN
		v1 := 'Level 2 ';
		v2 := 'Level 1 ';
	END
	ELSE
	BEGIN
		v1 := 'Level 1 ';
		v2 := 'Level 0 ';
	END;
	WriteGr(statx[1], staty[23], v1, yellow);
	WriteGr(statx[1], staty[25], v2, yellow);
END; {LasingLabels}

PROCEDURE Plotpops;  { populations vs. pump power }
	VAR
		i: integer;
		p, q: extended;
		v1, v2: str80;
BEGIN{PlotPops}
	EnterClearVP(MainVP);
	q := PumpVal;
	IF q <> 0.0 THEN
	BEGIN
		lf := 1;
		rt := 101;
		FOR i := lf TO rt DO
		BEGIN
			p := q * (0.9 + 0.1 * i);
			ThreeLevelPops(tempval, p);
			XArr[i] := p;
			IF Lase21 THEN
			BEGIN
				Y2Arr[i] := n2;
				Y1Arr[i] := n1;
			END
			ELSE
			BEGIN
				Y2Arr[i] := n1;
				Y1Arr[i] := n0;
			END
		END;
		XAxLabel := 'Relative Pump Power';
		YAxLabel := 'Population';

		WriteGr(statx[1], staty[9], '    Mirror      ', blanc);
		WriteGr(statx[1], staty[10], 'Transmission = 0', blanc);

		LasingLabels;
		ColorPlot;
	END
	ELSE
	BEGIN
		beep;
		announcement;
	END;
	ThreeLevelPops(TempVal, PumpVal); {restore values}
END; {PlotPops}

PROCEDURE PlotW; { coherent intensity vs. pump power }
	VAR
		i: integer;
		p, q: extended;
BEGIN {PlotW}
	EnterClearVP(MainVP);
	q := PumpVal;
	lf := 1;
	rt := 101;
	FOR i := lf TO rt DO
	BEGIN
		p := q * (0.9 + 0.1 * i);
		ThreeLevelPops(tempval, p);
		XArr[i] := p;
		Y2Arr[i] := W / 100;
		Y1Arr[i] := W / 100;
	END;
	XAxLabel := 'Relative Pump Power';
	YAxLabel := 'Intensity';
	WriteGr(statx[1], staty[9], '    Mirror      ', blanc);
	WriteGr(statx[1], staty[10], 'Transmission = 0', blanc);

	BigLasingLabel;
	ColorPlot;
	ThreeLevelPops(TempVal, PumpVal); {restore all values}
END;  {PlotW}

PROCEDURE PlotOutM; { laser output vs. mirror transmission }
	VAR
		i: integer;
		p, y, s: extended;
		v: STRING[6];
BEGIN {PlotOutM}
	EnterClearVP(MainVP);
	s := trans;     { save & restore trans value }
	trans := 0.0;
	ThreeLevelPops(TempVal, PumpVal);
 {IF W<>0.0 THEN                            BEGIN}
	lf := 1;
	rt := 100;
	FOR i := lf TO rt DO
	BEGIN
		Trans := 0.001 * (i - 1);
		ThreeLevelPops(TempVal, PumpVal);
		y := Trans * W * 1.0E-4;
		XArr[i] := Trans;
		Y2Arr[i] := y;
		Y1Arr[i] := y;
	END;
	XAxLabel := 'Mirror Transmission';
	YAxLabel := 'Output Intensity';

	WriteGr(statx[1], staty[9], 'Log(10)Pump Rate', blanc);
	v := NumStr(0.4343 * ln(PumpVal), 6, 3);
	WriteGr(statx[1], staty[10], v, yellow);

	BigLasingLabel;
	ColorPlot;
	trans := s;
	ThreeLevelPops(TempVal, PumpVal); {restore values}
END; {PlotOutM}

PROCEDURE PlotPopM; { populations vs. mirror transmission }
	VAR
		i: integer;
		p, y, s: extended;
		v: STRING[6];
BEGIN {PlotPopM}
	EnterClearVP(MainVP);
	s := trans;     { save & restore trans value }
	trans := 0.0;
	ThreeLevelPops(TempVal, PumpVal);
	lf := 1;
	rt := 100;
	FOR i := lf TO rt DO
	BEGIN
		Trans := 0.001 * (i - 1);
		ThreeLevelPops(TempVal, PumpVal);
		XArr[i] := Trans;
		IF Lase21 THEN
		BEGIN
			Y2Arr[i] := n2;
			Y1Arr[i] := n1;
		END
		ELSE
		BEGIN
			Y2Arr[i] := n1;
			Y1Arr[i] := n0;
		END;
	END;
	XAxLabel := 'Mirror Transmission';
	YAxLabel := 'Populations';

	WriteGr(statx[1], staty[9], 'Log(10)Pump Rate', blanc);
	v := NumStr(0.4343 * ln(PumpVal), 6, 3);
	WriteGr(statx[1], staty[10], v, yellow);

	LasingLabels;
	ColorPlot;
	trans := s;
	ThreeLevelPops(TempVal, PumpVal); {restore values}
END; {PlotPopM}

 {=====================  M E N U     S T U F F  ===========================}

PROCEDURE SetTopMenu;
	VAR
		i: integer;
BEGIN{SetTopMenu}
	TopMenu.Init;
	WITH TopMenu DO
	BEGIN
		column(1, 'File');
		row(1, 1, 'About CUPS    ');
		row(1, 2, 'About Program ');
		row(1, 3, 'Configuration');
		row(1, 4, 'Some Reminders');
		row(1, 5, 'About Using Sliders');
		row(1, 6, '-------------------');
		row(1, 7, 'Exit Program');
		rowactivate(1, 6, false);
		column(2, 'Lasers');
		row(2, 1, '2 levels:   no Lasing ');
		row(2, 2, '3 levels:   Lasing 2->1');
		row(2, 3, '3 levels:   Lasing 1->0');
		row(2, 4, 'Flow Rates: Lasing 2->1');
		row(2, 5, 'Flow Rates: Lasing 1->0');
		row(2, 6, 'Restore default values');
		row(2, 7, 'Flow Rate Help ');
		column(3, 'Do Plot');
		row(3, 1, 'Populations      vs. Pump Rate');
		row(3, 2, 'Cavity Intensity vs. Pump Rate');
		row(3, 3, 'Output Intensity vs. Mirror Trans.');
		row(3, 4, 'Populations      vs. Mirror Trans.');
		row(3, 5, 'Information about Plots.');
		column(4, 'Cavity');
		row(4, 1, 'Cavity Stability via Ray Tracing');
		row(4, 2, 'Set Cavity Parameters');
		row(4, 3, 'Cavity Stability Help');
		row(4, 4, 'Cavity Ray Help');
		row(4, 5, 'Beam Width Help');
	END;
END;{SetTopMenu}

PROCEDURE SetDefaults;
forward;

PROCEDURE HandleTopMenu;
	VAR
		i: integer;
	PROCEDURE CavityStability;
	BEGIN
		TopMenu.Display;
		PlotMirrors;
		InsertRay;
	END; {CavityStability}

BEGIN{HandleTopMenu}
	TopCall := false;
	Flows := False;
	WITH TopMenu DO
		CASE colChosen OF
			1: 
				CASE rowchosen OF
					1:	AboutCups;
					2:	OpeningScreen;
					3:	Configuration;
					4:	ReminderInfo;
					5:	SliderUseInfo;
					7:	ReturnFlag := TRUE;
				END; {case}
			2:
				CASE rowchosen OF
					1:	TwoLevels;
					2:
					BEGIN
						Lase21 := TRUE;
						ThreeLevels;
					END;
					3:
					BEGIN
						Lase21 := FALSE;
						ThreeLevels;
					END;
					4:
					BEGIN
						Flows := true;
						Lase21 := TRUE;
						ThreeLevels;
					END;
					5:
					BEGIN
						Flows := true;
						Lase21 := FALSE;
						ThreeLevels;
					END;
					6: SetDefaults;
					7: FlowRateInfo;
				END; {case}
			3:
				CASE rowchosen OF
					1: PlotPops;
					2: PlotW;
					3: PlotOutM;
					4: PlotPopM;
					5: AboutPlots;
				END; {case}
			4:
				CASE rowchosen OF
					1: CavityStability;
					2:
               BEGIN
						CavityParmScreen;
						CavityStability;
					END;
					3: ShowCavityHelp;
					4:	RayScreenHelp;
					5:	BeamWidthHelp;
				END; {case}
		END;{CASE}
	FrontKeys.Display;
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
	VAR
		i: integer;
BEGIN {SetDefaults}
	RLeft := 500.0;
	RRight := 600.0;
	MirrDist := 400.0;
	NrBounces := 100;
	CLeft := XLeft + RLeft;        {centers of curvature}
	CRight := XLeft + MirrDist - RRight;
	RayMinAng := 0.01;
	RayMaxAng := 0.12;
	RayInitAng := 0.15;
	YMag := 1.0;
	XInit := CLeft - RLeft + 150;

	A10Val := 1.0E-6;
	A21Val := 1.0E-6;
	A20Val := 1.0E-6;
	W := 0.0;
	Trans := 0.0;
	PumpVal := 1.0;
	E21Val := 10.0;
	E10Val := 10.0;
	E20Val := E21Val + E10Val;
	TempVal := 300.0;

	PlotFlag := false;
	TopDisplay := false;
	ValidPlot := FALSE;
	ReturnFlag := FALSE;
	Lase21 := TRUE;

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
	CInc[1] := 1;
	CInc[2] := 10;
	CInc[3] := 100;

	blanc := white;
	noir := black;
	PCVersion := true;

	heading := blanc;
	GraphBoxColor := LightGreen;   {Axes and Legend color.}
	AxisLabelColor := blanc;

	ChooseVP(FullVP);
	ClearVP;
	SetTopMenu;
	TopMenu.Display;
	TopDisplay := TRUE;
	PlotMirrors;                {'happy' first screen}
	SetFrontKeys;
END; {SetDefaults}

PROCEDURE LaserMain;
BEGIN{LaserMain}
	OpeningScreen;
	REPEAT
		CheckForEvents;
		IF (NOT TopDisplay) THEN
		BEGIN
			TopDisplay := true;
			TopMenu.Display;
			FrontKeys.Display;
		END;
    {Did the last activity call for top menu action?}
    {If, so don't check for further events and wipe out that call}
		IF FrontKeys.pressed(key) THEN HandleFrontKeys;
		IF TopMenu.Activated THEN HandleTopMenu;
		IF NOT TopCall THEN CheckEvents; {this could make TopCall true}
		IF TopCall THEN HandleTopMenu;
	UNTIL ReturnFlag;
	TopMenu.Done;
END;{LaserMain}

BEGIN{MainPgm}
	CupsInit; {  CUPSInit; turns mouse on, registers egavga}
	DefineViewport(FullVP, 0.0, 1.0, 0.0, 1.0);  {full screen}
	DefineViewPort(MainVP, 0.0, 1.0, 0.0, 0.95); {all except top}
	DefineViewport(GraphVP, 0.0, 0.8, 0.0, 0.95); {incl. titles &c}
	DefineViewport(YAxisVP, 0.0, 0.11, 0.0, 0.95); {incl. titles &c}
	DefineViewport(XAxisVP, 0.0, 0.8, 0.0, 0.11); {incl. titles &c}
	DefineViewport(InputVP, 0.0, 1.0, 0.0, 0.45); {Numeric input}
	DefineViewport(ColorPlotVP, 0.12, 0.75, 0.12, 0.92);
	DefineViewport(ColorRePlotVP, 0.10, 0.76, 0.0, 0.95);
	DefineViewport(MirrRePlotVP, 0.01, 0.95, 0.19, 0.84);{76}
	DefineViewport(MirrPlotVP, 0.08, 0.94, 0.24, 0.79);{75}
	MouseShowing := true;
	SetDefaults;
	LaserMain;
	CupsDone;
END.