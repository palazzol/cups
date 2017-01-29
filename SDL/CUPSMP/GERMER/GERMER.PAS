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


 {===============  DEFINE GLOBAL CONSTANTS, TYPES AND VARIABLES =======}

TYPE
	Str80 = STRING[80];

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

CONST
	FullVP = 0;  {Full screen viewport nr}
	TargetsVP = 1;
	lgy = lightgray;
	CtrX = 195;
	CtrY = 166;
	h = 6.626E-34;
	e = 1.6E-19;
	c = 2.998e8;
	Me = 9.1E-31;

VAR
	sliders: TSliders;
	HS: HelpScrType;  { global help screen }
	GBarMenu: TBarMenu;     { global bar menu    }
	TopMenu: TMenu;
	Hotkeys: THotkeys;
	FrontKeys: THotkeys;    {keys for opening screen}
	key: byte;

	WLength: real;
	VoltVal: real;
	CVoltS, FVoltS: real;    { coarse, fine voltage values }
	CVoltSX, FVoltSX: real;    { coarse, fine voltage for xrays}
	VoltValS: real;    { accelerating voltage for electrons }
	CVoltM, FVoltM: real;    { coarse, fine voltage values }
	VoltValM: real;    { accelerating voltage for electrons }
	TargAng, OldTargAng: real;    { target orientation angle }
	StartAngS, EndAngS: real;
	StartAngM, EndAngM: real;
	Gain: real;
	e1: real;
	NrCrystals: integer;
	NrXtallites: real;
	XtlNrX: real;
	XtlNr: integer; {# atoms in a crystallite row or col}
	DGCols: real; {Nr of columns in DG crystal}
	CenterAngle: real;
	CAngleMax, CAngleMin: real;
	SingleCrystal: boolean;
	GoTop, XRays: boolean;
	hk0, face: boolean; {hk0 or 111 in GPT, face plane in DG}
	PCVersion: boolean;
	NotChanged: boolean; {Sliders Not Changed}
	CurrentVP: integer; {number of currently chosen viewport}
	XScale, YScale: real;    { PC/Mac x,y scale factors}
	p1, p2, p3, p4, p5: PointType;

	ValidPlot: boolean; {is there a valid plot on the screeen?}
	RePlotFlag: boolean; {should module do a re-plot?}
	MouseShowing: boolean; {is mouse cursor showing on screen?}
	StatusShowing: boolean; {is status area showing on screen?}
	ReturnFlag: boolean; { exit the program? }
	TopDisplay: boolean; { top menu showing? }
	MouseButton: integer;
	blanc, noir: word;
	PlotFlag: boolean;
	Anglet: ARRAY[1..100] OF real;
	z: ARRAY[1..100] OF pointtype;

	heading: word;
	WlengthTextX, WlengthValX, WlengthMsgY: real;
	TotalVoltageY: real;
	BeamInX, BeamInY: real;
	ScanMsgY, ScanLoX, ScanMsgX, ScanHiX: real;
	LAnglY, LAnglTextX, LAnglValX: real;
	CVoltSl, FVoltSl, AStartSl, AEndSl: integer;
	CntrAngSl, DGColSl: integer;
	GainSl, TDirSl, XtalliteSl, XTlNrSl: integer;
	ch: char;

 {==========================  Utility Routines  =============================}
 {==========================  Utility Routines  =============================}
PROCEDURE HandleTopMenu; forward;
PROCEDURE SetFrontKeys;  forward;


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

PROCEDURE MLine (x1, y1, x2, y2: integer);
BEGIN {MLine}
	line(round(x1 * XScale), round(y1 * YScale), round(x2 * XScale), round(y2 * YScale));
END; {MLine}

PROCEDURE MBar (x1, y1, x2, y2: integer);
BEGIN {MBar}
 { do Bar on full screen in screen-rel coordinates }
	Bar(round(x1 * XScale), round(y1 * YScale), round(x2 * XScale), round(y2 * YScale));
END; {MBar}

PROCEDURE ClearArea (x1, y1, x2, y2: integer);
BEGIN {ClearArea}
	SetFillStyle(SolidFill, noir);
	MBar(x1, y1, x2, y2);
END;  {ClearArea}

FUNCTION dg (strg: Str80): STRING;
BEGIN {dg}
 { add degrees to a string }
	dg := concat(strg, chr(248));
END; {dg}

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

PROCEDURE SetEventEscape;
BEGIN{SetEventEsc}
	GoTop := true;
	IF TopMenu.Chosen THEN
		HandleTopMenu;
END;{SetEventEsc}

PROCEDURE WriteGr (XScreen, YScreen: real; msg: Str80; hue: integer);
 { write to x,y 0..1 in screen-relative coords}
BEGIN {WriteGr}
	RuboutXY(XScreen, YScreen, length(msg), noir);
	setcolor(hue);
	PrintXY(XScreen, YScreen, msg);
END;{WriteGr}

 {===================== INFORMATION ROUTINES ===============================}
 {===================== INFORMATION ROUTINES ===============================}

PROCEDURE OpeningScreen;
	VAR
		i: integer;
BEGIN{OpeningScreen}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[2] := ' Scattering of Electrons and X-Rays from Crystals';
	HS[4] := '             Written by Mike Moloney';
	HS[5] := '       Rose-Hulman Institute of Technology';
	HS[6] := '               Copyright (c) 1994';
	HS[8] := ' The classic experiments of Davisson/Germer and';
	HS[9] := ' Thomson are modelled by coherent scattering of';
	HS[10] := ' electrons from regular arrays of atoms. ';
	HS[12] := ' The physics lies in interference between';
	HS[13] := ' scattered beams. Scattered amplitudes must be';
	HS[14] := ' added with proper phase, and phase differences';
	HS[15] := ' occur due to the geometry of crystal orienta-';
	HS[16] := ' tion and scattering angle. ';
	HS[18] := ' Where multiple small crystals scatter, the ';
	HS[19] := ' phase of scattering from one crystal must be';
	HS[20] := ' related to the phase of waves scattered from';
	HS[21] := ' a reference point.';

	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
END;{OpeningScreen}

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
END;{SliderUseInfo}

PROCEDURE DavissonInfo;
	VAR
		i: integer;
BEGIN{DavissonInfo}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[4] := ' In 1927, Davisson and Germer at Bell Labs ';
	HS[5] := ' observed electron-wave diffraction of reflected';
	HS[6] := ' electrons from a nickel target. ';

	HS[8] := ' A single 2-D crystal simulates the';
	HS[9] := ' face-centered-cubic (fcc) nickel crystal used';
	HS[10] := ' by Davisson and Germer. The spacing perpendic-';
	HS[11] := ' ular to the incoming beam is 2.15 A, and the';
	HS[12] := ' spacing in the direction of the beam is 3.04 A';

	HS[15] := ' One may scan the detector over a range of ';
	HS[16] := ' angles, at fixed energy, and also scan over';
	HS[17] := ' accelerating V at a fixed detector angle of';
	HS[18] := ' 90 degrees.';

	HS[20] := ' Note that the gray angle markers are all 0.40';
	HS[21] := ' degrees wide, extending 0.2 degrees to either';
	HS[22] := ' side of the angle';

	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
END;{DavissonInfo}

PROCEDURE ThomsonInfo;
	VAR
		i: integer;
BEGIN{ThomsonInfo}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[2] := ' In 1928, G. P. Thomson passed a beam of cathode';
	HS[3] := ' rays through very thin metal foils and onto a';
	HS[4] := ' photographic plate. The resulting pattern of';
	HS[5] := ' concentric rings showed scattering of electron-';
	HS[6] := ' waves from the many small crystals oriented at';
	HS[7] := ' random in the metal foil. ';

	HS[9] := ' This 2-dimensional simulation offers the user up';
	HS[10] := ' to 100 small crystals, oriented within +\- 0.05 ';
	HS[11] := ' radians of a selected central angle, giving the ';
	HS[12] := ' effect of a portion of the small crystals in a';
	HS[13] := ' metal foil. The spacing is that of aluminum, ';
	HS[14] := ' (lattice type = fcc), oriented for scattering';
	HS[15] := ' in (mn0) directions.';

	HS[17] := ' The small crystals are N x N atoms, and the user';
	HS[18] := ' N to observe effects of crystal size on the';
	HS[19] := ' distribution of scattered electrons.';

	HS[21] := ' The aluminum (fcc) lattice constant 4.05';
	HS[22] := ' angstroms is used here, a value typical of the';
	HS[23] := ' metal foils employed by G. P. Thomson.';

	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
END;{ThomsonInfo}

PROCEDURE MoreThomsonInfo;
	VAR
		i: integer;
BEGIN{MoreThomsonInfo}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[2] := ' The user may select a fresh set of crystallites';
	HS[3] := ' via the ''NewXtals'' hotkey. Until then, the';
	HS[4] := ' same set of crystallites may be re-used for';
	HS[5] := ' many different runs.';

	HS[7] := ' The ''AngScan'' hotkey is used to begin a scan';
	HS[8] := ' over angles. In between scans, user may change';
	HS[9] := ' any of the slider values.';

	HS[10] := ' The user can set the central crystal angle, up';
	HS[11] := ' to 45 degrees. The crystallites are distributed';
	HS[12] := ' within plus or minus 0.05 radians of this value.';
	HS[13] := ' If only one crystallite is selected, its angle';
	HS[14] := ' is set to the central crystal angle.';

	HS[17] := ' It is interesting to change the number N of';
	HS[18] := ' atoms on a crystallite side. This changes the ';
	HS[19] := ' size of the ''side lobes'', with larger N ';
	HS[20] := ' giving sharper, narrower peaks.';

	HS[22] := ' Note that the gray angle markers are all 0.40';
	HS[23] := ' degrees wide, extending 0.2 degrees to either';
	HS[24] := ' side of the angle';

	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
END;{MoreThomsonInfo}

PROCEDURE VoltageInfo;
	VAR
		i: integer;
BEGIN{VoltageInfo}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[3] := ' Some electron scattering experiments have been ';
	HS[4] := ' done by scanning over electron accelerating ';
	HS[5] := ' voltage and observing the scattering from a ';
	HS[6] := ' fixed detector angle.';

	HS[8] := ' The detector in this scan will be fixed at ';
	HS[9] := ' 90ø, and the user may set the target at any';
	HS[10] := ' angle between 0ø and 90ø.';

	HS[12] := ' Voltage will be scanned from a starting ';
	HS[13] := ' value to 16 times the starting voltage value.';

	HS[15] := ' Detector amplitude will be plotted vs. ';
	HS[16] := '    the square root of voltage.';

	HS[18] := ' It is especially interesting to scan with the';
	HS[19] := ' target crystal at a 45ø angle, because the';
	HS[20] := ' detector can pick up Bragg-scattered electrons.';

	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
END;{VoltageInfo}


PROCEDURE XRayInfo;
	VAR
		i: integer;
BEGIN{XRayInfo}
	FOR i := 1 TO 25 DO
		HS[i] := ''; {clear it}
           {12345678901234567890123456789012345678901234567890}
	HS[3] := ' The x-ray diffractometer uses a single crystal';
	HS[4] := ' which is rotated at half the angular rate of';
	HS[5] := ' the detector, so that the detector is always';
	HS[6] := ' observing Bragg-scattered x-rays.';

	HS[8] := ' This simulation is included because of its ';
	HS[9] := ' similarity of x-ray scattering to electron';
	HS[10] := ' scattering.';

	HS[12] := ' It is also interesting to observe an instrument';
	HS[13] := ' contrived especially to examine ''pure'' Bragg';
	HS[14] := ' scattering.';

	HS[16] := ' The lattice spacing of the crystal is 2.014 A,';
	HS[17] := ' which is that of lithium fluoride (LiF), a';
	HS[18] := ' commonly-used crystal in x-ray work.';

	HS[20] := ' Note that the gray angle markers are all 0.40';
	HS[21] := ' degrees wide, extending 0.20 degrees to either';
	HS[22] := ' side of the angle.';

	HS[25] := ' Press <Enter> or click mouse to continue.  ';
	Help(HS);
END;{XRayInfo}

 {=====================  D I S P L A Y    R O U T I N E S ===================}
 {=====================  D I S P L A Y    R O U T I N E S ===================}

PROCEDURE BeamLabel;
BEGIN {BeamLabel}
	setcolor(green);
	Mline(0, 155, 125, 155);
	Mline(125, 155, 125, 145);
	Mline(125, 145, 135, 167);
	Mline(125, 185, 135, 167);
	Mline(125, 175, 125, 185);
	Mline(0, 175, 125, 175);
	IF NOT XRays THEN
		WriteGr(BeamInX, BeamInY, 'Electron beam in', lightgray)
	ELSE
		WriteGr(BeamInX, BeamInY, 'X-Ray beam in   ', lightgray);
END;  {BeamLabel}
PROCEDURE DrawQuad;
	VAR
		i: integer;
	VAR
		Grid: ARRAY[1..5] OF PointType;{}
 {var Grid: PolyType;}
BEGIN{DrawQuad}
	Grid[1] := p1;
	Grid[2] := p2;
	Grid[3] := p3;
	Grid[4] := p4;
	Grid[5] := p1;
	FOR i := 1 TO 5 DO
		Grid[i].x := round(Grid[i].x * XScale);
	FOR i := 1 TO 5 DO
		Grid[i].y := round(Grid[i].y * YScale);
	SetColor(cyan);
	SetLineStyle(SolidLn, 0, NormWidth);
	DrawPoly(5, Grid);
END; {DrawQuad}

PROCEDURE DrawCrystal (an: real);
	VAR
		s, c, cx, cy: integer;
BEGIN{DrawCrystal}
	ClearArea(CtrX - 50, CtrY - 50, CtrX + 50, CtrY + 50); {clear old area}
	s := -round(30 * sin(an));
	c := round(30 * cos(an));
	cx := CtrX;
	cy := CtrY;
	p1.x := cx + c + s;
	p1.y := cy - (-s + c);
	p2.x := cx + c - s;
	p2.y := cy - (-s - c);
	p3.x := cx - c - s;
	p3.y := cy - (+s - c);
	p4.x := cx - c + s;
	p4.y := cy - (+s + c);
	DrawQuad;
END;{DrawCrystal}

 {=====================  P H Y S I C S    R O U T I N E S ===================}
 {=====================  P H Y S I C S    R O U T I N E S ===================}

FUNCTION Momentum (VAR voltage: real): real;
BEGIN {Momentum}
	IF XRays THEN
		Momentum := e * Voltage / c
	ELSE
		Momentum := sqrt(2.0 * e * Me * Voltage) * (1.0 + e1 * Voltage);
END;  {Momentum}
FUNCTION Wavelength (Voltage: real): real;  {in angstroms}
BEGIN {Wavelength}
	Wavelength := 1.0E10 * h / Momentum(Voltage);
END;  {Wavelength}

PROCEDURE InitSliders;
BEGIN {InitSliders}
	Sliders.Init;
	IF SingleCrystal THEN
	BEGIN{single xtal}
		IF NOT XRays THEN  {DG}
		BEGIN
			sliders.create(CVoltSl, 0.1, 100.0, CVoltS, 0.6, 0.90, 0.98, 1, '0.1', '100.0', ' Coarse Voltage ', false);
			sliders.create(FVoltSl, 0.0, 5.0, FVoltS, 0.6, 0.82, 0.98, 2, '0.0', '5.0', ' Fine Voltage ', false);
			sliders.create(TDirSl, 0.0, 100.0, TargAng, 0.6, 0.30, 0.98, 1, dg('0'), dg('100'), 'Target Direction', false);
		END
		ELSE {XRays}
		BEGIN
			sliders.create(CVoltSl, 500, 1.0E5, CVoltSX, 0.6, 0.90, 0.98, 0, '500', '1.0E5', ' Coarse Voltage ', false);
			sliders.create(FVoltSl, 0.0, 1000.0, FVoltSX, 0.6, 0.82, 0.98, 0, '0', '1000', ' Fine Voltage ', false);
		END;
    {both DG and XRays}
		sliders.create(AStartSl, -170, +170, StartAngS, 0.6, 0.65, 0.98, 1, dg('-170'), dg('+170'), 'Scan Start', false);
		sliders.create(AEndSl, -170, +170, EndAngS, 0.6, 0.57, 0.98, 1, dg('-170'), dg('+170'), 'Scan End', false);
		sliders.create(GainSl, 1.0, 100, Gain, 0.6, 0.40, 0.98, 1, '1', '100', ' Detector Gain ', false);
	END {SingleXtal}
	ELSE {Crystallites}
	BEGIN
		sliders.create(CVoltSl, 100.0, 20000.0, CVoltM, 0.6, 0.90, 0.98, 0,
      '100', '2.0E4', ' Coarse Voltage ', false);
		sliders.create(FVoltSl, 0.0, 500, FVoltM, 0.6, 0.82, 0.98, 0,
      '0.0', '500', ' Fine Voltage ', false);

		sliders.create(AStartSl, -90, 90, StartAngM, 0.6, 0.65, 0.98, 1,
      dg('-90'), dg('+90'), 'Scan Start', false);
		sliders.create(AEndSl, -90, +90, EndAngM, 0.6, 0.57, 0.98, 1,
      dg('-90'), dg('+90'), 'Scan End', false);

		sliders.create(GainSl, 1.0, 100, Gain, 0.6, 0.40, 0.98, 0,
      '1', '100', ' Detector Gain ', false);

		sliders.create(XtalliteSl, 1.0, 100.0, NrXtallites, 0.6, 0.30, 0.98, 0,
      '1', '100', ' # Crystallites ', false);
		sliders.create(XtlNrSl, 2.0, 100.0, XtlNrX, 0.6, 0.22, 0.98, 0,
      '2', '100', '# in row or col (N)', false);
		sliders.create(CntrAngSl, CAngleMin, CAngleMax, CenterAngle,
      0.6, 0.14, 0.98, 1, NumStr(CAngleMin, 2, 0), NumStr(CAngleMax, 2, 0), ' Center Angle ', false)
	END;{Xtallites}
END;{InitSliders}

PROCEDURE Experiment;                                        {EXPERIMENT}
	VAR
		i, j, k, m, cx, cy: integer;
		LoAngle, HiAngle: real;
		f, f1, f2, fs, HalfPhase: real;
		t, step, ang, bigphase: real;
		p, g, x: real;
		v: STRING[8];
		y: real;
		xs, xc, ys, yc: integer;
		AngScan: boolean;
     { theta is scattering angle; alpha is xtal ccw rotation angle }
		an, del: real; {theta/2, theta/2 - alpha}
		CosAn, SinAn: real; {cos, sin of theta/2}
		DeltaPhCol, DeltaPhRow: real; {half phase difference in cols, rows}
		RowG, ColG: real; {RowG for scatt by members of same row}
		RowG2, ColG2, CosAn2, SinAn2: real;
		NrXtlX, NrXtlY: integer;
		conv: real;
		OldNrCrystals: integer;
		OldCenterAngle: real;
		OldXtlNr: integer;

	CONST
		LoRadius = 150;
		HiRadius = 160;
		YBot = 425;
		XtalYNr = 8;   {number of atoms in each column}
		XtalXNr = 4;   {number of atoms in each row }
                            { Lattice spacings Ni: 3.51 A, Al: 4.05 A }
		Ni111x = 2.0265; { Ni: in  111 dir : lattice const / sqrt(3) }
		Ni111y = 2.1494; { Ni:perp 111 dir : lattice const x sqrt(3/8) }
		Al111x = 2.33827;{ Al: in  111 dir : lattice const / sqrt(3) }
		Al111y = 2.48010;{ Al:perp 111 dir : lattice const x sqrt(3/8) }
		Alhk0xy = 2.025;  { Al: half of lattice const for (hk0) scatt }
		SpacingLiF = 2.014; {d[100]; fcc w/ lattice constant of 4.028 A}
		MaxAmpl = 80; { on stripchart }
		wlline = 10; { line to print wavelength }

	PROCEDURE DrawTargets;
	forward;

	PROCEDURE SetHotKeys;
	BEGIN{SetHotKeys}
		WITH HotKeys DO
		BEGIN
			IF NOT SingleCrystal THEN {Thomson}
			BEGIN
				Init(6);
				key[1] := 'F1-Help';
				key[2] := 'F2-MoreHelp';
				key[3] := 'F5-AngScan';
				key[4] := 'F6-Planes';
				key[5] := 'F7-NewXtals';
				key[6] := 'F10-Menu';
			END
			ELSE IF NOT XRays THEN
			BEGIN
				Init(6);
				key[1] := 'F1-Help';
				key[2] := 'F2-VScanHelp';
				key[3] := 'F5-AngScan';
				key[4] := 'F6-NrPlanes';
				key[5] := 'F7-VScan';
				key[6] := 'F10-Menu';
			END
			ELSE  {X-Ray Diffractometer}
			BEGIN
				Init(3);
				key[1] := 'F1-Help';
				key[2] := 'F5-AngScan';
				key[3] := 'F10-Menu';
			END;
			Display;
		END;
	END; {SetHotKeys}

	PROCEDURE VoltageScan;
	forward;

	PROCEDURE NewCrystallites;
	forward;

	PROCEDURE HandleHotKeys;
	BEGIN{HandleHotKeys}
		IF NOT SingleCrystal THEN {Thomson}
			CASE key OF
				1:	ThomsonInfo;
				2:	MoreThomsonInfo;
				3:	AngScan := true;
				4: BEGIN
						hk0 := not hk0;
						DrawTargets;
					END;
				5: NewCrystallites;
				6: SetEventEscape;
			END {case}
		ELSE IF XRays THEN
			CASE key OF
				1:	XRayInfo;
				2:	AngScan := true;
				3:	SetEventEscape;
			END {case}
		ELSE
			CASE key OF
				1:	DavissonInfo;
				2:	VoltageInfo;
				3:	AngScan := true;
				4: BEGIN
						face := not Face;
						DrawTargets;
					END;
				5: VoltageScan;
				6: SetEventEscape;
			END {case}
	END; {HandleHotKeys}

	PROCEDURE SetSliders;
	BEGIN{SetSliders}
		WriteGr(LAnglTextX, LAnglY, '                  ', blanc);
		InitSliders;
		sliders.drawAll;
		IF SingleCrystal THEN
			IF XRays THEN
				VoltVal := CVoltSX + FVoltSX
			ELSE
				VoltVal := CVoltS + FVoltS
		ELSE
			VoltVal := VoltValM;
		wlength := Wavelength(VoltVal);
		WriteGr(WlengthTextX, TotalVoltageY, 'Total Voltage', lightgray);
		IF VoltVal > 1000 THEN
			WriteGr(WlengthValX, TotalVoltageY, NumStr(VoltVal, 7, 0), lightred)
		ELSE
			WriteGr(WlengthValX, TotalVoltageY, NumStr(VoltVal, 7, 2), lightred);
		IF XRays THEN
			WriteGr(WlengthTextX, WlengthMsgY, '   X-Ray wavelength(A):', lightgray)
		ELSE
			WriteGr(WlengthTextX, WlengthMsgY, 'Electron wavelength(A):', lightgray);
		WriteGr(WlengthValX, WlengthMsgY, NumStr(wlength, 7, 4), lightred);
	END; {SetSliders}

	PROCEDURE SelectCrystallites;
		VAR
			i: integer;
	BEGIN{SelectXtallites}
   	FOR i := 1 TO NrCrystals DO
		BEGIN
			Anglet[i] := +CenterAngle + 0.10 * (random - 0.5);
			z[i].x := round(50.0 * (random - 0.5));
			z[i].y := round(50.0 * (random - 0.5));
		END;
		z[1].x := 0;
		z[1].y := 0; {reference xtal}
		IF NrCrystals = 1 THEN
			Anglet[1] := +CenterAngle;
	END; {SelectXtallites}

	PROCEDURE DrawTargets;
		VAR
			i, s, c, l, w, mag: integer;
			angl: real;
	BEGIN{DrawTargets}
		IF SingleCrystal THEN
		BEGIN
 { draw target 40 in y, 30 in x}
			angl := TargAng;
 {40 in y, 30 in x}
			w := 53;
			l := 127;
			mag := 50;
			IF (NOT XRays) AND (face) THEN
			BEGIN
 {40 in y, 8 in x}
				w := 84;
				l := 96;
				mag := 40;
			END;
			p1.x := CtrX + round(mag * cos(conv * (angl - l)));
			p1.y := CtrY - round(mag * sin(conv * (angl - l)));
			p2.x := CtrX + round(mag * cos(conv * (angl + l)));
			p2.y := CtrY - round(mag * sin(conv * (angl + l)));
			p3.x := CtrX + round(mag * cos(conv * (angl + w)));
			p3.y := CtrY - round(mag * sin(conv * (angl + w)));
			p4.x := CtrX + round(mag * cos(conv * (angl - w)));
			p4.y := CtrY - round(mag * sin(conv * (angl - w)));
			ClearArea(CtrX - 50, CtrY - 50, CtrX + 50, CtrY + 50); {clear old area}
			DrawQuad;
		END
		ELSE {G. P. Thomson}
		BEGIN
			FOR i := 1 TO NrCrystals DO
			BEGIN
				an := -conv * Anglet[i];
				s := round(30 * sin(an));
				c := round(30 * cos(an));
				cx := CtrX + z[i].x;
				cy := CtrY + z[i].y;
				p1.x := cx + c + s;
				p1.y := cy - (-s + c);
				p2.x := cx + c - s;
				p2.y := cy - (-s - c);
				p3.x := cx - c - s;
				p3.y := cy - (+s - c);
				p4.x := cx - c + s;
				p4.y := cy - (+s + c);
				DrawQuad;
			END;
			IF hk0 THEN
				WriteGr(0.28, 0.65, '(hk0)', white)
			ELSE
				WriteGr(0.28, 0.65, '(111)', white);
		END;
	END;{DrawTargets}

	PROCEDURE NewCrystallites;
		VAR
			v: STRING[6];
	BEGIN {NewCrystallites}
		SetFillStyle(SolidFill, noir);
 {MBar(0,0,299,299);}
		ChooseVP(TargetsVP);
		ClearVP;
		ChooseVP(FullVP);
		SelectCrystallites;
		DrawTargets;

		ChooseVP(FullVP);
	END;  {NewCrystallites}

	PROCEDURE DrawDetector (LoAngle, HiAngle: real);
	BEGIN{DrawDetector}
		p1.x := CtrX + round(LoRadius * cos(LoAngle));
		p1.y := CtrY - round(LoRadius * sin(LoAngle));
		p2.x := CtrX + round(HiRadius * cos(LoAngle));
		p2.y := CtrY - round(HiRadius * sin(LoAngle));
		p3.x := CtrX + round(HiRadius * cos(HiAngle));
		p3.y := CtrY - round(HiRadius * sin(HiAngle));
		p4.x := CtrX + round(LoRadius * cos(HiAngle));
		p4.y := CtrY - round(LoRadius * sin(HiAngle));
		DrawQuad;
	END; {DrawDetector}

	PROCEDURE Get_f1_f2;
	BEGIN{Get_f1_f2}
   {sum over a column}
		IF abs(DeltaPhCol) < 1.0E-3 THEN
			f1 := NrXtlY
		ELSE
			f1 := sin(NrXtlY * DeltaPhCol) / sin(DeltaPhCol);
   {sum over a row}
		IF abs(DeltaPhRow) < 1.0E-3 THEN
			f2 := NrXtlX
		ELSE
			f2 := sin(NrXtlX * DeltaPhRow) / sin(DeltaPhRow);
	END;{Getf1f2}

	PROCEDURE PlotIntensity (m: integer);
		VAR
			SinAn: real;
	BEGIN{PlotIntensity}
 { strip chart for detector angle and xtal angle }
 { ang = detector angle; TargAng = target angle }
 { want xtal angle alpha to be positive for ccw rotations}
 { an:=conv*ang;}
 { get first factor using y-dist, adding amplitudes along a column}
 { del = theta/2 - phi }
		IF XRays THEN
			del := 0.0
		ELSE
			del := an - conv * TargAng;
		DeltaPhCol := ColG2 * cos(del);
		DeltaPhRow := RowG2 * sin(del);
		Get_f1_f2;
		x := f * sqr(f1 * f2);
		IF x > MaxAmpl THEN
			x := MaxAmpl;
		setcolor(yellow);
		Mline(k + m, YBot - 5, k + m, Ybot - 5 - round(x));
	END; {PlotIntensity}

	PROCEDURE AngleScan;
		VAR
			i, m: integer;
	BEGIN{AngleScan}
		MouseOff;
		ChooseVP(FullVP);
		ClearArea(0, 0, 375, 400);
		IF XRays THEN
			DrawCrystal(0.0)
		ELSE
			DrawTargets;
		SetFillStyle(SolidFill, DarkGray);
		MBar(20, YBot - 7 - MaxAmpl, 375, YBot);
      SetColor(white);
		Rectangle(20, YBot - 7 - MaxAmpl, 375, YBot);
		Setcolor(brown);
		Mline(20, Ybot - 5, 375, Ybot - 5);
		BeamLabel;
		IF SingleCrystal THEN
			v := NumStr(StartAngS, 5, 1)
		ELSE
			v := NumStr(StartAngM, 5, 1);
		WriteGr(ScanLoX, ScanMsgY, v, lightgray);
		IF SingleCrystal THEN
			v := NumStr(EndAngS, 5, 1)
		ELSE
			v := NumStr(EndAngM, 5, 1);
		WriteGr(ScanHiX, ScanMsgY, v, lightgray);
		WriteGr(ScanMsgX, ScanMsgY, 'Scan angle (degrees)', lightgray);

		IF SingleCrystal THEN
			IF XRays THEN
				VoltVal := CVoltSX + FVoltSX
			ELSE
				VoltVal := CVoltS + FVoltS
		ELSE
			VoltVal := VoltValM;

		wlength := Wavelength(VoltVal);   {wavelength in angstroms}
		g := pi / wlength;
		IF SingleCrystal THEN {DGermer}
		BEGIN
			RowG := g * Ni111x;
			ColG := g * Ni111y;
			NrXtlX := XtalXNr;
			NrXtlY := XtalYNr;
			IF (NOT XRays) AND (face) THEN
				NrXtlX := 1;
			step := (EndAngS - StartAngS) / (XScale * 340);
			ang := StartAngS
		END
		ELSE {G. P.Thomson}
		BEGIN
			IF hk0 THEN
			BEGIN
       {RowG: scatt btw elements of a row}
				RowG := g * Alhk0xy;
				ColG := RowG;
			END
			ELSE
			BEGIN
				RowG := g * Al111x;
				ColG := g * Al111y;
			END;
			step := (EndAngM - StartAngM) / (Xscale * 340);
			ang := StartAngM;
			NrXtlX := XtlNr;
			NrXtlY := XtlNr;
		END;
		IF XRays THEN   { (100) plane of LiF }
		BEGIN
			RowG := g * SpacingLiF;
			ColG := RowG;
		END;

		DeltaPhCol := 0.0;
		DeltaPhRow := 0.0;
		Get_f1_f2;
		f := gain * MaxAmpl / sqr(f1 * f2); {amplitude normalization}
		fs := gain * MaxAmpl / sqr(NrXtallites * sqr(XtlNrX));  {amplitude normalization}

		k := round(XScale * 199);
		FOR m := round(-170 * XScale) TO round(XScale * 169) DO
		BEGIN
    {step scatt angle 'ang' at end of this loop}
			an := 0.5 * conv * ang; {theta/2}
			CosAn2 := cos(an);
			SinAn2 := sin(an); {cos,sin of theta/2}
			ColG2 := 2.0 * ColG * SinAn2;
			RowG2 := 2.0 * RowG * SinAn2;
			LoAngle := conv * (ang - 5); {angle limits}
			HiAngle := conv * (ang + 5);
			DrawDetector(LoAngle, HiAngle);
			dlay(20);
			IF XRays THEN
				DrawCrystal(an);
			IF SingleCrystal THEN
				PlotIntensity(m)
			ELSE {Thomson crystallites}
			BEGIN
				x := 0.0;
				y := 0.0;
				FOR i := 1 TO NrCrystals DO
				BEGIN
					del := an - conv * Anglet[i]; { 7/19/94 theta/2-alpha}

					DeltaPhCol := ColG2 * cos(del);   { for f1 }
					DeltaPhRow := RowG2 * sin(del);
					;  { for f2 }

					bigphase := 2 * g * (z[i].x * (CosAn2 - 1) + z[i].y * SinAn2) + (XtlNr - 1) * (DeltaPhCol + DeltaPhRow);
					Get_f1_f2;
					x := x + f1 * f2 * cos(bigphase);
					y := y + f1 * f2 * sin(bigphase);
				END;
				x := fs * (sqr(x) + sqr(y));
				IF x > MaxAmpl THEN
					x := MaxAmpl;
			END;

			SetLineStyle(SolidLn, 0, NormWidth);
			Setcolor(yellow);
			Mline(k + m, Ybot - 5, k + m, Ybot - 5 - round(x));

			x := abs(Ang);
			j := round(x);
			SetColor(LightGray);
			IF abs(x - 10 * (j DIV 10)) <= 0.2 THEN {10ø markers}
				Mline(k + m, YBot - 3, k + m, YBot);
			IF abs(x - 5 * (j DIV 5)) <= 0.2 THEN { 5ø markers}
				Mline(k + m, YBot - 1, k + m, YBot);
			IF abs(x - j) <= 0.2 THEN             { 1ø markers}
				MLine(k + m, YBot, k + m, YBot);
			IF abs(x - 30 * (j DIV 30)) <= 0.2 THEN {30ø markers}
			BEGIN
				setcolor(blanc);
				Mline(k + m, YBot - 3, k + m, YBot);
			END;
			IF abs(x - 90 * (j DIV 90)) <= 0.2 THEN {90ø markers}
			BEGIN
				setcolor(noir);
				Mline(k + m, YBot - 3, k + m, YBot);
			END;
			IF abs(x) <= 0.2 THEN { 0ø marker}
			BEGIN
				setcolor(noir);
				Mline(k + m, YBot - 3, k + m, YBot);
			END;

			IF graphresult <> 0 THEN
				beep;
			ang := ang + step;
                                   {detector sweep}
		END;
		MouseOn;
	END;{AngleScan}

	PROCEDURE VoltageScan;
		VAR
			VoltSqrt, InvWlength, FacX, FacY: real;
			m: integer;
	BEGIN{VoltageScan}
		NrXtlX := XtalXNr;
		NrXtlY := XtalYNr;
		MouseOff;
		ChooseVP(FullVP);
		ClearArea(0, 0, 375, 400);
		DrawTargets;
		SetFillStyle(SolidFill, DarkGray);
      SetColor(white);
		MBar(20, YBot - 7 - MaxAmpl, 375, YBot);
		Rectangle(20, YBot - 7 - MaxAmpl, 375, YBot);
		Setcolor(brown);
		Mline(20, Ybot - 5, 375, Ybot - 5);
		BeamLabel;
		setcolor(green);
		Mline(185, 110, 185, 45);
		Mline(185, 45, 175, 45);
		Mline(175, 45, 195, 35);
		Mline(215, 45, 195, 35);
		Mline(205, 45, 215, 45);
		Mline(205, 110, 205, 45);
		VoltSqrt := sqrt(CVoltS + FVoltS);
		step := 3.0 * VoltSqrt / (XScale * 340);
		v := NumStr(VoltSqrt, 5, 1);
		WriteGr(ScanLoX, ScanMsgY, v, lightgray);
		v := NumStr(4.0 * VoltSqrt, 5, 1);
		WriteGr(ScanHiX, ScanMsgY, v, lightgray);
		WriteGr(ScanMsgX, ScanMsgY, ' Square Root of Voltage', lightgray);

		DeltaPhCol := 0.0;
		DeltaPhRow := 0.0;
		Get_f1_f2;
		f := gain * MaxAmpl / sqr(f1 * f2); {amplitude normalization}

		ang := 90.0;
		an := 0.5 * conv * ang; {theta/2}
		CosAn2 := cos(an);
		SinAn2 := sin(an); {cos,sin of theta/2}
		LoAngle := conv * (ang - 5);
		HiAngle := conv * (ang + 5);
		DrawDetector(LoAngle, HiAngle);
		k := round(XScale * 199);
		FacX := 2.0 * SinAn2 * Pi * Ni111x;
		FacY := 2.0 * SinAn2 * Pi * Ni111y;
		FOR m := round(-170 * XScale) TO round(XScale * 169) DO
		BEGIN
			x := sqr(VoltSqrt);
			InvWlength := 1.0 / Wavelength(x); {wavelength in angstroms}
			RowG2 := InvWLength * FacX;
			ColG2 := InvWLength * FacY;
			PlotIntensity(m);
			SetLineStyle(SolidLn, 0, NormWidth);
			x := abs(VoltSqrt);
			j := round(x);
			SetColor(LightGray);
			IF abs(x - 10 * (j DIV 10)) <= 0.2 THEN {10 marker}
				Mline(k + m, YBot - 3, k + m, YBot);
			IF abs(x - 5 * (j DIV 5)) <= 0.2 THEN { 5 marker}
				Mline(k + m, YBot - 1, k + m, YBot);
			IF abs(x - j) <= 0.2 THEN             { 1 marker}
				MLine(k + m, YBot, k + m, YBot);
			IF graphresult <> 0 THEN
				beep;

			VoltSqrt := VoltSqrt + step;
			dlay(20);
                                  {end û V  sweep}
		END;
		MouseOn;
	END; {VoltageScan}

	PROCEDURE UpdateFromSliders;
	BEGIN{UpdateFmSliders}
		CheckForEvents;
		NotChanged := true;
		IF Sliders.Changed THEN
		BEGIN
        {hold previous values}
			NotChanged := False;
			OldNrCrystals := NrCrystals;
			OldCenterAngle := CenterAngle;
			OldXtlNr := XtlNr;
			OldTargAng := TargAng;

			IF SingleCrystal THEN
			BEGIN
				IF NOT XRays THEN {DG}
				BEGIN
					CVoltS := Sliders.Value(CVoltSl);
					FVoltS := Sliders.Value(FVoltSl);
					VoltValS := CVoltS + FVoltS;
					TargAng := Sliders.Value(TDirSl);
				END
				ELSE {Xrays}
				BEGIN
					CVoltSX := Sliders.Value(CVoltSl);
					FVoltSX := Sliders.Value(FVoltSl);
					VoltValS := CVoltSX + FVoltSX;
				END;
           {Both DG and XRays}
				StartAngS := Sliders.Value(AStartSl);
				EndAngS := Sliders.Value(AEndSl);
				SelectCrystallites;
			END
			ELSE   {G. P. Thomson}
			BEGIN
				CVoltM := Sliders.Value(CVoltSl);
				FVoltM := Sliders.Value(FVoltSl);
				VoltValM := CVoltM + FVoltM;
				StartAngM := Sliders.Value(AStartSl);
				EndAngM := Sliders.Value(AEndSl);
				NrXtallites := Sliders.Value(XtalliteSl);
				NrCrystals := round(NrXtallites);
				XtlNrX := round(Sliders.Value(XtlNrSl));
				XtlNr := round(XtlNrX);
				CenterAngle := Sliders.Value(CntrAngSl);
			END;
			Gain := Sliders.Value(GainSl);
			IF SingleCrystal THEN
				VoltVal := VoltValS
			ELSE
				VoltVal := VoltValM;
			wlength := Wavelength(VoltVal);
			ChooseVP(FullVP);
			IF VoltVal > 1000 THEN
				WriteGr(WlengthValX, TotalVoltageY, NumStr(VoltVal, 7, 0), lightred)
			ELSE
				WriteGr(WlengthValX, TotalVoltageY, NumStr(VoltVal, 7, 2), lightred);
			WriteGr(WlengthValX, WlengthMsgY, NumStr(wlength, 7, 4), lightred);
		END;{Sliders.Changed}
	END; {UpdateFmSliders}

BEGIN{Experiment}
	conv := Pi / 180.0;
	e1 := e / (4 * Me * 8.99E16);     {for relativistic correction}
	TopDisplay := False;
	MouseOff;
	SelectViewPort(FullVP);
	ClearVP;
	SetHotkeys;
	SetSliders;
	IF NOT SingleCrystal THEN SelectCrystallites;
	IF XRays THEN DrawCrystal(0.0)
	ELSE DrawTargets;
	MouseOn;
	REPEAT
		GoTop := false;
		AngScan := false;

		REPEAT
			UpDateFromSliders;
		UNTIL NotChanged;
		IF (OldNrCrystals <> NrCrystals) OR (OldCenterAngle <> CenterAngle) OR (OldXtlNr <> XtlNr) THEN
		BEGIN
			OldNrCrystals := NrCrystals;
			OldCenterAngle := CenterAngle;
			OldXtlNr := XtlNr;
			SelectCrystallites;
			NewCrystallites;
		END;
		IF OldTargAng <> TargAng THEN
		BEGIN
			OldTargAng := TargAng;
			DrawTargets;
		END;

		IF Hotkeys.pressed(key) THEN
			HandleHotKeys;
		IF AngScan THEN
			AngleScan;
	UNTIL GoTop;
	Hotkeys.Clear;
   SetFrontKeys;
	Sliders.Done;
END; {Experiment}


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
		row(1, 5, 'About Sliders');
		row(1, 6, '--------------');
		row(1, 7, 'Exit Program  ');
      rowactivate(1,6, false);
		column(2, 'Runs');
		row(2, 1, 'Davisson-Germer scattering');
		row(2, 2, 'G. P. Thomson scattering');
		row(2, 3, 'X-Ray Diffractometer');
		row(2, 4, '-------------------------');
		row(2, 5, 'Set Default Slider Values');
      rowactivate(2,4, false);
	END;
END;{SetTopMenu}

PROCEDURE SetDefaults;
forward;

PROCEDURE HandleTopMenu;
	VAR
		i: integer;
BEGIN{HandleTopMenu}

	WITH TopMenu DO
		CASE colChosen OF
			1:	CASE rowchosen OF
					1:	AboutCups;
					2:	OpeningScreen;
					3:	Configuration;
					4:	ReminderInfo;
					5:	SliderUseInfo;
					7:	ReturnFlag := TRUE;
				END; {case}
			2:	CASE rowChosen OF
					1: {D-G}
					BEGIN
						SingleCrystal := true;
						XRays := false;
						Experiment;
					END;
					2: {G.P.Thomson}
					BEGIN
						SingleCrystal := false;
						Xrays := false;
						Experiment;
					END;
					3: {X-Rays}
					BEGIN
						SingleCrystal := true;
						XRays := true;
						Experiment;
					END;
					5:
					BEGIN
						Sliders.Done;
						SetDefaults;
						InitSliders;
					END;
				END; {case}
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
		1:	ReminderInfo;
		2:	IF TopMenu.Chosen THEN
				HandleTopMenu;
	END; {case}
END; {HandleFrontKeys}


PROCEDURE SetDefaults;
BEGIN {SetDefaults}

	XScale := GetMaxX / 639;
	YScale := GetMaxY / 479;

 	PCVersion := true;
	noir := black;
	blanc := white;

    {screen-coords for outxy labels}

	WlengthTextX := 0.60;
	WlengthValX := 0.90;
	WlengthMsgY := 0.73;
	TotalVoltageY := 0.77;

	BeamInX := 0.001;
	BeamInY := 0.662;

	ScanMsgY := 0.10;
	ScanLoX := 0.001;
	ScanMsgX := 0.18;
	ScanHiX := 0.55;

	LAnglY := 0.48;
	LAnglTextX := 0.60;
	LAnglValX := 0.75;

	heading := blanc;
	CVoltSl := 1; { slider for accelerating voltage }
	FVoltSl := 2; { fine adjust for V }
	AStartSl := 3; { slider starting scan angle }
	AEndSl := 4; { slider ending scan angle }
	GainSl := 5; { gain slider}
	TDirSl := 6; { slider for target angle }
	XtalliteSl := 6;
	XtlNrSl := 7;
	DGColSl := 7;
	CntrAngSl := 8;

	CVoltS := 40.0;
	FVoltS := 0.0;
	VoltValS := CVoltS + FVoltS;
	CVoltSX := 6.0E4;
	FVoltSX := 0.0;
	CVoltM := 3.0E2;
	FVoltM := 309.0;
	VoltValM := CVoltM + FVoltM;
	TargAng := 0.0;
	CenterAngle := 55.0;
	CAngleMin := 0;
	CAngleMax := 90;
	StartAngS := 0;
	EndAngS := +170;
	StartAngM := -35;
	EndAngM := +35;
	Gain := 1.0;
	hk0 := true;
	NrXtallites := 1;
	NrCrystals := round(NrXtallites);
	XtlNrX := 6.0;
	XtlNr := round(XtlNrX);
	PlotFlag := false;
	TopDisplay := false;
	ValidPlot := FALSE;
	ReturnFlag := FALSE;
	ChooseVP(FullVP);
	ClearVP;
	SetTopMenu;
	TopMenu.Display;
	TopDisplay := TRUE;
	SetFrontKeys;
END; {SetDefaults}

PROCEDURE GermerMain;
BEGIN{GermerMain}
	SetDefaults;
	BeamLabel;
	DrawCrystal(0.0);
	OpeningScreen;

	REPEAT
		MouseOn;
		CheckForEvents;

		IF (NOT TopDisplay) THEN
		BEGIN
			ChooseVP(FullVP);
			ClearVP;
			TopDisplay := true;
			TopMenu.Display;
			FrontKeys.Display;
		END;
		IF FrontKeys.pressed(key) THEN
			HandleFrontKeys;
		IF TopMenu.Activated THEN
			HandleTopMenu;
	UNTIL ReturnFlag;
	TopMenu.Done;
END;{GermerMain}

 {======================= M A I N    P R O G R A M   =====================}
 {======================= M A I N    P R O G R A M   =====================}
BEGIN{Main Pgm}
	CupsInit; {  CUPSInit; turns mouse on, registers egavga}
	DefineViewport(FullVP, 0.0, 1.0, 0.0, 1.0);  {full screen}
	DefineViewport(TargetsVP, 0.0, 0.48, 0.375, 1.0);
	GermerMain;
	CupsDone;
END.{Program}