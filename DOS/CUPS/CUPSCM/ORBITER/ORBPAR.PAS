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
	{**            Unit OrbPar.Pas               **}
	{**          Program CentralF.Pas            **}
	{**          Central Force Orbits            **}
	{**        Written by Bruce Hawkins          **}
	{**           May 19, 1993                   **}
	{**        Last change:  2/10/95             **}
	{**             Version 1.00                 **}
	{**                                          **}
	{**      B. Hawkins Copyright @ 1992         **}
	{**             CUPS Project                 **}
	{**                                          **}
	{**     Uses 12/95 MUPPET & CUPS Utilities   **}
	{**********************************************}
	{**********************************************}

	 {Contains all parmameter initialization}
Unit OrbPar;

Interface

Uses
	Cups, Graph, CupsProc, CupsFunc, CupsGui, CUPSMupp;

PROCEDURE SetAllFalse;
PROCEDURE SaveThePlots;
PROCEDURE RestorePlots;
PROCEDURE SetScale;
PROCEDURE SetFrameNameTable;
PROCEDURE EMSParameters;
PROCEDURE ShuttleParameters;
PROCEDURE JupiterCometParameters;
PROCEDURE JupiterMoonsParameters;
PROCEDURE SolarSystemParameters;
PROCEDURE LagrangeParameters;
PROCEDURE BinaryParameters;
PROCEDURE RetrogradeParameters;
PROCEDURE ParInit;

CONST
	MaxHeavy = 5;  {Allow up to 5 massive bodies}
   NumNames = 25;
VAR
	MainMenu, HelpMenu : TMenu;
	HistoryLength : LongInt; {Must be < size of History Array}
   HistoryCount, RotNumber : Integer;
	History : Array[1..4000] of DVector;
	Xh, Yh, Rh_3: Array[1..Maxheavy] Of DVector;
	M: Array[1..MaxHeavy] Of Real;
	MainKeys, RunKeys: THotKeys;
	InputModeCartesian, AddingBody: Boolean;
	DataBody, DataCenterBody, ShowEnergyBody : Integer;
	TwoPi: real;
	Version: Real;
	PlotCofM, PlotBody1, PlotBody2, PlotBody3, PlotPoincare,
	PlotUniverse, PlotRotating: Boolean;
	SavePlots : Array[1..7] of Boolean;

	Plots: Integer;
	Zf, ZFact: Real;

	Zoomed, Running, Wrapped, Replicated, NewPicked, AutoReplay, Tracking, 
	ShowBodies, ShowCofM, DrawSightLine, ThrustAllowed, AutoNextReplay,
	Param1Exist, ClearIt: Boolean;

	NColor, TimeDirection, DotSize: Integer;

	Thrust, SavT, HThrust, SavHT, VThrust, SavVt,
	TimeStep, TimeStepUsed, MaxStep,
	MultipleSpread: Real; {Used as a factor}

	DotInterval, DotGap: Integer;

	SaveEnergy, SaveLBodyEnergy, SaveNt, 
	RelativeError, AbsError, CofMNormScale, Body1NormScale, 
	Body2NormScale, Body3NormScale, UnivNormScale, DataNormScale, 
	PoincareNormScale, VPScale, RotateNormScale, AllNormScale,
	RotNormCenterX, RotNormCenterY, PoinNormCenterX, PoinNormCenterY,
	UnivNormCenterX, UnivNormCenterY: Real;

	XCMSav: Array[1..7] Of Real;
	YCMSav: Array[1..7] Of Real;

	PeriName, ApeName: String[8];
	NBodies, N2Bodies, NBodiesOld, PoinBody, ThrustBody: Integer;
	NHeavy, N2Heavy, NHeavyOld, NHeavyPrime: Integer;
	Param1Name: String[10];
	Param1, PCriterion : Real;
	Body1Name, Body2Name, Body3Name: String[16];
	SysName: String[30];
	R1NameIndex, R2NameIndex: Integer;
	ScaleTable, CenterTableX, CentertableY: Array[1..7] Of Real;
	FrameNameTable: Array[1..7] Of String[35];
	NameTable: Array[1..NumNames] Of String[20];
   NameOrderTable : Array[1..NumNames] Of Integer;
	LengthName, TimeName: String[4];
	MassName: String[7];
	GName: String[30];
	LightEnergyName: String[10];
	ColorB: Array[1..NumNames] Of Integer;
	G: Real;
	R12, R23, V12, V23: Real;
	Xinit, Yinit, VxInit, VyInit: Array[1..NumNames] Of Real;

IMPLEMENTATION
CONST
	MyVersion = 1.00;

PROCEDURE SetAllFalse;
BEGIN
   PlotCofM := False;
   PlotBody1 := False;
   PlotBody2 := False;
   PlotBody3 := False;
   PlotUniverse := False;
   PlotRotating := False;
   PlotPoincare := False;
END; {SetAllFalse}

PROCEDURE SaveThePlots;
BEGIN
   SavePlots[1] := PlotCofM;
   SavePlots[2] := PlotBody1;
   SavePlots[3] := PlotBody2;
   SavePlots[4] := PlotBody3;
   SavePlots[5] := PlotUniverse;
   SavePlots[6] := PlotRotating;
   SavePlots[7] := PlotPoincare;
END; {SaveThePlots}

PROCEDURE RestorePlots;
BEGIN
   PlotCofM := SavePlots[1];
   PlotBody1 := SavePlots[2];
   PlotBody2 := SavePlots[3];
   PlotBody3 := SavePlots[4];
   PlotUniverse := SavePlots[5];
   PlotRotating := SavePlots[6];
   PlotPoincare := SavePlots[7];
END; {RestorePlots}

Procedure SetFrameNameTable;
{******** ScaleValues order must be the same as PlotList (in SetPlots)  ******}
BEGIN
	FrameNameTable[1] := 'Center of Mass Window';
	FrameNameTable[2] := Concat(Nametable[1] , ' Window');
	FrameNameTable[3] := Concat(Nametable[2] , ' Window');
	FrameNameTable[4] := Concat(Nametable[3] , ' Window');
	FrameNameTable[5] := 'Universe Window';
	FrameNameTable[6] := Concat('Rotating Window (' , Nametable[R1NameIndex],
								  ' & ' , Nametable[R2NameIndex] , ')');
	If PoinBody > 0 THEN
	FrameNameTable[7] := 'Poincar‚ of '+NameTable[PoinBody]+' (Vx vs X at Y=0)';
END; {SetFrameNameTable}

PROCEDURE SetScale;
 {******** ScaleValues order must be the same as PlotList (in SetPlots)  ******}
VAR
   i : Integer;
BEGIN
	For i := 1 to 7 DO
   BEGIN
      Scaletable[i] := AllNormScale;
      CenterTableX[i] := 0.0; CenterTableY[i] := 0.0
   END;
   IF CofMNormScale > 0.0 THEN ScaleTable[1] := CofMNormScale;
   IF Body1NormScale > 0.0 THEN ScaleTable[2] := Body1NormScale;
   IF Body2NormScale > 0.0 THEN ScaleTable[3] := Body2NormScale;
   IF Body3NormScale > 0.0 THEN ScaleTable[4] := Body3NormScale;
   IF UnivNormScale > 0.0 THEN ScaleTable[5] := UnivNormScale;
	IF RotateNormScale > 0.0 THEN ScaleTable[6] := RotateNormScale;
   IF PoincareNormScale > 0.0 THEN ScaleTable[7] := PoincareNormScale;
   CenterTableX[5] := UnivNormCenterX; CenterTableY[5] := UnivNormCenterY;
   CenterTableX[6] := RotNormCenterX; CenterTableY[6] := RotNormCenterY;
   CenterTableX[7] := PoinNormCenterX; CenterTableY[7] := PoinNormCenterY
END; {SetScale}

Procedure EMSParameters;
BEGIN
   NBodies := 3; N2Bodies := 6; NHeavy := 3; N2Heavy := 6;
	PoinBody := 3;
	MainMenu.row(2,10, 'Allow Thrust');
	ThrustBody := 0; {Not Allowed} MainMenu.rowActivate(2,10,False);
	ThrustAllowed := False;
   NHeavyPrime := NHeavy; NbodiesOld := NBodies; NHeavyOld := NHeavy;
	Param1Exist := False;
	SysName := 'Earth Moon & Sun System';

   {Assign half of available memory to history (history is half size of Y)}
	HistoryLength := Memavail*2 DIV (9*NBodies*Sizeof(Real));
	If HistoryLength > 6000 THEN HistoryLength := 4000;

	SetAllFalse;
   Plots :=4;
   PlotCofM := True;
   PlotBody1 := True;
   PlotBody2 := True;
   PlotRotating := True;
   DrawSightLine := False;
   Zf := 1.5;

   AllNormScale := 1.5;
	Body1NormScale := 0.075; {Sun: show detail of magnified orbits}
	Body2NormScale := 0.015; {Earth: Show moon}
	RotateNormScale := 0.017;
	PoincareNormScale := 0.017; VPScale := 1.0/20.0; PCriterion := 0.0;
   RotNormCenterX := 1.0; RotNormCenterY := 0.0;
   PoinNormCenterX := 1.0; PoinNormCenterY := 0.0;
   SetScale; {Zeros all entries in Centertables, Sets UnivCenter and RotCenter}
	CenterTableX[2] := 0.395; CenterTableY[2] := 0.91;

	LengthName := 'AU';
	PeriName := 'Perigee'; ApeName := 'Apogee';
   TimeName := 'Year';
	MassName := 'M_Sun';
	GName := 'AU^3/(M_Sun*Year^2)';

	Body1Name := 'Sun';   NameTable[1] := Body1Name;
   Body2Name := 'Earth'; NameTable[2] := Body2Name;
	Body3Name := 'Moon';  NameTable[3] := Body3Name;
   R1NameIndex := 1;
   R2NameIndex := 2;
   LightEnergyName := 'Moon En.'; ShowEnergyBody := 3;
	DataBody := 3; DataCenterBody := 2;
	SetFrameNameTable;

   ColorB[1] := Yellow; ColorB[2] := Green; ColorB[3] := LightCyan;

	TimeStep := 0.001;
	G := Pwr(TwoPi,2); {Au^3/(M_sun*Year^2)}

   M[1] := 1.0;  M[2] := 3.005027E-6;  M[3] := 3.690297E-8; {M_sun}
   R12 := 0.983; R23 := 0.002571191;         {Astronomical Units (Au)}
	V12 := 6.391; V23 :=  0.21839; {-0.21839;}  {Au/Year; circular orbits}
                         {v23:=+0.016613 is beautiful pattern}
	UnivNormCenterX := 0.0; UnivNormCenterY := 0.0;
END; {EMSParameters}

Procedure ShuttleParameters;
VAR SAngle : Real;
BEGIN
   NBodies := 3; N2Bodies := 6; NHeavy := 2; N2Heavy := 4;
	PoinBody := 0; PCriterion := 0.0;
	MainMenu.row(2, 10, 'Refuse Thrust');
	ThrustBody := 3; MainMenu.rowActivate(2,10,True);
	ThrustAllowed :=  True; Thrust := 10; SavT := Thrust;
	NHeavyPrime := NHeavy; NbodiesOld := NBodies; NHeavyOld := NHeavy;
	Param1Exist := False;
	SysName := 'Shuttle Docking';

   {Assign half of available memory to history (history is half size of Y)}
   HistoryLength := Memavail*2 DIV (9*NBodies*Sizeof(Real));
	If HistoryLength > 4000 THEN HistoryLength := 4000;

	SetAllFalse;
	Plots :=2;
	PlotBody1 := True;
	PlotRotating := True;
	DrawSightLine := False;
	Zf := 0.92;
	DotSize := 2;

   AllNormScale := 12000.0;
	RotateNormScale := 200.0;
	RotNormCenterX := 6538.0; RotNormCenterY := 0.0;
   SetScale; {Zeros all entries in Centertables, Sets UnivCenter and RotCenter}

	PeriName := 'Perigee'; ApeName := 'Apogee';
	LengthName := 'km';
   TimeName := 'Hour';
   MassName := 'kg';
   GName := 'km^3/(kg*Hour^2)';

   Body1Name := 'Earth';   NameTable[1] := Body1Name;
   Body2Name := 'Station'; NameTable[2] := Body2Name;
	Body3Name := 'Atlantis';  NameTable[3] := Body3Name;
	R1NameIndex := 1;
   R2NameIndex := 2;
   LightEnergyName := 'Atlan. En.'; ShowEnergyBody := 3;
	DataBody := 3; DataCenterBody := 1;
	SetFrameNameTable;

   ColorB[1] := Green; ColorB[2] := LightBlue; ColorB[3] := LightMagenta;

	TimeStep := 0.001;
   G := 8.6417E-13; {km^3/(kg*Hour^2)}

   M[1] := 5.977E24; {kg} M[2] := 1E7; {kg}
   R12 := 6538.0; {km}
	V12 := 28107.28; {km/hr}
   SAngle := 0.01; {radians}

   UnivNormCenterX := 0.0; UnivNormCenterY := 0.0;

   Xinit[1] := 0.0; Yinit[1] := 0.0;
	Xinit[2] := R12; Yinit[2] := 0.0;
	Xinit[3] := R12*Cos(SAngle); Yinit[3] := -R12*Sin(SAngle);
   VxInit[1] := 0.0; VyInit[1] := 0.0;
   VxInit[2] := 0.0; VyInit[2] := V12;
   VxInit[3] := V12*Sin(SAngle); VyInit[3] := V12*Cos(Sangle);

END; {ShuttleParameters}

PROCEDURE JupiterCometParameters;
BEGIN
   NBodies := 3; N2Bodies := 6; NHeavy := 2; N2Heavy := 4;
	PoinBody := 3; VPScale := 1.0; PCriterion := 0.0;
   PoinNormCenterX := 0.0; PoinNormCenterY := 0.0;
   MainMenu.row(2,10, 'Allow Thrust');
	ThrustBody := 3; {Comet}  Thrust := 0.1;  SavT := Thrust;
	MainMenu.rowActivate(2,10,True);
	ThrustAllowed := False;
   NHeavyPrime := NHeavy; NbodiesOld := NBodies; NHeavyOld := NHeavy;
   RelativeError := 1.0e-6;
   AbsError := 1.0e-6;

	{Assign half of available memory to history (history is half size of Y)}
   HistoryLength := Memavail*2 DIV (9*NBodies*Sizeof(Real));
	If HistoryLength > 4000 THEN HistoryLength := 4000;

   SetAllFalse;
   Plots := 4;
   PlotCofM := True;
	PlotBody2 := True;
   PlotRotating := True;
   PlotPoincare := True;
	DrawSightLine := False;
   Zf := 1.5;

   AllNormScale := 18.0;
   SetScale; {Zeros all entries in Centertables, Sets UnivCenter and RotCenter}

	PeriName := 'Perihel.'; ApeName := 'Aphel.';
	LengthName := 'AU';
   TimeName := 'Year';
   MassName := 'M_Sun';
   GName := 'AU^3/(M_Sun*Year^2)';

   Body1Name := 'Sun';     NameTable[1] := Body1Name;
   Body2Name := 'Jupiter'; NameTable[2] := Body2Name;
	Body3Name := 'Comet';    NameTable[3] := Body3Name;
	SysName := 'Jupiter & Comet';
   R1NameIndex := 1;
   R2NameIndex := 2;
	LightEnergyName := 'Comet En.'; ShowEnergyBody := 3;
	DataBody := 3; DataCenterBody := 1;
	Param1Name := ' ';
	SetFrameNameTable;

   ColorB[1] := Yellow; ColorB[2] := LightBlue; ColorB[3] := LightRed;

  TimeStep := 0.1;
   G := Pwr(TwoPi,2); {Au^3/(M_sun*Year^2)}
   Param1 := 0.0;

	M[1] := 1.0;  M[2] := 3.0359140000E-03;
	R12 := 4.953256;  {Astronomical Units (Au)} {Needed for eccentricity calc}
	V12 := 2.893;     {Au/Year; eccentricity 0.048}

	Yinit[1] := -1.4032862010E-02; Xinit[1] := -6.3388265607E-03;
	Yinit[2] := 4.6222857465;      Xinit[2] := 2.0879466812;
	Yinit[3] := 3.2985943432;      Xinit[3] := -6.1447425557;

	VyInit[1] := 7.6223466416E-03; VxInit[1] := 7.6223466416E-03;
	VyInit[2] := 1.2633779129;     VxInit[2] := -2.5107254822;
   VyInit[3] := 2.3886740953E-02; VxInit[3] := -2.4034409119;

END; {JupiterCometParameters}

PROCEDURE JupiterMoonsParameters;
VAR
   R24, V24, R25, V25, V26, R26 : Real;
BEGIN
   NBodies := 6; N2Bodies := 12; NHeavy := 2; N2Heavy := 4;
	PoinBody := 3; PCriterion := 0.0;
   MainMenu.row(2,10, 'Allow Thrust');
	ThrustBody := 6; Thrust := 0.1;  SavT := Thrust;
	MainMenu.rowActivate(2,10,True);
	ThrustAllowed := False;
   NHeavyPrime := NHeavy; NbodiesOld := NBodies; NHeavyOld := NHeavy;
   RelativeError := 1.0e-3;
   AbsError := 1.0e-3;

   {Assign half of available memory to history (history is half size of Y)}
   HistoryLength := Memavail*2 DIV (9*NBodies*Sizeof(Real));
	If HistoryLength > 4000 THEN HistoryLength := 4000;

   SetAllFalse;
   Plots :=4;
   PlotCofM := True;
	PlotBody1 := True;
   PlotBody2 := True;
   PlotRotating := True;
   DrawSightLine := False;
   Zf := 1.5;

   AllNormScale := 7.5;
   Body1NormScale := 0.03;
	Body2NormScale := 0.015; {Magnify to show Moons}
   RotateNormScale := 0.03;
	PoincareNormScale := 0.04; VPScale := 0.1;
   RotNormCenterX := 4.97; RotNormCenterY := 0.0;
   PoinNormCenterX := 4.98; PoinNormCenterY := 0.0;
   SetScale; {Zeros all entries in Centertables, Sets UnivCenter and RotCenter}
   CenterTableX[2] := -0.04; CenterTableY[2] := 4.95; {Center sun frame on Jupiter}

	PeriName := 'Perihel.'; ApeName := 'Aphel.';
	LengthName := 'AU';
   TimeName := 'Year';
   MassName := 'M_Sun';
   GName := 'AU^3/(M_Sun*Year^2)';

   Body1Name := 'Sun';     NameTable[1] := Body1Name;
   Body2Name := 'Jupiter'; NameTable[2] := Body2Name;
   Body3Name := 'Io';   NameTable[3] := Body3Name;
   NameTable[4] := 'Europa'; NameTable[5] := 'Ganymede';
	NameTable[6] := 'Callisto';
	SysName := 'Jupiter''s Moons';
   R1NameIndex := 1;
   R2NameIndex := 2;
   LightEnergyName := 'Io Ener.'; ShowEnergyBody := 3;
	DataBody := 3; DataCenterBody := 2;
   Param1Name := ' ';
   SetFrameNameTable;

   ColorB[1] := Yellow; ColorB[2] := LightGreen; ColorB[3] := LightCyan;
   ColorB[4] := LightRed; ColorB[5] := LightMagenta; ColorB[6] := LightBlue;

   TimeStep := 0.0001;
   G := Pwr(TwoPi,2); {Au^3/(M_sun*Year^2)}
	Param1 := 0.0;

   M[1] := 1.0;  M[2] := 9.5500E-4;
   R12 := 4.953256;  {Astronomical Units (Au)}
	V12 := 2.893;     {Au/Year; eccentricity 0.048}
   R23 := 2.8196E-3; {Io}
   V23 := 3.6567;    {Circular}
   R24 := 4.4862E-3; {Europa}
   V24 := 2.8990;
	R25 := 7.1559E-3; {Ganymede}
   V25 := 2.2954;
   R26 := 12.5865E-3; {Callisto}
   V26 := 1.7307;

   Yinit[1] := 0.0; Xinit[1] := 0.0;
   Yinit[2] := R12; Xinit[2] := 0.0;
   Yinit[3] := R12 + R23; Xinit[3] := 0.0;
   Yinit[4] := R12 ;      Xinit[4] := R24;
	Yinit[5] := R12 - R25; Xinit[5] := 0.0;
   Yinit[6] := R12;       Xinit[6] := -R26;

   VyInit[1] := 0.0; VxInit[1] := 0.0;
   VyInit[2] := 0.0; VxInit[2] := -V12;
   VyInit[3] := 0.0; VxInit[3] := -V12 - V23;
   VyInit[4] := V24; VxInit[4] := -V12;
   VyInit[5] := 0.0; VxInit[5] := -V12 + V25;
   VyInit[6] := -V26; VxInit[6] := -V12;

END; {JupiterMoonsParameters}

PROCEDURE SolarSystemParameters;
VAR
   R24, V24, R25, V25, V26, R26, R27, V27,
   R28, V28, V29, R29, R210, V210, Sqt2 : Real;
BEGIN
   MainMenu.row(2,10, 'Allow Thrust');
	ThrustBody := 0; MainMenu.rowActivate(2,10,False);
	ThrustAllowed := False;
	NBodies := 10; N2Bodies := 20; NHeavy := 3; N2Heavy := 6;
	PoinBody := 0; PCriterion := 0.0;
   NHeavyPrime := NHeavy; NbodiesOld := NBodies; NHeavyOld := NHeavy;
   RelativeError := 1.0e-6;
   AbsError := 1.0e-6;

   {Assign half of available memory to history (history is half size of Y)}
   HistoryLength := Memavail*2 DIV (9*NBodies*Sizeof(Real));
	If HistoryLength > 4000 THEN HistoryLength := 4000;

   SetAllFalse;
   Plots :=2;
   PlotCofM := True;
	PlotBody1 := True;
   DrawSightLine := False;
   Zf := 0.92;

   AllNormScale := 40.0;
   Body1NormScale := 2.00;
   SetScale; {Zeros all entries in Centertables, Sets UnivCenter and RotCenter}

	PeriName := 'Perihel.'; ApeName := 'Aphel.';
	LengthName := 'AU';
   TimeName := 'Year';
   MassName := 'M_Sun';
   GName := 'AU^3/(M_Sun*Year^2)';

   Body1Name := 'Sun';     NameTable[1] := Body1Name;
   Body2Name := 'Jupiter'; NameTable[2] := Body2Name;
   Body3Name := 'Saturn';   NameTable[3] := Body3Name;
   NameTable[4] := 'Mercury'; NameTable[5] := 'Venus';
   NameTable[6] := 'Earth'; NameTable[7] := 'Mars';
	NameTable[8] := 'Uranus'; NameTable[9] := 'Neptune';
	NameTable[10] := 'Pluto';
   {Display Names of Heavy Bodies in proper order of orbital size}
   NameOrderTable[2] := 4; NameOrderTable[3] := 5; NameOrderTable[4] := 6;
   NameOrderTable[5] := 7; NameOrderTable[6] := 2; NameOrderTable[7] := 3;
	SysName := 'Solar System';

   R1NameIndex := 1;
   R2NameIndex := 2;
   LightEnergyName := 'Pluto En.'; ShowEnergyBody := 10;
	DataBody := 10; DataCenterBody := 1;
   Param1Name := ' ';
   SetFrameNameTable;

   ColorB[1] := Yellow; ColorB[2] := LightMagenta; ColorB[3] := LightCyan;
   ColorB[4] := LightRed; ColorB[5] := LightBlue; ColorB[6] := LightGreen;
   ColorB[7] := LightRed; ColorB[8] := LightBlue; ColorB[9] := LightGreen;
   ColorB[10] := White;
   ShowBodies := False;

   TimeStep := 0.01;
   G := Pwr(TwoPi,2); {Au^3/(M_sun*Year^2)}
   Param1 := 0.0;

   M[1] := 1.0;  M[2] := 9.5500E-4; M[3] := 2.86E-4;

   R12 := 4.951338;  {Astronomical Units (Au) Jupiter}
	V12 := 2.891132;     {Au/Year; eccentricity 0.048332, a = 5.2028}
   R23 := 9.00681; {Saturn e = 0.055890, a = 9.540}
	V23 := 2.1513;
	R24 := 0.3075056; {Mercury e = 0.205615, a = 0.387099}
   V24 := 12.441068;
   R25 := 0.71839; {Venus e = 0.006820, a = 0.723332}
   V25 := 7.43829;
   R26 := 0.98325; {Earth e = 0.016750, a = 1.000000}
   V26 := 6.389325;
	R27 := 1.38151; {Mars e = 0.093312, a = 1.52369}
   V27 := 5.58952;
   R28 := 18.276;  {Uranus e = 0.0471, a = 19.18}
   V28 := 1.5039;
   R29 := 29.814; {Neptune e = 0.0085, a = 30.07}
   V29 := 1.1556;
	R210:= 29.603; {Pluto e = 0.2494, a = 39.44}
   V210:= 1.2907;

   Sqt2 := Sqrt(2);
   Yinit[1] := 0.0;  Xinit[1] := 0.0;
   Yinit[2] := R12;  Xinit[2] := 0.0;
   Yinit[3] := -R23; Xinit[3] := 0.0;
   Yinit[4] := 0.0;  Xinit[4] := R24;
   Yinit[5] := 0.0;  Xinit[5] := -R25;
   Yinit[6] := R26/Sqt2;  Xinit[6] := R26/Sqt2;
   Yinit[7] := -R27/Sqt2;  Xinit[7] := R27/Sqt2;
   Yinit[8] := R28/Sqt2;  Xinit[8] := -R28/Sqt2;
   Yinit[9] := -R29/Sqt2;  Xinit[9] := -R29/Sqt2;
   Yinit[10] := -R210/Sqt2;  Xinit[10] := R210/Sqt2;

   VyInit[1] := 0.0; VxInit[1] := 0.0;
   VyInit[2] := 0.0; VxInit[2] := -V12;
   VyInit[3] := 0.0; VxInit[3] :=  V23;
   VyInit[4] := V24; VxInit[4] :=  0.0;
   VyInit[5] := -V25; VxInit[5] := 0.0;
   VyInit[6] :=  V26/Sqt2; VxInit[6] := -V26/Sqt2;
   VyInit[7] :=  V27/Sqt2; VxInit[7] :=  V27/Sqt2;
   VyInit[8] := -V28/Sqt2; VxInit[8] := -V28/Sqt2;
   VyInit[9] := -V29/Sqt2; VxInit[9] :=  V29/Sqt2;
   VyInit[10] :=  V210/Sqt2; VxInit[10] :=  V210/Sqt2;

END; {SolarSystemParameters}

Procedure LagrangeParameters;
BEGIN
   NBodies := 5; N2Bodies := 10; NHeavy := 2; N2Heavy := 4;
   MainMenu.row(2,10, 'Allow Thrust');
	ThrustBody := 0; MainMenu.rowActivate(2,10,False);
	PoinBody := 3; {PCriterion := 0.0; Value set below}
	ThrustAllowed := False;
   NHeavyPrime := NHeavy; NbodiesOld := NBodies; NHeavyOld := NHeavy;
   Param1Exist := True;

   {Assign half of available memory to history (history is half size of Y)}
   HistoryLength := Memavail*2 DIV (9*NBodies*Sizeof(Real));
	If HistoryLength > 4000 THEN HistoryLength := 4000;

   SetAllFalse;
   Plots :=2;
   PlotRotating := True;
   PlotBody2 := True;
   DrawSightLine := False;
   Zf := 0.92;

	AllNormScale := 7.5;
	Body2NormScale := 18.0; Body3NormScale := 18.0;
	PoincareNormScale := 0.40; VPScale := 1.0;

	PeriName := 'Perihel.'; ApeName := 'Aphel.';
	LengthName := 'Au';
   TimeName := 'Year';
   MassName := 'M_Sun';
   GName := 'AU^3/(M_Sun*Year^2)';

   Body1Name := 'Sun';     NameTable[1] := Body1Name;
   Body2Name := 'Jupiter'; NameTable[2] := Body2Name;
   Body3Name := 'Body at 60'+Chr(248);   NameTable[3] := Body3Name;
	NameTable[4] := 'Body at 90'+Chr(248);
	NameTable[5] := 'Body at 180'+Chr(248);
	SysName := 'Jupiter Lagrangian Points';
	R1NameIndex := 1;
	R2NameIndex := 2;
	LightEnergyName := 'C->60'+Chr(248)+' En.'; ShowEnergyBody := 3;
	DataBody := 3; DataCenterBody := 1;
	Param1Name := 'Offset';
	SetFrameNameTable;

	ColorB[1] := Yellow; ColorB[2] := LightGreen; ColorB[3] := LightCyan;
   ColorB[4] := LightRed; ColorB[5] := LightMagenta;

   TimeStep := 0.1;
   G := Pwr(TwoPi,2); {Au^3/(M_sun*Year^2)}
   Param1 := 0.001; {Offset from exact Lagrangian Points}

   M[1] := 1.0;  M[2] := 9.5500E-4; {3.05914E-3; actual Jup, not stable}
   R12 := 4.953256;                {Astronomical Units (Au)}
	V12 := 2.893;                   {Au/Year; circular orbits}

   Xinit[1] := 0.0; Yinit[1] := 0.0;
   Xinit[2] := R12; Yinit[2] := 0.0;
   Xinit[3] := R12* Cos(TwoPi/6.0)*(1.0-Param1); 
   Yinit[3] := R12 * Sin(TwoPi/6.0)*(1.0-Param1); {At 60 deg, stable}
   Xinit[4] := 0.0; Yinit[4] := R12*(1.0-Param1);           {At 90 deg, not an equilibrium}
   Xinit[5] := -R12*(1.0-Param1); Yinit[5] := 0.0;          {Opposite Jupiter, unstable equilibrium}

   PCriterion := Yinit[3] + 0.2; {Most motion is above initial position}
   PoinNormCenterX := Xinit[3]+0.1; PoinNormCenterY := 0.0;
   SetScale; {Zeros all entries in Centertables, Sets UnivCenter and RotCenter}

   VxInit[1] := 0.0; VyInit[1] := 0.0;
   VxInit[2] := 0.0; VyInit[2] := V12;
   VxInit[3] := -V12* Sin(TwoPi/6.0); VyInit[3] := V12 * Cos(TwoPi/6.0);
   VxInit[4] := -V12; VyInit[4] := 0.0;
   VxInit[5] := 0.0; VyInit[5] := -V12;

END; {LagrangeParameters}

Procedure BinaryParameters;
BEGIN
   NBodies := 3; N2Bodies := 6; NHeavy := 3; N2Heavy := 6;
	PoinBody := 3; VPScale := 0.01; PCriterion := 0.0;
   PoinNormCenterX := 0.0; PoinNormCenterY := 0.0;
   MainMenu.row(2,10, 'Allow Thrust');
	ThrustBody := 3; Thrust := 0.1;  SavT := Thrust;
	MainMenu.rowActivate(2,10,True);
	ThrustAllowed := False;
   NHeavyPrime := NHeavy; NbodiesOld := NBodies; NHeavyOld := NHeavy;
   Param1Exist := False;

   {Assign half of available memory to history (history is half size of Y)}
   HistoryLength := Memavail*2 DIV (9*NBodies*Sizeof(Real));
	If HistoryLength > 4000 THEN HistoryLength := 4000;

   SetAllFalse;
   Plots :=4;
   PlotCofM := True;
   PlotBody1 := True;
   PlotBody2 := True;
   PlotRotating := True;
   DrawSightLine := False;
	Zf := 1.5;

   AllNormScale := 0.8;
   UnivNormScale := 5.0*AllNormScale;
   UnivNormCenterX := 0.0; UnivNormCenterY := -0.7*UnivNormScale;
   Body1NormScale := 1.2;
   Body2NormScale := 1.2; Body3NormScale := 1.2;
   SetScale; {Zeros all entries in Centertables, Sets UnivCenter and RotCenter}

	PeriName := 'PeriAstr.'; ApeName := 'ApAstr.';
	LengthName := 'AU';
   TimeName := 'Year';
   MassName := 'M_Sun';
   GName := 'AU^3/(M_Sun*Year^2)';

   Body1Name := 'Star A'; NameTable[1] := Body1Name;
   Body2Name := 'Star B'; NameTable[2] := Body2Name;
	Body3Name := 'Comet';  NameTable[3] := Body3Name;
	SysName := 'Binary Star & Comet';
	R1NameIndex := 1;
	R2NameIndex := 2;
   LightEnergyName := 'Comet En.'; ShowEnergyBody := 3;
	DataBody := 3; DataCenterBody := 2;
	SetFrameNameTable;

   ColorB[1] := Yellow; ColorB[2] := LightMagenta; ColorB[3] := LightCyan;

   TimeStep := 0.001;
   G := Pwr(TwoPi,2); {Au^3/(M_sun*Year^2)}

   M[1] := 1.0;  M[2] := 1.0;  M[3] := 5.0E-8; {M_sun}
   R12 := 1.0; R23 := -0.3;         {Astronomical Units (Au)}
	V12 := TwoPi; V23 :=  8.0; {Au/Year; circular orbits}
END; {BinaryParameters}

PROCEDURE RetrogradeParameters;
BEGIN
   NBodies := 3; N2Bodies := 6; NHeavy := 3; N2Heavy := 6;
	PoinBody := 0; PCriterion := 0.0;
   MainMenu.row(2,10, 'Allow Thrust');
	ThrustBody := 0; MainMenu.rowActivate(2,10,False);
	ThrustAllowed := False;
   NHeavyPrime := NHeavy; NbodiesOld := NBodies; NHeavyOld := NHeavy;
   Param1Exist := False;

   {Assign half of available memory to history (history is half size of Y)}
   HistoryLength := Memavail*2 DIV (9*NBodies*Sizeof(Real));
	If HistoryLength > 4000 THEN HistoryLength := 4000;

   SetAllFalse;
   Plots :=2;
   PlotCofM := True;
   PlotBody2 := True;
   DrawSightLine := True;
   Zf := 0.92;
   
	AllNormScale := 10.0;
   RotNormCenterX := 0.0; RotNormCenterY := 0.0;
   SetScale; {Zeros all entries in Centertables, Sets UnivCenter and RotCenter}
   
	PeriName := 'Perihel.'; ApeName := 'Aphel.';
	LengthName := 'AU';
   TimeName := 'Year';
   MassName := 'M_Sun';
	GName := 'AU^3/(M_Sun*Year^2)';

	Body1Name := 'Sun';    NameTable[1] := Body1Name;
	Body2Name := 'Earth';  NameTable[2] := Body2Name;
	Body3Name := 'Jupiter'; NameTable[3] := Body3Name;
	SysName := 'Jupiter Retrograde Motion';
   R1NameIndex := 1;
   R2NameIndex := 2;
	LightEnergyName := 'Jup. En.'; ShowEnergyBody := 3;
	DataBody := 3; DataCenterBody := 1;
   SetFrameNameTable;

   ColorB[1] := Yellow; ColorB[2] := Green; ColorB[3] := LightCyan;

   TimeStep := 0.01;
   G := Pwr(TwoPi,2); {Au^3/(M_sun*Year^2)}

	M[1] := 1.0;  M[2] := 3.035914E-6;  M[3] := 3.035914E-3; {M_sun}
	R12 := 0.983; R23 := 3.970256;         {Astronomical Units (Au)}
	V12 := 6.391; V23 := -3.498;   {Au/Year; circular orbits}
END; {RetrogradeParameters}

PROCEDURE ParInit;
BEGIN
	Version := MyVersion;
END;

END.