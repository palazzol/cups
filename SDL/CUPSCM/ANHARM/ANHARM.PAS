{ M 45000,0,655360}
	 {**********************************************}
	 {**********************************************}
	 {**           Program AnHarm.Pas             **}
	 {**      Non-Linear Oscillator Behaviour     **}
	 {**        Written by Bruce Hawkins          **}
	 {**           November 28, 1991              **}
	 {**        Last change:  2/15/94             **}
	 {**             Version 1.00                 **}
	 {**                                          **}
	 {**      B. Hawkins Copyright @ 1991         **}
	 {**             CUPS Project                 **}
	 {**                                          **}
	 {**                                          **}
	 {**     Uses 12/94 MUPPET & CUPS Utilities   **}
	 {**********************************************}
	 {**********************************************}

{******************************************************************}
{*                                                                *}
{*         PHYSICS is in Unit AnhPhy.Pas                          *}
{*                                                                *}
{*                                                                *}
{*                                                                *}
{*                                                                *}
{*    Computes the trajectory of various anharmonic oscillators   *}
{*   in one dimension using fourth order Runge Kutta integration  *}
{*                                                                *}
{*          Dissipation and sinusoidal driving forces             *}
{*                 can be turned on and off                       *}
{*                                                                *}
{*               Position-time and phase graphs                   *}
{*                 Poincar‚ and return maps                       *}
{*                                                                *}
{*     Animation provides visual correlation of the graphs        *}
{*   with the actual motion of the system amd with each other     *}
{*                                                                *}
{*       Variation of period with energy and parameters           *}
{*                can be calculated and displayed                 *}
{*                                                                *}
{*       An informal partial list of known deficiencies           *}
{*     and intended enhancements to the prototype follows         *}
{*                                                                *}
{******************************************************************}

PROGRAM AnHarm;
USES Dos, Crt, Graph, Cups, CUPSMupp, CupsFunc, CupsProc, CupsGui, CupsGrph,
		AnhPhy, AnhMenu, Anhtutor, AnhHelp;

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
	MyVersion : Real = 1.00;

PROCEDURE SpeedTest;
VAR i, Hour, Min, Sec, Sec100 : Word;
	Sec100b, Sec100a : LongInt;
BEGIN
	GetTime(Hour, Min, Sec, Sec100);
   i := 0;
	Sec100a := LongInt(6000)*Min+100*Sec + Sec100;
	REPEAT i := i + 1; GetTime(Hour, Min, Sec, Sec100);
		Sec100b:= longint(6000)*Min+100*Sec + Sec100;
	UNTIL Sec100b >= Sec100a + 50;
	IF i > 3000 THEN
		MasterDelayTime := 100
	ELSE
		MasterDelayTime := (i) DIV 30;
	IF MasterDelayTime < 0 THEN MasterDelayTIme := 0
END; {SpeedTest}
{1998 to 2346 on a 386 (20 MHz?); 1676 to 2035 on 386SX (20MHz?) }

Procedure AnHarmInitialize;
BEGIN
	If MyVersion > Version THEN Version := MyVersion;
	TwoPi := 2*Pi;
	AccelP := Accel;
	CUPSinit;
	SpeedTest;
	xpos.init(41);
   potential.init(41);
	xactpotential.init(41);
	SetUpPorts;
   SetUpMenus;
	WhichForce := Walls; {Uninitialized variable would crash in choosewalls}
   ChooseWalls;
	SetPlots
END; {AnHarmInitialize}

Procedure DisplayInitialize;
BEGIN
	ShowMenu := True;
   SetAllFalse;
	Plots := 2;
	PlotTime := True;
	PlotPhase := True;
	SetPlots;
	NewSystem({Clear: } True);
	DisplayChoice;
	RunInstruct;
	DrawSystem;
	Message('Press ESC to proceed immediately to Title Screen');
	ShowCursor;
	NColor := 10;
	Continuous := False;
	SetMyColor(rainbow[0].colors[NColor]);
	ShowTrajectory;
	DisplayMainMenu;
	ShowCursor
END; {DisplayInitialize}

 BEGIN {Program}
	AnHarmInitialize;
	DisplayInitialize;
	Help(OpenScreen);
	REPEAT
		CheckForEvents;
		IF MainMenu.Activated THEN HandleMenu;
		IF Event.ReadKey = Chr(27) THEN ClearPhasePort;
		IF Not NewEnergyScale THEN ChangePotential; {Via keys}
      MoveWall; {and change potential}
      PickEnergy; {And plot a trajectory}
	UNTIL false;
END. {Program}