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
	 {**          Program Colision.Pas            **}
	 {**          Two Body Collisions             **}
	 {**        Written by Bruce Hawkins          **}
	 {**           March 22, 1993                 **}
	 {**        Last change:  2/ 1/95             **}
	 {**             Version 1.00                 **}
	 {**                                          **}
	 {**      B. Hawkins Copyright @ 1993         **}
	 {**             CUPS Project                 **}
	 {**                                          **}
	 {**                                          **}
	 {**     Uses 12/94 MUPPET & CUPS Utilities   **}
	 {**********************************************}
	 {**********************************************}

{******************************************************************}
{*                                                                *}
{*    Displays collision trajectories in lab and c of systems     *}
{*    Calculates cross sections                                   *}
{*                                                                *}
{*          The motion may be viewed from one or several          *}
{*                 reference frames simultaneously                *}
{*                                                                *}
{*     Tutorial explains cross sections and reference systems     *}
{*                                                                *}
{******************************************************************}

PROGRAM Collision;
USES   CupsMupp, Graph, Cups, CupsFunc, CupsProc, CupsGui, CupsGrph,
		 ColMenus, ColPhy, ColTutor;

CONST
	MyVersion = 1.00;

Procedure CollisionInitialize;
BEGIN
	RegisterBGIFont(@TriplexFontProc);
	RegisterBGIFont(@SmallFontProc);
	If MyVersion > Version THEN Version := MyVersion;
	CUPSinit;
	HardSphereParameters;  {Initialze all constants}
	SoftSphereParameters;
	Woods_Saxon_Parameters;
	TruncatedParameters(False);
	ShieldedParameters(False);
	CoulombParameters(False);
	kc := 0.1; k0 := 0.1;
   NumberOfReps := 40;
   SaveNReps := 40;
	R0 := 0.1; r00 := 0.1;
   Power := 2;
	Yukawa_Parameters;
   OldForce := Yukawa;
	InitializeParameters;
	PreviousForce({SwapForces= } False);
	XScalMaxLog := -1E6; XScalMinLog := 1E6;
   XScalMax := -1E6; XScalMin := 1E6;
	DefineMyViewPort (AllPort, 0.0, 1.0, 0.05, 0.95);
	DefineMyViewPort (DemoPort, 0.60, 0.99, 0.24, 0.77);
	DefineMyViewPort (One, 0.22, 0.78, 0.16, 0.88);
	DefineMyViewPort (BigXS, 0.12, 0.678, 0.16, 0.89);
	DefineMyViewPort (LTextPort, 0.00, 0.59, 0.06, 0.95);
	DefineMyViewPort (RNamePort, 0.781, 1.00, 0.30, 0.70);
	DefineMyViewPort (Name2Port, 0.57, 0.98, 0.785, 0.85);
	DefineMyViewPort (Name4Port, 0.57, 0.96, 0.931, 0.97);

	DefineMyViewPort (LeftPort, 0.07, 0.48, 0.23, 0.78);
	DefineMyViewPort (RightPort, 0.57, 0.98, 0.23, 0.78);
	DefineMyViewPort (XSectPort, 0.57, 0.96, 0.23, 0.78);

	DefineMyViewPort (BigUpperLeft, 0.07, 0.46, 0.55, 0.93);
	DefineMyViewPort (BigUpperRight, 0.57, 0.96, 0.55, 0.93);
	DefineMyViewPort (BigLowerLeft, 0.07, 0.46, 0.12, 0.50);
	DefineMyViewPort (BigLowerRight, 0.57, 0.96, 0.12, 0.50);

	DefineMyViewPort (InitDistPort1, 0.34, 0.70, 0.085,0.117);
	DefineMyViewPort (InitDistPort2, 0.60, 0.90, 0.07, 0.09);
	DefineMyViewPort (AnglePort, 0.34, 0.70, 0.05, 0.09);
	DefineMyViewPort (ParamPort, 0.40, 0.80, 0.865, 0.965);

	CofMPort := LeftPort;
	LabPort := RightPort
END; {CollisionInitialize}

PROCEDURE NoSystem(VAR Done : BOOlean);
BEGIN
	Done := False;
	Message('Not Available Yet')
END;
CONST
	Cx1 = 0.15; Cx2 = 0.22; Cy1 = 0.18; Cy2 = 0.12;

VAR
	RepCount : Integer;
	Angle : Real;
	FirstTime : Boolean;

BEGIN {Program}
	CollisionInitialize;
	SetUpMenus;
	SetUpMainKeys;
   SetUpRunKeys;
   GraphBackColor := darkGray;
	ShowTrajectories;
	XScaleDef := XScale;
	YScaleDef := YScale;
	PxScaleDef := PxScale;
	PyScaleDef := PyScale;
	DotIntDef := DotInterval;
	TimeStepDef := TimeStep;
	ShowCursor;
	MainMenu.RowChecked(3,Ord(OldForce)+1, True);
	RepCount := 1;
	DelayTime := 40; {Cups Unit Variable}
   ErasePast := False;
	FirstTime := True;
	Running := True; {show one collision before Title Screen shows}
	REPEAT
		CheckForEvents;
		IF MainMenu.Activated THEN HandleMenu;
		If MainKeys.Pressed(key) THEN DoMainKeys(key);
		If (Running OR PickImpactParam) THEN InitializeParameters;
		IF Quit THEN BEGIN QuitRun; END;
		If Running THEN
		BEGIN
			HideCursor;
         IF ShowEffective AND Not Varying THEN DisplayEffectivePotential;
			ShowOrbit(Angle,True);
			MainMenu.RowActivate(1,9,True);
			IF Not Varying THEN
			BEGIN
				RepCount := 1;
				MainMenu.Display;
			END
		END;
		IF Varying THEN SpreadIt(RepCount);
		IF FirstTime THEN
		BEGIN
			Help(OpenScreen);
			FirstTime := False;
         DelayTime := 0
		END;
		ShowCursor
	UNTIL false;
END. {Program}