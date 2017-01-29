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
	 {**          Program Orbiter.Pas             **}
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

{******************************************************************}
{*                                                                *}
{*    Computes the trajectory of various combinations of objects  *}
{*    moving in two dimensions under gravitational forces         *}
{*                                                                *}
{*          The motion may be viewed from one or several          *}
{*                 reference frames simultaneously                *}
{*                                                                *}
{*               Either two or three massive bodies               *}
{*               and any umber of non-massive ones                *}
{*           the latter don't exert forces ont he former          *}
{*                                                                *}
{*   Retrograde motion is illustrated and sight lines drawn       *}
{*                                                                *}
{*       One system show lagrangian point stability               *}
{*                                                                *}
{*       An informal partial list of known deficiencies           *}
{*     and intended enhancements to the prototype follows         *}
{*                                                                *}
{******************************************************************}

PROGRAM Orbiter;
USES   CupsMupp, Graph, Cups, CupsFunc, CupsProc, CupsGui, CupsGrph,
		 OrbMenus, OrbPhy, OrbPar, OrbHelp;

CONST
	MyVersion = 1.00;

Procedure CentralFInitialize;
BEGIN
	If MyVersion > Version THEN Version := MyVersion;
	ParInit;
	PhyInit;
	MenuInit;
	CUPSinit;
	SetUpMenus;
	BinaryParameters;
	ThreeBodyInit; LagrangeParameters; GeneralInit;
	DefineMyViewPort (OneFull, 0.045, 0.955, 0.12, 0.92);  {MyViewPort allows > 10}
	DefineMyViewPort (One, 0.06, 0.65, 0.11, 0.91);  {MyViewPort allows > 10}

	DefineMyViewPort (LeftPort, 0.035, 0.465, 0.19, 0.815);
	DefineMyViewPort (RightPort, 0.535, 0.965, 0.19, 0.815);

	DefineMyViewPort (BigUpperLeft, 0.05, 0.46, 0.535, 0.90);
	DefineMyViewPort (BigUpperRight, 0.56, 0.96, 0.535, 0.90);
	DefineMyViewPort (BigLowerLeft, 0.05, 0.46, 0.12, 0.485);
	DefineMyViewPort (BigLowerRight, 0.56, 0.96, 0.12, 0.485);

	DefineMyViewPort (UpperLeft, 0.05, 0.32, 0.535, 0.90);
	DefineMyViewPort (UpperMid, 0.37, 0.64, 0.535, 0.90);
	DefineMyViewPort (UpperRight, 0.69, 0.96, 0.535, 0.90);
	DefineMyViewPort (LowerLeft, 0.05, 0.32, 0.12, 0.485);
	DefineMyViewPort (LowerMid, 0.37, 0.64, 0.12, 0.485);
	DefineMyViewPort (LowerRight, 0.69, 0.96, 0.12, 0.485);

	DefineMyViewPort (ChangePort, 0.658, 0.992, 0.11, 0.91);
	DefineMyViewPort (ClearD1Port, 0.53, 0.97, 0.20, 0.55);
	DefineMyViewPort (ClearD2Port, 0.25, 0.75, 0.85, 0.91);

	DefineMyViewPort (Mod1Port, 0.17, 0.97, 0.08, 0.10);
	DefineMyViewPort (Mod2Port, 0.67, 0.96, 0.57, 0.82);
   DefineMyViewPort (AllPort, 0.0, 1.0, 0.06, 0.95);

	DefineMyViewPort (ClearTimePort, 0.01, 0.97, 0.08, 0.10);
	DefineMyViewPort (ClearVCircPort, 0.50, 0.85, 0.08, 0.10);
	DefineMyViewPort (ClearThrustPort, 0.01, 0.99, 0.86, 0.94);
	DefineMyViewPort (MidNamePort, 0.0, 1.0, 0.485, 0.535);
	DefineMyViewPort (LowNamePort, 0.0, 1.0, 0.05, 0.18);
	DefineMyViewPort (LowerNamePort, 0.0, 1.0, 0.07, 0.11);

	CofMPort := One;
	DataPort := ChangePort
END; {CentralFInitialize}

PROCEDURE Tutorial;
BEGIN
	GraphBackColor := Red;
	OpenMyViewPort(AllPort);
	SetUserCharSize(1,1,3,2);
   Print(20,4,'Gravitational Orbits');
   SetUserCharSize(1,2,2,3);
   Print (15,10, 'The ''Systems'' section of the HELP menu has suggestions');
   Print (15,11, 'for interesting things to do with each system.');
   Print (20, 16, 'Press ENTER or click Mouse.');
   StaticPause
END; {Tutorial}

PROCEDURE NoSystem(VAR Done : BOOlean);
BEGIN
   Done := False;
   Message('Not Available Yet')
END;

PROCEDURE IntroScreen;
VAR Done : Boolean;
BEGIN
	GraphBackColor := darkGray;
	ChangeSystem(Solar);
	SetUpRunKeys;
	SetUpMainKeys;
{M   ShowCursor}
END; {IntroScreen}

Procedure ShowOrbit;
VAR
   iplot, iframe, i,j, k, i3, i4, i5, i6, i12,
      count, IFlag, DupCount, Gx, Gy, SightLineCount : Integer;
   h, temp1, temp2, temp3, xdir, ydir : Real;

   PROCEDURE CheckErrors;
   BEGIN
      Case IFlag of
         1 : ; {No error; continue}
         2 : BEGIN
             Message ('Bodies came too close; integration lost precision');
             StaticPause;
             Running := False;
             MainMenu.Display
             END;
         3 : BEGIN
             Message ('Illegal input values to integration routine');
             Running := False;
             MainMenu.Display
             END;
         4 : BEGIN
             Message ('Integration step size is too small: Decrease precison');
             StaticPause;
             Running := False;
             MainMenu.Display
             END;
         ELSE 
				BEGIN
				  Message ('Undetermined error in integration routine');
              StaticPause;
				  Running := False;
				  MainMenu.Display
				  END;
		END; {Case}
	END; {CheckErrors}

	PROCEDURE CheckEvents;
	BEGIN
	  CheckForEvents;
	  IF RunKeys.Pressed(key) THEN DoRunKeys(key) ELSE
	  If Event.MouseClicked THEN
		  BEGIN
		  MainKeys.key[2] := 'F2-Go';
		  Running := Not Running;
		  Debounce;
		  MainKeys.Display
		  END;
		IF ThrustAllowed AND Event.extendedKey THEN
      BEGIN
         temp1 := Y.Value(N2Bodies+2*ThrustBody-1);
			temp2 := Y.Value(N2Bodies+2*ThrustBody);
         temp3 := Sqrt(Sqr(temp1) + Sqr(temp2));
			xdir := Thrust*temp1/temp3; ydir := Thrust*temp2/temp3;
			CASE ord(event.ReadKey) OF
           72 : {Up Arrow}
                BEGIN
						 Y.Put(N2Bodies+2*ThrustBody-1, temp1+xdir);
						 Y.Put(N2Bodies+2*ThrustBody, temp2+ydir);
						 VThrust := VThrust + Thrust
					 END;
			  80 : {Down Arrow}
					 BEGIN
						 Y.Put(N2Bodies+2*ThrustBody-1, temp1-xdir);
						 Y.Put(N2Bodies+2*ThrustBody, temp2-ydir);
						 VThrust := VThrust - Thrust
					 END;
			  75 : {Left Arrow}
					 BEGIN
						 Y.Put(N2Bodies+2*ThrustBody-1, temp1-ydir);
						 Y.Put(N2Bodies+2*ThrustBody, temp2+xdir);
						 HThrust := HThrust - Thrust
					 END;
			  77 : {Right Arrow}
					 BEGIN
						 Y.Put(N2Bodies+2*ThrustBody-1, temp1+ydir);
						 Y.Put(N2Bodies+2*ThrustBody, temp2-xdir);
						 HThrust := HThrust + Thrust
					 END;
			  73 : {Page Up} Thrust := Thrust*3;
			  81 : {Page Down} Thrust := Thrust/3;
			  71 : {Home}
					 BEGIN VThrust := 0.0; HThrust := 0.0 END
			END; {Case ReadKey}
         IF Abs(VThrust) < 0.001*Thrust Then VThrust := 0;
       	IF Abs(HThrust) < 0.001*Thrust Then HThrust := 0;
			ShowData(True);
		END; {If ExtendedKey}
	END; {CheckEvents}

BEGIN {ShowOrbit}
{M	HideCursor;}
	SightLineCount := 1;
	GraphBackColor := Black;
	TempV.Init(2*N2Bodies);
	i3 := 3; i4 := 4; i5 := 5; i6 :=6; i12 := 12;
	For iplot := 1 to Plots DO BEGIN
      iframe := FrameUsed[iplot];
		SavePos[iframe].init(2*N2Bodies);
		Position0[iframe].init(2*N2Bodies);
		Positions[iframe].init(2*N2Bodies);
		ToFrame(iframe, Positions[iframe],Y);
      SavePos[iframe].Equate(1.0, Positions[iframe]);
      Position0[iframe].Equate(1.0, Positions[iframe]);
   END; {For iplot}
	Message('Press F3 or Click Mouse to Stop & Wait (sometimes long) to complete time step.');
   count := 0;
   If (HistoryCount > 1) OR Wrapped THEN
      Count := (HistoryCount - 1) MOD DotInterval;
   IF Not Wrapped THEN {Otherwise it already exists}
		History[HistoryCount].init(2*N2Bodies);
{ ---------- Finished preparation, now do the calculations and plots -----}
	While Running DO
   BEGIN
      While Running AND (HistoryCount <= HistoryLength) DO
      BEGIN
         IF Not Wrapped THEN {Otherwise it already exists}
				History[HistoryCount].init(2*N2Bodies);
         History[HistoryCount].Equate(1.0, Y); {Save CofM position for replay}

         StepRKF(N_Body_Force, t, Y, TimeStep, TimeStepUsed, MaxStep,
                 AbsError, RelativeError, IFlag);
         CheckErrors;

         DrawTrajectories;
         IF PlotPoinCare THEN DoPoincare(2*PoinBody-1, 2*PoinBody);
			Count := Count + 1;
         If Count >= DotInterval THEN
			BEGIN
            Count := 0; DrawBodies(SightLineCount, i3, i4, i5, i6, i12)
			END;
			CheckEvents;
         HistoryCount := HistoryCount + 1;
      END; {While Running AND HistoryCount <= HistoryLength}
      IF Running THEN {The circular buffer needs to wrap}
      BEGIN
			Wrapped := True;
         HistoryCount := 1 
      END {Circular buffer}
   END; {While Running}
   SaveY.Resize(4*Nbodies);
   SaveY.Equate(1.0, Y); SaveT := t;
	TempV.Free;
   DupCount := RotNumber;
   For iplot := 1 to Plots DO 
   BEGIN
      iframe := FrameUsed[iplot];
      IF (iframe <> 6) {Rotating} AND (DupCount = RotNumber) THEN
      BEGIN
         SavePos[iframe].free;
			Position0[iframe].Free;
         Positions[iframe].Free;
         IF iframe = 6 THEN DupCount := DupCount - 1
      END
   END;
   MainMenu.Display;
   IF MainMenu.Activated THEN HandleMenu;
{M   ShowCursor}
END; {ShowOrbit}

VAR
	FirstTime : Boolean;
BEGIN {Program}
   CentralFInitialize;
   IntroScreen;
	FirstTime := True;
   REPEAT
      CheckForEvents;
      IF MainMenu.Activated THEN HandleMenu;
		If MainKeys.Pressed(key) THEN DoMainKeys(key);
		If Running THEN ShowOrbit;
		IF ClearIt THEN
		BEGIN
			ClearPlots({Temporary:} False, {ShowInit:} True);
			Running := True;
			ClearIt := False
		END;
		IF FirstTime THEN
		BEGIN
			Help(OpenScreen);
			MildAnnounce('Use F2 to start, F3 to stop.');
			FirstTime := False;
		END;
	UNTIL false;
END. {Program}