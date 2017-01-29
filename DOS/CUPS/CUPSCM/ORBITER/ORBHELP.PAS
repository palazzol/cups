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

	 {*********************************************}
	 {**********************************************}
	 {**           Unit OrbHelp.Pas               **}
	 {**          Program CentralF.Pas            **}
	 {**          Central Force Orbits            **}
	 {**        Written by Bruce Hawkins          **}
	 {**             May 24, 1993                 **}
	 {**        Last change:  2/10/95             **}
	 {**             Version 1.00                 **}
	 {**                                          **}
	 {**      B. Hawkins Copyright @ 1992         **}
	 {**             CUPS Project                 **}
	 {**                                          **}
	 {**     Uses 12/95 MUPPET & CUPS Utilities   **}
	 {**********************************************}
	 {**********************************************}

{$F+}
UNIT OrbHelp;

INTERFACE

USES Graph, Cups, CupsMupp, CupsGui, CUPSGrph, CupsProc, OrbPhy, OrbPar;

VAR
	HelpKeys{, MainKeys, RunKeys} : THotKeys;
	UseManyHelp  : Boolean;
	OpenScreen, SettingHelp, WindowsHelp,
		ZoomInHelp, ZoomOutHelp : HelpScrType;

PROCEDURE SetUpMainKeys;  {Function keys }
PROCEDURE DoMainKeys (key: byte);  {Act on the function key presses}
PROCEDURE FreeReplayMemory;
PROCEDURE ReInit (Ask: Boolean);
PROCEDURE RestartIt;
PROCEDURE HandleHelpMenu;
PROCEDURE StartMany;

Implementation

PROCEDURE SetUpMainKeys;
BEGIN
	WITH MainKeys DO BEGIN
		Init(3);
		key[1] := 'F1-Help';
		key[2] := 'F2-Go';
		key[3] := 'F10-Menu';
	END;
	MainKeys.Display;
END; {SetUpMainKeys}

PROCEDURE DoMainKeys (key : byte);
BEGIN
	CASE key OF
	 1 : IF UseManyHelp THEN
			 ShowHelp('Orbiter.hlp','ManyHelp')
		  Else
			 HandleHelpMenu;
	 2 : BEGIN
			  RunKeys.Display;
			  Message(''); {Menu unavailable}
			  Running := True
		  END;
	END; {case}
	IF key <> 3 THEN Debounce
END; {DoMainKeys}

PROCEDURE FreeReplayMemory;
VAR i : Integer;
BEGIN
	IF HistoryCount > 1 THEN
		For i := 1 to HistoryCount - 1 DO
			History[i].free;
	IF Wrapped THEN
		For i := HistoryCount to HistoryLength DO
			History[i].free;
	Wrapped := False;
	HistoryCount := 1
END; {FreeReplayMemory}

PROCEDURE ReInit(Ask : Boolean);
VAR
	StillReplicated, UseNew, Done : Boolean;
	Screen : TInputScreen;
BEGIN
	t := 0.0;
	StillReplicated := False; UseNew := False;
	IF (Replicated OR NewPicked) AND Ask THEN {Ask}
	BEGIN
{M		HideCursor;}
		Message (' '); {Blank Menus since they are unavailable}
		WITH Screen DO
		BEGIN
			Init;
			LoadLine('Restart which System:');
			LoadLine('#1 Original System');
			If Replicated THEN
				LoadLine('#1 Replicated System')
			ELSE If NewPicked THEN
				LoadLine('#1 System Modified with Mouse');
			LoadLine(' [OK]     [Cancel]');
			DefineInputPort(0.05,0.45,0.75,0.95);
			AcceptScreen;
			IF NOT Canceled THEN
			BEGIN
				IF Replicated AND (GetRadioButton('1') = 2) THEN
					StillReplicated := True
				ELSE IF NewPicked AND (GetRadioButton('1') = 2) THEN
					UseNew := True;
			END; {Not Canceled}
			Done;
		END; {With Screen}
		MainMenu.Display
	END; {If Replicated OR NewPicked}
	IF StillReplicated THEN Y.Equate(1.0, RepInitY)
	ELSE BEGIN
		Replicated := False;
		NBodies := NBodiesOld;
		N2Bodies := 2* NBodies;
		NHeavy := NHeavyOld;
		NHeavyPrime := NHeavy;
		Y.Resize(4*NBodies);
		YP.Resize(4*NBodies);
		Y.Equate(1.0, InitY);
		IF UseNew THEN Y.Equate(1.0, NewInitY)
			ELSE NewPicked := False
	END
END; {ReInit}

PROCEDURE RestartIt;
BEGIN
	FreeReplayMemory;
	ReInit({Ask:} True);
	ClearPlots({Temporary:} False, {ShowInit:} True)
END; {RestartIt}

PROCEDURE StartMany;
VAR
	Done : Boolean;
	Body, When, Num, Coord : Integer;
	Bottom : Real;
	Screen : TInputScreen;
	Cancel : Boolean;

  PROCEDURE AskWhichBody;
  VAR i : Integer;
  BEGIN
	  Message (' '); {Blank Menus since they are unavailable}
	  NHeavyOld := NHeavy;
	  IF NBodies = NHeavy THEN
	  BEGIN
		  Comment(0.1,0.8,ConCat(
				 'This will launch several bodies from points near ',
						NameTable[NHeavy],'.'));
{M		  ShowCursor;}
		  Body := NHeavy
	  END {If NBodies}
	  ELSE With Screen DO
	  BEGIN
		  Init;
		  LoadLine ('This will launch several bodies');
		  LoadLine ('from starting points close together.');
		  LoadLine ('Which body do you want to replicate?');
		  FOR i := 1 to NBodies - NHeavy + 1 DO
			  LoadLine (ConCat('#1 ',NumStr(i,1,0),'- ',
					NameTable[NHeavy + i - 1]));
		  Screen.LoadLine ('');
		  LoadLine ('    [  OK  ]    [Cancel]');
		  Bottom := 0.6 - 0.041*(NBodies - NHeavy + 1);
		  DefineInputPort ( 0.02, 0.50, Bottom, 0.83);
		  AcceptScreen;
		  IF Not Canceled THEN Body := NHeavy - 1 + GetRadioButton('1')
		  ELSE Cancel := True;
		  Done;
	  END; {ELSE Nbodies > NHeavy}
	  IF Body = NHeavy THEN
		  NHeavyPrime := NHeavy - 1 {To prevent infinite energies}
  END; {AskWhichBody}

  PROCEDURE AskWhen;
  VAR i : Integer;
  BEGIN
	  Screen.Init;
	  SelectMyViewPort(LeftPort);
	  Screen.LoadLine ('Start them at what time?');
	  Screen.LoadLine ('#1 At t = 0');
	  Screen.LoadLine  ('#1 Now');
	  Screen.LoadLine ('');
	  Screen.LoadLine ('[  OK  ]    [Cancel]');
	  Screen.DefineInputPort ( 0.06, 0.40, 0.50, 0.73);
	  Screen.AcceptScreen;
	  IF Not Screen.Canceled THEN When := Screen.GetRadioButton('1') - 1
	  ELSE Cancel := True;
	  Screen.Done;
  END; {AskWhen}

  PROCEDURE ChangeSpreadFactor;
  VAR
	  SpreadScreen    : TInputScreen;
  BEGIN
	 WITH SpreadScreen DO
	 BEGIN
		Init;
		DefineInputPort(0.03, 0.47, 0.60, 0.65);
		LoadLine ('Spread Factor: {    } per cent');
		LoadLine (' [OK]            [Cancel]');
		SetNumber(1,100*MultipleSpread);
		Message('Press <ENTER> to change parameters, <ESC> to leave unchanged');
		AcceptScreen;
		IF Not Canceled THEN
			  MultipleSpread := GetNumber(1)/100.0;
		Message (' ');
		SetColor(Black);
		OpenMyViewPort(ClearD2Port);
		SetColor(White); GraphBackColor := Blue;
		Done;
	 END {With SpreadScreen}
  END; {ChangeSpreadFactor}

  PROCEDURE AskHowMany;
  VAR Tstr : String[3];
		i : Integer;
  BEGIN
	  IF When = 0 THEN Tstr := '0' ELSE Tstr := 'now';
	  Num := 0;
	  Screen.Init;
	  Screen.LoadLine ('How many new bodies do you want near');
	  Screen.LoadLine (Concat(NameTable[Body],' at time = ',Tstr,'?'));
	  FOR i := 1 to 4 DO
		  Screen.LoadLine (ConCat('#1 ', NumStr(2*i,1,0)));
	  Screen.LoadLine (ConCat('The bodies will be ',
					  Numstr(100*MultipleSpread,5,2),'% apart.'));
	  Screen.LoadLine ('You may change this if you wish.');
	  Screen.LoadLine ('');
	  Screen.LoadLine ('');
	  Screen.LoadLine  ('#F Change Spread Factor');
	  Screen.LoadLine ('');
	  Screen.LoadLine ('[  OK  ]    [Cancel]');
	  Screen.DefineInputPort ( 0.02, 0.52, 0.22, 0.80);
	  REPEAT
		  Screen.AcceptScreen;
		  IF Screen.Canceled THEN Cancel := True
		  ELSE IF Screen.GetBoolean(5) THEN ChangeSpreadFactor
		  ELSE Num := 2*Screen.GetRadioButton('1');
		  {Don't change spread factor again by accident: }
		  Screen.SetBoolean(5, False)
	  UNTIL (Num > 0) OR Cancel;
	  Screen.Done;
  END; {AskHowMany}

  PROCEDURE AskCoordinate;
  VAR i, imax : Integer;
		Bottom : Real;
  BEGIN
	  Screen.Init;
	  Screen.LoadLine ('Spread over which coordinates?');
	  Screen.LoadLine ('#1 x');
	  Screen.LoadLine ('#1 y');
	  Screen.LoadLine ('#1 vx');
	  Screen.LoadLine ('#1 vy');
	  Bottom := 0.50;
	  Case Num of
		  4,8 : BEGIN
					Screen.LoadLine ('#1 Both space coordinates');
					Screen.LoadLine ('#1 Both velocity coordinates');
					Bottom := 0.41;
					IF Num = 8 THEN
					BEGIN
						Screen.LoadLine ('#1 All space & velocity coords');
						Bottom := 0.37
					END
				  END
	  END; {Case Num}
	  Screen.LoadLine (' ');
	  Screen.LoadLine ('[  OK  ]    [Cancel]');
	  Screen.DefineInputPort ( 0.04, 0.46, Bottom, 0.80);
	  Screen.AcceptScreen;
	  IF Not Screen.Canceled THEN Coord := Screen.GetRadioButton('1')
	  ELSE Cancel := True;
	  Screen.Done;
 END; {AskCoordinate}

  PROCEDURE ManyProbesInit;
  VAR
	  i, ispread, ib, ibdel1, ibdel2, imax, j, jmax : Integer;
	  TempV : DVector;
	  R, Rmin, Delta, DeltaV : Real;
	  UseColor: Boolean;
  BEGIN
	  RepInitY.ReSize(4*Nbodies);
	  IF When = 1 Then RepInitY.Equate(1.0, Y) {Use position now}
		  ELSE RepInitY.Equate(1.0, InitY); {Use original Position}
	  NBodiesOld := NBodies; NBodies := NBodies + Num; N2Bodies := 2* Nbodies;
	  TempV.init(UnivVel.Length); TempV.Equate(1.0, UnivVel);
	  UnivVel.ReSize(N2Bodies);
	  UnivVel.put(1, TempV.Value(1)); UnivVel.put(2, TempV.Value(2));
	  ExtendVect(UnivVel); TempV.free;
 
	 Y.ReSize(4*NBodies); YP.ReSize(4*NBodies);
	 FOR i := 1 to NHeavy DO BEGIN
		 Xh[i].Resize(NBodies);
		 Yh[i].Resize(NBodies);
		 Rh_3[i].Resize(NBodies) END;

	 ib := 8;
	 FOR i := NBodiesOld+1 TO NBodies DO  {Choose bright colors for bodies}
	 BEGIN
		 ib := ib + 1;
		 IF ColorB[Body] = ib {rainbow[1].colors[ib]} THEN ib := ib + 1;
		 IF ib >= 15 THEN ib := 9;
		 ColorB[i] := ib {rainbow[1].colors[ib];}
	 END; {For i}

	 FOR i := 1 TO 2*NBodiesOld DO BEGIN
		 Y.Put(i, RepInitY.Value(i));                      {Transfer coordinates}
		 Y.Put(i+N2Bodies, RepInitY.Value(i+2*NBodiesOld)) {Transfer velocities}
	 END; {For i}
	 i := 2*NbodiesOld + 1;
	 WHILE i < N2Bodies DO BEGIN           {Fill in new bodies = body chosen}
		 Y.Put(i, Y.Value(2*Body-1));
		 Y.Put(i+1, Y.Value(2*Body));
		 Y.Put(i+N2Bodies, Y.Value(2*Body+N2Bodies-1));        {Velocities}
		 Y.Put(i+N2Bodies+1, Y.Value(2*Body+N2Bodies));
		 i := i + 2
	 END; {While i < N2Bodies}
	 ibdel1:= 1;
	 ibdel2:= 0;
	 imax := num;
	 jmax := 1;
	 Case Coord OF
		 1 : ib   := 2*NBodiesOld-1;          {x}
		 2 : ib   := 2*NBodiesOld;            {y}
		 3 : ib   := 2*NBodiesOld+N2Bodies-1; {vx}
		 4 : ib   := 2*NBodiesOld+N2Bodies;   {vy}
		 5 : BEGIN                            {Both space coordinates}
				ib   := 2*NBodiesOld-1;
				ibdel2 := NBodies-NBodiesOld+1;
				imax := Num DIV 2;
				jmax := 2
			  END;
		 6 : BEGIN                            {Both velocities}
				ib   := 2*(NBodiesOld+NBodies)-1;
				ibdel2 := NBodies-NBodiesOld+1;
				imax := Num DIV 2;
				jmax := 2
			  END;
		 7 : BEGIN                             {All four}
				ib   := 2*NBodiesOld-1;
				jmax := 4;
				imax := Num DIV jmax;
				ibdel1 := 2*imax+1;
				ibdel2:= N2Bodies-1
			  END;
	 END; {Case Coord}

{---------   Find Nearest massive Body, use fraction of distance to it   }

	 Rmin := 1.0E6;
	 FOR j := 1 to NHeavy DO
	 BEGIN
		 IF j <> Body THEN
			 R := Sqr(Y.Value(2*Body-1)-Y.Value(2*j-1))
					+ Sqr(Y.Value(2*Body)-Y.Value(2*j));
		 IF R < Rmin THEN Rmin := R
	 END; {For j := 1 to NHeavy}
	 Delta := MultipleSpread*Sqrt(Rmin);
	 DeltaV :=  Sqr(Y.Value(N2Bodies+2*Body-1)) 
			  + Sqr(Y.Value(N2Bodies+2*Body));
	 DeltaV := MultipleSpread*Sqrt(DeltaV);
	 If Coord in [3,4,6] THEN Delta := DeltaV;

	 FOR j := 1 TO jmax DO BEGIN
		 ispread := - imax DIV 2;
		 FOR i := 1 TO imax DO {Adjust the values for the new bodies}
			 BEGIN
			 Y.Put(2*i+ib, Y.Value(2*i+ib) + ispread*Delta);
			 IF i = imax DIV 2 THEN ispread := ispread + 2 
				 ELSE ispread := ispread + 1
			 END; {For i}
		 IF j = jmax DIV 2 THEN BEGIN
			 ib := ib + ibdel2;
			 IF Coord = 7 THEN Delta := DeltaV END
		  ELSE ib := ib + ibdel1
	 END; {For j}
	 t := 0.0;
	 Replicated := True;
	 FreeReplayMemory;
	 For i := NBodiesOld+1 TO NBodies DO Nametable[i] := NameTable[Body];
	 RepInitY.ReSize(4*Nbodies);
	 RepInitY.Equate(1.0, Y);
  END; {ManyProbesInit}

BEGIN {StartMany}
	IF Replicated THEN ReInit({Ask:} False);
	UseManyHelp := True; Cancel := False;
	AskWhichBody;
	IF Not Cancel THEN AskWhen;
	IF Not Cancel THEN AskHowMany;
	IF Not Cancel THEN AskCoordinate;
	IF Not Cancel THEN ManyProbesInit;
	MainKeys.Display;
	MainMenu.Display;
	UseManyHelp := False;
	ClearPlots({Temporary:} False, {ShowInit:} True)
END; {StartMany}

PROCEDURE DoHelpMenu(VAR Finished : Boolean);
BEGIN {DoHelpMenu}
	Finished := False;
	WITH HelpMenu DO
	CASE colChosen OF
	 1 : Case rowChosen of
			1 : Help(OpenScreen);
			2 : AboutCups;
			3 : ShowHelp('orbiter.hlp','ConfigureHelp');
			5 : ShowHelp('orbiter.hlp','ReadHelp');
			6 : ShowHelp('Orbiter.hlp','ReadHelp');
		  END; {case rowChosen in Column 1}
	 2 : CASE rowChosen OF
			1 : ShowHelp('Orbiter.hlp','ClearHelp');
			2 : ShowHelp('Orbiter.hlp','RestartHelp');
			3 : ShowHelp('Orbiter.hlp','ReplayHelp');
			4 : ShowHelp('Orbiter.hlp','ManyHelp');
			5 : BEGIN
					ShowHelp('Orbiter.hlp','SettingsHelp');
					ShowHelp('Orbiter.hlp','MoreSettings')
				 END;
			6 : ShowHelp('Orbiter.hlp','ReverseTimeHelp');
			7 : ShowHelp('Orbiter.hlp','ChangeParametersHelp');
			8 : ShowHelp('Orbiter.hlp','MoveBodyHelp');
			9 : ShowHelp('Orbiter.hlp','AddBodyHelp');
			10 : BEGIN
					ShowHelp('Orbiter.hlp','ThrustHelp');
					ShowHelp('Orbiter.hlp','MoreThrust')
				  END;
		  END; {case rowChosen in Column 2}
	 3 : CASE rowChosen OF
			1 : ShowHelp('Orbiter.hlp','NFrameHelp');
			2 : ShowHelp('Orbiter.hlp','NextOneHelp');
			3 : ShowHelp('Orbiter.hlp','NFrameHelp');
			4 : ShowHelp('Orbiter.hlp','NFrameHelp');
			5 : ShowHelp('Orbiter.hlp','NFrameHelp');
			6 : ShowHelp('Orbiter.hlp','MarkCofMassHelp');
			7 : ShowHelp('Orbiter.hlp','FullScreenHelp');
			8 : ShowHelp('Orbiter.hlp','ZoomHelp');
			9 : ShowHelp('Orbiter.hlp','ZoomHelp');
			10: ShowHelp('Orbiter.hlp','DefaultScaleHelp')
		  END; {case rowChosen in Column 3}
	 4 : CASE rowChosen OF
			1 : ShowHelp('Orbiter.hlp','SunEMoonHelp');
			2 : ShowHelp('Orbiter.hlp','SolarHelp');
			3 : ShowHelp('Orbiter.hlp','ShuttleHelp');
			4 : ShowHelp('Orbiter.hlp','SunJupCometHelp');
			5 : ShowHelp('Orbiter.hlp','SunJupMoonsHelp');
			6 : ShowHelp('Orbiter.hlp','LagrangeHelp');
			7 : ShowHelp('Orbiter.hlp','BinaryHelp');
			8 : ShowHelp('Orbiter.hlp','RetrogradeHelp');
			9 : ShowHelp('Orbiter.hlp','CreateSystemHelp')
		  END; {case rowChosen in Column 4}
	 5 : ShowHelp('Orbiter.hlp','IntroHelp');
	 6 : Finished := True
	END; {case colChosen}
	Debounce
END; {DoHelpMenu}

PROCEDURE HandleHelpMenu;
VAR Finished : Boolean;
	 key : byte;
BEGIN
	HelpKeys.Init(0); HelpKeys.Display;
	WITH HelpKeys DO
	BEGIN
		Init(2);
		key[1] := 'F8-Quit Help';
		key[2] := 'F10-Help Menu'
	END;
	HelpKeys.Display;
	HelpMenu.BackColor := Red;
	HelpMenu.Display;
	ShowHelp('Orbiter.hlp','IntroHelp');
{M	ShowCursor;}
	Finished := False;
	REPEAT
		CheckForEvents;
		IF Event.keypressed or Event.Mouseclicked THEN Finished := True;
		IF HelpKeys.Pressed(key) AND (key = 1) THEN Finished := True;
		IF HelpMenu.Activated THEN DoHelpMenu(Finished)
	UNTIL Finished;
{M	HideCursor;}
	Debounce;
	SetTextStyle(DefaultFont, HorizDir, 1);
	MainKeys.Display;
	ClearPlots({Temporary:} False, {ShowInit:} True);
	MainMenu.Display; {M ShowCursor}
END; {HandleHelpMenu}

BEGIN {Initialize unit}
   OpenScreen[2] := '               Gravitational Orbits';
   OpenScreen[3] := '';
   OpenScreen[4] := '                   Bruce Hawkins';
   OpenScreen[5] := '                   Smith College';
   OpenScreen[6] := '    Consortium for Upperlevel Physics Software';
   OpenScreen[7] := '         (c) 1995 John Wiley & Sons, Inc.';

   OpenScreen[9] := 'A simulation of the motion of bodies acted on by ';
	OpenScreen[10]:= 'gravity, such as planets, satellites, asteroids, ';
   OpenScreen[11]:= 'comets, and stars.';

   OpenScreen[13]:= 'A number of familiar or interesting systems are ';
   OpenScreen[14]:= 'included. You may find more information about each';
   OpenScreen[15]:= 'of them by selecting help and the desired system ';
   OpenScreen[16]:= 'from the Forces menu.';

   OpenScreen[18]:= 'You may also create your own systems, either by';
   OpenScreen[19]:= 'modifying the parameters of an existing one or by';
	OpenScreen[20]:= 'selecting ''Create System''.';

   OpenScreen[22]:= 'A small change in initial conditions often results';
   OpenScreen[23]:= 'in large changes in the later motion. You can';
   OpenScreen[24]:= 'explore this phenomenon with ''Replicate A Body''';
   OpenScreen[25]:= 'under ''Choices''.';


   SettingHelp[ 1] := '                 Settings';
   SettingHelp[ 2] := '- Time Step: time to update plot; integration ';
	SettingHelp[ 3] := '   can be many integration steps if bodies close';
   SettingHelp[ 4] := '- Largest Time Step:used internally by integration';

   SettingHelp[ 6] := '- Automatic Replay after changing number of plots';
   SettingHelp[ 7] := '- Automatic Replay when choosing next plots';
   SettingHelp[ 8] := '    If off, only current position will be shown ';
   SettingHelp[ 9] := '    in new plot, no orbit track';

   SettingHelp[11] := '- Size of Dots marking present body positions';
   SettingHelp[12] := '    1 and 2 the most useful, 3, 4, ... are bigger';
	SettingHelp[13] := '- Show body positions';
   SettingHelp[14] := '   Off: Solid tracks only';
   SettingHelp[15] := '   On : Dots, time marks in tracks shown';
   SettingHelp[16] := '        Time marks sometimes obscure entire track';

   SettingHelp[18] := '- Show past track of bodies';
   SettingHelp[19] := '   Off: shows only the current body position';
   SettingHelp[20] := '        (every 10 time steps)';

   SettingHelp[22] := '- Absolute Integration Error';
	SettingHelp[23] := '- Relative Integration Error';
   SettingHelp[24] := '   Integration routine internal time step chosen';
   SettingHelp[25] := '   to satisfy: |Error of x| < |RErr*x + AbsErr|.';

   WindowsHelp[ 1] := '     Choose One, Two, Four, or Six Windows';
   WindowsHelp[ 3] := 'Available windows are shown';

   WindowsHelp[ 5] := 'The first four move with the body named.';
   WindowsHelp[ 6] := '  They are usually centered on the body named;,';
   WindowsHelp[ 7] := '  some center elsewhere at startup and are zoomed.';

   WindowsHelp[ 9] := 'In the Universe Window the center of mass may move.';

   WindowsHelp[11] := 'In the Rotating Window, the Body 1-->Body 2 line';
   WindowsHelp[12] := '  is fixed; the window is centered and zoomed at';
   WindowsHelp[13] := '  startup for the most interesting display.';

   WindowsHelp[15] := 'The Second Rotating Window may have a different';
   WindowsHelp[16] := '  center and zoom factor.';

	WindowsHelp[18] := 'Poincar‚ (Vx vs X at Y=0) plots the X phase space';
   WindowsHelp[19] := '  position of (usually) body 3 every time that';
	WindowsHelp[20] := '  its Y position meets a criterion (usually 0)';
   WindowsHelp[21] := '  It makes ordinary or strange attractors visible.';

   WindowsHelp[23] := 'Any window may have its center changed by using';
   WindowsHelp[24] := '  Zoom In followed by Zoom Out.';
	{------------------------}
	ZoomInHelp[ 2] := '                   Zoom In';

   ZoomInHelp[ 4] := 'Select the window and then OK';
   ZoomInHelp[ 5] := '  Click the mouse to indicate the new center';
   ZoomInHelp[ 6] := '  for the window, usually near some body.';

   ZoomInHelp[ 8] := 'Change the zoom factor first if you need to.';

   ZoomInHelp[10] := 'To change the center of a window & not its scale,';
   ZoomInHelp[11] := '  zoom in, pick the center, and then zoom out.';

	ZoomOutHelp[ 2] := '                   Zoom Out';

   ZoomOutHelp[ 4] := 'Select the window and then OK';
   ZoomOutHelp[ 5] := '  The center of the window will not change.';

   ZoomOutHelp[ 7] := 'Change the zoom factor first if you need to.';

   ZoomOutHelp[ 9] := 'To change the center of a window & not its scale,';
   ZoomOutHelp[10] := ' zoom in, pick the center, and then zoom out.';

END.