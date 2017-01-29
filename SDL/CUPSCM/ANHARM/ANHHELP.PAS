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
 	 {**           Unit AnhHelp.Pas               **}
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

{Seperate Unit to avoid exceeding code segment size on MAC}
UNIT AnhHelp;
INTERFACE
USES CUPS;
VAR
	OpenScreen, ResScreen : HelpScrType;

PROCEDURE DoHelpMenu(VAR Finished : Boolean);
PROCEDURE OpenTextPort(Color : Byte);
PROCEDURE OpenRightTextPort(Color : Byte);
PROCEDURE EnterMessage(What : String);
PROCEDURE HandleHelpMenu;
{procedure SmallFontProc;
 procedure TriplexFontProc;}


IMPLEMENTATION
USES CUPSMupp, CupsGui, Graph, AnhMenu, AnhPhy;

CONST
	MyVersion : Real = 1.00;

{procedure SmallFontProc; external;}
{L LITT.OBJ }

{procedure TriplexFontProc; external;}
{L TRIP.OBJ }

PROCEDURE OpenTextPort(Color : Byte);
BEGIN
	GraphBackColor := Color; SetColor(White);
	OpenMyViewPort(LeftPort);
	SetTextStyle(SmallFont, HorizDir, {Size=} 6)
END; {OpenTextPort}

PROCEDURE OpenRightTextPort(Color : Byte);
BEGIN
	GraphBackColor := Color; SetColor(White);
	OpenMyViewPort(RightPort);
	SetTextStyle(SmallFont, HorizDir, {Size=} 6)
END; {OpenTextPort}

PROCEDURE OpenHelpPort(Color : Byte);
BEGIN
	GraphBackColor := Color; SetColor(White);
	OpenMyViewPort(AllPort);
	SetTextStyle(SmallFont, HorizDir, {Size=} 4);
	SetUserCharSize(11,8,7,4);
END; {OpenHelpPort}

PROCEDURE EnterMessage(What : String);
BEGIN
   IF MousePresent THEN
      Message (What+' Press ENTER or click the mouse')
   ELSE
      Message ('                        Press ENTER when ready ...');
	REPEAT CheckForEvents
   UNTIL (Event.ReadKey = Chr(13)) OR Event.MouseClicked;
	Debounce;
	Message ('')
END;

PROCEDURE DoHelpMenu(VAR Finished : Boolean);

BEGIN  {DoHelpMenu}
   WITH HelpMenu DO
   CASE colChosen OF
    1 : BEGIN CASE rowChosen OF
			1 : ShowHelp('anharm.hlp','RunHelp;');
			2 : AboutCups;
			3 : ShowHelp('anharm.hlp','ConfigureHelp');
			5 : ShowHelp('anharm.hlp','FileHelp');
			6 : ShowHelp('anharm.hlp','FileHelp');
			7 : ShowHelp('anharm.hlp','FileHelp');
			8 : ShowHelp('anharm.hlp','FileHelp');
			9 : ShowHelp('anharm.hlp','FileHelp');
			10: ShowHelp('anharm.hlp','SearchFileHelp;');
			11 : ShowHelp('anharm.hlp','HistoryHelp;');
		 END; {Case Row}
		 END; {Column 1}
	 2 : CASE rowChosen OF
			1 : ShowHelp('anharm.hlp','ClearHelp;');
			2 : ShowHelp('anharm.hlp','ClearHelp;');
			3 : ShowHelp('anharm.hlp','BothSidesHelp;');
			4 : ShowHelp('anharm.hlp','AnimationHelp;');
			5 : ShowHelp('anharm.hlp','SelectEnergyScaleHelp;');
			7 : ShowHelp('anharm.hlp','PeriodTableHelp;');
			8 : ShowHelp('anharm.hlp','ResonanceTableHelp;');
			9 : ShowHelp('anharm.hlp','FFTHelp;');
			10: ShowHelp('anharm.hlp','FlipHelp;');
			11 :ShowHelp('anharm.hlp','FlipHelp;');
			12 :ShowHelp('anharm.hlp','PhaseDemoHelp');
		  END; {case rowChosen in Column 1}
	 3 : CASE rowChosen OF
			1 : ShowHelp('anharm.hlp','GraphHelp;');
			2 : ShowHelp('anharm.hlp','GraphHelp;');
			3 : ShowHelp('anharm.hlp','GraphHelp;');
			4 : BEGIN
					ShowHelp('anharm.hlp','PoincareHelp;');
					ShowHelp('anharm.hlp','MorePoincareHelp;')
				 END;
			5 : ShowHelp('anharm.hlp','ReturnHelp;');
			7 : ShowHelp('anharm.hlp','Show2_4Help;');
			8 : ShowHelp('anharm.hlp','Show2_4Help;');
			10: ShowHelp('anharm.hlp','PeriodHelp;');
			11: ShowHelp('anharm.hlp','PeriodHelp;');
			13: BEGIN
					ShowHelp('anharm.hlp','OptionHelp;');
					ShowHelp('anharm.hlp','OptionHelp2;')
				 END
		  END; {case rowChosen in Column 3}
	 4 : CASE rowChosen OF
			1 : ShowHelp('anharm.hlp','Asymmetric;');
			2 : ShowHelp('anharm.hlp','Cubic;');
			3 : ShowHelp('anharm.hlp','Pendulum;');
			4 : ShowHelp('anharm.hlp','SHO;');
			5 : ShowHelp('anharm.hlp','Walls;');
			6 : ShowHelp('anharm.hlp','VanDePol;');
			8 : ;
			9 : ShowHelp('anharm.hlp','ExactParHelp;');
			10 :ShowHelp('anharm.hlp','InterestingHelp;');
			11 :BEGIN
					ShowHelp('anharm.hlp','SearchHelp;');
					ShowHelp('anharm.hlp','SearchHelpII;')
				 END;
			12 :ShowHelp('anharm.hlp','ApproxHelp;');
		  END; {case rowChosen in Column 4}
	 5 : ShowHelp('anharm.hlp','IntroHelp;');
	 6 : Finished := True
	END; {case colChosen}
END; {DoHelpMenu}

PROCEDURE HandleHelpMenu;
VAR Finished : Boolean;
	 key : byte;
BEGIN
	HelpKeys.Init(0); HelpKeys.Display;
	HelpMenu.BackColor := Red;
	HelpMenu.Display;
	ShowHelp('anharm.hlp','IntroHelp;');
	WITH HelpKeys DO
	BEGIN
		Init(2);
      key[1] := 'F8-Quit Help';
		key[2] := 'F10-Help Menu'
	END;
	HelpKeys.Display;
	ShowMouse; Finished := False;
	REPEAT
		CheckForEvents;
      IF HelpKeys.Pressed(key) AND (key = 1) THEN Finished := True;
		IF HelpMenu.Activated THEN DoHelpMenu(Finished);
		IF Event.keypressed OR Event.MouseClicked THEN Finished := True
	UNTIL Finished;
	HideMouse;
	Debounce;
	SetUpSystemKeys;
(*	CloseMyViewPort(AllPort); GraphBackColor := darkGray;
	NewSystem({Clear:} True); DisplayChoice;*)
	DisplayMainMenu; ShowMouse
END; {HandleHelpMenu}

BEGIN
   If MyVersion > Version THEN Version := MyVersion;

   OpenScreen[2] := '            The Anharmonic Oscillator';
	OpenScreen[3] := '';
   OpenScreen[4] := '                  Bruce Hawkins';
	OpenScreen[5] := '                  Smith College';
	OpenScreen[6] := '    Consortium for Upper-level Physics Software';
	OpenScreen[7] := '         (c) 1995 John Wiley & Sons, Inc.';

	OpenScreen[9] := '--Simulates the motion of several oscillators.';
   OpenScreen[10]:= '  Information about each is in the Help system.';
   OpenScreen[11]:= '  Many systems exhibit chaos, and/or many basins';
   OpenScreen[12]:= '  of attraction to periodic and chaotic orbits.';

   OpenScreen[14]:= '--"Interesting parameters" on the "Forces" menu';
	OpenScreen[15]:= '  gives access to some of this richness.';

   OpenScreen[17]:= '--"Search parameter space" gives access to more.';

	OpenScreen[19]:= '--Turn animation off when computation is slow.';

	OpenScreen[21]:= '--One to four graphs can be viewed at one time:';
	OpenScreen[22]:= '  controlled from the "Display" menu.';

	OpenScreen[24]:= '     To set the oscillator in motion, click';
	OpenScreen[25]:= '        in Phase plot or Potential diagram.';
						 {'  For line length control                      '}
						 
ResScreen[2] := '                Resonance Cautions';

ResScreen[4] := ' You will note a message at the top of the screen';
ResScreen[5] := ' telling you to press Q to quit, and G to go on.';

ResScreen[7] := ' G is provided only for unusual circumstances.';
ResScreen[8] := ' Pressing it aborts the process of finding the';
ResScreen[9] := ' steady state amplitude for each frequency.';

ResScreen[11] := ' This is only provided for cases when the program';
ResScreen[12] := ' is unable to settle down, which often takes a';
ResScreen[13] := ' long time; be VERY sure it isn'' going to before';
ResScreen[14] := ' pressing G; if the amplitude settles in a cycle,';
ResScreen[15] := ' you do need to press G.';

ResScreen[16] := ' Since the whole plot can take a very long time,';
ResScreen[17] := ' Q is provided so you can change your mind.';

END. {UNIT OscMenus}
