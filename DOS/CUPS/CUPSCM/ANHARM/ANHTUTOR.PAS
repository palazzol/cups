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
	 {**           Program AnHarm.Pas             **}
	 {**      Non-Linear Oscillator Behaviour     **}
	 {**              Unit AnhTutor               **}
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

UNIT AnhTutor;

INTERFACE
PROCEDURE Tutorial(CalledFirst : Boolean);

IMPLEMENTATION
USES Dos, Crt, Graph, Cups, CupsMUPP, CupsFunc, CupsProc, CupsGui, CupsGrph,
		AnhPhy, AnhMenu, AnhHelp;

VAR
	MyVersion : Real;
	key : Byte;
	TwoWallKeys : THotKeys;
	SaveDelay, SaveCrossings, SavePeriod : Integer;
	SaveSep : Real;
	SaveErase, Escape : Boolean;

PROCEDURE MouseMess;
BEGIN
	MildAnnounce('You were not asked to click the mouse.');
	Event.MouseClicked := False
END;

	PROCEDURE NewWall;
	BEGIN
		HideMouse;
		GraphBackColor := DarkGray;
		SetPlots;
		SetColor(White);
		DrawSystem;
		FindPotential(True);
		DisplayPotential(BothV, HoldVScale);
		displayChoice;
		ShowMouse
	END; {NewWall}

PROCEDURE SetUpTwoWallKeys;
BEGIN
	WITH TwoWallKeys DO
	BEGIN
		Init(2);
		key[1] := 'F2-Wall Up';
		key[2] := 'F4-Wall Down';
  END;
	TwoWallKeys.Display;
END; {SetUpTwoWallKeys}
 
PROCEDURE DoTwoWallKeys (key : byte);
BEGIN
	CASE key OF
	 1 : HalfWallSep:=HalfWallSep + 0.01;
	 2 : HalfWallSep:=HalfWallSep - 0.01;
	END; {case}
	IF HalfWallSep < 0.01 THEN HalfWallSep := 0.01;
	HideMouse;
	DrawSystem;
	FindPotential(True);
	DisplayPotential(BothV, HoldVScale);
	DisplayChoice;
	ShowMouse
END; {DoTwoWallKeys}

PROCEDURE CheckEscape;
BEGIN
	CheckForEvents;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True
END;

Procedure MoveIt;
BEGIN
	GraphBackColor := darkGray;
	MoveWall;
	CheckEscape; IF Escape THEN Exit;
	IF TwoWallKeys.Pressed(key) THEN DoTwoWallKeys(key)
END; {MoveIt}

PROCEDURE ShowSomePlots;
BEGIN
	SaveSep := HalfWallSep;
	ShowMouse;
	MoveWall;
	IF WallKeys.Pressed(key) THEN
	BEGIN
		DoSystemKeys;
		displayChoice;
		FindPotential(True);
		DisplayPotential(BothV, HoldVScale)
	END; {If Wallkeys}
	IF HalfWallSep <> SaveSep THEN ClearPhasePort;
	CheckEscape;
	PickEnergy;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True
END; {ShowSomePlots}

PROCEDURE ClickOrEnter;
BEGIN
	REPEAT CheckEscape
	UNTIL (Event.ReadKey = Chr(13)) OR Escape
		OR Event.MouseClicked;
(*	IF (Event.ReadKey = Chr(27)) THEN
	BEGIN
		Escape := True;
		ShowMouse
	END;*)
	Debounce
END;

PROCEDURE PressKey(Key: Char); {Expects lowercase}
BEGIN
	Repeat
		CheckEscape;
		If Escape THEN Exit;
		IF Event.Mouseclicked
		  or (Event.keypressed AND
			Not ((Event.ReadKey = Key) or (Event.Readkey = Chr(Ord(Key)-32))))
			THEN MildAnnounce('You were asked to press '+key+'.');
	UNTIL (Event.ReadKey = Key) or (Event.Readkey = Chr(Ord(Key)-32))
END; {PressKey}

PROCEDURE DescribeOsc;
BEGIN
	HideMouse;
	OpenTextPort(Red);
	Print(8,1,'An Anharmonic Oscillator');
	Print (5,3,'A ball is held by two springs');
	Print (5,4,'stretched between two walls.');
	Print (5,6,'The upper wall is moveable:');
	Print (10, 8, 'Please click or press ENTER.');
	ClickOrEnter
END; {DescribeOsc}

PROCEDURE MoveCloser;
BEGIN
	SelectLeftPort;
	SetTextStyle(SmallFont, HorizDir, {Size=} 6);
	SetColor(Red); {Erase}
	Print (5,6,'The upper wall is moveable:');
	Print (10, 8, 'Please click or press ENTER.');
	SetColor(White);
	Print (5,6,'When the walls are this far');
	Print (5,7,'apart, the springs are relaxed.');
	Print (5,9,'To keep your attention, the');
	Print (5,10,'instructions for continuing');
	Print (5,11,'will vary; -- this time,');
	Print (10, 13, 'please press the h key.');
	HalfWallSep := 0.4; NewWall;
	PressKey('h')
END; {MoveCloser}

PROCEDURE CloserStill;
BEGIN
	SelectLeftPort;
	SetTextStyle(SmallFont, HorizDir, {Size=} 6);
	SetColor(Red); {Erase}
	Print (5,6,'When the walls are this far');
	Print (5,7,'apart, the springs are relaxed.');
	Print (5,9,'To keep your attention, the');
	Print (5,10,'instructions for continuing');
	Print (5,11,'will vary; -- this time,');
	Print (10, 13, 'please press the h key.');
	SetColor(White);
	Print (5,6,'When the walls are still closer,');
	Print (5,7,'the springs would be compressed,');
	Print (5,8,'but of course they jump out to');
	Print (5,9,'the side instead.');
	Print (10,11, 'Please press e.');
	HalfWallSep := 0.3; Plus := False; NewWall;
	PressKey('e')
END; {CloserStill}

PROCEDURE TwoEquilibria;
BEGIN
	SelectLeftPort;
	SetTextStyle(SmallFont, HorizDir, {Size=} 6);
	SetColor(Red); {Erase}
	Print (5,6,'When the walls are still closer,');
	Print (5,7,'the springs would be compressed,');
	Print (5,8,'but of course they jump out to');
	Print (5,9,'the side instead.');
	Print (10,11, 'Please press e.');
	SetColor(White);
	Print (5,6,'Notice that the potential graph,');
	Print (5,7,'now has TWO minima.');
	Print (5,8,'Both are stable.');
	Print (5,9,'We say symmetry has been broken.');
	Print (10, 11, 'Press b to continue.');
	REPEAT
		HalfWallSep := 0.3; Plus := Not Plus; DrawSystem; Delay(1000);
		PressKey('b')
	UNTIL (Event.ReadKey = 'b') OR Escape;
	Event.ReadKey := Chr(0);
END; {TwoEquilibria}

PROCEDURE PracticeMoving;
BEGIN
	OpenTextPort(Red);
	Print(12,1,'Moving the Walls');
	Print (5,3,'You can move the wall with the');
	Print (5,4,'mouse or with the F2 & F4 keys.');
	Print (5,5,'You can also click the mouse on');
	Print (5,6,'F2 or F4 at the bottom of the');
	Print (5,7,'screen.');
	Print (2,9,'Try all these ways of moving the wall.');
	Print (10,11,'Then Press m.');
	SetupTwoWallKeys;
	ShowMouse;
	REPEAT
		MoveIt;
	UNTIL (Event.ReadKey = 'm') OR (Event.ReadKey = 'M')  OR Escape;
END; {PracticeMoving}

VAR
	Time : LongInt;

FUNCTION	GetATime : LongInt;
VAR
{DateTime = record
  Year,Month,Day,Hour,Min,Sec: Word;  }
	Dtime : DateTime;
	hund, dow : Word;
	Now : LongInt;
BEGIN
	With Dtime DO
	BEGIN
		GetTime(hour,min,sec,hund);
		GetDate(year,month,day,dow);
		Now := (Year-1990)*12 + Month;
		Now := Now*31 + Day;      {Glitches on short month, but who cares}
		Now := Now*24 + Hour;
		Now := Now*60 + Min;
		GetATime := Now*60 + Sec
	END
END;

PROCEDURE DiscussPotential;
BEGIN
	HideMouse;
	HalfWallSep := 0.2; NewWall;
	OpenTextPort(Red);
	Print(8,1,'Potential Function Shape');
	Print (5,3,'Move the wall all the way up');
	Print (5,4,'and notice how the potential');
	Print (5,5,'function looks: almost a');
	Print (5,6,'parabola as far as you can tell.');
	ShowMouse;
	Time := GetATime;
	REPEAT
		MoveIt;
		IF (GetAtime > Time + 3) AND (HalfWallSep < 0.45) THEN
		BEGIN
			MildAnnounce('Please move the wall up to at least 0.9.');
			Time := GetATime
		END;
		IF Escape THEN Exit;
	UNTIL (HalfWallSep >= 0.45);
	HideMouse;
	SelectLeftPort;
	SetTextStyle(SmallFont, HorizDir, {Size=} 6);
	Print (5,8,'Now move it down to a separation');
	Print (5,9,'of 0.8, (plus or minus 0.04),');
	PRINT (5,10,'and notice how the bottom');
	Print (5,11,'of the potential flattens out.');
	ShowMouse;
	Time := GetATime;
	REPEAT
	  MoveIt;
	  IF (GetAtime > Time + 3) AND
			((HalfWallSep > 0.42) OR (HalfWallSep < 0.38)) THEN
	  BEGIN
			MildAnnounce('Please move the wall to between 0.76 and 0.84.');
			Time := GetATime
	  END;
	  IF Escape THEN Exit
	UNTIL ((HalfWallSep <= 0.42) AND (HalfWallSep >= 0.38))
			OR Escape;
{	SetColor(White);
	DrawSystem}
END; {DiscussPotential}

PROCEDURE ShowApprox;
BEGIN
	HideMouse;
	OpenTextPort(Red);
	Print(10,1,'Approximate Potential');
	Print (4,3,'The blue line is a power series');
	Print (4,4,'approximation to the potential.');
	Print (4,5,'Notice the approximation is better');
	Print (4,6,'when the walls are farther apart.');
	Print (2,8,'(Adding more terms doesn''t help much.)');
	Print (2,10,'Move the wall and watch the potential.');
	Print (10,12,'(Press x to continue.)');
	ToggleExactAccel; HalfWallSep := 0.4; NewWall;
	Event.ReadKey := Chr(0);
	ShowMouse;
	REPEAT
		Moveit;
	UNTIL (Event.ReadKey = 'x') OR (Event.ReadKey = 'x') OR Escape;
	ToggleExactAccel;
END; {ShowApprox}

PROCEDURE DemoXandV;
BEGIN
	HideMouse;
	TimePlotInverted := False;
	OpenTextPort(Red);
	Print(11,1,'Graphs of the Motion');
	Print (2,3,'Now lets look at graphs of');
	Print (2,4,'position and velocity against time.');
	Print (2,6,'Click the mouse anywhere inside the');
	Print (2,7,'Potential energy diagram.');
	Print (2,9,'  (Press ENTER if you have no mouse.)');
	Print (2,11,'The total energy will also show on the');
	Print (2,12,'  potential energy diagram.');
	ChooseWalls;
	Continuous := False; ShowPeriods := 2; TimeLimit := True;
	HalfWallSep := 0.5;
	NewWall;
	ShowMouse;
	REPEAT CheckEscape; If Escape THEN Exit
	UNTIL Event.MouseClicked OR (Event.readkey = Chr(13));
	ShowXandV;
	Message ('Wait for the motion to reach the end of the graph.');
	PickEnergy;
	GraphBackColor := DarkGray;
	SetMyColor(10);
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	IF Escape THEN Exit;
	Message('Move walls and click in potential diagram; Press C to clear, G to go on.');
	SetMyColor(11); NColor := 11;
	Event.readkey := Chr(0);
	REPEAT
		ShowSomePlots;
		IF Event.ReadKey in ['C','c'] THEN
		BEGIN
			ClearPhasePort;
			DisplayPotential(False,False)
		END;
		IF (Event.ReadKey in ['G','g'])
			 AND ((HalfWallSep >= 0.37) OR (HalfWallSep <= 0.27)
					 OR (Energy < 0.0075)) THEN
			 BEGIN
			 Message('Make one with a double well and the energy just above'
					  +' the middle hump (Then G to go on)');
			 MildAnnounce('Try energy just above the hump in a double well.')
			 END
	UNTIL Escape OR ((Event.ReadKey in ['G','g'])
			  AND (HalfWallSep < 0.33) AND (HalfWallSep > 0.27)
			  AND (Energy >= 0.0075));
END; {DemoXandV}

PROCEDURE DemoAnimation;
BEGIN
	Message('Press G to stop motion and then select a new energy.');
	HideMouse;
	OpenTextPort(Red);
	SetColor(White);
	Print (15,1,'Animation');
	Print (2,3,'Watch a picture of the actual motion');
	Print (2,4,'along with a ball rolling up and down');
	Print (2,5,'the potential hill. Think about how');
	Print (2,6,'the speed of the latter is related to');
	Print (2,7,'the speed of the actual motion. Try as');
	Print (2,8,'many wall separations and energies as');
	Print (2,9,'you like. The spring is green when');
	Print (2,10,'stretched, magenta when compressed.');

	Print (2,12,'Press G to stop the motion.');
	Print (2,13,'Then select a new energy with');
	Print (2,14,'the mouse or the F keys.');

	Print (3,16,'When you have tried as many motions');
	Print (2,17,'as you want, press Q (once or twice).');
	GraphBackColor := DarkGray;
	HalfWallSep := 0.28;
	Energy := 0.015; NewWall;
	SetTextStyle(DefaultFont, HorizDir, {Size=} 1);
	DisplayChoice;
	SetAllFalse;
	Animating := True;
	SetMyColor(11); NColor := 11;
	ShowTrajectory;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	IF Escape THEN Exit;
	ShowMouse;
	REPEAT
		Event.readkey := Chr(0);
		ShowSomePlots;
		IF Event.ReadKey in ['C','c'] THEN
		BEGIN
			ClearPhasePort;
			DisplayPotential(False,False)
		END;
	UNTIL (Event.ReadKey in ['Q','q']) OR Escape;
	HideMouse;
	OpenTextPort(Red);
	SetColor(White);
	Print (15,1,'Animation');
	Print (3,3,'Now add graphs of v and x against t.');
	Print (3,4,'Think about how the graphs represent');
	Print (3,5,'the motion, especially the less');
	Print (3,6,'familiar velocity graph.');
	Print (10,12,'Click or ENTER.');
	Message('');
	GraphBackColor := DarkGray;
	HalfWallSep := 0.28; NewWall;
	Energy := 0.015;
	SetTextStyle(DefaultFont, HorizDir, {Size=} 1);
	DisplayChoice;
	ClickOrEnter;
	ShowMouse;
	ShowXandV;
	Message('Move walls and select energy; C clears, G new graph,'
			  +' Q to go on.');
	Animating := True;
	HideMouse;
	SetMyColor(11); NColor := 11;
	ShowTrajectory;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	IF Escape THEN Exit;
	ShowMouse;
	REPEAT
		Event.readkey := Chr(0);
		ShowSomePlots;
		IF Event.ReadKey in ['C','c'] THEN
		BEGIN
			ClearPhasePort;
			DisplayPotential(False,False)
		END;
	UNTIL (Event.ReadKey in ['Q','q']) OR Escape;
	Animating := False;
END; {DemoAnimation}

PROCEDURE DemoPhaseDiag;
BEGIN
	Debounce;
	HideMouse;
	OpenTextPort(Red);
	SetColor(White);
	Print(14,1,'Phase Diagram');
	Print (2,3,'We can also plot x against v.');
	Print (2,4,'This is called a Phase Diagram.');
	Print (2,5,'I''ll show you the x vs. t graph');
	Print (2,6,'turned on its side beside');
	Print (2,7,'the phase diagram to illustrate');
	Print (2,8,'the  relations between the graphs.');
	Print(2,10,'I''ll also slow the motion to make');
	Print(2,11,'the relations easier to see.');
	Print (2,13,'Watch how the graphs relate');
	Print (2,14,'to each other at their growing ends.');
	Print (10,16,'Click or press ENTER.');
	Message('');
	ClickOrEnter;
	GraphBackColor := DarkGray;
	DelayTime := 10;
	HalfWallSep := 0.265; TimePlotInverted := True; HidePoincare := True;
	NewWall;
	Energy := 0.02;
	SetTextStyle(DefaultFont, HorizDir, {Size=} 1);
	DisplayChoice;
	ShowPeriods := 1; TimeLimit := False;
	ShowXVP;
	NewWall;
	Message('Watch how the graphs relate to each other at their growing ends.');
	Animating := True;
	ShowTrajectory;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	IF Escape THEN Exit;
	Message('Try some more. Press C to clear the graphs; Press G to go on.');
	Animating := False;
	NColor := NColor + 1; IF NColor > 15 THEN NColor := 10;
	SetMyColor(NColor);
	DelayTime := 25;
	REPEAT
		Event.readkey := Chr(0);
		ShowSomePlots;
		IF Event.ReadKey in ['C','c'] THEN
		BEGIN
			ClearPhasePort;
			DisplayPotential(False,False)
		END;
	UNTIL (Event.ReadKey in ['G','g']) OR Escape;
	DelayTIme := 0;
END;

PROCEDURE ShowResistance;
BEGIN
	HideMouse;
	OpenTextPort(Red);
	SetColor(White);
	Print(8,1,'Phase Diagram: Resistance');
	Print (2,3,'The phase diagrams you just saw');
	Print (2,4,'had constant energy. Lets draw');
	Print (2,5,'another constant energy diagram');
	Print (2,6,'and then put one with energy loss');
	Print (2,7,'on top of it.');
	Print (10,9,'Please press k to continue.');
	Message('');
	PressKey('k');
	Message('Let it finish two cycles.');
	DelayTime := 0; DoErase := False;
	HalfWallSep := 0.5; TimePlotInverted := True;
	ShowPeriods := 2; TimeLimit := False;
	GraphBackColor := DarkGray;
	NewWall; ShowXVP;
	SetMyColor(11);
	Animating := True;
	Energy := 0.035;
	SetTextStyle(DefaultFont, HorizDir, {Size=} 1);
	DisplayChoice;
	Comment(0.4,0.45,'Constant Energy');
	ShowTrajectory;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	DoverM := 0.0;
	IF Escape THEN Exit;
	DoverM := 0.2;
	Continuous := True; SetMyColor(12);
	Comment(0.4,0.45,'               ');
	Comment(0.4,0.35,'Energy being Lost');
	Message('Press G to go on when you have seen enough.');
	ShowTrajectory;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	Comment(0.4,0.35,'                 ');
	DoverM := 0.0;
	Message('Lets do it again with a double well. Click or ENTER.');
	ClickOrEnter;
	Message('Let it finish two cycles.');
	IF Escape THEN Exit;
	HalfWallSep := 0.3;
	SetColor(White);
	NewWall; ShowXVP; Continuous := False;
	Energy := 0.016;
	SetTextStyle(DefaultFont, HorizDir, {Size=} 1);
	DisplayChoice;
	SetMyColor(11);
	DoverM := 0.0;
	Comment(0.4,0.45,'Constant Energy');
	ShowTrajectory;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	IF Escape THEN Exit;
	DoverM := 0.1;
	Comment(0.4,0.45,'               ');
	Comment(0.4,0.35,'Energy being Lost');
	Message('Press G to go on when you have seen enough.');
	Continuous := True; SetMyColor(12);
	ShowTrajectory;
	Comment(0.4,0.35,'                 ');
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	DoverM := 0.0;
	Animating := False;
	ShowMouse;
END; {ShowResistance}

PROCEDURE ShowPendulum;
BEGIN
	goverL := 9.8; DoverM := 0.0; F0overM := 0.0;
	SetColor(White);
	ChoosePendulum;
	DrawSystem;
	Energy := 10.0;
	TimePlotInverted := False;
	FindPotential(True);
	DisplayPotential(BothV, HoldVScale);
	SetTextStyle(DefaultFont, HorizDir, {Size=} 1);
	DisplayChoice;
	HideMouse;
	OpenTextPort(Blue);
	Print(14,1,'The Pendulum');
	Print (2,3,'The pendulum is another anharmonic');
	Print (2,4,'oscillator. ');
	Print (10,9,'Click or press ENTER.');
	Message('');
	StaticPause;
	Message('Press G when you have seen enough.');
	SetColor(Blue);
	SelectLeftPort;
	SetTextStyle(SmallFont, HorizDir, {Size=} 6);
	Print (10,9,'Click or press ENTER.');
	SetColor(White);
	Print (5,6,'Press G to see a motion with enough');
	Print (5,7,'energy to swing over the top.');
	DoErase := True; SetMyColor(11); Continuous := True;
	ShowPeriods := 2; TimeLimit := False; Animating := True;
	GraphBackColor := DarkGray;
	SetColor(White);
	SetAllFalse;
	Message('Press G when you have seen enough.');
	ShowTrajectory;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	IF Escape THEN Exit;

	Event.ReadKey := Chr(0);
	SetColor(Blue);
	SelectLeftPort;
	SetTextStyle(SmallFont, HorizDir, {Size=} 6);
	Print (5,6,'Press G to see a motion with enough');
	Print (5,7,'energy to swing over the top.');
	SetColor(White);
	Print (5,6,'Press G to see the graphs.');
	Energy := 21.0;
	SetTextStyle(DefaultFont, HorizDir, {Size=} 1);
	DisplayChoice;
	ShowTrajectory;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	IF Escape THEN Exit;

	Energy := 10.0; 
	TimePlotInverted := False;
	DisplayChoice;
	DelayTime := 100; SetMyColor(11);
	ShowPeriods := 2; TimeLimit := False; Animating := True;
	GraphBackColor := DarkGray;
	SetColor(White);
	Message('Press G when you have seen enough.');
	ShowXVP;
	ShowTrajectory;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	IF Escape THEN Exit;
	Message('Click or ENTER.');
	StaticPause;
	ShowMouse;
	Animating := False
END; {ShowPendulum}

PROCEDURE PendulumTwo;
BEGIN
	Debounce;
	Energy := 21.0; 
	TimePlotInverted := False;
	DisplayChoice;
	HideMouse;
	OpenTextPort(Blue);
	SetColor(White);
	Print(14,1,'More Pendulum');
	Print (2,3,'Now let it swing over the top again.');
	Print (2,4,'Notice that now the velocity is');
	Print (2,5,'always positive and the phase plot');
	Print (2,6,'point moves in one direction only.');
	Print (2,7,'The angle x increases continually,');
	Print (2,8,'and we can only represent it by ');
	Print (2,9,'going back to the first edge and');
	Print (2,10,'starting over.');
	Print (10,12,'Click or press ENTER.');
	Message('');
	ClickOrEnter;
	Plus := True;
	ShowPeriods := 2; TimeLimit := False; Animating := True;
	GraphBackColor := DarkGray;
	Message('Press G when you have seen enough.');
	ShowXVP;
	DelayTime := 100; SetMyColor(11);
	ShowTrajectory;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	IF Escape THEN Exit;
	Message('Click or ENTER.');
	StaticPause;
	ShowMouse;
	Message('Make some of your own (click in ''Potential'' diagram);'
				 +' C clears, G goes on.');
	Animating := False; Continuous := False;
	SetMyColor(11);
	REPEAT 
		Event.readkey := Chr(0);
		ShowSomePlots;
		IF Event.ReadKey in ['C','c'] THEN
		BEGIN
			ClearPhasePort;
			DisplayPotential(False,False)
		END;
	UNTIL (Event.ReadKey in ['G','g']) OR Escape;
END; {PendulumTwo}

PROCEDURE PendulumR;
BEGIN
	Message('Now add air resistance. (G when you have seen enough.)');
	DoverM := 0.15;
	Energy := 28.0;
	SetTextStyle(DefaultFont, HorizDir, {Size=} 1);
	DisplayChoice;
	Animating := True; DelayTime := 0; ShowPeriods := 20;
	DisplayPotential(BothV, HoldVScale);
	ClearPhasePort;
	Comment(0.41,0.45,'Air Resistance');
	Comment(0.392,0.35,'Energy being Lost');
	ShowTrajectory;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	IF Escape THEN Exit;
	Message('Click or ENTER.');
	StaticPause;
	Comment(0.41,0.45,'              ');
	Comment(0.392,0.35,'                 ');
	Animating := False
END; {PendulumR}

PROCEDURE DrivenSystem;
BEGIN
	Debounce;
	HideMouse;
	CloseAllPort;
	ChooseWalls; DrawSystem;
	Energy := 0.02; 
	SetTextStyle(DefaultFont, HorizDir, {Size=} 1);
	DisplayChoice;
	DeBounce;
	OpenTextPort(Red);
	Print(9,1,'Uses of the Phase Diagram');
	Print (2,3,'The phase diagram can tell us a lot');
	Print (2,4,'about more complicated motions.');
	Print (2,6,'A driven system can have a simple');
	Print (2,7,'motion like this:');
	Print (10,9,'Please press p.');
	Message('');
	PressKey('p');
	Plus := True; ShowPeriods := 15000; TimeLimit := False;
	HidePoincare := True;
	HalfWallSep := 0.4; DoverM := 0.1; F0overM := 0.3; omega := 0.45;
	MaxPos := 1.0; MaxVel := 1.5;
	GraphBackColor := DarkGray;
	SetTextStyle(DefaultFont, HorizDir, {Size=} 1);
	NewWall; DisplayChoice; DisplayDissip;
	REPEAT {UNTIL No}
		ShowXVP;
		Comment(0.35,0.45,'To see the simplicity,');
		Comment(0.35,0.4 ,'Press C after a while.');
		SetMyColor(11); NColor := 11;
		Message('Press C occasionally to clear the screen & keep plotting;'
				  +' Press G to go on.');
		ShowTrajectory;
		IF (Event.ReadKey = Chr(27)) THEN Escape := True;
		IF Escape THEN Exit;
		OpenTextPort(Red);
		Print(13,1,'A Limit Cycle');
		Print (2,3,'The motion very quickly settled');
		Print (2,4,'down to a simple cycle. This is');
		Print (2,5,'called a `limit'' cycle.');
		Print (2,6,'If you weren''t able to see that,');
		Print (2,7,'press C (as often as you like)');
		Print (2,8,'to clear the screen.');
		Print (5,10,'Would you like to see that again?');
		Print (10,11,'(Y or N)');
		REPEAT CheckEscape UNTIL (Event.Readkey in ['N','n','Y','y']) OR Escape;
		GraphBackColor := DarkGray
	UNTIL (Event.ReadKey in ['N','n']) OR Escape;
	IF Escape THEN Exit;
	F0overM := 0.0;
	DoverM := 0.0;
	Message('or a more complicated motion like this: '
				+'(Press C now and then, G to go on)');
	HalfWallSep := 0.4; DoverM := 0.00001; F0overM := 0.2; omega := 0.45;
	SetTextStyle(DefaultFont, HorizDir, {Size=} 1);
	DisplayChoice; DisplayDissip;
	REPEAT {Until No}
		ShowXVP;
		Comment(0.35,0.45,'Press C every now and');
		Comment(0.35,0.4 ,'then.                ');
		SetMyColor(11); NColor := 11;
		ShowTrajectory;
		IF (Event.ReadKey = Chr(27)) THEN Escape := True;
		IF Escape THEN Exit;
		OpenTextPort(Red);
		Print(11,1,'Another Limit Cycle?????');
		Print (2,3,'This time the motion may be');
		Print (2,4,'repeating itself, but if it does');
		Print (2,5,'it is only after many cycles of the');
		Print (2,6,'driving force; it is hard to be sure.');
		Print (2,7,'Even after repeated clearing, the');
		Print (2,8,'phase plot traces a complex orbit.');
		Print (5,10,'Would you like to see that again?');
		Print (10,11,'(Y or N)');
		REPEAT CheckEscape UNTIL (Event.Readkey in ['N','n','Y','y']) OR Escape;
		GraphBackColor := DarkGray
	UNTIL (Event.ReadKey in ['N','n']) OR Escape;
	IF Escape THEN Exit;
	F0overM := 0.0;
	DoverM := 0.0;
	Message('or a STILL more complicated motion like this: '
	+'(Press C now and then, G to go on)');
	HalfWallSep := 0.25; DoverM := 0.2; F0overM := 0.2; omega := 0.45;
	NewWall; DisplayChoice; DisplayDissip;
	REPEAT {Until No}
		ShowXVP;
		SetMyColor(11); NColor := 11;
		ShowTrajectory;
		IF (Event.ReadKey = Chr(27)) THEN Escape := True;
		IF Escape THEN Exit;
		OpenTextPort(Red);
		Print(7,1,'Still Another Limit Cycle?????');
		Print (2,3,'This motion is even more complex');
		Print (2,4,'and it is even more difficult to');
		Print (2,5,'tell if it ever repeats.');
		Print (2,6,'In a moment, we will look at another');
		Print (2,7,'way of analyzing these motions.');
		Print (5,10,'But first, would you like to see');
		Print (10,11,'THAT again? (Y or N)');
		REPEAT CheckEscape UNTIL (Event.Readkey in ['N','n','Y','y']) OR Escape;
		GraphBackColor := DarkGray
	UNTIL (Event.ReadKey in ['N','n']) OR Escape;
	ShowMouse; ShowMouse; {Can't figure out why twice, but necessary}
	F0overM := 0.0;
	DoverM := 0.0
END; {DrivenSystem}

PROCEDURE DrivenSystem2;
BEGIN
	Debounce;
	CloseAllPort;
	ChooseWalls; DrawSystem;
	Energy := 0.02; DelayTime := 0;
	ShowPeriods := 15000; TimeLimit := False; NPhases := 18;
	HideMouse;
	OpenTextPort(Red);
	Print(13,1,'The Poincar‚ Map');
	Print (2,3,'The phase plot is giving us too');
	Print (2,4,'much information to understand');
	Print (2,5,'readily, especially since it is');
	Print (2,6,'all on top of itself. To get');
	Print (2,7,'around this, we can take snapshots');
	Print (2,8,'of the position in the phase');
	Print (2,9,'diagram at regular time intervals.');
	Print (2,10,'This is called a Poincar‚ Map');
	Print (2,11,'after its 19th century inventor.');
	Print (2,13,'The snapshot points will be');
	Print (2,14,'highlighted in the phase diagram');
	Print (2,15,'as well as the new Poincar‚ diagram.');
	Print (10,17,'Click or press ENTER.');
	Message('');
	Comment (0.72,0.40,'Animation is off;');
	Comment (0.72,0.33,'it slows things');
	Comment (0.72,0.26,'down too much.');
	ClickOrEnter;
	Plus := True; ShowPeriods := 15000; TimeLimit := False;
	HidePoincare := False; DemoPoincare := True;
	HalfWallSep := 0.4; DoverM := 0.1; F0OverM := 0.3; omega := 0.45;
	MaxPos := 1.0; MaxVel := 1.5;
	GraphBackColor := DarkGray;
	NewWall; DisplayChoice; DisplayDissip;
	REPEAT
		ShowXVP;
		SetMyColor(11); NColor := 11;
		Message('After a moment, Press C to clear;'
				  +' Press G to go on. (Simplest motion you saw before)');
		ShowTrajectory;
		IF (Event.ReadKey = Chr(27)) THEN Escape := True;
		IF Escape THEN Exit;
		OpenTextPort(Red);
		Print(2,1,'The Simplest Limit Cycle Poincar‚ Map');
		Print (2,3,'The first few snapshots were in');
		Print (2,4,'different places, but after that');
		Print (2,5,'all snapshots fell on top of each');
		Print (2,6,'other. That is especially plain if you');
		Print (2,7,'clear the screen after the motion');
		Print (2,8,'settles down.');
		Print (5,10,'Would you like to see that again?');
		Print (10,11,'(Y or N)');
		REPEAT CheckEscape UNTIL (Event.Readkey in ['N','n','Y','y']) OR Escape;
		GraphBackColor := DarkGray
	UNTIL (Event.ReadKey in ['N','n']) OR Escape;
	DoverM := 0.0; F0overM := 0.0; DemoPoincare := False
END; {DrivenSystem2}

Procedure Intro2a;
BEGIN
	CloseAllPort;
	ChooseWalls;
	Debounce;
	OpenTextPort(Red);
	Print(15,1,'Poincar‚ Maps');
	Print (2,3,'These are snapshots of the');
	Print (2,4,'phase diagram taken at regular');
	Print (2,5,'time intervals. The following is');
	Print (2,6,'an example of a complex but cyclic');
	Print (2,7,'motion. Press C to clear the screen');
	Print (2,8,'now and then, G to go on.');
	Print (7,10,'Press ENTER or click.');
	ClickOrEnter;
	ShowMouse;
	GraphBackColor := DarkGray
END; {Intro2a}

PROCEDURE DrivenSystem2a;
BEGIN
	GraphBackColor := DarkGray; Ncolor := LightCyan; SetColor(NColor);
	MaxPos := 1.0; MaxVel := 1.5; DemoPoincare := True;
	ShowPeriods := 15000; TimeLimit := False; NPhases := 18;
	Energy := 0.02;
	NewWall;
	Message('Let the motion go on until the Poincar‚ map begins to repeat.'
			 +' (C, G)');
	HalfWallSep := 0.4; DoverM := 0.00001; F0overM := 0.2; omega := 0.45;
	DisplayChoice; DisplayDissip;
	HideMouse;
	REPEAT
		ShowXVP;
		SetMyColor(11); NColor := 11;
		ShowTrajectory;
		IF (Event.ReadKey = Chr(27)) THEN Escape := True;
		IF Escape THEN Exit;
		OpenTextPort(Red);
		Print(2,1,'A More Complex Limit Cycle Poin. Map');
		Print (2,3,'This time the snapshots are NOT');
		Print (2,4,'all in the same place, but they');
		Print (2,5,'do fall onto a cycle, telling us');
		Print (2,6,'that the motion DOES eventually');
		Print (2,7,'repeat. There IS a limit cycle,');
		Print (2,8,'but it is long. These cycles are');
		Print (2,9,'also called ATTRACTORS.');

		Print (5,11,'Would you like to see that again');
		Print (10,12,'faster? (Y or N)');

		Print (2,14,'From now on, the Poincar‚ points');
		Print (2,15,'will no longer be highlighted.');
		Print (2,17,'Next: the most complex motion');
		Print (2,18,' you saw before.');
		REPEAT CheckEscape UNTIL (Event.Readkey in ['N','n','Y','y']) OR Escape;
		DemoPoincare := False;
		GraphBackColor := DarkGray
	UNTIL (Event.ReadKey in ['N','n']) OR Escape;
	IF Escape THEN Exit;
	F0overM := 0.0;
	DoverM := 0.0;
	Message('Let the motion go on a good long time. (C, G)');
	HalfWallSep := 0.25; DoverM := 0.2; F0overM := 0.2; omega := 0.45;
	NewSystem({Clear:} True); DisplayChoice; DisplayDissip;
	NPhases := 32;
	{ShowXVP;}
	REPEAT
	SetMyColor(11); NColor := 11;
	ShowTrajectory;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	IF Escape THEN Exit;
	OpenTextPort(Red);
	Print(10,1,'A Strange Attractor');
	Print (2,3,'The Poincar‚ map still has structure,');
	Print (2,4,'but it now seems to have a layered');
	Print (2,5,'structure (if you let it go long');
	Print (2,6,'enough). We can confirm that by');
	Print (2,7,'looking at a movie made by taking');
	Print (2,8,'Several snapshots regularly spaced');
	Print (2,9,'in between those you just saw.');
	Print (2,12,'Click or ENTER to go on');
	ClickOrEnter;
	ShowMouse;
	GraphBackColor := DarkGray;
	ShowPoinAlone;
	REPEAT
		FlipEm({Slow:} True, {ReturnMap: } False);
		HideMouse;
		OpenTextPort(Red);
		Print(10,1,'A Strange Attractor');
		SetTextStyle(SmallFont, HorizDir, 5);
		Print (2,2,'Notice the way the layers fold over on each');
		Print (2,3,'other and build up, like making layered');
		Print (2,4,'pastry. They in fact form an infinite number');
		Print (2,5,'of layers in a FRACTAL structure. This is');
		Print (2,6,'called a STRANGE attractor.');

		Print (2,8,'The structure is made visible by taking the');
		Print (2,9,'snapshots at just the right times: one period');
		Print (2,10,'of the driving force apart for each');
		Print (2,11,'frame of the movie.');

		Print (2,13,'You may be able to see the structure better');
		Print (2,14,'using the F6 key to pause the diagram.');

		Print (5,16,'Would you like to see the movie again?');
		Print (10,17,'(Y or N)');
		REPEAT CheckEscape UNTIL (Event.Readkey in ['N','n','Y','y']) OR Escape;
		ShowMouse;
		GraphBackColor := DarkGray
	UNTIL (Event.ReadKey in ['N','n']) OR Escape;
	OpenTextPort(Blue);
	Print (2,2,'We can make the Poincar‚ map much');
	Print (2,3,'faster if we turn off the other');
	Print (2,4,'graphs.');

	Print (2,6,'More points makes the layered');
	Print (2,7,'structure more apparent.');

	Print (2,9,'Would you like to make the movie');
	Print (2,10,'again faster with more points?');
	Print (10,11,'(Y or N)');
	REPEAT CheckEscape UNTIL (Event.Readkey in ['N','n','Y','y']) OR Escape;
	Continuous := True; {Evidently get turned off somewhere}
	Message('Press G when you have seen enough.');
	ClearPhaseport;
	UNTIL (Event.ReadKey in ['N','n']) OR Escape;
	F0overM := 0.0;
	DoverM := 0.0;
END; {DrivenSystem2a}

PROCEDURE DrivenSystem3;
BEGIN
	HalfWallSep := 0.4; DoverM := 0.00001; F0overM := 0.2; omega := 0.45;
	Energy := 0.02;
	Continuous := True;
	MaxPos := 1.0; MaxVel := 1.5; DelayTime := 0;
	CloseAllPort;
	Message ('');
	DrawSystem;
	ShowPeriods := 15000; TimeLimit := False; NPhases := 18;
	HideMouse;
	OpenTextPort(Red);
	Print(12,1,'A Reprise');
	Print (2,3,'Lets go back and look at the');
	Print (2,4,'previous limit cycle Poincar‚ map');
	Print (2,5,'to see if it has the layered fractal');
	Print (2,6,'structure.');
	Print (2,8,'Click or ENTER to go on');
	ClickOrEnter;
	ShowMouse;
	GraphBackColor := DarkGray;
	NewWall;
	DisplayChoice;
	SetAllFalse; Plots := 2; PlotPhase := True; PlotPoincare := True;
	SetPlots;
	SetMyColor(11); NColor := 11;
	REPEAT
		Message('Let the Poincar‚ Map come back at least to its starting point, then press G)');
		ClearPhasePort;
		ShowTrajectory;
		IF (Event.ReadKey = Chr(27)) THEN Escape := True;
		IF Escape THEN Exit;
		FlipEm({Slow:} True, {ReturnMap: } False);
		HideMouse;
		OpenTextPort(Red);
		Print(8,1,'An Ordinary attractor');
		Print (2,3,'The diagram rotates, but the');
		Print (2,4,'layers do not fold in on each');
		Print (2,5,'other and build up a fractal ');
		Print (2,6,'structure.');
		Print (2,7,'This is an attractor, but not a ');
		Print (2,8,'strange one.');
		Print (5,11,'Would you like to see that again?');
		Print (5,12,'(with the phase map turned off');
		Print (5,13,'to go faster)?) (Y or N)');
		REPEAT CheckEscape UNTIL (Event.Readkey in ['N','n','Y','y']) OR Escape;
		GraphBackColor := DarkGray;
{		ShowPoinAlone;}
		ShowMouse
	UNTIL (Event.ReadKey in ['N','n']) OR Escape;
	F0overM := 0.0;
	DoverM := 0.0
END; {DrivenSystem3}

PROCEDURE PendulumChaos;
BEGIN
	gOverL := 1.0;
	ChoosePendulum;
	DrawSystem;
	Message('');
	Energy := 1.0;
	DisplayChoice;
	F0OverM := 0.7; omega := 0.7; DoverM := 0.16;
	ShowPeriods := 15000; TimeLimit := False; NPhases := 18;
	DisplayChoice; DisplayDissip;
	OpenTextPort(Red);
	MaxPos := Pi;
	HideMouse;
	OpenTextPort(Blue);
	Print(10,1,'The Pendulum');
	Print (2,3,'The pendulum has strange attractors ');
	Print (2,4,'too. As with the spring system,');
	Print (2,5,'strange attractors occur for');
	Print (2,6,'particular initial conditions. The');
	Print (2,7,'study of when strange attractors occur');
	Print (2,8,'is a large part of the study of Chaos.');
	Print (2,9,'Let this run until you have a nice');
	Print (2,10,'thick Poincar‚ diagram.');
	Print (2,12,'Please press t.');
	PressKey('t');
	GraphBackColor := DarkGray; Continuous := True;
	SetAllFalse; Plots := 2; PlotPhase := True; PlotPoincare := True;
	SetPlots;
	SetMyColor(11); NColor := 11;
	REPEAT
	Message('Build up a good thick Poincar‚ Map then press G)');
	ClearPhasePort;
	ShowTrajectory;
	IF (Event.ReadKey = Chr(27)) THEN Escape := True;
	IF Escape THEN Exit;
	ShowMouse;

	FlipEm({Slow:} True, {ReturnMap: } False);
	HideMouse;
	OpenTextPort(Blue);
	Print(10,1,'A Strange Attractor');
	Print (2,3,'The layered structure and the folding');
	Print (2,4,'may be more apparent here. Certainly');
	Print (2,5,'the shape is different.');
	Print (5,11,'Would you like to see that again');
	Print (5,12,'(with the phase map turned off');
	Print (5,13,'to go faster)?) (Y or N)');
	REPEAT CheckEscape UNTIL (Event.Readkey in ['N','n','Y','y']) OR Escape;
	ShowMouse;
	GraphBackColor := DarkGray
{	ShowPoinAlone}
	UNTIL (Event.ReadKey in ['N','n']) OR Escape;
	F0overM := 0.0;
	DoverM := 0.0
END; {PendulumChaos}

PROCEDURE EndScreen;
BEGIN
	HideMouse;
	GraphBackColor := Red;
	OpenAllPort;
	SetTextStyle(TriplexFont, HorizDir, {Size=} 4);
	Print(13,2,'The Anharmonic Oscillator');
	SetUserCharSize(2,3,2,3);
	Print (15, 5, 'All the facilities of the program will ');
	Print (15, 7, 'now be available through the menus');
	Print (15, 9, 'at the top of the screen.');
	SetUserCharSize(1,2,1,2);
	Print (25, 12, 'Please click or press ENTER.');
	ShowMouse;
	ClickOrEnter;
	CloseAllPort;
END; {EndScreen}

PROCEDURE IntroWalls;
BEGIN
	Debounce;
	CloseAllPort;
	ChooseWalls;
	TimePlotInverted := False;
	ChooseWalls; DrawSystem;
	ShowMouse;
	SetMyColor(LightCyan);
	DescribeOsc;
	IF Escape THEN Exit;
	MoveCloser;
	IF Escape THEN Exit;
	CloserStill;
	IF Escape THEN Exit;
	TwoEquilibria;
	IF Escape THEN Exit;
	PracticeMoving;
	IF Escape THEN Exit;
	DiscussPotential;
	IF Escape THEN Exit;
	ShowApprox;
END; {IntroWalls}

PROCEDURE DemoAnimate;
BEGIN
	CloseAllPort;
	ChooseWalls;
	SetMyColor(LightCyan);
	ShowMouse;
	DemoXandV;
	IF Escape THEN Exit;
	DemoAnimation;
END; {DemoAnimate}

PROCEDURE DemoPend;
BEGIN
	Debounce;
	CloseAllPort;
	ChooseWalls;
	SetMyColor(LightCyan);
	ShowMouse;
	DemoPhaseDiag;
	IF Escape THEN Exit;
	ShowResistance;
	IF Escape THEN Exit;
	ShowPendulum;
	IF Escape THEN Exit;
	PendulumTwo;
	IF Escape THEN Exit;
	PendulumR;
END; {DemoPend}

PROCEDURE ShowFFT(Test, Windowed, ClearIt, ZoomSet, DoTransform : Boolean;
						Period, Zoom : Real;
						Shift, Points, Shape, Color1 : Integer);
VAR NumData : Word;
	 TempIntutorial : Boolean;
BEGIN
	NumData := 512;
	TempInTutorial := Intutorial;
	InTutorial := True; {Always on for setup}
	SetUpFourierTransform
		(NumData,Test,Windowed,ClearIt,Period,Shift,Points,Shape,Color1);
	InTutorial := TempInTutorial; {Sometimes off to allow Zoom}
	IF DoTransform THEN
	BEGIN
		FastFourierTransform;
		PlotFourierTransform(NumData, Color1, green, Test,
							ZoomSet, ClearIt, Zoom)
	END
END; {ShowFFT}

PROCEDURE FFTDemo1;
BEGIN
	OpenTextPort(Red);
	Print (10, 1, 'Frequency Analysis');
	Print (2,3, 'When you analyze the history of an');
	Print (2,4, 'oscillator to see the frequencies');
	Print (2,5, 'in it, you need to be aware of the');
	Print (2,6, 'numerical pitfalls you can fall into.');

	Print (2,8, 'The analysis works best when a whole');
	Print (2,9, 'number of cycles takes a number of');
	Print (2,10,'times steps equal to a power of two:');

	Print (10, 12, 'Please click or press ENTER.');
	ClickOrEnter;
	GraphBackColor := DarkGray;
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} True, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 6.4,{Zoom=} 1.0,{Shift=} 0,{Points=} 128,
			{Shape(Triangle)=} 3, {Color1=} 11);
	OpenRightTextPort(Blue);
	Print(2,3,'Two periods just fit');
	Print(2,4,'128 time steps.');
	Print(2,6,'Notice the constant');
	SetColor(Green); Print(2,7,'phase');
	Setcolor(White); Print (9,7, 'graph.');
	PressKeyMessage;
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} False, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 5.2,{Zoom=} 1.0,{Shift=} 0,{Points=} 128,
			{Shape(Triangle)=} 3, {Color1=} 12);
	SelectMyViewPort(RightPort);
	Print (2,10,'shorter,');
	SetColor(white);
	Print (2,1,'Numerical Artifacts');
	Print (2,9,'If the period is');
	Print (12,10,'the graph');
	Print (2,11,'doesn''t drop to zero');
	Print (2,12,'where it should, and');
	Print (2,13,'the       wiggles.');
	SetColor(Green); Print(7,13,'phase');
	PressKeyMessage;
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} False, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 7.4,{Zoom=} 1.0,{Shift=} 0,{Points=} 128,
			{Shape(Triangle)=} 3, {Color1=} 13);
	SelectMyViewPort(RightPort);
	Print (2,16, 'longer');
	SetColor(white);
	Print (2,15, 'Likewise with a');
	Print (11,16, 'period.');
	PressKeyMessage;
	OpenRightTextPort(Blue);
	Print (2,1,'Numerical Artifacts');
	SetColor(14);
	Print (2,3,'Cutting oscillation');
	Print (2,4,'off'); Setcolor(White); Print (6,4, 'after two cycles');
	Print (2,5,'doesn''t work either.');
	GraphBackColor := DarkGray;
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} True, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 6.4,{Zoom=} 1.0,{Shift=} 0,{Points=} 128,
			{Shape(Triangle)=} 3, {Color1=} 11);
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} False, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 5.2,{Zoom=} 1.0,{Shift=} 0,{Points=} 102,
			{Shape(Triangle)=} 3, {Color1=} 14);
	PressKeyMessage;
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} False, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 5.2,{Zoom=} 1.0,{Shift=} 12,{Points=} 102,
			{Shape(Triangle)=} 3, {Color1=} 12);
	SelectMyViewPort(RightPort);
	Print (2,8,'beginning value');
	Setcolor(White);
	Print (2,7,'Adjusting the');
	Print (2,9, 'does a little better,');
	Print (2,10, 'but the       still');
	Print (2,11,'wiggles,');
	Print (2,13,'and the peaks are');
	Print (2,14,'broader than with');
	Print (2,15,'the period that');
	SetColor(11);
	Print (2,16,'fits 128 timesteps.');
	Setcolor(Green); Print(11,10,'phase');
	PressKeyMessage;
	CloseMyViewPort(RightPort)
END; {FFTDemo1}

Procedure FFTDemo2;
BEGIN
	OpenTextPort(Blue);
	Print (10, 1, 'Numerical Artifacts');
	Print (2,3,'These artifacts are a consequence of');
	Print (2,4,'two facts:');
	Print (2,6,'1) Frequency analysis works best on');
	Print (2,7,'periodic functions.');
	PressKeyMessage;
	Print (2,9,'2) Frequency analysis is far faster');
	Print (2,10,'when the number of points is a power');
	Print (2,11,'of two.');
	PressKeyMessage;
	Print (2,13,'A number of points less than a power');
	Print (2,14,'of two is handled by adding zeros.');
	PressKeyMessage;
	OpenTextPort(Blue);
	Print (13,1,'Windowing');
	Print (2,3,'A partial solution is what is called');
	Print (2,4,'`windowing''.');
	Print (2,6,'Windowing means multiplying the time');
	Print (2,7,'history by a function that is zero at');
	Print (2,8,'each end and large in the middle.');
	PressKeyMessage;
	Print (2,10,'Like this:');
	ShowFFT({Test=} True,{Windowed=} True,{Clear=} False, {ZoomSet=} True,
			{DoTransform=} False,
			{Period=} 1.2,{Zoom=} 1.0,{Shift=} 12,{Points=} 108,
			{Shape(Triangle)=} 3, {Color1=} White);
	PressKeyMessage;
	OpenRightTextPort(Blue);
	Print (8,1,'Windowing');
	Print(2,3,'Here are pairs of the');
	Print(2,4,'functions you saw');
	Print(2,5,'before, both:');
	Setcolor(12); Print(7,7,'Unwindowed');
	Setcolor(14); Print(7,9,' Windowed');
	Setcolor(White); Print(11,8,'and');
	Print(5,11,'Longer Period');
	GraphBackColor := DarkGray;
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} True, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 7.4,{Zoom=} 1.0,{Shift=} 0,{Points=} 128,
			{Shape(Triangle)=} 3, {Color1=} 12);
	ShowFFT({Test=} True,{Windowed=} True,{Clear=} False, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 7.4,{Zoom=} 1.0,{Shift=} 0,{Points=} 128,
			{Shape(Triangle)=} 3, {Color1=} 14);
	PressKeyMessage;
	SetColor(White);
	SelectMyViewPort(RightPort);
	Print(5,12,'Shorter Period');
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} True, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 5.2,{Zoom=} 1.0,{Shift=} 0,{Points=} 128,
			{Shape(Triangle)=} 3, {Color1=} 12);
	ShowFFT({Test=} True,{Windowed=} True,{Clear=} False, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 5.2,{Zoom=} 1.0,{Shift=} 0,{Points=} 128,
			{Shape(Triangle)=} 3, {Color1=} 14);
	PressKeyMessage;
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} True, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 5.2,{Zoom=} 1.0,{Shift=} 0,{Points=} 102,
			{Shape(Triangle)=} 3, {Color1=} 12);
	ShowFFT({Test=} True,{Windowed=} True,{Clear=} False, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 5.2,{Zoom=} 1.0,{Shift=} 0,{Points=} 102,
			{Shape(Triangle)=} 3, {Color1=} 14);
	SetColor(White);
	SelectMyViewPort(RightPort);
	Print(5,13,'Shortened');
	PressKeyMessage;
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} True, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 5.2,{Zoom=} 1.0,{Shift=} 12,{Points=} 102,
			{Shape(Triangle)=} 3, {Color1=} 12);
	ShowFFT({Test=} True,{Windowed=} True,{Clear=} False, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 5.2,{Zoom=} 1.0,{Shift=} 12,{Points=} 102,
			{Shape(Triangle)=} 3, {Color1=} 14);
	SetColor(White);
	SelectMyViewPort(RightPort);
	Print(5,14,'Shifted Left');
	PressKeyMessage;
	OpenRightTextPort(Blue);
	Print(7,1,'Windowing');
	Print(2,3,'You may have been able');
	Print(2,4,'tell that windowing is');
	Print(2,5,'not a perfect solution.');
	Print(2,7,'For example, here is');
	Print(2,8,'its effect on the');
	Print(2,9,'function that doesn''t');
	Print(2,10,'need it:');
	Print(11,13,'and');
	Print(2,16,'Therefore, you will');
	Print(2,17,'always have a choice:');
	Print(2,18,'use windowing or not.');
	Setcolor(12); Print(7,12,'Unwindowed');
	Setcolor(14); Print(7,14,' Windowed');
	GraphBackColor := DarkGray;
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} True, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 6.4,{Zoom=} 1.0,{Shift=} 0,{Points=} 128,
			{Shape(Triangle)=} 3, {Color1=} 12);
	ShowFFT({Test=} True,{Windowed=} True,{Clear=} False, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 6.4,{Zoom=} 1.0,{Shift=} 0,{Points=} 128,
			{Shape(Triangle)=} 3, {Color1=} 14);
	PressKeyMessage
END; {FFTDemo2}

Procedure FFTDemo3;
BEGIN
	OpenRightTextPort(Blue);
	Print(9,1,'Resolution');
	Print(2,3,'The longer the time');
	Print(2,4,'used in the analysis,');
	Print(2,5,'the narrower peaks');
	Print(2,6,'will be under ideal');
	Print(2,7,'conditions previously');
	Print(2,8,'described.');
	Print(5,11,'(Same Period)');
	SetColor(12); Print(6,10,'128 points');
	SetColor(14); Print(6,12,'512 points');
	GraphBackColor := DarkGray;
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} True, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 6.4,{Zoom=} 1.0,{Shift=} 0,{Points=} 128,
			{Shape(Triangle)=} 3, {Color1=} 12);
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} False,{ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 6.4,{Zoom=} 1.0,{Shift=} 0,{Points=} 512,
			{Shape(Triangle)=} 3, {Color1=} 14);
	PressKeyMessage;
	SelectMyViewPort(RightPort); Setcolor(white);
	Print(2,14,'For a change, here');
	Print(2,15,'is the same period');
	Print(2,16,'square wave');
	Print(2,17,'(512 points).');
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} True, {ZoomSet=} True,
			{DoTransform=} True,
			{Period=} 6.4,{Zoom=} 1.0,{Shift=} 0,{Points=} 512,
			{Shape(Square)=} 2, {Color1=} 12);
	PressKeyMessage;
	OpenRightTextPort(Blue);
	Print(6,1,'Low Frequency Detail');
	Print(2,3,'Often, interesting');
	Print(2,4,'details are found at');
	Print(2,5,'low frequency.');
	Print(2,14,'Zoom is provided to');
	Print(2,15,'examine them.');
	Print(6,17,'Try it out');
	InTutorial := False; {to allow Zoom repetition}
	ShowFFT({Test=} True,{Windowed=} False,{Clear=} True, {ZoomSet=} False,
			{DoTransform=} True,
			{Period=} 6.4,{Zoom=} 1.0,{Shift=} 0,{Points=} 512,
			{Shape(Square)=} 2, {Color1=} 12);
	InTutorial := True;
END;

Procedure Repeater(VAR Enough : Boolean);
BEGIN
	IF Escape THEN Enough := True
	ELSE
	BEGIN
		HideMouse;
		OpenTextPort(Red);
		Print (4,2,'Would you like to repeat the last');
		Print (4,3,'section? (Y or N)');
		Print (4,5,'ESC returns to the menu.');
		REPEAT
			CheckEscape;
			If Event.Mouseclicked Then MouseMess
		UNTIL (Event.Readkey in ['N','n','Y','y',Char(27)])
			 OR Escape;
		IF Event.ReadKey = Char(27) THEN Escape := True;
		IF Event.ReadKey in ['N','n'] THEN Enough := True ELSE Enough := Escape;
		ShowMouse
	END {ELSE}
END; {Repeater}

VAR Enuf : Boolean;

PROCEDURE FFTDemo;
BEGIN
	TimeStepUsed := 0.1;
	GraphBackColor := Black;
	CloseMyViewport(AllPort);
	REPEAT FFTDemo1; Repeater(Enuf) UNTIL Enuf;
	IF Escape THEN Exit;
	REPEAT FFTDemo2; Repeater(Enuf) UNTIL Enuf;
	IF Escape THEN Exit;
	REPEAT FFTDemo3; Repeater(Enuf) UNTIL Enuf;
	IF Escape THEN Exit;
END;

PROCEDURE Whole;
BEGIN
	REPEAT IntroWalls; Repeater(Enuf) UNTIL Enuf;
	IF Escape THEN Exit;
	REPEAT DemoAnimate; Repeater(Enuf) UNTIL Enuf;
	IF Escape THEN Exit;
	REPEAT DemoPend; Repeater(Enuf) UNTIL Enuf;
	IF Escape THEN Exit;
	REPEAT DrivenSystem; {Phase Plots} Repeater(Enuf) UNTIL Enuf;
	IF Escape THEN Exit;
	REPEAT DrivenSystem2; {PoinCare Maps} Repeater(Enuf) UNTIL Enuf;
	IF Escape THEN Exit;
	REPEAT DrivenSystem2a; {Flips PoinCare Maps}
			 IF Not Escape THEN DrivenSystem3; {Flips Poincare Maps} 
			 Repeater(Enuf)
	UNTIL Enuf;
	IF Escape THEN Exit;
	REPEAT 	PendulumChaos; Repeater(Enuf) UNTIL Enuf;
	IF Escape THEN Exit;
	FFTDemo
END; {Whole}

VAR QuitTutorial, TutorialChosen : Boolean;

PROCEDURE Tutorial;
BEGIN {Tutorial Main}
	ShowMenu := False; TimeLimit := True;
	SaveDelay := DelayTime;
	SavePeriod := ShowPeriods;
	SaveErase := DoErase;
	DoErase := True;
	InTutorial := True;
	Animating := False; {Was assumption made when tutorial first written}
	ShowMouse;

	REPEAT {Until Finished with Tutorial}
		Escape := False;
		Message ('');
		GraphBackColor := Red;
		SetColor(White);
		OpenAllPort;
		SetTextStyle(TriplexFont, HorizDir, {Size=} 4);
		Print(13,2,'The Anharmonic Oscillator');
		SetUserCharSize(1,2,1,2);
		Print (11, 5, 'Tutorial parts available: press number or click mouse.');
		Print (15, 7, '1- Entire Tutorial');
		Print (15, 8, '2- Introduction to Two Wall system and broken symmetry');
		Print (15, 9, '3- Introduction to motion and animation');
		Print (15, 10, '4- Introduction to Phase Diagrams; the Pendulum');
		Print (15, 11, '5- More on Phase Diagrams and their uses');
		Print (15, 12, '6- Introduction to Poincar‚ Maps');
		Print (15, 13, '7- Poincar‚ Maps in motion');
		Print (15, 14, '8- Harmonic Analysis');
		Print (15, 15, '9- Leave tutorial');

		Print (11, 17, 'Press ESC at almost any time to return to this screen');
		QuitTutorial := False;
		GraphBackColor := Black; {For least visual fuss setting up}
		REPEAT {UNTIL Tutorial Part Chosen}
		TutorialChosen := False;
		CheckForEvents;
		If Event.readKey in ['1','2','3','4','5','6','7','8','9',Chr(27)] THEN
		BEGIN
			Message('');
			CASE Event.ReadKey OF
				'1' : Whole;
				'2' : IntroWalls;
				'3' : DemoAnimate;
				'4' : DemoPend;
				'5' : DrivenSystem;
				'6' : BEGIN DrivenSystem2; IF Not Escape THEN DrivenSystem2a END;
				'7' : BEGIN
						  IF Not Escape THEN DrivenSystem3;
						  IF Not Escape THEN PendulumChaos
						END;
				'8' : FFTDemo;
				'9', Chr(27) : QuitTutorial := True
			END; {Case Readkey}
			TutorialChosen := True;
		END; {IF ReadKey}
		IF Event.MouseClicked THEN WITH event DO WITH views[0] DO
		BEGIN
			Message('');
			IF y < vy1 + 178 THEN Whole
			ELSE IF y < vy1 + 178 + 20 THEN IntroWalls
			ELSE IF y < vy1 + 178 + 40 THEN DemoAnimate
			ELSE IF y < vy1 + 178 + 60 THEN DemoPend
			ELSE IF y < vy1 + 178 + 80 THEN DrivenSystem
			ELSE IF y < vy1 + 178 + 100 THEN
				BEGIN DrivenSystem2; IF Not Escape THEN DrivenSystem2a END
			ELSE IF y < vy1 + 178 + 120 THEN
				BEGIN
					IF Not Escape THEN DrivenSystem3;
					IF Not Escape THEN PendulumChaos
				END
			ELSE IF y < vy1 + 178 + 140 THEN FFTDemo
			ELSE IF y < vy1 + 340 THEN QuitTutorial := True;
			Debounce;
			TutorialChosen := True;
		END; {With Event}
		ShowMouse; {Escape may or may not leave it hidden}
		DoverM := 0.0; F0overM := 0.0; {Restore standard conditions}
		DoErase := True;               {In case of Escape}
		Animating := False;  DemoPoincare := False
		UNTIL TutorialChosen;
		SetColor(White)
	UNTIL QuitTutorial;
	HideMouse;
	DelayTime := SaveDelay;
	DoErase := SaveErase;
	InTutorial := False; Animating := True;
	ShowPeriods := SavePeriod; DoverM := 0.0; F0overM := 0.0;
	ShowMenu := True; Continuous := True; TimePlotInverted := False;
	HidePoincare := False; TimeLimit := False;
	HalfWallSep := 0.5; Energy := 0.02; gOverL := 9.8;
	GraphBackColor := DarkGray;
	ChooseWalls;
	Message('');
	IF CalledFirst THEN
		EndScreen
	ELSE
	BEGIN
		CloseAllPort;
		DrawSystem;
		FindPotential(True);
		DisplayPotential(False,False);
		ShowXandPhase;
		RunInstruct;
		DisplayChoice;
		ShowTrajectory;
		MildAnnounce('Click in Potential vs. x or Phase Plot to launch motion.');
		DisplayMainMenu;
		ShowMouse
	END
END; {Tutorial}

BEGIN
	MyVersion := 0.63;
	IF MyVersion > Version Then Version := MyVersion
END.