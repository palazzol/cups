           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.0 (94/08/08) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

(*  Program Title  : FIELDQ *)
(*  Author         : Ron Stoner *)
(*  Version        : 8 *)
(*  Program Summary: The program simulates the electromagnetic *)
(*  field in the plane of motion generated of a point charge *)
(*  that is moving and accelerating in 2-D.  User chooses *)
(*  from among seven trajectories, and chooses values of *)
(*  maximum speed and viewing time.  The electric field *)
(*  pattern is recomputed after each change of trajectory or *)
(*  parameter;  thereafter, the user can investigate the *)
(*  electric field, magnetic field, retarded potentials, *)
(*  and Poynting-vector field by use of the mouse as a field *)
(*  probe, by using gridded overlays, and by allowing the user *)
(*  to generate plots of the various fields along cuts through *)
(*  the viewing plane. *)

(*  Library Files  :  The program uses MUPPET graphics, *)
(*  trig functions from CUPSfunc, and the FindZero rootfinder *)
(*  from CUPSproc. *)
(*  Algorithms     :  fast algorithm is used to generate smooth *)
(*  electric field lines.  Exploration of other fields uses *)
(*  formulas derived from Lienard-Weichert potentials.   *)

program ACCELQ;

(*****************************************************
  *      CUPS Simulation by Ron Stoner               *
  *    Displays Field of a Point Charge              *
  *    with choice of Trajectory                     *
  *    with option of exploring the                  *
  *    vector and scalar potentials, and             *
  *    electric, magnetic, and Poynting fields.      *
  * Modified 9/18/91 to avoid converting very        *
  * large numbers for numeric output                 *
  * Revised for pull-down menus, HotKeys             *
  * and rubber-band plots Dec 1991                   *
  *   Includes Jarek's revisions                     *
  * & suggestions from R. Ehrlich Jan 1992           *
  * Trajectory animation added March 1992 at         *
  * suggestion of Roger Rollins.                     *
  * Most "BUGS" from directors 7/30/93 Flitted 8/93. *
  * Revised as per Wiley reviewer comments.          *
  * Last change: June 29, 1994                       *
  **************************************************** *)

	uses
                CRT, DOS, Graph, CUPSmupp, CUPS, CUPSgrph, CUPSgui, CUPSfunc, CUPSproc;

	const
		TwoPi = 6.2831853;
		Rt2 = 1.4142136;

	var
		shades: array[1..5] of INTEGER;
		path: array[1..7] of string[20];
		MyMenu: TMenu;   (* Pull-down Menu *)
		key: byte;   (* Hotkey variable *)
		HotKeys: THotKeys;
		incase: Integer;   (* Identifies active trajectory *)
		nlines: Integer;   (* No. of E-field lines  *)
		range: Real;   (* Half-width of square plot window  *)
		vmax, dvmax: Real;   (* Charge speed (c=1) and increment  *)
		viewt, dviewt: Real;   (* Observation time & increment  *)
		xpos, ypos: real;   (* Position of mouse or chosen field point  *)
		SegL: real;   (* (Line segment length)/range  *)
                Finished : BOOLEAN;
		AnimatePath: BOOLEAN;

 (* -----   Utility Functions            ____________*)

	procedure ShowCredits;
		var
			screen: HelpScrType;
	begin
		screen[01] := '                                                  ';
		screen[02] := '   ELECTROMAGNETIC FIELDS NEAR A MOVING CHARGE    ';
		screen[03] := '                                                  ';
		screen[04] := '                                                  ';
		screen[05] := '               A CUPS Simulation                   ';
		screen[06] := '                                                  ';
		screen[07] := '                     by                           ';
		screen[08] := '                                                  ';
		screen[09] := '                by Ron Stoner                     ';
		screen[10] := '       Department of Physics and Astronomy        ';
		screen[11] := '         Bowling Green State University           ';
		screen[12] := '            Bowling Green, OH 43403               ';
		screen[13] := '                                                  ';
                screen[14] := '  This program draws a "snapshot" of the electric ';
		screen[15] := '  field produced by a moving electric charge.     ';
		screen[16] := '  Charge motion is confined to any one of several ';
		screen[17] := '  predefined trajectories in the plane of the     ';
		screen[18] := '  screen. Fields are also computed in that plane. ';
		screen[19] := '  Besides the electric field (E), the user can    ';
		screen[20] := '  examine magnetic field (B), scalar potential    ';
		screen[21] := '  (V), vector potential (A) and Poynting-vector   ';
		screen[22] := '  (S) by using various menu choices.              ';
		screen[23] := '     Copyright (c) John Wiley & Sons Inc. 1995    ';
		screen[24] := '                                                  ';
		screen[25] := '   >>>>>   PRESS ANY KEY TO CONTINUE      <<<<<   ';
		help(screen);
	end;

	procedure MainHelpScreen;
		var
			screen: HelpScrType;
	begin
		screen[1] := '                                                  ';
		screen[2] := '* TRAJECTORY menu allows choice of these types:   ';
		screen[3] := '    1. Linear acceleration from v=0 during 0<t<1. ';
		screen[4] := '    2. Linear deceleration to v=0   during 0<t<1. ';
		screen[5] := '    3. Simple harmonic motion with amplitude = 1. ';
		screen[6] := '    4. Counterclockwise motion on a unit circle.  ';
		screen[7] := '    5. A smooth turn by 90 degrees.               ';
		screen[8] := '    6. Sinuous ("wiggling") motion.               ';
		screen[9] := '    7. A Lissajous "Figure 8".                    ';
		screen[10] := '  Parameters common to all trajectories are max.  ';
		screen[11] := '  speed (Vmax<1) and viewtime.  Arrow key presses ';
		screen[12] := '  initiate changes in these parameters, followed  ';
		screen[13] := '  by redrawing of the electric field pattern.     ';
		screen[14] := '                                                  ';
		screen[15] := '* EXPLORE menu allows mouse use as a field probe. ';
		screen[16] := '                                                  ';
		screen[17] := '* PLOT WHAT menu plots chosen quantities along    ';
		screen[18] := '  straight lines between user-selected endpoints, ';
		screen[19] := '  or versus time at user-selected positions.      ';
                screen[20] := '                                                  ';
		screen[21] := '* GRID PLOT plots vector fields on a grid.        ';
		screen[22] := '  Dots (crosses) indicate direction into (out of) ';
		screen[23] := '  the screen.  Arrows for fields are in correct   ';
		screen[24] := '  direction, but length may not be proportional   ';
		screen[25] := '  to magnitude if termination is on boundary.     ';
		help(screen);
	end;

	procedure DataHelpScreen (var screen: HelpScrType);
	begin
		screen[1] := '* Parameters set by this screen are needed        ';
		screen[2] := '  to generate a field-pattern snapshot.           ';
		screen[3] := '                                                  ';
		screen[4] := '* The meanings of the parameters:                 ';
		screen[5] := '  1. View time in the frame of a distant observer.';
		screen[6] := '  2. Max. speed of the charge in units c=1.       ';
		screen[7] := '  3. Graph range is half width of plotted area.   ';
		screen[8] := '  4. Number of field lines to be plotted.         ';
		screen[9] := '  5. Length of a line segment in screen units.    ';
		screen[10] := '  6. Change in view time effected by hotkeys.     ';
		screen[11] := '  7. Change in Max. Speed effected by hotkeys.    ';
		screen[12] := '                                                  ';
		screen[13] := '* Use Mouse, arrow keys or Enter key to move      ';
		screen[14] := '  from one numeric parameter field to another.    ';
		screen[15] := '                                                  ';
		screen[16] := '* Click mouse on Ok to accept the values shown.   ';
		screen[17] := '  or on Cancel to leave screen with the values    ';
		screen[18] := '  of the parameters unchanged.                    ';
		screen[19] := '                                                  ';
		screen[20] := '* You should use values for the parameters that   ';
		screen[21] := '  are within the reasonable ranges suggested on   ';
		screen[22] := '  the screen to avoid unpredictable failure of the';
		screen[23] := '  algorithm.  The algorithm may also produce odd  ';
		screen[24] := '  results when the maximum speed approaches unity.';
		screen[25] := '                                                  ';
	end;

	procedure AboutUnits;
		var
			screen: HelpScrType;
	begin
		screen[1] := '                                                  ';
		screen[2] := '* You can think of the electric charge in this    ';
		screen[3] := '  simulation as being either positive (a proton)  ';
		screen[4] := '  or negative (an electron).  The electric field  ';
		screen[5] := '  lines are directed away from a positive charge  ';
		screen[6] := '  and toward a negative one.  The magnitude of the';
		screen[7] := '  electric field is proportional to the size of   ';
		screen[8] := '  the charge, and to the flux (density of lines)  ';
		screen[9] := '  but the number of lines is arbitrary.           ';
		screen[10] := '                                                  ';
		screen[11] := '* Units of distance and time are chosen so that   ';
		screen[12] := '  the speed of light is unity (c=1).  It would not';
		screen[13] := '  be very useful to assign units to the various   ';
		screen[14] := '  fields since neither S.I. nor Gaussian units    ';
		screen[15] := '  are very appropriate for describing the fields  ';
		screen[16] := '  very near individual elementary particles.      ';
		screen[17] := '  More important are the relative values of the   ';
		screen[18] := '  fields and how they depend on distance and time.';
		screen[19] := '  Therefore, units in this simulation are usually ';
		screen[20] := '  listed as "arbitrary."                          ';
		screen[21] := '                                                  ';
		screen[22] := '                                                  ';
		screen[23] := '                                                  ';
		screen[24] := '                                                  ';
		screen[25] := '                                                  ';
		help(screen);
	end;


	procedure AboutProgram;
		var
			screen: HelpScrType;
	begin
		screen[1] := '                                                  ';
		screen[2] := '   ELECTROMAGNETIC FIELDS OF A MOVING CHARGE      ';
		screen[3] := '                     ----                         ';
		screen[4] := '  This CUPS simulation draws the electric field-  ';
		screen[5] := '  line pattern due to a moving electric charge.   ';
		screen[6] := '  Motion is in the plane of the computer screen   ';
		screen[7] := '  and in one of several predefined trajectories.  ';
		screen[8] := '  Fields are also those in this plane of motion.  ';
		screen[9] := '                                                  ';
		screen[10] := '  The field is what a distant viewer would measure';
		screen[11] := '  at the indicated viewing time, which can be     ';
		screen[12] := '  changed in increments by pressing arrow "hot-   ';
		screen[13] := '  keys."  Other hotkeys allow the user to change  ';
		screen[14] := '  the maximum speed of the charge in the currently';
		screen[15] := '  active trajectory.  Menu items allow the viewing';
		screen[16] := '  time, speed and other parameters, such as the   ';
		screen[17] := '  scale of the field plot, to be modified.        ';
		screen[18] := '                                                  ';
		screen[19] := '  Besides the electric field (E), the user can    ';
		screen[20] := '  measure magnetic field (B), scalar potential    ';
		screen[21] := '  (V), vector potential (A) and Poynting-vector   ';
		screen[22] := '  (S) by using various menu choices.              ';
		screen[23] := '                                                  ';
		screen[24] := '                                                  ';
		screen[25] := '                                                  ';
		help(screen);
	end;

        Function EscapeFromThis : BOOLEAN;
        begin
           CheckForEvents;
	   IF (ord(Event.ReadKey) = 27) THEN EscapeFromThis := TRUE
              ELSE EscapeFromThis := FALSE;
        end;

        Procedure WaitForMouseInside;
        var
            inside:  BOOLEAN;
            x, y : REAL;
        {Loops until mouse is inside Viewport1}
        begin
            inside := FALSE;
            REPEAT  MousePosn (x, y,1,1,inside);
            UNTIL inside;
        end;


	Function Mag (xcomp, ycomp: REAL): REAL; (*Magnitude of a 2-D vector *)
	begin
		Mag := SQRT(SQR(xcomp) + SQR(ycomp));
	end;

        Procedure notate(instruct: STRING);
        begin
           IF instruct = '' THEN
             begin
               GraphBackColor := BLAKK;
               selectviewport(9);
               clearviewport
             end
           ELSE
             begin
               GraphBackColor := Blue;
               openviewport(8);
               setcolor(Lightred);
               print(1,1,instruct);
             end;
        end;

 (*-----           Input/Output Screen Procedures               -----*)

	procedure DefaultData;
	begin
		path[1] := 'ACCELERATED';
		path[2] := 'DECELERATED';
		path[3] := 'HARMONIC';
		path[4] := 'CIRCULAR';
		path[5] := 'RIGHT-ANGLE TURN';
		path[6] := 'SINUOUS';
		path[7] := 'FIGURE-8';
		shades[1] := Yellow;
		shades[2] := LightRed;
		shades[3] := LightBlue;
		shades[4] := LightGreen;
		shades[5] := Cyan;
		incase := 4;
		vmax := 0.60;
		viewt := 6.0;
		range := 25;
		nlines := 12;
		SegL := 0.04;
		dviewt := 1.0;
		dvmax := 0.1;
		AnimatePath := TRUE;
	end;

	procedure LoadDataScrn;
		var
			DataHelp: HelpScrType;
			DataScreen: TInputScreen;
	begin
		with DataScreen do
			begin
				init;
				DefineInputPort(0.01, 0.99, 0.05, 0.95);
				LoadLine('                 FIELD OF A MOVING CHARGE');
				LoadLine('               CUPS Simulation by Ron Stoner');
				LoadLine('           ** SET PARAMETERS FROM THIS SCREEN **');
				LoadLine('');
				LoadLine('         Initial parameters for trajectories:');
				LoadLine('           View Time    : {    }    units light-cm');
				LoadLine('           Max. Speed   : {    }   0.01 to 0.999, (c = 1)');
				LoadLine('         Parameters for field plotting: ');
				LoadLine('           Graph Range  : {    }  1.0 to 1000. , units cm');
				LoadLine('           No. of Lines : {    }  typically 4 to 32');
				LoadLine('           Segment size : {    }  0.001 to 0.1 ; affects both ');
				LoadLine('               computing speed and field line smoothness.');
				LoadLine('');
				LoadLine('         Increments/Decrements in trajectory parameters');
				LoadLine('         effected by arrow keys.');
				LoadLine('          Viewtime change dt = {    }  Up & Down Arrow ');
				LoadLine('          Vmax change     dv = {    }  Left & Right; (c=1)');
				LoadLine('');
				LoadLine('          Show Animated Trajectory #T');
				LoadLine('');
				LoadLine('                 [  Ok  ][Cancel][ Help ]');
				SetNumber(1, viewt);
				SetNumberLimits(1, -100, 100);
				SetNumber(2, vmax);
				SetNumberLimits(2, 0.01, 0.99);
				SetNumber(3, range);
				SetNumberLimits(3, 1, 1000);
				SetNumber(4, nlines);
				SetNumberLimits(4, 3, 32);
				SetNumber(5, SegL);
				SetNumberLimits(5, 2 / GetMaxY, 0.20);
				SetNumber(6, dViewt);
				SetNumberLimits(6, 0.001, 1.0);
				SetNumber(7, dVmax);
				SetNumberLimits(7, 0.001, 0.5);
				SetBoolean(8, AnimatePath);
				DataHelpScreen(DataHelp);
				SetHelpScreen(DataHelp);
				Accept;
				HideCursor;
				viewt := GetNumber(1);
				vmax := ABS(GetNumber(2));      {Maximum charge speed/Light Speed}
				range := ABS(GetNumber(3));     (*Both X and Y plotted to same scale *)
				if (range < 1) then
					range := 1;     (*Make a nicer scale if necessary *)
				if (range > 5) then
					range := 5 * ROUND(range / 5)
				else
					range := ROUND(range * 2) / 2;
				if (range > 20) then
					range := 20 * ROUND(range / 20);
				if (range > 50) then
					range := 50 * ROUND(range / 50);
				if (range > 200) then
					range := 200 * ROUND(range / 200);
				if (range > 1000) then
					range := 1000;
				nlines := Round(GetNumber(4));(*Number of E-field lines to plot *)
				segL := ABS(GetNumber(5));       (*Optimum segment length size/range *)
				dviewt := GetNumber(6);
				dvmax := GetNumber(7);
				AnimatePath := GetBoolean(8);
			end;
		ShowCursor;
	end;


	procedure SetUpMenu;  (*Pulldown menu *)
	begin
		with MyMenu do
			begin
				init;
				column(1, 'FILE');
				row(1, 1, 'About CUPS');
				row(1, 2, 'About Program');     (*Credits *)
				row(1, 3, 'Title Screen');
				row(1, 4, 'Configuration');
				row(1, 5, 'Menu Help');
				row(1, 6, 'About Units');
				row(1, 7, 'EXIT program');
				column(2, 'TRAJECTORY');
				row(2, 1, 'Linear Acceleration');
				row(2, 2, 'Linear Deceleration');
				row(2, 3, 'Harmonic Ocillation');
				row(2, 4, 'Circular Motion');
				row(2, 5, 'Right-Angle Turn');
				row(2, 6, 'Sinuous Motion');
				row(2, 7, 'Figure 8');
				column(3, 'PLOT WHAT');
				row(3, 1, '|E| vs position');
				row(3, 2, ' B vs position');
				row(3, 3, '|S| vs position');
				row(3, 4, ' V  vs position');
				row(3, 5, '|A| vs position');
				row(3, 6, '|E| vs time');
				row(3, 7, ' B  vs time');
				row(3, 8, '|S| vs time');
				row(3, 9, ' V  vs time');
				row(3,10, '|A| vs time');
				column(4, 'EXPLORE');
				row(4, 1, 'E,B & Poynt.');
				row(4, 2, 'Retarded V & A');
				column(5, 'GRID PLOT');
				row(5, 1, 'Mag. Field ');
				row(5, 2, 'Poynting ');
				row(5, 3, 'Vector Pot.');
				column(6, 'DATA');
				row(6, 1, 'New Parameters');
				row(6, 2, 'Use Defaults')
			end;
	end;

	procedure SetUpHotKeys;
	begin
		with HotKeys do
			begin
				Init(6);
				key[1] := 'F1-Help';
				key[2] := 'Up-t+dt';
				key[3] := 'Down-t-dt';
				key[4] := 'Right-v+dv';
				key[5] := 'Left-v-dv';
				key[6] := 'F10-Menu';
			end;
	end;



	procedure FieldScreen; (*Set up format for displaying field values. *)
	begin
		GraphBackColor := DarkGray;
		OpenViewport(3);
		SetColor(whitte);
		Print(5, 1, 'X=');
		Print(5, 2, 'Y=');
		SetColor(shades[1]);
		Print(4, 3, '|E|=');
		Print(4, 4, 'Ex =');
		Print(4, 5, 'Ey =');
		SetColor(shades[2]);
		Print(4, 6, 'Bz=');
		SetColor(shades[3]);
		Print(3, 7, '|S|=');
		Print(4, 8, 'Sx=');
		Print(4, 9, 'Sy=');
	end; (*FieldScreen *)



	procedure PrintFields (xmouse, ymouse, Ex, Ey, Bz, Sx, Sy: real);
		const
			MaxNum = 1000;
		var
			E, S: Real;
	begin
		FieldScreen;
		SetColor(whitte);
		Print(8, 1, NumStr(xmouse, 5, 2));
		Print(8, 2, NumStr(ymouse, 5, 2));
		SetColor(shades[1]);
		E := sqrt(Sqr(Ex) + Sqr(Ey));
		Print(8, 3, ScNumStr(E, 3));
		Print(8, 4, ScNumStr(Ex, 3));
		Print(8, 5, ScNumStr(Ey, 3));
		SetColor(shades[2]);
		Print(8, 6, ScNumStr(Bz, 3));
		SetColor(shades[3]);
		S := sqrt(sqr(Sx) + sqr(Sy));
		Print(8, 7, ScNumStr(S, 3));
		Print(8, 8, ScNumStr(Sx, 3));
		Print(8, 9, ScNumStr(Sy, 3));
	end; (*PrintFields *)


	procedure PotentialScreen;
	begin
		GraphBackColor := DarkGray;
		OpenViewport(3);
		SetColor(whitte);
		Print(3, 1, 'POSITION:');
		Print(5, 2, 'X=');
		Print(5, 3, 'Y=');
		SetColor(shades[5]);
		Print(3, 4, 'POTENTIALS (arb. units):');
		SetColor(shades[5]);
		Print(4, 5, '|A|=');
		Print(4, 6, 'Ax =');
		Print(4, 7, 'Ay =');
		SetColor(shades[4]);
		Print(4, 8, ' V =');
	end; (*PotentialScreen *)



	procedure PrintPots (xmouse, ymouse, Ax, Ay, V: real);
		const
			MaxPot = 1000.0;
		var
			A: Real;
	begin
		SelectViewport(3);
		PotentialScreen;
		SetColor(whitte);
		Print(8, 2, NumStr(xmouse, 5, 2));
		Print(8, 3, NumStr(ymouse, 5, 2));
		SetColor(shades[5]);
		A := sqrt(Sqr(Ax) + Sqr(Ay));
		Print(8, 5, ScNumStr(A, 5));
		Print(8, 6, ScNumStr(Ax, 5));
		Print(8, 7, ScNumStr(Ay, 5));
		SetColor(shades[4]);
		Print(8, 8, ScNumStr(V, 5));
	end;



 (**   ----------   Graphics Procedures       ----------------* *)


	procedure InitializeGraphics;
        (* Setup viewports *)
	begin
		DefineScale(1, -Range, Range, -Range, Range);
		DefineScale(4, 0, 1, 0, 1);
		DefineViewport(1, 0.42, 0.98, 0.142, 0.892); (*Field pattern port *)
		DefineViewport(2, 0.05, 0.36, 0.15, 0.56);
		DefineViewport(3, 0.01, 0.37, 0.578, 0.95);
		DefineViewport(4, 0.0, 0.372, 0.05, 0.585);
		DefineViewPort(5, 0, 1, 0.05, 0.95);
                DefineViewport(6, 0.01, 0.38, 0.06, 0.11);
                DefineViewPort(7, 0.42, 0.98,0.06,0.11);
                DefineViewport(8, 0.42, 0.98, 0.90,0.95);
                DefineViewport(9, 0.41, 0.99, 0.895,0.952);
		SetLineStyle(0, 0, 1);
		SetColor(LightGreen);
		SelectScale(2);
		SelectScale(1);
		SetColor(Yellow);
	end;

	procedure Arrow(xo, yo, xcomp, ycomp, long: real);
 (* Draws arrows of specified length from (xo,y0)
 to terminus or to intersection with viewport boundary. *)
		var
			xorg, yorg, xend, yend, dx, dy, size, temp: real;
	begin
		size := Mag(xcomp, ycomp);
		if (size < Macheps) then
			size := Macheps;
		xend := xo + long * xcomp / size;
		yend := yo + long * ycomp / size;
		if abs(xend) > range then
			begin          (*Replace by intersection with x-boundary. *)
				temp := range * xend / abs(xend);
				yend := yo + (yend - yo) * (temp - xo) / (xend - xo);
				xend := temp;
			end;
		if abs(yend) > range then
			begin          (*Replace by intersection with y-boundary. *)
				temp := range * yend / abs(yend);
				xend := xo + (xend - xo) * (temp - yo) / (yend - yo);
				yend := temp
			end;
		PlotLine(xo, yo, xend, yend);
		dx := 0.4 * (xend - xo);
		dy := 0.4 * (yend - yo);
		xorg := xend;
		yorg := yend;
		xend := xend - dx + 0.5 * dy;
		yend := yend - dy - 0.5 * dx;
		PlotLine(xorg, yorg, xend, yend);
		xend := xend - dy;
		yend := yend + dx;
		PlotLine(xorg, yorg, xend, yend);
	end; (*Arrow *)



	procedure RVector (a, b: pointType);  far;
 (* Used with CUPS rubber-band utility *)
	begin
		vector(a.x, a.y, b.x, b.y);
	end;



	procedure GetGraphLine (var xstart, ystart, xend, yend: REAL);
 (* Obtains and plots cut for GRAPH option. *)
		var
			xtic, ytic, dx, dy: REAL;  (*Position and size of tic marks. *)
			ntic: integer;        (*Tic mark number. *)
			inside: BOOLEAN;
			x, y, button: integer;
			a, b: pointType;
			vp: ViewPortType;
	begin
	   Notate('  Click inside and drag or <ESC>.');
           WaitForMouseInside;
	   ShowCursor;
	   SetLineStyle(SolidLn, 0, normWidth);
	   SelectViewPort(1);
	   GetViewSettings(vp);
	   repeat         (*Choose start point *)
	      begin
		 MouseGlobalPosn(x, y, button);
		 MousePosn(xstart, ystart, 1, 1, inside);
                 Finished := EscapeFromThis;
	      end
	   until (Finished OR (button <> 0));
           IF NOT Finished THEN Finished := NOT inside;
           IF NOT Finished THEN BEGIN
		a.x := x - vp.x1;
		a.y := y - vp.y1;
		SetColor(LightGray);
		DrawRubberBand(Rvector, a, b, DarkGray, true);
		MousePosn(xend, yend, 1, 1, inside);
		SelectScale(1);
		SetColor(whitte);
		SelectScale(1);
		HideCursor;
		PlotSymbol(xstart, ystart, 'x');       (*mark it. *)
		PlotSymbol(xend, yend, 'x');
		SetColor(LightGray);
		PlotLine(xstart, ystart, xend, yend);
		dx := (yend - ystart) / 40;                       (*Draw graph line *)
		dy := -(xend - xstart) / 40;
		for ntic := 1 to 9 do                        (*and divide by tic marks. *)
			begin
				xtic := xstart + ntic * (xend - xstart) / 10;
				ytic := ystart + ntic * (yend - ystart) / 10;
				PlotLine(xtic + dx, ytic + dy, xtic - dx, ytic - dy);
			end;
           END;
           notate('');
	   ShowCursor;
	end;



 (**   -----------  Physics Procedures       -----------------* *)


	procedure Trajectory (var t, x, y, vx, vy, vmax, ax, ay: Real);
 (* Computes position, velocity and acceleration as functions of time
    for various trajectories. *)
		var
			phase, tend, omega: Real; (*intermediate variables *)
		const
			Rt2 = 1.4142136;
			Piover2 = 1.5707963;
	begin
		ax := 0.0;
		ay := 0.0;
		y := 0.0;
		vy := 0.0;
		case incase of
			1: 
				if (t < 0) then        (*Accelerate from rest *)
					begin               (*to vmax during 0<t<1 *)
						x := 0.0;
						vx := 0.0;
					end
				else if (t > 1) then
					begin
						x := vmax * (t - 0.5);
						vx := vmax;
					end
				else            (*0 <= t <= 1, so *)
					begin
						ax := vmax;
						x := ax * t * t / 2;  (*Note acceleration is vmax. *)
						vx := ax * t;
					end;             (*of case 1 *)

			2: 
				if (t < 0) then        (*Decelerate to rest     *)
					begin              (*from vmax during 0<t<1 *)
						x := t * vmax;
						vx := vmax;
					end
				else if (t > 1) then
					begin
						x := vmax / 2;
						vx := 0;
					end
				else                    (* 0 <= t <= 1 , so      *)
					begin                 (* uniform acceleration  *)
						x := vmax * t * (1 - t / 2);  (* to rest.              *)
						vx := vmax * (1 - t);
						ax := -vmax;
					end; (*of case 2 *)

			3: 
				begin
					phase := t * vmax;           (*Simple Harmonic Motion   *)
					x := cos(phase);          (*unit amplitude, along +x *)
					vx := -vmax * sin(phase);
					ax := -x * SQR(vmax)
				end;  (*of case 3 *)

			4: 
				begin
					phase := t * vmax;           (*circular motion  *)
					x := cos(phase);           (*counterclockwise *)
					y := sin(phase);           (* on unit circle  *)
					vx := -vmax * y;
					vy := vmax * x;
					ax := -x * SQR(vmax);
					ay := -y * SQR(vmax);
				end;  (*of case 4 *)

			5: 
				if (t < 0) then              (*Turn a corner *)
					begin
						x := -1;                    (*For t<0 charge has  *)
						vx := 0;                   (*constant velocity   *)
						y := vmax * t;              (*in +y direction,    *)
						vy := vmax;               (*along the line      *)
					end                       (*   x = -1  ,        *)
				else
					begin
						tend := Piover2 / vmax;      (*and after the turn *)
						if (t > tend) then           (*it moves in +x-    *)
							begin                    (*direction along    *)
								vx := vmax;              (*  y=1.             *)
								vy := 0;
								y := 1;
								x := vmax * (t - tend);
							end
						else
							begin
								phase := t * vmax;       (*During the turn, the    *)
								x := -cos(phase);     (*charge moves clockwise  *)
								y := sin(phase);      (*on the unit quarter     *)
								vx := vmax * y;          (*circle.                 *)
								vy := -vmax * x;
								ax := -x * SQR(vmax);
								ay := -y * SQR(vmax);
							end;
					end; (*case5 *)

			6: 
				begin
					vy := vmax / Rt2;
					y := vy * t;     (*Motion is simple harmonic in x-direction *)
					x := cos(y);      (*and drift in y-direction.                *)
					vx := -vy * sin(y);
					ax := -x * SQR(vy);
				end;  (*case6 *)

			7: 
				begin                   (*Figure-8 Lissajous *)
					omega := vmax / Rt2;     (*Vertical oscillation frequency. *)
					phase := omega * t;
					x := 2 * cos(phase / 2); (*Horizontal freq. is 1/2 vertical freq. *)
					y := sin(phase);
					vx := -omega * sin(phase / 2);
					vy := omega * cos(phase);
					ax := -sqr(omega) * x / 2;
					ay := -sqr(omega) * y;
				end; (*case 7 *)
		end; (*case *)
	end;  (*Trajectory *)



	procedure ShowPath (time2, vmax, range: Real; incase: Integer; Show: BOOLEAN);
        (*Marks prior trajectory with velocity arrows, and animates charge motion. *)
		var
			time1, dt: REAL;  (*Beginning of shown path *)
			x1, y1, vx, vy, v, t, ax, ay, long: REAL;
			xp, yp: DataVector;
			ip, ntimes: INTEGER;
			TotalTimes: INTEGER;
                        H, M, Sec1, Tic1, Sec2, Tic2: WORD;

	begin
		case incase of
			1:     begin
			         time1 := 0;
				 if (time2 < time1) then
				    time1 := time2 - range / vmax;
				end;
			2:      time1 := -range / vmax;
			3:      time1 := time2 - Twopi / vmax;
			4:      time1 := time2 - Twopi / vmax;
			5:      time1 := -range / vmax;
			6:      time1 := -1.5 * range / vmax;
			7:      time1 := time2 - 2 * Rt2 * Twopi / vmax;
		end; (*Case *)
		dt := (time2 - time1) / 12;
		t := time1;
                SetColor(LightGreen);
		while t <= time2 do
			begin
				Trajectory(t, x1, y1, vx, vy, vmax, ax, ay);
				v := Mag(vx, vy);
                                long := dt * v / 2; (*Draw velocity arrow at charge position *)
				if (v > 0.01) then Arrow(x1, y1, vx, vy, long);
				t := t + dt;
			end;
		if Show then
		   Totaltimes := 2
		else
		   TotalTimes := 0;
		dt := (time2 - time1) / 24;    (*Animate the path in Viewport 2 *)
		SelectViewport(2);
		SelectScale(2);
		for ip := 1 to 52 do (*Compute points on trajectory *)
			begin
				t := time1 + (ip - 1) * (time2 - time1) / 50;
				Trajectory(t, xp[ip], yp[ip], vx, vy, vmax, ax, ay);
				xp[ip + 1] := xp[ip]; (*For later erasure of path end. *)
				yp[ip + 1] := yp[ip];
			end;
	        IF Totaltimes > 0 THEN for ntimes := 1 to TotalTimes do
			for ip := 2 to 51 do
				begin
					SetColor(LightGreen);
					PlotSymbol(xp[ip], yp[ip], '.');   (*Draw a new one. *)
                                        GetTime(H,M,Sec1,Tic1);
                                        REPEAT GetTime(H,M,Sec2,Tic2) UNTIL (Tic2 > Tic1+1) OR (Sec2<>Sec1);
					SetColor(Green);
					PlotSymbol(xp[ip], yp[ip], '.'); (*Erase an old one. *)
				end;
                PlotSymbol(xp[52],yp[52],'.');
		SetColor(LightGreen);
		for ip := 1 to 51 do PlotSymbol(xp[ip], yp[ip], '.');
		SetColor(Yellow); (*Mark end of path = current position *)
		PlotSymbol(xp[52], yp[52], 'x');
		SelectViewPort(1);
		SelectScale(1);
		SetColor(Yellow);
	end; (*ShowPath *)



	function Dminusct (tau: real): real;  FAR;  (*  Remove in MacIntosh Code *)
 (* The root of this function is the light-travel time, tau, from a
 screen-selected field point (xmouse, ymouse) to a point on the
 particle trajectory a distance c*tau away (remember c=1!).     *)
		var
			xq, yq, vqx, vqy, aqx, aqy, time, dist: real;
	begin
		time := viewt - tau;
		Trajectory(time, xq, yq, vqx, vqy, vmax, aqx, aqy);
		dist := sqrt(sqr(xpos - xq) + sqr(ypos - yq));  (*Function of tau *)
		Dminusct := dist - tau;
	end;



	procedure Fields (t, xp, yp: Real; var Ex, Ey, Bz, Sx, Sy: Real);
    (* Computes values of E, B and Poynting Vector
    using Lienard-Wiechert formulas *)
		var
			beta, tau: REAL;   (*Speed and retardation time. *)
			xq, yq, vqx, vqy, aqx, aqy: REAL;   (*Charge position, velocity, acc. *)
			tl, tu: REAL;   (*Bounds on tau. *)
			tsend: REAL;   (*When the wavefront was created. *)
			K, R, Rx, Ry, Ki3, gami2: REAL;   (*Intermediate variables. *)
			iter: INTEGER;(*Iterations to solve dist=c*time. *)
	begin
		Trajectory(t, xq, yq, vqx, vqy, vmax, aqx, aqy);  (*Get charge position *)
		tl := sqrt(sqr(xp - xq) + sqr(yp - yq)) / (1 + vmax); (*Lower bound on retardation. *)
		tu := sqrt(sqr(xp - xq) + sqr(yp - yq)) / (1 - vmax); (*Upper bound on retardation. *)
		FindZero(Dminusct, tl, tu, 1.0E-6, tau, iter); (*Solve to find retarded tau *)
		tsend := (t - tau); (*Time when wavefront intersecting (xq,yq) was "created". *)
		Trajectory(tsend, xq, yq, vqx, vqy, vmax, aqx, aqy); (*Find retarded posn. *)
		beta := sqrt(sqr(vqx) + sqr(vqy));     (*velocity and acceleration,      *)
		Rx := xp - xq;                           (*then compute fields. *)
		Ry := yp - yq;
		R := Mag(Rx, Ry);
		K := R - vqx * Rx - vqy * Ry; (*Formulas follow, for example, page 203 *)
		Ki3 := 1 / (K * sqr(K));      (*CLASSICAL ELECTROMAGNETIC RADIATION by *)
		gami2 := (1 - sqr(beta));     (*J. B. Marion, Academic Press 1965      *)
		Ex := (Rx - R * vqx) * (gami2 + Rx * aqx + Ry * aqy) * Ki3 - R * aqx * K * Ki3;
		Ey := (Ry - R * vqy) * (gami2 + Ry * aqy + Rx * aqx) * Ki3 - R * aqy * K * Ki3;
		Bz := (Rx * Ey - Ry * Ex) / R;
		Sx := Ey * Bz;
		Sy := -Ex * Bz;
	end; (*Fields *)



	procedure Potentials (t, xp, yp: real; var Ax, Ay, V: real);
        (* Computes Vector and Scalar Potentials
        using Lienard-Wiechert formulas*)
		var
			Rx, Ry, R, K, tau, MinK: REAL;   (*Intermediate variables. *)
			xq, yq, vqx, vqy, aqx, aqy: REAL;   (*Charge position, velocity, acc. *)
			tl, tu: REAL;   (*Bounds on tau. *)
			tsend: REAL;   (*When wavefront was created. *)
			iter: INTEGER;(*Iterations to find retardation. *)
	begin
		MinK := 1.0E-5 * vmax / range;
		Trajectory(t, xq, yq, vqx, vqy, vmax, aqx, aqy);  (*Find current position *)
		tl := sqrt(sqr(xp - xq) + sqr(yp - yq)) / (1.0 + vmax); (*Lower bound on tau *)
		tu := sqrt(sqr(xp - xq) + sqr(yp - yq)) / (1.0 - vmax); (*Upper bound on tau *)
		FindZero(Dminusct, tl, tu, 1.0E-6, tau, iter); (*Solve for retardation, tau *)
		tsend := (t - tau);       (*The time when fields were "created". *)
		Trajectory(tsend, xq, yq, vqx, vqy, vmax, aqx, aqy); (*Find retarded posn. *)
		Rx := xp - xq;                           (*then compute potentials. *)
		Ry := yp - yq;          (*Lienard-Wiechert formulas from, for example, p201 *)
		R := Mag(Rx, Ry);      (*of Marion's CLASSICAL ELECTROMAGNETIC RADIATION *)
		K := R - vqx * Rx - vqy * Ry;
		V := 1 / K;
		if ABS(K) > MinK then    (*Avoid division by zero. *)
			begin
				Ax := vqx / K;  (*Components of Vector Potential *)
				Ay := vqy / K;
			end
		else
			begin
				Ax := vqx / MinK;
				Ay := vqy / MinK;
			end;
	end; (*Potentials *)



	procedure OverPlot (index: Integer);
 (* Shows a grid of field vectors over E-field display
    Plots '.' and '+' for magnetic field directions. *)
		var
			Ex, Ey, Bz, Sx, Sy, Ax, Ay, V, long: Real;
			Ascale, Sscale: REAL;
			dx, MinBValue: Real;
			ix, iy: Integer;
	begin
		case index of
			1: 
				SetColor(shades[2]); (*For electric field,  *)
			2: 
				SetColor(shades[3]); (*magnetic field,      *)
			3: 
				SetColor(shades[5]); (*Poynting vector.     *)
		end;
		dx := range / 6;
		MinBValue := 1.0E-4 * vmax / sqr(range);
		Ascale := 0.1 * range / (2 * vmax / range);
		Sscale := sqr(vmax / sqr(range / 2) + 2 * sqr(vmax) / range);
		Sscale := 0.1 * range / Sscale;
		for ix := -6 to 5 do
			for iy := -6 to 5 do
				begin
					xpos := (ix + 0.5) * dx;
					ypos := (iy + 0.5) * dx;
					if index = 3 then
						Potentials(viewt, xpos, ypos, Ax, Ay, V)
					else
						Fields(viewt, xpos, ypos, Ex, Ey, Bz, Sx, Sy);
					SelectViewPort(1);
					SelectScale(1);
					case index of
						1: 
							if (ABS(Bz) > MinBValue) then
								if (Bz < 0) then
									PlotSymbol(xpos, ypos, '+')
								else
									PlotSymbol(xpos, ypos, '.');
						2: 
							begin
								long := Mag(Sx, Sy) * Sscale;
								Arrow(xpos, ypos, Sx, Sy, long);
							end;
						3: 
							begin
								long := Mag(Ax, Ay) * Ascale;
								Arrow(xpos, ypos, Ax, Ay, long);
							end;
					end;
				end;
	end;

   Procedure MouseReporter;
   VAR
     xpos, ypos   :  REAL;
     inside       :  BOOLEAN;
     ix, iy       : INTEGER;
   Begin
      SelectViewPort(3);
      SetColor(LightGray);
      REPEAT
        MousePosn(xpos, ypos, 1, 1, inside);
        IF inside THEN begin
           RubOut(16, 1, 10, DarkGray);
           RubOut(17, 2, 8, DarkGray);
           RubOut(17, 3, 8, DarkGray);
           Print(16, 1, 'Mouse at');
           Print(17, 2, NumStr(xpos, 5, 2));
           Print(17, 3, NumStr(ypos, 5, 2));
           Delay(100);
           end
        ELSE begin
           RubOut(16, 1, 10, DarkGray);
           RubOut(17, 2, 8, DarkGray);
           RubOut(17, 3, 8, DarkGray);
           end;
      UNTIL MouseClicked(ix, iy);
   End;


	procedure ExploreFields;
 (* Use mouse to find and evaluate fields. *)
		var
			Ex, Ey, Bz, Sx, Sy: Real;     (*Field components *)
			xmouse, ymouse: Real;     (*Mouse positions *)
			inside: Boolean;
			Sscale, long: Real;   (*Used to determine length of vectors *)
			EScale: REAL;   (*Typical field magnitude *)
			MinBValue: REAL;   (*Doesn't show B if less than this *)
	begin
		Notate('  Click inside for fields, outside to quit.');
		Sscale := sqr(vmax / sqr(range / 2) + 2 * sqr(vmax) / range);
		Sscale := 0.1 * range / Sscale;
		Escale := 1 / (sqrt(1 - sqr(vmax)) * sqr(range / 2));
		Escale := 0.1 * range / Escale;
		MinBValue := 1.0E-4 * vmax / sqr(range);
		FieldScreen;
		SelectViewPort(1);
		SelectScale(1);
                WaitforMouseInside;
		repeat
                        MouseReporter;
			{WaitOnMouseClick;}
			MousePosn(xmouse, ymouse, 1, 1, inside);
			if inside then
				begin
					xpos := xmouse;
					ypos := ymouse;
					Fields(viewt, xpos, ypos, Ex, Ey, Bz, Sx, Sy);
					PrintFields(xpos, ypos, Ex, Ey, Bz, Sx, Sy);
					SelectViewPort(1);
					SelectScale(1);
					long := Mag(Sx, Sy) * Sscale;
					SetColor(shades[3]);
					Arrow(xpos, ypos, Sx, Sy, long);
					SetColor(shades[1]);
					long := Mag(Ex, Ey) * Escale;
					Arrow(xpos, ypos, Ex, Ey, long);
					SetColor(shades[2]);
					if (ABS(Bz) > MinBValue) then
						if (Bz < 0) then
							PlotSymbol(xpos, ypos, '+')
						else
							PlotSymbol(xpos, ypos, '.');
				end;
		until not inside OR EscapeFromThis;
		Notate('');
	end;


	procedure ExplorePotentials;
		var
			Ax, Ay, V: Real;
			size: Real;
			xmouse, ymouse: Real;
			inside: Boolean;
	begin
		Notate('  Click on point, outside to quit.');
		PotentialScreen;
		SelectViewPort(1);
		SelectScale(1);
                WaitForMouseInside;
		repeat
                   MouseReporter;
		   MousePosn(xmouse, ymouse, 1, 1, inside);
		   IF inside THEN
		      begin
		         xpos := xmouse;
			 ypos := ymouse;
			 Potentials(viewt, xpos, ypos, Ax, Ay, V);
			 PrintPots(xpos, ypos, Ax, Ay, V);
			 SelectViewport(1);
			 size := 0.1 * range * Mag(Ax, Ay) / (2 * vmax / range);
			 SetColor(shades[5]);
			 Arrow(xpos, ypos, Ax, Ay, size);
		      end;
		until not inside OR EscapeFromThis;
		notate('');
	end; (*ExplorePotentials *)

        Procedure GetPoint(pointcolor: Word; VAR xmouse, ymouse: REAL);
           VAR
                inside:  BOOLEAN;

           BEGIN
		notate('  Click inside for graph or outside to quit.');
                WaitforMouseInside;
		SelectViewPort(1);
		SelectScale(1);
                WaitOnMouseClick;
	        MousePosn(xmouse, ymouse, 1, 1, inside);
		IF not inside OR EscapeFromThis THEN finished := TRUE;
                Setcolor(pointcolor);
                IF NOT finished THEN Plotsymbol(xmouse,ymouse,'X');
                notate('');
           END;


	procedure GraphField (N: integer);
		var
			x, y: Datavector;
			xstart, ystart, xend, yend: REAL;
			Ex, Ey, Bz, Sx, Sy: REAL;
			Ax, Ay, V, xp, yp: REAL;
                        Mcase : Integer;
			point: INTEGER;
                        tsave : REAL;
                        graphlabel : STRING;
	begin
           tsave := viewt;
	   IF N < 6 THEN
              begin
                 Mcase := N;
                 GetGraphLine(xstart, ystart, xend, yend)
              end
           ELSE
              begin
                 Mcase := n-5;
                 GetPoint(shades[Mcase],xend,yend);
              end;
           IF NOT Finished THEN
              BEGIN
                GraphBackColor := DarkGray;
		OpenViewport(3);
		SetColor(whitte);
                IF N < 6 THEN
                   BEGIN
		      print(2, 1, 'FIELD vs POSITION');
		      print(2, 3, 'From startpoint at:');
		      print(2, 6, 'to endpoint at:')
                   END
                ELSE
                   BEGIN
		      print(2, 1, 'FIELD vs TIME');
                      print(2, 3, 'Between times ');
                      print(2, 6, 'at point : ')
                   END;

		print(2, 9, 'Units are arbitrary.');
		GraphBackColor := blakk;      (*Erase previous axis and tics *)
                SelectViewPort(4);
                ClearViewPort;
                GraphBackColor := DarkGray;
                OpenViewPort(6);
		SetColor(WHITTE);
		case Mcase of
			1: graphlabel := '      |E|';
			2: graphlabel := '        B';
			3: graphlabel := '      |S|';
			4: graphlabel := '        V';
			5: graphlabel := '      |A|';
		end; (*CASE *)
                IF N < 6 THEN print(2, 1, concat(graphlabel,' vs POSITION'))
                   ELSE print(2, 1, concat(graphlabel,' vs TIME    '));
                SelectViewPort(3);
                Setcolor(Lightgreen);
                IF N<6 THEN
                   BEGIN
		      Print(3, 4, 'X =');
		      Print(3, 5, 'Y =');
		      Print(7, 4, NumStr(xstart, 5, 2));
		      Print(7, 5, NumStr(ystart, 5, 2));
                   END
                ELSE
                   BEGIN
                      SetColor(LightRed);
		      Print(3, 4, 'T1=');
		      Print(3, 5, 'T2=');
		      Print(7, 4, NumStr(tsave-2*pi*dviewt/vmax, 5, 2));
		      Print(7, 5, NumStr(tsave+2*pi*dviewt/vmax, 5, 2));
                      SetColor(LightGreen);
                   END;

		Print(3, 7, 'X =');
		Print(3, 8, 'Y =');
		Print(7, 7, NumStr(xend, 5, 2));
		Print(7, 8, NumStr(yend, 5, 2));
		for point := 1 to 61 do       (*Construct vector of data for plotting *)
			begin
				x[point] := (point - 1) / 60;
                                IF N < 6 THEN
                                   begin
                                      xp := xstart + (xend - xstart) * x[point];
				      yp := ystart + (yend - ystart) * x[point];
                                   end
                                else
                                   begin
                                      viewt := tsave+2*pi*dviewt*(point-31)/(30*vmax);
                                      xp := xend;
                                      yp := yend
                                   end;

				xpos := xp;
				ypos := yp;
				case Mcase of
					1:
						begin
							Fields(viewt, xp, yp, Ex, Ey, Bz, Sx, Sy);
							y[point] := Mag(Ex, Ey);
						end;
					2:
						begin
							Fields(viewt, xp, yp, Ex, Ey, Bz, Sx, Sy);
							y[point] := Bz;
						end;
					3:
						begin
							Fields(viewt, xp, yp, Ex, Ey, Bz, Sx, Sy);
							y[point] := Mag(Sx, Sy);
						end;
					4: 
						begin
							Potentials(viewt, xp, yp, Ax, Ay, V);
							y[point] := V;
						end;
					5: 
						begin
							Potentials(viewt, xp, yp, Ax, Ay, V);
							y[point] := Mag(Ax, Ay);
						end;
				end; (*CASE *)
			end;(*loop *)
		SelectViewPort(2);
		SelectScale(4);
		SetColor(LightGreen);   (*Scale and plot *)
		x[62] := 0;
		y[62] := 0;  (*Make sure origin is plotted *)
		Autoscale(2, x, y, 62);
		SetColor(shades[Mcase]);
		PlotData(x, y, 61);
           END;
           viewt := tsave;
	   MyMenu.Display;
	end; (*GraphField Procedure *)

 (* ****** Procedures for drawing the electric field pattern ********* *)

	procedure FieldPoint (var XField, YField, Vo, CsThn, SnThn, t, tau, dist: real);
(* Finds location at t=Time of a point on the field line
   that leaves charge at rest-frame angle, of which
   CsThn & SnThn are cosine and sine.  Tau is a delay time
   (or light-travel time) from the field point to a point
   on the prior trajectory of the charge.                 *)
		var
			CosTh, SinTh, CTh, STh: Real;   (* Cosines and sines of angles  *)
			CosPhi, SinPhi: Real;           (* used in the calculation.     *)
			SendTime: Real;                 (* Time when field signal was "sent." *)
			Denom, Distance: Real;          (* Intermediate quantities. *)
			Gamma, Beta: Real;              (* Relativity constants.  *)
			x, y, vx, vy, ax, ay: Real;     (* Trajectory variables.  *)
	begin
		SendTime := t - tau;
		Trajectory(SendTime, x, y, vx, vy, vmax, ax, ay);
		Beta := Mag(vx, vy);
		Gamma := 1 / Sqrt(1 - Sqr(Beta));
		XField := x + vx * Tau;   (*Extrapolate to where charge would be if   *)
		YField := y + vy * Tau;   (*there were no intermediate acceleration.  *)
		if (Beta > 0.001) then    (*Perform transform if charge has  *)
			begin                     (*significant motion.             *)
				CosTh := (CsThn * vx + SnThn * vy) / Beta; (*Rotate to make x the  *)
				SinTh := (SnThn * vx - CsThn * vy) / Beta; (*direction of motion *)
				Denom := Sqrt(1 - Sqr(Beta * CosTh));
				STh := SinTh / Denom;              (* Correct for transform of field  *)
				CTh := CosTh / (Gamma * Denom);    (* due to motion of the charge.    *)

				CosPhi := (CTh * vx - STh * vy) / Beta;    (*Rotate back to  *)
				SinPhi := (STh * vx + CTh * vy) / Beta;    (*observer frame. *)
			end
		else
			begin
				CosPhi := CsThn;
				SinPhi := SnThn;
				STh := SinTh;
				CTh := CosTh;
			end;
		dist := tau * (sqrt(1 - Sqr(Beta * STh)) - Beta * CTh); (*From field point  *)
		XField := XField + dist * CosPhi;  (*to extrapolated charge position *)
		YField := YField + dist * SinPhi;  (*is used to find field point.    *)
	end;


	procedure Makeline (var ThetaN, vmax, t, Res: Real);
 (* Constructs vector of points on a field line that makes angle
  ThetaN with observer's x-axis (when charge is at rest). Checks
  to keep distance between points approximately equal to Res,
  and attempts to compute all points on this line that remain
  within the viewing window. *)

		var
			np: Integer;  (*Index for plot vectors *)
			xp, yp: DataVector; (*plot vectors *)
			Tau, Dtau, TauMax: Real;  (*Tau = travel time    *)
			x, y, vx, vy: Real;  (*Trajectory variables *)
			XField, YField: Real;  (*Field point coordinates *)
			XStart, YStart: Real;
			CsThN, SnThN: Real;  (*Cosine and Sine of ThetaN *)
			MinD, MaxD, stepsize: Real;  (*Used to maintain resolution *)
			dist: Real;  (*From extrapolated charge position *)
			ax, ay: Real;  (*to field point. *)

		const
			Rt2 = 1.4142135;

	begin
		SnThN := Sin(ThetaN);   (* One can't avoid computing  *)
		CsThN := Cos(ThetaN);   (* sines and cosines here.    *)
		Tau := 0;
		Dtau := Res/50;            (* Initial time-grid size. *)
		MinD := Res / Rt2;
		MaxD := Res * Rt2;
		Trajectory(t, XStart, YStart, vx, vy, vmax, ax, ay);
		np := 1;
		xp[np] := XStart;
		yp[np] := YStart;
		dist := 0;
		while (dist < 3 * range) do
			begin
				repeat
					Tau := Tau + dtau;
					FieldPoint(XField, YField, vmax, CsThN, SnThN, t, Tau, dist);
					stepsize := Abs(YField - YStart) + Abs(XField - XStart);  (*Avoids Sqrt *)
					if (stepsize > MaxD) then     (* Reduce time step if  *)
						begin
							Tau := Tau - dtau;
							Dtau := dtau / Rt2;         (* step too large,   *)
						end;
				until (stepsize <= MaxD);
				if (stepsize < MinD) then      (* or increase time step  *)
					Dtau := Dtau * Rt2;         (* if step is short enough. *)
				XStart := XField;    (* Prepare for the next line segment  *)
				YStart := YField;    (* by setting its starting point.  *)
				np := np + 1;
				xp[np] := XStart;
				yp[np] := YStart;
				if (np = 64) then
					begin
						PlotData(xp, yp, np);
						np := 1;
						xp[np] := XStart;
						yp[np] := YStart;
					end;
			end;
		if (np > 1) then
			PlotData(xp, yp, np);
	end;


 (* -------------      PROGRAM CONTROL PROCEDURES      ----------------------- *)


	procedure DrawELines (viewt, vmax: real; incase, nlines: integer; Show: BOOLEAN);
		var
			line: INTEGER;
			segment, thetan, time1: REAL;
	begin
		CloseViewPort(5);
		HideCursor;
                GraphBackColor := DarkGray;
                OpenViewPort(6);
                SetColor(Whitte);
		Print(2, 1, '    CHARGE TRAJECTORY');
                GraphBackColor := DarkGray;
		OpenViewPort(3);
		print(2, 1, 'THE ELECTROMAGNETIC FIELD');
		print(2, 2, 'OF A POINT CHARGE IN');
		print(2, 3, Concat(path[incase], ' MOTION.'));
		SetColor(LightGreen);
		print(2, 5, 'Trajectory Parameters: ');
		print(2, 6, 'Vmax = ');
		print(2, 7, 'Time = ');
		Print(9, 6, NumStr(vmax, 3, 2));
		Print(9, 7, NumStr(viewt, 3, 2));
		SetColor(LightGreen);
                GraphBackColor := Blakk;
		OpenViewPort(2);
		DefineScale(2, -4.0, 4.0, -4.0, 4.0);
		SelectScale(2);
		Axis(-4.0, -4.0, 1, 1);
		SelectViewPort(1);
		DefineScale(1, -Range, Range, -Range, Range);
		SelectScale(1);
                GraphBackColor := BLAKK;
		OpenViewPort(1);
		Axis(-Range, -Range, Range / 5, Range / 5);
                GraphBackColor := DarkGray;
                OpenViewPort(7);
                SetColor(WHITTE);
                print(1,1,Concat('   E-FIELD PATTERN - ',path[incase], ' MOTION.'));
                SelectViewPort(1);
		ShowPath(viewt, vmax, range, incase, AnimatePath);
		segment := segl * range;           (*Optimum segment length. *)
		for line := 0 to nlines do
			begin
				thetan := TwoPi * (line + 0.5) / nlines;  (*Avoids horizontal E line. *)
				MakeLine(thetan, vmax, viewt, segment);
			end;
		HotKeys.Display;
		MyMenu.Display;
		ShowCursor
	end;


	procedure HandleMenu;
	begin
		with MyMenu do
			case ColChosen of
				1: 
					case rowChosen of
						1:    AboutCups;
						2:    AboutProgram;
						3:    ShowCredits;
						4:    Configuration;
						5:    MainHelpScreen;
						6:    AboutUnits;
						7:    begin
								HideCursor;
								ClearMuppetPort;
								CupsDone;
                                                                CloseGraph;
								Halt
							end;
					end;

				2:      begin
						incase := rowChosen;
						DrawELines(viewt, vmax, incase, nlines, TRUE);
					end;

				3:      begin
                                           Finished := FALSE;
                                           REPEAT GraphField(rowChosen)
                                           UNTIL Finished;
                                        end;

				4:      begin
						case rowChosen of
							1:    ExploreFields;
							2:    ExplorePotentials;
						end;
						SelectViewPort(1);
						SelectScale(1);
						ShowCursor;
						MyMenu.Display;
					end;

				5:      OverPlot(rowChosen);

				6:      begin
						if rowChosen = 2 then
							DefaultData
						else
							begin
								LoadDataScrn;
							end;
						HotKeys.Display;
						MyMenu.Display;
						DrawELines(viewt, vmax, incase, nlines, AnimatePath);
					end;
			end;  (*CASE *)

	end; (*HandleMenu Procedure *)


	procedure HandleHotKey (key: byte);
	begin
		case key of
			1:
				MainHelpScreen;
			2: 
				viewt := viewt + dviewt;
			3: 
				viewt := viewt - dviewt;
			4: 
				vmax := vmax + dvmax;
			5:
				vmax := vmax - dvmax;
			6: 
				if MyMenu.Chosen then
					HandleMenu;
		end; (*CASE *)
		if (Abs(vmax) > 0.99) then
			vmax := 0.99*vmax/ABS(vmax);
		if (ABS(vmax) < 0.010) then
			vmax := 0.010;
		if (key in [2, 3, 4, 5]) then
			DrawELines(viewt, vmax, incase, nlines, AnimatePath);
	end; (* HandleHotKey Procedure *)


  (*-----                 Main PROGRAM                      -----*)

begin
	CUPSinit;
	DefaultData;
	incase := 4;
	SetUpMenu;
	SetUpHotKeys;
	InitializeGraphics;
	DrawELines(viewt, vmax, incase, nlines, TRUE);
	ShowCredits;
	repeat
		CheckForEvents;
		if MyMenu.Activated then
			HandleMenu;
		if HotKeys.Pressed(key) then
			HandleHotKey(key);
	until false;
end.