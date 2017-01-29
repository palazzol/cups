            (*************************************************)
            (*************************************************)
            (**                                             **)
            (** Consortium of Upper-level Physics Software  **)
            (**             ( CUPS Project )                **)
            (**     (c) 1994 by John Wiley & Sons           **)
            (** Compiled with Utilities Ver. 1.8 (95/08/08) **)
            (**                                             **)
            (*************************************************)
            (*************************************************)

 (* CUPS SIMULATION - ELECTROMAGNETIC WAVES *)
 (*   Program Title:     EMWAVES            *)
 (*   Author:            Ron Stoner         *)
 (*   Version:           #11 (January 1995) *)
 (*   Program Summary:   This is a three-dimensional animation of a plane-}
{
      polarized, monochromatic electromagnetic wave.  Various options}
{
      include reflection, transmission and absorption at a material}
{
      interface, and transmission through polarizers and 1/4-wave plates.  *)


PROGRAM EMWAVES;

 (*         *************************************** *)
 (*         *   To animate the E and B Fields       *)
 (*         *   in a plane, monochromatic E-M       *)
 (*         *   wave for various polarizations.     *)
 (*         *   With provision for reflection and   *)
 (*         *   transmission at a boundary normal   *)
 (*         *   to the direction of propagation.    *)
 (*         *   Includes: 6 simulation choices      *)
 (*         *   Last change:  January 31, 1995      *)
 (*         *************************************** *)

	USES
		Graph, CUPSmupp, CUPS, CUPSgrph, CUPSgui, CUPSfunc, CUPSproc;


	TYPE
		CMatrixType = ARRAY[1..2, 1..2] OF COMPLEX;
		PhasorType = ARRAY[1..2] OF COMPLEX;

	VAR
		CaseName: ARRAY[1..6] OF STRING;
		Alt, Az, ViewR: REAL;      { Location of viewer}
		CAlt, CAz, SAlt, SAz: REAL;      { Trig functions of Alt and Az}
		P: REAL;      { Distance of screen from viewer}
		dt: REAL;      { Time between frames}
		Nwaves: REAL;      { No. of waves in wavetrain}
		kvac: REAL;      { Wave vector in vacuo }
		Nframes: INTEGER;   { Number of frames to show}
		s0, s1, s2, s3: REAL;      { Stokes Parameters }
		polar1, polar2: REAL;      {Polarizer Angles}
		polar3, polar4: REAL;
		plate1, plate2: REAL;
		Showpointer: BOOLEAN;
		StartOver, AnimateOff: BOOLEAN;
		NewData: BOOLEAN;
		DoCredits: BOOLEAN;
		PlotMenu: TMenu;
		Frame: TFrames;
		HotKeys: THotKeys;
		ncase: INTEGER;
		mu, sigma, epsilon: REAL;       { Material properties}



	PROCEDURE ShowCredits;  {Sets up Credits Screen}
		VAR
			screen: HelpScrType;
	BEGIN
		screen[1] := '                                                  ';
		screen[2] := '      ANIMATED ELECTROMAGNETIC PLANE WAVES        ';
		screen[3] := '                                                  ';
		screen[4] := '             A CUPS Simulation                    ';
		screen[5] := '                                                  ';
		screen[6] := '               by  Ron Stoner                     ';
		screen[7] := '       Department of Physics and Astronomy        ';
		screen[8] := '         Bowling Green State University           ';
		screen[9] := '            Bowling Green, OH 43403               ';
		screen[10] := '                                                  ';
		screen[11] := '  This program uses animation to illustrate the   ';
		screen[12] := '  behavior of electric and magnetic fields in a   ';
		screen[13] := '  polarized plane electromagnetic wave.  The user ';
		screen[14] := '  can vary Stokes parameters to see the effect on ';
		screen[15] := '  wave polarization, watch the behavior at an     ';
		screen[16] := '  interface, and see the effects of polarizers    ';
		screen[17] := '  and wave plates.  In each case the wave can     ';
		screen[18] := '  be viewed in perspective from different places  ';
		screen[19] := '  to assist the visualization of the 3-D spatial  ';
		screen[20] := '  relationship between the electric wave and the  ';
		screen[21] := '  magnetic wave.                                  ';
		screen[22] := '                                                  ';
		screen[23] := '    Copyright (c) John Wiley & Sons Inc. 1995     ';
		screen[24] := '                                                  ';
		screen[25] := '   >>>>>   PRESS ANY KEY TO CONTINUE      <<<<<   ';
		help(screen);
	END;


	PROCEDURE AboutProgram;    {Describes the Program}
		VAR
			screen: HelpScrType;
	BEGIN
		screen[1] := '                                                  ';
		screen[2] := '* This program uses animation to illustrate the   ';
		screen[3] := '  behavior of electric and magnetic field in a    ';
		screen[4] := '  polarized plane electromagnetic wave.  The user ';
		screen[5] := '  can choose to observe the wave in free space, or';
		screen[6] := '  to see the effect on the wave of incidence on   ';
		screen[7] := '  a material interface, or to see the effects of  ';
		screen[8] := '  optical elements that change its polarization.  ';
		screen[9] := '                                                  ';
		screen[10] := '* The user can change the polarization state of  ';
		screen[11] := '  the incident wave by specifying its Stokes      ';
		screen[12] := '  parameters.                                     ';
		screen[13] := '                                                  ';
		screen[14] := '* The electric and magnetic fields along an axis  ';
		screen[15] := '  parallel to the direction of propagation are    ';
		screen[16] := '  represented by colored lines.                   ';
		screen[17] := '                                                  ';
		screen[18] := '* Standing electromagnetic waves can be simulated ';
		screen[19] := '  by combining the incident travelling wave with  ';
		screen[20] := '  a reflected wave of the same amplitude.  The    ';
		screen[21] := '  user can do that by choosing appropriate        ';
		screen[22] := '  values of the physical properties of the medium ';
		screen[23] := '  on which the incident wave impinges in one of   ';
		screen[24] := '  the animations.                                 ';
		screen[25] := '                                                  ';
		help(screen);
	END;


	PROCEDURE AboutUnits;  {Explains units and scale for plotting fields.}
		VAR
			screen: HelpScrType;
	BEGIN
		screen[1] := '                                                  ';
		screen[2] := '* The lengths of the lines that represent electric';
		screen[3] := '  and magnetic fields are proportional to the     ';
		screen[4] := '  magnitudes of the field vectors along an axis   ';
		screen[5] := '  parallel to the direction of propagation, and   ';
		screen[6] := '  the directions of the lines are the directions  ';
		screen[7] := '  of those field vectors.  The proportionality    ';
		screen[8] := '  constants are arbitrary and would depend on     ';
		screen[9] := '  the wave intensity.                             ';
		screen[10] := '                                                  ';
		screen[11] := '* Since electric and magnetic fields contribute   ';
		screen[12] := '  equally to the average energy density for an    ';
		screen[13] := '  electromagnetic wave in free space, it is       ';
		screen[14] := '  convenient to think of electric field (E) and   ';
		screen[15] := '  magnetic field (B) as having the same amplitude ';
		screen[16] := '  as in the cgs (Gaussian) system of units.       ';
		screen[17] := '                                                  ';
		screen[18] := '* Remember that the fields do not have dimension  ';
		screen[19] := '  of length, so showing them as lines in 3-D space';
		screen[20] := '  is merely an artificial aid to visualization.   ';
		screen[21] := '  This simulation only shows the fields along     ';
		screen[22] := '  a single 1-D cut through 3-D space with         ';
		screen[23] := '  the other two perpendicular dimensions used to  ';
		screen[24] := '  represent E and B.                              ';
		screen[25] := '                                                  ';
		help(screen);
	END;

	PROCEDURE UnitsHelp;  {Help screen for more details on units.}
		VAR
			screen: HelpScrType;
	BEGIN
		screen[1] := '                                              ';
		screen[2] := '              HELP WITH UNITS                 ';
		screen[3] := '                                              ';
		screen[4] := ' Distance units are approximately centimeters,';
		screen[5] := ' (if viewing port is about 16 cm by 16 cm).   ';
		screen[6] := '                                              ';
		screen[7] := ' The time required for one oscillation is     ';
		screen[8] := ' about 6.28 internal time units.              ';
		screen[9] := '                                              ';
		screen[10] := ' Permittivity and susceptibility are relative ';
		screen[11] := ' to free space.                               ';
		screen[12] := '                                              ';
		screen[13] := ' Sigma is the conductivity in Gaussian units  ';
		screen[14] := ' divided by half the oscillation frequency.   ';
		screen[15] := '                                              ';
		screen[16] := ' E and B fields are scaled to 4 cm amplitude  ';
		screen[17] := ' when S0 = total incident intensity = 1       ';
		screen[18] := ' and no reflections.                          ';
		screen[19] := '                                              ';
		screen[20] := '                                              ';
		screen[21] := '                                              ';
		screen[22] := '                                              ';
		screen[23] := '                                              ';
		screen[24] := '                                              ';
		screen[25] := '                                              ';
		Help(screen);
	END;

	PROCEDURE StokesHelp (VAR screen: HelpScrType);
	BEGIN
		screen[1] := '    MEANINGS OF STOKES PARAMETERS             ';
		screen[2] := '                                              ';
		screen[3] := '    * Stokes parameters s0, s1, s2 and s3     ';
		screen[4] := '      characterize the wave polarization.     ';
		screen[5] := '  s0 is total incident intensity.             ';
		screen[6] := '  s1 is difference between intensities after  ';
		screen[7] := '     transmission through polarizer at 0      ';
		screen[8] := '     and 90 degrees with horizontal.          ';
		screen[9] := '  s2 is difference after transmission through ';
		screen[10] := '     polarizer at 45 and 135 degrees.         ';
		screen[11] := '  s3 is difference after passing through      ';
		screen[12] := '     right-hand and left-hand circular        ';
		screen[13] := '     polarizers.                              ';
		screen[14] := '                                              ';
		screen[15] := '    * Stokes parameters are automatically     ';
		screen[16] := '      renormalized to make s0 = 1.            ';
		screen[17] := '                                              ';
		screen[18] := '    * These parameters describe only the wave ';
		screen[19] := '      incident from free space on the left.   ';
		screen[20] := '                                              ';
		screen[21] := '   (s1,s2,s3)  -->  Polarization State        ';
		screen[22] := '                                              ';
		screen[23] := '   ( 1, 0, 0)  -->  Plane polarized           ';
		screen[24] := '   ( 0, 1, 0)  -->  Plane polarized           ';
		screen[25] := '   ( 0, 0, 1)  -->  Circularly polarized      ';
	END;

	PROCEDURE PerspectiveHelp;
		VAR
			screen: HelpScrType;
	BEGIN
		screen[1] := '         PERSPECTIVE VIEWING HELP             ';
		screen[2] := '                                              ';
		screen[3] := ' The arrow keys change the angular location   ';
		screen[4] := ' of the position of the viewer eye in the     ';
		screen[5] := ' frame of reference shown.  Use them to       ';
		screen[6] := ' look at the wave from different points of    ';
		screen[7] := ' view "on the fly."                           ';
		screen[8] := '                                              ';
		screen[9] := ' The perspective effect (i.e. nearby things   ';
		screen[10] := ' look bigger than those more distant) can     ';
		screen[11] := ' be enhanced by using the CHANGE menu to      ';
		screen[12] := ' reduce the viewpoint to origin distance.     ';
		screen[13] := '                                              ';
		screen[14] := ' Use a very large value for this viewpoint to ';
		screen[15] := ' origin distance to eliminate perspective and ';
		screen[16] := ' do simple projection onto the screen.  Use   ';
		screen[17] := ' this technique to see the behavior of a      ';
		screen[18] := ' single component of the field, for example.  ';
		screen[19] := '                                              ';
		screen[20] := '                                              ';
		screen[21] := '                                              ';
		screen[22] := '                                              ';
		screen[23] := '                                              ';
		screen[24] := '                                              ';
		screen[25] := '                                              ';
		Help(screen);
	END;

	PROCEDURE AnimateHelp (VAR screen: HelpScrType);
	BEGIN
		screen[1] := '   PARAMETERS SET FROM THIS SCREEN AFFECT     ';
		screen[2] := '   THE SPEED, DURATION AND SMOOTHNESS OF THE  ';
		screen[3] := '   WAVE ANIMATION.                            ';
		screen[4] := '                                              ';
		screen[5] := '    * The real-time speed of the animation    ';
		screen[6] := '      depends on the clock speed and display  ';
		screen[7] := '      speed of your computer, which are       ';
		screen[8] := '      limiting factors on the simulation.     ';
		screen[9] := '                                              ';
		screen[10] := '    * The animation sequence will stop        ';
		screen[11] := '      automatically after the specified       ';
		screen[12] := '      number of frames have been displayed.   ';
		screen[13] := '                                              ';
		screen[14] := '    * You can speed the animation by showing  ';
		screen[15] := '      only a few frames per oscillation, but  ';
		screen[16] := '      this may make the animation "jerky".    ';
		screen[17] := '                                              ';
		screen[18] := '    * If the distance from viewpoint (i.e. the';
		screen[19] := '      position of your eye) to the origin is  ';
		screen[20] := '      small, the perspective effect will be   ';
		screen[21] := '      enhanced, and vice-versa.               ';
		screen[22] := '                                              ';
		screen[23] := '    * Showing too many wavelengths may        ';
		screen[24] := '      undersample the fields and degrade the  ';
		screen[25] := '      wavelike appearance of the display.     ';
	END;

	PROCEDURE MaterialHelp (VAR screen: HelpScrType);
	BEGIN
		screen[1] := '     MEANING OF THE MATERIAL PARAMETERS       ';
		screen[2] := '                                              ';
		screen[3] := ' Distance units are approximately centimeters,';
		screen[4] := ' (if the viewing port about 16 cm by 16 cm).  ';
		screen[5] := '                                              ';
		screen[6] := ' The time required for one oscillation is     ';
		screen[7] := ' about 6.28 internal time units.              ';
		screen[8] := '                                              ';
		screen[9] := ' Permittivity and susceptibility are relative ';
		screen[10] := ' to free space.                               ';
		screen[11] := '                                              ';
		screen[12] := ' Sigma is the conductivity in Gaussian units  ';
		screen[13] := ' divided by half the oscillation frequency.   ';
		screen[14] := '                                              ';
		screen[15] := ' E and B fields are scaled to 4 cm amplitude  ';
		screen[16] := ' when S0 = total incident intensity = 1       ';
		screen[17] := ' and no reflections.                          ';
		screen[18] := '                                              ';
		screen[19] := '                                              ';
		screen[20] := '                                              ';
		screen[21] := '                                              ';
		screen[22] := '                                              ';
		screen[23] := '                                              ';
		screen[24] := '                                              ';
		screen[25] := '                                              ';
	END;

	PROCEDURE WhatToDo;
		VAR
			screen: HelpScrType;
	BEGIN
		screen[1] := '                                              ';
		screen[2] := '                 WHAT TO DO                   ';
		screen[3] := '                                              ';
		screen[4] := '    * Use Arrow Keys to look at the wave from ';
		screen[5] := '      different points of view.               ';
		screen[6] := '                                              ';
		screen[7] := '    * Press F10 or <ESC> to get to the main   ';
		screen[8] := '      menu where more help is available.      ';
		screen[9] := '                                              ';
		screen[10] := '    * CHANGE the Stokes Parameters to change  ';
		screen[11] := '      the polarization of the incident wave.  ';
		screen[12] := '      These will be different for the         ';
		screen[13] := '      transmitted wave, in general,           ';
		screen[14] := '      due to the effect of polarizers         ';
		screen[15] := '      and wave plates.                        ';
		screen[16] := '                                              ';
		screen[17] := '    * If field vectors extend beyond frame    ';
		screen[18] := '      boundaries, increase the viewpoint to   ';
		screen[19] := '      origin distance using the CHANGE menu.  ';
		screen[20] := '                                              ';
		screen[21] := '    * Options under CHANGE menu allow switch ';
		screen[22] := '      to purely plane or circularly polarized ';
		screen[23] := '      incident wave.                          ';
		screen[24] := '                                              ';
		screen[25] := '                                              ';
		Help(screen);
	END;

	PROCEDURE SimulateHelp;
		VAR
			screen: HelpScrType;
	BEGIN
		screen[1] := '   SIMULATIONS AVAILABLE:                     ';
		screen[2] := '                                              ';
		screen[3] := '    * First animation illustrates a polarized ';
		screen[4] := '      electromagnetic plane wave travelling   ';
		screen[5] := '      through matter-free space in the +X     ';
		screen[6] := '      direction.  Second animation shows the  ';
		screen[7] := '      reflection and transmission of this wave';
		screen[8] := '      at the interface between free space and ';
		screen[9] := '      matter.                                 ';
		screen[10] := '                                              ';
		screen[11] := '    * Other animations show the effects of    ';
		screen[12] := '      polarizers and wave plates on the wave. ';
		screen[13] := '                                              ';
		screen[14] := '    * Choose an animation by selecting        ';
		screen[15] := '      SHOW WHAT from the menuscreen.          ';
		screen[16] := '                                              ';
		screen[17] := '    * You need only change the parameters that';
		screen[18] := '      affect the animation you choose.        ';
		screen[19] := '                                              ';
		screen[20] := '                                              ';
		screen[21] := '                                              ';
		screen[22] := '                                              ';
		screen[23] := '                                              ';
		screen[24] := '                                              ';
		screen[25] := '                                              ';
		Help(screen);
	END;

	PROCEDURE DataHelp (screen: HelpScrType);
	BEGIN
		screen[1] := '   HELP WITH THE DATA SCREEN:                 ';
		screen[2] := '                                              ';
		screen[3] := '    * Orientation angles are in degrees       ';
		screen[4] := '      and represent the angle the axis of the ';
		screen[5] := '      active optical element makes with the   ';
		screen[6] := '      x-axis.                                 ';
		screen[7] := '                                              ';
		screen[8] := '    * Remember that polaroids absorb one      ';
		screen[9] := '      polarization component.                 ';
		screen[10] := '                                              ';
		screen[11] := '    * 1/4-wave plates change the phase of     ';
		screen[12] := '      the component polarized in the direction';
		screen[13] := '      of their axis by 45 degrees relative    ';
		screen[14] := '      to the component perpendicular to the   ';
		screen[15] := '      axis. .                                 ';
		screen[16] := '                                              ';
		screen[17] := '    * Remember CANCEL ignores the changes     ';
		screen[18] := '      you have made in the parameters, but    ';
		screen[19] := '      OK accepts them if they are in the      ';
		screen[20] := '      allowed range.                          ';
		screen[21] := '                                              ';
		screen[22] := '                                              ';
		screen[23] := '                                              ';
		screen[24] := '                                              ';
		screen[25] := '                                              ';
	END;

	PROCEDURE Defaults;
  {Sets or resets global variables to default values.}
	BEGIN
		casename[1] := 'PLANE E-M WAVE';
		casename[2] := 'MATERIAL INTERFACE';
		casename[3] := 'POLARIZER';
		casename[4] := 'WAVEPLATE';
		casename[5] := 'POLARIZER & WAVEPLATE';
		casename[6] := 'TWO POLARIZERS';
		ShowPointer := TRUE;
		ncase := 1;   {Animation number}
		NFrames := 9999;   {Frames in animation sequence}
		NWaves := 2.0;     {Length of wavetrain shown in one frame}
		dt := 0.20;  {internal time interval between frames}
		ViewR := 20; {distance from viewer eye to screen (cm)}
		s0 := 1;     {Stokes parameters}
		s1 := 1;
		s2 := 0.0;
		s3 := 0.0;
		mu := 1.0;   {Relative magnetic susceptibility in animation #2}
		sigma := 1.0; {Conductivity (divided by twice the frequency)}
		epsilon := 2.0; {Relative electric permittivity}
		polar1 := 30;   {polarizer orientations}
		polar2 := -15;
		polar3 := 45;
		polar4 := 90;
		plate1 := 45;  {waveplate orientations}
		plate2 := 30;
		Az := (PI / 180) * 20.0;   {Azimuth of viewer in wave coordinate frame}
		Alt := (PI / 180) * 15.0   {Altitude of viewer in wave frame}
	END;


  {******************* Utilities and Drawing Procedures *******************}
	FUNCTION EscapeFromThis: BOOLEAN;
	BEGIN
		CheckForEvents;
		IF (ord(Event.ReadKey) = 27) THEN
			EscapeFromThis := TRUE
		ELSE
			EscapeFromThis := FALSE;
	END;

	PROCEDURE PlotLine (x1, y1, x2, y2: REAL);
    { line color, and style can be changed.}
		VAR
			Gx1, Gy1, Gx2, Gy2: integer;
	BEGIN
		Map(x1, y1, Gx1, Gy1);
		Map(x2, y2, Gx2, Gy2);
		Line(Gx1, Gy1, Gx2, Gy2);
	END;



	PROCEDURE TrigFunctions;
  {Called each time viewer azimuth or altitude changes}
	BEGIN
		CAz := COS(Az);
		SAz := SIN(Az);
		CAlt := COS(Alt);
		SAlt := SIN(Alt);
	END;  {TrigFunctions}


	PROCEDURE ShowInformation;
  {Prints current values for reference during animation sequence}
	BEGIN
		GraphBackColor := DarkGray;
		OpenViewport(2);
		GraphBackcolor := Blue;
		Openviewport(3);
		GraphBackColor := BLAKK;
		SelectViewPort(2);
		SetColor(LightGreen);
		Print(3, 1, CaseName[ncase]);
		SetColor(whitte);
		Print(1, 2, ' Stokes Parameters:');
		Print(1, 3, '   S0 =  1.0000');
		Print(1, 4, '   S1 = ');
		Print(1, 5, '   S2 = ');
		Print(1, 6, '   S3 = ');
		Print(10, 4, NumStr(s1, 6, 4));
		Print(10, 5, NumStr(s2, 6, 4));
		Print(10, 6, NumStr(s3, 6, 4));
		SelectViewport(3);
		SetColor(LightRed);
		Print(2, 3, '    <ESC>    F10');
		Print(2, 1, Concat('    ', CHR(24), ', ', CHR(25), ', ', CHR(26), '    ', CHR(27)));
		SetColor(Whitte);
		Print(2, 1, 'Use         or ');
		Print(2, 2, 'to change viewpoint.');
		Print(2, 3, 'Use       or     to');
		Print(2, 4, 'reach upper menu.');
		SelectViewport(2);
		CASE ncase OF
			1: 
			BEGIN  {PLANE-POLARIZED ELECTROMAGNETIC WAVE}
				Print(1, 8, ' Sum of squares of');
				Print(1, 9, ' S1, S2 & S3 reset to ');
				Print(1, 10, ' square of S0 = 1.00. ');
			END;
			2: 
			BEGIN  {E-M WAVE INCIDENT ON MATERIAL INTERFACE}
				Print(1, 7, Concat(' Eps = ', NumStr(Epsilon, 5, 3)));
				Print(1, 8, concat(' Mu = ', NumStr(Mu, 5, 3)));
				Print(1, 9, Concat(' Sigma = ', NumStr(sigma, 5, 3)));
				Print(1, 10, ' (Medium occupies x>0.)');
			END;
			3: 
			BEGIN  {E-M WAVE WITH POLARIZER}
				Print(1, 8, ' Polarizer angle');
				Print(1, 9, Concat(' Theta = ', NumStr(polar1, 3, 0), CHR(248)));
				Print(1, 10, ' ');
			END;
			4: 
			BEGIN  {E-M WAVE WITH 1/4-WAVE PLATE}
				Print(1, 8, ' Waveplate Angle ');
				Print(1, 9, Concat(' Theta = ', NumStr(plate1, 3, 0), CHR(248)));
			END;
			5: 
			BEGIN  {E-M WAVE WITH POLARIZER AND 1/4-WAVE PLATE}
				Print(1, 7, ' Polarizer Angle');
				Print(1, 8, concat(' Theta1 =', NumStr(polar2, 3, 0), CHR(248)));
				Print(1, 9, ' Waveplate Angle');
				Print(1, 10, concat(' Theta2 =', NumStr(plate2, 3, 0), CHR(248)));
			END;
			6: 
			BEGIN  {E-M WAVE WITH TWO POLARIZERS}
				Print(1, 7, ' 1st Polarizer Angle');
				Print(1, 8, concat(' Theta1 = ', NumStr(polar3, 3, 0), CHR(248)));
				Print(1, 9, ' 2nd Polarizer Angle');
				Print(1, 10, concat(' Theta2 = ', NumStr(polar4, 3, 0), CHR(248)));
			END;
		END; {case}
       {ShowMouse ;}
	END;  {ShowInformation}


	PROCEDURE SetupGraph;
  {Defines viewports and scales}
	BEGIN
		DefineViewport(1, 0.32, 1.0, 0.08, 0.92);
		DefineViewPort(2, 0.00, 0.30, 0.31, 0.92);
		DefineViewPort(3, 0.00, 0.30, 0.076, 0.30);
		DefineScale(1, -8, 8, -8, 8);
		SelectScale(1);
		SelectViewport(1);
	END; {SetupGraph}


	PROCEDURE Perspective (x, y, z: real; VAR Xview, Yview: real);
   (*Finds coordinates (Xview,Yview) on viewing screen a distance P}
{
    from the viewer that is intersected by the line-of-sight from a}
{
    viewed point (x,y,z) to the viewer's location. *)
		VAR
			denom: real;
	BEGIN
		denom := ViewR - y * SAlt - CAlt * (z * CAz + x * SAz);
		Xview := P * (x * CAz - z * SAz) / denom;
		Yview := P * (y * CAlt - (z * CAz + x * SAz) * SAlt) / denom;
	END; {Perspective}


	PROCEDURE Arrow (x0, y0, z0, x1, y1, z1: REAL; barbs: BOOLEAN);
     (* Draws an arrow from (x0,y0,z0) to (x1,y1,z1) on the active}
{
       viewport using perspective projection.  Barbs are 1/10 length}
{
       of arrow and assumed to lie in the y-z plane.  *)

		CONST
			BarbSize = 0.05;
			range = 8;

		VAR
			xorg, yorg: REAL;
			xend, yend: REAL;
			zbarb, ybarb: REAL;
			temp: REAL; {Intermediate Variable}

	BEGIN
		Perspective(x0, y0, z0, xorg, yorg);   { Plot line from origin to terminus.}
		Perspective(x1, y1, z1, xend, yend);
		IF abs(xend) > range THEN
		BEGIN          {Replace by intersection with x-boundary.}
			temp := range * xend / abs(xend);
			yend := yorg + (yend - yorg) * (temp - xorg) / (xend - xorg);
			xend := temp
		END;
		IF abs(yend) > range THEN
		BEGIN          {Replace by intersection with y-boundary.}
			temp := range * yend / abs(yend);
			xend := xorg + (xend - xorg) * (temp - yorg) / (yend - yorg);
			yend := temp
		END;
		PlotLine(xorg, yorg, xend, yend);
		IF barbs THEN             {Add barbs at terminus.}
		BEGIN
			zbarb := z1 + BarbSize * (-z1 + z0 + (y1 - y0) / 2);  {Plot barb lines.}
			ybarb := y1 + BarbSize * (-y1 + y0 - (z1 - z0) / 2);
			xorg := xend;
			yorg := yend;
			Perspective(x1, ybarb, zbarb, xend, yend);
			Plotline(xorg, yorg, xend, yend);
			zbarb := z1 + BarbSize * (-z1 + z0 - (y1 - y0) / 2);
			ybarb := y1 + BarbSize * (-y1 + y0 + (z1 - z0) / 2);
			Perspective(x1, ybarb, zbarb, xend, yend);
			Plotline(xorg, yorg, xend, yend);
		END;
	END; {Arrow}


	PROCEDURE DrawAxes;
     (* Plots axes.  Center of viewscreen is assumed to be along}
{
       line of sight to point (Pi, 0, 0). y-axis and z-axis both}
{
       are represented by lines 5 units long.*)
		VAR
			xorg, yorg, xend, yend: real;

	BEGIN
		SetColor(LightGreen);
		Arrow(0, 0, 0, 5, 0, 0, FALSE); {Plot pos. x-axis}
		Arrow(0, 0, 0, -5, 0, 0, FALSE);{Plot neg. x-axis}
		Arrow(0, 0, -5, 0, 0, 5, TRUE);  {z-axis with barbs}
		Arrow(0, -5, 0, 0, 5, 0, TRUE);  {y-axis with barbs}
	END;{AxesPlot}


	PROCEDURE ShowInterface;   {a grid of dots in y-z plane}
		VAR
			ydot, zdot: REAL;
			xplot, yplot: REAL;
			ix, iy: INTEGER;
	BEGIN
		SetColor(LightBlue);
		FOR ix := -4 TO 5 DO
			FOR iy := -4 TO 5 DO
			BEGIN
				zdot := ix - 0.5;
				ydot := iy - 0.5;     {Offset dot grid from axes}
				Perspective(0.0, ydot, zdot, xplot, yplot);
				PlotSymbol(xplot, yplot, '.')
			END;
	END; {InterfacePlot}


	PROCEDURE ShowPolarizer (xlocus, theta: REAL; gcolor, Ngrids: INTEGER);
  (* Draws a grilled square parallel to y-z plane representing}
{
    polarizers and/or wave plates.  *)
		VAR
			sthet, cthet: REAL;
			y1, z1, y2, z2: REAL;
			xp1, yp1: REAL;
			xp2, yp2: REAL;
			grid: REAL;
			igrid, ns: INTEGER;

	BEGIN
		SetColor(gcolor);
		sthet := sin(theta);
		cthet := cos(theta);
		FOR igrid := 0 TO Ngrids DO      {Draw a grill at angle theta}
		BEGIN
			grid := -4 + igrid * (8 / Ngrids);
			z1 := grid * cthet - 4 * sthet;
			y1 := -grid * sthet - 4 * cthet;
			z2 := grid * cthet + 4 * sthet;
			y2 := -grid * sthet + 4 * cthet;
			Arrow(xlocus, y1, z1, xlocus, y2, z2, FALSE);
		END;
		grid := -4;                       {Terminate the grill lines}
		z1 := grid * cthet - 4 * sthet;
		y1 := -grid * sthet - 4 * cthet;
		grid := 4;
		z2 := grid * cthet - 4 * sthet;
		y2 := -grid * sthet - 4 * cthet;
		Arrow(xlocus, y1, z1, xlocus, y2, z2, FALSE);
		grid := -4;
		z1 := grid * cthet + 4 * sthet;
		y1 := -grid * sthet + 4 * cthet;
		grid := 4;
		z2 := grid * cthet + 4 * sthet;
		y2 := -grid * sthet + 4 * cthet;
		Arrow(xlocus, y1, z1, xlocus, y2, z2, FALSE);
	END;

  {*******************  Physics Procedures ***********************}

	PROCEDURE FieldPlot (time: REAL; a1, a2, del: REAL;  {Incident wave parameters}
									theta1, theta2: REAL;  {Polarizer orientations}
									a1r, a2r, Rphase: REAL;  {Reflected wave parameters}
									a1t, a2t, delt: REAL;  {Transmitted wave parameters}
									Tphase, Bphase: REAL;  {phase of wavevector}
									v, alpha, beta: REAL); {speed, wave number k=beta+i*alpha }
    {Procedure to draw the wave, axes, etc. in a given frame}
		CONST
			quarter = 1.57079;

		VAR
			Ey, Ez, By, Bz: REAL;  {field components}
			x, phasei: REAL;  {position and phase of incident wave}
			phaset, phaser: REAL;  {phases of transmitted and reflected parts}
			temp, kmag, ratio: REAL;  {intermediate variables}
			index: INTEGER;

	BEGIN
		ratio := sqrt(sqr(beta) + sqr(alpha));
		kmag := ratio * kvac;
		SelectViewPort(1);
		SelectScale(1);
		FOR index := -13 TO 0 DO       {Show E and B for incident + reflected}
		BEGIN                        {waves at points on the negative x-axis }
			x := (round(index) - 0.5) * (Pi / 9.0);
			phasei := kvac * (x - time);
			phaser := -kvac * (x + time) + Rphase;
			temp := a1r * cos(phaser);    {Reflected wave}
			Ey := a1 * cos(phasei) + temp;{added to transmitted wave}
			Bz := Ey - 2 * temp; {Correction 1/31/95}
			temp := a2r * cos(phaser + del);
			Ez := a2 * cos(phasei + del) + temp;
			By := -Ez + 2 * temp;  {Correction 1/31/95}
			Setcolor(Yellow);               {Plot vectors for E and B in}
			Arrow(x, 0.0, 0.0, x, Ey, Ez, FALSE);  {different colors.}
			SetColor(LightRed);
			Arrow(x, 0.0, 0.0, x, By, Bz, FALSE);
		END;

		CASE ncase OF
			1: 
				;      (* no extras *)
			2: 
				ShowInterface;
			3: 
				ShowPolarizer(-0.1, theta1, lightblue, 15);
			4: 
			BEGIN
				ShowPolarizer(-0.05, theta2, LightGray, 2);
				ShowPolarizer(0.05, theta2 + quarter, LightGray, 2)
			END;
			5: 
			BEGIN
				ShowPolarizer(-0.1, theta1, lightblue, 15);
				ShowPolarizer(0.05, theta2, LightGray, 2);
				ShowPolarizer(0.1, theta2 + quarter, LightGray, 2)
			END;
			6: 
			BEGIN
				ShowPolarizer(-0.1, theta1, Magenta, 15);
				ShowPolarizer(+0.1, theta2, Cyan, 15)
			END;
		END;

		FOR index := 1 TO 14 DO   {Show E and B for transmitted part}
		BEGIN                    {along positive x-axis}
			x := (round(index) - 0.5) * (Pi / 9.0);
			phaset := kvac * (x * beta - time) + Tphase;
			temp := exp(-alpha * x);
			Ey := a1t * cos(phaset) * temp;
			Bz := ratio * a1t * cos(phaset + Bphase) * temp;
			Ez := a2t * cos(phaset + delt) * temp;
			By := -ratio * a2t * cos(phaset + Bphase + delt) * temp;
			SetColor(Yellow);
			Arrow(x, 0, 0, x, Ey, Ez, FALSE);
			SetColor(LightRed);
			Arrow(x, 0, 0, x, By, Bz, FALSE);
		END;

		DrawAxes;
	END; {FieldPlot}


	PROCEDURE ComputeCoeff (a1, a2, epsin, muin, sigmain: REAL;
                           VAR a1r, a2r, a1t, a2t, Rphase, Tphase, Bphase,
                           alpha, beta, v: REAL);

  (* Procedure to compute amplitudes and phases of the transmitted}
{
 and reflected waves *)
		VAR
			temp: real;                                {intermediate variable}
			RCoeff, TCoeff: real;     {Reflection and Transmission Coefficients}
			xcomp, ycomp: real;     {Intermediate Variables}

	BEGIN
		v := 1 / sqrt(epsin * muin);                        {Note c = 1 in the units}
		beta := sqrt((1 + sqrt(1 + sqr(sigmain / epsin))) / 2) / v; {real part of k vector}
		alpha := 0.5 * sigmain * muin / beta;              {imaginary part of k vector}
		TCoeff := 2 / sqrt((sqr(1 + beta) + sqr(alpha)));
		a1t := Tcoeff * a1;
		a2t := Tcoeff * a2;          {Transmitted components}
		temp := sqr(1 - sqr(beta) - sqr(alpha)) + 4 * sqr(alpha);
		Rcoeff := sqrt(temp) / (sqr(1 + beta) + sqr(alpha));
		a1r := Rcoeff * a1;
		a2r := Rcoeff * a2;            {Reflected components}

		xcomp := 1 - SQR(beta) - SQR(alpha);
		ycomp := -2 * alpha;
		IF (SQR(xcomp) + SQR(ycomp) > 100 * Macheps) THEN
			Rphase := ArcTan2(xcomp, ycomp)
		ELSE
			Rphase := 0.0;

		Tphase := ArcTan2((1 + beta), -alpha);

		Bphase := ArcTan2(beta, alpha);    {Note kB = wE from Faraday Law}

	END; {ComputeCoeff}


	PROCEDURE OperateOnBeam (MatrixIn: CMatrixType; PhasorIn: PhasorType; VAR PhasorOut: PhasorType);
  (* Transforms PhasorIn to PhasorOut by matrix multiplication. *)
		VAR
			z1, z2: Complex;
	BEGIN
		CxOp(MatrixIn[1, 1], '*', PhasorIn[1], z1);
		CxOp(MatrixIn[1, 2], '*', PhasorIn[2], z2);
		CxOp(z1, '+', z2, PhasorOut[1]);
		CxOp(MatrixIn[2, 1], '*', PhasorIn[1], z1);
		CxOp(MatrixIn[2, 2], '*', PhasorIn[2], z2);
		CxOp(z1, '+', z2, PhasorOut[2]);
	END;


	PROCEDURE DoPolarizers (a1, a2, del, theta1, theta2: REAL; VAR a1t, a2t, delt: REAL);

  (* Computes effects on complex wave amplitudes of optical}
{
    elements, i.e. polarizers and wave plates.}
{
    Rotation matrices rotate wave axes to align with polarizer or wave-}
{
    plate axis; optical elements represented by diagonal matrices that}
{
    absorb or change phase of one component; rotate back after}
{
    matrix transform by optical element.*)

		VAR
			Analyzer: CMatrixType;   {Zeros 2nd component of a phasor}
			WavePlate: CMatrixType;   {Shifts 2nd component by pi/2}
			Rotate: CMatrixType;   {Rotates z-y frame by theta}
			Rotate2: CMatrixType;
			RotateBack: CMatrixType;   {Reverse rotation by theta}
			Rotate2Back: CMatrixType;
			InBeam: PhasorType;
			OutBeam: PhasorType;
			i, j: INTEGER;

	BEGIN
		InBeam[1].Re := a1;             {a1 is real -- zero phase shift}
		InBeam[1].Im := 0;
		InBeam[2].Re := a2 * cos(del);    {a2 is different by del radians}
		InBeam[2].Im := a2 * sin(del);
		FOR i := 1 TO 2 DO
			FOR j := 1 TO 2 DO   {Initialize matrices}
			BEGIN
				Analyzer[i, j].Re := 0;
				WavePlate[i, j].Re := 0;
				Analyzer[i, j].Im := 0;
				WavePlate[i, j].Im := 0;
				Rotate[i, j].Im := 0;
				RotateBack[i, j].Im := 0;
				Rotate2[i, j].Im := 0;
				Rotate2Back[i, j].Im := 0;
			END;

		Analyzer[1, 1].Re := 1;
		WavePlate[1, 1].Re := 1;
		WavePlate[2, 2].Im := -1;
         (* Set up axes rotation by theta1 *)
		Rotate[1, 1].Re := COS(theta1);
		Rotate[2, 2] := Rotate[1, 1];
		Rotate[1, 2].Re := SIN(theta1);
		Rotate[2, 1].Re := -Rotate[1, 2].Re;
		RotateBack[1, 1] := Rotate[1, 1];
		RotateBack[2, 2] := Rotate[2, 2];
		RotateBack[1, 2] := Rotate[2, 1];
		RotateBack[2, 1] := Rotate[1, 2];
         (* Set up axes rotation by theta2 *)
		Rotate2[1, 1].Re := COS(theta2);
		Rotate2[2, 2] := Rotate2[1, 1];
		Rotate2[1, 2].Re := SIN(theta2);
		Rotate2[2, 1].Re := -Rotate2[1, 2].Re;
		Rotate2Back[1, 1] := Rotate2[1, 1];
		Rotate2Back[2, 2] := Rotate2[2, 2];
		Rotate2Back[1, 2] := Rotate2[2, 1];
		Rotate2Back[2, 1] := Rotate2[1, 2];

		OutBeam := InBeam;


		IF ncase <> 4 THEN   {Insert 1st Analyzer}
		BEGIN
			OperateOnBeam(Rotate, OutBeam, OutBeam);
			OperateOnBeam(Analyzer, OutBeam, OutBeam);
			OperateOnBeam(RotateBack, OutBeam, OutBeam);
		END;
		IF ((ncase = 4) OR (ncase = 5)) THEN   {Insert WavePlate}
		BEGIN
			OperateOnBeam(Rotate2, OutBeam, OutBeam);
			OperateOnBeam(WavePlate, OutBeam, OutBeam);
			OperateOnBeam(Rotate2Back, Outbeam, Outbeam);
		END;
		IF ncase = 6 THEN {Insert 2nd analyzer}
		BEGIN
			OperateOnBeam(Rotate2, OutBeam, OutBeam);
			OperateOnBeam(Analyzer, OutBeam, OutBeam);
			OperateOnBeam(Rotate2Back, OutBeam, OutBeam);
		END;
		a1t := CAbs(OutBeam[1]);
		a2t := CAbs(OutBeam[2]);
		delt := Arg(OutBeam[2]) - Arg(OutBeam[1]);
	END; {Polarizer}

  { *********  Procedures for I/O and Program Control ***********}

	FUNCTION MaterialData: boolean;  (* Gets material properties. *)
		VAR
			DataScreen: TInputScreen;
			Hscreen: HelpScrType;
	BEGIN
		WITH DataScreen DO
		BEGIN
			init;
			DefineInputPort(0.15, 0.85, 0.2, 0.8);
			LoadLine(' SIMULATION OF A PLANE ELECTROMAGNETIC WAVE');
			LoadLine('      INCIDENT ON A MATERIAL INTERFACE');
			LoadLine('');
			LoadLine('  PROVIDE MATERIAL PROPERTIES:');
			LoadLine('      Permittivity epsilon = {      } (1 - 16)');
			LoadLine('      Permeability mu      = {      }  (1 - 16)');
			LoadLine('      Conductivity sigma   = {      } (0 - 1.0E6)');
			LoadLine('');
			LoadLine('       [  Ok  ]   [Cancel]   [ Help ]');
			SetNumber(1, epsilon);
			SetNumberLimits(1, 1.0, 16.0);
			SetNumber(2, mu);
			SetNumberLimits(2, 1.0, 16.0);
			SetNumber(3, sigma);
			SetNumberLimits(3, 0.0, 1.0E6);
			MaterialHelp(Hscreen);
			SetHelpScreen(Hscreen);
			Accept;
			epsilon := GetNumber(1);
			mu := GetNumber(2);
			sigma := GetNumber(3);
			MaterialData := NOT Canceled;
			done;
		END;
	END;

	FUNCTION PolarizerData: boolean; (* Gets orientation angles for polarizer. *)
		VAR
			DataScreen: TInputScreen;
			HScreen: HelpScrType;
	BEGIN
		WITH DataScreen DO
		BEGIN
			init;
			DefineInputPort(0.2, 0.8, 0.25, 0.75);
			LoadLine('SIMULATION OF A PLANE ELECTROMAGNETIC WAVE');
			LoadLine('      INCIDENT ON A PLANE POLARIZER');
			LoadLine('');
			LoadLine('  PROVIDE POLARIZER ORIENTATION ANGLE:');
			LoadLine('      THETA = {      } degs. (0 - 180)');
			LoadLine('');
			LoadLine('      [  Ok  ]  [Cancel]  [ Help ]');
			SetNumber(1, Polar1);
			SetNumberLimits(1, 0.0, 360.0);
			DataHelp(Hscreen);
			SetHelpScreen(Hscreen);
			Accept;
			polar1 := GetNumber(1);
			PolarizerData := NOT Canceled;
			done;
		END;
	END;

	FUNCTION WaveplateData: boolean;  (* Gets orientation of 1/4-waveplate *)
		VAR
			DataScreen: TInputScreen;
			HScreen: HelpScrType;
	BEGIN
		WITH DataScreen DO
		BEGIN
			init;
			DefineInputPort(0.2, 0.8, 0.25, 0.75);
			LoadLine(' SIMULATION OF A PLANE ELECTROMAGNETIC WAVE');
			LoadLine('       INCIDENT ON A 1/4-WAVE PLATE');
			LoadLine('');
			LoadLine('  PROVIDE WAVEPLATE AXIS ORIENTATION ANGLE:');
			LoadLine('      THETA = {      } degs. (0 - 180)');
			LoadLine('');
			LoadLine('     [  Ok  ]  [Cancel]  [ Help ]');
			SetNumber(1, Plate1);
			SetNumberLimits(1, 0.0, 360.0);
			DataHelp(Hscreen);
			SetHelpScreen(Hscreen);
			Accept;
			plate1 := GetNumber(1);
			WaveplateData := NOT Canceled;
			done;
		END;
	END;


	FUNCTION PolarPlateData: boolean;
		VAR
			DataScreen: TInputScreen;
			HScreen: HelpScrType;
	BEGIN
		WITH DataScreen DO
		BEGIN
			init;
			DefineInputPort(0.2, 0.8, 0.2, 0.8);
			LoadLine(' SIMULATION OF PLANE ELECTROMAGNETIC WAVES');
			LoadLine('   WITH A 1/4-WAVE PLATE AND POLARIZER');
			LoadLine('');
			LoadLine('  PROVIDE POLARIZER ORIENTATION ANGLE:');
			LoadLine('      THETA1 = {      } degs. (0 - 180)');
			LoadLine('');
			LoadLine('  PROVIDE WAVEPLATE ORIENTATION ANGLE:');
			LoadLine('      THETA2 = {      } degs. (0 - 180)');
			LoadLine('');
			LoadLine('     [  Ok  ]  [Cancel]  [ Help ]');
			SetNumber(1, Polar2);
			SetNumberLimits(1, 0.0, 360.0);
			SetNumber(2, Plate2);
			SetNumberLimits(2, 0.0, 360.0);
			DataHelp(Hscreen);
			SetHelpScreen(Hscreen);
			Accept;
			polar2 := GetNumber(1);
			plate2 := GetNumber(2);
			PolarPlateData := NOT Canceled;
			done;
		END;
       (* dispose(dataScreen); *)
	END;

	FUNCTION TwoPolarData: boolean;
		VAR
			DataScreen: TInputScreen;
			HScreen: HelpScrType;
	BEGIN
		WITH DataScreen DO
		BEGIN
			init;
			DefineInputPort(0.2, 0.8, 0.2, 0.8);
			LoadLine(' SIMULATION OF PLANE ELECTROMAGNETIC WAVES');
			LoadLine('          WITH TWO POLARIZERS');
			LoadLine('');
			LoadLine('  ORIENTATION ANGLE FOR FIRST POLARIZER:');
			LoadLine('      THETA1 = {      } degs. (0 - 180)');
			LoadLine('');
			LoadLine('  ORIENTATION ANGLE FOR SECOND POLARIZER:');
			LoadLine('      THETA2 = {      } degs. (0 - 180)');
			LoadLine('');
			LoadLine('       [  Ok  ]  [Cancel]  [ Help ]');
			SetNumber(1, Polar3);
			SetNumberLimits(1, 0.0, 360.0);
			SetNumber(2, Polar4);
			SetNumberLimits(2, 0.0, 360.0);
			DataHelp(Hscreen);
			SetHelpScreen(Hscreen);
			Accept;
			polar3 := GetNumber(1);
			polar4 := GetNumber(2);
			TwoPolarData := NOT Canceled;
			done;
		END;
	END;


	FUNCTION GetStokesParameters: boolean;
         (* User supplies or changes values of Stokes parameters. *)
		VAR
			StokesScreen: TInputScreen;
			Nomistake: BOOLEAN;
			Hscreen: HelpScrType;
	BEGIN
       (* new(StokesScreen); *)
		NoMistake := TRUE;
		WITH StokesScreen DO
		BEGIN
			init;
			DefineInputPort(0.2, 0.8, 0.2, 0.8);
			LoadLine('  USE THIS SCREEN TO FIX THE POLARIZATION');
			LoadLine('           OF THE INCIDENT WAVE');
			LoadLine('');
			LoadLine('         SUPPLY STOKES PARAMETERS:');
			LoadLine('       s1 = {      } = I( 0) - I( 90)');
			LoadLine('       s2 = {      } = I(45) - I(135)');
			LoadLine('       s3 = {      } = I(Right) -I(Left)');
			LoadLine('   (Values should be in the range -1 < s < 1)');
			LoadLine('');
			LoadLine('        [  Ok  ]   [Cancel]   [ Help ]');
			SetNumber(1, s1);
			SetNumberLimits(1, -1, 1);
			SetNumber(2, s2);
			SetNumberLimits(2, -1, 1);
			SetNumber(3, s3);
			SetNumberLimits(3, -1, 1);
			StokesHelp(Hscreen);
			SetHelpScreen(Hscreen);
			REPEAT
				Accept;
				s1 := GetNumber(1);
				s2 := GetNumber(2);
				s3 := GetNumber(3);
				s0 := SQRT(SQR(s1) + SQR(s2) + SQR(s3));
				NoMistake := s0 > MachEps;
				IF NoMistake THEN
				BEGIN
					s1 := s1 / s0;
					s2 := s2 / s0;
					s3 := s3 / s0;
					s0 := 1
				END
				ELSE
					Message('At least one of s1, s2 and s3 must be nonzero!');
			UNTIL NoMistake OR Canceled;
			GetStokesParameters := NOT Canceled;
			done;
		END;
	END;

	FUNCTION GetAnimateData: boolean;
  (* Supply parameters that affect appearance, speed and duration. *)
		VAR
			Nf: INTEGER;
			Hscreen: HelpScrType;
			AnimateData: TinputScreen;
	BEGIN
		WITH AnimateData DO
		BEGIN
			init;
			DefineInputPort(0.15, 0.85, 0.2, 0.8);
			LoadLine(' USE THIS SCREEN TO SET PARAMETERS THAT AFFECT THE');
			LoadLine(' APPEARANCE, SPEED AND DURATION OF THE WAVE ANIMATION.');
			LoadLine('');
			LoadLine('  Total No. of frames in a sequence       = {      }');
			LoadLine('  Animation frames per oscillation        = {      }');
			LoadLine('  Viewpoint to origin distance            = {      }');
			LoadLine('  Number of wavelengths in animated train = {      }');
			LoadLine('  Show mouse cursor during animation : #T  (X=YES)');
			LoadLine('');
			LoadLine('          [  Ok  ]    [Cancel]    [ Help ]');
			nf := ROUND(2 * PI / dt);
			SetNumber(1, NFrames);
			SetNumberLimits(1, 1, 9999);
			SetNumber(2, nf);
			SetNumberLimits(2, 2, 100);
			SetNumber(3, ViewR);
			SetNumberLimits(3, 10, 1e6);
			SetNumber(4, NWaves);
			SetNumberLimits(4, 0.1, 20);
			SetBoolean(5, Showpointer);
			AnimateHelp(Hscreen);
			SetHelpScreen(Hscreen);
			Accept;
			Nframes := Round(GetNumber(1));
			dt := 2 * PI / GetNumber(2);
			ViewR := GetNumber(3);
			Nwaves := GetNumber(4);
			ShowPointer := GetBoolean(5);
			GetAnimateData := NOT Canceled;
			done;
		END;
	END;


	PROCEDURE MakePlotMenu;
  (* Set up the pull-down menu. *)
	BEGIN
		WITH PlotMenu DO
		BEGIN
			init;
			column(1, 'File');
			row(1, 1, 'About CUPS');
			row(1, 2, 'Title Screen');       {Credits}
			row(1, 3, 'About Program');
			row(1, 4, 'About Units');
			row(1, 5, 'Configuration');
			row(1, 6, '-------------');
			row(1, 7, 'Exit Program');
			rowactivate(1, 6, false);
			column(2, 'Show What');
			row(2, 1, 'Polarized Wave');
			row(2, 2, 'Wave on Interface');
			row(2, 3, 'Wave + Polaroid');
			row(2, 4, 'Wave + Waveplate');
			row(2, 5, 'Wave + Polaroid & Plate');
			row(2, 6, 'Wave + 2 Polaroids');
         rowchecked(2,1,true);
			Column(3, 'Help');
			row(3, 1, 'What to do');
			row(3, 2, 'Perspective');
			row(3, 3, 'Simulations');
			row(3, 4, 'Units');
			column(4, 'Change');
			row(4, 1, 'Stokes Parameters');
			row(4, 2, 'Viewing Data');
			row(4, 3, 'Back to Defaults');
			row(4, 4, 'to Circle Polarized');
			row(4, 5, 'to Plane Polarized');
			column(5, 'Control');
			row(5, 1, 'Start Over');
			row(5, 2, 'Resume');
		END;
	END; {MakePlotMenu}

	PROCEDURE SetHotKeys (N: WORD);
         (* Initial HotKeys Functions. *)
	BEGIN
		WITH HotKeys DO
		BEGIN
			init(N);
			key[1] := 'F1-Help';
			key[2] := 'F2-Faster';
			key[3] := 'F3-Slower';
			key[4] := 'F4-Step';
			IF N = 6 THEN
				key[5] := 'F5-Rotate';
			key[N] := 'F10-Menu';
		END;
	END; {SetHotKeys}


	PROCEDURE HandleMenu;
   var
      i : integer;
         (* Take action based on menu selections. *)
	BEGIN
		StartOver := TRUE;
		Frame.Pause;
		WITH PlotMenu DO
			CASE ColChosen OF
				1: 
				BEGIN
					CASE rowChosen OF
						1: 
							AboutCups;
						2: 
							ShowCredits;
						3: 
							AboutProgram;
						4: 
							AboutUnits;
						5: 
							Configuration;
						7: 
							AnimateOFF := TRUE;
					END;
					StartOver := FALSE;
					NewData := FALSE
				END;

				2: 
				BEGIN
					CASE rowChosen OF
						1:
							NewData := true; (* No optical elements *)
						2:
							NewData := MaterialData;
						3:
							NewData := PolarizerData;
						4:
							NewData := WaveplateData;
						5:
							NewData := PolarPlateData;
						6:
							NewData := TwoPolarData;
					END;
               if NewData then
               begin
					   ncase := rowchosen;
                  for i := 1 to 6 do rowchecked(2,i,false);
                  rowchecked(2,ncase,true);
					   IF ncase < 3 THEN
						   SetHotKeys(5)
					   ELSE
						   SetHotKeys(6);
               end;
				END;

				3:
					CASE rowchosen OF
						1: 
							WhatToDo;
						2: 
							PerspectiveHelp;
						3: 
							SimulateHelp;
						4: 
							UnitsHelp
					END;

				4: 
				BEGIN
					CASE rowchosen OF
						1: 
							GetStokesParameters; {Stokes}
						2: 
							GetAnimateData;{Viewing}
						3: 
							Defaults;
						4: 
						BEGIN
							s1 := 0;
							s2 := 0;
							s3 := 1
						END;
						5: 
						BEGIN
							s1 := 1;
							s2 := 0;
							s3 := 0
						END;
					END;
					NewData := FALSE;
				END;

				5: 
					CASE RowChosen OF
						1: 
						BEGIN
							StartOver := TRUE;
							NewData := FALSE
						END;
						2: 
						BEGIN
							StartOver := FALSE
						END;
					END;
			END;  {CASE}
	END; {HandleMenu Procedure}



	PROCEDURE HotKeysControl (key: BYTE);
         (* Results of pressing HotKeys. *)
		VAR
			Hscreen: HelpScrType;
	BEGIN
		CASE key OF
			1: 
			BEGIN  {Help}
				Frame.Pause;
				WhatToDo;
				Frame.Continue;
			END;

			2: 
				dt := dt * 1.414;

			3: 
				dt := dt / 1.414;

			4: 
				DynamicPause;

			5: 
			BEGIN
				IF ncase < 3 THEN
				BEGIN
					StartOver := TRUE;
					Frame.Pause;
					Frame.Close;
					ShowMouse;
					PlotMenu.ColChosen := 3;
					PlotMenu.RowChosen := 2;
					PlotMenu.Display;
					REPEAT
						CheckForEvents;
						IF PlotMenu.Activated THEN
							HandleMenu;
					UNTIL PlotMenu.ColChosen <> 3;
					CloseViewPort(4);
					SelectViewPort(1);
				END

				ELSE IF ncase = 3 THEN
					Polar1 := Polar1 + 5
				ELSE IF ncase = 4 THEN
					Plate1 := Plate1 + 5
				ELSE IF ncase = 5 THEN
					Polar2 := Polar2 + 5
				ELSE IF ncase = 6 THEN
					Polar4 := Polar4 + 5;
				IF (ncase = 3) AND (Polar1 > 359.9) THEN
					Polar1 := Polar1 - 360
				ELSE IF (ncase = 4) AND (Plate1 > 359.9) THEN
					Plate1 := Plate1 - 360
				ELSE IF (ncase = 5) AND (Polar2 > 359.9) THEN
					Polar2 := Polar2 - 360
				ELSE IF (ncase = 6) AND (Polar4 > 359.9) THEN
					Polar4 := Polar4 - 360;
			END;
			6: 
			BEGIN
				StartOver := TRUE;
				Frame.Pause;
				ShowMouse;
				PlotMenu.ColChosen := 3;
				PlotMenu.RowChosen := 2;
				PlotMenu.Display;
				REPEAT
					CheckForEvents;
					IF PlotMenu.chosen THEN
						HandleMenu;
				UNTIL PlotMenu.ColChosen <> 3;
				CloseViewPort(4);
				SelectViewPort(1);
				Frame.Continue;
			END;

		END; {CASE}
		TrigFunctions;
		Plotmenu.Display;
	END; {HotKeysControl}


	PROCEDURE GetWaveData (amplitude: REAL; VAR a1, a2, del, theta1, theta2: REAL);
  (* Decodes Stokes parameters to get amplitudes and phases,}
{
    and converts polarizer angles to radian measure. *)
		CONST
			DegtoRad = (PI / 180);

	BEGIN
		a1 := amplitude * SQRT((s0 + s1) / 2.0);   {Interpret Stokes Parameters}
		a2 := amplitude * SQRT((s0 - s1) / 2.0);

		IF (SQR(s2) + SQR(s3)) > Macheps THEN
			del := ArcTan2(s2, s3)
		ELSE
			del := 0.0;

		CASE ncase OF
			3:
				Theta1 := polar1 * DegToRad;
			4: 
				Theta2 := plate1 * DegToRad;
			5: 
			BEGIN
				Theta1 := polar2 * DegToRad;
				Theta2 := plate2 * DegToRad
			END;
			6: 
			BEGIN
				Theta1 := polar3 * DegToRad;
				Theta2 := polar4 * DegToRad
			END;
		END; {Case}

		kvac := 2 * PI * Nwaves / 10;  {Wavelength = 10/Nwaves = 2*pi/kvac}
		IF (ViewR < 8) THEN
			ViewR := 8;
		P := ViewR;               {Viewer to Screen in cm}
		TrigFunctions;
	END;


	PROCEDURE MakeWaves;
  {Master procedure for wave animation.}
		CONST
			DegtoRad = (Pi / 180);
			TwoPi = 2 * Pi;
			Increment = Pi / 36;  {Rotations by 5-degree increments.}
		VAR
			t: REAL; {Time of frame}
			a1, a2, del: REAL; {Inc. component amplitudes & phase diff.}
			a1t, a2t, delt: REAL; {Transmitted amplitudes and phase diff.}
			a1r, a2r: REAL; {transmitted reflected amplitudes}
			Rc, Tc: REAL; {amplitude coefficients}
			Rphase, Tphase: REAL; {phase changes on reflection, transmission}
			Bphase: REAL; {phase of B w.r.t. E in medium}
			v, alpha, beta: REAL; {speed, real and imag. parts of k}
			theta1, theta2: REAL; {polarizer angles}
			ix, iy: Integer;
			GoToMenu: BOOLEAN;
			key: BYTE;
			InKey: WORD;
			cycle, ncycles: INTEGER;
			iframe: INTEGER;  {frame index}
	BEGIN
 {      CUPSdone;   Reinitialize the MUPPET port}
		Frame.Init;   {Use screen-flipping animation technique.}
		CUPSinit;
		REPEAT
			SetupGraph;
			CloseViewPort(4);
			SelectViewPort(1);
			GetWaveData(4.0, a1, a2, del, theta1, theta2);  {Incident wave amplitudes and angles}
			delt := del;
         case nCase of
          1 : ComputeCoeff(a1, a2, 1, 1, 0, a1r, a2r, a1t, a2t, Rphase,
                           Tphase, Bphase, alpha, beta, v);          {Component amplitudes}
          2 : ComputeCoeff(a1, a2, epsilon, mu, sigma, a1r, a2r, a1t,
                           a2t, Rphase, Tphase, Bphase, alpha, beta, v);          {Component amplitudes}
          3 : Theta1 := polar1 * DegToRad;
			 4 : Theta2 := plate1 * DegToRad;
			 5 : begin
                 Theta1 := polar2 * DegToRad;
                 Theta2 := plate2 * DegtoRad
              end;
			 6 : begin
                 Theta1 := polar3 * DegTORad;
                 Theta2 := polar4 * DegToRad;
              end;
         end;

			IF ncase>=3 THEN
			BEGIN
				DoPolarizers(a1, a2, del, theta1, theta2, a1t, a2t, delt);
				a1r := 0;
				a2r := 0;
				Bphase := 0;
				v := 1;
				Rphase := 0;
				Tphase := 0;
				alpha := 0;
				beta := 1;
				HotKeys.key[5] := 'F5-Rotate';
			END;
			PlotMenu.Display;
			HotKeys.Display; {Show HotKey menu.}
			ShowInformation;
			SelectViewPort(1);
			ShowMouse;
			Frame.Open(1);   {Existing screen is animation template.}
			Frame.SwitchandClean;
			ncycles := ROUND(Nframes / 2);
			t := 0;
			GoToMenu := FALSE;
			IF NOT ShowPointer THEN
				HideMouse;
			FOR iframe := 1 TO ncycles DO
			BEGIN
				StartOver := False;
				AnimateOff := False;
				NewData := TRUE;
				MouseBorders(0, 325, 639, 340);
                                         {ShowMouse ;}
				ShowInformation;
				t := t + dt;

				FieldPlot(t, a1, a2, del, theta1, theta2, a1r, a2r, Rphase, a1t, a2t, delt, Tphase, Bphase, v, alpha, beta);

				SetColor(LightRed);
				PutLabel(Inside, '              B is Red');
				SetColor(Yellow);
				PutLabel(Inside, 'E is Yellow           ');
				HotKeys.Clear;
				HotKeys.Display;
				IF DoCredits AND (iframe = 15) THEN
				BEGIN
					Frame.pause;
					ShowCredits;
					Frame.Continue;
					DoCredits := FALSE;
				END;
				Frame.SwitchAndClean;
				IF EscapeFromThis THEN
					GoToMenu := TRUE;
				IF HotKeys.Pressed(key) THEN
				BEGIN
					MouseBorders(0, 0, 639, 340);
					IF (((ncase < 3) AND (key = 5)) OR ((ncase > 2) AND (key = 6))) THEN
						GoToMenu := TRUE
					ELSE
						HotKeysControl(key);
               case nCase of
                3 : Theta1 := polar1 * DegToRad;
					 4 : Theta2 := plate1 * DegToRad;
					 5 : begin
                       Theta1 := polar2 * DegToRad;
                       Theta2 := plate2 * DegtoRad
                    end;
					 6 : begin
                       Theta1 := polar3 * DegTORad;
                       Theta2 := polar4 * DegToRad;
                    end;
               end;
					IF ncase > 2 THEN
						DoPolarizers(a1, a2, del, theta1, theta2, a1t, a2t, delt);
				END;

				IF GoToMenu AND NOT Frame.Firstpage THEN
				BEGIN
					MouseBorders(0, 0, 639, 340);
					HotKeysControl(6);
					Theta1 := 0;
					Theta2 := 0;
					GoToMenu := False;
				END;
				IF Event.ExtendedKey THEN
				BEGIN
					InKey := ord(Event.ReadKey);
					IF InKey = 72 THEN
						Alt := Alt + Increment
					ELSE IF InKey = 80 THEN
						Alt := Alt - Increment
					ELSE IF InKey = 77 THEN
						Az := Az + Increment
					ELSE IF InKey = 75 THEN
						Az := Az - Increment;
					TrigFunctions;
				END;
				IF (StartOver OR AnimateOff) THEN
					iframe := ncycles;
			END;
			CloseViewPort(4);
		UNTIL AnimateOff;
	END; {MakeWaves}


  { -------------------------   Main Program  -------------------------------}

BEGIN
	Defaults;
	SetHotKeys(5);
	MakePlotMenu;
	DoCredits := TRUE;
	ncase := 1;
	MakeWaves;
	PlotMenu.done;
	CupsDone;
END. {EMWAVE Program}