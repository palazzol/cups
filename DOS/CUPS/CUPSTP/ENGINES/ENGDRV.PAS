           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.6 (95/05/17) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

{$M 16384, 0, 65000}
PROGRAM Engdrv;

	{*************************************************}
	{*************************************************}
	{**	         Program Engine                   **}
	{**					             	**}
	{** This programs is the driver for the      	**}
	{** Engine Programs.                            **}
	{**                                         	**}
	{** The Engine programs are:               	**}
	{**                                             **}
	{** The Design Your Own Engine in which the     **}
	{** user creates an gas engine by selecting:    **}
      {**  the engine type (reversible or irreversible**}
	{**  the gas (Helium, Argon, Nitrogen or Steam),**}
      {**  the Initial Temperature and Volume,        **}
      {**  the percent of heat loss (in the case of an**}
      {**    irreversible engine) and                 **}
      {**  a series of processes (Adiabatic, Isobaric,**}
	{**    Isochoric or Isothermic) and the final   **}
	{**    condition for either T, V or P for that  **}
	{**    process.                                 **}
	{**                           			**}
	{** Examples of Gas Engines:                    **}
	{**  the Otto Engine - a Four Stroke Internal   **}
	{**   Combustion Engine                         **}
	{**  a Diesel Engine                            **}
	{**  the Wankel Engine                          **}
	{**                                             **}
	{**	    Written by L. B. Spornick	      	**}
	{**	    Applied Physics Laboratory		**}
	{**	    The Johns Hopkins University	      **}
	{**						            **}
	{**		  April 24, 1992                    **}
	{**		  Ver 1, July, 1993                 **}
	{**		  Ver 1.1, May, 1994                **}
	{**						            **}
	{**						            **}
	{*************************************************}
	{*************************************************}

 USES DOS, GRAPH, CUPS, CUPSMUPP, CUPSgui;

procedure aboutprogram;
var screen : HelpScrType;
begin
    screen[01] := '*************************************************';
    screen[02] := '*                                               *';
    screen[03] := '* The Engine Programs are:                      *';
    screen[04] := '*                                               *';
    screen[05] := '* The Design Your Own Engine in which the user  *';
    screen[06] := '* creates a gas engine by selecting:            *';
    screen[07] := '*                                               *';
    screen[08] := '*  the engine type (reversible or irreversible),*';
    screen[09] := '*  the gas (Helium,Argon,Nitrogen or Steam),    *';
    screen[10] := '*  the Initial Temperature and Volume,          *';
    screen[11] := '*  the percent of heat loss (in the case of an  *';
    screen[12] := '*      irreversible engine),                    *';
    screen[13] := '*  a series of processes (Adiabatic, Isobaric,  *';
    screen[14] := '*      Isochoric and Isothermic) and the        *';
    screen[15] := '*      final conditions associated with each    *';
    screen[16] := '*      process.                                 *';
    screen[17] := '*                                               *';
    screen[18] := '* Animated Examples of Gase Engines:            *';
    screen[19] := '*                                               *';
    screen[20] := '*  a four stroke Otto (gasoline) engine,        *';
    screen[21] := '*  a Diesel engine, and                         *';
    screen[22] := '*  a Wankel (Rotary) engine.                    *';
    screen[23] := '*                                               *';
    screen[24] := '*                                               *';
    screen[25] := '*************************************************';
    help(screen);
end;

procedure credits;
var screen : HelpScrType;
begin
    screen[01] := '*************************************************';
    screen[02] := '*                 Engines                       *';
    screen[03] := '*  L.B. Spornick, APL, The Johns Hopkins Univ.  *';
    screen[04] := '*            Version 1.1, May, 1994             *';
    screen[05] := '*              Copyright (c) 1994               *';
    screen[06] := '*                                               *';
    screen[07] := '*  Engines provides an introduction to the      *';
    screen[08] := '*  thermodynamics of Engines.  There are four   *';
    screen[09] := '*  programs: Design Your Own Engine, the Diesel *';
    screen[10] := '*  Engine, the Otto Engine, and the Wankel Eng. *';
    screen[11] := '*                                               *';
    screen[12] := '*  The program Design Your Own Engine lets the  *';
    screen[13] := '*  user create an engine by specifying the      *';
    screen[14] := '*  processes (adiabatic, isobaric, isochoric,   *';
    screen[15] := '*  and isothermic) in the cycle of the engine,  *';
    screen[16] := '*  the engine type (reversible or irreversible),*';
    screen[17] := '*  and the gas (Helium, Argon, Nitrogen or      *';
    screen[18] := '*  Steam).  The thermodynamic properites of     *';
    screen[19] := '*  each process (heat exchanged, work done,     *';
    screen[10] := '*  and change in Internal Energy) and the       *';
    screen[11] := '*  efficiency of the engine are computed.       *';
    screen[12] := '*                                               *';
    screen[13] := '*  The Diesel, Otto, and the Wankel Engines     *';
    screen[14] := '*  animate these engines.                       *';
    screen[15] := '*************************************************';
    help(screen);
end;

   VAR
      ExitRequest: Boolean;
      n: Integer;
    { Input Screen }
   FUNCTION ProgramChosen: Integer;
      VAR
         BarMenu: TBarMenu;
         TextInfo: TextSettingsType;
         OFont, ODir, OCharSize, Horiz, Vert: WORD;
   BEGIN
      CUPSInit;
      GetTextSettings(TextInfo);
      BarMenu.Init;
      ClearMUPPETPort;
      WITH TextInfo DO
      BEGIN
         OFont := TextInfo.Font;
         ODir := TextInfo.Direction;
         OCharSize := 2 * TextInfo.CharSize;
         Horiz := TextInfo.Horiz;
         Vert := TextInfo.Vert;
      END;
      SetTextStyle(OFont, ODir, OCharSize);
      OutTextXY(250, 50, 'ENGINES');
      SetTextStyle(OFont, ODir, OCharSize DIV 3);
      REPEAT  { Until n > 2 }
         WITH BarMenu DO
         BEGIN
            Row[1] := 'About CUPS ';
            Row[2] := 'About Engines';
            Row[3] := 'Design Your Own Engine.                  ';
            Row[4] := 'The Diesel Engine              ';
            Row[5] := 'The Otto Engine.             ';
            Row[6] := 'The Wankel Engine.                 ';
            Row[7] := 'Exit the Program                   ';
            Draw(0.2, 0.8, 7, n);
         END;  { WITH BarMenu DO }
         n := BarMenu.chosen;
         IF (n = 1) THEN aboutCUPS;
         IF (n = 2) THEN aboutprogram;
      UNTIL (n > 2);
      ProgramChosen := n;
      ClearMUPPETPort;
      CloseGraph;
      CUPSDone;
   END;  { FUNCTION ProgramChosen }

BEGIN
   CUPSInit;
   credits;
   CUPSDone;
   ExitRequest := false;
   n := 1;
   ShowCursor;
   REPEAT
      CASE ProgramChosen OF
         3:
         BEGIN
            SwapVectors;
            Exec('Engine.EXE', '');
            SwapVectors;
         END;
         4:
         BEGIN
            SwapVectors;
            Exec('Diesel.EXE', '');
            SwapVectors;
         END;
         5:
         BEGIN
            SwapVectors;
            Exec('Otto.EXE', '');
            SwapVectors;
         END;
         6:
         BEGIN
            SwapVectors;
            Exec('Wankel.EXE', '');
            SwapVectors;
         END;
         7:
            ExitRequest := True;
      END;  { CASE }
   UNTIL ExitRequest;
END.