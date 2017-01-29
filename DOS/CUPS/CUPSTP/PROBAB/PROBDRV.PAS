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
PROGRAM probdrv;

	{*************************************************}
	{*************************************************}
	{**	  Program Probability & Statistics      **}
	{**						**}
	{** This program is the driver for the      	**}
	{** Introduction to Probability and Statistics  **}
	{** Programs.                                   **}
	{**                                         	**}
	{** The Probability programs are:           	**}
	{**                                             **}
	{** The Galton Board which models balls falling **}
	{** down either a traditional Galton Board or   **}
	{** a Cutomized Galton Board with traps,        **}
	{** reflecting walls and/or absorbing walls.	**}
	{** This program demonstrates the Binomial 	**}
	{** and Normal Distributions and the Central  	**}
	{** Limit Theorem.                              **}
	{**                             		**}
	{** The Poisson Distribution and Exponential    **}
	{** Distribution which are demonstrated by      **}
	{** observing the decay of radioactive nuclei.  **}
	{**                        			**}
	{** A Random Walk in Two Dimensions which shows **}
	{** the paths of random walks on either a grid  **}
	{** or in random directions.  The program       **}
	{** demonstrates the joint probability of two   **}
	{** independent processes.                      **}
	{**                                             **}
	{** The Statistics Programs are:         	**}
	{**                                             **}
	{** The Kac Ring which demonstrates that large  **}
	{** mechanical systems, which obey time reversal**}
	{** and have a Poincare Cycle, can also be      **}
	{** described by Statistical Models.            **}
	{**                                             **}
	{** The Stadium Model which demonstrates that   **}
	{** there exists Mechanical Systems whose       **}
	{** Equations of Motion are solvable but whose  **}
	{** motion is not predictable because of its    **}
	{** Chaotic Nature.                             **)
	{**						**}
	{**	    Written by L. B. Spornick		**}
	{**	    Applied Physics Laboratory		**}
	{**	    The Johns Hopkins University	**}
	{**						**}
	{**		  March 8, 1992                 **}
	{**		  Ver 1.0, July, 1993           **}
	{**		  Ver 1.1, May, 1994            **}
	{**						**}
	{**						**}
	{*************************************************}
	{*************************************************}


 USES DOS, GRAPH, CUPS, CUPSMUPP, CUPSgui;
 CONST
         aboutprogram : HelpScrType =
   ('*************************************************',
    '* The Introduction to Probability and           *',
    '* Statistics Programs are:                      *',
    '*                                               *',
    '* The Galton Board which illustrates the        *',
    '* Binomial, the Normal, and System Unique       *',
    '* Distributions, and the Central Limit Theorem, *',
    '*                                               *',
    '* The PoisExp Program which illustrates the     *',
    '* Poisson and Exponential Distributions,        *',
    '*                                               *',
    '* A Random Walk in Two Dimensions which shows   *',
    '* the paths of a random walk confined either    *',
    '* to a grid or a plane, and the Rayleigh Dist.  *',
    '*                                               *',
    '* The Kac Ring which gives an example of the    *',
    '* difference between the solutions of the       *',
    '* equations of motion for a system and its      *',
    '* statistical model,                            *',
    '*                                               *',
    '* The Stadium Model which shows that even if    *',
    '* the equations of motions are solvable,        *',
    '* statistical methods may be needed because of  *',
    '* the chaotic nature of the system.             *',
    '*************************************************');
         credits : HelpScrType =
   ('*************************************************',
    '*  Introduction to Probability and Statistics   *',
    '*                L.B. Spornick                  *',
    '*      APL, The Johns Hopkins University        *',
    '*            Version 1.1, May, 1994             *',
    '*             Copyright (c), 1994               *',
    '* Introduction to Probability and Statistics    *',
    '* provides examples of several important        *',
    '* probability distribution functions (the       *',
    '* Binomial, the Normal, and the Rayleigh),      *',
    '* an introduction to probability theory,        *',
    '* the Central Limit Theorem, and a comparision  *',
    '* between statistical models and physical syst. *',
    '* There are five programs:                      *',
    '* The Galton Board (the Binomial, Normal, and   *',
    '* System Unique Distributions, and the Central  *',
    '* Limit Theorem),                               *',
    '* The PoisExp Program (the Poisson and the      *',
    '* Exponential Distributions),                   *',
    '* Random Walks in Two Dimensions (the Binomial  *',
    '* and Rayleigh Distributions),                  *',
    '* The Kac Ring (physical systems vs statistical *',
    '* models), and                                  *',
    '* The Stadium Model (physical systems and chaos).',
    '*************************************************');

 VAR
  ExitRequest : Boolean;
  n : Integer;
   { Input Screen }
   FUNCTION ProgramChosen : Integer;
    VAR BarMenu : TBarMenu;
    TextInfo : TextSettingsType;
    OFont,ODir,OCharSize,Horiz,Vert : WORD;
     BEGIN
      CUPSInit;
      BarMenu.Init;
      ClearMUPPETPort;
      GetTextSettings(TextInfo);
      WITH TextInfo DO
       BEGIN
        OFont := TextInfo.Font;
        ODir := TextInfo.Direction;
        OCharSize := 2*TextInfo.CharSize;
        Horiz := TextInfo.Horiz;
        Vert := TextInfo.Vert;
       END;
      SetTextStyle(OFont,ODir,OCharSize);
      OutTextXY(220,25,'Introduction To');
      OutTextXY(140,50,'Probability and Statistics');
      SetTextStyle(OFont,ODir,OCharSize  div 3);
      REPEAT  { Until n > 2 }
       WITH BarMenu DO
        BEGIN
         Row[1] := 'About CUPS ';
         Row[2] := 'About Introduction to Probability and Statistics';
         Row[3] :=
          'Galton Board - Binomial and Normal Dist, Central Limit Thrm';
         Row[4] := 'The Poisson and Exponential Distribution   ';
         Row[5] := 'Random Walks in Two Dimensions             ';
         Row[6] := 'The Kac Ring                ';
         Row[7] := 'The Stadium Model                ';
         Row[8] := 'Exit the Program                   ';
         Draw(0.2,0.8,8,n);
        END;  { WITH BarMenu DO }
       n := BarMenu.chosen;
       IF (n = 1) THEN aboutCUPS;
       IF (n = 2) THEN help(aboutprogram);
     UNTIL (n > 2);
    ProgramChosen := n;
    ClearMUPPETPort;
    CloseGraph;
    CUPSDone;
   END;  { FUNCTION ProgramChosen }

  BEGIN
   CUPSInit;
   help(credits);
   CUPSDone;
   ExitRequest := false;
   n := 1;
   ShowCursor;
   REPEAT
    CASE ProgramChosen OF
     3: BEGIN SwapVectors; Exec('Galton.EXE', ''); SwapVectors; END;
     4: BEGIN SwapVectors; Exec('PoisExp.EXE', ''); SwapVectors; END;
     5: BEGIN SwapVectors; Exec('TwoD.EXE', ''); SwapVectors; END;
     6: BEGIN SwapVectors; Exec('Kac.EXE' , ''); SwapVectors; END;
     7: BEGIN SwapVectors; Exec('Stadium.EXE' , ''); SwapVectors; END;
     8: ExitRequest := True;
    END;  { CASE }
   UNTIL ExitRequest;
 END.