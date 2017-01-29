           (*************************************************)
           (*************************************************)
           (**                                             **)
           (**  Consortium of Upper-level Physics Software **)
           (**              ( CUPS Project )               **)
           (**      ( c) 1994 by John Wiley & Sons         **)
           (**  Compiled with Utilities Ver. 1.6 (95/05/17)**)
           (**                                             **)
           (*************************************************)
           (*************************************************)

                   {************************************}
                   {************************************}
                   {**       Program CUPStp.Pas       **}
                   {**   Written by Jarek Tuszynski   **}
                   {**        for CUPS Project        **}
                   {**            May 1995            **}
                   {************************************}
                   {************************************}

{$M 32768, 0, 65536}
PROGRAM TP_Directory;
USES DOS, GRAPH, CUPSMupp, CUPS, CUPSgui;

PROCEDURE TriplexFontProc;  EXTERNAL; {$L Trip.OBJ }

VAR
   ExitRequest,second : Boolean;
   n,m : integer;

procedure AboutProbab;
var screen : HelpScrType;
begin
    screen[01] := '                                                 ';
    screen[02] := '  The Introduction to Probability and            ';
    screen[03] := '  Statistics Programs are:                       ';
    screen[04] := '                                                 ';
    screen[05] := '  The Galton Board which illustrates the         ';
    screen[06] := '  Binomial, the Normal, and System Unique        ';
    screen[07] := '  Distributions, and the Central Limit Theorem,  ';
    screen[08] := '                                                 ';
    screen[09] := '  The PoisExp Program which illustrates the      ';
    screen[10] := '  Poisson and Exponential Distributions,         ';
    screen[11] := '                                                 ';
    screen[12] := '  A Random Walk in Two Dimensions which shows    ';
    screen[13] := '  the paths of a random walk confined either     ';
    screen[14] := '  to a grid or a plane, and the Rayleigh Dist.   ';
    screen[15] := '                                                 ';
    screen[16] := '  The Kac Ring which gives an example of the     ';
    screen[17] := '  difference between the solutions of the        ';
    screen[18] := '  equations of motion for a system and its       ';
    screen[19] := '  statistical model,                             ';
    screen[20] := '                                                 ';
    screen[21] := '  The Stadium Model which shows that even if     ';
    screen[22] := '  the equations of motions are solvable,         ';
    screen[23] := '  statistical methods may be needed because of   ';
    screen[24] := '  the chaotic nature of the system.              ';
    screen[25] := '                                                 ';
    help(screen);
end;

FUNCTION Probab : integer;
VAR
   Menu : TBarMenu;
BEGIN
   if n>6 then n:=1;
   With Menu do
   begin
      init;
      Row[1] := 'About Programs';
      Row[2] := 'Galton Board';
      Row[3] := 'The Poisson and Exp. Distrib.';
      Row[4] := 'Random Walks in Two-Dim.';
      Row[5] := 'The Kac Ring';
      Row[6] := 'The Stadium Model';
      Row[7] := 'Main Menu';
      draw(0.54,0.474,7,n-4);
   end;
   repeat
      n := Menu.Chosen;
      if n=1 then AboutProbab;
   until n<>1;
   n := n+4;
   DefineViewPort(1,0.54,1,0,0.7);
   CloseViewPort(1);
   Probab := n mod 11;
END;

procedure aboutEngines;
var screen : HelpScrType;
begin
    screen[01] := '                                                 ';
    screen[02] := '                                                 ';
    screen[03] := '  The Engine Programs are:                       ';
    screen[04] := '                                                 ';
    screen[05] := '  The Design Your Own Engine in which the user   ';
    screen[06] := '  creates a gas engine by selecting:             ';
    screen[07] := '                                                 ';
    screen[08] := '   the engine type (reversible or irreversible), ';
    screen[09] := '   the gas (Helium,Argon,Nitrogen or Steam),     ';
    screen[10] := '   the Initial Temperature and Volume,           ';
    screen[11] := '   the percent of heat loss (in the case of an   ';
    screen[12] := '       irreversible engine),                     ';
    screen[13] := '   a series of processes (Adiabatic, Isobaric,   ';
    screen[14] := '       Isochoric and Isothermic) and the         ';
    screen[15] := '       final conditions associated with each     ';
    screen[16] := '       process.                                  ';
    screen[17] := '                                                 ';
    screen[18] := '  Animated Examples of Gase Engines:             ';
    screen[19] := '                                                 ';
    screen[20] := '   a four stroke Otto (gasoline) engine,         ';
    screen[21] := '   a Diesel engine, and                          ';
    screen[22] := '   a Wankel (Rotary) engine.                     ';
    screen[23] := '                                                 ';
    screen[24] := '                                                 ';
    screen[25] := '                                                 ';
    help(screen);
end;

FUNCTION Engines : integer;
VAR
   Menu : TBarMenu;
BEGIN
   if n>5 then n:=1;
   With Menu do
   begin
      init;
      Row[1] := 'About Engines';
      Row[2] := 'Design Your Own Engine';
      Row[3] := 'The Diesel Engine';
      Row[4] := 'The Otto Engine';
      Row[5] := 'The Wankel Engine';
      Row[6] := 'Main Menu';
      draw(0.54,0.537,6,n);
   end;
   repeat
      n := Menu.Chosen;
      if n=1 then AboutEngines;
   until n<>1;
   DefineViewPort(1,0.54,1,0,0.7);
   CloseViewPort(1);
   Engines := n mod 6;
END;

FUNCTION QMGas : integer;
VAR
   Menu : TBarMenu;
BEGIN
   if n>2 then n:=1;
   With Menu do
   begin
      init;
      Row[1] := 'Thermodynamic Properties';
      Row[2] := 'Monte Carlo Simulation';
      Row[3] := 'Main Menu';
      draw(0.54,0.35,3,n-11);
   end;
   n := Menu.Chosen+11;
   DefineViewPort(1,0.54,1,0,0.7);
   CloseViewPort(1);
   QmGas := n mod 14;
END;

FUNCTION ProgramChosen : integer;
VAR
   BarMenu : TBarMenu;
BEGIN
   CUPSInit;
   If (RegisterBGIfont(@TriplexFontProc) < 0) Then
   begin
     Writeln('Ega/Vga: ', GraphErrorMsg(GraphResult));
     Pause; Halt;
   end;
   BarMenu.Init;
   ClearMUPPETPort;
   SetTextStyle(TriplexFont,HorizDir,5);
   SetUserCharSize(2,1,1,1);
   print(3,4,'  Thermal Physics');
   print(3,6,'    Simulations');
   print(3,2,'       CUPS');
   repeat
       With BarMenu do
       begin
         Row[1] := 'Phase Diagrams of Fluids         ';
         Row[2] := 'Engines                              '+chr(16);
         Row[3] := 'Probability and Statistics           '+chr(16);
         Row[4] := 'Simulation of Liquids, Solids and Gases';
         Row[5] := 'Quantum Ideal Gas                    '+chr(16);
         Row[6] := 'Ising Model and Critical Phenomena';
         Row[7] := 'Exit Program';
         draw(0.01,0.6,7,m);
       end;
       if not ((m in [2,3,5] ) and (n<>0)) then m := BarMenu.Chosen;
       case m of
        1: n := 1;
        2: n := Engines;
        3: n := Probab;
        4: n := 11;
        5: n := QmGas;
        6,7: n := m +8;
       end;
   until n<>0;
   ProgramChosen := n;
   ClearMUPPETPort;
   CloseGraph;
   CUPSDone;
END;

procedure chdir(dir : string);
var
   Regs: registers;
begin
   dir := dir + chr(0);
   Regs.AH := $3b;
   Regs.DS := seg(dir[1]);
   Regs.DX := ofs(dir[1]);
   MsDos(Regs);
end;

procedure Run(dir,prog : string);
begin
   ChDir(dir);
   SwapVectors;
   Exec(prog+'.exe', '');
   SwapVectors;
   if DosError<>0 then writeln('Dos Error #',DosError);
   ChDir('..');
end;


BEGIN
   ExitRequest := False;
   second := false;
   n := 0;
   m := 1;
   ShowCursor;
   REPEAT
      CASE ProgramChosen of
       01: Run('Fluid'   ,'Fluid');
       02: Run('Engines' ,'Engine');
       03: Run('Engines' ,'Diesel');
       04: Run('Engines' ,'Otto');
       05: Run('Engines' ,'Wankel');
       06: Run('Probab'  ,'Galton');
       07: Run('Probab'  ,'PoisExp');
       08: Run('Probab'  ,'TwoD');
       09: Run('Probab'  ,'Kac');
       10: Run('Probab'  ,'Stadium');
       11: Run('ManyPart','ManyPart');
       12: Run('QMGas',   'QmGas1');
       13: Run('QMGas',   'QmGas2');
       14: Run('Ising',   'Ising');
       15: ExitRequest := true;
      END;
   UNTIL ExitRequest;
END.