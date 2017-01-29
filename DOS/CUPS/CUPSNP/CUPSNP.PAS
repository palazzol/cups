           (*************************************************)
           (*************************************************)
           (**                                             **)
           (**  Consortium of Upper-level Physics Software **)
           (**              ( CUPS Project )               **)
           (**      ( c) 1994 by John Wiley & Sons         **)
           (**  Compiled with Utilities Ver. 1.7 (95/06/27)**)
           (**                                             **)
           (*************************************************)
           (*************************************************)

                   {************************************}
                   {************************************}
                   {**       Program CUPSnp.Pas       **}
                   {**   Written by Jarek Tuszynski   **}
                   {**        for CUPS Project        **}
                   {**            May 1995            **}
                   {************************************}
                   {************************************}

{$M 32768, 0, 65536}
PROGRAM NP_Directory;
USES DOS, GRAPH, CUPSMupp, CUPS, CUPSgui;

PROCEDURE TriplexFontProc;  EXTERNAL; {$L Trip.OBJ }

VAR
  m,n : integer;
  ExitRequest : boolean;

FUNCTION Nuclear : integer;
VAR
   Menu : TBarMenu;
BEGIN
   if n>2 then n:=1;
   With Menu do
   begin
      init;
      Row[1] := 'Nuclear Properties';
      Row[2] := 'Nuclear Decays';
      Row[3] := 'Main Menu';
      draw(0.54,0.62,3,n);
   end;
   n := Menu.Chosen;
   DefineViewPort(1,0.54,1,0,0.7);
   CloseViewPort(1);
   Nuclear := n mod 3;
END;

FUNCTION DetSim : integer;
VAR
   Menu : TBarMenu;
BEGIN
   if n>2 then n:=1;
   With Menu do
   begin
      init;
      Row[1] := 'Detector Simulation';
      Row[2] := 'Sample Distribution';
      Row[3] := 'Main Menu';
      draw(0.54,0.495,3,n);
   end;
   n := Menu.Chosen;
   DefineViewPort(1,0.54,1,0,0.7);
   CloseViewPort(1);
   DetSim := (n + 3) mod 6;
END;

FUNCTION ProgramChosen : integer;
VAR
   BarMenu : TBarMenu;
   k : integer;
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
   SetUserCharSize(11,6,1,1);
   print(2,4,'Nuclear and Particle');
   print(2,6,'Physics Simulations');
   print(3,2,'       CUPS');
   With BarMenu do
   begin
     Row[1] := 'Nuclear Properties and Decays  '+chr(16);
     Row[2] := 'Relativistic Kinematics';
     Row[3] := 'Detector Simulation            '+chr(16);
     Row[4] := 'Radiation Interaction in Matter';
     Row[5] := 'Nuclear Shell Model';
     Row[6] := 'Two Nucleon Problem';
     Row[7] := 'Electron Nucleus Scattering';
     Row[8] := 'Exit Program';
     draw(0.09,0.62,8,m);
   end;
   m := BarMenu.Chosen;
   case m of
    1: k := Nuclear;
    2: k := 3;
    3: k := DetSim;
    else k:=m+2;
   end;
   ProgramChosen := k;
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

procedure Run(dir,prog : string; num : integer);
var 
   i : integer;
   badDir : boolean;
begin
   ChDir(dir);
   SwapVectors;
   Exec(prog+'.exe', '');
   SwapVectors;
   BadDir := (DosError=2);
   if DosError<>0 then 
   begin
      writeln(' Dos Error #',DosError);
      writeln(' Press <Enter> to Continue');
      readln;
   end;
   if not BadDir then
    for i := 1 to num do ChDir('..');
end;


BEGIN
   m := 1;
   ExitRequest := false;
   ShowCursor;
   REPEAT
      CASE ProgramChosen of
       01: Run('Nuclear' ,'Nuclear', 1);
       02: Run('Nuclear' ,'Decay',   1);
       03: Run('RelKin'  ,'RelKin',  1);
       04: Run('DetSim'  ,'DetSim',  1);
       05: Run('DetSim\Examples'  ,'trfitmen',2);
       06: Run('NucRad'  ,'NucRad',  1);
       07: Run('ShellMod','ShellMod',1);
       08: Run('TwoBody' ,'TwoBody', 1);
       09: Run('ElScatt' ,'ElScatt', 1);
       10: ExitRequest := true;
      END;
   UNTIL ExitRequest;
END.