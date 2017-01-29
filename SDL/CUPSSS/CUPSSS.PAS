           (*************************************************)
           (*************************************************)
           (**                                             **)
           (**  Consortium of Upper-level Physics Software **)
           (**              ( CUPS Project )               **)
           (**      ( c) 1994 by John Wiley & Sons         **)
           (**  Compiled with Utilities Ver. 2.0 (95/10/10)**)
           (**                                             **)
           (*************************************************)
           (*************************************************)

                 {************************************}
                 {************************************}
                 {**       Program CUPSSS.Pas       **}
                 {**   Written by Jarek Tuszynski   **}
                 {**        for CUPS Project        **}
                 {**         September 1995         **}
                 {************************************}
                 {************************************}

{$m 32768, 0, 65536}
PROGRAM SS_Directory;
USES DOS, GRAPH, CUPSMupp, CUPS, CUPSgui;

PROCEDURE TriplexFontProc;  EXTERNAL; {$L Trip.OBJ }

VAR
  n : integer;
  ExitRequest : boolean;


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
   print(7,4,'Solid State Physics');
   print(7,6,'   Simulations');
   print(3,2,'       CUPS');
   With BarMenu do
   begin
     Row[1] := 'Phonon Dispersion Curves and The Density of States';
     Row[2] := 'Lattice Specific Heat of Solids';
     Row[3] := 'Electron States in a One-Dimensional Lattice';
     Row[4] := 'Energy Bands in the Infinite 1-D Lattice';
     Row[5] := 'Forced Motion of a Wave Packet in a 1-D Lattice';
     Row[6] := 'Inhomogeneous Semiconductor Laboratory';
     Row[7] := 'LCAO Workbench';
     Row[8] := 'Exit Program';
     draw(0.15,0.62,8,n);
   end;
   n := BarMenu.Chosen;
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

procedure Run(prog : string);
begin
   ChDir(prog);
   SwapVectors;
   Exec(prog+'.exe', '');
   SwapVectors;
   if DosError<>0 then
   begin
      writeln('Dos Error #',DosError);
      Writeln('Press <Enter>');
      readln;
   end;
   ChDir('..');
end;


BEGIN
   n := 1;
   ExitRequest := false;
   ShowCursor;
   REPEAT
      CASE ProgramChosen of
       1: Run('Phonons');
       2: Run('spheat');
       3: Run('lattice');
       4: Run('bands');
       5: Run('packet');
       6: Run('solidlab');
       7: Run('lcao');
       8: ExitRequest := true;
      END;
   UNTIL ExitRequest;
END.