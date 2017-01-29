           (*************************************************)
           (*************************************************)
           (**                                             **)
           (**  Consortium of Upper-level Physics Software **)
           (**              ( CUPS Project )               **)
           (**      ( c) 1994 by John Wiley & Sons         **)
           (**  Compiled with Utilities Ver. 1.9 (95/09/10)**)
           (**                                             **)
           (*************************************************)
           (*************************************************)

                   {************************************}
                   {************************************}
                   {**       Program CUPSMP.Pas       **}
                   {**   Written by Jarek Tuszynski   **}
                   {**        for CUPS Project        **}
                   {**         September 1995         **}
                   {************************************}
                   {************************************}

{$m 32768, 0, 65536}
PROGRAM MP_Directory;
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
   print(7,4,' Modern Physics');
   print(7,6,'   Simulations');
   print(3,2,'       CUPS');
   With BarMenu do
   begin
     Row[1] := 'Rutherford Scattering';
     Row[2] := 'Scattering of Electrons and X-Rays from Crystals';
     Row[3] := 'Special Relativity';
     Row[4] := 'Laser Cavities and Dynamics';
     Row[5] := 'Nuclear Properties';
     Row[6] := 'Nuclear Decays';
     Row[7] := 'Quantum Mechanics';
     Row[8] := 'Hydrogenic Atoms and H2+ Molecule ';
     Row[9] := 'Exit Program';
     draw(0.16,0.62,9,n);
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

procedure Run(dir,prog : string);
begin
   ChDir(Dir);
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
       1: Run('Scatter','Scatter');
       2: Run('Germer' ,'Germer');
       3: Run('SpecRel','SpecRel');
       4: Run('Laser'  ,'Laser');
       5: Run('Nuclear','Nuclear');
       6: Run('Nuclear','Decay');
       7: Run('Quantum','Quantum');
       8: Run('HAtom'  ,'HAtom');
       9: ExitRequest := true;
      END;
   UNTIL ExitRequest;
END.