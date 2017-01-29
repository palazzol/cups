           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.0 (08/10/94) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

                   {************************************}
                   {************************************}
                   {***       Program CUPSqm.Pas     ***}
                   {***  Written by Jarek Tuszynski  ***}
                   {***       for CUPS Project       ***}
                   {***         June 8, 1994         ***}
                   {************************************}
                   {************************************}

{$M 32768, 0, 65536}
PROGRAM QM_Directory;
USES DOS, GRAPH, CUPSMupp, CUPS, CUPSgui;

PROCEDURE TriplexFontProc;  EXTERNAL; {$L Trip.OBJ }

VAR
  ExitRequest : Boolean;
  n : integer;

FUNCTION ProgramChosen : integer;
VAR BarMenu : TBarMenu;
BEGIN
   CUPSInit;
   IF (RegisterBGIfont(@TriplexFontProc) < 0) THEN
   BEGIN
     Writeln('Ega/Vga: ', GraphErrorMsg(GraphResult));
     Pause; Halt;
   END;
   BarMenu.Init;
   ClearMUPPETPort;
   SetTextStyle(TriplexFont,HorizDir,5);
   print(8,3,'  Quantum Mechanics');
   print(8,5,'      Simulations');
   SetUserCharSize(2,1,1,1);
   print(8,1,'     CUPS');
   With BarMenu do
   begin
      Row[1] := 'One Dimensional Bound State Problems';              {1}
      Row[2] := 'Stationary Scattering States in One Dimension';     {0}
      Row[3] := 'Quantum Mechanical Time Development';               {8}
      Row[4] := 'Electron States in a One Dimensional Lattice';      {3}
      Row[5] := 'Three Dimensional Bound State Problems';            {2}
      Row[6] := 'Identical Particles';                               {9}
      Row[7] := 'Stationary Scattering States in Three Dimensions';  {0}
      Row[8] := 'Bound States In Cylindrically-Symmetric Potentials'; {8}
      Row[9] := '             Exit the Program';
      draw(0.15,0.65,9,n);
   END;
   n := BarMenu.chosen;
   ProgramChosen := n;
   ClearMUPPETPort;
   CloseGraph;
   CUPSDone;
END;

procedure chdir(dir : string);
VAR
   Regs: registers;
BEGIN
   dir := dir + chr(0);
   Regs.AH := $3b;
   Regs.DS := seg(dir[1]);
   Regs.DX := ofs(dir[1]);
   MsDos(Regs);
END;

procedure Run(prog : string);
begin
   ChDir(prog);
   SwapVectors;
   Exec(prog+'.exe', '');
   SwapVectors;
   if DosError<>0 then writeln('Dos Error #',DosError);
   ChDir('..');
end;

BEGIN
   ExitRequest := False;
   n := 1;
   ShowCursor;
   REPEAT
      CASE ProgramChosen of
       1: Run('Bound1D');
       2: Run('Scattr1D');
       3: Run('QMtime');
       4: Run('Latce1D');
       5: Run('Bound3D');
       6: Run('Ident');
       7: Run('Scattr3D');
       8: Run('CylSym');
       9: exitRequest := true;
      END;
   UNTIL ExitRequest;
END.