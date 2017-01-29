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

                   {************************************}
                   {************************************}
                   {***      Program CUPSem.Pas      ***}
                   {***  Written by Jarek Tuszynski  ***}
                   {***       for CUPS Project       ***}
                   {***         June 8, 1993         ***}
                   {************************************}
                   {************************************}

{$M 32768, 0, 65536}
PROGRAM EM_Directory;
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
   print(8,3,'Electricity & Magnetism');
   print(8,5,'      Simulations');
   SetUserCharSize(2,1,1,1);
   print(8,1,'     CUPS');
   With BarMenu do
   begin
      Row[1] := 'Scalar and Vector Fields';
      Row[2] := 'Gauss'' Law in Symmetric Cases';
      Row[3] := 'Calculations of Potential using Poisson''s Eqn.';
      Row[4] := 'Image Charges and the Multipole Expansion';
      Row[5] := 'Atomic Polarization';
      Row[6] := 'Dielectric Media';
      Row[7] := 'Magnetostatics';
      Row[8] := 'Animated Electric Field of a Moving Charge';
      Row[9] := 'Electromagnetic Fields of a Moving Charge';
      Row[10]:= 'Electromagnetic Plane Waves';
      Row[11]:= '             Exit the Program';
      draw(0.17,0.7,11,n);
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
       1: Run('Fields');
       2: Run('Gauss');
       3: Run('Poisson');
       4: Run('Imag&Mul');
       5: Run('AtomPol');
       6: Run('Dielect');
       7: Run('MagStat');
       8: Run('QAnimate');
       9: Run('AccelQ');
       10:Run('EMwave');
       11:exitRequest := true;
      END;
   UNTIL ExitRequest;
END.