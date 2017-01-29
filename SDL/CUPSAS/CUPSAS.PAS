                   {************************************}
                   {************************************}
                   {***        Program AS.Pas        ***}
                   {***  Written by Jarek Tuszynski  ***}
                   {***       for CUPS Project       ***}
                   {***         June 8, 1993         ***}
                   {************************************}
                   {************************************}

{$M 32768, 0, 65536}
PROGRAM AS_Directory;
USES DOS, GRAPH, CUPSMupp, CUPS, CUPSgui;

PROCEDURE TriplexFontProc;  EXTERNAL; {$L Trip.OBJ }

VAR
  ExitRequest : Boolean;
  n : integer;

FUNCTION ProgramChosen : integer;
VAR BarMenu : TBarMenu;
BEGIN
   CUPSinit;
   IF (RegisterBGIfont(@TriplexFontProc) < 0) THEN
   BEGIN
     Writeln('Ega/Vga: ', GraphErrorMsg(GraphResult));
     Pause; Halt;
   END;
   BarMenu.Init;
   ClearMUPPETPort;
   SetTextStyle(TriplexFont,HorizDir,5);
   print(8,3,'Astrophysics Simulations');
   SetUserCharSize(2,1,1,1);
   print(8,1,'     CUPS');
   With BarMenu do
   begin
      Row[1] := 'Programs on Binary Stars           ';
      Row[2] := 'Programs on the Motion of n-Bodies ';
      Row[3] := 'Programs on Galactic Kinematics    ';
      Row[4] := 'Interior Model of a Star';{ Stellar Structure Model}
      Row[5] := 'Stellar Evolution';
      Row[6] := 'Stellar Pulsation';
      Row[7] := 'Model of Stellar Atmosphere';
      Row[8] := '        Exit the Program';
      draw(0.25,0.65,8,n);
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
{   ShowCursor;}
   REPEAT
      CASE ProgramChosen of
       1: Run('Binaries');
       2: Run('NBodies');
       3: Run('Galaxies');
       4: Run('Stellar');
       5: Run('Evolve');
       6: Run('Pulse');
       7: Run('Atmos');
       8: exitRequest := true;
      END;
   UNTIL ExitRequest;
END.