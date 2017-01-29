           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.5 (95/02/13) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

                   {************************************}
                   {************************************}
                   {***      Program CUPScm.Pas      ***}
                   {***  Written by Darren Lewis     ***}
                   {***       for CUPS Project       ***}
                   {***         June 16, 1993        ***}
                   {************************************}
                   {************************************}

{$M 32768, 0, 65536}
PROGRAM CM_Directory;
USES DOS, GRAPH, CUPSMupp, CUPS, CUPSgui;

PROCEDURE TriplexFontProc;  EXTERNAL; {$L Trip.OBJ }

VAR
   ExitRequest,second : Boolean;
   n : integer;

FUNCTION GenMot : integer;
VAR
   OscMenu : TBarMenu;
   n : integer;
BEGIN
   With OscMenu do
   begin
      init;
      Row[1] := 'The Motion Generator';
      Row[2] := 'Projectile Motion in Air';
      Row[3] := 'Pendulum';
      Row[4] := 'Race Track';
      Row[5] := 'Main Menu';
      draw(0.54,0.6,5,n);
   end;
   n := OscMenu.Chosen;
   second := (n<>5);
   DefineViewPort(1,0.54,1,0.2,0.7);
   CloseViewPort(1);
   GenMot := n mod 5;
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
   print(3,4,'Classical Mechanics');
   print(3,6,'    Simulations');
   print(3,2,'      CUPS');
   if n>4 then n:=n-3;
   repeat
       With BarMenu do
       begin
         Row[1] := 'The Motion Generator         '+chr(16);
         Row[2] := 'Gravitational Orbits';
         Row[3] := 'Anharmonic Oscillators';
         Row[4] := 'Two Body Collisions';
         Row[5] := 'Coupled Oscillators';
         Row[6] := 'Rigid Body Rotational Dynamics';
         Row[7] := 'Exit Program';
         draw(0.1,0.6,7,n);
       end;
       if second then n:=1 else n:=BarMenu.Chosen;
       second := (n=1);
       if second then n:=GenMot else n:=n+3;
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
   n := 1;
   ShowCursor;
   REPEAT
      CASE ProgramChosen of
       1: Run('GenMot'  ,'gmGener');
       2: Run('GenMot'  ,'gm1DGrav');
       3: Run('GenMot'  ,'gmPend');
       4: Run('GenMot'  ,'gmRace');
       5: Run('Orbiter' ,'Orbiter');
       6: Run('Anharm'  ,'Anharm');
       7: Run('Colision','Colision');
       8: Run('CoupOsc' ,'CoupOsc');
       9: Run('Rotate'  ,'Rotate');
       10:exitRequest := true;
      END;
   UNTIL ExitRequest;
END.