           (*************************************************)
           (*************************************************)
           (**                                             **)
           (**  Consortium of Upper-level Physics Software **)
           (**              ( CUPS Project )               **)
           (**      ( c) 1994 by John Wiley & Sons         **)
           (**  Compiled with Utilities Ver. 1.8 (95/08/10)**)
           (**                                             **)
           (*************************************************)
           (*************************************************)

                   {************************************}
                   {************************************}
                   {**       Program CUPSwo.Pas       **}
                   {**   Written by Jarek Tuszynski   **}
                   {**        for CUPS Project        **}
                   {**           August 1995          **}
                   {************************************}
                   {************************************}

{$m 32768, 0, 65536}
PROGRAM WO_Directory;
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
   print(7,4,'Waves and Optics');
   print(7,6,'   Simulations');
   print(3,2,'        CUPS');
   With BarMenu do
   begin
     Row[1] := 'Fourier Analysis and Fourier Transformations';
     Row[2] := 'Wave Equation and other PDE''s';
     Row[3] := 'Interference and Diffraction';
     Row[4] := 'Ray Tracing in Geometrical Optics';
     Row[5] := 'Ray Diagrams for Lenses and Mirrors';
     Row[6] := 'Applications of Interference and Diffraction';
     Row[7] := 'Electromagnetic Plane Waves';
     Row[8] := 'Waves Along a 1-D Chain of Point Masses';
     Row[9] := 'Exit Program';
     draw(0.2,0.62,9,n);
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
   ChDir(dir);
   SwapVectors;
   Exec(prog+'.exe', '');
   SwapVectors;
   if DosError<>0 then
   begin
      writeln('Dos Error #',DosError);
      Writeln('Press <Enter> to Continue');
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
       1: Run('Fourier', 'Fourier');
       2: Run('Wave',    'Wave');
       3: Run('Diffract','Diffract');
       4: Run('RayTrace','RayTrace');
       5: Run('TwoLens', 'TwoLens');
       6: Run('Spectrum','Spectrum');
       7: Run('EMWave',  'EMWave');
       8: Run('Chain' ,  'Chain');
       9: ExitRequest := true;
      END;
   UNTIL ExitRequest;
END.