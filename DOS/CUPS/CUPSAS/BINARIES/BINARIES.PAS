   (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   19th May, 1993.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*       Program menu for programs on binary stars.    *)
    (*                                                     *)
    (*******************************************************)

{$M 32768, 0, 65536}
{$F+}
PROGRAM Binaries;   {CUPS Programs for binary stars.}

USES DOS, GRAPH, CUPSMupp, CUPS, CUPSgui, binInit, Visual1, visual2,
     tidal, spectro, accrdisk, rocherad, eclipse;

{$O Visual1}
{$O visual2}
{$O tidal}
{$O spectro}
{$O accrdisk}
{$O rocherad}
{$O eclipse}

VAR
  ExitRequest : Boolean;
  n : integer;

FUNCTION ProgramChosen : integer;
VAR BarMenu : TBarMenu;
BEGIN
   ShowMouse;
   BarMenu.Init;
   ClearMUPPETPort;
   With BarMenu do
   begin
      Row[1] := 'The proper motion of a visual binary           ';
      Row[2] := 'The true orbit of a visual binary              ';
      Row[3] := 'Make your own spectroscopic binary             ';
      Row[4] := 'Make your own eclipsing binary                 ';
      Row[5] := 'The tidal history of a binary                  ';
      Row[6] := 'Orbits and Roche curves in a close binary      ';
      Row[7] := 'The formation of an accretion disk             ';
      Row[8] := '             Exit Program                      ';
      draw(0.2,0.8,8,n);
   END;
   n := BarMenu.chosen;
   ProgramChosen := n;
   ClearMUPPETPort;
END;

BEGIN
   CUPSInit;
   ExitRequest := False;
   n := 1;
   REPEAT
      CASE ProgramChosen of
       1: RunVisual1;
       2: RunVisual2;
       3: RunSpectro;
       4: RunEclipse;
       5: RunTidal;
       6: RunRocheRad;
       7: RunAccrDisk;
       8: ExitRequest := true;
      END;
   UNTIL ExitRequest;
   CUPSdone;
END.