    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   19th May, 1993.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*     Program menu for programs on n-body systems.    *)
    (*                                                     *)
    (*******************************************************)

{$M 32768, 0, 65536}
Program NBodies;
{$F+}
USES DOS, GRAPH, CUPS, CUPSMUPP, CUPSgui, NBInit, NBody, astroids,
     elements, planets, playback, twogalax;

{$O NBody}
{$O astroids}
{$O elements}
{$O planets}
{$O playback}
{$O twogalax}

VAR
  ExitRequest : Boolean;
  n : Integer;

FUNCTION ProgramChosen : Integer;
VAR BarMenu : TBarMenu;
BEGIN
   BarMenu.Init;
   ShowMouse;
   ClearMUPPETPort;
   WITH BarMenu DO
   BEGIN
      Row[1] := 'Interaction between two galaxies';
      Row[2] := 'The Sun, Jupiter and asteroids  ';
      Row[3] := 'Many-body motion                ';
      Row[4] := 'Make your own Solar System      ';
      Row[5] := 'Play-back                       ';
      Row[6] := 'Orbital element demonstration   ';
      Row[7] := '       Exit Program             ';
      Draw(0.25,0.7,7,n);
   END;
   n := BarMenu.chosen;
   ProgramChosen := n;
   ClearMUPPETPort;
END;

BEGIN
   CUPSInit;
   ExitRequest := false;
   n := 1;
   REPEAT
      CASE ProgramChosen OF
       1: RunTwoGalax;
       2: RunAstroids;
       3: RunNBody;
       4: RunPlanets;
       5: RunPlayBack;
       6: RunElements;
       7: ExitRequest := True;
      END;
   UNTIL ExitRequest;
   CUPSDone;
END.