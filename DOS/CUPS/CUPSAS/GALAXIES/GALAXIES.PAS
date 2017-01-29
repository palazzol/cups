PROGRAM Galaxies;   {Galaxies.PAS}

    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   19th May, 1993.          *)
    (*       Department of Mathematics,                    *)
    (*       North Carolina State University.              *)
    (*                                                     *)
    (*  Program menu for programs on galactic kinematics.  *)
    (*                                                     *)
    (*******************************************************)

{$M 64000,0,327680}
{$O+}
{$F+}

USES DOS, GRAPH, CUPS, CUPSMupp, CUPSgui,
     GalInit, Rotation, Arms21cm, OortCons;

{$O Rotation}
{$O Arms21cm}
{$O OortCons}

VAR
  exitRequest : Boolean;
  n : integer;

FUNCTION ProgramChosen : Integer;
VAR BarMenu : TBarMenu;
BEGIN
   BarMenu.Init;
   ClearMUPPETPort;
   WITH BarMenu DO
   BEGIN
      Row[1] := 'The rotation of a galaxy                       ';
      Row[2] := 'The spiral structure of a galaxy               ';
      Row[3] := 'Oort''s constants: Local rotation in a galaxy  ';
      Row[4] := '             Exit Program                      ';
      draw(0.2,0.8,4,n);
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
       1: RunRotation;
       2: RunArms21cm;
       3: RunOortCons;
       4: ExitRequest := True;
      END;
   UNTIL ExitRequest;
   CUPSDone;
END.