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


(*   CUPS SIMULATION - ELECTRIC FIELD OF A MOVING CHARGE-Part 2 *)
(*    Program Title:     QAnimate *)
(*    Author:            Ron Stoner and Greg Myers *)
(*    Version:           #9 April 13, 1993) *)
(*    Program Summary:   This is an interactive animation of the generation *)
(*       of electromagnetic radiation fields by a point electric charge moving *)
(*       in two dimensions.  Charge motion can be manipulated by the user *)
(*       using keyboard "HotKeys".  The display can include electric field *)
(*       lines, radiation wave fronts, and their points of intersection. *)
(*    Library Files:     Page-flipping animation fron MUPPET Animation *)
(*       library; function ArcTan2 from CUPSfunc unit;  pull-down menuing and *)
(*       HotKeys routines from CUPSgui unit; various graphics utilities from *)
(*       MUPPET; Macheps from CUPSproc unit. *)

PROGRAM QAnimate (input, output);
 (*  *)
(* ************************************************************************* *)
(* *                 FIELD LINES AROUND A MOVING CHARGE                    * *)
(* **                                                                      * *)
(* *                 Concept and Algorithm by Ron Stoner                   * *)
(* *                                                                       * *)
(* *           Initial Program Design and Coding by Greg Meyers            * *)
(* *                                                                       * *)
(* *            Physics Dept., Bowling Green State University              * *)
(* *                        Bowling Green, Ohio                            * *)
(* *                                                                       * *)
(* *              Revisions by Ron Stoner March 1992:                      * *)
(* *                   1) Pull-down menus installed;                       * *)
(* *                   2) HotKeys installed;                               * *)
(* *                   3) Simplification of Main Program;                  * *)
(* *                   4) Minor display revisions made;                    * *)
(* *                   5) Reduced steering options;                        * *)
(* *                   6) Comments revised and supplemented;               * *)
(* *              Revisions by Stoner and Tuszynski Jul/Aug 1992           * *)
(* *                   1) Revised to run with newer animation software;    * *)
(* *                   2) Pascal code "prettied up";                       * *)
(* *              Revised July 1993 by Stoner to use                       * *)
(* *                 CUPS utilities distributed June 1993                  * *)
(* *                and to provide more robust help screens.               * *)
(* *     Last changed 6/26/94 -- minor bugs corrected                      * *)
(* ************************************************************************* *)

(* First, a description of the terminology in this program, and the *)
(* mechanics of the calculation. *)
(* All that exists: one moving charge, many points.  The points are used to *)
(* approximate electric field lines emanating from the charge. *)
(* The charge travels in a path determined by a manual control. *)
(* The charge's speed may never exceed the speed of light (a velocity *)
(* of 1.0 in the units used here). *)
(* It is helpful in understanding how this program works to think of *)
(* a point metaphorically, like a person who is "born" as one "generation" of *)
(* a "family", has an active life, "dies" and is "buried". *)
(* The points in a given generation are born simulataneously at the charge. *)
(* The line connecting all points in a given generation is a wavefront *)
(* circle, centered on the position of the charge when the front was *)
(* created.  The center of this circle does not change, but the wavefront *)
(* circle's radius increases at the speed of light. *)

(* The direction from the center of the circle to a point is where a *)
(* given field line meets the wavefront , and does not change as the wavefront *)
(* expands. In the rest frame of the charge, the field lines are isotropically *)
(* spread around the charge, but in the viewer's frame, the distribution of *)
(* these angles is distorted as the charge travels at higher *)
(* speeds.  This distortion is due to Lorentz contraction, and causes the *)
(* field line directions to compress along the direction of motion, i.e. to *)
(* shift toward the direction perpendicular to the direction of charge motion. *)
(* In the charge's rest frame, the angle a field line makes with the horizontal *)
(* is an integral multiple of 360 degrees divided by the number of points in a *)
(* generation (that is, the number of field lines shown) which is a constant *)
(* number set by the user from a data screen.  Since the wavefront is traveling *)
(* at light speed, no information about the charge's subsequent motions can *)
(* effect the direction at which point moves with the expanding wave front. *)
(* So, a point on a given field line is always moving radially from the place *)
(* where the charge was when the wavefront it is on was created, and in *)
(* a direction determined at that place and time, i.e. the point's birth. *)
(* Since the electric field has zero dispersion except at the charge itself, *)
(* a given field line is continuous, and can be approximated by connecting all *)
(* of the points in the same "family" in different generations.  However, since *)
(* the charge moves and accelerates, both centers of the wavefronts and the *)
(* direction of radial expansion will be different for the different points *)
(* of the same family, so that in general, a curved and distorted field line *)
(* will result. *)
(* Once "born" points continue to move until they move out of the viewing area, *)
(* or "die".  Dead points are only kept track of for as long as they are needed; *)
(* then they are deleted from memory ("buried").  A dead point is buried when *)
(* it has no living neighbors (children or parents) in its field line (family). *)
(* How is all this represented in memory? *)
(* The charge's position, velocity, and acceleration are updated in *)
(* these variables: *)
(* ElectronX         ElecVelX          Velocity          Acceleration *)
(* ElectronY         ElecVelY          Angle             Turn *)
(* Turning (a Boolean variable) *)
(* All the rest of the necessary data about the points is stored in an *)
(* array of linked lists, for the field lines, and a single linked list, *)
(* for the circles. There is no advantage in using the points to draw the *)
(* circles; the built-in Turbo Pascal Circle procedure is more accurate *)
(* than drawing the lines connecting the points in a generation.  The field *)
(* lines themselves can be drawn quickly by moving along any one of the *)
(* linked lists in the FieldLines array.   Succeeding frames are drawn by *)
(* advancing all the points in their respective directions, and increasing *)
(* the circle radii at the speed of light. *)
(* Object-Oriented Programming (OOP) is used extensively in the simulation. *)
(* The points themselves are objects; these objects are *)
(* packaged with pointers to make records; lists of these records are used *)
(* in larger objects.  The CircleList is a large object, as is each *)
(* FieldLine.  The FieldLines then make up another linked list -- the Field- *)
(* LinePtr structure.   *)

   USES
      Graph, CUPSmupp, CUPS, CUPSgrph, CUPSgui, CUPSfunc, CUPSproc;
 {R-}

   TYPE


 {**************** charge Old Positions data structure ***************}

      ElecOldPositionsPtr = ^ElecOldPositionsNode;
      ElecOldPositionsNode = RECORD
            X, Y: Real;
            Next: ElecOldPositionsPtr
         END;


 {*********************** Point data structures ************************}
      Point = OBJECT
            X, Y, VelX, VelY: Real;
            Living: Boolean;          {Living = on the viewing area}
            PROCEDURE InitPoint (XPos, YPos, XVel, YVel: Real);
           {initialize all the point's vital statistics}
            PROCEDURE Move (dtime: REAL);          {update a point's position   }
            PROCEDURE Kill;                        {mark the Living field FALSE }
            PROCEDURE ReadPos (VAR XPos, YPos: Real); {return X and Y           }
            FUNCTION Dead: Boolean;                   {return TRUE or FALSE     }
         END;

      PointPtr = ^PointNode;
      PointNode = RECORD
            Data: Point;           {a single point object        }
            Next: PointPtr         {pointer to the next node     }
         END;

 {********************** Field line data structures ********************}

      FieldLine = OBJECT                   {one list of points           }
            ListStart, Cursor, Previous: PointPtr;{pointrs to single PointNodes }
            BaseAngle: Real;    {This is used to calculate each}
{
                                          point's departure angle      }
            PROCEDURE Initialize (LineNumber, MaxNumber: Integer);
              {set fieldline original values}

            PROCEDURE GiveBirth (XPos, YPos, Beta, Angle: Real);
              {perform all necessary calculations and initiate a point }

            PROCEDURE Bury;                       {delete the point}
            PROCEDURE PlotPoints;
            PROCEDURE DrawLine (ScaleSize, ElectronX, ElectronY: Real);

            PROCEDURE UpdateLine (ScaleSize: REAL; dtime: REAL);
              {update member points}

            PROCEDURE DestroyLine;                {completely remove the line   }
         END;

      FieldLinePtr = ^FieldLineNode;
      FieldLineNode = RECORD
            Data: FieldLine;
            Next: FieldLinePtr
         END;

 {********************* Wave Front data structures *********************}

      WaveFront = OBJECT                {one single circle               }
            CntrX, CntrY: Integer;           {The center does not move!       }
            Radius: REAL;              {Only the Radius changes!        }
            PROCEDURE Initialize (X, Y: Real); {set starting values             }
            PROCEDURE Enlarge (dtime: REAL);   {increase Radius           }
            PROCEDURE Draw (ScaledX, ScaledY: Real);
            FUNCTION ReadRadius: REAL;         {return Radius                   }
         END;

      WaveFrontPtr = ^WaveFrontNode;       {analogous to PointPtr and PointNode}
      WaveFrontNode = RECORD
            Data: WaveFront;
            Next: WaveFrontPtr
         END;

      WaveFrontList = OBJECT  {the list of WaveFrontNodes          }
            ListStart, Cursor, Previous: WaveFrontPtr;
               {pntrs. to individual WaveFrontNodes }
            PROCEDURE Initialize;             {set the three pointers to NIL       }
            PROCEDURE Insert (X, Y: Real);    {add new node to WaveFront list      }
            PROCEDURE Delete;                 {take old node off WaveFront list    }
            PROCEDURE DrawCircles (ScaledX, ScaledY: Real);
              {draw all circles in the list        }
            PROCEDURE UpdateList (ScaleSize: REAL; dtime: REAL);
              {enlarge all circles                 }
            PROCEDURE DestroyList;            {completely erase the list           }
         END;

      {----------------------- Program Variables ------------------------}
   CONST
      whitte = white;
      blakk = black;
   VAR

      { ------ The two important data structures ------ }
      FieldLineListStart, FieldLineCursor: FieldLinePtr;
      CircleList: WaveFrontList;
       { ----------------------------------------------- }

      DisplayDataScreen    :  TInputScreen;
      KeyboardDataScreen   : TInputScreen;
      ColorDataScreen      : TInputScreen;

      TheScaleSize: Real;
      NumInGeneration: Integer; {Number of field lines set by user}
      TrailLength  : Integer;
      ElectronColor,                 {Color values set by user         }
      PointColor, FieldLineColor, WaveFrontColor: WORD;
      Turn          : REAL;

      MyMenu: TMenu;   { Pull-down Menu}
      key: byte;   { Hotkey variable}
      HotKeys: THotKeys;

      TempString: STRING;  {Used for displaying electron values.}

      ScaledX,                       {Used to draw wavefronts to scale.}
      ScaledY: Real;
      ViewPortTester: ViewPortType;

      Frame: TFrames;  {Used in frame flipping animation.}

      dtime: REAL;
      Turning : BOOLEAN;
      StopAnimation : BOOLEAN;
      JustDisplaced : BOOLEAN;
      MousOn        : BOOLEAN;

      xstart,                        {User-supplied intitial values.}
      ystart, vxstart, vystart: REAL;

      AxisVelInc,                    {Keyboard adjustments set by user.}
      AngleInc, VelocInc, TurnInc, AccelInc: REAL;

      PlotPoints,                    {Deciding on animation options.}
      DrawLines, DrawCircles, ShowKeybdInfo: BOOLEAN;
      QuitFlag                             : BOOLEAN;

 {------------------------Point Procedures------------------------------}

   PROCEDURE Point.InitPoint (XPos, YPos, XVel, YVel: Real);
   BEGIN
      X := XPos;
      Y := YPos;
      VelX := XVel;
      VelY := YVel;
      Living := TRUE
   END;

   PROCEDURE Point.Move (dtime: REAL);
   BEGIN
      X := X + VelX * dtime;
      Y := Y + VelY * dtime;
   END;

   PROCEDURE Point.Kill;
   BEGIN
      Living := FALSE
   END;

   PROCEDURE Point.ReadPos (VAR XPos, YPos: Real);
   BEGIN
      XPos := X;
      YPos := Y
   END;

   FUNCTION Point.Dead: Boolean;
   BEGIN
      Dead := NOT Living
   END;

 {------------------------Field Line Procedures-------------------------}

   PROCEDURE FieldLine.Initialize (LineNumber, MaxNumber: Integer);
   BEGIN
      ListStart := NIL;
      Cursor := NIL;
      Previous := NIL;
      BaseAngle := (PI * 2 * (LineNumber-0.5) / MaxNumber)
   END;

   PROCEDURE FieldLine.GiveBirth (XPos, YPos, Beta, Angle: Real);
{This procedure gives a point is the direction it will move until off screen.}
      VAR
         ThetaN, Temp, Alpha, Gamma, Theta, XVel, YVel: Real;

   BEGIN
      Cursor := ListStart;
      new(ListStart);
      ListStart^.next := Cursor;
      IF Beta < 0 THEN
      BEGIN
         Beta := -Beta;
         Angle := Angle + pi
      END;

      Gamma  := 1 / sqrt(1 - sqr(Beta));
      ThetaN := BaseAngle - Angle;
      Theta  := ArcTan2(cos(ThetaN) / Gamma, sin(ThetaN));
      Alpha  := Theta - ArcSin(Beta * sin(Theta));
      XVel   := cos(Angle + Alpha);
      YVel   := sin(Angle + Alpha);

      ListStart^.data.InitPoint(XPos, YPos, XVel, YVel)
   END;

   PROCEDURE FieldLine.Bury;
   BEGIN
      Previous^.Next := Cursor^.Next;
      Dispose(Cursor);
      Cursor := Previous^.Next
   END;

   PROCEDURE FieldLine.PlotPoints;
      VAR
         Xloc, Yloc: Real;
   BEGIN
      Cursor := ListStart;
      WHILE NOT (Cursor = NIL) DO
      BEGIN
         Cursor^.data.ReadPos(Xloc, Yloc);
         PlotSymbol ( Xloc , Yloc , #46 );    {Plot point as a solid dot}
         Cursor := Cursor^.Next
      END
   END;

   PROCEDURE FieldLine.DrawLine (ScaleSize, ElectronX, ElectronY: Real);

      PROCEDURE Clip (VAR X1, Y1: Real; X2, Y2, ScaleSize: Real);
         BEGIN
         IF abs (X1) > ScaleSize THEN
            BEGIN
            Y1 := Y1+((Y2-Y1)/(X2-X1))*(ScaleSize*(X1/abs(X1))-X1);
            X1 := ScaleSize*(X1/abs(X1))
            END;
         IF abs (Y1) > ScaleSize THEN
            BEGIN
            X1 := X1+((X2-X1)/(Y2-Y1))*(ScaleSize*(Y1/abs(Y1))-Y1);
            Y1 := ScaleSize*(Y1/abs(Y1))
            END
			END; {Proc. Clip}
 
   VAR
      XPos, YPos, OldX, OldY, TempX, TempY: REAL;
      LastPointDead: BOOLEAN;
   BEGIN
      Cursor := ListStart;
      LastPointDead := FALSE;
      OldX := ElectronX;
      OldY := ElectronY;
      WHILE NOT (Cursor = NIL) DO
         BEGIN
         Cursor^.Data.ReadPos (XPos, YPos);
         TempX := XPos;
         TempY := YPos;
         IF (LastPointDead) XOR (Cursor^.Data.Dead) THEN
            IF LastPointDead
               THEN Clip (OldX, OldY, TempX, TempY, ScaleSize)
            ELSE Clip (TempX, TempY, OldX, OldY, ScaleSize);
         IF NOT ((LastPointDead) AND (Cursor^.Data.Dead)) THEN
            PlotLine (OldX, OldY, TempX, TempY);
         OldX := XPos;
         OldY := YPos;
         LastPointDead := Cursor^.Data.Dead;
         Cursor:=Cursor^.Next
         END
    END;



   PROCEDURE FieldLine.UpdateLine (ScaleSize: REAL; dtime: REAL);
{Procedure to move all points on a field line, and to bury all points}
{that are no longer necessary.}
      VAR
         Xpos, Ypos: Real;
      PROCEDURE Advance;
      BEGIN
         Previous := Cursor;
         Cursor := Cursor^.Next
      END;

   BEGIN
      Cursor := ListStart;
      Previous := NIL;
      WHILE NOT (Cursor = NIL) DO
      BEGIN
         Cursor^.Data.Move(dtime);
         Cursor^.Data.ReadPos(Xpos, Ypos);
         IF (abs(Xpos) > ScaleSize) OR (abs(Ypos) > ScaleSize) THEN
            Cursor^.Data.Kill;
         IF NOT (Previous = NIL) THEN
            IF Cursor^.data.Dead AND Previous^.data.Dead THEN
               IF (Cursor^.next = NIL) THEN
                  Bury
               ELSE IF Cursor^.next^.data.Dead THEN
                  Bury
               ELSE
                  Advance
            ELSE
               Advance
         ELSE
            Advance
      END
   END; {proc. FieldLine.UpdateLine}

   PROCEDURE FieldLine.DestroyLine;
   BEGIN
      Cursor := ListStart;
      WHILE NOT (Cursor = NIL) DO
      BEGIN
         Previous := Cursor;
         Cursor := Cursor^.Next;
         Dispose(Previous)
      END
   END;

 {----------------------Wave Front Procedures---------------------------}

   PROCEDURE WaveFront.Initialize (X, Y: Real);
   BEGIN
      Radius := 0;
      Map(X, Y, CntrX, CntrY)
   END;

   PROCEDURE WaveFront.Enlarge (dtime: REAL);
   BEGIN
      Radius := Radius + dtime
   END;

   PROCEDURE WaveFront.Draw (ScaledX, ScaledY: Real);
   BEGIN
      Ellipse(CntrX, CntrY, 0, 360, Round(ScaledX * Radius), Round(ScaledY * Radius))
   END;

   FUNCTION WaveFront.ReadRadius: real;
   BEGIN
      ReadRadius := Radius
   END;

   PROCEDURE WaveFrontList.Initialize;
   BEGIN
      ListStart := NIL;
      Cursor := NIL;
      Previous := NIL
   END;

   PROCEDURE WaveFrontList.Insert (X, Y: Real);
   BEGIN
      Cursor := ListStart;
      new(ListStart);
      ListStart^.Next := Cursor;
      ListStart^.Data.Initialize(X, Y)
   END;

   PROCEDURE WaveFrontList.Delete;
   BEGIN
      Previous^.Next := NIL;
      Dispose(Cursor);
      Cursor := NIL
   END;

   PROCEDURE WaveFrontList.DrawCircles (ScaledX, ScaledY: REAL);
   BEGIN
      Cursor := ListStart;
      WHILE NOT (Cursor = NIL) DO
      BEGIN
         Cursor^.Data.Draw(ScaledX, ScaledY);
         Cursor := Cursor^.Next
      END
   END;

   PROCEDURE WaveFrontList.UpdateList (ScaleSize, dtime: REAL);
   BEGIN
      Cursor := ListStart;
      WHILE NOT (Cursor = NIL) DO
      BEGIN
         IF Cursor^.data.ReadRadius < (2.84 * ScaleSize) THEN
         BEGIN
            Cursor^.Data.Enlarge(dtime);
            Previous := Cursor;
            Cursor := Cursor^.Next
         END
         ELSE
            Delete
      END
   END;

   PROCEDURE WaveFrontList.DestroyList;
   BEGIN
      Cursor := ListStart;
      WHILE NOT (Cursor = NIL) DO
      BEGIN
         Previous := Cursor;
         Cursor := Cursor^.Next;
         Dispose(Previous)
      END
   END;


 {---------------------   Program Procedures   ---------------------------}
Procedure Credits;
   Var
      screen: HelpScrType;
   Begin
   screen[01] :=  '                                                  ';
   screen[02] :=  '    ANIMATED ELECTRIC FIELD OF A MOVING CHARGE    ';
   screen[03] :=  '                                                  ';
   screen[04] :=  '                CUPS Simulation                   ';
   screen[05] :=  '                       by                         ';
   screen[06] :=  '           Ron Stoner  and  Greg Myers            ';
   screen[07] :=  '       Department of Physics and Astronomy        ';
   screen[08] :=  '         Bowling Green State University           ';
   screen[09] :=  '            Bowling Green, OH 43403               ';
   screen[10] :=  '                                                  ';
   screen[11] :=  'This program uses animation to show the electric  ';
   screen[12] :=  'field pattern generated by a point electric charge';
   screen[13] :=  'moving in the plane of the screen.  The charge    ';
   screen[14] :=  'motion can be controlled by the user via the      ';
   screen[15] :=  'keyboard arrow keys and spacebar.  The rate at    ';
   screen[16] :=  'which the animation frames are generated can be   ';
   screen[17] :=  'increased or decreased by using function keys     ';
   screen[18] :=  'F4 and F5 as indicated at bottom of screen,       ';
   screen[19] :=  'but F4 and F5 do not affect charge velocity.      ';
   screen[20] :=  'Various options for controlling the display and   ';
   screen[21] :=  'the animations can be accessed by pressing <ESC>  ';
   screen[22] :=  'to access a pull-down menu.                       ';
   screen[23] :=  '  Copyright (c) John Wiley & Sons Inc. 1995       ';
   screen[24] :=  '                                                  ';
   screen[25] :=  '   >>>>>   PRESS ANY KEY TO CONTINUE      <<<<<   ';
   help(screen);
   END;

   Procedure Instructions;
   Var
      screen: HelpScrType;
   Begin
      screen[01] :=   '                                                  ';
      screen[02] :=   '                                                  ';
      screen[03] :=   '   KEYBOARD CONTROL USES THESE KEYS:              ';
      screen[04] :=   '                                                  ';
      screen[05] :=   '    UP   Arrow -- Increases forward velocity.     ';
      screen[06] :=   '  DOWN   Arrow -- Decreases forward velocity.     ';
      screen[07] :=   ' RIGHT   Arrow -- Increments right turning rate.  ';
      screen[08] :=   '  LEFT   Arrow -- Decrements right turning rate.  ';
      screen[09] :=   ' SPACE   Bar   -- Toggles Turning ON/OFF.         ';
      screen[10] :=   '                                                  ';
      screen[11] :=    ' The arrow keys are used much like the accelerator';
      screen[12] :=    ' and wheel of a car, except that velocity is      ';
      screen[13] :=    ' changed in small increments, and the spacebar    ';
      screen[14] :=    ' engages or disengages the steering.              ';
      screen[15] :=    '                                                  ';
      screen[16] :=    ' With steering engaged, the charge will move in a ';
      screen[17] :=    ' circle.  The velocity will not increase beyond   ';
      screen[18] :=    ' the speed of light, nor decrease to less than    ';
      screen[19] :=    ' the negative of the speed of light.  At constant ';
      screen[20] :=    ' speed and turning rate, the charge can be        ';
      screen[21] :=    ' controlled by the spacebar alone.  Hotkeys F4 and';
      screen[22] :=    ' F5 control speed of animation, not the actual    ';
      screen[23] :=    ' physical velocity of the charge.                 ';
      screen[24] :=   '                                                  ';
      screen[25] :=    '   >>>>>   PRESS ANY KEY TO CONTINUE      <<<<<   ';
      help(screen);
   END;

   PROCEDURE PlotLine( x1, y1, x2, y2: REAL);
   VAR
      Gx1, Gy1, Gx2, Gy2: integer;
   BEGIN
      Map(x1, y1, Gx1, Gy1);
      Map(x2, y2, Gx2, Gy2);
      Line(Gx1, Gy1, Gx2, Gy2);
   END;


   PROCEDURE HandleHotKeys( key : BYTE; VAR dt : REAL);
   BEGIN
         CASE key OF
            1: Begin
                 Frame.Pause ;
                 Instructions
               End;
            2: DynamicPause;
            3: begin
                 dt := dt*1.414;
                 if dt > 1.0   then  dt := 1.0   ;
               end;
            4: dt := dt/1.414;
            5: StopAnimation := TRUE;
         END; {CASE key}
   END;

   PROCEDURE UpdateElectron (VAR EnoughElec, Turning: BOOLEAN;
                           VAR Turnrate, angle, velocity, dtime, vx, vy, x, y: REAL;
                           TurnInc, AccelInc, AngleInc, VelocInc, AxisVelInc: REAL;
                           VAR ElecListStart, ElecListEnd: ElecOldPositionsPtr;
                           VAR ElecCount, MaxElecCount: INTEGER);
      VAR
         InKey: WORD;
         OldVel, OldVX, OldVY, dv: REAL;
         Cursor: ElecOldPositionsPtr;
         Temp1, Temp2: ShortInt;
         Palette: PaletteType;
   BEGIN
      CheckforEvents;
      IF HotKeys.Pressed(key) THEN HandleHotKeys(key, dtime);
      IF ord(Event.ReadKey) = 27 THEN StopAnimation := TRUE ELSE
      IF Event.ExtendedKey THEN
      BEGIN
        InKey := ord(Event.ReadKey);
        IF (InKey = 72) AND (abs(dtime*accelinc + Velocity) < 1)
               THEN Velocity := Velocity + dtime*accelinc
            ELSE IF (InKey = 80) AND   (abs(Velocity-dtime*accelinc) < 1)
               THEN Velocity := Velocity - dtime*accelinc
            ELSE IF InKey = 77 THEN
               BEGIN
                  Turning := TRUE;
                  TurnRate := TurnRate - TurnInc;
               END
            ELSE IF InKey = 75 THEN
               BEGIN
                  Turning := TRUE;
                  TurnRate := TurnRate + TurnInc;
               END;
          END;
      IF Event.ReadKey = ' ' THEN Turning := NOT Turning;

      IF Turning THEN Angle := Angle + (Turn * Velocity * dtime);
      VX := Velocity * cos(Angle);
      VY := Velocity * sin(Angle);
      IF ABS(Angle) > PI THEN
         Angle := Angle - (2 * PI * (Angle / ABS(Angle)));
      X := X + VX * dtime;
      Y := Y + VY * dtime;
      new(Cursor);
      IF (ElecListEnd = NIL) THEN
         ElecListEnd := Cursor;
      ElecListStart^.Next := Cursor;
      ElecListStart := Cursor;
      Cursor^.X := X;
      Cursor^.Y := Y;
      Cursor^.Next := NIL;

      IF EnoughElec THEN
      BEGIN
         Cursor := ElecListEnd;
         ElecListEnd := ElecListEnd^.Next;
         dispose(Cursor)
      END
      ELSE
      BEGIN
         INC(ElecCount);
         EnoughElec := (ElecCount = MaxElecCount)
      END;

      Cursor := ElecListEnd;
      REPEAT
         PlotSymbol ( Cursor ^ . X , Cursor ^ . Y , #46 ); {Plots position of charge}
         Cursor := Cursor^.Next;
      UNTIL (Cursor = NIL);
   END; {proc. UpdateElecton}

   PROCEDURE MainInitialize (VAR FLListStart, FLCursor: FieldLinePtr;
                           VAR CList: WaveFrontList;
                           NumberOfFieldLines: Integer);
      VAR
         LineCounter: Integer;
   BEGIN
      new(FLListStart);
      FLCursor := FLListStart;
      FLListStart^.data.Initialize(0, NumberOfFieldLines);
      LineCounter := 1;
      WHILE NOT (LineCounter = NumberOfFieldLines) DO
      BEGIN
         new(FLCursor^.next);
         FLCursor := FLCursor^.next;
         FLCursor^.data.Initialize(LineCounter, NumberOfFieldLines);
         INC(LineCounter);
      END;
      FLCursor^.next := NIL;
      CList.Initialize
   END; {Proc. MainInitialize}



   PROCEDURE MainDestroy (VAR FLCursor: FieldLinePtr;
                           VAR CList: WaveFrontList);
      VAR
         FLPrevious: FieldLinePtr;

   BEGIN
      WHILE NOT (FLCursor = NIL) DO
      BEGIN
         FLPrevious := FLCursor;
         FLCursor := FLCursor^.Next;
         FLPrevious^.Data.DestroyLine;
         Dispose(FLPrevious);
      END;
      CList.DestroyList
   END; {MainDestroy}



   PROCEDURE SetUpMyMenu;  {Pulldown MENU}
   BEGIN
      WITH MyMenu DO
      BEGIN
         init;
         column(1, 'FILE');
         row(1, 1, 'About CUPS');
         row(1, 2, 'Credits');       {Credits}
         row(1, 3, 'Configuration');
         row(1, 4, 'EXIT program');
         column(2, 'ANIMATE');
         row(2, 1, 'Use new or current data');
         row(2, 2, 'Return to default data');
         column(3, 'MODIFY');
         row(3, 1, 'Animation Data');
         row(3, 2, 'Color Choices');
         row(3, 3, 'Display Options');
         column(4, 'EXPLAIN');
         row(4, 1, 'About Program'); {MainHelp}
         row(4, 2, 'Motion Controls');
         row(4, 3, 'About Units');   {UnitsHelp}
      END;
   END;

   PROCEDURE SetUpHotKeys;
   BEGIN
      WITH HotKeys DO
      BEGIN
         init(5);
         key[1] := 'F1-HELP';
         key[2] := 'F3-STEP';
         key[3] := 'F4-FASTER';
         key[4] := 'F5-SLOWER';
         key[5] := 'ESC-QUIT';
      END;
   END;

   Procedure OptionsDefaults(VAR ScreenSize: REAL;
                           VAR NumLines, TrailLength: INTEGER;
                           VAR Points, Lines, Circles, KeybdVal: BOOLEAN);
   BEGIN
      ScreenSize :=  10 ;
      NumLines :=   8  ;
      TrailLength := 8   ;
      Points :=   FALSE     ;
      Lines :=   TRUE     ;
      Mouson := FALSE ;
      Circles :=  FALSE     ;
      KeyBdVal := TRUE    ;
   END;

   Procedure DisplayHelp (Var screen: HelpScrType);
   Begin
      screen[1] := '                                                  ';
      screen[2] := '* Parameters set by this screen determine         ';
      screen[3] := '  how the animation will be displayed.            ';
      screen[4] := '                                                  ';
      screen[5] := '* Choosing too many field lines may make the      ';
      screen[6] := '  animation too slow or overload the memory.      ';
      screen[7] := '                                                  ';
      screen[8] := '* The electron trail is a series of past position ';
      screen[9] := '  markers to show the recent charge trajectory.   ';
      screen[10] := '                                                  ';
      screen[11] := '* An X indicates that the indicated objects       ';
      screen[12] := '  will be drawn.  For example, if the lines       ';
      screen[13] := '  box is blank, no field lines are drawn.         ';
      screen[14] := '                                                  ';
      screen[15] := '* Drawing wave fronts can slow the animation      ';
      screen[16] := '  but adds to intuition about how the radiation   ';
      screen[17] := '  field is created.                               ';
      screen[18] := '                                                  ';
      screen[19] := '  Ok accepts screen values;  Cancel resets them.  ';
      screen[20] := '                                                  ';
      screen[21] := '* You should use values for the parameters that   ';
      screen[22] := '  are within the reasonable ranges suggested on   ';
      screen[23] := '  the screen.                                     ';
      screen[24] := '                                                  ';
      screen[25] := '                                                  ';
   End;

   PROCEDURE GetDisplayOptions (VAR ScreenSize: REAL;
                           VAR NumLines, Trail: INTEGER;
                           VAR Points, Lines, Circles, KeybdVal: BOOLEAN);
       {Data input screen for display parameters}
   VAR
      DisplayHelpScr       : HelpScrType;
   BEGIN
      DisplayHelp(DisplayHelpScr);
      With DisplayDataScreen Do
      Begin
      init;
      DefineInputPort(0.13, 0.87, 0.10, 0.90);
      LoadLine('');
      LoadLine( '                    PROGRAM OPTIONS:                    ');
      LoadLine('');
      LoadLine(  ' Size of Display Area :  (5.0-80.0) {    }  (cm.?)');
      LoadLine(  ' Number of flux lines to display:  (3-15)   {   }       ');
      LoadLine(  ' Length of electron trail:  (1-20)   {   }              ');
      LoadLine('');
      LoadLine(  ' Display Options:         (X = YES)                   ');
      LoadLine(  '   Plot Points               #F                         ');
      LoadLine(  '   Draw Field Lines          #T                         ');
      LoadLine(  '   Draw Wave Front Circles   #F                         ');
      LoadLine(  '   Show Keyboard Values      #T                       ');
      LoadLine(  '   Show Mouse Cursor         #T                         ');
      LoadLine(  ' To flip switches, select the button and hit any key.  ');
      LoadLine('');
      LoadLine('          [  Ok  ]       [Cancel]       [ Help ]');
      SetNumber(1,ScreenSize);
      SetNumberLimits(1, 5, 80);
      SetNumber(2,NumLines);
      SetNumberLimits(2, 3, 15);
      SetNumber(3,TrailLength);
      SetNumberLimits(3, 1, 20);
      SetBoolean(4,Points);
      SetBooleaN(5,Lines);
      SetBoolean(6,Circles );
      SetBoolean(7,KeybdVal);
      SetBoolean(8,Mouson  );
      SetHelpScreen(DisplayHelpScr);
      Accept;
      ScreenSize := GetNumber(1);
      NumLines := Round(GetNumber(2));
      Trail:= Round(GetNumber(3));
      Points := GetBoolean(4);
      Lines := GetBoolean(5);
      Circles := GetBoolean(6);
      KeybdVal := GetBoolean(7);
      MousOn   := GetBoolean(8)
      End;
   END; {OptionsDefaults}



   PROCEDURE DefaultColors (VAR Electron, Points, Lines, Circles: WORD);

   BEGIN
      electron := 7;
      points := 10;
      lines := 14;
      circles := 4;
   END; {ColorDefaults}

    Procedure ColorsHelp (Var screen: HelpScrType);
   Begin
      screen[1] := '                                                  ';
      screen[2] := '* This data screen allows you to choose your      ';
      screen[3] := '  own set of colors for displaying the moving     ';
      screen[4] := '  charge, the field lines, the wave fronts,       ';
      screen[5] := '  and the points where they intersect.            ';
      screen[6] := '                                                  ';
      screen[7] := '* Remember that the brighter colors are White,    ';
      screen[8] := '  Yellow and Light Green.   Darker colors are     ';
      screen[9] := '  Black, DarkGray and DarkBlue.                   ';
      screen[10] := '                                                  ';
      screen[11] := '                                                  ';
      screen[12] := '                                                  ';
      screen[13] := '                                                  ';
      screen[14] := '                                                  ';
      screen[15] := '                                                  ';
      screen[16] := '                                                  ';
      screen[17] := '                                                  ';
      screen[18] := '                                                  ';
      screen[19] := '  Ok accepts screen values;  Cancel resets them.  ';
      screen[20] := '                                                  ';
      screen[21] := '                                                  ';
      screen[22] := '                                                  ';
      screen[23] := '                                                  ';
      screen[24] := '                                                  ';
      screen[25] := '                                                  ';
   End;

   PROCEDURE GetColors (VAR Electron, Points, Lines, Circles: WORD);
    {Interprets data screen for color choices}
   VAR ColorsHelpScreen : HelpScrType;
   BEGIN
      ColorsHelp(ColorsHelpScreen);
      With ColorDataScreen Do
      Begin
      init;
      DefineInputPort(0.13, 0.87, 0.10, 0.90);
      LoadLine('');
      LoadLine('                     COLOR SETTINGS:  ');
      LoadLine('   Electron:           (Default Lt. Gray)   {   } ');
      LoadLine('   Points:             (Default Lt. Green)  {   } ');
      LoadLine('   Field Lines:        (Default Yellow)     {   } ');
      LoadLine('   Wave Front Circles: (Default Red)        {   } ');
      LoadLine('');
      LoadLine('          [  Ok  ]       [Cancel]       [ Help ]');
      LoadLine('');
      LoadLine('');
      LoadLine(' ------------------- Possible colors ------------------ ');
      LoadLine('  0 Black      1 Blue          2 Green       3 Cyan     ');
      LoadLine('  4 Red        5 Magenta       6 Brown       7 Lt. Gray ');
      LoadLine('  8 Dk. Gray   9 Lt. Blue     10 Lt. Green  11 Lt. Cyan ');
      LoadLine(' 12 Lt. Red   13 Lt. Magenta  14 Yellow     15 White    ');
      SetNumber(1,Electron);
      SetNumber(2,Points);
      SetNumber(3,Lines);
      SetNumber(4,Circles);
      SetHelpScreen(ColorsHelpScreen);
      Accept;
      Electron := Round(GetNumber(1));
      Points := Round(GetNumber(2));
      Lines := Round(GetNumber(3));
      Circles := Round(GetNumber(4));
      END;
   END;


   PROCEDURE KeyboardDefaults (VAR AccInc, TurnInc, dtime: REAL );
   BEGIN
   AccInc :=  0.20;
   TurnInc :=  0.10;
   Turning := TRUE;
   dtime := 0.25;
   Turn:= 1.0;
   xstart := 8.5;
   ystart := 8.0;
   vxstart := 0;
   vystart := 0.75;

   END; {KeyboardDefaults}

   Procedure KeyBoardHelp (Var screen: HelpScrType);
   Begin
      screen[1] := '                                                  ';
      screen[2] := '* This data screen allows you to choose the       ';
      screen[3] := '  values of parameters that affect the way the    ';
      screen[4] := '  moving charge responds to "on-the-fly" input    ';
      screen[5] := '  from the computer keyboard.                     ';
      screen[6] := '                                                  ';
      screen[7] := '* The acceleration is the velocity per            ';
      screen[8] := '  unit time added by one press of the             ';
      screen[9] := '  up arrow or subtracted by one press of the      ';
      screen[10] := '  down arrow.                                     ';
      screen[11] := '                                                  ';
      screen[12] := '* The Turn Increment determines how much the      ';
      screen[13] := '  turning rate is changed by a press of the       ';
      screen[14] := '  left arrow or the right arrow.  The turning     ';
      screen[15] := '  rate is proportional to the rate of change      ';
      screen[16] := '  of the direction of particle velocity with time.';
      screen[17] := '                                                  ';
      screen[18] := '                                                  ';
      screen[19] := '  Ok accepts screen values;  Cancel resets them.  ';
      screen[20] := '                                                  ';
      screen[21] := '                                                  ';
      screen[22] := '                                                  ';
      screen[23] := '                                                  ';
      screen[24] := '                                                  ';
      screen[25] := '                                                  ';
   End;
   PROCEDURE GetKeyboard (VAR Accel, TurnInc, dtime, TurnRate: REAL);
  {Interprets data screen -- related to effects of keys}
   VAR
      KeyboardHelpScreen : HelpScrType;
      speed    : REAL;
   BEGIN
      KeyboardHelp(KeyboardHelpScreen);
      With KeyboardDataScreen  DO
      Begin
      init;
      DefineInputPort(0.13, 0.87, 0.10, 0.90);
      LoadLine('');
      LoadLine(  '   ARROW KEY SENSITIVITY: ');
      LoadLine(  '   Acceleration           :    {     }  (Up/Down Keys) ');
      LoadLine(  '   Turning Rate Increment :    {     }  (Right/Left Keys) ');
      LoadLine('');
      LoadLine(  '   TIME BETWEEN WAVEFRONTS (Line smoothness & speed)');
      LoadLine(  '      dtime (> 0.01 )        = {     } ');
      LoadLine('');
      LoadLine(  '   INITIAL CHARGE POSITION AND MOTION:');
      LoadLine(  '      X position   =     {    }   (Note: Charge must ');
      LoadLine(  '      Y position   =     {    }    be on the screen.)');
      LoadLine(  '      X velocity   =     {    }    (Speed must be slower');
      LoadLine(  '      Y velocity   =     {    }    than c, i.e. < 1.00 )');
      LoadLine(  '    Turning Rate   =     {    }    Radians/Light-cm');
      LoadLine(  ' ');
      LoadLine('          [  Ok  ]       [Cancel]       [ Help ]');
      SetNumber(1, Accel);
      SetNumberLimits(1, 0., 0.25);
      SetNumber( 2, TurnInc);
      SetNumberLimits(2, 0., 0.50);
      SetNumber( 3, dtime);
      SetNumberLimits(3, 0., 1.0);
      SetNumberLimits(4, -0.8*TheScaleSize, 0.8*TheScaleSize);
      SetNumberLimits(5, -0.8*TheScaleSize, 0.8*TheScaleSize);
      SetNumber( 4, xstart);
      SetNumber( 5, ystart);
      SetNumber( 6, vxstart);
      SetNumber( 7, vystart);
      SetNumberLimits(6, -0.99, 0.99);
      SetNumberLimits(7, -0.99, 0.99);
      SetNumber( 8, Turn);
      SetNumberLimits(8,-Pi, Pi);
      SetHelpScreen(KeyBoardHelpScreen);
      Accept;
      Accel := GetNumber(1);
      TurnInc := GetNumber(2);
      dtime := GetNumber(3);
      Turn := Turn * dtime;
      xstart := GetNumber(4);
      ystart := GetNumber(5);
      vxstart := GetNumber(6);
      vystart := GetNumber(7);
      speed := sqrt(sqr(vxstart)+sqr(vystart));
      IF speed > 0.99 THEN
         BEGIN
            vxstart:= vxstart*0.99/speed;
            vystart:= vystart*0.99/speed
         END;
      Turn:= GetNumber(8);
      End;
    {Adjust initial values to put in allowable range.}
      IF ABS(xstart) > TheScaleSize THEN
         xstart := 0.9 * TheScaleSize * xstart / ABS(xstart);
      IF ABS(ystart) > TheScaleSize THEN
         ystart := 0.9 * TheScaleSize * ystart / ABS(ystart);
      IF (SQR(vxstart) + SQR(vystart)) > 0.98 THEN
      BEGIN
         IF ABS(vxstart) > Macheps THEN
            vxstart := 0.70 * vxstart / ABS(vxstart);
         IF ABS(vystart) > Macheps THEN
            vystart := 0.70 * vystart / ABS(vystart);
      END;
   END; {GetKeyboard}

   Procedure DrawArrow(x,y,dirx,diry: real; itscolor: word);
   BEGIN
   SetColor(itscolor);
   PlotLine(x, y, x + dirx, y + diry);
   PlotLine(x+dirx, y+diry, x + 0.5*diry, y - 0.5*dirx);
   PlotLine(x+dirx, y+diry, x - 0.5*diry, y + 0.5*dirx);
   END;


   Procedure OffScreenCorrection(VAR ElectronX, ElectronY, ElecVelX, ElecVelY :REAL);
   BEGIN
      IF (abs(ElectronX + ElecVelX) > TheScaleSize) THEN
         BEGIN
            ElectronX := ElectronX + ElecVelX
              - 2*TheScaleSize*ElectronX/ABS(ElectronX);
            {StopAnimation := TRUE;}
            JustDisplaced := TRUE
         END;
      IF (abs(ElectronY + ElecVelY) > TheScaleSize) THEN
         BEGIN
            ElectronY := ElectronY + ElecVelY
              - 2*TheScaleSize*ElectronY/ABS(ElectronY);
            {StopAnimation := TRUE;}
            JustDisplaced := TRUE
         END;

   END;


   PROCEDURE DoAnimation(ShowCredits:BOOLEAN);
 {Interactive control of the moving charge}
      VAR
         ElectronX,                     {Keeping track of electron motions   }
         ElectronY, ElecVelX, ElecVelY, Velocity,
         Angle  : Real;
         EnoughElecPos: Boolean; {keeping track of previous           }
         ElecListStart,                 {charge positions                  }
         ElecListEnd: ElecOldPositionsPtr;
         FrameCount,
         ElecCount, MaxElecCount: Integer;
         Printout   : Array[1..10] of String;

   BEGIN
      CloseGraph;
      CUPSdone;
      Frame.Init;
      CUPSinit;
      IF Mouson THEN ShowCursor ELSE HideCursor;
      HotKeys.Display;
      IF Mouson THEN MouseBorders(0,GetMaxY,GetMaxX,Round(0.95*GetMaxY))
         ELSE  MouseBorders(0,ROUND(0.05*GetMaxY),GetMaxX,GetMaxY);
      GraphBackColor := Blakk;
      IF ShowKeybdInfo THEN
      BEGIN
         DefineViewPort(1, 0.26,0.98, 0.11, 0.99);
         DefineViewPort(2, 0.0, 0.21, 0.5, 0.99);
         GraphBackColor := DarkGray;
         OpenViewPort(2);
         Print(3, 1, 'X =');
         Print(3, 2, 'Y =');
         Print(3, 3, 'V =');
         Print(2, 4, 'Time per frame:');
         Print(3, 6, 'Turning at');
         Print(8, 7, 'radians');
         Print(1, 8, ' per light-cm.');
         DefineViewPort(3, 0.0, 0.21, 0.11,0.47);
         GraphBackColor := Blue;
         OpenViewPort(3);
         Print(2, 1, CHR(24)+' & '+CHR(25)+' change');
         Print(2, 2, 'velocity;');
         Print(2, 3, CHR(27)+' & '+CHR(26)+' change');
         Print(2, 4, 'turning rate;');
         Print(2, 5, 'spacebar turns');
         PRINT(2, 6, 'steer ON/OFF.');
         GraphBackColor := Blakk
      END
      ELSE
      DefineViewPort(1, 0.14, 0.85, 0.10, 0.99);
      OpenViewPort(1);
      DefineScale(1, -TheScaleSize, TheScaleSize, -TheScaleSize, TheScaleSize);
      SelectScale(1);
      Setcolor(Whitte);
      Axis(-TheScaleSize, -TheScaleSize, 5, 5);
      Frame.Open(1);
      FrameCount := 0;
      ElectronX := xstart;  {Initialize charge motion.}
      ElectronY := ystart;
      ElecVelX := vxstart;
      ElecVelY := vystart;
      Velocity := SQRT(SQR(vxstart) + SQR(vystart));
      Angle := 0.0;
      IF (Velocity > 2 * Macheps) THEN
         IF ABS(vxstart) > Macheps THEN
            Angle := ArcTan2(vxstart, vystart)
         ELSE
            angle := 0.5 * PI * vystart / ABS(vystart);
      EnoughElecPos := FALSE;
      ElecCount := 0;
      FieldLineListStart := NIL;
      ElecListStart := NIL;
      ElecListEnd := NIL;

      MainInitialize(FieldLineListStart, FieldLineCursor, CircleList,
                     NumInGeneration);

 {The next few lines ensure that the ellipses drawn by the}
 {WaveFront procedures will be to the correct scale.}

      GetViewSettings(ViewPortTester);
      ScaledX := (ViewPortTester.x2 - ViewPortTester.x1);
      ScaledY := (ViewPortTester.y2 - ViewPortTester.y1);
      ScaledX := ScaledX / (2 * TheScaleSize);
      ScaledY := ScaledY / (2 * TheScaleSize);
      Turning := TRUE;


      REPEAT
         SetColor(ElectronColor);
         StopAnimation := FALSE;
         JustDisplaced := FALSE;
         UpdateElectron(EnoughElecPos, Turning, Turn,
                        Angle, Velocity, dtime, ElecVelX, ElecVelY,
                        ElectronX, ElectronY, TurnInc, AccelInc, AngleInc,
                        VelocInc, AxisVelInc, ElecListStart, ElecListEnd,
                        ElecCount, TrailLength);
         IF ABS(Velocity) > 0 THEN
         DrawArrow(ElectronX, ElectronY,
                   0.075*TheScaleSize*ElecVelX/ABS(Velocity),
                   0.075*TheScaleSize*ElecVelY/ABS(Velocity),
                   ElectronColor);

         OffScreenCorrection(ElectronX, ElectronY, ElecVelX, ElecVelY);
         IF JustDisplaced THEN
         BEGIN
            MainDestroy(FieldLineListStart, CircleList);
            EnoughElecPos := FALSE;
            ElecCount := 0;
            FieldLineListStart := NIL;
            ElecListStart := NIL;
            ElecListEnd := NIL;
            MainInitialize(FieldLineListStart, FieldLineCursor, CircleList,
                     NumInGeneration);
         END;

         IF ShowKeybdInfo THEN
         BEGIN
            SelectViewPort(2);
            SetColor(Whitte);
            PrintOut[1] :=  NumStr(dtime, 6, 3);
            Print(5, 5,  PrintOut[1]);
            PrintOut[2] :=  NumStr(ElectronX, 6, 2);
            Print(7, 1, PrintOut[2]);
            PrintOut[3] :=  NumStr(ElectronY, 6, 2);
            Print(7, 2, PrintOut[3]);
            PrintOut[4] :=  NumStr(Velocity, 6, 3);
            Print(7, 3,  PrintOut[4]);
            {PrintOut[5] :=  NumStr(dtime, 6, 3);
            Print(5, 5, PrintOut[5]);}
            IF Turning THEN
               Print(1, 7, NumStr(Turn, 6, 3))
            ELSE
               Print(1, 7, NumStr(0, 6, 3));
            SelectViewPort(1)
         END;

         IF DrawCircles THEN
         BEGIN
            SetColor(WaveFrontColor);
            CircleList.Insert(ElectronX, ElectronY);
            CircleList.UpdateList(TheScaleSize, dtime);
            CircleList.DrawCircles(ScaledX, ScaledY)
         END;

         IF DrawLines OR PlotPoints THEN
         BEGIN
            FieldLineCursor := FieldLineListStart;
            WHILE NOT (FieldLineCursor = NIL) DO
            BEGIN
               FieldLineCursor^.data.GiveBirth(ElectronX, ElectronY,
                                               Velocity, Angle);
               FieldLineCursor^.data.UpdateLine(TheScaleSize, dtime);
               IF PlotPoints THEN
               BEGIN
                  SetColor(PointColor);
                  FieldLineCursor^.data.PlotPoints
               END;
               IF DrawLines THEN
               BEGIN
                  SetColor(FieldLineColor);
                  FieldLineCursor^.data.DrawLine(TheScaleSize, ElectronX,
                                                               ElectronY)
               END;
               FieldLineCursor := FieldLineCursor^.Next
            END

         END;
         Frame.SwitchAndClean;
         IF ShowKeybdInfo THEN
            BEGIN
               SelectViewPort(2);
               RubOut(7, 1,6, DarkGray);
               RubOut(7, 2, 6, DarkGray);
               RubOut(7, 3, 6, DarkGray);
               RubOut(5, 5, 6, DarkGray);
               RubOut(1, 7, 6, DarkGray);
               SelectViewPort(1);
            END;
         inc(Framecount);
         IF ShowCredits AND (Framecount = 55) THEN
            BEGIN
               Frame.Pause;
               Credits;
               ShowCredits := FALSE;
            END;
      UNTIL StopAnimation;
      MainDestroy(FieldLineListStart, CircleList);
      Frame.Close;
      GraphMode := VGAhi;
      CloseGraph;
      CUPSdone;
      CUPSinit
   END;  {DoAnimation Procedure}

   Procedure MainHelp;
      Var
         screen: HelpScrType;
   Begin
      screen[1] := '                                                  ';
      screen[2] := '* This is an animated display of the electric     ';
      screen[3] := '  field generated by a moving electric charge,    ';
      screen[4] := '  such as an electron.                            ';
      screen[5] := '                                                  ';
      screen[6] := '* The charge itself is represented by the moving  ';
      screen[7] := '  point from which the electric field lines       ';
      screen[8] := '  emanate.  A trail of dots represents the        ';
      screen[9] := '  charge position at previous times.  An arrow    ';
      screen[10] := '  shows the current direction of its motion.      ';
      screen[11] := '                                                  ';
      screen[12] := '* You can control the motion of the charge by     ';
      screen[13] := '  using the arrow keys, either to change the speed';
      screen[14] := '  or to "steer".  Steering is accomplished via    ';
      screen[15] := '  arrow keys that "turn" the steering wheel       ';
      screen[16] := '  right or left in increments.  Speed changes     ';
      screen[17] := '  are accomplished by arrow keys that change the  ';
      screen[18] := '  tangential acceleration.                        ';
      screen[19] := '                                                  ';
      screen[20] := '* Data input screens allow you to change what is  ';
      screen[21] := '  displayed and how to display it, as well as     ';
      screen[22] := '  to vary the increments in acceleration or in    ';
      screen[23] := '  turning that result from the arrow keys.        ';
      screen[24] := '                                                  ';
      screen[25] := '    <<Mouseclick or any key to continue>>         ';
      help(screen);
   End;

   Procedure UnitsHelp;
      Var
         screen: HelpScrType;
   Begin
      screen[1] := '                                                  ';
      screen[2] := '* You can think of the electric charge in this    ';
      screen[3] := '  simulation as being either positive (a proton)  ';
      screen[4] := '  or negative (an electron).  The electric field  ';
      screen[5] := '  lines are directed away from a positive charge  ';
      screen[6] := '  and toward a negative one.  The magnitude of the';
      screen[7] := '  electric field is proportional to the size of   ';
      screen[8] := '  the charge, and to the flux (density of lines)  ';
      screen[9] := '  but the proportionality constant is arbitrary.  ';
      screen[10] := '                                                  ';
      screen[11] := '* Units of distance and time are chosen so that   ';
      screen[12] := '  the speed of light is unity (c=1).  For example,';
      screen[13] := '  time might be in years and distance in light-   ';
      screen[14] := '  years, but it is probably better to use cm. for ';
      screen[15] := '  distance and light-cm. (3.33 E-11 sec.) as the  ';
      screen[16] := '  unit of time.  The latter choice would make the ';
      screen[17] := '  scale on the computer screen realistic, but the ';
      screen[18] := '  animation is then some eleven orders of         ';
      screen[19] := '  magnitude slower than "real time."              ';
      screen[20] := '                                                  ';
      screen[21] := '* Better yet, think of the distance in Angstroms, ';
      screen[22] := '  and let the unit of time be the one light-      ';
      screen[23] := '  Angstrom -- the time for light to cross the     ';
      screen[24] := '  hydrogen atom.                                  ';
      screen[25] := '                                                  ';
      help(screen);
   End;

   PROCEDURE Information;
   BEGIN
      DefineViewPort(3, 0.10, 0.90, 0.15, 0.85);
      GraphBackColor := Blue;
      OpenViewPort(3);
      SelectViewPort(3);
      SetColor(Whitte);
      Print(1, 2, '          ELECTRIC FIELD OF A MOVING CHARGE');
      Print(1, 3, '            by Ron Stoner and Greg Meyers');
      Print(1, 4, '   ********************* MENU CHOICES **********************      ');
      Print(6, 21, '                      *******************                   ');
      Print(1, 6, '   * FILE    -- ');
      Print(1, 8, '   * MODIFY  -- ');
      Print(1, 13, '   * ANIMATE -- ');
      Print(1, 15, '   * EXPLAIN -- ');
      SetColor(Yellow);
      Print(18, 6, 'For general information or EXIT the program.');
      Print(18, 13, 'To restart the animation.');
      Print(18, 8, 'Data screens for program parameters.');
      Print(18, 15, 'For help or instructions.');
      SetColor(LightGreen);
      Print(5, 9, '>> Animation Data  -- Motion/control parameters. <<');
      Print(5,10, '>> Color Choices   -- Change colors in display.  <<');
      Print(5,11, '>> Display Options -- Decide what to show.       <<');
   END;

Procedure HandleMyMenu;
BEGIN
   With MyMenu DO
      BEGIN
         CASE ColChosen OF

            1:
               Case RowChosen Of
                  1:
                     AboutCUPS;
                  2:
                     Credits;
                  3:
                     Configuration;
                  4:
                     QuitFlag := true;
               End; {Case RowChosen}

            2:
            BEGIN
               If Rowchosen = 2 THEN
                  BEGIN
                     OptionsDefaults(TheScaleSize, NumInGeneration,
                                     TrailLength,  PlotPoints, DrawLines,
                                     DrawCircles, ShowKeybdInfo);
                     DefaultColors(ElectronColor, PointColor,
                                   FieldLineColor, WaveFrontColor);
                     KeyboardDefaults(AccelInc, TurnInc, dtime);
                  END;
               DoAnimation(FALSE);
               ClearMuppetPort;
               MyMenu.display;
               Information;
            END;

            3:
               CASE RowChosen OF

                  1: 
                  BEGIN
                     GetKeyBoard(AccelInc, TurnInc, dtime, Turn);
                     ClearMuppetPort;
                     display;
                     Information;
                  END;

                  2: 
                  BEGIN
                     GetColors(ElectronColor, PointColor,
                               FieldLineColor, WaveFrontColor);
                     ClearMuppetPort;
                     display;
                     Information;
                  END;

                  3: 
                  BEGIN
                     GetDisplayOptions(TheScaleSize, NumInGeneration,
                                       TrailLength,  PlotPoints,
                                       DrawLines, DrawCircles, ShowKeybdInfo);
                     ClearMuppetPort;
                     display;
                     Information;
                  END;
                  {4: BEGIN
                     OptionsDefaults(TheScaleSize, NumInGeneration,
                                     TrailLength,  PlotPoints, DrawLines,
                                     DrawCircles, ShowKeybdInfo);
                     DefaultColors(ElectronColor, PointColor,
                                   FieldLineColor, WaveFrontColor);
                     KeyboardDefaults(AccelInc, TurnInc, dtime);
                     END;}
               END; {Case RowChosen}

            4: Case RowChosen of
                  1: MainHelp;
                  2: Instructions;
                  3: UnitsHelp
               END;


      END; {Case ColChosen}
   END;
END;

 {******************** Start of Main Program *****************************}

BEGIN {Main Program}
   CUPSinit;
   {Credits;}
   ClearMUPPETPort;
   OptionsDefaults(TheScaleSize, NumInGeneration,  TrailLength,
        PlotPoints, DrawLines, DrawCircles, ShowKeybdInfo);
   DefaultColors(ElectronColor, PointColor, FieldLineColor, WaveFrontColor);
   KeyboardDefaults(AccelInc, TurnInc, dtime);
   SetupMyMenu;
   SetUpHotKeys;
   DoAnimation(TRUE);
   ClearMuppetPort;
   MyMenu.Display;
   Information;
   REPEAT
      CheckForEvents;
      IF MyMenu.Activated THEN  HandleMyMenu;
   UNTIL QuitFlag;
   ClearMUPPETPort;
   CUPSdone;
END.