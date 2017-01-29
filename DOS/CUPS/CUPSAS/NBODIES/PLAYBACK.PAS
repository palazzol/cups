    (*******************************************************)
    (*                                                     *)
    (*       J.M.Anthony Danby.   1st July, 1993.          *)
    (*             Department of Mathematics,              *)
    (*          North Carolina State University.           *)
    (*                                                     *)
    (*       Program to play back data from a file         *)
    (*       set by TwoGalax, Astroids, N-Bodies or        *)
    (*       Planets,                                      *)
    (*                                                     *)
    (*******************************************************)

unit Playback;

{$M 64000,0,327680}
{$O+}
{$F+}
interface
procedure RunPlayback;

implementation
USES Crt, Graph, CUPSMupp, CUPS, Cupsgui;

procedure RunPlayback;
TYPE
   PointRecord = Record
                    x, y, z : integer;
                 END;

VAR
   Filename: String;
   F1: File of PointRecord;
   Points: PointRecord;
   nS: Integer;  {Number of stars.}
   vx, vy, vz: real; {Components of a vector in the line of sight.}
   f: real;
   Lag: integer;   {Delay time between frames in milliseconds.}
   PlayBackMenu: TMenu;
   Picture: TFrames;
   Action, PlotOrbit, DataOK, FileOK: Boolean;
   HotKeys: THotkeys;
   Key: Byte;
   FirstPage: Boolean;
   Altitude, Azimuth, Radius: Real;
   Calt, Salt, Caz, Saz: Real;
   xMin, xMax, yMin, yMax: Real;
   SplitScreen: Boolean;
   FileSelected: Boolean;
   FileOpened : Boolean;
   ShowGalaxy: Boolean;
   ExitRequest: Boolean;
   LightMenu: Boolean;

PROCEDURE HandlePlayBackMenu; Forward;

{------------------------------------------------------------------------}

PROCEDURE SetUpPlayBackMenu;
BEGIN
   With PlayBackMenu DO
   BEGIN
      Init;
      Column(1, 'File');
       row(1,1, 'About CUPS');
       row(1,2, 'About the program');
       row(1,3, 'Configuration');
       row(1,4, 'Open file');
       row(1,5, 'Leave this module');
      Column(2, 'Playback');
       row(2,1, 'Single screen');
       row(2,2, 'Split screens');
      RowActivate(2,1, False);
      RowActivate(2,2, False);
   END;
   PlayBackMenu.Display;
END;

PROCEDURE SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      HotKeys.Init(6);
      Key[1] := 'F2-Stop';
      Key[2] := 'RIGHT-Right';
      Key[3] := 'LEFT-Left';
      Key[4] := 'UP-Up';
      Key[5] := 'DOWN-Down';
      Key[6] := 'F10-Menu';
   END;
   HotKeys.BackColor := Blue;
   HotKeys.HotKeyColor := Yellow;
   HotKeys.Display;
END;


{-------------------------------------------------------------------------}

PROCEDURE HandleHotKeys(key: Byte; ActionIn: Boolean; VAR PlotOrbit, Action: Boolean);
CONST
   Increment = Pi/36;
VAR
   DelLag: Integer;
BEGIN
   DelLag := 20;
   CASE Key OF
      1: BEGIN
            IF ActionIn = True THEN
            BEGIN
               Action := False;
               WITH HotKeys DO Key[1] := 'F2-Run';
               HotKeys.BackColor := Red;
               Picture.Pause;
               HotKeys.Display;
               {Picture.Switch;}
            END
            ELSE
            BEGIN
               Action := True;
               WITH HotKeys DO Key[1] := 'F2-Stop';
               HotKeys.BackColor := Blue;
               HotKeys.Display;
               Picture.Continue;
            END;
         END;
 
      2: Azimuth := Azimuth + Increment;
      3: Azimuth := Azimuth - Increment;
      4: Altitude := Altitude + Increment;
      5: Altitude := Altitude - Increment;
      6: BEGIN
            PlotOrbit := False;
            Picture.Close;
            LightMenu := True;
            ShowMouse;
         END;
   END;
END;



Procedure SetUpProjection;
VAR
   Norm: real;
BEGIN
   Norm := Sqrt(vx*vx + vy*vy + vz*vz);
   vx := vx/Norm; vy := vy/Norm; vz := vz/Norm; {Change to a unit vector.}
   f := Sqrt(1 - vz*vz);
END;

{------------------------------------------------------------------------}

Procedure Project(x, y, z: real; VAR xProj, yProj: real);
BEGIN
   IF f > 0.01 THEN
   BEGIN
      xProj := (y*vx - x*vy)/f;
      yProj := (z - vz*(x*vx + y*vy + z*vz))/f;
   END
   ELSE
   BEGIN
      xProj := x; yProj := y;
   END;
END;

{------------------------------------------------------------------------}

PROCEDURE TrigFunctions;
BEGIN
   Calt := Cos(Altitude);
   Salt := Sin(Altitude);
   Caz := Cos(Azimuth);
   Saz := Sin(Azimuth);
END;

PROCEDURE View(x, y, z: Real; VAR XView, YView: Real);
BEGIN
   XView := (x*Caz - z*Saz);
   YView := (y*Calt - (z*Caz + x*Saz)*Salt);
END;

{------------------------------------------------------------------------}

PROCEDURE ProjectAxes;
VAR
   dx, dy, xOrigin, yOrigin, xEnd, yEnd: Real;
   xSc, ySc: Integer;
BEGIN
   SetColor(White);
   dx := (xMax-xMin)/10;
   dy := (yMax-yMin)/4;
   xOrigin := xMax - 2*dx;
   yOrigin := yMax - dy;
   View(dx, 0, 0, xEnd, yEnd);
   xEnd := xOrigin + xEnd;
   yEnd := yOrigin + yEnd;
   PlotLine(xOrigin, yOrigin, xEnd, yEnd);
   View(1.1*dx, 0, 0, xEnd, yEnd);
   Map(xOrigin + xEnd, yOrigin + yEnd, xSc, ySc);
   OutTextxy(xSc, ySc, 'X');
   View(0, dx, 0, xEnd, yEnd);
   xEnd := xOrigin + xEnd;
   yEnd := yOrigin + yEnd;
   PlotLine(xOrigin, yOrigin, xEnd, yEnd);
   View(0, 1.1*dx, 0, xEnd, yEnd);
   Map(xOrigin + xEnd, yOrigin + yEnd, xSc, ySc);
   OutTextxy(xSc, ySc, 'Y');
   View(0, 0, dx, xEnd, yEnd);
   xEnd := xOrigin + xEnd;
   yEnd := yOrigin + yEnd;
   PlotLine(xOrigin, yOrigin, xEnd, yEnd);
   View(0, 0, 1.1*dx, xEnd, yEnd);
   Map(xOrigin + xEnd, yOrigin + yEnd, xSc, ySc);
   OutTextxy(xSc, ySc, 'Z');
END;

{-------------------------------------------------------------------------}

Procedure StarPlot(xg, yg: real; GalColor: integer);
var x,y : integer;
BEGIN
   Map(xg,yg,x,y);
   PutPixel(x,y,galColor);
   PutPixel(x+1,y,galColor);
   PutPixel(x-1,y,galColor);
   PutPixel(x,y+1,galColor);
   PutPixel(x,y-1,galColor);
END;

{------------------------------------------------------------------------}

Procedure MakeScreen;
VAR
   n: integer;
   x, y: real;
   StarColor: Word;
BEGIN
   HideCursor;
   Delay(Trunc(Lag));
   TrigFunctions;
   IF NOT SplitScreen THEN OpenViewPort(1)
   ELSE
   BEGIN
      OpenViewPort(3);
      OpenViewPort(4);
   END;
  { PlayBackMenu.Display;}
   HotKeys.Display;
   IF SplitScreen THEN SelectViewPort(4);
   ProjectAxes;
   IF ShowGalaxy = True THEN
   BEGIN
      IF SplitScreen = True THEN
      BEGIN
         SelectViewPort(4);
         PlotSymbol(0, 0, '0');
         SelectViewPort(3);
         PlotSymbol(0, 0, '0');
      END
      ELSE
      PlotSymbol(0, 0, '0');
      Read(F1, Points);
      View(Points.x/1000, Points.y/1000, Points.z/1000, x, y);
      SetColor(6);
      IF SplitScreen = True THEN
      BEGIN
         SelectViewPort(4);
         PlotSymbol(x, y, 'o');
         SelectViewPort(3);
         PlotSymbol(Points.x/1000, Points.y/1000, 'o');
      END
      ELSE
      PlotSymbol(x, y, 'o');
   END;
   StarColor := 0;
   FOR n := 1 TO nS DO
   BEGIN
      StarColor := StarColor + 1; IF StarColor > 16 THEN StarColor := 1;
      Read(F1, Points);
      View(Points.x/1000, Points.y/1000, Points.z/1000, x, y);
      SetColor(StarColor);
      IF SplitScreen = True THEN
      BEGIN
         SelectViewPort(4);
         StarPlot(x, y, StarColor);
         {PlotSymbol(x, y, '.');}
         SelectViewPort(3);
         StarPlot(Points.x/1000, Points.y/1000, StarColor);
         {PlotSymbol(Points.x/1000, Points.y/1000, '.');}
      END
      ELSE
         StarPlot(x, y, StarColor);
         {PlotSymbol(x, y, '.');}
   END;
   ShowCursor;
END;

{-----------------------------------------------------------------------}

PROCEDURE AnimateMotion;
BEGIN
   OpenViewPort(1);
   HideMouse;
   Message('');
   LightMenu := False;
   SetUpHotKeys;
   assign(F1,FileName);
   Reset(F1);
   FileOpened := True;
   IF EOF(F1) THEN
   BEGIN
      Announce('This file is empty');
      Close(F1);
      FileSelected := False;
      FileOpened := False;
      HotKeys.Clear;
      LightMenu := True;
      Exit;
   END;
   Read(F1, Points);
   xMin := Points.x/1000;
   xMax := Points.y/1000;
   yMin := Points.z/1000;
   Read(F1, Points);
   yMax := Points.x/1000;
   nS := Trunc(Points.y + 0.0001);
   IF Points.z > 0 THEN ShowGalaxy := True ELSE ShowGalaxy := False;
   DefineScale(1, xMin, xMax, yMin, yMax);
   DefineScale(3, xMin, xMax, 1.5*yMin, 1.5*yMax);
   IF SplitScreen THEN SelectScale(3) ELSE SelectScale(1);
   FirstPage := True;
   PlotOrbit := True;
   Action := True;
END;   

{----------------------------------------------------------------------}

procedure FileOpen;
var
   NBDatPlayBack: TInputScreen;
begin
   Message('Press <ESC> to cancel');
   fileName := OpenFile('*.pos');
   if FileName <> '' then
   begin
      WITH NBDatPlayBack DO
      BEGIN
         init;
         DefineInputPort(0.25, 0.75, 0.4, 0.6);
         LoadLIne(' Time lag between frames: {     } ms');
         LoadLine('');
         LoadLIne('       [  OK  ]     [Cancel]');
         SetNumber(1, Lag);
         SetNumberLimits(1, 1, 10000);
         AcceptScreen;
         Lag := Trunc(GetNumber(1) + 0.001);
         if canceled then exit;
         Done;
      END;
      fileSelected := true;
      IF FileSelected THEN
      BEGIN
         PlayBackMenu.RowActivate(2,1, True);
         PlayBackMenu.RowActivate(2,2, True);
      END;
   end; IF PLayBackMenu.Chosen THEN HandlePlayBackMenu;
end;

{----------------------------------------------------------------------}

PROCEDURE HandlePlayBackMenu;
BEGIN
   With PlayBackMenu DO
    CASE ColChosen OF
       1: CASE RowChosen OF
             1: BEGIN
                   AboutCUPS;
                   LightMenu := True;
                END;
             2: BEGIN
                   ShowHelp('HELPNbod.HLP', 'Playback Hello');
                   LightMenu := True;
                END;
             3: BEGIN
                   Configuration;
                   LightMenu := True;
                END;
             4: FileOpen;
             5: ExitRequest := True;
          END;
       2: begin
             IF FileSelected THEN
             BEGIN
                SplitScreen := (RowChosen=2);
                AnimateMotion;
             END
             ELSE Announce('Please open a file');
          end;
     END;
END;

{------------------------------------------------------------------------}

PROCEDURE Initialize;
BEGIN
   DefineViewPort(1, 0, 1, 0.06, 0.94);
   DefineViewPort(3, 0, 0.5, 0.05, 0.94);
   DefineViewPort(4, 0.5, 1, 0.05, 0.94);
   SplitScreen := False;
   FileSelected := False;
   FileOpened := false;
   ExitRequest := False;
   GraphBackColor := Black;
   Azimuth := 0;
   Altitude := 0;
END;

{------------------------------------------------------------------------}

BEGIN   {Main Program for PlayBack.}
   Picture.Init;
{  CUPSInit;}
   SetGraphMode(vgamed);
   ShowMouse;
   Initialize;
   ShowHelp('HELPNbod.HLP', 'Playback Hello');
   SetUpPlayBackMenu;
   LightMenu := True;
   Lag := 50;
   REPEAT
      CheckForEvents;
      IF Hotkeys.Pressed(Key) THEN
                  HandleHotKeys(Key, Action, PlotOrbit, Action);
      IF (FileOpened) AND (Action) AND (PlotOrbit) AND (NOT Eof(F1)) THEN
      BEGIN
         Picture.SwitchAndClean;
         MakeScreen;
      END;
      IF (NOT PLotOrbit) AND (FileOpened) THEN
      BEGIN
         Picture.Close;
         Close(F1);
         FileSelected := False;
         FileOpened := False;
         LightMenu := True;
         ShowMouse;
      END;
      IF (FileOpened) AND Eof(F1) THEN
      BEGIN
         Picture.Close;
         Close(F1);
         Action := False;
         PlotOrbit := False;
         FileSelected := False;
         FileOpened := False;
         LightMenu := True;
         ShowMouse;
      END;
      IF LIghtMenu THEN
      BEGIN
         IF PLayBackMenu.Chosen THEN HandlePlayBackMenu;
      END
      ELSE
         IF PlayBackMenu.Activated THEN HandlePlayBackMenu;
   UNTIL  ExitRequest;
   IF (FileOpened) THEN Close(F1);
{  CUPSDone;}
   PlayBackMenu.done;
   SetGraphMode(vgahi);
END;

END.




