                 (**********************************************)
                 (**********************************************)
                 (**                                          **)
                 (**   Unit CUPSgui - Graphics User Interface **)
                 (**   Written by Jarek Tuszynski             **)
                 (**   for CUPS Project                       **)
                 (**   Department of Physics and Astronomy    **)
                 (**   George Mason University                **)
                 (**   Fairfax, Virginia 22030                **)
                 (**   (c) 1994 by Jarek Tuszynski            **)
                 (**   Originated:  91/02/28                  **)
                 (**   Version: 1.3 (94/11/14)                **)
                 (**                                          **)
                 (**********************************************)
                 (**********************************************)



{M 65520, 0, 65536}
UNIT CUPSgui;

INTERFACE
USES Graph, CUPSmupp, CUPSscrn, CUPS;

{----------------------------- Mouse Related -------------------------------}
TYPE
  FigureType = PROCEDURE(a,b: PointType);

PROCEDURE DrawRubberBand(figure: FigureType; VAR a,b: PointType;
                           BkColor: integer; XORmode: BOOLEAN);
FUNCTION  OpenFile(path : string) : string;
PROCEDURE Configuration;
PROCEDURE ReadDelayTime;

{------------------------------- HotKeys ------------------------------------}

TYPE
   THotKeys = object
      key    : ARRAY [1..12] OF string[16];
      active : ARRAY [1..12] OF boolean;
      BackColor, TextColor, HotKeyColor, DisactivateColor : integer;
      PROCEDURE Init(num_: integer);
      PROCEDURE Display;
      FUNCTION  Pressed( VAR knum : byte) : boolean;
      PROCEDURE Clear;
      PROCEDURE Done;
   PRIVATE
      code: ARRAY[1..12] OF byte;
      num : byte;
   END;

{--------------------------- Pull-Down Menus ----------------------------}

   str35 = string[35];
   RowPtr = ^RowNode;
   RowNode = RECORD
      name : str35;
      active,checked : boolean;
      next : RowPtr;
   END;
   TMenu  =  object
      colChosen,rowChosen : byte;
      BackColor, TextColor, GrayOutColor, HiGrayOutColor : integer;
      PROCEDURE Init;
      PROCEDURE Column(c : integer; name : str35);
      PROCEDURE Row( c,r : integer; name : str35);
      PROCEDURE RowActivate(c,r : integer; flag : boolean);
      PROCEDURE RowChecked( c,r : integer; flag : boolean);
      PROCEDURE AutoChecking( c,rInitial : integer);
      PROCEDURE Display;
      FUNCTION  Chosen    : boolean;
      FUNCTION  Activated : boolean;
      PROCEDURE Done;
   PRIVATE
      col : ARRAY[1..7] OF RowPtr;
      terminator : RowPtr;
      Initialized : boolean;
      Choice, Size, Width  : ARRAY[0..7] OF byte;
      PROCEDURE print( num : byte);
      PROCEDURE MakeBox( x1,y1,x2,y2,FillColor: integer);
   END;

{------------------------------ Bar Menus ----------------------------------}

   TBarMenu = object
      Row : ARRAY [1..15] OF string[80];
      BackColor, TextColor : integer;
      PROCEDURE Init;
      PROCEDURE Draw(x,y : real; numRows, Initial : integer);
      FUNCTION  Chosen : integer;
   PRIVATE
      size, previous, num, current : integer;
      p : pointType;
      PROCEDURE ReDraw;
      PROCEDURE MakeBox( x1,y1,x2,y2,FillColor: integer);
   END;

{------------------------------ Sliders ----------------------------------}

   TSlider = object
      chosen,changed : boolean;
      step : real;
      CONSTRUCTOR Create(ValMin, ValMax, VInitial, posX, posY, size_ : real;
         decimals_: integer; Low_,High_,Name_ : string; vertical_ : boolean);
      PROCEDURE Draw;
      PROCEDURE ReDraw;
      FUNCTION  Value : real;
      PROCEDURE Reset(VCurrent_: real);
      PROCEDURE Erase(color : integer);
      DESTRUCTOR done;
   PRIVATE
      active : boolean;
      VMin, VMax, VCurrent : real;
      px, py, size, decimals,NumWidth,NumLength : integer;
      vertical,bareSlider : boolean;
      Low,High,Name : string[20];
      putText : pointType;
      MainBox,SliderBox,TextBox,ThumbBox : rectType;
      PROCEDURE ReadNumber;
   END;

   SliderTypePtr = ^SliderType;
   SliderType = record
      Slider  : TSlider;
      current : real;
      num     : integer;
      next    : SliderTypePtr;
   end;
   TSliders = object
      PROCEDURE init;
      PROCEDURE Create(num : integer; ValMin, ValMax, VInitial, posX, posY,
                size : real; decimals: integer;
                Low,High,Name : string; vertical : boolean);
      PROCEDURE DrawAll;
      PROCEDURE Draw(num : integer);
      FUNCTION  Changed : boolean;
      FUNCTION  Value(num : integer) : real;
      FUNCTION  LastAltered : integer;
      PROCEDURE Delete(num : integer);
      PROCEDURE Reset(num : integer; VCurrent_ : real);
      PROCEDURE Erase(num, color : integer);
      PROCEDURE SetStepSize(num : integer; step : real);
      PROCEDURE Done;
   PRIVATE
      root,active : SliderTypePtr;
   END;

{-------------------------- TInputScreen ---------------------------------}
{Object TDialogScreen is definded in CUPSscrn unit.}
 
   {Please treat TInputScreen as if it was definded as :}
   TInputScreen = object(TDialogScreen)
      {CONSTRUCTOR init;}
      {PROCEDURE SetHelpScreen(VAR HelpScr_: HelpScrType);}
      {PROCEDURE SetHelpFile(HelpFileName_,HelpScrName_ : string);}
      {PROCEDURE DefineInputPort( X1, X2, Y1, Y2: REAL);}
      {PROCEDURE LoadLine(str : string);}
      {PROCEDURE SetNumLimits( n : integer; min,max  : real);}
      {PROCEDURE SetNumber( n : integer; num  : real);}
      {PROCEDURE SetString( n : integer; str  : string);}
      {PROCEDURE SetBoolean( n : integer; bool : boolean);}
      {PROCEDURE SetRadioButton(ID : char; num : integer);}
      {PROCEDURE Accept;}
      {PROCEDURE AcceptScreen;}
      {FUNCTION  GetNumber( n : integer) : real;}
      {FUNCTION  GetString( n : integer) : string;}
      {FUNCTION  GetBoolean( n : integer) : boolean;}
      {FUNCTION  GetRadioButton(ID : char) : integer;}
      {PROCEDURE ClearInputPort;}
      {FUNCTION  Canceled : boolean;}
      {DESTRUCTOR done;}
   END;

{------------------------------- TButtons ---------------------------------}

   ButtonTypePtr = ^ButtonType;
   ButtonType = RECORD
      name     : string;       {text on the button}
      x,y,num  : integer;
      active   : boolean;
      next : ButtonTypePtr;
   END;

   TButtons = object
      number: integer;
      PROCEDURE init;
      PROCEDURE create(num : integer; x,y : real; name : string);
      PROCEDURE DrawAll;
      PROCEDURE Draw(num : integer);
      FUNCTION  Changed : boolean;
      PROCEDURE Delete(num : integer);
      procedure Erase(num : integer; color : integer);
      PROCEDURE Done;
   PRIVATE
      root : ButtonTypePtr;
   END;

IMPLEMENTATION
USES Crt, Dos;

{---------------- Local constants, variables and procedures -----------------}

PROCEDURE Error(errorStr : string);
VAR i : integer;
BEGIN
   ErrorStr := 'Error: '+ErrorStr;
   i := GetGraphMode;
   IF GraphResult=0 THEN announce(errorStr) ELSE writeln(errorStr);
   PAUSE;
   HALT;
END;

PROCEDURE Swap(VAR x,y : real);
VAR t : real;
BEGIN
   t:=x; x:=y; y:=t;
END;

PROCEDURE SetRect(VAR r : rectType; x1,y1,x2,y2 : integer);
BEGIN
   r.x1:=x1; r.y1:=y1; r.x2:=x2; r.y2:=y2;
END;

PROCEDURE SetPoint(VAR p : PointType; x,y : integer);
BEGIN
   p.x:=x; p.y:=y;
END;

FUNCTION PtInside( p:pointType; r:rectType) : boolean;
BEGIN
    PtInside := (p.x>r.x1) AND (p.x<r.x2) AND (p.y>r.y1) AND (p.y<r.y2);
END;


PROCEDURE DrawBox( name : rectType; color : integer);
BEGIN
   SetFillStyle(solidFill,color);
   WITH name DO bar( x1,y1,x2,y2);
END;
{----------------------------- MouseRelated --------------------------------}

PROCEDURE DrawRubberBand(figure:FigureType; VAR a,b: PointType;
                           BkColor: integer; XORmode: BOOLEAN);
VAR
   x,y,button,color: integer;
   vp : ViewPortType;
BEGIN
   GetViewSettings(vp);
   IF NOT mousePresent THEN exit;
   REPEAT MouseGlobalPosn(x,y,button) UNTIL button<>0;
   color := GetColor;
   b := a;
   IF XORMode THEN
      BEGIN
         HideMouse;
         SetWriteMode(XORput);
         SetColor(color XOR BkColor);
         figure(a,b);
         ShowMouse;
         REPEAT
            HideMouse;
            figure(a,b);
            MouseGlobalPosn(x,y,button);
            b.x := x-vp.x1; b.y := y-vp.y1;
            figure(a,b);
            ShowMouse;
            delay(10)
         UNTIL button=0;
         HideMouse;
         figure(a,b);
         SetWriteMode(COPYput);
      END
   ELSE
      BEGIN
         REPEAT
            SetColor(BkColor);
            HideMouse;
            figure(a,b);
            MouseGlobalPosn(x,y,button);
            b.x := x-vp.x1; b.y := y-vp.y1;
            SetColor(color);
            figure(a,b);
            ShowMouse;
            delay(10);
         UNTIL button = 0;
         SetColor(BkColor);
         HideMouse;
         figure(a,b)
      END;
   ShowMouse;
   SetColor(color);
   ClearMKBuffers;
END;

{------------------------- Open File ---------------------------------------}
TYPE
   FNameTp = ARRAY [1..200] OF String[12];


FUNCTION DriveReady(DriveNum : byte) : boolean;
VAR Regs: registers;
BEGIN
   Regs.DL := DriveNum;
   Regs.AH := $32;
   Intr($21, Regs);
   DriveReady := (Regs.AL <> $FF);
END;

FUNCTION DriveExist(DriveNum : byte) : boolean;
VAR
   Regs     : registers;
   DriveStr : string[6];
   drive    : ARRAY[0..5] OF char;
   buffer   : pointer;
BEGIN
   GetMem(buffer,$25);                        {space for unopened FCB record}
   DriveStr  := concat( chr(DriveNum+64), ':*.*', chr(0) );
   move( DriveStr[1], drive, length(driveStr));  {fill array}
   Regs.DS := seg(drive);
   Regs.SI := ofs(drive);
   Regs.ES := seg(buffer^);
   Regs.DI := ofs(buffer^);
   Regs.AL := 1;
   Regs.AH := $29;                              {parse FileName}
   Intr($21, Regs);
   DriveExist := (Regs.AL <> $FF);
   FreeMem(buffer,$25);
END;

FUNCTION GetDrives : string;
VAR
   i : integer;
   drives : string;
BEGIN
   drives := '';
   FOR i := 1 TO 26 DO
    IF DriveExist(i) AND ((i=2) OR DriveReady(i)) THEN
     drives := drives + chr(i+64);
   GetDrives := drives;
END;

FUNCTION GetFile(VAR chosen : integer; VAR NRows : integer;
                 VAR Rows : FNameTp) : boolean;
VAR
   num,last,cx,cy,dx,dy,size : integer;
   found,escape              : boolean;
   slider                    : TSlider;
   bx,by,x                   : real;

   PROCEDURE PrintMenuBox(VAR num,chosen,last : integer);
   VAR
      j,lnum,c,l : integer;
   BEGIN
      SetViewPort( cx, cy, cx+dx, cy+dy, ClipOn);
      lnum := num;
      IF (chosen<num) THEN num:=chosen ELSE
      IF (chosen>=num+size) THEN num:=chosen-size+1;
      c := chosen -num +1;
      l := last   -num +1;
      HideMouse;
      IF lnum<>num THEN
      BEGIN
         FOR j := 1 TO size DO
         BEGIN
            IF (j=c) THEN
             BEGIN
                SetColor(DarkGray);
                SetFillStyle(SolidFill,white);
             END
            ELSE
             BEGIN
                SetColor(white);
                SetFillStyle(SolidFill,DarkGray);
             END;
            Bar( 3, (j-1)*RowHt+2, dx-3, j*RowHt-2);
            OutTextXY(3,(j-1)*RowHt+8,Rows[num+j-1]);
         END;
      END
      ELSE
      BEGIN
         SetFillStyle(SolidFill,DarkGray);
         Bar( 3, (l-1)*RowHt+2, dx-3, l*RowHt-2);
         SetFillStyle(SolidFill,white);
         Bar( 3, (c-1)*RowHt+2, dx-3, c*RowHt-2);
         SetColor(white);
         OutTextXY(3,(l-1)*RowHt+8,Rows[num+l-1]);
         SetColor(DarkGray);
         OutTextXY(3,(c-1)*RowHt+8,Rows[num+c-1]);
      END;
      ShowMouse;
   END;

BEGIN
   size   := 12;
   num    := 2;
   last   := 0;
   chosen := 1;
   dx     := 14*ColWidth;
   dy     := size*RowHt;
   bx     := ((GetMaxX+dx) DIV 2)/GetMaxX;
   by     :=-((GetMaxY-dy) DIV 2)/GetMaxY + 1;
   cx     := round(bx*GetMaxX)-dx-1;
   cy     := round((1-by)*GetMaxY);
   found  := false;
   escape := false;
   SetFillStyle(SolidFill,DarkGray);
   SetColor(white);
   HideMouse;
   Bar(cx,cy,cx+dx,cy+dy);
   rectangle(cx,cy,cx+dx,cy+dy);
   ShowMouse;
   x := dy/GetMaxY;
   Slider.Create(1,NRows+0.01,NRows,bx,by,by-x,-1,'','','',true);
   slider.step := 1;
   Slider.draw;
   CheckForEvents;
   REPEAT
      IF last<>chosen THEN PrintMenuBox(num,chosen,last);
      CheckForEvents;
      last   := chosen;
      chosen := NRows - round(slider.value) +1;
      WITH event DO
      IF mouseClicked AND (x>cx) AND (x<cx+dx) AND (y>cy) AND (y<cy+dy) THEN
      BEGIN
         chosen := num + (y-cy)*size DIV dy;
         found  := true;
         PrintMenuBox(num,chosen,last);
         Delay(500);
      END ELSE
      IF keyPressed AND
      ((readKey=chr(27)) OR (readKey=chr(13))) THEN
       IF (readKey=chr(13)) THEN found:=true ELSE escape:=true;
   UNTIL found OR escape;
   GetFile := (NOT escape);
END;


FUNCTION ChooseFile(VAR FName : string) : boolean;
VAR
   NumFiles,i,j,chosen           : Integer;
   file_     : SearchRec;
   MyFile    : string[12];
   FileName  : FNameTp;
   Dir       : DirStr;
   Name      : NameStr;
   ext       : ExtStr;
   path      : PathStr;
   FileFound,Escaped : boolean;
   drives    : string;
BEGIN
   Path := FName;
   FSplit(FExpand(path),Dir,Name,Ext);
   REPEAT
      IF dir='' THEN
      BEGIN
         drives := GetDrives;
         FOR NumFiles := 1 TO length(drives) DO
          FileName[NumFiles] := drives[numFiles]+':\';
         FOR i := NumFiles+1 TO 200 DO FileName[i]:='';
      END
      ELSE
      BEGIN
         NumFiles := 0;                           {Read Files from Directory}
         IF length(dir)=3 THEN
         BEGIN
            inc(NumFiles);
            FileName[NumFiles] := '..\';
         END;
         FindFirst(Dir+'*.*',Directory,file_);
         WHILE (DosError=0) AND (NumFiles<200) DO
         BEGIN
            IF (file_.attr=directory) AND (file_.name<>'.') then
            BEGIN
               inc(NumFiles);
               FileName[NumFiles] := file_.Name+'\';
            END;
            FindNext(file_);
         END;
         FindFirst(Dir+Name+Ext,archive,file_);
         WHILE (DosError=0) AND (NumFiles<200) DO
         BEGIN
            inc(NumFiles);
            FileName[NumFiles] := file_.Name;
            FOR i := 1 TO 12 DO
            BEGIN
               j := ord(FileName[NumFiles][i]);
               IF (j>64) AND (j<91) THEN FileName[NumFiles][i] := chr(j+32);
            END;
            FindNext(file_);
         END;
         IF (DosError=0) AND (NumFiles=200) THEN
           announce (' Error: too many files! ');
         FOR i := NumFiles+1 TO 200 DO FileName[i]:='';
         FOR i := 1 TO NumFiles DO            {Sort filenames alphabetically}
          FOR j := i+1 TO NumFiles DO
           IF FileName[j]<FileName[i] THEN
           BEGIN
              MyFile      := FileName[i];
              FileName[i] := FileName[j];
              FileName[j] := MyFile;
           END;
      END;
      FileFound := false;
      Escaped   := false;
      IF GetFile(chosen,NumFiles,FileName) THEN
      BEGIN
         IF (chosen<=NumFiles) THEN
         BEGIN
            MyFile := FileName[chosen];
            FileFound := (MyFile[length(MyFile)]<>'\');
            IF NOT fileFound THEN
            BEGIN
               IF MyFile='..\' THEN
               BEGIN
                  i := length(dir);
                  REPEAT  delete(Dir,i,1); dec(i) UNTIL (i=0)or(dir[i]='\');
               END
               ELSE dir := dir+MyFile;
            END;
         END
      END
      ELSE escaped := true;
   UNTIL FileFound OR escaped;
   FName := Dir+FileName[chosen];
   ChooseFile := NOT escaped;
END;

FUNCTION OpenFile(path : string) : string;
{ Input: Path string; Output: Selected String; If no file selected then
  output is enpty string}
VAR
   dx,dy,cx,cy,n : integer;
   image         : TImage;
   envir         : TEnvironment;
BEGIN
   envir.Save;
   envir.Standardize;
   SetFillStyle(SolidFill,blue);
   dx := 14*ColWidth;
   dy := 14*RowHt;
   cx := (GetMaxX-dx) DIV 2;
   cy := (GetMaxY-dy) DIV 2 -5;
   Image.Get(cx-1,cy,cx+dx+16,cy+dy);
   HideMouse;
   Bar3D(cx,cy+3,cx+round(16*ColWidth)-3,cy+RowHt+3,3,true);
   OutTextXY(cx+6,cy+8,'  Open File');
   ShowMouse;
   IF NOT ChooseFile(path) THEN path:='';
   SelectMuppetPort;
   ClearMKBuffers;
   CheckForEvents;
   image.put(cx-1,cy);
   envir.Reset;
   OpenFile:=path;
END;

FUNCTION ChooseDirectory(VAR FName : string) : boolean;
VAR
   NumFiles,i,j,chosen           : Integer;
   file_     : SearchRec;
   MyFile    : string[12];
   FileName  : FNameTp;
   Dir       : DirStr;
   Name      : NameStr;
   ext       : ExtStr;
   path      : PathStr;
   FileFound,Escaped : boolean;
   drives    : string;
BEGIN
   Path := FName;
   FSplit(FExpand(path),Dir,Name,Ext);
   REPEAT
      IF dir='' THEN
      BEGIN
         drives := GetDrives;
         FOR NumFiles := 1 TO length(drives) DO
          FileName[NumFiles] := drives[numFiles]+':\';
         FOR i := NumFiles+1 TO 200 DO FileName[i]:='';
      END
      ELSE
      BEGIN
         NumFiles := 1;                           {Read Files from Directory}
         FileName[1] := ' [ Accept ]';
         IF length(dir)=3 THEN
         BEGIN
            inc(NumFiles);
            FileName[NumFiles] := '..\';
         END;
         FindFirst(Dir+'*.*',Directory,file_);
         WHILE (DosError=0) AND (NumFiles<200) DO
         BEGIN
            IF (file_.attr=directory) AND (file_.name<>'.') then
            BEGIN
               inc(NumFiles);
               FileName[NumFiles] := file_.Name+'\';
            END;
            FindNext(file_);
         END;
         FOR i := NumFiles+1 TO 200 DO FileName[i]:='';
         FOR i := 2 TO NumFiles DO            {Sort filenames alphabetically}
          FOR j := i+1 TO NumFiles DO
           IF FileName[j]<FileName[i] THEN
           BEGIN
              MyFile      := FileName[i];
              FileName[i] := FileName[j];
              FileName[j] := MyFile;
           END;
      END;
      FileFound := false;
      Escaped   := false;
      IF GetFile(chosen,NumFiles,FileName) THEN
      BEGIN
         IF (chosen<=NumFiles) THEN
         BEGIN
            MyFile := FileName[chosen];
            FileFound := (MyFile[length(MyFile)]<>'\');
            IF NOT fileFound THEN
            BEGIN
               IF MyFile='..\' THEN
               BEGIN
                  i := length(dir);
                  REPEAT  delete(Dir,i,1); dec(i) UNTIL (i=0)or(dir[i]='\');
               END
               ELSE dir := dir+MyFile;
            END;
         END
      END
      ELSE escaped := true;
   UNTIL FileFound OR escaped;
   FName := Dir;
   ChooseDirectory := NOT escaped;
END;

PROCEDURE ChangeTempDrive;
{ Input: Path string; Output: Selected String; If no file selected then
  output is enpty string}
VAR
   dx,dy,cx,cy,n : integer;
   image         : TImage;
   envir         : TEnvironment;
   path          : string;
BEGIN
   envir.Save;
   envir.Standardize;
   SetFillStyle(SolidFill,blue);
   dx := 14*ColWidth;
   dy := 14*RowHt;
   cx := (GetMaxX-dx) DIV 2;
   cy := (GetMaxY-dy) DIV 2 -5;
   Image.Get(cx-1,cy,cx+dx+16,cy+dy);
   HideMouse;
   Bar3D(cx,cy+3,cx+round(16*ColWidth)-3,cy+RowHt+3,3,true);
   OutTextXY(cx+6,cy+8,'Temp Directory');
   ShowMouse;
   path := TempDrive;
   IF ChooseDirectory(path) THEN TempDrive := path;
   SelectMuppetPort;
   ClearMKBuffers;
   CheckForEvents;
   image.put(cx-1,cy);
   envir.Reset;
END;

{---------------------------- Hidden Hot-Keys ------------------------------}

PROCEDURE ChangeColors;
VAR
   colorScreen : TInputScreen;
   palette,original : paletteType;
   i,color : integer;
BEGIN
   GetPalette(palette);
   GetDefaultPalette(original);
   IF (GraphDriver=VGA) AND (GraphMode=VGAhi) THEN
   WITH colorScreen DO
   BEGIN
      init;
      DefineInputPort (0.1, 0.9, 0, 1);
      LoadLine('                   Change Screen Colors');
      LoadLine('');
      LoadLine('          black    cyan     brown    l.blue   l.red    white');
      LoadLine('             blue     red      l.gray   l.green  l.magenta');
      LoadLine('                green    magenta  gray     l.cyan   yellow');
      LoadLine('black     #0 #0 #0 #0 #0 #0 #0 #0 #0 #0 #0 #0 #0 #0 #0 #0');
      LoadLine('blue      #1 #1 #1 #1 #1 #1 #1 #1 #1 #1 #1 #1 #1 #1 #1 #1');
      LoadLine('green     #2 #2 #2 #2 #2 #2 #2 #2 #2 #2 #2 #2 #2 #2 #2 #2');
      LoadLine('cyan      #3 #3 #3 #3 #3 #3 #3 #3 #3 #3 #3 #3 #3 #3 #3 #3');
      LoadLine('red       #4 #4 #4 #4 #4 #4 #4 #4 #4 #4 #4 #4 #4 #4 #4 #4');
      LoadLine('magenta   #5 #5 #5 #5 #5 #5 #5 #5 #5 #5 #5 #5 #5 #5 #5 #5');
      LoadLine('brown     #6 #6 #6 #6 #6 #6 #6 #6 #6 #6 #6 #6 #6 #6 #6 #6');
      LoadLine('l.gray    #7 #7 #7 #7 #7 #7 #7 #7 #7 #7 #7 #7 #7 #7 #7 #7');
      LoadLine('gray      #8 #8 #8 #8 #8 #8 #8 #8 #8 #8 #8 #8 #8 #8 #8 #8');
      LoadLine('l.blue    #9 #9 #9 #9 #9 #9 #9 #9 #9 #9 #9 #9 #9 #9 #9 #9');
      LoadLine('l.green   #: #: #: #: #: #: #: #: #: #: #: #: #: #: #: #:');
      LoadLine('l.cyan    #; #; #; #; #; #; #; #; #; #; #; #; #; #; #; #;');
      LoadLine('l.red     #< #< #< #< #< #< #< #< #< #< #< #< #< #< #< #<');
      LoadLine('l.magenta #= #= #= #= #= #= #= #= #= #= #= #= #= #= #= #=');
      LoadLine('yellow    #> #> #> #> #> #> #> #> #> #> #> #> #> #> #> #>');
      LoadLine('white     #? #? #? #? #? #? #? #? #? #? #? #? #? #? #? #?');
      LoadLine('');
      LoadLine(' [  Ok  ]  [Cancel]  [Default]  [Reverse]  [Black/White]');
      FOR i := 0 TO maxColors DO
      BEGIN
         color := 0;
         WHILE (color<16) AND (palette.colors[i]<>original.colors[color]) DO
          inc(color);
         IF color=16 THEN color := palette.colors[i] MOD 16;
         SetRadioButton( chr(48+i), color+1);
      END;
      AcceptScreen;
      IF GetBoolean(259) THEN palette := original ELSE
      IF GetBoolean(260) THEN
      BEGIN
         FOR i := 0 TO maxColors DO
          palette.colors[i] := original.colors[MaxColors-i];
         palette.colors[8] := original.colors[15];
      END
      ELSE
      IF GetBoolean(261) THEN
      BEGIN
         FOR i := 0 TO 6         DO palette.colors[i] := original.colors[15];
         palette.colors[7] := original.colors[0];
         palette.colors[8] := original.colors[15];
         FOR i := 9 TO MaxColors DO palette.colors[i] := original.colors[0];
     END
      ELSE
       FOR i := 0 TO maxColors DO
        palette.colors[i] := original.colors[GetRadioButton(chr(48+i))-1];
     done;
   END
   ELSE
   WITH colorScreen DO
   BEGIN
      init;
      DefineInputPort (0.1, 0.9, 0.4, 0.6);
      LoadLine('                   Change Screen Colors');
      LoadLine(' [  Ok  ]  [Cancel]  [Default]  [Reverse]  [Black/White]');
      AcceptScreen;
      IF GetBoolean(3) THEN palette := original ELSE
      IF GetBoolean(4) THEN
      begin
         FOR i := 0 TO maxColors DO
          palette.colors[i] := original.colors[MaxColors-i];
         palette.colors[8] := original.colors[15];
      end
      ELSE
      IF GetBoolean(5) THEN
      BEGIN
         FOR i := 0 TO 6         DO palette.colors[i] := original.colors[15];
         palette.colors[7] := original.colors[0];
         palette.colors[8] := original.colors[15];
         FOR i := 9 TO MaxColors DO palette.colors[i] := original.colors[0];
      END;
      done;
   END;
   SetAllPalette(palette);
END;

PROCEDURE ReadDoubleClickTime;
VAR
   envir  : TEnvironment;
   r,p    : RectType;
   image  : TImage;
   slider : TSlider;
   e      : pointType;
BEGIN
   envir.Save;
   envir.Standardize;
   SetRect( r, round(0.25*GetMaxX), round(0.35*GetMaxY),
               round(0.75*GetMaxX), round(0.65*GetMaxY));
   SetRect( p, round(0.5*GetMaxX)-4*ColWidth, round(0.6*GetMaxY) -RowHt DIV 2,
               round(0.5*GetMaxX)+4*ColWidth, round(0.6*GetMaxY) +RowHt DIV 2);
   Image.Get( r.x1, r.y1, r.x2, r.y2);
   HideMouse;
   DrawBox(r,blue);
   DrawBox(p,LightGray);
   rectangle( r.x1+1, r.y1+1, r.x2-1, r.y2-1);
   SetLineStyle(SolidLn,0,ThickWidth);
   SetColor(black);
   rectangle( p.x1, p.y1, p.x2, p.y2);
   SetLineStyle(SolidLn,0,NormWidth);
   ShowMouse;
   SetTextJustify(CenterText,CenterText);
   SetColor(black);
   PrintXY(0.5,0.4,'Ok');
   SetColor(white);
   PrintXY(0.5,0.61,'Set Double Click Time.');
   PrintXY(0.5,0.58,'Double Click to hear the beep.');
   SetTextJustify(LeftText,TopText);
   slider.create(2,20,DoubleClickTime,0.35,0.5,0.69,0,
                 'fast','slow','',false);
   slider.step := 1;
   slider.draw;
   WITH event DO
   REPEAT
      CheckForEvents;
      IF DoubleClicked THEN beep;
      DoubleClickTime := round(slider.value);
      e.x:=event.x;  e.y:=event.y;
   UNTIL (KeyPressed   AND ((ReadKey=chr(13)) OR (ReadKey=chr(27))) OR
         (MouseClicked AND PtInside(e,p)));
   SelectMuppetPort;
   Image.Put( r.x1, r.y1);
   envir.Reset;
END;

PROCEDURE ReadDelayTime;
VAR
   envir  : TEnvironment;
   r,p    : RectType;
   image  : TImage;
   slider : TSlider;
   e,c    : pointType;
   counter : integer;
   s      : string[4];
BEGIN
   envir.Save;
   envir.Standardize;
   viewPortNumber := 0;
   s := '\|/-';
   counter := 1;
   SetRect( r, round(0.25*GetMaxX), round(0.35*GetMaxY),
               round(0.75*GetMaxX), round(0.65*GetMaxY));
   SetRect( p, round(0.5*GetMaxX)-4*ColWidth, round(0.6*GetMaxY) -RowHt DIV 2,
               round(0.5*GetMaxX)+4*ColWidth, round(0.6*GetMaxY) +RowHt DIV 2);
   SetPoint(c,round(0.5*GetMaxX), round(0.52*GetMaxY));
   Image.Get( r.x1, r.y1, r.x2, r.y2);
   HideMouse;
   OutTextXY( c.x, c.y, s[(counter MOD 4)+1] );
   DrawBox(r,blue);
   DrawBox(p,LightGray);
   rectangle( r.x1+1, r.y1+1, r.x2-1, r.y2-1);
   SetLineStyle(SolidLn,0,ThickWidth);
   SetColor(black);
   rectangle( p.x1, p.y1, p.x2, p.y2);
   SetLineStyle(SolidLn,0,NormWidth);
   ShowMouse;
   SetTextJustify(CenterText,CenterText);
   SetColor(black);
   PrintXY(0.5,0.4,'Ok');
   SetColor(white);
   PrintXY(0.5,0.6,'Set Delay Time.');
   SetTextJustify(LeftText,TopText);
   slider.create(0,500,DelayTime,0.35,0.53,0.69,0,
                 '0','500','Delay Time (ms)',false);
   slider.step := 25;
   slider.draw;
   WITH event DO
   REPEAT
      CheckForEvents;
      DelayTime := round(slider.value);
      SetColor(blue);
      OutTextXY( c.x, c.y, s[(counter MOD 3)+1] );
      IF counter<100 THEN inc(counter) ELSE counter:=1;
      SetColor(White);
      OutTextXY( c.x, c.y, s[(counter MOD 3)+1] );
      e.x:=event.x;  e.y:=event.y;
   UNTIL (KeyPressed   AND ((ReadKey=chr(13)) OR (ReadKey=chr(27))) OR
         (MouseClicked AND PtInside(e,p)));
   SelectMuppetPort;
   Image.Put( r.x1, r.y1);
   envir.Reset;
END;

PROCEDURE PrinterSetup;
VAR
   screen : TInputScreen;
   num : integer;
   cancel,reverse : boolean;
   prt,command : string;
BEGIN
   WITH screen DO
   BEGIN
      init;
      IF (GraphDriver=VGA) AND (GraphMode=VGAhi) THEN
           DefineInputPort(0.15,0.85,0.18,0.82)
      else DefineInputPort(0.15,0.85,0.05,0.95);
      LoadLine('               Print Screen Setup');
      LoadLine('');
      loadLine('Choose printer:');
      loadLine('1) Hewlett-Packard:           #1PCL (Default)');
      LoadLine(' #1ThinkJet    #1LaserJet     #1LaserJet II,III,4');
      LoadLine(' #1DeskJet     #1RuggedWriter #1RuggedWriterwide ');
      LoadLine(' #1PaintJet    #1QuietJet     #1QuietJet Plus ');
      LoadLine('');
      LoadLine('2) IBM:        #1Graphics     #1Graphics (wide)');
      LoadLine(' #1Color B/W   #1Color RGB    #1Color CMY       ');
      LoadLine(' #1Thermal     #1Proprinter   #1Quietwriter ');
      LoadLine(' #T Reverse Colors ');
      LoadLine('');
      LoadLine('               [  Ok  ]    [Cancel]');
      AcceptScreen;
      num := GetRadioButton('1');
      reverse := GetBoolean(19);
      cancel  := canceled;
      done;
   END;
   if (not cancel) then
   begin
      case num of
       1 : prt := 'HPDefault';
       2 : prt := 'ThinkJet';
       3 : prt := 'LaserJet';
       4 : prt := 'LaserJetII';
       5 : prt := 'DeskJet';
       6 : prt := 'RuggedWriter';
       7 : prt := 'RuggedWriterWide';
       8 : prt := 'PaintJet';
       9 : prt := 'QuietJet';
       10: prt := 'QuietJetPlus';
       11: prt := 'Graphics';
       12: prt := 'GraphicsWide';
       13: prt := 'Color1';
       14: prt := 'Color4';
       15: prt := 'Color8';
       16: prt := 'Thermal';
       17: prt := 'Graphics';
       18: prt := 'Graphics';
      end;
      command := 'graphics '+prt+' c:\dos\graphics.pro';
      if reverse then command := command + ' /r';
      WITH screen DO
      BEGIN
         init;
         IF (GraphDriver=VGA) AND (GraphMode=VGAhi) THEN
              DefineInputPort(0.13,0.87,0.24,0.76)
         else DefineInputPort(0.13,0.87,0.11,0.89);
         LoadLine('In order to install Print Screen memory resident program:');
         LoadLine('');
         LoadLine(' 1) Exit current program to DOS');
         LoadLine(' 2) If you have DOS version 5.0 or higher then type:');
         LoadLine('    '+command);
         LoadLine(' 3) When you run the program again <Shift>+<Print Screen>');
         LoadLine('    can be used in order to send screen dump to printer.');
         LoadLine(' 4) Use ''Change Colors'' to adjust colors if required.');
         LoadLine('');
         LoadLine('               [  Ok  ]  [Exit Program]');
         AcceptScreen;
         Cancel := getBoolean(2);
         done;
         if cancel then
         begin
            CupsDone;
            Halt;
         end;
      end;
   END;
END;


PROCEDURE HiddenHotKeys(key : char);
BEGIN
   CASE ord(key) OF
    32 : ChangeTempDrive;                          {alt-D}
    46 : ChangeColors;                             {alt-c}
    45 : BEGIN CupsDone; halt END;                 {alt-x}
    47 : begin announce('CUPS utilities version 1.3');
         event.ReadKey:=chr(1); end;             {alt-v}
    50 : begin announce(NumStr(MemAvail,6,0)+            {alt-m}
         ' bytes avaiable on heap. Largest block is '+NumStr(MaxAvail,6,0));
         event.ReadKey:=chr(1); end;
    20 : ReadDoubleClickTime;                      {alt-t}
    21 : ReadDelayTime;                            {alt-y}
    25 : PrinterSetUp;                             {alt-p}
   END;
END;

PROCEDURE Configuration;
VAR
   screen : TInputScreen;
BEGIN
   WITH screen DO
   BEGIN
      init;
      IF (GraphDriver=VGA) AND (GraphMode=VGAhi) THEN
         DefineInputPort(0.2,0.8,0.24,0.77)
      ELSE
         DefineInputPort(0.2,0.8,0.15,0.85);
      LoadLine('            Configuration Screen');
      LoadLine(' Use arrow keys and space-bar to select ');
      LoadLine(' an option and <Return> or <Esc> to quit.');
      LoadLine('');
      loadLine(' [Path Temporary Files Directory] (alt-d)');
      LoadLine(' [       Change Colors          ] (alt-c)');
      LoadLine(' [   Change Double Click Time   ] (alt-t)');
      LoadLine(' [         Delay Time           ] (alt-y)');
      LoadLine(' [     Print Screen Setup       ] (alt-p)');
      LoadLine(' [       Check  Memory          ] (alt-m)');
      LoadLine('');
      LoadLine('            [  Ok  ]');
      REPEAT
         AcceptScreen;
         IF GetBoolean(1) THEN ChangeTempDrive ELSE
         IF GetBoolean(2) THEN ChangeColors ELSE
         IF GetBoolean(3) THEN ReadDoubleClickTime ELSE
         IF GetBoolean(4) THEN ReadDelayTime ELSE
         IF GetBoolean(5) THEN PrinterSetup ELSE
         IF GetBoolean(6) THEN announce(NumStr(MemAvail,6,0)+
                              ' bytes avaiable on heap. Largest block is '+
                               NumStr(MaxAvail,6,0));
      UNTIL GetBoolean(7) OR canceled;
      done;
   END;
END;


{------------------------------- HotKeys ------------------------------------}

PROCEDURE THotKeys.Init(num_: integer);
var i : integer;
BEGIN
   num := num_;
   BackColor    := blue;
   TextColor    := white;
   HotKeyColor  := LightRed;
   DisactivateColor := LightBlue;
   for i := 1 to 12 do
   begin
      active[i] := true;
      code[i] := 0;
   end;
END;

PROCEDURE THotKeys.Display;
{ Displays already declared HotKeys in the buttom line of the screen. }
CONST
   name: ARRAY[1..23] OF STRING[6] =
      ('F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','UP','DOWN','LEFT',
      'RIGHT','PGUP','PGDN','END','HOME','INS','DEL','TAB','ESC','RETURN');
VAR
   i,n,m : byte;
   head,head1: STRING[6];
   envir : TEnvironment;
BEGIN
   envir.Save;
   envir.Standardize;
   SetFillStyle (SolidFill, BackColor);
   HideMouse;
   if num<=6 then i:=1 else i:=2;
   SetViewPort( 0, GetMaxY -i*RowHt -4, GetMaxX, GetMaxY, true);
   Bar3D( 0, 4, GetMaxX-4, i*RowHt, 3,true);
   FOR i := 1 TO num DO
   BEGIN
      n:=2;
      REPEAT inc(n) UNTIL (copy(key[i],n,1) = '-');
      head := copy(key[i],1,n-1);
      head1:= head;
      FOR m:=1 TO n-1 DO head1[m] := upcase(head[m]);
      m := 0;
      REPEAT inc(m) UNTIL (head1=name[m]) OR (m=24);
      IF m=24 THEN ERROR('THotKey.Display: '+head1+' is an invalid hot-key name');
      code[i] := m;
      IF (head1='RETURN') THEN head := chr(27) + chr(217) ELSE
      IF (head1='UP')     THEN head := chr(24) ELSE
      IF (head1='DOWN')   THEN head := chr(25) ELSE
      IF (head1='LEFT')   THEN head := chr(27) ELSE
      IF (head1='RIGHT')  THEN head := chr(26);
      if active[i] then SetColor(HotKeyColor)
      else SetColor(DisactivateColor);
      Print((i-1) mod 6 *13 + 2, (i-1) div 6 +1, head+'-');
      if active[i] then SetColor(TextColor)
      else SetColor(DisactivateColor);
      Print( (i-1) mod 6 *13 + Length(head) + 3, (i-1) div 6 + 1,
                 copy(key[i], n+1, Length(key[i]) -n) );
   END;
   ShowMouse;
   envir.Reset;
END;

FUNCTION THotKeys.Pressed( VAR knum : byte) : boolean;
{ Checks if one of the keys was pressed, or mouse is clicked on one of them.}
{ if yes then: function is true, and Knum returns number of a key. }
CONST
   KeyNum : ARRAY[1..23] OF byte =
      (59,60,61,62,63,64,65,66,67,68,72,80,75,77,73,81,79,71,82,83,09,27,13);
VAR
   n,a,button : integer;
   flag       : boolean;
BEGIN
   knum := 0;
   WITH event DO
   IF ExtendedKey OR KeyPressed OR MouseClicked THEN
    IF ExtendedKey THEN
     BEGIN
        n:=1; knum:=1;
        WHILE (n<=20) AND (ord(ReadKey)<>KeyNum[n]) DO inc(n);
        WHILE (knum<=num) AND (code[knum]<>n)       DO inc(knum);
        HiddenHotKeys(ReadKey);
     END
    ELSE
     IF KeyPressed THEN
      BEGIN
         n:=21; knum:=1;
         WHILE (n<=23) AND (ord(ReadKey)<>KeyNum[n]) DO inc(n);
         WHILE (knum<=num) AND (code[knum]<>n)       DO inc(knum);
      END
     ELSE
      IF (MouseClicked AND (y > (GetMaxY-2*RowHt-4))) THEN
      begin
         IF (y > (GetMaxY-RowHt-4)) THEN
           if (num<=6) then knum := x DIV 104 +1 else knum := x DIV 104 +7
         else
           if (num<=6) then knum := 0 else knum := x DIV 104 +1;
      end;
   flag := (knum<= num) AND (knum<>0);
   IF flag THEN ClearMkBuffers;
   pressed := flag and active[knum];
END;

PROCEDURE THotKeys.Clear;
{ Clears HotKeys line (the buttom one).}
VAR
   viewPort : ViewPortType;
BEGIN
   GetViewSettings( viewPort);
   SetViewPort(0,0,GetMaxX,GetMaxY,clipOn);
   HideMouse;
   SetFillStyle(SolidFill,black);
   if num<=6 then Bar( 0, GetMaxY -RowHt -4, GetMaxX, GetMaxY)
   else Bar( 0, GetMaxY -2*RowHt -4, GetMaxX, GetMaxY);
   ShowMouse;
   WITH viewPort DO SetViewPort( x1,y1,x2,y2,clip);
END;

PROCEDURE THotKeys.Done;
BEGIN
   init(0);
END;

{--------------------------- Pull-Down Menus ----------------------------}


PROCEDURE TMenu.MakeBox( x1,y1,x2,y2,FillColor: integer);
{inside procedure}
BEGIN
   SetFillStyle(SolidFill,FillColor);
   SetColor(TextColor);
   Bar3D(x1,y1,x2,y2,3,true);
END;


PROCEDURE TMenu.Print( num : byte);
{inside procedure}
VAR
   i,x,y : integer;
BEGIN
   message('');
   SelectMuppetPort;
   HideMouse;
   SetTextJustify(CenterText,CenterText);
   FOR i:=1 TO size[0] DO
   BEGIN
      x:=i*90-50; y:=12;
      IF i=num THEN
      BEGIN
         MakeBox( x-5*ColWidth, y-8, x+5*ColWidth, y+8,TextColor);
         SetColor(BackColor);
      END ELSE BEGIN
         MakeBox( x-5*ColWidth, y-8, x+5*ColWidth, y+8,BackColor);
         SetColor(TextColor);
      END;
      OutTextXY(x,y, Col[i]^.name);
   END;
   SetTextJustify(LeftText,TopText);
   ShowMouse;
END;


PROCEDURE TMenu.Init;
{ Initiate menu.  Call it BEFORE setting the menu.}
VAR i : byte;
BEGIN
   FOR i := 0 TO 7 DO size[i]:=0;
   new(terminator);
   terminator^.next:=terminator;
   terminator^.name:='';
   FOR i := 1 TO 7 DO col[i] := terminator;
   initialized    := false;
   BackColor      := blue;
   TextColor      := white;
   GrayOutColor   := lightBlue;
   HiGrayOutColor := lightCyan;
END;

PROCEDURE TMenu.Column(c : integer; name : str35);
VAR
   node : RowPtr;
   i : integer;
BEGIN
   IF (c<=7) AND (c>=1) THEN
   BEGIN
      FOR i := 1 TO c DO
        IF col[i]=terminator THEN
        BEGIN
           new(node);
           node^.name := '';
           node^.active := false;
           node^.checked := false;
           node^.next := col[i]^.next;
           col[i] := node;
        END;
      col[c]^.name:=name;
      col[c]^.active:=true;
      IF size[0]<c THEN size[0]:=c;
   END;
END;

PROCEDURE TMenu.Row(c,r : integer; name : str35);
VAR
   node,t : RowPtr;
   i : integer;
BEGIN
   IF (c>=1) THEN
   BEGIN
      t:=col[c];
      FOR i := 1 TO r DO
      BEGIN
         IF t^.next=terminator THEN
         BEGIN
            new(node);
            node^.next:=terminator;
            node^.name:='';
            node^.active:=false;
            node^.checked:=false;
            t^.next:=node;
         END;
         t:=t^.next;
      END;
      IF size[c]<r THEN size[c]:=r;
      t^.name := name;
      t^.active := true;
   END;
END;

PROCEDURE TMenu.RowActivate(c,r : integer; flag : boolean);
VAR t : rowPtr;
    i : integer;
BEGIN
   t:=col[c];
   i:=0;
   WHILE (t^.next<>terminator) AND (i<>r) DO
   BEGIN
      inc(i);
      t:=t^.next;
   END;
   IF i=r THEN t^.active := flag;
END;

PROCEDURE TMenu.RowChecked( c,r : integer; flag : boolean);
VAR t : rowPtr;
    i : integer;
BEGIN
   t:=col[c];
   i:=0;
   WHILE (t^.next<>terminator) AND (i<>r) DO
   BEGIN
      inc(i);
      t:=t^.next;
   END;
   IF i=r THEN t^.Checked := flag;
END;

PROCEDURE TMenu.AutoChecking( c,rInitial : integer);
BEGIN
   col[c]^.Checked := true;
   RowChecked(c,rInitial,true);
END;

PROCEDURE TMenu.Display;
{ Displays menu on the screen, but does not activate it.}
VAR
   i : byte;
   t : RowPtr;
   envir : TEnvironment;
BEGIN
   envir.Save;
   envir.Standardize;
   Initialized := true;
   FOR i := 0 TO 7 DO choice[i] := 1;
   FOR i := 1 TO size[0] DO
   BEGIN
      width[i] := 8;
      t := col[i]^.next;
      WHILE t<>terminator DO
      BEGIN
         IF width[i]<length(t^.name) THEN width[i]:=length(t^.name);
         t:=t^.next;
      END;
      width[i] := width[i] + 4;
   END;
   print(0);
   envir.Reset;
END;


FUNCTION TMenu.Chosen : boolean;
CONST
   h = 14;
   w = 8;
VAR
   i,c,r,attrib        : integer;
   X,Y,button          : integer;
   ch                  : char;
   first,second,escape : boolean;
   image               : TImage;
   t                   : RowPtr;
   envir               : TEnvironment;

   PROCEDURE PrintMenuBox;
   VAR
      c,j : integer;
      t : RowPtr;
      check : string;
   BEGIN
      IF (size[choice[0]] > 0) THEN
      BEGIN
         c := choice[0];
         HideMouse;
         SetViewPort( (c-1)*90, 26, (c-1)*90+w*width[c] , h*size[c]+26, ClipOn);
         IF r=0 THEN
         BEGIN
            MakeBox(0,0,w*width[c],h*size[c],BackColor);
            SetFillStyle(SolidFill,TextColor);
            Bar( 3, (choice[c]-1)*h+2, width[c]*w-3, choice[c]*h-2);
            t:=col[c];
            FOR j := 1 TO size[c] DO
            BEGIN
               t:=t^.next;
               IF (j=choice[c]) THEN
                  IF t^.active THEN SetColor(BackColor)
                  ELSE SetColor(HiGrayOutColor)
               ELSE
                  IF t^.active THEN SetColor(TextColor)
                  ELSE SetColor(GrayOutColor);
               IF t^.checked THEN check:=chr(251)+' ' else check:='  ';
               OutTextXY(3,(j-1)*h+4,check+t^.name);
            END;
         END
         ELSE
         BEGIN
            SetFillStyle(SolidFill,BackColor);
            Bar( 3, (r-1)*h+2, width[c]*w-3, r*h-2);
            SetFillStyle(SolidFill,TextColor);
            Bar( 3, (choice[c]-1)*h+2, width[c]*w-3, choice[c]*h-2);
            t:=col[c];
            FOR j := 1 TO size[c] DO
            BEGIN
               t:=t^.next;
               IF t^.checked THEN check:=chr(251)+' ' else check:='  ';
               IF j=r THEN
               BEGIN
                  IF t^.active THEN SetColor(TextColor)
                  ELSE SetColor(GrayOutColor) ;
                  OutTextXY(3,(j-1)*h+4,check+t^.name);
               END;
               IF j=choice[c] THEN
               BEGIN
                  IF t^.active THEN SetColor(BackColor)
                  ELSE SetColor(HiGrayOutColor) ;
                  OutTextXY(3,(j-1)*h+4,check+t^.name);
               END 
            END;
         END;
         SelectMuppetPort;
         ShowMouse;
         t:=col[c];
         i:=0;
         WHILE (t^.next<>terminator) AND (i<>choice[c]) DO
         BEGIN
            inc(i);
            t:=t^.next;
         END;
         second := second AND (i=choice[c]) AND t^.active;
      END;
   END;

BEGIN
   envir.Save;
   envir.Standardize;
   ShowMouse;
   escape := false;
   IF NOT Initialized THEN Display;
   first := false;
   Print(choice[0]);
   IF event.y>22 THEN CheckForEvents;
   REPEAT
      IF event.ExtendedKey THEN
       BEGIN
          CASE ord(event.ReadKey) OF
           75: IF choice[0]=1 THEN choice[0]:= size[0]
               ELSE dec(choice[0]);
           77: IF choice[0]=size[0] THEN choice[0]:=1
               ELSE inc(choice[0]);
           71,73 : choice[0]:=1;
           79,81 : choice[0]:=size[0];
           80: first := true;
          END; {case}
          HiddenHotKeys(event.ReadKey);
          Print(choice[0]);
       END
      ELSE
         IF event.KeyPressed THEN
          BEGIN
             IF (ord(event.ReadKey) = 27) THEN escape := TRUE
             ELSE IF (ord(event.ReadKey) = 13) THEN first := TRUE;
             Print(choice[0]);
          END
         ELSE
          IF event.MouseClicked THEN
           IF (event.y < 22) AND (event.x DIV 90 < size[0]) THEN
            BEGIN
               choice[0] := event.x DIV 90 + 1;
               Print(choice[0]);
               first := TRUE;
            END
           ELSE escape := TRUE;
         CheckForEvents;
   UNTIL first OR escape;
   IF (size[choice[0]] > 0) AND (NOT escape) THEN
   REPEAT
      r := 0;
      c := choice[0];
      Image.Get( (c-1)*90, 23, (c-1)*90+8*width[c]+3, 14*size[c]+26);
      PrintMenuBox;
      second := false;
      REPEAT
        IF keyPressed THEN
        BEGIN
          r := choice[c];
          ch := readKey;
          IF (ord(ch)=27) THEN escape := true;                    {esc}
          IF (ord(ch)=13) THEN second := true ELSE              {return}
          IF (ord(ch)=0)  THEN
          BEGIN
            ch := readKey;
            CASE ord(ch) OF
       {L}   75 : IF choice[0]=1 THEN choice[0]:=size[0] ELSE dec(choice[0]);
       {R}   77 : IF choice[0]=size[0] THEN choice[0]:=1 ELSE inc(choice[0]);
       {U}   72 : IF choice[c]=1 THEN choice[c]:=size[c] ELSE dec(choice[c]);
       {D}   80 : IF choice[c]=size[c] THEN choice[c]:=1 ELSE inc(choice[c]);
             71,73 : choice[c]:=1;
             79,81 : choice[c]:=size[c];
            END; {case}
            HiddenHotKeys(ch);
          END;
          IF choice[0]=c THEN PrintMenuBox;
          r := 0;
        END ELSE
        IF mouseClicked(x,y) THEN
           IF (y<22) AND (x DIV 90 < size[0]) THEN
           BEGIN
              choice[0] := x DIV 90 +1;
              IF size[choice[0]]=0 THEN second := true;
           END ELSE
           IF (x>(c-1)*90) AND (x<(c-1)*90+8*width[c]) AND
            (y>26) AND (y<14*size[c]+26) THEN
           BEGIN
              choice[c] := (y-26) DIV 14 +1;
              second := true;
              PrintMenuBox;
              delay (50);
           END
        ELSE escape := true;
     UNTIL second OR escape OR (c<>choice[0]);
     Print(choice[0]);
     Image.Put((c-1)*90,23);
   UNTIL second OR escape;
   Print(0);
   Chosen := NOT escape;
   colChosen := choice[0];
   rowChosen := choice[ choice[0] ];
   IF (NOT escape) AND col[colChosen]^.checked THEN
   BEGIN
      t:=col[c];
      FOR r := 1 TO size[c] DO
      BEGIN
         t:=t^.next;
         IF r=rowChosen THEN t^.checked:=true ELSE t^.checked:=false;
      END;
   END;
   envir.Reset;
END;


FUNCTION TMenu.Activated : boolean;
{ This function checks if user requests a menu during execution time.   }
{ If <F10> is pressed or mouse clicked on the upper row, the menu is    }
{ activated. Now if you choose a menu item the value returned is TRUE.  }
{ If you press <Esc> or click mouse outside the menu the value returned }
{ is FALSE.}
VAR
   x,y : integer;
BEGIN
   IF (event.ExtendedKey AND (ord(event.ReadKey)=68))   {F10}
   OR (event.mouseClicked AND (event.y<22)) THEN
   Activated := Chosen ELSE Activated := false;
   IF event.ExtendedKey THEN HiddenHotKeys(event.ReadKey);
END;

PROCEDURE TMenu.Done;
VAR i,j : integer;
    t,u : RowPtr;
BEGIN
   FOR i := 1 TO size[0] DO
   BEGIN
      t:=col[i];
      FOR j := 1 TO size[i]+1 DO
      BEGIN
         u:=t;
         t:=t^.next;
         dispose(u);
      END;
   END;
   dispose(terminator);
END;

{------------------------------ Bar Menus ----------------------------------}

PROCEDURE TBarMenu.MakeBox( x1,y1,x2,y2,FillColor: integer);
{inside procedure}
BEGIN
   SetFillStyle(SolidFill,FillColor);
   SetColor(TextColor);
   Bar3D(x1,y1,x2,y2,3,true);
END;

PROCEDURE TBarMenu.Init;
BEGIN
   BackColor    := blue;
   TextColor    := white;
END;

PROCEDURE TBarMenu.Draw(x,y : real; numRows, initial : integer);
VAR
   maxLength,mn,n,dy : integer;
   envir : TEnvironment;
BEGIN
   envir.Save;
   envir.Standardize;
   p.x:=round(x*GetMaxX);
   p.y:=round((1-y)*GetMaxY);
   num := numRows;
   current := initial;
   IF (current<1) OR (current>15) THEN current:=1;
   previous := current;
   maxLength:=0;
   FOR n := 1 TO num DO
    IF maxLength<length(row[n]) THEN
    BEGIN
       maxLength:=length(row[n]);
       mn := n;
    END;
    size := TextWidth(Row[mn])+20;
    HideMouse;
    FOR n := 1 TO num DO
    BEGIN
       dy := 3*(n-1)*RowHt DIV 2;
       IF n=current THEN
       BEGIN
          MakeBox(p.x,p.y+dy,p.x+size,p.y+dy+RowHt,TextColor);
          SetColor(BackColor);
       END ELSE BEGIN
          MakeBox(p.x,p.y+dy,p.x+size,p.y+dy+RowHt,BackColor);
          SetColor(TextColor);
       END;
       OutTextXY(p.x+10,p.y+dy+5, Row[n]);
    END;
    ShowMouse;
    envir.Reset;
END;

PROCEDURE TBarMenu.ReDraw;
BEGIN
    HideMouse;
    MakeBox(p.x,p.y+3*(previous-1)*RowHt DIV 2,p.x+size,
            p.y+3*(previous-1)*RowHt DIV 2+RowHt, BackColor);
    SetColor(TextColor);
    OutTextXY(p.x+10,p.y+3*(previous-1)*RowHt DIV 2+5, Row[previous]);
    MakeBox(p.x,p.y+3*(current-1)*RowHt DIV 2,p.x+size,
            p.y+3*(current-1)*RowHt DIV 2+RowHt, TextColor);
    SetColor(BackColor);
    OutTextXY(p.x+10,p.y+3*(current-1)*RowHt DIV 2+5, Row[current]);
    ShowMouse;
END;

FUNCTION TBarMenu.Chosen : integer;
VAR
   vp : ViewPortType;
   found : boolean;
   envir : TEnvironment;
BEGIN
   envir.Save;
   envir.Standardize;
   found := false;
   WITH event DO
   REPEAT
      previous := current;
      IF extendedKey THEN
      BEGIN
          CASE ord(readkey) OF
           80,75 : IF Current=num THEN Current:= 1  ELSE inc(Current);
           72,77 : IF Current= 1  THEN Current:=num ELSE dec(Current);
           73,71 : Current:=1;
           81,79 : Current:=num;
          END; {case}
         ReDraw;
      END;
      IF mouseClicked THEN
      BEGIN
         GetViewSettings(vp);
         x:=x-vp.x1-p.x; y:=y-vp.y1-p.y;
         IF (x>0) AND (x<size) AND (y>0) AND (y<3*num*RowHt DIV 2) THEN
         BEGIN
            current := y DIV (3*RowHt DIV 2)+1;
            ReDraw;
            found := true; 
         END;
      END;
      CheckForEvents;
   UNTIL (ReadKey=#13) OR found;
   ClearMKBuffers;
   CheckForEvents;
   chosen := current;
   envir.Reset;
END;

{------------------------------ Sliders ----------------------------------}



CONSTRUCTOR TSlider.create(ValMin, ValMax, VInitial, posX, posY, size_ : real;
         decimals_ : integer; Low_,High_,Name_ : string; vertical_ : boolean);
CONST w=15;
VAR
   sMin,sMax : string;
   d,n : integer;
BEGIN
   vMin:=ValMin; vMax:=ValMax; VCurrent:=VInitial;
   IF ValMin>ValMax THEN Swap(ValMin,ValMax);
   IF VCurrent<ValMin THEN VCurrent := ValMin;
   IF VCurrent>ValMax THEN VCurrent := ValMax;
   decimals:=decimals_; vertical:=vertical_;
   Low:=low_; High:=High_; Name:=name_;
   sMin := numStr(ValMax,0,decimals);
   sMax := numStr(ValMin,0,decimals);
   IF TextWidth(sMin)>TextWidth(sMax) THEN sMax:=sMin;
   NumWidth  := TextWidth(sMax) +4;
   NumLength := Length(sMax);
   bareSlider := decimals_<0;
   IF vertical THEN
    BEGIN
       IF size_<posY THEN swap( posY,size_);
       px   := round( posX*GetMaxX);
       py   := round( (1-size_)*GetMaxY);
       IF bareSlider THEN d:=0 ELSE
        IF length(name)=0 THEN d:=rowHt+4 ELSE
         d := rowHt +TextHeight(Name) +8;
       size := round( (1-posY)*GetMaxY) -d -py;
       SetRect(SliderBox, px+1, py+w+1, px+w-1, py+size-w-1); {main}
       n := py+size+4;
       IF length(name)<>0 THEN n := n +TextHeight(Name) +4;
       SetRect(TextBox, px+round(w-NumWidth) DIV 2, n,
                        px+round(w+NumWidth) DIV 2, n+RowHt);
       PutText.x := px+w DIV 2;
       PutText.y := n +RowHt DIV 2;
       SetRect  ( MainBox, px,py,px+w,py+size);
    END
   ELSE
    BEGIN
       IF size_<posX THEN swap( posX,size_);
       px   := round( posX*GetMaxX);
       py   := round( (1-posY)*GetMaxY)-w;
       size := round( size_*GetMaxX) - NumWidth - px;
       SetRect( SliderBox, px+w+1, py+1, px+size-w-1, py+w-1); {main}
       SetRect( TextBox, px+size+5,          py+round(w-RowHt) DIV 2,
                         px+size+5+NumWidth, py+round(w+RowHt) DIV 2);
       PutText.x := px +size +NumWidth DIV 2 +5;
       PutText.y := py +w DIV 2;
       SetRect  ( MainBox,px,py,px+size,py+w);
    END;
   step   := 2*(ValMax-ValMin)/size;
   chosen := true;
   active := false;
END;

PROCEDURE TSlider.Draw;
VAR
   envir : TEnvironment;
CONST w=15;

   PROCEDURE triangle(x1,y1,x2,y2,direction : integer);
   {directions: 0-Up, 1-Left, 2-Down, 3-Right}
   VAR
      v,c : real;
      s,n : integer;
      t : ARRAY [1..3] OF pointType;
   BEGIN
      SetFillStyle(solidFill,lightGray);
      bar(x1,y1,x2,y2);
      rectangle(x1,y1,x2,y2);
      v:=w/6;
      c:=w*(3-sqrt(3))/6;
      s:=direction SHR 1;
      t[1].x:=round(  v);   t[1].y:=round(((s+1) MOD 2)*w+(2*s-1)*c);
      t[2].x:=round(3*v);   t[2].y:=round(s*w-(2*s-1)*c);
      t[3].x:=round(5*v);   t[3].y:=t[1].y;
      IF (direction MOD 2)=1 THEN
       FOR n := 1 TO 3 DO BEGIN s:=t[n].x; t[n].x:=t[n].y; t[n].y:=s END;
      FOR n := 1 TO 3 DO  BEGIN t[n].x:=t[n].x+x1; t[n].y:=t[n].y+y1 END;
      SetFillStyle(solidFill,black);
      SetColor(lightGray);
      fillPoly(3,t);
      SetColor(white);
   END;

   PROCEDURE Print(Horiz,Vert,x,y: integer; text : string);
   BEGIN
      SetTextJustify( Horiz,Vert);
      outTextXY     ( x,y,text);
   END;

BEGIN
   envir.Save;
   envir.Standardize;
   active := true;
   HideMouse;
   IF vertical THEN
   BEGIN
      Triangle ( px,py+size-w,px+w,py+size,2);   {Down box}
      Triangle ( px,py,px+w,py+w,0);             {Up box}
      rectangle( px,py+w,px+w,py+size-w);
      Print( CenterText, TopText,    px+w DIV 2, py+size+4, Name);
      Print( RightText,  TopText,    px-2,       py,        High);
      Print( RightText,  BottomText, px-2,       py+size,   Low);
   END
   ELSE
   BEGIN
      Triangle ( px,py,px+w,py+w,1);           {left  box}
      Triangle ( px+size-w,py,px+size,py+w,3); {rigth box}
      rectangle( px+w,py,px+size-w,py+w);
      Print( LeftText,   TopText, px,            py+w+2, Low);
      Print( CenterText, TopText, px+size DIV 2, py+w+2, Name);
      Print( rightText,  TopText, px+size,       py+w+2, High);
   END;
   WITH SliderBox DO rectangle( x1-1,y1-1,x2+1,y2+1);
   ShowMouse;
   ReDraw;
   envir.Reset;
END;

PROCEDURE TSlider.ReDraw;
VAR
   n : integer;
   TextInfo : TextSettingsType;
CONST w=15;
BEGIN
   if active then
   begin
      GetTextSettings( TextInfo);
      SetTextJustify ( CenterText,CenterText);
      IF vertical THEN
      BEGIN
         n := round((VMax-VCurrent)/(VMax-VMin)*(size-3*w));
         SetRect(ThumbBox, px+1, py+w+n+1, px+w-1, py+2*w+n-1);
      END
      ELSE
      BEGIN
         n := round((VCurrent-Vmin)/(VMax-VMin)*(size-3*w));
         SetRect(ThumbBox, px+w+n+1, py+1, px+2*w+n-1, py+w-1);
      END;
      HideMouse;
      DrawBox( SliderBox,DarkGray);
      IF NOT BareSlider THEN DrawBox( TextBox,White);
      DrawBox( ThumbBox,LightGray);
      SetColor( black);
      IF NOT bareSlider THEN
       OutTextXY( PutText.x, PutText.y, numStr(VCurrent,0,decimals));
      WITH ThumbBox DO
      BEGIN
         rectangle(x1,y1,x2,y2);
         IF chosen THEN OutTextXY( x1+w DIV 2, y1+w DIV 2, chr(177));
      END;
      ShowMouse;
      changed := true;
      SetTextJustify( TextInfo.Horiz,TextInfo.vert);
   end;
END;

PROCEDURE TSlider.ReadNumber;
VAR
   v : real;
   z : char;
   TextInfo : TextSettingsType;
   x,y,i,cpos,counter,blinkingRate : integer;
   oldText,line,blanks,underline,cursorLine : string;
   exitFlag,onOff : boolean;
   e,textPos : pointType;

   PROCEDURE DelSpaces(VAR field: string);
   VAR n: integer;
   BEGIN
      n := length(field);
      WHILE (n>0) DO
      BEGIN
         IF (field[n]=' ') THEN delete(field,n,1);
         dec(n);
      END;
   END;

   PROCEDURE print( text : string);
   BEGIN
      SetColor(LightRed);
      HideMouse;
      OutTextXY(TextPos.x, TextPos.y, oldtext);
      SetColor(black);
      OutTextXY(TextPos.x, TextPos.y, text);
      ShowMouse;
      oldtext := text;
   END;

   PROCEDURE blink;
   BEGIN
      cursorLine := blanks;
      IF (length(line) MOD 2 <> NumLength MOD 2) THEN delete(cursorLine,1,1);
      insert('_',cursorLine,cPos+(NumLength-length(line)) DIV 2);
      REPEAT
         IF counter>BlinkingRate THEN
         BEGIN
            IF OnOff THEN
            BEGIN
               SetColor(LightRed);
               OutTextXY(TextPos.x, TextPos.y+1, underline);
            END
            ELSE
            BEGIN
               SetColor(black);
               OutTextXY(TextPos.x, TextPos.y+1, cursorLine);
            END;
            counter := 0;
            OnOff := NOT OnOff;
         END
         ELSE
         BEGIN
            delay(5);
            inc(counter);
         END;
      UNTIL keyPressed OR MouseClicked(E.x,E.y);   {cursor blinking}
   END;

BEGIN
   GetTextSettings( TextInfo);
   SetTextJustify ( LeftText,CenterText);
   SetPoint(textPos,TextBox.x1+3,PutText.y);
   IF TextPos.x<1 THEN TextPos.x:=1;
   line      := numstr(VCurrent,0,decimals);
   cpos      := 1;
   oldtext   := '';
   blanks    := '';
   underline := '';
   onOff     := false;
   exitFlag  := false;
   counter   := 0;
   blinkingRate := 40;
   FOR i := length(line)+1 TO NumLength DO line := concat(line,' ');
   FOR i := 1 TO NumLength-1 DO blanks    := concat(blanks,' ');
   FOR i := 1 TO NumLength   DO underline := concat(underline,'_');
   HideMouse;
   DrawBox(TextBox,lightRed);
   ShowMouse;
   SetColor(black);
   REPEAT
      print(line);
      e.x := 0;
      blink;
      IF e.x=0 THEN
      BEGIN
         z := readKey;
         CASE z OF
          '0'..'9','-','.','e','E':
                           IF cPos<=NumLength THEN
                           BEGIN
                              line[cPos] := z;
                              IF cPos<NumLength THEN inc(cPos);
                           END;
         chr(13),chr(27) : exitFlag:=true;                   {return,esc}
         chr(8),' '      : IF cPos>0 THEN                    {backSpace}
                           BEGIN
                              IF z=chr(8) THEN dec(cpos);
                              delete(line,cpos,1);
                              line := concat(line,' ');
                           END;
         chr(0) : CASE readKey OF
                    'M': IF cPos<=NumLength THEN inc(cPos);  {right arrow}
                    'K': IF cPos>0 THEN dec(cPos);           {Left  arrow}
                    'S': IF length(line)>=cPos THEN          {Delete}
                         BEGIN
                            delete(line,cPos,1);
                            line := concat(line,' ');
                         END;
                   END; {case}
         END; {case}
      END
      ELSE ExitFlag := NOT PtInside(e,TextBox);
   UNTIL exitFlag;
   DelSpaces(line);
   Val(line,v,i);
   IF (i=0) AND (z<>chr(27)) AND (v>=vMin) AND (v<=vMax) THEN vCurrent:=v;
   HideMouse;
   DrawBox(TextBox,white);
   SetTextJustify ( CenterText,CenterText);
   SetColor(black);
   OutTextXY(PutText.x,PutText.y,numstr(VCurrent,0,decimals));
   ShowMouse;
   SetTextJustify( TextInfo.Horiz,TextInfo.vert);
END;


FUNCTION TSlider.value : real;
CONST w=15;
VAR
   e            : pointType;
   button,ex,ey,x : integer;
   num          : String[20];
   h,v,vOld     : real;
   envir        : TEnvironment;
   OK           : boolean;

   FUNCTION PtAbove( p:pointType; r1,r2:rectType; vertical : boolean) : boolean;
   BEGIN
     IF vertical THEN
        PtAbove :=(p.x>r1.x1) AND(p.x<r2.x2) AND(p.y>r1.y1) AND(p.y<r2.y1)
     ELSE
        PtAbove :=(p.x>r2.x2) AND(p.x<r1.x2) AND(p.y>r2.y1) AND(p.y<r1.y2);
   END;


BEGIN
   MouseGlobalPosn(e.x,e.y,button);
   IF active and (event.MouseClicked OR event.KeyPressed OR (button<>0)) THEN
   BEGIN
      envir.Save;
      envir.Standardize;
      IF button=0 THEN BEGIN e.x:=event.x; e.y:=event.y END;
      vOld := vCurrent;
      IF chosen AND event.extendedKey THEN
         CASE ord(event.readkey) OF
          75,80 : IF VCurrent>vMin+step THEN
                    VCurrent:=VCurrent-step
                  ELSE
                    VCurrent:=vMin;
          77,72 : IF VCurrent<vMax-step THEN
                    VCurrent:=VCurrent+step
                  ELSE
                    VCurrent:=vMax;
          73 : BEGIN
                  v := (vMax-vMin)*w/(size-w);          {step size}
                  VCurrent:=round(VCurrent/v)*v;        {round it to the grid}
                  IF VCurrent<vMax-v THEN VCurrent:=VCurrent+v
                  ELSE VCurrent:=vMax
               END;
          79 : VCurrent:=vMax;
          81 : BEGIN
                  v := (vMax-vMin)*w/(size-w);          {step size}
                  VCurrent:=round(VCurrent/v)*v;        {round it to the grid}
                  IF VCurrent>vMin+v THEN VCurrent:=VCurrent-v
                  ELSE VCurrent:=vMin;
               END;
          71 : VCurrent:=vMin;
         END {case}
      ELSE
      IF event.mouseClicked OR (button<>0) THEN
       IF ptInside(e,MainBox) THEN
       BEGIN
         IF NOT event.MouseClicked THEN Ok := MouseClicked(x,x);
         IF ptInside(e,SliderBox) THEN
         BEGIN
            IF ptInside(e,ThumbBox) THEN
            BEGIN
               h:=(vMax-vMin)/(size-3*w);
               v:=w+w DIV 2;
               REPEAT
                  MouseGlobalPosn(ex,ey,button);
                  IF vertical THEN
                     IF (ey<=py+v)      THEN VCurrent:=VMax ELSE
                     IF (ey>=py-v+size) THEN VCurrent:=VMin ELSE
                     VCurrent := vMax-((ey-py-v)*h)
                  ELSE
                     IF (ex>=px-v+size) THEN VCurrent:=VMax ELSE
                     IF (ex<=px+v)      THEN VCurrent:=VMin ELSE
                     VCurrent := vMin+((ex-px-v)*h);
                  Redraw;
                  delay(50);
               UNTIL button=0;
            END
            ELSE {if not in ThumbBox}
            BEGIN
               v := (vMax-vMin)*w/(size-w);          {step size}
               VCurrent:=round(VCurrent/v)*v;        {round it to the grid}
               IF ptAbove(e,SliderBox,ThumbBox,vertical) THEN {right or up area}
                  IF VCurrent<vMax-v THEN VCurrent:=VCurrent+v
                  ELSE VCurrent:=vMax
               ELSE                                          {left or down area}
                  IF VCurrent>vMin+v THEN VCurrent:=VCurrent-v
                  ELSE VCurrent:=vMin;
               IF event.MouseClicked THEN Delay(500);
            END;
         END
         ELSE {if not inside sliderBox}
         BEGIN
            IF ptAbove(e,MainBox,SliderBox,vertical) THEN  {right or up arrow}
               IF (VCurrent<vMax-step) THEN VCurrent:=VCurrent+step
               ELSE VCurrent:=vMax
            ELSE                                          {left or down arrow}
               IF (VCurrent>vMin+step) THEN VCurrent:=VCurrent-step
               ELSE VCurrent:=vMin;
            IF event.MouseClicked THEN Delay(500);
         END
      END
      ELSE
      IF ptInside(e,TextBox) THEN ReadNumber;
      IF vCurrent<>vOld THEN ReDraw ELSE changed:=false;
      envir.Reset;
   END
   ELSE changed := false;
   value := VCurrent;
END;

PROCEDURE TSlider.Reset(VCurrent_ : real);
VAR
   envir : TEnvironment;
BEGIN
   envir.Save;
   envir.Standardize;
   VCurrent := VCurrent_;
   if VCurrent<vMin then VCurrent:=vMin;
   if VCurrent>vMax then VCurrent:=vMax;
   ReDraw;
   envir.Reset;
END;

PROCEDURE TSlider.Erase(color : integer);
   PROCEDURE EraseStr(Horiz,Vert,x,y: integer; text : string);
   BEGIN
      SetTextJustify( Horiz,Vert);
      RubOutXY( x/GetMaxX, 1 -y/GetMaxY, length(text), color);
   END;

CONST w=15;
VAR
   envir : TEnvironment;
BEGIN
   envir.Save;
   envir.Standardize;
   active := false;
   HideMouse;
   DrawBox(MainBox, color);
   DrawBox(TextBox, color);
   IF vertical THEN
   BEGIN
      EraseStr( CenterText, TopText,    px+w DIV 2, py+size+4, Name);
      EraseStr( RightText,  TopText,    px-2,       py,        High);
      EraseStr( RightText,  BottomText, px-2,       py+size,   Low);
   END
   ELSE
   BEGIN
      EraseStr( LeftText,   TopText, px,            py+w+2, Low);
      EraseStr( CenterText, TopText, px+size DIV 2, py+w+2, Name);
      EraseStr( rightText,  TopText, px+size,       py+w+2, High);
   END;
   ShowMouse;
   envir.Reset;
END;


DESTRUCTOR TSlider.Done;
BEGIN
   active := false;
END;

{------------------------------ TSliders ---------------------------------}

PROCEDURE TSliders.init;
BEGIN
   root := nil;
END;

PROCEDURE TSliders.create(num : integer; ValMin, ValMax, VInitial, posX,
           posY, size : real; decimals: integer;
           Low,High,Name : string; vertical : boolean);
VAR
   SliderPtr,p : SliderTypePtr;
BEGIN
   p:=root;
   while (p<>nil) do begin p^.slider.chosen:=false; p:=p^.next end;
   SliderPtr := root;
   while (SliderPtr<>nil) and (SliderPtr^.num<>num) do
    SliderPtr := SliderPtr^.next;
   if (SliderPtr=nil) then
   begin
      new(SliderPtr);
      SliderPtr^.next := root;
      root := SliderPtr;
   end;
   SliderPtr^.slider.create(ValMin, ValMax, VInitial, posX, posY, size,
                            decimals, Low,High,Name, vertical);
   SliderPtr^.current := VInitial;
   SliderPtr^.num := num;
   active:=SliderPtr;
END;

PROCEDURE TSliders.DrawAll;
VAR
   SliderPtr : SliderTypePtr;
BEGIN
   SliderPtr := root;
   WHILE SliderPtr<>nil DO
   begin
      SliderPtr^.Slider.Draw;
      SliderPtr := SliderPtr^.next;
   end;
END;

PROCEDURE TSliders.Draw(num : integer);
VAR
   SliderPtr : SliderTypePtr;
BEGIN
   SliderPtr := root;
   WHILE (SliderPtr<>nil) and (SliderPtr^.num<>num) DO
    SliderPtr:=SliderPtr^.next;
   if (SliderPtr=nil) then
    Error('TSliders.Draw: There is no slider #'+NumStr(num,2,0)) else
   SliderPtr^.Slider.Draw;
END;

FUNCTION TSliders.Changed : boolean;
VAR n    : byte;
    flag : boolean;
    vp : ViewPortType;
    p  : SliderTypePtr;
BEGIN
   IF event.keyPressed AND (event.readKey=#9) THEN   {Tab}
   BEGIN
      GetViewSettings(vp);
      SetViewPort(0,0,GetMaxX,GetMaxY,true);
      p:=root;
      while (p<>nil) do begin p^.slider.chosen:=false; p:=p^.next end;
      IF active^.next=nil THEN active:=root ELSE active:=active^.next;
      active^.Slider.chosen:=true;
      p:=root;
      while (p<>nil) do begin p^.Slider.ReDraw; p:=p^.next end;
      WITH vp DO SetViewPort(x1,y1,x2,y2,clip);
   END;
   p:=root;
   while (p<>nil) do begin p^.current:=p^.Slider.value; p:=p^.next end;
   flag := false;
   p:=root;
   while (p<>nil) do begin flag:=flag OR p^.Slider.Changed; p:=p^.next end;
   changed := flag;
END;


FUNCTION TSliders.LastAltered : integer;
VAR
   p : SliderTypePtr;
BEGIN
   p:=root; while (p<>nil) and (NOT p^.Slider.Changed) do p:=p^.next;
   IF p=nil THEN LastAltered := 0 ELSE LastAltered := p^.num;
END;


FUNCTION TSliders.Value(num : integer) : real;
VAR
   SliderPtr : SliderTypePtr;
BEGIN
   SliderPtr := root;
   WHILE (SliderPtr<>nil) and (SliderPtr^.num<>num) DO
    SliderPtr:=SliderPtr^.next;
   if (SliderPtr=nil) then
    Error('TSliders.Value: There is no slider #'+NumStr(num,2,0)) else
   value := SliderPtr^.current;
END;


PROCEDURE TSliders.Delete(num : integer);
VAR
   SliderPtr1,SliderPtr2 : SliderTypePtr;
BEGIN
   IF active^.num=num THEN
    IF num<>root^.num THEN active:=root ELSE active:=root^.next;
   SliderPtr1 := root;
   IF root<>nil THEN
   IF (SliderPtr1^.num=num) THEN
   BEGIN                         {deleate first element}
      root := SliderPtr1^.next;
      dispose(SliderPtr1);
   END
   ELSE
   BEGIN
      WHILE (SliderPtr1^.next^.num<>num) AND (SliderPtr1^.next<>nil) DO
        SliderPtr1:=SliderPtr1^.next;
      IF (SliderPtr1^.next<>nil) THEN
      BEGIN
         SliderPtr2 := SliderPtr1^.next;
         SliderPtr1^.next := SliderPtr2^.next;
         dispose(SliderPtr2);
      END
      else Error('TSliders.Delete: There is no slider #'+NumStr(num,2,0));
   END;
END;

PROCEDURE TSliders.SetStepSize(num : integer; step : real);
VAR
   SliderPtr : SliderTypePtr;
BEGIN
   SliderPtr := root;
   WHILE (SliderPtr<>nil) and (SliderPtr^.num<>num) DO
    SliderPtr:=SliderPtr^.next;
   if (SliderPtr=nil) then
    Error('TSliders.SetStepSize: There is no slider #'+NumStr(num,2,0)) else
   SliderPtr^.Slider.step:=step;
END;


PROCEDURE TSliders.Reset(num : integer; VCurrent_ : real);
VAR
   SliderPtr : SliderTypePtr;
BEGIN
   SliderPtr := root;
   WHILE (SliderPtr<>nil) and (SliderPtr^.num<>num) DO
    SliderPtr:=SliderPtr^.next;
   if (SliderPtr=nil) then
    Error('TSliders.Reset: There is no slider #'+NumStr(num,2,0)) else
   SliderPtr^.Slider.Reset(vCurrent_);
END;


PROCEDURE TSliders.Erase(num, color : integer);
VAR
   SliderPtr : SliderTypePtr;
BEGIN
   SliderPtr := root;
   WHILE (SliderPtr<>nil) and (SliderPtr^.num<>num) DO
    SliderPtr:=SliderPtr^.next;
   if (SliderPtr=nil) then
    Error('TSliders.Erase: There is no slider #'+NumStr(num,2,0)) else
   SliderPtr^.Slider.Erase(color);
END;


PROCEDURE TSliders.Done;
VAR
   SliderPtr1,SliderPtr2 : SliderTypePtr;
BEGIN
   SliderPtr1 := root;
   WHILE SliderPtr1<>nil DO
   BEGIN
      SliderPtr2 := SliderPtr1;
      SliderPtr1 := SliderPtr1^.next;
      Dispose(SliderPtr2);
   END;
   root := nil;
END;



{------------------------------- TRadioButtonField -------------------------}


{---------------------------------------------------------------------------}
PROCEDURE TButtons.Init;
{---------------------------------------------------------------------------}
BEGIN
   root  := NIL;
END;

{---------------------------------------------------------------------------}
PROCEDURE TButtons.create(num : integer; x,y : real; name : string);
{---------------------------------------------------------------------------}
VAR
   ButtonPtr : ButtonTypePtr;
BEGIN
   ButtonPtr := root;
   while (ButtonPtr<>nil) and (ButtonPtr^.num<>num) do
    ButtonPtr := ButtonPtr^.next;
   if (ButtonPtr=nil) then
   begin
      new(ButtonPtr);
      ButtonPtr^.next := root;
      root := ButtonPtr;
   end;
   ButtonPtr^.num := num;
   ButtonPtr^.x := round(x*GetMaxX);
   ButtonPtr^.y := round((1-y)*GetMaxY);
   ButtonPtr^.name := name;
   ButtonPtr^.active := false;
   number := num;
END;

{---------------------------------------------------------------------------}
PROCEDURE TButtons.DrawAll;
{---------------------------------------------------------------------------}
{Draws one Radio Button with the "color" color of background                }
VAR
   ButtonPtr : ButtonTypePtr;
   environment : TEnvironment;
BEGIN
   environment.save;
   environment.standardize;
   HideMouse;
   SetFillStyle(SolidFill,white);
   ButtonPtr := root;
   WHILE ButtonPtr<>nil DO
   WITH ButtonPtr^ DO
   BEGIN
      SetColor(black);
      FillEllipse(x,y,ColWidth,ColWidth);
      IF (Number=num) THEN
      BEGIN
         SetFillStyle(SolidFill,black);
         FillEllipse(x,y,ColWidth DIV 2,ColWidth DIV 2);
         SetFillStyle(SolidFill,white);
      END;
      SetColor(white);
      SetTextJustify(LeftText,CenterText);
      OutTextXY(x+2*ColWidth,y,name);
      ButtonPtr := ButtonPtr^.next;
      active := true;
   END;
   ShowMouse;
   environment.Reset;
END;

{---------------------------------------------------------------------------}
PROCEDURE TButtons.Draw(num : integer);
{---------------------------------------------------------------------------}
{Draws one Radio Button with the "color" color of background                }
VAR
   ButtonPtr : ButtonTypePtr;
   environment : TEnvironment;
BEGIN
   environment.save;
   environment.standardize;
   HideMouse;
   SetFillStyle(SolidFill,white);
   ButtonPtr := root;
   WHILE (ButtonPtr<>nil) and (ButtonPtr^.num<>num) DO
    ButtonPtr:=ButtonPtr^.next;
   if (ButtonPtr=nil) then
    Error('TButtons.Draw: There is no button #'+NumStr(num,2,0)) else
   WITH ButtonPtr^ DO
   BEGIN
      SetColor(black);
      FillEllipse(x,y,ColWidth,ColWidth);
      IF (Number=num) THEN
      BEGIN
         SetFillStyle(SolidFill,black);
         FillEllipse(x,y,ColWidth DIV 2,ColWidth DIV 2);
         SetFillStyle(SolidFill,white);
      END;
      SetColor(white);
      SetTextJustify(LeftText,CenterText);
      OutTextXY(x+2*ColWidth,y,name);
      ButtonPtr := ButtonPtr^.next;
      active := true;
   END;
   ShowMouse;
   environment.Reset;
END;

{---------------------------------------------------------------------------}
PROCEDURE TButtons.delete(num : integer);
{---------------------------------------------------------------------------}
VAR
   ButtonPtr1,ButtonPtr2 : ButtonTypePtr;
BEGIN
   IF number=num THEN
    IF num<>root^.num THEN number:=root^.num ELSE number:=root^.next^.num;
   ButtonPtr1 := root;
   IF root<>nil THEN
   IF (ButtonPtr1^.num=num) THEN
   BEGIN                         {deleate first element}
      root := ButtonPtr1^.next;
      dispose(ButtonPtr1);
   END
   ELSE
   BEGIN
      WHILE (ButtonPtr1^.next^.num<>num) AND (ButtonPtr1^.next<>nil) DO
        ButtonPtr1:=ButtonPtr1^.next;
      IF (ButtonPtr1^.next<>nil) THEN
      BEGIN
         ButtonPtr2 := ButtonPtr1^.next;
         ButtonPtr1^.next := ButtonPtr2^.next;
         dispose(ButtonPtr2);
      END
      else Error('TButtons.Delete: There is no button #'+NumStr(num,2,0));
   END;
END;

{---------------------------------------------------------------------------}
PROCEDURE TButtons.done;
{---------------------------------------------------------------------------}
VAR
   ButtonPtr1,ButtonPtr2 : ButtonTypePtr;
BEGIN
   ButtonPtr1 := root;
   WHILE ButtonPtr1<>nil DO
   BEGIN
      ButtonPtr2 := ButtonPtr1;
      ButtonPtr1 := ButtonPtr1^.next;
      Dispose(ButtonPtr2);
   END;
   root := nil;
END;

{---------------------------------------------------------------------------}
FUNCTION TButtons.Changed : boolean;
{---------------------------------------------------------------------------}
VAR
   ButtonPtr : ButtonTypePtr;
   oldNumber : integer;
   environment : TEnvironment;
BEGIN
   oldNumber := number;
   ButtonPtr := root;
   IF event.mouseClicked THEN
   WHILE ButtonPtr<>nil DO
   WITH ButtonPtr^ DO
   BEGIN
      ButtonPtr := ButtonPtr^.next;
      IF (abs(event.x-x)<ColWidth) AND (abs(event.y-y)<ColWidth) THEN
       number:=num;
   END;
   IF oldNumber<>number THEN
   BEGIN
      environment.save;
      environment.standardize;
      HideMouse;
      ButtonPtr := root;
      WHILE ButtonPtr<>nil DO
      BEGIN
         IF number=ButtonPtr^.num THEN
         BEGIN
            SetColor(black);
            SetFillStyle(SolidFill,black);
            WITH ButtonPtr^ DO FillEllipse(x,y,ColWidth DIV 2,ColWidth DIV 2);
         END ELSE
         IF oldNumber=ButtonPtr^.num THEN
         BEGIN
            SetColor(white);
            SetFillStyle(SolidFill,white);
            WITH ButtonPtr^ DO FillEllipse(x,y,ColWidth DIV 2,ColWidth DIV 2);
         END;
         ButtonPtr := ButtonPtr^.next;
      END;
      ShowMouse;
      environment.Reset;
   END;
   changed := (oldNumber<>number);
END;

procedure TButtons.Erase(num : integer; color : integer);
var
   ButtonPtr : ButtonTypePtr;
   environment : TEnvironment;
BEGIN
   ButtonPtr := root;
   WHILE (ButtonPtr<>nil) and (ButtonPtr^.num<>num) DO
    ButtonPtr := ButtonPtr^.next;
   if (ButtonPtr<>nil) then
   begin
      environment.save;
      environment.standardize;
      SetFillStyle(SolidFill,color);
      SetColor(color);
      SetTextJustify(LeftText,CenterText);
      HideMouse;
      WITH ButtonPtr^ DO
      BEGIN
         FillEllipse(x,y,ColWidth,ColWidth);
         OutTextXY(x+2*ColWidth,y,name);
         active := false;
      END;
      ShowMouse;
      environment.Reset;
   end else Error('TButtons.Erase: There is no button #'+NumStr(num,2,0));
end;

END. {CUPSgui}
