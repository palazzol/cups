                 (**********************************************)
                 (**********************************************)
                 (**                                          **)
                 (**     Unit CUPS - Basic Utilities          **)
                 (**     Written by Jarek Tuszynski           **)
                 (**     for CUPS Project                     **)
                 (**     Department of Physics and Astronomy  **)
                 (**     George Mason University              **)
                 (**     Fairfax, Virginia 22030              **)
                 (**     (c) 1994 by Jarek Tuszynski          **)
                 (**     Originated:  91/02/13                **)
                 (**     Version: 1.3 (94/11/14)              **)
                 (**                                          **)
                 (**********************************************)
                 (**********************************************)

UNIT CUPS;
INTERFACE
USES graph;

{$IFDEF CPU87}
  TYPE
     REAL = DOUBLE;
{$ELSE}
  TYPE
     SINGLE = REAL;
     DOUBLE = REAL;
     EXTENDED = REAL;
     COMP = REAL;
{$ENDIF}
{-------------------------- Constants ------------------------------------}

TYPE
   eventTp = RECORD
      keyPressed, extendedKey, doubleClicked, mouseClicked : boolean;
      readkey : char;
      x,y     :integer;
   END;

   rectType = RECORD
      x1,y1,x2,y2 : integer;
   END;

   View = RECORD
      Vx1, Vy1, Vx2, Vy2: Integer;
   END;

   GraphScale = RECORD
      Xmin, Ymin, Xmax, Ymax: REAL;
   END;

CONST
   GraphDriver : integer = Detect;
   GraphMode   : integer = VgaHi;

   shade : ARRAY [0..8] OF FillPatternType =
   (($00,$00,$00,$00,$00,$00,$00,$00),
    ($88,$00,$00,$00,$88,$00,$00,$00),
    ($22,$00,$88,$00,$22,$00,$88,$00),
    ($55,$00,$AA,$00,$55,$00,$AA,$00),
    ($55,$AA,$55,$AA,$55,$AA,$55,$AA),
    ($55,$FF,$AA,$FF,$55,$FF,$AA,$FF),
    ($77,$FF,$DD,$FF,$77,$FF,$DD,$FF),
    ($77,$FF,$FF,$FF,$77,$FF,$FF,$FF),
    ($FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF));

   Rainbow : ARRAY[0..2] OF PaletteType =
     ((size   : 16; colors : (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)),
      (size   : 16; colors : (0,1,9,11,3,10,2,14,6,13,5,12,4,8,7,15)),
      (size   : 16; colors : (0,4,12,5,13,6,14,2,10,3,11,9,1,8,7,15)));

   whitte   = 15;
   blakk    = 0;
VAR
   event       : eventTp;
   macheps     : real;
   HaltIfError : boolean;
   ErrorFound  : boolean;

   TempDrive   : string;
   DelayTime   : integer;
   DoubleClickTime : integer;

   ViewPortNumber : integer;
   GraphBackColor : integer;  {default = DarkGray}
   Views          : ARRAY[0..20] OF View;
   ScaleNumber    : integer;
   GraphScales    : ARRAY[0..20] OF GraphScale;
   ColWidth       : integer;
   RowHt          : integer;
   MousePresent   : boolean;

{------------------------- Control --------------------------------------}

PROCEDURE CupsInit;
PROCEDURE CupsDone;
PROCEDURE CheckForEvents;
PROCEDURE Announce(text : string);

{--------------------------------- Help ------------------------------------}

TYPE
   HelpScrType = ARRAY [1..25] OF string [50];

PROCEDURE Help(helpScr : HelpScrType);
PROCEDURE ShowHelp(HelpFileName,HelpScrName : string);
PROCEDURE AboutCUPS;

{---------------------- Number to String Conversion -----------------------}

FUNCTION NumStr ( num: REAL; i, j: integer): string;
FUNCTION ScNumStr(num: REAL; decimals: integer): string;
FUNCTION Num2Str( num: REAL; size: integer): string;

{------------------------------ CONCERNING MOUSE --------------------------}

PROCEDURE HideMouse;
PROCEDURE ShowMouse;
PROCEDURE HideCursor;
PROCEDURE ShowCursor;
PROCEDURE MouseGlobalPosn( VAR x, y, button : integer);
PROCEDURE MousePosn( VAR x,y: REAL; vNum,sNum: integer; VAR inside: boolean);
PROCEDURE MousePosition( VAR X, Y: REAL; vNum, sNum: integer;
                         VAR Inside: boolean);
PROCEDURE WaitOnMouseClick;
PROCEDURE Pause;
PROCEDURE DynamicPause;
PROCEDURE StaticPause;
FUNCTION  MouseClicked( VAR x,y : integer) : boolean;
PROCEDURE ClearMKBuffers;
PROCEDURE MouseBorders( x1,y1,x2,y2 : integer);

{------------------------- TEnvironment --------------------------------------}
TYPE
   TEnvironment = object
      PROCEDURE Save;
      PROCEDURE Standardize;
      PROCEDURE Reset;
   PRIVATE
      color,x,y : integer;
      text  : TextSettingsType;
      line  : LineSettingsType;
      fill  : FillSettingsType;
      view  : ViewPortType;
      vpNum : integer;
   END;

{------------------------------ TImage ---------------------------------}

   TImage = object
      PROCEDURE get(x1,y1,x2,y2 : integer);
      PROCEDURE put(x,y : integer);
      PROCEDURE Free;
   PRIVATE
      dx,dy,num : integer;
      p : pointer;
      myfile : FILE;
      active : boolean;
  END;

{------------------------------ TWaitMessage ----------------------------}

  TWaitMessage = object(TImage)
       PROCEDURE Show;
       PROCEDURE Update;
       PROCEDURE Hide;
   PRIVATE
       s : string[4];
       c,d : pointType;
       counter : integer;
   END;



{*************************************************************************}

IMPLEMENTATION
USES crt,dos;
const GraphicsOpen : boolean = false;

FUNCTION MouseDetected : boolean;  FORWARD;

PROCEDURE Error(str : string);
BEGIN
   announce(str);
   HALT;
END;


PROCEDURE Findmacheps(VAR macheps: REAL);
BEGIN
  macheps := 1.0;
  REPEAT
    macheps := macheps/2 ;
  UNTIL ((macheps + 1.0) = 1.0 )
END;

PROCEDURE Swap(var x,y : integer);
var d : integer;
BEGIN
   d := x;
   x := y;
   y := d;
END;


{---------------------- Timer (local) ------------------------------------}
{  The timer object class is used to measure time intervals between
   various events.  Written by Steve Spicklemire.}

TYPE
    TTimer = object
       lastTime : LONGINT;
       PROCEDURE ResetTimer;
       FUNCTION  GetDeltaTicks : LONGINT;
    END;

VAR
   mouseTimer : TTimer;

PROCEDURE TTimer.ResetTimer;
VAR a : longint;
BEGIN
   a := GetDeltaTicks;
END;

FUNCTION TTimer.GetDeltaTicks : LONGINT;
VAR
   Regs : registers;
   TickCount : Longint;
BEGIN
   regs.AX := 0;
   Intr($1A,Regs);
   TickCount := regs.CX SHL 16 + regs.DX;
   GetDeltaTicks := TickCount - lastTime;
   lastTime := TickCount;
END;

{--------------------------- controls ------------------------------------}

PROCEDURE EgaVgaDriverProc; EXTERNAL; {$L EGAVGA.OBJ }

PROCEDURE CUPSinit;
VAR err: integer;
BEGIN
   Findmacheps(macheps);
   event.KeyPressed    := false;
   event.MouseClicked  := false;
   event.ExtendedKey   := false;
   event.doubleClicked := false;
   IF GraphDriver = Detect THEN DetectGraph(GraphDriver, GraphMode);
   IF (not HaltIfError) and (GraphDriver<>VGA) THEN
   BEGIN
      Writeln('Graphics Error: This program works only with VGA driver!');
      Pause; Halt;
   END;
   IF NOT GraphicsOpen THEN
   BEGIN
      InitGraph(GraphDriver, GraphMode, '');
      err := GraphResult;
      IF err <> 0 THEN
      BEGIN
         Writeln('Graphics Error: ', GraphErrorMsg(err));
         Pause; Halt;
      END;  {if err}
   END;    {if not}
   HaltIfError    := TRUE;
   ErrorFound     := False;
   GraphicsOpen   := True;
   ViewPortNumber := 0;               {Default Viewport}
   ScaleNumber    := 0;               {Default scale}
   GraphBackColor := DarkGray;
   ClearDevice;
   SetViewport(0, 0, GetMaxX, GetMaxY, ClipOn);
   MouseBorders(0,0,GetMaxX,GetMaxY);
   ShowMouse;
   views[0].vx1:=0; views[0].vx2:=GetMaxX;
   views[0].vy1:=0; views[0].vy2:=GetMaxY;
END;


PROCEDURE CUPSdone;
BEGIN
   MouseBorders(0,0,GetMaxX,GetMaxY);
   HideMouse;
   CloseGraph;
   RestoreCRTMode;
   GraphicsOpen := false;
   GraphDriver  := Detect;
   GraphMode    := VgaHi;
END;


PROCEDURE CheckForEvents;
VAR
   x,y,button,i : WORD;
BEGIN
   Delay(delayTime);
   event.KeyPressed := keyPressed;
   IF event.keyPressed THEN event.ReadKey := ReadKey
   ELSE event.ReadKey := chr(1);
   event.ExtendedKey := (ord(event.readKey)=0);
   IF event.ExtendedKey THEN event.ReadKey := ReadKey;
   event.MouseClicked  := mouseClicked(event.x,event.y);
   event.doubleClicked := event.MouseClicked
      AND (mouseTimer.GetDeltaTicks < DoubleClickTime);
   IF NOT event.MouseClicked THEN
      BEGIN event.x := 0; event.y := 0 END;
END;


PROCEDURE Announce(text : string);
VAR
   dx,dy,cx,cy,a,n : integer;
   image           : TImage;
   z               : char;
   envir           : TEnvironment;
BEGIN
   envir.Save;
   envir.Standardize;
   SetFillStyle(SolidFill,red);
   dx := TextWidth(text)+13;
   dy := RowHt + 3;
   cx := (GetMaxX-dx) DIV 2;
   cy := (GetMaxY-dy) DIV 2;
   if (cx>2) then
   begin
      Image.Get(cx,cy,cx+dx,cy+dy);
      Sound(220);    {beep}
      Delay(200);
      NoSound;
      HideMouse;
      Bar3D(cx,cy+3,cx+dx-3,cy+dy,3,true);
      ShowMouse;
      ClearMKBuffers;
      REPEAT
         SetColor(red);
         HideMouse;
         OutTextXY(cx+6,cy+8,Text);
         ShowMouse;
         delay(50);
         SetColor(white);
         HideMouse;
         OutTextXY(cx+6,cy+8,Text);
         ShowMouse;
         delay(250);
      UNTIL keyPressed OR mouseClicked(a,a);
      ClearMKBuffers;
      image.put(cx,cy);
   end;
   envir.Reset;
END;

{----------------- Number to String Conversion -------------------------}

FUNCTION NumStr( Num: REAL; i, j: integer): string;
   VAR
      tempStr: string;
BEGIN
   IF j<0 THEN j := 0;
   str(Num : i : j, tempstr);
   numStr := tempstr;
END;

FUNCTION ScNumStr( num : REAL; decimals : integer) : string;
VAR
   nStr : string;
   mag  : integer;
BEGIN
   IF num=0 THEN nStr := '0' ELSE
   BEGIN
      mag := trunc(ln(abs(num))/ln(10));
      IF (mag<0) THEN dec(mag);
      IF (decimals<0) THEN decimals := 0;
      NStr := numStr(num/exp(mag*ln(10)),decimals+3,decimals);
      IF mag<>0 THEN NStr := concat( NStr, 'E', numStr(mag,5,0));
      mag := 1;
      WHILE mag<=length(nStr) DO                         {delete extra spaces}
       IF nStr[mag]= ' ' THEN delete(nStr,mag,1) ELSE inc(mag);
   END;
   ScNumStr := nStr;
END;


FUNCTION Num2Str(num : real; size : integer) : string;
VAR
   mag,sign,decim,n : integer;
   nStr : string;
   NoInteger,Scientific : boolean;
BEGIN
   IF size<=0 THEN nStr:=''
   ELSE
     IF num=0 THEN nStr := '0'
     ELSE
     BEGIN
        IF num<0 THEN sign:=1 ELSE sign:=0;
        size := size-sign;                          {space avaiable }
        num := abs(num);
        mag := trunc(ln(num)/ln(10));               {order of magnitude}
        IF (mag<0) THEN dec(mag);                   {correction for small num}
        decim:= size - (length(ScNumStr(num,1))-1); {num of decimals in Sc N}
        IF (int(num)<>0) THEN
        BEGIN
           IF (mag+1<size) THEN
             nStr := numStr(num, size, size-mag-2)  {2 becouse of .}
           ELSE
             nStr := ScNumStr(num,decim);
        END
        ELSE
        BEGIN
           IF (3-mag<=size) AND (mag>-4) THEN
              nStr := numStr(num, size, size-2)
           ELSE
              nStr := ScNumStr(num,decim);
        END;
        n := 1;
        WHILE n<=length(nStr) DO                {delete extra spaces}
         IF nStr[n]= ' ' THEN delete(nStr,n,1) ELSE inc(n);
        n := Pos('E',nStr);
        Scientific := (n<>0);
        IF Scientific THEN dec(n) else n := length(nStr);
        IF Pos('.',nStr)<>0 THEN                {if not integer...}
         WHILE (nStr[n]='0') DO
         BEGIN                                  {and has 0's on the end...}
            Delete(nStr,n,1);                   {...delete them}
            dec(n);
         END;
        IF nStr[n]='.' then
         If Scientific then Insert('0',nStr,n+1)   { . -> .0 }
         else Delete(nStr,n,1);                    {erase .}
        IF (sign=1) AND (nStr<>'0') THEN
         nStr := concat('-',nStr); {negative num}
     END;
   Num2Str := nStr;
END;

{------------------------------ Image -------------------------------------}

VAR numFilesOpen : byte;

PROCEDURE TImage.get(x1,y1,x2,y2 : integer);
VAR
   size,a,rows,xx : word;
   TempDrv : string;
BEGIN
   HideMouse;
   num:=0; dx:=x2-x1; dy:=y2-y1;
   REPEAT
      inc(num);
      Rows := dx DIV num + 1;
      size := ImageSize(0,0,rows,dy);
   UNTIL ((size<>0) AND (size<maxAvail)) OR (rows=1);
   IF (size>=maxAvail) THEN
   BEGIN
      announce(' Error: Not enougth space on the heap.');
      Halt;
   END;
   GetMem(p,size);
   IF num=1 THEN GetImage(x1,y1,x2,y2,p^)
   ELSE BEGIN
      inc(numFilesOpen);
      if TempDrive='' then TempDrv:='' else TempDrv:=TempDrive+'\';
      assign(myfile,TempDrv+'Temp'+NumStr(numFilesOpen,2,0)+'.CUP');
      rewrite(myfile,size);
      FOR a := 1 TO num DO
      BEGIN
         xx := x1+a*(dx DIV num +1);
         IF xx>GetMaxX THEN xx:=GetMaxX;
         GetImage( x1+(a-1)*(dx DIV num +1), y1, xx, y2,p^);
         BlockWrite(myFile,p^,1);
      END;
      FreeMem(p,size); 
      close(myFile);
   END;
   ShowMouse;
   active := true;
END;

PROCEDURE TImage.put(x,y : integer);
VAR size,a : word;
BEGIN
   IF active THEN
   BEGIN
      HideMouse;
      size := imageSize(0,0,dx DIV num +1,dy);
      IF num=1 THEN PutImage(x,y,p^,copyPut)
      ELSE
        BEGIN
         reset(myfile,size);
         GetMem(p,size);
         FOR a := 1 TO num DO
           BEGIN
             BlockRead(myFile,p^,1);
             PutImage(x+(a-1)*(dx DIV num +1),y,p^,CopyPut);
           END;
         close(myFile);
         erase(myFile);
         dec(numFilesOpen);
        END;
      FreeMem(p,size);
      ShowMouse;
      active := false;
   END;
END;

PROCEDURE TImage.free;
VAR
   size : word;
BEGIN
   IF active THEN
   BEGIN
      size := imageSize(0,0,dx DIV num +1,dy);
      IF num<>1 THEN erase(myFile);
      FreeMem(p,size);
      active := false;
   END;
END;

{--------------------------------- Help ------------------------------------}

PROCEDURE HelpXY(x1,y1 : real; helpScr : HelpScrType);
VAR
   envir   : TEnvironment;
   image   : TImage;
   x,y,dx,dy,h,i,size,color : word;
BEGIN
   envir.Save;
   envir.Standardize;
   SetRGBPalette(green, $00, $18, $7);
   SetFillStyle(SolidFill,green);
   SetLineStyle(solidLn, 0,thickWidth);
   dx:= round(0.64 *GetMaxX);
   IF GraphMode=VGAMed THEN dy:= round(0.88 *GetMaxY)
   ELSE dy:= round(0.64 *GetMaxY);
   x := round(x1*GetMaxX);
   y := round(y1*GetMaxY);
   IF (x<0) OR (x+dx>GetMaxX) THEN x := (GetMaxX-dx) DIV 2;
   IF (y<0) OR (y+dy>GetMaxY) THEN y := (GetMaxY-dy) DIV 2;
   SetViewPort(x, y, x+dx, y+dy, clipOn);
   HideMouse;
   image.get(0,0,dx,dy);
   Bar(0,0,dx,dy);
   rectangle(1,1,dx-1,dy-1);
   h := 3 * TextHeight('O') DIV 2;
   FOR i := 1 TO 25 DO OutTextXY(5,(i-1)*h + 5, helpScr[i]);
   ShowMouse;
   Delay(200);
   StaticPause;
   HideMouse;
   image.put(0,0);
   ShowMouse;
   ClearMKBuffers;
   SetRGBPalette(green, $00, $24, $00);
   envir.Reset;
END;

PROCEDURE Help(helpScr : HelpScrType);
BEGIN
   IF GraphMode=VGAMed THEN
      HelpXY(0.18,0.06,helpScr)
   ELSE
      HelpXY(0.18,0.18,helpScr);
END;

PROCEDURE AboutCUPS;
VAR
   AboutScr : HelpScrType;
BEGIN              {                                                  }
   AboutScr[01] := '                                                  ';
   AboutScr[02] := '                       CUPS                       ';
   AboutScr[03] := '   Consortium for Upper-level Physics Software    ';
   AboutScr[04] := '                                                  ';
   AboutScr[05] := ' The CUPS collaboration is an international group ';
   AboutScr[06] := ' of  27 physicists  who  have  produced  computer ';
   AboutScr[07] := ' software  and accompanying  textbooks for use in ';
   AboutScr[08] := ' nine junior/senior level  physics courses.  This ';
   AboutScr[09] := ' work,  published  by  John  Wiley and Sons,  was ';
   AboutScr[10] := ' supported  by  the  National  Science Foundation ';
   AboutScr[11] := ' (grant # PHY-9014548),  George Mason University, ';
   AboutScr[12] := ' the IBM Corporation, and Apple Computer, Inc.    ';
   AboutScr[13] := '                                                  ';
   AboutScr[14] := ' The CUPS project is directed by Maria Dworzecka, ';
   AboutScr[15] := ' Robert Ehrlich,  and  William M. MacDonald.   It ';
   AboutScr[16] := ' uses utilities written by William MacDonald  and ';
   AboutScr[17] := ' Jaroslaw  Tuszynski.  CUPS  programs  are  copy- ';
   AboutScr[18] := ' righted by John Wiley and Sons, Inc.             ';
   AboutScr[19] := '                                                  ';
   AboutScr[20] := ' For more information about  the CUPS project see ';
   AboutScr[21] := ' "Computer in Physics" 6 (1992) 90, 7 (1993) 508, ';
   AboutScr[22] := ' and 8 (1994) 386.  You may also contact the dir- ';
   AboutScr[23] := ' ectors at internet address cups@gmuvax.gmu.edu.  ';
   AboutScr[24] := '                                                  ';
   AboutScr[25] := '                                                  ';
   Help(AboutScr);
END;

PROCEDURE ShowHelp(HelpFileName,HelpScrName : string);
{Written by Randolf Jones, modified by Jarek Tuszynski}
VAR
   FileVar     : Text;
   i,imax      : integer;
   HelpScr     : HelpScrType;
   HelpLine    : string [50];
   word,upWord : string [11];
   wait        : TWaitMessage;
   EmptyLine   : boolean;
   x,y         : real;
BEGIN
   for i:=length(HelpScrName) downto 1 do
    IF (HelpScrName[i]=' ') or (HelpScrName[i]=chr(9)) then
	  Delete(HelpScrName,i,1)
    else HelpScrName[i] := upCase(HelpScrName[i]);
  {+-------------------------------------------------------------------}
  {| Check that help file exists.  I know these EXITS are blasphemy.
  {+-------------------------------------------------------------------}
   Assign(FileVar,HelpFileName);
   {$i-} Reset(FileVar); {$i+}
   IF IOResult<>0 THEN
   BEGIN
      Announce('Help file is not available in current directory');
      EXIT;
   END;
  {+-------------------------------------------------------------------}
  {| Look for Help Screen Name.
  {+-------------------------------------------------------------------}
   HelpLine := ' ';
   wait.show;
   WHILE NOT (EOF(FileVar) OR (HelpLine=HelpScrName)) DO
   BEGIN
      {$I-}
      emptyLine := true;
      REPEAT                        {find no empty line}
         READLN(FileVar,HelpLine);
         i := 1;
         WHILE emptyline AND (i<=length(HelpLine)) DO
         BEGIN
            EmptyLine := EmptyLine AND (HelpLine[i]=' ');
            inc(i);
         END;
      UNTIL NOT(emptyLine) OR EOF(FileVar);
      for i:=length(HelpLine) downto 1 do
       IF (HelpLine[i]=' ') or (HelpLine[i]=chr(9)) then Delete(HelpLine,i,1)
       else HelpLine[i] := upCase(HelpLine[i]);
      IF (HelpLine<>HelpScrName) AND (NOT EOF(FileVar)) THEN
      BEGIN
         Readln(FileVar,word);                      {read first word}
         upWord := word;
         FOR i := 1 TO 11 DO upWord[i] := UpCase(upWord[i]); {UpCase the word}
         IF upWord='COORDINATES' THEN imax := 25 ELSE imax := 24;
         FOR i := 1 TO imax DO READLN(FileVar);
      END;
      {$I+}
      wait.update;
      IF IOResult<>0 THEN
      BEGIN
         Announce('Error occured while reading a file');
         CLOSE(FileVar);
         EXIT;
      END;
   END;
   wait.hide;
   IF HelpScrName<>HelpLine THEN
   BEGIN
      Announce('No Help available on this topic');
      CLOSE(FileVar);
      EXIT;
   END;
  {+-------------------------------------------------------------------}
  {| Display the help screen.
  {+-------------------------------------------------------------------}
    {$I-}
    Read(FileVar,word);                             {read first word}
    upWord := word;
    FOR i := 1 TO 11 DO upWord[i] := UpCase(upWord[i]); {UpCase the word}
    IF upWord='COORDINATES' THEN
    BEGIN
        ReadLn(FileVar,x,y);
        FOR i := 1 TO 25 DO READLN(FileVar,HelpScr[i]);
        IF IOResult=0 THEN HelpXY(x,y,HelpScr);
    END
    ELSE
    BEGIN
        ReadLn(FileVar,HelpLine);
        HelpScr[1] := concat(word,HelpLine);
        FOR i := 2 TO 25 DO READLN(FileVar,HelpScr[i]);
        IF IOResult=0 THEN Help(HelpScr);  
    END;
    {$I+}
   IF IOResult<>0 THEN
   BEGIN
      Announce('Error occured while reading a file');
      CLOSE(FileVar);
      EXIT;
   END;
   CLOSE(FileVar);
END;

{------------------------------- WaitMessage -----------------------------}
{---------------  Based on code written by Graham Keeler  ----------------}
                                  
PROCEDURE TWaitMessage.Show;
VAR
   r     : rectType;
   text  : string;
   n     : integer;
   envir : TEnvironment;
BEGIN
   envir.Save;
   envir.Standardize;
   SetFillStyle(SolidFill,blue);
   text := 'Calculating - please wait';
   d.x := TextWidth(text)+13;
   d.y := RowHt + 3;
   r.x1 := (GetMaxX - d.x) DIV 2;
   r.y1 := (GetMaxY - d.y) DIV 2;
   r.x2 := (GetMaxX + d.x) DIV 2;
   r.y2 := (GetMaxY + d.y) DIV 2;
   Get(r.x1,r.y1,r.x2,r.y2);
   HideMouse;
   Bar3D(r.x1,r.y1+3,r.x2-3,r.y2,3,true);
   OutTextXY(r.x1+6,r.y1+8,Text);
   ShowMouse;
   s := '\|/-';
   c.x := r.x1+6+12*ColWidth;
   c.y := r.y1+8;
   counter := 2;
   envir.Reset;
END;

PROCEDURE TWaitMessage.Update;
VAR   n,color : integer;
      view    : ViewPortType;
      text    : TextSettingsType;
BEGIN
   GetViewSettings(view);
   color := getColor;
   SetViewPort(0,0,GetMaxX,GetMaxY,ClipOn);
   SetColor(Blue);
   GetTextSettings(text);
   SetTextStyle( DefaultFont, HorizDir, 1);
   SetTextJustify( LeftText, TopText);
   OutTextXY( c.x, c.y, s[(counter MOD 4)+1] );
   IF counter<100 THEN inc(counter) ELSE counter:=1;
   SetColor(White);
   OutTextXY( c.x, c.y, s[(counter MOD 4)+1] );
   SetColor(color);
   WITH text DO SetTextStyle( Font, Direction, CharSize);
   WITH text DO SetTextJustify( Horiz, Vert);
   WITH view DO SetViewPort( x1,y1,x2,y2,clip);
END;

PROCEDURE TWaitMessage.Hide;
VAR
   view    : ViewPortType;
BEGIN
   GetViewSettings(view);
   SetViewPort(0,0,GetMaxX,GetMaxY,ClipOn);
   put((GetMaxX - d.x) DIV 2, (GetMaxY - d.y) DIV 2);
   WITH view DO SetViewPort( x1,y1,x2,y2,clip);
END;

{------------------ MOUSE PROCEDURES -----------------------}

FUNCTION MouseDetected: boolean;
   VAR
      Regs: registers;
BEGIN
   Regs.AX := 0;
   Intr($33, Regs);
   mouseDetected := (Regs.AX <> 0);
END;

PROCEDURE ShowCursor;
begin
   ShowMouse;
end;

PROCEDURE HideCursor;
begin
   HideMouse;
end;

PROCEDURE ShowMouse;
   VAR
      Regs: registers;
BEGIN
   IF MousePresent THEN
   BEGIN
      Regs.AX := 1;
      Intr($33, Regs);
   END;
END;

PROCEDURE HideMouse;
   VAR
      Regs: registers;
BEGIN
   IF MousePresent THEN
   BEGIN
      Regs.AX := 2;
      Intr($33, Regs);
   END;
END;

PROCEDURE MouseGlobalPosn( VAR x, y, button : integer);
   VAR
      Regs: registers;
BEGIN
   IF MousePresent THEN
      WITH Regs DO
      BEGIN
         AX := 3;
         Intr($33, Regs);
         X := CX;
         Y := DX;
         button := BX;
      END
   ELSE button := 0;
END;


PROCEDURE MousePosn( VAR X, Y: REAL; vNum, sNum: integer; VAR Inside: boolean);
   VAR
      XFactor, YFactor: REAL;
      xGlobal, yGlobal, button: integer;
BEGIN
   if (vNum<0) or (vNum>20) then
    Error('MousePosn: view port number out of range');
   if (sNum<0) or (sNum>20) then
    Error('MousePosn: scale number out of range');
   MouseGlobalPosn(xGlobal, yGlobal, button);
   WITH Views[vNum] DO
      WITH GraphScales[sNum] DO
      BEGIN
         xFactor := (xmax - xmin) / (Vx2 - Vx1);
         yFactor := (ymax - ymin) / (Vy2 - Vy1);
         X       := (xFactor * (xGlobal - Vx1)) + xmin;
         Y       :=-(yFactor * (yGlobal - Vy1)) + ymax;
         inside  := (xGlobal >= Vx1) AND (xGlobal <= Vx2)
                AND (yGlobal >= Vy1) AND (yGlobal <= Vy2);
      END;
END;

PROCEDURE MousePosition( VAR X, Y: REAL; vNum, sNum: integer;
                         VAR Inside: boolean);
   VAR
      XFactor, YFactor: REAL;
      xGlobal, yGlobal, button: integer;
BEGIN
   if (vNum<0) or (vNum>20) then
    Error('MousePosn: view port number out of range');
   if (sNum<0) or (sNum>20) then
    Error('MousePosn: scale number out of range');
   WITH Views[vNum] DO
      WITH GraphScales[sNum] DO
      BEGIN
         xFactor := (xmax - xmin) / (Vx2 - Vx1);
         yFactor := (ymax - ymin) / (Vy2 - Vy1);
         X       := (xFactor * (event.x - Vx1)) + xmin;
         Y       :=-(yFactor * (event.y - Vy1)) + ymax;
         inside  := (event.x >= Vx1) AND (event.x <= Vx2)
                AND (event.y >= Vy1) AND (event.y <= Vy2);
      END;
END;



PROCEDURE WaitOnMouseClick;
VAR
   x, y, button: integer;
BEGIN
   IF MousePresent THEN
   BEGIN
      REPEAT  MouseGlobalPosn(x, y, button)  UNTIL(button<>0);
      REPEAT  MouseGlobalPosn(x, y, button)  UNTIL(button=0);
   END;
END;

PROCEDURE Pause;
VAR Z: char;
BEGIN
   Z := ReadKey;
END;

PROCEDURE DynamicPause;
{ Waits on keyPressed or mouseClick.  It doesn't clean the buffer so you can}
{  check what key was pressed. }
VAR
   x,y,button : integer;
BEGIN
   REPEAT
      mouseGlobalPosn( x,y,button)
   UNTIL keyPressed OR (button<>0);
END;


PROCEDURE StaticPause;
{ Waits on keyPressed or mouseClick.  If you use keybord StaticPause does   }
{ clean the buffer so you can't check what key was pressed. If you use mouse}
{ StaticPause waits until the mouse button is released.}
VAR
   z : char;
   x,y,button : integer;
BEGIN
   REPEAT
      mouseGlobalPosn( x,y,button)
   UNTIL keyPressed OR (button<>0);
   IF keyPressed THEN
      z := readkey
   ELSE
     REPEAT
        mouseGlobalPosn( x,y,button)
     UNTIL (button =0);
END;


FUNCTION MouseClicked( VAR x,y : integer) : boolean;
{ Checks if mouse was clicked since last call, and if it was, returns its   }
{ position.}
VAR
   Regs : registers;
   flag : boolean;
BEGIN
   flag := false;
   IF mousePresent THEN
     WITH regs DO
       BEGIN
         AX := 5;
         BX := 0;
         Intr($33,Regs);
         X := CX;
         Y := DX;
         flag := (BX<>0);
         AX := 5;
         BX := 1;
         Intr($33,Regs);
         IF (BX<>0) THEN
         BEGIN
            X := CX;
            Y := DX;
         END;
         flag := (BX<>0) OR flag;
       END;
   MouseClicked := flag;
END;


PROCEDURE ClearMKBuffers;
VAR
   z          : char;
   x,y,button : integer;
BEGIN
   REPEAT MouseGlobalPosn(y,y,button) UNTIL button=0;
   WHILE KeyPressed DO z := readKey;
   IF MouseClicked(x,x) THEN;
END;


PROCEDURE MouseBorders( x1,y1,x2,y2 : integer);
VAR regs : registers;
BEGIN
   if x1<0 then x1:=0;
   if x2<0 then x2:=0;
   if y1<0 then y1:=0;
   if y2<0 then y2:=0;
   if x1>GetMaxX then x1:=GetMaxX;
   if x2>GetMaxX then x2:=GetMaxX;
   if y1>GetMaxY then y1:=GetMaxY;
   if y2>GetMaxY then y2:=GetMaxY;
   if x1>x2 then swap(x1,x2);
   if y1>y2 then swap(y1,y2);
   IF mousePresent THEN
   BEGIN
      regs.AX := 7;
      regs.CX := x1;
      regs.DX := x2;
      Intr(51,regs);
      regs.AX := 8;
      regs.CX := y1;
      regs.DX := y2;
      Intr(51,regs);
   END;
END;

{------------------------- TEnvironment ------------------------------------}
{    You can use those procedures to make sure that some procedure leaves   }
{    the environment exactly the way it found it.                           }


PROCEDURE TEnvironment.Save;
{saves current color, view Port, line, fill and text settings.}
BEGIN
   color := GetColor;
   x := GetX;
   y := GetY;
   GetViewSettings( view);
   GetLineSettings( line);
   GetTextSettings( text);
   GetFillSettings( fill);
   vpNum := ViewPortNumber;
END;

PROCEDURE TEnvironment.Standardize;
{sets default color, view Port, line and text settings.}
BEGIN
   SetViewPort ( 0,0,GetMaxX,GetMaxY,clipOn);
   SetLineStyle( SolidLn, 0, NormWidth);
   SetTextStyle( DefaultFont, HorizDir, 1);
   SetTextJustify( LeftText, TopText);
   SetColor( white);
   MoveTo(0,0);
END;

PROCEDURE TEnvironment.Reset;
{Reset stored color, view Port, line, fill and text settings.}
BEGIN
   SetColor( color);
   WITH view DO SetViewPort ( x1,y1,x2,y2,clip);
   WITH line DO SetLineStyle( LineStyle, Pattern, Thickness);
   WITH fill DO SetFillStyle( Pattern, Color);
   WITH text DO SetTextStyle( Font, Direction, CharSize);
   WITH text DO SetTextJustify( Horiz, Vert);
   moveTo(x,y);
   ViewPortNumber := vpNum;
END;


{-----------------------------------------------------------------------}

procedure SetTempDrive;
var
   td : Char;
   f  : file of Char;
   OK : boolean;
begin
   TempDrive := GetEnv('TEMP');
   OK := false;
   REPEAT
      if (TempDrive<>'') then
      begin
    {$I-}
         assign(f,TempDrive+'temp.cup');
         rewrite(f);
         write(f,td);
         close(f);
         erase(f);
    {$I+}
      end;
      IF (TempDrive='') or (IOResult<>0) THEN
      BEGIN
         writeln('Environmental TEMP variable has not been set.');
         writeln('Please, input path to directory for temporary files:');
         write  ('>');
         readln (TempDrive);
      END
      ELSE OK := true;
   UNTIL OK;
end;

BEGIN
   IF (RegisterBGIdriver(@EgaVgaDriverProc) < 0) THEN
   BEGIN
     Writeln('Ega/Vga: ', GraphErrorMsg(GraphResult));
     Pause; Halt;
   END;
   mouseTimer.ResetTimer;
   DoubleClickTime := 10;
   numFilesOpen := 0;
   DelayTime    := 0;
   SetTempDrive;
   Findmacheps(macheps);
   HaltIfError := TRUE;
   ErrorFound  := False;
   event.KeyPressed    := false;
   event.MouseClicked  := false;
   event.ExtendedKey   := false;
   event.doubleClicked := false;
   MousePresent        := MouseDetected;
   RowHt    := 20;
   ColWidth := 8;
END.
