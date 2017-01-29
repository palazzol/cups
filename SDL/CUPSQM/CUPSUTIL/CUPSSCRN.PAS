                 (**********************************************)
                 (**********************************************)
                 (**                                          **)
                 (**     Unit CUPSscrn - Input Screens        **)
                 (**     Written by Jarek Tuszynski           **)
                 (**     for CUPS Project                     **)
                 (**     Department of Physics and Astronomy  **)
                 (**     George Mason University              **)
                 (**     Fairfax, Virginia 22030              **)
                 (**     (c) 1994 by Jarek Tuszynski          **)
                 (**     Originated:  92/08/27                **)
                 (**     Version: 1.3 (94/11/14)              **)
                 (**                                          **)
                 (**********************************************)
                 (**********************************************)


UNIT CUPSscrn;

INTERFACE
USES CUPSmupp, CUPS;

TYPE
   FieldTypeSet = (number,text_,button,CheckBox,RadioButton);

   TDialogPtr = ^TDialogField;
   TDialogField = object             {virtual dialog object}
      fieldtype: FieldTypeSet;       {kind of node}
      next: TDialogPtr;              {pointer to next node}
      boxlength: integer;            {number of characters in the dialog box}
      box: RectType;                 {coordinates of the dialog box}
      CONSTRUCTOR init;
      PROCEDURE SetLimits(min_,max_: real);    VIRTUAL; {see TNumberField}
      PROCEDURE Position(x,y,width: integer);  VIRTUAL; {calculate box coords}
      PROCEDURE Show;                          VIRTUAL; {display dialog port}
      PROCEDURE Read;                          VIRTUAL; {reads new value}
      PROCEDURE SetValue(field: string);       VIRTUAL; {sets new value}
      FUNCTION  GetValue: string;              VIRTUAL; {gets current value}
      FUNCTION  GetID: char;                   VIRTUAL; {see TRadioButtonField}
      FUNCTION  GetName: string;               VIRTUAL; {see TButtonField}
      PROCEDURE StoreBackup;                   VIRTUAL; {stores backup value}
      PROCEDURE RevertChanges;                 VIRTUAL; {cancel changes}
      DESTRUCTOR done;                         VIRTUAL;
   END;

   TNumberField = object(TDialogField)
      value,backup,max,min: real;
      limits: boolean;              {Are there any limits on number stored?}
      CONSTRUCTOR init(num: real);
      PROCEDURE SetLimits(min_,max_: real);    VIRTUAL; {sets limits of num}
      PROCEDURE Show;                          VIRTUAL;
      PROCEDURE Read;                          VIRTUAL;
      FUNCTION  GetValue: string;              VIRTUAL;
      PROCEDURE SetValue(field: string);       VIRTUAL;
      PROCEDURE StoreBackup;                   VIRTUAL;
      PROCEDURE RevertChanges;                 VIRTUAL;
      DESTRUCTOR done;                         VIRTUAL;
   END;

   TStringField = object(TDialogField)
      value,backup: string;
      CONSTRUCTOR init(field: string);
      PROCEDURE Show;                          VIRTUAL;
      PROCEDURE Read;                          VIRTUAL;
      FUNCTION  GetValue: string;              VIRTUAL;
      PROCEDURE SetValue(field: string);       VIRTUAL;
      PROCEDURE StoreBackup;                   VIRTUAL;
      PROCEDURE RevertChanges;                 VIRTUAL;
      DESTRUCTOR done;                         VIRTUAL;
   END;


   TCheckBoxField = object(TDialogField)
      value,backup: boolean;
      CONSTRUCTOR init(flag: boolean);
      PROCEDURE Show;                          VIRTUAL;
      PROCEDURE Read;                          VIRTUAL;
      PROCEDURE Position(x,y,width: integer);  VIRTUAL;
      PROCEDURE Draw(color: integer);          VIRTUAL;
      FUNCTION  GetValue: string;              VIRTUAL;
      PROCEDURE SetValue(field: string);       VIRTUAL;
      PROCEDURE StoreBackup;                   VIRTUAL;
      PROCEDURE RevertChanges;                 VIRTUAL;
      DESTRUCTOR done;                         VIRTUAL;
   END;

   TRadioButtonField = object(TCheckBoxField)
      ID: char;               {ID number to identify buttons from same group}
      CONSTRUCTOR init(ch: char);
      PROCEDURE Draw(color: integer);          VIRTUAL;
      FUNCTION  GetID: char;                   VIRTUAL; {returns ID number}
      DESTRUCTOR done;                         VIRTUAL;
   END;

   TButtonPtr = ^TButtonField;
   TButtonField = object(TCheckBoxField)
      name: string;       {text on the button}
      CONSTRUCTOR init(field: string);
      PROCEDURE Position(x,y,width: integer);  VIRTUAL;
      PROCEDURE Draw(color: integer);          VIRTUAL;
      FUNCTION  getName: string;               VIRTUAL; {returns name}
      DESTRUCTOR done;                         VIRTUAL;
   END;

   StringTpPtr = ^StringTp;
   StringTp = RECORD       {structure used to store static text}
      next: StringTpPtr;   {pointer to next line}
      line: string[80];    {line of text}
   END;

   TDialogScreen = object
      cancelPressed,okPressed : boolean;
      CONSTRUCTOR init;
      PROCEDURE SetNumber( index: integer; num: real);
      PROCEDURE SetNumberLimits( index: integer; min,max: real);
      PROCEDURE SetString( index: integer; field: string);
      PROCEDURE SetBoolean(index: integer; bool: boolean);
      PROCEDURE SetRadioButton(ID: char; n: integer);
      FUNCTION  GetNumber( index: integer): real;
      FUNCTION  GetString( index: integer): string;
      FUNCTION  GetBoolean(index: integer): boolean;
      FUNCTION  GetRadioButton(ID: char): integer;
      PROCEDURE SetHelpScreen(VAR HelpScr_: HelpScrType);
      PROCEDURE SetHelpFile(HelpFileName_,HelpScrName_: string);
      PROCEDURE DefineInputPort( X1, X2, Y1, Y2: REAL);
      PROCEDURE ClearInputPort;
      PROCEDURE LoadLine(inputline: string);
      PROCEDURE Accept;
      PROCEDURE AcceptScreen;
      FUNCTION  Canceled: boolean;
      DESTRUCTOR done;
   PRIVATE
      root        : TDialogPtr;    {root of linked list of dialog fields}
      OK,Esc,Help_: TDialogPtr;    {pointers to OK,Cancel and Help buttons}
      text        : StringTpPtr;   {static text storage}
      inputPort   : RectType;      {enclosing rectangle}
      numLines    : integer;       {number of lines of text}
      active      : boolean;       {is dialog screen currently displayed?}
      HelpScr     : ^HelpScrType;  {pointer to help screen text (if avaiable)}
      HelpFileName,HelpScrName: string;
      phase       : integer;
      PROCEDURE Display;           {display Input Screen}
      PROCEDURE SetPort(x_pos, width: integer; fieldtag: char; field: string);
      PROCEDURE SaveTextLine(line: string); {store Line of static text}
      PROCEDURE FindField(index: integer; VAR dPtr: TDialogPtr);
      PROCEDURE SelectRadioButton(rb: TDialogPtr);
      PROCEDURE SetScrnRadioButtons;
      PROCEDURE SetIfBoolean(current: TDialogPtr);  {if boolean then set it}
      PROCEDURE LookForOkEsc;      {set OK,Esc and Help pointers}
      PROCEDURE ResetButtons;
      PROCEDURE StoreBackup;       {store backup copies of all fields}
      PROCEDURE RevertChanges;
      PROCEDURE CheckForErrors(level : integer; proc : string);
   END;


{###########################################################################}


IMPLEMENTATION
USES crt, graph;

{---------------------------------------------------------------------------}
PROCEDURE Error(errorStr: string);
{---------------------------------------------------------------------------}
VAR i: integer;
BEGIN
   ErrorStr := 'Input Screen Error: '+ErrorStr;
   i := GetGraphMode;
   IF GraphResult=0 THEN announce(errorStr) ELSE writeln(errorStr);
   PAUSE;
   HALT;
END;


{---------------------------------------------------------------------------}
PROCEDURE Swap(VAR x,y: real);
{---------------------------------------------------------------------------}
VAR t: real;
BEGIN
   t:=x; x:=y; y:=t;
END;


{---------------------------------------------------------------------------}
PROCEDURE SetRect(VAR r: RectType; x1,y1,x2,y2: integer);
{---------------------------------------------------------------------------}
BEGIN
   r.x1:=x1; r.y1:=y1; r.x2:=x2; r.y2:=y2;
END;


{---------------------------------------------------------------------------}
FUNCTION ClickedInside(box: RectType): boolean;
{---------------------------------------------------------------------------}
VAR vp: ViewPortType;
BEGIN
    GetViewSettings(vp);
    ClickedInside :=
       (event.x>box.x1+vp.x1) AND (event.x<box.x2+vp.x1) AND
       (event.y>box.y1+vp.y1) AND (event.y<box.y2+vp.y1);
END;


{---------------------------------------------------------------------------}
PROCEDURE DrawBox1( box: RectType; color: integer);
{---------------------------------------------------------------------------}
BEGIN
   HideMouse;
   SetColor(blue);
   SetFillStyle(solidFill,blue);
   WITH box DO bar( x1,y1-3,x2+3,y2);
   SetColor(black);
   SetFillStyle(solidFill,color);
   WITH box DO bar( x1,y1,x2,y2 );
   WITH box DO rectangle( x1,y1,x2,y2 );
   ShowMouse;
END;


{---------------------------------------------------------------------------}
PROCEDURE DrawBox( box: RectType; color: integer);
{---------------------------------------------------------------------------}
BEGIN
   HideMouse;
   SetColor(blue);
   SetFillStyle(solidFill,blue);
   WITH box DO bar( x1,y1-3,x2+3,y2);
   SetColor(black);
   SetFillStyle(solidFill,color);
   WITH box DO bar( x1+2,y1-1,x2+2,y2-1 );
   WITH box DO rectangle( x1+2,y1-1,x2+2,y2-1 );
   ShowMouse;
END;


{---------------------------------------------------------------------------}
PROCEDURE Draw3DBox( box: RectType; color: integer);
{---------------------------------------------------------------------------}
BEGIN
   SetColor(blue);
   SetFillStyle(solidFill,blue);
   HideMouse;
   WITH box DO bar( x1,y1-3,x2+3,y2 );
   SetColor(black);
   SetFillStyle(solidFill,color);
   WITH box DO bar3d( x1,y1,x2,y2,3,true);
   ShowMouse;
END;


{---------------------------------------------------------------------------}
PROCEDURE blink(head: pointType; textlength, cursor: integer);
{---------------------------------------------------------------------------}
{input: head       - point you start writing from}
{       textlength - length of text cursor might be under}
{       cursor     - position of the cursor}
CONST BlinkingRate = 40;
VAR
   counter: integer;
   OnOff : boolean;
   underline, textstring, blanks: string;
BEGIN
   blanks    := '';
   underline := '';
   FOR counter := 1 TO textlength-1 DO blanks := concat(blanks,' ');
   FOR counter := 1 TO textlength   DO underline := concat(underline,'_');
   textstring := blanks;
   insert('_',textstring,cursor);
   counter := 0;
   WITH event DO
   REPEAT
      CheckForEvents;
      IF counter>BlinkingRate THEN
      BEGIN
         IF OnOff THEN        {erase cursor}
         BEGIN
            SetColor(white);
            OutTextXY(head.x, head.y+1, underline);
         END
         ELSE
         BEGIN                {draw cursor}
            SetColor(black);
            OutTextXY(head.x, head.y+1, textstring);
         END;
         counter := 0;
         OnOff := NOT OnOff;
      END
      ELSE
      BEGIN
         delay(5);
         inc(counter);
      END;
   UNTIL keyPressed OR mouseClicked;
   SetColor(white);
   OutTextXY(head.x, head.y+1, underline);  {erase cursor}
END;


{---------------------------------------------------------------------------}
PROCEDURE DelSpaces(VAR field: string);
{---------------------------------------------------------------------------}
{Deletes spaces from the end of the string}
VAR n: integer;
BEGIN
   n := length(field);
   WHILE (n>0) AND (field[n]=' ') do
   BEGIN
      delete(field,n,1);
      dec(n);
   END;
END;



{---------------------------- TDialogField ---------------------------------}



{---------------------------------------------------------------------------}
CONSTRUCTOR TDialogField.init;
{---------------------------------------------------------------------------}
BEGIN
   next := NIL;
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogField.Position(x,y,width: integer);
{---------------------------------------------------------------------------}
VAR cx,cy: integer;
BEGIN
   boxlength := width;
   SetRect( box, round((x-0.7)*colWidth),  (y-1)*RowHt+4,
                 round((x+0.3+boxlength)*colWidth), y*rowHt);
END;


{---------------------------------------------------------------------------}
FUNCTION  TDialogField.GetID: char;
{---------------------------------------------------------------------------}
BEGIN
   GetID := ' ';
END;


PROCEDURE TDialogField.Show;                         BEGIN END;
PROCEDURE TDialogField.Read;                         BEGIN END;
PROCEDURE TDialogField.SetValue(field: string);      BEGIN END;
FUNCTION  TDialogField.GetValue: string;             BEGIN END;
PROCEDURE TDialogField.SetLimits(min_,max_: real);   BEGIN END;
FUNCTION  TDialogField.GetName: string;              BEGIN END;
PROCEDURE TDialogField.StoreBackup;                  BEGIN END;
PROCEDURE TDialogField.RevertChanges;                BEGIN END;
DESTRUCTOR TDialogField.done;                        BEGIN END;



{------------------------------- TStringField ------------------------------}


{---------------------------------------------------------------------------}
CONSTRUCTOR TStringField.init(field: string);
{---------------------------------------------------------------------------}
BEGIN
   next  := NIL;
   fieldtype  := text_;
   DelSpaces(field);
   value := field;
END;


{---------------------------------------------------------------------------}
PROCEDURE TStringField.Show;
{---------------------------------------------------------------------------}
BEGIN
   Draw3DBox( box,white);
   SetTextJustify(LeftText,CenterText);
   setColor(black);
   HideMouse;
   WITH box DO OutTextXY(x1+3, (y2+y1) DIV 2, value);
   ShowMouse;
   SetTextJustify(LeftText,TopText);
END;


{---------------------------------------------------------------------------}
PROCEDURE TStringField.Read;
{---------------------------------------------------------------------------}
VAR
   v: real;
   z: char;
   head: pointType;
   TextInfo: TextSettingsType;
   x,y,i,cursor: integer;
   oldText,line,blanks,underline,cursorLine: string;
   exitFlag: boolean;
   vp : viewPortType;

   PROCEDURE print( text: string; color: integer);
   {erase old text and prints a new one}
   BEGIN
      SetColor(white);
      HideMouse;
      OutTextXY(head.x, head.y, oldtext);
      SetColor(color);
      OutTextXY(head.x, head.y, text);
      ShowMouse;
      oldtext := text;
   END;

BEGIN
   GetTextSettings( TextInfo);
   GetViewSettings(vp);
   SetTextJustify ( LeftText,CenterText);
   head.x := box.x1+5;
   head.y := (box.y2+box.y1) DIV 2;
   line   := value;
   oldtext:= '';
   DrawBox(box,white);
   IF boxlength=length(line) THEN cursor:=boxlength
   ELSE cursor:=length(line)+1;
   exitFlag  := false;
   REPEAT
      print(line,red);
      blink(head,boxlength,cursor);
      z := event.readKey;
      IF event.extendedKey THEN
       CASE z OF
        'M': IF cursor<boxlength THEN inc(cursor)
             ELSE exitFlag:=true;  {right arrow}
        'K': IF cursor>0 THEN dec(cursor)
             ELSE exitFlag:=true;     {Left  arrow}
        'S': IF length(line)>=cursor THEN delete(line,cursor,1);   {Delete}
             ELSE exitFlag := true;
       END
      ELSE IF event.keyPressed THEN
       IF (z>=' ') and (z<='}') THEN
        IF (length(line)<boxlength) THEN
        BEGIN
           insert(z,line,cursor);
           IF (cursor<>boxlength) THEN inc(cursor);
        END
        ELSE Announce('  No more space!  ')
       ELSE
        CASE z OF
         chr(13),chr(27),chr(9) :
             exitFlag:=true;                   {return,esc}
         chr(8) :
             IF cursor>1 THEN                    {backSpace}
             BEGIN
                dec(cursor);
                delete(line,cursor,1);
             END;
        END; {case}
     IF event.mouseClicked THEN
      IF ClickedInside(box) THEN
       cursor := (event.x-vp.x1-box.x1-3) DIV ColWidth +1
      ELSE exitFlag := true;
   UNTIL exitFlag;
   IF (z<>chr(27)) THEN value := line;
   SetTextJustify( TextInfo.Horiz,TextInfo.vert);
   Show;
END;


{---------------------------------------------------------------------------}
FUNCTION TStringField.GetValue: string;
{---------------------------------------------------------------------------}
BEGIN
   GetValue := value;
END;


{---------------------------------------------------------------------------}
PROCEDURE TStringField.SetValue(field: string);
{---------------------------------------------------------------------------}
BEGIN
   value := copy(field,1,boxlength);
END;


{---------------------------------------------------------------------------}
PROCEDURE TStringField.StoreBackup;
{---------------------------------------------------------------------------}
BEGIN
   backup := value;
END;


{---------------------------------------------------------------------------}
PROCEDURE TStringField.RevertChanges;
{---------------------------------------------------------------------------}
BEGIN
   Value := backup;
END;


{---------------------------------------------------------------------------}
DESTRUCTOR TStringField.done;
{---------------------------------------------------------------------------}
BEGIN
END;



{------------------------------ TNumberField -------------------------------}



{---------------------------------------------------------------------------}
CONSTRUCTOR TNumberField.init(num: real);
{---------------------------------------------------------------------------}
BEGIN
   next   := NIL;
   limits := false;
   fieldtype:= number;
   value  := num;
END;


{---------------------------------------------------------------------------}
PROCEDURE TNumberField.SetLimits(min_,max_: real);
{---------------------------------------------------------------------------}
{Sets limits on the numerical value}
BEGIN
   limits := true;
   min := min_;
   max := max_;
   IF value>max THEN value := max;
   IF value<min THEN value := min;
END;


{---------------------------------------------------------------------------}
PROCEDURE TNumberField.Show;
{---------------------------------------------------------------------------}
VAR nStr: string;
BEGIN
   nStr := Num2Str(value,boxlength);
   IF length(nStr)>boxlength THEN
   BEGIN
      nStr  := '0';
      value := 0;
   END;
   Draw3DBox( box,white);
   SetTextJustify(RightText,CenterText);
   setColor(black);
   HideMouse;
   WITH box DO OutTextXY(x2-3, (y2+y1) DIV 2, nStr);
   ShowMouse;
   SetTextJustify(LeftText,TopText);
END;


{---------------------------------------------------------------------------}
PROCEDURE TNumberField.Read;
{---------------------------------------------------------------------------}
VAR
   z: char;
   head,e: pointType;
   TextInfo: TextSettingsType;
   x,y,i,cursor: integer;
   oldText,line,blanks,underline,cursorLine: string;
   exitFlag: boolean;
   vp: ViewPortType;

   PROCEDURE print( text: string; color: integer);
   {erase old text and prints a new one}
   BEGIN
      SetColor(white);
      HideMouse;
      OutTextXY(head.x, head.y, oldtext);
      SetColor(color);
      OutTextXY(head.x, head.y, text);
      ShowMouse;
      oldtext := text;
   END;

BEGIN
   head.x   := box.x1+5;
   head.y   := (box.y2+box.y1) DIV 2;
   exitFlag := false;
   line     := Num2Str(value,boxlength);
   oldtext  := '';
   cursor   := 1;
   DrawBox(box,white);
   GetTextSettings( TextInfo);
   SetTextJustify ( LeftText,CenterText);
   GetViewSettings(vp);
   REPEAT
      DelSpaces(line);
      print(line,red);
      blink(head,boxlength,cursor);
      z := event.readKey;
      IF event.extendedKey THEN
       CASE z OF
        'M': IF cursor<boxlength THEN inc(cursor)
             ELSE exitFlag:=true;   {right arrow}
        'K': IF cursor>0 THEN dec(cursor)
             ELSE exitFlag:=true;      {Left  arrow}
        'S': IF length(line)>=cursor THEN delete(line,cursor,1);    {Delete}
        ELSE exitFlag := true;
       END
      ELSE
        IF event.keyPressed THEN
         CASE z OF
          '0'..'9','-','.','e','E':
              BEGIN
                 IF length(line)>=cursor THEN line[cursor] := z
                 ELSE line := line + z;
                 IF (cursor<>boxlength) THEN inc(cursor);
              END;
          ' ': IF length(line)>=cursor THEN delete(line,cursor,1);  {Delete}
          chr(13),chr(27),chr(9) :
              exitFlag:=true;                   {return,esc}
          chr(8) :
              IF cursor>1 THEN                    {backSpace}
              BEGIN
                 dec(cursor);
                 delete(line,cursor,1);
              END;
           ELSE announce('  Number was expected!  ');
         END; {case}
      IF event.mouseClicked THEN
       IF ClickedInside(box) THEN
        cursor := (event.x-vp.x1-box.x1+4) DIV ColWidth
       ELSE exitFlag := true;
      if exitFlag AND (z<>chr(27)) then
      begin
         Val(line,value,i);
         IF i<>0 THEN announce('Syntax error: Try again.');
         IF (i=0) AND limits AND ((value>Max) OR (value<Min)) THEN
            announce('Number out of limits: Try again');
         exitFlag := (i=0) AND ((NOT limits) OR
                     (limits AND (value>=Min) AND (value<=Max)));
      end;
   UNTIL exitFlag;
   Show;
   SetTextJustify( TextInfo.Horiz,TextInfo.vert);
END;


{---------------------------------------------------------------------------}
FUNCTION TNumberField.GetValue: string;
{---------------------------------------------------------------------------}
BEGIN
   GetValue := Num2Str(value,boxlength);
END;


{---------------------------------------------------------------------------}
PROCEDURE TNumberField.SetValue(field: string);
{---------------------------------------------------------------------------}
VAR
   i: integer;
BEGIN
   Val(field,value,i);
END;


{---------------------------------------------------------------------------}
PROCEDURE TNumberField.StoreBackup;
{---------------------------------------------------------------------------}
BEGIN
   backup := value;
END;


{---------------------------------------------------------------------------}
PROCEDURE TNumberField.RevertChanges;
{---------------------------------------------------------------------------}
BEGIN
   Value := backup;
END;


{---------------------------------------------------------------------------}
DESTRUCTOR TNumberField.done;
{---------------------------------------------------------------------------}
BEGIN
END;



{------------------------------ TCheckBoxField -----------------------------}



{---------------------------------------------------------------------------}
CONSTRUCTOR TCheckBoxField.init(flag: boolean);
{---------------------------------------------------------------------------}
BEGIN
   next  := NIL;
   fieldtype  := CheckBox;
   value := flag;
END;


{---------------------------------------------------------------------------}
PROCEDURE TCheckBoxField.Position(x,y,width: integer);
{---------------------------------------------------------------------------}
VAR ax,ay: word;
BEGIN
   GetAspectRatio(ax,ay);
   SetRect( box, round((x-0.7)*ColWidth), (y-1)*RowHt+4,
                 round((x-0.7)*ColWidth+(RowHt-6.0)*ax/ay), y*rowHt-2);
END;


{---------------------------------------------------------------------------}
PROCEDURE TCheckBoxField.Show;
{---------------------------------------------------------------------------}
BEGIN
   Draw(white);
END;


{---------------------------------------------------------------------------}
PROCEDURE TCheckBoxField.Draw(color: integer);
{---------------------------------------------------------------------------}
{draws Check Box with "color" color of background}
BEGIN
   DrawBox1( box,color);
   IF value THEN
    WITH box DO
    BEGIN
       HideMouse;
       line(x1,y1,x2,y2);
       line(x1,y2,x2,y1);
       ShowMouse;
    END;
END;


{---------------------------------------------------------------------------}
PROCEDURE TCheckBoxField.Read;
{---------------------------------------------------------------------------}
VAR
   OnOff: boolean;
   counter: integer;
   BlinkingRate: integer;
BEGIN
   BlinkingRate := 60;
   SetColor(black);
   REPEAT
      CheckForEvents;
      IF counter>BlinkingRate THEN
       BEGIN
          IF OnOff THEN Draw(white) ELSE Draw(DarkGray);
          counter := 0;
          OnOff := NOT OnOff;
       END
      ELSE
       BEGIN
          delay(5);
          inc(counter);
       END;
   UNTIL event.keyPressed OR event.mouseClicked;
   Draw(white);
  { IF (event.keyPressed AND (event.readKey=' ')) then value := not value;}
END;


{---------------------------------------------------------------------------}
FUNCTION TCheckBoxField.GetValue: string;
{---------------------------------------------------------------------------}
BEGIN
   IF value THEN GetValue := 'T' ELSE GetValue := 'F';
END;


{---------------------------------------------------------------------------}
PROCEDURE TCheckBoxField.SetValue(field: string);
{---------------------------------------------------------------------------}
BEGIN
   value := (field='T');
END;


{---------------------------------------------------------------------------}
PROCEDURE TCheckBoxField.StoreBackup;
{---------------------------------------------------------------------------}
BEGIN
   backup := value;
END;


{---------------------------------------------------------------------------}
PROCEDURE TCheckBoxField.RevertChanges;
{---------------------------------------------------------------------------}
BEGIN
   IF fieldtype<>button THEN Value := backup;
END;


{---------------------------------------------------------------------------}
DESTRUCTOR TCheckBoxField.done;
{---------------------------------------------------------------------------}
BEGIN
END;



{------------------------------- TRadioButtonField -------------------------}



{---------------------------------------------------------------------------}
CONSTRUCTOR TRadioButtonField.init(ch: char);
{---------------------------------------------------------------------------}
BEGIN
   next  := NIL;
   ID    := ch;
   fieldtype:= RadioButton;
   value := false;
END;


{---------------------------------------------------------------------------}
PROCEDURE TRadioButtonField.Draw(color: integer);
{---------------------------------------------------------------------------}
{Draws one Radio Button with the "color" color of background                }
VAR
   xCent,yCent: integer;
BEGIN
   WITH box DO
   BEGIN
      xCent := (x1+x2) DIV 2;
      yCent := (y1+y2) DIV 2;
      SetFillStyle(SolidFill,color);
      SetColor(black);
      HideMouse;
      FillEllipse(XCent,Ycent,XCent-x1,YCent-y1);
      IF value THEN
      BEGIN
         SetFillStyle(SolidFill,black);
         FillEllipse(XCent,Ycent,(XCent-x1) DIV 2, (YCent-y1) DIV 2);
      END;
      ShowMouse;
   END;
END;


{---------------------------------------------------------------------------}
FUNCTION TRadioButtonField.GetID: char;
{---------------------------------------------------------------------------}
BEGIN
   GetID := ID;
END;


{---------------------------------------------------------------------------}
DESTRUCTOR TRadioButtonField.done;
{---------------------------------------------------------------------------}
BEGIN
END;



{---------------------------------- TButtonField ---------------------------}



{---------------------------------------------------------------------------}
CONSTRUCTOR TButtonField.init(field: string);
{---------------------------------------------------------------------------}
VAR i,j : integer;
BEGIN
   next  := NIL;
   value := false;
   fieldtype:= button;
   name  := field;
   for i:=length(field) downto 1 do
    IF (field[i]=' ') then Delete(field,i,1)    {delete spaces}
    else field[i] := upCase(field[i]);          {change to uper-case}
   IF (field='OK')     THEN name := '  OK  ';
   IF (field='CANCEL') THEN name := 'Cancel';
   IF (field='HELP')   THEN name := ' Help ';
   boxlength := length(name);
END;


{---------------------------------------------------------------------------}
PROCEDURE TButtonField.Position(x,y,width: integer);
{---------------------------------------------------------------------------}
BEGIN
   SetRect( box, round((x-0.7)*ColWidth),        (y-1)*RowHt+4,
                 round((x+0.3+boxlength)*ColWidth),  y*RowHt-1);
END;


{---------------------------------------------------------------------------}
PROCEDURE TButtonField.draw(color: integer);
{---------------------------------------------------------------------------}
{ Draws button with given color                                             }
BEGIN {first invert colors, so TCheckBox draw and read work without changes }
   IF color=15 THEN color:=7 ELSE color:=15;   {invert colors, so TCheckBox}
   IF name = '  OK  ' THEN                     {Highlight [  OK  ] button}
     BEGIN
        SetLineStyle(SolidLn,0,ThickWidth);
        Draw3DBox(box,color);
        SetLineStyle(SolidLn,0,NormWidth);
     END
   ELSE Draw3DBox(box,color);
   SetTextJustify(CenterText,CenterText);
   HideMouse;
   WITH box DO OutTextXY((x2+x1) DIV 2, (y2+y1) DIV 2, name);
   ShowMouse;
   SetTextJustify(LeftText,TopText);
END;


{---------------------------------------------------------------------------}
FUNCTION  TButtonField.GetName: string;
{---------------------------------------------------------------------------}
BEGIN
   GetName := name;
END;


{---------------------------------------------------------------------------}
DESTRUCTOR TButtonField.done;
{---------------------------------------------------------------------------}
BEGIN
END;



{-------------------------- private TDialogScreen --------------------------}



{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.FindField(index: integer; VAR dPtr: TDialogPtr);
{---------------------------------------------------------------------------}
{Finds field with given index}
VAR i: integer;
BEGIN
   dPtr := root;
   IF root<>nil THEN
    FOR i := 1 TO index-1 DO
      IF (dPtr^.next<>nil) THEN dPtr:=dPtr^.next ELSE
      error('TInputScreen: There is no input field corresponding to index #'
            +NumStr(index,0,0)+'.')
   ELSE error('This input screen does not have any input fields.');
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.SetPort(x_pos, width: integer; fieldtag: char;
                                field: string);
{---------------------------------------------------------------------------}
{Creates new node in link list, and initializes it}
VAR
   dPtr: TDialogPtr;
   nPtr: ^TNumberField;
   cPtr: ^TCheckBoxField;
   sPtr: ^TStringField;
   rPtr: ^TRadioButtonField;
   bPtr: TButtonPtr;
   code,i: integer;
   num: real;
   spaces: boolean;
BEGIN
   dPtr := root;
   IF root<>nil THEN
     WHILE dPtr^.next<>nil DO dPtr:=dPtr^.next;
   CASE fieldtag OF
    '"': BEGIN
            new(sPtr,init(field));
            IF root=nil THEN root:=sPtr ELSE dPtr^.next:=sPtr;
            dPtr := sPtr;
         END;
    '{': BEGIN
            Spaces := true;
            FOR i := 1 TO length(field) DO
              Spaces := Spaces AND (field[i]=' ');
            IF spaces THEN num := 0 ELSE val(field,num,code);
            new(nPtr,init(num));
            IF (NOT spaces) AND (code<>0) THEN
              error('TInputScreen.LoadLine: '+field+
                    ' can''t be converted to a number');
            IF root=nil THEN root:=nPtr ELSE dPtr^.next:=nPtr;
            dPtr := nPtr;
         END;
    '[': BEGIN
            new(bPtr,init(field));
            IF root=nil THEN root:=bPtr ELSE dPtr^.next:=bPtr;
            dPtr := bPtr;
         END;
    '#': IF (field='T') OR (field='F') THEN
          BEGIN
             new(cPtr,init(field='T'));
             IF root=nil THEN root:=cPtr ELSE dPtr^.next:=cPtr;
            dPtr := cPtr;
          END
         ELSE
          BEGIN
             new(rPtr,init(field[1]));
             IF root=nil THEN root:=rPtr ELSE dPtr^.next:=rPtr;
             dPtr := rPtr;
          END;
  END;
  dPtr^.Position(x_pos,numLines,width);
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.SaveTextLine(line: string);
{---------------------------------------------------------------------------}
VAR
   tPtr,TextLine: StringTpPtr;
BEGIN
   IF length(line)>80 THEN line:=copy(line,1,80);
   new(textLine);
   textLine^.line := line;
   textLine^.next := NIL;
   tPtr := text;
   IF text<>nil THEN
   BEGIN
      WHILE tPtr^.next<>nil DO tPtr:=tPtr^.next;
      tPtr^.next:=textLine;
   END
   ELSE
      text:=textLine;
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.SetScrnRadioButtons;
{---------------------------------------------------------------------------}
{Makes sure that only one button from each group is set to true.}
VAR
   ID: char;
   dPtr,cPtr: TDialogPtr;
   flag: boolean;
BEGIN
   dPtr := root;
   WHILE dPtr<>nil DO
   BEGIN
      IF dPtr^.fieldtype=RadioButton THEN
      BEGIN
         ID   := dPtr^.GetID;
         flag := false;
         cPtr := root;
         WHILE (cPtr<>nil) DO
         BEGIN
            IF (cPtr^.GetID=ID) THEN
            BEGIN
               IF flag THEN cPtr^.SetValue('F');       {check until one set}
               flag := flag OR (cPtr^.GetValue='T');   {then turn off rest }
            END;
            cPtr:=cPtr^.next;
         END;                   { if none set then turn on the current one.}
         IF NOT flag THEN SelectRadioButton(dPtr);
      END;
      dPtr:=dPtr^.next;
   END;
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.SelectRadioButton(rb: TDialogPtr);
{---------------------------------------------------------------------------}
{Sets RadioButton to true and reset all the rest from the same group.}
VAR
   id: char;
   dPtr: TDialogPtr;
BEGIN
   id := rb^.getID;
   rb^.SetValue('T');
   dPtr := root;
   WHILE dPtr<>nil DO
   BEGIN
      IF (dPtr^.fieldtype=RadioButton) AND (dPtr<>rb)
               AND (dPtr^.GetID=ID) THEN
        BEGIN
          dPtr^.SetValue('F');
          IF active THEN dPtr^.Show;
        END;
      dPtr:=dPtr^.next;
   END;
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.ResetButtons;
{---------------------------------------------------------------------------}
VAR
   dPtr: TDialogPtr;
BEGIN
   dPtr := root;
   WHILE dPtr<>nil DO
   BEGIN
     IF (dPtr^.fieldtype=Button) THEN dPtr^.SetValue('F');
     dPtr:=dPtr^.next;
   END;
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.LookForOkEsc;
{---------------------------------------------------------------------------}
VAR
   dPtr: TDialogPtr;
BEGIN
   dPtr := root;
   WHILE dPtr<>nil DO
   BEGIN
     IF (dPtr^.fieldtype=Button) AND (dPtr^.GetName='  OK  ') THEN OK   :=dPtr;
     IF (dPtr^.fieldtype=Button) AND (dPtr^.GetName='Cancel') THEN Esc  :=dPtr;
     IF (dPtr^.fieldtype=Button) AND (dPtr^.GetName=' Help ') THEN Help_:=dPtr;
     dPtr:=dPtr^.next;
   END;
   IF OK =nil THEN new(TButtonPtr(ok), init('  OK  '));
   IF esc=nil THEN new(TButtonPtr(esc),init('Cancel'));
   IF (Help_<>nil) AND (HelpScr=nil) AND (HelpScrName='') THEN
    Error('You have "Help" button, but no help screen.');
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.Display;
{---------------------------------------------------------------------------}
VAR
   dPtr: TDialogPtr;
   tPtr: StringTpPtr;
   n : integer;
BEGIN
   if NumLines=0 then error('TInputScreen.Accept: Input Screen not Defined');
   SetScrnRadioButtons;
   LookForOkEsc;
   SetColor(white);
   SetFillStyle(SolidFill,blue);
   WITH InputPort DO                      {draw blue background}
   BEGIN
      SetViewPort(x1,y1,x2,y2,ClipOn);
      HideMouse;
      Bar(0,0,x2-x1,y2-y1);
      Rectangle(0,0,x2-x1,y2-y1);
      ShowMouse;
   END;
   tPtr := text;                          {print all the static text}
   FOR n := 1 TO numLines DO
   BEGIN
      Print(2,n,tPtr^.line);
      tPtr:=tPtr^.next;
   END;
   dPtr := root;                          {print all the fields}
   WHILE dPtr<>nil DO
   BEGIN
      dPtr^.Show;
      dPtr:=dPtr^.next;
   END;
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.StoreBackup;
{---------------------------------------------------------------------------}
VAR
   dPtr: TDialogPtr;
BEGIN
   dPtr := root;
   WHILE dPtr<>nil DO
   BEGIN
      dPtr^.StoreBackup;
      dPtr:=dPtr^.next;
   END;
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.RevertChanges;
{---------------------------------------------------------------------------}
VAR
   dPtr: TDialogPtr;
BEGIN
   dPtr := root;
   WHILE dPtr<>nil DO
   BEGIN
      dPtr^.RevertChanges;
      dPtr:=dPtr^.next;
   END;
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.SetIfBoolean(current: TDialogPtr);
{---------------------------------------------------------------------------}
BEGIN
   IF (current^.fieldtype=button) THEN
   BEGIN
      Current^.SetValue('T');                                {turn on}
      active := false;                               {leave input screen}
   END;
   IF (current^.fieldtype=CheckBox) THEN
     IF Current^.GetValue='T' THEN Current^.SetValue('F')  {set to opposite}
     ELSE Current^.SetValue('T');
   IF current^.fieldtype=RadioButton THEN SelectRadioButton(current);
END;

{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.CheckForErrors(level : integer; proc : string);
{---------------------------------------------------------------------------}
BEGIN
   if (phase<1) or (phase>3) then
    Error('TInputScreen.'+proc+': Object not Initialized.');
   if (phase=3) then
    Error('TInputScreen.'+proc+
          ': You can''t call any object method after calling "Done".');
   if (level=1) and (NumLines=0) then
    error('TInputScreen.'+proc+': Input Screen not Defined yet.');
   if (level=2) and (phase=1) then
    error('TInputScreen.'+proc+': You have to call "Accept" procedure first.');
END;


{--------------------------- public TDialogScreen --------------------------}



{---------------------------------------------------------------------------}
CONSTRUCTOR TDialogScreen.Init;
{---------------------------------------------------------------------------}
{   This procedure must be called first, before any other method of this    }
{   object is called.                                                       }
BEGIN
   cancelPressed := false;
   okPressed := false;
   phase    := 1;      {initialization}
   numLines := 0;
   active   := false;
   text     := NIL;
   root     := NIL;
   esc      := NIL;
   OK       := NIL;
   Help_    := NIL;
   HelpScr  := NIL;
   HelpScrName  := '';
   HelpFileName := '';
   SetRect( InputPort, 30, 30, GetMaxX-30, GetMaxY-30);
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.LoadLine(inputline: string);
{---------------------------------------------------------------------------}
{   Rules for numerical, text and boolean fields are coded:                 }
{                                                                           }
{   * Static Text (that cannot be modified): all characters                 }
{     that are not enclosed by one of the four sets of field markers are    }
{     read as static text.                                                  }
{                                                                           }
{   * Modifiable text fields must be placed between double-quotes.          }
{                                                                           }
{   * Numerical fields must be enclosed between braces (curly brackets).    }
{                                                                           }
{   * Check boxes are the independent boolean fields                        }
{                              and must be given the field value '#T'       }
{     (field initially true) or '#F' (field initially false).               }
{                                                                           }
{   * Radio Buttons belong to a set of boolean fields in which only one     }
{     field can be TRUE; all others automatically are forced to FALSE.  In  }
{     MUPPET all the Radio Buttons on a given input screen automatically    }
{     belonged to the same set. In new version, Radio Buttons are organized }
{     into groups and many groups can be placed on the same input screen.   }
{     The field of every Radio Button in a group is set as '#c' where 'c' is}
{     any character EXCEPT 'T' or 'F' but must be the SAME for every Radio  }
{     Button in the SAME group. This character 'c' must be different for    }
{     each group. I suggest using numbers for these ID characters, so for   }
{     example all the fields in one group might be marked by '#1'.          }
{     Initially, the first field in each group is set to be TRUE. If you    }
{     want some other field from the group to be initially set to TRUE use  }
{     the SetValue procedure described below.                               }
{   * Button is a new structure created to provide 'OK', 'Cancel', and      }
{     'Help' buttons that can be clicked or selected by keyboard.  The      }
{     fields in each of these buttons must be exactly six characters long   }
{     including blank spaces. Each input screen must have these both an     }
{     'OK' and a 'Cancel' button, otherwise an error message is issued.     }
{     Define these buttons by placing the expressions '[  OK  ]' and        }
{     '[Cancel]' where you want them to appear on the input screen .        }
{     Pressing <Return> has the same effect as clicking the 'OK' button     }
{     with the mouse or selecting it from the keyboard. Pressing the <Esc>  }
{     key is the same as selecting 'Cancel'. A '[ Help ]' button will       }
{     display a help screen specified by using the procedure GiveHelp(      }
{     helpScr: helpScrType).   In addition, you can create buttons other    }
{     then 'OK', 'Cancel' and 'Help' simply by enclosing a name of six      }
{     characters including blanks in quotes. These buttons are treated as   }
{     boolean fields that are initially FALSE although this setting is not  }
{     indicated in any way.  When a button is selected is value is set to   }
{     TRUE.                                                                 }

VAR
   i,j,n: integer;
   textpart,field: string;
   fieldtag: char;
BEGIN
   CheckForErrors(0,'LoadLine');
   inc(numLines);
   i := 1;
   textpart := '';
   REPEAT
      j := i;
      WHILE NOT ((i>length(inputline)) OR (inputline[i] IN ['"','{','[','#']))
          DO inc(i);
      textpart := concat(textpart,copy(inputline,j,i-j));
      IF i<=length(inputline) THEN
      BEGIN
         fieldtag := inputline[i];
         inc(i);
         j := i;
         IF fieldtag<>'#' THEN
           BEGIN
           WHILE
             NOT ((i>length(inputline)) OR (inputline[i] IN ['"','}',']'])) DO
                inc(i);
            IF (i>length(inputline)) THEN
             error('Field not closed with matching delimiter');
           field := copy(inputline,j,i-j);
           END
         ELSE
           field := UpCase(inputline[i]);
         inc(i);
         FOR n := 1 TO i-j+1 DO textpart := concat(textpart,' ');
         SetPort(j,i-j-1,fieldtag,field);
      END;
   UNTIL (i>length(inputline));
   SaveTextLine(textpart);
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.SetNumberLimits( index: integer; min,max: real);
{---------------------------------------------------------------------------}
{   Sets the limits for the value in the numerical field with given index   }
{   to be between 'min' and 'max'.                                          }
VAR
   dPtr: TDialogPtr;
BEGIN
   CheckForErrors(1,'SetNumberLimits');
   FindField(index,dPtr);
   IF dPtr^.fieldtype<>number THEN
     Error(Concat('Input Field #',numstr(index,0,0),' is not numeric.'));
   dPtr^.SetLimits(min,max);
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.SetNumber( index: integer; num: real);
{---------------------------------------------------------------------------}
{   Sets the value in the numerical field with the given index.             }
VAR
   dPtr: TDialogPtr;
BEGIN
   CheckForErrors(1,'SetNumber');
   FindField(index,dPtr);
   IF dPtr^.fieldtype<>number THEN
     Error(ConCat('Input Field #',numstr(index,0,0),' is not numeric.'));
   dPtr^.SetValue(Num2Str(num,dPtr^.boxlength));
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.SetString( index: integer; field: string);
{---------------------------------------------------------------------------}
{   Places the string 'field' in the field with the given index             }
VAR
   dPtr: TDialogPtr;
BEGIN
   CheckForErrors(1,'SetString');
   FindField(index,dPtr);
   IF (dPtr^.fieldtype<>text_) THEN
     Error(ConCat('Input Field #',numstr(index,0,0),' is not a text field.'));
   dPtr^.SetValue(field);
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.SetBoolean(index: integer; bool: boolean);
{---------------------------------------------------------------------------}
{   Sets field with given index (Check Box or Radio Button) to the boolean  }
{   value 'bool'.                                                           }
VAR
   dPtr: TDialogPtr;
   field: string;
BEGIN
   CheckForErrors(1,'SetBoolean');
   FindField(index,dPtr);
   IF (dPtr^.fieldtype<>CheckBox) AND (dPtr^.fieldtype<>RadioButton) THEN
     Error(ConCat('Input Field #',numstr(index,0,0),
                   ' is not a boolean field.'));
   IF bool THEN field:='T' ELSE field:='F';
   dPtr^.SetValue(field);
   IF bool AND (dPtr^.fieldtype=RadioButton) THEN SelectRadioButton(dPtr);
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.SetRadioButton(ID: char; n: integer);
{---------------------------------------------------------------------------}
{   Gives a way of setting the value of a Radio Button besides SetBool.     }
{   In the group of Radio Buttons with the ID character "ID" the value of   }
{   the nth one is set to TRUE.                                             }
VAR
   dPtr,ePtr : TDialogPtr;
   index : integer;
BEGIN
   CheckForErrors(1,'SetRadioButton');
   dPtr  := root;
   index := 0;
   WHILE (dPtr<>nil) AND (n<>index) DO
   BEGIN
      IF (dPtr^.fieldtype=RadioButton) AND (dPtr^.GetID=ID) THEN inc(index);
      ePtr := dPtr;         {ePtr points to one before dPtr}
      dPtr := dPtr^.next;
   END;
   IF (n<1) or (n>index) THEN
    error('In group "'+ID+'" there is no radio button #'+numstr(n,2,0));
   IF (index=0) THEN error('There are no radio-buttons with ID '+ID);
   SelectRadioButton(ePtr);
END;


{---------------------------------------------------------------------------}
FUNCTION TDialogScreen.GetNumber( index: integer): real;
{---------------------------------------------------------------------------}
{   Returns the value from the numerical field with given index.            }
VAR
   dPtr: TDialogPtr;
   i: integer;
   r: real;
BEGIN
   CheckForErrors(2,'GetNumber');
   FindField(index,dPtr);
   IF dPtr^.fieldtype<>number THEN
     Error(ConCat('Input Field #',numstr(index,0,0),' is not numeric.'));
   val(dPtr^.GetValue,r,i);
   GetNumber := r;
END;


{---------------------------------------------------------------------------}
FUNCTION TDialogScreen.GetString( index: integer): string;
{---------------------------------------------------------------------------}
{   Returns the string value from the text field with given index.          }
VAR
   dPtr: TDialogPtr;
BEGIN
   CheckForErrors(2,'GetString');
   FindField(index,dPtr);
   IF dPtr^.fieldtype<>text_ THEN
     Error(ConCat('Input Field #',numstr(index,0,0),' is not text field.'));
   GetString := dPtr^.GetValue;
END;


{---------------------------------------------------------------------------}
FUNCTION TDialogScreen.GetBoolean(index: integer): boolean;
{---------------------------------------------------------------------------}
{   Returns the value from the boolean field (Check Box, Radio Button) with }
{   the given index.                                                        }
VAR
   dPtr: TDialogPtr;
BEGIN
   CheckForErrors(2,'GetBoolean');
   FindField(index,dPtr);
   IF (dPtr^.fieldtype<>CheckBox) AND (dPtr^.fieldtype<>RadioButton)
    AND (dPtr^.fieldtype<>Button) THEN
     Error(ConCat('Input Field #',numstr(index,0,0),
          ' is not boolean field.'));
   GetBoolean := (dPtr^.GetValue='T');
END;


{---------------------------------------------------------------------------}
FUNCTION TDialogScreen.GetRadioButton(ID: char): integer;
{---------------------------------------------------------------------------}
{   Gives a way of reading the value of a Radio Button besides GetBoolean.  }
{   It checks the value of each Radio Button with the ID character "ID" and }
{   returns the index of one set to TRUE.                                   }
VAR
   dPtr: TDialogPtr;
   index: integer;
   quitFlag : boolean;
BEGIN
   CheckForErrors(2,'GetRadioButton');
   dPtr     := root;
   index    := 0;
   quitFlag := false;
   WHILE (dPtr<>nil) AND (NOT quitFlag) DO
   BEGIN
      IF (dPtr^.fieldtype=RadioButton) AND (dPtr^.GetID=ID) THEN
      BEGIN
         inc(index);
         quitFlag := (dPtr^.GetValue='T');
      END;
      dPtr:=dPtr^.next;
   END;
   IF (index=0) THEN Error('There are no radio-buttons with ID '+ID);
   GetRadioButton :=  index;
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.DefineInputPort( X1, X2, Y1, Y2: REAL);
{---------------------------------------------------------------------------}
{  Defines size of input screen.                                            }
BEGIN
   CheckForErrors(0,'DefineInputPort');
   IF (x1 = x2) OR (y1 = y2) THEN
     error('TInputScreen.DefineInputPort: Error in InputPort Values');
   IF x1>x2 THEN swap(x1,x2);
   IF y1>y2 THEN swap(y1,y2);
   SetRect(InputPort, round(X1 * GetMaxX), round((1.0 - Y2) * GetMaxY),
                      round(X2 * GetMaxX), round((1.0 - Y1) * GetMaxY));
END;

{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.ClearInputPort;
{---------------------------------------------------------------------------}
{  Clears input screen.                                                     }
VAR
   environment : TEnvironment;
BEGIN
   CheckForErrors(0,'ClearInputPort');
   Environment.Save;
   SetViewPort(0,0,GetMaxX,GetMaxY,ClipOn);
   SetFillStyle(SolidFill,black);
   HideMouse;
   WITH InputPort DO Bar(x1,y1,x2,y2);
   ShowMouse;
   Environment.Reset;
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.SetHelpScreen(VAR HelpScr_: HelpScrType);
{---------------------------------------------------------------------------}
{   SetHelpScreen(HelpScr) pass the name of help screen, so it can be       }
{   displayed if you have 'Help' button and it was clicked.                 }
BEGIN
   HelpScr := @HelpScr_;
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.SetHelpFile(HelpFileName_,HelpScrName_: string);
{---------------------------------------------------------------------------}
{ - SetHelpFile(HelpFileName,HelpScrName) pass the name of the file where   }
{   help screens can be found and the name of the help-screen you want to   }
{   be displayed if you have 'Help' button and it was clicked.              }
BEGIN
   HelpFileName := HelpFileName_;
   HelpScrName  := HelpScrName_;
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.Accept;
{---------------------------------------------------------------------------}
{   Activate procedure is the same as in MUPPET. It activates an Input Port }
{   and reads the input until <Return> is pressed or the 'OK' button is     }
{   selected.                                                               }
VAR
   current,dPtr: TDialogPtr;
   envir: TEnvironment;

   PROCEDURE GoUp( VAR current: TDialogPtr);
   VAR x,y,dist,min: real;
   BEGIN
       dPtr := root;
       x := 1.0*(current^.box.x2 + current^.box.x1);
       y := 1.0*(current^.box.y2 + current^.box.y1);
       WITH dPtr^.box DO min := sqr(y2+y1-y) + sqr(x2+x1-x);
       WHILE (dPtr<>nil) AND (dPtr^.box.y2<current^.box.y1) DO
       BEGIN
          WITH dPtr^.box DO dist := sqr(y2+y1-y) + sqr(x2+x1-x);
          IF dist < min THEN min := dist;
          dPtr:=dPtr^.next;
       END;
       dPtr := root;
       WITH dPtr^.box DO dist := sqr(y2+y1-y) + sqr(x2+x1-x);
       WHILE (dPtr^.next<>nil) AND (dist<>min) DO
       BEGIN
          dPtr:=dPtr^.next;
          WITH dPtr^.box DO dist := sqr(y2+y1-y) + sqr(x2+x1-x);
       END;
       if (current<>dPtr) then current := dPtr else
       begin
          WHILE (dPtr^.next<>nil) DO dPtr:=dPtr^.next; {PgDn}
          current := dPtr;
       end;
    END;

BEGIN
   CheckForErrors(1,'AcceptScreen');
   Phase := 2;
   envir.Save;
   envir.Standardize;
   StoreBackup;
   display;
   active  := true;
   current := root;
   ResetButtons;
   REPEAT
     current^.Read;
     dPtr := root;
     IF event.mouseClicked THEN
       BEGIN
          WHILE (dPtr<>nil) AND (NOT ClickedInside(dPtr^.box)) DO
            dPtr := dPtr^.next;
          IF dPtr<>nil THEN
           BEGIN
             current := dPtr;
             SetIfBoolean(current);
           END
       END
     ELSE
       IF event.extendedKey THEN
        CASE event.ReadKey OF
         'G':begin                                           {home}
               dPtr := root;
               WHILE (dPtr<>nil) AND (dPtr^.next^.box.y2<current^.box.y1)
               DO dPtr:=dPtr^.next;
               IF (dPtr<>nil) AND (dPtr^.next<>nil) THEN
                IF dPtr=root THEN current:=root ELSE current := dPtr^.next;               END;
         'O': BEGIN                                           {end}
               dPtr := current;
               WHILE (dPtr<>nil) AND (dPtr^.next^.box.y1<current^.box.y2)
               DO dPtr:=dPtr^.next;
               IF dPtr<>nil THEN current := dPtr;
              END;
         'I': current := root;                                {PgUp}
         'Q': BEGIN
               WHILE (dPtr^.next<>nil) DO dPtr:=dPtr^.next; {PgDn}
               current := dPtr;
              END;
         'M': IF current^.next<>nil THEN current := current^.next
              ELSE current := root;                           {right arrow}
         'K': BEGIN
                WHILE (dPtr^.next<>nil) AND (dPtr^.next<>current) DO
                dPtr:=dPtr^.next;                             {Left  arrow}
                current := dPtr;
              END;
         'H': GoUp(current);
         'P': BEGIN                                           {down}
                dPtr := current;
                WHILE (dPtr<>nil) AND ((dPtr^.box.y1<current^.box.y2)
                OR (dPtr^.box.x2<current^.box.x1)) DO dPtr:=dPtr^.next;
                IF dPtr<>nil THEN current := dPtr ELSE current := root;
              END;
         ';': IF Help_<>nil THEN help_^.SetValue('T');           {F1}
        END {case}
     ELSE
       IF event.keyPressed THEN
        CASE Event.ReadKey OF
         chr(13): SetIfBoolean(OK);                          {return}
         chr(27): SetIfBoolean(esc);                         {esc}
         chr(09): IF current^.next=nil THEN current := root ELSE
                   current := current^.next;            {tab}
         ELSE      SetIfBoolean(current);
        END; {case}
     IF (Help_<>nil) AND (Help_^.GetValue='T') THEN
       BEGIN               {if there is help key and it was pressed then...}
         IF (HelpScr<>nil) THEN Help(HelpScr^) ELSE
         IF (HelpScrName<>'') THEN ShowHelp(HelpFileName,HelpScrName);
         help_^.SetValue('F');
         active := true;
       END;
   UNTIL NOT active;
   cancelPressed := (esc^.GetValue='T');
   okPressed := (ok^.GetValue='T');
   IF cancelPressed THEN RevertChanges;
   ClearMKBuffers;         {clear all the events}
   CheckForEvents;
   envir.Reset;
END;


{---------------------------------------------------------------------------}
PROCEDURE TDialogScreen.AcceptScreen;
{---------------------------------------------------------------------------}
{   AcceptScreen procedure saves the background, calls the Activate         }
{   procedure, and puts the background back over the input Screen.          }
VAR
   image: TImage;
   vp: viewPortType;
BEGIN
   GetViewSettings(vp);
   SetViewPort(0,0,GetMaxX,GetMaxY,clipOn);
   WITH InputPort DO
   BEGIN 
      Image.Get(x1,y1,x2,y2);
      Accept;
      Image.Put(x1,y1);
   END;
   WITH vp DO SetViewPort(x1,y1,x2,y2,clipOn);
END;


{---------------------------------------------------------------------------}
FUNCTION TDialogScreen.Canceled: boolean;
{---------------------------------------------------------------------------}
{  Returns TRUE if the [Cancel] button was chosen.                          }
BEGIN
   CheckForErrors(2,'Canceled');
   Canceled := (esc^.GetValue='T');
END;


{---------------------------------------------------------------------------}
DESTRUCTOR TDialogScreen.Done;
{---------------------------------------------------------------------------}
{   Clears all the memory used by Input Screens. No procedure from this     }
{   object can be called after Done is executed.                            }
VAR
   dPtr: TDialogPtr;
   tPtr: StringTpPtr;
   n   : integer;
   EscDone, OkDone : boolean;
BEGIN
   dPtr := root;
   OKDone := (esc<>nil);
   EscDone:= (ok <>nil);
   WHILE dPtr<>nil DO
   BEGIN
      root := dPtr^.next;
      if (dPtr^.fieldtype=Button) AND (dPtr^.GetName='  OK  ') then OKDone := true;
      if (dPtr^.fieldtype=Button) AND (dPtr^.GetName='Cancel') then EscDone:= true;
      dispose(dPtr,done);
      dPtr:=root;
   END;
   if (not EscDone) then dispose(esc,done);
   if (not OKDone)  then dispose(ok ,done);
   tPtr := text;
   FOR n := 1 TO numLines DO
   BEGIN
      text := tPtr^.next;
      dispose(tPtr);
      tPtr := text;
   END;
   EscDone := cancelPressed;
   OkDone  := okPressed;
   init;
   cancelPressed := EscDone;
   okPressed := OkDone;
   phase := 3;   {done}
END;


END. {CUPSscrn}
