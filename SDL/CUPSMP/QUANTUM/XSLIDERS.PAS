           (*************************************************)
           (*************************************************)
           (**                                             **)
           (**  Consortium of Upper-level Physics Software **)
           (**              ( CUPS Project )               **)
           (**      ( c) 1994 by John Wiley & Sons         **)
           (**  Compiled with Utilities Ver. 1.9 (95/09/10)**)
           (**                                             **)
           (*************************************************)
           (*************************************************)


UNIT XSliders;

INTERFACE
USES Graph, CUPSmupp, CUPSscrn, CUPS, CUPSGui, CUPSFunc;

TYPE
      TXSlider = object
      chosen,changed : boolean;
      dVal : real;
      CONSTRUCTOR Create(ValMin, ValMax, VInitial, posX, posY, size_ : real;
         decimals_: integer; Low_,High_,Name_ : string; vertical_ : boolean;
         SliderType_: Integer);
      PROCEDURE Draw;
      PROCEDURE ReDraw;
      FUNCTION  Value : real;
      PROCEDURE Reset(VCurrent_: real);
      PROCEDURE Erase(Color: Integer);
      DESTRUCTOR done;
   PRIVATE
      VMin, VMax, VCurrent : real;
      px, py, size, decimals,NumWidth,NumLength,SliderType: integer;
      vertical,bareSlider : boolean;
      Low,High,Name : string[20];
      putText : pointType;
      MainBox,SliderBox,TextBox,ThumbBox : rectType;
      PROCEDURE ReadNumber;
   END;

   TXSliders = object
      numUsed : ARRAY[1..20] OF boolean;
      PROCEDURE init;
      PROCEDURE Create(num : integer; ValMin, ValMax, VInitial, posX, posY,
                size : real; decimals: integer;
                Low,High,Name : string; vertical : boolean;
                ScaleType: Integer);
      PROCEDURE DrawAll;
      FUNCTION  Changed : boolean;
      FUNCTION  Value(num : integer) : real;
      PROCEDURE Delete(num : integer);
      PROCEDURE Reset(num : integer; VCurrent_ : real);
      PROCEDURE Erase(num,color: Integer);
      PROCEDURE Done;

   PRIVATE
      active : byte;
      list : ARRAY[1..20] OF ^TXSlider;
      current : ARRAY[1..20] OF real;
   END;

IMPLEMENTATION

USES Crt, Dos;

PROCEDURE Swap(VAR x,y : real);
VAR t : real;
BEGIN
   t:=x; x:=y; y:=t;
END;

PROCEDURE SetRect(VAR r : rectType; x1,y1,x2,y2 : integer);
BEGIN
   r.x1:=x1; r.y1:=y1; r.x2:=x2; r.y2:=y2;
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


CONSTRUCTOR TXSlider.create(ValMin, ValMax, VInitial, posX, posY, size_ : real;
         decimals_ : integer; Low_,High_,Name_ : string; vertical_ : boolean;
         SliderType_: Integer);
CONST w=15;
VAR
   sMin,sMax : string;
   d : integer;
BEGIN
   vMin:=ValMin; vMax:=ValMax; VCurrent:=VInitial;
   decimals:=decimals_; vertical:=vertical_;
   sMin := numStr(Vmax,0,decimals);
   sMax := numStr(Vmin,0,decimals);
   IF TextWidth(sMin)>TextWidth(sMax) THEN sMax:=sMin;
   NumWidth  := TextWidth(sMax) +4;
   NumLength := Length(sMax);
   bareSlider := decimals_<0;
   IF vertical THEN
    BEGIN
       IF size_<posY THEN swap( posY,size_);
       px   := round( posX*GetMaxX);
       py   := round( (1-size_)*GetMaxY);
       IF bareSlider THEN d:=0 ELSE d := rowHt +TextHeight(Name) +8;
       size := round( (1-posY)*GetMaxY) -d -py;
    END
   ELSE
    BEGIN
       IF size_<posX THEN swap( posX,size_);
       px   := round( posX*GetMaxX);
       py   := round( (1-posY)*GetMaxY)-w;
       size := round( size_*GetMaxX) - NumWidth - px;
    END;
   dVal   := 2*(vMax-Vmin)/size;
   chosen := true;
   Low:=low_; High:=High_; Name:=name_;
   SliderType := SliderType_;
END;

PROCEDURE TXSlider.Draw;
VAR
   num : string[20];
   n : integer;
   TextInfo : TextSettingsType;
   vp : ViewPortType;
   DownLeftBox,UpRightBox : rectType;
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
   HideCursor;
   IF vertical THEN
   BEGIN
      SetRect(SliderBox, px+1, py+w+1, px+w-1, py+size-w-1); {main}
      n := py+TextHeight(Name)+size+8;
      SetRect(TextBox, px+round(w-NumWidth) DIV 2, n,
                       px+round(w+NumWidth) DIV 2, n+RowHt);
      PutText.x := px+w DIV 2;
      PutText.y := py +TextHeight(Name) +size +RowHt DIV 2+8;
      SetRect  ( MainBox, px,py,px+w,py+size);
      Triangle ( px,py+size-w,px+w,py+size,2);   {Down box}
      Triangle ( px,py,px+w,py+w,0);             {Up box}
      rectangle( px,py+w,px+w,py+size-w);
      SetColor ( white);
      Print( CenterText, TopText,    px+w DIV 2, py+size+4, Name);
      Print( RightText,  TopText,    px-2,       py,        High);
      Print( RightText,  BottomText, px-2,       py+size,   Low);
   END
   ELSE
   BEGIN
      SetRect( SliderBox, px+w+1, py+1, px+size-w-1, py+w-1); {main}
      SetRect( TextBox, px+size+5,          py+round(w-RowHt) DIV 2,
                        px+size+5+NumWidth, py+round(w+RowHt) DIV 2);
      PutText.x := px +size +NumWidth DIV 2 +5;
      PutText.y := py +w DIV 2;
      SetRect  ( MainBox,px,py,px+size,py+w);
      SetRect  ( DownLeftBox, px,py,px+w,py+w);
      Triangle ( px,py,px+w,py+w,1);           {left  box}
      Triangle ( px+size-w,py,px+size,py+w,3); {rigth box}
      rectangle( px+w,py,px+size-w,py+w);
      SetColor ( white);
      Print( LeftText,   TopText, px,            py+w+2, Low);
      Print( CenterText, TopText, px+size DIV 2, py+w+2, Name);
      Print( rightText,  TopText, px+size,       py+w+2, High);
   END;
   WITH SliderBox DO rectangle( x1-1,y1-1,x2+1,y2+1);
   ShowCursor;
   ReDraw;
   envir.Reset;
END;

PROCEDURE TXSlider.ReDraw;
VAR
   n : integer;
   TextInfo : TextSettingsType;
CONST w=15;
BEGIN
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
   HideCursor;
   DrawBox( SliderBox,DarkGray);
   IF NOT BareSlider THEN DrawBox( TextBox,White);
   DrawBox( ThumbBox,LightGray);
   SetColor( black);
   IF NOT bareSlider THEN
      CASE SliderType OF
         1: OutTextXY( PutText.x, PutText.y, numStr(VCurrent,0,decimals));
         2: OutTextXY( PutText.x,PutText.y,numStr(Pwr(10,VCurrent),0,decimals));
      END;
   WITH ThumbBox DO
   BEGIN
      rectangle(x1,y1,x2,y2);
      IF chosen THEN OutTextXY( x1+w DIV 2, y1+w DIV 2, chr(177));
   END;
   ShowCursor;
   changed := true;  
   SetTextJustify( TextInfo.Horiz,TextInfo.vert);
END;

PROCEDURE TXSlider.ReadNumber;
VAR
   v : real;
   z : char;
   TextInfo : TextSettingsType;
   x,y,i,cpos,counter,blinkingRate : integer;
   oldText,line,blanks,underline,cursorLine : string;
   exitFlag,onOff,c : boolean;

   PROCEDURE print( text : string; color : integer);
   BEGIN
      SetColor(white);
      OutTextXY(PutText.x, PutText.y, oldtext);
      SetColor(color);
      OutTextXY(PutText.x, PutText.y, text);
      oldtext := text;
   END;

BEGIN
   GetTextSettings( TextInfo);
   SetTextJustify ( CenterText,CenterText);
   CASE SliderType OF
      1: line := numstr(VCurrent,0,decimals);
      2: line := numstr(Pwr(10,VCurrent),0,decimals);
   END;
   cpos      := length(line)+1;
   oldtext   := '';
   blanks    := '';
   underline := '';
   onOff     := false;
   exitFlag  := false;
   counter   := 0;
   c := (decimals<>0);
   blinkingRate := 40;
   FOR i := 1 TO NumLength-1 DO blanks    := concat(blanks,' ');
   FOR i := 1 TO NumLength   DO underline := concat(underline,'_');
   REPEAT
      print(line,red);
      cursorLine := blanks;
      IF (length(line) MOD 2 <> NumLength MOD 2) THEN delete(cursorLine,1,1);
      insert('_',cursorLine,cPos+(NumLength-length(line)) DIV 2);
      WHILE NOT keyPressed DO   {cursor blinking}
      BEGIN
         IF counter>BlinkingRate THEN
         BEGIN
            IF OnOff THEN
            BEGIN
               SetColor(white);
               OutTextXY(PutText.x, PutText.y+1, underline);
            END
            ELSE
            BEGIN
               SetColor(black);
               OutTextXY(PutText.x, PutText.y+1, cursorLine);
            END;
            counter := 0;
            OnOff := NOT OnOff;
         END
         ELSE
         BEGIN
            delay(5);
            inc(counter);
         END;
      END;  {blinking}
      z := readKey;
      CASE z OF
       '0'..'9','-'   : IF length(line)<NumLength THEN
                        BEGIN
                           inc(cPos);
                           insert(z,line,cPos-1);
                        END;
       '.'            : IF (length(line)<NumLength) AND (NOT c) THEN
                        BEGIN
                           inc(cPos);
                           insert(z,line,cPos-1);
                           c := true;
                        END;
      chr(13),chr(27) : exitFlag:=true;                   {return,esc}
      chr(8)          : IF cPos>0 THEN                    {backSpace}
                        BEGIN
                           dec(cpos);
                           c := c AND (copy(line,cpos,1)<>'.');
                           delete(line,cpos,1);
                        END;
      chr(0) : CASE readKey OF
                 'M': IF cPos<=NumLength THEN inc(cPos);  {right arrow}
                 'K': IF cPos>0 THEN dec(cPos);           {Left  arrow}
                 'S': IF length(line)>=cPos THEN          {Delete}
                      BEGIN
                         c := c AND (copy(line,cpos,1)<>'.');
                         delete(line,cPos,1); 
                      END;
                END; {case}
      END; {case}
   UNTIL exitFlag;
   Val(line,v,i);
   CASE SliderType OF
      1: BEGIN
            IF (i=0) AND (z<>chr(27)) AND (v>=vMin) AND (v<=vMax) THEN
                                                    vCurrent:= v;
            print(numstr(VCurrent,0,decimals),black);
         END;
      2: BEGIN
            IF (i=0) AND (z<>chr(27)) AND (log10(v)>=vMin) AND (log10(v)<=vMax) THEN
                                                    vCurrent:= log10(v);
            print(numstr(Pwr(10,VCurrent),0,decimals),black);
         END;
   END;
   SetColor(white);
   OutTextXY(PutText.x, PutText.y+1, underline);
   SetTextJustify( TextInfo.Horiz,TextInfo.vert);
END;


FUNCTION TXSlider.value : real;
CONST w=15;
VAR
   e            : pointType;
   button,ex,ey : integer;
   num          : String[20];
   h,v,vOld     : real;
   envir        : TEnvironment;

   FUNCTION PtAbove( p:pointType; r1,r2:rectType; vertical : boolean) : boolean;
   BEGIN
     IF vertical THEN
        PtAbove :=(p.x>r1.x1) AND(p.x<r2.x2) AND(p.y>r1.y1) AND(p.y<r2.y1)
     ELSE
        PtAbove :=(p.x>r2.x2) AND(p.x<r1.x2) AND(p.y>r2.y1) AND(p.y<r1.y2);
   END;


BEGIN
   MouseGlobalPosn(e.x,e.y,button);
   IF event.MouseClicked OR event.KeyPressed OR (button<>0) THEN
   BEGIN
      envir.Save;
      envir.Standardize;
      IF button=0 THEN BEGIN e.x:=event.x; e.y:=event.y END;
      vOld := vCurrent;
      IF chosen AND event.extendedKey THEN
         CASE ord(event.readkey) OF
          75,80 : IF VCurrent>vMin+dVal THEN
                    VCurrent:=VCurrent-dVal
                  ELSE
                    VCurrent:=vMin;
          77,72 : IF VCurrent<vMax-dVal THEN
                    VCurrent:=VCurrent+dVal
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
              IF (VCurrent<vMax-dVal) THEN VCurrent:=VCurrent+dVal
              ELSE VCurrent:=vMax
           ELSE                                          {left or down arrow}
              IF (VCurrent>vMin+dVal) THEN VCurrent:=VCurrent-dVal
              ELSE VCurrent:=vMin;
           IF event.MouseClicked THEN Delay(500);
        END
        ELSE
        IF ptInside(e,TextBox) THEN ReadNumber;
      IF vCurrent<>vOld THEN ReDraw ELSE changed:=false;
      envir.Reset;
   END
   ELSE changed := false;
   CASE SliderType OF
      1: value := VCurrent;
      2: value := Pwr(10,VCurrent);
   END;
END;

PROCEDURE TXSlider.Reset(VCurrent_ : real);
VAR
   envir : TEnvironment;
BEGIN
   envir.Save;
   envir.Standardize;
   VCurrent := VCurrent_;
   ReDraw;
   envir.Reset;
END;

PROCEDURE TXSlider.Erase(color : integer);
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



DESTRUCTOR TXSlider.Done;
BEGIN
END;



PROCEDURE TXSliders.init;
VAR n : byte;
BEGIN
   FOR n := 1 TO 20 DO numUsed[n]:= false;
END;

PROCEDURE TXSliders.create(num : integer; ValMin, ValMax, VInitial, posX,
           posY, size : real; decimals: integer;
           Low,High,Name : string; vertical : boolean; ScaleType: Integer);
VAR n : byte;
BEGIN
   IF (num>=0) AND (num<=20) AND NOT numUsed[num] THEN
   BEGIN
      FOR n := 1 TO 20 DO
       IF numUsed[n] THEN list[n]^.chosen := false;
      new(list[num]);
      list[num]^.create(ValMin, ValMax, VInitial, posX, posY, size,decimals,
                         Low,High,Name, vertical, ScaleType);
      numUsed[num] := true;
      active:=num;
   END;
END;

PROCEDURE TXSliders.DrawAll;
VAR n : byte;
BEGIN
   FOR n := 1 TO 20 DO IF numUsed[n] THEN list[n]^.Draw;
END;

FUNCTION TXSliders.Changed : boolean;
VAR n    : byte;
    flag : boolean;
   vp : ViewPortType;
BEGIN
   IF event.keyPressed AND (event.readKey=#9) THEN
   BEGIN
      GetViewSettings(vp);
      SetViewPort(0,0,GetMaxX,GetMaxY,true);
      REPEAT
         IF active=20 THEN active:=1 ELSE inc(active)
      UNTIL numUsed[active];
      FOR n := 1 TO 20 DO IF numUsed[n] THEN list[n]^.chosen:=false;
      list[active]^.chosen:=true;
      FOR n := 1 TO 20 DO IF numUsed[n] THEN list[n]^.ReDraw;
      WITH vp DO SetViewPort(x1,y1,x2,y2,clip);
   END;
   FOR n := 1 TO 20 DO IF numUsed[n] THEN current[n]:=list[n]^.value;
   flag := false;
   FOR n := 1 TO 20 DO IF numUsed[n] THEN flag:=flag OR list[n]^.changed;
   changed := flag;
END;

FUNCTION TXSliders.Value(num : integer) : real;
BEGIN
   value := current[num];
END;

PROCEDURE TXSliders.Delete(num : integer);
BEGIN
   IF numUsed[num] THEN dispose(list[num],done);
   numUsed[num] := false;
END;

PROCEDURE TXSliders.Reset(num : integer; VCurrent_ : real);
BEGIN
   IF numUsed[num] THEN list[num]^.Reset(vCurrent_);
END;



PROCEDURE TXSliders.Erase(num, color : integer);
BEGIN
   IF numUsed[num] THEN list[num]^.Erase(color);
END;



PROCEDURE TXSliders.Done;
VAR n : byte;
BEGIN
   FOR n := 1 TO 20 DO IF numUsed[n] THEN dispose(list[n],done);
   FOR n := 1 TO 20 DO numUsed[n] := false;
END;

END.