(***********************************************************************)
(***********************************************************************)
(**                                                                   **)
(**     Unit CUPSmupp - 2D Plotting Procedures Originally Written for **)
(**     M.U.P.P.E.T. project, rewritten as part of CUPS utilities.    **)
(**     Authored by Jack M. Wilson (UMd)           89/05/15           **)
(**     Changes  by Ian Johnston                   91/06/03           **)
(**     Revised and expanded by Jarek Tuszynski    1991 - 1994        **}
(**     Department of Physics and Astronomy                           **)
(**     George Mason University                                       **)
(**     Fairfax, Virginia 22030                                       **)
(**     (c) 1994 by Jarek Tuszynski                                   **)
(**     Version: 1.3 (94/11/14)                                       **)
(**                                                                   **)
(***********************************************************************)
(***********************************************************************)

UNIT CUPSmupp;
INTERFACE

uses cups, CUPSproc;

{------------------------------ Sound Procedures --------------------------}

  PROCEDURE Beep;
  PROCEDURE Note( frequency: REAL; amplitude, duration: integer);

{--------------------------- GENERAL PRINTING -----------------------------}

  PROCEDURE PrintXY( x,y : real; text: string);
  PROCEDURE Print( x,y: integer; text: string);
  PROCEDURE RubOutXY( x,y : real; num,BkColor: word);
  PROCEDURE RubOut( x,y: integer; num,BkColor: word);
  PROCEDURE Message( text: string);
  PROCEDURE Comment( x,y: real; text: string);
  PROCEDURE PressKeyMessage;

{------------------------- CONCERNING VIEWPORTS ---------------------------}

  PROCEDURE DefineViewPort( vNum: integer; vpx1, vpx2, vpy1, vpy2: REAL);
  PROCEDURE OpenViewPort  ( vNum: integer);
  PROCEDURE SelectViewPort( vNum: integer);
  PROCEDURE CloseViewPort ( vNum: integer);
  PROCEDURE SelectMuppetPort;
  PROCEDURE ClearMuppetPort;

{----------------------- CONCERNING GRAPH PLOTTING ------------------------}

  CONST
     MaxNumData = 401;
  TYPE
     DataVector = ARRAY[1..MaxNumData] OF REAL;
     Place = (Left, Right, Top, Bottom, Inside);

  PROCEDURE PlotData( x,y: DataVector; numdata: integer);
  PROCEDURE PlotDVectors( x,y : DVector; ibeg,iend: Integer);
  PROCEDURE PlotDataPoints( x,y: DataVector; numdata: integer; symbol: char);
  PROCEDURE PlotPoints(x,y : DVector; symbol : char);
  PROCEDURE PlotLine( x1, y1, x2, y2: REAL);
  PROCEDURE PlotSymbol( x,y: REAL; symbol: char);
  PROCEDURE Axis( xInt, yInt, tickX, tickY: REAL);
  PROCEDURE DrawAxis( xInt, yInt, tickX, tickY: REAL; color: integer);
  PROCEDURE PutLabel( p: place; title: string);
  PROCEDURE Vector (x1,y1,x2,y2 : integer);

{-------------------------- CONCERNING SCALING ----------------------------}

  PROCEDURE DefineScale( sNum: integer; sXmin, sXmax, sYmin, sYmax: REAL);
  PROCEDURE SelectScale( sNum: integer);
  FUNCTION  MapX( Xdata: REAL) : integer;
  FUNCTION  MapY( Ydata: REAL) : integer;
  PROCEDURE Map( Xdata, Ydata: REAL; VAR xScreen, yScreen: integer);
  FUNCTION  TickSpace( dis : REAL) : REAL;
  PROCEDURE AutoScale(  sNum: integer; VAR x,y: DataVector; numData: integer);
  PROCEDURE AutoScaling(sNum: integer; VAR x,y: DVector);

{----------------------------- TFrames -----------------------------------}
type
   TFrames = object
      FirstPage : boolean;
      PROCEDURE Init;
      PROCEDURE Select( fNum : byte);
      PROCEDURE Open( fNum : byte);
      PROCEDURE Switch;
      PROCEDURE SwitchAndClean;
      PROCEDURE Pause;
      PROCEDURE Continue;
      PROCEDURE Close;
   END;


{****************************************************************************}

IMPLEMENTATION
USES crt, dos, graph;

{------------------------- local Procedures --------------------------------}


   PROCEDURE Error(str : string);
   BEGIN
      announce(str);
      HALT;
   END;

   PROCEDURE Swap(var x,y : real);
   var d : real;
   BEGIN
      d := x;
      x := y;
      y := d;
   END;



{----------------- GENERAL -----------------------------}

PROCEDURE Beep;
BEGIN
   Sound(220);
   Delay(200);
   NoSound;
END;

PROCEDURE Note( frequency: REAL; amplitude, duration: integer);
CONST beat = 100;
BEGIN
   sound(round(frequency));
   delay(duration * beat);
   nosound;
END;

{----------------- GENERAL PRINTING -------------------------}

PROCEDURE Print( X, Y: integer; text: string);
{ Color,font,direction and size can be set before this call.}
   VAR
      Row, Col: integer;
BEGIN
   Col := Round((x - 0.6) * ColWidth);
   Row := Round((y - 0.6) * RowHt);
   HideMouse;
   OutTextXY(Col, Row, text);
   ShowMouse;
END;

PROCEDURE PrintXY( X, Y: real; text: string);
{ Color,font,direction and size can be set before this call.}
VAR vp : integer;
BEGIN
   vp := ViewPortNumber;
   SelectMuppetPort;
   HideMouse;
   OutTextXY( Round(x*GetMaxX), Round((1-y)*GetMaxY), text);
   ShowMouse;
   SelectViewPort(vp);
END;


PROCEDURE RubOut(x,y:integer; num,BkColor:word);
{ It erase num characters ,starting from location x,y.}
VAR
   col,row,color,h,w : integer;
   fill : FillSettingsType;
   text : TextSettingsType;
BEGIN
   color := GetColor;
   SetColor(BkColor);
   GetFillSettings( fill);
   GetTextSettings( text);
   SetFillStyle(SolidFill,BkColor);
   if text.Direction=HorizDir then
   begin
      h := TextHeight('W');
      w := num*TextWidth ('W');
   end
   else
   begin
      w := TextHeight('W');
      h := num*TextWidth ('W');
   end;
   Col := Round((x - 0.6) * ColWidth - w*text.horiz/2);
   Row := Round((y - 0.6) * RowHt - h*(2 - text.vert)/2 );
   if (text.Direction=VertDir) and (text.horiz=LeftText) then Col := Col-w;
   HideMouse;
   Bar(Col, Row, Col+w, Row+h);
   ShowMouse;
   SetColor(Color);
   WITH fill DO SetFillStyle( Pattern, Color);
END;

PROCEDURE RubOutXY(x,y:real; num,BkColor:word);
{ It erase num characters ,starting from location x,y.}
VAR
   col,row,color,h,w,vp : integer;
   fill : FillSettingsType;
   text : TextSettingsType;
BEGIN
   vp := viewPortNumber;
   SelectMuppetPort;
   color := GetColor;
   SetColor(BkColor);
   GetFillSettings( fill);
   GetTextSettings( text);
   SetFillStyle(SolidFill,BkColor);
   if text.Direction=HorizDir then
   begin
      h := TextHeight('W');
      w := num*TextWidth ('W');
   end
   else
   begin
      w := TextHeight('W');
      h := num*TextWidth ('W');
   end;
   Col := Round(x*GetMaxX - w*text.horiz/2);
   Row := Round((1-y)*GetMaxY - h*(2 - text.vert)/2 );
   if (text.Direction=VertDir) and (text.horiz=LeftText) then Col := Col-w;
   HideMouse;
   Bar(Col, Row, Col+w, Row+h);
   ShowMouse;
   SetColor(Color);
   WITH fill DO SetFillStyle( Pattern, Color);
   SelectViewPort(vp);
END;

PROCEDURE Comment(x,y : real; text : string);
VAR
   i,dx,dy,cx,cy,n : integer;
   spacesOnly      : boolean;
   Environment     : TEnvironment;
BEGIN
   Environment.Save;
   Environment.Standardize;
   spacesOnly := true;
   FOR i := 1 TO length(text) DO spacesOnly := spacesOnly AND (text[i]=' ');
   dx := TextWidth(text)+13;
   dy := RowHt + 3;
   cx := round(GetMaxX*x);
   cy := round(GetMaxY*(1-y));
   HideMouse;
   IF spacesOnly THEN
    BEGIN
       SetFillStyle(SolidFill,Black);
       bar(cx,cy,cx+dx+1,cy+dy+1);
    END
   ELSE
    BEGIN
       SetFillStyle(SolidFill,blue);
       Bar3D(cx,cy+3,cx+dx-3,cy+dy,3,true);
       OutTextXY(cx+6,cy+8,Text);
    END;
   ShowMouse;
   Environment.Reset;
END;


PROCEDURE Message( text: string);

   PROCEDURE MakeBox( x1, y1, x2, y2, EdgeColor, FillColor: integer);
   BEGIN
      SetFillStyle(1, FillColor);
      SetColor(EdgeColor);
      HideMouse;
      Bar3D(x1, y1, x2, y2, 3, true);
      ShowMouse;
   END;

VAR
   n,color : integer;
   Environment : TEnvironment;
BEGIN
   Environment.Save;
   Environment.Standardize;
   IF (text<>'') THEN
      MakeBox(0, 0, GetMaxX, RowHt DIV 2 + 10, White, Blue)
   ELSE
      MakeBox(0, 0, GetMaxX, RowHt DIV 2 + 10, black, black);
   SetColor(white);
   Print(2, 1, text);
   Environment.Reset;
END;

PROCEDURE PressKeyMessage;
BEGIN
   IF MousePresent THEN
   Message('                Press any key or click the mouse when ready ...')
   ELSE
   Message('                        Press any key when ready ...');
   StaticPause;
   Message('');
END;


{----------------- CONCERNING VIEWPORTS -------------------------}

PROCEDURE DefineViewPort( vNum: integer; VPX1, VPX2, VPY1, VPY2: REAL);
BEGIN
   IF (vpx1 = vpx2) OR (vpy1 = vpy2) THEN
    Error('Error: view port #'+NumStr(vNum,2,0)+' has the same min and max values');
   IF vNum>20 THEN
    Error('Error: Your are trying to define view port with number biger then 20.');
   IF vNum<0 THEN
    Error('Error: Your are trying to define view port with number smaller then 0.');
   WITH Views[vNum] DO
   BEGIN
      if (vpx1>vpx2) then swap(vpx1,vpx2);
      if (vpy1>vpy2) then swap(vpy1,vpy2);
      Vx1 := Round(VPX1 * GetMaxX);
      Vx2 := Round(VPX2 * GetMaxX);
      Vy1 := Round((1.0 - VPY2) * GetMaxY);
      Vy2 := Round((1.0 - VPY1) * GetMaxY);
   END;
END;

PROCEDURE OpenViewport( vNum: integer);
VAR
   color : integer;
   line  : LineSettingsType;
   fill  : FillSettingsType;
BEGIN
   color := GetColor;
   GetLineSettings( line);
   GetFillSettings( fill);
   SetColor( white);
   SetLineStyle( SolidLn, 0, NormWidth);
   SetFillStyle(SolidFill, GraphBackColor);
   SelectViewPort(vNum);
   HideCursor;
   WITH Views[vNum] DO bar( 0,0,Vx2-Vx1,Vy2-Vy1);
   WITH Views[vNum] DO rectangle( 0,0,Vx2-Vx1,Vy2-Vy1);
   ShowCursor;
   SetColor( color);
   WITH line DO SetLineStyle( LineStyle, Pattern, Thickness);
   WITH fill DO SetFillStyle( Pattern, Color);
END;

PROCEDURE SelectViewport( vNum: integer);
BEGIN
   if (vNum<0) or (vNum>20) then
    Error('SelectViewPort: view port number out of range');
   ViewPortNumber := vNum;
   WITH Views[vNum] DO SetViewPort(Vx1, Vy1, Vx2, Vy2, ClipOn);
END;


PROCEDURE CloseViewport( vNum: integer);
BEGIN
   SelectViewport(vNum);
   HideMouse;
   ClearViewport;
   ShowMouse;
   SelectViewport(0);
END;

PROCEDURE SelectMuppetPort;
BEGIN
   ViewPortNumber := 0;
   SetViewport(0, 0, GetMaxX, GetMaxY, ClipOn);
END;

PROCEDURE ClearMuppetPort;
BEGIN
   SelectMuppetPort;
   HideMouse;
   ClearDevice;
   ShowMouse;
END;

{----------------- CONCERNING GRAPH PLOTTING -------------------------}

PROCEDURE PlotData( x, y: DataVector; numdata: integer);
{ Color, and line style can be set before this call.}
   VAR
      I, oldX,oldY, newX,newY, y1: Integer;
BEGIN
   If (NumData<1) or (NumData>MaxNumData) then
    Error('PlotData: Number of elements exceeds length of DataVector');
   IF ScaleNumber = 0 THEN AutoScale(0, x, y, numdata);
   map( x[1], y[1], oldX, oldY);
   HideMouse;
   FOR i := 2 TO NumData DO
   BEGIN
      map( x[i], y[i], newX, newY);
      Line (newX,newY,newX,newY);
      Line (oldX,oldY,newX,newY);
      oldX:=newX; oldY:=newY;
   END;
   ShowMouse;
END;


PROCEDURE PlotDVectors(x, y : DVector; ibeg, iend: Integer);
{ You can control line color and setting.}
VAR
   ib, ie, di, i, n : Integer;
   oldX,oldY,newX,newY : integer;
BEGIN
{$IFOPT D+}
   IF (ibeg>x.Size) or (ibeg>y.Size) or (ibeg<0) or
      (iend>x.Size) or (iend>y.Size) or (iend<0) THEN
     Error('PlotDVectors: Index exceeds length of the vectors');
{$ENDIF}
{$R-}
   IF ibeg <> iend THEN
   BEGIN
      IF scaleNumber=0 THEN AutoScaling(0,x,y);
      IF ibeg < iend THEN BEGIN ib:=ibeg; ie:=iend; di:= 1 END
      ELSE                BEGIN ib:=iend; ie:=ibeg; di:=-1 END;
      n := ibeg;
      map( x.vecPtr^[n], y.vecPtr^[n], oldX, oldY);
      HideMouse;
      FOR i := ib+1 TO ie DO
      BEGIN
         n:=n+di;
         map( x.vecPtr^[n], y.vecPtr^[n], newX, newY);
         Line (newX,newY,newX,newY);
         Line (oldX,oldY,newX,newY);
         oldX:=newX; oldY:=newY;
      END;
      ShowMouse;
   END;
{$R+}
END;


PROCEDURE PlotSymbol( x,y : REAL; symbol : char);
{ Symbol color can be changed before this call.}
   VAR
      Gx, Gy :integer;
      text : TextSettingsType;
BEGIN
   Map(x,y,Gx,Gy);
   GetTextSettings( text);
   SetTextStyle( DefaultFont, HorizDir, 1);
   SetTextJustify(CenterText, CenterText);
   HideMouse;
   OutTextXY(Gx,Gy,symbol);
   ShowMouse;
   WITH text DO SetTextStyle( Font, Direction, CharSize);
   WITH text DO SetTextJustify( Horiz, Vert);
END;


PROCEDURE PlotPoints(x,y : DVector; symbol : char);
{ You can control color of the symbols. }
VAR
   n,num : integer;
   text : TextSettingsType;
BEGIN
   GetTextSettings (text);
   SetTextStyle (DefaultFont, HorizDir, 1);
   SetTextJustify(CenterText, CenterText);
   IF scaleNumber=0 THEN AutoScaling(0,x,y);
   IF x.length<>y.length THEN exit;
   num := x.length;
   HideMouse;
{$R-}
   FOR n := 1 TO num DO
     OutTextXY(mapX(x.vecPtr^[n]),mapY(y.vecPtr^[n]),symbol);
{$R+}
   ShowMouse;
   WITH text DO SetTextStyle (Font, Direction, CharSize);
   WITH text DO SetTextJustify (Horiz, Vert);
END;


PROCEDURE PlotDataPoints( x, y: DataVector; numdata: integer; symbol: char);
{ Symbol color can be changed before this call.}
   VAR
      i,gx,gy: Integer;
      text : TextSettingsType;
BEGIN
   If (NumData<1) or (NumData>MaxNumData) then
    Error('PlotDataPoints: Number of elements exceeds length of DataVector');
   IF ScaleNumber = 0 THEN
      AutoScale(0, x, y, numdata);
   GetTextSettings( text);
   SetTextStyle( DefaultFont, HorizDir, 1);
   SetTextJustify(CenterText, CenterText);
   HideMouse;
   FOR i := 1 TO NumData DO
   BEGIN
      Map(x[i], y[i], Gx, Gy);
      OutTextXY(Gx, Gy, symbol);
   END; {for}
   ShowMouse;
   WITH text DO SetTextStyle( Font, Direction, CharSize);
   WITH text DO SetTextJustify( Horiz, Vert);
END;


PROCEDURE PlotLine( x1, y1, x2, y2: REAL);
{ line color, and style can be changed.}
   VAR
      Gx1, Gy1, Gx2, Gy2: integer;
BEGIN
   Map(x1, y1, Gx1, Gy1);
   Map(x2, y2, Gx2, Gy2);
   HideMouse;
   Line(Gx1, Gy1, Gx2, Gy2);
   ShowMouse;
END;


PROCEDURE DrawAxis( Xint, Yint, TickX, TickY: REAL; color: integer);
   VAR
      x, y, TickLength,min,max : REAL;
      Xs, Ys, n, m : integer;
      Environment : TEnvironment;

   PROCEDURE LabelTickX( VAR x : real);
   BEGIN
      WITH GraphScales[ScaleNumber] DO
      IF (x>=Min) AND (x<=Max) THEN      {if inside the plot then...}
      BEGIN
         PlotLine(x, ymin, x, ymin + TickLength);
         PlotLine(x, ymax, x, ymax - TickLength);
         Map(x, ymin, Xs, Ys);
         IF (TickX>1E-5) AND (abs(x)<1E-6) THEN x := 0;
         OutTextXY(Xs, Ys + 6, Num2Str(x,7));
      END;
      x := x + TickX;
   END;

   PROCEDURE LabelTickY( VAR y : real);
   BEGIN
      WITH GraphScales[ScaleNumber] DO
      IF (y>=Min) AND (y<=Max) THEN      {if inside the plot then...}
      BEGIN
         PlotLine(xmin, y, xmin + TickLength, y);
         PlotLine(xmax, y, xmax - TickLength, y);
         Map(xmin, y, Xs, Ys);
         IF (TickY>1E-5) AND (abs(y)<1E-6) THEN y := 0;
         OutTextXY(Xs - 5, Ys, Num2Str(y,7));
      END;
      y := y + TickY;
   END;

BEGIN
   Environment.Save;
   Environment.Standardize;
   SetColor(color);
   WITH Views[ViewPortNumber] DO SetViewPort(Vx1,Vy1,Vx2,Vy2,ClipOff);
   WITH GraphScales[ScaleNumber] DO
   BEGIN
      IF (TickX=0) or (TickY=0) or
         (((xmax-xmin)/Tickx) > 20) OR (((ymax-ymin)/Ticky) > 20) THEN
      BEGIN
         TickX := TickSpace(xmax-xmin);
         TickY := TickSpace(ymax-ymin);
      END;
      HideMouse;
      SetTextJustify(CenterText, TopText);
      IF TickX > 0 THEN
      BEGIN
         TickLength := 0.02 * (ymax - ymin);      {Tick hight}
         x := Xint;
         IF xMin<xMax THEN
          BEGIN
             max := xMax;
             min := xMin;
          END
         ELSE
          BEGIN
             max := xMin;
             min := xMax;
          END;
         IF (xInt>Min) AND (xInt<Max) THEN      {plot y axis}
           PlotLine(Xint, ymin, Xint, ymax);
         WHILE x <= max DO LabelTickX(x);        {plot along  x axis}
         x := Xint;
         TickX := -TickX;
         WHILE x >= min DO LabelTickX(x);        {plot along -x axis}
      END;  {if}
      SetTextJustify(RightText, CenterText);
      IF TickY > 0 THEN
      BEGIN
         TickLength := 0.02 * (xmax - xmin);
         y := Yint;
         IF yMin<yMax THEN
          BEGIN
             max := yMax;
             min := yMin;
          END
         ELSE
          BEGIN
             max := yMin;
             min := yMax;
          END;
         IF (yInt>Min) AND (yInt<Max) THEN     {plot x axis}
           PlotLine(xmin, Yint, xmax, Yint);
         WHILE y <= max DO LabelTickY(y);       {plot along y axis}
         y := Yint;
         TickY := -TickY;
         WHILE y >= min DO LabelTickY(y);       {plot along -y axis}
      END;  {if}
      ShowMouse;
      Environment.Reset;
   END; {with GraphScales}
END;


PROCEDURE Axis (Xint, yint, TickX, TickY: REAL);
BEGIN
   DrawAxis( Xint, Yint, TickX, TickY, white);
END;


PROCEDURE PutLabel( p: place; title: string);
{ You can control color of the labels.}
   VAR
      angle, x, y, Q: integer;
      text : TextSettingsType;
BEGIN
   GetTextSettings( text);
   WITH Views[ViewPortNumber] DO
   BEGIN
      Q := GetMaxX DIV 30;
      SetViewPort(Vx1, Vy1, Vx2, Vy2, ClipOff);
      SetTextJustify(CenterText, CenterText);
      CASE P OF
         Top:
         BEGIN
            angle := HorizDir;
            x := (Vx2 - Vx1) DIV 2;
            y := -Q;
         END;
         Bottom:
         BEGIN
            angle := HorizDir;
            x := (Vx2 - Vx1) DIV 2;
            y := Vy2 - Vy1 + Q;
         END;
         Inside:
         BEGIN
            angle := HorizDir;
            x := (Vx2 - Vx1) DIV 2;
            y := Q;
         END;
         Left:
         BEGIN
            angle := VertDir;
            x := -2 * Q;
            y := (Vy2 - Vy1) DIV 2;
         END;
         Right:
         BEGIN
            angle := VertDir;
            x := Vx2 - Vx1 + Q;
            y := (Vy2 - Vy1) DIV 2;
         END;
      END;                          {of Case}
      SetTextStyle(DefaultFont, angle, 1);
      HideMouse;
      OutTextXY(x, y, title);
      ShowMouse;
   END;                              {of with}
   WITH text DO SetTextStyle( Font, Direction, CharSize);
   WITH text DO SetTextJustify( Horiz,Vert);
   SelectViewPort(ViewPortNumber);
END;                                 {of PROCEDURE}

PROCEDURE vector (x1,y1,x2,y2 : integer);
{ You can control line color and settings. } 
VAR
   a,s : REAL;
   l : integer;
BEGIN
   line (x1,y1,x2,y2);
   IF (x2<>x1) THEN a:=arcTan((y2-y1)/(x2-x1)) ELSE a:=pi/2;
   IF (x2<x1) OR ((x1=x2) AND (y2<y1)) THEN a:=a+pi;
   s := sqr(1.0*x2-x1)+sqr(1.0*y2-y1);
   IF s>70 THEN l:=6 ELSE l:=round(s+5) DIV 15;
   moveTo (x2,y2);
   lineRel (round(l*cos(5*pi/6-a)),-round(l*sin(5*pi/6-a)));
   moveTo (x2,y2);
   lineRel (round(l*cos(7*pi/6-a)),-round(l*sin(7*pi/6-a)));
END;



{------------------ SCALING PROCEDURES -----------------------}


PROCEDURE DefineScale( sNum: integer; sXmin, sXmax, sYmin, sYmax: REAL);
BEGIN
   IF (sXmin=sXmax) OR (sYmin=sYmax) THEN
    Error(concat('Error: Scale #',NumStr(sNum,2,0),' has the same min and max values'));
   IF sNum>20 THEN
    Error('Error: Your are trying to define Graph Scale with number biger then 20.');
   IF sNum<0 THEN
    Error('Error: Your are trying to define Graph Scale with number smaller then 0.');
   WITH GraphScales[sNum] DO
   BEGIN
      Xmin := sXmin;
      Xmax := sXmax;
      Ymin := sYmin;
      Ymax := sYmax;
      ScaleNumber := sNum;
   END;
END;


PROCEDURE SelectScale( sNum: integer);
BEGIN
   if (sNum<0) or (sNum>20) then
    Error('SelectScale: scale number out of range');
   ScaleNumber := sNum;
END;


PROCEDURE Map (Xdata, Ydata: REAL; VAR Xscreen, Yscreen: integer);
{maps data coordinants to screen}
VAR
   x,y : real;
BEGIN
   WITH Views[ViewPortNumber] DO
    WITH GraphScales[ScaleNumber] DO
    BEGIN
       x := ((Vx2-Vx1) / (xmax-xmin)) * (Xdata-xmin);
       y := ((Vy2-Vy1) / (ymax-ymin)) * (ymax-Ydata);
    END;
   IF abs(x)<MaxInt THEN XScreen := round(x) ELSE
   IF x>0 THEN XScreen:=MaxInt ELSE XScreen:=-MaxInt;
   IF abs(y)<MaxInt THEN YScreen := round(y) ELSE
   IF y>0 THEN YScreen:=MaxInt ELSE YScreen:=-MaxInt;
END;

FUNCTION MapX( Xdata :REAL) : integer;
VAR
   x : real;
BEGIN
   WITH GraphScales [ScaleNumber] DO
   WITH Views[ViewPortNumber] DO
   x := (Vx2-Vx1) / (xmax-xmin) * (Xdata-xmin);
   IF abs(x)<MaxInt THEN MapX := round(x) ELSE
   IF x>0 THEN MapX:=MaxInt ELSE MapX:=-MaxInt;
END;


FUNCTION MapY( Ydata :REAL) : integer;
VAR
   y : real;
BEGIN
   WITH GraphScales [ScaleNumber] DO
   WITH Views[ViewPortNumber] DO
   y := (Vy2-Vy1) / (ymax-ymin) * (ymax-Ydata);
   IF abs(y)<MaxInt THEN MapY := round(y) ELSE
   IF y>0 THEN MapY:=MaxInt ELSE MapY:=-MaxInt;
END;


FUNCTION TickSpace( dis : REAL) : REAL;
CONST nTick = 5;
VAR l,e,f,nf,log : REAL;
BEGIN
   dis := abs(dis);
   IF dis<>0 THEN dis := dis/(nTick-1) ELSE dis:=1;
   log := ln(10);
   l := ln(dis)/log;
   IF l>=0 THEN e:=trunc(l) ELSE e:=trunc(l-1);
   f := dis/exp(e*log);
   IF f<1.5 THEN nf:=1 ELSE
   IF f<3.5 THEN nf:=2 ELSE
   IF f<7.5 THEN nf:=5 ELSE
   nf:=10;
   TickSpace := nf*exp(e*log);
END;

PROCEDURE AutoScale( sNum : integer; VAR x, y: DataVector; numdata: integer);
VAR
   min,max,Tick,CInt : ARRAY[1..2] OF real;
   n : integer;
BEGIN
   If (NumData<1) or (NumData>MaxNumData) then
    Error('AutoScale: Number of elements exceeds length of DataVector');
   if (sNum<0) or (sNum>20) then
    Error('AutoScale: scale number out of range');
   min[1] := x[1];  max[1] := x[1];           {find max and min values of...}
   min[2] := y[1];  max[2] := y[1];           {... the data vectors x and y.}
   FOR n := 2 TO NumData DO
   BEGIN
      IF x[n] < min[1] THEN min[1] := x[n];
      IF x[n] > max[1] THEN max[1] := x[n];
      IF y[n] < min[2] THEN min[2] := y[n];
      IF y[n] > max[2] THEN max[2] := y[n];
   END;
   max[2] := max[2] + (max[2] - min[2])/10.0;
   min[2] := min[2] - (max[2] - min[2])/10.0;
   FOR n := 1 TO 2 DO
   begin
      IF min[n]=max[n] THEN
      BEGIN
         max[n] := max[n] + 1;
         min[n] := min[n] - 1;
      END;
      Tick[n] := TickSpace ((max[n]-min[n]));
   end;
   DefineScale(sNum,min[1],max[1],min[2],max[2]);
   Axis(0,0,Tick[1],Tick[2]);
END;

PROCEDURE AutoScaling (sNum : integer; VAR x,y : DVector);
VAR
   min,max,Tick,CInt : array[1..2] of real;
   n : integer;
BEGIN
   if (sNum<0) or (sNum>20) then
    Error('AutoScaling: scale number out of range');
   x.MinMax(n,min[1],n,max[1]);
   y.MinMax(n,min[2],n,max[2]);
   max[2] := max[2] + (max[2] - min[2])/10.0;
   min[2] := min[2] - (max[2] - min[2])/10.0;
   FOR n := 1 TO 2 DO
   BEGIN
      IF min[n]=max[n] THEN
      BEGIN
         max[n] := max[n] + 1;
         min[n] := min[n] - 1;
      END;
      Tick[n] := TickSpace ((max[n]-min[n]));
   END;
   DefineScale(sNum,min[1],max[1],min[2],max[2]);
   Axis(0,0,Tick[1],Tick[2]);
END;

{----------------------------- TFrames -----------------------------------}
{ based on code written for M.U.P.P.E.T. project}

PROCEDURE TFrames.Init;
BEGIN
   FirstPage   := true;
   GraphDriver := VGA;
   GraphMode   := VGAMed;
END;

PROCEDURE TFrames.Switch;
BEGIN
   FirstPage := NOT FirstPage;
   Continue;
END;

PROCEDURE TFrames.SwitchAndClean;
BEGIN
   FirstPage := NOT FirstPage;
   Continue;
   HideMouse;
   ClearViewPort;
   ShowMouse;
END;

PROCEDURE TFrames.Pause;
BEGIN
   IF FirstPage THEN
    BEGIN
       SetVisualPage(1);
       SetActivePage(1);
    END
   ELSE
    BEGIN
       SetVisualPage(0);
       SetActivePage(0);
    END;
END;

PROCEDURE TFrames.Continue;
BEGIN
   IF FirstPage THEN
    BEGIN
       SetVisualPage(1);
       SetActivePage(0);
    END
   ELSE
    BEGIN
       SetVisualPage(0);
       SetActivePage(1);
    END;
END;

PROCEDURE TFrames.Close;
BEGIN
   SetActivePage(0);
   SetVisualPage(0);
   FirstPage := true;
END;

PROCEDURE TFrames.Select( fNum : byte);
VAR
   i,XI  : integer;
   image : TImage;
BEGIN
   SetViewport(0,0,GetMaxX,GetMaxY,true);
   XI := round((GetMaxX+1)/2)-1;
   FOR i:=0 TO 1 DO
   BEGIN
      Image.get( i*XI, 0, (i+1)*XI, GetMaxY);
      IF FirstPage THEN SetActivePage(1) ELSE SetActivePage(0);
      Image.put(i*XI,0);
      IF FirstPage THEN SetActivePage(0) ELSE SetActivePage(1);
   END;
   WITH Views[Fnum] DO SetViewport(Vx1,Vy1,Vx2,Vy2,true);
END;

PROCEDURE TFrames.Open( fNum : byte);
VAR
   color : integer;
   line  : LineSettingsType;
BEGIN
   GetLineSettings( line);
   color := GetColor;
   HideMouse;
   WITH Views[Fnum] DO
   BEGIN
      SetViewport(Vx1-1,Vy1-1,Vx2+1,Vy2+1,true);
      ClearViewport;
      Rectangle( 0, 0, Vx2-Vx1+2, Vy2-Vy1+2);
   END;
   select(fNum);
   ShowMouse;
   SetColor( color);
   WITH line DO SetLineStyle( LineStyle, Pattern, Thickness);
END;

END.  {of Unit MUPPET}
