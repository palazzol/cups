                 (**********************************************)
                 (**********************************************)
                 (**                                          **)
                 (**     Unit CUPSgrph - 3D Plotting          **)
                 (**     Written by Jarek Tuszynski           **)
                 (**     for CUPS Project                     **)
                 (**     Department of Physics and Astronomy  **)
                 (**     George Mason University              **)
                 (**     Fairfax, Virginia 22030              **)
                 (**     (c) 1994 by Jarek Tuszynski          **)
                 (**     Originated:  91/02/28                **)
                 (**     Version: 1.3 (94/11/14)              **)
                 (**                                          **)
                 (**********************************************)
                 (**********************************************)


{$O+,F+,R-}
UNIT CUPSgrph;

INTERFACE
USES Graph, CUPSmupp, CUPS, CUPSfunc, CUPSproc, CUPSgui;

{-------------------------- Contour Plot -----------------------------------}

TYPE
   heightType = ARRAY [1..14] OF REAL;
   TContour = object
      autoSetting : boolean;
      PROCEDURE Init;
      PROCEDURE SetPaletteNum (PaletteNumber : word);
      PROCEDURE SetHeights (ContourNum :word; height :heightType);
      PROCEDURE GetHeights (VAR ContourNum : word; VAR height : heightType);
      PROCEDURE PlotFunction (f: functionXY; colorLines : boolean);
      PROCEDURE DrawFunction (f: functionXY);
      PROCEDURE DrawMatrix (VAR matrix : DMatrix);
      PROCEDURE PrintLegend (size : word);
   PRIVATE
      ContNum,PaletteNum : word;
      CHeight : heightType;
      CColorLines : boolean;
   END;

{--------------------------- 3D graphics --------------------------------}

   point2D = ARRAY [1..2] OF REAL;
   point3D = ARRAY [1..3] OF REAL;
   mat3D   = ARRAY [1..4,1..4] OF REAL;
   Curve3D = PROCEDURE (t : REAL;VAR x,y,z : REAL);
   TGraph3D = object
      RMat : mat3D;
      BoxDrawn,autoScaling,arrows : boolean;
      SurfaceColor, BoxColor, LineColor, CurveColor : integer;
      CONSTRUCTOR Init;
      PROCEDURE Set3DScales (xmin,xmax,ymin,ymax,zmin,zmax : REAL);
      PROCEDURE Get3DScales (VAR minp,maxp : point3d);
      PROCEDURE SetBoxRatios (x,y,z : REAL);
      PROCEDURE GetBoxRatios (var x,y,z : REAL);
      PROCEDURE SetViewAngles (a,b,c : REAL);
      PROCEDURE SetDistance (size_,distance_ : REAL);
      PROCEDURE GetPoint (x,y,z : REAL;VAR point : pointType);
      PROCEDURE Line3D   (x1,y1,z1,x2,y2,z2 : REAL);
      PROCEDURE Vector3D (x1,y1,z1,x2,y2,z2 : REAL);
      PROCEDURE DrawBox;
      PROCEDURE DrawSurface (VAR matrix : DMatrix);
      PROCEDURE PlotSurface (f : functionXY);
      PROCEDURE DrawCurve (f : Curve3D; minT,maxT,dT : REAL);
      PROCEDURE PlotCurve (VAR x,y,z : DVector);
   PRIVATE
      size, distance : REAL;
      min, max, boxRatio, GraphScale,GraphCenter,angle : point3D;
      StartingCase            : integer;
      PortCenter              : pointType;
      screenSize              : REAL;
      PROCEDURE GetScrPoint (p : point3D; VAR point : pointType);
      PROCEDURE Surface (VAR input; matInp : boolean);
      PROCEDURE DrawCorner (p : point3D);
   END;

  TPlot3D = object(TGraph3D)
      CONSTRUCTOR Init(vp : Integer);
      PROCEDURE PlotMatrix( VAR matrix_: DMatrix; name_: string);
      PROCEDURE Check;        {this should be in main loop}
      DESTRUCTOR done;
   PRIVATE
      active   : boolean;
      Contours : boolean;
      alpha,beta : real;
      sliders  : TSliders;
      matrix   : ^DMatrix;
      name     : string;
      vpNumber : integer;
      button   : rectType;
      PROCEDURE PlotContours;
   END;


IMPLEMENTATION
USES Crt, Dos;

{-------------------------- Contour Plot -----------------------------------}

PROCEDURE TContour.Init;
BEGIN
   PaletteNum := 1;
   AutoSetting := true;
   ContNum := 12;
END;


PROCEDURE TContour.SetPaletteNum(PaletteNumber : word);
BEGIN
   IF (PaletteNumber<=3) THEN PaletteNum:=PaletteNumber ELSE PaletteNum:=1;
END;


PROCEDURE TContour.SetHeights (ContourNum:word; height:heightType);
BEGIN
   IF ContourNum<=14 THEN ContNum:=ContourNum ELSE ContNum:=11;
   CHeight := height;
   autoSetting := false;
END;


PROCEDURE TContour.GetHeights (VAR ContourNum :word; VAR height :heightType);
BEGIN
   ContourNum := ContNum;
   height := CHeight;
END;


PROCEDURE TContour.PlotFunction (f: functionXY; colorLines : boolean);
{ Procedure that draws two kinds of contourPlot, according to 'colorLines'  }
{ flag (also see legend). It uses very little memory and gives the best     }
{ possible contour plot on this resolution, but it takes much more time than}
{ other contour plot procedures.  In this procedure every }
{ pixel on the screen is examined.}
VAR
   x,y,dx,dy : integer;
   a,b,Xnumber,Ynumber,color : word;
   viewPort        : ViewPortType;
   list            : ARRAY[0..480] OF byte;
   Xfactor,Yfactor,min,max,zVal,d : REAL;
   ch              : char;

BEGIN
   CColorLines := colorLines;
   ClearMKBuffers;
   GetViewSettings (viewPort);
   HideMouse;
   WITH ViewPort DO
   WITH GraphScales[scaleNumber] DO
   BEGIN
      IF autoSetting THEN
      BEGIN
         IF colorLines THEN ContNum := 12 ELSE contNum := 11;
         Xfactor := (xmax-xmin)/29;
         Yfactor := (ymax-ymin)/29;
         max:=f(xmin,ymax); min:=max;
         FOR x := 1 TO 30 DO
           FOR y := 1 TO 30 DO
           BEGIN
             zVal := f(xmin + (x-1)*Xfactor, ymax - (y-1)*Yfactor);
             IF zVal>max THEN max:=zVal ELSE  IF zVal<min THEN min:=zVal;
           END;
         FOR a := 1 TO ContNum DO CHeight[a] := min + (max-min)*a/(ContNum+1);
      END;
      Xnumber := x2-x1-2; {# OF points IN hor. dir.}
      Ynumber := y2-y1-2; {# OF points IN ver. dir.}
      Xfactor := (Xmax-Xmin) / Xnumber;
      Yfactor := (Ymax-Ymin) / Ynumber;
      d := (ContNum-1) / (CHeight[ContNum]-CHeight[1]);
      Rectangle (1,1,Xnumber,Ynumber);
      FOR y := 1 TO Ynumber+1 DO
      BEGIN
         list[y] := Trunc(d*(F(Xmin+Xfactor, Ymax-Y*Yfactor)-CHeight[1])+2);
         IF list[y]>ContNum+1 THEN list[y]:=ContNum+1;
      END;
      FOR x := 1 TO Xnumber DO
      BEGIN
         b:= list[1];
         list[1] :=
          Trunc(d*(F(Xmin+(x+1)*Xfactor, Ymax-Yfactor)-CHeight[1])+2);
         IF list[1]>ContNum+1 THEN list[1]:=ContNum+1;
         FOR y := 1 TO Ynumber DO
         BEGIN
            a := b; b := list[y+1];
            list[y+1] :=
             Trunc(d*(F(Xmin+(x+1)*Xfactor,Ymax-(y+1)*Yfactor)-CHeight[1])+2);
            IF list[y+1]>ContNum+1 THEN list[y+1]:=ContNum+1;
            IF CColorLines THEN
            BEGIN
               IF ((a<>list[y]) OR (a<>list[y+1]) OR (a<>b)) THEN
                  IF ((a<list[y]) OR (a<list[y+1]) OR (a<b)) THEN
                  color := a+1 ELSE color := a
               ELSE color := 0;
            END ELSE BEGIN
               IF ((a<>list[y]) OR (a<>list[y+1]) OR (a<>b))
               THEN color := 0 ELSE color := a;
            END;
            PutPixel(x,y,rainbow[PaletteNum].colors[color]);
         END; {y loop}
         IF (KeyPressed OR MouseClicked(dx,dy)) THEN
         BEGIN  IF keypressed THEN ch := readkey; exit  END;
      END; {x loop}
   END; {with}
   ShowMouse;
END; {procedure}


PROCEDURE TContour.DrawFunction (f : functionXY);
VAR
   matrix : DMatrix;
   Xfactor,Yfactor : REAL;
   row,col : integer;
BEGIN
   WITH graphScales[ScaleNumber] DO
   BEGIN
      matrix.Init(30,30);
      Xfactor := (xmax-xmin)/(30-1);
      Yfactor := (ymax-ymin)/(30-1);
      FOR row := 1 TO 30 DO
       FOR col := 1 TO 30 DO
        matrix.put(row,col,f(xmin +(col-1)*Xfactor, ymax -(row-1)*Yfactor));
      DrawMatrix (matrix);
      matrix.free;
   END;
END;

PROCEDURE TContour.DrawMatrix (VAR matrix : DMatrix);
TYPE
   edgeType = RECORD
                 xing : boolean;
                 x,y  : integer;
              END;
   edgePtr  = ^node;
   node = RECORD
             edge : edgeType;
             next : edgePtr;
          END;
VAR
   topEdge  : edgeType;
   edge     : ARRAY[1..4] OF edgeType;
   Head,p,q : edgePtr;
   CutHere  : pointer;
   col,row,maxCol,maxRow,maxY,n,k,h,PosSlope,NegSlope : integer;
   Xfactor, Yfactor,max,min,zVal : REAL;
   ViewPort : ViewPortType;

   FUNCTION EdgeXing (col1,row1,col2,row2 : integer) : boolean;
   VAR
      zVal1,zVal2 : REAL;
   BEGIN
      zVal1 := matrix.value(row1,col1);
      zVal2 := matrix.value(row2,col2);
      EdgeXing := ((zVal1 > CHeight[h]) xor (zVal2 > CHeight[h])) 
   END;

   PROCEDURE ContourFound (col1,row1,col2,row2 : integer; VAR edge : edgeType);
   VAR
      zVal1,zVal2,dis : REAL;
   BEGIN
      zVal1 := matrix.value(row1,col1);
      zVal2 := matrix.value(row2,col2);
      IF ((zVal1 > CHeight[h]) xor (zVal2 > CHeight[h])) THEN
      BEGIN
         edge.xing := true;
         dis := abs((zVal1-CHeight[h]) / (zVal1-zVal2));
         edge.x := round( xFactor*( col1 + (col2-col1)*dis -1));
         edge.y := round( yFactor*( row1 + (row2-row1)*dis -1));
      END ELSE edge.xing := false;
   END;

BEGIN
   IF autoSetting THEN
   BEGIN
      ContNum := 12;
      matrix.MinMax(n,n,min,n,n,max);
      FOR n := 1 TO ContNum DO CHeight[n] := min + (max-min)*n/(ContNum+1);
   END;
   matrix.GetSize(maxRow,maxCol);
   CColorLines := true;
   SetWriteMode(copyPut);
   GetViewSettings(ViewPort);
   HideMouse;
   WITH ViewPort DO
   BEGIN
      SetFillStyle (SolidFill,black);
      Bar (1,1,x2-x1-2,y2-y1-2);
      Xfactor := (x2-x1)/(maxCol-1);
      Yfactor := (y2-y1)/(maxRow-1);
      maxY := y2-y1;
   END;
   FOR row := 1 TO maxRow-1 DO
   BEGIN
      new (p);
      IF row=1 THEN head := p ELSE q^.next := p;
      q := p;
   END;
   FOR h := 1 TO ContNum DO
   BEGIN
      SetColor(rainbow[PaletteNum].colors[h]);
      p := head;
      FOR row := 1 TO maxRow-1 DO
      BEGIN
         ContourFound( 1, row, 1, row+1, p^.edge);
         p := p^.next;
      END;
      FOR col := 1 TO maxCol-1 DO
      BEGIN
         ContourFound( col, 1, col+1, 1, topEdge);
         p := head;
         FOR row := 1 TO maxRow-1 DO
         BEGIN
            edge[1] := topEdge;
            ContourFound( col+1, row  , col+1, row+1, edge[2]);
            ContourFound( col+1, row+1, col  , row+1, edge[3]);
            edge[4] := p^.edge;
            p^.edge := edge[2];
            p := p^.next;
            topEdge := edge[3];
            n := 0;
            FOR k := 1 TO 4 DO
            IF edge[k].xing THEN
            BEGIN
               n := n + 1 ;
               edge[n] := edge[k];
            END;
            CASE n OF
             2: line( edge[1].x, edge[1].y, edge[2].x, edge[2].y);
             4: IF (col>1) AND (col<maxCol-1) AND (row>1) AND (row<maxRow-1) THEN
                BEGIN
                   PosSlope := 0;
                   IF EdgeXing( col+1, row, col+1, row-1) THEN inc(PosSlope);
                   IF EdgeXing( col+1, row, col+2, row  ) THEN inc(PosSlope);
                   IF EdgeXing( col, row+1, col-1, row+1) THEN inc(PosSlope);
                   IF EdgeXing( col, row+1, col,   row+2) THEN inc(PosSlope);
                   NegSlope := 0;
                   IF EdgeXing( col,   row,   col-1, row  ) THEN inc(NegSlope);
                   IF EdgeXing( col,   row,   col,   row-1) THEN inc(NegSlope);
                   IF EdgeXing( col+1, row+1, col+1, row+2) THEN inc(NegSlope);
                   IF EdgeXing( col+1, row+1, col+2, row+1) THEN inc(NegSlope);
                   IF abs(PosSlope-NegSlope) > 0 THEN
                     IF PosSlope>NegSlope THEN
                     BEGIN
                       line( edge[1].x, edge[1].y, edge[4].x, edge[4].y);
                       line( edge[2].x, edge[2].y, edge[3].x, edge[3].y);
                     END
                     ELSE
                     BEGIN
                       line( edge[1].x, edge[1].y, edge[2].x, edge[2].y);
                       line( edge[3].x, edge[3].y, edge[4].x, edge[4].y);
                     END
                   ELSE
                   BEGIN
                     line( edge[1].x, edge[1].y, edge[3].x, edge[3].y);
                     line( edge[2].x, edge[2].y, edge[4].x, edge[4].y);
                   END
                END;
            END; {case}
         END; {row}
      END; {col}
   END; {height}
   FOR row := 1 TO maxRow-1 DO
   BEGIN
      p    := head;
      head := head^.next;
      dispose(p);
   END;
   SetColor(white);
   WITH ViewPort DO rectangle(0,0,x2-x1,y2-y1);
   ShowMouse;
END; {DrawContours}



PROCEDURE TContour.PrintLegend(size : word);
{ This procedure draws a legend of a contourPlot.  It is displayed in the   }
{ right bottom corner of the ViewPort. type of legend Depends on            }
{ 'contour.colorLines' value: if colorLines=true then we have color contour }
{ lines on black background, otherwise we have black contour lines on color }
{ background.}
VAR
   x,y,n,NumLen : integer;
   nStr : string;
   viewPort : viewPortType;
   envir : TEnvironment;
BEGIN
   envir.Save;
   SetTextStyle(SmallFont,HorizDir,2);
   GetViewSettings(viewPort);
   NumLen := (size+2) * TextWidth('8');
   WITH ViewPort DO BEGIN x:=x2-x1-NumLen-5; y:=y2-y1-5  END;
   HideMouse;
   IF CColorLines THEN
   BEGIN
      SetColor(white);
      rectangle (x-15,y-ContNum*10-10,x,y);
      FOR n := 1 TO ContNum DO
      BEGIN
         SetColor(rainbow[PaletteNum].colors[n]);
         line(x-14,y-n*10,x-1,y-n*10);
      END;
   END ELSE BEGIN
      SetColor(black);
      FOR n := 1 TO ContNum+1 DO
      BEGIN
         SetFillStyle (SolidFill,rainbow[PaletteNum].colors[n]);
         Bar      (x-15,y-n*10,x,y-n*10+10);
         Rectangle(x-15,y-n*10,x,y-n*10+10);
      END;
   END;
   SetColor(white);
   FOR n := 1 TO ContNum DO
   BEGIN
      IF CHeight[n]>=0 THEN nStr := ' '+Num2Str(CHeight[n], size)
      ELSE nStr := Num2Str(CHeight[n], size+1);  {so pos. and neg. are even}
      OutTextXY(x+2,y-n*10-TextHeight('8') DIV 2,nStr);
   END;
   ShowMouse;
   envir.Reset;
END;

{------------------------------- 3D graphics -----------------------------}

PROCEDURE MultMatrix(AMat,BMat: mat3D; VAR CMat: Mat3D);
VAR
   i,j,k: INTEGER;
   Sum: REAL;
BEGIN
   FOR i:=1 TO 4 DO FOR j:= 1 TO 4 DO
   BEGIN
      Sum:=0.0;
      FOR k:= 1 TO 4 DO Sum:= Sum + AMat[i,k]*BMat[k,j];
      CMat[i,j]:=Sum;
   END
END;

CONSTRUCTOR Tgraph3D.Init;
VAR viewPort : viewPortType;
BEGIN
   SurfaceColor := Green;
   BoxColor     := white;
   LineColor    := white;
   CurveColor   := red;
   autoScaling  := true;
   BoxDrawn     := false;
   arrows       := false;
   BoxRatio[1]  := 1;
   BoxRatio[2]  := 1;
   BoxRatio[3]  := 0.6;
   distance     := 2.5;
   Set3DScales     (-1,1, -1,1, 1,1);
   GetViewSettings (viewPort);
   WITH ViewPort DO
   BEGIN
      PortCenter.x := (x2-x1) DIV 2;
      PortCenter.y := (y2-y1) DIV 2;
      IF (x2-x1 > y2-y1) THEN ScreenSize := (y2-y1) / sqrt(3)
      ELSE ScreenSize := (x2-x1) / sqrt(3);
   END;
   Size := screenSize*distance;
   SetViewAngles (30,20,0);
END;


PROCEDURE Tgraph3D.Set3DScales (xmin,xmax,ymin,ymax,zmin,zmax : REAL);
VAR n : byte;
BEGIN
   autoScaling := (zmax<=zmin);
   min[1]:=xmin;   min[2]:=ymin;   min[3]:=zmin;
   max[1]:=xmax;   max[2]:=ymax;   max[3]:=zmax;
   FOR n := 1 TO 3 DO
   BEGIN
      IF max[n]<=min[n] THEN BEGIN min[n]:=-1; max[n]:=1 END;
      GraphCenter[n] := (max[n]+min[n]) / 2;
      IF max[n]=min[n] THEN GraphScale[n] := 0 ELSE
      GraphScale[n] := BoxRatio[n] / (max[n]-min[n]);
   END;
END;


PROCEDURE TGraph3D.Get3DScales (VAR minp,maxp : point3d);
BEGIN
   minp:=min; maxp:=max;
END;


PROCEDURE TGraph3D.SetBoxRatios(x,y,z : REAL);
VAR
   m : REAL;
   n : byte;
BEGIN
   m:=x;                 IF m<y THEN m:=y;     IF m<z THEN m:=z;
   BoxRatio[1] := x/m;   BoxRatio[2] := y/m;   BoxRatio[3] := z/m;
   FOR n := 1 TO 3 DO GraphScale[n] := BoxRatio[n] / (max[n]-min[n]);
END;


PROCEDURE TGraph3D.GetBoxRatios(var x,y,z : REAL);
BEGIN
   x := BoxRatio[1];
   y := BoxRatio[2];
   z := BoxRatio[3];
END;


PROCEDURE TGraph3D.SetDistance (size_,distance_ : REAL);
BEGIN
   size := size_*distance_*screenSize;
   distance := distance_;
   SetViewAngles(angle[1],angle[2],angle[3]);
END;


PROCEDURE TGraph3D.SetViewAngles (a,b,c : REAL);
VAR
   sa,sb,sc, ca,cb,cc, minY,mY : REAL;
   x,y,z : point3D;
   i,j : integer;
BEGIN
   angle[1]:=a;           angle[2]:=b;           angle[3]:=c;
   a:=a*Pi/180;           b:=b*Pi/180;           c:=c*Pi/180;
   sa:=sin(a);            sb:=sin(b);            sc:=sin(c);
   ca:=cos(a);            cb:=cos(b);            cc:=cos(c);
   x[1]:= ca*cc-sa*cb*sc; x[2]:=-sa*cc-ca*cb*sc; x[3]:= sb*sc;
   y[1]:= ca*sc+sa*cb*cc; y[2]:=-sa*sc+ca*cb*cc; y[3]:=-sb*cc;
   z[1]:= sa*sb;          z[2]:= ca*sb;          z[3]:= cb;
   FOR i := 1 TO 3 DO
   BEGIN
      RMat[1,i]:=PortCenter.x*y[i] +size*x[i];
      RMat[2,i]:=PortCenter.y*y[i] -size*z[i];
      RMat[3,i]:=y[i];
      RMat[4,i]:=y[i];
   END;
   RMat[1,4]:=PortCenter.x*distance;  RMat[2,4]:=PortCenter.y*distance;
   RMat[3,4]:=0;                      RMat[4,4]:=distance;
   minY := 0;
   FOR i := 0 TO 1 DO FOR j := 0 TO 1 DO
   BEGIN
      my := (2*i-1) * (y[1]*((j+1) MOD 2) +y[2]*j);
      IF mY<minY THEN BEGIN StartingCase:=2*i+j; minY:=mY; END;
   END;
END;


PROCEDURE TGraph3D.GetScrPoint (p : point3D; VAR point : pointType);
VAR
   q : point3d;
   n : byte;
BEGIN
   q[1]:= RMat[1,1]*p[1] +RMat[1,2]*p[2] +RMat[1,3]*p[3] +RMat[1,4];
   q[2]:= RMat[2,1]*p[1] +RMat[2,2]*p[2] +RMat[2,3]*p[3] +RMat[2,4];
   q[3]:= RMat[4,1]*p[1] +RMat[4,2]*p[2] +RMat[4,3]*p[3] +RMat[4,4];
   point.x:=round(q[1]/q[3]);
   point.y:=round(q[2]/q[3]);
END;


PROCEDURE TGraph3D.GetPoint (x,y,z : REAL; VAR point : pointType);
VAR
   n : byte;
   p,q : point3d;
BEGIN
   p[1]:=x;  p[2]:=y;  p[3]:=z;
   FOR n := 1 TO 3 DO q[n] := GraphScale[n] * (p[n]-GraphCenter[n]);
   GetScrPoint(q, point);
END;


PROCEDURE TGraph3D.Line3D (x1,y1,z1,x2,y2,z2 : REAL);
VAR p,q : pointType;
BEGIN
   GetPoint (x1,y1,z1, p);
   GetPoint (x2,y2,z2, q);
   Line (p.x,p.y, q.x,q.y);
END;


PROCEDURE TGraph3D.Vector3D (x1,y1,z1,x2,y2,z2 : REAL);
VAR p,q : pointType;
BEGIN
   GetPoint (x1,y1,z1, p);
   GetPoint (x2,y2,z2, q);
   Vector (p.x,p.y, q.x,q.y);
END;


PROCEDURE tGraph3D.DrawCorner(p : point3D);
VAR
   n : integer;
   q : point3D;
   flag  : boolean;
   a,b   : pointType;
BEGIN
   SetColor(boxColor);
   flag := arrows AND (p[1]=-1) AND (p[2]=-1) AND (p[3]=-1);
   FOR n := 1 TO 3 DO p[n] := p[n]*BoxRatio[n]/2;
   GetScrPoint(p,a);
   HideMouse;
   FOR n := 1 TO 3 DO
   BEGIN
      q:=p; IF flag THEN q[n]:=-1.2*p[n] ELSE q[n]:=-p[n];
      GetScrPoint(q,b);
      IF flag THEN vector (a.x,a.y, b.x,b.y) ELSE line (a.x,a.y, b.x,b.y);
   END;
   ShowMouse;
END;


PROCEDURE TGraph3D.DrawBox;
VAR p : ARRAY [0..7] OF pointType;
    n,m, color : integer;
    r : point3D;
BEGIN
   color := GetColor;
   BoxDrawn := true;
   FOR n := 1 TO 7 DO
   BEGIN
      r[1]:=(n SHR 2)-0.5; r[2]:=((n SHR 1) MOD 2)-0.5; r[3]:=(n MOD 2)-0.5;
      FOR m := 1 TO 3 DO r[m] := r[m]*BoxRatio[m];
      GetScrPoint (r, p[n]);
   END;
   SetColor(boxColor);
   HideMouse;
   FOR n := 1 TO 3 DO Line (p[n].x,p[n].y, p[n+4].x,p[n+4].y);
   FOR n := 1 TO 3 DO Line (p[2*n].x,p[2*n].y, p[2*n+1].x,p[2*n+1].y);
   FOR n := 4 TO 5 DO Line (p[n].x,p[n].y, p[n+2].x,p[n+2].y);
   Line (p[1].x,p[1].y, p[3].x,p[3].y);
   ShowMouse;
   r[1]:=-1; r[2]:=-1; r[3]:=-1;
   DrawCorner(r);
   SetColor(color);
END;

PROCEDURE TGraph3D.DrawSurface (VAR matrix : DMatrix);
BEGIN
   surface (matrix,true);
END;

PROCEDURE TGraph3D.PlotSurface (f : functionXY);
BEGIN
   surface (f,false);
END;

PROCEDURE TGraph3D.Surface(VAR input; matInp : boolean);
TYPE
   nodePtr  = ^nodeType;
   nodeType = RECORD
                 point : pointType;
                 zVal  : REAL;
                 next  : nodePtr;
               END;
VAR
   zVal1,zVal2,zVal3,zVal4,zAvr,y,minY,XFactor,YFactor  : REAL;
   xs,ys,maxC,maxR,n,l     : integer;
   fillSetting             : FillSettingsType;
   CutHere                 : pointer;
   head,node,node1         : nodePtr;
   col,row,maxCol,maxRow,shadeNum,color1,color2 : integer;
   poly                    : ARRAY[1..4] OF PointType;
   m,d                     : pointType;   
   q,r                     : point3D;
   PMat                    : mat3D;
   DMatPtr                 : ^Dmatrix;
   DMat                    : DMatrix;
   fctXY                   : functionXY;

   FUNCTION mat (row,col : integer) : REAL;
   BEGIN
      IF matInp THEN mat := DMat.matPtr^[(Row-1)*MaxCol+Col] ELSE
      mat := fctXY( min[1] + (col-1)*XFactor, max[2] - (row-1)*Yfactor);
   END;

   PROCEDURE FindCoords (row,col : word;
                         VAR point : pointType; VAR zVal : real);
   VAR
      c,r : word;
      q : point3D;
   BEGIN
      c := m.x*col -m.y*row +d.x;
      r := m.y*col +m.x*row +d.y;  
      zVal := mat(r,c);
      IF (zVal>max[3]) THEN zVal := max[3] ELSE   {cliping}
      IF (zVal<min[3]) THEN zVal := min[3];
      q[1]:= PMat[1,1]*c +PMat[1,2]*r +PMat[1,3]*zVal +PMat[1,4];
      q[2]:= PMat[2,1]*c +PMat[2,2]*r +PMat[2,3]*zVal +PMat[2,4];
      q[3]:= PMat[4,1]*c +PMat[4,2]*r +PMat[4,3]*zVal +PMat[4,4];
      point.x:=round(q[1]/q[3]);
      point.y:=round(q[2]/q[3]);
   END;

BEGIN
   IF matInp THEN
   BEGIN
      Dmat := Dmatrix(Input);
      Dmat.GetSize(maxRow,maxCol);
   END ELSE BEGIN
      fctXY := functionXY(Input);
      maxCol:=25; maxRow:=25;
      XFactor := (max[1]-min[1])/(maxCol-1);
      YFactor := (max[2]-min[2])/(maxRow-1);
   END;
   GetFillSettings (fillSetting);
   color1 := fillSetting.color;
   color2 := getColor;
   IF boxDrawn THEN DrawBox;
   SetColor(LineColor);
   IF autoScaling THEN   {find max[3] and min[3]}
   BEGIN
      max[3] := mat(1,1);  min[3] := max[3];
      FOR col := 1 TO maxCol DO
       FOR row := 1 TO maxRow DO
        IF mat(row,col)>max[3] THEN max[3]:=mat(row,col) ELSE
        IF mat(row,col)<min[3] THEN min[3]:=mat(row,col);
      IF max[3]=min[3] THEN
      BEGIN
         max[3] := max[3] + 1;
         min[3] := min[3] - 1;
      END;
      GraphScale[3]  := BoxRatio[3] / (max[3]-min[3]);
      GraphCenter[3] := (max[3]+min[3]) / 2;
   END;
   FOR n := 1 TO 4 DO FOR l := 1 TO 4 DO PMat[n,l]:=0;
   PMat[1,1]:= BoxRatio[1]/(maxCol-1);
   PMat[1,4]:=-BoxRatio[1]*(1/(maxCol-1)+0.5);
   PMat[2,2]:=-BoxRatio[2]/(maxRow-1);
   PMat[2,4]:= BoxRatio[2]*(maxRow/(maxRow-1)-0.5);
   PMat[3,3]:= GraphScale[3];
   PMat[3,4]:=-GraphScale[3]*GraphCenter[3];
   PMat[4,4]:= 1;
   MultMatrix(RMat,PMat,PMat);
   CASE StartingCase OF
    1,3 : BEGIN maxR:=maxRow; maxC:=maxCol; END;
    0,2 : BEGIN maxR:=maxCol; maxC:=maxRow; END;
   END;
   l:=StartingCase SHR 1;
   n:=StartingCase MOD 2;
   m.x:=(1-2*l)*n;
   m.y:=(1-2*l)*((n+1) MOD 2);
   d.x:=(((l xor n)+1) MOD 2)*(maxC*n+maxR*((n+1) MOD 2) +1);
   d.y:=l*(maxR*n+maxC*((n+1) MOD 2) +1);
   FOR col := 1 TO maxC DO   {draw surface}
   BEGIN
      new (node);
      FindCoords( 1, col, node^.point, node^.zVal);
      IF col=1 THEN head := node ELSE node1^.next := node;
      node1 := node;
   END;
   FOR row := 2 TO maxR DO
   BEGIN
      node := head;
      FindCoords( row, 1, poly[4], zVal4);
      HideMouse;
      FOR col := 2 TO maxC DO
      BEGIN
         poly[1] := node^.next^.point;  zVal1 := node^.next^.zVal;
         poly[2] := node^.point;        zVal2 := node^.zVal;
         poly[3] := poly[4];            zVal3 := zVal4;
         FindCoords(row,col,poly[4],zVal4);
         zAvr := (zVal1+zVal2+zVal3+zVal4) / 4;
         IF zAvr=min[3] THEN shadeNum := 0 ELSE
         shadeNum := Trunc(8*(zAvr-min[3])/(max[3]-min[3])+1) MOD 9;
         SetFillPattern(shade[shadeNum],surfaceColor);
         FillPoly(4,poly);
         node^.point := poly[3];       node^.zVal:=zVal3;
         node := node^.next;
      END;
      ShowMouse;
      node^.point := poly[4];          node^.zVal:=zVal4;
   END;
   FOR row := 1 TO maxC DO
   BEGIN
      node := head;
      head := head^.next;
      dispose(node);
   END;
   IF BoxDrawn THEN    {redraw erased parts of box}
   BEGIN
      minY :=0;
      FOR n := 0 TO 7 DO
      BEGIN
         q[1]:=2*(n SHR 2)-1; q[2]:=2*((n SHR 1)mod 2)-1; q[3]:=2*(n MOD 2)-1;
         y := RMat[3,1]*q[1] + RMat[3,2]*q[2] + RMat[3,3]*q[3];
         IF y<minY THEN BEGIN minY:=y; r:=q END;
      END;
      DrawCorner(r);
   END;
   WITH fillSetting DO SetFillStyle (Pattern, Color);
   SetColor(color2);
END;


PROCEDURE TGraph3D.DrawCurve (f : Curve3D; minT,maxT,dT : REAL);
VAR
   n : byte;
   t,x,y,z : REAL;
   p,mn,mx : point3D;
   q,r : pointType;
   color : integer;
   line_ : lineSettingsType;
BEGIN
   color := GetColor;
   GetLineSettings (line_);
   IF autoScaling THEN
   BEGIN
      f(1,p[1],p[2],p[3]);
      FOR n := 1 TO 3 DO BEGIN mx[n]:=p[n]; mn[n]:=p[n] END ;
      t := minT;
      REPEAT
         f(t,p[1],p[2],p[3]);
         FOR n := 1 TO 3 DO
         IF p[n]>mx[n] THEN mx[n]:=p[n] ELSE IF p[n]<mn[n] THEN mn[n]:=p[n];
         t := t+dt;
      UNTIL (t>=maxT);
      Set3DScales(mn[1],mx[1], mn[2],mx[2], mn[3],mx[3]);
      autoScaling := true;
   END;
   IF boxDrawn THEN DrawBox;
   SetColor(CurveColor);
   t := minT;
   f(t,x,y,z);
   GetPoint(x,y,z, q);
   HideMouse;
   REPEAT
      t := t+dt;
      f(t,x,y,z);
      GetPoint(x,y,z, r);
      line (q.x+1,q.y,r.x,r.y);
      q:=r;
   UNTIL (t>=maxT);
   ShowMouse;
   WITH line_ DO SetLineStyle (LineStyle, Pattern, Thickness);
   SetColor(color);
END;

PROCEDURE TGraph3D.PlotCurve (VAR x,y,z : DVector);
VAR
   n,m,numData : integer;
   mn,mx : point3D;
   q,p : pointType;
   color : integer;
   line_ : lineSettingsType;
BEGIN
   color := GetColor;
   GetLineSettings (line_);
   numData := x.length;
   IF autoScaling THEN
   BEGIN
      x.MinMax(n,mn[1],m,mx[1]);
      y.MinMax(n,mn[2],m,mx[2]);
      z.MinMax(n,mn[3],m,mx[3]);
      Set3DScales(mn[1],mx[1], mn[2],mx[2], mn[3],mx[3]);
      autoScaling := true;
   END;
   IF boxDrawn THEN DrawBox;
   SetColor(CurveColor);
   GetPoint(x.vecPtr^[1],y.vecPtr^[1],z.vecPtr^[1], q);
   HideMouse;
   FOR n := 2 TO numData DO
   BEGIN
      GetPoint(x.vecPtr^[n],y.vecPtr^[n],z.vecPtr^[n], p);
      line (q.x+1,q.y,p.x,p.y);
      q:=p;
   END;
   ShowMouse;
   WITH line_ DO SetLineStyle (LineStyle, Pattern, Thickness);
   SetColor(color);
END;

{--------------------------- TPlot3d --------------------------------------}

FUNCTION PtInside(x,y : integer; r:rectType) : boolean;
BEGIN
    PtInside := (x>r.x1) AND (x<r.x2) AND
                (y>r.y1) AND (y<r.y2);
END;

CONSTRUCTOR TPlot3D.Init(vp : integer);
VAR
   xmin,xmax,ymin,ymax : real;
   d : integer;
BEGIN
   IF vp>10 THEN
      Error('TPlot3D.Init: Please draw your graph in viewports 1 to 10.');
   active   := false;
   contours := false;
   alpha    := 30;
   beta     := 20;
   vpNumber := VP;
   d        := round(0.025*GetMaxX);
   sliders.init;
   WITH views[vpNumber] DO
   BEGIN
      xmin := vx1/GetMaxX;
      xmax := vx2/GetMaxX;
      ymin := 1-vy2/GetMaxY;
      ymax := 1-(vy1+1)/GetMaxY;
      button.x1:=vx1+6;     button.y1:=vy1;
      button.x2:=vx1+5+d;   button.y2:=vy1+d;
   END;
   DefineViewPort(10+vpNumber, xmin+0.04, xmax, ymin, ymax-0.04);
   SelectViewPort(10+vpNumber);
   TGraph3D.init;
   SetViewAngles(alpha,beta,0);
   SetDistance(1.3,2.5);
   arrows := true;
   sliders.create(1,  0 ,360,alpha, xmin+0.04, ymax-0.03, xmax-0.01,0,'','','',false);
   sliders.create(2,-180,180, beta, xmin+0.01, ymin,      ymax-0.04,0,'','','',true );
END;

PROCEDURE TPlot3D.PlotContours;
VAR
   envir : TEnvironment;
   contour : TContour;
   x,y,z : real;
   vp : viewPortType;
   x1,x2,y1,y2 : integer;
BEGIN
   envir.Save;
   envir.Standardize;
   GetBoxRatios(x,y,z);
   OpenViewPort(vpNumber);
   with views[vpNumber] do
   if ((vx2-vx1)/x > (vy2-vy1)/y) then
   begin
      x1:= round(vx1+vx2+(vy1-vy2)*x/y) div 2;
      x2:= round(vx1+vx2-(vy1-vy2)*x/y) div 2;
      y1 := vy1;
      y2 := vy2;
   end
   else
   begin
      x1 := vx1;
      x2 := vx2;
      y1:= round(vy1+vy2-(vx1-vx2)*y/x) div 2;
      y2:= round(vy1+vy2+(vx1-vx2)*y/x) div 2;
   end;
   SetViewPort(x1,y1,x2,y2,true);
   contour.init;
   contour.DrawMatrix(matrix^);
   SetColor(white);
   Print(4,1,name);
   SelectMuppetPort;
   HideMouse;
   SetFillStyle(SolidFill,DarkGray);
   WITH button DO bar(x1-6,y1,x2-6,y2);
   WITH button DO rectangle(x1-6,y1,x2-6,y2);
   SetFillStyle(SolidFill,black);
   WITH button DO bar(x1,y1+6,x2-12,y2-6);
   ShowMouse;
   SelectViewPort(vpNumber);
   envir.Reset;
END;

PROCEDURE TPlot3D.PlotMatrix( VAR matrix_: DMatrix; name_: string);
VAR
   envir : TEnvironment;
BEGIN
   envir.Save;
   envir.Standardize;
   matrix := @matrix_;
   name   := name_;
   active := true;
   sliders.drawAll;
   HideMouse;
   SetFillStyle(SolidFill,DarkGray);
   WITH button DO bar(x1,y1,x2,y2);
   WITH button DO rectangle(x1,y1,x2,y2);
   SetFillStyle(SolidFill,black);
   WITH button DO bar(x1+6,y1+6,x2-6,y2-6);
   ShowMouse;
   OpenViewPort(10+vpNumber);
   DrawBox;
   DrawSurface(matrix^);
   BoxColor := BoxColor xor GraphBackColor;
   SetColor(white);
   Print(1,1,name_);
   envir.Reset;
END;


PROCEDURE TPlot3D.check;
VAR
   x : integer;
BEGIN
   IF active THEN
   BEGIN
      IF (NOT contours) AND sliders.Changed THEN
      BEGIN
         OpenViewPort(10+vpNumber);
         SetWriteMode (XorPut);
         DrawBox;
         REPEAT
            CheckForEvents;
            DrawBox;
            alpha := sliders.value(1);
            beta  := sliders.value(2);
            SetViewAngles(alpha,beta,0);
            DrawBox;
            Delay(20);
         UNTIL NOT sliders.changed;
         SetWriteMode (CopyPut);
         BoxColor := BoxColor xor GraphBackColor;
         DrawSurface(matrix^);
         BoxColor := BoxColor xor GraphBackColor;
         Print(1,1,name);
      END;
      IF event.mouseClicked THEN
      BEGIN
         IF contours THEN x:=event.x+6 ELSE x:=event.x;
         IF PtInside(x,event.y,button) THEN
         BEGIN
            CloseViewPort(vpNumber);
            IF NOT contours THEN
            BEGIN
               sliders.erase(1,black);
               sliders.erase(2,black);
               PlotContours;
            END ELSE PlotMatrix(matrix^,name);
            contours := NOT contours;
         END;
      END;
   END;
END;

DESTRUCTOR TPlot3D.done;
BEGIN
   CloseViewPort(vpNumber);
   SelectViewPort(vpNumber);
   sliders.erase(1,black);
   sliders.erase(2,black);
   Sliders.done;
   active := false;
END;

END. {unit CUPSgrph}
