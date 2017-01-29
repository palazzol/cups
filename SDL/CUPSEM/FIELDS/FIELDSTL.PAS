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

                   {****************************************}
                   {****************************************}
                   {***          Unit FieldsTl           ***}
                   {***  Part of the Program Fields.Pas  ***}
                   {***    Written by Jarek Tuszynski    ***}
                   {***         January 3, 1993          ***}
                   {****************************************}
                   {****************************************}
unit FieldsTl;
{ Fields_Tools unit contains various independent and selfcontaining }
{ objects which were used in Fields program.}

{$R-}
interface
USES Graph, Crt, CupsMupp, CUPS, CupsGrph, CupsFunc, CupsProc,
     CupsGui, CupsPars;
type

   point = record
      x,y : real;
   end;

   numD = (_2D,_3D);
   kind = (scalar_,vector_);
   VMatrix = array[1..3] of DMatrix;
   TPlot = object(TGraph3D)
      active,TwoD : boolean;
      CONSTRUCTOR Init(vp : Integer);
      procedure PlotScalar( TwoD_: boolean; var matrix_: DMatrix; name_: string);
      procedure PlotVector( TwoD_: boolean; var matrix_: VMatrix; name_: string; plane : integer);
      procedure Check(var update,inside : boolean; var p : point);
      procedure PlotMagnitude( TwoD_: boolean; var matrix_: VMatrix;
                               name_: string;  var mag: DMatrix);
      procedure Erase;
      DESTRUCTOR done;
   PRIVATE
      alpha,beta : real;
      sliders  : TSliders;
      matrix   : ^DMatrix;
      matrix3  : ^VMatrix;
      name     : string;
      vpNumber : integer;
      button   : rectType;
      PlaneNorm : integer;
      procedure DrawButton;
      procedure DrawVectors(var matrix_: vMatrix; step : integer);
   end;

   Figure = procedure(a,b,c : pointType);
   ProbeType = procedure;
   TIntegral = object
      active : boolean;
      port   : integer;
      procedure Init;
      procedure Clean;
      procedure Prepare;
      procedure ReadPath(probe : probeType; dis : point);
      procedure ScalarField(field : DMatrix; ScaleVec_: point; PlaneNorm_: integer; name : string);
      procedure VectorField(field : VMatrix; ScaleVec_: point; PlaneNorm_: integer; name : string);
      function  Check : boolean;
      procedure DrawPath( viewport : integer; dis : point);
      procedure Done;
   private
      oldPort, PlaneNorm : integer;
      OpenPath, scalar   : boolean;
      sField   : DMatrix;
      vField   : VMatrix;
      x,y      : IDVector;
      d,v      : DVector;
      buttons  : TButtons;
      ScaleVec : point;
      procedure info (str : string);
      procedure CalculateField(n : integer);
      procedure Integrate(name : string);
      procedure MapOnGridX(a,b: pointType; var d1,v1: DVector; PathVec : point3D);
      procedure MapOnGridY(a,b: pointType; var d2,v2: DVector; PathVec : point3D);
      procedure MergeDVectors(var d1,v1,d2,v2 : DVector; distance : real);
   end;


procedure DrawVectorField (Matrix : VMatrix; PlaneNorm : integer);
procedure SetPoint(var p : point; x,y : real);
procedure SetPoint3D(var p : Point3D; x,y,z : real);

implementation
{--------------------------- TPlot --------------------------------------}

FUNCTION PtInside(x,y : integer; r:rectType) : boolean;
BEGIN
    PtInside := (x>r.x1) AND (x<r.x2) AND
                (y>r.y1) AND (y<r.y2);
END;

function NStr(num : real; i : integer) : string;
var s : string;
begin
   s := Num2Str(num,i);
   if num>=0 then s := ' '+s;
   NStr := s;
end;


CONSTRUCTOR TPlot.Init(vp : integer);
VAR
   xmin,xmax,xmin2,xmax2,ymin,ymax : real;
   d : integer;
BEGIN
   If vp>10 then
      Error('TPlot.Init: Please draw your graph in viewports 1 to 10.');
   active   := false;
   alpha    := 30;
   beta     := 20;
   vpNumber := VP;
   matrix   := nil;
   matrix3  := nil;
   TwoD     := true;
   d        := round(0.025*GetMaxX);
   WITH views[vpNumber] DO
   BEGIN
      xmin := vx1/GetMaxX;
      xmax := vx2/GetMaxX;
      xmin2:= vx1/GetMaxX;
      xmax2:= (vx1+vy2-vy1)/GetMaxX;
      ymin := 1-vy2/GetMaxY;
      ymax := 1-(vy1+1)/GetMaxY;
      button.x1:=vx1+6;     button.y1:=vy1;
      button.x2:=vx1+5+d;   button.y2:=vy1+d;
   END;
   DefineViewPort(10+vpNumber, xmin+0.04, xmax, ymin, ymax-0.04);
   DefineViewPort(15+vpNumber, xmin2, xmax2, ymin, ymax);
   SelectViewPort(10+vpNumber);
   TGraph3D.init;
   SetViewAngles(alpha,beta,0);
   SetDistance(1.3,2.5);
   arrows := true;
   sliders.init;
   sliders.create(1, 0,360,alpha, xmin+0.04, ymax-0.03, xmax-0.01,0,'','','',false);
   sliders.create(2,-180,180, beta, xmin+0.01, ymin,      ymax-0.04,0,'','','',true );
END;


PROCEDURE TPlot.PlotScalar( TwoD_: boolean; VAR matrix_: DMatrix; name_: string);
var
   contour : TContour;
   min,max : real;
   i : integer;
BEGIN
   erase;
   matrix := @matrix_;    name := name_;
   active := true;        TwoD := TwoD_;
   matrix3:= nil;
   if TwoD then
    begin
      OpenViewPort(vpNumber);
      matrix_.MinMax(i,i,Min,i,i,Max);
      if min=max then
      begin
         Print(27,1,'Const. Func.');
         Print(28,2,'='+Num2Str(min,6));
         GraphBackColor := black;
         OpenViewPort(15+vpNumber);
         GraphBackColor := DarkGray;
      end
      else
      begin
         Print(27,1,'Min ='+NStr(min,6));
         Print(27,2,'Max ='+NStr(max,6));
         SelectViewPort(15+vpNumber);
         contour.init;
         contour.DrawMatrix(matrix^);
         DefineScale(2, 1,matrix^.NCols, matrix^.NRows,1);
         SelectViewPort(vpNumber);
         contour.PrintLegend(6);
      end;
    end
   else
    begin
      sliders.drawAll;
      OpenViewPort(10+vpNumber);
      DrawBox;
      DrawSurface(matrix^);
      BoxColor := BoxColor xor GraphBackColor;
    end;
    DrawButton;
END;


PROCEDURE TPlot.PlotVector( TwoD_: boolean; VAR matrix_: VMatrix; name_: string; plane : integer);
var
   min,max : array[1..3] of real;
   i,j : integer;
   xyz : string;
BEGIN
   erase;
   matrix3 := @matrix_;    name := name_;
   active  := true;        TwoD := TwoD_;
   matrix  := nil;
   PlaneNorm := Plane;
   if TwoD then
    begin
      for j := 1 to 3 do matrix_[j].MinMax(i,i,Min[j],i,i,Max[j]);
      OpenViewPort(vpNumber);
      Print(27,1,'x Min='+NStr(min[1],5));
      Print(27,2,'x Max='+NStr(max[1],5));
      Print(27,3,'y Min='+NStr(min[2],5));
      Print(27,4,'y Max='+NStr(max[2],5));
      Print(27,5,'z Min='+NStr(min[3],5));
      Print(27,6,'z Max='+NStr(max[3],5));
      xyz := 'xyz';
      Print(27,8,'Color shows');
      Print(27,9,xyz[PlaneNorm]+' comp. of');
      Print(27,10,'the vectors');
      SelectViewPort(15+vpNumber);
      DrawVectorField(matrix3^,PlaneNorm);
      DefineScale(2, 1,matrix3^[1].NCols, matrix3^[1].NRows,1);
    end
   else
    begin
     sliders.drawAll;
     OpenViewPort(10+vpNumber);
     DrawBox;
     DrawVectors(matrix3^,3);
     BoxColor := BoxColor xor GraphBackColor;
    end;
    DrawButton;
END;


procedure TPlot.PlotMagnitude( TwoD_: boolean; var matrix_: VMatrix;
                               name_: string;  var mag: DMatrix);
var
   p,MaxRow,MaxCol : integer;
begin
   matrix_[1].GetSize(MaxRow,MaxCol);
   mag.init(MaxRow,MaxCol);
   for p := 1 to MaxRow*MaxCol do
    mag.MatPtr^[p] := sqrt(sqr(matrix_[1].MatPtr^[p])
                          +sqr(matrix_[2].MatPtr^[p])
                          +sqr(matrix_[3].MatPtr^[p]));
   PlotScalar( TwoD_, mag, name_);
end;


PROCEDURE TPlot.check(var update,inside : boolean; var p : point);
var
   x : integer;
BEGIN
   update := false;
   IF active THEN
   begin
      IF (not TwoD) and sliders.Changed THEN
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
         if matrix=nil then DrawVectors(matrix3^,3)
         else DrawSurface(matrix^);
         BoxColor := BoxColor xor GraphBackColor;
         Print(1,1,name);
      END;
      if event.mouseClicked then  {button}
      begin
         if TwoD then x:=event.x+6 else x:=event.x;
         if PtInside(x,event.y,button) then
         begin
            erase;
            active := true;
            if matrix=nil then PlotVector(not TwoD,matrix3^,name, PlaneNorm)
            else               PlotScalar(not TwoD,matrix^, name);
            update:=TwoD;
         end;
      end;
      If (not Inside) and TwoD then mousePosn(p.x,p.y,vpNumber+15,2,inside);
   end;
END;

procedure TPlot.DrawButton;
var
   envir : TEnvironment;
   d : integer;
begin
   envir.Save;
   envir.Standardize;
   if TwoD then d:=6 else d:=0;
   HideMouse;
   SetFillStyle(SolidFill,GraphBackColor);
   with button do bar(x1-d,y1,x2-d,y2);
   with button do rectangle(x1-d,y1,x2-d,y2);
   SetFillStyle(SolidFill,black);
   with button do bar(x1+6-d,y1+6,x2-6-d,y2-6);
   ShowMouse;
   if TwoD then
   begin
      SelectViewPort(VpNumber+15);
      PutLabel(inside,name)
   end
   else
   begin
      SelectViewPort(VpNumber+10);
      SetColor(white);
      Print(1,1,name);
   end;
   envir.Reset;
end;

procedure TPlot.Erase;
begin
   active := false;
   CloseViewPort(vpNumber);
   SelectViewPort(vpNumber);
   if not TwoD then
   begin
      sliders.erase(1,black);
      sliders.erase(2,black);
   end;
end;

DESTRUCTOR TPlot.done;
BEGIN
   erase;
   Sliders.done;
END;

procedure TPlot.DrawVectors(var matrix_: vMatrix; step : integer);
VAR
   i,j,k,c,r,m,MaxCol,MaxRow,pos,color : integer;
   maxE,maxEz,minEz,dE,disX,disY : real;
   E : point3D;
begin
   color := GetColor;
   DrawBox;
   matrix_[1].GetSize(MaxRow,MaxCol);
   i := (maxCol+maxRow) div (4*step);
   Set3DScales(1,MaxCol div step, 1,MaxRow div step, -2,2);
   maxE  := 0;
   case PlaneNorm of
    1: begin i:=2; j:=3; k:=1; end;
    2: begin i:=3; j:=1; k:=2; end;
    3: begin i:=1; j:=2; k:=3; end;
   end;
   FOR c := 1 TO maxCol div step DO             {find max E on the grid}
    FOR r := 1 TO maxRow div step DO
    begin
       dE  := sqr(Matrix_[1].value(step*r,step*c))
             +sqr(Matrix_[2].value(step*r,step*c))
             +sqr(Matrix_[3].value(step*r,step*c));
       if   dE>maxE  then maxE :=dE;
    END;
   if maxE<1e-8 then maxE:=1 else maxE:=sqrt(maxE);
   FOR c := 1 TO maxCol div step DO
    FOR r := 1 TO maxRow div step DO
     begin
        for m := 1 to 3 do E[m] := Matrix_[m].value(maxRow-step*r+1,step*c);
        dE := sqrt( sqr(E[1]) +sqr(E[2]) +sqr(E[3]) );
        if dE<1E-8 then SetColor(yellow)
        else SetColor(rainbow[1].colors[trunc(11*dE/maxE)+1]);
        vector3D( c, r, 0, c+E[i]/maxE, r+E[j]/maxE, E[k]/maxE);
     END;
   SetColor(color);
end;


procedure DrawVectorField (Matrix : VMatrix; PlaneNorm : integer);
{ Draws field vectors in the current view-port. stand-alone }
VAR
   c,r,m,i,j,k,MaxCol,MaxRow,pos,color : integer;
   maxE,maxEz,minEz,dE,dEz,disX,disY     : real;
   E : point3D;
begin
   case PlaneNorm of
    1: begin i:=2; j:=3; k:=1; end;
    2: begin i:=3; j:=1; k:=2; end;
    3: begin i:=1; j:=2; k:=3; end;
   end;
   color := GetColor;
   HideMouse; ClearViewPort; ShowMouse;
   matrix[1].GetSize(MaxRow,MaxCol);
   with views[viewPortNumber] do
   begin
      disX := (vx2-vx1) / (maxCol-1);    {horiz. grid spacing}
      disY := (vy2-vy1) / (maxRow-1);    {vert.  grid spacing}
   end;
   maxE  := 0;
   maxEz := 0;
   minEz := 1e20;
   FOR c := 2 TO maxCol-1 DO             {find max E on the grid}
    FOR r := 2 TO maxRow-1 DO
    begin
       pos := (r-1)*maxCol+c;
       dE  := sqr(Matrix[i].value(r,c)) +sqr(Matrix[j].value(r,c));
       E[k]:= Matrix[k].value(r,c);
       if   dE>maxE  then maxE :=dE;
       if E[3]>maxEz then maxEz:=E[k];
       if E[3]<minEz then minEz:=E[k];
    END;
   dEz := MaxEz-MinEz;
   maxE:=2*sqrt(maxE);
   HideCursor;
   if maxE<>0 then
   FOR c := 2 TO maxCol-1 DO
    FOR r := 2 TO maxRow-1 DO
     begin
        for m := 1 to 3 do E[m] := Matrix[m].value(r,c);
        if dEz<1E-8 then SetColor(yellow)
        else SetColor(rainbow[1].colors[trunc(11*(E[k]-minEz)/dEz)+1]);
        vector(round(disX*(c-1-E[i]/maxE)), round(disY*(r-1+E[j]/maxE)),
               round(disX*(c-1+E[i]/maxE)), round(disY*(r-1-E[j]/maxE)));
     END;
   SetColor(white);
   with views[viewPortNumber] do Rectangle(0,0,vx2-vx1,vy2-vy1);
   ShowCursor;
end;

{------------------------------ General --------------------------------}

FUNCTION PtInsideView (x,y,r : integer) : boolean;
var d : integer;
BEGIN
   d := round(0.025*GetMaxX);     {button size}
   with views[r] do
   PtInsideView := (x>vx1) and (x<vx2) and (y>vy1) and (y<vy2) and
                  ((x>vx1+d) or (y>vy1+d));
END;

FUNCTION PtBetween (p:point; r,q:point) : boolean;
{checks if point 'p' is inside a rectangle created from points 'r' and 'q'.}
BEGIN
   PtBetween := ( (abs(r.x-q.x)>=abs(2*p.x-r.x-q.x)) AND
                (  abs(r.y-q.y)>=abs(2*p.y-r.y-q.y)) );
END;

procedure SetPoint(var p : point; x,y : real);
begin
   p.x := x; p.y := y;
end;

procedure SetPoint1(var p : pointType; x,y : integer);
begin
   p.x := x; p.y := y;
end;

function SamePoint(a,b : point) : boolean;
begin
   SamePoint := (abs(b.x-a.x)<1e-5) and (abs(b.y-a.y)<1e-5);
end;

procedure SetPoint3D(var p : Point3D; x,y,z : real);
begin
   p[1] := x; p[2] := y; p[3] := z;
end;

function valX(x : integer) : real;
begin
   WITH Views[viewPortNumber] DO WITH GraphScales[ScaleNumber] DO
    valX := ((xmax-xmin) / (Vx2-Vx1) * x) + xmin;
end;

function valY(y : integer) : real;
begin
   WITH Views[viewPortNumber] DO WITH GraphScales[ScaleNumber] DO
    valY :=-((ymax-ymin) / (Vy2-Vy1) * y) + ymax;
end;

function Interpolate(v1,v2,v3,v4,x,y : real) : real;
begin
   if x+y<1 then
      Interpolate := (v2-v1)*x +(v3-v1)*y +v1
   else
      Interpolate := (v3-v4)*(1-x) +(v2-v4)*(1-y) +v4;
end;

function Interpolate1 (y0,y1,y2,y3,x : real) : real;
{ interpolate using qubic aproximation.         }
{ Input: 4 values of y for 4 points on the stright line, x which is   }
{ distance from second point (so in the middle section).              }
var a,b,c : real;
begin
   a := (y3-3*y2+3*y1-y0)/6;
   b := (y2-2*y1+y0)/2;
   c := y1-y0 -a +b;
   Interpolate1 := ((a*x +b)*x +c)*x + y1;
end;

PROCEDURE PlotAreaUnderCurve(x, y : DVector; color : integer);
VAR
   n,i  : Integer;
   poly : array[1..4] of PointType;
   min,max,Tick : ARRAY[1..2] OF real;
BEGIN
{   autoScaling(0,x,y); }
   x.MinMax(n,min[1],n,max[1]);
   y.MinMax(n,min[2],n,max[2]);
   if min[2]>0 then min[2] := 0;
   if max[2]<0 then max[2] := 0;
   FOR n := 1 TO 2 DO
   BEGIN
      max[n] := max[n] + (max[n] - min[n])/10.0;
      min[n] := min[n] - (max[n] - min[n])/10.0;
      if min[n]=max[n] then
      begin
         max[n] := max[n] + 1;
         min[n] := min[n] - 1;
      end;
      Tick[n] := TickSpace ((max[n]-min[n]));
   END;
   DefineScale(10,min[1],max[1],min[2],max[2]);
   SetColor(color);
   SetFillStyle(SolidFill,color);
   map( x.vecPtr^[1], y.vecPtr^[1], poly[2].x, poly[2].y);
   poly[3].x := poly[2].x;
   poly[3].y := MapY(0);
   HideMouse;
   FOR i := 2 TO x.size DO
   BEGIN
      poly[1] := poly[2];
      poly[4] := poly[3];
      map( x.value(i), y.value(i), poly[2].x, poly[2].y);
      poly[3].x := poly[2].x;
      FillPoly(4,poly)
   END;
   ShowMouse;
   Axis(0,0,Tick[1],Tick[2]);
END;

function GeneralSimpson(x,y : DVector; min,max : integer) : real;
var
   sum,x1,x2,y1,y2,yAv : real;
   i : integer;
begin
   sum := 0;
   i   := min;
   while (i+2<=max) do
   begin
      x1  :=  x.VecPtr^[i+1] - x.VecPtr^[i];
      x2  :=  x.VecPtr^[i+2] - x.VecPtr^[i];
      y1  :=  y.VecPtr^[i+1] - y.VecPtr^[i];
      y2  :=  y.VecPtr^[i+2] - y.VecPtr^[i];
      yAv := (y.VecPtr^[i+2] + y.VecPtr^[i])/2;
      Sum := sum + yAv*x2 - x2*x2*x2/6 * (y2/x2 - y1/x1) / (x2-x1);
      i := i + 2;
   end;
   if (i=max-1) then
   begin
      x1  :=  x.VecPtr^[i+1] - x.VecPtr^[i];
      yAv := (y.VecPtr^[i+1] + y.VecPtr^[i])/2;
      sum := sum + yAv*x1;
   end;
   GeneralSimpson := sum;
end;

PROCEDURE DrawRubberLine(figure:Figure; probe : ProbeType;
                         VAR a,b,c: PointType; var button : integer);
VAR
   x,y,color: integer;
   vp : View;
   down1,down2 : boolean;
BEGIN
   vp := views[ViewPortNumber];
   IF NOT mousePresent THEN exit;
   b := a;
   HideMouse;
   SetWriteMode(XORput);
   figure(a,b,c);
   ShowMouse;
   down1 := false;
   down2 := false;
   REPEAT
      HideMouse;
      figure(a,b,c);        {erase}
      MouseGlobalPosn(x,y,button);
      if (button=1) then down1:=true;
      if (button=2) then down2:=true;
      b.x := x-vp.vx1; b.y := y-vp.vy1;
      figure(a,b,c);        {draw}
      ShowMouse;
      probe;
      delay(5)
   UNTIL (down1 or down2) and (button=0);
   if down2 then button := 2;
   HideMouse;
   figure(a,b,c);
   ShowMouse;
   SetWriteMode(COPYput);
end;

{------------------------------ TIntegral --------------------------------}

procedure OpenPoly (a,b,c : pointType); far;
begin
   line(a.x,a.y,b.x,b.y);
end;

procedure ClosedPoly (a,b,c : pointType); far;
begin
   line(a.x,a.y,b.x,b.y);
   line(c.x,c.y,b.x,b.y);
end;

procedure TIntegral.Init;
begin
   active := false;
   OpenPath := true;
   x.size := 0;
   y.size := 0;
   DefineViewPort(5, 0.04, 0.48, 0.11, 0.34);
   buttons.init;
   buttons.create(1,0.04,0.4,'Open Path');
   buttons.create(2,0.24,0.4,'Closed Path');
end;

procedure TIntegral.Done;
begin
   if active then buttons.done;
   clean;
end;

procedure TIntegral.Clean;
begin
   if active then
   begin
      OpenViewPort(3);
      if x.size>0 then x.free;
      if y.size>0 then y.free;
      active := false;
   end;
end;

procedure TIntegral.DrawPath( viewport : integer; dis : point);
var i,color : integer;
begin
   if (x.size>0) and (viewport=OldPort) then
   begin
      color := GetColor;
      SelectViewPort(OldPort);
      SetWriteMode(XORput);
      HideMouse;
      for i := 1 to (x.size-1) do
      begin
         Line( round(dis.x*(x.value(i)-1)),   round(dis.y*(y.value(i)-1)),
               round(dis.x*(x.value(i+1)-1)), round(dis.y*(y.value(i+1)-1)));
{        PutPixel( round(dis.x*(x.value(i)-1)), round(dis.y*(y.value(i)-1)),
                   color);}
      end;
      ShowMouse;
      SetWriteMode(COPYput);
   end;
end;

procedure TIntegral.ReadPath(probe : ProbeType; dis : point);
VAR
   button,i,num : integer;
   a,b,c,h : pointType;
BEGIN
   HideMouse;
   if x.size>0 then
   begin
      DrawPath(OldPort,dis);
      x.free;
      y.free;
   end;
   ShowMouse;
   OldPort := Port;
   x.init(100);
   y.init(100);
   num := 1;
   SelectViewPort(port);
   SetPoint1(h, round(2*dis.x), round(2*dis.y) );
   with views[port] do
    mouseBorders( vx1+h.x, vy1+h.y, vx2-h.x, vy2-h.y);
   a.x := round((event.x-views[port].vx1)/ dis.x);
   a.y := round((event.y-views[port].vy1)/ dis.y);
   x.put(num, a.x+1);
   y.put(num, a.y+1);
   a.x := round(a.x * dis.x);
   a.y := round(a.y * dis.y);
   c := a;
   REPEAT
      if OpenPath or (num=1) then DrawRubberLine(OpenPoly,probe,a,b,c,button)
      else DrawRubberLine(ClosedPoly,probe,a,b,c,button);
      inc(num);
      b.x := round(round(b.x / dis.x) * dis.x);
      b.y := round(round(b.y / dis.y) * dis.y);
      HideMouse;
      SetWriteMode(XORput);
      Line(a.x,a.y,b.x,b.y);
      SetWriteMode(COPYput);
      ShowMouse;
      x.put(num, round(b.x/dis.x)+1);
      y.put(num, round(b.y/dis.y)+1);
      a := b;
   UNTIL (button=2);
   if not OpenPath then
   begin
      inc(num);
      HideMouse;
      line(a.x,a.y,c.x,c.y);
      ShowMouse;
      x.put(num, x.value(1));
      y.put(num, y.value(1));
   end;
   mouseBorders(0,0,GetMaxX,GetMaxY);
   ClearMKBuffers;
   x.resize(num);
   y.resize(num);
END;


procedure TIntegral.info(str : string);
var
   vp,i : integer;
begin
   vp := viewPortNumber;
   SelectViewPort(3);
   for i := length(str)+1 to 36 do str := concat(str,' ');
   comment(0.005,0.48,str);
   SelectViewPort(vp);
end;


procedure TIntegral.Prepare;
var i : integer;
begin
   active := true;
   OpenViewPort(3);
   Info('Mouse Buttons: L mark corners; R exit');
   GraphBackColor := black;
   OpenViewPort(5);
   GraphBackColor := DarkGray;
   if OpenPath then buttons.number:=1 else buttons.number:=2;
   buttons.DrawAll;
end;

function TIntegral.Check : boolean;
var i,button : integer;
begin
   if active then
   begin
      if buttons.changed then OpenPath := (buttons.number=1);
      port := 0;
      if event.mouseClicked then
       for i := 1 to 4 do
        if PtInsideView(event.x,event.y,i+15) then Port := i+15;
      check := (port<>0) and (port<>3+15);
   end else check := false;
end;


procedure TIntegral.MapOnGridX(a,b : pointType; var d1,v1 : DVector;
                               PathVec : point3D);

   function InterpolateX(var field : DMatrix; var r,c : integer; var row : real) : real;
   begin
      InterpolateX := interpolate1( field.value(r-1,c), field.value(r,c),
                       field.value(r+1,c), field.value(r+2,c), frac(row) );
   end;

var
   i,r,c,step : integer;
   row,ratio  : real;
begin
   c := a.x;
   i := 1;
   if (a.x< b.x) then step:=1 else step:=-1;
   if (a.x<>b.x) then ratio := (a.y-b.y)/(a.x-b.x);
   while (c<>b.x) do   {calculate intersections on horiz. grid lines}
   begin
      row := (c-a.x)*ratio+a.y;
      r := trunc(row);
      d1.put( i, sqrt(sqr(ScaleVec.x*(c-a.x))+sqr(ScaleVec.y*(row-a.y))) );
      if scalar then v1.put( i, interpolateX( sfield   ,r,c,row))
      else v1.put( i, PathVec[1]*interpolateX( vfield[1],r,c,row)
                     +PathVec[2]*interpolateX( vfield[2],r,c,row)
                     +PathVec[3]*interpolateX( vfield[3],r,c,row));
      c := c+step;
      inc(i);
   end;
   d1.put( i, sqrt(sqr(ScaleVec.x*(a.x-b.x))+sqr(ScaleVec.y*(a.y-b.y))) );
   if scalar then v1.put( i, sfield.value(b.y,b.x))
   else v1.put( i, PathVec[1]*vfield[1].value(b.y,b.x)
                  +PathVec[2]*vfield[2].value(b.y,b.x)
                  +PathVec[3]*vfield[3].value(b.y,b.x));
end;

procedure TIntegral.MapOnGridY(a,b : pointType; var d2,v2 : DVector;
                               PathVec : point3D);

   function InterpolateY(var field : DMatrix; var r,c : integer; var col : real) : real;
   begin
      InterpolateY := interpolate1( field.value(r,c-1), field.value(r,c),
                      field.value(r,c+1), field.value(r,c+2), frac(col) );
   end;
var
   i,c,r,step : integer;
   col,ratio  : real;
begin
   r := a.y;
   i := 1;
   if (a.y< b.y) then step:=1 else step:=-1;
   if (a.y<>b.y) then ratio := (a.x-b.x)/(a.y-b.y);
   while (r<>b.y) do   {calculate intersections on vert. grid lines}
   begin
      if (a.x=b.x) then col:=a.x else col:=(r-a.y)*ratio+a.x;
      c := trunc(col);
      d2.put( i, sqrt(sqr(ScaleVec.x*(col-a.x))+sqr(ScaleVec.y*(r-a.y))) );
      if scalar then v2.put( i, interpolateY( sfield,r,c,col))
      else v2.put( i, PathVec[1]*interpolateY( vfield[1],r,c,col)
                     +PathVec[2]*interpolateY( vfield[2],r,c,col)
                     +PathVec[3]*interpolateY( vfield[3],r,c,col));
      r := r + step;
      inc(i);
   end;
   d2.put( i, sqrt(sqr(ScaleVec.x*(a.x-b.x))+sqr(ScaleVec.y*(a.y-b.y))) );
   if scalar then v2.put( i, sfield.value(b.y,b.x))
   else v2.put( i, PathVec[1]*vfield[1].value(b.y,b.x)
                  +PathVec[2]*vfield[2].value(b.y,b.x)
                  +PathVec[3]*vfield[3].value(b.y,b.x));
end;

procedure TIntegral.MergeDVectors(var d1,v1,d2,v2 : DVector; distance : real);
var
   i,j  : integer;
   dist : real;
begin
   i := 1;
   j := 1;
   if d.size=0 then dist:=0 else dist:=d.value(d.size);
   distance := distance+dist;
   repeat                              {merge d1 & d2 into d}
      inc(d.size);
      inc(v.size);
      if d1.value(i)>d2.value(j) then
      begin
         d.put( d.size, d2.value(j)+dist );
         v.put( v.size, v2.value(j) );
         inc(j);
      end
      else
      begin
         d.put( d.size, d1.value(i)+dist );
         v.put( v.size, v1.value(i) );
         if (d1.value(i)=d2.value(j)) then inc(j);
         inc(i);
      end;
   until d.value(d.size)>=distance;
end;

procedure TIntegral.CalculateField(n : integer);
var
   a,b,displacement   : PointType;
   distance,sign      : real;
   d1,d2,v1,v2        : Dvector;
   PathVec            : point3D;
   min : integer;
begin
   SetPoint1( a, x.value(n),   y.value(n));
   SetPoint1( b, x.value(n+1), y.value(n+1));
   SetPoint1( displacement, abs(a.x-b.x)+1, abs(a.y-b.y)+1);
   distance := sqrt(sqr(ScaleVec.x*(a.x-b.x))+sqr((ScaleVec.y*(a.y-b.y))));
   if distance<>0 then
   begin
      case PlaneNorm of
       1: SetPoint3D( PathVec, 0, ScaleVec.x*(b.x-a.x)/distance, ScaleVec.y*(a.y-b.y)/distance);
       2: SetPoint3D( PathVec, ScaleVec.x*(b.x-a.x)/distance, 0, ScaleVec.y*(a.y-b.y)/distance);
       3: SetPoint3D( PathVec, ScaleVec.x*(b.x-a.x)/distance, ScaleVec.y*(a.y-b.y)/distance, 0);
      end;
      d1.init(displacement.x);
      v1.init(displacement.x);
      d2.init(displacement.y);
      v2.init(displacement.y);
      MapOnGridX(a,b,d1,v1,PathVec);
      MapOnGridY(a,b,d2,v2,PathVec);
      if (scalar and (((PlaneNorm= 1) and (PathVec[2]<0)) or
                      ((PlaneNorm<>1) and (PathVec[1]<0)))) then
      begin
         v1.MultScalar(-1);
         v2.MultScalar(-1);
      end;
      MergeDVectors(d1,v1,d2,v2,distance);
      d1.free;
      v1.free;
      d2.free;
      v2.free;
   end;
end;

procedure TIntegral.Integrate(name : string);
var
   i,min,max : integer;
   Integral,aa  : real;
   f : text;
begin
   Prepare;
   d.init(1000);
   v.init(1000);
   d.size   := 0;
   v.size   := 0;
   Integral := 0;
   for i := 1 to x.size-1 do
   begin
      min := d.size+1;
      CalculateField(i);
      Max := d.size;
      Integral := Integral + GeneralSimpson(d,v,min,max);
   end;
   SelectScale(0);
   PlotAreaUnderCurve(d,v,lightRed);
   SelectMuppetPort;
   SetColor(yellow);
   RuboutXY(0.03,0.36,20,DarkGray);
   PrintXY (0.03,0.36,concat(name,' Path Integral = ',NumStr(Integral,5,3)));
   d.free;
   v.free;
end;

procedure TIntegral.ScalarField(field : DMatrix; ScaleVec_: point; PlaneNorm_: integer; name : string);
begin
   sField    := field;
   scalar    := true;
   ScaleVec  := ScaleVec_;
   PlaneNorm := PlaneNorm_;
   Integrate(name);
end;

procedure TIntegral.VectorField(field : VMatrix; ScaleVec_: point; PlaneNorm_: integer; name : string);
begin
   vField    := field;
   scalar    := false;
   ScaleVec  := ScaleVec_;
   PlaneNorm := PlaneNorm_;
   Integrate(name);
end;

end.
