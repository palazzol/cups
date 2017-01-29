                   {****************************************}
                   {****************************************}
                   {***           Unit PoissonT          ***}
                   {***  Part of the Program Poisson.Pas ***}
                   {***    Written by Jarek Tuszynski    ***}
                   {***          June 3, 1991            ***}
                   {****************************************}
                   {****************************************}

unit PoissonT;
{Poisson_Tools unit contains various independent and selfcontaining }
{objects which were used in Poisson program.}
{$R-}

interface
USES Graph, Crt, CupsMupp, CUPS, CupsGrph, CupsFunc, CupsProc,
     CupsGui, CupsPars, PoissonP;
Type
   DrawProcedure = procedure(var potential : DMatrix);

   Bbase = ARRAY[1..1] OF Boolean;
   BbasePtr =^Bbase;
   BbaseHandle =^BbasePtr;
   BoolMatrix = OBJECT
     BoolPtr : BbasePtr;
     NRows, NCols : INTEGER;                    {dimensions of BoolMatrix}
     MemBlock: LongInt;
     PROCEDURE Init(NumRows,NumCols: INTEGER);
     PROCEDURE Free;
     PROCEDURE Put(Row,Col: INTEGER; X: boolean);
     FUNCTION  Value(Row,Col: INTEGER): boolean;
     PROCEDURE Fill(FillValue: Boolean);
   end;

   TRelaxation = object
      procedure Init;
      procedure Draw1(var potential,charges : DMatrix; var mask : BoolMatrix);
      procedure Draw (var potential,charges : DMatrix; var mask : BoolMatrix);
      procedure ReDraw(var potential : DMatrix);
      procedure ReadParameters;
   private
      Omega,Tolerance : real;
      dIter,MaxI,MinI : integer;
      converged       : boolean;
      contour         : TContour;
      legend          : boolean;
   end;

   TPlot3DSurface = object
      active : boolean;
      procedure Init;
      procedure Draw(var matrix_ : DMatrix; label1 : string);
      procedure SetAnglesAndDraw;
      procedure HandleSliders;
      procedure done;
   private
      alpha,beta,gamma  : real;
      surface,smallBox :TGraph3D;
      contour : TContour;
      sliders : TSliders;
      matrix  : DMatrix;
      label_   : string;
      procedure initialize;
   end;

   T4Graphs = object
      active : boolean;
      procedure init( var V,Q,E : DMatrix; mask : boolMatrix; DFnc : point );
      procedure check;
      procedure done;
   private
      plot : array[1..3] of TPlot3D;
      potential, charge, field : dMatrix;
   end;

   ParserData = record
      FuncStr : string;
      MaxN    : byte;
      NOdd    : byte;
   end;

   TUserFunction = object
      procedure init;
      function Process(RectSys : boolean; var matrix : DMatrix;
                       var h : point; var Scale : GraphScale) : boolean;
      function CreateMatrix(var matrix : DMatrix) : boolean;
      procedure done;
   private
      rect, sphere : ParserData;
      RectSystem   : boolean;
      radius  : real;
      Min,Max : Point2D;
      parser  : TParser;
      function ReadRect : boolean;
      function ReadSphere  : boolean;
   end;

   TIcons = object
      modify : boolean;
      choice : ShapeType;
      procedure Draw;
      procedure Update;
      function  Clicked : ShapeType;
   private
      size,d : integer;
   end;

procedure DrawCrossSection (var potential: DMatrix; h : point; scale: GraphScale);
procedure DrawFieldLines (Redraw : DrawProcedure; var potential,
                          charges : DMatrix; var mask: BoolMatrix);
procedure DrawFieldVectors (var potential : DMatrix; var mask : BoolMatrix);
procedure PotentialAroundCharges(var potential,charges : DMatrix;
                                 qRadius : real);
procedure FindDivergence(VAR V,D,temp : DMatrix; h : point);
procedure FindField(VAR V,E : DMatrix; h : point);

Function  ReadFileName : string;
Procedure Ask(question : string; var answer : string);
procedure CleanViewPort(num : integer);




Implementation

procedure swap (var x,y : real);
var temp : real;
begin
   temp := x;
   x    := y;
   y    := temp;
end;

procedure swapPoints (var x,y : pointType);
var temp : PointType;
begin
   temp := x;
   x    := y;
   y    := temp;
end;

procedure CleanViewPort(num : integer);
begin
   SelectViewPort(num);
   HideMouse;
   ClearViewPort;
   ShowMouse;
end;       


{--------------------------- TPlot3DSurface --------------------------------------}

procedure TPlot3DSurface.init;
begin
   active := false;
   alpha  := 330;
   beta   := 20;
   gamma  := 0;
{MAC   new(contour);}
{MAC   new(sliders);}
{MAC   new(matrix); }
end;

procedure TPlot3DSurface.initialize;
begin
   active := true;

   DefineViewPort (4, 0.0,  1.0, 0.05, 0.92);     {cleans area of the screen}
   CloseViewPort(4);
   DefineViewPort (4, 0.0,  0.65, 0.08, 0.92);    {left;         for surface}
   DefineViewPort (5, 0.72, 1.00, 0.08, 0.43);    {right-bottom; for box}
   DefineViewPort (6, 0.68, 1.00, 0.63, 0.92);    {right-top;    for contour}

   OpenViewPort (4);
   surface.init;
   surface.SetBoxRatios(1,0.7,2/3);
   surface.DrawBox;

   SelectMuppetPort;
   RubOut(55,10,40,blakk);
   Print(55,10,concat('Contour Map of ',label_));

   OpenViewPort (5);
   SmallBox.Init;
   smallBox.arrows := true;
   smallBox.boxColor := whitte xor darkGray;
   smallBox.SetBoxRatios(1,0.7,2/3);
   smallBox.SetViewAngles(alpha,beta,gamma);
   SetWriteMode (XorPut);
   smallBox.DrawBox;
   SetWriteMode (CopyPut);

   sliders.init;
   sliders.create(1,  0 ,360,alpha, 0.68,0.48,0.99,0, '0','360','alpha',false);
   sliders.create(2,-180,180, beta, 0.68,0.08,0.43,0, '', '',   'beta' ,true );
   sliders.drawAll;
   SetAnglesAndDraw;

   SelectViewPort(6);
   contour.init;
   contour.DrawMatrix(matrix);
end;

PROCEDURE TPlot3DSurface.SetAnglesAndDraw;
BEGIN
   OpenViewPort(4);
   Print(1,1,concat(' Plot of ',label_,' as function of x and y'));
   surface.SetViewAngles(alpha,beta,gamma);
   surface.DrawSurface(matrix);
END;


PROCEDURE TPlot3DSurface.Draw(var matrix_ : DMatrix; label1 : string);
BEGIN
   label_ := label1;
   matrix := matrix_;
   if not active then initialize
   else
   begin
      OpenViewPort(4);
      Print(1,1,concat(' Plot of ',label_,' as function of x and y'));
      surface.DrawSurface(matrix);
      SelectMuppetPort;
      RubOut(55,10,40,blakk);
      Print(55,10,concat('Contour Map of ',label_));
      SelectViewPort(6);
      contour.DrawMatrix(matrix);
   end;
END;

procedure TPlot3DSurface.HandleSliders;
begin
   if sliders.changed then
   begin
      SelectViewPort(5);
      SetWriteMode (XorPut);
      SmallBox.DrawBox;
      alpha := sliders.value(1);
      beta  := sliders.value(2);
      smallBox.SetViewAngles(alpha,beta,gamma);
      smallBox.DrawBox;
      SetWriteMode (CopyPut);
      Delay(20);
      SelectViewPort(4);
   end;
   if event.doubleclicked then SetAnglesAndDraw;
end;

procedure TPlot3DSurface.done;
BEGIN
   if active then
   begin
      DefineViewPort (4, 0.0,  1.0, 0.05, 0.92);
      CloseViewPort(4);
      Sliders.done;
      active := false;
{MAC  dispose(contour);}
{MAC  dispose(sliders);}
{MAC  dispose(matrix); }
   end;
END;

{---------------------------- T4Graphs --------------------------------}

procedure T4Graphs.Init( var V,Q,E : DMatrix; mask : BoolMatrix; DFnc : point );
var i,vp : integer;
begin
   vp := ViewPortNumber;
   active := true;
   potential := V;
   charge := Q;
   Field := E;
{   with potential do charge.Init(NRows,NCols);
   with potential do  field.Init(NRows,NCols);}
   FindDivergence(potential,Charge,Field,DFnc);
   FindField(potential,Field,DFnc);
   DefineViewPort (4, 0.0,  1.0, 0.05, 0.92);     {cleans area of the screen}
   CloseViewPort(4);
   DefineViewPort(4, 0.00, 0.49, 0.51, 0.93);
   DefineViewPort(5, 0.51, 1.00, 0.51, 0.93);
   DefineViewPort(6, 0.00, 0.49, 0.07, 0.49);
   DefineViewPort(7, 0.51, 1.00, 0.07, 0.49);
   for i := 1 to 3 do plot[i].init(3+i);
   for i := 1 to 3 do plot[i].SetBoxRatios(1,0.7,2/3);
   plot[1].PlotMatrix(potential,'Potential');
   plot[2].PlotMatrix(charge,'Charge Distribution');
   plot[3].PlotMatrix(field,'Field Magnitude');
   SelectViewPort(7);
   DrawFieldVectors(potential, mask);
   PutLabel( inside, 'Electric Field');
   SelectViewPort(vp);
end;

procedure T4Graphs.done;
var i,vp : integer;
begin
   if active then
   begin
      vp := viewPortNumber;
      for i := 1 to 3 do plot[i].done;
{     charge.free;
      field.free;  }
      DefineViewPort (4, 0.0,  1.0, 0.05, 0.95);     {cleans area of the screen}
      CloseViewPort(4);
      SelectViewPort(vp);
   end;
   active := false;
end;

procedure T4Graphs.check;
var i : integer;
begin
   for i := 1 to 3 do plot[i].check;
end;

{---------------------------- Relaxation -----------------------------------}
Type
  TMessage = object
       procedure Init;
       PROCEDURE Show;
       PROCEDURE Update(iteration_: integer; Error_: real);
   PRIVATE
       i,e : PointType;
       error : real;
       Iteration : integer;
   END;

PROCEDURE TMessage.Init;
begin
   error := 0;
   Iteration := 0;
end;

PROCEDURE TMessage.Show;
VAR
   r     : rectType;
   text  : string;
   n     : integer;
   envir : TEnvironment;
BEGIN
{MAC new(envir);}
   envir.Save;
   envir.Standardize;
   SetFillStyle(SolidFill,blue);
   text := 'Iteration #    , Sigma       ';
   i.x := TextWidth(text)+13;
   i.y := 2*RowHt + 3;
   r.x1 := (GetMaxX - i.x) DIV 2;
   r.y1 := (GetMaxY - i.y) DIV 2;
   r.x2 := (GetMaxX + i.x) DIV 2;
   r.y2 := (GetMaxY + i.y) DIV 2;
   HideMouse;
   Bar3D(r.x1,r.y1+3,r.x2-3,r.y2,3,true);
   OutTextXY(r.x1+6,r.y1+8,Text);
   OutTextXY(r.x1+6,r.y2-RowHt+4,'   Press any key to Cancel');
   i.x := r.x1+6+11*ColWidth;
   i.y := r.y1+8;
   e.x := r.x1+6+23*ColWidth;
   e.y := r.y1+8;
   OutTextXY( i.x, i.y, NumStr(iteration,4,0) );
   OutTextXY( e.x, e.y, Num2Str(error,6) );
   ShowMouse;
   envir.Reset;
{MAC dispose(envir);}
END;

PROCEDURE TMessage.Update(iteration_: integer; Error_: real);
VAR
   envir : TEnvironment;
BEGIN
{MAC new(envir);}
   Iteration := Iteration_;
   Error     := Error_;
   envir.Save;
   envir.Standardize;
   SetColor(Blue);
   SetFillStyle(SolidFill,blue);
   HideMouse;
   bar( i.x, i.y, i.x+4*ColWidth, i.y+8);
   bar( e.x, e.y, e.x+6*ColWidth, e.y+8);
   SetColor(whitte);
   OutTextXY( i.x, i.y, NumStr(iteration,4,0) );
   OutTextXY( e.x, e.y, Num2Str(error,6) );
   ShowMouse;
   envir.Reset;
{MAC dispose(envir);}
END;

Procedure TRelaxation.init;
begin
{MAC new(contour);}
   dIter     := 500;
   MinI      := 40;
   MaxI      := 500;
   Tolerance := 0.3;
   omega     := 1.5;
   legend    := false;
   contour.init;
end;

PROCEDURE TRelaxation.ReadParameters;
VAR
   RScreen  : TInputScreen;
   MRow,MCol,MaxRow,MaxCol : integer;
   numLines : word;
   n : byte;
   s : string;
   dummy : HeightType;
BEGIN
{MAC new(RScreen);}
   Contour.GetHeights(numLines,dummy);
   with RScreen do
   begin
      init;
      DefineInputPort (0.16, 0.85, 0.23, 0.77);
      LoadLine('              Relaxation Parameters');
      LoadLine('');
      LoadLine(' Omega (Relaxation Parameter)  = {      }  0  to 2');
      LoadLine(' Tolerance                     = {      }  0  to 1');
      LoadLine(' Minimum Number of Iterations  = {      }  10 to 1000');
      LoadLine(' Maximum Number of Iterations  = {      }  10 to 1000');
      LoadLine(' Number of iterations per plot = {      }  1  to 1000');
      LoadLine(' Number of contour lines       = {      }  1  to 14');
      LoadLine('  #F Show color key');
      LoadLine('');
      LoadLine('               [  Ok  ]   [Cancel]');
      SetNumber (1,omega);
      SetNumber (2,tolerance);
      SetNumber (3,MinI);
      SetNumber (4,MaxI);
      SetNumber (5,dIter);
      SetNumber (6,numLines);
      SetBoolean(7,legend);
      SetNumberLimits (1,0,2);
      SetNumberLimits (2,0,1);
      SetNumberLimits (3,10,1000);
      SetNumberLimits (4,10,1000);
      SetNumberLimits (5,1,1000);
      SetNumberLimits (6,1,14);
      repeat
         AcceptScreen;
         MinI   := round(GetNumber(3));
         MaxI   := round(GetNumber(4));
         if (MinI>MaxI) then
          announce('Minimum can''t be bigger then Maximum Number of Iterations');
      until (MinI<=MaxI);
      omega     := GetNumber(1);
      tolerance := GetNumber(2);
      dIter     := round(GetNumber(5));
      numLines  := round(GetNumber(6));
      legend    := Getboolean(7);
      done;
   end;
   Contour.SetHeights(numLines,dummy);
   Contour.AutoSetting := true;
{MAC dispose(RScreen);}
END;


procedure TRelaxation.Draw1(var potential,charges : DMatrix; var mask : BoolMatrix);
{Potential matrix is relaxed. Contour plot is drawn in current view-port.}
VAR
   i,j,numIter,step : integer;
   error, Vmin, Vmax, Qmin, Qmax, range : real;
   quit : boolean;
   wait : TMessage;

   procedure Iteration(direction,step : integer; omega : real);
   var
      sum,resv : real;
      i,j,MaxCol,MaxRow,pos,NumPoints : integer;
   begin
      inc(numIter);
      sum := 0;
      potential.GetSize(MaxRow,MaxCol);
      NumPoints := MaxCol*MaxRow+1;
      with potential do
      FOR i := 1+step TO maxCol-step DO
       FOR j := 1+step TO maxRow-step DO
        BEGIN
          case direction of
           0 : pos := (j-1)*maxCol+i;
           1 : pos := NumPoints - (j-1)*maxCol-i;
           2 : pos := j*maxCol-i+1;
           3 : pos := NumPoints - j*maxCol+i-1;
          end;
          if not mask.BoolPtr^[pos] then
          begin      {this is Poisson's equation}
             resv := (MatPtr^[pos-step*maxCol] +MatPtr^[pos-step]
                     +MatPtr^[pos+step*maxCol] +MatPtr^[pos+step])/4
                     -MatPtr^[pos] +charges.MatPtr^[pos]/4;
             MatPtr^[pos] := MatPtr^[pos] + omega*resv;
             sum := sum + sqr(resv);
          end;
       END;
      error := sqrt(sum)/range;
      wait.update(numIter,error);
   end;

begin
{MAC new(wait);}
   ClearMKBuffers;
   charges.MinMax(i,i,Qmin,i,i,Qmax);
   potential.MinMax(i,i,Vmin,i,i,Vmax);
   if Vmin>Qmin then Vmin:=Qmin;
   if Vmax<Qmax then Vmax:=Qmax;
   range := Vmax-Vmin;
   NumIter := 0;
   step := 3;
   wait.init;
   wait.Show;
   REPEAT
      if (numIter>3*MinI div 4) then step := 1 else
      if (numIter>MinI div 2) then step := 2;
      iteration(numIter mod 4,numIter mod step +1,omega);
      quit := ((NumIter>MinI-5) and (error<Tolerance)) OR (NumIter>MaxI-5)
               OR keyPressed OR MouseClicked(i,j);
      if (numIter MOD dIter = 0) then
       begin contour.DrawMatrix(potential); wait.Show; end;
   UNTIL quit;
   for i := 1 to 5 do iteration(numIter mod 4,1,1);
   contour.DrawMatrix(potential);
   if legend then contour.PrintLegend(7);
   ClearMKBuffers;
{MAC dispose(wait);}
end;

procedure TRelaxation.Draw(var potential,charges : DMatrix; var mask : BoolMatrix);
{Potential matrix is relaxed. Contour plot is drawn in current view-port.}
VAR
   i,j,numIter,step : integer;
   error, Vmin, Vmax, Qmin, Qmax, range : real;
   quit : boolean;
   wait : TMessage;

   function GetVAvr(p : integer) : real;
   begin
      with potential do
      GetVAvr := (MatPtr^[p]       +MatPtr^[p+1]
                 +MatPtr^[p+NCols] +MatPtr^[p+Ncols+1])/4;
   end;

   function GetQAvr(p : integer) : real;
   begin
      with charges do
      GetQAvr := (MatPtr^[p]       +MatPtr^[p+1]
                 +MatPtr^[p+NCols] +MatPtr^[p+Ncols+1])/4;
   end;

   procedure Iteration1(direction : integer; omega : real);
   var
      sum,resv : real;
      i,j,MaxCol,MaxRow,pos,NumPoints : integer;
   begin
      inc(numIter);
      sum := 0;
      potential.GetSize(MaxRow,MaxCol);
      NumPoints := MaxCol*MaxRow+1;
      with potential do
      FOR i := 1 TO maxCol DO
       FOR j := 1 TO maxRow DO
        BEGIN
          case direction of
           0 : pos := (j-1)*maxCol+i;
           1 : pos := NumPoints - (j-1)*maxCol-i;
           2 : pos := j*maxCol-i+1;
           3 : pos := NumPoints - j*maxCol+i-1;
          end;
          if not mask.BoolPtr^[pos] then
          begin      {this is Poisson's equation}
             resv := (MatPtr^[pos-maxCol] +MatPtr^[pos-1]
                     +MatPtr^[pos+maxCol] +MatPtr^[pos+1])/4
                     -MatPtr^[pos] +charges.MatPtr^[pos]/4;
             MatPtr^[pos] := MatPtr^[pos] + omega*resv;
             sum := sum + sqr(resv);
          end;
       END;
      error := sqrt(sum)/range;
      wait.update(numIter,error);
   end;

   procedure Iteration2(direction : integer; omega : real);
   var
      sum,resv : real;
      i,j,c,col,row,MaxCol,MaxRow,pos,NumPoints : integer;
   begin
      inc(numIter);
      sum := 0;
      potential.GetSize(MaxRow,MaxCol);
      NumPoints := MaxCol*MaxRow+1;
      c := MaxCol;
      with potential do
      FOR col := 2 TO maxCol div 2 -2 DO
       FOR row := 2 TO maxRow div 2 -2 DO
        BEGIN
          i := 2*col;
          j := 2*row;
          case direction of
           0 : pos := (j-1)*maxCol+i;
           1 : pos := NumPoints - (j-1)*maxCol-i;
           2 : pos := j*maxCol-i+1;
           3 : pos := NumPoints - j*maxCol+i-1;
          end;
          if not (mask.BoolPtr^[pos] or mask.BoolPtr^[pos+1] or
           mask.BoolPtr^[pos+maxCol] or mask.BoolPtr^[pos+maxCol+1]) then
          begin      {this is sort of Poisson's equation}
             resv := (GetVAvr(pos-2*maxCol) +GetVAvr(pos-2)
                     +GetVAvr(pos+2*maxCol) +GetVAvr(pos+2))/4
                     -GetVAvr(pos) +GetQAvr(pos)/4;
             MatPtr^[pos]     := MatPtr^[pos]     + omega*resv;
             MatPtr^[pos+1]   := MatPtr^[pos+1]   + omega*resv;
             MatPtr^[pos+c]   := MatPtr^[pos+c]   + omega*resv;
             MatPtr^[pos+c+1] := MatPtr^[pos+c+1] + omega*resv;
             sum := sum + sqr(resv);
          end;
       END;
      error := 4*sqrt(sum)/range;
      wait.update(numIter,error);
   end;

begin
{MAC new(wait);}
   ClearMKBuffers;
   charges.MinMax(i,i,Qmin,i,i,Qmax);
   potential.MinMax(i,i,Vmin,i,i,Vmax);
   if Vmin>Qmin then Vmin:=Qmin;
   if Vmax<Qmax then Vmax:=Qmax;
   range := Vmax-Vmin;
   NumIter := 0;
   step := 3;
   wait.init;
   wait.Show;
   REPEAT
      if (numIter>3*MinI div 4) then iteration1(numIter mod 4,omega) else
      iteration2(numIter mod 4,omega);
      quit := ((NumIter>MinI-5) and (error<Tolerance)) OR (NumIter>MaxI-5)
               OR keyPressed OR MouseClicked(i,j);
      if (numIter MOD dIter = 0) then
       begin contour.DrawMatrix(potential); wait.Show; end;
   UNTIL quit;
   for i := 1 to 5 do iteration1(numIter mod 4,1);
   contour.DrawMatrix(potential);
   if legend then contour.PrintLegend(7);
   ClearMKBuffers;
{MAC dispose(wait);}
end;

procedure TRelaxation.ReDraw(var potential : DMatrix);
begin
   contour.DrawMatrix(potential);
   if legend then contour.PrintLegend(7);
end;


{----------------------------- PotentialAroundCharges -------------------}

function ElectricField (v0,v1,v2,v3 : real) : real;
{ Find Electric Field between 4 points with known potential on the }
{ stright line.                                                    }
begin
   ElectricField := (v0-8*v1+8*v2-v3)/12;
end;


procedure PotentialAroundCharges(var potential,charges : DMatrix;
                                 qRadius : real);
{ This procedure takes matrix of charges and returns matrix of potential }
{ around those charges. Also contour plot is drawn in the current        }
{ view-port(stand-alone)                                                 }
type
   chargeType = record
      q : real;
      x,y : integer
   end;
var
   QList : array[1..500] of chargeType;
   i,j,k,n,MaxRow,MaxCol,pos : integer;
   v,r     : real;
   contour : TContour;
   wait    : TWaitMessage;
begin
{MAC new(wait);}
{MAC new(contour);}
   Contour.init;
   Wait.Show;
   potential.GetSize(MaxRow,MaxCol);         {get size of the matrix}
   n:=0;
   for i := 1 to MaxRow do                   {change matrix of charges...}
   begin                                     {... to array of charges}
     for j := 1 to MaxCol do
      if (charges.value(i,j)<>0) then
      begin
         inc(n);
         QList[n].q := charges.value(i,j);
         QList[n].x := j;
         QList[n].y := i;
      end;
     Wait.update;
   end;
   for i := 1 to MaxRow do
   begin
      for j := 1 to MaxCol do
      begin                                  {V is calculated directly from}
         V := 0;                             {version of V=Q/R equation.   }
         for k := 1 to n do
         with QList[k] do
         begin
             r := sqr(j-x)+sqr(i-y);         {square of radius}
             if (r<qRadius) then
              V := V -q*sqr(r/qRadius)/4 +q/4 -q*ln(qRadius)/2
             else V := V -q*ln(r)/2;         {v = v + c1*q*ln(c2/r) =}
         end;                                {  = v + c3 -c4*q*ln(r)}
         potential.put(i,j,V);
      end;
      wait.Update;
   end;
   wait.Hide;
   Contour.DrawMatrix(potential);
{MAC dispose(contour);}
{MAC dispose(wait);}
end;

procedure DrawFieldVectors (var potential : DMatrix; var mask : BoolMatrix);
{ Draws field vectors in the current view-port. stand-alone }
VAR
   i,j,MaxCol,MaxRow,pos,maxPos,color : integer;
   maxE,E,Ex,Ey,disX,disY     : real;
begin
   color := GetColor;
   SetColor(white);
   HideMouse;
   ClearViewPort;
   with views[ViewPortNumber] do rectangle(0,0,vx2-vx1,vy2-vy1);
   ShowMouse;
   potential.GetSize(MaxRow,MaxCol);
   with views[viewPortNumber] do
   begin
      disX := (vx2-vx1) / (maxCol-1);    {horiz. grid spacing}
      disY := (vy2-vy1) / (maxRow-1);    {vert.  grid spacing}
   end;
   maxE :=0;
   with potential do
   FOR i := 2 TO maxCol-1 DO             {find max E on the grid}
   FOR j := 2 TO maxRow-1 DO
    if not mask.value(j,i) then
    begin
       pos := (j-1)*maxCol+i;
       E := sqr(MatPtr^[pos-maxCol]-matPtr^[pos+maxCol]) +
          + sqr(MatPtr^[pos-1]-matPtr^[pos+1]);
       if E>maxE then maxE:=e;
    END;
   maxE:=sqrt(maxE);
   with potential do
   FOR i := 2 TO maxCol-1 DO
    FOR j := 2 TO maxRow-1 DO
     if not mask.value(j,i) then
     begin
        pos := (j-1)*maxCol+i;
        Ex := MatPtr^[pos-1] - MatPtr^[pos+1];
        Ey := MatPtr^[pos-maxCol] - MatPtr^[pos+maxCol];
        E  := sqrt(sqr(Ex)+sqr(Ey)) +1;
        SetColor(rainbow[1].colors[trunc(11*E/maxE)+1]);
        E  := 2*E;
        vector(round(disX*(i-1-Ex/E)), round(disY*(j-1-Ey/E)),
               round(disX*(i-1+Ex/E)), round(disY*(j-1+Ey/E)));
     END;
   SetColor(color);
end;


{------------------------ Follow Field Line Tools --------------------------}

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

function Interpolate2 (y0,y1,y3,y4 : real) : real;
{ interpolate using qubic aproximation.         }
{ Input: 4 values of y for 4 points on the stright line.   }
{ Output: approximate y2          }
begin
   Interpolate2 := (4*(y1+y3)-(y0+y4))/6;
end;

function derivative1 (v0,v1,v2,v3,x : real) : real;
{ Find first derivative using qubic aproximation.                     }
{ Input: 4 values of V for 4 points on the stright line, x which is   }
{ distance from second point (so in the middle section).              }
var a,b,c : real;
begin
   a := (v3-3*v2+3*v1-v0)/6;
   b := (v2-2*v1+v0)/2;
   c := v1-v0 -a +b;
   derivative1 := (3*a*x +2*b)*x + c;
end;

function derivative2 (v0,v1,v2,v3,x : real) : real;
{ Find Electric Field knowing 4 values of V for 4 points around    }
{ x is horizontal distance from left 2 points.                     }
begin
   derivative2 := x*(v1-v0+v2-v3)/2 - (v0-v2)/2;
end;

function derivative3 (v0,v1,v3,v4 : real) : real;
{ Find Electric Field between 4 points with known potential on the }
{ stright line.                                                    }
begin
   derivative3 := (v0-8*v1+8*v3-v4)/12;
end;


                                                       
procedure Rect_Vector_intersection(r : rectType; p,v : point; var o : point);
{ This procedure solves rectangle-vector intersection. Rectangle "r" is    }
{ intersected by vector "v" in point "p". Point "o" is a second point of   }
{ intersection. stand-alone                                                }
var
   d    : point;
   kind : char;
   b    : real;
begin
   d.x := r.x2-r.x1;                {size of the rectangle}
   d.y := r.y2-r.y1;
   p.x := (p.x-r.x1) / d.x;         {intersection in size independed coords}
   p.y := (p.y-r.y1) / d.y;
   v.x := v.x/d.x;                  {adjust vector to scaling made}
   v.y := v.y/d.y;
   if v.x=0 then                    {v - vertical}
    begin
       if v.y<0 then kind:='B' else
       if v.y>0 then kind:='U' else
       kind:='0';                   {v = (0,0)}
    end
   else
    begin                           {v - not vertical}
       b := p.y - v.y/v.x*p.x;
       if v.x<0 then                {v points left}
        begin
           if b<0 then kind:='B' else
           if b>1 then kind:='U' else
           kind:='L';
        end
       else
        begin                       {v points right}
           b := b + v.y/v.x;
           if b<0 then kind:='B' else
           if b>1 then kind:='U' else
           kind:='R';
        end;
    end;
   case kind of
    'L': begin  o.x:=0;  o.y := p.y - p.x*v.y/v.x;      end;  {left}
    'R': begin  o.x:=1;  o.y := p.y + (1-p.x)*v.y/v.x;  end;  {right}
    'B': begin  o.x := p.x - p.y*v.x/v.y;       o.y:=0  end;  {bottom}
    'U': begin  o.x := p.x + (1-p.y)*v.x/v.y;   o.y:=1  end;  {top}
    '0': begin  o.x := p.x; o.y := p.y;                 end;  {v = (0,0)}
   end;
   o.x := o.x*d.x + r.x1;
   o.y := o.y*d.y + r.y1;
end;

{------------------------ TFieldLines Object -------------------------------}

Type
   DirectionType = (UpField,DownField);
   TFieldLines = object
      procedure init(Redraw : DrawProcedure;
                     var potential_, charges_ : DMatrix;
                     var mask_: BoolMatrix);
   private
      potential, charges : DMatrix;
      mask      : BoolMatrix;
      FLHotKeys : THotKeys;
      contour   : TContour;
      direction : directionType;
      ExitFlag  : boolean;
      disX,disY : real;
      procedure FindE ( q : point; var E : point);
      procedure FollowLine(r,c : real; direction_ : directionType);
      function  ClickedAt(var r,c : real) : boolean;
   end;


procedure TFieldLines.FindE(q : point; var E : point);
var
   p : integer;
   c,maxRow : Integer;
   dummy,V0,V1,V3,V4 : real;
begin
   potential.GetSize(MaxRow,c);
   p := round((int(q.y)-1)*c+int(q.x));
   with potential do
   begin
      if (abs(q.x-round(q.x))<0.05) and (abs(q.y-round(q.y))<0.05) then
       begin
          E.x := dmdx( round(q.y), round(q.x), 1);
          E.y := dmdy( round(q.y), round(q.x), 1);
       end
      else
      if frac(q.y)=0 then
       begin
          E.x := Derivative1(  MatPtr^[p-1], MatPtr^[p],
                               MatPtr^[p+1], MatPtr^[p+2], frac(q.x));
          p   := p - 2*c;
          V0  := Interpolate1( MatPtr^[p-1], MatPtr^[p],
                               MatPtr^[p+1], MatPtr^[p+2], frac(q.x));
          p   := p + c;
          V1  := Interpolate1( MatPtr^[p-1], MatPtr^[p],
                               MatPtr^[p+1], MatPtr^[p+2], frac(q.x));
          p   := p + 2*c;
          V3  := Interpolate1( MatPtr^[p-1], MatPtr^[p],
                               MatPtr^[p+1], MatPtr^[p+2], frac(q.x));
          p   := p + c;
          V4  := Interpolate1( MatPtr^[p-1], MatPtr^[p],
                               MatPtr^[p+1], MatPtr^[p+2], frac(q.x));
          E.y  :=Derivative3( V0, V1, V3, V4);
       end
      else
       begin
          E.y := Derivative1(  MatPtr^[p-c], MatPtr^[p],
                               MatPtr^[p+c], MatPtr^[p+2*c], frac(q.y));
          p   := p - 2;
          V0  := Interpolate1( MatPtr^[p-c], MatPtr^[p],
                               MatPtr^[p+c], MatPtr^[p+2*c], frac(q.y));
          p   := p + 1;
          V1  := Interpolate1( MatPtr^[p-c], MatPtr^[p],
                               MatPtr^[p+c], MatPtr^[p+2*c], frac(q.y));
          p   := p + 2;
          V3  := Interpolate1( MatPtr^[p-c], MatPtr^[p],
                               MatPtr^[p+c], MatPtr^[p+2*c], frac(q.y));
          p   := p + 1;
          V4  := Interpolate1( MatPtr^[p-c], MatPtr^[p],
                               MatPtr^[p+c], MatPtr^[p+2*c], frac(q.y));
          E.x := Derivative3( V0, V1, V3, V4);
      end;
   end;
   If direction=DownField then begin E.x:=-E.x; E.y:=-E.y end;
end;


procedure TFieldLines.FollowLine(r,c : real; direction_ : directionType);
const small = 0.05;
var
   maxCol,maxRow,counter : Integer;
   E,E1,p,pOld : point;
   t : pointType;
   rct : rectType;

   procedure SetRect(var r : rectType; x1,y1,x2,y2 : real);
   begin
      r.x1:=trunc(x1+small); r.x2:=trunc(x2+small);
      r.y1:=trunc(y1+small); r.y2:=trunc(y2+small);
   end;

   function InForbitenArea(p : point) : boolean;
   var
      inside, NearCharge, NearPlate : boolean;
      t : pointType;
   begin
      inside := (p.y>2) and (p.y<maxRow-1) and (p.x>2) and (p.x<maxCol-1);
      t.x:=trunc(p.x); t.y:=trunc(p.y);
      with charges do
       NearCharge := (value(t.y,t.x)<>0)   or (value(t.y+1,t.x)<>0)
                  or (value(t.y,t.x+1)<>0) or (value(t.y+1,t.x+1)<>0)
                  or (value(t.y,t.x-1)<>0) or (value(t.y-1,t.x)<>0);
      with mask do
       NearPlate := value(t.y,t.x)   or value(t.y+1,t.x)
                 or value(t.y,t.x+1) or value(t.y+1,t.x+1)
                 or value(t.y,t.x-1) or value(t.y-1,t.x);
       InForbitenArea := (not inside) or NearPlate or Nearcharge;
   end;

begin
   direction := direction_;
   potential.GetSize(MaxRow,MaxCol);
   p.x:=c; p.y:=r;
   moveTo( round(disX*(p.x-1)), round(disY*(p.y-1)));
   counter := 0;
   HideMouse;
   if InForbitenArea(p) then beep;
   while (not InForbitenArea(p)) and (counter<100) do
   begin
      inc(counter);
      findE(p,E);
      if (abs(p.x-round(p.x))<small) and (abs(p.y-round(p.y))<small) then
       SetRect(rct, p.x-1, p.y-1, p.x+1, p.y+1)
      else
      if frac(p.y)=0 then SetRect(rct, p.x,   p.y-1, p.x+1, p.y+1)
      else {frac(p.x)=0}  SetRect(rct, p.x-1, p.y,   p.x+1, p.y+1);
      pOld:=p;
      Rect_Vector_intersection(rct,pOld,E,p);
      lineTo( round(disX*(p.x-1)), round(disY*(p.y-1)));
   end;
   ShowMouse;
end;


function TFieldLines.ClickedAt(var r,c : real) : boolean;
{ Detects where the mouse was clicked and if it is inside the plot returns }
{ its value.                                                               } 
var xc,yc,button : integer;
begin
   ClickedAt := false;
   if event.mouseClicked then
    with event do with views[1] do
     if (x>vx1) and (x<vx2) and (y>vy1) and (y<vy2) then
     begin
{        mouseGlobalPosn(xc,yc,button);}
        c := (x-vx1) / disX + 1.0;
        r := (y-vy1) / disY + 1.0;
        if abs(round(c)-c)>abs(round(r)-r) then  { this one is rounded...}
        r:=1.0*round(r) else c:=1.0*round(c);
        clickedAt := true;                       {... so point is on the ...}
     end;                                        {... grid lines. }
end;


procedure TFieldLines.Init(Redraw : DrawProcedure;
                           var potential_, charges_ : DMatrix;
                           var mask_ : BoolMatrix);
{ Draws "potential_" using procedure "Redraw" then waits for mouse click...}
{ ... and draws the field line that passes this point.                     }
var
   c,r : real;
   key : byte;
   maxCol,maxRow : integer;
begin
{MAC new(envir);}
{MAC new(potential);}
{MAC new(mask);}
{MAC new(charges);}
{MAC new(FLHotKeys);}
   potential := potential_;
   charges   := charges_;
   mask      := mask_;
   potential.GetSize(MaxRow,MaxCol);
   with views[viewPortNumber] do
   begin
      disX := (vx2-vx1) / (maxCol-1);    {horiz. grid spacing}
      disY := (vy2-vy1) / (maxRow-1);    {vert.  grid spacing}
   end;
   Message('Click the mouse to where you want to start.');
   WITH FLHotKeys DO
   BEGIN
      init(3);
      key[1] := 'F1-Help';
      key[2] := 'Del-Clear';
      key[3] := 'Esc-Exit';
      Display;
   END;
   Redraw(potential);
   repeat
      CheckForEvents;
      If ClickedAt(c,r) then
      begin
         FollowLine(c,r,upfield);
         FollowLine(c,r,downfield);
      end;
      if FLHotKeys.pressed(key) then
        case key of
         1: ShowHelp('Poisson.hlp','Field Lines Help');
         2: ReDraw(potential);
        end;
   until (Key=3);
{MAC dispose(envir);}
{MAC dispose(potential);}
{MAC dispose(mask);}
{MAC dispose(charges);}
{MAC dispose(FLHotKeys);}
end;

procedure DrawFieldLines(Redraw : DrawProcedure;
                         var potential, charges : DMatrix;
                         var mask : BoolMatrix);
{ this procedure was created only to hide FieldLines object. }
var
   FieldLines : TFieldLines;
   envir : TEnvironment;
begin
{MAC new(envir);}
{MAC new(FieldLines);}
   envir.Save;
   envir.Standardize;
   SelectViewPort(1);
   FieldLines.Init( ReDraw, potential, charges, mask);
   envir.reset;
{MAC dispose(FieldLines);}
{MAC dispose(envir);}
end;


{---------------------------- TCrossSection --------------------------------}
Type
   TCrossSection = object
      procedure Draw(var potential_: DMatrix; h_ : point; scale_: GraphScale);
   private
      V,E,Ex,Ey,D : DVector;
      Potential : DMatrix;
      h         : point;
      scale     : GraphScale;
      FuncFlag  : boolean;
      CSHotKeys : THotKeys;
      procedure Initialize;
      procedure FindVE(a,b : pointType);
      procedure PlotVE(scaleV : point);
      procedure UpdateMeter(var dis,p : point);
   end;


PROCEDURE Print1( X, Y: integer; Num : real; color : integer);
VAR
   Row, Col, a : integer;
   s : string[8];
BEGIN
   s := '';
   FOR a := 1 TO 8 DO s := s + char(219);
   Col := Round((x - 0.6) * ColWidth);
   Row := Round((y - 0.6) * RowHt);
   SetColor(DarkGray);
   OutTextXY(Col, Row, s);
   SetColor(Color);
   s := NumStr(num,5,1);
   OutTextXY(Col, Row, s);
END;


procedure FLine(a,b : pointType); far;
begin
   line(a.x,a.y,b.x,b.y);
end;


procedure TCrossSection.Initialize;
var
   contour : TContour;
begin
   DefineViewPort(3,0.06,0.49,0.74,1.00);     {potential}
   DefineViewPort(4,0.57,1.00,0.74,1.00);     {E field}
   DefineViewPort(7,0.00,1.00,0.74,1.00);     {E field}
   DefineViewPort(5,0.31,1.00,0.08,0.72);     {system}
   DefineViewPort(6,0.00,0.29,0.08,0.72);     {info}
   ClearMuppetPort;
   OpenViewPort(3);
   PutLabel(inside,'Potential and Div of Field');
   OpenViewPort(4);
   PutLabel(inside,'Electric Field');
   SelectViewPort(5);
   Contour.init;
   Contour.DrawMatrix(potential);
   OpenViewPort(6);
   print(1,1,'Mouse is pointing at:');
   print(1,2,' Potential =');
   print(1,3,'Electric Field:');
   print(1,4,'  Ex       =');
   print(1,5,'  Ey       =');
   print(1,6,' |E|       =');
   print(1,7,' Div E     =');
   print(1,8,'Position:');
   print(1,9,'  x        =');
   print(1,10,'  y        =');
   print(1,14,'Use mouse to draw line');
   print(1,15,'  of cross section.');
   WITH CSHotKeys DO
   BEGIN
      init(2);
      key[1] := 'F1-Help';
      key[2] := 'Esc-Exit';
      BackColor := Green;
      Display;
   END;
end;

function RubberLineDrawn(dis : point; var a,b : pointType) : boolean;
var
   LineLength : integer;
   d : pointType;
begin
   LineLength := 0;
   d.x := round(dis.x);
   d.y := round(dis.y);
   with views[5] do
   if event.mouseClicked then
   begin
      if (event.x>vx1+d.x) and (event.x<vx2-d.x) and
         (event.y>vy1+d.y) and (event.y<vy2-d.y) then
      begin
         SelectViewPort(5);
         mouseBorders (vx1+d.x,vy1+d.y,vx2-d.x,vy2-d.y);
         a.x := round((a.x-1)*dis.X);
         a.y := round((a.y-1)*dis.Y);
         b.x := round((b.x-1)*dis.X);
         b.y := round((b.y-1)*dis.Y);
         SetWriteMode(XORput);
         HideMouse;
         FLine(a,b);  {erase old cross section line}
         ShowMouse;
         SetWriteMode(CopyPut);
         a.x := round( int((event.x-vx1+dis.X/2) / dis.X) * dis.X);
         a.y := round( int((event.y-vy1+dis.Y/2) / dis.Y) * dis.Y);
         DrawRubberBand(FLine,a,b,blakk,true);
         b.x := round( int((b.x+dis.X/2) / dis.X) * dis.X);
         b.y := round( int((b.y+dis.Y/2) / dis.Y) * dis.Y);
         SetWriteMode(XORput);
         HideMouse;
         FLine(a,b);  {draw new cross section line}
         ShowMouse;
         SetWriteMode(CopyPut);
         a.x := round(a.x/dis.X) +1;
         a.y := round(a.y/dis.Y) +1;
         b.x := round(b.x/dis.X) +1;
         b.y := round(b.y/dis.Y) +1;
         lineLength := sqr((b.x-a.x)) + sqr((b.y-a.y));
         mouseBorders(0,0,GetMaxX,GetMaxY);
         SelectViewPort(6);
      end else
      if (event.x>vx1) and (event.x<vx2) and
         (event.y>vy1) and (event.y<vy2) then
      announce('Please start your cross-section line farther from the boundary');
   end;
   RubberLineDrawn := (LineLength>10);
end;

function Interpolate(v1,v2,v3,v4,x,y : real) : real;
begin
   if x+y<1 then
      Interpolate := (v2-v1)*x +(v3-v1)*y +v1
   else
      Interpolate := (v3-v4)*(1-x) +(v2-v4)*(1-y) +v4;
end;

procedure FindVDE(p,h : point; var Potn,Divr : real; var elec : point;
                  var Potential : DMatrix);
var
   x,y : real;
   col,row,nCol,nRow,i : integer;
   v : array [2..13] of real;
   k : point;
begin
   x   := frac(p.x);
   y   := frac(p.y);
   col := trunc(p.x);
   row := trunc(p.y);
   potential.GetSize(nRow,nCol);
   if (col>1) and (col<nCol-1) and (row>1) and (row<nRow-1) then
   begin
      with potential do
      for i := 1 to 8 do
       v[i+1] := value( row-1 + i div 3, col-1 + i mod 3);
      v[10] := potential.value( row,   col+2 );
      v[11] := potential.value( row+2, col);
      v[12] := potential.value( row+1, col+2 );
      v[13] := potential.value( row+2, col+1);
      Potn  := interpolate( v[5],v[6],v[8],v[9], x,y);
      Elec.x :=-interpolate( (v[6] -v[4])/(2*h.x), (v[10] -v[5])/(2*h.x),
                             (v[9] -v[7])/(2*h.x), (v[12] -v[8])/(2*h.x), x,y);
      Elec.y := interpolate( (v[8] -v[2])/(2*h.y), (v[9]  -v[3])/(2*h.y),
                             (v[11]-v[5])/(2*h.y), (v[13] -v[6])/(2*h.y), x,y);
      k.x := sqr(h.x);
      k.y := sqr(h.y);
      divr   := interpolate ((v[4]+v[6] -2*v[5])/k.x +(v[2]+v[8] -2*v[5])/k.y,
                             (v[5]+v[10]-2*v[6])/k.x +(v[3]+v[9] -2*v[6])/k.y,
                             (v[7]+v[9] -2*v[8])/k.x +(v[5]+v[11]-2*v[8])/k.y,
                             (v[8]+v[12]-2*v[9])/k.x +(v[6]+v[13]-2*v[9])/k.y,
                              x,y);
   end
   else
   begin
      Elec.x := 0;
      Elec.y := 0;
      divr   := 0;
   end;
end;

procedure TCrossSection.UpdateMeter(var dis,p : point);
var
   x,y,button,MaxX,MaxY,l : integer;
   Dv,Pt : real;
   m,El  : point;
begin
   l := 14;
   MouseGlobalPosn(x,y,button);
   with views[5] do
   if (x>vx1) and (x<vx2) and (y>vy1) and (y<vy2) then
   begin
      m.x := (x-vx1) / dis.X +1;
      m.y := (y-vy1) / dis.Y +1;
      if ((m.x<>p.x) or (m.y<>p.y)) then
      begin
         potential.GetSize(MaxY,MaxX);
         FindVDE( m,h,Pt,Dv,El,potential);
         Print1 (l,02,   Pt, yellow);
         Print1 (l,04, El.x, lightGreen);
         Print1 (l,05, El.y, lightMagenta);
         Print1 (l,06, sqrt(sqr(El.x)+sqr(El.y)), lightCyan);
         Print1 (l,07,   Dv, lightRed);
         if FuncFlag then
          with scale do
           begin
             Print1 (l,09, xMin +(xMax-xMin)*m.x/MaxX, whitte);
             Print1 (l,10, yMax -(yMax-yMin)*m.y/MaxY, whitte);
           end
         else
          begin
             Print1 (l,09, m.x -MaxX div 2 -1, whitte);
             Print1 (l,10, MaxY div 2 -m.y +1, whitte);
          end;
      end;
      p := m;
   end
   else
   if (p.x<>0) then
   begin
      p.x := 0;
      RubOut(l,2,8,DarkGray);
      for y := 4 to 10 do RubOut(l,y,8,DarkGray);
   end;
end;

procedure Average(var V : dVector);
var
   r : integer;
begin
   for r := 2 to V.size-1 do
    V.put( r, (V.value(r-1)+V.value(r)+V.value(r+1))/3);
   for r := V.size-1 downto 2 do
    V.put( r, (V.value(r-1)+V.value(r)+V.value(r+1))/3);
end;

procedure TCrossSection.FindVE( a,b : pointType);
var
   m,n,dx,x,y : real;
   step,i : integer;

   procedure SaveVE(x,y : real);
   var
      p,e_  : point;
      v_,d_ : real;
   begin
      p.x := x;
      p.y := y;
      inc(step);
      findVDE(p,h, v_,d_,e_,potential);
      V.put(step, v_);
      Ex.put(step, e_.x);
      Ey.put(step, e_.y);
      D.put(step, d_);
   end;

begin
   step := 0;
   if (b.x<>a.x) then
   begin
      m  := (b.y-a.y)/(b.x-a.x);
      n  := a.y - m*a.x;
      dx := (b.x-a.x)/(V.size-1);
      x  := a.x;
      while (step<V.size) do
      begin
         SaveVE( x, m*x+n);
         x := x + dx;
      end;
   end
   else
   begin
      dx := (b.y-a.y)/(V.size-1);
      y  := a.y;
      while (step<V.size) do
      begin
         SaveVE( a.x, y);
         y := y + dx;
      end;
   end;
   for i := 1 to E.size do E.put( i, sqrt(sqr(Ex.value(i))+sqr(Ey.value(i))) );
end;

procedure DrawXTickMarks;
var
   TickLength,dx,x : real;
   i : integer;
begin
   WITH GraphScales[ScaleNumber] DO
   BEGIN
      TickLength := 0.02 * (ymax - ymin);      {Tick hight}
      dx := (xmax-xmin)/10;
      for i := 1 to 9 do
      begin
         x := xmin+i*dx;
         PlotLine(x, ymin, x, ymin + TickLength);
         PlotLine(x, ymax, x, ymax - TickLength);
      end;
   END;
end;

procedure TCrossSection.PlotVE( scaleV : point);
var
   n : integer;
   a,min1,min2,max1,max2,LineLength : real;
   pos : DVector;
begin
{MAC new(pos);}
   pos.init(v.size);
   for n := 1 to pos.size do pos.put(n,n);
   CleanViewPort(7);
   OpenViewPort(3);
   PutLabel(inside,'Potential and Div of Field');
   D.MinMax( n,min1, n,max1);
   if (min1>scaleV.x) then min1 := scaleV.x;
   if (max1<scaleV.y) then max1 := scaleV.y;
   a := (max1-min1)/10;
   defineScale( 3, 1, pos.size, min1-a, max1+a);
   DrawXTickMarks;
   axis( 0, 0, 1000, tickSpace(max1-min1));
   SetColor(Yellow);
   PlotDVectors( Pos, V, 1, V.size);
   SetColor(lightRed);
   PlotDVectors( Pos, D, 1, D.size);
   OpenViewPort(4);
   SetColor(whitte);
   PutLabel( inside,'Electric Field');
   E .MinMax( n,a,    n,max1);
   Ex.MinMax( n,min1, n,a);
   Ey.MinMax( n,min2, n,a);
   if min1>min2 then min1 := min2;
   a := (max1-min1)/10;
   defineScale( 1, 1, pos.size, min1-a, max1+a);
   DrawXTickMarks;
   axis( 0, 0, 1000, tickSpace(max1-min1));
   SetColor(lightGreen);
   PlotDVectors( Pos, Ex, 1, E.size);
   SetColor(lightMagenta);
   PlotDVectors( Pos, Ey, 1, E.size);
   SetColor(lightCyan);
   PlotDVectors( Pos, E, 1, E.size);
   SetColor(whitte);
   SelectViewPort(6);
   pos.free;
{MAC dispose(pos);}
end;

procedure TCrossSection.Draw(var potential_: DMatrix; h_: point; scale_: GraphScale);
var
   key                  : byte;
   vp                   : view;
   dis,p,scaleV         : point;
   a,b                  : pointType;
   maxCol,MaxRow,n      : integer;
begin
{MAC   new(V); new(E); new(Ex); new(Ey); new(D); }
{MAC   new(Potential);}
{MAC   new(CSHotKeys);}
   potential := potential_;
   scale     := scale_;
   h         := h_;
   FuncFlag  := (scale.xMax<>scale.xMin);
   a.x       := 0;
   a.y       := 0;
   b         := a;
   initialize;
   potential.GetSize(MaxRow,MaxCol);
   potential.MinMax(n,n,scaleV.x, n,n,scaleV.y);
   vp := views[5];
   dis.x := (vp.vx2-vp.vx1) / (MaxCol-1);
   dis.y := (vp.vy2-vp.vy1) / (MaxRow-1);
   V.init(100);
   E.init(100);
   Ex.init(100);
   Ey.init(100);
   D.init(100);
   repeat
      CheckForEvents;
      UpdateMeter(dis,p);
      if RubberLineDrawn(dis,a,b) then
      begin
         FindVE(a,b);
         PlotVE(scaleV);
      end;
      if CSHotKeys.pressed(key) and (key=1) then
       ShowHelp('Poisson.hlp','Cross Section Help');
   until (Key=2);
   ClearMuppetPort;
   V.free;
   E.free;
   Ex.free;
   Ey.free;
   D.free;
{MAC   dispose(V); dispose(E); dispose(Ex); dispose(Ey); dispose(D); }
{MAC   dispose(Potential);}
{MAC   dispose(CSHotKeys);}
end;

procedure DrawCrossSection(var potential: DMatrix; h: point; scale: GraphScale);
var CrossSection : TCrossSection;
begin
{MAC new(CrossSection);}
   CrossSection.Draw(potential,h,scale);
{MAC dispose(CrossSection);}
end;

{------------------------- Divergence -------------------------------------}

procedure FindDivergence(VAR V,D,temp : DMatrix; h : point);
var
  p,c,r,i,j : integer;
  min,max : real;
begin
   D.fill(0);
   V.GetSize(R,C);
   FOR i := 1 TO C DO
    FOR j := 1 TO R DO
     Temp.Put(j,i, -V.d2mdx2(j,i,h.x)-V.d2mdy2(j,i,h.y) );
  with Temp do
  FOR i := 3 TO C-2 DO
   FOR j := 3 TO R-2 DO
   begin
      p := (j-1)*C+i;
      D.Put(j,i,MatPtr^[p]/2
      + (MatPtr^[p-1-c] +MatPtr^[p+1-c] +MatPtr^[p-1+c] +MatPtr^[p+1+c])/24
      + (MatPtr^[p-c]   +MatPtr^[p-1]   +MatPtr^[p+1]   +MatPtr^[p+c]  )/12);
   end;
  temp.fill(0);
  D.MinMax(i,i,Min,i,i,Max);
  if (max-min)<1E-6 then D.Fill(Max);
end;

procedure FindField(VAR V,E : DMatrix; h : point);
var
  c,r,i,j : integer;
  Ex,Ey : real;
  min,max : real;
begin
   V.GetSize(R,C);
   FOR i := 1 TO C DO
    FOR j := 1 TO R DO
    begin
       Ex := V.dmdx(j,i,h.x);
       Ey :=-V.dmdy(j,i,h.y);
       E.Put(j,i,sqrt(sqr(Ex)+sqr(Ey)));
    end;
  E.MinMax(i,i,Min,i,i,Max);
  if (max-min)<1E-6 then E.Fill(Max);
end;

{------------------------- User Function ----------------------------------}

procedure TUserFunction.Init;
begin
{MAC   new(parser);}
   rect.MaxN      := 20;
   rect.NOdd      := 1;
   rect.FuncStr   := 'exp(-n*x*pi)*sin(n*y*pi)/n';
   sphere.MaxN    := 1;
   sphere.NOdd    := 3;
   sphere.FuncStr := 'ln(r)';
   min[1]         := 0;
   max[1]         := 1.4;
   min[2]         := 0;
   max[2]         := 1;
   radius         := 15;
   RectSystem     := true;
   parser.init;
end;

procedure TUserFunction.Done;
begin
{MAC   dispose(parser);}
end;

function TUserFunction.Process(RectSys : boolean;
                               var matrix : DMatrix;
                               var h : point;
                               var Scale : GraphScale) : boolean;
var
   flag : boolean;
begin
   RectSystem := RectSys;
   flag := (RectSys and ReadRect) or ((not RectSys) and ReadSphere);
   flag := flag and CreateMatrix(matrix);
   if RectSystem then
   begin
      h.x := (max[1]-min[1])/(matrix.NCols-1);
      h.y := (max[2]-min[2])/(matrix.NRows-1);
      scale.xMin := min[1];  scale.xMax := max[1];
      scale.yMin := min[2];  scale.yMax := max[2];
   end
   else
   begin
      h.x := 2*radius/(matrix.NRows-1);
      h.y := h.x;
      scale.yMax :=  radius;
      scale.yMin := -scale.yMax;
      scale.xMax :=  radius*matrix.NCols/matrix.NRows;
      scale.xMin := -scale.xMax;
   end;
   Process := flag;
end;

function TUserFunction.ReadRect : boolean;
VAR
   Screen : TInputScreen;
   n : byte;
   StrOK : boolean;
   ParserHelp : HelpScrType;
BEGIN
{MAC new(Screen);}
   with Screen do
   with rect do
   begin
      init;
      DefineInputPort (0.04, 0.96, 0.3, 0.7);
      LoadLine('        Input Function of Potential (rectangular coordinates) ');
      LoadLine('');
      LoadLine('For n = 1 to {   } add V(x,y,n)= ');
      LoadLine('"                                                                      "');
      LoadLine('Where:  n is  #1 odd   #1 even   #1 both');
      LoadLine('        x is between {     } and {     }');
      LoadLine('        y is between {     } and {     }');
      LoadLine('');
      LoadLine('                   [  Ok  ]  [Cancel]  [  Help ] ');
      SetNumber( 1, MaxN);
      SetString( 2, FuncStr);
      SetRadioButton( '1', NOdd);
      SetNumber( 6, min[1]);
      SetNumber( 7, max[1]);
      SetNumber( 8, min[2]);
      SetNumber( 9, max[2]);
      SetNumberLimits(1,1,100);
      AboutParser(ParserHelp);
      SetHelpScreen(ParserHelp);
      repeat
         AcceptScreen;
         FuncStr := GetString(2);
         StrOK   := parser.parse('x','y','n',' ', FuncStr) or canceled;
         if not StrOK then
         announce('Error: Cannot evaluate this expression. Please try again.');
      until StrOk;
      MaxN     := round(GetNumber(1));
      NOdd     := GetRadioButton('1');
      min[1]   := GetNumber(6);
      max[1]   := GetNumber(7);
      min[2]   := GetNumber(8);
      max[2]   := GetNumber(9);
      if min[1]>max[1] then swap(min[1],max[1]);
      if min[2]>max[2] then swap(min[2],max[2]);
      ReadRect := Not Canceled;
      done;
   end;
{MAC dispose(Screen);}
end;


function TUserFunction.ReadSphere : boolean;
VAR
   Screen : TInputScreen;
   n : byte;
   StrOK : boolean;
   ParserHelp : HelpScrType;
BEGIN
{MAC new(Screen);}
   with Screen do
   with sphere do
   begin
      init;
      DefineInputPort (0.07, 0.93, 0.3, 0.7);
      LoadLine('      Input Function of Potential (Cylindrical coordinates) ');
      LoadLine('');
      LoadLine('For n = 1 to {   } add V(r,a,n)=');
      LoadLine('"                                                                  "');
      LoadLine('         Where:   n is #1 odd   #1 even   #1 both');
      LoadLine('                  a is angle measured in radians ');
      LoadLine('                  r is between  0  and  {    }   ');
      LoadLine('');
      LoadLine('                      [  Ok  ]   [Cancel]   [ Help ]');
      SetNumber(1,MaxN);
      SetString(2,FuncStr);
      SetRadioButton('1',NOdd);
      SetNumber(6,radius);
      SetNumberLimits(1,1,100);
      SetNumberLimits(1,0,1E9);
      AboutParser(ParserHelp);
      SetHelpScreen(ParserHelp);
      repeat
         AcceptScreen;
         FuncStr := GetString(2);
         StrOK   := parser.parse('r','a','n',' ', FuncStr) or Canceled;
         if not StrOK then
         announce('Error: Cannot evaluate this expression. Please try again.');
      until StrOk;
      MaxN       := round(GetNumber(1));
      NOdd       := GetRadioButton('1');
      radius     := GetNumber(6);
      ReadSphere := Not Canceled;
      done;
   end;
{MAC dispose(Screen);}
end;

function TUserFunction.CreateMatrix (var matrix : DMatrix) : boolean;
var
   MaxCol, MaxRow : integer;
   center : pointType;
   XFactor,YFactor,RFactor : real;
   n, col,row : integer;
   x,y,old,r : real;
   NMax, NOdd : integer;
   wait : TWaitMessage;
BEGIN
{MAC new(wait);}
   matrix.GetSize(MaxRow,MaxCol);
   if rectSystem then NMax:=rect.MaxN else NMax:=sphere.MaxN;
   if rectSystem then NOdd:=rect.NOdd else NOdd:=sphere.NOdd;
   Xfactor  := (max[1]-min[1])/(maxCol-1);
   Yfactor  := (max[2]-min[2])/(maxRow-1);
   Rfactor  := 2*radius/(maxRow-1);
   center.x := maxCol div 2 +1;
   center.y := maxRow div 2 +1;
   HaltIfError := false;
   matrix.fill(0);
   wait.Show;
   with parser do
   FOR n := 1 to NMax do
   begin
      if (NOdd=3) or ((NOdd=1) and odd(n)) or ((NOdd=2) and (not odd(n))) then 
      FOR col := 1 TO maxCol DO
      begin
       FOR row := 1 TO maxRow DO
        IF errorFound THEN
         BEGIN
            HaltIfError  := true;
            ErrorFound   := false;
            CreateMatrix := false;
            wait.hide;
            Exit
         END
        ELSE
         begin
            old := matrix.Value(row,col);
            if RectSystem then
                matrix.put(row,col, old + f(min[1]+(col-1)*XFactor,
                                            max[2]-(row-1)*YFactor,n,0))
            else
             begin
                x := col-center.x;
                y := row-center.y;
                r := sqrt(x*x+y*y)*RFactor;
                if ((x=0) and (y=0)) then matrix.put(row,col,0) else
                matrix.put(row,col, old + f(r,arcTan2(x,y),n,0 ));
             end;
         end;
        wait.update;
      end;
   end;
   { To avoid singularities at (0,0) }
   if not RectSystem then with matrix do
    put(center.y,center.x, 4*value(center.y, center.x+1)
                          -6*value(center.y, center.x+2)
                          +4*value(center.y, center.x+3)+
                          -  value(center.y, center.x+4));
   wait.hide;
   HaltIfError  := true;
   ErrorFound   := false;
   CreateMatrix := true;
{MAC dispose(wait);}
end;

{---------------------------- interface tools ----------------------------}

Function ReadFileName : string;
var
   Screen   : TInputScreen;
   FileName : string;
   n        : integer;
begin
{MAC new(Screen);}
   with Screen do
   begin
      init;
      DefineInputPort(0.3,0.7,0.41,0.59);
      LoadLine('Save File');
      LoadLine('"                            "');
      LoadLine('');
      LoadLine('     [  Ok  ]    [Cancel]');
      AcceptScreen;
      if canceled then FileName := ''
      else FileName := GetString(1);
      done;
   end;
   n := Pos('.',FileName)+1;
   if n>1 then
   begin
      Delete(FileName,n,3);
      Insert('CFG',FileName,n);
   end else FileName := concat (FileName,'.CFG');
   ReadFileName := FileName;
{MAC dispose(Screen);}
end;

Procedure Ask(question : string; var answer : string);
var
   Screen : TInputScreen;
   dx     : real;
begin
{MAC new(Screen);}
   with Screen do
   begin
      init;
      dx := TextWidth(question)/(1.9*GetMaxX);
      DefineInputPort(0.5 - dx, 0.5 + dx,0.45,0.55);
      LoadLine(question);
      LoadLine(' [  Ok  ] [  No  ] [Cancel]');
      AcceptScreen;
      if GetBoolean(1) then answer := 'Yes' else
      if GetBoolean(2) then answer := 'No' else
      if GetBoolean(3) then answer := 'Cancel';
      done;
   end;
{MAC dispose(Screen);}
end;

{---------------------------- Icons ---------------------------------------}

procedure TIcons.Draw;
{ Draws pictures inside each ikon box}
VAR
   a,h,color : word;
   envir     : TEnvironment;
BEGIN
{MAC new(envir);}
   envir.save;
   envir.Standardize;
   OutTextXY(5,50,'Plates');
   size := GetMaxY div 17;
   d := 65;  {36}
   HideMouse;
   h := 5;
   SetColor (lightcyan);
   a:=1;                                                           {row 1}
   line( h, size*(a-1)+d+h, size-h, size*a+d-h);                   {line}
   rectangle( size+h, size*(a-1)+d+h, 2*size-h, size*a+d-h);       {rectangle}
   inc(a);                                                         {row 2}
   circle(  size div 2, size*(2*a-1) div 2+d, size div 2-h);       {ellipse}
   line( size+h, size*(a-1)+d+h, 2*size-h, size*(a-1)+d+h);        {2 plates}
   line( size+h, size*a+d-h, 2*size-h, size*a+d-h);
   inc(a);                                                         {row 3}
   circle(  size div 2,  size*(2*a-1) div 2+d, 2);                 {point}
   SetLineStyle(DottedLn,0,3);
   line( size+h, size*(a-1)+d+h, 2*size-h, size*a+d-h);            {line of q}
   inc(a);                                                         {row 4}
   SetTextJustify(CenterText,CenterText);
   outTextXY(size,size*(2*a-1) div 2 + d,'Modify');
   ShowMouse;
   update;
   envir.reset;
{MAC dispose(envir);}
end;

procedure TIcons.Update;
var
   a : integer;
   str : string;
   envir     : TEnvironment;
BEGIN
{MAC new(envir);}
   envir.save;
   envir.Standardize;
   SetLineStyle(SolidLn,0,ThickWidth);
   SetColor(blakk);
   HideMouse;
   FOR a := 1 TO 3 DO      {erase old ikons boxes}
   BEGIN
      rectangle(    0, size*(a-1)+d,   size, size*a+d);
      rectangle( size, size*(a-1)+d, 2*size, size*a+d);
   END;
   a:=4;
   rectangle( 0, size*(a-1)+d, 2*size, size*a+d);   {"modify" box}
   SetLineStyle(SolidLn,0,NormWidth);
   SetColor(whitte);
   FOR a := 1 TO 3 DO {2}     {draw new ikon boxes}
   BEGIN
      rectangle(    0, size*(a-1)+d,   size, size*a+d);
      rectangle( size, size*(a-1)+d, 2*size, size*a+d);
   END;
   a:=4;
   rectangle( 0, size*(a-1)+d, 2*size, size*a+d);   {"modify" box}
   SetColor(whitte);
   SetLineStyle(SolidLn,0,ThickWidth);
   CASE choice OF                                    {update penMode ikons}
    line_   : rectangle(    0,       d,   size,   size+d);
    rect_   : rectangle( size,       d, 2*size,   size+d);
    oval_   : rectangle(    0,  size+d,   size, 2*size+d);
    IIPlts  : rectangle( size,  size+d, 2*size, 2*size+d);
    point_  : rectangle(    0,2*size+d,   size, 3*size+d);
    qLine   : rectangle( size,2*size+d, 2*size, 3*size+d);
   END;
   if modify then rectangle( 0,  3*size+d, 2*size, 4*size+d);
   SetTextStyle(0,VertDir,1);
   SetTextJustify(LeftText,CenterText);
   RubOutXY(0.04,0.4,14,blakk);
   SetColor(whitte);
   if (choice=point_) or (choice=qLine) then PrintXY(0.04,0.4,'Charge') else
   if (choice=nil_)  then PrintXY(0.04,0.4,'Deactivated') else
   PrintXY(0.04,0.4,'Potential (V)');
   ShowMouse;
   envir.reset;
{MAC dispose(envir);}
end;

function TIcons.clicked : ShapeType;
var c: integer;
begin
   if event.mouseClicked then
    with event do
     if (x<2*size) and (y>d) and (y<4*size+d) then
     begin
        if x<size then c:=1 else c:=2;
        if (y>size+d)   and (y<2*size+d) then c:=c+2;
        if (y>2*size+d) and (y<3*size+d) then c:=c+4;
        if (y>3*size+d) then modify := not modify else   
        case c of
         1: choice := line_;
         2: choice := rect_;
         3: choice := oval_;
         4: choice := IIPlts;
         5: choice := point_;
         6: choice := qLine;
        end;
        update;
     end;
   clicked := choice;
end;

{---------------------------- BoolMatrix -----------------------------------}

PROCEDURE BoolMatrix.Init (NumRows, NumCols: INTEGER);
VAR
   Row,Col: INTEGER;
   MemReq, MaxBlock: String[10];
BEGIN
   NRows:= NumRows; NCols:= NumCols;
   MemBlock := LongInt(NCols)*LongInt(NRows)*SizeOf(boolean);
   IF ((MemBlock > MaxAvail) OR (MemBlock >=65521)) THEN
{MAC   IF (MemBlock >=65521) THEN}
     BEGIN
     HaltIfError:=TRUE;
     MemReq := NumStr(MemBlock,10,0);
     MaxBlock := NumStr(MaxAvail,10,0);
     Error(concat('BoolMatrix: Request for ',MemReq,
                ' exceeds free memory ',MaxBlock,' or 65521'));
{MAC Error(concat('BoolMatrix: Request for ',MemReq,' exceeds 65521'));}
     END
   ELSE
     BEGIN
     GetMem(BoolPtr,MemBlock);
{MAC BoolHandle := DHandle(NewHandle(MemBlock));}
     FOR Row:=1 TO NRows DO FOR Col:=1 TO NCols DO
        BoolPtr^[(Row -1)*NCols + Col] := false;
     END
END;

PROCEDURE BoolMatrix.Free;
BEGIN
   FreeMem(BoolPtr,MemBlock);
{MAC DisposHandle(handle(BoolHandle));}
   NRows:= 0; NCols:= 0;
END;

PROCEDURE BoolMatrix.Put(Row,Col: INTEGER; X: Boolean);
BEGIN
   BoolPtr^[(Row -1)*NCols + Col]:= X
END;

FUNCTION BoolMatrix.Value(Row,Col: INTEGER): boolean;
BEGIN
   Value:= BoolPtr^[(Row -1)*NCols + Col]
END;

PROCEDURE BoolMatrix.Fill(FillValue: boolean);
VAR
  Row,Col: INTEGER;
BEGIN
   FOR Row:= 1 TO NRows DO
      FOR Col:=1 TO NCols DO
         BoolPtr^[(Row -1)*NCols + Col]:= FillValue
END;

end. {tools}




