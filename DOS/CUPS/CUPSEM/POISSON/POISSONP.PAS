                   {************************************}
                   {************************************}
                   {***                              ***}
                   {***         Unit PoissonP        ***}
                   {***  Part of program Poisson.Pas ***}
                   {***  Written by Jarek Tuszynski  ***}
                   {***                              ***}
                   {************************************}
                   {************************************}

Unit PoissonP;
{ Poisson_Plates unit contains code for object oriented structure used  }
{ to control all operations on plates. It is a backbone of drowing part }
{ of Poisson Program.}
{$F+}

interface

USES Graph, CUPS, CUPSmupp, CUPSproc, CUPSpars, CUPSgui;

type
   MapMarkProc   = procedure(x,y : integer);
   ShapeType     = (point_,line_,oval_,rect_,qLine, IIPlts, nil_);

   PointPtr = ^point;
   point = record
      x,y : real;
   end;

   PlatePtr  = ^TPointCharge;
   PlatesPtr = ^TPlates;
   TPlates = object
      dis      : Point;           {midgrid distances}
      root     : PlatePtr;
      port     : rectType;
      modifyOn : boolean;
      askForV  : boolean;         {always ask for V function on the plate}
      drawFromCenter : boolean;   {begin drawing from the center.}
      PlateSymbol    : real;      {V matrix will be filled with it}
      procedure Init;
      procedure PassVQMatrixes(var potential_,charges_ : DMatrix);
      function  Read(Shape : ShapeType; value : real) : boolean;
      Procedure DrawAll;
      procedure Modify(var Shape : ShapeType; var value : real);
      procedure RemoveAllSelected(var Shape : ShapeType; var value : real);
      function  ReadFromFile(Var f : text; Var MaxRow,MaxCol : integer;
                             var ChargesOnly : boolean) : boolean;
      function  CheckIfChargesOnly : boolean;
      procedure SaveToFile(var f : text);
      procedure ChangeFunction;
      procedure ReMapAll;
      procedure done;
   private
      currentPlate : PlatePtr;
      function  SelectedPlate : platePtr;
      function  Select(e : pointType) : boolean;
      function  Reshape(e : pointType) : boolean;
      procedure SelectAllInside(r : rectType; flag : boolean);
      procedure CreatePlate(a,b : pointType; Shape : ShapeType;
                     value : real; step : integer);
      procedure DrawPointsOfAllSelected(flag : boolean);
      procedure MoveAllSelected(d : pointType);
   end;

   TPointCharge = object
      select : boolean;             {is plate selected by modify mode}
      constructor init(p : PointType; parent_ : PlatesPtr; Value_ : real);
      procedure Map;                              Virtual;
      procedure draw;                             Virtual;
      FUNCTION  Picked(p : pointType) : boolean;  Virtual;
      FUNCTION  Reshape(e : pointType) : boolean; Virtual;
      FUNCTION  Inside (r : rectType) : Boolean;  Virtual;
      PROCEDURE DrawPoints(noErase : boolean);    Virtual;
      Procedure MoveAndDraw(d : pointType);       Virtual;
      function  Shape : ShapeType;                Virtual;
      destructor done;                            Virtual;
   private
      next   : PlatePtr;            {pointer to next plate}
      parent : PlatesPtr;           {pointer to rest of plates}
      a,b    : pointType;           {end points for point b=(0,0)}
      value  : real;                {potential/charge value}
      Functn : string;              {distribution function}
      procedure ReadFunction;                     Virtual;
      function  GetStepSize : integer;            Virtual;
      procedure MapPlate(mark : mapMarkProc);     Virtual;
      procedure MatrixCoords(var p,q : pointType);
   end;

   TLinePlate = object(TPointCharge)
      constructor init(p,q : PointType; parent_ : PlatesPtr; Value_ : real);
      procedure Map;                              Virtual;
      procedure draw;                             Virtual;
      FUNCTION  Picked(p : pointType) : boolean;  Virtual;
      FUNCTION  Reshape(e : pointType) : boolean; Virtual;
      PROCEDURE DrawPoints(noErase : boolean);    Virtual;
      Procedure MoveAndDraw(d : pointType);       Virtual;
      function  Shape : ShapeType;                Virtual;
      FUNCTION  Inside (r : rectType) : Boolean;  Virtual;
      destructor done;                            Virtual;
   private
      procedure ReadFunction;                     Virtual;
      procedure MapPlate(mark : mapMarkProc);     Virtual;
      procedure AcceptInputScreen(var Screen : TInputScreen); Virtual;
   end;

   TRectPlate = object(TLinePlate)
      constructor init(p,q : PointType; parent_ : PlatesPtr; Value_ : real);
      procedure draw;                             Virtual;
      FUNCTION  Picked(p : pointType) : boolean;  Virtual;
      FUNCTION  Reshape(e : pointType) : boolean; Virtual;
      PROCEDURE DrawPoints(noErase : boolean);    Virtual;
      Procedure MoveAndDraw(d : pointType);       Virtual;
      function  Shape : ShapeType;                Virtual;
      destructor done;                            Virtual;
   private
      procedure ReadFunction;                     Virtual;
      procedure MapPlate(mark : mapMarkProc);     Virtual;
   end;


   TOvalPlate = object(TLinePlate)
      constructor init(p,q : PointType; parent_ : PlatesPtr; Value_ : real);
      procedure Map;                              Virtual;
      procedure draw;                             Virtual;
      FUNCTION  Picked(p : pointType) : boolean;  Virtual;
      FUNCTION  Reshape(e : pointType) : boolean; Virtual;
      PROCEDURE DrawPoints(noErase : boolean);    Virtual;
      Procedure MoveAndDraw(d : pointType);       Virtual;
      function  Shape : ShapeType;                Virtual;
      destructor done;                            Virtual;
   private
      procedure ReadFunction;  Virtual;
      procedure MapPlate(mark : mapMarkProc);     Virtual;
   end;

   T2ParallelPlates = object(TRectPlate)
      constructor init(p,q : PointType; parent_ : PlatesPtr; Value_ : real);
      procedure draw;                             Virtual;
      FUNCTION  Picked(p : pointType) : boolean;  Virtual;
      FUNCTION  Reshape(e : pointType) : boolean; Virtual;
      Procedure MoveAndDraw(d : pointType);       Virtual;
      function  Shape : ShapeType;                Virtual;
      destructor done;                            Virtual;
   private
      procedure ReadFunction; Virtual;
      procedure mapPlate(mark : mapMarkProc);     Virtual;
   end;

   TLineOfCharges = object(TLinePlate)
      constructor init(p,q : PointType; parent_ : PlatesPtr;
                       Value_ : real; step_ : integer);
      procedure Map;                              Virtual;
      procedure draw;                             Virtual;
      FUNCTION  Reshape(e : pointType) : boolean; Virtual;
      Procedure MoveAndDraw(d : pointType);       Virtual;
      function  Shape : ShapeType;                Virtual;
      destructor done;                            Virtual;
   private
      Step : integer;
      procedure AskForNumOfCharges;
      procedure ReadFunction;  Virtual;
      function  GetStepSize : integer;            Virtual;
      procedure AcceptInputScreen(var Screen : TInputScreen); Virtual;
   end;

CONST
   on  = true;
   off = false;



implementation
USES Crt, dos, CupsFunc;

{variables used by procedures passed to Map metchods.}
Var
   {variables used in ordinary Map procedures}
   CurrentValue       : real;           {current value on the slider}
   potential, charges : DMatrix;        {pointers to DMatrixes}

   {variables used to count points on the line}
   NumberPoints       : integer;
   StepSize           : integer;

   {variable  used to map function of potential}
   vector             : DVector;

   {variables that has to be passed to Counter procedure}
   counterOn          : boolean;
   dist               : point;          {distances between grid lines}



{---------------------------- General ----------------------------------}

FUNCTION samePt (p,r : pointType) : boolean;
{checks if points p and q are the same. gravitation=3 points.}                                     
BEGIN
   samePt := (3>=abs(p.x-r.x)) AND (3>=abs(p.y-r.y));
END;

{...........................................................................}

FUNCTION samePo (p : pointType; x,y : integer) : boolean;
{checks if points p have coordinates (x,y). gravitation=3 points.}                                     
BEGIN
   samePo := (3>=abs(p.x-x)) AND (3>=abs(p.y-y));
END;

{...........................................................................}

PROCEDURE EraseXY(x,y:integer; num,BkColor:word);
{ It erase num characters ,starting from location x,y.}
{ Color,font,direction and size can be set before this call.}
BEGIN
   SetFillStyle(SolidFill,BkColor);
   Bar(x,y,x+num*ColWidth,y+RowHt);
END;

{...........................................................................}

FUNCTION PtInside (p:pointType; r:rectType) : boolean;
{checks if point 'p' is inside a rectangle 'r'.}
BEGIN
   p.x := p.x + views[ViewPortNumber].vx1;            {local to global}
   p.y := p.y + views[ViewPortNumber].vy1;
   PtInside := ( (abs(r.x1-r.x2)>=abs(2*p.x-r.x1-r.x2)) AND
               (  abs(r.y1-r.y2)>=abs(2*p.y-r.y1-r.y2)) );
END;

{...........................................................................}

PROCEDURE AddPt (VAR a:pointType; b,c:pointType);
{ adds points b and c and returns them as a}
BEGIN
   a.x:=b.x+c.x;
   a.y:=b.y+c.y;
END;

{...........................................................................}

PROCEDURE SwapPoints (VAR a,b : pointType);
var
   temp : pointType;
BEGIN
   temp := a;
   a    := b;
   b    := temp;
END;

{...........................................................................}

PROCEDURE SwapIntegers (VAR a,b : integer);
var
   temp : integer;
BEGIN
   temp := a;
   a    := b;
   b    := temp;
END;

{...........................................................................}

PROCEDURE SetRect(VAR r : rectType; x1,y1,x2,y2 : integer);
{ creates rectangle 'r' from given coordinates.}
BEGIN
   r.x1:=x1;
   r.y1:=y1;
   r.x2:=x2;
   r.y2:=y2;
END;

{...........................................................................}

procedure counter(a,b : pointType);
begin
   if counterOn then
   begin
      EraseXY(29,5,3,black);
      EraseXY(84,5,3,black);
      OutTextXY(29,5,numStr(round(abs(a.x-b.x)/dist.x),3,0));
      OutTextXY(84,5,numStr(round(abs(a.y-b.y)/dist.y),3,0));
   end;
end;

{------------------------- Bresenham Algorithms---------------------------}

procedure BresenhamLine(p,q : pointType; mark : MapMarkProc);
{ Uses Bresenham's Line Algorithm.}
var
   x,y,dx,dy,d,inc1,inc2,i,max : integer;
begin
   dx := abs(q.x-p.x);   dy := abs(q.y-p.y);
   i  := round(sgn((q.x-p.x)*(q.y-p.y)));
   if (dy=0) or ((dx/dy)>1) then
   begin
      d:=2*dy-dx; inc1:=2*dy; inc2:=2*(dy-dx);
      if p.x>q.x then begin x:=q.x; y:=q.y; max:=p.x end
      else begin x:=p.x; y:=p.y; max:=q.x end;
      mark(x,y);
      while x<max do
      begin
         if d<=0 then d:=d+inc1 else begin y:=y+i; d:=d+inc2 end;
         x:=x+1;
         mark(x,y);
      end;
   end
   else
   begin
      d:=2*dx-dy; inc1:=2*dx; inc2:=2*(dx-dy);
      if p.y>q.y then begin x:=q.x; y:=q.y; max:=p.y end
      else begin x:=p.x; y:=p.y; max:=q.y end;
      mark(x,y);
      while y<max do
      begin
         if d<=0 then d:=d+inc1 else begin x:=x+i; d:=d+inc2 end;
         y:=y+1;
         mark(x,y);
      end;
   end;
end;

{...........................................................................}

procedure BresenhamEllipse(p,q : pointType; mark : MapMarkProc);
{ Uses Bresenham's Ellipse Algorithm.}

   procedure OvalPoints(x,y : integer);
   begin
      mark( round((p.x+x)/2), round((p.y+y)/2) );
      mark( round((p.x-x)/2), round((p.y+y)/2) );
      mark( round((p.x-x)/2), round((p.y-y)/2) );
      mark( round((p.x+x)/2), round((p.y-y)/2) );
   end;

var
   d,c,dx,dy : real;
   x,y : integer;
begin        
   p.x := p.x+q.x;           {blow it 2 times}
   p.y := p.y+q.y;           { p is now center of the circle}
   q.x := 2*q.x;
   q.y := 2*q.y;
   dx := sqr(q.x-p.x);  dy := sqr(q.y-p.y);
   x:=0;   y:=abs(q.y-p.y);
   d := dy +dx*(0.25-y);
   OvalPoints(x,y);
   while (dx*(y-0.5) > dy*(x+1)) do                               {region 1}
   begin                                                          {upper}
      if d>=0 then begin  dec(y); d:=d-2*dx*y end;                {SE}
      d := d +dy*(3+2*x);
      inc(x);
      OvalPoints(x,y);
   end;
   d := dy*sqr(x+0.5)+(sqr(y-1.0) - dy)*dx;
   while y>0 do                                                   {region 2}
   begin                                                          {lower}
      if d<0 then begin inc(x); d:=d+2*dy*x; end;                 {S}
      d := d +dx*(3-2*y);
      dec(y);
      OvalPoints(x,y);
   end;
end;

function FunctToVector (var funct : string; var vector : DVector;
                        PlateSymbol : real) : boolean;
var
   flag    : boolean;
   parser  : TParser;
   size,i  : integer;
   z       : real;
begin
   ErrorFound  := false;
   HaltIfError := false;
   parser.init;
   flag := parser.parse('t','o','t','t',funct);
   size := vector.size-1;
   i := 0;
   repeat
      z := parser.f(i/size,PlateSymbol,0,0);
      if (abs(z) > 1E10) then
      begin
         ErrorFound := true;
         announce(concat('This function has singularity at ',
                         NumStr(i/size,4,2),'. Please try again.'));
      end;
      vector.put(i+1,z);
      inc(i);
   until (i>size) or errorFound;
   FunctToVector := not ErrorFound;
   ErrorFound    := false;
   HaltIfError   := true;
end;

{---------------------------- Mark Procedures ------------------------------}


procedure markQ (x,y : integer);
begin
   charges.put(y,x,CurrentValue)
end;

procedure markV (x,y : integer);
begin
   potential.put(y,x,CurrentValue)
end;

procedure markVFun (x,y : integer);
begin
   inc(NumberPoints);
   potential.put( y, x, vector.value(NumberPoints));
end;

procedure CountPoints (x,y : integer);
begin
   Inc(NumberPoints);
end;


{---------------------------- PointPlate --------------------------------}

procedure FPoint(a,b : pointType); far;
begin
   circle(b.x,b.y,2);
end;

constructor TPointCharge.init(p : PointType; parent_ : PlatesPtr;
                              Value_ : real);
begin
   a      := p;
   b.x    := 0;
   b.y    := 0;
   value  := value_;
   parent := parent_;
   select := true;
   next   := parent^.root;
   functn := '';
end;

destructor TPointCharge.done;
begin
   DrawPoints(off);
end;

procedure TPointCharge.Draw;
begin
   FPoint(a,a);
end;

function TPointCharge.Shape : ShapeType;
begin
   Shape := point_;
end;

procedure TPointCharge.MapPlate(mark : MapMarkProc);
var
   p,q : pointType;
begin
   MatrixCoords(p,q);
   mark(p.x,p.y);
end;

procedure TPointCharge.Map;
begin
   CurrentValue := value;
   MapPlate(markQ);
end;

FUNCTION TPointCharge.Picked(p : pointType) : boolean;
begin
   Picked := samePt(p,a);
end;

FUNCTION TPointCharge.Reshape(e : pointType) : boolean;
begin
   IF samePt(a,e) THEN
   BEGIN
      with parent^.port do MouseBorders(x1,y1,x2,y2);
      drawPoints(off);
      DrawRubberBand(FPoint,a,a,black,false);
      drawPoints(on);
      reshape := true;
      MouseBorders(0,0,GetMaxX,GetMaxY);
   END
   ELSE reshape := false;
end;

FUNCTION TPointCharge.Inside (r : rectType) : Boolean;
begin
   Inside := ptInside(a,r);
end;

PROCEDURE TPointCharge.DrawPoints(noErase : boolean);
begin
   SetTextJustify(CenterText,Centertext);
   IF noErase THEN SetColor (yellow) ELSE setColor(black);
   HideMouse;
   outTextXY(a.x,a.y, chr(219));
   Draw;
   ShowMouse;
   SetTextJustify(LeftText,TopText);
   SetColor(whitte);
end;

Procedure TPointCharge.MoveAndDraw(d : pointType);
begin
   addPt(d,a,d);
   FPoint(d,d);
end;

procedure TPointCharge.ReadFunction;
begin  {dummy procedure}  end;

function TPointCharge.GetStepSize : integer;
begin GetStepSize := 0; {dummy function} end;

procedure TPointCharge.MatrixCoords(var p,q : pointType);
begin
   p.x := round(a.x/parent^.dis.x) +1;      { add 1 - matrix starts from (1,1)}
   p.y := round(a.y/parent^.dis.y) +1;
   q.x := round(b.x/parent^.dis.x) +1;
   q.y := round(b.y/parent^.dis.y) +1;
end;

{---------------------------- LinePlate --------------------------------}

procedure FLine1(a,b : pointType); far;
begin
   line(a.x,a.y,b.x,b.y);
   counter(a,b);
end;

procedure FLine2(a,b : pointType); far;
begin
   line( 2*a.x-b.x, 2*a.y-b.y, b.x, b.y);
   counter(a,b);
end;


constructor TLinePlate.init(p,q : PointType; parent_ : PlatesPtr;
                            Value_ : real);
begin
   a := p;
   b := q;
   value  := value_;
   parent := parent_;
   select := true;
   next   := parent^.root;
   functn := '';
   if ((a.y<>b.y) and (a.y>b.y)) or ((a.y=b.y) and (a.x>b.x)) then
     swapPoints(a,b);
end;

destructor TLinePlate.done;
begin
   DrawPoints(off);
end;


procedure TLinePlate.Draw;
begin
   FLine1(a,b);
end;

function TLinePlate.Shape : ShapeType;
begin
   Shape := line_;
end;


PROCEDURE TLinePlate.DrawPoints(noErase : boolean);
{ Draws points on vertxes, or erase them if noErase=false.}
BEGIN
   SetTextJustify(CenterText,Centertext);
   IF noErase THEN SetColor (yellow) ELSE setColor(black);
   HideMouse;
   OutTextXY(a.x,a.y,chr(219));
   OutTextXY(b.x,b.y,chr(219));
   Draw;
   ShowMouse;
   SetColor(whitte);
   SetTextJustify(LeftText,TopText);
END;


procedure TLinePlate.MapPlate(mark : MapMarkProc);
{ Uses Bresenham's Line Algorithm.}
var
   p,q : pointType;
begin
   MatrixCoords(p,q);
   NumberPoints := 0;
   BresenhamLine(p,q,mark);
end;

procedure TLinePlate.Map;
var
   i : integer;
begin
   CurrentValue := value;
   if (functn<>'') then
   begin
       MapPlate(CountPoints);
       vector.init(NumberPoints);
       if FunctToVector(functn,vector,parent^.PlateSymbol) then
        MapPlate(MarkVFun);
       vector.free;
   end
   else MapPlate(markV);
end;


FUNCTION TLinePlate.Reshape(e : pointType) : boolean;
{ If "e" if one of control points then figure is reshaped.}
var
   c,aOld : pointType;
BEGIN
   IF (samePt(a,e) OR samePt(b,e)) THEN
   BEGIN
      with parent^.port do MouseBorders(x1,y1,x2,y2);
      drawPoints(off);
      IF samePt(b,e) THEN begin c:=a; a:=b; b:=c; end;
      if parent^.DrawFromCenter then
      begin
         aOld := a;
         c.x  := (a.x+b.x) div 2;
         c.y  := (a.y+b.y) div 2;
         DrawRubberBand(FLine2,c,a,black,true);
         c.x := 2*c.x-a.x;
         c.y := 2*c.y-a.y;
         if ptInside(c,parent^.port) then b:=c else a:=aOld;
      end
      ELSE DrawRubberBand(FLine1,b,a,black,true);
      drawPoints(on);
      reshape := true;
      MouseBorders(0,0,GetMaxX,GetMaxY);
   END
   ELSE reshape := false;
END;


FUNCTION TLinePlate.Picked(p : pointType) : boolean;
{ Checks if point "p" belongs to figure. gravitation=3 points.}
{ fast point on line test, working only on integers. }
VAR
   d    : pointType;
BEGIN
   d.x:=abs(a.x-b.x);   d.y:=abs(a.y-b.y);
   Picked := (abs(2*p.x-a.x-b.x)<=d.x) AND (abs(2*p.y-a.y-b.y)<=d.y) AND
    (abs(1.0*(a.x-b.x)*(p.y-b.y) -1.0*(a.y-b.y)*(p.x-b.x)) <= (d.x+d.y));
END;


Procedure TLinePlate.MoveAndDraw(d : pointType);
var
   a1,b1 : pointType;
begin
   addPt(a1,a,d);
   addPt(b1,b,d);
   FLine1(a1,b1);
end;

FUNCTION TLinePlate.Inside (r : rectType) : Boolean;
{ Checks if figure if inside "r" recrangle.}
BEGIN
   inside := (PtInside(a,r) AND PtInside(b,r));
END;


procedure TLinePlate.ReadFunction;
var
   screen  : TInputScreen;
begin
   with screen do
   begin
      init;
      DefineInputPort(0.1,0.9,0.4,0.6);
      LoadLine(' Parametric Function of Potential distribution on line plate');
      LoadLine('V(t)="                                            '
              +'            "');
      LoadLine(' Where 0 '+chr(243)+' t '+chr(243)+' 1;   t=0 at upper-left end');
      LoadLine('              [  Ok  ]   [Cancel]   [ Help ]');
      AcceptInputScreen(screen);
      done;
   end;
end;

procedure TLinePlate.AcceptInputScreen(var screen : TInputScreen);
var
   flag    : boolean;
   parser  : TParser;
   size,i  : integer;
   vec     : dVector;
   ParserHelp : HelpScrType;
begin
   parser.init;
   with screen do
   repeat
      AboutParser(ParserHelp);
      SetHelpScreen(ParserHelp);
      SetString(1,functn);
      AcceptScreen;
      functn := getString(1);
      flag   := parser.parse('t','o','t','t',functn);
      if canceled then functn := ''
      else
       if not flag then
       announce('Error: Cannot evaluate this expression. Please try again.')
       else
       begin
          MapPlate(CountPoints);
          vec.init(NumberPoints);
          flag := FunctToVector(functn,vec,parent^.PlateSymbol);
          vec.free;
       end;
   until flag or canceled;
end;

{---------------------------- RectPlate --------------------------------}

procedure FRect1(a,b : pointType); far;
begin
   Rectangle(a.x,a.y,b.x,b.y);
   counter(a,b);
end;

procedure FRect2(a,b : pointType); far;
begin
   Rectangle( 2*a.x-b.x, 2*a.y-b.y, b.x, b.y);
   counter(a,b);
end;

constructor TRectPlate.init(p,q : PointType; parent_ : PlatesPtr;
                            Value_ : real);
begin
   a := p;
   b := q;
   value  := value_;
   parent := parent_;
   select := true;
   next   := parent^.root;
   functn := '';
   if (a.x>b.x) then SwapIntegers(a.x,b.x);
   if (a.y>b.y) then SwapIntegers(a.y,b.y);
end;

destructor TRectPlate.done;
begin
   DrawPoints(off);
end;

procedure TRectPlate.Draw;
begin
   FRect1(a,b);
end;

function TRectPlate.Shape : ShapeType;
begin
   Shape := rect_;
end;


procedure TRectPlate.MapPlate(mark : MapMarkProc);
var
   i,t : integer;
   p,q : pointType;
begin
   MatrixCoords(p,q);
   t := (q.y+p.y) div 2;
   {go counterclockwise from the middle of right edge}
   NumberPoints := 0;
   for i :=   t   downto p.y do mark( q.x, i);
   for i := q.x-1 downto p.x do mark( i, p.y);
   for i := p.y+1     to q.y do mark( p.x, i);
   for i := p.x+1     to q.x do mark( i, q.y);
   for i := q.y-1 downto t+1 do mark( q.x, i);
end;


PROCEDURE TRectPlate.DrawPoints(noErase : boolean);
{ Draws points on vertxes, or erase them if noErase=false.}
BEGIN
   SetTextJustify(CenterText,Centertext);
   IF noErase THEN SetColor(yellow) ELSE setColor(black);
   HideMouse;
   outTextXY(a.x,a.y, chr(219));
   outTextXY(b.x,b.y, chr(219));
   outTextXY(a.x,b.y, chr(219));
   outTextXY(b.x,a.y, chr(219));
   Draw;
   ShowMouse;
   SetColor(whitte);
   SetTextJustify(LeftText,TopText);
END;




FUNCTION TRectPlate.Reshape(e : pointType) : boolean;
{ If "e" if one of control points then figure is reshaped.}
VAR
   c,bOld : pointType;
BEGIN
   IF ((3>=abs(e.x-a.x)) OR (3>=abs(e.x-b.x))) AND
      ((3>=abs(e.y-a.y)) OR (3>=abs(e.y-b.y))) THEN
   BEGIN
      with parent^.port do MouseBorders(x1,y1,x2,y2);
      drawPoints(off);
      IF (2>=abs(e.x-a.x)) OR samePo( e, (a.x+b.x) DIV 2, a.y) THEN
       BEGIN c:=b; b:=a; a:=c END;  {Swich a add b to reduce number of cases.}
      IF not samePt(e,b) THEN BEGIN c.y:=a.y; a.y:=b.y; b.y:=c.y; END;
      if parent^.DrawFromCenter then
      begin
         bOld := a;
         c.x := (a.x+b.x) div 2;
         c.y := (a.y+b.y) div 2;
         DrawRubberBand(FRect2,c,b,black,true);
         c.x := 2*c.x-b.x;
         c.y := 2*c.y-b.y;
         if ptInside(c,parent^.port) then a:=c else b:=bOld;
      end
      ELSE DrawRubberBand(FRect1,a,b,black,true);
      drawPoints(on);
      reshape := true;
      MouseBorders(0,0,GetMaxX,GetMaxY);
   END
   ELSE reshape := false;
END;


FUNCTION TRectPlate.Picked(p : pointType) : boolean;
{ Checks if point "p" belongs to figure. gravitation=3 points.}
BEGIN
   picked := (( (abs(a.y-b.y)>=abs(2*p.y-a.y-b.y)) AND
               ((3>=abs(p.x-a.x)) OR (3>=abs(p.x-b.x))) ) OR
              ( (abs(a.x-b.x)>=abs(2*p.x-a.x-b.x)) AND
               ((3>=abs(p.y-a.y)) OR (3>=abs(p.y-b.y))) ));
END;

Procedure TRectPlate.MoveAndDraw(d : pointType);
var
   a1,b1 : pointType;
begin
   addPt(a1,a,d);
   addPt(b1,b,d);
   FRect1(a1,b1);
end;

procedure TRectPlate.ReadFunction;
var
   screen  : TInputScreen;
begin
   with screen do
   begin
      init;
      DefineInputPort(0.1,0.9,0.38,0.62);
      LoadLine('Parametric Function of Potential distrib. on rectangular plate');
      LoadLine('V(t)="                                            '
              +'            "');
      LoadLine('Where 0 '+chr(243)+' t '+chr(243)+' 1');
      LoadLine('t=0 at the center of right edge, and increases counterclockwise');
      LoadLine('             [  Ok  ]   [Cancel]   [ Help ]');
      AcceptInputScreen(screen);
      done;
  end;
end;



{---------------------------- OvalPlate --------------------------------}

procedure MarkOvalVFun(x,y : integer);
var
   number : integer;
begin
   case (NumberPoints mod 4) of
    0: number :=   StepSize - (NumberPoints div 4);
    1: number :=   StepSize + (NumberPoints div 4);
    2: number := 3*StepSize - (NumberPoints div 4);
    3: number := 3*StepSize + (NumberPoints div 4);
   end;
   inc(NumberPoints);
   Potential.put( y, x, vector.value(number));
end;

procedure FOval1(a,b: pointType); far;
begin
   ellipse( (a.x+b.x) div 2, (a.y+b.y) div 2, 0, 360,
            abs(b.x-a.x) div 2, abs(b.y-a.y) div 2);
   counter(a,b);
end;

procedure FOval2(a,b: pointType); far;
begin
   ellipse( a.x, a.y, 0, 360, abs(b.x-a.x), abs(b.y-a.y));
   counter(a,b);
end;

constructor TOvalPlate.init(p,q : PointType; parent_ : PlatesPtr;
                            Value_ : real);
begin
   a := p;
   b := q;
   value  := value_;
   parent := parent_;
   select := true;
   next   := parent^.root;
   functn := '';
end;

destructor TOvalPlate.done;
begin
   DrawPoints(off);
end;

procedure TOvalPlate.Draw;
begin
   FOval1(a,b);
end;

function TOvalPlate.Shape : ShapeType;
begin
   Shape := oval_;
end;

procedure TOvalPlate.MapPlate(mark : MapMarkProc);
{ Uses Bresenham's Elipse Algorithm.}
var
   p,q : pointType;
begin
   MatrixCoords(p,q);
   NumberPoints := 0;
   BresenhamEllipse(p,q, mark);
end;

procedure TOvalPlate.Map;
var
   i : integer;
begin
   CurrentValue := value;
   if (functn<>'') then
   begin
      MapPlate(CountPoints);
      vector.init(NumberPoints);   {-4 becouse 4 points are counted twice}
      StepSize:= NumberPoints div 4;
      if FunctToVector(functn,vector,parent^.plateSymbol) then
       MapPlate(MarkOvalVFun);
      vector.free;
   end
   else  MapPlate(markV);
end;


PROCEDURE TOvalPlate.DrawPoints(noErase : boolean);
{ Draws points on vertxes, or erase them if noErase=false.}
BEGIN
   SetTextJustify(CenterText,Centertext);
   IF noErase THEN SetColor(yellow) ELSE setColor(black);
   if noErase then SetLineStyle(DottedLn,0,1)
   else SetLineStyle(SolidLn,0,1);
   HideMouse;
   Rectangle(a.x,a.y,b.x,b.y);
   outTextXY(a.x,a.y, chr(219));
   outTextXY(b.x,b.y, chr(219));
   outTextXY(a.x,b.y, chr(219));
   outTextXY(b.x,a.y, chr(219));
   SetLineStyle(SolidLn,0,1);
   Draw;
   ShowMouse;
   SetColor(whitte);
   SetTextJustify(LeftText,TopText);
END;

FUNCTION TOvalPlate.Reshape(e : pointType) : boolean;
{ If "e" if one of control points then figure is reshaped.}
VAR
   c,bOld : pointType;
BEGIN
   IF ((3>=abs(e.x-a.x)) OR (3>=abs(e.x-b.x))) AND
      ((3>=abs(e.y-a.y)) OR (3>=abs(e.y-b.y))) THEN
   BEGIN
      with parent^.port do MouseBorders(x1,y1,x2,y2);
      drawPoints(off);
      IF (2>=abs(e.x-a.x)) OR samePo( e, (a.x+b.x) DIV 2, a.y) THEN
       BEGIN c:=b; b:=a; a:=c END;  {Swich a add b to reduce number of cases.}
      IF not samePt(e,b) THEN BEGIN c.y:=a.y; a.y:=b.y; b.y:=c.y; END;
      if parent^.DrawFromCenter then
      begin
         bOld := b;
         c.x  := (a.x+b.x) div 2;
         c.y  := (a.y+b.y) div 2;
         DrawRubberBand(FOval2,c,b,black,false);
         c.x := 2*c.x-b.x;
         c.y := 2*c.y-b.y;
         if ptInside(c,parent^.port) then a:=c else b:=bOld;
      end
      ELSE DrawRubberBand(FOval1,a,b,black,false);
      drawPoints(on);
      reshape := true;
      MouseBorders(0,0,GetMaxX,GetMaxY);
   END
   ELSE reshape := false;
END;

Function TOvalPlate.Picked(p : pointType) : boolean;
var
   xcen,ycen,xrad,yrad,dist,angle,radius : real;
begin
   xcen   := (a.x+b.x)/2;
   ycen   := (a.y+b.y)/2;
   xrad   := (a.x-b.x)/2;
   yrad   := (a.y-b.y)/2;
   dist   := sqrt( sqr(p.x-xcen) + sqr(p.y-ycen));
   angle  := arctan2( p.x-xcen, p.y-ycen);
   radius := sqrt( 1 / ((sqr(cos(angle))/sqr(xrad)) +
                        (sqr(sin(angle))/sqr(yrad))) );
   Picked := (abs(radius-dist)<=3);
end;

Procedure TOvalPlate.MoveAndDraw(d : pointType);
var
   a1,b1 : pointType;
begin
   addPt(a1,a,d);
   addPt(b1,b,d);
   FOval1(a1,b1);
end;

procedure TOvalPlate.ReadFunction;
var
   screen  : TInputScreen;
begin
   with screen do
   begin
      init;
      DefineInputPort(0.1,0.9,0.38,0.62);
      LoadLine('Parametric Function of Potential distrib. on eliptical plate');
      LoadLine('V(t)="                                            '
              +'            "');
      LoadLine('Where 0 '+chr(243)+' t '+chr(243)+' 1');
      LoadLine('t=0 at the extrime right, and increases counterclockwise');
      LoadLine('             [  Ok  ]   [Cancel]   [ Help ]');
      AcceptInputScreen(screen);
      done;
   end;
end;


{---------------------------- Line Of Charges ------------------------------}


procedure Step_MarkQ (x,y : integer);
begin
   if ((NumberPoints mod StepSize) = 0) then
    charges.put(y,x,CurrentValue);
   inc(NumberPoints);
end;

procedure MarkQFun (x,y : integer);
var
   c : real;
begin
   if ((NumberPoints mod StepSize) = 0) then
   begin
      c := CurrentValue * vector.value((NumberPoints div StepSize)+1);
      charges.put( y, x, c);
   end;
   inc(NumberPoints);
end;


Procedure MarkCharge (x,y : integer);
{ used to draw line of charges}
begin
   if ((NumberPoints mod StepSize) = 0) then
      circle( round((x-1)*dist.x), round((y-1)*dist.y), 2);
   inc(NumberPoints);
end;


procedure FQLine1(a,b : pointType); far;
begin
   SetLineStyle(DottedLn,0,ThickWidth);
   line(a.x,a.y,b.x,b.y);
   SetLineStyle(SolidLn,0,NormWidth);
   counter(a,b);
end;


procedure FQLine2(a,b : pointType); far;
begin
   SetLineStyle(DottedLn,0,ThickWidth);
   line( 2*a.x-b.x, 2*a.y-b.y, b.x, b.y);
   SetLineStyle(SolidLn,0,NormWidth);
   counter(a,b);
end;


constructor TLineOfCharges.init(p,q : PointType; parent_ : PlatesPtr;
                                Value_ : real; step_ : integer);
begin
   a := p;
   b := q;
   value  := value_;
   parent := parent_;
   step   := step_;
   select := true;
   next   := parent^.root;
   functn := '';
   if ((a.y<>b.y) and (a.y>b.y)) or ((a.y=b.y) and (a.x>b.x)) then
     swapPoints(a,b);
   if step=0 then AskForNumOfCharges;
end;


destructor TLineOfCharges.done;
begin
   DrawPoints(off);
end;


procedure TLineOfCharges.Draw;
begin
   StepSize := Step;
   MapPlate(MarkCharge)
end;


function TLineOfCharges.Shape : ShapeType;
begin
   Shape := qline;
end;


procedure TLineOfCharges.Map;
var
   i : integer;
begin
   MapPlate(CountPoints);
   CurrentValue := value;
   StepSize     := Step;
   if (functn<>'') then
   begin
      vector.init((NumberPoints div Step)+1);
      if FunctToVector(functn,vector,parent^.PlateSymbol) then
       MapPlate(MarkQFun);
      vector.free;
   end
   else MapPlate(Step_markQ);
end;


procedure TLineOfCharges.AskForNumOfCharges;
var
   QNumScreen : TInputScreen;
   n,m,num,total : integer;
   str : string;
   steps : array[0..10] of integer;
begin
   MapPlate(CountPoints);
   total := NumberPoints;
   n     := 1;
   m     := 0;
   steps[0] := 2;
   str   := '';
   while (total div n>2) and (n<=10) do
   begin
      num := (total-1) div n + 1;
      if ((total-1) div steps[m] +1=num) then steps[m]:=n
      else
      begin
         str := str +'#1' +numstr(num,3,0) +'  ';
         inc(m);
         steps[m] := n;
      end;
      inc(n);
   end;
   if (n>2) then
    with QNumScreen do
    begin
       init;
       DefineInputPort(0.07, 0.93, 0.42, 0.58);
       LoadLine('      Choose number of charges you want in the row ');
       LoadLine(str);
       loadLine('                   [  Ok  ]  [Cancel]');
       AcceptScreen;
       if canceled then step:=1 else step:=steps[GetRadioButton('1')];
       done;
    end
   else step := 1;
end;


FUNCTION TLineOfCharges.Reshape(e : pointType) : boolean;
{ If "e" if one of control points then figure is reshaped.}
var
   c,aOld : pointType;
BEGIN
   IF (samePt(a,e) OR samePt(b,e)) THEN
   BEGIN
      with parent^.port do MouseBorders(x1,y1,x2,y2);
      drawPoints(off);
      IF samePt(b,e) THEN begin c:=a; a:=b; b:=c; end;
      if parent^.DrawFromCenter then
      begin
         aOld := a;
         c.x  := (a.x+b.x) div 2;
         c.y  := (a.y+b.y) div 2;
         DrawRubberBand(FQLine2,c,a,black,true);
         c.x := 2*c.x-a.x;
         c.y := 2*c.y-a.y;
         if ptInside(c,parent^.port) then b:=c else a:=aOld;
      end
      ELSE DrawRubberBand(FQLine1,b,a,black,true);
      MouseBorders(0,0,GetMaxX,GetMaxY);
      AskForNumOfCharges;
      drawPoints(on);
      reshape := true;
   END
   ELSE reshape := false;
END;


Procedure TLineOfCharges.MoveAndDraw(d : pointType);
var
   a1,b1 : pointType;
begin
   addPt(a1,a,d);
   addPt(b1,b,d);
   FQLine1(a1,b1);
end;


procedure TLineOfCharges.ReadFunction;
var
   screen  : TInputScreen;
begin
   with screen do
   begin
      init;
      DefineInputPort(0.1,0.9,0.4,0.6);
      LoadLine('    Parametric Function of charge values in row of charges');
      LoadLine('Q(t)="                                            '
              +'            "');
      LoadLine(' Where 0 '+chr(243)+' t '+chr(243)+' 1;  and t=0 at upper-left end');
      LoadLine('             [  Ok  ]   [Cancel]   [ Help ]');
      AcceptInputScreen(screen);
      done;
   end;
end;

function TLineOfCharges.GetStepSize : integer;
begin
   GetStepSize := step;
end;

procedure TLineOfCharges.AcceptInputScreen(var screen : TInputScreen);
var
   flag    : boolean;
   parser  : TParser;
   size,i  : integer;
   vec     : dVector;
   ParserHelp : HelpScrType;
begin
   parser.init;
   with screen do
   repeat
      AboutParser(ParserHelp);
      SetHelpScreen(ParserHelp);
      SetString(1,functn);
      AcceptScreen;
      functn := getString(1);
      flag   := parser.parse('t','o','t','t',functn);
      if canceled then functn := ''
      else
       if not flag then
       announce('Error: Cannot evaluate this expression. Please try again.')
       else
       begin
          MapPlate(CountPoints);
          vec.init(NumberPoints div step);
          flag := FunctToVector(functn,vec,Parent^.PlateSymbol);
          vec.free;
       end;
   until flag or canceled;
end;

{---------------------------- 2 Parallel Plates ----------------------------}


procedure F2Plates1(a,b: pointType); far;
begin
   line(a.x,a.y,b.x,a.y);
   line(a.x,b.y,b.x,b.y);
   counter(a,b);
end;


procedure F2Plates2(a,b: pointType); far;
begin
   a.x := 2*a.x-b.x;
   a.y := 2*a.y-b.y;
   F2Plates1(a,b);
end;


constructor T2ParallelPlates.init(p,q : PointType; parent_ : PlatesPtr;
                                  Value_ : real);
begin
   a := p;
   b := q;
   value  := value_;
   parent := parent_;
   select := true;
   next   := parent^.root;
   functn := '';
   if (a.x>b.x) then SwapIntegers(a.x,b.x);
end;


destructor T2ParallelPlates.done;
begin
   DrawPoints(off);
end;


procedure T2ParallelPlates.Draw;
begin
   F2Plates1(a,b);
end;


function T2ParallelPlates.Shape : ShapeType;
begin
   Shape := IIPlts;
end;


procedure T2ParallelPlates.mapPlate(mark : MapMarkProc);
var
   i,x1,x2,y1,y2 : integer;
   p,q : pointType;
begin
   MatrixCoords(p,q);
   NumberPoints := 0;
   for i := p.x to q.x do mark(i,p.y);
   CurrentValue := -CurrentValue;
   NumberPoints := 0;
   for i := p.x to q.x do mark(i,q.y);
   CurrentValue := -CurrentValue;
end;


FUNCTION T2ParallelPlates.Reshape(e : pointType) : boolean;
{ If "e" if one of control points then figure is reshaped.}
VAR
   c,bOld : pointType;
BEGIN
   IF ((3>=abs(e.x-a.x)) OR (3>=abs(e.x-b.x))) AND
      ((3>=abs(e.y-a.y)) OR (3>=abs(e.y-b.y))) THEN
   BEGIN
      with parent^.port do MouseBorders(x1,y1,x2,y2);
      drawPoints(off);
      IF (2>=abs(e.x-a.x)) OR samePo( e, (a.x+b.x) DIV 2, a.y) THEN
       BEGIN c:=b; b:=a; a:=c END;  {Swich a add b to reduce number of cases.}
      IF not samePt(e,b) THEN BEGIN c.y:=a.y; a.y:=b.y; b.y:=c.y; END;
      if parent^.DrawFromCenter then
      begin
         bOld := a;
         c.x := (a.x+b.x) div 2;
         c.y := (a.y+b.y) div 2;
         DrawRubberBand(F2Plates2,c,b,black,true);
         c.x := 2*c.x-b.x;
         c.y := 2*c.y-b.y;
         if ptInside(c,parent^.port) then a:=c else b:=bOld;
      end
      ELSE DrawRubberBand(F2Plates1,a,b,black,true);
      IF (a.x>b.x) THEN BEGIN c:=b; b:=a; a:=c END;
      drawPoints(on);
      reshape := true;
      MouseBorders(0,0,GetMaxX,GetMaxY);
   END
   ELSE reshape := false;
END;


Function T2ParallelPlates.Picked(p : pointType) : boolean;
{ Checks if point "p" belongs to figure. gravitation=3 points.}
BEGIN
   picked :=  ( (abs(a.x-b.x)>=abs(2*p.x-a.x-b.x)) AND
               ((3>=abs(p.y-a.y)) OR (3>=abs(p.y-b.y))) );
end;


Procedure T2ParallelPlates.MoveAndDraw(d : pointType);
var
   a1,b1 : pointType;
begin
   addPt(a1,a,d);
   addPt(b1,b,d);
   F2Plates1(a1,b1);
end;


procedure T2ParallelPlates.ReadFunction;
var
   screen  : TInputScreen;
begin
   with screen do
   begin
      init;
      DefineInputPort(0.09,0.91,0.38,0.62);
      LoadLine('Parametric Function of potential on upper plate of parallel plate ');
      LoadLine('capacitor. Potential on lower plate is inverse of top plate.');
      LoadLine('V(t)="                                            '
              +'            "');
      LoadLine(' Where 0 '+chr(243)+' t '+chr(243)+' 1;  and t=0 at left end');
      LoadLine('             [  Ok  ]   [Cancel]   [ Help ]');
      AcceptInputScreen(screen);
      done;
   end;
end;


{----------------------- Plates Private Procedures -----------------------}

procedure TPlates.DrawPointsOfAllSelected(flag : boolean);
{Draws all selected plates}
var
   pPtr  : PlatePtr;
begin
   HideMouse;
   pPtr  := root;
   while pPtr<>nil do
   begin
      if pPtr^.select then pPtr^.DrawPoints(flag);
      pPtr := pPtr^.next;
   end;
   ShowMouse;
end;

procedure TPlates.SelectAllInside(r : rectType; flag : boolean);
{Sets all select flags to false}
var
   pPtr  : PlatePtr;
begin
   HideMouse;
   pPtr  := root;
   while pPtr<>nil do
   begin
      if pPtr^.inside(r) then
      begin
         pPtr^.select := flag;
         pPtr^.DrawPoints(flag);  
      end
      else
      begin
         pPtr^.select := not flag;
         pPtr^.DrawPoints(not flag); 
      end;
      pPtr := pPtr^.next;
   end;
   DrawAll;
   ShowMouse;
end;

function TPlates.Select(e : pointType) : boolean;
{check if any figure was selected.}
var
   pPtr : PlatePtr;
   flag,RightMouseButton : boolean;
   a,button : integer;
begin
   pPtr  := root;
   flag  := false;
   MouseGlobalPosn(a,a,button);    {"a" is a dummy}
   RightMouseButton := (button=2);
   while pPtr<>nil do
   begin
       if pPtr^.Picked(e) then
       begin
          flag := true;                           {if shifted then add}
          if not RightMouseButton then SelectAllInside(Port,off);
          pPtr^.select := true;
       end;
      pPtr := pPtr^.next;
   end;
   Select := flag;
end;

function TPlates.SelectedPlate : platePtr;
var
   pPtr,plate : PlatePtr;
   numSelected : integer;
begin
   pPtr  := root;
   numSelected := 0;
   while pPtr<>nil do
   begin
      if pPtr^.select then
      begin
         plate := pPtr;
         inc(numSelected);
      end;
      pPtr := pPtr^.next;
   end;
   if (numSelected>1) then plate := nil else
   if (numSelected=0) then
    if (root<>nil) then
    begin
       root^.select := true;
       root^.DrawPoints(on);
       plate := root;
    end
    else plate := nil;
   SelectedPlate := plate;
end;

function TPlates.Reshape(e : pointType) : boolean;
{check if any figure was reshaped.}
var
   pPtr : PlatePtr;
   flag : boolean;
begin
   pPtr  := root;
   flag  := false;
   while pPtr<>nil do
   begin
      if pPtr^.select then
       flag := flag or pPtr^.reshape(e);
      pPtr := pPtr^.next;
   end;
   if flag then DrawAll;                    {redraw all figures}
   Reshape := flag;
end;

procedure TPlates.CreatePlate(a,b : pointType; Shape : ShapeType;
                       value : real; step : integer);
var
   pPtr : ^TPointCharge;
   rPtr : ^TRectPlate;
   oPtr : ^TOvalPlate;
   lPtr : ^TLinePlate;
   qPtr : ^TLineOfCharges;
   tPtr : ^T2ParallelPlates;
begin
   if root<>nil then root^.select := false;
   case Shape of
     point_ : begin   new(pPtr,init(a,  @self,value)); root:=pPtr;   end;
     rect_  : begin   new(rPtr,init(a,b,@self,value)); root:=rPtr;   end;
     line_  : begin   new(lPtr,init(a,b,@self,value)); root:=lPtr;   end;
     oval_  : begin   new(oPtr,init(a,b,@self,value)); root:=oPtr;   end;
     IIPlts : begin   new(tPtr,init(a,b,@self,value)); root:=tPtr;   end;
     qLine  : begin   new(qPtr,init(a,b,@self,value,step)); root:=qPtr; end;
   end;
   currentPlate := root;
end;

procedure TPlates.MoveAllSelected(d : pointType);

   procedure figure(a : pointType);
   var
      pPtr : PlatePtr;
   begin
      pPtr  := root;
      while pPtr<>nil do
      begin
         if pPtr^.select then pPtr^.MoveAndDraw(a);
         pPtr := pPtr^.next;
      end;
   end;

const
   BkColor : integer = black;
   color   : integer = whitte;
VAR
   x,y,button,i : integer;
   vp   : ViewPortType;
   c    : pointType;
   pPtr : PlatePtr;
BEGIN
   i:=0;
   REPEAT                              {wait fraction of second}
      mouseGlobalPosn(x,x,button);     {x is a dummy}
      inc(i);
      delay(4);
   UNTIL (button=0) OR (i=50);
   IF i=50 THEN                        {if mouse still clicked}
   begin
      DrawPointsOfAllSelected(off);
      c.x := 0;
      c.y := 0;
      REPEAT
         SetColor(BkColor);
         HideMouse;
         figure(c);
         MouseGlobalPosn(x,y,button);
         c.x := x-port.x1-d.x; c.y := y-port.y1-d.y;
         SetColor(color);
         figure(c);
         ShowMouse;
         delay(10);
      UNTIL button = 0;
      pPtr  := root;                     {move all selected}
      while pPtr<>nil do
      begin
         with pPtr^ do
          if select then
          begin
             addPt(a,c,a);
             addPt(b,c,b);
          end;
         pPtr := pPtr^.next;
      end;
      DrawAll;
   end;
end;


{----------------------- Plates Public Procedures ------------------------}

procedure TPlates.Init;
begin
   with views[ViewPortNumber] do
    SetRect(port,vx1,vy1,vx2,vy2);
   root           := nil;
   CurrentPlate   := nil;
   modifyOn       := false;
   DrawFromCenter := true;
   AskForV        := false;
   counterOn      := false;
   PlateSymbol    := random*1E-10;
end;

procedure TPlates.done;
var
   pPtr,nPtr  : PlatePtr;
begin
   pPtr  := root;
   while pPtr<>nil do
   begin
      nPtr := pPtr^.next;
      pPtr^.done;
      dispose(pPtr);
      pPtr := nPtr;
   end;
   root         := nil;
   CurrentPlate := nil;
   modifyOn     := false;
end;

procedure TPlates.RemoveAllSelected(var Shape : ShapeType; var value : real);
{disposes all currently selected plates}
var
   pPtr,tPtr,rPtr  : PlatePtr;
   i : integer;
begin
   pPtr := root;
   tPtr := root;
   while (pPtr<>nil) do
   begin
      if pPtr^.select then
      begin
         if (pPtr=root) then
         begin
            if (root^.next=nil) then tPtr:=nil
            else tPtr:=pPtr^.next;
            dispose(pPtr,done);
            root := tPtr;
         end
         else
         begin
            tPtr^.next := pPtr^.next;
            dispose(pPtr,done);
         end;
         pPtr := tPtr;
      end
      else
      begin
         tPtr := pPtr;
         pPtr := pPtr^.next;
      end;
   end;
   if root<>nil then
   begin
      DrawAll;
      root^.select := true;
      root^.DrawPoints(on);
      shape := root^.shape;                {those 2 to adjust slider + icons}
      value := root^.value
   end
   else
   begin
      shape    := nil_;
      ModifyOn := false;
   end;
end;


procedure TPlates.DrawAll;
{Draws all plates}
var
   pPtr  : PlatePtr;
begin
   setColor(whitte);
   HideMouse;
   pPtr  := root;
   while pPtr<>nil do
   begin
      pPtr^.Draw;
      pPtr := pPtr^.next;
   end;
   ShowMouse;
end;

procedure TPlates.ReMapAll;
{Does Remapping of all plates}
var
   pPtr  : PlatePtr;
begin
   pPtr  := root;
   while pPtr<>nil do
   begin
      pPtr^.map;
      pPtr := pPtr^.next;
   end;
end;


function TPlates.Read(Shape : ShapeType; value : real) : boolean;
{ Initialization procedure. Reads the figure.}
VAR
   x,y,button : integer;
   a,b : pointType;
   valid : boolean;
BEGIN
   a.x := event.x-port.x1;    {global to local}
   a.y := event.y-port.y1;
   valid := false;
   if modifyOn then           {when you call it first time after modify}
   begin
      SelectAllInside(port,off);      {erase all selection marks}
      root^.select := true;
      modifyOn := false
   end;
   if event.mouseClicked and PtInside(a,port) then
   begin
      a.x := round(round(a.x / dis.x) * dis.x);
      a.y := round(round(a.y / dis.y) * dis.y);
      if ptInside(a,port) then
      begin
         SetColor (whitte);
         counterOn := true;
         OutTextXY( 5,5,'dx=');
         OutTextXY(60,5,'dy=');
         if DrawFromCenter then
         begin
           case Shape of
            line_ : DrawRubberBand(FLine2,a,b,black,true);
            rect_ : DrawRubberBand(FRect2,a,b,black,true);
            oval_ : DrawRubberBand(FOval2,a,b,black,false);
            IIPlts: DrawRubberBand(F2Plates2,a,b,black,true);
            qLine : DrawRubberBand(FQLine2,a,b,black,true);
            point_: begin  b.x:=0; b.y:=0;  end;
           end;
           if (shape<>point_) then
           begin
              a.x := 2*a.x-b.x;
              a.y := 2*a.y-b.y;
           end;
         end
         else
         begin
           case Shape of
            line_ : DrawRubberBand(FLine1,a,b,black,true);
            rect_ : DrawRubberBand(FRect1,a,b,black,true);
            oval_ : DrawRubberBand(FOval1,a,b,black,false);
            IIPlts: DrawRubberBand(F2Plates1,a,b,black,true);
            qLine : DrawRubberBand(FQLine1,a,b,black,true);
            point_: begin  b.x:=0; b.y:=0;  end;
           end;
         end;
         EraseXY(5,5,13,Black);
         counterOn := false;
         b.x := round(round(b.x / dis.x) * dis.x);
         b.y := round(round(b.y / dis.y) * dis.y);
         valid := ((abs(a.x-b.x)>2*dis.x) or (abs(a.y-b.y)>2*dis.y)) and
                  ptInside(a,port) and ptInside(b,port);
      end;
      if valid then
      begin
         CreatePlate(a,b,Shape,value,0);
         if AskForV then root^.ReadFunction;
         if (Shape=oval_) then DrawAll
         else Begin  HideMouse; root^.Draw; ShowMouse;  end;
      end;
   end;
   read := valid;
END;


procedure TPlates.Modify(var Shape : ShapeType; var value : real);
var
   e,q : pointType;
   r : rectType;
   plate : PlatePtr;
   i : integer;
begin
   e.x := event.x - port.x1;                {global to local}
   e.y := event.y - port.y1;
   if not modifyOn then                     {when you call modify first time}
   begin
      DrawPointsOfAllSelected(on);
      modifyOn:=true;
   end;
   if event.mouseClicked and PtInside(e,port) then
   begin
      if not reshape(e) then
       if not Select(e) then                  {if no figure picked then..}
       BEGIN                                  {..draw rubber rectangle and..}
          with port do                        {..select all inside.}
           mouseBorders (x1,y1,x2,y2);
          SetColor(whitte);
          SetLineStyle(dottedLn,0,NormWidth);
          DrawRubberBand(FRect1,e,q,black,true);
          SetLineStyle(SolidLn,0,NormWidth);
          mouseBorders(0,0,GetMaxX,GetMaxY);
          if (e.x<>q.x) or (e.y<>q.y) then    {if rectangle drawn}
          begin
             with port do SetRect(r, e.x+x1,e.y+y1,q.x+x1,q.y+y1);
             SelectAllInside(r,on);
          end
       END
       ELSE MoveAllSelected(e);                {otherwise move all selected}
      DrawPointsOfAllSelected(on);
   end;
   Plate := SelectedPlate;
   If Plate<>nil then                          {if plate is selected}
   begin
      if plate<>CurrentPlate then              {and is a new one then help}
      begin                                    {reseting slider}
         Shape := plate^.shape;
         Value := plate^.value;
         CurrentPlate := plate;
      end
      else plate^.Value := value;              {else read value of slider}
   end
   else shape := nil_;
end;


function TPlates.ReadFromFile(var f : text; Var MaxRow,MaxCol : integer;
                               var ChargesOnly : boolean) : boolean;
var
   pPtr   : PlatePtr;
   s      : string;
   a,b    : pointType;
   value  : real;
   ValStr : string;
   shape  : ShapeType;
   valid  : boolean;
   i,step : integer;
begin
   ChargesOnly := true;
   valid       := false;
   Reset(f);
   Readln(f,s);
   if s='File created by Poisson.EXE program.' then
   begin 
      Readln(f);                  {Grid Size:}
      Readln(f,MaxRow,MaxCol);
      with port do
      begin
         dis.x := (x2-x1) / (maxCol-1);
         dis.y := (y2-y1) / (maxRow-1);
      end;
      dist := dis;
      Readln(f);                    {blank Line}
      Readln(f);                    {List of all plates:}
      while not eof(f) do
      begin
         Readln(f);               {blank line}
         Readln(f,s);
         case s[1] of
          'P': shape := Point_;
          'L': shape := Line_;
          'R': shape := Rect_;
          'E': shape := Oval_;
          'T': shape := IIPlts;
          'A': shape := qLine;
         end;
         ChargesOnly := ChargesOnly and (shape=point_) and (shape=qLine);
         ReadLn(f);               {Extension rectangle (x1,y1,x2,y2) :}
         Readln(f,a.x,a.y,b.x,b.y);
         Readln(f);               {Potential/Charge:}
         Readln(f,valStr);
         val(valStr,value,i);
         if (shape=qline) then
         begin
            Readln(f);               {Step size:}
            readln(f,step);
         end
         else step:=0;
         CreatePlate(a,b,shape,value,step);
         Readln(f);               {Distribution Function:}
         readln(f,root^.functn);
         if root^.functn='Constant' then root^.functn:='';
      end;
      valid := true;
   end
   else announce('Selected file was not created by this program.');
   close(f);
   ReadFromFile := valid;
end;

procedure TPlates.SaveToFile(var f : text);
var
   pPtr : PlatePtr;
   s    : string;
begin
   Rewrite(f);
   Writeln(f,'File created by Poisson.EXE program.');
   Writeln(f,'Grid Size:');
   with port do
    writeln(f, round((y2-y1)/dis.y+1),' ', round((x2-x1)/dis.x+1));
   writeln(f);
   writeln(f,'List of all plates:');
   pPtr  := root;
   while pPtr<>nil do
   begin
      with pPtr^ do
      begin
         writeln(f);
         case shape of
          Point_: s := 'Point Charge';
          Line_ : s := 'Linear Plate';
          Rect_ : s := 'Rectangular Plate';
          Oval_ : s := 'Elliptical Plate';
          IIPlts: s := 'Two Parallel Plates';
          qLine : s := 'A Line of Charges';
         end;
         writeln(f,s);
         writeLn(f,'Extension rectangle (x1,y1,x2,y2) :');
         writeln(f,a.x,' ',a.y,' ',b.x,' ',b.y);
         if (shape=point_) or (shape=QLine) then writeln(f,'Charge:')
         else writeln(f,'Potential:');
         writeln(f,NumStr(value,10,7));
         if (shape=qline) then
         begin
            writeln(f,'Step size:');
            writeln(f,GetStepSize);
         end;
         writeln(f,'Distribution Function:');
         if functn='' then writeln(f,'Constant') else writeln(f,functn);
      end;
      pPtr := pPtr^.next;
   end;
   close(f);
end;

procedure TPlates.PassVQMatrixes(var potential_,charges_ : DMatrix);
var
   maxRow, maxCol : integer;
begin
   potential_.GetSize(maxRow,maxCol);
   with port do
   begin
      dis.x := (x2-x1) / (maxCol-1);
      dis.y := (y2-y1) / (maxRow-1);
   end;
   dist := dis;
   potential := potential_;
   charges   := charges_;
end;

procedure TPlates.ChangeFunction;
var
   pPtr,plate : PlatePtr;
   numSelected : integer;
begin
   pPtr  := root;
   numSelected := 0;
   while pPtr<>nil do
   begin
      if pPtr^.select then
      begin
         plate := pPtr;
         inc(numSelected);
      end;
      pPtr := pPtr^.next;
   end;
   if (numSelected=1) then plate^.ReadFunction else
   announce('You can change the function of only one plate a time.');
end;

function TPlates.CheckIfChargesOnly : boolean;
var
   pPtr  : PlatePtr;
   ChargesOnly : boolean;
begin
   pPtr  := root;
   ChargesOnly := true;
   while pPtr<>nil do
   begin
      ChargesOnly := ChargesOnly and
		               ((pPtr^.shape=point_) or (pPtr^.shape=qLine));
      pPtr := pPtr^.next;
   end;
end;

end. {unit}
