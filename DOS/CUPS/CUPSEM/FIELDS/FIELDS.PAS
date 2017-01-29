                   {************************************}
                   {************************************}
                   {***      Program Fields.Pas      ***}
                   {***  Written by Jarek Tuszynski  ***}
                   {***       January 3, 1993        ***}
                   {************************************}
                   {************************************}
{$O+,F+}
PROGRAM fields;

USES Graph, Crt, CupsMupp, CUPS, CupsGrph, CupsFunc, CupsProc, CupsGui,
     CupsPars, FieldsTl;
TYPE
   PathType = (open,closed);
   InfoProc = procedure( str : string);
   TScalarField = object
      IntegralOn : boolean;
      constructor init;
      procedure UpDate;
      procedure ResizeMatrix;
      procedure DefinePlane;
      function  ReadFunction  : boolean;      virtual;
      procedure ChooseGraphs;                 virtual;
      procedure FieldProbe(p : point);        virtual;
      destructor done;                        virtual;
   private
      probe,active     : boolean;
      Scalar1,Scalar2  : DMatrix;
      Magn1,Magn2      : DMatrix;
      Vector1,Vector2  : VMatrix;
      Min,Max,Center,h : Point3D;
      Equation         : array[1..3]  of string;
      PlotNum          : array[1..4]  of integer;
      Plot             : array[1..4]  of TPlot;
      PlaneNorm        : integer;
      mousePosition    : point;
      Integral         : TIntegral;
      coordinates      : integer;
      small            : real;
      procedure initialize;
      procedure PrepareIntegral;
      function  CreateMatrix( func : string; var matrix : DMatrix) : boolean;
      Function  Find_Grad_Laplacian : boolean;
      procedure FindField;                    virtual;
      Procedure Resize(mRow,mCol : integer);  virtual;
      procedure PrepareProbe;                 virtual;
      procedure DrawPlots;                    virtual;
      procedure CalculatePathIntegral;        virtual;
      function  Evaluate( parser : TParser; x,y,z : real) : real;
   end;

   TVectorField = object(TScalarField)
      constructor init;
      function  ReadFunction : boolean;       virtual;
      procedure ChooseGraphs;                 virtual;
      procedure FieldProbe(p : point);        virtual;
      destructor done;                        virtual;
   private
      Function  Find_Div_Curl : boolean;
      procedure FindField;                    virtual;
      Procedure Resize(mRow,mCol : integer);  virtual;
      procedure PrepareProbe;                 virtual;
      procedure DrawPlots;                    virtual;
      procedure CalculatePathIntegral;        virtual;
   end;


Var
   QuitFlag    : boolean;
   Menu        : TMenu;
   HotKeys     : THotKeys;
   ScalarField : ^TScalarField;
   VectorField : ^TVectorField;
   Field       : ^TScalarField;

{-------------------------- General Use Procedures -----------------------}

PROCEDURE PrintScalar( X, Y: integer; Num : real);
VAR
   Row, Col, a : integer;
   s : string[30];
BEGIN
   s := '';
   FOR a := 1 TO 30 DO s := s + char(219);
   Col := Round((x - 0.6) * ColWidth);
   Row := Round((y - 0.6) * RowHt);
   SetColor(DarkGray);
   OutTextXY(Col, Row, s);
   SetColor(white);
   OutTextXY(Col, Row, Num2Str(num,6));
END;

PROCEDURE PrintVector( c,r: integer; x,y,z : real);
VAR
   Row, Col, a : integer;
   s : string[30];
   st : string;
BEGIN
   s := '';
   FOR a := 1 TO 30 DO s := s + char(219);
   Col := Round((c - 0.6) * ColWidth);
   Row := Round((r - 0.6) * RowHt);
   SetColor(DarkGray);
   OutTextXY(Col, Row, s);
   SetColor(white);
   st := concat('(',Num2Str(x,5),',',Num2Str(y,5),',',Num2Str(z,5),')');
   OutTextXY(Col, Row, st);
END;


procedure swap (var x,y : real);
var temp : real;
begin
   temp := x;
   x    := y;
   y    := temp;
end;


{------------------------ Help Screens -----------------------------------}

procedure AboutProgram;
var
   HelpScr : HelpScrType;
   i : integer;
begin
   for i := 1 to 25 do
    HelpScr[i] := '                                                  ';
   HelpScr[04] := '           Scalar And Vector Fields               ';
   HelpScr[07] := '          Written by Jarek Tuszynski              ';
   HelpScr[08] := '           George Mason University                ';
   HelpScr[09] := '         (c) 1995, John Wiley & Sons              ';
   HelpScr[16] := '   FIELDS allows you to explore the behavior      ';
   HelpScr[17] := '   of arbitrary scalar or vector field when       ';
   HelpScr[18] := '   operated on by various differential operators  ';
   HelpScr[19] := '   and when integrated along arbitrary paths.     ';
   HelpScr[25] := '   Press any Key or Click the Mouse when done.    ';
   help(HelpScr);
end;

procedure GeneralHelp;
var HelpScr : HelpScrType;
begin
   HelpScr[01] :=' Menu Options:                                    ';
   HelpScr[02] :='  -File:                                          ';
   HelpScr[03] :='   About Cups    - General Information about CUPS ';
   HelpScr[04] :='                   Project                        ';
   HelpScr[05] :='   About Program - Credit Screen and General      ';
   HelpScr[06] :='                   Information about the Program  ';
   HelpScr[07] :='   Configuration - Ways to Customize the Program  ';
   HelpScr[08] :='  -Field:                                         ';
   HelpScr[09] :='   Read Scalar Field - Scalar Field Function      ';
   HelpScr[10] :='                       Input Screen               ';
   HelpScr[11] :='   Read Vector Field - Vector Field Function      ';
   HelpScr[12] :='                       Input Screen               ';
   HelpScr[13] :='   Grid Size     - Change the size of the grid    ';
   HelpScr[14] :='                   used to perform calculations on';
   HelpScr[15] :='   Choose Plane  - Choose the Plane on which      ';
   HelpScr[16] :='                   Calculations are made          ';
   HelpScr[17] :='  -Graphs:                                        ';
   HelpScr[18] :='   Choose Graphs - Choose the operations you want ';
   HelpScr[19] :='                   to perform on the field        ';
   HelpScr[20] :='   Mouse Probe   - Shortcut to get a mouse probe  ';
   HelpScr[21] :='                   in bottom right corner         ';
   HelpScr[22] :='   Path Integral - Shortcut to Get Path Integral  ';
   HelpScr[23] :='                   Window in bottom left corner   ';
   HelpScr[24] :=' Use button in top-left corner of each graph to   ';
   HelpScr[25] :=' Switch between 2 and 3-Dimensions Graphs.        ';
   help(HelpScr);
   HelpScr[01] := ' About Graphs :                                   ';
   HelpScr[02] := '                                                  ';
   HelpScr[03] := ' 1) Plots of Scalar Fields                        ';
   HelpScr[04] := '                                                  ';
   HelpScr[05] := '  - Two-Dimensional Plots: Represented by Contour ';
   HelpScr[06] := '    Plot (Plot of curves with equal values).      ';
   HelpScr[07] := '                                                  ';
   HelpScr[08] := '  - Three-Dimentional Plots: Represented by 3D    ';
   HelpScr[09] := '    Surface where the value of the field defines  ';
   HelpScr[10] := '    the height of the surface at a given point.   ';
   HelpScr[11] := '                                                  ';
   HelpScr[12] := ' 2) Plots of Vector Fields                        ';
   HelpScr[13] := '                                                  ';
   HelpScr[14] := '  - Two-Dimensional Plots: Represented by 2D      ';
   HelpScr[15] := '    vectors drawn at each grid point. Values of   ';
   HelpScr[16] := '    the field in directions parallel to the plane ';
   HelpScr[17] := '    are used to draw the vectors. The value of    ';
   HelpScr[18] := '    the field in the direction perpendicular to   ';
   HelpScr[19] := '    the plane is used to determine the color of   ';
   HelpScr[20] := '    the vector.                                   ';
   HelpScr[21] := '                                                  ';
   HelpScr[22] := '  - Three-Dimensional Plots: Represented by 3D    ';
   HelpScr[23] := '    vectors drawn at each grid point. The         ';
   HelpScr[24] := '    Magnitude of each vector is used to determine ';
   HelpScr[25] := '    its color.                                    ';
   help(HelpScr);
end;

procedure PathIntegralHelp;
var HelpScr : HelpScrType;
begin
   HelpScr[01] :='Path Integrals:                                   ';
   HelpScr[02] :='                                                  ';
   HelpScr[03] :='1) Choose "Path Integral" option either from      ';
   HelpScr[04] :='   "Graphs/Choose Graph" input-screen or directly ';
   HelpScr[05] :='   from "Graphs" menu.                            ';
   HelpScr[06] :='                                                  ';
   HelpScr[07] :='2) Choose Open or Closed Path button.             ';
   HelpScr[08] :='                                                  ';
   HelpScr[09] :='3) Click left mouse button in any open 2-D        ';
   HelpScr[10] :='   window: scalar or vector.                      ';
   HelpScr[11] :='                                                  ';
   HelpScr[12] :='4) Draw the path by clicking left mouse button on ';
   HelpScr[13] :='   every corner of the figure. Watch mouse probe  ';
   HelpScr[14] :='   (if active) for precise placing.               ';
   HelpScr[15] :='                                                  ';
   HelpScr[16] :='5) Click right mouse button on your last corner.  ';
   HelpScr[17] :='                                                  ';
   HelpScr[18] :='6) Program draws path integral as "area under the ';
   HelpScr[19] :='   curve" and also returns its actual number.     ';
   HelpScr[20] :='                                                  ';
   HelpScr[21] :='                                                  ';
   HelpScr[22] :='                                                  ';
   HelpScr[23] :='                                                  ';
   HelpScr[24] :='                                                  ';
   HelpScr[25] :='                                                  ';
   help(HelpScr);
end;

{------------------------ TScalarFields public methods -------------------}


procedure ReadProbe;  far;
var
   a : point;
   inside : boolean;
begin
   mousePosn(a.x,a.y,Field^.Integral.Port,2,inside);
   Field^.FieldProbe(a);
end;


constructor TScalarField.Init;
var
   i : integer;
const
   size = 30;
begin
   initialize;
   Scalar2.init(size,size);
   PlotNum[1]  := 1;
   PlotNum[2]  := 2;
   PlotNum[3]  := 7;
   PlotNum[4]  := 8;
   Equation[1] := 'x*y+z*r';
   active := true;
   FindField;
end;


function TScalarField.ReadFunction : boolean;
VAR
   Screen : TInputScreen;
   n : byte;
   StrOK : boolean;
   parser : TParser;
   str : string;
   i : integer;
   HelpScr : HelpScrType;
BEGIN
   with Screen do
   begin
      init;
      DefineInputPort (0.04, 0.96, 0.27, 0.73);
      LoadLine('                    Input Scalar Field Function');
      LoadLine('');
      LoadLine('Choose coordinates:  #A F(x,y,z)  #A F(r,phi,z)  #A F(rho,theta,phi)');
      LoadLine('F="                                                                    "');
      LoadLine('Where:           x is between {     } and {     }');
      LoadLine('                 y is between {     } and {     }');
      LoadLine('                 z is between {     } and {     }');
      loadLine('');
      LoadLine('                     [  Ok  ]  [Cancel]  [ Help ]   ');
      SetRadioButton('A',coordinates);
      SetString( 4, Equation[1]);
      for i := 1 to 3 do SetNumber( 2*i+3, min[i]);
      for i := 1 to 3 do SetNumber( 2*i+4, max[i]);
      AboutParser(HelpScr);
      SetHelpScreen(HelpScr);
      parser.init;
      repeat
         AcceptScreen;
         Str := GetString(4);
         coordinates := GetRadioButton('A');
         if coordinates=3 then parser.SetRealVariable('theta',0);
         if coordinates=3 then parser.SetRealVariable('rho',0);
         if coordinates>1 then parser.SetRealVariable('phi',0);
         StrOK := parser.parse('x','y','z','r', Str) or canceled;
         if not StrOK then
         announce('Error: Cannot evaluate this expression. Please try again.');
      until StrOk;
      If not canceled then Equation[1] := Str;
      for i := 1 to 3 do min[i] := GetNumber( 2*i+3);
      for i := 1 to 3 do max[i] := GetNumber( 2*i+4);
      for i := 1 to 3 do if min[i]>max[i] then swap(min[i],max[i]);
      for i := 1 to 3 do if min[i]=max[i] then begin min[i]:=-1; max[i]:=1 end;
      ReadFunction := Not Canceled;
      active := active or Not Canceled;
      done;
   end;
end;


procedure TScalarField.ChooseGraphs;
var
   graphsScreen : TInputScreen;
   i  : integer;
begin
   with graphsScreen do
   begin
      init;
      DefineInputPort (0.2, 0.8, 0.21, 0.79);
      LoadLine('  Choose the Graphs of Scalar Field F  ');
      loadLine('            Graph1 Graph2 Graph3 Graph4');
      LoadLine('       F      #A     #B     #C     #D  ');
      LoadLine('  Grad F      #A     #B     #C     #D  ');
      LoadLine('Mag(Grad F)   #A     #B     #C     #D  ');
      LoadLine('  dF/dx       #A     #B     #C     #D  ');
      LoadLine('  dF/dy       #A     #B     #C     #D  ');
      LoadLine('  dF/dz       #A     #B     #C     #D  ');
      LoadLine('Laplacian F   #A     #B     #C     #D  ');
      loadLine('Path Integral Window        #C         ');
      loadLine('Mouse  Probe  Window               #D  ');
      LoadLine('');
      LoadLine('            [  Ok  ]   [Cancel]        ');
      If IntegralOn then PlotNum[3] := 8;  {if IntegralOn set elswhere}
      for i := 1 to 4 do SetRadioButton(chr(64+i),PlotNum[i]);   {A,B,C,D}
      AcceptScreen;
      for i := 1 to 4 do PlotNum[i] := GetRadioButton(chr(64+i)); {A,B,C,D}
      IntegralOn := (PlotNum[3] = 8);
      if not canceled then DrawPlots;
      done;
   end;
end;


destructor TScalarField.done;
var
   i : integer;
begin
   Scalar1.free;
   Scalar2.free;
   for i := 1 to 3 do Vector1[i].free;
end;

{------------------------ TScalarFields private methods -------------------}

procedure TScalarField.FindField;
begin
   If active and CreateMatrix(Equation[1],Scalar1) and
      Find_Grad_Laplacian then DrawPlots;
end;


Procedure TScalarField.Resize(mRow,mCol : integer);
var
   i : integer;
begin
   Scalar1.free;
   Scalar2.free;
   for i := 1 to 3 do Vector1[i].free;
   ErrorFound  := false;
   HaltIfError := false;
   Scalar1.Init(mRow,mCol);
   if not errorFound then Scalar2.Init(mRow,mCol);
   for i := 1 to 3 do
    if not errorFound then Vector1[i].init(mRow,mCol);
   if ErrorFound then
   begin
      mRow := 40;
      mCol := 40;
      announce('Switching to default grid.');
      resize(mRow,mCol);
   end
   else
   begin
      ErrorFound  := false;
      HaltIfError := true;
      DefineScale(2, 1,mCol, mRow,1);
      FindField;
   end;
end;


procedure TScalarField.DrawPlots;
var
   i : integer;
   TwoD : boolean;
begin
   If active then
   begin
      If IntegralOn then PlotNum[3] := 8;
      if magn1.NRows>0 then magn1.free;
      if magn2.NRows>0 then magn2.free;
      Integral.Clean;
      probe := false;
      for i := 1 to 4 do
      begin
         Plot[i].Set3DScales(0,0,0,0,0,0); {Reset Scales}
         TwoD := Plot[i].TwoD;
         SelectViewPort(i);
         with Plot[i] do
          case PlotNum[i] of
           1: PlotScalar( TwoD, Scalar1,    'Scalar Field F');
           2: PlotVector( TwoD, vector1,    'Grad F',PlaneNorm);
           3: PlotMagnitude( TwoD, vector1, 'Magnitude of Grad F', Magn1);
           4: PlotScalar( TwoD, vector1[1], 'dF/dx');
           5: PlotScalar( TwoD, vector1[2], 'dF/dy');
           6: PlotScalar( TwoD, vector1[3], 'dF/dz');
           7: PlotScalar( TwoD, Scalar2,    'Laplacian F');
           8: if i=3 then PrepareIntegral else PrepareProbe;
          end;
      end;
   end;
end;


procedure TScalarField.PrepareProbe;
var i : integer;
begin
   PlotNum[4] := 8;
   probe := false;
   for i := 1 to 4 do
    probe := probe or (plot[i].active and (plot[i].TwoD));
   OpenViewPort(4);
   Print(2,1,concat('F(x,y,z) =',Equation[1]));
   case PlaneNorm of
    1: Print(2,2,concat('yz-plane   x=',num2str(center[1],4)));
    2: Print(2,2,concat('zx-plane   y=',num2str(center[2],4)));
    3: Print(2,2,concat('xy-plane   z=',num2str(center[3],4)));
   end;
   if probe then
   begin
      Print(2,4,'Mouse Probe:');
      Print(2,5,' Position    =');
      Print(2,6,' Field F     =');
      Print(2,7,' Grad F      =');
      Print(2,8,' Laplacian F =');
   end;
end;


procedure TScalarField.FieldProbe(p : point);
const n = 17;
var
    x,y : real;
    c,r,i,j,k,vp : integer;
    v   : point3D;
begin
   if probe and ((MousePosition.x<>p.x) or (MousePosition.y<>p.y)) then
   begin
      MousePosition := p;
      case PlaneNorm of
       1: begin i:=2; j:=3; k:=1; end;
       2: begin i:=3; j:=1; k:=2; end;
       3: begin i:=1; j:=2; k:=3; end;
      end;
      vp := ViewPortNumber;
      SelectViewPort(4);
      x := p.x;
      y := p.y;
      v[i] := min[i] +(x-1)*h[i];
      v[j] := max[j] -(y-1)*h[j];
      v[k] := center[k];
      PrintVector(n,5, v[1],v[2],v[3]);
      PrintScalar(n,6, scalar1.interpolate(y,x));
      PrintVector(n,7, vector1[1].interpolate(y,x),
                       vector1[2].interpolate(y,x),
                       vector1[3].interpolate(y,x));
      PrintScalar(n,8, scalar2.interpolate(y,x));
      SelectViewPort(vp);
   end;
end;

function TScalarField.Evaluate( parser : TParser; x,y,z : real) : real;
var xx,yy,zz,r : real;
begin
   case PlaneNorm of
    1: begin xx:=z; yy:=x; zz:=y; end;
    2: begin xx:=y; yy:=z; zz:=x; end;
    3: begin xx:=x; yy:=y; zz:=z; end;
   end;
   r := sqrt(xx*xx+yy*yy);
   if coordinates=3 then parser.SetRealVariable('theta',arctan2(zz,r));
   if coordinates=3 then parser.SetRealVariable('rho',sqrt(r*r+zz*zz));
   if coordinates>1 then parser.SetRealVariable('phi',arctan2(xx,yy));
   evaluate := parser.f( x, y, z, r);
end;

Function TScalarField.Find_Grad_Laplacian : boolean;
var
   x,y,z,r, v1,v2,v3,v4,v5, Vmin,Vmax : real;
   MaxCol,MaxRow, i,j,k, col,row          : integer;
   parser : TParser;
   Ok     : boolean;
   Wait   : TWaitMessage;
   xyz    : string[3];
begin
   xyz := 'xyz';
   case PlaneNorm of
    1: begin i:=2; j:=3; k:=1; end;
    2: begin i:=3; j:=1; k:=2; end;
    3: begin i:=1; j:=2; k:=3; end;
   end;
   parser.init;
   if coordinates=3 then parser.SetRealVariable('theta',0);
   if coordinates=3 then parser.SetRealVariable('rho',0);
   if coordinates>1 then parser.SetRealVariable('phi',0);
   OK := parser.Parse(xyz[i],xyz[j],xyz[k],'r', Equation[1]);
   scalar1.GetSize(MaxRow,MaxCol);
   Wait.Show;
   HaltIfError := false;
   FOR col := 1 TO maxCol DO
   begin
      FOR row := 1 TO maxRow DO
      IF errorFound THEN
       BEGIN
          HaltIfError  := true;
          ErrorFound   := false;
          Find_Grad_Laplacian := false;
          Wait.hide;
          Exit
       END
      ELSE
       BEGIN
          x := min[i]+(col-1)*h[i];
          y := max[j]-(row-1)*h[j];
          v1 := evaluate( parser, x, y, center[k]-2*h[k] );
          v2 := evaluate( parser, x, y, center[k]-h[k] );
          v3 := scalar1.value(row,col);
          v4 := evaluate( parser, x, y, center[k]+h[k] );
          v5 := evaluate( parser, x, y, center[k]+2*h[k] );
          vector1[i].put( row,col, Scalar1.dmdx(row,col,h[i]));
          vector1[j].put( row,col,-Scalar1.dmdy(row,col,h[j]));
          vector1[k].put( row,col, (v1-8*v2+8*v4-v5)/(12*h[k]));
          scalar2   .put( row,col, (-v1+16*v2-30*v3+16*v4-v5)/(12*sqr(h[k])));
       END;
       Wait.Update;
   end;
   HaltIfError  := true;
   ErrorFound   := false;
   Find_Grad_Laplacian := true;
   FOR col := 1 TO maxCol DO
    FOR row := 1 TO maxRow DO
     scalar2.put(row,col, scalar2.value(row,col)
                         +Vector1[i].dmdx(row,col,h[i])
                         -Vector1[j].dmdy(row,col,h[j]));
   for i := 1 to 3 do
   begin
      vector1[i].MinMax(j,j,Vmin,j,j,Vmax);
      if (Vmax-Vmin)<small then
       if (Vmin*Vmax<0) then vector1[i].fill(0)
       else vector1[i].fill((Vmin+Vmax)/2)
   end;
   scalar2.MinMax(j,j,Vmin,j,j,Vmax);
   if (Vmax-Vmin)<small then
    if (Vmin*Vmax<0) then scalar2.fill(0)
    else scalar2.fill((Vmin+Vmax)/2);
   wait.hide;
end;


procedure TScalarField.CalculatePathIntegral;
var
   ScaleVec : point;
begin
   with GraphScales[1] do with scalar1 do
    SetPoint(ScaleVec, (xmax-xmin)/(NCols-1), (ymax-ymin)/(NRows-1));
   with integral do
    case PlotNum[Integral.port-15] of
     1: ScalarField( scalar1,    ScaleVec, PlaneNorm, 'Field F');
     2: VectorField( Vector1,    ScaleVec, PlaneNorm, 'Grad F');
     3: ScalarField( Magn1,      ScaleVec, PlaneNorm, 'Mag(Grad F)');
     4: ScalarField( vector1[1], ScaleVec, PlaneNorm, 'dF/dx');
     5: ScalarField( vector1[2], ScaleVec, PlaneNorm, 'dF/dy');
     6: ScalarField( vector1[3], ScaleVec, PlaneNorm, 'dF/dz');
     7: ScalarField( scalar2,    ScaleVec, PlaneNorm, 'Laplacian F');
    end;
end;


{------------------------- TScalarFields not virtual methods --------------}

procedure TScalarField.Initialize;
var
   i : integer;
const
   size = 30;
begin
   DefineViewPort(1, 0.00, 0.49, 0.51, 0.93);
   DefineViewPort(2, 0.51, 1.00, 0.51, 0.93);
   DefineViewPort(3, 0.00, 0.49, 0.07, 0.49);
   DefineViewPort(4, 0.51, 1.00, 0.07, 0.49);
   DefineScale(2, 1, size, size,1);
   SetPoint3D(min, -1,-1,-1);
   SetPoint3D(max,  1, 1, 1);
   for i := 1 to 3 do Center[i]   := 0;
   for i := 1 to 3 do Equation[i] := '';
   PlaneNorm  := 3;
   probe      := false;
   active     := false;
   IntegralOn := false;
   Scalar1.init(size,size);
   for i := 1 to 3 do vector1[i].init(size,size);
   for i := 1 to 4 do Plot[i].init(i);
   SetPoint(mousePosition, 0,0);
   magn1.NRows := 0;
   magn1.NCols := 0;
   magn2.NRows := 0;
   magn2.NCols := 0;
   coordinates := 1;
   small       := 1E-10;
   integral.init;
end;


procedure TScalarField.UpDate;
var
   inside : boolean;
   redraw : array [1..4] of boolean;
   i : integer;
   UnitArea : real;
   p,PortUnitVec : point;
begin
   if active then
   begin
      inside := false;
      for i := 1 to 4 do Plot[i].Check(redraw[i],inside,p);
      for i := 1 to 4 do if (Integral.active and redraw[i]) then
      begin
         with views[i+15] do with scalar1 do
          SetPoint( PortUnitVec, (vx2-vx1)/(NCols-1), (vy2-vy1)/(NRows-1));
         Integral.DrawPath( i+15, PortUnitVec);
      end;
      if inside and (p.x<scalar1.nCols) and (p.y<scalar1.nRows) then
       FieldProbe(p);
      if Integral.Check and plot[Integral.Port-15].TwoD then
      begin
         with views[Integral.Port] do with scalar1 do
          SetPoint( PortUnitVec, (vx2-vx1)/(NCols-1), (vy2-vy1)/(NRows-1));
         Integral.ReadPath( ReadProbe, PortUnitVec);
         CalculatePathIntegral;
      end;
   end;
end;


PROCEDURE TScalarField.ResizeMatrix;
VAR
   GridScreen : TInputScreen;
   MRow,MCol,MaxRow,MaxCol,n : integer;
BEGIN
   Scalar1.GetSize(maxRow,MaxCol);
   with GridScreen do
   begin
      init;
      DefineInputPort (0.34, 0.65, 0.33, 0.67);
      LoadLine('  Input New Grid Size');
      LoadLine('');
      LoadLine('Number of:');
      LoadLine('Columns = {   } (10-80)');
      LoadLine('Rows    = {   } (10-80)');
      LoadLine('');
      LoadLine('   [  Ok  ]  [Cancel]  ');
      SetNumber(1,MaxCol);
      SetNumber(2,MaxRow);
      SetNumberLimits(1,10,80);
      SetNumberLimits(2,10,80);
      AcceptScreen;
      MCol := trunc(GetNumber(1));
      MRow := trunc(GetNumber(2));
      done;
   end;
   if (MaxCol<>mCol) or (MaxRow<>mRow) then Resize(mRow,mCol);
END;


procedure TScalarField.DefinePlane;
var
   PlaneScreen : TInputScreen;
   i : integer;
begin
   with PlaneScreen do
   begin
      init;
      DefineInputPort(0.32,0.68,0.32,0.68);
      LoadLine('     Choose a Plane');
      LoadLine('');
      LoadLine(' #1 x={       }  (yz-plane)');
      LoadLine(' #1 y={       }  (xz-plane)');
      LoadLine(' #1 z={       }  (xy-plane)');
      LoadLine('');
      LoadLine('    [  Ok  ]  [Cancel]');
      for i := 1 to 3 do SetNumber(2*i,center[i]);
      SetRadioButton('1',PlaneNorm);
      AcceptScreen;
      for i := 1 to 3 do center[i] := GetNumber(2*i);
      PlaneNorm := GetRadioButton('1');
      if not canceled then FindField;
      done;
   end;
end;


procedure TScalarField.PrepareIntegral;
begin
   Plot[3].erase;
   IntegralOn := true;
{   PlotNum[3] := 8;}
   if not (plot[1].TwoD or plot[2].TwoD or plot[4].TwoD) then
   begin
      announce('You have to have at least one 2D plot to draw the path in.');
      ChooseGraphs;
   end;
   if (plot[1].TwoD or plot[2].TwoD or plot[4].TwoD) then
    Integral.Prepare;
end;


function TScalarField.CreateMatrix( func : string; var matrix : DMatrix) : boolean;
var
   MaxCol,MaxRow, col,row, i,j,k : integer;
   x,y,z,r,xx,yy,zz  : real;
   parser : TParser;
   Ok     : boolean;
   Wait   : TWaitMessage;
   xyz    : string[3];
begin
   xyz := 'xyz';
   case PlaneNorm of
    1: begin i:=2; j:=3; k:=1; end;
    2: begin i:=3; j:=1; k:=2; end;
    3: begin i:=1; j:=2; k:=3; end;
   end;
   parser.init;
   if coordinates=3 then parser.SetRealVariable('theta',0);
   if coordinates=3 then parser.SetRealVariable('rho',0);
   if coordinates>1 then parser.SetRealVariable('phi',0);
   OK := parser.Parse(xyz[i],xyz[j],xyz[k],'r', func);
   matrix.GetSize(MaxRow,MaxCol);
   h[i] := (max[i]-min[i])/(maxCol-1);
   h[j] := (max[j]-min[j])/(maxRow-1);
   h[k] := 0.01;
   Wait.Show;
   HaltIfError := false;
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
       BEGIN
          x := min[i]+(col-1)*h[i];
          y := max[j]-(row-1)*h[j];
          z := center[k];
          matrix.put(row,col, evaluate( parser, x, y, z));
       END;
       Wait.Update;
   end;
   wait.hide;
   HaltIfError  := true;
   ErrorFound   := false;
   CreateMatrix := true;
   DefineScale(1, min[i], max[i], min[j], max[j]);
end;


{------------------------- TVectorFields public methods -------------------}


constructor TVectorField.Init;
var
   i : integer;
const
   size = 30;
begin
   initialize;
   for i := 1 to 3 do vector2[i].init(size,size);
   PlotNum[1] := 1;
   PlotNum[2] := 6;
   PlotNum[3] := 7;
   PlotNum[4] := 12;
end;


function TVectorField.ReadFunction : boolean;
VAR
   Screen : TInputScreen;
   n      : byte;
   StrOK  : array [1..3] of boolean;
   Str    : array [1..3] of string;
   parser : TParser;
   i : Integer;
   HelpScr : HelpScrType;
BEGIN
   with Screen do
   begin
      init;
      DefineInputPort (0.04, 0.96, 0.25, 0.75);
      LoadLine('                    Input Vector Field Functions');
      LoadLine('');
      LoadLine('Choose coordinates:  #A A(x,y,z)  #A A(r,phi,z)  #A A(rho,phi,theta)');
      LoadLine('Ax="                                                                   "');
      LoadLine('Ay="                                                                   "');
      LoadLine('Az="                                                                   "');
      LoadLine('Where:           x is between {     } and {     }');
      LoadLine('                 y is between {     } and {     }');
      LoadLine('                 z is between {     } and {     }');
      LoadLine('');
      LoadLine('                     [  Ok  ]  [Cancel]  [ Help ] ');
      SetRadioButton('A',coordinates);
      for i := 1 to 3 do SetString( i+3, Equation[i]);
      for i := 1 to 3 do SetNumber( 2*i+5, min[i]);
      for i := 1 to 3 do SetNumber( 2*i+6, max[i]);
      AboutParser(HelpScr);
      SetHelpScreen(HelpScr);
      parser.init;
      repeat
         AcceptScreen;
         coordinates := GetRadioButton('A');
         for i := 1 to 3 do Str[i] := GetString(i+3);
         if coordinates=3 then parser.SetRealVariable('theta',0);
         if coordinates=3 then parser.SetRealVariable('rho',0);
         if coordinates>1 then parser.SetRealVariable('phi',0);
         for i := 1 to 3 do
          StrOK[i] := parser.parse('x','y','z','r', Str[i]) or canceled;
         if not StrOK[1] then
         announce('Error: Cannot evaluate Ax function. Please try again.') else
         if not StrOK[2] then
         announce('Error: Cannot evaluate Ay function. Please try again.') else
         if not StrOK[3] then
         announce('Error: Cannot evaluate Az function. Please try again.');
      until StrOk[1] and StrOk[2] and StrOk[3];
      If not canceled then
       for i := 1 to 3 do Equation[i] := Str[i];
      for i := 1 to 3 do min[i] := GetNumber( 2*i+5);
      for i := 1 to 3 do max[i] := GetNumber( 2*i+6);
      for i := 1 to 3 do if min[i]>max[i] then swap(min[i],max[i]);
      for i := 1 to 3 do if min[i]=max[i] then begin min[i]:=-1; max[i]:=1 end;
      ReadFunction := Not Canceled;
      active := active or Not Canceled;
      done;
   end;
end;


procedure TVectorField.ChooseGraphs;
var
   graphsScreen : TInputScreen;
   i : integer;
   OK : boolean;
begin
   OK := true;
   with graphsScreen do
   begin
      init;
      DefineInputPort (0.2, 0.8, 0.13, 0.87);
      LoadLine('  Choose the Graphs of Vector Field A  ');
      loadLine('            Graph1 Graph2 Graph3 Graph4');
      LoadLine('       A      #A     #B     #C     #D  ');
      LoadLine('   mag(A)     #A     #B     #C     #D  ');
      LoadLine('       Ax     #A     #B     #C     #D  ');
      LoadLine('       Ay     #A     #B     #C     #D  ');
      LoadLine('       Az     #A     #B     #C     #D  ');
      LoadLine('   Div A      #A     #B     #C     #D  ');
      LoadLine('  Curl A      #A     #B     #C     #D  ');
      LoadLine('mag(Curl A)   #A     #B     #C     #D  ');
      LoadLine(' (Curl A)x    #A     #B     #C     #D  ');
      LoadLine(' (Curl A)y    #A     #B     #C     #D  ');
      LoadLine(' (Curl A)z    #A     #B     #C     #D  ');
      loadLine('Path Integral Window        #C         ');
      loadLine('Mouse  Probe  Window               #D  ');
      LoadLine('');
      LoadLine('           [  Ok  ]   [Cancel]         ');
      If IntegralOn then PlotNum[3] := 12;  {if IntegralOn set elswhere}
      for i := 1 to 4  do SetRadioButton(chr(64+i),PlotNum[i]);   {A,B,C,D}
      AcceptScreen;
      for i := 1 to 4  do PlotNum[i] := GetRadioButton(chr(64+i)); {A,B,C,D}
      IntegralOn := (PlotNum[3] = 12);
      If not canceled then DrawPlots;
      done;
   end;
end;


destructor TVectorField.done;
var
   i : integer;
begin
   Scalar1.free;
   for i := 1 to 3 do Vector1[i].free;
   for i := 1 to 3 do Vector2[i].free;
end;


{------------------------- TVectorFields private methods ------------------}


procedure TVectorField.FindField;
begin
   if active and
      CreateMatrix(Equation[1],vector1[1]) and
      CreateMatrix(Equation[2],vector1[2]) and
      CreateMatrix(Equation[3],vector1[3]) and
      Find_Div_Curl then DrawPlots;
end;


Procedure TVectorField.Resize(mRow,mCol : integer);
var
   i : integer;
begin
   Scalar1.free;
   for i := 1 to 3 do Vector1[i].free;
   for i := 1 to 3 do Vector2[i].free;
   Scalar1.Init(mRow,mCol);
   for i := 1 to 3 do Vector1[i].init(mRow,mCol);
   for i := 1 to 3 do Vector2[i].init(mRow,mCol);
   DefineScale(2, 1,mCol, mRow,1);
   FindField;
end;


procedure TVectorField.DrawPlots;
var
   i : integer;
   TwoD : boolean;
begin
   if active then
   begin
      If IntegralOn then PlotNum[3] := 12;
      probe := false;
      if magn1.NRows<>0 then magn1.free;
      if magn2.NRows<>0 then magn2.free;
      Integral.Clean;
      for i := 1 to 4 do
      begin
         Plot[i].Set3DScales(0,0,0,0,0,0); {Reset Scales}
         TwoD := Plot[i].TwoD;
         with Plot[i] do
          case PlotNum[i] of
           1: PlotVector( TwoD, vector1,    'Vector Field A',PlaneNorm);
           2: PlotMagnitude( TwoD, vector1, 'Magnitude of A', Magn1);
           3: PlotScalar( TwoD, vector1[1], 'Ax');
           4: PlotScalar( TwoD, vector1[2], 'Ay');
           5: PlotScalar( TwoD, vector1[3], 'Az');
           6: PlotScalar( TwoD, Scalar1,    'Div A');
           7: PlotVector( TwoD, vector2,    'Curl A',PlaneNorm);
           8: PlotMagnitude( TwoD, vector2, 'Magnitude of Curl A', Magn1);
           9: PlotScalar( TwoD, vector2[1], '(Curl A)x');
          10: PlotScalar( TwoD, vector2[2], '(Curl A)y');
          11: PlotScalar( TwoD, vector2[3], '(Curl A)z');
          12: if i=3 then PrepareIntegral else PrepareProbe;
          end;
      end;
   end;
end;

procedure TVectorField.PrepareProbe;
var i : integer;
begin
   PlotNum[4] := 12;
   probe := false;
   for i := 1 to 4 do probe := probe or (plot[i].TwoD);
   OpenViewPort(4);
   Print(2,1,concat('Ax = ',Equation[1]));
   Print(2,2,concat('Ay = ',Equation[2]));
   Print(2,3,concat('Az = ',Equation[3]));
   case PlaneNorm of
    1: Print(2,4,concat('yz-plane  x=',num2str(center[1],4)));
    2: Print(2,4,concat('zx-plane  y=',num2str(center[2],4)));
    3: Print(2,4,concat('xy-plane  z=',num2str(center[3],4)));
   end;
   if probe then
   begin
      Print(2,6,'Mouse Probe:');
      Print(2,7,'Position =');
      Print(2,8,'Field A  =');
      Print(2,9,'Div A    =');
      Print(2,10,'Curl A   =');
   end;
end;


procedure TVectorField.FieldProbe(p : point);
const n = 12;
var
    x,y : real;
    c,r,i,j,k,vp : integer;
    v   : point3D;
begin
   if probe and ((MousePosition.x<>p.x) or (MousePosition.y<>p.y)) then
   begin
      MousePosition := p;
      case PlaneNorm of
       1: begin i:=2; j:=3; k:=1; end;
       2: begin i:=3; j:=1; k:=2; end;
       3: begin i:=1; j:=2; k:=3; end;
      end;
      vp := viewPortNumber;
      SelectViewPort(4);
      x := p.x;
      y := p.y;
      v[i] := min[i] +(x-1)*h[i];
      v[j] := max[j] -(y-1)*h[j];
      v[k] := center[k];
      PrintVector(n,7, v[1],v[2],v[3]);
      PrintVector(n,8, vector1[1].interpolate(y,x),
                       vector1[2].interpolate(y,x),
                       vector1[3].interpolate(y,x));
      PrintScalar(n,9, scalar1.interpolate(y,x));
      PrintVector(n,10,vector2[1].interpolate(y,x),
                       vector2[2].interpolate(y,x),
                       vector2[3].interpolate(y,x));
      SelectViewPort(vp);
   end;
end;


Function TVectorField.Find_Div_Curl : boolean;
var
   XFunc,YFunc,ZFunc : TParser;
   MaxCol, MaxRow, col,row,n, i,j,k : integer;
   x,y,z1,z2,vMin,vMax,dmdz : real;
   Ok   : boolean;
   Wait : TWaitMessage;
   xyz  : string[3];
begin
   xyz := 'xyz';
   case PlaneNorm of
    1: begin i:=2; j:=3; k:=1; end;
    2: begin i:=3; j:=1; k:=2; end;
    3: begin i:=1; j:=2; k:=3; end;
   end;
   XFunc.init;              YFunc.init;              ZFunc.init;
   if coordinates>1 then
   begin
      XFunc.SetRealVariable('theta',0);
      YFunc.SetRealVariable('theta',0);
      ZFunc.SetRealVariable('theta',0);
   end;
   if coordinates=3 then
   begin
      XFunc.SetRealVariable('rho',0);  XFunc.SetRealVariable('phi',0);
      YFunc.SetRealVariable('rho',0);  YFunc.SetRealVariable('phi',0);
      ZFunc.SetRealVariable('rho',0);  ZFunc.SetRealVariable('phi',0);
   end;
   OK := XFunc.Parse( xyz[i],xyz[j],xyz[k],'r', Equation[PlaneNorm mod 3 +1]);
   OK := YFunc.Parse( xyz[i],xyz[j],xyz[k],'r', Equation[(PlaneNorm+1) mod 3 +1]);
   OK := ZFunc.Parse( xyz[i],xyz[j],xyz[k],'r', Equation[PlaneNorm]);
   scalar1.GetSize(MaxRow,MaxCol);
   h[i] := (max[i]-min[i])/(maxCol-1);
   h[j] := (max[j]-min[j])/(maxRow-1);
   Wait.Show;
   HaltIfError := false;
   FOR col := 1 TO maxCol DO
   begin
    FOR row := 1 TO maxRow DO
    IF errorFound THEN
     BEGIN
        HaltIfError   := true;
        ErrorFound    := false;
        Find_Div_Curl := false;
        Wait.hide;
        Exit
     END
    ELSE
    BEGIN
      x  := min[i]+(col-1)*h[i];
      y  := max[j]-(row-1)*h[j];
      z1 := center[k]-h[k];
      z2 := center[k]+h[k];
      dmdz := (evaluate(ZFunc, x,y,z2)-evaluate(ZFunc, x,y,z1))/(2*h[k]);
      scalar1.put(row, col, dmdz +Vector1[i].dmdx(row,col,h[i])
                                 -Vector1[j].dmdy(row,col,h[j]));

      dmdz := (evaluate(YFunc, x,y,z2)-evaluate(YFunc, x,y,z1))/(2*h[k]);
      vector2[i].put(row,col,-dmdz -vector1[k].dmdy(row,col,h[j]));
      dmdz := (evaluate(XFunc, x,y,z2)-evaluate(XFunc, x,y,z1))/(2*h[k]);
      vector2[j].put(row,col, dmdz -vector1[k].dmdx(row,col,h[i]));
      vector2[k].put(row,col, vector1[j].dmdx(row,col,h[i])
                             +vector1[i].dmdy(row,col,h[j]));
     END;
     Wait.Update;
   end;
   HaltIfError   := true;
   ErrorFound    := false;
   Find_Div_Curl := true;
   for i := 1 to 3 do
   begin
      vector2[i].MinMax(j,j,Vmin,j,j,Vmax);
      if (Vmax-Vmin)<small then
       if (Vmin*Vmax<0) then vector2[i].fill(0)
       else vector2[i].fill((Vmin+Vmax)/2)
   end;
   scalar1.MinMax(j,j,Vmin,j,j,Vmax);
   if (Vmax-Vmin)<small then
    if (Vmin*Vmax<0) then scalar1.fill(0)
    else scalar1.fill((Vmin+Vmax)/2);
   wait.hide;
end;

procedure TVectorField.CalculatePathIntegral;
var
   ScaleVec : point;
begin
   with GraphScales[1] do with scalar1 do
    SetPoint(ScaleVec, (xmax-xmin)/(NCols-1), (ymax-ymin)/(NRows-1));
   with integral do
    case PlotNum[Integral.port-15] of
     1: VectorField( Vector1,    ScaleVec, PlaneNorm, 'Field A');
     2: ScalarField( Magn1,      ScaleVec, PlaneNorm, 'Mag(A)');
     3: ScalarField( vector1[1], ScaleVec, PlaneNorm, 'Ax');
     4: ScalarField( vector1[2], ScaleVec, PlaneNorm, 'Ay');
     5: ScalarField( vector1[3], ScaleVec, PlaneNorm, 'Az');
     6: ScalarField( scalar1,    ScaleVec, PlaneNorm, 'Div A');
     7: VectorField( Vector2,    ScaleVec, PlaneNorm, 'Curl A');
     8: ScalarField( Magn2,      ScaleVec, PlaneNorm, 'Mag(Curl A)');
     9: ScalarField( vector2[1], ScaleVec, PlaneNorm, '(Curl A)x');
    10: ScalarField( vector2[2], ScaleVec, PlaneNorm, '(Curl A)y');
    11: ScalarField( vector2[3], ScaleVec, PlaneNorm, '(Curl A)z');
    end;
end;

{------------------------- initialization ----------------------------------}


PROCEDURE SetUpHotKeys(var HotKeys : THotKeys);
BEGIN
   WITH HotKeys DO
   BEGIN
      init(2);
      key[1] := 'F1-Help';
      key[2] := 'F10-Menu';
      Display;
   END;
END;


procedure SetUpMenu(var menu : TMenu);
var i : integer;
begin
   with menu do
   begin
      init;
      column(1,'File');
       row(1,1,'About CUPS');
       row(1,2,'About Program');
       row(1,3,'Configuration');
       row(1,4,'-------------');
       row(1,5,'Exit Program');
      column(2,'Field');
       row(2,1,'Read Scalar Field');
       row(2,2,'Read Vector Field');
       row(2,3,'-----------------');
       row(2,4,'Grid Size');
       row(2,5,'Choose Plane');
      column(3,'Graphs');
       row(3,1,'Choose Graphs');
       row(3,2,'Mouse Probe');
       row(3,3,'Path Integral');
      display;
   end;
end;


{----------------------------- Control -------------------------------------}

Procedure HandleMenu;
var
   OK : boolean;
   oldField : ^TScalarField;
begin
   case Menu.ColChosen of
    1: case Menu.RowChosen of
        1 : AboutCups;
        2 : AboutProgram;
        3 : configuration;
        5 : QuitFlag := true;
       end;
    2: case Menu.RowChosen of
        1 : begin
               oldField := Field;
               Field    := ScalarField;
               if Field^.ReadFunction then Field^.FindField
               else Field := oldField;
            end;
        2 : begin
               oldField := Field;
               Field    := VectorField;
               if Field^.ReadFunction then Field^.FindField
               else Field := oldField;
            end;
        4 : Field^.ResizeMatrix;
        5 : Field^.DefinePlane;
       end;
    3: case Menu.RowChosen of
        1 : Field^.ChooseGraphs;
        2 : Field^.PrepareProbe;
        3 : Field^.PrepareIntegral;
       end;
   end;
end;


procedure HandleHotKeys(key : integer);
begin
   case key of
    1: if Field^.IntegralOn then PathIntegralHelp else GeneralHelp;
    2: if menu.chosen then HandleMenu;
   end;
end;


procedure Initialize;
begin
   QuitFlag := false;
   new(ScalarField,init);
   new(VectorField,init);
   Field := ScalarField;
end;


procedure Finish;
begin
   dispose(ScalarField,done);
   dispose(VectorField,done);
end;


var key : byte;
   palette : PaletteType;
BEGIN
   CUPSinit;
   GetDefaultPalette(palette);
   SetUpHotKeys(HotKeys);
   SetUpMenu(menu);
   Initialize;
   AboutProgram;
   repeat
      CheckForEvents;
      if hotKeys.pressed(Key) then HandleHotKeys(key);
      if menu.activated then HandleMenu;
      Field^.UpDate;
   until QuitFlag;
   Finish;
   Menu.Done;
   CupsDone;
END.
