                   {************************************}
                   {************************************}
                   {***      Program Poisson.Pas     ***}
                   {***  Written by Jarek Tuszynski  ***}
                   {***        June 3, 1991          ***}
                   {************************************}
                   {************************************}
{$O+,F+}
PROGRAM Poisson;

USES Graph, Crt, CupsMupp, CUPS, CupsGrph, CupsFunc, CupsProc, CupsGui,
     PoissonP, PoissonT;

type
   PlotType = (contours,vectors,threeD,Div3D,Field3D,all);

   TMain = object
      procedure init;
      procedure done;
      procedure Reset;
      procedure Restart;
      procedure SetScreen;
      procedure Resize;
      procedure ReadInput;
      procedure ModifyPlates;
      procedure DrawPlates;
      procedure DrawBoundary;
      procedure SetBoundary;
      procedure NotOnlyChargesPlot;
      procedure Draw;
      procedure DrawGrid(flag : boolean);
      procedure SaveFile;
      procedure ReadFile;
      procedure MakeReadyToDraw(flag : boolean);
      function  ChooseAlgorithm : boolean;
   private
      DScr,DFnc         : point;            {midgrid screen dist.}
      plotKind          : PlotType;
      plateKind         : ShapeType;        {current PlateKind}
      ExactMethod       : boolean;          {Exact solution or relaxation?}
      GroundedBox       : boolean;
      ShowGrid          : boolean;
      ChargesOnly       : boolean;
      ReadyToDraw       : boolean;
      UserFunction      : boolean;
      potential,charges,extra : DMatrix;          {matrixes of Q and V}
      mask              : BoolMatrix;       {Mask showing where plates are}
      plates            : TPlates;          {plates data}
      CurrentCharge     : real;             {current value on slider}
      ChargeRadius      : real;
      FileName          : string;
      Scale             : GraphScale;
   end;

VAR
   QuitFlag         : boolean;

   HotKeys          : THotKeys;
   icons            : TIcons;
   menu             : TMenu;
   Main             : TMain;
   Plot3D           : TPlot3DSurface;
   AllPlots         : T4Graphs;
   relaxation       : TRelaxation;
   UserDefPotential : TUserFunction;
   Slider           : TSlider;          {slider control}

procedure DrawPotential; forward;
procedure HandleMenu; forward;

{------------------------- initialization ----------------------------------}


PROCEDURE SetUpHotKeys(var HotKeys : THotKeys);
BEGIN
   WITH HotKeys DO
   BEGIN
      init(4);
      key[1] := 'F1-Help';
      key[2] := 'Return-Run';
      key[3] := 'Del-Delete';
      key[4] := 'F10-Menu';
      Display;
   END;
END;

PROCEDURE AdjustHotKeys(PlotKind : PlotType; var HotKeys : THotKeys);
BEGIN
   WITH HotKeys DO
   case PlotKind of
    all:
      begin
         init(3);
         key[2] := 'Del-Delete';
         key[3] := 'F10-Menu';
      end;
    contours,vectors:
      begin
         init(4);
         key[2] := 'Return-Run';
         key[3] := 'Del-Delete';
      end;
    threeD,Div3D,Field3D :
      begin
         init(4);
         key[2] := 'Return-Redraw';
         key[3] := 'Del-Delete';
      end;
   end;
   HotKeys.Display;
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
       row(1,5,'Open...');
       row(1,6,'Save');
       row(1,7,'Save as...');
       row(1,8,'-------------');
       row(1,9,'Exit Program');
      column(2,'Plot How');
       row(2,1,'Contour Map of Potential');
       row(2,2,'Electric Field Vectors');
       row(2,3,'3D Plot of Potential');
       row(2,4,'3D Plot of Charge Distribution');
       row(2,5,'3D Plot of Electric Field Magnitude');
       row(2,6,'Above Four Plots');
      column(3,'Extras');
       row(3,1,'Cross Section of E and V');
       row(3,2,'Field Line through a Point');
       row(3,3,'-----------------------------');
       row(3,4,'Input Potential in rectangular coords.');
       row(3,5,'Input Potential in cylindrical coords.');
      column(4,'Control');
       row(4,1,'Relaxation Parameters');
       row(4,2,'Grid Matrix Size');
       row(4,3,'Algorithm and Boundary Conditions');
       row(4,4,'-------------------------------------');
       row(4,5,'Show Grid');
       row(4,6,'Draw with Fixed Center');
       row(4,7,'Ask for Potential or Charge Functions');
      column(5,'Help');
       row(5,1,'General Informations');
       row(5,2,'Step by Step Tutorial');
       row(5,3,'Modify Mode Tutorial');
       row(5,4,'About Plates');
      display;
      RowChecked(4,5,true);
      RowChecked(4,6,true);
      RowActivate(1,4,false);
      RowActivate(1,8,false);
      RowActivate(3,3,false);
      RowActivate(4,4,false);
      AutoChecking(2,1);     {plotKInd - contour-plot}
   end;
end;

procedure PrepareUserFunc(flag : boolean);
var i : integer;
begin
   if (Main.UserFunction <> flag) then
   with menu do
   begin
      Main.UserFunction := flag;                {if flag true then}
      for i := 6 to 7 do RowActivate(1,i, not flag); {can't save}
      for i := 2 to 6 do RowActivate(2,i, flag);     {yes - other graphs}
      for i := 1 to 2 do RowActivate(3,i, flag);     {yes - extras}
   end;
end;

procedure ReDraw(var potential : DMatrix);  far;
{ This procedure is drawing procedure for FieldLines object.  It was removed}
{ from this object to make it independed of all global variables.           }
begin
   relaxation.ReDraw(potential);
   Main.Plates.DrawAll;
end;

{---------------------------- Main ------------------------------------}

function TMain.ChooseAlgorithm : boolean;
VAR
   AlgorithmScreen : TInputScreen;
   solutionFlag,boundaryFlag : boolean;
   n : integer;
BEGIN
{MAC new(AlgorithmScreen);}
   with AlgorithmScreen do
   begin
      init;
      DefineInputPort (0.2, 0.8, 0.24, 0.76);
      LoadLine('   Choose Algorithm and Boundary Conditions   ');
      LoadLine('');
      LoadLine('Algorithms: ');
      LoadLine(' #1 Relaxation Algorithm');
      LoadLine('    Boundary Conditions: #2 Grounded Box ' );
      LoadLine('                         #2 Parallel Capacitor');
      LoadLine('');
      if ChargesOnly then
       LoadLine(' #1 Exact Solution (for charges only)')
      else
       LoadLine('    Exact Solution (not avaiable for plates)');
      LoadLine('    Boundary Conditions:    No Boundaries ' );
      LoadLine('    Charge Radius = {       } 0<r'+chr(243)+'5');
      LoadLine('');
      LoadLine('             [  Ok  ]  [Cancel]  ');
      if ExactMethod then SetRadioButton('1',2) else SetRadioButton('1',1);
      if GroundedBox then SetRadioButton('2',1) else SetRadioButton('2',2);
      SolutionFlag := ExactMethod;
      BoundaryFlag := GroundedBox;
      if ChargesOnly then n:=5 else n:=4;
      SetNumber(n,ChargeRadius);
      SetNumberLimits(n,1e-10,5);
      AcceptScreen;
      ExactMethod := (GetRadioButton('1')=2);
      GroundedBox := (GetRadioButton('2')=1);
      ChooseAlgorithm := (ExactMethod<>SolutionFlag) or
                         (GroundedBox<>BoundaryFlag);
      ChargeRadius := GetNumber(n){+1E-6};
      done;
   end;
{MAC dispose(AlgorithmScreen);}
end;

procedure TMain.DrawBoundary;
var i,vpNum : integer;
begin
   HideMouse;
   vpNum := ViewPortNumber;
   CloseViewPort(vpNum);                      {clean viewPort}
   SetLineStyle(0,0,ThickWidth);
   with views[vpNum] do
   begin
      SetColor(blakk);
      rectangle(vx1-2,vy1-2,vx2+2,vy2+2);
      i := (vy1+vy2) div 2;
      line(vx1-2,i,vx1-15,i);
      line(vx2+2,i,vx2+15,i);
      line(vx2+2,vy2+2,vx2+2,vy2+12);
      line(vx2-10,vy2+12,vx2+14,vy2+12);
      line(vx2-5,vy2+17,vx2+9,vy2+17);
      line(vx2+1,vy2+22,vx2+3,vy2+22);
      SetColor(white);
      if UserFunction then
      begin
         SetLineStyle(0,0,NormWidth);
         rectangle(vx1-2,vy1-2,vx2+2,vy2+2);
      end
      else
      if ExactMethod then
      begin
         SetLineStyle(0,0,NormWidth);
         rectangle(vx1,vy1,vx2,vy2);
      end
      else
       if GroundedBox then
       begin                       {grounded box}
          rectangle(vx1-2,vy1-2,vx2+2,vy2+2);
          line(vx2+2,vy2+2,vx2+2,vy2+12);
          line(vx2-10,vy2+12,vx2+14,vy2+12);
          line(vx2-5,vy2+17,vx2+9,vy2+17);
          line(vx2+1,vy2+22,vx2+3,vy2+22);
       end
       else
       begin                      {Capacitor}
          line(vx1-2,vy1-2,vx1-2,vy2+2);
          line(vx2+2,vy1-2,vx2+2,vy2+2);
          line(vx1-2,i,vx1-15,i);
          line(vx2+2,i,vx2+15,i);
       end;
   end;
   ShowMouse;
   SetLineStyle(0,0,NormWidth);
   SelectViewPort(vpNum);
end;


procedure TMain.SetBoundary;
var
   i,j,MaxRow,MaxCol : integer;
   value : real;
begin
   potential.GetSize(maxRow,MaxCol);
   FOR i := 1 TO maxCol DO               {mask upper, lower boundary}
   begin
      if groundedBox then Value:=0 else Value:=100*(1-2*(i-1)/(maxCol-1));
      potential.put(1,i,value);
      potential.put(MaxRow,i,value);
   end;
   FOR j := 1 TO maxRow DO               {mask right, left boundary}
   begin
      if groundedBox then Value:=0 else Value:=100;
      potential.Put(j,1,value);
      potential.Put(j,MaxCol,-value);
   end;
end;


procedure TMain.DrawGrid(flag : boolean);
var
   i,j,MaxRow,MaxCol,dx,dy,color,currentColor : integer;
begin
   currentColor := getColor;
   potential.GetSize(maxRow,MaxCol);
   if flag then color:=LightGray else color:=blakk;
   HideMouse;
   dx := maxCol div 2;
   dy := maxRow div 2;
   for i := 1 to dx do
    for j := 1 to dy do
    begin
       PutPixel( round((dx+i)*DScr.x), round((dy+j)*DScr.y), color);
       PutPixel( round((dx+i)*DScr.x), round((dy-j)*DScr.y), color);
       PutPixel( round((dx-i)*DScr.x), round((dy+j)*DScr.y), color);
       PutPixel( round((dx-i)*DScr.x), round((dy-j)*DScr.y), color);
    end;
   SetColor(color);
   vector(0, round(dy*DScr.y), round((maxCol-1)*DScr.x), round(dy*DScr.y));
   vector(round(dx*DScr.x), round((maxRow-1)*DScr.y), round(dx*DScr.x), 0);
   setColor(currentColor);
   ShowMouse;
end;

PROCEDURE TMain.Resize;
VAR
   GridScreen : TInputScreen;
   MRow,MCol,MaxRow,MaxCol : integer;
   n : byte;
BEGIN
{MAC new(GridScreen);}
   potential.GetSize(maxRow,MaxCol);
   with GridScreen do
   begin
      init;
      DefineInputPort (0.29, 0.71, 0.28, 0.71);
      LoadLine('    Input New Grid   ');
      LoadLine('');
      LoadLine('Number of:');
      LoadLine('  Columns = {    } (10-100)');
      LoadLine('  Rows    =  '+NumStr(MaxRow,0,0));
      LoadLine('Number of rows is adjusted auto-');
      LoadLine('maticaly to number of columns.');
      LoadLine('Grids bigger then 70-80 columns');
      LoadLine('may cause memory problems');
      LoadLine('    [  Ok  ]  [Cancel]  ');
      SetNumber(1,MaxCol);
      SetNumberLimits(1,10,100);
      AcceptScreen;
      MCol := trunc(GetNumber(1));
      done
   end;
   mRow := trunc(0.7*mCol);
   if (MaxCol<>mCol) then
   begin
      done; {free all DMatrixes}
      HaltIfError := false;
      ErrorFound  := false;
      potential.Init(mRow,mCol);
      if not errorFound then charges.Init(mRow,mCol);
      if not errorFound then mask   .init(mRow,mCol);
      if not errorFound then extra  .init(mRow,mCol);
      if errorFound then
      begin
         done;
         Announce('Switching to default grid size');
         mCol := 40;
         mRow := trunc(0.7*mCol);
         potential.Init(mRow,mCol);
         charges  .Init(mRow,mCol);
         mask     .init(mRow,mCol);
         extra    .init(mRow,mCol);
      end;
      HaltIfError := true;
      ErrorFound  := false;
      with views[1] do
      begin
         DScr.x := (vx2-vx1) / (mCol-1);
         DScr.y := (vy2-vy1) / (mRow-1);
      end;
      plates.PassVQMatrixes(potential,charges);
      if not (Plot3D.active or AllPlots.active) then CleanViewPort(1);
      if (UserFunction and UserDefPotential.CreateMatrix(potential))
      then DrawPotential else
      if (ReadyToDraw and (not Icons.Modify)) then Draw else DrawPlates;
   end;
END;


procedure TMain.Init;
var
   NumCol,NumRow : integer;
begin
   DefineScale(1,-3,3,-9/4,9/4);
   PlotKind     := contours;
   ExactMethod  := false;
   GroundedBox  := true;
   ChargesOnly  := true;
   ShowGrid     := true;
   UserFunction := false;
   icons.modify := false;
   icons.choice := line_;
   FileName     := '';
   NumCol       := 40;
   NumRow       := trunc(0.7*NumCol);
   ChargeRadius := 0.25;
   DefineViewPort (1, 0.11, 0.97, 0.1 , 0.9);
   DefineViewPort (2, 0.0 ,  1.0, 0.05, 0.95);
   Slider   .Create(-100,100,100,0.055,0.1,0.6,0,'-100','100','',true);
   SetScreen;
   plates   .init;
   potential.init(NumRow,NumCol);
   charges  .init(NumRow,NumCol);
   mask     .init(NumRow,NumCol);
   extra    .init(NumRow,NumCol);
   charges  .fill(0);                               {all charges = 0}
   potential.fill(plates.PlateSymbol);
   Reset;
end;

Procedure TMain.Done;
begin
   Potential.free;
   Charges.free;
   extra.free;
   mask.free;
end;

Procedure TMain.SetScreen;
begin
   if UserFunction then icons.choice:=nil_  else
   if icons.choice=nil_ then icons.choice:=line_;
   icons.draw;
   slider.draw;
   CleanViewPort(1);
   DrawBoundary;
   HotKeys.Display;
end;

procedure TMain.Restart;
begin
   PrepareUserFunc(false);
   Plates.done;
   UserFunction := false;
   ChargesOnly  := true;
   SetScreen;
   Reset;
   menu.RowActivate(4,6,true);                      {exact metchod true}
end;

procedure TMain.Reset;
var
   i,j,MaxCol,MaxRow : integer;
begin
   CleanViewPort(1);
   potential.GetSize(MaxRow,MaxCol);
   with views[1] do
   begin
      DScr.x := (vx2-vx1) / (maxCol-1);
      DScr.y := (vy2-vy1) / (maxRow-1);
   end;
   if showGrid then DrawGrid(ShowGrid);
   plates.PassVQMatrixes(potential,charges);
   MakeReadyToDraw(false);
end;


procedure TMain.ModifyPlates;
var
   oldPlate  : ShapeType;
   oldCharge : real;
   i,j,MaxCol,MaxRow : integer;
begin
   if not (plates.ModifyOn) then                {clean before modify}
   begin
      HotKeys.key[2] := 'F7-Remove';
      HotKeys.key[3] := 'F8-Function';
      HotKeys.BackColor := green;
      HotKeys.Display;
      menu.RowActivate(3,4,false);              {UserFunctions}
      menu.RowActivate(3,5,false);
      CleanViewPort(1);
      DrawPlates;
      if ShowGrid then DrawGrid(ShowGrid);
      MakeReadyToDraw(false);
   end;
   oldCharge := CurrentCharge;
   oldPlate  := PlateKind;
   plates.modify(PlateKind,CurrentCharge);
   if oldPlate<>PlateKind then
   begin
      icons.choice := PlateKind;
      icons.Update;
   end;
   if oldCharge<>CurrentCharge then slider.Reset(CurrentCharge);
end;

procedure TMain.ReadInput;
var
   i,j,MaxCol,MaxRow : integer;
begin
    if event.mouseClicked then
    begin
       if plates.modifyOn then                       {clean after modify}
       begin
          HotKeys.key[2] := 'Return-Run';
          HotKeys.key[3] := 'Del-Delete';
          HotKeys.BackColor := blue;
          HotKeys.Display;
          menu.RowActivate(3,4,true);                {UserFunctions}
          menu.RowActivate(3,5,true);
          if (plateKind=nil_) then
          begin
             plateKind := Line_;
             icons.choice := PlateKind;
             icons.Update;
          end;
          Reset;
          ChargesOnly := plates.CheckIfChargesOnly;
       end;
       if plates.read(PlateKind,CurrentCharge) then
       begin
          if ShowGrid then DrawGrid(ShowGrid);
          if ReadyToDraw then      {start drawing on the old picture}
          begin
             MakeReadyToDraw(false);
             plates.DrawAll;
          end;
          if ChargesOnly and ((PlateKind<>point_) and (PlateKind<>qLine)) then
            NotOnlyChargesPlot;
       end;
    end;
end;


procedure TMain.NotOnlyChargesPlot;
var i : integer;
begin
   ChargesOnly := false;
   ReadyToDraw := false;
   if ExactMethod then
   begin
      ExactMethod := false;
      DrawBoundary;
      DrawPlates;
   end;
end;


procedure TMain.DrawPlates;
begin
   if not (Plot3D.active or AllPlots.active or UserFunction) then
    Plates.DrawAll;
   if not (Plot3D.active or AllPlots.active) and ShowGrid then
    DrawGrid(ShowGrid);
end;

procedure TMain.MakeReadyToDraw(flag : boolean);
var
   i : integer;
begin
   ReadyToDraw := flag;
   for i := 2 to 6 do menu.rowActivate(2,i,flag);  {other plots}
   for i := 1 to 2 do menu.rowActivate(3,i,flag);  {extras}
end;


procedure TMain.SaveFile;
var
   f : text;
begin
   if FileName<>'' then           {save as...}
   begin
       assign(f,FileName);
       plates.SaveToFile(f);
   end;
end;

procedure TMain.ReadFile;
var
   f : text;
   maxCol,maxRow,i,j : integer;
   answer : string;
   ChargesOnlyFlag : boolean;
begin
   if plates.root<>nil then
   begin
      Ask('Save changes before overwritting?',answer);
      if answer='Yes' then
      begin
         If FileName='' then FileName := ReadFileName;
         SaveFile;
      end;
   end;
   if answer<>'Cancel' then
   begin
      FileName := OpenFile('*.cfg');
      if FileName<>'' then
      begin
         SelectViewPort(1);
         assign(f,FileName);
         plates.done;
         if plates.ReadFromFile(f,MaxRow,MaxCol,ChargesOnlyFlag) then
         begin
            potential.free;
            charges.  free;
            mask     .free;
            extra    .free;
            potential.Init(maxRow,maxCol);
            charges.  Init(maxRow,maxCol);
            mask     .init(maxRow,maxCol);
            extra    .init(maxRow,maxCol);
            Reset;
            ChargesOnly := ChargesOnlyFlag;
            PlotKind := Contours;
            If not chargesOnly then NotOnlyChargesPlot;
            Draw;
         end
         else Reset;
      end;
   end;
end;


procedure TMain.Draw;
var
   maxCol,maxRow,i,j : integer;
begin
   if (plates.root=nil) then
      announce('Please place your charges and plates first.')
   else
   begin
      if (plot3D.active or AllPlots.active) then
      begin
         CleanViewPort(2);             {deleate 3D plot}
         SetScreen;
      end;
      charges  .fill(0);                               {all charges = 0}
      potential.fill(plates.PlateSymbol);
      SetBoundary;
      Plates.ReMapAll;
      potential.GetSize(MaxRow,MaxCol);
      FOR i := 1 TO maxCol DO
       FOR j :=  1 TO maxRow DO
       begin
          mask.Put(j,i, (potential.value(j,i)<>Plates.PlateSymbol) );
          if (not GroundedBox) and (not mask.value(j,i)) then
           potential.put(j,i, 100*(1 - 2*(i-1)/(maxCol-1)));
       end;
      if ExactMethod then PotentialAroundCharges (potential,charges,ChargeRadius)
      else relaxation.draw(potential,charges,mask);
      if PlotKind=Div3D then FindDivergence(potential,Extra,charges,DFnc) else
      if PlotKind=Field3D then FindField(potential,Extra,DFnc);
      AdjustHotKeys(PlotKind,HotKeys);
      Plot3D.active := false;       {redraw 3D plots}
      if (plotKind<>Contours) then DrawPotential;
      MakeReadyToDraw(true);
      DrawPlates;
      scale.xMin:=0; scale.xMax:=0; scale.yMin:=0; scale.yMax:=0;
      DFnc.x := 1; DFnc.y := 1;
   end;
end;


{-------------------------------- Main Controls ----------------------------}


procedure DrawPotential;
begin
   with Main do
   begin
       case plotKind of
        Contours : relaxation.ReDraw(potential);
        vectors  : DrawFieldVectors(potential, mask);
        threeD   : plot3D.draw(potential,'Potential');
        Div3D    : plot3D.draw(Extra,'Charge');
        Field3D  : plot3D.draw(Extra,'Field Magnitude');
        all      : AllPlots.Init(potential,Charges,Extra, mask, DFnc);
       end;
       DrawPlates;
   end;
end;

procedure HandleModifyHotKeys(key : integer);
begin
   with Main do
   case key of
    1: ShowHelp('Poisson.hlp','Help on Modify');
    2: begin                               {remove plate}
          Plates.RemoveAllSelected(PlateKind,CurrentCharge);
          slider.Reset(CurrentCharge);
          icons.modify := plates.ModifyOn;
          icons.choice := PlateKind;
          icons.Update;
          if icons.modify = false then
          begin
             Plates.ModifyOn := true;
             ReadInput;                    {clean after modify}
          end;
       end;
    3: Plates.ChangeFunction;
    4: if menu.chosen then HandleMenu;                                  {!}
   end
end;

procedure HandleMainHotKeys(key : integer);
begin
   case key of
    1: ShowHelp('Poisson.hlp','General Help');
    2: if not Main.userFunction then Main.draw;
    3: Main.restart;
    4: if menu.chosen then HandleMenu;                                  {!}
   end;
end;


procedure Handle3DPlotHotKeys(key : integer);
begin
   case key of
    1: ShowHelp('Poisson.hlp','3D Plot Help');
    2: Plot3D.SetAnglesAndDraw;
    3: begin
          plot3D.done;
          HotKeys.key[2] := 'Return-Run';
          Main.Restart;
       end;
    4: if menu.chosen then HandleMenu;
   end;
end;

procedure Handle4PlotsHotKeys(key : integer);
begin
   case key of
    1: ShowHelp('Poisson.hlp','4 Plots Help');
    2: begin
          AllPlots.done;
          SetUpHotKeys(HotKeys);
          Main.Restart;
       end;
    3: if menu.chosen then HandleMenu;
   end;
end;


Procedure HandleFileMenu;
begin
   with Main do
   case Menu.RowChosen of
    1 : AboutCUPS;
    2 : ShowHelp('Poisson.hlp','About Program');
    3 : Configuration;
    5 : ReadFile;
    6 : begin
           If FileName='' then FileName := ReadFileName;
           SaveFile;
        end;
    7 : begin
           FileName := ReadFileName;
           SaveFile;
        end;
    9 : QuitFlag := true;
   end;
end;


Procedure HandlePlotHowMenu;
var
   maxCol,maxRow,i,j : integer;
   oldPlot : plotType;
begin
   with relaxation do                               {Plot How}
   with Main do
   begin
      OldPlot  := PlotKind;
      case menu.RowChosen of
       1: PlotKind := contours;
       2: PlotKind := vectors;
       3: PlotKind := threeD;
       4: begin PlotKind := Div3D; FindDivergence(potential,Extra,charges,DFnc); end;
       5: begin PlotKind := Field3D; FindField(potential,Extra,DFnc); end;
       6: PlotKind := all;
      end;
      AllPlots.done;
      plot3D.done;
      AdjustHotKeys(PlotKind, HotKeys);
      if (OldPlot<>contours) and (OldPlot<>vectors) and
        ((PlotKind=contours) or  (PlotKind=vectors)) then Main.SetScreen;
      DrawPotential;
   end;
end;

Procedure HandleExtrasMenu;
var
   StorePlate : ShapeType;
   flag,rectSys : boolean;
   i,j, maxRow,maxCol : integer;
begin
   if menu.rowChosen>3 then
   with Main do
   begin
      rectSys := (menu.rowChosen=4);
      if UserDefPotential.Process(RectSys, Potential, DFnc, scale) then
      begin
         PrepareUserFunc(true);
         if not (Plot3D.active or AllPlots.active) then
         begin
            CleanViewPort(2);
            SetScreen;
         end;
         charges.fill(0);                               {all charges = 0}
         if PlotKind=Div3D then FindDivergence(potential,Extra,charges,DFnc) else
         if PlotKind=Field3D then FindField(potential,Extra,DFnc);
         DrawPotential;
      end;
   end
   else
   begin
      StorePlate   := Icons.choice;
      Icons.choice := nil_;
      if (plot3D.active or AllPlots.active) then
      begin
         CleanViewPort(2);
         Main.setScreen;
      end else Icons.update;
      with main do
      case menu.rowChosen of
       1: DrawCrossSection( potential, DFnc, scale);
       2: DrawFieldLines( ReDraw, potential, charges, mask);
      end;
      Icons.choice := StorePlate;
      if (Plot3D.active or AllPlots.active) then CleanViewPort(2)
      else Main.SetScreen;
      Plot3D.active := false;    {to redraw 3D screen}
      menu.Display;
      HotKeys.Display;
      SelectViewPort(1);
      DrawPotential;
      ClearMKBuffers;
   end;
end;

Procedure HandleControlMenu;
begin
   case menu.rowChosen of                             {control}
    1: relaxation.readParameters;
    2: Main.resize;
    3: with Main do
       if ChooseAlgorithm then
       begin
          if not (plot3D.active or AllPlots.active) then DrawBoundary;
          DrawPlates;
          if readyToDraw then Draw;
       end;
    5: with Main do
       begin
          ShowGrid := not ShowGrid;
          menu.rowChecked(4,5,ShowGrid);
          if not (plot3D.active or AllPlots.active) then DrawGrid(ShowGrid);
       end;
    6: with Main.plates do
       begin
          DrawFromCenter := not DrawFromCenter;
          menu.RowChecked(4,6, DrawFromCenter);
       end;
    7: with Main do
       begin
          plates.AskForV := not plates.AskForV;
          menu.RowChecked(4,7, plates.AskForV);
       end;
   end;
end;

Procedure HandleHelpMenu;
begin
   case menu.rowChosen of
    1: ShowHelp('Poisson.hlp','General Help');
    2: ShowHelp('Poisson.hlp','How to use the program?');
    3: ShowHelp('Poisson.hlp','Help on Modify');
    4: ShowHelp('Poisson.hlp','About Plates');
   end;
end;


procedure HandleHotKeys(key : byte);
begin
   if Plot3D.active   then Handle3DPlotHotKeys(key) else
   if AllPlots.active then Handle4PlotsHotKeys(key) else
   if icons.modify    then HandleModifyHotKeys(key) else
   HandleMainHotKeys(key);
end;

procedure HandleMenu;
begin
   case menu.colChosen of
    1 : HandleFileMenu;
    2 : HandlePlotHowMenu;
    3 : HandleExtrasMenu;
    4 : HandleControlMenu;
    5 : HandleHelpMenu;
   end;
end;

procedure initialize;
begin
{MAC
   new(HotKeys);
   new(icons);
   new(menu);
   new(Main);
   new(Plot3D);
   new(relaxation);
   new(UserDefPotential);
   new(Slider);}
   SetUpHotKeys(HotKeys);
   SetUpMenu(menu);
   Main.init;
   relaxation.init;
   Plot3D.init;
   UserDefPotential.init;
   QuitFlag := false;
end;

procedure CleanUp;
begin
{MAC
   dispose(HotKeys);
   dispose(icons);
   dispose(menu);
   dispose(Main);
   dispose(Plot3D);
   dispose(relaxation);
   dispose(UserDefPotential);
   dispose(Slider);}
   UserDefPotential.Done;
   Menu.Done;
   Main.done;
end;

var key : byte;
BEGIN {Poisson_}
   CUPSinit;
   initialize;
   ShowHelp('Poisson.hlp','About Program');
   repeat
      CheckForEvents;
      if Plot3D.active then Plot3D.HandleSliders else
      if AllPlots.active then AllPlots.check
      else
        if not Main.UserFunction then
        with Main do
        begin
           PlateKind := icons.clicked;
           if PlateKind<>nil_ then CurrentCharge := slider.value;
           if icons.modify then ModifyPlates else ReadInput;
        end;
      if hotKeys.pressed(Key) then HandleHotKeys(key);
      if menu.activated then HandleMenu;
   until QuitFlag;
   CleanUp;
   CupsDone;
END.







