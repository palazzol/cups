           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.5 (95/02/13) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

{+-------------------------------------------------------------------}
{| PROCEDURES FOR MOTION GENERATOR for MECHANICS section of CUPS
{| Author:  Randall Jones
{|    Uses (10-03-92) CUPS utilities
{| Date:  {1/12/93}
{+-------------------------------------------------------------------}
UNIT GMUtil2;
INTERFACE
      USES Graph,Crt,Dos,CUPS,CUPSMUPP,CUPSgui,CupsGrph,CUPSProc;

{+-------------------------------------------------------------------}
{| Global Variable definitions:
{|    InitVals,InitNames,InitUnits ARRAY[1..7]  [t0,x0,vx0,y0,vy0,z0,vz0]
{| ForceDef Variable definitions
{|    VarVals ARRAY[0..10]               [none,t,x,vx,ax,y,vy,ay,z,vz,az]
{|    ParmVals ARRAY[1..10]
{|    VarNames,VarUnits,VarDesc ARRAY[0..30]
{|       [ 0..10] = [none,t,x,vx,ax,y,vy,ay,z,vz,az]
{|       [11..20] = User-Defined Functions
{|       [21..30] = ForceParameters
{+-------------------------------------------------------------------}
CONST
   MaxData=500;
   MaxGraphs=10;
TYPE
   String5 = String[5];
   OutWinData = RECORD
                LeftEdge, TopEdge, Width, Height: Real;
                HorizVar,VertVar:Integer;
                HMin,HMax,VMin,VMax:Real;
                PlotTraj,DisplayAxes,Transparent: Boolean;
                GraphTitle:STRING[80];
                END;
   ValsType = ARRAY[0..10] OF Real;
   ForceSystem = RECORD
         Name:STRING[6];
         ForceTitle: String[80];
         ForceDesc: String[80];
         ConfigFileName: STRING;
         NumDim,NumDynVars,NumParms,NumDynFuns:Integer;
         VarVals: ValsType;
         ParmVals: ARRAY[1..10] OF Real;
         VarNames: ARRAY[0..30] OF STRING[05];
         VarUnits: ARRAY[0..30] OF STRING[10];
         VarDesc : ARRAY[0..30] OF STRING[20];
         NumMeth : Integer;
         OutWin: ARRAY[1..MaxGraphs] OF OutWinData;
         NumOutWins: Integer;
         DisplayVars: ARRAY[1..3] OF Integer;
      END;
{+---------------------------------------------------------------------}
{| Global variables
{+---------------------------------------------------------------------}
VAR
   ColArray:ARRAY[0..5] OF BYTE;
   InitVals: ARRAY[1..7] of Real;
   InitNames: ARRAY[1..7] of String[06];
   InitUnits: ARRAY[1..7] of String[10];
   dt,dtAnim,dtAnimInit,dtAnimMax: Real;
   ForceDef:ForceSystem;
   DataStore: ARRAY[1..10,1..MaxData] OF REAL;
   NumPnts:Integer;
{+---------------------------------------------------------------------}
{| Utility variables
{+---------------------------------------------------------------------}
VAR
      NumProcs: ARRAY[1..5] OF String[10];
      NumNumMeth:Integer;
      xV,xVP:DVector;
{+---------------------------------------------------------------------}
{| Utility variables
{+---------------------------------------------------------------------}
CONST
      Blakk = Black;
      Whitte = White;
VAR
      ColVar: Integer;
      dtShift:Real;
      KeyHit:Byte;
      GenTimeVarCalled:Boolean;
      ResetFlag,ESCFlag,DoneFlag,RunFlag: Boolean;
      InWindow: ARRAY[1..10] OF Boolean;
TYPE
      AnimObj = Array[1..50] OF Integer;
VAR
      CircleData: AnimObj;
      PrintUnits: Array[0..3] OF String[8];
      PrintPos: Array[0..3] OF Integer;

{+---------------------------------------------------------------------}
{| Screen and Menu Variables.
{+---------------------------------------------------------------------}
VAR
      MainMenu: TMenu;
      HotKeys,SelWinKeys,ArrowKeys:THotKeys;
      GenericScreen:TInputScreen;
      CircleImage:TImage;
      UserHelpScreen:HelpScrType;
{+---------------------------------------------------------------------}
{| Public Procedures:
{+---------------------------------------------------------------------}
   FUNCTION FNum2Str(Num:Real; StrLen:Integer):String;
   PROCEDURE PutCircle(x1,y1:Real);
   PROCEDURE DrawSpring(x1,y1,x2,y2,SpringWidth:Real);
   PROCEDURE DrawVector(x,y,Vx,Vy:Real);
   PROCEDURE DrawCircle(xCen,yCen,Rho:REAL);
   PROCEDURE GenTimeVar(TimeName,TimeUnit:String);
   PROCEDURE GenDynVar(xName,vName,aName,xUnit:String);
   PROCEDURE GenForceParm(PName,PUnit,PDesc:String);
   PROCEDURE GenDynFun(FName,FUnit,FDesc:String);
   PROCEDURE GetCircle(VAR CircleData: AnimObj);
   PROCEDURE ChangeSpeed(VAR dtAnim:Real; UpDown:Integer);
   PROCEDURE PrintForceTitle;
   PROCEDURE PrintText;
   PROCEDURE ChangeColor(Direction:Integer);
   PROCEDURE SetUpMenu;
   PROCEDURE SetUpHotKeys;
   FUNCTION PadStr(TxtStr:STRING; StrLen:Integer):STRING;
   PROCEDURE GetGraphName(Index:Integer; VAR GraphName:String);
   PROCEDURE HiliteWin(Index:Integer);
   PROCEDURE GetPlotVar(PlotText:String; VAR PlotVar:Integer;
                    VAR ESCFlag:Boolean);
   PROCEDURE GetDisplayVars;

IMPLEMENTATION
{+---------------------------------------------------------------------}
{| Public Procedures:
{+---------------------------------------------------------------------}
   PROCEDURE GenTimeVar(TimeName,TimeUnit:String);
   BEGIN
      WITH ForceDef DO
         BEGIN
            VarNames[1] := TimeName;
            VarUnits[1] := TimeUnit;
            VarDesc[1]  := Concat(TimeName,' (',TimeUnit,')');
            InitNames[1] := Concat(TimeName,'0');
            InitUnits[1] := TimeUnit;
         END;
      GenTimeVarCalled := True;
   END;

   PROCEDURE GenDynVar(xName,vName,aName,xUnit:String);
   BEGIN
      IF NOT GenTimeVarCalled THEN
         Announce('ERROR in PROCEDURE DefForceParms - Call GenTimeVar before GenDynVar');
      IF ForceDef.NumDim = 3
         THEN
            Announce('ERROR in PROCEDURE DefForceParms - Too many dynamical variables')
         ELSE WITH ForceDef DO
            BEGIN
               NumDim := NumDim + 1;
               NumDynVars := 1 + 3*NumDim;
               VarNames[3*NumDim-1] := xName;
               VarUnits[3*NumDim-1] := xUnit;
               VarDesc[3*NumDim-1]  := Concat(xName,' (',xUnit,')');
               InitNames[2*NumDim] := Concat(xName,'0');
               InitUnits[2*NumDim] := xUnit;
               VarNames[3*NumDim] := vName;
               VarUnits[3*NumDim] := Concat(xUnit,'/',VarUnits[1]);
               VarDesc[3*NumDim]  := Concat(vName,' (',VarUnits[3*NumDim],')');
               InitNames[2*NumDim+1] := Concat(vName,'0');
               InitUnits[2*NumDim+1] := Concat(xUnit,'/',VarUnits[1]);
               VarNames[3*NumDim+1] := aName;
               VarUnits[3*NumDim+1] := Concat(xUnit,'/',VarUnits[1],'^2');
               VarDesc[3*NumDim+1] := Concat(aName,' (',VarUnits[3*NumDim+1],')');
            END;
   END;

   PROCEDURE GenForceParm(PName,PUnit,PDesc:String);
   BEGIN
      IF ForceDef.NumParms = 10
         THEN
            Announce('ERROR in PROCEDURE DefForceParms - Too many Force Parameters')
         ELSE WITH ForceDef DO
            BEGIN
               NumParms := NumParms + 1;
               VarNames[20+NumParms] := PName;
               VarUnits[20+NumParms] := PUnit;
               VarDesc[20+NumParms] := PDesc;
            END;
   END;

   PROCEDURE GenDynFun(FName,FUnit,FDesc:String);
   BEGIN
      IF ForceDef.NumDynFuns=10 THEN
         Announce('ERROR in PROCEDURE DefForceParms - Too many Dynamical Functions')
         ELSE WITH ForceDef DO
            BEGIN
               NumDynFuns := NumDynFuns + 1;
               VarNames[10+NumDynFuns] := FName;
               VarUnits[10+NumDynFuns] := FUnit;
               VarDesc[10+NumDynFuns] := FDesc+' ('+FUnit+')';
            END;
   END;

   PROCEDURE PutCircle(x1,y1:Real);
  {+------------------------------------------------------------------}
  {| This procedure draws a circle at the point (x1,y1) using the
  {| currently selected viewport and scale.
  {+------------------------------------------------------------------}
   VAR
      xScr,yScr:Integer;
   BEGIN
      Map(x1,y1,xScr,yScr);
      PutImage(xScr-5,yScr-5,CircleData,XORPut);
   END;

   PROCEDURE DrawSpring(x1,y1,x2,y2,SpringWidth:Real);
  {+------------------------------------------------------------------}
  {| This procedure draws a spring of width, SpringWidth between the
  {| points (x1,y1) and (x2,y2).  It uses the currently selected
  {| viewport and scale.
  {+------------------------------------------------------------------}
   VAR
      nDiv,i: Integer;
      x3,y3,x4,y4: Real;
      dx,dy,xOld,yOld,xNew,yNew: Real;
      dxUp,dyUp,dxDown,dyDown: Real;
      CosPhi,SinPhi: Real;
      SpringLen,StraightLen,Distance: Real;
      Loop:integer;
      EquilDist,EquilLen:real;

   BEGIN

   Distance := sqrt(ABS(sqr(x2-x1)+sqr(y2-y1)));
   CosPhi := (x2-x1)/Distance;
   SinPhi := (y2-y1)/Distance;
   EquilLen := 0.8*Distance;
   EquilDist:= Distance;
   StraightLen := (EquilDist - EquilLen)/2.0;
   SpringLen:= Distance - 2*StraightLen;
   x3 := x1 + StraightLen*CosPhi; y3 := y1 + StraightLen*SinPhi;
   x4 := x2 - StraightLen*CosPhi; y4 := y2 - StraightLen*SinPhi;

   PlotLine(x1,y1,x3,y3);
   PlotLine(x4,y4,x2,y2);

   xOld := x3; yOld := y3;
   nDiv:=8;
   dx := SpringLen/(4*nDiv);
   dy := SpringWidth;
      dxUp :=  dx*CosPhi + (-dy)*SinPhi;
      dyUp := -dx*SinPhi + (-dy)*CosPhi;
      dxDown :=  dx*CosPhi + dy*SinPhi;
      dyDown := -dx*SinPhi + dy*CosPhi;

      xNew := xOld + dxUp; yNew := yOld - dyUp;
      PlotLine(xOld,yOld,xNew,yNew);
      xOld := xNew ; yOld := yNew;
      FOR i := 1 to nDiv-1 DO
         BEGIN
            xNew := xOld + 2*dxDown;  yNew := yOld - 2*dyDown;
            PlotLine(xOld,yOld,xNew,yNew);
            xOld :=xNew; yOld := yNew;

            xNew := xOld + 2*dxUp;  yNew := yOld - 2*dyUp;
            PlotLine(xOld,yOld,xNew,yNew);
            xOld := xNew; yOld := yNew;
         END;
      xNew := xOld + 2*dxDown;  yNew := yOld - 2*dyDown;
      PlotLine(xOld,yOld,xNew,yNew);
      xOld :=xNew; yOld := yNew;

      xNew := xOld + dxUp; yNew := yOld - dyUp;
      PlotLine(xOld,yOld,xNew,yNew);
      xOld := xNew ; yOld := yNew;
   END; { DrawSpring}

   PROCEDURE DrawVector(x,y,Vx,Vy:Real);
  {+------------------------------------------------------------------}
  {| This procedure draws a vector with components Vx and Vy at the
  {| point (x,y).  It uses the currently selected viewport and scale.
  {+------------------------------------------------------------------}
   BEGIN
      Vector(MapX(x),MapY(y),MapX(x+Vx),MapY(y+Vy));
   END;

   PROCEDURE DrawCircle(xCen,yCen,Rho:REAL);
  {+------------------------------------------------------------------}
  {| This procedure draws a circle of radius Rho with center at the
  {| points (xCen,yCen).
  {| It uses the currently selected viewport and scale.
  {+------------------------------------------------------------------}
   VAR
      X1,Y1,X2,Y2,theta:Real;
      i:Integer;
   BEGIN
      X2 := xCen + Rho;
      Y2 := yCen;
      FOR i := 1 to 60 DO
         BEGIN
            X1 := X2;
            Y1 := Y2;
            theta := i*Pi/30;
            X2 := xCen + Rho*COS(theta);
            Y2 := yCen + Rho*SIN(theta);
            PlotLine(X1,Y1,X2,Y2);
         END;
   END;

   FUNCTION FNum2Str(Num:Real; StrLen:Integer):String;
  {+-------------------------------------------------------------------}
  {| This utility uses Num2Str(Num,Len) to generate a string of
  {| guaranteed total length, Len.
  {+-------------------------------------------------------------------}
   VAR
      TxtStr:String;
   BEGIN
      TxtStr := Num2Str(Num,StrLen);
      IF LENGTH(TxtStr) < StrLen
         THEN FNum2Str := PadStr(TxtStr,StrLen)
         ELSE FNum2Str := TxtStr;
   END;

{+---------------------------------------------------------------------}
{| Animation Routines
{+---------------------------------------------------------------------}
   {+------------------------------------------------------------------}
   {| Utilities
   {+------------------------------------------------------------------}
    PROCEDURE GetCircle(VAR CircleData: AnimObj);
    BEGIN
         SelectMUPPETport;
         SetColor(7); Circle(5,5,5); SetColor(15);
         GetImage(0,0,10,10,CircleData);
         PutImage(0,0,CircleData,XORPut);
    END;

    PROCEDURE ChangeSpeed(VAR dtAnim:Real; UpDown:Integer);
   {+----------------------------------------------------------------}
   {| This procedure uses a somewhat obscure technique to increase or
   {| decrease the value of dtAnim through the range
   {|   ... 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0 ...
   {| If dtAnim is increased, the next value of tNew is chosen so that it
   {| is a multiple of dtAnim.
   {+----------------------------------------------------------------}
    VAR
    LogVal,TruncVal:Real;
    Power,IntRep:integer;
    InvFlag,NegFlag:Boolean;
    BEGIN
       IF dtAnim<0 THEN
          BEGIN
             dtAnim := -dtAnim;
             NegFlag := True;
          END
       ELSE
          NegFlag := False;

       IF dtAnim<1.0 then
          BEGIN
             dtAnim:= 1.0/dtAnim;
             InvFlag:=True;
             UpDown:=-UpDown;
          END
       ELSE
           InvFlag:=False;
       LogVal:=ln(dtAnim)/ln(10.0);
       TruncVal:=LogVal - Int(LogVal);
       Power:=Round(LogVal-TruncVal);
       IntRep:=Round(10*TruncVal);
       if UpDown=1 THEN
           BEGIN
               CASE IntRep OF
                   0: dtAnim:=2.0;
                   3: dtAnim:=5.0;
                   7: dtAnim:=10.0;
               ELSE dtAnim:=10.0;
               END;
           END
       ELSE
           BEGIN
               CASE IntRep OF
                   0: dtAnim:=0.5;
                   3: dtAnim:=1.0;
                   7: dtAnim:=2.0;
               ELSE dtAnim:=1.0;
               END;
           END;
    {The following expression simply multiplies dtAnim by the
    {appropriate power of 10.  There should be a simpler method!}
        dtAnim:=dtAnim*exp(Power*ln(10.0));
        If InvFlag THEN dtAnim := 1.0/dtAnim;
        If NegFlag THEN dtAnim := -dtAnim;
    END;

   PROCEDURE PrintForceTitle;
   VAR
      xPos:Integer;
   BEGIN
      SelectMuppetPort;
      Rectangle(0,RowHt+1,GetMaxX,2*RowHt);
      SetFillStyle(SolidFill,LightGray);
      BAR(1,RowHt+2,GetMaxX-1,2*RowHt-1);
      SetColor(Red);
      xPos := 80-Length(ForceDef.ForceTitle);
      xPos := Round(xPos/2);
      Print(xPos,2,ForceDef.ForceTitle);
      SetColor(Whitte);
   END;

   PROCEDURE PrintText;
    {+---------------------------------------------------------------}
    {| This routine and the next write the current values of the
    {| dynamical variables to the screen.  This one writes the
    {| variable names and the other writes the numbers at each
    {| screen update.
    {+---------------------------------------------------------------}
   VAR
      NameTxt: STRING[5];
      i,dx,yPos,yScrPos: Integer;
   BEGIN
      SelectMuppetPort;
      yPos := 3;
      yScrPos := 2*RowHt;
      SelectMuppetPort;
      RubOut(1,yPos,80,Blakk);
      dx := Round(GetMaxX/4);
      FOR i:= 0 to 3 DO Rectangle(i*dx,yScrPos,(i+1)*dx-1,yScrPos+RowHt);
      Print(1,yPos,'dtAnim:');
      PrintPos[0] := 8;
      PrintUnits[0] := ForceDef.VarUnits[1];
      FOR i := 1 TO 3 DO WITH ForceDef DO
         IF DisplayVars[i]<> 0 THEN
            BEGIN
               NameTxt := VarNames[DisplayVars[i]];
               Print(20*i+1,yPos,Concat(NameTxt,':'));
               PrintPos[i] := 20*i + Length(NameTxt)+2;
               PrintUnits[i] := VarUnits[DisplayVars[i]];
            END;
   END;

   PROCEDURE ChangeColor(Direction:Integer);
   BEGIN
      ColVar:=(ColVar+6+Direction) MOD 6;
   END;

    PROCEDURE SetUpMenu;
    BEGIN
       MainMenu.Init;
       WITH MainMenu DO
          BEGIN
             Column(1,'FILE');
                Row(1,1,'About CUPS');
                Row(1,2,'About Program');
                Row(1,3,'Configuration');
                Row(1,4,'Load Parameters');
                Row(1,5,'Save Parameters');
                Row(1,6,'Save Data');
                Row(1,7,'EXIT program');
             Column(2,'Graphs');
                Row(2,1,'Reset Variables');
                Row(2,2,'Clear All');
                Row(2,3,'Clear All But Last');
                Row(2,4,'Change Color');
                Row(2,5,'Help');
             Column(3,'Parameters');
                Row(3,1,'Force');
                Row(3,2,'Initial Conditions');
                Row(3,3,'Numerical Parameters');
                Row(3,4,'Help');
             Column(4,'Help');
                Row(4,1,'General');
                Row(4,2,'User Defined');
             Column(5,'Windows');
                Row(5,1,'Modify Window Contents');
                Row(5,2,'Modify Numerical Window');
                Row(5,3,'Modify Window Layout');
                Row(5,4,'Modify User-Defined Help');
                Row(5,5,'Load Generic 2 Window Format');
                Row(5,6,'Load Generic 4 Window Format');
                Row(5,7,'Help');
          END;
    END;

    Procedure SetUpHotKeys;
    BEGIN
       WITH HotKeys DO BEGIN
          Init(6);
          key[1] := 'F2-Run';
          key[2] := 'F3-Step';
          key[3] := 'F4-Reverse';
          key[4] := 'F5-Slow-Down';
          key[5] := 'F6-Speed-Up';
          key[6] := 'F10-Menu';
       END;
       WITH SelWinKeys DO BEGIN
          Init(4);
          key[1] := 'ESC-Done';
          key[2] := 'RIGHT-Next';
          key[3] := 'LEFT-Prior';
          key[4] := 'RETURN-Select';
       END;
       WITH ArrowKeys DO BEGIN
          Init(6);
          key[1] := 'ESC-Quit';
          key[2] := 'UP-Up';
          key[3] := 'DOWN-Down';
          key[4] := 'LEFT-Left';
          key[5] := 'RIGHT-Right';
          key[6] := 'RETURN-Done';
       END;
    END;

{+-------------------------------------------------------------------}
{| Input Screens and Other User Input
{+-------------------------------------------------------------------}
  {+----------------------------------------------------------------}
  {| Utilities:
  {+----------------------------------------------------------------}
   FUNCTION PadStr(TxtStr:STRING; StrLen:Integer):STRING;
  {+----------------------------------------------------------------}
  {| This function takes TxtStr and returns a string of length StrLen
  {| that is truncated or padded with blanks.
  {+----------------------------------------------------------------}
   VAR
      i:Integer;
      TxtStrHere:STRING;
   BEGIN
      TxtStrHere := COPY(TxtStr,1,StrLen);
      FOR i := Length(TxtStrHere) TO StrLen-1 DO
         TxtStrHere := Concat(TxtStrHere,' ');
      PadStr := TxtStrHere;
   END;

   PROCEDURE GetGraphName(Index:Integer; VAR GraphName:String);
   VAR
      TxtStr1,TxtStr2:String[5];
   BEGIN
      WITH ForceDef.OutWin[Index] DO
         BEGIN
            TxtStr1 := ForceDef.VarNames[VertVar];
            TxtStr2 := ForceDef.VarNames[HorizVar];
            GraphName := Concat(TxtStr1,' VERSUS ',TxtStr2);
         END;
   END;

   PROCEDURE HiliteWin(Index:Integer);
   BEGIN
      IF Index=0 THEN EXIT;
      SetWriteMode(XORPut);
      HideCursor;
      WITH ForceDef.OutWin[Index] DO
         BEGIN
            Rectangle(-1+Round(LeftEdge*GetMaxX),
                      -1+Round(TopEdge*GetMaxY),
                       1+Round((LeftEdge+Width)*GetMaxX),
                       1+Round((TopEdge+Height)*GetMaxY));
            Rectangle( 1+Round(LeftEdge*GetMaxX),
                       1+Round(TopEdge*GetMaxY),
                      -1+Round((LeftEdge+Width)*GetMaxX),
                      -1+Round((TopEdge+Height)*GetMaxY));
         END;
      SetWriteMode(NormalPut);
      ShowCursor;
   END;

   PROCEDURE GetPlotVar(PlotText:String; VAR PlotVar:Integer;
                    VAR ESCFlag:Boolean);
  {+-------------------------------------------------------------------}
  {| This procedure generates a list of the dynamical variables and
  {| allows the user to select one for graphing.  Note that this procedure
  {| uses its own inputScreen since it is called from another screen.
  {+-------------------------------------------------------------------}
   VAR
      i:Integer;
      ButtonSelected:Integer;
      TxtStr:String;
      LocalGenericScreen:TInputScreen;
   BEGIN
      WITH LocalGenericScreen DO WITH ForceDef DO
         BEGIN
            init;
            DefineInputPort(0.2,0.9,0.05,0.45);
            LoadLine(Concat('Select Variable to Plot on ',PlotText,' axis'));
            LoadLine(' #1 None');
            LoadLine(Concat(' #1 ',VarNames[1]));
           {+----------------------------------------------------------}
           {| Add dynamical variables x,vx,ax,y,vy,ay,z,vz,ax
           {+----------------------------------------------------------}
            TxtStr := '';
            FOR i := 1 to NumDim DO
               LoadLine(Concat(' #1',PadStr(VarNames[3*i-1],8),   {Pos}
                            '    #1',PadStr(VarNames[3*i],8),     {Vel}
                            '    #1',PadStr(VarNames[3*i+1],8))); {Acc}
           {+----------------------------------------------------------}
           {| Add dynamical functions (5 per line)
           {+----------------------------------------------------------}
            TxtStr := '';
            FOR i := 1 to NumDynFuns DO
               BEGIN
                  TxtStr := Concat(TxtStr,' #1',PadStr(VarNames[10+i],8));
                  IF (i=5) THEN
                     BEGIN LoadLine(TxtStr); TxtStr := ''; END;
               END;
            IF NumDynFuns<>5 THEN LoadLine(TxtStr);
            LoadLine('');
            LoadLine('[  OK  ]      [Cancel]      [ Help ]');
           {+----------------------------------------------------------}
           {| Change boolean value to true for the current selected
           {| variable.
           {+----------------------------------------------------------}
            IF PlotVar<=NumDynVars
               THEN SetRadioButton('1',1+PlotVar)
               ELSE SetRadioButton('1',1+NumDynVars+PlotVar-10);
            SetHelpFile('GenMot.HLP','GetPlotVariableHelp');
            AcceptScreen;
            ESCFlag := Canceled;
            IF NOT ESCFlag THEN
               BEGIN
                  ButtonSelected := GetRadioButton('1');
                  IF ButtonSelected <= NumDynVars+1
                     THEN PlotVar := ButtonSelected-1
                     ELSE PlotVar := 10 + (ButtonSelected - NumDynVars-1);
               END;
            done;
         END;
   END;

   PROCEDURE GetDisplayVars;
  {+----------------------------------------------------------------+}
  {| This routine displays the list of dynamical variables and
  {| allows the user to select up to 3 for the numerical window.
  {+----------------------------------------------------------------+}
   VAR
      i,ind:Integer;
      TxtStr:String;
   BEGIN
      WITH GenericScreen DO WITH ForceDef DO
         BEGIN
            init;
            DefineInputPort(0.2,0.9,0.05,0.45);
            LoadLine('Select up to three variables for numerical window');
           {+----------------------------------------------------------}
           {| Add dynamical variables x,y,z/vx,vy,vz/ax,ay,az
           {+----------------------------------------------------------}
            LoadLine(Concat(' #F ',VarNames[1]));
            TxtStr := '';
            FOR i := 1 to NumDim DO
               LoadLine(Concat(' #F',PadStr(VarNames[3*i-1],8),   {Pos}
                            '    #F',PadStr(VarNames[3*i],8),     {Vel}
                            '    #F',PadStr(VarNames[3*i+1],8))); {Acc}
           {+----------------------------------------------------------}
           {| Add dynamical functions (5 per line)
           {+----------------------------------------------------------}
            TxtStr := '';
            FOR i := 1 to NumDynFuns DO
               BEGIN
                  TxtStr := Concat(TxtStr,' #F',PadStr(VarNames[10+i],8));
                  IF (i=5) THEN
                     BEGIN LoadLine(TxtStr); TxtStr := ''; END;
               END;
            IF NumDynFuns<>5 THEN LoadLine(TxtStr);
            LoadLine('');
            LoadLine('[  OK  ]      [Cancel]      [ Help ]');
           {+----------------------------------------------------------}
           {| Change boolean value to true for the current selected
           {| variable.
           {+----------------------------------------------------------}
            FOR i := 1 to 3 DO IF DisplayVars[i]<>0 THEN
               IF DisplayVars[i]<=NumDynVars
                  THEN SetBoolean(DisplayVars[i],TRUE)
                  ELSE SetBoolean(DisplayVars[i]-10+NumDynVars,TRUE);
            SetHelpFile('GenMot.HLP','GetDisplayVariableHelp');
            AcceptScreen;
            IF NOT Canceled THEN
               BEGIN
                  FOR i := 1 to 3 DO DisplayVars[i] := 0;
                  ind := 1;
                  FOR i := 1 TO NumDynVars DO
                     IF GetBoolean(i) THEN
                        BEGIN
                           DisplayVars[ind] := i;
                           ind := 1 + (ind MOD 3);
                        END;
                  FOR i := 1 to NumDynFuns DO
                     IF GetBoolean(i+NumDynVars) THEN
                        BEGIN
                           DisplayVars[ind] := 10 + i;
                           ind := 1 + (ind MOD 3);
                        END;
                  PrintText;
               END;
            done;
         END;
   END;

END.