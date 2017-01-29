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

{ Start of file SR_MINK.PAS }

{ ----------------------------------------------------------------------
   Program Title:    Minkowski Diagrams
   Author:           Douglas E. Brandt
   Version:          0.11
   Program Summary:  
   Library Files:    CRT,DOS,GRAPH,MUPPET,CUPS,CUPSGRPH,CUPSFUNC,CUPSPROC,
                     CUPSGUI,OBJECTS
   Utilities:
   Algorithms:
   NOTE:             This file can be compiled as a stand alone program
                     or as a unit.  If MAIN is defined by a compiler define
                     directive then this file will compile as a stand alone
                     program.  If MAIN is not defined by a compiler define
                     directive then this file will compile to a unit.
------------------------------------------------------------------------}

{$M 65520,0,640000}
{$N+}


USES Crt, Dos, Graph, CUPSmupp, CUPS, CUPSgrph, CUPSfunc,CUPSproc, CUPSgui,
        Objects,srminkx,srobj1,srobj2,srlist;




{**************************************************************************

   CONSTANT Declarations

***************************************************************************}

   CONST

      Short = 0;
      Long = 1;
      TAB = 9;
      ENTER = 13;

      Graph1 = 5;
      Graph2 = 6;

      Key1 = 1;
      Key2 = 2;
      Key3 = 3;
      Key4 = 4;
      Key5 = 5;
      Key6 = 6;
      Key7 = 7;
      Key8 = 8;
      Key9 = 9;
      Key0 = 10;
      KeyDecimal = 11;
      KeyPlusMinus = 12;
      KeyExp = 13;

      StickMode = 0;
      VectorMode = 1;
      TensorMode = 2;

      ORod = 1;
      OFlash = 3;
      OBall = 5;
      ORectangle = 7;
      ORocket = 2;
      OBarn = 4;
      OBlinker = 6;

      On = TRUE;
{**************************************************************************

   TYPE Declarations

***************************************************************************}

TYPE

   MessageRec = Record
      Quit: Boolean;
      Done: Boolean;
      Running: Boolean;
      Paused: Boolean;
      Next: Integer;
      Change: Integer;
      KeyPressed : Byte;
      ReInit: Boolean;
   END;

   FNameTp = ARRAY [1..200] OF String[12];

   SaveRecord = Record
      Description: DescriptionString;
      Graph1Type,Graph1RefFrame: Byte;
      Graph2Type,Graph2RefFrame: Byte;
      Graph1WorldLines,Graph2WorldLines: Boolean;
      Time: Integer;
      Betax,Betay,Betaz: Real;
      Mode: Byte;
      NumberOfObjects: Byte;
      ObjectData: ARRAY [1..61] OF Real;
   END;

   IVector = Object
   Value: ARRAY [1..100] OF Integer;
   END;

   TPoint = Record
   x,y: Real;
   END;

   ReferenceFrame = Record
   betax,betay,betaz,phi,eta,theta: Real;
   END;



   TObject = Object
   ObjectType: Integer;
   ObjectColor: Integer;
   END;


   TView = Object
   ViewType: Integer;
   END;

   TKeyGroup = Object
    Keys: ARRAY [1..4] OF Integer;
    Max: Byte;
    Selected: Byte;
   END;

   TKeyList = Object

   Key: ARRAY [1..50] OF TKey;
   Groups: ARRAY [1..6] OF TKeyGroup;
   PROCEDURE Init;
   PROCEDURE DrawKeys;
   PROCEDURE EraseKeys;
   PROCEDURE CreateKey(KeyNum: Integer;
                               x,y,Length: Real; KeyLabel: String;
                                  CanSelect,CanHold: Boolean; GroupNum: Byte);
   PROCEDURE DeleteKey(KeyNum: Integer);
   PROCEDURE Process(KeyPressed: Byte; KeyOnly: Boolean);
   PROCEDURE ProcessHoldKey(KeyPressed: Byte);

   END;


   TCalculator = Object
   Rotation: TSpaceVector;
   Sliders: TSliders;
   Keys: TKeyList;
   CalculatorBackColor, CalculatorViewPort, CalculatorScale: Integer;
   PROCEDURE Draw;
   END;

TPathFunction = Object
   x,y,z: FUNCTIONXY;
   END;

TObjectList = Object
   NumberDefined: Integer;
   CurrentInfo: Byte;
   ObjectPtr: ARRAY [1..6] OF ^GObject;
   PROCEDURE Init;
   PROCEDURE AddObject(ObjectType: Integer; Initial: InitRec);
   PROCEDURE DeleteObject(i : Integer);
   PROCEDURE DrawCurrent(t,lastt: Integer; GraphType:Integer);
   PROCEDURE DrawWorldTubes(GraphNum, GraphType: Integer);
   PROCEDURE Transform(Beta: TSpaceVector);
   PROCEDURE DrawCard;
   PROCEDURE DrawTab(i: Integer);
   PROCEDURE EraseTab(i: Integer);
   PROCEDURE ChangeCurrentInfo(i: Integer);
   END;

TCheckBox = Object
   Color: Integer;
   Points: ARRAY [1..2] OF TPoint;
   ScrnReg: Region;
   Checked: Boolean;
   PROCEDURE Init(x1,y1,x2,y2: Real);
   PROCEDURE Draw;
   PROCEDURE Check;
   PROCEDURE Uncheck;
END;
{***************************************************************************

   VARIABLE Declarations

***************************************************************************}

   VAR
      Calculator: TCalculator;
       normx,normy,normz: ARRAY [5..6] OF Real;
      knum: byte;
      KeyList: TKeyList;
      i,j,k: Integer;
      Continue,FirstTime,tempb: Boolean;
      a: Point;
      x,y,button: Integer;
      Scalar1,Scalar2: TScalar;
      Vector1,Vector2: TVector;
      Tensor1, Tensor2: TTensor;
      Slope: Real;
      P: Pointer;
      Size: Word;
      pal1: PaletteType;
      Message: MessageRec;
      t0: Integer;
      CurrentGraph1Type,CurrentGraph2Type: Integer;
      Graph1Frame,Graph2Frame: Byte;
      ObjectList: TObjectList;
      Initial: InitRec;
      timefield,bx,by,bz,gamma: TScalar;
      RunDelay: Integer;
      waitm: TwaitMessage;
      WorldTubes: ARRAY [5..6] OF Boolean;
      BadVelocity: Boolean;
      CurrentHelp: Byte;
      h1,m1,s1,hun1,h2,m2,s2,hun2: Word;
      WaitBeta: Boolean;
      TFlashFileXY: File OF TFlashPoints1;
      TFlashFileXYZ: File OF TFlashPoints1;
      TFlashFileXYT: File OF TFlashPoints2;
      checkbox: ARRAY [1..3] OF TCheckBox;
      CurrentRegion,SearchRegion,CurrentTensNum,CurrentVectNum: Integer;
      CurrentTensorPtr: ^TTensor;
      CurrentVectorPtr: ^TVector;
      Mode: Byte;
      OldGraph: ARRAY [5..6,0..24] OF Real;
      symmetry: Byte;
      SaveScreen: TInputScreen;
      InitData, CurrentData: SaveRecord;
{***************************************************************************

   Common Procedures Called By Methods

***************************************************************************}

PROCEDURE InitialDraw(t,GraphNum: Integer); FORWARD;
FUNCTION  ProcessMenu(VAR Message:MessageRec): Boolean; FORWARD;
FUNCTION  QueryEvent(VAR Message: MessageRec): Boolean; FORWARD;

{***************************************************************************

     TCalculator Methods

***************************************************************************}


PROCEDURE TCalculator.Draw;

   BEGIN
      GraphBackColor := CalculatorBackColor;
      OpenViewPort(CalculatorViewPort);
      SelectScale(CalculatorScale);
      Keys.DrawKeys;
      {PlotLine();}
      SetColor(MWhite);

      SetColor(MWhite);
   END;

{***************************************************************************

     Transformation Methods  "THE PHYSICS"

***************************************************************************}
PROCEDURE DrawTensor(GraphNum,GraphType: Integer); FORWARD;

PROCEDURE DrawVector(GraphNum,GraphType: Integer);

   VAR x,y,z,p,q,r,s: Real;
       THeVectorPtr: ^TVector;

   BEGIN
      SelectViewPort(GraphNum);

      SelectScale(7);
      SetWriteMode(XORPut);
      SetColor(MLightCyan+2);
      IF GraphType <= 4 THEN THeVectorPtr := @vector1
           ELSE TheVectorPtr := @vector2;
      WITH TheVectorPtr^ DO
      CASE GraphType OF
        XYGraph,XYPrimeGraph: 
                  BEGIN
                   PlotLine(0,0,Oldgraph[graphnum,1],oldgraph[graphnum,2]);
                   PlotLine(0,0,Field[1].Value,Field[2].Value);
                   Oldgraph[Graphnum,1] := Field[1].value;
                   Oldgraph[graphnum,2] := Field[2].value;
                  END;
        XYTGraph,XYTPrimeGraph:
                  BEGIN
                   PlotLine(0,0,oldgraph[graphnum,1],oldgraph[graphnum,2]);
                   SetColor(MDarkGray);
                   IF oldgraph[graphnum,1] <> 0 THEN
                      PlotLine(oldgraph[graphnum,1],oldgraph[graphnum,2],oldgraph[graphnum,1],oldgraph[graphnum,0]);
                   IF ABS(oldgraph[graphnum,7]) > 0.03 THEN
                        PlotLine(oldgraph[graphnum,3],oldgraph[graphnum,5],
                                 oldgraph[graphnum,1],oldgraph[graphnum,0]);
                   IF ABS(oldgraph[graphnum,8]) > 0.03 THEN
                        PlotLine(oldgraph[graphnum,4],oldgraph[graphnum,6],
                                oldgraph[graphnum,1],oldgraph[graphnum,0]);
                   p := cosRtheta*Field[1].Value;
                   q := sinRTheta*Field[2].Value;
                   r := -sinRTheta*Field[1].Value*sinRAlpha;
                   s := cosRTheta*Field[2].Value*sinRalpha;
                   x := p+q;
                   z := (r+s);
                   y := z + cosRAlpha*Field[0].Value;
                   SetColor(MlightCyan+2);
                   Plotline(0,0,x,y);
                   SetColor(MDarkGray);
                   IF x<> 0 THEN PlotLine(x,z,x,y);
                   IF ABS(Field[1].Value) > 0.03 THEN PlotLine(p,r,x,z);
                   IF ABS(Field[2].Value) > 0.03 THEN PlotLine(q,s,x,z);
                   oldgraph[graphnum,1] := x;
                   oldgraph[graphnum,2] := y;
                   oldgraph[graphnum,0] := z;
                   oldgraph[graphnum,3] := p;
                   oldgraph[graphnum,4] := q;
                   oldgraph[graphnum,5] := r;
                   oldgraph[graphnum,6] := s;
                   oldgraph[graphnum,7] := Field[1].Value;
                   oldgraph[graphnum,8] := Field[2].Value;
                  END;
        XTGraph,XTPrimeGraph:
                  BEGIN
                   PlotLine(0,0,oldgraph[graphnum,1],oldgraph[graphnum,2]);
                   PlotLine(0,0,Field[1].Value,Field[0].Value);
                   oldgraph[graphnum,1] := Field[1].value;
                   oldgraph[graphnum,2] := Field[0].value;
                  END;
        XYZGraph,XYZPrimeGraph:
                  BEGIN
                   PlotLine(0,0,oldgraph[graphnum,1],oldgraph[graphnum,2]);
                   SetColor(MDarkGray);
                   IF oldgraph[graphnum,1] <> 0 THEN
                      PlotLine(oldgraph[graphnum,1],oldgraph[graphnum,2],oldgraph[graphnum,1],oldgraph[graphnum,0]);
                   IF ABS(oldgraph[graphnum,7]) > 0.03 THEN
                      PlotLine(oldgraph[graphnum,3],oldgraph[graphnum,5],
                               oldgraph[graphnum,1],oldgraph[graphnum,0]);
                   IF ABS(oldgraph[graphnum,8]) > 0.03 THEN
                      PlotLine(oldgraph[graphnum,4],oldgraph[graphnum,6],
                                 oldgraph[graphnum,1],oldgraph[graphnum,0]);
                   p := cosRtheta*Field[1].Value;
                   q := sinRTheta*Field[2].Value;
                   r := -sinRTheta*Field[1].Value*sinRAlpha;
                   s := cosRTheta*Field[2].Value*sinRalpha;
                   x := p+q;
                   z := (r+s);
                   y := z + cosRAlpha*Field[3].Value;
                   SetColor(MlightCyan+2);
                   Plotline(0,0,x,y);
                   SetColor(MDarkGray);
                   IF x<> 0 THEN PlotLine(x,z,x,y);
                   IF ABS(Field[1].Value) > 0.03 THEN PlotLine(p,r,x,z);
                   IF ABS(Field[2].Value) > 0.03 THEN PlotLine(q,s,x,z);
                   oldgraph[graphnum,1] := x;
                   oldgraph[graphnum,2] := y;
                   oldgraph[graphnum,0] := z;
                   oldgraph[graphnum,3] := p;
                   oldgraph[graphnum,4] := q;
                   oldgraph[graphnum,5] := r;
                   oldgraph[graphnum,6] := s;
                   oldgraph[graphnum,7] := Field[1].Value;
                   oldgraph[graphnum,8] := Field[2].Value;
                  END;
      END;
      SetWriteMode(CopyPut);
   END;


{***************************************************************************

    TCheckBox Methods

***************************************************************************}
PROCEDURE TCheckBox.Init(x1,y1,x2,y2: Real);
   BEGIN
      Color := MBlack;
      SelectViewPort(1);
      SelectScale(1);
      Points[1].x := x1;
      Points[1].y := y1;
      Points[2].x := x2;
      Points[2].y := y2;
      ScrnReg.UpLeft.x := Mapx(x1);
      ScrnReg.UpLeft.y := Mapy(y2)+30;
      ScrnReg.LowRt.x := Mapx(x2);
      ScrnReg.LowRt.y := MapY(y1)+30;
   END;

PROCEDURE TCheckBox.Draw;

   BEGIN
      SetColor(Color);
      SelectViewPort(1);
      SelectScale(1);
      PlotLine(Points[1].x,Points[1].y,Points[1].x,Points[2].y);
      PlotLine(Points[1].x,Points[2].y,Points[2].x,Points[2].y);
      PlotLine(Points[2].x,Points[2].y,Points[2].x,Points[1].y);
      PlotLine(Points[2].x,Points[1].y,Points[1].x,Points[1].y);
      SetFillStyle(1,MWhite);
      FloodFill(MapX((Points[1].x+Points[2].x)/2),MapY((Points[1].y+
                            Points[2].y)/2),MBlack);
   END;


PROCEDURE TCheckBox.Check;

   BEGIN
      SetColor(Color);
      SelectViewPort(1);
      SelectScale(1);
      checked := True;
      PlotLine(Points[1].x,Points[1].y,Points[2].x,Points[2].y);
      PlotLine(Points[2].x,Points[1].y,Points[1].x,Points[2].y);
   END;

PROCEDURE TCheckBox.UnCheck;
   BEGIN
      checked := False;
      SetColor(MWhite);
      SelectViewPort(1);
      SelectScale(1);
      PlotLine(Points[1].x,Points[1].y,Points[2].x,Points[2].y);
      PlotLine(Points[2].x,Points[1].y,Points[1].x,Points[2].y);
      Draw;
   END;
{***************************************************************************

    TKeyList Methods

***************************************************************************}
PROCEDURE DrawAxes(GraphNum,GraphType: Integer); FORWARD;

PROCEDURE DrawCurrentAxes(t,GraphNum,GraphType: Integer); FORWARD;

PROCEDURE ChangeGraph(GraphNum,GraphType,Frame: Integer);

   VAR
      NoChange: Boolean;

   BEGIN
      NoChange := False;
      IF GraphNum = 5 THEN
         BEGIN
            IF ((CurrentGraph1Type=GraphType+4*(Frame-1)) AND (Frame=Graph1Frame)) THEN
               NoChange := True
            ELSE
               BEGIN
                  Menu.Rowchecked(3,CurrentGraph1Type-4*(Graph1Frame-1),False);
                  Menu.RowChecked(3,GraphType,true);
                  Menu.RowChecked(3,Graph1Frame+5,False);
                  Menu.RowChecked(3,Frame+5,True);
                  CurrentGraph1Type := GraphType+4*(Frame-1);
                  Graph1Frame := Frame;
               END;
         END;
      IF GraphNum = 6 THEN
         BEGIN
            IF ((CurrentGraph2Type=GraphType+4*(Frame-1)) AND (Frame=Graph2Frame)) THEN
               NoChange := True
            ELSE
               BEGIN
                  Menu.Rowchecked(4,CurrentGraph2Type-4*(Graph2Frame-1),False);
                  Menu.RowChecked(4,GraphType,true);
                  Menu.RowChecked(4,Graph2Frame+5,False);
                  Menu.RowChecked(4,Frame+5,True);
                  CurrentGraph2Type := GraphType+4*(Frame-1);
                  Graph2Frame := Frame;
               END;
         END;
      IF NoChange = False THEN InitialDraw(lasti,GraphNum);
   END;


PROCEDURE StepTime(Step: Integer);

   VAR
      t: Integer;

   BEGIN
      WITH ObjectList DO
      BEGIN
      t := lasti+Step;
      IF t > 191 THEN t := 1;
      IF t < 1 THEN t := 191;
      IF Mode = 0 THEN
       BEGIN
      SelectViewPort(Graph1);
      SelectScale(5);
      DrawCurrent(t,lasti,CurrentGraph1Type);
      {IF Not FirstTime THEN} DrawCurrentAxes(lasti,Graph1,CurrentGraph1Type);
      DrawCurrentAxes(t,Graph1,CurrentGraph1Type);
      SelectViewPort(Graph2);
      DrawCurrent(t,lasti,CurrentGraph2Type);
      {IF Not FirstTime THEN} DrawCurrentAxes(lasti,Graph2,CurrentGraph2Type);
      DrawCurrentAxes(t,Graph2,CurrentGraph2Type);
      selectScale(5);
       END;
      Timefield.field.changeValue((t-96)/96.0);
      lasti := t;
      FirstTime := False;
      END;
   END;

PROCEDURE RunTime;

   VAR
   Stop: Boolean;
   PreviousHelp: Byte;

   BEGIN
     HotKeys.Key[2] := 'F2-Stop';
     HotKeys.active[3] := False;
     HotKeys.active[4] := (RunDelay<>0);
     HotKeys.active[5] := true;
     HotKeys.Display;
     DisplayPrompt(3);
     PreviousHelp := CurrentHelp;
     CurrentHelp := 3;
     SelectViewPort(1);
     SelectSCale(1);
      MouseBorders(Mapx(43.5),MapY(53)+30,MapX(56.5),MapY(58)+30);
      Stop := False;
      WHILE NOT Stop DO
       BEGIN
        Delay(RunDelay);
        StepTime(1);
        CheckForEvents;
         IF event.MouseClicked THEN
           BEGIN
              Stop := True;
              SelectViewPort(1);
              SelectSCale(1);
              Calculator.keys.key[40].Depress;
              Calculator.keys.key[40].selected := False;
                REPEAT
                 BEGIN
                  MouseGlobalPosn(x,y,button);
                 END;
               UNTIL (button=0);
              Calculator.keys.key[40].unHighlight;
              Calculator.keys.key[40].selected := False;
           END;
         IF hotKeys.Pressed(kNum) THEN
            CASE kNum of
               2: BEGIN
                    Stop := True;
                    SelectViewPort(1);
                    SelectSCale(1);
                    Calculator.keys.key[40].unHighlight;
                    Calculator.keys.key[40].selected := False;
                  END;
               4: BEGIN
                    IF RunDelay > 10 THEN RunDelay := RunDelay - 10
                    ELSE
                     BEGIN
                      RunDelay := 0;
                      HotKeys.active[4] := False;
                      HotKeys.Display;
                     END;
                  END;
               5: BEGIN
                    RunDelay := RunDelay + 10;
                    IF NOT Hotkeys.active[4] THEN
                      BEGIN
                        HotKeys.Active[4] := True;
                        HotKeys.Display;
                      END;
                  END;
            END;
       END;
      MouseBorders(0,0,640,480);
      ClearMKBuffers;
      DisplayPrompt(1);
      CurrentHelp := PreviousHelp;
      HotKeys.Key[2] := 'F2-Run';
      HotKeys.active[3] := true;
      HotKeys.active[4] := False;
      HotKeys.active[5] := false;
      HotKeys.Display;
   END;


PROCEDURE ChangeBeta(kBx,kBy,kBz: Real; FieldOnly: Boolean);

   BEGIN
      IF ((((Bx.field.value < kBx) AND (Bx.field.value < 0.99)) OR
          ((Bx.field.value > kBx) AND (Bx.field.value > -0.99))) AND
          ((Sqr(kbx)+Sqr(Beta.y)+Sqr(beta.z)) < 0.99)) THEN
       BEGIN
         Bx.field.Changevalue(kBx);
         Beta.x := Bx.field.Value;
       END;
      IF ((((By.field.value < kBy) AND (By.field.value < 0.99)) OR
          ((By.field.value > kBy) AND (By.field.value > -0.99)))  AND
          ((Sqr(Beta.x)+Sqr(kby)+Sqr(beta.z)) < 0.99))THEN
       BEGIN
         By.field.Changevalue(kBy);
         Beta.y := By.field.Value;
       END;
      IF ((((Bz.field.value < kBz) AND (Bz.field.value < 0.99)) OR
          ((Bz.field.value > kBz) AND (Bz.field.value > -0.99))) AND
          ((Sqr(Beta.x)+Sqr(Beta.y)+Sqr(kbz)) < 0.99)) THEN
       BEGIN
         Bz.field.Changevalue(kBz);
         Beta.z := Bz.field.Value;
       END;
       Beta.Beta2 := Sqr(Beta.x)+Sqr(Beta.y)+Sqr(Beta.z);
       Beta.Gamma := 1/Sqrt(1-Beta.Beta2);
       Gamma.Field.ChangeValue(Beta.Gamma);
       IF NOT FieldOnly THEN
       CASE Mode OF
         0: ObjectList.ObjectPtr[ObjectList.CurrentInfo]^.Update(0,BadVelocity);
         1: BEGIN
               vector1.transform(normal,beta,beta,vector2);
               IF CurrentGraph1Type>4 THEN DrawVector(graph1,CurrentGraph1Type);
               IF CurrentGraph2Type>4 THEN DrawVector(graph2,CurrentGraph2Type);
            END;
         2: BEGIN
               tensor1.transform(normal,beta,beta,tensor2);
               IF CurrentGraph1Type>4 THEN DrawTensor(graph1,CurrentGraph1Type);
               IF CurrentGraph2Type>4 THEN DrawTensor(graph2,CurrentGraph2Type);
            END;
       END;
       GetTime(h1,m1,s1,hun1);
       WaitBeta := True;
   END;

PROCEDURE TKeyList.ProcessHoldKey(KeyPressed: Byte);

   BEGIN
      CASE KeyPressed OF
         38,39: StepTime(77-2*keypressed);
         43: ChangeBeta(BX.field.value+0.01,By.field.value,Bz.field.value,False);
         44: ChangeBeta(Bx.field.value-0.01,By.field.value,Bz.field.value,False);
         47: ChangeBeta(Bx.field.value,By.field.value+0.01,Bz.field.value,False);
         48: ChangeBeta(Bx.field.value,By.field.value-0.01,Bz.field.value,False);
         45: ChangeBeta(Bx.field.value,By.field.value,Bz.field.value+0.01,False);
         46: ChangeBeta(Bx.field.value,By.field.value,Bz.field.value-0.01,False);
         6:  BEGIN
             CASE Mode OF
             2:BEGIN
               CurrenttensorPtr^.Field[CurrentRegion DIV 4,
                                      CurrentRegion MOD 4].IncValue(0.02);
               CASE symmetry OF
                  2: CurrenttensorPtr^.Field[CurrentRegion MOD 4,
                        CurrentRegion DIV 4].ChangeValue(
                        CurrenttensorPtr^.Field[CurrentRegion DIV 4,
                        CurrentRegion MOD 4].Value);
                  3: BEGIN
                     CurrenttensorPtr^.Field[CurrentRegion MOD 4,
                        CurrentRegion DIV 4].ChangeValue(
                        -CurrenttensorPtr^.Field[CurrentRegion DIV 4,
                        CurrentRegion MOD 4].Value);
                      IF CurrentRegion MOD 4 = CurrentRegion DIV 4 THEN
                        CurrenttensorPtr^.Field[CurrentRegion MOD 4,
                        CurrentRegion DIV 4].ChangeValue(0);
                     END;
               END;
               IF currentTensnum = 1 THEN
                  tensor1.transform(normal,beta,beta,tensor2)
               ELSE tensor2.transform(inverse,beta,beta,tensor1);
               DrawTensor(graph1,CurrentGraph1Type);
               DrawTensor(graph2,CurrentGraph2Type);
               END;
             1:BEGIN
               CurrentvectorPtr^.Field[CurrentRegion].IncValue(0.02);
               IF currentvectnum = 1 THEN
                  vector1.transform(normal,beta,beta,vector2)
               ELSE vector2.transform(inverse,beta,beta,vector1);
               DrawVector(graph1,CurrentGraph1Type);
               DrawVector(graph2,CurrentGraph2Type);
               END;
            END;
            END;
         7:  BEGIN
             CASE Mode OF
             1:BEGIN
                CurrentvectorPtr^.Field[CurrentRegion].IncValue(-0.02);
               IF currentvectnum = 1 THEN
                  vector1.transform(normal,beta,beta,vector2)
               ELSE vector2.transform(inverse,beta,beta,vector1);
               DrawVector(graph1,CurrentGraph1Type);
               DrawVector(graph2,CurrentGraph2Type);
              END;
             2:BEGIN
               CurrenttensorPtr^.Field[CurrentRegion DIV 4,
                                      CurrentRegion MOD 4].IncValue(-0.02);
               CASE symmetry OF
                  2: CurrenttensorPtr^.Field[CurrentRegion MOD 4,
                        CurrentRegion DIV 4].ChangeValue(
                        CurrenttensorPtr^.Field[CurrentRegion DIV 4,
                        CurrentRegion MOD 4].Value);
                  3: BEGIN
                      CurrenttensorPtr^.Field[CurrentRegion MOD 4,
                        CurrentRegion DIV 4].ChangeValue(
                        -CurrenttensorPtr^.Field[CurrentRegion DIV 4,
                        CurrentRegion MOD 4].Value);
                      IF CurrentRegion MOD 4 = CurrentRegion DIV 4 THEN
                        CurrenttensorPtr^.Field[CurrentRegion MOD 4,
                        CurrentRegion DIV 4].ChangeValue(0);
                     END;
               END;
               IF currentTensnum = 1 THEN
                  tensor1.transform(normal,beta,beta,tensor2)
               ELSE tensor2.transform(inverse,beta,beta,tensor1);
               DrawTensor(graph1,CurrentGraph1Type);
               DrawTensor(graph2,CurrentGraph2Type);
               END;
            END;
              END;
      END;
   END;

PROCEDURE DisplayaddList(VAR i: Integer);

   VAR AddObjectScreen: TInputScreen;

   BEGIN
      WITH AddObjectScreen DO
      BEGIN
         init;
         DefineInputPort(0.16,0.49,0.12,0.38);
         Loadline(' Add Object Type: ');
         Loadline(' #1 Stick   #1 Rocket');
         Loadline(' #1 Flash   #1 Barn');
         Loadline(' #1 Ball    #1 Blinker');
         Loadline(' #1 Rectangle ');
         Loadline('    [  Ok  ] [Cancel]   ');
         AcceptScreen;
         i := GetRadioButton('1');
         IF Canceled THEN i := 0;
         Done;
      END;
  END;

PROCEDURE AddObject;

    VAR i,previous: Integer;
        memFail: Byte;

    BEGIN
     MemFail := 0;
     WITH ObjectList DO
      IF NumberDefined < 6 THEN
      BEGIN
       Previous := CurrentInfo;
       DrawTab(CurrentInfo);
       ChangeCurrentInfo(NumberDefined + 1);
       DisplayAddList(i);
       CASE i OF
          ORod: IF MaxAvail < sizeOf(TRod) THEN memfail := 1;
          OFlash : IF MaxAvail < sizeOf(TFlash) THEN memfail := 1;
          ORectangle: IF MaxAvail < sizeOf(TRectangle) THEN memfail := 1;
          OBall: IF MaxAvail < sizeOf(TBall) THEN memfail := 1;
          OBarn: IF MaxAvail < sizeOf(TBarn) THEN memfail := 1;
          ORocket: IF MaxAvail < sizeOf(TRocket) THEN memfail := 1;
       END;
        IF MemFail = 1 THEN
         BEGIN
           Announce('Not enough heap to add another object of this type.');
           ChangeCurrentInfo(Previous);
           IF NumberDefined <> 0 THEN EraseTab(NumberDefined+1);
         END
        ELSE
        IF i > 0 THEN
         BEGIN
          waitm.show;
          ObjectList.AddObject(i,Initial);
          ChangeCurrentInfo(NumberDefined);
          waitm.hide;
          InitialDraw(lasti,Graph1);
          InitialDraw(lasti,Graph2);
         END
       ELSE
         BEGIN
          ChangeCurrentInfo(Previous);
          IF NumberDefined <> 0 THEN EraseTab(NumberDefined+1);
         END;
      END
      ELSE Announce('Maximum number of objects already defined.');
      IF ObjectList.NumberDefined > 0 THEN
         BEGIN
            Calculator.Keys.Key[2].activate;
            Calculator.Keys.Key[3].activate;
            Calculator.Keys.Key[4].activate;
            Calculator.Keys.Key[5].activate;
         END;
   END;

PROCEDURE DeleteObject;

   BEGIN
      IF ObjectList.NumberDefined > 0 THEN
      BEGIN
      WITH ObjectList DO
        BEGIN
         IF NumberDefined > 0 THEN DeleteObject(CurrentInfo);
         IF NumberDefined > 0 THEN EraseTab(NumberDefined+1);
         IF CurrentInfo > NumberDefined THEN CurrentInfo := CurrentInfo-1;
         IF CurrentInfo = 0 THEN CurrentInfo := 1;
         ChangeCurrentInfo(CurrentInfo);
         IF NumberDefined = 0 THEN
            BEGIN
               Calculator.Keys.Key[2].Inactivate;
               Calculator.Keys.Key[3].Inactivate;
               Calculator.Keys.Key[4].Inactivate;
               Calculator.Keys.Key[5].Inactivate;
            END;
        END;
        InitialDraw(lasti,Graph1);
        InitialDraw(lasti,graph2);
        END;
   END;

PROCEDURE NextObject;

   BEGIN

      WITH ObjectList DO
         IF NumberDefined > 1 THEN
            ChangeCurrentInfo((CurrentInfo MOD NumberDefined)+1);
   END;

PROCEDURE EditObject;

   VAR LEave,RegionFound,NewRegion: Boolean;
      CurrentRegion,SearchRegion: Integer;
      TheKey: Char;

   BEGIN
      WITH ObjectList.ObjectPtr[ObjectList.CurrentInfo]^ DO
         BEGIN
            NewRegion := True;
            DisplayPrompt(2);
            CurrentHelp := 2;
            Leave := False;
           { CurrentRegion := InfoMin;}
            InfoRegions[CurrentRegion].HighLight(True);
            SelectViewPort(1);
            SelectScale(1);
            Calculator.Keys.Key[1].Inactivate;
            Calculator.Keys.Key[2].Inactivate;
           { Calculator.Keys.Key[3].Inactivate;}
            Calculator.Keys.Key[5].Inactivate;
            Calculator.Keys.Key[4].HighLight;
            WHILE Leave = FALSE DO
            BEGIN
            CheckForEvents;
            IF Event.KeyPressed THEN
              BEGIN
                TheKey := Event.ReadKey;
                IF (TheKey = CHR(TAB)) THEN
                  BEGIN
                    InfoRegions[CurrentRegion].HighLight(False);
                    CurrentRegion := CurrentRegion + 1;
                    IF NOT BAdVelocity THEN
                      BEGIN
                        IF CurrentRegion > NumberInfoRegions THEN
                                                    CurrentRegion := InfoMin;
                      END
                    ELSE IF CurrentRegion = 9 THEN CurrentRegion := 6
                    ELSE IF CurrentRegion = 16 THEN CurrentRegion := 13;
                    InfoRegions[CurrentRegion].HighLight(True);
                    NewRegion := True;
                  END;
                IF TheKey = CHR(ENTER) THEN Leave := True;
                IF TheKey IN EditChars THEN
                    BEGIN
                       IF NewRegion THEN InfoRegions[CurrentRegion].ChangeValue(0);
                       InfoRegions[CurrentRegion].Edit(TheKey);
                       Update(CurrentRegion,BadVelocity);
                       IF BadVelocity THEN
                          BEGIN
                          DisplayPrompt(5);
                          CurrentHelp := 5;
                          END;
                       NewRegion := False;
                    END;
              END;
            IF HotKeys.Pressed(knum) THEN
             case knum of
              1: ShowHelp(2);
              3: Leave := true;
{             6: IF menu.Chosen THEN ProcessMenu(Message);   }
             end;
            IF Event.MouseClicked THEN
              BEGIN
                a.x := Event.x;
                a.y := Event.y;
                WITH Calculator DO
                  BEGIN
                   IF Keys.Key[4].KeyRegion.PtInRegion(a) THEN
                       Leave := True;
                   IF Keys.Key[3].KeyRegion.PtInRegion(a) THEN
                     BEGIN
                       InfoRegions[CurrentRegion].HighLight(False);
                       CurrentRegion := CurrentRegion + 1;
                       IF NOT BAdVelocity THEN
                         BEGIN
                           IF CurrentRegion > NumberInfoRegions THEN
                                                    CurrentRegion := 1;
                         END
                       ELSE IF CurrentRegion = 9 THEN CurrentRegion := 6
                       ELSE IF CurrentRegion = 16 THEN CurrentRegion := 13;
                       InfoRegions[CurrentRegion].HighLight(True);
                       NewRegion := True;
                     END;
                  END;
                RegionFound := False;
                SearchRegion := 1;
                WHILE ((RegionFound = False) AND (SearchRegion <= NumberInfoRegions)) DO
                   BEGIN
                      IF InfoRegions[SearchRegion].ScrnReg.PtInRegion(a) THEN
                        BEGIN
                        IF (NOT BadVelocity) OR ((CurrentRegion < 9)
                             AND ((SearchRegion = 6) OR (SearchRegion = 7)
                              OR (SearchRegion = 8))) OR ((CurrentRegion > 9)
                             AND ((SearchRegion = 13) OR (SearchRegion = 14)
                              OR (SearchRegion = 15))) THEN
                        BEGIN
                           InfoRegions[CurrentRegion].HighLight(False);
                           CurrentRegion := SearchRegion;
                           InfoRegions[CurrentRegion].HighLight(True);
                           NewRegion := True;
                        END;
                        RegionFound := True;
                        END
                      ELSE SearchRegion := SearchRegion + 1;
                   END;
            END;
            IF BadVelocity THEN Leave := False;
         END;
         InfoRegions[CurrentRegion].HighLight(False);
         Calculator.Keys.Key[1].activate;
         Calculator.Keys.Key[2].activate;
         {Calculator.Keys.Key[3].activate;}
         Calculator.Keys.Key[5].activate;
         Calculator.Keys.Key[4].UnHighLight;
         Waitm.Show;
         Change;
         waitm.hide;
         DisplayPrompt(1);
         CurrentHelp := 1;
         InitialDraw(lasti,Graph1);
         InitialDraw(lasti,Graph2);
      END;
   END;

PROCEDURE ClearObjects;

   VAR i,j: Integer;

   BEGIN
      WITH ObjectList DO
       BEGIN
         j := NumberDefined;
         FOR i := 1 TO j DO EraseTab(i);
         WHILE j > 0 DO
          BEGIN
           DeleteObject(j);
           j := j - 1;
          END;

         ChangeCurrentInfo(1);
         DrawTab(1);
         Calculator.Keys.Key[2].Inactivate;
         Calculator.Keys.Key[3].Inactivate;
         Calculator.Keys.Key[4].Inactivate;
         Calculator.Keys.Key[5].Inactivate;
       END;
       InitialDraw(lasti,Graph1);
       InitialDraw(lasti,graph2);
   END;

PROCEDURE ToggleWorldLines(GraphNum: Integer);

   BEGIN
      WorldTubes[GraphNum] := NOT WorldTubes[GraphNum];
      Menu.RowChecked(GraphNum-2,9,WorldTubes[GraphNum]);
     IF Mode = 0 THEN
     CASE GraphNum OF
      Graph1:CASE CurrentGraph1Type OF
       XTGraph,XTPrimeGraph,XYTGraph,XYTPrimeGraph: InitialDraw(Lasti,Graph1);
        END;
      Graph2:CASE CurrentGraph2Type OF
       XTGraph,XTPrimeGraph,XYTGraph,XYTPrimeGraph: InitialDraw(Lasti,Graph2);
       END;
     END;
   END;

PROCEDURE TKeyList.Process(KeyPressed: Byte; KeyOnly: Boolean);

   BEGIN
      Key[Keypressed].depress;
      IF key[keypressed].holdable THEN
        BEGIN
           Delay(142);
           ProcessHoldKey(KeyPressed);
        END;
      REPEAT
       BEGIN
        MouseGlobalPosn(x,y,button);
        IF (button<>0) AND Key[KeyPressed].Holdable THEN
          ProcessHoldKey(KeyPressed);
       END;
      UNTIL (button=0);
      IF Key[KeyPressed].Selectable THEN
       BEGIN
        IF Key[KeyPressed].Group = 0 THEN
         BEGIN
          IF NOT Key[Keypressed].Selected THEN
           BEGIN
            Key[KeyPressed].Selected := True;
            Key[KeyPressed].Highlight;
           END
         ELSE
          BEGIN
           Key[KeyPressed].Selected := False;
           Key[KeyPressed].UnHighlight;
          END;
         END
        ELSE
         BEGIN
            Key[Groups[Key[KeyPressed].Group].Selected].UnHighLight;
            Key[KeyPressed].HighLight;
            Groups[Key[KeyPressed].Group].Selected := KeyPressed;
         END;
      END
      ELSE Key[KeyPressed].Release;
      IF NOT KeyOnly THEN
      CASE KeyPressed OF
         1: AddObject;
         2: DeleteObject;
         3: NextObject;
         4: EditObject;
         5: ClearObjects;
         18: ChangeGraph(Graph1,XTGraph,Graph1Frame);
         19: ChangeGraph(Graph1,XYGraph,Graph1Frame);
         20: ChangeGraph(Graph1,XYTGraph,Graph1Frame);
         21: ChangeGraph(Graph1,XYZGraph,Graph1Frame);
         22: ChangeGraph(Graph2,XTGraph,Graph2Frame);
         23: ChangeGraph(Graph2,XYGraph,Graph2Frame);
         24: ChangeGraph(Graph2,XYTGraph,Graph2Frame);
         25: ChangeGraph(Graph2,XYZGraph,Graph2Frame);
         26: ChangeGraph(Graph1,CurrentGraph1Type-4*(Graph1Frame-1),1);
         27: ChangeGraph(Graph1,CurrentGraph1Type-4*(Graph1Frame-1),2);
         28: ChangeGraph(Graph2,CurrentGraph2Type-4*(Graph2Frame-1),1);
         29: ChangeGraph(Graph2,CurrentGraph2Type-4*(Graph2Frame-1),2);
         40: RunTime;
         42: ToggleWorldLines(Graph1);
         41: ToggleWorldLines(Graph2);
      END;
   END;

PROCEDURE TKeyList.DrawKeys;

   BEGIN
      FOR i := 1 TO 50 DO
         BEGIN
            IF Key[i].Defined THEN Key[i].Draw;
         END;
   END;

PROCEDURE TKeyList.EraseKeys;

   BEGIN
      FOR i := 1 TO 50 DO
         BEGIN
         END;   
   END;

PROCEDURE TkeyList.CreateKey(KeyNum: Integer;
                               x,y,Length: Real; KeyLabel: String;
                                    CanSelect,CanHold: Boolean; GroupNum: Byte);
   VAR
      i: Integer;

   BEGIN
      Key[KeyNum].Create(x,y,KeyLabel,Length,CanSelect,CanHold,GroupNum);
      Key[KeyNum].selected := False;
      IF GroupNum <> 0 THEN
      WITH Groups[GroupNum]DO
       BEGIN
        Max := Max+1;
        Keys[Max] := KeyNum;
      END;
   END;

PROCEDURE TKeyList.Init;

   BEGIN
      FOR i := 1 TO 50 DO Key[i].Defined := False;
      FOR i := 1 TO 6 DO Groups[i].Max := 0;
      FOR i := 1 TO 6 DO Groups[i].Selected := 1;
   END;

PROCEDURE TKeyList.DeleteKey(KeyNum: Integer);

   BEGIN
      Key[KeyNum].Defined := False;
   END;




PROCEDURE DrawTensor(GraphNum,GraphType: Integer);

   VAR x,y,z,p,q,r,s: Real;
       TheTensorPtr: ^TTensor;
       xcrossy: ARRAY [1..3] OF Real;
       zdotxcrossy,fwddotxcrossy: Real;
       signx,signy,signz: Integer;

   FUNCTION signum(z: Real): Integer;
     BEGIN
      {IF z = 0 THEN signum := 0
       ELSE} IF z >= 0 THEN signum := 1
        ELSE signum := -1;
     END;

   PROCEDURE Plot2D;
      BEGIN
         SetColor(MYellow);
         PlotLine(oldgraph[graphnum,1],oldgraph[graphnum,2],
                        oldgraph[graphnum,3],oldgraph[graphnum,4]);
         SetColor(MLightCyan);
         PlotLine(-oldgraph[graphnum,1],-oldgraph[graphnum,2],
                         oldgraph[graphnum,3],oldgraph[graphnum,4]);
         SetColor(MLightMagenta);
         PlotLine(oldgraph[graphnum,1],oldgraph[graphnum,2],
                         -oldgraph[graphnum,3],-oldgraph[graphnum,4]);
         SetColor(MWhite);
         PlotLine(-oldgraph[graphnum,1],-oldgraph[graphnum,2],
                         -oldgraph[graphnum,3],-oldgraph[graphnum,4]);
      END;

      PROCEDURE Plot3D(Color: Integer);
        BEGIN
          SetWriteMode(XORPut);
          SelectScale(7);
            PlotLine(oldgraph[graphnum,1],oldgraph[graphnum,2],
                     oldgraph[graphnum,3],oldgraph[graphnum,4]);
            PlotLine(oldgraph[graphnum,1],oldgraph[graphnum,2],
                     oldgraph[graphnum,5],oldgraph[graphnum,6]);
            PlotLine(oldgraph[graphnum,1],oldgraph[graphnum,2],
                     oldgraph[graphnum,7],oldgraph[graphnum,8]);
{           END;  }
          IF (Normz[graphnum] > 0.0003) THEN
           BEGIN
            PlotLine(oldgraph[graphnum,3],oldgraph[graphnum,4],
                     oldgraph[graphnum,9],oldgraph[graphnum,10]);
            PlotLine(oldgraph[graphnum,3],oldgraph[graphnum,4],
                     oldgraph[graphnum,11],oldgraph[graphnum,12]);
           END;
          IF Normx[graphnum] > 0.0003 THEN
           BEGIN
            PlotLine(oldgraph[graphnum,5],oldgraph[graphnum,6],
                     oldgraph[graphnum,9],oldgraph[graphnum,10]);
            PlotLine(oldgraph[graphnum,5],oldgraph[graphnum,6],
                     oldgraph[graphnum,13],oldgraph[graphnum,14]);
           END;
          IF Normy[graphnum] > 0.0003 THEN
           BEGIN
            PlotLine(oldgraph[graphnum,7],oldgraph[graphnum,8],
                     oldgraph[graphnum,11],oldgraph[graphnum,12]);
            PlotLine(oldgraph[graphnum,7],oldgraph[graphnum,8],
                     oldgraph[graphnum,13],oldgraph[graphnum,14]);
           END;
       END;

   BEGIN
      SelectViewPort(GraphNum);
      SelectScale(7);
      SetWriteMode(XORPut);
      SetColor(MLightCyan+2);
      IF GraphType <= 4 THEN TheTensorPtr := @Tensor1
      ELSE TheTensorPtr := @Tensor2;
      WITH TheTensorPtr^ DO
      CASE symmetry OF
      1,2: BEGIN
        CASE GraphType OF
         XTGraph,XTPrimeGraph:
          BEGIN
            Plot2D;         
            oldgraph[graphnum,1] := Field[1,1].Value+Field[0,1].value;
            oldgraph[graphnum,2] := Field[1,0].Value+Field[0,0].value;
            oldgraph[graphnum,3] := Field[1,1].value-Field[0,1].value;
            oldgraph[graphnum,4] := Field[1,0].value-Field[0,0].value;
            Plot2D;
          END;
         XYGraph,XYPrimeGraph:
          BEGIN
            Plot2D;
            oldgraph[graphnum,1] := Field[1,1].Value+Field[2,1].value;
            oldgraph[graphnum,2] := Field[1,2].Value+Field[2,2].value;
            oldgraph[graphnum,3] := Field[1,1].value-Field[2,1].value;
            oldgraph[graphnum,4] := Field[1,2].value-Field[2,2].value;
            Plot2D;
          END;
         XYTGraph,XYTPrimeGraph:
            BEGIN
              Plot3D(MBlack);
              normx[graphnum] := sqr(field[1,1].value)+sqr(field[2,1].value)+sqr(field[3,1].value);
              normy[graphnum] := sqr(field[1,2].value)+sqr(field[2,2].value)+sqr(field[3,2].value);
              normz[graphnum] := sqr(field[1,3].value)+sqr(field[2,3].value)+sqr(field[3,3].value);
              xcrossy[1] := Field[2,1].value*Field[3,2].value-Field[3,1].value*Field[2,2].value;
              xcrossy[2] := Field[3,1].value*Field[1,2].value-Field[1,1].value*Field[3,2].value;
              xcrossy[3] := Field[1,1].value*Field[2,2].value-Field[2,1].value*Field[1,2].value;
              zdotxcrossy := Field[1,3].value*xcrossy[1]+Field[2,3].value*xcrossy[2]+
                              Field[3,3].value*xcrossy[3];
              fwddotxcrossy := cosRAlpha*(sinRTheta*xcrossy[1]-cosRTheta*xcrossy[2])+
                                sinRAlpha*xcrossy[3];
              signz := signum(zdotxcrossy*fwddotxcrossy);
              xcrossy[1] := Field[2,3].value*Field[3,1].value-Field[3,3].value*Field[2,1].value;
              xcrossy[2] := Field[3,3].value*Field[1,1].value-Field[1,3].value*Field[3,1].value;
              xcrossy[3] := Field[1,3].value*Field[2,1].value-Field[2,3].value*Field[1,1].value;
              zdotxcrossy := Field[1,2].value*xcrossy[1]+Field[2,2].value*xcrossy[2]+
                              Field[3,2].value*xcrossy[3];
              fwddotxcrossy := cosRAlpha*(sinRTheta*xcrossy[1]-cosRTheta*xcrossy[2])+
                                sinRAlpha*xcrossy[3];
              signy := signum(zdotxcrossy*fwddotxcrossy);
              xcrossy[1] := Field[2,2].value*Field[3,3].value-Field[3,2].value*Field[2,3].value;
              xcrossy[2] := Field[3,2].value*Field[1,3].value-Field[1,2].value*Field[3,3].value;
              xcrossy[3] := Field[1,2].value*Field[2,3].value-Field[2,2].value*Field[1,3].value;
              zdotxcrossy := Field[1,1].value*xcrossy[1]+Field[2,1].value*xcrossy[2]+
                              Field[3,1].value*xcrossy[3];
              fwddotxcrossy := cosRAlpha*(sinRTheta*xcrossy[1]-cosRTheta*xcrossy[2])+
                                sinRAlpha*xcrossy[3];
              signx := signum(zdotxcrossy*fwddotxcrossy);
              x := signz*Field[1,3].value+signx*Field[1,1].value+signy*Field[1,2].value;
              y := signz*Field[2,3].value+signx*Field[2,1].value+signy*Field[2,2].value;
              z := signz*Field[3,3].value+signx*Field[3,1].value+signy*Field[3,2].value;
              oldgraph[graphnum,1] := cosRTheta*x+sinRTheta*y;
              oldgraph[graphnum,2] := cosRAlpha*z+sinRAlpha*
                                                 (-sinRTheta*x+cosRTheta*y);
              x := -signz*Field[1,3].value+signx*Field[1,1].value+signy*Field[1,2].value;
              y := -signz*Field[2,3].value+signx*Field[2,1].value+signy*Field[2,2].value;
              z := -signz*Field[3,3].value+signx*Field[3,1].value+signy*Field[3,2].value;
              oldgraph[graphnum,3] := cosRTheta*x+sinRTheta*y;
              oldgraph[graphnum,4] := cosRAlpha*z+sinRAlpha*
                                                 (-sinRTheta*x+cosRTheta*y);
              x := signz*Field[1,3].value-signx*Field[1,1].value+signy*Field[1,2].value;
              y := signz*Field[2,3].value-signx*Field[2,1].value+signy*Field[2,2].value;
              z := signz*Field[3,3].value-signx*Field[3,1].value+signy*Field[3,2].value;
              oldgraph[graphnum,5] := cosRTheta*x+sinRTheta*y;
              oldgraph[graphnum,6] := cosRAlpha*z+sinRAlpha*
                                                 (-sinRTheta*x+cosRTheta*y);
              x := signz*Field[1,3].value+signx*Field[1,1].value-signy*Field[1,2].value;
              y := signz*Field[2,3].value+signx*Field[2,1].value-signy*Field[2,2].value;
              z := signz*Field[3,3].value+signx*Field[3,1].value-signy*Field[3,2].value;
              oldgraph[graphnum,7] := cosRTheta*x+sinRTheta*y;
              oldgraph[graphnum,8] := cosRAlpha*z+sinRAlpha*
                                                (-sinRTheta*x+cosRTheta*y);
              x := -signz*Field[1,3].value-signx*Field[1,1].value+signy*Field[1,2].value;
              y := -signz*Field[2,3].value-signx*Field[2,1].value+signy*Field[2,2].value;
              z := -signz*Field[3,3].value-signx*Field[3,1].value+signy*Field[3,2].value;
              oldgraph[graphnum,9] := cosRTheta*x+sinRTheta*y;
              oldgraph[graphnum,10] := cosRAlpha*z+sinRAlpha*
                                                 (-sinRTheta*x+cosRTheta*y);
              x := -signz*Field[1,3].value+signx*Field[1,1].value-signy*Field[1,2].value;
              y := -signz*Field[2,3].value+signx*Field[2,1].value-signy*Field[2,2].value;
              z := -signz*Field[3,3].value+signx*Field[3,1].value-signy*Field[3,2].value;
              oldgraph[graphnum,11] := cosRTheta*x+sinRTheta*y;
              oldgraph[graphnum,12] := cosRAlpha*z+sinRAlpha*
                                                 (-sinRTheta*x+cosRTheta*y);
              x := signz*Field[1,3].value-signx*Field[1,1].value-signy*Field[1,2].value;
              y := signz*Field[2,3].value-signx*Field[2,1].value-signy*Field[2,2].value;
              z := signz*Field[3,3].value-signx*Field[3,1].value-signy*Field[3,2].value;
              oldgraph[graphnum,13] := cosRTheta*x+sinRTheta*y;
              oldgraph[graphnum,14] := cosRAlpha*z+sinRAlpha*
                                                 (-sinRTheta*x+cosRTheta*y);
              Plot3D(MLightCyan+2);
           END;
         XYZGraph,XYZPrimeGraph:;
         END;
         END;
      3:BEGIN
         WITH TheTensorPtr^ DO
         CASE GraphType OF
         XYGraph,XYPrimeGraph: 
                  BEGIN
                   PlotLine(0,0,Oldgraph[graphnum,1],oldgraph[graphnum,2]);
                   PlotLine(0,0,Field[1,0].Value,Field[2,0].Value);
                   Oldgraph[Graphnum,1] := Field[1,0].value;
                   Oldgraph[graphnum,2] := Field[2,0].value;
                  END;
        XYTGraph,XYTPrimeGraph:
                  BEGIN
                   PlotLine(0,0,oldgraph[graphnum,1],oldgraph[graphnum,2]);
                   SetColor(MDarkGray);
                   IF ABS(oldgraph[graphnum,7]) > 0.03 THEN
                        PlotLine(oldgraph[graphnum,3],oldgraph[graphnum,5],
                                 oldgraph[graphnum,1],oldgraph[graphnum,2]);
                   IF ABS(oldgraph[graphnum,8]) > 0.03 THEN
                        PlotLine(oldgraph[graphnum,4],oldgraph[graphnum,6],
                                oldgraph[graphnum,1],oldgraph[graphnum,2]);
                   p := cosRtheta*Field[1,0].Value;
                   q := sinRTheta*Field[2,0].Value;
                   r := -sinRTheta*Field[1,0].Value*sinRAlpha;
                   s := cosRTheta*Field[2,0].Value*sinRalpha;
                   x := p+q;
                   z := (r+s);
                   SetColor(MlightCyan+2);
                   Plotline(0,0,x,z);
                   SetColor(MDarkGray);
                   IF ABS(Field[1,0].Value) > 0.03 THEN PlotLine(p,r,x,z);
                   IF ABS(Field[2,0].Value) > 0.03 THEN PlotLine(q,s,x,z);
                   oldgraph[graphnum,1] := x;
                   oldgraph[graphnum,2] := z;
                   oldgraph[graphnum,3] := p;
                   oldgraph[graphnum,4] := q;
                   oldgraph[graphnum,5] := r;
                   oldgraph[graphnum,6] := s;
                   oldgraph[graphnum,7] := Field[1,0].Value;
                   oldgraph[graphnum,8] := Field[2,0].Value;
                  END;
        XTGraph,XTPrimeGraph:
                  BEGIN
                   PlotLine(0,0,oldgraph[graphnum,1],0);
                   PlotLine(0,0,Field[1,0].Value,0);
                   oldgraph[graphnum,1] := Field[1,0].value;
                  END;
        XYZGraph,XYZPrimeGraph:
                  BEGIN
                   PlotLine(0,0,oldgraph[graphnum,1],oldgraph[graphnum,2]);
                   SetColor(MDarkGray);
                   IF oldgraph[graphnum,1] <> 0 THEN
                      PlotLine(oldgraph[graphnum,1],oldgraph[graphnum,2],oldgraph[graphnum,1],oldgraph[graphnum,0]);
                   IF ABS(oldgraph[graphnum,7]) > 0.03 THEN
                      PlotLine(oldgraph[graphnum,3],oldgraph[graphnum,5],
                               oldgraph[graphnum,1],oldgraph[graphnum,0]);
                   IF ABS(oldgraph[graphnum,8]) > 0.03 THEN
                      PlotLine(oldgraph[graphnum,4],oldgraph[graphnum,6],
                                 oldgraph[graphnum,1],oldgraph[graphnum,0]);
                   p := cosRtheta*Field[1,0].Value;
                   q := sinRTheta*Field[2,0].Value;
                   r := -sinRTheta*Field[1,0].Value*sinRAlpha;
                   s := cosRTheta*Field[2,0].Value*sinRalpha;
                   x := p+q;
                   z := (r+s);
                   y := z + cosRAlpha*Field[3,0].Value;
                   SetColor(MlightCyan+2);
                   Plotline(0,0,x,y);
                   SetColor(MDarkGray);
                   IF x<> 0 THEN PlotLine(x,z,x,y);
                   IF ABS(Field[1,0].Value) > 0.03 THEN PlotLine(p,r,x,z);
                   IF ABS(Field[2,0].Value) > 0.03 THEN PlotLine(q,s,x,z);
                   oldgraph[graphnum,1] := x;
                   oldgraph[graphnum,2] := y;
                   oldgraph[graphnum,0] := z;
                   oldgraph[graphnum,3] := p;
                   oldgraph[graphnum,4] := q;
                   oldgraph[graphnum,5] := r;
                   oldgraph[graphnum,6] := s;
                   oldgraph[graphnum,7] := Field[1,0].Value;
                   oldgraph[graphnum,8] := Field[2,0].Value;
                  END;
      END;
      SetColor(MLightMagenta+1);
      WITH TheTensorPtr^ DO
      CASE GraphType OF
        XYGraph,XYPrimeGraph: 
                  BEGIN
                   PlotLine(0,0,Oldgraph[graphnum,11],oldgraph[graphnum,12]);
                   PlotLine(0,0,Field[3,2].Value,Field[1,3].Value);
                   Oldgraph[Graphnum,11] := Field[3,2].value;
                   Oldgraph[graphnum,12] := Field[1,3].value;
                  END;
        XYTGraph,XYTPrimeGraph:
                  BEGIN
                   PlotLine(0,0,oldgraph[graphnum,11],oldgraph[graphnum,12]);
                   SetColor(MDarkGray);
                   IF ABS(oldgraph[graphnum,17]) > 0.03 THEN
                        PlotLine(oldgraph[graphnum,13],oldgraph[graphnum,15],
                                 oldgraph[graphnum,11],oldgraph[graphnum,12]);
                   IF ABS(oldgraph[graphnum,18]) > 0.03 THEN
                        PlotLine(oldgraph[graphnum,14],oldgraph[graphnum,16],
                                oldgraph[graphnum,11],oldgraph[graphnum,12]);
                   p := cosRtheta*Field[3,2].Value;
                   q := sinRTheta*Field[1,3].Value;
                   r := -sinRTheta*Field[3,2].Value*sinRAlpha;
                   s := cosRTheta*Field[1,3].Value*sinRalpha;
                   x := p+q;
                   z := (r+s);
                   SetColor(MlightMagenta+1);
                   Plotline(0,0,x,z);
                   SetColor(MDarkGray);
                   IF ABS(Field[3,2].Value) > 0.03 THEN PlotLine(p,r,x,z);
                   IF ABS(Field[1,3].Value) > 0.03 THEN PlotLine(q,s,x,z);
                   oldgraph[graphnum,11] := x;
                   oldgraph[graphnum,12] := z;
                   oldgraph[graphnum,13] := p;
                   oldgraph[graphnum,14] := q;
                   oldgraph[graphnum,15] := r;
                   oldgraph[graphnum,16] := s;
                   oldgraph[graphnum,17] := Field[3,2].Value;
                   oldgraph[graphnum,18] := Field[1,3].Value;
                  END;
        XTGraph,XTPrimeGraph: 
                  BEGIN
                   PlotLine(0,0,oldgraph[graphnum,11],0);
                   PlotLine(0,0,Field[3,2].Value,0);
                   oldgraph[graphnum,11] := Field[3,2].value;
                   oldgraph[graphnum,12] := Field[1,3].value;
                  END;
        XYZGraph,XYZPRimeGraph:
                  BEGIN
                   PlotLine(0,0,oldgraph[graphnum,11],oldgraph[graphnum,12]);
                   SetColor(MDarkGray);
                   IF oldgraph[graphnum,11] <> 0 THEN
                      PlotLine(oldgraph[graphnum,11],oldgraph[graphnum,12],oldgraph[graphnum,11],oldgraph[graphnum,10]);
                   IF ABS(oldgraph[graphnum,17]) > 0.03 THEN
                      PlotLine(oldgraph[graphnum,13],oldgraph[graphnum,15],
                               oldgraph[graphnum,11],oldgraph[graphnum,10]);
                   IF ABS(oldgraph[graphnum,18]) > 0.03 THEN
                      PlotLine(oldgraph[graphnum,14],oldgraph[graphnum,16],
                                 oldgraph[graphnum,11],oldgraph[graphnum,10]);
                   p := cosRtheta*Field[3,2].Value;
                   q := sinRTheta*Field[1,3].Value;
                   r := -sinRTheta*Field[3,2].Value*sinRAlpha;
                   s := cosRTheta*Field[1,3].Value*sinRalpha;
                   x := p+q;
                   z := (r+s);
                   y := z + cosRAlpha*Field[2,1].Value;
                   SetColor(MlightMagenta+1);
                   Plotline(0,0,x,y);
                   SetColor(MDarkGray);
                   IF x<> 0 THEN PlotLine(x,z,x,y);
                   IF ABS(Field[3,2].Value) > 0.03 THEN PlotLine(p,r,x,z);
                   IF ABS(Field[1,3].Value) > 0.03 THEN PlotLine(q,s,x,z);
                   oldgraph[graphnum,11] := x;
                   oldgraph[graphnum,12] := y;
                   oldgraph[graphnum,10] := z;
                   oldgraph[graphnum,13] := p;
                   oldgraph[graphnum,14] := q;
                   oldgraph[graphnum,15] := r;
                   oldgraph[graphnum,16] := s;
                   oldgraph[graphnum,17] := Field[3,2].Value;
                   oldgraph[graphnum,18] := Field[1,3].Value;
                  END;
      END;
      END;
   END;
   SetWriteMode(CopyPut);
   SelectViewPort(1);
   SelectScale(1);
   END;


PROCEDURE InitializeFlashes;

   BEGIN
      FlashXY.init;
      FlashXYZ.init;
      FlashXYT.init;
      Assign(TflashFileXY,'FlashXY.dat');
      Reset(TFlashFileXY);
      read(TflashFileXY,FlashXY.PointsPtr^);
      Close(TFlashFileXY);
      Assign(TflashFileXYZ,'FlashXYZ.dat');
      Reset(TFlashFileXYZ);
      read(TflashFileXYZ,FlashXYZ.PointsPtr^);
      Close(TFlashFileXYZ);
      Assign(TflashFileXYT,'FlashXYT.dat');
      Reset(TFlashFileXYT);
      read(TflashFileXYT,FlashXYT.PointsPtr^);
      Close(TFlashFileXYT);
   END;

PROCEDURE TObjectList.Init;

   BEGIN
      NumberDefined := 0;
      FOR i := 1 TO 6 DO ObjectPtr[i] := nil;
   END;


PROCEDURE TObjectList.AddObject(ObjectType: Integer; Initial: InitRec);

   VAR i: Integer;

   BEGIN
      IF NumberDefined < 6 THEN
         BEGIN
            i := 1;
            WHILE ObjectPtr[i] <> Nil DO i := i + 1;
            CASE ObjectType OF
               ORod: ObjectPtr[i] := New(TRodPtr,Init(Initial));
               OFlash : ObjectPtr[i] := New(TFlashPtr,Init(Initial));
               ORectangle: ObjectPtr[i] := New(TRectanglePtr,Init(Initial));
               OBall: ObjectPtr[i] := New(TBallPtr,Init(Initial));
               OBarn: ObjectPtr[i] := New(TBarnPtr,Init(Initial));
               ORocket: ObjectPtr[i] := New(TRocketPtr,Init(Initial));
            END;
            NumberDefined := NumberDefined + 1;
            ObjectPtr[i]^.Transform(Beta);
         END
      ELSE Announce('Maximum number of objects already defined.');
   END;

PROCEDURE TObjectList.DeleteObject(i: Integer);

   BEGIN
      Dispose(ObjectPtr[i],Done);
      NumberDefined := NumberDefined-1;
      IF NumberDefined <> 0 THEN FOR j := i TO NumberDefined DO
          IF ObjectPtr[j+1] <> NIL THEN ObjectPtr[j] := ObjectPtr[j+1];
      ObjectPtr[NumberDefined+1] := NIL;
   END;

PROCEDURE InitialDraw(t,GraphNum: Integer);

   VAR i: Integer;

   BEGIN
      OpenViewPort(GraphNum);
      SelectScale(Graph1);
      IF GraphNum = Graph1 THEN
       BEGIN
         IF mode = 0 THEN
          IF WorldTubes[GraphNum] = On THEN
            ObjectList.DrawWorldTubes(GraphNum,CurrentGraph1Type);
         DrawAxes(GraphNum,CurrentGraph1Type);
         LabelAxes(GraphNum,CurrentGraph1Type);
         SetColor(MWhite);
         PlotLine(-1,-1,-1,1);
         PlotLine(-1,1,1,1);
         PlotLine(1,1,1,-1);
         PlotLine(1,-1,-1,-1);
         CASE Mode OF
           0: ObjectList.DrawCurrent(t,-1000,CurrentGraph1Type);
           1: BEGIN
                 FOR i := 0 TO 24 DO oldgraph[graphnum,i] := 0;
                 DrawVector(Graph1,CurrentGraph1Type);
              END;
           2: BEGIN
                 IF symmetry = antisymmetric THEN
                    BEGIN
                       SelectViewPort(graph1);
                       SelectScale(5);
                       SetColor(MlightCyan);
                       Outtextxy(MapX(-0.9),MapY(-0.9),'E');
                       PlotLine(-0.8,-0.94,-0.6,-0.94);
                       SetColor(MlightMagenta);
                       Outtextxy(MapX(-0.5),MapY(-0.9),'B');
                       PlotLine(-0.4,-0.94,-0.2,-0.94);
                    END;
                 FOR i := 0 TO 24 DO oldgraph[graphnum,i] := 0;
                 DrawTensor(Graph1,CurrentGraph1Type);
              END;
         END;
         IF Mode = 0 THEN DrawCurrentAxes(t,GraphNum,CurrentGraph1type);
       END
      ELSE
       BEGIN
         IF Mode = 0 THEN
          IF WorldTubes[GraphNum] = On THEN
            ObjectList.DrawWorldTubes(GraphNum,CurrentGraph2Type);
         DrawAxes(GraphNum,CurrentGraph2Type);
         LabelAxes(GraphNum,CurrentGraph2Type);
         SetColor(MWhite);
         PlotLine(-1,-1,-1,1);
         PlotLine(-1,1,1,1);
         PlotLine(1,1,1,-1);
         PlotLine(1,-1,-1,-1);
         CASE Mode OF
          0: ObjectList.DrawCurrent(t,-1000,CurrentGraph2Type);
          1: BEGIN
                 FOR i := 0 TO 24 DO oldgraph[graphnum,i] := 0;
                 DrawVector(Graph2,CurrentGraph2Type);
              END;
          2: BEGIN
                 IF symmetry = antisymmetric THEN
                    BEGIN
                       SelectViewPort(graph2);
                       SelectScale(5);
                       SetColor(MlightCyan);
                       Outtextxy(MapX(-0.9),MapY(-0.9),'E');
                       PlotLine(-0.8,-0.94,-0.6,-0.94);
                       SetColor(MlightMagenta);
                       Outtextxy(MapX(-0.5),MapY(-0.9),'B');
                       PlotLine(-0.4,-0.94,-0.2,-0.94);
                    END;
                 FOR i := 0 TO 24 DO oldgraph[graphnum,i] := 0;
                 DrawTensor(graph2,CurrentGraph2Type);
             END;
         END;
         IF Mode = 0 THEN DrawCurrentAxes(t,GraphNum,CurrentGraph2type);
       END;
   END;

PROCEDURE TObjectList.DrawCurrent(t,lastt,GraphType:Integer);

   VAR
      i: Integer;

   BEGIN
      FOR i := 1 TO 6 DO
         IF ObjectPtr[i] <> NIL THEN ObjectPtr[i]^.DrawCurrent(t,lastt,GraphType);
   END;

PROCEDURE TObjectList.DrawWorldTubes(GraphNum,GraphType: Integer);
   BEGIN
      FOR i := 1 TO 6 DO
         IF ObjectPtr[i] <> NIL THEN ObjectPtr[i]^.DrawWorldTube(GraphNum,GraphType);
   END;

PROCEDURE TObjectList.Transform(Beta: TSpaceVector);
   BEGIN
      FOR i := 1 TO 6 DO
         IF ObjectPtr[i] <> NIL THEN
                    ObjectPtr[i]^.Transform(Beta);
   END;

{PROCEDURE TObjectList.WriteObjectInfo;

   BEGIN
   END;}


PROCEDURE DrawAxes(GraphNum,GraphType: Integer);

   VAR
      y: Real;

   BEGIN
      SelectViewPort(GraphNum);
      SelectScale(5);
      HideCursor;
      SetColor(MDarkGray);
      CASE GraphType OF
         XYTGraph,XYTPrimeGraph,XYZGraph,XYZPrimeGraph:
            BEGIN
               y := cosRTheta*sinRAlpha/sinRTheta;
               PlotLine(0,tmin,0,tmax);
               PlotLine(1,y,-1,-y);
               y := sinRTheta*sinRAlpha/cosRTheta;
               PlotLine(1,-y,-1,y);
            END;
         XTGraph,XTPrimeGraph,XYGraph,XYPrimeGraph:
            BEGIN
               PlotLine(0,tmin,0,tmax);
               PlotLine(tmin,0,tmax,0);
            END;
      END;
      ShowCursor;
  END;

PROCEDURE DrawCurrentAxes(t,GraphNum,GraphType: Integer);

   VAR
      y: Integer;

   BEGIN
      SelectViewPort(GraphNum);
      SelectScale(5);
      SetLineStyle(SolidLn,0,1);
      SetWriteMode(XORPut);
      HideCursor;
      SetColor(MDarkGray);
      IF t <> 96 THEN CASE GraphType OF
         XYTGraph,XYTPrimeGraph:
            BEGIN
               {Line(0,-28+tbottom+1-t,192,28+tbottom+1-t);}
               Line(0,-28+tbottom-t,192,28+tbottom-t);
               {Line(0,83+tbottom+1-t,192,-83+tbottom+1-t);}
               Line(0,83+tbottom-t,192,-83+tbottom-t);
            END;
         XTGraph,XTPrimeGraph:
            BEGIN
               PlotLine(tmin,96-t,tmax,96-t);
            END;
      END;
      SetWriteMode(CopyPut);
      ShowCursor;

  END;

PROCEDURE TObjectList.EraseTab(i:Integer);

   VAR x1: Real;

   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      x1 := 6*(i-1)+14;
      HideCursor;
      SetColor(DeskTopColor);
      OuttextXY(MapX(x1+3.5),MapY(42.5),'#'+Num2Str(i,1));
      SetColor(MWhite);
      PlotLine(1.5+x1,40,7.5+x1,40);
      SetFillStyle(1,DeskTopColor);
      FloodFill(MapX(x1+3.5),MapY(42.5),MWhite);
      SetColor(DeskTopColor);
      PlotLine(1.5+x1,40,2+x1,40.5);
      PlotLine(2+x1,40.5,2+x1,43);
      PlotLine(2+x1,43,3+x1,44);
      PlotLine(3+x1,44,6+x1,44);
      PlotLine(6+x1,44,7+x1,43);
      PlotLine(7+x1,43,7+x1,40.5);
      PlotLine(7+x1,40.5,7.5+x1,40);
      ShowCursor;

   END;

PROCEDURE TObjectList.DrawTab(i: Integer);

   VAR
      x1,x2: Integer;

   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      SetColor(MWhite);
      x1 := 6*(i-1)+14;
      IF i = CurrentInfo THEN SetColor(MLightGray) ELSE SetColor(MWhite);
      HideCursor;
      PlotLine(1.5+x1,40,7.5+x1,40);
      SetColor(MWhite);
      PlotLine(1.5+x1,40,2+x1,40.5);
      PlotLine(2+x1,40.5,2+x1,43);
      PlotLine(2+x1,43,3+x1,44);
      PlotLine(3+x1,44,6+x1,44);
      PlotLine(6+x1,44,7+x1,43);
      PlotLine(7+x1,43,7+x1,40.5);
      PlotLine(7+x1,40.5,7.5+x1,40);
      IF i = CurrentInfo THEN SetFillStyle(1,MLightGray)
        ELSE SetFillStyle(1,MDarkGray);
      FloodFill(MapX(x1+3.5),MapY(42.5),MWhite);
      IF i = CurrentInfo THEN SetColor(MWhite) ELSE SetColor(MLightGray);
      OuttextXY(MapX(x1+3.5),MapY(42.5),'#'+Num2Str(i,1));
      {IF ((i = CurrentInfo) AND (i <= NumberDefined)) THEN
                                                    ObjectPtr[i]^.DrawInfo;}
      ShowCursor;
   END;

PROCEDURE TObjectList.DrawCard;

   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      SetColor(MWhite);
      HideCursor;
      PlotLine(12,2,12,40);
      PlotLine(12,40,53,40);
      PlotLine(53,40,53,2);
      PlotLine(53,2,12,2);
      SetFillStyle(1,MLightGray);
      FloodFill(MapX(20),MapY(10),MWhite);
      SetColor(MWhite);
      OutTextxy(MapX(1),MapY(43),'Objects:');
      FOR i := 1 TO NumberDefined DO
        {IF ObjectPtr[i] <> NIL THEN} DrawTab(i);
      ShowCursor;
   END;

PROCEDURE TObjectList.ChangeCurrentInfo(i: Integer);

   VAR last: Integer;
       rect: ARRAY [1..4] OF PointType;
   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      SetColor(MLightGray);
      SetFillStyle(1,MLightGray);
      rect[1].x := MapX(12)+1;
      Rect[1].y := MapY(3)-1;
      rect[2].x := MapX(12)+1;
      rect[2].y := MapY(40)+1;
      rect[3].x := MApX(53)-1;
      rect[3].y := MapY(40)+1;
      rect[4].x := MapX(53)-1;
      rect[4].y := MapY(3)-1;
      HideCursor;
      FillPoly(4,rect);
      last := CurrentInfo;
      CurrentInfo := i;
      IF NumberDefined > 0 THEN DrawTab(last);
      IF NumberDefined > 0 THEN DrawTab(CurrentInfo);
      IF (NumberDefined >= CurrentInfo) THEN ObjectPtr[i]^.DrawInfo;
      IF NumberDefined = 0 THEN
         BEGIN
            SetColor(MBlack);
            OutTextxy(MapX(21),MapY(23),'No Objects Defined');
         END;
      ShowCursor;
   END;

PROCEDURE ReInitExternals(InitData: SaveRecord); FORWARD;

PROCEDURE SticksReinit(InitData: SaveRecord);
   VAR
     GInitRec: InitRec;
     i: Integer;
     OldNumberOfObjects,NumberOfObjects: Byte;
   BEGIN
      ReinitExternals(initData);
      WITH ObjectList DO
         BEGIN
           OldNumberOfObjects := ObjectList.NumberDefined;;
           NumberOFObjects := Trunc(InitData.ObjectData[1]+0.1);
           FOR i := 1 TO OldNumberOfObjects DO DeleteObject(1);
           FOR i := 1 TO NumberOFObjects DO
             BEGIN
               {ObjectType  := ObjectList.ObjectPtr[i]^.ObjectType;}
               GINitRec.pos.x := InitData.ObjectData[10*(i-1)+2];
               GINitRec.pos.y :=  InitData.ObjectData[10*(i-1)+3];
               GINitRec.pos.z := InitData.ObjectData[10*(i-1)+4];
               GINitRec.v.x := InitData.ObjectData[10*(i-1)+5];
               GINitRec.v.y := InitData.ObjectData[10*(i-1)+6];
               GINitRec.v.z := InitData.ObjectData[10*(i-1)+7];
               GINitRec.size.x := InitData.ObjectData[10*(i-1)+8];
               GINitRec.size.y := InitData.ObjectData[10*(i-1)+9];
               GINitRec.pos.t := InitData.ObjectData[10*(i-1)+10];
               ObjectList.AddObject(Trunc(InitData.ObjectData[10*(i-1)+11]+0.1),GInitRec);
            END;
         END;
   END;

PROCEDURE Sticks(InitData:SaveRecord; ReInit: Boolean);
var i : integer;
   BEGIN
      for i := 2 to 3 do hotkeys.active[i] := true;
      hotkeys.Display;
      Mode := 0;
      Message.Done := False;
      Menu.RowChecked(2,1,True);
      FOR i := 6 TO 7 DO
       BEGIN
        Calculator.Keys.key[i].active := False;
        Calculator.Keys.Key[i].erase;
      END;
      FOR i := 1 TO 5 DO
       BEGIN
        Calculator.Keys.key[i].active := True;
        Calculator.Keys.Key[i].Draw;
       END;
      Calculator.Keys.key[40].active := True;
      Calculator.Keys.Key[40].Draw;
      IF Message.ReInit THEN SticksReInit(InitData);
      InitialDraw(96,Graph1);
      InitialDraw(96,Graph2);
      lasti := 96;
      ObjectList.CurrentInfo := 1;
      ObjectList.DrawCard;
      ObjectList.ObjectPtr[1]^.DrawInfo;
      DisplayPrompt(1);
      CurrentHelp := 1;
      Message.ReInit := False;
      WHILE Message.done=False DO TempB := QueryEvent(Message);
      Menu.RowChecked(2,1,False);
   END;

PROCEDURE EditTensor;

   VAR LEave,RegionFound,keyfound: Boolean;
      TheKey: Char;
      eventResult: Boolean;

 PROCEDURE RegionChange(tensnum, regionnum: integer);

    BEGIN
      CurrentTensorPtr^.Field[CurrentRegion DIV 4,
                                 CurrentRegion MOD 4].HighLight(False);
      IF tensnum = 0 THEN
      BEGIN
       IF CurrentRegion < 15 THEN CurrentRegion := CurrentRegion+1
        ELSE
          BEGIN
            CurrentRegion := 0;
            IF CurrentTensNum = 1 THEN
              BEGIN
                CurrentTensorPtr := @Tensor2;
                CurrentTensNum := 2;
              END
            ELSE
              BEGIN
                CurrentTensorPtr := @Tensor1;
                CurrentTensNum := 1;
              END;
          END;
      END
      ELSE
       BEGIN
        IF tensnum = 1 THEN CurrentTensorPtr := @tensor1
        ELSE CurrentTensorPtr := @tensor2;
        CurrentRegion := regionNum;
       END;
     CurrentTensorPtr^.Field[CurrentRegion DIV 4,
                                CurrentRegion MOD 4].HighLight(True);

    END;

   BEGIN
      Leave := False;
      CurrentTensorPtr := @tensor1;
      CurrentTensNum := 1;
      CurrentRegion := 0;
      SelectViewPort(1);
      SelectScale(1);
      tensor1.Field[0,0].highlight(true);
      WHILE Message.Done = FALSE DO
       BEGIN
        CheckForEvents;
        IF Menu.Activated THEN EventResult := ProcessMenu(Message);
          IF Event.KeyPressed THEN
           BEGIN
             TheKey := Event.ReadKey;
               IF (TheKey = CHR(TAB)) THEN RegionChange(0,0);
                IF TheKey IN EditChars THEN
                    BEGIN
                       CurrenttensorPtr^.Field[CurrentRegion DIV 4,
                                      CurrentRegion MOD 4].Edit(TheKey);
                       CASE symmetry OF
                        2: CurrenttensorPtr^.Field[CurrentRegion MOD 4,
                             CurrentRegion DIV 4].ChangeValue(
                             CurrenttensorPtr^.Field[CurrentRegion DIV 4,
                             CurrentRegion MOD 4].Value);
                        3: BEGIN
                           CurrenttensorPtr^.Field[CurrentRegion MOD 4,
                             CurrentRegion DIV 4].ChangeValue(
                             -CurrenttensorPtr^.Field[CurrentRegion DIV 4,
                             CurrentRegion MOD 4].Value);
                            IF CurrentRegion MOD 4 = CurrentRegion DIV 4 THEN
                              CurrenttensorPtr^.Field[CurrentRegion MOD 4,
                              CurrentRegion DIV 4].ChangeValue(0);
                            END;
                       END;
                       IF CurrentTensnum = 1 THEN
                          tensor1.transform(normal,beta,beta,tensor2)
                       ELSE tensor2.transform(inverse,beta,beta,tensor1);
                       DrawTensor(graph1,CurrentGraph1Type);
                       DrawTensor(graph2,CurrentGraph2Type);
                    END;
            END;
         IF HotKeys.Pressed(knum) THEN
          case knum of
           1: ShowHelp(6);
           6: IF menu.Chosen THEN EventResult := ProcessMenu(Message);
          end;
         IF Event.MouseClicked THEN
              BEGIN
                a.x := Event.x;
                a.y := Event.y;
                WITH Calculator DO
                 BEGIN
                 i := 1;
                 keyfound := False;
                 WHILE (keyfound=false) AND (i <= 50) DO
                  BEGIN
                   IF ((Keys.Key[i].Active) AND ( Keys.Key[i].Defined) AND  
                      (Keys.Key[i].KeyRegion.PtInRegion(a))) THEN
                      BEGIN
                        keyfound := True;
                        Keys.Process(i,False);
                      END;
                   i := i + 1;
                  END;
                  END;
                RegionFound := False;
                SearchRegion := 0;
                IF keyfound = False THEN
                WHILE ((RegionFound = False) AND (SearchRegion <= 15)) DO
                   BEGIN
                      IF tensor1.Field[SearchRegion DIV 4,
                                      SearchRegion MOD 4].ScrnReg.PtInRegion(a) THEN
                        BEGIN
                          RegionChange(1,searchRegion);
                          RegionFound := True;
                          CurrenttensNum := 1;
                        END
                      ELSE
                      IF tensor2.Field[SearchRegion DIV 4,
                                      SearchRegion MOD 4].scrnReg.PtInRegion(a) THEN
                        BEGIN
                          RegionChange(2,searchRegion);
                          RegionFound := True;
                          CurrentTensNum := 2;
                        END;
                       SearchRegion := SearchRegion + 1;
                   END;
                SearchRegion := 1;
                IF ((keyfound = False) AND (RegionFound = False)) THEN
                  While ((RegionFound = False) AND (searchRegion <= 3)) DO
                   BEGIN
                      IF checkbox[searchregion].scrnreg.PtInRegion(a) THEN
                        BEGIN
                          IF symmetry <> searchregion THEN
                            BEGIN
                               checkbox[symmetry].uncheck;
                               checkbox[searchregion].check;
                               symmetry := searchregion;
                               Tensor1.symmeterize(symmetry);
                               tensor1.transform(Normal,Beta,Beta,tensor2);
                               InitialDraw(0,Graph1);
                               InitialDraw(0,Graph2);
                             END;
                        END;
                      searchregion := searchregion + 1;
                   END;
            END;
         END;
      END;

PROCEDURE Tensorsinit;

  BEGIN
    tensor1.init(16,27,35,12);
    tensor2.init(16,8,35,12);
    checkbox[1].Init(13,3,15,6);
    checkbox[2].Init(28,3,30,6);
    checkbox[3].Init(39,3,41,6);
    FOR i := 0 TO 3 DO
     FOR j := 0 TO 3 DO
      BEGIN
      tensor1.Field[i,j].Value := 0;
      tensor2.Field[i,j].Value := 0;
      END;
    tensor1.Field[0,1].Value := 1;
    tensor1.Field[0,3].Value := 1;
    tensor1.Field[1,0].Value := -1;
    tensor1.Field[3,0].Value := -1;
   END;

PROCEDURE TensorsReinit(InitData: SaveRecord);
   BEGIN
      ReinitExternals(initData);
      FOR i := 0 TO 3 DO
        FOR j := 0 TO 3 DO
         BEGIN
          tensor1.Field[i,j].Value := InitData.ObjectData[4*i+j+1];
          {tensor1.Field[i,j].Value := InitData.ObjectData[4*i+j];}
         END;
       Symmetry := Trunc(InitData.Objectdata[17]+0.1);
   END;


PROCEDURE Tensors(InitData: SaveRecord; ReInit: Boolean);

   VAR
      rect: ARRAY [1..4] OF Point;
      i: Integer;
   BEGIN
    for i := 2 to 5 do hotkeys.active[i] := false;
    hotkeys.Display;
    Mode := 2;
    DisplayPrompt(6);
    CurrentHelp := 6;
    Message.Done := False;
    Menu.RowChecked(2,3,True);
    WITH ObjectList DO
     FOR i := 1 TO NumberDefined DO EraseTab(i);
    SetColor(MWhite);
      {SetColor(MLightGray);}
      SetFillStyle(1,MLightGray);
      rect[1].x := MapX(12);
      Rect[1].y := MapY(2);
      rect[2].x := MapX(12);
      rect[2].y := MapY(45);
      rect[3].x := MApX(53);
      rect[3].y := MapY(45);
      rect[4].x := MapX(53);
      rect[4].y := MapY(2);
      HideCursor;
      FillPoly(4,rect);
    setColor(MBlack);
    OuttextXY(MapX(13),MapY(44),'Frame 1:');
    OuttextXY(MapX(13),MapY(25),'Frame 2:');
    OuttextXY(MapX(14),MapY(10.5),'z');
    OuttextXY(MapX(14),MapY(13),'y');
    OuttextXY(MapX(14),MapY(15.5),'x');
    OuttextXY(MapX(14),MapY(18),'t');
    OuttextXY(MapX(14),MapY(29.5),'z');
    OuttextXY(MapX(14),MapY(32),'y');
    OuttextXY(MapX(14),MapY(34.5),'x');
    OuttextXY(MapX(14),MapY(37),'t');
    OuttextXY(MapX(20),MapY(41),'t');
    OuttextXY(MapX(29),MapY(41),'x');
    OuttextXY(MapX(38),MapY(41),'y');
    OuttextXY(MapX(47),MapY(41),'z');
    OuttextXY(MapX(20),MapY(22),'t');
    OuttextXY(MapX(29),MapY(22),'x');
    OuttextXY(MapX(38),MapY(22),'y');
    OuttextXY(MapX(47),MapY(22),'z');
    OuttextXY(MapX(16),MapY(5),'no sym.');
    OuttextXY(MapX(31),MapY(5),'sym.');
    OuttextXY(MapX(42),MapY(5),'antisym.');
    IF Message.ReInit THEN TensorsReInit(InitData);
    Message.ReInit := False;
    SetColor(MBlack);
    SetFillStyle(1,MWhite);
    FOR i := 1 TO 3 DO checkbox[i].draw;
    tensor1.display;
    tensor2.display;
    FOR i := 1 TO 5 DO
      BEGIN
        Calculator.Keys.key[i].active := False;
        Calculator.Keys.Key[i].Erase;
      END;
    Calculator.Keys.key[40].active := False;
    Calculator.Keys.Key[40].Erase;
    FOR i := 6 TO 7 DO
      BEGIN
        Calculator.Keys.key[i].activate;
        Calculator.Keys.Key[i].Draw;
      END;
    ShowCursor;
    tensor1.transform(normal,Beta,Beta,tensor2);
    InitialDraw(0,graph1);
    InitialDraw(0,graph2);
    checkbox[symmetry].check;
    EditTensor;
    SetColor(DeskTopColor);
    SetFillStyle(1,DeskTopColor);
    HideCursor;
    FillPoly(4,rect);
    ShowCursor;
    Menu.RowChecked(2,3,False);
   END;

PROCEDURE EditVector;

   VAR LEave,RegionFound,keyfound,EventResult: Boolean;
      TheKey: Char;

 PROCEDURE RegionChange(vectnum, regionnum: integer);

    BEGIN
      CurrentVectorPtr^.Field[CurrentRegion].HighLight(False);
      IF vectnum = 0 THEN
      BEGIN
       IF CurrentRegion < 3 THEN CurrentRegion := CurrentRegion+1
        ELSE
          BEGIN
            CurrentRegion := 0;
            IF CurrentVectNum = 1 THEN
              BEGIN
                CurrentVectorPtr := @Vector2;
                CurrentVectNum := 2;
              END
            ELSE
              BEGIN
                CurrentVectorPtr := @Vector1;
                CurrentVectNum := 1;
              END;
          END;
      END
      ELSE
       BEGIN
        IF vectnum = 1 THEN CurrentVectorPtr := @vector1
        ELSE CurrentVectorPtr := @vector2;
        CurrentRegion := regionNum;
       END;
     CurrentVectorPtr^.Field[CurrentRegion].HighLight(True);

    END;

   BEGIN
      {DisplayPrompt(2);}
      CurrentHelp := 7;
      Message.Done := False;
      CurrentVectorPtr := @vector1;
      CurrentVectNum := 1;
      CurrentRegion := 0;
      SelectViewPort(1);
      SelectScale(1);
      vector1.Field[0].highlight(true);
      WHILE Message.Done = FALSE DO
       BEGIN
        SelectViewPort(1);
        SelectScale(1);
        CheckForEvents;
         IF Menu.Activated THEN EventResult := ProcessMenu(Message);
         IF Event.KeyPressed THEN
           BEGIN
             TheKey := Event.ReadKey;
               IF (TheKey = CHR(TAB)) THEN RegionChange(0,0);
                IF TheKey IN EditChars THEN
                    BEGIN
                       CurrentvectorPtr^.Field[CurrentRegion].Edit(TheKey);
                       IF CurrentVectnum = 1 THEN
                          vector1.transform(normal,beta,beta,vector2)
                       ELSE vector2.transform(inverse,beta,beta,vector1);
                       DrawVector(graph1,CurrentGraph1Type);
                       DrawVector(graph2,CurrentGraph2Type);
                    END;
            END;
         IF HotKeys.Pressed(knum) THEN
          case knum of
           1: ShowHelp(CurrentHelp);
           6: IF menu.Chosen THEN EventResult := ProcessMenu(Message);
          end;
         IF Event.MouseClicked THEN
              BEGIN
                a.x := Event.x;
                a.y := Event.y;
                WITH Calculator DO
                 BEGIN
                 i := 1;
                 keyfound := False;
                 WHILE (keyfound=false) AND (i <= 50) DO
                  BEGIN
                   IF ((Keys.Key[i].Active) AND ( Keys.Key[i].Defined) AND
                      (Keys.Key[i].KeyRegion.PtInRegion(a))) THEN
                      BEGIN
                        keyfound := True;
                        Keys.Process(i,False);
                      END;
                   i := i + 1;
                  END;
                  END;
                RegionFound := False;
                SearchRegion := 0;
                IF keyfound = False THEN
                WHILE ((RegionFound = False) AND (SearchRegion <= 3)) DO
                   BEGIN
                      IF vector1.Field[SearchRegion].ScrnReg.PtInRegion(a) THEN
                        BEGIN
                          RegionChange(1,searchRegion);
                          RegionFound := True;
                          CurrentvectNum := 1;
                        END
                      ELSE
                      IF vector2.Field[SearchRegion].scrnReg.PtInRegion(a) THEN
                        BEGIN
                          RegionChange(2,searchRegion);
                          RegionFound := True;
                          CurrentvectNum := 2;
                        END;
                       SearchRegion := SearchRegion + 1;
                   END;
            END;
         END;
      END;

PROCEDURE ReInitExternals(InitData: SaveRecord);

   BEGIN
      WITH InitData DO
       BEGIN
        IF Graph1Type <> CurrentGraph1Type THEN
         BEGIN
           CASE Graph1Type OF
              1,5: Calculator.Keys.Process(18,True);
              2,6: Calculator.Keys.Process(19,True);
              3,7: Calculator.Keys.Process(20,True);
              4,8: Calculator.Keys.Process(21,True);
           END;
           CurrentGraph1Type := Graph1Type;
         END;
        IF Graph1RefFrame <> Graph1Frame THEN
         BEGIN
           CASE Graph1RefFrame OF
              1: Calculator.Keys.Process(26,True);
              2: Calculator.Keys.Process(27,True);
           END;
           Graph1Frame := Graph1RefFrame;
         END;
        IF Graph2Type <> CurrentGraph2Type THEN
         BEGIN
           CASE Graph2Type OF
              1,5: Calculator.Keys.Process(22,True);
              2,6: Calculator.Keys.Process(23,True);
              3,7: Calculator.Keys.Process(24,True);
              4,8: Calculator.Keys.Process(25,True);
           END;
           CurrentGraph2Type := Graph2Type;
         END;
        IF Graph2RefFrame <> Graph2Frame THEN
         BEGIN
           CASE Graph2RefFrame OF
              1: Calculator.Keys.Process(28,True);
              2: Calculator.Keys.Process(29,True);
           END;
           Graph2Frame := Graph2RefFrame;
         END;
        IF Graph1WorldLines <> WorldTubes[Graph1] THEN
          BEGIN
           Calculator.keys.Process(42,True);
           WorldTubes[Graph1] := Graph1WorldLines;
          END;
        IF Graph2WorldLines <> WorldTubes[Graph2] THEN
          BEGIN
           Calculator.Keys.Process(41,True);
           WorldTubes[Graph2] := Graph2WorldLines;
          END;
        IF Time <> lasti THEN
          BEGIN
          END;
        IF Betax <> Beta.x THEN
          BEGIN
            ChangeBeta(Betax,By.field.value,Bz.field.value,true)
          END;
        IF Betay <> Beta.y THEN
          BEGIN
            ChangeBeta(Bx.field.value,Betay,Bz.field.value,true)
          END;
        IF Betaz <> Beta.z THEN
          BEGIN
            ChangeBeta(Bx.field.value,By.field.value,Betaz,true)
          END;
        END;
     END;

PROCEDURE VectorsReInit(InitData: SaveRecord);
   BEGIN
      ReInitExternals(initdata);
      Vector1.Field[0].Value := InitData.ObjectData[1] ;
      Vector1.Field[1].Value := InitData.ObjectData[2] ;
      Vector1.Field[2].Value := InitData.ObjectData[3] ;
      Vector1.Field[3].Value := InitData.ObjectData[4] ;
   END;

PROCEDURE Vectorsinit;

   BEGIN
    vector1.init(28,27,10,12);
    vector2.init(28,8,10,12);
   END;

PROCEDURE Vectors(InitData: SaveRecord; ReInit: Boolean);

   VAR
      rect: ARRAY [1..4] OF Point;
      i: Integer;
   BEGIN
    for i := 2 to 5 do hotkeys.active[i] := false;
    hotkeys.Display;
    Mode := 1;
    Message.Done := False;
    DisplayPrompt(7);
    CurrentHelp := 7;
    Menu.RowChecked(2,2,True);
    FOR i := 0 TO 24 DO oldgraph[graph1,i] := 0;
    FOR i := 0 TO 24 DO oldgraph[graph2,i] := 0;
    WITH ObjectList DO
     FOR i := 1 TO NumberDefined DO EraseTab(i);
    SetColor(MWhite);
      {SetColor(MLightGray);}
      SetFillStyle(1,MLightGray);
      rect[1].x := MapX(12);
      Rect[1].y := MapY(2);
      rect[2].x := MapX(12);
      rect[2].y := MapY(45);
      rect[3].x := MApX(53);
      rect[3].y := MapY(45);
      rect[4].x := MapX(53);
      rect[4].y := MapY(2);
      HideCursor;
      FillPoly(4,rect);
    setColor(MBlack);
    OuttextXY(MapX(13),MapY(38),'Frame 1:');
    OuttextXY(MapX(13),MapY(19),'Frame 2:');
    OuttextXY(MapX(26),MapY(10.5),'z');
    OuttextXY(MapX(26),MapY(13),'y');
    OuttextXY(MapX(26),MapY(15.5),'x');
    OuttextXY(MapX(26),MapY(18),'t');
    OuttextXY(MapX(26),MapY(29.5),'z');
    OuttextXY(MapX(26),MapY(32),'y');
    OuttextXY(MapX(26),MapY(34.5),'x');
    OuttextXY(MapX(26),MapY(37),'t');
    SetColor(MBlack);
    SetFillStyle(1,MWhite);
    IF Message.ReInit THEN VectorsReInit(InitData);
    Message.ReInit := False;
    vector1.display;
    vector2.display;
    SelectViewPort(1);
    SelectScale(1);
    FOR i := 1 TO 5 DO
      BEGIN
        Calculator.Keys.key[i].active := False;
        Calculator.Keys.Key[i].Erase;
      END;
    Calculator.Keys.key[40].active := False;
    Calculator.Keys.Key[40].Erase;
    FOR i := 6 TO 7 DO
      BEGIN
        Calculator.Keys.key[i].active := True;
        Calculator.Keys.Key[i].Draw;
      END;
    ShowCursor;
    vector1.transform(normal,Beta,Beta,vector2);
    InitialDraw(0,graph1);
    InitialDraw(0,graph2);
    EditVector;
    SetColor(DeskTopColor);
    SetFillStyle(1,DeskTopColor);
    HideCursor;
    FillPoly(4,rect);
    ShowCursor;
    Menu.RowChecked(2,2,False);
   END;


PROCEDURE CreateKeyList;

   BEGIN
      WITH Calculator.Keys DO
         BEGIN
            Init;
            CreateKey(1,1,34,10,'Add',False,False,0);
            CreateKey(2,1,27,10,'Delete',False,False,0);
            CreateKey(3,1,20,10,'Next',False,False,0);
            CreateKey(4,1,13,10,'Edit',False,False,0);
            CreateKey(5,1,6,10,'Clear',False,False,0);
            CreateKey(6,1,27,4,CHR(24),False,True,0);
            key[6].active := False;
            CreateKey(7,6,27,4,CHR(25),False,True,0);
            key[7].active := False;
            CreateKey(18,1,89,10,'x,t',True,False,2);
            CreateKey(19,1,82,10,'x,y',True,False,2);
            CreateKey(20,1,75,10,'x,y,t',True,False,2);
            CreateKey(21,1,68,10,'x,y,z',True,False,2);
            CreateKey(22,89,89,10,'x,t',True,False,3);
            CreateKey(23,89,82,10,'x,y',True,False,3);
            CreateKey(24,89,75,10,'x,y,t',True,False,3);
            CreateKey(25,89,68,10,'x,y,z',True,False,3);
            CreateKey(26,1.5,54,4,'1',True,False,4);
            CreateKey(27,6.5,54,4,'2',True,False,4);
            CreateKey(28,89.5,54,4,'1',True,False,5);
            CreateKey(29,94.5,54,4,'2',True,False,5);
            CreateKey(38,44,60,5, CHR(24),False,True,0);
            CreateKey(39,51,60,5, CHR(25),False,True,0);
            CreateKey(43,61,31,4, CHR(24),False,True,0);
            CreateKey(44,66,31,4, CHR(25),False,True,0);
            CreateKey(45,89,31,4, CHR(24),False,True,0);
            CreateKey(46,94,31,4, CHR(25),False,True,0);
            CreateKey(47,75,31,4, CHR(24),False,True,0);
            CreateKey(48,80,31,4, CHR(25),False,True,0);
            CreateKey(40,43.5,53,13,'Run/Stop',True,False,0);
            CreateKey(42,43.5,84,5,'1',True,False,0);
            CreateKey(41,51.5,84,5,'2',True,False,0);

         END;
   END;
{*************************************************}

{********************}

PROCEDURE LoadData(VAR Message: MessageRec; VAR Data: SaveRecord);

  TYPE
     DescriptionData = RECORD
       ListNumber: Byte;
       Description: DescriptionString;
     END;

  VAR
    IndexFile: File Of DescriptionData;
    DataFile: File Of SaveRecord;
    Description: Array [1..60] OF DescriptionData;
    DescriptionA: DescriptionArray;
    DataNumber: Integer;
    p,N: Integer;
    eofv: Boolean;
    InUseList: ARRAY [1..60] OF Byte;
    TempData: DescriptionData;

PROCEDURE DeleteRecord(DataNumber: Integer);
  BEGIN
    IF DataNumber <= 3 THEN Announce('Cannot delete the first three configurations in list.')
    ELSE
     BEGIN
      Reset(DataFile);
      Seek(DataFile,DataNumber-1);
      FOR k := 1 TO FileSize(DataFile)-DataNumber DO
       BEGIN
        Seek(DataFile,DataNumber-1+k);
        Read(DataFile,Data);
        Seek(DataFile, DataNumber-2+k);
        Write(DataFile,Data);
       END;
      Truncate(DataFile);
      Close(DataFile);
    END;
  END;

  BEGIN
    DataNumber := 0;
    i := 0;
{$I-}
    Assign(DataFile,'Saved.Dat');
    Reset(DataFile);
{$I+}
    IF ((IOResult <> 0) OR EOF(DataFile)) THEN
      BEGIN
        Announce('Data File Missing or Empty.  No Data to Load');
      END
    ELSE
      BEGIN
            i := 1;
            p := FileSize(DataFile);
            WHILE (i <= p) DO
              BEGIN
                Read(DataFile,Data);
                DescriptionA[i] := Data.Description;
                i := i+1;
              END;
            Close(DataFile);
          {END;}
        DataNumber := SelectFromList(i-1,DescriptionA);
        IF (DataNumber > 0) THEN
          BEGIN
            Reset(DataFile);
            Seek(DataFile,DataNumber-1);
            Read(DataFile,Data);
            Message.Next := Data.Mode+1;
            Message.Done := True;
            Message.ReInit := True;
            Close(DataFile);
          END
        ELSE IF (DataNumber < 0) THEN
          BEGIN
             DeleteRecord(-DataNumber);
          END;
      END;
  END;

PROCEDURE SaveData(DataToSave: SaveRecord);
  VAR
    {IndexFile: File Of DescriptionData;}
    DataFile: File Of SaveRecord;
    Data,DataTemp: SaveRecord;
    SaveScreen: TinputScreen;
    SaveHelp: HelpScrType;
    {TempIndexData: DescriptionData;}
  BEGIN
      SaveHelp[01] := '';
      SaveHelp[02] := 'Input a description of the configuration you wish';
      SaveHelp[03] := 'to save.  If you load this configuration from the';
      SaveHelp[04] := 'load menu item, the program will return to the';
      SaveHelp[05] := 'state to which it is currently set.';
      SaveHelp[06] := '';
      SaveHelp[07] := 'The maximum description length is 48 characters.';
      SaveHelp[08] := 'If no characters are entered, the data will not be';
      SaveHelp[09] := 'saved.';
      SaveHelp[10] := '';
      SaveHelp[11] := '';
      SaveHelp[12] := '';
      SaveHelp[13] := '';
      SaveHelp[14] := '';
      SaveHelp[15] := '';
      SaveHelp[16] := '';
      SaveHelp[17] := '';
      SaveHelp[18] := '';
      SaveHelp[19] := '';
      SaveHelp[20] := '';
      SaveHelp[21] := '';
      SaveHelp[22] := '';
      SaveHelp[23] := '';
      SaveHelp[24] := '';
      SaveHelp[25] := '     Press any key or click mouse to continue       ';
      WITH SaveScreen DO
      BEGIN
         init;
         DefineInputPort(0.145,0.855,0.12,0.85);
         Loadline('                                                       ');
         Loadline('                                                       ');
         Loadline('                                                       ');
         Loadline('Enter a description for the current configuration of   ');
         Loadline('the program to be saved.'                               );
         Loadline('                                                       ');
         Loadline(' "                                                  "  ');
         Loadline('                                                       ');
         Loadline('                                                       ');
         Loadline('                                                       ');
         Loadline('                                                       ');
         Loadline('                                                       ');
         Loadline('                                                       ');
         Loadline('                                                       ');
         Loadline('                                                       ');
         Loadline('             [  Ok  ] [Cancel] [ Help ]                ');
{         AboutParser(ParserHelp); }
         SetHelpScreen(SaveHelp);
         AcceptScreen;
         Data.Description := GetString(1);
      END;
    IF (Length(Data.Description) <> 0) THEN
     BEGIN
      Data.Graph1Type := CurrentGraph1Type;
      Data.Graph1RefFrame := Graph1Frame;
      Data.Graph1WorldLines := WorldTubes[Graph1];
      Data.Graph2Type := CurrentGraph2Type;
      Data.Graph2RefFrame := Graph2Frame;
      Data.Graph2WorldLines := WorldTubes[Graph2];
      Data.Time := lasti;
      Data.Betax := Beta.x;
      Data.Betay := Beta.y;
      Data.Betaz := Beta.z;
      Data.Mode := Mode;
      CASE mode OF
        0: BEGIN
             Data.ObjectData[1] := ObjectList.NumberDefined;
             FOR i := 1 TO Trunc(Data.ObjectData[1]+0.1) DO
              BEGIN
               Data.ObjectData[10*(i-1)+11] := ObjectList.ObjectPtr[i]^.ObjectType;
               Data.ObjectData[10*(i-1)+2] := ObjectList.ObjectPtr[i]^.GINitRec.pos.x;
               Data.ObjectData[10*(i-1)+3] := ObjectList.ObjectPtr[i]^.GInitRec.pos.y;
               Data.ObjectData[10*(i-1)+4] := ObjectList.ObjectPtr[i]^.GInitRec.pos.z;
               Data.ObjectData[10*(i-1)+5] := ObjectList.ObjectPtr[i]^.GInitRec.v.x;
               Data.ObjectData[10*(i-1)+6] := ObjectList.ObjectPtr[i]^.GInitRec.v.y;
               Data.ObjectData[10*(i-1)+7] := ObjectList.ObjectPtr[i]^.GInitRec.v.z;
               Data.ObjectData[10*(i-1)+8] := ObjectList.ObjectPtr[i]^.GInitRec.size.x;
               Data.ObjectData[10*(i-1)+9] := ObjectList.ObjectPtr[i]^.GInitRec.size.y;
               Data.ObjectData[10*(i-1)+10] := ObjectList.ObjectPtr[i]^.GInitRec.pos.t;
              END;
           END;
        1: BEGIN
             Data.ObjectData[1] := Vector1.Field[0].Value;
             Data.ObjectData[2] := Vector1.Field[1].Value;
             Data.ObjectData[3] := Vector1.Field[2].Value;
             Data.ObjectData[4] := Vector1.Field[3].Value;
           END;
        2: BEGIN
             FOR i := 0 TO 3 DO
              FOR j := 0 TO 3 DO
               BEGIN
                Data.ObjectData[4*i+j+1] := tensor1.Field[i,j].Value;
               END;
             Data.ObjectData[17] := Symmetry;
           END;
       END;
      {GetDescription(Data);}
{$I-}
      Assign(DataFile,'Saved.Dat');
      Reset(DataFile);
{$I+}
      IF (IOResult <> 0) THEN {Announce('Saved.dat file missing.  Reload from original disks.')}
         ReWrite(DataFile)
        ELSE
        BEGIN
          Seek(DataFile,FileSize(DataFile));
          Write(DataFile,Data);
          Close(DataFile);
        END;
    END;
  END;

FUNCTION ProcessMenu(VAR Message:MessageRec): Boolean;

   BEGIN
     WITH Message DO
     CASE Menu.ColChosen OF
       1: CASE Menu.RowChosen of
           1: AboutCups;
           2: StartUpScreen;
           3: Configuration;
           5: LoadData(Message,InitData);
           6: SaveData(CurrentData);
           8: BEGIN
                Quit := TRUE;
                Done := True;
              END;
          END;
        2: BEGIN
             IF Menu.RowChosen <> Mode+1 THEN
             CASE Menu.RowChosen of
               1: Next := 1;
               2: Next := 2;
               3: Next := 3;
               4: Next := 4;
               5: Next := 5;
               6: Next := 6;
               7: Next := 7;
               8: Next := 8;
             END;
             Done := True;
             Quit := False;
             Paused := False;
             ProcessMenu := True;
           END;
        3: BEGIN
             CASE menu.RowChosen of
               1: Calculator.Keys.Process(18,False);
               2: Calculator.Keys.Process(19,False);
               3: Calculator.Keys.Process(20,False);
               4: Calculator.Keys.Process(21,False);
               6: Calculator.Keys.Process(26,False);
               7: Calculator.Keys.Process(27,False);
               9: Calculator.Keys.Process(42,False);
             END;
             ProcessMenu := True;
        END;
        4: BEGIN
             CASE menu.RowChosen of
               1: Calculator.Keys.Process(22,False);
               2: Calculator.Keys.Process(23,False);
               3: Calculator.Keys.Process(24,False);
               4: Calculator.Keys.Process(25,False);
               6: Calculator.Keys.Process(28,False);
               7: Calculator.Keys.Process(29,False);
               9: Calculator.Keys.Process(41,False);
             END;
             ProcessMenu := True;
        END;
     END;
   ProcessMenu := True;
   END;

FUNCTION ProcessEvent(VAR Message: MessageRec): Boolean;

   VAR
      knum: Byte;
      KeyFound,RegionFound: Boolean;
      i,SearchRegion: Integer;
      xfactor,yfactor: Real;
      y1,y2: Real;

   BEGIN
      WITH Message DO
      BEGIN
         selectViewPort(1);
         SelectScale(1);
         ProcessEvent := False;
         IF WaitBeta THEN
           BEGIN
           IF Event.mouseclicked THEN
            BEGIN
             a.x := Event.x;
             a.y := Event.y;
             i := 43;
             KeyFound := False;
              WITH Calculator DO
                WHILE ((NOT KeyFound) AND (i<= 48)) DO
                  BEGIN
                    IF Keys.Key[i].Defined AND
                         Keys.key[i].Active AND
                          Keys.Key[i].KeyRegion.PtInRegion(a) THEN
                       BEGIN
                        KeyPressed := i;
                        Keys.Process(KeyPressed,False);
                        KeyFound := True;
                       END;
                    i := i + 1;
                  END;

            END;
            GetTime(h2,m2,s2,hun2);
            IF (s2 > s1 + 1) OR ((s1 >= 58) AND (s2 < s1)) THEN
               BEGIN
                  WaitBeta := False;
                  waitm.show;
                  ObjectList.Transform(Beta);
                  Waitm.hide;
                  IF Graph1Frame = 2 THEN InitialDraw(lasti,Graph1);
                  IF Graph2Frame = 2 THEN InitialDraw(lasti,Graph2);
                  ClearMKBuffers;
               END;
            END

         ELSE IF Menu.Activated THEN ProcessEvent := ProcessMenu(Message)
         ELSE
         IF hotKeys.Pressed(kNum) THEN
            BEGIN
            ProcessEvent := True;
            CASE kNum of
               1: BEGIN
                     ShowHelp(CurrentHelp);
                     ProcessEvent := False;
                  END;
               2: Calculator.Keys.Process(40,False);
               3: IF Calculator.Keys.Key[4].Active
                      THEN Calculator.Keys.Process(4,False);
               4: Change := 14;
               5: Change := 15;
               6: IF menu.Chosen THEN ProcessEvent := ProcessMenu(Message);
            END;
            END
         ELSE
            IF Event.MouseClicked THEN
             BEGIN
              a.x := Event.x;
              a.y := Event.y;
              KeyPressed := 0;
              i := 1;
              KeyFound := False;
              WITH Calculator DO
                WHILE ((NOT KeyFound) AND (i <= 50)) DO
                  BEGIN
                    IF Keys.Key[i].Defined AND
                         Keys.key[i].Active AND
                          Keys.Key[i].KeyRegion.PtInRegion(a) THEN
                      BEGIN
                        KeyPressed := i;
                        Keys.Process(KeyPressed,False);
                        KeyFound := True;
                      END;
                    i := i + 1;
                  END;
                RegionFound := False;
                SearchRegion := 1;
                IF ((keyfound = False) AND (ObjectList.NumberDefined > 0))THEN
                WITH ObjectList.ObjectPtr[ObjectList.CurrentInfo]^ DO
                WHILE ((RegionFound = False) AND (SearchRegion <= NumberInfoRegions)) DO
                   BEGIN
                      IF InfoRegions[SearchRegion].ScrnReg.PtInRegion(a) THEN
                        BEGIN
                           CurrentRegion := SearchRegion;
                           RegionFound := True;
                        END
                      ELSE SearchRegion := SearchRegion + 1;
                   END;
                IF RegionFound = True THEN EditObject
                ELSE
                   BEGIN
                      i := 1;
                      IF ((a.y <= MapY(40)+30) AND (a.y >= Mapy(44)+30))
                      THEN
                      WHILE ((RegionFound = False) AND (i <=  ObjectList.NumberDefined)) DO
                          BEGIN
                            IF ((a.x <= Mapx(6*(i-1)+21.5)) AND (a.x >= MapX(6*(i-1)+15.5)))
                            THEN
                             BEGIN
                              ObjectList.ChangeCurrentInfo(i);
                              RegionFound := True;
                             END
                            ELSE i := i + 1;
                          END;
                      END;
                  END;
           END;
    END;

FUNCTION QueryEvent(VAR Message: MessageRec): Boolean;
   BEGIN
      WITH Message DO
      BEGIN
         QueryEvent := False;
         CheckForEvents;
         IF Event.KeyPressed OR Event.MouseClicked OR WaitBeta THEN
             QueryEvent := ProcessEvent(Message);

      END;
   END;

BEGIN
   InitializeFlashes;
   Initialize;
   StartupScreen;
   symmetry := antisymmetric;
   Mode := 0;
   hotkeys.active[4] := false;
   hotkeys.display;
   WITH Calculator DO
      BEGIN
         SetMinkowskiPalette;
         CalculatorViewPort := 1;
         CalculatorScale := 1;
         DefineScale(CalculatorScale,0,100,0,100);
         DefineScale(7,-3,+3,-3,+3);
         DefineViewPort(CalculatorViewPort,0,1,0.06,0.94);
         CalculatorBackColor := DeskTopColor;
         SelectViewPort(1);
         SelectScale(1);
         CreateKeyList;
         Calculator.Draw;

         timefield.init(47,67,9,5);
         timefield.display;
         Bx.init(61,38,9,5);
         Bx.display;
         Bx.Field.ChangeValue(-0.8);
         By.init(75,38,9,5);
         By.display;
         Bz.init(89,38,9,5);
         Bz.display;
         Gamma.init(75,22,9,5);
         Gamma.display;
         Gamma.Field.ChangeValue(0.6);
         DisplayWrite;
         BadVelocity := False;
         SelectScale(5);
         Message.quit := False;
         Tbottom:= MapY(tmin);
         TTop := MapY(Tmax);
         TLeft := MapX(TMin);
         TRight := MapX(TMax);
         tstep := (Tmax-Tmin)/tbottom;
         RunDelay := 0;
         CurrentGraph1Type := XYTGraph;
         CurrentGraph2Type := XYTPrimeGraph;
         Keys.key[20].selected := true;
         Keys.key[24].selected := true;
         Keys.key[20].HighLight;
         Keys.key[24].HighLight;
         Keys.Groups[2].selected := 20;
         Keys.Groups[3].selected := 24;
         Graph1Frame:= 1;
         Graph2Frame:= 2;
         WaitBeta := False;
         Keys.key[26].selected := true;
         Keys.key[29].selected := true;
         Keys.key[26].HighLight;
         Keys.key[29].HighLight;
         Keys.Groups[4].selected := 26;
         Keys.Groups[5].selected := 29;
         Keys.key[41].selected := true;
         Keys.key[41].HighLight;
         Keys.key[42].selected := true;
         Keys.key[42].HighLight;
         FirstTime := True;
         Initial.pos.x := 0;
         Initial.pos.y := 0;
         Initial.pos.z := 0;
         Initial.pos.t := 0;
         Beta.x := -0.8;
         Beta.y := 0;
         Beta.z := 0;
         Beta.Beta2 := Sqr(beta.x) + Sqr(Beta.y) + Sqr(Beta.z);
         Beta.Gamma := 1/sqrt(1-Beta.Beta2);
         WITH Initial DO
            BEGIN
               pos.x := 0;
               Pos.y := 0;
               Pos.z := 0;
               v.x := 0.4;
               v.y := 0;
               v.z := 0;
               size.x := 0.4;
            END;
         ObjectList.Init;
         WITH ObjectList DO
            BEGIN
               Waitm.show;
               Initial.Pos.x := -0.2;
               Initial.Pos.t := -0.0;
               Initial.size.y := 0.1;
               Initial.v.x := -0.4;
               Initial.size.x := 0.5;
               waitm.update;
               Initial.Pos.x := 0.2;
               Initial.Pos.t := 0;
               waitm.update;
               AddObject(OFlash,Initial);
               Initial.Pos.x := -0.2;
               Initial.Pos.t := 0;
               waitm.update;
               AddObject(OFlash,Initial);
               Initial.Pos.x := -0.2;
               Initial.Pos.t := 0.1;
               SelectViewPort(Graph1);
               waitm.hide;
               WorldTubes[Graph1] := On;
               WorldTubes[Graph2] := On;
               selectViewPort(1);
               SelectScale(1);
               tensorsinit;
               vectorsinit;
               InitializePrompt;
               Message.Quit := False;
               Sticks(InitData,True);
               WHILE Message.Quit=False DO
                 BEGIN
                   TempB := QueryEvent(Message);
                   CASE Message.Next OF
                    1: Sticks(InitData,Message.ReInit);
                    2: Vectors(InitData,Message.ReInit);
                    3: Tensors(InitData,Message.ReInit);
                   END;
                 END;
               CupsDone;
            END;
      END;
END.

