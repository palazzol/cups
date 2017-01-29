            (*************************************************)
            (*************************************************)
            (**                                             **)
            (** Consortium of Upper-level Physics Software  **)
            (**             ( CUPS Project )                **)
            (**     (c) 1994 by John Wiley & Sons           **)
            (** Compiled with Utilities Ver. 1.8 (95/08/08) **)
            (**                                             **)
            (*************************************************)
            (*************************************************)

{$R-}
UNIT Wav_Base;

INTERFACE

USES
   Wav_Gbl,Cups,CupsGui,CUPSmupp,CupsGrph,CupsFunc,CupsProc,Crt,Graph;



TYPE

    BasePtr=^Basis;

    AnalysisArray = Array[1..5] of BasePtr;

    AxesPtr=^Axes;

    BasisType = ( Seg, Det, Sou, Ref, Other );

   Basis = Object
          x0,y0:                           Real;
          GridIndex:                       Integer;
          EvenGrid:                        Boolean;
          ParSource:                       SourceInfo;
          ParSegment:                      SegmentInfo;
          ParDetector:                     DetectorInfo;
          ParReflector:                    ReflectorInfo;
          ParentPointer:                   Pointer;
          isVisible,Outlined:              Boolean;
          DragIt,
          Disabled:                        Boolean;
          TheBasisType:                    BasisType;
          ObjColor:                        Word;
          Constructor Initialize;
          Destructor Done;                                           Virtual;
          Procedure Display;                                         Virtual;
          Procedure Remove;                                          Virtual;
          Function  MouseInside: Boolean;                            Virtual;
          Function Inspector:Boolean;                                Virtual;
          Procedure ChangeData( nm, nv, nk1, nk2, nk3,nd,ns: Real ); Virtual;
          Function  GetSourceValue: Real;                            Virtual;
          Procedure GetDetectorValues(Var Right, Left,Magnitude:Real);Virtual;
          Procedure PutPos( x: Real );                               Virtual;
          Procedure AddWaveToDVec;                                   Virtual;
          Procedure Clear;                                           Virtual;
          Procedure Move( x: Real );
          Procedure Drag;
          Procedure CalcGridIndex;
          Procedure Archive(Var filehandle: text);                   Virtual;
          Procedure SetObjColor(theColor:Word);
    End;

    Title = Object(Basis)
      titleStr:   String;
      Constructor  Initialize( Plot : pointer);
      Destructor   Done;     Virtual;
      Procedure    Display;               Virtual;
      Procedure    Remove;                Virtual;
      Procedure    SetStr(aString:String);
    End;

    Button = Object( basis )
          Ix,Iy,w,h:        Real;
          ButtonNum:        ShortInt;
          box:              Array[1..4] of PointType;
          Constructor Initialize( Plot : pointer; theButton: ShortInt );
          Destructor Done;                  Virtual;
          Procedure Display;                Virtual;
          Procedure Hide;                   Virtual;
          Procedure DrawIcon;               Virtual;
          Function MouseInside: BOOLEAN;    Virtual;
          Procedure Action;                 Virtual;
          Procedure CalculateBox;           Virtual;
   End;

   SourceButton = Object( Wav_Base.Button )
              Constructor Initialize( Plot: pointer );
              Destructor Done;               Virtual;
              Procedure Action;              Virtual;
              Procedure DrawIcon;            Virtual;

   End;

   SegmentButton = Object( Wav_Base.Button )
              Constructor Initialize( Plot: pointer );
              Destructor Done;               Virtual;
              Procedure Action;              Virtual;
              Procedure DrawIcon;            Virtual;

   End;

   DetectorButton = Object( Wav_Base.Button )
              Constructor Initialize( Plot: pointer );
              Destructor Done;               Virtual;
              Procedure Action;              Virtual;
              Procedure DrawIcon;            Virtual;

   End;

   ReflectorButton = Object( Wav_Base.Button )
              Constructor Initialize( Plot: pointer );
              Destructor Done;               Virtual;
              Procedure Action;              Virtual;
              Procedure DrawIcon;            Virtual;

   End;

  CornerButton = Object( Wav_Base.Button )
                CornerNum:   ShortInt;
                Constructor Initialize( Plot: pointer; theCorner: Integer );
                Destructor Done;            Virtual;
                Procedure CalculateBox;     Virtual;
   End;

   SclButton = Object( Wav_Base.CornerButton )
              Constructor Initialize( Plot: pointer );
              Destructor Done;               Virtual;
              Procedure Action;              Virtual;
              Procedure DrawIcon;            Virtual;
   End;

   ExpandButton = Object( Wav_Base.CornerButton )
              Constructor Initialize( Plot: pointer);
              Destructor Done;               Virtual;
              Procedure Action;              Virtual;
              Procedure DrawIcon;            Virtual;
   End;

   AttributeButton = Object( Wav_Base.CornerButton )
              Constructor Initialize( Plot: pointer);
              Destructor Done;               Virtual;
              Procedure Action;              Virtual;
              Procedure DrawIcon;            Virtual;
   End;

   Axes = Object( Wav_Base.basis )
         AxesTic:         AxesRect;
         Constructor Initialize( Plot: pointer );
         Procedure SetUp;
         Destructor Done;                            Virtual;
         Procedure Display;                          Virtual;
         Procedure Remove;                           Virtual;
    End;

Var
   MaxData:     Word;

IMPLEMENTATION

USES Wav_plot,Wav_LST,Wav_Anly,Wav_Graf;

{****************************************************************************}
{Various Porcedures and Functions                                            }
{****************************************************************************}


{****************************************************************************}
{Base Object for All Other Objects                                           }
{****************************************************************************}
Constructor Basis.Initialize;

Begin;
      With ParSource do
           begin;
                 Kind:=Gaussian;
                 Direction:=toLeft;
                 Freq:=0;
                 Amp:=0;
                 A:=0;
                 X0:=0;
                 Radius:=10;
                 SourcePeriod:=1;
                 Noiseon:=False;
                 AmpNoise:=0.00;
                 CoherenceTime:=1;
                 PhaseShift:=0;
                 Power:=0;
                 Num:=1;
                 FuncStr:='sin(10*t)*sin(100*t)';
                 Periodic:=False;
           end;
      With ParSegment Do
         Begin
               Index:=2;
               X0:=0;
               Y0:=0;
               Width:=0;
               Gain:=0;
          End;
      With ParDetector do
           begin;
                 x0:=0;
                 y0:=0.5;
                 num:=1;
                 {IntensityOn:=False; }
           end;
      With ParReflector do
           begin
                x0:=0;
                y0:=round(2/7);
                ReflectKind:=theSum;
           end;
      ParentPointer:=Nil;
      Outlined:=False;
      Disabled:=False;
      DragIt:=True;
      TheBasisType:=Other;
      isVisible:=True;
End;

Procedure Basis.Archive(Var filehandle: text);

Var
   Store1,Store2:       Integer;

Begin;
      Case TheBasisType of
           Sou:        Store1:=1;
           Seg:        Store1:=2;
           Det:        Store1:=3;
           Ref:        Store1:=4;
           Other:      Store1:=5;
      End; {of Case}
      WriteLn(filehandle, x0,' ',y0,' ',ToNum(IsVisible),' ',ToNum(OutLined),' ',ToNum(DragIt),' ',ToNum(Disabled),' ',Store1);
      Case TheBasisType of
           Sou: With ParSource do
                     begin
                          Case Kind of
                               Sine:        Store1:=1;
                               Gaussian:    Store1:=2;
                               Pulse:       Store1:=3;
                               Step:        Store1:=4;
                               ModGaussian: Store1:=5;
                               UserDefined: Store1:=6;
                          End;
                          Case Direction of
                               toLeft:        Store2:=1;
                               toRight:       Store2:=2;
                               bidirectional: Store2:=3;
                          End;
                          WriteLn(filehandle,Num,' ',store1,' ',store2,' ',Freq,' ',Delay,' ',Amp,' ',A,' ',Radius);
                          WriteLn(filehandle,FuncStr);
                          WriteLn(filehandle,ToNum(Periodic),' ',SourcePeriod);
                          WriteLn(filehandle,ToNum(Noiseon),' ',AmpNoise,' ',CoherenceTime);
                          WriteLn(filehandle,ToNum(ForceAmplitude));
                     end;
           Seg:      With Parsegment do writeLn(filehandle,Index,' ',Width,' ',Gain,' ',objColor);
           Det:      With ParDetector do
                          begin
                               WriteLn(filehandle, Num,' '{,AvgPeriod,' ',ToNum(IntensityOn)});
                          end;
           Ref:     With ParReflector do
                         begin
                               Case Reflectkind of
                                    theRight:  Store1:=1;
                                    theLeft:   Store1:=2;
                                    theSum:    Store1:=3;
                               End;
                               writeln(filehandle,Store1);
                         end;
           Other: ;
      End; {of Case}
End;





Destructor Basis.Done;
Begin;
      {Self.Remove;}
      ParentPointer:=Nil;
End;
Procedure Basis.CalcGridIndex;
Begin
     GridIndex:=0;
     Repeat
           Inc(GridIndex);
     Until (xvec.Value(GridIndex)>x0) OR (GridIndex=NumPts);
     If GridIndex<2 then GridIndex:=2;
     If GridIndex>=Numpts then GridIndex:=Numpts-1;
     If (GridIndex Mod 2)=0 Then evenGrid:=True else evenGrid:=False;
End;{of CalcGridIndex}

Procedure Basis.Display;
Begin;
End;

Procedure Basis.Remove;
Begin;
End;

Function Basis.MouseInside: Boolean;
Begin;
End;

Procedure Basis.Clear;
Begin
Announce('Error: Clear should be overridden by subclass.');
End;

Procedure Basis.Drag;

VAR
   xtemp,
   ySet,xSet,
   xRub,
   xpos,ypos         : REAL;
   inside            : BOOLEAN;
   x,y,button,num    : INTEGER;
   oldStatus         : BOOLEAN;
   OldColor          : WORD;
Procedure InitDragCoor;

Begin;
      If PlotPtr(parentPointer)^.expanded then num:=19 else num:=9;
      xPos:=x0;
      ySet:=0;
      If ((xPos<0.01) and (xPos>-0.01)) then xSet:=0 else xSet:=xPos;
      Print(1,num,'Position = ('+Num2Str(xSet,5)+' '+Num2Str(ySet,5)+')');
      xRub:=xPos;
End;

Procedure MainDragCoor;

Begin;
      If not(xRub=xPos) then
         begin;
               xRub:=xPos;
               If ((xPos<0.01) and (xPos>-0.01)) then xSet:=0 else xSet:=xPos;
               RubOut(12,num,15,Graphbackcolor);
               Print(12,num,'('+Num2Str(xSet,5)+' '+Num2Str(ySet,5)+')');
               xRub:=xPos;
         end;
End;

Procedure DoneDragCoor;

Begin;
      RubOut(1,num,26,GraphBackcolor);
End;

BEGIN
   If not(DragIt) then exit;
   Plot(parentPointer^).lockFocus;
   oldColor:=GraphBackColor;
   GraphBackColor:=Black;
   Remove;
   oldStatus := outlined;
   outlined := TRUE;
   Display;
   InitDragCoor;
   REPEAT
      MouseGlobalPosn(x,y, button);
      MousePosn(xpos,ypos,1,1,inside);
      Move(xpos);
      MainDragCoor;
   UNTIL (button = 0);
   DoneDragCoor;
   GraphBackColor:=OldColor;
   Case TheBasisType of
        Seg: With ParSegment Do
                  if xPos>(StopEdge-width) then xpos:=(StopEdge-width);
        Sou,
        Det,
        Ref: If xPos>Stopedge then xpos:=StopEdge;
   End; {of Case}

   If xPos<StartEdge then xpos:=StartEdge;

   PutPos(xPos);
   display;
   outlined := oldStatus;
   remove;
   PutPos(xpos);

   If (TheBasisType = Seg) Then
     Begin
     TheWave.UpdateVecs;
     TheWave.AutoSetTimeInc;
     End
     Else CalcGridIndex;
   If (TheBasisType = Ref) or (TheBasisType = Sou) then Clear;  {Detectors?}
   TheWave.Display;
END;

Procedure Basis.SetObjColor(theColor:Word);
Begin
  objColor:=theColor;
End;

Function Basis.Inspector:Boolean;
Begin;
End;

Procedure Basis.ChangeData( nm, nv, nk1, nk2, nk3, nd, ns: Real );
Begin;
End;

Function  Basis.GetSourceValue: Real;
Begin;
End;

Procedure Basis.GetDetectorValues(Var Right, Left,Magnitude: Real);
Begin;
End;

PROCEDURE Basis.PutPos(x : REAL);

BEGIN
   x0 := x;
END;

PROCEDURE Basis.Move(x : REAL);

BEGIN
   Remove;
   PutPos(x);
   Display;
END;

Procedure Basis.AddWaveToDVec;

BEGIN
END;
{****************************************************************************}
{Title Object                                                                 }
{****************************************************************************}
Constructor Title.Initialize( Plot: pointer);

Begin;
      Basis.Initialize;
      ParentPointer:=PlotPtr(Plot);
      objColor:=Magenta;
      TitleStr:='Title';
      IsVisible:=True;
End;

Procedure Title.Remove;
Var
   OldColour,i:   Word;
   x,y:Real;
Begin;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      x:=PlotPtr(parentPointer)^.Xo+PlotPtr(parentPointer)^.width/2-Length(titleStr)*colWidth/GetMaxX/2;
      y:=PlotPtr(parentPointer)^.Yo+PlotPtr(parentPointer)^.height+1.5/80;
      SelectViewPort(0);
      RuboutXY(x,y,Length(TitleStr),Black);
      SetColor( OldColour );
      PlotPtr(parentPointer)^.LockFocus;
End;
Procedure Title.SetStr(aString:String);
Begin
  TitleStr:=aString;
End;
Procedure Title.Display;
Var
   OldColor,i:   Word;
   x,y:Real;
Begin
  PlotPtr(parentPointer)^.LockFocus;
  OldColor:=GetColor;
  x:=PlotPtr(parentPointer)^.Xo+PlotPtr(parentPointer)^.width/2 -Length(titleStr)*ColWidth/GetMaxX/2;
  y:=PlotPtr(parentPointer)^.Yo+PlotPtr(parentPointer)^.height+1.5/80;
  SelectViewport(0);
  RuboutXY(x,y,Length(TitleStr),objColor);
  SetColor(White);
  PrintXY(x,y,TitleStr);
  SetColor(OldColor);
  PlotPtr(parentPointer)^.LockFocus;
End;
Destructor Title.Done;
Begin;
      Self.Remove;
      Basis.Done;
End;
{****************************************************************************}
{Axes Object                                                                 }
{****************************************************************************}
Constructor Axes.Initialize( Plot: pointer);

Begin;
      Basis.Initialize;
      ParentPointer:=PlotPtr(Plot);
      {IsVisible:=False; changed by wc }
      IsVisible:=True;
End;
Procedure Axes.Setup;

Begin;
      {IsVisible:=True;  removed by wc}
      PlotPtr(parentPointer)^.LockFocus;
      With PlotPtr(parentPointer)^.Scale do
           begin;
                 If (xMin<0.0001) and (xMin>-0.0001) then xMin:=0;
                 If (yMin<0.0001) and (yMin>-0.0001) then yMin:=0;
                 AxesTic.xOrg:=xMin;
                 If (yMax>0) then AxesTic.yOrg:=yMin else AxesTic.yOrg:=yMax;
                 AxesTic.xTic:=2*TickSpace(xMax-xMin);
                 AxesTic.yTic:=2*TickSpace(yMax-yMin);
                 If (AnlyPlotPtr(parentpointer)^.GeneralType=FastFT) then
                    begin                   {Special for FFT}
                    AxesTic.yOrg:=0;
                    AxesTic.xTic:=Round((xMax-xMin)/4 + 0.51);
                    end;
           end; {of With}
End;

Destructor Axes.Done;

Begin;
      Self.Remove;
      Basis.Done;
End;

Procedure Axes.Display;

Var
   i,OldColour:   Word;

Begin;
      If not(IsVisible) then exit;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      With AxesTic do
           begin;
                 Axis(  xOrg, yOrg, xTic, yTic  );

           end;
      SetColor( OldColour );
End;

Procedure Axes.Remove;

Var
   OldColour,i:   Word;

Begin;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      With AxesTic do
           begin;
                 DrawAxis( xOrg, yOrg, xTic, yTic, Black );
           end;
      SetColor( OldColour );
End;

{****************************************************************************}
{Button Objects                                                              }
{****************************************************************************}
Constructor Button.Initialize( Plot : pointer; theButton: ShortInt );

Begin;
      Basis.Initialize;
      ParentPointer:=PlotPtr(Plot);
      PlotPtr(parentPointer)^.LockFocus;
      ButtonNum:=thebutton;
      PlotPtr(parentPointer)^.ButtonList.addItem(@self);
End;

Destructor Button.done;

Begin;
      Self.hide;
      Basis.Done;
End;

Procedure Button.Display;

Var
   oldColour:            Word;
   oldFillStyle:        FillSettingsType;

Begin;
      if Not(isVisible) or Not(PlotPtr(parentPointer)^.isVisible) Then Exit;

      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      GetFillSettings( oldFillStyle );
      Self.CalculateBox;
      Graph.Rectangle( box[1].x, box[1].y, box[3].x, box[3].y );
      Self.DrawIcon;
      SetFillStyle( oldFillStyle.pattern, oldFillStyle.color );
      SetColor( oldColour );
      ShowCursor;
End;

Procedure Button.Hide;

VAR
   oldColour:     Word;
   oldFillStyle: FillSettingsType;

Begin;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      CalculateBox;
      OldColour:=GetColor;
      GetFillSettings( oldFillstyle );
      SetColor( graphBackColor );
      SetFillStyle( SOLIDFILL, graphBackColor );
      FillPoly( 4, box );
      SetColor( oldColour );
      SetFillStyle( oldFillStyle.pattern, oldFillStyle.color );
      ShowCursor;
END;

Function Button.MouseInside : BOOLEAN;

Var
   xpos,ypos:   Real;
   Xin,Yin,
   inside:      Boolean;

Begin;
      if Not(isVisible) then begin mouseInside:=False; exit end;
      PlotPtr(parentPointer)^.LockFocus;
      Self.CalculateBox;
      MouseInside:=False;
      If not(PlotPtr(parentPointer)^.isvisible) then exit;
      If Event.MouseClicked then
         begin;
               MousePosn(xpos,ypos, 1, PlotPtr(parentPointer)^.SclNum, Inside);
               If (xpos > Ix) and (xpos < (Ix + w)) then Xin:=True else Xin:=False;
               If ((Iy + h)>Iy) then
                  If (ypos > Iy) and (ypos < (Iy + h)) then Yin:=True else Yin:=False
               else
                  If (ypos < Iy) and (ypos > (Iy + h)) then Yin:=True else Yin:=False;
               If Xin and Yin then
                      begin
                            MouseInside:=True;
                            Beep;
                      end;
         end;
End;

Procedure Button.DrawIcon;

Begin;
End;

Procedure Button.Action;

Begin;
Announce('In Button.Action.  Should be over ridden.');
End;                                        

PROCEDURE Button.CalculateBox;

Begin;
      with PlotPtr(parentPointer)^.Scale do
           begin;
                 Ix:=xMin;
                 Iy:=yMin;
                 w:=(xMax-xMin)/20;
                 h:=(yMax-yMin)/10;
           end;
      PlotPtr(parentPointer)^.lockFocus;
      Iy:=Iy + (ButtonNum-1)*h;

      Box[1].x:=mapx(Ix) + 1;
      Box[1].y:=mapy(Iy) - 1;

      Box[2].x:=mapx(Ix + w) + 1;
      Box[2].y:=box[1].y;

      Box[3].x:=box[2].x;
      Box[3].y:=mapy(Iy + h) + 1;

      Box[4].x:=box[1].x;
      Box[4].y:=box[3].y
End;

{****************************************************************************}
Constructor CornerButton.Initialize( Plot : pointer; theCorner : INTEGER );

Begin;
      Button.Initialize( PlotPtr(Plot), theCorner );
      CornerNum:=theCorner;
End;

Destructor CornerButton.Done;

Begin;
      Button.Done;
End;

Procedure CornerButton.CalculateBox;
                                              
Begin;
      Case CornerNum of
           1:                  
              begin;
                    with PlotPtr(parentPointer)^.Scale do
                         begin;
                               Ix:=xMin;
                               Iy:=yMax;
                               w:=(xMax-xMin)/20;
                               h:=(yMax-yMin)/10;
                         end;
                    Iy:=Iy - h;
              end;
           2:
             begin;
                    with PlotPtr(parentPointer)^.Scale do
                         begin;
                               Ix:=xMax;
                               Iy:=yMax;
                               w:=(xMax-xMin)/20;
                               h:=(yMax-yMin)/10;
                         end;
                   Iy:=Iy - h;
                   Ix:=Ix - w;
             end;
           3:
             begin;
                    with PlotPtr(parentPointer)^.Scale do
                         begin;
                               Ix:=xMax;
                               Iy:=yMin;
                               w:=(xMax-xMin)/20;
                               h:=(yMax-yMin)/10;
                         end;
                   Iy:=Iy;
                   Ix:=Ix - w;
             end;
      End;
      PlotPtr(parentPointer)^.lockFocus;

      Box[1].x := mapx(Ix);
      Box[1].y := mapy(Iy);

      Box[2].x := mapx(Ix + w) - 1;
      Box[2].y := box[1].y;

      Box[3].x := box[2].x;
      Box[3].y := mapy(Iy + h) + 1;

      Box[4].x := box[1].x;
      Box[4].y := box[3].y;
End;                                                                              
{****************************************************************************}

Constructor SclButton.Initialize( Plot : Pointer);

Begin;
      CornerButton.Initialize( PlotPtr(Plot), 1 );
End;

Destructor SclButton.Done;

Begin;
      CornerButton.Done;
End;

Procedure SclButton.DrawIcon;

Var
   oldColour:    Word;
   oldFillStyle: FillSettingsType;

Begin;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      GetFillSettings( oldFillStyle );
      SetFillStyle(XHatchFILL, Red);
      FillPoly(4, box);
      SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
      ShowCursor;
End;


Procedure SclButton.Action;
Begin;
PlotPtr(ParentPointer)^.ChangeScale;
End;

{****************************************************************************}
Constructor ExpandButton.Initialize( Plot : pointer);

Begin;
      CornerButton.Initialize( PlotPtr(Plot), 2 );
End;

Destructor ExpandButton.Done;

Begin;
      CornerButton.Done;
End;

Procedure ExpandButton.DrawIcon;

Var
   oldColour:    Word;
   oldFillStyle: FillSettingsType;

Begin;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      GetFillSettings( oldFillStyle );
      SetFillStyle(xHatchFILL, Magenta);
      FillPoly(4, box);
      SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
      ShowCursor;
End;


Procedure ExpandButton.Action;

Begin;
      PlotPtr(parentPointer)^.Expand;
      IF PlotPtr(parentPointer)^.isVisible
        AND PlotPtr(parentPointer)^.Expanded
        AND NOT (PlotPtr(parentPointer)^.Freeze)
          THEN ShowDensity;
End;
{****************************************************************************}
Constructor AttributeButton.Initialize( Plot : pointer);

Begin;
      CornerButton.Initialize( PlotPtr(Plot), 3 );
End;

Destructor AttributeButton.Done;

Begin;
      CornerButton.Done;
End;

Procedure AttributeButton.DrawIcon;

Var
   oldColour:    Word;
   oldFillStyle: FillSettingsType;

Begin;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      GetFillSettings( oldFillStyle );
      SetFillStyle(xHatchFILL, Blue);
      FillPoly(4, box);
      SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
      ShowCursor;
End;


Procedure AttributeButton.Action;

Begin;
PlotPtr(ParentPointer)^.ShowAttrib;
End;
{****************************************************************************}
Constructor  SourceButton.Initialize( Plot : pointer);

Begin;
      Button.Initialize( PlotPtr(Plot), 3 );
End;

Destructor SourceButton.Done;

Begin;
      Button.Done;
End;

Procedure SourceButton.DrawIcon;

Var
   oldColour:    Word;
   oldFillStyle: FillSettingsType;

Begin;
      CalculateBox;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      GetFillSettings( oldFillStyle );
      SetFillStyle(SOLIDFILL, red);
      FillPoly(4, box);
      PlotSymbol(Ix+w/2,Iy+h/2,'s');
      SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
      ShowCursor;
End;


Procedure SourceButton.Action;

Begin;
      WavePtr(ParentPointer)^.AddSource;
End;


{****************************************************************************}
Constructor  SegmentButton.Initialize( Plot : pointer);

Begin;
      Button.Initialize( PlotPtr(Plot), 2 );
End;

Destructor SegmentButton.Done;

Begin;
      Button.Done;
End;

Procedure SegmentButton.DrawIcon;

Var
   oldColour:    Word;
   oldFillStyle: FillSettingsType;

Begin;
      CalculateBox;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      GetFillSettings( oldFillStyle );
      SetFillStyle(SOLIDFILL, Green);
      FillPoly(4, box);
      PlotSymbol(Ix+w/2,Iy+h/2,'n');
      SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
      ShowCursor;
End;


Procedure SegmentButton.Action;

Begin;
      WavePtr(ParentPointer)^.AddSegment;
End;
{****************************************************************************}
Constructor  DetectorButton.Initialize( Plot : pointer);

Begin;
      Button.Initialize( PlotPtr(Plot), 1 );
End;

Destructor DetectorButton.Done;

Begin;
      Button.Done;
End;

Procedure DetectorButton.DrawIcon;

Var
   oldColour:    Word;
   oldFillStyle: FillSettingsType;

Begin;
      CalculateBox;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      GetFillSettings( oldFillStyle );
      SetFillStyle(SOLIDFILL, Blue);
      FillPoly(4, box);
      PlotSymbol(Ix+w/2,Iy+h/2,'d');
      SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
      ShowCursor;
End;


Procedure DetectorButton.Action;

Begin;
      WavePtr(ParentPointer)^.AddDetector;
End;
{****************************************************************************}
Constructor  ReflectorButton.Initialize( Plot : pointer);

Begin;
      Button.Initialize( PlotPtr(Plot), 4 );
End;

Destructor ReflectorButton.Done;

Begin;
      Button.Done;
End;

Procedure ReflectorButton.DrawIcon;

Var
   oldColour:    Word;
   oldFillStyle: FillSettingsType;

Begin;
      CalculateBox;
      HideCursor;
      PlotPtr(parentPointer)^.LockFocus;
      OldColour:=GetColor;
      GetFillSettings( oldFillStyle );
      SetFillStyle(SOLIDFILL, ReflectorColour);
      FillPoly(4, box);
      PlotSymbol(Ix+w/2,Iy+h/2,'r');
      SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
      ShowCursor;
End;


Procedure ReflectorButton.Action;

Begin;
      WavePtr(ParentPointer)^.AddReflector;
End;
End.