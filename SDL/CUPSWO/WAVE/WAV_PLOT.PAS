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
Unit Wav_Plot;

Interface
Uses Wav_Base,Wav_LST,Wav_Gbl,
     Cups,CupsGui,CUPSmupp,CupsGrph,CupsFunc,CupsProc,Crt,Graph;

Type
    PlotPtr=^Plot;

    Plot = Object
          Xo,Yo,Width,Height:           Real;
          SclNum:                       ShortInt;
          Scale:                        ScaleRect;
          AltPlotSize:                  PlotRect;
          ExPanded,isVisible,Freeze:    Boolean;
          PlotAxes:                     Axes;
          AttrBut:                      AttributeButton;
          ExpandBut:                    ExpandButton;
          PlotSclBut:                   SclButton;
          ButtonList:                   List;
          PlotTitle:                    Title;
          AnalysisType:                 PlotType;
          Constructor Initialize( ISclNum: Byte; x, y, w, h: Real);
          Destructor Done;                           Virtual;
          Procedure Display;                         Virtual;
          Procedure Remove;                          Virtual;
          Function MouseInside : BOOLEAN;            Virtual;
          Procedure LockFocus;                       Virtual;
          Procedure SetScale(Sx1, Sy1, Sx2, Sy2: Real );
          Procedure ChangeScale;                     Virtual;
          Procedure Expand;
          Procedure ShowAttrib;                       Virtual;
          Procedure RePlot;   Virtual; {draws inside a plot.  Should be over ridden}
    End;
Procedure RemovePlots;
Procedure DisplayAnalysis;
Procedure RemoveAnalysis;
Procedure UpDate;


VAR
   SourceList:          List;
   SegmentList:         List;
   DetectorList:        List;
   ReflectorList:       List;


Implementation
{****************************************************************************}
{Graphics OutPut Object                                                      }
{****************************************************************************}
Uses Wav_Anly,Wav_Graf;


Procedure RemovePlots;
Begin;
SelectViewport(0);
SetFillStyle(SolidFill,Black);
Bar(0,Round(0.045*GetMaxy),GetMaxX,Round(0.95*GetMaxy));
   {Bar seems faster!
    Anly1.remove;
    Anly2.remove;
    TheWave.remove;}
End;

Procedure RemoveAnalysis;
Begin;
Anly1.Remove;
Anly2.Remove;
End;

Procedure DisplayAnalysis;
Begin;
      Anly1.isVisible:=True;
      Anly1.Display;
      Anly2.isVisible:=True;
      Anly2.Display;
End;

Procedure UpDate;

Begin;

                 If Anly1.isVisible Then
                   Begin
                        Anly1.LockFocus;
                        Anly1.Display;
                   End;
                 If Anly2.isVisible Then
                   Begin
                        Anly2.LockFocus;
                        Anly2.Display;
                   End;
                 If TheWave.isVisible Then
                   Begin
                        TheWave.LockFocus;
                        TheWave.Display;
                        ShowDensity;
                   End;
End;


Constructor Plot.Initialize( ISclNum: Byte; x, y, w, h: Real);

Var
   i:   ShortInt;             

Begin;
      PlotAxes.Initialize( @Self );
      With Scale do
           begin;
                 xMin:=0;
                 yMin:=-1;
                 xMax:=10;
                 yMax:=1;
           end;
      Xo:=x;
      Yo:=y;
      Width:=w;
      Height:=h;
      SclNum:=ISclNum;
      DefineViewPort(1, x, x + w, y, y + h );
      isVisible:=False;
      Expanded:=False;
      With AltPlotSize do
           begin;
                 Left:=0.1;
                 Bottom:=0.1;
                 HSize:=0.8;
                 VSize:=0.8;
           end;
      ButtonList.Init;
      PlotTitle.Initialize(@Self);

End;

Destructor Plot.Done;

Begin;
      Self.Remove;
      Self.ButtonList.Done(False);     {This May Be A Bug - Buttons be True}
      PlotTitle.Done;
      PlotAxes.Done;
End;

Procedure Plot.LockFocus;

Begin;
      DefineViewPort( 1, Xo, Xo + Width, Yo, Yo + Height );
      SelectViewPort( 1 ); 
      With Scale do DefineScale( SclNum,  xMin, xMax,yMin, yMax  );
      SelectScale( SclNum );
End;

Procedure Plot.ShowAttrib;
Begin
  Announce('Error in Plot.ShowAttrib. This should have been overridden.');
End;
Procedure Plot.Display;
Begin;
  if not(isVisible) then exit;
      LockFocus;
      HideCursor;
      SelectViewport( 1 );
      OpenViewPort( 1 );
      if AnalysisType <> YvsT3D then PlotAxes.Display;
      ButtonList.Display;
      PlotTitle.Display;
     {isVisible:=True;  removed by wc}
      ShowCursor;
End;

Procedure Plot.Remove;

Begin;
      LockFocus;
      HideCursor;
      PlotAxes.Remove;
      ButtonList.Remove;
      PlotTitle.Remove;
      CloseViewport( 1 );
      isVisible:=False;
      ShowCursor;
End;

Procedure Plot.SetScale(Sx1, Sy1, Sx2, Sy2: Real );

Begin; 
       With Scale do
            Begin;
                  xMin:=Sx1;
                  yMin:=Sy1;
                  xMax:=Sx2;
                  yMax:=Sy2;
                  DefineScale( SclNum ,  xMin,xMax, yMin, yMax );
            End;
       SelectScale( SclNum );
       PlotAxes.SetUp;
End;
{****************************************************************************}
Function Plot.MouseInside : BOOLEAN;

Var
   xpos, ypos:  REAL;
   inside:      BOOLEAN;

Begin;
      LockFocus;
      MouseInside:=False;
      If Event.MouseClicked then
         begin;
               If not(isvisible) then exit;
               MousePosn(xpos,ypos, 1, SclNum, Inside);
               If Inside then MouseInside:=True;
         end;
End;
{****************************************************************************}
Procedure Plot.ChangeScale;
Begin;
     Announce('Programming BUG.  ChangeScale should to be cast to either Analysis for Graph Plot types.');
End;
Procedure Plot.Replot;
Begin;
     Announce('Programming BUG.  ReplotAnalysis should to be cast to either Analysis for Graph Plot types.');
End;

Procedure Plot.Expand;

Var
   TempSize:        PlotRect;

Begin;
      RemovePlots;
      LockFocus;
      Expanded:=not(Expanded);
      TempSize:=AltPlotSize;
      {PlotAxes.Remove;}
      Remove;
      Self.isVisible:=True;
      HideCursor;
      With TempSize do DefineViewPort( 1, Left, Left+hSize, Bottom, Bottom+vSize );
      With AltPlotSize do
           begin;
                 Left:=Xo;
                 Bottom:=Yo;
                 hSize:=width;
                 vSize:=height;
           end;
     With TempSize do
          begin;
                Xo:=left;
                Yo:=bottom;
                width:=hSize;
                height:=vSize;
          end;
     If Not(Expanded) then
          Begin {All plots will show since we have just contracted}
          TheWave.isVisible:=TRUE;
          Anly1.isVisible:=TRUE;
          Anly2.isVisible:=TRUE;
          Update;
          End
        else {Set All plots to not visible except self.  Then replot.}
        begin;
             If @self<>@Anly1 Then Anly1.isVisible:=FALSE;
             If @self<>@Anly2 Then Anly2.isVisible:=FALSE;
             If @self<>@TheWave  Then TheWave.isVisible:=FALSE;
             LockFocus;
             Display;
             If AnalysisType<>YvsT3D then PlotAxes.Display;
        end;
     ShowCursor;
End;



end.