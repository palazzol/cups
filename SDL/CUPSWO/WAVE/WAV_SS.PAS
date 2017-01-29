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
UNIT Wav_SS;
INTERFACE
USES
   Wav_base,Wav_Gbl, Wav_plot,Wav_Graf,
   Cups, CupsGui, CupsMupp, CupsGrph, CupsFunc, CupsProc, CRT, GRAPH, CupsPars;
Type
   SegmentPtr = ^Segment;
   Segment = OBJECT(Basis)
      xBegin, xEnd : INTEGER;
      theRect : array [1..4] of PointType;
      CONSTRUCTOR Init(x : real; thePlot : PlotPtr);
      DESTRUCTOR  Done;                         Virtual;
      PROCEDURE   Display;                      VIRTUAL;
      PROCEDURE   Remove;                       VIRTUAL;
      PROCEDURE   PutPos(x : REAL);             VIRTUAL;
      FUNCTION    MouseInside : BOOLEAN;        VIRTUAL;
      PROCEDURE   CalculateRect;                VIRTUAL;
      PROCEDURE   RecalcWidth;                  VIRTUAL;
      Function    Inspector:Boolean;                                Virtual;
   END;

   SourcePtr = ^Source;
   Source = OBJECT (Basis)
      direction : sourceDir;
      CONSTRUCTOR Init(TheNum: Integer; x : REAL; thePlot : plotPtr);
      DESTRUCTOR  Done;                         Virtual;
      PROCEDURE   Display;                      VIRTUAL;
      PROCEDURE   remove;                       VIRTUAL;
      FUNCTION    MouseInside: BOOLEAN;         VIRTUAL;
      PROCEDURE   RecalcWidth;                  VIRTUAL;
      Function    Inspector:Boolean;            Virtual;
      Procedure   AddWaveToDVec;                Virtual;
      Procedure   CWAddWaveToDVec;
      Procedure   SchAddWaveToDVec;
      Procedure   DiffAddWaveToDVec;
      Function    Amplitude(LocalTime:Real):Real;    {evaluates the source term}
      Procedure   Clear;                        Virtual;
   END;

   DetectorPtr = ^Detector;
   Detector = OBJECT (Basis)
      RectU,
      RectD:   Array[1..4] of PointType;
      CONSTRUCTOR Init(TheNum: Integer; x : REAL; thePlot : plotPtr);
      DESTRUCTOR  Done;                                              Virtual;
      PROCEDURE   Display;                                           VIRTUAL;
      PROCEDURE   Remove;                                            VIRTUAL;
      FUNCTION    MouseInside: BOOLEAN;                              VIRTUAL;
      PROCEDURE   GetDetectorValues(Var Right,Left,Magnitude: Real); Virtual;
      Function    Inspector:Boolean;                                  Virtual;
      PROCEDURE   CalculateIcon;
      Procedure   Clear;                                             Virtual;
   END;

   ReflectorPtr = ^Reflector;
   Reflector = OBJECT (Basis)
      IconU,
      IconD:   Array[1..4] of PointType;
      Side:    Word;
      CONSTRUCTOR Init(x : REAL; thePlot : plotPtr);
      DESTRUCTOR  Done;                                              Virtual;
      PROCEDURE   Display;                                           VIRTUAL;
      PROCEDURE   Remove;                                            VIRTUAL;
      FUNCTION    MouseInside: BOOLEAN;                              VIRTUAL;
      Function    Inspector:Boolean;                                 Virtual;
      PROCEDURE   AddWaveToDVec;                                     Virtual;
      PROCEDURE   Clear;                                             Virtual;
      PROCEDURE   CalculateIcon;
   END;

Implementation
Uses Wav_Anly;
{========================================================================}
{               Source Methods                                           }
{========================================================================}


CONSTRUCTOR Source.Init(TheNum: Integer; x : REAL; thePlot : plotPtr);
VAR dx      :REAL;
BEGIN
   Basis.Initialize;
   TheBasisType:=Sou;
   With ParSource Do
     Begin
     num := TheNum;
     x0 := x; {Position of source inside the medium}
     y0 := 0.0;
     Amp:=0.5; {Amplitude of source}
     Delay:=0.5;
     A:=0.2;    {Width of Gaussian}
     Freq:=1;
     PhaseShift:=0;
     parentPointer := thePlot;
     outLined := false;
     radius   := 6;
     ForceAmplitude:=False;
     kind:= Gaussian;
     objColor := defSourceColor;
     direction := ToRIGHT;
     dx:= (StopEdge-StartEdge)/(vacuumPts-1);
     CalcGridIndex;
     End; {of With ParSource}
END;

DESTRUCTOR Source.Done;
BEGIN
   Plot(parentPointer^).lockFocus;
   remove;
   basis.done;
END;

Function Source.Amplitude(LocalTime:Real):Real;

VAR
   LocalAmp,
   Value:       Real;

Begin
With ParSource do
     if not(kind=UserDefined) And (ParSource.Kind<>Sine) then
     begin
          If (not(Periodic)) and (LocalTime>SourcePeriod) then exit;
          If (Periodic) and (LocalTime>SourcePeriod) then
             LocalTime:=LocalTime-SourcePeriod*Trunc(LocalTime/SourcePeriod);
     end;

With ParSource do
     If Noiseon then
        begin
             {PhaseShift:=PhaseNoise*(-1 + Random(3))*(Pi/180) + PhaseShift; }
             If Random(Trunc(CoherenceTime/Abs(dt)))=0 Then PhaseShift:=2*pi*Random(1000)/999;
             LocalAmp:=Amp + AmpNoise*(-0.5 + Random(1001)/1000);
        end
     else LocalAmp:=Amp;

With parSource Do                {xxx}
  Case Kind of
       Sine:Amplitude:=LocalAmp*Sin(2*Pi*Freq*LocalTime + PhaseShift);
       Gaussian:
         Begin
         Value:=Sqr(2*(LocalTime-Delay)/A);
         If (Value<10) then Amplitude:=LocalAmp*Exp(-Value) else Amplitude:=0
         End;
       ModGaussian:
         Begin
         Value:=Sqr(2*(LocalTime-Delay)/A);
         If (Value<10) then Amplitude:=LocalAmp*Exp(-Value)*Sin(2*Pi*Freq*LocalTime + PhaseShift) else Amplitude:=0;
         End;
       Pulse:
         Begin
         If (LocalTime<(Delay-A/2)) then Amplitude:=0;
                If (((Delay-A/2)<LocalTime) and (LocalTime<Delay)) then Amplitude:=LocalAmp;
                If (((Delay)<LocalTime) and (LocalTime<(A/2+Delay))) then Amplitude:=-LocalAmp;
                If (LocalTime>(A/2+Delay)) then Amplitude:=0;
         end;
       Step:
         Begin
                If (LocalTime<(Delay-A/2)) then Amplitude:=0;
                If (((Delay-A/2)<LocalTime) and (LocalTime<(A/2+Delay))) then Amplitude:=LocalAmp;
                If (LocalTime>(A/2+Delay)) then Amplitude:=0;
         End;
       WhiteNoise:Amplitude:=LocalAmp*(-0.5 + Random(1001)/1000);
       UserDefined:
         Begin
              With Parser do
                   begin
                        Init;
                        Parse('t',' ',' ',' ',FuncStr);
                        Amplitude:=F(LocalTime,0,0,0);
                   end;
         End;

    end; {of Kind Case}

End;
Procedure Source.AddWaveToDVec;
Begin
  Case EquationType of
  EandM:CWAddWaveToDVec;
  diffusion: DiffAddWaveToDVec;
  Schroedinger:SchAddWaveToDVec;
  End;{of Case}
End;

Procedure Source.DiffAddWaveToDVec;
Var Value,dx:Real;
Begin
dx:= (StopEdge-StartEdge)/(vacuumPts-1);
With ParSource Do
  Begin
  Value:=Amplitude(Time);
  yVec.VecPtr^[gridIndex]:=Value;
  yPrevVec.VecPtr^[gridIndex]:=Value;
  End;
End;

Procedure Source.SchAddWaveToDVec;
Var dx:Real;

function  rePsi(t,x:Real):Real;
Begin
rePsi:=Cos(Sqrt(2*600)*x-600*t);
end;
function  imPsi(t,x:Real):Real;
Begin
imPsi:=Sin(Sqrt(2*600)*x-600*t);
end;
Begin
dx:= (StopEdge-StartEdge)/(vacuumPts-1);
parsource.direction:=Bidirectional;
With ParSource Do
  if EvenTime
    Then
    Begin
    yPrevVec.VecPtr^[gridIndex]:=imPsi(time,0);
    yPrevVec.VecPtr^[gridIndex-1]:=imPsi(time,-dx);
    yPrevVec.VecPtr^[gridIndex+1]:=imPsi(time,dx);
    yVec.VecPtr^[gridIndex]:=rePsi(time-dt,0);
    Case Direction  of
      toLeft:
        Begin
        yPrevVec.VecPtr^[gridIndex+2]:=0;
        yVec.VecPtr^[gridIndex+1]:=0;
        end;
      toRight:
        Begin
        yPrevVec.VecPtr^[gridIndex-2]:=0;
        yVec.VecPtr^[gridIndex-1]:=0;
        end;
      end;{of Case}
    End
    Else
    Begin
    yVec.VecPtr^[gridIndex]:=rePsi(time,0);
    yVec.VecPtr^[gridIndex-1]:=rePsi(time,-dx);
    yVec.VecPtr^[gridIndex+1]:=rePsi(time,+dx);
    yPrevVec.VecPtr^[gridIndex]:=imPsi(time-dt,0);
    Case Direction of
      toLeft:
        Begin
        yPrevVec.VecPtr^[gridIndex+1]:=0;
        yVec.VecPtr^[gridIndex+2]:=0;
        end;
      toRight:
        Begin
        yPrevVec.VecPtr^[gridIndex-1]:=0;
        yVec.VecPtr^[gridIndex-2]:=0;
        end;
      end;{of Case}
    End; {of oddTime Else}
End;

Procedure Source.CWAddWaveToDVec;
Var
   AdjustedTime,
   Value:               Real;
   ke,pe,energy:        Real;
   temp:                Real;


Procedure ReverseTime;
Begin
If Not(ParSource.ForceAmplitude) Then
With ParSource Do
 If Direction= ToLeft then Direction:=ToRight Else
 If Direction= ToRight then Direction:=ToLeft;
End;

Begin
 With ParSource do
 AdjustedTime:=(Time);
 gridPointDensities(gridIndex,ke,pe);
 energy:=ke+pe;
 {The wave energy at the the source's position}
 if dt<0 Then reverseTime;
  With ParSource Do
  Case Direction of
       toRight:
         begin
         Value:=Amplitude(AdjustedTime);
         If ForceAmplitude
           Then
           Begin
           yVec.VecPtr^[gridIndex]:=Value-yPrevVec.VecPtr^[gridIndex];
           yPrevVec.VecPtr^[gridIndex]:=0;
           End
           Else
           Begin
           if dt<0 Then Value:=-Value;
           yVec.VecPtr^[gridIndex]:=yVec.VecPtr^[gridIndex]+Value;
           end;
         End;
       toLeft:
         begin
         Value:=Amplitude(AdjustedTime);
         If ForceAmplitude
         Then
           Begin;
           yPrevVec.VecPtr^[gridIndex]:=Value-yVec.VecPtr^[gridIndex];
           yVec.VecPtr^[gridIndex]:=0;
           End
           Else
           Begin
           if dt<0 Then Value:=-Value;
           yPrevVec.VecPtr^[gridIndex]:=yPrevVec.VecPtr^[gridIndex]+Value;
           End;
         End;
       biDirectional:
         Begin
         Value:=Amplitude(AdjustedTime);
         If ForceAmplitude
           Then
           Begin
           temp:=yVec.VecPtr^[gridIndex];
           yVec.VecPtr^[gridIndex]:=Value-yPrevVec.VecPtr^[gridIndex];
           yPrevVec.VecPtr^[gridIndex]:=Value-temp;
           End
           Else
           Begin
           if dt<0 Then Value:=-Value;
           yVec.VecPtr^[gridIndex]:=yVec.VecPtr^[gridIndex]+Value;
           yPrevVec.VecPtr^[gridIndex]:=yPrevVec.VecPtr^[gridIndex]+Value;
           end;
         end;
    End;{of Case}
  {Change in wave energy at the the source's position}
  gridPointDensities(gridIndex,ke,pe);
  energy:=ke+pe-energy;
  With ParSource do Power:=energy*(StopEdge-StartEdge)/(Numpts-2*hiddenPts-1)/dt;
if dt<0 Then reverseTime
End;

PROCEDURE Source.Display;
VAR
   oldColor : WORD;
   oldFillStyle : FillSettingsType;
BEGIN
   hideCursor;
   Plot(parentPointer^).lockFocus;
   oldColor := GetColor;
   GetFillSettings(oldFillStyle);
   SetFillStyle(SOLIDFILL, objColor);
   With ParSource Do
   With Plot(parentPointer^).Scale Do
   IF (outlined) THEN BEGIN
      SetWriteMode(XORPut);
      Line(MapX(x0),MapY(Ymin),MapX(x0),MapY(Ymax));
      If not(Running) then
         Graph.rectangle(mapx(x0) - radius, mapy(y0) - radius, mapx(x0) + radius, mapy(y0) + radius);
      SetWriteMode(COPYPut);
   END
   ELSE
       begin;
             If not(Running) then
                FillEllipse(mapx(x0), mapy(y0), radius, radius);
             SetColor(objColor);
             Line(MapX(x0),MapY(Ymin),MapX(x0),MapY(Ymax));
       end;
   SetColor(oldColor);
   SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
   showCursor;
END;

PROCEDURE Source.remove;
VAR
   oldColor : WORD;
   oldFillStyle : FillSettingsType;
BEGIN
   hideCursor;
   Plot(parentpointer^).lockFocus;
   oldColor := GetColor;
   GetFillSettings(oldFillStyle);
   SetColor(graphBackColor);
   SetFillStyle(SOLIDFILL, graphBackColor);
   With ParSource Do
   With Plot(parentPointer^).Scale Do
   IF outlined THEN BEGIN
      SetColor(oldColor);
      SetWriteMode(XORput);
      Line(MapX(x0),MapY(Ymin),MapX(x0),MapY(Ymax));
      If not(Running) then
         Graph.rectangle(mapx(x0) - radius, mapy(y0) - radius, mapx(x0) + radius, mapy(y0) + radius);
      SetWriteMode(COPYPut);
   END
   ELSE
       begin;
             Line(MapX(x0),MapY(Ymin),MapX(x0),MapY(Ymax));
             If not(Running) then
                FillEllipse(mapx(x0), mapy(y0), radius, radius);
       end;
   SetColor(oldColor);
   SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
   showCursor;
END;

FUNCTION Source.MouseInside : BOOLEAN;
VAR
   xpos, ypos : REAL;
   inside     : BOOLEAN;
BEGIN
   MouseInside := False;
   Plot(parentPointer^).lockFocus;
   MousePosn(xpos, ypos, 1, 1, inside);
   With ParSource Do IF ( mapx(xpos) >= mapx(x0) - radius) AND (mapx(xpos) <= mapx(x0) + radius) AND
      (mapy(ypos) >= mapy(y0) - radius)  AND (mapy(ypos) <= mapy(y0) + radius) AND
      inside
   THEN
       MouseInside := True;
END;

PROCEDURE Source.RecalcWidth;
BEGIN
   ParSource.radius := 10;
END;


Function Source.Inspector:Boolean;
Const timePts=512;
Var
   Leave:                Boolean;
   OldNumber,
   OldColour,
   OldBack:              Word;
   DragBut,
   FuncBut:              TButtons;
   Opt_Button, Del_Button, Ok_Button : TButton;
   Info,
   PositionSlider:       TSliders;
   LocAmp,LocFreq,LocalDt,
   tempAmp,
   LocA,LocDelay:        Real;
   LocalYVec,
   LocalTimeVec:         Dvector;
   dx,Xinc,Yinc:         Real;
   Tot:                  String;

Procedure LocalInit;  Forward;

Procedure SetSpecialButtons;
Begin
   Opt_Button.Create(0.40, 0.53, 'Adv Options','o');
   Del_Button.Create(0.28, 0.58, 'Delete','Del');
   Ok_Button. Create(0.28, 0.53, 'Ok','Return');
end;

Procedure SetButtons;
Begin;
      With FuncBut do
           begin;
                 Init;
                 Create(11, 0.13, 0.89, 'Sine');
                 Create(22, 0.13, 0.85, 'Gaussian');
                 Create(33, 0.13, 0.81, 'Pulse');
                 Create(44, 0.13, 0.77, 'Step');
                 Create(23, 0.13, 0.73, 'Mod-Gauss');
                 Create(26, 0.13, 0.69, 'White Noise');
                 Create(24, 0.13, 0.65, 'User Defined');
{                Create(25, 0.40, 0.55, 'Adv Options');
                 Create(55, 0.28, 0.59, 'Delete');
                 Create(66, 0.28, 0.55, 'Exit');  }
                 Number:=11;
                 FuncBut.DrawAll;
           end;
      SetSpecialButtons;
      With DragBut do
           begin;
                 Init;
                 Create(11,0.13,0.59,'Drag');
                 Create(22,0.13,0.55,'No Drag');
                 Number:=11;
                 DragBut.DrawAll;
           end;

      With PositionSlider Do
          Begin
          Message('--------------------------- Edit The Source Parameters --------------------------');
          StopKeys.Clear;
            Init;
            Create(1,StartEdge,StopEdge,x0,0.10,0.02,0.89,4,ToStr(StartEdge),ToStr(StopEdge),
                   'Source Position',False);
            DrawAll;
          End;
End;

Procedure SetSineSliders;

Begin;
      LocAmp:=ParSource.Amp;
      LocFreq:=ParSource.Freq;
      With Info do
           begin;
                 Init;
                 Create(1,-5,5,LocAmp,0.38,0.93,0.64,2,'-5','5','Amp',True);
                 Create(2,0.001,20,LocFreq,0.48,0.93,0.64,3,'0.001','20','Freq',True);
                 DrawAll;
           end;
End;
Procedure SetWhiteNoiseSliders;

Begin;
      LocAmp:=ParSource.Amp;
      With Info do
           begin;
                 Init;
                 Create(1,0,5,LocAmp,0.38,0.93,0.64,2,'0','5','Amp',True);
                 DrawAll;
           end;
End;

Procedure SetModGaussianSliders;

Begin;
      LocAmp:=ParSource.Amp;
      LocA:=ParSource.A;
      LocDelay:=ParSource.Delay;
      LocFreq:=ParSource.Freq;

        With ParSource do
        With Info do
           begin;
                 Init;
                 Create(1,-5,5,LocAmp,0.32,0.90,0.64,2,'-5','5','Amp',True);
                 Create(2,0.1,SourcePeriod,LocA,0.39,0.90,0.64,2,'0.1',Tot,'Width',True);
                 Create(3,0,SourcePeriod,LocDelay,0.46,0.90,0.64,2,'0',Tot,'Delay',True);
                 Create(4,0.001,20,LocFreq,0.54,0.90,0.64,3,'0.001','20','Freq',True);
                 DrawAll;
           end;
End;

Procedure SetPulseStepGaussianSliders;

Begin;
          LocAmp:=ParSource.Amp;
          LocA:=ParSource.A;
          LocDelay:=ParSource.Delay;

      With ParSource do
      With Info do
           begin;
                 Init;
                 Create(1,-5,5,LocAmp,0.33,0.93,0.64,2,'-5','5','Amp',True);
                 Create(2,0.01,SourcePeriod,LocA,0.42,0.93,0.64,2,'0.01',Tot,'Width',True);
                 Create(3,0, SourcePeriod,LocDelay,0.51,0.93,0.64,2,'0',Tot,'Delay',True);
                 DrawAll;
           end;
End;


Procedure AddNoise;

Begin
     With ParSource do
          If Noiseon then
             begin
                  {PhaseShift:=PhaseNoise*(-1 + Random(3))*(Pi/180) + PhaseShift;}
                  If Random(Trunc(CoherenceTime/Abs(dt)))=0 Then PhaseShift:=2*pi*Random(1000)/999;
                  TempAmp:=LocAmp + AmpNoise*(-0.5 + Random(1001)/1000);
             end
          Else
            TempAmp:=LocAmp;
End;

Procedure DrawSine;

Var
   i:           Integer;
   Value,
   localtime:   Real;

Begin;
      SelectViewPort(6);
      SelectScale(5);
      Axis(0,-6,Xinc,Yinc);

      LocalTime:=0;
      With ParSource do
      For i:=1 to timePts do
          begin;
                AddNoise;
                Value:=TempAmp*Sin(2*Pi*LocFreq*LocalTime + PhaseShift);
                localYVec.Put(i,Value);
                LocalTimeVec.Put(i,LocalTime);
                LocalTime:=LocalTime + LocalDt;
          end;
      SetColor(White);
      PlotDvectors(LocalTimeVec,LocalYVec,1,timePts);
End;

Procedure DrawGaussian;
Var
   i:           Integer;
   Value,
   LocalTime:           Real;

Begin;
      SelectViewPort(6);
      SelectScale(5);
      Axis(0,-6,Xinc,Yinc);

      LocalTime:=0;
      For i:=1 to timePts do
          begin;
                AddNoise;
                Value:=Sqr(2*(LocalTime-LocDelay)/LocA);
                If (Value<10) then Value:=TempAmp*Exp(-Value) else Value:=0;
                LocalYVec.Put(i,Value);
                LocalTimeVec.Put(i,LocalTime);
                LocalTime:=LocalTime + LocalDt;
          end;
      SetColor(White);
      PlotDvectors(LocalTimeVec,LocalYVec,1,timePts);
End;

Procedure DrawWhiteNoise;
Var
   i:           Integer;
   Value,
   LocalTime:           Real;

Begin;
      SelectViewPort(6);
      SelectScale(5);
      Axis(0,-6,Xinc,Yinc);

      LocalTime:=0;
      For i:=1 to timePts do
          begin;
                Value:=LocAmp*(-0.5 + Random(1001)/1000);;
                LocalYVec.Put(i,Value);
                LocalTimeVec.Put(i,LocalTime);
                LocalTime:=LocalTime + LocalDt;
          end;
      SetColor(White);
      PlotDvectors(LocalTimeVec,LocalYVec,1,timePts);
End;

Procedure DrawModGaussian;
Var
   i:           Integer;
   Value,
   LocalTime:           Real;

Begin;
      SelectViewPort(6);
      SelectScale(5);
      Axis(0,-6,Xinc,Yinc);

      LocalTime:=0;
      With ParSource do
      For i:=1 to timePts do
          begin;
                AddNoise;
                Value:=Sqr(2*(LocalTime-LocDelay)/LocA);
                If (Value<10) then Value:=TempAmp*Exp(-Value)*Sin(2*Pi*LocFreq*LocalTime + PhaseShift) else Value:=0;
                LocalYVec.Put(i,Value);
                LocalTimeVec.Put(i,LocalTime);
                LocalTime:=LocalTime + LocalDt;
          end;
      SetColor(White);
      PlotDvectors(LocalTimeVec,LocalYVec,1,timePts);
End;

Procedure DrawPulse;

Var
   i:           Integer;
   Value,
   LocalTime:           Real;

Begin;
      SelectViewPort(6);
      SelectScale(5);
      Axis(0,-6,Xinc,Yinc);

      LocalTime:=0;
      For i:=1 to timePts do
          begin;
                AddNoise;
                If (LocalTime<(LocDelay-LocA/2)) then Value:=0;
                If (((LocDelay-LocA/2)<LocalTime) and (LocalTime<(LocDelay))) then Value:=tempAmp;
                If (((LocDelay)<LocalTime) and (LocalTime<(LocA/2+LocDelay))) then Value:=-tempAmp;
                If (LocalTime>(LocA/2+LocDelay)) then Value:=0;
                LocalYVec.Put(i,Value);
                LocalTimeVec.Put(i,LocalTime);
                LocalTime:=LocalTime + LocalDt;
          end;
      SetColor(White);
      PlotDvectors(LocalTimeVec,LocalYVec,1,timePts);
End;

Procedure DrawStep;

Var
   i:           Integer;
   Value,
   LocalTime:           Real;

Begin;
      SelectViewPort(6);
      SelectScale(5);
      Axis(0,-6,Xinc,Yinc);

      LocalTime:=0;
      For i:=1 to timePts do
          begin;
                AddNoise;
                If (LocalTime<(LocDelay-LocA/2)) then Value:=0;
                If (((LocDelay-LocA/2)<LocalTime) and (LocalTime<(LocA/2+LocDelay))) then Value:=tempAmp;
                If (LocalTime>(LocA/2+LocDelay)) then Value:=0;
                LocalYVec.Put(i,Value);
                LocalTimeVec.Put(i,LocalTime);
                LocalTime:=LocalTime + LocalDt;
          end;
      SetColor(White);
      PlotDvectors(LocalTimeVec,LocalYVec,1,timePts);
End;

Function DrawParsedFunction : boolean;

Var
   i:           Integer;
   Value,
   LocalTime:           Real;
   Ok : boolean;
Begin;
      SelectViewPort(6);
      SelectScale(5);
      Axis(0,-6,Xinc,Yinc);

      Parser.Init;
      Parser.Parse('t',' ',' ',' ',ParSource.FuncStr);
      ErrorFound := False;
      HaltIfError := false;
      LocalTime:=0;
      i:=1;
      while (i<=timePts) and (not ErrorFound) do
          begin
                AddNoise;
                Value:=Parser.F(LocalTime,0,0,0);
                LocalYVec.Put(i,Value);
                LocalTimeVec.Put(i,LocalTime);
                LocalTime:=LocalTime + LocalDt;
                inc(i);
          end;
      SetColor(White);
      if not ErrorFound then PlotDvectors(LocalTimeVec,LocalYVec,1,timePts);
      DrawParsedFunction := not ErrorFound;
      ErrorFound := False;
      HaltIfError := true;
End;

Procedure RemoveGraph;

Begin;
      SelectViewPort(6);
      SelectScale(5);
      DrawAxis(0,-6,Xinc,Yinc, Blue);
      SetColor(Black);
      PlotDVectors(LocalTimeVec,LocalYVec,1,timePts);
End;

Procedure RemoveSliders;

Begin;
      GraphBackColor:=Blue;
      OpenViewPort(5);
      GraphBackColor:=Black;
      OpenViewPort(6);
      Info.Done;
      FuncBut.DrawAll;
      SetSpecialButtons;
      DragBut.DrawAll;
End;

Procedure AdvdScreen;

Var
   AvdScreen:       TinputScreen;
   Ok,Cancel,
   Leave1,
   Leave2,
   Leave3:          Boolean;
   OldSourcePeriod: Real;

Function DefineFuncString: String;

Var
   Ok,Cancel,
   Leave:       Boolean;
   FuncScreen:  TinputScreen;
   fieldOffset: Integer;

Begin
     With FuncScreen do
          begin
               fieldOffset:=0;
               Init;
               If EquationType in [EandM,SineGordon,KleinGordon,DoubleSineGordon] Then
               Begin
               DefineInputPort(0.20,0.85,0.40,0.80);
               LoadLine(' Source Direction:                 ');
               LoadLine('              #1 Bidirectional     ');
               LoadLine('              #1 Right             ');
               LoadLine('              #1 Left              ');
               fieldOffset:=3;
               end
               Else DefineInputPort(0.20,0.85,0.40,0.70);
               LoadLine('Function =                                         ');
               LoadLine('"                                                 "');
               LoadLine('');
               LoadLine('       [  Ok  ]      [Cancel]      [ Help ]        ');
               SetHelpFile('Wave.Hlp','Function Help');
               SetString(1+fieldOffset,ParSource.FuncStr);
               If EquationType in [EandM,SineGordon,KleinGordon,DoubleSineGordon] Then
                 Case ParSource.Direction of
                    Bidirectional: SetRadioButton('1',1);
                    ToRight:       SetRadioButton('1',2);
                    ToLeft:        SetRadioButton('1',3);
                 End; { of Case }
               Leave:=False;
               Repeat
                     AcceptScreen;
                     Parser.Init;
                     RemoveGraph;
                     ParSource.FuncStr:=GetString(1+FieldOffset);
                     If Parser.Parse('t',' ',' ',' ',ParSource.FuncStr)
                     and DrawParsedFunction then Leave:=true
                     else Announce('That function is not acceptable - f(t).');
               Until Leave or CancelPressed;
               if GetBoolean(2+fieldOffset) Then
                 Begin
                 If EquationType in [EandM,SineGordon,KleinGordon,DoubleSineGordon] Then
                  With ParSource do
                            Case GetRadioButton('1') of
                                 1:   Direction:=Bidirectional;
                                 2:   Direction:=ToRight;
                                 3:   Direction:=ToLeft;
                            End; { of Case }
                 End;
               Done;{with FuncScreen}
          end;

{     If OldNumber=24 then
        begin
             RemoveGraph;
             DrawParsedFunction;
        end;}
End;

Begin
     if ParSource.Kind=UserDefined Then
       begin
       DefineFuncString;
       FuncBut.Number:=OldnUmber;
       FuncBut.DrawAll;
       exit;
       end;
     OldSourcePeriod:=ParSource.SourcePeriod;
     With AvdScreen do
          begin
               Init;
               DefineInputPort(0.30,0.77,0.15, 0.88);
               LoadLine('     Advanced Source Variables     ');
               LoadLine(' Noise On: #F                      ');
               LoadLine('   Amplitude       {     }         '    );
               LoadLine('   Coherence Time  {     }         '    );
               LoadLine('-----------------------------------');
               LoadLine(' Source Direction:                 ');
               LoadLine('              #1 Bidirectional     ');
               LoadLine('              #1 Right             ');
               LoadLine('              #1 Left              ');
               LoadLine('-----------------------------------');
               LoadLine(' Periodic:  #F                     ');
               LoadLine(' Period:      {      } Seconds     ');
               LoadLine('-----------------------------------');
               LoadLine(' Y(x) will track source:  #F  ');
               LoadLine('-----------------------------------');
               LoadLine('');
               LoadLine('  [  Ok  ]   [Cancel]   [ Help ]   ');
               SetHelpFile('Wave.Hlp' , 'Advanced Help');
               With ParSource do
                    begin
                         SetBoolean(1, NoiseOn);
                         SetNumber(2, AmpNoise);
                         SetNumber(3, CoherenceTime);
                    end;
               Case ParSource.Direction of
                    Bidirectional: SetRadioButton('1',1);
                    ToRight:       SetRadioButton('1',2);
                    ToLeft:        SetRadioButton('1',3);
               End; { of Case }
               SetBoolean(7, ParSource.Periodic);
               SetNumber(8,ParSource.SourcePeriod);
               SetBoolean(9, ParSource.ForceAmplitude);
               Leave1:=False;
               Leave2:=False;
               Leave3:=False;
               Repeat
                     AcceptScreen;
                     If (GetNumber(2)>=0) and (GetNumber(2)<=5) then Leave1:=True else
                        Announce('The noise amplitude is out of range: 0<= Amp <=5.');
                     If (GetNumber(3)>=0) and (GetNumber(3)<=10) then Leave2:=True else
                        Announce('The noise phase is out of range: 0<= Phase <=10.');
                     If (GetNumber(8)>=1) and (GetNumber(8)<=10) then Leave3:=True else
                        Announce('The period of the Source is out of range: 1<= Period <=10.');
               Until Leave1 and Leave2 and Leave3;

              Ok:=GetBoolean(10);

              If Ok then
                  begin
                       With ParSource do
                            begin
                                 Noiseon:=GetBoolean(1);
                                 AmpNoise:=GetNumber(2);
                                 CoherenceTime:=GetNumber(3);
                            end;
                       With ParSource do
                            Case GetRadioButton('1') of
                                 1:   Direction:=Bidirectional;
                                 2:   Direction:=ToRight;
                                 3:   Direction:=ToLeft;
                            End; { of Case }

                       ParSource.Periodic:=GetBoolean(7);
                       ParSource.SourcePeriod:=GetNumber(8);
                       ParSource.ForceAmplitude:=GetBoolean(9);
                       LocalDt:=Parsource.sourcePeriod/TimePts;
                       If not(PArSource.Noiseon) then ParSource.PhaseShift:=0;
                  end;
               Done;
          end;

          FuncBut.Number:=OldnUmber;
          FuncBut.DrawAll;

          With ParSource do
               If NoiseOn then
                  begin
                       RemoveGraph;
                       With ParSource do
                            begin
                            Freq:=LocFreq;
                            Amp:=LocAmp;
                            A:=LocA;
                            Delay:=LocDelay;
                            end;

                       LocalInit;
                  end;

          With ParSource do
          If SourcePeriod<>OldSourcePeriod then
             begin
                   RemoveGraph;
                   RemoveSliders;

                   {timePts:=trunc(SourcePeriod*vacuumPts)+1;}
                   LocalDt:=SourcePeriod/TimePts;
                   LocalYVec.Resize(timePts);
                   LocalTimeVec.Resize(timePts);

                   DefineScale(5,0,SourcePeriod,-6,6 );
                   Tot:=Num2Str(SourcePeriod,3);
                   Xinc:=SourcePeriod/4;
                   Yinc:=3;
                   With ParSource do
                        begin
                             Freq:=LocFreq;
                             Amp:=LocAmp;
                             A:=LocA;
                             Delay:=LocDelay;
                        end;

                   LocalInit;
             end;
End;

Procedure ExecuteButtons;

Begin;
            If DragBut.Changed then
               Case DragBut.Number of
                    11: DragIt:=True;
                    22: DragIt:=True;
               End;
            If PositionSlider.Changed Then
              Begin
              remove;
              x0:=PositionSlider.Value(1);
              GraphBackColor:=oldBack;
              theWave.Display;
              End;
            If FuncBut.Changed then
               Begin
               Case FuncBut.Number of
                    11: begin;
                              RemoveGraph;
                              RemoveSliders;
                              ParSource.Kind:=Sine;
                              SetSineSliders;
                              DrawSine;
                              OldNumber:=11;
                        end;
                    22: begin;
                              RemoveGraph;
                              RemoveSliders;
                              ParSource.Kind:=Gaussian;
                              SetPulseStepGaussianSliders;
                              DrawGaussian;
                              OldNumber:=22;
                        end;
                    23: begin;
                              RemoveGraph;
                              RemoveSliders;
                              ParSource.Kind:=ModGaussian;
                              SetModGaussianSliders;
                              DrawModGaussian;
                              OldNumber:=23;
                        end;
                    24: begin
                             RemoveGraph;
                             RemoveSliders;
                             ParSource.Kind:=UserDefined;
                             DrawParsedFunction;
                             OldNumber:=24;
                        end;
                    26: begin;
                              RemoveGraph;
                              RemoveSliders;
                              ParSource.Kind:=WhiteNoise;
                              SetWhiteNoiseSliders;
                              DrawWhiteNoise;
                              OldNumber:=26;
                        end;
                    33: begin;
                              RemoveGraph;
                              RemoveSliders;
                              ParSource.Kind:=Pulse;
                              SetPulseStepGaussianSliders;
                              DrawPulse;
                              OldNumber:=33;
                        end;
                    44: begin;
                              RemoveGraph;
                              RemoveSliders;
                              ParSource.Kind:=Step;
                              SetPulseStepGaussianSliders;
                              DrawStep;
                              OldNumber:=44;
                        end;
{                   25: AdvdScreen;
                    55: begin;
                              Disabled:=True;
                              Leave:=True;
                        end;
                    66: Leave:=True;  }
               End;{of Case}
               SelectViewport(5);
               Print(2,1,'Function Type:');
               End;
               If Opt_Button.Clicked then AdvdScreen;
               If Del_Button.Clicked then begin
                  Disabled:=True;
                  Leave:=True;
               end;
               If OK_Button.Clicked then Leave:=True;  {Exit}
End;
Procedure ExecuteSliders;

Begin;
      If OldNumber=24 then exit;

      If Info.Changed then
         Case FuncBut.Number of
              11: begin;
                        LocAmp:=Info.Value(1);
                        LocFreq:=Info.Value(2);
                        RemoveGraph;
                        DrawSine;
                  end;
              22: begin;
                        LocAmp:=Info.Value(1);
                        LocA:=Info.Value(2);
                        LocDelay:=Info.Value(3);
                        RemoveGraph;
                        DrawGaussian;
                  end;
              23: begin;
                        LocAmp:=Info.Value(1);
                        LocA:=Info.Value(2);
                        LocDelay:=Info.Value(3);
                        LocFreq:=Info.Value(4);
                        RemoveGraph;
                        DrawModGaussian;
                  end;
              26: begin;
                        LocAmp:=Info.Value(1);
                        RemoveGraph;
                        DrawWhiteNoise;
                  end;
              33: begin;
                        LocAmp:=Info.Value(1);
                        LocA:=Info.Value(2);
                        LocDelay:=Info.Value(3);
                        RemoveGraph;
                        DrawPulse;
                  end;
              44: begin;
                        LocAmp:=Info.Value(1);
                        LocA:=Info.Value(2);
                        LocDelay:=Info.Value(3);
                        RemoveGraph;
                        DrawStep;
                  end;
         End;
End;

Procedure SetSourceData;

Begin;
                 Case OldNumber of
                      11:begin;
                               ParSource.Kind:=Sine;
                               ParSource.Freq:=locFreq;
                               ParSource.Amp:=locAmp;
                         end;
                      22:begin;
                               ParSource.Kind:=Gaussian;
                               ParSource.Delay:=LocDelay;
                               ParSource.A:=LocA;
                               ParSource.Amp:=LocAmp;
                         end;
                      23:begin;
                               ParSource.Kind:=ModGaussian;
                               ParSource.Delay:=LocDelay;
                               ParSource.A:=LocA;
                               ParSource.Amp:=LocAmp;
                               ParSource.Freq:=LocFreq;
                         end;
                      26: begin;
                               ParSource.Kind:=WhiteNoise;
                               ParSource.Amp:=LocAmp;
                         end;
                      24:;
                      33:begin;
                               ParSource.Kind:=Pulse;
                               ParSource.Delay:=LocDelay;
                               ParSource.A:=LocA;
                               ParSource.Amp:=LocAmp;
                         end;
                      44:begin;
                               ParSource.Kind:=Step;
                               ParSource.Delay:=LocDelay;
                               ParSource.A:=LocA;
                               ParSource.Amp:=LocAmp;
                         end;
                 End;

                 Case DragBut.Number of
                      11: DragIt:=True;
                      22: DragIt:=False;
                 End;
{      If not(FuncBut.Number=55) then
         begin;
               Disabled:=False;
         end;}
End;
Procedure LocalInit;

Begin;
                Case ParSource.Kind of
                      Sine:     begin;
                                      FuncBut.Number:=11;
                                      SetSineSliders;
                                      DrawSine;
                                      OldNumber:=11;
                                end;
                      Gaussian: begin;
                                      FuncBut.Number:=22;
                                      SetPulseStepGaussianSliders;
                                      DrawGaussian;
                                      OldNumber:=22;
                                end;
                      ModGaussian: begin;
                                      FuncBut.Number:=23;
                                      SetModGaussianSliders;
                                      DrawModGaussian;
                                      OldNumber:=23;
                                end;
                      WhiteNoise: begin;
                                      FuncBut.Number:=26;
                                      SetWhiteNoiseSliders;
                                      DrawWhiteNoise;
                                      OldNumber:=26;
                                end;
                      UserDefined: begin
                                        FuncBut.Number:=24;
                                        DrawParsedFunction;
                                        OldNumber:=24;
                                   end;
                      Pulse:    begin;
                                      FuncBut.Number:=33;
                                      SetPulseStepGaussianSliders;
                                      DrawPulse;
                                      OldNumber:=33;
                                end;
                      Step:     begin;
                                      FuncBut.Number:=44;
                                      SetPulseStepGaussianSliders;
                                      DrawStep;
                                      OldNumber:=44;
                                end;
                 End;

                 If DragIt then DragBut.Number:=11 else DragBut.Number:=22;

           FuncBut.DrawAll;
           SetSpecialButtons;
           DragBut.DrawAll;
End;

Begin;
      Inspector:=True;
      anly1.remove;
      anly2.remove;
      Leave:=False;
      OldColour:=GetColor;
      OldBack:=GraphBackColor;

      With ParSource do
           begin
                LocalDt:=SourcePeriod/TimePts;
                LocalYVec.Init(timePts);
                LocalTimeVec.Init(timePts);

                DefineScale(5,0,SourcePeriod,-6,6 );
                Tot:=Num2Str(SourcePeriod,3);
                Xinc:=SourcePeriod/4;
                Yinc:=3;
           end;

      DefineViewPort(5, 0.10, 0.90, 0.51, 0.95);
      DefineViewPort(6, 0.60, 0.88, 0.55, 0.94);
      GraphBackColor:=Blue;
      OpenViewPort(5);
      Print(2,1,'Function Type:');
      GraphBackColor:=Black;
      OpenViewPort(6);

      SetButtons;

      LocalInit;

      Repeat
            CheckForEvents;
            ExecuteButtons;
            ExecuteSliders;
      Until Leave;
      SetSourceData;
      If not(OldNumber=24) then Info.Done;
      FuncBut.Done;
      DragBut.Done;
      PositionSlider.Done;

      LocalYVec.Free;
      LocalTimeVec.Free;

      CloseViewPort(6);
      CloseViewPort(5);

      SetColor(OldColour);
      GraphBackColor:=OldBack;

   dx:= (StopEdge-StartEdge)/(vacuumPts-1);
   CalcGridIndex;
   If Disabled then
     Begin
     beep;
     SourceList.DeleteItem(@Self);
     inspector:=FALSE;
     end
     Else Inspector:=True;

   anly1.isVisible:=True;
   anly2.IsVisible:=True;
   Message('');

   SelectViewPort(0);
   DefineScale(6,0,1,0,1);
   SelectScale(6);
   SetFillStyle(SolidFill,Black);
   Bar(MapX(0),MapY(0),MapX(1),MapY(0.08));

   ParSource.PhaseShift:=0;

   MainMenu.Display;
   StopKeys.display;
   Update;
End; {of Source Inspector}

Procedure Source.Clear;

Begin
     With ParSource do
       Begin
       Power:=0;
       phaseShift:=0;
       end;
End;


{========================================================================}
{               Detector Methods                                           }
{========================================================================}


CONSTRUCTOR Detector.init(TheNum: Integer; x : REAL; thePlot : plotPtr);
BEGIN
   Basis.Initialize;
   TheBasisType:=Det;
   With ParDetector Do
     Begin
          x0:=x; {Position of Detector inside the medium}
          num:=TheNum;
          parentPointer := thePlot;
          With Plot(parentPointer^).Scale do Y0:=abs((Ymax-Ymin)/4);
          outLined:=false;
          Side:=6;
          ObjColor:=Num;
          CalcGridIndex;
     End; {of With ParDetector}
  CalculateIcon;
END;

DESTRUCTOR Detector.Done;
BEGIN
     Plot(parentPointer^).lockFocus;
     Remove;
     {DetectorList.DeleteItem(@self);}
     Basis.Done;
END;

Procedure Detector.CalculateIcon;

Begin;
      With ParDetector do
           begin;
                 With Plot(parentPointer^).Scale do
                      y0:=((Ymax-Ymin)/4 + Ymin);
                 RectU[1].x:=MapX(x0);
                 RectU[1].y:=MapY(y0)+side;
                 RectU[2].x:=MapX(x0)-Side;
                 RectU[2].y:=MapY(y0);
                 RectU[3].x:=RectU[1].x;
                 RectU[3].y:=MapY(y0)-Side;
                 RectU[4].x:=MapX(x0)+Side;
                 RectU[4].y:=RectU[2].y;

                 With Plot(parentPointer^).Scale do
                      y0:=(3*(Ymax-Ymin)/4 + Ymin);

                 RectD[1].x:=MapX(x0);
                 RectD[1].y:=MapY(y0)+side;
                 RectD[2].x:=MapX(x0)-Side;
                 RectD[2].y:=MapY(y0);
                 RectD[3].x:=RectD[1].x;
                 RectD[3].y:=MapY(y0)-Side;
                 RectD[4].x:=MapX(x0)+Side;
                 RectD[4].y:=RectD[2].y;
           end;
End;



PROCEDURE Detector.Display;
VAR
   oldColor : WORD;
   oldFillStyle : FillSettingsType;
BEGIN
   hideCursor;
   Plot(parentPointer^).lockFocus;
   oldColor := GetColor;
   GetFillSettings(oldFillStyle);
   CalculateIcon;
   SetFillStyle(SOLIDFILL, objColor);
   With ParDetector do
   With Plot(parentpointer^).Scale Do
   If Outlined then
      begin
           SetWriteMode(XORPut);
           Line(MapX(x0),MapY(Ymin),MapX(x0),MapY(Ymax));
           If not(Running) then
              begin;
                    DrawPoly(4,RectU);
                    DrawPoly(4,RectD);
              end;
           SetWriteMode(CopyPut);
      end
   else
       begin;
             SetColor(Blue);
             Line(MapX(x0),MapY(Ymin),MapX(x0),MapY(Ymax));
             SetColor(White);
             If not(Running) then
                begin;
                      FillPoly(4,RectU);
                      FillPoly(4,RectD);
                end;
       end;
   SetColor(oldColor);
   SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
   showCursor;
END;

PROCEDURE Detector.remove;
VAR
   oldColor : WORD;
   oldFillStyle : FillSettingsType;
BEGIN
   hideCursor;
   Plot(parentpointer^).lockFocus;
   oldColor := GetColor;
   GetFillSettings(oldFillStyle);
   SetColor(graphBackColor);
   SetFillStyle(SOLIDFILL, graphBackColor);
   With ParDetector Do
   With Plot(parentpointer^).Scale Do
   IF outlined THEN BEGIN
      SetColor(oldColor);
      SetWriteMode(XORput);
      Line(MapX(x0),MapY(yMin),MapX(x0),MapY(yMax));
      If not(Running) then
         begin;
               DrawPoly(4,RectU);
               DrawPoly(4,RectD);
         end;
      SetWriteMode(COPYPut);
   END
   ELSE
       begin;
             Line(MapX(x0),MapY(yMin),MapX(x0),MapY(yMax));
             If not(Running) then
                begin;
                      FillPoly(4,RectU);
                      FillPoly(4,RectD);
                end;
       end;
   SetColor(oldColor);
   SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
   showCursor;
END;

FUNCTION Detector.MouseInside : BOOLEAN;
VAR
   xpos, ypos : REAL;
   inside     : BOOLEAN;
BEGIN
   MouseInside := False;
   Plot(parentPointer^).lockFocus;
   MousePosn(xpos, ypos, 1, 1, inside);
   With ParDetector Do
        IF (mapx(xpos) >= mapx(x0) - side) AND
           (mapx(xpos) <= mapx(x0) + side) AND
           (((mapy(ypos) >= mapy(y0) - side) AND (mapy(ypos) <= mapy(y0) + side)) OR
           ((mapy(ypos) <= mapy(-y0) + side) AND (mapy(ypos) >= mapy(-y0) - side))) AND
           Inside
   THEN
       MouseInside := True;
END;

Procedure Detector.GetDetectorValues(Var Right, Left,Magnitude: Real);
Var dx:Real;
Begin;
  Case EquationType of
     EandM:
       begin
             Right:=Yvec.VecPtr^[GridIndex];
             Left:=YPrevVec.VecPtr^[GridIndex+1];
             Magnitude:=DisplayVec.VecPtr^[GridIndex];
        end;
       Schroedinger:
         Begin
          dx:= (StopEdge-StartEdge)/(vacuumPts-1);
          right:=yVec.vecPtr^[gridIndex];
          left:=yPrevVec.VecPtr^[gridIndex];
          {Magnitude:=Sqr(DisplayVec.VecPtr^[gridIndex]);}
          Magnitude:=
             (YVec.VecPtr^[GridIndex]*(YPrevVec.VecPtr^[GridIndex+1]-YPrevVec.VecPtr^[GridIndex-1])-
             YPrevVec.VecPtr^[GridIndex]*(YVec.VecPtr^[GridIndex+1]-YVec.VecPtr^[GridIndex-1]))/dx/2;
         End;
       Else
         Begin
          left:=0;
          right:=0;
          Magnitude:=DisplayVec.VecPtr^[GridIndex];
         End;
       End; {of Case}
End;

Function Detector.Inspector:Boolean;
VAR
   screen1 : TInputScreen;
   Ok,
   Kill,
   Leave:  Boolean;
   Rwave,
   Lwave,
   Swave:      Real;

Begin;
   Inspector:=TRUE;
   GetDetectorValues(Rwave,Lwave,Swave);
   WITH screen1 DO BEGIN
      Init;
      DefineInputPort(0.30,0.72,0.40,0.90);
      LoadLine('            DETECTOR        ');
      LoadLine(' ------- Current Readings---------');
      LoadLine('  Position =  '+Num2Str(x0,5));
      LoadLine('  Left     =  '+Num2Str(Lwave,5));
      LoadLine('  Right    =  '+Num2Str(Rwave,5));
      LoadLine('  Sum      =  '+Num2Str(Swave,5));
      LoadLine(' -----------Attributes------------');
      LoadLine('  X Position           {       }  ');
      LoadLine('  Able to Drag            #T      ');
      LoadLine('                                  ');
      LoadLine('   [  Ok  ]  [Cancel]  [Delete]   ');

      With ParDetector Do
       Begin
            SetNumber(1, x0);
            SetBoolean(2,DragIt);
       End;

      Leave:=False;
      Repeat
            AcceptScreen;
            If ((GetNumber(1)>=Startedge) and (GetNumber(1)<=StopEdge)) then
               Leave:=True
            else
                Announce('The x-position must be within the boundries of the medium.');
      Until Leave;

      Ok:=GetBoolean(3);
      Kill:=GetBoolean(5);

      If Ok then
         With ParDetector do
              begin;
                    If (GetNumber(1)>StopEdge)then PutPos(StopEdge)
                    else
                    If (GetNumber(1)<StartEdge) then PutPos(startEdge)
                    else PutPos(GetNumber(1));
                    DragIt:=GetBoolean(2);
                    CalcGridIndex;
              end;
              If Kill then
                 begin;
                       {DetectorList.DeleteItem(@Self);
                       DetectorList.RenumberDetectors;}
                       Disabled:=True;
                       Inspector:=False;
                 end
                 else Inspector:=True;
      Screen1.Done;
     End;
END;
{========================================================================}
{               Segment Methods                                          }
{========================================================================}



CONSTRUCTOR Segment.Init(x : REAL; thePlot : PlotPtr);
BEGIN
   Basis.Initialize;
   TheBasisType:=Seg;
   x0 := x;
   parSegment.index := 2;
   parentPointer := thePlot;
   outlined := FALSE;
   parSegment.width := (Plot(parentPointer^).Scale.xmax - Plot(parentPointer^).Scale.xmin)/40;
   objColor:= DefSegmentColor;
END;


DESTRUCTOR Segment.Done;
VAR
   xActual : INTEGER;
BEGIN
Plot(parentPointer^).lockFocus;
remove;
{SegmentList.DeleteItem(@self);}
basis.done;
END;

PROCEDURE Segment.Display;
VAR
   oldColor : WORD;
   oldFillStyle : FillSettingsType;
   dummy : integer;
BEGIN
   hideCursor;
   CalculateRect;
   Plot(parentPointer^).lockFocus;
   oldColor := GetColor;
   GetFillSettings(oldFillStyle);
   IF outlined THEN BEGIN
     SetWriteMode(XORPut);
     Graph.rectangle(theRect[1].x, theRect[1].y, theRect[2].x, theRect[3].y);
     SetWriteMode(COPYPut);
   END ELSE BEGIN
      SetFillStyle(SOLIDFILL, objColor);
      FillPoly(4, theRect);
   END;
   SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
   SetColor(oldColor);
   ShowCursor;
END;

PROCEDURE Segment.remove;
VAR
   oldColor : word;
   oldFillStyle : FillSettingsType;
BEGIN
   HideCursor;
   Plot(parentPointer^).lockFocus;
   oldColor := GetColor;
   GetFillSettings(oldFillStyle);
   CalculateRect;
   SetFillStyle(SOLIDFILL, graphBackColor);
   SetColor(GraphBackColor);
   IF outlined THEN BEGIN
      SetColor(oldColor);
      SetWriteMode(XORPut);
      Graph.rectangle(theRect[1].x, theRect[1].y, theRect[2].x, theRect[3].y);
      SetWriteMode(COPYPut);
   END ELSE
      FillPoly(4, theRect);
   SetFillStyle(oldFillStyle.pattern, oldFillStyle.color);
   SetColor(oldColor);
   ShowCursor;
END;



PROCEDURE Segment.PutPos(x : REAL);
VAR
   xActual : INTEGER;
BEGIN
   Plot(parentPointer^).lockFocus;
   With ParSegment Do
   Begin x0 := x;
         theRect[1].x := mapx(x0);
         theRect[2].x := mapx(x0 + width);
         theRect[3].x := theRect[2].x;
         theRect[4].x := theRect[1].x;
   End;
END;



PROCEDURE Segment.RecalcWidth;
VAR
   xActual : INTEGER;
BEGIN
   Plot(parentPointer^).lockFocus;
   self.CalculateRect;
END;



FUNCTION Segment.MouseInside : BOOLEAN;
VAR
   xpos, ypos, dx : REAL;
   inside         : BOOLEAN;
BEGIN
   MouseInside := FALSE;
   Plot(parentPointer^).lockFocus;
   MousePosn(xpos,ypos, 1, 1, inside);
   dx:= (StopEdge-StartEdge)/(vacuumPts-1);
   With ParSegment Do IF (xpos >= x0) AND (xpos <= x0 + width +2*dx) AND
      inside
   THEN
      MouseInside := TRUE;
END;


PROCEDURE Segment.CalculateRect;
VAR
   xleft, xright, ytop, ybottom : WORD;
BEGIN
   Plot(parentPointer^).lockFocus;
   With  ParSegment Do
     Begin
     xleft := mapx(x0);
     xright := mapx(x0 + width);
     ybottom := mapy(Plot(parentpointer^).Scale.ymin + 0.01);
     ytop    := mapy(Plot(parentPointer^).Scale.ymax - 0.01);
     theRect[1].x := xleft ; theRect[1].y := ybottom;
     theRect[2].x := xright ; theRect[2].y := ybottom;
     theRect[3].x := xright ; theRect[3].y := ytop;
     theRect[4].x := xleft; theRect[4].y := ytop;
     End;
END;

Function Segment.Inspector:Boolean;
VAR
   screen1 : TInputScreen;
   Leave1,Leave2,Leave3,Leave4:  Boolean;
   fieldOffset:                  Integer;
BEGIN
  Inspector:=True;
     CalcGridIndex;
   WITH screen1 DO BEGIN
      Init;
      DefineInputPort(0.3,0.8,0.47,0.90);
      LoadLine('          SEGMENT ATTRIBUTES');
      {LoadLine('   GridIndex ='+ Num2Str(GridIndex,5) ); }
      LoadLine('');
      LoadLine('   X Position          {       } ');
      LoadLine('   Width of Segment    {       } ');
      Case EquationType of
      Schroedinger:
          Begin
          LoadLine('   Potential           {       }10^4');
          fieldOffset:=1;
          End;
      Diffusion:
          Begin
          LoadLine('   Diffusivity         {       }  ');
          fieldOffset:=1;
          End;
        Else
          begin
          LoadLine('   Index of Refraction {       } ');
          LoadLine('   Gain                {       } ');
          fieldOffset:=2;
          End;
      End;{of Case}
      LoadLine('   Color of Segment    {       } ');
      LoadLine('   Able to Drag           #T     ');
      LoadLine('                                 ');
      LoadLine('      [ Ok ]   [Cancel]  [Delete]');
     With ParSegment Do
       Begin
         SetNumber(1, x0);
         SetNumber(2, width);
         if EquationType=Schroedinger
           Then setNumber(3,index)
           Else
             Begin
             SetNumber(3, index);
             SetNumber(4, gain);
             end;
         SetNumber(3+fieldOffset, objColor);
         SetBoolean(4+fieldOffset, DragIt);
       End;

      Leave1:=False;
      Leave2:=False;
      Leave3:=False;
      Leave4:=False;
      Repeat
            AcceptScreen;
            If ((GetNumber(1)>=Startedge) and (GetNumber(1)<=(StopEdge))) then Leave1:=True
            else Announce('The x-position must be within the boundries of the medium.');

            If ((GetNumber(2)>=0) and (GetNumber(2)<=(StopEdge-StartEdge))) then Leave2:=True
            else Announce('Width out of range: 0<= n <=' +Num2Str((StopEdge-StartEdge),4)+'.');
            Case EquationType of
              Schroedinger:
                Begin
                If ((GetNumber(3)>=-100) and (GetNumber(3)<=100)) then Leave3:=True
                  else Announce('Potential our of range: -100 <= V(x) <= 100.');
                leave4:=True;
                End
              else
                begin
                If ((GetNumber(3)>=0.1) and (GetNumber(3)<=10)) then Leave3:=True
                  else Announce('Index out of range: 0.1 <= n <= 10.');
                If ((GetNumber(4)>=-5) and (GetNumber(4)<=5)) then Leave4:=True
                else Announce('Gain out of range: -5 <= n <= 5.');
              end;
            end;
      Until Leave1 and Leave2 and Leave3 and Leave4;
      IF NOT (canceled) THEN
         With ParSegment do
              begin
                   Width:=GetNumber(2);
                   If Width>(StopEdge-StartEdge) Then Width:=(StopEdge-StartEdge);
                   If ((GetNumber(1)+Width)>StopEdge)then PutPos(StopEdge-Width)
                   else
                   If (GetNumber(1)<StartEdge) then PutPos(startEdge)
                   else PutPos(GetNumber(1));
                   if EquationType<>Schroedinger Then gain:=Getnumber(4);
                   objColor := TRUNC(GetNumber(3+fieldOffset));
                   DragIt:=GetBoolean(4+fieldOffset);
                   IF index <> GetNumber(3) THEN index:=GetNumber(3);
              end;

      IF NOT (canceled) THEN
         IF (GetBoolean(7+fieldOffset)) THEN
            BEGIN
                 disabled:=True;  {This will destroy the segment after the inspector exits.}
                 SegmentList.DeleteItem(@Self);
                 inspector:=False;
            END Else inspector:=TRUE;
     Screen1.done;
   END;
   theWave.UpdateVecs;
   theWave.AutoSetTimeInc;
   if (anly1.AnalysisType=Index_Gain)or(anly1.AnalysisType=EnergyDist) then anly1.display;
   if (anly2.AnalysisType=Index_Gain)or(anly2.AnalysisType=EnergyDist)then anly2.display;
END;

Procedure Detector.Clear;

Begin
     Announce('Fix me.');
End;
{========================================================================}
{               Reflector Methods                                           }
{========================================================================}


CONSTRUCTOR Reflector.init( x : REAL; thePlot : plotPtr);
BEGIN
   Basis.Initialize;
   TheBasisType:=Ref;
   With ParReflector Do
     Begin
          x0:=x;       {Position of Reflector inside the medium}
          parentPointer := thePlot;
          With Plot(parentPointer^).Scale do Y0:=abs((1/9)*(Ymax-Ymin));
          ReflectKind:=TheSum;
          Side:=4;
          OutLined:=False;
          CalcGridIndex;
     End; {of With ParReflector}
  CalculateIcon;
END;

DESTRUCTOR Reflector.Done;
BEGIN
     Plot(parentPointer^).lockFocus;
     Remove;
     Basis.Done;
END;
Procedure Reflector.Clear;
Begin
End;
Procedure Reflector.CalculateIcon;

Begin;
      With ParReflector do
           begin;
                 With Plot(parentPointer^).Scale do
                      y0:=((1/9)*(Ymax-Ymin) + Ymin);
                 IconU[1].x:=MapX(x0)-side;
                 IconU[1].y:=MapY(y0)+side;
                 IconU[2].x:=MapX(x0)+Side;
                 IconU[2].y:=MapY(y0)-Side;
                 IconU[3].x:=IconU[1].x;
                 IconU[3].y:=IconU[2].y;
                 IconU[4].x:=IconU[2].x;
                 IconU[4].y:=IconU[1].y;

                 Case ReflectKind of
                      TheLeft:
                                begin
                                     IconU[2].x:=MapX(x0);
                                     IconU[2].y:=MapY(y0);

                                     IconU[4].x:=IconU[2].x;
                                     IconU[4].y:=IconU[2].y;
                                end;
                      TheRight:  begin
                                     IconU[1].x:=MapX(x0);
                                     IconU[1].y:=MapY(y0);

                                     IconU[3].x:=IconU[1].x;
                                     IconU[3].y:=IconU[1].y;
                                end;
                      TheSum: ;
                 End;

                 With Plot(parentPointer^).Scale do
                      y0:=((8/9)*(Ymax-Ymin) + Ymin);
                 IconD[1].x:=MapX(x0)-side;
                 IconD[1].y:=MapY(y0)+side;
                 IconD[2].x:=MapX(x0)+Side;
                 IconD[2].y:=MapY(y0)-Side;
                 IconD[3].x:=IconD[1].x;
                 IconD[3].y:=IconD[2].y;
                 IconD[4].x:=IconD[2].x;
                 IconD[4].y:=IconD[1].y;

                 Case ReflectKind of
                      TheLeft:
                                begin
                                     IconD[2].x:=MapX(x0);
                                     IconD[2].y:=MapY(y0);

                                     IconD[4].x:=IconD[2].x;
                                     IconD[4].y:=IconD[2].y;
                                end;
                      TheRight:  begin
                                     IconD[1].x:=MapX(x0);
                                     IconD[1].y:=MapY(y0);

                                     IconD[3].x:=IconD[1].x;
                                     IconD[3].y:=IconD[1].y;
                                end;
                      TheSum: ;
                 End;

           end;
End;



PROCEDURE Reflector.Display;
VAR
   oldColor : WORD;

BEGIN
     HideCursor;
     Plot(parentPointer^).lockFocus;
     oldColor := GetColor;
     CalculateIcon;
     With ParReflector do
     With Plot(parentpointer^).Scale Do
     If OutLined then
        begin
             SetWriteMode(XORPut);
             Line(MapX(x0),MapY(Ymin),MapX(x0),MapY(Ymax));
             If not(Running) then
                begin;
                      Line(IconU[1].x,IconU[1].y,IconU[2].x,IconU[2].y);
                      Line(IconU[3].x,IconU[3].y,IconU[4].x,IconU[4].y);

                      Line(IconD[1].x,IconD[1].y,IconD[2].x,IconD[2].y);
                      Line(IconD[3].x,IconD[3].y,IconD[4].x,IconD[4].y);
                end;
             SetWriteMode(CopyPut);
        end
     else
       begin
            SetColor(ReflectorColour);
            Line(MapX(x0),MapY(Ymin),MapX(x0),MapY(Ymax));
            If not(Running) then
               begin
                    SetColor(White);
                    Line(IconU[1].x,IconU[1].y,IconU[2].x,IconU[2].y);
                    Line(IconU[3].x,IconU[3].y,IconU[4].x,IconU[4].y);

                    Line(IconD[1].x,IconD[1].y,IconD[2].x,IconD[2].y);
                    Line(IconD[3].x,IconD[3].y,IconD[4].x,IconD[4].y);
               end;
     end;
     SetColor(oldColor);
     showCursor;
END;

PROCEDURE Reflector.Remove;
VAR
   oldColor : WORD;

BEGIN
     HideCursor;
   Plot(parentpointer^).lockFocus;
   oldColor := GetColor;
   SetColor(graphBackColor);
   With ParReflector Do
   With Plot(parentpointer^).Scale Do
   IF outlined THEN BEGIN
      SetColor(oldColor);
      SetWriteMode(XORput);
      Line(MapX(x0),MapY(yMin),MapX(x0),MapY(yMax));
      If not(Running) then
         begin;
               Line(IconU[1].x,IconU[1].y,IconU[2].x,IconU[2].y);
               Line(IconU[3].x,IconU[3].y,IconU[4].x,IconU[4].y);

               Line(IconD[1].x,IconD[1].y,IconD[2].x,IconD[2].y);
               Line(IconD[3].x,IconD[3].y,IconD[4].x,IconD[4].y);
         end;
      SetWriteMode(COPYPut);
   END
   ELSE
       begin;
             Line(MapX(x0),MapY(yMin),MapX(x0),MapY(yMax));
             If not(Running) then
                begin;
                      Line(IconU[1].x,IconU[1].y,IconU[2].x,IconU[2].y);
                      Line(IconU[3].x,IconU[3].y,IconU[4].x,IconU[4].y);

                      Line(IconD[1].x,IconD[1].y,IconD[2].x,IconD[2].y);
                      Line(IconD[3].x,IconD[3].y,IconD[4].x,IconD[4].y);
                end;
       end;
   SetColor(oldColor);
   showCursor;
END;

FUNCTION Reflector.MouseInside : BOOLEAN;
VAR
   xpos, ypos : REAL;
   y1, y2     : Real;
   inside     : BOOLEAN;
BEGIN
   MouseInside := False;
   Plot(parentPointer^).lockFocus;
   MousePosn(xpos, ypos, 1, 1, inside);
   With Plot(ParentPointer^).Scale do
        begin
             y1:=(1/9)*(Ymax-Ymin)+Ymin;
             y2:=(8/9)*(Ymax-Ymin)+Ymin;
        end;
   With ParReflector Do
        IF (mapx(xpos) >= mapx(x0) - side) AND
           (mapx(xpos) <= mapx(x0) + side) AND
           (((mapy(ypos) >= mapy(y1) - side) AND (mapy(ypos) <= mapy(y1) + side)) OR
            ((mapy(ypos) >= mapy(y2) - side) AND (mapy(ypos) <= mapy(y2) + side))) AND
           Inside
   THEN
       MouseInside := True;
END;


Function Reflector.Inspector:Boolean;
VAR
   screen1 : TInputScreen;
   Ok,Kill,
   Leave1:  Boolean;

Begin;
   Inspector:=True;
   WITH screen1 DO BEGIN
      Init;
      DefineInputPort(0.30,0.72,0.39,0.90);
      LoadLine('     REFLECTOR ATTRIBUTES       ');
      LoadLine('                                  ');
      LoadLine('    #1 Reflect Left-going      ');
      LoadLine('    #1 Reflect Right-going     ');
      LoadLine('    #1 Reflect Both Waves      ');
      LoadLine('                                  ');
      LoadLine('  X Position =   {       }  ');
      LoadLine('  Able to Drag         #T      ');
      LoadLine('                                  ');
      LoadLine(' [  Ok  ] [Cancel] [Delete]   ');

      Case ParReflector.ReflectKind of
           TheLeft:  SetRadioButton('1',1);
           TheRight: SetRadioButton('1',2);
           TheSum:   SetRadioButton('1',3);
      End;
     With ParReflector Do
       Begin
            SetNumber(4, x0);
            SetBoolean(5,DragIt);
       End;

      Leave1:=False;
      Repeat
            AcceptScreen;
            If ((GetNumber(4)>=Startedge) and (GetNumber(4)<=StopEdge)) then
               Leave1:=True
            else
                Announce('The x-position must be within the boundries of the medium.');
      Until Leave1;

      Ok:=GetBoolean(6);
      Kill:=GetBoolean(8);

      If Ok then
         With ParReflector do
              begin;
                    Case GetRadioButton('1') of
                         1: ReflectKind:=TheLeft;
                         2: ReflectKind:=TheRight;
                         3: ReflectKind:=TheSum;
                    End;


                    If (GetNumber(4)>StopEdge)then PutPos(StopEdge)
                    else
                    If (GetNumber(4)<StartEdge) then PutPos(startEdge)
                    else PutPos(GetNumber(4));
                    DragIt:=GetBoolean(5);
                    CalcGridIndex;
              end;
              If Kill then
                 begin;
                       ReflectorList.DeleteItem(@Self);
                       Disabled:=True;
                       Inspector:=FALSE;
                 end else Inspector:=True;
      Screen1.Done;
     end;
End;

Procedure Reflector.AddWaveToDVec;
var Temp: Real;
Begin
    With ParReflector Do
    Case ReflectKind of
       theRight:
         begin
         yVec.VecPtr^[gridIndex]:=0;
         yPrevVec.VecPtr^[gridIndex]:=-yVec.Vecptr^[gridIndex];
         End;
       theLeft:
         begin
         yPrevVec.VecPtr^[gridIndex]:=0;
         yVec.VecPtr^[gridIndex]:=-yPrevVec.Vecptr^[gridIndex];
         End;
       theSum:
           Begin
           temp:=yVec.vecPtr^[gridIndex];
           yVec.VecPtr^[gridIndex]:=-yPrevVec.VecPtr^[gridIndex];
           yPrevVec.VecPtr^[gridIndex]:=-temp;
           end;
    End;{of Case}
End;
end.