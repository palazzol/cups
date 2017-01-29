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

{Version Number = 3.3        4-29-94}
{$R-}
Program Wave;
USES Wav_Base,Wav_Plot,Wav_Anly,Wav_Gbl,Wav_Graf,Wav_Lst,Wav_SS,Wav_Prop,
     Graph, Crt, CUPSmupp, CUPS, CUPSgui, CUPSgrph;

Const
     CurrentVersion=3.3;
     FileHeader = 'CUPS:Wave Ver 3.3';
     Str0 = 'EquationType';
     Str1 = 'Time And Dt';
     Str2 = 'Animation Speed';
     Str3 = 'vacuumPts, Start, Stop';
     Str4 = 'Left Boundary';
     Str5 = 'Right Boundary';
     Str6 = 'Sources';
     Str7 = 'Segments';
     Str8 = 'Detectors';
     str9 = 'Reflectors';
     Str10 = 'Analysis Type';
     Str11 = 'Wave Data:NumPts, YMin, YMax and Data Vectors';
     str12 = 'Bye.';


VAR
   version:     Real;
   ItemClicked: BasePtr;
   QuitFlag :   boolean;

{see Wav_Gbl for global variables.
   See Wav_Plot.Pas for
       SourceList:          List;
       SegmentList:         List;
       DetectorList:        List;
       ReflectorList:       List;}

{----------------------------------------------------------------------------}

Procedure Initialize;

Begin;
      CUPSInit;
      VariableInit;
      AdjustForBoundry;
      SourceList.Init;
      DetectorList.Init;
      SegmentList.Init;
      ReflectorList.Init;
      AtOutline:=False;
      DefaultPath:='';

      TheWave.Initialize(1, 0.1, 0.1, 0.80, 0.38);

      Anly1.Initialize(2, 0.1, 0.55, 0.35, 0.35);
      Anly2.Initialize(3, 0.55, 0.55, 0.35, 0.35);
      Anly1.Display;
      Anly1.LockFocus;

      Anly2.Display;
      Anly2.LockFocus;

      TheWave.Display;
      YInitVec.Equate(1, YVec);
      YPrevInitVec.Equate(1, yPrevVec);
      ShowDensity;


      WITH MainMenu DO
           begin;
                 Init;
                 Column(1, 'File');
                 Row(1, 1, 'About CUPS');
                 Row(1, 2, 'About The Program');
                 Row(1, 3, 'Configuration');
                 Row(1, 4, '-----------------');
                 Row(1, 5, 'Open ...');
                 Row(1, 6, 'Merge...');
                 Row(1, 7, 'Save ...');
                 Row(1, 8, 'Save As');
                 Row(1, 9, '-----------------');
                 Row(1,10, 'Exit');
                 RowActivate(1,4,FALSE);
                 RowActivate(1,9,FALSE);
                 Column(2, 'Parameters');
                 Row(2, 1, 'Space Parameters');
                 Row(2, 2, 'Time Parameters');
                 Row(2, 3, 'Scale Wave');
                 Row(2, 4, 'Scale Graph1');
                 Row(2, 5, 'Scale Graph2');
                 Row(2, 6, '----Wave Equations----');
                 RowActivate(2,6,FALSE);
                 Row(2, 7, 'Electromagnetic');
                 RowChecked(2,7,True);
                 Row(2, 8, 'Klein-Gordon');
                 Row(2, 9, 'sine-Gordon');
                 Row(2, 10, 'phi-four');
                 Row(2, 11, 'double sine-Gordon');
                 Row(2, 12, '----------------------');
                 RowActivate(2,12,FALSE);
                 Row(2, 13, 'Diffusion');
                 Row(2, 14, '-----------------------');
                 RowActivate(2,14,FALSE);
                 Row(2, 15, 'Schroedinger');
                 {RowActivate(2,15,FALSE); }

                 Column(3, 'Init');
                 Row(3, 1, 'Zero');
                 Row(3,2, 'Random');
                 Row(3, 3, 'Sine');
                 Row(3, 4, 'Gaussian');
                 Row(3, 5, 'Modulated Gaussian');
                 Row(3,6, 'Pulse');
                 Row(3, 7, 'Symmetric Pulse');
                 Row(3, 8, 'Modes');
                 Row(3, 9, 'User Defined Function');
                 autochecking(3,3);

                 Column(4, 'Graph-1');
                 Row(4, 1, 'Disable Graph');
                 Row(4, 2, 'Segment: n(x), V(x) or K(x)');
                 Row(4, 3, 'Space Integrated I(t)=Y*Y');
                 Row(4, 4, 'Y(x,t)- Contour');
                 Row(4, 5, 'Y(x,t)- 3D');
                 Row(4, 6, 'Fourier Analysis of Y(x)');
                 Row(4, 7, 'Fourier Analysis of Y(t)');
                 Row(4, 8, 'Detector Readings');
                 Row(4, 9, 'Source Power:P(t)');
                 Row(4, 10, 'I(x,t)=Y(x,t)*Y(x,t)');
                 Row(4, 11, '---------------------------');
                 Row(4, 12, 'Refresh Graph');
                 rowactivate(4,11,false);
                 rowChecked(4,10,true);

                 Column(5, 'Graph-2');
                 Row(5, 1, 'Disable Graph');
                 Row(5, 2, 'Segment: n(x), V(x) or K(x)');
                 Row(5, 3, 'Space Integrated I(t)=Y*Y');
                 Row(5, 4, 'Y(x,t)- Contour');
                 Row(5, 5, 'Y(x,t)- 3D');
                 Row(5, 6, 'Fourier Analysis of Y(x)');
                 Row(5, 7, 'Fourier Analysis of Y(t)');
                 Row(5, 8, 'Detector Readings');
                 Row(5, 9, 'Source Power:P(t)');
                 Row(5, 10, 'I(x,t)=Y(x,t)*Y(x,t)');
                 Row(5, 11, '---------------------------');
                 Row(5, 12, 'Refresh Graph');
                 rowactivate(5,11,false);
                 rowChecked(5,4,true);

                 Column(6, 'Medium');
                 Row(6,1, 'Boundary');
                 Row(6,2, 'Saturation');
                 Row(6,3, 'Parse:n, V or K');
                 Row(6,4, '------------------');
                 RowActivate(6,4,FALSE);
                 Row(6,5, 'Inspect Reflectors');
                 Row(6,6, 'Remove  Reflectors');
                 Row(6,7, '------------------');
                 RowActivate(6,7,FALSE);
                 Row(6,8, 'Inspect Sources');
                 Row(6,9, 'Remove  Sources');
                 Row(6,10, '------------------');
                 RowActivate(6,10,FALSE);
                 Row(6,11, 'Inspect Segments');
                 Row(6,12, 'Remove  Segments');
                 Row(6,13, '------------------');
                 RowActivate(6,13,FALSE);
                 Row(6,14, 'Inspect Detectors');
                 Row(6,15, 'Remove  Detectors');
                 Column(7, 'Pref');
                 Row(7, 1, 'Display');
                 Row(7, 2, 'Files');
                 Row(7, 3, 'Speed');
                 Row(7, 4, 'Demo');
           end;
      Anly1.PlotTitle.Remove;
      Anly1.PlotTitle.SetStr('I(x,t)=Y(x,t)*Y(x,t)');
      Anly1.AnalysisType:=EnergyDist;
      Anly1.RefreshAnalysisPlot(True,StartEdge,0,StopEdge,1.5);
      Anly1.AnalysisSelection(@TheWave);
      Anly1.AnalysisCounter:=1;
      Anly1.DvecCompress:=1;
      Anly1.DvecCompressCounter:=1;

      Anly2.PlotTitle.Remove;
      Anly2.PlotTitle.SetStr('Contour of Y(x,t)');
      Anly2.AnalysisType:=YvsTdensity;
      Anly2.AnalysisSelection(@TheWave);
      Anly2.RefreshAnalysisPlot(True,startEdge,Time,stopEdge,Time+1);
      Anly2.AnalysisCounter:=1;
      Anly2.DvecCompress:=1;
      Anly2.DvecCompressCounter:=1;
      Anly2.Replot;
      SetStopKeys;
      SetRunKeys;
End;
{----------------------------------------------------------------------------}
Function MouseRegion: Byte;

var
   x,y:        Real;
   Inside:      Boolean;
Begin;
      DefineScale( 5, 0, 1, 0, 1 );
      SelectScale(  5 );
      SelectViewport( 0 );
      MousePosn( x, y, 0, 5, Inside);

      If TheWave.Expanded then
         begin;
               If ( x >= 0    ) and ( y >= 0    ) and
                  ( x <= 1)  and ( y <= 0.05 )
                  then MouseRegion:=1;
               If ( x >= 0 ) and ( y >= 0.05 ) and
                  ( x <= 1 ) and ( y <= 0.90 )
                  then MouseRegion:=2;
               If ( x >= 0 ) and ( y >= 0.90 ) and
                  ( x <= 1    ) and ( y <= 1    )
                  then MouseRegion:=5;
         end;

      If Anly1.Expanded then
         begin;
               If ( x >= 0    ) and ( y >= 0    ) and
                  ( x <= 1)  and ( y <= 0.05 )
                  then MouseRegion:=1;
               If ( x >= 0 ) and ( y >= 0.05 ) and
                  ( x <= 1 ) and ( y <= 0.90 )
                  then MouseRegion:=3;
               If ( x >= 0 ) and ( y >= 0.90 ) and
                  ( x <= 1    ) and ( y <= 1    )
                  then MouseRegion:=5;
         end;

      If Anly2.Expanded then
         begin;
               If ( x >= 0    ) and ( y >= 0    ) and
                  ( x <= 1)  and ( y <= 0.05 )
                  then MouseRegion:=1;
               If ( x >= 0 ) and ( y >= 0.05 ) and
                  ( x <= 1 ) and ( y <= 0.90 )
                  then MouseRegion:=4;
               If ( x >= 0 ) and ( y >= 0.90 ) and
                  ( x <= 1    ) and ( y <= 1    )
                  then MouseRegion:=5;
         end;

      If not(TheWave.Expanded) and not(Anly1.Expanded) and not(Anly2.Expanded) then
         begin;
               If ( x >= 0    ) and ( y >= 0    ) and
                  ( x <= 1)  and ( y <= 0.05 )
                  then MouseRegion:=1;
               If ( x >= 0 ) and ( y >= 0.05 ) and
                  ( x <= 1 ) and ( y <= 0.50 )
                  then MouseRegion:=2;
               If ( x >= 0 ) and ( y >= 0.50 ) and
                  ( x <= 0.50 ) and ( y <= 0.90 )
                  then MouseRegion:=3;
               If ( x >= 0.50 ) and ( y >= 0.50 ) and
                  ( x <= 1 ) and ( y <= 0.90 )
                  then MouseRegion:=4;
               If ( x >= 0 ) and ( y >= 0.90 ) and
                  ( x <= 1    ) and ( y <= 1    )
                  then MouseRegion:=5;
         end;
End;
{----------------------------------------------------------------------------}
Procedure ResetIndexPlot;
Begin;
  if  (anly1.AnalysisType=Index_Gain) or (anly1.AnalysisType=EnergyDist)  then anly1.display;
  if  (anly2.AnalysisType=Index_Gain) or (anly2.AnalysisType=EnergyDist) then anly2.display;
End;

Procedure ResetSourcePlot;
Begin;
  if  anly1.AnalysisType=PvsT then
    begin Anly1.disable; anly1.sourceClicked:=nil; end;
  if  anly2.AnalysisType=PvsT then
    Begin Anly2.disable; anly2.sourceClicked:=nil; end;
End;

Procedure ResetDetectorPlot;
Begin;
  if (anly1.AnalysisType=FFTtime) or (Anly1.AnalysisType=YvsT) then
    begin Anly1.Disable; anly1.detectorClicked:=nil; end;
  if (anly2.AnalysisType=FFTtime) or (Anly2.AnalysisType=YvsT)  then
    begin Anly2.Disable; anly2.detectorclicked:=nil; end;
End;

Procedure ResetAnly(ThePlot: AnlyPlotPtr; Must: Boolean);
Begin;
      With ThePlot^ do
           begin
                If Freeze Then Exit;
                If not(Must) And not(GeneralType=ThreeD) AND Not(AnalysisType=FFTtime)
                  then Exit;
                AnalysisCounter:=1;
                InitialTime:=Time;
                If (GeneralType=ThreeD) then
                           begin
                                TheMatrix.Fill(0);
                                OldMatrix.Fill(0);
                                if isVisible and (analysistype<>Disabled) then
                                  Comment(Xo+Width/2-0.15,0.8,' Data has been cleared. ');
                           end;
                If (AnalysisType=FFTtime) And isVisible and (analysistype<>Disabled) then
                                  Comment(Xo+Width/2-0.15,0.8,' Data has been cleared. ');
                TimeCompress:=1;
                SkipCounter:=1;
           End;
End;
{----------------------------------------------------------------------------}
Procedure SpeedPref;
Var
   SpeedScrn:        TInputScreen;
   Ok, Cancel:          Boolean;
   oldSpeed:            Integer;

Begin;
oldSpeed:=animationSpeed;
      With SpeedScrn do
           begin;
                 Init;
                 DefineInputPort( 0.25, 0.75, 0.20 ,0.92 );

                 LoadLine('        Animation Parameters          ');
                 LoadLine('______________________________________');
                 LoadLine('    Mouse ON during animation?   #F');
                 LoadLine('______________________________________');
                 LoadLine(' The following parameters can be used');
                 LoadLine(' to speedup or slowdown the program   ');
                 LoadLine(' without changing the accuracy of the ');
                 LoadLine(' calculation.  (Parameter Range 1..99)');
                 LoadLine('');
                 LoadLine('#1 Draw after every {   } calculation.');
                 Loadline('#1 Pause {   }10 ms after calculation.');
                 LoadLine('');
                 LoadLine(' These parameters can also be changed ');
                 LoadLine(' while the program is running using   ');
                 LoadLine(' the speedup and slowdown hot keys.   ');
                 LoadLine('                                      ');
                 LoadLine('   [  Ok  ]   [Cancel]    [ Help ]    ');

                 If AnimationSpeed<=0 then SetRadioButton('1',1)
                    else SetRadioButton('1',2);
                 if MouseWhileRunning Then SetBoolean(1,TRUE)Else SetBoolean(1,FALSE);
                 SetNumber(3, Abs(AnimationSpeed)+1);
                 SetNumber(5, Abs(AnimationSpeed));

                 SetHelpFile('Wave.Hlp', 'Speed Help');
                 Repeat
                   AcceptScreen;
                   Ok:=GetBoolean(6);
                   Cancel:=GetBoolean(7);
                   if Cancel Then
                     Begin
                     SpeedScrn.Done;
                     Exit;
                     End;
                   If OK AND NOT((GetNumber(3)>0 ) AND (GetNumber(5)>=0) AND
                     (GetNumber(3)<=99 ) AND (GetNumber(5)<=99))  THEN
                     Announce('Number Out of Range. RE-EDIT.');
                 Until ((GetNumber(3)>0 ) AND (GetNumber(5)>=0) AND
                        (GetNumber(3)<=99 ) AND (GetNumber(5)<=99));
                 If OK Then mouseWhileRunning:=GetBoolean(1);
                 If Ok then
                          Case GetRadioButton('1') of
                          1: begin;
                             AnimationSpeed:=-Round(GetNumber(3))+1;
                             end;
                          2: begin;
                             AnimationSpeed:=Round(GetNumber(5));
                             end;
                          End;{of Case}
                 Done;
           end;
if AnimationSpeed<>oldSpeed Then
  Begin
  ResetAnly(@Anly1,False);
  ResetAnly(@Anly2,False);
  end;
End;

Procedure DemoPref;
Var
   InptPAtmScrn:        TInputScreen;
   Ok, Cancel:          Boolean;

Begin;
      With InptPAtmScrn do
           begin;
                 Init;
                 DefineInputPort( 0.25, 0.75, 0.3 ,0.90 );

                 LoadLine('              Demo Mode            ');
                 LoadLine('');
                 LoadLine(' Selecting DEMO MODE will reset the');
                 LoadLine(' program to time = 0 whenever the ');
                 LoadLine(' time is greater than DemoTime.   ');
                 LoadLine('        0 >= Time > DemoTime        ');
                 LoadLine('');
                 LoadLine('Enable Demo Mode:  #F');
                 LoadLine('        DemoTime:  {     }');
                 LoadLine('');
                 LoadLine('<ESC> will alow you to exit Demo Mode.');
                 LoadLine('                                      ');
                 LoadLine('   [  Ok  ]   [Cancel]    [ Help ]    ');
                 SetHelpFile('Wave.Hlp', 'Demo Help');
                 SetBoolean(1,DemoMode);
                 SetNumber(2,DemoTime);
                 AcceptScreen;
                 Ok:=GetBoolean(3);
                 Cancel:=GetBoolean(4);
                 If Ok then
                   Begin
                   DemoMode:=GetBoolean(1);
                   if DemoMode Then
                     Begin
                     DemoTime:=Abs(GetNumber(2));
                     dt:=Abs(dt);
                     End;
                   End;
                 Done;
           end;
End;

{----------------------------------------------------------------------------}
Procedure ParseIndex;

function DrawParsedFunction : boolean;

Var
   i:           Integer;
   Index:       Real;
   oldColor,OldBack:Word;
   bad : boolean;
Begin;
      Parser.Init;
      Parser.Parse('x',' ',' ',' ',IndexParserStr);
      HaltIfError := false;
      ErrorFound  := False;
      bad := false;
      i:=1;
      while (i<=xSGVec.Length) and (not ErrorFound) and (not bad) do
      begin
            Index:=Parser.F(xSGVec.Value(i),0,0,0)+IndexSGVec.Value(i);
            If EquationType in [EandM,Diffusion] Then Index:=Abs(Index);
            If EquationType=EandM then bad := (index<1);
            TempSGvec.Put(i,Index);
            inc(i);
      end;
      DrawParsedFunction := (not ErrorFound) and (not bad);
      if bad then
       announce('Index of refraction (n=c/v) can not be smaller then 1.') else
      if ErrorFound then announce('Parser Error - Try Egain')
      else
      begin
         OldColor:=GetColor;
         OldBack:=GraphBackColor;
         DefineviewPort(5,0.13,0.87,0.25,0.65);
         GraphBackColor:=Blue;
         OpenViewPort(5);
         DefineViewPort(6, 0.2, 0.83, 0.31, 0.60);
         GraphBackColor:=Black;
         OpenViewPort(6);
         AutoScaling(2,xSGVec,TempSGvec);
         SelectScale(2);
         SetColor(White);
         PlotDvectors(xSGVec,TempSGvec,1,xSGVec.Length);
         SetColor(OldColor);
         GraphBackColor:=OldBack;
      end;
      HaltIfError := True;
      ErrorFound  := False;
End;

Var
   Ok,Cancel,View,
   Leave:       Boolean;
   ParseScreen:  TinputScreen;
   oldStr,name:    String;
Begin
  {The following if statement fixes a bug in the CUPS utilities}
  if Not(EquationType in [EandM, Schroedinger,Diffusion]) Then
      Begin Announce('Only homogeneous medium are supported for the current PDE.'); exit; end;
      oldStr:=IndexParserStr;
         Case EquationType of
          EandM:        name := 'n(x)';
          Schroedinger: name := 'V(x)';
          diffusion:    name := 'K(x)';
         end;
         with ParseScreen do
          begin
               Init;
               DefineInputPort(0.13,0.87,0.65,0.95);
               LoadLine('Parser values for '+name+' are added to initial segment');
               LoadLine('values. No stability checking is done.');
               LoadLine('');
               Case EquationType of
                 EandM:LoadLine('Input the index of refraction as a function of x.');
                 Schroedinger:LoadLine('Input the potential, V(x), in units of 10^4.');
                 diffusion:LoadLine('Input the diffusivity as a function of x.');
               end;{of Case}
               LoadLine(name+'="                                                 "');
               LoadLine('');
               LoadLine('     [  Ok  ]    [Cancel]    [ View ]    [ Help ]    ');
               SetHelpFile('Wave.Hlp','MediumParser Help');
               SetString(1,IndexParserStr);
               Leave:=False;
               IndexParserStr := '0';
               DrawParsedFunction;
               Repeat
                     Accept;
                     OK:=GetBoolean(2);  Cancel:=GetBoolean(3); View:=GetBoolean(4);
                     if Cancel Then begin
                        IndexParserStr:=oldStr;
                        leave:=true;
                     end Else begin
                        IndexParserStr:=GetString(1);
                        if IndexParserStr='' then IndexParserStr:='0';
                        Parser.Init;
                        If NOT Parser.Parse('x',' ',' ',' ',IndexParserStr)
                        Then Announce('This function is not acceptable.')
                        Else leave := DrawParsedFunction and Ok;
                     end;
               Until Leave;
               Done;{with ParseScreen}
          end;

if IndexParserStr='0' then IndexParserStr:='';
If OK then
  Begin
  theWave.UpdateVecs;
  theWave.AutoSetTimeInc;
  end;
ClearMuppetPort;
MainMenu.Display;
SetRunKeys;
SetStopKeys;
StopKeys.Display;
Update;
End;

{----------------------------------------------------------------------------}
Procedure BoundaryPref;

Var
   InptBoundScrn:       TInputScreen;
   Ok, Cancel:          Boolean;
   MaxNum,k:            Integer;
   SaveConstants:       Boolean;
   oldRightB,OldLeftB:  Boundry;

Begin;
  oldRightB:=rightB;
  oldLeftB:=leftB;
  SaveConstants:=FALSE;
  if EquationType in [KleinGordon,SineGordon,phiFour,doubleSineGordon,diffusion] then
    if (leftB=matched) or (rightB=Matched) Then
      Begin
      Announce('Boundary conditions reset to fixed this type of equation.');
      leftB:=Fixed; rightB:=Fixed;
      end;
      With InptBoundScrn do
           begin;
                 Init;
                 DefineInputPort( 0.31, 0.67, 0.40 ,0.90 );

                 LoadLine('    Boundary Conditions      ');
                 LoadLine('  ');
                 LoadLine('  Periodic will override    ');
                 LoadLine('  fixed or absorbing.      ');
                 LoadLine('    Periodic       #F       ');
                 LoadLine('                             ');
                 LoadLine('               Left   Right  ');
                 LoadLine('    Fixed       #2     #1    ');
                 LoadLine('    Absorbing   #2     #1    ');
                 LoadLine('');
                 LoadLine(' [  Ok  ] [Cancel] [ Help ]  ');
                 SetHelpFile('Wave.Hlp', 'Boundary Help');

                 Case RightB of
                      Fixed:      SetRadioButton('1',1);
                      Matched:    SetRadioButton('1',2);
                      Periodic: SetBoolean(1,True);
                 End;

                 Case LeftB of
                      Fixed:      SetRadioButton('2',1);
                      Matched:    SetRadioButton('2',2);
                      Periodic: SetBoolean(1,True);
                 End;
                 AcceptScreen;
                 Ok:=GetBoolean(6);
                 Cancel:=GetBoolean(7);
                 If Ok then
                 Begin
                 Case GetRadioButton('1') of
                          1: RightB:=Fixed;
                          2: RightB:=Matched;
                          End;

                 Case GetRadioButton('2') of
                          1: LeftB:=Fixed;
                          2: LeftB:=Matched;
                          End;

                 If GetBoolean(1) then
                             begin;
                                   RightB:=Periodic;
                                   LeftB:=Periodic;
                             end;
                 End;
                 Done;
           end;{of  With InptBoundScrn }
  if EquationType in [KleinGordon,SineGordon,phiFour,doubleSineGordon,diffusion] then
    if (leftB=matched) or (rightB=Matched) Then
      Begin
      Announce('Absorbing boundary conditions not implemented for this type of equation.');
      leftB:=Fixed; rightB:=Fixed;
      end;
  if (oldRightB<>rightB) or (oldLeftB<>leftB) Then
    Begin
      If (anly1.AnalysisType=FFTpos) And Not(Anly1.Freeze)
        Then Announce('CAUTION: Check your FFT basis functions using blue button.');
      If(anly2.AnalysisType=FFTpos) AND Not(Anly2.Freeze)
        Then Announce('CAUTION: Check FFT your basis functions using blue button.');
    End;
  AdjustForBoundry;
End;
{----------------------------------------------------------------------------}
Procedure SaturationPref;

Var
   InptSatScrn:       TInputScreen;
   Ok, Cancel:          Boolean;

Begin;
 With InptSatScrn do
           begin;
                 Init;
                 DefineInputPort( 0.29, 0.71, 0.40 ,0.80 );

                 LoadLine('    Gain Saturation      ');
                 LoadLine('  ');
                 LoadLine('Saturation On.   #F  ');
                 LoadLine('');
                 LoadLine('Saturation will limit the gain');
                 LoadLine('inside a segment when the total');
                 LoadLine('energy approaches 10.');
                 LoadLine('');
                 LoadLine(' [  Ok  ] [Cancel] [ Help ]  ');
                 SetHelpFile('Wave.Hlp', 'Saturation Help');
                 SetBoolean(1,TheWave.SaturationOn);
                 AcceptScreen;
                 Ok:=GetBoolean(2);
                 Cancel:=GetBoolean(3);
                 If Ok then TheWave.SaturationOn:=GetBoolean(1);
                 Done;
           end;{of  With InptBoundScrn }
End;
{----------------------------------------------------------------------------}
PROCEDURE FilesPref;
VAR
  theInputPort:  TInputScreen;
  fHandle:       TEXT;
  msg:           string;
  i:             INTEGER;

BEGIN
   WITH theInputPort DO BEGIN
      Init;
      DefineInputPort(0.19, 0.81, 0.45, 0.85);
      LoadLine('                File Preferences               ');
      LoadLine('-----------------------------------------------');
      LoadLine('Drive and directory path for data files.       ');
      LoadLine('New path="                                    "');
      LoadLine('-----------------------------------------------');
      LoadLine('Drive and directory path for overflow files.   ');
      LoadLine('New path="                                    "');
      LoadLine('                                               ');
      LoadLine('    [  Ok  ]                       [Cancel]    ');
      SetString(1,DefaultPath);
      SetString(2,TempDrive);

      AcceptScreen;

      If Not(Canceled) THEN
        BEGIN
      {$I-}
          ChDir(GetString(1));
          IF IOResult = 0     {See if ChDir worked!}
            THEN defaultPath:=GetString(1)
            ELSE Announce('Cannot Find new data directory: '+GetString(1));
          IF Length(GetString(2))>70
            THEN Announce('TEMP directory path too long.');
          Assign(fHandle,GetSTring(2)+'NEWFILE.$$$');
          Rewrite(fHandle); Writeln( fHandle,'TEST OF OVERFLOW DIRECTORY');
          IF IOResult = 0 THEN tempDrive:=GetString(2)
             ELSE Announce('Cannot open TEMP directory: '+GetString(2));
          CLOSE(fHandle);
          ERASE(FHandle);
        {$IFDEF DEBUG} {$I+} {$ELSE} {$I-} {$ENDIF}
         END;
      done;
   END;
END;
{----------------------------------------------------------------------------}
Procedure Save( SaveAs: Boolean );

Var
   SaveInputPort:       TInputScreen;
   fHandle:             Text;
   msg:                 String;
   Leave,Ok,Cancel:     Boolean;
   Store1,Store2,i:     Integer;

Function Continue( errStr: String ): Boolean;

Var
   Ok,Cancel:           Boolean;
   ConInputPort:        TInputScreen;

Begin;
      With ConInputPort do
           begin;
                 Init;
                 DefineInputPort(0.20,0.80,0.70,0.80);
                 LoadLine(' '+errStr );
                 LoadLine('     [  Ok  ]   [Cancel]');
                 AcceptScreen;
                 Ok:=GetBoolean(1);
                 Cancel:=GetBoolean(2);
                 If Ok then Continue:=True;
                 If Cancel then Continue:=False;
                 Done;
           end;
End;

Begin;
      If SaveAs then
      With SaveInputPort do
           begin;
                 Init;
                 DefineInputPOrt(0.20,0.80,0.40,0.90);
                 LoadLine('Save Program Data.  Use a *.WAV extension.     ');
                 LoadLine('File Name = "                                 "');
                 LoadLine('-----------------------------------------------');
                 LoadLine('Comments:                                      ');
                 LoadLine('"                                             "');
                 LoadLine('"                                             "');
                 LoadLine('-----------------------------------------------');
                 LOadLine('   The directory can be changed using the      ');
                 LoadLine('       Pref ->Files  menu option.              ');
                 LoadLine('-----------------------------------------------');
                 LoadLine('      [  Ok  ]      [Cancel]      [ Help ]');
                 SetString( 1, SaveStr );
                 SetHelpFile('Wave.Hlp','Save Help');
                 SetString(2,Comment1);
                 SetString(3,Comment2);
                 Leave:=False;
                 Repeat
                        AcceptScreen;
                        Ok:=GetBoolean(4);
                        If not(Ok) then exit;
                        If (GetString(1)='')
                           then Announce('A string is required.')
                        else
                            Leave:=True;
                 Until Leave;
                 SaveStr:=GetString(1);
                 Comment1:=GetString(2);
                 Comment2:=GetString(3);
                 Done;
           end;
      {$I-}
      Assign(Fhandle,SaveStr);
      Reset(Fhandle);
      If IOResult=0 then
         begin;
               msg:=SaveStr+' exists. Overwrite?';
               Beep;
               If not(Continue(msg)) then
                  begin;
                        Close(Fhandle);
                        exit;
                  end;
               Close(Fhandle);
         end;
      Assign(Fhandle,SaveStr);
      Rewrite(Fhandle);
      If not(IOResult=0) then
         begin;
               Announce('Path or file name error. File cannot be saved.');
               exit;
         end;
      theWave.UpdateVecs;
      WriteLn(Fhandle,FileHeader);

      If IOResult=105 then
         begin;
               Announce('You cannot write to that drive or directory.');
               Exit;
         end;
      {$IFDEF DEBUG} {$I+} {$ELSE} {$I-} {$ENDIF}
      Writeln(FHandle,str0);
      Case EquationType of
        EandM:Writeln(FHandle,1);
        Diffusion:Writeln(FHandle,2);
        Schroedinger:Writeln(FHandle,3);
        KleinGordon:Writeln(FHandle,4);
        SineGordon:Writeln(FHandle,5);
        PhiFour:Writeln(FHandle,6);
        doubleSineGordon:Writeln(FHandle,7);
      End;{of Case}
      WriteLn(Fhandle,Str1);
      WriteLn(Fhandle,Time,' ',dt);

      WriteLn(Fhandle,Str2);
      WriteLn(Fhandle,AnimationSpeed);

      Writeln(Fhandle,str3);
      WriteLn(Fhandle,vacuumPts,' ',StartEdge,' ',StopEdge);

      Case LeftB of
           Fixed:       Store1:=1;
           Periodic:    Store1:=2;
           Matched:     Store1:=3;
      End;
      WriteLn(Fhandle,str4);
      WriteLn(Fhandle,Store1);

      Case RightB of
           Fixed:       Store1:=1;
           Periodic:    Store1:=2;
           Matched:     Store1:=3;
      End;
      WriteLn(Fhandle,str5);
      WriteLn(Fhandle,Store1);

      WriteLn(Fhandle,str6);
      WriteLn(Fhandle, SourceList.NumberOfItems);
      SourceList.ArchiveAll(fhandle);

      WriteLn(Fhandle,str7);
      WriteLn(Fhandle, SegmentList.NumberOfItems);
      SegmentList.ArchiveAll(fhandle);
      WriteLn(Fhandle,'Index_Parser_String  ');
      WriteLn(FHandle,indexParserStr);
      WriteLn(Fhandle,'Initial_Function_Parser_String  ');
      WriteLn(FHandle,ParserStr1);
      WriteLn(FHandle,ParserStr2);

      Writeln(Fhandle,str8);
      WriteLn(Fhandle, DetectorList.NumberOfItems);
      DetectorList.ArchiveAll(fhandle);

      Writeln(Fhandle,str9);
      WriteLn(Fhandle, ReflectorList.NumberOfItems);
      ReflectorList.ArchiveAll(fhandle);

      Writeln(Fhandle,str10);
      Anly1.Archive(fhandle);
      Anly2.Archive(fhandle);

      WriteLn(Fhandle,str11);
      WriteLn(Fhandle,NumPts,' ',theWave.Scale.yMin,' ',theWave.Scale.yMax);
      For i:=1 to Numpts do
          WriteLn(Fhandle, Yvec.Value(i),' ',YprevVec.Value(i),' ',DisplayVec.Value(i));

      WriteLn(Fhandle,Comment1);
      WriteLn(Fhandle,Comment2);

      WriteLn(Fhandle,str12);

      Close(Fhandle);
      {$I+}
      Announce('File Saved.');
End;
{----------------------------------------------------------------------------}
Procedure Load;

{$I-}

Var
   LoadInputPort:       TInputScreen;
   fHandle:             Text;
   LoadStr,
   msg:                 String;
   SouPtr:              SourcePtr;
   SegPtr:              SegmentPtr;
   Dptr:                DetectorPtr;
   Rptr:                ReflectorPtr;
   Leave:               Boolean;
   Store1,Store2:       Integer;
   dX,x:                Real;
   i,j,counter:         Integer;

Function CompareFormat( msg1, msg2: String ): Boolean;

Begin;
      CompareFormat:=True;
      If not(msg1=msg2) then
         begin;
               Announce('This file format is not valid.');
               Close(Fhandle);
               CompareFormat:=False;
         end;
End;

PROCEDURE ShowComments;

VAR
  theInputPort:  TInputScreen;

BEGIN
   WITH theInputPort DO BEGIN
      Init;
      DefineInputPort(0.19, 0.81, 0.48, 0.75);
      LoadLine('----LOAD operation has completed successfully---');
      LoadLine('Load file comments:                            ');
      LoadLine(''+Comment1);
      LoadLine(''+Comment2);
      LoadLine('-----------------------------------------------');
      LoadLine('   Hit <Enter> to clear comments from screen.  ');
      LoadLine('');
      LoadLine('');
      LoadLine('    [  Ok  ]                       [Cancel]    ');

      AcceptScreen;

      done;
   END;
END;

Procedure UnArchiveBasisData(Var anItem: BasePtr);

Var
   Store1,Store2,Store3,
   Store4,Store5:        Integer;

Begin
     With anItem^ do
          begin
               ReadLn(fhandle,x0,y0,Store1,Store2,Store3,Store4,Store5);
               IsVisible:=FromNum(Store1);
               OutLined:=FromNum(Store2);
               DragIt:=FromNum(Store3);
               Disabled:=FromNum(Store4);
               Case Store5 of
                    1:      TheBasisType:=Sou;
                    2:      TheBasisType:=Seg;
                    3:      TheBasisType:=Det;
                    4:      TheBasisType:=Ref;
                    5:      TheBasisType:=Other;
               End; {of Case}
          end;
End;

Procedure UnArchiveSourceData;

Var
   Store1,Store2,Store3,Store4:       Integer;

Begin
     With SouPtr^.ParSource do
          begin
               ReadLn(fhandle,Num,store1,store2,Freq,Delay,Amp,A,Radius);
               ReadLn(fhandle,FuncStr);
               ReadLn(fhandle,Store3,SourcePeriod);
               Periodic:=FromNum(Store3);
               ReadLn(fhandle,Store4,AmpNoise,CoherenceTime);
               Noiseon:=FromNum(Store4);
               ReadLn(fhandle,Store4);
               ForceAmplitude:=FromNum(Store4);
               Case Store1 of
                    1:     kind:=Sine;
                    2:     kind:=Gaussian;
                    3:     kind:=Pulse;
                    4:     kind:=step;
                    5:     kind:=ModGaussian;
                    6:     kind:=UserDefined;
               End; {of Case}

               Case Store2 of
                    1:     direction:=toleft;
                    2:     direction:=toright;
                    3:     direction:=bidirectional;
               End; {of Case}
          end;
End;

Procedure UnArchiveSegmentData;

Begin
     With SegPtr^.ParSegment do
          ReadLn(fhandle,Index,Width,Gain,SegPtr^.objColor);
End;

Procedure UnArchiveDetectorData;

Var
   Store1{,Store2}:       Integer;

Begin
     With Dptr^.ParDetector do
          begin
               ReadLn(fhandle, Num);
               Dptr^.objColor:=Num;
          end;
End;

Procedure UnArchiveReflectorData;

Var
   Store1,Store2:       Integer;

Begin
     With Rptr^.ParReflector do
          begin
          ReadLn(fhandle,Store1);
               Case Store1 of
                    1:   ReflectKind:=theRight;
                    2:   ReflectKind:=theLeft;
                    3:   ReflectKind:=theSum;
               End; {of Case}
          end;
End;

Begin;
      {$I-}
      LoadStr:=OpenFile('*.wav');
      If LoadStr='' then exit;
      Assign(Fhandle,LoadStr);
      Reset(Fhandle);
      If not(IOResult=0) then
         begin;
               Announce('That File does not exist.');
               exit;
         end;
      ReadLn(Fhandle,msg);
      If msg=FileHeader Then version:=currentVersion
        Else if msg= 'CUPS:Wave Ver 3.2' Then version:=3.2
          Else version:=0;
      If version < 3.2-0.01 then
        Begin
        Announce('Incorrect WAVE format.');
        exit;
        end;
      If version <currentVersion-0.01 Then
        Announce('Caution:  Old WAVE file format.  Not all parameters have been loaded.');
      {$IFDEF DEBUG} {$I+} {$ELSE} {$I-} {$ENDIF}
      SaveStr:=LoadStr;
      Time:=0;
      EvenTime:=True;
      Anly1.Disable;
      Anly2.Disable;

      SourceList.Empty;
      DetectorList.Empty;
      sourceNum:=0;
      detectNum:=0;
      SegmentList.Empty;
      ReflectorList.Empty;
      theWave.PlotTitle.remove;
      TheWave.Remove;
      TheWave.isVisible:=True;
      IndexParserStr:='';
      offsetComponents:=FALSE;
      ReadLn(Fhandle,msg);
      If not(CompareFormat(msg,str0)) then
        Begin
        Announce('This file does not have a  WAVE data format.');
        exit;
        end;
      Readln(FHandle,store1);
      Case store1 of
        1:EquationType:=EandM;
        2:EquationType:=diffusion;
        3:EquationType:=Schroedinger;
        4:EquationType:=KleinGordon;
        5:EquationType:=sineGordon;
        6:EquationType:=phiFour;
        7:EquationType:=doubleSinegordon;
      End;{of Case}
      ReadLn(Fhandle,msg);
      If not(CompareFormat(msg,str1)) then
        Begin
        Announce('This file does not have a  WAVE data format.');
        exit;
        end;
      ReadLn(Fhandle,Time,dt);


      ReadLn(Fhandle,msg);
      If not(CompareFormat(msg,str2)) then
        Begin
        Announce('This file does not have a WAVE data format.');
        exit;
        end;
      ReadLn(Fhandle, AnimationSpeed);

      ReadLn(Fhandle,msg);
      If not(CompareFormat(msg,str3)) then
        Begin
        Announce('This file does not have a WAVE data format.');
        exit;
        end;
      ReadLn(Fhandle, VacuumPts,StartEdge,StopEdge);
      dx:=(stopEdge-startEdge)/(VacuumPts-1);
      if EquationType in [EandM,KleinGordon,sineGordon,doubleSineGordon,phiFour] Then dt:=1/VacuumPts;
      if EquationType=Schroedinger Then hiddenPts:=vacuumPts DIV 2 else hiddenPts:=1;
      ReadLn(Fhandle,msg);
      If not(CompareFormat(msg,str4)) then
        Begin
        Announce('This file does not have a CUPS WAVE data format.');
        exit;
        end;
      ReadLn(Fhandle,Store1);
      Case Store1 of
           1:LeftB:=Fixed;
           2:LeftB:=Periodic;
           3:LeftB:=Matched;
      End;

      ReadLn(Fhandle,msg);
      If not(CompareFormat(msg,str5)) then
        Begin
        Announce('This file does not have a CUPS WAVE data format.');
        exit;
        end;
      ReadLn(Fhandle,Store1);
      Case Store1 of
           1:RightB:=Fixed;
           2:RightB:=Periodic;
           3:RightB:=Matched;
      End;

      Readln(Fhandle,msg);
      If not(CompareFormat(msg,str6)) then
        Begin
        Announce('This file does not have a CUPS WAVE data format.');
        exit;
        end;
      ReadLn(Fhandle,counter);
      If (Counter>0) then
         For i:=1 to counter do
             begin
                  New(SouPtr, Init(1, 0, @TheWave));
                  UnArchiveBasisData(BasePtr(SouPtr));
                  UnArchiveSourceData;
                  SouPtr^.CalcGridIndex;
                  SourceList.AddItem(SouPtr);
             end;

      Readln(Fhandle,msg);
      If not(CompareFormat(msg,str7)) then
        Begin
        Announce('This file does not have a CUPS WAVE data format.');
        exit;
        end;
      ReadLn(Fhandle,counter);
      If (Counter>0) then
         For i:=1 to counter do
             begin
                  New(SegPtr, Init(0, @TheWave));
                  UnArchiveBasisData(BasePtr(SegPtr));
                  UnArchiveSegmentData;
                  SegPtr^.CalcGridIndex;
                  SegmentList.AddItem(SegPtr);
             end;
      Readln(FHandle,msg); {Index_Parser_String}
      Readln(FHandle,IndexParserStr);
      If version>3.25 Then
        Begin
        Readln(FHandle,msg);{Initial_Function_Parser_string}
        Readln(FHandle,ParserStr1);
        Readln(FHandle,ParserStr2);
        End;
      Readln(Fhandle,msg);
      If not(CompareFormat(msg,str8)) then
        Begin
        Announce('This file does not have a CUPS WAVE data format.');
        exit;
        end;
      ReadLn(Fhandle,counter);
      DetectNum:=0;
      If (Counter>0) then
         For i:=1 to counter do
             begin
                  New(Dptr, Init(1,0, @TheWave));
                  UnArchiveBasisData(BasePtr(Dptr));
                  UnArchiveDetectorData;
                  Dptr^.CalcGridIndex;
                  DetectorList.AddItem(Dptr);
                  Inc(DetectNum);
             end;

      Readln(Fhandle,msg);
      If not(CompareFormat(msg,str9)) then
        Begin
        Announce('This file does not have a CUPS WAVE data format.');
        exit;
        end;
      ReadLn(Fhandle,counter);
      If (Counter>0) then
         For i:=1 to counter do
             begin
                  New(Rptr, Init(0, @TheWave));
                  UnArchiveBasisData(BasePtr(Rptr));
                  UnArchiveReflectorData;
                  Rptr^.CalcGridIndex;
                  ReflectorList.AddItem(Rptr);
             end;

       Readln(Fhandle,msg);
       If not(CompareFormat(msg,str10))
       then
        Begin
        Announce('Analysis type error. Load not complete');
        exit;
        end
       else
        begin
        Anly1.Unarchive(fHandle);
        Anly2.Unarchive(fHandle);
        end;

      Readln(Fhandle,msg);
      If not(CompareFormat(msg,str11)) then
        Begin
        Announce('This file does not have a CUPS WAVE data format.');
        exit;
        end
      Else theWave.Unarchive(fHandle);

      ReadLn(Fhandle, Comment1);
      ReadLn(Fhandle, Comment2);
      If (Comment1='') and (Comment2='') then Comment1:='No comments in file.';
      Close(Fhandle);
      time:=0;
      yPrevInitVec.Equate( 1, YprevVec );
      YInitVec.Equate( 1, Yvec );
      initDisplayVec.Equate( 1, displayvec );
      Case EquationType of
        EandM:theWave.PlotTitle.SetStr('Electromagnetic');
        kleinGordon:theWave.PlotTitle.SetStr('Klein-Gordon');
        sineGordon:theWave.PlotTitle.SetStr('sine-Gordon');
        phiFour:theWave.PlotTitle.SetStr('phi-four');
        doubleSineGordon:theWave.PlotTitle.SetStr('double sine-Gordon');
        diffusion:theWave.PlotTitle.SetStr('Diffusion');
        Schroedinger:theWave.PlotTitle.SetStr('Schroedinger');
      end;{of case}
      TheWave.UpdateVecs;
      TheWave.AutoSetTimeInc;
      theWave.buttonList.SetVisible;
      TheWave.Display;
      If TheWave.isVisible Then ShowDensity;
      ShowComments;
      anly1.resetAndInit;
      anly2.resetAndInit;
      MainMenu.Display;
      SetStopKeys;
      StopKeys.Display;
      theWave.DisplayLegend;
{$I+}
End;
{----------------------------------------------------------------------------}
Procedure Merge;

{$I-}

Var
   LoadInputPort:       TInputScreen;
   fHandle:             Text;
   LoadStr,
   msg:                 String;
   Leave:               Boolean;
   Store1,Store2:       Integer;
   dX,x:                Real;
   i,j,counter:         Integer;

Function CompareFormat( msg1, msg2: String ): Boolean;

Begin;
      CompareFormat:=True;
      If not(msg1=msg2) then
         begin;
               Announce('This file format is not valid.');
               Close(Fhandle);
               CompareFormat:=False;
         end;
End;

PROCEDURE ShowComments;

VAR
  theInputPort:  TInputScreen;

BEGIN
   WITH theInputPort DO BEGIN
      Init;
      DefineInputPort(0.19, 0.81, 0.48, 0.75);
      LoadLine('    MERGE operation completed successfully.    ');
      LoadLine('---The two wave functions have been combined.--');
      LoadLine('Merged file comments:                          ');
      LoadLine(''+Comment1);
      LoadLine(''+Comment2);
      LoadLine('-----------------------------------------------');
      LoadLine('   Hit <Enter> to clear comments from screen.  ');
      LoadLine('');
      LoadLine('    [  Ok  ]                       [Cancel]    ');

      AcceptScreen;

      done;
   END;
END;

Begin;
      {$I-}
      LoadStr:=OpenFile('*.wav');
      If LoadStr='' then exit;
      Assign(Fhandle,LoadStr);
      Reset(Fhandle);
      If not(IOResult=0) then
         begin;
               Announce('That File does not exist.');
               exit;
         end;
      ReadLn(Fhandle,msg);
      If msg=FileHeader Then version:=currentVersion
        Else if msg= 'CUPS:Wave Ver 3.2' Then version:=3.2
          Else version:=0;
      If version < 3.2-0.01 then
        Begin
        Announce('Incorrect WAVE format.');
        exit;
        end;
      {$IFDEF DEBUG} {$I+} {$ELSE} {$I-} {$ENDIF}
      ReadLn(Fhandle,msg);
      If not(CompareFormat(msg,str0)) then
        Begin
        Announce('This file does not have a  WAVE data format.');
        exit;
        end;
      Readln(FHandle,store1);
      Case store1 of
        1:if EquationType<>EandM then begin Announce('Sorry: Cannot merge different equation types.'); exit end;
        2:if EquationType<>diffusion then begin Announce('Sorry: Cannot merge different equation types.'); exit end;
        3:if EquationType<>schroedinger then begin Announce('Sorry: Cannot merge different equation types.'); exit end;
        4:if EquationType<>kleinGordon then begin Announce('Sorry: Cannot merge different equation types.'); exit end;
        5:if EquationType<>sineGordon then begin Announce('Sorry: Cannot merge different equation types.'); exit end;
        6:if EquationType<>phiFour then begin Announce('Sorry: Cannot merge different equation types.'); exit end;
        7:if EquationType<>doubleSinegordon then begin Announce('Sorry: Cannot merge different equation types.'); exit end;
      End;{of Case}
      Repeat ReadLn(Fhandle,msg) until msg= str11;
      theWave.Merge(fHandle);

      ReadLn(Fhandle, Comment1);
      ReadLn(Fhandle, Comment2);
      If (Comment1='') and (Comment2='') then Comment1:='No comments in file.';
      Close(Fhandle);
      yPrevInitVec.Equate( 1, YprevVec );
      YInitVec.Equate( 1, Yvec );
      initDisplayVec.Equate( 1, displayvec );
      TheWave.UpdateVecs;
      TheWave.Display;
      If TheWave.isVisible Then ShowDensity;
      ShowComments;
      anly1.resetAndInit;
      anly2.resetAndInit;
      MainMenu.Display;
{$I+}
End;
{----------------------------------------------------------------------------}

Procedure HandleMenu;
Procedure MenuUnCheck(Col:Integer);
  Var i:Integer;
  Begin
  For i:=1 to 16 Do MainMenu.RowChecked(Col,i,False);
  End;

Procedure MenuActivate(col:Integer);
var  i:Integer;
Begin
  For i:=1 to 16 Do mainMenu.RowActivate(col,i,True);
End;

Procedure EnableAllRows;
Begin
Anly1.Disable;
Anly2.Disable;
menuUncheck(4);
menuUncheck(5);
menuUncheck(2);
menuActivate(3);
menuActivate(4);
menuActivate(5);
theWave.buttonList.MakeVisible
End;

Procedure ResetAnlyRows;
var i : integer;
Begin
for i := 1 to 9 do MainMenu.rowchecked(3,i,false);
MainMenu.rowchecked(3,9,true);
With MainMenu do
Case EquationType of
  EandM:
                          RowChecked(2,7,True);
  KleinGordon:
                          Begin
                          RowChecked(2,8,True);
                          RowActivate(4,3,FALSE);
                          RowActivate(5,3,FALSE);
                          RowActivate(4,4,FALSE);
                          RowActivate(5,4,FALSE);
                          RowActivate(4,10,FALSE);
                          RowActivate(5,10,FALSE);
                          RowActivate(4,11,FALSE);
                          RowActivate(5,11,FALSE);
                          End;
  sineGordon:
                          Begin
                          RowChecked(2,9,True);
                          RowActivate(4,3,FALSE);
                          RowActivate(5,3,FALSE);
                          RowActivate(4,4,FALSE);
                          RowActivate(5,4,FALSE);
                          RowActivate(4,10,FALSE);
                          RowActivate(5,10,FALSE);
                          RowActivate(4,11,FALSE);
                          RowActivate(5,11,FALSE);
                          End;
  phiFour:
                          Begin
                          RowChecked(2,10,True);
                          RowActivate(4,3,FALSE);
                          RowActivate(5,3,FALSE);
                          RowActivate(4,4,FALSE);
                          RowActivate(5,4,FALSE);
                          RowActivate(4,10,FALSE);
                          RowActivate(5,10,FALSE);
                          RowActivate(4,11,FALSE);
                          RowActivate(5,11,FALSE);
                          End;
  doublesineGordon:
                          Begin
                          RowChecked(2,11,True);
                          RowActivate(4,3,FALSE);
                          RowActivate(5,3,FALSE);
                          RowActivate(4,4,FALSE);
                          RowActivate(5,4,FALSE);
                          RowActivate(4,10,FALSE);
                          RowActivate(5,10,FALSE);
                          RowActivate(4,11,FALSE);
                          RowActivate(5,11,FALSE);
                          End;
  diffusion:
                          Begin
                          RowChecked(2,13,True);
                          RowActivate(4,10,FALSE);
                          RowActivate(5,10,FALSE);
                          End;
  Schroedinger:
                          Begin
                          RowChecked(2,15,True);
                          RowActivate(3,2,FALSE);
                          RowActivate(3,3,FALSE);
                          RowActivate(3,5,FALSE);
                          RowActivate(3,6,FALSE);
                          RowActivate(3,7,FALSE);
                          {RowActivate(4,4,FALSE);
                          RowActivate(5,4,FALSE);}
                          RowActivate(4,10,FALSE);
                          RowActivate(5,10,FALSE);
                          End;
end;{of Case}
End;

Var
   i:           Integer;
   numOfSteps:  Integer;
   ok:    Boolean;

Begin;
     if TheWave.Expanded Then TheWave.expand;
     if anly1.Expanded Then anly1.expand;
     if anly2.Expanded Then anly2.expand;
     if animationSpeed<=0 Then numOfSteps:=1-AnimationSpeed Else numOfSteps:=1;
     With MainMenu DO
           CASE colChosen OF
                1:Case rowChosen of
                   1: AboutCups;
                   2: ShowHelp('Wave.Hlp','About Wave');
                   3: Configuration;
                   5: Begin
                      EnableAllRows;
                      Load;
                      theWave.buttonList.SetVisible;
                      theWave.buttonList.Display;
                      resetAnlyRows;
                      End;
                   6: Merge;
                   7: Save(False);
                   8: Save(True);
                   10: QuitFlag := true;
                   End;

                2:Begin
                    Case rowChosen of
                       1: if theWave.SetSpaceIncrement then
                             Begin
                                theWave.AutoSetTimeInc;
                                SetStopKeys;
                                StopKeys.Display;
                                theWave.updateVecs;
                                Anly1.Disable;
                                Anly2.Disable;
                             end;
                       2: If theWave.SetTimeIncrement Then
                                Begin
                                     SetStopKeys;
                                     StopKeys.Display;
                                     ResetAnly(@Anly1,True);
                                     ResetAnly(@Anly2,True);
                                     Anly1.DvecCompress:=1;
                                     Anly1.DvecCompressCounter:=1;
                                     Anly2.DvecCompress:=1;
                                     Anly2.DvecCompressCounter:=1;
                                     If TheWave.isVisible then
                                        begin
                                        TheWave.LockFocus;
                                        ShowDensity;
                                        end
                                end;
                       3: TheWave.ChangeScale;
                       4: Anly1.ChangeScale;
                       5: Anly2.ChangeScale;
                       7: If EquationType<>EandM Then
                          Begin
                          EquationType:=EandM;
                          IndexParserStr:='';
                          EnableAllRows;
                          theWave.PlotTitle.Remove;
                          segmentlist.empty;
                          theWave.InitZero;
                          sourcelist.empty;
                          sourceNum:=0;
                          reflectorlist.empty;
                          detectorlist.empty;
                          detectNum:=0;
                          theWave.buttonList.SetVisible;
                          theWave.PlotTitle.SetStr('Electromagnetic');
                          ResetAnlyRows;
                          End;
                       8: If EquationType<>KleinGordon Then
                          Begin
                          EquationType:=KleinGordon;
                          IndexParserStr:='';
                          EnableAllRows;
                          theWave.PlotTitle.Remove;
                          SegmentList.Empty;
                          sourcelist.empty;
                          sourceNum:=0;
                          reflectorlist.empty;
                          detectorlist.empty;
                          detectNum:=0;
                          theWave.buttonList.SetVisible;
                          theWave.PlotTitle.SetStr('Klein-Gordon');
                          theWave.InitZero;
                          ResetAnlyRows;
                          End;
                       9: If EquationType<>sineGordon Then
                          Begin
                          EquationType:=sineGordon;
                          IndexParserStr:='';
                          EnableAllRows;
                          theWave.PlotTitle.Remove;
                          SegmentList.Empty;
                          sourcelist.empty;
                          sourceNum:=0;
                          reflectorlist.empty;
                          detectorlist.empty;
                          detectNum:=0;
                          theWave.buttonList.SetVisible;
                          theWave.PlotTitle.SetStr('sine-Gordon');
                          theWave.InitZero;
                          ResetAnlyRows;
                          End;
                       10: If EquationType<>phiFour Then
                          Begin
                          EquationType:=phiFour;
                          IndexParserStr:='';
                          EnableAllRows;
                          theWave.PlotTitle.Remove;
                          SegmentList.Empty;
                          sourcelist.empty;
                          sourceNum:=0;
                          reflectorlist.empty;
                          detectorlist.empty;
                          detectNum:=0;
                          theWave.buttonList.SetVisible;
                          theWave.PlotTitle.SetStr('phi-Four');
                          theWave.InitZero;
                          ResetAnlyRows;
                          End;
                       11:If EquationType<>doublesineGordon Then
                          Begin
                          EquationType:=doubleSineGordon;
                          IndexParserStr:='';
                          EnableAllRows;
                          theWave.PlotTitle.Remove;
                          SegmentList.Empty;
                          sourcelist.empty;
                          sourceNum:=0;
                          reflectorlist.empty;
                          detectorlist.empty;
                          detectNum:=0;
                          theWave.buttonList.SetVisible;
                          theWave.PlotTitle.SetStr('double sine-Gordon');
                          theWave.InitZero;
                          ResetAnlyRows;
                          End;
                       13:If EquationType<>diffusion Then
                          Begin
                          EquationType:=diffusion;
                          IndexParserStr:='';
                          EnableAllRows;
                          theWave.PlotTitle.Remove;
                          SegmentList.Empty;
                          theWave.PlotTitle.SetStr('diffusion');
                          theWave.InitZero;
                          sourcelist.empty;
                          sourcenum:=0;
                          reflectorlist.empty;
                          detectorlist.empty;
                          detectNum:=0;
                          theWave.buttonList.SetVisible;
                          ResetAnlyRows;
                          End;
                       15:If EquationType<>Schroedinger Then
                          Begin
                          EquationType:=Schroedinger;
                          IndexParserStr:='';
                          EnableAllRows;
                          theWave.PlotTitle.Remove;
                          SegmentList.Empty;
                          theWave.PlotTitle.SetStr('Schroedinger');
                          theWave.InitZero;
                          sourcelist.empty;
                          sourceNum:=0;
                          reflectorlist.empty;
                          detectorlist.empty;
                          detectNum:=0;
                          theWave.buttonList.SetVisible;
                          ResetAnlyRows;
                          End;
                    End;{of RowChosen Case}
                    SetRunKeys;
                    SetStopKeys;
                    StopKeys.Display;
                    TheWave.Display;
                  End;

                3: Begin
                       Case rowChosen of
                       1:theWave.InitZero;
                       2:ok:=theWave.InitRandom;
                       3:ok:=TheWave.InitFunction(Sine);
                       4:ok:=TheWave.InitFunction(Gaussian);
                       5:ok:=TheWave.InitFunction(modGaussian);
                       6:ok:=TheWave.InitFunction(Step);
                       7:ok:=TheWave.InitFunction(Pulse);
                       8:ok:=TheWave.InitModes;
                       9:ok:=TheWave.UserFunc;
                       End;{of Init RowChosen Case.}
                       if Not(ok) Then
                         Begin
                         theWave.Display;
                         Anly1.Display;
                         Anly2.Display;
                         Exit;
                         End;
                       anly1.remove;
                       anly2.remove;
                       time:=0; EvenTime:=True;
                       sourceList.Clear;
                       yInitVec.Equate(1,yvec);
                       yPrevInitVec.Equate(1,yPrevVec);
                       initDisplayVec.Equate(1,displayVec);
                       If Anly1.Freeze=False
                         Then
                         Begin
                         ResetAnly(@Anly1,True);
                         Anly1.DvecCompress:=1;
                         Anly1.DvecCompressCounter:=1;
                         End;
                       If Anly2.Freeze=False
                         Then
                         Begin
                         ResetAnly(@Anly2, True);
                         Anly2.DvecCompress:=1;
                         Anly2.DvecCompressCounter:=1;
                         End;

                       anly1.isVisible:=True;
                       anly2.isVisible:=True;
                       SourceList.Clear;
                       ReflectorList.Clear;
                       DetectorList.ZeroDarray(@Anly1);
                       DetectorList.ZeroDarray(@Anly2);
                       SetRunKeys;
                       SetStopKeys;
                       StopKeys.Display;
                       theWave.DisplayLegend;
                       Update;
                   End;

           4: begin;
                    if rowchosen<11 then
                    begin
                       for i := 1 to 10 do rowchecked(4,i,false);
                       rowchecked(4,rowchosen,true);
                    end;
                    Case rowChosen of
                         1: Anly1.Disable;
                         2: begin;   {Index of Refraction}
                                  Anly1.AnalysisType:=Index_Gain;
                                  Anly1.ResetAndInit;
                            end;
                         3: begin; {<Y*Y> vs Time}
                                  Anly1.AnalysisType:=EvsT;
                                  Anly1.ResetAndInit;
                            end;
                         4: begin; {Wave-Time Contour}
                                   Anly1.AnalysisType:=YvsTdensity;
                                   Anly1.ResetAndInit;
                            end;
                         5: begin; {Wave Time- 3D}
                                   Anly1.AnalysisType:=YvsT3D;
                                   Anly1.ResetAndInit;
                            end;
                         6: begin;{FFT - y(x)}
                                  Anly1.AnalysisType:=FFTpos;
                                  Anly1.ResetAndInit;
                            end;
                         7: if detectorList.nodes=nil Then
                                 announce('Please create a DETECTOR before you select this analysis type.')
                                 else
                                 begin {FFT - y(t)}
                                 Anly1.AnalysisType:=FFTtime;
                                 Anly1.ResetAndInit;
                                end;
                         8: if detectorList.nodes=nil Then
                                 announce('Please create a DETECTOR before you select this analysis type.')
                                  else
                                  begin  {Detectors}
                                  Anly1.AnalysisType:=YvsT;
                                  Anly1.ResetAndInit;
                                  end;
                         9: if sourceList.nodes=nil Then
                                 announce('Please create a SOURCE before you select this analysis type.')
                                 else
                                 begin  {Source Power}
                                  Anly1.AnalysisType:=PvsT;
                                  Anly1.ResetAndInit;
                                  end;
                         10: begin { Y*Y Distribution }
                                  Anly1.AnalysisType:=EnergyDist;
                                  Anly1.ResetAndInit;
                             end;
                         12: begin; {Refresh}
                                  If (Anly1.GeneralType=ThreeD) then GraphBackColor:=Black;
                                  Anly1.display;
                                  If GraphBackColor=Black then GraphBackColor:=DarkGray;
                            end;
                    End; {of Case}
                    MainMenu.Display;
           End;
           5:begin;
                    if rowchosen<11 then
                    begin
                       for i := 1 to 10 do rowchecked(5,i,false);
                       rowchecked(5,rowchosen,true);
                    end;
                    Case rowChosen of
                         1: Anly2.Disable;
                         2: begin;   {Index of Refraction}
                                  Anly2.AnalysisType:=Index_Gain;
                                  Anly2.ResetAndInit;
                            end;
                         3: begin; {<Y*Y> vs Time}
                                  Anly2.AnalysisType:=EvsT;
                                  Anly2.ResetAndInit;
                            end;
                         4: begin; {Wave-Time Contour}
                                   Anly2.AnalysisType:=YvsTdensity;
                                   Anly2.ResetAndInit;
                            end;
                         5: begin; {Wave Time- 3D}
                                   Anly2.AnalysisType:=YvsT3D;
                                   Anly2.ResetAndInit;
                            end;
                         6: begin;{FFT - y(x)}
                                  Anly2.AnalysisType:=FFTpos;
                                  Anly2.ResetAndInit;
                            end;
                         7: if detectorList.nodes=nil Then
                                 announce('Please create a DETECTOR before you select this analysis type.')
                                 else
                                 begin {FFT - y(t)}
                                 Anly2.AnalysisType:=FFTtime;
                                 Anly2.ResetAndInit;
                            end;
                         8: if detectorList.nodes=nil Then
                                 announce('Please create a DETECTOR before you select this analysis type.')
                                  else
                                  begin  {Detectors}
                                  Anly2.AnalysisType:=YvsT;
                                  Anly2.ResetAndInit;
                             end;
                         9: if sourceList.nodes=nil Then
                                 announce('Please create a SOURCE before you select this analysis type.')
                                  else
                                  begin  {Source Power}
                                  Anly2.AnalysisType:=PvsT;
                                  Anly2.ResetAndInit;
                             end;
                         10: begin { y*y Distribution }
                                  Anly2.AnalysisType:=EnergyDist;
                                  Anly2.ResetAndInit;
                             end;
                         12: begin; {Refresh}
                                  If (Anly2.GeneralType=ThreeD) then GraphBackColor:=Black;
                                  Anly2.display;
                                  If GraphBackColor=Black then GraphBackColor:=DarkGray;
                            end;
                    End; {of Case}
                    MainMenu.Display;
           End;

                6: Case rowChosen of
                   1:BoundaryPref;
                   2:SaturationPref;
                   3:ParseIndex;
                 5,6:if ReflectorList.NumberOfItems=0 then
                        announce(' You do not have any reflectors. ')
                     else Begin
                        if rowChosen=5 then ReflectorList.InspectAll
                        else ReflectorList.Empty;
                        theWave.Display;
                     End;
                 8,9:if sourceList.NumberOfItems=0 then
                        announce(' You do not have any sources. ')
                     else begin
                        if rowchosen=8 then SourceList.InspectAll
                        else SourceList.Empty;
                        theWave.Display;
                        resetSourcePlot;
                     End;
               11,12:if segmentList.NumberOfItems=0 then
                        announce(' You do not have any segments. ')
                     else Begin
                        if rowchosen=11 then SegmentList.InspectAll
                        else SegmentList.Empty;
                        theWave.UpdateVecs;
                        theWave.Display;
                        resetIndexPlot;
                     End;
               14,15:if DetectorList.NumberOfItems=0 then
                        announce(' You do not have any detectors. ')
                     else begin
                        if rowchosen=14 then DetectorList.InspectAll
                        else DetectorList.Empty;
                        theWave.Display;
                     End;
                   End;

                7: Case rowChosen of
                   1: TheWave.ShowAttrib;
                   2: FilesPref;
                   3: SpeedPref;
                   4: DemoPref;
                End;

      End;
End;
{----------------------------------------------------------------------------}
Procedure HandleRunKeys( key: Byte );
Var temp: Real;   i:Integer;
Begin;
      Case Key of
           1: ShowHelp('Wave.HLP','Run Help');
           2: Running:=False;
           3: if EquationType in [EandM,Schroedinger] Then
              begin
              TheWave.PlotAxes.Remove;
              if OffsetComponents Then {rescale down by 2x to remove offset}
                With theWave.Scale Do Begin
                temp:=yMax-yMin;
                TheWave.Scale.yMax:=yMax-temp/4;
                TheWave.Scale.yMin:=yMin+temp/4;
                End
              Else {rescale up by 2x to add offset}
                With theWave.Scale Do Begin
                temp:=yMax-yMin;
                TheWave.Scale.yMax:=yMax+temp/2;
                TheWave.Scale.yMin:=yMin-temp/2;
                End;
              With TheWave.Scale do TheWave.SetScale(xMin,yMin,xMax,yMax);
              TheWave.PlotAxes.Display;
              offsetComponents:=Not(offsetComponents);
              SetRunKeys;
              SetStopKeys;
              RunKeys.Display;
              theWave.Display;
              end
              else
              begin
              offsetComponents:=Not(offsetComponents);
              SetRunKeys;
              SetStopKeys;
              RunKeys.Display;
              theWave.Display;
              end;
           4: Case EquationType of
                Diffusion:Announce('Time is not reversible in the diffusion equation.');
                Schroedinger:
                  Begin
                  dt:=-dt;
                  SetRunKeys;
                  SetStopKeys;
                  RunKeys.Display;
                  ResetAnly(@Anly1,False);
                  ResetAnly(@Anly2,False);
                  End;
                Else
                  Begin
                  For i:=1 To Numpts Do
                    Begin
                    temp:=yVec.VecPtr^[i];
                    yVec.VecPtr^[i]:=yPrevVec.VecPtr^[i];
                    yPrevVec.VecPtr^[i]:=temp;
                    End ;
                  dt:=-dt;
                  if EquationType<>EandM Then time:=time+dt;
                  SetRunKeys;
                  SetStopKeys;
                  RunKeys.Display;
                  ResetAnly(@Anly1,False);
                  ResetAnly(@Anly2,False);
                  End;{of Else}
              End;{of Case}
           5: begin
              If (AnimationSpeed <=  99) then Dec(AnimationSpeed);
              If Anly1.Freeze=FALSE Then ResetAnly(@Anly1,False);
              If Anly2.Freeze=FALSE Then ResetAnly(@Anly2,False);
              end;
           6: begin
              If (AnimationSpeed >= -99) then Inc(AnimationSpeed);
              If Anly1.Freeze=FALSE Then ResetAnly(@Anly1,False);
              If Anly2.Freeze=FALSE Then ResetAnly(@Anly2,False);
              end;

      End;
End;
{----------------------------------------------------------------------------}
Procedure HandleStopKeys( key: Byte );

Var
   LastTime1,
   LastTime2:           Real;
   AnalysisSize,i:      Integer;
   temp:                Real;
Procedure IncrementAnalysisPlots( ThePlot: AnlyPlotPtr );

Var
   xNew:        Real;

 Begin;
       With ThePlot^ do
            begin;
                  If Freeze then exit;
                  If not(AnalysisType=EvsT) then
                  If not(isVisible) or not((GeneralType=Dynamic) or (GeneralType=VersusTime)) then exit;
                  If (AnalysisCounter>1024) then exit;

                  With Scale do
                       begin
                            If (Time>xMax) then
                               begin
                               xNew:=xmax+(xMax-xMin);
                               if (dt<0) and (time<xNew) then xNew:=Time;
                               RefreshAnalysisPlot(False,xMin,yMin,xNew,yMax);
                               end;
                            If (Time<xMin) then
                               begin
                               xNew:=xMin-(xMax-xMin) ;
                               if (dt>0) and (time>xnew) Then xNew:=Time;
                               RefreshAnalysisPlot(False,xNew,yMin,xMax,yMax);
                               end;
                       end;  {of With Scale}
            end; {of With ThePlot^.}
 End;

Procedure RunIt;

Begin;
      Running:=True;
      If not(SingleStep) then
         begin;
               SetRunKeys;
               Runkeys.Display;
               If DemoMode
               Then
                Message('----------------Running in DemoMode. <ESC> to exit.-------------------------')
               Else
                Message('----------------------------------Running-----------------------------------');
         end;

      If Not MouseWhileRunning
        Then HideCursor
        Else MouseBorders(0, getMaxy-GetMaxY Div 10, GetMaxX,GetMaxY);
      If Time=0 then
         begin;
               YPrevInitVec.Equate( 1, yPrevVec );
               YInitVec.Equate( 1, YVec );
         end;

      If Anly1.AnalysisCounter=1 then Anly1.IntializeAnalysisPlots;
      If Anly2.AnalysisCounter=1 then Anly2.IntializeAnalysisPlots;

      If not(Anly1.AnalysisCounter=1) and (Anly1.GeneralType=Dynamic) then Anly1.Replot;
      If not(Anly2.AnalysisCounter=1) and (Anly2.GeneralType=Dynamic) then Anly2.Replot;

      TheWave.Display;

      While Running do
            begin;
                  If (AnimationSpeed < 0) then TheWave.StepRun(1-AnimationSpeed)
                     else
                         begin;
                               TheWave.StepRun(1);
                               If not(SingleStep) then Delay(10*AnimationSpeed);
                         end;

                  Anly1.UpdatePlot;
                  Anly2.UpdatePlot;

                  TheWave.LockFocus;
                  TheWave.UpdatePlot;
                  If not(SingleStep) THEN
                     BEGIN
                          CheckForEvents;
                          If Event.KeyPressed and DemoMode then
                            Begin
                            DemoMode:=False;
                            Running:=False;
                            Beep;
                            end;
                          If RunKeys.Pressed(Key) then HandleRunKeys(Key);
                     END
                  ELSE
                      begin
                           SingleStep:=False;
                           Running:=False;
                      end;

                  IncrementAnalysisPlots(@Anly1);
                  IncrementAnalysisPlots(@Anly2);
                  if DemoMode and (Time>DemoTime)
                    then HandleStopKeys(5);
            end;
      SetStopKeys;
      StopKeys.Display;
      TheWave.Display;
      Message('');
      MainMenu.Display;
      if Not MouseWhileRunning
        Then ShowCursor
        Else MouseBorders(0, 0, GetMaxX,GetMaxY);
      anly1.buttonlist.display;
      anly2.buttonlist.display;
End;


Begin;
      i:=1;
      AnalysisSize:=1024;
      LastTime1:=1;
      LastTime2:=1;


      Case Key of
           1: ShowHelp('Wave.HLP','Global Help');
           2: Begin RunIt; End;
           3: if EquationType in [EandM,Schroedinger] Then
              begin;
              TheWave.PlotAxes.Remove;
              if OffsetComponents Then {rescale down by 2x to remove offset}
                With theWave.Scale Do Begin
                temp:=yMax-yMin;
                TheWave.Scale.yMax:=yMax-temp/4;
                TheWave.Scale.yMin:=yMin+temp/4;
                End
              Else {rescale up by 2x to add offset}
                With theWave.Scale Do Begin
                temp:=yMax-yMin;
                TheWave.Scale.yMax:=yMax+temp/2;
                TheWave.Scale.yMin:=yMin-temp/2;
                End;
              With TheWave.Scale do TheWave.SetScale(xMin,yMin,xMax,yMax);
              TheWave.PlotAxes.Display;
              offsetComponents:=Not(offsetComponents);
              SetRunKeys;
              SetStopKeys;
              StopKeys.Display;
              theWave.Display;
              end
              Else
              Begin
              offsetComponents:=Not(offsetComponents);
              SetRunKeys;
              SetStopKeys;
              StopKeys.Display;
              theWave.Display;
              End;
           4: begin;
                  SingleStep:=True;
                  RunIt;
              end;
           5: begin; {reset to t=0 conditions}
                    yPrevVec.Equate( 1, yPrevInitVec );
                    YVec.Equate( 1, YInitVec );
                    displayVec.Equate(1,initDisplayVec);

                    Time:=0;
                    EvenTime:=True;
                    If Anly1.Freeze=False Then
                      Begin
                      ResetAnly(@Anly1,True);
                      Anly1.DvecCompress:=1;
                      Anly1.DvecCompressCounter:=1;
                      End;
                    If Anly2.Freeze=False Then
                      Begin
                      ResetAnly(@Anly2,True);
                      Anly2.DvecCompress:=1;
                      Anly2.DvecCompressCounter:=1;
                      End;
                    ReflectorList.Clear;
                    SourceList.Clear;
                    DetectorList.ZeroDarray(@Anly1);
                    DetectorList.ZeroDarray(@Anly2);
                    TheWave.LockFocus;
                    Update;
                    TheWave.LockFocus;
                    If TheWave.isVisible Then ShowDensity;
                    SetStopKeys;
                    StopKeys.Display;
              end;
           6: begin;
                    CheckForEvents;
                    If MainMenu.Chosen THEN HandleMenu;
              end;

      End;
End;
{----------------------------------------------------------------------------}
Procedure FindScreenCoordinates(num1, num2: Integer; thePlot: PlotPtr; Colour: Word );

Const
     Side = 5;
     Digits=7;
Var
   x,y,button:  Integer;
   xSet,ySet,
   xRub,yRub,
   xPos,yPos:   Real;
   Inside:      Boolean;

Begin;
      If not(thePlot^.PlotAxes.IsVisible) then exit;
      thePlot^.LockFocus;
      If thePlot^.expanded then num2:=19;
      HideCursor;
      MousePosn(xPos,yPos,1,thePlot^.SclNum,Inside);
      xSet:=xPos;
      ySet:=yPos;
      Print(num1,num2,'(x,y)=('+Num2Str(xSet,digits)+' '+Num2Str(ySet,digits)+')');
      xRub:=xPos;
      yRub:=yPos;
      SetWriteMode(XorPut);
      Line(MapX(xPos),MapY(yPos)+Side,MapX(xPos),MapY(yPos)-Side);
      Line(MapX(xPos)+Side,MapY(yPos),MapX(xPos)-Side,MapY(yPos));
      Repeat
            Line(MapX(xPos),MapY(yPos)+Side,MapX(xPos),MapY(yPos)-Side);
            Line(MapX(xPos)+Side,MapY(yPos),MapX(xPos)-Side,MapY(yPos));

            MouseGlobalPosn(x,y,button);
            MousePosn(xPos,yPos,1,thePlot^.SclNum,Inside);
            If not((xRub=xPos) and (yRub=yPos)) then
               begin;
                     xRub:=xPos;
                     yRub:=yPos;
                     xSet:=xPos;
                     ySet:=yPos;
                     RubOut(6+num1,num2,2*digits+3,Colour);
                     Print(6+num1,num2,'('+Num2Str(xSet,digits)+' '+Num2Str(ySet,digits)+')');
                     xRub:=xPos;
                     yRub:=yPos;
               end;
            Line(MapX(xPos),MapY(yPos)+Side,MapX(xPos),MapY(yPos)-Side);
            Line(MapX(xPos)+Side,MapY(yPos),MapX(xPos)-Side,MapY(yPos));
      Until (button = 0);
      Line(MapX(xPos),MapY(yPos)+Side,MapX(xPos),MapY(yPos)-Side);
      Line(MapX(xPos)+Side,MapY(yPos),MapX(xPos)-Side,MapY(yPos));
      SetWriteMode(CopyPut);

      ShowCursor;
      RubOut(num1,num2,2*digits+9, Colour);
      If ThePlot=@TheWave then TheWave.Display;
End;
{----------------------------------------------------------------------------}
Procedure ButtonSetUp( Plot: PlotPtr; FindCoor: Boolean);

Var
   Colour: Word;

Begin;
      If Plot^.PlotSclBut.MouseInside then Plot^.PlotSclBut.Action
      else
      If Plot^.ExpandBut.MouseInside then Plot^.ExpandBut.Action
      else
      If Plot^.AttrBut.MouseInside then
        Begin
          Plot^.Attrbut.Action;
          if Plot^.AnalysisType=EnergyDist Then  plot^.display;
        end
      else
      If Event.MouseClicked and FindCoor then
         if Plot^.AnalysisType <> YvsT3D then
         begin
              If AnlyPlotPtr(Plot)^.AnalysisType=YvsTDensity then Colour:=black else Colour:=DarkGray;
              FindScreenCoordinates(2, 8, Plot, colour);
         end;
End;
{----------------------------------------------------------------------------}
Begin;
      Initialize;
      MainMenu.Display;
      StopKeys.Display;
      ShowHelp('Wave.Hlp','Initial Screen');
      QuitFlag := false;                                  {Isolate Mouse Events}
      ClearMKBuffers;
      Repeat
            CheckForEvents;
            Region:=MouseRegion;
            If StopKeys.Pressed(key) then HandleStopKeys(key);
            If DemoMode then HandleStopKeys(2);
            Case Region of
                 1: {Hot Key Strip};
                 2:begin;
                         ButtonSetUp( @TheWave, False );
                         If TheWave.SourceBut.MouseInside then TheWave.SourceBut.Action
                            Else
                         If TheWave.SegmentBut.MouseInside then
                            Begin
                            TheWave.SegmentBut.Action;
                            resetIndexPlot;
                            End
                            Else
                         If TheWave.DetectorBut.MouseInside then TheWave.DetectorBut.Action
                            Else
                         If TheWave.ReflectBut.MouseInside then TheWave.ReflectBut.Action
                            Else
                         Begin {first check the source since it is smaller}
                           DetectorList.ReturnClickedItem(ItemClicked );
                           If ItemClicked = Nil Then SourceList.ReturnClickedItem( ItemClicked );
                           If ItemClicked = Nil Then ReflectorList.ReturnClickedItem( ItemClicked );
                           If ItemClicked = Nil Then SegmentList.ReturnClickedItem( ItemClicked );
                         End;
                         If (ItemClicked <> Nil) and Event.DoubleClicked then
                           Begin   {Inspector will retrun FALSE if the item has been destroyed}
                                Case ItemClicked^.theBasisType of
                                  Sou:If NOT(ItemClicked^.Inspector)then resetSourcePlot;
                                  Det:If NOT(ItemClicked^.Inspector)then
                                    Begin
                                    DetectorList.DeleteItem(ItemClicked);
                                    DetectorList.RenumberDetectors;
                                    resetDetectorPlot;
                                    End;
                                  Seg:If NOT(ItemClicked^.Inspector)then resetIndexPlot;
                                  Ref:If NOT(ItemClicked^.Inspector)then ;
                                  end;{of Case}
                                theWave.Display;
                           End
                         Else
                         If (ItemClicked <> Nil) And (itemClicked^.DragIt) and Event.MouseClicked then
                           Begin ItemClicked^.Drag; resetIndexPlot; end
                         Else
                         If ((ItemClicked = Nil) or (Not(itemClicked^.DragIt)))
                             and Event.MouseClicked then FindScreenCoordinates(5, 9,@TheWave, Black);
                   end;
                 3:ButtonSetUp( @Anly1, True );
                 4:ButtonSetUp( @Anly2, True );
                 5:If MainMenu.Activated then HandleMenu;
            end;
      Until QuitFlag;
      MainMenu.Done;
      VariableDone;
      Anly1.Done;
      Anly2.Done;
      TheWave.Done;
      CupsDone;
END.
{----------------------------------------------------------------------------}
