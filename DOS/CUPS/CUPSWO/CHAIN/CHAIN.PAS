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

PROGRAM Chain;
{Version Number = 3.3                                                   4-29-94}

{$R-}
USES Chn_Base,Chn_Plot,Chn_Anly,Chn_Gbl,Chn_Graf, Chn_Lst,Chn_Fun,
     Graph, Crt, CUPSmupp, CUPS, CUPSgui, CUPSgrph;

Const
     currentVersion=3.3;
     Fileheader = 'CUPS:Chain Ver 3.3';
     str1 = 'AtomsPerCell, YMin, yMax';
     str2 = 'Animation Speed';
     str3 = 'Number of Atoms';
     str4 = 'Left Boundary';
     str5 = 'Right Boundary';
     str6 = 'Number,         y,         v,      m,      k1,     k2,     k3,     damp,   self';
     str7 = 'Next Line is Source Data:  SourceType,     ExcitationType,         Freq,   Amp,    A,      Xo';
     str8 = 'Parser Strings';
     str9 = 'AnalysisType';
VAR
   ItemClicked:         BasePtr;
   LoadStr,
   SaveStr:             String;
   Comment1,
   Comment2:            String;

{see Chn_Gbl for global variables.}
{----------------------------------------------------------------------------}
Procedure SetStopKeys;

Begin;
      With StopKeys do
           Begin;
                 Init(6);
                 key[1]:='F1-Help';
                 If Time=0 then key[2]:='F2-Run'
                    else key[2]:='F2-Continue';
                 key[3]:='F3-Step';
                 If TPlus then key[4]:='F4-PosTime'
                    else key[4]:='F4-NegTime';
                 key[5]:='F7-Reset';
                 key[6]:='F10-Menu';
           End;
End;

Procedure SetRunKeys;

Begin;
      With RunKeys do
           Begin;
                 Init(6);
                 key[1]:='F1-Help';
                 key[2]:='F2-Pause';
                 If ShowV then key[3]:='F3-Hide-V'
                    else key[3]:='F3-Show-V';
                 If TPlus then key[4]:='F4-PosTime'
                    else key[4]:='F4-NegTime';
                 key[5]:='F5-Faster';
                 key[6]:='F6-Slower';
           End;
End;

Procedure Initialize;

Begin;
      CUPSInit;
      VariableInit;
      ParserStr1:='';
      ParserStr2:='';
      parser.init;
      SaveStr:='Data1.Chn    ';
      LoadStr:='Data1.Chn    ';
      Comment1:='';
      Comment2:='';
      AtOutline:=False;
      RadScale:=False;
      DefaultPath:='';
      MouseWhileRunning:=True;
      DemoMode:=FALSE;
      DemoTime:=100;
      MovingFrame:=FALSE;
      FrameVelocity:=1;
      FrameShift:=0;
      TheChain.Initialize(1, 0.1, 0.1, 0.80, 0.38);
      Anly1.Initialize(2, 0.1, 0.55, 0.35, 0.35);
      Anly2.Initialize(3, 0.55, 0.55, 0.35, 0.35);

      Anly1.PlotAxes.Visible:=TRUE;
      Anly1.Display;
      Anly1.LockFocus;
      Anly1.Plottitle.SetStr('Disabled');

      Anly2.PlotAxes.Visible:=TRUE;
      Anly2.Display;
      Anly2.LockFocus;
      Anly2.Plottitle.SetStr('Disabled');


      TheChain.InitialAtoms;
      TheChain.Display;
      YInitVec.Equate(1, YVec);
      VInitVec.Equate(1, VVec);
      CalcEnergy;
      ShowEnergy;


      WITH MainMenu DO
           begin;
                 Init;
                 Column(1, 'File');
                 Row(1, 1, 'About CUPS');
                 Row(1, 2, 'About The Program');
                 Row(1, 3, 'Configuration');
                 Row(1, 4, '-----------------');
                 Row(1, 5, 'Open ...');
                 Row(1, 6, 'Save ...');
                 Row(1, 7, 'Save as');
                 Row(1, 8, '-----------------');
                 Row(1, 9, 'Exit');
                 RowActivate(1, 4, False);
                 RowActivate(1, 8, False);

                 Column(2, 'Parameters');
                 Row(2, 1, 'Number of Atoms and Cells');
                 Row(2, 2, 'Examine Atom');
                 Row(2, 3, 'Examine Cell');
                 Row(2, 4, 'Time');
                 Row(2, 5, 'Scale Chain');
                 Row(2, 6, 'Scale Graph1');
                 Row(2, 7, 'Scale Graph2');

                 Column(3, 'Init');
                 Row(3, 1, 'Zero');
                 Row(3,2, 'Random');
                 Row(3, 3, 'Sine');
                 Row(3, 4, 'Gaussian');
                 Row(3, 5, 'Modulated Gaussian');
                 Row(3, 6, 'Pulse');
                 Row(3, 7, 'Symmetric Pulse');
                 Row(3, 8, 'Modes');
                 Row(3, 9, 'User Defined Function');
                 Row(3, 10, '--------------------');
                 RowActivate(3, 10, False);
                 Row(3, 11, 'Zero Vertical Drift');
                 Row(3, 12, 'Zero Velocity');
                 rowChecked(3,4,true);

                 Column(4, 'Graph-1');
                 Row(4, 1, 'Disable Graph');
                 Row(4, 2, 'Displacement:  Y(t)');
                 Row(4, 3, 'Velocity:      V(t)');
                 Row(4, 4, 'Source Power:  P(t)');
                 Row(4, 5, 'Energy:        E(t)');
                 Row(4, 6, 'Phase Space:   V(y)');
                 Row(4, 7, 'Y1(t) vs Y2(t)');
                 Row(4, 8, 'Space Time- Contour');
                 Row(4, 9,'Space Time- 3D');
                 Row(4,10, 'FFT - y(x)');
                 Row(4,11, 'FFT - y(t)');
                 Row(4,12, '-------------------');
                 Row(4,13, 'Clear Graph');
                 RowActivate(4, 12, False);

                 Column(5, 'Graph-2');
                 Row(5, 1, 'Disable Graph');
                 Row(5, 2, 'Displacement:  Y(t)');
                 Row(5, 3, 'Velocity:      V(t)');
                 Row(5, 4, 'Source Power:  P(t)');
                 Row(5, 5, 'Energy:        E(t)');
                 Row(5, 6, 'Phase Space:   V(y)');
                 Row(5, 7, 'Y1(t) vs Y2(t)');
                 Row(5, 8, 'Space Time- Contour');
                 Row(5, 9,'Space Time- 3D');
                 Row(5,10, 'FFT - y(x)');
                 Row(5,11, 'FFT - y(t)');
                 Row(5,12, '-------------------');
                 Row(5,13, 'Clear Graph');
                 RowActivate(5, 12, False);

                 Column(6, 'Boundary');
                 Row(6,1,'Stationary');
                 Row(6,2,'Moving');

                 Column(7, 'Pref');
                 Row(7, 1, 'Display');
                 Row(7, 2, 'Color');
                 Row(7, 3, 'Files');
                 Row(7, 4, 'Speed');
                 Row(7, 5, 'Sources');
                 Row(7, 6, 'Demo');

           end;
      Anly1.AnalysisPlot:=FFT_x;
      Anly1.ResetAndInit;
      CheckIt(4,True,@Anly1);

      Anly2.AnalysisCounter:=1;
      Anly2.DvecCompress:=1;
      Anly2.DvecCompressCounter:=1;
      CheckIt(5,False,@Anly2);
      Anly2.AnalysisPlot:=YvsTDensity;
      Anly2.Plottitle.SetStr('Space Time: Contour');
      GraphBackColor:=Black;
      Anly2.RefreshAnalysisPlot(True,0,-1,10,1 );
      GraphBackColor:=DarkGray;
      Anly2.AtomAnalysisSelection(@TheChain);
      CheckIt(5,True,@Anly2);

      SetStopKeys;
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

      If TheChain.Expanded then
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

      If not(TheChain.Expanded) and not(Anly1.Expanded) and not(Anly2.Expanded) then
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
                 LoadLine('');
                 LoadLine('           Time > DemoTime        ');
                 LoadLine('');
                 LoadLine('Enable Demo Mode:  #F');
                 LoadLine('        DemoTime:  {     }');
                 LoadLine('');
                 LoadLine('<ESC> will alow you to exit Demo Mode.');
                 LoadLine('                                      ');
                 LoadLine('   [  Ok  ]   [Cancel]    [ Help ]    ');
                 SetHelpFile('Chain.Hlp', 'Demo Help');
                 SetBoolean(1,DemoMode);
                 SetNumber(2,DemoTime);
                 AcceptScreen;
                 Ok:=GetBoolean(3);
                 Cancel:=GetBoolean(4);
                 If Ok then
                   Begin
                   DemoMode:=GetBoolean(1);
                   DemoTime:=Abs(GetNumber(2));
                   End;
                 Done;
           end;
End;

{----------------------------------------------------------------------------}
Procedure ResetAnly(ThePlot: AnlyPlotPtr; Must: Boolean);
Var i,numOfSteps:Integer;
Begin;
 if animationSpeed<=0 Then numOfSteps:=1-AnimationSpeed Else numOfSteps:=1;
     if ThePlot^.Freeze or (thePlot^.AnalysisPlot=Disabled)  then Exit;
      With ThePlot^ do
           begin
                If not(Must) And not( AnalysisPlot in [YvsTDensity, YvsT3D,FFT_T])
                  then Exit;
                InitialTime:=Time;
                Case AnalysisPlot of
                  YvsTDensity, YvsT3D:
                     begin
                     AnalysisCounter:=1;
                     TimeCompress:=1;
                     SkipCounter:=1;
                     DvecCompress:=1;
                     DvecCompressCounter:=1;
                     TheMatrix.Fill(0);
                     OldMatrix.Fill(0);
                     if Visible then Comment(Xo+Width/2-0.15,0.8,' Data has been cleared. ');
                     end;
                   FFT_T:
                      begin
                      AnalysisCounter:=1;
                      TimeCompress:=1;
                      SkipCounter:=1;
                      DvecCompress:=1;
                      DvecCompressCounter:=1;
                      AmpVec.Fill(0);
                      If Visible then Comment(Xo+Width/2-0.15,0.8,' Data has been cleared. ');
                      End;
                   EvsT:
                     Begin;
                     AnalysisCounter:=1;
                     TimeCompress:=1;
                     SkipCounter:=1;
                     DvecCompress:=1;
                     DvecCompressCounter:=1;
                     Evec.Fill(0);
                     KEvec.Fill(0);
                     PEvec.Fill(0);
                     Tvec.Fill(0);
                     With Scale Do RefreshAnalysisPlot(False,0,yMin,time+20*dt*numOfSteps,yMax);
                     If Visible Then Display;
                     end;
                   SvsT:
                     Begin;
                     AnalysisCounter:=1;
                     TimeCompress:=1;
                     SkipCounter:=1;
                     DvecCompress:=1;
                     DvecCompressCounter:=1;
                     For i:=1 to 5 do PowerArray[i].Fill(0);
                     Tvec.Fill(0);
                     With Scale Do RefreshAnalysisPlot(False,0,yMin,time+20*dt*numOfSteps,yMax);
                     If Visible Then Display;
                     end;
                   YvsT,VvsT:
                      begin
                      AnalysisCounter:=1;
                      TimeCompress:=1;
                      SkipCounter:=1;
                      DvecCompress:=1;
                      DvecCompressCounter:=1;
                      tVec.Fill(0);
                      For i:=1 to 5 Do IF AnalyzedAtoms[i]<>nil
                              THEN
                              Begin
                              AnlArray[i].Fill(0);
                              AnlArray[i+5].Fill(0);
                              End;
                      With Scale Do RefreshAnalysisPlot(False,0,yMin,time+20*dt*numOfSteps,yMax);
                      If Visible Then Display;
                      End;
                   VvsY,YvsY:
                      begin
                      AnalysisCounter:=1;
                      TimeCompress:=1;
                      SkipCounter:=1;
                      DvecCompress:=1;
                      DvecCompressCounter:=1;
                      tVec.Fill(0);
                      For i:=1 to 5 Do IF AnalyzedAtoms[i]<>nil
                              THEN
                              Begin
                              AnlArray[i].Fill(0);
                              AnlArray[i+5].Fill(0);
                              End;
                      If Visible Then Display;
                      End;
                   Else
                      begin
                      SkipCounter:=1;
                      DvecCompressCounter:=1;
                      End;
                end; {of Case}

           End;
End;{----------------------------------------------------------------------------}
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

                 SetHelpFile('Chain.Hlp', 'Speed Help');
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
{---------------------------------------------------------------------------------------------}
Procedure SourcePref;
Var
   InptSourceScrn:      TInputScreen;
   Leave,
   Ok, Cancel:          Boolean;

Begin;
      With InptSourceScrn do
           begin;
                 Init;
                 DefineInputPort( 0.30, 0.74, 0.63 ,0.85 );

                 LoadLine('        Source Preferences          ');
                 LoadLine('                                    ');
                 LoadLine('      Source Period =  {      }     ');
                 LoadLine('                                    ');
                 LoadLine('  [  Ok  ]   [Cancel]    [ Help ]   ');

                 SetNumber(1, SourcePeriod);

                 SetHelpFile('Chain.Hlp', 'Source Help');

                 Leave:=False;

                 Repeat
                       AcceptScreen;
                       Ok:=GetBoolean(2);
                       Cancel:=GetBoolean(3);
                       If ((GetNumber(1)<10 ) or (GetNumber(1)>100)) then
                          Announce('The Source Period must be >10 s and <=100 s')
                       else Leave:=True;
                 Until Leave;

                 If Ok then SourcePeriod:=GetNumber(1);

                 Done;
           end;
End;

Procedure ColorPref;

Var
   InptPAtmScrn:        TInputScreen;
   Ok, Cancel:          Boolean;
   TheColour:           Word;

Begin;
      TheColour:=1;
      With InptPAtmScrn do
           begin;
                 Init;
                 DefineInputPort( 0.25, 0.75, 0.38 ,0.90 );

                 LoadLine('        Color Preferences             ');
                 LoadLine('                                       ');
                 LoadLine('#1 All Atoms Same Color. Color={   } ');
                 Loadline('#1 All Cells Same Color.               ');
                 Loadline('#1 Cycle color.                       ');
                 LoadLine('                                       ');
                 LoadLine('Allows the user to change how an atom  ');
                 LoadLine('is colored. Graphs will use the atom  ');
                 LoadLine('color so be careful that you do not   ');
                 LoadLine('set the color to the background!      ');
                 LoadLine('                                       ');
                 LoadLine('   [  Ok  ]   [Cancel]    [ Help ]     ');
                 Case TheChain.Ctype of
                      SameAll:    SetRadioButton('1',1);
                      SameCell:   SetRadioButton('1',2);
                      Cycle:      SetRadioButton('1',3);
                 End;
                 SetNumber(2,TheColour);
                 SetHelpFile('Chain.Hlp', 'colour Help');
                 Repeat
                   AcceptScreen;
                   Ok:=GetBoolean(5);
                   Cancel:=GetBoolean(6);

                   If OK AND NOT((GetNumber(2)>=0 ) AND
                                 (GetNumber(2)<MaxColors))THEN
                     Announce('Color Out of Range. RE-EDIT.');
                 Until ((GetNumber(2)>0 ) AND (GetNumber(2)<MaxColors));

                 If Ok then
                    begin;
                          TheColour:=trunc(GetNumber(2));
                          Case GetRadioButton('1') of
                          1: TheChain.Ctype:=SameAll;
                          2: TheChain.Ctype:=SameCell;
                          3: TheChain.Ctype:=Cycle;
                          End;{of Case}
                          AtomList.ChangeColour(TheChain.Ctype,TheColour);
                    end;

                 Update;
                 Done;
           end;
End;

{----------------------------------------------------------------------------}
Procedure MovingBoundaryPref;
Var
   ReferenceScrn:        TInputScreen;
   Ok, Cancel,OldMovingFrame:          Boolean;

Begin;
OldMovingFrame:=MovingFrame;
      With ReferenceScrn do
           begin;
                 Init;
                 DefineInputPort( 0.25, 0.75, 0.3 ,0.90 );

                 LoadLine('      Reference Frame Pref         ');
                 LoadLine('');
                 LoadLine(' Selecting a moving reference frame');
                 LoadLine(' will cause position and velocity to');
                 LoadLine(' shift as the system evolves.');
                 LoadLine('');
                 LoadLine('Enable MovingFrame:  #F');
                 LoadLine('    Frame Velocity:  {     }');
                 LoadLine('');
                 LoadLine('CAUTION:');
                 LoadLine('Data will be lost as the frame moves.');
                 LoadLine('                                      ');
                 LoadLine('   [  Ok  ]   [Cancel]    [ Help ]    ');
                 SetHelpFile('Chain.Hlp', 'Moving-Frame Help');
                 SetBoolean(1,MovingFrame);
                 SetNumber(2,FrameVelocity);
                 AcceptScreen;
                 Ok:=GetBoolean(3);
                 Cancel:=GetBoolean(4);
                 If Ok then
                   Begin
                   MovingFrame:=GetBoolean(1);
                   FrameVelocity:=GetNumber(2);
                   End;
                 Done;
           end;
If MovingFrame<>oldMovingFrame Then
  Begin
  theChain.Display;
  If Anly1.Freeze=FALSE Then ResetAnly(@Anly1,False);
  If Anly2.Freeze=FALSE Then ResetAnly(@Anly2,False);
  ShowEnergy;
  End;
End;

{----------------------------------------------------------------------------}
Procedure StationaryBoundaryPref;

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
      With InptBoundScrn do
           begin;
                 Init;
                 DefineInputPort( 0.31, 0.67, 0.45 ,0.90 );

                 LoadLine('   Boundary   Conditions     ');
                 LoadLine('                             ');
                 LoadLine('  OverRides Fixed & Free     ');
                 LoadLine('    Periodic       #F        ');
                 LoadLine('                             ');
                 LoadLine('               Left   Right  ');
                 LoadLine('    Fixed       #1     #2    ');
                 Loadline('    Free        #1     #2    ');
                 LoadLine('                             ');
                 LoadLine(' [  Ok  ] [Cancel] [ Help ]  ');
                 SetHelpFile('Chain.Hlp', 'Fixed-Frame Help');

                 Case RightB of
                      Fixed:      SetRadioButton('2',1);
                      Free:       SetRadioButton('2',2);
                      Continuous: SetBoolean(1,True);
                 End;

                 Case LeftB of
                      Fixed:      SetRadioButton('1',1);
                      Free:       SetRadioButton('1',2);
                      Continuous: SetBoolean(1,True);
                 End;

                 AcceptScreen;


                 Ok:=GetBoolean(6);
                 Cancel:=GetBoolean(7);
                 maxNum:=yVec.Length;
                 k:=2;
                 If Ok then
                    begin;
                    Case GetRadioButton('2') of
                          1: RightB:=Fixed;
                          2: RightB:=Free;
                          End;

                    Case GetRadioButton('1') of
                          1: LeftB:=Fixed;
                          2: LeftB:=Free;
                          End;

                    If GetBoolean(1) then
                             begin;
                                   RightB:=Continuous;
                                   LeftB:=Continuous;
                             end;
                    end;
                 Done;
           end;
    AdjustForBoundry;
    AtomList.UpdateList;
    if (oldRightB<>rightB) or (oldLeftB<>leftB) Then
    Begin
      if (anly1.AnalysisPlot in [FFT_x,FFT_V]) OR (anly2.AnalysisPlot in [FFT_x,FFT_V])
       then
          Announce('CAUTION: Check FFT your basis functions using blue button.');
    End;

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
      DefineInputPort(0.19, 0.81, 0.40, 0.80);
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
      {$I+}
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
   i:                   Integer;
   Leave,
   Ok,Cancel:           Boolean;
   Ny,Nv,Nm,Nk1,Nk2,
   Nk3,Nd,Ns:           Real;

   Stype,
   Etype,
   Period,
   SourceVal,
   Storage:             Integer;

   anatom:              BasePtr;


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
                 DefineInputPOrt(0.18,0.81,0.40,0.80);
                 LoadLine('                   Save Screen             ');
                 LoadLine('File Name ="                                   "');
                 LOadLine('NOTE:Drive and Path can be set in Pref|System.');
                 LoadLine('-----------------------------------------------');
                 LoadLine('Comments:                                       ');
                 LoadLine('"                                              "');
                 LoadLine('"                                              "');
                 LoadLine('');
                 LoadLine('    [  Ok  ]        [Cancel]        [ Help ]');
                 SetString(1, SaveStr );
                 SetString(2,Comment1);
                 SetString(3,Comment2);
                 SetHelpFile('Chain.Hlp','Save Help');
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
               msg:='Overwrite?: '+SaveStr;
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
      WriteLn(Fhandle,FileHeader);

      If IOResult=105 then
         begin;
               Announce('You cannot write to that drive or directory.');
               Exit;
         end;

      WriteLn(Fhandle, str1);
      WriteLn(Fhandle,Cell,' ',theChain.Scale.yMin,' ',theChain.Scale.yMax);

      WriteLn(Fhandle, str2);
      WriteLn(Fhandle, AnimationSpeed);

      Writeln(Fhandle,str3);
      WriteLn(Fhandle,Yvec.Length);

      Case LeftB of
           Fixed:       Storage:=1;
           Free:        Storage:=2;
           Continuous:  Storage:=3;
      End;
      WriteLn(Fhandle,str4);
      WriteLn(Fhandle,Storage);

      Case RightB of
           Fixed:       Storage:=1;
           Free:        Storage:=2;
           Continuous:  Storage:=3;
      End;
      WriteLn(Fhandle,str5);
      WriteLn(Fhandle,Storage);
      WriteLn(Fhandle,str6);
      For i:=0 to (Yvec.Length-1) do
          begin;
                AtomList.GetAtom(i,Nm,Ny,Nv,Nk1,Nk2,Nk3,Nd,Ns);
                If AtomList.IsItASource(i) then SourceVal:=1 else SourceVal:=0;
                WriteLn(Fhandle,i,' ',Ny,' ',Nv,' ',Nm,' ',Nk1,' ',Nk2,' ',Nk3,' ',Nd,' ',Ns,' ',SourceVal);
                If SourceVal=1 then
                   begin;
                         WriteLn(Fhandle,str7);
                         AtomList.GetAnAtom(i,anatom);
                         with anatom^.parSource do
                              begin;
                                    Case Kind of
                                         Sine:       Stype:=1;
                                         Gaussian:   Stype:=2;
                                         Pulse:      Stype:=3;
                                         Step:       Stype:=4;
                                         ModGaussian:Stype:=5;
                                         UserDefined:Stype:=6;
                                    End; {of Case}
                                    Case Excite of
                                         Displacement:  Etype:=1;
                                         Force:         Etype:=2;
                                    End;  {of Case}
                                    If Periodic=True then Period:=1 else Period:=0;
                                    WriteLn(Fhandle, Stype,' ',Etype,' ',Freq,' ',Amp,' ',A,' ',Xo,' ',Period);
                                    WriteLn(Fhandle, SourcePeriod);
                                    Writeln(fHandle, funcStr);
                              end;
                   end; {of If...}

          end; {of Loop}
      WriteLn(Fhandle,str8);
      WriteLn(Fhandle,ParserStr1);
      WriteLn(Fhandle,ParserStr2);
      WriteLn(Fhandle,str9);
      Anly1.Archive(fhandle);
      Anly2.Archive(fhandle);
      WriteLn(Fhandle, Comment1);
      WriteLn(Fhandle, Comment2);

      Close(Fhandle);
      {$I+}
      Announce('File Saved.');
End;
{----------------------------------------------------------------------------}
Procedure Load;

PROCEDURE ShowComments;

VAR
  theInputPort:  TInputScreen;

BEGIN
   if (comment1='No Comments in file.') AND (comment2='') Then Exit;
   WITH theInputPort DO BEGIN
      Init;
      DefineInputPort(0.19, 0.81, 0.48, 0.75);
      LoadLine('-----------------------------------------------');
      LoadLine('Comments:                                      ');
      LoadLine(''+Comment1);
      LoadLine(''+Comment2);
      LoadLine('-----------------------------------------------');
      LoadLine('                   [  Ok  ]');

      AcceptScreen;

      done;
   END;
END;

Var
   LoadInputPort:       TInputScreen;
   fHandle:             Text;
   msg:                 String;
   i,Num:               Integer;

   Ny,Nv,Nm,Nk1,Nk2,
   Nk3,Nd,Ns:           Real;

   anatom:              AtomPtr;
   Ok,Cancel:           Boolean;

   Stype,
   Etype,
   Period,
   AtomNumber,
   SourceVal,
   Storage:             Integer;

Procedure CompareFormat( msg1, msg2: String );

Begin;
      If not(msg1=msg2) then
         begin;
               Announce('This file format is not valid.');
               Close(Fhandle);
               exit;
         end;
End;

Begin;
      {$I-}
      LoadStr:=OpenFile('*.chn');
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
        Else if msg= 'CUPS:Chain Ver 3.2' Then version:=3.2
          Else version:=0;   {nothing less than 3.2 counts!}
      If version < 3.1 then
        Begin
        Announce('Incorrect CHAIN format.');
        exit;
        end;
      If version <currentVersion-0.01 Then
        Announce('Caution:  Old WAVE file format.  Not all parameters have been loaded.');
      Loading:=True;
      Time:=0;
      Anly1.ClearAnalysis;
      Anly2.ClearAnalysis;
      CheckIt(4,False,@Anly1);
      CheckIt(5,False,@Anly2);
      for i := 1 to 9 do MainMenu.rowchecked(3,i,false);
      MainMenu.rowchecked(3,9,true);
      Anly1.PlotTitle.Remove;
      Anly2.PlotTitle.Remove;

      Anly1.AnalysisPlot:=Disabled;
      Anly2.AnalysisPlot:=Disabled;

      Anly1.Plottitle.SetStr('Disabled');
      Anly2.Plottitle.SetStr('Disabled');

      Anly1.Freeze:=False;
      Anly2.Freeze:=False;

      Anly1.PlotTitle.SetTitleColor(Magenta);
      Anly2.PlotTitle.SetTitleColor(Magenta);

      For i:=1 to 5 do
          begin;
                Anly1.AnalyzedAtoms[i]:=Nil;
                Anly2.AnalyzedAtoms[i]:=Nil;
          end;
      Anly1.AnalysisCounter:=1;
      Anly2.AnalysisCounter:=1;
      Anly1.TimeCompress:=1;
      Anly2.TimeCompress:=1;

      Anly1.DvecCompress:=1;
      Anly1.DvecCompressCounter:=1;
      Anly2.DvecCompress:=1;
      Anly2.DvecCompressCounter:=1;

      If Anly1.PlotAxes.Visible then
         begin
              Anly1.PlotAxes.Remove;
              Anly1.PlotAxes.Visible:=False;
         end;
      Anly1.ClearScreen;

      If Anly2.PlotAxes.Visible then
         begin
              Anly2.PlotAxes.Remove;
              Anly2.PlotAxes.Visible:=False;
         end;
      Anly2.ClearScreen;

      i:=0;
      TheChain.Remove;
      AtomList.Done(True);
      SourceList.Done(False);
      AtomList.Init;
      SourceList.Init;

      ReadLn(Fhandle,msg);
      CompareFormat(msg,str1);
      ReadLn(Fhandle,Cell,theChain.Scale.yMin,theChain.Scale.yMax);
      ReadLn(Fhandle,msg);
      CompareFormat(msg,str2);
      ReadLn(Fhandle,AnimationSpeed);

      ReadLn(Fhandle,msg);
      CompareFormat(msg,str3);
      ReadLn(Fhandle,AtomNumber);

      YVec.Resize(AtomNumber);
      VVec.Resize(AtomNumber);
      YInitVec.Resize(AtomNumber);
      VInitVec.Resize(AtomNumber);
      MVec.Resize(AtomNumber);
      K1Vec.Resize(AtomNumber);
      K2Vec.Resize(AtomNumber);
      K3Vec.Resize(AtomNumber);
      KDVec.Resize(AtomNumber);
      KSVec.Resize(AtomNumber);
      Avec.ReSize(AtomNumber);
      Yold.Resize(AtomNumber);

      ReadLn(Fhandle,msg);
      CompareFormat(msg,str4);
      ReadLn(Fhandle,Storage);
      Case Storage of
           1:LeftB:=Fixed;
           2:LeftB:=Free;
           3:LeftB:=Continuous;
      End;

      ReadLn(Fhandle,msg);
      CompareFormat(msg,str5);
      ReadLn(Fhandle,Storage);
      Case Storage of
           1:RightB:=Fixed;
           2:RightB:=Free;
           3:RightB:=Continuous;
      End;

      Readln(Fhandle,msg);
      CompareFormat(msg,str6);
      For i:=0 to (AtomNumber-1) do
            begin;
                  New(anAtom);
                  ReadLn(Fhandle,Num,Ny,Nv,Nm,Nk1,Nk2,Nk3,Nd,Ns,SourceVal);
                  anAtom^.Initialize(Num, Ny, @TheChain);
                  anAtom^.ChangeData( Nm, Nv, Nk1, Nk2, Nk3, Nd, Ns);
                  If SourceVal=1 then
                     begin;
                           Readln(Fhandle,msg);
                           CompareFormat(msg,str7);
                           SourceList.AddItem(anatom);
                           anatom^.source:=True;
                           With anatom^.parsource do
                                begin;
                                      ReadLn(Fhandle,Stype,Etype,Freq,Amp,A,Xo,Period);
                                      ReadLn(Fhandle,SourcePeriod);
                                      Case Stype of
                                           1:kind:=Sine;
                                           2:kind:=Gaussian;
                                           3:kind:=Pulse;
                                           4:kind:=step;
                                           5:kind:=ModGaussian;
                                           6:kind:=UserDefined;
                                      End; {of Case}
                                      Case Etype of
                                           1:excite:=Displacement;
                                           2:excite:=Force;
                                      End; {of Case}
                                      If Period=1 then Periodic:=True else Periodic:=False;
                                      If version>3.25 Then Readln(Fhandle, funcStr);
                                end; {of with}
                     end; {of If...}
                  AtomList.AddItem(anAtom);
            end;{of loop}
      AtomList.ChangeColour(TheChain.Ctype,i);
      AtomList.UpdateDvec;
      With TheChain.Scale do TheChain.SetScale(xMin, yMin, AtomNumber-1, yMax);
      TheChain. plotTitle.SetStr(num2Str(AtomNumber-2,4)+' Atom Chain');
      TheChain.Display;
      Readln(Fhandle,msg);
      CompareFormat(msg,str8);
      Readln(Fhandle,parserStr1);
      Readln(Fhandle,parserStr2);
      Readln(Fhandle,msg);
      CompareFormat(msg,str9);
      Anly1.Unarchive(fHandle);
      Anly2.Unarchive(fHandle);
      ReadLn(Fhandle, Comment1);
      ReadLn(Fhandle, Comment2);

      If (Comment1='') and (Comment2='') then Comment1:='No Comments in file.';

      Close(Fhandle);
      {$I+}
      AtomList.UpdateDvec;
      VInitVec.Equate( 1, Vvec );
      YInitVec.Equate( 1, Yvec );
      CalcEnergy;
      anly1.resetAndInit;
      anly2.resetAndInit;
      ShowComments;
      saveStr:=loadStr;
      mainmenu.Display;
      Loading:=False;
End;
{----------------------------------------------------------------------------}
Procedure Reset3D(ThePlot: AnlyPlotPtr; Must: Boolean);
Begin;
      With ThePlot^ do
           begin
                If not(Must) then
                   If not(AnlyType=ThreeD) then Exit;

                If not(Freeze) then
                   begin
                        PlotTitle.SetTitleColor(Magenta);
                        AnalysisCounter:=1;
                        InitialTime:=Time;
                        If (AnlyType=ThreeD) then
                           begin
                                TheMatrix.Fill(0);
                                OldMatrix.Fill(0);
                           end;
                   end;

                TimeCompress:=1;
                OnlyOnce:=True;
                SkipCounter:=1;
           end;
End;
{----------------------------------------------------------------------------}
Procedure HandleMenu;

Var
   Storage:     Boolean;
   i:           ShortInt;
   HelpScreen:  TInputScreen;

Begin;
     If theChain.Expanded Then theChain.Expand;
     If anly1.Expanded Then anly1.Expand;
     If anly2.Expanded Then anly2.Expand;
     With MainMenu DO
           CASE colChosen OF

                1:
                Case rowChosen of
                   1: AboutCups;
                   2: ShowHelp('Chain.Hlp','About Chain');
                   3: Configuration;
                   5: Load;
                   6: Save(False); {Save}
                   7: Save(True);  {Save as}
                   9: Begin;
                            MainMenu.Done;
                            VariableDone;
                            Anly1.Done;
                            Anly2.Done;
                            TheChain.Done;
                            CupsDone;
                            Halt;
                      End;
                   End;

                2:
                  Case rowChosen of
                       1: Begin;
                                CheckIt(4,False,@Anly1);
                                CheckIt(5,False,@Anly2);
                                Anly1.PlotTitle.Remove;
                                Anly2.PlotTitle.Remove;

                                For i:=1 to 5 do Anly1.AnalyzedAtoms[i]:=Nil;
                                For i:=1 to 5 do Anly2.AnalyzedAtoms[i]:=Nil;

                                Anly1.DvecCompress:=1;
                                Anly1.DvecCompressCounter:=1;
                                Anly2.DvecCompress:=1;
                                Anly2.DvecCompressCounter:=1;

                                Anly1.AnalysisPlot:=Disabled;
                                Anly1.Freeze:=False;
                                Anly1.PlotTitle.SetTitleColor(Magenta);
                                Anly1.Plottitle.SetStr('Disabled');
                                If Anly1.PlotAxes.Visible then
                                     begin
                                          Anly1.PlotAxes.Remove;
                                          Anly1.PlotAxes.Visible:=False;
                                     end;
                                Anly1.ClearScreen;

                                Anly2.AnalysisPlot:=Disabled;
                                Anly2.Freeze:=False;
                                Anly2.PlotTitle.SetTitleColor(Magenta);
                                Anly2.Plottitle.SetStr('Disabled');
                                If Anly2.PlotAxes.Visible then
                                     begin
                                          Anly2.PlotAxes.Remove;
                                          Anly2.PlotAxes.Visible:=False;
                                     end;
                                Anly2.ClearScreen;

                                TheChain.AddAtoms;
                                If TheChain.Visible Then Begin TheChain.LockFocus; ShowEnergy; End;
                                CheckIt(4,True,@Anly1);
                                CheckIt(5,True,@Anly2);
                          End;
                       2: AtomList.GetAtomInspector;
                       3: Begin
                          TheChain.CellInformation;
                          If TheChain.Visible Then Begin TheChain.LockFocus; ShowEnergy; End;
                          End;

                       4: begin;
                                TheChain.SetTimeIncrement;
                                SetStopKeys;
                                StopKeys.Display;
                                If TheChain.Visible then
                                   begin;
                                         TheChain.LockFocus;
                                         ShowEnergy;
                                   end;
                                Reset3D(@Anly1,True);
                                Reset3D(@Anly2,True);
                                Anly1.AnalysisCounter:=1;
                                Anly2.AnalysisCounter:=1;
                                Anly1.TimeCompress:=1;
                                Anly2.TimeCompress:=1;
                          end;
                       5: TheChain.ChangeScale;
                       6: Anly1.ChangeScale;
                       7: Anly2.ChangeScale;
                  End;

                3:Begin
                          IF yVec.Length<=3 THEN
                            BEGIN
                            Announce('You must have more than 1 atom to define a function.');
                            Exit;
                            END;
                          Storage:=ShowEnvelope;
                          ShowEnvelope:=False;
                          if RowChosen<10 then begin
                             for i := 1 to 9 do RowChecked(3,i,false);
                             rowChecked(3,RowChosen,true);
                          end;
                          Case rowChosen of
                               1: Zero;
                               2: RandomPosVel;
                               3: begin;
                                        Anly2.Remove;
                                        SineFun;
                                        Anly2.Display;
                                  end;
                               4: begin;
                                        Anly2.Remove;
                                        Gauss_Pulse_SymPulse(Gauss);
                                        Anly2.Display;
                                  end;
                               5: begin;
                                        Anly2.Remove;
                                        ModulatedGaussian;
                                        Anly2.Display;
                                  end;
                               6: begin;
                                        Anly2.Remove;
                                        Gauss_Pulse_SymPulse(Pul);
                                        Anly2.Display;
                                  end;
                               7: begin;
                                        Anly2.Remove;
                                        Gauss_Pulse_SymPulse(SymPul);
                                        Anly2.Display;
                                  end;
                               8: Modes;
                               9: UserDefinedInit;
                               11:begin
                                       SetCMFrame;
                                       TheChain.Display;
                                       Exit;
                                  end;
                               12:begin
                                       ZeroVelocity;
                                       TheChain.Display;
                                       Exit;
                                  end;
                          End; {of Case}

                          Time:=0;
                          Reset3D(@Anly1,True);
                          Reset3D(@Anly2,True);

                          Anly1.DvecCompress:=1;
                          Anly1.DvecCompressCounter:=1;
                          Anly2.DvecCompress:=1;
                          Anly2.DvecCompressCounter:=1;

                          TheChain.LockFocus;
                          YInitVec.Equate(1, YVec);
                          VInitVec.Equate(1, VVec);
                          SetStopKeys;
                          StopKeys.Display;
                          Update;
                          IF TheChain.Visible Then ShowEnergy;
                          ShowEnvelope:=Storage;
                   End;

           4:begin;
                    CheckIt(4,False,@Anly1);
                    Anly1.Freeze:=False;
                    Anly1.PlotTitle.SetTitleColor(Magenta);
                    Anly1.PlotTitle.Remove;

                    If not(rowChosen=13) then Anly1.ClearAnalysis;

                    Case rowChosen of
                         1: begin;
                                  Anly1.AnalysisPlot:=Disabled;
                                  anly1.ResetAndInit;
                            end;
                         2: begin;
                                  Anly1.AnalysisPlot:=YvsT;
                                  Anly1.ResetAndInit;
                            end;
                         3: begin;
                                  Anly1.AnalysisPlot:=VvsT;
                                  Anly1.ResetAndInit;
                            end;
                         4: begin;
                                  Anly1.AnalysisPlot:=SvsT;
                                  Anly1.ResetAndInit;
                            end;
                         5: begin;
                                  Anly1.AnalysisPlot:=EvsT;
                                  Anly1.ResetAndInit;
                           end;
                         6: begin;
                                  Anly1.AnalysisPlot:=VvsY;
                                  Anly1.ResetAndInit;
                            end;
                         7:begin;
                                 Anly1.AnalysisPlot:=YvsY;
                                 Anly1.ResetAndInit;
                           end;
                         8:begin;
                                  Anly1.AnalysisPlot:=YvsTDensity;
                                  Anly1.resetAndInit;
                            end;
                         9:begin;
                                   Anly1.AnalysisPlot:=YvsT3D;
                                   Anly1.ResetAndInit;
                            end;
                         10:begin;
                                  Anly1.AnalysisPlot:=FFT_x;
                                  Anly1.ResetAndInit;
                            end;
                         11:begin;
                                  Anly1.AnalysisPlot:=FFT_t;
                                  Anly1.ResetAndInit;
                            end;
                         13: begin;
                                  If (Anly1.AnlyType=ThreeD) then GraphBackColor:=Black;
                                  Anly1.ClearScreen;
                                  If GraphBackColor=Black then GraphBackColor:=DarkGray;
                            end;
                    End;
                    Anly1.AnalysisCounter:=1;
                    Anly1.DvecCompress:=1;
                    Anly1.DvecCompressCounter:=1;
                    MainMenu.Display;
                    CheckIt(4,True,@Anly1);
           End;


           5: begin;
                    CheckIt(5,False,@Anly2);
                    Anly2.Freeze:=False;
                    Anly2.PlotTitle.SetTitleColor(Magenta);
                    Anly2.PlotTitle.Remove;
                    If not(rowChosen=13) then Anly2.ClearAnalysis;

                    Case rowChosen of
                         1: begin;
                                  Anly2.AnalysisPlot:=Disabled;
                                  Anly2.ResetAndInit;
                            end;
                         2: begin;
                                  Anly2.AnalysisPlot:=YvsT;
                                  Anly2.ResetAndInit;
                            end;
                         3: begin;
                                  Anly2.AnalysisPlot:=VvsT;
                                  Anly2.ResetAndInit;
                            end;
                         4: begin;
                                  Anly2.AnalysisPlot:=SvsT;
                                  Anly2.ResetAndInit;
                            end;
                         5: begin;
                                  Anly2.AnalysisPlot:=EvsT;
                                  Anly2.ResetAndInit;
                            end;
                         6: begin;
                                  Anly2.AnalysisPlot:=VvsY;
                                  Anly2.ResetAndInit;
                            end;
                         7: begin;
                                   Anly2.AnalysisPlot:=YvsY;
                                   Anly2.ResetAndInit;
                             end;
                         8:begin;
                                  Anly2.AnalysisPlot:=YvsTDensity;
                                  Anly2.ResetAndInit;
                            end;
                         9:begin;
                                  Anly2.AnalysisPlot:=YvsT3D;
                                  Anly2.ResetAndInit;
                            end;

                         10:begin;
                                  Anly2.AnalysisPlot:=FFT_x;
                                  Anly2.ResetAndInit;
                            end;
                         11:begin;
                                  Anly2.AnalysisPlot:=FFT_t;
                                  Anly2.ResetAndInit;
                            end;
                         13: begin
                                 If (Anly2.AnlyType=ThreeD) then GraphBackColor:=Black;
                                 Anly2.ClearScreen;
                                 If GraphBackColor=Black then GraphBackColor:=DarkGray;
                            end;
                    End;
                    Anly2.AnalysisCounter:=1;
                    Anly2.DvecCompress:=1;
                    Anly2.DvecCompressCounter:=1;

                    MainMenu.Display;

                    CheckIt(5,True,@Anly2);
           End;

                6:  Case rowChosen Of
                    1:StationaryBoundaryPref;
                    2:MovingBoundaryPref;
                    End;

                7: Case rowChosen of
                   1: TheChain.ChangeAttributes;
                   2: ColorPref;
                   3: FilesPref;
                   4: SpeedPref;
                   5: SourcePref;
                   6: DemoPref;
                End;

      End;
End;
{----------------------------------------------------------------------------}
Procedure HandleRunKeys( key: Byte );

Begin;
      Case Key of
           1: ShowHelp('Chain.HLP','Run Help');
           2: Running:=False;
           3: begin;
                    ShowV:=Not(ShowV);
                    SetRunKeys;
                    SetStopKeys;
                    RunKeys.Display;
                    If theChain.Visible Then TheChain.Display;
              end;
           4: begin;
                    TPlus:=Not(TPlus);
                    If Tplus then dt:=abs(dt) else dt:=-abs(dt);
                    SetRunKeys;
                    SetStopKeys;
                    RunKeys.Display;
                    ResetAnly(@Anly1,False);
                    ResetAnly(@Anly2,False);
              end;
           5: begin;
                    If (AnimationSpeed <=  99) then Dec(AnimationSpeed);
                    If Anly1.Freeze=FALSE Then ResetAnly(@Anly1,False);
                    If Anly2.Freeze=FALSE Then ResetAnly(@Anly2,False);
              end;
           6: begin;
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
   TimeRelated1,
   TimeRelated2,
   isStable:        Boolean;

Procedure RunIt;

Procedure RescaleAnalysisPlots( ThePlot: AnlyPlotPtr );

Var
   xNew:        Real;

 Begin;
 if ThePlot^.Freeze or (thePlot^.AnalysisPlot=Disabled)  then Exit;
       With ThePlot^ do
            begin;
                  If  not(Visible) or (not(AnlyType=Dynamic) and not(AnlyType=VersusTime)) then exit;
                  If (AnalysisPlot=YvsY) or (AnalysisPlot=VvsY) then exit;
                  If {(AnalysisCounter<1) or }(AnalysisCounter>1024) then exit;

                  With Scale do
                       begin    {Changed by wc  10/11/93}
                            If (Time>xMax) then
                               begin
                               xNew:=xMax+(xMax-xMin);
                               if (dt<0) and (time<xNew) then xNew:=Time;
                               RefreshAnalysisPlot(False,xMin,yMin,xNew,yMax);
                               end;
                            If (Time<xMin) then
                               begin
                               xNew:=xMin-(xMax-xMin);
                               if (dt>0) and (time>xnew) Then xNew:=Time;
                               RefreshAnalysisPlot(False,xNew,yMin,xMax,yMax);
                               end;
                       end;  {of With Scale}
            end; {of With ThePlot^.}
 End;

Begin;
      Running:=True;

      If not(SingleStep) then
         begin;
           SetRunKeys;
           Runkeys.Display;
           If DemoMode
               Then
                Begin
                Message('----------------Running in DemoMode. <ESC> to exit.-------------------------');
                RunKeys.Clear;
                End
               Else
                Message('----------------------------------Running-----------------------------------');
           If TheChain.Visible then TheChain.ButtonList.Remove;
         end;
      If Not MouseWhileRunning
        Then HideCursor
        Else MouseBorders(0, getMaxy-GetMaxY Div 10, GetMaxX,GetMaxY);
                    TheChain.LockFocus;
                    AtomList.UpdateDvec;{make sure things are current}

                    If Time=0 then
                       begin;
                             VInitVec.Equate( 1, VVec );
                             YInitVec.Equate( 1, YVec );
                       end;
                    If not(Tplus) then dt:=-abs(dt);

                    If (Anly1.AnalysisCounter=1) then
                       Anly1.RegisterDataPoint;
                    If (Anly2.AnalysisCounter=1) then
                       Anly2.RegisterDataPoint;

                    While Running do
                          begin;
                                If (AnimationSpeed < 0) then isStable:=TheChain.StepRun(1-AnimationSpeed)
                                   else
                                       begin;
                                             isStable:=TheChain.StepRun(1);
                                             If not(SingleStep) then
                                                Delay(10*AnimationSpeed);
                                       end;
                                 AtomList.Display;  {AtomList.Display will use DVectors for speed if running is True.}
                                 CalcEnergy;
                                IF (TheChain.Visible) THEN ShowEnergy;
                                Anly1.UpdateAnalysisPlots;
                                Anly2.UpdateAnalysisPlots;
                                RescaleAnalysisPlots(@Anly1);
                                RescaleAnalysisPlots(@Anly2);
                                if DemoMode and (Time>DemoTime)
                                  then HandleStopKeys(5);
                                If not(SingleStep)
                                  THEN
                                     BEGIN
                                     CheckForEvents;
                                     If Event.KeyPressed and DemoMode then
                                       Begin
                                       DemoMode:=False;
                                       Running:=False;
                                       Beep;
                                       end;
                                     If RunKeys.Pressed(Key) and Not DemoMode then HandleRunKeys(Key);
                                     END
                                   ELSE
                                     begin
                                         SingleStep:=False;
                                         Running:=False;
                                     end;
                                   if Not isStable Then
                                     Begin
                                     Running:=FALSE;
                                     Announce('The system has become unstable.  Check you parameter values.');
                                     End;
                          end;
                    dt:=abs(dt);
                    Message('');
                    MainMenu.Display;
                    if Not MouseWhileRunning
                      Then ShowCursor
                      Else MouseBorders(0, 0, GetMaxX,GetMaxY);
                    SetStopKeys;
                    StopKeys.Display;
                    If TheChain.Visible then TheChain.ButtonList.Display;
End;


Begin;
      Case Key of
           1: ShowHelp('Chain.HLP','Global Help');
           2: RunIt;
           3: begin;
                  SingleStep:=True;
                  RunIt;
              end;
           4: begin;
                    TPlus:=Not(TPlus);
                    SetStopKeys;
                    StopKeys.Display;
                    ResetAnly(@Anly1,False);
                    ResetAnly(@Anly2,False);
              end;
           5: begin; {rest to t=0 conditions}
                    VVec.Equate( 1, VInitVec );
                    YVec.Equate( 1, YInitVec );

                    Time:=0;
                    TheChain.LockFocus;
                    AtomList.UpdateList;
                    Update;
                    TheChain.LockFocus;
                    CalcEnergy;
                    if TheChain.Visible Then ShowEnergy;
                    SetStopKeys;
                    StopKeys.Display;
                    If Anly1.Freeze=False Then
                      Begin
                      ResetAnly(@Anly1,True);
                      Anly1.RegisterDataPoint;
                      End;
                    If Anly2.Freeze=False Then
                      Begin
                      ResetAnly(@Anly2,True);
                      Anly2.RegisterDataPoint;
                      End;
              end;
           6: begin;
                    CheckForEvents;
                    If MainMenu.Chosen THEN HandleMenu;
              end;

      End;
End;
{----------------------------------------------------------------------------}
Procedure FindScreenCoordinates(num: Integer; thePlot: PlotPtr; colour: Word );
                                                   {add colour variable}
Const
     Side = 5;

Var
   x,y,button:  Integer;
   xSet,ySet,
   xRub,yRub,
   xPos,yPos:   Real;
   Inside:      Boolean;

Begin;
      If thePlot^.expanded then num:=19;
      HideCursor;
      MousePosn(xPos,yPos,1,thePlot^.SclNum,Inside);
      If ((xPos<0.01) and (xPos>-0.01)) then xSet:=0 else xSet:=xPos;
      If ((yPos<0.01) and (yPos>-0.01)) then ySet:=0 else ySet:=yPos;
      Print(1,num,'Position = ('+Num2Str(xSet,5)+' '+Num2Str(ySet,5)+')');
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
                     If ((xPos<0.01) and (xPos>-0.01)) then xSet:=0 else xSet:=xPos;
                     If ((yPos<0.01) and (yPos>-0.01)) then ySet:=0 else ySet:=yPos;
                     RubOut(12,num,15,colour);
                     Print(12,num,'('+Num2Str(xSet,5)+' '+Num2Str(ySet,5)+')');
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
      RubOut(1,num,26,colour);
End;
{----------------------------------------------------------------------------}
Procedure ButtonSetUp( ThePlot: Plot; Screen: Boolean; Graph: PlotType );

Var
   thecolour:   Word;

Begin;
      If ThePlot.PlotSclBut.MouseInside then ThePlot.PlotSclBut.Action
         else
      If ThePlot.ExpandBut.MouseInside then ThePlot.ExpandBut.Action
         else
      If ThePlot.AttrBut.MouseInside then ThePlot.AttrBut.Action
         else
      If Screen and (Event.MouseClicked) then
         begin;
               If not(ThePlot.PlotAxes.Visible) then exit;
               If (Graph=YvsTdensity) then thecolour:=black else thecolour:=DarkGray;
               FindScreenCoordinates(8,@ThePlot,thecolour);
         end;
End;
{----------------------------------------------------------------------------}
Procedure DragFFT( theplot: AnlyPlotPtr);

Begin
     If theplot^.Visible AND (theplot^.AnalysisPlot in [FFT_x,FFT_V,FFT_T]) then
             theplot^.Display;
End;
{----------------------------------------------------------------------------}
Begin;
      Initialize;
      MainMenu.Display;
      StopKeys.Display;
      ShowHelp('Chain.Hlp','Initial Screen');
                                        {Isolate Mouse Events}
      ClearMKBuffers;
      Repeat
            CheckForEvents;
            Region:=MouseRegion;
            If StopKeys.Pressed(key) then HandleStopKeys(key);
            If DemoMode then HandleStopKeys(2);
            Case Region of
                 1: {Hot Key Strip};
                 2:begin;
                         ButtonSetUp( TheChain ,False, Disabled);
                         AtomList.ReturnClickedItem( ItemClicked );
                         If (ItemClicked <> Nil) and Event.MouseClicked then
                            begin
                                 ItemClicked^.Drag;
                                 DragFFT(@Anly1);
                                 DragFFT(@Anly2);
                            end;
                         If (ItemClicked <> Nil) and Event.DoubleClicked then
                            begin
                                 ItemClicked^.Inspector;
                                 DragFFT(@Anly1);
                                 DragFFT(@Anly2);
                            end;
                         If (ItemClicked = Nil) and Event.MouseClicked then
                            FindScreenCoordinates(9,@TheChain,DarkGray);
                   end;
                 3:ButtonSetUp( Anly1 ,True, Anly1.AnalysisPlot);
                 4:ButtonSetUp( Anly2 ,True, Anly2.AnalysisPlot);
                 5:If MainMenu.Activated then HandleMenu;
            end;
      Until False;
END.
{----------------------------------------------------------------------------}
