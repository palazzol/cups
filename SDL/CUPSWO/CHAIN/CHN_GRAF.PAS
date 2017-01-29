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

Unit Chn_Graf;
{$R-}
Interface
Uses Chn_Plot,Chn_Base,CHN_LST,Chn_Gbl,
     Cups,CupsGui,CUPSmupp,CupsGrph,CupsFunc,CupsProc,Crt,Graph;

Type

ChainPlotPtr=^ChainPlot;
ChainPlot=Object(Plot)
    Ctype:            ColourType;
    Constructor Initialize( ISclNum: Byte; x, y, w, h: Real );
    Destructor Done;         Virtual;
    Procedure ChangeScale;   Virtual;
    Procedure Replot;        Virtual;   {Draws inside the plot}
    Procedure AddAtoms;
    Procedure InitialAtoms;
    Procedure CellInformation;
    Function  StepRun( N: Word ):Boolean;
    Procedure ChangeAttributes;   Virtual;
    Procedure Display;            Virtual;
    Procedure Remove;             Virtual;
    Procedure SetTimeIncrement;
End;

Var
  TheChain:              ChainPlot;

Implementation

Uses Chn_Fun;

Constructor ChainPlot.Initialize( ISclNum: Byte; x, y, w, h: Real);
Begin
     Plot.Initialize( ISclNum,x, y, w, h);
     HasAtoms:=True;
     AtomList.Init;
     PlotSclBut.Initialize(@Self);
     ExpandBut.Initialize(@Self);
     AttrBut.Initialize(@Self);
     SetScale( 0, -1, (InitNum-1), 1 );
     PlotAxes.ShowCell:=True;
     Ctype:=Cycle;
     plotTitle.SetStr(num2Str(InitNum-2,3)+' Atom Chain');
End;

Destructor ChainPlot.Done;
Begin
AtomList.Done(True);
Plot.done;
End;
Procedure ChainPlot.Replot;
Begin
  Atomlist.Display;
End;

Procedure ChainPlot.SetTimeIncrement;

Var
   TimeInptScrn:        TInputScreen;
   Leave,
   Ok,Cancel:           Boolean;

Begin;
      With TimeInptScrn do
           begin;
                 Init;
                 DefineInputPort( 0.43, 0.66, 0.64 ,0.90 );
                 LoadLine('  Time Increment  ');
                 LoadLine('                  ');
                 LoadLine(' Time = {      }  ');
                 LoadLine('  dT  = {      }  ');
                 LoadLine('                  ');
                 LoadLine('[  Ok  ] [Cancel] ');

                 SetNumber(1,Time);
                 SetNumber(2,dT);

                 Leave:=False;
                 Repeat
                       AcceptScreen;
                       If not(GetNumber(2)>0) then
                          Announce('The time incrment must be greater than zero.')
                       else Leave:=True;
                 Until  Leave;

                 Ok:=GetBoolean(3);
                 Cancel:=GetBoolean(4);

                 If Ok then
                    begin;
                          Time:=GetNumber(1);
                          dT:=GetNumber(2);
                    end;

                 Done;
           end;
End;

Procedure ChainPlot.AddAtoms;

Var
   anAtom:           AtomPtr;
   num:              Integer;
   i:              Word;
   InptAtmScreen:    TInputScreen;
   Ok,Cancel,Top:    Boolean;
   MemReq,MaxBlock:  String;
   addedMemory:      LongInt;

Begin;
      If ((Yo + height) < 0.6) then top:=true
                               else top:=false;
      i:=0;
      With InptAtmScreen do
           begin;
                 Init;
                 Case Top of
                      True:  DefineInputPort( 0.31, 0.70, 0.56 ,0.90 );
                      False: DefineInputPort( 0.31, 0.70, 0.16 ,0.50 );
                 End;
                 Repeat
                       LoadLine('        Atom Input          ');
                       LoadLine('                            ');
                       LoadLine('   Number of Atoms = {    } ');
                       Loadline('                            ');
                       LoadLine('   Atoms Per Cell  = {    } ');
                       Loadline('                            ');
                       LoadLine(' [  Ok  ] [Cancel] [ Help ] ');
                       LoadLine('                            ');
                       SetNumber(1, (Yvec.Length-2) );
                       SetNumber(2, Cell );
                       SetHelpFile('Chain.Hlp', 'Num Help');
                       AcceptScreen;

                       Num:=round(GetNumber(1)+2);
                       Cell:=round(GetNumber(2));
                       Ok:=GetBoolean(3);
                       Cancel:=GetBoolean(4);
                       If (Cell < 1) or (Cell > 10) then
                          Begin Announce('Atoms/Cell must be >1 and < 11'); OK:=FALSE  END;
                       If (Num<0) or (Num>258) Then
                          BEGIN Announce('The Number of Atoms must be >0 and <=256'); OK:=FALSE END;
                       If (Cell>(Num-2)) then
                          Announce('The number of atoms must be greater than the number of atoms in a cell.');
                 Until (Cell > 0) and (Cell < 11) AND (Num>0) AND (Num<259) AND (Cell<=(Num-2));
                 Done;
           end;
      IF OK and (num>yVec.Length) Then {Ckeck for memory.  Each new atom takes about 700 Byte}
        Begin
        addedMemory:=(num-yVec.Length);
        addedMemory:=700*addedMemory;
        If addedMemory>MaxAvail Then
           BEGIN
           Announce('Not enough memory for this many atoms!');
           Str(MaxAvail:10,MaxBlock);
           OK:=FALSE;
           Cancel:=TRUE;
           END
        End;
      If Ok then
        IF (num<>yVec.Length) THEN
         begin;
               If not(Int((Num-2)/Cell)=(Num-2)/Cell) then
                  Announce('Caution: Your chain has a partial cell at the end.');
               AtomList.Done(True);
               SourceList.Done(False);

               AtomList.Init;
               SourceList.Init;

               LockFocus;
               for i:=1 to (Num) do
                     Begin
                     New(anatom);
                     anAtom^.Initialize(i-1, 0, @self);
                     anAtom^.ChangeData( 1, 0, 0.5, 0, 0, 0, 0);
                     AtomList.AddItem(anAtom);
                     End;
               YVec.Resize(Num);
               VVec.Resize(Num);
               YInitVec.Resize(Num);
               VInitVec.Resize(Num);
               MVec.Resize(Num);
               K1Vec.Resize(Num);
               K2Vec.Resize(Num);
               K3Vec.Resize(Num);
               KDVec.Resize(Num);
               KSVec.Resize(Num);
               Avec.ReSize(Num);
               Yold.Resize(Num);

               PlotAxes.Remove;
               plotTitle.Remove;
               plotTitle.SetStr(num2Str(Num-2,4)+' Atom Chain');
               plotTitle.Display;
               With Scale do SetScale(xMin, yMin, Num-1, yMax);

               Ctype:=Cycle;
               AtomList.ChangeColour(Ctype,1);
               time:=0;
               Update;
               AtomList.UpdateDvec;
         end ELSE Update;
End;

Procedure ChainPlot.CellInformation;

Var
   i,j,CNum,CellNum:    Integer;
   DaCelInptScrn:       TInputScreen;
   Ok,Cancel,All,Leave: Boolean;
   nm,nl,nq,nc,ns,nd,
   ny,nv:               Real;

Begin;
      With DaCelInptScrn do
           begin;
                 Init;
                 DefineInputPort( 0.40, 0.65, 0.64 ,0.90 );
                 LoadLine('   Examine Cell    ');
                 LoadLine('      Number       ');
                 LoadLine('      {    }       ');
                 LoadLine('                   ');
                 LoadLine(' [  Ok  ] [Cancel] ');

                 SetNumber(1, 1);
                 Repeat
                       AcceptScreen;
                       CellNum:=trunc(GetNumber(1));

                       If ((CellNum>0) and (CellNum<=((Yvec.Length-2)/Cell))) then
                          Leave:=True
                       else Announce('That Cell Number is out of Range.');
                 Until Leave=True;

                 Ok:=GetBoolean(2);
                 Cancel:=GetBoolean(3);

                 Done;
            end;
            If Cancel then exit;

      Leave:=False;

      With DaCelInptScrn do
           begin;
                 Init;
                 DefineInputPort( 0.10, 0.92, 0.10 ,0.90 );
           LoadLine('                    Cell  Information  Screen                   ');
           Loadline('                                                                ');
           LoadLine('         |----------Force--Constants---------|                  ');
           LoadLine(' Mass    Linear   Quad   Cubic   Self  Damping  Y-Value  V-Value');
           For i:=1 to Cell do
           LoadLine(' {    } {     } {     } {     } {    } {     } {     }  {     } ');
           Loadline('                                                                ');
           Loadline('         #1   Apply to Entire Chain                             ');
           Loadline('         #1   Apply to Single Cell Number Only:   {   }         ');
           Loadline('                                                                ');
           LoadLine('            [  Ok  ]       [Cancel]        [ Help ]             ');
           LoadLine('                                                                ');

                        j:=0;
                        For i:=((CellNum-1)*Cell+1) to (CellNum*Cell) do
                            begin;
                                  AtomList.GetAtom(i,Nm,Ny,Nv,Nl,Nq,Nc,Nd,Ns);
                                  SetNumber(1+j, Nm);
                                  SetNumber(2+j, Nl);
                                  SetNumber(3+j, Nq);
                                  SetNumber(4+j, Nc);
                                  SetNumber(5+j, Ns);
                                  SetNumber(6+j, Nd);
                                  SetNumber(7+j, Ny);
                                  SetNumber(8+j, Nv);
                                  j:=j+8;
                            end;
                        SetNumber(Cell*8+3,CellNum);
                        SetHelpFile('Chain.Hlp', 'Cell Help');
                        SetRadioButton('1',2);
                        Repeat
                              AcceptScreen;
                              CNum:=Trunc(GetNumber(Cell*8+3));
                              If ((CNum>0) and (CNum<=((Yvec.Length-2)/Cell)))
                                 then Leave:=True
                              else
                                  Announce('Cell Number out of Range.');
                        Until Leave;
                        Ok:=GetBoolean(Cell*8+4);
                        Cancel:=GetBoolean(Cell*8+5);
                        If Ok then
                           begin;
                                 Case GetRadioButton('1') of
                                      1: All:=True;
                                      2: All:=False;
                                 End;
                                 CNum:=Trunc(GetNumber(Cell*8+3));

                                 AtomList.Remove;
                                 j:=0;
                                 For i:=1 to Cell do
                                     begin;
                                           nm:=GetNumber(1+j);
                                           nl:=GetNumber(2+j);
                                           nq:=GetNumber(3+j);
                                           nc:=GetNumber(4+j);
                                           ns:=GetNumber(5+j);
                                           nd:=GetNumber(6+j);
                                           ny:=GetNumber(7+j);
                                           nv:=GetNumber(8+j);
                                           AtomList.SetCellData(i,CNum,All,Nm,Ny,Nv,Nl,Nq,Nc,Nd,Ns);
                                           j:=j+8;
                                     end;
                                 AtomList.Display;
                           end;
                 Done;
           end;
CalcEnergy;
End;
Procedure ChainPlot.InitialAtoms;

Var
   anAtom:      AtomPtr;
   trash,
   i,j:         Word;
   y:           Real;

Begin;
      LockFocus;
      j:=1;
      For i:=1 to (InitNum) do
          begin;
                New(anatom);
                y:=0.8*Exp(-Sqr((i-17.5)/2.0));
                anAtom^.Initialize(i-1, y, @self);
                anAtom^.ChangeData( 1, 0, 0.5, 0, 0, 0, 0);
                If (i=1) or (i=(InitNum-1)) then Dec(j);
                AtomList.AddItem(anAtom);
                Inc(j);
          end;
      AtomList.ChangeColour(Ctype,Trash);
      AtomList.UpdateDvec;
End;


Procedure ChainPlot.ChangeScale;

Var                             
   InptSclScrn:                 TInputScreen;
   Ok, Cancel, AsXY,
   AsX,AsY,Top:                 Boolean;
   Temp:                        ScaleRect;
   i:                           Integer;
Begin;
      If ((Yo + height) < 0.6) then top:=true
                               else top:=false;
      Temp:=Scale;

      With InptSclScrn do
           begin;
                 Init;
                 Case Top of
                      True:  DefineInputPort( 0.31, 0.66, 0.30 ,0.90 );
                      False: DefineInputPort( 0.31, 0.66, 0.12 ,0.70 );
                 End;
                 LoadLine('       Scale Input         ');
                 LoadLine(' #T Display Scale On/Off?  ');
                 LoadLine('  Minimum       Maximum    ');
                 Loadline('                           ');
                 LoadLine(' X={     }     X={     }   ');
                 LoadLine(' Y={     }     Y={     }   ');
                 Loadline('                           ');
                 Loadline('     Full Screen    #T     ');
                 Loadline('                           ');
                 Loadline('     AutoMatic Scaling     ');
                 LoadLine(' [   X  ] [  XY  ] [  Y   ]');
                 Loadline('                           ');
                 LoadLine(' [  Ok  ] [Cancel] [ Help ]');
                 LoadLine('                           ');
                 With Temp do
                      begin;
                            SetBoolean(1, Plotaxes.Visible );
                            SetBoolean(6, Expanded );
                            SetNumber(2, xMin );
                            SetNumber(3, xMax );
                            SetNumber(4, yMin );
                            SetNumber(5, yMax );
                      end;
                 SetHelpFile('Chain.Hlp', 'Scaling Help');
                 AcceptScreen;

                 With Temp do
                      begin;
                            xMin:=GetNumber(2);
                            xMax:=GetNumber(3);
                            yMin:=GetNumber(4);
                            yMax:=GetNumber(5);
                      end;
                 PlotAxes.Visible:=GetBoolean(1);
                 AsXY:=GetBoolean(8);
                 If not(Expanded=GetBoolean(6)) then Expand;
                 Expanded:=GetBoolean(6);
                 AsX:=GetBoolean(7);
                 AsY:=GetBoolean(9);
                 Ok:=GetBoolean(10);
                 Cancel:=GetBoolean(11);
                 If Temp.xMin<0 THEN Temp.xMin:=0;
                 If (Temp.xMax> yVec.Length-1) then
                    begin;
                          Announce('X Max cannot be larger than the number of Atoms+1');
                          Exit;
                    end;

                 If (Temp.xMin>=Temp.xMax) OR (Temp.yMin>=Temp.yMax) then
                      begin;
                           Announce('Minimum vales must be less than Maximum value.');
                           Exit;
                      end;


                  Done;
           end;
           If Ok then
                       begin;
                             Remove;
                             With Temp do SetScale( xMin, yMin, xMax, yMax );
                             Display;
                             ShowEnergy;
                       end;

           If AsXY or AsX or AsY then
                 begin;
                       Remove;
                       With Temp do
                         begin;
                               If ((AsXY) or (AsX)) then
                                  begin;
                                        xMin:=0;
                                        xMax:=Yvec.Length-1;
                                  end;
                               If ((AsXY) or (AsY)) then
                                 Begin
                                 Yvec.MinMax(i,yMin,i,yMax);
                                 If (yMin=yMax) or (xMin=xMax) then
                                          begin;
                                                PlotAxes.Display;
                                                ButtonList.Display;
                                                AtomList.Display;
                                                Announce('The Minimum and Maximum Values are the Same.');
                                                Exit;
                                          end;

                                yMin:=FancyTrunc(yMin-0.1*(yMax-yMin));
                                yMax:=FancyTrunc(yMax+0.1*(yMax-yMin));
                                End;
                         SetScale(xMin, yMin, xMax, yMax);
                         end;
                       Display;
                       ShowEnergy;
                 end;
End;

Function ChainPlot.StepRun( N: Word ):Boolean;
Var
   MaxNum:              Word;
   i,m:                 Word;
   two:                 Integer;
   yTemp,vTemp,aTemp:   Real;

Begin;
      StepRun:=TRUE;
      MaxNum:=Yvec.Length;
      two:=2;
      For m:=1 to N do
          begin;
                For i:=1 to (MaxNum) do YOld.VecPtr^[i]:=Yvec.VecPtr^[i];
                For i:=2 to (MaxNum-1) do
                Yvec.VecPtr^[i]:=
                YOld.VecPtr^[i]+Vvec.VecPtr^[i]*dt+AVec.VecPtr^[i]*sqr(dt)/2;

                {AdjustForBoundary}
                If MovingFrame and (leftB<>Continuous)
                  Then
                    Begin
                    YVec.VecPtr^[1]:=yvec.VecPtr^[two];
                    YVec.VecPtr^[MaxNum]:=yvec.VecPtr^[MaxNum-1];
                    End
                  Else
                    Begin {stationary reference frame}
                    If LeftB=Fixed THEN YVec.VecPtr^[two]:=0;
                    If RightB=Fixed THEN YVec.VecPtr^[MaxNum-1]:=0;
                    IF LeftB=Continuous THEN
                      BEGIN
                      Yvec.VecPtr^[1]:=Yvec.VecPtr^[MaxNum-1];
                      Yvec.VecPtr^[MaxNum]:=Yvec.VecPtr^[two];
                      END;
                    End;

                For i:=2 to (MaxNum-1) do
                Avec.VecPtr^[i]:=
                Accel(i,YVec.VecPtr^[i-1],YVec.VecPtr^[i],YVec.VecPtr^[i+1],VVec.VecPtr^[i]);
                SourceList.AdjustDvec;
                {AdjustForBoundary}
                If MovingFrame and (leftB<>Continuous)
                  Then
                    Begin
                    aVec.VecPtr^[1]:=0;
                    aVec.VecPtr^[MaxNum]:=0;
                    End
                  Else
                    Begin
                    If LeftB=Fixed THEN aVec.VecPtr^[two]:=0;
                    If RightB=Fixed THEN aVec.VecPtr^[MaxNum-1]:=0;
                    IF LeftB=Continuous THEN
                      BEGIN
                      avec.VecPtr^[1]:=avec.VecPtr^[MaxNum-1];
                      avec.VecPtr^[MaxNum]:=avec.VecPtr^[two];
                      END;
                  End;
                For i:=2 to (MaxNum-1) do
                Vvec.VecPtr^[i]:=(Yvec.VecPtr^[i]-Yold.VecPtr^[i])/dt+AVec.VecPtr^[i]*dt/2;
                {AdjustForBoundary}
                if MovingFrame and (leftB<>Continuous)
                  Then
                    Begin
                    vVec.VecPtr^[1]:=0;
                    vVec.VecPtr^[MaxNum]:=0;
                    End
                  Else
                  Begin
                  If LeftB=Fixed THEN vVec.VecPtr^[two]:=0;
                  If RightB=Fixed THEN vVec.VecPtr^[MaxNum-1]:=0;
                  IF LeftB=Continuous THEN
                      BEGIN
                      vvec.VecPtr^[1]:=vvec.VecPtr^[MaxNum-1];
                      vvec.VecPtr^[MaxNum]:=vvec.VecPtr^[two];
                      END;
                  End;

                If MovingFrame Then frameShift:=frameShift+frameVelocity*dt;
                Time:=Time + dt;
                IF MovingFrame AND (FrameShift>1)
                  Then
                  Begin
                  frameShift:=frameShift-1;
                  yTemp:=yvec.VecPtr^[1];
                  vTemp:=vvec.VecPtr^[1];
                  aTemp:=avec.VecPtr^[1];
                  For i:=2 To MaxNum Do
                    Begin
                    yvec.VecPtr^[i-1]:=yvec.VecPtr^[i];
                    vvec.VecPtr^[i-1]:=vvec.VecPtr^[i];
                    avec.VecPtr^[i-1]:=avec.VecPtr^[i];
                    End;
                  If leftB=continuous Then
                    Begin
                    yvec.VecPtr^[MaxNum]:=yTemp;
                    vvec.VecPtr^[MaxNum]:=vTemp;
                    avec.VecPtr^[MaxNum]:=aTemp;
                    End
                    Else
                    Begin
                    yvec.VecPtr^[MaxNum]:=yvec.VecPtr^[MaxNum-1];
                    vvec.VecPtr^[MaxNum]:=0;
                    avec.VecPtr^[MaxNum]:=0;
                    End;
                  End;
                IF MovingFrame AND (FrameShift<-1)
                  Then
                  Begin
                  frameShift:=frameShift+1;
                  yTemp:=yvec.VecPtr^[maxNum];
                  vTemp:=vvec.VecPtr^[maxNum];
                  aTemp:=avec.VecPtr^[maxNum];
                  For i:=MaxNum DownTo 2 Do
                    Begin
                    yvec.VecPtr^[i]:=yvec.VecPtr^[i-1];
                    vvec.VecPtr^[i]:=vvec.VecPtr^[i-1];
                    avec.VecPtr^[i]:=avec.VecPtr^[i-1];
                    End;
                  If leftB=continuous Then
                    Begin
                    yvec.VecPtr^[1]:=yTemp;
                    vvec.VecPtr^[1]:=vTemp;
                    avec.VecPtr^[1]:=aTemp;
                    End
                    Else
                    Begin
                    yvec.VecPtr^[1]:=yvec.VecPtr^[two];
                    vvec.VecPtr^[1]:=0;
                    avec.VecPtr^[1]:=0;
                    End;
                  End;
      end;
 For i:=1 to (MaxNum) do
 If Sqr(Yvec.VecPtr^[i])>10E6 Then StepRun:=False; {System is unstable}
End;

Procedure ChainPlot.ChangeAttributes;

Var
   InptPAtmScrn:        TInputScreen;
   Ok, Cancel:          Boolean;

Begin;
      With InptPAtmScrn do
           begin;
                 Init;
                 DefineInputPort( 0.25, 0.75, 0.34 ,0.90 );

                 LoadLine('         Display Preferences          ');
                 LoadLine('--------------------------------------');
                 LoadLine('  Coloured  Atoms          #1         ');
                 LoadLine('  Outlined  Atoms          #1         ');
                 LoadLine('--------------------------------------');
                 LoadLine('  Pixel  Radius            #2  {    } ');
                 LoadLine('  Scaled Radius            #2  {    } ');
                 LoadLine('  Scale Atom Size By Mass  #T         ');
                 LoadLine('--------------------------------------');
                 LoadLine('  Show Velocity            #F         ');
                 LoadLine('  Show Envelope            #F         ');
                 LoadLine('--------------------------------------');
                 LoadLine('   [  Ok  ]   [Cancel]    [ Help ]    ');

                 If not(AtOutLine) then SetRadioButton('1',1)
                    else SetRadioButton('1',2);
                 If not(RadScale) then SetRadioButton('2',1)
                    else SetRadioButton('2',2);
                 SetNumber(4, 4.00);
                 SetNumber(6, 0.25);
                 SetBoolean(7, MassScaling);
                 SetBoolean(8, ShowV);
                 SetBoolean(9, ShowEnvelope);

                 SetHelpFile('Chain.Hlp', 'Display Help');
                 AcceptScreen;


                 Ok:=GetBoolean(10);
                 Cancel:=GetBoolean(11);

                 If Ok then
                    begin;
                           AtomList.Remove;

                          Case GetRadioButton('1') of
                          1: begin;
                                   AtomList.NotOutLine;
                                   AtOutline:=False;
                             end;
                          2: begin;
                                   AtomList.Outline;
                                   AtOutline:=True;
                             end;
                          End;

                          MassScaling:=GetBoolean(7);

                          Case GetRadioButton('2') of
                          1: begin;
                                   AtomList.RadSize(1,GetNumber(4));
                                   RadScale:=False;
                             end;
                          2: begin;
                                   AtomList.RadSize(2,GetNumber(6));
                                   RadScale:=True;
                             end;
                          End;

                          ShowV:=GetBoolean(8);
                          ShowEnvelope:=GetBoolean(9);
                          Update;
                    end;
                 Done;
           end;
End;

Procedure ChainPlot.Display;

Begin;
      Plot.Display;
      Replot;
End;

Procedure ChainPlot.Remove;

Begin;
      Atomlist.Remove;
      Plot.Remove;
End;

End.