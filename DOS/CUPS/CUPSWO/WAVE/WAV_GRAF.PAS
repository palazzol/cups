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

{$UNDEF DEBUG}
Unit Wav_Graf;
Interface
Uses Wav_Plot,Wav_Base,Wav_LST,Wav_Gbl,
     Cups,CupsGui,CUPSmupp,CupsGrph,CupsFunc,CupsProc,Crt,Graph,CUPSPars;

Type
WavePtr=^WavePlot;
WavePlot=Object(Plot)
    SegmentBut:      SegmentButton;
    SourceBut:       SourceButton;
    DetectorBut:     DetectorButton;
    ReflectBut:      ReflectorButton;
    ShowLeftWave,
    ShowRightWave,
    ShowBothWave:       Boolean;
    connectPoints:      Boolean;
    SaturationOn:       Boolean;
    RightWaveColor,
    LeftWaveColor,
    WaveColor:    Word;

    Constructor Initialize( ISclNum: Byte; x, y, w, h: Real );
    Destructor Done;      Virtual;
    Procedure ChangeScale;   Virtual;
    Procedure Replot; Virtual;   {Draws inside the plot while NOT running}
    Procedure ComponentReplot;
    Procedure StepRun( N: Word );
    Procedure Display;  Virtual;
    Procedure ShowAttrib; Virtual;
    Procedure UpdatePlot; Virtual;   {Draws inside the plot while running}
    Procedure PointsUpdatePlot;
    Procedure ConnectUpdatePlot;
    Procedure ComponentUpdatePlot;
    PROCEDURE CalculateAndResize;
    PROCEDURE UpdateVecs;
    Procedure AddSegment;
    Procedure AddSource;
    Procedure AddDetector;
    Procedure AddReflector;
    Function InitFunction(funcType:FunctionType):Boolean;
    Function InitRandom:Boolean;
    Function InitModes:Boolean;
    Procedure InitZero;
    Function UserFunc:Boolean;
    Procedure DisplayLegend;
    Procedure AutoSetTimeInc;
    Function SetTimeIncrement:Boolean;
    Function SetSpaceIncrement:Boolean;
    Procedure UnArchive(Var filehandle: text);
    Procedure Merge(Var filehandle: text);
End;

   TButton = object
      procedure Create(x,y : real; name_, key_: string);
      function  Clicked : boolean;
   private
      box : rectType;
      name : string;
      pos : pointType;
      key : integer;
      Extended : boolean;
   end;

Var
  TheWave:              WavePlot;

Implementation
Uses Wav_ss, Wav_Prop;

{------------------------ TButton type ---------------------------------}

PROCEDURE Error(errorStr : string);
VAR i : integer;
BEGIN
   ErrorStr := 'Error: '+ErrorStr;
   i := GetGraphMode;
   IF GraphResult=0 THEN announce(errorStr) ELSE writeln(errorStr);
   PAUSE;
   HALT;
END;

PROCEDURE SetRect(VAR r : rectType; x1,y1,x2,y2 : integer);
BEGIN
   r.x1:=x1; r.y1:=y1; r.x2:=x2; r.y2:=y2;
END;

PROCEDURE SetPoint(VAR p : PointType; x,y : integer);
BEGIN
   p.x:=x; p.y:=y;
END;

FUNCTION PtInside(var p:pointType;var r:rectType) : boolean;
BEGIN
    PtInside := (p.x>r.x1) AND (p.x<r.x2) AND (p.y>r.y1) AND (p.y<r.y2);
END;

procedure TButton.Create(x,y : real; name_,key_ : string);
var
   m,i : integer;
   envir : TEnvironment;
CONST
   KeyName: ARRAY[1..23] OF STRING[6] =
      ('F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','UP','DOWN','LEFT',
      'RIGHT','PGUP','PGDN','END','HOME','INS','DEL','TAB','ESC','RETURN');
   KeyNum : ARRAY[1..23] OF byte =
      (59,60,61,62,63,64,65,66,67,68,72,80,75,77,73,81,79,71,82,83,09,27,13);
BEGIN
   FOR m:=1 TO length(key_) DO key_[m] := upcase(key_[m]);
   m := 0;
   Extended := false;
   if (length(key_)=1) and (ord(key_[1])>=65) and (ord(key_[1])<=90) then
      Key := ord(key_[1])
   ELSE begin
      REPEAT inc(m) UNTIL (key_=KeyName[m]) OR (m=24);
      IF m=24 THEN
         ERROR('TButton.Create: '+key_+' is an invalid key name')
      else key := KeyNum[m];
      Extended := (m<21);
   end;
   name := Name_;
   for i:=length(name_) downto 1 do
    IF (name_[i]=' ') then Delete(name_,i,1)    {delete spaces}
    else name_[i] := upCase(name_[i]);          {change to uper-case}
   IF (name_='OK')     THEN name := '  OK  ';
   IF (name_='CANCEL') THEN name := 'Cancel';
   IF (name_='HELP')   THEN name := ' Help ';
   envir.Save;
   envir.Standardize;
   viewPortNumber := 0;
   SetRect( box, round(x*GetMaxX), round((1-y)*GetMaxY-RowHt+5),
                 round(x*GetMaxX+(length(name)+1)*ColWidth), round((1-y)*GetMaxY));
   HideMouse;
   SetFillStyle(solidFill,LightGray);
   SetColor(black);
   if name='  OK  ' then SetLineStyle(SolidLn,0,ThickWidth);
   WITH box DO bar3D( x1,y1,x2,y2,3,true);
   SetLineStyle(SolidLn,0,NormWidth);
   SetTextJustify(CenterText,CenterText);
   SetColor(black);
   WITH box DO OutTextXY((x2+x1) DIV 2, (y2+y1) DIV 2+1, name);
   ShowMouse;
   Envir.Reset;
End;

function TButton.Clicked : boolean;
var
   e : pointType;
begin
   e.x:=event.x;  e.y:=event.y;
   with event do
   clicked := (MouseClicked AND PtInside(e,box)) OR
              (KeyPressed and (ExtendedKey=Extended) and
              (ord(ReadKey)=key));
END;

{------------------------------------------------------------------------}

Procedure WavePlot.AutoSetTimeInc;
Var dx:Real; plusDt:Boolean; loop:Integer;
Begin
dx:=(stopEdge-startEdge)/(VacuumPts-1);
if dt>0 then plusDt:=True Else plusDt:=False;
{$R-}
Case EquationType of
  Diffusion:
    Begin
    PlusDt:=TRUE;
    dt:=Sqr(dx)/2;
    For loop:=1 To numpts Do
      If (IndexOGVec.VecPtr^[loop]>0) AND(dt>sqr(dx)/IndexOGVec.VecPtr^[loop]/2)
        Then dt:=sqr(dx)/IndexOGVec.VecPtr^[loop]/2;
    End;
  Schroedinger:
    begin
    dt:=Sqr(dx);
    For loop:=1 To numpts Do
      If (IndexOGVec.VecPtr^[loop]>0) AND(dt>1/(IndexOGVec.VecPtr^[loop]/2+1/Sqr(dx)))
        Then dt:=1/(IndexOGVec.VecPtr^[loop]/2+1/Sqr(dx));
    For loop:=1 To numpts Do
      If (IndexOGVec.VecPtr^[loop]<0) AND(dt>-2/IndexOGVec.VecPtr^[loop])
        Then dt:=-2/IndexOGVec.VecPtr^[loop];
    dt:=0.45*dt; {Our calculation uses dt twice Visscher's. Add another factor of 10% for safety. 0.9/2=0.45}
    end;
  Else dt:=(stopEdge-startEdge)/VacuumPts
  end; {of Case}
{$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
If  plusDt Then dt:=Abs(dt) Else dt:=-Abs(dt);
End;

Function WavePlot.SetTimeIncrement:Boolean;
Var
   TimeInptScrn:        TInputScreen;
   Ok,Cancel,Posdt:     Boolean;
   i:            Integer;
   ABSDt,oldTime,oldDt:        Real;
   fieldOffset:  Integer;
   msg:          String;

Begin;
      Posdt:=(dt>0);
      ABSdt:=abs(dt);
      oldTime:=Time;
      oldDt:=dt;
      With TimeInptScrn do
           begin;
                 Init;
                 fieldOffset:=0;
                 DefineInputPort( 0.30, 0.70, 0.40 ,0.90 );
                 LoadLine(' Set Time and Time Increment ');
                 LoadLine('                              ');
                 LoadLine('     Time = {        }        ');
                 if EquationType in [diffusion,Schroedinger] Then
                   begin
                   LoadLine('     |dt| = {        }        ');
                   fieldOffset:=1;
                   SetNumber(2,Abs(dt));
                   end
                 else
                   Begin
                   Str(1/NumPts:10:7,msg);
                   LoadLine('     |dt| ='+msg);
                   End;
                 LoadLine('     Positive dt: #T          ');
                 LoadLine('                              ');
                 LoadLine('NOTE: Setting time=0 will     ');
                 LoadLine('      make the current wave   ');
                 LoadLine('      the inital wave.        ');
                 LoadLine('                              ');
                 LoadLine('  [  Ok  ]  [Cancel]  [ Help ]   ');
                 SetHelpFile('wave.Hlp', 'Time Help');
                 SetNumber(1,Time);
                 SetBoolean(2+fieldOffset,dt>0);
                 AcceptScreen;
                 Ok:=OkPressed;
                 IF not CancelPressed then
                 begin
                    If Ok then Time:=GetNumber(1);
                    if time=0 then
                      Begin
                      yInitVec.Equate(1,yVec);
                      yPrevInitVec.Equate(1,yPrevVec);
                      initDisplayVec.Equate(1,DisplayVec);
                      evenTime:=True;
                      end;
                    if (EquationType in [Diffusion,Schroedinger]) and OK and
                       (Abs(Abs(GetNumber(2))-ABSdt)>ABSdt*0.001)
                    then dt:=Abs(GetNumber(2));
                    If Ok and not(Posdt=GetBoolean(2+fieldOffset)) then  {The user has reversed time direction}
                    Case EquationType of
                      EandM:
                       begin
                            Temp1OgVec.Equate(1,Yvec);
                            Yvec.Equate(1,yPrevVec);
                            yPrevVec.Equate(1,Temp1OgVec);
                            time:=time-dt;
                       end;
                      Diffusion:Announce('The Diffusion equation is not reversible.  You CANNOT change direction of <dt>.');
                      Else Announce('Change of  <dt> direction is not implemented yet.');
                      End;{of Case}
                    if GetBoolean(2+fieldOffset) Then dt:=Abs(dt) Else dt:=-Abs(dt);
                    setTimeIncrement := ((time<>OldTime) or (Abs(dt)<>Abs(oldDt))
                                        or (Posdt<>GetBoolean(2+fieldOffset)));
                 end;
                 Done;
           end;
End;

Function WavePlot.SetSpaceIncrement:Boolean;
Var
   SpaceInptScrn:        TInputScreen;
   Leave,
   Ok:                  Boolean;
   dx,x:                Real;
   OldVacPts,i:            Integer;
   oldStartEdge,OldStopEdge: REAL;
   AddedMemory:              LongInt;

Begin;
      OldVacPts:=VacuumPts;
      oldStartEdge:=StartEdge;
      OldStopEdge:=StopEdge;
      SetSpaceIncrement:=FALSE;
      With SpaceInptScrn do
           begin;
                 Init;
                 DefineInputPort( 0.30, 0.70, 0.40 ,0.90 );
                 LoadLine('   Space & Number of Points   ');
                 LoadLine('                              ');
                 LoadLine(' Left Coordinate= {          }');
                 LoadLine('Right Corodinate= {          }');
                 LoadLine('');
                 LoadLine('Number of Grid Points {     } ');
                 LoadLine('                              ');
                 LoadLine('NOTE: Changing a value will   ');
                 If (EquationType=Schroedinger) or (EquationType=Diffusion)
                 then    LoadLine('      reset Time.      ')
                 Else LoadLine('      reset Time and dt.      ');
                 LoadLine('');
                 LoadLine('  [  Ok  ]  [Cancel] [Help]   ');
                 SetHelpFile('wave.Hlp', 'Points Help');
                 SetNumber(1,StartEdge);
                 SetNumber(2,StopEdge);
                 SetNumber(3,VacuumPts);

                Leave:=False;
                 Repeat
                       AcceptScreen;
                       if GetBoolean(5) then Exit;
                       If (trunc(GetNumber(3)) Mod 2 = 0)and
                          (GetNumber(3)>=6) and
                          (GetNumber(3)<=4048) and
                          (GetNumber(1)<GetNumber(2))
                       then Leave:=True;
                       If (trunc(GetNumber(3)) Mod 2 = 1) then
                          Announce('Must have an even number of grid points.');
                       If ((GetNumber(3)<6) or (GetNumber(3)>4048)) then
                          Announce('The number of grid points must be >=6 and <=4048.');
                       If  (GetNumber(1)>=GetNumber(2)) Then
                         Announce('Left Coordinate must be < Right Coordinate');
                 Until Leave;

                 Ok:=GetBoolean(4);
                 if OK and (GetNumber(3)>2048) And (GetNumber(3)>VacuumPts) Then
                   Announce('CAUTION: Did you check available memory for this many points?');
                 IF OK and (GetNumber(3)>OldVacPts) Then {Check for memory.  Each new point takes about 128 Bytes}
                   Begin
                   addedMemory:=Round(GetNumber(3))-OldVacPts;
                   addedMemory:=128*addedMemory;
                   If addedMemory>MaxAvail Then
                   BEGIN
                   Announce('Not enough memory for this many points!');
                   OK:=FALSE;
                   END;
        End;
                 If Ok and ((OldVacPts<>GetNumber(3)) or (OldStartEdge<>GetNumber(1)) or (OldStopEdge<>GetNumber(2))) then
                    begin;
                          SetSpaceIncrement:=True;
                          startEdge:=GetNumber(1);
                          stopEdge:=GetNumber(2);
                          VacuumPts:=Round(Abs(GetNumber(3)));
                          if (VacuumPts Mod 2)=1 then vacuumPts:=vacuumPts+1;
                          if EquationType=Schroedinger Then hiddenPts:=vacuumPts DIV 2 else hiddenPts:=1;
                          Numpts:=VacuumPts+2*hiddenPts;
                          dx:=(stopEdge-startEdge)/(VacuumPts-1);
                          {SourceList.Empty;
                          DetectorList.Empty;
                          sourceNum:=0;
                          detectNum:=0;
                          SegmentList.Empty;
                          ReflectorList.Empty; }
                          IndexSGVec.Free;
                          gainSGVec.Free;
                          TempSGVec.Free;
                          xSGVec.Free;
                          xVec.Free;
                          YVec.Free;
                          yPrevVec.Free;
                          YInitVec.Free;
                          yPrevInitVec.Free;
                          IndexOGVec.Free;
                          gainOGVec.Free;
                          temp1OGVec.Free;
                          temp2OGVec.Free;
                          displayVec.Free;
                          initDisplayVec.Free;
                          OldRPixVec.Free;
                          OldLPixVec.Free;
                          OldPixVec.Free;
                          OldColorVec.Free;
                          xPixVec.Free;
                          IndexSGVec.Init(VacuumPts);
                          gainSGVec.Init(VacuumPts);
                          TempSGVec.Init(VacuumPts);
                          xSGVec.Init(VacuumPts);
                          xVec.Init(numpts);
                          YVec.Init(NumPts);
                          yPrevVec.Init(NumPts);
                          YInitVec.Init(NumPts);
                          yPrevInitVec.Init(NumPts);
                          IndexOGVec.Init(NumPts);
                          gainOGVec.Init(NumPts);
                          temp1OGVec.Init(NumPts);
                          temp2OGVec.Init(NumPts);
                          displayVec.Init(NumPts);
                          initDisplayVec.Init(NumPts);
                          OldRPixVec.Init(NumPts);
                          OldLPixVec.Init(NumPts);
                          OldPixVec.Init(NumPts);
                          OldColorVec.Init(numPts);
                          xPixVec.Init(numPts);
                          IndexSGVec.Fill(1);
                          gainSGVec.Fill(0);
                          IndexOGVec.Fill(1);
                          gainOGVec.Fill(0);
                          x:=StartEdge;
                          for i:=1 to VacuumPts Do
                            Begin
                            xSGVec.Put(i,x);
                            x:=x+dx;
                            end;
                          x:=StartEdge-dx;
                          for i:=1 to HiddenPts Do xVec.Put(i,x);
                          x:=StartEdge;
                          for i:=1+hiddenPts to VacuumPts+hiddenPts Do
                            Begin
                            xVec.Put(i,x);
                            x:=x+dx;
                            end;
                          x:=StopEdge+dx;
                          for i:=VacuumPts+HiddenPts+1 to vacuumPts+2*HiddenPts Do xVec.Put(i,x);
                          time:=0; EvenTime:=True;
                          AutoSetTimeInc;
                          PlotAxes.Remove;
                          theWave.SetScale( StartEdge,-1,StopEdge,1);
                          updateVecs;
                          IntegrateDensity;
                    end;
                 Done;
           end;

End;

Procedure WavePlot.CalculateAndResize;
VAR
   theNodes :           NodePtr;
   xLeft, xRight,
   interval, TOD,index:       Real;
   leftGrid, rightGrid,
   loop:                Integer;

Begin;
      hiddenPts:=1;
      If EquationType=Schroedinger Then  IndexSGVec.Fill(0) else IndexSGVec.Fill(1);
      GainSGVec.Fill(0);
      theNodes:=segmentList.nodes;
      interval:=(StopEdge - StartEdge)/ (VacuumPts-1);
{$R-}
      While theNodes <> NIL DO
            begin;
                  xLeft:=SegmentPtr(theNodes^.Item)^.x0;
                  xRight:=xLeft + SegmentPtr(theNodes^.Item)^.ParSegment.Width;
                  leftGrid:=1+round((xLeft - StartEdge)/interval);
                  rightGrid:=1+round((xRight - StartEdge)/interval);

                  If leftGrid<1 then leftGrid:=1;
                  If rightGrid>VacuumPts then rightGrid:=VacuumPts;

                  WHILE leftGrid < rightGrid DO
                  BEGIN;
                        IndexSGVec.VecPtr^[leftGrid] := SegmentPtr(theNodes^.Item)^.ParSegment.index;
                        gainSGVec.VecPtr^[leftGrid] := SegmentPtr(theNodes^.Item)^.ParSegment.gain;
                        leftGrid := leftGrid + 1;
                  END;
                  theNodes := theNodes^.Next;
            end;
      If (EquationType in [EandM,Diffusion,Schroedinger]) AND (IndexParserStr<>'') Then
        Begin
        Parser.Init;
        If Parser.Parse('x',' ',' ',' ',IndexParserStr) Then
        For loop:=1 to xSGVec.Length do
          begin;
          Index:=Parser.F(xSGVec.Value(loop),0,0,0);
          IndexSGVec.VecPtr^[loop]:= IndexSGVec.VecPtr^[loop]+index;
          If EquationType in [EandM,Diffusion]
            Then
            Begin
            IndexSGVec.VecPtr^[loop]:=Abs(IndexSGVec.VecPtr^[loop]);
            If IndexSGVec.VecPtr^[loop]=0 Then IndexSGVec.VecPtr^[loop]:=10*macheps;
            End;
          End;
        End;
      Case EquationType Of
        KleinGordon,sineGordon,
        phiFour,doubleSineGordon,
        diffusion:     numpts:=vacuumPts+2;
        Schroedinger: Begin hiddenPts:=vacuumPts DIV 2; numpts:=vacuumPts+2*hiddenPts; end;
        EandM:
          Begin
          TOD:=0;
          For loop:=1 to VacuumPts-1 DO TOD:=TOD+IndexSGVec.VecPtr^[loop];
          Tod:=Tod*Interval;
          NumPts:=round(Tod*VacuumPts/(StopEdge-StartEdge)) + 2;
          end;
        end; {of Case}
      If ((Numpts Mod 2) <> 0) then Inc(NumPts);  {make sure the number of points is even}
      if xVec.Length<> numPts THEN
        Begin
        xVec.Resize(numpts);
        YVec.Resize(NumPts);
        yPrevVec.Resize(NumPts);
        YInitVec.Resize(NumPts);
        yPrevInitVec.Resize(NumPts);
        temp1OGVec.Resize(NumPts);
        temp2OGVec.Resize(NumPts);
        displayVec.Resize(NumPts);
        initDisplayVec.Resize(numpts);
        IndexOGVec.Resize(NumPts);
        GainOGVec.Resize(NumPts);
        ReflectorList.Clear;
        SourceList.Clear;
        OldRPixVec.Free;
        OldLPixVec.Free;
        OldPixVec.Free;
        OldColorVec.Free;
        xPixVec.Free;
        OldRPixVec.Init(NumPts);
        OldLPixVec.Init(NumPts);
        OldPixVec.Init(NumPts);
        OldColorVec.Init(numPts);
        xPixVec.Init(numPts);
        if isVisible Then Begin LockFocus; ShowDensity;  End;
        End;
      if EquationType=Schroedinger
          Then IndexOGVec.Fill(0)
          Else IndexOGVec.Fill(1);
      GainOGVec.Fill(0);
{$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
END;

PROCEDURE WavePlot.UpdateVecs;
Const maxAbsorb=2.0e-2;
VAR
   xOld,dX:                 REAL;
   loop, count, Registered: INTEGER;
   two:                     Integer;
   maxIndex,minIndex:       Real;

BEGIN
{$R-}
  SegmentList.Sort;
  CalculateAndResize;  {Get New Numpts}
  dX:=(StopEdge-StartEdge)/(VacuumPts-1);
  xOld:=StartEdge;   {xOld keeps track of the optical distance.}
  Registered:=1;
  Count:=1;
  For loop:=1 to hiddenPts Do xVec.VecPtr^[loop]:=startEdge-dx;
  For loop:=1 to hiddenPts Do xVec.VecPtr^[Numpts-loop+1]:=stopEdge+dx;
  {Set the following vectors on the gird where the calculation is done:  xVec,IndexOG,GainOd,DelIndex.}
  Case EquationType Of
    KleinGordon,sineGordon,
    phiFour,doubleSineGordon:
      For loop:=1+hiddenPts to Numpts+1-hiddenPts Do
        Begin
        xVec.VecPtr^[loop]:=xOld;
        IndexOgVec.VecPtr^[loop]:=1;  {Not used by the calculation}
        GainOgVec.VecPtr^[loop]:=0;   {Not used by the calculation}
        xOld:=xOld+dx;
        End;
    diffusion:
      For loop:=1+hiddenPts to Numpts-hiddenPts Do
        Begin
        xVec.VecPtr^[loop]:=xOld;
        IndexOgVec.VecPtr^[loop]:=IndexSGVec.VecPtr^[loop-hiddenPts];
        GainOgVec.VecPtr^[loop]:=0;
        xOld:=xOld+dx;
        End;
    Schroedinger:
      begin
      maxIndex:=IndexSGVec.Value(2);
      minIndex:=maxIndex;
      For loop:=1+hiddenPts to Numpts-hiddenPts-1 Do
       IndexOgVec.VecPtr^[loop]:=
          10000*(IndexSGVec.VecPtr^[loop-hiddenPts]+IndexSGVec.VecPtr^[loop-hiddenPts+1])/2;
       IndexOgVec.VecPtr^[loop]:=10000*(IndexSGVec.VecPtr^[loop-hiddenPts]);
      For loop:=1+hiddenPts to Numpts-hiddenPts Do
        Begin
        xVec.VecPtr^[loop]:=xOld;
        GainOgVec.VecPtr^[loop]:=0;
        xOld:=xOld+dx;
        if maxIndex<IndexSGVec.VecPtr^[loop-hiddenPts] then maxIndex:= IndexSGVec.VecPtr^[loop-hiddenPts];
        if minIndex>IndexSGVec.VecPtr^[loop-hiddenPts] then minIndex:= IndexSGVec.VecPtr^[loop-hiddenPts];
        End;
      For Loop:=1 to HiddenPts+1 Do
        Begin
        IndexOgVec.VecPtr^[loop]:=IndexOgVec.VecPtr^[2+hiddenPts];
        IndexOgVec.VecPtr^[numpts-loop+1]:=IndexOgVec.VecPtr^[numpts-hiddenPts-2];
        End;
      For loop:=1 to hiddenPts Do   {fill in the values so that the wave is slowly absorbed}
        Begin
        xVec.VecPtr^[loop]:=startEdge-dx;
        xVec.VecPtr^[numPts-hiddenPts+loop]:=stopEdge+dx;
        GainOgVec.VecPtr^[numpts-hiddenPts+loop]:=-(loop-1)/hiddenPts*maxAbsorb;
        GainOgVec.VecPtr^[loop]:=-(hiddenPts-loop)/hiddenPts*maxAbsorb;
        end;
      End;
    EandM:
      begin
      For loop:=hiddenPts+1 to NumPts-hiddenPts do
        begin
           xVec.VecPtr^[loop]:=xOld;
           IndexOGVec.VecPtr^[loop]:=IndexSGVec.VecPtr^[Count];
           gainOGVec.VecPtr^[loop]:=gainSGVec.VecPtr^[Count];
           If ((IndexSGVec.VecPtr^[count]-IndexSGVec.VecPtr^[count+1]) <> 0)
           then
              if (Registered <> Count)
              Then
              begin
              Registered:=Count;
              xOld:=xOld + dX/IndexSGVec.VecPtr^[count];
              end
              else xOld:=xOld +dX/IndexSGVec.VecPtr^[count]
           else
             begin
             xOld:=xOld + dX/IndexSGVec.VecPtr^[count];
             end;{of if index}
           count:=1+round((VacuumPts-1)*(xOld-StartEdge)/(StopEdge-StartEdge));
           If count>(VacuumPts-1) then count:=(VacuumPts-1);
           if count<1 then count:=1;
        end;{of Do loop}
        IndexOgVec.VecPtr^[1]:=1;
        gainOgVec.VecPtr^[1]:=0;
        IndexOgVec.VecPtr^[numPts]:=1;
        gainOgVec.VecPtr^[numPts]:=0;
        loop:=2;
        Case leftB of
          Matched,Fixed:
            xVec.VecPtr^[1]:=xVec.VecPtr^[loop]-dx/IndexSGVec.VecPtr^[1];
          Periodic:
            xVec.VecPtr^[1]:=xVec.VecPtr^[loop]-(dx/IndexSGVec.VecPtr^[VacuumPts-1]);
        End;
       Case rightB of
          Matched,Fixed:
            xVec.VecPtr^[numPts]:=xVec.VecPtr^[numPts-1]+dx/IndexSGVec.VecPtr^[VacuumPts-2];
          Periodic:
            xVec.VecPtr^[numPts]:=xVec.VecPtr^[numPts-1]+dx/IndexSGVec.VecPtr^[1];
        End;
      end; {of EandM and Acoustic}
   End; {of case}
{R+}
  SourceList.CalcGridIndex;
  DetectorList.CalcGridIndex;
  ReflectorList.CalcGridIndex;
  IntegrateDensity;
END;


Constructor WavePlot.Initialize( ISclNum: Byte; x, y, w, h: Real);
Begin
 Equationtype:=EandM;
 Plot.Initialize( ISclNum,x, y, w, h);
 PlotTitle.SetStr('Electromagnetic');
 SetScale( StartEdge,-1,StopEdge,1);
 PlotSclBut.Initialize( @self );
 ExpandBut.Initialize( @self );
 SourceBut.Initialize( @self);
 SegmentBut.Initialize( @self);
 DetectorBut.Initialize( @Self );
 ReflectBut.Initialize( @Self );
 AttrBut.Initialize( @self);
 SaturationOn:=True;
 OffsetComponents:=False;
 ShowLeftWave:=True;
 LeftWaveColor:=Yellow;
 ShowRightWave:=True;
 RightWaveColor:=LightMagenta;
 ShowBothWave:=True;
 ConnectPoints:=True;
 WaveColor:=White;
 isVisible:=True;
End;

Destructor WavePlot.Done;
Begin
Plot.done;
End;

Procedure  WavePlot.UnArchive(Var filehandle: text);
var x,dx,yMin,yMax:REAL; i,oldPts:Integer;
Begin
      dx:=(StopEdge-StartEdge)/(VacuumPts-1);
      IndexSGVec.Resize(VacuumPts);
      TempSGVec.Resize(VacuumPts);
      xSGVec.Resize(VacuumPts);
      GainSGVec.Resize(VacuumPts);
      initZero;
      x:=StartEdge;
      for i:=1 to VacuumPts Do
                            Begin
                            xSGVec.Put(i,x);
                            x:=x+dx;
                            end;
      time:=0; EvenTime:=True;
      UpdateVecs;
      ReadLn(fileHandle,oldPts,Ymin,Ymax);
      SetScale(startEdge,yMin,stopEdge,yMax);
      If oldPts<>Numpts Then Announce('Number of data points has changed.');
      if NumPts>=OldPts
         Then
         For i:=1 to OldPts do
         ReadLn(fileHandle, Yvec.VecPtr^[i], YprevVec.VecPtr^[i],DisplayVec.Vecptr^[i])
         Else
         Begin
         For i:=1 to Numpts do
         ReadLn(fileHandle, Yvec.VecPtr^[i], YprevVec.VecPtr^[i],DisplayVec.Vecptr^[i]);
         For i:=NumPts+1 to Oldpts do
         ReadLn(fileHandle, Yvec.VecPtr^[NumPts], YprevVec.VecPtr^[NumPts],DisplayVec.Vecptr^[NumPts]);
         End;
      {$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
End;

Procedure  WavePlot.Merge(Var filehandle: text);
var i,oldPts:Integer; ymin,ymax,ya,yb,d:Real;
Begin
      ReadLn(fileHandle,oldPts,Ymin,Ymax);
      {$R-}
      if NumPts>=OldPts
         Then
           For i:=1 to OldPts do
           Begin
           ReadLn(fileHandle, Ya, Yb,D);
           yVec.VecPtr^[i]:=yVec.VecPtr^[i]+ya;
           yPrevVec.VecPtr^[i]:=yPrevVec.VecPtr^[i]+yb;
           displayVec.VecPtr^[i]:=displayVec.VecPtr^[i]+d;
           End
         Else
           Begin
           For i:=1 to Numpts do
             Begin
             ReadLn(fileHandle, Ya, Yb,D);
             yVec.VecPtr^[i]:=yVec.VecPtr^[i]+ya;
             yPrevVec.VecPtr^[i]:=yPrevVec.VecPtr^[i]+yb;
             displayVec.VecPtr^[i]:=displayVec.VecPtr^[i]+d;
             end;
           For i:=NumPts+1 to Oldpts do
           ReadLn(fileHandle, Ya, Yb,D);
           End;
      {$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
End;

Procedure WavePlot.ComponentUpdatePlot;
{A Fast drawing routine that minimizes calculations by remembering the pixel values.}
Var
 xInc,yInc:                  Real;
 xStartPix,yStartPix,
 xEndPix,yEndPix:            Word;
 x1,y1,x2:                   LongInt;
 i,count:                    Word;
 yOffset:                      Word;
 vecPtr:                       DBasePtr;
Begin
If not(isVisible) then exit;
If OffsetComponents  And (EquationType in [phiFour,sineGordon,DoubleSineGordon,KleinGordon,Diffusion])
  Then
    Begin
    partialX(temp1OGVec,DisplayVec);
    VecPtr:=temp1OGVec.Vecptr;
    End
  Else VecPtr:=displayVec.vecPtr;
LockFocus;
With Scale Do
  Begin
  xInc:=(MapX(xMax)-MapX(xMin))/(xMax-xMin);
  yInc:=(MapY(yMax)-MapY(yMin))/(yMax-yMin);
  xStartPix:=MapX(xMin);
  xEndPix:=MapX(xMax);
  yStartPix:=MapY(yMin);
  yEndPix:=MapY(yMax);
  yOffset:=Round((yStartPix-yEndPix)/4);
  {$R-}
  count:=HiddenPts;
  Repeat
    Inc(count);
    x1:=Round(xStartPix+xinc*(xvec.Vecptr^[count]-xMin));
  Until x1>=xStartPix;
  If ShowRightWave Then
    Begin
    If Count>2 then i:=count-1 Else i:=2;
    x1:=xPixVec.wvecptr^[i];
    PutPixel(x1,OldRPixVec.wvecptr^[i],oldColorVec.wvecptr^[i]);
    x1:=xPixVec.wvecptr^[i];
    y1:=Round(yStartPix+yinc*(yVec.Vecptr^[i]-yMin));
    if offsetComponents Then y1:=y1-yOffset;
    if y1<0 then y1:=0;
    PutPixel(x1,y1,RightWaveColor);
    oldRPixVec.wvecptr^[i]:=y1;
    While (i<=Numpts-1) AND (x1<xEndPix)  Do
      begin
      Repeat i:=i+1 Until (xPixVec.wvecptr^[i]>x1+1) OR (i>=NumPts);
      x1:=xPixVec.wvecptr^[i];
      PutPixel(x1,OldRPixVec.wvecptr^[i],OldColorVec.wvecptr^[i]);
      y1:=Round(yStartPix+yinc*(yVec.Vecptr^[i]-yMin));
      if offsetComponents Then y1:=y1-yOffset;
      if y1<0 then y1:=0;
      PutPixel(x1,y1,RightWaveColor);
      oldRPixVec.wvecptr^[i]:=y1;
      End;
    End;
  If ShowLeftWave Then
    Begin
    If Count>2 then i:=count-1 Else i:=2;
    x1:=xPixVec.wvecptr^[i];
    PutPixel(x1,OldLPixVec.wvecptr^[i],oldColorVec.wvecptr^[i]);
    y1:=Round(yStartPix+yinc*(yPrevvec.Vecptr^[i]-yMin));
    if offsetComponents Then y1:=y1+yOffset;
    if y1<0 then y1:=0;
    PutPixel(x1,y1,LeftWaveColor);
    oldLPixVec.wvecptr^[i]:=y1;
    While (i<=Numpts-1) AND (x1<xEndPix)  Do
      begin
      Repeat i:=i+1 Until (xPixVec.wvecptr^[i]>x1+1) OR (i>=NumPts);
      x1:=xPixVec.wvecptr^[i];
      PutPixel(x1,OldLPixVec.wvecptr^[i],OldColorVec.wvecptr^[i]);
      x1:=xPixVec.wvecptr^[i];
      y1:=Round(yStartPix+yinc*(yPrevvec.Vecptr^[i]-yMin));
      if offsetComponents Then y1:=y1+yOffset;
      if y1<0 then y1:=0;
      PutPixel(x1,y1,LeftWaveColor);
      oldlPixVec.wvecptr^[i]:=y1;
      End;
    End;
   {$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
  End; {of With Scale}
  ShowDensity;
End;

Procedure WavePlot.UpdatePlot;
Begin
if ConnectPoints AND ((NumPts<256))OR((NumPts<511) And (EquationType=Schroedinger))
  then ConnectUpdatePlot else PointsUpdatePlot;
end;

Procedure WavePlot.PointsUpdatePlot;
{A Fast drawing routine that minimizes calculations by remembering the pixel values.}
Var
 xInc,yInc:                  Real;
 xStartPix,yStartPix,
 xEndPix,yEndPix:            Word;
 x1,y1,x2:                   LongInt;
 i,count:                    Word;
 vecPtr:                     DBasePtr;

Begin
If not(isVisible) then exit;
If OffsetComponents  And (EquationType in [phiFour,sineGordon,DoubleSineGordon,KleinGordon,Diffusion])
  Then
    Begin
    partialX(temp1OGVec,DisplayVec);
    VecPtr:=temp1OGVec.Vecptr;
    End
  Else VecPtr:=DisplayVec.VecPtr;
LockFocus;
If ShowBothWave Then With Scale Do
  Begin
  xInc:=(MapX(xMax)-MapX(xMin))/(xMax-xMin);
  yInc:=(MapY(yMax)-MapY(yMin))/(yMax-yMin);
  xStartPix:=MapX(xMin);
  xEndPix:=MapX(xMax);
  yStartPix:=MapY(yMin);
  yEndPix:=MapY(yMax);
  {$R-}
  count:=HiddenPts;;
  Repeat
    Inc(count);
    x1:=Round(xStartPix+xinc*(xvec.Vecptr^[count]-xMin));
  Until x1>=xStartPix;
  If Count>2 then i:=count-1 Else i:=2;
  x1:=xPixVec.wvecptr^[i];
  PutPixel(x1,OldPixVec.wvecptr^[i],oldColorVec.wvecptr^[i]);
  x1:=xPixVec.wvecptr^[i];
  y1:=Round(yStartPix+yinc*(vecPtr^[i]-yMin));
  if y1<0 then y1:=0;
  PutPixel(x1,y1,WaveColor);
  oldPixVec.wvecptr^[i]:=y1;
  While (i<=Numpts-2) AND (x1<xEndPix)  Do
      begin
      i:=i+1;
      x1:=xPixVec.wvecptr^[i];
      PutPixel(x1,OldPixVec.wvecptr^[i],OldColorVec.wvecptr^[i]);
      x1:=xPixVec.wvecptr^[i];
      y1:=Round(yStartPix+yinc*(vecPtr^[i]-yMin));
      if y1<0 then y1:=0;
      PutPixel(x1,y1,WaveColor);
      oldPixVec.wvecptr^[i]:=y1;
      End;
   {$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
  End; {of With Scale}
  Case EquationType of
    EandM,
    Schroedinger:ComponentUpdatePlot;
    End; {of Case}
  ShowDensity;
End;

Procedure WavePlot.ConnectUpdatePlot;
Var
 xInc,yInc:                  Real;
 xStartPix,yStartPix,
 xEndPix,yEndPix,yOffset:    Word;
 x1,y1,x2:                   longInt;
 i,count:                    Word;
 vecptr:                     DBaseptr;

Begin
If not(isVisible) then exit;
If OffsetComponents  And (EquationType in [phiFour,sineGordon,DoubleSineGordon,KleinGordon,Diffusion])
  Then
    Begin
    partialX(temp1OGVec,DisplayVec);
    VecPtr:=temp1OGVec.Vecptr;
    End
  Else VecPtr:=DisplayVec.VecPtr;
LockFocus;
If ShowBothWave Then With Scale Do
  Begin
  SetWriteMode(XORPut);
  xInc:=(MapX(xMax)-MapX(xMin))/(xMax-xMin);
  yInc:=(MapY(yMax)-MapY(yMin))/(yMax-yMin);
  xStartPix:=MapX(xMin);
  xEndPix:=MapX(xMax);
  yStartPix:=MapY(yMin);
  yEndPix:=MapY(yMax);
  yOffset:=Round((yStartPix-yEndPix)/4);
  {$R-}
  count:=HiddenPts;
  Repeat
    Inc(count);
    x1:=Round(xStartPix+xinc*(xvec.Vecptr^[count]-xMin));
  Until x1>=xStartPix;
  If Count>2 then i:=count-1 Else i:=2;
  x1:=xPixVec.wvecptr^[i];
  y1:=Round(yStartPix+yinc*(vecPtr^[i]-yMin));
  if y1<0 then y1:=0;
  While (i<=Numpts-1) AND (x1<xEndPix)  Do
      begin
      MoveTo(x1,oldPixVec.wvecptr^[i]);  {go to the beginning of the old line segment}
      LineTo(x1,oldPixVec.wvecptr^[i]); {XOR the first pixel in the line sement to turn it on.}
      oldPixVec.wvecptr^[i]:=y1; {Save the y value so we don't need to recalculate.}
      Repeat i:=i+1 until (xPixVec.wvecptr^[i]>x1+1) OR (i>=Numpts);
      x2:=xPixVec.wvecptr^[i];
      LineTo(x2,OldPixVec.wvecptr^[i]);   {erase the old line}
      putPixel(x2,OldPixVec.wvecptr^[i],oldColorVec.wvecptr^[i]); {turn off the last pixel}
      MoveTo(x1,y1);{move to the beginning of the new line segment}
      y1:=Round(yStartPix+yinc*(vecPtr^[i]-yMin));
      if y1<0 then y1:=0;
      LineTo(x2,y1);   {draw the new line with XOR}
      LineTo(x2,y1);   {erase the last point with XOR}
      x1:=x2;
      end;
      oldPixVec.wvecptr^[i]:=y1; {Save the y value so we don't need to recalculate.}
   {$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
  End; {of With Scale}
  SetWriteMode(CopyPut);
  Case EquationType of
    EandM,
    Schroedinger:ComponentUpdatePlot;
    End; {of Case}
  ShowDensity;
End;


Procedure WavePlot.ComponentReplot;
Var
 xInc,yInc:                  Real;
 xStartPix,yStartPix,
 xEndPix,yEndPix:            Word;
 x1,y1,x2,y2:                LongInt;
 i,count:                    Word;
 oldColor:                   Word;
 yOffset:                    Word;
Begin
OldColor:=GetColor;
LockFocus;
If Not(Running) then HideCursor;
With Scale Do
  Begin
  xInc:=(MapX(xMax)-MapX(xMin))/(xMax-xMin);
  yInc:=(MapY(yMax)-MapY(yMin))/(yMax-yMin);
  xStartPix:=MapX(xMin);
  xEndPix:=MapX(xMax);
  yStartPix:=MapY(yMin);
  yEndPix:=MapY(yMax);
  yOffset:=Round((yStartPix-yEndPix)/4);
  {$R-}
  count:=HiddenPts;
  Repeat
    Inc(count);
    x1:=Round(xStartPix+xinc*(xvec.Vecptr^[count]-xMin));
  Until x1>=xStartPix;
  If ShowRightWave Then
    Begin
    if count>2 Then i:=count-1 Else i:=2;
    x1:=Round(xStartPix+xinc*(xvec.Vecptr^[i]-xMin));
    y1:=Round(yStartPix+yinc*(yVec.Vecptr^[i]-yMin));
    if offsetComponents Then y1:=y1-yOffset;
    MoveTo(x1,y1);
    If NOT(Running) THEN SetColor(rightWaveColor);
    While (i<=Numpts-1) AND (x1<xEndPix)  Do
      begin
      i:=i+1;
      x1:=Round(xStartPix+xinc*(xvec.Vecptr^[i]-xMin));
      y1:=Round(yStartPix+yinc*(yVec.Vecptr^[i]-yMin));
      if offsetComponents Then y1:=y1-yOffset;
      If connectPoints And (y1<=yStartPix)And (y1>=yEndPix) Then  LineTo(x1,y1) Else MoveTo(x1,y1);
      If NOT(connectPoints) And (y1<=yStartPix)And (y1>=yEndPix) Then PutPixel(x1,y1,rightWaveColor);
      End;
    End;
  If ShowLeftWave Then
    Begin
    if count>2 Then i:=count-1 Else i:=2;
    x1:=Round(xStartPix+xinc*(xvec.Vecptr^[i]-xMin));
    y1:=Round(yStartPix+yinc*(yPrevVec.Vecptr^[i]-yMin));
    if offsetComponents Then y1:=y1+yOffset;
    MoveTo(x1,y1);
    If Not(Running) then SetColor(leftWaveColor);
    While (i<=Numpts-1) AND (x1<xEndPix)  Do
      begin
      i:=i+1;
      x1:=Round(xStartPix+xinc*(xvec.Vecptr^[i]-xMin));
      y1:=Round(yStartPix+yinc*(yPrevVec.Vecptr^[i]-yMin));
      if offsetComponents Then y1:=y1+yOffset;
      If ConnectPoints And (y1<=yStartPix)And (y1>=yEndPix) Then LineTo(x1,y1) Else MoveTo(x1,y1);
      If NOT(connectPoints) And (y1<=yStartPix)And (y1>=yEndPix) Then PutPixel(x1,y1,leftWaveColor);
      End;
    End;
   {$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
  End; {of With Scale}
If Not(Running) THen
  Begin SetColor(oldColor);
        ShowCursor;
  End;
End;

Procedure WavePlot.Replot;

Var
 xInc,yInc:                  Real;
 xStartPix,yStartPix,
 xEndPix,yEndPix:            Word;
 x1,y1,x2,y2:                LongInt;
 i,count:                    Word;
 oldColor:                   Word;
 VecPtr:                     DbasePtr;

Begin
If OffsetComponents  And (EquationType in [phiFour,DoubleSineGordon,sineGordon,KleinGordon,Diffusion])
  Then
    Begin
    partialX(temp1OGVec,DisplayVec);
    VecPtr:=temp1OGVec.Vecptr;
    End
  Else VecPtr:=DisplayVec.VecPtr;
OldColor:=GetColor;
SetColor(waveColor);
LockFocus;
If Not(Running) then HideCursor;
If ShowBothWave Then With Scale Do
  Begin
  xInc:=(MapX(xMax)-MapX(xMin))/(xMax-xMin);
  yInc:=(MapY(yMax)-MapY(yMin))/(yMax-yMin);
  xStartPix:=MapX(xMin);
  xEndPix:=MapX(xMax);
  yStartPix:=MapY(yMin);
  yEndPix:=MapY(yMax);
  {$R-}
  count:=HiddenPts;
  Repeat
    Inc(count);
    x1:=Round(xStartPix+xinc*(xvec.Vecptr^[count]-xMin));
  Until x1>=xStartPix;
  If Count>2 then i:=count-1 Else i:=2;
  x1:=Round(xStartPix+xinc*(xvec.Vecptr^[i]-xMin));
  y1:=Round(yStartPix+yinc*(Vecptr^[i]-yMin));
  MoveTo(x1,y1);
  While (i<=Numpts-2) AND (x1<xEndPix)  Do
      begin
      i:=i+1;
      x1:=Round(xStartPix+xinc*(xvec.Vecptr^[i]-xMin));
      y1:=Round(yStartPix+yinc*(Vecptr^[i]-yMin));
      If connectPoints And (y1<=yStartPix)And (y1>=yEndPix)
        Then  LineTo(x1,y1)
        Else MoveTo(x1,y1);
      If NOT(connectPoints) And (y1<=yStartPix)And (y1>=yEndPix) Then PutPixel(x1,y1,WaveColor);
      End;
   {$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
  End; {of With Scale}
Case EquationType of
  EandM,
  Schroedinger:ComponentReplot;
  End;{ofCase}
SetColor(oldColor);
If Not(Running) Then ShowCursor;
End;

Procedure WavePlot.InitZero;
  Begin;  {zero the wave}
  YVec.Fill(0);
  displayVec.Fill(0);
  InitDisplayVec.Fill(0);
  yPrevVec.Fill(0);
  yPrevInitVec.Fill(0);
  yInitVec.Fill(0);
  time:=0; EvenTime:=True; dt:=Abs(dt);
  updateVecs;
  AutoSetTimeInc;
  End;

Procedure MatchVecsToEquationType( waveDirection:SourceDir);
{Assume that a function is defined in yVec.  Set yPrevVec to match the stepping algorithm.}
Const  momentum=128;

Var i:   Integer;
    temp:Real;
Begin
{$R-}
EvenTime:=True;  {<-  This should be true anyway since time=0 when this function is called but we'll make sure!}
For i:=1 to numpts Do DisplayVec.VecPtr^[i]:=yVec.VecPtr^[i]; {Default-  recalc in Case if need be.}
Case EquationType of
  Schroedinger:
    Case WaveDirection of
     Bidirectional:
       Begin
       For i:=1 to numpts Do Begin yPrevVec.VecPtr^[i]:=0; DisplayVec.VecPtr^[i]:=Sqr(yVec.VecPtr^[i]);End;
       End;
     toRight:
       For i:=1 to numpts Do
         Begin
         yPrevVec.VecPtr^[i]:=-yVec.VecPtr^[i]*Cos(xVec.VecPtr^[i]*momentum);
         yVec.VecPtr^[i]:=yVec.VecPtr^[i]*Sin(xVec.VecPtr^[i]*momentum);
         DisplayVec.VecPtr^[i]:=Sqrt(Sqr(yVec.VecPtr^[i])+Sqr(yPrevVec.VecPtr^[i]));
         End;
     toLeft:
        For i:=1 to numpts Do
         Begin
         yPrevVec.VecPtr^[i]:=yVec.VecPtr^[i]*Cos(xVec.VecPtr^[i]*momentum);
         yVec.VecPtr^[i]:=yVec.VecPtr^[i]*Sin(xVec.VecPtr^[i]*momentum);
         DisplayVec.VecPtr^[i]:=Sqrt(Sqr(yVec.VecPtr^[i])+Sqr(yPrevVec.VecPtr^[i]));
         End;
     End; {of Case}
  Diffusion:
      Begin
      For i:=1 to numpts Do yPrevVec.VecPtr^[i]:=yVec.VecPtr^[i];
      End;
  EandM:
      Case waveDirection of
                 toLeft:
                         For i:=1 to Numpts Do
                           Begin
                           yPrevVec.Vecptr^[i]:=yVec.VecPtr^[i];
                           displayVec.VecPtr^[i]:=yVec.VecPtr^[i];
                           yVec.Vecptr^[i]:=0;
                           End;
                 toRight:Begin
                         For i:=1 to Numpts-1 Do yPrevVec.Vecptr^[i]:=0;
                         end;

                 biDirectional:
                         For i:=1 to Numpts Do
                           Begin
                           yPrevVec.Vecptr^[i]:=yVec.VecPtr^[i];
                           displayVec.VecPtr^[i]:=yVec.VecPtr^[i]+yPrevVec.Vecptr^[i];
                           end;
                End; {of Direction Case}
  KleinGordon,sineGordon,
  phiFour,
  doubleSineGordon:
      Begin
      For i:=1 to (Numpts Div 2) Do yVec.Vecptr^[2*i-1]:=0;
      Case waveDirection of
                 toLeft: For i:=2 to Numpts Do yPrevVec.Vecptr^[i]:=yvec.Vecptr^[i-1];
                 toRight:For i:=1 to Numpts-1 Do yPrevVec.Vecptr^[i]:=yVec.Vecptr^[i+1];
                 biDirectional:For i:=2 to Numpts-1 Do yPrevVec.Vecptr^[i]:=(yVec.Vecptr^[i+1]+yVec.Vecptr^[i-1])/2;
                End; {of Case}
      End;
End; {of Case}
{$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
End;

Function WavePlot.UserFunc;
Var
   Ok,Cancel,GoodFunc,
   Leave,DrawFunc:    Boolean;
   FuncScreen:  TinputScreen;
   Clear:       Boolean;
   FuncStr:String;
   i:           Integer;
   x,t,out,zero:  Complex;
   dx:          Real;
   tempyVec,tempyPrevvec,tempDisplayVec:DVector;
   tempTime:         Real;
   tempEvenTime:     Boolean;
   waveDirection: SourceDir;
   varOffset:     Integer;

Begin
Clear:=True;
UserFunc:=False;
dX:=(StopEdge-StartEdge)/(VacuumPts-1);
tempTime:=time;
time:=0;
tempEvenTime:=evenTime;
evenTime:=True;
tempyVec.Init(Numpts);
tempyPrevVec.Init(Numpts);
tempDisplayVec.Init(Numpts);
tempyVec.Equate(1,yVec);
tempyPrevVec.Equate(1,yPrevVec);
tempDisplayVec.Equate(1,displayVec);
waveDirection:=BiDirectional;
Parser.init;
     With FuncScreen do
          begin
               varoffset:=0;
               funcScreen.Init;
               DefineInputPort(0.02,0.98,0.51,0.95);
               LoadLine('                         Input the initial function. ');
               LoadLine('F(x,t)=  ');
               LoadLine('"                                                                        "');
               LoadLine('"                                                                        "');
               LoadLine('Add to exisiting wavefunction?  #F  ');
               LoadLine('');
               Case EquationType of
                 EandM:Begin
                       LoadLine('             Select a direction of propagation for the wave.');
                       LoadLine('                  #3 Left   #3 Right   #3 Bi-Directional');
                       varOffset:=3;
                       End;
                 Schroedinger: LoadLine('  Input a complex valued function  --  (e.g.,cos(x)+i*sin(x).');
                 else LoadLine('  Input a function of x and t  --  (e.g.,cos(3*x)*Exp(-2*t).');
               end;{of Case}
               LoadLine('  ');
               LoadLine('               [  Ok  ]  [Cancel]  [ Help ]  [ View ] ');
               SetHelpFile('Wave.Hlp','Function Help');
               SetString(1,ParserStr1);
               SetString(2,ParserStr2);
               Leave:=False;
               Repeat
                     AcceptScreen;
                     clear:=Not GetBoolean(3);
                     Ok:=GetBoolean(4+varOffset);
                     Cancel:=GetBoolean(5+varOffset);
                     DrawFunc:=GetBoolean(7+varOffset);
                     if equationtype=EandM Then
                     Case  GetRadioButton('3')of
                      1:WaveDirection:=toLeft;
                      2:WaveDirection:=toRight;
                      3:WaveDirection:=biDirectional;
                      end; {of Case}
                     If Cancel Then Begin
                                    yVec.Equate(1,tempYVec);
                                    yPrevVec.Equate(1,tempyPrevVec);
                                    DisplayVec.Equate(1,tempDisplayVec);
                                    Time:=tempTime;
                                    EvenTime:=tempEvenTime;
                                    tempyVec.Free;
                                    tempyPrevVec.Free;
                                    tempDisplayVec.Free;
                                    UserFunc:=False;
                                    Exit;
                                    End;
                     Parser.Init;
                     FuncStr:=GetString(1)+GetString(2);
                     If FuncStr='' then FuncStr:='0';
                     GoodFunc := Parser.Parse('x','t',' ',' ',funcStr);
                     if GoodFunc and DrawFunc Then
                       Begin
                       x.im:=0;
                       t.im:=0;
                       ErrorFound := false;
                       HaltIfError := false;
                       i:=2;
                       {$R-}
                       while (i<=NumPts-1) and not ErrorFound do
                       Begin
                         x.re:=xVec.VecPtr^[i];
                         t.re:=Time;
                         Parser.CxF(x,t,zero,zero,out);
                         If Clear
                           Then yVec.VecPtr^[i]:=out.re
                           Else yVec.VecPtr^[i]:=out.re+tempYVec.VecPtr^[i];
                         Case EquationType of
                           EandM:
                             Begin
                             If Clear
                               Then yPrevVec.VecPtr^[i]:=out.re
                               Else yPrevVec.VecPtr^[i]:=out.re+tempyPrevVec.VecPtr^[i];
                             displayVec.VecPtr^[i]:=yPrevVec.VecPtr^[i];
                             End;
                           Schroedinger:
                             Begin
                             If Clear
                               Then yPrevVec.VecPtr^[i]:=out.im
                               Else yPrevVec.VecPtr^[i]:=out.im+tempYPrevVec.VecPtr^[i];
                             displayVec.VecPtr^[i]:=Sqrt(Sqr(yVec.VecPtr^[i])+Sqr(yPrevVec.VecPtr^[i]));
                             end;
                           Else
                             Begin
                             t.re:=Time-dt;
                             Parser.CxF(x,t,zero,zero,out);
                             If Clear
                               Then yPrevVec.VecPtr^[i]:=out.Re
                               Else yPrevVec.VecPtr^[i]:=out.Re+tempYPrevVec.VecPtr^[i];
                             displayVec.VecPtr^[i]:=yvec.VecPtr^[i];
                             End;
                         end;{of case}
                         inc(i);
                         End;
                         {$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
                       if not ErrorFound then
                       begin
                          if EquationType=EandM Then MatchVecsToEquationType(waveDirection);
                          display;
                       end;
                       GoodFunc := not ErrorFound;
                       errorFound := false;
                       HaltIfError := true;
                       End;
                     Leave := (GoodFunc and OK);
                     if leave then
                     begin
                        errorFound := false;
                        HaltIfError := false;
                        x.im:=0;
                        t.im:=0;
                        i:=2;
                        while (i<=NumPts-1) and (not ErrorFound) Do
                        Begin
                        {$R-}
                          x.re:=xVec.VecPtr^[i];
                          t.re:=Time;
                          Parser.CxF(x,t,zero,zero,out);
                          yVec.Put(i,out.re);
                          Case EquationType of
                          EandM:
                            Begin
                            yPrevVec.VecPtr^[i]:=out.re;
                            displayVec.VecPtr^[i]:=out.re;
                            End;
                          Schroedinger:
                            Begin
                            yPrevVec.Put(i,out.im);
                            displayVec.VecPtr^[i]:=Sqrt(Sqr(out.im)+Sqr(out.Re));
                            end;
                          Else
                            Begin
                            t.re:=Time-dt;
                            Parser.CxF(x,t,zero,zero,out);
                            yPrevVec.VecPtr^[i]:=out.re;
                            displayVec.VecPtr^[i]:=out.Re;
                            End;
                          end;{of case}
                          {$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
                          inc(i);
                        End;
                        If EquationType=EandM then MatchVecsToEquationType(waveDirection);
                        If Not Clear Then
                          Begin
                          yVec.AddVector(1,TempYVec);
                          yPrevVec.AddVector(1,TempYPrevVec);
                          displayVec.AddVector(1,TempDisplayVec);
                          End;
                       GoodFunc := not ErrorFound;
                       errorFound := false;
                       HaltIfError := true;
                     end;
                     Leave := (GoodFunc and OK);
                     if not goodFunc then
                        Announce('That function is not acceptable - f(x,t).');
               Until Leave;
               FuncStr:=GetString(1)+GetString(2);
               ParserStr1 := GetString(1);
               ParserStr2 := GetString(2);
               FuncScreen.Done;
          end;
yInitVec.Equate(1,yVec);
yPrevInitVec.Equate(1,yPrevVec);
time:=0;
evenTime:=True;
AdjustForBoundry;
tempyVec.Free;
tempyPrevVec.Free;
tempDisplayVec.Free;
UserFunc:=True;
End;

Function WavePlot.InitRandom;
Var
   Amp:  Real;
   TempYVec, tempyPrevVec,
   tempDisplayVec:   Dvector;
   tempTime:         Real;
   tempEvenTime:     Boolean;
   OldColour:    Word;
   Info:     TSliders;
   clearBut
{   actionBut}:  TButtons;
   Ok_Button, Cancel_Button : TButton;
   Clear:Boolean;

Procedure DrawFunction;

Var
   i:            Word;
   RValue,LValue:        Real;
Begin;
 {$R-}
     yVec.Fill(0);
     yPrevVec.Fill(0);
     DisplayVec.Fill(0);
      For i:=2 to Numpts-1 do
        begin;
          RValue:=Amp*(-0.5+Random(1024)/1024);
          LValue:=Amp*(-0.5+Random(1024)/1024);
          If Clear
          Then
            Begin
            yVec.Vecptr^[i]:=RValue;
            yPrevVec.Vecptr^[i]:=LValue;
            end
          Else
            Begin
            yVec.Vecptr^[i]:=rValue+TempYVec.VecPtr^[i];
            yPrevVec.Vecptr^[i]:=LValue+TempYPrevVec.VecPtr^[i];
            End;
          DisplayVec.VecPtr^[i]:=yVec.Vecptr^[i]+yPrevVec.VecPtr^[i];
        end;  {of For loop}
      AdjustForBoundry;
      SelectViewPort(1);  {lock on to the Wave Viewport}
      Display;
      SelectViewPort(4);  {lock on to the Slider Viewport}
End;
Procedure InitSliderWindow;
Var OldColor:Word;
Begin
      DefineViewPort(4, 0.20, 0.80, 0.52, 0.85);
      oldColor:=GraphBackColor;
      GraphBackColor:=Blue;
      OpenViewPort(4);
      GraphBackColor:=OldColor;
      With Info do
           With Scale Do
               begin;
                 Init;
                 Create(1,0,    yMax,   Amp,0.25,0.68,0.75,2,'0',ToStr(yMax),'Amp',False);
                 DrawAll;
               end;
{    With actionBut do
           begin;
                 Init;
                 Create(11,0.29,0.56,'Draw');
                 Create(22,0.45,0.56,'Cancel');
                 Create(33,0.62,0.56,'Accept');
                 Number:=11;
                 DrawAll;
           end;  }
     Cancel_Button.Create(0.50,0.56,'Cancel','Esc');
     Ok_Button.Create(0.35,0.56,'OK','Return');
     With ClearBut do
           begin
                 Init;
                 Create(11,0.25,0.78,'Clear Wave');
                 Create(22,0.50,0.78,'Add To Exisiting Wave');
                 Number:=11;
                 Clear:=True;
                 DrawAll;
           end;
End;
Procedure ServiceSliderWindow;

Var
   Leave:    Boolean;
   i:        Integer;
Begin
      Leave:=False;

      Repeat
            CheckForEvents;
            If Info.Changed then
              With Info Do
               begin;
                     amp:=Value(1);
                     drawfunction;
               end;
               if Cancel_Button.Clicked then
                         Begin Leave:=True;     {restore the old values}
                               yVec.Equate(1,TempYVec);
                               yPrevVec.Equate(1,tempYPrevVec);
                               displayVec.Equate(1,tempDisplayVec);
                               time:=tempTime;
                               evenTime:=TempevenTime;
                               InitRandom:=False;
                               AdjustForBoundry;
                               SelectViewPort(1);  {lock on to the Wave Viewport}
                               Display;
                         End;
               if Ok_Button.Clicked then
                         Begin
                         EvenTime:=True; time:=0;
                         drawFunction;{get Things up to date}
                         yInitVec.Equate(1,yVec);
                         yPrevInitVec.Equate(1,yPrevVec);
                         initDisplayVec.Equate(1,DisplayVec);
                         Leave:=True; EvenTime:=True; {Draw Function has changed EvenTime to false}
                         InitRandom:=True;
                         drawFunction;
                         End;

           If ClearBut.Changed then
             Begin
               Case ClearBut.Number of
                    11: Clear:=TRUE;
                    22: Clear:=FALSE;
               End;
               drawFunction;
             End;
      Until Leave=True;
End;

Begin;
      InitRandom:=False;
      OldColour:=GetColor;
      if Not(EvenTime) Then StepRun(1);
      temptime:=time;
      tempEvenTime:=EvenTime;
      time:=0;
      EvenTime:=True;
      Amp:=0.5;
      TempYVec.Init(numpts);
      tempYPrevVec.Init(numPts);
      tempDisplayVec.Init(numpts);
      tempYVec.Equate(1,yVec);
      tempYPrevVec.Equate(1,yPrevVec);
      tempDisplayvec.Equate(1,displayVec);
      InitSliderWindow;
      LockFocus;
      Running:=True;  {Fool the program into filling the PixColorVec and xPixVec for FASTER drawing}
      Display;
      Running:=False;
      drawFunction;
      ServiceSliderWindow;
      TempYVec.Free;
      tempYPrevVec.Free;
      tempDisplayVec.Free;
      CloseViewPort(4);
      Info.Done;
{      actionBut.Done; }
      ClearBut.Done;
      SetColor(OldColour);
End; {of InitRandom}

Function WavePlot.InitFunction(funcType:FunctionType):Boolean;
Var
   Amp,Delay,A,k:  Real;
   TempYVec, tempyPrevVec,
   tempDisplayVec:   Dvector;
   tempTime:         Real;
   tempEvenTime:     Boolean;
   OldColour:    Word;
   Info:     TSliders;
   clearBut,
   {actionBut,}
   directionBut:  TButtons;
   Ok_Button, Cancel_Button : TButton;
   waveDirection: SourceDir;
   Clear:Boolean;

Procedure DrawFunction;

Var
   i:            Word;
   Value,x,dx:        Real;
Begin;
     yVec.Fill(0);
     yPrevVec.Fill(0);
     DisplayVec.Fill(0);
     dx:=(stopEdge-startEdge)/(VacuumPts-1);
      For i:=2 to Numpts-1 do
          begin
            x:=xVec.Value(i)-dx/2;
            Case FuncType of
              Gaussian:Begin
                Value:=Sqr(2*(x-Delay)/A);
                If (Value<20) then Value:=Amp*Exp(-Value) else Value:=0;
                End;
              ModGaussian:
                Begin
                Value:=Sqr(2*(x-Delay)/A);
                If (Value<10) then Value:=Amp*Exp(-Value)*Sin(4*Pi*x/A) else Value:=0;
                End;
              Step: if ((x-Delay)>=0) and ((x-Delay-A)<0)  Then
                    value:= Amp Else Value:=0;
              Pulse:  if ((x-Delay+A)>=0) and ((x-Delay)<0)  Then value:=-Amp
                      Else
                      if ((x-Delay)>=0) and ((x-Delay-A)<=0)  Then Value:=Amp
                      Else Value:=0;
               Sine: Value:=Amp*Sin(2*Pi*x/A+Delay);
            End;{of Case}

            Case EquationType Of
            Schroedinger:
                If Clear then
                  Begin
                  yVec.vecPtr^[i]:=value*Cos(k*(xVec.Value(i)-Delay));
                  yPrevVec.VecPtr^[i]:=value*Sin(k*(xVec.Value(i)-Delay));
                  displayVec.VecPtr^[i]:=Sqrt(Sqr(yVec.vecPtr^[i])+Sqr(yPrevVec.vecPtr^[i]))
                  End
                Else
                  Begin
                  yVec.vecPtr^[i]:=value*Cos(k*(xVec.Value(i)-Delay))+TempYVec.VecPtr^[i];
                  yPrevVec.VecPtr^[i]:=value*Sin(k*(xVec.Value(i)-Delay))+TempYPrevVec.VecPtr^[i];
                  displayVec.VecPtr^[i]:=Sqrt(Sqr(yVec.vecPtr^[i])+Sqr(yPrevVec.vecPtr^[i]))
                  End;
            EandM:
                 Begin
                 Case waveDirection of
                 toLeft: If Clear Then yPrevVec.Vecptr^[i]:=value
                                  Else
                                    Begin
                                    yVec.VecPtr^[i]:=tempYVec.VecPtr^[i];
                                    yPrevVec.Vecptr^[i]:=value+TempyPrevVec.VecPtr^[i];
                                    end;
                 toRight: If Clear Then yVec.Vecptr^[i]:=value
                                   Else
                                     Begin
                                     yVec.Vecptr^[i]:=value+TempYVec.Vecptr^[i];
                                     yPrevVec.VecPtr^[i]:=TempyPrevVec.VecPtr^[i];
                                     end;
                 biDirectional:
                           If Clear Then
                           Begin
                           yVec.Vecptr^[i]:=value;
                           yPrevVec.Vecptr^[i]:=value;
                           end
                           Else
                           Begin
                           yVec.Vecptr^[i]:=value+TempYVec.VecPtr^[i];
                           yPrevVec.Vecptr^[i]:=value+TempYPrevVec.VecPtr^[i];
                           End
                End; {of Direction Case}
                DisplayVec.VecPtr^[i]:=yVec.Vecptr^[i]+yPrevVec.VecPtr^[i];
                End;
            Diffusion:
                Begin
                if Clear Then yVec.vecPtr^[i]:=value Else yVec.vecPtr^[i]:=value+TempyVec.VecPtr^[i];
                yPrevVec.VecPtr^[i]:=yVec.VecPtr^[i];
                DisplayVec.VecPtr^[i]:=yVec.VecPtr^[i];
                End;
            Else {a Klein-Grodon type equation}
                if (i Mod 2)=1
                Then
                  Begin
                  yVec.vecPtr^[i]:=0;
                  If Clear Then DisplayVec.VecPtr^[i]:=Value
                           Else  DisplayVec.VecPtr^[i]:=Value+TempDisplayVec.VecPtr^[i];
                  End
                Else
                  begin
                  if Clear Then yVec.vecPtr^[i]:=value Else yVec.vecPtr^[i]:=value+tempYVec.VecPtr^[i];
                  displayVec.VecPtr^[i]:=yVec.VecPtr^[i];
                  Case waveDirection of
                  ToLeft:
                    if Clear Then yPrevVec.vecPtr^[i+1]:=value
                             Else yPrevVec.vecPtr^[i+1]:=value+TempYPrevVec.vecPtr^[i+1];
                  ToRight:
                    If Clear Then yPrevVec.vecPtr^[i-1]:=value
                             Else yPrevVec.vecPtr^[i-1]:=value+TempYPrevVec.vecPtr^[i-1];
                  biDirectional:
                    if Clear
                    Then
                      Begin
                      yPrevVec.vecPtr^[i+1]:=yPrevVec.vecPtr^[i+1]+value/2;
                      yPrevVec.vecPtr^[i-1]:=yPrevVec.vecPtr^[i-1]+value/2;
                      End
                    Else
                      Begin
                      yPrevVec.vecPtr^[i+1]:=yPrevVec.vecPtr^[i+1]+value/2+TempYPrevVec.vecPtr^[i+1]/2;
                      yPrevVec.vecPtr^[i-1]:=yPrevVec.vecPtr^[i-1]+value/2+TempyPrevVec.vecPtr^[i-1]/2;
                      End
                  End{of WaveDirection Case}
                  End;
            End; {of EquationType Case}
      end;  {of For loop}
      AdjustForBoundry;
      SelectViewPort(1);  {lock on to the Wave Viewport}
      Display;
      SelectViewPort(4);  {lock on to the Slider Viewport}
End;
Procedure InitSliderWindow;
Var OldColor:Word;
Begin
      DefineViewPort(4, 0.20, 0.80, 0.52, 0.95);
      oldColor:=GraphBackColor;
      GraphBackColor:=Blue;
      OpenViewPort(4);
      GraphBackColor:=OldColor;
      OutTextXY(7,28,'CAUTION: Be sure you match Y(x) to boundaries!');
      With Info do
           With Scale Do
           Case FuncType of
             Gaussian,Pulse,Step,ModGaussian:
                begin;
                 Init;
                 Create(1,yMin,    yMax,   Amp,0.25,0.83,0.75,2,ToStr(yMin),ToStr(yMax),'Amp',False);
                 Create(2,0.001,xMax-xMin,A,0.25,0.76,0.75,2,'0.001',ToStr(xMax-xMin),'Width',False);
                 Create(3,xMin,xMax,Delay,0.25,0.69,0.75,2,ToStr(xMin),ToStr(xMax),'Pos.',False);
                 if EquationType=Schroedinger Then
                  Create(4,-200,200,k,0.25,0.62,0.75,1,'-200','200','Momentum: k',False);
                 DrawAll;
                 end;
             Sine:
               begin;
                 Init;
                 Create(1,0,    yMax,   Amp,0.25,0.83,0.75,2,'0',ToStr(yMax),'Amp',False);
                 Create(2,0.001,2*(xMax-xMin),A,0.25,0.76,0.78,4,'0.001',ToStr(2*(xMax-xMin)),'Lambda',False);
                 Create(3,0,2*Pi,Delay,0.25,0.69,0.75,2,ToStr(0),ToStr(2*Pi),'Phase',False);
                 DrawAll;
               end;
           end; {of Case}
{    Print(1,9,'-------------------------------------------------');
     With actionBut do
           begin;
                 Init;
                 Create(11,0.29,0.56,'Draw');
                 Create(22,0.45,0.56,'Cancel');
                 Create(33,0.62,0.56,'Accept');
                 Number:=11;
                 DrawAll;
           end;  }
     Cancel_Button.Create(0.6,0.54,'Cancel','Esc');
     Ok_Button.Create(0.4,0.54,'Ok','Return');
    If (EquationType<>Diffusion) And (EquationType<>Schroedinger) Then
      With DirectionBut Do
           begin;
                 {Print(1,7,'_________________________________________________');}
                 Init;
                 Create(11,0.29,0.625,'To Left');
                 Create(22,0.45,0.625,'To Right');
                 Create(33,0.62,0.625,'Bidirect.');
                 Number:=33;
                 DrawAll;
           end;
     With ClearBut do
           begin;
                 Init;
                 Create(11,0.25,0.92,'Clear Wave');
                 Create(22,0.50,0.92,'Add To Exisiting Wave');
                 Number:=11;
                 Clear:=True;
                 DrawAll;
           end;
End;
Procedure ServiceSliderWindow;

Var
   Leave:    Boolean;
   i:        Integer;
Begin
      Leave:=False;

      Repeat
            CheckForEvents;

            If Info.Changed then
              With Info Do
               begin;
                     amp:=Value(1);
                     A:=Value(2);
                     Delay:=Value(3);
                     if EquationType=Schroedinger Then k:=Value(4);
                     drawfunction;
               end;
               if Cancel_Button.Clicked then
                         Begin Leave:=True;     {restore the old values}
                               yVec.Equate(1,TempYVec);
                               yPrevVec.Equate(1,tempYPrevVec);
                               displayVec.Equate(1,tempDisplayVec);
                               time:=tempTime;
                               evenTime:=TempevenTime;
                               InitFunction:=False;
                         End;
               if Ok_Button.Clicked then
                         Begin
                         EvenTime:=True; time:=0;
                         drawFunction;{get Things up to date}
                         yInitVec.Equate(1,yVec);
                         yPrevInitVec.Equate(1,yPrevVec);
                         initDisplayVec.Equate(1,DisplayVec);
                         Leave:=True; EvenTime:=True; {Draw Function has changed EvenTime to false}
                         InitFunction:=True;
                         End;

           If (EquationType<>Diffusion) And (EquationType<>Schroedinger)
             then if directionBut.Changed then
             Begin
               Case directionBut.Number of
                    11: waveDirection:=toLeft;
                    22: WaveDirection:=toRight;
                    33: WaveDirection:=Bidirectional;
               End;
               drawFunction;
             End;
           If ClearBut.Changed then
             Begin
               Case ClearBut.Number of
                    11: Clear:=TRUE;
                    22: Clear:=FALSE;
               End;
               drawFunction;
             End;
      Until Leave=True;
If Equationtype=Schroedinger Then
  If Abs(k/pi)>(VacuumPts-1)/(stopEdge-startEdge)
    Then Announce('NOT ENOUGH GIRD POINTS! Phase oscillation is > Pi between points!');
End;

Begin
      InitFunction:=False;
      OldColour:=GetColor;
      if Not(EvenTime) Then StepRun(1);
      temptime:=time;
      tempEvenTime:=EvenTime;
      time:=0;
      EvenTime:=True;
      waveDirection:=BiDirectional;
      Amp:=0.5;
      Delay:=(Scale.xMax+Scale.xMin)/2;
      A:=(Scale.xMax-Scale.xMin)/8;
      k:=50;
      TempYVec.Init(numpts);
      tempYPrevVec.Init(numPts);
      tempDisplayVec.Init(numpts);
      tempYVec.Equate(1,yVec);
      tempYPrevVec.Equate(1,yPrevVec);
      tempDisplayvec.Equate(1,displayVec);
      InitSliderWindow;
      LockFocus;
      Running:=True;  {Fool the program into filling the PixColorVec and xPixVec for FASTER drawing}
      Display;
      Running:=False;
      drawFunction;
      ServiceSliderWindow;
      TempYVec.Free;
      tempYPrevVec.Free;
      tempDisplayVec.Free;
      CloseViewPort(4);
      Info.Done;
{      actionBut.Done; }
      if (EquationType<>Diffusion) and (EquationType<>Schroedinger) Then DirectionBut.Done;
      ClearBut.Done;
      SetColor(OldColour)
End; {of InitFunction}

Procedure WavePlot.ShowAttrib;

Var
   InptPAtmScrn:        TInputScreen;
   Ok, Cancel:          Boolean;
   Offset:              Integer;

Begin;
      With InptPAtmScrn do
           begin;
                 Init;
                 DefineInputPort( 0.25, 0.75, 0.50 ,0.93 );

                 LoadLine('         Display Options         ');
                 Case EquationType of
                 Schroedinger:
                   Begin
                   Loadline('#T ABS(Psi)               COLOR={   } ');
                   LoadLine('#T Real Component         COLOR={   } ');
                   Loadline('#T Imaginary Component    COLOR={   } ');
                   LoadLine('   Offset Components             #T   ');
                   Offset:=7;
                   End;
                 EandM:
                   Begin
                   Loadline('#T Total Wave             COLOR={   } ');
                   LoadLine('#T Right Traveling Wave   COLOR={   } ');
                   Loadline('#T Left Traveling Wave    COLOR={   } ');
                   LoadLine('   Offset Components             #T   ');
                   Offset:=7;
                   End;
                   Else
                    Begin
                    Loadline('#T Total Wave             COLOR={   } ');
                    LoadLine('#T Right Traveling Wave   COLOR={   } ');
                    Loadline('#T Left Traveling Wave    COLOR={   } ');
                    LoadLine('   Plot Derivative               #T   ');
                    Offset:=7;
                    End
                 End;{of Case}
                 LoadLine('   Connect Points                #T   ');
                 LoadLine('Allows user to change how wave will   ');
                 LoadLine('be displayed. Do not set the color    ');
                 LoadLine('to the background!                    ');
                 LoadLine('   [  Ok  ]   [Cancel]    [ Help ]    ');
                 SetBoolean(1, ShowBothWave);
                 SetNumber(2,WaveColor);
                 SetBoolean(3, ShowRightWave);
                 SetNumber(4,RightWaveColor);
                 SetBoolean(5, ShowLeftWave);
                 SetNumber(6,LeftWaveColor);
                 SetBoolean(7,offsetComponents);
                 SetBoolean(offset+1,connectPoints);
                 Case EquationType of
                   Schroedinger:SetHelpFile('Wave.Hlp', 'Schroedinger Help');
                   EandM:SetHelpFile('Wave.Hlp', 'EandM Help');
                   Diffusion:SetHelpFile('Wave.Hlp', 'Diffusion Help');
                   KleinGordon,sineGordon,
                   phiFour,doubleSineGordon:SetHelpFile('Wave.Hlp', 'KleinGordon Help');
                 end;

                 Repeat
                   AcceptScreen;
                   Ok:=GetBoolean(offset+2);
                   Cancel:=GetBoolean(offset+3);
                   If OK AND NOT((GetNumber(2)>=0 ) AND
                                 (GetNumber(2)<=MaxColors))THEN
                     Announce('Color Out of Range. RE-EDIT.');
                   If OK AND NOT((GetNumber(4)>=0 ) AND
                                 (GetNumber(4)<=MaxColors))THEN
                     Announce('Color Out of Range. RE-EDIT.');
                   If OK AND NOT((GetNumber(6)>=0 ) AND
                                 (GetNumber(6)<=MaxColors))THEN
                     Announce('Color Out of Range. RE-EDIT.');
                 Until ((GetNumber(2)>=0 ) AND (GetNumber(2)<=MaxColors)And
                        (GetNumber(4)>=0 ) AND (GetNumber(4)<=MaxColors)And
                        (GetNumber(6)>=0 ) AND (GetNumber(6)<=MaxColors));

                 If Ok then
                   Begin
                   ShowBothWave:=getBoolean(1);
                   WaveColor:=Round(getNumber(2));
                   ShowRightWave:=getBoolean(3);
                   RightWaveColor:=Round(getNumber(4));
                   ShowLeftWave:=getBoolean(5);
                   LeftWaveColor:=Round(getNumber(6));
                   offsetComponents:=GetBoolean(7);
                   connectPoints:=GetBoolean(offset+1);
                   End;
                   SetRunKeys;
                   SetStopKeys;
                   StopKeys.Display;
                 Display;
                 Done;
           end;
End; {of ShowAttrib}

Procedure WavePlot.DisplayLegend;
  var i:Integer; oldColor:Word;
  Begin
  if Not(isVisible) then Exit;
  SelectViewport(0);
  oldColor:=GetColor;
  setColor(Black);
  for i:=1 to 3 do Rubout(74,15+i,6,Black);
    if ShowBothWave Then
    Begin
    i:=3;
    Rubout(74,15+i,6,WaveColor);
    if EquationType=Schroedinger then Print(74,15+i,' Abs') Else Print(74,15+i,' Total');
    end;
  if EquationType in [KleinGordon,phiFour,DoubleSineGordon,SineGordon,Diffusion] then
    Begin
    i:=1;
    Rubout(74,15+i,6,WaveColor);
    if OffsetComponents then Print(74,15+i,' D/Dx of') else Rubout(74,15+i,6,Black);
    SelectViewport(1);
    setColor(oldColor);
    Exit;
    End;
  if ShowRightWave Then
    Begin
    i:=1;
    Rubout(74,15+i,6,RightWaveColor);
    if EquationType=Schroedinger then Print(74,15+i,' Re')Else Print(74,15+i,' Right');
    end;
  if ShowLeftWave Then
    Begin
    i:=2;
    Rubout(74,15+i,6,LeftWaveColor);
    if EquationType=Schroedinger then Print(74,15+i,' Im') Else Print(74,15+i,' Left');
    end;
  SelectViewport(1);
  setColor(oldColor);
  End;

Procedure WavePlot.Display;
  Procedure DrawOnce;
  Var
    xInc,yInc:                  Real;
    xStartPix,yStartPix,
    xEndPix,yEndPix:            Word;
    x1,y1,x2:                   LongInt;
    i,count:                    Word;
    vecPtr:                     DbasePtr;
  Begin
 If OffsetComponents  And (EquationType in [phiFour,sineGordon,DoubleSineGordon,KleinGordon,Diffusion])
  Then
    Begin
    partialX(temp1OGVec,DisplayVec);
    VecPtr:=temp1OGVec.Vecptr;
    End
  Else VecPtr:=displayVec.vecPtr;
  LockFocus;
  With Scale Do
      Begin
      SetWriteMode(XORPut);
      xInc:=(MapX(xMax)-MapX(xMin))/(xMax-xMin);
      yInc:=(MapY(yMax)-MapY(yMin))/(yMax-yMin);
      xStartPix:=MapX(xMin);
      xEndPix:=MapX(xMax);
      yStartPix:=MapY(yMin);
      yEndPix:=MapY(yMax);
      count:=HiddenPts;
      {$R-}
      Repeat
        Inc(count);
        x1:=Round(xStartPix+xinc*(xvec.Vecptr^[count]-xMin));
        Until x1>=xStartPix;
      If Count>2 then i:=count-1 Else i:=2;
      x1:=xPixVec.wvecptr^[i];
      y1:=Round(yStartPix+yinc*(Vecptr^[i]-yMin));
      if y1<0 then y1:=0;
      MoveTo(x1,y1);
      While (i<=Numpts-1) AND (x1<xEndPix)  Do
        begin
        oldPixVec.wvecptr^[i]:=y1; {Save the y value so we don't need to recalculate.}
        Repeat i:=i+1 until (xPixVec.wvecptr^[i]>x1+1) OR (i>=NumPts);
        x2:=xPixVec.wvecptr^[i];
        y1:=Round(yStartPix+yinc*(Vecptr^[i]-yMin));
        if y1<0 then y1:=0;
        LineTo(x2,y1);   {draw the new line}
        LineTo(x2,y1);   {erase the last point}
        x1:=x2;
        End;
        oldPixVec.wvecptr^[i]:=y1; {Save the y value so we don't need to recalculate.}
        {$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
  End; {of With Scale}
  SetWriteMode(CopyPut);
  End;
var
  i:    Integer;
  x,y:Integer;
  oldColor                    :Word;

Begin
If not(isVisible) then exit;
oldColor:=GraphBackColor;
GraphBackColor:=Black;
plot.display;
DisplayLegend;
If Running Then
  Begin
    lockFocus;
    OpenViewPort(1);
    SegmentList.Display;
    SourceList.Display;
    DetectorList.Display;
    ReflectorList.Display;
    For i:=1 to Numpts Do
  {$R-}
    Begin
    x:=MapX(xVec.VecPtr^[i]);
    if x<0 Then x:=0;
    y:=MapY((Scale.yMax+Scale.yMin)/2);
    oldColorVec.wvecptr^[i]:=GetPixel(x,y);
    xPixVec.wvecptr^[i]:=x;
    end;
   if ConnectPoints AND ((NumPts<256))OR((NumPts<511) And (EquationType=Schroedinger))
     Then DrawOnce;
  {$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
    PlotAxes.Display;
    End
  Else
    Begin
         SegmentList.Display;
         SourceList.Display;
         DetectorList.Display;
         ReflectorList.Display;
         replot;
         LockFocus;
         ButtonList.Display;
         if isVisible then ShowDensity;
    End;
GraphBackColor:=OldColor;
End;

Procedure WavePlot.ChangeScale;

Var                             
   InptSclScrn:                 TInputScreen;
   Ok, Cancel, AsXY,
   AsX,AsY,Top:                 Boolean;
   Temp:                        ScaleRect;
   i:                           Integer;
   Size:                        Real;

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
                 LoadLine('      Display Scale? #T    ');
                 LoadLine('  Minimum       Maximum    ');
                 Loadline('                           ');
                 LoadLine(' X={     }     X={     }   ');
                 LoadLine(' Y={     }     Y={     }   ');
                 Loadline('                           ');
                 Loadline('     Full Screen    #T     ');
                 Loadline('                           ');
                 Loadline('     AutoMatic Scaling     ');
                 LoadLine(' [  XY  ] [  X   ] [  Y   ]');
                 Loadline('                           ');
                 LoadLine(' [  Ok  ] [Cancel] [ Help ]');
                 LoadLine('                           ');
                 With Temp do
                      begin;
                            SetBoolean(1, PlotAxes.IsVisible );
                            SetBoolean(6, Expanded );
                            SetNumber(2, xMin );
                            SetNumber(3, xMax );
                            SetNumber(4, yMin );
                            SetNumber(5, yMax );
                      end;
                 SetHelpFile('Wave.Hlp', 'Scaling Help');
                 AcceptScreen;

                 With Temp do
                      begin;
                            xMin:=GetNumber(2);
                            xMax:=GetNumber(3);
                            yMin:=GetNumber(4);
                            yMax:=GetNumber(5);
                      end;
                 PlotAxes.IsVisible:=GetBoolean(1);
                 AsXY:=GetBoolean(7);
                 If not(Expanded=GetBoolean(6)) then Expand;
                 Expanded:=GetBoolean(6);
                 AsX:=GetBoolean(8);
                 AsY:=GetBoolean(9);
                 Ok:=GetBoolean(10);
                 Cancel:=GetBoolean(11);
                 Done;
           end;

           If (Temp.xMin>=Temp.xMax) or (Temp.yMin>=Temp.yMax) then
              begin
                   Announce('Minimum vales must be less than Maximum value.');
                   Exit;
              end;

           If Ok then
                       begin;
                             Remove;
                             isVisible:=True;
                             With Temp do SetScale( xMin, yMin, xMax, yMax );
                             Scale:=Temp;
                             Display

                       end;

           If AsXY or AsX or AsY then
                 begin;
                       Remove;
                       isVisible:=True;
                       With Temp do
                            begin;
                                  If AsX or AsXY then
                                     begin;
                                           xMax:=StopEdge;
                                           xMin:=StartEdge;
                                     end;
                                  If AsY or AsXY then
                                     Begin
                                     if OffsetComponents AND
                                       (EquationType in [phiFour,KleinGordon,sineGordon,DoubleSineGordon,Diffusion])
                                       Then
                                       Begin
                                       partialX(temp1OGVec,DisplayVec);
                                       temp1OGVec.MinMax(i,yMin,i,yMax);
                                       End
                                       Else DisplayVec.MinMax(i,yMin,i,yMax);
                                     If EquationType=Schroedinger Then yMin:=-yMax;
                                     Size:=(yMax-yMin);
                                     yMin:=yMin-0.10*size;
                                     yMax:=yMax+0.10*Size;
                                     If OffSetComponents then
                                       begin;
                                       yMax:=yMax + Size/2;
                                       yMin:=yMin - Size/2;
                                       end;
                                     yMin:=FancyTrunc(yMin);
                                     ymax:=(yMax);
                                     End;
                            SetScale( xMin, yMin, xMax, yMax );
                            end;
                       Scale:=Temp;
                       Display;
                 End;
      end;

PROCEDURE WavePlot.AddSegment;
VAR
   xpos, ypos : REAL;
   inside     : BOOLEAN;
   aSegment   : SegmentPtr;
BEGIN
   LockFocus;
   Comment(Xo+Width/2-0.15,0.35,'Click and Drag to place segment. ');
   REPEAT
      CheckForEvents;
   UNTIL  event.MouseClicked;
   beep;
   MousePosn(xpos, ypos, 1,1, inside);
   if xPos<startEdge then xpos:=startEdge;
   if xPos>stopEdge then xpos:=stopEdge;
   if 1000>MaxAvail
     Then
       Begin
       Announce('Not Enough Memory for a segment.');
       Exit;
       End
     Else new(aSegment,init(xpos, @self));
   SegmentList.addItem(aSegment);
   UpdateVecs;
   AutoSetTimeInc;
   Display;
   If isVisible Then ShowDensity;
   aSegment^.Drag;
   aSegment^.Inspector;
END;

PROCEDURE WavePlot.AddSource;
VAR
   aSource : SourcePtr;
   xpos, ypos : REAL;
   inside     : BOOLEAN;
BEGIN
   LockFocus;
   Comment(Xo+Width/2-0.15,0.35,'Click and Drag to place source. ');
   REPEAT
      CheckForEvents;
   UNTIL event.MouseClicked;
   display;
   beep;
   MousePosn(xpos, ypos, 1, 1, inside);
   If 1000>MaxAvail
     Then
       Begin
       Announce('Not Enough Memory for a source.');
       Exit;
       End
     Else new(aSource);
   Inc(SourceNum);
   if xPos<startEdge then xPos:=startEdge;
   if xPos>stopEdge then xPos:=StopEdge;
   aSource^.Init(SourceNum, xpos, @self);
   SourceList.addItem(aSource);
   aSource^.Display;
   aSource^.Drag;
   aSource^.Inspector;
   If isVisible Then ShowDensity
END;

PROCEDURE WavePlot.AddDetector;
VAR
   aDetector : DetectorPtr;
   xpos, ypos : REAL;
   inside     : BOOLEAN;

BEGIN
   If (DetectNum=MaxDetNum) then
      begin
           Announce('You have exceeded the number of allowed detectors.');
           Exit;
      end
   else Inc(DetectNum);
   LockFocus;
   Comment(Xo+Width/2-0.15,0.35,'Click and Drag to place detector. ');
   REPEAT
      CheckForEvents;
   UNTIL event.MouseClicked;
   beep;
   MousePosn(xpos, ypos, 1, 1, inside);
   If 1000>MaxAvail
     Then
       Begin
       Announce('Not Enough Memory for a detector.');
       Exit;
       End
     Else New(aDetector);
   if xPos<StartEdge then xPos:=StartEdge;
   if xPos>stopEdge then xPos:=StopEdge;
   aDetector^.Init(DetectNum, xpos, @self);
   DetectorList.addItem(aDetector);
   aDetector^.Display;
   Display;
   aDetector^.Drag;
END;

PROCEDURE WavePlot.AddReflector;
VAR
   aReflector : ReflectorPtr;
   xpos, ypos : REAL;
   inside     : BOOLEAN;
BEGIN
   LockFocus;
   Comment(Xo+Width/2-0.15,0.35,'Click and Drag to place reflector. ');
   REPEAT
      CheckForEvents;
   UNTIL event.MouseClicked;
   beep;
   MousePosn(xpos, ypos, 1, 1, inside);
   if 1000>MaxAvail
     Then
       Begin
       Announce('Not Enough Memory for a reflector.');
       Exit;
       End
     Else New(aReflector);
   if xpos<startEdge then xPos:=startEdge;
   if xPos>stopEdge then xPos:=stopEdge;
   aReflector^.Init(xpos, @self);
   ReflectorList.addItem(aReflector);
   aReflector^.Display;
   Display;
   aReflector^.Drag;
END;


Procedure WavePlot.StepRun( N: Word );
Begin
Case EquationType of
  EandM:CWStepRun(N);
  KleinGordon,
  sineGordon,
  phiFour,
  doubleSineGordon:KGStepRun(N);
  diffusion:diffStepRun(n);
  Schroedinger:SchStepRun(n);
  End; {of Case}
End;

Function SchEigenValue(m:Integer):Real;
{Newton's method to estimate the WKB energy}
Var e0,e1,a0,a1,tempE,dx:Real; i,symm:Integer;
Begin
{$R-}
  if leftB=periodic Then symm:=2 Else Symm:=1;
  m:=Abs(m);
  dx:=(StopEdge-StartEdge)/(VacuumPts);
  e0:=Sqr(symm*Pi/(StopEdge-StartEdge))/2;
  e0:=Sqr(m)*e0;
  Repeat
  a0:=0;
  for i:=hiddenPts+1 to Numpts-hiddenPts do
    if (e0-IndexOGVec.VecPtr^[i])>0 Then
      a0:=a0+Sqrt(2*(e0-IndexOGVec.VecPtr^[i]));
    a0:=a0*dx/Pi/symm;
    If a0<1 Then e0:=e0*2;
  Until a0>1;
  a1:=a0;
  e1:=e0+(m-a0)*a0/Sqr(StopEdge-StartEdge);
  While Abs(a1-M)>1.0E-3 Do
  Begin
  a1:=0;
  for i:=hiddenPts+1 to Numpts-hiddenPts do
    if (e1-IndexOGVec.VecPtr^[i])>0 Then
      a1:=a1+Sqrt(2*(e1-IndexOGVec.VecPtr^[i]));
  a1:=a1*dx/Pi/symm;
  If a1=0
    Then
      Begin
      e1:=e0-Abs(e0-e1)/10.0
      End
    Else
      Begin
      tempE:=e1;
      e1:=e1+(m-a1)*(e1-e0)/(a1-a0);
      a0:=a1; e0:=tempE;
      End;
  End;
{$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
  {Announce('Action: '+Num2Str(a1,10)+' Energy: '+Num2Str(e1,10));}
  SchEigenValue:=e1;
End;

Procedure CalcSchWave(m:Integer);
Var
  x,dx,reValue,imValue,e1,k,norm: Real;
  i:                       Integer;
{$R-}
begin
  e1:=SchEigenValue(m);
  dx:=(StopEdge-StartEdge)/(VacuumPts);
  Temp1OGVec.Fill(0); {Real part of wavefunction}
  Temp2OGVec.Fill(0); {Imaginary part of wavefunction}
  if m=0 then Exit;
  If (RightB=Periodic)
    then
    begin
    x:=StartEdge-0.5*dx;
    For i:=hiddenPts to Numpts-hiddenPts+1 do
                 begin
                   if (e1-IndexOGVec.VecPtr^[i])>0
                     Then
                     k:=Sqrt(2*(e1-IndexOGVec.VecPtr^[i]))
                     Else k:=0;
                   if k>1.0e-2 Then
                     Begin
                     reValue:=Cos(2*Pi*m*(x-StartEdge)/(StopEdge-StartEdge))/Sqrt(k);
                     imValue:=Sin(2*Pi*m*(x-StartEdge)/(StopEdge-StartEdge))/Sqrt(k);
                     End
                   Else
                     Begin
                     reValue:=0;
                     imValue:=0;
                     End;
                   If (m>0) then
                     begin
                     temp1OGVec.VecPtr^[i]:=reValue;
                     temp2OGVec.VecPtr^[i]:=imValue;
                     end
                   Else
                     begin
                     temp1OGVec.VecPtr^[i]:=reValue;
                     temp2OGVec.VecPtr^[i]:=-imValue;
                     end;
                 x:=x+dx*k*(StopEdge-StartEdge)/Pi/2/m;
                 end;{of For i:= loop}
    end; {of if Boundary=periodic}
  If (RightB=Fixed) and (LeftB=Fixed) then
    begin
    x:=startEdge;
    For i:=hiddenPts+1 to Numpts-hiddenPts do
              Begin
              if (e1-IndexOGVec.VecPtr^[i])>0
                     Then
                     k:=Sqrt(2*(e1-IndexOGVec.VecPtr^[i]))
                     Else k:=0;
              if k>1.0e-2 Then
                reValue:=Sin(Pi*Abs(m)*(x-StartEdge)/(StopEdge-StartEdge))/Sqrt(k)
              Else reValue:=0;
              temp1OGVec.VecPtr^[i]:=reValue;
              temp2OGVec.VecPtr^[i]:=0;
              x:=x+dx*k*(StopEdge-StartEdge)/Pi/m;
              end;{of for i:= loop}
  end;{of if Boundry=Fixed}
norm:=0;
For i:=hiddenPts+1 to Numpts-hiddenPts do
  norm:= norm+(Sqr(temp1OGVec.VecPtr^[i])+Sqr(temp2OGVec.VecPtr^[i]));
norm:=Sqrt(norm*dx);
If norm>1.0E-10 Then
  Begin
  temp1OGVec.multScalar(1/Norm);
  temp2OGVec.multScalar(1/Norm);
  End;
{$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
end;
Function InitSchModes:Boolean;
Var
   OldColour:           Word;
   InptModeScrn:        TInputScreen;
   Ok, Cancel:          Boolean;
   i,m:                 Integer;
   ReValue,imValue,
   x,dx:                Real;
   loop:                Integer;
   e0,e1:               REAL;

Begin;
InitSchModes:=False;
OldColour:=GetColor;
With InptModeScrn do
           begin;
           Init;
           DefineInputPort( 0.31, 0.70, 0.37 ,0.90 );
           LoadLine('      WKB  Wavefunctions    ');
           LoadLine('           E>V(X)           ');
           LoadLine(' Mode Number       Amplitude');
           LoadLine('  N = 1       #F    {    }  ');
           Loadline('  N = 2       #F    {    }  ');
           Loadline('  N = 3       #F    {    }  ');
           Loadline('  N = 4       #T    {    }  ');
           Loadline('  N = 5       #F    {    }  ');
           Loadline('  N = {    }  #F    {    }  ');
           Loadline('  N = {    }  #F    {    }  ');
           LoadLine('                            ');
           LoadLine(' [  Ok  ] [Cancel] [ Help ] ');
           With ModeData do
             begin;
             SetBoolean(1,EnableMode[1]);SetNumber(2,  Amp[1]);
             SetBoolean(3,EnableMode[2]);SetNumber(4,  Amp[2]);
             SetBoolean(5,EnableMode[3]);SetNumber(6,  Amp[3]);
             SetBoolean(7,EnableMode[4]);SetNumber(8,  Amp[4]);
             SetBoolean(9,EnableMode[5]);SetNumber(10, Amp[5]);
             SetNumber(11, Mode[6]);
             SetBoolean(12,EnableMode[6]);
             SetNumber(13, Amp[6]);
             SetNumber(14, Mode[7]);
             SetBoolean(15,EnableMode[7]);
             SetNumber(16, Amp[7]);
             end;
           SetHelpFile('Wave.Hlp', 'Mode Help');
           AcceptScreen;
           Ok:=GetBoolean(17);
           Cancel:=GetBoolean(18);
           if Cancel Then
             Begin
             InptModeScrn.Done;
             InitSchModes:=False;
             Exit;
             End;
           If Ok then With ModeData do
             begin;
             EnableMode[1]:=getBoolean(1);Amp[1]:=GetNumber(2);
             EnableMode[2]:=getBoolean(3);Amp[2]:=GetNumber(4);
             EnableMode[3]:=getBoolean(5);Amp[3]:=GetNumber(6);
             EnableMode[4]:=getBoolean(7);Amp[4]:=GetNumber(8);
             EnableMode[5]:=getBoolean(9);Amp[5]:=GetNumber(10);

             Mode[6]:=Round(GetNumber(11));
             EnableMode[6]:=GetBoolean(12);
             Amp[6]:=GetNumber(13);

             Mode[7]:=Round(GetNumber(14));
             EnableMode[7]:=GetBoolean(15);
             Amp[7]:=GetNumber(16);
             end;{of With InputMode Screen}
           If Ok then
             begin
{$R-}        InitSchModes:=True;
             yVec.Fill(0);
             yPrevVec.Fill(0);
             With ModeData Do
             For m:=1 to 7 do
                 If EnableMode[m] then
                 Begin
                 CalcSchWave(Mode[m]);
                 yVec.AddVector(Amp[m],Temp1OGVec);
                 yPrevVec.AddVector(Amp[m],Temp2OGVec);
                 end;
          For i:=1 to numPts Do
            displayVec.VecPtr^[i]:=Sqrt(Sqr(yVec.vecptr^[i])+Sqr(yPrevVec.vecptr^[i]));
{$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
        AdjustForBoundry;
        end;{if OK}
      Done; { get rid of InptModeScrn}
      end;  {of With InptModeScrn}
  SetColor(OldColour);
End;{of InitSchModes}
{---------------------------------------------------------------------------------------}
Procedure CalcEMMode(m:Integer);
Var TOD,x,dx:       Real;    i:      Integer;
Begin
  Temp1OGVec.Fill(0);
  Temp2OGVec.Fill(0);
  if m=0 Then exit;
  TOD:=0;
 {$R-}
  For i:=1 to VacuumPts-1 DO TOD:=TOD+IndexSGVec.VecPtr^[i];
  TOD:=Tod/(VacuumPts-1);
  If (RightB=Periodic) and (LeftB=Periodic)
  Then
    Begin
    dx:=(StopEdge-StartEdge)/(VacuumPts)/TOD;
    x:=StartEdge-0.5*dx;
    For i:=hiddenPts to Numpts-hiddenPts+1 do
      Begin
      If m>0 Then
        temp1OGVec.VecPtr^[i]:=Sin(2*Pi*m*(x-StartEdge)/(StopEdge-StartEdge))/Sqrt(IndexOGVec.Vecptr^[i])
      Else
        temp2OGVec.VecPtr^[i]:=Sin(2*Pi*m*(x-StartEdge)/(StopEdge-StartEdge))/Sqrt(IndexOGVec.Vecptr^[i]);
      x:=x+dx;
      End;
    End {of periodic boundary}
  Else
    Begin {fixed boundary}
    dx:=(StopEdge-StartEdge)/TOD/(VacuumPts);
    x:=startEdge;
    m:=Abs(m);
    For i:=hiddenPts+1 to Numpts-hiddenPts do
      Begin
      temp1OGVec.VecPtr^[i]:=Sin(Pi*m*(x-StartEdge)/(StopEdge-StartEdge))/2/Sqrt(IndexOGVec.Vecptr^[i]);
      temp2OGVec.VecPtr^[i]:=Sin(Pi*m*(x-StartEdge)/(StopEdge-StartEdge))/2/Sqrt(IndexOGVec.Vecptr^[i]);
      x:=x+dx;
      End
    End;
{$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
End;
{---------------------------------------------------------------------------------------}
Procedure CalcKGMode(m:Integer);
Var x,dx,omega,lambda:       Real;    i:      Integer;
Begin
  Temp1OGVec.Fill(0);
  Temp2OGVec.Fill(0);
  if m=0 Then exit;
  lambda:=(StopEdge-StartEdge)/m;
  omega:=Sqrt(Sqr(2*pi/lambda)+1);
 {$R-}
  If (RightB=Periodic) and (LeftB=Periodic)
  Then
    Begin
    dx:=(StopEdge-StartEdge)/(VacuumPts);
    x:=StartEdge-0.5*dx;
    For i:=hiddenPts to Numpts-hiddenPts+1 do
      Begin
      temp1OGVec.VecPtr^[i]:=Sin(2*Pi*Abs(m)*(x-StartEdge)/(StopEdge-StartEdge));
      If m>0 Then
        temp2OGVec.VecPtr^[i]:=Sin(2*Pi*m*(x-StartEdge)/(StopEdge-StartEdge)+omega*dt)
      Else
        temp2OGVec.VecPtr^[i]:=Sin(2*Pi*Abs(m)*(x-StartEdge)/(StopEdge-StartEdge)-omega*dt);
      x:=x+dx;
      End;
    End {of periodic boundary}
  Else
    Begin {fixed boundary}
    dx:=(StopEdge-StartEdge)/(VacuumPts);
    x:=startEdge;
    m:=Abs(m);
    For i:=hiddenPts+1 to Numpts-hiddenPts do
      Begin
      temp1OGVec.VecPtr^[i]:=Sin(Pi*m*(x-StartEdge)/(StopEdge-StartEdge))/2;
      temp2OGVec.VecPtr^[i]:=Sin(Pi*m*(x-StartEdge)/(StopEdge-StartEdge))/2;
      x:=x+dx;
      End
    End;
  For i:=1 To NumPts Div 2 Do
  Begin
  temp1OGVec.VecPtr^[2*i-1]:=0;{Set the odd grid points =0}
  temp2OGVec.VecPtr^[2*i]:=0;{Set the even grid points =0}
  End;
{$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
End;

Function WavePlot.InitModes;
Var
   OldColour:           Word;
   InptModeScrn:        TInputScreen;
   Ok, Cancel:          Boolean;
   i,m:                 Integer;
   loop:                Integer;
   omega:               Real;

Begin
InitModes:=False;
If (RightB=Matched) or (LeftB=Matched) then
                            begin
                            Announce('The Modes intital condition requires Fixed or Periodic Boundaries.');
                            exit;
                            end;
If EquationType in [sineGordon, phiFour,doubleSineGordon] then
  Announce('Klein-Gordon modes will be used for this equation.');
      if Equationtype=Schroedinger Then Begin InitModes:=InitSchModes; exit end;
      OldColour:=GetColor;
      With InptModeScrn do
           begin;
                 Init;
                 DefineInputPort( 0.31, 0.70, 0.37 ,0.90 );

                 LoadLine('    WKB Wavefunctions       ');
                 LoadLine('                            ');
                 LoadLine(' Mode Number       Amplitude');
                 LoadLine('  N = 1       #F    {    }  ');
                 Loadline('  N = 2       #F    {    }  ');
                 Loadline('  N = 3       #F    {    }  ');
                 Loadline('  N = 4       #T    {    }  ');
                 Loadline('  N = 5       #F    {    }  ');
                 Loadline('  N = {    }  #F    {    }  ');
                 Loadline('  N = {    }  #F    {    }  ');
                 LoadLine('                            ');
                 LoadLine(' [  Ok  ] [Cancel] [ Help ] ');

                 With ModeData do
                      begin;
                      SetBoolean(1,EnableMode[1]);SetNumber(2,  Amp[1]);
                      SetBoolean(3,EnableMode[2]);SetNumber(4,  Amp[2]);
                      SetBoolean(5,EnableMode[3]);SetNumber(6,  Amp[3]);
                      SetBoolean(7,EnableMode[4]);SetNumber(8,  Amp[4]);
                      SetBoolean(9,EnableMode[5]);SetNumber(10, Amp[5]);

                      SetNumber(11, Mode[6]);
                      SetBoolean(12,EnableMode[6]);
                      SetNumber(13, Amp[6]);

                      SetNumber(14, Mode[7]);
                      SetBoolean(15,EnableMode[7]);
                      setNumber(16, Amp[7]);
                      end;

                 SetHelpFile('Wave.Hlp', 'Mode Help');
                 AcceptScreen;
                 Ok:=GetBoolean(17);
                 Cancel:=GetBoolean(18);
                 If Cancel then
                   Begin
                   InptModeScrn.Done;
                   InitModes:=False;
                   Exit;
                   End;
                 yVec.Fill(0);
                 yPrevVec.Fill(0);
                 InitModes:=True;
                 With ModeData do
                         begin;
                         EnableMode[1]:=getBoolean(1);Amp[1]:=GetNumber(2);
                         EnableMode[2]:=getBoolean(3);Amp[2]:=GetNumber(4);
                         EnableMode[3]:=getBoolean(5);Amp[3]:=GetNumber(6);
                         EnableMode[4]:=getBoolean(7);Amp[4]:=GetNumber(8);
                         EnableMode[5]:=getBoolean(9);Amp[5]:=GetNumber(10);
                         Mode[6]:=Round(GetNumber(11));
                         EnableMode[6]:=GetBoolean(12);
                         Amp[6]:=GetNumber(13);
                         Mode[7]:=Round(GetNumber(14));
                         EnableMode[7]:=GetBoolean(15);
                         For m:=1 to 7 do
                                 If EnableMode[m] then
                                  Begin
                                  Case EquationType of
                                    EandM: CalcEMMode(Mode[m]);
                                    KleinGordon,sineGordon,
                                    phiFour,DoubleSineGordon: CalcKGMode(Mode[m]);
                                    Else  CalcEMMode(Mode[m])
                                  End;{of Case}
                                  yVec.AddVector(Amp[m],Temp1OGVec);
                                  yPrevVec.AddVector(Amp[m],Temp2OGVec);
                                  end;
                         end;
           Done; { get rid of InptModeScrn}
           end;{of WithInputModeScreen}
    {$R-}
     {We need to adjust the yVec since different types of equations use the gid points differently.}
      Case EquationType of
        KleinGordon,sineGordon,
        phiFour,DoubleSineGordon:  {These equation types use the EvenTime=EvenGrid values only.}
          Begin
          For i:=1 to (Numpts Div 2)-1 Do
            Begin
            DisplayVec.VecPtr^[2*i]:=yVec.VecPtr^[2*i];
            DisplayVec.VecPtr^[2*i+1]:=(yvec.VecPtr^[2*i]+yVec.VecPtr^[2*i+2])/2;
            End;
          End; {of KG type equations}
        EandM: For loop:=1 to numPts Do
          DisplayVec.VecPtr^[loop]:=yVec.vecptr^[loop]+yPrevVec.vecPtr^[loop];
        diffusion: {All grid points are used. Inpterpolate to get odd grid points}
          Begin
          For i:=1 to numPts Do
            yVec.VecPtr^[i]:=yVec.vecptr^[i]+yPrevVec.VecPtr^[i];
          yPrevVec.Equate(1,yvec);
          displayVec.Equate(1,yVec);
          End;{of Diffusion Equation}
        End; {of case}
{$IFDEF DEBUG} {$R+} {$ELSE} {$R-} {$ENDIF}
  AdjustForBoundry;
  SetColor(OldColour);
End;

End.