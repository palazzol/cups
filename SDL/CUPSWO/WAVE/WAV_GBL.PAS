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

Unit Wav_GBL;
{The follosing files are needed for chain.
  Wave2.pas     //The main program.
  Wav_gbl.pas    //Global variables
  Wav_base.pas   // Atoms, buttons, and other display objects
  Wav_Plot.pas   // A generic plot objects that has scale, knows how to go full screen etc.
  Wav_Anly.pas   // The analysis routines that take data from the chain and plot it.
  Wav_graf.pas   // The drawing rountines for the chain.
  Wav_Lst.pas    // A linked list object.}

{$R-}

INTERFACE
USES
   Cups,CupsGui,CUPSmupp,CupsGrph,CupsFunc,CupsProc,Crt,Graph,CupsPars;

CONST
     MaxDetNum=4;
     Alength=1024;
     ESC=Chr(27);
     CR=Chr(13);
     defSourceColor=Red;
     DefSegmentColor=Green;
     DefDetectorColor=Blue;
     ReflectorColour = Cyan;
     defaultPts=256;
TYPE
    DvectorArray = Array[1..3*MaxDetNum] of Dvector;

    WaveEquations=(EandM,KleinGordon,SineGordon,PhiFour,DoubleSineGordon,Diffusion,Schroedinger);
    Boundry = (Fixed, Periodic,Matched);
    PlotType = (Disabled,Index_Gain, YvsTDensity, YvsT3D, FFTpos,FFTtime,YvsT, EvsT, PvsT, EnergyDist );
    FunctionType = (Sine, Gaussian, Pulse, Step, ModGaussian,WhiteNoise, UserDefined);
    SourceDir = (toLeft,toRight,bidirectional);
    DirectionType = (theLeft, theRight,theSum);
    DisplayType=(showLeft,showRight,ShowTotal);

    EnergyComponetsType = (Total, Kinetic, Potential );
    ScaleRect =
      Record
              xMin, yMin, xMax, yMax: Real;
      End;

    PlotRect =
      Record
              Left, Bottom , HSize, VSize: Real;
      End;
    AxesRect =
      Record
              xOrg, yOrg , xTic, yTic: Real;
      End;

    SourceInfo = Record
               Num:    Integer;
               Kind:   FunctionType;
               Direction: SourceDir;
               Freq,Delay,
               Amp,A:     Real;
               Radius:Integer;
               NoiseOn:      Boolean;
               Periodic:     Boolean;
               AmpNoise,
               CoherenceTime,
               PhaseShift:   Real;
               SourcePeriod: Real;
               Power:        Real;    {power delivered to wave}
               ForceAmplitude: Boolean;
               FuncStr:      String;  {user defined function}
    End;
    DetectorInfo = Record
               Num:               Integer;
               Side:              Integer;
               {AvgPeriod:         Real;
               AverageTimeNumber: Real;  }
              { IntensityOn:       Boolean;   }
    End;
    SegmentInfo = Record
               Index:     Real;
               Width:     Real;
               Gain:      Real;
    End;
    ReflectorInfo = Record
                ReflectKind: DirectionType;
    End;

        ModeType = Record
             Amp:               Array[1..7] of Real;
             Mode:              Array[1..7] of Integer;
             EnableMode:        Array[1..7] of Boolean;
    End;

  Wbase = ARRAY[1..1] OF Word;
  WbasePtr =^Wbase;

 WVector = OBJECT
    WVecPtr: WbasePtr;                 {This points to the WVector}
    Size: INTEGER;                {This is the number of elements used   }
    MemBlock: LongInt;
    PROCEDURE Init(NumberElements: INTEGER{ input });
    PROCEDURE Free;                           {This releases the memory}
    FUNCTION  Length: INTEGER;
    PROCEDURE ReSize(NumberElements: INTEGER);
    PROCEDURE Equate( s: REAL; VectB :WVector );
                                      {Equate  to s*VectB}
 End;

Var
   dT,Time,
   Energy,
   PotentialEnergy,
   KineticEnergy:               Real;
   VacuumPts,
   hiddenPts,
   NumPts:                      Integer;
   OldLPixVec,
   OldRPixVec,
   OldPixVec,
   OldColorVec,
   xPixVec:                     WVector;

   xVec,
   xSGVec,
   IndexOGVec,
   IndexSGVec,
   gainOGVec,
   gainSGVec,
   temp1OGVec,
   temp2OGVec,
   tempSGVec,
   YVec,
   yPrevVec,
   displayVec,
   initDisplayVec,
   YInitVec,
   yPrevInitVec:                Dvector;
   SingleStep,
   Running,EvenTime:            Boolean;
   DemoMode:                    Boolean;
   DemoTime:                    Real;
   AnimationSpeed:      Integer;
   StartEdge,StopEdge:  Real;
   mouseWhileRunning:   Boolean;
   LeftB,RightB:        Boundry;


   MainMenu:            TMenu;
   StopKeys,
   RunKeys:             THotKeys;
   Parser:              TParser;
   Key:                 Byte;
   Region:              Byte;
   AtOutline:           Boolean;
   DefaultPath:         String;
   IndexParserStr,
   ParserStr1,
   ParserStr2:      String;
   offsetComponents:    Boolean;
   TheThreeD:           TGraph3D;
   TheContour:          TContour;
   DetectNum:           Integer;
   SourceNum:           Integer;
   EquationType:        WaveEquations;
   ModeData:            ModeType;
   SaveStr:             String;
   Comment1,
   Comment2:            String;

Function  ToNum( Bool: Boolean): Integer;
Function  FromNum( TheNum: Integer): Boolean;
PROCEDURE GridPointDensities(i:Integer;Var ke,pe:Real);
Procedure IntegrateDensity;
Procedure ShowDensity;
Procedure VariableInit;
Procedure VariableDone;
Function  ToStr(number: Real):String;
Function  FancyTrunc(value:Real): Real;
Procedure SetRunKeys;
Procedure SetStopKeys;
Procedure TransformToSpaceGrid(OGVec, SGVec:DVector);
Procedure PartialX(partVec, vec:DVector);

IMPLEMENTATION

Uses
    Wav_Plot;

Function ToNum( Bool: Boolean): Integer;

Begin
     If Bool then ToNum:=1 else ToNum:=0;
End;

Function FromNum( TheNum: Integer): Boolean;

Begin
     If TheNum=1 then FromNum:=True else FromNum:=False;
End;

Function ToStr(number: Real):String;
var      Temp: string;
Begin
Str( number:4:1,Temp);
ToStr:=temp;
End;

Function FancyTrunc(value:Real): Real;
Var i,PowerOfTen:    Integer;
      positive:        Boolean;
Begin
  if Value=0 Then Exit;
  if Value>0 Then Positive:=TRUE ELSE Positive:=FALSE;
  Value:=Abs(Value);
  PowerOfTen:=Trunc(Ln(Value)/Ln(10));{Get the exponent}
  If PowerOfTen >=0
    THEN
      For i:=1 To PowerOfTen Do Value:=Value/10.0
    ELSE
      For i:=-1 DownTo PowerOfTen Do  Value:=Value*10.0;
   Value:=Trunc(50*Value)/50.0;
   If PowerOfTen >=0
  THEN
   For i:=1 To PowerOfTen Do Value:=Value*10.0
  ELSE
   For i:=-1 DownTo PowerOfTen Do  Value:=Value/10.0;
   If Positive=FALSE THEN Value:=-Value;
   FancyTrunc:=Value;
End;
Procedure PartialX(partVec, vec:DVector);
Var i:Integer;
Begin
For i:=1 to 2 Do partVec.VecPtr^[i]:=0;
For i:=vec.Length-2 to vec.Length Do partVec.VecPtr^[i]:=0;
For i:=3 to Vec.Length-2 Do
  partVec.VecPtr^[i]:=
    (vec.VecPtr^[i-2]-16*vec.VecPtr^[i-1]+16*vec.VecPtr^[i+1]-vec.VecPtr^[i+2])/6/(xVec.VecPtr^[i+1]-xvec.VecPtr^[i-1]);
End;
Procedure TransformToSpaceGrid(OGVec, SGVec:DVector);
Var
  i,count:Integer;
  Interpolation:Real;
Begin
  if OGVec.Length<>numPts then Announce('Bug!  TransfromToSpaceGrid OpticalGrigVec wrong length');
  if SGVec.Length<>vacuumPts then Announce('Bug!  TransfromToSpaceGrid SpaceGgridVec wrong length');
  count:=hiddenPts;
  for i:=1 to VacuumPts Do
  Begin
    While (xVec.VecPtr^[count]<xSGVec.VecPtr^[i]) and (Count<(NumPts)) Do Inc(Count);
    If Count>2
    Then Interpolation:=(OGVec.VecPtr^[count]-OGVec.VecPtr^[count-2])*
                        (xVec.VecPtr^[count]-xSGVec.VecPtr^[i])/(xVec.VecPtr^[count]-xVec.VecPtr^[count-2])
    Else Interpolation:=(OGVec.VecPtr^[count]-OGVec.VecPtr^[count+2])*
                        (xVec.VecPtr^[count]-xSGVec.VecPtr^[i])/(xVec.VecPtr^[count]-xVec.VecPtr^[count+2]);
    SGVec.VecPtr^[i]:=OGVec.vecPtr^[count]-Interpolation;
  End;
End;


Procedure SetRunKeys;
Begin;
      With RunKeys do
           Begin;
                 Init(6);
                 key[1]:='F1-Help';
                 key[2]:='F2-Pause';
                 Case EquationType of
                   EandM,Schroedinger:
                     If offsetComponents then key[3]:='F3-N0 Offset' else key[3]:='F3-Offset';
                   Else
                     If offsetComponents then key[3]:='F3-D/Dx' else key[3]:='F3-Function';
                 End;
                 If dt<0 then key[4]:='F4-Neg dt ' Else  key[4]:='F4-Pos dt ';
                 key[5]:='F5-Faster';
                 key[6]:='F6-Slower';
           End;
End;
Procedure SetStopKeys;
Begin;
      With StopKeys do
           Begin;
                 Init(6);
                 key[1]:='F1-Help';
                 If Time=0 then key[2]:='F2-Run'
                    else key[2]:='F2-Continue';
                 Case EquationType of
                   EandM,Schroedinger: If offsetComponents then key[3]:='F3-N0 Offset' else key[3]:='F3-Offset';
                   Else If offsetComponents then key[3]:='F3-D/Dx' else key[3]:='F3-Function';
                 End;
                 key[4]:='F4-Step';
                 key[5]:='F7-Reset';
                 key[6]:='F10-Menu';
           End;
End;


Procedure VariableInit;

Var
   i:   Integer;
   dx,x,exponent:REAL;

Begin;
      StartEdge:=-0.5;
      StopEdge:=0.5;
      Time:=0;
      EvenTime:=True;
      DemoMode:=False;
      DemoTime:=10;
      Energy:=0;
      VacuumPts:=defaultPts;
      hiddenPts:=1;
      numPts:=defaultPts+2*hiddenPts;  {Must be even!}
      dt:=(stopEdge-StartEdge)/VacuumPts;
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
      dX:=(StopEdge-StartEdge)/(VacuumPts-1);
      x:=StartEdge;
      for i:=1 to VacuumPts Do  {fill the x values on the space grid}
      Begin
        xSGVec.Put(i,x);
        x:=x+dx;
      end;
      x:=StartEdge-dx;
      for i:=1 to numPts do  {fill the x values on the optical grid}
        Begin
        xVec.VecPtr^[i]:=x;
        x:=x+dx;
        End;

      {fill yVec with a standing wave for the initial condition}
      yVec.Put(1,0);
      dX:=(StopEdge-StartEdge)/(VacuumPts);
      x:=StartEdge;
      for i:=2 to Numpts-1 Do
        Begin
        yVec.Vecptr^[i]:=Sin(2*Pi*(x-StartEdge)/(stopEdge-StartEdge))/2;
        x:=x+dx;
        End;
      yVec.Put(numPts,0);
      yprevVec.Equate(1,yVec);
      yInitVec.Equate(1,yVec);
      yPrevInitVec.Equate(1,yPrevVec);
      For i:=1 to numPts Do
          DisplayVec.VecPtr^[i]:= yVec.vecptr^[i]+yPrevVec.vecPtr^[i];

      initDisplayVec.Equate(1,DisplayVec);
      AnimationSpeed:=-8;
      MouseWhileRunning:=True;
      Running:=False;
      LeftB:=Fixed;
      RightB:=Fixed;
      SingleStep:=False;
      EquationType:=EandM;
      DetectNum:=0;
      SourceNum:=0;
      With ModeData do
        Begin
           For i:=1 to 7 Do
             Begin
                Amp[i]:=0.5;
                EnableMode[i]:=False;
                Mode[i]:=i;
             End;
        EnableMode[4]:=TRUE;
        end;
      IndexSGVec.Fill(1);
      gainSGVec.Fill(0);
      IndexOGVec.Fill(1);
      gainOGVec.Fill(0);
      IntegrateDensity;
      SaveStr:='Data1.wav';
      Comment1:='';
      Comment2:='';
      IndexParserStr:='';
      ParserStr1:='';{'tanh((x-0.5*t+20)/Sqrt((1-Sqr(0.5))*2))'; }
      ParserStr2:=''; {'-tanh((x+0.5*t-20)/Sqrt((1-Sqr(0.5))*2))-1'; phi-four kink/antiKink}
End;
Procedure VariableDone;

Var
   i:   Integer;

Begin;
      xVec.Free;
      YVec.Free;
      yPrevVec.Free;
      yPrevInitVec.Free;
      yInitVec.Free;
      IndexSGVec.Free;
      gainSGVec.Free;
      IndexOGVec.Free;
      gainOGVec.Free;
      OldRPixVec.Free;
      OldLPixVec.Free;
      OldPixVec.Free;
      OldColorVec.Free;
      xPixVec.Free;
      temp1OGVec.Free;
      temp2OGVec.Free;
      displayVec.Free;
      initDisplayVec.Free;
      tempSGVec.Free;
      xSGVec.Free;
      SegMentList.Done(True);
      SourceList.Done(True);
      DetectorList.Done(True);
      ReflectorList.Done(True);
End;

PROCEDURE GridPointDensities(i:Integer;Var ke,pe:Real);
Var dx:Real;
Begin
  dX:=(StopEdge-StartEdge)/(VacuumPts-1);
  if i<hiddenPts+1 then i:=hiddenPts+1;
  if i>numpts-hiddenPts then i:=numpts-hiddenPts;
     Case Equationtype of
     eAndM:
             begin
              ke:=IndexOGVec.VecPtr^[i]*Sqr(yVec.VecPtr^[i] + yPrevVec.VecPtr^[i])/2;  {electric field energy}
              pe:=IndexOGVec.VecPtr^[i]*Sqr(yVec.VecPtr^[i] - yPrevVec.VecPtr^[i])/2;  {magnetic field energy}
             end;
     Schroedinger:
             begin
             ke:=sqr(yVec.VecPtr^[i]);
             pe:=sqr(yPrevVec.VecPtr^[i]);
             end;
     Else
             begin
              ke:=DisplayVec.VecPtr^[i];
              pe:=0;
             end;

     End; {of Case}
End;

PROCEDURE IntegrateDensity;
Var
   i:  Integer;
   ke,pe:Real;
Begin
  KineticEnergy:=0;
  PotentialEnergy:=0;
  For i:=hiddenPts+1 to Numpts-hiddenPts DO
    Begin
    GridPointDensities(i,ke,pe);
    KineticEnergy:=KineticEnergy+ke;
    PotentialEnergy:=PotentialEnergy+pe;
    End;
  if rightB=Fixed then
    Begin
    GridPointDensities(numPts-HiddenPts+1,ke,pe);
    KineticEnergy:=KineticEnergy+ke/2;
    PotentialEnergy:=PotentialEnergy+pe/2;
    End;
  if leftB=Fixed then
    Begin
    GridPointDensities(hiddenPts+1,ke,pe);
    KineticEnergy:=KineticEnergy-ke/2;
    PotentialEnergy:=PotentialEnergy-pe/2;
    End;
  KineticEnergy:=KineticEnergy*(StopEdge-StartEdge)/(Numpts-2*hiddenPts-1);
  PotentialEnergy:=PotentialEnergy*(StopEdge-StartEdge)/(Numpts-2*hiddenPts-1);
  energy:= KineticEnergy + PotentialEnergy;
END;

PROCEDURE ShowDensity;
Var
i:                   Integer;
Begin
     Case EquationType of
       EandM:
         Begin
         IntegrateDensity;
         Rubout(37,1,23,LightBlue);
         Print(37,1,'Energy = '+Num2Str(energy,6)+' J');
         End;
       Schroedinger:
         Begin
         IntegrateDensity;
         Rubout(37,1,23,LightBlue);
         Print(37,1,'<Psi*Psi>='+Num2Str(energy,6));
         End;
       Diffusion:
         Begin
         IntegrateDensity;
         Rubout(37,1,23,LightBlue);
         Print(37,1,'Mass '+Num2Str(energy,6));
         End;
      else Energy:=0;
     End; {of Case}
     Rubout(5,1,16,LightBlue);
     Print(5,1,'  t = '+Num2Str(time,6)+' s');
END;

PROCEDURE WVector.Init(NumberElements: INTEGER);
 VAR
    i: INTEGER;
    MemReq,MaxBlock: String[10];
BEGIN
   If NumberElements<0 then Begin Error('Error initalizing WVector: NumElem <0'); Exit;End;
   Size := NumberElements;
   MemBlock:= longint(Size)*SizeOf(WORD);
   IF ((MemBlock > MaxAvail) OR (MemBlock >= 65521)) THEN
   BEGIN
     Str(MemBlock:10,MemReq);
     Str(MaxAvail:10,MaxBlock);
     Error('WVector.Init: Request for '+ MemReq +
               ' exceeds free memory ' + MaxBlock + 'or 65521');
     MemBlock := 0;
     WVecPtr   := NIL;
     Size     := 0;
   END
   ELSE
     IF size=0 THEN WVecPtr:=nil ELSE
     BEGIN
       GetMem(WVecPtr,MemBlock);
       FOR i:=1 TO Size DO WVecPtr^[i]:=0;
     END
 END;

PROCEDURE WVector.Free;
BEGIN
   FreeMem(wVecPtr,MemBlock);
   Size:= 0;
   wVecPtr:=Nil;
   memBlock:=0;
END;
FUNCTION WVector.Length: INTEGER;
BEGIN
    Length:= Size;
END;
PROCEDURE WVector.ReSize(NumberElements: INTEGER);
var temp : WVector; i:Integer;
BEGIN
   If self.size=NumberElements Then Exit;
   temp.init(Size);
   For i :=1 To Size Do temp.wVecptr^[i]:=self.wVecPtr^[i];
   {temp.equate(1,self); }
   Free;
   Init(NumberElements);
   {equate(1,temp);}
   If numberElements <temp.length
     Then For i:=1 to numberElements Do self.wVecPtr^[i]:=temp.wvecPtr^[i]
     Else For i:=1 to temp.Length Do self.wVecPtr^[i]:=temp.wvecPtr^[i];
   temp.free;
END;

PROCEDURE WVector.Equate(s: REAL; VectB: WVector {input});
VAR
  i: INTEGER;
BEGIN
If Self.Length<>VectB.Length Then
  Begin
  Announce('Equate called with vectors of unequal length.');
  Exit;
  end;
   FOR i:=1 TO Size DO
       wVecPtr^[i]:= Round(s*VectB.wVecPtr^[i])
End;

Begin
End.