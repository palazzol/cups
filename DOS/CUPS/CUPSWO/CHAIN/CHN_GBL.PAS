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

Unit Chn_GBL;
{$R-}
INTERFACE
USES
   Cups,CupsGui,CUPSmupp,CupsGrph,CupsFunc,CupsProc,Cupspars,Crt,Graph;

CONST
     InitNum=34;
     Alength=1024;
     ESC=Chr(27);
     CR=Chr(13);
TYPE
    Boundry = (Fixed, Free, Continuous);
    PlotType = (Disabled, YvsT, VvsT, VvsY, EvsT, YvsTDensity, YvsT3D, YvsY, SvsT,FFT_X,FFT_V,FFT_T);
    SourceType = (NoSource, Sine, Gaussian, Pulse, Step, ModGaussian,UserDefined);
    ExcitationType = (Displacement,Force);

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
    AtomInfo = Record
             Num:       Word;
             y,v,m,
             k1,k2,k3,
             Damp,Self: Real;
    End;

    SourceInfo = Record
               Kind:      SourceType;
               Excite:    ExcitationType;
               Periodic:  Boolean;
               Freq,
               Amp,A,
               Xo:        Real;
               FuncStr:  String;
    End;

    ModeType = Record
             Amp1,Amp2,Amp3,
             Amp4,Amp5,Amp6,
             Amp7,
             Mode6,Mode7:        Real;
             B1,B2,B3,B4,
             B5,B6,B7:           Boolean;
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
   Version:     Real;
   dT,Time,
   demoTime,
   FrameVelocity,
   frameShift,
   SourcePeriod,
   Energy,
   KineticEnergy,
   PotentialEnergy:     Real;

   YVec,VVec,MVec,
   K1Vec,K2Vec,K3Vec,
   KSVec,KDVec,
   YInitVec,VInitVec ,
   AVec,Yold:           Dvector;

   MouseWhileRunning,
   ShowEnvelope,
   MassScaling,
   ShowV,TPlus,
   SingleStep,
   DemoMode,
   MovingFrame,
   Running,Loading:             Boolean;
   Cell,
   AnimationSpeed:      Integer;
   LeftB,RightB:        Boundry;
   TheContour:          TContour;
   TheThreeD:           TGraph3D;

   MainMenu:            TMenu;
   StopKeys,
   RunKeys:             THotKeys;
   Key,
   Region:              Byte;
   RadScale,
   AtOutline:           Boolean;
   DefaultPath,
   ParserStr1,
   ParserStr2:         String;
   Parser:              TParser;
   ModeData:            ModeType;

Function Accel(i:INTEGER; y1,y2,y3,v2:REAL):REAL;
Procedure ShowEnergy;
Procedure CalcEnergy;
Procedure AdjustForBoundry;
Function  FancyTrunc(value:Real): Real;

IMPLEMENTATION
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
Function Accel(i:INTEGER; y1,y2,y3,v2:REAL):REAL;

VAR mass:REAL;
   sign:Boolean;
   a:Real;
BEGIN
  mass:=Mvec.VecPtr^[i];
  a:=(K1vec.VecPtr^[i]*(y3-y2) + K1vec.VecPtr^[i-1]*(y1-y2) +
          K2vec.VecPtr^[i]*Sqr(y3-y2) - K2vec.VecPtr^[i-1]*Sqr(y2-y1)+
          K3vec.VecPtr^[i]*(y3-y2)*SQR(y3-y2) +
          K3vec.VecPtr^[i-1]*(y1-y2)*SQR(y1-y2) -
          KSvec.VecPtr^[i]*y2 - KDvec.vecptr^[i]*v2)/mass;
if a*dt >3.0E7 Then Comment(0.12,0.4,'Warning:  Particle velocity approaching the speed of light!');
Accel:=a;
END;

PROCEDURE CalcEnergy;
VAR
   MaxNum,
   i,k:Integer;
   start,last: Integer;

BEGIN
MaxNum:=Yvec.Length;
last:=MaxNum-2;start:=2; {defaults}
energy:=0;  {default initial value}
KineticEnergy:=0;
PotentialEnergy:=0;
If RightB=Free then
                   begin;
                         KineticEnergy:=Mvec.Vecptr^[MaxNum-1]*Sqr(Vvec.VecPtr^[MaxNum-1])/2;
                         PotentialEnergy:=KSvec.Vecptr^[MaxNum-1]*Sqr(Yvec.Vecptr^[MaxNum-1])/2;
                   end;
k:=2;
If LeftB=Continuous THEN start:=1;

For i:=start to last do
    begin;
          KineticEnergy:=KineticEnergy + Mvec.Vecptr^[i]*Sqr(Vvec.VecPtr^[i])/2;
          PotentialEnergy:=PotentialEnergy +
              K1vec.VecPtr^[i]*Sqr(Yvec.Vecptr^[i]-Yvec.VecPtr^[i+1])/2+
              K2vec.VecPtr^[i]*(Sqr(Yvec.Vecptr^[i]-Yvec.VecPtr^[i+1])*(Yvec.Vecptr^[i]-Yvec.VecPtr^[i+1]))/3+
              K3vec.Vecptr^[i]*Sqr(Sqr(Yvec.Vecptr^[i]-Yvec.VecPtr^[i+1]))/4+
              KSvec.Vecptr^[i]*Sqr(Yvec.Vecptr^[i])/2;
    end;
Energy:=KineticEnergy + PotentialEnergy;
END;
PROCEDURE ShowEnergy;
Begin
     Rubout(42,1,18,LightBlue);
     Print(42,1,' Energy = '+Num2Str(energy,6)+' J');

     Rubout(5,1,16,LightBlue);
     Print(5,1,'  t = '+Num2Str(time,6)+' s');

     If MovingFrame Then
       Begin
       Rubout(24,1,14,Red);
       If frameVelocity >0 Then Print(24,1,'--> Moving -->');
       If frameVelocity <0 Then Print(24,1,'<-- Moving <--');
       End;
END;
Procedure AdjustForBoundry;
Var k,maxNum:INTEGER;
Begin
  K:=2; maxNum:=yVec.Length;
                Case LeftB of
                     Fixed: YVec.VecPtr^[k]:=0;
                     Continuous:  begin;
                                  Yvec.VecPtr^[1]:=Yvec.VecPtr^[MaxNum-1];
                                  Vvec.VecPtr^[1]:=Vvec.VecPtr^[MaxNum-1];
                                  Avec.VecPtr^[1]:=Avec.VecPtr^[MaxNum-1];
                                  end;
                     End;


                Case RightB of
                Fixed:Yvec.VecPtr^[MaxNum-1]:=0;
                   Continuous:  begin;
                                Yvec.VecPtr^[MaxNum]:=Yvec.VecPtr^[k];
                                Vvec.VecPtr^[MaxNum]:=Vvec.VecPtr^[k];
                                Avec.VecPtr^[MaxNum]:=Avec.VecPtr^[k];
                                end;
                     End;
End;
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
   FreeMem(WVecPtr,MemBlock);
   Size:= 0;
   WVecPtr:=Nil;
   MemBlock:=0;
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
   For i :=1 To Size Do temp.WVecptr^[i]:=self.WVecPtr^[i];
   Free;
   Init(NumberElements);
   If numberElements <temp.length
     Then For i:=1 to numberElements Do self.WVecPtr^[i]:=temp.WvecPtr^[i]
     Else For i:=1 to temp.Length Do self.WVecPtr^[i]:=temp.WvecPtr^[i];
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
       WVecPtr^[i]:= Round(s*VectB.WVecPtr^[i])
End;

Begin
End.