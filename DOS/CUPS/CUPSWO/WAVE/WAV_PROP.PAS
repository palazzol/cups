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
UNIT Wav_Prop;
INTERFACE

USES
   Wav_base,Wav_Gbl, Wav_plot,Wav_Graf,
   Cups, CupsGui, CupsMupp, CupsGrph, CupsFunc, CupsProc, CRT, GRAPH, CupsPars;

  Procedure KGStepRun( N: Word );
  Procedure CWStepRun( N: Word );
  Procedure diffStepRun( N: Word );
  Procedure SchStepRun( N: Word );
  Procedure adjustforBoundry;


IMPLEMENTATION
Procedure  CWAdjustforBoundry;
var
   two:         Integer;
   ref,dx1,dx2:     Real;
Begin
two:=2;
    CASE RightB OF
        FIXED: yPrevVec.VecPtr^[numpts]:=-yVec.VecPtr^[numpts];
        PERIODIC:
          BEGIN
          {reflect the ONLY the right wave as it leaves the medium.}
          dx2:=xVec.VecPtr^[Numpts]-xVec.VecPtr^[Numpts-1];
          dx1:=xVec.VecPtr^[Numpts-1]-xVec.VecPtr^[Numpts-2];
          ref:=(dx2-dx1)/(dx1+dx2);
          yPrevVec.VecPtr^[NumPts-1]:=yPrevVec.VecPtr^[NumPts-1]+ref*yVec.VecPtr^[NumPts-1];
          yVec.VecPtr^[NumPts-1]:=yVec.VecPtr^[NumPts-1]+ref*yVec.VecPtr^[NumPts-1];
          {reflect the ONLY the LEFT wave as it leaves the medium.}
          yVec.VecPtr^[two]:=yVec.VecPtr^[two]-ref*yPrevVec.VecPtr^[two];
          yPrevVec.VecPtr^[two]:=yPrevVec.VecPtr^[two]-ref*yPrevVec.VecPtr^[two];
          {Make the hidden points equal the other end of the medium.}
          yVec.VecPtr^[Numpts] := yVec.VecPtr^[two];
          yPrevVec.VecPtr^[Numpts]:=yPrevVec.VecPtr^[two];
          yVec.VecPtr^[1] := yVec.VecPtr^[Numpts-1];
          yPrevVec.VecPtr^[1]:=yPrevVec.VecPtr^[NumPts-1];
          END;
        MATCHED: yPrevVec.VecPtr^[numpts]:=0;

        END; {of CASE RBound}
    CASE LeftB OF
        FIXED:BEGIN
              yVec.VecPtr^[two]:=-yPrevVec.VecPtr^[two];
              yVec.VecPtr^[1]:=0;
              yPrevVec.VecPtr^[1]:=0;
              END;
        Matched:yVec.VecPtr^[1]:=0;
        END;{of CASE LBound}
End;

Procedure AdjustForBoundry;
Var two:Integer;
Begin
If   (EquationType=EandM) Then
    Begin
    CWAdjustForBoundry;
    Exit;
    End;
two:=2;
    CASE RightB OF
        FIXED: BEGIN
               yVec.VecPtr^[numpts-hiddenPts+1]:=0;
               yPrevVec.VecPtr^[numpts-hiddenPts+1]:=0;
               END;
        PERIODIC:BEGIN
                 yVec.VecPtr^[hiddenPts] := yVec.VecPtr^[Numpts-hiddenPts];
                 yPrevVec.VecPtr^[hiddenPts]:=yPrevVec.VecPtr^[NumPts-hiddenPts];
                 END;
        MATCHED:BEGIN
               yVec.VecPtr^[numpts]:=2*yVec.VecPtr^[numpts-1]-yVec.VecPtr^[numpts-2];
               yPrevVec.VecPtr^[numpts]:=2*yPrevVec.VecPtr^[numpts-1]-yPrevVec.VecPtr^[numpts-2];
               END;

        END; {of CASE RBound}
    CASE LeftB OF
        FIXED:BEGIN
              yVec.VecPtr^[hiddenPts]:=-yVec.VecPtr^[hiddenPts+2];
              yVec.VecPtr^[hiddenPts+1]:=0;
              yPrevVec.VecPtr^[hiddenPts]:= -yPrevVec.VecPtr^[hiddenPts+2];
              yPrevVec.VecPtr^[hiddenPts+1]:=0;
              END;

        PERIODIC:
          BEGIN
          yVec.VecPtr^[Numpts-hiddenPts+1] := yVec.VecPtr^[hiddenPts+1];
          yPrevVec.VecPtr^[Numpts-hiddenPts+1]:=yPrevVec.VecPtr^[hiddenPts+1];
          END;
        Matched:BEGIN
              yVec.VecPtr^[1]:=2*yVec.VecPtr^[two]-yVec.VecPtr^[two+1];
              yPrevVec.VecPtr^[1]:= 2*yPrevVec.VecPtr^[two]-yPrevVec.VecPtr^[two+1];
              END;
        END;{of CASE LBound}
End;

Procedure CWReflections;
Var
   i,loop:              INTEGER;
   tempRight,TempLeft:  REAL;
   dx1,dx2,ref:         Real;
   YPtr,yPrevPtr,xPtr:   DBasePtr;
Begin
  xPtr:=xVec.VecPtr;
  yPtr:=yVec.VecPtr;
  yPrevPtr:=yPrevVec.VecPtr;
  loop:=2;
  dx1:=xPtr^[loop]-xPtr^[loop+1];
  For loop:=3 to numPts-2 Do
    Begin
    dx2:=xPtr^[loop]-xPtr^[loop+1];
    if Abs(dx1-dx2)>100.0*macheps Then  {index has changed}
    if (ReflectorList.nodes=nil) or (ReflectorList.NoConflict(loop)) Then
      Begin
      ref:=(dx2-dx1)/(dx1+dx2)/2;
      tempLeft:=yPrevPtr^[loop];
      tempRight:=yPtr^[loop];
      yPrevPtr^[loop]:=yPrevPtr^[loop]+ref*(tempRight-tempLeft);
      yPtr^[loop]:=yPtr^[loop]+ref*(tempRight-tempLeft);
      end;
    dx1:=dx2;
    End;
End;
Procedure CWGain;
Var
  Loop:                 Integer;
  YPtr,yPrevPtr,xPtr:   DBasePtr;
  dx1,gain:             Real;

Begin
  xPtr:=xVec.VecPtr;
  yPtr:=yVec.VecPtr;
  yPrevPtr:=yPrevVec.VecPtr;
  if theWave.SaturationOn then
    Begin
    Energy:=0;
    For loop:=2 To NumPts-1 Do
      Energy:=Energy+
              IndexOGVec.VecPtr^[loop]*Sqr(yPtr^[loop] + yPrevPtr^[loop])/2+
              IndexOGVec.VecPtr^[loop]*Sqr(yPtr^[loop] - yPrevPtr^[loop])/2;
    Energy:=Energy*(StopEdge-StartEdge)/(Numpts-2*hiddenPts-1);
    End;
  For Loop:=2 to NumPts-1 Do
    if gainOGVec.VecPtr^[loop]<>0 Then
    Begin
    dx1:=xPtr^[loop+1]-xPtr^[loop];
    if theWave.SaturationOn And (gainOGVec.VecPtr^[loop]>0)
      Then gain:=1+dx1*gainOGVec.VecPtr^[loop]*(1-Energy/10)
      Else gain:=1+dx1*gainOGVec.VecPtr^[loop];
    if gain<0 then gain:=0;
    yPtr^[loop]:=yPtr^[loop]*gain;
    yPrevPtr^[loop]:=yPrevPtr^[loop]*gain;
    End;
End;

Procedure CWStepRun(N:Word);
Var
   i,loop:              INTEGER;
   tempRight,TempLeft:  REAL;
   dx1,dx2,ref:         Real;
   gain:                 Real;
   YPtr,yPrevPtr,xPtr:   DBasePtr;

Begin
xPtr:=xVec.VecPtr;
yPtr:=yVec.VecPtr;
yPrevPtr:=yPrevVec.VecPtr;
for i := 1 to N do
  Begin
  If SegmentList.Nodes<>Nil then CWReflections;
  If SourceList.Nodes<>Nil Then SourceList.updateDVec;  {have the sources add to yVec and yPrevVec.}
  If ReflectorList.Nodes<>Nil Then ReflectorList.updateDVec;
  cwAdjustForBoundry;
  tempRight:=yPtr^[Numpts]; {save the last point}
  tempLeft:=yPrevPtr^[1];   {save the first point}
  For Loop:=Numpts DownTo 2 Do yPtr^[loop]:=yPtr^[loop-1]; {Right Wave}
  yPtr^[1]:=tempRight;
  For Loop:=1 To Numpts-1 Do yPrevPtr^[loop]:=yPrevPtr^[loop+1];  {Left Wave}
  yPrevPtr^[numPts]:=tempLeft;
  time:=time+dt;
  if SegmentList.Nodes<>Nil Then CWGain;
  {reflection due to index change}
  If (SegmentList.Nodes<>Nil) then CWReflections;
  End;
cwAdjustForBoundry;
For loop:=1 to Numpts Do Displayvec.vecPtr^[loop]:=yPtr^[loop]+yPrevPtr^[loop];
End;


Procedure KGStepRun( N: Word );
VAR
   i, loop:            INTEGER;
   overflow:           BOOLEAN;

   Function Fun(u:Real):Real;
   Begin
     Case EquationType Of
     KleinGordon:Fun:=u;
     sineGordon:Fun:=Sin(u);
     phiFour:Fun:=-u*(1-Sqr(u));
     doubleSineGordon:Fun:=Sin(u)+Sin(u/2)/2;
     End;{of Case}
   End;
BEGIN
Overflow:=False;
for i := 1 to N do
   Begin
   AdjustForBoundry;
   SourceList.updateDVec;  {have the sources add to uVec and yPrevVec.}
   ReflectorList.updateDVec;
   temp1OGVec.Equate(1, yVec); {save the old values during calculation.}
   If EvenTime
   then
     for loop := 1 To (NumPts Div 2)-1 Do
     yVec.VecPtr^[2*loop+1]:=
     -yPrevVec.VecPtr^[2*loop+1]+Temp1OGVec.VecPtr^[2*loop+2]+Temp1OGVec.VecPtr^[2*loop]-
     Fun((Temp1OGVec.VecPtr^[2*loop+2]+Temp1OGVec.VecPtr^[2*loop])/2)*Sqr(dt)
   else  {odd Time}
    for loop := 1 To (NumPts Div 2)-1 Do
     yVec.VecPtr^[2*loop]:=
     -yPrevVec.VecPtr^[2*loop]+Temp1OGVec.VecPtr^[2*loop+1]+Temp1OGVec.VecPtr^[2*loop-1]-
     Fun((Temp1OGVec.VecPtr^[2*loop+1]+Temp1OGVec.VecPtr^[2*loop-1])/2)*Sqr(dt);
   {NOTE we now have the wave at THREE times stored in yVec, tempVec, yPrevVec}
   yPrevVec.Equate(1, temp1OGVec);
   time := time + dt;
   EvenTime := NOT(EvenTime);
   if i=n Then  {Fill the DisplayVec for drawing!}
     For loop:=2 To (NumPts DIV 2)-1 Do
       if EvenTime Then
         Begin
         DisplayVec.VecPtr^[2*loop-1]:=(yVec.VecPtr^[2*loop]+yVec.VecPtr^[2*loop-2])/2;
         DisplayVec.VecPtr^[2*loop]:=yVec.VecPtr^[2*loop];
         end
       Else
         Begin
         DisplayVec.VecPtr^[2*loop-1]:=yVec.VecPtr^[2*loop-1];
         DisplayVec.VecPtr^[2*loop]:=(yVec.VecPtr^[2*loop-1]+yVec.VecPtr^[2*loop+1])/2;
         End;
  END; { of NumSteps}
For loop:=1 to Numpts Do
         if DisplayVec.VecPtr^[loop]>100 then
         Begin
         DisplayVec.VecPtr^[loop]:=100;
         running:=FALSE;
         overFlow:=True;
         End;
If OverFlow Then
  Begin
  Announce('Caution: Wavefunction > 100.');
  ClearMKBuffers;
  end;
If EquationType=SineGordon Then
  For loop:=1 To NumPts Do
    DisplayVec.VecPtr^[loop]:=
      DisplayVec.vecPtr^[loop]-8*Pi*Trunc(DisplayVec.vecPtr^[loop]/8/Pi);
END; {of KGSTEP}

Procedure DiffStepRun( N: Word );
VAR
   i, loop: INTEGER;
   two:INTEGER;
   dx: REAL;
   alpha: REAL;

BEGIN
dx:=(StopEdge-StartEdge)/(VacuumPts-1);
alpha:=dt*2/Sqr(dx);
for i := 1 to N do
   Begin
   AdjustForBoundry;
   SourceList.updateDVec;  {have the sources add to uVec and yPrevVec.}
   temp1OGVec.Equate(1, yVec); {save the old values during calculation.}
   for loop := 2 To NumPts-1 Do
     yVec.VecPtr^[loop]:=
     (yPrevVec.VecPtr^[loop]+IndexOGVec.vecPtr^[loop]*alpha*
     (Temp1OGVec.VecPtr^[loop+1]+Temp1OGVec.VecPtr^[loop-1]-yPrevVec.VecPtr^[loop]))/(1+IndexOGVec.vecPtr^[loop]*alpha);

   {NOTE we now have the wave at THREE times stored in yVec, tempVec, yPrevVec}
   if i=n Then
     For loop:=2 to numPts-2 Do
       DisplayVec.VecPtr^[loop]:=
           (yVec.vecptr^[loop]+yPrevVec.vecPtr^[loop]+Temp1OGVec.VecPtr^[loop-1]+Temp1OGVec.VecPtr^[loop+1])/4;
   yPrevVec.Equate(1, temp1OGVec);
   time := time + dt;
   EvenTime := NOT(EvenTime);
  END; { of NumSteps}
END; {of STEP}

Procedure SchStepRun( N: Word );
VAR
   i, loop: INTEGER;
   two:INTEGER;
   dx: REAL;
   alpha: REAL;
   overFlow:Boolean;
   startIndex, stopIndex:Integer;

BEGIN
dx:=(StopEdge-StartEdge)/(VacuumPts-1);
alpha:=dt/Sqr(dx);
overFlow:=False;
AdjustForBoundry;
for i := 1 to N do
   Begin
   { SourceList.updateDVec;  have the sources add to uVec and yPrevVec. Sources not implemented for SchODE}
   if leftB=Matched Then startIndex:=2 Else startIndex:= HiddenPts-1;
   if rightB=Matched Then stopIndex:=numPts-1 Else stopIndex:= numpts-HiddenPts+1;
   for loop := startindex To stopIndex Do
     yPrevVec.VecPtr^[loop]:=yPrevVec.VecPtr^[loop]+
         alpha*(yVec.VecPtr^[loop+1]+yVec.VecPtr^[loop-1]-2*yVec.VecPtr^[loop])-
         2.0*IndexOGVec.vecPtr^[loop]*yVec.VecPtr^[loop]*dt;
   time:=time+dt;
  { SourceList.updateDVec;  have the sources add to uVec and yPrevVec.  Sources not implemented for SchODE}
   AdjustForBoundry;
   for loop := startIndex To stopIndex Do
     yVec.VecPtr^[loop]:=yVec.VecPtr^[loop]-
         alpha*(yPrevVec.VecPtr^[loop+1]+yPrevVec.VecPtr^[loop-1]-2*yPrevVec.VecPtr^[loop])+
         2.0*IndexOGVec.vecPtr^[loop]*yPrevVec.VecPtr^[loop]*dt;
   if RightB=Matched Then For loop:=numPts-hiddenPts-1 to numPts do
       Begin
       yPrevVec.VecPtr^[loop]:= yPrevVec.VecPtr^[loop]*(1+gainOGVec.VecPtr^[loop]);
       yVec.VecPtr^[loop]:= yVec.VecPtr^[loop]*(1+gainOGVec.VecPtr^[loop]);
       End;
    if LeftB=Matched Then For loop:=1 to hiddenPts do
       Begin
       yPrevVec.VecPtr^[loop]:= yPrevVec.VecPtr^[loop]*(1+gainOGVec.VecPtr^[loop]);
       yVec.VecPtr^[loop]:= yVec.VecPtr^[loop]*(1+gainOGVec.VecPtr^[loop]);
       End;
   AdjustForBoundry;
   if i=n Then {check for overfow on the last pass and calculate the Display vector.}
     For loop:=1 to numPts Do
       Begin
       DisplayVec.VecPtr^[loop]:=
           Sqrt(Sqr(yVec.vecptr^[loop])+Sqr(yPrevVec.vecPtr^[loop]));
       if DisplayVec.VecPtr^[loop]>100 then
         Begin
         DisplayVec.VecPtr^[loop]:=100;
         running:=FALSE;
         overFlow:=True;
         End;
       end;
   time := time + dt;
  END; { of NumSteps}
  if overflow Then
  Begin
  Announce('Caution:  Re(Psi)>100 or Im(Psi) > 100.  ');
  ClearMKBuffers;
  End;
AdjustForBoundry;
END; {of STEP}

end.
