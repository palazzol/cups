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

Unit Chn_Anly;
{$R-}
Interface
Uses Chn_Base,Chn_Plot,Chn_Gbl, Chn_Graf,Chn_Lst,
     Cups,CupsGui,CUPSmupp,CupsGrph,CupsFunc,CupsProc,Crt,Graph;
Const Big=1000.00;
      Decimal=4;     {Number of Decimals on the Contour Legend}
Type

AnalysisType = ( None, Dynamic, ThreeD, FastFT, VersusTime);
FFTBasisType=(sinOnly,sinCos);

MediumDvectorArray = Array[1..5] of Dvector;

DvectorArray = Array[1..10] of Dvector;

RotationAngles = Array[1..3] of Real;

AnlyPlotPtr=^AnlyPlot;
AnlyPlot=Object(Plot)
          EuAngles:                     RotationAngles;
          PalNum:                       Word;
          OnlyOnce,
          ShowMaxHist,
          Freeze,
          LegBool,
          BoxBool:                      Boolean;
          AnalysisPlot:                 PlotType;
          FFTBasis:                     FFTBasisType;
          AnlyType:                     AnalysisType;
          AnalyzedAtoms:                AnalysisArray;
          KEvec,PEvec,
          Evec,Tvec,
          AmpVec:                       Dvector;
          PowerArray:                   MediumDvectorArray;
          AnlArray:                     DvectorArray;
          OldMatrix,
          TheMatrix:                    DMatrix;
          MaxHistVec:                   WVector;  {Holds the maximum histogram values.}
          FFTVec:                       DVector;  {Holds the packed yvec values for inplace fft}
          FFTx:                         DVector;  {holds the x coords for the hist.}
          FFTSize:                      Integer;  {Number of points in fft.  Must be power of 2}
          MatSize,
          xCompress,
          TimeCompress,
          DvecCompress,
          DvecCompressCounter,
          AnalysisCounter,    {AnalysisCounter: number of data points that are up to date.}
          OldCount,
          SkipCounter:                  Integer; {Counter that is decremented to determine if data will be recorded}
          InitialTime:                  Real;
            Constructor Initialize( ISclNum: Byte; x, y, w, h: Real);
            Destructor Done;                       Virtual;
            Procedure AtomAnalysisSelection( TheChain: PlotPtr);
            Procedure RefreshAnalysisPlot(Clear: Boolean; Ix, Iy, Fx, Fy: Real);
            Procedure RegisterDataPoint;
            Procedure UpdateAnalysisPlots;
            Procedure RePlot;                      Virtual; {draws inside a plot}
            Procedure ChangeScale;                 Virtual;
            Procedure DMatTimeCompress( mat: Dmatrix );
            Procedure DMatPositionCompress( size: Integer );
            Procedure CompactDVec( vec: Dvector );
            Procedure MaxMinAnalysis( Var MinX, MaxX, MinY, MaxY: Real );
            Procedure ReFFT;
            Procedure SinFFT;
            Procedure Four1(nn, isign1D : integer);
            Procedure FourierAnalysis( ScaleIt,ResetIt: Boolean );
            Procedure HistPlot(data:Dvector; n: Integer);
            Procedure HistScale(Data:Dvector; n:Integer);
            Procedure ResetMaxHistScale(n: Integer);
            Procedure SetAnalysisType;
            Procedure ChangeAttributes;         Virtual;
            Procedure ClearAnalysis;
            Procedure CompressAnalysis;
            Function SetUpAnalysis( MatrixSize: Integer ):Boolean;
            Procedure Display;      Virtual;
            Procedure ClearScreen;
            Procedure Archive(Var filehandle: text);
            Procedure UnArchive(Var filehandle: text);
            Procedure resetAndInit;
            Procedure PlotAnlyVectors(HorzVec, VertVec:Dvector;start, finish:Integer);
end;

Procedure CheckIt( num: Integer; switch: Boolean; plot: AnlyPlotPtr );

Var
   Anly1,
   Anly2:               AnlyPlot;

Implementation

Const
  eps=1.0e-3;


Var
    InDat,OutDat,x:            Dvector;
    max:                       Real;
    loop:                      Integer;

Procedure CheckIt( num: Integer; switch: Boolean; plot: AnlyPlotPtr );

Begin;
      With MainMenu do
      Case plot^.AnalysisPlot of
           Disabled:    RowChecked(num,1,switch);
           YvsT:        RowChecked(num,2,switch);
           VvsT:        RowChecked(num,3,switch);
           SvsT:        RowChecked(num,4,switch);
           EvsT:        RowChecked(num,5,switch);
           VvsY:        RowChecked(num,6,switch);
           YvsY:        RowChecked(num,7,switch);
           YvsTDensity: RowChecked(num,8,switch);
           YvsT3D:      RowChecked(num,9,switch);
           FFT_X,FFT_V: RowChecked(num,10,switch);
           FFT_T:       RowChecked(num,11,switch);
           End; {of Case}
End;
{---------------------------------------------------------------------}
Procedure limitedMinMax(Var min,max:Real; vec:DVector;minInteger, maxIndex:Integer);
VAR
   i: INTEGER;
   tval: REAL;
BEGIN
   if vec.length <1 then exit;
   if maxIndex>vec.length then Begin Announce('Error:Index in limitedMinMax procedure >size of vector.'); exit; end;
   Min:=vec.VecPtr^[minInteger]; Max:=vec.VecPtr^[minInteger];
   FOR  i:=minInteger+1 TO maxIndex DO
      BEGIN
         tval:=vec.VecPtr^[i];
         IF tval<Min THEN Min:=tval;
         IF tval>Max THEN Max:=tval;
      END
END;
{--------------------------------------------------------------------------------------}
Procedure AnlyPlot.PlotAnlyVectors(HorzVec, VertVec:Dvector;start, finish:Integer);
Var
 xInc,yInc:                  Real;
 xStartPix,yStartPix,
 xEndPix,yEndPix:            Word;
 x1,y1,x2,y2,xOrigin:                LongInt;
 i:                    Word;
Begin
if HorzVec.Length <> vertVec.length Then EXIT;
if start<1 Then start:=1;
if finish <=start Then finish:=start+1;
if finish > vertVec.length Then Finish:=vertVec.Length;
if finish-start<1 Then exit;
With Scale Do
  Begin
  xInc:=(MapX(xMax)-MapX(xMin))/(xMax-xMin);
  yInc:=(MapY(yMax)-MapY(yMin))/(yMax-yMin);
  xStartPix:=MapX(xMin);
  xEndPix:=MapX(xMax);
  yStartPix:=MapY(yMin);
  yEndPix:=MapY(yMax);
  i:=start;
  If AnlyType=Dynamic
    Then xOrigin:=Round(xStartPix+xinc*(0-xMin))
    Else xOrigin:=Round(xStartPix+xinc*(HorzVec.Vecptr^[1]-xMin));
  x1:=Round(xStartPix+xinc*(HorzVec.Vecptr^[i]-xMin));
  y1:=Round(yStartPix+yinc*(VertVec.Vecptr^[i]-yMin));
  MoveTo(x1,y1);
  While (i<finish)Do
      begin
      i:=i+1;
      x1:=Round(xStartPix+xinc*(HorzVec.Vecptr^[i]-xMin));
      y1:=Round(yStartPix+yinc*(VertVec.Vecptr^[i]-yMin));
      If (y1<=yStartPix)And (y1>=yEndPix) AND ((x1<>xOrigin) Or Running)
        Then LineTo(x1,y1) Else MoveTo(x1,y1);
      End;
  If running and (x1<>xOrigin) Then PutPixel(x1,y1,white);
  End; {of With Scale}
End;
{------------------------------------------------------------------------------}
{Fourier Analysis Procedures---------------------------------------------------}
{------------------------------------------------------------------------------}
Procedure AnlyPlot.Four1(nn, isign1D : integer);
{PROCEDURE four1(VAR Data1D: array[1..MaxSubX2] of real;
		nn,isign:integer);  }
  {Replaces data in Data1D by its discrete/inveres discrete Fourier Transform
		if isign is	1/  -1.  nn must be power of 2}
VAR
   ii,jj,n,mmax,m,j,i,istep             :Integer;
   wtemp,wr,wi,wpr,wpi, theta           :Double;
   tempr,tempi, wrs,wis                 :Double;
   Data1DPtr:    dBasePtr;
   Dummy:                            Double;
BEGIN
    data1DPtr:=FFTVec.VecPtr;  {use the dVector's data}
    For i:=1 to 2*nn Do
      Dummy:=data1dPtr^[i];   {look at what we have}
   n:=2*nn;
   j:=1;
   {Bit Reversal}
   FOR ii:= 1 TO nn DO BEGIN
      i:=2*ii - 1;
      IF j > i THEN BEGIN
            {Exchange the two complex numbers}
	    tempr:=data1dPtr^[j];	   tempi:=data1dPtr^[j+1];
	    data1dPtr^[j]:=data1dPtr^[i];  data1dPtr^[j+1]:=data1dPtr^[i+1];
	    data1dPtr^[i]:=tempr;	   data1dPtr^[i+1]:=tempi;
         END;
	 m:=n DIV 2;
	 WHILE (m>=2) AND (j>m) DO BEGIN
            j:=j-m;
            m:=m DIV 2;
	 END;
	 j:=j+m;
      END;
      mmax:=2;
      WHILE n>mmax DO BEGIN       {log nn (base 2) times}
         istep:= 2*mmax;
	 {Initialise for trig recurrence}
	 theta:=6.28318530717959/(isign1D * mmax);
	 wpr:=-2.0 * sqr(sin(0.5*theta)); 	wpi:=sin(theta);
	 wr:=1.0;	wi:=0.0;
	 FOR ii:=1 TO mmax DIV 2 DO BEGIN
	 {nested inner loops}
         m:=2 * ii - 1;
	 wrs:=wr; wis:=wi;
	 FOR jj:=0 TO (n-m) DIV istep DO BEGIN
	    {Danielson-Lanczos formula}
	    i:=m + jj*istep;
	    j:=i +mmax;
	    tempr:=wrs * data1dPtr^[j] - wis * data1dPtr^[j+1];
	    tempi:=wrs * data1dPtr^[j+1] + wis * data1dPtr^[j];
	    data1dPtr^[j]:=data1dPtr^[i] - tempr;
	    data1dPtr^[j+1]:=data1dPtr^[i+1] - tempi;
	    data1dPtr^[i] := data1dPtr^[i]  +  tempr;
	    data1dPtr^[i+1]:= data1dPtr^[i+1]+ tempi;
	 END;
	 wtemp:=wr;	{Trig recurrence}
	 wr:=wr*wpr - wi*wpi   + wr;
	 wi:= wi*wpr + wtemp*wpi +wi;
      END;
      mmax:= istep;
   END;
END;{end of 1D FFT}


Procedure AnlyPlot.ReFFT;
(* Programs using routine REALFT must define the type
TYPE
    gldarray = ARRAY [1..2*n] OF real;
where 2*n is the dimension of the input data array.  When
routine FOUR1 is used with REALFT, its data type 'gldarray'
should be set as in this program.  *)

VAR
    wr,wi,wpr,wpi,wtemp,theta:  double; i,i1,i2,i3,i4:  integer;
    c1,c2,hir,h1i,h2r,h2i,wrs,wis:  real;
    n:Integer;  {make this a local variable since the length of DVector is known}
    iSign:      Integer;{Flag for forward or inverse transform}
    DataPtr:    dBasePtr;
    two:        Integer;  {can't use constants with: dvector.VecPtr^[2]}
    dummy:real;
BEGIN
    n:=fftsize DIV 2;
    iSign:=1;  {always do forward transform}
    two:=2;
    dataPtr:=FFTVec.VecPtr;  {use the dVector's data}
    For i:=1 to fftSize Do
      Dummy:=DataPtr^[i];
    theta := 6.28318530717959/(2.0*n); c1 := 0.5;
    IF (isign = 1) THEN BEGIN c2 := -0.5; four1(n,1); END
    ELSE BEGIN c2 := 0.5; theta := -theta; END;
    wpr := -2.0*sqr(sin(0.5*theta)); wpi := sin(theta);
    wr := 1.0+wpr; wi := wpi;
    FOR i := 2 TO (n DIV 2)+1 DO BEGIN
        i1 := i+i-1; i2 :=i1+1; i3:= n+n+3-i2; i4:=i3+1;
        wrs := wr; wis :=wi; hir:=c1*(dataPtr^[i1]+dataPtr^[i3]);
        h1i :=c1*(dataPtr^[i2]-dataPtr^[i4]); h2r :=-c2*(dataPtr^[i2]+dataPtr^[i4]);
        h2i :=c2*(dataPtr^[i1]-dataPtr^[i3]); dataPtr^[i1] :=hir+wrs*h2r-wis*h2i;
        dataPtr^[i2] := h1i+wrs*h2i+wis*h2r; dataPtr^[i3] :=hir-wrs*h2r+wis*h2i;
        dataPtr^[i4] := -h1i+wrs*h2i+wis*h2r; wtemp := wr;
        wr := wr*wpr-wi*wpi+wr; wi := wi*wpr+wtemp*wpi+wi END;
    IF (isign = 1) THEN BEGIN
       hir := dataPtr^[1]; dataPtr^[1] := hir+dataPtr^[two]; dataPtr^[two] :=hir-dataPtr^[two] END
    ELSE BEGIN
        hir := dataPtr^[1]; dataPtr^[1] := c1*(hir+dataPtr^[two]);
        dataPtr^[two] := c1*(hir-dataPtr^[two]); four1(n,-1) END;
    For i:=1 To FFTSize Do Begin
      Dummy:=DataPtr^[i]; if Dummy>big then DataPtr^[i]:=big end;
END;
PROCEDURE AnlyPlot.SinFFT;
(*Programs using routine SINFT must define the type
TYPE
    glyarray = ARRAY [1..n] OF real;
where n is the dimension of the input data. *)
VAR
    DataPtr:    dBasePtr;
    n,two:          Integer;
    jj,j,m,n2: integer;
    sum,y1,y2: real;
    theta,wi,wr,wpi,wpr,wtemp: double;
BEGIN
    n:=fftSize;
    two:=2;
    dataPtr:=FFTVec.VecPtr;  {use the dVector's data}
    theta := Pi/n; wr := 1.0; wi := 0.0;
    wpr:= -2.0*sqr(sin(0.5*theta)); wpi := sin(theta); dataPtr^[1] := 0.0;
    m := n DIV 2;
    n2 := n+2;
    FOR j := 2 TO (m+1) DO BEGIN
        wtemp := wr; wr := wr*wpr-wi*wpi+wr; wi := wi*wpr+wtemp*wpi+wi;
        y1 :=(wi)*(dataPtr^[j]+dataPtr^[n2-j]); y2:= 0.5*(dataPtr^[j]-dataPtr^[n2-j]);
        dataPtr^[j] := y1+y2; dataPtr^[n2-j] := y1-y2 END;
    reFFT; sum := 0.0; dataPtr^[1] := 0.5*dataPtr^[1]; dataPtr^[two] := 0.0;
    FOR jj := 0 TO (m-1) DO BEGIN
        j := 2*jj+1; sum := sum+dataPtr^[j]; dataPtr^[j] := dataPtr^[j+1]; dataPtr^[j+1] := sum END
END;

Procedure AnlyPlot.HistScale(Data:Dvector; n:Integer);

Var
   i:    Integer;
   max:  Real;

Begin
  LockFocus;
  max:=Data.Value(1);
  For i:=2 to n Do If max<Data.Value(i) THEN max:=Data.Value(i);
  if Max>5 Then max:=Round(max);
  Plot.remove;
  If (2*Max<=0.01) then Max:=0.5;
  SetScale( 0, -0.01, n+1, 2*Max );
  Plot.Display;
End;

Procedure AnlyPlot.ResetMaxHistScale(n: Integer);

Var
   i,yPix:    Integer;

Begin
      yPix:=Mapy(0);
      For i:=1 To n Do maxHistVec.WVecPtr^[i]:=yPix;
      For i:=1 To n Do FFTx.VecPtr^[i]:=i;
End;


Procedure Anlyplot.HistPlot(data:Dvector; n: Integer);

Var Rect:  Array[1..4] of PointType;
    OldColour,
    BarWidth:   Word;
    i:          Integer;
    two:        Integer;
    FillInfo:   FillSettingsType;

Begin
    if NOT Visible Then Exit;
     GetFillSettings(FillInfo);
     OldColour:=GetColor;
     LockFocus;
     HideCursor;

     Barwidth:=Round((MapX(FFTx.Value(2))-MapX(FFTx.Value(1)))*0.45);
      For i:=1 To n Do
        Begin;
        rect[1].x:=Mapx(FFTx.Value(i));  rect[1].y:=MapY(0);
        rect[2].x:=rect[1].x; rect[2].y:=MapY(data.Value(i));
        If rect[2].y<=0 Then rect[2].y:=1;
        rect[3].x:=rect[1].x+BarWidth; rect[3].y:=rect[2].y;
        rect[4].x:=rect[3].x; rect[4].y:=rect[1].y;
        SetColor(red); SetFillStyle(solidFill,red);
        FillPoly(4,Rect);

        rect[1].y:=rect[2].y; rect[4].y:=rect[3].y;
        If maxHistVec.WVecPtr^[i]>rect[2].y then maxHistVec.WVecPtr^[i]:=rect[2].y;
        {Pixel number increases from TOP to BOTTOM of screen.}
        If ShowMaxHist then
           begin
                SetColor(Blue);
                SetFillStyle(solidFill,Blue);
           end
        else
           begin
                SetColor(GraphBackColor);
                SetFillStyle(solidFill,GraphBackColor);
           end;
           rect[2].y:=maxHistVec.WVecPtr^[i]; rect[3].y:=rect[2].y;
           FillPoly(4,Rect);
           If ShowMaxHist then SetColor(White) else SetColor(GraphBackColor);
           Line(rect[2].x,rect[2].y,rect[3].x,rect[3].y);
      End; {For loop}
      With FillInfo Do SetFillStyle(Pattern,Color);
      SetColor(OldColour);
      ShowCursor;
END;


Procedure AnlyPlot.FourierAnalysis( ScaleIt,ResetIt: Boolean );

Var
   i,two:   Integer;
   Temp:    Real;

Begin;
      two:=2;
      If not(Visible)then exit;
      If (Yvec.Length-2)<4 then
         begin;
               Announce('Fast Fourier Analysis must have at least 4 atoms.');
               Plottitle.SetStr('Disabled');
               AnalysisPlot:=Disabled;
               Exit;
         end;
      If not(Freeze)
      then
         begin
           For i:= 1 to FFTSize Do     {pack data into FFTVec}
              Case AnalysisPlot of
                   FFT_x: FFTVec.VecPtr^[i]:=yVec.VecPtr^[i+1];
                   FFT_V: FFTVec.VecPtr^[i]:=vVec.VecPtr^[i+1];
                   FFT_T: FFTVec.VecPtr^[i]:=AmpVec.VecPtr^[i];
              End; {of Case}
           if FFTBasis=SinOnly
              Then
                Begin
                SinFFT;
                For i:=1 to FFTSize-1 DO
                FFTVec.VecPtr^[i]:=2*Abs(FFTVec.VecPtr^[i+1])/FFTSize;
                fftvec.VecPtr^[fftsize]:=0;
                If ScaleIt then HistScale(FFTVec, FFTSize);
                If ResetIt then ResetMaxHistScale(FFTSize);
                End
              Else
                Begin
                ReFFT;
                Temp:=Abs(FFTVec.VecPtr^[two])/FFTsize;
                For i:=2 to FFTSize DIV 2 DO
                FFTVec.VecPtr^[i-1]:=Sqrt(Sqr(FFTVec.VecPtr^[2*i-1]) +
                                 Sqr(FFTVec.VecPtr^[2*i]))*2/FFTsize;
                FFTvec.VecPtr^[FFTsize div 2]:=Temp;
                If ScaleIt then HistScale(FFTVec, FFTSize Div 2);
                If ResetIt then ResetMaxHistScale(FFTSize Div 2);
                End;
         end
         else ResetMaxHistScale(FFTSize);
         if FFTBasis=SinOnly
           Then  HistPlot(FFTVec,FFTSize)
           Else  HistPlot(FFTVec,FFTSize Div 2);
End;
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
Constructor AnlyPlot.Initialize( ISclNum: Byte; x, y, w, h: Real );
Var i:Integer;
Begin
     Plot.Initialize( ISclNum, x, y, w, h);
     AnalysisPlot:=Disabled;
     AnlyType:=None;
     For i:=1 to 5 do AnalyzedAtoms[i]:=Nil;
     For i:=1 to 10 do AnlArray[i].Init(0);
     Tvec.Init(0);
     OldMatrix.Init(0,0);
     TheMatrix.Init(0,0);
     AmpVec.Init(0);
     maxHistVec.Init(0);
     fftVec.Init(0);
     fftx.Init(0);
     Evec.Init(0);
     KEvec.Init(0);
     PEvec.Init(0);
     For i:=1 to 5 do PowerArray[i].Init(0);
     PlotSclBut.Initialize(@Self);
     ExpandBut.Initialize(@Self);
     AttrBut.Initialize(@Self);
     SetScale( 0, -1, 10, 1 );
     PlotAxes.Visible:=False;

     PlotAxes.ShowCell:=False;
     HasAtoms:=FALSE;
     AnalysisCounter:=1;
     TimeCompress:= 1;
     SkipCounter:=1;
     OldCount:=1;
     EuAngles[1]:=30;
     EuAngles[2]:=20;
     EuAngles[3]:=0;
     PalNum:=1;
     LegBool:=False;
     BoxBool:=True;
     Freeze:=False;
     ShowMaxHist:=True;
     DvecCompress:=1;
     DvecCompressCounter:=1;
End;
Destructor AnlyPlot.Done;
Begin;
      ClearAnalysis;
      Plot.done;
End;
Procedure  AnlyPlot.Archive(Var filehandle: text);
Var  i:    Integer;
Begin
  If Freeze
    Then WriteLn(filehandle, Ord(Disabled))
    Else WriteLn(filehandle, Ord(AnalysisPlot));
  For i:=1 to 5 do
    If (AnalyzedAtoms[i]<> Nil)
    Then Write(filehandle,AnalyzedAtoms[i]^.ParAtm.Num, '  ')
    Else Write(filehandle,0, '  ');
  Writeln(fileHandle,' ');
End;

Procedure  AnlyPlot.UnArchive(Var filehandle: text);
{These types are defined in Global-
 (Disabled, YvsT, VvsT, VvsY, EvsT, YvsTDensity, YvsT3D, YvsY, SvsT,FFT);}
Var  aType,i,num:     Integer;
Begin;

  ReadLn(filehandle, aType);
  Case aType of
    0:AnalysisPlot:=Disabled;
    1:AnalysisPlot:=YvsT;
    2:AnalysisPlot:=VvsT;
    3:AnalysisPlot:=VvsY;
    4:AnalysisPlot:=EvsT;
    5:AnalysisPlot:=YvsTDensity;
    6:AnalysisPlot:=YvsT3D;
    7:AnalysisPlot:=YvsY;
    8:AnalysisPlot:=SvsT;
    9:AnalysisPlot:=FFT_X;
    10:AnalysisPlot:=FFT_V;
    11:AnalysisPlot:=FFT_T;
  end;
  For i:=1 to 5 do
    Begin
    Read(fileHandle,num);
    If num = 0
      Then AnalyzedAtoms[i]:=Nil
      Else AnalyzedAtoms[i]:=AtomList.GetItemPtr(num);
    End;
    Readln(filehandle);
End;
Procedure AnlyPlot.ResetAndInit;
Var numOfSteps,i:         Integer;
Begin
 if animationSpeed<=0 Then numOfSteps:=1-AnimationSpeed Else numOfSteps:=1;
 PlotAxes.Visible:=TRUE; {This is the default.  Set to false if you do not want axes}
 Case AnalysisPlot of
     Disabled:
       Begin
       Plottitle.SetStr('Disabled');
       If PlotAxes.Visible then
       begin
         PlotAxes.Remove;
         PlotAxes.Visible:=False;
         end;
       ClearScreen;
       AtomAnalysisSelection(@TheChain);
       For i:=1 to 5 do AnalyzedAtoms[i]:=nil;
       End;
     YvsT:
       Begin
       Plottitle.SetStr('Displacement: Y(t)');
       AtomAnalysisSelection(@TheChain);
       RefreshAnalysisPlot(True,time,-1,time+20*dt*numOfSteps,1);
       End;
     VvsT:
       Begin
       Plottitle.SetStr('Velocity: V(t)');
       AtomAnalysisSelection(@TheChain);
       RefreshAnalysisPlot(True,time,-1,time+20*dt*numOfSteps,1);
       End;
     VvsY:
       Begin
       Plottitle.SetStr('Phase Space: V(Y)');
       AtomAnalysisSelection(@TheChain);
       RefreshAnalysisPlot(True,-2,-2,2,2);
       End;
     EvsT:
       Begin
       Plottitle.SetStr('Energy: E(t)');
       RefreshAnalysisPlot(True,time,0,time+20*dt*numOfSteps,0.5);
       AtomAnalysisSelection(@TheChain);
       End;
     YvsTDensity:
       Begin
       Plottitle.SetStr('Space Time: Contour');
       GraphBackColor:=Black;
       RefreshAnalysisPlot(True,0,-1,10,1);
       GraphBackColor:=DarkGray;
       AtomAnalysisSelection(@TheChain);
       End;
     YvsT3D:
       Begin
       If PlotAxes.Visible then
         begin
         PlotAxes.Remove;
         PlotAxes.Visible:=False;
         end;
       Plottitle.SetStr('Space Time: 3D');
       GraphBackColor:=Black;
       ClearScreen;
       GraphBackColor:=DarkGray;
       AtomAnalysisSelection(@TheChain);
       End;
     FFT_x:
       Begin
       If leftB=Continuous
         Then FFTBasis:=SinCos
         Else FFTBasis:=SinOnly;
       Plottitle.SetStr('FFT: Y(x)');
       RefreshAnalysisPlot(True,0,-1,10,1);
       AtomAnalysisSelection(@TheChain);
       End;
     FFT_V:
       Begin
       If leftB=Continuous
         Then FFTBasis:=SinCos
         Else FFTBasis:=SinOnly;
       Plottitle.SetStr('FFT: Y(x)');
       RefreshAnalysisPlot(True,0,-1,10,1);
       AtomAnalysisSelection(@TheChain);
       End;
     FFT_T:
       Begin
       FFTBasis:=SinCos;
       Plottitle.SetStr('FFT: Y(t)');
       RefreshAnalysisPlot(True,0,0,129,1);
       AtomAnalysisSelection(@TheChain);
       if (AnalysisPlot<>Disabled) then FourierAnalysis(True,True);
       End;
     YvsY:
       Begin
       Plottitle.SetStr('Y1(t) vs Y2(t)');
       RefreshAnalysisPlot(True,-2,-2,2,2);
       AtomAnalysisSelection(@TheChain);
       End;
     SvsT:
       Begin
       Plottitle.SetStr('Source Power: P(t)');
       AtomAnalysisSelection(@TheChain);
       RefreshAnalysisPlot(True,time,-1,time+20*dt*numOfSteps,1);
       End;
   End; {of Case}
   AnalysisCounter:=1;
   DvecCompress:=1;
   DvecCompressCounter:=1;
   skipcounter:=1;
   timeCompress:=1;
   if @self=@Anly1 Then CheckIt(4,True,@Anly1);
   if @self=@Anly2 Then CheckIt(5,True,@Anly2);
End;


Procedure AnlyPlot.MaxMinAnalysis( Var MinX, MaxX, MinY, MaxY: Real );

Var
   i,p:           Integer;
   New_Min_X,
   New_Max_X,
   New_Min_Y,
   New_Max_Y:     Real;

begin;
      Case AnalysisPlot of
           YvsTDensity,
           YvsT3D,FFT_x,FFT_V,FFT_T,
           Disabled:    ;
           YvsT:
                begin;
                      AnlArray[1].MinMax(p,MinY,p,MaxY);
                      For i:=2 to 5 do
                          If not(AnalyzedAtoms[i]=Nil) then
                             begin;
                                   AnlArray[i].MinMax(p,New_Min_Y,p,New_Max_Y);
                                   If (New_Min_Y < MinY) then MinY:=New_Min_Y;
                                   If (New_Max_Y > MaxY) then MaxY:=New_Max_Y;
                             end;
                      LimitedMinMax(MinX,MaxX,TVec,1,AnalysisCounter);
                end;
           VvsT:
                begin;
                      AnlArray[6].MinMax(p,MinY,p,MaxY);
                      For i:=2 to 5 do
                          If not(AnalyzedAtoms[i]=Nil) then
                             begin;
                                   AnlArray[i+5].MinMax(p,New_Min_Y,p,New_Max_Y);
                                   If (New_Min_Y < MinY) then MinY:=New_Min_Y;
                                   If (New_Max_Y > MaxY) then MaxY:=New_Max_Y;
                             end;
                      LimitedMinMax(MinX,MaxX,TVec,1,AnalysisCounter);
                end;
           VvsY:
                begin;
                      AnlArray[1].MinMax(p,MinX,p,MaxX);
                      AnlArray[6].MinMax(p,MinY,p,MaxY);
                      For i:=2 to 5 do
                          If not(AnalyzedAtoms[i]=Nil) then
                             begin;
                                   AnlArray[i].MinMax(p,New_Min_X,p,New_Max_X);
                                   If (New_Min_X < MinX) then MinX:=New_Min_X;
                                   If (New_Max_X > MaxX) then MaxX:=New_Max_X;

                                   AnlArray[i+5].MinMax(p,New_Min_Y,p,New_Max_Y);
                                   If (New_Min_Y < MinY) then MinY:=New_Min_Y;
                                   If (New_Max_Y > MaxY) then MaxY:=New_Max_Y;
                             end;
                end;
           EvsT:
                begin;
                      KEVec.MinMax(p,MinY,p,MaxY);
                      PEVec.MinMax(p,New_Min_Y,p,New_Max_Y);
                      If (New_Min_Y < MinY) then MinY:=New_Min_Y;
                      If (New_Max_Y > MaxY) then MaxY:=New_Max_Y;
                      Evec.MinMax(p,New_Min_Y,p,New_Max_Y);
                      If (New_Min_Y < MinY) then MinY:=New_Min_Y;
                      If (New_Max_Y > MaxY) then MaxY:=New_Max_Y;
                      LimitedMinMax(MinX,MaxX,TVec,1,AnalysisCounter);
                end;
           SvsT:
                begin;
                      PowerArray[1].MinMax(p,MinY,p,MaxY);
                      For i:=2 to 5 do
                          If not(AnalyzedAtoms[i]=Nil) then
                             begin;
                                   PowerArray[i].MinMax(p,New_Min_Y,p,New_Max_Y);
                                   If (New_Min_Y < MinY) then MinY:=New_Min_Y;
                                   If (New_Max_Y > MaxY) then MaxY:=New_Max_Y;
                             end;
                      LimitedMinMax(MinX,MaxX,TVec,1,AnalysisCounter);
                end;
           YvsY:
                begin;
                      AnlArray[2].MinMax(p,MinY,p,MaxY);
                      AnlArray[1].MinMax(p,MinX,p,MaxX);
                end;
      End;
End;

Procedure AnlyPlot.CompactDVec( vec: Dvector );

Var
   i:           Integer;

Begin;
      For i:=1 to (Alength Div 2) do vec.Put(i,vec.Value(2*i-1));
End;

Procedure AnlyPlot.DMatTimeCompress( mat: Dmatrix );
Var
   i,j:         Integer;

Begin;
      For i:=0 to (matsize Div 2 - 1) do
          for j:=1 to (matsize) do
              mat.Put(MatSize - i , j ,mat.Value(Matsize-(2*i),j) + mat.Value(MatSize-(2*i+1),j));

      For i:=1 to (matsize Div 2) do
          for j:=1 to (matsize) do
              mat.Put(i , j ,0);
      mat.MultScalar(TimeCompress);

      AnalysisCounter:=MatSize Div 2 + 1;
      TimeCompress:=TimeCompress*2;

      SkipCounter:=TimeCompress;
End;

Procedure AnlyPlot.DMatPositionCompress( Size: Integer );

Var
   xCmprsCtr,i:         Integer;

Begin;
      xCompress:=1;
      xCmprsCtr:=0;

      While (Size>70) do
            begin;
                  Inc(xCmprsCtr);
                  Size:=Size Div 2;
            end;

      MatSize:=Size; {Sets Matrix Size such that the Matrix will be square.}

      While not(xCmprsCtr=0) do
            begin;
                  Dec(xCmprsCtr);
                  xCompress:=xCompress*2;
            end;
End;

Procedure AnlyPlot.ChangeScale;

Var                             
   InptSclScrn:                 TInputScreen;
   Ok, Cancel, AsXY,
   AsX,AsY,Top:                 Boolean;
   Temp,Old:                    ScaleRect;
   i,ypix:                      Integer;
Begin;
      If (AnalysisPlot=Disabled) then
         begin
              Announce('The Scale has been disabled.');
              Exit;
         end;
      If (AnalysisPlot=YvsTDensity) or (AnalysisPlot=YvsT3D) then
         begin;
               Announce('You cannot rescale this type of analysis plot.');
               Exit;
         end;

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
                 LoadLine(' #T Display Scale On/Off?' );
                 LoadLine('  Minimum       Maximum    ');
                 Loadline('                           ');
                 LoadLine(' X={     }     X={     }   ');
                 LoadLine(' Y={     }     Y={     }   ');
                 Loadline('                           ');
                 Loadline('     Full Screen    #T     ');
                 Loadline('                           ');
                 Loadline('     AutoMatic Scaling     ');
                 Loadline(' [   X  ] [  XY  ] [  Y   ]');
                 Loadline('                           ');
                 LoadLine(' [  Ok  ] [Cancel] [ Help ]');
                 LoadLine('                           ');
                 With Temp do
                      begin;
                            SetBoolean(1, PlotAxes.Visible );
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
                 AsX:=GetBoolean(7);
                 AsXY:=GetBoolean(8);
                 AsY:=GetBoolean(9);
                 If not(Expanded=GetBoolean(6)) then Expand;
                 Expanded:=GetBoolean(6);
                 Ok:=GetBoolean(10);
                 Cancel:=GetBoolean(11);
                 If (Temp.xMin>=Temp.xMax) OR (Temp.yMin>=Temp.yMax) THEN
                      BEGIN OK:=FALSE; Cancel:=TRUE;
                      Announce('Minimum vales must be less than Maximum value.');
                      END;
                  Done;
           end;

           If Ok then
                          begin;
                                Plot.Remove;
                                PlotAxes.Remove;
                                Scale:=Temp;
                                With Temp do SetScale( xMin, yMin, xMax, yMax );
                                If AnalysisPlot in [FFT_x,FFT_v,FFT_T] then
                                   begin
                                   Plot.Display;
                                   FourierAnalysis(False,True);
                                   end
                                else
                                Display;
                          end;

           If ((AsXY) or (AsX) or (AsY)) and (AnalysisPlot in [FFT_x,FFT_V,FFT_t])  then
                          begin;
                                Plot.Remove;
                                PlotAxes.Remove;
                                Display;  {This resets both X and Y axes}
                                if AsX Then
                                  Begin
                                  Plot.Remove;
                                  PlotAxes.Remove;
                                  FourierAnalysis(True,True);
                                  PlotAxes.Remove;
                                  SetScale( scale.xMin, temp.yMin, scale.xMax, temp.yMax );
                                  Plot.Display;
                                  FourierAnalysis(False,True);
                                  PlotAxes.Display;
                                  End;
                                if AsY Then
                                  Begin
                                  Plot.Remove;
                                  PlotAxes.Remove;
                                  FourierAnalysis(True,True);
                                  PlotAxes.Remove;
                                  SetScale( temp.xMin, scale.yMin, temp.xMax, scale.yMax );
                                  Plot.Display;
                                  FourierAnalysis(False,True);
                                  PlotAxes.Display;
                                  End;
                                If AsXY Then
                                  Begin
                                  Plot.Remove;
                                  PlotAxes.Remove;
                                  FourierAnalysis(True,True);
                                  End;
                                ButtonList.Display;
                                PlotTitle.Display;
                                Exit;
                          end;

                       If AsY OR AsX OR AsXY then
                          begin;
                                PlotAxes.Remove;
                                Remove;
                                Old:=Temp;
                                With Temp do
                                     begin;
                                           MaxMinAnalysis( xMin, xMax, yMin, yMax );

                                           If (yMin=yMax) or (xMin=xMax) then
                                              begin;
                                                    Display;
                                                    PlotAxes.Display;
                                                    ButtonList.Display;
                                                    Announce('The Minimum and Maximum Values are the Same.');
                                                    Exit;
                                              end;
                                           If AsXY or AsX then
                                              begin
                                              xMin:=FancyTrunc(xMin);
                                              xMax:=xmax;
                                              end;
                                           If AsXY or AsY then
                                              begin
                                              yMin:=FancyTrunc(yMin-0.05*(yMax-yMin));
                                              yMax:=(yMax+0.05*(yMax-yMin));
                                              end;
                                           If AsXY then
                                              begin;
                                                    Scale:=Temp;
                                                    SetScale( xMin, yMin, xMax, yMax );
                                              end
                                           else
                                               begin;
                                                     If AsX then
                                                        begin;
                                                              yMin:=Old.yMin;
                                                              yMax:=Old.ymax;
                                                        end;
                                                     If AsY then
                                                        begin;
                                                              xMin:=Old.xMin;
                                                              xMax:=Old.xMax;
                                                        end;
                                                     Scale:=Temp;
                                                     SetScale( xMin, yMin, xMax, yMax );
                                               end;
                                     end;
                                Display;
                                PlotAxes.Display;
                                ButtonList.Display;
                                PlotTitle.Display;
                          end;
End;




Procedure AnlyPlot.RePlot;

Var
   OldColour,
   Trash:       Word;
   i:           Integer;

begin;
  if Not Visible Then EXIT;
  OldColour:=GetColor;
  If ((AnalysisCounter<=1) and not(AnalysisPlot in [FFT_x,FFT_V])) THEN EXIT;

  Case AnalysisPlot of
       Disabled:    ;
       YvsT:    For i:=1 to 5 do
                    If not(AnalyzedAtoms[i]=Nil) then
                       begin
                       LockFocus;
                       SetColor(AnalyzedAtoms[i]^.AtomColour);
                       PlotAnlyvectors(Tvec, AnlArray[i], 1, AnalysisCounter);
                       end;
       VvsT:    For i:=1 to 5 do
                    If not(AnalyzedAtoms[i]=Nil) then
                       begin;
                       LockFocus;
                       SetColor(AnalyzedAtoms[i]^.AtomColour);
                       PlotAnlyVectors(Tvec, AnlArray[i+5], 1, AnalysisCounter);
                       end;
       VvsY:    For i:=1 to 5 do
                    If not(AnalyzedAtoms[i]=Nil) then
                       begin;
                       LockFocus;
                       SetColor(AnalyzedAtoms[i]^.AtomColour);
                       PlotAnlyvectors(AnlArray[i], AnlArray[i+5], 1, AnalysisCounter);
                       end;
       EvsT:      begin;
                        SetColor(Green);
                        PlotAnlyvectors(Tvec, Evec, 1, AnalysisCounter);
                        SetColor(Blue);
                        PlotAnlyvectors(Tvec, KEvec, 1, AnalysisCounter);
                        SetColor(Red);
                        PlotAnlyvectors(Tvec, PEvec, 1, AnalysisCounter);
                  end;
       SvsT:      For i:=1 to 5 do
                        If not(AnalyzedAtoms[i]=Nil) then
                           begin;
                                 SetColor(AnalyzedAtoms[i]^.AtomColour);
                                 PlotAnlyVectors(Tvec, PowerArray[i], 1, AnalysisCounter);
                           end;
       YvsY:      begin;
                  LockFocus;
                  SetColor(Magenta);
                  PlotAnlyVectors(AnlArray[1],AnlArray[2],1,AnalysisCounter);
                  end;
       YvsTDensity:   begin;
                      HideCursor;
                      TheContour.Init;
                      if (TimeCompress>1)Then TheContour.SetPaletteNum(PalNum);
                      if (TimeCompress>1)Then TheContour.DrawMatrix(OldMatrix);
                      If LegBool then TheContour.PrintLegend(Decimal);
                      ButtonList.Display;
                      PlotAxes.Display;
                      ShowCursor;
                      end;
       YvsT3D:    begin;
                        Remove;
                        GraphBackColor:=Black;
                        Plot.Display;
                        GraphBackColor:=DarkGray;
                        HideCursor;
                        TheThreeD.Init;
                        TheThreeD.SetDistance(1.5,2.5);
                        TheThreeD.SetBoxRatios(1,4/5,2/3);
                        TheThreeD.SetViewAngles( EuAngles[1],EuAngles[2],EuAngles[3]);
                        If BoxBool then TheThreeD.DrawBox;
                        if (TimeCompress>1)Then TheThreeD.DrawSurface(OldMatrix);
                        ButtonList.Display;
                        ShowCursor;
                 end;
       FFT_x,FFT_V:
         FourierAnalysis(False,True);
       FFT_t:
           Begin
           ResetMaxHistScale(FFTSize);
           HistPlot(FFTVec,FFTSize);
           end;
  End;
      SetColor(OldColour);
End;

Procedure AnlyPlot.UpdateAnalysisPlots;

Var
   OldColour:           Word;

Procedure Collect_Plot_Dynamic_Analytes;

Var
   i:   Integer;

Begin;
      If (DvecCompressCounter>1) then Dec(DvecCompressCounter) else
         begin
              Inc(Analysiscounter);
              Tvec.Put(AnalysisCounter,Time);
              For i:=1 to 5 do
                  If not(AnalyzedAtoms[i]=Nil) then
                     begin;
                           AnlArray[i+5].Put(AnalysisCounter,AnalyzedAtoms[i]^.ParAtm.v);
                           AnlArray[i].Put(AnalysisCounter,AnalyzedAtoms[i]^.ParAtm.y);

                           If Visible then
                              begin;
                              LockFocus;
                              SetColor(AnalyzedAtoms[i]^.AtomColour);
                              Case AnalysisPlot of
                                         YvsT: PlotAnlyVectors(Tvec, AnlArray[i], AnalysisCounter-1, AnalysisCounter);
                                         VvsT: PlotAnlyVectors(Tvec, AnlArray[i+5], AnalysisCounter-1, AnalysisCounter);
                                         VvsY: PlotAnlyVectors(AnlArray[i], AnlArray[i+5], AnalysisCounter-1, AnalysisCounter);
                              End; {of Case}
                              end;
                     end;

              If AnalysisPlot=YvsY then
                 If Visible then
                    begin;
                    LockFocus;
                    SetColor(Magenta);
                    PlotAnlyVectors(AnlArray[1],AnlArray[2],analysiscounter-1,analysiscounter);
                    end;

              If (DvecCompress>1) then DvecCompressCounter:=DvecCompress;
         end;
End;

Procedure Collect_Plot_Compact_Matrix;

Var
   i,j,index:   Integer;
   Trash:       Word;
   Sum:         Real;

Begin;
      If (AnalysisCounter=2) And Visible And OnlyOnce then
         begin;
               Comment(Xo+Width/2-0.15,0.8,'Data being Accumulated.');
               OnlyOnce:=False;
         end;

      If (AnalysisCounter<=MatSize) then
         begin
              For i:=1 to Matsize do
                  begin
                       Sum:=0;
                       For j:=xCompress*(i-1) to xCompress*(i)-1 do
                           begin
                                If (j+2 > Yvec.Length-1) then index:=Yvec.length-1 else index:=j+2;
                                Sum:=Sum + Yvec.Value(index);
                           end;
                       TheMatrix.Put(MatSize - AnalysisCounter+1, i ,Sum/xCompress +
                                     TheMatrix.Value(MatSize - AnalysisCounter+1, i));
                  end;

              If (SkipCounter > 1)then Dec(SkipCounter) else
                begin
                     Inc(analysisCounter);
                     SkipCounter:=TimeCompress;  {reset the skipCounter}
                end;
         end;
      If (AnalysisCounter=(MatSize+1)) then
         begin;
               TheMatrix.MultScalar(1/TimeCompress);

               Case AnalysisPlot of
               YvsT3D:
                      begin;
                            If Visible then
                               begin;
                                     GraphBackColor:=Black;
                                     Plot.Display;
                                     GraphBackColor:=DarkGray;
                                     TheThreeD.Init;
                                     TheThreeD.SetDistance(1.5,2.5);
                                     TheThreeD.SetBoxRatios(1,4/5,2/3);
                                     HideCursor;
                                     TheThreeD.SetViewAngles(EuAngles[1],EuAngles[2],EuAngles[3]);
                                     If BoxBool then TheThreeD.DrawBox;
                                     TheThreeD.DrawSurface(TheMatrix);
                                     ButtonList.Display;
                                     Plottitle.SetStr('Space-Time: 3D');
                                     PlotTitle.Display;
                                     ShowCursor;
                               end;
                      end;
               YvsTDensity:
                           begin;
                                 If Visible then
                                    begin;
                                          PlotAxes.Remove;
                                          TheContour.Init;
                                          HideCursor;
                                          TheContour.SetPaletteNum(PalNum);
                                          TheContour.DrawMatrix(TheMatrix);
                                          If LegBool then TheContour.PrintLegend(Decimal);
                                          With Scale do
                                               begin;
                                                   xMin:=1;
                                                   xMax:=(Yvec.Length-2);
                                                   yMin:=InitialTime;
                                                   yMax:=Time;
                                                   SetScale(xMin,yMin,xMax,yMax)
                                              end;
                                          ButtonList.Display;
                                          PlotAxes.Display;
                                          Plottitle.SetStr('Space-Time: Contour');
                                          PlotTitle.Display;
                                          ShowCursor;
                                    end;
                           end;
               End;
               OldMatrix.Equate(TheMatrix);
               DMatTimeCompress(TheMatrix);
               {trunc(Exp(tCmprsCtr*Ln(2)));}
         end;
         {If not(tCmprsCtr=0) and (TimeCompress=0) then
         TimeCompress:=trunc(Exp(tCmprsCtr*Ln(2)));  }
End;

Procedure Collect_Plot_TimeDependent_Analytes;

Var
   i:   Integer;

Begin;
      If (DvecCompressCounter>1) then Dec(DvecCompressCounter) else
         begin
              Case AnalysisPlot of
                   EvsT:
                        begin;
                              Inc(Analysiscounter);
                              Tvec.Put(analysiscounter,Time);
                              Evec.Put(analysiscounter,Energy);
                              KEvec.Put(analysiscounter,KineticEnergy);
                              PEvec.Put(analysiscounter,PotentialEnergy);
                              If Visible then
                                 begin;
                                       SetColor(Green);
                                       PlotAnlyvectors(Tvec,Evec,analysiscounter-1,analysiscounter);
                                       SetColor(Blue);
                                       PlotAnlyVectors(Tvec,KEvec,analysiscounter-1,analysiscounter);
                                       SetColor(Red);
                                       PlotAnlyvectors(Tvec,PEvec,analysiscounter-1,analysiscounter);
                                 end;
                        end;
                   SvsT:
                        begin;
                              Inc(AnalysisCounter);
                              Tvec.Put(AnalysisCounter,Time);
                              For i:=1 to 5 do
                                  If not(AnalyzedAtoms[i]=Nil) then
                                     begin;
                                           PowerArray[i].Put(AnalysisCounter,AnalyzedAtoms[i]^.power);
                                           If Visible then
                                              begin;
                                                    SetColor(AnalyzedAtoms[i]^.AtomColour);
                                                    PlotAnlyVectors(Tvec, PowerArray[i], AnalysisCounter-1, AnalysisCounter);
                                              end;
                                     end;
                        end;
                   End;{ of Case }

                   If (DvecCompress>1) then DvecCompressCounter:=DvecCompress;
              end;
End;

Procedure Collect_Plot_FFT;

Begin
     If not(AnalysisPlot=FFT_t) then FourierAnalysis(False,False)
        else
            begin
                 If (AnalysisCounter=FFTSize+1) then exit;
                 If (AnalysisCounter=1) And Visible And OnlyOnce then
                    begin;
                          Comment(Xo+Width/2-0.15,0.8,'Data being Accumulated.');
                          OnlyOnce:=False;
                    end;

                 If (AnalysisCounter<=FFTsize) then
                    begin
                         Ampvec.Put(AnalysisCounter,AnalyzedAtoms[1]^.ParAtm.y);
                         Inc(AnalysisCounter);
                    end;
                 If (AnalysisCounter=FFTSize+1) then
                    begin
                         if Visible Then Plot.Display;
                         FourierAnalysis(False,True);
                         AnalysisCounter:=1;
                         DVecCompress:=2;
                         Ampvec.Put(AnalysisCounter,AnalyzedAtoms[1]^.ParAtm.y);
                         Inc(AnalysisCounter);
                    end;
            end;
End;


Begin;
      If ((AnalysisPlot=Disabled) or (Freeze)) then
         begin;
               If Visible AND Expanded then
                  begin;
                        OldColour:=GetColor;
                        LockFocus;
                        SetColor(White);
                        ShowEnergy;
                        SetColor(OldColour);
                  end;
               Exit;
         end;

      If (Analysisplot<>FFT_T) And (AnalysisCounter>=Alength) then CompressAnalysis;

      LockFocus;
      OldColour:=GetColor;

         Case AnlyType of
              None:         ;
              Dynamic:      Collect_Plot_Dynamic_Analytes;
              ThreeD:       Collect_Plot_Compact_Matrix;
              VersusTime:   Collect_Plot_TimeDependent_Analytes;
              FastFT:       Collect_Plot_FFT;
         End;

      If Visible AND Expanded then
         begin;
               SetColor(White);
               ShowEnergy;
         end;

      SetColor(OldColour);
End;

Procedure AnlyPlot.RegisterDataPoint;

Var
   i:   Integer;
   yPix: Word;
   dummy:real;

Begin;
OnlyOnce:=True;
initialTime:=Time;
Case AnalysisPlot of
     FFT_x,FFT_V,FFT_t,
     Disabled:   ;
     YvsY,
     YvsT:        begin;
                        Tvec.Put(AnalysisCounter,Time);
                        For i:=1 to 5 do
                            IF AnalyzedAtoms[i]<>nil THEN
                               AnlArray[i].Put(AnalysisCounter,AnalyzedAtoms[i]^.ParAtm.y);
                  end;
     VvsT:        begin;
                        Tvec.Put(1,Time);
                        For i:=1 to 5 do
                            IF AnalyzedAtoms[i]<>nil THEN
                               AnlArray[i+5].Put(AnalysisCounter,AnalyzedAtoms[i]^.ParAtm.v);
                  end;
     VvsY:        For i:=1 to 5 do
                      IF AnalyzedAtoms[i]<>nil THEN
                         begin;
                               AnlArray[i].Put(AnalysisCounter,AnalyzedAtoms[i]^.ParAtm.y);
                               AnlArray[i+5].Put(AnalysisCounter,AnalyzedAtoms[i]^.ParAtm.v);
                         end;

     EvsT:        begin;
                        Tvec.Put(AnalysisCounter,Time);
                        Evec.Put(AnalysisCounter,energy);
                        KEvec.Put(AnalysisCounter,KineticEnergy);
                        PEvec.Put(AnalysisCounter,PotentialEnergy);
                  end;

     SvsT: begin;
                 Tvec.Put(1,Time);
                 For i:=1 to 5 do
                     If not(AnalyzedAtoms[i]=Nil) then
                        PowerArray[i].Put(AnalysisCounter,AnalyzedAtoms[i]^.power);
           end;

     YvsTDensity,
     YvsT3D:     ;
     End; {of Case}
End;


Procedure AnlyPlot.RefreshAnalysisPlot(Clear: Boolean; Ix, Iy, Fx, Fy: Real);

Begin;
      If AnalysisPlot=Disabled then Exit;
      If Visible then Plot.Remove;
      SetScale( Ix, Iy, Fx, Fy );
      If Visible Then
        Begin
        Plot.Display;
        If not(Clear) then Replot;
        End;
End;

Procedure AnlyPlot.AtomAnalysisSelection( TheChain: PlotPtr);

Const MaxAnalyzedAtms=5;

Var
   ItemClicked:         BasePtr;
   OldColour:           Word;
   i:                   Integer;
   AnalyScrn:           TInputScreen;
   Ok,Cancel,
   Leave:               Boolean;
   LimitStr,
   NumStr:              String;
{   Buttons:             TButtons; }
   Button_OK, Button_Cancel, Button_Clear : TButton;
   SelectList:          Array[1..MaxAnalyzedAtms] of BasePtr;

Function ArrayNumber: Integer;

Var
   i,TheNum:   Integer;

Begin
     TheNum:=0;
     For i:=1 to 5 do if (AnalyzedAtoms[i]<>Nil) then Inc(TheNum);
     ArrayNumber:=TheNum;
End;



  Procedure ThreeD;
          Begin;
                MatSize:=(Yvec.Length-2);
                OldCount:=(Yvec.Length-2);
                PlotAxes.Remove;
                DMatPositionCompress(MatSize);
                If  (AnalysisPlot=YvsTDensity) then
                    With Scale do
                         begin
                              xMin:=1;
                              xMax:=(Yvec.Length-2);
                              yMin:=Time;
                              yMax:=Time + 1;
                              SetScale(xMin,yMin,xMax,yMax)
                         end;
                PlotAxes.Display;
          End;

  Procedure MultiAtom( limit: Integer);

  VAR
     i,NumSources:        Integer;
  Begin
      Message('..............Click on all the atoms you wish to analyze................');
      Leave:=FALSE;

      Remove;
      DefineViewPort( 4, Xo, Xo + Width, Yo, Yo + Height );
      GraphBackColor := blue;
      OpenViewPort(4);
      GraphBackColor := DarkGray;
{        With Buttons do
         begin;
               Init;
               Create(11,Xo+0.05,Yo+0.05,'Clear');
               Create(22,Xo+0.05,Yo+0.10,'Edit');
               Create(33,Xo+0.25,Yo+0.05,'Accept');
               Create(44,Xo+0.25,Yo+0.10,'Cancel');
               Number:=22;
               Buttons.DrawAll;
         end;  }
      Button_Clear.Create (Xo+0.24,Yo+0.05,' Clear ','Del');
      Button_OK.Create    (Xo+0.02,Yo+0.05,'  OK  ','Return');
      Button_Cancel.Create(Xo+0.13,Yo+0.05,'Cancel','Esc');
      NumSources := 0;
      For i:=1 to Limit DO
        Begin
        Rubout(2,i,18,LightBlue);
        SelectList[i]:=Nil;
        If AnalysisPlot=SvsT Then
        Begin
            If (AnalyzedAtoms[i]<>nil) AND (AnalyzedAtoms[i]^.Source=TRUE)
            Then begin
               SelectList[i]:=AnalyzedAtoms[i];
               inc(NumSources);
            end;
        End
        Else SelectList[i]:=AnalyzedAtoms[i];
        If SelectList[i]=nil
          Then  Print(2,i,'Atom Number=')
          Else
            Begin
            Str(SelectList[i]^.ParAtm.Num:3, NumStr);
            Print(2,i,'Atom Number='+NumStr);
            End;
        End;

      Rubout(19,1,1,LightRed);
      i:=0;
      if loading Then
        Begin
        Delay(250);
        Leave:=True;
        End;
      if (AnalysisPlot=SvsT) and (NumSources=0) then begin
         announce('You do not have any source atoms');
         AnalysisPlot:=Disabled;
      end else
      Repeat
         CheckforEvents;
         SelectViewPort(4);
         if Button_Clear.Clicked then
         Begin
            For i:=1 To Limit DO Begin
              Rubout(2,i,18,LightBlue);
              Print(2,i,'Atom Number=');
              SelectList[i]:=Nil;
              End;
            Rubout(19,1,1,LightRed);
            i:=0;
         End;
         if Button_OK.Clicked then
         begin
            Leave:=True;
            For i:=1 to Limit do AnalyzedAtoms[i]:=SelectList[i];
            If AnalyzedAtoms[1]=nil Then AnalysisPlot:=Disabled;
         end;  {normal exit}
         if Button_Cancel.Clicked then
         begin
            {If AnalyzedAtoms[1]=nil Then} AnalysisPlot:=Disabled;
            Leave:=True;
         end; {Canceled.  No Atoms selected}
         If Event.mouseClicked THEN   {look for clicked atom}
         Begin
            AtomList.ReturnClickedItem( itemclicked );
            If ItemClicked<>Nil THEN
              Begin
              If (AnalysisPlot=SvsT) and not(ItemClicked^.Source=True) then
                 Announce('That Atom is not a Source.')
              else
                  begin;
                        SelectList[(i MOD Limit)+1]:=itemclicked;
                        Str(Itemclicked^.ParAtm.Num:3, NumStr);
                        SelectViewPort(4);
                        Rubout(2,(i MOD Limit) +1,18,LightBlue);
                        Print(2,(i MOD Limit)+1,'Atom Number='+NumStr);
                        Rubout(19,((i+1) Mod Limit)+1,1,LightRed);
                        Inc(i);
                  end;
              End;
          End;
      Until Leave;
      CloseViewport(4);
{      Buttons.Done;}
      Plot.Display;
      Message('');
      If (AnalysisPlot=YvsY) and (ArrayNumber<2) then AnalysisPlot:=Disabled;
  End;

  Procedure Fourier;

  VAR
     msg:      String;
  Begin;
        FFTSize:=1;
        While (2*FFTSize<=(yVec.Length-2)) DO FFTSize:=FFTSize*2;
        Str(FFTSize:4,msg);
        msg:=Concat('Fast Fourier Analysis must use  2^n atoms.  Analysis will use: ',msg);
        If fftSize <> yVec.Length -2 Then Announce(msg);

        If MaxHistVec.Length<>FFTSize Then
           begin;
                 MaxHistVec.free;
                 MaxHistVec.Init(FFTSize);
           end;
        If FFTVec.Length<>FFTSize Then
           begin;
                 FFTVec.Free;
                 FFTVec.Init(FFTSize);
           end;
        If FFTx.Length<>FFTSize Then
           begin;
                 FFTx.Free;
                 FFTx.Init(FFTSize);
           end;

        FourierAnalysis(True,True);
  End;
  Procedure SelectionFailed;
  Begin
        ClearAnalysis;
        AnalysisPlot:=Disabled;
        AnlyType:=None;
        PlotTitle.Remove;
        If PlotAxes.Visible then PlotAxes.Remove;
        PlotAxes.Visible:=FALSE;
        Plottitle.SetStr('Disabled');
  End;



Begin;
      InitialTime:=Time;
      ClearAnalysis;
      SetAnalysisType;

      TimeCompress:=1;
      Case AnalysisPlot of
           YvsT3D,YvsTDensity:       ThreeD;
           YvsY,SvsT,
           YvsT, VvsT, VvsY:
             Begin
             MultiAtom(5);
             if AnalyzedAtoms[1]=nil Then SelectionFailed;
             end
      End; {of CASE}

      If NOT(SetUpAnalysis(MatSize)) Then SelectionFailed;

      Case AnalysisPlot of
              FFT_X,
              FFT_V:              Fourier;
              FFT_T:              begin
                                          FFTsize:=256;
                                          MultiAtom(1);
                                          If AnalyzedAtoms[1]=nil
                                          Then SelectionFailed
                                          Else
                                            Begin
                                            AmpVec.Resize(FFTsize);
                                            If MaxHistVec.Length<>FFTSize Then
                                             begin
                                                  MaxHistVec.free;
                                                  MaxHistVec.Init(FFTSize);
                                             end;
                                            If FFTVec.Length<>FFTSize Then
                                             begin
                                                  FFTVec.Free;
                                                  FFTVec.Init(FFTSize);
                                             end;
                                            If FFTx.Length<>FFTSize Then
                                             begin
                                                  FFTx.Free;
                                                  FFTx.Init(FFTSize);
                                             end;
                                          End;
                                     end;
         End;

      If (AnalysisPlot=Disabled) then
         begin
         If PlotAxes.Visible then PlotAxes.Remove;
         plotAxes.Visible:=FALSE;
         Display;
         end;
End;



Procedure AnlyPlot.SetAnalysisType;

Begin;
      Case AnalysisPlot of
           YvsT,VvsT,VvsY,YvsY: AnlyType:=Dynamic;
           YvsT3D,YvsTDensity:  AnlyType:=ThreeD;
           FFT_x,FFT_V,FFT_t:                 AnlyType:=FastFT;
           SvsT,
           EvsT:                AnlyType:=VersusTime;
           Disabled:            AnlyType:=None;
      End;
End;

Procedure AnlyPlot.ChangeAttributes;

Var
   AttrScrn:            TInputScreen;
   Euler,
   Ok,Cancel,
   OldFreeze:           Boolean;
   i:                   Word;
   min,max:             point3d;
   OldFFTsize:          Integer;
(*
Procedure RotateBox;

Var
   Box:                 TGraph3D;
   Info:                TSliders;
   Leave:               Boolean;
   FillInfo:            FillSettingsType;
   OldColour:           Word;

Begin;
      OldColour:=GetColor;
      GetFillSettings(FillInfo);
      SelectViewport(0);
      SetFillStyle(SolidFill,Black);
      Bar(0,round(0.045*GetMaxY),Round(0.45*GetMaxX),Round(0.45*GetMaxy));
      With FillInfo Do SetFillStyle(Pattern,Color);

      {Anly1.Remove;}
      SetColor(White);
      DefineViewPort(4, 0.20, 0.45, 0.55, 0.80);
      OpenViewPort(4);
      Box.Init;
      Box.Arrows:=True;
      Box.SetBoxRatios(1,4/5,2/3);
      Box.SetViewAngles(EuAngles[1],EuAngles[2],EuAngles[3]);

      Info.init;
      Info.create(1,  0 ,360,EuAngles[1], 0.12,0.85,0.32,0, '0','360','alpha',false);
      Info.create(2,-180,180,EuAngles[2], 0.12,0.80,0.55,0, '', '',   'beta' ,true );
      Info.drawAll;
      Message('Double click the mouse to leave.');

      SetWriteMode(XorPut);
      Box.DrawBox;

      Leave:=False;
      Repeat
            CheckforEvents;
            If Info.changed then
               begin;
                     Box.DrawBox;
                     EuAngles[1]:=Info.value(1);
                     EuAngles[2]:=Info.value(2);
                     Box.SetViewAngles(EuAngles[1],EuAngles[2],EuAngles[3]);
                     Box.DrawBox;
               end;
            If Event.DoubleClicked then Leave:=True;
      Until Leave;
      SetWriteMode(CopyPut);
      Message('');
      MainMenu.Display;
      SelectViewPort(SclNum);
      TheThreeD.SetViewAngles(EuAngles[1],EuAngles[2],EuAngles[3]);
      SetColor(OldColour);
End; *)

Procedure RotateBox;

Var
   Box:                 TPlot3D;
   Leave:               Boolean;
   FillInfo:            FillSettingsType;
   OldColour:           Word;

Begin;
      OldColour:=GetColor;
      GetFillSettings(FillInfo);
      SelectViewport(0);
      SetFillStyle(SolidFill,Black);
      Bar(0,round(0.045*GetMaxY),Round(0.45*GetMaxX),Round(0.45*GetMaxy));
      With FillInfo Do SetFillStyle(Pattern,Color);

      SetColor(White);
      DefineViewPort(4, 0.10, 0.45, 0.55, 0.90);
      GraphBackColor := Black;
      Box.Init(4);
      Box.Arrows:=True;
      Box.SetBoxRatios(1,4/5,2/3);
      Box.SetViewAngles(EuAngles[1],EuAngles[2],EuAngles[3]);
      Box.PlotMatrix(oldMatrix,'');

      Message('Double click the mouse to leave.');
      Leave:=False;
      Repeat
            CheckforEvents;
            box.check;
            If Event.DoubleClicked then Leave:=True;
      Until Leave;
      GraphBackColor := DarkGray;
      MainMenu.Display;
      SelectViewPort(SclNum);
      Box.GetViewAngles(EuAngles[1],EuAngles[2],EuAngles[3]);
      TheThreeD.SetViewAngles(EuAngles[1],EuAngles[2],EuAngles[3]);
      SetColor(OldColour);
      DefineViewPort(4, 0.08, 0.44, 0.54, 0.92);
      CloseViewPort(4);
End;

Function ArrayNumber: Integer;

Var
   i,TheNum:   Integer;

Begin
     TheNum:=0;
     For i:=1 to 5 do if (AnalyzedAtoms[i]<>Nil) then Inc(TheNum);
     ArrayNumber:=TheNum;
End;

Begin;
      CheckIt(4,False,@Anly1);
      CheckIt(5,False,@Anly2);
      Euler:=False;
      OldFreeze:=Freeze;
      OldFFTsize:=FFTsize;
      If AnlyType=ThreeD then
         begin
              TheContour.Init;
              TheThreeD.Init;
              TheThreeD.SetDistance(1.5,2.5);
              TheThreeD.SetBoxRatios(1,4/5,2/3);
         end;
      With AttrScrn do
           begin;
                 Init;
                 Case AnlyType of
                      None:       DefineInputPort( 0.30, 0.70, 0.50 ,0.68 );
                      VersusTime: DefineInputPort( 0.30, 0.67, 0.50 ,0.73 );
                      FastFT:     DefineInputPort( 0.30, 0.67, 0.40 ,0.90 );
                      Dynamic:    DefineInputPort( 0.30, 0.67, 0.50 ,0.90 );
                      ThreeD:     DefineInputPort( 0.30, 0.67, 0.12 ,0.87 );
                 End;

                 LoadLine('         Attributes        ');

If not(AnlyType=none) then LoadLine('   Freeze Plot        #T   ');

                 Case AnlyType of
                      Dynamic:
                              begin;
                                    LoadLine('  Y Versus Time     #1   ');
                                    LoadLine('  Vel Versus Time   #1   ');
                                    LoadLine('  Vel Versus Y      #1   ');
                                    LoadLine('  Y-1 Versus Y-2    #1   ');
                              end;
                      ThreeD:
                             begin;
                                   TheThreeD.Get3DScales(min,max);
                                   LoadLine('---Y-Time Contour-----#1---');
                                   LoadLine('                           ');
                                   LoadLine('Palette Number(0-4) {    } ');
                                   LoadLine('Legend                #F   ');
                                   LoadLine('                           ');
                                   LoadLine('---Y-Time 3D----------#1---');
                                   LoadLine('                           ');
                                   LoadLine(' Z - Min  =  '+Num2Str(Min[3],4));
                                   LoadLine(' Z - Max  =  '+Num2Str(Max[3],4));
                                   LoadLine('                           ');
                                   LoadLine(' DrawBox              #F   ');
                                   LoadLine('                           ');
                                   LoadLine('   [Change Euler Angles]   ');
                             end;
                      FastFT:
                             If (AnalysisPlot<>FFT_T) then
                                begin
                                     LoadLine('   FFT of Y(x)        #1   ');
                                     LoadLine('   FFT of V(x)        #1   ');
                                     LoadLine('                           ');
                                     LoadLine('FFT Basis set: ');
                                     LoadLine('   Sin #2      Sin/Cos #2  ');
                                     LoadlIne(' Show Maximum Values    #T ');
                                end
                             else
                                begin
                                     LoadLine('  #1    #1     #1     #1   ');
                                     LoadLine('  256   512   1024   2048  ');
                                     LoadLine('  Number of Points in FFT  ');
                                     LoadLine('                           ');
                                     LoadlIne(' Show Maximum Values    #T ');
                                end;
                      None: LoadLine('            None           ');
                 End;
                 Loadline('                           ');
                 LoadLine(' [  Ok  ] [Cancel] [ Help ]');
                 LoadLine('                           ');
                 Case AnlyType of
                      None:        SetHelpFile('Chain.Hlp', 'No Help');
                      Dynamic:     SetHelpFile('Chain.Hlp', 'Dynamic Help');
                      ThreeD:      SetHelpFile('Chain.Hlp', 'ThreeD Help');
                      VersusTime:  SetHelpFile('Chain.Hlp', 'VersusTime Help');
                      FastFT:      Case AnalysisPlot of
                                        FFT_X,
                                        FFT_V: SetHelpFile('Chain.Hlp', 'FFT-x Help');
                                        FFT_T: SetHelpFile('Chain.Hlp', 'FFT-t Help');
                                   End; {of Case}
                 End; {of Case}

                 If not(AnlyType=None) then SetBoolean(1,Freeze); {Sets Initial Analysis Selection}

                 Case AnlyType of
                      Dynamic:
                              Case AnalysisPlot of
                                   YvsT:  SetRadioButton('1',1);
                                   VvsT:  SetRadioButton('1',2);
                                   VvsY:  SetRadioButton('1',3);
                                   YvsY:  SetRadioButton('1',4);
                              End;
                      ThreeD:
                             begin;
                                   Case AnalysisPlot of
                                        YvsTDensity: SetRadioButton('1',1);
                                        YvsT3D: SetRadioButton('1',2);
                                   End;
                                   {RePlot;}
                                   SetNumber(3,PalNum);
                                   SetBoolean(4,LegBool);
                                   SetBoolean(6,BoxBool);
                             end;
                      FastFT:Case AnalysisPlot of
                               FFT_X: begin
                                      SetRadioButton('1',1);
                                      SetBoolean(6,ShowMaxHist);
                                      If FFTBasis=SinOnly Then
                                         SetRadioButton('2',1) Else SetRadioButton('2',2);
                                      end;
                               FFT_V: begin
                                      SetRadioButton('1',2);
                                      SetBoolean(6,ShowMaxHist);
                                      If FFTBasis=SinOnly Then
                                        SetRadioButton('2',1) Else SetRadioButton('2',2);
                                      end;
                               FFT_T:
                                      Begin
                                      Case FFTsize of
                                          256:  SetRadioButton('1',1);
                                          512:  SetRadioButton('1',2);
                                          1024: SetRadioButton('1',3);
                                          2048: SetRadioButton('1',4);
                                     End;
                                     SetBoolean(6,ShowMaxHist);
                                     end;
                               End{of AnalysisPlot Case}
                 End;

                 AcceptScreen;

                 Case AnlyType of         {Gets the Ok Button's boolean}
                      Dynamic:              Ok:=GetBoolean(6);
                      FastFT:               If AnalysisPlot=FFT_T
                                              then Ok:=GetBoolean(7)
                                              else Ok:=GetBoolean(7);
                      ThreeD:               begin;
                                                  Euler:=GetBoolean(7);
                                                  Ok:=GetBoolean(8);
                                            end;
                      VersusTime:           Ok:=GetBoolean(2);
                      None:                 Ok:=GetBoolean(1);
                 End;
                 If Ok or Euler then PlotTitle.Remove;
                 If Ok or Euler then
                 Case AnlyType of       {Get the radiobutton's boolean}
                      None,
                      VersusTime: ;
                      Dynamic:    Case GetRadioButton('1') of
                                          1:Begin
                                            AnalysisPlot:=YvsT;
                                            Plottitle.SetStr('Displacement: Y(t)');
                                            End;
                                          2:Begin
                                            AnalysisPlot:=VvsT;
                                            Plottitle.SetStr('Velocity: V(t)');
                                            End;
                                          3:Begin
                                            AnalysisPlot:=VvsY;
                                            Plottitle.SetStr('Phase Space: V(Y)');
                                            end;
                                          4: If not(ArrayNumber<2) then
                                            Begin
                                            AnalysisPlot:=YvsY;
                                            Plottitle.SetStr('Y1(t) vs Y2(t)');
                                            End;
                                  End;
                      ThreeD:     begin;
                                        Case GetRadioButton('1') of
                                             1:
                                               begin;
                                                     If (AnalysisPlot=YvsT3D) then
                                                            With Scale do
                                                                 begin;
                                                                 PlotAxes.Visible:=TRUE;
                                                                       xMin:=1;
                                                                       xMax:=(Yvec.Length-2);
                                                                       yMin:=InitialTime;
                                                                       yMax:=Time;
                                                                       if ymax<=ymin then ymax:=ymin+1;
                                                                       SetScale(xMin,yMin,xMax,yMax)
                                                                 end;
                                                     AnalysisPlot:=YvsTDensity;
                                                     PalNum:=trunc(GetNumber(3));
                                                     LegBool:=GetBoolean(4);
                                                     Plottitle.SetStr('Space Time: Contour');
                                               end;
                                             2:
                                               begin;
                                                     If (AnalysisPlot=YvsTDensity) then Plotaxes.Remove;
                                                     PlotAxes.Visible:=False;
                                                     AnalysisPlot:=YvsT3D;
                                                     Plottitle.SetStr('Space Time: 3D');
                                                     BoxBool:=GetBoolean(6);
                                                     TheThreeD.boxDrawn := BoxBool;
                                                     If Euler then
                                                        begin;
                                                              If Expanded then Remove;
                                                              Visible:=True;
                                                              RotateBox;
                                                              Update;
                                                              If not(GetBoolean(1)) then Exit;
                                                        end;
                                                end;
                                        End;
                                  end;
                      FastFT:    If not(AnalysisPlot=FFT_T) then
                                    begin
                                         Case GetRadioButton('1') of
                                              1:Begin
                                                AnalysisPlot:=FFT_x;
                                                Plottitle.SetStr('FFT: Y(x)');
                                                End;
                                              2:Begin
                                                AnalysisPlot:=FFT_V;
                                                Plottitle.SetStr('FFT: V(x)');
                                                End;
                                         End;
                                         ShowMaxHist:=GetBoolean(6);
                                         If GetRadioButton('2')=1
                                          Then FFTBasis:=SinOnly
                                          Else FFTBasis:=SinCos;
                                         With Scale do
                                              RefreshAnalysisPlot(True,xMin,yMin,xMax,yMax);
                                         FourierAnalysis(True,True);
                                    end
                                 else
                                    begin
                                         AnalysisPlot:=FFT_T;
                                         Plottitle.SetStr('FFT: Y(t)');
                                         Case GetRadioButton('1') of
                                              1: FFTsize:=256;
                                              2: FFTsize:=512;
                                              3: FFTsize:=1024;
                                              4: FFTsize:=2048;
                                         End;
                                         ShowMaxHist:=GetBoolean(6);
                                         If (OldFFTsize<>FFTsize) then
                                            begin
                                                 AnalysisCounter:=1;
                                                 OnlyOnce:=True;
                                                 Plot.Display;

                                                 AmpVec.Resize(FFTsize);

                                                 AmpVec.Fill(0);

                                                 If MaxHistVec.Length<>FFTSize Then
                                                    begin
                                                         MaxHistVec.free;
                                                         MaxHistVec.Init(FFTSize);
                                                    end;
                                                 If FFTVec.Length<>FFTSize Then
                                                    begin
                                                         FFTVec.Free;
                                                         FFTVec.Init(FFTSize);
                                                    end;
                                                 If FFTx.Length<>FFTSize Then
                                                    begin
                                                         FFTx.Free;
                                                         FFTx.Init(FFTSize);
                                                    end;
                                                 FourierAnalysis(TRUE,TRUE);
                                            end
                                         else FourierAnalysis(True,True);
                                    end;
                      End;

                 If Ok and not(AnlyType=None) or (Euler) then
                    begin;
                          Freeze:=GetBoolean(1);
                          If Freeze Then  Plottitle.SetTitleColor(Red);
                          If not(OldFreeze=Freeze) then
                             If (OldFreeze) then
                                begin;
                                      ClearAnalysis;
                                      AnlyType:=none;
                                      AnalysisPlot:=Disabled;
                                      TimeCompress:=1;
                                      PlotTitle.Remove;
                                      Plottitle.SetStr('Disabled');
                                      PlotTitle.Display;
                                      If PlotAxes.Visible then
                                         begin
                                              PlotAxes.Remove;
                                              PlotAxes.Visible:=False;
                                         end;
                                      Plot.Display;
                                      For i:=1 to 5 do AnalyzedAtoms[i]:=nil;
                                      CheckIt(4,True,@Anly1);
                                      CheckIt(5,True,@Anly2);
                                      Exit;
                                end;
                    end;

                 Done;
           end;

           If ((AnalysisPlot=YvsTDensity) or (AnalysisPlot=YvsT3D)) then
              begin;
                    PlotAxes.Display;
                    PlotTitle.Display;
                    RePlot;
                    CheckIt(4,True,@Anly1);
                    CheckIt(5,True,@Anly2);
                    Exit;
              end;

           If not(AnalysisCounter=1)  or  not(AnalysisPlot in [FFT_x,FFT_V,FFT_T]) then
              With Scale do
                   RefreshAnalysisPlot(False,xMin,yMin,xMax,yMax);
           PlotTitle.Display;
           CheckIt(4,True,@Anly1);
           CheckIt(5,True,@Anly2);
End;

Procedure AnlyPlot.ClearAnalysis;

Var
   i:   Integer;

Begin
For i:=1 to 10 do AnlArray[i].Free;
Tvec.Free;
OldMatrix.Free;
TheMatrix.Free;
AmpVec.Free;
maxHistVec.Free;
fftVec.Free;
fftx.Free;
Evec.Free;
KEvec.Free;
PEvec.Free;
For i:=1 to 5 do PowerArray[i].Free;
End;

Function AnlyPlot.SetUpAnalysis( MatrixSize: Integer ):Boolean;

Var
   i:   Integer;

Begin;
      SetupAnalysis:=TRUE;
      HaltIfError:=FALSE;
      Case AnlyType of
           Dynamic:
                   begin
                   For i:=1 to 10 do
                     Begin
                     AnlArray[i].Init(Alength);
                     If ErrorFound Then
                       Begin
                       SetUpAnalysis:=FALSE;
                       ErrorFound:=False;  {We'll take care of this ourselves}
                       End;
                     End;
                   Tvec.Init(Alength);
                   If ErrorFound Then
                       Begin
                       SetUpAnalysis:=FALSE;
                       Announce('Not enough memory.');
                       ErrorFound:=False;  {We'll take care of this ourselves}
                       End;
                   end;
           ThreeD:
                  begin;
                  OldMatrix.Init(MatrixSize,MatrixSize);
                  If ErrorFound
                    Then SetUpAnalysis:=FALSE
                    Else OldMatrix.Fill(0);
                  TheMatrix.Init(MaTrixSize,MatrixSize);
                  If ErrorFound
                    Then  SetUpAnalysis:=FALSE
                    Else TheMatrix.Fill(0);
                  If ErrorFound Then Announce('Not enough memory.');
                  ErrorFound:=FALSE;
                  end;
           FastFT:
                  begin
                  AmpVec.Init(Alength);
                  If ErrorFound Then
                       Begin
                       SetUpAnalysis:=FALSE;
                       Announce('Not enough memory.');
                       ErrorFound:=False;  {We'll take care of this ourselves}
                       End;
                  maxHistVec.Init(Alength);
                  If ErrorFound Then
                       Begin
                       SetUpAnalysis:=FALSE;
                       Announce('Not enough memory.');
                       ErrorFound:=False;  {We'll take care of this ourselves}
                       End;
                  fftVec.Init(Alength);
                  If ErrorFound Then
                       Begin
                       SetUpAnalysis:=FALSE;
                       Announce('Not enough memory.');
                       ErrorFound:=False;  {We'll take care of this ourselves}
                       End;
                  fftx.Init(Alength);
                  If ErrorFound Then
                       Begin
                       SetUpAnalysis:=FALSE;
                       Announce('Not enough memory.');
                       ErrorFound:=False;  {We'll take care of this ourselves}
                       End;
                  end;
           VersusTime:
                      begin;
                            Case AnalysisPlot of
                                 EvsT:
                                   begin
                                   Evec.Init(Alength);
                                   If ErrorFound Then
                                     Begin
                                     SetUpAnalysis:=FALSE;
                                     ErrorFound:=False;  {We'll take care of this ourselves}
                                     End;
                                   KEvec.Init(Alength);
                                   If ErrorFound Then
                                     Begin
                                     SetUpAnalysis:=FALSE;
                                     ErrorFound:=False;  {We'll take care of this ourselves}
                                     End;
                                   PEvec.Init(Alength);
                                   If ErrorFound Then
                                     Begin
                                     SetUpAnalysis:=FALSE;
                                     ErrorFound:=False;  {We'll take care of this ourselves}
                                     End;
                                   end;
                                 SvsT: For i:=1 to 5 do
                                   Begin
                                   PowerArray[i].Init(Alength);
                                   If ErrorFound Then
                                     Begin
                                     SetUpAnalysis:=FALSE;
                                     ErrorFound:=False;  {We'll take care of this ourselves}
                                     End;
                                   End;
                            End; {of Case}
                            Tvec.Init(Alength);
                            If ErrorFound Then
                                     Begin
                                     SetUpAnalysis:=FALSE;
                                     Announce('Not enough memory.');
                                     ErrorFound:=False;  {We'll take care of this ourselves}
                                     End;
                      end;
           None: ;
      End; {of Case}
HaltIfError:=TRUE;
End;

Procedure AnlyPlot.CompressAnalysis;

Var
   i:   Integer;

Begin;
      If Freeze then exit;
      DvecCompress:=DvecCompress*2;
      DvecCompressCounter:=DvecCompress;

      Case AnlyType of
           Dynamic:
                   begin;
                         For i:=1 to 10 do
                             CompactDvec(AnlArray[i]);
                         CompactDvec(Tvec);
                         AnalysisCounter:=AnalysisCounter Div 2;
                   end;
           VersusTime:
                      begin;
                            Case AnalysisPlot of
                                 EvsT: begin;
                                             CompactDvec(Evec);
                                             CompactDvec(KEvec);
                                             CompactDvec(PEvec);
                                       end;
                                 SvsT: For i:=1 to 5 do
                                           CompactDvec(PowerArray[i]);
                            End; {of Case}
                            CompactDvec(Tvec);
                            AnalysisCounter:=AnalysisCounter Div 2;
                      end;
           ThreeD,
           FastFT,
           None: ;
      End; {of Case}
End;

Procedure AnlyPlot.Display;
Begin;
      Plot.Display;
      Replot;
End;


Procedure AnlyPlot.ClearScreen;

Begin
     Plot.Display;
End;

end.
