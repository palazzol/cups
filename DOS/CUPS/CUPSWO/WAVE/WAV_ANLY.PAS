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
Unit Wav_Anly;
Interface
Uses Wav_Base,Wav_Plot,Wav_Gbl, Wav_Graf,Wav_Lst, Wav_SS,
     Cups,CupsGui,CUPSmupp,CupsGrph,CupsFunc,CupsProc,Crt,Graph;
Const
      Decimal=4;
Type

GeneralAnalysisType = ( None, ThreeD, FastFT, Dynamic, VersusTime, VersusPosition, EnergyRelated);
FFTBasisType=(sinOnly,sinCos);
RotationAngles = Array[1..3] of Real;

AnlyPlotPtr=^AnlyPlot;
AnlyPlot=Object(Plot)
          GeneralType:                  GeneralAnalysisType;
          FFTBasis:                     FFTBasisType;
          OldMatrix,
          TheMatrix:                    DMatrix;
          MaxHistVec:                   WVector;{Holds the maximum histogram values.}
          FFTVec:                       DVector;{Holds the packed yvec values for inplace fft}
          FFTx:                         DVector;{holds the x coords for the hist.}
          Rwave,
          Lwave,
          Tvec,
          KEvec,
          PEvec,
          Evec,
          DistVec,
          PowerVec:                     Dvector;
          Darray:                       DvectorArray;
          FFTSize:                      Integer;{Number of points in fft.  Must be power of 2}
          PalNumber,
          MatSize,
          xCompress,
          TimeCompress,
          DvecCompress,
          DvecCompressCounter,
          AnalysisCounter,                   {AnalysisCounter: number of data points that are up to date.}
          SkipCounter:                  Integer;{Counter that is decremented to determine if data will be recorded}
          EuAngles:                     RotationAngles;
          LegBool,
          BoxBool,
          ShowWaitMsg,
          ShowMaxHist:                  Boolean;
          Distype:                      DisplayType;
          SourceClicked,
          DetectorClicked:              BasePtr;
          InitialTime:                  Real;
          ShowEnergyType:               EnergyComponetsType;
            Constructor Initialize( ISclNum: Byte; x, y, w, h: Real);
            Destructor Done;                       Virtual;
            Procedure AnalysisSelection( theplot: PlotPtr);
            Procedure RefreshAnalysisPlot(Clear: Boolean; Ix, Iy, Fx, Fy: Real);
            Procedure IntializeAnalysisPlots;
            Procedure UpdatePlot;
            Procedure Display;                     Virtual;
            Procedure RePlot;                      Virtual;  {draws inside a plot}
            Procedure ChangeScale;                 Virtual;
            Procedure Archive(Var filehandle: text);Virtual;
            Procedure UnArchive(Var filehandle: text);
            Procedure Disable;
            Procedure DMatTimeCompress( mat: Dmatrix );
            Procedure DMatPositionCompress( Size: Integer );
            Procedure CompactDVec( vec: Dvector );
            Procedure MaxMin(Var MinX, MaxX, MinY, MaxY: Real);
            Procedure ReFFT;
            Procedure SinFFT;
            Procedure Four1(nn, isign1D : integer);
            Procedure HistPlot(data:Dvector; n: Integer);
            Procedure HistScale(Data:Dvector; n:Integer);
            Procedure ZeroMaxHistVec(n: Integer);
            Procedure SetMaxHistVec(Data:DVector; n: Integer);
            Procedure FourierAnalysis( ScaleIt,ResetIt,PlotIt: Boolean );
            Procedure DensityPlot;
            Procedure IndexPlot;
            Procedure SetGeneralType;
            Procedure RemoveStorage;
            Procedure CreateStorage;
            Procedure CompressAnalysis;
            Procedure ShowAttrib;                       Virtual;
            Procedure ClearScreen;
            Procedure ShowTimeAverage;
            Procedure AverageQMCurrent;
            Procedure ShowSpaceAverage;
            Procedure ResetAndInit;
end;

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
BEGIN
    n:=fftsize DIV 2;
    iSign:=1;  {always do forward transform}
    two:=2;
    dataPtr:=FFTVec.VecPtr;  {use the dVector's data}
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
  if Max<=0 Then Max:=0.30;  {Assume the user hasn't initialized the wave yet.}
  PlotAxes.remove;
  If (EquationType=Schroedinger) AND (FFTBasis=SinCos)
    Then SetScale( -n Div 2 -1, -0.01, n Div 2+1, 2*Max )
    Else SetScale( 0, -0.01, n+1, 2*Max );
  PlotAxes.Display;
End;

Procedure AnlyPlot.ZeroMaxHistVec(n: Integer);

Var
   i,yPix:    Integer;

Begin
      yPix:=Mapy(0);
      For i:=1 To n Do maxHistVec.wVecPtr^[i]:=yPix;
      If (EquationType=Schroedinger) AND (FFTBasis=SinCos)
        Then For i:=1 To n Do FFTx.VecPtr^[i]:=i- (n Div 2)-1
        Else For i:=1 To n Do FFTx.VecPtr^[i]:=i;
End;

Procedure AnlyPlot.SetMaxHistVec(Data:DVector; n: Integer);
Var Rect:  Array[1..4] of PointType;
    i:          Integer;
Begin
  For i:=1 To n Do
    maxHistVec.wVecPtr^[i]:=MapY(data.Value(i)); {Pixel number increases from TOP to BOTTOM of screen.}
END;

Procedure Anlyplot.HistPlot(data:Dvector; n: Integer);

Var Rect:  Array[1..4] of PointType;
    OldColour,
    BarWidth:   Word;
    i:          Integer;
    two:        Integer;
    FillInfo:   FillSettingsType;
    start,stop: Integer;
    Offset:     Integer;

Begin
     GetFillSettings(FillInfo);
     OldColour:=GetColor;
     LockFocus;
     hideCursor;
     openViewPort(1);  {clear the viwport to background color}
     buttonlist.display;
     Barwidth:=Round((MapX(FFTx.Value(2))-MapX(FFTx.Value(1)))*0.45);
     if (EquationType=Schroedinger) And (FFTBasis=SinCos)
       Then offset:= n Div 2 Else Offset:=0;
     if Round(Scale.xmin)+offset<1 then start:=1 Else start:=Round(scale.xmin)+offset;
     if Round(Scale.xmax-0.5)+offset>n then stop:=n Else stop:=Round(scale.xmax-0.6)+offset;
     For i:=start To stop Do
        Begin;
        rect[1].x:=Mapx(FFTx.Value(i));  rect[1].y:=MapY(0);
        rect[2].x:=rect[1].x; rect[2].y:=MapY(data.Value(i));
        If rect[2].y<=0 Then rect[2].y:=1;
        rect[3].x:=rect[1].x+BarWidth; rect[3].y:=rect[2].y;
        rect[4].x:=rect[3].x; rect[4].y:=rect[1].y;
        SetColor(red); SetFillStyle(solidFill,red);
        FillPoly(4,Rect);

        rect[1].y:=rect[2].y; rect[4].y:=rect[3].y;
        If maxHistVec.wVecPtr^[i]>rect[2].y then maxHistVec.wVecPtr^[i]:=rect[2].y;
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
           rect[2].y:=maxHistVec.wVecPtr^[i]; rect[3].y:=rect[2].y;
           FillPoly(4,Rect);
           If ShowMaxHist then SetColor(White) else SetColor(GraphBackColor);
           Line(rect[2].x,rect[2].y,rect[3].x,rect[3].y);
      End; {For loop}
      With FillInfo Do SetFillStyle(Pattern,Color);
      showCursor;
      SetColor(OldColour);
END;


Procedure AnlyPlot.FourierAnalysis( ScaleIt,ResetIt,PlotIt: Boolean );


Var
   i,two:   Integer;
   halfFFTSize,
   quarterFFTSize:Integer;
   temp:    Real;

Procedure FFT_y_x;

Var
   i:   Integer;

Begin
     If EquationType=Schroedinger
       Then
         Begin
         TransformToSpaceGrid(yVec,TempSGVec);{This the the REAL par of wave}
         i:=1;
         While i<=FFTSize Div 2 Do Begin FFTVec.VecPtr^[2*i-1]:=tempSGVec.VecPtr^[i]; i:=i+1; End;  {Get the REAL part}
         i:=1;
         TransformToSpaceGrid(yPrevVec,TempSGVec);
         While i<=FFTSize Div 2 Do Begin FFTVec.VecPtr^[2*i]:=TempSGVec.VecPtr^[i]; i:=i+1; End;  {Get the imaginary part}
         End
       Else
         Begin
         TransformToSpaceGrid(displayVec,TempSGVec);
         For i:=1 to FFTSize Do     {pack data into FFTVec}
         FFTVec.VecPtr^[i]:=TempSGVec.VecPtr^[i];
         end;
End;

Procedure FFT_y_t;

Var
   i:   Integer;

Begin
     if EquationType=Schroedinger
     Then
         Begin
         i:=1;
         While i<=FFTSize Do Begin FFTVec.VecPtr^[i]:=RWave.VecPtr^[i]; i:=i+2; End;  {Get the REAL part}
         i:=2;
         While i<=FFTSize Do Begin FFTVec.VecPtr^[i]:=LWave.VecPtr^[i]; i:=i+2; End;  {Get the imaginary part}
         End
     else   For i:=1 to FFTsize do FFTvec.VecPtr^[i]:=Lwave.VecPtr^[i]+Rwave.VecPtr^[i];
End;
Begin
If not(isVisible) then exit;
If Freeze then
  begin ZeroMaxHistVec(FFTSize Div 2);
  HistPlot(FFTVec ,FFTSize DIV 2);
  exit;
  end;
If FFTBasis=SinCos then
  Begin {use Sin-Cos transform}
      two:=2;
      if AnalysisType=FFTtime then FFT_y_t  else  FFT_y_x;
      If EquationType= Schroedinger then Four1(fftSize Div 2,1) else Refft;
      if EquationType=Schroedinger Then
        Begin
        halfFFTSize:=fftSize Div 2;
        QuarterFFTSize:=fftSize Div 4;
        For i:=HalfFFTSize DownTo 1 DO
          FFTVec.VecPtr^[i+halfFFTSize]:=Sqrt(Sqr(FFTVec.VecPtr^[2*i-1]) +
                              Sqr(FFTVec.VecPtr^[2*i]))/halfFFTsize; {unpack Data into n/2 to n}
        for i:=1 to quarterFFTSize Do
          FFTVec.VecPtr^[i]:=FFTVec.VecPtr^[halfFFTSize+quarterFFTSize+i];
        for i:=1 to quarterFFTSize Do
          FFTVec.VecPtr^[i+quarterFFTSize]:=FFTVec.VecPtr^[halfFFTSize+i];
        end
      Else
        Begin
          temp:=Abs(FFTVec.VecPtr^[two])/FFTsize;
          For i:=2 to FFTSize DIV 2 DO
          FFTVec.VecPtr^[i-1]:=Sqrt(Sqr(FFTVec.VecPtr^[2*i-1]) +
                              Sqr(FFTVec.VecPtr^[2*i]))*2/FFTsize; {unpack Data}
          FFTvec.vecptr^[FFTsize div 2]:=temp;
        End;
      If ScaleIt then HistScale(FFTVec, FFTSize Div 2);
      If ResetIt then ZeroMaxHistVec(FFTSize Div 2);
      If PlotIt then
        HistPlot(FFTVec ,FFTSize DIV 2)
  End
Else  {use SIN transform}
  Begin;
      if EquationType=Schroedinger Then
        Begin
        fftSize:=fftSize Div 2; {Reduce the size of the fft since we will only use 1/2 the  wave data}
        For i:=1 to FFTSize Do
           FFTVec.VecPtr^[i]:=yVec.VecPtr^[i+hiddenPts];
         SinFFT;
         For i:=1 to FFTSize DO
           temp1OGVec.VecPtr^[i]:=FFTVec.VecPtr^[i];
         For i:=1 to FFTSize Do
           FFTVec.VecPtr^[i]:=yPrevVec.VecPtr^[i+hiddenPts];
         SinFFT;
         For i:=1 to FFTSize-1 DO
           FFTVec.VecPtr^[i]:=Sqrt(2*(Sqr(FFTVec.VecPtr^[i+1])+Sqr(temp1OGVec.VecPtr^[i+1])))/FFTSize;
         FFTVec.VecPtr^[fftSize]:=0;
         If ScaleIt then HistScale(FFTVec, FFTSize);
         fftSize:=fftSize*2; {Reset the size of the fft}
         End
      Else
        Begin
          FFT_y_x;
          SinFFT;
          For i:=1 to FFTSize-1 DO
            FFTVec.VecPtr^[i]:=2*Abs(FFTVec.VecPtr^[i+1])/FFTSize;
          fftvec.VecPtr^[fftsize]:=0;
          If ScaleIt then HistScale(FFTVec, FFTSize);
        End;
      If ResetIt then ZeroMaxHistVec(FFTSize);
      If PlotIt then
              HistPlot(FFTVec,FFTSize);
End;
End;
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------}
Constructor AnlyPlot.Initialize( ISclNum: Byte; x, y, w, h: Real );

Var i:Integer;

Begin
     Plot.Initialize( ISclNum, x, y, w, h);
     AnalysisType:=Disabled;
     PlotTitle.Remove;
     PlotTitle.SetStr('Disabled');
     PlotTitle.Display;
     GeneralType:=None;
     isVisible:=True;
     AnalysisCounter:=1;
     TimeCompress:= 1;
     SkipCounter:=1;
     disType:=ShowTotal;
     maxHistVec.Init(0);
     fftVec.Init(0);
     fftx.Init(0);
     Rwave.Init(0);
     Lwave.Init(0);
     TheMatrix.Init(0,0);
     OldMatrix.Init(0,0);
     Tvec.Init(0);
     Evec.Init(0);
     KEvec.Init(0);
     PEvec.Init(0);
     PowerVec.Init(0);
     DistVec.Init(0);
     For i:=1 to 3*MaxDetNum do Darray[i].Init(0);
     SetScale( 0, -1, 1, 1 );
     {PlotAxes.IsVisible:=False; changed to T by wc }
     PlotAxes.IsVisible:=True;
     PlotSclBut.Initialize( @self );
     ExpandBut.Initialize( @self );
     AttrBut.Initialize( @self );
     ShowMaxHist:=True;
     ShowWaitMsg:=True;
     LegBool:=False;
     BoxBool:=True;
     Freeze:=False;
     PlotTitle.setObjColor(Magenta);
     PalNumber:=2;
     EuAngles[1]:=20;
     EuAngles[2]:=30;
     EuAngles[3]:=0;
     Distype:=showTotal;
     DetectorClicked:=Nil;
     SourceClicked:=Nil;

     DvecCompress:=1;
     DvecCompressCounter:=1;

     ShowEnergyType:=Total;
End;
Destructor AnlyPlot.Done;

Var
   i:   Integer;

Begin;
      Plot.done;
      maxHistVec.Free;
      fftVec.Free;
      fftx.Free;
      Tvec.Free;
      TheMatrix.Free;
      OldMatrix.Free;
      Evec.Free;
      KEvec.Free;
      PEvec.Free;
      Rwave.Free;
      Lwave.Free;
      PowerVec.Free;
      DistVec.Free;
      For i:=1 to 3*MaxDetNum do Darray[i].Free;
End;
Procedure AnlyPlot.Disable;
begin
  removeStorage;
  AnalysisType:=Disabled;
  GeneralType:=None;
  PlotTitle.Remove;
  Freeze:=False;
  PlotTitle.setObjColor(Magenta);
  PlotTitle.SetStr('Disabled');
  If PlotAxes.IsVisible then
    begin
    PlotAxes.Remove;
    PlotAxes.IsVisible:=False;
    end;
  ClearScreen;
  AnalysisCounter:=1;
  DvecCompress:=1;
  DvecCompressCounter:=1;
  TimeCompress:=1;
  xCompress:=1;
  detectorclicked:=nil;
  sourceclicked:=nil;
end;

Procedure  AnlyPlot.Archive(Var filehandle: text);
Begin;
           WriteLn(filehandle, Ord(AnalysisType));
End;

Procedure  AnlyPlot.UnArchive(Var filehandle: text);
{These types are defined in Global-
 (Disabled,Index_Gain, YvsTDensity, YvsT3D, FFTpos,FFTtime, YvsT, EvsT, PvsT, EnergyDist );}
Var  aType:     Integer;
Begin;
  ReadLn(filehandle, aType);
  Case aType of
    0:AnalysisType:=Disabled;
    1:AnalysisType:=Index_Gain;
    2:AnalysisType:=yVsTDensity;
    3:AnalysisType:=YvsT3D;
    4:AnalysisType:=FFTpos;
    5:AnalysisType:=FFTtime;
    6:AnalysisType:=YvsT;
    7:AnalysisType:=EvsT;
    8:AnalysisType:=PvsT;
    9:AnalysisType:=EnergyDist;
  end;
End;

Procedure AnlyPlot.ResetAndInit;
Var numOfSteps:         Integer;
Begin
 if animationSpeed<=0 Then numOfSteps:=1-AnimationSpeed Else numOfSteps:=1;
 Case AnalysisType of
   Disabled:;
   Index_Gain:
       begin;   {Index of Refraction}
       PlotTitle.Remove;
       Case EquationType of
                 Schroedinger:PlotTitle.SetStr('Potential');
                 Diffusion: PlotTitle.SetStr('Diffusivity');
                 else PlotTitle.SetStr('Index of Refraction');
       end; {of Case}
       PlotTitle.setObjColor(Magenta);
       if EquationType=Schroedinger
                 Then RefreshAnalysisPlot(True,StartEdge,-1.0e5,StopEdge,1.0e5)
                 Else RefreshAnalysisPlot(True,StartEdge,-10,StopEdge,10);
                      AnalysisSelection(@TheWave);
       end;
   EvsT:
       begin; {<Y*Y> vs Time}
       PlotTitle.Remove;
       PlotTitle.SetStr('Space Integrated I(t)=Y*Y');
       PlotTitle.setObjColor(Magenta);
       AnalysisSelection(@TheWave);
       RefreshAnalysisPlot(True,Time,-1,time+20*numOfSteps*Dt,1);
       end;
   YvsTDensity:
       begin; {Wave-Time Contour}
       PlotTitle.Remove;
       PlotTitle.SetStr('Contour of y(x,t)');
       PlotTitle.setObjColor(Magenta);
       AnalysisSelection(@TheWave);
       RefreshAnalysisPlot(True,startEdge,Time,stopEdge,Time+1);
       end;
   YvsT3D:
       begin; {Wave Time- 3D}
       PlotTitle.Remove;
       PlotTitle.SetStr('3D of y(x,t)');
       PlotTitle.setObjColor(Magenta);
       With PlotAxes do
         If IsVisible then
               begin
               Remove;
               IsVisible:=False;
               end;
       ClearScreen;
       AnalysisSelection(@TheWave);
       end;
   FFTpos:
       begin;{FFT - y(x)}
       If (LeftB=Fixed)And(RightB=Fixed)
          Then FFTBasis:=SinOnly
          Else FFTBasis:=SinCos;
       PlotTitle.Remove;
       PlotTitle.SetStr('Fourier Transform of Y(x)');
       PlotTitle.setObjColor(Magenta);
       AnalysisSelection(@TheWave);
       end;
   FFTtime:
       if detectorList.nodes=nil
       Then
         announce('Please create a DETECTOR before you select this analysis type.')
       else
         begin {FFT - y(t)}
           FFTBasis:=SinCos;
           PlotTitle.Remove;
           PlotTitle.SetStr('Fourier Transform of Y(t)');
           PlotTitle.setObjColor(Magenta);
           PlotTitle.Display;
           AnalysisSelection(@TheWave);
           FourierAnalysis(True,True,True)
           end;
   YvsT:
     if detectorList.nodes=nil
     Then
       announce('Please create a DETECTOR before you select this analysis type.')
     else
     begin  {Detectors}
       PlotTitle.Remove;
       if EquationType=Schroedinger
         then  PlotTitle.SetStr('Detector Current vs Time')
         else PlotTitle.SetStr('Detectors:Amplitude');
       PlotTitle.setObjColor(Magenta);
       AnalysisType:=YvsT;
       AnalysisSelection(@TheWave);
       RefreshAnalysisPlot(True,(Time),-1,(time+20*numOfSteps*Dt),1);
       end;
     PvsT:
       if sourceList.nodes=nil
       Then
         announce('Please create a SOURCE before you select this analysis type.')
       else
         begin  {Source Power}
         PlotTitle.Remove;
         PlotTitle.SetStr('P(t)- Source Power');
         PlotTitle.setObjColor(Magenta);
         PlotTitle.Display;
         AnalysisSelection(@TheWave);
         RefreshAnalysisPlot(True,(Time),-1,(Time+20*numOfSteps*Dt),1);
         end;
     EnergyDist:
         begin { Y*Y Distribution }
         ShowEnergyType:=Total;
         PlotTitle.Remove;
         PlotTitle.SetStr(' I(x,t)=Y(x,t)*Y(x,t) ');
         PlotTitle.setObjColor(Magenta);
         RefreshAnalysisPlot(True,StartEdge,0,StopEdge,1);
         AnalysisSelection(@TheWave);
         end;
   End; {of Case}
   AnalysisCounter:=1;
   DvecCompress:=1;
   DvecCompressCounter:=1;
   skipcounter:=1;
   timeCompress:=1;
End;


Procedure AnlyPlot.MaxMin(Var MinX, MaxX, MinY, MaxY: Real);
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
Procedure DetectorMaxMin(Var Min,Max: Real);

Var
   tempMin,tempMax:     rEAL;
   num:                 Integer;
Begin;
  num:=1;
    Case disType of
    ShowLeft:  limitedminMax(Min,Max,Darray[Num+MaxDetNum],1,AnalysisCounter);
    showRight: limitedMinMax(Min,Max,Darray[Num],1,AnalysisCounter);
    showTotal: limitedMinMax(Min,Max,Darray[Num+2*MaxDetNum],1,AnalysisCounter);
    End;
  FOR num:=2 TO DetectNum do
  begin
  Case disType of
    ShowLeft:  limitedminMax(tempMin,tempMax,Darray[Num+MaxDetNum],1,AnalysisCounter);
    showRight: limitedMinMax(tempMin,tempMax,Darray[Num],1,AnalysisCounter);
    showTotal: limitedMinMax(tempMin,tempMax,Darray[Num+2*MaxDetNum],1,AnalysisCounter);
    End;
  if tempMin<min then min:=tempMin;
  if tempMax>max Then max:=tempMax;
  end;
End;


Begin;
      Case AnalysisType of
           YvsT:
                begin
                     limitedMinMax(MinX,MaxX,tVec,1,AnalysisCounter);
                     DetectorMaxMin(MinY,MaxY);
                end;
           EvsT:
                begin
                     limitedMinMax(MinX,MaxX,tVec,1,AnalysisCounter);
                     Case ShowEnergyType of
                          Total:     limitedMinMax(MinY,Maxy,EVec,1,AnalysisCounter);
                          Kinetic:   limitedMinMax(MinY,Maxy,KEVec,1,AnalysisCounter);
                          Potential: limitedMinMax(MinY,Maxy,PEVec,1,AnalysisCounter);
                     End; {of Case}
                end;
           PvsT:
                begin
                     limitedMinMax(MinX,MaxX,TVec,1,AnalysisCounter);
                     limitedMinMax(MinY,MaxY,PowerVec,1,AnalysisCounter);
                end;
           Index_Gain:
                begin
                     limitedMinMax(MinX,MaxX,xVec,2,xVec.Length-1);
                     limitedMinMax(MinY,MaxY,indexOGVec,2,xVec.Length-1);
                end;
           EnergyDist:
                begin
                     limitedMinMax(MinX,MaxX,xVec,2,xVec.Length-1);
                     limitedMinMax(MinY,maxY,distVec,2,xVec.Length-1);
                end;
      End;
End;

Procedure AnlyPlot.CompactDVec( vec: Dvector );

Var
   index,
   i,j:           Integer;
   sum:           Real;

Begin;
      For i:=1 to alength Div 2 do           {PROBLEM}
          Vec.Put(i,Vec.Value(2*i-1));
      For i:=(alength Div 2 + 1) to alength do Vec.Put(i,0);
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
   Ok, AsXY,
   AsX,AsY,Top:                 Boolean;
   Old,Temp:                    ScaleRect;
   i:                           Integer;
Begin;
      If generalType=none Then Exit;
      If (GeneralType=ThreeD) then
         begin;
               Announce('Scale cannot be changed for Contour or 3D plots.');
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
                 LoadLine('      Display Scale?  #T   ');
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
                            SetBoolean(1, PlotAxes.IsVisible );
                            SetBoolean(6, Expanded );
                            SetNumber(2, xMin );
                            SetNumber(3, xMax );
                            SetNumber(4, yMin );
                            SetNumber(5, yMax );
                      end;
                 SetHelpFile('Wave.Hlp', 'Scaling Help');
                 AcceptScreen;
                 if GetBoolean(11) then begin done; exit; end;
                 With Temp do
                      begin;
                            xMin:=GetNumber(2);
                            xMax:=GetNumber(3);
                            yMin:=GetNumber(4);
                            yMax:=GetNumber(5);
                      end;
                 PlotAxes.IsVisible:=GetBoolean(1);
                 AsX:=GetBoolean(7);
                 AsXY:=GetBoolean(8);
                 AsY:=GetBoolean(9);
                 If not(Expanded=GetBoolean(6)) then Expand;
                 Expanded:=GetBoolean(6);
                 Ok:=GetBoolean(10);
                 Done;
           end;

           If (Temp.xMin>=Temp.xMax) or (Temp.yMin>=Temp.yMax) then
              begin
                   Announce('Minimum vales must be less than Maximum value.');
                   Exit;
              end;

           If (AnalysisType=FFTpos)or(AnalysisType=FFTtime) then
              begin;
                    Plot.Remove;
                    isVisible:=True;
                    If Ok then
                       begin
                       SetScale(temp.xMin,temp.yMin, temp.xMax, temp.yMax );
                       Plot.Display;
                       FourierAnalysis(False,True,True);
                       end;
                    If AsXY or AsX or AsY then
                       begin;
                             Plot.Display;
                             FourierAnalysis(True,True,True);
                       end;
                    if AsX Then
                      Begin
                      plot.remove;
                      isVisible:=True;
                      SetScale(scale.xMin, temp.yMin, scale.xMax, temp.yMax );
                      Plot.Display;
                      FourierAnalysis(False,True,True);
                      End;
                    if AsY Then
                      Begin
                      plot.remove;
                      isVisible:=True;
                      SetScale(temp.xMin, scale.yMin, temp.xMax, scale.yMax );
                      Plot.Display;
                      FourierAnalysis(False,True,True);
                      End;
                    Exit;
              end;


           If Ok then
                 begin;
                       Self.Remove;
                       With Temp do
                            SetScale( xMin, yMin, xMax, yMax );
                       isVisible:=True;
                       Display;
                       Scale:=Temp;
                 end;

           If AsXY or AsX or AsY then
              begin;
                    Remove;
                    Old:=Temp;
                    With Temp do
                         begin;
                               MaxMin(xMin,xMax,yMin,yMax);
                               If (yMin=yMax) or (xMin=xMax) then
                                  begin;
                                        isVisible:=True;
                                        Display;
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

                               If AsXy then SetScale( xMin, yMin, xMax, yMax );
                               If AsX then SetScale(xMin, Old.yMin, xMax, Old.ymax);
                               If AsY then SetScale(Old.xMin, yMin, Old.xMax, yMax);
                         end;
                    isVisible:=True;
                    Display;
              end;
End;


Procedure AnlyPlot.Display;
Begin;
      If NOT(IsVisible) Then Exit;
      Plot.Display;   {<-Plots the viewport, axes, and buttons}
      Replot;
End;

Procedure AnlyPlot.RePlot;
{plots just the data INSIDE the analysis plot.  The scale, buttons, etc are not drawn.}
Var
   OldColour:   Word;
   i:Integer;


Begin;
      If (AnalysisType=Disabled) or Not(isVisible) then exit;

      OldColour:=GetColor;
      hideCursor;
      Case AnalysisType of
           FFTpos,FFTTime:   FourierAnalysis(False,True,True);
           Index_Gain: IndexPlot;
           YvsT3D:
                  begin;
                        Remove;
                        GraphBackColor:=Black;
                        isVisible:=TRUE;
                        Plot.Display;
                        GraphBackColor:=DarkGray;
                        TheThreeD.Init;
                        TheThreeD.SetDistance(1.5,2.5);
                        TheThreeD.SetBoxRatios(1,4/5,2/3);
                        TheThreeD.SetViewAngles( EuAngles[1],EuAngles[2],EuAngles[3]);
                        If TimeCompress = 1
                              Then Comment(Xo+Width/2-0.10,0.8,'Matrix empty.')
                              Else
                              Begin
                                    If BoxBool then TheThreeD.DrawBox;
                                    TheThreeD.DrawSurface(OldMatrix);
                              End;
                        ButtonList.Display;
                  end;
           YvsTDensity:
                       begin;
                            TheContour.Init;
                            TheContour.SetPaletteNum(PalNumber);
                            If TimeCompress = 1
                              Then Comment(Xo+Width/2-0.10,0.8,'Matrix empty.')
                              Else TheContour.DrawMatrix(OldMatrix);
                            If LegBool then TheContour.PrintLegend(Decimal);
                            ButtonList.Display;
                       end;
           YvsT:
                begin
                     ClearScreen;
                     DetectorList.RePlotDetector(@Self,Tvec,Distype,AnalysisCounter);
                end;
           EvsT:
                begin
                     SetColor(Green);
                     Case ShowEnergyType of
                          Total:     PlotDvectors(Tvec,Evec,1,AnalysisCounter);
                          Kinetic:   PlotDvectors(Tvec,KEvec,1,AnalysisCounter);
                          Potential: PlotDvectors(Tvec,PEvec,1,AnalysisCounter);
                     End; {of Case}
                end;
           PvsT:
                begin
                     Setcolor(SourceClicked^.ParSource.Num);
                     PlotDvectors(Tvec,PowerVec,1,AnalysisCounter);
                end;
           EnergyDist: DensityPlot;
      End; {of Case}

      IF isVisible AND Expanded And Not(Freeze) THEN
         BEGIN
              Plot.LockFocus;
              SetColor(White);
              ShowDensity;
         END;
      SetColor(OldColour);
      showCursor;
End;


Procedure AnlyPlot.UpdatePlot;
{Registers the data in the DVectors and draws the new data.}
Var
   OldColour:           Word;
   i:                   Integer;

Procedure Collect_Plot_Compact_Matrix;

Var
   i,j,Index:           Integer;
   Trash:               Word;
   Sum:                 Real;
Begin;
      If (AnalysisCounter=2) And isVisible And ShowWaitMsg then
         begin;
               Comment(Xo+Width/2-0.15,0.8,'Data being Accumulated.');
               ShowWaitMsg:=False;
         end;

      If (AnalysisCounter<=MatSize) then
         begin
              TransformToSpaceGrid(displayVec,TempSGVec);

              For i:=1 to Matsize do
                  begin
                       Sum:=0;
                       For j:=(xCompress*(i-1)) to (xCompress*(i)-1) do
                           begin
                                If (j+1)>vacuumPts then index:=vacuumPts else index:=j+1;
                                Sum:=Sum + tempSGVec.Value(index);
                           end;
                       TheMatrix.Put(MatSize - AnalysisCounter+1, i ,Sum/xCompress +
                                     TheMatrix.Value(MatSize - AnalysisCounter+1, i));
                  end;

              If (SkipCounter > 1)then Dec(SkipCounter) else
                 begin
                      Inc(analysisCounter);
                      SkipCounter:=TimeCompress;  {reset the skipCounter}
                 end;
         end; {of If}


      If (AnalysisCounter=MatSize+1) then
         begin;
               TheMatrix.MultScalar(1/TimeCompress);

               Case AnalysisType of
               YvsT3D:
                      begin;
                            GraphBackColor:=Black;
                            Plot.Display;
                            GraphBackColor:=DarkGray;
                            TheThreeD.Init;
                            TheThreeD.SetDistance(1.5,2.5);
                            TheThreeD.SetBoxRatios(1,4/5,2/3);
                            If isVisible then
                               begin;
                                     TheThreeD.SetViewAngles(EuAngles[1],EuAngles[2],EuAngles[3]);
                                     If BoxBool then TheThreeD.DrawBox;
                                     TheThreeD.DrawSurface(TheMatrix);
                                     ButtonList.Display;
                               end;
                      end;
               YvsTDensity:
                           begin;
                                 TheContour.Init;
                                 If isVisible then
                                    begin;
                                          PlotAxes.Remove;
                                          TheContour.SetPaletteNum(PalNumber);
                                          With Scale do
                                               begin;
                                                   xMin:=startEdge;
                                                   xMax:=StopEdge;
                                                   yMin:=InitialTime;
                                                   yMax:=Time;
                                                   SetScale(xMin,yMin,xMax,yMax)
                                              end;
                                          TheContour.DrawMatrix(TheMatrix);
                                          If LegBool then TheContour.PrintLegend(Decimal);
                                          PlotAxes.Display;
                                          ButtonList.Display;
                                    end;
                           end;
               End;
               OldMatrix.Equate(TheMatrix);
               DMatTimeCompress(TheMatrix);
         end;
End;

Procedure Collect_Plot_YvsT;

Var
   Ok:  Boolean;

Begin
     If (DvecCompressCounter>1) then Dec(DvecCompressCounter)else
        begin
             Inc(AnalysisCounter);
             DetectorList.DetectorUpdate(@Self);
             Tvec.Put(AnalysisCounter,Time);
             if isVisible then DetectorList.DetectorPlot(@Self,Distype);
             If (DvecCompress>1) then DvecCompressCounter:=DvecCompress;
        end;
End;

Procedure Collect_Plot_FFT;

Var
   Ok:          Boolean;
   RightWave,Magnitude,
   LeftWave:    Real;

Begin
     If (AnalysisType=FFTpos) then FourierAnalysis(False,False,True)
     else
         begin
              If (AnalysisCounter=1) And isVisible And ShowWaitMsg then
                 begin
                 lockFocus;
                 openViewPort(1);  {clear the viwport to background color}
                 buttonlist.display;
                 Comment(Xo+Width/2-0.15,0.8,'Data being Accumulated.');
                 ShowWaitMsg:=False;
                 end;
              If AnalysisCounter>FFTsize then exit;
              If (AnalysisCounter<=FFTsize) then
                begin
                     DetectorClicked^.GetDetectorValues(RightWave,LeftWave,Magnitude);
                     Rwave.VecPtr^[AnalysisCounter]:=RightWave;
                     Lwave.VecPtr^[AnalysisCounter]:=LeftWave;
                     Inc(AnalysisCounter);
                end;
              If (AnalysisCounter=FFTsize+1) Then
                begin ShowWaitMsg:=False; FourierAnalysis(False,True,True);AnalysisCounter:=1;end;
         end;
End;

Procedure Collect_Plot_Erelated;

Begin
        begin
             Case AnalysisType of
                  EvsT:
                       begin
                            If (DvecCompressCounter>1) then
                               Dec(DvecCompressCounter)
                            else
                                begin
                                     Inc(AnalysisCounter);
                                     Evec.Put(AnalysisCounter, Energy);
                                     KEvec.Put(AnalysisCounter, KineticEnergy);
                                     PEvec.Put(AnalysisCounter, PotentialEnergy);
                                     Tvec.Put(AnalysisCounter, Time);
                                     SetColor(Green);
                                     if isVisible then
                                     Case ShowEnergyType of
                                          Total:     PlotDvectors(Tvec, Evec, AnalysisCounter-1, AnalysisCounter);
                                          Kinetic:   PlotDvectors(Tvec, KEvec, AnalysisCounter-1, AnalysisCounter);
                                          Potential: PlotDvectors(Tvec, PEvec, AnalysisCounter-1, AnalysisCounter);
                                     End; {of Case}
                                If (DvecCompress>1) then DvecCompressCounter:=DvecCompress;
                                end;
                       end;
                  EnergyDist: DensityPlot;
             End; { of Case }
      end;
End;


Procedure Collect_Plot_VersusTime;

Begin
     If (DvecCompressCounter>1) then Dec(DvecCompressCounter)else
        begin
             Case AnalysisType of
                  PvsT:
                       begin
                            Inc(AnalysisCounter);
                            PowerVec.Put(AnalysisCounter, SourceClicked^.ParSource.Power);
                            Tvec.Put(AnalysisCounter,Time);
                            SetColor(SourceClicked^.ParSource.Num);
                            if isVisible then
                              PlotDvectors(Tvec, PowerVec ,AnalysisCounter-1, AnalysisCounter);
                       end;
                  End; { of Case }

                  If (DvecCompress>1) then DvecCompressCounter:=DvecCompress;
             end;
End;

Begin;
      If (AnalysisType=Disabled) or (Freeze)then Exit;

      If (AnalysisCounter>=Alength) then CompressAnalysis;

      LockFocus;
      OldColour:=GetColor;

      Case GeneralType of
           FastFT:         Collect_Plot_FFT;
           ThreeD:         Collect_Plot_Compact_Matrix;
           Dynamic:        Collect_Plot_YvsT;
           EnergyRelated:  Collect_Plot_Erelated;
           VersusTime:     Collect_Plot_VersusTime;
           VersusPosition: IndexPlot;
      End; {of Case}

      If isVisible AND Expanded And Not(Freeze) then
         begin;
               Plot.LockFocus;
               SetColor(White);
               ShowDensity;
         end;
      SetColor(OldColour);
End;

Procedure AnlyPlot.DensityPlot;

Var
    i,Junk:       Integer;
    OldColour:    Word;
    MinY,MaxY:    Real;
    ke,pe:        Real;
    dx:           Real;

Begin
     if Not(isVisible) then Exit;
     If Not(Freeze) then
     begin
     OldColour:=GetColor;
     Plot.LockFocus;
     If (DistVec.Length <> Numpts) then
        begin
             DistVec.Free;
             DistVec.Init(Numpts);
             distVec.Fill((Scale.yMax+Scale.Ymin)/2)
        end;
     if time=0 then distVec.Fill((Scale.yMax+Scale.Ymin)/2);
     SetColor(GraphBackColor);
     PlotDvectors(xVec,DistVec,2,Numpts-1);
     i:=2;
     ke:=0;  pe:=0;
     dx:=(StopEdge-StartEdge)/(VacuumPts-1);
     While i<= Numpts -1 Do
     Begin
     GridPointDensities(i,ke,pe);
     Case ShowEnergyType of
                   Total:       DistVec.Put(i,(ke+pe));
                   Kinetic:     DistVec.Put(i,(ke));
                   Potential:   DistVec.Put(i,(pe));
              End; {of Case}
     i:=i+1;
     End;{of Do loop}
     end;
     SetColor(Green);
     PlotDvectors(xVec,DistVec,4,NumPts-3);
     SetColor(OldColour);
End;

Procedure AnlyPlot.IndexPlot;

Var dx:           Real;
    i:            Integer;
    OldColour:    Word;

Begin
     if Not(isVisible) then Exit;
     OldColour:=GetColor;
     Plot.LockFocus;
     if plotAxes.Isvisible Then plotAxes.Display ;
     SetColor(Red);
     PlotDvectors(xVec,IndexOGVec,hiddenPts+1,NumPts-hiddenPts-1);
     {SetColor(LightRed);
     PlotDvectors(xVec,GainOGVec,hiddenPts+1,NumPts-hiddenPts-1); }
     SetColor(OldColour);
End;

Procedure AnlyPlot.IntializeAnalysisPlots;

Var
   i:   Integer;
   Ok:  Boolean;

Begin;
      Case AnalysisType of
           YvsT:
                begin
                     DetectorList.DetectorUpdate(@Self);
                     Tvec.Put(1,Time);
                end;
           EvsT:
                begin
                     Evec.Put(1,Energy);
                     KEvec.Put(1,KineticEnergy);
                     PEvec.Put(1,PotentialEnergy);
                     Tvec.Put(1,Time);
                end;
           PvsT:
                begin
                     Tvec.Put(AnalysisCounter,Time);
                     PowerVec.Put(1, SourceClicked^.ParSource.Power);
                end;
           End; {of Case}
End;

Procedure AnlyPlot.RefreshAnalysisPlot(Clear: Boolean; Ix, Iy, Fx, Fy: Real);

Begin;
      If AnalysisType=Disabled then Exit;
      Remove;
      PlotAxes.Remove;
      isVisible:=True;
      plotAxes.isVisible:=True;
      SetScale( Ix, Iy, Fx, Fy );
      Plot.Display;
      PlotAxes.Display;
      If not(Clear) then Replot;
End;

Procedure AnlyPlot.AnalysisSelection(thePlot:plotPtr);

Var
   OldColour:           Word;
   i:                   Integer;
   AnalyScrn:           TInputScreen;
   Ok,Cancel,
   Leave:               Boolean;
   LimitStr,
   NumStr:              String;
   Buttons:             TButtons;

Procedure Fourier_y_x;
  VAR
     msg:      String;
  Begin
        FFTSize:=1;
        While (2*FFTSize<=(VacuumPts)) DO FFTSize:=FFTSize*2;
        Str(FFTSize:4,msg);
        msg:=Concat('Fast Fourier Analysis must use  2^n points.  Analysis will use: ',msg);
        If fftSize <> VacuumPts Then Announce(msg);
        If EquationType=Schroedinger Then FFTSize:=FFTSize*2;
  End;

Procedure SelectIt(TheType: BasisType; TheList: List; Var TheStorage: BasePtr);

  Var
     Leave:               Boolean;
     LimitStr,
     NumStr:              String;
     Ok,Clear,Cancel:             TButton;
     ItemClicked:         BasePtr;

  Begin
      if theList. NumberOfItems=1 then begin  theStorage:=theList.nodes^.item; Exit; end;
      Case theType of
        det:Message('..............Click on the detector that you wish to analyze................');
        sou:Message('..............Click on the source that you wish to analyze................');
        else Message('..............Click on the item that you wish to analyze................');
      end; {of Case}
      {Added by WC}
      Leave:=FALSE;
        Begin
        { Remove; }
        DefineViewPort( 4, Xo, Xo + Width, Yo, Yo + Height );
        OpenViewPort(4);
{        With Buttons do
           begin;
                 Init;
                 Create(11,Xo+0.05,Yo+0.05,'Clear');
                 Create(22,Xo+0.05,Yo+0.10,'Edit');
                 Create(33,Xo+0.25,Yo+0.05,'Accept');
                 Create(44,Xo+0.25,Yo+0.10,'Cancel');
                 Number:=22;
                 Buttons.DrawAll;
           end; }
           Clear .Create(Xo+0.05,Yo+0.05,'Clear','Del');
           Ok    .Create(Xo+0.25,Yo+0.05,'Ok','Return');
           Cancel.Create(Xo+0.25,Yo+0.10,'Cancel','Esc');

        End;

        ItemClicked:=TheStorage;

        Rubout(2,2,20,LightBlue);
        If ItemClicked=nil
          Then Case theType of
            det:Print(2,2,'Detector # ');
            sou:Print(2,2,'Source   # ');
           else Print(2,2,' Item    # ');
            end{of Case}
          Else
            Begin
                 Case TheType of
                      Det: Str(ItemClicked^.ParDetector.Num:3, NumStr);
                      Sou: Str(ItemClicked^.ParSource.Num:3, NumStr);
                      else numstr:='n/a';
                 End; {of Case}
                 Case theType of
                   det:Print(2,2,'Detector # '+NumStr);
                   sou:Print(2,2,' Source  # '+NumStr);
                  else Print(2,2,' Item    # '+NumStr);
                 end;{ofCase}
            End;
      Print(2,1,'Click on an item to set #.');
      Repeat
                  CheckforEvents;
{                  If Event.Keypressed THEN
                    Begin
                    If Event.Readkey=ESC THEN Leave:=TRUE Else beep;
                    End;}
                 SelectViewPort(4);
                    if Clear.Clicked then
                            Begin
                                 Rubout(2,2,20,LightBlue);
                                 Case theType of
                                   det:Print(2,2,'Detector # ');
                                   sou:Print(2,2,'Source   # ');
                                  else Print(2,2,'item     # ');
                                 end; {of case}
                                 Print(2,1,'Click on an item to set #.');
                                 ItemClicked:=Nil;
                                 Buttons.Number:=22;
                                 Buttons.DrawAll;
                            End;
                    if Ok.Clicked then
                            begin
                                 Leave:=True;
                                 If ItemClicked=nil Then
                                   Begin AnalysisType:=Disabled;
                                         PlotTitle.Remove;
                                         PlotTitle.SetStr('Disabled');
                                         PlotTitle.Display;
                                   End
                                 else TheStorage:=ItemClicked;
                            end;  {normal exit}
                    if Cancel.Clicked then
                            begin
                                 ItemClicked:=nil;
                                 AnalysisType:=Disabled;
                                 PlotTitle.Remove;
                                 PlotTitle.SetStr('Disabled');
                                 PlotTitle.Display;
                                 Leave:=True; end; {Canceled.  No Item selected}
                    if Event.MouseClicked then{look for clicked item}
                    Begin
                         TheList.ReturnClickedItem( ItemClicked );
                         If ItemClicked<>Nil THEN
                            Begin
                                 Case TheType of
                                      Det: Str(ItemClicked^.ParDetector.Num:3, NumStr);
                                      Sou: Str(ItemClicked^.ParSource.Num:3, NumStr);
                                 End; {of Case}
                                 SelectViewPort(4);
                                 Rubout(2,2,20,LightBlue);
                                 Case theType of
                                   det:Print(2,2,'Detector # '+NumStr);
                                   sou:Print(2,2,' Source  # '+NumStr);
                                   else Print(2,2,' Item    # '+NumStr);
                                 end;{ofCase}
                                 Print(2,1,'Click on an item to set #.');
                            end;
                    End;
      Until Leave;
      CloseViewport(4);
{      Buttons.Done; }
      Plot.Display;
      Message('');
  End;

Procedure Fourier_y_t;

Begin
     SelectIt(Det,DetectorList,DetectorClicked);
     If AnalysisType=Disabled then exit;
     FFTsize:=256;
     If EquationType=Schroedinger Then FFTSize:=FFTSize*2;
End;


Procedure ThreeDPlots;

Begin;
      MatSize:=(VacuumPts);

      DMatPositionCompress(MatSize);

      TheMatrix.Free;
      OldMatrix.Free;
      HaltIfError:=FALSE;
      TheMatrix.Init(MatSize, MatSize);
      If NOT ErrorFound
        Then TheMatrix.Fill(0);

      OldMatrix.Init(MatSize,MatSize);
      If ErrorFound
        Then
         Begin
         Disable;
         Announce('Not enough memory for this analysis.');
         End
        Else OldMatrix.Fill(0);
      HaltIfError:=TRUE;
      ErrorFound:=FALSE;
      InitialTime:=Time;
End;

Begin;
  RemoveStorage;
  TimeCompress:=1;
  DvecCompress:=1;
  PlotTitle.setObjColor(Magenta);
  Freeze:=False;
  SetGeneralType;

  Case AnalysisType of
      YvsT:                     begin
                                ShowWaitMsg:=True;
                                DisType:=showTotal;
                                end;
      YvsT3D,
      YvsTDensity:              ThreeDPlots;
      FFTpos:                   Fourier_y_x;
      FFTTime:                  begin
                                  Fourier_y_t;
                                  ShowWaitMsg:=True;
                                  disType:=showTotal;
                                end;
      PvsT:                     SelectIt(Sou,SourceList,SourceClicked);
      Index_Gain:               IndexPlot;
      End; {of Case}
  CreateStorage;
  PlotAxes.IsVisible:=True;
  If AnalysisType=Disabled then
     begin
          PlotAxes.IsVisible:=False;
          PlotAxes.Remove;
          Plot.Display;
     end;


  Case AnalysisType of
       FFTpos,FFTtime:
           begin
                ClearScreen;
                FourierAnalysis(True,True,True);
           end;
       EnergyDist:DensityPlot;
  End; {of Case}
End;

Procedure AnlyPlot.RemoveStorage;

Var
   i:   Integer;

Begin;
  DistVec.Free;
  Evec.Free;
  KEvec.Free;
  PEvec.Free;
  Tvec.Free;
  Rwave.Free;
  Lwave.Free;
  FFTvec.Free;
  FFTx.Free;
  MaxHistVec.Free;
  For i:=1 to 3*MaxDetNum do Darray[i].Free;
  Tvec.Free;
  TheMatrix.Free;
  OldMatrix.Free;
  PowerVec.Free;
End;

Procedure AnlyPlot.CreateStorage;

Var
   i:   Integer;
   EnoughMemory: Boolean;

Begin
HaltIfError:=False;
EnoughMemory:=True;
     Case GeneralType of
      EnergyRelated:
                 begin
                      Case AnalysisType of
                           EnergyDist:
                             begin
                             DistVec.Init(NumPts);
                             If ErrorFound
                               Then EnoughMemory:=FALSE
                               Else DistVec.Fill(0);
                               End;
                           EvsT:
                                begin
                                Evec.Init(Alength);
                                If ErrorFound Then EnoughMemory:=False;
                                KEvec.Init(Alength);
                                If ErrorFound Then EnoughMemory:=False;
                                PEvec.Init(Alength);
                                If ErrorFound Then EnoughMemory:=False;
                                Tvec.Init(Alength);
                                If ErrorFound
                                  Then EnoughMemory:=False
                                  Else Tvec.Put(1,Time);
                                end;
                      End; {of Case}
                 end;
          Dynamic:
                  begin
                       For i:=1 to 3*MaxDetNum do
                           begin
                           Darray[i].Init(alength);
                           If ErrorFound Then EnoughMemory:=False;
                           end;
                       Tvec.Init(Alength);
                       If ErrorFound
                         Then EnoughMemory:=False
                         Else Tvec.Put(1,Time);
                  end;
          FastFT:
                 begin
                      Rwave.Init(FFTsize);
                      If ErrorFound Then EnoughMemory:=False;
                      Lwave.Init(FFTsize);
                      If ErrorFound Then EnoughMemory:=False;
                      If MaxHistVec.Length<>FFTSize Then
                         begin
                         MaxHistVec.Free;
                         MaxHistVec.Init(FFTSize);
                         If ErrorFound Then EnoughMemory:=False;
                         end;
                       If FFTx.Length<>FFTSize Then
                         begin
                         FFTx.Free;
                         FFTx.Init(FFTSize);
                         If ErrorFound Then EnoughMemory:=False;
                         end;
                      If FFTVec.Length<>FFTSize Then
                         begin
                         FFTVec.Free;
                         FFTVec.Init(FFTSize);
                         If ErrorFound Then EnoughMemory:=False;
                         end;
                 end;
          VersusTime:
                 begin
                      Case AnalysisType of
                           PvsT:
                                begin
                                PowerVec.Init(Alength);
                                If ErrorFound Then EnoughMemory:=False;
                                Tvec.Init(Alength);
                                If ErrorFound
                                  Then EnoughMemory:=False
                                  Else Tvec.Put(1,Time);
                                end;
                           End; {of Case}
                 end;
     End; {of Case }
If Not EnoughMemory Then
  Begin
  Announce('Not Enough memory.');
  Disable;
  ErrorFound:=FALSE;
  End;
HaltIfError:=True;
End;

Procedure AnlyPlot.CompressAnalysis;

Var
   i:   Integer;

Begin;
      If Freeze then exit;

      DvecCompress:=DvecCompress*2;
      DvecCompressCounter:=DvecCompress;

      Case GeneralType of
           Dynamic:
                   begin;
                         For i:=1 to 3*MaxDetNum do CompactDvec(Darray[i]);
                         CompactDvec(Tvec);
                         AnalysisCounter:=AnalysisCounter Div 2;
                   end;
           EnergyRelated:
                      begin
                           Case AnalysisType of
                                EvsT:
                                     begin
                                          CompactDvec(Tvec);
                                          CompactDvec(Evec);
                                          CompactDvec(KEvec);
                                          CompactDvec(PEvec);
                                          AnalysisCounter:=AnalysisCounter Div 2;
                                     end;
                                EnergyDist: ;
                           End; {of Case}
                      end;
           VersusTime:
                      begin
                           Case AnalysisType of
                                PvsT:
                                     begin
                                          CompactDvec(PowerVec);
                                          CompactDvec(Tvec);
                                     end;
                                End; {of Case}
                           AnalysisCounter:=AnalysisCounter Div 2;
                      end;
           None: ;
      End; {of Case}
End;

Procedure AnlyPlot.SetGeneralType;

Begin;
    Case AnalysisType of
         Disabled:        GeneralType:=None;
         FFTpos,FFTtime:  GeneralType:=FastFT;
         YvsTDensity,
         YvsT3D:          GeneralType:=ThreeD;
         YvsT:            GeneralType:=Dynamic;
         EnergyDist,
         EvsT:            GeneralType:=EnergyRelated;
         PvsT:            GeneralType:=VersusTime;
         Index_Gain:      GeneralType:=VersusPosition;
    End;
End;

Procedure AnlyPlot.ShowSpaceAverage;
Var
  AveScrn: TInputScreen;
  OK,Cancel:Boolean;
  msg1,msg2:                         String;
  startIntegrate,stopIntegrate:      Real;
  StartIndex,StopIndex:              Integer;
  ke,pe,ave:                               Real;
  i:                                 Integer;

Begin
StartIntegrate:=startEdge;
StopIntegrate:=stopEdge;
With AveScrn Do
  Repeat
  init;
  DefineInputPort( 0.20, 0.80, 0.35 ,0.95 );
  LoadLine('Space Average of Distribution Function.');
  LoadLine('---------------Last Calculation---------------');
  str(StartIntegrate:8:4,msg1);
  str(StopIntegrate:8:4,msg2);
  msg1:=Concat('Start Integrate= ',msg1);
  msg2:=Concat(' Stop Integrate= ',msg2);
  LoadLine(msg1);
  LoadLine(msg2);
  StartIndex:=0;
  Repeat
    startIndex:=startIndex+1;
    Until (xVec.VecPtr^[startIndex]>=startIntegrate) or (startIndex>=Numpts-HiddenPts-1);
  stopIndex:=startIndex-1;
  Repeat
    stopIndex:=stopIndex+1;
    Until (xVec.VecPtr^[stopIndex]>=stopIntegrate) or (stopIndex>=Numpts-HiddenPts);

  str((StopIndex-Startindex):5,msg1);
  msg1:=Concat('There were ', msg1,' grid points on this interval.');
  LoadLine(msg1);
  {Trapezoidal Rule}
  GridPointDensities(startIndex,ke,pe);
  Case ShowEnergyType of
                   Total:  ave:=(ke+pe)/2;
                   Kinetic:ave:=ke/2;
                   Potential: ave:=pe/2;
              End; {of Case}
  For i:=StartIndex+1 To StopIndex-1 Do
    Begin
    GridPointDensities(i,ke,pe);
    Case ShowEnergyType of
                   Total:  ave:=ave+ke+pe;
                   Kinetic:ave:=ave+ke;
                   Potential: ave:=ave+pe;
              End; {of Case}
    end;
  GridPointDensities(StopIndex,ke,pe);
  Case ShowEnergyType of
                   Total:  ave:=ave+(ke+pe)/2;
                   Kinetic:ave:=ave+ke/2;
                   Potential: ave:=ave+pe/2;
              End; {of Case}
  LoadLine('');
  if ((StopIndex-StartIndex)>0)Then
      Begin
      ave:=ave*(StopEdge-StartEdge)*(StopIntegrate-StartIntegrate)/(StopIndex-StartIndex);
      str(ave:8:4,msg1);
      str((StopIndex-Startindex):4,msg2);
      msg1:=Concat('Space Integral= ',msg1, '.');
      LoadLine(msg1);
      End
      Else LoadLine('X Interval too small to do integral.');
  Loadline('---------------Next Calculation---------------');
  LoadLine('Start Integrate= {        }');
  LoadLine(' Stop Integrate= {        }');
  LoadLine('');
  LoadLine('OK will re-do average; Cancel will resume.');
  LoadLine('');
  LoadLine('   [  Ok  ]     [Cancel]      [ Help ]  ' );
  SetHelpFile('Wave.Hlp', 'Energy Average Help');
  SetNumber(1,startIntegrate);
  SetNumber(2,stopIntegrate);
  AcceptScreen;
  ok:=GetBoolean(3);
  cancel:=GetBoolean(4);
  if OK Then
      Begin
      startIntegrate:=GetNumber(1);
      stopIntegrate:=GetNumber(2);
      End;
  If StopIntegrate<StartIntegrate then StopIntegrate:=StartIntegrate;
  if StartIntegrate<startEdge then startIntegrate:=StartEdge;
  if stopIntegrate>StopEdge Then stopIntegrate:=StopEdge;
  done;
  Until Cancel;
End;


Procedure AnlyPlot.AverageQMCurrent;
Var
   i,ioOffset:                    Integer;
   AveScrn:                     TInputScreen;
   msg1,msg2,msg3:              String;
   startTime,stopTime:          Real;
   startIndex,stopIndex:        Integer;
   avg:                         Real;
   ok,cancel:                   Boolean;
   count,offset:                Integer;
   time,sum:                    Real;
Begin
if EquationType<>Schroedinger Then Exit;
DisType:=ShowTotal;
startTime:=tVec.Value(1);
stopTime:=tVec.Value(AnalysisCounter);
SelectViewport(0);
 For i:=1 to DetectorList.numberOfItems do
    Begin
    Rubout(74,15+i,6,DetectorList.GetItemColor(i));
    Print(74,15+i,Concat('Det#',toStr(i)));
    End;
repeat
With AveScrn Do
  Begin
  init;
  DefineInputPort( 0.20, 0.80, 0.35 ,0.95 );
  msg1:='Current: ';
  msg1:=concat(msg1,'(Psi* DEL(Psi)-Psi DEL(PSI*))/2i');
  LoadLine(msg1);
  LoadLine('-------------Last Detector Integrals-------------');
  str(StartTime:8:4,msg1);
  str(StopTime:8:4,msg2);
  msg1:=Concat('StartTime= ',msg1,'  StopTime= ',msg2);
  LoadLine(msg1);
  StartIndex:=0;
  Repeat
    startIndex:=startIndex+1;
    Until (tVec.VecPtr^[startIndex]>=startTime) or (startIndex>=AnalysisCounter-1);
  stopIndex:=startIndex-1;
  Repeat
    stopIndex:=stopIndex+1;
    Until (tVec.VecPtr^[stopIndex]>=stopTime) or (stopIndex>=AnalysisCounter);

  For i:=1 to DetectorList.numberOfItems do
    Begin
    offset:=2*maxDetNum;
    {Trapezoidal rule}
    sum:=Darray[i+offset].VecPtr^[StartIndex]/2;
    For count:=startIndex+1 to stopIndex-1 Do
     sum:=sum+Darray[i+offset].VecPtr^[count];
    sum:=sum+Darray[i+offset].VecPtr^[StopIndex]/2;

    time:=tVec.VecPtr^[stopIndex]-tVec.VecPtr^[StartIndex];
    if time<>0
       Then
       Begin
       avg:=sum*time/(stopIndex-startIndex+1);
       str(avg:8:4,msg1);
       str(avg/Time:8:4,msg3);
       End
       Else
       begin
       msg1:=' Undefined!  del-time=0!';
       msg3:='';
       End;
     str(i:1,msg2);
     LoadLine(concat('<num ',msg2,'>: Integral=',msg1,'   Avg.=',msg3));
     End;
    While i<4 do
       Begin
       inc(i);
       str(i:1,msg2);
       LoadLine(concat('Detector num ',msg2,' not defined.'));
       End;
  Loadline('---------------Next Calculation---------------');
  LoadLine('Start Time for Integral= {        }');
  LoadLine('  End Time for Integral= {        }');
  ioOffset:=0;
  LoadLine('OK will re-do integrals; Cancel will resume.');
  LoadLine('   [  Ok  ]     [Cancel]      [ Help ]  ' );
  SetHelpFile('Wave.Hlp', 'Quantum Current Help');
  setNumber(1,startTime);
  SetNumber(2,stopTime);
  AcceptScreen;
  ok:=GetBoolean(3+ioOffset);
  cancel:=GetBoolean(4+ioOffset);
  if OK Then
      Begin
      startTime:=GetNumber(1);
      stopTime:=GetNumber(2);
      End;
  If StopTime<StartTime then StopTime:=StartTime;
  if StartTime<tVec.Value(1) then startTime:=tVec.Value(1);
  if stopTime>tVec.Value(AnalysisCounter) Then stopTime:=tVec.Value(AnalysisCounter);
  Done;{viewport}
  End;
until Cancel;
SelectViewPort(0);
 For i:=1 to DetectorList.numberOfItems do
    Begin
    Rubout(74,15+i,6,black);
    End;
End;

Procedure AnlyPlot.ShowTimeAverage;
Var
   i,ioOffset:                    Integer;
   AveScrn:                     TInputScreen;
   msg1,msg2,msg3:                         String;
   startTime,stopTime:          Real;
   startIndex,stopIndex:        Integer;
   avg:                         Real;
   ok,cancel:                   Boolean;
   square:                      Boolean;
   count,offset:                Integer;
   time,sum:                    Real;
Begin
If EquationType=Schroedinger Then
  Begin
  AverageQMCurrent;
  Exit;
  End;
startTime:=tVec.Value(1);
stopTime:=tVec.Value(AnalysisCounter);
square:=True;
SelectViewport(0);
 For i:=1 to DetectorList.numberOfItems do
    Begin
    Rubout(74,15+i,6,DetectorList.GetItemColor(i));
    Print(74,15+i,Concat('Det#',toStr(i)));
    End;
repeat
With AveScrn Do
  Begin
  init;
  DefineInputPort( 0.20, 0.80, 0.35 ,0.95 );
  msg1:='Time Integral:  ';
  Case EquationType of
  Schroedinger:
    Case DisType of
      ShowRight: msg1:=concat(msg1,'Re(Psi)');
      ShowLeft:  msg1:=concat(msg1,'Im(Psi)');
      Showtotal: msg1:=concat(msg1,'Psi* Psi');
      End;{of Case}
  Else
    Case DisType of
      ShowRight: msg1:=concat(msg1,'Right Going Wave');
      ShowLeft:  msg1:=concat(msg1,'Left Going Wave');
      Showtotal: msg1:=concat(msg1,'Left+Right Going Wave');
      End;{of Case}
  End;{of Case EquationType}
  LoadLine(msg1);
  LoadLine('---------------Last Calculation---------------');
  str(StartTime:8:4,msg1);
  str(StopTime:8:4,msg2);
  msg1:=Concat('StartTime= ',msg1,'  StopTime= ',msg2);
  LoadLine(msg1);
  StartIndex:=0;
  Repeat
    startIndex:=startIndex+1;
    Until (tVec.VecPtr^[startIndex]>=startTime) or (startIndex>=AnalysisCounter-1);
  stopIndex:=startIndex-1;
  Repeat
    stopIndex:=stopIndex+1;
    Until (tVec.VecPtr^[stopIndex]>=stopTime) or (stopIndex>=AnalysisCounter);

  For i:=1 to DetectorList.numberOfItems do
    Begin
    Case DisType of
         ShowRight:offset:=0;
         ShowLeft:offset:=MaxDetNum;
         ShowTotal:offset:=2*maxDetNum;
         end;{of Case}
     {Trapezoidal Rule}
     if square
       Then sum:=Sqr(Darray[i+offset].VecPtr^[StartIndex])/2
       Else sum:=Darray[i+offset].VecPtr^[StartIndex]/2;
     For count:=startIndex+1 to stopIndex-1 Do
       if square
         Then sum:=sum+Sqr(Darray[i+offset].VecPtr^[count])
         Else sum:=sum+Darray[i+offset].VecPtr^[count];
     if square
       Then sum:=sum+Sqr(Darray[i+offset].VecPtr^[StopIndex])/2
       Else sum:=sum+Darray[i+offset].VecPtr^[StopIndex]/2;
     time:=tVec.VecPtr^[stopIndex]-tVec.VecPtr^[StartIndex];
     if time<>0
       Then
       Begin
       avg:=sum*time/(stopIndex-startIndex+1);
       str(avg:8:4,msg1);
       str(avg/time:8:4,msg3);
       End
       Else
         Begin
         msg1:=' Undefined!  del-time=0!';
         msg3:='';
         End;
     str(i:1,msg2);
     LoadLine(concat('<num ',msg2,'>: Integral=',msg1,'   Avg.=',msg3));
     End;
    While i<4 do
       Begin
       inc(i);
       str(i:1,msg2);
       LoadLine(concat('Detector num ',msg2,' not defined. '));
       End;
  Loadline('---------------Next Calculation---------------');
  LoadLine('Start Time for Average= {        }');
  LoadLine('  End Time for Average= {        }');
  Case EquationType of
    EandM:
      Begin
      LoadLine('Amp  #1  or  Sqr(Amp) #1   ');
      LoadLine('Which Components?  Right #2   Left #2   Sum #2');
      ioOffset:=5;
      End;
    Schroedinger:
      Begin
      LoadLine('Components? Re(Psi)#2  Im(Psi)#2  Psi*Psi#2');
      ioOffset:=3;
      End;
    Else
      Begin
      LoadLine('Amp  #1  or  Sqr(Amp) #1   ');
      ioOffset:=2;
      End;
  End;{of case EquationType}
  LoadLine('OK will re-do average; Cancel will resume.');
  LoadLine('   [  Ok  ]     [Cancel]      [ Help ]  ' );
  SetHelpFile('Wave.Hlp', 'Detector Average Help');
  setNumber(1,startTime);
  SetNumber(2,stopTime);
  if EquationType<>Schroedinger Then If square Then SetRadioButton('1',2) Else setRadioButton('1',1);
  Case EquationType of
    Schroedinger,EandM:
      Case disType of
        showRight:SetRadioButton('2',1);
        showLeft:SetRadioButton('2',2);
        showTotal:SetRadioButton('2',3);
      End; {of Case Radiobutton}
  End;{of Case EquationType}
  AcceptScreen;
  Case EquationType of
    Schroedinger,EandM:
      Case getRadioButton('2') of
        1:disType:=showRight;
        2:disType:=showLeft;
        3:disType:=showTotal;
      End; {of Case Radiobutton}
  End;{of Case EquationType}
    ok:=GetBoolean(3+ioOffset);
    cancel:=GetBoolean(4+ioOffset);
    If EquationType<>Schroedinger Then if (getRadioButton('1')=1) then square:=False Else square:=True;
    if OK Then
      Begin
      startTime:=GetNumber(1);
      stopTime:=GetNumber(2);
      End;
  If StopTime<StartTime then StopTime:=StartTime;
  if StartTime<tVec.Value(1) then startTime:=tVec.Value(1);
  if stopTime>tVec.Value(AnalysisCounter) Then stopTime:=tVec.Value(AnalysisCounter);
  Done;{viewport}
  End;
until Cancel;
SelectViewPort(0);
 For i:=1 to DetectorList.numberOfItems do
    Begin
    Rubout(74,15+i,6,black);
    End;
plotTitle.remove;
Case EquationType of
  EandM:
    Case DisType of
      ShowRight: PlotTitle.SetStr('Detectors: Right Wave Amp');
      ShowLeft:  PlotTitle.SetStr('Detectors: Left Wave Amp');
      Showtotal: PlotTitle.SetStr('Detectors: Wave Amp');
      End;{of Case}
    Schroedinger:
    Case DisType of
      ShowRight: PlotTitle.SetStr('Detectors: Re( Psi )');
      ShowLeft:  PlotTitle.SetStr('Detectors: Im( Psi )');
      Showtotal: PlotTitle.SetStr('Detector Current vs Time');
      End;{of Case}
    else
    Case DisType of
      ShowRight: PlotTitle.SetStr('Detectors: Right Amp');
      ShowLeft:  PlotTitle.SetStr('Detectors: Left Amp');
      Showtotal: PlotTitle.SetStr('Detectors: Amplitude');
      End;{of Case}
  End;
PlotTitle.Display;
Display;
End;


Procedure AnlyPlot.ShowAttrib;

Var
   AttrScrn:            TInputScreen;
   Euler,
   Average,
   Ok,Cancel,
   OldFreeze:           Boolean;
   i:                   Word;
   OldFFTsize:          Integer;
   oldFFTBasis:         FFTBasisType;

{Procedure RotateBox;

Var
   Box:                 TGraph3D;
   Info:                TSliders;
   Leave:               Boolean;

Begin;
      Anly1.Remove;
      SetColor(White);
      DefineViewPort(4, 0.20, 0.45, 0.55, 0.80);
      SelectViewPort(4);
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
End;}

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


Begin;
      if AnalysisType=Disabled then Exit;
      oldFFTBasis:=FFTBasis;
      Euler:=False;
      Average:=False;
      OldFreeze:=Freeze;
      OldFFTsize:=FFTsize;

      With AttrScrn do
           begin;
                 Init;
                 Case GeneralType of
                      VersusPosition,
                      None:           DefineInputPort( 0.30, 0.70, 0.50 ,0.68 );
                      Dynamic:        DefineInputPort( 0.30, 0.70, 0.40, 0.80 );
                      VersusTime:     DefineInputPort( 0.30, 0.70, 0.50 ,0.72 );
                      FastFT:         DefineInputPort( 0.30, 0.67, 0.40 ,0.90 );
                      ThreeD:         DefineInputPort( 0.30, 0.67, 0.15 ,0.84 );
                      EnergyRelated:  DefineInputPort( 0.30, 0.70, 0.50, 0.85 );
                 End;

                 LoadLine('         Attributes        ');

If not(GeneralType=none) and
   not(GeneralType=VersusPosition) then LoadLine('   Freeze Plot        #T   ');

                 Case GeneralType of
                      ThreeD:
                             begin;
                                   LoadLine('---Contour Plot-------#1---');
                                   LoadLine('                           ');
                                   LoadLine('Palette Number(0-4) {    } ');
                                   LoadLine('Legend                #F   ');
                                   LoadLine('                           ');
                                   LoadLine('---3D Plot------------#1---');
                                   LoadLine('                           ');
                                   LoadLine(' DrawBox              #F   ');
                                   LoadLine('                           ');
                                   LoadLine('   [Change Euler Angles]   ');
                             end;
                      Dynamic:
                              Case EquationType of
                              EandM:
                                   begin
                                   LoadLine(' Display Left Wave      #1  ');
                                   LoadlIne(' Display Right Wave     #1  ');
                                   LoadLine(' Display Total          #1  ');
                                   LoadLine('     [ Time Integral ]     ');
                                   end;
                               else
                                   begin
                                   LoadLine('');
                                   LoadLine('');
                                   LoadLine('     [ Time Integral ]     ');
                                   end;
                                end;{of EquationType}
                      EnergyRelated: if EquationType=EandM then
                                begin
                                     LoadLine('     Total     #1    ');
                                     LoadLine('     Electric  #1    ');
                                     LoadLine('     Magnetic  #1    ');
                                     LoadLine('       [Space Average]   ');
                                end Else   LoadLine('        [Space Average]   ');
                      FastFT:
                             If AnalysisType=FFTpos then
                                begin
                                LoadlIne('Show Maximum Values   #T ');
                                LoadLine('                           ');
                                LoadLine('FFT Basis set: ');
                                If EquationType= Schroedinger
                                  Then LoadLine('Energy #2     Momentum #2  ')
                                  Else LoadLine('   Sin #2      Sin/Cos #2  ');
                                end
                             else
                                begin
                                LoadLine('Show Maximum Values   #T ');
                                LoadLine(' ');
                                LoadLine('FFT basis set is sin/cos. ');
                                LoadLine('Number of Points in FFT    ');
                                LoadLine('  #1    #1     #1     #1   ');
                                LoadLine(' 128   256    512   1024  ');

                                end;
                      VersusPosition,
                      None: LoadLine('            None           ');
                 End;
                 Loadline('                           ');
                 LoadLine(' [  Ok  ] [Cancel] [ Help ]');
                 LoadLine('                           ');
                 Case AnalysisType of
                    Disabled: SetHelpFile('Wave.Hlp', 'No Help');
                    Index_Gain: SetHelpFile('Wave.Hlp', 'Index Plot Help');
                    YvsTDensity:  SetHelpFile('Wave.Hlp', 'ThreeD Help');
                    YvsT3D:SetHelpFile('Wave.Hlp', 'ThreeD Help');
                    FFTpos,FFTTime:SetHelpFile('Wave.Hlp', 'FastFt Help');
                    YvsT: SetHelpFile('Wave.Hlp', 'Wave Detectors Help');
                    EvsT: SetHelpFile('Wave.Hlp', 'Integrated YSquare Help');
                    PvsT:SetHelpFile('Wave.Hlp', 'Source Power Help');
                    EnergyDist: SetHelpFile('Wave.Hlp', 'YSquare Help');
                 End;

                 If (GeneralType<>None) and
                    (GeneralType<>VersusPosition) then
                       SetBoolean(1,Freeze); {Sets Initial Analysis Selection}

                 Case GeneralType of
                      ThreeD:
                             begin;
                                   Case AnalysisType of
                                        YvsTDensity: SetRadioButton('1',1);
                                        YvsT3D:      SetRadioButton('1',2);
                                   End;
                                   TheContour.Init;
                                   TheThreeD.Init;
                                   TheThreeD.SetDistance(1.5,2.5);
                                   TheThreeD.SetBoxRatios(1,4/5,2/3);
                                   {RePlot;}
                                   SetNumber(3,PalNumber);
                                   SetBoolean(4,LegBool);
                                   SetBoolean(6,BoxBool);
                             end;
                      EnergyRelated:
                              if EquationType=EandM then
                              begin
                                   Case ShowEnergyType of
                                        Total:     SetRadioButton('1',1);
                                        Kinetic:   SetRadioButton('1',2);
                                        Potential: SetRadioButton('1',3);
                                   End; { of Case }
                              end;
                      Dynamic:
                              begin
                                   Case equationType of
                                   EandM:
                                   Case DisType of
                                        showLeft: setRadioButton('1',1);
                                        showRight: setRadioButton('1',2);
                                        showTotal: SetRadioButton('1',3);
                                     End; {of Case DisType}
                                   End; { of Case equationtype}
                              end;
                      FastFT:Begin
                             SetBoolean(2,ShowMaxHist);
                             If analysisType=FFTTime
                             Then
                               Begin
                               If EquationType=Schroedinger Then FFTSize:=FFTSize DIV 2;
                               Case FFTsize of
                                 128:  SetRadioButton('1',1);
                                 256:  SetRadioButton('1',2);
                                 512: SetRadioButton('1',3);
                                 1024: SetRadioButton('1',4);
                                 End; {of case}
                               End
                             Else
                                 If FFTBasis=SinOnly Then
                                 SetRadioButton('2',1) Else SetRadioButton('2',2);
                             End;
                 End;

                 Repeat
                 Begin
                   AcceptScreen;
                   Case GeneralType of         {Gets the Ok Button's boolean}
                      Dynamic:              Case EquationType of
                                            EandM:
                                              begin
                                                 Average:=GetBoolean(5);
                                                 Ok:=GetBoolean(6);
                                                 Cancel:=GetBoolean(7);
                                              end;
                                            else
                                              begin
                                                 Average:=GetBoolean(2);
                                                 Ok:=GetBoolean(3);
                                                 Cancel:=GetBoolean(4);
                                              end;
                                            end;{ofCase Equationtype}
                      FastFT:               If analysisType=FFTpos
                                               Then
                                                 Begin
                                                 Ok:=GetBoolean(5);
                                                 cancel:=GetBoolean(6);
                                                 End
                                               Else
                                               Begin
                                               Ok:=GetBoolean(7);
                                               cancel:=Getboolean(8);
                                               End;
                      ThreeD:               begin;
                                                  Euler:=GetBoolean(7);
                                                  Ok:=GetBoolean(8);
                                                  Cancel:=GetBoolean(9);
                                            end;
                      VersusTime:           Begin Ok:=GetBoolean(2); Cancel:=GetBoolean(3); End ;
                      EnergyRelated:        if EquationType=EandM
                                              then Begin Average:=GetBoolean(5); Ok:=GetBoolean(6); Cancel:=GetBoolean(7); End
                                              Else Begin Average:=GetBoolean(2); Ok:=GetBoolean(3); Cancel:=GetBoolean(4); End;
                      VersusPosition,
                      None:                 Begin Ok:=GetBoolean(1);  Cancel:=GetBoolean(2); End;
                   End;{of Case GeneralType}
                 If Average Then
                   begin
                   done;
                   if GeneralType=EnergyRelated Then ShowSpaceAverage Else ShowTimeAverage;
                   exit
                   end;
                 End Until Not(Average) OR Ok OR Cancel;


                 If Ok or Euler then
                 Case GeneralType of       {Get the radiobutton's boolean}
                      None: ;
                      ThreeD:     begin;
                                        Case GetRadioButton('1') of
                                             1:
                                               begin;
                                                     If AnalysisType=YvsT3D then
                                                        With Scale do
                                                             begin
                                                             PlotAxes.IsVisible:=TRUE;
                                                             xMin:=1;
                                                             xMax:=VacuumPts;
                                                             yMin:=InitialTime;
                                                             yMax:=Time;
                                                             If yMin>=yMax then yMax:=yMin+1;
                                                             SetScale(xMin,yMin,xMax,yMax);
                                                             PlotAxes.Display;
                                                             end;
                                                     AnalysisType:=YvsTDensity;
                                                     PlotTitle.Remove;
                                                     PlotTitle.SetStr('Contour of y(x) vs Time');
                                                     PlotTitle.Display;
                                                     PalNumber:=trunc(GetNumber(3));
                                                     LegBool:=GetBoolean(4);
                                               end;
                                             2:
                                               begin;
                                                     If AnalysisType=YvsTDensity then
                                                        begin
                                                             PlotAxes.Remove;
                                                             PlotAxes.IsVisible:=False;
                                                        end;

                                                     AnalysisType:=YvsT3D;
                                                     PlotTitle.Remove;
                                                     PlotTitle.SetStr('3D of y(x) vs Time');
                                                     PlotTitle.Display;
                                                     BoxBool:=GetBoolean(6);
                                                     If Euler then
                                                        begin;
                                                              RotateBox;
                                                              If ExPanded then
                                                                 begin
                                                                      isVisible:=True;
                                                                      Display;
                                                                 end
                                                              else
                                                                  DisplayAnalysis;
                                                              If not(GetBoolean(1)) then Exit;
                                                        end;
                                                end;
                                        End;
                                  end;
                      EnergyRelated:if EquationType=EandM Then
                             begin
                                  Case GetRadioButton('1') of
                                       1: ShowEnergyType:=Total;
                                       2: ShowEnergyType:=Kinetic;
                                       3: ShowEnergyType:=Potential;
                                  End; { of Case }
                             end;
                      Dynamic:
                             begin
                                  AnalysisType:=YvsT;
                                  Case EquationType of
                                    EandM:
                                      Case GetRadioButton('1') of
                                       1: disType:=showleft;
                                       2: disType:=showRight;
                                       3: disType:=showTotal;
                                      End; { of Case RadioButton}
                                    else disType:=showTotal;
                                  end;
                                  plotTitle.remove;
                                  Case EquationType of
                                    EandM:
                                      Case DisType of
                                      ShowRight: PlotTitle.SetStr('Detectors: Right Wave Amp');
                                      ShowLeft:  PlotTitle.SetStr('Detectors: Left Wave Amp');
                                      Showtotal: PlotTitle.SetStr('Detectors: Wave Amp');
                                      End;{of Case}
                                    Schroedinger: PlotTitle.SetStr('Detector Current vs Time');
                                    Else PlotTitle.SetStr('Detectors: Amplitude');
                                   End;
                                   PlotTitle.Display;
                             end;{of Dynamic}
                      FastFT: If analysisType=FFTpos then
                                 begin
                                      AnalysisType:=FFTpos;
                                      If GetRadioButton('2')=1
                                        Then FFTBasis:=SinOnly
                                        Else FFTBasis:=SinCos;
                                      PlotTitle.Remove;
                                      PlotTitle.SetStr('Fourier Analysis of y(x).');
                                      PlotTitle.Display;
                                      ShowMaxHist:=GetBoolean(2);
                                      ClearScreen;
                                      if OldFFTBasis<>FFTBasis
                                        Then FourierAnalysis(True, True,True)
                                        Else FourierAnalysis(False,True,True);
                                 end
                              else
                                  begin
                                       AnalysisType:=FFTtime;
                                       PlotTitle.Remove;
                                       PlotTitle.SetStr('Fourier Analysis of y(t).');
                                       PlotTitle.Display;
                                       Case GetRadioButton('1') of
                                            1: FFTsize:=128;
                                            2: FFTsize:=256;
                                            3: FFTsize:=512;
                                            4: FFTsize:=1024;
                                       End;
                                       If EquationType=Schroedinger Then FFTSize:=FFTSize*2;
                                       ShowMaxHist:=GetBoolean(2);
                                       If (OldFFTsize<>FFTsize)
                                         Then
                                          begin
                                               AnalysisCounter:=1;
                                               ShowWaitMsg:=True;
                                               Plot.Display;

                                               Rwave.Resize(FFTsize);
                                               Lwave.Resize(FFTsize);

                                               Rwave.Fill(0);
                                               Lwave.Fill(0);

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
                                               FourierAnalysis(True,True,True)
                                          end
                                       else
                                         If (oldFFTBasis<>FFTBasis)
                                           Then FourierAnalysis(True,True,True)
                                           Else FourierAnalysis(False,True,True);
                                  end;
                      End;

                 If Ok or Euler then
                  If (GeneralType<>None) and (GeneralType<>VersusPosition) then
                    begin;
                          Freeze:=GetBoolean(1);
                          if Freeze Then PlotTitle.setObjColor(Red) Else PlotTitle.setObjColor(Magenta);
                          PlotTitle.Display;
                          If not(OldFreeze=Freeze) then
                             If (OldFreeze) then
                                begin;
                                      RemoveStorage;
                                      GeneralType:=none;
                                      AnalysisType:=Disabled;
                                      PlotTitle.Remove;
                                      PlotTitle.SetStr('Disabled');
                                      PlotTitle.Display;
                                      TimeCompress:=1;
                                      PlotAxes.Remove;
                                      PlotAxes.IsVisible:=False;
                                      ClearScreen;
                                      Exit;
                                end;
                    end;

                 Done;
           end;

           If not(AnalysisCounter=1) and not((AnalysisType=FFTpos)or(AnalysisType=FFTtime)) then
              begin
                   ClearScreen;
                   Replot;
              end;
End;

Procedure AnlyPlot.ClearScreen;

Begin
     Plot.Display;
End;

end.
