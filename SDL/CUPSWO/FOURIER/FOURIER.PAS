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

          {**********************************************}
          {**********************************************}
          {**           Program Fourier.Pas            **}
          {**  Fourier Analysis and Fourier Transforms **}
          {**          Written by Brian James          **}
          {**         Last change: 9 May 1995          **}
          {**********************************************}
          {**********************************************}

Program Fourier;

USES Graph,crt,DOS,CUPSMUPP,CUPS,CupsProc,CupsFunc,Cupsgui,CupsGrph,CupsPars;

{$M 32768,0,655360} {memory control for Stack and Heap }

const

   ndim  = 2;
   ndat2 = 2048;
   maxCol = 64;
   maxRow = 64;

   alpha      : real    = 30;
   beta       : real    = 20;
   gamma      : real    = 0;
   boxN       : integer = 1;
   size       : real    = 1;
   distance   : real    = 2.5;


PROCEDURE ShowInitialScreen;
var
   helpScr0    : HelpScrType;
BEGIN
   helpscr0[01]:= '                                                 ';
   helpscr0[02]:= '      FOURIER ANALYSIS & FOURIER TRANSFORMS      ';
   helpscr0[03]:= '                                                 ';
   helpscr0[04]:= '                Brian W James                    ';
   helpscr0[05]:= '   University of Salford, Salford, M5 4WT, UK    ';
   helpscr0[06]:= '                                                 ';
   helpscr0[07]:= '                  Version 1.0                    ';
   helpscr0[08]:= '               Copyright (c) 1995                ';
   helpscr0[09]:= '                                                 ';
   helpscr0[10]:= '  In Fourier analysis any periodic function      ';
   helpscr0[11]:= '  can be represented by a sum of sine and        ';
   helpscr0[12]:= '  cosine functions. The effects of dispersion    ';
   helpscr0[13]:= '  and attenuation on the propagation of the      ';
   helpscr0[14]:= '  waveform can be investigated. The evolution    ';
   helpscr0[15]:= '  of a standing wave can be illustrated.         ';
   helpscr0[16]:= '                                                 ';
   helpscr0[17]:= '  Non-periodic functions  can be represented     ';
   helpscr0[18]:= '  by a integral sum of a spectrum of sine and    ';
   helpscr0[19]:= '  cosine functions of all frequencies called the ';
   helpscr0[20]:= '  Fourier transform. One and two dimensional     ';
   helpscr0[21]:= '  discrete Fourier transforms are illustrated.   ';
   helpscr0[22]:= '                                                 ';
   helpscr0[23]:= '  Press <enter> or click the mouse to continue.  ';
   helpscr0[24]:= '  F10  will select the menu.                     ';
   helpscr0[25]:= '                                                 ';
   help(helpscr0);
END;

TYPE
   Twave = (none, square, triangle, sawtooth, half_rect, full_rect, pulse,
             fnct, curve, coeffs, propagate, evolve);
   Tfft1D = (none1,pulse1, twin, grating, coswave, step, sawtooth1, triangle1,
   Gaussian, fnctn, drawn, inverse);
   Tmode = (initial,fseries, fft_1d, fft_2d, dataIO);
   Tstate = (nogo, go, add, extra, auto, dun);
   Tplot = (notset,Re,Im,modz,pwr);
   Tdata = RECORD
             wave : Twave;
             transform : Tfft1D;
             mode : Tmode;
             addterm,PropScale,upcont,yvalset : boolean;
             state : Tstate;
             numT,numS,numC,numTset,numSset,slitwidth,slitpos,slitsep,
                mysign,sign2d,PhaseStep,fft1_num,Oldfft1_num : integer;
             time,PhaseCount : longint;
             ABcoeffs  : array[0..3,0..100] of REAL;
             RMSdif : array[0..100] of REAL;
             picksel : array [0..4095] of CHAR;
             PulseRatio, Cauchy_A, Cauchy_B, AttenCoeff,yrange1d,oldy : REAL;
             ContourDrawn : Tplot;
             coeffile,curvefile,picfile,datastr1,datastr2 : string;
           END;

   RealArrayNDAT2 = ARRAY [1..ndat2] OF double;
   IntegerArrayNDIM = ARRAY [1..ndim] OF integer;

   TCurveF = object
      PROCEDURE SetUpF (Zcoef : real ; nData, Color1,Color2 : word);
      PROCEDURE DrawF (Acoef,Bcoef : real; Color,t : word);
      PROCEDURE Draw_Sum (VAR MaxDif : REAL; Color : word);
      PROCEDURE Draw_Evolve (VAR ephase : real; Color : word);
      PROCEDURE ReStartPropagateWave (Color : word ; phase : real);
      PROCEDURE PropagateWave (Color : word);
      PROCEDURE Evaluate (nData : word);
      PROCEDURE MouseF (VAR ym : real; VAR  im : integer);
      FUNCTION Aintegrate(tnum : word) : REAL;
      FUNCTION Bintegrate(tnum : word) : REAL;
      FUNCTION Zintegrate(tnum : word) : REAL;
PRIVATE
      x : ARRAY [0..MaxNumData] OF integer;
      yp : ARRAY [0..MaxNumData, 0..2] OF integer;
      yr : ARRAY [0..MaxNumData, 0..2] of real;
      termnum   : word;
   END;
{**************************************************************}
const
   MaxSub=512;
   MaxSubX2=1024;
TYPE
   Fft1D = OBJECT
      Procedure SetUpFourierTransform(numPts : integer);
      PROCEDURE ModifyTransform (VAR Datarecord : Tdata; VAR CalcInv : BOOLEAN);
      Procedure MouseT (VAR ym : REAL; VAR im : INTEGER);
      PROCEDURE Evaluate ( nData : word);
      PROCEDURE Plotfft_1d(plotcolor : word);
      Procedure StdDataForFourierTransform(VAR Datarecord : Tdata);
      Procedure Pack1Ddata;
      Procedure Unpack1Ddata;
      Procedure Pack2DRow(iRow : integer);
      Procedure Unpack2DRow(iRow : integer);
      Procedure Pack2DCol(jCol : integer);
      Procedure Unpack2DCol(jCol : integer);
      Procedure New2DFFT(Var isign2D : integer);
      Procedure FastFourierTransform(isign1D : integer);
      Procedure PlotFourierTransform(color1,color2,color3 : word);
PRIVATE
   Areal,Bimage : ARRAY[0..MaxSub] OF REAL;
   xr,yr : REAL;
   Data1D: ARRAY[1..MaxSubX2] of DOUBLE;
   nnn,isign:integer;
END;

VAR
   num1d : word;
   TsfmData :   Fft1D;
   iloop,jloop : integer;
   Menu    : Tmenu;
   HotKeys : THotKeys;
   Datarecord    : Tdata;
   termF   : TCurveF;
   FourierFunc : TParser;
   FuncString : string;
   surface,SmallBox : TGraph3D;
   ARe,BIm,Cmodz,Power,Yvals1,Yvals2,Yvals3 : DMatrix;
   contour      : TContour;
   data: RealArrayNDAT2;
   nn : IntegerArrayNDIM;
   sliders      : TSliders;
   evolvephase, Xconst, xr, yr : REAL;
   isign : integer;
   stdwave : set of Twave;
   coeffile,curvefile : text;           { standard serial text file }
   saved,picflag,picture : boolean; {saved TRUE if a successfully loaded or saved }
   coefsfile,curvesfile : string;
   rgbpal : array[0..15] of integer;
   oldpalette : array[1..3,0..15] of integer;
   mydelay : TWaitMessage;
   fft1_button : TButtons;
   Cxzero : Complex;
   ExitFlag : Boolean;

Procedure Fft1D.SetUpFourierTransform(numPts : integer);

{Proc 'four1' Replaces  data in Data1D by its discrete/inveres discrete
 Fourier Transform if isign is	1/  -1.  nnn must be power of 2}

BEGIN
   isign:= 1; {discrete Fourier Transform}
   nnn:=numPts;
END;

PROCEDURE Continue_F10;
Begin
   announce(' Continue - or press F10 to make a NEW choice from the menu. ');
END;


PROCEDURE Fft1D.ModifyTransform (VAR Datarecord : Tdata; VAR CalcInv : BOOLEAN);
VAR
   Ch1l,Ch1r,Ch2l,Ch2r : longint;
   imt, RadioChoice : integer;
   dataScreen : TInputScreen;
BEGIN
with dataScreen DO
   BEGIN
      init;
      DefineInputPort(0.1,0.9,0.5,0.925);
      LoadLine('  FILTERED INVERSE TRANSFORM.');
      LoadLine('');
      LoadLine('  Give the range of frequencies to be set to zero.');
      LoadLine('  Start  {      }  and end at  {      } (-256 to 256)');
      LoadLine('  A second range of frequencies may be set to zero.');
      LoadLine('  Start  {      }  and end at  {      }');
      LoadLine('');
      LoadLine('  Calculate inverse transform without filtering #2');
      LoadLine('  Calculate inverse transform after filtering   #2');
      LoadLine('               [  Ok  ]   [Cancel] ');
      FOR imt:= 1 TO 4 DO SetNumberLimits(imt,-256,256);
      SetNumber(1,-256);
      SetNumber(2,-256);
      SetNumber(3,256);
      SetNumber(4,256);
      SetRadioButton('2',1);
      AcceptScreen;
      IF Canceled THEN BEGIN
         IF Datarecord.transform in [fnctn..inverse] THEN
            Continue_F10(*Announce(' Continue - or F10 to make a NEW choice from the menu. ')*)
         ELSE Announce(' Transform cancelled - adjust sliders as needed. ');
         CalcInv:= false;
      END
      ELSE WITH Datarecord DO BEGIN
         Ch1l:= round(GetNumber(1)) + 256;
         Ch1r:= round(GetNumber(2)) + 256;
         Ch2l:= round(GetNumber(3)) + 256;
         Ch2r:= round(GetNumber(4)) + 256;
         imt:= Ch1l;
         CalcInv:= true;
         RadioChoice:= GetRadioButton('2');
         IF RadioChoice = 2 THEN BEGIN
            FOR imt:= Ch1l TO Ch1r DO BEGIN
               Areal[imt]:= 0;
               Bimage[imt]:= 0;
            END;
            FOR imt:= Ch2l TO Ch2r DO BEGIN
               Areal[imt]:= 0;
               Bimage[imt]:= 0;
            END;
         END;
      END;
      done;
   END;
END;

PROCEDURE TcurveF.MouseF (VAR ym : real; VAR im : integer);
BEGIN
   yr[im,2]:= ym;
END;

Procedure Fft1d.MouseT (VAR ym : REAL; VAR im : INTEGER);
BEGIN
   Areal[im]:= ym;
   Bimage[im]:= 0;
END;

PROCEDURE MouseDraw;
VAR
   xm,ym,xmold,ymold,xlo,xhi : REAL;
   im,maxim,numofview : INTEGER;
   inside : BOOLEAN;

BEGIN
   CASE DataRecord.mode OF
   fseries : BEGIN
                xlo:= -Pi;
                xhi:= Pi;
                numofview:=1;
                maxim:= Datarecord.numS;
             END;
   fft_1d  : BEGIN
                xlo:= - MaxSub div 2;
                xhi:= MaxSub div 2;
                numofview:= 5;
                maxim:= 512;
                DefineScale(5,xlo,xhi,-1.2,+1.3);
             END;
   END; {case}
   OpenViewPort(numofview);
   SelectScale(numofview);
   SelectViewPort(numofview);
   IF numofview = 1 THEN Axis(0.0,0.0,1,0.5) ELSE Axis(0.0,0.0,maxim/8,1);

   WITH GraphScales [numofview] DO Xconst:= (xmax - xmin) / maxim;

   message('Please draw curve with mouse from left to right of screen slowly');
   im:=0;
   repeat
      mousePosn(xm,ym,numofview,numofview,inside);
      mousePosn(xm,ym,numofview,numofview,inside);
      IF xm<(xlo+Xconst) THEN
      BEGIN
         mousePosn(xm,ym,numofview,numofview,inside);
         mousePosn(xm,ym,numofview,numofview,inside);
         xmold:=xm;
         ymold:=ym;
         IF numofview = 1 THEN termF.MouseF (ym,im)
            ELSE  TsfmData.MouseT (ym,im);
         im:=1;
         SetColor(lightcyan);
         SelectScale(numofview);
         SelectViewPort(numofview);
         PlotLine(xhi - 5 * Xconst,ym,xhi,ym);
      END;
   until im=1;
    beep;

   SetColor(white);
   repeat
      mousePosn(xm,ym,numofview,numofview,inside);
      IF xm>(xlo + im * Xconst) THEN
      BEGIN
         IF numofview = 1 THEN termF.MouseF (ym,im)
            ELSE  TsfmData.MouseT (ym,im);
         SelectScale(numofview);
         SelectViewPort(numofview);
         PlotLine(xmold,ymold,xm,ym);
         im:=im + 1;
         IF im = (maxim/2) THEN
          message(' - to avoid a discontinuity aim for blue line on the right hand side ');
         xmold:= xm;
         ymold:= ym;
      END;
   until im = maxim + 1;
END;

function open(var fp:text; name:string; filetype:integer):boolean;
{ checks to see if file exists }
begin
     assign(fp,name);
     {$I-}
     if filetype = 0 then
          Reset(fp)
     else
          rewrite(fp);
     {$I+}
    open := (ioresult = 0);
end { open };

Procedure loadcoef( VAR loaded : boolean; fileName : string; VAR DataRecord : Tdata);

var
   i : integer;
   key : char;
	flag : boolean;

  begin
     if open(coeffile,fileName,0) then
     begin
          readln(coeffile,Datarecord.numc);
          for i:= 0 to Datarecord.numc  do
          begin
               readln(coeffile,Datarecord.ABcoeffs[0,i]);
               readln(coeffile,Datarecord.ABcoeffs[1,i]);
          end;{ for }
          close(Coeffile);
		loaded := true;
	end; {if }
end; {proc loadcoef}

Procedure loadcurve( VAR loaded : boolean; fileName : string;
                                          VAR DataRecord : Tdata);
var
   yrval : real;
   i : integer;
   key : char;
	flag : boolean;

begin
   loaded:= false;
   if open(curvefile,fileName,0) then begin
      readln(curvefile,Datarecord.numS);
      for i:= 0 to Datarecord.numS  do BEGIN
         readln(curvefile,yrval);
         termF.MouseF (yrval,i);
      END;
      close(curvefile);
		loaded := true;
	end;
end; {proc loadcurve}

procedure savecoefs(fileName:string);
{ saves Fourier coeffs }
var
   i : integer;
   key : char;
	flag : boolean;
begin
	saved := false;
     if open(coeffile,filename,1) then
     begin
          writeln(coeffile,Datarecord.numC);
          for i:= 0 to Datarecord.numC do
          begin
               writeln(coeffile,Datarecord.ABcoeffs[0,i]);
               writeln(coeffile,Datarecord.ABcoeffs[1,i]);
          end;{ for }
          close(coeffile);
		saved := true;
     end;{ else}
end; {proc savecoef}


procedure savepict(fileName : string; VAR levels : DMatrix);
{ saves picture in compact form }
var
   pmin,pmax : real;
   isp,jsp,iisp,jjsp,ksp : integer;
   key : char;
	flag : boolean;
begin
	saved := false;
      if open(coeffile,filename,1) then
      begin
         levels.MinMax(isp,jsp,pmin,iisp,jjsp,pmax);
         FOR jsp:=0 to MaxCol - 1 DO BEGIN
            FOR isp:=0 to MaxRow - 1 DO BEGIN
               ksp:=trunc((levels.Value(jsp+1,isp+1)-pmin)*31/(pmax-pmin)+0.5);
               IF ksp <= 9 THEN iisp:= ksp + 48 ELSE iisp:= ksp + 55;
               write(coeffile,chr(iisp));
            END;
            writeln(coeffile);
         END;
         close(coeffile);
		   saved:= true;
     end;{ else}
end; {proc savepict}

Procedure DataInput (VAR ChosenFile : string; VAR FileSelected : Boolean; DotExt : string);
VAR
   DataInScreen : TInputscreen;
   n : byte;
   MyFile : string;
BEGIN
   MyFile:= '*.'+DotExt;
   MyFile:= OpenFile(MyFile);
   BEGIN
      with DataInScreen do
      BEGIN
         init;
         DefineInputPort (0.12, 0.88, 0.39, 0.61);
         LoadLine('  Either accept the file chosen or');
         LoadLine('  give the full name of the data file');
         LoadLine('  from which the data is to be read.');
         LoadLine('filename = "                                         "');
         LoadLine('        [  Ok  ]    [Cancel] ');
         SetString(1,MyFile);
         AcceptScreen;
         IF Canceled THEN BEGIN
            Announce('No file has been selected.');
            FileSelected:= false;
         END
         ELSE BEGIN
            ChosenFile:= GetString(1);
            FileSelected:= true;
         END;
         done;
      END;
   END;
END;

FUNCTION DataOutput (VAR LensSfile : string) : boolean;
VAR
   DataOutScreen : TInputscreen;
   n : byte;
   satisfied : boolean;
BEGIN
   with DataOutScreen do
   BEGIN
      init;
      DefineInputPort (0.12, 0.88, 0.39, 0.61);
      IF picflag THEN BEGIN
        LoadLine('  The picture should be stored in a file which has');
        LoadLine('  the extension .TXT  ');
      END ELSE BEGIN
        LoadLine('  The coefficients should be stored in a file which has');
        LoadLine('  the extension .dat');
      END;
      LoadLine('  Please give the full name for the data file');
      LoadLine('filename = "                                         "');
      LoadLine('                 [  Ok  ]  [Cancel] ');
      LensSfile:= '';
      REPEAT
      AcceptScreen;
         IF Canceled THEN BEGIN
            Announce('The data has not been filed.');
            Dataoutput:= true;
            satisfied:= true;
         END
         ELSE BEGIN
            IF GetString(1) = '' THEN satisfied:= false
            ELSE BEGIN
               LensSfile:= GetString(1);
               Dataoutput:= true;
               satisfied:= true;
            END;
         END;
      UNTIL satisfied;
      done;
   END;
END;


FUNCTION FInput : boolean;
VAR
   FScreen : TInputScreen;
   MyParserHelp : HelpScrType;
   fString,ystr,rstr,tstr : string;
   ychar : char;
   n : byte;
   Ok : Boolean;
BEGIN
   with FScreen do
   BEGIN
      init;
      ystr:= '';
      rstr:= ' Remember the x range is - pi to + pi ';
      CASE  DataRecord.mode OF
         fseries : tstr:='Fourier';
         fft_1d  : tstr:='1-D DFT.';
         fft_2d  : BEGIN
                      ystr:= ',y';
                      rstr:= ' Remember that you need a function of x and y';
                      tstr:='2-D DFT.'
                   END;
      END;{case}
      AboutParser (MyParserHelp);
      SetHelpScreen(MyParserHelp);
      DefineInputPort (0.10, 0.90, 0.35, 0.65);
      LoadLine('                Input a FUNCTION for '+tstr);
      LoadLine(rstr );
      LoadLine(' You Can Use: +, -, *, /, ^, sqrt, abs, ln, exp, pi, e, h AND');
      LoadLine(' all trigonometric/hyperbolic functions AND their inverses.');
      LoadLine(' f(x'+ystr+') = "                                                   " ');
      LoadLine('');
      LoadLine('                 [  Ok  ]   [Cancel]   [ Help ] ');
      SetString( 1, FuncString);
      repeat
         AcceptScreen;
         Ok := False;
         IF not Canceled THEN {Announce ('Function input cancelled.')
         ELSE} BEGIN
            FuncString:= GetString(1);
            IF DataRecord.mode = fft_2d THEN ychar:= 'y' ELSE ychar:= ' ';
            OK := FourierFunc.parse('x',ychar,' ',' ',FuncString);
            if (not Ok) and (not Canceled) then
             announce('Expression can not be evaluated - Please try again');
         END;
      until Ok or canceled;
      FInput := true;
      done;
   END;
END;

{ ------------ PROCEDURES FOR COLOR CODING (from Hiller) -------------- }
{modified by B James for linear scale}
FUNCTION ColorWheel(val:Real; VAR numColors:Integer) : Word;
  {Convert val to color on color wheel.}
   CONST
      range = 7;
      wheel : ARRAY [0..range] OF Word
                    = (lightred, yellow, lightgreen, lightcyan,
                       lightblue, blue, magenta, lightmagenta);
   VAR
      index : Integer;
   BEGIN
      numColors := range+1;
      val := frac(val/(2*Pi)+0.0625);
      IF val<0.0
         THEN val := val+1.0;
      index := trunc(numColors*val);
      IF index=numColors
         THEN index:=0;
      ColorWheel := Wheel[index]
   END;      {ColorWheel}

PROCEDURE OpenHiddenViewport (Vnum: integer);
BEGIN
   SetColor(black);
   SelectViewPort(vNum);
   HideMouse ;
   SetFillStyle(SolidFill, black);
   WITH Views[vnum] DO bar(0,0,Vx2-Vx1,Vy2-Vy1);
   WITH Views[vnum] DO rectangle(0,0,Vx2-Vx1,Vy2-Vy1);
   ShowMouse ;
END;

PROCEDURE DisplayLinearColorWheel;
{Display for phase scale.}
   VAR
      numColors,n,m,sx,sy,temp : Integer;
      color : Word;
      delta : Real;
   BEGIN
      OpenHiddenViewport(11);
      Color := ColorWheel(0.0,numColors);
      delta := 180.0/NumColors;
      DefineScale(8,0,1,0,2);
      SelectScale(8);
      Map(0.1,1.75,sx,sy);
      FOR n:= 0 TO 2*numColors+1 DO
         BEGIN
            Color:= ColorWheel(2*Pi*(2*numColors - n)*delta/360,temp);
            SetColor(Color);
          FOR m:=0 to 7 do
          line(sx,sy+round(n*7.5)+m,sx+12,sy+round(n*7.5)+m);
         END;
      SetColor(white);
      rectangle(sx-1,sy,sx+12,sy+135);
      Print(2,8,'-0');
      Print(2,5,'-'+chr(227));
      Print(2,2,'-2'+chr(227));
      Print(1,9,'phase:');
   END;

FUNCTION PulseWidth (VAR r : real) : REAL;
VAR
   WScreen : TInputScreen;
BEGIN
   with WScreen do
   BEGIN
      init;
      DefineInputPort (0.12, 0.88, 0.36, 0.67);
      LoadLine('');
      LoadLine(' Input pulse width as a fraction of the screen width');
      LoadLine('');
      LoadLine(' Pulse width  {      } (0.01 to 0.9)');
      LoadLine('');
      LoadLine('               [  Ok  ]     [Cancel]');
      LoadLine('');
      SetNumber(1,r);
      SetNumberLimits(1,0.01,0.9);
      AcceptScreen;
      PulseWidth:= r;
      IF Canceled THEN Announce ('Previous value of pulse width retained.')
      ELSE PulseWidth:= GetNumber(1);
      done;
   END;
END;

PROCEDURE Fft1D.Evaluate (nData : word);
   VAR
      hinc : REAL;
        i  : INTEGER;
        myout,rx : Complex;
BEGIN
   HaltIfError := false;
   ErrorFound  := false;
   hinc:=2*Pi/nData;
   rx.im:= 0;
   i:= 0;
   WHILE (i <= nData) AND (DataRecord.state <> nogo) and (not ErrorFound) DO
   BEGIN
      rx.Re:= i*hinc - pi;
      FourierFunc.Cxf(rx,Cxzero,Cxzero,Cxzero,myout);
      Areal[i]:= myout.Re;
      Bimage[i]:= myout.Im;
      IF (i>0) AND (ABS(Areal[i-1]) > 0) AND (ABS(Areal[i]) > 0) THEN BEGIN
         IF ((Areal[i-1]/Areal[i] < 0) AND (ABS(Areal[i-1]) > 10)) OR
            (ABS(Areal[i]) > 1.0e15) THEN BEGIN
               announce('No Fourier transform possible for this function');
               DataRecord.state:= nogo;
            END;
      END;
   i:= i + 1;
   END;
   if ErrorFound then DataRecord.state:= nogo;
   HaltIfError := true;
   ErrorFound  := false;
END;

PROCEDURE Fft1D.Plotfft_1d(plotcolor : word);
VAR
   i,lowx : integer;
   max1dreal : real;

BEGIN
   OpenViewPort(5);
   SelectScale(5);
   IF Datarecord.transform = inverse THEN BEGIN
      DrawAxis(0.0,0.0,num1D/8,(trunc(DataRecord.oldy))/2,black);
      OpenViewPort(5);
      max1dreal:= 1;
      FOR i:= 0 TO nnn DO BEGIN
         IF ABS(Areal[i]) > max1dreal THEN max1dreal:= ABS(Areal[i]);
         IF ABS(Bimage[i]) > max1dreal THEN max1dreal:= ABS(Bimage[i])
      END;
      DefineScale(5,-num1D/2,+num1D/2,-max1dreal*1.1,+max1dreal*1.2);
      DrawAxis(0.0,0.0,num1D/8,(trunc(max1dreal*1.1))/2,white);
      DataRecord.oldy:= max1dreal*1.1;
   END
   ELSE BEGIN
      DefineScale(5,-num1D/2,+num1D/2,-1.2,+1.3);
      DrawAxis(0.0,0.0,num1D/8,1,white);
   END;
   SelectViewPort(5);
   SelectScale(5);
   lowx:= -(nnn DIV 2);
   Setcolor(white);
   IF Datarecord.transform <> inverse THEN Print(8,1,'Input function')
   ELSE Print(8,1,'Filtered input');
   PutLabel(top,'                                         1-D Discrete Fourier Transform');
   Setcolor(plotcolor);
   IF Datarecord.transform <> inverse THEN
                   Plotline(lowx*0.9,1.15,lowx*0.8,1.15);
   FOR i:=1 TO nnn DO Plotline((lowx+i-1),(Areal[i-1]),(lowx+i),(Areal[i]));
   Setcolor(lightmagenta);
   IF Datarecord.transform = inverse THEN
   FOR i:=1 TO nnn DO Plotline((lowx+i-1),(Bimage[i-1]),(lowx+i),(Bimage[i]));
END;

Procedure Fft1D.StdDataForFourierTransform( VAR Datarecord : Tdata);
VAR
   i,j,k,wid,pos,sep : integer;
   F10 : functionXY;
BEGIN
   wid:= Datarecord.slitwidth;
   pos:= Datarecord.slitpos;
   sep:= Datarecord.slitsep;
   Datarecord.state:= go;
   WITH GraphScales [5] DO Xconst  := (xmax - xmin) / nnn;
   IF Datarecord.transform <> inverse THEN
      FOR i:=0 TO nnn DO BEGIN  Areal[i]:=0.0;  Bimage[i]:=0; END;
   CASE Datarecord.transform OF
      pulse1 : FOR i:= 256 - wid + pos TO 256 + wid + pos DO Areal[i]:= 1;
      twin :   FOR i:= - wid TO wid DO BEGIN
                   Areal[256 - sep +i + pos]:= 1;
                   Areal[256 + sep +i + pos]:= 1;
               END;
      grating : FOR i:= 1 TO nnn DO
                   IF odd((i+pos) div wid) THEN Areal[i]:= 1;
      coswave : FOR i:= 0 TO nnn DO
                   Areal[i]:= cos(i * 2 * pi/wid + 2 * pi * pos/wid);
      step : BEGIN
             FOR i:= 256 - wid + pos TO round(nnn/2) + pos DO
                Areal[i]:= -1;
             FOR i:= round(nnn/2) + 1 + pos TO 256 + wid + pos DO
                Areal[i]:= 1;
             END;
      sawtooth1 : BEGIN
                     k:= round(nnn/2) - wid;
                     FOR i:= k TO round(nnn/2) - 1 DO
                        Areal[i + pos]:=(i - k)/((nnn/2) - k);
                     k:= round(nnn/2) + wid;
                     FOR i:= round(nnn/2) + 1 TO k DO
                        Areal[i + pos]:= (k-i)/((nnn/2) - k);
                  END;
      triangle1 : BEGIN
                     k:= round(nnn/2) - wid + pos;
                     FOR i:= k TO round(nnn/2) + pos DO
                        Areal[i]:= (i-k)/wid;
                     k:= round(nnn/2) + wid + pos;
                     FOR i:= round(nnn/2)+1 + pos TO k DO
                        Areal[i]:= (k - i)/wid;
               END;
      Gaussian : For i:= - pos TO MaxSub - pos DO
                       Areal[i+pos]:= exp(-SQR((i-256)/wid));
      fnctn : IF FInput THEN Fft1D.Evaluate (MaxSub)
                 ELSE BEGIN
                    Announce ('Error:  Expression can not be evaluated');
                    Datarecord.state:= nogo;
                 END;
      drawn : BEGIN
                 MouseDraw;
                 Menu.Display;
              END;
      inverse :{do nothing Areal and Bimage already set};
              ELSE Datarecord.state:= nogo;
           END;
   IF  Datarecord.state = go THEN Plotfft_1d(lightgreen);
END;

Procedure Fft1D.Pack1Ddata;
VAR
   ii,jj,n : integer;
BEGIN
   ii:=(nnn DIV 2) - 1;
   jj:=ii + 2;
   FOR n:=1 TO jj DO
   BEGIN
      Data1D[n*2 - 1]:= Areal[ii + n];
      Data1D[n*2]:= Bimage[ii + n];
   END;
   FOR n:=jj TO nnn DO
   BEGIN
      Data1D[n*2 - 1]:= Areal[n - jj];
      Data1D[n*2]:= Bimage[n - jj];
   END;
END;{end of pack data}


Procedure Fft1D.FastFourierTransform(isign1D : integer);


{PROCEDURE four1(VAR Data1D: array[1..MaxSubX2] of real;
		nn,isign:integer);  }
  {Replaces data in Data1D by its discrete/inveres discrete Fourier Transform
		if isign is	1/  -1.  nn must be power of 2}
VAR
   ii,jj,n,mmax,m,j,i,istep:integer;
   wtemp,wr,wi,wpr,wpi, theta:real;
   tempr,tempi, wrs,wis: real;
BEGIN
   n:=2*nnn;
   j:=1;
   {Bit Reversal}
   FOR ii:= 1 TO nnn DO BEGIN
      i:=2*ii - 1;
      IF j > i THEN BEGIN
            {Exchange the two complex numbers}
	    tempr:=Data1D[j];	   tempi:=Data1D[j+1];
	    Data1D[j]:=Data1D[i];  Data1D[j+1]:=Data1D[i+1];
	    Data1D[i]:=tempr;	   Data1D[i+1]:=tempi;
         END;
	 m:=n DIV 2;
	 WHILE (m>=2) AND (j>m) DO BEGIN
            j:=j-m;
            m:=m DIV 2;
	 END;
	 j:=j+m;
      END;
      mmax:=2;
      WHILE n>mmax DO BEGIN       {log nnn (base 2) times}
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
	    tempr:=wrs * Data1D[j] - wis * Data1D[j+1];
	    tempi:=wrs * Data1D[j+1] + wis * Data1D[j];
	    Data1D[j]:=Data1D[i] - tempr;
	    Data1D[j+1]:=Data1D[i+1] - tempi;
	    Data1D[i] := Data1D[i]  +  tempr;
	    Data1D[i+1]:= Data1D[i+1]+ tempi;
	 END;
	 wtemp:=wr;	{Trig recurrence}
	 wr:=wr*wpr - wi*wpi   + wr;
	 wi:= wi*wpr + wtemp*wpi +wi;
      END;
      mmax:= istep;
   END;
   IF isign1D < 0 THEN FOR j:= 1 TO MaxSubX2 DO Data1D[j]:= Data1D[j]/nnn;
END;{end of 1D FFT}

Procedure Fft1D.Unpack1Ddata;
VAR
   ii,jj,n : integer;
BEGIN
   ii:= (nnn DIV 2) - 1;
   jj:= ii + 2;
   FOR n:=1 TO jj DO
   BEGIN
      Areal[ii + n]:= Data1D[n*2 - 1];
      Bimage[ii + n]:= Data1D[n*2];
   END;
   FOR n:=jj TO nnn DO
   BEGIN
      Areal[n - jj]:= Data1D[n*2 - 1];
      Bimage[n - jj]:= Data1D[n*2];
   END;
END;{end of unpack 1D data}

Procedure Fft1D.Pack2DRow(iRow : integer);
VAR
   mid,n : integer;
BEGIN
   mid:=(nnn DIV 2);
   FOR n:=1 TO mid DO
   BEGIN
      Data1D[n*2 - 1]:= ARe.Value(iRow,n + mid);
      Data1D[n*2]:= BIm.Value(iRow,n + mid);
   END;
   FOR n:= mid + 1 TO nnn DO
   BEGIN
      Data1D[n*2 - 1]:= ARe.Value(iRow,n - mid);
      Data1D[n*2]:= BIm.Value(iRow,n - mid);
   END;
END;{end of pack row data}

Procedure Fft1D.Unpack2DRow(iRow : integer);
VAR
   mid,n : integer;
BEGIN
   mid:= (nnn DIV 2);
   FOR n:=1 TO mid DO
   BEGIN
      ARe.Put(iRow,n + mid,Data1D[n*2 - 1]);
      BIm.Put(iRow,n + mid,Data1D[n*2]);
   END;
   FOR n:= mid + 1 TO nnn DO
   BEGIN
      ARe.Put(iRow,n - mid,Data1D[n*2 - 1]);
      BIm.Put(iRow,n - mid,Data1D[n*2]);
   END;
END;{end of unpack row data}

Procedure Fft1D.Pack2DCol(jCol : integer);
VAR
   mid,n : integer;
BEGIN
   mid:=(nnn DIV 2);
   FOR n:=1 TO mid DO
   BEGIN
      Data1D[n*2 - 1]:= ARe.Value(n + mid,jCol);
      Data1D[n*2]:= BIm.Value(n + mid,jCol);
   END;
   FOR n:= mid + 1 TO nnn DO
   BEGIN
      Data1D[n*2 - 1]:= ARe.Value(n - mid,jCol);
      Data1D[n*2]:= BIm.Value(n - mid,jCol);
   END;
END;{end of pack column data}

Procedure Fft1D.Unpack2DCol(jCol : integer);
VAR
   mid,n : integer;
BEGIN
   mid:= (nnn DIV 2);
   FOR n:=1 TO mid DO
   BEGIN
      ARe.Put(n + mid,jCol,Data1D[n*2 - 1]);
      BIm.Put(n + mid,jCol,Data1D[n*2]);
   END;
   FOR n:= mid + 1 TO nnn DO
   BEGIN
      ARe.Put(n - mid,jCol,Data1D[n*2 - 1]);
      BIm.Put(n - mid,jCol,Data1D[n*2]);
   END;
END;{end of unpack column data}

Procedure Fft1D.New2DFFT(VAR isign2D : integer);
VAR
   iRow,jCol : integer;

BEGIN
   mydelay.Show;
   TsfmData.SetUpFourierTransform(maxCol);
   mydelay.Update;
   FOR iRow:= 1 TO nnn DO BEGIN
      TsfmData.Pack2DRow(iRow);
      TsfmData.FastFourierTransform(isign2D);
      TsfmData.Unpack2DRow(iRow);
      mydelay.Update;
   END;
   FOR jCol:= 1 TO nnn DO BEGIN
      TsfmData.Pack2DCol(jCol);
      TsfmData.FastFourierTransform(isign2D);
      TsfmData.Unpack2DCol(jCol);
      mydelay.Update;
   END;
   mydelay.Hide;
END;

Procedure Fft1D.PlotFourierTransform(color1,color2,color3 : word);
VAR
   xrange,max1dreal,max1dimag,phi : REAL;
   ip,pcolor,temp : integer;
   lastX,lastA,lastB,lastC,newX,newA,newB,newC : integer;

Begin
   xrange:=nnn DIV 2;
   IF DataRecord.yrange1d <> 0 THEN BEGIN
      SelectViewPort(6);
      SelectScale(6);
      DrawAxis(0.0,0.0,xrange/4,trunc(DataRecord.yrange1d)/2,black);
   END;
   IF DataRecord.Oldfft1_num <> DataRecord.fft1_num THEN BEGIN
      IF DataRecord.fft1_num = 4 THEN  DisplayLinearColorWheel;
      IF DataRecord.Oldfft1_num = 4 THEN OpenHiddenViewport(11);
   END;
   DataRecord.Oldfft1_num:= DataRecord.fft1_num;
   OpenViewPort(6);
   max1dreal:= 1.0;
   IF DataRecord.fft1_num IN [1..3] THEN BEGIN
      FOR ip:= 0 TO nnn DO BEGIN
         IF ABS(Areal[ip]) > max1dreal THEN max1dreal:= ABS(Areal[ip]);
         IF ABS(Bimage[ip]) > max1dreal THEN max1dreal:= ABS(Bimage[ip])
      END;
      max1dreal:= (max1dreal*1.1);
      DataRecord.yrange1d:=max1dreal;
      DefineScale(6,-xrange,+xrange,-max1dreal*1.1,+max1dreal*1.2);
      SelectScale(6);
      DrawAxis(0.0,0.0,xrange/4,trunc(max1dreal)/2,white);
      fft1_button.init;
      fft1_button.create(1,0.135,0.45,'Real transform');
      fft1_button.create(2,0.42,0.45,'Both');
      fft1_button.create(3,0.54,0.45,'Imaginary transform');
      fft1_button.number:= DataRecord.fft1_num;
      fft1_button.DrawAll;
      Setcolor(color1);
      Plotline(-(MaxSub div 2)*0.4,1.05*max1dreal,-(MaxSub div 2)*0.3,1.05*max1dreal);
      Setcolor(color2);
      Plotline((MaxSub div 2)*0.77,1.05*max1dreal,(MaxSub div 2)*0.87,1.05*max1dreal);
      Setcolor(white);
   END
   ELSE BEGIN
      FOR ip:= 0 TO nnn DO
      IF (sqr(Areal[ip]) + sqr(Bimage[ip])) > max1dreal THEN
         max1dreal:= (sqr(Areal[ip]) + sqr(Bimage[ip]));
      IF DataRecord.fft1_num = 4 THEN max1dreal:= sqrt(max1dreal);
      max1dreal:= (max1dreal*1.1);
      DataRecord.yrange1d:=max1dreal;
      DefineScale(6,-xrange,+xrange,0,+max1dreal*1.2);
      SelectScale(6);
      DrawAxis(0.0,0.0,xrange/4,trunc(max1dreal)/2,white);
   END;
   lastA:=MapY(Areal[0]);
   lastB:=MapY(Bimage[0]);
   IF DataRecord.fft1_num = 4 THEN lastC:= MapY(sqrt(Sqr(Areal[0]) + Sqr(Bimage[0])))
   ELSE lastC:=MapY(Sqr(Areal[0]) + Sqr(Bimage[0]));
   lastX:=MapX(-xrange);
   Setcolor(white);
   IF DataRecord.fft1_num = 4 THEN BEGIN
      Print(4,1,'Phase and amplitude plot');
      SelectViewPort(6);
   END;
   IF DataRecord.fft1_num = 5 THEN BEGIN
      Setcolor(color3);
      Plotline(-(MaxSub div 2)*0.33,1.1*max1dreal,-(MaxSub div 2)*0.43,1.1*max1dreal);
      Setcolor(white);
      Print(4,1,'Power Spectrum');
   END;
   HideMouse;
   FOR ip:=1 TO nnn DO
   BEGIN
      newX:=MapX(ip*Xconst - xrange);
      newA:=MapY(Areal[ip]);
      newB:=MapY(Bimage[ip]);
      SetColor(color1);
      IF DataRecord.fft1_num IN [1..2] THEN line(lastX,lastA,newX,newA);
      Setcolor(color2);
      IF DataRecord.fft1_num IN [2..3] THEN line(lastX,lastB,newX,newB);
      IF DataRecord.fft1_num = 4 THEN BEGIN
         phi:= ArcTan2(Areal[ip],Bimage[ip]);
         pcolor:= ColorWheel(phi,temp);
         Setcolor(pcolor);
         newC:=MapY(Sqrt(Sqr(Areal[ip]) + Sqr(Bimage[ip])));
         line(newX,MapY(0),newX,newC);
      END;
      IF DataRecord.fft1_num = 5 THEN BEGIN
         Setcolor(color3);
         newC:=MapY(Sqr(Areal[ip]) + Sqr(Bimage[ip]));
         line(lastX,lastC,newX,newC);
      END;
      lastX:=newX;
      lastA:=newA;
      lastB:=newB;
      lastC:=newC;
   END;
   ShowMouse ;
END;

{**************************************************************}

FUNCTION TargetFn(xvalue : REAL) : REAL;

BEGIN
   CASE DataRecord.wave OF
      square    : IF (xvalue<-Pi/2) OR (xvalue>Pi/2) THEN TargetFn:=-1
                     ELSE TargetFn:=1;
      triangle  : IF (xvalue<0) THEN TargetFn:=1+2*xvalue/Pi
                     ELSE TargetFn:=1-2*xvalue/Pi;
      sawtooth  : IF (xvalue<0) THEN TargetFn:= -1 - xvalue/Pi
                     ELSE TargetFn:= 1 - xvalue/Pi;
      half_rect : IF (xvalue > 0) THEN TargetFn:= abs(sin(xvalue))
                     ELSE TargetFn:=0.0;
      full_rect : TargetFn:= abs(sin(xvalue));
      pulse     : IF (xvalue<-DataRecord.PulseRatio*Pi) OR
                     (xvalue>DataRecord.PulseRatio*Pi) THEN TargetFn:=-1
                     ELSE TargetFn:=1;
      coeffs    : TargetFn:= 0.0;
   END; {case}
END;

PROCEDURE Zcoefficient(var Zcoef : REAL );

BEGIN
   CASE DataRecord.wave OF
      square    : Zcoef:= 0.0;
      triangle  : Zcoef:= 0.0;
      sawtooth  : Zcoef:= 0.0;
      half_rect : Zcoef:= 1/Pi;
      full_rect : Zcoef:= 2/Pi;
      pulse     : Zcoef:= -(1 - 2 *DataRecord.PulseRatio);{ Mark to period ratio}
      fnct      : Zcoef:= termF.Zintegrate(0);
      curve     : Zcoef:= termF.Zintegrate(0);
      coeffs    : Zcoef:= Datarecord.ABcoeffs[0,0];
   END; {case}
END;

PROCEDURE ABcoefficients(var Acoef,Bcoef : REAL ;termnum : INTEGER);

BEGIN
      CASE DataRecord.wave OF
      square    : BEGIN
                     IF NOT odd(termnum) THEN Acoef := 0.0 ELSE
                        IF termnum mod 4 = 3 THEN  Acoef := -4/Pi/termnum
                        ELSE  Acoef := 4/Pi/termnum;
                     Bcoef:= 0.0;
                  END;
      triangle  : BEGIN
                     IF odd(termnum) THEN Acoef:= 8/Pi/Pi/termnum/termnum
                     ELSE Acoef := 0.0;
                     Bcoef:= 0.0
                  END;
      sawtooth  : BEGIN
                     Acoef:= 0.0;
                     Bcoef:=  2/Pi/termnum;
                  END;
      half_rect : BEGIN
                     IF odd(termnum) THEN Acoef := 0.0
                        ELSE Acoef := -2/Pi/(termnum*termnum-1);
                     IF (termnum = 1) THEN  Bcoef:= 0.5
                        ELSE  Bcoef:=0.0;
                  END;
      full_rect : BEGIN
                     IF odd(termnum) THEN   Acoef :=0.0
                        ELSE   Acoef := -4/Pi/(termnum*termnum - 1);
                     Bcoef:= 0.0;
                  END;
      pulse     : BEGIN
                     Acoef:= 4/Pi*sin(termnum*Pi*DataRecord.PulseRatio)/termnum;
                     Bcoef:= 0.0;
                  END;
      fnct      : BEGIN
                     Acoef:= termF.Aintegrate (termnum);
                     Bcoef:= termF.Bintegrate (termnum);
                  END;
      curve     : BEGIN
                     Acoef:= termF.Aintegrate (termnum);
                     Bcoef:= termF.Bintegrate (termnum);
                  END;
      Coeffs    : BEGIN
                     Acoef:= Datarecord.ABcoeffs[0,termnum];
                     Bcoef:= Datarecord.ABcoeffs[1,termnum];
                  END;
      END; {case}
END;

PROCEDURE Histogram(Hcoef : real; portnum,xpsn : word);
VAR
   xhist,yhist1,yhist2  : integer;

BEGIN
   SelectViewport(portnum);
   Selectscale(portnum);
   xhist:=MapX (xpsn);
   yhist1:= MapY (0.0);
   yhist2:= MapY (Hcoef);
   SetColor(lightgreen);
   line(xhist,yhist1,xhist,yhist2);
END;{Histogram}

PROCEDURE TCurveF.SetUpF (Zcoef : real; nData, Color1, Color2 : word);
VAR
   i : integer;
   Xfactor : REAL;

BEGIN
   WITH GraphScales [{ScaleNumber}1] DO Xconst  := (xmax - xmin) / nData;
      x[0] := MapX (-Pi);
      yr[0,0] := Zcoef;
      yp[0,0] := MapY (yr[0,0]);
      IF Datarecord.wave IN stdwave  THEN yr[0,2]:= TargetFn(-Pi);
      yp[0,2] := MapY (yr[0,2]);
   FOR i := 1 TO nData DO
   BEGIN
      x[i] := MapX (i*Xconst-Pi);
      yr[i,0] := Zcoef;
      yp[i,0] := MapY (yr[i,0]);
      IF Datarecord.wave IN stdwave  THEN yr[i,2]:= TargetFn(i*Xconst-pi);
      yp[i,2] := MapY (yr[i,2]);
      HideMouse ;
      SetColor(Color1);
      line(x[i-1],yp[i-1,0],x[i],yp[i,0]);
      SetColor(Color2);
      line(x[i-1],yp[i-1,2],x[i],yp[i,2]);
      ShowMouse ;{ new }
   END;
   SetColor (white);
   line (x[0], MapY(0), x[nData], MapY(0));
   Histogram(Zcoef,2,0);

END;

PROCEDURE TCurveF.DrawF (Acoef,Bcoef : real; Color,t : word);
VAR
   lasty : integer;
   i,n   : integer;

BEGIN
   termnum:=t;
   yr[0,1] := Acoef*cos(-Pi*t) + Bcoef*sin(-Pi*t);
   yp[0,1] := MapY (yr[0,1]);
   HideMouse ;
   FOR i := 1 TO DataRecord.numS DO
   BEGIN
      yr[i,1] := Acoef*cos((Xconst*i - Pi)*t) + Bcoef*sin((Xconst*i - Pi)*t);
      yp[i,1] := MapY (yr[i,1]);
      SetColor (Color);
      line (x[i-1],yp[i-1,1],x[i],yp[i,1]);
   END;
   SetColor (white);
   line (x[0], MapY(0), x[DataRecord.numS], MapY(0));
   WITH Views[ViewPortNumber] DO rectangle (0,0,Vx2-Vx1,Vy2-Vy1);
   ShowMouse ;
END;


PROCEDURE TCurveF.Draw_Sum (VAR MaxDif : REAL; Color : word);

VAR
   lastY,i,n,weight : integer;

BEGIN
   lastY := yp[0,0];
   yr[0,0]  := yr[0,0] + yr[0,1];
   yp[0,0] := MapY(yr[0,0]);
   MaxDif:= sqr(yr[0,0] - yr[0,2]);
   HideMouse ;
   FOR i := 1 TO DataRecord.numS DO
   BEGIN
      SetColor (GraphBackColor);
      line (x[i-1],lastY,x[i],yp[i,0]); {rub out old}
      line (x[i-1],yp[i-1,1],x[i],yp[i,1]); {rub out term}
      lastY := yp[i,0];
      yr[i,0] := yr[i,0] + yr[i,1];
      yp[i,0] := MapY (yr[i,0]);
      SetColor (Color);
      line (x[i-1],yp[i-1,0],x[i],yp[i,0]);
      IF i mod 2 = 1 THEN weight:= 4 ELSE weight:= 2;
      MaxDif:=MaxDif + weight * sqr(yr[i,0] - yr[i,2]);
   END;
   n:=DataRecord.numS;
   MaxDif:= MaxDif - sqr(yr[n,0] - yr[n,2]);
   MaxDif:= MaxDif/3/n;{Simpson's rule integration of difference sqrd}
   IF Datarecord.wave <> coeffs THEN MaxDif:=sqrt(MaxDif) ELSE MaxDiF:=0;
   SetColor (white);
   line (x[0], mapY(0), x[n], mapY(0));
   WITH Views[ViewPortNumber] DO rectangle (0,0,Vx2-Vx1,Vy2-Vy1);
   ShowMouse ;
END;

PROCEDURE TCurveF.Draw_Evolve (VAR ephase : real; Color : word);

VAR
   lastY,i,n,weight,drpc : integer;

BEGIN
   IF DataRecord.PhaseCount < DataRecord.PhaseStep THEN BEGIN
      mydelay.Update;
      termf.ReStartPropagateWave (lightgreen,ephase); {calculates curve}
      ephase:= ephase + 2*pi/DataRecord.PhaseStep;
      IF DataRecord.PhaseCount in [0..19] THEN
         FOR i:=  0 TO 200 DO Yvals1.Put(DataRecord.PhaseCount+1,i+1,yr[i,0]);
      IF DataRecord.PhaseCount in [20..39] THEN
         FOR i:=  0 TO 200 DO Yvals2.Put(DataRecord.PhaseCount-19,i+1,yr[i,0]);
      IF DataRecord.PhaseCount in [40..59] THEN
         FOR i:=  0 TO 200 DO Yvals3.Put(DataRecord.PhaseCount-39,i+1,yr[i,0]);
   END
   ELSE BEGIN
      IF DataRecord.PhaseCount = DataRecord.PhaseStep THEN mydelay.Hide;
      drpc:=  DataRecord.PhaseCount MOD DataRecord.PhaseStep;
      IF drpc in [0..19] THEN
         FOR i:=  0 TO 200 DO yr[i,0]:= Yvals1.value(drpc+1,i+1);
      IF drpc in [20..39] THEN
         FOR i:=  0 TO 200 DO yr[i,0]:= Yvals2.value(drpc-19,i+1);
      IF drpc in [40..59] THEN
         FOR i:=  0 TO 200 DO yr[i,0]:= Yvals3.value(drpc-39,i+1);
   END;
   DataRecord.PhaseCount:= DataRecord.PhaseCount + 1;
   lastY := yp[0,1];
   yr[0,0]  := yr[0,0] + yr[0,1];
   yp[0,0] := MapY(yr[0,0]);
   yp[0,1]:= yp[0,0];
   HideMouse ;
   FOR i := 1 TO DataRecord.numS DO
   BEGIN
      SetColor (GraphBackColor);
      line (x[i-1],lasty,x[i],yp[i,1]); {rub out term}
      lastY := yp[i,1];
      yr[i,0] := yr[i,0] + yr[i,1];
      yp[i,0] := MapY (yr[i,0]);
      yp[i,1] := yp[i,0];
      SetColor (Color);
      line (x[i-1],yp[i-1,0],x[i],yp[i,0]);
   END;
   SetColor (white);
   line (x[0], mapY(0), x[DataRecord.numS], mapY(0));
   WITH Views[ViewPortNumber] DO rectangle (0,0,Vx2-Vx1,Vy2-Vy1);
   ShowMouse ;
END;


PROCEDURE TCurveF.ReStartPropagateWave (Color : word; phase : real);
VAR
   i1,i2 : INTEGER;
   ysum, phaseval, angle : REAL;
BEGIN
   SetColor (Color);
   x[0] := MapX(-Pi);
   ysum:= 0;
   FOR i2:= 0 TO Datarecord.numC DO
      ysum:= ysum +
             Datarecord.ABcoeffs[0,i2]*cos(( - Pi)*i2)*cos(phase*i2)
             + Datarecord.ABcoeffs[1,i2]*sin(( - Pi)*i2)*cos(phase*i2);
   yr[0,0]  := ysum;
   yp[0,0] := MapY(yr[0,0]);
   FOR i1:= 1 TO DataRecord.numS DO BEGIN
      x[i1] := MapX(i1*Xconst-Pi);
      ysum:= 0;
      FOR i2:= 0 TO Datarecord.numC DO begin
      IF (ABS(Datarecord.ABcoeffs[0,i2]) > 1E-5)  OR
          (ABS(Datarecord.ABcoeffs[1,i2]) > 1E-5) THEN BEGIN
            phaseval:= cos(phase*i2);
            angle:= (i1*Xconst - Pi)*i2;
            ysum:= ysum + Datarecord.ABcoeffs[0,i2]*cos(angle)*phaseval
                + Datarecord.ABcoeffs[1,i2]*sin(angle)*phaseval;
         END;
      END;
      yr[i1,0]  := ysum;
      yp[i1,0] := MapY (yr[i1,0]);
      IF DataRecord.wave = propagate THEN
         line (x[i1-1],yp[i1-1,0],x[i1],yp[i1,0]);
   END;
END;

PROCEDURE TCurveF.PropagateWave (Color : word);

VAR
   lastY,i,nS,weight : integer;
   ysum, Xepoch, Xtime, ACauchy, BCauchy, CCauchy, Atten_func : real;

BEGIN
   Atten_func:=1;
   SelectViewport(1);
   SelectScale(1);
   lastY := yp[0,0];
   yr[0,0]  := yr[1,0];
   yp[0,0] := MapY(yr[0,0]);
   HideMouse ;
   nS:= DataRecord.numS;
   FOR i := 1 TO nS - 1 DO
   BEGIN
      SetColor (GraphBackColor);
      line (x[i-1],lastY,x[i],yp[i,0]); {rub out old}
      lastY := yp[i,0];
      yr[i,0] := yr[i + 1,0];
      yp[i,0] := MapY (yr[i,0]);
      SetColor (Color);
      line (x[i-1],yp[i-1,0],x[i],yp[i,0]);
   END;
   SetColor (GraphBackColor);
   line (x[nS-1],lastY,x[nS],yp[nS,0]); {rub out old}
   SetColor (Color);
   ysum:= 0;
   ACauchy:= Datarecord.Cauchy_A;
   BCauchy:= Datarecord.Cauchy_B;
   atten_func:= Datarecord.AttenCoeff;
   Xtime:= Xconst*Datarecord.time;
   FOR i:= 0 TO Datarecord.numC DO BEGIN
      Xepoch:= ACauchy/(ACauchy + BCauchy*i*i);
      atten_func:= Datarecord.AttenCoeff * atten_func;
      Datarecord.ABcoeffs[2,i]:= Datarecord.ABcoeffs[2,i]*atten_func;
      Datarecord.ABcoeffs[3,i]:= Datarecord.ABcoeffs[3,i]*atten_func;
      ysum:= ysum +
             Datarecord.ABcoeffs[2,i]*cos((Xtime/Xepoch - Pi)*i)
             + Datarecord.ABcoeffs[3,i]*sin((Xtime/Xepoch - Pi)*i);
   END;
   Datarecord.time:= Datarecord.time + 1;
   yr[nS,0]:= ysum;
   yp[nS,0]:= MapY (yr[nS,0]);
   line (x[nS-1], yp[nS-1,0], x[nS], yp[nS,0]);
   SetColor (white);
   WITH Views[ViewPortNumber] DO rectangle (0,0,Vx2-Vx1,Vy2-Vy1);
   line (x[0], mapY(0), x[nS], mapY(0));
         IF (Datarecord.PropScale = true)
           OR (((Datarecord.time mod 32) = 0) AND (Datarecord.time > 0 )) THEN BEGIN
            Selectscale(10);
            DrawAxis(-4,-2,1,0.5,black);
            DefineScale(10,xtime-Pi,xtime+Pi,-1.3,+1.3);
            DrawAxis(-4,-2,1,0.5,white);
         END;
   ShowMouse ;
END;

PROCEDURE TcurveF.Evaluate ( nData : word);
   VAR
      hinc : REAL;
        i  : INTEGER;
      myout,rx : Complex;
BEGIN
   HaltIfError := false;
   ErrorFound  := false;
   hinc:= 2*Pi/nData;
   rx.im:= 0;
   i:= 0;
   WHILE (i <= nData) AND (DataRecord.state <> nogo) and (not ErrorFound) DO
   BEGIN
      rx.Re:= i*hinc - pi;
      FourierFunc.Cxf(rx,Cxzero,Cxzero,Cxzero,myout);
      yr[i,2]:= myout.Re;
      IF (i>0) AND (ABS(yr[i-1,2]) > 0) AND (ABS(yr[i,2]) > 0) THEN BEGIN
         IF ((yr[i-1,2]/yr[i,2] < 0) AND (ABS(yr[i-1,2]) > 10)) OR
            (ABS(yr[i,2]) > 1.0e15) THEN BEGIN
               announce('Fourier analysis not possible for this function');
               DataRecord.state:= nogo;
            END;
      END;
   I:= i + 1;
   END;
   if ErrorFound then DataRecord.state:= nogo;
   HaltIfError := true;
   ErrorFound  := false;
END;

FUNCTION TcurveF.Aintegrate(tnum : word) : REAL;
VAR
   sum,h,t : REAL;
   i : INTEGER;

BEGIN
   h:=2*Pi/DataRecord.numS;
   t:=-Pi;
   sum:=yr[0,2]*cos(tnum*t);
   FOR i:= 1 to DataRecord.numS DO
   BEGIN
      t:=t + h;
      sum:=sum + 4*yr[i,2]*cos(tnum*t);
      i:=i+1;
      t:=t+h;
      sum:=sum + 2*yr[i,2]*cos(tnum*t);
   END;
   sum:=sum - yr[DataRecord.numS,2]*cos(tnum*t);
   Aintegrate:=1/Pi*h/3*sum;
END;

FUNCTION TcurveF.Bintegrate(tnum : word) : REAL;
VAR
   sum,h,t : REAL;
   i : INTEGER;

BEGIN
   h:=2*Pi/DataRecord.numS;
   t:=-Pi;
   sum:=yr[0,2]*sin(tnum*t);
   FOR i:= 1 to DataRecord.numS DO
   BEGIN
      t:=t + h;
      sum:=sum + 4*yr[i,2]*sin(tnum*t);
      i:=i+1;
      t:=t+h;
      sum:=sum + 2*yr[i,2]*sin(tnum*t);
   END;
   sum:=sum - yr[DataRecord.numS,2]*sin(tnum*t);
   Bintegrate:=1/Pi*h/3*sum;
END;

FUNCTION TcurveF.Zintegrate(tnum : word) : REAL;
VAR
   sum,h,t : REAL;
   i : INTEGER;

BEGIN
   h:=2*Pi/DataRecord.numS;
   t:=-Pi;
   sum:=yr[0,2];
   FOR i:= 1 to DataRecord.numS DO
   BEGIN
      t:=t + h;
      sum:=sum + 4*yr[i,2];
      i:=i+1;
      t:=t+1;
      sum:=sum + 2*yr[i,2];
   END;
   sum:=sum - yr[DataRecord.numS,2];
   Zintegrate:=1/Pi*h/6*sum;
END;

PROCEDURE SetDisplayScreen (VAR Datarecord : Tdata);
VAR
   i : integer;
   dataScreen : TInputScreen;
BEGIN
with dataScreen DO
   BEGIN
      init;
      DefineInputPort(0.1,0.9,0.1,0.9);
      LoadLine('');
      LoadLine('Fourier Analysis - Control of Display');
      LoadLine('');
      LoadLine('The maximum number of harmonic terms that are evaluated');
      LoadLine('and which have their contributions added to the Fourier');
      LoadLine('synthesised waveform in RUN mode is initially set to 40.');
      LoadLine('Maximum number of harmonics, T = {      }(10-80)');
      LoadLine('');
      LoadLine('The choice of the number of points at which the Fourier');
      LoadLine('series is evaluated is a compromise  between the time taken');
      LoadLine('to calculate each term at all points and obtaining adequate');
      LoadLine('resolution. Larger values give better resolution but take');
      LoadLine('longer to plot.  The number of points should be roughly four');
      LoadLine('times the number of harmonics and should be an even number.');
      LoadLine('Number of points (at least 4 * T) {      } (100-400)');
      LoadLine('');
      LoadLine('                 [  Ok  ]   [Cancel]');
      SetNumber(1,Datarecord.numT);
      SetNumber(2,Datarecord.numS);
      SetNumberLimits(1,10,80);
      SetNumberLimits(2,100,400);
      AcceptScreen;
      IF Canceled THEN Announce('The old Fourier ''Display Setup'' has been retained.')
      ELSE WITH Datarecord DO BEGIN
         numT := ABS(round(GetNumber(1)));
         numS := ABS(round(GetNumber(2)));
         IF numS MOD 2 <> 0 THEN numS:= numS + 1;
         IF numS > 400 THEN numS:= 400;
         IF numS < 100 THEN numS:= 100;
         IF numT > numS/4 THEN numS:= numT * 4;
         IF numT < 10 THEN numT:= 10;
         numTset:= numT;
         numSset:= numS;
      END;
      done;
   END;
END;

PROCEDURE PropagateDisplayScreen (VAR Datarecord : Tdata);
VAR
   mycanceled : BOOLEAN;
   id, RadioChoice : integer;
   dataScreen : TInputScreen;
BEGIN
with dataScreen DO
   BEGIN
      init;
      DefineInputPort(0.08,0.92,0.1,0.6);
      LoadLine('The effects of dispersion and attenuation on the propagated wave. ');
      LoadLine('');
      LoadLine('Cauchy A coefficient {          }   ( between 1 and 2 )');
      LoadLine('Cauchy B coefficient {          } (a small or zero value )');
      LoadLine('Attenuation factor   {          } ( between 0.0 and 0.1)');
      LoadLine('');
      LoadLine('The effects of attenuation and propagation on the wave may');
      LoadLine('be shown with continuous re-scaling of the graph     -  #1 ');
      LoadLine('or with re-scaling of the graph at regular intervals -  #1 ');
      LoadLine('');
      LoadLine('               [  Ok  ]  [Cancel]  [Help]                                  ');
      SetNumber(1,Datarecord.Cauchy_A);
      SetNumber(2,Datarecord.Cauchy_B);
      SetNumber(3,(1-Datarecord.AttenCoeff)*100);
      SetNumberLimits(1,1.0,2.0);
      SetNumberLimits(2,-0.1,+0.1);
      SetNumberLimits(3,0.0,0.1);
      IF Datarecord.PropScale = true THEN SetRadioButton('1',1)
      ELSE SetRadioButton('1',2);
      SetHelpFile('Fourier.hlp','Help 5');
      AcceptScreen;
      SelectViewport(1);
      SetColor(white);
      IF Canceled THEN BEGIN
         mycanceled:= true;
         Continue_F10;(*Announce('Continue, or press F10 to make a NEW choice from the menu.');*)
         IF Datarecord.wave = propagate THEN
            PutLabel(top,'                                      Fourier Analysis - Propagation')
         ELSE IF Datarecord.wave = evolve THEN
            PutLabel(top,'                                      Fourier Analysis - Evolution')
            ELSE
            PutLabel(top,'                                        Fourier Analysis ');
      END
      ELSE WITH Datarecord DO BEGIN
         mycanceled:= false;
         Cauchy_A:= GetNumber(1);
         Cauchy_B:= GetNumber(2);
         AttenCoeff:= 1-GetNumber(3)/100;
         Datarecord.time:= 0;
         IF Datarecord.wave = propagate THEN  Announce('Restarted with new values.');
         FOR id:= 0 TO 100 DO BEGIN
            Datarecord.ABcoeffs[2,id]:= Datarecord.ABcoeffs[0,id];
            Datarecord.ABcoeffs[3,id]:= Datarecord.ABcoeffs[1,id];
         END;
         RadioChoice:= GetRadioButton('1');
         IF RadioChoice = 1 THEN Datarecord.PropScale:= true
         ELSE Datarecord.PropScale:= false;
         Datarecord.wave:= propagate;
         PutLabel(top,'                                      Fourier Analysis - Propagation');
      END;
      done;
      IF NOT mycanceled THEN BEGIN
         IF Datarecord.wave = propagate THEN BEGIN
             Openviewport(1);
             Selectscale(1);
             termf.ReStartPropagateWave (lightgreen,0);
             Selectscale(10);
         END
         ELSE Selectscale(1);
         DrawAxis(0.0,0.0,1,0.5,black);
         setcolor(graphbackcolor);
         Line(MapX(0),MapY(1.3),MapX(0),MapY(-1.3));
         DefineScale(10,-Pi,+Pi,-1.3,+1.3);
         Selectscale(10);
         DrawAxis(0.0,0.0,1,0.5,white);
      END;
   END;
END;

PROCEDURE YvalsInit;

BEGIN
   IF Datarecord.mode <> initial THEN
   begin ARe.Free; BIm.Free; Cmodz.Free; Power.Free; end;
   Yvals1.Init(20,Datarecord.numS + 1);
   Yvals2.Init(20,Datarecord.numS + 1);
   Yvals3.Init(20,Datarecord.numS + 1);
END;

PROCEDURE EvolveDisplayScreen (VAR Datarecord : Tdata);
VAR
   mycanceled : BOOLEAN;
   id, RadioChoice : integer;
   EdataScreen : TInputScreen;

BEGIN
with EdataScreen DO
   BEGIN
      init;
      DefineInputPort(0.08,0.92,0.3,0.9);
      LoadLine(' Evolve function');
      Loadline('');
      Loadline(' The function that has been Fourier analysed may be regarded as');
      Loadline(' the intial displacement curve of a standing wave.');
      Loadline(' The temporal variation of this standing wave is displayed by');
      Loadline(' calculating the displacement at regular time intervals. The');
      Loadline(' program will calculate each step in the cycle and then display');
      Loadline(' an animated simulation. ');
      Loadline('');
      Loadline(' Number of time steps in one cycle of the wave {   } (20 to 60)');
      Loadline('');
      Loadline(' A large number of steps will give a smoother animation.');
      Loadline('');
      LoadLine('                   [  Ok  ]   [Cancel] ');
      SetNumber(1,60);
      SetNumberLimits(1,20,60);
      AcceptScreen;
      SelectViewport(1);
      SetColor(white);
      IF Canceled THEN BEGIN
         mycanceled:= true;
         Continue_F10;(*Announce('Continue, or press F10 to make a NEW choice from the menu.');*)
         IF Datarecord.wave = evolve THEN
            PutLabel(top,'                                      Fourier Analysis - Evolution')
         ELSE IF Datarecord.wave = propagate THEN
            PutLabel(top,'                                      Fourier Analysis - Propagation')
            ELSE
            PutLabel(top,'                                        Fourier Analysis ');
      END
      ELSE WITH Datarecord DO BEGIN
         mycanceled:= false;
         Datarecord.time:= 0;
         Datarecord.state:= AUTO;
         DataRecord.PhaseCount:= 0;
         DataRecord.PhaseStep:= round(GetNumber(1));
         IF Datarecord.wave = evolve THEN  Announce('Restarted with new values.');
         FOR id:= 0 TO 100 DO BEGIN
            Datarecord.ABcoeffs[2,id]:= Datarecord.ABcoeffs[0,id];
            Datarecord.ABcoeffs[3,id]:= Datarecord.ABcoeffs[1,id];
         END;
         Datarecord.wave:= evolve;
         PutLabel(top,'                                      Fourier Analysis - Evolution');
      END;
      done;
      IF NOT mycanceled THEN BEGIN
         IF Datarecord.wave = evolve THEN BEGIN
             Openviewport(1);
             Mydelay.Show;
             Selectscale(1);
             termf.ReStartPropagateWave (lightgreen,0);

             Selectscale(10);
         END
         ELSE Selectscale(1);
         DrawAxis(0.0,0.0,1,0.5,black);
         DefineScale(10,-Pi,+Pi,-1.3,+1.3);
         Selectscale(10);
         DrawAxis(0.0,0.0,1,0.5,white);
         evolvephase:= 0.0;
         termf.Draw_Evolve (evolvephase, lightgreen);
      END;
   END;
END;


Procedure CoeffInput(VAR lowterm : word; VAR DataRecord : Tdata);
VAR
   i,j,k : integer;
   CoeffScreen : TInputScreen;
   tStr1,tStr2 : string;

BEGIN
with CoeffScreen DO
   BEGIN
      init;
      DefineInputPort(0.1,0.9,0.1,0.9);
      IF lowterm = 1 THEN tStr1:='0' ELSE tStr1:= NumStr(lowterm,2,0);
      IF (lowterm + 12) <=  DataRecord.numT THEN k:= 11
         ELSE k:= DataRecord.numT - lowterm;
      tStr2:= NumStr(lowterm + k,3,0);
      LoadLine('Fourier Synthesis  -  Enter Coefficients  for terms '+tStr1+' to '+tStr2+'.');
      LoadLine('INITIAL VALUES FROM LAST WAVEFORM or DEFAULT SET');
      IF lowterm = 1 THEN BEGIN
         j:= 2;
         LoadLine('Number of terms to be shown {      }  1 to 99');
         LoadLine('Zero coefficient  {       }  (-1.0 to 1.0)');
      END
      ELSE BEGIN
         j:= 0;
         tStr1:= NumStr(DataRecord.numT,3,0);
         LoadLine(' ');
         LoadLine('Number of terms to be shown '+tStr1+'.');
      END;
      LoadLine('Enter values in the range   -1.0 to 1.0 ');
      FOR i := 0 TO k DO BEGIN
         tStr1 := NumStr(lowterm + i,2,0);
         LoadLine('Coefficients A('+tStr1+') and B('+tStr1+')   {        } {        } ');
      END;
      LoadLine('');
      LoadLine('   [  Ok  ]  [Cancel] ');
      IF lowterm = 1 THEN BEGIN
         IF Datarecord.numC <= 1 THEN SetNumber(1,12)
         ELSE SetNumber(1,Datarecord.numC);
         SetNumber(2,Datarecord.ABcoeffs[0,0]);
         SetNumberLimits(1,1,99);
      END;
      FOR i:= 0 TO k DO
      BEGIN
         SetNumber(i*2+j+1,Datarecord.ABcoeffs[0,lowterm + i]);
         SetNumber(i*2+j+2,Datarecord.ABcoeffs[1,lowterm + i]);
      END;
      AcceptScreen;
      IF Canceled THEN Announce('The old values have been retained.')
      ELSE BEGIN
         IF lowterm =  1 THEN BEGIN
            Datarecord.numT:= ABS(round(GetNumber(1)));
            Datarecord.ABcoeffs[0,0] := GetNumber(2);
         END;
         FOR i:= 0 TO k DO
         BEGIN
            Datarecord.ABcoeffs[0,lowterm + i] := GetNumber(i*2+j+1);
            Datarecord.ABcoeffs[1,lowterm + i] := GetNumber(i*2+j+2);
         END;
      END;
      done;
   END;
END;

PROCEDURE SlitDist(VAR NumCh : INTEGER);
VAR
   lo : integer;
   low,dstr : string;

BEGIN
   dstr:= 'Width';  {pulse1,step,sawtooth1,triangle1,Gaussian}
   IF Datarecord.transform IN [coswave,grating] THEN  dstr:= 'Wlgth';
   IF Datarecord.transform IN [coswave, grating,sawtooth1,triangle1,Gaussian] THEN
   BEGIN
      lo:= 4;
      low:= '4';
   END
   ELSE BEGIN
      lo:= 0;
      low:= '0';
   END;
   sliders.init;
   sliders.create(3,  lo ,64,NumCh, 0.95,0.5,0.93,0,low,'64',dstr,true);
   sliders.create(4,  0 ,64,NumCh, 0.03,0.5,0.93,0,'0','64','Posn.',true);
   IF Datarecord.transform = twin THEN
      sliders.create(5,lo,64,12{NumCh}, 0.95,0.06,0.47,0,low,'64','Sepn.',true);
   sliders.drawAll;
END;

{*************** FFT 2D ********************}

PROCEDURE StdVGAcolors;
VAR   id,kd : word;

BEGIN
   FOR id:=0 to 15 DO BEGIN
      kd:=rgbpal[id];
      setrgbpalette(kd,oldpalette[1,id],oldpalette[2,id],oldpalette[3,id]);
   END;
END;

PROCEDURE UpdateContour(VAR Upmatrix : Dmatrix);
VAR
   min,max : point3D;
   col,row : integer;
BEGIN
   OpenViewPort(9);
   surface.Get3DScales(min,max);
   DefineScale(7 ,min[1],max[1], min[2],max[2]);
   contour.DrawMatrix(Upmatrix);
   PutLabel(top,' 2-D Fourier Transform');
END;

PROCEDURE Draw2DFT(VAR matrix : Dmatrix; plottype: Tplot);
VAR
   id,jd,iid,jjd : INTEGER;
   pmin,pmax : real;
   tStr : string;
   contht : HeightType;
BEGIN
   SetColor(white);
   matrix.MinMax(id,jd,pmin,iid,jjd,pmax);
   StdVGAcolors;
   OpenViewPort(7);
   surface.init;
   surface.Set3DScales(-2,2, -1.7,1.7, 0,1);
   Surface.SetViewAngles(alpha,beta,gamma);
   surface.DrawBox;
   IF (pmax - pmin) < 0.1 THEN BEGIN
      surface.Set3DScales(-2,2, -1.7,1.7, -0.07,0.07);
      FOR id:= 1 TO 14 DO contht[id]:= 0.01*id -0.06;
      contour.SetHeights(14,contht);
   END
   ELSE BEGIN
      surface.Set3DScales(-2,2, -1.7,1.7, 0,0);
      contour.init;
   END;
   surface.DrawSurface(matrix);
   Print(3,1, Datarecord.datastr2+Datarecord.datastr1);
   tStr:= NumStr(pmin,5,3);
   Print(2,20,'minimum value '+tStr);
   tStr:= NumStr(pmax,5,3);
   Print(30,20,'maximum value '+tStr);
   IF Datarecord.upcont AND (Datarecord.ContourDrawn <> plottype) THEN
      UpdateContour(matrix);
   Datarecord.ContourDrawn:= plottype;
END;

PROCEDURE SetUpMatrix;
VAR
   min,max : point3D;
   XFactor,YFactor,cz : real;
   col,row : integer;
BEGIN
   surface.Get3DScales(min,max);
   Xfactor := (max[1]-min[1])/(maxCol-1);
   Yfactor := (max[2]-min[2])/(maxRow-1);
   Draw2DFT(ARe,Re);
   FOR row:= 1 TO maxRow DO          {loops to get modz}
      FOR col:= 1 TO maxCol DO BEGIN
         cz:= SQR(ARe.Value(row,col)) + SQR(BIm.Value(row,col));
         Power.Put(row,col,cz);
         cz:= SQRT(cz);
        { IF (ARe.value(row,col) < 0) AND Picture THEN cz:= - cz; }
         Cmodz.Put(row,col,cz);   {mod z takes sign of real part}
      END                         {when a picture is loaded}
END;


PROCEDURE HandleSliders;

BEGIN
   CASE Datarecord.mode OF

   fft_1d : BEGIN
               Datarecord.slitwidth:= round(sliders.value(3));
               Datarecord.slitpos:= round(sliders.value(4));
               IF Datarecord.transform = twin THEN
                  Datarecord.slitsep:= round(sliders.value(5));
               IF Datarecord.transform IN [pulse1,twin,grating,Gaussian,
                  sawtooth1,triangle1,coswave,step] THEN
                     TsfmData.StdDataForFourierTransform(Datarecord);
            END;

   fft_2d : BEGIN
               SelectViewPort(8);
               SetWriteMode (XorPut);
               SmallBox.DrawBox;
               alpha:=sliders.value(1);
               beta:=sliders.value(2);
               smallBox.SetViewAngles(alpha,beta,gamma);
               smallBox.DrawBox;
               SetWriteMode (CopyPut);
               Delay(20);
            END;
   END;{case}
end;

PROCEDURE SquareHole (VAR A : Dmatrix; xypos1, xypos2, xydim1, xydim2 : INTEGER;
                       zvalue : REAL);
VAR
   ir,ic,iph,jph : INTEGER;
BEGIN
   ir:=maxRow DIV 2;
   ic:=maxCol DIV 2;
   FOR iph:= - xydim2 + 1 + xypos2 TO xydim2 + 1 + xypos2 DO
      FOR jph:= - xydim1 + 1 + xypos1 TO xydim1 + 1 + xypos1 DO
         IF (ir+iph>0) AND (ir+iph<=maxRow) AND (ic+jph>0) AND (ic+jph<=maxCol)
             THEN A.Put(ir + iph,ic + jph,zvalue);
END;

PROCEDURE SquareFrame (VAR A : Dmatrix; extent : INTEGER; zvalue : REAL);
VAR
   iph,jph : INTEGER;
BEGIN
   FOR iph:= 1 TO extent DO
      FOR jph:= 1 TO maxcol DO A.Put(iph,jph,zvalue);
   FOR iph:= maxRow - extent+1 TO maxRow DO
      FOR jph:= 1 TO maxcol DO A.Put(iph,jph,zvalue);
   FOR iph:= 1 TO maxRow DO
      FOR jph:= 1 TO extent DO A.Put(iph,jph,zvalue);
   FOR iph:= 1 TO maxRow DO
      FOR jph:= maxCol - extent+1 TO maxCol DO A.Put(iph,jph,zvalue);
END;

PROCEDURE Circle2DFT (VAR A : Dmatrix; xp, yp, extent : INTEGER;
                                    zvalue : REAL; lowpass : BOOLEAN);
VAR ic,ir,icrow,iccol,xsqr : integer;

BEGIN
   ir:= (maxRow DIV 2) + 1 + yp;
   ic:= (maxCol DIV 2) + 1 + xp;
   xsqr:= sqr(extent);
   FOR icrow:= 1 TO maxRow DO
      FOR iccol:= 1 TO maxCol DO BEGIN
          IF (trunc(sqr(icrow - ir) + sqr(iccol - ic)) <= xsqr) AND
              NOT(lowpass) THEN A.Put(icrow,iccol,zvalue);  {high pass}
          IF (trunc(sqr(icrow - ir) + sqr(iccol - ic)) > xsqr) AND
              lowpass THEN A.Put(icrow,iccol,zvalue);   {low pass}
      END;
END;

PROCEDURE Gauss2DFT (VAR A : Dmatrix; xc, yc, sigma :integer);
VAR
   ig,jg,ic,ir : integer;

BEGIN
   sigma:= SQR(sigma);
   ic:= MaxCol DIV 2;
   ir:= MaxRow DIV 2;
   For jg:= 1 TO MaxRow DO BEGIN
      For ig:= 1 TO MaxCol DO
      A.Put(ig,jg, exp(-(SQR(ig-ic-xc)+SQR(jg-ir-yc))/sigma));
   END;
END;

PROCEDURE Checks (VAR A : Dmatrix; CheckPattern : BOOLEAN);
VAR
   k1,k2,iph,jph : INTEGER;
   zvalue :REAL;
BEGIN
   FOR iph:= 1 TO maxRow DO
      FOR jph:= 1 TO maxCol DO BEGIN
         k1:= ((iph +1 ) div 4) mod 2;
         k2:= ((jph + 1) div 4) mod 2;
         IF CheckPattern THEN zvalue:= k2 + k1 ELSE zvalue:= abs(k2 - k1);
         A.Put(iph,jph,zvalue);
      END;
END;

PROCEDURE TwoPins (VAR A : Dmatrix);
VAR
   k1,k2,iph,jph,kph : INTEGER;
   zvalue :REAL;
BEGIN
   k1:= round(maxCol/2)-2;
   k2:= round(maxCol/2)+2;
   A.Put(k1,k1,10);
   A.Put(k2,k2,10);
END;

PROCEDURE MyCosWave (VAR A : Dmatrix);
VAR
   ic,jc : INTEGER;
   coeff : real;
BEGIN
   coeff:= (2*pi/maxCol)*3;
   FOR ic:= 1 TO maxCol DO
      FOR jc:= 1 TO maxCol DO
         A.Put(ic,jc,cos(coeff*(ic+jc)));
END;

PROCEDURE Aperture (VAR xc, yc, rc1, rc2 : integer; Gauss : BOOLEAN);
VAR
   AScreen : TInputScreen;
   descriptn : string;
BEGIN
   IF Gauss THEN descriptn:=' Standard Deviation:  r {   }'
   ELSE CASE Datarecord.transform OF
      pulse1 : descriptn:=' Aperture size:  x {   }  y {   }';
      step   : descriptn:=' Aperture radius:  r {   }';
   END; {case}
   with AScreen do
   BEGIN
      init;
      xc:= 0; yc:= 0; rc1:= 10; rc2:= 10;
      DefineInputPort (0.22, 0.78, 0.36, 0.67);
      LoadLine(' Define Aperture - Pixel Coordinates');
      LoadLine('');
      LoadLine(' Center position:  x {    }  y {    } ');
      LoadLine('');
      LoadLine(descriptn);
      LoadLine('');
      LoadLine('         [  Ok  ]     [Cancel] ');
      SetNumber(1,xc); SetNumber(2,yc);
      IF Datarecord.transform = pulse1 THEN BEGIN
         SetNumber(3,rc1);
         SetNumber(4,rc2);
         SetNumberLimits(4,0,32);
      END ELSE SetNumber(3,rc1);
      SetNumberLimits(1,-32,32);
      SetNumberLimits(2,-32,32);
      SetNumberLimits(3,0,32);
      AcceptScreen;
      IF Canceled THEN  Datarecord.state:= nogo
      ELSE BEGIN
         xc:= Round(GetNumber(1));
         yc:= Round(GetNumber(2));
         IF Datarecord.transform = pulse1 THEN BEGIN
            rc1:= Round(GetNumber(3));
            rc2:= Round(GetNumber(4));
         END ELSE
         rc1:= Round(GetNumber(3));
      END;
      done;
   END;
END;

PROCEDURE Calccmodz;
VAR
   row,col : integer;
   cz : REAL;
BEGIN
 FOR row:= 1 TO maxRow DO
   FOR col:= 1 TO maxCol DO BEGIN
      cz:= SQR(ARe.Value(row,col)) + SQR(BIm.Value(row,col));
      Power.Put(row,col,cz);
      cz:= SQRT(cz);
      Cmodz.Put(row,col,cz);
   END;
END;

PROCEDURE Modify2dfft (VAR Datarecord : Tdata; VAR CalcInv :Boolean);
VAR
   ig,jg,ic,ir,imt, RadioChoice,filtrad : integer;
   Modify2dfftScreen, ConfirmScreen : TInputScreen;
   filterflag: Boolean;
   sigmasqr : REAL;
   templot : tplot;
   temstr2 : string;
BEGIN
   Cmodz.Equate(ARe);
   Power.Equate(BIm);
   Datarecord.datastr2:= 'Real part';
   Datarecord.ContourDrawn:= notset;
   filterflag:= false;
   with Modify2dfftScreen DO BEGIN
      init;
      DefineInputPort(0.45,1.0,0.08,0.42);
      LoadLine('CALCULATE FILTERED INVERSE 2-D TRANSFORM.');
      LoadLine('#B Without (further) filtering.   0 to 44');
      LoadLine('#B High pass filter. Pixel radius  {   }');
      LoadLine('#B Low pass filter.  Pixel radius  {   }');
      LoadLine('#B Gaussian filtering   Std. Dev.  {   }');
      LoadLine('#B Inverse Gaussian filtering      {   }');
      LoadLine('#F Display filtered data ');
      LoadLine('         [  Ok  ] [Cancel]');
      SetRadioButton('B',1);
      SetNumberLimits(3,0,44);
      SetNumber(3,2);
      SetNumberLimits(5,1,44);
      SetNumber(5,15);
      SetNumberLimits(7,1,44);
      SetNumber(7,10);
      SetNumberLimits(9,1,44);
      SetNumber(9,10);
      AcceptScreen;
      IF Canceled THEN BEGIN
         Continue_F10;(*Announce(' Continue - or F10 to use menu. ');*)
         ARe.Equate(Cmodz);
         BIm.Equate(Power);
         Calccmodz;
         CalcInv:= false;
      END
      ELSE BEGIN
         ic:= MaxCol DIV 2;
         ir:= MaxRow DIV 2;
         CalcInv:= true;
         RadioChoice:= GetRadioButton('B');
         CASE RadioChoice OF
          2 : BEGIN      {high pass filter}
                 filtrad:= trunc(GetNumber(3));
                 Circle2DFT(ARe,0,0,filtrad,0.0,false);
                 Circle2DFT(BIm,0,0,filtrad,0.0,false);
              END;
          3 : BEGIN      {low pass filter}
                 filtrad:= trunc(GetNumber(5));
                 Circle2DFT(ARe,0,0,filtrad,0.0,true);
                 Circle2DFT(BIm,0,0,filtrad,0.0,true);
              END;
          4 : BEGIN
                 sigmasqr:= sqr(GetNumber(7));
                 For jg:= 1 TO MaxRow DO BEGIN
                    For ig:= 1 TO MaxCol DO BEGIN
                       ARe.Put(ig,jg, exp(-(SQR(ig-ic)+SQR(jg-ir))/sigmasqr)
                          *ARe.Value(ig,jg));
                       BIm.Put(ig,jg, exp(-(SQR(ig-ic)+SQR(jg-ir))/sigmasqr)
                          *BIm.Value(ig,jg));
                    END;
                 END;
              END;
          5 : BEGIN
                 sigmasqr:= sqr(GetNumber(9));
                 For jg:= 1 TO MaxRow DO BEGIN
                    For ig:= 1 TO MaxCol DO BEGIN
                       ARe.Put(ig,jg,1/exp(-(SQR(ig-ic)+SQR(jg-ic))/sigmasqr)
                          *ARe.Value(ig,jg));
                       BIm.Put(ig,jg,1/exp(-(SQR(ig-ic)+SQR(jg-ic))/sigmasqr)
                          *BIm.Value(ig,jg));
                    END;
                 END;
              END;
         END;{case}
         IF GetBoolean(10) THEN filterflag:= true;
      END;
      done;
   END;
   IF filterflag THEN BEGIN
      Draw2DFT(ARe,Re);
      Datarecord.ContourDrawn:= notset;
      CalcInv:= false;
      with ConfirmScreen DO BEGIN
         init;
         DefineInputPort(0.56,1.0,0.08,0.26);
         LoadLine('#B Calculate inverse transform  ');
         LoadLine('#B Add further filtering.  ');
         LoadLine('#B Restore unfiltered transform ');
         LoadLine('[  Ok  ] [Cancel] - or RETURN  ');
         AcceptScreen;
         IF Canceled THEN BEGIN
            Announce(' Any filtering used has been removed. ');
            Continue_F10;(*Announce(' Continue - or F10 to use menu. ');*)
            ARe.Equate(cmodz);
            Draw2DFT(ARe,Re);
            BIm.Equate(Power);
            CalcCmodz;
         END
         ELSE BEGIN
            ic:= MaxCol DIV 2;
            ir:= MaxRow DIV 2;
            RadioChoice:= GetRadioButton('B');
            CASE RadioChoice OF
               1 : CalcInv:= true;
               2 : Announce ('Use F4 again');
               3 : BEGIN
                      ARe.Equate(Cmodz);
                      Draw2DFT(ARe,Re);
                      BIm.Equate(Power);
                      CalcCmodz;
                   END;
            END;{case}
         END;
         done;
      END;
   END;
END;

PROCEDURE PackData(VAR data : RealArrayNDAT2);
VAR
   ac1,ac2,bd1,bd2,ip,jp,kp,lp,mp,np : INTEGER;
BEGIN
   kp:= 1;
   mp:= maxRow DIV 2;
   np:= maxCol DIV 2;
   ac1:= mp + 1;
   ac2:= 1;
   bd1:= mp + 1;
   bd2:= np + 1;
   FOR lp:= 1 TO 2 DO
   BEGIN
      FOR ip:= 0 TO mp - 1 DO
      BEGIN
         FOR jp:= 0 TO np - 1 DO
         BEGIN
            data[kp]:= ARe.Value(bd1 + ip,bd2 + jp);
            data[kp + 1]:= BIm.Value(bd1 + ip,bd1 + jp);
            kp:= kp + 2;
         END;
         FOR jp:= 0 TO np - 1 DO
         BEGIN
            data[kp]:= ARe.Value(ac1 + ip,ac2 + jp);
            data[kp + 1]:= BIm.Value(ac1 + ip,ac2 + jp);
            kp:= kp + 2;
         END;
      END;
   bd1:= 1;
   bd2:= np + 1;
   ac1:= 1;
   ac2:= 1;
   END;
END;   {end of PackData}

PROCEDURE UnpackData (VAR data : RealArrayNDAT2);
VAR
   ac1,ac2,bd1,bd2,ip,jp,kp,lp,mp,np : INTEGER;
BEGIN
   kp:= 1;
   mp:= maxRow DIV 2;
   np:= maxCol DIV 2;
   ac1:= mp + 1;
   ac2:= 1;
   bd1:= mp + 1;
   bd2:= np + 1;
   FOR lp:= 1 TO 2 DO
   BEGIN
      FOR ip:= 0 TO mp - 1 DO
      BEGIN
         FOR jp:= 0 TO np - 1 DO
         BEGIN
            ARe.Put(bd1 + ip, bd2 + jp, data[kp]);
            BIm.Put(bd1 + ip, bd1 + jp, data[kp + 1]);
            kp:= kp + 2;
         END;
         FOR jp:= 0 TO np - 1 DO
         BEGIN
            ARe.Put(ac1 + ip,ac2 + jp, data[kp]);
            BIm.Put(ac1 + ip,ac2 + jp, data[kp + 1]);
            kp:= kp + 2;
         END;
      END;
   bd1:= 1;
   bd2:= np + 1;
   ac1:= 1;
   ac2:= 1;
   END;
END;   {end of UnpackData}

(*PROCEDURE fourn(VAR data : RealArrayNDAT2; VAR nn : IntegerArrayNDIM;
                    ndim,isign : INTEGER);
VAR
   i1,i2,i3,i2rev,i3rev,ibit,idim,ip1,ip2,ip3,ifp1,ifp2 : INTEGER;
   k1,k2,n,ii1,ii2,ii3,nprev,nrem,ntot : INTEGER;
   tempi,tempr,wrs,wis : REAL;
   theta,wi,wpi,wpr,wr,wtemp : DOUBLE;
BEGIN
   PackData( data );
   ntot:= 1;
   FOR idim:= 1 TO ndim DO ntot:= ntot*nn[idim];
   nprev:= 1;
   FOR idim:= ndim DOWNTO 1 DO BEGIN  {Main loop over the dimensions.}
      n:= nn[idim];
      nrem:= ntot DIV (n*nprev);
      ip1:= 2*nprev;
      ip2:=ip1*n;
      ip3:=ip2*nrem;
      i2rev:= 1;
      {This is the bit reversal section of the routine.}
      FOR ii2:= 0 TO (ip2 - 1) DIV ip1 DO BEGIN
         i2:= 1 + ii2*ip1;
         IF i2 < i2rev THEN BEGIN
            FOR ii1:= 0 TO (ip1 - 2) DIV 2 DO BEGIN
               i1:= i2 + ii1*2;
               FOR ii3:= 0 TO (ip3 - i1) DIV ip2 DO BEGIN
                  i3:= i1 + ii3*ip2;
                  i3rev:= i2rev + i3 - i2;
                  tempr:= data[i3];
                  tempi:= data[i3 + 1];
                  data[i3]:= data[i3rev];
                  data[i3 + 1]:= data[i3rev + 1];
                  data[i3rev]:= tempr;
                  data[i3rev + 1]:= tempi;
               END;
            END;
         END;
         ibit:= ip2 DIV 2;
         WHILE (ibit >= ip1) AND (i2rev > ibit) DO BEGIN
            i2rev:= i2rev - ibit;
            ibit:= ibit DIV 2;
         END;
         i2rev:= i2rev + ibit;
      END;
      {Here begins the Danielson-Lanczos section of the routine.}
      ifp1:= ip1;
      WHILE ifp1 < ip2 DO BEGIN
         ifp2:= 2*ifp1;   {Initialize for the trig recurrence.}
         theta:= isign*6.28318530717959/(ifp2 DIV ip1);
         wpr:= -2.0*sqr(sin(0.5*theta));
         wpi:= sin(theta);
         wr:= 1.0;
         wi:= 0.0;
         FOR ii3:= 0 TO (ifp1 - 1) DIV ip1 DO BEGIN
            i3:= 1 + ii3*ip1;
            wrs:= wr;     {Casts double to real. A few compilers require}
            wis:= wi;      {a function angl().}
            FOR ii1:= 0 TO (ip1 - 2) DIV 2 DO BEGIN
               i1:= i3 + ii1*2;
               FOR ii2:= 0 TO (ip3 - i1) DIV ifp2 DO BEGIN
                  i2:= i1 + ii2*ifp2;
                  k1:= i2;            {Danielson-Lanczos formula:}
                  k2:= k1 + ifp1;
                  tempr:= wrs*data[k2] - wis*data[k2 + 1];
                  tempi:= wrs*data[k2 + 1] + wis*data[k2];
                  data[k2]:= data[k1] - tempr;
                  data[k2 + 1]:= data[k1 + 1] - tempi;
                  data[k1]:= data[k1] + tempr;
                  data[k1 + 1]:= data[k1 + 1] + tempi;
               END;
            END;
            wtemp:= wr;   {Trignometric recurrence.}
            wr:= wr*wpr - wi*wpi + wr;
            wi:= wi*wpr + wtemp*wpi + wi;
         END;
         ifp1:= ifp2;
      END;
      nprev:= n*nprev;
   END;
   UnPackData(data);
END;  *)

PROCEDURE Produce2Dfft(HotFlag : Boolean);
BEGIN
   nn[1]:= maxRow;
   nn[2]:= maxCol;
   IF HotFlag THEN BEGIN
     { fourn(data, nn, ndim, isign); }     { only use 1 of these 2 procs. }
      TsfmData.New2DFFT(Datarecord.sign2d);
      isign:= -isign;
      Datarecord.sign2d:= - Datarecord.sign2d;
      IF Datarecord.sign2d > 0 THEN
         Datarecord.datastr1:=' of inverse transform'
      ELSE Datarecord.datastr1:=' of transform'
   END
   ELSE BEGIN
      isign:= 1;
  {    PackData (data);  }
   END;
   SetUpMatrix;
   OpenViewPort (8);
   SmallBox.Init;
   smallBox.arrows := true;
   smallBox.boxColor := white xor darkGray;
   smallBox.SetViewAngles(alpha,beta,gamma);    {temp}
   SetWriteMode (XorPut); smallBox.DrawBox; SetWriteMode (CopyPut);
   ShowMouse ;
   SelectMuppetPort;
   sliders.init;
   sliders.create(1,  0 ,360,30, 0.675,0.46,0.99,0, '0','360','alpha',false);
   sliders.create(2,-180,180,20, 0.675,0.08,0.43,0, '', '',   'beta' ,true );
   sliders.drawAll;
end;

{*************** FFT 2D ********************}


PROCEDURE SetUpMenu(VAR Menu : TMenu);
var
   i : integer;
BEGIN
   Menu.Init;
   WITH Menu DO
   BEGIN
      column(1, 'File');
       row(1,1, 'About program');
       row(1,2, 'About CUPS');
       row(1,3, 'Configuration');
       row(1,4, '---------------');
       row(1,5, 'Load Coeffs');
       row(1,6, 'Load Curve Pts.');
       row(1,7, 'Save Coeffs');
       row(1,8, '---------------');
       row(1,9, 'Load Picture');
       row(1,10, 'Save Picture');
       row(1,11, '--------------');
       row(1,12, 'Exit');
       for i := 4 to 11 do rowactivate(1,i,false);
      column(2, 'Fourier');
       row(2,1, 'Square wave');
       row(2,2, 'Triangle wave');
       row(2,3, 'Sawtooth wave');
       row(2,4, 'Half-wave Rect');
       row(2,5, 'Full-wave Rect');
       row(2,6, 'Pulse');
       row(2,7, 'Input Function');
       row(2,8, 'Draw curve');
       row(2,9, 'Set Coeffs.');
       row(2,10, 'Propagate');
       rowactivate(2,10,false);
       row(2,11, 'Evolve');
       rowactivate(2,11,false);
       row(2,12, 'Display Setup');
      column(3, '1-D DFT');
       row(3,1, 'Single pulse');
       row(3,2, 'Double pulse');
       row(3,3, 'Grating');
       row(3,4, 'Cosine wave');
       row(3,5, 'Step down/up');
       row(3,6, 'Sawtooth');
       row(3,7, 'Triangle');
       row(3,8, 'Gaussian');
       row(3,9, 'Input Function');
       row(3,10, 'Draw curve');
      column(4, '2-D DFT');
       row(4,1, 'Single delta');
       row(4,2, 'Rectangular hole');
       row(4,3, 'Square Frames');
       row(4,4, 'Square Peg/Hole');
       row(4,5, 'Circular Hole');
       row(4,6, 'Gaussian');
       row(4,7, 'Checker Board');
       row(4,8, 'Check Pattern');
       row(4,9, 'Cosine Wave');
       row(4,10, 'Double deltas');
       row(4,11, 'Input function');
       row(4,12, 'Picture');
   END;
   Menu.Display;
END;

PROCEDURE SetUpHotKeys(VAR HotKeys : THotKeys; VAR Datarecord : Tdata);
BEGIN
   CASE Datarecord.mode OF
   fseries :  WITH HotKeys DO BEGIN
                 HotKeys.Init(4);
                 key[1] := 'F1-Help';
                 key[2] := 'F2-Run/Stop';
                 key[3] := 'F3-Step';
                 key[4] := 'F10-Menu';
                 HotKeys.Display;
              END;
   fft_1d :   WITH HotKeys DO BEGIN
                 HotKeys.Init(6);
                 key[1] := 'F1-Help';
                 IF Datarecord.mysign > 0 THEN key[2] := 'F2-Inv Trans'
                 ELSE key[2] := 'F2-Transform';
                 key[3] := 'F3-Real/Imag';
                 key[4] := 'F4-Phase/Amp';
                 key[5] := 'F5-Power';
                 key[6] := 'F10-Menu';
                 HotKeys.Display;
              END;
   fft_2d :   WITH HotKeys DO BEGIN
                 HotKeys.Init(6);
                 key[1] := 'F1-Help';
                 IF Datarecord.sign2d > 0 THEN key[2] := 'F4-Transform'
                 ELSE key[2] := 'F4-Inv Trans';
                 key[3] := 'F6-Real part';
                 key[4] := 'F7-Imaginary';
                 key[5] := 'F8-Power/Gray';
                 key[6] := 'F10-Menu';
                 HotKeys.Display;
              END;
   dataIO :   WITH HotKeys DO BEGIN
                 HotKeys.Init(2);
                 key[1] := 'F1-Help';
                 key[2] := 'F10-Menu';
                 HotKeys.Display;
              END;
   END; {case}
END;

Function ScanCoeff(VAR Datarecord : Tdata) : REAL;
VAR
   termnum             : integer;
   Maxcoef,Acoef,Bcoef : real;
BEGIN
   Maxcoef:=0.1;
   FOR termnum := 1 TO 12 DO BEGIN
      ABcoefficients(Acoef,Bcoef,termnum);
      Datarecord.ABcoeffs[0,termnum]:=Acoef;
      Datarecord.ABcoeffs[1,termnum]:=Bcoef;
      Acoef:= abs(Acoef);
      Bcoef:= abs(Bcoef);
      IF Acoef > Maxcoef THEN Maxcoef:= Acoef;
      IF Bcoef > Maxcoef THEN Maxcoef:= Bcoef;
   END;
   Scancoeff:= Maxcoef;
END;

PROCEDURE InitF(VAR Datarecord : Tdata);
VAR
   termnum                : word;
   key                    : byte;
   tStr,aStr,bStr         : string;
   Zerocoef,AcoefT,BcoefT,Yrange : real;

BEGIN
   Datarecord.addterm:= FALSE;
   Datarecord.state := add;
   ClearMUPPETport;
   Menu.Display;
   Openviewport(1);
   Openviewport(2);
   Openviewport(3);
   Openviewport(4);
   Yrange:= Scancoeff(Datarecord);
   DefineScale(2,0.0,82,-Yrange,+Yrange);
   DefineScale(3,0.0,82,-Yrange,+Yrange);
   IF Yrange > 0.5 THEN Yrange:= 0.5 ELSE Yrange:= 0.1;

   Selectscale(2);
   SelectViewport(2);
   SetColor(white);
   Axis(0.0,0.0,10,Yrange);
   PutLabel(inside,'cos coef. (a)');
   SelectViewport(3);
   Axis(0.0,0.0,10,Yrange);
   PutLabel(inside,'sin coef. (b)');
   Selectscale(4);
   SelectViewport(4);
   Axis(0.0,0.0,10,0.2);
   PutLabel(inside,'RMS Dif.');
   Selectscale(1);
   SelectViewport(1);
   DrawAxis(0.0,0.0,1,0.5,white);
   SetColor(white);
   PutLabel(top,'                                       Fourier Analysis');
   Zcoefficient (Zerocoef);
   Datarecord.ABcoeffs[0,0]:=Zerocoef;
   termF.SetUpF(Zerocoef,Datarecord.numS,lightgreen,cyan);
   Datarecord.numC:=0;
END;

PROCEDURE ShowtermF(VAR Datarecord : Tdata);
VAR
   termnum                : word;
   key                    : byte;
   tStr,aStr,bStr         : string;
   Zerocoef,AcoefT,BcoefT : real;
BEGIN
   SelectViewport(1);
   SelectScale(1);
   Datarecord.numC := Datarecord.numC + 1;
   termnum := Datarecord.numC;
   tStr := NumStr(termnum,2,0);
   ABcoefficients(AcoefT,BcoefT,termnum);
   Histogram(AcoefT,2,termnum);
   Histogram(BcoefT,3,termnum);
   Datarecord.ABcoeffs[0,termnum]:=AcoefT;
   Datarecord.ABcoeffs[1,termnum]:=BcoefT;

   SetColor(lightmagenta);
   SelectViewport(1);
   Selectscale(1);

   RubOut(1,1,25,GraphBackColor);
   RubOut(1,2,13,GraphBackColor);
   RubOut(1,3,13,GraphBackColor);
   IF (Abs(AcoefT)<2*MachEps) AND (Abs(BcoefT)<2*MachEps) THEN BEGIN
      termF.DrawF(AcoefT,BcoefT,lightmagenta,termnum);
      SetColor(white);
      IF (Datarecord.State = add) OR (Datarecord.State = extra) THEN
         Print(1,1,concat('Coefs. for n=',tStr,' are zero.'));
   END
   ELSE BEGIN
      termF.DrawF(AcoefT,BcoefT,lightmagenta,termnum);
      aStr := NumStr(AcoefT,6,3);
      bStr := NumStr(BcoefT,6,3);
      IF (Datarecord.State = add) OR (Datarecord.State = extra) THEN BEGIN
         Print(1,1,concat('Coefs. for n=',tStr));
         Print(1,2,' a(n)='+aStr);
         Print(1,3,' b(n)='+bStr);
      END;
   END;
END;

PROCEDURE AddtermF(VAR Datarecord : Tdata);
VAR
   termnum                : word;
   key                    : byte;
   tStr,aStr,bStr         : string;
   Zerocoef,AcoefT,BcoefT : real;
BEGIN
   termnum := Datarecord.numC;
   IF termnum >10 THEN BEGIN
      menu.rowactivate(2,10,true);
      menu.rowactivate(2,11,true);
      menu.rowactivate(1,7,true);
   END;
   SelectViewport(1);
   SelectScale(1);
   termF.Draw_Sum(Datarecord.RMSDif[termnum],lightgreen);
   Histogram(Datarecord.RMSDif[termnum],4,termnum);
   IF termnum >= Datarecord.numT THEN Datarecord.state:= dun;
   MouseBorders (0,0,GetMaxX-8,GetMaxY-8);
END;


procedure initrgbpal; {sets standard ega colors in rgbpal array 0..15}
VAR
   ipal : integer;

begin                                      {these are later changed to set}
   for ipal:=0 to 5 do rgbpal[ipal]:=ipal; {gray levels in the rgb palette}
   rgbpal[6]:=20;
   rgbpal[7]:=7;
   for ipal:=0 to 7 do rgbpal[ipal+8]:=56+ipal;
end {initrgbpal};


procedure picload (VAR Datarecord : Tdata);
VAR
   ipic,jpic,kpic,kphase : integer;
   piclet,pint : Char;
   picfile1,picfile2 : text;
   phase : real;       {phase}

begin
   IF NOT(picflag) THEN BEGIN
      assign(picfile1,Datarecord.picfile);
      reset(picfile1);
   END;
   for jpic:= 0 to MaxCol - 1 do begin
      for ipic:= 0 to MaxRow - 1 do begin
         IF picflag THEN  piclet:= DataRecord.picksel[ipic+jpic*(MaxCol-1)]
         ELSE BEGIN
            read(picfile1,piclet);
            DataRecord.picksel[ipic+jpic*(MaxCol-1)]:=piclet;
         END;
         kpic:= ord(piclet);
         if kpic>63 then kpic:=kpic-55 else kpic:=kpic-48;
               {uses ASCII code in ORD}
         kphase:= 0;
         ARe.Put(jpic+1,ipic+1,(kpic/32)-0.5);
         BIm.Put(jpic+1,ipic+1,kphase/32);
      end;
      IF NOT(picflag) THEN readln(picfile1);
   end;
   picflag:= true;
   picture:= true;
end {picload};

procedure picdraw(VAR levels : DMatrix);
VAR
   id,jd,iid,jjd,kd,kd1,kd2,ld,md,ipl : integer;
   pmin,pmax : real;

BEGIN
   FOR id:=0 TO 15 DO BEGIN
      kd:=rgbpal[id];
      setrgbpalette(kd,id*4,id*4,id*4); {defines gray levels}
   END;
   OpenViewPort(7);
   ipl:= 256 DIV MaxCol;
   levels.MinMax(id,jd,pmin,iid,jjd,pmax);
   HideMouse ;
   FOR jd:=0 to MaxCol - 1 DO BEGIN
      FOR id:=0 to MaxRow - 1 DO
      BEGIN
         iid:=80+id*ipl;
         jjd:=64+jd*ipl;
         kd:=trunc((levels.Value(jd+1,id+1)-pmin)*31/(pmax-pmin)+0.5);
         IF odd(kd) THEN BEGIN
            kd1:= kd div 2;
            IF kd >= 31 THEN kd2:= (kd div 2) ELSE kd2:= (kd div 2) + 1;
         END
         ELSE BEGIN kd1:= kd div 2; kd2:= kd div 2; END;
         kd:=kd div 2;                        {k is the required gray level}
         for ld:=0 to ipl - 1 do
         for md:=0 to ipl - 1 do BEGIN
            IF odd(ld + md) THEN kd:=kd1 ELSE kd:=kd2;
            putpixel(iid+ld,jjd+md,kd);
         END;
      END;
   Print(3,1, Datarecord.datastr2+Datarecord.datastr1);
   ShowMouse ;
   END;
END;{picdraw}

PROCEDURE Eval2dft ( nData : word);
   VAR
      hinc : REAL;
      ix, iy  : INTEGER;
      myout,rx,ry : Complex;
BEGIN
   HaltIfError := false;
   ErrorFound  := false;
   hinc:=2*Pi/(nData);
   rx.im:= 0;
   ry.im:= 0;
   ix := 1;
   while (ix<=nData)  DO
   begin
      iy := 1;
      while (iy<=nData) and (not ErrorFound) DO BEGIN
         rx.Re:= (ix - 1) * hinc - pi;
         ry.Re:= (iy - 1) * hinc - pi;
         FourierFunc.Cxf(rx,ry,Cxzero,Cxzero,myout);
         ARe.Put(ix,iy,myout.Re);
         BIm.Put(ix,iy,myout.Im);
         inc(iy);
      END;
      inc(ix);
   end;
   if ErrorFound then DataRecord.state:= nogo;
   HaltIfError := true;
   ErrorFound  := false;
END;

PROCEDURE Mag2dfft (VAR Datarecord : Tdata; VAR MagShow : Boolean);
VAR
   col, row, imt, RadioChoice : integer;
   MagScreen : TInputScreen;
BEGIN
with MagScreen DO
   BEGIN
      init;
      DefineInputPort(0.60,1.0,0.08,0.42);
      LoadLine(' Magnitude of 2-D DFT.');
      LoadLine(' #C  mod z surface');
      LoadLine(' #C  Power spectrum surface');
      LoadLine(' #C  Gray levels of power');
      LoadLine(' #C  Gray levels of mod z');
      LoadLine(' #C  Gray levels of real part');
      IF Datarecord.upcont THEN LoadLine(' #T  Update Contours')
      ELSE LoadLine(' #F  Update Contours');
      LoadLine('   [  Ok  ]   [Cancel]');
      SetRadioButton('C',1);
      AcceptScreen;
      IF Canceled THEN BEGIN
         Continue_F10;(*Announce(' Continue - or F10 to use menu. ');*)
         MagShow:= false;
      END
      ELSE BEGIN
         MagShow:= true;
         Datarecord.upcont:= GetBoolean(6);
         RadioChoice:= GetRadioButton('C');
         CASE RadioChoice OF
          1 : BEGIN
                 DataRecord.datastr2:= 'mod z';
                 Draw2DFT(cmodz,modz);
              END;
          2 : BEGIN
                 DataRecord.datastr2:='Power spectrum';
                 Draw2DFT(Power,Pwr);
              END;
          3 : BEGIN
                 DataRecord.datastr2:='Gray levels of power';
                 PicDraw(Power);
                 IF Datarecord.upcont THEN UpdateContour(Power);
              END;
          4 : BEGIN
                 DataRecord.datastr2:='Gray levels of mod z';
                 PicDraw(Cmodz);
                 IF Datarecord.upcont THEN UpdateContour(Cmodz);
              END;
          5 : BEGIN
                 DataRecord.datastr2:= 'Gray levels';
                 PicDraw(ARe);
                 IF Datarecord.upcont THEN UpdateContour(cmodz);
              END;
         END;{case}
      END;
      done;
   END;
END;

PROCEDURE StoreInit;

BEGIN
   yvals1.Free; Yvals2.Free; Yvals3.Free;
   ARe.init(maxRow,maxCol);
   BIm.init(maxRow,maxCol);
   Cmodz.init(maxRow,maxCol);
   Power.init(maxRow,maxCol);
END;

PROCEDURE HandleMenu(VAR Datarecord : Tdata);
VAR Snum1,Snum2,id,kd : integer;
    F10 : functionXY;
    termdone : word;
    FileChosen, found : boolean;
    picfile : string;
BEGIN
   WITH Menu DO
   IF NOT((colChosen = 1) AND (rowChosen IN [1,2,3,6,8])) THEN BEGIN
      sliders.done;
      menu.rowactivate(1,7,false);
      menu.rowactivate(1,9,false);
      menu.rowactivate(1,10,false);
      Datarecord.mode:=initial;
      Datarecord.transform := none1;
   END;
   IF menu.colChosen <> 1 THEN BEGIN
      IF DataRecord.fft1_num IN [1..3] THEN fft1_button.done;
      DataRecord.fft1_num:= 0;
   END;
   WITH Menu DO
   IF colChosen IN [1..4] THEN
   CASE colChosen OF
    1 : BEGIN
           CASE rowChosen OF
              1 : ShowHelp('FOURIER.HLP','Help 1' );
              2 : aboutCUPS;
              3 : Configuration;
              5 : BEGIN   {Load Fourier coefficients}
                     rowactivate(2,10,false);
                     rowactivate(2,11,false);
                     Datarecord.mode:= fseries;
                     Datarecord.wave:= coeffs;
                     SetUpHotKeys(HotKeys,Datarecord);
                     DataInput (coefsFile, FileChosen, 'dat');
                     IF FileChosen THEN BEGIN
                        Datarecord.coeffile:= coefsfile;
                        found:= false;
                        Loadcoef(found,Datarecord.coeffile,Datarecord);
                        Datarecord.state:= go;
                        Datarecord.numT:= Datarecord.numC;
                        Datarecord.time:= 0;
                     END;
                     IF  Datarecord.state = nogo THEN
                        Announce('Use the menu to select an option')
                     ELSE InitF(Datarecord);
                     SetUpHotKeys(HotKeys,Datarecord);
                     ShowMouse ;
                  END;
              6 : BEGIN   {Load Curve of equally spaced points}
                     rowactivate(2,10,false);
                     rowactivate(2,11,false);
                     Datarecord.mode:= dataIO;
                     Datarecord.wave:= curve;
                     Datarecord.state:= nogo;
                     SetUpHotKeys(HotKeys,Datarecord);
                     found:= false;
                     DataInput(curvesfile, FileChosen, 'pts');
                     IF FileChosen THEN BEGIN
                        Datarecord.curvefile:= curvesfile;
                        Loadcurve(found,Datarecord.curvefile,Datarecord);
                     END;
                     IF found THEN BEGIN
                        Datarecord.state:= go;
                        Datarecord.numT:= round(Datarecord.numS/4);
                        Datarecord.time:= 0;
                        Datarecord.mode:= fseries;
                     END;
                     ShowMouse ;
                     IF  Datarecord.state = nogo THEN
                        Announce('Use the menu to select an option')
                     ELSE InitF(Datarecord);
                     SetUpHotKeys(HotKeys,Datarecord);
                  END;
              7 : BEGIN
                     SetUpHotKeys(HotKeys,Datarecord);
                     REPEAT
                     UNTIL DataOutput(coefsfile);
                     IF coefsfile <> '' THEN SaveCoefs(coefsfile);
                     Announce('Continue or use the menu to select an option');
                  END;
              9 : BEGIN
                      IF picflag THEN found:= true ELSE found:= false;
                      picflag:= false;
                      DataInput(curvesfile, FileChosen, 'txt');
                      IF FileChosen THEN BEGIN
                         Datarecord.picfile:= curvesfile;
                         picload(Datarecord);
                         Datarecord.ContourDrawn:= notset;
                         Datarecord.upcont:= true;
                         Produce2Dfft(False);
                         Datarecord.sign2d:= 1;
                      END
                      ELSE BEGIN
                         IF found THEN BEGIN
                            Announce('Old picture retained');
                            picflag:= true;
                          END
                          ELSE Announce('Continue or use the menu to select an option');
                      END;
                      Datarecord.mode:= fft_2d;
                      menu.rowactivate(1,9,true);
                      menu.rowactivate(1,10,true);
                      SetUpHotKeys(HotKeys,Datarecord);
                   END;
              10 : BEGIN
                     SetUpHotKeys(HotKeys,Datarecord);
                     REPEAT
                     UNTIL  DataOutput(coefsfile);
                     IF coefsfile <> '' THEN savepict(coefsfile, ARe);
                     Announce('Continue or use the menu to select an option');
                  END;
              12 :  ExitFlag := true;
           END;{case of row}
        END;
    2 : BEGIN
           Datarecord.mode:= fseries;
           Datarecord.state:= go;
           rowactivate(1,5,true);
           rowactivate(1,6,true);
           rowactivate(1,9,false);
           rowactivate(1,10,false);
           IF rowChosen IN [1..9,12] THEN BEGIN
              ClearMUPPETport;
              StdVGAcolors;
              Menu.Display;
              menu.rowactivate(2,10,false);
              menu.rowactivate(2,11,false);
              Datarecord.numT:= Datarecord.numTset;
              Datarecord.numS:= Datarecord.numSset;
           END;
           CASE rowChosen OF
              1 : Datarecord.wave := square;
              2 : Datarecord.wave := triangle;
              3 : Datarecord.wave := sawtooth;
              4 : Datarecord.wave := half_rect;
              5 : Datarecord.wave := full_rect;
              6 : BEGIN
                     Datarecord.wave := pulse;
                     Datarecord.PulseRatio:= PulseWidth(Datarecord.PulseRatio);
                   END;
              7 : BEGIN
                     IF FInput THEN BEGIN
                        Datarecord.wave := fnct;
                        termF.Evaluate (Datarecord.numS);
                     END
                     ELSE BEGIN
                        Announce('Error: Expression can not be evaluated');
                        Datarecord.state:= nogo;
                     END;
                  END;
              8 : BEGIN
                     Datarecord.wave := curve;
                     MouseDraw;
                     Menu.Display;
                  END;
              9 : BEGIN
                     termdone:= 1;
                     Datarecord.wave := coeffs;
                     Announce('Over-rides Fourier ''Display Setup'' to number of terms given in input.');
                     REPEAT
                        CoeffInput(termdone,DataRecord);
                        termdone:= termdone + 12;
                     UNTIL termdone > Datarecord.numT;
                  END;
             10 : BEGIN
                     PropagateDisplayScreen(Datarecord);
                  END;
             11 : BEGIN {Evolve}
                     Announce('Over-rides Fourier ''Display Setup'' - screen plotted at 200 points.');
                     Datarecord.numS:= 200;

   WITH GraphScales [{ScaleNumber}1] DO Xconst  := (xmax - xmin) / DataRecord.numS;
                     IF Datarecord.numC > 50 THEN Datarecord.numC:= 50;
                     EvolveDisplayScreen(Datarecord);
                  END;
             12 : BEGIN
                     Datarecord.wave := none;
                     Datarecord.mode:= dataIO;
                     SetUpHotKeys(HotKeys,Datarecord);
                     SetDisplayScreen(Datarecord);
                     Datarecord.state:= nogo;
                  END;
              ELSE Datarecord.state:= nogo;
           END;{case of row}
           If NOT(Datarecord.yvalset) THEN BEGIN
              Yvalsinit;
              Datarecord.yvalset:= true;
           END;
           IF  Datarecord.state = nogo THEN Announce('Use the menu to select an option')
           ELSE BEGIN
              IF (Datarecord.wave in [square..coeffs]) AND
                    (rowChosen in [1..9]) THEN initF(Datarecord);
              SetUpHotKeys(HotKeys,Datarecord);
           END;
        END;
    3 : BEGIN
           ClearMUPPETport;
           StdVGAcolors;
           rowactivate(1,5,false);
           rowactivate(1,6,false);
           rowactivate(2,10,false);
           rowactivate(2,11,false);
           rowactivate(1,9,false);
           rowactivate(1,10,false);
           Menu.Display;
           Datarecord.mode:=fft_1d;
           Datarecord.state:= go;
           DataRecord.yrange1d:= 0;
           Datarecord.slitwidth:= 6;
           Datarecord.slitpos:= 6;
           Datarecord.slitsep:= 12;
           Datarecord.mysign:= 1;
           DataRecord.oldy:= 1;
           DataRecord.fft1_num:= 2;
           CASE rowChosen OF
              1 : Datarecord.transform := pulse1;
              2 : Datarecord.transform := twin;
              3 : Datarecord.transform := grating;
              4 : Datarecord.transform := coswave;
              5 : Datarecord.transform := step;
              6 : Datarecord.transform := sawtooth1;
              7 : Datarecord.transform := triangle1;
              8 : Datarecord.transform := Gaussian;
              9 : Datarecord.transform := fnctn;
              10 : Datarecord.transform := drawn;
              ELSE Datarecord.state:= nogo;
           END;
           TsfmData.SetUpFourierTransform(num1D);
           TsfmData.StdDataForFourierTransform(Datarecord);
           IF Datarecord.transform IN [pulse1,twin,grating,Gaussian,
              sawtooth1,triangle1,coswave,step] THEN
                 SlitDist(Datarecord.slitwidth); {draws sliders}
           IF Datarecord.state = nogo THEN
              Announce('Use the menu to select an option');
        END;
    4 : BEGIN
           ClearMUPPETport;
           StdVGAcolors;
           rowactivate(1,5,false);
           rowactivate(1,6,false);
           rowactivate(2,10,false);
           rowactivate(2,11,false);
           rowactivate(1,9,true);
           rowactivate(1,10,true);
           Menu.Display;
           IF Datarecord.yvalset THEN StoreInit;
           Datarecord.yvalset:= false;
           Datarecord.mode:= fft_2d;
           Datarecord.state:= go;
           Datarecord.sign2d:= 1;
           DataRecord.datastr1:=' of original data';
           Datarecord.ContourDrawn:= notset;
           Datarecord.upcont:= true;
           Datarecord.datastr2:= 'Real part';
           picture:= false;
           ARe.Fill(0.0);
           BIm.Fill(0.0);
           alpha:= 30;      {set angles for box at default values}
           beta:= 20;
           gamma:= 0;
           CASE rowChosen OF
              1 : SquareHole(ARe,0,0,0,0,1.0);{ARe.Put(40,31,1.0); }
              2 : BEGIN
                     Datarecord.transform := pulse1;
                     Aperture (id, kd, snum1,snum2,false);
                     SquareHole(ARe,id,kd,snum1,snum2,1.0);
                  END;
              3 : BEGIN
                     SquareHole(ARe,0,0,10,10,1.0);
                     SquareHole(ARe,0,0,8,8,0.0);
                     SquareHole(ARe,0,0,6,6,1.0);
                     SquareHole(ARe,0,0,4,4,0.0);
                     SquareHole(ARe,0,0,2,2,1.0);
                  END;
              4 : BEGIN
                     SquareHole(ARe,0,0,5,5,-1);
                     SquareHole(ARe,0,0,3,3,1);
                  End;
              5 : BEGIN  {circular hole}
                     Datarecord.transform := step;
                     Aperture (id, kd, snum1,snum2,false);
                     IF Datarecord.state <> nogo THEN BEGIN
                        ARe.Fill(1.0);
                        Circle2DFT(ARe, id, kd, snum1, 0, true);
                     END;
                  END;
              6 : BEGIN
                     Aperture (id, kd, snum1,snum2,true);
                     IF Datarecord.state <> nogo THEN
                              Gauss2DFT(ARe,id,kd,snum1);
                  END;
              7 : Checks(ARe,FALSE);
              8 : Checks(ARe,TRUE);
              9 : MyCosWave(ARe);
              10 : TwoPins(ARe);
              11 : BEGIN
                      IF FInput THEN BEGIN
                         Eval2dft (MaxCol);
                      END
                      ELSE BEGIN
                         Announce ('Error:  Expression can not be evaluated');
                         Datarecord.state:= nogo;
                       END;
                    END;
              12 : BEGIN
                      IF NOT(picflag) THEN BEGIN
                         found:= false;
                         DataInput(curvesfile, FileChosen, 'txt');
                         IF FileChosen THEN BEGIN
                            Datarecord.picfile:= curvesfile;
                            picload(Datarecord);
                         END
                         ELSE Datarecord.state:= nogo;
                      END
                      ELSE picload(Datarecord);
                   END;
           END; {case}
           IF Datarecord.state = nogo THEN
              Announce('Use the menu to select an option')
           ELSE BEGIN
              Produce2Dfft(False);
              SetUpHotKeys(HotKeys,Datarecord);
           END;
        END;
   END; {case}
END;

PROCEDURE NoChoice;
Begin
   message('No option selected - F10 to continue ');
END;

PROCEDURE HandleHotKeys (key : byte; VAR Datarecord : TData);
VAR
   CompInv, drawcont : BOOLEAN;
   tStr : string;
   hki : integer;

BEGIN
   CASE Datarecord.mode OF
   Initial :  CASE key OF
      1 : ShowHelp('FOURIER.HLP','Help 1' );
      2 : IF Menu.Chosen THEN HandleMenu(Datarecord);
      END; {case Initial}
   fseries : CASE key OF
      1 :  ShowHelp('FOURIER.HLP','Help 2' );

      2 : BEGIN
             IF Datarecord.state = nogo THEN NoChoice
             ELSE IF (Datarecord.numC >= Datarecord.numT) AND
                (Datarecord.wave in [square..coeffs]) THEN
                Announce('Current Fourier setup stops further Auto plotting use  Step  (F3)')
                ELSE IF Datarecord.state <> auto THEN Datarecord.state:= auto
                   ELSE Datarecord.state:= add;
          END;
      3 : with Datarecord DO BEGIN
             IF (wave = propagate) or (wave = evolve) THEN BEGIN
                IF (wave = propagate) THEN termf.PropagateWave(lightgreen)
                ELSE termf.Draw_Evolve(evolvephase, lightgreen);
                state:= add;
             END
             ELSE BEGIN
             IF state = nogo THEN NoChoice
             ELSE BEGIN
                tStr:= NumStr(numT,2,0);
                IF ((wave = coeffs) AND (numC <numT)) OR (wave <> coeffs) THEN
                BEGIN
                   IF (4 * numC < numS)  Then BEGIN
                      IF numC > numT THEN state := extra ELSE state := add;
                      IF addterm THEN BEGIN
                         addterm:= NOT addterm;
                         AddtermF(Datarecord);
                      END
                      ELSE BEGIN
                         addterm:= NOT addterm;
                         ShowtermF(Datarecord);
                      END;
                   END
                   ELSE Announce('Current Fourier ''Display Setup'' stops further RMS Dif. decrease. ');
                END
                ELSE Announce('Only '+tStr+' sine and cosine coefficients have been chosen.');
             END;{else}
             END;{else}
          END;
      4 : BEGIN
             CheckForEvents;
             Datarecord.state:=dun;
             IF Menu.Chosen THEN HandleMenu(Datarecord);
          END;
      END; {case}
   fft_1d :  CASE key OF
               1 : ShowHelp('FOURIER.HLP','Help 3' );
               2 : BEGIN
                      TsfmData.ModifyTransform ( Datarecord, CompInv );
                      IF CompInv THEN BEGIN
                         IF Datarecord.transform = twin THEN
                         sliders.Erase(5,black);
                         IF Datarecord.transform in [fnctn,inverse, drawn] THEN {nothing}
                         ELSE BEGIN
                            sliders.Erase(3,black);
                            sliders.Erase(4,black);
                         END;
                         IF Datarecord.transform in [fnctn,inverse, drawn] THEN {nothing}
                         ELSE
                         sliders.done;
                         Datarecord.transform:= inverse;
                         TsfmData.StdDataForFourierTransform(Datarecord);
                      END
                   END;
               3 : BEGIN   {Real/Imag}
                      DataRecord.fft1_num:= 2;
                      IF Datarecord.state = nogo THEN Announce('Use the menu to select an option')
                      ELSE TsfmData.PlotFourierTransform(lightgreen,lightmagenta,cyan);
                   END;
               4 : BEGIN   {Phase/Amp}
                      fft1_button.done;
                      DataRecord.fft1_num:= 4;
                      IF Datarecord.state = nogo THEN Announce('Use the menu to select an option')
                      ELSE TsfmData.PlotFourierTransform(lightgreen,lightmagenta,cyan);
                   END;
               5 : BEGIN    {Power}
                      fft1_button.done;
                      DataRecord.fft1_num:= 5;
                      IF Datarecord.state = nogo THEN Announce('Use the menu to select an option')
                      ELSE TsfmData.PlotFourierTransform(lightgreen,lightmagenta,cyan);
                   END;
               6 : BEGIN
                      CheckForEvents;
                      IF Menu.Chosen THEN HandleMenu(Datarecord);
                   END;
      END; {case}
   fft_2D :  CASE key OF
      1 :  BEGIN
              StdVGAcolors;
              ShowHelp('FOURIER.HLP','Help 4' );
           END;
      2 : BEGIN
             CompInv:= true;
             IF Datarecord.sign2d < 0 THEN Modify2dfft (Datarecord,CompInv);
             IF CompInv THEN BEGIN
                Datarecord.ContourDrawn:= notset;
                Datarecord.datastr2:= 'Real part';
                Produce2Dfft(True);
                SetUpHotKeys(HotKeys,Datarecord);
             END;
       END;
      3 : BEGIN
             Datarecord.datastr2:= 'Real part';
             Draw2DFT(ARe,Re);
          END;
      4 : BEGIN
             Datarecord.datastr2:='Imaginary part';
             Draw2DFT(BIm,Im);
          END;
      5 : BEGIN
             Mag2dfft (Datarecord, drawcont );
             Datarecord.ContourDrawn:= modz;
          END;
      6 : BEGIN
             IF Menu.Chosen THEN HandleMenu(Datarecord);
          END;
      END; {case}
   DataIO :  CASE key OF
      1 : ShowHelp('FOURIER.HLP','Help 1' );
      2 : IF Menu.Chosen THEN HandleMenu(Datarecord);
      END; {case DataIO}
   END; {case}
END;


var
   color : word;
   key : byte;
   xnum,id : integer;
(************************** Main Program ***************************)

BEGIN
   CUPSinit;
   color := 0;
   ExitFlag := false;
   FOR id:= 0 TO 15 DO oldpalette[1,id]:= 0;
   FOR id:= 0 TO 15 DO oldpalette[2,id]:= 0;
   FOR id:= 0 TO 15 DO oldpalette[3,id]:= 0;
   oldpalette[3,1]:= 42;  {blue}
   oldpalette[2,2]:= 42;  {green}
   oldpalette[2,3]:= 42; oldpalette[3,3]:= 42;
   oldpalette[1,4]:= 42;  {red}
   oldpalette[1,5]:= 42; oldpalette[3,5]:= 42;
   oldpalette[2,6]:= 42; oldpalette[1,6]:= 42;
   oldpalette[1,7]:= 42; oldpalette[2,7]:= 42; oldpalette[3,7]:= 42;
   oldpalette[1,8]:= 21; oldpalette[2,8]:= 21; oldpalette[3,8]:= 21;
   oldpalette[3,9]:= 63;  {light blue}
   oldpalette[2,10]:= 63; {light green}
   oldpalette[2,11]:= 63; oldpalette[3,11]:= 63;
   oldpalette[1,12]:= 63;
   oldpalette[1,13]:= 63; oldpalette[3,13]:= 63;
   oldpalette[1,14]:= 63; oldpalette[2,14]:= 63;
   oldpalette[1,15]:= 63; oldpalette[2,15]:= 63; oldpalette[3,15]:= 63;
   initrgbpal;
   Defineviewport(1, 0.06, 0.97, 0.4, 0.92);
   DefineScale(1,-Pi,+Pi,-1.3,+1.3);
   DefineScale(10,-Pi,+Pi,-1.3,+1.3);
   Defineviewport(2, 0.06, 0.32, 0.09, 0.35);
   Defineviewport(3, 0.39, 0.65, 0.09, 0.35);
   Defineviewport(4, 0.71, 0.97, 0.09, 0.35);
   DefineScale(4,0.0,82.0,0.0,+0.4);

   Datarecord.coeffile:= 'sqwvcoef.dat';
   stdwave:=[square, triangle, sawtooth, half_rect, full_rect,pulse,coeffs];
   num1D:= 512;
   xnum:= MaxSub div 2;
   FuncString:= '';
   Cxzero.Re:= 0;
   Cxzero.Im:= 0;
   Datarecord.mysign:=1;
   Datarecord.Cauchy_A:=1.5;
   Datarecord.Cauchy_B:= - 0.00001;
   Datarecord.AttenCoeff:= 1.0;
   DefineViewPort(5, 0.1, 0.9, 0.55, 0.925);
   DefineViewPort(6, 0.1, 0.9, 0.1, 0.475);
   DefineScale(5,-xnum,+xnum,-1.2,+1.3);
   DefineScale(6,-xnum,+xnum,-30,+30);
   DefineViewPort (7, 0.0,  0.65, 0.08, 0.92);
   DefineViewPort (8, 0.72, 1.0,  0.08, 0.43);
   DefineViewPort (9, 0.675, 1.0,  0.52, 0.92);
   DefineViewPort (11, 0.0,0.08, 0.1,0.5);
   FourierFunc.init;
   contour.init;

   Datarecord.wave:=square;
   Datarecord.mode:= initial;
   Datarecord.state:=go;
      Datarecord.numT := 40;
      Datarecord.numTset:= Datarecord.numT;
      Datarecord.numS := 200;
      Datarecord.numSset := Datarecord.numS;
      Datarecord.PulseRatio:= 0.1;
      Datarecord.slitwidth:= 6;
      Datarecord.slitpos:= 6;
      Datarecord.slitsep:= 12;
      Datarecord.PropScale:= false;
      DataRecord.fft1_num:= 0;
   picflag:= false;
   YvalsInit;
   Datarecord.yvalset:= true;
   Datarecord.mode:=fseries;
   ClearViewport;
   SetUpMenu(Menu);
   FOR iloop:= 0 TO 1 DO
      FOR jloop:= 0 TO 100 DO DataRecord.ABcoeffs[iloop,jloop]:= 0.0;
   InitF(Datarecord);
   SetUphotKeys(HotKeys,Datarecord);
   menu.rowactivate(1,5,true);
   menu.rowactivate(1,6,true);
   ShowInitialScreen;        { ShowHelp('FOURIER.HLP','Help 1' );  }
   REPEAT
      CheckForEvents;
      IF HotKeys.Pressed(key) THEN HandleHotKeys (key,Datarecord);
      IF Menu.Activated THEN HandleMenu(Datarecord);
      IF Sliders.Changed THEN HandleSliders;
      IF fft1_button.Changed THEN BEGIN
         DataRecord.fft1_num:= fft1_button.number;
         fft1_button.done;
         TsfmData.PlotFourierTransform(lightgreen,lightmagenta,cyan);
      END;
      IF (Datarecord.wave = propagate) AND (Datarecord.mode = fseries) AND
         (Datarecord.state = AUTO) THEN termf.PropagateWave(lightgreen);
      IF (Datarecord.wave = evolve) AND (Datarecord.mode = fseries) AND
         (Datarecord.state = AUTO) THEN termf.Draw_Evolve(evolvephase,lightgreen);
      IF (Datarecord.mode = fseries) AND (Datarecord.state = AUTO) AND
         (Datarecord.wave in [square..coeffs]) THEN
         BEGIN
         IF Datarecord.addterm THEN BEGIN
            Datarecord.addterm:= NOT Datarecord.addterm;
            AddtermF(Datarecord);
         END
         ELSE BEGIN
            Datarecord.addterm:= NOT Datarecord.addterm;
            ShowtermF(Datarecord);
         END;
      END;

      IF (Datarecord.mode = fft_1d) AND (Datarecord.state = go) THEN
      BEGIN
         TsfmData.Pack1Ddata;
         IF Datarecord.transform = inverse THEN
            Datarecord.mysign:= -Datarecord.mysign;
         TsfmData.FastFourierTransform(Datarecord.mysign);    {value for }
         TsfmData.Unpack1Ddata;              {isign, inverse transform -1}
         TsfmData.PlotFourierTransform(lightgreen,lightmagenta,cyan);
         Datarecord.state:= dun;
         SetUpHotKeys(HotKeys,Datarecord);
      END;

   UNTIL ExitFlag;
   Menu.Done;
   Sliders.done;
   fft1_button.done;
   IF (aRe.NCols>0) THEN
   begin ARe.Free; BIm.Free; Cmodz.Free; Power.Free; end;
   IF (yvals1.NCols>0) THEN
   begin yvals1.Free; Yvals2.Free; Yvals3.Free; end;
   CupsDone;
END.

