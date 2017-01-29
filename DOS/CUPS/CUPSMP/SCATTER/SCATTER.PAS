           (*************************************************)
           (*************************************************)
           (**                                             **)
           (**  Consortium of Upper-level Physics Software **)
           (**              ( CUPS Project )               **)
           (**      ( c) 1994 by John Wiley & Sons         **)
           (**  Compiled with Utilities Ver. 1.9 (95/09/10)**)
           (**                                             **)
           (*************************************************)
           (*************************************************)


{ ----------------------------------------------------------------------
   Program Title:    Rutherford Scattering
   Author:           Douglas E. Brandt
   Version:          0.11
   Program Summary:  This is a program consisting of several sections that
                     can be used to gain understanding of scattering
                     experiments.

   Library Files:    CRT,DOS,GRAPH,MUPPET,CUPSGRAPH,CUPSMATH
   Utilities:
   Algorithms:
   NOTE:             Each of the section files are set up so that they can
                     be compiled either as a unit or as a stand alone
                     program.  To compile any of the sections as a stand
                     alone program, the compiler define directive must be used
                     to define MAIN.  To compile to a unit, MAIN must not be
                     defined by the compiler define directive.
------------------------------------------------------------------------}
{F+}
{$M 64000, 50000, 655360}

PROGRAM Rutherford_Scattering;

USES Crt, Dos, Graph, CUPSmupp, CUPS, CUPSfunc, CUPSproc, CUPSgrph,
     CUPSgui, CUPSpars, Objects;

CONST
   ScintWindow = 21;
   Length = 200;

TYPE
   THistogram = Object
      Number_Of_Bins: Integer;
      Bin: ARRAY [0..359] OF LongInt;
      Resolution: Real;
      Color: Integer;
      BackColor: Integer;
      HistViewPort: Integer;
      HistScale: Integer;
      DetailViewPort: Integer;
      DetailScale: Integer;
      MaxHits: LongInt;
      DetailSliders: TSliders;

      PROCEDURE SetBinNumber(number: Integer);
      PROCEDURE SetResolution(newresolution: Real);
      PROCEDURE SetHColor(newcolor: Integer);
      PROCEDURE SetHBackColor(newcolor: Integer);
      PROCEDURE Add_Hit(theta: Real);
      PROCEDURE Rescale;
      PROCEDURE Clear;
      PROCEDURE Display;
      PROCEDURE Hide;
      PROCEDURE SaveDataFile(FileName: String);
      PROCEDURE SetHistViewPort(ViewPortNumber: Integer; x1,x2,y1,y2: Real);
      PROCEDURE SetHistScale(ScaleNumber: Integer);
      PROCEDURE SetDetailViewPort(ViewPortNumber: Integer; x1,x2,y1,y2: Real);
      PROCEDURE SetDetailScale(ScaleNumber: Integer);
      PROCEDURE SetMaxHits(NewMaxHits: LongInt);
   End;

   TPolarHistogram = Object(THistogram)
      BaseRadius, MaxRadius: Real;
      xCenter, yCenter: Real;
      Scale: Real;
      AngleHit: Real;
      SampleAngle: Real;
      DetailMaxHits: LongInt;
      MinBin,MaxBin: Integer;
      DetailMin: Real;
      DetailMax: Real;
      Displayed: Boolean;

      PROCEDURE Set_Center(x: Real; y: Real);
      PROCEDURE Set_Radii(inner: Real; outer: Real);
      PROCEDURE Add_Hit(theta:Real);
      PROCEDURE Clear;
      PROCEDURE Rescale;
      FUNCTION Angle(i: Integer):Real;
      FUNCTION AngleD(i: Integer): Real;
      PROCEDURE PolarLine(r1, ang1, r2, ang2: Real);
      PROCEDURE Draw;
      PROCEDURE DrawDetail;
      PROCEDURE ChangeSampleAngle(NewSampleAngle: Real);
      PROCEDURE ChangeDetailScale;
      PROCEDURE ReplotDetail;
      PROCEDURE DetailRescale;
   End;

   TScintillator = Object
      Width: Real;
      Position: Real;
      LowEdge, HighEdge: Real;
      Color: Integer;
      BackColor: Integer;
      Persist: Integer;
      ScintWindow: Integer;
      ScintScale: Integer;
      PROCEDURE ShowEvent(theta: Real);
      PROCEDURE SetWidth(newwidth: Real);
      PROCEDURE SetPosition(theta: Real);
      PROCEDURE SetPersist(newpersist: Integer);
      PROCEDURE SetHColor(newcolor: Integer);
      PROCEDURE SetHBackColor(newcolor: Integer);
      PROCEDURE SetWindow(ViewPortNumber: Integer;x1,x2,y1,y2: Real);
      PROCEDURE Display;
      PROCEDURE Erase;
      PROCEDURE SetScale(ScaleNumber: Integer; x1,x2,y1,y2: Real);
   END;

   MessageRec = Record
      Quit: Boolean;
      Done: Boolean;
      Running: Boolean;
      Paused: Boolean;
      Next: Integer;
      Change: Integer;
   END;

   Polygon = ARRAY [0..9] OF Real;

   EllipseRec = Record
      A,B: Real;
      CurrentAngle: Real;
      Alpha: Real;
      Beta: Real;
      Gamma: Real;
      Delta: Real;
   END;

   SectorRec = Record
      alpha: Real;
      CurrentAngle: Real;
      Range1Max: Real;
      Range2Min: Real;
      CircleRangeMin, CircleRangeMax: Real;
   END;

{   TProcess = Object
      Status: Integer;
      PROCEDURE Run;
      PROCEDURE Restart;
   END;}

VAR
   kNum: byte;
   done,next: integer;
   Main_menu: TMenu;
   HotKeys: THotKeys;
   Experiment_Parameter_Screen, NGonScreen: TInputScreen;
   Change_Scatterer_Screen,VerifyClearScreen,NewUnknownScreen: TInputScreen;
   Change_Collection_Screen,FunctionInputScreen: TInputScreen;
   ChangeBeamDivergenceScreen: TInputScreen;
   Detector_Width, BeamDivergence, Beam_Energy: Real;
   Detector_Efficiency, Target_Thickness: Real;
   Scatter_Density, Thickness: Real;
   Sliders: TSliders;
   DelayCount: Integer;
   ImageSave: TImage;
   CurrentHelp: HelpScrType;
   HelpScr1, ParserHelp: HelpScrType;
   Message: MessageRec;
   Hist1: TPolarHistogram;
   x,f: DVector;
   NSampleAngle, OldSampleAngle: Real;
   EllipseRecord: EllipseRec;
   SectorRecord: SectorRec;
   PolyVertex: Polygon;
   DetailCenter, DetailWidth,ScatterType: Integer;
   OldSlider: Array [1..5] of Real;
   energy: Real;
   FacesShowing: Integer;
   SinVertex: ARRAY [1..11] OF Real;
   ScatterAngle: ARRAY [1..10] OF Real;
   Beam_Width: Real;
   CalculateDeflection: FunctionXY;
   qBeam,qscatt,MBeam,mscatt,alpha,radius : Real;
   N: Integer;
   Scint1: TScintillator;
   Force: FunctionX;
   m: ReaL;
   ScattererString,OldScattererString: String[40];
   UserDef2, HistogramDrawn: Boolean;
   TotalHits: LongInt;
   TotalHitsString,MaxHitsString: String[20];
   HelpScreen: ARRAY [1..3] OF HelpScrType;
   CurrentPotential: Integer;

PROCEDURE DrawAxis( Xint, Yint, TickX, TickY: REAL; color: integer);
   VAR
      x, y, TickLength,min,max : REAL;
      Xs, Ys, n, m : integer;
      Environment : TEnvironment;

   PROCEDURE LabelTickX( VAR x : real);
   BEGIN
      WITH GraphScales[ScaleNumber] DO
      IF (x>=Min) AND (x<=Max) THEN      {if inside the plot then...}
      BEGIN
         PlotLine(x, ymin, x, ymin + TickLength);
         Map(x, ymin, Xs, Ys);
         IF (TickX>1E-5) AND (abs(x)<1E-6) THEN x := 0;
         OutTextXY(Xs, Ys + 6, Num2Str(x,7));
      END;
      x := x + TickX;
   END;

BEGIN
   Environment.Save;
   Environment.Standardize;
   SetColor(color);
   WITH Views[ViewPortNumber] DO SetViewPort(Vx1,Vy1,Vx2,Vy2,ClipOff);
   WITH GraphScales[ScaleNumber] DO
   BEGIN
      HideMouse;
      SetTextJustify(CenterText, TopText);
      IF TickX > 0 THEN
      BEGIN
         TickLength := 0.02 * (ymax - ymin);      {Tick hight}
         x := Xint;
         IF xMin<xMax THEN
          BEGIN
             max := xMax;
             min := xMin;
          END
         ELSE
          BEGIN
             max := xMin;
             min := xMax;
          END;
         WHILE x <= max DO LabelTickX(x);        {plot along  x axis}
         x := Xint;
         TickX := -TickX;
         WHILE x >= min DO LabelTickX(x);        {plot along -x axis}
      END;  {if}
      SetTextJustify(RightText, CenterText);
      ShowMouse;
      Environment.Reset;
   END; {with GraphScales}
END;

PROCEDURE autoyScaling (sNum : integer; xmin,xmax: Real; VAR y : DVector);
VAR
   min,max,Tick,CInt : point2D;
   n : integer;
BEGIN
   min[1] := xmin;
   max[1] := xmax;
   y.MinMax(n,min[2],n,max[2]);
      max[1] := max[1] + (max[1] - min[1])/40.0;
      min[1] := min[1] - (max[1] - min[1])/40.0;
      max[2] := max[2] + (max[2] - min[2])/10.0;
      min[2] := min[2] - (max[2] - min[2])/20.0;
   FOR n := 1 TO 2 DO
   BEGIN
      IF min[n]=max[n] THEN
      BEGIN
         max[n] := max[n] + 1;
         min[n] := min[n] - 1;
      END;
      Tick[n] := TickSpace ((max[n]-min[n]));
   END;
   DefineScale(snum,min[1],max[1],min[2],max[2]);
   DrawAxis(0,0,Tick[1],Tick[2],White);
END;


FUNCTION MapRadius (data :REAL) : integer;

   VAR
      Yfactor : REAL;

   BEGIN
      WITH GraphScales [ScaleNumber] DO
      WITH Views[ViewPortNumber] DO
         BEGIN
            Yfactor := (Vy2 - Vy1) / (ymax - ymin);
            MapRadius := round(Yfactor * data);
         END;
   END;

PROCEDURE MCircle(x, y, radius: Real);

   BEGIN
      Circle(MapX(x),MapY(y),MapRadius(radius));
   END;

{=========================================================================

    METHODS FOR THISTOGRAM OBJECT

==========================================================================}


PROCEDURE THistogram.SetHistViewPort(ViewPortNumber: Integer; x1,x2,y1,y2: Real);
   BEGIN
      HistViewPort := ViewPortNumber;
      DefineViewPort(HistViewPort,x1,x2,y1,y2);
   END;

PROCEDURE THistogram.SetHistScale(ScaleNumber: Integer);
   BEGIN
      HistScale := ScaleNumber;
   END;

PROCEDURE THistogram.SetDetailViewPort(ViewPortNumber: Integer; x1,x2,y1,y2: Real);
   BEGIN
      DetailViewPort := ViewPortNumber;
      DefineViewPort(DetailViewPort,x1,x2,y1,y2);
   END;

PROCEDURE THistogram.SetDetailScale(ScaleNumber: Integer);
   BEGIN
      DetailScale := ScaleNumber;
   END;

PROCEDURE TPolarHistogram.ChangeDetailScale;
   VAR
      i:Integer;
      Max: Integer;
   BEGIN
      DetailMin := DetailCenter-DetailWidth/2.0;
      DetailMax := DetailCenter+DetailWidth/2.0;
      MinBin := Trunc(DetailMin*Number_of_Bins/360) mod 360;
      MaxBin := Trunc(DetailMax*Number_of_Bins/360) mod 360;
      IF MinBin < 0 THEN MinBin := MinBin + 360;
      IF MaxBin < 0 THEN MaxBin := MaxBin + 360;
      Max := 10;
      IF MinBin < MaxBin THEN
          FOR i := MinBin TO MaxBin DO
             BEGIN
                IF Bin[i] > Max THEN Max := Bin[i]
             END
      ELSE BEGIN
              FOR i := 0 TO MaxBin DO IF Bin[i] > Max THEN Max := Bin[i];
              FOR i := MinBin TO Number_Of_Bins-1 DO
                 IF Bin[i] > Max THEN Max := Bin[i];
           END;
      DetailMaxHits := 10 * TRUNC(Pwr(2,(TRUNC(Log(2.0,Max/10.0) + 0.99))));
      SelectViewPort(4);
      ClearViewPort;
      SetColor(Yellow);
      Print(10,1,'Detail Histogram');
      OpenViewPort(DetailViewPort);
      SelectViewPort(DetailViewPort);
      DefineScale(DetailScale,DetailMin-0.05*DetailWidth,DetailMax+0.05*DetailWidth,0,DetailMaxHits);
      Axis(0,0,Trunc((DetailMax-DetailMin)/5+1),DetailMaxHits/10);
      ReplotDetail;
   END;

PROCEDURE THistogram.SetMaxHits(NewMaxHits: LongInt);
   BEGIN
      MaxHits := NewMaxHits;
   END;



PROCEDURE THistogram.SaveDataFile(Filename: String);
   BEGIN

   END;


PROCEDURE THistogram.Hide;
   BEGIN

   END;


PROCEDURE THistogram.Rescale;

   VAR
      i: Integer;

   BEGIN
      MaxHits := 2*MaxHits;
      DefineScale(HistScale,0,Number_Of_Bins+1,0,MaxHits+1);
{      SetBackColor(BackColor);}
      OpenViewPort(HistViewPort);
      SelectViewPort(HistViewPort);
      SelectScale(HistScale);
      SetColor(color);
      FOR i := 1 TO Number_Of_Bins DO PlotLine(i,0,i,Bin[i]);
   END;

PROCEDURE THistogram.Add_Hit(theta: Real);

   VAR
      BinHit: Integer;

   BEGIN
      BinHit := TRUNC(theta*Number_Of_Bins/Resolution);
      Bin[BinHit] := Bin[BinHit] + 1;
      IF Bin[BinHit] > MaxHits THEN Rescale;
      SelectViewPort(HistViewPort);
      SelectScale(HistScale);
      SetColor(Color);
      PlotLine(BinHit,0,BinHit,Bin[BinHit]);
   END;


PROCEDURE THistogram.Display;
   BEGIN

   END;

PROCEDURE THistogram.SetBinNumber(number: Integer);
   BEGIN
      Number_Of_Bins := number;
   END;

PROCEDURE THistogram.SetResolution(newresolution: Real);
   BEGIN
      Resolution := newresolution;
   END;

PROCEDURE THistogram.Clear;
   VAR
      i: Integer;
   BEGIN
      FOR i := 0 TO Number_Of_Bins DO Bin[i] := 0;
      OpenViewPort(HistViewPort);
   END;

PROCEDURE THistogram.SetHColor(newcolor: Integer);
   BEGIN
      Color := newcolor;
   END;

PROCEDURE THistogram.SetHBackColor(newcolor: Integer);
   BEGIN
      BackColor := newcolor;
   END;


{=========================================================================

    METHODS FOR TPOLARHISTOGRAM OBJECT

==========================================================================}

PROCEDURE TPolarHistogram.Set_Center(x: Real; y: Real);

   BEGIN
      xCenter := x;
      yCenter := y;
   END;

PROCEDURE TPolarHistogram.Set_Radii(inner: Real; outer: Real);

   BEGIN
      BaseRadius := inner;
      MaxRadius := outer;
   END;

PROCEDURE TPolarHistogram.Rescale;

   VAR
      i: Integer;

   BEGIN
      MaxHits := 2*MaxHits;
      Scale := (MaxRadius-BaseRadius)/MaxHits;
      SelectViewPort(HistViewPort);
      SelectScale(HistScale);
      SetColor(GraphBackColor);
      HideCursor;
      Print(15,13,MaxHitsString);
      Str(MaxHits,MaxHitsString);
      SetColor(15);
      Print(15,13,MaxHitsString);
      SetFillStyle(1,0);
      FloodFill(MapX(xcenter),MapY(ycenter+(BaseRadius+MaxRadius)/2), 15);
      SetColor(color);
      FOR i := 0 TO Number_Of_Bins-1 DO
         IF Bin[i] > 0 THEN
            PolarLine(BaseRadius,angle(i),BaseRadius+Bin[i]*Scale,angle(i));
      ShowCursor;
   END;

PROCEDURE TPolarHistogram.Add_Hit(theta: Real);

   VAR
      BinHit: Integer;
      Degrees: Boolean;
      Theta2: Real;

   BEGIN
      Degrees := True;
      Theta2 := Theta*180/Pi;
   {   SelectViewPort(9);
      Print(1,3,Num2Str(theta2,6));}
      BinHit := ROUND(theta2);
     { Print(1,4,Num2Str(BinHit,6));
      Pause;}
      TotalHits := TotalHits + 1;
      SelectViewPort(HistViewPort);
      SetColor(GraphBackColor);
      Print(17,14, TotalHitsString);
      Str(TotalHits,TotalHitsString);
      SetColor(15);
      Print(17,14,TotalHitsString);
      IF BinHit = 360 THEN BinHit := 0;
      Bin[BinHit] := Bin[BinHit] + 1;
      IF Bin[BinHit] > MaxHits THEN Rescale;
      SelectScale(HistScale);
      SetColor(Color);
      PolarLine(BaseRadius+(Bin[BinHit]-1)*Scale,angle(BinHit),
                        BaseRadius+Bin[BinHit]*Scale,angle(BinHit));
      IF Degrees THEN AngleHit := AngleD(BinHit)
           ELSE AngleHit := Angle(BinHit);
      SelectViewPort(DetailViewPort);
      SelectScale(DetailScale);
      SetColor(Color);
      IF ((MinBin<MaxBin) AND (BinHit>= MinBin) AND (BinHit <= MaxBin)) THEN
         BEGIN
            IF Bin[BinHit] > DetailMaxHits THEN DetailRescale;
            IF AngleHit+0.8 < DetailMin THEN
               PlotLine(AngleHit+360,0,AngleHit+360,Bin[BinHit])
            ELSE IF AngleHit-0.8 > DetailMax THEN
               PlotLine(AngleHit-360,0,AngleHit-360,Bin[BinHit])
            ELSE PlotLine(AngleHit,0,AngleHit,Bin[BinHit]);
         END
      ELSE IF ((MaxBin<MinBin) AND ((BinHit <= MaxBin) OR (BinHit >= MinBin))) THEN
         BEGIN
            IF Bin[BinHit] > DetailMaxHits THEN DetailRescale;
            IF AngleHit >= DetailMin THEN
               PlotLine(AngleHit,0,AngleHit,Bin[BinHit])
            ELSE PlotLine(360+AngleHit,0,360+AngleHit,Bin[BinHit]);
            IF AngleHit <= DetailMax THEN
               PlotLine(AngleHit,0,AngleHit,Bin[BinHit])
            ELSE PlotLine(AngleHit-360,0,AngleHit-360,Bin[BinHit]);
         END;

   END;

PROCEDURE TPolarHistogram.DetailRescale;

   VAR
      i: Integer;
      Width: Real;

   BEGIN
      DetailMaxHits := DetailMaxHits*2;
      Width := DetailMax-DetailMin;
      DefineScale(DetailScale,DetailMin-0.05*Width,DetailMax+0.05*Width,0,DetailMaxHits);
      SelectViewPort(4);
      ClearViewPort;
      SetColor(Yellow);
      Print(10,1,'Detail Histogram');
      OpenViewPort(DetailViewPort);
      SelectViewPort(DetailViewPort);
      SelectScale(DetailScale);
      Axis(0,0,Trunc((DetailMax-DetailMin)/5+1),DetailMaxHits/10);
      SetColor(Color);
      ReplotDetail;
   END;

PROCEDURE TPolarHistogram.ReplotDetail;

   VAR
      i: Integer;

   BEGIN
      SetColor(Yellow);
      IF MinBin<MaxBin THEN
      FOR i := MinBin TO MaxBin DO
         IF Hist1.AngleD(i) < DetailMin THEN
              PlotLine(Hist1.AngleD(i)+360,0,Hist1.AngleD(i)+360,Bin[i])
         ELSE IF Hist1.AngleD(i) > DetailMax THEN
                  PlotLine(Hist1.AngleD(i)-360,0,Hist1.AngleD(i)-360,Bin[i])
         ELSE PlotLine(Hist1.AngleD(i),0,Hist1.AngleD(i),Bin[i])
      ELSE
         BEGIN
            FOR i := 0 TO MaxBin DO
               IF Hist1.AngleD(i) > DetailMin THEN
                  PlotLine(Hist1.AngleD(i),0,Hist1.AngleD(i),Bin[i])
               ELSE PlotLine(360+Hist1.AngleD(i),0,360+Hist1.AngleD(i),Bin[i]);
            FOR i := MinBin TO Number_Of_Bins-1 DO
               IF Hist1.AngleD(i) < DetailMax THEN
                  PlotLine(Hist1.AngleD(i),0,Hist1.AngleD(i),Bin[i])
               ELSE PlotLine(Hist1.AngleD(i)-360,0,Hist1.AngleD(i)-360,Bin[i]);
         END;
   END;

PROCEDURE TPolarHistogram.Clear;
   VAR
      i: Integer;
   BEGIN
      FOR i := 0 TO Number_Of_Bins-1 DO Bin[i] := 0;
      SetMaxHits(5);
      DetailMaxHits := 10;
      Scale := (MaxRadius-BaseRadius)/MaxHits;
      DefineScale(DetailScale,DetailMin-0.05*DetailWidth,DetailMax+0.05*DetailWidth,0,DetailMaxHits);
      DefineScale(HistScale,-0.875,0.475,-0.5,0.5);
      TotalHits := 0;
      Str(TotalHits,TotalHitsString);
      Draw;
      DrawDetail;
   END;

PROCEDURE TPolarHistogram.Draw;

   VAR
      CosEnd,SinEnd: Real;
      i: Integer;

   BEGIN
      HideCursor;
      OpenViewPort(HistViewPort);
      SelectViewPort(HistViewPort);
      SelectScale(HistScale);
      SetColor(15);
      MCircle(xcenter,ycenter,BaseRadius-0.01);
      MCircle(xCenter,yCenter,MaxRadius+0.01);
      MCircle(xcenter,ycenter,BaseRadius-0.013);
      MCircle(xCenter,yCenter,MaxRadius+0.013);
      FOR i := 0 TO 11 DO
         PolarLine(MaxRadius,Pi*i/6,MaxRadius+0.02,Pi*i/6);
      SetFillStyle(1,0);
      FloodFill(MapX(xcenter),MapY(ycenter+(BaseRadius+MaxRadius)/2), 15);
      CosEnd := 0.05*cos(sampleAngle);
      SinEnd := 0.05*sin(sampleAngle);
      PlotLine(-CosEnd,-SinEnd,2*CosEnd,2*SinEnd);
      PlotLine(SinEnd,-CosEnd,-SinEnd,CosEnd);
      SetColor(13);
      PlotLine(-0.8,0.06,-0.6,0.06);
      PlotLine(-0.6,0.06,-0.6,0.1);
      PlotLine(-0.6,0.1,-0.5,0.0);
      PlotLine(-0.5,0.0,-0.6,-0.1);
      PlotLine(-0.6,-0.1,-0.6,-0.06);
      PlotLine(-0.6,-0.06,-0.8,-0.06);
      PlotLine(-0.8,-0.06,-0.8,0.06);
      DefineViewPort(14,0.0,0.3,0.30,0.44);
      SelectViewPort(14);
      Print(4,1,'Incident');
      {Print(4,7,'Incident');}
      SelectViewPort(HistViewPort);
      Print(6,8,'Beam');
      SetColor(15);
      Print(2,13,'Full Scale = ');
      Str(MaxHits,MaxHitsString);
      Print(15,13,MaxHitsString);
      Print(2,14,'Total Events = ');
      Print(17,14,TotalHitsString);
      ShowCursor;
   END;

PROCEDURE TPolarHistogram.DrawDetail;
   BEGIN
      HideCursor;
      SetColor(15);
      SelectViewPort(4);
      ClearViewPort;
      SelectViewPort(4);
      SetColor(Yellow);
      Print(10,1, 'Detail Histogram');
      OpenViewPort(DetailViewPort);
      SelectViewPort(DetailViewPort);
      SelectScale(DetailScale);
      Axis(0,0,Trunc((DetailMax-DetailMin)/5+1),DetailMaxHits/10);
      ShowCursor;
   END;
            
PROCEDURE TPolarHistogram.ChangeSampleAngle(NewSampleAngle: Real);

   VAR
      CosEnd, SinEnd: Real;

   BEGIN
      SelectViewPort(HistViewPort);
      SelectScale(HistScale);
      SetColor(GraphBackColor);
      CosEnd := 0.05*cos(sampleAngle);
      SinEnd := 0.05*sin(sampleAngle);
      PlotLine(-CosEnd,-SinEnd,2*CosEnd,2*SinEnd);
      PlotLine(SinEnd,-CosEnd,-SinEnd,CosEnd);
      SampleAngle := NewSampleAngle;
      SetColor(15);
      CosEnd := 0.05*cos(sampleAngle);
      SinEnd := 0.05*sin(sampleAngle);
      PlotLine(-CosEnd,-SinEnd,2*CosEnd,2*SinEnd);
      PlotLine(SinEnd,-CosEnd,-SinEnd,CosEnd);
   END;


FUNCTION TPolarHistogram.Angle(i: Integer): Real;
   BEGIN
      Angle := i * 2*pi/Number_Of_Bins;
   END;

FUNCTION TPolarHistogram.AngleD(i: Integer): Real;
   VAR
      Temp: Real;
   BEGIN
      Temp := i*360.0/Number_Of_Bins;
      IF Temp > 180 THEN AngleD := Temp-360 ELSE AngleD := Temp;
   END;

PROCEDURE TPolarHistogram.PolarLine(r1, ang1, r2, ang2: Real);
   BEGIN
      PlotLine(r1*cos(ang1),r1*sin(ang1),r2*cos(ang2),r2*sin(ang2));
   END;


{========================================================================

     METHODS FOR TSCINTILLATOR OBJECT

=========================================================================}

PROCEDURE TScintillator.SetScale(ScaleNumber: Integer; x1,x2,y1,y2: Real);
   BEGIN
      ScintScale := ScaleNumber;
      DefineScale(ScintScale,x1,x2,y1,y2);
   END;


PROCEDURE TScintillator.SetWindow(ViewPortNumber: Integer; x1,x2,y1,y2: Real);
   BEGIN
      Position := 0;
      Width := 1;
      ScintWindow := ViewPortNumber;
      DefineViewPort(ScintWindow,x1,x2,y1,y2);
      DefineViewPort(scintWindow+1,x1-0.05,x2+0.05,y1-0.05,y1);
      SelectViewPort(scintwindow+1);
      ClearViewPort;
      OpenViewPort(ScintWindow);
   END;

PROCEDURE TScintillator.ShowEvent(theta: Real);

   VAR
      VertPos: Real;
      i: Integer;
      x: Integer;

   BEGIN
      VertPos := Random;
      SelectViewPort(ScintWindow);
      SelectScale(ScintScale);
      SetColor(LightGreen);
      theta := 180*theta/Pi;
      PlotSymbol(theta, VertPos,'.');
      IF persist > 0 THEN
        BEGIN
           FOR i := 0 TO persist DO x := 1;
           SetColor(GraphBackColor);
           PlotSymbol(theta, VertPos,'.');
        END;
   END;


PROCEDURE TScintillator.Display;

   VAR
      Tick: Real;

   BEGIN
      DefineViewPort(16,0.0,1.0,0.90,0.95);
      SelectViewPort(16);
      SetColor(15);
      Print(31,1,'Scintillation Screen');
      SelectViewPort(16);
      SelectViewPort(scintWindow+1);
      ClearViewPort;
      OpenViewPort(ScintWindow);
      Tick := TickSpace(width);
      DrawAxis(0,0,Tick,0,White);
   END;

PROCEDURE TScintillator.Erase;
   BEGIN

   END;

PROCEDURE TScintillator.SetPersist(newpersist: Integer);
   BEGIN
      Persist := newpersist;
   END;

PROCEDURE TScintillator.SetHColor(newcolor: Integer);
   BEGIN
      Color := newcolor;
   END;

PROCEDURE TScintillator.SetHBackColor(newcolor: Integer);
   BEGIN
      BackColor := newcolor;
   END;

PROCEDURE TScintillator.SetWidth(newwidth: Real);
   BEGIN
      Width := newwidth;
      Lowedge := Position-width;
      HighEdge := Position+ Width;
      DefineScale(ScintScale,Position-Width/2,Position+Width/2,-0.1,1.1);
   END;

PROCEDURE TScintillator.SetPosition(theta: Real);
   BEGIN
      Position := theta;
      Lowedge := Position-width;
      HighEdge := Position+ Width;
      DefineScale(ScintScale,Position-Width/2,Position+Width/2,-0.1,1.1);
   END;


PROCEDURE CreateNGon(N: Integer; VAR PolyVertex: Polygon;
                                             SampleAngle: Real); FORWARD;
PROCEDURE FindPolyScattering(PolyVertex: Polygon;
                                             SampleAngle: Real); FORWARD;

PROCEDURE Initialize;

   BEGIN
      CUPSinit;
      DefineViewPort(17,0,1.0,0.05,0.95);
      Main_Menu.init;
      Main_Menu.Column(1,'File');
         Main_Menu.Row(1,1,'About CUPS');
         Main_Menu.Row(1,2,'About Program');
         Main_Menu.Row(1,3,'Configuration');
         Main_Menu.Row(1,4,'-------------');
         Main_Menu.Row(1,5,'Save');
         Main_Menu.Row(1,6,'Load');
         Main_Menu.Row(1,7,'-------------');
         Main_Menu.Row(1,8,'Exit Program');
         Main_Menu.rowactivate(1,4,false);
         Main_Menu.rowactivate(1,7,false);
      Main_Menu.Column(2,'Mode');
         Main_Menu.Row(2,1,'Geiger-Marsden Experiment');
         Main_Menu.Row(2,2,'Single Scatterer');
         {Main_Menu.Row(2,3,'A Collection of Scatterers');}
         Main_Menu.Row(2,3,'Guess the Scatterer');
      Main_Menu.Column(3,'Parameters');
         Main_Menu.Row(3,1,'Beam Parameters');
         {Main_Menu.Row(3,2,'Beam Particle Charge');
         Main_Menu.Row(3,3,'Beam particle Mass');
         Main_Menu.RowActivate(3,2,False);
         Main_Menu.RowActivate(3,3,False);}
      Main_Menu.Column(4,'Force');
         Main_Menu.Row(4,1, 'Hard Sphere');
         Main_Menu.Row(4,2, 'Regular N-Gon (N < 10)');
         Main_Menu.Row(4,3, '1/r');
         Main_Menu.Row(4,4, '1/r^2 Exact');
         Main_Menu.Row(4,5, '1/r^2 Integrated');
         Main_Menu.Row(4,6, '1/r^3');
         Main_Menu.Row(4,7, 'Yukawa');
         Main_Menu.Row(4,8, 'Uniform Scattering');
         Main_Menu.Row(4,9, 'User Defined 1: F(r)');
         Main_Menu.Row(4,10, 'User Defined 2: F(x,y,vx,vy)');
      Main_Menu.Column(5,'Plot');
         Main_Menu.Row(5,1,'Plot Function Over Data');
      Main_Menu.AutoChecking(2,2);
      Main_Menu.AutoChecking(4,1);
      {Main_Menu.Row(5,3,'Distribution');
      Main_Menu.Column(6,'Beam');
      Main_Menu.Row(6,1,'Energy');
      Main_Menu.Row(6,2,'Flux');
      Main_Menu.Row(6,2,'Divergence');
      Main_Menu.Column(5,'Detector');
      Main_Menu.Row(5,1,'Angular Resolution');
      Main_Menu.Row(5,2,'Efficiency');}
      Main_menu.Display;
      hotKeys.init(4);
      hotKeys.key[1] := 'F1-Help';
      hotKeys.key[2] := 'F2-Run/Stop';
      hotKeys.key[3] := 'F3-Clear';
{      hotKeys.key[4] := 'F4-    ';}
      hotKeys.key[4] := 'F10-Menu';
      hotKeys.Display;
   END;

PROCEDURE EnableMenuColumn4(Enable: Boolean);

   BEGIN
      IF Enable THEN
         BEGIN
            Main_Menu.RowActivate(4,1,True);
            Main_Menu.RowActivate(4,2,True);
            Main_Menu.RowActivate(4,3,True);
            Main_Menu.RowActivate(4,4,True);
            Main_Menu.RowActivate(4,5,True);
            Main_Menu.RowActivate(4,6,True);
            Main_Menu.RowActivate(4,7,True);
            Main_Menu.RowActivate(4,8,True);
            Main_Menu.RowActivate(4,9,True);
            Main_Menu.RowActivate(4,10,True);
         END
      ELSE
         BEGIN
            Main_Menu.RowActivate(4,1,False);
            Main_Menu.RowActivate(4,2,False);
            Main_Menu.RowActivate(4,3,False);
            Main_Menu.RowActivate(4,4,False);
            Main_Menu.RowActivate(4,5,False);
            Main_Menu.RowActivate(4,6,False);
            Main_Menu.RowActivate(4,7,False);
            Main_Menu.RowActivate(4,8,False);
            Main_Menu.RowActivate(4,9,False);
            Main_Menu.RowActivate(4,10,False);
         END;
   END;

{=========================================================================

     Information and Help Screens

==========================================================================}


PROCEDURE AboutRutherford;
   VAR
      AboutScr : HelpScrType;
   BEGIN              {                                                  }
      AboutScr[01] := '                                                  ';
      AboutScr[02] := '                CUPS MODERN PHYSICS               ';
      AboutScr[03] := '               RUTHERFORD SCATTERING              ';
      AboutScr[04] := '                                                  ';
      AboutScr[05] := '                        by                        ';
      AboutScr[06] := '                 Douglas E. Brandt                ';
      AboutScr[07] := '           Eastern Illinois University            ';
      AboutScr[08] := '                                                  ';
      AboutScr[09] := '          Version 0.12     May 24, 1993           ';
      AboutScr[10] := '                Copyright (c) 1993                ';
      AboutScr[11] := '                                                  ';
      AboutScr[12] := '                                                  ';
      AboutScr[13] := ' Rutherford Scattering is a program that allows   ';
      AboutScr[14] := ' students to investigate classical scattering of  ';
      AboutScr[15] := ' particles through simulations of particle        ';
      AboutScr[16] := ' scattering trajectories.  It is intended to help ';
      AboutScr[17] := ' users to develop an understanding of how  a      ';
      AboutScr[18] := ' scattering experiment is useful in probing matter';
      AboutScr[19] := ' and to gain understanding of differential cross-                                                 ';
      AboutScr[20] := ' section.                                                 ';
      AboutScr[21] := '                                                  ';
      AboutScr[22] := '                                                  ';
      AboutScr[23] := '                                                  ';
      AboutScr[24] := '    Press any Key or Click Mouse to continue.     ';
      AboutScr[25] := '                                                  ';
      Help(AboutScr);
   END;



PROCEDURE StartupScreen;
   VAR
      AboutScr : HelpScrType;
   BEGIN              {                                                  }
      AboutScr[01] := '                                                  ';
      AboutScr[02] := '                CUPS MODERN PHYSICS               ';
      AboutScr[03] := '               RUTHERFORD SCATTERING              ';
      AboutScr[04] := '                                                  ';
      AboutScr[05] := '                        by                        ';
      AboutScr[06] := '                 Douglas E. Brandt                ';
      AboutScr[07] := '           Eastern Illinois University            ';
      AboutScr[08] := '                                                  ';
      AboutScr[09] := '          Version 0.12     May 24, 1993           ';
      AboutScr[10] := '                Copyright (c) 1993                ';
      AboutScr[11] := '                                                  ';
      AboutScr[12] := '                                                  ';
      AboutScr[13] := ' Rutherford Scattering is a program that allows   ';
      AboutScr[14] := ' students to investigate classical scattering of  ';
      AboutScr[15] := ' particles through simulations of particle        ';
      AboutScr[16] := ' scattering trajectories.  It is intended to help ';
      AboutScr[17] := ' users to develop an understanding of how  a      ';
      AboutScr[18] := ' scattering experiment is useful in probing matter';
      AboutScr[19] := ' and to gain understanding of differential cross-                                                 ';
      AboutScr[20] := ' section.                                                 ';
      AboutScr[21] := '                                                  ';
      AboutScr[22] := '                                                  ';
      AboutScr[23] := '                                                  ';
      AboutScr[24] := '    Press any Key or Click Mouse to continue.     ';
      AboutScr[25] := '                                                  ';
      Help(AboutScr);
   END;


PROCEDURE InitHelpscreens;

   BEGIN              {                                                  }
      HelpScreen[1,01] := '                                                  ';
      HelpScreen[1,02] := '     HELP FOR GEIGER MARSDEN EXPERIMENT MODE      ';
      HelpScreen[1,03] := '                                                  ';
      HelpScreen[1,04] := ' The purpose of this mode is that the user might  ';
      HelpScreen[1,05] := ' gain some appreciation of the type of experi-    ';
      HelpScreen[1,06] := ' mental work that was performed by Rutherford and ';
      HelpScreen[1,07] := ' his colleagues in gathering data that support the';
      HelpScreen[1,08] := ' currently accepted model of the nuclear atom.    ';
      HelpScreen[1,09] := '                                                  ';
      HelpScreen[1,10] := ' The first screen shows a diagram of the simple   ';
      HelpScreen[1,11] := ' apparatus used by Geiger and Marsden to first    ';
      HelpScreen[1,12] := ' observe large angle scattering of alpha particles';
      HelpScreen[1,13] := ' incident on a thin piece of solid material.      ';
      HelpScreen[1,14] := ' ';
      HelpScreen[1,15] := ' After continuing from the first screen, a scint- ';
      HelpScreen[1,16] := ' illation screen is shown.  The sliders below the ';
      HelpScreen[1,17] := ' screen can be used to adjust angular direction   ';
      HelpScreen[1,18] := ' and width of the screen.  Also provided is a     ';
      HelpScreen[1,19] := ' slider to adjust source activity.  The PERSIST   ';
      HelpScreen[1,20] := ' ON/OFF hot key can be used to toggle the screen  ';
      HelpScreen[1,21] := ' to be have as either film or a scintillation     ';
      HelpScreen[1,22] := ' screen.';
      HelpScreen[1,23] := ' ';
      HelpScreen[1,24] := '    Press any Key or Click Mouse to continue.     ';
      HelpScreen[1,25] := '                                                  ';
      HelpScreen[2,01] := '                                                  ';
      HelpScreen[2,02] := '           HELP FOR SINGLE SCATTER MODE           ';
      HelpScreen[2,03] := '                                                  ';
      HelpScreen[2,04] := '   Select a scattering force from the FORCE menu  ';
      HelpScreen[2,05] := ' item.  Various scattering experiment simulations ';
      HelpScreen[2,06] := ' can then be performed.  The lower left window    ';
      HelpScreen[2,07] := ' displays a polar histogram of the scattering     ';
      HelpScreen[2,08] := ' events into one degree wide channels.  The window';
      HelpScreen[2,09] := ' on the right is a detail of the polar histogram  ';
      HelpScreen[2,10] := ' with width and direction adjustable with the     ';
      HelpScreen[2,11] := ' slider controls beneath it.  The sliders above   ';
      HelpScreen[2,12] := ' detail histogram control beam width, beam energy ';
      HelpScreen[2,13] := ' and the orientation of the scatterer.  Additional';
      HelpScreen[2,14] := ' beam parameters can be adjusted selecting the    ';
      HelpScreen[2,15] := ' menu item under the PARAMETERS menu.             ';
      HelpScreen[2,16] := '                                                  ';
      HelpScreen[2,17] := ' A function may be plotted in the detail histogram';
      HelpScreen[2,18] := ' to check the fit of the data against a cross-    ';
      HelpScreen[2,19] := ' section calculated from theory by selecting the  ';
      HelpScreen[2,20] := ' menu item under the PLOT menu.                   ';
      HelpScreen[2,21] := '                                                  ';
      HelpScreen[2,22] := '                                                  ';
      HelpScreen[2,23] := '                                                  ';
      HelpScreen[2,24] := '    Press any Key or Click Mouse to continue.     ';
      HelpScreen[2,25] := '                                                  ';
      HelpScreen[3,01] := '                                                  ';
      HelpScreen[3,02] := '        HELP FOR GUESS THE SCATTERER MODE         ';
      HelpScreen[3,03] := '                                                  ';
      HelpScreen[3,04] := '   This mode provides an exercise in inferring    ';
      HelpScreen[3,05] := ' properties of the objects scattering from the    ';
      HelpScreen[3,06] := ' data gathered in simulated scattering experi-    ';
      HelpScreen[3,07] := ' ments.  The program chooses a scatterer randomly ';
      HelpScreen[3,08] := ' and the user can perform various scattering      ';
      HelpScreen[3,09] := ' experiments to try to determine the nature of the';
      HelpScreen[3,10] := ' scatterer.  The same parameters of the experiment';
      HelpScreen[3,11] := ' can be adjusted as in the SINGLE SCATTERER MODE. ';
      HelpScreen[3,12] := '                                                  ';
      HelpScreen[3,13] := '   The user should be able to determine the form  ';
      HelpScreen[3,14] := ' of scattering force, as well as the charge and/or';
      HelpScreen[3,15] := ' size of the scatterer as appropriate.            ';
      HelpScreen[3,16] := ' ';
      HelpScreen[3,17] := '   Once the user believes they have inferred a    ';
      HelpScreen[3,18] := ' correct model of the scatterer, they can check if';
      HelpScreen[3,19] := ' their model is correct by using the SHOW UNKNWN  ';
      HelpScreen[3,20] := ' hot key.';
      HelpScreen[3,21] := '                                                  ';
      HelpScreen[3,22] := '   A new unknown can be selected at any time with ';
      HelpScreen[3,23] := ' the NEW UNKNWN hot key.                          ';
      HelpScreen[3,24] := '    Press any Key or Click Mouse to continue.     ';
      HelpScreen[3,25] := '                                                  ';
   END;


{=========================================================================

    Define Input Screens

==========================================================================}

PROCEDURE Define_Input_Screens;

   BEGIN
      WITH ChangeBeamDivergenceScreen DO
      BEGIN
         init;
         DefineInputPort(0.17,0.83,0.23,0.63);
         Loadline('                                                   ');
         Loadline('                 BEAM PARAMETERS                   ');
         Loadline('                                                   ');
         Loadline('Angular Divergence of Beam in Degrees = {    }(0-6)');
         Loadline('Charge (q) of Beam Particles = {    }(0-10)        ');
         Loadline('Mass (m) of Beam Particles = {      }(1-10000)     ');
         Loadline('                                                   ');
         Loadline('                 [  Ok  ] [Cancel]                 ');
         SetNumber(1,0.0);
         SetNumberLimits(1,0,6);
         SetNumber(2,0.01);
         SetNumber(3,1);
         SetNumberLimits(2,0,10);
         SetNumberLimits(3,1,10000);
      END;
      WITH Change_Scatterer_Screen DO
      BEGIN
         init;
         DefineInputPort(0.23,0.77,0.12,0.77);
         Loadline('                                         ');
         Loadline('        PROPERTIES OF SCATTERERS         ');
         Loadline('                                         ');
         Loadline(' Charge of Particles in Beam (Q) = {    } ');
         Loadline('                                         ');
         Loadline(' Charge of Target Particles (q) = {    } ');
         Loadline('                                         ');
         Loadline(' Mass of Particles in Beam (M) = {    }  ');
         Loadline('                                         ');
         Loadline(' Mass of Target Particles (m) = {    }   ');
         Loadline('                                         ');
         Loadline(' Scale Radius (R) = {    }               ');
         Loadline('                                         ');
         Loadline('            [  Ok  ] [Cancel]            ');
         SetNumber(1,0.01);
         SetNumber(2,1.0);
         SetNumber(3,1.0);
         SetNumber(4,1.0);
         SetNumber(5,1.0);
      END;
      WITH Change_Collection_Screen DO
      BEGIN
         init;
         DefineInputPort(0.1,0.55,0.1,0.5);
         Loadline('                                  ');
         Loadline('    COLLECTION OF SCATTERERS      ');
         Loadline('                                  ');
         Loadline(' Density of Scatterers = {    }   ');
         Loadline('                                  ');
         Loadline(' Thickness of Target = {    }     ');
         Loadline('                                  ');
         Loadline('        [  Ok  ] [Cancel]         ');
         SetNumber(1, 0);
         SetNumber(2, 0);
      END;
      WITH NGonScreen DO
         BEGIN
            init;
            DefineInputPort(0.1,0.55,0.5,0.9);
            LoadLine('                                  ');
            LoadLine('  HARD REGULAR POLYGON SCATTERER  ');
            LoadLine('                                  ');
            LoadLine('  Number of Sides = {  } (3-9)    ');
            LoadLine('                                  ');
            LoadLine('  Diameter = {     } (>0)         ');
            Loadline('                                  ');
            Loadline('        [  Ok  ] [Cancel]         ');
            SetNumber(1,3);
            SetNumber(2,1.0);
         END;
      WITH VerifyClearScreen DO
         BEGIN
            init;
            DefineInputPort(0.25,0.75,0.5,0.75);
            LoadLine('                                      ');
            LoadLine(' ARE YOU SURE YOU WANT TO ERASE DATA?' );
            LoadLine('                                      ');
            Loadline('          [  Ok  ] [Cancel]           ');
         END;
      WITH NewUnknownScreen DO
         BEGIN
            init;
            DefineInputPort(0.25,0.75,0.5,0.75);
            LoadLine('                                      ');
            LoadLine(' ARE YOU SURE YOU WANT A NEW UNKNOWN? ');
            LoadLine('                                      ');
            Loadline('          [  Ok  ] [Cancel]           ');
         END;
      WITH FunctionInputScreen DO
         BEGIN
            init;
            DefineInputPort(0.00,0.6,0.25,0.6);
            LoadLine('                                      ');
            LoadLine(' Input a function of angle, x, in radians to');
            LoadLine(' plot over data in the detail histogram.    ');
            LoadLine('                                            ');
            LoadLine(' f(x) = "                                  "');
            LoadLine('                                            ');
            Loadline('          [  Ok  ] [Cancel] [ Help ]        ');
            AboutParser(ParserHelp);
            SetHelpScreen(ParserHelp);

         END;
   END;

PROCEDURE CreateSliders;

   VAR i: Integer;

   BEGIN
      Sliders.Init;
      Sliders.Create(1,-180,180,0,0.7,0.18,0.96,0,'-180','180','Center',False);
      Sliders.Create(2,1,360,20,0.7,0.10,0.96,0,'1','360','Width',False);
      Sliders.Create(3,-180.0,180.0,0,0.65,0.7,0.98,0,'-180','180','Orientation',False);
      Sliders.Create(4,0.1,10,1,0.65,0.78,0.98,2,'0.1','10','Beam Energy',False);
      Sliders.Create(5,0.1,2,1,0.65,0.86,0.98,2,'0.1','2.0','Beam Width',False);
      FOR i := 1 TO 5 DO OldSlider[i] := Sliders.Value(i);
      Sliders.DrawAll;
   END;

PROCEDURE VerifyClear;
   BEGIN
      WITH VerifyClearScreen DO
         BEGIN
            AcceptScreen;
            IF NOT Canceled THEN
               IF Message.Next <> 1 THEN Hist1.Clear
               ELSE scint1.Display;
         END;
   END;

PROCEDURE PrintScattererString;
   BEGIN
      SelectViewPort(6);
      RubOut(22,1,24,GraphBackColor);
      SetColor(15);
      Print(22,1,ScattererString);
   END;


PROCEDURE Draw_Scatterer;

   VAR
      i: Integer;
      radius1,radius2: Real;
      maxradius, minradius: Real;
      ScattererString: String;
      MaxX,MinX,MaxY,MinY,Scale,ShiftX,ShiftY: Real;

   BEGIN
      DefineViewPort(18,0.22,0.78,0.2,0.8);
      SelectViewPort(18);
      DefineScale(18,-1.2072,1.2072,-1,1);
      SelectScale(18);
      HideCursor;
      ImageSave.Get(MapX(-1.2072),MapY(1),MapX(1.2072),MapY(-1));
      SetColor(15);
      GraphBackColor := Red;
      OpenViewPort(18);
      PRINT(15,1,'UNKNOWN SCATTERER');
      PRINT(3,14,'PRESS ANY KEY OR CLICK MOUSE TO CONTINUE');
      SetColor(DarkGray);
      SetFillStyle(1,DarkGray);
      CASE ScatterType OF
         1: BEGIN
               SetColor(White);
               Print(2,2,'Scatterer is a circle of radius');
               Print(2,3,Num2Str(radius,6) +'.');
               SetColor(DarkGray);
               Circle(MapX(0),MapY(0),MapX(0.5)-MapX(0));
               FloodFill(MapX(0),MapY(0),DarkGray);
            END;
         2: BEGIN
               CASE N of
                  3: ScattererString := 'an equilateral triangle';
                  4: ScattererString := 'a square';
                  5: ScattererString := 'a regular pentagon';
                  6: ScattererString := 'a regular hexagon';
                  7: ScattererString := 'a regular heptagon';
                  8: ScattererString := 'a regular octagon';
                  9: ScattererString := 'a regular nonagon';
               END;
               SetColor(White);
               Print(6,2,'Scatterer is '+ScattererString+'.');
               Print(6,3,'Sides are of length '+Num2Str(radius*2*cos(2*pi/N),4)+'.');
               SetColor(DarkGray);
               FOR i := 1 TO N DO
                  BEGIN
                     PlotLine(0.5*cos((i-1)*2*pi/N),0.5*sin((i-1)*2*pi/N),
                               0.5*cos(i*2*pi/N),0.5*sin(i*2*pi/N));
                  END;
               FloodFill(MapX(0),MapY(0),DarkGray);
            END;
         3: BEGIN
               SetColor(15);
               Print(2,5,'Scattering force is inversely proportional');
               Print(2,6,'to distance: F = qQ/r.');
               Print(2,8,'The value of Q is '+Num2Str(Qscatt,5)+'.');
            END;
         4: BEGIN
               SetColor(15);
               Print(2,5,'Scattering force is inversely proportional to');
               Print(2,6,'to distance squared: F = qQ/r^2.');
               Print(2,8,'The value of Q is '+Num2Str(Qscatt,5)+'.');
            END;
         5: BEGIN
               SetColor(15);
               Print(2,5,'Scattering force is inversely proportional');
               Print(2,6,'to distance squared: F = qQ/r^2');
               Print(2,8,'The value of Q is '+Num2Str(Qscatt,5)+'.');
            END;
         6: BEGIN
               SetColor(15);
               Print(2,5,'Scattering force is inversely proportional');
               Print(2,6,'to distance cubed: F = qQ/r^3.');
               Print(2,8,'The value of Q is '+Num2Str(Qscatt,5)+'.');
            END;
         7: BEGIN
               SetColor(15);
               Print(2,5,'Scattering force is exponentially');
               Print(2,6,'dependent on distance: F = qQexp(-r/L).');
               Print(2,8,'The value of Q is '+Num2Str(Qscatt,5)+'.');
               Print(2,10,'The value of L is '+Num2Str(radius,5)+'.');
            END;
         11: BEGIN
                MaxX := 0;
                MaxY := 0;
                MinX := 0;
                MinY := 0;
                SetColor(DarkGray);
                CASE Round(PolyVertex[0]) of
                  3: ScattererString := 'a triangle';
                  4: ScattererString := 'a quadrilateral';
                  5: ScattererString := 'a pentagon';
                  6: ScattererString := 'a hexagon';
                  7: ScattererString := 'a heptagon';
                  8: ScattererString := 'an octagon';
                  9: ScattererString := 'a nonagon';
               END;
                FOR i := 1 TO Round(PolyVertex[0]) DO
                   BEGIN
                      IF cos(PolyVertex[i]) > MaxX THEN
                          MaxX := cos(PolyVertex[i]);
                      IF cos(PolyVertex[i]) < MinX THEN
                          MinX := cos(PolyVertex[i]);
                      IF sin(PolyVertex[i]) > MaxY THEN
                          MaxY := sin(PolyVertex[i]);
                      IF sin(PolyVertex[i]) < MinY THEN
                          MinY := sin(PolyVertex[i]);
                   END;
                IF MaxX-Minx >= MaxY-MinY THEN Scale := (MaxX-MinX)*0.3
                ELSE Scale := (MaxY-MinY)*0.3;
                ShiftX := -(MaxX+MinX)/2;
                ShiftY := -(MaxY+MinY)/2;
                FOR i := 1 TO Round(PolyVertex[0]-1) DO
                    PlotLine(Scale*(cos(PolyVertex[i])+ShiftX),
                             Scale*(sin(PolyVertex[i])+ShiftY),
                             Scale*(cos(PolyVertex[i+1])+ShiftX),
                             Scale*(sin(PolyVertex[i+1])+ShiftY));
                PlotLine(Scale*(cos(PolyVertex[Round(PolyVertex[0])])+ShiftX),
                         Scale*(sin(PolyVertex[Round(PolyVertex[0])])+ShiftY),
                         Scale*(cos(PolyVertex[1])+ShiftX),
                         Scale*(sin(PolyVertex[1])+ShiftY));
                FloodFill(Mapx(0),MapY(0),DarkGray);
                SetColor(White);
                Print(2,2,'Scatterer is ' + scattererString+'.');
                Print(2,3,'A circumscribed circle has radius '+
                                                      Num2Str(radius,4)+'.');
             END;
         12: BEGIN
                Radius1 := 1/sqrt(EllipseRecord.a);
                Radius2 := 1/sqrt(EllipseRecord.b);
                IF Radius1 >= Radius2 THEN
                   BEGIN
                      maxradius := radius1;
                      minradius := radius2;
                      radius2 := radius2/radius1;
                      radius1 := 1.0;
                   END
                ELSE
                   BEGIN
                      maxradius := radius2;
                      minradius := radius1;
                      radius1 := radius1/radius2;
                      radius2 := 1.0;
                   END;
                SetColor(DarkGray);
                Ellipse(MapX(0),MapY(0),0,360,(MAPX(radius1)-MapX(0)) DIV 2,
                                              (Mapy(0)-MAPY(radius2)) DIV 2);
                FloodFill(MapX(0),MapY(0),DarkGray);
                SetColor(White);
                Print(4,2,'Scatterer is an ellipse with semimajor');
                Print(4,3,'axis '+Num2Str(maxradius,6) + ' and semiminor axis '
                            + Num2Str(minradius,6) + '.');
             END;
         13: BEGIN
               SetColor(DarkGray);
               Arc(MapX(0),MapY(0),180-Trunc(180*SectorRecord.Alpha/2/Pi)-1,
                       180+Trunc(180*SectorRecord.Alpha/2/Pi)+1,MAPX(0.5)-MapX(0));
               PlotLine(0,0,Cos(Pi-SectorRecord.Alpha/2)/1.95,
                               Sin(Pi-SectorRecord.Alpha/2)/1.95);
               PlotLine(0,0,Cos(Pi+SectorRecord.Alpha/2)/1.95,
                               Sin(Pi+SectorRecord.Alpha/2)/1.95);
               FloodFill(MapX(-0.25),MapY(0),DarkGray);
               SetColor(White);
                 Print(2,2,'Scatterer is a sector of a circle with');
                 Print(2,3,'radius ' + Num2Str(radius,6) + ' and angle '+
                    Num2Str(180*SectorRecord.Alpha/Pi,6) + ' degrees.');
             END;
          14: BEGIN
                 SetColor(DarkGray);
                 PlotLine(0.01,-0.5,0.01,0.5);
                 SetColor(White);
                 Print(2,2,'Scatterer is a line element of length '+Num2Str(radius,4)+'.');
              END;
      END;
      {IF Ngon THEN
         FOR i := 1 TO Trunc(PolyVertex[0]) DO
            PlotLine(            }
      IF ((ScatterType < 3) OR (ScatterType > 7)) THEN
         BEGIN
            SetColor(LightGray);
            PlotLine(-0.2,0.0,0.4,0.0);
            PlotLine(0.0,0.2,0.0,-0.2);
         END;
      GraphBackColor := DarkGray;
      REPEAT CheckForEvents UNTIL (Event.KeyPressed OR Event.MouseClicked);
      ImageSave.Put(MapX(-1.2072),MapY(1));
      ShowCursor;
      ImageSave.Free;
   END;

PROCEDURE Show_Event;

   BEGIN

   END;

{=========================================================================

     Handle the Input Screens

==========================================================================}



PROCEDURE CurveFit(VAR Redo: Boolean);

   CONST
      Length = 200;

   VAR
      i: Integer;
      t: Real;
      Parser: TParser;
      q,y,z,tt: Char;

   BEGIN
      WITH FunctionInputScreen DO
         BEGIN
            Redo := False;
            AcceptScreen;
            IF NOT Canceled THEN
               BEGIN
               q := 'x';
               Parser.init;
               IF Parser.parse(q,y,z,tt,GetString(1)) THEN
                  BEGIN
                     SetColor(DarkGray);
                     SelectViewPort(HIst1.DetailViewPort);
                     SelectScale(Hist1.DetailScale);
                     PlotDVectors(x,f,1,Length);
                     SetColor(White);
                     PlotLine(Hist1.DetailMin,0,Hist1.DetailMax,0);
                     Hist1.ReplotDetail;
                     FOR i := 1 TO Length DO x.put(i,(Hist1.DetailMax-Hist1.DetailMin)*(i-1)/(Length-1) + Hist1.DetailMin);
                     FOR i := 1 TO Length DO
                        f.put(i,Parser.f(x.value(i),t,t,t));
                     SetColor(lightgreen);
                     PlotDVectors(x,f,1,Length);
                     Redo := True;
                  END
               ELSE
                  BEGIN
                     announce('Error: Parser could not parse function');
                     ReDo := True;
                  END;{ELSE}
            END;
         END;
   END;

PROCEDURE PrintParameters;

   VAR
      PrintString: String[8];

   BEGIN
      SelectViewPort(6);
      SetColor(15);
      PrintString := Num2Str(MBeam,6);
      RubOut(15,3,6,GraphBackColor);
      Print(15,3,PrintString);
      IF Message.Next <> 3 THEN PrintString := Num2Str(mscatt,6) ELSE
          PrintString := '?';
      RubOut(40,3,6,GraphBackColor);
      Print(40,3,PrintString);
      PrintString := Num2Str(QBeam,6);
      RubOut(17,4,6,GraphBackColor);
      Print(17,4,PrintString);
      IF Message.Next <> 3 THEN PrintString := Num2Str(qscatt,6) ELSE
         PrintString := '?';
      RubOut(42,4,6,GraphBackColor);
      Print(42,4,PrintString);
      IF Message.Next <> 3 THEN PrintString := Num2Str(radius,6) ELSE
         PrintString := '?';
      RubOut(30,5,6,GraphBackColor);
      Print(30,5,PrintString);
   END;

PROCEDURE Change_Scatterers;

   VAR
      PrintString: String[10];

   BEGIN
      WITH Change_Scatterer_Screen DO
      BEGIN
         AcceptScreen;
         MBeam := GetNumber(3);
         mscatt := GetNumber(4);
         QBeam := GetNumber(1);
         qscatt := GetNumber(2);
         radius := GetNumber(5);
         PrintParameters;
      END;
   END;

PROCEDURE TogglePersist;

   BEGIN
      WITH Scint1 DO
      BEGIN
      If Persist = 0 THEN
         BEGIN
            hotKeys.key[5] := 'F7-Persist On';
            HotKeys.Display;
            Persist := 5;
            Scint1.Display;
         END
      ELSE
         BEGIN
            Persist := 0;
            hotKeys.key[5] := 'F7-Persist Off';
            HotKeys.Display;
         END;
      END;
   END;


PROCEDURE Change_Collection;

   BEGIN
      WITH Change_Collection_Screen DO
      BEGIN
         AcceptScreen;
         {Scatter_Density := GetNumber(1);
         Thickness := GetNumber(2);}

      END;

   END;

FUNCTION ChangedSlider: Integer;

   BEGIN
      IF OldSlider[1] <> Sliders.Value(1) THEN ChangedSlider := 1
      ELSE IF OldSlider[2] <> Sliders.Value(2) THEN ChangedSlider := 2
      ELSE IF OldSlider[3] <> Sliders.Value(3) THEN ChangedSlider := 3
      ELSE IF OldSlider[4] <> Sliders.Value(4) THEN ChangedSlider := 4
      ELSE IF OldSlider[5] <> Sliders.Value(5) THEN ChangedSlider := 5;
   END;

PROCEDURE ChangeBeamDivergence;

   BEGIN
      WITH ChangeBeamDivergenceScreen DO
         BEGIN
            AcceptScreen;
            IF NOT Canceled THEN
            BEGIN
            BeamDivergence := GetNumber(1);
            QBeam := GetNumber(2);
            MBeam := GetNumber(3);
            PrintParameters;
            END;
         END;
   END;


PROCEDURE ProcessSlider;

   BEGIN
      CASE ChangedSlider OF
         1: BEGIN
               OldSlider[1] := Sliders.value(1);
               CASE message.Next OF
                  2,3,4: BEGIN
                             DetailCenter := Trunc(Sliders.Value(1));
                             Hist1.ChangeDetailScale;
                         END;
                  1: BEGIN
                        Scint1.setposition(Sliders.value(1));
                        Scint1.Display;
                     END;
               END;
            END;
         2: BEGIN
               OldSlider[2] := Sliders.value(2);
               CASE message.next OF
                  2,3,4: BEGIN
                            DetailWidth := Trunc(Sliders.Value(2));
                            Hist1.ChangeDetailScale;
                         END;
                  1: BEGIN
                        Scint1.SetWidth(Sliders.Value(2));
                        Scint1.Display;
                     END;
                  END;
            END;
         3: BEGIN
               OldSlider[3] := Sliders.value(3);
               CASE message.next OF
                  2,3,4: BEGIN
                        NSampleAngle := OldSlider[3]*Pi/180;
                        IF ((ScatterType = 2) OR (ScatterType = 11))  THEN
                             FindPolyScattering(PolyVertex,NSampleAngle);
                        Hist1.ChangeSampleAngle(NsampleAngle);
                     END;
                  1: BEGIN
                        DelayCount := Trunc(1000/OldSlider[3])-10;
                     END;
               END;
            END;
        { 1: BEGIN
               OldSlider[1] := Sliders.value(1);
               DetailCenter := Trunc(Sliders.Value(1));
               Hist1.ChangeDetailScale;
            END;
         2: BEGIN
               OldSlider[2] := Sliders.value(2);
               DetailWidth := Trunc(Sliders.Value(2));
               Hist1.ChangeDetailScale;
            END;
         3: BEGIN
               OldSlider[3] := Sliders.value(3);
               NSampleAngle := OldSlider[3]*Pi/180;
               FindPolyScattering(PolyVertex,NSampleAngle);
               Hist1.ChangeSampleAngle(NsampleAngle);
            END;  }
         4: BEGIN
               OldSlider[4] := Sliders.value(4);
               energy := Sliders.value(4);
            END;
         5: BEGIN
               OldSlider[5] := Sliders.value(5);
               Beam_Width := Sliders.Value(5);
            END;
      END;
   END;

{=========================================================================

     THE PHYSICS: Calculating the scattering angle

==========================================================================}


FUNCTION UserDefined1(r: Real): Real; FAR;

   BEGIN
      UserDefined1 := 0;
   END;

FUNCTION UserDefined2(x,y,vx,vy: Real; VAR Fx, Fy : Real): Real; FAR;

   BEGIN
      Fx := 0;
      Fy := 0;
   END;


FUNCTION CircleScatter(b,radius: Real): Real; FAR;
   VAR
      u: Real;

   BEGIN
      IF b >= radius THEN CircleScatter := 0
      ELSE BEGIN
              u:= Pi - 2*arcsin(b/radius);
              CircleScatter := u;
           END;
   END;

FUNCTION UniformScatter(b,q: Real): Real; FAR;

   BEGIN
      UniformScatter := 2*Pi*Random;
   END;

FUNCTION RSquaredScatter(b,q: Real): Real; FAR;

   VAR
      a: Real;

   BEGIN
      IF b <> 0 THEN a := 2*Arctan((QBeam*qscatt)/(2*energy*b))
      ELSE a := Pi;
      IF a > 0 THEN RSquaredScatter := a ELSE RSquaredScatter := a + 2*Pi;
   END;

FUNCTION RSquaredForce(r: Real): Real; FAR;
   BEGIN
      RSquaredForce := QBeam*qscatt/Sqr(r);
   END;

FUNCTION RCubedForce(r: Real): Real; FAR;
   BEGIN
      RCubedForce := QBeam*qscatt/Pwr(r,3);
   END;

FUNCTION RForce(r: Real): Real; FAR;
   BEGIN
      RForce := QBeam*qscatt/r;
   END;

FUNCTION YukawaForce(r: Real): Real; FAR;
   BEGIN
      YukawaForce := 1000*QBeam*qscatt*Exp(-radius*r);
   END;

FUNCTION NGonScatter(b,d: Real): Real; FAR;

   VAR
      i: Integer;
      Found: Boolean;

   BEGIN
      Found := False;
      I := 1;
      NGonScatter := 0;
      WHILE ((NOT Found) AND (i <= FacesShowing)) DO
         BEGIN
            IF ((b <= d * SinVertex[2*i-1]) AND (b > d * SinVertex[2*i]))
               THEN BEGIN
                       NGonScatter := ScatterAngle[i];
                       Found := True;
                    END
            ELSE i := i + 1;
         END;
   END;

FUNCTION UserScatter(b,r: Real): Real;
   BEGIN
   END;

FUNCTION EllipseScatter(y,r: Real): Real; FAR;

   VAR
      CosA, SinA, Cos2A, Sin2A: Real;
      x, Rad, t1, t2, temp: Real;

   BEGIN
      WITH EllipseRecord DO
      BEGIN
         IF NSampleAngle <> CurrentAngle THEN
            BEGIN
               CosA := Cos(NSampleAngle);
               SinA := Sin(NSampleAngle);
               Cos2A := Sqr(CosA);
               Sin2A := Sqr(SinA);
               CurrentAngle := NSampleAngle;
               Alpha := Cos2A*A+Sin2A*B;
               Beta := Cos2A*B+Sin2A*A;
               Gamma := (A-B)*CosA*SinA;
               Delta := Gamma/(2*Alpha);
            END;
         Rad := 1/Alpha - (Beta/Alpha - Sqr(Delta))*Sqr(y);
         IF Rad >= 0 THEN
            BEGIN
               x := -Delta*y-Sqrt(Rad/Alpha);
               t1 := 2*Alpha*x + Gamma*y;
               t2 := -2*Beta*y - Gamma*x;
               IF ABS(t1) <= ABS(t2) THEN
                  BEGIN
                  temp := 2*arctan(t1/t2);
                  IF temp < 0 THEN temp := Temp + 2*Pi;
                  EllipseScatter := temp
                  END
               ELSE
                  BEGIN
                  temp := Pi-2*arctan(t2/t1);
                  EllipseScatter := Pi-2*arctan(t2/t1);
                  END;
            END
         ELSE
            BEGIN
            temp := 0;
            EllipseScatter := 0;
            END;
      END;
   END;

FUNCTION LineScatter(b,l: Real): Real; FAR;

   VAR
      edge: Real;

   BEGIN
      edge := ABS(l*Cos(NSampleAngle));
      IF ((b < edge) AND (b > edge)) THEN LineScatter := pi + 2*NSampleAngle
      ELSE LineScatter := 0;
   END;

FUNCTION SectorScatter(b,r: Real): Real; FAR;

   VAR
      a1,a2,theta,Temp: Real;

   BEGIN
      WITH SectorRecord DO
      BEGIN
         IF NSampleAngle <> CurrentAngle THEN
            BEGIN
               CurrentAngle := NSampleAngle;
               a1 := Pi - alpha/2 + NSampleAngle;
               IF a1 < 0 THEN a1 := a1 + 2 * Pi;
               a2 := Pi + alpha/2 + NSampleAngle;
               IF a2 > 2 * Pi THEN a2 := a2 - 2 * Pi;
               IF a1 > Pi THEN
                 BEGIN
                    Range1Max := a1;
                 END
               ELSE
                 BEGIN
                    range1Max := Pi;
                 END;
               IF a2 < Pi THEN
                 BEGIN
                    Range2Min := a2;
                 END
               ELSE
                 BEGIN
                    range2Min := Pi;
                 END;
               IF a1 > Pi/2 THEN CircleRangeMin := a1
               ELSE CircleRangeMin := Pi/2;
               IF a2 < 3*Pi/2 THEN CircleRangeMax := a2
               ELSE CircleRangeMax := 3*Pi/2;
            END;
         Theta := Pi-arcsin(b/r);
         IF ((CircleRangeMin<theta) AND (CircleRangeMax>theta)) THEN
            Temp := CircleScatter(b,r)
         ELSE IF ((Pi > Theta) AND (theta > range2Min)) THEN
            Temp := alpha + 2*NSampleAngle
         ELSE IF ((Pi < theta) AND (theta < range1Max)) THEN
            Temp := -alpha + 2*NSampleAngle
         ELSE Temp := 0;
         IF Temp >= 2*Pi THEN SectorScatter := Temp-2*Pi
         ELSE IF Temp < 0 THEN SectorScatter := Temp + 2 * Pi
         ELSE SectorScatter := Temp;
      END;
   END;


PROCEDURE Acceleration(t: Real; VAR Y,YP: DVector); FAR;

   VAR
      F,r: Real;
      Fx,Fy: Real;

   BEGIN
      YP.put(1,Y.value(3));
      YP.put(2,Y.value(4));
      r := Sqrt(Sqr(Y.value(1)) + Sqr(Y.value(2)));
      IF (NOT UserDef2) THEN
         BEGIN
            F := Force(r);
            YP.put(3,(F*Y.value(1)/r));
            YP.put(4,(F*Y.value(2)/r));
         END
      ELSE
         BEGIN
            F := UserDefined2(Y.value(1),Y.value(2),Y.value(3),Y.value(4),Fx,Fy);
            YP.put(3,Fx);
            YP.put(4,Fy);
         END;
   END;

FUNCTION Integrate(b,d: Real): Real; FAR;
   VAR
      IFlag: Integer;
      h,hmax,t,dt: Real;
      V: DVector;
      R,R0,vOX,Temp: Real;

   BEGIN
      V.init(4);
      Vox := Sqrt(2*energy/mBeam);
      V.put(1, -10);
      V.put(2,b);
      R0 := Sqrt(100 + Sqr(b));
      R := R0;
      V.put(3,Vox);
      V.put(4,0);
      t := 0;
      dt := 0.1;
      h := 0.01;
      hmax := 0.1;
      WHILE R <= R0 DO
         BEGIN
            StepRKF(Acceleration,t,V,dt,h,hmax,0.1,0.1,Iflag);
            R := Sqrt(Sqr(V.value(1)) + Sqr(V.value(2)));
         END;
      Temp := Arccos(V.value(3)/Sqrt(Sqr(V.value(3))+Sqr(V.Value(4))));
      IF V.Value(4) > 0 THEN Integrate := Temp
        ELSE IF V.Value(4) < 0 THEN Integrate := -Temp + 2*Pi
           ELSE IF V.Value(3) > 0 THEN Integrate := Temp
              ELSE Integrate := Temp + Pi;
   END;

PROCEDURE CreateINGon(N: Integer; VAR PolyVertex: Polygon;
                                                         SampleAngle: Real);

   VAR
      i: Integer;
      TotalAngle: Real;

   BEGIN
      TotalAngle := 0;
      PolyVertex[0] := N;
      PolyVertex[1] := 0;
      FOR i := 2 TO N DO
         BEGIN
            PolyVertex[i] := (2*Pi-TotalAngle)*(Random*0.25+0.25)+
                                                            PolyVertex[i-1];
            TotalAngle := PolyVertex[i];
         END;
   END;

PROCEDURE CreateEllipse;

    BEGIN
       WITH EllipseRecord DO
          BEGIN
             A := 0.9*Random +0.1;
             B := 0.9*Random +0.1;
             A := 1/sqr(A);
             B := 1/Sqr(B);
             CurrentAngle := 0;
             Alpha := A;
             Beta := B;
             Gamma := 0;
             Delta := 0;
          END;
    END;

PROCEDURE CreateSector;
   BEGIN
      WITH SectorRecord DO
         BEGIN
            alpha := Pi*(1-0.8*Random);
            CurrentAngle := 0;
            radius := 1;
            Range1Max := Pi;
            Range2Min := Pi;
            CircleRangeMin := 5*Pi/6;
            CircleRangeMax := 7*Pi/6;
         END;
   END;


PROCEDURE CreateNGon(N: Integer; VAR PolyVertex: Polygon;
                                                         SampleAngle: Real);

   VAR
      i: Integer;

   BEGIN
      PolyVertex[0] := N;
      PolyVertex[1] := 0;
      FOR i := 2 TO N DO
         BEGIN
            PolyVertex[i] := (i-1)*2*Pi/N;
         END;
   END;

PROCEDURE FindPolyScattering(PolyVertex: Polygon;
                                             SampleAngle: Real);
   VAR
      i: Integer;
      Sangle: ARRAY [1..10] OF Real;

   BEGIN
      FacesShowing := 0;
      FOR i := 1 TO Trunc(PolyVertex[0]) DO
         BEGIN
            IF (i>1) THEN
                Sangle[i] := (PolyVertex[i]+PolyVertex[i-1])/2 + SampleAngle
            ELSE Sangle[i] := (2*Pi + PolyVertex[Trunc(PolyVertex[0])])/2 + SampleAngle;
            IF Sangle[i] > 2*Pi THEN Sangle[i] := Sangle[i] - 2*Pi;
            IF Sangle[i] < 0 THEN Sangle[i] := Sangle[i] - 2*Pi;
            IF Cos(Sangle[i]) < 0 THEN
               BEGIN
                  FacesShowing := FacesShowing + 1;
                  ScatterAngle[FacesShowing] := 2*Sangle[i] - Pi;
                  SinVertex[2*FacesShowing-1] := sin(Sangle[i]-Pi/N);
                  SinVertex[2*FacesShowing] := sin(Sangle[i]+Pi/N);
               END;
         END;
   END;

PROCEDURE DivergeBeam(VAR theta1: Real);

   BEGIN
      theta1 := theta1+(random-0.5)*BeamDivergence*pi/180;
      IF theta1 >= 2*pi THEN theta1 := theta1-2*pi;
      IF theta1 < 0 THEN theta1 := theta1+2*pi;
   END;


PROCEDURE Generate_Event(R: Real);

   VAR
      b, theta1: Real;

   BEGIN
      b := 2*(R-0.5) * Beam_Width;
      theta1 := CalculateDeflection(b,radius);
      IF BeamDivergence <> 0 THEN DivergeBeam(theta1);
      Hist1.Add_Hit(theta1);
   END;

PROCEDURE SelectPotential(PotentialType: Integer; Unknown: Boolean);

   VAR
      RandomN: Integer;

   BEGIN
      CurrentPotential := PotentialType;
      UserDef2 := False;
      ScatterType := PotentialType;
      CASE PotentialType of
         1: BEGIN
               CalculateDeflection := CircleScatter;
               IF NOT Unknown THEN
                  BEGIN
                     OldScattererString := ScattererString;
                     ScattererString := 'Hard Sphere';
                     PrintScattererString;
                  END;
            END;
         2: BEGIN
               CalculateDeflection := NGonScatter;
               IF NOT Unknown THEN
                  WITH NGonScreen DO
                     REPEAT
                        BEGIN
                           AcceptScreen;
                           N := Trunc(GetNumber(1));
                        END;
                  UNTIL ((2 < N) AND (N < 10))
               ELSE N := TRUNC(Random*8 + 3);
               CreateNGon(N,PolyVertex,NSampleAngle);
               FindPolyScattering(PolyVertex,NSampleAngle);
               IF NOT Unknown THEN
               BEGIN
                  OldScattererString := ScattererString;
                  CASE N of
                     3: ScattererString := 'Equilateral Triangle';
                     4: ScattererString := 'Square';
                     5: ScattererString := 'Regular Pentagon';
                     6: ScattererString := 'Regular Hexagon';
                     7: ScattererString := 'Regular Heptagon';
                     8: ScattererString := 'Regular Octagon';
                     9: ScattererString := 'Regular Nonagon';
                  END;
                  PrintScattererString;
               END;
            END;
         3: BEGIN
               CalculateDeflection := Integrate;
               Force := RForce;
               IF NOT Unknown THEN
                  BEGIN
                     OldScattererString := ScattererString;
                     ScattererString := 'Qq/r';
                     PrintScattererString;
                  END;
            END;
         4: BEGIN
               CalculateDeflection := RSquaredScatter;
               IF NOT Unknown THEN
                  BEGIN
                     OldScattererString := ScattererString;
                     ScattererString := 'Qq/r^2 Exact';
                     PrintScattererString;
                  END;
             END;
         5: BEGIN
               CalculateDeflection := Integrate;
               Force := RSquaredForce;
               IF NOT Unknown THEN
                  BEGIN
                     OldScattererString := ScattererString;
                     ScattererString := 'Qq/r^2 Integrated';
                     PrintScattererString;
                  END;
            END;
         6: BEGIN
               CalculateDeflection := Integrate;
               Force := RCubedForce;
               IF NOT Unknown THEN
                  BEGIN
                     OldScattererString := ScattererString;
                     ScattererString := 'Qq/r^3';
                     PrintScattererString;
                  END;
            END;
         7: BEGIN
               CalculateDeflection := Integrate;
               Force := YukawaForce;
               IF NOT Unknown THEN
                  BEGIN
                     OldScattererString := ScattererString;
                     ScattererString := 'Qqexp(-r/L)';
                     PrintScattererString;
                  END;
            END;
         8: BEGIN
               CalculateDeflection:= UniformScatter;
               IF NOT Unknown THEN
                  BEGIN
                     OldScattererString := ScattererString;
                     ScattererString := 'Uniform Scattering';
                     PrintScattererString;
                  END;
            END;
         9: BEGIN
               CalculateDeflection := Integrate;
               Force := UserDefined1;
               IF NOT Unknown THEN
                  BEGIN
                     OldScattererString := ScattererString;
                     ScattererString := 'User Defined';
                     PrintScattererString;
                  END;
            END;
         10: BEGIN
               CalculateDeflection := Integrate;
               UserDef2 := True;
               IF NOT Unknown THEN
                  BEGIN
                     OldScattererString := ScattererString;
                     ScattererString := 'User Defined';
                     PrintScattererString;
                  END;
            END;
         11: BEGIN
               CalculateDeflection := NGonScatter;
               Radius := 0.8*random +0.2;
               CreateINGon(Trunc(4*Random)+3,PolyVertex,NSampleAngle);
               FindPolyScattering(PolyVertex,NSampleAngle);
             END;
         12: BEGIN
               CalculateDeflection := EllipseScatter;
               CreateEllipse;
               IF NOT Unknown THEN
                  BEGIN
                     OldScattererString := ScattererString;
                     ScattererString := 'Ellipse';
                     PrintScattererString;
                  END;
            END;
          13: BEGIN
               CalculateDeflection := SectorScatter;
               CreateSector;
               IF NOT Unknown THEN
                  BEGIN
                     OldScattererString := ScattererString;
                     ScattererString := 'Sector';
                     PrintScattererString;
                  END;
               END;
          14: BEGIN
               CalculateDeflection := LineScatter;
               Radius := 0.9*Random+0.1;
               IF NOT Unknown THEN
                  BEGIN
                     OldScattererString := ScattererString;
                     ScattererString := 'Line';
                     PrintScattererString;
                  END;
               END;
         101: BEGIN
               RandomN := 9;
               WHILE ((RandomN >= 8) AND (RandomN <= 10)) DO
                  RandomN := Trunc(14*Random)+1;
               Qscatt := 0.9*random+0.1;
               SelectPotential(RandomN,True);
               OldScattererString := ScattererString;
               ScattererString := 'Unknown';
               PrintScattererString;
             END;
      END;
               IF Message.Next <> 1 THEN Hist1.Clear
               ELSE scint1.Display;
   END;

PROCEDURE SelectNewUnknown;

   BEGIN
      WITH NewUnknownScreen DO
         BEGIN
            AcceptScreen;
            IF NOT Canceled THEN SelectPotential(101,False);
         END;
   END;


{========================================================================

     Process event and send appropriate message back

=========================================================================}
PROCEDURE ProcessEvent(VAR Message: MessageRec);

   VAR
      MenuPicked, Redo: Boolean;
      StoreFile: String;

PROCEDURE ProcessMenu;

   BEGIN
      WITH MEssage DO
      CASE Main_Menu.ColChosen OF
         1: CASE Main_Menu.RowChosen of
               1: AboutCups;
               2: AboutRutherford;
               3: Configuration;
               5:;
               6: StoreFile := OpenFile('C:\*.bak');
               8: BEGIN
                     Paused := False;
                     Quit := TRUE;
                     Done := True;
                  END;
            END;
         2: BEGIN
               CASE Main_Menu.RowChosen of
                  1: Next := 1;
                  2: Next := 2;
                  3: Next := 3;
                  4: Next := 4;
               END;
               Quit := True;
               Paused := False;
            END;
         3: BEGIN
               CASE Main_menu.RowChosen OF
                  1: ChangeBeamDivergence;
               END;
               Quit := True;
            END;
         4: BEGIN
               SelectPotential(Main_Menu.RowChosen, False);
            END;
         5: BEGIN
               Redo := True;
               IF Next<>1 THEN
                  BEGIN
                     WHILE Redo DO CurveFit(Redo);
                     Hist1.DrawDetail;
                     Hist1.ReplotDetail;
                  END;
            END;
      END;
   END;


   BEGIN
      WITH Message DO
      BEGIN
         IF Main_Menu.Activated THEN ProcessMenu;
            {   CASE Main_Menu.ColChosen OF
                  1: CASE Main_Menu.RowChosen of
                        1: AboutCups;
                        2: AboutRutherford;
                        3: Configuration;
                        6: BEGIN
                              Paused := False;
                              Quit := TRUE;
                              Done := True;
                           END;
                     END;
                  2: BEGIN
                        CASE Main_Menu.RowChosen of
                           1: Next := 1;
                           2: Next := 2;
                           3: Next := 3;
                           4: Next := 4;
                        END;
                        Quit := True;
                        Paused := False;
                     END;
                  3: BEGIN
                        CASE Main_menu.RowChosen OF
                           1: ChangeBeamDivergence;

                        Quit := True;
                     END;
                  4: BEGIN
                        SelectPotential(Main_Menu.RowChosen, False);
                     END;
               END; }
         IF hotKeys.Pressed(kNum) THEN
            CASE kNum of
               1: BEGIN
                     Help(HelpScreen[next]);
                  END;
               2: BEGIN
                     IF Paused THEN Paused := False ELSE Paused := True;
                     Quit := False;
                  END;
               3: BEGIN
                     VerifyClear;
                  END;
               4: IF Main_menu.Chosen THEN ProcessMenu;
               5: IF Next = 3 THEN Draw_scatterer ELSE TogglePersist;
               6: IF Next = 3 THEN SelectNewUnknown;
            END;
         IF Sliders.Changed THEN
            BEGIN
               ProcessSlider;
            END;
      END;
   END;

{=========================================================================

     Check for events and loop during pauses.

==========================================================================}
PROCEDURE QueryEvent(VAR Message: MessageRec);
   BEGIN
      WITH Message DO
      BEGIN
         CheckForEvents;
         IF Event.KeyPressed OR Event.MouseClicked THEN
              ProcessEvent(Message);
         WHILE Paused DO
            BEGIN
               CheckForEvents;
               IF Event.KeyPressed OR Event.MouseClicked THEN
                    ProcessEvent(Message);
            END;
      END;
   END;

PROCEDURE Test_Histogram;

   VAR
      Hist1: THistogram;
      i: Integer;

   BEGIN
      WITH Hist1 DO
      WITH Message DO
      BEGIN
         Quit := FALSE;
         Paused := True;
         SetHistViewPort(2,0.0,0.6,0.1,0.7);
         SetHistScale(2);
         SetDetailViewPort(3,0.8,1.0,0.3,0.6);
         SetDetailScale(3);
         SetMaxHits(5);
         SetHColor(1);
         SetHBackColor(4);
         SetBinNumber(100);
         SetResolution(1.0);
         OldSlider[1] := 0;
         OldSlider[2] := 10;
         OldSlider[3] := 0.0;
         OldSlider[4] := 1.0;
         OldSlider[5] := 1.0;
         Clear;
         Rescale;
         Randomize;
         i := 0;
         WHILE (Paused AND (NOT Quit)) DO QueryEvent(Message);
         WHILE ((i <= 10000) AND (NOT Quit)) DO
            BEGIN
               QueryEvent(Message);
               IF (NOT Quit) THEN
                  BEGIN
                     Generate_Event(Random);
                     i := i + 1;
                  END;
            END;
      END;
   END;

PROCEDURE Test_Scintillator;
   VAR
      i: Integer;
      b,theta: Real;

   BEGIN
      WITH Scint1 DO
      WITH Message DO
         BEGIN
            Message.next := 1;
            HotKeys.init(5);
            hotKeys.key[5] := 'F7-Persist On';
            HotKeys.Display;
            SetWindow(11,0.3,0.7,0.5,0.9);
            SetScale(11,-1,1,-0.1,1.1);
            SetWidth(10.0);
            SetPosition(0.0);
            SetHColor(15);
            SetHBackColor(4);
            SetPersist(32000);
            Display;
            Sliders.Init;
            Sliders.Create(1,-180,180,0,0.3,0.4,0.7,0,'-180','180','Center Direction',False);
            Sliders.Create(2,2,30,10,0.3,0.3,0.7,1,'2','30','Screen Width',False);
            Sliders.Create(3,1,100,100,0.3,0.2,0.7,0,'1','100','Source Activity',False);
            Sliders.DrawAll;
            OldSlider[1] := 0;
            OldSlider[2] := 10;
            OldSlider[3] := 100;
            Quit := False;
            QBeam := 2;
            QScatt := 79;
            energy := 20000/1.44;
            DelayCount := 0;
            WHILE (Paused AND (NOT Quit)) DO QueryEvent(Message);
            WHILE (NOT Quit) DO
               BEGIN
                  QueryEvent(Message);
                  IF (NOT Quit) THEN
                     BEGIN
                        b := 2*(Random-0.5) * Beam_Width;
                        theta := RSquaredScatter(b,1);
                        IF theta > 2*Pi+lowedge*pi/180 THEN ShowEvent(theta-2*Pi)
                        ELSE
                        IF theta < highedge*pi/180-2*Pi THEN ShowEvent(theta+2*Pi)
                        ELSE ShowEvent(theta);
                        i := i + 1;
                        Delay(DelayCount);
                     END;
               END;
            Paused := True;
            WHILE (Paused AND (NOT Quit)) DO QueryEvent(Message);
         END;
         Sliders.Done;
         HotKeys.init(4);
         HotKeys.Display;
   END;


PROCEDURE GuessTheScatterer;

   VAR
      i: LongInt;

   BEGIN
      EnableMenuColumn4(False);
      WITH Message DO
      BEGIN
      i := 0;
      Quit := FALSE;
      HotKeys.init(6);
      hotKeys.key[5] := 'F7-See Unknwn';
      HotKeys.key[6] := 'F8-New Unknwn';
      HotKeys.Display;
      SelectPotential(101, False);
      IF hist1.Displayed = False THEN
         BEGIN
            SelectViewPort(17);
            ClearViewPort;
            Hist1.Clear;
            OpenViewPort(6);
            SetColor(15);
            Print(2,1,'Current Scatterer = ');
            Print(2,2,'Beam Particles:          Scatterer:');
            Print(4,3,'Mass (M) =               Mass (m) = ');
            Print(4,4,'Charge (Q) =             Charge (q) = ');
            Print(2,5,'Characteristic Length (L) = ');
            CreateSliders;
            hist1.Displayed := True;
         END;
      PrintScattererString;
      PrintParameters;
         WHILE (Paused AND (NOT Quit)) DO QueryEvent(Message);
         WHILE ((i <= 2147483647) AND (NOT Quit)) DO
            BEGIN
               QueryEvent(Message);
               IF (NOT Quit) THEN
                  BEGIN
                     Generate_Event(Random);
                     i := i + 1;
                  END;
            END;
         Paused := True;
         WHILE (Paused AND (NOT Quit)) DO QueryEvent(Message);
      END;
      HotKeys.init(4);
      HotKeys.Display;
      EnableMenuColumn4(True);
   END;

PROCEDURE Rutherford;

   PROCEDURE PrintAlpha(x,y,width,height: Real);
      BEGIN
         SetColor(15);
         PlotLine(x+width/2,y+height/2,x,y-height/2);
         PlotLine(x,y-height/2,x-width/4,y-height/2);
         PlotLine(x-width/4,y-height/2,x-width/2,y-height/4);
         PlotLine(x-width/2,y-height/4,x-width/2,y);
         PlotLine(x-width/2,y,x-width/4,y+height/2);
         PlotLine(x-width/4,y+height/2,x,y+height/2);
         PlotLine(x,y+height/2,x+width/4,y+height/4);
         PlotLine(x+width/4,y+height/4,x+width/3,y-3*height/8);
         PlotLine(x+width/3,y-3*height/8,x+width/2,y-height/2);
      END;

   PROCEDURE DrawApparatus;
      BEGIN
         DefineScale(13,0,10,-1,6);
         SelectScale(13);
         PlotLine(0.8,4.8,3.35,3.05);
         PlotLine(0.8,4.8,3.15,2.8);
         PlotLine(0.8,4.8,3.21,2.875);
         PlotLine(0.8,4.8,3.29,2.975);
         PlotLine(3.35,3.05,3.15,2.8);
         PlotLine(4.1,1.9,4.1,3.1);
         PlotLine(3.0,1.82,4.0,1.82);
         PlotLine(0.7,2.65,3.4,2.65);
         PlotLine(3.4,2.65,3.4,2.35);
         PlotLine(3.4,2.35,0.7,2.35);
         PlotLine(0.7,2.35,0.7,2.65);
         PlotLine(3.0,0.4,3.0,1.2);
         PlotLine(3.0,1.2,3.3,1.2);
         PlotLine(3.3,1.2,3.3,1.5);
         PlotLine(4.05,0.4,4.05,1.2);
         PlotLine(4.05,1.2,3.75,1.2);
         PlotLine(3.75,1.2,3.75,1.5);
         Ellipse(MapX(3.525),MapY(1.5),0,360,Mapx(3.75)-MapX(3.525),
                           MapY(1.2)-MapY(1.275));
         SetFillStyle(1,Black);
         FloodFill(MapX(3.525),MapY(1.5),White);
         PlotLine(3.0,0.4,4.05,0.4);
         FloodFill(MapX(3.525),MapY(0.8),White);
         SetColor(Black);
         PlotLine(3.0,0.4,4.05,0.4);
         SetColor(White);
         PlotSymbol(0.8,4.92,'A');
         PlotSymbol(3.38,3.20,'B');
         PlotSymbol(0.57,2.5,'P');
         PlotSymbol(4.23,3.1,'R');
         PlotSymbol(4.23,1.9,'R');
         PlotSymbol(2.9,1.83,'S');
         PlotSymbol(3.525,0.85,'M');
         SetFillStyle(1,White);
         FloodFill(MapX(3.15), MapY(2.90), White);
         FloodFill(MapX(3.29), MapY(3.00), White);
         SetFillStyle(3,White);
         FloodFill(MapX(1.4),MapY(2.50), White);
         PlotLine(0.8,4.8,3.18,2.8375);
         PlotLine(0.8,4.8,3.32,3.0125);
      END;

   BEGIN
      EnableMenuColumn4(False);
      Main_Menu.RowActivate(3,1,False);
      Main_Menu.RowActivate(5,1,False);
      Sliders.Done;
      HistogramDrawn := False;
      HideCursor;
      OpenViewPort(17);
      SetColor(15);
      Print(25,2, 'Geiger and Marsden''s Experiment');
      DrawApparatus;
      Print(48, 6, 'AB - Conical Source Tube');
      Print(48,7,'P - Lead Plate');
      Print(48,8,'RR - Reflector');
      Print(48,9,'S - Zinc Sulfide Screen');
      Print(48,10,'M - Microscope');
      Print(2,18, 'This drawing is based on a diagram included in the article "On a Diffuse');
      Print(2,19, 'Reflection of the  -Particles"  by H. Geiger and E. Marsden, Proc. Roy.');
      Print(2,20, 'Soc. lxxxii. p. 495 (1909)');
      Print(20,21, ' PRESS ANY KEY OR CLICK MOUSE TO CONTINUE');
      PrintAlpha(2.47,-0.020,0.1,0.1);
      ShowCursor;
      REPEAT CheckForEvents UNTIL (Event.KeyPressed OR Event.MouseClicked);
      HideCursor;
      ClearViewPort;
      ShowCursor;
      Test_Scintillator;
      Hist1.Displayed := False;
      Done := 1;
      EnableMenuColumn4(True);
      Main_Menu.RowActivate(3,1,True);
      Main_Menu.RowActivate(5,1,True);
   END;

PROCEDURE Test_PolarHistogram;

   VAR
      i: LongInt;

   BEGIN
      WITH Hist1 DO
      WITH Message DO
      BEGIN
         Force := RSquaredForce;
        { CalculateDeflection := RSquaredScatter;}
         QBeam := 0.01;
         qscatt := 1;
         energy := 1;
         mBeam:= 1;
         mscatt := 1;
         BeamDivergence := 0;
         N := 7;
         TotalHits := 0;
         TotalHitsString := '0';
         Beam_Width := 1;
         radius := 1;
         Alpha := 1;
         Quit := FALSE;
         Paused := True;
         NsampleAngle := 0;
         sampleAngle := 0;
         IF HistogramDrawn = False THEN
            BEGIN
               SelectViewPort(17);
               ClearViewPort;
            END;
         DefineViewPort(6,0.0,0.6,0.72,0.93);
         OpenViewPort(6);
         PrintParameters;
         SetColor(15);
         Print(2,1,'Current Scatterer = ');
         Print(2,2,'Beam Particles:          Scatterer:');
         Print(4,3,'Mass (M) =               Mass (m) = ');
         Print(4,4,'Charge (Q) =             Charge (q) = ');
         Print(2,5,'Characteristic Length (L) = ');
         {ScattererString := '1/r^2';}
         SetHistViewPort(2,0.0,0.6,0.1,0.7);
         SetHistScale(2);
         SetDetailViewPort(3,0.7,0.96,0.28,0.6);
         SetDetailScale(3);
{         BigDetailViewPort := 4;          }
         DefineViewPort(4,0.61,1.0,0.23,0.65);
         SetMaxHits(5);
         SetHColor(Yellow);
         SetHBackColor(4);
         SetBinNumber(360);
         SetResolution(1.0);
         Set_Radii(0.25,0.45);
         Set_Center(0.0,0.0);
         DetailMax := 10;
         DetailMin := -10;
         DetailCenter := 0;
         DetailWidth := 20;
         SelectPotential(CurrentPotential,False);
         MinBin := 351;
         MaxBin := 10;
         DetailMaxHits := 10;
         DefineScale(DetailScale, DetailMin-0.05*DetailWidth,DetailMax+0.05*DetailWidth,0,DetailMaxHits);
         OldSlider[1] := 0;
         OldSlider[2] := 10;
         OldSlider[3] := 0.0;
         OldSlider[4] := 1.0;
         OldSlider[5] := 1.0;
         Clear;
         Hist1.Displayed := True;
         Randomize;
         CreateSliders;
         i := 0;
         WHILE (Paused AND (NOT Quit)) DO QueryEvent(Message);
         WHILE ((i <= 2147483647) AND (NOT Quit)) DO
            BEGIN
               QueryEvent(Message);
               IF (NOT Quit) THEN
                  BEGIN
                     Generate_Event(Random);
                     i := i + 1;
                  END
               ELSE
                   CASE Change OF
                      2: Change_Scatterers;
                      3: Change_Collection;
                   END;
            END;
         Paused := True;
         WHILE (Paused AND (NOT Quit)) DO QueryEvent(Message);
      END;
   END;


{=========================================================================

   Main procedure

==========================================================================}
BEGIN
   Initialize;
   InitHelpScreens;
   x.init(Length);
   f.init(Length);
   CurrentHelp := HelpScr1;
   Define_Input_Screens;
   CurrentPotential := 1;
   WITH Message DO
   BEGIN
      Paused := True;
      Done := False;
      next := 2;
      StartUpScreen;
      Test_PolarHistogram;
      WHILE Done = False DO
         CASE next OF
            1: Rutherford;
            {2: SingleScatterTest_Scintillator;}
            2: Test_PolarHistogram;
            3: GuessTheScatterer;
         END;
   END;
   x.Free;
   F.Free;
   CUPSdone;

END.   { End of file EX_MAIN.PAS }