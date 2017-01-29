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

{*  Unit for DIFFRACT.PAS  ----  December 6th 1994     *}

UNIT DiffUnit;
INTERFACE
USES graph, crt, CUPS, CUPSmupp, CUPSgui, CUPSgrph,
     CUPSproc,CUPSFunc;

FUNCTION PointInside(X,Y,VPortNumber : Integer) : Boolean;
FUNCTION NewSimpson (y : dVector; Number : Integer) : real;
PROCEDURE MakeBlueBox;
PROCEDURE Colours(VAR M : Integer; VAR Lambda : DVector);
PROCEDURE Print2( X,Y,places: integer; Num : real; color,BackColor : integer);

TYPE
   TSlitsDiffraction = object
      Active,F10Used : Boolean;
      Sliders,Slider : TSliders;
      SlitButtons    : TButtons;
      HotKeys        : THotKeys;
      PROCEDURE Init;
      PROCEDURE SetDefaults;
      PROCEDURE SetUp;
      PROCEDURE HandleHotKeys(KeyNum : Byte);
      PROCEDURE CheckMouse;
      PROCEDURE Done;
   PRIVATE
      LeftEdge,Lambda                                    : DVector;
      ScalingFactor,SlitWidth,SlitSep,XRange,YRange      : Real;
      NumberofSlits                                      : Integer;
      FirstTime,OKtoPLOT                                 : Boolean;
      PROCEDURE SetUpSliders;
      PROCEDURE SetUpHotKeys;
      PROCEDURE HandleSliders;
      PROCEDURE HandleButtons;
      PROCEDURE PositionSlits;
      PROCEDURE PlotSlits;
      PROCEDURE Slits;
      PROCEDURE CalcSlits(YMouse             : Real;
                          VAR Max            : Real;
                          VAR Intensity,XPos : DVector;
                          NumberofXPoints    : Integer);
   END;

   TPointsDiffraction = object
      Active,F10Used               : Boolean;
      Sliders,Slider               : TSliders;
      HotKeys                      : THotKeys;
      ZoomButtons,SourceButtons    : TButtons;
      PROCEDURE Points;
      PROCEDURE Init;
      PROCEDURE SetDefaults;
      PROCEDURE SetUp;
      PROCEDURE HandleHotKeys(KeyNum : Byte);
      PROCEDURE CheckMouse;
      PROCEDURE Done;
   PRIVATE
      Scalefactor,PhaseDelta,THreshold,
      FrequencyDelta,PointSep,Lambda,XRange,YRange   :   Real;
      NumberofSources                                :   Integer;
      PointPosition,Pointphase                       :   DVector;
      PROCEDURE SetUpSlidersandButtons;
      PROCEDURE SetUpHotKeys;
      PROCEDURE HandleSliders;
      PROCEDURE HandleButtons;
      PROCEDURE PositionPoints;
      PROCEDURE PlotSources;
      PROCEDURE ContourPlot;
   END;

   TCornuSlit = Object
       Active,F10Used   : Boolean;
       HotKeys          : THotKeys;
       Sliders          : TSliders;
       PROCEDURE Init;
       PROCEDURE SetDefaults;
       PROCEDURE SetUp;
       PROCEDURE Done;
       PROCEDURE CheckMouse;
       PROCEDURE HandleHotKeys(KeyNum : Byte);
     PRIVATE
       VMaximum,A,B,VMaxIncrement,RealSlitWidth,
       VSlit,SlitArc                                 : Real;
       Lambda,X,Y                                    : DVector;
       SlitWidth,JNumber,INumber                     : Integer;
       FirstTime,InLoop,TOOWIDE                      : Boolean;
       PROCEDURE SetUpSliders;
       PROCEDURE HandleSliders1;
       PROCEDURE HandleSliders2;
       PROCEDURE SetUpHotKeys;
       PROCEDURE PreSpiral;
       PROCEDURE Spiral;
       PROCEDURE Slit;

END;

   TCornuSpiral = Object
       Active,F10Used : Boolean;
       HotKeys        : THotKeys;
       PROCEDURE Init;
       PROCEDURE SetDefaults;
       PROCEDURE SetUp;
       PROCEDURE Done;
       PROCEDURE CheckMouse;
       PROCEDURE HandleHotKeys(KeyNum : Byte);
     PRIVATE
       VMaximum,A,B,VMaxIncrement,NewA,NewB    : Real;
       Lambda,X,Y                              : DVector;
       JNumber,INumber,FirstX,FirstY           : Integer;
       InViewPort                              : Boolean;
       PROCEDURE SetUpHotKeys;
       PROCEDURE StudySpiral1;
       PROCEDURE StudySpiral2;
       PROCEDURE PlotSinCos;
END;

   TCornuEdge = Object
       Active,F10Used : Boolean;
       Sliders        : TSliders;
       HotKeys        : THotKeys;
       PROCEDURE Init;
       PROCEDURE SetDefaults;
       PROCEDURE SetUp;
       PROCEDURE Done;
       PROCEDURE CheckMouse;
       PROCEDURE HandleHotKeys(KeyNum : Byte);
     PRIVATE
       VMaximum,A,B,VMaxIncrement   : Real;
       Lambda,X,Y                   : DVector;
       JNumber,INumber              : Integer;
       FirstTime,InLoop,TOOWIDE     : Boolean;
       PROCEDURE SetUpSliders;
       PROCEDURE HandleSliders;
       PROCEDURE SetUpHotKeys;
       PROCEDURE Edge;
       PROCEDURE Spiral;
END;

  TCornuObstacle = Object
       Active,F10Used : Boolean;
       Sliders        : TSliders;
       HotKeys        : THotKeys;
       PROCEDURE Init;
       PROCEDURE SetDefaults;
       PROCEDURE SetUp;
       PROCEDURE Done;
       PROCEDURE CheckMouse;
       PROCEDURE HandleHotKeys(KeyNum : Byte);
     PRIVATE
       VMaximum,A,B,VMaxIncrement,RealObstWidth,
       VObst,ObstArc                              : Real;
       Lambda,X,Y                                 : DVector;
       ObstWidth,JNumber,INumber                  : Integer;
       FirstTime,InLoop,TOOWIDE                   : Boolean;
       PROCEDURE SetUpSliders;
       PROCEDURE HandleSliders1;
       PROCEDURE HandleSliders2;
       PROCEDURE SetUpHotKeys;
       PROCEDURE PreSpiral;
       PROCEDURE Spiral;
       PROCEDURE Obstacle;
END;

   TCornuTwoSlit = Object
       Active,F10Used : Boolean;
       Sliders        : TSliders;
       HotKeys        : THotKeys;
       PROCEDURE Init;
       PROCEDURE SetDefaults;
       PROCEDURE SetUp;
       PROCEDURE Done;
       PROCEDURE CheckMouse;
       PROCEDURE HandleHotKeys(KeyNum : Byte);
     PRIVATE
       VMaximum,A,B,VMaxIncrement,
       RealLeftSlitWidth,RealRightSlitWidth,
       RealSlitSeparation,RightSlitFactor,SeparationFactor,
       SlitArc,VSlit,LeftSlitArc,RightSlitArc,SeparationArc,
       VLeftSlit,VRightSlit,VSlitSeparation,VTwoSlit          : Real;
       FirstTime,InLoop,TOOWIDE                               : Boolean;
       Lambda,X,Y                                             : DVector;
       JNumber,INumber,
       LeftSlitWidth,RightSlitWidth,SlitSeparation            : Integer;
       PROCEDURE SetUpSliders;
       PROCEDURE HandleSliders1;
       PROCEDURE HandleSliders2;
       PROCEDURE SetUpHotKeys;
       PROCEDURE PreSpiral;
       PROCEDURE Spiral;
       PROCEDURE TwoSlit;
END;

   TCornuTwoObstacle = Object
       Active,F10Used : Boolean;
       Sliders        : TSliders;
       HotKeys        : THotKeys;
       PROCEDURE Init;
       PROCEDURE SetDefaults;
       PROCEDURE SetUp;
       PROCEDURE Done;
       PROCEDURE CheckMouse;
       PROCEDURE HandleHotKeys(KeyNum : Byte);
     PRIVATE
       VMaximum,A,B,VMaxIncrement,
       RealLeftObstacleWidth,RealRightObstacleWidth,
       RealObstacleSeparation,RightObstacleFactor,SeparationFactor,
       ObstArc,VSlit,LeftObstArc,RightObstArc,
       SeparationArc,VLeftObst,VRightObst,VObstSeparation,VTwoObst : Real;
       FirstTime,InLoop,TOOWIDE                                    : Boolean;
       Lambda,X,Y                                                  : DVector;
       JNumber,INumber,LeftObstacleWidth,RightObstacleWidth,
       ObstSeparation                                              : Integer;
       PROCEDURE SetUpSliders;
       PROCEDURE HandleSliders1;
       PROCEDURE HandleSliders2;
       PROCEDURE SetUpHotKeys;
       PROCEDURE PreSpiral;
       PROCEDURE Spiral;
       PROCEDURE TwoObstacle;
END;


VAR
   SlitsDiffraction     : TSlitsDiffraction;
   PointsDiffraction    : TPointsDiffraction;
   CornuSlit            : TCornuSlit;
   CornuSpiral          : TCornuSpiral;
   CornuEdge            : TCornuEdge;
   CornuObstacle        : TCornuObstacle;
   CornuTwoSlit         : TCornuTwoSlit;
   CornuTwoObstacle     : TCornuTwoObstacle;
   MainHotKeys          : THotKeys;

(**********************)  IMPLEMENTATION  (**************************)

FUNCTION PointInside(X,Y,VPortNumber : Integer) : Boolean;
BEGIN
   WITH Views[VPortNumber] DO
    PointInside := ( (X>VX1) AND (X<VX2) AND (Y>VY1) AND (Y<VY2) );
END;

PROCEDURE MakeBlueBox;
BEGIN
   SetFillStyle(SolidFill,Blue);
   SetColor(White);
   HideMouse;
   WITH Views[ViewPortNumber] DO
      Bar3D(0,3,vx2-vx1-3,vy2-vy1,3,True);
   ShowMouse;
END;



PROCEDURE Colours(VAR M : Integer; VAR Lambda : DVector);
BEGIN
   IF Lambda.Value(M) > 640 THEN SetColor(lightred)
              ELSE IF Lambda.Value(M) > 600 THEN SetColor(brown)
              ELSE IF Lambda.Value(M) > 550 THEN SetColor(yellow)
              ELSE IF Lambda.Value(M) > 500 THEN SetColor(lightgreen)
              ELSE IF Lambda.Value(M) > 470 THEN SetColor(lightcyan)
              ELSE SetColor(lightblue);
END;


PROCEDURE Print2(X,Y,places: integer; Num : real; color,BackColor : integer);
VAR
   Row, Col, a : integer;
   s : string[30];
BEGIN
   s := '';
   FOR a := 1 TO places + 4 DO s := s + char(219);
   Col := Round((x - 0.6) * ColWidth);
   Row := Round((y - 0.6) * RowHt);
   SetColor(BackColor);
   OutTextXY(Col, Row, s);
   SetColor(Color);
   OutTextXY(Col, Row, NumStr(num,0,places));
END;

FUNCTION NewSimpson (y : dVector; Number : Integer) : real;
var
  i,n,s : integer;
  x : real;
begin
{$R-}   if y.length =1 then NewSimpson := y.VecPtr^[1] else
   begin
      s := 2*((Number+1) div 2)-1;
      i := 2;
      if s=1 then x := 0 else x := y.VecPtr^[1] + 4*y.VecPtr^[i] + y.VecPtr^[s];
      i := 4;
      while i<s do
      begin
         x := x + 2*y.VecPtr^[i-1] + 4*y.VecPtr^[i];
         i := i + 2;
      end;
      if s<>Number then NewSimpson := x/3 + (y.VecPtr^[s]+y.VecPtr^[Number])/2
      else NewSimpson := x/3;
   end;
{$R+}
end;

{--------------------------- TSlitsDiffraction --------------------------}
PROCEDURE TSlitsDiffraction.Init;
BEGIN
   LeftEdge.init(10);
   Lambda.init(1);
   Sliders.init;
   Slider.init;
   SlitButtons.init;
   SetDefaults;
END;

PROCEDURE TSlitsDiffraction.Done;
BEGIN
   LeftEdge.free;
   Lambda.free;
   Sliders.Done;
   Slider.Done;
   SlitButtons.Done;
END;

PROCEDURE TSlitsDiffraction.SetDefaults;
BEGIN
   Lambda.put(1,550);
   SlitWidth       := 0.01;
   SlitSep         := 0.01;
   ScalingFactor   := 1;
   GraphBackColor  := Black;
   SetUp;
END;

PROCEDURE TSlitsDiffraction.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(1,0.5,0.9,0.15,0.85);       (* Slits *)
   DefineViewPort(2,0.4,1.0,0.1,0.9);
   DefineViewport(3,0,0.39,0.7,0.85);
   SelectViewPort(3);
   MakeBlueBox;
   Print(1,1,'  Click mouse in viewport to ');
   Print(1,2,' see intensities at different ');
   Print(1,3,'    distances from slit(s)');
   SetUpSliders;
   SetUpHotKeys;
   Slits;
END;

PROCEDURE TSlitsDiffraction.SetUpSliders;
BEGIN
   WITH Sliders DO
   BEGIN
      create(1,400,800,Lambda.Value(1),       0.02,0.3,0.39,0,'400','800','Wavelength nm',False);
      create(2,0.005,0.05,SlitWidth, 0.02,0.4,0.39,3,'0.005','0.05','Slit Width mm',False);
      create(3,1,50,Scalingfactor,  0.02,0.2,0.39,0,'1','50','X-Y Scale Multiplier',False);
   END;
   WITH Slider DO
   BEGIN
      create(1,0.005,0.05,SlitSep,   0.02,0.5,0.39,3,'0.005','0.05','Barrier Width',False);
   END;
   WITH SlitButtons DO
   BEGIN
      create(1,0.06,0.62,'');
      create(2,0.15,0.62,'');
      create(3,0.24,0.62,'');
      create(4,0.33,0.62,'');
      SlitButtons.Number := 1;
   END;
END;


PROCEDURE TSlitsDiffraction.HandleSliders;
BEGIN
   Lambda.put(1,Sliders.Value(1));
   SlitWidth := Round(1000*Sliders.Value(2))/1000;
   Scalingfactor := Round(Sliders.Value(3));
   IF SlitButtons.Number <> 1 THEN SlitSep := Round(1000*Slider.Value(1))/1000;
   {FirstTime := True;}
   Slits;
END;

PROCEDURE TSlitsDiffraction.HandleButtons;
BEGIN
   IF SlitButtons.Number = 1 THEN
   BEGIN
      slider.erase(1,Black);
      slider.delete(1);
      SlitSep := 0.01;
   END
   ELSE
   BEGIN
      Slider.init;
      {SlitSep := 0.01;}
      Slider.create(1,0.005,0.05,SlitSep,   0.02,0.5,0.39,3,'0.005','0.05','Barrier Width',False);
      slider.DrawAll;
   END;
   Slits;
END;

PROCEDURE TSlitsDiffraction.SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TSlitsDiffraction.HandleHotKeys(KeyNum : Byte);
BEGIN
    CASE KeyNum OF
    1: ShowHelp('diffract.hlp','HELPSCREEN5');
    2: BEGIN
          IF SlitButtons.Number <> 1 THEN slider.erase(1,Black);
          Done;
          init;
       END;
    3: F10Used := True;
    END;
END;


PROCEDURE TSlitsDiffraction.Slits;
VAR NumberofXPoints                       :  Integer;
    YMouse,ESin,ECos,xSlit,Max            :   Real;
    XPos,Intensity                        :   DVector;
    Wait                                  :   TWaitMessage;
    AString                               :  String;
BEGIN
     Sliders.DrawAll;
     IF SlitButtons.Number > 1 THEN Slider.DrawAll;
     MainHotKeys.Clear;
     HotKeys.Display;
     SlitButtons.DrawAll;
     PrintXY(0,0.67,'        NUMBER OF SLITS');
     PrintXY(0,0.58,'    1       2      3      4');
     Active := true;
     NumberofXPoints := 256;
     XPos.init(NumberofXPoints+1);
     Intensity.init(NumberofXPoints);
     XRange := SlitButtons.Number*ScalingFactor*0.06;
     YRange := 10*SlitButtons.Number*Scalingfactor*0.06;
     PositionSlits;
     DefineScale(1,-XRange,XRange,0,YRange);
     CloseViewPort(2);
     OpenViewPort(1);
     PrintXY(0.3,0.92,'HUYGHENS CONSTRUCTION - SLIT(S)');
     Axis(0,0,XRange/2,YRange/4);
     PutLabel(right,'Distance to Screen  - mm');
     PutLabel(bottom,'   Slit Axis - mm');
     PlotSlits;
     YMouse:= 0.5*ScalingFactor*SlitButtons.Number*0.06;
     CalcSlits(YMouse,Max,Intensity,XPos,NumberofXPoints);
     XPos.free;
     Intensity.free;
END;

PROCEDURE TSlitsDiffraction.CheckMouse;
VAR v1,v2,YMouse,Max                  : Real;
    NumberofXPoints                   : Integer;
    Intensity,XPos                    : DVector;
BEGIN
   IF Active THEN
   BEGIN
      IF (Sliders.Changed OR Slider.Changed) THEN HandleSliders;
      IF SlitButtons.Changed THEN HandleButtons;

      IF(Event.MouseClicked AND PointInside(Event.X,Event.Y,1)) THEN
      WITH Views[1] DO
      BEGIN
         NumberofXPoints := 256;
         Intensity.init(NumberofXPoints);
         XPos.init(NumberofXPoints+1);
         v1 := vy2 - Event.Y;
         v2 := vy2 - vy1;
         YMouse := YRange*v1/v2;
         IF YMouse < SlitButtons.Number*(SlitWidth + SlitSep) THEN
               Announce('   Point chosen too close to Slit(s) for accurate result - choose again   ')

         ELSE
         BEGIN
            SelectScale(1);
            SelectViewport(1);
            PlotSymbol(-0.9*XRange,YMouse,'*');
            CalcSlits(YMouse,Max,Intensity,XPos,NumberofXPoints);
         END;
         Intensity.free;
         XPos.free;
      END;
   END;
END;

PROCEDURE TSlitsDiffraction.PositionSlits;
VAR
   N : Integer;
BEGIN
   LeftEdge.put(1,-SlitButtons.Number*SlitWidth/2  -(SlitButtons.Number-1)*SlitSep/2);
   FOR N := 2 TO SlitButtons.Number DO
      LeftEdge.put(N,LeftEdge.Value(N-1) + SlitWidth + SlitSep);
END;

PROCEDURE TSlitsDiffraction.PlotSlits;
VAR N     : Integer;
BEGIN
   FOR N := 1 to SlitButtons.Number DO
   BEGIN
      SetColor(black);
      PlotLine(LeftEdge.Value(N),0,LeftEdge.Value(N),0);
      PlotLine(LeftEdge.Value(N) + SlitWidth,0,LeftEdge.Value(N) + SlitWidth,0);
      PlotLine(LeftEdge.Value(N),0,Leftedge.Value(N) + SlitWidth,0);
   END;
   SetColor(white);
END;

PROCEDURE TSlitsDiffraction.CalcSlits(YMouse              : Real;
                                       VAR Max            : Real;
                                       VAR Intensity,XPos : DVector;
                                       NumberofXPoints    : Integer);
VAR M,N,IMin,IMax                      :   Integer;
    ESin,ECos,r,Min,Increment,
    Costheta,c,phase,k,xSlit,amplitude :   Real;
    ColNUm                             :   Integer;
    Wait                               :   TWaitmessage;
BEGIN
   XPos.put(1,-XRange);
   Wait.Show;
   Increment := 2*XRange/NumberofXPoints;
   FOR N := 1 to NumberofXPoints DO
   BEGIN
      ESin := 0;
      ECos := 0;
      FOR M := 1 to SlitButtons.Number DO
      BEGIN
         xSlit := LeftEdge.Value(M) + 1e-6*Lambda.Value(1)/2;
         WHILE xSlit < LeftEdge.Value(M) + SlitWidth DO
         BEGIN
            R := Sqrt(Sqr(XPos.Value(N) + xSlit) + Sqr(YMouse));
            Costheta := Abs((YMouse)/R);
            C := (1 + Costheta)/2;
            Amplitude := C/R;
            Phase := 2 * pi * R/(1e-6*Lambda.Value(1));
            ESin := ESin + amplitude * Sin(phase);
            ECos := ECos + amplitude * Cos(phase);
            xSlit := xSlit + 1e-6*Lambda.Value(1);
         END;
      END;
      Intensity.put(N,Sqr(ESin) + Sqr(ECos));
      XPos.put((N+1),XPos.Value(N) + Increment);
      IF N mod 25 = 0 THEN Wait.Update;
   END;
   Wait.Hide;
   Intensity.MinMax(IMin,Min,IMax,Max);
   FOR N := 1 to NumberofXPoints DO
        Intensity.put(N, YMouse + Intensity.Value(N)*YRange/(10*Max));
   ColNum := 1;
   Colours(ColNum,Lambda);
   PlotDVectors(XPos,Intensity,1,Intensity.size);
   SetColor(white);

END;

{---------------------------- TPointsDiffraction -----------------------}
PROCEDURE TPointsDiffraction.Init;
BEGIN
   DefineViewPort(2,0.52,0.95,0.2,0.77);
   PointPosition.init(10);
   Pointphase.init(10);
   Sliders.init;
   Slider.init;
   ZoomButtons.init;
   SourceButtons.init;
   SetDefaults;
END;

PROCEDURE TPointsDiffraction.Done;
BEGIN
    PointPosition.free;
    Pointphase.free;
    Sliders.Done;
    Slider.Done;
    ZoomButtons.Done;
    SourceButtons.Done;
END;


PROCEDURE TPointsDiffraction.SetDefaults;
BEGIN
   ScaleFactor     := 1;
   Lambda          := 550;
   PointSep        := 550;
   PhaseDelta      := 0;
   THreshold       := 0.2;
   NumberofSources := 2;
   SetUp;
END;

PROCEDURE TPointsDiffraction.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(2,0.52,0.95,0.2,0.77);
   DefineViewPort(1,0.42,1,0.06,0.85);
   SetUpSlidersandButtons;
   SetUpHotKeys;
   IF Active THEN
   BEGIN
      Sliders.DrawAll;
      IF SourceButtons.Number > 1 THEN Slider.DrawAll;
   END;
   Points;
END;

PROCEDURE TPointsDiffraction.SetUpSlidersandButtons;
BEGIN
   WITH Sliders DO
   BEGIN
      create(2,400,800,Lambda,0,0.4,0.41,0,'400','800','Wavelength nm',False);
      create(3,1,20,Scalefactor,0,0.3,0.41,0,'1','20','X-Y Scale Multiplier',False);
      create(4,0.1,1,Threshold,0,0.2,0.41,3,'0.1','1','Threshold',False);
    END;

    WITH Slider DO
    BEGIN
       create(1,20,2500,PointSep,0,0.6,0.41,1,'20','2500','Separation nm',False);
       create(2,-180,180,Phasedelta,0,0.5,0.41,1,'-180','180','Phase Delta deg',False);
    END;

    WITH SourceButtons DO
    BEGIN
       create(1,0.03,0.75,'');
       create(2,0.08,0.75,'');
       create(3,0.13,0.75,'');
       create(4,0.18,0.75,'');
       create(5,0.23,0.75,'');
       create(6,0.28,0.75,'');
       create(7,0.33,0.75,'');
       create(8,0.38,0.75,'');
       SourceButtons.Number := 2;
    END;

    WITH ZoomButtons DO
    BEGIN
       create(1,0.1,0.1,'Low Range');
       create(2,0.1,0.15,'High Range');
       Number := 2;
    END;
END;

PROCEDURE TPointsDiffraction.HandleButtons;
BEGIN
   IF SourceButtons.Number = 1 THEN
   BEGIN
      slider.erase(1,Black);
      slider.erase(2,Black);
      slider.delete(1);
      slider.delete(2);
   END
   ELSE
   BEGIN
      slider.create(1,20,2500,PointSep,0,0.6,0.41,1,'20','2500','Separation nm',False);
      slider.create(2,-180,180,Phasedelta,0,0.5,0.41,1,'-180','180','Phase Delta deg',False);
      slider.DrawAll;
   END;
   NumberofSources := SourceButtons.Number;

   WITH Sliders DO
         BEGIN
            erase(4,black);
            delete(4);
            Threshold := 0.2;
            create(4,0.1,1,Threshold,0,0.2,0.41,3,'0.1','1','Threshold',False);
            DrawAll;
            ZoomButtons.Number := 2;
            ZoomButtons.DrawAll;
         END;
   Points;
END;


PROCEDURE TPointsDiffraction.HandleSliders;
BEGIN
   Lambda      := Sliders.Value(2);
   Scalefactor := Round(Sliders.Value(3));
   Threshold   := Sliders.Value(4);
   IF SourceButtons.Number <> 1 THEN
   BEGIN
      PointSep   := Slider.Value(1);
      Phasedelta := Slider.Value(2);
   END;
   Points;
END;

PROCEDURE TPointsDiffraction.SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TPointsDiffraction.HandleHotKeys(KeyNum : Byte);
BEGIN
    CASE KeyNum OF
    1: ShowHelp('diffract.hlp','HELPSCREEN4');
    2: BEGIN
          Done;
          init;
       END;
    3: F10Used := True;
    END;
END;


PROCEDURE TPointsDiffraction.PositionPoints;
VAR N : Integer;
BEGIN
   PointPosition.put(1,-(NumberofSources/2 -1/2)*PointSep);
   Pointphase.put(1,0);
   IF NumberofSources > 1 THEN
   BEGIN
      FOR N := 2 to NumberofSources DO
      BEGIN
         PointPosition.put(N,PointPosition.Value(N-1) + PointSep);
         Pointphase.put(N,Pointphase.Value(N-1) + pi/180*PhaseDelta);
      END;
   END;
END;

PROCEDURE TPointsDiffraction.PlotSources;
VAR N  :  Integer;
BEGIN
   SetColor(white);
   FOR N := 1 to NumberofSources DO
      PlotSymbol(PointPosition.Value(N),0,'*');
END;



PROCEDURE TPointsDiffraction.ContourPlot;
VAR ESin,ECos,r,sqrtr,phase,
    xIncrement,yIncrement,Limit,Min,Max,Value          : Real;
    ContourXPos,ContourYPos                            : DVector;
    K,N,M,NumberofValues,MinRow,MinCol,MaxRow,MaxCol   : Integer;
    Contour                                            : TContour;
    Intensity                                          : DMatrix;
    Wait                                               : TWaitmessage;
BEGIN
   NumberofValues := 51;
   Intensity.init(NumberofValues,NumberofValues);
   ContourXPos.init(NumberofValues + 1);
   ContourYPos.init(NumberofValues + 1);
   Wait.Show;
   xIncrement := XRange/((NumberofValues-1)/2);
   yIncrement := YRange/((NumberofValues-1)/2);
   ContourYPos.put(1,-YRange);
   ContourXPos.put(1,-XRange);
   Limit := PointSep/5;
   FOR K := 1 to NumberofValues  DO             {y range loop }
   BEGIN
      FOR N := 1 to NumberofValues  DO          {x range loop }
      BEGIN
         ESin := 0;
         ECos := 0;
         FOR M := 1 to NumberofSources DO
         BEGIN
            r := Sqrt( Sqr(ContourXPos.Value(N) + PointPosition.Value(M))
                     + Sqr(ContourYPos.Value(K)) );
            IF  r < Limit THEN r := Limit
                ELSE r := r;
            phase := (2* pi * r/Lambda
                        + Pointphase.Value(M));
            ESin := ESin + Sin(phase)/r;
            ECos := ECos + Cos(phase)/r;
         END;
         ContourXPos.put((N+1),ContourXPos.Value(N) + xIncrement);
         Intensity.put(K,N,Sqr(ESin) + Sqr(ECos));
      END;
      ContourXPos.put(1, -XRange);
      ContourYPos.put((K + 1),ContourYPos.Value(K) + yIncrement);
      IF K mod 5 = 0 THEN Wait.UpDate;
   END;
   Wait.Hide;
   Intensity.MinMax(MinRow,MinCol,Min,MaxRow,MaxCol,Max);
   FOR K := 1 to NumberofValues DO
   BEGIN
      FOR N := 1 to NumberofValues DO
      BEGIN
        Intensity.put(K,N,Intensity.Value(K,N)/Max);
        IF Intensity.Value(K,N) > THreshold THEN
           Intensity.put(K,N,THreshold);
      END;
   END;
   Contour.init;
   CloseViewPort(1);
   GraphBackColor := black;
   OpenViewPort(2);
   Contour.DrawMatrix(Intensity);
   Axis(0,0,XRange/2,YRange/2);
   PlotSources;
   ContourXPos.free;
   ContourYPos.free;
   Intensity.free
END;

PROCEDURE TPointsDiffraction.Points;
VAR Intensity,XPos                                       :   DVector;
    AString                                              :   String;
BEGIN
   IF NOT Active THEN
   BEGIN
      Sliders.DrawAll;
      IF NumberofSources > 1 THEN Slider.DrawAll;
   END;
   ZoomButtons.DrawAll;
   SourceButtons.DrawAll;
   PrintXY(0.02,0.81,'      NUMBER OF SOURCES');
   PrintXY(0.02,0.71,'1   2   3   4   5   6   7   8');
   MainHotKeys.Clear;
   HotKeys.Display;
   Active := true;
   XPos.init(201);
   Intensity.init(200);
   PrintXY(0.2,0.9,'INTENSITY VARIATION FROM POINT SOURCE(S)');
   XRange := 100*round(5*NumberofSources*ScaleFactor*PointSep/200);
   YRange := 100*round(5*NumberofSources*ScaleFactor*PointSep/200);
   DefineScale(1,-XRange,XRange,-YRange,YRange);
   PositionPoints;
   ContourPlot;
   XPos.free;
   Intensity.free;
END;

PROCEDURE TPointsDiffraction.CheckMouse;
BEGIN
   IF Active THEN

   BEGIN
      IF Sliders.Changed OR Slider.Changed THEN HandleSliders;
      IF SourceButtons.Changed THEN HandleButtons;

      IF ZoomButtons.Changed THEN
      BEGIN
         IF (ZoomButtons.Number = 2) THEN
         WITH Sliders DO
         BEGIN
            erase(4,black);
            delete(4);
            Threshold := 0.2;
            create(4,0.1,1,Threshold,0,0.2,0.41,3,'0.1','1','Threshold',False);
            DrawAll;
            Points;
         END;

         IF (ZoomButtons.number = 1) THEN
         WITH Sliders Do
         BEGIN
            erase(4,black);
            delete(4);
            Threshold := 0.05;
            create(4,0.001,0.1,Threshold,0,0.2,0.41,3,'0.001','0.1','Threshold',False);
            DrawAll;
            Points;
         END;
      END;
   END;
END;

{*************************** TCornuSlit **********************}
PROCEDURE TCornuSlit.Init;
BEGIN
   Lambda.init(1);
   Sliders.init;
   SetDefaults;
END;

PROCEDURE TCornuSlit.Done;
BEGIN
   Lambda.free;
   Sliders.Done;
END;

PROCEDURE TCornuSlit.SetDefaults;
BEGIN
   A              := 0.1;
   B              := 1;
   RealSlitWidth  := 1.0;
   Lambda.put(1,550);
   GraphBackColor := Black;
   SetUp;
END;

PROCEDURE TCornuSlit.SetUp;
BEGIN
   IF NOT Active  THEN CloseViewPort(20);
   DefineViewPort(2,0.1,0.9,0.57,0.87);     {Diffraction window}
   DefineViewPort(1,0.5,0.97,0.08,0.45);    {Blue message box}
   DefineViewPort(6,0.0,0.38,0.4,0.9);
   DefineViewPort(3,0,0.097,0.5,0.9);
   FirstTime      := True;
   InLoop         := False;
   SetUpHotKeys;
   SetUpSliders;
   GraphBackColor := Black;
   PreSpiral;
END;


PROCEDURE TCornuSlit.SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TCornuSlit.HandleHotKeys(KeyNum : Byte);
VAR VMax                                   : Real;
    MenuKeyUsed,DefaultKeyUsed,FirstRun    : Boolean;
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN23');
   2: BEGIN
         Done;
         Init;
      END;
   3:  F10Used := True;
   END;
END;

PROCEDURE TCornuSlit.SetUpSliders;
BEGIN
   WITH Sliders DO
   BEGIN
      create(1,0.01,1,A,     0.02,0.4,0.45,    2,'0.01','1','Source to Slit m',False);
      create(2,0.001,1,B,0.02,0.3,0.45,3,'0.001','1',' Slit to Screen m',False);
      create(3,0.05,3,RealSlitWidth,0.02,0.2,0.45,2,'0.05','3','Slit Width mm',False);
      create(4,400,800,Lambda.Value(1),0.02,0.1,0.45,0,'400','800','Wavelength nm',False);
    END;
END;

PROCEDURE TCornuSlit.CheckMouse;
BEGIN
  IF (Active AND NOT InLoop AND Sliders.Changed) THEN HandleSliders1;
END;

PROCEDURE TCornuSlit.HandleSliders1;
VAR VMax                                       : Real;
    KeyNum                                     : Byte;
    MenuKeyUsed,DefaultKeyUsed,FirstRun        : Boolean;
BEGIN
      A := Sliders.Value(1);
      B := Sliders.Value(2);
      Lambda.put(1,Sliders.Value(4));
      RealSlitWidth := Sliders.Value(3);
      FirstRun := True;
      MenuKeyUsed := False;
      DefaultKeyUsed := False;
      REPEAT
         CheckForEvents;
         InLoop := True;
         SlitArc := a*arctan(0.001*RealSlitWidth/A);
         VSlit := 0.5*SlitArc*sqrt(2*(A+B)/
                  (A*B*lambda.Value(1)*1e-9));
         TOOWIDE := (VSlit > 7.5);
         IF TOOWIDE THEN
         BEGIN
            IF FirstRun THEN
            BEGIN
               SelectViewport(1);
               MakeBlueBox;
               Beep;
               Print(1,2,' Inappropriate Choice of Parameters');
               Print(1,4,'   Increase Source/Slit Distance');
               Print(1,5,'  or Increase Slit/Screen Distance');
               Print(1,6,'       or Decrease Slit Width ');
               Print(1,7,'       or Increase Wavelength ');
            END;
            FirstRun := False;
         END;
         IF Sliders.Changed THEN HandleSliders2;
         IF HotKeys.Pressed(KeyNum) THEN
         BEGIN
            IF KeyNum = 1 THEN ShowHelp('diffract.hlp','HELPSCREEN26')
            ELSE IF KeyNum = 3 THEN
            BEGIN
               DefaultKeyUsed := True;
               CloseViewPort(1);
               CornuSlit.SetDefaults;
            END
            ELSE IF KeyNum = 4 THEN
            BEGIN
               MenuKeyUsed := True;
               TOOWIDE := False;
            END;
         END;
         UNTIL NOT TOOWIDE;
         IF MenuKeyUsed THEN F10Used := True;
         IF (NOT  F10Used AND NOT DefaultKeyUsed) THEN
         BEGIN
            CloseViewPort(1);
            SlitArc := a*arctan(0.001*RealSlitWidth/A);
            VSlit := 0.5*SlitArc*sqrt(2*(A+B)/
                     (A*B*lambda.Value(1)*1e-9));
            VMax := VSlit;
            WHILE VMax < 15 DO
            BEGIN
               VMax := VMax + VSlit;
            END;
            VMaximum := VMax;
            VMaxIncrement := VMaximum/300;
            SlitWidth := round(2*VSlit/VMaxIncrement);
            InLoop := False;
            PreSpiral;
         END;
END;

PROCEDURE TCornuSlit.HandleSliders2;
BEGIN
      Delay(20);
      A := Sliders.Value(1);
      B := Sliders.Value(2);
      Lambda.put(1,Sliders.Value(4));
      RealSlitWidth := Sliders.Value(3);
      SlitArc := a*arctan(0.001*RealSlitWidth/A);
      VSlit := 0.5*SlitArc*sqrt(2*(A+B)/
               (A*B*lambda.Value(1)*1e-9));
      TOOWIDE := (VSlit > 7.5);
END;

PROCEDURE TCornuSlit.PreSpiral;
VAR VMax   : Real;
BEGIN
   SlitArc := a*arctan(0.001*RealSlitWidth/a);
   VSlit := 0.5*SlitArc*sqrt( 2*(A+B)/(A*B*Lambda.Value(1)*1e-9) );
   VMax := VSlit;
   WHILE VMax < 15 DO
   BEGIN
      Vmax := Vmax + VSlit;
   END;
   VMaximum := VMax;
   VMaxIncrement  := VMaximum/300;
   SlitWidth := round(2*VSlit/VMaxIncrement);
   Spiral;
END;

PROCEDURE TCornuSlit.Spiral;
VAR I,K,L,NUmber                     : Integer;
    DeltaV,VMin,VMax                 : Real;
    V,Cs,Sn                          : DVector;
    Wait                             : TWaitMessage;
    AString                          : String;
BEGIN
   JNumber := Round(Vmaximum/VMaxIncrement) + 1 ;
   INumber := 4*(JNumber -1);
   Cs.init(INumber + 1);
   Sn.init(INumber + 1);
   X.init(2*JNumber + 1);
   Y.init(2*JNumber + 1);
   V.init(INumber + 2);
   VMin := 0;
   V.put(1,VMin);
   DeltaV := (Vmaximum - VMin)/(INumber);
   Wait.Show;
   FOR I := 1 to INumber+ 1  DO
   BEGIN
      Cs.put(I,cos(pi*V.Value(I)*V.Value(I)/2));
      Sn.put(I,sin(pi*V.Value(I)*V.Value(I)/2));
      V.put(I+1,V.Value(I) + DeltaV);
      IF I mod 50 = 0 THEN Wait.UpDate;
   END;
   K := 1;
   For L := JNumber to 2*JNumber -1   Do
   BEGIN
      X.put(L,DeltaV*NewSimpson(Cs,K));
      Y.put(L,DeltaV*NewSimpson(Sn,K));
      K := K + 4;
      IF L mod 10 = 0 THEN Wait.UpDate;
   END;
   Wait.Hide;
   FOR K := 1 to JNumber  DO
   BEGIN
      X.put(K,-X.Value(2*JNumber - K));
      Y.put(K,-Y.Value(2*JNumber - K));
   END;
   Cs.free;
   Sn.free;
   V.free;
   Slit;
END;

PROCEDURE TCornuSlit.Slit;
VAR K,M,IMin1,IMin2,IMax1,IMax2         : Integer;
    Tx,Ty,Vvalue,h,Min1,Min2,Max1,Max2  : Real;
    IntSlit,XPositionSlit               : DVector;
    AString                             : String;
BEGIN
   Sliders.DrawAll;
   FirstTime := False;
   MainHotKeys.Clear;
   HotKeys.Display;
   Active := True;
   CloseViewPort(3);
   RubOutXY(0.05,0.559,75,Black);
   IntSlit.init(2*JNumber - SlitWidth - 1);
   XPositionSlit.init(2*JNumber - SlitWidth - 1);
   OpenViewPort(2);
   PutLabel(top,'DIFFRACTION FROM A SLIT');
   PutLabel(bottom,'Position on Axis - m');
   PutLabel(left,'Intensity');
   Vvalue:= -VMaximum;
   FOR K := 1 to 2*JNumber - SlitWidth - 1  DO
   BEGIN
      XPositionSlit.put(K,(Vvalue+Slitwidth*VMaxIncrement/2)
                          *sqrt( (A+B)*B*Lambda.Value(1)*1e-9/(2*A) ) );
      Tx :=   X.Value(Slitwidth + K)
            - X.Value(K);
      Ty :=   Y.Value(Slitwidth + K)
            - Y.Value(K);
      IntSlit.put(K, (Tx*Tx + Ty*Ty));
      Vvalue := Vvalue + VMaxIncrement;
   END;
   h := 0.5*(A+B)*Tan(0.001*RealSlitWidth/a); {half Height of slit}
   IntSlit.MinMax(IMin1,Min1,IMax1,Max1);
   XPositionSlit.MinMax(IMin2,Min2,IMax2,Max2);
   DefineScale(2,Min2,Max2,Min1,1.1*Max1);
   SelectScale(2);
   Axis(0,0,TickSpace(Max2-Min2),TickSpace(Max1-Min1));
   SetColor(Black);
   PlotLine(0,0,0,10);
   M := 1;
   Colours(M,Lambda);
   PlotDVectors(XPositionSlit,IntSlit,1,2*JNumber - SlitWidth - 2);
   SetColor(lightgray);
   SetLineStyle(0,0,3);
   PlotLine(Min2,0.1*Max1,-h,0.1*Max1);
   PlotLine(h,0.1*Max1,Max2,0.1*Max1);
   SetLineStyle(0,0,1);
   SetColor(White);
   IntSlit.free;
   XPositionSlit.free;
   X.free;
   Y.free;
END;

{-------------------------   TCornuSpiral   -----------------------}
PROCEDURE TCornuSpiral.Init;
BEGIN
   Lambda.init(1);
   SetDefaults;
   SetUp;
END;

PROCEDURE TCornuSpiral.Done;
BEGIN
   Lambda.free;
END;

PROCEDURE TCornuSpiral.SetDefaults;
BEGIN
   A          := 0.1;
   B          := 1;
   Lambda.put(1,550);
   SetUpHotKeys;
   FirstX     := 0;
   FirstY     := 0;
   InViewPort := False;
   GraphBackColor := Black;
END;

PROCEDURE TCornuSpiral.SetUp;
BEGIN
   IF NOT  Active  THEN CloseViewPort(20);
   DefineViewPort(3,0.47,0.93,0.38,0.93);      {Cornu Spiral - large}
   DEfineViewPort(6,0.0,0.38,0.4,0.9);       {Blue message box}
   DefineViewPort(7,0.01,0.45,0.13,0.43);   {Parameter box}
   DefineViewPort(8,0.05,0.93,0.13,0.3);   {Fresnel Cos and Sine}
   DefineViewPort(9,0,0.96,0.06,0.3);    {Blackout 8}
   StudySpiral1;
END;

PROCEDURE TCornuSpiral.SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      init(4);
      Key[1] := 'F1-Help';
      Key[2] := 'F2-Step';
      Key[3] := 'F9-Defaults';;
      Key[4] := 'F10-Menu';
   END;
END;

Procedure TCornuSpiral.PlotSinCos;
VAR HNumber,I                 : Integer;
    VMin,HDeltaV,Value        : Real;
    HV, HSn, HCs              : DVector;
BEGIN
   HNumber := 400;
   HSn.init(HNumber + 1);
   HCs.init(HNumber + 1);
   HV.init(HNumber + 2);
   VMin := 0;
   HV.put(1,VMin);
   HDeltaV := (CornuSpiral.Vmaximum+0.2 - VMin)/(HNumber);
   FOR I := 1 to HNumber+ 1  DO
   BEGIN
      HSn.put(I,sin(pi*HV.Value(I)*HV.Value(I)/2));
      HCs.put(I,cos(pi*HV.Value(I)*HV.Value(I)/2));
      HV.put(I+1,HV.Value(I) + HDeltaV);
   END;
   CloseViewPort(9);
   GraphBackColor := Black;
   OpenViewPort(8);
   AutoScaling(10,HV,HCS);
   SelectScale(10);
   PutLabel(bottom,'Fresnel Sine and Cosine Function - V Value');
   SetColor(LightMagenta);
   PutLabel(bottom,'        Sine                              ');
   PlotDVectors(HV,HSn,1,HNumber);
   SetColor(lightGreen);
   PutLabel(bottom,'                 Cosine                   ');
   PlotDVectors(HV,HCs,1,HNumber+1);
   Value := HV.Value(HNumber+1);
   SetColor(lightred);
   PlotLine(CornuSpiral.VMaximum,-2,CornuSpiral.VMaximum,2);
   Value := CornuSpiral.VMaximum;
   SetColor(white);
   HCs.free;
   HSn.free;
   HV.Free;
END;

PROCEDURE TCornuSpiral.HandleHotKeys(KeyNum : Byte);
VAR HNumber,I                 : Integer;
    VMin,HDeltaV,Value        : Real;
    HV,HCs,HSn                : DVector;
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN27');
   2: BEGIN
         IF CornuSpiral.VMaximum < 5 THEN
         BEGIN
            CornuSpiral.VMaximum := CornuSpiral.Vmaximum + 1;
            CornuSpiral.StudySpiral2;
         END
         ELSE
         BEGIN
            SelectViewPort(3);
            Print(1,13,' Maximum V = 5 reached');
         END;
      END;
   3: BEGIN
         Done;
         init;
      END;
   4:  F10Used := True;
   END;
END;

PROCEDURE TCornuSpiral.CheckMouse;
VAR XX,YY,Button : Integer;
    AString      : String;
BEGIN
   IF Active THEN
   BEGIN
      MouseGlobalPosn(xx,yy,button);
      WITH Views[3] DO
      BEGIN
         IF(xx>vx1) AND (xx<vx2) AND(yy>vy1) AND(yy<vy2) THEN
         BEGIN
            SelectViewPort(3);
            NewA := 2*(xx - vx2)/(vx2 - vx1) + 1;
            NewB := 1 - 2*(yy - vy1)/(vy2 - vy1);
            IF (yy <> FirstY) OR (xx <> FirstX) THEN
            BEGIN
               AString := concat(chr(229),'  = ');
               Print(2,2,AString );
               AString := concat(chr(230),'  = ');
               Print(2,3,AString);
               Print2(8,2,4,NewA,lightgreen,Black);
               Print2(8,3,4,NewB,lightgreen,Black);
               FirstX := xx;
               FirstY := yy;
               InViewPort := True;
               SetColor(white);
            END;
         END
         ELSE
         BEGIN
            IF InViewPort THEN
            BEGIN
               SelectViewPort(3);
               RubOut(2,2,13,Black);
               RubOut(2,3,13,Black);
            END;
            InViewPort := False;
         END;
      END;
   END;
END;

PROCEDURE TCornuSpiral.StudySpiral1;
BEGIN
   CloseViewPort(20);
   Active := False;
   IF NOT Active THEN MainHotKeys.Clear;
   HotKeys.Display;
   VMaxIncrement := 0.05;
   VMaximum := 1;
   StudySpiral2;
END;

PROCEDURE TCornuSpiral.StudySpiral2;
VAR I,K,L,Number                                          : Integer;
    DeltaV,VMin,XPosValue,XNegValue,YPosValue,YNegValue   : Real;
    V,Cs,Sn                                               : DVector;
    Wait                                                  : TWaitMessage;
    AString                                               : String;
BEGIN
   JNumber := trunc(Vmaximum/VMaxIncrement) + 1 ;
   INumber := 4*(JNumber -1);
   Cs.init(INumber + 1);
   Sn.init(INumber + 1);
   X.init(2*JNumber + 1);
   Y.init(2*JNumber + 1);
   V.init(INumber + 2);
   VMin := 0;
   V.put(1,VMin);
   DeltaV := (Vmaximum - VMin)/(INumber);
   Wait.Show;
   FOR I := 1 to INumber+ 1  DO
   BEGIN
      Cs.put(I,cos(pi*V.Value(I)*V.Value(I)/2));
      Sn.put(I,sin(pi*V.Value(I)*V.Value(I)/2));
      V.put(I+1,V.Value(I) + DeltaV);
      IF I mod 50 = 0 THEN Wait.UpDate;
   END;
   K := 1;
   For L := JNumber to 2*JNumber -1   Do
   BEGIN
      X.put(L,DeltaV*NewSimpson(Cs,K));
      Y.put(L,DeltaV*NewSimpson(Sn,K));
      K := K + 4;
      IF L mod 10 = 0 THEN Wait.UpDate;
   END;
   Wait.Hide;
   FOR K := 1 to JNumber  DO
   BEGIN
      X.put(K,-X.Value(2*JNumber - K));
      Y.put(K,-Y.Value(2*JNumber - K));
   END;
   CloseViewPort(9);
   IF Active = True THEN SelectViewPort(3)
      ELSE OpenViewPort(3);
   SetColor(white);
   Print(1,1,' CORNU SPIRAL');
   PrintXY(0.42,0.7,chr(230));   {along y}
   PrintXY(0.7,0.32,chr(229));   {along x}
   DefineScale(1,-1,1,-1,1);
   SelectScale(1);
   Axis(0,0,0.5,0.5);
   PlotDVectors(X,Y,1,2*JNumber - 1);
   XPosValue := X.Value(2*JNumber - 1);
   YPosValue := Y.Value(2*JNumber - 1);
   XNegValue := X.Value(1);
   YNegValue := Y.Value(1);
   SetColor(lightred);
   PlotSymbol(XPosValue,YPosValue,'*');
   PlotSymbol(XNegValue,YNegValue,'*');
   SetColor(white);
   IF Active = False THEN
   BEGIN
      SelectViewPort(6);
      MakeBlueBox;
      Print(1,1,'      Move the Mouse on');
      Print(1,2,'    the Spiral to measure');
      Print(1,3,'        X and Y values');
      Print(1,5,'       The red asterisks ');
      Print(1,6,'         denote integer  ');
      Print(1,7,'          values of V');
      Print(1,9,' Use F2 Key to advance spiral');
   END;
   Active := True;
   Cs.free;
   Sn.free;
   V.free;
   PlotSinCos;
END;

{***************************TCornuEdge ********************}

PROCEDURE TCornuEdge.Init;
BEGIN
   Lambda.init(1);
   Sliders.init;
   SetDefaults;
END;

PROCEDURE TCornuEdge.Done;
BEGIN
   Lambda.free;
   Sliders.Done;
END;

PROCEDURE TCornuEdge.SetDefaults;
BEGIN
   A             := 0.1;
   B             := 1;
   Lambda.put(1,550);
   VMaximum      := 10;
   VMaxIncrement := 0.05;
   SetUp;
END;

PROCEDURE TCornuEdge.SetUp;
BEGIN
   IF NOT Active  THEN CloseViewPort(20);
   DefineViewPort(2,0.1,0.9,0.57,0.87);     {Diffraction window}
   DefineViewPort(7,0.01,0.45,0.13,0.43);   {Parameter box}
   FirstTime := True;
   SetUpHotKeys;
   SetUpSliders;
   GraphBackColor := Black;
   Spiral;
END;

PROCEDURE TCornuEdge.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TCornuEdge.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN22');
   2: BEGIN
         Done;
         init;
      END;
   3:  F10Used := True;
   END;
END;

PROCEDURE TCornuEdge.SetUpSliders;
BEGIN
   WITH Sliders DO
   BEGIN
      create(1,0.01,1,A,     0.02,0.4,0.45,    2,'0.01','1','Source to Edge m',False);
      create(2,0.01,1,B,0.02,0.3,0.45,2,'0.01','1',' Edge to Screen m',False);
      create(3,400,800,Lambda.Value(1),0.02,0.2,0.45,0,'400','800','Wavelength nm',False);
    END;
END;

PROCEDURE TCornuEdge.CheckMouse;
BEGIN
   IF Active AND Sliders.Changed THEN HandleSliders;
END;

PROCEDURE TCornuEdge.HandleSliders;
BEGIN
   REPEAT
      CheckForEvents;
      Delay(20);
      A := Sliders.Value(1);
      B := Sliders.Value(2);
      Lambda.put(1,Sliders.Value(3));
   UNTIL NOT Sliders.changed;
   Spiral;
END;

PROCEDURE TCornuEdge.Spiral;
VAR I,K,L,Number                  : Integer;
    DeltaV,VMin                   : Real;
    V,Cs,Sn                       : DVector;
    Wait                          : TWaitMessage;
    AString                       : String;
BEGIN
   JNumber := Round(Vmaximum/VMaxIncrement) + 1 ;
   INumber := 4*(JNumber -1);
   Cs.init(INumber + 1);
   Sn.init(INumber + 1);
   X.init(2*JNumber + 1);
   Y.init(2*JNumber + 1);
   V.init(INumber + 2);
   VMin := 0;
   V.put(1,VMin);
   DeltaV := (Vmaximum - VMin)/(INumber);
   Wait.Show;
   FOR I := 1 to INumber+ 1  DO
   BEGIN
      Cs.put(I,cos(pi*V.Value(I)*V.Value(I)/2));
      Sn.put(I,sin(pi*V.Value(I)*V.Value(I)/2));
      V.put(I+1,V.Value(I) + DeltaV);
      IF I mod 50 = 0 THEN Wait.UpDate;
   END;
   K := 1;
   For L := JNumber to 2*JNumber -1   Do
   BEGIN
      X.put(L,DeltaV*NewSimpson(Cs,K));
      Y.put(L,DeltaV*NewSimpson(Sn,K));
      K := K + 4;
      IF L mod 10 = 0 THEN Wait.UpDate;
   END;
   Wait.Hide;
   FOR K := 1 to JNumber  DO
   BEGIN
      X.put(K,-X.Value(2*JNumber - K));
      Y.put(K,-Y.Value(2*JNumber - K));
   END;
   Cs.free;
   Sn.free;
   V.free;
   Edge;
END;

PROCEDURE TCornuEdge.Edge;
VAR J,K,IMin1,IMin2,IMax1,IMax2,M       : Integer;
    DeltaV,Vvalue,Min1,Min2,Max1,Max2   : Real;
    IntEdge,XPositionEdge               : DVector;
    AString                             : String;
BEGIN
   Sliders.DrawAll;
   Active := True;
   FirstTime := False;
   MainHotKeys.Clear;
   HotKeys.Display;
   RubOutXY(0.05,0.559,75,Black);
   IntEdge.init(2*JNumber);
   XPositionEdge.init(2*JNumber - 1);
   OpenViewPort(2);
   PutLabel(top,'DIFFRACTION FROM AN EDGE');
   PutLabel(bottom,'Position on Axis - m');
   PutLabel(left,'Intensity');
   Vvalue:= -VMaximum;
   J := 1;
   Colours(J,Lambda);
   FOR K := 1 to 2*JNumber-1  DO
      BEGIN
         XPositionEdge.put(K,Vvalue*sqrt( (A+B)*B*Lambda.Value(1)*1e-9/(2*A)) );
         IntEdge.put(K, (0.5+X.Value(K)) * (0.5+X.Value(K))
                + (0.5+Y.Value(K)) * (0.5+Y.Value(K)) );
         Vvalue := Vvalue + VMaxIncrement;
      END;
   Intedge.MinMax(IMin1,Min1,IMax1,Max1);
   XPositionEdge.MinMax(IMin2,Min2,IMax2,Max2);
   DefineScale(2,Min2,Max2,Min1,1.1*Max1);
   SelectScale(2);
   Axis(0,0,TickSpace(Max2-Min2),TickSpace(Max1-Min1));
   SetColor(Black);
   PlotLine(0,0,0,10);
   SetColor(lightgray);
   SetLineStyle(0,0,3);
   PlotLine(Min2,0.1*Max1,0,0.1*Max1);
   SetLineStyle(0,0,1);
   M := 1;
   Colours(M,Lambda);
   PlotDVectors(XPositionEdge,IntEdge,1,2*JNumber -1);
   SetColor(white);
   IntEdge.free;
   XPositionEdge.free;
   X.free;
   Y.free;
END;

{*************************** TCornuObstacle **********************}
PROCEDURE TCornuObstacle.Init;
BEGIN
   Lambda.init(1);
   Sliders.init;
   SetDefaults;
END;

PROCEDURE TCornuObstacle.Done;
BEGIN
   Lambda.free;
   Sliders.Done;
END;

PROCEDURE TCornuObstacle.SetDefaults;
BEGIN
   A             := 0.1;
   B             := 1;
   RealObstWidth := 1.0;
   Lambda.put(1,550);
   SetUp;
END;

PROCEDURE TCornuObstacle.SetUp;
BEGIN
   IF NOT Active  THEN CloseViewPort(20);
   DefineViewPort(2,0.1,0.9,0.57,0.87);     {Diffraction window}
   DefineViewPort(4,0.0,1.0,0.50,0.94);     {Outer for 2}
   DefineViewPort(1,0.5,1.0,0.08,0.45);   {Blue message box}
   DefineViewPort(3,0,0.097,0.5,0.9);
   FirstTime := True;
   InLoop := False;
   SetUpHotKeys;
   SetUpSliders;
   GraphBackColor := Black;
   PreSpiral;
END;

PROCEDURE TCornuObstacle.SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TCornuObstacle.HandleHotKeys(KeyNum : Byte);
VAR
   VMax                                    : Real;
   FirstRun,MenuKeyUsed,DefaultKeyUsed     : Boolean;
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN24');
   2: BEGIN
         Done;
         init;
      END;
   3:  F10Used := True;
   END;
END;

PROCEDURE TCornuObstacle.SetUpSliders;
BEGIn
   WITH Sliders DO
   BEGIN
      create(1,0.01,1,A,     0.02,0.4,0.45,    2,'0.01','1','Source to Obstacle m',False);
      create(2,0.01,1,B,0.02,0.3,0.45,2,'0.01','1',' Obstacle to Screen m',False);
      create(3,0.05,3,RealObstWidth,0.02,0.2,0.45,2,'0.05','3','Obstacle Width mm',False);
      create(4,400,800,Lambda.Value(1),0.02,0.1,0.45,0,'400','800','Wavelength nm',False);
    END;
END;

PROCEDURE TCornuObstacle.CheckMouse;
BEGIN
   IF (Active AND NOT InLoop AND Sliders.Changed) THEN HandleSliders1;
END;

PROCEDURE TCornuObstacle.HandleSliders1;
VAR VMax                                 : Real;
    KeyNum                               : Byte;
    MenuKeyUsed,DefaultKeyUsed,FirstRun  : Boolean;
BEGIN
    A := Sliders.Value(1);
    B := Sliders.Value(2);
    Lambda.put(1,Sliders.Value(4));
    RealObstWidth := Sliders.Value(3);
    FirstRun := True;
    MenuKeyUsed := False;
    DefaultKeyUsed := False;
    REPEAT
       CheckForEvents;
       InLoop := True;
       ObstArc := a*arctan(0.001*RealObstWidth/A);
       VObst := 0.5*ObstArc*sqrt(2*(A+B)/
                (A*B*lambda.Value(1)*1e-9));
       TOOWIDE := (VObst > 7.5);
     IF TOOWIDE THEN
       BEGIN
          IF FirstRun THEN
          BEGIN
             SelectViewport(1);
             MakeBlueBox;
             Print(1,2,'  Inappropriate Choice of Parameters');
             Print(1,4,'   Increase Source/Obstacle Distance');
             Print(1,5,'  OR Increase Obstacle/Screen Distance');
             Print(1,6,'       OR Decrease Obstacle Width ');
             Print(1,7,'         OR Increase Wavelength ');
          END;
          FirstRun := False;
       END;
       IF Sliders.Changed THEN HandleSliders2;
       IF HotKeys.Pressed(KeyNum) THEN
       BEGIN
          IF KeyNum = 1 THEN ShowHelp('diffract.hlp','HELPSCREEN26')
          ELSE IF KeyNum = 3 THEN
          BEGIN
             DefaultKeyUsed := True;
             CloseViewPort(1);
             CornuObstacle.SetDefaults;
          END
          ELSE IF KeyNum = 4 THEN
          BEGIN
             MenuKeyUsed := True;
             TOOWIDE := False;
          END;
       END;
    UNTIL NOT TOOWIDE;
    IF MenuKeyUsed THEN F10Used := True;
    IF (NOT  F10Used AND NOT DefaultKeyUsed) THEN
    BEGIN
       CloseViewPort(1);
       ObstArc := a*arctan(0.001*RealObstWidth/A);
       VObst := 0.5*ObstArc*sqrt(2*(A+B)/
                (A*B*lambda.Value(1)*1e-9));
       VMax := VObst;
       WHILE VMax < 15 DO
       BEGIN
          VMax := VMax + VObst;
       END;

       VMaximum := VMax;
       VMaxIncrement := VMaximum/300;

       ObstWidth := round(2*VObst/VMaxIncrement);
       InLoop := False;
       PreSpiral;
    END;
END;

PROCEDURE TCornuObstacle.HandleSliders2;
BEGIN
   Delay(20);
   A := Sliders.Value(1);
   B := Sliders.Value(2);
   Lambda.put(1,Sliders.Value(4));
   RealObstWidth := Sliders.Value(3);
   ObstArc := a*arctan(0.001*RealObstWidth/A);
   VObst := 0.5*ObstArc*sqrt(2*(A+B)/
            (A*B*lambda.Value(1)*1e-9));
   TOOWIDE := (VObst > 7.5);
END;

PROCEDURE TCornuObstacle.PreSpiral;
VAR
   VMax   : Real;
BEGIN
   ObstArc := a*arctan(0.001*RealObstWidth/A);
   VObst := 0.5*ObstArc*sqrt( 2*(A+B)/(A*B*Lambda.Value(1)*1e-9) );
   VMax := VObst;
   WHILE VMax < 15 DO
   BEGIN
      Vmax := Vmax + VObst;
   END;
   VMaximum := VMax;
   VMaxIncrement  := VMaximum/300;
   ObstWidth := round(2*VObst/VMaxIncrement);
   Spiral;
END;

PROCEDURE TCornuObstacle.Spiral;
VAR
    I,K,L,Number                  : Integer;
    DeltaV,VMin                   : Real;
    V,Cs,Sn                       : DVector;
    Wait                          : TWaitMessage;
    AString                       : String;
BEGIN
   JNumber := Round(Vmaximum/VMaxIncrement) + 1 ;
   INumber := 4*(JNumber -1);
   Cs.init(INumber + 1);
   Sn.init(INumber + 1);
   X.init(2*JNumber + 1);
   Y.init(2*JNumber + 1);
   V.init(INumber + 2);
   VMin := 0;
   V.put(1,VMin);
   DeltaV := (Vmaximum - VMin)/(INumber);
   Wait.Show;
   FOR I := 1 to INumber+ 1  DO
   BEGIN
      Cs.put(I,cos(pi*V.Value(I)*V.Value(I)/2));
      Sn.put(I,sin(pi*V.Value(I)*V.Value(I)/2));
      V.put(I+1,V.Value(I) + DeltaV);
      IF I mod 50 = 0 THEN Wait.UpDate;
   END;
   K := 1;
   For L := JNumber to 2*JNumber -1   Do
   BEGIN
      X.put(L,DeltaV*NewSimpson(Cs,K));
      Y.put(L,DeltaV*NewSimpson(Sn,K));
      K := K + 4;
      IF L mod 10 = 0 THEN Wait.UpDate;
   END;
   Wait.Hide;
   FOR K := 1 to JNumber  DO
   BEGIN
      X.put(K,-X.Value(2*JNumber - K));
      Y.put(K,-Y.Value(2*JNumber - K));
   END;
   Cs.free;
   Sn.free;
   V.free;
   Obstacle;
END;

PROCEDURE TCornuObstacle.Obstacle;
VAR K,M,IMin1,IMin2,IMax1,IMax2         : Integer;
    h,Tx,Ty,Vvalue,Min1,Min2,Max1,Max2  : Real;
    IntObst,XPositionObst               : DVector;
    AString                             : String;
BEGIN
   Sliders.DrawAll;
   FirstTime := False;
   MainHotKeys.Clear;
   HotKeys.Display;
   Active := True;
   CloseViewPort(3);
   RubOutXY(0.05,0.559,75,Black);
   IntObst.init(2*JNumber - ObstWidth - 1);
   XPositionObst.init(2*JNumber - ObstWidth - 1);
   OpenViewPort(2);
   PutLabel(top,'DIFFRACTION FROM AN OBSTACLE');
   PutLabel(bottom,'Position on Axis - m');
   PutLabel(left,'Intensity');
   M := 1;
   Colours(M,Lambda);
   Vvalue:= -VMaximum;
   FOR K := 1 to 2*JNumber - ObstWidth - 1 DO
   BEGIN
      XPositionObst.put(K,(Vvalue + Obstwidth*VMaxIncrement/2)
                           *sqrt( (A+B)*B*Lambda.Value(1)*1e-9/(2*A)) );
      Tx := 0.5
            - X.Value(Obstwidth + K)
            + 0.5
            + X.Value(K);
      Ty := 0.5
            - Y.Value(ObstWidth + K)
            + 0.5
            + Y.Value(K);
      IntObst.put(K,(Tx*Tx + Ty*Ty));
      Vvalue := Vvalue + VMaxIncrement;
   END;
   h := 0.5*(A+B)*Tan(0.001*RealObstWidth/a);
   IntObst.MinMax(IMin1,Min1,IMax1,Max1);
   XPositionObst.MinMax(IMin2,Min2,IMax2,Max2);
   DefineScale(2,Min2,Max2,0,1.1*Max1);
   SelectScale(2);
   Axis(0,0,TickSpace(Max2-Min2),TickSpace(Max1-Min1));
   SetColor(Black);
   PlotLine(0,0,0,10);
   M := 1;
   Colours(M,Lambda);
   PlotDVectors(XPositionObst,IntObst,1,2*JNumber - ObstWidth - 2);
   SetColor(lightgray);
   SetLineStyle(0,0,3);
   PlotLine(-h,0.1*Max1,h,0.1*Max1);
   SetLineStyle(0,0,1);
   SetColor(White);
   IntObst.free;
   XPositionObst.free;
   X.free;
   Y.free;
END;

{************************** TCornuTwoSlit ********************}
PROCEDURE TCornuTwoSlit.Init;
BEGIN
   Lambda.init(1);
   Sliders.init;
   SetDefaults;
END;

PROCEDURE TCornuTwoSlit.Done;
BEGIN
   Lambda.free;
   Sliders.Done;
END;

PROCEDURE TCornuTwoSlit.SetDefaults;
BEGIN
   A                  := 0.1;
   B                  := 1;
   RealLeftSlitWidth  := 1.0;
   RealRightSlitWidth := RealLeftSlitWidth;
   RealSlitSeparation := RealLeftSlitWidth;
   RightSlitFactor    := 1;
   SeparationFactor   := 1;
   Lambda.put(1,550);
   SetUp;
END;

PROCEDURE TCornuTwoSlit.SetUp;
VAR AString                   : String;
BEGIN
   IF NOT Active  THEN CloseViewPort(20);
   DefineViewPort(2,0.1,0.9,0.57,0.87);     {Diffraction window}
   DefineViewPort(4,0.0,1.0,0.50,0.94);     {Outer for 2}
   DefineViewPort(1,0.52,1,0.08,0.45);   {Blue message box}
   DefineViewPort(5,0.52,1,0.20,0.40);
   DefineViewPort(3,0,0.097,0.5,0.9);
   FirstTime := True;
   InLoop := False;
   SetUpHotKeys;
   SetUpSliders;
   GraphBackColor := Black;
   Prespiral;
END;


PROCEDURE TCornuTwoSlit.SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TCornuTwoSlit.HandleHotKeys(KeyNum : Byte);
VAR VMax                                         : Real;
    FirstRun,MenuKeyUsed,DefaultKeyUsed          : Boolean;
    AString                                      : String;
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN25');
   2: BEGIN
            Done;
            init;
         END;
   3:  F10Used := True;
   END;
END;

PROCEDURE TCornuTwoSlit.SetUpSliders;
BEGIN
   WITH Sliders DO
   BEGIN
      create(1,0.1,1,A,                     0.02,0.42,0.45,    2,'0.1','1','Source to Slits m',False);
      create(2,0.001,1,B,                   0.02,0.36,0.45,3,'0.001','1',' Slits to Screen m',False);
      create(3,400,800,Lambda.Value(1),     0.02,0.30,0.45,0,'400','800','Wavelength nm',False);
      create(4,0.05,1.5,RealLeftSlitWidth,  0.02,0.24,0.45,2,'0.05','1.5',' Left Slit Width mm',False);
      create(5,1,5,SeparationFactor,        0.02,0.18,0.45,0,'1','5',' Separation Factor',False);
      create(6,1,5,RightSlitFactor,         0.02,0.12,0.45,0,'1','5',' Right Slit Factor',False);
    END;
END;

PROCEDURE TCornuTwoSlit.CheckMouse;
BEGIN
   IF (Active AND NOT InLoop AND Sliders.Changed) THEN HandleSliders1;
END;

PROCEDURE TCornuTwoSlit.HandleSliders1;
VAR VMax                                       : Real;
    KeyNum                                     : Byte;
    MenuKeyUsed,DefaultKeyUsed,FirstRun        : Boolean;
    AString                                    : String;
BEGIN
   A := Sliders.Value(1);
   B := Sliders.Value(2);
   Lambda.put(1,Sliders.Value(3));
   RealLeftSlitWidth := Sliders.Value(4);
   RealSlitSeparation := Round(Sliders.Value(5))*RealLeftSlitWidth;
   RealRightSlitWidth := Round(Sliders.Value(6))*RealLeftSlitWidth;
   FirstRun := True;
   MenuKeyUsed := False;
   DefaultKeyUsed := False;
   REPEAT
      CheckForEvents;
      InLoop := True;
      LeftSlitArc := A*arctan(0.001*(RealLeftSlitWidth)/A);
      RightSlitArc := A*arctan(0.001*(RealRightSlitWidth)/A);
      SeparationArc := A*arctan(0.001*(RealSlitSeparation)/A);
      VLeftSlit :=  0.5*LeftSlitArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
      VRightSlit :=  0.5*RightSlitArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
      VSlitSeparation :=  0.5*SeparationArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
      TOOWIDE := (VLeftSlit + VRightSlit + VSlitSeparation > 10);
      IF TOOWIDE THEN
      BEGIN
         IF FirstRun THEN
         BEGIN
            SelectViewport(1);
            MakeBlueBox;
            Beep;
            Print(1,2,'  Inappropriate Choice of Parameters');
            Print(1,4,'    Increase Source/Slit Distance');
            Print(1,5, '  OR Increase Slit/Screen Distance');
            Print(1,6,'       OR Decrease Slit Widths ');
            Print(1,7,'     OR Decrease Slit Separation');
            Print(1,8,'        OR Increase Wavelength ');
         END;
         FirstRun := False;
      END;
      IF Sliders.Changed THEN HandleSliders2;
      IF HotKeys.Pressed(KeyNum) THEN
       case KeyNum of
        1: ShowHelp('diffract.hlp','HELPSCREEN26');
        2: BEGIN
              DefaultKeyUsed := True;
              CloseViewPort(1);
              CornuTwoSlit.SetDefaults;
              TOOWIDE := False;
           END;
        3: BEGIN
              MenuKeyUsed := True;
              TOOWIDE := False;
           END;
       END;
   UNTIL NOT TOOWIDE;
   IF MenuKeyUsed THEN F10Used := True;
   IF (NOT  F10Used AND NOT DefaultKeyUsed) THEN
   BEGIN
      CloseViewPort(1);
      SelectViewPort(5);
      MakeBlueBox;
      AString := concat('    Left Slit Width = ',NumStr(RealLeftSlitWidth,1,2),' mm');
      Print(1,2,AString);
      AString := concat('    Slit Separation = ',NumStr(RealSlitSeparation,1,2),' mm');
      Print(1,3,AString);
      AString := concat('   Right Slit Width = ',NumStr(RealRightSlitWidth,1,2),' mm');
      Print(1,4,AString);
      SlitArc := a*arctan(0.001*
           (RealLeftSlitWidth+RealRightSlitWidth+RealSlitSeparation)/a);
      VTwoSlit := 0.5*SlitArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
      LeftSlitArc := A*arctan(0.001*(RealLeftSlitWidth)/A);
      RightSlitArc := A*arctan(0.001*(RealRightSlitWidth)/A);
      SeparationArc := A*arctan(0.001*(RealSlitSeparation)/A);
      VLeftSlit :=  0.5*LeftSlitArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
      VRightSlit :=  0.5*RightSlitArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
      VSlitSeparation := 0.5*SeparationArc*sqrt(2*(A+B)/(A*B*lambda.Value(1)));
      WHILE VMax < 20 DO
      BEGIN
         Vmax := Vmax + VTwoSlit;
     END;
     VMaximum := VMax;
     VMaxIncrement  := VMaximum/400;
     LeftSlitWidth := round(2*VLeftSlit/VMaxIncrement);
     RightSlitWidth := round(2*VRightSlit/VmaxIncrement);
     SlitSeparation := round(2*VSlitSeparation/VMaxIncrement);
     InLoop := False;
     PreSpiral;
  END;
END;

PROCEDURE TCornuTwoSlit.HandleSliders2;
VAR VMax                                : Real;
    KeyNum                              : Byte;
    MenuKeyUsed,DefaultKeyUsed,FirstRun : Boolean;
BEGIN
   Delay(20);
   A := Sliders.Value(1);
   B := Sliders.Value(2);
   Lambda.put(1,Sliders.Value(3));
   RealLeftSlitWidth := Sliders.Value(4);
   RealSlitSeparation := Round(Sliders.Value(5))*RealLeftSlitWidth;
   RealRightSlitWidth := Round(Sliders.Value(6))*RealLeftSlitWidth;
   VLeftSlit :=  0.5*LeftSlitArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
   VRightSlit :=  0.5*RightSlitArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
   VSlitSeparation :=  0.5*SeparationArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
   TOOWIDE := (VLeftSlit + VRightSlit + VSlitSeparation > 10);
END;

PROCEDURE TCornuTwoSlit.PreSpiral;
VAR  VMax    : Real;
     AString : String;
BEGIN
   SlitArc := a*arctan(0.001*
        (RealLeftSlitWidth+RealRightSlitWidth+RealSlitSeparation)/a);
   VTwoSlit := 0.5*SlitArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
   LeftSlitArc := A*arctan(0.001*(RealLeftSlitWidth)/A);
   RightSlitArc := A*arctan(0.001*(RealRightSlitWidth)/A);
   SeparationArc := A*arctan(0.001*(RealSlitSeparation)/A);
   VLeftSlit :=  0.5*LeftSlitArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
   VRightSlit :=  0.5*RightSlitArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
   VSlitSeparation :=  0.5*SeparationArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
   VMax := VTwoSlit;
   WHILE VMax < 20 DO
   BEGIN
      Vmax := Vmax + VTwoSlit;
   END;
   VMaximum := VMax;
   VMaxIncrement  := VMaximum/400;
   LeftSlitWidth := round(2*VLeftSlit/VMaxIncrement);
   RightSlitWidth := round(2*VRightSlit/VmaxIncrement);
   SlitSeparation := round(2*VSlitSeparation/VMaxIncrement);
   Spiral;
END;


PROCEDURE TCornuTwoSlit.Spiral;
VAR I,K,L,Number                  : Integer;
    DeltaV,VMin                   : Real;
    V,Cs,Sn                       : DVector;
    Wait                          : TWaitMessage;
    AString                       : String;
BEGIN
   JNumber := Round(Vmaximum/VMaxIncrement) + 1 ;
   INumber := 4*(JNumber -1);
   Cs.init(INumber + 1);
   Sn.init(INumber + 1);
   X.init(2*JNumber + 1);
   Y.init(2*JNumber + 1);
   V.init(INumber + 2);
   VMin := 0;
   V.put(1,VMin);
   DeltaV := (Vmaximum - VMin)/(INumber);
   Wait.Show;
   FOR I := 1 to INumber+ 1  DO
   BEGIN
      Cs.put(I,cos(pi*V.Value(I)*V.Value(I)/2));
      Sn.put(I,sin(pi*V.Value(I)*V.Value(I)/2));
      V.put(I+1,V.Value(I) + DeltaV);
      IF I mod 50 = 0 THEN Wait.UpDate;
   END;
   K := 1;
   For L := JNumber to 2*JNumber -1   Do
   BEGIN
      X.put(L,DeltaV*NewSimpson(Cs,K));
      Y.put(L,DeltaV*NewSimpson(Sn,K));
      K := K + 4;
      IF L mod 10 = 0 THEN Wait.UpDate;
   END;
   Wait.Hide;
   FOR K := 1 to JNumber  DO
   BEGIN
      X.put(K,-X.Value(2*JNumber - K));
      Y.put(K,-Y.Value(2*JNumber - K));
   END;
   Cs.free;
   Sn.free;
   V.free;
   TwoSlit;
END;

PROCEDURE TCornuTwoSlit.TwoSlit;
VAR K,M,IMin1,IMin2,IMax1,IMax2                 : Integer;
    h1,h2,h3,Vvalue,Tx,Ty,Min1,Min2,Max1,Max2   : Real;
    IntTwoSlit,XpositionTwoSlit                 : DVector;
    AString                                     : String;
BEGIN
   SelectViewPort(5);
   MakeBlueBox;
   AString := concat('      Left Slit Width = ',NumStr(RealLeftSlitWidth,1,2),' mm');
   Print(1,2,AString);
   AString := concat('      Slit Separation = ',NumStr(RealSlitSeparation,1,2),' mm');
   Print(1,3,AString);
   AString := concat('     Right Slit Width = ',NumStr(RealRightSlitWidth,1,2),' mm');
   Print(1,4,AString);
   Sliders.DrawAll;
   FirstTime := False;
   MainHotKeys.Clear;
   HotKeys.Display;
   Active := True;
   CloseViewPort(3);
   RubOutXY(0.05,0.559,75,Black);
   IntTwoSlit.init(2*JNumber - LeftSlitWidth - RightSlitWidth - SlitSeparation);
   XPositionTwoSlit.init(2*JNumber - LeftSlitWidth - RightSlitWidth - SlitSeparation);
   OpenViewPort(2);
   PutLabel(top,'DIFFRACTION FROM TWO SLITS');
   PutLabel(bottom,'Position on Axis - m');
   PutLabel(left,'Intensity');
   Vvalue:= -VMaximum;
   FOR K := 1 to 2*JNumber - LeftSlitWidth - RightSlitWidth - SlitSeparation - 1 DO
   BEGIN
      XPositionTwoSlit.put(K,(Vvalue+RightSlitwidth*VMaxIncrement + SlitSeparation*VMaxIncrement/2)
              *sqrt( (A+B)*B*Lambda.Value(1)*1e-9/(2*A)) );
      Tx := + X.Value(LeftSlitWidth + RightSlitWidth + SlitSeparation + K)
            - X.Value(RightSlitWidth + SlitSeparation + K)
            + X.Value(RightSlitWidth + K)
            - X.Value(K);
      Ty := + Y.Value(LeftSlitWidth + RightSlitWidth + SlitSeparation + K)
            - Y.Value(RightSlitWidth + SlitSeparation + K)
            + Y.Value(RightSlitWidth + K)
            - Y.Value(K);
      IntTwoSlit.put(K,Tx*Tx + Ty*Ty  );
      Vvalue := Vvalue + VMaxIncrement;
   END;
   h1 := (A+B)*Tan(0.001*(RealLeftSlitWidth+RealSlitSeparation/2)/a);
   {Left edge}
   h2 := (A+B)*Tan(0.001*(RealRightSlitWidth+RealSlitSeparation/2)/a);
   {right edge}
   h3 := 0.5*(A+B)*Tan(0.001*RealSlitSeparation/a);
   IntTwoSlit.MinMax(IMin1,Min1,IMax1,Max1);
   XPositionTwoSlit.MinMax(IMin2,Min2,IMax2,Max2);
   DefineScale(2,Min2,Max2,Min1,1.1*Max1);
   SelectScale(2);
   Axis(0,0,TickSpace(Max2-Min2),TickSpace(Max1-Min1));
   SetColor(Black);
   PlotLine(0,0,0,10);
   M := 1;
   Colours(M,Lambda);
   PlotDVectors(XPositionTwoSlit,IntTwoSlit,1,
                2*JNumber - LeftSlitWidth - RightSlitWidth - SlitSeparation - 2);
   SetColor(lightgray);
   SetLineStyle(0,0,3);
   PlotLine(Min2,0.1*Max1,-h1,0.1*Max1);
   PlotLine(-h3,0.1*Max1,h3,0.1*Max1);
   PlotLine(h2,0.1*Max1,Max2,0.1*Max1);
   SetLineStyle(0,0,1);
   SetColor(White);
   IntTwoSlit.free;
   XPositionTwoSlit.free;
   X.free;
   Y.free;
END;

{************************** TCornuTwoObstacle ********************}
PROCEDURE TCornuTwoObstacle.Init;
BEGIN
   Lambda.init(1);
   Sliders.init;
   SetDefaults;
END;

PROCEDURE TCornuTwoObstacle.Done;
BEGIN
   Lambda.free;
   Sliders.Done;
END;

PROCEDURE TCornuTwoObstacle.SetDefaults;
BEGIN
   A                      := 0.1;
   B                      := 1;
   RealLeftObstacleWidth  := 1.0;
   RealRightObstacleWidth := RealLeftObstacleWidth;
   RealObstacleSeparation := RealLeftObstacleWidth;
   RightObstacleFactor    := 1;
   SeparationFactor       := 1;
   Lambda.put(1,550);
   SetUp;
END;

PROCEDURE TCornuTwoObstacle.SetUp;
VAR AString : String;
BEGIN
   IF NOT Active  THEN CloseViewPort(20);
   DefineViewPort(2,0.1,0.9,0.57,0.87);     {Diffraction window}
   DefineViewPort(4,0.0,1.0,0.50,0.94);     {Outer for 2}
   DefineViewPort(1,0.52,1.0,0.08,0.45);   {Blue message box}
   DefineViewPort(5,0.52,1,0.20,0.40);
   DefineViewPort(3,0,0.097,0.5,0.9);
   FirstTime:= True;
   InLoop := False;
   SetUpHotKeys;
   SetUpSliders;
   GraphBackColor := Black;
   Prespiral;
END;

PROCEDURE TCornuTwoObstacle.SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TCornuTwoObstacle.HandleHotKeys(KeyNum : Byte);
VAR
   VMax                                 : Real;
   FirstRun,MenuKeyUsed,DefaultKeyUsed  : Boolean;
   AString                              : String;
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN26');
   2: BEGIN
            Done;
            init;
         END;
   3:  F10Used := True;
   END;
END;

PROCEDURE TCornuTwoObstacle.SetUpSliders;
BEGIN
   WITH Sliders DO
   BEGIN
      create(1,0.1,1,A,                        0.02,0.42,0.48,    2,'0.1','1','Source to Obstacles m',False);
      create(2,0.1,1,B,                        0.02,0.36,0.48,2,'0.1','1',' Obstacles to Screen m',False);
      create(3,400,800,Lambda.Value(1),        0.02,0.30,0.48,0,'400','800','Wavelength nm',False);
      create(4,0.03,1.5,RealLeftObstacleWidth, 0.02,0.24,0.48,2,'0.01','1.5',' Left Obstacle Width mm',False);
      create(5,1,5,SeparationFactor,           0.02,0.18,0.48,0,'1','5',' Separation Factor',False);
      create(6,1,5,RightObstacleFactor,        0.02,0.12,0.48,0,'1','5','Rt Obstacle Factor',False);
    END;
END;

PROCEDURE TCornuTwoObstacle.CheckMouse;
BEGIN
   IF (Active AND NOT InLoop AND Sliders.Changed) THEN HandleSliders1;
END;

PROCEDURE TCornuTwoObstacle.HandleSliders1;
VAR VMax                                   : Real;
    KeyNum                                 : Byte;
    MenuKeyUsed,DefaultKeyUsed,FirstRun    : Boolean;
    AString                                : String;
BEGIN
      A := Sliders.Value(1);
      B := Sliders.Value(2);
      Lambda.put(1,Sliders.Value(3));
      RealLeftObstacleWidth := Sliders.Value(4);
      RealObstacleSeparation := Round(Sliders.Value(5))*RealLeftObstacleWidth;
      RealRightObstacleWidth := Round(Sliders.Value(6))*RealLeftObstacleWidth;
      CloseViewPort(1);
      FirstRun := True;
      MenuKeyUsed := False;
      DefaultKeyUsed := False;
      REPEAT
         CheckForEvents;
         InLoop := True;
         LeftObstArc := A*arctan(0.001*(RealLeftObstacleWidth)/A);
         RightObstArc := A*arctan(0.001*(RealRightObstacleWidth)/A);
         SeparationArc := A*arctan(0.001*(RealObstacleSeparation)/A);
         VLeftObst :=  0.5*LeftObstArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
         VRightObst :=  0.5*RightObstArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
         VObstSeparation :=  0.5*SeparationArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
         TOOWIDE := (VLeftObst + VRightObst + VObstSeparation > 10);
         IF TOOWIDE THEN
         BEGIN
            IF FirstRun THEN
            BEGIN
               SelectViewport(1);
               MakeBlueBox;
               Print(1,2,' Inappropriate Choice of Parameters');
               Print(1,4,'  Increase Source/Obstacle Distance');
               Print(1,5,' OR Increase Obstacle/Screen Distance');
               Print(1,6,'     OR Decrease Obstacle Widths ');
               Print(1,7,'   OR Decrease Ostacle Separation');
               Print(1,8,'       OR Increase Wavelength ');
            END;
            FirstRun := False;
         END;
         IF Sliders.Changed THEN HandleSliders2;
         IF HotKeys.Pressed(KeyNum) THEN
         BEGIN
            IF KeyNum = 1 THEN ShowHelp('diffract.hlp','HELPSCREEN26')
            ELSE IF KeyNum = 3 THEN
            BEGIN
               DefaultKeyUsed := True;
               CloseViewPort(1);
               CornuTwoObstacle.SetDefaults;
            END
            ELSE IF KeyNum = 4 THEN
            BEGIN
               MenuKeyUsed := True;
               TOOWIDE := False;
            END;
         END;
      UNTIL NOT TOOWIDE;
      IF MenuKeyUsed THEN F10Used := True;
      IF (NOT  F10Used AND NOT DefaultKeyUsed) THEN
      BEGIN
         CloseViewPort(1);
         SelectViewPort(5);
         MakeBlueBox;
         AString := concat('    Left Obstacle Width = ',NumStr(RealLeftObstacleWidth,1,2),' mm');
         Print(1,2,AString);
         AString := concat('    Obstacle Separation = ',NumStr(RealObstacleSeparation,1,2),' mm');
         Print(1,3,AString);
         AString := concat('   Right Obstacle Width = ',NumStr(RealRightObstacleWidth,1,2),' mm');
         Print(1,4,AString);
         ObstArc := a*arctan(0.001*
              (RealLeftObstacleWidth+RealRightObstacleWidth+RealObstacleSeparation)/a);
         VTwoObst := 0.5*ObstArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
         LeftObstArc := A*arctan(0.001*(RealLeftObstacleWidth)/A);
         RightObstArc := A*arctan(0.001*(RealRightObstacleWidth)/A);
         SeparationArc := A*arctan(0.001*(RealObstacleSeparation)/A);
         VLeftObst :=  0.5*LeftObstArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
         VRightObst :=  0.5*RightObstArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
         VObstSeparation := 0.5*SeparationArc*sqrt(2*(A+B)/(A*B*lambda.Value(1)));
         WHILE VMax < 20 DO
         BEGIN
            Vmax := Vmax + VTwoObst;
        END;
        VMaximum := VMax;
        VMaxIncrement  := VMaximum/400;
        LeftObstacleWidth := round(2*VLeftObst/VMaxIncrement);
        RightObstacleWidth := round(2*VRightObst/VmaxIncrement);
        ObstSeparation := round(2*VObstSeparation/VMaxIncrement);
        InLoop := False;
        PreSpiral;
     END;
   END;

PROCEDURE TCornuTwoObstacle.HandleSliders2;
BEGIN
   Delay(20);
   A := Sliders.Value(1);
   B := Sliders.Value(2);
   Lambda.put(1,Sliders.Value(3));
   RealLeftObstacleWidth := Sliders.Value(4);
   RealObstacleSeparation := Round(Sliders.Value(5))*RealLeftObstacleWidth;
   RealRightObstacleWidth := Round(Sliders.Value(6))*RealLeftObstacleWidth;
   VLeftObst :=  0.5*LeftObstArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
   VRightObst :=  0.5*RightObstArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
   VObstSeparation :=  0.5*SeparationArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
   TOOWIDE := (VLeftObst + VRightObst + VObstSeparation > 10);
END;

PROCEDURE TCornuTwoObstacle.PreSpiral;
VAR  VMax    : Real;
     AString : String;
BEGIN
   ObstArc := a*arctan(0.001*
        (RealLeftObstacleWidth+RealRightObstacleWidth+RealObstacleSeparation)/a);
   VTwoObst := 0.5*ObstArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
   LeftObstArc := A*arctan(0.001*(RealLeftObstacleWidth)/A);
   RightObstArc := A*arctan(0.001*(RealRightObstacleWidth)/A);
   SeparationArc := A*arctan(0.001*(RealObstacleSeparation)/A);
   VLeftObst :=  0.5*LeftObstArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
   VRightObst :=  0.5*RightObstArc*sqrt( 2*(A+B)/(A*B*lambda.Value(1)*1e-9) );
   VObstSeparation := 0.5*SeparationArc*sqrt(2*(A+B)/(A*B*lambda.Value(1)*1e-9));
   VMax := VTwoObst;
   WHILE VMax < 20 DO
   BEGIN
      Vmax := Vmax + VTwoObst;
   END;
   VMaximum := VMax;
   VMaxIncrement  := VMaximum/400;
   LeftObstacleWidth := round(2*VLeftObst/VMaxIncrement);
   RightObstacleWidth := round(2*VRightObst/VmaxIncrement);
   ObstSeparation := round(2*VObstSeparation/VMaxIncrement);
   Spiral;
END;

PROCEDURE TCornuTwoObstacle.Spiral;
VAR I,K,L,Number                  : Integer;
    DeltaV,VMin                   : Real;
    V,Cs,Sn                       : DVector;
    Wait                          : TWaitMessage;
    AString                       : String;
BEGIN
   JNumber := Round(Vmaximum/VMaxIncrement) + 1 ;
   INumber := 4*(JNumber -1);
   Cs.init(INumber + 1);
   Sn.init(INumber + 1);
   X.init(2*JNumber + 1);
   Y.init(2*JNumber + 1);
   V.init(INumber + 2);
   VMin := 0;
   V.put(1,VMin);
   DeltaV := (Vmaximum - VMin)/(INumber);
   Wait.Show;
   FOR I := 1 to INumber+ 1  DO
   BEGIN
      Cs.put(I,cos(pi*V.Value(I)*V.Value(I)/2));
      Sn.put(I,sin(pi*V.Value(I)*V.Value(I)/2));
      V.put(I+1,V.Value(I) + DeltaV);
      IF I mod 50 = 0 THEN Wait.UpDate;
   END;
   K := 1;
   For L := JNumber to 2*JNumber -1   Do
   BEGIN
      X.put(L,DeltaV*NewSimpson(Cs,K));
      Y.put(L,DeltaV*NewSimpson(Sn,K));
      K := K + 4;
      IF L mod 10 = 0 THEN Wait.UpDate;
   END;
   Wait.Hide;
   FOR K := 1 to JNumber  DO
   BEGIN
      X.put(K,-X.Value(2*JNumber - K));
      Y.put(K,-Y.Value(2*JNumber - K));
   END;
   Cs.free;
   Sn.free;
   V.free;
   TwoObstacle;
END;

PROCEDURE TCornuTwoObstacle.TwoObstacle;
VAR K,M,IMin1,IMin2,IMax1,IMax2                 : Integer;
    h1,h2,h3,Vvalue,Tx,Ty,Min1,Min2,Max1,Max2   : Real;
    IntTwoObstacle,XPositionTwoObstacle         : DVector;
    AString                                     : String;
BEGIN
   SelectViewPort(5);
   MakeBlueBox;
   AString := concat('    Left Obstacle Width = ',NumStr(RealLeftObstacleWidth,1,2),' mm');
   Print(1,2,AString);
   AString := concat('    Obstacle Separation = ',NumStr(RealObstacleSeparation,1,2),' mm');
   Print(1,3,AString);
   AString := concat('   Right Obstacle Width = ',NumStr(RealRightObstacleWidth,1,2),' mm');
   Print(1,4,AString);
   FirstTime := False;
   Sliders.DrawAll;
   MainHotKeys.Clear;
   HotKeys.Display;
   Active := True;
   CloseViewPort(3);
   RubOutXY(0.05,0.559,75,Black);
   IntTwoObstacle.init(2*JNumber - LeftObstacleWidth - RightObstacleWidth - ObstSeparation);
   XPositionTwoObstacle.init(2*JNumber - LeftObstacleWidth - RightObstacleWidth - ObstSeparation);
   OpenViewPort(2);
   PutLabel(top,'DIFFRACTION FROM A DOUBLE OBSTACLE');
   PutLabel(bottom,'Position on Axis - m');
   PutLabel(left,'Intensity');
   Vvalue:= -VMaximum;
   FOR K := 1 to 2*JNumber - LeftObstacleWidth - RightObstacleWidth - ObstSeparation - 1 DO
   BEGIN
      XPositionTwoObstacle.put(K,(Vvalue+RightObstacleWidth*VMaxIncrement + ObstSeparation*VMaxIncrement/2)
              *sqrt( (A+B)*B*Lambda.Value(1)*1e-9/(2*A)) );
      Tx := 0.5
            - X.Value(LeftObstacleWidth + RightObstacleWidth + ObstSeparation + K)
            + X.Value(RightObstacleWidth + ObstSeparation + K)
            - X.Value(RightObstacleWidth + K)
            + X.Value(K)
            + 0.5;
      Ty := 0.5
            - Y.Value(LeftObstacleWidth + RightObstacleWidth + ObstSeparation + K)
            + Y.Value(RightObstacleWidth + ObstSeparation + K)
            - Y.Value(RightObstacleWidth + K)
            + Y.Value(K)
            + 0.5;
      IntTwoObstacle.put(K,Tx*Tx + Ty*Ty  );
      Vvalue := Vvalue + VMaxIncrement;
   END;
   h1 := (A+B)*Tan(0.001*(RealLeftObstacleWidth+RealObstacleSeparation/2)/a);
   h2 := (A+B)*Tan(0.001*(RealRightObstacleWidth+RealObstacleSeparation/2)/a);
   h3 := 0.5*(A+B)*Tan(0.001*RealObstacleSeparation/a);
   IntTwoObstacle.MinMax(IMin1,Min1,IMax1,Max1);
   XPositionTwoObstacle.MinMax(IMin2,Min2,IMax2,Max2);
   DefineScale(2,Min2,Max2,0,1.1*Max1);
   SelectScale(2);
   Axis(0,0,TickSpace(Max2-Min2),TickSpace(Max1-Min1));
   SetColor(black);
   PlotLine(0,0,0,10);
   M := 1;
   Colours(M,Lambda);
   PlotDVectors(XPositionTwoObstacle,IntTwoObstacle,1,
                2*JNumber - LeftObstacleWidth - RightObstacleWidth - ObstSeparation - 2);
   SetColor(lightgray);
   SetLineStyle(0,0,3);
   PlotLine(-h1,0.1*Max1,-h3,0.1*Max1);
   PlotLine(h3,0.1*Max1,h2,0.1*Max1);
   SetLineStyle(0,0,1);
   SetColor(White);
   IntTwoObstacle.free;
   XPositionTwoObstacle.free;
   X.free;
   Y.free;
END;

END.