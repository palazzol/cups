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

{* Unit for SPECTRUM.PAS ---- december 6th  1994  *}

UNIT SpecUnit;
INTERFACE
USES  graph, crt, CUPS, CUPSmupp, CUPSgui, CUPSgrph, CUPSproc,
      CUPSFunc;

FUNCTION PointInside(X,Y,VPortNumber : Integer) : Boolean;
PROCEDURE SetPoint(A : PointType; X,Y : Integer);
PROCEDURE Colours(VAR M : Integer; VAR Lambda : DVector);
PROCEDURE MakeBlueBox;
PROCEDURE Print2(X,Y,places: integer; Num : real; color,BackColor : integer);

TYPE
  TGratingSpectrum = object
        Active,F10Used                   : Boolean;
        Sliders                          : TSliders;
        HotKeys                          : THotKeys;
        PROCEDURE Init;
        PROCEDURE SetUp;
        PROCEDURE SetDefaults;
        PROCEDURE CheckMouse;
        PROCEDURE HandleHotKeys(KeyNum : Byte);
        PROCEDURE Done;
     PRIVATE
        Slitwidth,SlitSep,Max,NewA,NewB,
        AngleMax,AngleMin,NewAngleMax,NewAngleMin      :  Real;
        NumberofSlits,FirstX,FirstY                    :  Integer;
        InViewPort                                     :  Boolean;
        Lambda                                         :  DVector;
        PROCEDURE Transmission;
        PROCEDURE SetUpSliders;
        PROCEDURE HandleSliders;
        PROCEDURE SetUpHotKeys;
  END;

  TReflection = object
        Active,F10Used                   :  Boolean;
        Sliders                          :  TSliders;
        HotKeys                          :  THotKeys;
        BlazeButtons,IncidentButtons     :  TButtons;
        PROCEDURE SetUp;
        PROCEDURE Init;
        PROCEDURE SetDefaults;
        PROCEDURE CheckMouse;
        PROCEDURE HandleHotKeys(KeyNum : Byte);
        PROCEDURE Done;
    PRIVATE
        BlazeLength,BlazeAngle,IncidentAngle,NewA   :  Real;
        FirstX                                      : Integer;
        InViewPort                                  :  Boolean;
        Lambda                                      :  Dvector;
        PROCEDURE SetUpSlidersandButtons;
        PROCEDURE Blazing;
        PROCEDURE HandleSliders;
        PROCEDURE SetUpHotKeys;
  END;

   TPrismSpectrum = object
        Active,F10Used                             : Boolean;
        Sliders                                    : TSliders;
        IncidentButtons,RangeButtons,
        MeasurementButtons                         : TButtons;
        HotKeys                                    : THotKeys;
        FUNCTION  ReadData : Boolean;
        FUNCTION  ReadSource : Boolean;
        FUNCTION  ReadMaterial : Boolean;
        PROCEDURE Init;
        PROCEDURE SetDefaults;
        PROCEDURE SetUp;
        PROCEDURE HandleHotKeys(KeyNum : Byte);
        PROCEDURE CheckMouse;
        PROCEDURE Done;
    PRIVATE
        PrismAngle,Baselength,A,B,ACr,BCr,
        ABF,BBF,ADBF,BDBF,ADF,BDF,IncidentAngle,NewA,
        AngleMin,AngleMax,FirstAngleMin,FirstAngleMax        : Real;
        ZoomUsed,InViewPort,MeasureFlag                      : Boolean;
        NumberofPoints,NumberofLines,Number,
        Refracted,Reflected,MaterialNumber,SourceNumber,
        LastMaterialNumber,LastSourceNumber,FirstX           : Integer;
        AngleValues,Deviation,Deviation1,Lambda,Mu,
        Helium,Hydrogen,Mercury,Beta,Gamma,Delta             : DVector;
        Intensity                                            : DMatrix;
        UserSource,UserMaterial,DataShown                    : Boolean;
        Material,Source,RefString                            : String;
        PROCEDURE SetUpSlidersAndButtons;
        PROCEDURE Spectrum;
        PROCEDURE TableofValues;
        PROCEDURE Draw;
        PROCEDURE HandleSlider;
        PROCEDURE SetUpHotKeys;
   END;

   TPrismResolution = object
        Active,F10Used           : Boolean;
        IncidentButtons          : TButtons;
        Sliders                  : TSliders;
        HotKeys,HotKeys1         : THotKeys;
        FUNCTION  ReadData : Boolean;
        FUNCTION  ReadMaterial : Boolean;
        FUNCTION  ReadSource : Boolean;
        PROCEDURE Init;
        PROCEDURE SetDefaults;
        PROCEDURE SetUp;
        PROCEDURE HandleHotKeys(KeyNum : Byte);

        PROCEDURE CheckMouse;
        PROCEDURE Done;
    PRIVATE
        PrismAngle,Baselength,A,B,IncidentAngle,
        ResAngleMax,ResAngleMin                                    : Real;
        NumberofPoints,NumberofLines,Number,
        Refracted,Reflected,MaterialNumber,SourceNumber,
        LastMaterialNumber,LastSourceNumber                        : Integer;
        AngleValues,Deviation,Deviation1,ResLambda,Lambda,Mu,
        Beta,Gamma,Delta                                           : DVector;
        Intensity                                                  : DMatrix;
        UserSource,UserMaterial,TopSlider,BottomSlider,DataShown   : Boolean;
        ResMaterial,ResSource                                      : String;
        PROCEDURE SetUpSlidersAndButtons;
        PROCEDURE Resolution1;
        PROCEDURE Resolution2;
        PROCEDURE HandleSliders;
        PROCEDURE TableofAngles;
        PROCEDURE SetUpHotKeys;
   END;

   TMichelsonFrequency = object
      Active,F10Used                       : Boolean;
      WindowButtons,PointsButtons          : TButtons;
      WaveSliders,MirrorSliders            : TSliders;
      HotKeys                              : THotKeys;
      PROCEDURE Init;
      PROCEDURE SetDefaults;
      PROCEDURE SetUp;
      PROCEDURE HandleWaveSliders;
      PROCEDURE HandleMirrorSliders;
      PROCEDURE HandleHotKeys(KeyNum : Byte);
      PROCEDURE CheckMouse;
      PROCEDURE Done;
   PRIVATE
      Wavelength,AngleMax,AngleMin,MirrorTravel,
      PlateSeparation,Increment,NewA,BinValue,Factor3           : Real;
      NP,Number,FirstX,Factor1,Factor2                          : Integer;
      Lambda,Data                                               : DVector;
      InViewPort,WelchWindow,Problem1,Problem2,
      MainProgram,FirstTime1,FirstTime2,
      Problem1Used,Problem2Used      : Boolean;
      PROCEDURE SetUpButtons;
      PROCEDURE Frequency;
      PROCEDURE SetUpHotKeys;
   END;

{************************** END of OBJECTS **************************}

VAR
   GratingSpectrum                               : TGratingSpectrum;
   PrismSpectrum                                 : TPrismSpectrum;
   PrismResolution                               : TPrismResolution;
   MichelsonFrequency                            : TMichelsonFrequency;
   Reflection                                    : TReflection;
   MainHotKeys                                   : THotKeys;
{************************* IMPLEMENTATION ****************************}

IMPLEMENTATION

FUNCTION PointInside(X,Y,VPortNumber : Integer) : Boolean;
BEGIN
   WITH Views[VPortNumber] DO
    PointInside := ( (X>VX1) AND (X<VX2) AND (Y>VY1) AND (Y<VY2) );
END;

PROCEDURE SetPoint(A : PointType; X,Y : Integer);
BEGIN
   A.x := X;
   A.y := Y;
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


PROCEDURE MakeBlueBox;
BEGIN
   HideMouse;
   SetFillStyle(SolidFill,Blue);
   SetColor(white);
   WITH Views[ViewPortNumber] DO
      Bar3D(0,3,vx2-vx1-3,vy2-vy1,3,True);
   ShowMouse;

END;

PROCEDURE Four1(VAR Data : DVector;
                nn,isign : Integer);
VAR  II,JJ,N,Mmax,M,J,Istep,I         : Integer;
     WTemp,Wr,Wpr,Wpi,Wi,Theta        : Double;
     Tempr,Tempi,Wrs,Wis              : Real;
BEGIN
   N := 2*NN;
   J := 1;
   FOR II := 1 to NN Do
   BEGIN
      I := 2*II - 1;
      IF J > I THEN
      BEGIN
         Tempr := Data.Value(J);
         Tempi := Data.Value(J + 1);
         Data.put(J,Data.Value(I));
         Data.put(J + 1,Data.Value(I + 1));
         Data.put(I,Tempr);
         Data.put(I + 1,Tempi);
      END;
      M := n Div 2;
      WHILE (M >= 2) AND (J > M) DO
      BEGIN
         J := J - M;
         M := M Div 2;
      END;
      J := J + M;
   END;
   MMax := 2;

   WHILE N > MMax DO
   BEGIN
      Istep := 2*MMax;
      Theta := 6.28318530717959/(Isign*Mmax);
      Wpr := -2.0*sqr(sin(0.5*Theta));
      Wpi := sin(Theta);
      Wr := 1.0;
      Wi := 0.0;
      FOR II := 1 to MMax Div 2 DO
      BEGIN
         M := 2*II - 1;
         Wrs := Wr;
         Wis := Wi;
         FOR JJ := 0 to (N - M) Div IStep DO
         BEGIN
            I := M + JJ*Istep;
            J := I + MMax;
            Tempr := Wrs*Data.Value(J) - Wis*Data.Value(J + 1);
            Tempi := Wrs*Data.Value(J + 1) + Wis*Data.Value(J);
            Data.put(J,Data.Value(I) - Tempr);
            Data.put(J + 1,Data.Value(I + 1) - Tempi);
            Data.put(I,Data.Value(I) + tempr);
            Data.put(I + 1,Data.Value(I + 1) + Tempi);
         END;
         WTemp := Wr;
         Wr := Wr*Wpr - Wi*Wpi + Wr;
         Wi := Wi*Wpr + Wtemp*Wpi + Wi;
      END;
      MMax := Istep;
   END;
END;

PROCEDURE RealFT(VAR Data    : DVector;
                     N,Isign : Integer);
VAR WR,Wi,Wpr,Wpi,Wtemp,Theta   : Double;
I,I1,I2,I3,I4                   :Integer;
C1,C2,H1r,H1i,H2r,H2i,Wrs,Wis   : Real;
BEGIN
   Theta := 6.28318530717959/(2.0*N);
   C1 := 0.5;
   IF ISign = 1 THEN
   BEGIN
      C2 := -0.5;
      Four1(Data,N,1);
   END
   ELSE
   BEGIN
      C2 := 0.5;
      Theta := -Theta;
   END;
   Wpr := -2.0*sqr(sin(0.5*Theta));
   Wpi := sin(Theta);
   Wr := 1.0 + Wpr;
   Wi := Wpi;
   FOR I := 2 to N Div 2 DO
   BEGIN
      I1 := I + I - 1;
      I2 := I1 + 1;
      I3 := N + N + 3 -I2;
      I4 := I3 + 1;
      Wrs := Wr;
      Wis := Wi;
      H1r := c1*(Data.Value(I1) + Data.Value(I3));
      H1i := c1*(Data.Value(I2) - Data.Value(I4));
      H2r := -c2*(Data.Value(I2) + Data.Value(I4));
      H2i := c2*(Data.Value(I1) - Data.Value(I3));
      Data.put(I1, H1r + Wrs*H2r - Wis*H2i);
      Data.put(I2, H1i + Wrs*H2i + Wis*H2r);
      Data.put(I3, H1r - Wrs*H2r + Wis*H2i);
      Data.put(I4,-H1i + Wrs*H2i + Wis*H2r);
      Wtemp := Wr;
      Wr := Wr*Wpr - Wi*Wpi + Wr;
      Wi := Wi*Wpr + WTemp*Wpi + Wi;
   END;
   IF Isign = 1 THEN
   BEGIN
      H1r := Data.Value(1);
      Data.put(1,H1r + Data.value(2));
      Data.put(2,H1r - Data.Value(2));
   END
   ELSE
   BEGIN
      H1r := Data.Value(1);
      Data.put(1,c1*(H1r + Data.Value(2)));
      Data.put(2,c1*(H1r - Data.Value(2)));
      Four1(Data,n,-1);
   END;
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

PROCEDURE Print3(X,Y,places: integer; Num : real; color,BackColor : integer);
VAR
   Row, Col, a : integer;
   s : string[30];
BEGIN
   s := '';
   FOR a := 1 TO places + 8 DO s := s + char(219);
   Col := Round((x - 0.6) * ColWidth);
   Row := Round((y - 0.6) * RowHt);
   SetColor(BackColor);
   OutTextXY(Col, Row, s);
   SetColor(Color);
   OutTextXY(Col, Row, NumStr(num,0,places));
END;


{--------------------------- TGratingSpectrum --------------------------------------}
PROCEDURE TGratingSpectrum.Init;
BEGIN
   Lambda.init(2);
   SetDefaults;
END;

PROCEDURE TGratingSpectrum.Done;
BEGIN
   Lambda.free;
END;

PROCEDURE TGratingSpectrum.SetDefaults;
BEGIN
   Slitwidth     := 0.001;{mm}
   SlitSep       := 0.003; {mm}
   NumberofSlits := 2;
   AngleMax      := 5;
   AngleMin      := -5;
   Lambda.put(1,650);
   Lambda.put(2,500);
   FirstX        := 0;
   FirstY        := 0;
   InViewPort    := False;
END;

PROCEDURE TGratingSpectrum.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(1,0.1,0.9,0.11,0.45);
   DefineViewPort(3,0.1,0.4,0.5,0.6);
   DefineScale(1,-90,90,0,1.1);
   SetUpHotKeys;
   SetUpSliders;
   IF Active THEN Sliders.DrawAll;
   Transmission;
END;

PROCEDURE TGratingSpectrum.SetUpSliders;
BEGIN
   WITH Sliders DO
   BEGIN
      init;
      create(1,400,800,Lambda.Value(1),0.5,0.61,0.95,0,'400','800','First Wavelength nm',False);
      create(2,400,800,Lambda.Value(2),0.5,0.51,0.95,0,'400','800','Second Wavelength nm',False);
      create(3,0.001,0.01,SlitSep,0.50,0.81,0.95,4,'0.001','0.01','Slit Separation mm',False);
      create(4,0.0001,0.002,SlitWidth,0.50,0.71,0.95,4,'0.0001','0.002','Slit Width mm',False);
   END;
END;

PROCEDURE TGratingSpectrum.HandleSliders;
BEGIN
   Lambda.put(1,Sliders.Value(1));
   Lambda.put(2,Sliders.Value(2));
   SlitSep := Sliders.Value(3);
   SlitWidth := Sliders.Value(4);
   Transmission;
END;

PROCEDURE TGratingSpectrum.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TGratingSpectrum.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: ShowHelp('Spectrum.hlp','HELPSCREEN2');
   2: BEGIN
         GratingSpectrum.SetDefaults;
         IF GratingSpectrum.Active THEN GratingSpectrum.SetUp;
      END;
   3: F10Used := True;
   END;
END;



PROCEDURE TGratingSpectrum.Transmission;
VAR SinDiffangle,Term,Term1,Number,rad            : Real;
    Height,DiffAngle,NegDiffAngle,NegHeight       : DVector;
    M,N,ColNum,NumberofOrders                     : Integer;
    AString                                       : String;
BEGIN
   Sliders.DrawAll;
   MainHotKeys.Clear;
   HotKeys.Display;
   Active := True;
   rad := 180/pi;
   Active := True;
   GraphBackColor := Black;{Black;}
   DiffAngle.init(40);
   Height.init(40);
   SetColor(white);
   OpenViewPort(1);
   SelectScale(1);
   Axis(0,0,10,0.5);
   PrintXY(0.35,0.9,'TRANSMISSION GRATING ');
   PutLabel(bottom,'Angle in Degrees');
   PutLabel(left,' Intensity');
   Number := (1e3/(SlitWidth + SlitSep));
   AString := concat('Lines/m = ',NumStr(Number,8,0));
   Print(35,1,AString);
   FOR N := 1 to 2 DO
   BEGIN
      ColNum := N;
      Colours(ColNum,Lambda);
      NumberofOrders := 1;
      SinDiffAngle := NumberofOrders*(1e-6*Lambda.Value(N))/SlitSep;
      IF SinDiffAngle >0.99 THEN
         Announce
         ('        Angle of Diffraction is greater than 90 degrees        ');
      WHILE SinDiffAngle < 0.99 DO
      BEGIN
         IF SinDiffAngle = 0 THEN
            SinDiffAngle := 1e-9;
         Term := 180/pi*arcsin(SinDiffAngle);
         DiffAngle.put(NumberofOrders,rad*arcsin(SinDiffAngle));
         Term1:= pi*SlitWidth*SinDiffAngle/(1e-6*Lambda.Value(N));
         Height.put(NumberofOrders,sqr(Sin(Term1)/Term1));
         SinDiffAngle := NumberofOrders*(1e-6*Lambda.Value(N))
                          /SlitSep;
         NumberofOrders := NumberofOrders + 1;
      END;
      NegDiffAngle.init(NumberofOrders);
      NegHeight.init(NumberofOrders);
      FOR M := 2 TO NumberofOrders - 1  DO
         BEGIN
            NegDiffAngle.put(M,-DiffAngle.Value(M));
            NegHeight.put(M,Height.Value(M));
         END;
      FOR M := 1 to NumberofOrders - 1 DO
      BEGIN
         PlotLine(DiffAngle.Value(M),Height.Value(M),DiffAngle.Value(M),0);
      END;
      FOR M := 2 to NumberofOrders - 1 DO
          PlotLine(NegDiffAngle.Value(M),NegHeight.Value(M),
            NegDiffAngle.Value(M),0);
   END;
   SetColor(white);
   OpenViewPort(3);
   RubOutXY(0.3,0.535,5,Black);
   RubOutXY(0.3,0.57,5,Black);
   FOR N := 1 to 2 DO
   BEGIN
      SetColor(white);
      PrintXY(0.1,0.535,' Wavelength 1 =      nm');
      ColNum := 1;
      Colours(ColNum,Lambda);
      PrintXY(0.3,0.535,NumStr(Lambda.Value(1),4,0));
      SetColor(white);
      PrintXY(0.1,0.57,' Wavelength 2 =      nm');
      ColNum := 2;
      Colours(ColNum,Lambda);
      PrintXY(0.3,0.57,NumStr(Lambda.Value(2),4,0));
   END;
   SetColor(white);
   DiffAngle.free;
   NegDiffAngle.free;
   Height.free;
   NegHeight.free;
END;

PROCEDURE TGratingSpectrum.CheckMouse;

VAR  X,Y,button  : Integer;
     v1,v2,v3,v4 : Real;
     AString     : String;
BEGIN
   IF Active THEN
   BEGIN
      IF Sliders.Changed THEN HandleSliders;

      BEGIN
         MouseGlobalPosn(x,y,button);
         WITH Views[1] DO
         BEGIN
            IF(x>vx1) AND (x<vx2) AND(y>vy1) AND(y<vy2) THEN
            BEGIN
               SelectViewPort(1);
               v1 := (X - vx1);
               v2 := (vx2 - vx1);
               v3 := (vy2 - Y);
               v4 := (vy2 - vy1);
               NewA := -90 + 180*v1/v2;
               NewB :=  1.1*v3/v4;
               IF  (X <> FirstX) OR (Y <> FirstY)  THEN
               BEGIN
                 Print(1,1,'    Angle');
                 Print(1,2,'Intensity');
                 Print2(12,1,2,NewA,lightgreen,Black);
                 Print2(12,2,2,NewB,lightgreen,Black);
                 FirstX := X;
                 FirstY := Y;
                 InViewPort := True;
                 SetColor(white);
               END;
            END
            ELSE
            BEGIN
               IF InViewPort THEN
               BEGIN
                  SelectViewPort(1);
                  RubOut(1,1,18,Black);
                  RubOut(1,2,18,Black);
               END;
               InViewPort := False;
            END;
         END;
      END;
   END;
END;

{------------------------------ TReflection  ----------------------------}
PROCEDURE TReflection.Init;
BEGIN
   Lambda.init(1);
   SetDefaults;
END;

PROCEDURE TReflection.Done;
BEGIN
   Lambda.free;
END;

PROCEDURE TReflection.SetDefaults;
BEGIN
   Blazelength   := 4000;
   Lambda.put(1,550);
   IncidentAngle := 30;
   BlazeAngle    := 8.5;
   FirstX        := 0;
   InViewPort    := False;
END;

PROCEDURE TReflection.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(1,0.01,0.63,0.15,0.45);
   DefineViewPort(2,0.01,0.63,0.6,0.9);
   DefineViewPort(3,0.0,1,0.06,0.94);
   DefineViewPort(5,0.32,0.63,0.15,0.45);   {for the Mouse}
   DefineViewPort(6,0.32,0.63,0.6,0.9);     {for the Mouse}
   DefineScale(1,-90,90,0,1.1);
   DefineScale(2,-90,90,0,1.1);
   IF Active  THEN
   BEGIN
      Sliders.erase(3,Black);
      Sliders.erase(2,Black);
      Sliders.Done;
   END;
   SetUpSlidersAndButtons;
   SetUpHotKeys;
   Blazing;
END;

PROCEDURE TReflection.SetUpSlidersAndButtons;
BEGIN
   WITH BlazeButtons DO
   BEGIN
      init;
      Create(2,0.83,0.70,'Zoom');
      Create(1,0.70,0.70,'Normal');
      Number := 1;
   END;

   WITH IncidentButtons DO
   BEGIN
      init;
      Create(2,0.83,0.88,'Zoom');
      Create(1,0.70,0.88,'Normal');
      Number := 1;
   END;

   WITH Sliders DO
   BEGIN
      init;
      create(2,0,90,IncidentAngle,0.65,0.80,0.98,1,'0','90','Incident Angle',False);
      create(3,0,30,BlazeAngle,0.65,0.62,0.98,2,'0','30','Blaze Angle',False);
      create(4,400,800,Lambda.Value(1),0.65,0.35,0.98,0,'400','800','Wavelength nm',False);
      create(1,400,8000,BlazeLength,0.65,0.17,0.98,0,'400','8000','Step Length nm',False);
   END;
END;

PROCEDURE TReflection.HandleSliders;
VAR rad,Blaze : Real;
BEGIN
   rad := pi/180;
   Blazelength := Sliders.Value(1);
   IncidentAngle := Sliders.Value(2);
   BlazeAngle := Sliders.Value(3);
   Lambda.put(1,Sliders.Value(4));
   SelectViewPort(2);
   RubOut(22,2,4,Black);
   Blaze := 2*Blazelength*Sin(BlazeAngle*rad);
   Print2(22,2,0,Blaze,lightgreen,Black);
   SetColor(white);
   Blazing;
END;

PROCEDURE TReflection.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TReflection.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: ShowHelp('Spectrum.hlp','HELPSCREEN3');
   2: BEGIN
         Reflection.SetDefaults;
         IF Reflection.Active THEN Reflection.SetUp;
      END;
   3: F10Used := True;
   END;
END;

{Old}
PROCEDURE TReflection.Blazing;
VAR rad,SinDiffAngle,Term,Term1,LambdaB,Number,DiffAngleVal   : Real;
    Angle,Curveheight,FullHeight,Height,DiffAngle             : DVector;
    K,M,NumberofOrders,ColNum                                 : Integer;
    AString1                                                  : String;
BEGIN
   Sliders.DrawAll;
   BlazeButtons.DrawAll;
   IncidentButtons.DrawAll;
   MainHotKeys.Clear;
   HotKeys.Display;
   Active :=  True;
   GraphBackColor := Black;{Black;}
   Angle.init(80);
   CurveHeight.init(80);
   FullHeight.init(80);
   Height.init(80);
   DiffAngle.init(80);
   rad := pi/180;
   NumberofOrders := 1;
   SinDiffAngle := (NumberofOrders*Lambda.Value(1) + Blazelength*
                    Sin(incidentAngle*rad))/Blazelength;
   IF SinDiffAngle >0.99 THEN
      Announce('Angle of Diffraction is greater than 90 degrees ');
   WHILE SinDiffAngle <0.99 DO
   BEGIN
      IF SinDiffAngle = 0 THEN
      SinDiffAngle := 1e-9;
      Term := SinDiffAngle;
      Term := 180/pi*arcsin(Term);
      DiffAngle.put(NumberofOrders,Term);
      Term1 :=  pi*BlazeLength*(Sin(incidentAngle*rad)
                - SinDiffAngle + 2*Sin(BlazeAngle*rad))/Lambda.Value(1);
      Height.put(NumberofOrders,sqr(Sin(Term1)/Term1));
      FullHeight.put(NumberofOrders,1);
      NumberofOrders := NumberofOrders + 1;
      SinDiffAngle := (NumberofOrders*Lambda.Value(1) + Blazelength*Sin(incidentAngle*rad))
                       /Blazelength;
   END;

   FOR K := 1 to 80 DO
      BEGIN
         Angle.put(K,K);
         Term1 := pi*Blazelength*(Sin(incidentAngle*rad)
                  - Sin(angle.Value(K)*rad)
                  + 2*Sin(BlazeAngle*rad))/Lambda.Value(1);
         IF Term1 = 0 then Term1 := 1e-9;
         CurveHeight.put(K,sqr(Sin(Term1)/Term1));
      END;
      SelectViewPort(3);
      Comment(0.01,0.52,'  Use Mouse to measure angles in Lower Viewport');
      OpenViewPort(1);
      SelectScale(1);
      Axis(-90,0,30,1);
      PutLabel(bottom,'  Incident Angle                Diffracted Angle');
      LambdaB := 2*Blazelength*Sin(BlazeAngle*rad);
      SetColor(white);
      PlotLine(-incidentangle,1,-incidentangle,0);
      ColNum := 1;
      Colours(ColNum,Lambda);
      FOR M := 1 to (NumberofOrders - 1) DO
         PlotLine(DiffAngle.Value(M),Height.Value(M),DiffAngle.Value(M),0);
      PlotLine(-incidentangle,1,-incidentangle,0);
      OpenViewPort(2);
      SelectScale(2);
      Axis(-90,0,30,1);
      SetColor(white);
      Print(2,2,' Blaze Wavelength = ');
      SetColor(Lightgreen);
      AString1 := concat(NumStr(LambdaB,0,0));
      Print(22,2,AString1);
      Print(26,2,' nm');
      SetColor(white);
      PlotDVectors(angle,CurveHeight,1,angle.size);
      SetColor(white);
      PutLabel(bottom,'  Incident Angle                Diffracted Angle');
      Number := (1e3/(Blazelength));
      Colours(ColNum,Lambda);
      FOR M := 1 to (NumberofOrders - 1) DO
          PlotLine(DiffAngle.Value(M),FullHeight.Value(M),DiffAngle.Value(M),0);
      PlotLine(-incidentangle,1,-incidentangle,0);
      SetColor(white);
      Angle.free;
      Curveheight.free;
      FullHeight.free;
      Height.free;
      DiffAngle.free;
END;

PROCEDURE TReflection.CheckMouse;
VAR v1,v2,Angle,Max,Min              : Real;
    X,Y,Button                       : Integer;
    AString1                         : String;
BEGIN
   IF Active THEN
   BEGIN
      IF Sliders.Changed THEN HandleSliders;

      BEGIN
         MouseGlobalPosn(x,y,button);
         WITH Views[1] DO
         BEGIN
            IF(x>vx1) AND (x<vx2) AND(y>vy1) AND(y<vy2) THEN
            BEGIN
               SelectViewPort(1);
               v1 := (X - vx1);
               v2 := (vx2 - vx1);
               NewA := 180*v1/v2 - 90;
               IF  X <> FirstX   THEN
               BEGIN
                 Print(2,2,'Angle');
                 Print2(8,2,2,NewA,lightgreen,Black);
                 FirstX := X;
                 InViewPort := True;
                 SetColor(white);
               END;
            END
            ELSE
            BEGIN
               IF InViewPort THEN
               BEGIN
                  SelectViewPort(1);
                  RubOut(2,2,15,Black);
               END;
               InViewPort := False;

            END;
         END;
      END;

      IF BlazeButtons.Changed THEN
      BEGIN
         IF (BlazeButtons.Number = 1) THEN
         BEGIN
            Sliders.erase(3,black);
            Sliders.delete(3);
            Sliders.create(3,0,30,BlazeAngle,0.67,0.62,0.98,2,'0','30','Blaze Angle',False);
            Sliders.DrawAll;
         END;

         IF (BlazeButtons.number = 2) THEN
         BEGIN
            Min := trunc(BlazeAngle - 2);
              IF Min < 0 THEN MIn := 0;
            Max := Min + 4;
               IF Max > 30 THEN Max := 30;
            WITH Sliders Do
            BEGIN
               erase(3,black);
               Delete(3);
               create(3,MIn,Max,BlazeAngle,0.67,0.62,0.98,2,
               NumStr(Min,1,0),NumSTr(Max,1,0),'Blaze Angle',False);
               DrawAll;
            END;
         END;
      END;

      IF IncidentButtons.Changed THEN
      BEGIN
         IF (IncidentButtons.Number = 1) THEN
         BEGIN
            IncidentAngle := Sliders.Value(2);
            WITH Sliders DO
            BEGIN
               erase(2,black);
               Delete(2);
               create(2,0,90,IncidentAngle,0.67,0.80,0.98,1,'0','90',
               'Incident Angle',False);
               DrawAll;
            END;
         END;

         IF (IncidentButtons.number = 2) THEN
         BEGIN
            IncidentAngle := Sliders.Value(2);
            Min := trunc(IncidentAngle - 5);
              IF Min < 0 THEN Min := 0;
            Max := Min + 10;
               IF Max > 90 THEN Max := 90;
            WITH Sliders DO
            BEGIN
               erase(2,black);
               Delete(2);
               create(2,Min,Max,IncidentAngle,0.67,0.80,0.98,1,
               NumStr(Min,1,0),NumStr(Max,1,0),'Incident Angle',False);
               DrawAll;
            END;
         END;
      END;
   END;
END;


{------------------------------ TPrismSpectrum ----------------------------------}
PROCEDURE TPrismSpectrum.init;
BEGIN
   Lambda.init(8);
   Deviation.init(10);
   Beta.init(8);
   Gamma.init(8);
   Delta.init(8);
   Mu.init(8);
   NumberofPoints := 401;
   AngleValues.init(NumberofPoints);
   Intensity.init(2,NumberofPoints);
   SetDefaults;
END;

PROCEDURE TPrismSpectrum.Done;
BEGIN
   Lambda.free;
   Deviation.free;
   Beta.free;
   Gamma.free;
   Delta.free;
   Mu.free;
   AngleValues.free;
   Intensity.free;
END;

PROCEDURE TPrismSpectrum.SetDefaults;
BEGIN
   Number             := 8;
   NumberofPoints     := 401;
   Source             := 'Helium';
   SourceNumber       := 1;
   LastSourceNumber   := 1;
   Material           := 'Crown';
   MaterialNumber     := 1;
   LastMaterialNumber := 1;
   PrismAngle         := 60;
   A                  := 1.504096;
   B                  := 4526.658;
   IncidentAngle      := 60;
   Lambda.put(1,667.8);
   Lambda.put(2,587.5);
   Lambda.put(3,501.5);
   Lambda.put(4,492.2);
   Lambda.put(5,471.3);
   Lambda.put(6,447.3);
   Lambda.put(7,438.9);
   Lambda.put(8,402.7);
   FirstAngleMin      := 0;
   FirstAngleMax      := 100;
   FirstX             := 0;
   InViewPort         := False;
   DataShown          := False;
END;

PROCEDURE TPrismSpectrum.SetUp;
VAR C       : Real;
    M       : Integer;
    AString : String;
BEGIN
   IF (UserMaterial AND ReadMaterial) THEN C := C;
   IF (UserSource AND ReadSource) THEN C := C ;
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(2,0.2,0.8,0.54,0.87);    {Spectrum}
   DefineViewPort(6,0.81,0.99,0.50,0.80);
   DefineViewPort(8,0,0.17,0.55,0.68);     {Blackout}
   DefineViewPort(3,0.53,0.98,0.07,0.47);  {Prism}
   DefineViewPort(4,0,0.385,0.07,0.47);  {TableofData}
   DefineViewPort(10,0.39,0.525,0.07,0.47);     {Wavelengths}
   ZoomUsed := False;
   MeasureFlag := False;
   AngleMax := FirstAngleMax;
   AngleMin := FirstAngleMin;
   SetUpSlidersAndButtons;
   SetUpHotKeys;
   GraphBackColor := Blue;
   OpenViewPort(10);
   AString := (' Lambda');
   Print(1,1,AString);
   FOR M := 1 to Number DO
   BEGIN
      AString := concat(NumStr(Lambda.Value(M),5,3));
      Print(2,1+M,AString);
   END;
   Spectrum;
END;


PROCEDURE TPrismSpectrum.SetUpSlidersAndButtons;
BEGIN
   WITH IncidentButtons DO
   BEGIN
      init;
      Create(2,0.87,0.85,'Zoom');
      Create(1,0.87,0.90,'Normal');
      Number := 1;
   END;
   WITH RangeButtons DO
   BEGIN
      init;
      Create(2,0.02,0.60,'Zoom Range');
      Create(1,0.02,0.65,'Normal');
      Number := 1;
   END;

   WITH Sliders DO
   BEGIN
      init;
      Create(1,0,90,IncidentAngle,0.89,0.5,0.80,3,'0','90','Incident Angle',True);
   END;
END;

PROCEDURE TPrismSpectrum.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(5);
      Key[1] := 'F1-Help';
      Key[2] := 'F5-Data';
      Key[3] := 'F8-Rerun';
      Key[4] := 'F9-Defaults';
      Key[5] := 'F10-Menu';
   END;
END;

PROCEDURE TPrismSpectrum.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: ShowHelp('Spectrum.hlp','HELPSCREEN43');
   2: IF NOT DataShown THEN TableofValues;
   3: BEGIN
         IF ReadData THEN
              BEGIN
                 WITH Sliders DO
                 BEGIN
                    erase(1,black);
                    delete(1);
                    create(1,0,90,IncidentAngle,0.89,0.5,0.80,3,'0','90','Incident Angle',True);
                    DrawAll;
                 END;
                 CloseViewPort(4);
                 SetUp;
              END;
         END;
     4: BEGIN
         Sliders.erase(1,Black);
         Sliders.delete(1);
         Sliders.Create(1,0,90,IncidentAngle,0.89,0.5,0.80,3,'0','90','Incident Angle',True);
         Sliders.DrawAll;
         Done;
         init;
         IF ReadData THEN
              BEGIN
                 CloseViewPort(4);
                 SetUp;
              END;
         END;
   5: F10Used := True;
   END;
END;

FUNCTION TPrismSpectrum.ReadMaterial          : Boolean;
VAR  FirstScreen                              : TInputScreen;
     OK                                       : Boolean;
BEGIN
   WITH FirstScreen DO
   BEGIN
      init;
      DefineInputPort(0.01,0.5,0.06,0.4);
      LoadLine('         User Defined Material                  ');
      LoadLine('                                                ');
      LoadLine('   Cauchy Formula n = A + B/lambda^2            ');
      LoadLine('                                                ');
      LoadLine('         A = {       } (0 - 2)                  ');
      LoadLine('         B = {       } (0 - 20000)              ');
      LoadLine('                                                ');
      LoadLine('     [  OK  ]  [Cancel]  [ Help ]               ');
      SetNumber(1,A);
      SetNumberLimits(1,0,2);
      SetNumber(2,B);
      SetNumberLimits(2,0,20000);
      SetHelpFile('Spectrum.hlp','HELPSCREEN32');
      REPEAT
         Acceptscreen;
         A := GetNumber(1);
         B := GetNumber(2);
         OK := GetBoolean(3);
      UNTIL OK or Canceled;
      IF NOT Canceled THEN
      BEGIN
         Material := 'User Defined';
         MaterialNumber := 5;
      END
      ELSE MaterialNumber := LastMaterialNumber;
      ReadMaterial := NOT Canceled;
      Done;
   END;
END;

FUNCTION TPrismSpectrum.ReadSource : Boolean;
VAR  FirstScreen         : TInputScreen;
     M                   :Integer;
     OK : Boolean;
BEGIN
   WITH FirstScreen DO
   BEGIN
      init;
      DefineInputPort(0.01,0.5,0.06,0.66);
      LoadLine('       User Defined Source                  ');
      LoadLine('                                            ');
      LoadLine('    Number of Wavelengths = { }             ');
      LoadLine(' Limit (1 - 8)   Range (380 - 700)          ');
      LoadLine(' Line 1 Wavelength = {       }  nm          ');
      LoadLine(' Line 2 Wavelength = {       }  nm          ');
      LoadLine(' Line 3 Wavelength = {       }  nm          ');
      LoadLine(' Line 4 Wavelength = {       }  nm          ');
      LoadLine(' Line 5 Wavelength = {       }  nm          ');
      LoadLine(' Line 6 Wavelength = {       }  nm          ');
      LoadLine(' Line 7 Wavelength = {       }  nm          ');
      LoadLine(' Line 8 Wavelength = {       }  nm          ');
      LoadLine('                                            ');
      LoadLine('   [  OK  ]  [Cancel]  [ Help ]             ');
      SetNumber(1,Number);
      FOR M := 2 TO 9 DO
      BEGIN
          SetNumber(M,Lambda.Value(M-1));
          SetNumberLimits(M,380,700);
      END;
      SetHelpFile('Spectrum.hlp','HELPSCREEN33');
      REPEAT
         Acceptscreen;
         Number := trunc(GetNumber(1));
         FOR M := 1 to 8 DO
             Lambda.put(M,GetNumber(M+1));
         OK := GetBoolean(10);
      UNTIL OK or Canceled;
      IF NOT Canceled THEN
      BEGIN
         Source := 'User Defined';
         SourceNumber := 7;
      END
      ELSE SourceNumber  := LastSourceNumber;
      ReadSource := NOT Canceled;
      Done;
   END;
END;

FUNCTION TPrismSpectrum.ReadData : Boolean;
VAR FirstScreen                                   : TInputScreen;
    AMax,AMin                                     : Real;
    M                                             : Integer;
    AString                                       : String;
    AngleOK,OK,He,H,Hg,Na,Kr,Li,Mcr,Mbf,Mdbf,Mdf  : Boolean;
BEGIN
   WITH FirstScreen DO
   BEGIN
      init;
      DefineInputPort(0.01,0.6,0.06,0.63);
      LoadLine('            PRISM - SPECTRUM                  ');
      LoadLine('     Prism Angle = {     }   (  1 -  +60 )    ');
      LoadLine('Incident Angle   = {     }   (  0 -  +90 )    ');
      LoadLine('                                              ');
      LoadLine(' Cauchy Constants          Light Source       ');
      LoadLine('  #1 CROWN                   #2 HELIUM        ');
      LoadLine('  #1 BARIUM FLINT            #2 HYDROGEN      ');
      LoadLine('  #1 DENSE BARIUM FLINT      #2 MERCURY       ');
      LoadLine('  #1 DENSE FLINT             #2 SODIUM        ');
      LoadLine('  #1 Choose Material         #2 KRYPTON       ');
      Loadline('                             #2 LITHIUM       ');
      Loadline('                             #2 Choose Source ');
      LoadLine('       [  OK  ]  [Cancel]  [ Help ]           ');
      SetNumber(1,PrismAngle);
      SetNumberLimits(1,1,60);
      SetNumber(2,IncidentAngle);
      SetNumberLimits(2,0,90);
      SetRadioButton('1',MaterialNumber);
      SetRadioButton('2',SourceNumber);
      SetHelpFile('Spectrum.hlp','HELPSCREEN10');
      REPEAT
         Acceptscreen;
         PrismAngle := GetNumber(1);
         IncidentAngle := GetNumber(2);
         Mcr := GetBoolean(3);
         Mbf := GetBoolean(5);
         Mdbf := GetBoolean(7);
         Mdf := GetBoolean(9);
         UserMaterial := GetBoolean(11);
         OK := GetBoolean(15);
         He := GetBoolean(4);
         H  := GetBoolean(6);
         Hg := GetBoolean(8);
         Na := GetBoolean(10);
         Kr := GetBoolean(12);
         Li := GetBoolean(13);
         UserSource := GetBoolean(14);

         IF Hg THEN
         BEGIN
             Lambda.put(1,579.1);
             Lambda.put(2,577.0);
             Lambda.put(3,546.2);
             Lambda.put(4,491.7);
             Lambda.put(5,436.0);
             Lambda.put(6,407.9);
             Lambda.put(7,404.8);
             Lambda.put(8,0);
             Source := 'Mercury';
             Number := 7;
             SourceNumber := 3;
         END;
         IF H THEN
         BEGIN
            Lambda.put(1,656.3);
            Lambda.put(2,486.1);
            Lambda.put(3,434.0);
            Lambda.put(4,410.1);
            Lambda.put(5,397.0);
            Lambda.put(6,388.9);
            Lambda.put(7,383.5);
            Lambda.put(8,379.7);
            Source := 'Hydrogen';
            Number := 8;
            SourceNumber := 2;
         END;
         IF He THEN
         BEGIN
            Lambda.put(1,667.8);
            Lambda.put(2,587.5);
            Lambda.put(3,501.5);
            Lambda.put(4,492.2);
            Lambda.put(5,471.3);
            Lambda.put(6,447.3);
            Lambda.put(7,438.9);
            Lambda.put(8,402.7);
            Source := 'Helium';
            Number := 8;
            SourceNumber := 1;
         END;
         IF Na THEN
         BEGIN
            Lambda.put(1,615.7);
            Lambda.put(2,589.5);
            Lambda.put(3,588.9);
            Lambda.put(4,568.6);
            Lambda.put(5,498.0);
            Lambda.put(6,466.6);
            Lambda.put(7,454.3);
            Lambda.put(8,442.1);
            Source := 'Sodium';
            Number := 8;
            SourceNumber := 4;
         END;
         IF Kr THEN
         BEGIN
            Lambda.put(1,769.4);
            Lambda.put(2,768.5);
            Lambda.put(3,760.1);
            Lambda.put(4,758.7);
            Lambda.put(5,587.0);
            Lambda.put(6,557.0);
            Lambda.put(7,450.2);
            Lambda.put(8,445.3);
            Source := 'Krypton';
            Number := 8;
            SourceNumber := 5;
         END;
         IF Li THEN
         BEGIN
            Lambda.put(1,670.8);
            Lambda.put(2,610.4);
            Lambda.put(3,460.3);
            Lambda.put(4,413.3);
            Lambda.put(5,398.5);
            Lambda.put(6,391.5);
            Source := 'Lithium';
            Number := 6;
            SourceNumber := 6;
         END;
         IF Mcr THEN
         BEGIN
            ACr := 1.504096;
            BCr := 4526.658;
            Material := 'Crown';
            MaterialNumber := 1;
         END;
         IF Mbf THEN
         BEGIN
            ABF := 1.550231;
            BBF := 6209.335;
            Material := 'Barium Flint';
            MaterialNumber := 2;
         END;
         IF Mdbf THEN
         BEGIN
            ADBF := 1.599661;
            BDBF := 6251.998;
            Material := 'Dense Barium Flint';
            MaterialNumber := 3;
         END;
         IF Mdf THEN
         BEGIN
            ADF :=  1.625529;
            BDF :=  10418.74;
            Material := 'Dense Flint';
            MaterialNumber := 4;
         END;

      UNTIL OK or Canceled;
      IncidentButtons.Number := 1;
      RangeButtons.Number := 1;
      LastMaterialNumber := MaterialNumber;
      LastSourceNumber := SourceNumber;

      IF Canceled THEN
      BEGIN
          CloseViewPort(20);
          PrismSpectrum.Active := False;
          Comment(0.2,0.5,'          Make another Menu Choice         ');
          HotKeys.Clear;
      END;
      ReadData := not Canceled;
      done;
   END;
END;

PROCEDURE TPrismSpectrum.HandleSlider;
VAR Left,Right,SinBeta,CosBeta,Rad,Term,SinGamma,
    DownDevAngle,UpDevAngle,OldAngleMax,OldAngleMin,
    NewAngleMIn,NewAngleMax                          : Real;
    M                                                : Integer;
BEGIN
   DataShown := False;
   CloseViewPort(4);
   rad := pi/180;
   IncidentAngle := Sliders.Value(1);
   Reflected := 0;
   Reflected := 0;
   FOR M := 1 to Number  DO
   BEGIN
      SinBeta := Mu.Value(1);
      SinBeta := Sin(IncidentAngle* rad)/Mu.Value(M);
      CosBeta := sqrt(1-sqr(SinBeta));
      Beta.put(M,arctan(SinBeta/CosBeta));
      IF PrismAngle > Beta.Value(M)/rad THEN {defines downward deflection}
      BEGIN
         Term := Mu.Value(M)*(Sin(PrismAngle*rad)*CosBeta
                 - SinBeta*Cos(PrismAngle*rad));
         IF Term < 1 THEN
         BEGIN
            Refracted:= M;
            NumberofLines := M;
            Delta.put(M,arctan(sqrt(Term*Term/(1 - Term*Term))));
            SinGamma := Sin(Delta.Value(M))/Mu.Value(M);
            Gamma.put(M,arctan(sqrt(SinGamma*SinGamma/(1-SinGamma*SinGamma))));
            DownDevAngle := IncidentAngle + Delta.Value(M)/rad - PrismAngle;
            Deviation.put(M,DownDevAngle);
         END
         ELSE
         BEGIN
            Reflected := M ;
            Gamma.put(M,PrismAngle*rad - Beta.Value(M));
         END;
      END
      ELSE {defines upward deflection}
      BEGIN
         Refracted:= M;
         Term := Mu.Value(M)*(Sin(PrismAngle*rad)*CosBeta
                               - SinBeta*Cos(PrismAngle*rad));
         Delta.put(M,-ArcTan(sqrt(Term*Term/(1-Term*Term))));
         SinGamma := Sin(Delta.Value(M))/Mu.Value(M);
         Gamma.Put(M,-arctan(sqrt(SinGamma*SinGamma/(1-SinGamma*SinGamma))));
         UpDevAngle := IncidentAngle + Delta.Value(M)/rad - PrismAngle;
         Deviation.put(M,UpDevAngle);
      END;
   END;

   IF Refracted > 0 THEN
   BEGIN
      Left := Deviation.Value(Refracted);
      Right := Deviation.Value(1);
      OldAngleMax := AngleMax;
      OldAngleMin := AngleMin;
      IF (RangeButtons.Number = 2) AND (Reflected < 8) THEN
      BEGIN
         IF (Left > (OldAngleMax) ) THEN
         BEGIN
            REPEAT
                  AngleMax := AngleMax + 4;
            UNTIL AngleMax > Left;
            AngleMin := AngleMax - 12;
         END;

         IF (Right < (OldAngleMin) ) THEN
         BEGIN
            REPEAT
                  AngleMin := AngleMin - 4;
            UNTIL AngleMin < Right;
            AngleMax := AngleMin + 12;
         END;
      END;
   END;
   Spectrum;
END;

PROCEDURE TPrismSpectrum.Spectrum;
VAR  rad,Term,SinBeta,CosBeta,
     DownDevAngle,UpDevAngle,SinGamma :  Real;
     M                                :  Integer;
     AString                          :  String;
BEGIN
   Sliders.DrawAll;
   IncidentButtons.DrawAll;
   RangeButtons.DrawAll;
   MainHotKeys.Clear;
   HotKeys.Display;
   PrintXY(0.01,0.73,'Range of Angle');
   PrintXY(0.01,0.70,' of Deviation');
   Active := True;
   rad := pi/180;
   FOR M := 1 to Number DO
     Mu.put(M,A + B/sqr(Lambda.Value(M)));
   Refracted := 0;
   Reflected := 0;
   GraphBackColor := Black;{Black;}
   OpenViewPort(2);
   DefineScale(2,AngleMax,AngleMin,0,1.5);
   SelectScale(2);
   RubOutXY(0.1,0.53,60,black);
   IF RangeButtons.Number = 1 THEN Axis(0,0,TickSpace(AngleMax-AngleMin),1);
   IF RangeButtons.Number = 2 THEN Axis(0,0,1,1);
   PutLabel(bottom,' Angle of Deviation');
   AString := concat(' Source = ',String(Source));
   Print(1,2,AString);
   FOR M := 1 to Number  DO
   BEGIN
      SinBeta := Sin(IncidentAngle* rad)/Mu.Value(M);
      CosBeta := sqrt(1-sqr(SinBeta));
      Beta.put(M,arctan(SinBeta/CosBeta));
      IF PrismAngle > Beta.Value(M)/rad THEN {defines downward deflection}
      BEGIN
         Term := Mu.Value(M)*(Sin(PrismAngle*rad)*CosBeta
                 - SinBeta*Cos(PrismAngle*rad));
         IF Term < 1 THEN
         BEGIN
            Refracted:= M;
            NumberofLines := M;
            Delta.put(M,arctan(sqrt(Term*Term/(1 - Term*Term))));
            SinGamma := Sin(Delta.Value(M))/Mu.Value(M);
            Gamma.put(M,arctan(sqrt(SinGamma*SinGamma/(1-SinGamma*SinGamma))));
            DownDevAngle := IncidentAngle + Delta.Value(M)/rad - PrismAngle;
            Deviation.put(M,DownDevAngle);
         END
         ELSE
         BEGIN
            Reflected := M ;
            Gamma.put(M,PrismAngle*rad - Beta.Value(M));
            IF Reflected = 8 THEN
               AString := (' Internal Reflection Line')
               ELSE
               AString := (' Internal Reflection Lines ');

            Print(1,3,AString);
            Print(26  + 2*Reflected,3,NumStr(Reflected,2,0));
         END;
      END
      ELSE {defines upward deflection}
      BEGIN
         Refracted:= M;
         Term := Mu.Value(M)*(Sin(PrismAngle*rad)*CosBeta
                               - SinBeta*Cos(PrismAngle*rad));
         Delta.put(M,-ArcTan(sqrt(Term*Term/(1-Term*Term))));
         SinGamma := Sin(Delta.Value(M))/Mu.Value(M);
         Gamma.Put(M,-arctan(sqrt(SinGamma*SinGamma/(1-SinGamma*SinGamma))));
         UpDevAngle := IncidentAngle + Delta.Value(M)/rad - PrismAngle;
         Deviation.put(M,UpDevAngle);
      END;
   END;
   FOR M :=  1 To Refracted  DO
   BEGIN
      Colours(M,Lambda);
      PlotLine(Deviation.Value(M),0,Deviation.Value(M),1);
   END;
   SetColor(white);
   {IF  Refracted > 0 THEN
     Comment(0.32,0.93,'Use Mouse to measure Angles');}
   GraphBackColor := DarkGray;
   Draw;
   GraphBackColor := DarkGray;
END;

PROCEDURE TPrismSpectrum.TableofValues;
VAR  rad                                       :  Real;
     M                                         :  Integer;
     AString                                   :  String;
BEGIN
   rad := pi/180;
   GraphBackColor := Blue;
   OpenViewPort(4);
   AString := (' n       Beta    Gamma   Delta ');
   Print(1,1,AString);
   FOR M := 1 to Number DO
   BEGIN
      AString := concat(NumStr(Mu.value(M),6,4),
                         '  ',NumStr(Beta.Value(M)/rad,5,3),
                         '  ',NumStr(Gamma.Value(M)/rad,5,3)  );
      Print(1,1+M,AString);
   END;
   FOR M := 1 to Refracted Do
   BEGIN
      AString :=concat ('     ',NumStr(Delta.value(M)/rad,5,3));
      Print(20,1+M,AString);
   END;
   GraphBackColor := DarkGray;
   DataShown := True;
END;

PROCEDURE TPrismSpectrum.Draw;
VAR M,AB,AC,BC,XCenter,YCenter,XNormEnd,YNormEnd,
    Xinc,Yinc,XBottom,YBottom,XXRight,YYRight,XStart,
    YStart,XFinish,YFinish,XCorner,YCorner,XOrigin,
    YOrigin,X1,X2,X3,Y1,Y2,Y3                      : Integer;
    SlopeAB,SlopeDE,SlopeDF,SlopeDG,SlopeBC,
    SlopeAC,SlopeGH,SlopeGK,SlopeGL,SlopeCorner,
    XB,YB,XS,YS,XI,YI,XF,YF,Rad,TanIncidentAngle,
    Base,Radius,Theta,Bottom                       : Real;
    XRight,YRight                                  : DVector;
    Poly                                           : array[1..4] of PointType;
    AString                                        : String;
BEGIN
   OpenViewPort(3);
   AString := concat(' Material = ',String(Material));
   Print(1,1,AString);
   Print(1,8,' Prism');
   AString := concat(' Angle = ',NumStr(PrismAngle,0,1));
   Print(1,9,AString);
   XRight.init(8);
   YRight.init(8);
   rad := pi/180;
   WITH views[3] DO XCenter := (vx2 - vx1) div 2;
   WITH Views[3] DO YCenter := (vy2 - vy1) div 2;
   XCorner := Views[3].vx2;
   YCorner := Views[3].vy2;
   Radius := 0.9*YCenter;
   Theta := IncidentAngle - PrismAngle/2;
   XOrigin := XCenter;
   YOrigin := Round(YCenter/10);
   X1 := Round(Radius*(Sin(Theta*rad))) + XOrigin;
   y1 := Round(Radius*(1 - cos(Theta*rad))) + YOrigin;
   X3 := Round(Radius*cos((90 - PrismAngle + Theta)*rad)) + XOrigin;
   Y3 := Round(Radius*sin((90 - PrismAngle + Theta)*rad) + Radius) + YOrigin;
   X2 := -Round(Radius*cos((90 - PrismAngle - Theta)*rad)) + XOrigin;
   Y2 :=  Round(Radius*sin((90 - PrismAngle - Theta)*rad) + Radius) + YOrigin;
   Poly[1].X := X1;
   Poly[1].Y := Y1;
   Poly[2].X := X2;
   Poly[2].Y := Y2;
   Poly[3].X := X3;
   Poly[3].Y := Y3;
   Poly[4] := Poly[1];
   DrawPoly(4,Poly);
   SetFillStyle(SolidFill,lightgray);
   FillPoly(3,Poly);
   YS := 0.5*(Poly[2].Y - Poly[1].Y) + Poly[1].Y;
   YStart := Round(YS);
   XS := XCenter/10;
   XStart := Round(XS);
   YI := YS;
   YInc := Round(YI);
   Bottom := (Poly[1].X - Poly[2].X);
   IF Bottom = 0
      THEN Bottom := 1e-9;
   SlopeAB := (Poly[1].Y - Poly[2].Y) /Bottom ;
   IF SlopeAB = 0
      THEN SlopeAB := 1e-19;
   XI := (YS - Poly[1].Y + Poly[1].X*SlopeAB)/SlopeAB;
   XInc := Round(XI);
   IF IncidentAngle > 89
      THEN XInc := XCenter;
   Line(XStart,YStart,XInc,YInc);

   SlopeDE := -1/SlopeAB;  {* Normal*}

                       {* First Refracted Ray *}
   FOR M := 1 to NUMBER DO
   BEGIN
      Colours(M,Lambda);
      SlopeDG := ( SlopeDE - Sin(Beta.Value(M))/Cos(Beta.Value(M)))
                       /(1 + SlopeDE*Sin(Beta.Value(M))/cos(Beta.Value(M)) );
      Bottom := (Poly[3].X - Poly[1].X);
      IF Bottom = 0
         THEN Bottom := 1e-19;
      SlopeAC := (Poly[3].Y - Poly[1].Y) / Bottom;
      XRight.put(M, (SlopeDG*XInc - YInc
                + Poly[1].Y - SlopeAC*Poly[1].X)/(SlopeDG - SlopeAC) );
      XXRight := Round(XRight.Value(M));
      YRight.put(M,SlopeDG*(XXRight - XInc) + YInc );
      YYRight := Round(YRight.Value(M));
      Line(XInc,YInc,XXRight,YYRight);
      SetColor(white);
   END;
                     {* Second Refracted Ray *}
   IF Refracted >= 1 THEN
   FOR M := 1 to Refracted DO
   BEGIN
      Colours(M,Lambda);
      SlopeGH := -1/SlopeAC;
      SlopeGK :=  ( SlopeGH + Sin(Delta.Value(M))/Cos(Delta.Value(M)) )
                   /(1 - SlopeGH*Sin(Delta.Value(M))/Cos(Delta.Value(M)) );
      XXRight := Round(XRight.Value(M));
      YYRight := Round(YRight.Value(M));
      SlopeCorner := (YRight.Value(M) - YCorner)/(XRight.Value(M) - XCorner);
      IF SlopeGK >= SlopeCorner THEN
      BEGIN
         YF := 1.9*YCenter;
         XF := (YF - YRight.Value(M) + SlopeGK*XRight.Value(M))/SlopeGK;
         IF XF > 1.9*Xcenter THEN
         BEGIN
            XF := 1.9*Xcenter;
            YF := SlopeGK*(XF - XRight.Value(M)) + YRight.Value(M) ;
         END;
      END
      ELSE
      BEGIN
         XF := 1.9*Xcenter;
         YF := SlopeGK*(XF - XRight.Value(M)) + YRight.Value(M) ;
         IF YF > 1.9*YCenter THEN
         BEGIN
            YF := 1.9*YCenter;
            XF :=  (YF - YRight.Value(M) + SlopeGK*XRight.Value(M))/SlopeGK ;
         END;
      END;
      XFinish := Round(XF);
      YFinish := Round(YF);
      Line(XXRight,YYRight,XFinish,YFinish);
      SetColor(white);
   END;
   SetColor(white);

   IF Refracted < 8 THEN    { Internal Reflection }
   FOR M := Refracted + 1 TO Number DO
   BEGIN
      Colours(M,Lambda);
      SlopeGH := -1/SlopeAC;
      SlopeGL := ( SlopeGH - Sin(Gamma.Value(M))/Cos(Gamma.Value(M)) )
                       /(1 + SlopeGH*Sin(Gamma.Value(M))/Cos(Gamma.Value(M)) );
      XXRight := Round(XRight.Value(M));
      YYRight := Round(YRight.Value(M));
      Bottom := (Poly[3].X - Poly[2].X);
        IF Bottom = 0 THEN Bottom := 1e-9;
      SlopeBC := (Poly[3].Y - Poly[2].Y) / Bottom;
      XB :=  (SlopeGL*XRight.Value(M) - SlopeBC*Poly[2].X - YRight.Value(M) + Poly[2].Y)/(SlopeGL - SlopeBC) ;
      IF XB > Poly[2].X  THEN
         YB := SlopeBC*(XB - Poly[2].X) + Poly[2].Y
      ELSE
      BEGIN
         XB := (SlopeGL*XRight.Value(M) - SlopeAB*Poly[2].X - YRight.Value(M) + Poly[2].Y)/(SlopeGL - SlopeAB) ;
         YB := SlopeAB*(XB - Poly[2].X) + Poly[2].Y;
      END;
      XBottom := Round(XB);
      YBottom := Round(YB);
      Line(XXRight,YYRight,XBottom,YBottom);
      SetColor(white);
   END;
   XRight.free;
   YRight.free;
END;

PROCEDURE TPrismSpectrum.CheckMouse;
VAR Range,Min,Max,First,Last                    : Real;
    x,y,Button                                  : Integer;
    AString,AString1                            : String;
BEGIN
   IF Active THEN
   BEGIN
      IF Sliders.Changed THEN HandleSlider;

      IF MeasurementButtons.Changed THEN
      BEGIN
         IF MeasurementButtons.Number = 1 THEN MeasureFlag := False
         ELSE MeasureFlag := True;
      END;

      IF (Refracted > 0)   THEN
      BEGIN
         MouseGlobalPosn(x,y,button);
         WITH Views[2] DO
         BEGIN
            IF(x>vx1) AND (x<vx2) AND(y>vy1) AND(y<vy2) THEN
            BEGIN
               SelectViewPort(2);
               NewA := AngleMax - (AngleMax - AngleMin)*(X - vx1)/(vx2 - vx1);
               IF  X <> FirstX THEN
               BEGIN
                 Print(30,2,'Angle = ');
                 Print2(38,2,2,NewA,lightgreen,Black);
                 FirstX := X;
                 InViewPort := True;
                 SetColor(white);
               END;
            END
            ELSE
            BEGIN
               IF InViewPort THEN
               BEGIN
                  SelectViewPort(2);
                  RubOut(30,2,7,Black);
                  RubOut(38,2,6,Black);
               END;
               InViewPort := False;
            END;
         END;
      END;

      IF IncidentButtons.Changed THEN
      BEGIN
         IF (IncidentButtons.Number = 1) THEN
         BEGIN
            WITH Sliders DO
            BEGIN
               erase(1,black);
               delete(1);
               create(1,0,90,IncidentAngle,0.89,0.5,0.80,3,'0','90','Incident Angle',True);
               DrawAll;
            END;
         END;

         IF (IncidentButtons.number = 2) THEN
         BEGIN
            Min := trunc(IncidentAngle - 10);
              IF Min < 0 THEN Min := 0;
            Max := Min + 20;
              IF Max > 90 THEN Max := 90;
            AString := NumStr(Max,1,1);
            AString1 := NumStr(Min,1,1);
            WITH Sliders DO
            BEGIN
               erase(1,black);
               delete(1);
               create(1,Min,Max,IncidentAngle,0.89,0.5,0.80,3,AString1,AString,'Incident Angle',True);
               DrawAll;
            END;
         END;
      END;

      IF RangeButtons.Changed THEN
      BEGIN
         IF (RangeButtons.Number = 1) THEN
         BEGIN
            SelectViewPort(2);
            SelectScale(2);
            AngleMax := FirstAngleMax;
            AngleMin := FirstAngleMin;
            Spectrum;
         END
         ELSE IF ((RangeButtons.Number = 2) AND (Refracted > 0)) THEN
         BEGIN
            ZoomUsed := True;
            IF Deviation.Value(1) > Deviation.Value(Refracted) THEN
            BEGIN
               First := Deviation.Value(1);
               Last := Deviation.Value(Refracted);
            END
            ELSE
            BEGIN
               First := Deviation.Value(Refracted);
               Last := Deviation.Value(1);
            END;
            SelectViewPort(2);
            SelectScale(2);
            AngleMax := Round(First) + 2;
            AngleMin := AngleMax - 12;{Trunc(Last) - 2;}
            Spectrum;
         END;
      END
      ELSE IF ((RangeButtons.Number = 2) AND (Refracted = 0)) THEN
      BEGIN
         Announce('     No Spectral Lines present - Increase Incident Angle     ');
         Rangebuttons.Number := 1;
         RangeButtons.DrawAll;
         AngleMax := FirstAngleMax;
         AngleMin := FirstAngleMin;
         RubOutXY(0.1,0.53,60,black);
         SelectViewPort(2);
         DefineScale(2,AngleMax,AngleMin,0,1.5);
         SelectScale(2);
         Axis(0,0,TickSpace(AngleMax-AngleMin),1);
      END;
   END;
END;

{------------------------------ TPrismResolution ----------------------------------}
PROCEDURE TPrismResolution.init;
BEGIN
   ResLambda.init(2);
   Deviation1.init(2);
   Mu.init(2);
   SetDefaults;
END;

PROCEDURE TPrismResolution.Done;
BEGIN
   Reslambda.free;
   Deviation1.free;
   Mu.free;
END;

PROCEDURE TPrismResolution.SetDefaults;
BEGIN
   ResSource          := 'Sodium';
   ResMaterial        := 'Crown';
   TopSlider          := False;
   BottomSlider       := False;
   LastMaterialNumber := 1;
   LastSourceNumber   := 1;
   MaterialNumber     := 1;
   SourceNumber       := 2;
   PrismAngle         := 60;
   Baselength         := 0.05;
   IncidentAngle      := 60;
   ResLambda.put(1,589.592);
   ResLambda.put(2,588.995);
   A                  := 1.504096;
   B                  := 4526.658;
END;

PROCEDURE TPrismResolution.SetUp;
VAR C : Real;
BEGIN
   IF (UserMaterial AND ReadMaterial) THEN ;
   IF (UserSource AND ReadSource) THEN ;
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(1,0.05,0.80,0.645,0.875);
   DefineViewPort(2,0.05,0.80,0.325,0.575);
   DefineViewPort(5,0.05,0.80,0.08,0.27);
   SetUpSlidersAndButtons;
   SetUpHotKeys;
   Resolution1;
END;

PROCEDURE TPrismResolution.SetUpSlidersAndButtons;
BEGIN
   WITH IncidentButtons DO
   BEGIN
      init;
      Create(2,0.85,0.87,'Zoom');
      Create(1,0.85,0.92,'Normal');
      Number := 1;
   END;

   WITH Sliders DO
   BEGIN
      init;
      Create(1,0,90,IncidentAngle,0.89,0.47,0.82,2,'0','90','Incident Angle',True);
      Create(2,0.005,0.1,BaseLength,0.89,0.08,0.42,4,'0.005','0.1','Base Length m',True);
   END;
END;

PROCEDURE TPrismResolution.HandleSliders;
BEGIN
   IncidentAngle := Sliders.Value(1);
   Baselength := Sliders.Value(2);
   Resolution1;
END;

PROCEDURE TPrismResolution.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(4);
      Key[1] := 'F1-Help';
      Key[2] := 'F8-Rerun';
      Key[3] := 'F9-Defaults';
      Key[4] := 'F10-Menu';
   END;
END;

PROCEDURE TPrismResolution.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: ShowHelp('Spectrum.hlp','HELPSCREEN44');
   2:  BEGIN
         IF ReadData THEN
              BEGIN
                 WITH Sliders DO
                 BEGIN
                    erase(1,black);
                    delete(1);
                    create(1,0,90,IncidentAngle,0.89,0.47,0.82,2,'0','90','Incident Angle',True);
                    DrawAll;
                 END;
                 SetUp;
              END;
          END;
   3: BEGIN
         Sliders.erase(1,Black);
         Sliders.delete(1);
         Sliders.Create(1,0,90,IncidentAngle,0.89,0.47,0.82,2,'0','90','Incident Angle',True);
         Sliders.DrawAll;
         Done;
         init;
         IF ReadData THEN
              BEGIN
                 SetUp;
              END;
          END;
   4: F10Used := True;
   END;
END;

FUNCTION TPrismResolution.ReadMaterial         : Boolean;
VAR FirstScreen                                : TInputScreen;
    OK                                         : Boolean;
BEGIN
   WITH FirstScreen DO
   BEGIN
      init;
      DefineInputPort(0.25,0.75,0.06,0.35);
      LoadLine('         User Defined Material                  ');
      LoadLine('   Cauchy Formula n = A + B/lambda^2            ');
      LoadLine('         A = {       } (0 - 2)                  ');
      LoadLine('         B = {       } (0 - 20000)              ');
      LoadLine('                                                ');
      LoadLine('     [  OK  ]  [Cancel]  [ Help ]               ');
      SetNumber(1,A);
      SetNumberLimits(1,0,2);
      SetNumber(2,B);
      SetNumberLimits(2,0,20000);
      SetHelpFile('Spectrum.hlp','HELPSCREEN34');
      REPEAT
         Acceptscreen;
         A := GetNumber(1);
         B := GetNumber(2);
         OK := GetBoolean(3);
      UNTIL OK or Canceled;
      IF NOT Canceled THEN
      BEGIN
         ResMaterial := 'User Defined Material';
         MaterialNumber := 5;
      END
      ELSE MaterialNumber := LastMaterialNumber;
      ReadMaterial := NOT Canceled;
      Done;
   END;
END;

FUNCTION TPrismResolution.ReadSource : Boolean;
VAR  FirstScreen                                : TInputScreen;
     Lambda1,Lambda2                            : Real;
     LambdaOK,OK                                : Boolean;
BEGIN
   WITH FirstScreen DO
   BEGIN
      init;
      DefineInputPort(0.15,0.85,0.06,0.35);
      LoadLine('                User Defined Source               ');
      LoadLine('                                                  ');
      LoadLine('     Line Wavelength 1 = {       }  (400 - 800)   ');
      LoadLine('     Line Wavelength 2 = {       }  (400 - 800)   ');
      LoadLine('                                                  ');
      LoadLine('             [  OK  ]  [Cancel]  [ Help ]         ');
      SetNumber(1,ResLambda.Value(1));
      SetNumberLimits(1,400,800);
      SetNumber(2,Reslambda.Value(2));
      SetNumberLimits(2,400,800);
      SetHelpFile('Spectrum.hlp','HELPSCREEN35');
      REPEAT
         Acceptscreen;
         Lambda1 := GetNumber(1);
         Lambda2 := GetNumber(2);
         LambdaOK := Lambda1 > Lambda2;
         IF NOT LambdaOK THEN
         BEGIN
            ResLambda.put(2,Lambda1);
            ResLambda.put(1,Lambda2);
         END
         ELSE
         BEGIN
            ResLambda.put(1,Lambda1);
            ResLambda.put(2,Lambda2);
         END;
         OK := GetBoolean(3);
      UNTIL OK or Canceled;
      IF NOT Canceled THEN
      BEGIN
         ResSource := 'User Defined Source';
         SourceNumber := 5;
      END
      ELSE SourceNumber  := LastSourceNumber;
      ReadSource := NOT Canceled;
      Done;
   END;
END;

FUNCTION TPrismResolution.ReadData : Boolean;
VAR FirstScreen                                     : TInputScreen;
    AMax,AMIn                                       : Real;
    Na,Hg,K,Kr,Mcr,Mdf,Mdbf,Mbf,OK,AngleOk,Greater  : Boolean;
    M                                               : Integer;
BEGIN
   WITH FirstScreen DO
   BEGIN
      init;
      DefineInputPort(0.18,0.82,0.06,0.63);
      LoadLine('             PRISM - RESOLUTION                    ');
      LoadLine('       Prism Angle = {    }       ( 1  - +60 )     ');
      LoadLine('    Incident Angle = {       }    ( 0  - +90 )     ');
      LoadLine(' Prism Base Length = {       }  m ( 0.001 - 0.1)   ');
      LoadLine('                                                   ');
      LoadLine('  Prism Material              Light Source         ');
      LoadLine('     #1 CROWN                   #2 KRYPTON         ');
      LoadLine('     #1 BARIUM FLINT            #2 SODIUM          ');
      LoadLine('     #1 DENSE BARIUM FLINT      #2 MERCURY         ');
      LoadLine('     #1 DENSE FLINT             #2 POTASSIUM       ');
      LoadLine('     #1 Choose Material         #2 Choose Source   ');
      LoadLine('                                                   ');
      LoadLine('         [  OK  ]  [Cancel]  [ Help ]              ');
      SetNumber(1,PrismAngle);
      SetNumberLimits(1,1,60);
      SetNumber(2,IncidentAngle);
      SetNumberLimits(2,0,90);
      SetRadioButton('1',MaterialNumber);
      SetRadioButton('2',SourceNumber);
      SetNumber(3,Baselength);
      SetNumberLimits(3,0.001,0.10);
      SetHelpFile('Spectrum.hlp','HELPSCREEN11');
      REPEAT
         Acceptscreen;
         PrismAngle := GetNumber(1);
         IncidentAngle := GetNumber(2);
         Baselength := GetNumber(3);
         Mcr := GetBoolean(4);
         Mbf := GetBoolean(6);
         Mdbf := GetBoolean(8);
         Mdf := GetBoolean(10);
         UserMaterial := GetBoolean(12);
         Kr := GetBoolean(5);
         Na := GetBoolean(7);
         Hg := GetBoolean(9);
         K := GetBoolean(11);
         UserSource := GetBoolean(13);
         OK := GetBoolean(14);
      UNTIL OK or Canceled;
      IF Kr THEN
      BEGIN
         ResLambda.put(1,769.454);
         ResLambda.put(2,768.524);
         ResSource := 'Krypton';
         SourceNumber := 1;
      END;
      IF Na THEN
      BEGIN
          ResLambda.put(1,589.592);
          ResLambda.put(2,588.995);
          ResSource := 'Sodium';
          SourceNumber := 2;
      END;
      IF Hg THEN
      BEGIN
         ResLambda.put(1,579.066);
         ResLambda.put(2,576.960);
         ResSource := 'Mercury';
         SourceNumber := 3;
      END;
      IF K THEN
      BEGIN
         ResLambda.put(1,404.721);
         ResLambda.put(2,404.414);
         ResSource := 'Potassium';
         SourceNumber := 4;
      END;
      IF Mcr THEN
      BEGIN
         A := 1.504096;
         B := 4526.658;
         ResMaterial := 'Crown';
         MaterialNumber := 1;
      END;
      IF Mbf THEN
      BEGIN
         A := 1.550231;
         B := 6209.335;
         ResMaterial := 'Barium Flint';
         MaterialNumber := 2;
      END;
      IF Mdbf THEN
      BEGIN
         A := 1.599661;
         B := 6251.998;
         ResMaterial := 'Dense Barium Flint';
         MaterialNumber := 3;
      END;
      IF Mdf THEN
      BEGIN
         A :=  1.625529;
         B :=  10418.74;
         ResMaterial := 'Dense Flint';
         MaterialNumber := 4;
      END;
      IncidentButtons.Number := 1;
      LastMaterialNumber  := MaterialNumber;
      LastSourceNumber  := SourceNumber;
      IF Canceled THEN
      BEGIN
          CloseViewPort(20);
          PrismResolution.Active := False;
          Comment(0.2,0.5,'          Make another Menu Choice         ');
          HotKeys.Clear;
      END;
      ReadData := not Canceled;
      done;
   END;
END;

PROCEDURE TPrismResolution.Resolution1;
VAR  rad,Term,DownDevAngle,UpDevAngle,SinBeta,CosBeta,
     SinGamma,alpha,Min,Max,Width,Angle,
     AngleRange,LambdaDiff,RP1,RP2,IntVal,AngleDiff,
     Range,v1,v2,First,Last                               : Real;
     PrismWidth,ResIntensity                              : DVector;
     M,N,IMin,IMax,Shift                                  : Integer;
     AString,AString1                                     : String;
BEGIN
   Beta.init(8);
   Gamma.init(8);
   Delta.init(8);
   NumberofPoints := 401;
   AngleValues.init(NumberofPoints);
   Intensity.init(2,NumberofPoints);
   Sliders.DrawAll;
   IncidentButtons.DrawAll;
   MainHotKeys.Clear;
   HotKeys.Display;
   Active := true;
   GraphBackColor := Black;
   rad := pi/180;
   FOR M := 1 to 2 DO
     Mu.put(M,A + B/sqr(ResLambda.Value(M)));
   Reflected := 0;
   Refracted := 0;
   OpenViewPort(1);
   RubOutXY(0,0.635,70,Black);
   PutLabel(bottom,' Deviation Angle');
   FOR M := 1 to 2 DO
   BEGIN
      SinBeta := Sin(IncidentAngle * rad)/Mu.Value(M);
      CosBeta := sqrt(1-sqr(SinBeta));
      Beta.put(M,arctan(SinBeta/CosBeta)/rad);
      IF PrismAngle > Beta.Value(M) THEN {defines downward deflection}
      BEGIN
         Term := Mu.Value(M)*(Sin(PrismAngle*rad)*CosBeta
                              - SinBeta*Cos(PrismAngle*rad));
         IF Term < 1.00 THEN
         BEGIN
            Refracted:= M;
            Delta.put(M,ArcTan(sqrt(Term*Term/(1-Term*Term))));
            SinGamma := Sin(Delta.Value(M))/Mu.Value(M);
            Gamma.put(M,arctan(sqrt(SinGamma*SinGamma/(1-SinGamma*SinGamma))));
            DownDevAngle := IncidentAngle + Delta.Value(M)/rad - PrismAngle;
            Deviation1.put(M,DownDevAngle);
         END
         ELSE
         BEGIN
            Reflected := M ;
            AString :=concat
             (' Total Internal Deflection occurs Line',NumStr(M,2,0));
            Print(5,M+2,AString);
         END;
      END
      ELSE {defines upward deflection}
      BEGIN
         Refracted:= M;
         Term := Mu.Value(M)*(Sin(PrismAngle*rad)*CosBeta
                              - SinBeta*Cos(PrismAngle*rad));
         Delta.put(M,ArcTan(sqrt(Term*Term/(1-Term*Term))));
         SinGamma := Sin(Delta.Value(M))/Mu.Value(M);
         Gamma.Put(M,arctan(sqrt(SinGamma*SinGamma/(1-SinGamma*SinGamma))));
         UpDevAngle := IncidentAngle - Delta.Value(M)/rad - PrismAngle;
         Deviation1.put(M,UpDevAngle);
      END;
   END;
   PrismWidth.init(2);
   ResIntensity.init(NumberofPoints);
   FOR N := 1 to NumberofPoints DO
   BEGIN
      Intensity.put(1,N,0);
      Intensity.put(2,N,0);
   END;
   IF Refracted > 0 THEN
   BEGIN
      FOR M := 1 to Refracted DO
      BEGIN
         PrismWidth.put
           (M,1e9*BaseLength/(2*Sin(PrismAngle*rad/2))*Sin(Delta.Value(M)));
         Width := PrismWidth.Value(M);
      END;
   IF Refracted > 0 THEN
   BEGIN
     IF Deviation1.Value(1) > Deviation1.Value(Refracted) THEN
           BEGIN
              First := Deviation1.Value(1);
              Last := Deviation1.Value(Refracted);
           END
           ELSE
           BEGIN
              First := Deviation1.Value(Refracted);
              Last := Deviation1.Value(1);
           END;
           Range := 4*(First - Last);
           IF Range = 0 THEN Range := 1;
           ResAngleMax := First + Range;
           ResAngleMin := Last - Range;
           DefineScale(1,ResAngleMax,ResAngleMin,0,1.5);
           SelectScale(1);
           Axis(0,0,TickSpace(ResAngleMax-ResAngleMin),1);
      END;

      FOR M := 1 to Refracted DO   {* Main Loop *}
      BEGIN
         AngleRange := 2*(ResAngleMax - ResAngleMin);
         Angle := -0.5*AngleRange;
         FOR N := 1 to NumberofPoints DO
            BEGIN
               alpha := pi*PrismWidth.Value(M)*Sin(Angle*rad)/(ResLambda.Value(M));
               IF alpha = 0 THEN
                  alpha := 1e-9;
               Term := sqr(Sin(alpha)/alpha);
               Intensity.put(M,N,Term);
               Angle := Angle + AngleRange/(NumberofPoints -1);
               ResIntensity.put(N,Intensity.Value(M,N));
               AngleValues.put(N,Angle + Deviation1.Value(M));
            END;
         IF M = 1 THEN SetColor(yellow);
         IF M = 2 THEN SetColor(green);
         PlotDvectors(AngleValues,ResIntensity,1,AngleValues.size);
      END;
   END;
   PrismWidth.free;
   ResIntensity.free;
   Beta.free;
   Gamma.free;
   Delta.free;
   Resolution2;
END;

PROCEDURE TPrismResolution.Resolution2;
VAR  AngleRange,AngleDiff       : Real;
     TotalIntensity             : DVector;
     N,Shift                    : Integer;
     AString1 : String;
BEGIN
   TotalIntensity.init(NumberofPoints);
   SetColor(white);
   AngleRange := 2*(ResAngleMax -ResAngleMin);
   Shift := 0;
   IF Refracted = 2 THEN
   BEGIN
       AngleDiff := Deviation1.Value(2) - Deviation1.Value(1);
       Shift := (trunc( AngleDiff/AngleRange*NumberofPoints ) );
   END;
   OpenViewPort(2);
   AString1 := concat(' Prism Angle = ',NumStr(PrismAngle,4,2));
   Print(1,1,AString1);
   IF Refracted > 0 THEN
   BEGIN
   DefineScale(2,ResAngleMax,ResAngleMin,0,2.5);
   SelectScale(2);
   RubOutXY(0,0.315,70,Black);
   Axis(0,0,TickSpace(ResAngleMax-ResAngleMin),1);
   FOR N := 1 to (NumberofPoints - Shift ) DO
      TotalIntensity.put(N,Intensity.Value(1,N + Shift)
                         + Intensity.Value(2,N) );
   PlotDVectors(AngleValues,TotalIntensity,1,NumberofPoints - Shift);
   END
   ELSE
   Announce('      No Spectral Lines present - increase Incident Angle     ');
   TableofAngles;
   TotalIntensity.free;
   AngleValues.free;
   Intensity.free;
END;


PROCEDURE TPrismResolution.TableofAngles;
VAR  AString : String;
BEGIN
   GraphBackColor := Blue;
   OpenViewPort(5);
   Print(1,1,' Deviation Angles: ');
   IF Refracted = 2 THEN
     BEGIN
        AString := concat(NumStr(Deviation1.Value(2),7,5),
                           ' deg  ',NumStr(Deviation1.Value(1),7,5),' deg');
        Print(20,1,AString);
     END
     ELSE
     IF Refracted = 1 THEN
     BEGIN
        AString := concat(' Longer Wavelength = ',NumStr(Deviation1.Value(1),7,5),' deg');
        Print(20,1,AString);
     END
     ELSE
     BEGIN
        Print(20,1,'Internal Reflection for both Wavelengths ');
     END;
     AString := (' Wavelengths:');
     Print(1,2,AString);
     AString := concat(NumStr(ResLambda.Value(1),7,3),' nm   ',NumStr(ResLambda.Value(2),7,3),' nm');
     Print(21,2,AString);
     AString := concat(' Material: ',Resmaterial);
     Print(1,3,AString);
     AString := concat('Source: ',ResSource);
     Print(33,3,AString);
     AString := (' Refractive Indices ');
     Print(1,4,AString);
     AString := concat('    n 1 = ',NumStr(Mu.Value(1),7,5),'         n 2 = ',NUmStr(Mu.Value(2),7,5));
     Print(20,4,AString);
     GraphBackColor := DarkGray;
     DataShown := True;
END;

PROCEDURE TPrismResolution.CheckMouse;
VAR Range,v1,v2,Max,Min,First,Last     :      Real;
BEGIN
   IF Active THEN
   BEGIN
      IF Sliders.Changed THEN HandleSliders;

      IF IncidentButtons.Changed THEN
      BEGIN
         IF (IncidentButtons.Number = 1) THEN
         BEGIN
            WITH Sliders DO
            BEGIN
               erase(1,black);
               delete(1);
               create(1,0,90,IncidentAngle,0.89,0.47,0.82,2,'0','90','Incident Angle',True);
               DrawAll;
            END;
         END;

         IF (IncidentButtons.number = 2) THEN
         BEGIN
            Min := trunc(IncidentAngle - 5);
              IF Min < 0 THEN Min := 0;
            Max := Min + 10;
              IF Max > 90 THEN Max := 90;
            IncidentAngle := Sliders.Value(1);
            WITH Sliders DO
            BEGIN
               erase(1,black);
               delete(1);
               create(1,Min,Max,IncidentAngle,0.89,0.47,0.82,2,NumStr(Min,1,1),
                       NumStr(Max,1,1),'Incident Angle',True);
               DrawAll;
            END;
         END;
      END;
   END;
END;

{*********************** TMichelsonFrequency ***********************}
PROCEDURE TMichelsonFrequency.Init;
BEGIN
   DefineViewPort(20,0,1,0.05,0.94);
   Lambda.init(2);
   Data.init(2000);
   WindowButtons.init;
   PointsButtons.init;
   WaveSliders.init;
   MirrorSliders.init;
   SetDefaults;
END;

PROCEDURE TMichelsonFrequency.Done;
BEGIN
   Lambda.free;
   Data.free;
END;

PROCEDURE TMichelsonFrequency.SetDefaults;
BEGIN
   Problem1        := False;
   Problem2        := False;
   MainProgram     := True;
   FirstTime1      := True;
   FirstTime2      := True;
   Problem1Used    := False;
   Problem2Used    := False;
   Factor1         := 1;
   Factor2         := 3;
   Factor3         := 0.5;
   NP              := 256;
   AngleMin        := -5;
   AngleMax        := 5;
   PlateSeparation := 0;
   Lambda.put(1,1200);
   Lambda.put(2,2400);
   MirrorTravel    := 3.6e-5;
   WelchWindow     := False;
   FirstX          := 0;
   InViewPort      := False;
END;

PROCEDURE TMichelsonFrequency.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   WelchWindow := False;
   DefineViewPort(2,0.1,0.95,0.40,0.60);
   DefineViewPort(7,0.1,0.95,0.12,0.32);
   DefineViewPort(3,0,1,0.8,0.95);
   DefineViewPort(8,0.05,0.1,0.12,0.35);
   DefineViewPort(9,0.05,0.1,0.395,0.61);
   DefineScale(1,AngleMin,AngleMax,0,2.2);
   SetUpButtons;
   SetUpHotKeys;
   Active := true;
   GraphBackColor := Black;
   Frequency;
END;


PROCEDURE TMichelsonFrequency.SetUpButtons;
BEGIN
   WITH WindowButtons DO
   BEGIN
      Create(1,0.75,0.57,'ON');
      Create(2,0.85,0.57,'OFF');
      Number := 2;
   END;
   WITH PointsButtons DO
   BEGIN
      create(1,0.4,0.75,'64');
      create(2,0.5,0.75,'128');
      create(3,0.6,0.75,'256');
      create(4,0.7,0.75,'512');
      create(5,0.8,0.75,'1024');
      Number := 3
   END;
    WITH WaveSliders DO
    BEGIN
       Create(1,400,4000,Lambda.Value(1),0.05,0.9,0.95,0,'400','4000','Wavelength 1',False);
       Create(2,400,4000,Lambda.Value(2),0.05,0.825,0.95,0,'400','4000','Wavelength 2',False);
    END;

    WITH MirrorSliders DO
    BEGIN
       Create(1,0.004,0.3,1000*MirrorTravel,0.05,0.65,0.95,5,'0.004','0.3','Mirror Travel mm',False);
    END;
END;

PROCEDURE TMichelsonFrequency.HandleWaveSliders;
BEGIN
    Lambda.put(1,WaveSliders.Value(1));
    Lambda.put(2,WaveSliders.Value(2));
    Frequency;
END;

PROCEDURE TMichelsonFrequency.HandleMirrorSliders;
BEGIN
    MirrorTravel := MirrorSliders.Value(1)/1000;
    Frequency;
END;


PROCEDURE TMichelsonFrequency.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(5);
      Key[1] := 'F1-Help';
      Key[2] := 'F2-Problem d';
      Key[3] := 'F3-Problem e';
      Key[4] := 'F9-Defaults';
      Key[5] := 'F10-Menu';
   END;
END;

PROCEDURE TMichelsonFrequency.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: BEGIN
         IF MainProgram THEN ShowHelp('Spectrum.hlp','HELPSCREEN42');
         IF NOT MainProgram And NOT Problem2 AND Problem1 THEN
                ShowHelp('Spectrum.hlp','HELPSCREEN52');
         IF NOT MainProgram AND NOT Problem1 AND Problem2 THEN
                ShowHelp('Spectrum.hlp','HELPSCREEN53');
      END;
   2: BEGIN
         IF NOT Problem1Used THEN
         BEGIN
            MainProgram := False;
            Problem1 := True;
            Problem2 := False;
            Problem1Used := True;
            Problem2Used := False;
            NP := 256;
            Frequency;
         END;
      END;
   3: BEGIN
         IF NOT Problem2Used THEN
         BEGIN
            MainProgram := False;
            Problem1 := False;
            Problem2 := true;
            Problem1Used := False;
            Problem2Used := True;
            NP := 256;
            Frequency;
         END;
      END;
   4: BEGIN
         Done;
         init;
         SetUp;
      END;

   5: F10Used := True;
   END;
END;

PROCEDURE TMichelsonFrequency.CheckMouse;
VAR  M,x,y,Button              : Integer;
BEGIN
   IF Active THEN
   BEGIN
      IF MirrorSliders.Changed THEN HandleMirrorSliders;
      IF MainProgram AND WaveSliders.Changed THEN HandleWaveSliders;
      IF PointsButtons.Changed THEN
      BEGIN
         IF PointsButtons.Number = 1 THEN
         NP := 64;
         IF PointsButtons.Number = 2 THEN
         NP := 128;
         IF PointsButtons.Number = 3 THEN
         NP := 256;
         IF PointsButtons.Number = 4 THEN
         NP := 512;
         IF PointsButtons.Number = 5 THEN
         NP := 1024;
         Frequency;
      END;
      IF WindowButtons.Changed THEN
      BEGIN
         IF WindowButtons.Number = 1 THEN
         BEGIN
            WelchWindow := true;
            Frequency;
         END
         ELSE
         BEGIN
            WelchWindow := False;
            Frequency;
         END;
      END;

      BEGIN
         MouseGlobalPosn(x,y,button);
         If ViewPortNumber = 7 THEN WITH Views[7] DO
         BEGIN
            IF(x>vx1) AND (x<vx2) AND(y>vy1) AND(y<vy2) THEN
            BEGIN
               SelectViewPort(7);
               BinValue := 0.5*NP*(X - vx1)/(vx2 - vx1);
               NewA := 2*1e9*MirrorTravel/BinValue;
               IF X <> FirstX THEN
               BEGIN
                  Print(2,1,'Wavelength nm');
                  Print3(16,1,1,NewA,lightgreen,Black);
                  FirstX := X;
                  InViewPort := True;
                  SetColor(white);
               END;
            END
            ELSE
            BEGIN
               IF InViewPort THEN
               BEGIN
                  SelectViewPort(7);
                  RubOut(2,1,23,Black);
               END;
               InViewPort := False;
            END;
         END;
      END;
   END;
END;


PROCEDURE TMichelsonFrequency.Frequency;
CONST
    eps = 1.0e-3;
    width = 50;
VAR Intensity1,Intensity2,
    Delta1,Delta2,Separation,Theta,Size                        : DVector;
    AngleRange,rad,Delta,B,A,big,scal,
    small,XValue,Min,Max,
    NewIntensity1,NewIntensity2,NewIntensity3,NewIntensity4,
    NewIntensity5,NewIntensity6,NewIntensity7,NewIntensity8    : Real;
    I,M,N,Nlim,IMin,IMax                                       : Integer;
    AString                                                    : String;
BEGIN
   PointsButtons.DrawAll;
   SetColor(lightgreen);
   PrintXY(0.05,0.76,'Number of Transform Points');
   Setcolor(white);
   CloseViewPort(3);
   IF MainProgram THEN
   BEGIN
      WaveSliders.DrawAll;
      MirrorSliders.DrawAll;
   END;
   MainHotKeys.Clear;
   HotKeys.Display;
   rad := pi/180;
   Number := NP;
   Intensity1.init(Number+1);
   Intensity2.init(Number+1);
   Theta.init(Number+2);
   Delta1.init(Number+1);
   Delta2.init(Number+1);
   Separation.init(NP+2);
   AngleRange := AngleMax - AngleMin;
   Separation.put(1,0);
   IF MainProgram THEN
   BEGIN
      FOR M := 1 to NP + 1 DO
      BEGIN
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /Lambda.Value(1);
         NewIntensity1 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /Lambda.Value(2);
         NewIntensity2 := sqr(cos(Delta/2));
         Data.put(M,NewIntensity1 + NewIntensity2);
         Separation.put(M+1,Separation.Value(M) + MirrorTravel/NP);
      END;
   END;

   IF Problem1 THEN
   BEGIN
      IF FirstTime1 THEN
      BEGIN
         PointsButtons.Number := 3;
         CloseViewPort(20);
         ShowHelp('spectrum.hlp','HELPSCREEN50');
         MirrorTravel := 3.6e-5;
         WindowButtons.Number := 2;
         WindowButtons.DrawAll;
         WelchWindow := False;
         FirstTime1 := False;
         FirstTime2 := True;
      END;
      PrintXY(0.24,0.9,'You are studying Problem 6.7.7 (d)');
      FOR M := 1 to NP + 1 DO
      BEGIN
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /667.8;
         NewIntensity1 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /587.5;
         NewIntensity2 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /501.5;
         NewIntensity3 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /492.2;
         NewIntensity4 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /471.3;
         NewIntensity5 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /447.3;
         NewIntensity6 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /438.9;
         NewIntensity7 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /402.7;
         NewIntensity8 := sqr(cos(Delta/2));
         Data.put(M,NewIntensity1 + NewIntensity2 +
                    NewIntensity3 + NewIntensity4 +
                    NewIntensity5 + NewIntensity6 +
                    NewIntensity7 + NewIntensity8);
         Separation.put(M+1,Separation.Value(M) + MirrorTravel/NP);
      END;
      Factor1 := 4;
      Factor2 := 8;
      Factor3 := 0.1;
      SetColor(lightgreen);
      PrintXY(0.05,0.76,'Number of Transform Points');
      Setcolor(white);
      PointsButtons.DrawAll;
      MirrorSliders.reset(1,Mirrortravel);
      MirrorSliders.DrawAll;
   END;

   IF Problem2 THEN
   BEGIN
      IF FirstTime2 THEN
      BEGIN
         PointsButtons.Number := 3;
         CloseViewPort(20);
         ShowHelp('spectrum.hlp','HELPSCREEN51');
         MirrorTravel := 3.6e-5;
         WindowButtons.Number := 2;
         WindowButtons.DrawAll;
         WelchWindow := False;
         FirstTime2 := False;
         FirstTime1 := True;
      END;
      PrintXY(0.24,0.9,'You are studying Problem 6.7.7 (e)');
      FOR M := 1 to NP + 1 DO
      BEGIN
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /1875.6;
         NewIntensity1 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /1282.2;
         NewIntensity2 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /1094.1;
         NewIntensity3 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /1005.2;
         NewIntensity4 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /954.9;
         NewIntensity5 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /923.3;
         NewIntensity6 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /901.7;
         NewIntensity7 := sqr(cos(Delta/2));
         Delta := pi + 2*pi*2*Separation.Value(M)*1e9*cos(rad*Theta.Value(M))
                  /886.5;
         NewIntensity8 := sqr(cos(Delta/2));
         Data.put(M,NewIntensity1 + NewIntensity2 +
                    NewIntensity3 + NewIntensity4 +
                    NewIntensity5 + NewIntensity6 +
                    NewIntensity7 + NewIntensity8);
         Separation.put(M+1,Separation.Value(M) + MirrorTravel/NP);

      END;
      Factor1 := 4;
      Factor2 := 8;
      Factor3 := 0.1;
      SetColor(lightgreen);
      PrintXY(0.05,0.76,'Number of Transform Points');
      Setcolor(white);
      PointsButtons.DrawAll;
      MirrorSliders.reset(1,Mirrortravel);
      MirrorSliders.DrawAll;
   END;

   CloseViewPort(9);
   OpenViewPort(2);
   DefineScale(2,0,MirrorTravel,0,Factor2);  {change}
   SelectScale(2);
   RubOutXY(0.05,0.39,80,Black);
{   Axis(0,0,MirrorTravel/5,1);}
   Axis(0,0,0,1);
   SetColor(white);
   PutLabel(left,'Intensity');
   PutLabel(bottom,'Mirror Travel m');
   WindowButtons.DrawAll;
   PrintXY(0.58,0.58,'Welch Window');

   IF WelchWindow THEN
   BEGIN
      FOR  M := 1 to NP + 1 DO
      BEGIN
         B := 1 - sqr( (M - 0.5*(NP))/(0.5*(NP + 2) ));
         Data.put(M,Data.Value(M) - Factor1);      {change}
         Data.put(M,Data.Value(M)*B);
         Data.put(M,Data.Value(M) + Factor1);      {change}
      END;
      PlotDVectors(Separation,Data,1,NP+1);
   END
   ELSE PlotDVectors(Separation,Data,1,NP+1);
   DefineScale(7,0,NP/2,0,Factor3);   {change}
   Size.init(2*NP);
   N := np div 2;
   RealFT(Data,N,+1);
   size.put(1,Data.Value(1));
   big := size.Value(1);
   FOR I := 2 to N DO
   BEGIN
      size.put(I,sqrt( sqr(Data.Value(2*I-1)) + sqr(Data.Value(2*I))  ) );
      IF Size.Value(I) > big THEN
         big := Size.Value(I);
   END;
   SetColor(white);
   SelectViewPort(20);
   AString := ('          Number of Waves in Full Mirror Travel Distance');
   PrintXY(0.1,0.08,AString);
   CloseViewPort(8);
   SelectScale(7);
   OpenViewPort(7);
   RubOutXY(0.05,0.11,80,Black);
   PutLabel(left,'Intensity');
{   Axis(0,0,NP/16,0.1);}
   Axis(0,0,0,0.1);
   Size.MinMax(IMin,MIn,Imax,Max);
   FOR M := 1 to N DO
     Size.put(M,Size.Value(M)/Max);
   SetColor(lightred);
   FOR M := 1 to N DO
     PlotLine(M-1,Size.Value(M),M-1,0);
   SetColor(white);
   Size.free;
   Intensity1.free;
   Intensity2.free;
   Delta1.free;
   Delta2.free;
   Theta.free;
   Separation.free;
END;


END.
