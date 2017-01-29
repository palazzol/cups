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

{  ***********************************************************  }
{  *                    IBM PC Version                       *  }
{  *                                                         *  }
{  *          A Module in the Waves and Optics Section       *  }
{  *                   of the CUPS Project                   *  }
{  *     December 6th 1994   SPECTRUM.PAS                    *  }
{  *                         AUTHOR                          *  }
{  *                     Robin A. Giles                      *  }
{  *            Department of Physics and Astronomy          *  }
{  *                   Brandon University                    *  }
{  *                    Manitoba, Canada                     *  }
{  *                        R7A 6A9                          *  }
{  *                E-Mail giles@BrandonU.CA                 *  }
{  *                   Tel: 204 727 7402                     *  }
{  *                                                         *  }
{  *           This module contains the following            *  }
{  *                                                         *  }
{  *                    Gratings                             *  }
{  *            a) Transmission Grating                      *  }
{  *            b) Resolution of Transmission Grating        *  }
{  *            c) Blazed Refection Grating                  *  }
{  *                                                         *  }
{  *                     Prism                               *  }
{  *            a) Spectrum                                  *  }
{  *            b) Resolution                                *  }
{  *                                                         *  }
{  *                   Fabry-Perot Etalon                    *  }
{  *            a) Spectrum and Resolution                   *  }
{  *                                                         *  }
{  *                 Michelson Interferometer                *  }
{  *            a)  Fringe Pattern                           *  }
{  *            b)  Fourier Transform Spectroscopy           *  }
{  *            c)  Refractive Index of a Gas                *  }
{  *            d)  Refractive Index of a Solid              *  }
{  ***********************************************************  }




PROGRAM DIFFRACTION2;

USES graph, crt, CUPS, CUPSmupp, CUPSgui, CUPSgrph, CUPSproc,
     CUPSFunc, SpecUnit;

TYPE
  TGratingResolution = object
        Active             : Boolean;
        PROCEDURE Init;
        PROCEDURE SetUp;
        PROCEDURE SetDefaults;
        PROCEDURE CheckMouse;
        PROCEDURE Done;
     PRIVATE
        ResSlitWidth,ResSlitSep,Max,NewA,
        AngleMax,AngleMin,NewAngleMax,NewAngleMin      :  Real;
        NumberofSlits,FirstY                           :  Integer;
        InViewPort                                     :  Boolean;
        ResLambda                                      :  DVector;
        Slider1,Slider2,Sliders3                       :  TSliders;
        Buttons                                        :  TButtons;
        HotKeys                                        :  THotKeys;
        ZoomUsed : Boolean;
        PROCEDURE Resolution;
        PROCEDURE SetUpSlidersAndButtons;
        PROCEDURE SetUpHotKeys;
        PROCEDURE HandleHotKeys(KeyNum: Byte);
        PROCEDURE HandleSlider1;
        PROCEDURE HandleSlider2;
        PROCEDURE HandleSliders3;
  END;

  TFabry = object
        Active  : Boolean;
        PROCEDURE Init;
        PROCEDURE SetDefaults;
        PROCEDURE SetUp;
        PROCEDURE CheckMouse;
        PROCEDURE Done;
    PRIVATE
        R,EtalonSeparation,AngleMin,AngleMax,
        Max,Increment,NewA                              : Real;
        FirstWavelength,FirstY                          : Integer;
        InViewPort,SliderUsed,ZoomUsed,
        FirstTime,RingsDrawn                            : Boolean;
        Lambda                                          : DVector;
        Buttons                                         : TButtons;
        HotKeys                                         : THotKeys;
        Sliders1,Sliders2                               : TSliders;
        PROCEDURE SetUpSlidersAndButtons;
        PROCEDURE SetUpHotKeys;
        PROCEDURE DrawRings;
        PROCEDURE Resolution;
        PROCEDURE HandleSliders1;
        PROCEDURE HandleSliders2;
        PROCEDURE HandleHotKeys(KeyNum : Byte);
  END;

   TMichelsonSpectrum = object
        Active             : Boolean;
        FUNCTION  ReadData : Boolean;
        PROCEDURE Init;
        PROCEDURE SetDefaults;
        PROCEDURE SetUp;
        PROCEDURE CheckMouse;
        PROCEDURE Reset;
        PROCEDURE Done;
    PRIVATE
        Wavelength,AngleMin,AngleMax,MirrorTravel,
        PlateSeparation,FirstPlateSeparation,
        Increment,Distance,LowValue,HighValue,Motion       : Real;
        Number                                             : Integer;
        FirstTime,SliderUsed,FirstRun                      : Boolean;
        Lambda,Theta,TotalIntensity,Intensity1,
        Intensity2,Delta1,Delta2,Separation                : DVector;
        ProblemButtons,AnimationButtons,MirrorButtons      : TButtons;
        AnimationHotKeys,HotKeys                           : THotKeys;
        Sliders                                            : TSliders;
        Problem,Animation,StopFlag,Running,RingsDrawn      : Boolean;
        PROCEDURE Spectrum;
        PROCEDURE SetUpSlidersAndButtons;
        PROCEDURE HandleSliders;
        PROCEDURE SetUpAnimationHotKeys;
        PROCEDURE SetUpHotKeys;
        PROCEDURE HandleAnimationHotKeys(KeyNum : Byte);
        PROCEDURE HandleHotKeys(KeyNum : Byte);
        PROCEDURE Close;
        PROCEDURE DRawRings;
   END;

   TMichelsonSolidIndex = object
        Active             : Boolean;
        FUNCTION  ReadData : Boolean;
        PROCEDURE Init;
        PROCEDURE SetDefaults;
        PROCEDURE CheckMouse;
        PROCEDURE SetUp;
        PROCEDURE Done;
    PRIVATE
        Wavelength,AngleMin,AngleMax,BackMirror,
        FirstBackMirror,Distance,Thickness,MuValue,
        Mu,LowValue,HighValue,SampleThickness             : Real;
        Number,SampleNumber                               : Integer;
        Crown,SampleA,SampleB,FirstTime                   : Boolean;
        Material                                          : String;
        Lambda                                            : DVector;
        SampleButtons,MirrorButtons                       : TButtons;
        Sliders                                           : TSliders;
        HotKeys                                           : THotKeys;
        PROCEDURE SetUpSlidersAndButtons;
        PROCEDURE HandleSliders;
        PROCEDURE SetUpHotKeys;
        PROCEDURE HandleHotKeys(KeyNum : Byte);
        PROCEDURE SolidMeasurement;
   END;


  TMichelsonGasIndex = object
      Active             : Boolean;
      Gas                : String;
      FUNCTION  ReadData : Boolean;
      PROCEDURE Init;
      PROCEDURE SetDefaults;
      PROCEDURE SetUp;
      PROCEDURE Reset;
      PROCEDURE Close;
      PROCEDURE Done;
   PRIVATE
      Wavelength,AngleMin,AngleMax,OldOrder,
      SecretMu,Mu,CellLength,CellLengthA,CellLengthB,
      PressureIncrement,Pressure,Increment,Order        : Real;
      Number,GasNumber                                  : Integer;
      Lambda,Theta,Intensity1,TotalIntensity,
      Intensity2,Delta1,Delta2                          : DVector;
      Air,GasA,GasB,StopFlag,Running                    : Boolean;
      HotKeys1,HotKeys2                                 : THotKeys;
      PROCEDURE SetUpHotKeys1;
      PROCEDURE HandleHotKeys1(KeyNum : Byte);
      PROCEDURE Index;
  END;


{*********************************************************************}

VAR
   Menu                                          : TMenu;
   MainHotKeys                                   : THotKeys;
   QuitFlag                                      : Boolean;
   KeyNum                                        : Byte;
   GratingResolution                             : TGratingResolution;
   Fabry                                         : TFabry;
   MichelsonSpectrum                             : TMichelsonSpectrum;
   MichelsonGasIndex                             : TMichelsonGasIndex;
   MichelsonSolidIndex                           : TMichelsonSolidIndex;
   Picture                                       : TFrames;
{************* INDEPENDENT FUNCTIONS AND PROCEDURES ********************}

PROCEDURE MenuClearCheck;
VAR I,J: Integer;
BEGIN
   FOR I := 1 to 16 DO
   BEGIN
      FOR J := 1 to 5 DO Menu.RowChecked(J,I,False);
   END
END;

PROCEDURE SetUpMenu;
BEGIN
   DefineViewPort(20,0,1,0.05,0.94);
   Menu.Init;
   WITH Menu DO
   BEGIN
      Column(1,'File');
        Row(1,1,'About CUPS');
        Row(1,2,'About Program');
        Row(1,3,'Program Details');
        Row(1,4,'Configuration');
        Row(1,5,'---------------');
        Row(1,6,'Exit Program');
        RowActivate(1,5,false);
      Column(2,' Gratings');
        Row(2,1,'Transmission Grating - Spectrum');
        RowChecked(2,1,True);
        Row(2,2,'Transmission Grating - Resolution');
        Row(2,3,'Blazed Reflection Grating');
      Column(3,'Prism');
        Row(3,1,'Spectrum');
        Row(3,2,'Resolution');
      Column(4,'Michelson');
        Row(4,1,'Spectrum');
        Row(4,2,'Fourier Transform Spectroscopy');
        Row(4,3,'Refractive Index of a Gas');
        Row(4,4,'Refractive Index of a Solid');
      Column(5,'FabryPerot');
        Row(5,1,'Spectrum and Resolution');
      ColSize := 11;
      Display;
    END;
END;

PROCEDURE SetUpMainHotKeys;
BEGIN
  WITH MainHotKeys DO
  BEGIN
      Init(2);
      Key[1] := 'F1-Help';
      Key[2] := 'F10-Menu';
      Display;
   END;
END;

PROCEDURE SetFlagsFalse;
BEGIN
   QuitFlag                   := False;
   PrismSpectrum.Active       := False;
   PrismResolution.Active     := False;
   GratingSpectrum.Active     := False;
   GratingResolution.Active   := False;
   Reflection.Active          := False;
   Fabry.Active               := False;
   MichelsonSpectrum.Active   := False;
   MichelsonFrequency.Active  := False;
   MichelsonGasIndex.Active   := False;
   MichelsonSolidIndex.Active := False;
   GratingSpectrum.F10Used := False;
   Reflection.F10Used := False;
END;

PROCEDURE CleanUp;
BEGIN
   IF GratingSpectrum.Active THEN
   BEGIN
      GratingSpectrum.Sliders.Done;
      GratingSpectrum.done;
   END;
   IF Reflection.Active THEN
   BEGIN
      Reflection.done;
      Reflection.Sliders.Done;
      Reflection.BlazeButtons.Done;
      Reflection.IncidentButtons.Done;
   END;
   IF GratingResolution.Active THEN
   BEGIN
      GratingResolution.Sliders3.Done;
      GratingResolution.Slider2.Done;
      GratingResolution.Slider1.Done;
      GratingResolution.Buttons.done;
      GratingResolution.done;
   END;
   IF PrismResolution.Active THEN
   BEGIN
      PrismResolution.Sliders.Done;
      PrismResolution.IncidentButtons.Done;
      PrismResolution.done;
   END;
   IF PrismSpectrum.Active THEN
   BEGIN
      PrismSpectrum.Sliders.Done;
      PrismSpectrum.RangeButtons.done;
      PrismSpectrum.IncidentButtons.Done;
      PrismSpectrum.done;
    END;
   IF Fabry.Active THEN
   BEGIN
      Fabry.Sliders1.Done;
      Fabry.Sliders2.Done;
      Fabry.Buttons.done;
      Fabry.done;
   END;
   IF MichelsonSolidIndex.Active THEN
   BEGIN
      MichelsonSolidIndex.Sliders.Done;
      MichelsonSolidIndex.SampleButtons.Done;
      MichelsonSolidIndex.MirrorButtons.Done;
      MichelsonSolidIndex.done;
   END;
   IF MichelsonFrequency.Active THEN
   BEGIN
      MichelsonFrequency.WindowButtons.Done;
      MichelsonFrequency.WaveSliders.Done;
      MichelsonFrequency.MirrorSliders.Done;

      MichelsonFrequency.done;
   END;
   IF MichelsonSpectrum.Active THEN
   BEGIN
      MichelsonSpectrum.Sliders.Done;
      MichelsonSpectrum.ProblemButtons.Done;
      MichelsonSpectrum.AnimationButtons.Done;
      MichelsonSpectrum.MirrorButtons.Done;
      MichelsonSpectrum.done;
   END;
   If MichelsonGasIndex.Active THEN  MichelsonGasIndex.done;
   SetFlagsFalse;
END;

PROCEDURE HandleMenu;
BEGIN
   IF MichelsonGasIndex.Active    THEN MichelsonGasIndex.Reset;
   IF MichelsonSpectrum.Animation THEN MichelsonSpectrum.Reset;
   MenuClearCheck;
   WITH Menu DO
   CASE ColChosen OF
   1: CASE rowChosen OF
          1:  BEGIN
                AboutCUPS;
                RowChecked(ColChosen,RowChosen,True);
              END;
          2:  BEGIN
                 ShowHelp('Spectrum.hlp','HELPSCREEN9');
                 RowChecked(ColChosen,RowChosen,True);
              END;
          3:  BEGIN
                 ShowHelp('Spectrum.hlp','HELPSCREEN1');
                 RowChecked(ColChosen,RowChosen,True);
              END;
          4:  BEGIN
                 Configuration;
                 RowChecked(ColChosen,RowChosen,True);
              END;
          6:  BEGIN
                 QuitFlag := True;
                 RowChecked(ColChosen,RowChosen,True);
              END;
      END; {of row 1 CASE}

   2: CASE rowChosen OF
           1: BEGIN
                 CleanUp;
                 GratingSpectrum.Init;
                 GratingSpectrum.SetUp;
                 RowChecked(ColChosen,RowChosen,True);
              END;
           2: BEGIN
                 CleanUp;
                 GratingResolution.Init;
                 GratingResolution.SetUp;
                 RowChecked(ColChosen,RowChosen,True);
              END;
           3: BEGIN
                 CleanUp;
                 Reflection.Init;
                 Reflection.SetUp;
                 RowChecked(ColChosen,RowChosen,True);
              END;
       END; {of row 2 CASE}

     3: CASE rowChosen OF
           1: BEGIN
                 CleanUp;
                 PrismSpectrum.init;
                 IF PrismSpectrum.ReadData THEN
                 BEGIN
                    PrismSpectrum.SetUp;
                    RowChecked(ColChosen,RowChosen,True);
                 END;
              END;

           2: BEGIN
                 CleanUp;
                 PrismResolution.init;
                 IF PrismResolution.ReadData THEN
                 BEGIN
                    PrismResolution.SetUp;
                    RowChecked(ColChosen,RowChosen,True);
                 END;
              END;
       END; {of col 4 CASE}

     4: CASE rowChosen OF
           1: BEGIN
                 CleanUp;
                 MichelsonSpectrum.Init;
                 IF MichelsonSpectrum.ReadData THEN
                 BEGIN
                    MichelsonSpectrum.SetUp;
                    RowChecked(ColChosen,RowChosen,True);
                 END;
              END;

           2: BEGIN
                 CleanUp;
                 MichelsonFrequency.Init;
                 MichelsonFrequency.SetUp;
                 RowChecked(ColChosen,RowChosen,True);
              END;

           3: BEGIN
                 CleanUp;
                 MichelsonGasIndex.Init;
                 IF MichelsonGasIndex.ReadData THEN
                 BEGIN
                    MichelsonGasIndex.SetUp;
                    RowChecked(ColChosen,RowChosen,True);
                 END;
              END;

           4: BEGIN
                 CleanUp;
                 MichelsonSolidIndex.Init;
                 IF MichelsonSolidIndex.ReadData THEN
                 BEGIN
                    MichelsonSolidIndex.SetUp;
                    RowChecked(ColChosen,RowChosen,True);
                 END;
              END;
          END; {of col 4 CASE}

       5: CASE rowChosen OF
           1: BEGIN
                 CleanUp;
                 Fabry.Init;
                 Fabry.SetUp;
                 RowChecked(ColChosen,RowChosen,True);
              END; {of col 5 CASE}
          END; {of col CASE}
   END;
   IF MichelsonGasIndex.Active THEN
   BEGIN
      MenuClearCheck;
      Menu.RowChecked(4,3,True);
   END;
   IF MichelsonSpectrum.Animation THEN
   BEGIN
      MenuClearCheck;
      Menu.RowChecked(4,1,True);
   END;
END;

PROCEDURE HandleHotKeys(keynum : byte);
BEGIN
   CASE KeyNum OF
    1: ShowHelp('Spectrum.hlp','HELPSCREEN1');
    2: IF Menu.chosen then HandleMenu;
   END;
END;

PROCEDURE CheckHotKeys;
BEGIN
   IF (GratingSpectrum.Active AND GratingSpectrum.HotKeys.Pressed(KeyNum))
       THEN GratingSpectrum.HandleHotKeys(KeyNum);
   IF (Reflection.Active AND Reflection.HotKeys.Pressed(KeyNum))
       THEN Reflection.HandleHotKeys(KeyNum);
   IF  (GratingResolution.Active AND GratingResolution.HotKeys.pressed(Keynum))
       THEN GratingResolution.HandleHotKeys(KeyNum);
   IF  (Fabry.Active AND Fabry.HotKeys.Pressed(KeyNum))
        THEN Fabry.HandleHotKeys(KeyNum);
   IF  ( MichelsonSpectrum.Active AND NOT MichelsonSpectrum.Animation
         AND MichelsonSpectrum.HotKeys.Pressed(KeyNum))
         THEN MichelsonSpectrum.HandleHotKeys(KeyNum);
   IF  ( MichelsonSpectrum.Active AND MichelsonSpectrum.Animation
         AND MichelsonSpectrum.AnimationHotKeys.Pressed(KeyNum)
         AND NOT MichelsonSpectrum.Running)
         THEN MichelsonSpectrum.HandleAnimationHotKeys(KeyNum);
   IF  (MichelsonGasIndex.Active
        AND MichelsonGasIndex.HotKeys1.Pressed(KeyNum)
        AND NOT MichelsonGasIndex.Running)
        THEN MichelsonGasIndex.HandleHotKeys1(KeyNum);
   IF  (MichelsonSolidIndex.Active AND MichelsonSolidIndex.HotKeys.Pressed(KeyNum))
       THEN MichelsonSolidIndex.HandleHotKeys(KeyNum);
   IF  (PrismSpectrum.Active AND PrismSpectrum.HotKeys.Pressed(KeyNum)) THEN
           PrismSpectrum.HandleHotKeys(KeyNum);
   IF  (PrismResolution.Active AND PrismResolution.HotKeys.Pressed(KeyNum))
       THEN PrismResolution.HandleHotKeys(KeyNum);
   IF  (MichelsonFrequency.Active AND MichelsonFrequency.HotKeys.Pressed(KeyNum))
       THEN MichelsonFrequency.HandleHotKeys(KeyNum);
   IF NOT (GratingSpectrum.Active
      OR   Reflection.Active
      OR   GratingResolution.Active
      OR   PrismSpectrum.Active
      OR   Fabry.Active
      OR   MichelsonSpectrum.Active
      OR   MIchelsonGasIndex.Active
      OR   MichelsonSolidIndex.Active
      OR   PrismSpectrum.Active
      OR   PrismResolution.Active
      OR   MichelsonFrequency.Active)
      AND MainHotKeys.Pressed(KeyNum) THEN HandleHotKeys(KeyNum);
END;

PROCEDURE CheckMenu;
BEGIN
   IF Menu.activated THEN HandleMenu;
END;

PROCEDURE CheckMouse;
BEGIN
   PrismSpectrum.CheckMouse;
   PrismResolution.CheckMouse;
   GratingSpectrum.CheckMouse;
   GratingResolution.CheckMouse;
   Reflection.CheckMouse;
   Fabry.CheckMouse;
   MichelsonSpectrum.CheckMouse;
   MichelsonFrequency.CheckMouse;
   MichelsonSolidIndex.CheckMouse;
END;

PROCEDURE CheckUnitHotKeys;
BEGIN
   IF GratingSpectrum.F10Used THEN
   BEGIN
      GratingSpectrum.F10Used := False;
      IF Menu.Chosen THEN HandleMenu;
   END;
   IF Reflection.F10Used THEN
   BEGIN
      Reflection.F10Used := False;
      IF Menu.Chosen THEN HandleMenu;
   END;
   IF PrismSpectrum.F10Used THEN
   BEGIN
      PrismSpectrum.F10Used := False;
      IF Menu.Chosen THEN HandleMenu;
   END;
   IF PrismResolution.F10Used THEN
   BEGIN
      PrismResolution.F10Used := False;
      IF Menu.Chosen THEN HandleMenu;
   END;
   IF MichelsonFrequency.F10Used THEN
   BEGIN
      MichelsonFrequency.F10Used := False;
      IF Menu.Chosen THEN HandleMenu;
   END;
END;

PROCEDURE Start;
BEGIN
   GratingSpectrum.init;
   SetFlagsFalse;
   GratingSpectrum.SetUp;
END;


{--------------------------- TGratingResolution --------------------------------------}

PROCEDURE TGratingResolution.Init;
BEGIN
   ResLambda.init(2);
   SetDefaults;
END;

PROCEDURE TGratingResolution.Done;
BEGIN
   ResLambda.free;
END;

PROCEDURE TGratingResolution.SetDefaults;
BEGIN
   ZoomUsed      := False;
   ResSlitWidth  := 0.005;        {4000};
   ResSlitSep    := 0.02; {10000;}   {mm}
   NumberofSlits := 2;
   AngleMax      := 5;
   AngleMin      := -5;
   ResLambda.put(1,600);
   ResLambda.put(2,550);
   FirstY        := 0;
   InViewPort    := False;
END;

PROCEDURE TGratingResolution.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(3,0.52,0.95,0.07,0.46);
   DefineViewPort(4,0.55,0.92,0.07,0.30);
   DefineViewPort(1,0.05,0.48,0.56,0.88);
   DefineViewPort(11,0.0,0.50,0.50,0.90);
   DefineViewPort(2,0.05,0.48,0.13,0.43);
   DefineViewPort(12,0.0,0.50,0.07,0.45);
   SetUpSlidersAndButtons;
   SetUpHotKeys;
   Resolution;
END;

PROCEDURE TGratingResolution.SetUpSlidersAndButtons;
BEGIN
   WITH Buttons DO
   BEGIN
      init;
      Create(1,0.65,0.33,'Zoom Range');
      Create(2,0.65,0.38,'Measurement');
      Number := 1;
   END;

   WITH Slider1 DO
   BEGIN
      init;
      Create(1,0.001,0.1,ResSlitSep,0.52,0.65,0.95,3,'0.001','0.1',' Slit Separation mm ',False);
   END;

   WITH Slider2 DO
   BEGIN
      init;
      Create(1,0.002,0.01,ResSlitWidth,0.52,0.75,0.95,3,'0.002','0.01','  Slit Width mm',False);
   END;

   WITH Sliders3 DO
   BEGIN
      init;
      Create(1,1,50,NumberofSlits,0.52,0.85,0.95,0,'1','50','Number of Slits',False);
      Create(2,400,800,ResLambda.Value(1),0.52,0.55,0.95,0,'400','800','Wavelength 1  green',False);
      Create(3,400,800,ResLambda.Value(2),0.52,0.45,0.95,0,'400','800','Wavelength 2  yellow',False);
   END;
END;

PROCEDURE TGratingResolution.SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      Init(4);
      Key[1] := 'F1-Help';
      Key[2] := 'F4-Zoom Out';
      Key[3] := 'F9-Defaults';
      Key[4] := 'F10-Menu';
   END;
END;


PROCEDURE TGratingResolution.HandleHotKeys(KeyNum : Byte);
BEGIN
   WITH GratingResolution DO
   BEGIN
      CASE KeyNum OF
          1: ShowHelp('Spectrum.hlp','HELPSCREEN12');
          2: BEGIN
                IF ZoomUsed THEN
                BEGIN
                   AngleMax := 5;
                   AngleMin := -5;
                   Buttons.Number := 1;
                   Buttons.DrawAll;
                   ZoomUsed := False;
                   Resolution;
                END;
             END;
          3: BEGIN
                GratingResolution.SetDefaults;
                IF GratingResolution.Active THEN GratingResolution.SetUp;
             END;
          4: IF Menu.Chosen THEN HandleMenu;
      END;
   END;
END;

PROCEDURE TGratingResolution.HandleSliders3;
BEGIN
   NumberofSlits := Round(Sliders3.Value(1));
   IF NumberofSlits = 1 THEN
   BEGIN
      slider1.erase(1,Black);
      slider1.delete(1);
   END
   ELSE
   BEGIN
      slider1.init;
      slider1.create(1,0.001,0.1,ResSlitSep,0.52,0.65,0.95,3,'0.001','0.1',' Slit Separation mm ',False);
      slider1.DrawAll;
   END;
   ResLambda.put(1,Sliders3.Value(2));
   ResLambda.put(2,Sliders3.Value(3));
   Resolution;
END;


PROCEDURE TGratingResolution.HandleSlider2;
BEGIN
   ResSlitWidth := Slider2.Value(1);
   Resolution;
END;


PROCEDURE TGratingResolution.HandleSlider1;
BEGIN
   ResSlitSep := Slider1.Value(1);
   AngleMax := 5;
   AngleMin := -5;
   Buttons.Number := 1;
   Buttons.DrawAll;
   ZoomUsed := false;
   Resolution;
END;

PROCEDURE TGratingResolution.CheckMouse;
VAR Angle,XMouse1,XMouse2,YMouse,v1,v2,
    OldAngleMax,OldAngleMin                   :  Real;
    X,Y,Button                                : Integer;
    KeyNum                                    : Byte;
    SecondClick                               : Boolean;
BEGIN
   IF Active THEN
   BEGIN
      IF Slider1.Changed  THEN HandleSlider1;
      IF Slider2.Changed  THEN HandleSlider2;
      IF Sliders3.Changed THEN HandleSliders3;
      IF Buttons.Changed  THEN
      BEGIN
         IF (Buttons.Number = 1) THEN
         BEGIN
            SelectViewPort(4);
            MakeBlueBox;
            Print(1,1,'   To Zoom Angular Range   ');
            Print(1,2,'    Use Either Viewport     ');
            Print(1,3,'  Click Mouse on Low Angle');
            Print(1,4,' Click Mouse on  High Angle');
            Print(1,5,'    Press F4 to Zoom Out  ');
         END;
         IF (Buttons.Number = 2) THEN
         BEGIN
            SelectViewPort(4);
            MakeBlueBox;
            Print(1,1,'    To Measure Intensity   ');
            Print(1,2,'                           ');
            Print(1,3,'        Use Mouse in         ');
            Print(1,4,'       Lower Viewport       ');
         END;
      END;

      IF  (Buttons.Number = 2) THEN
      BEGIN
         MouseGlobalPosn(x,y,button);
         WITH Views[2] DO
         BEGIN
            IF(x>vx1) AND (x<vx2) AND(y>vy1) AND(y<vy2) THEN
            BEGIN
               SelectViewPort(2);
               NewA := 2.2*(vy2 - Y)/(vy2 - vy1);
               IF Y <>FirstY THEN
               BEGIN
                  Print(2,2,'Intensity ');
                  Print2(12,2,2,NewA,lightgreen,Black);
                  FirstY := Y;
                  InViewPort := True;
                  SetColor(white);
               END;
            END
            ELSE
            BEGIN
               IF InViewPort THEN
               BEGIN
                  SelectViewPort(2);
                  RubOut(2,2,14,Black);
               END;
               InViewPort := False;
             END;
         END;
      END;

      IF ( (Buttons.Number = 1) AND Event.MouseClicked
         AND PointInside(Event.X,Event.Y,1) ) THEN
      BEGIN
         WITH views[1] DO
         BEGIN
            SecondClick := false;
            v1 := Event.X - vx1;
            v2 := vx2 - vx1;
            XMouse1 := AngleMin + (AngleMax - AngleMin)*v1/v2;
            REPEAT
               CheckForEvents;
               IF Event.MouseClicked THEN
               BEGIN
                  SecondClick := PointInside(Event.X,Event.Y,1);
                  if not SecondClick then
                     Announce('               Second point must be within ViewPort           ');
               END;
            UNTIL SecondClick;
            v1 := Event.X - vx1;
            v2 := vx2 - vx1;
            XMouse2 := AngleMin + (AngleMax - AngleMin)*v1/v2;
            OldAngleMax := AngleMax;
            OldAngleMin := AngleMin;
            AngleMin := Trunc(1000*XMouse1)/1000;
            AngleMax := Trunc(1000*XMouse2)/1000;
            IF AngleMax < AngleMin THEN
            BEGIN
               Angle := AngleMax;
               AngleMax := AngleMin;
               AngleMin := Angle;
            END;
            IF AngleMax = AngleMin THEN
            BEGIN
               AngleMin := OldAngleMin;
               AngleMax := OldAngleMax;
               Announce('  Values must be different - Old values retained - Try again  ');
               ZoomUsed := True;
               Resolution;
            END
            ELSE
            BEGIN
               ZoomUsed := True;
               Resolution;
            END;
         END;
      END;

      IF ( (Buttons.Number = 1) AND Event.MouseClicked
         AND PointInside(Event.X,Event.Y,2) ) THEN
      BEGIN
         WITH views[2] DO
         BEGIN
            SecondClick := false;
            v1 := Event.X - vx1;
            v2 := vx2 - vx1;
            XMouse1 := AngleMin + (AngleMax - AngleMin)*v1/v2;
            REPEAT
               CheckForEvents;
               IF Event.MouseClicked THEN
               BEGIN
                  SecondClick := PointInside(Event.X,Event.Y,2);
                  if not SecondClick then
                  Announce('            Second point must be within ViewPort              ');
               END;
            UNTIL SecondClick;
            v1 := Event.X - vx1;
            v2 := vx2 - vx1;
            XMouse2 := AngleMin + (AngleMax - AngleMin)*v1/v2;
            OldAngleMax := AngleMax;
            OldAngleMin := AngleMin;
            AngleMin := Trunc(1000*XMouse1)/1000;
            AngleMax := Trunc(1000*XMouse2)/1000;
            IF AngleMax < AngleMin THEN
            BEGIN
               Angle := AngleMax;
               AngleMax := AngleMin;
               AngleMin := Angle;
            END;
            IF AngleMax = AngleMin THEN
            BEGIN
               AngleMin := OldAngleMin;
               AngleMax := OldAngleMax;
               Announce('  Values must be different - old values retained - Try again  ');
               ZoomUsed := True;
               Resolution;
            END
            ELSE
            BEGIN
               ZoomUsed := True;
               Resolution;
            END;
         END;
      END;
   END;
END;

PROCEDURE TGratingResolution.Resolution;
VAR Width,Alpha,Beta,Term,x1,y1,Range,rad,Min         :      Real;
    Intensity                                         :      DMatrix;
    ResIntensity,Angle,TotalIntensity                 :      DVector;
    M,N,NumberofPoints,IMin,IMax,x,y,Button           :      Integer;
    AString                                           :      String;
BEGIN
   SetColor(white);
   Slider2.DrawAll;
   Sliders3.DrawAll;
   IF NumberofSlits > 1 THEN Slider1.DrawAll;
   Buttons.DrawAll;
   MainHotKeys.Clear;
   HotKeys.Display;
   GraphBackColor := Black;
   WITH Buttons DO
   BEGIN
      Done;
      Init;
      Create(1,0.65,0.33,'Zoom Range');
      Create(2,0.65,0.38,'Measurement');
      Number := 1;
      DrawAll;
   END;
   SelectViewPort(4);
   MakeBlueBox;
   Print(1,1,'   To Zoom Angular Range   ');
   Print(1,2,'    Use Either Viewport     ');
   Print(1,3,'  Click Mouse on Low Angle');
   Print(1,4,' Click Mouse on  High Angle');
   Print(1,5,'    Press F4 to Zoom Out  ');
   Active := true;
   rad := pi/180;
   NumberofPoints := 263;
   Intensity.init(2,NumberofPoints);
   Angle.init(NumberofPoints + 1);
   ResIntensity.init(NumberofPoints);
   TotalIntensity.init(NumberofPoints);
   OpenViewPort(1);
   DefineScale(1,AngleMin,AngleMax,0,1.1);
   SelectScale(1);
   RubOutXY(0,0.55,40,Black);
   PutLabel(bottom,'Angular Range');
   AString:= ('       Individual Intensities');
   Print(1,1,AString);

   FOR M := 1 to 2 DO
   BEGIN
      Angle.put(1,AngleMin);
      Range := (AngleMax-AngleMin);
      FOR N := 1 to NumberofPoints DO
      BEGIN
         Alpha := pi*sin(Angle.Value(N)*rad)*ResSlitWidth/(1e-6*ResLambda.Value(M));
           IF Alpha = 0 THEN Alpha := 1e-9;
         Beta  := pi*sin(Angle.Value(N)*rad)*ResSlitSep/(1e-6*ResLambda.Value(M));
           IF Beta = 0 THEN Beta := 1e-9;
         Term := sqr(Sin(alpha)/alpha)*sqr(Sin(NumberofSlits*Beta)/Sin(Beta));
         Intensity.put(M,N,Term/sqr(NumberofSlits));
         Angle.put(N,Angle.Value(N));
         Angle.put(N + 1,Angle.Value(N) + Range/(NumberofPoints - 1));
      END;
      IF M = 1 THEN SetColor(lightgreen);
      IF M = 2 THEN SetColor(yellow);
      For N := 1 to NumberofPoints DO
        ResIntensity.put(N,Intensity.Value(M,N));
      Axis(0,0,TickSpace(AngleMax - AngleMin),0.5);
      PlotDvectors(Angle,ResIntensity,1,NumberofPoints);
   END;
   SetColor(white);
   FOR N := 1 to NumberofPoints DO
      TotalIntensity.put(N,Intensity.Value(1,N)
                         + Intensity.Value(2,N));
   TotalIntensity.MinMax(IMin,Min,IMax,Max);
   OpenViewPort(2);
   DefineScale(2,AngleMin,AngleMax,0,2.2);
   SelectScale(2);
   RubOutXY(0,0.12,40,Black);
   Axis(0,0,TickSpace(AngleMax - AngleMin),0.5);
   Astring := ('Total Intensity');
   Print(10,1,AString);
   PutLabel(bottom,'Angular Range');
   PlotDvectors(Angle,TotalIntensity,1,NumberofPoints);
   AString:= ('         Total Intensity');
   Print(1,1,AString);
   Intensity.free;
   Angle.free;
   ResIntensity.free;
   TotalIntensity.free;
END;

{--------------------------- TFabry  --------------------------------}
PROCEDURE TFabry.Init;
BEGIN
   SetDefaults;
END;

PROCEDURE TFabry.Done;
BEGIN

END;

PROCEDURE TFabry.SetDefaults;
BEGIN
   ZoomUsed         := false;
   R                := 0.9;
   EtalonSeparation := 0.001;
   AngleMin         := -2;
   AngleMax         := 2;
   FirstWavelength  := 500;
   Increment        := 0.17;
   FirstY           := 0;
   FirstTime        := True;
   InViewPort       := False;
   RingsDrawn       := False;
END;

PROCEDURE TFabry.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(1,0.1,0.57,0.60,0.89);
   DefineViewPort(2,0.1,0.57,0.19,0.47);
   DefineViewPort(8,0.59,1.0,0.68,0.9);  {Data Box}
   DefineViewPort(3,0.59,1.0,0.37,0.63);
   DefineViewPort(19,0.1,0.57,0.19,0.47);
   DefineViewPort(18,0,0.095,0.15,0.5);
   IF Active THEN
    BEGIN
       Sliders1.erase(1,black);
       Sliders1.erase(2,black);
       Sliders2.erase(1,black);
       Sliders2.erase(2,black);
       Sliders1.done;
       Sliders2.done;
    END;
   SetUpSlidersAndButtons;
   SetUpHotKeys;
   RingsDrawn := False;
   Resolution;
END;


PROCEDURE TFabry.SetUpHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      Init(5);
      Key[1] := 'F1-Help';
      Key[2] := 'F3-ShowRings';
      Key[3] := 'F4-Zoom Out';
      Key[4] := 'F9-Defaults';
      Key[5] := 'F10-Menu';
   END;
END;

PROCEDURE TFabry.SetUpSlidersAndButtons;
BEGIN
   WITH Buttons DO
   BEGIN
      init;
      Create(1,0.15,0.52,'Zoom Range');
      Create(2,0.35,0.52,'Measurement');
      Number := 1;
   END;

   WITH Sliders1 DO
   BEGIN
      init;
      Create(1,0,0.005,EtalonSeparation,0.59,0.24,1.00,6,'0','0.005','Separation ',False);
      Create(2,400,800,FirstWavelength,0.59,0.17,0.99,0,'400','800','Wavelength nm',False);
   END;

   WITH Sliders2 DO
   BEGIN
      init;
      Create(1,0,0.975,R,0.59,0.31,1.00,4,'0','0.975','Reflectivity',False);
      Create(2,0.001,0.5,Increment,0.59,0.10,1.00,4,'0.001','0.5','Increment nm',False);
   END;
END;

PROCEDURE TFabry.HandleHotKeys(KeyNum : Byte);
   Procedure ZoomOut;
   BEGIN
      SelectViewport(1);
      DrawAxis(0,0,TickSpace(AngleMax - AngleMin),1,Black);
      SelectViewPort(2);
      DrawAxis(0,0,TickSpace(AngleMax - AngleMin),2,Black);
      AngleMax := 2;
      AngleMin := -2;
      ZoomUsed := False;
      FirstTime := True;
      CloseViewPort(8);
      RingsDrawn := False;
      Comment(0.05,0.13,'                                        ');
      Resolution;
   END;
BEGIN
   WITH Fabry DO
   BEGIN
      CASE KeyNum OF
       1: ShowHelp('Spectrum.hlp','HELPSCREEN14');
       2: BEGIN
             IF ZoomUsed THEN
                Announce('                Zoom Out before Drawing Rings                 ')
             ELSE BEGIN
                RingsDrawn := not RingsDrawn;
                IF RingsDrawn THEN
                BEGIN
                   DrawRings;
                   HotKeys.Key[2] := 'F3-HideRings';
                END ELSE BEGIN
                   ZoomOut;
                   HotKeys.Key[2] := 'F3-ShowRings';
                end;
                HotKeys.Display;
             END;
          END;
       3: IF ZoomUsed OR RingsDrawn THEN
          BEGIN
            ZoomOut;
            Hotkeys.Key[2] := 'F3-ShowRings';
            HotKeys.Display;
          END;

       4: BEGIN
             SetDefaults;
             SetUp;
          END;
       5: IF Menu.Chosen THEN HandleMenu;
      END;
   END;
END;

PROCEDURE TFabry.HandleSliders2;
BEGIN
   R := Sliders2.Value(1);
   Increment := Sliders2.Value(2);
   IF RingsDrawn THEN
   BEGIN
      Resolution;
      DrawRings;
   END;
   IF Not RingsDrawn THEN
   BEGIN
      Resolution;
   END;
END;

PROCEDURE TFabry.HandleSliders1;
BEGIN
   EtalonSeparation := Sliders1.Value(1);
   FirstWavelength := Round(Sliders1.Value(2));
   AngleMax := 2;
   AngleMin := -2;
   IF RingsDrawn THEN
   BEGIN
      Resolution;
      DrawRings;
   END;
   IF Not RingsDrawn THEN
   BEGIN
      Resolution;
      Comment(0.05,0.13,'                                        ')

   END;
END;

PROCEDURE TFabry.CheckMouse;
VAR v1,v2,XMouse,XMouse1,XMouse2,rad,Order,RP,
    Ratio,SpectralRange,Angle,OldAngleMax,OldAngleMin     : Real;
    X,Y,Button                                            : Integer;
    SecondClick                                           : Boolean;
    AString                                               : String;
BEGIN
   IF Active THEN
   BEGIN
      IF Sliders1.Changed THEN HandleSliders1;
      IF Sliders2.Changed THEN HandleSliders2;

      IF ((Buttons.Number = 2) AND Event.MouseClicked
        AND PointInside(Event.X,Event.Y,1) ) THEN
      WITH views[1] DO
      BEGIN
         SelectViewPort(1);
         rad := pi/180;
         v1 := Event.X - vx1;
         v2 := vx2 - vx1;
         XMouse := v1/v2*(AngleMax - AngleMin) + AngleMin;
         SetColor(yellow);
         RubOut(8,1,11,Black);
         Order := 2*EtalonSeparation*1e9*cos(rad*XMouse)/FirstWavelength;
         AString := concat(' Order = ',NUmStr(Order,4,2));
         Print(1,1,AString);
         SetColor(lightgreen);
         RubOut(8,2,11,Black);
         Order := 2*EtalonSeparation*1e9*cos(rad*XMouse)/(FirstWavelength+Increment);
         AString := concat(' Order = ',NUmStr(Order,4,2));
         Print(1,2,AString);
         SetColor(white);
         SelectViewport(8);
         MakeBlueBox;
         RubOut(25,1,7,blue);
         RP := Order*1.49*sqrt(4*R/sqr(1-R));
         AString := concat('      Resolving Power = ',NumStr(RP,5,0));
         Print(1,1,AString);
         Ratio := 0.5*(FirstWavelength + Increment)
             /Increment;
         AString := concat('     Lambda/dLambda = ',(NUmStr(Ratio,5,0)));
         Print(1,4,AString);
         SpectralRange := 0.5*(FirstWavelength + Increment)/Order;
         AString := ('   Free Spectral Range');
         Print(4,2,AString);
         RubOut(7,3,12,Blue);
         AString := concat('       ',NumStr(SpectralRange,3,6),' nm');
         Print(4,3,AString);
         AString :=
           concat('   Finesse = ',NumStr(0.5*pi*Sqrt(4*R/sqr(1-R)),5,2));
         Print(5,5,AString);
         FirstTime := False;
      END;

      IF  (Buttons.Number = 2) AND NOT RingsDrawn THEN
      BEGIN
         MouseGlobalPosn(x,y,button);
         WITH Views[2] DO
         BEGIN
            IF(x>vx1) AND (x<vx2) AND(y>vy1) AND(y<vy2) THEN
            BEGIN
               SelectViewPort(2);
               NewA := 2.2*(vy2 - Y)/(vy2 - vy1);
               IF Y <> FirstY THEN
               BEGIN
                  Print(2,1,'Intensity');
                  Print2(12,1,2,NewA,lightgreen,Black);
                  FirstY := Y;
                  InViewPort := True;
                  SetColor(white);
               END;
            END
            ELSE
            BEGIN
               IF InViewPort THEN
               BEGIN
                  SelectViewPort(2);
                  RubOut(2,1,14,Black);
               END;
               InViewPort := False;
            END;
         END;
      END;

      IF Buttons.Changed THEN
      BEGIN
         IF (Buttons.Number = 1) THEN
         BEGIN
            CloseViewPort(8);
            FirstTime := True;
            SelectViewPort(3);
            MakeBlueBox;
            Print(1,1,'     To Zoom Angular Range   ');
            Print(1,3,'      Use Either Viewport    ');
            Print(1,4,'   Click Mouse on  Low Angle');
            Print(1,5,'   Click Mouse on  High Angle');
            Print(1,6,'      Press F4 to Zoom Out  ');
         END;

         IF (Buttons.Number = 2) THEN
         BEGIN
            SelectViewPort(3);
            MakeBlueBox;
            Print(1,1,'  Click Mouse in Upper Window ');
            Print(1,2,'           for Data ');
            Print(1,4,'     To Measure Intensity ');
            Print(1,5,'   Use Mouse in Lower Window ');

         END;
      END;

      IF ( (Buttons.Number = 1) AND Event.MouseClicked
        AND PointInside(Event.X,Event.Y,2) ) THEN
      BEGIN
         CloseViewPort(8);
         Comment(0.05,0.13,'                                        ');
         FirstTime := True;
         WITH views[2] DO
         BEGIN
            SecondClick := false;
            v1 := Event.X - vx1;
            v2 := vx2 - vx1;
            XMouse1 := AngleMin + (AngleMax - AngleMin)*v1/v2;
            REPEAT
               CheckForEvents;
               IF Event.MouseClicked THEN
               BEGIN
                  SecondClick := PointInside(Event.X,Event.Y,2);
                  if not SecondClick then
                  Announce('             Second point must be within ViewPort             ');
               END;
            UNTIL SecondClick;
            v1 := Event.X - vx1;
            v2 := vx2 - vx1;
            XMouse2 := AngleMin + (AngleMax - AngleMin)*v1/v2;
            OldAngleMax := AngleMax;
            OldAngleMin := AngleMin;
            AngleMin := Trunc(1000*XMouse1)/1000;
            AngleMax := Trunc(1000*XMouse2)/1000;
            IF AngleMax < AngleMin THEN
            BEGIN
               Angle := AngleMax;
               AngleMax := AngleMin;
               AngleMin := Angle;
            END;
            IF AngleMax = AngleMin THEN
            BEGIN
               AngleMin := OldAngleMin;
               AngleMax := OldAngleMax;
               Announce('  Values must be different - Old values retained - Try again  ');
               ZoomUsed := True;
               Resolution;
            END
            ELSE
            BEGIN
               ZoomUsed := True;
               Resolution;
            END;
            Hotkeys.Key[2] := 'F3-ShowRings';
            HotKeys.Display;
         END;
      END;

      IF ( (Buttons.Number = 1) AND Event.MouseClicked
        AND PointInside(Event.X,Event.Y,1) ) THEN
      BEGIN
         CloseViewPort(8);
         Comment(0.05,0.13,'                                        ');
         FirstTime := True;
         WITH views[1] DO
         BEGIN
            SecondClick := false;
            v1 := Event.X - vx1;
            v2 := vx2 - vx1;
            XMouse1 := AngleMin + (AngleMax - AngleMin)*v1/v2;

            REPEAT
               CheckForEvents;
               IF Event.MouseClicked THEN
               BEGIN
                  SecondClick := PointInside(Event.X,Event.Y,1);
                  if not SecondClick then
                  Announce('             Second point must be within ViewPort             ');
               END;
            UNTIL SecondClick;
            v1 := Event.X - vx1;
            v2 := vx2 - vx1;
            XMouse2 := AngleMin + (AngleMax - AngleMin)*v1/v2;
            SelectViewport(1);
            DrawAxis(0,0,TickSpace(AngleMax - AngleMin),1,Black);
            SelectViewPort(2);
            DrawAxis(0,0,TickSpace(AngleMax - AngleMin),2,Black);
            OldAngleMax := AngleMax;
            OldAngleMin := AngleMin;
            AngleMin := Trunc(1000*XMouse1)/1000;
            AngleMax := Trunc(1000*XMouse2)/1000;
            IF AngleMax < AngleMin THEN
            BEGIN
               Angle := AngleMax;
               AngleMax := AngleMin;
               AngleMin := Angle;
            END;
            IF AngleMax = AngleMin THEN
            BEGIN
               AngleMin := OldAngleMin;
               AngleMax := OldAngleMax;
               Announce('  Values must be different - Old values retained - Try again  ');
               ZoomUsed := True;
               Resolution;
            END
            ELSE
            BEGIN
               ZoomUsed := True;
               Resolution;
            END;
            Hotkeys.Key[2] := 'F3-ShowRings';
            HotKeys.Display;
         END;

      END;
   END;
END;

PROCEDURE TFabry.Resolution;
VAR XValue,IncidentAngle,TotalIntensity,
    Intensity1,Intensity2                                  : DVector;
    Ratio,TMax,F,Mu,AngleRange,rad,Min                     : Real;
    M,N,Number,IMin,IMax                                   : Integer;
    AString                                                : String;
BEGIN
   Sliders1.DrawAll;
   Sliders2.DrawAll;
   Buttons.DrawAll;
   MainHotKeys.Clear;
   HotKeys.Display;
   WITH Buttons DO
   BEGIN
      Done;
      Init;
      Create(1,0.15,0.52,'Zoom Range');
      Create(2,0.35,0.52,'Measurement');
      Number := 1;
      DrawAll;
   END;
   CloseViewPort(8);
   SelectViewPort(3);
   MakeBlueBox;
   Print(1,1,'     To Zoom Angular Range   ');
   Print(1,3,'       Use Either Viewport    ');
   Print(1,4,'   Click Mouse on  Low Angle');
   Print(1,5,'   Click Mouse on  High Angle');
   Print(1,6,'      Press F4 to Zoom Out  ');
   Active := true;
    rad := pi/180;
    Number := 500;
    TotalIntensity.init(Number);
    Intensity1.init(Number);
    Intensity2.init(Number);
    XValue.init(Number + 1);
    IncidentAngle.init(Number + 1);
    TMax := 1;
    Mu := 1.0;
    F := 4*R/sqr(1-R);
    SetColor(green);
    AngleRange := AngleMax - AngleMin;
    IncidentAngle.put(1,AngleMin);
    XValue.put(1,4*pi*mu*EtalonSeparation*1e9*cos(IncidentAngle.Value(1)));
    FOR N := 1 to Number DO
    BEGIN
       Intensity1.put(N,TMax/(1 + F*sqr(sin(XValue.Value(N)/FirstWavelength/2))));
       Intensity2.put(N,TMax/(1 + F*sqr(sin(XValue.Value(N)/(FirstWavelength+Increment)/2))));
       IncidentAngle.put(N+1,IncidentAngle.Value(N) + AngleRange/Number);
       XValue.put(N+1,4*pi*Mu*EtalonSeparation*1e9*cos(rad*IncidentAngle.Value(N+1)));
       TotalIntensity.put(N,Intensity1.Value(N) + Intensity2.Value(N));
    END;
    TotalIntensity.MinMax(IMin,MIn,IMax,Max);
    SetColor(white);
    GraphBackColor := Black;
    OpenViewPort(1);
    DefineScale(1,AngleMin,AngleMax,0,2.2);
    SelectScale(1);
    RubOutXY(0,0.59,46,Black);
    Axis(0,0,TickSpace(AngleMax - AngleMin),1);
    PutLabel(left,'Intensity');
    PutLabel(bottom,'Angle');
    SetColor(yellow);
    AString := NumStr(FirstWavelength,5,3);
    Print(30,1,AString);
    PlotDvectors(IncidentAngle,Intensity1,1,Number - 1);
    SetColor(lightgreen);
    AString := NumStr(FirstWavelength+Increment,5,3);
    Print(30,2,AString);
    PlotDvectors(IncidentAngle,Intensity2,1,Number - 1);
    CloseViewPort(18);
    IF NOT RingsDrawn OR ZoomUsed THEN
    BEGIN
       OpenViewPort(2);
       DefineScale(2,AngleMin,AngleMax,0,2.2);
       SelectScale(2);
       RubOutXY(0,0.18,46,Black);
       Axis(0,0,TickSpace(AngleMax - AngleMin),1);
       SetColor(white);
       PutLabel(left,'Intensity');
       PutLabel(bottom,'Angle');
       PlotDvectors(IncidentAngle,TotalIntensity,1,Number - 1);
    END;
    TotalIntensity.free;
    Intensity1.free;
    Intensity2.free;
    XValue.free;
    IncidentAngle.free;
END;

PROCEDURE TFabry.DrawRings;
Var M2Center,M1Center,M1,M2,DDistance,Angle1,Angle2,N,Value : Real;
    A1,A2,F,G,H                                             : Word;
    L                                                       : Integer;
BEGIN
   M1Center := 2*Etalonseparation*1e9/FirstWavelength + 1e-9;
   M1 := trunc(2*Etalonseparation*1e9/FirstWavelength + 1e-9) ;
   M2Center := 2*Etalonseparation*1e9/(FirstWavelength + Increment) + 1e-9;
   M2 := trunc(2*Etalonseparation*1e9/(FirstWavelength + Increment) + 1e-9) ;
   CloseViewPort(18);
   OpenViewPort(19);
   DefineScale(19,Anglemin,Anglemax,Anglemin,Anglemax);
   RubOutXY(0,0.18,46,Black);
   Axis(0,0,TickSpace(AngleMax - AngleMin),10);
   With Views[19] Do
   BEGIN
      F := (vx2-vx1);
      G := round((vx2+vx1)/2 - vx1);
      H := round((vy2+vy1)/2 - vy1);
   END;
   SetColor(yellow);
   N := 0;
   REPEAT
      Value := (M1 - N)/M1Center;
      IF abs(Value) > 1 THEN Value := 0.99;
      Angle1 := 180/pi*arccos(Value);
      A1 := round((Angle1/AngleMax)*0.5*F);
      Circle(G,H,A1);
      N := N + 1;
   UNTIL Angle1 > AngleMax;
   SetColor(lightgreen);
   N := 0;
   REPEAT
      Value := (M2 - N)/M2Center;
      IF abs(Value) > 1 THEN Value := 0.99;
      Angle2 := 180/pi*arccos(Value);
      A2 := round((Angle2/AngleMax)*0.5*F);
      Circle(G,H,A2);
      N := N + 1;
   UNTIL Angle2 > AngleMax;
   SetColor(white);
   RingsDrawn := True;
END;

{----------------------- TMichelsonSpectrum  --------------------------------}
PROCEDURE TMichelsonSpectrum.Init;
BEGIN
   Lambda.init(2);
   SetDefaults;
END;

PROCEDURE TMichelsonSpectrum.Done;
BEGIN
   Lambda.free;
END;

PROCEDURE TMichelsonSpectrum.SetDefaults;
BEGIN
   SliderUsed      := False;
   LowValue        := -0.002;
   HighValue       := 0.002;
   Lambda.put(1,600);
   Lambda.put(2,650);
   Number          := 500;
   PlateSeparation := 0.001;
   MirrorTravel    := 6e-6;
   AngleMin        := -5;
   AngleMax        := 5;
   Motion          := 1.2e-5;
   GraphBackColor  := Black;
END;

PROCEDURE TMichelsonSpectrum.SetUp;
BEGIN
    IF NOT Active THEN CloseViewPort(20);
    DefineViewPort(1,0.1,0.7,0.55,0.85);
    DefineViewPort(2,0.1,0.7,0.17,0.47);
    DefineViewPort(3,0.7,1,0.06,0.15);
    DefineViewPort(4,0.1,0.7,0.17,0.47);
    DefineViewPort(10,0.72,1,0.15,0.25);
    DefineViewPort(11,0.72,1,0.06,0.15);
    DefineViewPort(13,0.2,0.8,0.80,0.90);
    DefineViewPort(19,0.1,0.7,0.17,0.47);
    DefineViewPort(18,0,0.095,0.1,0.5);
    DefineViewPort(17,0.7,1,0.06,0.15);
    DefineScale(1,AngleMin,AngleMax,0,2.2);
    DefineScale(2,Anglemin,Anglemax,0,2.2);
    DefineScale(12,Anglemin,Anglemax,-0.4,2.4);
    SetUpSlidersAndButtons;
    SetUpAnimationHotKeys;
    SetUpHotKeys;
    Animation  := False;
    SliderUsed := True;
    FirstRun   := True;
    RingsDrawn := False;
    Spectrum;
END;

PROCEDURE TMichelsonSpectrum.SetUpHotKeys;
BEGIN
   WITH HotKeys DO
    BEGIN
       Init(6);
       Key[1] := 'F1-Help';
       Key[2] := 'F3-ShowRings';
       Key[3] := 'F4-ClearRings';
       Key[4] := 'F8-Rerun';
       Key[5] := 'F9-Defaults';
       Key[6] := 'F10-Menu';
     END;
END;

PROCEDURE TMichelsonSpectrum.SetUpAnimationHotKeys;
BEGIN
   WITH AnimationHotKeys DO
   BEGIN
      Init(5);
       Key[1] := 'F1-Help';
       Key[2] := 'F2-Stop';
       Key[3] := 'F4-Faster';
       Key[4] := 'F5-Slower';
       Key[5] := 'F10-Menu';
   END;
END;

PROCEDURE TMichelsonSpectrum.SetUpSlidersAndButtons;
BEGIN
   WITH ProblemButtons DO
    BEGIN
       init;
       Create(1,0.75,0.23,'Start of Growth');
       Create(2,0.75,0.18,'Finish of Growth');
       Number := 1;
    END;

    WITH AnimationButtons DO
    BEGIN
       init;
       Create(1,0.75,0.23,'Animation ON');
       Create(2,0.75,0.18,'Animation OFF');
       Number := 2;
    END;

    WITH MirrorButtons DO
    BEGIN
       init;
       Create(1,0.83,0.83,'Normal');
       Create(2,0.83,0.78,'Zoom');
       Number := 1;
    END;

    WITH Sliders DO
    BEGIN
       init;
       Create(1,LowValue,HighValue,PlateSeparation,0.85,0.30,0.75,7,NumStr(LowValue,1,3),
                NumStr(HighValue,1,3),'Plate Separation',True);
    END;
END;

FUNCTION TMichelsonSpectrum.ReadData : Boolean;
VAR FirstScreen       :  TInputScreen;
    N                 :  Integer;
    OK                : Boolean;
BEGIN
   With FirstScreen Do
   BEGIN
      init;
      DefineInputPort(0.01,0.75,0.06,0.55);
      LoadLine('             Michelson Interferometer                   ');
      LoadLine('                                                        ');
      LoadLine('Initial Mirror Separation = {          } m  (0 - 0.005) ');
      LoadLine('             Wavelength 1 = {          } nm (400 - 800) ');
      LoadLine('             Wavelength 2 = {          } nm (400 - 800) ');
      LoadLine('                                                        ');
      LoadLine('   Mirror Travel Distance = {      } m  (0 - 0.01)      ');
      LoadLine('                      ---------------------             ');
      LoadLine('     #1 Spectrum      #1  For the Exercises             ');
      LoadLine('                      ---------------------             ');
      LoadLine('         [  OK  ]   [Cancel]   [ Help ]                 ');
      Problem := False;
      SetNumber(1,PlateSeparation);
      SetNumberLimits(1,0,0.005);
      SetNumber(2,Lambda.Value(1));
      SetNumberLimits(2,400,800);
      SetNumber(3,Lambda.Value(2));
      SetNumberLimits(3,400,800);
      SetNumber(4,MirrorTravel);
      SetNumberLimits(4,0,0.1);
      SetRadioButton('1',1);
      SetHelpFile('Spectrum.hlp','HELPSCREEN26');
      REPEAT
         AcceptScreen;
         FOR N := 1 to 2 DO
           Lambda.put(N,GetNumber(N+1));
           Problem := GetBoolean(6);
         IF NOT Problem THEN
         BEGIN
            MirrorTravel := GetNumber(4);
            PlateSeparation := GetNumber(1);
         END;
         OK := GetBoolean(7);
      UNTIL OK or Canceled;
      FirstTime := False;
      IF Canceled THEN
      BEGIN
         CloseViewPort(20);
         MichelsonSpectrum.Active := False;
         Comment(0.2,0.5,'          Make another Menu Choice         ');
         HotKeys.Clear;
      END;
      ReadData :=  not Canceled;
      done;
   END;
END;

PROCEDURE TMichelsonSpectrum.HandleSliders;
BEGIN
   PlateSeparation := Sliders.Value(1);
   SliderUsed := True;
   FirstRun := False;
   IF Not RingsDrawn THEN Spectrum;
   IF RingsDrawn THEN
   BEGIN
      Spectrum;
      DrawRings;
   END;
END;

PROCEDURE TMichelsonSpectrum.HandleHotKeys(keynum : byte);
BEGIN
   With MichelsonSpectrum DO
   CASE KeyNum OF
       1:  BEGIN
              If Problem THEN ShowHelp('Spectrum.hlp','HELPSCREEN27');
              IF NOT Problem THEN ShowHelp('Spectrum.hlp','HELPSCREEN45');
           END;
       2:  BEGIN
              IF NOT RingsDrawn THEN
              BEGIN
                 DrawRings;
                 RingsDrawn := True;
              END;
           END;
       3:  BEGIN
              IF RingsDrawn = True THEN
              BEGIN
                 RingsDrawn := False;
                 Spectrum;
              END;
           END;
       4:  BEGIN
              IF ReadData THEN
              BEGIN
                 SetUp;
              END;
           END;
       5:  BEGIN
              CloseViewPort(3);
              RubOutXY(0.2,0.1,35,Black);
              Sliders.erase(1,Black);
              Sliders.delete(1);
              Sliders.Create(1,LowValue,HighValue,PlateSeparation,0.85,0.30,0.75,7,NumStr(LowValue,1,3),
                NumStr(HighValue,1,3),'Plate Separation',True);
              Sliders.DrawAll;
              Done;
              init;
              IF ReadData THEN
              BEGIN
                 SetUp;
              END;
           END;
       6: IF Menu.chosen THEN HandleMenu;
   END;
END;


PROCEDURE TMichelsonSpectrum.HandleAnimationHotKeys(keynum : byte);
VAR AString : String;
BEGIN
   With MichelsonSpectrum DO
   CASE KeyNum OF
       1: BEGIN
             IF Running THEN
             BEGIN
                Picture.Pause;
                ShowHelp('Spectrum.hlp','HELPSCREEN41');
                Picture.Continue;
             END;
             IF NOT Running THEN ShowHelp('Spectrum.hlp','HELPSCREEN41');
          END;
      2:  BEGIN
             StopFlag := Not StopFlag;
             IF StopFlag THEN AnimationHotKeys.Key[2] := 'F2-Run'
             ELSE AnimationHotKeys.Key[2] := 'F2-Stop';
             IF Running THEN
             BEGIN
                AnimationHotKeys.Display;
                Picture.Switch;
                AnimationHotKeys.Display;
                Picture.Switch;
             END;
          END;
       3: Increment := Increment*1.5;
       4: Increment := Increment/1.5;
       5: BEGIN
             IF NOT Running AND Menu.Chosen THEN
             BEGIN
               ShowMouse;
               HandleMenu;
             END;

             IF Running THEN
             BEGIN
                Picture.close;
                PlotDvectors(Theta,TotalIntensity,1,Number);
                RubOut(17,1,7,Black);
                AString := concat('Mirror Travel = ',NumStr(1e9*Distance,1,1),' nm');
                Print(2,1,AString);
                IF Menu.chosen THEN
                BEGIN
                   Close;
                   HandleMenu;
                END
                ELSE Picture.Continue;
             END;
        END;
   END;
END;

PROCEDURE TMichelsonSpectrum.CheckMouse;
VAR  v1,v2,XMouse,rad,Order,NewLowValue,NewHighValue      :  Real;
     AString                                              : String;
BEGIN
   IF Active AND NOT Animation THEN
   BEGIN
      IF (NOT Problem AND AnimationButtons.Changed) THEN
      BEGIN
         IF (AnimationButtons.Number = 1) THEN
         BEGIN
            Animation := true;
            Spectrum;
         END;
      END;

      IF (Problem AND ProblemButtons.Changed) THEN
      BEGIN
         IF (ProblemButtons.Number = 2) THEN
         BEGIN
            FirstTime := True;
            Motion := 10.7e-5; {mirror travel = 9.5e-3 m}
            Spectrum;         { giving 3.3e-8 m/sec}
         END;
                       {2 cm per week}

         IF (ProblemButtons.Number = 1) THEN
         BEGIN
            Motion := 1.2e-5;
            Spectrum;
         END;
      END;

      IF MirrorButtons.Changed THEN
      BEGIN
         IF (MirrorButtons.Number = 1) THEN
         BEGIN
            PlateSeparation := Sliders.Value(1);
            Sliders.erase(1,Black);
            Sliders.delete(1);
            Sliders.create(1,LowValue,HighValue,PlateSeparation,0.85,0.30,0.75,7,NumStr(LowValue,1,3),
             NumStr(HighValue,1,3),'Plate Separation',True);
            Sliders.DrawAll;
         END;

         IF (MirrorButtons.Number = 2) THEN
         BEGIN
            PlateSeparation := Sliders.Value(1);
            Sliders.erase(1,Black);
            Sliders.delete(1);
            IF PlateSeparation = 0 THEN
            BEGIN
               NewLowValue := 0;
               NewHighValue := 0.2*(HighValue - LowValue);
            END
            ELSE
            BEGIN
               NewLowValue := PlateSeparation - 0.05*(HighValue - LowValue);
               NewHighValue := PlateSeparation + 0.05*(HighValue - LowValue);
            END;
            Sliders.create(1,NewLowValue, NewHighValue,PlateSeparation,0.85,0.30,0.75,7,
              NumStr(NewLowValue,1,6),NumStr(NewHighValue,1,6),
              ' ',True);
            Sliders.DrawAll;
         END;
      END;

      IF Sliders.changed THEN HandleSliders;

      If (Event.MouseClicked AND PointInside(Event.X,Event.Y,1)) THEN
      WITH views[1] DO
      BEGIN
         SelectViewPort(1);
         rad := pi/180;
         v1 := Event.X - vx1;
         v2 := vx2 - vx1;
         XMouse := v1/v2*(AngleMax - AngleMin) + AngleMin;
         SetColor(yellow);
         RubOut(8,1,7,Black);
         Order := 2*PlateSeparation*1e9*cos(rad*XMouse)
              /Lambda.Value(1);
         AString := concat('Order = ',NUmStr(Order,4,1));
         Print(1,1,AString);
         SetColor(cyan);
         RubOut(8,2,7,Black);
         Order := 2*PlateSeparation*1e9*cos(rad*XMouse)
              /Lambda.Value(2);
         AString := concat('Order = ',NUmStr(Order,4,1));
         Print(1,2,AString);
         SetColor(white);
      END;
   END;
END;

PROCEDURE TMichelsonSpectrum.Spectrum;
VAr AngleRange,rad                        : Real;
    N                                     : Integer;
    AString                               : String;
BEGIN
   MainHotKeys.Clear;
   HotKeys.Display;
   IF FirstRun THEN CloseViewPort(10);
   Number := 500;
   Active := true;
   TotalIntensity.init(Number);
   Intensity1.init(Number);
   Intensity2.init(Number);
   Theta.init(Number + 1);
   Delta1.init(Number + 1);
   Delta2.init(Number + 1);
   Separation.init(101);
   rad := pi/180;
   AngleRange := Anglemax - Anglemin;
   Theta.put(1,Anglemin);
   IF NOT Animation THEN
   BEGIN
      IF Problem AND FirstRun THEN
      BEGIN
         Showhelp('spectrum.hlp','HELPSCREEN20');
         Lambda.put(1,550);
         Lambda.put(2,550);
         FirstRun := False;
      END;
      FOR N := 1 to Number DO
      BEGIN
         Delta1.put(N, pi + 2*pi*2*(PlateSeparation + Motion)*1e9*cos(rad*Theta.Value(N))
                      /Lambda.Value(1));
         Intensity1.put(N,sqr( cos(Delta1.Value(N)/2) ) );
         Delta2.put(N, pi + 2*pi*2*(PlateSeparation + Motion)*1e9*cos(rad*Theta.Value(N))
                      /Lambda.Value(2));
         Intensity2.put(N,sqr(cos(Delta2.Value(N)/2)));
         TotalIntensity.put(N,Intensity1.Value(N) + Intensity2.Value(N));
         Theta.put(N+1,Theta.Value(N) + AngleRange/Number);
      END;
      SetColor(white);
      IF NOT Problem THEN RubOutXY(0.2,0.1,70,Black);
      IF Problem THEN     PrintXY(0.2,0.1,'You are studying problem 6.7.6 (c)');
      OpenViewPort(1);
      SelectScale(1);
      Axis(0,0,TickSpace(AngleRange),0.5);
      PutLabel(left,'Intensity');
      PutLabel(bottom,'Angle');
      IF NOT Problem THEN
      BEGIN
         SetColor(lightcyan);
         AString := NumStr(Lambda.Value(1),5,5);
         Print(38,1,AString);
         PlotDvectors(Theta,Intensity1,1,Number - 1);
      END;
      SetColor(yellow);
      AString := NumStr(Lambda.Value(2),5,5);
      Print(38,2,AString);
      PlotDvectors(Theta,Intensity2,1,Number - 1);
      CloseViewPort(18);

      IF NOT RingsDrawn THEN
      BEGIN
         OpenViewPort(2);
         SelectScale(2);
         Axis(0,0,TickSpace(AngleRange),1);
         SetColor(white);
         PutLabel(left,'Intensity');
         PutLabel(bottom,'Angle');
         PlotDvectors(Theta,TotalIntensity,1,Number);
         Comment(0.13,0.92,'Click Mouse on Fringes to determine Order');
      END;
      IF Problem THEN ProblemButtons.DrawAll
      ELSE AnimationButtons.DrawAll;
      MirrorButtons.DrawAll;
      Sliders.DrawAll;
      SliderUsed := False;
   END;

   IF Animation THEN
   BEGIN
      Picture.init;
      SetGraphMode(GraphMode);
      ShowMouse;
      HotKeys.Clear;
      AnimationHotKeys.Display;
      Menu.Display;
      DefineViewPort(12,0.2,0.8,0.30,0.70);
      Increment := 1e-9*Lambda.Value(1)/10;
      OpenViewPort(13);
      RubOut(17,1,7,Black);
      AString := concat('Total Mirror Travel Distance = ',NUmSTr(1e9*MirrorTravel,1,1),' nm');
      Print(2,1,AString);
      AString := concat('Initial Mirror Position = ',NUmStr(1e9*PlateSeparation,1,1), ' nm');
      Print(2,2,AString);
      OpenViewPort(12);
      SelectScale(12);
      Axis(0,0,TickSpace(AngleRange),1);
      SetColor(white);
      PutLabel(left,'Intensity');
      PutLabel(bottom,'Angle');
      Picture.Open(12);
      Distance := 0;
      StopFlag := False;
      Running := true;
      WHILE (Distance < MirrorTravel) AND Running DO
      BEGIN
         CheckForEvents;
         IF AnimationHotKeys.Pressed(KeyNum) THEN HandleAnimationHotKeys(KeyNum);
         IF Event.MouseClicked AND (Event.Y < 20) THEN
         BEGIN
            Close;
            IF Menu.activated THEN HandleMenu;
         END;
         IF Running THEN
         BEGIN
            FOR N := 1 to Number DO
            BEGIN
               Delta1.put(N,pi + 2*pi*2*(PlateSeparation + Distance)*1e9*cos(rad*Theta.Value(N))
                            /Lambda.Value(1));
               Intensity1.put(N,sqr( cos(Delta1.Value(N)/2) ) );
               Delta2.put(N,pi + 2*pi*2*(PlateSeparation+Distance)*1e9*cos(rad*Theta.Value(N))
                            /Lambda.Value(2));
               Intensity2.put(N,sqr(cos(Delta2.Value(N)/2)));
               TotalIntensity.put(N,Intensity1.Value(N) + Intensity2.Value(N));
               Theta.put(N+1,Theta.Value(N) + AngleRange/Number);
            END;
            PlotDvectors(Theta,TotalIntensity,1,Number);
            Distance := Distance + Increment;
            RubOut(17,1,7,Black);
            AString := concat('Mirror Travel = ',NumStr(1e9*Distance,1,1),' nm');
            Print(2,1,AString);
            IF StopFlag THEN
            BEGIN
               IF NOT Picture.FirstPage THEN
               BEGIN
                  DynamicPause;
               END;
            END;
            Picture.SwitchandClean;
         END;
      END;

      IF Running THEN
      BEGIN
         Close;
      END;
   END;

   Intensity1.free;
   Intensity2.free;
   TotalIntensity.free;
   Delta1.free;
   Delta2.free;
   Theta.free;
   Separation.free;
END;

PROCEDURE TMichelsonSpectrum.Close;
VAR AString                        :   String;
BEGIN
   AnimationButtons.Number := 2;
   AnimationButtons.DrawAll;
   Picture.Close;
   PlotDvectors(Theta,TotalIntensity,1,Number);
   RubOut(17,1,7,Black);
   AString := concat('Mirror Travel = ',NumStr(1e9*Distance,1,1),' nm');
   Print(2,1,AString);
   Menu.RowChecked(4,1,True);
   Running := False;

END;

PROCEDURE TMichelsonSpectrum.Reset;
BEGIN
   IF Active THEN
   BEGIN
      GraphMode := VgaHi;
      SetGraphMode(GraphMode);
      ShowMouse;
      Menu.Display;
      MainHotKeys.Display;
      Animation := False;
   END;
END;

PROCEDURE TMichelsonSpectrum.DrawRings;
Var M2Center,M1Center,M1,M2,absM2Center,absM1Center,absM1,absM2,
    DDistance,Angle1,Angle2,N
    ,Value : Real;
    A1,A2,F,G,H  : Word;
    L : Integer;
BEGIN
   M1Center := 2*(Plateseparation+Motion)*1e9/Lambda.Value(1) + 1e-9;
   absM1Center := abs(M1center);
   M1 := round(2*(Plateseparation+Motion)*1e9/Lambda.Value(1) + 1e-9) + 0.5 ;
   absM1 := abs(M1);
   M2Center := 2*(Plateseparation+Motion)*1e9/Lambda.Value(2) + 1e-9;
   absM2Center := abs(M2Center);
   M2 := round(2*(Plateseparation+Motion)*1e9/Lambda.Value(2) + 1e-9) + 0.5 ;
   absM2 := abs(M2) ;
   CloseViewPort(18);
   OpenViewPort(19);
   DefineScale(19,Anglemin,Anglemax,Anglemin,Anglemax);
   Axis(0,0,TickSpace(AngleMax-AngleMin),10);
   With Views[19] Do
   BEGIn
      F := (vx2-vx1);
      G := round((vx2+vx1)/2 - vx1);
      H := round((vy2+vy1)/2 - vy1);
   END;

   IF Lambda.Value(1) <> Lambda.Value(2) THEN SetColor(lightcyan);
   IF M1Center > 0 THEN N := 1
   ELSE N := 0;
   REPEAT
      Value := (absM1 - N)/absM1Center;
      IF abs(Value) >1 THEN Value := 0.99;
      Angle1 := 180/pi*arccos(Value);
      A1 := round((Angle1/AngleMax)*0.5*F);
      Circle(G,H,A1);
      N := N + 1;
   UNTIL Angle1 > AngleMax;

   SetColor(yellow);
   IF M2Center > 0 THEN N := 1
   ELSE N := 0;
   REPEAT
      Value := (absM2 - N)/absM2Center;
      IF abs(Value) >1 THEN Value := 0.99;
      Angle2 := 180/pi*arccos(Value);
      A2 := round((Angle2/AngleMax)*0.5*F);
      Circle(G,H,A2);
      N := N + 1;
   UNTIL Angle2 > AngleMax ;
   RingsDrawn := True;
   SetColor(white);
END;

{----------------------- TMichelsonSolidIndex -------------------------------}
PROCEDURE TMichelsonSolidIndex.Init;
BEGIN
   Lambda.init(2);
   SetDefaults;
END;

PROCEDURE TMichelsonSolidIndex.Done;
BEGIN
   Lambda.free;
END;

PROCEDURE TMichelsonSolidIndex.SetDefaults;
BEGIN
   Lambda.put(1,550);
   Lambda.put(2,600);
   Number          := 500;
   BackMirror      := 0.0015;
   AngleMin        := -5;
   AngleMax        := 5;
   SampleThickness := 0.001;
   SampleNumber    := 1;
   LowValue        := -2*SampleThickness;
   HighValue       := 2*SampleThickness;
   GraphBackColor  := Black;
END;

PROCEDURE TMichelsonSolidIndex.SetUp;
BEGIN
    IF NOT Active THEN CloseViewPort(20);
    DefineViewPort(1,0.1,0.7,0.55,0.85);
    DefineScale(1,AngleMin,AngleMax,0,2.2);
    DefineViewPort(2,0.1,0.7,0.15,0.45);
    DefineScale(2,Anglemin,Anglemax,0,2.2);
    DefineViewPort(4,0.1,0.7,0.15,0.45);
    IF Active THEN
    BEGIN
       Sliders.erase(1,black);
       Sliders.done;
    END;
    SetUpSlidersAndButtons;
    SetUpHotKeys;
    FirstTime := True;
    SolidMeasurement;
END;

PROCEDURE TMichelsonSolidIndex.SetUpSlidersAndButtons;
BEGIN
   WITH Sliders DO
   BEGIN
      init;
      Create(1,LowValue,HighValue,BackMirror,0.83,0.30,0.75,8,NumStr(LowValue,1,3),
                NumStr(HighValue,1,3),'Back Mirror Position',True);
   END;

   WITH SampleButtons DO
   BEGIN
      init;
      Create(1,0.75,0.23,'Sample In');
      Create(2,0.75,0.18,'Sample Out');
      Number := 2;
   END;

   WITH MirrorButtons DO
   BEGIN
      init;
      Create(1,0.80,0.85,'Normal');
      Create(2,0.80,0.80,'Zoom');
      Number := 1;
   END;
END;

FUNCTION TMichelsonSolidIndex.ReadData : Boolean;
VAR FirstScreen                    : TInputScreen;
    OK                             : Boolean;
BEGIN
   With FirstScreen Do
   BEGIN
      init;
      DefineInputPort(0.1,0.9,0.06,0.65);
      LoadLine('               Michelson Interferometer                    ');
      LoadLine('    Measurement of the Refractive Index of a Solid         ');
      LoadLine('                                                           ');
      LoadLine('       Back Mirror Position = {          } m (-0.02 - 0.02) ');
      LoadLine('              Wavelength 1  = {    } nm  (400 - 800)        ');
      LoadLine('              Wavelength 2  = {    } nm  (400 - 800)        ');
      LoadLine('     Sample Plate Thickness = {      } m (0 - 0.0015)       ');
      LoadLine('                         _________________________          ');
      LoadLine('                            For  the  Exercises             ');
      LoadLine('          #1 CROWN GLASS       #1 SOLID A                   ');
      LoadLine('                         _________________________          ');
      LOadLine('                                                            ');
      LoadLine('             [  OK  ]   [Cancel]   [ Help ]                 ');
      SetNumber(1,BackMirror);
      SetNumberLimits(1,-0.002,0.002);
      SetNUmber(2,Lambda.Value(1));
      SetNumberLimits(2,400,800);
      SetNUmber(3,Lambda.Value(2));
      SetNumberLimits(3,400,800);
      SetNumber(4,SampleThickness);
      SetNumberLimits(4,0,0.0015);
      SetRadioButton('1',SampleNumber);
      SetHelpFile('Spectrum.hlp','HELPSCREEN25');
      REPEAT
         AcceptScreen;
         Crown := GetBoolean(5);
         SampleA := GetBoolean(6);
         BackMirror := GetNumber(1);
         Lambda.put(1,GetNumber(2));
         Lambda.put(2,GetNumber(3));
         SampleThickness := GetNumber(4);
         OK := GetBoolean(7);
      UNTIL OK or Canceled;
      IF Crown THEN
         BEGIN
            Material := 'CROWN';
            MuValue := 1.505;
         END;
         IF SampleA THEN
         BEGIN
            Material := 'Solid A'; {Fluorite}
            MuValue := 1.387;
         END;
      IF Canceled THEN
      BEGIN
         CloseViewPort(20);
         MichelsonSolidIndex.Active := False;
         Comment(0.2,0.5,'          Make another Menu Choice         ');
         HotKeys.Clear;
      END;
      ReadData :=  not Canceled;
      done;
   END;
END;

PROCEDURE TMichelsonSolidIndex.HandleSliders;
BEGIN
   BackMirror := Sliders.Value(1);
   SolidMeasurement;
END;

PROCEDURE TMichelsonSolidIndex.SetUpHotKeys;
BEGIN
WITH HotKeys DO
   BEGIN
       Init(4);
       Key[1] := 'F1-Help';
       Key[2] := 'F2-Rerun';
       Key[3] := 'F9-Defaults';
       Key[4] := 'F10-Menu';
   END;
END;

PROCEDURE TMichelsonSolidIndex.HandleHotKeys(keynum : byte);
BEGIN
   CASE KeyNum OF
       1:  BEGIN
              IF MichelsonSolidIndex.Crown THEN ShowHelp('Spectrum.hlp','HELPSCREEN39');
              IF MichelsonSolidIndex.SampleA THEN ShowHelp('Spectrum.hlp','HELPSCREEN28');
           END;
       2: BEGIN
             IF ReadData THEN
              BEGIN
                 SetUp;
              END;
          END;
       3: BEGIN
             Sliders.erase(1,Black);
             Sliders.delete(1);
             Sliders.Create(1,LowValue,HighValue,BackMirror,0.83,0.30,0.75,8,NumStr(LowValue,1,3),
                NumStr(HighValue,1,3),'Back Mirror Position',True);
             Sliders.DrawAll;
             Done;
             init;
             IF ReadData THEN
              BEGIN
                 SetUp;
              END;
          END;
       4: BEGIN
             IF Menu.chosen THEN HandleMenu;
          END;
   END;
END;

PROCEDURE TMichelsonSolidIndex.CheckMouse;
VAR  v1,v2,XMouse,rad,Order,NewLowValue,NewHighValue          : Real;
     AString                                                  : String;
BEGIN
   IF Active THEN
   BEGIN
      IF Sliders.changed THEN HandleSliders;

      IF SampleButtons.Changed THEN
      BEGIN
         BackMirror := Sliders.Value(1);
         SolidMeasurement;
      END;

      If MirrorButtons.Changed THEN
      BEGIN
         IF (MirrorButtons.Number = 1) THEN
         BEGIN
            BackMirror := Sliders.Value(1);
            WITH Sliders DO
            BEGIN
               erase(1,Black);
               delete(1);
               create(1,LowValue,HighValue,BackMirror,0.83,0.30,0.75,8,NumStr(LowValue,1,3),
                      NumStr(HighValue,1,3),'Back Mirror Position',True);
               DrawAll;
            END;
         END;

         IF (MirrorButtons.Number = 2) THEN
         BEGIN
            BackMirror := Sliders.Value(1);
            WITH Sliders DO
            BEGIN
               erase(1,Black);
               delete(1);
            END;
            IF ((BackMirror > -0.000001) AND (BackMirror < 0.000001))  THEN
            BEGIN
               NewLowValue := -0.0001;
               NewHighValue := 0.0001;
            END
            ELSE
            BEGIN
               NewLowValue := 0.90*BackMirror;
               NewHighValue := 1.10*BackMirror;
            END;
            WITH Sliders DO
            BEGIN
               create(1,NewLowValue, NewHighValue,BackMirror,0.83,0.30,0.75,8,
                       NumStr(NewLowValue,1,6),NumStr(NewHighValue,1,6),'Back Mirror Position',True);
               DrawAll;
            END;
         END;
      END;
   END;
END;

PROCEDURE TMichelsonSolidIndex.SolidMeasurement;
VAR AngleRange,rad,Value,StartThickness         : Real;
    N                                           : Integer;
    Theta,TotalIntensity,Intensity1,
    Intensity2,Delta1,Delta2                    : DVector;
    AString                                     : String;
BEGIN
   IF FirstTime AND SampleA THEN Showhelp('spectrum.hlp','HELPSCREEN21');
   FirstTime := False;
   Sliders.DrawAll;
   SampleButtons.DrawAll;
   MirrorButtons.DrawAll;
   MainHotKeys.Clear;
   HotKeys.Display;
   Active := True;
   TotalIntensity.init(Number);
   Intensity1.init(Number);
   Intensity2.init(Number);
   Theta.init(Number + 1);
   Delta1.init(Number + 1);
   Delta2.init(Number + 1);
   rad := pi/180;
   AngleRange := Anglemax - Anglemin;
   Theta.put(1,Anglemin);
   Value := Intensity1.Value(250);
   StartThickness := 0.00045;
   IF (SampleButtons.Number = 2) THEN
   BEGIN
      Mu := 1;
      Thickness := 0;
   END
   ELSE
   BEGIN
      Mu :=MuValue;
      Thickness := SampleThickness;
   END;
   FOR N := 1 to Number DO
   BEGIN
      Delta1.put(N,pi + 2*pi*2*(StartThickness+BackMirror - (Mu - 1)*
         Thickness)*1e9*cos(rad*Theta.Value(N))/Lambda.Value(1));
      Intensity1.put(N,sqr( cos(Delta1.Value(N)/2) ) );
      Delta2.put(N,pi + 2*pi*2*(StartThickness +BackMirror - (Mu - 1)*
         Thickness)*1e9*cos(rad*Theta.Value(N))/Lambda.Value(2));
      Intensity2.put(N,sqr( cos(Delta2.Value(N)/2) ) );
      TotalIntensity.put(N,Intensity1.Value(N) + Intensity2.Value(N));
      Theta.put(N+1,Theta.Value(N) + AngleRange/Number);
   END;
   SetColor(white);
   OpenViewPort(1);
   SelectScale(1);
   Axis(0,0,TickSpace(AngleMax - AngleMin),0.5);
   PutLabel(left,'Intensity');
   PutLabel(bottom,'Angle');
   AString := concat(' Material = ',String(Material));
   Print(1,1,AString);
   IF (Material = 'CROWN') THEN Print(2,2,NumStr(MuValue,1,5));
   SetColor(yellow);
   AString := NumStr(Lambda.Value(1),5,5);
   Print(38,1,AString);
   PlotDvectors(Theta,Intensity1,1,Number - 1);
   SetColor(cyan);
   AString := NumStr(Lambda.Value(2),5,5);
   Print(38,2,AString);
   PlotDvectors(Theta,Intensity2,1,Number - 1);
   OpenViewPort(2);
   SelectScale(2);
   Axis(0,0,TickSpace(AngleRange),1);
   SetColor(white);
   PutLabel(left,'Intensity');
   PutLabel(bottom,'Angle');
   PlotDvectors(Theta,TotalIntensity,1,Number);
   IF Material = 'Solid A' THEN PrintXY(0.2,0.08,'You are studying Problem 6.7.9 (b)');
   Intensity1.free;
   Intensity2.free;
   TotalIntensity.free;
   Delta1.free;
   Delta2.free;
   Theta.free;
END;


{********************** TMichelsonGasIndex ************************}
PROCEDURE TMichelsonGasIndex.Init;
BEGIN
   Lambda.init(1);
   SetDefaults;
END;

PROCEDURE TMichelsonGasIndex.Done;
BEGIN
   Lambda.free;
END;

PROCEDURE TMichelsonGasIndex.SetDefaults;
BEGIN
   Lambda.put(1,600);
   AngleMin             := -2;
   AngleMax             :=  2;
   CellLength           := 0.01;
   CellLengthA          := 0.015;
   CellLengthB          := 0.012;
   Mu                   := 1.0002926;
   Number               := 500;
   GasNumber            := 1;
   Gas                  := 'Air';
   GraphBackColor       := Black;
END;

PROCEDURE TMichelsonGasIndex.SetUp;
BEGIN
   IF NOt Active THEN CloseViewPort(20);
   Picture.init;
   SetGraphMode(GraphMode);
   ShowMouse;
   Active := true;
   DefineViewPort(1,0.1,0.7,0.35,0.75);
   DefineScale(1,AngleMin,AngleMax,0,2.2);
   DefineViewPort(19,0.72,0.96,0.35,0.75);
   SetUpHotKeys1;
   StopFlag := False;
   Index;
END;

PROCEDURE TMichelsonGasIndex.SetUpHotKeys1;
BEGIN
WITH HotKeys1 DO
   BEGIN
      Init(5);
       Key[1] := 'F1-Help';
       Key[2] := 'F2-Stop';
       Key[3] := 'F4-Faster';
       Key[4] := 'F5-Slower';
       Key[5] := 'F10-Menu';
   END;
END;



FUNCTION TMichelsonGasIndex.ReadData :  Boolean;
VAR FirstScreen                      :  TInputScreen;
    N                                :  Integer;
    OK                               :  Boolean;
BEGIN
   With FirstScreen Do
   BEGIN
      init;
      DefineInputPort(0.1,0.9,0.10,0.65);
      LoadLine('                  Michelson  Interferometer                  ');
      LoadLine('                  Refractive Index of a Gas                  ');
      LoadLine('                                                             ');
      LoadLine('          Wavelength  = {          } nm   (400 - 800)        ');
      LoadLine('          Cell Length = {     } m         (0 - 0.02)         ');
      LoadLine('                                                             ');
      LoadLine('                __________________________________________   ');
      LoadLine(' Specified Gas           For  the Exercises                  ');
      LoadLine('   #1 AIR       #1 MEASURE GAS A     #1 MEASURE CELL LENGTH  ');
      LoadLine('                ___________________________________________  ');
      LoadLine('                                                             ');
      LoadLine('               [  OK  ]   [Cancel]   [ Help ]                ');
      SetNumber(1,Lambda.Value(1));
      SetNumberLimits(1,400,800);
      SetNumber(2,CellLength);
      SetNumberLimits(2,0,0.02);
      SetRadioButton('1',GasNumber);
      SetHelpFile('Spectrum.hlp','HELPSCREEN18');
      REPEAT
         AcceptScreen;
         Lambda.put(1,GetNumber(1));
         Celllength := GetNumber(2);
         Air := GetBoolean(3);
         GasA := GetBoolean(4);
         GasB := GetBoolean(5);
         OK := GetBoolean(6);
      UNTIL OK or Canceled;
      IF Air THEN
         BEGIN
            Gas := 'Air';
            Mu := 1.0002926;
         END;
         IF GasA THEN
         BEGIN
            Gas := 'Gas A';
            Mu := 1.000036;
         END;
         IF GasB THEN
         BEGIN
            Gas := 'Gas B';
            Mu := 1.000132;
         END;
      IF Canceled THEN
      BEGIN
         CloseViewPort(20);
         MichelsonGasIndex.Active := False;
         Comment(0.2,0.5,'          Make another Menu Choice         ');
         HotKeys1.Clear;
      END;
      ReadData := not Canceled;
      done;
   END;
END;


PROCEDURE TMichelsonGasIndex.HandleHotKeys1(keynum : byte);
VAR AString : String;
BEGIN
   CASE KeyNum OF
   1: BEGIN
         IF Running THEN
         BEGIN
            Picture.Pause;
            IF MichelsonGasIndex.Air
                THEN ShowHelp('Spectrum.hlp','HELPSCREEN38');
            IF MichelsonGasIndex.GasA
                THEN ShowHelp('Spectrum.hlp','HELPSCREEN30');
            IF MichelsonGasIndex.GasB
                THEN ShowHelp('Spectrum.hlp','HELPSCREEN31');
            Picture.Continue;
         END;
         IF Not Running THEN
         BEGIN
            IF MichelsonGasIndex.Air
                THEN ShowHelp('Spectrum.hlp','HELPSCREEN38');
            IF MichelsonGasIndex.GasA
                THEN ShowHelp('Spectrum.hlp','HELPSCREEN30');
            IF MichelsonGasIndex.GasB
                THEN ShowHelp('Spectrum.hlp','HELPSCREEN31');
         END;
      END;
   2: BEGIN
         StopFlag := Not StopFlag;
         IF StopFlag THEN HotKeys1.Key[2] := 'F2-Run'
         ELSE HotKeys1.Key[2] := 'F2-Stop';
         IF Running THEN
         BEGIN
            HotKeys1.Display;
            Picture.Switch;
            HotKeys1.Display;
            Picture.Switch;
         END;
      END;
   3: PressureIncrement := PressureIncrement*1.5;
   4: PressureIncrement := PressureIncrement/1.5;
   5: BEGIN
         IF NOT Running AND Menu.Chosen THEN
            BEGIN
               ShowMouse;
               HandleMenu;
            END;

         IF Running THEN
         BEGIN
            Picture.Close;
            PlotDvectors(Theta,Intensity1,1,Number);
            RubOut(11,3,10,Black);
            AString := concat('Pressure = ',NUmStr(Pressure,4,2),' mm Hg');
            Print(1,3,AString);
            IF Menu.chosen THEN
            BEGIN
                Close;
                HandleMenu;
            END
            ELSE Picture.Continue;
         END;
      END;
   END;
END;


PROCEDURE TMichelsonGasIndex.Index;
VAR Term,AngleRange,rad                  : Real;
    N                                    : Integer;
    AString                              : String;
BEGIN
    Number := 500;
    Intensity1.init(Number);
    TotalIntensity.init(Number);
    Intensity2.init(Number);
    Delta1.init(Number + 1);
    Delta2.init(Number + 1);
    Theta.init(Number + 1);
    Menu.Display;
    HotKeys1.Display;
    IF Gas = 'Gas A' THEN
    BEGIN
       CellLength := CelllengthA;
       Showhelp('spectrum.hlp','HELPSCREEN23');
    END
    ELSE
    IF Gas = 'Gas B' THEN
    BEGIN
    Showhelp('spectrum.hlp','HELPSCREEN24');
       CellLength := CelllengthB;
    END
    ELSE
    IF Gas = 'Air' THEN CellLength := Celllength;
    rad := pi/180;
    AngleRange := AngleMax - AngleMin;
    SetColor(white);
    OpenViewPort(19);
    Print(3,1,'Wavelength');
    AString := concat(NumStr(Lambda.Value(1),5,2),' nm');
    Print(3,2,AString);
    IF Gas = 'Air' THEN
    BEGIN
       Print(3,3,'Cell Length');
       AString := concat(NumStr(CellLength,1,3),' m');
       Print(3,4,AString);
    END;
    IF Gas = 'Gas A' THEN
    BEGIN
       Print(3,3,'Cell Length');
       AString := concat(NumStr(CellLengthA,1,3),' m');
       Print(3,4,AString);
    END;

    Print(3,5,'Refractive Index ');
    IF Gas = 'Air' THEN
    BEGIN
       Print(3,6,String(Gas));
       Print(3,7,NumStr(Mu,1,7));
    END;
    IF Gas = 'Gas A'  THEN
    BEGIN
       Print(3,6,String(Gas));
       PrintXY(0.2,0.1,'You are studying Problem 6.7.8 (b)');
    END;
    IF Gas = 'Gas B'  THEN
    BEGIN
       Print(3,6,NumStr(Mu,1,7));
       PrintXY(0.2,0.1,'You are studying Problem 6.7.8 (c)');
    END;
    OpenViewPort(1);
    SelectScale(1);
    Axis(0,0,TickSpace(AngleRange),0.5);
    PutLabel(left,'Intensity');
    PutLabel(bottom,'Angle');
    Picture.Open(1);
    Pressure := 0;
    PressureIncrement := 760/50;
    Running := True;
    Theta.put(1,AngleMin);
    Menu.RowChecked(4,3,True);
    WHILE (Pressure < 759.99)  AND Running DO
    BEGIN
       CheckForEvents;

       IF  HotKeys1.Pressed(KeyNum) THEN HandleHotKeys1(KeyNum);
       IF Event.MouseClicked AND (Event.Y < 20) THEN
         BEGIN
            Close;
            IF Menu.activated THEN HandleMenu;
         END;
       IF Running THEN
       BEGIN
          FOR N := 1 to Number DO
          BEGIN
             Term :=  (1 + (Mu - 1)*Pressure/760);
             Delta1.put(N,pi +2*pi*Term*2*CellLength*1e9*
                          cos(rad*Theta.Value(N))/Lambda.Value(1));
             Intensity1.put(N,sqr( cos(Delta1.Value(N)/2) ) );
             Theta.put(N+1,Theta.Value(N) + AngleRange/Number);
          END;
          PlotDvectors(Theta,Intensity1,1,Number);
          Pressure := Pressure + PressureIncrement;
          RubOut(11,3,10,Black);
          AString := concat('Pressure = ',NUmStr(Pressure,4,2),' mm Hg');
          Print(1,3,AString);
          SetColor(white);
          RubOut(26,1,15,Black);
          Order := 2*Celllength*1e9/Lambda.Value(1);
          AString := concat(' Initial Central Fringe = ',NUmStr(Order,4,2));
          Print(1,1,AString);
          RubOut(26,2,15,Black);
          Order := 2*(1+((Mu-1)*Pressure/760))*Celllength*1e9/Lambda.Value(1);
          AString := concat('   Final Central Fringe = ',NUmStr(Order,4,2));
          Print(1,2,AString);
          IF StopFlag THEN
            BEGIN
               IF NOT Picture.FirstPage THEN
               BEGIN
                  DynamicPause;
               END;
            END;
          Picture.SwitchandClean;
          SetColor(white);
          RubOut(26,1,15,Black);
          Order := 2*Celllength*1e9/Lambda.Value(1);
          AString := concat(' Initial Central Fringe = ',NUmStr(Order,4,2));
          Print(1,1,AString);
          RubOut(26,2,15,Black);
          Order := 2*(1+((Mu-1)*Pressure/760))*Celllength*1e9/Lambda.Value(1);
          AString := concat('   Final Central Fringe = ',NUmStr(Order,4,2));
          Print(1,2,AString);
       END;
    END;
    IF Running THEN Close;
    Intensity1.free;
    Intensity2.free;
    TotalIntensity.free;
    Delta1.free;
    Delta2.free;
    Theta.free;
END;

PROCEDURE TMichelsonGasIndex.Close;
VAR AString               : String;
BEGIN
    Picture.Close;
    PlotDvectors(Theta,Intensity1,1,Number);
    RubOut(11,3,10,Black);
    AString := concat('Pressure = ',NUmStr(Pressure,4,2),' mm Hg');
    Print(1,3,AString);
    Running := False;
END;

PROCEDURE TMichelsonGasIndex.Reset;
BEGIN
   IF Active THEN
   BEGIN
      GraphMode := VgaHi;
      SetGraphMode(GraphMode);
      ShowMouse;
      Menu.Display;
      {MainHotKeys.Display;}
   END;
END;


{**************************  MAIN PROGRAM  **************************}
BEGIN
   CUPSinit;
   SetUpMenu;
   SetUpMainHotKeys;
   ShowHelp('spectrum.hlp','HELPSCREEN9');
   Start;
   REPEAT
      CheckForEvents;
      CheckMenu;
      CheckMouse;
      CheckHotKeys;
      CheckUnitHotKeys;
   UNTIL QUITFlag;
   Menu.done;
   CUPSdone;
END.




