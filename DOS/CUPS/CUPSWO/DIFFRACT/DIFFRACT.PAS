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
{  *             December 5th 1994   DIFFRACT.PAS            *  }
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
{  *               Fraunhofer Diffraction                    *  }
{  *            a) Slit or Set of Slits                      *  }
{  *            b) Rectangular Aperture                      *  }
{  *            c) Circular perture                          *  }
{  *                                                         *  }
{  *                  Fresnel Diffraction                    *  }
{  *            a) Slit or Set of Slits                      *  }
{  *            b) Point Source or Line of Sources           *  }
{  *                                                         *  }
{  *               THE CORNU SPIRAL                          *  }
{  *         a)  Study of the Spiral                         *  }
{  *         b)  Diffraction from an Edge                    *  }
{  *         c)  Diffraction from a Slit                     *  }
{  *         d)  Diffraction from an Obstacle                *  }
{  *         e)  Diffraction from Two Slits                  *  }
{  *         f)  Diffraction from Two Obstacles              *  }
{  *                                                         *  }
{  *                     Coherence                           *  }
{  *            a) One Slit - Two Sources                    *  }
{  *            b) Two Slits - Two Incoherent Sources        *  }
{  *            c) Two Slits - Line Source                   *  }
{  *            d) Double Stars                              *  }
{  *            e) Star Diameter                             *  }
{  ***********************************************************  }

PROGRAM DIFFRACTION2;

USES graph, crt, CUPS, CUPSmupp, CUPSgui, CUPSgrph,
     CUPSproc,CUPSFunc,DiffUnit;

TYPE
   TMultipleSlits = object
      Active : Boolean;
      PROCEDURE Init;
      PROCEDURE SetDefaults;
      PROCEDURE SetUp;
      PROCEDURE Done;
   PRIVATE
      SlitWidth,SlitSep,alpha,Beta,Term,Max,Min,x1,
      AngleRange,NewA,NewB, AngleMin                : Real;
      NumberofSlits,NumberofPoints,
      IMin,IMax,ColNum,FirstX,FirstY                : Integer;
      Lambda,Intensity,XValues                      : DVector;
      Slider1,Slider2,Slider3                       : TSliders;
      HotKeys                                       : THotKeys;
      SlitButtons                                   : TButtons;
      FirstTime,InViewPort,Cleared                  : Boolean;
      PROCEDURE SetUpSlidersandButtons;
      PROCEDURE HandleSliders1;
      PROCEDURE HandleSliders2;
      PROCEDURE HandleSliders3;
      PROCEDURE HandleButtons;
      PROCEDURE SetUpHotKeys;
      PROCEDURE HandleHotKeys(KeyNum : Byte);
      PROCEDURE CheckMouse;
      PROCEDURE Plot;
   END;

   TRectangular = object
      Active,Plotted     : Boolean;
      PROCEDURE Init;
      PROCEDURE SetDefaults;
      PROCEDURE SetUp;
      PROCEDURE Done;
   PRIVATE
      Horwidth,Vertht,ScreenDistance,XCenter,
      YCenter,Max,I,Limit,XMax,YMax,YMin,XMin          : Real;
      NumberofPoints,vx1,vx2,vy1,vy2                   : Integer;
      Lambda                                           : Dvector;
      a,b                                              : PointType;
      Sliders                                          : TSliders;
      HotKeys                                          : THotkeys;
      PROCEDURE SetUpSliders;
      PROCEDURE HandleSliders;
      PROCEDURE SetUpHotKeys;
      PROCEDURE HandleHotKeys(KeyNum : Byte);
      PROCEDURE CheckMouse;
      PROCEDURE Screen;
      PROCEDURE Plot;
   END;

 TCircular = object
      Active,Plotted : Boolean;
      PROCEDURE Init;
      PROCEDURE SetDefaults;
      PROCEDURE SetUp;
      PROCEDURE Done;
   PRIVATE
      Radius,ScreenDistance,XCenter,YCenter,
      Max,I,Limit,XMax,YMax,YMin,XMin          : Real;
      NumberofPoints,vx1,vx2,vy1,vy2           : Integer;
      a,b                                      : PointType;
      Lambda                                   : Dvector;
      Sliders                                  : TSliders;
      HotKeys                                  : THotkeys;
      PROCEDURE SetUpSliders;
      PROCEDURE HandleSliders;
      PROCEDURE SetUpHotKeys;
      PROCEDURE HandleHotKeys(KeyNum : Byte);
      PROCEDURE CheckMouse;
      PROCEDURE Screen;
      PROCEDURE Plot;
   END;


 TCoherenceCircle = object
        Active          : Boolean;
        PROCEDURE Init;
        PROCEDURE SetDefaults;
        PROCEDURE SetUp;
        PROCEDURE Done;
    PRIVATE
        Radius,XMax,XMin,ScreenDistance,
        SourceDistance1,SourceDistance2,
        SourceDistancefromSlits,XMinAngle,
        YMinAngle,SourceSeparation,NewA,NewB           :  Real;
        FirstY,FirstX                                  :  Integer;
        InViewPort                                     :  Boolean;
        Sliders                                        :  TSliders;
        Buttons                                        :  TButtons;
        Lambda                                         :  DVector;
        Circle,Slit                                    :  DMatrix;
        ResolutionPlot                                 :  TPlot3D;
        HotKeys                                        :  THotkeys;
        PROCEDURE SetUpSlidersandButtons;
        PROCEDURE HandleSliders;
        PROCEDURE HandleButtons;
        PROCEDURE SetUpHotKeys;
        PROCEDURE HandleHotKeys(KeyNum : Byte);
        PROCEDURE CheckMouse;
        PROCEDURE OneCircle;
 END;

  TCoherenceSlit = object
        Active          : Boolean;
        PROCEDURE Init;
        PROCEDURE SetDefaults;
        PROCEDURE SetUp;
        PROCEDURE Done;
    PRIVATE
        SlitWidth,
        SlitSeparation,XMax,XMin,ScreenDistance,
        SourceDistance1,SourceDistance2,
        SourceDistancefromSlits,XMinAngle,
        YMinAngle,SourceSeparation,NewA,NewB           :  Real;
        FirstY,FirstX                                  :  Integer;
        InViewPort                                     :  Boolean;
        Sliders                                        :  TSliders;
        Buttons                                        :  TButtons;
        Lambda                                         :  DVector;
        Slit                                           :  DMatrix;
        ResolutionPlot                                 :  TPlot3D;
        HotKeys                                        :  THotkeys;
        PROCEDURE SetUpSliders;
        PROCEDURE HandleSliders;
        PROCEDURE SetUpHotKeys;
        PROCEDURE HandleHotKeys(KeyNum : Byte);
        PROCEDURE CheckMouse;
        PROCEDURE OneSlit;
 END;

 TCoherenceTwo = object
        Active             : Boolean;
        PROCEDURE Init;
        PROCEDURE SetDefaults;
        PROCEDURE SetUp;
        PROCEDURE Done;
    PRIVATE
        SlitWidth,SlitSeparation,SourceDistance1,
        SourceDistance2,XMinAngle,SourceSeparation,
        ScreenDistance                                 :  Real;
        Sliders                                        :  TSliders;
        ZoomSourceButtons,ZoomSlitButtons              :  TButtons;
        Lambda                                         :  DVector;
        ResolutionPlot                                 :  TPlot3D;
        HotKeys                                        :  THotkeys;
        PROCEDURE SetUpSlidersandButtons;
        PROCEDURE HandleSliders;
        PROCEDURE SetUpHotKeys;
        PROCEDURE HandleHotKeys(KeyNum : Byte);
        PROCEDURE CheckMouse;
        PROCEDURE TwoSources;
 END;

 TCoherenceLine = object
        Active             : Boolean;
        PROCEDURE Init;
        PROCEDURE SetDefaults;
        PROCEDURE SetUp;
        PROCEDURE Done;
    PRIVATE
        SlitWidth,
        SlitSeparation,XMax,XMin,ScreenDistance,
        SourceTop,SourceBottom,NewC,NewF,NewA,NewD,
        SourceDistancefromSlits,XMinAngle,
        SourceWidth,DeltaTheta1                        : Real;
        FirstX,FirstY                                  : Integer;
        InUpperViewPort,InLowerViewPort                : Boolean;
        Sliders                                        : TSliders;
        ZoomSourceButtons,ZoomSlitButtons              : TButtons;
        Lambda                                         : DVector;
        ResolutionPlot                                 : TPlot3D;
        HotKeys                                        : THotkeys;
        PROCEDURE SetUpSlidersandButtons;
        PROCEDURE HandleSliders;
        PROCEDURE SetUpHotKeys;
        PROCEDURE HandleHotKeys(KeyNum : Byte);
        PROCEDURE CheckMouse;
        PROCEDURE LineSource;
 END;

 TStellarTwo = object
        Active             : Boolean;
        PROCEDURE Init;
        PROCEDURE SetDefaults;
        PROCEDURE SetUp;
        PROCEDURE Done;
    PRIVATE
        SlitWidth,SlitSeparation,ScreenDistance,
        XMax,XMin,StarAngle,DistancealphaC,Arc,
        DistanceCastor,DistanceKruger60,
        DistancepEri                                   :  Real;
        Sliders                                        :  TSliders;
        Buttons,ZoomButtons                            :  TButtons;
        Lambda                                         :  DVector;
        HotKeys                                        :  THotkeys;
        PROCEDURE SetUpSlidersandButtons;
        PROCEDURE HandleSliders;
        PROCEDURE SetUpHotKeys;
        PROCEDURE HandleHotKeys(KeyNum : Byte);
        PROCEDURE CheckMouse;
        PROCEDURE TwoSources;
 END;

 TStellarDiameter = object
        Active             : Boolean;
        PROCEDURE Init;
        PROCEDURE SetDefaults;
        PROCEDURE SetUp;
        PROCEDURE Done;
    PRIVATE
        SlitWidth,StarAngle,SlitSeparation,XMax,XMin,
        ScreenDistance,XMinAngle,DeltaTheta1,NewA,
        DistanceBetelgeuse,DistanceAldeberan,
        DistanceArcturus                               :  Real;
        Buttons,ZoomButtons                            :  TButtons;
        Sliders                                        :  TSliders;
        Lambda                                         :  DVector;
        ResolutionPlot                                 :  TPlot3D;
        HotKeys                                        :  THotkeys;
        PROCEDURE SetUpSlidersandButtons;
        PROCEDURE HandleSliders;
        PROCEDURE SetUpHotKeys;
        PROCEDURE HandleHotKeys(KeyNum : Byte);
        PROCEDURE CheckMouse;
        PROCEDURE Diameter;
 END;


{*************************  VARIABLES  *******************************}
 VAR
   Menu                                          : TMenu;
   MainHotKeys                                   : THotKeys;
   QuitFlag                                      : Boolean;
   KeyNum                                        : Byte;
   MultipleSlits                                 : TMultipleSlits;
   Rectangular                                   : TRectangular;
   Circular                                      : TCircular;
   CoherenceCircle                               : TCoherenceCircle;
   CoherenceSlit                                 : TCoherenceSlit;
   CoherenceTwo                                  : TCoherenceTwo;
   CoherenceLine                                 : TCoherenceLine;
   StellarTwo                                    : TStellarTwo;
   StellarDiameter                               : TStellarDiameter;

 {------------------------- INDEPENDENT FUNCTION ----------------------}

PROCEDURE MenuClearCheck;
VAR I,J: Integer;
BEGIN
   FOR I := 1 to 16 DO
   BEGIN
      FOR J := 1 to 5 DO Menu.RowChecked(J,I,False);
   END;
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
      Column(2,'Huyghens');
        Row(2,1,'Slits');
        RowChecked(2,1,True);
        Row(2,2,'Point Sources');
      Column(3,'Cornu');
        Row(3,1,'Study the Cornu Spiral');
        Row(3,2,'Edge');
        Row(3,3,'Single Slit');
        Row(3,4,'Single Obstacle');
        Row(3,5,'Double Slit');
        Row(3,6,'Double Obstacle');
      Column(4,' Fraunhof');
        Row(4,1,'Single Slit and Multiple Slits');
        Row(4,2,'Rectangular Aperture');
        Row(4,3,'Circular Aperture');
      Column(5,'Coherence');
        Row(5,1,'Circular Aperture');
        Row(5,2,'Slit Aperture');
        Row(5,3,'Two Slits - Two Line Sources');
        Row(5,4,'Two Slits - One Line Source');
        Row(5,5,'Double Star');
        Row(5,6,'Star Diameter');
      Display
     END;
END;

PROCEDURE SetUpHotKeys;
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
   QuitFlag                  := False;
   Circular.Active           := False;
   Rectangular.Active        := False;
   MultipleSlits.Active      := False;
   PointsDiffraction.Active  := False;
   SlitsDiffraction.Active   := False;
   CoherenceCircle.Active    := False;
   CoherenceSlit.Active      := False;
   CoherenceTwo.Active       := False;
   CoherenceLine.Active      := False;
   StellarTwo.Active         := False;
   StellarDiameter.Active    := False;
   CornuSpiral.Active        := False;
   CornuEdge.Active          := False;
   CornuSlit.Active          := False;
   CornuObstacle.Active      := False;
   CornuTwoSlit.Active       := False;
   CornuTwoObstacle.Active   := False;
   SlitsDiffraction.F10Used  := False;
   PointsDiffraction.F10Used := False;
   CornuSpiral.F10Used       := False;
   CornuEdge.F10Used         := False;
   CornuSlit.F10Used         := False;
   CornuObstacle.F10Used     := False;
   CornuTwoSlit.F10Used      := False;
   CornuTwoObstacle.F10Used  := False;
END;

PROCEDURE CleanUp;
BEGIN
   IF MultipleSlits.Active     THEN MultipleSlits.done;
   IF Rectangular.Active       THEN Rectangular.Done;
   IF Circular.Active          THEN Circular.Done;
   IF CoherenceCircle.Active   THEN CoherenceCircle.done;
   IF CoherenceSlit.Active     THEN CoherenceSlit.done;
   IF CoherenceTwo.Active      THEN CoherenceTwo.done;
   IF CoherenceLine.Active     THEN CoherenceLine.done;
   IF StellarTwo.Active        THEN StellarTwo.Done;
   IF StellarDiameter.Active   THEN StellarDiameter.Done;
   IF SlitsDiffraction.Active  THEN SlitsDiffraction.done;
   IF PointsDiffraction.Active THEN PointsDiffraction.done;
   IF CornuSpiral.Active       THEN CornuSpiral.Done;
   IF CornuEdge.Active         THEN CornuEdge.Done;
   IF CornuSlit.Active         THEN CornuSlit.Done;
   IF CornuObstacle.Active     THEN CornuObstacle.Done;
   IF CornuTwoSlit.Active      THEN CornuTwoSlit.Done;
   IF CornuTwoObstacle.Active  THEN CornuTwoObstacle.Done;
   SetFlagsFalse;
END;

PROCEDURE Start;
BEGIN
   SetFlagsFalse;
   SlitsDiffraction.init;
END;

PROCEDURE HandleMenu;
VAR ColNum,RowNum : Integer;
BEGIN
   MenuClearCheck;
   WITH Menu DO
   CASE ColChosen OF
   1: CASE rowChosen OF
         1:   AboutCUPS;
         2:   ShowHelp('Diffract.hlp','HELPSCREEN9');
         3:   ShowHelp('Diffract.hlp','HELPSCREEN1');
         4:   Configuration;
         6:   QuitFlag := True;
      END; {of row 1 CASE}

   2: CASE rowChosen OF
         1: BEGIN
               CleanUp;
               SlitsDiffraction.Init;
            END;
         2: BEGIN
               CleanUp;
               PointsDiffraction.Init;
            END;
      END;

   3: CASE RowChosen OF
         1: BEGIN
               CleanUp;
               CornuSpiral.Init;
            END;
         2: BEGIN
               CleanUp;
               CornuEdge.Init;
            END;
         3: BEGIN
               CleanUp;
               CornuSlit.Init;
            END;
         4: BEGIN
               CleanUp;
               CornuObstacle.Init;
            END;
         5: BEGIN
               CleanUp;
               CornuTwoSlit.Init;
            END;
         6: BEGIN
               CleanUp;
               CornuTwoObstacle.Init;
            END;
      END; {of Row 4 CASE}

   4: CASE rowChosen OF
         1: BEGIN
               CleanUp;
               MultipleSlits.Init;
            END;
         2: BEGIN
               CleanUp;
               Rectangular.Init;
            END;
         3: BEGIN
               CleanUp;
               Circular.Init;
            END;
      END; {of row 2 CASE}

   5: CASE rowChosen OF
         1: BEGIN
               CleanUp;
               CoherenceCircle.Init;
            END;
         2: BEGIN
               CleanUp;
               CoherenceSlit.Init;
            END;
         3: BEGIN
               CleanUp;
               CoherenceTwo.Init;
            END;
         4: BEGIN
               CleanUp;
               CoherenceLine.Init;
            END;
         5: BEGIN
               CleanUp;
               StellarTwo.Init;
            END;
         6: BEGIN
               CleanUp;
               StellarDiameter.Init;
            END;
      END;
   END;
   Menu.RowChecked(Menu.ColChosen,Menu.RowChosen,True);
END;


PROCEDURE HandleHotKeys(keynum : byte);
BEGIN
   CASE KeyNum OF
   1:        IF CornuEdge.Active         THEN ShowHelp('diffract.hlp','HELPSCREEN22')
        ELSE IF CornuSlit.Active         THEN ShowHelp('diffract.hlp','HELPSCREEN23')
        ELSE IF CornuTwoSlit.Active      THEN ShowHelp('diffract.hlp','HELPSCREEN25')
        ELSE IF CornuObstacle.Active     THEN ShowHelp('diffract.hlp','HELPSCREEN24')
        ELSE IF CornuTwoObstacle.Active  THEN ShowHelp('diffract.hlp','HELPSCREEN26')
        ELSE IF SlitsDiffraction.Active  THEN ShowHelp('Diffract.hlp','HELPSCREEN5')
        ELSE IF PointsDiffraction.Active THEN ShowHelp('Diffract.hlp','HELPSCREEN4')
        ELSE IF MultipleSlits.Active     THEN ShowHelp('Diffract.hlp','HELPSCREEN6')
        ELSE IF Rectangular.Active       THEN ShowHelp('Diffract.hlp','HELPSCREEN7')
        ELSE IF Circular.Active          THEN ShowHelp('Diffract.hlp','HELPSCREEN8')
        ELSE IF CoherenceCircle.Active   THEN ShowHelp('Diffract.hlp','HELPSCREEN13')
        ELSE IF CoherenceSlit.Active     THEN ShowHelp('Diffract.hlp','HELPSCREEN14')
        ELSE IF CoherenceTwo.Active      THEN ShowHelp('Diffract.hlp','HELPSCREEN15')
        ELSE IF CoherenceLine.Active     THEN ShowHelp('Diffract.hlp','HELPSCREEN16')
        ELSE IF StellarTwo.Active        THEN ShowHelp('Diffract.hlp','HELPSCREEN17')
        ELSE IF StellarDiameter.Active   THEN ShowHelp('Diffract.hlp','HELPSCREEN18')
        ELSE ShowHelp('diffract.hlp','HELPSCREEN9');
   2: IF Menu.chosen then HandleMenu;
   END;
END;

PROCEDURE CheckHotKeys;
BEGIN
   IF (MultipleSlits.Active AND MultipleSlits.HotKeys.Pressed(KeyNum))
      THEN MultipleSlits.HandleHotKeys(KeyNum);
   IF (Circular.Active AND Circular.HotKeys.Pressed(KeyNum))
      THEN Circular.HandleHotKeys(KeyNum);
   IF (Rectangular.Active AND Rectangular.HotKeys.Pressed(KeyNum))
      THEN Rectangular.HandleHotKeys(KeyNum);
   IF (SlitsDiffraction.Active AND SlitsDiffraction.HotKeys.Pressed(KeyNum))
      THEN SlitsDiffraction.HandleHotKeys(KeyNum);
   IF (PointsDiffraction.Active AND PointsDiffraction.HotKeys.Pressed(KeyNum))
      THEN PointsDiffraction.HandleHotKeys(KeyNum);
   IF (CornuSpiral.Active AND CornuSpiral.HotKeys.Pressed(KeyNum))
      THEN CornuSpiral.HandleHotKeys(KeyNum);
   IF (CornuEdge.Active AND CornuEdge.HotKeys.Pressed(KeyNum))
     THEN CornuEdge.HandleHotKeys(KeyNum);
   IF (CornuSlit.Active AND CornuSlit.HotKeys.Pressed(KeyNum))
     THEN CornuSlit.HandleHotKeys(KeyNum);
   IF (CornuTwoSlit.Active AND CornuTwoSlit.HotKeys.Pressed(KeyNum))
     THEN CornuTwoSlit.HandleHotKeys(KeyNum);
   IF (CornuObstacle.Active AND CornuObstacle.HotKeys.Pressed(KeyNum))
     THEN CornuObstacle.HandleHotKeys(KeyNum);
   IF (CornuTwoObstacle.Active AND CornuTwoObstacle.HotKeys.Pressed(KeyNum))
     THEN CornuTwoObstacle.HandleHotKeys(KeyNum);
   IF (CoherenceCircle.Active AND CoherenceCircle.HotKeys.Pressed(KeyNum))
     THEN CoherenceCircle.HandleHotKeys(KeyNum);
   IF (CoherenceSlit.Active AND CoherenceSlit.HotKeys.Pressed(KeyNum))
     THEN CoherenceSlit.HandleHotKeys(KeyNum);
   IF (CoherenceTwo.Active AND CoherenceTwo.HotKeys.Pressed(KeyNum))
      THEN CoherenceTwo.HandleHotKeys(KeyNum);
   IF (CoherenceLine.Active AND CoherenceLIne.HotKeys.Pressed(KeyNum))
      THEN CoherenceLine.HandleHotKeys(KeyNum);
   IF (StellarTwo.Active AND StellarTwo.HotKeys.Pressed(KeyNum))
      THEN StellarTwo.HandleHotKeys(KeyNum);
   IF (StellarDiameter.Active AND StellarDiameter.HotKeys.Pressed(KeyNum))
      THEN StellarDiameter.HandleHotKeys(KeyNum);
   IF  NOT (MultipleSlits.Active
       OR Rectangular.Active
       OR Circular.Active
       OR SlitsDiffraction.Active
       OR PointsDiffraction.Active
       OR CornuSpiral.Active
       OR CornuEdge.Active
       OR CornuSlit.Active
       OR CornuTwoSlit.Active
       OR CornuObstacle.Active
       OR CornuTwoObstacle.Active
       OR CoherenceCircle.Active
       OR CoherenceSlit.Active
       OR CoherenceTwo.Active
       OR CoherenceLine.Active
       OR StellarTwo.Active
       OR StellarDiameter.Active)
       AND MainHotKeys.pressed(Keynum) THEN HandleHotKeys(keynum);
END;

PROCEDURE CheckMenu;
BEGIN
   IF Menu.activated THEN HandleMenu;
END;

PROCEDURE CheckMouse;
BEGIN
   SlitsDiffraction.CheckMouse;
   PointsDiffraction.CheckMouse;
   MultipleSlits.CheckMouse;
   Rectangular.CheckMouse;
   Circular.CheckMouse;
   CoherenceCircle.CheckMouse;
   CoherenceSlit.CheckMouse;
   CoherenceTwo.CheckMouse;
   CoherenceLine.CheckMouse;
   StellarTwo.CheckMouse;
   StellarDiameter.CheckMouse;
   CornuSpiral.CheckMouse;
   CornuEdge.CheckMouse;
   CornuSlit.CheckMouse;
   CornuObstacle.CheckMouse;
   CornuTwoSlit.CheckMouse;
   CornuTwoObstacle.CheckMouse;
END;

PROCEDURE CheckUnitHotKeys;
BEGIN
   IF SlitsDiffraction.F10Used THEN
   BEGIN
      SlitsDiffraction.F10Used := False;
      IF Menu.Chosen THEN HandleMenu;
   END;

   IF PointsDiffraction.F10Used THEN
   BEGIN
      PointsDiffraction.F10Used := False;
      IF Menu.Chosen THEN HandleMenu;
   END;

   IF CornuSpiral.F10Used THEN
   BEGIN
      CornuSpiral.F10Used := False;
      IF Menu.Chosen THEN HandleMenu;
   END;

   IF CornuSlit.F10Used THEN
   BEGIN
      CornuSlit.F10Used := False;
      IF Menu.Chosen THEN HandleMenu;
   END;

   IF CornuObstacle.F10Used THEN
   BEGIN
      CornuObstacle.F10Used := False;
      IF Menu.Chosen THEN HandleMenu;
   END;

   IF CornuTwoObstacle.F10Used THEN
   BEGIN
      CornuTwoObstacle.F10Used := False;
      IF Menu.Chosen THEN HandleMenu;
   END;

   IF CornuTwoSlit.F10Used THEN
   BEGIN
      CornuTwoSlit.F10Used := False;
      IF Menu.Chosen THEN HandleMenu;
   END;

   IF CornuEdge.F10Used THEN
   BEGIN
      CornuEdge.F10Used := False;
      IF Menu.Chosen THEN HandleMenu;
   END;
END;

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

FUNCTION BessJ1(Ex : Real) : Real ;        {calculate Bessel function}
 VAR
     ax, xx, z: Real;
     y, ans,ans1, ans2: Double;

BEGIN
   IF Abs(Ex) < 8.0 THEN
   BEGIN
      y := sqr(Ex);
      ans1 := Ex*(72362614232.0+y*(-7895059235.0+y*(242396853.1+
              y*(-2972611.439+y*(15704.48260+y*(-30.16036606))))));
      ans2 := 144725228442.0+y*(2300535178.0+
              y*(18583304.74+y*(99447.43394+y*(376.9991397+y*1.0))));
      BessJ1 := ans1/ans2
   END
   ELSE
   BEGIN
      ax := Abs(Ex);
      z := 8.0/ax;
      y := Sqr(z);
      xx := ax-2.356194491;
      ans1 := 1.0+y*(0.183105e-2+y*(-0.3516396496e-4+
              y*(0.2457520174e-5+y*(-0.240337019e-6))));
      ans2 := 0.04687499995+y*(-0.2002690873e-3+y*(0.8449199096e-5+
              y*(-0.88228987e-6+y*0.105787412e-6)));
      ans := sqrt(0.636619772/ax)*(Cos(xx)*ans1-z*Sin(xx)*ans2);
      IF Ex < 0.0 THEN
        ans := -ans;
      BessJ1 := ans;
   END;
END;

FUNCTION Simpson (y : dVector) : real;
var
  i,n,s : integer;
  x : real;
begin
{$R-}
   if y.length =1 then Simpson := y.VecPtr^[1] else
   begin
      s := 2*((y.size+1) div 2)-1;
      i := 2;
      if s=1 then x := 0 else x := y.VecPtr^[1] + 4*y.VecPtr^[i] + y.VecPtr^[s];
      i := 4;
      while i<s do
      begin
         x := x + 2*y.VecPtr^[i-1] + 4*y.VecPtr^[i];
         i := i + 2;
      end;
      if s<>y.size then Simpson := x/3 + (y.VecPtr^[s]+y.VecPtr^[y.size])/2
      else Simpson := x/3;
   end;
{$R+}
end;

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

PROCEDURE Square(a,b : PointType); far;
BEGIN
   WITH Views[3] DO
   BEGIN
      HideCursor;
      Rectangle(b.x - round(0.1*(vx2-vx1)),b.y - round(0.1*(vy2-vy1)),
                b.x + round(0.1*(vx2-vx1)),b.y + round(0.1*(vy2-vy1)));
      ShowCursor;
   END;
END;

FUNCTION NewSimpson (y : dVector; Number : Integer) : real;
var
  i,n,s : integer;
  x : real;
begin
{$R-}
   if y.length =1 then NewSimpson := y.VecPtr^[1] else
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
{R+}
end;

   {------------------------- TMultipleSlits --------------------------------------}
PROCEDURE TMultipleSlits.Init;
BEGIN
   Lambda.init(1);
   Slider1.init;
   Slider2.init;
   Slider3.init;
   SlitButtons.init;
   SetDefaults;
END;

PROCEDURE TMultipleSlits.Done;
BEGIN
   Lambda.free;
   Slider1.Done;
   Slider2.Done;
   Slider3.Done;
   SlitButtons.Done;
END;

PROCEDURE TMultipleSlits.SetDefaults;
BEGIN
   SlitWidth      := 0.02;   (* Slit width in millimetres *)
   SlitSep        := 0.2;    (* Slit separation in millimetres *)
   AngleRange     := 2.5;
   FirstTime      := True;
   FirstX         := 0;
   FirstY         := 0;
   InViewPort     := False;
   Cleared        := False;
   GraphBackColor := Black;
   SetUp;
END;

PROCEDURE TMultipleSlits.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(1,0.1,0.9,0.40,0.85);  {Diffraction window}
   DefineViewPort(3,0,1,0.3,0.95);
   Lambda.put(1,550);
   SetUpHotKeys;
   SetUpSlidersandButtons;
   IF Active THEN
   BEGIN
      Slider1.DrawAll;
      IF SlitButtons.Number > 1 THEN Slider2.DrawAll;
      Slider3.DrawAll;
   END;
   Active := true;
   Plot;
END;

PROCEDURE TMultipleSlits.SetUpSlidersandButtons;
BEGIN
   With Slider1 DO
   BEGIN
      create(1,0.0001,0.3,SlitWidth,0.05,0.17,0.48,4,'0.0001','0.3','Width mm',False);
      create(2,400,800,Lambda.Value(1),0.52,0.17,0.95,0,'400','800','Wavelength nm',False);
   END;

   With Slider2 DO
   BEGIN
      create(1,0.01,0.5,SlitSep,0.05,0.1,0.48,4,'0.01','0.5','Separation mm',False);
   END;

   With Slider3 DO
   BEGIN
      create(1,0.001,5,AngleRange,0.52,0.1,0.95,3,'0.001','5',' Angular Range deg',False);
   END;

   WITH SlitButtons DO
   BEGIN
      create(1,0.27,0.27,'');
      create(2,0.32,0.27,'');
      create(3,0.37,0.27,'');
      create(4,0.42,0.27,'');
      create(5,0.47,0.27,'');
      create(6,0.52,0.27,'');
      create(7,0.57,0.27,'');
      create(8,0.62,0.27,'');
      create(9,0.67,0.27,'');
      create(10,0.72,0.27,'');
      SlitButtons.Number := 2;
   END;
END;

PROCEDURE TMultipleSlits.HandleSliders1;
BEGIN
   REPEAT
      CheckForEvents;
      Delay(20);
      SlitWidth := Slider1.Value(1);
      Lambda.put(1,Slider1.Value(2));
   UNTIL NOT Slider1.Changed;
   Plot;
END;

PROCEDURE TMultipleSlits.HandleSliders2;
BEGIN
   REPEAT
      CheckForEvents;
      Delay(20);
      IF SlitButtons.Number <> 1 THEN SlitSep := Slider2.Value(1);
   UNTIL NOT Slider2.Changed;
   Plot;
END;

PROCEDURE TMultipleSlits.HandleSliders3;
BEGIN
   REPEAT
      CheckForEvents;
      Delay(20);
      AngleRange := Slider3.Value(1);
   UNTIL NOT Slider3.Changed;
   Plot;
END;

PROCEDURE TMultipleSlits.HandleButtons;
BEGIN
   IF SlitButtons.Number = 1 THEN
   BEGIN
      Slider2.erase(1,Black);
      Slider2.delete(1);
   END
   ELSE
   BEGIN
      Slider2.create(1,0.01,0.5,SlitSep,0.05,0.1,0.45,4,'0.01','0.5','Separation mm',False);
      Slider2.DrawAll;
   END;
   Plot;
END;

PROCEDURE TMultipleSlits.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(5);
      Key[1] := 'F1-Help';
      Key[2] := 'F2-LastPlot';
      Key[3] := 'F3-Clear All';
      Key[4] := 'F9-Defaults';
      Key[5] := 'F10-Menu';
   END;
END;

PROCEDURE TMultipleSlits.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN6');
   2: BEGIN
         CloseViewPort(1);
         OpenViewPort(1);
         RubOutXY(0.05,0.39,70,Black);
         AngleRange := Slider3.Value(1);
         AngleMin := -AngleRange/2;
         Definescale(1,AngleMin,AngleMin + AngleRange,0,1.1);
         SelectScale(1);
         Axis(0,0,TickSpace(AngleRange),0.2);
         FirstTime := True;
         Plot;
      END;
   3: BEGIN
         IF NOT Cleared THEN
         BEGIN
            CloseViewPort(1);
            OpenViewPort(1);
            RubOutXY(0.05,0.39,70,Black);
            AngleRange := Slider3.Value(1);
            AngleMin := -AngleRange/2;
            Definescale(1,AngleMin,AngleMin + AngleRange,0,1.1);
            SelectScale(1);
            Axis(0,0,TickSpace(AngleRange),0.2);
            FirstTime := True;
            Cleared := True;
         END;
      END;
   4:  BEGIN
          Done;
          init;
       END;
   5: IF Menu.Chosen THEN HandleMenu;
   END;
END;

PROCEDURE TMultipleSlits.CheckMouse;
VAR  X,Y,Button                   : Integer;
BEGIN
   IF Active THEN
   BEGIN
      IF Slider1.Changed  THEN HandleSliders1;
      IF Slider2.Changed  THEN HandleSliders2;
      IF Slider3.Changed  THEN
      BEGIN
         CloseViewPort(1);
         OpenViewPort(1);
         HandleSliders3;
         Plot;
      END;

      IF SlitButtons.Changed THEN HandleButtons;

      BEGIN
         MouseGlobalPosn(x,y,button);
         WITH Views[1] DO
         BEGIN
            IF(x>vx1) AND (x<vx2) AND(y>vy1) AND(y<vy2) THEN
            BEGIN
               SelectViewPort(1);
               NewA := 1.1*(vy2 - Y)/(vy2 - vy1);
               NewB := (X - vx1)/(vx2 - vx1)*AngleRange - AngleRange/2;
               IF (Y <> FirstY) OR (X <> FirstX) THEN
               BEGIN
                  Print(45 ,1,'Intensity ');
                  Print(45 ,2,'    Angle ');
                  Print2(56,1,2,NewA,lightgreen,Black);
                  Print2(56,2,2,NewB,lightgreen,Black);
                  FirstX := X;
                  FirstY := Y;
                  SetColor(white);
                  InViewPort := True
               END;
            END
            ELSE
            BEGIN
               IF InViewPort THEN
               BEGIN
                  SelectViewPort(1);
                  RubOut(45,1,19,Black);
                  RubOut(45,2,19,Black);
               END;
               InViewPort := False;
            END;
         END;
      END;
   ENd;
END;


PROCEDURE TMultipleSlits.Plot;
VAR N : Integer;
BEGIN
   Cleared := false;
   Slider1.DrawAll;
   Slider3.DrawAll;
   IF SlitButtons.Number > 1 THEN Slider2.DrawAll;
   SlitButtons.DrawAll;
   PrintXY(0.26,0.31,'            NUMBER OF SLITS');
   PrintXY(0.26,0.24,'1   2   3   4   5   6   7   8   9   10');
   MainHotKeys.Clear;
   HotKeys.Display;
   NumberofPoints := 500;
   Intensity.init(NumberOfPoints);
   XValues.init(NumberOfPoints);
   AngleMin := -AngleRange/2;
   Definescale(1,AngleMin,AngleMin + AngleRange,0,1.1);
   SelectScale(1);
   Comment(0.15,0.925,'Use Mouse to measure Intensities inside the Viewport');
   x1 := pi*AngleMin/180;
   FOR N := 1 to NumberofPoints DO
   BEGIN
      alpha := x1*pi*SlitWidth/(1e-6*Lambda.Value(1));
      Beta  := x1*pi*SlitSep/(1e-6*Lambda.Value(1));
      Term := sqr(Sin(alpha)/alpha)*sqr(Sin(SlitButtons.Number*Beta)/Sin(Beta));
      Intensity.put(N,Term);
      XValues.put(N,x1*180/pi);
      x1 := x1 + pi/180*AngleRange/(NumberofPoints -1);
   END;
   Intensity.MinMax(IMin,Min,IMax,Max);
   FOR N := 1 to NumberofPoints DO
      Intensity.put(N,Intensity.Value(N)/Max);
   IF FirstTime THEN OpenViewPort(1);
   RubOutXY(0.05,0.39,70,Black);
   Axis(0,0,TickSpace(AngleRange),0.2);
   PutLabel(left,'Intensity');
   PutLabel(bottom,'Angle - degrees');
   FirstTime := False;
   ColNum := 1;
   Colours(ColNum,Lambda);
   PlotDvectors(XValues,Intensity,1,XValues.size);
   SetColor(white);
   Intensity.free;
   XValues.free;
END;

{--------------------------- TRectangular -------------------------}
PROCEDURE TRectangular.Init;
BEGIN
   Lambda.init(1);
   Sliders.init;
   SetDefaults;
END;

PROCEDURE TRectangular.Done;
BEGIN
   Lambda.Free;
   Sliders.Done;
END;

PROCEDURE TRectangular.SetDefaults;
BEGIN
   Limit          := 0.1;
   Lambda.put(1,550);
   XCenter        := 0;
   YCenter        := 0;
   ScreenDistance :=  1;
   Horwidth       :=  0.02;   (* aperture width in nanometres *)
   Vertht         :=  0.02;   (* aperture height in nanometres *)
   Limit          := 0.1;
   GraphBackColor := Black;
   SetUp;
END;

PROCEDURE TRectangular.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(1,0.1,0.40,0.09,0.49);   {Contour Plot}
   DefineViewPort(11,0,0.43,0.05,0.5);  {Blackout Contour Scales}
   DefineViewPort(2,0.52,0.99,0.06,0.66);   {3D Plot}
   DefineViewPort(3,0.17,0.35,0.53,0.78);    {dummy Screen}
   DefineViewPort(4,0,0.52,0.81,0.94);
   SetUpSliders;
   SetUpHotKeys;
   IF Active THEN
   WITH Views[3] DO
   BEGIN
      b.x := round(0.5*(vx2-vx1));
      b.y := round(0.5*(vy2-vy1));
      Screen;
   END;
   Plot;
END;

PROCEDURE TRectangular.SetUpSliders;
BEGIN
   With Sliders DO
   BEGIN
      create(1,0.01,1,Limit,0.55,0.90,0.95,2,'0.01','1','Threshold',False);
      create(2,0.005,0.05,HorWidth,0.55,0.83,0.95,3,'0.005','0.05','Width mm',False);
      create(3,0.005,0.05,VertHt,0.55,0.76,0.95,3,'0.005','0.05','Height mm',False);
      create(4,400,800,Lambda.Value(1),0.55,0.69,0.95,0,'400','800','Wavelength nm',False);
   END;
END;

PROCEDURE TRectangular.HandleSliders;
BEGIN
   Limit    := Sliders.Value(1);
   HorWidth := Sliders.Value(2);
   VertHt   := Sliders.Value(3);
   Lambda.put(1,Sliders.Value(4));
   Plot;
END;

PROCEDURE TRectangular.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TRectangular.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN7');
   2: BEGIN
          Done;
          init;
      END;
   3: IF Menu.Chosen THEN HandleMenu;
   END;
END;


PROCEDURE TRectangular.Screen;
VAR AString        : String;
BEGIN
   SetColor(white);
   OpenViewPort(3);
   DefineScale(3,-0.5,0.5,-0.5,0.5);
   Axis(0,0,0.5,0.5);
   SetColor(LightRed);
   Square(a,b);
   SelectViewPort(4);
   SetColor(white);
   AString := concat('   Current Center: X = ',NumStr(XCenter,1,2),
                      ' Y = ',NumStr(YCenter,1,2));
   RubOut(20,3,20,blue);
   Print(1,3,AString);
END;

PROCEDURE TRectangular.CheckMouse;
VAR N,IntX,IntY,XX,YY,Button,v1,v2,v3,v4               : Integer;
    x1,XMouse,wx,wy,Term,xLength,X,Y                   : Real;
    c                                                  : PointType;
    OK                                                 : Boolean;
    AString                                            : String;
BEGIN
   IF Active THEN
   BEGIN
      IF Sliders.Changed THEN HandleSliders;

      IF (Event.MouseClicked
           AND PointInside(Event.X,Event.Y,3)) THEN
      WITH Views[3] DO
      BEGIN
         SelectViewPort(3);
         SelectScale(3);
         c.x := b.x;
         c.y := b.y;
         v1 := round(b.x + round(0.1*(vx2-vx1)));
         v2 := round(b.x - round(0.1*(vx2-vx1)));
         v3 := round(b.y + round(0.1*(vy2-vy1)));
         v4 := round(b.y - round(0.1*(vy2-vy1)));
         REPEAT
           MouseGlobalPosn(XX,YY,button);
           OK := (   ((XX - vx1) < v1)
                 AND ((XX - vx1) > v2)
                 AND ((YY - vy1) < v3)
                AND  ((YY - vy1) > v4) );
         UNTIL  (button<>0) AND OK ;
         b.x := XX -vx1;
         b.y := YY - vy1;
         SetColor(Black);
         hideCursor;
         Square(a,c);
         Square(a,b);
         ShowCursor;
         SetColor(Lightred);
         with views[3] do
          MouseBorders(vx1,vy1,vx2,vy2);
         DrawRubberBand(Square,a,b,Black,true);
         MouseBorders(0,0,GetMaxX,GetMaxY);
         SetColor(white);
         Axis(0,0,0.5,0.5);
         SetColor(LightRed);
         Square(a,b);
         SetColor(white);
         SelectViewPort(4);
         XCenter := round(100*(b.x/(vx2-vx1)-0.5))/100;
         YCenter := round(100*(0.5 - b.y/(vy2-vy1)))/100;
         AString := concat('   Current Center: X = ',NumStr(Xcenter,1,2),
                            ' Y = ',NumStr(YCenter,1,2));
         RubOut(20,3,20,blue);
         Print(1,3,AString);
         Plot;
      END;
   END;
END;

PROCEDURE TRectangular.Plot;
VAR AString1,AString2,AString3,AString4           :  String;
    x1,y1,yLength,Min,wx,wy,Term,
    xLength,SinTheta,SinPhi                       :  Real;
    Contours                                      :  DMatrix;
    X,Y,MinRow,MinCol,MaxRow,MaxCol,ColNum        :  Integer;
    Graph3D                                       :  TGraph3D;
    Contour                                       :  TContour;
    AString                                       :  String;
BEGIN
   IF NOT Active THEN
   BEGIN
      OpenViewPort(3);
      DefineScale(3,-0.5,0.5,-0.5,0.5);
      Axis(0,0,0.5,0.5);
      SetColor(lightred);
      WITH Views[3] DO
      BEGIN
         b.x := round(0.5*(vx2-vx1));
         b.y := round(0.5*(vy2-vy1));
         Square(a,b);
         SetColor(white);
         SelectViewPort(4);
         MakeBlueBox;
         Print(1,1,' Click and Drag Mouse in ViewPort below ');
         Print(1,2,'  to change position of Viewing Screen ');
         AString1 := concat('   Current Center: X = ',NumStr((b.x/(vx2-vx1)-0.5),1,2),
                            ' Y = ',NumStr((0.5 - b.y/(vy2-vy1)),1,2));
         Print(1,3,AString1);
         XCenter := round(100*(b.x/(vx2-vx1)-0.5))/100;
         YCenter := round(100*(0.5 - b.y/(vy2-vy1)))/100;
      END;
   END;
   Active := True;
   Sliders.DrawAll;
   MainHotKeys.Clear;
   HotKeys.Display;
   NumberofPoints := 40;
   Contours.init(NumberofPoints,NumberofPoints);
   OpenViewPort(2);
   SetColor(white);
   XMin := XCenter - 0.1;
   XMax :=  XMin + 0.2;
   ymin :=  YCenter - 0.1;
   ymax := ymin + 0.2;
   x1 := XMin;
   y1 := ymax;
   xLength := 0.2;   { x screensize }
   yLength := 0.2;   { y screensize }
   FOR X := 1 to NumberofPoints  DO            { x loop }
   BEGIN
      FOR Y := 1 to NumberofPoints  DO         { y loop }
      BEGIN
         SinTheta := x1/sqrt((sqr(x1) + sqr(ScreenDistance)));
         wx := pi*SinTheta/(Lambda.Value(1)*1e-6);
         SinPhi := y1/sqrt((sqr(y1) + sqr(ScreenDistance)));
         wy := pi*SinPhi/(Lambda.Value(1)*1e-6);
         IF wx = 0 THEN wx := 1e-10;
         IF wy = 0 THEN wy := 1e-10;
         Term :=  sqr(Sin(wx*horwidth)/(wx*horwidth))
                  *sqr(Sin(wy*vertht)/(wy*vertht));
         Contours.put(Y,X,Term);
         y1 := y1 - ylength/(NumberofPoints -1);
      END;
      y1 := ymax;
      x1 := x1 + xLength/(NumberofPoints - 1);
   END;
   Contours.MinMax(MinRow,MinCol,Min,MaxRow,MaxCol,Max);
   FOR X := 1 to NumberofPoints  DO            { x loop }
   BEGIN
      FOR Y := 1 to NumberofPoints  DO         { y loop }
      BEGIN
         IF Contours.Value(Y,X) > Limit THEN
         Contours.put(Y,X,Limit);
      END;
   END;
   Graph3D.Init;
   Graph3D.SetViewAngles(30,20,0);
   Graph3D.Set3DScales(XMin,XMax,ymin,ymax,0,0);
   HideMouse;
   Graph3D.Drawbox;
   Graph3D.DrawSurface(Contours);
   ShowMouse;
   ColNum := 1;
   Colours(ColNum,Lambda);
   Print(1,2,'       Rectangular Aperture');
   SetColor(White);
   CloseViewPort(11);
   OpenViewPort(1);
   HideMouse;
   Contour.Init;
   Contour.DrawMatrix(Contours);
   ShowMouse;
   DefineScale(1,XMin,XMax,YMin,YMax);
   Axis(XMin,YMin,0.05,0.05);
   Plotted := False;
   Contours.free;
END;

{-------------------- TCircular -----------------------------}
PROCEDURE TCircular.Init;
BEGIN
   Lambda.init(1);
   Sliders.init;
   SetDefaults;
END;

PROCEDURE TCircular.Done;
BEGIN
   Lambda.Free;
   Sliders.Done;
END;

PROCEDURE TCircular.SetDefaults;
BEGIN
   Radius         :=  0.01; (* aperture Radius in nanometres *)
   ScreenDistance :=  1;
   Lambda.put(1,550);  {Wavelength in nm)}
   XCenter        :=  0;
   YCenter        :=  0;
   XMin           := XCenter - 0.1;
   XMax           :=  XMin + 0.2;
   ymin           :=  YCenter - 0.1;
   ymax           := ymin + 0.2;
   Limit          := 0.1;
   GraphBackColor := Black;
   SetUp;
END;

PROCEDURE TCircular.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(1,0.1,0.43,0.09,0.49);   {Contour Plot}
   DefineViewPort(11,0,0.41,0.05,0.5);  {Blackout Contour Scales}
   DefineViewPort(2,0.52,0.99,0.06,0.66);   {3D Plot}
   DefineViewPort(3,0.17,0.35,0.53,0.78);    {dummy Screen}
   DefineViewPort(4,0,0.52,0.81,0.94);
   SetUpHotKeys;
   SetUpSliders;
   IF Active THEN
   WITH Views[3] DO
   BEGIN
      b.x := round(0.5*(vx2-vx1));
      b.y := round(0.5*(vy2-vy1));
      Screen;
   END;
   Plot;
END;

PROCEDURE TCircular.SetUpSliders;
BEGIN
   With Sliders DO
   BEGIN
      create(1,0.01,1,Limit,0.55,0.90,0.95,2,'0.01','1','Threshold',False);
      create(2,0.005,0.04,Radius,0.55,0.80,0.95,3,'0.005','0.04','Radius mm',False);
      create(3,400,800,Lambda.Value(1),0.55,0.70,0.95,0,'400','800','Wavelength nm',False);
   END;
END;

PROCEDURE TCircular.HandleSliders;
BEGIN
   Limit  := Sliders.Value(1);
   Radius := Sliders.Value(2);
   Lambda.put(1,Sliders.Value(3));
   Plot;
END;

PROCEDURE TCircular.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TCircular.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN8');
   2: BEGIN
         Done;
         init;
      END;
   3: IF Menu.Chosen THEN HandleMenu;
   END;
END;

PROCEDURE TCircular.Screen;
VAR AString        : String;
BEGIN
   SetColor(white);
   OpenViewPort(3);
   DefineScale(3,-0.5,0.5,-0.5,0.5);
   Axis(0,0,0.5,0.5);
   SetColor(LightRed);
   Square(a,b);
   SelectViewPort(4);
   SetColor(white);
   AString := concat('   Current Center: X = ',NumStr(XCenter,1,2),
                      ' Y = ',NumStr(YCenter,1,2));
   RubOut(20,3,20,blue);
   Print(1,3,AString);
END;

PROCEDURE TCircular.CheckMouse;
VAR N,IntX,IntY,XX,YY,Button,v1,v2,v3,v4    : Integer;
    x1,XMouse,rho,inner,Term,xLength        : Real;
    c                                       : PointType;
    OK                                      : Boolean;
    AString                                 : String;
BEGIN
   IF Active THEN
   BEGIN
      IF Sliders.Changed THEN HandleSliders;

      IF (Event.MouseClicked
          AND PointInside(Event.X,Event.Y,3)) THEN
      WITH Views[3] DO
      BEGIN
         SelectViewPort(3);
         SelectScale(3);
         c.x := b.x;
         c.y := b.y;
         v1 := round(b.x + round(0.1*(vx2-vx1)));
         v2 := round(b.x - round(0.1*(vx2-vx1)));
         v3 := round(b.y + round(0.1*(vy2-vy1)));
         v4 := round(b.y - round(0.1*(vy2-vy1)));
         REPEAT
           MouseGlobalPosn(XX,YY,button);
           OK := (   ((XX - vx1) < v1)
                 AND ((XX - vx1) > v2)
                 AND ((YY - vy1) < v3)
                AND  ((YY - vy1) > v4) );
         UNTIL  (button<>0) AND OK ;
         b.x := XX -vx1;
         b.y := YY - vy1;
         SetColor(Black);
         hideCursor;
         Square(a,c);
         Square(a,b);
         ShowCursor;
         SetColor(Lightred);
         with views[3] do
          MouseBorders(vx1,vy1,vx2,vy2);
         DrawRubberBand(Square,a,b,Black,true);
         MouseBorders(0,0,GetMaxX,GetMaxY);
         SetColor(white);
         Axis(0,0,0.5,0.5);
         SetColor(LightRed);
         Square(a,b);
         SetColor(white);
         SelectViewPort(4);
         XCenter := round(100*(b.x/(vx2-vx1)-0.5))/100;
         YCenter := round(100*(0.5 - b.y/(vy2-vy1)))/100;
         AString := concat('   Current Center: X = ',NumStr(Xcenter,1,2),
                            ' Y = ',NumStr(YCenter,1,2));
         RubOut(20,3,20,blue);
         Print(1,3,AString);
         Plot;
      END;
   END;
END;

PROCEDURE TCircular.Plot;
VAR AString1                                            : String;
    Angle1,Angle2,Angle3,Min,rho,inner,Term,xLength,
    x1,y1,yLength,XMouse,YMouse                         : Real;
    Contours                                            : DMatrix;
    MinRow,MinCol,MaxRow,MaxCol,X,Y,ColNum              : Integer;
    Graph3D                                             : TGraph3D;
    Contour                                             : TContour;
BEGIN
   IF NOT Active THEN
   BEGIN
      OpenViewPort(3);
      DefineScale(3,-0.5,0.5,-0.5,0.5);
      Axis(0,0,0.5,0.5);
      SetColor(lightred);
      WITH Views[3] DO
      BEGIN
         b.x := round(0.5*(vx2-vx1));
         b.y := round(0.5*(vy2-vy1));
         Square(a,b);
         SetColor(white);
         SelectViewPort(4);
         MakeBlueBox;
         Print(1,1,' Click and Drag Mouse in ViewPort below ');
         Print(1,2,'  to change position of Viewing Screen ');
         AString1 := concat('   Current Center: X = ',NumStr((b.x/(vx2-vx1)-0.5),1,2),
                            ' Y = ',NumStr((0.5 - b.y/(vy2-vy1)),1,2));
         Print(1,3,AString1);
         XCenter := round(100*(b.x/(vx2-vx1)-0.5))/100;
         YCenter := round(100*(0.5 - b.y/(vy2-vy1)))/100;
      END;
   END;
   Active          := True;
   Sliders.DrawAll;
   MainHotKeys.Clear;
   HotKeys.Display;
   NumberofPoints  := 40;
   Contours.init(NumberofPoints,NumberofPoints);
   OpenViewPort(2);
   SetColor(white);
   XMin            := XCenter - 0.1;
   XMax            := XMin + 0.2;
   ymin            := YCenter - 0.1;
   ymax            := ymin + 0.2;
   x1              := XMin;
   y1              := ymax;
   xLength         := 0.2;
   yLength         := 0.2;
   FOR X := 1 to NumberofPoints DO
   BEGIN
      FOR Y := 1 to NumberofPoints DO
      BEGIN
         rho := (Sqrt(Sqr(x1) + Sqr(y1)));
         inner := 2*pi*Radius*rho/sqrt(sqr(rho) + sqr(ScreenDistance))/(Lambda.Value(1)*1e-6);
         Term  := sqr(2*BessJ1(inner)/inner);
         Contours.put(Y,X,term);
         y1 := y1 - yLength/(NumberofPoints-1);
      END;
      y1 := ymax;
      x1 := x1 + xLength/(NumberofPoints-1);
   END;
   Contours.MinMax(MinRow,MinCol,Min,MaxRow,MaxCol,Max);
   FOR X := 1 to NumberofPoints  DO            { x loop }
   BEGIN
      FOR Y := 1 to NumberofPoints  DO         { y loop }
      BEGIN
         IF Contours.Value(Y,X) > Limit THEN
         Contours.put(Y,X,Limit);
      END;
   END;
   Graph3D.Init;
   Graph3D.SetViewAngles(20,20,0);
   Graph3D.Set3DScales(XMin,XMax,ymin,ymax,0,0);
   HideMouse;
   Graph3D.Drawbox;
   Graph3D.DrawSurface(Contours);
   ShowMouse;
   ColNum := 1;
   Colours(ColNum,Lambda);
   Print(1,2,'       Circular Aperture');
   SetColor(White);
   CloseViewPort(11);
   OpenViewPort(1);
   HideMouse;
   Contour.Init;
   Contour.DrawMatrix(Contours);
   ShowMouse;
   DefineScale(1,XMin,XMax,YMin,YMax);
   Axis(XMin,YMin,0.05,0.05);
   Plotted := False;
   Contours.free;
END;

{------------------------- TCoherenceCircle -------------------------------}
PROCEDURE TCoherenceCircle.Init;
BEGIN
   Lambda.init(1);
   Circle.init(1,1);
   Sliders.init;
   Buttons.init;
   SetDefaults;
END;

PROCEDURE TCoherenceCircle.Done;
BEGIN
   Lambda.free;
   ResolutionPlot.done;
   Sliders.Done;
   Buttons.Done;
   Circle.free;
END;

PROCEDURE TCoherenceCircle.SetDefaults;
BEGIN
   Lambda.put(1,550);
   Radius            := 0.04 {mm};
   SourceSeparation  := 10; {0.01;  {mm}
   SourceDistance1   := -SourceSeparation/2;
   SourceDistance2   :=  SourceSeparation/2;
   XMinAngle         := -2*SourceSeparation/1000;
   YMinAngle         := XMinAngle;
   FirstY            := 0;
   FirstX            := 0;
   InViewPort        := False;
   SetUp;
END;

PROCEDURE TCoherenceCircle.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(12,0.08,0.48,0.13,0.65);         {One Aperture  left}
   DefineViewPort(1,0.50,0.98,0.13,0.65);         {One Aperture  l right}
   DefineViewPort(2,0,1,0.06,0.94);               {One Aperture - blackout}
   DefineViewPort(3,0.53,0.96,0.78,0.88);         {Message Box u right}
   DefineViewPort(5,0,0.6,0.06,0.12);
   ResolutionPlot.init(1);
   SetUpHotKeys;
   SetUpSlidersandButtons;
   OneCircle;
END;

PROCEDURE TCoherenceCircle.SetUpSlidersandButtons;
BEGIN
   WITH Sliders DO
   BEGIN
      create(1,0,20,SourceSeparation,0.02,0.87,0.48,3,'0','20','Separation mm',False);
      create(2,400,800,Lambda.Value(1),0.02,0.73,0.48,0,'400','800','Wavelength nm',False);
      create(3,0.01,0.08,Radius,0.02,0.80,0.48,3,'0.01','0.08','Radius mm',False);
   END;
   WITH Buttons DO
   BEGIN
     create(1,0.52,0.7,'0.01 - 0.08');
     create(2,0.72,0.7,'1 - 5');
     create(3,0.85,0.7,'100 - 1000');
     Number := 1;
   END;
END;

PROCEDURE TCoherenceCircle.HandleSliders;
BEGIN
    SourceSeparation := Sliders.Value(1);
    Lambda.put(1,Sliders.Value(2));
    Radius           := Sliders.Value(3);
    OneCircle;
END;

PROCEDURE TCoherenceCircle.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TCoherenceCircle.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN13');
   2: BEGIN
         Sliders.erase(1,Black);
         Sliders.erase(2,Black);
         Sliders.erase(3,Black);
         Done;
         init;
      END;
   3: IF Menu.Chosen THEN HandleMenu;
   END;
END;

PROCEDURE TCoherenceCircle.HandleButtons;
BEGIN
   IF (Buttons.Number = 3) THEN
   BEGIN
      WITH Sliders DO
      BEGIN
         erase(1,black);
         delete(1);
         erase(3,black);
         delete(3);
         SourceSeparation := 0.00125;     {mm}
         XMinAngle := -1.5*SourceSeparation/1000; {radians}
         YMinAngle := XMinAngle;
         Radius := 500;
         create(1,0,0.0025,SourceSeparation,0.02,0.87,0.48,5,'0','0.0025','Separation mm',False);
         create(3,100,1000,Radius,0.02,0.80,0.48,1,'100','1000','Radius mm',False);
         DrawAll;
      END;
   END;

   IF (Buttons.Number = 2) THEN
   BEGIN
      WITH Sliders DO
      BEGIN
         erase(1,black);
         delete(1);                                                              erase(3,black);
         delete(3);
         SourceSeparation := 0.25;
         XMinAngle := -1.5*SourceSeparation/1000;
         YMinAngle := XMinAngle;
         Radius := 2.5;
         create(1,0,0.5,SourceSeparation,0.02,0.87,0.48,3,'0','0.5','Separation mm',False);
         create(3,1,5,Radius,0.02,0.80,0.48,3,'1','5','Radius mm',False);
         DrawAll;
      END;
   END;

   IF  (Buttons.Number = 1) THEN
   BEGIN
      WITH Sliders DO
      BEGIN
         erase(1,black);
         delete(1);
         erase(3,black);
         delete(3);
         SourceSeparation := 20;
         XMinAngle := -2*SourceSeparation/1000;
         YMinAngle := XMinAngle;
         Radius := 0.04;
         create(1,0,40,SourceSeparation,0.02,0.87,0.48,3,'0','40','Separation mm',False);
         create(3,0.01,0.08,Radius,0.02,0.80,0.48,3,'0.01','0.08','Radius mm',False);
         DrawAll;
      END;
   END;
   OneCircle;
END;


PROCEDURE TCoherenceCircle.CheckMouse;
VAR Vis,C,F,Value1,Value2,Value3,Value4,A           : Real;
    B,D,E,x,y,button                                : Integer;
    AString                                         : String;
BEGIN
   ResolutionPlot.Check;

   IF Active THEN
   BEGIN
      IF Sliders.Changed THEN HandleSliders;
      IF Buttons.Changed THEN HandleButtons;

      MouseGlobalPosn(x,y,button);
      WITH Views[12] DO
      BEGIN
         IF(x>vx1) AND (x<vx2) AND(y>vy1) AND(y<vy2) THEN
         BEGIN
            SelectViewPort(12);
            NewA := 2.2*(vy2 - Y)/(vy2 - vy1);
            NewB := -2000*XMinAngle*(X - vx1)/(vx2-vx1) + 1000*XMinAngle;
            IF (Y <> FirstY) OR (X <> FirstX) THEN
            BEGIN
               Print(2,2,'Intensity');
               Print(2,3,'Position');
               Print2(12,2,2,NewA,lightgreen,Black);
               IF Buttons.Number = 1 THEN
                  Print2(12,3,1,NewB,lightgreen,Black);
               IF Buttons.Number = 2 THEN
                  Print2(12,3,3,NewB,lightgreen,Black);
               IF Buttons.Number = 3 THEN
                  Print2(12,3,5,NewB,lightgreen,Black);
               FirstY := Y;
               FirstX := X;
               InViewPort := True;
               SetColor(white);
            END;
         END
         ELSE
         BEGIN
            IF InViewPort THEN
            BEGIN
               SelectViewPort(12);
               RubOut(2,2,16,Black);
               RubOut(2,3,18,Black);
            END;
            InViewPort := False;
         END;
      END;
   END;
END;

PROCEDURE TCoherenceCircle.OneCircle;
VAR  Length,Alpha1,Alpha2,Term1,Term2,X,Y,
     Theta1,Theta2,Inner1,Inner2,rho1,rho2                  :     Real;
     M,N,NumberofPoints,NumberofContourPoints,ColNUm        :  Integer;
     Intensity1,Intensity2,TotalIntensity12,
     Intensity3,Intensity4,TotalIntensity34,XValues         :  DVector;
     Graph3DCircle                                          :  TGraph3D;
     AString                                                :  String;
     Wait                                                   :  TWaitMessage;
BEGIN
   NumberofPoints := 192;
   NumberofContourPoints := 40;
   Intensity1.init(NumberofPoints);
   Intensity2.init(NumberofPoints);
   TotalIntensity12.init(NumberofPoints);
   Intensity3.init(NumberofPoints);
   Intensity4.init(NumberofPoints);
   TotalIntensity34.init(NumberofPoints);
   XValues.init(NumberofPoints);
   Length := -2*XMinAngle;
   DefineScale(12,1000*XMinAngle,1000*(XMinAngle+Length),0,2.2);
   SetColor(white);
   GraphBackColor := darkgray;
   X := XMinAngle;
   Y := YMinAngle;
   Circle.free;
   Circle.init(NumberofContourPoints,NumberofContourPoints);
   SourceDistance1:= -SourceSeparation/2;
   SourceDistance2 :=  SourceSeparation/2;
   Theta1 := SourceDistance1/1000;
   Theta2 := SourceDistance2/1000;
   IF NOT Active THEN
   BEGIN
      SelectViewPort(3);
      MakeBlueBox;
      Print(1,1,'  Measure Intensities using the');
      Print(1,2,'     Mouse in Left Viewport');
   END;
   Active := True;
   MainHotKeys.Clear;
   HotKeys.Display;
   Sliders.DrawAll;
   Buttons.DrawAll;
   PrintXY(0.6,0.75,'RADIUS OF APERTURE in mm');
   FOR N := 1 to NumberofPoints DO
   BEGIN
      Inner1 :=2*pi*(X-Theta1)*Radius/(1e-6*Lambda.Value(1));
      Inner2 :=2*pi*(X-Theta2)*Radius/(1e-6*Lambda.Value(1));
      Term1 := sqr(2*BessJ1(Inner1)/Inner1);
      Term2 := sqr(2*BessJ1(Inner2)/Inner2);
      Intensity3.put(N,Term1);
      Intensity4.put(N,Term2);
      TotalIntensity34.put(N,Term1 + Term2);
      XValues.put(N,x);
      x := x + Length/(NumberofPoints - 1);
   END;
   FOR N := 1 to NumberofPoints DO
      XValues.put(N,1000*XValues.Value(N));
   CloseViewPort(5);
   GraphBackColor := Black;
   OpenViewPort(12);
   SelectScale(12);
   IF Buttons.Number = 1 THEN
   AString := concat(' Separation Arc = ',
      NumStr(SourceSeparation*180*3600/pi/1000,0,0),' sec')
      ELSE IF Buttons.Number = 2 THEN
      AString := concat(' Separation Arc = ',
      NumStr(SourceSeparation*180*3600/pi/1000,0,2),' sec')
      ELSE AString := concat(' Separation Arc = ',
      NumStr(SourceSeparation*180*3600/pi/1000,0,3),' sec');
   Print(1,1,AString);
   Axis(0,0,-1000*XMinAngle/2,1);
   SetColor(Black);
   PlotLine(0,1.65,0,2.19);
   SetColor(white);
   PutLabel(left,'Intensity');
   PutLabel(bottom,'Position mm');
   SetColor(lightcyan);
   PlotDvectors(XValues,Intensity3,1,XValues.size);
   PlotDvectors(XValues,Intensity4,1,XValues.size);
   SetColor(white);
   PlotDVectors(XValues,TotalIntensity34,1,XValues.size);
   X :=  XMinAngle;
   Y :=  YMInAngle;
   Wait.Show;
   FOR M := 1 to NumberofContourPoints DO
   BEGIN
      FOR N := 1 to NumberofContourPoints DO
      BEGIN
         rho1 := sqrt(sqr(x-Theta1) + sqr(y));
         rho2 := sqrt(sqr(x-Theta2) + sqr(y));
         Inner1 :=2*pi*rho1*Radius/(1e-6*Lambda.Value(1));
         Inner2 :=2*pi*rho2*Radius/(1e-6*Lambda.Value(1));
         Term1 := sqr(2*BessJ1(Inner1)/Inner1);
         Term2 := sqr(2*BessJ1(Inner2)/Inner2);
         Circle.put(M,N,Term1 + Term2);
         X := X + Length/(NumberofContourPoints - 1);
      END;
      X := XMinAngle;
      Y := Y + Length/(NumberofContourPoints - 1);
      Wait.UpDate;
   END;
   Wait.Hide;
   ResolutionPlot.PlotMatrix(circle,' Circular Aperture');
   Intensity1.free;
   Intensity2.free;
   Intensity3.free;
   Intensity4.free;
   TotalIntensity12.free;
   TotalIntensity34.free;
   XValues.free;
END;

{------------------------- TCoherenceSlit -------------------------------}
PROCEDURE TCoherenceSlit.Init;
BEGIN
   Lambda.init(1);
   Slit.init(1,1);
   Sliders.init;
   SetDefaults;
END;

PROCEDURE TCoherenceSlit.Done;
BEGIN
   Lambda.free;
   ResolutionPlot.done;
   Sliders.Done;
   Slit.free;
END;

PROCEDURE TCoherenceSlit.SetDefaults;
BEGIN
   Lambda.put(1,550);
   SlitWidth        := 0.08;
   SourceSeparation := 10;
   SourceDistance1  := -SourceSeparation/2;
   SourceDistance2  :=  SourceSeparation/2;
   XMinAngle        := -2*SourceSeparation/1000;
   YMinAngle        := XMinAngle;
   FirstY           := 0;
   FirstX           := 0;
   InViewPort       := False;
   SetUp;
END;

PROCEDURE TCoherenceSlit.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(12,0.08,0.48,0.13,0.65);         {One Aperture  left}
   DefineViewPort(1,0.50,0.98,0.13,0.65);         {One Aperture  l right}
   DefineViewPort(3,0.53,0.96,0.78,0.88);         {Message Box u right}
   ResolutionPlot.init(1);
   SetUpHotKeys;
   SetUpSliders;
   OneSlit;
END;

PROCEDURE TCoherenceSlit.SetUpSliders;
BEGIN
   WITH Sliders DO
   BEGIN
      create(1,0,20,SourceSeparation,0.02,0.87,0.48,3,'0','20','Source Separation mm',False);
      create(2,400,800,Lambda.Value(1),0.02,0.73,0.48,0,'400','800','Wavelength nm',False);
      create(3,0.01,0.16,SlitWidth,0.02,0.80,0.48,3,'0.01','0.16','Slit Width mm',False);
   END;
END;

PROCEDURE TCoherenceSlit.HandleSliders;
BEGIN
    SourceSeparation := Sliders.Value(1);
    Lambda.put(1,Sliders.Value(2));
    SlitWidth        := Sliders.Value(3);
    SourceDistance1  := -SourceSeparation/2;
    SourceDistance2  :=  SourceSeparation/2;
    OneSlit;
END;

PROCEDURE TCoherenceSlit.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TCoherenceSlit.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN14');
   2: BEGIN
         Done;
         init;
      END;
   3: IF Menu.Chosen THEN HandleMenu;
   END;
END;

PROCEDURE TCoherenceSlit.CheckMouse;
VAR AString                 : String;
    A                       : Real;
    X,Y,Button              : Integer;
BEGIN
   ResolutionPlot.Check;

    IF Active THEN
    BEGIN
       IF Sliders.Changed THEN HandleSliders;

       BEGIN
         MouseGlobalPosn(x,y,Button);
         WITH Views[12] DO
         BEGIN
            IF(x>vx1) AND (x<vx2) AND(y>vy1) AND(y<vy2) THEN
            BEGIN
               SelectViewPort(12);
               NewA := 2.2*(vy2 - Y)/(vy2 - vy1);
               NewB := -2000*XMinAngle*(X - vx1)/(vx2-vx1) + 1000*XMinAngle;
               IF (Y <> FirstY) OR (X <> FirstX) THEN
               BEGIN
                  Print(2,2,'Intensity');
                  Print(2,3,'Position');
                  Print2(12,2,2,NewA,lightgreen,Black);
                  Print2(12,3,1,NewB,lightgreen,Black);
                  FirstY := Y;
                  FirstX := X;
                  InViewPort := True;
                  SetColor(white);
               END;
            END
            ELSE
            BEGIN
               IF InViewPort THEN
               BEGIN
                  SelectViewPort(12);
                  RubOut(2,2,15,Black);
                  RubOut(2,3,15,Black);
               END;
               InViewPort := False;
            END;
         END;
      END;
   END;
END;


PROCEDURE TCoherenceSlit.OneSlit;
VAR  Length,Alpha1,Alpha2,Term1,Term2,X,Y,
     Theta1,Theta2,Inner1,Inner2,rho1,rho2                 :     Real;
     M,N,NumberofPoints,NumberofContourPoints,ColNum       :  Integer;
     Intensity1,Intensity2,TotalIntensity12,
     Intensity3,Intensity4,TotalIntensity34,XValues        :  DVector;
     Graph3DSlit                                           :  TGraph3D;
     AString                                               :  String;
     Wait                                                  :  TWaitMessage;
BEGIN
   NumberofPoints := 192;
   NumberofContourPoints := 40;
   Intensity1.init(NumberofPoints);
   Intensity2.init(NumberofPoints);
   TotalIntensity12.init(NumberofPoints);
   Intensity3.init(NumberofPoints);
   Intensity4.init(NumberofPoints);
   TotalIntensity34.init(NumberofPoints);
   XValues.init(NumberofPoints);
   Length := -2*XMinAngle;
   DefineScale(12,1000*XMinAngle,1000*(XMinAngle+Length),0,2.2);
   SetColor(white);
   GraphBackColor :=darkgray;
   X := XMinAngle;
   Y := YMinAngle;
   Slit.free;
   Slit.init(NumberofContourPoints,NumberofContourPoints);
   SourceDistance1:= -SourceSeparation/2;
   SourceDistance2 :=  SourceSeparation/2;
   Theta1 := SourceDistance1/1000;
   Theta2 := SourceDistance2/1000;
   IF NOT Active THEN
   BEGIN
      SelectViewPort(3);
      MakeBlueBox;
      Print(1,1,'  Measure Intensities using the');
      Print(1,2,'     Mouse in Left Viewport');
   END;
   MainHotKeys.Clear;
   HotKeys.Display;
   Sliders.DrawAll;
   Active := True;
   FOR N := 1 to NumberofPoints DO
   BEGIN
      Alpha1 := pi*SlitWidth*(X - Theta1)/(1e-6*Lambda.Value(1));
      IF Alpha1 = 0 THEN
         Alpha1 := 1e-9;
      Alpha2 := pi*SlitWidth*(X - Theta2)/(1e-6*Lambda.Value(1));
      IF Alpha2 = 0 THEN
         Alpha2 := 1e-9;
      Term1 := sqr(Sin(alpha1)/alpha1);
      Term2 := sqr(Sin(alpha2)/alpha2);
      Intensity1.put(N,Term1);
      Intensity2.put(n,Term2);
      TotalIntensity12.put(N,Term1 + Term2);
      XValues.put(N,x);
      x := x + length/(NumberofPoints - 1);
   END;
   FOR N := 1 to NumberofPoints DO
      XValues.put(N,1000*XValues.Value(N));
   GraphBackColor := Black;
   OpenViewPort(12);
   SelectScale(12);
   AString := concat(' Separation Arc = ',
      NumStr(SourceSeparation*180*3600/pi/1000,0,0),' sec');
   Print(1,1,AString);
   Axis(0,0,-1000*XMinAngle/2,1);
   SetColor(Black);
   PlotLine(0,1.65,0,2.19);
   SetColor(white);
   PutLabel(left,'Intensity');
   PutLabel(bottom,'Position mm');
   SetColor(lightcyan);
   PlotDVectors(XValues,Intensity1,1,XValues.size);
   PlotDVectors(XValues,Intensity2,1,XValues.size);
   SetColor(white);
   PlotDVectors(XValues,TotalIntensity12,1,XValues.size);
   X := XMinAngle;
   Y := YMinAngle;

   FOR N := 1 to NumberofContourPoints DO
   BEGIN
      Alpha1 := pi*SlitWidth*(X - Theta1)/(1e-6*Lambda.Value(1));
      IF Alpha1 = 0 THEN
         Alpha1 := 1e-9;
      Alpha2 := pi*SlitWidth*(X - Theta2)/(1e-6*Lambda.Value(1));
      IF Alpha2 = 0 THEN
         Alpha2 := 1e-9;
      Term1 := sqr(Sin(alpha1)/alpha1);
      Term2 := sqr(Sin(alpha2)/alpha2);
      TotalIntensity12.put(N,Term1 + Term2);
      XValues.put(N,x);
      X := X + length/(NumberofContourPoints - 1);
   END;
   Wait.Show;
   For M := 1 to NumberofContourPoints DO
   BEGIN
      FOR N := 1 to NumberofContourPoints DO
      BEGIN
         Slit.put(M,N,TotalIntensity12.Value(N));
      END;
      Wait.UpDate;
   END;
   Wait.Hide;
   ResolutionPlot.PlotMatrix(Slit,' Slit Aperture');
   Intensity1.free;
   Intensity2.free;
   Intensity3.free;
   Intensity4.free;
   TotalIntensity12.free;
   TotalIntensity34.free;
   XValues.free;
END;

{********************** TCoherenceTwo ***********************}

PROCEDURE TCoherenceTwo.Init;
BEGIN
   Lambda.init(1);
   Sliders.init;
   ZoomSourceButtons.init;
   ZoomSlitButtons.init;
   SetDefaults;
END;

PROCEDURE TCoherenceTwo.Done;
BEGIN
   Lambda.free;
   Sliders.Done;
   ZoomSourceButtons.Done;
   ZoomSlitButtons.Done;
END;

PROCEDURE TCoherenceTwo.SetDefaults;
BEGIN
   Lambda.put(1,550);
   SlitWidth        := 0.002; {2000} {mm}
   SlitSeparation   := 0.03;{30000;} {mm}
   SourceSeparation := 0.01;       {m}
   SourceDistance1  := -SourceSeparation/2;
   SourceDistance2  := SourceSeparation/2;
   ScreenDistance   := 1;               {m}
   GraphBackColor   := Black;
   SetUp;
END;

PROCEDURE TCoherenceTwo.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(4,0.25,0.75,0.69,0.86);        {Top}
   DefineViewPort(5,0.25,0.75,0.42,0.59);       {Middle}
   DefineViewPort(6,0.25,0.75,0.15,0.32);       {Bottom}
   SetUpHotKeys;
   SetUpSlidersandButtons;
   TwoSources;
END;

PROCEDURE TCoherenceTwo.SetUpSlidersandButtons;
BEGIN
   WITH  Sliders DO
   BEGIN
      create(1,0,0.05,SourceSeparation,0.08,0.06,0.4,4,'0','0.05','Separation m',True);
      create(2,400,800,Lambda.Value(1),0.08,0.53,0.87,0,'400','800','Wavelength nm',True);
      create(3,0.0001,0.004,SlitWidth ,0.87,0.53,0.87,4,'0.0001','0.004',' Width mm',True);
      create(4,0.0,0.05,SlitSeparation ,0.87,0.06,0.4,4,'0','0.05','Separation mm',True);
   END;

   WITH ZoomSlitButtons DO
   BEGIN
      init;
      create(1,0.82,0.43,'Zoom');
      create(2,0.82,0.47,'Normal');
      Number := 2;
   END;
   WITH ZoomSourceButtons DO
   BEGIN
      init;
      create(1,0.02,0.43,'Zoom');
      create(2,0.02,0.47,'Normal');
      Number := 2;
   END;
END;

PROCEDURE TCoherenceTwo.HandleSliders;
BEGIN
   SourceSeparation := Sliders.Value(1);
   SourceDistance1  := -SourceSeparation/2;
   SourceDistance2  := SourceSeparation/2;
   Lambda.put(1,Sliders.Value(2));
   SlitWidth        := Sliders.Value(3);
   SlitSeparation   := Sliders.Value(4);
   TwoSources;
END;

PROCEDURE TCoherenceTwo.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TCoherenceTwo.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN15');
   2: BEGIN
         sliders.erase(1,Black);
         sliders.erase(4,Black);
         Done;
         init;
      END;
   3: IF Menu.Chosen THEN HandleMenu;
   END;
END;

PROCEDURE TCoherenceTwo.CheckMouse;
VAR MIn,Max : Real;
BEGIN
   IF Active THEN
   BEGIN
      IF Sliders.Changed THEN HandleSliders;

      IF ZoomSlitButtons.Changed THEN
      BEGIN
         IF (ZoomSlitButtons.Number = 2) THEN
         WITH Sliders DO
         BEGIN
            erase(4,black);
            delete(4);
            create(4,0.0,0.05,SlitSeparation,0.87,0.06,0.4,4,'0','0.05','Separation mm',True);
            DrawAll;
         END;

         IF (ZoomSlitButtons.Number = 1) THEN
         BEGIN
            Min := SlitSeparation - 0.0025;
              IF Min < 0.0025 THEN Min := 0;
            Max := Min + 0.005;
               IF Max > 0.045 THEN Max := 0.05;
            WITH Sliders Do
            BEGIN
               erase(4,black);
               Delete(4);
               create(4,Min,Max,SlitSeparation,0.87,0.06,0.4,4,
               NumStr(Min,1,3),NumStr(Max,1,3),'Separation mm',True);
               DrawAll;
            END;
         END;
      END;

      IF ZoomSourceButtons.Changed THEN
      BEGIN
         IF (ZoomSourceButtons.Number = 2) THEN
         WITH Sliders DO
         BEGIN
            erase(1,black);
            delete(1);
            create(1,0,0.05,SourceSeparation,0.08,0.06,0.4,4,'0','0.05','Separation m',True);
            DrawAll;
         END;

         IF (ZoomSourceButtons.number = 1) THEN
         BEGIN
            Min := SourceSeparation - 0.0025;
              IF Min < 0.004 THEN Min := 0;
            Max := Min + 0.005;
               IF Max > 0.045 THEN Max := 0.05;
            WITH Sliders Do
            BEGIN
               erase(1,black);
               Delete(1);
               create(1,Min,Max,SourceSeparation,0.08,0.06,0.4,4,
               NumStr(Min,1,3),NumStr(Max,1,3),'Separation m',True);
               DrawAll;
            END;
         END;
      END;
   END;
END;

PROCEDURE TCoherenceTwo.TwoSources;
VAR  Length,Alpha,Beta,Term,Min,Max,X,Y,SourceDistance,
     A,Theta1,Theta2,Distance,Value                       :  Real;
     M,N,NumberofPoints,ColNum                            :  Integer;
     Intensity1,Intensity2,Intensity3,Intensity4,
     Intensity5,Intensity6,XValues                        :  DVector;
     AString                                              :  String;
     Inside                                               : Boolean;
BEGIN
   Sliders.DrawAll;
   ZoomSourceButtons.DrawAll;
   ZoomSlitButtons.DrawAll;
   MainHotKeys.Clear;
   HotKeys.Display;
   Active := True;
   NumberofPoints := 300;
   Intensity1.init(NumberofPoints);
   Intensity2.init(NumberofPoints);
   Intensity3.init(NumberofPoints);
   Intensity4.init(NumberofPoints);
   Intensity5.init(NumberofPoints);
   Intensity6.init(NumberofPoints);
   XValues.init(NumberofPoints);
   DefineScale(4,-0.1,0.1,0,10);
   DefineScale(5,-0.1,0.1,0,10);
   DefineScale(6,-0.1,0.1,0,10);
   SetColor(White);
   SourceDistance := 1;
   Theta1 := ArcSin(SourceDistance1/sqrt(sqr(SourceDistance)+sqr(SourceDistance1)));
   Theta2 := ArcSin(SourceDistance2/sqrt(sqr(SourceDistance)+sqr(SourceDistance2)));
   X := -0.1;
   Length := 0.2;
   FOR N := 1 to NumberofPoints DO
   BEGIN
      A := ArcSin(x/(sqr(x) + sqr(ScreenDistance)));
      Alpha := 2*pi*SlitWidth*(A - Theta1)/(1e-6*Lambda.Value(1));
      IF Alpha = 0 THEN
         Alpha := 1e-9;
      Term := sqr(Sin(alpha)/alpha);
      Intensity1.put(N,Term);
      XValues.put(N,x);
      x := x + length/(NumberofPoints - 1);
   END;
   x := -0.1;
   FOR N :=  1 to NumberofPoints Do
   BEGIN
      A := ArcSin(x/(sqr(x) + sqr(ScreenDistance)));
      Intensity2.put(N,2*Intensity1.Value(N)*
      (1 + cos(2*pi*SlitSeparation*(A - Theta1)/(1e-6*Lambda.Value(1))) ) );
       XValues.put(N,x);
      x := x + length/(NumberofPoints - 1);
   END;
   PrintXY(0.02,0.92,'Source Sliders');
   PrintXY(0.8,0.92,'Slit Sliders');
   OpenViewPort(4);
   SelectScale(4);
   Axis(0,0,0.1/2,2);
   PutLabel(top,' Two Slits - First Source');
   ColNum := 1;
   Colours(ColNum,Lambda);
   PlotDVectors(XValues,Intensity2,1,XValues.size);
   x := -0.1;
   FOR N := 1 to NumberofPoints DO
   BEGIN
      A := ArcSin(x/(sqr(x) + sqr(ScreenDistance)));
      Alpha := 2*pi*SlitWidth*(A - Theta2)/(1e-6*Lambda.Value(1));
      IF Alpha = 0 THEN
         Alpha := 1e-9;
      Term := sqr(Sin(alpha)/alpha);
      Intensity4.put(N,Term);       {one Slit}
      XValues.put(N,x);
      x := x + Length/(NumberofPoints -1);
   END;
   x := -0.1;
   FOR N :=  1 to NumberofPoints Do
   BEGIN
       A := ArcSin(x/(sqr(x) + sqr(ScreenDistance)));
       Intensity5.put(N,2*Intensity4.Value(N)*
      (1 + cos(2*pi*SlitSeparation*(A - Theta2)/(1e-6*Lambda.Value(1))) ) );
       XValues.put(N,x);
      x := x + length/(NumberofPoints - 1);
   END;
   OpenViewPort(5);
   SelectScale(5);
   Axis(0,0,0.05,2);
   Setcolor(white);
   PutLabel(top,' Two Slits - Second Source ');
   ColNum := 1;
   Colours(ColNum,Lambda);
   PlotDVectors(XValues,Intensity5,1,XValues.size);
   FOR N := 1 to NumberofPoints DO
   BEGIN
      Intensity6.put(N,Intensity2.Value(N) + Intensity5.Value(N));
   END;
   OpenViewPort(6);
   SelectScale(6);
   Axis(0,0,0.05,2);
   SetColor(white);
   PutLabel(top,'Two Slits - Two Sources added');
   PutLabel(bottom,'Position on Screen');
   SetColor(White);


   PlotDVectors(XValues,Intensity6,1,XValues.size);
   { Two sources two slits }
 Intensity1.free;
 Intensity2.free;
 Intensity3.free;
 Intensity4.free;
 Intensity5.free;
 Intensity6.free;
 XValues.free;

END;

{************************** TCoherenceLine *********************}

PROCEDURE TCoherenceLine.Init;
BEGIN
   Lambda.init(1);
   Sliders.init;
   ZoomSourceButtons.init;
   ZoomSlitButtons.init;
   SetDefaults;
END;

PROCEDURE TCoherenceLine.Done;
BEGIN
   Lambda.free;
   Sliders.Done;
   ZoomSourceButtons.Done;
   ZoomSlitButtons.Done;
END;

PROCEDURE TCoherenceLine.SetDefaults;
BEGIN
   Lambda.put(1,550);
   SlitWidth               := 0.002;{2000;} {mm}
   SlitSeparation          := 0.03; {60000;} {mm}
   XMax                    := 0.1;
   XMIn                    := -0.1;
   ScreenDistance          := 1;
   SourceWidth             := 0.04;          {metres}
   SourceDistancefromSlits := 1; {metres}
   SourceTop               := -SourceWidth/2;
   SourceBottom            :=  SourceWidth/2;
   FirstY                  := 0;
   FirstX                  := 0;
   InUpperViewPort         := False;
   InLowerViewPort         := False;
   GraphBackColor          := Black;
   SetUp;
END;

PROCEDURE TCoherenceLine.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(8,0.3,0.7,0.58,0.90);
   DefineViewPort(9,0.22,0.78,0.12,0.38);
   SetUpHotKeys;
   SetUpSlidersandButtons;
   LineSource;
END;

PROCEDURE TCoherenceLine.SetUpSlidersandButtons;
BEGIN
   WITH Sliders DO
   BEGIN
      create(1,0.001,0.05,SourceWidth,     0.08,0.06,0.40,4,'0.001','0.05','Width m',True);
      create(2,500,800,Lambda.Value(1),0.08,0.53,0.87,0,'500','800','Wavelength nm',True);
      create(3,0.0001,0.004,SlitWidth,0.87,0.53,0.87,4,'0.0001','0.004',' Width mm',True);
      create(4,0.001,0.04,SlitSeparation,0.87,0.06,0.4,4,'0.001','0.04','Separation mm',True);
   END;
   WITH ZoomSlitButtons DO

   BEGIN
      create(1,0.82,0.43,'Zoom');
      create(2,0.82,0.47,'Normal');
      Number := 2;
   END;
   WITH ZoomSourceButtons DO
   BEGIN
      create(1,0.02,0.43,'Zoom');
      create(2,0.02,0.47,'Normal');
      Number := 2;
   END;
END;

PROCEDURE TCoherenceLine.HandleSliders;
VAR T1,T2 : Real;
    AString : String;
BEGIN
   SourceWidth       := Sliders.Value(1);
   Lambda.put(1,Sliders.Value(2));
   SlitWidth         := Sliders.Value(3);
   SlitSeparation    := Sliders.Value(4);
   SourceTop         := -SourceWidth/2;
   SourceBottom      :=  SourceWidth/2;
   SelectViewPort(20);
   RubOut(52,13,10,Black);
   SetColor(lightgreen);
   T1 := ArcSin(SourceTop/sqrt(sqr(SourceTop)+sqr(SourceDistancefromSlits)));
   T2 := ArcSin(SourceBottom/sqrt(sqr(SourceBottom)+sqr(SourceDistancefromSlits)));
   DeltaTheta1 := T2 - T1;
   NewD := SlitSeparation*DeltaTheta1/(1e-6*Lambda.Value(1));
   Print2(52,13,2,NewD,lightgreen,black);
   SetColor(white);
   LineSource;
END;

PROCEDURE TCoherenceLine.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TCoherenceLine.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN16');
   2: BEGIN
         sliders.erase(1,Black);
         sliders.erase(4,Black);
         Done;
         init;
      END;
   3: IF Menu.Chosen THEN HandleMenu;
   END;
END;

PROCEDURE TCoherenceLine.CheckMouse;
VAR Min,Max,A                  : Real;
    AString                    : String;
    B,D,E,X,Y,Button           : Integer;
BEGIN
   IF Active THEN
   BEGIN
      IF Sliders.Changed THEN HandleSliders;

      BEGIN
         MouseGlobalPosn(x,y,button);
         WITH Views[9] DO
         BEGIN
            IF(x>vx1) AND (x<vx2) AND(y>vy1) AND(y<vy2) THEN
            BEGIN
               SelectViewPort(9);
               NewA := 1.5*(vy2 - Y)/(vy2 - vy1);
               IF Y <> FirstY THEN
               BEGIN
                  Print(2,2,'Intensity');
                  Print2(12,2,2,NewA,lightgreen,Black);
                  FirstY := Y;
                  InLowerViewPort := True;
                  SetColor(white);
               END;
            END
            ELSE
            BEGIN
               IF InLowerViewPort THEN
               BEGIN
                  SelectViewPort(9);
                  RubOut(2,2,14,Black);
               END;
               InLowerViewPort := False;
            END;
         END;
      END;

      IF ZoomSlitButtons.Changed THEN
      BEGIN
         IF (ZoomSlitButtons.Number = 2) THEN
         WITH Sliders DO
         BEGIN
            erase(4,black);
            delete(4);
            create(4,0.001,0.04,SlitSeparation,0.87,0.06,0.4,4,'0.001','0.04','Separation mm',True);
            ZoomSlitButtons.Number := 2;
            ZoomSlitButtons.DrawAll;
            DrawAll;
         END;

         IF (ZoomSlitButtons.Number = 1) THEN
         BEGIN
            Min := SlitSeparation - 0.003;
              IF Min < 0.006 THEN Min := 0.001;
            Max := Min + 0.006;
               IF Max > 0.034 THEN Max := 0.04;
            WITH Sliders Do
            BEGIN
               erase(4,black);
               Delete(4);
               create(4,Min,Max,SlitSeparation,0.87,0.06,0.4,4,
               NumStr(Min,1,3),NumStr(Max,1,3),'Separation mm',True);
               DrawAll;
            END;
         END;
      END;

      IF ZoomSourceButtons.Changed THEN
      BEGIN
         IF (ZoomSourceButtons.Number = 2) THEN
         WITH Sliders DO
         BEGIN
            erase(1,black);
            delete(1);
            create(1,0.001,0.05,SourceWidth,0.08,0.06,0.4,4,'0.001','0.05','Width mm',True);
            DrawAll;
         END;

         IF (ZoomSourceButtons.number = 1) THEN
         BEGIN
            Min := SourceWidth - 0.005;
              IF Min < 0.005 THEN Min := 0.001;
            Max := Min + 0.01;
               IF Max > 0.09 THEN Max := 0.1;
            WITH Sliders Do
            BEGIN
               erase(1,black);
               Delete(1);
               create(1,Min,Max,SourceWidth,0.08,0.06,0.4,5,
               NumStr(Min,1,3),NumStr(Max,1,3),'Width mm',True);
               DrawAll;
            END;
         END;
      END;
   END;
END;

PROCEDURE TCoherenceLine.LineSource;
VAR  Length,Alpha,Beta,Term,Min,Max,x,y,SourceDistance,
     A,B1,B2, TermA, Theta,ThetaPrime,Theta0,Theta1,Theta2,
     Distance,Value,ThetaPrimeRange,ValueV,Visby,
     XMouse1,YMouse1,XMOuse2,YMouse2,vis,Factor         : Real;
     M,N,IMin,IMax,ColNum,RangeValue,
     NumberofPoints,Number,RangeNumber                  : Integer;
     Intensity1,Bracket,XValues,DeltaTheta,V            : DVector;
     AString                                            : String;
     Inside                                             : Boolean;
     Wait                                               : TWaitMessage;
BEGIN
   Sliders.DrawAll;
   ZoomSourceButtons.DrawAll;
   ZoomSlitButtons.DrawAll;
   MainHotKeys.Clear;
   HotKeys.Display;
   DefineScale(9,Xmin,XMax,0,1.5);
   NumberofPoints := 300;
   Number := 20;
   Intensity1.init(NumberofPoints);
   Bracket.init(Number);
   XValues.init(NumberofPoints);
   V.init(200);
   DeltaTheta.init(201);
   SetColor(White);
   Theta1 := ArcSin(SourceTop/sqrt(sqr(SourceTop)+sqr(SourceDistancefromSlits)));
   Theta2 := ArcSin(SourceBottom/sqrt(sqr(SourceBottom)+sqr(SourceDistancefromSlits)));
   DeltaTheta.put(1,0.02);
   FOR N := 1 to 200 DO
   BEGIN
      V.put(N,abs(1/(pi*DeltaTheta.Value(N))*sin(pi*DeltaTheta.Value(N))));
      DeltaTheta.put(N + 1,DeltaTheta.Value(N) + 0.02);
   END;
   SelectviewPort(20);
   DeltaTheta1 := Theta2 - Theta1;
   RubOut(52,13,10,Black);
   AString := concat('Slit Separation * D',chr(233),'/Lambda = ');
   Print(22,13,AString);
   SetColor(lightgreen);
   Print(52,13,NumSTr(SlitSeparation*deltaTheta1/(1e-6*Lambda.Value(1)),0,2));
   SetColor(white);
   PrintXY(0.02,0.92,'Source Sliders');
   PrintXY(0.8,0.92,'Slit Sliders');
   DefineScale(8,0,4,0,1);
   OpenViewPort(8);
   SelectScale(8);
   Axis(0,0,1,0.1);
   AString := ('Visibility Function');
   Print(8,1,AString);
   AString := concat('Slit Separation * D',chr(233),'/Lambda');
   PrintXY(0.32,0.53,AString);
   PlotDVectors(DeltaTheta,V,1,V.Size);
   Print(16,4,'Visibility');
   Print(22,5,'Axis');
   ValueV := SlitSeparation*deltaTheta1/(1e-6*Lambda.Value(1));
   VisBy :=abs(1/(pi*ValueV)*sin(pi*ValueV));
   SetColor(lightgreen);
   Print(27,4,NumStr(Visby,0,2));
   Print(27,5,NUmStr(ValueV,0,2));
   PlotLine(Valuev,0,Valuev,1);
   SetColor(white);
   Wait.Show;
   TermA := 2*pi*SlitSeparation/(1e-6*Lambda.Value(1));
   Length :=  XMax - XMin;
   x := Xmin;
   Factor := 1/SourceDistancefromSlits;
   SourceWidth :=- Factor*(SourceTop - SourceBottom);
   FOR N := 1 to NumberofPoints DO
   BEGIN
      Theta := SourceTop/SourceDistancefromSlits;
      FOR M := 1 to Number Do    {Choose Position on Source - Theta}
      BEGIN
         ThetaPrime :=  ArcSin(x/(sqr(x) + sqr(ScreenDistance)));
         Alpha := 2*pi*SlitWidth*(ThetaPrime - Theta)/(1e-6*Lambda.Value(1));
         IF Alpha = 0 THEN
           Alpha := 1e-9;
         Term := sqr(sin(alpha)/alpha)*(1 + cos(TermA*(ThetaPrime - Theta)));
         Bracket.put(M,Term);
         Theta := Theta + SourceWidth/(Number - 1);
      END;
      Intensity1.put(N,Simpson(Bracket));
      XValues.put(N,x);
      x := x + Length/(NumberofPoints - 1);
      IF N mod 20 = 0 THEN Wait.UpDate;
   END;
   Intensity1.MinMax(IMin,Min,IMax,Max);
   FOR N := 1 to Numberofpoints DO
       Intensity1.put(N,Intensity1.Value(N)/Max);
   Wait.Hide;
   OpenViewPort(9);
   SelectScale(9);
   Axis(0,0,(XMax-XMin)/5,0.5);
   PutLabel(bottom,'Position on Screen');
   ColNum := 1;
   Colours(ColNum,Lambda);
   PlotDVectors(XValues,Intensity1,1,XValues.size);
   SetColor(White);
   Active := True;
   Intensity1.free;
   Bracket.free;
   XValues.free;
   DeltaTheta.free;
   V.free;
END;

{********************** TStellarTwo ***********************}

PROCEDURE TStellarTwo.Init;
BEGIN
   Lambda.init(1);
   Sliders.init;
   Buttons.init;
   ZoomButtons.init;
   SetDefaults;
END;

PROCEDURE TStellarTwo.Done;
BEGIN
   Lambda.free;
   Sliders.Done;
   Buttons.Done;
   ZoomButtons.Done;
END;

PROCEDURE TStellarTwo.SetDefaults;
BEGIN
   Lambda.put(1,550);
   SlitWidth        := 0.002;
   SlitSeparation   := 0.025;
   ScreenDistance   := 1;  {metres}
   StarAngle        := 5.84; {Castor}
   XMax             := 0.0002;
   XMin             := -0.0002;
   DistancealphaC   := 1/0.760;
   DistanceCastor   := 1/0.074;
   DistanceKruger60 := 1/0.253;
   DistancepEri     := 1/0.170;
   GraphBackColor   := Black;
   SetUp;
END;

PROCEDURE TStellarTwo.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(6,0.2,0.8,0.15,0.45);       {Bottom}
   SetUpHotKeys;
   SetUpSlidersandButtons;
   TwoSources;
END;

PROCEDURE TStellarTwo.SetUpSlidersandButtons;
BEGIN
   WITH  Sliders DO
   BEGIN
      create(1,400,800,Lambda.Value(1),0.08,0.06,0.4,0,'400','800','Wavelength nm',True);
      create(2,0.002,0.05,SlitSeparation,0.9,0.06,0.6,5,'0.002','0.05','Separation m',True);
   END;
   WITH Buttons DO
   BEGIN
      create(1,0.1,0.8,'Castor');
      create(2,0.1,0.75,'Kruger 60');
      create(3,0.1,0.7,'pEri ');
      create(4,0.1,0.65,'Alpha Centauri');
      Number := 1;
   END;
   WITH ZoomButtons DO
   BEGIN
      init;
      create(1,0.85,0.65,'Zoom');
      create(2,0.85,0.7,'Normal');
      Number := 2;
   END;
END;

PROCEDURE TStellarTwo.HandleSliders;
BEGIN
   Lambda.put(1,Sliders.Value(1));
   SlitSeparation := Sliders.Value(2);
   SelectViewPort(20);
   RubOut(46,10,5,Black);
   Arc := 1e-9*Lambda.Value(1)*3600*180/(2*pi*SlitSeparation);
   Print2(46,10,2,Arc,lightgreen,black);
   TwoSources;
END;

PROCEDURE TStellarTwo.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TStellarTwo.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN17');
   2: BEGIN
         sliders.erase(2,Black);
         Done;
         init;
      END;
   3: IF Menu.Chosen THEN HandleMenu;
   END;
END;

PROCEDURE TStellarTwo.CheckMouse;
VAR Min,Max : Real;
BEGIN
   IF Active THEN
   BEGIN
      IF Sliders.Changed THEN HandleSliders;

      IF Buttons.Changed THEN
      BEGIN
         IF Buttons.Number = 1 THEN
         BEGIN
            StarAngle := 5.84; {Castor}
            TwoSources;
         END;
         IF Buttons.Number = 2 THEN
         BEGIN
            StarAngle := 2.362; {Kruger 60}
            TwoSources;
         END;
         IF Buttons.Number = 3 THEN
         BEGIN
            StarAngle := 8.025; {pEri}
            TwoSources;
         END;
         IF Buttons.Number = 4 THEN
         BEGIN
            StarAngle := 17.66;   {alpha Centauri}
            TwoSources;
         END;

         IF (ZoomButtons.Number = 1) THEN
         WITH Sliders DO
         BEGIN
            erase(2,black);
            delete(2);
            create(2,0.002,0.05,SlitSeparation,0.9,0.06,0.6,5,'0.002','0.05','Separation m',True);
            DrawAll;
            ZoomButtons.Number := 2;
            ZoomButtons.DrawAll;
         END;
      END;

      IF ZoomButtons.Changed THEN
      BEGIN
         IF (ZoomButtons.Number = 2) THEN
         WITH Sliders DO
         BEGIN
            erase(2,black);
            delete(2);
            create(2,0.002,0.05,SlitSeparation,0.9,0.06,0.6,5,'0.002','0.05','Separation m',True);
            DrawAll;
         END;

         IF (ZoomButtons.number = 1) THEN
         BEGIN
            Min := SlitSeparation - 0.001;
              IF Min < 0.002 THEN Min := 0.002;
            Max := Min + 0.002;
               IF Max > 0.099 THEN Max := 0.1;
            WITH Sliders Do
            BEGIN
               erase(2,black);
               Delete(2);
               create(2,Min,Max,SlitSeparation,0.9,0.06,0.6,5,
               NumStr(Min,1,3),NumStr(Max,1,3),'Separation m',True);
               DrawAll;
            END;
         END;
      END;
   END;
END;

PROCEDURE TStellarTwo.TwoSources;
VAR  Length,Alpha,Term,X,
     A,Theta1,Theta2                                     :  Real;
     N,NumberofPoints,ColNum                             :  Integer;
     Intensity1,Intensity2,Intensity3,Intensity4,
     Intensity5,Intensity6,XValues                       :  DVector;
     AString                                             :  String;
BEGIN
   SetColor(yellow);
   PrintXY(0.15,0.9,'MICHELSON STELLAR INTERFEROMETER - DOUBLE STAR SEPARATION');
   SetColor(white);
   Buttons.DrawAll;
   ZoomButtons.DrawAll;
   Sliders.DrawAll;
   PrintXY(0.82,0.78,'Mirror Slider');
   MainHotKeys.Clear;
   HotKeys.Display;
   Active := True;
   NumberofPoints := 300;
   Intensity1.init(NumberofPoints);
   Intensity2.init(NumberofPoints);
   Intensity3.init(NumberofPoints);
   Intensity4.init(NumberofPoints);
   Intensity5.init(NumberofPoints);
   Intensity6.init(NumberofPoints);
   XValues.init(NumberofPoints);
   DefineScale(6,Xmin,XMax,0,8);
   SelectViewPort(20);
   RubOut(46,10,6,black);
   SetColor(White);
   Arc := 1e-9*Lambda.Value(1)*3600*180/(2*pi*SlitSeparation);
   Print(5,10,'Star Separation from Mirror Separation = ');
   Print(52,10,'seconds of arc');
   SetColor(lightgreen);
   Print(46,10,NumStr(Arc,1,2));
   SetColor(white);
   RubOutXY(0.31,0.807,35,black);
   RubOutXY(0.31,0.757,35,black);
   RubOutXY(0.31,0.707,35,black);
   RubOutXY(0.31,0.657,35,black);
   IF Buttons.Number = 4 THEN
   BEGIN
      AString := concat('Separation =      AU');
      PrintXY(0.31,0.657,AString);
      SetColor(lightGreen);
      PrintXY(0.472,0.657,NumStr(Arc*DistanceAlphaC,1,1));
   END;
   IF Buttons.Number = 1 THEN
   BEGIN
      AString := concat('Separation =      AU');
      PrintXY(0.31,0.807,AString);
      SetColor(lightGreen);
      PrintXY(0.472,0.807,NumStr(Arc*DistanceCastor,1,1));
   END;
   IF Buttons.Number = 2 THEN
   BEGIN
      AString := concat('Separation =      AU');
      PrintXY(0.31,0.757,AString);
      SetColor(lightGreen);
      PrintXY(0.472,0.757,NumStr(Arc*DistanceKruger60,1,1));
   END;
   IF Buttons.Number = 3 THEN
   BEGIN
      AString := concat('Separation =      AU');
      PrintXY(0.31,0.707,AString);
      SetColor(lightGreen);
      PrintXY(0.472,0.707,NumStr(Arc*DistancepEri,1,1));
   END;
   Theta1 := -pi/180*StarAngle/(2*60*60);     {in radians}
   Theta2 := pi/180*StarAngle/(2*60*60);
   X := XMin;
   Length := XMax - XMin;
   FOR N := 1 to NumberofPoints DO
   BEGIN
      A := ArcSin(x/(sqr(x) + sqr(ScreenDistance)));;
      Alpha := 2*pi*SlitWidth*(A - Theta1)/(1e-6*Lambda.Value(1));
      IF Alpha = 0 THEN
         Alpha := 1e-9;
      Term := sqr(Sin(alpha)/alpha);
      Intensity1.put(N,Term);
      XValues.put(N,x);
      x := x + length/(NumberofPoints - 1);
   END;

   X := XMin;
   FOR N :=  1 to NumberofPoints Do
   BEGIN
      A := ArcSin(x/(sqr(x) + sqr(ScreenDistance)));
      Intensity2.put(N,2*Intensity1.Value(N)*
      (1 + cos(2*pi*SlitSeparation*1000*(A - Theta1)/(1e-6*Lambda.Value(1))) ) );
       XValues.put(N,x);
      x := x + length/(NumberofPoints - 1);
   END;
   X := XMin;
   FOR N := 1 to NumberofPoints DO
   BEGIN
      A := ArcSin(x/(sqr(x) + sqr(ScreenDistance)));
      Alpha := 2*pi*SlitWidth*(A - Theta2)/(1e-6*Lambda.Value(1));
      IF Alpha = 0 THEN
         Alpha := 1e-9;
      Term := sqr(Sin(alpha)/alpha);
      Intensity4.put(N,Term);       {one Slit}
      XValues.put(N,x);
      x := x + Length/(NumberofPoints -1);
   END;

   X := XMin;
   FOR N :=  1 to NumberofPoints Do
   BEGIN
       A := ArcSin(x/(sqr(x) + sqr(ScreenDistance)));
       Intensity5.put(N,2*Intensity4.Value(N)*
      (1 + cos(2*pi*SlitSeparation*1000*(A - Theta2)/(1e-6*Lambda.Value(1))) ) );
       XValues.put(N,x);
      x := x + length/(NumberofPoints - 1);
   END;
   FOR N := 1 to NumberofPoints DO
   BEGIN
      Intensity6.put(N,Intensity2.Value(N) + Intensity5.Value(N));
   END;
   OpenViewPort(6);
   SelectScale(6);
   Axis(0,0,1000*(XMax-XMin)/5,4);
   SetColor(white);
   PutLabel(top,'Fringe Pattern');
   PutLabel(bottom,'Position on Screen');
   ColNum := 1;
   Colours(ColNUm,Lambda);
   PlotDVectors(XValues,Intensity6,1,XValues.size);
   SetColor(white);
   { Two sources two slits }
 Intensity1.free;
 Intensity2.free;
 Intensity3.free;
 Intensity4.free;
 Intensity5.free;
 Intensity6.free;
 XValues.free;
END;

{************************** TStellarDiameter *********************}

PROCEDURE TStellarDiameter.Init;
BEGIN
   Lambda.init(1);
   Sliders.init;
   Buttons.init;
   ZoomButtons.init;
   SetDefaults;
 END;

PROCEDURE TStellarDiameter.Done;
BEGIN
   Lambda.free;
   Sliders.Done;
   Buttons.Done;
   ZoomButtons.Done;
END;

PROCEDURE TStellarDiameter.SetDefaults;
BEGIN
   Lambda.put(1,550);
   SlitWidth           := 0.002;
   SlitSeparation      := 1; {m}
   XMax                := 0.000001;
   XMin                := -0.000001;
   ScreenDistance      := 1;
   StarAngle           := 0.0473; {seconds of arc - default Betelgeuse}
   DistanceBetelgeuse  := 159;
   DistanceAldeberan   := 21;
   DistanceArcturus    := 11;
   GraphBackColor      := Black;
   SetUp;
END;

PROCEDURE TStellarDiameter.SetUp;
BEGIN
   IF NOT Active THEN CloseViewPort(20);
   DefineViewPort(8,0.3,0.7,0.58,0.90);
   DefineViewPort(9,0.2,0.8,0.12,0.38);
   SetUpHotKeys;
   SetUpSlidersandButtons;
   Diameter;
END;

PROCEDURE TStellarDiameter.SetUpSlidersandButtons;
BEGIN
   WITH Sliders DO
   BEGIN
      create(1,400,800,Lambda.Value(1),0.08,0.06,0.4,0,'400','800','Wavelength nm',True);
      create(2,0.01,10,SlitSeparation,0.9,0.06,0.6,4,'0.01','10','Separation m',True);
   END;
   WITH Buttons DO
   BEGIN
      create(1,0.1,0.8,'Betelgeuse');
      create(2,0.1,0.75,'Aldeberan');
      create(3,0.1,0.7,'Arcturus');
      Number := 1;
   END;
   WITH ZoomButtons DO
   BEGIN
      init;
      create(1,0.85,0.65,'Zoom');
      create(2,0.85,0.7,'Normal');
      Number := 2;
   END;
END;

PROCEDURE TStellarDiameter.HandleSliders;
Var A: Real;
BEGIN
   Lambda.put(1,Sliders.Value(1));
   SlitSeparation := Sliders.Value(2);
   SelectViewPort(20);
   RubOut(52,13,8,Black);
   NewA := SlitSeparation/1.22*DeltaTheta1/(1e-9*Lambda.Value(1));
   Print2(52,13,3,NewA,lightgreen,black);
   RubOut(40,10,8,black);
   A := 1.22*1e-9*Lambda.Value(1)*3600*180/(pi*SlitSeparation);
   SetColor(lightgreen);
   Print2(41,10,4,A,lightgreen,black);
   Diameter;
END;

PROCEDURE TStellarDiameter.SetUpHotKeys;
BEGIN
  WITH HotKeys DO
  BEGIN
      Init(3);
      Key[1] := 'F1-Help';
      Key[2] := 'F9-Defaults';
      Key[3] := 'F10-Menu';
   END;
END;

PROCEDURE TStellarDiameter.HandleHotKeys(KeyNum : Byte);
BEGIN
   CASE KeyNum OF
   1: ShowHelp('diffract.hlp','HELPSCREEN18');
   2: BEGIN
         sliders.erase(2,Black);
         Done;
         init;
      END;
   3: IF Menu.Chosen THEN HandleMenu;
   END;
END;

PROCEDURE TStellarDiameter.CheckMouse;
VAR Max,Min                  : Real;
BEGIN
   IF Active THEN
   BEGIN
      IF Sliders.Changed THEN HandleSliders;

      IF Buttons.Changed THEN
      BEGIN
         IF Buttons.Number = 1 THEN
         BEGIN
            StarAngle := 0.0473; {Betelgeuse}
            Diameter;
         END;
         IF Buttons.Number = 2 THEN
         BEGIN
            StarAngle := 0.0198; {Aldeberan}
            Diameter;
         END;
         IF Buttons.Number = 3 THEN
         BEGIN
            StarAngle := 0.0194; {Arcturus}
            Diameter;
         END;

         IF (ZoomButtons.Number = 1) THEN
         WITH Sliders DO
         BEGIN
            erase(2,black);
            delete(2);
            create(2,0.01,10,SlitSeparation,0.9,0.06,0.6,4,'0.01','10','Separation m',True);
            DrawAll;
            ZoomButtons.Number := 2;
            ZoomButtons.DrawAll;
         END;
      END;

      IF ZoomButtons.Changed THEN
      BEGIN
         IF (ZoomButtons.Number = 2) THEN
         WITH Sliders DO
         BEGIN
            erase(2,black);
            delete(2);
            create(2,0.01,10,SlitSeparation,0.9,0.06,0.6,4,'0.01','10','Separation m',True);
            DrawAll;
         END;

         IF (ZoomButtons.number = 1) THEN
         BEGIN
            Min := SlitSeparation - 0.5;
            IF Min < 0.025 THEN Min := 0.01;
            Max := Min + 1;
            IF Max > 9 THEN Max := 10;
            WITH Sliders Do
            BEGIN
               erase(2,black);
               delete(2);
               create(2,Min,Max,SlitSeparation,0.9,0.06,0.6,4,
               NumStr(Min,1,2),NumStr(Max,1,2),'Separation m',True);
               DrawAll;
            END;
         END;
      END;
   END;
END;

PROCEDURE TStellarDiameter.Diameter;
VAR  Length,Alpha,Term,Min,Max,x,y,SourceWidth,TermA,
     ThetaPrime,Theta1,Theta2,Arc,Theta               : Real;
     M,N,IMin,IMax,ColNum,
     NumberofPoints,Number                            : Integer;
     Intensity1,Bracket,XValues                       : DVector;
     AString                                          : String;
     Wait                                             : TWaitMessage;
BEGIN
   SetColor(yellow);
   PrintXY(0.2,0.9,'MICHELSON STELLAR INTERFEROMETER - STAR DIAMETERS');
   SetColor(white);
   Sliders.DrawAll;
   PrintXY(0.82,0.78,'Mirror Slider');
   MainHotKeys.Clear;
   HotKeys.Display;
   Buttons.DrawAll;
   ZoomButtons.DrawAll;
   Active := True;
   DefineScale(9,Xmin,XMax,0,1.5);
   NumberofPoints := 300;
   Number := 20;
   Intensity1.init(NumberofPoints);
   Bracket.init(Number);
   XValues.init(NumberofPoints);
   SetColor(White);
   Theta1 := -pi/180*StarAngle/(2*60*60);     {in radians}
   Theta2 := pi/180*StarAngle/(2*60*60);
   DeltaTheta1 := Theta2 - Theta1;
   SelectViewPort(20);
   RubOut(51,13,6,black);
   Print(14,13,'Slit Separation * DeltaTheta/Lambda = ');
   SetColor(lightgreen);
   NewA := SlitSeparation/1.22*deltaTheta1/(1e-9*Lambda.Value(1));
   Print(52,13,NumSTr(NewA,5,3));
   SetColor(white);
   Print(5,10,'Star Angle from Mirror Separation = ');
   RubOut(40,10,8,black);
   Arc := 1.22*1e-9*Lambda.Value(1)*3600*180/(pi*SlitSeparation);
   SetColor(lightgreen);
   Print(41,10,NumStr(Arc,1,4));
   SetColor(white);
   Print(49,10,'seconds of arc');
   SetColor(white);
   RubOutXY(0.27,0.807,35,black);
   RubOutXY(0.27,0.757,35,black);
   RubOutXY(0.27,0.707,35,black);
   IF Buttons.Number = 1 THEN
   BEGIN
      AString := concat('Diameter = ',
               NumStr(Arc*DistanceBetelgeuse/9.28e-3,1,1),' Sun Diameters');
      PrintXY(0.27,0.807,AString);
      SetColor(lightGreen);
      PrintXY(0.408,0.807,NumStr(Arc*DistanceBetelgeuse/9.28e-3,1,1));
   END;
   IF Buttons.Number = 2 THEN
   BEGIN
      AString := concat('Diameter = ',
               NumStr(Arc*DistanceAldeberan/9.28e-3,1,1),' Sun Diameters');
      PrintXY(0.27,0.757,AString);
      SetColor(lightGreen);
      PrintXY(0.408,0.757,NumStr(Arc*DistanceAldeberan/9.28e-3,1,1));
   END;
   IF Buttons.Number = 3 THEN
   BEGIN
      AString := concat('Diameter = ',
               NumStr(Arc*DistanceArcturus/9.28e-3,1,1),' Sun Diameters');
      PrintXY(0.27,0.707,AString);
      SetColor(lightGreen);
      PrintXY(0.408,0.707,NumStr(Arc*DistanceArcturus/9.28e-3,1,1));
   END;
   Wait.Show;

   TermA := 2*pi*SlitSeparation/1.22/(1e-9*Lambda.Value(1));
   Length :=  XMax - XMin;
   x := Xmin;
   SourceWidth := -(Theta2 - Theta1);
   FOR N := 1 to NumberofPoints DO
   BEGIN
      Theta := Theta1;  {Choose Position on Source - Theta}
      FOR M := 1 to Number DO
      BEGIN
         ThetaPrime := ArcSin(x/(sqr(x) + sqr(ScreenDistance)));
         Alpha := 2*pi*SlitWidth*0.001*(ThetaPrime - Theta1)/(1e-9*Lambda.Value(1));
         IF Alpha = 0 THEN
           Alpha := 1e-9;
         Term := sqr(sin(alpha)/alpha)*(1 + cos(TermA*(ThetaPrime - Theta)));
         Bracket.put(M,Term);
         Theta := Theta+ SourceWidth/(Number - 1);
      END;
      Intensity1.put(N,Simpson(Bracket));
      XValues.put(N,x);
      x := x + Length/(NumberofPoints - 1);
      IF N mod 20 = 0 THEN Wait.UpDate;
   END;

   Intensity1.MinMax(IMin,Min,IMax,Max);
   FOR N := 1 to Numberofpoints DO
       Intensity1.put(N,Intensity1.Value(N)/Max);

   Wait.Hide;
   OpenViewPort(9);
   SelectScale(9);
   Axis(0,0,1000*(XMax-XMin)/5,0.5);
   PutLabel(bottom,'Position on Screen ');
   ColNum := 1;
   Colours(ColNum,Lambda);
   PlotDVectors(XValues,Intensity1,1,XValues.size);
   SetColor(White);
   Intensity1.free;
   Bracket.free;
   XValues.free;
END;


          {*************  Main Program  *************}

BEGIN
   CUPSinit;
   SetUpMenu;
   SetUpHotKeys;
   ShowHelp('Diffract.hlp','HELPSCREEN9');
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




