           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.5 (95/02/13) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

PROGRAM CoupledOscillators;
{+-------------------------------------------------------------------}
{| PROCEDURES FOR COUPLED OSCILLATORS - SIM2 for MECHANICS for CUPS
{| Version E
{| Author:  Randall Jones
{| Uses 1-14-93 CUPS utilities
{| Date:  {1/93}
{+-------------------------------------------------------------------}
USES CRT, Printer, Graph,
     CUPS, CUPSMUPP, CUPSgui, CUPSGrph, CUPSfunc, CUPSproc,
     CO_Phys;
CONST
   MaxData = 100;
   MaxRadius = 8;
TYPE
   FreeAll = (FreeMasses,AllMasses);
   EquilCur = (Equil,CurPos);
VAR
   NewGenScreen:TInputScreen;
   MainMenu: TMenu;
   HotKeys,ModMassesKeys,ModSpringsKeys,MassSelectKeys:THotKeys;
   ChangeNormalModeKeys,DisplacementKeys:THotKeys;
   FileBar: TBarMenu;
   ExtForceSliders:TSliders;
   Environment:TEnvironment;
   Anim:TFrames;
   RunFlag,DoneFlag:Boolean;
   AnimFact,MaxAnimFact,NumIntFact:Real;
   KeyHit:Byte;
   x,y:Integer;
   Data1,Data2:ARRAY[0..MaxData-1] of Real;
   NumPnts:Integer;
   PlotObj: ARRAY[1..2] OF Word;
{+--------------------------------------------------------------------------}
{| Utilities
{+--------------------------------------------------------------------------}
   PROCEDURE Debounce;
  {+-------------------------------------------------------------------}
  {| This procedure pauses until the mouse button is released.  It
  {| prevents a mouse routine from registering two clicks when only
  {| one actually occured.
  {+-------------------------------------------------------------------}
   VAR
      x,y,button:Integer;
   BEGIN
      REPEAT
         MouseGlobalPosn(x,y,button);
      UNTIL button=0;
   END; 

   PROCEDURE AnimClean;
  {+-------------------------------------------------------------------}
  {| This procedure does the "clean" of the animation object for
  {| screen 1.  The clear is done at the start of the drawing cycle
  {| so that both screens will have images whenever animation is halted.
  {+-------------------------------------------------------------------}
   BEGIN
      WITH Views[1] DO
      BEGIN
         SetViewport(Vx1+2,Vy1+2,Vx2-2,Vy2-2,true);
         ClearViewport;
      END;
   END;

   PROCEDURE GetObjectRep(VAR Config:ConfigType);
  {+-------------------------------------------------------------------}
  {| This procedure specifies color and size of mass and spring images
  {| Mass images are generated and stored in the MassVal data structure.
  {+-------------------------------------------------------------------}
      PROCEDURE GetCircle(VAR MassVal:MassInfo);
     {+----------------------------------------------------------------}
     {| This procedure draws the images representing the masses and
     {| saves them in the MassVal data structure.
     {+----------------------------------------------------------------}
      VAR
         HoldColor:Word;
         Diam:Word;
      BEGIN
         HoldColor := GetColor;
         WITH MassVal DO
            BEGIN
               Diam := 2*Radius;
               SetViewPort(0,0,Diam,Diam,ClipOn); ClearViewPort;
               SetColor(Color);
               SetFillStyle(SolidFill,Color);
               FillEllipse(Radius,Radius,Radius,Radius);
               GetImage(0,0,Diam,Diam,MassImage);
               ClearViewPort;
               SetColor(HoldColor);
            END;
      END; {GetCircle}

   BEGIN {GetObjectRep}
      WITH Config DO
         BEGIN
            WITH MassVals[Inf] DO BEGIN Radius := 3; Color := 15; END;
            GetCircle(MassVals[Inf]);
            WITH MassVals[mA] DO BEGIN Radius := 5; Color := 14; END;
            GetCircle(MassVals[mA]);
            WITH MassVals[mB] DO BEGIN Radius := 6; Color := 13; END;
            GetCircle(MassVals[mB]);
            WITH MassVals[mC] DO BEGIN Radius := 7; Color := 12; END;
            GetCircle(MassVals[mC]);
            WITH SpringVals[sA] DO BEGIN Width := 0.2; Color := 11; END;
            WITH SpringVals[sB] DO BEGIN Width := 0.3; Color := 10; END;
            WITH SpringVals[sC] DO BEGIN Width := 0.4; Color := 9; END;
            WITH SpringVals[sD] DO BEGIN Width := 0.5; Color := 7; END;
         END;
   END; {GetObjectRep}

   PROCEDURE DrawVector(x,y,Vx,Vy:Real);
  {+------------------------------------------------------------------}
  {| This procedure draws a vector with components Vx and Vy at the
  {| point (x,y).  It uses the currently selected viewport and scale.
  {+------------------------------------------------------------------}
   BEGIN
      HideCursor;
      Vector(MapX(x),MapY(y),MapX(x+Vx),MapY(y+Vy));
      ShowCursor;
   END;

   PROCEDURE ShowArrowKeys(VAR Config:ConfigType);
  {+------------------------------------------------------------------}
  {| This procedure displays the four arrow keys as a single HotKey.
  {+------------------------------------------------------------------}
   CONST
      HotKeysColor=LightRed;
   BEGIN
      Environment.Save;
      SetColor(HotKeysColor);
      SetWriteMode(NormalPut);
      SetViewPort (0, GetMaxY -RowHt DIV 2 -14,
                      GetMaxX, GetMaxY, true);
      IF Config.TwoDim
         THEN Print(62,1,chr(24)+chr(25)+chr(27)+chr(26)+'-')
         ELSE Print(62,1,chr(27)+chr(26)+'  -');
      SetColor(White);
      Print(67,1,'Move');
      Environment.Reset;
{      SetWriteMode(XORPut);}
   END;

{+--------------------------------------------------------------------------}
{| Mass and spring drawing utilities;
{+--------------------------------------------------------------------------}
   PROCEDURE PutMass(MassVal:MassInfo; x1,y1:Real; HowPut:Byte);
  {+------------------------------------------------------------------}
  {| This procedure puts the object represented in MassInfo on the
  {| graph at x1,y1
  {+------------------------------------------------------------------}
   VAR
      xScr,yScr:Integer;
   BEGIN
      Map(x1,y1,xScr,yScr);
      WITH MassVal DO
         PutImage(xScr-Radius,yScr-Radius,MassImage,HowPut);
   END;

   PROCEDURE GetMassPos(VAR Config:ConfigType; Where:EquilCur;
                          MassNum:Word; VAR x1,y1:Real);
   BEGIN
      WITH Config DO
         BEGIN
            x1 := Masses[MassNum].xEquilPos;
            y1 := Masses[MassNum].yEquilPos;
            IF (Where=CurPos) AND (MassNum<=NumFreeMasses) THEN
               BEGIN
                  x1 := x1 + Displacements.Value(MassNum);
                  IF TwoDim THEN
                     y1 := y1 + Displacements.Value(NumFreeMasses+MassNum);
               END;
         END;
   END;

   PROCEDURE DrawMass(VAR Config:ConfigType; MassNum:Integer;
                      Where:EquilCur; HowPut:Byte);
   VAR
      xPos,yPos:Real;
   BEGIN
      GetMassPos(Config,Where,MassNum,xPos,yPos);
      WITH Config DO
         PutMass(MassVals[Masses[MassNum].Class],xPos,yPos,HowPut);
   END;

   PROCEDURE DrawMasses(VAR Config:ConfigType;
                        WhichMasses:FreeAll;
                        Where:EquilCur;
                        HowPut:Byte);
   VAR
      MassNum:Integer;
   BEGIN
      SelectViewPort(1); SelectScale(1);
      WITH Config DO
         CASE WhichMasses OF
            FreeMasses: {Draw free masses}
               FOR MassNum := 1 to NumFreeMasses DO
                  DrawMass(Config,MassNum,Where,HowPut);
            AllMasses: {Draw all masses}
               FOR MassNum := 1 to NumMasses DO
                  DrawMass(Config,MassNum,Where,HowPut);
         END; {CASE}
   END;

   PROCEDURE PutSpring(SpringVal:SpringInfo; x1,y1,x2,y2:Real);
  {+------------------------------------------------------------------}
  {| This procedure draws a spring of width, SpringWidth between the
  {| points (x1,y1) and (x2,y2).  It uses the currently selected
  {| viewport and scale.
  {+------------------------------------------------------------------}
   VAR
      nDiv,i: Integer;
      x3,y3,x4,y4: Real;
      dx,dy,xOld,yOld,xNew,yNew: Real;
      dxUp,dyUp,dxDown,dyDown: Real;
      CosPhi,SinPhi: Real;
      SpringLen,StraightLen,Distance: Real;
      Loop:integer;
      HoldColor:Word;
      EquilDist,EquilLen:real;

   BEGIN
      HoldColor := GetColor;
      SetColor(SpringVal.Color);
   Distance := sqrt(ABS(sqr(x2-x1)+sqr(y2-y1)));
   CosPhi := (x2-x1)/Distance;
   SinPhi := (y2-y1)/Distance;
   EquilLen := 0.8*Distance;
   EquilDist:= Distance;
   StraightLen := (EquilDist - EquilLen)/2.0;
   SpringLen:= Distance - 2*StraightLen;
   x3 := x1 + StraightLen*CosPhi; y3 := y1 + StraightLen*SinPhi;
   x4 := x2 - StraightLen*CosPhi; y4 := y2 - StraightLen*SinPhi;

   PlotLine(x1,y1,x3,y3);
   PlotLine(x4,y4,x2,y2);

   xOld := x3; yOld := y3;
   nDiv:=8;
   dx := SpringLen/(4*nDiv);
   dy := SpringVal.Width;
      dxUp :=  dx*CosPhi + (-dy)*SinPhi;
      dyUp := -dx*SinPhi + (-dy)*CosPhi;
      dxDown :=  dx*CosPhi + dy*SinPhi;
      dyDown := -dx*SinPhi + dy*CosPhi;

      xNew := xOld + dxUp; yNew := yOld - dyUp;
      PlotLine(xOld,yOld,xNew,yNew);
      xOld := xNew ; yOld := yNew;
      FOR i := 1 to nDiv-1 DO
         BEGIN
            xNew := xOld + 2*dxDown;  yNew := yOld - 2*dyDown;
            PlotLine(xOld,yOld,xNew,yNew);
            xOld :=xNew; yOld := yNew;

            xNew := xOld + 2*dxUp;  yNew := yOld - 2*dyUp;
            PlotLine(xOld,yOld,xNew,yNew);
            xOld := xNew; yOld := yNew;
         END;
      xNew := xOld + 2*dxDown;  yNew := yOld - 2*dyDown;
      PlotLine(xOld,yOld,xNew,yNew);
      xOld :=xNew; yOld := yNew;

      xNew := xOld + dxUp; yNew := yOld - dyUp;
      PlotLine(xOld,yOld,xNew,yNew);
      xOld := xNew ; yOld := yNew;
      SetColor(HoldColor);  
   END; { PutSpring}

   PROCEDURE GetSpringPos(VAR Config:ConfigType; Where:EquilCur;
                          SpringNum:Word; VAR x1,y1,x2,y2:Real);
   VAR
      Mass1,Mass2:Word;
   BEGIN
      Mass1 := Config.Springs[SpringNum].End1;
      Mass2 := Config.Springs[SpringNum].End2;
      GetMassPos(Config,Where,Mass1,x1,y1);
      GetMassPos(Config,Where,Mass2,x2,y2);
   END;

   PROCEDURE DrawSpring(VAR Config:ConfigType; SpringNum:Integer;
                        Where:EquilCur; SpringCode:Boolean; HowPut:Byte);
   VAR
      x1,y1,x2,y2:Real;
      SpringType:SpringTypes;
   BEGIN
      SetWriteMode(HowPut);
      WITH Config DO
         BEGIN
            GetSpringPos(Config,Where,SpringNum,x1,y1,x2,y2);
            SpringType := Springs[SpringNum].Class;
            IF SpringCode THEN PutSpring(SpringVals[SpringType],x1,y1,x2,y2)
                          ELSE PlotLine(x1,y1,x2,y2);
         END;
      SetWriteMode(NormalPut);
   END;

   PROCEDURE DrawSprings(VAR Config:ConfigType; Where:EquilCur;
                         SpringCode:Boolean; HowPut:Byte);
   VAR
      SpringNum:Word;
      x1,y1,x2,y2:Real;
      SpringType:SpringTypes;
   BEGIN
      SelectViewPort(1); SelectScale(1);
      WITH Config DO
         FOR SpringNum := 1 to NumSprings DO
            DrawSpring(Config,SpringNum,Where,SpringCode,HowPut);
   END;

   PROCEDURE HiliteSelectedMass(VAR Config:ConfigType; MassNum:Integer);
   VAR
      xPos,yPos:Real;
      xScr,yScr:Integer;
   BEGIN
      SelectViewPort(1); SelectScale(1);
      GetMassPos(Config,Equil,MassNum,xPos,yPos);
      Map(xPos,yPos,xScr,yScr);
      SetWriteMode(XORPut); HideCursor;
      Rectangle(xScr-MaxRadius,yScr-MaxRadius,
                xScr+MaxRadius,yScr+MaxRadius);
      SetWriteMode(NormalPut); ShowCursor;
   END;

   PROCEDURE HiliteSelectedSpring(VAR Config:ConfigType;
                                      SpringNum:Integer);
   BEGIN
      SelectViewPort(1); SelectScale(1);
      DrawSpring(Config,SpringNum,Equil,False,XORPut);
   END;

   PROCEDURE DrawDisplacementVectors(VAR Config:ConfigType);
  {+------------------------------------------------------------------}
  {| This procedure draws vectors on the masses representing the
  {| current displacements.
  {+------------------------------------------------------------------}
   VAR
      MassNum:Integer;
      xComp,yComp,xPos,yPos:Real;
   BEGIN
      WITH Config DO
         BEGIN
            FOR MassNum := 1 TO NumFreeMasses DO WITH Masses[MassNum] DO
               BEGIN
                  xPos := xEquilPos;
                  xComp := Displacements.Value(MassNum);
                  IF TwoDim
                     THEN
                        BEGIN
                           yPos := yEquilPos;
                           yComp:= Displacements.Value(NumFreeMasses+MassNum);
                        END
                     ELSE
                        BEGIN
                           yPos := -0.8;
                           yComp := 0.0;
                        END;
                  IF (ABS(xComp)+ABS(yComp))<>0.0 THEN
                     BEGIN
                        SelectViewPort(1); SelectScale(1);
                        SetWriteMode(XORPut);
                        SetLineStyle(0,0,ThickWidth);
                        DrawVector(xPos,yPos,xComp,yComp);
                        SetLineStyle(0,0,NormWidth);
                        SetWriteMode(NormalPut);
                     END;
               END;
         END;
   END;

   PROCEDURE ShowMassSpringPicts(VAR Config:ConfigType);
   VAR
      SpringType:SpringTypes;
      MassType:MassTypes;
      xPos,yPos:Real;
      Column:Integer;
   BEGIN
      OpenViewPort(6); SelectScale(6);
      xPos := -3.5; yPos := 0.0;
      Column := 1;
      HideCursor;
      WITH Config DO
         BEGIN
            FOR SpringType := sA to sD DO
               BEGIN
                  PutSpring(SpringVals[SpringType],
                            xPos-0.4,yPos,xPos+0.4,yPos);
                  xPos := xPos + 1.0;
               END;
            FOR MassType := Inf TO mC DO
               BEGIN
                  PutMass(MassVals[MassType],xPos,yPos,NormalPut);
                  xPos := xPos + 1.0;
               END;
         END;
      ShowCursor;
   END;

   PROCEDURE DrawExternalForce(VAR Config:ConfigType);
   VAR
      xPos,yPos,fact:Real;
   BEGIN
      WITH Config DO WITH NumParms DO IF CurExtForce<>0 THEN
         BEGIN
            fact := 2/DFMax;
            xPos := Masses[AppliedMass].xEquilPos;
            IF NOT EquilDrawn THEN
               xPos := xPos + Displacements.Value(AppliedMass);
            IF TwoDim
               THEN
                  BEGIN
                     yPos := Masses[AppliedMass].yEquilPos;
                     IF NOT EquilDrawn THEN yPos := yPos +
                        Displacements.Value(NumFreeMasses+AppliedMass);
                  END
               ELSE yPos := 0.0;
            SetWriteMode(XORput); SetLineStyle(0,0,ThickWidth);
            SelectViewPort(1); SelectScale(1); 
            DrawVector(xPos,yPos,fact*xComp*CurExtForce,
                                 fact*yComp*CurExtForce);
            SetWriteMode(NORMALput); SetLineStyle(0,0,NormWidth);
         END;
   END;

   PROCEDURE DrawEquilConfig(VAR Config:ConfigType; HowPut:Byte);
   BEGIN
      HideCursor;
      Config.EquilDrawn := True;
      IF Config.xVERSUSt THEN
         BEGIN
            SelectMUPPETport;
            Print(75,16,'t'); Print(40,16,'Displacements');
            OpenViewPort(3); SelectScale(3);
            PlotLine(0,0,MaxData+10,0); PlotLine(MaxData,-1,MaxData,1);
         END;
      t := 0.0;
      NumPnts := -1;
      AnimClean;
      DrawSprings(Config,Equil,True,HowPut);
      DrawMasses(Config,AllMasses,Equil,HowPut);
      IF (Config.CalcMethod=NumericalCalc) AND (HowPut=NormalPut) THEN
         BEGIN
            SetUpRK4(Config);
            DrawExternalForce(Config);
         END;
      SelectMUPPETPort;
      Rubout(40,2,16,LightBlue);
      Print(40,2,'  Equilibrium');
      ShowCursor;
   END;

{+--------------------------------------------------------------------------}
{| Normal Mode Occupation Procedures
{+--------------------------------------------------------------------------}
   PROCEDURE GetNumBarChars(VAR Config:ConfigType;
                            VAR NumBarChars:NumBarCharType);
  {+------------------------------------------------------------------}
  {| This procedure determines the number of characters to use for
  {| generating the histogram of the occupation of each mode.
  {+------------------------------------------------------------------}
   VAR
      i:Integer;
      Energy,dEnergy:Real;
   BEGIN
      dEnergy := Config.MaxModeEnergy/NumBarDiv;
      WITH Config DO FOR i:= 1 to NumModes DO
         BEGIN
            Energy := SQR(Amplitude.Value(i)*eVals.Value(i))/2;
            NumBarChars[i] :=Round(Energy/dEnergy);
            IF NumBarChars[i]>NumBarDiv THEN NumBarChars[i] := NumBarDiv;
         END;
   END;

   PROCEDURE UpdateNormalModesGraph(VAR Config:ConfigType);
   VAR
      NumBarChars:NumBarCharType;
      i,j,yPos:Word;
      dxP,dyP:Integer;
   BEGIN
      GetNumBarChars(Config,NumBarChars);
      HideCursor;
      SelectViewport(2); ClearViewPort;
      SelectViewport(7);
      WITH Config DO WITH ModePutInfo DO
      FOR i:= 1 to NumModes DO
         BEGIN
            yPos := yInit+i*h;
            FOR j := 1 to NumBarChars[i] DO
               OutTextXY(xInitBar+j*w,yPos,BarChr);
            SetColor(Black);
            FOR j := NumBarChars[i]+1 to NumBarDiv DO
               OutTextXY(xInitBar+j*w,yPos,BarChr);
            SetColor(White);
            IF Evals.Value(i)>0 THEN
               BEGIN
                  yPos := yPos + 3;
                  Circle(xInitPhase,yPos,PhaseRadiusX);
                  dxP :=  Round(PhaseRadiusX*cos(Phase.Value(i)));
                  dyP := -Round(PhaseRadiusY*sin(Phase.Value(i)));
                  Line(xInitPhase,yPos,xInitPhase+dxP,yPos+dyP);
               END;
         END;
      ShowCursor;
   END;

   PROCEDURE ChangeCurMode(VAR Config:ConfigType;
                           OldMode,CurMode:Integer);
   BEGIN
      SelectViewPort(7);
      WITH ModePutInfo DO
         BEGIN
            HideCursor;
            SetColor(Black);
            OutTextXY(xInitStar,yInit+OldMode*h,BarChr);
            SetColor(White);
            OutTextXY(xInitStar,yInit+CurMode*h,'*');
            ShowCursor;
         END;
   END;

   PROCEDURE UpdateMode(VAR Config:ConfigType;NumBarChrs,CurMode:Integer);
   VAR
      Fact,Amp,Energy,dEnergy:Real;
      j,yPos:Integer;
   BEGIN
      WITH Config DO
         BEGIN
            IF ABS(eVals.Value(CurMode))>0.0
               THEN Fact := SQRT(2.0)/eVals.Value(CurMode)
               ELSE Fact := 1.0;
            dEnergy := MaxModeEnergy/NumBarDiv;
            Energy := NumBarChrs*dEnergy;
            Amp := Fact*SQRT(Energy);
            DrawDisplacementVectors(Config);
            Amplitude.Put(CurMode,Amp);
            GenDispVel(Config,0.0);
            DrawDisplacementVectors(Config);
            SelectViewPort(7);
            HideCursor;
            WITH ModePutInfo DO
               BEGIN
                  yPos := yInit+CurMode*h;
                  FOR j := 1 to NumBarChrs DO
                     OutTextXY(xInitBar+j*w,yPos,BarChr);
                  SetColor(Black);
                  FOR j := NumBarChrs+1 to NumBarDiv DO
                     OutTextXY(xInitBar+j*w,yPos,BarChr);
                  SetColor(White);
               END;
            ShowCursor;
         END;
   END;

   PROCEDURE OpenNormalModesGraph(VAR Config:ConfigType);
  {+-------------------------------------------------------------------}
  {| Initialize graph for display of normal mode occuption.
  {+-------------------------------------------------------------------}
   VAR
      i,yPos,xLine,xAsp,yAsp:Word;
   BEGIN
      OpenViewPort(7); HideCursor;
      WITH ModePutInfo DO
         BEGIN
            GetAspectRatio(xAsp,yAsp);
            AspectFactor := yAsp/xAsp;
            WITH Views[7] DO BEGIN dy := Vy2-Vy1; dx := Vx2-Vx1; END;
            h := 3 * TextHeight('O') DIV 2;
            w := TextWidth('0');
            yInit := 8+h;
            xInitBar := Round(dx*0.37) - w;
            xInitStar := xInitBar-1;
            xInitPhase := Round(dx*0.87);
            PhaseRadiusY := Round(h/2)-1;
            PhaseRadiusX := Round(AspectFactor*PhaseRadiusY);
            xLine := Round(0.78*dx);
            OutTextXY(5,5,'Normal Modes');
            OutTextXY(5,5+h,'Freq Hz');
            OutTextXY(xInitStar+2,5+h,'Energy (J)');
            OutTextXY(xLine+5,5+h,'Phase');
            Line(0,2+h,dx,2+h);
            Line(0,2+2*h,dx,2+2*h);
            Line(xInitStar-1,2+h,xInitStar-1,dy);
            Line(xInitStar+w,2+2*h,xInitStar+w,dy);
            Line(xLine,2+h,xLine,dy);
            WITH Config DO FOR i:= 1 to NumModes DO
               OutTextXY(1,yInit+i*h,NumStr(EVals.Value(i)/(2*Pi),7,2));
            With Views[7] DO   {Viewport 2 is used to erase phase stuff}
               BEGIN
                  Views[2].Vx1 := Vx1+xLine+1;
                  Views[2].Vx2 := Vx2-1;
                  Views[2].Vy1 := Vy1 + yInit+h-PhaseRadiusY;
                  Views[2].Vy2 := vy2-1;
               END;
         END;
      UpdateNormalModesGraph(Config);
      ShowCursor;
   END;

{+--------------------------------------------------------------------------}
{| Procedures to modify initial displacements
{+--------------------------------------------------------------------------}
   PROCEDURE ChangeDisplacement(VAR Config:ConfigType;
                               CurMass:Word; KeyHit:Char);                                    {RIGHT}
   VAR
      CurMassY:Word;
   BEGIN
      IF KeyHit IN ['M','K','H','P'] THEN WITH Config DO
         BEGIN
            DrawDisplacementVectors(Config);
            IF TwoDim THEN CurMassY := CurMass + NumFreeMasses;
            CASE KeyHit OF
               'M': Displacements.Put(CurMass,Displacements.Value(CurMass) + 0.1);
               'K': Displacements.Put(CurMass,Displacements.Value(CurMass) - 0.1);
               'H': IF TwoDim THEN
                       Displacements.Put(CurMassY,Displacements.Value(CurMassY) + 0.1);
               'P': IF TwoDim THEN
                       Displacements.Put(CurMassY,Displacements.Value(CurMassY) - 0.1);
            END; {CASE}
            GetModes(Config,0);
{            GenDispVel(Config,0);}
            UpDateNormalModesGraph(Config);
            DrawDisplacementVectors(Config);
         END;
   END;

   PROCEDURE GetMouseClickMass(VAR Config:ConfigType;
                               VAR SelMassNum:Integer);
   CONST
      MaxRadius = 8;
   VAR
      i,xClick,yClick,xScr,yScr:Integer;
      xPos,yPos:Real;
   BEGIN
      i := 1;
      SelMassNum := 0;
      SelectViewport(1);  SelectScale(1);
      xClick := Event.x - Views[1].Vx1;
      yClick := Event.y - Views[1].Vy1;
      REPEAT
         GetMassPos(Config,Equil,i,xPos,yPos);
         Map(xPos,yPos,xScr,yScr);
         IF (xClick<xScr+MaxRadius) AND (xClick>xScr-MaxRadius) AND
            (yClick<yScr+MaxRadius) AND (yClick>yScr-MaxRadius)
            THEN SelMassNum := i;
         i := i + 1;
      UNTIL (SelMassNum<>0) OR (i>Config.NumMasses);
   END;

   PROCEDURE HandleMouseDisplace(VAR Config:ConfigType;
                                 VAR CurMass:Word);
   VAR
      xMouse,yMouse,MouseButton,MassNum:Integer;
      xPos,yPos,xDisp,yDisp:Real;
      Inside:Boolean;
   BEGIN
      GetMouseClickMass(Config,MassNum);
      IF (MassNum=0) OR (MassNum>Config.NumFreeMasses) THEN EXIT;
     {+----------------------------------------------------------------}
     {| Change selected mass if mouse clicked at new spot.
     {+----------------------------------------------------------------}
      IF MassNum<>CurMass THEN
         BEGIN
            HiLiteSelectedMass(Config,CurMass);
            CurMass := MassNum;
            HiLiteSelectedMass(Config,CurMass);
         END;
     {+----------------------------------------------------------------}
     {| Allow change to new position if button is still pushed.
     {+----------------------------------------------------------------}
      Delay(200);
      MouseGlobalPosn(xMouse,yMouse,MouseButton);
      IF MouseButton=0 THEN EXIT;
      REPEAT
         MousePosn(xPos,yPos,1,1,Inside);
         IF Inside THEN WITH Config DO
            BEGIN
               DrawDisplacementVectors(Config);
               xDisp := xPos - Masses[CurMass].xEquilPos;
               Displacements.Put(CurMass,xDisp);
               IF Config.TwoDim THEN
                  BEGIN
                     yDisp := yPos - Masses[CurMass].yEquilPos;
                     Displacements.Put(CurMass+NumFreeMasses,yDisp);
                  END;
               GetModes(Config,0);
{               GenDispVel(Config,0);}
               DrawDisplacementVectors(Config);
               UpDateNormalModesGraph(Config);
               Delay(100);
            END;
         MouseGlobalPosn(xMouse,yMouse,MouseButton);
      UNTIL MouseButton=0;
   END;

{+--------------------------------------------------------------------------}
{| Procedures to modify configuration.
{+--------------------------------------------------------------------------}
   PROCEDURE WriteMassText(VAR Config:ConfigType; MassNum:Integer);
   VAR
      xPos,yPos:Real;
      MassType:MassTypes;
   BEGIN
      GetMassPos(Config,Equil,MassNum,xPos,yPos);
      GraphBackColor := Red; OpenViewPort(5); GraphBackColor := Black;
      MassType := Config.Masses[MassNum].Class ;
      Print(1,1,'MassType: '+MassCode[MassType]+'     ');
      IF MassType<>Inf
          THEN Print(1,2,NumStr(Config.MassVals[MassType].Mass,10,4)+
                                   ' kg  ')
          ELSE Print(1,2,'                  ');
      Print(1,3,'x = '+NumStr(xPos,7,2)+' m    ');
      IF Config.TwoDim THEN Print(1,4,'y = '+NumStr(yPos,7,2)+' m    ');
      SelectViewPort(1);
   END;

   PROCEDURE AddMass(VAR Config:ConfigType; VAR MassNum:Integer);              {Ins}
   VAR
      x1,y1:Real;
   BEGIN
      IF Config.NumMasses=MaxMass THEN
         BEGIN
            Announce('Only '+NumStr(MaxMass,3,0)+'objects allowed -'+
                    'Hit any key to continue');
            Message('Modify equilibrium configuration - OBJECTS      F1-Help');
            SelectViewPort(1);
         END
      ELSE WITH Config DO
         BEGIN
            NumMasses := NumMasses + 1;
            GetMassPos(Config,Equil,MassNum,x1,y1);
            HiLiteSelectedMass(Config,MassNum);
            Masses[NumMasses] := Masses[MassNum];
            MassNum := NumMasses;
            Masses[MassNum].xEquilPos := x1+0.3;
            DrawMass(Config,MassNum,Equil,XORPut);
            HiLiteSelectedMass(Config,MassNum);
            WriteMassText(Config,MassNum);
         END;
      END;

   PROCEDURE DeleteMass(VAR Config:ConfigType;
                        VAR MassNum:Integer);
   VAR
      i:Integer;
      SpringNum,DeletedSprings:Integer;
      Mass1,Mass2:Integer;
   BEGIN
      IF Config.NumMasses=2 THEN
         BEGIN
            Announce('1 mass not allowed - hit any key to continue');
            Message('Modify equilibrium configuration - OBJECTS      F1-Help');
            SelectViewPort(1);
         END
      ELSE WITH Config DO
         BEGIN
           {+---------------------------------------------------------}
           {| Delete springs attached to this mass from record
           {+---------------------------------------------------------}
            DeletedSprings := 0;
            FOR SpringNum := 1 to NumSprings DO
               BEGIN
                  Mass1 := Springs[SpringNum].End1;
                  Mass2 := Springs[SpringNum].End2;
                  IF (Mass1=MassNum) OR (Mass2=MassNum) THEN
                     BEGIN
                        DrawSpring(Config,SpringNum,Equil,True,XORPut);
                        DeletedSprings := DeletedSprings + 1;
                     END
                  ELSE
                     BEGIN
                        IF Mass1>MassNum
                           THEN Springs[SpringNum].End1 := Mass1 - 1;
                        IF Mass2>MassNum
                           THEN Springs[SpringNum].End2 := Mass2 - 1;
                        IF DeletedSprings<>0
                           THEN Springs[SpringNum-DeletedSprings] :=
                                Springs[SpringNum];
                     END;
               END;
            NumSprings := NumSprings - DeletedSprings;
            HiLiteSelectedMass(Config,MassNum);
            DrawMass(Config,MassNum,Equil,XORPut);
           {+---------------------------------------------------------}
           {| Shift other mass values to fill in hole.
           {+---------------------------------------------------------}
            FOR i := MassNum +1 TO NumMasses DO Masses[i-1] := Masses[i];
            NumMasses := NumMasses - 1;
           {+---------------------------------------------------------}
           {| Hilite next mass
           {+---------------------------------------------------------}
            MassNum := 1 + (MassNum-1) MOD NumMasses;
            HiLiteSelectedMass(Config,MassNum);
            WriteMassText(Config,MassNum);
         END;
   END;

   PROCEDURE MoveMass(VAR Config:ConfigType;
                      SelMassNum:Integer; KeyHit:Char);
   VAR
      x1,y1:Real;
   BEGIN
      IF KeyHit IN ['M','K','H','P'] THEN WITH Config DO
         BEGIN
            GetMassPos(Config,Equil,SelMassNum,x1,y1);
            CASE KeyHit OF
               'M': x1 := x1 + 0.1;
               'K': x1 := x1 - 0.1;
               'H': IF TwoDim THEN y1 := y1 + 0.1;
               'P': IF TwoDim THEN y1 := y1 - 0.1;
            END; {CASE}
            Masses[SelMassNum].xEquilPos := x1;
            IF TwoDim THEN Masses[SelMassNum].yEquilPos := y1;
         END
      ELSE IF KeyHit=';' THEN
         ShowHelp('CoupOsc.HLP','Modify Masses Help');
      HideCursor;
      DrawEquilConfig(Config,XORPut);
      HiLiteSelectedMass(Config,SelMassNum);
      ShowCursor;
      WriteMassText(Config,SelMassNum);
   END;

   PROCEDURE HandleMassMouse(VAR Config:ConfigType;
                             VAR SelMassNum:Integer);
  {+------------------------------------------------------------------}
  {| This procedure allows the user to use the mouse to select and
  {| move an object.
  {+------------------------------------------------------------------}
   VAR
      xWord,yWord,Button:Integer;
      MassNum:Integer;
      xPos,yPos:Real;
      Inside:Boolean;
   BEGIN
      GetMouseClickMass(Config,MassNum);
      IF MassNum=0 THEN EXIT;
     {+----------------------------------------------------------------}
     {| Change selected mass
     {+----------------------------------------------------------------}
      IF MassNum<>SelMassNum THEN
         BEGIN
            HideCursor;
            HiLiteSelectedMass(Config,SelMassNum);
            SelMassNum := MassNum;
            HiLiteSelectedMass(Config,SelMassNum);
            WriteMassText(Config,SelMassNum);
            ShowCursor;
         END;
     {+----------------------------------------------------------------}
     {| Allow change to new position if button is still pushed.
     {+----------------------------------------------------------------}
      Delay(200);
      MouseGlobalPosn(xWord,yWord,Button);
      IF Button=0 THEN EXIT;
      REPEAT
         MouseGlobalPosn(xWord,yWord,Button);
         MousePosn(xPos,yPos,1,1,Inside);
         IF Inside THEN
            BEGIN
               Config.Masses[SelMassNum].xEquilPos := xPos;
               SelectViewport(5); RubOut(1,3,11,Red);
               Print(1,3,'x = '+NumStr(xPos,7,2)+' cm    ');
               IF Config.TwoDim THEN
                  BEGIN
                     Config.Masses[SelMassNum].yEquilPos := yPos;
                     RubOut(1,4,11,Red);
                     Print(1,4,'y = '+NumStr(yPos,7,2)+' cm    ');
                  END;
               SelectViewport(1);
               Delay(100);
            END;
      UNTIL Button=0;
      HideCursor;
      DrawEquilConfig(Config,XORPut);
      HiLiteSelectedMass(Config,SelMassNum);
      ShowCursor;
   END;

   PROCEDURE ModifyMasses(VAR Config:ConfigType);
  {+------------------------------------------------------------------}
  {| This procedure allows the user to move through the masses used in
  {| the system and change the positions and masstype of each object.
  {+------------------------------------------------------------------}
   VAR
      DoneFlag:Boolean;
      xPos,yPos:Real;
      SelMassNum: Integer;
      KeyHit:Byte;
      SaveScreen:TImage;
   BEGIN
      SelMassNum := 1;
      Config.EVecDone := False;
      HideCursor;
      SelectMuppetPort;
      WITH Views[5] DO SaveScreen.Get(Vx1,Vy1,Vx2,Vy2);
      DrawEquilConfig(Config,XORPut);
      ModMassesKeys.Display;
      ShowArrowKeys(Config);
      Message('Modify equilibrium configuration - OBJECTS      F1-Help');
      SelectViewPort(1);
      HiLiteSelectedMass(Config,SelMassNum);
      WriteMassText(Config,SelMassNum);
      DoneFlag:=False;
      ShowCursor;
      WITH Config DO
         REPEAT
            CheckForEvents;
            IF Event.ExtendedKey THEN
               MoveMass(Config,SelMassNum,Event.ReadKey);                                    {RIGHT}
            IF ModMassesKeys.Pressed(KeyHit) THEN
               BEGIN
                  HideCursor;
                  CASE KeyHit OF
                  1: DoneFlag:= True;   {Return}
                  2: BEGIN              {Tab}
                        HiLiteSelectedMass(Config,SelMassNum);
                        SelMassNum := 1 + (SelMassNum Mod NumMasses);
                        HiLiteSelectedMass(Config,SelMassNum);
                        WriteMassText(Config,SelMassNum);
                     END;
                  3: BEGIN               {PGUP}
                        DrawMass(Config,SelMassNum,Equil,XORPut);
                        CASE Masses[SelMassNum].Class OF
                           mA: Masses[SelMassNum].Class := mB;
                           mB: Masses[SelMassNum].Class := mC;
                           mC: Masses[SelMassNum].Class := Inf;
                           Inf: Masses[SelMassNum].Class := mA;
                        END; {CASE}
                        DrawMass(Config,SelMassNum,Equil,XORPut);
                        WriteMassText(Config,SelMassNum);
                     END;
                  4: DeleteMass(Config,SelMassNum);           {Del}
                  5: AddMass(Config,SelMassNum);              {Ins}
                  END; {CASE}
                  ShowCursor;
               END;
            IF Event.MouseClicked THEN HandleMassMouse(Config,SelMassNum);
         UNTIL DoneFlag;
      HiLiteSelectedMass(Config,SelMassNum);
      SelectMuppetPort;
      WITH Views[5] DO SaveScreen.Put(Vx1,Vy1);
   END;

   PROCEDURE WriteSpringText(VAR Config:ConfigType; SpringNum:Integer);
   VAR
      SpringType:SpringTypes;
   BEGIN
      GraphBackColor := Red; OpenViewPort(5); GraphBackColor := Black;
      SpringType := Config.Springs[SpringNum].Class;
      Print(1,1,'SpringType: '+SpringCode[SpringType]+'  ');
      Print(1,2,NumStr(Config.SpringVals[SpringType].SpringConst,8,3)+
            ' N/m ');
      SelectViewPort(1);
   END;

   PROCEDURE GetSpringNum(VAR Config:ConfigType; VAR SpringNum:Integer;
                          Mass1Num,Mass2Num:Integer);
  {+-------------------------------------------------------------------}
  {| This procedure determines which spring has been selected.
  {+-------------------------------------------------------------------}
   VAR
      i,Mass1,Mass2:Integer;
   BEGIN
      SpringNum:=0;
      WITH Config DO
         BEGIN
            FOR i := 1 to NumSprings DO
               BEGIN
                  Mass1 := Springs[i].End1;
                  Mass2 := Springs[i].End2;
                  IF (Mass1Num IN [Mass1,Mass2]) AND
                     (Mass2Num IN [Mass1,Mass2]) THEN SpringNum := i;
               END;
         END;
   END;

   PROCEDURE AddSpring(VAR Config:ConfigType; VAR SpringNum:Integer);
   VAR
      Mass1,Mass2,SelMassNum,OldSpring:Integer;
      KeyHit:Byte;
   BEGIN
      WITH Config DO
         BEGIN
           {+----------------------------------------------------------}
           {| Take care of special cases
           {+----------------------------------------------------------}
            IF (NumMasses=2) AND (NumSprings=0) THEN
               BEGIN
                  NumSprings := 1;
                  SpringNum := 1;
                  Springs[SpringNum].End1 := 1;
                  Springs[SpringNum].End2 := 2;
                  Springs[SpringNum].Class := sA;
                  EXIT;
               END;
            IF NumSprings=MaxSprings THEN
               BEGIN
                  Announce('No more springs are allowed - Hit any Key to continue');
                  EXIT;
               END;
           {+----------------------------------------------------------}
           {| Normal ADDSPRING case follows
           {+----------------------------------------------------------}
            IF SpringNum<>0 THEN HiLiteSelectedSpring(Config,SpringNum);
            MESSAGE('Select first object to connect with spring.');
            Mass2 := 0;
            Mass1 := 1;
            HiLiteSelectedMass(Config,Mass1);
            MassSelectKeys.Display;
            ShowCursor;
            REPEAT
               CheckForEvents;
               IF MassSelectKeys.Pressed(KeyHit) THEN
                  CASE KeyHit OF
                     2: BEGIN
                           HideCursor;
                           HiliteSelectedMass(Config,Mass1);
                           Mass1 := 1 + Mass1 Mod NumMasses;
                           HiliteSelectedMass(Config,Mass1);
                           ShowCursor;
                        END;
                     4:ShowHelp('CoupOsc.HLP','Select first mass for spring help');
                  END; {CASE}
               IF Event.MouseClicked THEN
                  BEGIN
                     GetMouseClickMass(Config,SelMassNum);
                     IF (SelMassNum<>0) AND (SelMassNum<=NumMasses) THEN
                        BEGIN
                           HideCursor;
                           HiLiteSelectedMass(Config,Mass1);
                           Mass1 := SelMassNum;
                           HiLiteSelectedMass(Config,Mass1);
                           ShowCursor;
                           KeyHit := 1;
                        END;
                  END;
            UNTIL KeyHit IN [1,3];
            IF KeyHit=1 THEN
               BEGIN
                  MESSAGE('Select second object');
                  Mass2 := Mass1;
                  MassSelectKeys.Display;
                  REPEAT
                     CheckForEvents;
                     IF MassSelectKeys.Pressed(KeyHit) THEN
                        CASE KeyHit OF
                           2: BEGIN
                                 HideCursor;
                                 IF Mass2<>Mass1 THEN
                                    HiliteSelectedMass(Config,Mass2);
                                 REPEAT
                                    Mass2 := 1 + Mass2 Mod NumMasses;
                                 UNTIL Mass1<>Mass2;
                                 HiliteSelectedMass(Config,Mass2);
                                 ShowCursor;
                              END;
                           4: ShowHelp('CoupOsc.HLP','Select second mass for spring help');
                        END; {CASE}
                     IF Event.MouseClicked THEN
                        BEGIN
                           GetMouseClickMass(Config,SelMassNum);
                           IF (SelMassNum<>0) AND (SelMassNum<=NumMasses)
                              AND (SelMassNum<>Mass1) THEN
                              BEGIN
                                 HideCursor;
                                 IF Mass2<>Mass1 THEN
                                    HiLiteSelectedMass(Config,Mass2);
                                 Mass2 := SelMassNum;
                                 HiLiteSelectedMass(Config,Mass2);
                                 KeyHit := 1;
                                 ShowCursor;
                              END;
                        END;
                  UNTIL KeyHit IN [1,3];
               END;
            HideCursor;
            IF (KeyHit=1) AND (Mass1<>Mass2) THEN
               BEGIN
                  GetSpringNum(Config,OldSpring,Mass1,Mass2);
                  IF OldSpring=0 THEN
                     BEGIN
                        NumSprings := NumSprings + 1;
                        IF SpringNum<>0
                           THEN Springs[NumSprings].Class :=
                                Springs[SpringNum].Class
                           ELSE Springs[NumSprings].Class := sA;
                        SpringNum := NumSprings;
                        Springs[SpringNum].End1 := Mass1;
                        Springs[SpringNum].End2 := Mass2;
                        WriteSpringText(Config,SpringNum);
                        DrawSpring(Config,SpringNum,Equil,True,XORPut);
                     END;
               END;
            HiLiteSelectedMass(Config,Mass1);
            IF SpringNum<>0 THEN
               HiLiteSelectedSpring(Config,SpringNum);
            IF (Mass2<>0) AND (Mass2<>Mass1) THEN
               HiLiteSelectedMass(Config,Mass2);
         END;
      ModSpringsKeys.Display;
      Message('Modify equilibrium configuration - SPRINGS');
   END;

   PROCEDURE DeleteSpring(VAR Config:ConfigType;
                          VAR SpringNum:Integer);
   VAR
      i:Integer;
   BEGIN
      IF Config.NumSprings=1 THEN
         Announce('Must have at least 1 spring - hit any key to continue')
      ELSE WITH Config DO
         BEGIN
            SelectViewport(1); SelectScale(1);
            DrawSpring(Config,SpringNum,Equil,False,XORPut);
            DrawSpring(Config,SpringNum,Equil,True,XORPut);
            NumSprings := NumSprings - 1;
            FOR i := SpringNum to NumSprings DO
               Springs[i] := Springs[i+1];
            SpringNum := 1 + (SpringNum-1) MOD NumSprings;
            WriteSpringText(Config,SpringNum);
            HiliteSelectedSpring(Config,SpringNum);
         END;
   END;

   PROCEDURE HandleSpringMouse(VAR Config:ConfigType;
                               VAR SelSpringNum:Integer);
   VAR
      i,SpringNum,xClick,yClick:Integer;
      x1Scr,y1Scr,x2Scr,y2Scr,xPoint,yPoint:Integer;
      x1,y1,x2,y2:Real;
      TiltFlag:Boolean;
   BEGIN
      SpringNum := 0;
      i := 1;
      xClick := Event.x - Views[1].Vx1;
      yClick := Event.y - Views[1].Vy1;
      SelectViewport(1);  SelectScale(1);
      REPEAT
         GetSpringPos(Config,Equil,i,x1,y1,x2,y2);
         TiltFlag := ABS(x2-x1)>ABS(y2-y1);
         IF (TiltFlag AND (x1<=x2)) OR ((NOT TiltFlag) AND (y1>=y2))
            THEN BEGIN Map(x1,y1,x1Scr,y1Scr); Map(x2,y2,x2Scr,y2Scr); END
            ELSE BEGIN Map(x1,y1,x2Scr,y2Scr); Map(x2,y2,x1Scr,y1Scr); END;
         IF TiltFlag AND (xClick<x2Scr) AND (xClick>x1Scr) THEN
            BEGIN
               yPoint := y1Scr + Round((y2Scr-y1Scr)*(xClick-x1Scr)/(x2Scr-x1Scr));
               IF ABS(yPoint-yClick) <= 8 THEN SpringNum := i;
            END;
         IF (NOT TiltFlag) AND (yClick<y2Scr) AND (yClick>y1Scr) THEN
            BEGIN
               xPoint := x1Scr + Round((x2Scr-x1Scr)*(yClick-y1Scr)/(y2Scr-y1Scr));
               IF ABS(xPoint-xClick) <= 8 THEN SpringNum := i;
            END;
         i := i + 1;
      UNTIL (SpringNum<>0) OR (i>Config.NumSprings);
     {+----------------------------------------------------------------}
     {| Change selected spring
     {+----------------------------------------------------------------}
      IF (SpringNum<>0) AND (SpringNum<>SelSpringNum) THEN
         BEGIN
            HideCursor;
            HiLiteSelectedSpring(Config,SelSpringNum);
            SelSpringNum := SpringNum;
            HiLiteSelectedSpring(Config,SelSpringNum);
            WriteSpringText(Config,SelSpringNum);
            ShowCursor;
         END;
   END;

   PROCEDURE ModifySprings(VAR Config:ConfigType);
  {+------------------------------------------------------------------}
  {| This procedure allows the user to change spring types and to
  {| add and delete springs.
  {+------------------------------------------------------------------}
   VAR
      DoneFlag:Boolean;
      SpringNum,Mass1Num,Mass2Num:Integer;
      SaveScreen:TImage;
      KeyHit:Byte;
   BEGIN
      SpringNum := Config.NumSprings;
      IF SpringNum=0
         THEN REPEAT AddSpring(Config,SpringNum) UNTIL SpringNum=1;
      Config.EVecDone := False;
      HideCursor;
      SelectMuppetPort;
      WITH Views[5] DO SaveScreen.Get(Vx1,Vy1,Vx2,Vy2);
      DrawEquilConfig(Config,XORPut);
      ModSpringsKeys.Display;
      Message('Modify equilibrium configuration - SPRINGS');
      SelectViewPort(1);
      WriteSpringText(Config,SpringNum);
      HiliteSelectedSpring(Config,SpringNum);
      DoneFlag := False;
      ShowCursor;
      WITH Config DO
         REPEAT
            CheckForEvents;
            IF ModSpringsKeys.Pressed(KeyHit) THEN
               BEGIN
                  HideCursor;
                  CASE KeyHit OF
                  1: DoneFlag:= True;   {Return}
                  2: BEGIN              {Tab}
                        HiLiteSelectedSpring(Config,SpringNum);
                        SpringNum := 1 + (SpringNum Mod NumSprings);
                        HiLiteSelectedSpring(Config,SpringNum);
                        WriteSpringText(Config,SpringNum);
                     END;
                  3: BEGIN               {PGUP}
                        SelectViewPort(1);
                        DrawSpring(Config,SpringNum,Equil,True,XORPut);
                        CASE Springs[SpringNum].Class OF
                           sA: Springs[SpringNum].Class := sB;
                           sB: Springs[SpringNum].Class := sC;
                           sC: Springs[SpringNum].Class := sD;
                           sD: Springs[SpringNum].Class := sA;
                        END;
                        DrawSpring(Config,SpringNum,Equil,True,XORPut);
                        WriteSpringText(Config,SpringNum);
                     END;
                  4: DeleteSpring(Config,SpringNum);           {Del}
                  5: AddSpring(Config,SpringNum);              {Ins}
                  6: ShowHelp('CoupOsc.HLP','Modify Springs Help');
                  END; {CASE}
                  ShowCursor;
                  ClearMKBuffers;
               END;
            IF Event.MouseClicked THEN HandleSpringMouse(Config,SpringNum);
         UNTIL DoneFlag;
      HiLiteSelectedSpring(Config,SpringNum);
      SelectMuppetPort;
      WITH Views[5] DO SaveScreen.Put(Vx1,Vy1);
   END;

   PROCEDURE ModifyComponents(VAR Config:ConfigType);
  {+-------------------------------------------------------------------}
  {| This procedure allows the user to modify the mass and spring
  {| numerical values.
  {      InpBackColor := Black;}
  {+-------------------------------------------------------------------}
   VAR
      x,y : integer;
      image : TImage;
      Spring:SpringTypes;
      Mass:MassTypes;
   BEGIN
      WITH NewGenScreen DO WITH Config DO
         BEGIN
            init;
            DefineInputPort(0.0,0.99,0.01,0.2);
            LoadLine('  A (N/m)  B (N/m)  C (N/m)  D (N/m)           A (kg)   B (kg)   C (kg)');
            LoadLine(' {       }{       }{       }{       }Infinite {       }{       }{       }');
            LoadLine('             [  OK  ]      [Cancel]      [ Help ]');
            SetHelpFile('CoupOsc.HLP','Modify Numerical Values');
            FOR Spring := sA to sD DO
               SetNumber(1+Ord(Spring),SpringVals[Spring].SpringConst);
            FOR Mass := mA TO mC DO
               SetNumber(4+Ord(Mass),MassVals[Mass].Mass);
            HideCursor;
            WITH Views[6] DO BEGIN x:=Vx2-Vx1; y:=Vy2-Vy1 END;
            SelectViewport(6);
            Image.Get(0,0,x,y);
            ShowMassSpringPicts(Config);
            ShowCursor;
            AcceptScreen;
            SelectViewPort(6); Image.Put(0,0);
            IF NOT Canceled THEN
               BEGIN
                  Config.EVecDone := False;
                  FOR Spring := sA TO sD DO
                     Config.SpringVals[Spring].SpringConst := GetNumber(1+Ord(Spring));
                  FOR Mass := mA to mC DO
                     Config.MassVals[Mass].Mass := GetNumber(4+Ord(Mass));
               END;
            done;
         END;
   END;

   PROCEDURE FineTuneMassPositions(VAR Config:ConfigType);
   VAR
      Height:Real;
      MassNum:Integer;
   BEGIN;
      WITH NewGenScreen DO WITH Config DO
         BEGIN
            init;
            Height := (2.0+NumMasses)/12;
            DefineInputPort(0.0,0.5,0.0,height);
            LoadLine('Enter Equilibrium Mass Positions');
            IF TwoDim
               THEN LoadLine('     xPos         yPos')
               ELSE LoadLine('     xPos');
            IF TwoDim
               THEN FOR MassNum:= 1 to NumMasses DO 
                  BEGIN
                     LoadLine('   {       } m    {       } m');
                     SetNumber(2*MassNum-1,Masses[MassNum].xEquilPos);
                     SetNumber(2*MassNum,Masses[MassNum].yEquilPos);
                  END
               ELSE FOR MassNum:= 1 to NumMasses DO 
                  BEGIN
                     LoadLine('   {       } m');
                     SetNumber(MassNum,Masses[MassNum].xEquilPos)
                  END;
            LoadLine('  [  OK  ]  [Cancel]  [ Help ]');
            SetHelpFile('CoupOsc.HLP','Fine Tune Mass Positions');
            AcceptScreen;
            IF NOT canceled THEN IF TwoDim
               THEN FOR MassNum := 1 to NumMasses DO
                  BEGIN
                     Masses[MassNum].xEquilPos := GetNumber(2*MassNum-1);
                     Masses[MassNum].yEquilPos := GetNumber(2*MassNum);
                  END
               ELSE FOR MassNum := 1 to NumMasses DO
                  Masses[MassNum].xEquilPos := GetNumber(MassNum);
            done;
            EVecDone := False;
            DrawEquilConfig(Config,XORPut);
         END;
   END;

{+--------------------------------------------------------------------------}
{| Procedures to read in new configuration
{+--------------------------------------------------------------------------}
   PROCEDURE Load1D(VAR Config:ConfigType);
   VAR
      i:Integer;
   BEGIN
      WITH Config DO
         BEGIN
            FileName:='NEWCONF.CFG';
            Description := 'Generic one dimensional configuration';
            TwoDim := False;
            NumMasses := 4;
            NumSprings := 3;
            NumModes := 4;
            WITH MassVals[Inf] DO BEGIN
               Mass :=  0; Radius := 3; Color := 15; END;
            WITH MassVals[mA] DO BEGIN
               Mass := 10; Radius := 5; Color := 14; END;
            WITH MassVals[mB] DO BEGIN
               Mass := 20; Radius := 6; Color := 13; END;
            WITH MassVals[mC] DO BEGIN
               Mass := 30; Radius := 7; Color := 12; END;
            WITH SpringVals[sA] DO BEGIN
               SpringConst := 20; Width := 0.2; nDiv := 8; Color := 11; END;
            WITH SpringVals[sB] DO BEGIN
               SpringConst := 40; Width := 0.3; nDiv := 8; Color := 10; END;
            WITH SpringVals[sC] DO BEGIN
               SpringConst := 60; Width := 0.4; nDiv := 8; Color :=  9; END;
            WITH SpringVals[sD] DO BEGIN
               SpringConst :=  3; Width := 0.5; nDiv := 8; Color :=  7; END;
            WITH MASSES[1] DO BEGIN
               xEquilPos := -1.0; yEquilPos := 0; Class := mA; END;
            WITH MASSES[2] DO BEGIN
               xEquilPos :=  1.0; yEquilPos := 0; Class := mA; END;
            WITH MASSES[3] DO BEGIN
               xEquilPos :=  3.0; yEquilPos := 0; Class := mA; END;
            WITH MASSES[4] DO BEGIN
               xEquilPos := -3.0; yEquilPos := 0; Class := mA; END;
            WITH SPRINGS[1] DO BEGIN
               End1 := 4; End2 := 1; Class := sA; END;
            WITH SPRINGS[2] DO BEGIN
               End1 := 1; End2 := 2; Class := sD; END;
            WITH SPRINGS[3] DO BEGIN
               End1 := 2; End2 := 3; Class := sA; END;
            FOR i := 1 to NumModes DO
               BEGIN
                  DiskAmplitude[i] := 0;
                  DiskPhase[i] := 0;
               END;
            DiskAmplitude[1] := 1.666; DiskAmplitude[2] := 1.732;
            WITH NumParms DO
               BEGIN
                  Damping := 0.1;
                  DriveForce := 1.0; CurExtForce := 1.0; DFMax := 5.0;
                  ExtFreq := 0.08; EFMax := 0.5;
                  AppliedMass := 1; DFAngle := 0.0;
                  xComp := 1; yComp := 0.0;
               END;
            MaxModeEnergy := 15;
            xVERSUSt := True;
         END;
   END;

   PROCEDURE Load2D(VAR Config:ConfigType);
   VAR
      i:Integer;
   BEGIN
      WITH Config DO
         BEGIN
            FileName:='NEWCONF.CFG';
            Description := 'Generic two dimensional configuration';
            TwoDim := True;
            NumMasses := 3;
            NumSprings := 3;
            NumModes := 6;
            WITH MassVals[Inf] DO BEGIN
               Mass :=  0; Radius := 3; Color := 15; END;
            WITH MassVals[mA] DO BEGIN
               Mass := 10; Radius := 5; Color := 14; END;
            WITH MassVals[mB] DO BEGIN
               Mass := 20; Radius := 6; Color := 13; END;
            WITH MassVals[mC] DO BEGIN
               Mass := 30; Radius := 7; Color := 12; END;
            WITH SpringVals[sA] DO BEGIN
               SpringConst := 20; Width := 0.2; nDiv := 4; Color := 11; END;
            WITH SpringVals[sB] DO BEGIN
               SpringConst := 40; Width := 0.3; nDiv := 4; Color := 10; END;
            WITH SpringVals[sC] DO BEGIN
               SpringConst := 60; Width := 0.4; nDiv := 4; Color :=  9; END;
            WITH SpringVals[sD] DO BEGIN
               SpringConst := 80; Width := 0.5; nDiv := 4; Color :=  7; END;
            WITH MASSES[1] DO BEGIN
               xEquilPos := -2; yEquilPos := -2; Class := mB; END;
            WITH MASSES[2] DO BEGIN
               xEquilPos :=  2; yEquilPos := -2; Class := mB; END;
            WITH MASSES[3] DO BEGIN
               xEquilPos :=  0; yEquilPos := 1.4641; Class := mB; END;
            WITH SPRINGS[1] DO BEGIN
               End1 := 1; End2 := 2; Class := sA; END;
            WITH SPRINGS[2] DO BEGIN
               End1 := 1; End2 := 3; Class := sA; END;
            WITH SPRINGS[3] DO BEGIN
               End1 := 2; End2 := 3; Class := sA; END;
            FOR i := 1 to NumModes DO
               BEGIN
                  DiskAmplitude[i] := 0;
                  DiskPhase[i] := 0;
               END;
            DiskAmplitude[1] := 3.1; DiskAmplitude[2] := 2.5;
            WITH NumParms DO
               BEGIN
                  Damping := 0.1;
                  DriveForce := 1.0; CurExtForce := 1.0; DFMax := 5.0;
                  ExtFreq := 0.16; EFMax := 0.5;
                  AppliedMass := 1; DFAngle := 45.0;
                  xComp := COS(DFAngle*Pi/180);
                  yComp := SIN(DFAngle*Pi/180);
               END;
            MaxModeEnergy := 15;
            xVERSUSt := False;
         END;
   END;

   PROCEDURE LoadConfig(VAR Config:ConfigType);
  {+-------------------------------------------------------------------}
  {| This procedure reads in the values of a particular configuration.
  {| Note that Mode Amplitudes and Phases will ultimately be stored in
  {| DVectors.  Since we do not know how many modes will be present,
  {| these are read into normal arrays here, then copied into the
  {| dynamical vectors in OpenPlot
  {+-------------------------------------------------------------------}
   CONST
      HoldMassTypes:   ARRAY[0..3] OF MassTypes =  (Inf,mA,mB,mC);
      HoldSpringTypes: ARRAY[0..3] OF SpringTypes = (sA,sB,sC,sD);
   VAR
      TypeRep:Byte;
      f:text;
      MassType:MassTypes;
      SpringType:SpringTypes;
      i:Integer;
      AmpVal,PhaseVal:Real;
   BEGIN
      Assign(f,Config.FileName);
      Reset(f);
      WITH Config DO
         BEGIN
            xVERSUSt := False;
            READLN(f);   {Do not read in filename in case it is wrong}
            READLN(f,Description);
            READLN(f,TypeRep);
               IF TypeRep=1 THEN TwoDim := True ELSE TwoDim := False;
            READLN(f,NumMasses,NumSprings,NumModes);
            READLN(f);   {MASS TYPE INFO}
               FOR MassType := Inf to mC DO WITH MassVals[MassType] DO
                  READLN(f,Mass,Radius,Color);
            READLN(f);   {SPRING TYPE INFO}
               FOR SpringType := sA to sD DO WITH SpringVals[SpringType] DO
                  READLN(f,SpringConst,Width,nDiv,Color);
            READLN(f);   {MASS INFO}
               FOR i := 1 to NumMasses DO WITH Masses[i] DO
                  BEGIN
                     READLN(f,xEquilPos,yEquilPos,TypeRep);
                     Class := HoldMassTypes[TypeRep];
                  END;
            READLN(f);   {SPRING INFO}
               FOR i := 1 to NumSprings DO WITH Springs[i] DO
                  BEGIN
                     READLN(f,End1,End2,TypeRep);
                     Class := HoldSpringTypes[TypeRep];
                  END;
            READLN(f);   {INITIAL AMPLITUDES AND PHASES}
               FOR i := 1 to NumModes DO
                  READ(f,DiskAmplitude[i]);
               READLN(f);
               FOR i := 1 to NumModes DO
                  READ(f,DiskPhase[i]);
               READLN(f);
            READLN(f);   {PROGRAM AND NUMERICAL PARAMETERS}
               WITH NumParms DO
                  BEGIN
                     READLN(f,Damping,DriveForce,CurExtForce,
                            DFMax,ExtFreq,EFMax,AppliedMass,DFAngle);
                     xComp := COS(DFAngle*Pi/180);
                     yComp := SIN(DFAngle*Pi/180);
                  END;
               READLN(f,MaxModeEnergy);
         END;
      CLOSE(f);
   END;

{+--------------------------------------------------------------------------}
{| Procedures to set up screen layout and dynamical info.
{+--------------------------------------------------------------------------}
   PROCEDURE AnalyzeConfig(VAR Config:ConfigType);
   VAR
      WorkMessage:TWaitMessage;
      ShortestPeriod:Real;
   BEGIN
      t := 0.0;
      SortMasses(Config);
      IF Config.NumFreeMasses=0 THEN
         REPEAT
            BEEP;
            Announce('NO FREE MASSES PRESENT - NO DYNAMICS - Hit any key');
            ModifyMasses(Config);
            SortMasses(Config);
         UNTIL Config.NumFreeMasses<>0;
      WorkMessage.Show;
      GenDynMat(Config);
      WorkMessage.Update;
      GetEvecs(Config);
      WorkMessage.Update;
      ShortestPeriod := 2*Pi/Config.EVals.Value(1);
      dtAnim := AnimFact*ShortestPeriod;
      dtAnimMax := MaxAnimFact*ShortestPeriod;
      dt := NumIntFact*ShortestPeriod;
      Config.NumParms.EFMax := Config.Evals.Value(1)/Pi;
      WorkMessage.Hide;
   END;

   PROCEDURE OpenPlot(VAR Config:ConfigType);
   VAR
      i:Integer;
   BEGIN
      Anim.Close;
      ClearMUPPETport; Anim.Switch; ClearMUPPETport; Anim.Switch;
      t := 0.0;
      HideCursor;
     {+----------------------------------------------------------------}
     {| Set Up and Display ViewPorts
     {|    1 - Animation of masses
     {|    3 - Window for position versus time graph
     {|    4 - Window for displaying eigenvalue/eigenvector values
     {|    5 - Window showing mass and spring values during modification
     {|    6 - Display mass and spring info
     {|    7 - Normal Mode Occupation
     {|    2 - Phase segment of Normal Modes Graph
     {|    8 - External force sliders
     {+----------------------------------------------------------------}
      DefineViewPort(6,0.0,0.99,0.01,0.35);   {display Mass and spring info}
      DefineScale(6,-4,4.5,-5.5,1.5);
      DefineViewPort(4,0.1,0.9,0.0,0.9);   {Used to display full screen}
      DefineViewPort(5,0.01,0.31,0.58,0.86);    {Show mass,spring vals}
      IF NOT Config.TwoDim
         THEN
            BEGIN  {1-D Viewports}
               DefineViewPort(1,0.35,0.99,0.6,0.85);
               DefineScale(1,-5,5,-1,1);
               DefineViewPort(3,0.40,0.99,0.15,0.55);  {x versus t}
               DefineScale(3,0,MaxData+10,-1,1);
               DefineViewPort(7,0.0,0.32,0.30,0.87);    {Normal Mode Screen}
               DefineViewPort(8,0.0,0.32,0.072,0.295);  {External force sliders}
            END
         ELSE
            BEGIN  {2-D Viewports}
               DefineViewPort(1,0.35,0.87,0.15,0.87);
               DefineScale(1,-5,5,-5,5);
               DefineViewPort(7,0.0,0.32,0.072,0.87);     {Normal Mode Screen}
               DefineViewPort(8,0.88,1.0,0.00,1.0);  {External force sliders}
            END;
      SelectMUPPETport;
      HotKeys.Display; MainMenu.Display;
      RubOut(1,2,5,LightBlue);
      IF Config.TwoDim THEN Print(2,2,'2-D') ELSE Print(2,2,'1-D');
      Rubout(10,2,24,LightBlue);
      IF Config.CalcMethod=NumericalCalc
         THEN
            WITH Config.NumParms DO BEGIN
               Print(10,2,'Calc Method: Numerical  ');
               PrintXY(TxtX,TxtY,ExtForceLabel);
               ExtForceSliders.DrawAll;
            END
         ELSE Print(10,2,'Calc Method: Normal Mode');
      OpenViewPort(1); SelectScale(1); Axis(0,0,1,1);
      DrawEquilConfig(Config,NormalPut);
{      WITH Config DO FOR i := 1 to NumModes DO
         BEGIN
           Amplitude.Put(i,DiskAmplitude[i]);
           Phase.Put(i,DiskPhase[i]);
         END;
      GenDispVel(Config,0);
}
      OpenNormalModesGraph(Config);
      ShowCursor;
      Anim.Select(1);
      Anim.FirstPage := False;
      Anim.Pause;
      ShowCursor;
   END;

   PROCEDURE SetUpNormalModeCalc(VAR Config:ConfigType);
      BEGIN
         Config.CalcMethod := NormalModeCalc;
      END; {SetUpNormalModeCalc}

   PROCEDURE SetUpNumericalCalc(VAR Config:ConfigType);
   BEGIN
      Config.CalcMethod := NumericalCalc;
      ExtForceSliders.Delete(1);
      ExtForceSliders.Delete(2);
      WITH Config.NumParms DO IF Config.TwoDim
      THEN
         BEGIN  {2-D Sliders}
            ExtForceSliders.Create(1,0,DFMax,DriveForce,0.93,0.88,0.5,2,
                                   '','','Amp (N)',True);
            ExtForceSliders.Create(2,0,EFMax,ExtFreq,0.93,0.48,0.08,2,
                                   '','','Freq (Hz)',True);
            TxtX := 0.88; TxtY := 0.92; ExtForceLabel := 'Ext Force';
         END
      ELSE
         BEGIN  {1-D Sliders}
            ExtForceSliders.Create(1,0,DFMax,DriveForce,0.0,0.18,0.30,2,
                                   '','','Amp (N)',False);
            ExtForceSliders.Create(2,0,EFMax,ExtFreq,0.0,0.1,0.30,2,
                                   '','','Freq (Hz)',False);
            TxtX := 0; TxtY := 0.275; ExtForceLabel := 'Applied External Force';
         END;
   END;

{+--------------------------------------------------------------------------}
{| Procedures called from Main Menu
{+--------------------------------------------------------------------------}
   PROCEDURE LoadStartUpConfig(VAR Config:ConfigType);
   VAR
      ConfigLoaded,EscapeFlag:Boolean;
      RadioChoice,i:Integer;
   BEGIN
      ConfigLoaded := False;
      WITH Config DO With NewGenScreen DO
         REPEAT
            Init;
            LoadLine('                CUPS - MECHANICS');
            LoadLine(' COUPLED OSCILLATORS IN ONE AND TWO DIMENSIONS');
            LoadLine('');
            LoadLine('   #A - One Dimensional Oscillation');
            LoadLine('   #A - Two Dimensional Oscillation');
            LoadLine('   #A - User Defined system');
            LoadLine('    [  Ok  ]   [Cancel]  [ Help ] [ CUPS ]');
            SetHelpFile('CoupOsc.HLP','General Help');
            DefineInputPort(0.0,0.62,0.4,0.82);
            AcceptScreen;
            EscapeFlag := Canceled;
            IF GetBoolean(7)
               THEN aboutCUPS
               ELSE IF NOT EscapeFlag THEN
                  BEGIN
                     RadioChoice := GetRadioButton('A');
                     CASE RadioChoice OF
                        1: BEGIN Load1d(Config); ConfigLoaded := True; END;
                        2: BEGIN Load2d(Config); ConfigLoaded := True; END;
                        3: BEGIN
                              Config.Filename := OpenFile('*.cfg');
                              IF Config.FileName<>'' THEN
                                 BEGIN
                                    LoadConfig(Config);
                                    ConfigLoaded := True;
                                 END;
                           END;
                     END; {CASE}
                  END;
            done;
         UNTIL (ConfigLoaded=True) OR EscapeFlag;
      IF ConfigLoaded THEN
         BEGIN
            Config.CalcMethod := NormalModeCalc;
            ModeUpDate := True;
            AnalyzeConfig(Config);
            WITH Config DO FOR i := 1 to NumModes DO
               BEGIN
                  Amplitude.Put(i,DiskAmplitude[i]);
                  Phase.Put(i,DiskPhase[i]);
               END;
            GenDispVel(Config,0);
            OpenPlot(Config);
         END;
   END;

   PROCEDURE SaveConfig(VAR Config:ConfigType);
  {+----------------------------------------------------------------+}
  {| This procedure writes the configuration info to the output file
  {+----------------------------------------------------------------+}
   VAR
      TxtStr:String;
      f:Text;
      MassType:MassTypes;
      SpringType:SpringTypes;
      i:Integer;
      ESCFlag:Boolean;
   BEGIN
      WITH Config DO With NewGenScreen DO
         BEGIN
            Init;
            DefineInputPort(0.0,0.7,0.45,0.9);
            LoadLine('Select name for Configuration File');
            LoadLine('Enter filename (.CFG is assumed)');
            LoadLine('"                                    "');
            LoadLine('');
            LoadLine('Enter line of descriptive text');
            LoadLine('"                                                  "');
            LoadLine('  [  OK  ]   [Cancel]   [ Help ]');
            TxtStr := FileName + '.';
            TxtStr := COPY(TxtStr,1,POS('.',TxtStr)-1);
            SetString(1,TxtStr);
            SetString(2,Description);
            SetHelpFile('CoupOsc.HLP','Configuration Save');
            AcceptScreen;
            ESCflag := Canceled;
            IF NOT ESCflag THEN
               BEGIN
                  FileName := GetString(1)+'.CFG';
                  Description := GetString(2);
               END;
            done;
         END;
      IF NOT ESCflag THEN WITH Config DO
         BEGIN
            Assign(f,FileName);
            Rewrite(f);
            WRITELN(f,FileName);
            WRITELN(f,Description);
            WRITELN(f,ORD(TwoDim),' Number of Dimensions - 1');
            WRITELN(f,NumMasses,' ',NumSprings,' ',NumModes,
                       ' Number of Masses, Number of Springs');
            WRITELN(f,'MASS TYPE INFO');
               FOR MassType := Inf to mC DO WITH MassVals[MassType] DO
                  WRITELN(f,Mass,' ',Radius,' ',Color,' ',
                            MassCode[MassType]);
            WRITELN(f,'SPRING TYPE INFO');
               FOR SpringType := sA TO sD DO WITH SpringVals[SpringType] DO
                  WRITELN(f,SpringConst,' ',Width,' ',nDiv,' ',Color,' ',
                            SpringCode[SpringType]);
            WRITELN(f,'MASS INFO');
               FOR i := 1 TO NumMasses DO WITH Masses[i] DO
                  WRITELN(f,xEquilPos,' ',yEquilPos,' ',ORD(Class));
            WRITELN(f,'SPRING INFO');
               FOR i := 1 TO NumSprings DO WITH Springs[i] DO
                  WRITELN(f,End1,' ',End2,' ',ORD(Class));
            WRITELN(f,'INITIAL AMPLITUDES AND PHASES');
               FOR i := 1 to NumModes DO WRITE(f,Amplitude.Value(i),' ');
               WRITELN(f);
               FOR i := 1 to NumModes DO WRITE(f,Phase.Value(i),' ');
               WRITELN(f);
            WRITELN(f,'PROGRAM AND NUMERICAL PARAMETERS');
               WITH NumParms DO
                  BEGIN
                     WRITELN(f,Damping,' ',DriveForce,' ',
                               CurExtForce,' ',DFMax);
                     WRITELN(f,ExtFreq,' ',EFMax,' ',
                               AppliedMass,' ',DFAngle);
                  END;
               WRITELN(f,MaxModeEnergy);
             CLOSE(f);
         END;
   END;

   PROCEDURE ChangeNormalModeOccup(VAR Config:ConfigType);
   VAR
      i,OldMode,CurMode,HPos,VPos:Integer;
      KeyHit:Byte;
      NumBarChars:NumBarCharType;
   BEGIN;
      WITH Config DO
      BEGIN
         IF NOT EquilDrawn THEN DrawEquilConfig(Config,NormalPut);
         GenDispVel(Config,0.0); PosVel.Fill(0.0); GetModes(Config,0.0);
         UpdateNormalModesGraph(Config);
         GetNumBarChars(Config,NumBarChars);
         Message('Change Normal Mode Energies            F1 - Help');
         ChangeNormalModeKeys.Display;
         CurMode := 1; OldMode := 1;
         ChangeCurMode(Config,OldMode,CurMode);
         DrawDisplacementVectors(Config);
         REPEAT
            IF Event.ExtendedKey THEN IF Event.ReadKey=';' THEN
               ShowHelp('CoupOsc.HLP','Change Normal Mode Occupation Help');
            IF ChangeNormalModeKeys.Pressed(KeyHit) THEN
              BEGIN
                  Debounce;
                  ClearMKBuffers;
                  CheckForEvents;
                  OldMode := CurMode;
                  CASE KeyHit OF
                     2: IF NumBarChars[CurMode]<NumBarDiv THEN
                           NumBarChars[CurMode] := NumBarChars[CurMode] + 1;
                     3: IF NumBarChars[CurMode]>0 THEN
                           NumBarChars[CurMode] := NumBarChars[CurMode] - 1;
                     4: IF CurMode>1
                           THEN CurMode := CurMode - 1
                           ELSE CurMode := NumModes;
                     5: IF CurMode<NumModes
                           THEN CurMode := CurMode + 1
                           ELSE CurMode := 1;
                     6: BEGIN
                           Phase.Put(CurMode,Pi - Phase.Value(CurMode));
                           UpdateNormalModesGraph(Config);
                        END;
                  END; {CASE}
                  CASE KeyHit OF
                     2,3,6: UpDateMode(Config,NumBarChars[CurMode],CurMode);
                     4,5: ChangeCurMode(Config,OldMode,CurMode);
                  END; {CASE}
               END;
            WITH Event DO IF mouseClicked THEN With Views[7] DO
               IF (x>=vx1) AND (x<=vx2) AND (y>=vy1) AND (y<=vy2) THEN
                  WITH ModePutInfo DO
                  BEGIN
                     HPos := Round((x-Vx1-xInitBar-w/2)/w);
                     VPos := (y-Vy1-yInit) div h;
                     IF (VPos>0) AND (VPos<=NumModes) THEN
                        BEGIN
                           OldMode := CurMode;
                           CurMode := VPos;
                           IF (HPos>NumBarDiv+1) THEN
                              BEGIN
                                 Phase.Put(CurMode,Phase.Value(CurMode)+Pi);
                                 UpdateNormalModesGraph(Config);
                                 ChangeCurMode(Config,OldMode,CurMode);
                                 UpdateMode(Config,NumBarChars[CurMode],CurMode);
                              END
                           ELSE
                              BEGIN
                                 IF (HPos>NumBarDiv) THEN HPos := NumBarDiv;
                                 IF (HPos<0) THEN HPos := 0;
                                 NumBarChars[CurMode] := HPos;
                                 ChangeCurMode(Config,OldMode,CurMode);
                                 UpdateMode(Config,NumBarChars[CurMode],CurMode);
                              END;
                        END;
                  END;
            CheckForEvents;
         UNTIL KeyHit=1;
         DrawDisplacementVectors(Config);            {Erase these vectors}
         SelectViewport(7); SetColor(Black);
         WITH ModePutInfo DO OutTextXY(xInitStar,yInit+CurMode*h,BarChr);
         SetColor(White);
         GetModes(Config,t);
         GenDispVel(Config,t);
         UpdateNormalModesGraph(Config);
         HotKeys.Display;
         MainMenu.Display;
         SetActivePage(1);
         UpdateNormalModesGraph(Config);
         SetActivePage(0);
         IF CalcMethod=NumericalCalc THEN SetUpRK4(Config);
      END;
   END;

   PROCEDURE FineTuneNormalModeOccup(VAR Config:ConfigType);
  {+-------------------------------------------------------------------}
  {| This procedure obtains the amplitudes and phase of the normal
  {| coordinates from the user.
  {+-------------------------------------------------------------------}
   VAR
      i:Integer;
      Fact:Real;
   BEGIN
      WITH Config DO With NewGenScreen DO
         BEGIN
            IF NOT EquilDrawn THEN DrawEquilConfig(Config,NormalPut);
            Init;
            DefineInputPort(0.0,0.7,0.0,0.9);
            LoadLine('  Enter normal mode energies and phases');
            LoadLine('   Mode    Freq     Energy        Phase');
            FOR i:= 1 to NumModes DO
               LoadLine(Concat(NumStr(i,4,0),'   ',
                               NumStr(EVals.Value(i)/(2*Pi),8,4),
                               ' Hz  {     } J     {     } deg'));
            LoadLine('   [  Ok  ]      [Cancel]      [Help]');
            FOR i := 1 to NumModes DO
               BEGIN
                  IF eVals.Value(i)>0.01*eVals.Value(1)
                     THEN Fact := SQRT(2.0)/eVals.Value(i)
                     ELSE Fact := 1;
                  SetNumber(2*i-1,SQR(Amplitude.Value(i)/Fact));
                  SetNumber(2*i,Phase.Value(i)/DegToRad);
                  SetNumberLimits(2*i,0,360);
               END;
            SetHelpFile('CoupOsc.HLP','Fine Tune Normal Mode Occupation Help');
            AcceptScreen;
            IF NOT Canceled THEN
               BEGIN
                  FOR i := 1 to NumModes DO
                     BEGIN
                        IF eVals.Value(i)>0.01*eVals.Value(1)
                           THEN Fact := SQRT(2.0)/eVals.Value(i)
                           ELSE Fact := 1;
                        Amplitude.Put(i,SQRT(GetNumber(2*i-1))*Fact);
                        Phase.Put(i,DegToRad*GetNumber(2*i));
                     END;
               END;
            Done;
         END;
      GetModes(Config,t);
      GenDispVel(Config,t);
      UpdateNormalModesGraph(Config);
      SetActivePage(1);
      UpdateNormalModesGraph(Config);
      SetActivePage(0);
      IF Config.CalcMethod=NumericalCalc THEN SetUpRK4(Config);
   END;

   PROCEDURE ChangeInitialDisplacements(VAR Config:ConfigType);
   VAR
      CurMass:Word;
      KeyHit:Byte;
   BEGIN;
      WITH Config DO
      BEGIN
         IF NOT EquilDrawn THEN DrawEquilConfig(Config,NormalPut);
         GenDispVel(Config,0.0); PosVel.Fill(0.0); GetModes(Config,0.0);
         UpdateNormalModesGraph(Config);
         Message('Change Initial Displacements');
         DisplacementKeys.Display;
         ShowArrowKeys(Config);
         CurMass := 1;
         DrawDisplacementVectors(Config);
         HiLiteSelectedMass(Config,CurMass);
         REPEAT
            CheckForEvents;
            IF Event.ExtendedKey THEN
               ChangeDisplacement(Config,CurMass,Event.ReadKey);                                    {RIGHT}
            IF DisplacementKeys.Pressed(KeyHit) THEN
              BEGIN
                  Debounce;
                  ClearMKBuffers;
                  CheckForEvents;
                  CASE KeyHit OF
                   2:BEGIN
                        HiLiteSelectedMass(Config,CurMass);
                        CurMass := 1 + CurMass MOD NumFreeMasses;
                        HiLiteSelectedMass(Config,CurMass);
                     END;
                   3: ShowHelp('CoupOsc.HLP','Change Initial Displacement Help');
                  END; {CASE}
               END;
            WITH Event DO IF mouseClicked THEN With Views[1] DO
               IF (x>=vx1) AND (x<=vx2) AND (y>=vy1) AND (y<=vy2) THEN
               HandleMouseDisplace(Config,CurMass);

         UNTIL KeyHit=1;
         HiLiteSelectedMass(Config,CurMass);
         DrawDisplacementVectors(Config);            {Erase these vectors}
         HotKeys.Display;
         MainMenu.Display;
         GetModes(Config,t);
         GenDispVel(Config,t);
         UpdateNormalModesGraph(Config);
         SetActivePage(1);
         UpdateNormalModesGraph(Config);
         SetActivePage(0);
         IF CalcMethod=NumericalCalc THEN SetUpRK4(Config);
      END;
   END;

   PROCEDURE FineTuneInitialDisplacements(VAR Config:ConfigType);
   VAR
      i,iy:Integer;
      x1,y1:Real;
   BEGIN
      IF NOT Config.EquilDrawn THEN DrawEquilConfig(Config,NormalPut);
      GenDispVel(Config,0.0);
      WITH Config DO With NewGenScreen DO IF TwoDim
         THEN
            BEGIN
               Init;
               DefineInputPort(0.0,0.9,0.0,1.0);
               LoadLine('Enter initial displacements and velocities  (MKS units)');
               LoadLine('Equilibrium Position   Displacements            Velocities');
               LoadLine('    x         y        dx          dy           vx           vy');
               FOR i := 1 to NumFreeMasses DO
                  BEGIN
                     GetMassPos(Config,Equil,i,x1,y1);
                     LoadLine(NumStr(x1,8,4)+NumStr(y1,10,4)+
                     '    {        }   {        }   {        }   {        }');
                     SetNumber(1+4*(i-1),Displacements.Value(i));
                     SetNumber(2+4*(i-1),Displacements.Value(i+NumFreeMasses));
                     SetNumber(3+4*(i-1),Velocities.Value(i));
                     SetNumber(4+4*(i-1),Velocities.Value(i+NumFreeMasses));
                  END;
               LoadLine('  [  OK  ]  [Cancel]  [ Help ]');
               SetHelpFile('CoupOsc.HLP','Fine Tune Initial Displacements Help');
               AcceptScreen;
               IF NOT canceled THEN FOR i := 1 to NumFreeMasses DO
                  BEGIN
                     iy := i + NumFreeMasses;
                     Displacements.Put(i,GetNumber(1+4*(i-1)));
                     Displacements.Put(iy,GetNumber(2+4*(i-1)));
                     Velocities.Put(i,GetNumber(3+4*(i-1)));
                     Velocities.Put(iy,GetNumber(4+4*(i-1)));
                  END;
               done;
            END
         ELSE
            BEGIN
               Init;
               DefineInputPort(0.0,0.7,0.0,1.0);
               LoadLine('Initial displacements and velocities (MKS units)');
               LoadLine('   Equilibrium  Displacement   Velocity');
               FOR i := 1 to NumFreeMasses DO
                  BEGIN
                     GetMassPos(Config,Equil,i,x1,y1);
                     LoadLine(Concat('    ',NumStr(x1,8,4),
                              '    {        }     {        }'));
                     SetNumber(1+2*(i-1),Displacements.Value(i));
                     SetNumber(2+2*(i-1),Velocities.Value(i));
                  END;
               LoadLine('  [  OK  ]  [Cancel]  [ Help ]');
               SetHelpFile('CoupOsc.HLP','Fine Tune Initial Displacements Help');
               AcceptScreen;
               IF NOT canceled THEN FOR i := 1 to NumFreeMasses DO
                  BEGIN
                     Displacements.Put(i,GetNumber(1+2*(i-1)));
                     Velocities.Put(i,GetNumber(2+2*(i-1)));
                  END;
               done;
            END;
      WITH Config DO FOR i := 1 to NumModes DO
         BEGIN
            PosVel.Put(i,Displacements.Value(i));
            PosVel.Put(i+NumModes,Velocities.Value(i));
         END;
      GetModes(Config,t);
      GenDispVel(Config,t);
      UpdateNormalModesGraph(Config);
      SetActivePage(1);
      UpdateNormalModesGraph(Config);
      SetActivePage(0);
      IF Config.CalcMethod=NumericalCalc THEN SetUpRK4(Config);
   END;

   PROCEDURE ShowEigenVectors(VAR Config:ConfigType);
  {+-------------------------------------------------------------------}
  {| This procedure displays the eigenvectors
  {|   Currently this is limited to systems with NumModes <= 8.
  {| Note that EVecs has been scaled to account for the masses.  This
  {| is undone for display here.
  {| IF NumModes<=6 the dynamical matrix is shown as well.
  {+-------------------------------------------------------------------}
   VAR
      i,j,MaxTerms:Integer;
      EVecComp,DynMatComp:Real;
      SaveScreen:TImage;
      RowNum:Integer;
      TxtStr:String;
   BEGIN
      SelectViewPort(4);
      SaveScreen.Get(0,0,Round(0.8*GetMaxX),Round(0.9*GetMaxY));
      Message('Hit any key to continue');
      OpenViewPort(4);
      WITH Config DO
         BEGIN
            PRINT(1,1,'EIGENVECTORS (COLUMNS)');
            IF NumModes>6 THEN MaxTerms := 6 ELSE MaxTerms := NumModes;
            RowNum := 2;
            FOR i := 1 TO MaxTerms DO
               BEGIN
                  IF TwoDim AND (i>NumFreeMasses)
                     THEN TxtStr := 'Y'+NumStr(i-NumFreeMasses,1,0)
                     ELSE TxtSTr := 'X'+NumStr(i,1,0);
                  Print(1,RowNum,TxtStr);
                  FOR j := 1 to MaxTerms DO
                     BEGIN
                        EVecComp := EVecs.Value(i,j)/MassScale.Value(i);
                        Print(5+8*(j-1),RowNum,NumStr(EVecComp,8,4));
                     END;
                  RowNum := RowNum + 1;
               END;
            PRINT(1,RowNum,'DYNAMICAL MATRIX:');
            RowNum := RowNum + 1;
            FOR i := 1 to MaxTerms DO
               BEGIN
                  IF TwoDim AND (i>NumFreeMasses)
                     THEN TxtStr := 'Y'+NumStr(i-NumFreeMasses,1,0)
                     ELSE TxtSTr := 'X'+NumStr(i,1,0);
                  Print(8+8*(i-1),RowNum,TxtStr);
               END;
            RowNum := RowNum + 1;
            FOR i := 1 TO MaxTerms DO
               BEGIN
                  IF TwoDim AND (i>NumFreeMasses)
                     THEN TxtStr := 'Y'+NumStr(i-NumFreeMasses,1,0)
                     ELSE TxtSTr := 'X'+NumStr(i,1,0);
                  Print(1,RowNum,TxtStr);
                  FOR j := 1 to MaxTerms DO
                     BEGIN
                        DynMatComp := DynMat.Value(i,j)/(MassScale.Value(i)*
                                      MassScale.Value(j));
                        Print(5+8*(j-1),RowNum,NumStr(DynMatComp,8,4));
                     END;
                  RowNum := RowNum + 1;
               END;
         END;
      StaticPause;
      SelectViewPort(4); SaveScreen.Put(0,0);
   END;

   PROCEDURE ShowModeOcc(VAR Config:ConfigType);
  {+-------------------------------------------------------------------}
  {| This procedure displays numerical values of eigenvalues and mode
  {| occupation energies.
  {+-------------------------------------------------------------------}
   VAR
      SaveScreen:TImage;
      i:Integer;
      Energy:Real;
   BEGIN
      SelectViewPort(4);
      SaveScreen.Get(0,0,Round(0.8*GetMaxX),Round(0.9*GetMaxY));
      Message('Hit any key to continue');
      OpenViewPort(4);
      GenDispVel(Config,t);
      GetModes(Config,t);
      WITH Config DO
         BEGIN
            Print(1,1,'Normal Mode Occupation - t = '+
                       Num2Str(t,5)+' s');
            Print(1,2,'Mode   Freq (Hz)     Energy (J)');
            FOR i := 1 to NumModes DO
               BEGIN
                  Energy := SQR(Amplitude.Value(i)*eVals.Value(i))/2;
                  Print(1,i+2,NumStr(i,2,0));
                  Print(8,i+2,Num2Str(EVals.Value(i)/(2*Pi),8));
                  Print(22,i+2,Num2Str(Energy,8));
               END;
         END;
      StaticPause;
      SelectViewPort(4); SaveScreen.Put(0,0);
   END;

   PROCEDURE ShowCurrentPosVel(VAR Config:ConfigType);
  {+-------------------------------------------------------------------}
  {| This procedure displays the current positions and velocities of
  {| the particles.
  {+-------------------------------------------------------------------}
   VAR
      SaveScreen:TImage;
      i,iPos:Integer;
   BEGIN
      SelectViewPort(4);
      SaveScreen.Get(0,0,Round(0.8*GetMaxX),Round(0.9*GetMaxY));
      Message('Hit any key to continue');
      OpenViewPort(4);
      GenDispVel(Config,t);
      WITH Config DO
         BEGIN
            Print(1,1,'Current Positions and Velocities - t = '+
                       Num2Str(t,5)+' s');
            Print(1,2,'Obj  xEquil    dx        vx');
            Print(1,3,'      (m)      (m)       (m/s)');
            IF TwoDim THEN Print(36,2,'yEquil    dy        vy');
            IF TwoDim THEN Print(36,3,'  (m)     (m)       (m/s)');
            FOR i := 1 to NumFreeMasses DO
               BEGIN
                  Print(1,i+3,NumStr(i,2,0));
                  Print(6,i+3,Num2Str(Masses[i].xEquilPos,8));
                  Print(16,i+3,Num2Str(Displacements.Value(i),8));
                  Print(26,i+3,Num2Str(Velocities.Value(i),8));
                  IF TwoDim THEN
                     BEGIN
                        iPos := i + NumFreeMasses;
                        Print(36,i+3,Num2Str(Masses[i].yEquilPos,8));
                        Print(46,i+3,Num2Str(Displacements.Value(iPos),8));
                        Print(56,i+3,Num2Str(Velocities.Value(iPos),8));
                     END;
               END;
         END;
      StaticPause;
      SelectViewPort(4); SaveScreen.Put(0,0);
   END;

   PROCEDURE ShowMassSpringTypes(VAR Config:ConfigType);
   VAR
      TxtStr:String;
      Spring:SpringTypes;
      Mass:MassTypes;
      x,y : integer;
      image : TImage;
   BEGIN
      HideCursor;
      WITH Views[6] DO BEGIN x:=Vx2-Vx1; y:=GetMaxY-Vy1-1; END;
      SelectViewport(6); Image.Get(0,0,x,y);
      OpenViewPort(6);
      ShowMassSpringPicts(Config);
      Print(1,3,'  A (N/m)  B (N/m)  C (N/m)  D (N/m)            A (kg)   B (kg)   C (kg)');
      TxtStr := '';
      FOR Spring := sA to sD DO WITH Config DO
         TxtStr := TxtStr+NumStr(SpringVals[Spring].SpringConst,9,4);
      TxtStr := TxtStr + '  Infinite';
      FOR Mass := mA to mC DO WITH Config DO
         TxtStr := TxtStr + NumStr(MassVals[Mass].Mass,9,4);
      Print(1,4,TxtStr);
      MESSAGE('Hit any key to continue'); StaticPause;
      SelectViewPort(6); Image.Put(0,0);
      ShowCursor;
   END;

   PROCEDURE ModifyConfig(VAR Config:ConfigType);
   VAR
      SaveScreen:TImage;
      BarChosen:Integer;
   BEGIN
      FileBar.Init;
      FileBar.Row[1] := 'Modify Objects';
      FileBar.Row[2] := 'Modify Springs';
      FileBar.Row[3] := 'Fine Tune Positions';
      FileBar.Row[4] := 'Mod. Numer. Values';
      FileBar.Row[5] := 'Help';
      FileBar.Row[6] := 'DONE';
      BarChosen := 6;
      REPEAT
         ShowCursor;
         OpenViewPort(7); 
         Print(1,1,'Select Option');
         FileBar.Draw(0.01,0.8,6,BarChosen);
         BarChosen := FileBar.Chosen;
         SetWriteMode(NormalPut); SetColor(15);
         OpenViewPort(7);
         CASE BarChosen OF
            1: ModifyMasses(Config);
            2: ModifySprings(Config);
            3: FineTuneMassPositions(Config);
            4: ModifyComponents(Config);
            5: ShowHelp('CoupOsc.HLP','Modify Configuration Help');
         END; {CASE}
{         HotKeys.Clear;}
      UNTIL BarChosen=6;
      SetWriteMode(NormalPut); SetColor(15);
      IF NOT Config.EvecDone THEN
         BEGIN
            AnalyzeConfig(Config);
            Config.Amplitude.Put(1,1.0);
         END;
      OpenPlot(Config);
   END;

   PROCEDURE GetPlotObjects;
   VAR
      SelMassNum:Integer;
   BEGIN
      MESSAGE('Select first object to graph');
      MassSelectKeys.Display;
      SetColor(LightRed);
      PlotObj[1] := 1;
      PlotObj[2] := 2;
      HiLiteSelectedMass(Config,PlotObj[1]);
      REPEAT
         CheckForEvents;
         IF MassSelectKeys.Pressed(KeyHit) THEN
            CASE KeyHit OF
               2: BEGIN
                     HiliteSelectedMass(Config,PlotObj[1]);
                     PlotObj[1] := 1 + PlotObj[1] Mod Config.NumFreeMasses;
                     HiliteSelectedMass(Config,PlotObj[1]);
                  END;
               4:ShowHelp('CoupOsc.HLP','SelectFirstObjectForGraphHelp');
               END; {CASE}
         IF Event.MouseClicked THEN
            BEGIN
               GetMouseClickMass(Config,SelMassNum);
               IF (SelMassNum<>0) AND (SelMassNum<=Config.NumFreeMasses) THEN
                  BEGIN
                     HiLiteSelectedMass(Config,PlotObj[1]);
                     PlotObj[1] := SelMassNum;
                     HiLiteSelectedMass(Config,PlotObj[1]);
                     KeyHit := 1;
                  END;
            END;
      UNTIL KeyHit IN [1,3];
      IF KeyHit=1 THEN
         BEGIN
            MESSAGE('Select second object to graph.');
            PlotObj[2] := PlotObj[1];
            MassSelectKeys.Display;
            SetColor(Yellow);
            REPEAT
               CheckForEvents;
               IF MassSelectKeys.Pressed(KeyHit) THEN
                  CASE KeyHit OF
                     2: BEGIN
                           IF PlotObj[1]<>PlotObj[2] THEN
                              HiliteSelectedMass(Config,PlotObj[2]);
                           REPEAT
                              PlotObj[2] := 1 + PlotObj[2] Mod Config.NumFreeMasses;
                           UNTIL PlotObj[1]<>PlotObj[2];
                           HiliteSelectedMass(Config,PlotObj[2]);
                        END;
                     4: ShowHelp('CoupOsc.HLP','SelectSecondMassForGraphHelp');
                  END; {CASE}
               IF Event.MouseClicked THEN
                  BEGIN
                     GetMouseClickMass(Config,SelMassNum);
                     IF (SelMassNum<>0) AND (SelMassNum<=Config.NumFreeMasses)
                           AND (SelMassNum<>PlotObj[1]) THEN
                        BEGIN
                           IF PlotObj[1]<>PlotObj[2] THEN
                              HiLiteSelectedMass(Config,PlotObj[2]);
                           PlotObj[2] := SelMassNum;
                           HiLiteSelectedMass(Config,PlotObj[2]);
                           KeyHit := 1;
                           ShowCursor;
                        END;
                  END;
            UNTIL KeyHit IN [1,3];
         END;
      SetColor(White);
   END;

   PROCEDURE ChangeProgParms(VAR Config:ConfigType);
   VAR
      ShortestPeriod:Real;
      TxtStr:String;
      ChangeGraph:Boolean;
   BEGIN;
      ChangeGraph := False;
      WITH NewGenScreen DO WITH Config DO
         BEGIN
            init;
            DefineInputPort(0.1,0.9,0.15,0.9);
            LoadLine('PROGRAM PARAMETERS');
            LoadLine('   Maximum Energy per Mode: {       } Joules');
            LoadLine('   Default Animation Interval = {     }*(Shortest Period)');
            LoadLine('             Longest Interval = {     }*(Shortest Period)');
            LoadLine('');
            LoadLine('   Calculation Method');
            LoadLine('     #1 Normal Mode     #1 Numerical');
            LoadLine('      Integration Interval = {     }*(Shortest Period)');
            LoadLine('      #T Include Dynamic Update of Mode Occupation');
            IF NOT TwoDim THEN
               BEGIN
                  LoadLine('');
                  LoadLine('   #T Include position versus time graphs');
                  LoadLine('      #F Change objects graphed');
               END;
            LoadLine('  [  Ok  ]  [Cancel]  [Help]');
            SetHelpFile('CoupOsc.HLP','Change Program Parameters');
            SetNumber(1,MaxModeEnergy);
            SetNumber(2,AnimFact);
            SetNumber(3,MaxAnimFact);
            SetBoolean(4,CalcMethod=NormalModeCalc);
            SetBoolean(5,CalcMethod=NumericalCalc);
            SetNumber(6,NumIntFact);
            SetBoolean(7,ModeUpdate);
            IF NOT TwoDim THEN SetBoolean(8,xVERSUSt);
            AcceptScreen;
            IF NOT canceled THEN
               BEGIN
                  MaxModeEnergy := GetNumber(1);
                  AnimFact := GetNumber(2);
                  MaxAnimFact := GetNumber(3);
                  IF GetBoolean(5)
                     THEN SetUpNumericalCalc(Config)
                     ELSE SetUpNormalModeCalc(Config);
                  NumIntFact := GetNumber(6);
                  ModeUpdate := GetBoolean(7);
                  IF NOT TwoDim THEN
                     BEGIN
                        xVERSUSt := GetBoolean(8);
                        IF xVERSUSt THEN ChangeGraph := GetBoolean(9);
                     END;
                  ShortestPeriod := 2*Pi/Config.EVals.Value(1);
                  dtAnim := AnimFact*ShortestPeriod;
                  dtAnimMax := MaxAnimFact*ShortestPeriod;
                  dt := NumIntFact*ShortestPeriod;
                  OpenPlot(Config);
               END;
            done;
         END;
      IF ChangeGraph AND (Config.NumFreeMasses>1) THEN GetPlotObjects;
   END;

   PROCEDURE ModifyExternalForce(VAR Config:ConfigType);
   BEGIN
      WITH NewGenScreen DO WITH Config DO WITH NumParms DO
         BEGIN
            init;
            DefineInputPort(0.1,0.9,0.2,0.9);
            LoadLine('    EXTERNAL FORCES');
            LoadLine('These forces are ignored unless a numerical solution is used');
            LoadLine('   Damping Coefficient (All Masses) =  {       } N/(m/s)');
            LoadLine('   Periodic Force:');
            LoadLine('      Amplitude:  Initial = {       } N');
            LoadLine('                  Maximum = {       } N');
            LoadLine('      Frequency:  Initial = {       } Hz');
            LoadLine('                  Maximum = {       } Hz');
            SetNumber(1,Damping);
            SetNumber(2,DriveForce);
            SetNumber(3,DFMax);
            SetNumber(4,ExtFreq);
            SetNumber(5,EFMax);
            IF TwoDim THEN
               BEGIN
                  LoadLine('   Direction:  {       } Degrees');
                  SetNumber(6,DFAngle);
               END;
            LoadLine('  [  Ok  ]  [Cancel]  [ Help ]');
            SetHelpFile('CoupOsc.HLP','Modify External Force');
            AcceptScreen;
            IF NOT canceled THEN
               BEGIN
                  Damping := GetNumber(1);
                  DriveForce := GetNumber(2);
                  DFMax := GetNumber(3);
                  ExtFreq := GetNumber(4);
                  EFMax := GetNumber(5);
                  IF TwoDim THEN
                     BEGIN
                        DFAngle := GetNumber(6);
                        xComp := COS(DFAngle*Pi/180);
                        yComp := SIN(DFAngle*Pi/180);
                     END
                     ELSE xComp := 1;
                  IF CalcMethod=NumericalCalc THEN
                     BEGIN
                        SetUpNumericalCalc(Config);
                        OpenPlot(Config);
                     END;
               END;
            done;
         END;
   END;
   PROCEDURE GetDisplacements(VAR Config:ConfigType);
  {+-------------------------------------------------------------------}
  {| This procedure determines the positions of the objects at a
  {| time, t.
  {+-------------------------------------------------------------------}
   VAR
      i:Integer;
      NormVal:Real;
   BEGIN
      WITH Config DO
         CASE CalcMethod OF
            NormalModeCalc:
               BEGIN
                  FOR i:= 1 to NumModes DO
                     BEGIN
                        NormVal := Amplitude.Value(i) *
                                COS(EVals.Value(i)*t + Phase.Value(i));
                        Work.Put(i,NormVal);
                     END;
                  EVecs.MultVector(Work,Displacements);
               END;
            NumericalCalc: GetPosNumerical(Config,t,dt,dtAnim);
         END; {CASE}
   END;

   PROCEDURE PlotGraph;
   VAR
      ArrayIndex,StartPnt,NumToDraw,n,nD,nShift:Integer;
      OldVal1,OldVal2:Real;
      xPos,yPos:Real;
      xScr,yScr:Integer;
   BEGIN
      NumPnts := NumPnts + 1;
      ArrayIndex := NumPnts MOD MaxData;
      SelectViewPort(1); SelectScale(1);
      Data1[ArrayIndex] := Config.Displacements.Value(PlotObj[1]);

      GetMassPos(Config,CurPos,PlotObj[1],xPos,yPos);
      Map(xPos,yPos,xScr,yScr);
      SetColor(LightRed);
      Rectangle(xScr-MaxRadius,yScr-MaxRadius,
                xScr+MaxRadius,yScr+MaxRadius);
      IF Config.NumFreeMasses>1 THEN
         BEGIN
            Data2[ArrayIndex] := Config.Displacements.Value(PlotObj[2]);
            GetMassPos(Config,CurPos,PlotObj[2],xPos,yPos);
            Map(xPos,yPos,xScr,yScr);
            SetColor(Yellow);
            Rectangle(xScr-MaxRadius,yScr-MaxRadius,
                      xScr+MaxRadius,yScr+MaxRadius);
         END;
      SetColor(White);
      OpenViewPort(3); SelectScale(3);
      PlotLine(0,0,MaxData+10,0);  PlotLine(MaxData,-1,MaxData,1);
      IF NumPnts<=MaxData
         THEN BEGIN StartPnt := 0; NumToDraw := NumPnts; END
         ELSE BEGIN StartPnt := (1+NumPnts) MOD MaxData; NumToDraw:=MaxData; END;
      nShift := MaxData - NumToDraw;
      OldVal1 := Data1[StartPnt];
      OldVal2 := Data2[StartPnt];
      FOR n := 1 to NumToDraw-1 DO
         BEGIN
            nD :=  ((StartPnt+n) MOD MaxData);
            SetColor(LightRed);  PlotLine(n+nShift-1,OldVal1,n+nShift,Data1[nD]);
            IF Config.NumFreeMasses>1 THEN
               BEGIN
                  SetColor(Yellow);
                  PlotLine(n+nShift-1,OldVal2,n+nShift,Data2[nD]);
               END;
            SetColor(White);
            OldVal1 := Data1[nd];
            OldVal2 := Data2[nd];
         END;
   END;

   PROCEDURE GenMotion(VAR Config:ConfigType);
  {+-------------------------------------------------------------------}
  {| Draw system for new time value
  {+-------------------------------------------------------------------}
   VAR
      i,MassNum:Integer;
   BEGIN
      Config.EquilDrawn := False;
      FOR i := 0 to 1 DO WITH Config DO
         BEGIN
            GetDisplacements(Config);
            t := t + dtAnim;
            SelectMUPPETPort;
            Rubout(40,2,16,LightBlue);
            Print(40,2,'  t = '+Num2Str(t,6)+' s');
            AnimClean;
            DrawSprings(Config,CurPos,True,NormalPut);
            DrawMasses(Config,AllMasses,CurPos,NormalPut);
            IF (xVERSUSt) THEN PlotGraph;
            IF CalcMethod=NumericalCalc THEN
               BEGIN
                  DrawExternalForce(Config);
                  IF ModeUpDate THEN
                     BEGIN
                        GetModes(Config,t);
                        UpdateNormalModesGraph(Config);
                     END;
               END;
            Anim.Switch;
         END;
   END;

{+--------------------------------------------------------------------------}
{| Menu and HotKey definitions
{+--------------------------------------------------------------------------}
   PROCEDURE SetUpMenu;
   BEGIN
      MainMenu.Init;
      WITH MainMenu DO
         BEGIN
            Column(1,'FILE');
               Row(1,1,'About CUPS');
               Row(1,2,'About Program');
               Row(1,3,'Configuration');
               Row(1,4,'Load New System');
               Row(1,5,'Save System');
               Row(1,6,'EXIT Program');
            Column(2,'InitCond');
               Row(2,1,'Modify Normal Mode Occupations');
               Row(2,2,'Fine Tune Normal Mode Occupations');
               Row(2,3,'Modify Initial Displacements');
               Row(2,4,'Fine Tune Initial Displacements');
               Row(2,5,'Help');
            Column(3,'Info');
               Row(3,1,'Show Eigenvectors');
               Row(3,2,'Show Mass/Spring Types');
               Row(3,3,'Show Current Pos/Vel');
               Row(3,4,'Show Mode Occupation');
               Row(3,5,'Help');
            Column(4,'Modify');
               Row(4,1,'Modify Current System');
               Row(4,2,'Modify Simulation Parameters');
               Row(4,3,'Modify External Force');
               Row(4,4,'Help');
            Column(5,'Reset');
         END;
   END;

   Procedure SetUpHotKeys;
   BEGIN
      WITH HotKeys DO BEGIN
         Init(6);
         key[1] := 'F2-Run/Stop';
         key[2] := 'F3-Step';
         key[3] := 'F4-Reverse';
         key[4] := 'F5-Slower';
         key[5] := 'F6-Faster';
         key[6] := 'F10-Menu';
      END;
      WITH MassSelectKeys DO BEGIN
         Init(4);
         key[1] := 'RETURN-Select';
         key[2] := 'TAB-Next';
         key[3] := 'ESC-Quit';
         key[4] := 'F1-Help';
      END;
      WITH ChangeNormalModeKeys DO BEGIN
         Init(6);
         key[1] := 'RETURN-Done';
         key[2] := 'RIGHT-Increase';
         key[3] := 'LEFT-Decrease';
         key[4] := 'UP-Next';
         key[5] := 'DOWN-Prev';
         key[6] := 'TAB-Phase';
      END;
      WITH ModMassesKeys DO BEGIN
         Init(5);
         key[1] := 'RETURN-Done';
         key[2] := 'TAB-Select';
         key[3] := 'PGUP-Change';
         key[4] := 'DEL-Delete';
         key[5] := 'INS-Add';
      END;
      WITH ModSpringsKeys DO BEGIN
         Init(6);
         key[1] := 'RETURN-Done';
         key[2] := 'TAB-Select';
         key[3] := 'PGUP-Change';
         key[4] := 'DEL-Delete';
         key[5] := 'INS-Add';
         key[6] := 'F1-Help';
      END;
      WITH DisplacementKeys DO BEGIN
           Init(3);
         key[1] := 'RETURN-Done';
         key[2] := 'TAB-Next';
         key[3] := 'F1-Help';
      END;
   END;

   PROCEDURE SIM2init(VAR Config:ConfigType);
   VAR
      i:Integer;
   BEGIN
      WITH Config DO
         BEGIN
            DoneFlag := False;
            RunFlag := False;
            EVecDone := False;
            EquilDrawn := False;
            xVERSUSt := False;
            PlotObj[1]:=1;
            PlotObj[2]:=2;
            CalcMethod := NormalModeCalc;
            EVecs.Init(1,1);
            InvEvecs.Init(1,1);
            DynMat.Init(1,1);
            MassScale.Init(1);
            EVals.Init(1);
            Amplitude.Init(1);
            Phase.Init(1);
            OldDisp.Init(1);
            Displacements.Init(1);
            Velocities.Init(1);
            Work.Init(1);
            Work2.Init(1);
            PosVel.Init(1);
            PosVelP.Init(1);
         END;
      ModeUpDate := False;
      AnimFact := 0.05;
      MaxAnimFact := 0.2;
      NumIntFact := 0.025;
      DegToRad := Pi/180;
      SetUpMenu;
      SetUpHotKeys;
      ExtForceSliders.Init;
      GetObjectRep(Config);
      GraphBackColor := Black;

      Load1d(Config);
      AnalyzeConfig(Config);
      WITH Config DO FOR i := 1 to NumModes DO
         BEGIN
            Amplitude.Put(i,DiskAmplitude[i]);
            Phase.Put(i,DiskPhase[i]);
         END;
      GenDispVel(Config,0);
      OpenPlot(Config);
   END;

   PROCEDURE StepRun(VAR Config:ConfigType);
   BEGIN
      HideCursor;
      Anim.Continue;
      GenMotion(Config);
      Anim.Pause;
      ShowCursor;
   END;

   PROCEDURE StartRun(VAR RunFlag:Boolean);
   BEGIN
      RunFlag := True;
      HideCursor;
{      MouseBorders(0,GetMaxY-20,GetMaxX,GetMaxY-10);}
      Anim.Continue;
   END;

   PROCEDURE StopRun(VAR RunFlag:Boolean);
   BEGIN
      Anim.Pause;
      RunFlag := False;
      MouseBorders(0,0,GetMaxX,GetMaxY);
      IF Config.CalcMethod=NumericalCalc THEN 
         BEGIN
            GetModes(Config,t);
            UpdateNormalModesGraph(Config);
            SetActivePage(1);
            UpdateNormalModesGraph(Config);
            SetActivePage(0);
         END;
      ShowCursor;
   END;

   PROCEDURE HandleMenu(VAR Config:ConfigType;
                        VAR DoneFlag:Boolean);
   VAR
      i:Integer;
   BEGIN
      StopRun(RunFlag);
{      HotKeys.Clear;}
      IF MainMenu.Chosen THEN WITH MainMenu DO
         CASE colChosen OF
          1: CASE rowChosen OF                                  {FILE}
              1: aboutCUPS;
              2: ShowHelp('CoupOsc.HLP','General Help');
              3: Configuration;
              4: LoadStartUpConfig(Config);
              5: SaveConfig(Config);
              6: DoneFlag := TRUE;
             END; {CASE}
          2: CASE rowChosen OF                               {INIT COND}
              1: ChangeNormalModeOccup(Config);
              2: FineTuneNormalModeOccup(Config);
              3: ChangeInitialDisplacements(Config);
              4: FineTuneInitialDisplacements(Config);
              5: ShowHelp('CoupOsc.HLP','Initial Condition Help');
             END; {CASE}
          3: CASE rowChosen OF                             {Information}
              1: ShowEigenVectors(Config);
              2: ShowMassSpringTypes(Config);
              3: ShowCurrentPosVel(Config);
              4: ShowModeOcc(Config);
              5: ShowHelp('CoupOsc.HLP','Show Help');
             END; {CASE}
          4: CASE rowChosen OF                             {Modify}
              1: ModifyConfig(Config);
              2: ChangeProgParms(Config);
              3: ModifyExternalForce(Config);
              4: ShowHelp('CoupOsc.HLP','Modify Help');
             END; {CASE}
          5: BEGIN                                         {Reset}
                WITH Config DO FOR i := 1 to NumModes DO
                BEGIN
                   Amplitude.Put(i,DiskAmplitude[i]);
                   Phase.Put(i,DiskPhase[i]);
                END;
                GenDispVel(Config,0);
                OpenPlot(Config);
             END;
         END; {CASE}
      Debounce;
      ClearMKBuffers;
      CheckForEvents;
      MainMenu.Display;
      HotKeys.Display;
   END;

   PROCEDURE HandleExtForceSliders(VAR Config:ConfigType;
                                   VAR RunFlag:Boolean);
   BEGIN
      Debounce;
      ClearMKBuffers;
      CheckForEvents;
      DrawExternalForce(Config);
      WITH Config.NumParms DO
         BEGIN
            DriveForce := ExtForceSliders.Value(1);
            ExtFreq := ExtForceSliders.Value(2);
            CurExtForce := Driveforce*COS(t*ExtFreq*2*Pi);
         END;
      DrawExternalForce(Config);
   {Must update slider on both screens}
      SetActivePage(1);
      ExtForceSliders.DrawAll;
      SetActivePage(0);
      SelectMUPPETport;
   END;

   PROCEDURE HandleHotKey(VAR Config:ConfigType;
                           KeyHit: Byte;
                           VAR RunFlag,DoneFlag:Boolean);
  {+-------------------------------------------------------------------}
  {| This procedure handles hot keys for animation
  {+-------------------------------------------------------------------}
   BEGIN
     {+----------------------------------------------------------------}
     {| Determine the appropriate action for the hot key choice
     {+----------------------------------------------------------------}
      Debounce;
      ClearMKBuffers;
      CheckForEvents;
      CASE keyHit OF
         1: IF RunFlag THEN StopRun(RunFlag)               {F2 - Run/Stop}
                       ELSE StartRun(RunFlag);
         2: IF RunFlag THEN StopRun(RunFlag)               {F3 - Step}
                       ELSE StepRun(Config);
         3: BEGIN dtAnim := -dtAnim; NumPnts := 0 END;     {F4 - Reverse}
         4: BEGIN dtAnim := dtAnim/2;NumPnts := 0 END;     {F5 - Slower}
         5: IF dtAnim<dtAnimMax THEN                       {F6 - Faster}
            BEGIN dtAnim := dtAnim*2;NumPnts := 0 END;
         6: HandleMenu(Config,DoneFlag);                    {F10 - Menu}
      END {CASE};
   END;

   PROCEDURE HandleKeyPressed(VAR Config:ConfigType);
   BEGIN
      IF (Config.CalcMethod=NumericalCalc) AND (' ' = Event.ReadKey) THEN
        {+-------------------------------------------------------------}
        {| Change applied mass if spacebar is hit
        {+-------------------------------------------------------------}
         BEGIN
            DrawExternalForce(Config);
            WITH Config DO WITH NumParms DO
               AppliedMass := 1 + AppliedMass MOD NumFreeMasses;
            DrawExternalForce(Config);
         END;
      ClearMKBuffers;
   end;

   PROCEDURE HandleMouseClick(VAR Config:ConfigType; VAR DoneFlag:Boolean);
   BEGIN
      WITH Event DO
         BEGIN
            IF y<22 THEN HandleMenu(Config,DoneFlag);
            WITH Views[7] DO
               IF (x>=vx1) AND (x<=vx2) AND (y>=vy1) AND (y<=vy2) THEN
                  ChangeNormalModeOccup(Config);
            WITH Views[1] DO
               IF (x>=vx1) AND (x<=vx2) AND (y>=vy1) AND (y<=vy2) THEN
                  ChangeInitialDisplacements(Config);
         END;
      Debounce;
      ClearMKBuffers;
      CheckForEvents;
   END;

BEGIN
   Anim.Init;
   CUPSinit;
   ClearMUPPETport; Anim.Switch; ClearMUPPETport; Anim.Switch;
   Sim2Init(Config);
   ShowHelp('CoupOsc.HLP','General Help');
   REPEAT
      CheckForEvents;
      IF HotKeys.Pressed(KeyHit) THEN HandleHotKey(Config,KeyHit,RunFlag,DoneFlag);
      IF Event.ExtendedKey AND (Event.ReadKey=';') THEN
         ShowHelp('CoupOsc.HLP','General Help');
      IF NOT RunFlag
         THEN
            BEGIN
               IF Event.KeyPressed THEN HandleKeyPressed(Config);
               IF (Config.CalcMethod=NumericalCalc) THEN
               IF ExtForceSliders.Changed THEN
                  HandleExtForceSliders(Config,RunFlag);
               IF event.mouseClicked THEN HandleMouseClick(Config,DoneFlag);
            END
         ELSE
            IF Event.MouseClicked OR Event.KeyPressed
               THEN
                  BEGIN
                     StopRun(RunFlag);
                     Debounce;
                     ClearMKBuffers;
                     CheckForEvents;
                  END
               ELSE GenMotion(Config);
   UNTIL DoneFlag;
   Anim.Close;
   CUPSdone;
END.


