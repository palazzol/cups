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


{ Start of file QM_FPTI.PAS }

{ ----------------------------------------------------------------------
   Program Title:    Free Particles - Time Independent
   Author:           Douglas E. Brandt
   Version:          0.11
   Program Summary:  This is one section of the CUPS Modern Physics
                     One Dimensional Wave Mechanics Program.  It is written
                     to give insight into the complementary nature of
                     position and momentum in wave mechanics in relation
                     to the uncertainty principle.  It is enable to
                     investigate Gaussian, rectangular, and triangular
                     shaped wave packets.  It can be used at an elementary
                     level by not displaying real and imaginary components
                     of the functions.
   Library Files:    CRT,DOS,GRAPH,MUPPET,CUPS,CUPSGRPH,CUPSFUNC,CUPSPROC,
                     CUPSGUI,OBJECTS,QM_FUNC
   Utilities:
   Algorithms:
   NOTE:             This file can be compiled as a stand alone program
                     or as a unit.  If MAIN is defined by a compiler define
                     directive then this file will compile as a stand alone
                     program.  If MAIN is not defined by a compiler define
                     directive then this file will compile to a unit.
------------------------------------------------------------------------}

{$IFNDEF MAIN}

UNIT QM_FPTI;

INTERFACE

{$ENDIF}

USES Crt, Dos, Graph, CUPSmupp, CUPS, CUPSgrph, CUPSfunc, CUPSproc, CUPSgui,
        CUPSpars, Objects, QM_Ifac, XSliders;

{$IFNDEF MAIN}

PROCEDURE Free_Particle(VAR QMInterface: TInterface; VAR Message: MessageRec);

IMPLEMENTATION


PROCEDURE Free_Particle(VAR QMInterface: Tinterface; VAR Message: MessageRec);

{$ENDIF}

TYPE
   FunctionXYZ = FUNCTION(x,y,z: Real): Real;

CONST

   PsiSquaredColor = 13;
   RealPsiColor = 11;
   ImagPsiColor = 10;
   Length = 512;
   Stand_alone = True;

VAR
   Psi_Squared, Phi_Squared, xvec, x2vec: DVector;
   Psi, Phi, FFTOut: CDVector;
   Null_DVector: DVector;

   i: Integer;
   Free_Time_Ind_Disp: Base_Display;
   Plot_Components,ReDo: Boolean;
   done, HelpNumber: integer;
   xcenter, xwidth, pcenter, pwidth, psitype, phitype: real;
   Change_Wave_Parms, User_Function_Screen: TInputScreen;
   Psi_Function: Integer;
   Phi_Function: Integer;
   Psi_Function_String, Phi_Function_String: String;
   xcenter_String,sigmax_String,pcenter_String,sigmap_String: String;
   OldSlider: ARRAY [1..4] OF Real;
   PsiTypeKnown, PsiWidthKnown: Boolean;
   ParserHelp, currentHelpScreen: HelpScrType;
   parser: TParser;
   {$IFDEF MAIN}
   next: Integer;
   {$ENDIF}

PROCEDURE Initialize_Vectors;

   VAR
      i: Integer;

   BEGIN
      Psi_Squared.init(Length);
      Psi.init(Length);
      Phi_Squared.init(Length);
      Phi.init(Length);
      FFTOut.init(Length);
      xvec.init(Length);
      FOR i := 0 TO Length-1 DO xvec.put(i+1,(i-Length/2)*10/Length);
      x2vec.init(Length);
      FOR i := 0 TO Length-1 DO x2vec.put(i+1,(i-1-Length/2)*2*Pi/10);
      Null_DVector.init(Length);
      FOR i := 1 TO Length DO Null_DVector.put(i,0);
   END;

PROCEDURE Free_Vectors;

   BEGIN
      Psi_Squared.Free;
      Psi.Free;
      Phi_Squared.Free;
      Phi.Free;
      FFTOut.Free;
      xvec.Free;
      x2vec.Free;
      Null_DVector.Free;
   END;

{
PROCEDURE PlotDVectors(x, y : DVector; ibeg, iend: Integer);
VAR
   ib, ie, di, i, n : Integer;
   oldX,oldY,newX,newY : integer;
BEGIN
   IF ibeg <> iend THEN
   BEGIN
      IF scaleNumber=0 THEN autoScaling(0,x,y);
      IF ibeg < iend THEN BEGIN ib:=ibeg; ie:=iend; di:= 1 END
      ELSE                BEGIN ib:=iend; ie:=ibeg; di:=-1 END;
      n := ibeg;
      map( x.vecPtr^[n], y.vecPtr^[n], oldX, oldY);
      HideMouse;
      FOR i := ib+1 TO ie DO
      BEGIN
         n:=n+di;
         map( x.vecPtr^[n], y.vecPtr^[n], newX, newY);
         Line (oldX,oldY,newX,newY);
         oldX:=newX; oldY:=newY;
      END;
      ShowMouse;
   END;
END; }

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

   PROCEDURE LabelTickY( VAR y : real);
   BEGIN
      WITH GraphScales[ScaleNumber] DO
      IF (y>=Min) AND (y<=Max) THEN      {if inside the plot then...}
      BEGIN
         PlotLine(xmin, y, xmin + TickLength, y);
         Map(xmin, y, Xs, Ys);
         IF (TickY>1E-5) AND (abs(y)<1E-6) THEN y := 0;
         OutTextXY(Xs - 5, Ys, Num2Str(y,7));
      END;
      y := y + TickY;
   END;

BEGIN
   Environment.Save;
   Environment.Standardize;
   SetColor(color);
   WITH Views[ViewPortNumber] DO SetViewPort(Vx1,Vy1,Vx2,Vy2,ClipOff);
   WITH GraphScales[ScaleNumber] DO
   BEGIN
      IF (((xmax-xmin)/Tickx) > 20) OR (((ymax-ymin)/Ticky) > 20) THEN
      BEGIN
         Announce('Error: You ask for too many Ticks.');
         PAUSE;
         EXIT;
      END;
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
      IF TickY > 0 THEN
      BEGIN
         TickLength := 0.02 * (xmax - xmin);
         y := Yint;
         IF yMin<yMax THEN
          BEGIN
             max := yMax;
             min := yMin;
          END
         ELSE
          BEGIN
             max := yMin;
             min := yMax;
          END;
         WHILE y <= max DO LabelTickY(y);       {plot along y axis}
         y := Yint;
         TickY := -TickY;
         WHILE y >= min DO LabelTickY(y);       {plot along -y axis}
      END;  {if}
      ShowMouse;
      Environment.Reset;
   END; {with GraphScales}
END;

PROCEDURE autoxScaling (sNum : integer; VAR x: DVector; ymin,ymax: Real);
VAR
   min,max,Tick,CInt : point2D;
   n : integer;
BEGIN
   min[2] := ymin;
   max[2] := ymax;
   x.MinMax(n,min[1],n,max[1]);
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

FUNCTION Gaussian(x, sigma, center: Real): Real;

   VAR
      A: Real;

   BEGIN

      A := -1.0 * Sqr(x - center)/(4 * Sqr(sigma));
      IF A > -28 THEN A := Exp(A) ELSE A := 0;
      Gaussian := A/Sqrt(sigma*Sqrt(2 * Pi));
   END;


FUNCTION Rectangle(x, width, center: Real): Real;

   BEGIN
      width := sqrt(12)*width;
      IF ABS(center - x) < (width/2) THEN Rectangle := 1/Sqrt(width)
      ELSE Rectangle := 0;
   END;


FUNCTION FTRectangle(x, width, center: Real): Real;

   VAR
      A: Real;

   BEGIN
      width := width/sqrt(12);
      IF x-center <> 0 THEN A := sqrt(width)*
                 (sin((x-center)/(2*width))/(x-center))
      ELSE A := 1/sqrt(4*width);
      FTRectangle := A/Sqrt(Pi/2);
   END;


FUNCTION Triangle(x, width, center: real): Real;

   BEGIN
      width := width*sqrt(10);
      IF (x < center) AND (x > center - width)
          THEN Triangle := ((x-center)/width + 1)*Sqrt(3/(2*width));
      IF (x > center) AND (x < center + width)
          THEN Triangle := ((center-x)/width +1)*Sqrt(3/(2*width));
      IF (x <= center - width) OR (x >= center + width) THEN Triangle := 0;
      IF x = center THEN Triangle := Sqrt(3/(2*width));
   END;


FUNCTION FTTriangle(x, width, center: real): real;

   BEGIN
      width := width/sqrt(10);
      IF x <> center THEN FTTriangle := (1-cos((x-center)/width))/
                                       (Sqr((x-center))/width)*Sqrt(3/(Pi/Width))
      ELSE FTTriangle := Sqrt(3/(pi/width))/(2*width);
   END;


FUNCTION User(x, width , center: real): real;

   VAR
      t: Real;

   BEGIN
      User := Parser.f(x,width,center,t);
   END;

FUNCTION UserFFT(x, width, center: real): real;

   BEGIN
      i := Trunc(x/10*length + length/2 + 0.1);
      UserFFT := FFTOut.Repart(i);
   END;

FUNCTION f(ftype: Integer; x,width,center: Real): Real;

   BEGIN
      CASE ftype OF
         1: f := Gaussian(x,width,center);
         2: f := Rectangle(x,width,center);
         3: f := Triangle(x,width,center);
         4: f := User(x,width,center);
         12: f := FTRectangle(x,width,center);
         13: f := FTTriangle(x,width,center);
         14: f := UserFFT(x,width,center);
       END;
    END;


PROCEDURE PsiChar(x,y,height,width: Real);

   BEGIN
      PlotLine(x,y,x,y+height);
      PlotLine(x-width/2,y+0.75*height,x-0.4*width,y+0.625*height);
      PlotLine(x-0.4*width,y+0.625*height,x-0.15*width,y+0.375*height);
      PlotLine(x-0.15*width,y+0.375*height,x,y+0.3*height);
      PlotLine(x+0.4*width,y+0.625*height,x+width/2,y+0.75*height);
      PlotLine(x+0.15*width,y+0.375*height,x+0.4*width,y+0.625*height);
      PlotLine(x,y+0.3*height,x+0.15*width,y+0.375*height);
   END;

PROCEDURE PhiChar(x,y,height,width: Real);

   BEGIN
      PlotLine(x,y,x,y+height);
      PlotLine(x-width/2,y+0.45*height,x-width/2,y+0.55*height);
      PlotLine(x+width/2,y+0.45*height,x+width/2,y+0.55*height);
      PlotLine(x-0.5*width,y+0.45*height,x-0.15*width,y+0.25*height);
      PlotLine(x-0.5*width,y+0.55*height,x-0.15*width,y+0.75*height);
      PlotLine(x+0.5*width,y+0.45*height,x+0.15*width,y+0.25*height);
      PlotLine(x+0.5*width,y+0.55*height,x+0.15*width,y+0.75*height);
      PlotLine(x-0.15*width,y+0.25*height,x+0.15*width,y+0.25*height);
      PlotLine(x-0.15*width,y+0.75*height,x+0.15*width,y+0.75*height);
   END;


PROCEDURE Initialize_Display_Object;

   BEGIN
      WITH Free_Time_Ind_Disp DO
         BEGIN
            Title := 'Uncertainty Principle';
            Window_num := 2;
            Window_corner[1,1].x := 0.05;
            Window_corner[1,1].y := 0.25;
            Window_corner[1,2].x := 0.50;
            Window_corner[1,2].y := 0.85;
            Window_corner[2,1].x := 0.55;
            Window_corner[2,1].y := 0.25;
            Window_corner[2,2].x := 1.00;
            Window_corner[2,2].y := 0.85;
            {Window_corner[3,1].x := 0.75;
            Window_corner[3,1].y := 0.12;
            Window_corner[3,2].x := 0.95;
            Window_corner[3,2].y := 0.82;}
            Init := True;
         END;
   END;

PROCEDURE HelpScreenInit;

   BEGIN
         currentHelpScreen[01] := '         HELP FOR UNCERTAINTY PRINCIPLE           ';
         currentHelpScreen[02] := '                                                  ';
         currentHelpScreen[03] := '   With the currently displayed functions, sliders';
         currentHelpScreen[04] := 'can vary width and mean of distributions.  To     ';
         currentHelpScreen[05] := 'select a new function type, use the particle      ';
         currentHelpScreen[06] := 'parameter function under the parameters menu item.';
         currentHelpScreen[07] := 'This will allow you to select a function type or  ';
         currentHelpScreen[08] := 'input your own function.  The program will        ';
         currentHelpScreen[09] := 'calculate the appropriate transform and calculate ';
         currentHelpScreen[10] := 'the approriate means and standard deviations.     ';
         currentHelpScreen[11] := '                                                  ';
         currentHelpScreen[12] := 'If you do select user defined function, make sure ';
         currentHelpScreen[13] := 'the function evaluates to a real number and that  ';
         currentHelpScreen[14] := 'the only variable is the one you have specified   ';
         currentHelpScreen[15] := 'with the radio buttons.                           ';
         currentHelpScreen[16] := '                                                  ';
         currentHelpScreen[17] := 'The F6-Show Re hot key will toggle on and off the ';
         currentHelpScreen[18] := 'display of the real and imaginary components of   ';
         currentHelpScreen[19] := 'the wavefunction.';
         currentHelpScreen[20] := '                                                  ';
         currentHelpScreen[21] := 'See text for dicussion of units.                  ';
         currentHelpScreen[22] := '';
         currentHelpScreen[23] := '                                                  ';
         currentHelpScreen[24] := '    Press any Key or Click Mouse to continue.     ';
         currentHelpScreen[25] := '                                                  ';

   END;

PROCEDURE Define_Input_Screens;

   BEGIN
      WITH Change_Wave_Parms do
      begin
         init;
         DefineInputPort(0.1,0.9,0.20,0.80);
         Loadline('               Parameters of Distributions                 ');
         Loadline('                                                           ');
         Loadline('    Select a function type in variable  #2 x or #2 p ');
         Loadline('                                                           ');
         Loadline('    #1 Gaussian    |    Mean x = {    }   Mean p = {    }   ');
         Loadline('    #1 Rectangle   |    Uncertainty in specidied ');
         Loadline('    #1 Triangle    |         variable = {    }                                          ');
         Loadline(' ---------------------------------------------------------- ');
         Loadline('    #1 User Defined function in above variable =   ');
         Loadline(' "                                                       " ');
         Loadline('                                                           ');
         Loadline('                [  Ok  ]   [Cancel]   [ Help ]                     ');
         SetNumber(4,pcenter);
         SetNumber(5,xcenter);
         SetNumber(8,xwidth);
      end;
      WITH User_Function_Screen do
      begin
         init;
         DefineInputPort(0.1,0.9,0.20,0.80);
         Loadline('                 User Defined Function                     ');
         Loadline('                                                           ');
         Loadline('    #1 Spatial Distribution  #1 Momentum Distribution      ');
         Loadline('                                                           ');
         Loadline('    Input a function that can be parsed by the parser      ');
         Loadline('    for the distribution in the variable specified above.  ');
         Loadline('    The effective domain of the function is (-5,5).        ');
         Loadline('                                                           ');
         Loadline(' "                                                       " ');
         Loadline('  ');
         Loadline('  ');
         Loadline('          F1 will display the parser help screen           ');
         Loadline('                                                           ');
         Loadline('                   [  Ok  ]   [Cancel]                      ');
      end;
   END;


PROCEDURE Plot_functions;

   VAR
      dummy1,dummy2: Integer;
      maxpsiR,MinPsiR,MaxPsiI,MinPsiI,MaxPsi2,MinPsi2: Real;

   BEGIN
         DefineViewPort(10,0.0,1.0,0.19,0.89);
         SelectViewPort(10);
         ClearViewPort;
         SetColor(15);
         Print(13,1,'Spatial Distribution');
         Print(52,1,'Momentum Distribution');
         DefineViewPort(11,0.47,0.5,0.18,0.24);
         SelectViewPort(11);
         Print(1,1,'x');
         DefineViewPort(11,0.97,1.0,0.18,0.24);
         SelectViewPort(11);
         Print(1,1,'p');
         OpenViewPort(1);
         SelectViewPort(1);
         SelectScale(7);
         SetColor(3);
         {Axis(0,0,1,1);}
         IF Psi_function = 14 THEN
             BEGIN
                psi.Re.minmax(dummy1,MinPsiR,dummy2,maxPsiR);
                psi.Im.minmax(dummy1,MinPsiI,dummy2,maxPsiI);
                psi_squared.MinMax(dummy1,MinPsi2,dummy2,MaxPsi2);
                IF MinPsiI < MinPsiR THEN MinPsiR := MinPsiI;
                IF MaxPsiI > MaxPsiR THEN MaxPsiR := MaxPsiI;
                IF MaxPsi2 > MaxPsiR THEN MaxPsiR := MaxPsi2;
                IF Plot_Components THEN Autoxscaling(10,x2vec,MinPsiR, MaxPsiR)
                ELSE Autoxscaling(10,x2vec,0,MaxPsi2);
             END
         ELSE 
             BEGIN
                IF Plot_Components THEN Autoxscaling(10,xvec,-2,2)
                ELSE Autoxscaling(10,xvec,0,2);
             END;
         SelectScale(10);
         HideCursor;

         IF Plot_Components = True THEN
            BEGIN
               SetColor(RealPsiColor);
               IF psi_function = 14 THEN PlotDVectors(x2vec, Psi.Re, 1, length)
                  ELSE PlotDVectors(xvec, Psi.Re, 1, length);
               Print(1,2,'Re');
               SelectScale(8);
               PsiChar(-4.2,1.75,0.07,0.2);
               SetColor(ImagPsiColor);
               PsiChar(-4.2,1.61,0.07,0.2);
               SelectScale(10);
               IF psi_function = 14 THEN PlotDVectors(x2vec, Psi.Im, 1, length)
                  ELSE PlotDVectors(xvec, Psi.Im, 1, length);
               Print(1,3,'Im');
            END;
         SetColor(PsiSquaredColor);
         IF psi_function = 14 THEN PlotDVectors(x2vec, Psi_Squared, 1, length)
              ELSE PlotDVectors(xvec, Psi_Squared, 1, length);
         {Print(2,1,'Psi Squared');}
         SelectScale(8);
         PsiChar(-4.7,1.88,0.07,0.2);
         PlotLine(-4.51,1.88,-4.51,1.95);
         PlotLine(-4.89,1.88,-4.89,1.95);
         PlotLine(-4.41,1.97,-4.31,1.97);
         PlotLine(-4.31,1.97,-4.31,1.95);
         PlotLine(-4.31,1.95,-4.41,1.95);
         PlotLine(-4.41,1.95,-4.41,1.92);
         PlotLine(-4.41,1.92,-4.31,1.92);
         SelectScale(7);
         OpenViewPort(2);
         SelectViewPort(2);
         IF Phi_function = 14 THEN
             BEGIN
                phi.Re.minmax(dummy1,MinPsiR,dummy2,maxPsiR);
                phi.Im.minmax(dummy1,MinPsiI,dummy2,maxPsiI);
                phi_squared.MinMax(dummy1,MinPsi2,dummy2,MaxPsi2);
                IF MinPsiI < MinPsiR THEN MinPsiR := MinPsiI;
                IF MaxPsiI > MaxPsiR THEN MaxPsiR := MaxPsiI;
                IF MaxPsi2 > MaxPsiR THEN MaxPsiR := MaxPsi2;
                IF Plot_Components THEN Autoxscaling(10,x2vec,MinPsiR, MaxPsiR)
                ELSE Autoxscaling(10,x2vec,0,MaxPsi2);
             END
         ELSE 
             BEGIN
                IF Plot_Components THEN Autoxscaling(10,xvec,-2,2)
                ELSE Autoxscaling(10,xvec,0,2);
             END;
         SelectScale(10);
         SetColor(3);
         IF Plot_Components = True THEN
            BEGIN
               SetColor(RealPsiColor);
               IF phi_function = 14 THEN PlotDVectors(x2vec, Phi.Re, 1, length)
                  ELSE PlotDVectors(xvec, Phi.Re, 1, length);
               Print(1,2,'Re');
               SelectScale(8);
               PhiChar(-4.2,1.75,0.07,0.2);
               SetColor(ImagPsiColor);
               PhiChar(-4.2,1.61,0.07,0.2);
               SelectScale(10);
               IF phi_function = 14 THEN PlotDVectors(x2vec, Phi.Im, 1, length)
                  ELSE PlotDVectors(xvec, Phi.Im, 1, length);
               Print(1,3,'Im');
            END;
         SetColor(PsiSquaredColor);
         IF phi_function = 14 THEN PlotDVectors(x2vec, Phi_Squared, 1, length)
              ELSE PlotDVectors(xvec, Phi_Squared, 1, length);
         ShowCursor;
         SelectScale(8);
         PhiChar(-4.7,1.88,0.07,0.2);
         PlotLine(-4.51,1.88,-4.51,1.95);
         PlotLine(-4.89,1.88,-4.89,1.95);
         PlotLine(-4.41,1.97,-4.31,1.97);
         PlotLine(-4.31,1.97,-4.31,1.95);
         PlotLine(-4.31,1.95,-4.41,1.95);
         PlotLine(-4.41,1.95,-4.41,1.92);
         PlotLine(-4.41,1.92,-4.31,1.92);
   END;


PROCEDURE Evaluate(VAR vector: CDvector; functionType: Integer;
                                     width, center, pcenter: Real);

   VAR
      i: Integer;
      s, p, z: Complex;
      r,q: Real;

   BEGIN
      s.Re := 0;
      FOR i := 1 TO vector.length DO
         BEGIN
            q:= xvec.value(i);
            r := f(functionType,xvec.value(i), width, center);
            s.Im := xvec.value(i)*Pcenter;
            CExp(s,p);
            RCProd(r,p,z);
            vector.put(i,z);
         END;
   END;


FUNCTION Zero(x, sigma, center: Real): Real;

   BEGIN
      Zero := 0;
   END;


{PROCEDURE Write_info;

   VAR
      Dummy_String: String;

   BEGIN
      OpenViewPort(3);
      SelectViewPort(3);
      SetColor(15);
      Print(2,2,'Psi Function:');
      Print(5,3, Psi_Function_String);
      Print(2,5,'Phi Function:');
      Print(5,6, Phi_Function_String);
      Print(2,8,'Mean x = ' + xcenter_String);
      Print(2,10,'Sigma x = ' + sigmax_String);
      Print(2,12,'Mean p = ' + pcenter_String);
      Print(2,14,'Sigma p = ' + sigmap_String);
   END;}


FUNCTION Test(x: Real): Real; FAR;

   BEGIN
      Test := 0;
   END;

FUNCTION ChangedSlider: Integer;

   BEGIN
      WITH QMInterface DO
      BEGIN
      IF OldSlider[1] <> Sliders.Value(1) THEN ChangedSlider := 1
      ELSE IF OldSlider[2] <> Sliders.Value(2) THEN ChangedSlider := 2
      ELSE IF OldSlider[3] <> Sliders.Value(3) THEN ChangedSlider := 3
      ELSE IF OldSlider[4] <> Sliders.Value(4) THEN ChangedSlider := 4;
      END;
   END;



PROCEDURE ProcessSlider;

   BEGIN
      WITH QMInterface DO
      BEGIN
      CASE ChangedSlider OF
         1: BEGIN
               OldSlider[1] := Sliders.value(1);
               xcenter := OldSlider[1];
               xcenter_String := NumStr(xcenter,4,2);
            END;
         2: BEGIN
               pcenter := Sliders.value(2);
               OldSlider[2] := pcenter;
               xcenter_String := NumStr(xcenter,4,2);
            END;
         3: BEGIN
               xwidth := Sliders.Value(3);
               pwidth := pwidth*OldSlider[3]/xwidth;
               OldSlider[3] := Sliders.value(3);
               OldSlider[4] := pwidth;
               IF PsiType <> 3 THEN Sliders.Reset(4,log10(OldSlider[4]))
                  ELSE
                     BEGIN
                        sigmap_String := NumStr(Sqrt(3)*pwidth,4,2);
                        SelectViewPort(12);
                        SetColor(15);
                        ClearViewPort;
                        Print(4,1,'Delta p = ' + sigmap_String);
                     END;
            END;
         4: BEGIN
               pwidth := Sliders.Value(4);
               xwidth := xwidth*OldSlider[4]/pwidth;
               OldSlider[4] := Sliders.value(4);
               OldSlider[3] := xwidth;
               IF PsiType <> 3 THEN Sliders.Reset(3,log10(OldSlider[3]))
                  ELSE
                     BEGIN
                        sigmax_String := NumStr(Sqrt(3)*xwidth,4,2);
                        SelectViewPort(12);
                        SetColor(15);
                        ClearViewPort;
                        Print(4,1,'Delta x = ' + sigmax_String);
                     END;
               
            END;
      END;
      Evaluate(Psi, Psi_Function, xwidth, xcenter, pcenter);
      Evaluate(Phi, Phi_function, pwidth, pcenter, -xcenter);
      Psi.AbsSqr(Psi_Squared);
      Phi.AbsSqr(Phi_Squared);
      Plot_Functions;

      END;
   END;

PROCEDURE FFT(Input: CDVector; VAR Output: CDVector);

   VAR
      One, OneI, z, z1, z2, z3, z4: Complex;
      step,n,m,k,q,p,i,nmax: Integer;
      r,angle, Norm: Real;

   BEGIN
      One.Re := 1;
      One.Im := 0;
      OneI.Re := 0;
      OneI.Im := 1;
      Output.Equate(One,Input);
      n := Input.Length;
      FOR i := 1 TO (N DIV 2) DO
         BEGIN
            Output.Get(i,z);
            Output.Get(i + (N DIV 2), z1);
            Output.put(i + (N DIV 2), z);
            Output.put(i, z1);
         END;
      m := n;
      k := 1;
      FOR q := 1 TO n DO
         BEGIN
            IF k > q THEN
               BEGIN
                  Output.Get(k,z);
                  Output.Get(q,z1);
                  Output.Put(k,z1);
                  Output.Put(q,z);
               END;
            m := n DIV 2;
            WHILE ((m >= 2) AND (k > m)) DO
               BEGIN
                  k := k - m;
                  m := m DIV 2;
               END;
            k := k + m;
         END;
      nmax := 1;
      WHILE (n > nmax) DO
         BEGIN
            step := 2*nmax;
            angle := Pi/nmax;
            z1.Re := -2.0 * SQR(sin(angle/2.0));
            z1.Im := sin(angle);
            z2.Re := 1.0;
            z2.Im := 0.0;
            FOR q:= 1 TO nmax DO
               BEGIN
                  m := q;
                  FOR p := 0 TO ((n-m) DIV step) DO
                     BEGIN
                        i := m + P*step;
                        k := i + nmax;
                        z.Re := z2.Re*Output.RePart(k)-z2.Im*Output.ImPart(k);
                        z3.Re := -z.Re;
                        z.Im := z2.Re*Output.ImPart(k)+z2.Im*Output.RePart(k);
                        z3.Im := -z.Im;
                        Output.Get(i,z4);
                        Output.AddZ(i,z);
                        Output.Put(k,z3);
                        Output.AddZ(k,z4);
                     END;{FOR}
                        r := z2.Re;
                        z2.Re := z2.Re + z2.Re*z1.Re - z2.Im*z1.Im;
                        z2.Im := z2.Im + z2.Im*z1.Re + r*z1.Im;
               END;{FOR}
            nmax := step;
         END;{WHILE}
      FOR i := 1 TO (N DIV 2) DO
         BEGIN
            Output.Get(i,z);
            Output.Get(i + (N DIV 2), z1);
            Output.put(i + (N DIV 2), z);
            Output.put(i, z1);
         END;
      Norm := 0;
      FOR i := 1 TO N DO
         BEGIN
            Norm := Norm + Sqr(Output.Re.value(i)) + Sqr(Output.Im.value(i));
         END;
      Norm := Sqrt(Norm*2*Pi/10);
      FOR i := 1 TO N DO
         BEGIN
            Output.Re.Put(i, Output.Re.Value(i)/Norm);
            Output.Im.Put(i, Output.Im.Value(i)/Norm);
         END;
   END;

PROCEDURE Normalize(x: DVector; VAR Psi: CDVector);

   VAR
      i: Integer;
      m, deltax: Real;

   BEGIN
      deltax := x.value(2)-x.value(1);
      m := 0;
      FOR i:= 1 TO Psi.Length DO
         BEGIN
            m := m + Sqr(Psi.RePart(i)) + Sqr(Psi.ImPart(i));
         END;
            m := m * deltax;
      IF m > 0 THEN
         FOR i := 1 TO Length DO
            BEGIN
               Psi.Re.put(i,Psi.Re.value(i)*1/Sqrt(m));
               Psi.Im.put(i,Psi.Im.value(i)*1/Sqrt(m));
            END;
   END;

FUNCTION Mean(x: DVector; psi: CDVector): Real;

   VAR
      i: Integer;
      value, sum: Real;

   BEGIN
      Sum := 0;
      FOR i := 1 TO psi.Length DO
         BEGIN
            value := Sqr(Psi.RePart(i)) + Sqr(Psi.ImPart(i));
            Sum := Sum + x.value(i)*value;
         END;
      Mean := Sum*(x.value(2)-x.value(1));
   END;

FUNCTION StandardDeviation(x: DVector; psi: CDVector): Real;

   VAR
      i: Integer;
      value, average, sum: Real;

   BEGIN
      Average := Mean(x, psi);
      sum := 0;
      FOR i := 1 TO psi.Length DO
         BEGIN
            value := Sqr(Psi.RePart(i)) + Sqr(Psi.ImPart(i));
            sum := sum + Sqr(x.value(i)-average)*value;
         END;
         StandardDeviation := Sqrt(sum*(x.value(2)-x.value(1)));
   END;
     


PROCEDURE Change_Wave(VAR ReDo: Boolean);

   VAR
      xcenter,xwidth, pcenter, pwidth, old_xwidth, old_pwidth: Real;
      {Psi_Function, Phi_Function: FunctionXYZ;}
      i: Integer;
      x,y,z,t: Char;
      InputString: String;
      Change_Wave_Again: Boolean;
      helpScreen : HelpScrType;
   BEGIN
      DefineViewPort(13,0.12,0.50,0.136,0.176);
      DefineViewPort(14,0.62,1.00,0.12,0.186);
      ReDo := False;
      WITH Change_Wave_Parms DO
      BEGIN
         AboutParser(HelpScreen);
         SetHelpScreen(HelpScreen);
         AcceptScreen;
         IF (NOT Canceled) THEN
            BEGIN
               xcenter := GetNumber(4);
               pcenter := GetNumber(5);
               CASE GetRadioButton('2') OF
                  1: BEGIN
                        xwidth := GetNumber(8);
                        PsiWidthKnown := True;
                     END;
                  2: BEGIN
                        pwidth := GetNumber(8);
                        PsiWidthKnown := False;
                     END;
               END;
               IF GetRadioButton('2') = 1 THEN
                  BEGIN
                     PsiType := GetRadioButton('1');
                     PsiTypeKnown := True;
                  END
               ELSE
                  BEGIN
                     PhiType := GetRadioButton('1');
                     PsiTypeKnown := False;
                  END;

         xcenter_String := NumStr(xcenter,4,2);
         pcenter_String := NumStr(pcenter,4,2);
         IF PsiTypeKnown THEN
            BEGIN
               CASE Trunc(psitype) OF
                  1: BEGIN
                        Psi_function := 1;
                        Phi_function := 1;
                        IF PsiWidthKnown THEN pwidth := 1/(2*xwidth)
                           ELSE xwidth := 1/(2*pwidth);
                        sigmax_String := NumStr(xwidth,4,2);
                        sigmap_String := NumStr(pwidth,4,2);
                        Psi_Function_String := 'Gaussian';
                        Phi_Function_String := 'Gaussian';
                        If NOT QMInterface.Sliders.numUsed[3] THEN
                            QMInterface.Sliders.Create(3,-1.60206,1.30103,
                              0,0.05,0.07,0.5,2,'.025','20','Delta x',False,2);
                        If NOT QMInterface.Sliders.numUsed[4] THEN
                            QMInterface.Sliders.Create(4,-1.60206,1.30103,-0.30103,
                              0.55,0.07,1.0,2,'.025','20','Delta p',False,2);
                        QMInterface.Sliders.drawall;
                     END;{1}
                  2: BEGIN
                         Psi_function := 2;
                        Phi_function := 12;
                        pwidth := 1/xwidth;
                        sigmax_String := NumStr(xwidth/Sqrt(3),4,2);
                        sigmap_String := 'infinite';
                        Psi_Function_String := 'Rectangle';
                        Phi_Function_String := 'FT Rectangle';
                        QMInterface.Sliders.erase(4,0);
                        QMInterface.Sliders.delete(4);
                        If NOT QMInterface.Sliders.numUsed[3] THEN
                            QMInterface.Sliders.Create(3,-1.60206,1.30103,
                              0,0.05,0.07,0.5,2,'.025','20','Delta x',False,2);
                        QMInterface.Sliders.drawall;
                        DefineViewPort(12,0.62,1.00,0.06,0.106);
                        SelectViewPort(12);
                        ClearViewPort;
                        SetColor(15);
                        Print(1,1,'Delta p is infinite');
                     END;{2}
                  3: BEGIN
                        Psi_function := 3;
                        Phi_function := 13;
                        IF PsiWidthKnown THEN pwidth := 1/xwidth
                           ELSE xwidth := 1/pwidth;
                        sigmax_String := NumStr(xwidth/Sqrt(10),4,2);
                        sigmap_String := NumStr(Sqrt(3)*pwidth,4,2);
                        Psi_Function_String := 'Triangle';
                        Phi_Function_String := 'FT Triangle';
                        QMInterface.Sliders.erase(4,0);
                        QMInterface.Sliders.delete(4);
                        If NOT QMInterface.Sliders.numUsed[3] THEN
                            QMInterface.Sliders.Create(3,-1.60206,1.30103,
                              0,0.05,0.07,0.5,2,'.025','20','Delta x',False,2);
                        QMInterface.Sliders.drawall;
                        DefineViewPort(12,0.62,1.00,0.06,0.106);
                        SelectViewPort(12);
                        ClearViewPort;
                        SetColor(15);
                        Print(4,1,'Delta p = ' + sigmap_String);
                     END;{3}
                  4: BEGIN
                        {WITH User_Function_Screen DO}
                           {BEGIN                      }
                              {AcceptScreen;}
                              x := 'x';
                              IF (NOT Parser.parse(x,y,z,t,GetString(10))) THEN
                                 BEGIN
                                    announce('Error: Parser could not parse function');
                                    AboutParser(ParserHelp);
                                    Help(ParserHelp);
                                    ReDo := True;
                                 END
                              ELSE
                                 BEGIN
                                    Psi_function := 4;
                                    Phi_function := 14;
                                    Evaluate(Psi, Psi_Function, xwidth, xcenter, pcenter);
                                    Normalize(xvec,psi);
                                    FFT(Psi,FFTOut);
                                    Evaluate(Phi, Phi_Function, xwidth, xcenter, pcenter);
                                    Normalize(x2vec,phi);
                                    xcenter := Mean(xvec,psi);
                                    pcenter := Mean(x2vec,Phi);
                                    xwidth := StandardDeviation(xvec,psi);
                                    pwidth := StandardDeviation(x2vec,phi);
                                    QMInterface.Sliders.erase(1,0);
                                    QMInterface.Sliders.delete(1);
                                    QMInterface.Sliders.erase(2,0);
                                    QMInterface.Sliders.delete(2);
                                    QMInterface.Sliders.erase(3,0);
                                    QMInterface.Sliders.delete(3);
                                    QMInterface.Sliders.erase(4,0);
                                    QMInterface.Sliders.delete(4);
                                    DefineViewPort(13,0.12,0.50,0.136,0.176);
                                    SelectViewPort(13);
                                    SetColor(15);
                                    ClearViewPort;
                                    xcenter_String := NumStr(xcenter,4,2);
                                    Print(4,1,'Mean x = ' + xcenter_String);
                                    DefineViewPort(14,0.62,1.00,0.136,0.176);
                                    SelectViewPort(14);
                                    ClearViewPort;
                                    pcenter_String := NumStr(pcenter,4,2);
                                    Print(4,1,'Mean p = ' + pcenter_String);
                                    DefineViewPort(12,0.12,0.50,0.06,0.106);
                                    SelectViewPort(12);
                                    ClearViewPort;
                                    sigmax_String := NumStr(xwidth,4,2);
                                    Print(4,1,'Delta x = ' + sigmax_String);
                                    DefineViewPort(12,0.62,1.00,0.06,0.106);
                                    SelectViewPort(12);
                                    ClearViewPort;
                                    sigmap_String := NumStr(pwidth,4,2);
                                    Print(4,1,'Delta p = ' + sigmap_String);
                                 END;{ELSE}
                        {END;WITH}
                     END{4}
                  ELSE xwidth := old_xwidth * old_pwidth / pwidth ;
               END;{CASE}
               IF ReDo = False THEN
               BEGIN
               old_xwidth := xwidth;
               old_pwidth := pwidth;
               Evaluate(Psi, Psi_Function, xwidth, xcenter, pcenter);
               Evaluate(Phi, Phi_function, pwidth, pcenter, -xcenter);
               IF psi_function = 4 THEN
                  BEGIN
                     Normalize(xvec,psi);
                     Normalize(x2vec,phi);
                  END;
               IF phi_function = 4 THEN
                  BEGIN
                     Normalize(x2vec,psi);
                     Normalize(xvec,phi);
                  END;
               Psi.AbsSqr(Psi_Squared);
               Phi.AbsSqr(Phi_Squared);
               Plot_Functions;
               END;{THEN}
            END{THEN}
         ELSE
            BEGIN
               CASE Trunc(phitype) OF
                  1: BEGIN
                        Psi_function :=  1;
                        Phi_function := 1;
                        IF NOT PsiWidthKnown THEN xwidth := 1/(2*pwidth)
                           ELSE pwidth := 1/(2*xwidth);
                        sigmap_String := NumStr(pwidth,4,2);
                        sigmax_String := NumStr(xwidth,4,2);
                        Psi_Function_String := 'Gaussian';
                        Phi_Function_String := 'Gaussian';
                        If NOT QMInterface.Sliders.numUsed[3] THEN
                            QMInterface.Sliders.Create(3,-1.60206,1.30103,
                              0,0.05,0.07,0.5,2,'.025','20','Delta x',False,2);
                        If NOT QMInterface.Sliders.numUsed[4] THEN
                            QMInterface.Sliders.Create(4,-1.60206,1.30103,-0.30103,
                              0.55,0.07,1.0,2,'.025','20','Delta p',False,2);
                        QMInterface.Sliders.drawall;
                     END;{1}
                  2: BEGIN
                        Psi_function := 12;
                        Phi_function := 2;
                        xwidth := 1/pwidth;
                        sigmap_String := NumStr(pwidth/Sqrt(3),4,2);
                        sigmax_String := 'inf';
                        Psi_Function_String := 'FT Rectangle';
                        Phi_Function_String := 'Rectangle';
                        QMInterface.Sliders.erase(3,0);
                        QMInterface.Sliders.delete(3);
                        If NOT QMInterface.Sliders.numUsed[4] THEN
                            QMInterface.Sliders.Create(4,-1.60206,1.30103,-0.30103,
                              0.55,0.07,1.0,2,'.025','20','Delta p',False,2);
                        QMInterface.Sliders.drawall;
                        DefineViewPort(12,0.12,0.50,0.06,0.106);
                        SelectViewPort(12);
                        ClearViewPort;
                        SetColor(15);
                        Print(1,1,'Delta x is infinite');
                     END;{2}
                  3: BEGIN
                        Psi_function := 13;
                        Phi_function := 3;
                        IF NOT PsiWidthKnown THEN xwidth := 1/pwidth
                           ELSE pwidth := 1/xwidth;
                        sigmap_String := NumStr(pwidth/Sqrt(10),4,2);
                        sigmax_String := NumStr(Sqrt(3)*xwidth,4,2);
                        Psi_Function_String := 'FT Triangle';
                        Phi_Function_String := 'Triangle';
                        QMInterface.Sliders.erase(3,0);
                        QMInterface.Sliders.delete(3);
                        If NOT QMInterface.Sliders.numUsed[4] THEN
                            QMInterface.Sliders.Create(4,-1.60206,1.30103,-0.30103,
                              0.55,0.07,1.0,2,'.025','20','Delta p',False,2);
                        QMInterface.Sliders.drawall;
                        DefineViewPort(12,0.12,0.50,0.06,0.106);
                        SelectViewPort(12);
                        ClearViewPort;
                        SetColor(15);
                        Print(4,1,'Delta x = ' + sigmax_String);
                     END;{3}
                  4: BEGIN
                        x := 'p';
                        IF (NOT Parser.parse(x,y,z,t,GetString(10))) THEN
                           BEGIN
                              announce('Error: Parser could not parse function');
                              AboutParser(ParserHelp);
                              Help(ParserHelp);
                              ReDo := True;
                           END
                        ELSE
                           BEGIN
                              Psi_function := 14;
                              Phi_function := 4;
                              Evaluate(Phi, Phi_Function, xwidth, xcenter, pcenter);
                              Normalize(xvec,phi);
                              FFT(Phi,FFTOut);
                              Evaluate(Psi, Psi_Function, xwidth, xcenter, pcenter);
                              Normalize(x2vec,psi);
                              xcenter := Mean(x2vec,psi);
                              pcenter := Mean(xvec,Phi);
                              xwidth := StandardDeviation(x2vec,psi);
                              pwidth := StandardDeviation(xvec,phi);
                              QMInterface.Sliders.erase(1,0);
                              QMInterface.Sliders.delete(1);
                              QMInterface.Sliders.erase(2,0);
                              QMInterface.Sliders.delete(2);
                              QMInterface.Sliders.erase(3,0);
                              QMInterface.Sliders.delete(3);
                              QMInterface.Sliders.erase(4,0);
                              QMInterface.Sliders.delete(4);
                              {If NOT QMInterface.Sliders.numUsed[4] THEN
                                 BEGIN
                                    QMInterface.Sliders.erase(1,0);
                                    QMInterface.Sliders.delete(1);
                                 END;
                              If NOT QMInterface.Sliders.numUsed[4] THEN
                                 BEGIN
                                    QMInterface.Sliders.erase(2,0);
                                    QMInterface.Sliders.delete(2);
                                 END;
                              If NOT QMInterface.Sliders.numUsed[4] THEN
                                 BEGIN
                                    QMInterface.Sliders.erase(3,0);
                                    QMInterface.Sliders.delete(3);
                                 END;
                              If NOT QMInterface.Sliders.numUsed[4] THEN
                                 BEGIN
                                    QMInterface.Sliders.erase(4,0);
                                    QMInterface.Sliders.delete(4);
                                 END; }
                              DefineViewPort(13,0.12,0.50,0.106,0.146);
                              DefineViewPort(13,0.12,0.50,0.136,0.176);
                              SelectViewPort(13);
                              SetColor(15);
                              ClearViewPort;
                              xcenter_String := NumStr(xcenter,4,2);
                              Print(4,1,'Mean x = ' + xcenter_String);
                              DefineViewPort(14,0.62,1.00,0.136,0.176);
                              {DefineViewPort(14,0.62,1.00,0.12,0.186);}
                              SelectViewPort(14);
                              ClearViewPort;
                              pcenter_String := NumStr(pcenter,4,2);
                              Print(4,1,'Mean p = ' + pcenter_String);
                              DefineViewPort(12,0.12,0.50,0.06,0.106);
                              SelectViewPort(12);
                              ClearViewPort;
                              sigmax_String := NumStr(xwidth,4,2);
                              Print(4,1,'Delta x = ' + sigmax_String);
                              DefineViewPort(12,0.62,1.00,0.06,0.106);
                              SelectViewPort(12);
                              ClearViewPort;
                              sigmap_String := NumStr(pwidth,4,2);
                              Print(4,1,'Delta p = ' + sigmap_String);
                           END;{ELSE}
                     END{4}
                  ELSE pwidth := old_pwidth * old_xwidth / xwidth ;
               END;{CASE}
               IF ReDo = False THEN
                  BEGIN
                     Evaluate(Psi, Psi_function, xwidth, xcenter, pcenter);
                     Evaluate(Phi, Phi_function, pwidth, pcenter, -xcenter);
                     IF psi_function = 4 THEN
                        BEGIN
                           Normalize(xvec,psi);
                           Normalize(x2vec,phi);
                        END;
                     IF phi_function = 4 THEN
                        BEGIN
                           Normalize(x2vec,psi);
                           Normalize(xvec,phi);
                        END;
                     Psi.AbsSqr(Psi_squared);
                     Phi.AbsSqr(Phi_Squared);
                     Plot_Functions;
                  END;
            END;{ELSE}
            IF ReDo = False THEN
               BEGIN
                  QMInterface.Sliders.Reset(1,xcenter);
                  QMInterface.Sliders.Reset(2,pcenter);
                  IF QMInterface.Sliders.numUsed[3] THEN
                     QMInterface.Sliders.Reset(3,log(10,xwidth));
                  IF QMInterface.Sliders.numUsed[4] THEN
                  QMInterface.Sliders.Reset(4,log(10,pwidth));
                  FOR i := 1 TO 4 DO OldSlider[i] := QMInterface.SLiders.value(i);
               END;
      IF ((psiType <> 4) AND (PhiType <> 4)) THEN
      BEGIN
      SelectViewPort(13);
      ClearViewPort;
      SelectViewPort(14);
      ClearViewPort;
      If NOT QMInterface.Sliders.numUsed[1] THEN
         QMInterface.Sliders.Create(1,-4,4,0,0.05,0.14,0.5,2,'-4.0','4.0',
                                                          'Mean x',False,1);
      If NOT QMInterface.Sliders.numUsed[2] THEN
         QMInterface.Sliders.Create(2,-4,4,0,0.55,0.14,1.00,2,'-4.0','4.0',
                                                          'Mean p',False,1);
      QMInterface.Sliders.DrawAll;
      END;{THEN}
      END;{THEN}
      END;{WITH}
   END;



PROCEDURE Toggle_Components(VAR Components:Boolean);

   BEGIN
      Components := NOT(Components);
      IF Components THEN QMInterface.HotKeys.Key[4] := 'F6-Hide Real'
      ELSE QMInterface.HotKeys.Key[4] := 'F6-Show Real';
      QMInterface.HotKeys.Display;
      IF Components = True THEN DefineScale(7,-5,5,-3,4)
      ELSE DefineScale(7,-5,5,-0,4);
      SelectViewPort(9);
      ClearViewPort;
      Plot_Functions;
   END;

PROCEDURE CreateSliders;

   BEGIN
      WITH QMInterface DO
         BEGIN
            Sliders.Init;
            Sliders.Create(1,-4,4,0,0.05,0.14,0.5,2,'-4.0','4.0','Mean x',False,1);
            Sliders.Create(2,-4,4,0,0.55,0.14,1.0,2,'-4.0','4.0','Mean p',False,1);
            Sliders.Create(3,-1.60206,1.30103,0,0.05,0.07,0.5,2,'.025','20','Delta x',False,2);
            Sliders.Create(4,-1.60206,1.30103,-0.30103,0.55,0.07,1.0,2,'.025','20','Delta p',False,2);
            Sliders.DrawAll;
         END;
    END;

PROCEDURE Process;

   BEGIN
      WITH Free_Time_Ind_Disp DO
      WITH Message DO
      BEGIN
         Open;
         Parser.init;
         HelpScreenInit;
         DefineViewPort(9,0.0,1.00,0.20,0.89);
         DefineViewPort(10,0.0,1.0,0.85,0.89);
         QMInterface.Menu.RowActivate(3,2,False);
         QMInterface.Menu.RowActivate(3,3,False);
         HelpNumber := 1;
         CreateSliders;
         quit := False;
         done := False;
         xcenter := 0;
         xwidth := 1;
         pcenter := 0;
         pwidth := 0.5;
         OldSlider[1] := xcenter;
         OldSlider[2] := pcenter;
         OldSlider[3] := xwidth;
         OldSlider[4] := Pwidth;
         xcenter_String := '0.00';
         pcenter_String := '0.00';
         sigmax_String := '1.00';
         sigmap_String := '0.50';
         Psi_Function := 1;
         Phi_Function := 1;
         Psi_Function_String := 'Gaussian';
         Phi_Function_String := 'Gaussian';
         Plot_Components := False;
         Define_Input_Screens;
         DefineScale(7,-5,5,0,2);
         DefineScale(8,-5,5,0,2);
         Evaluate(Psi, 1, xwidth, xcenter, pcenter);
         Psi.AbsSqr(Psi_Squared);
         Evaluate(Phi, 1, pwidth, pcenter, -xcenter);
         Phi.AbsSqr(Phi_Squared);
         Plot_Functions;

         {Write_info;}
      END;
   END;


BEGIN
   {$IFDEF MAIN}
   {$ENDIF}
{   ClearMUPPETport;}
   Initialize_Vectors;
   Initialize_Display_Object;
   QMInterface.SlidersExist := True;
   QMInterface.HotKeys.Key[4] := 'F6-Show Real';
   QMInterface.HotKeys.Active[2] := False;
   QMInterface.HotKeys.Active[3] := False;
   QMInterface.HotKeys.Active[5] := False;
   QMInterface.HotKeys.Display;
   Process;
   WITH QMInterface DO
   WITH Message DO
   WHILE (NOT Quit) DO
      BEGIN
         IF QueryEvent(Message) THEN
            IF Change <> 0 THEN
               BEGIN
                  CASE Change OF
                     1: REPEAT Change_Wave(ReDo) UNTIL Redo = False;
                     10: Help(CurrentHelpScreen);
                     14: Toggle_Components(Plot_Components);
                     20: ProcessSlider;
                  END;
                  Change := 0;
               END;
            {IF Help THEN CallHelp(HelpNumber);
            Help := False;}
      END;
   QMInterface.Menu.RowActivate(3,2,True);
   QMInterface.Menu.RowActivate(3,3,True);
   QMInterface.Sliders.Done;
   Change_Wave_Parms.Done;
   User_Function_Screen.Done;
   QMInterface.HotKeys.Active[2] := True;
   QMInterface.HotKeys.Active[3] := True;
   QMInterface.HotKeys.Active[5] := True;
   QMInterface.HotKeys.Display;
   Free_Vectors;

{   Free_Time_Ind_Disp.Menu.Done;}
   {$IFDEF MAIN}
   CUPSdone;
   {$ENDIF}
{$IFNDEF MAIN}
END;
{$ENDIF}

END.  { End of file FPTI.PAS }


