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

{ Start of file QM_BARR.PAS }

{ ----------------------------------------------------------------------
   Program Title:    Barriers
   Author:           Douglas E. Brandt
   Version:          0.11
   Program Summary:  This is one section of the CUPS Modern Physics
                     One Dimensional Wave Mechanics Program.  This section
                     shows Shrodinger waves incident on potential barriers.
                     It displays the potential as a function of position,
                     the reflection and transmission coefficients of
                     energy eigenstates incident on the barrier, and can display
                     the time evolution of the the energy eigenstates or the
                     time evolution of a wave packet incident on the barrier.
                     The current version only allows a single barrier, but the
                     final version will allow a general piecewise constant
                     barrier.
   Library Files:    CRT,DOS,GRAPH,CUPSmupp,CUPS,CUPSGRPH,CUPSFUNC,CUPSPROC,
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
{$M 64000,64000,640000}
UNIT QM_Barr;

INTERFACE

{$ENDIF}

USES Crt, Dos, Graph, CUPSmupp, CUPS, CUPSgrph, CUPSfunc,CUPSproc, CUPSgui,
        Objects, QM_Ifac;

{$IFNDEF MAIN}

PROCEDURE Potential_Barriers(QMInterface: TInterface; VAR Message: MessageRec);

IMPLEMENTATION

PROCEDURE Potential_Barriers(QMInterface: TInterface; VAR Message: MessageRec);

{$ENDIF}

   TYPE
      Function3 = FUNCTION(x,y,z: Real): Real;
      Wave_Record = RECORD
         A: ARRAY [1..5] OF Complex;
         B: ARRAY [1..5] OF Complex;
         Reflect,Transmit: Real;
         PsiType: ARRAY [1..5] OF Integer;
         absk: ARRAY [1..5] OF Real;
      END;
      PotentialDef = RECORD
         Width: ARRAY [1..5] OF Real;
         Potential: ARRAY [1..5] OF Real;
         Regions: Integer;
      END;

   CONST
      Length = 200;
      Stand_alone = True;
      Num_Points = 12;
      Num_Components = 25;
      PsiSquaredColor = 13;
      RealPsiColor = 11;
      ImagPsiColor = 10;

   VAR
      Psi_Squared, Real_Psi, Imag_Psi, Last_Real_Psi: DVector;
      Old_Real_Psi,Old_Imag_Psi,Old_Psi_Squared, ReflectVect, TransmitVect: DVector;
      Real_E_Psi, Imag_E_Psi, EVect: DVector;
      Psi_0: ARRAY [0..Num_Components] OF CDVector;
      m, LastE: Real;
      x: DVector;
      i, done, return: Integer;
      Barrier_Disp: Base_Display;
      Change_Particle_Parameter_Screen, Change_Barrier_Screen,
            Time_Step_Screen,IntegrateScreen: TInputScreen;
      Vi,height, width: Real;
      pmean,xwidth,deltax,deltat,t: Real;
      Packet,Components, LastPacket: Boolean;
      _i: Complex;
      Matrix: DMatrix;
      SolutionVector,EPacket, LastEPacket: Dvector;
      Num_Steps: Integer;
      knum: Byte;
      Psi: CDVector;
      {E: ARRAY [1..40] OF Real;}
      E,L: Real;
      One, zd: Complex;
      CurrentBarrier: PotentialDef;
      CurrentHelpScreen: HelpScrType;
      {$IFDEF MAIN}
      next: Integer;
      {$ENDIF}
{R-}
{PROCEDURE PlotDVectors(x, y : DVector; ibeg, iend: Integer);
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
END;}
{R+}
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

PROCEDURE Initialize_Vectors;

   VAR
      i: Integer;

   BEGIN
      Psi_Squared.init(Length);
      Real_Psi.init(Length);
      Imag_Psi.init(Length);
      Old_Real_Psi.init(Length);
      Old_Imag_Psi.init(Length);
      Old_Psi_Squared.init(Length);
      Real_E_Psi.init(Length);
      Imag_E_Psi.init(Length);
      ReflectVect.init(100);
      TransmitVect.init(100);
      EPacket.init(100);
      LastEPacket.init(100);
      Evect.init(100);
      FOR i := 1 TO 100 DO EVect.put(i,i/10.0);
      x.init(Length);
      FOR i := 1 TO Num_Components DO Psi_0[i].init(Length);
      FOR i := 1 TO Length DO
         BEGIN
            x.put(i, 4 * i/Length - 2);
            x.put(i, 8 * i/Length - 4);
         END;
      
   END;


PROCEDURE Free_Vectors;

   VAR
      i: Integer;

   BEGIN
      Psi_Squared.Free;
      Real_Psi.Free;
      Imag_Psi.Free;
      Old_Real_Psi.Free;
      Old_Imag_Psi.Free;
      Old_Psi_Squared.Free;
      Real_E_Psi.Free;
      Imag_E_Psi.Free;
      ReflectVect.free;
      TransmitVect.Free;
      EPacket.Free;
      LastEPacket.Free;
      Evect.Free;
      x.Free;
      FOR i := 1 TO Num_Components DO Psi_0[i].Free;

   END;



PROCEDURE Initialize_Display_Object;

   BEGIN
      WITH Barrier_Disp DO
         BEGIN
            Title := 'Barriers';
            Window_num := 1;
            Window_corner[1,1].x := 0.06;
            Window_corner[1,1].y := 0.12;
            Window_corner[1,2].x := 0.58;
            Window_corner[1,2].y := 0.81;
            DefineViewPort(2,0.0,0.58,0.06,0.81);
            {Window_corner[2,1].x := 0.05;
            Window_corner[2,1].y := 0.49;
            Window_corner[2,2].x := 0.65;
            Window_corner[2,2].y := 0.81;}
            DefineViewPort(3,0.65,1.00,0.30,0.81);
            DefineViewPort(4,0.65,1.00,0.20,0.83);
            {Window_corner[3,1].x := 0.75;
            Window_corner[3,1].y := 0.49;
            Window_corner[3,2].x := 0.95;
            Window_corner[3,2].y := 0.81;}
            Init := True;
         END;
   END;

PROCEDURE HelpScreenInit;

   BEGIN
         currentHelpScreen[01] := '               HELP FOR BARRIERS                  ';
         currentHelpScreen[02] := '                                                  ';
         currentHelpScreen[03] := '    Define a potential barrier.  Select "Barrier  ';
         currentHelpScreen[04] := ' Parameters" from the "Parameters" menu.  Select ';
         currentHelpScreen[05] := ' the number of constant potential regions with  ';
         currentHelpScreen[06] := ' radio buttons and define the widths and ';
         currentHelpScreen[07] := ' potentials of those regions.  The program will ';
         currentHelpScreen[08] := ' calculate the reflection and transmission co-';
         currentHelpScreen[09] := ' efficients for the range of energies shown and ';
         currentHelpScreen[10] := ' display it on the graph. ';
         currentHelpScreen[11] := '   ';
         currentHelpScreen[12] := '    The wavefunction displayed on the left can be ';
         currentHelpScreen[13] := ' chosen to be an energy eigenfunction or a wave-';
         currentHelpScreen[14] := ' packet.  Select "Wave Function Parameters" from  ';
         currentHelpScreen[15] := ' the "Parameters" menu and make appropriate';
         currentHelpScreen[16] := ' choices for the wavefunction you wish to display.  ';
         currentHelpScreen[17] := ' Once a wavefunction has been chosen, the energy  ';
         currentHelpScreen[18] := ' of the energy eigenfunction or the mean energy ';
         currentHelpScreen[19] := ' of the wavepacket can be adjusted using the ';
         currentHelpScreen[20] := ' "Energy" slider on the display.  The wavepacket';
         currentHelpScreen[21] := ' energy amplitude is displayed in gray on the';
         currentHelpScreen[22] := ' Reflection and Transmission graph.';
         currentHelpScreen[23] := '                                                 ';
         currentHelpScreen[24] := '    Press any Key or Click Mouse to continue.     ';
         currentHelpScreen[25] := '                                                  ';

   END;


PROCEDURE Define_Input_Screens;

   BEGIN
      WITH Change_Barrier_Screen DO
      BEGIN
         init;
         DefineInputPort(0.1,0.9,0.07,0.85);
         Loadline('                              ');
         Loadline('                         The Barrier                              ');
         Loadline('   If the number of regions selected is n < 5, then the        ');
         Loadline('   program disregards the numbers for the fields for regions   ');
         Loadline('   greater than n.  The potentail in region I is always zero.  ');
         Loadline('             ___________              ___________         ');
         Loadline('            |           |            |           |          ');
         Loadline('   Region I | Region II | Region III | Region IV | Region V ');
         Loadline('            |           |____________|           |__________');
         Loadline(' V=0________|                                  ');
         Loadline('                                                          ');
         Loadline('         Number of Regions  #1 2   #1 3   #1 4   #1 5     ');
         Loadline('Potential in Region II  = {    }  Width of Region II  =  {    }');
         Loadline('Potential in Region III = {    }  Width of Region III =  {    }');
         Loadline('Potential in Region IV  = {    }  Width of Region IV  =  {    }');
         Loadline('Potential in Region V   = {    }                               ');
         Loadline('                              ');
         Loadline('                     [  Ok  ]   [Cancel]                        ');
         SetNumber(5,2.0);
         SetNumber(6,0.3);
         SetNumber(7,0);
      END;
      WITH Time_Step_Screen DO
      BEGIN
         init;
         DefineInputPort(0.5,0.9,0.5,0.62);
         Loadline('                              ');
         Loadline('     Time step =  {    }      ');
         Loadline('                              ');
         Loadline('     [  Ok  ]   [Cancel]       ');
         SetNumber(1,0.01);
      END;
      WITH Change_Particle_Parameter_Screen DO
      BEGIN
         init;
         DefineInputPort(0.1,0.9,0.22,0.78);
         Loadline('                    Particle Parameters                ');
         Loadline('                                ');
         Loadline('      Display an energy eigenwave or a wavepacket?                          ');
         Loadline('                 #1 Eigenwave    #1 Wavepacket           ');
         Loadline('                                ');
         Loadline('                 Mass of Particle =  {    }   ');
         Loadline('                                ');
         Loadline(' The energy of the particle is the energy of the eigenwave ');
         Loadline(' or the component with the mean wavevector of the wavepacket. ');
         Loadline('                 Energy of Particle = {    }                ');
         Loadline('                                ');
         Loadline('                    [  Ok  ]   [Cancel]        ');
         SetNumber(3,50);
         SetNumber(4,1.0);
      END;
      WITH IntegrateScreen DO
      BEGIN
         init;
         DefineInputPort(0.60,0.9,0.15,0.6);
         Loadline('');
         Loadline(' Lower limit = {    }');
         Loadline(' Upper limit = {    }');
         Loadline(' Integrand:  ');
         Loadline('#1 Probability Density');
         Loadline('#1 x*Prob. Dens.      ');
         Loadline('#1 x^2*Prob. Dens.    ');
         Loadline('#1 V(x)*Prob. Dens.   ');
         Loadline('                       ');
         Loadline('   [  Ok  ] [Cancel]   ');
         SetNumber(1,0);
         SetNumber(2,0);
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

PROCEDURE Draw_Barrier;

   VAR
      i: Integer;
      xn,Vmax, Vmin: Real;

   BEGIN
      VMax := 0;
      VMin := 0;
      WITH CurrentBarrier DO
      FOR i := 1 TO Regions DO
         BEGIN
            IF Potential[i] > Vmax THEN VMax := Potential[i];
            IF Potential[i] < Vmin THEN VMin := Potential[i];
         END;
      SelectViewPort(1);
      Autoxscaling(19,x,Vmin,Vmax);
      SelectScale(19);
      SetColor(yellow);
   {   Print(38,1,'Potential');}
      PlotLine(x.value(1),0,0,0);
      xn := 0;
      WITH CurrentBarrier DO
      BEGIN
         FOR i := 2 TO Regions DO
            BEGIN
               xn := xn + Width[i];
               PlotLine(xn-Width[i],Potential[i-1],xn-Width[i],Potential[i]);
               PlotLine(xn-Width[i],Potential[i],xn,Potential[i]);
            END;
         PlotLine(xn,Potential[Regions],x.value(length),Potential[Regions]);
      END;
   END; 

PROCEDURE DrawEDistribution(packet: Boolean);

   BEGIN
      SelectViewPort(3);
      SelectScale(3);
      SetColor(GraphBackColor);
      IF LastPacket THEN PlotDVectors(EVect,LastEPacket,1,100)
      ELSE PlotLine(LastE,-0.04,LastE,1.05);
      SetColor(12);
      PlotDVectors(EVect,ReflectVect,1,100);
      SetColor(14);
      PlotDVectors(EVect,TransmitVect,1,100);
      SetColor(7);
      IF Packet THEN PlotDVectors(EVect,EPacket,1,100)
      ELSE PlotLine(E,-0.04,E,1.05);
   END;


PROCEDURE Plot_functions(Real_Psi,Imag_Psi,Psi_Squared: DVector);

   BEGIN
      SelectViewPort(1);
      SetColor(1);
      SelectScale(7);
      SetColor(GraphBackColor);
      HideCursor;
      IF Packet THEN PlotDVectors(x,Old_Psi_Squared, 1, Length);
      SetColor(PsiSquaredColor);
      PlotDVectors(x, Psi_Squared, 1, length);
      ShowCursor;
      Old_Psi_Squared.Equate(1,Psi_Squared);
      IF Components = True THEN
         BEGIN
            SetColor(GraphBackColor);
            HideCursor;
            PlotDVectors(x,Old_Real_Psi,1,Length);
            PlotDVectors(x,Old_Imag_Psi,1,Length);
            ShowCursor;
            SetColor(GraphBackColor);
            HideCursor;
            SetColor(RealPsiColor);
            PlotDVectors(x, Real_Psi, 1, length);
            SetColor(ImagPsiColor);
            PlotDVectors(x, Imag_Psi, 1, length);
            ShowCursor;
            Old_Imag_Psi.Equate(1,Imag_Psi);
            Old_Real_Psi.Equate(1,Real_Psi);
         END;
         DefineScale(12,0,0.88136,0,1);
         SelectScale(12);
         SetColor(PsiSquaredColor);
         PsiChar(0.04,0.955,0.024,0.022);
         PlotLine(0.022,0.955,0.022,0.979);
         PlotLine(0.055,0.955,0.055,0.979);
         PlotLine(0.065,0.985,0.074,0.985);
         PlotLine(0.074,0.985,0.074,0.977);
         PlotLine(0.074,0.977,0.065,0.977);
         PlotLine(0.065,0.977,0.065,0.968);
         PlotLine(0.065,0.968,0.074,0.968);
         IF Components THEN
            BEGIN
               SetColor(RealPsiColor);
               PsiChar(0.149,0.955,0.024,0.022);
               Print(5,1,'Re');
               SetColor(ImagPsiColor);
               PsiChar(0.23,0.955,0.024,0.022);
               Print(9,1,'Im');
            END;
         SetColor(14);
         Print(45,1,'V');
      SetColor(4);
      ShowCursor;
   END;

FUNCTION xindex(z: Real): Integer;

   VAR
      i: Integer;

   BEGIN
      i := 1;
      WHILE ((x.value(i) < z) AND (i < Length)) DO i := i + 1;
      xindex := i;
   END;

PROCEDURE Integrate;

   VAR
      UpperIndex, LowerIndex, OldBack,sign, IntegralType: Integer;
      LowerLimit, UpperLimit, Integral,deltax: Real;
      Doone, ImageSaved: Boolean;
      ImageSave :TImage;
      IntegrandString: String;
      Integrand,xsquared: DVector;

   PROCEDURE PrintIntegral;

      VAR
         CharUpper, CharLower: Integer;

      BEGIN
         HideCursor;
         SetColor(15);
         GraphBackColor := Red;
         OpenViewPort(12);
         SelectScale(13);
         IF UpperLimit <> 0 THEN
            CharUpper := Trunc(log(10,Abs(UpperLimit)))+1
         ELSE CharUpper := 1;
         IF LowerLimit <> 0 THEN
            CharLower := Trunc(log(10,abs(LowerLimit)))+1
         ELSE CharLower := 1;
         IF (sign = 1) THEN
         BEGIN
            Print(7,2,NumStr(UpperLimit,CharUpper,2));
            Print(5,4,NumStr(LowerLimit,CharLower,2));
         END
         ELSE
         BEGIN
            Print(5,4,NumStr(UpperLimit,CharUpper,2));
            Print(7,2,NumStr(LowerLimit,CharLower,2));
         END;
         Print(15,3,'dx = '+ NumStr(Integral,4,2));
         Print(7,3,IntegrandString);
         PlotLine(0.15,0.65,0.20,0.85);
         PlotLine(0.20,0.85,0.21,0.86);
         PlotLine(0.21,0.86,0.22,0.86);
         PlotLine(0.22,0.86,0.23,0.85);
         PlotLine(0.15,0.65,0.14,0.64);
         PlotLine(0.14,0.64,0.13,0.64);
         PlotLine(0.13,0.64,0.12,0.65);
         PsiChar(0.40,0.71,0.07,0.05);
         PlotLine(0.36,0.71,0.36,0.78);
         PlotLine(0.44,0.71,0.44,0.78);
         PlotLine(0.46,0.785,0.48,0.785);
         PlotLine(0.48,0.785,0.485,0.78);
         PlotLine(0.485,0.78,0.485,0.775);
         PlotLine(0.485,0.775,0.48,0.77);
         Plotline(0.48,0.77,0.47,0.77);
         PlotLine(0.47,0.77,0.46,0.76);
         PlotLine(0.46,0.76,0.46,0.75);
         PlotLine(0.46,0.75,0.48,0.75);
         ShowCursor;
      END;

   PROCEDURE MultVectors(VAR Product: DVector;Factor1,Factor2: DVector);

      VAR i: Integer;

      BEGIN
         FOR i := 1 TO Length DO
            Product.put(i, Factor1.value(i)*Factor2.value(i));
      END;

   PROCEDURE GetLimits;

      VAR
         Temp,i: Integer;
         psi2upper,psi2lower,temp2: Real;

      BEGIN
         Integrand.init(Length);
         xsquared.init(Length);
         WITH IntegrateScreen DO
            BEGIN
               Doone := False;
               Accept;
               IF (NOT Canceled) THEN
                  BEGIN
                     sign := 1;
                     deltax := x.value(2)-x.value(1);
                     GraphBackColor := MyBackColor;
                     HideCursor;
                     OpenViewPort(1);
                     {GraphBackColor := Red;
                     OpenViewPort(15);
                     SetColor(15);
                     Print(1,1,'t = '+Num2Str(t,6));
                     GraphBackColor := DarkGray;}
                     Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
                     Draw_Barrier;
                     ShowCursor;
                     LowerLimit := GetNumber(1);
                     UpperLimit := GetNumber(2);
                     LowerIndex := xindex(LowerLimit);
                     UpperIndex := xindex(UpperLimit);
                     IntegralType := GetRadioButton('1');
                     IF LowerLimit > UpperLimit THEN
                        BEGIN
                           Temp := LowerIndex;
                           LowerIndex := UpperIndex;
                           UpperIndex := Temp;
                           Sign := -1;
                           Temp2 := LowerLimit;
                           LowerLimit := UpperLimit;
                           UpperLimit := Temp2;
                        END;
                     CASE IntegralType OF
                        1:  BEGIN
                               Integrand.Equate(1,Psi_squared);
                               IntegrandString := '';
                            END;
                        2:  BEGIN
                               MultVectors(Integrand,Psi_Squared,x);
                               IntegrandString := '  x';
                            END;
                        3:  BEGIN
                               MultVectors(xsquared,x,x);
                               MultVectors(Integrand,Psi_Squared,xsquared);
                               IntegrandString := 'x^2';
                            END;
                        4:  BEGIN
                               Integrand.Fill(0);
                               IntegrandString := '  V';
                            END;
                     END;
{                     Integral := integral+(Psi_squared.value(lowerindex)+
                        Psi_squared.value(lowerindex-1))/2*(x.value(lowerindex)-lowerlimit)/deltax;
                     Integral := integral+(Psi_squared.value(upperindex)+
                        Psi_squared.value(upperindex-1))/2*(upperlimit-x.value(upperindex-1))/deltax;
                     Integral := integral*deltax;}
                     Integral := Integrand.Simpson(LowerIndex,UpperIndex-1);
                     IF LowerIndex = UpperIndex THEN Integral := 0;
                     Integral := Integral*deltax*sign;
                     PrintIntegral;
                     SelectViewPort(1);
                     SelectScale(7);
                     SetColor(red);
                     HideCursor;
                     psi2upper := (Psi_Squared.value(upperindex)-Psi_Squared.value(upperindex-1))/
                                    deltax*(upperlimit-x.value(upperindex-1))+psi_squared.value(upperindex-1);
                     Line(MapX(UpperLimit),MapY(0)+2,MapX(UpperLimit),MapY(Psi2upper));
                     PlotLine(upperlimit,psi2upper,x.value(upperindex-1),psi_squared.value(upperindex-1));
                     psi2lower := (Psi_Squared.value(lowerindex)-Psi_Squared.value(lowerindex+1))/
                                    deltax*(lowerlimit-x.value(lowerindex+1))+psi_squared.value(lowerindex+1);
                     Line(MapX(LowerLimit),MapY(0)+2,MapX(LowerLimit),MapY(Psi2lower));
                     PlotLine(lowerlimit,psi2lower,x.value(lowerindex),psi_squared.value(lowerindex));
                     Line(MapX(LowerLimit),MapY(0)+2,MapX(Upperlimit),MapY(0)+2);
                     PlotDVectors(x,psi_squared,lowerindex,upperindex-1);
                    { SetColor(Black);
                     PlotLine(x.value(lowerindex),0,x.value(lowerindex),psi_squared.value(lowerindex));
                     PlotLine(x.value(lowerindex-1),0,x.value(lowerindex-1),psi_squared.value(lowerindex-1));}

                     SetFillStyle(1,LightBlue);
                     FloodFill(MapX((x.value(lowerindex)+x.value(upperindex))/2),
                        MapY(0)+1,
                           red);
                     Line(MapX(LowerLimit),MapY(0)+1,MapX(Upperlimit),MapY(0)+1);
                     ShowCursor;
                  END
               ELSE
                  Doone := True;
            END;
         Integrand.Free;
         xsquared.Free;
      END;

   BEGIN
      DefineViewPort(12,0.58,0.92,0.13,0.80);
      SelectViewPort(12);
      DefineScale(13,0,1,-0.59523,1);
      SelectScale(13);
      ImageSave.Get(MapX(0),MapY(1),MapX(1),MapY(-0.59523));
      SetColor(15);
      GraphBackColor := Red;
      UpperLimit := 0;
      LowerLimit := 0;
      Integral := 0;
      IntegrandString := '';
      PrintIntegral;
      Doone := False;
      WHILE (Doone = False) DO GetLimits;
      SelectViewPort(12);
      Definescale(14,0,1,0,1);
      SelectScale(14);
      HideCursor;
      ImageSave.Put(MapX(0),MapY(1));
      GraphBackColor := MyBackColor;
      {SelectViewPort(19);
      ClearViewPort;}
      OpenViewport(1);
      {GraphBackColor := Red;
      OpenViewPort(15);
      SetColor(15);
      Print(1,1,'t = '+Num2Str(t,6));}
      GraphBackColor := MyBackColor;
      Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
      Draw_Barrier;
      ShowCursor;

   END;


PROCEDURE Calculate_Eigenwave(Wave: Wave_record; CurrentBarrier: PotentialDef;
                                VAR Real_Psi,Imag_Psi,Psi_Squared:DVector);

   VAR
      i,j: Integer;
      coskx,sinkx,expkx,expmkx: Real;
      EndOfRegion,xx: Real;


   BEGIN
      WITH Wave DO
      WITH CurrentBarrier DO
      BEGIN
         EndOfRegion := 0;
         j := 1;
         FOR i := 1 TO Length DO
            BEGIN
               IF ((x.value(i) > EndOfRegion) AND (j < Regions)) THEN
                  BEGIN
                     j:= j + 1;
                     EndOfRegion := EndOfRegion + Width[j];
                  END;
               CASE PsiType[j] OF
                  1: BEGIN
                        Coskx := cos(absk[j]*x.value(i));
                        Sinkx := sin(absk[j]*x.value(i));
                        Real_Psi.put(i,coskx*A[j].Re +
                                        coskx*B[j].Re -
                                         sinkx*A[j].Im +
                                          sinkx*B[j].Im);
                        Imag_Psi.put(i,sinkx*A[j].Re -
                                        sinkx*B[j].Re +
                                         coskx*A[j].Im +
                                          coskx*B[j].Im);
                     END;
                  -1: BEGIN
                        expkx := exp(-absk[j]*x.value(i));
                        expmkx := exp(absk[j]*x.value(i));
                        Real_Psi.put(i,expkx*A[j].Re + expmkx*B[j].Re);
                        Imag_Psi.put(i,expkx*A[j].Im + expmkx*B[j].Im);
                      END;
                   0: BEGIN
                         Real_Psi.put(i, B[j].Re + A[j].Re*x.value(i));
                         Imag_Psi.put(i, B[j].Im + A[j].Im*x.value(i));
                      END;
               END;
               Psi_Squared.put(i, Sqr(Real_Psi.value(i)) +
                                       Sqr(Imag_Psi.value(i)));
            END;
      END;
   END;


PROCEDURE Solve_Coefficients(E: Real; CPotential: PotentialDef;
                                                      VAR Wave: Wave_Record);

   VAR
      i,j: Integer;
      k: CDVector;
      RCond: Real;
      IPivot: IDVector;
      Matrix: DMatrix;
      x: ARRAY [1..5] OF Real;
      ak: Complex;

   BEGIN
      WITH CPotential DO
      WITH Wave DO
      BEGIN
      IPivot.init(4*(Regions-1));
      Matrix.init(4*(Regions-1),4*(Regions-1));
      SolutionVector.init(4*(regions-1));
      k.init(Regions);
      x[1] := 0;
      IF Regions > 2 THEN
         FOR i := 2 TO Regions-1 DO
            x[i] := x[i-1] + Width[i];
      FOR i := 1 TO Regions DO
         BEGIN
            IF Potential[i] < E THEN
               BEGIN
                  ak.Re := Sqrt(2*m*(E-Potential[i]));
                  ak.Im := 0;
                  k.put(i,ak);
                  absk[i] := ak.Re;
                  PsiType[i] := 1;
               END
            ELSE
            IF Potential[i] > E THEN
               BEGIN
                  ak.Re := 0;
                  ak.Im := Sqrt(2*m*(Potential[i]-E));
                  k.put(i,ak);
                  absk[i] := ak.Im;
                  PsiType[i] := -1;
               END
            ELSE
               BEGIN
                  ak.Re := 0;
                  ak.Im := 0;
                  k.put(i,ak);
                  absk[i] := 0;
                  PsiType[i] := 0;
               END;
         END;
         Matrix.put(1,1,1);
         Matrix.put(1,2,0);
         Matrix.put(2,1,0);
         Matrix.put(2,2,1);
         Matrix.put(3,1,0);
         Matrix.put(3,2,k.RePart(1));
         Matrix.put(4,1,-k.RePart(1));
         Matrix.put(4,2,0);
         i:= Regions-1;
         CASE PsiType[Regions] OF
            1: BEGIN
                  Matrix.put(4*i-3,4*i-1,-cos(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-3,4*i, sin(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-2,4*i-1, -sin(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-2,4*i, -cos(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-1,4*i-1, k.RePart(i+1)*sin(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-1,4*i, k.RePart(i+1)*cos(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i,4*i-1, -k.RePart(i+1)*cos(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i,4*i, k.RePart(i+1)*sin(k.RePart(i+1)*x[i]));
               END;
            0: BEGIN
                  Matrix.put(4*i-3,4*i-1, -1);
                  Matrix.put(4*i-3,4*i, 0);
                  Matrix.put(4*i-2,4*i-1, 0);
                  Matrix.put(4*i-2,4*i, -1);
                  Matrix.put(4*i-1,4*i-1, 0);
                  Matrix.put(4*i-1,4*i, 0);
                  Matrix.put(4*i,4*i-1, 0);
                  Matrix.put(4*i,4*i, 0);
               END;
           -1: BEGIN
                  Matrix.put(4*i-3,4*i-1, -exp(-k.ImPart(i+1)*x[i]));
                  Matrix.put(4*i-3,4*i, 0);
                  Matrix.put(4*i-2,4*i-1, 0);
                  Matrix.put(4*i-2,4*i, -exp(-k.ImPart(i+1)*x[i]));
                  Matrix.put(4*i-1,4*i-1, k.ImPart(i+1)*exp(-k.ImPart(i+1)*x[i]));
                  Matrix.put(4*i-1,4*i, 0);
                  Matrix.put(4*i,4*i-1, 0);
                  Matrix.put(4*i,4*i, k.ImPart(i+1)*exp(-k.ImPart(i+1)*x[i]));
               END;
         END;
      IF Regions > 2 THEN
         FOR i := 1 TO Regions-2 DO
            CASE PsiType[i+1] OF
               1: BEGIN
                  Matrix.put(4*i-3,4*i-1,-cos(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-3,4*i, sin(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-3,4*i+1, -cos(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-3,4*i+2, -sin(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-2,4*i-1, -sin(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-2,4*i, -cos(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-2,4*i+1, sin(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-2,4*i+2, -cos(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-1,4*i-1, k.RePart(i+1)*sin(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-1,4*i, k.RePart(i+1)*cos(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-1,4*i+1, k.RePart(i+1)*sin(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i-1,4*i+2, -k.RePart(i+1)*cos(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i,4*i-1, -k.RePart(i+1)*cos(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i,4*i, k.RePart(i+1)*sin(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i,4*i+1, k.RePart(i+1)*cos(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i,4*i+2, k.RePart(i+1)*sin(k.RePart(i+1)*x[i]));
                  Matrix.put(4*i+1,4*i-1, cos(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+1,4*i, -sin(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+1,4*i+1, cos(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+1,4*i+2, sin(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+2,4*i-1, sin(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+2,4*i, cos(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+2,4*i+1, -sin(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+2,4*i+2, cos(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+3,4*i-1, -k.RePart(i+1)*sin(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+3,4*i, -k.RePart(i+1)*cos(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+3,4*i+1, -k.RePart(i+1)*sin(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+3,4*i+2, k.RePart(i+1)*cos(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+4,4*i-1, k.RePart(i+1)*cos(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+4,4*i, -k.RePart(i+1)*sin(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+4,4*i+1, -k.RePart(i+1)*cos(k.RePart(i+1)*x[i+1]));
                  Matrix.put(4*i+4,4*i+2, -k.RePart(i+1)*sin(k.RePart(i+1)*x[i+1]));
                  END;
               0: BEGIN
                  Matrix.put(4*i-3,4*i-1, -x[i]);
                  Matrix.put(4*i-3,4*i, 0);
                  Matrix.put(4*i-3,4*i+1, -1);
                  Matrix.put(4*i-3,4*i+2, 0);
                  Matrix.put(4*i-2,4*i-1, 0);
                  Matrix.put(4*i-2,4*i, -x[i]);
                  Matrix.put(4*i-2,4*i+1, 0);
                  Matrix.put(4*i-2,4*i+2, -1);
                  Matrix.put(4*i-1,4*i-1, -1);
                  Matrix.put(4*i-1,4*i, 0);
                  Matrix.put(4*i-1,4*i+1, 0);
                  Matrix.put(4*i-1,4*i+2, 0);
                  Matrix.put(4*i,4*i-1, 0);
                  Matrix.put(4*i,4*i, -1);
                  Matrix.put(4*i,4*i+1, 0);
                  Matrix.put(4*i,4*i+2, 0);
                  Matrix.put(4*i+1,4*i-1, x[i+1]);
                  Matrix.put(4*i+1,4*i, 0);
                  Matrix.put(4*i+1,4*i+1, 1);
                  Matrix.put(4*i+1,4*i+2, 0);
                  Matrix.put(4*i+2,4*i-1, 0);
                  Matrix.put(4*i+2,4*i, x[i+1]);
                  Matrix.put(4*i+2,4*i+1, 0);
                  Matrix.put(4*i+2,4*i+2, 1);
                  Matrix.put(4*i+3,4*i-1, 1);
                  Matrix.put(4*i+3,4*i, 0);
                  Matrix.put(4*i+3,4*i+1, 0);
                  Matrix.put(4*i+3,4*i+2, 0);
                  Matrix.put(4*i+4,4*i-1, 0);
                  Matrix.put(4*i+4,4*i, 1);
                  Matrix.put(4*i+4,4*i+1, 0);
                  Matrix.put(4*i+4,4*i+2, 0);
                  END;
              -1: BEGIN
                  Matrix.put(4*i-3,4*i-1, -exp(-k.ImPart(i+1)*x[i]));
                  Matrix.put(4*i-3,4*i, 0);
                  Matrix.put(4*i-3,4*i+1, -exp(k.ImPart(i+1)*x[i]));
                  Matrix.put(4*i-3,4*i+2, 0);
                  Matrix.put(4*i-2,4*i-1, 0);
                  Matrix.put(4*i-2,4*i, -exp(-k.ImPart(i+1)*x[i]));
                  Matrix.put(4*i-2,4*i+1, 0);
                  Matrix.put(4*i-2,4*i+2, -exp(k.ImPart(i+1)*x[i]));
                  Matrix.put(4*i-1,4*i-1, k.ImPart(i+1)*exp(-k.ImPart(i+1)*x[i]));
                  Matrix.put(4*i-1,4*i, 0);
                  Matrix.put(4*i-1,4*i+1, -k.ImPart(i+1)*exp(k.ImPart(i+1)*x[i]));
                  Matrix.put(4*i-1,4*i+2, 0);
                  Matrix.put(4*i,4*i-1, 0);
                  Matrix.put(4*i,4*i, k.ImPart(i+1)*exp(-k.ImPart(i+1)*x[i]));
                  Matrix.put(4*i,4*i+1, 0);
                  Matrix.put(4*i,4*i+2, -k.ImPart(i+1)*exp(k.ImPart(i+1)*x[i]));
                  Matrix.put(4*i+1,4*i-1, exp(-k.ImPart(i+1)*x[i+1]));
                  Matrix.put(4*i+1,4*i, 0);
                  Matrix.put(4*i+1,4*i+1, exp(k.ImPart(i+1)*x[i+1]));
                  Matrix.put(4*i+1,4*i+2, 0);
                  Matrix.put(4*i+2,4*i-1, 0);
                  Matrix.put(4*i+2,4*i, exp(-k.ImPart(i+1)*x[i+1]));
                  Matrix.put(4*i+2,4*i+1, 0);
                  Matrix.put(4*i+2,4*i+2, exp(k.ImPart(i+1)*x[i+1]));
                  Matrix.put(4*i+3,4*i-1, -k.ImPart(i+1)*exp(-k.ImPart(i+1)*x[i+1]));
                  Matrix.put(4*i+3,4*i, 0);
                  Matrix.put(4*i+3,4*i+1, k.ImPart(i+1)*exp(k.ImPart(i+1)*x[i+1]));
                  Matrix.put(4*i+3,4*i+2, 0);
                  Matrix.put(4*i+4,4*i-1, 0);
                  Matrix.put(4*i+4,4*i, -k.ImPart(i+1)*exp(-k.ImPart(i+1)*x[i+1]));
                  Matrix.put(4*i+4,4*i+1, 0);
                  Matrix.put(4*i+4,4*i+2, k.ImPart(i+1)*exp(k.ImPart(i+1)*x[i+1]));
                  END;
            END;
         SolutionVector.put(1,-1);
         SolutionVector.put(2,0);
         SolutionVector.put(3,0);
         SolutionVector.put(4,-k.RePart(1));
         IF Regions > 2 THEN
            FOR i:= 1 TO Regions-2 DO
               BEGIN
                  SolutionVector.put(4*i+1,0);
                  SolutionVector.put(4*i+2,0);
                  SolutionVector.put(4*i+3,0);
                  SolutionVector.put(4*i+4,0);
               END;
         IF Regions > 2 THEN
            BEGIN
               SolutionVector.put(4*(Regions-1)-1,0);
               SolutionVector.put(4*(Regions-1),0);
            END;
         LUdecomp(Matrix,Rcond,IPivot);
         LUSolve(Matrix,SolutionVector,IPivot);
         A[1].Re := 1;
         A[1].Im := 0;
         FOR i := 2 TO Regions DO
            BEGIN
               A[i].Re := SolutionVector.value(4*i-5);
               A[i].Im := SolutionVector.value(4*i-4);
            END;
         FOR i := 1 TO Regions-1 DO
            BEGIN
               B[i].Re := SolutionVector.value(4*i-3);
               B[i].Im := SolutionVector.value(4*i-2);
            END;
         IF PsiType[Regions] = 0 THEN
            BEGIN
               A[Regions].Re := 0;
               A[Regions].Im := 0;
               B[Regions].Re := SolutionVector.value(4*Regions-5);
               B[Regions].Im := SolutionVector.value(4*Regions-4);
            END
         ELSE
            BEGIN
               B[Regions].Re := 0;
               B[Regions].Im := 0;
            END;
         Reflect := Sqr(CABS(B[1]));
         IF PsiType[Regions] = 1 THEN
               Transmit := Sqr(CABS(A[Regions]))*absk[Regions]/absk[1]
         ELSE Transmit := 0;
      END;
      Matrix.free;
      IPivot.free;
      SolutionVector.free;
      k.free;
   END;



PROCEDURE Show_Eigenwave(E: Real);

   VAR
      Wave: Wave_record;
      i: Integer;

   BEGIN
      Solve_Coefficients(E,CurrentBarrier,Wave);
      Calculate_Eigenwave(Wave,CurrentBarrier,Real_E_Psi,Imag_E_Psi,Psi_Squared);
      
      Real_Psi.Equate(1, Real_E_Psi);
      Imag_Psi.Equate(1,Imag_E_Psi);
      Old_Psi_Squared.equate(1, Psi_Squared);
      Old_Real_Psi.equate(1, Real_Psi);
      Old_Imag_Psi.equate(1, Imag_Psi);

{      Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
      Draw_Barrier;}
   END;


PROCEDURE Calculate_TranRefl;

   VAR
      Wave: Wave_Record;
      E,Old_E,Old_R,Old_T: Real;
      i: Integer;

   BEGIN
      Old_R := 1;
      Old_T := 0;
      Old_E := 0;
      SetColor(15);
      HideCursor;
      OpenViewPort(3);
      DefineScale(3, -0.4,10.4,-0.05,1.15);
      Axis(-2,-1,2,0.2);
      SetColor(15);
{      SelectViewPort(3);

      Print(2,1,'Reflection');}
      SelectViewPort(3);
      SetColor(12);
      Print(2,1,'Reflection');
      SetColor(14);
      Print(15,1,'Transmission');
      SelectViewPort(4);
      SetColor(15);
      Print(25,14,'E');
      FOR i := 1 TO 100 DO
         WITH Wave DO
         BEGIN
            E := i/10;
            Solve_Coefficients(E,CurrentBarrier,Wave);
            SelectViewPort(3);
            SelectScale(3);
            SetColor(12);
            ReflectVect.put(i,Reflect);
            TransmitVect.put(i,Transmit);
            PlotLine(Old_E,Old_R,E,Reflect);
            PlotLine(Old_E,Old_R,E,Reflect);
            SelectViewPort(3);
            SelectScale(3);
            SetColor(14);
            PlotLine(Old_E,Old_T,E,Transmit);
            Old_E := E;
            Old_T := Transmit;
            Old_R := Reflect;
         END;
      ShowCursor;
   END;





PROCEDURE Current_k(n,x: Real; VAR kk: Complex);

   BEGIN
   END;


PROCEDURE Evaluate_Psi_2(t: Real; Packet: Boolean);

   VAR
      i: Integer;
      Sum_Components: CDVector;
      z: Complex;
      Ei: Real;

   BEGIN
      IF Packet = True THEN
         BEGIN
            Sum_Components.init(Length);
            Sum_Components.MultReal(0);
            FOR i := 1 TO Num_Components DO
               BEGIN
                  Ei := Sqr(i)/(2*m);
                  z.Re := cos(Ei*t);
                  z.Im := sin(-Ei*t);
                  Sum_Components.AddVector(z,Psi_0[i]);
               END;
            Real_Psi.Equate(1,Sum_Components.Re);
            Imag_Psi.Equate(1,Sum_Components.Im);
            FOR i := 1 TO Length DO
               BEGIN
                  Psi_Squared.put(i, Sqr(Real_Psi.value(i))
                                    + Sqr(Imag_Psi.value(i)));
               END;
            Sum_Components.Free;
         END
      ELSE
         BEGIN
            Real_Psi.SumOfVectors(cos(E*t),Real_E_Psi,sin(E*t),Imag_E_Psi);
            Imag_Psi.SumOfVectors(sin(-E*t),Real_E_Psi,cos(E*t),Imag_E_Psi);
         END;
   END;

PROCEDURE CalculateEDistribution;

   BEGIN
   END;

PROCEDURE Construct_WavePacket(E: Real);

   VAR
      i: Integer;
      Amp,k0,ki,Ei,kspacing,k: Real;
      Wave: Wave_Record;
      p: Complex;
      q: Complex;
      s: Real;


   BEGIN
      k0 := Sqrt(2*m*E);
      FOR i := 1 TO Length DO
         BEGIN
            Real_Psi.put(i,0);
            Imag_Psi.put(i,0);
         END;
      FOR i := 1 TO 25 DO
         BEGIN
            kspacing := 3/25/deltax{k0/12};
            {IF (Kspacing > 2*pi/12) THEN Kspacing := 2*pi/12;}

            ki := k0 + (i-13)*kspacing;
            Ei := Sqr(ki)/(2*m);
            s := Sqr((ki-k0)*deltax);
            IF s < 28 THEN Amp := Exp(-s)*Sqrt(deltax/Sqrt(2*Pi))*kspacing
            ELSE Amp := 0;
            Solve_Coefficients(Ei,CurrentBarrier,Wave);
            Calculate_Eigenwave(Wave, CurrentBarrier,Psi_0[i].Re, Psi_0[i].Im, Psi_Squared);
            Psi_0[i].MultReal(Amp);
            p.Re := cos(2*Wave.absk[1]);
            p.Im := sin(2*Wave.absk[1]);
            {zd.Re := Wave.absk[1];
            zd.Im := 0;
            CxOp(zd,'*',_i,q);
            CExp(q,p);}
            Psi_0[i].MultScalar(p);

         END;
      FOR i := 1 TO 100 DO
         BEGIN
            k := Sqrt(2*m*Evect.value(i));
            s := Sqr((k-k0)*deltax);
            IF s < 28 THEN Amp := Exp(-s)
            ELSE Amp := 0;
            EPacket.put(i,sqr(Amp));
         END;
   END;


{PROCEDURE Label_Psi_Graph(Components: Boolean);

   BEGIN
      SelectViewPort(1);
      SetColor(PsiSquaredColor);
      Print(36,1,'Psi Squared');
      IF Components = True THEN
         BEGIN
            SetColor(RealPsiColor);
            Print(39,6,'Real Psi');
            SetColor(ImagPsiColor);
            Print(39,7,'Imag Psi');
         END;
   END;}


PROCEDURE Reset_Time;

   BEGIN
      t := 0;
      {t := 1/(-Sqrt(2*E/m));}
      {Evaluate_Psi_2(t,Packet}
      Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
      Draw_Barrier;
   END;


PROCEDURE Step_Time;

   BEGIN
      t := t + deltat;
      Evaluate_Psi_2(t,Packet);
      Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
      Draw_Barrier;
   END;


PROCEDURE Toggle_Components(VAR Components:Boolean);

   BEGIN
      Components := NOT(Components);
      IF Components THEN QMInterface.HotKeys.Key[4] := 'F6-Hide Real'
      ELSE QMInterface.HotKeys.Key[4] := 'F6-Show Real';
      QMInterface.HotKeys.Display;
      OpenViewPort(1);
      IF Components = True THEN DefineScale(7,-4-3/40,4+3/40,-5,5)
      ELSE DefineScale(7,-4-3/40,4+3/40,-1,8);
      Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
      Draw_Barrier;
      {Label_Psi_Graph(Components);}
   END;

{PROCEDURE ReduceBarrier;

   BEGIN
      WITH CurrentBarrier DO
         FOR i := 2 TO Regions DO
            BEGIN
               IF Potential[i] := Potential[i-1] THEN
               Width[i-1] := Width[i-1] + Width[i]
 }
PROCEDURE Change_Barrier;

   VAR
      i: Integer;

   BEGIN
      WITH Change_Barrier_Screen DO
      BEGIN
         AcceptScreen;
         IF (NOT Canceled) THEN
            WITH CurrentBarrier DO
            BEGIN
               Regions := 1 + GetRadioButton('1');
               FOR i := 1 TO Regions-1 DO
                  Potential[i+1] := GetNumber(2*i+3);
               IF Regions > 2 THEN
               FOR i := 1 TO Regions-2 DO
                  Width[i+1] := GetNumber(2*i+4);
              { ReduceBarrier;}
              SelectViewport(2);
              ClearViewPort;
              SetColor(15);
              Print(43,18,'x');
              Print(1,1,'V');
              {Pause;}
               OpenViewPort(1);
               Draw_Barrier;
               Calculate_TranRefl;
               t := 0;{Reset_Time;}
               IF Packet = False THEN Show_Eigenwave(E)
                  ELSE
                     BEGIN
                        Construct_WavePacket(E);
                        Evaluate_Psi_2(t,Packet);
                      {  Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
                        Draw_Barrier;}
                     END;
               Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
               Draw_Barrier;
            END;
      END;
   END;

PROCEDURE CreateSliders;

   BEGIN
      WITH QMInterface DO
         BEGIN
            Sliders.Init;
            Sliders.Create(1,0.1,10.0,1.0,0.65,0.15,1.00,2,'0.1','10.0','Energy',False,1);
            {Sliders.Create(2,10,200,50,0.9,0.08,0.4,2,'10','200','Mass',True,1);}
            Sliders.DrawAll;
         END;
    END;


PROCEDURE Change_Time_Step;

   BEGIN END;
{      WITH Time_Step_Screen DO
      BEGIN
         AcceptScreen;
         deltat := GetNumber(1);
      END;
   END;}


PROCEDURE Change_Particle_Parameters;

   VAR
      Lastm: Real;

   BEGIN
      WITH Change_Particle_Parameter_Screen DO
      WITH Message DO
      WITH QMInterface DO
      BEGIN
         Lastm := m;
         LastE := E;
         LastEPacket.Equate(1,EPacket);
         LastPacket := Packet;
         IF Change = 20 THEN
            BEGIN
               E := Sliders.value(1);
               SetNumber(4,E);
               {m := Sliders.value(2);
               SetNumber(3,m);}
            END
         ELSE
            BEGIN
               AcceptScreen;
               IF (NOT Canceled) THEN
                  BEGIN
                     IF GetBoolean(1) THEN Packet := FALSE ELSE Packet := True;
                     m := GetNumber(3);
                     E := GetNumber(4);
                     Sliders.Reset(1,E);
                  END;
            END;
         IF lastm <> m THEN Calculate_TranRefl;
         t := 0;{Reset_Time;}
         SetColor(15);
         SelectViewPort(2);
         ClearViewPort;
         SetColor(15);
         Print(43,18,'x');
         Print(1,1,'V');
         OpenViewPort(1);
         IF Packet = False THEN Show_Eigenwave(E)
         ELSE
            BEGIN
               Construct_WavePacket(E);
               Evaluate_Psi_2(t,Packet);
               CalculateEDistribution;
               {Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
               Draw_Barrier;}
            END;
         DrawEDistribution(packet);
         Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
         Draw_Barrier;
         {Label_Psi_Graph(Components);}
      END;
   END;





PROCEDURE Process;

   BEGIN
      WITH Barrier_Disp DO
      WITH Message DO
      WITH CurrentBarrier DO
      BEGIN
         Open;
         QMInterface.Menu.RowActivate(3,3,False);
         Regions := 2;
         Potential[1] := 0;
         Potential[2] := 2;
         Potential[3] := 0;
         Potential[4] := 0;
         Potential[5] := 0;
         Width[2] := 0.35;
         Width[3] := 5;
         Width[4] := 5;
         Width[5] := 5;
         done := False;
         next := 0;
         Components := True;
         One.Re := 1; {These two statements define 1 in complex numbers}
         One.Im := 0;
         _i.Re := 0; {These two statements define square root -1 in
         _i.Im := 1;     complex numbers}
         pmean := 10;
         xwidth := 0.5;
         height := 1;
         deltat := 0.2;
         deltax := 0.5;
         m := 50;
         E := 1;
         CreateSliders;
         HelpScreenInit;
         OpenViewPort(1);
         Definescale(7,-4-3/40,4+3/40,-2,5);
         DefineViewPort(11,0.05,1.00,0.82,0.86);
         SelectViewPort(11);
         SetColor(15);
         Print(9,1,'Potential and Wavefunction');
         Print(49,1,'Reflection and Transmission');
         Draw_Barrier;
         SelectViewPort(2);
         SetColor(15);
         Print(43,18,'x');
         Print(1,1,'V');
         Packet := False;
         Show_Eigenwave(E);
         Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
         Draw_Barrier;
         Components := True;
         {Label_Psi_Graph(Components);}
         Calculate_TranRefl;
         LastPacket := False;
         LastE := E;
         SelectViewPort(3);
         SetColor(7);
         SelectScale(3);
         PlotLine(E,-0.04,E,1.05);
         t := 0;{Reset_Time;}
      END;
   END;


BEGIN
   {$IFDEF MAIN}
   {$ENDIF}
   {ClearMUPPETport;}
   Initialize_Vectors;
   Initialize_Display_Object;
   QMInterface.HotKeys.Key[4] := 'F6-Hide Real';
   QMInterface.HotKeys.Display;
   Define_Input_Screens;
   GraphBackColor := MyBackColor;
   Process;
   WITH QMInterface DO
   WITH Message DO
      BEGIN
         Next := 3;
         Paused := True;
         Quit := False;
         Change := 0;
         WHILE (NOT Quit) DO
            BEGIN
               IF ((NOT QueryEvent(Message)) AND (NOT Paused)) THEN Step_Time
               ELSE IF Change <> 0 THEN
                  BEGIN
                     CASE Change OF
                        1: Change_Particle_Parameters;
                        2: Change_Barrier;
                        10: Help(CurrentHelpScreen);
                        14: Toggle_Components(Components);
                        15: Integrate;
                        13: BEGIN
                               Reset_Time;
                               Evaluate_Psi_2(t,Packet);
                               Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
                               Draw_Barrier;
                            END;
                        20: Change_Particle_Parameters;
                     END;
                     Change := 0;
                  END;
            END;
      END;
   Free_Vectors;
   QMInterface.Menu.RowActivate(3,3,True);
   QMInterface.Sliders.Done;
   Change_Particle_Parameter_Screen.Done;
   Change_Barrier_Screen.Done;
   Time_Step_Screen.Done;
   IntegrateScreen.Done;
   {Barrier_Disp.Menu.Done;}
   {$IFDEF MAIN}
   CUPSdone;
   {$ENDIF}
{$IFNDEF MAIN}
END;
{$ENDIF}

END.  { End of file QM_BARR.PAS }



