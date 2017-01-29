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

{ Start of file QM_SWELL.PAS }

{ ----------------------------------------------------------------------
   Program Title:    Particles in Wells
   Author:           Douglas E. Brandt
   Version:          0.11
   Program Summary:  This is one section of the CUPS Modern Physics
                     One Dimensional Wave Mechanics Program.  This program
                     solves for the energy eigenvalues and eigenstates for a
                     piecewise constant potential well.  The current version
                     solves only a single well, but the final version will
                     solve for a more general piecewise constant potential.
                     The program displays the energy eigenstates of the well,
                     energy of the corresponding width infinite square well
                     energy eigenvalues, and a wavepacket made of a
                     superposition of up to three energy eigenstates.  The
                     propagation of the wavepacket through time can be shown
                     in animation.
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
{$M 64000, 80000, 655360}

UNIT QM_SWel;

INTERFACE

{$ENDIF}

USES Crt, Dos, Graph, CUPSmupp, CUPS, CUPSgrph, CUPSfunc,CUPSproc, CUPSgui,
        CUPSpars, Objects, QM_Func, QM_Ifac;

{$IFNDEF MAIN}

PROCEDURE Simple_Well(QMInterface: TInterface; VAR Message: MessageRec);

IMPLEMENTATION

PROCEDURE Simple_Well(QMInterface: TInterface; VAR Message: MessageRec);

{$ENDIF}

   TYPE
      Function3 = FUNCTION(x,y,z: Real): Real;

   CONST
 
      Length = 240;
      fineLength = 1200;
      Stand_alone = True;
      Num_Points = 12;
      PsiSquaredColor = 13;
      RealPsiColor = 11;
      ImagPsiColor = 10;


   VAR
      HighLimit,LowLimit: Real;
      Psi_Squared, Real_Psi, Imag_Psi: DVector;
      Old_Psi_Squared, Old_Real_Psi, Old_Imag_Psi: DVector;
      Psi: ARRAY [1..3] OF DVector;
      EigenFunc: ARRAY [1..10] OF DVector;
      EigenVal: ARRAY [1..10] OF Real;
      Energy,V: Dvector;
      x,xvec,finex,finev: DVector;
      q,y,z,tt: Char;
      i, done, return, Num_Roots,ptype: Integer;
      Simple_Well_Disp: Base_Display;
      Well_Parameters, Wave_Components, Change_position_parms: TInputScreen;
      Time_Step_Screen, IntegrateScreen: TInputScreen;
      center, width, v1, v2, e, L, VMin,VMax,t,deltat,deltax: Real;
      vleft,vright,vtop: Real;
      xcenter, xwidth, pcenter, pwidth: Real;
      k,A: ARRAY [1..25,1..4] OF Real;
      Amplitude: ARRAY [1..4] OF Real;
      n: ARRAY [1..3] OF Integer;
      Norm: ARRAY [1..25] OF Real;
      Components,Redo, Hardwalls: Boolean;
      knum: Byte;
      Parser: TParser;
      ParserHelp, currentHelpScreen: HelpScrType;
      FirstIndex, LastIndex,dummy1,dummy2: Integer;
      psimin,psimax, psiscale, psisquaredscale: Real;
      {$IFDEF MAIN}
      next: Integer;
      {$ENDIF}

PROCEDURE Initialize_Vectors;

   VAR
      i: Integer;

   BEGIN
      Psi_Squared.init(Length);
      Real_Psi.init(Length);
      Imag_Psi.init(Length);
      Old_Psi_Squared.init(Length);
      Old_Real_Psi.init(Length);
      Old_Imag_Psi.init(Length);
      FOR i := 1 TO 3 DO Psi[i].init(Length);
      x.init(Length);
      FOR i := 1 TO Length DO x.put(i, 3*i/Length-1);
      deltax := x.value(2)-x.value(1);
      V.init(Length);
      finex.init(fineLength);
      FOR i := 1 TO fineLength DO finex.put(i, 3*i/Length-1);
      finev.init(fineLength);
      Energy.init(length);
      FOR i := 1 TO 10 DO Eigenfunc[i].init(length);
   END;


PROCEDURE Free_Vectors;

   VAR
      i: Integer;

   BEGIN
      Psi_Squared.Free;
      Real_Psi.Free;
      Imag_Psi.Free;
      Old_Psi_Squared.Free;
      Old_Real_Psi.Free;
      Old_Imag_Psi.Free;
      FOR i := 1 TO 3 DO Psi[i].Free;
      x.Free;
      V.Free;
      finex.free;
      fineV.free;
      Energy.Free;
      FOR i := 1 TO 10 DO Eigenfunc[i].free;
   END;



PROCEDURE Initialize_Display_Object;

   BEGIN
      WITH Simple_Well_Disp DO
         BEGIN
            Title := 'Bound Solutions in a Potential Well';
            Window_num := 2;
            Window_corner[1,1].x := 0.07;
            Window_corner[1,1].y := 0.10;
            Window_corner[1,2].x := 0.57;
            Window_corner[1,2].y := 0.81;
            Window_corner[2,1].x := 0.65;
            Window_corner[2,1].y := 0.10;
            Window_corner[2,2].x := 0.90;
            Window_corner[2,2].y := 0.81;
            DefineViewPort(3,0.91,1.00,0.10,0.81);
            DefineViewPort(4,0.58,1.00,0.6,0.85);
            DefineViewPort(6,0.237,0.404,0.34,0.58);
            DefineViewPort(15,0.42,0.56,0.13,0.18);
            DefineViewPort(17,0.15,0.49,0.59,0.78);
            DefineViewPort(19,0.0,0.57,0.06,0.82);
            DefineViewPort(20,0.572,0.90,0.06,0.82);
            Init := True;
         END;
   END;


PROCEDURE HelpScreenInit;

   BEGIN
         currentHelpScreen[01] := '        HELP FOR BOUND SOLUTIONS IN WELLS         ';
         currentHelpScreen[02] := '                                                  ';
         currentHelpScreen[03] := '    Input a potential by selecting the "Well   ';
         currentHelpScreen[04] := ' Parameters" item from the "Parameters" menu. You';
         currentHelpScreen[05] := ' may select a square well potential for which the';
         currentHelpScreen[06] := ' program finds the exact wavefunctions for the ';
         currentHelpScreen[07] := ' well defined by the parameters specified or you';
         currentHelpScreen[08] := ' may input a function to define a well.  If you';
         currentHelpScreen[09] := ' define your own well, the program integrates';
         currentHelpScreen[10] := ' Schrodinger''s time independent equation across';
         currentHelpScreen[11] := ' the well and searches for energy eigenvalues and ';
         currentHelpScreen[12] := ' eigenfunctions.';
         currentHelpScreen[13] := '';
         currentHelpScreen[14] := '    A wavepacket can be defined by specifying the  ';
         currentHelpScreen[15] := ' component amplitudes of the eigenfunctions that';
         currentHelpScreen[16] := ' have been found.  Select the "Wave Function ';
         currentHelpScreen[17] := ' Parameters" menu item under the "Parameters"  ';
         currentHelpScreen[18] := ' menu.  The wavepacket will be normalized by the';
         currentHelpScreen[19] := ' program so only relative amplitudes need be';
         currentHelpScreen[20] := ' entered.  The time evolution of the wavepacket  ';
         currentHelpScreen[21] := ' can be observed by pressing the F2-Run/Stop hot ';
         currentHelpScreen[22] := ' key.  Integrals of interest can be found using ';
         currentHelpScreen[23] := ' the F7-Integral hot key.                                                 ';
         currentHelpScreen[24] := '    Press any Key or Click Mouse to continue.     ';
         currentHelpScreen[25] := '                                                  ';

   END;

PROCEDURE Define_Input_Screens;

   BEGIN
      WITH Well_Parameters DO
      BEGIN
         init;
         DefineInputPort(0.145,0.855,0.12,0.85);
         Loadline(' Choose either square well or user defined well       ');
         Loadline('______________________________________________________');
         Loadline('  #1 Square Well  (Exact Wave Functions)               ');
         Loadline('                                                       ');
         Loadline('          Right Potential  = {    }                    ');
         Loadline('                                                       ');
         Loadline('          Left Potential  = {    }                     ');
         Loadline('                                                       ');
         Loadline('          Well Width  = {    }                         ');
         Loadline('______________________________________________________');
         Loadline('  #1 User Defined Well (Approximate Wave Functions)    ');
         Loadline('                                                       ');
         Loadline(' V(x) = "                                            "');
         Loadline(' Domain = {    },{    } #2 V is constant beyond domain ');
         Loadline('                        #2 V is infinite beyond domain ');
         Loadline('             [  Ok  ] [Cancel] [ Help ]                ');
         SetNumber(2,150.0);
         SetNumber(3,150.0);
         SetNumber(4,1.0);
         SetNumber(7,-1);
         SetNumber(8,1);
         SetString(6,'150*x^2');
         SetRadioButton('1',2);
         AboutParser(ParserHelp);
         SetHelpScreen(ParserHelp);
      END;
      WITH Wave_Components DO
      BEGIN
         init;
         DefineInputPort(0.5,0.9,0.2,0.8);
         Loadline('  Eigenfunction Components  ');
         Loadline('       of Wavepacket        ');
         Loadline('                           ');
         Loadline('  Component 1              ');
         Loadline('       n = { }   A = {   } ');
         Loadline('                           ');
         Loadline('  Component 2              ');
         Loadline('       n = { }   A = {   } ');
         Loadline('                           ');
         Loadline('  Component 3              ');
         Loadline('       n = { }   A = {   } ');
         Loadline('                           ');
         Loadline('     [  Ok  ] [Cancel]     ');
         SetNumber(1,1);
         SetNumber(2,3);
         SetNumber(3,2);
         SetNumber(4,3);
         SetNumber(5,3);
         SetNumber(6,0);
         SetNumberLimits(1,1,6);
         SetNumberLimits(3,1,6);
         SetNumberLimits(5,1,6);
      END;
      WITH Time_Step_Screen DO
      BEGIN
         init;
         DefineInputPort(0.5,0.9,0.5,0.62);
         Loadline('                          ');
         Loadline('    Time step = {    }    ');
         Loadline('                          ');
         Loadline('    [  Ok  ] [Cancel]     ');
         SetNumber(1,0.01);
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
{$R-}
PROCEDURE PlotDVectors(x, y : DVector; ibeg, iend: Integer);
{ You can control line color and setting.}
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
END;
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

FUNCTION xindex(z: Real): Integer;

   VAR
      i: Integer;

   BEGIN
      i := 1;
      WHILE ((x.value(i) < z) AND (i < Length)) DO i := i + 1;
      xindex := i;
   END;

FUNCTION fxindex(z: Real): Integer;

   VAR
      i: Integer;

   BEGIN
      i := 1;
      WHILE ((finex.value(i) < z) AND (i < fineLength)) DO i := i + 1;
      fxindex := i;
   END;



PROCEDURE Draw_Well(ptype: Integer);

   VAR
      imin,imax: Integer;
      RangeLength : Real;

   BEGIN
      IF ptype = 1 THEN
         BEGIN
            AutoyScaling(6,-L,2*L,V);
            SelectScale(6);
            HideCursor;
            PlotLine(-L,V1,0,V1);
            PlotLine(0,V1,0,0);
            PlotLine(0,0,L,0);
            PlotLine(L,0,L,V2);
            PlotLine(L,V2,2*L,V2);
            ShowCursor;
         END
      ELSE
         BEGIN
            v.MinMax(imin,Vmin,imaX,vMAX);
            RangeLength := VMax-VMin;
            DefineScale(6,LowLimit-L,HighLimit+L,VMin-0.05*RangeLength,VMin+1.2*RangeLength);
            Autoyscaling(6,LowLimit-L,HighLimit+L,fineV);
            SelectScale(6);
            HideCursor;
            IF Hardwalls THEN
               BEGIN
                  PlotLine(LowLimit,fineV.Value(fxindex(LowLimit)),LowLimit,2*VMax);
                  PlotDVectors(finex,finev,fxindex(LowLimit),fxindex(HighLimit));
                  PlotLine(HighLimit,fineV.Value(fxindex(HighLimit)),HighLimit,2*VMax);
               END
            ELSE
               PlotDVectors(finex,fineV,1,fineLength);
            ShowCursor;
         END;
   END;


{FUNCTION xindex(z: Real): Integer;

   VAR
      i: Integer;

   BEGIN
      i := 1;
      WHILE ((x.value(i) < z) AND (i < Length)) DO i := i + 1;
      xindex := i;
   END;}



FUNCTION Nodes(VAR psi: DVector; E: Real; BeginIntegrate,
                    EndIntegrate: Integer; VAR slopenodes: Integer): Integer;

   VAR
      tnodes,tsnodes,i: Integer;
      p, lastp, slope, lastslope,psimin,psimax : Real;
      SignChange: Boolean;

   BEGIN
      lastslope := Psi.value(BeginIntegrate-1)-Psi.Value(BeginIntegrate-2);
      lastp := Psi.value(BeginIntegrate-1);
      tnodes := 0;
      tsnodes := 0;
      SignChange := True;
      FOR i := BeginIntegrate TO EndIntegrate DO
         BEGIN
            p := (2+(-E+V.value(i-1))*Sqr(deltax)*2)
                  *psi.value(i-1)-psi.value(i-2);
            Slope := p-lastp;
            IF (p*lastp < 0) THEN
               BEGIN
                  tnodes := tnodes + 1;
                  SignChange := True;
               END;
            IF ((LastSlope*Slope < 0) AND (SignChange)) THEN
               BEGIN
                  tsnodes := tsnodes + 1;
                  SignChange :=False;
               END;
            psi.put(i,p);
            IF (p <> 0) THEN lastp := p;
            IF (slope <> 0) THEN LastSlope := slope;
         END;
      Nodes := tnodes;
      SlopeNodes := tsnodes;
   END;

PROCEDURE Normalize(VAR psi: DVector);

   VAR
      Norm : Real;
      i: Integer;

   BEGIN
      Norm := 0;
      FOR i := 1 TO Psi.Length DO
         Norm := Norm + Sqr(Psi.value(i));
      Norm := Sqrt(Norm*deltax);
      FOR i := 1 TO Psi.Length DO
         Psi.put(i, Psi.value(i)/Norm);
   END;
      


PROCEDURE SearchForEigenvalues;

   VAR
      RootsFound, NumberOfNodes, LastNodes, IntervalNodes,i,
         SlopeNodes, NextBottomSlopeNodes,
         TopSlopeNodes,BottomSlopeNodes, BottomNodes, TopNodes,
         TempNodes, MaxSlopeNodes, Key_Num, dummy1, dummy2 : Integer;
      Error, Tolerance, ETrial, Step, VLastInterval,
         InitialStep, Kleft, Kright, IntervalTop, IntervalBottom,
         NextBottom,VTrial,TEndSlope, psimin, psimax: Real;
      DoubleR: Double;
      PsiTemp: DVector;
      FoundInterval, SearchQuit, LastWait, Wait,singlestep: Boolean;
      Key,knum: Byte;
      LastEPrint: Integer;

   FUNCTION EndSlope: Real;
      BEGIN
         IF (Psitemp.value(LastIndex) <> 0) THEN
         EndSlope := ABS((Psitemp.value(LastIndex)-Psitemp.value(LastIndex-1))
                        /Psitemp.value(LastIndex)/deltax)
         ELSE EndSlope := Pwr(10,38);
      END;

   BEGIN
      LastEPrint := 32000;
      QMInterface.HotKeys.Key[3] := 'F3-Step';
      QMInterface.HotKeys.Key[4] := 'F4-Abandon';
      QMInterface.HotKeys.Display;
      HideCursor;
      SelectViewPort(11);
      SetColor(0);
      Print(17,1,'Wavepacket');
      SetColor(15);
      Print(10,1,'Searching for Eigenvalues');
      ShowCursor;
      RootsFound := 0;
      SetColor(15);
      PsiTemp.init(length);
      FirstIndex := Trunc(Length/3);
      LastIndex := Trunc(2*Length/3);
      IF HardWalls THEN
          BEGIN
             psiTemp.put(FirstIndex,0);
             psiTemp.put(FirstIndex+1,1);
             NumberOfNodes := 10;
             InitialStep := Sqr(pi)/2/Sqr(L);
             VTop := 1E38;
          END
       ELSE
          BEGIN
             Kleft := Sqrt((Vtop)*2);
             Kright := Sqrt((Vtop)*2);
             psiTemp.put(FirstIndex,1);
             psiTemp.put(FirstIndex+1, Kleft*deltax+1);
             NumberOfNodes := Nodes(psitemp, Vtop, FirstIndex+2, LastIndex, slopenodes);
             InitialStep := (Vtop - Vmin)/NumberOfNodes;
             IF EndSlope < Kright THEN NumberOFNodes := NumberOfNodes-1;
             IF (NumberOfNodes > 10) THEN
                BEGIN
                   Announce('This potential has > 10 bound states.  Will solve for first 10. ');
                  { +NumStr(NumberOfNodes,Trunc(log(10,1.0*NumberOfNodes+1)),0)+
                   'bound state solutions.');}
                   NumberOFNodes := 10;
                END;
          END;
      Tolerance := 0.0001*InitialStep;
      LastNodes := 0;
      TopNodes := 0;
      TopSlopeNodes := 0;
      BottomNodes := 0;
      BottomSlopeNodes := 0;
      IntervalBottom := VMin;
      IntervalTop := VMin;
      NextBottom := VMin;
      MaxSlopeNodes:= SlopeNodes;
      HideCursor;
      GraphBackColor := Red;
      OpenViewPort(17);
      SelectViewPort(17);
      Print(1,1,' Press:');
      Print(3,2,'  F2 - Run/Stop Search');
      Print(3,3,'  F3 - Step Search');
      Print(3,4,'  F4 - Abandon Search');
      GraphBackColor := MyBackColor;
      ShowCursor;   
      SearchQuit := False;
      Wait := True;
      SingleStep := False;
      WHILE Wait DO
         BEGIN
            CheckForEvents;
            IF QMInterface.HotKeys.Pressed(knum) THEN
               BEGIN
                  CASE knum OF
                   2: Wait := False;
                   3: BEGIN
                       SingleStep := True;
                       Wait := False;
                      END;
                   4: BEGIN
                       SearchQuit := True;
                       Wait := False;
                      END;
                  END;
               END;
         END;
      WHILE ((RootsFound < NumberOfNodes{MaxSlopeNodes})
            AND (NOT SearchQuit)) DO
         BEGIN
            Step := InitialStep;
            FoundInterval := False;
            IntervalBottom := IntervalTop;
            BottomSlopeNodes := TopSlopeNodes;
            BottomNodes := TopNodes;
            WHILE ((TopNodes <= RootsFound) AND (NOT SearchQuit)) DO
               BEGIN
                  IntervalTop := IntervalTop + InitialStep;
                  IF IntervalTop > VTop THEN IntervalTop := VTop;
                  IF (NOT Hardwalls) THEN
                     BEGIN
                        Kleft := Sqrt((Vleft-IntervalTop)*2);
                        Kright := Sqrt((Vright-IntervalTop)*2);
                        psiTemp.put(FirstIndex+1, Kleft*deltax+1);
                        psiTemp.put(FirstIndex, 1);
                     END;
                  TopNodes := Nodes(psitemp, IntervalTop, FirstIndex+2, LastIndex, slopenodes);
                  TopSlopeNodes := slopeNodes;
                  SelectViewPort(6);
                  HideCursor;
                  ClearViewPort;
                  Psitemp.MinMax(dummy1,psimin,dummy2,psimax);
                  IF (ABS(PsiMin)>Psimax) THEN PsiMax := ABS(PsiMin);
                  DefineScale(9,LowLimit,HighLimit,-psimax,psimax);
                  SelectScale(9);
                  PlotDVectors(x,psitemp,FirstIndex,LastIndex);
                  ShowCursor;
                  IF SingleStep THEN
                   Wait := True;
                   SingleStep := False;
                   WHILE Wait DO
                    BEGIN
                     CheckForEvents;
                     IF QMInterface.HotKeys.Pressed(knum) THEN
                      BEGIN
                       CASE knum OF
                        2: Wait := False;
                        3: BEGIN
                            SingleStep := True;
                            Wait := False;
                           END;
                        4: BEGIN
                            SearchQuit := True;
                            Wait := False;
                           END;
                       END;
                     END;
                   END;
                END;
            Step := (IntervalTop-IntervalBottom)/2;
            WHILE ((((BottomNodes <> RootsFound)
                    OR (NOT Hardwalls AND (TopNodes <> RootsFound))
                    OR (BottomSlopeNodes <> RootsFound+1)
                    OR (TopSlopeNodes <> RootsFound+1))
                    AND (Step > 1E-10*InitialStep)) AND (NOT SearchQuit)) DO
               BEGIN

                  Step := (IntervalTop - IntervalBottom)/2;
                  VTrial := IntervalBottom+Step;
                  IF (NOT Hardwalls) THEN
                     BEGIN
                        Kleft := Sqrt((Vleft-VTrial)*2);
                        Kright := Sqrt((Vright-VTrial)*2);
                        psiTemp.put(FirstIndex+1, Kleft*deltax+1);
                     END;
                  TempNodes := Nodes(psitemp,vtrial,FirstIndex+2,
                                        LastIndex,slopenodes);
                  SelectViewPort(6);
                  HideCursor;
                  ClearViewPort;
                  Psitemp.MinMax(dummy1,psimin,dummy2,psimax);
                  IF (ABS(PsiMin)>Psimax) THEN PsiMax := ABS(PsiMin);
                  DefineScale(9,LowLimit,HighLimit,-psimax,psimax);
                  SelectScale(9);
                  PlotDVectors(x,psitemp,FirstIndex,LastIndex);
                  ShowCursor;
                  TEndSlope := EndSlope;
                  IF (NOT Hardwalls) THEN
                  BEGIN{1}
                  IF (TempNodes = RootsFound) THEN
                     BEGIN{2}
                     IF (SlopeNodes = RootsFound+1) THEN
                        BEGIN{3}
                        IF (EndSlope < Kright) THEN
                           BEGIN{4}
                              IntervalBottom := VTrial;
                              BottomNodes := RootsFound;
                              BottomSlopeNodes := RootsFound+1;
                           END{4}
                        ELSE
                           BEGIN{5}
                              IntervalTop := VTrial;
                              TopNodes := RootsFound;
                              TopSlopeNodes := RootsFound+1;
                           END;{5}
                        END{3}
                     ELSE
                        BEGIN{6}
                        IF ((SlopeNodes < RootsFound+1)) {OR
                              ((SlopeNodes = RootsFound+1)
                                  AND (EndSlope < Kright))} THEN
                           BEGIN{7}
                              IntervalBottom := VTrial;
                              BottomNodes := TempNodes;
                              BottomSlopeNodes := SlopeNodes;
                           END{7}
                        ELSE
                           BEGIN{8}
                              IntervalTop := VTrial;
                              TopNodes := TempNodes;
                              TopSlopeNodes := SlopeNodes;
                           END;{8}
                        END;{6}
                     END{2}
                  ELSE
                     BEGIN{9}
                     IF TempNodes < RootsFound THEN
                        BEGIN{10}
                           IntervalBottom := VTrial;
                           BottomNodes := TempNodes;
                           BottomSlopeNodes := SlopeNodes;
                        END{10}
                     ELSE IntervalTop := VTrial;
                        BEGIN{11}
                           IntervalTop := VTrial;
                           TopNodes := TempNodes;
                           TopSlopeNodes := SlopeNodes;
                         END;{11}
                      END;{9}
                     END{1}
                  ELSE
                  BEGIN
                  IF (TempNodes > RootsFound) THEN
                      BEGIN{12}
                         IntervalTop := VTrial;
                         TopNodes := TempNodes;
                         TopSlopeNodes := SlopeNodes;
                      END{12}
                  ELSE
                      BEGIN{13}
                         IntervalBottom := VTrial;
                         BottomNodes := TempNodes;
                         BottomSlopeNodes := SlopeNodes;
                      END;{13}
                   END;
            Step := (IntervalTop-IntervalBottom)/2;
                IF SingleStep THEN
                   Wait := True;
                   SingleStep := False;
                   WHILE Wait DO
                    BEGIN
                     CheckForEvents;
                     IF QMInterface.HotKeys.Pressed(knum) THEN
                      BEGIN
                       CASE knum OF
                        2: Wait := False;
                        3: BEGIN
                            SingleStep := True;
                            Wait := False;
                           END;
                        4: BEGIN
                            SearchQuit := True;
                            Wait := False;
                           END;
                       END;
                    END;
                 END;
            END;
            IF SearchQuit = False THEN REPEAT
               BEGIN
                  ETrial := IntervalBottom+Step;
                  IF (NOT Hardwalls) THEN
                     BEGIN
                        Kleft := Sqrt((Vleft-ETrial)*2);
                        Kright := Sqrt((Vright-ETrial)*2);
                        psiTemp.put(FirstIndex+1, Kleft*deltax+1);
                     END;
                  FOR i := FirstIndex+2 TO LastIndex DO
                     BEGIN
                        PsiTemp.put(i, (2+(-ETrial+V.value(i-1))*Sqr(deltax)*2)
                                *psiTemp.value(i-1)-psiTemp.value(i-2));

                     END;
                  SelectViewPort(6);
                  HideCursor;
                  ClearViewPort;
                  Psitemp.MinMax(dummy1,psimin,dummy2,psimax);
                  IF (ABS(PsiMin)>Psimax) THEN PsiMax := ABS(PsiMin);
                  DefineScale(9,LowLimit,HighLimit,-psimax,psimax);
                  SelectScale(9);
                  PlotDVectors(x,psitemp,FirstIndex,LastIndex);
                  ShowCursor;
                  IF Hardwalls THEN error :=
                         PsiTemp.value(LastIndex)*(1-2*(Rootsfound MOD 2))
                  ELSE error := (PsiTemp.value(LastIndex)
                                  -PsiTemp.value(LastIndex-1))/deltax
                                  /PsiTemp.value(LastIndex)
                                      + Kright;
                  IF (error > 0) THEN IntervalBottom := IntervalBottom + Step;
                  Step := Step/2;
                  IF SingleStep THEN
                   Wait := True;
                   SingleStep := False;
                   WHILE Wait DO
                    BEGIN
                     CheckForEvents;
                     IF QMInterface.HotKeys.Pressed(knum) THEN
                      BEGIN
                       CASE knum OF
                        2: Wait := False;
                        3: BEGIN
                            SingleStep := True;
                            Wait := False;
                           END;
                        4: BEGIN
                            SearchQuit := True;
                            Wait := False;
                           END;
                       END;
                    END;
                 END;
              END;
            UNTIL ((Step < 0.0000001*InitialStep) OR (SearchQuit = True));
            IF NOT SearchQuit THEN
            BEGIN
            SelectViewPort(6);
            {ClearViewPort;}
            RootsFound := RootsFound + 1;
            EigenVal[RootsFound] := ETrial;
            FOR i := 1 TO FirstIndex-1 DO
            BEGIN
            e := 2.718281828;
            DoubleR := kleft*x.value(i);
               IF (NOT Hardwalls) THEN psitemp.put(i, exp(kLeft*x.value(i))
                                         *psitemp.value(FirstIndex)
                                           /exp(kleft*x.value(FirstIndex)))
               ELSE psitemp.put(i,0);
            END;
            FOR i := LastIndex+1 TO LEngth DO
               IF (NOT Hardwalls) THEN psitemp.put(i, exp(-kright*x.value(i))*
                                         psitemp.value(LastIndex)
                                           *exp(kright*x.value(LastIndex)))
               ELSE psitemp.put(i,0);
            EigenFunc[RootsFound].Equate(1/psimax,PsiTemp);
            Normalize(EigenFunc[RootsFound]);
            VLastInterval := VLastInterval + 2*Step;
            LastNodes := RootsFound;
            SelectViewPort(3);
            SetColor(15);
            DefineScale(15,0,1,GraphScales[6].Ymin,GraphScales[6].Ymax);
            SelectScale(15);
            IF (LastEPrint > (MapY(ETrial) + TextHeight('M'))) THEN
              LastEPrint := MapY(ETrial)
            ELSE LastEPrint := LastEPrint-TextHeight('M')-1;
            OutTextxy(MapX(0.1),LastEPrint,NumStr(ETrial,4,2));
            END;
            LastWait := False;
            Wait := True;
            WHILE Wait DO
               BEGIN
                  CheckForEvents;
                  IF QMInterface.HotKeys.Pressed(knum) THEN
                     BEGIN
                        CASE knum OF
                         2: LastWait := NOT LastWait;
                         3: BEGIN
                             SingleStep := True;
                             LastWait := False;
                            END;
                         4: BEGIN
                             SearchQuit := True;
                             LastWait := False;
                            END;
                        END;
                     END;
                  Wait := LastWait;
              END;
         END;
      Num_Roots := RootsFound;
      WITH Wave_Components DO
        BEGIN
          SetNumberLimits(1,1,Num_Roots);
          SetNumberLimits(3,1,Num_Roots);
          SetNumberLimits(5,1,Num_Roots);
        END;
      HideCursor;
      SelectViewPort(11);
      SetColor(0);
      Print(10,1,'Searching for Eigenvalues');
      SetColor(15);
      Print(17,1,'Wavepacket');
      QMInterface.HotKeys.Key[3] := 'F3-Reset';
      IF Components THEN QMInterface.HotKeys.Key[4] := 'F6-Hide Real'
       ELSE QMInterface.HotKeys.Key[4] := 'F6-Show Real';
      QMInterface.HotKeys.Display;
      ShowCursor;
      PsiTemp.free;
   END;



PROCEDURE Solve_Eigenvalues(ptype: Integer);

   VAR
      q,Emax,k1k2,sink2L: Real;
      F: FunctionX;
      i, N, LastEPrint: Integer;


   BEGIN
      IF ptype = 1 THEN
         BEGIN
            N := Trunc(VMin*Sqr(L)/3)+4;
            IF V2 < V1 THEN Emax := V2 ELSE Emax := V1;
            PRoots(F2,v1,v2,L,macheps,0.98*Emax,0.0001*Emax,N,Num_Roots,Energy);
            FOR i := 1 TO Num_Roots DO
               BEGIN
                  q := Energy.value(i);
                  k[i,1] := Sqrt(2 * (v1 - Energy.value(i)));
                  k[i,2] := Sqrt(2 * Energy.value(i));
                  k[i,3] := Sqrt(2 * (v2 - Energy.value(i)));
                  k1k2 := k[i,1]/k[i,2];
                  sink2L := Sin(k[i,2] * L);
                  Norm[i] := 1/(k[i,1]);
                  Norm[i] := Norm[i] + Exp(-k[i,3] * L)/(k[i,3])
                             *(Cos(k[i,2]*L)+k1k2*sink2L);
                  Norm[i] := Norm[i]+(1-Sqr(k1k2))*sink2L*Cos(k[i,2]*L)/(k[i,2]);
                  Norm[i] := Norm[i] + 4 * k[i,1]/Sqr(k[i,2]) * Sqr(sink2L);
                  Norm[i] := Norm[i] + L * (1+Sqr(k1k2));
                  Norm[i] := Sqrt(Norm[i]/2);
                  A[i,1] := 1/Norm[i];
                  A[i,2] := 1/Norm[i];
                  A[i,3] := k[i,1]/k[i,2]/Norm[i];
                  A[i,4] := Exp(k[i,3]*L)*(A[i,2]*Cos(k[i,2]*L)+A[i,3]*Sin(K[i,2]*L));
               END;
            WITH Wave_Components DO
             BEGIN
              SetNumberLimits(1,1,Num_Roots);
              SetNumberLimits(3,1,Num_Roots);
              SetNumberLimits(5,1,Num_Roots);
             END;
            SelectViewPort(3);
            OpenViewPort(3);
            SetColor(15);
            SelectViewPort(3);
            SetColor(15);
            DefineScale(15,0,1,GraphScales[6].Ymin,GraphScales[6].Ymax);
            SelectScale(15);
            LastEPrint := 32000;
            FOR i := 1 TO Num_Roots DO
            BEGIN
              IF (LastEPrint > (MapY(Energy.Value(i)) + TextHeight('M'))) THEN
                LastEPrint := MapY(Energy.Value(i))
              ELSE LastEPrint := LastEPrint-TextHeight('M')-1;
              OutTextxy(MapX(0.1),LastEPrint,NumStr(Energy.Value(i),4,2));
            END;
         END
      ELSE
         BEGIN
         SearchForEigenValues;
         END;
   END;


PROCEDURE Plot_functions;

   BEGIN
         SelectViewPort(1);
         SetColor(1);
         SelectScale(7);
         SetColor(GraphBackColor);
         HideCursor;
         PlotDVectors(x,Old_Psi_Squared, 1, Length);
         SetColor(PsiSquaredColor);
         PlotDVectors(x, Psi_Squared, 1, length);
         Old_Psi_Squared.Equate(1,Psi_Squared);
         IF Components = True THEN
            BEGIN
               SelectScale(8);
               SetColor(GraphBackColor);
               PlotDVectors(x,Old_Real_Psi,1,Length);
               SetColor(RealPsiColor);
               PlotDVectors(x, Real_Psi, 1, length);
               Old_Real_Psi.Equate(1,Real_Psi);
               SetColor(GraphBackColor);
               PlotDVectors(x,Old_Imag_Psi,1,Length);
               SetColor(ImagPsiColor);
               PlotDVectors(x, Imag_Psi, 1, length);
               Old_Imag_Psi.Equate(1,Imag_Psi);
            END;
         SelectViewPort(1);
         SetColor(14);
         Draw_Well(ptype);
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
               PsiChar(0.175,0.955,0.024,0.022);
               Print(5,1,'Re');
               SetColor(ImagPsiColor);
               PsiChar(0.27,0.955,0.024,0.022);
               Print(9,1,'Im');
            END;
         SetColor(14);
         Print(39,1,'V');
         ShowCursor;
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
                     GraphBackColor := Red;
                     OpenViewPort(15);
                     SetColor(15);
                     Print(1,1,'t = '+Num2Str(t,6));
                     GraphBackColor := MyBackColor;
                     Plot_Functions;
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
                               MultVectors(Integrand,Psi_squared,V);
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
      HideCursor;
      ImageSave.Get(MapX(0),MapY(1),MapX(1),MapY(-0.59523));
      ShowCursor;
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
      SelectScale(12);
      HideCursor;
      ImageSave.Put(MapX(0),MapY(1));
      GraphBackColor := MyBackColor;
      SelectViewPort(19);
      ClearViewPort;
      OpenViewport(1);
      GraphBackColor := Red;
      OpenViewPort(15);
      SetColor(15);
      Print(1,1,'t = '+Num2Str(t,6));
      GraphBackColor := MyBackColor;
      Plot_Functions;
      ShowCursor;

   END;


PROCEDURE Normalize_Amplitude;

   VAR
      Norm: Real;
      i: Integer;

   BEGIN
      Norm := 0;
      FOR i := 1 TO 3 DO Norm := Norm + Sqr(Amplitude[i]);
      FOR i := 1 TO 3 DO Amplitude[i] := Amplitude[i]/Sqrt(Norm);
   END;


{PROCEDURE Normalize;

   BEGIN
      Norm := 0;
      FOR i := 1 TO length DO
         Norm := Norm + Psi_Squared.Value(i);
      Norm := Norm/(x.value(2)-x.value(1));
      FOR i := 1 TO Length DO
         BEGIN
            Psi_Squared.put(i, Psi_Squared.Value(i)/Norm);
            Real_psi.put(i,Real_Psi.value(i)/Sqrt(norm));
            Imag_Psi.put(i,imag_Psi.value(i)/Sqrt(norm));
         END;
   END;}

PROCEDURE Draw_Eigenstates;

   VAR
      i,j: Integer;
      Psi: DVector;
      q,q1: Real;

   BEGIN
      Psi.init(Length);
      SetColor(15);
      HideCursor;
      SelectViewPort(20);
      ClearViewPort;
      OpenViewPort(2);
      SelectViewPort(2);
      SetColor(Yellow);
      Draw_Well(ptype);
      FOR i := 1 TO Num_Roots DO
         BEGIN
            IF (Ptype = 1) THEN
               BEGIN
                  SelectScale(6);
                  SetColor(7);
               FOR j:= 1 TO Length DO
                  BEGIN
                     IF x.value(j) < 0 THEN
                        Psi.put(j, exp(k[i,1] * x.value(j))*A[i,1])
                     ELSE
                        IF x.value(j) > L THEN
                           Psi.put(j, exp(-k[i,3]* x.value(j))*A[i,4])
                        ELSE
                           Psi.put(j, Cos(k[i,2] * x.value(j))*A[i,2] +
                                    Sin(k[i,2] * x.value(j))*A[i,3]);
                  END;
                  SelectViewPort(2);
                  Psi.MultScalar((VMin)/(4*Num_Roots));
                  Psi.AddScalar(Energy.value(i));
                  SetColor(7); 
                  PlotDVectors(x,Psi,1,Length);
               END
            ELSE
               BEGIN
                   SelectScale(6);
                   psi.equate(1,eigenfunc[i]);
                   SelectViewPort(1);
                   SelectViewPort(2);
                   Psi.MultScalar((VMax-VMin)/(4*Num_Roots));
                   Psi.AddScalar(eigenval[i]);
                   SetColor(7);
                   PlotDVectors(x,Psi,1,Length);
                END;
         END;
      Psi.Free;
      ShowCursor;
   END;


PROCEDURE Evaluate_Psi(t: Real);

   VAR
      Theta1,Theta2,Theta3: Real;
      i: Integer;

   BEGIN
      IF (ptype = 1) THEN
         BEGIN
            Theta1 := Energy.value(n[1]) * t;
            Theta2 := Energy.value(n[2]) * t;
            Theta3 := Energy.value(n[3]) * t;
         END
      ELSE
         BEGIN
            Theta1 := Eigenval[n[1]] * t;
            Theta2 := EigenVal[n[2]] * t;
            Theta3 := EigenVal[n[3]] * t;
         END;
      FOR i:= 1 TO Length DO
         BEGIN
            Real_Psi.put(i, Psi[1].value(i) * Cos(Theta1)
                          + Psi[2].value(i) * Cos(Theta2)
                          + Psi[3].value(i) * Cos(Theta3));
            Imag_Psi.put(i, Psi[1].value(i) * Sin(Theta1)
                          + Psi[2].value(i) * Sin(Theta2)
                          + Psi[3].value(i) * Sin(Theta3));
            Psi_Squared.put(i, Sqr(Real_psi.value(i))
                                 + Sqr(Imag_Psi.value(i)));
         END;
   END;


PROCEDURE Prepare_Wave;

   VAR
      i,j: Integer;
      Norm :Real;

   BEGIN
      IF (ptype = 1) THEN
      BEGIN
      FOR i := 1 TO 3 DO
         BEGIN
            FOR j:= 1 TO Length DO
               BEGIN
                  IF x.value(j) < 0 THEN
                     Psi[i].put(j, Amplitude[i] * A[n[i],1] *
                                       exp(k[n[i],1] * x.value(j)))
                  ELSE
                     IF x.value(j) > L THEN
                        Psi[i].put(j, Amplitude[i] * A[n[i],4] *
                                          exp(-k[n[i],3] * x.value(j)))
                     ELSE
                        Psi[i].put(j, Amplitude[i] *
                                   (A[n[i],2] * Cos(k[n[i],2] * x.value(j)) +
                                   A[n[i],3] * Sin(k[n[i],2] * x.value(j))));
               END;
         END;
      Norm := 0;
      FOR i := 1 TO 3 DO
      FOR j := 1 TO Length DO
         BEGIN
            Norm := Norm + Sqr(Psi[i].value(j));
         END;
      Norm := Sqrt((Norm)*(x.value(2)-x.value(1)));
      FOR i := 1 TO 3 DO
      FOR j := 1 TO Length DO
         BEGIN
            Psi[i].put(j,Psi[i].value(j)/Norm);
         END;
      END
      ELSE
      BEGIN
      FOR i := 1 TO 3 DO
            FOR j:= 1 TO Length DO
                     Psi[i].put(j, Amplitude[i]*Eigenfunc[n[i]].value(j));
{      Norm := 0;
      FOR i := 1 TO 3 DO
      FOR j := 1 TO Length DO
         BEGIN
            Norm := Norm + Sqr(Psi[i].value(j));
         END;
      Norm := Sqrt((Norm)*(x.value(2)-x.value(1)));
      FOR i := 1 TO 3 DO
      FOR j := 1 TO Length DO
         BEGIN
            Psi[i].put(j,Psi[i].value(j)/Norm);
         END;}
      END;
   END;

PROCEDURE Change_Wave_Packet;

   VAR
      i: Integer;

   BEGIN
      WITH Wave_Components DO
      BEGIN
         AcceptScreen;
         FOR i := 1 TO 3 DO
            BEGIN
               n[i] := Trunc(GetNumber(2*i-1));
               Amplitude[i] := GetNumber(2*i);
            END;
      END;
      Normalize_Amplitude;
      Prepare_Wave;
      Evaluate_Psi(0);
      Psi_squared.MinMax(dummy1,psimin,dummy2,psimax);
      PsiScale := sqrt(psimax);
      PsisquaredScale := psimax;
      {Normalize;}
      Plot_Functions;
   END;



PROCEDURE Toggle_Components(VAR Components:Boolean);

   BEGIN
      Components := NOT(Components);
      IF Components THEN QMInterface.HotKeys.Key[4] := 'F6-Hide Real'
      ELSE QMInterface.HotKeys.Key[4] := 'F6-Show Real';
      QMInterface.HotKeys.Display;
      IF Components = True THEN DefineScale(7,-L+LowLimit-3/40*L,2*L+LowLimit+3/40*L,
                       -2*psisquaredscale,5*psisquaredscale/3)
      ELSE DefineScale(7,-L+LowLimit-3/40*L,2*L+LowLimit+3/40*L,
                         -1/3*psisquaredscale,5*psisquaredscale/3);
      OpenViewPort(1);
      GraphBackColor := Red;
      OpenViewPort(15);
      SetColor(15);
      Print(1,1,'t = '+Num2Str(t,6));
      GraphBackColor := MyBackColor;
      Plot_Functions;
     { Label_Psi_Graph(Components);}
   END;


PROCEDURE Change_Well;

   VAR
      i, VminIndex,VMaxIndex: Integer;
      extra: Real;

   BEGIN
      ReDo := False;
      WITH Well_Parameters DO
      BEGIN
         AcceptScreen;
         IF NOT Canceled THEN
         BEGIN
         ptype := GetRadioButton('1');
         IF (ptype = 1) THEN
            BEGIN
               V1 := GetNumber(2);
               V2 := GetNumber(3);
               L := GetNumber(4);
               FOR i := 1 TO Length DO x.put(i, (3*i/Length-1)*L);
               deltax := x.value(2)-x.value(1);
               IF V2 > V1 THEN
                  BEGIN
                     VMin := V1;
                     VMax := V2;
                  END
               ELSE
                  BEGIN
                     VMin := V2;
                     VMax := V1;
                  END;
               FOR i := 1 TO Length DO
                  BEGIN
                     IF x.value(i)<0 THEN V.put(i,V1)
                     ELSE IF x.value(i)<=L THEN V.Put(i,0)
                     ELSE V.Put(i,V2);
                  END;
            END
         ELSE
            BEGIN
               q := 'x';
               IF Parser.parse(q,y,z,tt,GetString(6)) THEN
                  BEGIN
                     HardWalls := False;
                     IF GetRadioButton('2') = 2 THEN Hardwalls := True;
                     IF GetNumber(7) <> GetNumber(8) THEN
                        BEGIN
                           LowLimit := GetNumber(7);
                           HighLimit := GetNumber(8);
                           IF LowLimit > HighLimit THEN
                              BEGIN
                                 HighLimit := LowLimit;
                                 LowLimit := GetNumber(8);
                              END;
                           L := HighLimit-LowLimit;
                           FOR i := 1 TO Length DO
                              BEGIN
                                 x.put(i,LowLimit+(3*i/Length-1)*L);
                                 deltax := x.value(2)-x.value(1);
                                 IF x.value(i) < LowLimit THEN v.put(i,Parser.f(LowLimit,t,t,t))
                                    ELSE IF x.value(i) >= HighLimit THEN v.put(i,Parser.f(HighLimit,t,t,t))
                                       ELSE v.put(i,Parser.f(x.value(i),t,t,t));
                              END;
                           FOR i := 1 TO FineLength DO
                              BEGIN
                                 finex.put(i,LowLimit+(3*L*i/FineLength-1)*L);

                                 IF finex.value(i) < LowLimit THEN finev.put(i,Parser.f(LowLimit,t,t,t))
                                    ELSE IF finex.value(i) >= HighLimit THEN finev.put(i,Parser.f(HighLimit,t,t,t))
                                       ELSE finev.put(i,Parser.f(finex.value(i),t,t,t));
                              END;
                        END{THEN}
                     ELSE
                        BEGIN
                           announce('Error: Domain can''t have length zero');
                           ReDo := True;
                        END;
                     VLeft := V.value(1);
                     VRight := V.value(Length);
                     IF (Vleft < Vright) THEN Vtop := Vleft ELSE Vtop := VRight;
                     V.MinMax(VMinIndex,Vmin,VMaxIndex,VMax);
                     IF ((Vmin = Vtop) AND (NOT ReDo)) THEN
                        BEGIN
                           ReDo := True;
                           Announce('Need well with minimum for bound states');
                        END;
                  END{THEN}
               ELSE
                  BEGIN
                     announce('Error: Parser could not parse function');
                     AboutParser(ParserHelp);
                     Help(ParserHelp);
                     ReDo := True;
                  END;{ELSE}
            END;{ELSE}
         IF (ReDo = False) THEN
         BEGIN
         SelectViewPort(19);
         ClearViewPort;
         OpenViewPort(1);
         GraphBackColor := Red;
         OpenViewPort(15);
         SetColor(15);
         Print(1,1,'t = '+Num2Str(t,6));
         GraphBackColor := MyBackColor;
         SelectViewPort(1);
         SetColor(14);
         Draw_Well(ptype);
         SelectViewPort(20);
         ClearViewPort;
         OpenViewPort(2);
         SelectViewPort(2);
         Draw_Well(ptype);
         OpenViewPort(3);
         Solve_eigenvalues(ptype);
         Draw_Eigenstates;
         {Infinite_Well_Eigenvalues;}
         Prepare_Wave;
         t := 0;
         Evaluate_Psi(t);
         Psi_squared.MinMax(dummy1,psimin,dummy2,psimax);
         PsiScale := sqrt(psimax);
         PsisquaredScale := psimax;
         IF (ptype = 1) THEN
            BEGIN
               DefineScale(7, -L-3/40*L,2*L+3/40*L,-2*psisquaredscale,5*psisquaredscale/3);
               DefineScale(8, -L-3/40*L,2*L+3/40*L,-1*psiscale,8*psiscale/3);
               deltat := ABS(0.1/energy.value(1));
            END
         ELSE
            BEGIN
               Extra := 3*L/40;
               DefineScale(7,LowLimit-L-3/40*L,HighLimit+L+3/40*L,
                                  -2*psisquaredscale,5*psisquaredscale/3);
               DefineScale(8,LowLimit-L-3/40*L,HighLimit+L+3/40*L,
                                     -1*psiscale,8*psiscale/3);
               deltat := ABS(0.1/(eigenval[1]-VMin));
            END;
         SelectViewPort(19);
         ClearViewPort;
         SelectViewPort(1);
         OpenViewPort(1);
         GraphBackColor := Red;
         OpenViewPort(15);
         SetColor(15);
         Print(1,1,'t = '+Num2Str(t,6));
         GraphBackColor := MyBackColor;
         SelectViewPort(1);
         SetColor(14);
         Draw_Well(ptype);
         {Label_Psi_Graph(Components);}
         Plot_Functions;
         END;{THEN}
         END;{THEN}
      END;{DO}
   END;


PROCEDURE Change_Time_Step;

   BEGIN
      WITH Time_Step_Screen DO
      BEGIN
         AcceptScreen;
         deltat := GetNumber(1);
      END;
   END;


PROCEDURE Reset_Time;

   BEGIN
      t := 0;
      Evaluate_Psi(0);
      Plot_Functions;
      SetColor(15);
      SelectViewPort(15);
      RubOut(5,1,6,Red);
      SetColor(15);
      Print(1,1,'t = '+num2Str(t,6));
   END;


PROCEDURE Step_Time;

   BEGIN
      t := t + deltat;
      Evaluate_Psi(t);
      Plot_Functions;
      SetColor(15);
      SelectViewPort(15);
      RubOut(5,1,6,Red);
      SetColor(15);
      Print(1,1,'t = '+num2Str(t,6));
   END;


PROCEDURE Run_Time;

   VAR
      Stop_Key: Boolean;
      Key_Num: Byte;

   BEGIN
      Key_Num := 0;
      WHILE Key_Num <> 3 DO
         BEGIN
            Step_Time;
            CheckForEvents;
            {Stop_Key := Simple_Well_Disp.HotKeys.Pressed(Key_Num);}
         END;
   END;


PROCEDURE Initialize_Potential;

   VAR
      i: Integer;

   BEGIN
      ptype := 1;
      v1 := 150;
      v2 := 150;
      L := 1;
      LowLimit := 0;
      DefineScale(7, -L-3/40,2*L+3/40,-6,5);
      DefineScale(8,-L-3/40,2*L+3/40,-3,8);
      VMin := 150;
      Vmax := 150;
      FOR i := 1 TO Length DO
         BEGIN
            IF x.value(i)<0 THEN V.put(i,150)
            ELSE IF x.value(i)<=L THEN V.Put(i,0)
            ELSE V.Put(i,150);
         END;
      OpenViewPort(3);
      SelectViewPort(1);
      SetColor(14);
      Draw_Well(ptype);
      Solve_Eigenvalues(ptype);
      Draw_Eigenstates;
      {Infinite_Well_Eigenvalues;}
   END;


PROCEDURE Initialize_Wave;

   BEGIN
      deltat := 0.01;
      Components := True;
      t := 0.00;
      n[1] := 1;
      n[2] := 2;
      n[3] := 3;
      Amplitude[1] := 1;
      Amplitude[2] := 1;
      Amplitude[3] := 1/3;
      Normalize_Amplitude;
      Prepare_Wave;
      Evaluate_Psi(0);
      Psi_squared.MinMax(dummy1,psimin,dummy2,psimax);
      PsiScale := sqrt(psimax);
      PsisquaredScale := psimax;
      Plot_Functions;
   END;


PROCEDURE Process;

   BEGIN
      WITH Simple_Well_Disp DO
      WITH Message DO
      BEGIN
         Open;
         Parser.init;
         HelpScreenInit;
         done := False;
         t:= 0;
         Components := False;
         DefineScale(12,0,1,0,1);
         Initialize_Potential;
         Initialize_Wave;
         DefineViewPort(11,0.05,1.00,0.82,0.86);
         SelectViewPort(11);
         SetColor(15);
         Print(17,1,'Wavepacket');
         Print(52,1,'Eigenfunctions');
         Print(69,1,'Energy');
{         Label_Psi_Graph(Components);}
     END;
   END;


BEGIN
   {$IFDEF MAIN}
   {$ENDIF}
   Initialize_Vectors;
   {ClearMUPPETPort;}
   Initialize_Display_Object;
   Define_Input_Screens;
   QMInterface.HotKeys.Key[4] := 'F6-Hide Real';
   QMInterface.HotKeys.Display;
   Process;
   GraphBackColor := Red;
   OpenViewPort(15);
   SetColor(15);
   Print(1,1,'t = '+Num2Str(t,6));
   GraphBackColor := MyBackColor;
   WITH QMInterface DO
   WITH Message DO
      BEGIN
         Menu.RowActivate(3,2,False);
         Next := 4;
         Paused := True;
         Quit := False;
         Change := 0;
         WHILE (NOT Quit) DO
            BEGIN
               IF ((NOT QueryEvent(Message)) AND (NOT Paused)) THEN Step_Time
               ELSE IF Change <> 0 THEN
                  BEGIN
                     CASE Change OF
                        1: Change_Wave_Packet;
                        3: BEGIN
                              ReDo := True;
                              WHILE (ReDO = True) DO Change_Well;
                           END;
                        10: Help(CurrentHelpScreen);
                        14: Toggle_Components(Components);
                        15: Integrate;
                        13: BEGIN
                               Reset_Time;
                            END;
                     END;
                     Change := 0;
                  END;
            END;
         Menu.RowActivate(3,2,True);
      END;
   Free_Vectors;
   Well_Parameters.done;
   Wave_Components.done;
   Time_Step_Screen.done;
   IntegrateScreen.done;
   {Simple_Well_Disp.menu.done;}
   {$IFDEF MAIN}
   CUPSdone;
   {$ENDIF}
{$IFNDEF MAIN}
END;
{$ENDIF}

END.  { End of file QM_SWELL.PAS }


