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

{ Start of file QM_FPTD.PAS }

{ ----------------------------------------------------------------------
   Program Title:    Free Particles - Time Dependent
   Author:           Douglas E. Brandt
   Version:          0.11
   Program Summary:  This is one section of the CUPS Modern Physics
                     One Dimensional Wave Mechanics Program.  It displays
                     free Gaussian wave packets and animates their time
                     evolution.  It can display the exact evolution of a
                     true Gaussian wave packet according to the Schrodinger
                     equation.  In order to give insight into phase and
                     group velocity, the time propagation of three momentum
                     eigenfunction components of the wave packet are also
                     displayed.  To give insight into dispersion relations,
                     the gaussian wave packet can be modeled as a
                     superposition of momentum eigenwaves.  This
                     superposition of momentum eigenwaves can be propagated
                     through time subject to a choice of dispersion relations.
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

UNIT QM_FPTD;

INTERFACE

{$ENDIF}


USES Crt, Dos, Graph, CUPSmupp, CUPS, CUPSgrph, CUPSfunc,CUPSproc, CUPSgui,
        Objects,QM_func,QM_Ifac;

{$IFNDEF MAIN}

PROCEDURE Free_Particle_Time(QMInterface: TInterface; VAR Message: MessageRec);

IMPLEMENTATION

PROCEDURE Free_Particle_Time(QMInterface: Tinterface; VAR Message: MessageRec);

{$ENDIF}

   TYPE
      Function3 = FUNCTION(x,y,z: Real): Real;
      Int3 = ARRAY [1..3] OF REAL;

   CONST
      Length = 100;
      Num_Components = 10;
      kmin = 0.1;
      Exact_Gaussian = 0;
      Random_Gaussian = 1;
      Periodic = 2;
      Schrodinger = 1;
      EM = 2;
      Cubic = 3;
      User = 4;
      PsiSquaredColor = 13;
      RealPsiColor = 11;
      ImagPsiColor = 10;


   VAR
      Psi_Squared, Real_Psi, Imag_Psi, Phi_Squared, Phi: DVector;
      Old_Real_Psi, Old_Imag_Psi, Old_Psi_Squared: DVector;
      Real_Psi_1, Imag_Psi_1: DVector;
      Real_Psi_2, Imag_Psi_2: DVector;
      Real_Psi_3, Imag_Psi_3: DVector;
      Psi_0: ARRAY [1..Num_Components] OF CDVector;
      Old_Psi: ARRAY [1..3] OF CDvector;
      Component: ARRAY [1..3] OF CDvector;
      Component_0: ARRAY [1..3] OF CDVector;
      x,xvec: DVector;
      k,xp: DVector;
      i, done, return, seed: Integer;
      knum: Byte;
      Free_Time_Dep_Disp: Base_Display;
      Change_position_parms,Select_Dispersion_Screen: TInputScreen;
      Time_Step_Screen: TInputScreen;
      center, width, m, k0, t: Real;
      xcenter, xwidth, pcenter, pwidth, deltax, deltat: Real;
      Components: Boolean;
      Component_Set: Int3;
      Dispersion_Func: FunctionXY;
      One: Complex;
      Distribution_Type: Integer;
      Packet_String: ARRAY [0..2] OF String;
      Current_Packet_String, Current_Dispersion_Type: String;
      Dummy: DVector;
      Oldx: ARRAY [1..3] OF Real;
      CurrentHelpScreen: HelpScrType;
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
      FOR i := 1 TO 3 DO
         BEGIN
            Old_Psi[i].init(Length);
            Component[i].init(Length);
         END;
      Phi_Squared.init(Length);
      Phi.init(Length);
      FOR i := 1 TO Num_Components DO Psi_0[i].init(Length);
      k.init(Num_Components);
      x.init(Length);
      FOR i := 1 TO Length DO x.put(i, 30 * i/Length - 5);
      xp.init(Length);
      FOR i := 1 TO 3 DO Component_0[i].init(Length);
      Dummy.init(Length);
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
      FOR i := 1 TO 3 DO
         BEGIN
            Old_Psi[i].Free;
            Component[i].Free;
            Component_0[i].Free;
         END;
      Phi_Squared.Free;
      Phi.Free;
      FOR i := 1 TO Num_Components DO Psi_0[i].Free;
      k.Free;
      x.Free;
      xp.Free;
      Dummy.Free;
   END;


PROCEDURE Initialize_Display_Object;

   BEGIN
      WITH Free_Time_Dep_Disp DO
         BEGIN
            Title := 'Free Particles - Time Dependent';
            Window_num := 1;
            Window_corner[1,1].x := 0.07;
            Window_corner[1,1].y := 0.10;
            Window_corner[1,2].x := 0.57;
            Window_corner[1,2].y := 0.81;
            DefineViewPort(11,0.05,1.00,0.82,0.86);
            DefineViewPort(3,0.65,1.00,0.44,0.64);
            DefineViewPort(6,0.60,1.00,0.40,0.69);
            DefineViewPort(4,0.65,1.00,0.10,0.35);
            DefineViewPort(16,0.60,1.00,0.06,0.39);
            Init := True;
         END;
   END;

PROCEDURE HelpScreenInit;

   BEGIN
         currentHelpScreen[01] := '     HELP FOR TIME DEPENDENT FREE PARTICLES       ';
         currentHelpScreen[02] := '                                                  ';
         currentHelpScreen[03] := '    The wavefunction parameters can be adjusted   ';
         currentHelpScreen[04] := ' by selecting "Wavefunction Parameters" from the  ';
         currentHelpScreen[05] := ' "Parameters" menu.  ';
         currentHelpScreen[06] := '  ';
         currentHelpScreen[07] := '    The initial width and mean momentum of the';
         currentHelpScreen[08] := ' wave packet can be adjusted using the sliders';
         currentHelpScreen[09] := ' shown on the display.';
         currentHelpScreen[10] := ' ';
         currentHelpScreen[11] := '   ';
         currentHelpScreen[12] := ' ';
         currentHelpScreen[13] := '';
         currentHelpScreen[14] := '   ';
         currentHelpScreen[15] := ' ';
         currentHelpScreen[16] := '  ';
         currentHelpScreen[17] := '   ';
         currentHelpScreen[18] := ' ';
         currentHelpScreen[19] := ' ';
         currentHelpScreen[20] := '   ';
         currentHelpScreen[21] := ' ';
         currentHelpScreen[22] := ' ';
         currentHelpScreen[23] := '                                                 ';
         currentHelpScreen[24] := '    Press any Key or Click Mouse to continue.     ';
         currentHelpScreen[25] := '                                                  ';

   END;

PROCEDURE Define_Input_Screens;

   BEGIN
      WITH Change_Position_Parms DO
      BEGIN
         init;
         DefineInputPort(0.5,0.9,0.25,0.80);
         Loadline('                            ');
         Loadline('       Delta x =  {    }    ');
         Loadline('                            ');
         Loadline('       Mean p = {    }      ');
         Loadline('                            ');
         Loadline('       [  Ok  ] [Cancel]    ');
         SetNumber(1,1.0);
         SetNumber(2,0.0);
      END;
      WITH Select_Dispersion_Screen DO
      BEGIN
         init;
         DefineInputPort(0.5,0.9,0.25,0.80);
         Loadline('                              ');
         Loadline('  Select Dispersion =  { }    ');
         Loadline('                              ');
         Loadline('    1 = EM Wave               ');
         Loadline('    2 = Schrodinger Wave      ');
         Loadline('    3 = Cubic                 ');
         Loadline('    4 = User Defined          ');
         Loadline('      [  Ok  ] [Cancel]       ');
         SetNumber(1,2);
      END;                           
      WITH Time_Step_Screen DO
      BEGIN
         init;
         DefineInputPort(0.5,0.9,0.25,0.80);
         Loadline('                              ');
         Loadline('     Time step = {     }      ');
         Loadline('                              ');
         Loadline('      [  Ok  ] [Cancel]       ');
         SetNumber(1,0.01);
      END;               
   END;

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


PROCEDURE Write_Info;

   BEGIN
     { OpenViewPort(5);
      SelectViewPort(5);
      SetColor(15);  }
     { Print(2,2,'Mean k = ' + NumStr(k0,4,2));
      Print(2,4,'Sigma k = ' + NumStr(pwidth,4,2));}
      {Print(2,6,'Dispersion:');
      Print(7,7,Current_Dispersion_Type);
      Print(2,9, 'Wave Packet Type:');
      Print(7,10, Current_Packet_String); }
   END;

PROCEDURE Plot_Components;

   VAR
      i: Integer;
      y1,y2: Real;

   BEGIN
      {FOR i := 1 TO 3 DO
         BEGIN
            SelectViewPort(1);
            SelectScale(6);
            SetColor(GraphBackColor);
            y1 := 5.2-2.1*i;
            y2 := 3.2 - 2.1*i;
            HideCursor;
            PlotLine(Oldx[i],y1,Oldx[i],y2);
            PlotDVectors(x,Old_Psi[i].Re, 1, Length);
            PlotDVectors(x,Old_Psi[i].Im, 1, Length);
            SetColor(RealPsiColor);
            PlotDVectors(x, Component[i].Re, 1, length);
            SetColor(ImagPsiColor);
            PlotDVectors(x, Component[i].Im, 1, Length);
            Old_Psi[i].Equate(One,Component[i]);
            IF Component_Set[i] <> 0 THEN
               Oldx[i] := Dispersion_Func(Component_Set[i],m)/Component_Set[i]*t
            ELSE Oldx[i] := 0;
            SetColor(15);
            PlotLine(Oldx[i],y1,Oldx[i],y2);
            ShowCursor;
         END;}
   END;


PROCEDURE Plot_functions(Real_Psi,Imag_Psi,Psi_Squared: DVector);

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
      DefineScale(12,0,1,0,1);
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
      ShowCursor;
   END;


PROCEDURE Evaluate(vector: Dvector; f: Function3; width, center: Real);

   VAR
      i: Integer;

   BEGIN
      FOR i := 1 TO vector.length DO
         BEGIN
            vector.put(i, f(xvec.value(i), width, center));
         END;
   END;


PROCEDURE Evaluate_Psi(t: Real);

   VAR
      B2, B4, P2, X2, T2, A, B, C, Q, R, RQ, S, SgnT, COSS, SINS,f,g: Single;
      j: Integer;
      RTRTPI,RTRT2,Root2: Single;

   BEGIN

      B2 := Sqr(pwidth);
      B4 := Sqr(B2);
      P2 := Sqr(pcenter);
      T2 := Sqr(t);
      Root2 := Sqrt(2);
      RTRTPI := Sqrt(Sqrt(pi));
      RTRT2  := Sqrt(Sqrt(2));
      IF t <> 0 THEN sgnT := t/Sqrt(t2) ELSE sgnT := 0;
      Q := 4 * B4 * T2 + 1;
      RQ := Sqrt(Q);
      A :=  Sqrt(RQ - 1) * SgnT;
      B := Sqrt(RQ + 1);
      FOR j := 1 TO LENGTH DO
         BEGIN
            X2 := Sqr(x.value(j));
            R := 4 * B4 * t * X2 + 2 * pcenter * x.value(j) - P2 * t;
            S := R/(2 * Q);
            SINS := Sin(S);
            COSS := Cos(S);
            f := -X2 + 2 * pcenter * t * x.value(j) - P2 * T2;
            g := f*B2/RQ;
            IF g < -20 THEN C := 0
            ELSE C := exp(g)/RQ;
            Real_Psi.put(j, C * ( A * COSS + B * SINS )/Root2);
            Imag_Psi.put(j, C * ( A * SINS - B * COSS )/Root2);
            Psi_Squared.put(j, ( Sqr(C) * RQ));
         END;
   END;


FUNCTION Zero(x, sigma, center: Real): Real;

   BEGIN
      Zero := 0;
   END;


FUNCTION Gaussian(x, sigma, center: Real): Real;

   VAR
      A: Real;

   BEGIN
      A := Exp(-1.0 * Sqr(x - center)/(2 * Sqr(sigma)));
      Gaussian := A/Sqrt(2 * Pi * sigma);
   END;


PROCEDURE Get_Momentum_Distribution(k: DVector; center, sigma: Real);

   VAR
      i: Integer;
      x, y, r: Real;

   BEGIN
      FOR i := 1 TO k.Length DO
         BEGIN
            r := 2;
            WHILE r >= 1 DO
               BEGIN
                  x := 2 * Urand(seed) - 1;
                  y := 2 * Urand(seed) - 1;
                  r := Sqr(x) + Sqr(y);
               END;
           k.put(i, x * Sqrt(2) * sigma * Sqrt( -2 * ln(r)/r) + center);
         END;
   END;



PROCEDURE Evaluate_Psi_2(t: Real; w: FunctionXY);

   VAR
      i: Integer;
      Sum_Components: CDVector;
      z: Complex;
      Ei: Real;

   BEGIN
      BEGIN
         Sum_Components.init(Length);
         Sum_Components.MultReal(0);
         FOR i := 1 TO Num_Components DO
            BEGIN
               Ei := w(k.value(i),m);
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
      END;
   END;


PROCEDURE Plot_Dispersion(Dispersion_Function: FunctionXY);

   VAR
      Old_k,Old_w,k,w: Real;
      i: Integer;

   BEGIN
      Old_k := 0;
      Old_w := 0;
      SetColor(15);
      HideCursor;
      SelectViewPort(16);
      ClearViewPort;
      OpenViewPort(4);
      DefineScale(4, 0, 4, 0, 4);
      SelectScale(4);
      SelectViewPort(4);
      Axis(0,0,1,1);
      SetColor(15);
      Print(2,1,'w');
      Print(27,6,'k');
      SetColor(LightRed);
      FOR i := 1 TO 50 DO
         BEGIN
            k := 4*i/50;
            w := Dispersion_Function(k,m);
            PlotLine(Old_k,Old_w,k,w);
            Old_k := k;
            Old_w := w;
         END;
      ShowCursor;
   END;


PROCEDURE Evaluate_Components(t: Real; Component_Set: Int3;
                                    w: FunctionXY);

   VAR
      i: Integer;
      Ei,q : Real;
      z: Complex;

   BEGIN
      FOR i := 1 TO 3 DO
         BEGIN
            q := Component_Set[i];
            Ei := w(Component_Set[i],m);
            z.Re := Cos(Ei*t);
            z.Im := Sin(-Ei*t);
            Component[i].equate(z,Component_0[i]);
            z.Re := 4.2-2.1*i;
            z.Im := 4.2-2.1*i;
            Component[i].AddScalar(z);
         END;
   END;


PROCEDURE Select_Components(VAR Component_Set: Int3);

   BEGIN
      Component_Set[1] := k0;
      Component_Set[2] := k0 + pwidth;
      Component_Set[3] := k0 - pwidth;
   END;


PROCEDURE Select_Dispersion_Type(Input: Integer;
                                 VAR Dispersion_Function: FunctionXY);


   BEGIN
      CASE Input OF
         Schrodinger:
            BEGIN
               Dispersion_Function := wSchrodinger;
               Current_Dispersion_Type := 'Schrodinger';
            END;
         EM:
            BEGIN
               Dispersion_Function := wEM;
               Current_Dispersion_Type := 'E&M Waves';
            END;
         Cubic:
            BEGIN
               Dispersion_Function := wCubic;
               Current_Dispersion_Type := 'Cubic';
            END;
         User:
            BEGIN
               Dispersion_Function := wUser;
               Current_Dispersion_Type := 'User Defined';
            END;
      END;
      Plot_Dispersion(Dispersion_Function);
      Write_Info;
   END;


PROCEDURE Calculate_Eigenwave(k: Real;
                               VAR Real_Psi,Imag_Psi,Psi_Squared:DVector);

   VAR
      i: Integer;

   BEGIN
      FOR i:= 1 TO Length DO
         BEGIN
            Real_Psi.put(i, cos(k*x.value(i)));
            Imag_Psi.put(i, sin(k*x.value(i)));
            Psi_Squared.put(i, Sqr(Real_Psi.value(i))+Sqr(Imag_Psi.value(i)));
         END;
   END;

      
PROCEDURE Evaluate_Component_0;

   VAR
      I: Integer;

   BEGIN
      FOR i:= 1 TO 3 DO
         BEGIN
            Calculate_Eigenwave(Component_Set[i],Component_0[i].Re,
                                 Component_0[i].Im,Dummy);
         END;
   END;


PROCEDURE Construct_Phi;

   VAR
      i: Integer;
      s: Real;

   BEGIN
      SetColor(15);
      SelectViewPort(6);
      ClearViewPort;
      Print(7,1,'Momentum Distribution');
      OpenViewPort(3);
      DefineScale(8,k0-4*pwidth,k0+4*pwidth,0.0,1.1);
      SelectScale(8);
      SelectViewPort(3);
      Print(28,1,'Phi Squared');
      {Axis(0,0,1,1);}
      FOR i:= 1 TO Length DO
         BEGIN
            xp.put(i, -4*pwidth + i/Length*8*Pwidth + k0);
            s := Sqr((xp.value(i)-k0)/(2*pwidth));
            IF s < 28 THEN s := Exp(-s)
            ELSE s := 0;
            Phi.put(i, s);
            Phi_Squared.put(i, Sqr(s));
         END;
      SetColor(15);
      Autoscaling(8,xp,Phi_Squared);
      SelectScale(8);
      SetColor(PsiSquaredColor);
      PlotDVectors(xp,Phi_Squared,1,Length);
   END;


PROCEDURE Construct_WavePacket;

   VAR
      i,n,m: Integer;
      Amp,d: Real;
      p: Complex;
      q: Complex;
      s, min, max: Real;


   BEGIN
      Construct_Phi;
      IF Distribution_Type = Periodic THEN
         FOR i := 1 TO Num_Components DO
             k.put(i,kmin*Trunc(k0/kmin-Num_Components/2+i))
      ELSE Get_Momentum_Distribution(k,k0,pwidth);
      IF Distribution_Type = Exact_Gaussian THEN Evaluate_Psi(0)
      ELSE
         BEGIN
            SetColor(1);
            FOR i := 1 TO Length DO
               BEGIN
                  Real_Psi.put(i,0);
                  Imag_Psi.put(i,0);
               END;
            FOR i := 1 TO Num_Components DO
               BEGIN
                  IF Distribution_Type = Random_Gaussian
                  THEN Amp := 1/Num_Components
                  ELSE
                     BEGIN
                        s := Sqr((k.value(i)-k0)/(2*pwidth));
                        IF s < 28 THEN
                           BEGIN
                              d := Exp(-s);
                              Amp := d*kmin/(Sqrt(pi)*2*pwidth);
                           END
                        ELSE Amp := 0;
                     END;
                  Calculate_Eigenwave(k.value(i), Psi_0[i].Re, Psi_0[i].Im,
                                                              Psi_Squared);
                  Psi_0[i].MultReal(Amp);
                  Real_Psi.AddVector(1,Psi_0[i].Re);
                  Imag_Psi.AddVector(1,Psi_0[i].Im);
                  IF Distribution_Type = Random_Gaussian
                  THEN PlotLine(k.value(i),0,k.value(i),0.7)
                  ELSE PlotLine(k.value(i),0,k.value(i),Sqr(d));
               END;
            FOR i := 1 TO Length DO
               Psi_Squared.put(i,Sqr(Real_Psi.value(i))+
                                 Sqr(Imag_Psi.value(i)));
         END;
      Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
   END;


PROCEDURE Change_Distribution(New_Type: Integer;
                                    VAR Distribution_Type: Integer);

   BEGIN
      Distribution_Type := New_Type;
      WITH Free_Time_Dep_Disp DO
         BEGIN
            IF Distribution_Type = Exact_Gaussian THEN
               BEGIN
                  Dispersion_Func := wSchrodinger;
                  Plot_Dispersion(Dispersion_Func);
               END
         END;
      t := 0;
      Construct_WavePacket;
      Evaluate_Components(t,Component_Set,Dispersion_Func);
      Plot_Components;
      Current_Packet_String := Packet_String[New_Type];
      Write_Info;
   END;

{PROCEDURE Change_psi;

   VAR
      i: Integer;

   BEGIN
      WITH Change_Position_Parms DO
      BEGIN
         AcceptScreen;
         xwidth  := GetNumber(1);
         k0 := GetNumber(2);
      END;
      pwidth := Abs(1/(2*xwidth));
      Construct_WavePacket;
      Write_Info;
   END;  }


PROCEDURE Reset_Time;

   BEGIN
      t := 0;
      IF Distribution_Type = Exact_Gaussian THEN Evaluate_Psi(t)
      ELSE Evaluate_Psi_2(t,Dispersion_Func);
      Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
      Evaluate_Components(t,Component_Set,Dispersion_Func);
      Plot_Components;
   END;


PROCEDURE ProcessSliders;

   VAR
      i: Integer;

   BEGIN
      WITH QMInterface DO
      BEGIN
         xwidth := Sliders.value(2);
         k0 := Sliders.value(1);
         pcenter := k0;
      END;
      pwidth := Abs(1/(2*xwidth));
      Construct_Phi;
      Select_Components(Component_Set);
      Evaluate_Component_0;

      Reset_Time;
{      Construct_WavePacket;
      Write_Info;}
   END;



{PROCEDURE Label_Psi_Graph(Components: Boolean);

   BEGIN
      SelectViewPort(1);
      SetColor(15);
      HideCursor;
      Print(28,1,'Psi Squared');
      IF Components = True THEN
         BEGIN
            SetColor(1);
            Print(30,4,'Real Psi');
            SetColor(2);
            Print(30,5,'Imag Psi');
         END;
      ShowCursor;
   END; }



PROCEDURE Toggle_Components(VAR Components:Boolean);

   BEGIN
      Components := NOT(Components);
      IF Components THEN QMInterface.HotKeys.Key[4] := 'F6-Hide Real'
      ELSE QMInterface.HotKeys.Key[4] := 'F6-Show Real';
      QMInterface.HotKeys.Display;
      SelectViewPort(2);
      ClearViewPort;
      OpenViewPort(1);
      IF Components = True THEN Autoxscaling(7,x,-1.1,1.1)
      ELSE Autoxscaling(7,x,-0.1,1.05);
      Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
   END;



PROCEDURE Step_Time;

   BEGIN
      t := t + deltat;
      IF Distribution_Type = Exact_Gaussian THEN Evaluate_Psi(t)
      ELSE Evaluate_Psi_2(t,Dispersion_Func);
      Plot_Functions(Real_Psi,Imag_Psi,Psi_Squared);
      Evaluate_Components(t,Component_Set,Dispersion_Func);
      Plot_Components;
   END;


PROCEDURE Change_Time_Step;

   BEGIN
      WITH Time_Step_Screen DO
      BEGIN
         AcceptScreen;
         deltat := GetNumber(1);
      END;
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
            {Stop_Key := Free_Time_Dep_Disp.HotKeys.Pressed(Key_Num);}
         END;
   END;

PROCEDURE CreateSliders;

   BEGIN
      WITH QMInterface DO
         BEGIN
            Sliders.Init;
            Sliders.Create(1,-4,4,2,0.6,0.8,1.0,2,'-4.0','4.0','Mean k',False,1);
            Sliders.Create(2,0.2,4,0.5,0.6,0.73,1.0,2,'0.2','4.0','Initial Delta x',False,1);
            Sliders.DrawAll;
         END;
    END;


PROCEDURE Process;

   VAR j: Integer;

   BEGIN
      WITH Free_Time_Dep_Disp DO
      WITH Message DO
      WITH QMInterface DO
      BEGIN
         Open;
         QMInterface.Menu.RowActivate(3,1,False);
         QMInterface.Menu.RowActivate(3,2,False);
         QMInterface.Menu.RowActivate(3,3,False);
         One.Re := 1;
         One.Im := 0;
         return := 0;
         xcenter := 0;
         xwidth := 1;
         pcenter := 2;
         pwidth := 1/(2*xwidth);
         deltax := 0.5;
         deltat := 0.1;
         t := 0;
         k0 := 2;
         m := 1;
         seed := -1;
         CreateSliders;
         FOR j := 1 TO 3 DO Oldx[j] := 0;
         Packet_String[0] := 'Exact';
         Packet_String[1] := 'Random';
         Packet_String[2] := 'Periodic';
         Current_Dispersion_Type := 'Schrodinger';
         Distribution_Type := Exact_Gaussian;
         Dispersion_Func := wSchrodinger;
         Plot_Dispersion(Dispersion_Func);
         SelectViewPort(11);
         SetColor(15);
         Print(10,1,'Wavepacket and Components');
         Components := True;
         HelpScreenInit;
         Select_Components(Component_Set);
         Evaluate_Component_0;
         SetColor(15);
         SelectViewPort(6);
         Print(7,1,'Momentum Distribution');
         Print(31,6,'p');
         Print(32,7,'p');
         Print(33,8,'p');
         SelectViewPort(16);
         Print(14,1,'Dispersion');
         Print(30,9,'k');
         DefineViewPort(2,0.0,0.57,0.06,0.81);
         SelectViewPort(1);
         Autoxscaling(7,x,-1.1,1.1); {DefineScale(7,-5,25.1,-1.1,1.1);}
         DefineScale(6,-5,25.1,-3.2,3.2);
         Get_Momentum_Distribution(k,k0,pwidth);
         {Label_Psi_Graph(Components);}
         Construct_WavePacket;
         Evaluate_Components(t,Component_Set,Dispersion_Func);
         Plot_Components;
         Write_Info;
      END;
   END;


BEGIN
   {$IFDEF MAIN}
   {$ENDIF}
{   ClearMUPPETPort;}
   Initialize_Vectors;
   Initialize_Display_Object;
   Define_Input_Screens;
   QMInterface.HotKeys.Key[4] := 'F6-Hide Real';
   QMInterface.HotKeys.Active[5] := False;
   QMInterface.HotKeys.Display;
   Process;
   WITH QMInterface DO
   WITH Message DO
      BEGIN
      Paused := True;
      Quit := False;
      WHILE (NOT Quit) DO
         BEGIN
            IF (NOT QueryEvent(Message) AND (NOT Paused)) THEN Step_Time
            ELSE IF Change<> 0 THEN
               BEGIN
                  CASE Change OF
                     1: ;
                     10: Help(CurrentHelpScreen);
                     14: Toggle_Components(Components);
                     13: Reset_Time;
                     20: ProcessSliders;
                  END;
                  Change := 0;
               END;
         END;
      END;
   Free_Vectors;
   QMInterface.Menu.RowActivate(3,1,True);
   QMInterface.Menu.RowActivate(3,2,True);
   QMInterface.Menu.RowActivate(3,3,True);
   QMInterface.Sliders.Done;
   Change_position_parms.done;
   Select_Dispersion_Screen.done;
   Time_step_screen.done;
   QMInterface.HotKeys.Active[5] := True;
   QMInterface.HotKeys.Display;
   {$IFDEF MAIN}
   CUPSdone;
   {$ENDIF}
{$IFNDEF MAIN}
END;
{$ENDIF}

END.  { End of file QM_FPTD.PAS }


