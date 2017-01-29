PROGRAM EVOLVE;

{******************************************************************}
{*    Computes the Evolution of a Star                            *}
{*                                                                *}
{*        R.T. Kouzes   Copyright @ 1991, 1992, 1993, 1994, 1995  *}
{*        R.T. Kouzes   rt_kouzes@pnl.gov                         *}
{*        CUPS Project  Evolve REV 1.4   December  6, 1992        *}
{*        CUPS Project  Evolve REV 1.5       June 29, 1993        *}
{*        CUPS Project  Evolve REV 1.6    October 17, 1993        *}
{*        CUPS Project  Evolve REV 2.0       June 26, 1994        *}
{*                                                                *}
{*     This program integrates 4 differential equations for the   *}
{*     star, using 2 boundary conditions at the surface and 2     *}
{*     boundary conditions at the center, determining 4 parameters*}
{*     for the star - central temperature, central density,       *}
{*     radius  and luminosity. Units are cgs.                     *}
{*        References are given in code by initials:               *}
{*          KW: Kippenhahn and Weigert "Stellar Structure"        *}
{*          BP: Bohdan Paczynski, Princeton U., private comm.     *}
{*          AC: A.N. "Cox Stellar Structure"                      *}
{******************************************************************}

{---------------------------------------------------------------------------}
{*----- The following is a list of all Proceedures in this Program  -------*}
{---------------------------------------------------------------------------}

{*PROCEDURE InitializeModel;                                      *}

{*-----      Graphics Proceedures                            -----*}

{*PROCEDURE LabelTicX;                                            *}
{*PROCEDURE LabelTicY;                                            *}
{*PROCEDURE InitializeOnePlot(xlabel,ylabel :string);             *}
{*PROCEDURE InitializeModelPlot(numplot :Integer);                *}
{*PROCEDURE InitializeMoreModelPlot(numplot :Integer);            *}
{*PROCEDURE InitializeCompositionPlot(numplot :Integer);          *}
{*PROCEDURE InitializeProtostarPlot;                              *}
{*PROCEDURE InitializeThermodynamicsPlot;                         *}
{*PROCEDURE InitializeHRPlot(LowT: Double);                       *}
{*PROCEDURE PlotDataModel(screen :Integer; x :Double; y :Double); *}
{*PROCEDURE ReplaceZero(VAR temp :String);                        *}

{*-----      Procedures for Physics Calculations           -------*}

{*PROCEDURE NuclearPower(VAR xx,yy,zz,zn,d,t :Double);            *}
{*PROCEDURE Opacity(VAR xx,yy,zz,d,t :Double);                    *}
{*PROCEDURE EquationOfState(VAR xx,yy,zz,d,t :Double);            *}
{*PROCEDURE ModifyComposition;                                    *}
{*PROCEDURE EvaluateEquations(VAR Params, Deriv :Vector5);        *}
{*PROCEDURE IntegralStep(VAR Parameters : Vector5);               *}
{*PROCEDURE Outward(Tcent, Dcent :Double; Core :Vector5; Error :Integer);*}
{*PROCEDURE Inward(Teff, Lum :Double; Surf :Vector5; Error :Integer);*}
{*PROCEDURE CorrectBoundary(VAR deriv :Array5x5);                 *}
{*PROCEDURE StellarModel(VAR Error, Loop :Integer);               *}
{*PROCEDURE PrepareForModelCalc;                                  *}
{*PROCEDURE InitializeProtostar;                                  *}
{*PROCEDURE FreeFall;                                             *}
{*PROCEDURE ProtostarEvolution;                                   *}

(*-----         Input Screen Menu Procedures                 -----*)

{*PROCEDURE SetUpHotKeys;                                         *}
{*PROCEDURE SetUpStagesHotKeys;                                   *}
{*PROCEDURE SetUpProtoHotKeys;                                    *}
{*PROCEDURE SetUpMyMenu;                                          *}
{*PROCEDURE LoadMainMenu;                                         *}
{*PROCEDURE LoadBoundaryConditionsMenu;                           *}
{*PROCEDURE LoadProtostarMenu;                                    *}
{*PROCEDURE LoadComputeMenu;                                      *}
{*PROCEDURE FillComputeMenu;                                      *}
{*PROCEDURE LoadResultsMenu;                                      *}
{*PROCEDURE LoadMoreResultsMenu;                                  *}
{*PROCEDURE LoadChemicalMenu;                                     *}
{*PROCEDURE FillChemicalMenu;                                     *}
{*PROCEDURE LoadExplainHRMenu;                                    *}
{*PROCEDURE LoadStagesMenu;                                       *}
{*PROCEDURE LoadExplainProtostarMenu;                             *}
{*PROCEDURE GetVal(VAR localcase :Integer);                       *}
{*PROCEDURE ShowSubMenu;                                          *}
{*PROCEDURE ShowCalcMenu;                                         *}
{*PROCEDURE ShowAcceptMenu;                                       *}
{*PROCEDURE Help1;                                                *}

{*-----      Menu Response handling Procedures               -----*}

{*PROCEDURE AboutEvolve;                                          *}
{*PROCEDURE GettingStarted;                                       *}
{*PROCEDURE FileMenuItem;                                         *}
{*PROCEDURE BoundaryConditions;                                   *}
{*PROCEDURE Compute;                                              *}
{*PROCEDURE Protostar;                                            *}
{*PROCEDURE PlotHayashi(m :Double);                               *}
{*PROCEDURE HRdiagram;                                            *}
{*PROCEDURE Stages;                                               *}
{*PROCEDURE HandleMenu;                                           *}
{*PROCEDURE HotKeyHandle (key :byte);                             *}
{* then comes the main program                                    *}

{---------------------------------------------------------------------------}
{*-----        Declare all global variables and constants           -------*}
{---------------------------------------------------------------------------}

{for Mac, delete crt,dos,printer}
USES  crt,dos,printer,graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph,
      evolvec;

{---------------------------------------------------------------------------}
{*-----      Initialization & physics moved to EVOLVEC.PAS            -----*}
{---------------------------------------------------------------------------}

{---------------------------------------------------------------------------}
(*-----             Input Screen Menu Procedures                      -----*)
{---------------------------------------------------------------------------}

PROCEDURE SetUpHotKeys;
 {main hot key setup}

BEGIN
   WITH HotKeys DO BEGIN
      init(2);
      key[1] := 'F1-Help';
      key[2] := 'F10-Menu';
{      key[3] := 'ESC-Main Menu';
      active[3] := false; }
   END;
   HotKeys.Display;
END;

{---------------------------------------------------------------------------}

PROCEDURE SetUpStagesHotKeys;
 {HR menu hot key setup}

BEGIN
   WITH HRHotKeys DO BEGIN
      Init(1);
      key[1] := 'ESC-Main Menu';
   END;
   HRHotKeys.Display;
END;

{---------------------------------------------------------------------------}

PROCEDURE SetUpProtoHotKeys;
 {Protostar menu hot key setup}

BEGIN
   WITH ProtoHotKeys DO BEGIN
      Init(3);
      key[1] := 'F2-Pause';
      key[2] := 'F3-Replot';
      key[3] := 'ESC-Main Menu';
   END;
   ProtoHotKeys.Display;
END;

{---------------------------------------------------------------------------}

PROCEDURE SetUpMyMenu;
 {main menu setup}

BEGIN
   MyMenu.Init;
   WITH MyMenu DO
   BEGIN
      column(1, 'File');
       row(1,1, 'About EVOLVE');
       row(1,2, 'About CUPS');
       row(1,3, 'Getting Started');
       row(1,4, 'File Save of Results');
       row(1,5, 'File Read of Previous Results');
       row(1,6, 'Configuration');
       row(1,7, 'Exit');
      column(2, 'Stages');
       row(2,1, 'M=1 Star Evolution');
       row(2,2, 'Explain Evolution');
      column(3, 'HR Diagram');
       row(3,1, 'Show Star Data HR Diagram');
       row(3,2, 'Show HR Diagram Trends');
       row(3,3, 'Explain HR Diagram');
      column(4, 'Boundary');
       row(4,1, 'Modify Initial Parameters');
       row(4,2, 'Modify Chemical Composition');
       row(4,3, 'Reinitialize Program');
      column(5, 'Protostar');
       row(5,1, 'Protostar Evolution');
       row(5,2, 'Show Protostar Results');
       row(5,3, 'Modify Model Parameters');
       row(5,4, 'Explain Protostar');
       row(5,5, 'Protostar Thermodynamics');
    {  row(5,6, '2nd Model Protostar Evolution');  disable this old model}
      column(6, 'Main Seq');
       row(6,1, 'Run One Time Step');
       row(6,2, 'Evolve From ZAMS');
       row(6,3, 'Continue Evolution');
       row(6,4, 'Show Model Results');
       row(6,5, 'Show More Results');
       row(6,6, 'Plot Model Results');
       row(6,7, 'Plot More Results');
       row(6,8, 'Plot Composition');
       row(6,9, 'Modify Model Params');
   END;
   MyMenu.Display;
END;

{---------------------------------------------------------------------------}

PROCEDURE AboutEvolve;
VAR
  IntroScr : HelpScrType;
  i : integer;
BEGIN
   IntroScr[01] := ' ';
   IntroScr[02] := ' ';
   IntroScr[03] := '            Stellar Evolution Model';
   IntroScr[04] := '              by Richard T. Kouzes';
   IntroScr[05] := '          Pacific Northwest Laboratory';
   IntroScr[06] := '        (c) 1995 John Wiley & Sons, Inc.';
   IntroScr[07] := '              EVOLVE Revision 2.0';
   IntroScr[08] := ' ';
   IntroScr[09] := ' This program models the evolution of a star from';
   IntroScr[10] := ' a primordial gas cloud to the main sequence and';
   IntroScr[11] := ' and through a series of hydrostatic equilibrium';
   IntroScr[12] := ' stages produced as the stellar composition ages';
   IntroScr[13] := ' due to nuclear reactions in the core of the star.';
   IntroScr[14] := ' ';
   IntroScr[15] := '  The menu choices are:';
   IntroScr[16] := ' File       - Exit and file handling';
   IntroScr[17] := ' Stages     - investigate the stages of evolution';
   IntroScr[18] := ' HR Diagram - show and explain the HR diagram';
   IntroScr[19] := ' Boundary   - the boundary conditions on the star';
   IntroScr[20] := ' Protostar  - run a protostar model calculation';
   IntroScr[21] := ' Main Seq   - run a ZAMS model calculation';
   FOR i := 22 To 25 DO IntroScr[i] := '';
   IntroScr[24] := ' See the Getting Started selection under File.';
   Help(IntroScr);
END;

{---------------------------------------------------------------------------}

PROCEDURE GettingStarted;
var
   MainScreen : HelpScrType;
BEGIN
   MainScreen[01] := ' ';
   MainScreen[02] := '           GETTING STARTED WITH EVOLVE';
   MainScreen[03] := ' ';
   MainScreen[04] := ' * Start by selecting the M=1 Star Evolution item';
   MainScreen[05] := '   under the Stages menu. This will step you';
   MainScreen[06] := '   through stages of evolution for a mass 1 star.';
   MainScreen[07] := ' ';
   MainScreen[08] := ' * Next, select the Show Star Data HR Diagram';
   MainScreen[09] := '   item under the HR Diagram menu. Refer to a';
   MainScreen[10] := '   text to understand the significance of this';
   MainScreen[11] := '   diagram. The Show HR Diagram Trends item is';
   MainScreen[12] := '   also valuable to examine and understand.';
   MainScreen[13] := ' ';
   MainScreen[14] := ' * Next, select the Protostar Evolution item';
   MainScreen[15] := '   in the Protostar menu. The model will show';
   MainScreen[16] := '   the collapse of the cloud proceeding from';
   MainScreen[17] := '   the inside to the outside. Pause the collapse';
   MainScreen[18] := '   and try to understand the significance of';
   MainScreen[19] := '   each plot.';
   MainScreen[20] := ' ';
   MainScreen[21] := ' * Next, select the Evolve From ZAMS item in the';
   MainScreen[22] := '   Main Sequence menu. You will see the program';
   MainScreen[23] := '   take the star through a time sequence of';
   MainScreen[24] := '   convergences. Note the changes occuring in';
   MainScreen[25] := '   this time sequence.';
   help(mainScreen);
END;

{---------------------------------------------------------------------------}

PROCEDURE ModifyBoundaryConditions;
Var
   BoundaryConditions : TInputScreen;
BEGIN
   {new(BoundaryConditions);}       {needed on Mac}
   WITH BoundaryConditions DO
   BEGIN
      init;
      DefineInputPort(0.15,0.85,0.10,0.90);
      LoadLine('                 BOUNDARY CONDITIONS');
      LoadLine(' ');
      LoadLine('  The stellar model is governed by four differential');
      LoadLine('  equations which require four boundary conditions');
      LoadLine('  be specified. Two of these conditions are at the');
      LoadLine('  center of the star (r=0) where the integral mass');
      LoadLine('  and the Luminosity are both zero. Two are at the');
      LoadLine('  surface of the star where the integral mass of the');
      LoadLine('  star equals the specified mass (where the density');
      LoadLine('  has dropped to a low value). You must specify the');
      LoadLine('  mass and make a guess at four initial conditions:');
      LoadLine(' ');
      LoadLine('    Mass (units of solar mass)      : {         }');
      LoadLine('    Central temperature (K)         : {         }');
      LoadLine('    Central density (g/cc)          : {         }');
      LoadLine('    Luminosity (units of solar Lum) : {         }');
      LoadLine('    Surface temperature (K)         : {         }');
      LoadLine(' ');
      LoadLine('               [  Ok  ]        [Cancel]');

      SetNumber(1,Mass);
      SetNumber(2,TCentral);
      SetNumber(3,DCentral);
      SetNumber(4,Luminosity);
      SetNumber(5,Tsurface);
      SetNumberLimits(1,0.001,10e4); {M}
      SetNumberLimits(2,1000,10e10); {Tc}
      SetNumberLimits(3,0.001,10e10);{D}
      SetNumberLimits(4,0.00001,10e10);{L}
      SetNumberLimits(5,1.0,10e10);  {Ts}
      Accept;
      mass          := GetNumber(1);
      Tcentral      := GetNumber(2);
      Dcentral      := GetNumber(3);
      Luminosity    := GetNumber(4);
      Tsurface      := GetNumber(5);
      Teffective    := Tsurface*TwoTo14;
      ComputedModel := 0;
      done;
   END;
   {dispose(BoundaryConditions);}       {needed on Mac}
END;

{---------------------------------------------------------------------------}

PROCEDURE ProtostarInputScreen;
VAR
   ProtostarScreen : TInputScreen;
BEGIN
   {new(ProtostarScreen);}       {needed on Mac}
   WITH ProtostarScreen DO
   BEGIN
      init;
      DefineInputPort(0.15,0.85,0.10,0.90);
      LoadLine('                PROTOSTAR');
      LoadLine(' ');
      LoadLine(' The protostar simulation treats the star as layers');
      LoadLine(' of matter, all of equal mass. The layers are');
      LoadLine(' effected by gravity and pressure (if it is turned');
      LoadLine(' on below). The simplest treatment is to allow the');
      LoadLine(' layers to free fall. If pressure is turned on, the');
      LoadLine(' thermodynamic pressure prevents the star from');
      LoadLine(' collapsing to zero radius.');
      LoadLine(' ');
      LoadLine('  Include Protostar Pressure            : #T');
      LoadLine('  Include Protostar Temperature Change  : #T');
      LoadLine('  Include Protostar Radiation Effect    : #T');
      LoadLine(' ');
      LoadLine('  Time Step for Protostar (years): {      }');
      LoadLine(' ');
      LoadLine('               [  Ok  ]      [Cancel]');
      SetBoolean(1,ProtoPressure);
      SetBoolean(2,ProtoTemp);
      SetBoolean(3,ProtoLum);
      SetNumber (4,DeltaTimeProto);
      Accept;
      ProtoPressure  := GetBoolean(1);
      ProtoTemp      := GetBoolean(2);
      ProtoLum       := GetBoolean(3);
      DeltaTimeProto := GetNumber (4);
      ClearInputPort;
      done;
   END;
   {dispose(ProtostarScreen);}       {needed on Mac}
END;

{---------------------------------------------------------------------------}

PROCEDURE ComputeMenu;
VAR
   ComputeScreen : TInputScreen;
BEGIN
   {new(ComputeScreen);}       {needed on Mac}
   WITH ComputeScreen DO
   BEGIN
      init;
      DefineInputPort(0.15,0.85,0.10,0.90);
      LoadLine('    MODIFY EVOLVE FROM ZAMS COMPUTE PARAMETERS');
      LoadLine(' ');
      LoadLine(' The precision of the fit to the fitting point mass');
      LoadLine(' can be varied (default 2%) as can the precision of');
      LoadLine(' the fit (default 20%) and the value of the fitting');
      LoadLine(' point mass (default 0.3). The fractional change in');
      LoadLine(' x and y is the amount changed at the end of each  ');
      LoadLine(' model convergence (default 1%).');
      LoadLine('  ');
      LoadLine('   Mass Fit Precision (0-1): {     }');
      LoadLine('   Fit Precision      (0-1): {     }');
      LoadLine('   Fit Mass Fraction  (0-1): {     }');
      LoadLine('   Fraction of x, y   (0-1): {     }');
      LoadLine('   Sound on : #T');
      LoadLine('  ');
      LoadLine('               [  Ok  ]      [Cancel]');
      SetNumber (1,precision);
      SetNumber (2,ModelPrecision);
      SetNumber (3,FitMassFraction);
      SetNumber (4,FractionDelX);
      SetBoolean(5,SoundOnOff);
      Accept;
      precision       := GetNumber(1); {desired fit accuracy In(Out)ward}
      ModelPrecision  := GetNumber(2); {desired accuracy in StellarModel}
      FitMassFraction := GetNumber(3); {desired fit mass fraction}
      FractionDelX    := GetNumber(4); {desired delta in x,y per step}
      SoundOnOff      := GetBoolean(5);
      done;
   END; {Modify}
   {dispose(ComputeScreen);}       {needed on Mac}
END;

{---------------------------------------------------------------------------}

PROCEDURE ShowResults;

VAR
 i  :Integer;
 j  :Integer;
BEGIN
   SetColor(whitte);
   DefineViewPort(7, 0.15, 0.85, 0.10, 0.90);
   GraphBackColor := blue;
   OpenViewPort(7);
   GraphBackColor := DarkGray;
   Print(1,1,'   Mass  Temperature  Density      Radius  Luminosity');
   j:= 1;
   FOR i := 3 TO 17 DO
   BEGIN {Print results of model values, only nonzero ones}
      Print(4,i,concat(NumStr(M_array[j],5,3),' ',
                       NumStrExp(T_array[j],10),' ',
                       NumStr(D_array[j],10,5),' ',
                       NumStr(R_array[j],10,3),' ',
                       NumStrExp(L_array[j],10) ));
      j := j + TRUNC(keepsize/15);
      IF (T_array[j] <= 0) THEN BEGIN j :=j+1; END; {dont print zeros}
      IF (T_array[j] <= 0) THEN BEGIN j :=j+1; END;
      IF (T_array[j] <= 0) THEN BEGIN j :=j+1; END;
      IF (T_array[j] <= 0) THEN BEGIN j :=j+1; END;
      IF (T_array[j] <= 0) THEN BEGIN j :=j+1; END;
      IF (j >= keepsize) THEN BEGIN j :=keepsize; END;
   END; {FOR}
END;

{---------------------------------------------------------------------------}

PROCEDURE ShowMoreResults;

VAR
 i  :Integer;
 j  :Integer;
BEGIN
   SetColor(whitte);
   DefineViewPort(7, 0.15, 0.85, 0.10, 0.90);
   GraphBackColor := blue;
   OpenViewPort(7);
   GraphBackColor := DarkGray;
   Print(1,1,'   Mass     Opacity  Power  Pressure      X       Y');
   j:= 1;
   FOR i := 3 TO 17 DO
   BEGIN {Print results of model values, only nonzero ones}
      Print(4,i,concat(NumStr(M_array[j],5,3),' ',NumStr(K_array[j],8,3),' '
                      ,NumStr(E_array[j],8,3),' ',NumStrExp(P_array[j],10),' '
                      ,NumStr(X[j],7,3),' ',NumStr(Y[j],7,3) ));
      j := j+TRUNC(keepsize/15);
      IF (T_array[j] <= 0) THEN BEGIN j :=j+1; END; {dont print zeros}
      IF (T_array[j] <= 0) THEN BEGIN j :=j+1; END;
      IF (T_array[j] <= 0) THEN BEGIN j :=j+1; END;
      IF (T_array[j] <= 0) THEN BEGIN j :=j+1; END;
      IF (T_array[j] <= 0) THEN BEGIN j :=j+1; END;
      IF (j >= keepsize) THEN BEGIN j :=keepsize; END;
   END; {FOR}
END;

{---------------------------------------------------------------------------}

PROCEDURE ModifyChemicalComposition;
CONST
  complimit = 1e-10; {minimum value of X,Y,Z}
Var
   ChemCompScreen : TInputScreen;
   xVal,yVal      : real;
   GoodValues     : boolean;
   i              : integer;
BEGIN
   {new(ChemCompScreen);}       {needed on Mac}
   WITH ChemCompScreen DO
   BEGIN
      init;
      DefineInputPort(0.15,0.85,0.10,0.90);
      LoadLine('            CHEMICAL COMPOSITION');
      LoadLine(' ');
      LoadLine(' The chemical composition of a star effects the');
      LoadLine(' opacity, the nuclear energy generation rate and');
      LoadLine(' the equation of state of a star. The mass fraction');
      LoadLine(' of hydrogen, helium and heavy elements are');
      LoadLine(' represented by the symbols x, y and z respectively.');
      LoadLine(' The values below will be used as the composition');
      LoadLine(' from the prestar formation up to the zero age');
      LoadLine(' main sequence when nuclear reactions modify it.');
      LoadLine('  ');
      LoadLine('  x (Hydrogen fraction): {         }');
      LoadLine('  y (Helium fraction)  : {         }');
      LoadLine('  z (Heavy fraction)   :  1-x-y     ');
      LoadLine(' ');
      LoadLine('              [  Ok  ]      [Cancel]');
      SetNumber(1,x[0]);
      SetNumber(2,y[0]);
      SetNumberLimits(1, complimit, 1.0-2*complimit);
      SetNumberLimits(2, complimit, 1.0-2*complimit);
      REPEAT
          GoodValues := true;
          Accept;
          IF not Canceled THEN {unless ESC, compute}
          BEGIN
           xVal := GetNumber(1);
           yVal := GetNumber(2);
           IF (xVal+yVal > 1.0) THEN
            BEGIN
             announce('Error: x+y+z must be equal to 1.0  <Enter> to continue');
             GoodValues := false;
            END
           ELSE
            BEGIN
             x[0] := xVal; {get values from screen}
             y[0] := yVal;
             z[0] := 1.0-x[0]-y[0];
             FOR i := 0 TO Keepsize DO
               BEGIN
                 x[i] := x[0];    {Fill uniform composition}
                 y[i] := y[0];
                 z[i] := z[0];
               END;
             TimeFromZAMS := 0;
             ComputedModel := 0;
            END; {IF}
          END;
      UNTIL GoodValues or Canceled;
      done;
   END;
   DefineViewPort(7,0.15,0.85,0.10,0.90);
   SelectViewPort(7);
   SetFillStyle(SolidFill,blue);
   HideCursor;
   bar( round(0.01*GetMaxX), round(0.45*GetMaxY),
        round(0.60*GetMaxX), round(0.78*GetMaxY) );
   ShowCursor;
   Print(2,12,concat('  x (Hydrogen fraction): ', Num2Str(x[0],9) ));
   Print(2,13,concat('  y (Helium fraction)  : ', Num2Str(y[0],9) ));
   Print(2,14,concat('  z (Heavy fraction)   : ', Num2Str(z[0],9) ));
   {dispose(ChemCompScreen);}       {needed on Mac}
end;

{---------------------------------------------------------------------------}

PROCEDURE ExplainHRDiagram;
VAR
   HRHelp : HelpScrType;
   i : integer;
BEGIN
   HRHelp[01] := '';
   HRHelp[02] := '              HERTZSPUNG-RUSSELL DIAGRAM';
   HRHelp[03] := '';
   HRHelp[04] := 'The Hertzsprung-Russell (HR) diagram plots stellar';
   HRHelp[05] := 'Luminosity against surface temperature. The star';
   HRHelp[06] := 'data values plotted here are from a range of';
   HRHelp[07] := 'stellar masses spanning from 0.1 to 501 solar';
   HRHelp[08] := 'masses, rising from the bottom of the plot to the';
   HRHelp[09] := 'top, with three slightly different compositions.';
   HRHelp[10] := 'The plot shows that stars which are in hydrostatic';
   HRHelp[11] := 'equilibrium form the band known as the Zero Age';
   HRHelp[12] := 'Main Sequence. The data plotted is actual values';
   HRHelp[13] := 'for main sequence, giant and white dwarf stars,';
   HRHelp[14] := 'as well as output from the calculations of the';
   HRHelp[15] := 'Stellar Model program, and is contained in file';
   HRHelp[16] := 'Evolve.dat. At a constant temperature, an increase';
   HRHelp[17] := 'in luminosity results from an increase in radius.';
   HRHelp[18] := 'At constant Luminosity, a decrease in radius';
   HRHelp[19] := 'means an increase in surface temperature. The';
   HRHelp[20] := 'Hayashi line represents, for a given composition,';
   HRHelp[21] := 'the seperation between the allowed (left) and';
   HRHelp[22] := 'forbidden (right) regions of the HR diagram.';
   FOR i := 23 TO 25 DO HRHelp[i] := '';
   Help(HRHelp);
END;

{---------------------------------------------------------------------------}

PROCEDURE ExplainStagesMenu;
VAR
   StagesHelp : HelpScrType;
   i : integer;
BEGIN
   StagesHelp[01] := '';
   StagesHelp[02] := '              STAGES OF EVOLUTION';
   StagesHelp[03] := '';
   StagesHelp[04] := 'Stars evolve from the protostar stage through a';
   StagesHelp[05] := 'contraction phase, leading to nuclear ignition';
   StagesHelp[06] := 'and onto the zero age main sequence. They spend';
   StagesHelp[07] := 'the majority of their life on the main sequence';
   StagesHelp[08] := 'fusing hydrogen into helium. When their hydrogen';
   StagesHelp[09] := 'fuel is depleted, stars evolve off the main';
   StagesHelp[10] := 'sequence to a red giant or supergiant and thence';
   StagesHelp[11] := 'to their eventual demise as a white dwarf or a';
   StagesHelp[12] := 'supernova. The M=1 Star Evolution shows how a one';
   StagesHelp[13] := 'solar mass star evolves in time from its birth to';
   StagesHelp[14] := 'its death.';
   FOR i := 15 TO 25 DO StagesHelp[i] := '';
   Help(StagesHelp);
END;

{---------------------------------------------------------------------------}

PROCEDURE ExplainProtostar;
VAR
   StarHelp : HelpScrType;
   i : integer;
BEGIN
   StarHelp[01] := '';
   StarHelp[02] := '                  PROTOSTAR';
   StarHelp[03] := '';
   StarHelp[04] := 'Stars condense gravitationally from optically';
   StarHelp[05] := 'invisible, giant molecular clouds found in the';
   StarHelp[06] := 'disk of spiral galaxies. The gas collapses,';
   StarHelp[07] := 'initially in free-fall, gaining energy from the';
   StarHelp[08] := 'gravitational collapse, and heating the interior.';
   StarHelp[09] := 'Thermal pressure slows the inner regions, forming';
   StarHelp[10] := 'a protostar with a shock front absorbing the';
   StarHelp[11] := 'collapse energy and an isothermal interior. The';
   StarHelp[12] := 'outer regions are opaque, hiding the shock front';
   StarHelp[13] := 'for a time. When the core temperature of the star';
   StarHelp[14] := 'is high enough, nuclear reactions begin, and the';
   StarHelp[15] := 'star becomes a zero age main sequence (ZAMS) star.';
   For i := 16 TO 25 DO StarHelp[i]:= '';
   Help(StarHelp);
END;

{---------------------------------------------------------------------------}

PROCEDURE ShowAcceptMenu;

BEGIN
 Message('<ENTER> or <DoubleClick> to accept radius value, <ESC> for main menu');
END;

{---------------------------------------------------------------------------}

PROCEDURE Help1;

VAR
 helpScr    : HelpScrType;
BEGIN
 helpScr[1] := '                 HELP SCREEN';
 helpScr[2] :=   '';
 helpScr[3] :=   '   The menu choices are:';
 helpScr[4] :=   '';
 helpScr[5] :=   '    File       - Exit and file handling';
 helpScr[6] :=   '    Stages     - the stages of stellar';
 helpScr[7] :=   '                 evolution';
 helpScr[8] :=   '    HR Diagram - show temperature versus';
 helpScr[9] :=   '                 luminosity plots';
 helpScr[10]:=   '    Boundary   - the boundary conditions';
 helpScr[11] :=  '                 on the star, a description';
 helpScr[12] :=  '                 of the model assumptions,';
 helpScr[13] :=  '                 and the equations used';
 helpScr[14] :=  '    Protostar  - evolve a protostar onto';
 helpScr[15] :=  '                 the main sequence';
 helpScr[16] :=  '    Main Seq   - run a model calculation';
 helpScr[17] :=  '                 for a ZAMS star';
 helpScr[18] :=  '';
 helpScr[19] :=  '   To perform a simulation, set the';
 helpScr[20] :=  '   parameters of the star in the Boundary';
 helpScr[21] :=  '   Conditions menu and then go to the';
 helpScr[22] :=  '   Protostar or Main Sequence menus. There';
 helpScr[23] :=  '   you can initiate the evolution from a';
 helpScr[24] :=  '   protostar to the ZAMS, or from the ZAMS';
 helpScr[25] :=  '   to the demise of the star.';

 help(helpScr);
END;

{---------------------------------------------------------------------------}
{*-----               Menu Response handling Procedures               -----*}
{---------------------------------------------------------------------------}

PROCEDURE FileMenuItem; {FIle menu handling}

VAR
 fname    :String;
 dummy    :String;
 datafile :Text;
 i        :Integer;
 s        :STRING;

BEGIN

  WITH MyMenu DO CASE rowChosen OF

   1: AboutEvolve;

   2: AboutCUPS;

   3: GettingStarted;

   4: BEGIN  {File Save}
     {before saving, must fillin arrays with missing points-else read won't work}
       FOR i := 1 TO Keepsize DO
          BEGIN
           IF(T_array[i] <= 0)THEN
            BEGIN                    {copy last values}
             M_array[i] := M_array[i-1];
             T_array[i] := T_array[i-1];
             D_array[i] := D_array[i-1];
             R_array[i] := R_array[i-1];
             L_array[i] := L_array[i-1];
             K_array[i] := K_array[i-1];
             E_array[i] := E_array[i-1];
             P_array[i] := P_array[i-1];
             x[i]       := x[i-1];
             y[i]       := y[i-1];
            END;
         END;
       fname := concat('ES',NumStr(Mass,7,3));  {filename; get rid of blanks}
       WHILE POS(' ',fname) > 0 DO BEGIN fname[POS(' ',fname)] := '0'; END;
       ASSIGN(datafile,fname); {OPEN on Mac}
         REWRITE(datafile);
         WRITELN(datafile,Mass,        '  :Mass        ');
         WRITELN(datafile,Tsurface,    '  :Tsurface    ');
         WRITELN(datafile,Teffective,  '  :Teffective  ');
         WRITELN(datafile,Luminosity,  '  :Luminosity  ');
         WRITELN(datafile,Tcentral,    '  :Tcentral    ');
         WRITELN(datafile,Dcentral,    '  :Dcentral    ');
         WRITELN(datafile,TimeFromZAMS,'  :TimeFromZAMS');
         WRITELN(datafile,concat('Columns are: Mass, Temperature, Density, Radius,',
          ' Pressure, Luminosity, Opacity, Power, H mass frac, He mass frac'));
         FOR i := 0 TO Keepsize DO
          BEGIN
           IF(T_array[i] > 0)THEN
            BEGIN
             WRITELN(datafile, M_array[i], T_array[i], D_array[i], R_array[i]
               ,P_array[i], L_array[i], K_array[i], E_array[i], x[i], y[i]);
            END;
         END;
         CLOSE(datafile); i := IOResult; {close and clear}
         comment (0.1,0.9,concat(
            'The computed model data results were written to file: ',fname));
      END; {File Save}


   5: BEGIN  {File Read}
    {   fname := concat('ES',NumStr(Mass,7,3)); } {filename; get rid of blanks}
    {   WHILE POS(' ',fname) > 0 DO BEGIN fname[POS(' ',fname)] := '0'; END;}
       fname := OpenFile('ES*.*');
       IF fname='' THEN announce ('No file was selected.')
        ELSE
         BEGIN
          ASSIGN(datafile,fname); {OPEN on Mac}
          {$I-} RESET(datafile);

          IF(IOResult = 0)THEN {make sure file exists}
           BEGIN
            InitializeModel; {reset everything}
            READLN(datafile,Mass);
            READLN(datafile,Tsurface);
            READLN(datafile,Teffective);
            READLN(datafile,Luminosity);
            READLN(datafile,Tcentral);
            READLN(datafile,Dcentral);
            READLN(datafile,TimeFromZAMS);
            READLN(datafile,dummy);
            FOR i := 0 TO Keepsize DO
             BEGIN
              READLN(datafile, M_array[i], T_array[i], D_array[i], R_array[i]
                ,L_array[i], K_array[i], E_array[i], P_array[i], x[i], y[i]);
             END;
            CLOSE(datafile); i := IOResult; {close and clear}
            comment (0.1,0.9,concat(
              'The computed model data file was read: ',fname));
        END
       ELSE
        BEGIN
         comment (0.1,0.9,concat(
            'The computed model data file does not exist: ',fname));
          Beep;
       END; {IF}
         END;{IF}
      END; {File Read}

   6: Configuration;

   7: BEGIN CUPSdone; HALT; END; {Quit}

  END; {CASE menu}


END;   {FileMenuItem}

{---------------------------------------------------------------------------}

PROCEDURE BoundaryConditions; {handle Boundary conditions option}

BEGIN
   WITH MyMenu DO CASE rowChosen OF
    1: ModifyBoundaryConditions;
    2: ModifyChemicalComposition;
    3: InitializeModel; {reinitialize}
   END; {CASE}
END;   {BoundaryConditions}

{---------------------------------------------------------------------------}

{needed on Mac $S evolve3}

PROCEDURE Compute; {do the stellar calcs}

VAR
 Error    :Integer; {error returned by StellarModel}
 fname    :String;
 datafile :Text;
 fevolvename :String;
 fileevolve  :Text;
 i     :Integer;
 j     :Integer;
 Loop  :Integer;
 xx    :Double;
 yy    :Double;
 numplot :Integer;

BEGIN

  PlotControl := 1;  {update plot if >0}

  radius := SQRT(Luminosity)/SQR(Teffective/SunTsurface); {initial r}

  WITH MyMenu DO CASE rowChosen OF

   1: BEGIN  {Run One Model}
       EvLoop := 0;  {time step loop counter}
       Loop := 0;    {do not loop forever}
       PrepareForModelCalc;
       comment (0,0.05,
       'Hot: cC=Tcentral dD=Density eE=Teff lL=Luminos pP=Pause rR=Replot sS=Stop +/-');
       StellarModel(Error,Loop);  {go compute the model}
       comment (0.05,0.96,'                                                                         ');
       comment (0.05,0.96,concat('Te:',NumStr(Teffective,4,0)
          ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
          ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2)));
       comment (0,0.05,
       '                                                                              ');

      END; {Run Model}

 2,3: BEGIN  {Run/Continue Evolution of Model}
       Loop := 1;  {loop forever}
       Error := 0;
       PrepareForModelCalc;  {initialize arrays}

       {Determine the disk file name for the evolution file}
       fevolvename := concat('EV',NumStr(Mass,7,3)); {filename for evolution results}
       WHILE POS(' ',fevolvename) > 0 DO     {get rid of blanks}
             BEGIN fevolvename[POS(' ',fevolvename)] := '0'; END;
       ASSIGN(fileevolve,fevolvename);      {OPEN on Mac}
       {Must open file for results; decide whether to append or not}
       {$I-} RESET(fileevolve); CLOSE(fileevolve); 
       IF((rowChosen = 3) AND (IOResult = 0))THEN {append if continue}
         BEGIN   {on Mac, for append use seek(fileevolve,Filesize(fileevolve))}
           APPEND(fileevolve); END  {append to existing file with results}
         ELSE BEGIN
           {ERASE(fileevolve);}      {open NEW the data file-not needed}
           REWRITE(fileevolve);    {open NEW the data file}
           {Write file header}
           WRITELN(fileevolve,'Evolution of star from ZAMS of Mass=',Mass:10:6);
           WRITELN(fileevolve,concat('Time(Myrs)  Teff ',
           'Luminosity  Tcentral    Dcentral  Radius   Xc      Yc  Iters'));
         END;


       IF(rowChosen = 2)THEN {Run Evolution has more initialization than continue}
        BEGIN
         FOR i:=0 TO HRsize DO    {must clear arrays}
          BEGIN
           TS_array[i] :=0;
           LS_array[i] :=0;
          END; {FOR}
         EvLoop := 0;  {loop counter}
        END; {IF}

       comment (0,0.05,
       'Hot: cC=Tcentral dD=Density eE=Teff lL=Luminos pP=Pause rR=Replot sS=Stop +/-');

       WHILE (Error = 0) DO
        BEGIN

          StellarModel(Error,Loop);  {go compute the model}

          IF((Error = 0) OR (Error = -2))THEN  {write out result if no error, or exit on convergence}
           BEGIN
             WRITELN(fileevolve, TimeFromZAMS:8:0, Teffective:8:0,
                  Luminosity:12, Tcentral:12, Dcentral:9:1,
                  Radius:8:3, X[0]:8:4, Y[0]:8:4, ModIteration:3);
             IF(EvLoop < HRsize)THEN EvLoop := EvLoop+1;
             TS_array[EvLoop] := Teffective; {save surface T}
             LS_array[EvLoop] := Luminosity; {save surface L}
             SetColor(Whitte);
           END; {IF}

          IF(Error = 0)THEN
           BEGIN
             InitializeModelPlot(5);  {prepare plot screen}
             FOR i:=1 TO HRsize DO    {update HR plot}
              BEGIN
               IF (TS_array[i] > 0)THEN BEGIN
                xx := Log(10.0,TS_array[i]);
                yy := Log(10.0,LS_array[i]);
                PlotDataModel(3,xx,yy); {L vs. T}
               END; {IF}
              END; {FOR}
           END; {IF}
          comment (0.05,0.96,concat(
           'Last values:      Te:',NumStr(Teffective,4,0)
           ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
           ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2),'        '));

       END; {DO WHILE}
       comment (0,0.05,
       '                                                                              ');


       CLOSE(fileevolve); i := IOResult; {close and clear}
       Message(concat('The data was written to file: ',fevolvename,
               '     <Enter> to continue'));
       StaticPause;

      END; {Run/Continue Evolve Model}

   4: ShowResults;

   5: ShowMoreResults;

{needed on Mac $S evolve4}

   6: BEGIN  {Plot Results}
       numplot := 5;
       REPEAT
        {HideCursor;}
        ClearMUPPETPort;
        comment (0.1,0.96,concat('  Te:',NumStr(Teffective,4,0)
            ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
            ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2)));
        Message('Click on plot to zoom in/out, <ESC> for main menu');
        InitializeModelPlot(numplot);  {prepare plot screen}

        SetColor(Yellow);
        FOR i := 0 TO keepsize DO
           BEGIN
              xx := M_array[i]/mass;
              IF (T_array[i] > 0)THEN BEGIN
                yy := Log(10.0,T_array[i]);
                IF((numplot=5) OR (numplot=1))THEN PlotDataModel(1,xx,yy); {T vs. M}
              END; {IF}
              IF (D_array[i] > 0)THEN BEGIN
                yy := Log(10.0,D_array[i]);
                IF((numplot=5) OR (numplot=2))THEN PlotDataModel(2,xx,yy); {D vs. M}
              END; {IF}
              IF (L_array[i] > 0)THEN BEGIN
                yy := Log(10.0,L_array[i]);
                IF((numplot=5) OR (numplot=4))THEN PlotDataModel(4,xx,yy); {L vs. M}
              END; {IF}
           END; {FOR}

           SetColor(Whitte); 
           FOR i:=1 TO HRsize DO    {HR plot}
             BEGIN
              IF (TS_array[i] > 0)THEN BEGIN
                xx := Log(10.0,TS_array[i]);
                yy := Log(10.0,LS_array[i]);               
                IF((numplot=5) OR (numplot=3))THEN PlotDataModel(3,xx,yy); {L vs. T}
              END; {IF}
             END; {FOR}

        REPEAT;
        CheckForEvents;
        {ShowCursor;}
        UNTIL ((Event.Readkey=chr(27)) OR (Event.MouseClicked));

        IF(Event.MouseClicked)THEN  {look for coordinates of mouse click}
          BEGIN
            IF(numplot = 5)THEN
            BEGIN
              numplot:=1;
              IF((Event.x <= 335) AND (Event.y <= 220))THEN numplot :=2;
              IF((Event.x >  335) AND (Event.y >  220))THEN numplot :=3;
              IF((Event.x >  335) AND (Event.y <= 220))THEN numplot :=4;
            END
            ELSE BEGIN
            numplot:=5;
            END;
          END
          ELSE BEGIN
            numplot:=0;
          END;
        ClearMKbuffers;
       UNTIL (numplot=0);
      END; {Results}

   7: BEGIN  {Plot More Results}
       numplot := 5;
       REPEAT
        {HideCursor;}
        ClearMUPPETPort;
        comment (0.1,0.96,concat('  Te:',NumStr(Teffective,4,0)
            ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
            ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2)));
        Message('Click on plot to zoom in/out, <ESC> for main menu');
        InitializeMoreModelPlot(numplot);  {prepare plot screen}

        SetColor(Yellow);
        FOR i := 0 TO keepsize DO
           BEGIN
              xx := M_array[i]/mass;
              IF (K_array[i] > 0)THEN BEGIN
                yy := Log(10.0,K_array[i]);
                IF((numplot=5) OR (numplot=1))THEN PlotDataModel(1,xx,yy); {K vs. M}
              END; {IF}
              IF (E_array[i] > 0)THEN BEGIN
                NuclearPower(x[i],y[i],z[i],znitrogen[i],D_array[i],T_array[i]);
                yy := Log(10.0,powerpp);
                IF((numplot=5) OR (numplot=2))THEN PlotDataModel(2,xx,yy); {E vs. M}
              END; {IF}
              IF (P_array[i] > 0)THEN BEGIN
                yy := Log(10.0,P_array[i]);
                IF((numplot=5) OR (numplot=3))THEN PlotDataModel(3,xx,yy); {P vs. M}
              END; {IF}
              IF (R_array[i] > 0)THEN BEGIN
                yy := Log(10.0,R_array[i]);
                IF((numplot=5) OR (numplot=4))THEN PlotDataModel(4,xx,yy); {R vs. M}
              END; {IF}
           END; {FOR}

        SetColor(LightGreen); {total power}
        FOR i := 0 TO keepsize DO  {must do again for other plot}
           BEGIN
              xx := M_array[i]/mass;
              IF (E_array[i] > 0)THEN BEGIN
                NuclearPower(x[i],y[i],z[i],znitrogen[i],D_array[i],T_array[i]);
                yy := Log(10.0,power);
                IF((numplot=5) OR (numplot=2))THEN PlotDataModel(2,xx,yy); {E vs. M}
              END; {IF}
           END; {FOR}

        REPEAT;
           CheckForEvents;
           {ShowCursor;}
        UNTIL ((Event.Readkey=chr(27)) OR (Event.MouseClicked));

        IF(Event.MouseClicked)THEN  {look for coordinates of mouse click}
          BEGIN
            IF(numplot = 5)THEN
            BEGIN
              numplot:=1;
              IF((Event.x <= 335) AND (Event.y <= 220))THEN numplot :=2;
              IF((Event.x >  335) AND (Event.y >  220))THEN numplot :=3;
              IF((Event.x >  335) AND (Event.y <= 220))THEN numplot :=4;
            END
            ELSE BEGIN
            numplot:=5;
            END;
          END
          ELSE BEGIN
            numplot:=0;
          END;
        ClearMKbuffers;
       UNTIL (numplot=0);
      END; {More Results}

   8: BEGIN  {Plot composition}
       numplot := 5;
       REPEAT
        {HideCursor;}
        ClearMUPPETPort;
        comment (0.1,0.96,concat('  Te:',NumStr(Teffective,4,0)
            ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
            ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2)));
        Message('Click on plot to zoom in/out, <ESC> for main menu');
        InitializeCompositionPlot(numplot);  {prepare plot screen}

        SetColor(Yellow);
        FOR i := 0 TO Keepsize DO
           BEGIN
            xx := M_array[i]/mass;
            IF(xx > 0)THEN
             BEGIN
              yy := Log(10.0,x[i]);
              IF((numplot=5) OR (numplot=1))THEN PlotDataModel(1,xx,yy); {x vs. M}
              yy := Log(10.0,y[i]);
              IF((numplot=5) OR (numplot=2))THEN PlotDataModel(2,xx,yy); {y vs. M}
              yy := Log(10.0,z[i]);
              IF((numplot=5) OR (numplot=3))THEN PlotDataModel(3,xx,yy); {z vs. M}
              yy := Log(10.0,znitrogen[i]);
              IF((numplot=5) OR (numplot=4))THEN PlotDataModel(4,xx,yy); {znitrogen vs. M}
             END;
           END; {FOR}
           SetColor(Whitte);

        REPEAT;
           CheckForEvents;
           {ShowCursor;}
        UNTIL ((Event.Readkey=chr(27)) OR (Event.MouseClicked));

        IF(Event.MouseClicked)THEN  {Zoom: look for coordinates of mouse click}
          BEGIN
            IF(numplot = 5)THEN
            BEGIN
              numplot:=1;
              IF((Event.x <= 335) AND (Event.y <= 220))THEN numplot :=2;
              IF((Event.x >  335) AND (Event.y >  220))THEN numplot :=3;
              IF((Event.x >  335) AND (Event.y <= 220))THEN numplot :=4;
            END
            ELSE BEGIN
            numplot:=5;
            END;
          END
          ELSE BEGIN
            numplot:=0;
          END;
        ClearMKbuffers;
       UNTIL (numplot=0);
      END; {Composition}

  9: BEGIN  {Modify Val}
       ComputeMenu;
       ClearMUPPETport;
     END;
  END; {CASE menu}

END;   {Compute}

{---------------------------------------------------------------------------}

PROCEDURE Protostar;
{Evolve a protostar. Units are sun mass and sun radius, g/cc}
{Either free fall or freefall plus pressure effect is computed}
{Protostar is constrained not to grow bigger}

VAR
   i         :Integer;
   lastR     :Double;
   deltaR    :Double;
   deltaT    :Double;
   r0        :Double;
   R         :Double;
   T         :Double;
   L         :Double;
   gamma     :Double;
   cee       :Double;
   volume    :Double;
   pVtoGamma :Double;
   TVtoGamma :Double;
   pVideal   :Double;
   MeanIonWeight :Double;
   Moles     :Double;
   RadiusSlider: Tsliders;

BEGIN

 WITH MyMenu DO CASE rowChosen OF

   1: BEGIN {protostar evolution}

      InitializeProtostarPlot;
      InitializeProtostar;
      SetUpProtoHotKeys;
      SelectViewport(5);
      SelectScale(5);
      PlotSymbol(5-Log(10.0,6400),0,'*'); {mark sun's position on HR plot}

      Message('r = cloud radius (units of solar radius)   <Any Key> continues from pause');
      SelectViewport(4);
      comment (0,0.96,
        'Free Fall of a Diffuse Cloud; the * on the HR diagram is the Sun');

      ProtostarEvolution; {go evolve the protostar}

     END; {protostar evolution}

  2: ShowResults;

  3: ProtostarInputScreen;

  4: ExplainProtostar;

  5: BEGIN {Thermodynamics}
        R := 1e6;
        lastR := 0;
        T := 10;
        L := SQR(SQR(T)*(R*SunRadius))*sigma*4*pi/SunLum;

        gamma := 1.67;
        volume := 4*pi*(PWR((SunRadius*R),3))/3; {initial volume}
        Density := Mass*SunMass/volume;
        EquationOfState(x[1],y[1],z[1],Density,T);
        TVtoGamma := T*PWR(volume,(gamma-1)); {adiabatic TV^(gamma-1)=const}
        PVtoGamma := Pressure*PWR(volume,gamma); {adiabatic PV^gamma=const}
        PVideal   := Pressure*volume; {ideal gas pV=const}
        MeanIonWeight := 1.0/(x[1]+y[1]/4.0+z[1]/16.0); {mean moleclr weight}
        Moles := Mass*Sunmass/MeanIonWeight;

        ClearMUPPETport;
        RadiusSlider.init;
        InitializeThermodynamicsPlot;
        ShowAcceptMenu;

        SelectViewport(1);
        Print(1,1,'INITIAL STAR CONDITIONS:');
        Print(54,1,'FINAL STAR CONDITIONS:');
        SetColor(LightGreen);
        Print(1,2,concat('R     =',NumStrExp(R,8),' Rsun'));
        Print(1,3,concat('volume=',NumStrExp(volume,8),' cc'));
        Print(1,4,concat('rho   =',NumStrExp(density,8),' g/cc'));
        Print(1,5,concat('T     =',NumStrExp(T,8),' K'));
        Print(1,6,concat('P     =',NumStrExp(Pressure,8),' dyne/cm^2'));
        Print(1,7,concat('L     =',NumStrExp(L,8),' Lsun'));
        Print(1,8,concat('mass  =',NumStrExp(Mass,8),' Msun'));
        Print(1,9,concat('moles =',NumStrExp(Moles,8)));

        comment (0.2,0.955,'Set new radius to find new thermodynamic conditions');
        RadiusSlider.create(1,1,1000000,R,0,0.05,1,0,'1','1000000',
                            'Radius',false);
        RadiusSlider.drawall;

        REPEAT
         REPEAT
          CheckForEvents;
          IF(RadiusSlider.changed)THEN R:=RadiusSlider.value(1);
          IF(R < 0.1)THEN R := 0.1;

         UNTIL (Event.Readkey=chr(27)) OR (Event.Readkey=chr(13)) OR Event.DoubleClicked;
        Density := Density*volume;
        volume := 4*pi*(PWR((SunRadius*R),3))/3; {new volume}
        Density := Density/volume; {scale density}

        EquationOfState(x[1],y[1],z[1],Density,T);
        SelectViewport(1);
        SetColor(yellow);
        RubOut(54,2,22,blue);
        Print(54,2,concat('R      =',NumStrExp(R,8),' Rsun'));
        RubOut(54,3,22,blue);
        Print(54,3,concat('volume =',NumStrExp(volume,8),' cc'));
        RubOut(54,4,22,blue);
        Print(54,4,concat('rho    =',NumStrExp(density,8),' g/cc'));
        RubOut(54,5,22,blue);
        T := 10; {isothermal}
        Print(54,5,concat('T(isoT)=',NumStrExp(T,8),' K'));
        RubOut(54,6,22,blue);
        Print(54,6,concat('P(isoT)=',NumStrExp(Pressure,8),' dyne/cm^2'));

        T := TVtoGamma/PWR(volume,(gamma-1));
        Pressure := PVtoGamma/PWR(volume,gamma);
        RubOut(54,7,22,blue);
        Print(54,7,concat('T(ad)  =',NumStrExp(T,8),' K'));
        RubOut(54,8,22,blue);
        Print(54,8,concat('P(ad)  =',NumStrExp(Pressure,8),' dyne/cm^2'));
        L := SQR(SQR(T)*(R*SunRadius))*sigma*4*pi/SunLum;
        RubOut(54,9,22,blue);
        Print(54,9,concat('L(ad)  =',NumStrExp(L,8),' Lsun'));

        Circle(290,225,trunc(140*R/1e6)); {new circle}
        SetColor(Blue);
        Circle(290,225,trunc(140*lastR/1e6)); {erase old circle}
        lastR := R;
        UNTIL (Event.Readkey=chr(27));

        RadiusSlider.Delete(1);
        RadiusSlider.Done;
        ClearMUPPETport;
     END; {Thermodynamics}

  6: BEGIN {Free Fall evolution- star collapse under gravity}

      InitializeProtostarPlot;
      InitializeProtostar;
      SetUpProtoHotKeys;
      SelectViewport(5);
      SelectScale(5);
      PlotSymbol(5-Log(10.0,6400),0,'*'); {mark sun's position on HR plot}
      Message('r = cloud radius (in units of solar radius) <Any Key> continues from pause');
      SelectViewport(4);
      comment (0.1,0.96,
        'Free Fall of a Diffuse Cloud; the * on the HR diagram is the Sun');

      FreeFall;      {go do the freefall calculation}

     END; {free fall evolution}

 END; {CASE}
END;   {Protostar}

{---------------------------------------------------------------------------}

PROCEDURE PlotHayashi(m :Double);  {plot Hayashi line}

VAR
 i:       Integer;
 t:       Double;
 l:       Double;
 A:       Double;
 B:       Double;

 BEGIN
      SelectViewPort(5);  {put up Hayashi lines}
      SelectScale(5);
      SetColor(yellow);
      A := 0.05; {Hayashi parameters from KW, p. 228}
      B := 0.2;
      l := -1;
      FOR i := 1 TO 100 DO
       BEGIN
         t := -A*l+B*log(10,m)+3.7;  {Hayashi formula from KW, p.227}
         PlotLine(t,l,t,l);
         l := l +6.0/100.0; {step luminosity from -1 to 5}
       END;
 END; {PlotHayashi}
{---------------------------------------------------------------------------}

{needed on Mac $S evolve5}

PROCEDURE HRdiagram;  {handle HR diagram menu item}

VAR
 x:       Double;
 z:       Double;
 m:       Double;
 t:       Double;
 l:       Double;
 tu:      Double;
 lu:      Double;
 tc:      Double;
 rho:     Double;
 r:       Double;
 f:       text;
 i:       integer;
 starcolorflag:   integer;
 ch:      char;
 A:       Double;
 B:       Double;

BEGIN
 WITH MyMenu DO CASE rowChosen OF
  1: BEGIN {show HR diagram of star data}
      Assign(f,'evolve.dat');  {OPEN on Mac}
      {Must open data file; make sure it exists}
      {$I-} RESET(f);
      IF(IOResult <> 0)THEN
        BEGIN
         Message('Data file Evolve.dat could not be opened <any key> to continue.');
         StaticPause;
         END
        ELSE BEGIN
         InitializeHRPlot(3.0);
         SelectScale(5);
         SelectViewPort(5);

         Readln(f,ch);    {skip first two lines of file}
         Readln(f,ch);    {skip first two lines of file}
         WHILE not Eof(f) DO
          BEGIN
           Readln(f,x,z,m,t,l,tc,rho,r,starcolorflag);
           If(starcolorflag >0) THEN
                      SetColor(Lightgreen); {actual star data or calculated?}
           t := t/TwoTo14; {convert effective T to surface T}
           PlotLine(Log(10.0,t)-0.01,Log(10.0,l),Log(10.0,t)+0.01,Log(10.0,l));
           PlotLine(Log(10.0,t),Log(10.0,l)-0.1,Log(10.0,t),Log(10.0,l)+0.1);
           SetColor(whitte);
          END; {DO}
         SetColor(LightGreen);
         Print(2,1,'High mass ZAMS');
         Print(50,18,'Low mass ZAMS');
         Print(2,11,'Green: actual star data');
         SetColor(whitte);
         Print(20,17,'White Dwarfs');
         Print(2,12,'White: calculated stars');
         SetColor(LightRed);
         Print(42,4,'Red Giants');

         comment (0.05,0.96,
          'HR Diagram of Main Sequence Stars with 0.1 < M < 501 (File Evolve.dat)');
         SetColor(whitte);
      END;
     END; {show HR}

  2: BEGIN {show HR diagram trends}
      InitializeHRPlot(3.0);
      Message('HR Diagram (Effective Temp vs. Luminosity)              <Click> for next step');
      Staticpause;

      SelectViewPort(5);
      t := Log(10.0,3000/TwoTo14);  {plot ZAMS line}
      l := Log(10.0,0.001);
      tu := Log(10.0,73000/TwoTo14);
      lu := Log(10.0,16210676.0);
      SetColor(LightGreen);
      PlotLine(t,l,tu,lu);
      Print(9,10,'Zero Age Main Sequence');
      SetColor(Yellow);
      Print(11,2,'High mass');
      Print(42,17,'Low mass');
      SetColor(LightRed);
      Print(42,3,'Red Giants');
      SetColor(whitte);
      Print(20,17,'White Dwarfs');
      Message('HR Diagram of the Zero Age Main Sequence Line           <Click> for next step');
      Staticpause;

      SelectViewPort(5);
      Print(20,4,'Star radius decreases at constant L');
      PlotLine(3.5,5.2,4.45,5.2); {horizontal line}
      PlotLine(4.4,5.4,4.45,5.2); {arrow}
      PlotLine(4.4,5.0,4.45,5.2); {arrow}

      Print(49,11,'Star radius');
      Print(49,12,'increases at');
      Print(49,13,'constant T');
      PlotLine(3.5,-2.0,3.5,3.0); {vertical line}
      PlotLine(3.47,2.7,3.5,3.0);
      PlotLine(3.53,2.7,3.5,3.0);
      Message('HR Diagram Stellar Radius Trends with T and L           <Click> for next step');
      Staticpause;

      SelectViewPort(5);
      PlotHayashi(1);
      Print(43,8,'M=1');
      PlotHayashi(10);
      Print(36,8,'M=10');
      Print(28,7,'Hayashi Lines');
      Message('Hayashi Lines Define Limit of Hydrostatic Equilibrium   <Click> for next step');
      Staticpause;

      comment (0.1,0.96,
       'HR Diagram Showing the ZAMS, Radius Trends and Hayashi Lines');

     END; {show HR trends}

  3: ExplainHRDiagram;

 END; {CASE}
END;   {HRdiagram}


{---------------------------------------------------------------------------}

PROCEDURE Stages; {handle stages menu item}

VAR
 t:       Double;
 l:       Double;
 tu:      Double;
 lu:      Double;
 i:       integer;
 j:       integer;
 hits:    integer;
 key:     Char;
 hkey:    Byte;
 junk:    LongInt;
 dummy:   LongInt;
 Mass_1t: Array[1..20] of String[44]; {Labels for data above}
 Mass_1:  Array[1..20,1..3] of Double; {array of points on evolution;time,L,T}
 Mass_1n: Integer;
 xx:      Double;
 yy:      Double;
 xtime:   Double;
 ytime:   Double;
 ytimelast: Double;

BEGIN
 Mass_1n := 20;         {number of entries}
 Mass_1[1,1] := 0;      {1-time of this stage}
 Mass_1[1,2] := 0.08;   {2-luminoscity}
 Mass_1[1,3] := 40;     {3-temperature}
 Mass_1[2,1] := 200;
 Mass_1[2,2] := 0.2;
 Mass_1[2,3] := 50.0;
 Mass_1[3,1] := 1000;
 Mass_1[3,2] := 2;
 Mass_1[3,3] := 70;
 Mass_1[4,1] := 10000;
 Mass_1[4,2] := 20;
 Mass_1[4,3] := 150;
 Mass_1[5,1] := 50000;
 Mass_1[5,2] := 40;
 Mass_1[5,3] := 500;
 Mass_1[6,1] := 1.0e5;
 Mass_1[6,2] := 40;
 Mass_1[6,3] := 1000;
 Mass_1[7,1] := 2.35e5;
 Mass_1[7,2] := 15;
 Mass_1[7,3] := 2000;
 Mass_1[8,1] := 2.5e5;
 Mass_1[8,2] := 15;
 Mass_1[8,3] := 8000;
 Mass_1[9,1] := 4.2e5;
 Mass_1[9,2] := 5;
 Mass_1[9,3] := 6000;
 Mass_1[10,1] := 8.9e6;
 Mass_1[10,2] := 0.8;
 Mass_1[10,3] := 5000;
 Mass_1[11,1] := 5.0e7;      {ZAMS}
 Mass_1[11,2] := 1.0;        {ZAMS}
 Mass_1[11,3] := 6400;       {ZAMS}
 Mass_1[12,1] := 10.0e9;
 Mass_1[12,2] := 1.0;
 Mass_1[12,3] := 6400;
 Mass_1[13,1] := 10.1e9;
 Mass_1[13,2] := 5.0;
 Mass_1[13,3] := 6400;
 Mass_1[14,1] := 10.3e9;
 Mass_1[14,2] := 5.0;
 Mass_1[14,3] := 4000;
 Mass_1[15,1] := 10.5e9;     {helium flash - only a few minutes}
 Mass_1[15,2] := 1100.0;
 Mass_1[15,3] := 4000;
 Mass_1[16,1] := 10.5e9+0.00001;
 Mass_1[16,2] := 100.0;
 Mass_1[16,3] := 5000;
 Mass_1[17,1] := 10.5e9+20000;
 Mass_1[17,2] := 1000.0;
 Mass_1[17,3] := 5000;
 Mass_1[18,1] := 10.5e9+55000;
 Mass_1[18,2] := 10000.0;
 Mass_1[18,3] := 4000;
 Mass_1[19,1] := 10.5e9+65000;
 Mass_1[19,2] := 10000.0;
 Mass_1[19,3] := 100000.0;  {was 20000 until 6/93}
 Mass_1[20,1] := (10.5e9)+75000;
 Mass_1[20,2] := 0.2;
 Mass_1[20,3] := 31700.0;

 Mass_1t[1] := '1) Dense core forms from interstellar cloud';
 Mass_1t[2] := '2) Dense core forms from interstellar cloud';
 Mass_1t[3] := '3) Protostar evolves';
 Mass_1t[4] := '4) Large radius protostar';
 Mass_1t[5] := '5) Large radius protostar';
 Mass_1t[6] := '6) Convective protostar';
 Mass_1t[7] := '7) Convective protostar';
 Mass_1t[8] := '8) Convective protostar';
 Mass_1t[9] := '9) Decreasing Luminosity';
 Mass_1t[10] := '10) Thermonuclear reactions begin';
 Mass_1t[11] := '11) Zero age main sequence';
 Mass_1t[12] := '12) Hydrogen burning in the core';
 Mass_1t[13] := '13) Hydrogen core depleted, H shell burning';
 Mass_1t[14] := '14) H shell burning; convection dominates';
 Mass_1t[15] := '15) Helium flash; He core burning; Red Giant';
 Mass_1t[16] := '16) Hellium core burning';
 Mass_1t[17] := '17) He and H shell burning; thermal pulses';
 Mass_1t[18] := '18) Outer layers expelled';
 Mass_1t[19] := '19) Planetary nebula with central star';
 Mass_1t[20] := '20) Reactions stop; white dwarf; cooling';


 WITH MyMenu DO CASE rowChosen OF
  1: BEGIN {show stages diagram}
      comment (0.05,0.96,
       'Schematic Showing Trends in the Stages of Evolution; <Click> to Advance');
      SetUpStagesHotKeys;  

      Plotcolor :=2;
      GraphBackColor := Blue;
      DefineViewport(6, 0.92, 0.98, 0.15, 0.90);  {put up time bar}
      DefineScale(6,0.0,1.0,0.0,1.0); 
      SelectScale(6);
      OpenViewPort(6);
      Print(1,1,'time');
      ytimelast := 0.01;

      InitializeHRPlot(1.5); {set up main plot}

      t := Log(10.0,3000/TwoTo14);  {plot ZAMS line, axis is inverted}
      l := Log(10.0,0.001);
      tu := Log(10.0,73000/TwoTo14);
      lu := Log(10.0,16210676.0);
      SetColor(whitte);
      PlotLine(t,l,tu,lu);
      Print(7,2,'ZAMS');

      Print(20,2,'1.0 solar mass');
      SetColor(yellow);              {put up label}
      Print(15,4,Mass_1t[1]);
      Print(20,5,concat(NumStr(Mass_1[1,1],10,0),' years'));
      Print(55,14,'Start:');
      PlotSymbol(Log(10.0,Mass_1[1,3]),Log(10.0,Mass_1[1,2]),'*');

      hits := 0;      {number of speed up hits}
      hkey := 0;
      i := 2;         {put up timeline, pause on each step}
      REPEAT

       Dynamicpause;
       CheckForEvents;
       ClearMKBuffers;
       IF(HRHotKeys.Pressed(hkey))THEN BEGIN END;

       PlotColor := PlotColor+1;    {update the plot color}
       IF(PlotColor = 3)THEN PlotColor :=7;   {skip}
       IF(PlotColor = 8)THEN PlotColor :=10;   {skip}
       IF(PlotColor > 15)THEN PlotColor :=2;
       SetColor(PlotColor);         {set the plot color}

      SelectViewport(6);           {update the time bar}
      SelectScale(6);
      ytime := 0.01+0.94*Mass_1[i,1]/Mass_1[Mass_1n,1];
      PlotLine(0.1,ytime,0.9,ytime);
      FOR j := 10 TO 90 DO
      BEGIN
         xtime := j/100.0;
         PlotLine(xtime,ytimelast,xtime,ytime);
      END; {DO}
      ytimelast := ytime;
      SelectViewport(5);
      SelectScale(5);


        RubOut(15,4,44,blue);
        Print(15,4,Mass_1t[i]);
        RubOut(20,5,40,blue);
        RubOut(20,6,40,blue);
        IF(Mass_1[i,1]<1e5) THEN
              Print(20,5,concat(NumStr(Mass_1[i,1],10,0),' years from start'));
        IF((Mass_1[i,1]>=1e5) AND (Mass_1[i,1]<1e9)) THEN
              Print(20,5,concat(NumStr((Mass_1[i,1]/1e6),10,2),
                       ' Million years from start'));
        IF(Mass_1[i,1]>=1e9) THEN
              Print(20,5,concat(NumStr((Mass_1[i,1]/1e9),10,2),
                       ' Billion years from start'));
        IF(i <> Mass_1n)THEN
         BEGIN
          IF(Mass_1[i+1,1]-Mass_1[i,1] < 1e6) THEN
              Print(20,6,concat(NumStr((Mass_1[i+1,1]-Mass_1[i,1]),10,0),
                       ' years for this stage'));
          IF(Mass_1[i+1,1]-Mass_1[i,1] >= 1e6) THEN
              Print(20,6,concat(NumStr((Mass_1[i+1,1]-Mass_1[i,1])/1e6,10,2),
                       ' Million years for this stage'));
         END;

        Plotline(Log(10.0,Mass_1[i-1,3]),Log(10.0,Mass_1[i-1,2]),
                 Log(10.0,Mass_1[i,3]),Log(10.0,Mass_1[i,2]));
        PlotSymbol(Log(10.0,Mass_1[i,3]),Log(10.0,Mass_1[i,2]),'*');
        i := i+1;
        IF(SoundOnOff)THEN Beep;
     UNTIL ((i > Mass_1n) OR (hkey = 1));

     IF(hkey <>1)THEN     {ESC?- else put up last part of plot}
        BEGIN
          xx := Log(10.0,Mass_1[Mass_1n,3]);
          yy := Log(10.0,Mass_1[Mass_1n,2]);
          PlotSymbol(xx,yy,'*');            {put up last point and arrow}
          PlotLine(xx,yy,xx-0.1,yy-0.9);    {draw arrow}
          PlotLine(xx-0.1,yy-0.9,xx-0.1,yy-0.5);    {draw arrow}
          PlotLine(xx-0.1,yy-0.9,xx-0.03,yy-0.7);    {draw arrow}
          Print(5,14,'End:');
        END;
     END; {show stages diagram}

  2: ExplainStagesMenu;

 END; {CASE}
END;   {Stages}

{---------------------------------------------------------------------------}

PROCEDURE HandleMenu;

 BEGIN
  ClearMUPPETPort;
  WITH mymenu DO CASE colChosen OF

    1: FileMenuItem;

    2: Stages;

    3: HRdiagram;

    4: Boundaryconditions;

    5: Protostar;

    6: Compute;  {Main Sequence}

   END; {CASE}

    MyMenu.Display; {make sure menu is up}
    SetUpHotKeys;

END;  {HandleMenu}

{---------------------------------------------------------------------------}

PROCEDURE HotKeyHandle (key :byte);

BEGIN
   CASE key OF
    1 : Help1;
    2 : BEGIN {menu}
          CheckForEvents;
          IF MyMenu.Chosen THEN HandleMenu;
        END;
   END; {case}
END;

{===========================================================================}
(*-----              ****    MAIN PROGRAM    ****                     -----*)
{---------------------------------------------------------------------------}

{This program controls the flow of the main menu}

VAR
 key   :Byte;

BEGIN {Main}
 CUPSinit;

 {new(hotkeys);}       {needed on Mac}
 {new(HRhotkeys);}     {needed on Mac}
 {new(Protohotkeys);}  {needed on Mac}
 {new(mymenu);}        {needed on Mac}

 InitializeModel;
 SetUpHotKeys;
 SetUpMyMenu;
 AboutEvolve;

 REPEAT
  CheckForEvents;
  IF HotKeys.Pressed(key) THEN HotKeyHandle(key);
  IF MyMenu.Activated THEN HandleMenu;
 UNTIL false; {repeat forever}
END.
