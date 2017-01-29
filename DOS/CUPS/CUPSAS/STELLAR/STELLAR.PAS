PROGRAM STELLAR;

{******************************************************************}
{*    Computes the Structure of a Chemically Homogeneous Star     *}
{*    in Thermal and Hydrostatic Equilibrium                      *}
{*                                                                *}
{*     R.T. Kouzes    Copyright @ 1991, 1992, 1993                *}
{*        R.T. Kouzes, rt_kouzes@pnl.gov                          *}
{*        CUPS Project. Stellar REV 1.3    July  3, 1992          *}
{*        CUPS Project. Stellar REV 1.5    June 30, 1993          *}
{*        CUPS Project. Stellar REV 2.0    June 26, 1994          *}
{*                                                                *}
{*     This program integrates 4 differential equations for the   *}
{*     star, using 2 boundary conditions at the surface and 2     *}
{*     boundary conditions at the center, determining 4 parameters*}
{*     for the star - central temperature, central density,       *}
{*     radius  and Luminosity. Units are cgs.                     *}
{*        References are given in code by initials:               *}
{*          KW: Kippenhahn and Weigert "Stellar Structure"        *}
{*          BP: Bohdan Paczynski, Princeton U., private comm.     *}
{*          AC: A.N. "Cox Stellar Structure"                      *}
{*     The computational approach used here follows that developed*}
{*     by Bohdan Paczynski                                        *}
{******************************************************************}

{---------------------------------------------------------------------------}
{*----- The following is a list of all Proceedures in this Program  -------*}
{---------------------------------------------------------------------------}

{*PROCEDURE InitializeModel;                                 *}

(*-----   Graphics Proceedures                        -------*)

{*PROCEDURE InitializeModelPlot;                             *}
{*PROCEDURE InitializeMoreModelPlot;                         *}
{*PROCEDURE InitializeEnergyPlot;                            *}
{*PROCEDURE InitializeEqStatePlot;                           *}
{*PROCEDURE InitializeMoreEqStatePlot;                       *}
{*PROCEDURE InitializeOpacityPlot;                           *}
{*PROCEDURE InitializeMoreOpacityPlot;                       *}
{*PROCEDURE PlotDataModel;                                   *}
{*PROCEDURE MessageXY;                                       *}

(*-----   Procedures for Physics Calculations         -------*)

{*PROCEDURE NuclearPower(xx,yy,zz,zn,d,t :Double);           *}
{*PROCEDURE NuclearPowerAlpha(xx,yy,zz,zn,d,t :Double);      *}
{*PROCEDURE Opacity(xx,yy,zz,d,t :Double);                   *}
{*PROCEDURE EquationOfState(xx,yy,zz,d,t :Double);           *}
{*PROCEDURE EvaluateEquations(VAR Params, Deriv :Vector5);   *}
{*PROCEDURE IntegralStep(VAR Parameters : Vector5);          *}
{*PROCEDURE Outward(Tcent, Dcent :Double; VAR Core : Vector5;*}
{*                  VAR Error :Integer);                     *}
{*PROCEDURE Inward(Teff, Lum :Double; VAR Surf :Vector5;     *}
{*                 VAR Error :Integer);                      *}
{*PROCEDURE CorrectBoundary(VAR deriv :Array5x5);            *}
{*PROCEDURE StellarModel(VAR Error :Integer; Learning :Boolean);*}

(*-----   Input Screen Menu Procedures                -------*)

{*PROCEDURE ReplaceZero;                                     *}
{*FUNCTION  NumStrExp(Num: Double; width : Integer): String; *}
{*PROCEDURE SetUpHotKeys;                                    *}
{*PROCEDURE SetUpMyMenu;                                     *}
{*PROCEDURE LoadMainMenu;                                    *}
{*PROCEDURE LoadStellarConditionsMenu;                       *}
{*PROCEDURE LoadEquationsMenu;                               *}
{*PROCEDURE LoadBoundaryConditionsMenu;                      *}
{*PROCEDURE LoadComplicationsMenu;                           *}
{*PROCEDURE LoadComputeMenu;                                 *}
{*PROCEDURE FillComputeMenu;                                 *}
{*PROCEDURE LoadResultsMenu;                                 *}
{*PROCEDURE LoadOpacityMenu;                                 *}
{*PROCEDURE FillOpacityMenu;                                 *}
{*PROCEDURE LoadChemicalMenu;                                *}
{*PROCEDURE FillChemicalMenu;                                *}
{*PROCEDURE LoadEnergyMenu;                                  *}
{*PROCEDURE FillEnergyMenu;                                  *}
{*PROCEDURE LoadConvectionDegeneracyMenu;                    *}
{*PROCEDURE LoadEqStateMenu;                                 *}
{*PROCEDURE FillEqStateMenu;                                 *}
{*PROCEDURE LoadNeutrinosMenu;                               *}
{*PROCEDURE GetVal(VAR localcase :Integer);                  *}
{*PROCEDURE ShowWaitMenu;                                    *}
{*PROCEDURE ShowSubMenu;                                     *}
{*PROCEDURE ShowCalcMenu;                                    *}
{*PROCEDURE ShowAcceptMenu;                                  *}
{*PROCEDURE Help1;                                           *}

(*-----   Menu Response handling Procedures           -------*)

{*PROCEDURE StellarConditions;                               *}
{*PROCEDURE Equations;                                       *}
{*PROCEDURE BoundaryConditions;                              *}
{*PROCEDURE Compute;                                         *}
{*PROCEDURE FileMenuItem;                                    *}
{*PROCEDURE PowerGeneration;                                 *}
{*PROCEDURE HandleEqofState;                                 *}
{*PROCEDURE Complications;                                   *}
{*PROCEDURE AboutStellar;                                    *}
{*PROCEDURE GettingStarted;                                  *}
{*PROCEDURE HandleMenu;                                      *}
{*PROCEDURE HotKeyHandle;                                    *}
{* then comes the main program                               *}


{---------------------------------------------------------------------------}
{*-----        Declare all global variables and constants           -------*}
{---------------------------------------------------------------------------}

{for Mac delete crt,dos,printer}
USES  crt,dos,printer,graph,CUPSmupp,CUPS,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph;

CONST

 c = 2.998e10;            {speed of light cm/s}
 G = 6.67e-8;             {gravitational constant cm^3/g/s^2}
 sigma = 5.67e-5;         {Boltzmannconstant erg/cm^2/s/K^4}
 astrounit = 1.4e13;      {Astronomical unit cm}

 nitrogenfraction = 0.3;  {fraction of N in heavy ions}
 SunMass = 1.99e33;       {mass of Sun in grams}
 SunLum  = 3.83e33;       {Luminosity of Sun in erg/sec}
 SunRadius = 6.96e10;     {radius of Sun in cm}
 SunAge = 4.6e9;          {age of sun in years}
 SunTcentral = 15.5e6;    {central T of sun in K}
 SunTsurface = 5800;      {surface T of sun in K}
 SunDcentral = 147.2;     {central density of sun g/cc}
 SunX = 0.71;             {x of sun}
 SunY = 0.27;             {y of sun}
 SunZ = 0.02;             {z of sun}
 EnergyHtoHe = 7e18;      {E release by converting 1g H into He}

 Keepsize = 50;           {array size for keeping profiles}

TYPE
 Vector5  = Array[1..5]      of Real;
 Array5x5 = Array[1..5,1..5] of Real;

VAR
 MyMenu        :TMenu;   {Main Menu}
 precision     :Real;  {desired fit accuracy at fitting point}
 ModelPrecision :Real; {desired accuracy in Stellar Model=10*precision}
 ModIteration  :Integer; {iteration counter in Stellar Model}
 PlotControl   :Real;  {Plot control:0=none,1=yes}
 PlotMassOrRadius:Boolean; {Plots are versus: false=mass, true=radius}
 TwoTo14       :Real;  {PWR(2,.25)}
 ComputedModel :Integer; {0 unless calculated values are current}
 Hotkeys       :THotKeys;{hot keys}
 PlotColor     :Integer; {color value used on data screen in compute}
 SoundOnOff    :Boolean; {flag for beep on/off}
 xLast         :Array[1..6] of Real; {last plotted x,y pair}
 yLast         :Array[1..6] of Real; {last plotted x,y pair}


 x             :Real; {Hydrogen by mass fraction }
 y             :Real; {Helium by mass fraction}
 z             :Real; {Heavy ions by mass fraction}
 znitrogen     :Real; {Nitrogen by mass fraction}
 density       :Real; {Density g/cc}
 temperature   :Real; {Temperature}

 Mass          :Real; {Mass of star}
 Radius        :Real; {Effective radius of star surface}
 Luminosity    :Real; {Luminosity of star surface}
 Tsurface      :Real; {Surface temperature}
 Teffective    :Real; {Effective temperature of surface}
 Tcentral      :Real; {Central temperature of star}
 Dcentral      :Real; {Central density of star}

 FitMass       :Real; {mass where match inward and outward integs}
 FitMassFraction :Real;{mass fraction for fitmass}
 Sundensity    :Real; {average sun density}
 Constgrrad    :Real; {constant sun lum/sun mass/(16*pi*c*G)}
 ConstdPdM     :Real; {constant G/4/pi*(Sun mass/Sun radius**2)**2}

 kopacity      :Real; {Opacity cm^2/g}
 kelectron     :Real; {electron scattering opacity}
 kKramer       :Real; {Kramers opacity}
 kHminus       :Real; {H minus opacity}
 kmolecular    :Real; {molecular opacity}
 kradiative    :Real; {total radiative opacity}
 kconductive   :Real; {electron conductivity opacity}

 powerpp       :Real; {Power generated by p-p}
 powerCNO      :Real; {Power generated by CNO}
 power3alpha   :Real; {Power generated by 3 alpha burning}
 power         :Real; {Power generated by p-p + CNO}
 dxdt          :Real; {rate of Hydrogen depletion}
 dydt          :Real; {rate of Helium depletion}

 pressure      :Real; {total p, radiation+ion+electron (dyne/cm^2)}
 dLogPdLogT    :Real; {d log(p)/d log(T) at constant density}
 dLogPdLogd    :Real; {d log(p)/d log(density) at constant T}
 radpressure   :Real; {radiation pressure}
 ionpressure   :Real; {ion pressure}
 electpressure :Real; {electron pressure}
 gaspressure   :Real; {gas pressure, ion+electron}
 DegRpressure  :Real; {pressure degenerate relativistic electrons}
 DegNRpressure :Real; {pressure non-relativistic degenerate electrons}
 Degpressure   :Real; {combined pressure of degenerate electrons}
 Nonpressure   :Real; {non-degen electron pressure, perfect gas law}
 dLnTdLnPrad   :Real; {radiative temperature gradiant}
 dLnTdLnPad    :Real; {d logT/d logP const entorpy; adiabatic T grad}

 corevals      :Vector5; {Core values of fit parameters}
 surfacevals   :Vector5; {Envelope values of fit parameters}
 corevalsT     :Vector5; {Core values with T varied}
 surfacevalsT  :Vector5; {Envelope values with T varied}
 corevalsD     :Vector5; {Core values with Density varied}
 surfacevalsL  :Vector5; {Envelope values with Lum varied}
 maxparamstep  :Vector5; {Maximum step size for each parameter}
 corrections   :Vector5; {corrections computed by CorrectBoundary}
 maxcorrect    :Vector5; {max correct applied to parms in CorrectBoundary}
 derivitive    :Array5x5;{Derivitive of fit point vals}

 M_array       :Array[0..Keepsize] of Real; {mass profile}
 T_array       :Array[0..Keepsize] of Real; {temperature profile}
 D_array       :Array[0..Keepsize] of Real; {density profile}
 R_array       :Array[0..Keepsize] of Real; {radius profile}
 L_array       :Array[0..Keepsize] of Real; {Luminosity profile}
 K_array       :Array[0..Keepsize] of Real; {opacity profile}
 E_array       :Array[0..Keepsize] of Real; {energy rate profile}
 P_array       :Array[0..Keepsize] of Real; {pressure profile}

{---------------------------------------------------------------------------}
{*-----               Initialization                                  -----*}
{---------------------------------------------------------------------------}

PROCEDURE InitializeModel;
{ This routine initializes: the global values for the stars parameters
to those of the Sun; some constants used in physics equations; arrays
of parameters used in the fitting procedure; zeros out the arrays of the
stars interior condition as a function of mass (radius) which are filled
as the model computes}

VAR
 i :Integer;

BEGIN
 TwoTo14 := PWR(2.0,0.25);  {2**(0.25)}
 SoundOnOff := FALSE;
 ComputedModel := 0;
 PlotMassOrRadius := FALSE;  {Default plots versus Mass=false}

 x := SunX;  {Hydrogen mass fraction}
 y := SunY;  {Helium mass fraction}
 z := SunZ;  {Heavy mass fraction}
 znitrogen := z*nitrogenfraction;  {N mass fraction}

 Mass := 1.0; {mass of star in solar units}
 Luminosity := 0.51;  {Luminosity in solar units}
 Tsurface := 4250; {surface T}
 Teffective := Tsurface*TwoTo14; {effective T}
 Tcentral := 1.31e7; {central T}
 Dcentral := 66; {central density}

 FitMassFraction := 0.5;  {mass fraction for fit mass - was 1/3}
 FitMass := mass*FitMassFraction;  {mass where inward and outward matched}

 density := SunDcentral;  {density}
 temperature := SunTcentral; {temperature}

 Constgrrad  := (SunLum/SunMass)/(16.0*pi*c*G); {used in EvaluateEq}
 ConstdPdM   := G/4.0/pi*SQR(Sunmass/SQR(Sunradius)); {used in EvaluateEq}
 Sundensity  := SunMass/(4*pi*Sunradius*Sunradius*Sunradius/3); {Ave Sun dnsty}

 maxparamstep[1] := 0.20; {Maximum step size for temperature - was .05}
 maxparamstep[2] := 0.20; {Maximum step size for density - was .15}
 maxparamstep[3] := 0.20; {Maximum step size for radius - was .05}
 maxparamstep[4] := 0.20; {Maximum step size for Luminosity}
 maxparamstep[5] := 0.20; {Maximum step size for mass}

 precision       := 0.001; {desired fit accuracy at fitting point}
 ModelPrecision  := 10*precision;  {desired accuracy in Stellar Model}

 maxcorrect[1] := 0.03; {Teff}{max correction to be applied to each param}
 maxcorrect[2] := 0.10; {Lum }{in CorrectBoundary}
 maxcorrect[3] := 0.02; {Tcen - was .01}
 maxcorrect[4] := 0.10; {Dcen}
 maxcorrect[5] := 0.00; {Mass}

 FOR i:=0 to keepsize DO    {Clear arrays}
  BEGIN
    M_array[i] :=0;
    T_array[i] :=0;
    D_array[i] :=0;
    R_array[i] :=0;
    L_array[i] :=0;
    K_array[i] :=0;
    E_array[i] :=0;
    P_array[i] :=0;
  END; {FOR}

END;

{---------------------------------------------------------------------------}
{*-----               Graphics Procedures                            -----*}
{---------------------------------------------------------------------------}

PROCEDURE InitializeModelPlot(numplot :Integer);
 {set up the plots for Compute, numplot=1..4 for seperate or 5 for all}

VAR
 i :Integer;
 s :Real;
 sb :Real;
BEGIN
 GraphBackColor := Blue;
 SetLineStyle(0,0,1);
 SetColor(LightGreen);

 FOR i:=1 to 6 DO    {Clear arrays}
  BEGIN
    xLast[i]:= 0;
    yLast[i]:= 0;
  END; {FOR}

 IF(numplot = 5)THEN
  BEGIN
   DefineViewport(1, 0.10, 0.45, 0.13, 0.45);  {bottom left}
   DefineViewport(2, 0.10, 0.45, 0.55, 0.87);  {top left}
   DefineViewport(3, 0.60, 0.95, 0.13, 0.45);  {bottom right}
   DefineViewport(4, 0.60, 0.95, 0.55, 0.87);  {top right}
  END
 ELSE
  BEGIN
   DefineViewport(numplot, 0.10, 0.90, 0.13, 0.87);  {whole screen}
 END;

IF((numplot = 5) OR (numplot = 1))THEN
BEGIN
 s := Log(10.0,Tcentral*10.0); {find upper scale}
 DefineScale(1,0.0,1.0,3.0,s); {T.vs.M scale}
 SelectScale(1);
 OpenViewPort(1);
 Axis(0.0,3.0,0.2,1.0);
 PutLabel(Left,'Log(Temperature)');
 IF(PlotMassOrRadius)THEN BEGIN PutLabel(Bottom,'R/Rstar'); END
                          ELSE BEGIN PutLabel(Bottom,'M/Mstar') END;
END;

IF((numplot = 5) OR (numplot = 2))THEN
BEGIN
 s := Log(10.0,Dcentral*10.0); {find upper scale}
 DefineScale(2,0.0,1.0,-1.0,s); {density.vs.M scale}
 SelectScale(2);
 OpenViewPort(2);
 Axis(0.0,-1.0,0.2,1.0);
 PutLabel(Left,'Log(Density)');
END;

IF((numplot = 5) OR (numplot = 3))THEN
BEGIN
 s := Log(10.0,radius*10.0); {find upper scale}
 DefineScale(3,0.0,1.0,-2.0,s); {radius.vs.M scale}
 SelectScale(3);
 OpenViewPort(3);
 Axis(0.0,-2.0,0.2,1.0);
 PutLabel(Left,'Log(Radius)');
 IF(PlotMassOrRadius)THEN BEGIN PutLabel(Bottom,'R/Rstar'); END
                          ELSE BEGIN PutLabel(Bottom,'M/Mstar') END;
END;

IF((numplot = 5) OR (numplot = 4))THEN BEGIN
 s := Log(10.0,Luminosity*10.0); {find upper scale}
 sb :=s-3;
 DefineScale(4,0.0,1.0,sb,s); {Luminosity.vs.M scale}
 SelectScale(4);
 OpenViewPort(4);
 Axis(0.0,-10,0.2,1.0);
 PutLabel(Left,'Log(Luminosity)');
END;

 SetColor(Whitte);

 PlotColor :=2;              {Set up colors for cycling during calcultion}

END;

{---------------------------------------------------------------------------}

PROCEDURE InitializeMoreModelPlot(numplot :Integer);
 {set up more plots for Compute, numplot=1..4 for seperate or 5 for all}

VAR
 i :Integer;
 s :Real;
 sb :Real;
BEGIN
 GraphBackColor := Blue;

 IF(numplot = 5)THEN
  BEGIN
   DefineViewport(1, 0.10, 0.45, 0.13, 0.45);  {bottom left}
   DefineViewport(2, 0.10, 0.45, 0.55, 0.87);  {top left}
   DefineViewport(3, 0.60, 0.95, 0.13, 0.45);  {bottom right}
   DefineViewport(4, 0.60, 0.95, 0.55, 0.87);  {top right}
  END
 ELSE
  BEGIN
   DefineViewport(numplot, 0.10, 0.90, 0.13, 0.87);  {whole screen}
 END;

 SetLineStyle(0,0,1);
 SetColor(LightGreen);

IF((numplot = 5) OR (numplot = 1))THEN
BEGIN
 s :=4;
 IF(K_array[1]>0)THEN s := Log(10.0,K_array[1]*10.0); {find upper scale}
 DefineScale(1,0.0,1.0,s-2,s); {K.vs.M scale}
 SelectScale(1);
 OpenViewPort(1);
 Axis(0.0,-4,0.2,0.5);
 PutLabel(Left,'Log(Opacity)');
 IF(PlotMassOrRadius)THEN BEGIN PutLabel(Bottom,'R/Rstar'); END
                          ELSE BEGIN PutLabel(Bottom,'M/Mstar') END;
END;

IF((numplot = 5) OR (numplot = 2))THEN
BEGIN
 IF(E_array[1]>0)THEN s := Log(10.0,E_array[1]*10.0); {find upper scale}
 DefineScale(2,0.0,1.0,s-4,s); {energy.vs.M scale}
 SelectScale(2);
 OpenViewPort(2);
 Axis(0.0,-4,0.2,1.0);
 PutLabel(Left,'Log(Power/gram)');

 SetColor(LightGreen); {total power}
 Print(1,1,'pp+CNO');
 SetColor(Yellow); {pp power}
 Print(13,1,'pp');
 SetColor(LightGreen);
END;

IF((numplot = 5) OR (numplot = 3))THEN
BEGIN
 IF(P_array[1]>0)THEN s := Log(10.0,P_array[1]*10.0); {find upper scale}
 DefineScale(3,0.0,1.0,s-4,s); {pressure.vs.M scale}
 SelectScale(3);
 OpenViewPort(3);
 Axis(0.0,-4,0.2,1.0);
 PutLabel(Left,'Log(Pressure)');
 IF(PlotMassOrRadius)THEN BEGIN PutLabel(Bottom,'R/Rstar'); END
                          ELSE BEGIN PutLabel(Bottom,'M/Mstar') END;
END;

IF((numplot = 5) OR (numplot = 4))THEN
BEGIN
 DefineScale(4,0.0,1.0,-2,2); {dLnTdLnP.vs.M scale}
 SelectScale(4);
 OpenViewPort(4);
 Axis(0.0,-2,0.2,1.0);
 PutLabel(Left,'Log[d ln(T)/d ln(P)]');

 SetColor(LightGreen);
 Print(1,1,'Adiabatic');
 SetColor(Yellow);
 Print(13,1,'Radiative');
END;

 SetColor(Whitte);
END;

{---------------------------------------------------------------------------}

PROCEDURE InitializeEnergyPlot;
  {plot for Power generation}

VAR
 s  :Real;
 s1 :Real;
BEGIN
 GraphBackColor := Blue;

 DefineViewport(5, 0.10, 0.90, 0.13, 0.87);  {whole screen}

 SetLineStyle(0,0,1);
 SetColor(LightGreen);

 s  := 10.0; {find upper scale}
 s1 := Log(10.0,Power);
 DefineScale(5,6.5,s,-2.0,s1); {E.vs.T scale}
 SelectScale(5);
 OpenViewPort(5);
 Axis( 0, -10, 0.5, 5.0);
 PutLabel(Left,'Log(Power/gram)');
 PutLabel(Bottom,'Log(Temperature)');
 Print(19,14,'pp power');
 SetColor(yellow);
 Print(14,17,'CNO power');
 SetColor(lightgray);
 Print(43,10,'3 alpha power');
 SetColor(Whitte);
 Print(27,5,'total power');
END;

{---------------------------------------------------------------------------}

PROCEDURE InitializeEqStatePlot(VAR d :Real);
 {plots for Eq of State}

VAR
 s  :Real;
 s1 :Real;
BEGIN

 GraphBackColor := Blue;

 DefineViewport(1, 0.10, 0.45, 0.15, 0.47);  {bottom left}
 DefineViewport(2, 0.10, 0.45, 0.57, 0.89);  {top left}
 DefineViewport(3, 0.60, 0.95, 0.15, 0.47);  {bottom right}
 DefineViewport(4, 0.60, 0.95, 0.57, 0.89);  {top right}

 SetLineStyle(0,0,1);
 SetColor(LightGreen);

 s := 10.0; {find upper scale}
 s1 :=3;
 DefineScale(1,s1,s,12,20); {Pion.vs.T scale}
 SelectScale(1);
 OpenViewPort(1);
 Axis(s1,12,1.0,1.0);
 PutLabel(Left,'Log(Ion Pressure)');
 PutLabel(Bottom,'Log(Temperature)');

 DefineScale(2,s1,s,12,20); {deg e pressure.vs.T scale}
 SelectScale(2);
 OpenViewPort(2);
 Axis(s1,12,1.0,1.0);
 PutLabel(Left,'Log(Deg e Pressure)');

 Print(1,1,concat('Density=',NumStr(d,7,2)));
 SetColor(Yellow);
 Print(1,2,concat('Density=',NumStr(d/10,7,2)));
 SetColor(LightGreen);

 DefineScale(3,s1,s,12,20); {p nondeg pressure.vs.T scale}
 SelectScale(3);
 OpenViewPort(3);
 Axis(s1,12,1.0,1.0);
 PutLabel(Left,'Log(Non-deg e Pressure)');
 PutLabel(Bottom,'Log(Temperature)');

 DefineScale(4,s1,s,12,20); {e pressure.vs.T scale}
 SelectScale(4);
 OpenViewPort(4);
 Axis(s1,12,1.0,1.0);
 PutLabel(Left,'Log(Electron Pressure)');

 SetColor(Whitte);
END;

{---------------------------------------------------------------------------}

PROCEDURE InitializeMoreEqStatePlot(VAR d :Real);
 {more plots for Eq of State}

VAR
 s  :Real;
 s1 :Real;
BEGIN

 GraphBackColor := Blue;

 DefineViewport(1, 0.10, 0.45, 0.15, 0.47);  {bottom left}
 DefineViewport(2, 0.10, 0.45, 0.57, 0.89);  {top left}
 DefineViewport(3, 0.60, 0.95, 0.15, 0.47);  {bottom right}
 DefineViewport(4, 0.60, 0.95, 0.57, 0.89);  {top right}

 SetLineStyle(0,0,1);
 SetColor(LightGreen);

 s := 10.0; {find upper scale}
 s1 :=3;
 DefineScale(1,s1,s,-10,10); {radiative dT/dP.vs.T scale}
 SelectScale(1);
 OpenViewPort(1);
 Axis(s1,-10,1.0,2.0);
 PutLabel(Left,'Log(dLn(T)/dLn(p) rad)');
 PutLabel(Bottom,'Log(Temperature)');

 DefineScale(2,s1,s,-1,1); {adiabatic dT/dP.vs.T scale}
 SelectScale(2);
 OpenViewPort(2);
 Axis(s1,-1,1.0,0.5);
 PutLabel(Left,'Log(dLn(T)/dLn(p) ad)');

 Print(1,1,concat('Density=',NumStr(d,7,2)));
 SetColor(Yellow);
 Print(1,2,concat('Density=',NumStr(d/10,7,2)));
 SetColor(LightGreen);

 DefineScale(3,s1,s,14,26); {total p.vs.T scale}
 SelectScale(3);
 OpenViewPort(3);
 Axis(s1,14,1.0,2.0);
 PutLabel(Left,'Log(Total Pressure)');
 PutLabel(Bottom,'Log(Temperature)');

 DefineScale(4,s1,s,-2,25); {radiative pressure.vs.T scale}
 SelectScale(4);
 OpenViewPort(4);
 Axis(s1,-4,1.0,4.0);
 PutLabel(Left,'Log(Radiative Pressure)');

 SetColor(Whitte);
END;

{---------------------------------------------------------------------------}

{on Mac need $S stellar1}

PROCEDURE InitializeOpacityPlot(VAR d :Real);
 {plots for Opacity}

VAR
 s  :Real;
 s1 :Real;
BEGIN

 GraphBackColor := Blue;

 DefineViewport(1, 0.10, 0.45, 0.15, 0.47);  {bottom left}
 DefineViewport(2, 0.10, 0.45, 0.57, 0.89);  {top left}
 DefineViewport(3, 0.60, 0.95, 0.15, 0.45);  {bottom right}
 DefineViewport(4, 0.60, 0.95, 0.57, 0.89);  {top right}

 SetLineStyle(0,0,1);
 SetColor(LightGreen);

 s := 10.0; {find upper scale}
 s1 :=3;
 DefineScale(1,s1,s,-8,2); {k.vs.T scale}
 SelectScale(1);
 OpenViewPort(1);
 Axis(s1,-8.0,1.0,2.0);
 PutLabel(Left,'Log(Electron Opacity)');
 PutLabel(Bottom,'Log(Temperature)');

 DefineScale(2,s1,s,-10,15); {k.vs.T scale}
 SelectScale(2);
 OpenViewPort(2);
 Axis(s1,-12.0,1.0,4.0);
 PutLabel(Left,'Log(Kramers Opacity)');

 Print(1,1,concat('        Density=',NumStr(d,7,2)));
 SetColor(Yellow);
 Print(1,2,concat('        Density=',NumStr(d/10,7,2)));
 SetColor(LightGreen);

 DefineScale(3,s1,s,-5,15); {k.vs.T scale}
 SelectScale(3);
 OpenViewPort(3);
 Axis(s1,-6.0,1.0,3.0);
 PutLabel(Left,'Log(H- Opacity)');
 PutLabel(Bottom,'Log(Temperature)');

 DefineScale(4,s1,s,-8,-2); {k.vs.T scale}
 SelectScale(4);
 OpenViewPort(4);
 Axis(s1,-8.0,1.0,1.0);
 PutLabel(Left,'Log(Molecular Opacity)');

 SetColor(Whitte);
END;

{---------------------------------------------------------------------------}

PROCEDURE InitializeMoreOpacityPlot(VAR d :Real);
 {plots for Opacity}

 VAR
 s  :Real;
 s1 :Real;
BEGIN

 GraphBackColor := Blue;

 DefineViewport(1, 0.10, 0.45, 0.15, 0.47);  {bottom left}
 DefineViewport(2, 0.10, 0.45, 0.55, 0.89);  {top left}
 DefineViewport(3, 0.60, 0.95, 0.15, 0.47);  {bottom right}
 DefineViewport(4, 0.60, 0.95, 0.57, 0.89);  {top right}

 SetLineStyle(0,0,1);
 SetColor(LightGreen);

 s := 10.0; {find upper scale}
 s1 :=3;
 DefineScale(1,s1,s,-5,10); {k.vs.T scale}
 SelectScale(1);
 OpenViewPort(1);
 Axis(s1,-5.0,1.0,3.0);
 PutLabel(Left,'Log(Radiative Opacity)');
 PutLabel(Bottom,'Log(Temperature)');

 DefineScale(2,s1,s,-5,12); {k.vs.T scale}
 SelectScale(2);
 OpenViewPort(2);
 Axis(s1,-5.0,1.0,2.0);
 PutLabel(Left,'Log(Conductive Opacity)');

 Print(1,1,concat('Density=',NumStr(d,7,2)));
 SetColor(Yellow);
 Print(1,2,concat('Density=',NumStr(d/10,7,2)));
 SetColor(LightGreen);

 DefineScale(3,s1,s,-5,5); {k.vs.T scale}
 SelectScale(3);
 OpenViewPort(3);
 Axis(s1,-5.0,1.0,2.0);
 PutLabel(Left,'Log(Total Opacity)');
 PutLabel(Bottom,'Log(Temperature)');

 SetColor(Whitte);
END;

{---------------------------------------------------------------------------}

PROCEDURE PlotDataModel(screen :Integer;VAR x :Real; y :Real);
  {plot data point on screen in Compute/inward/outward}
VAR
   xVal,yVal : integer;
BEGIN
 SelectScale(screen);
 SelectViewPort(screen);
 IF((ABS(x-xLast[screen]) < 0.1) AND (xLast[screen] <> 0))
       THEN PlotLine(xLast[screen],yLast[screen],x,y); {must limit so doesn't jump}
 xLast[screen] := x;
 yLast[screen] := y;
END;

{---------------------------------------------------------------------------}

PROCEDURE ReplaceZero(VAR temp :String);
 {replace one blanks with zero in a string}

VAR
 i     :Integer;
BEGIN
 i := Pos(' ',temp);
 IF(i > 0)THEN
  BEGIN
   Delete(temp,i,1);
   Insert('0',temp,i);
  END;
END;

{---------------------------------------------------------------------------}

FUNCTION NumStrExp(Num: Real; width : Integer): String;
 {This produces an exponential format number for display - changed for mac}

VAR
 temp  :String;
 temp1 :String;
 temp2 :String;
 i     :Integer;
 dec   :Integer;
 expon :Integer;

BEGIN
 {str(Num:width, temp);}  {on mac temp := numstr(Num,width,dec);}
 {NumStrExp := temp;   }
 IF(Num <> 0)THEN i := Trunc(Log(10.0,Abs(Num))) ELSE  i := 0; {get exponent}
 expon := 3;      {exponent size}
 IF((i >= 0) AND (i < 100))THEN expon := 2; {make exponent space smaller}
 IF((i >= 0) AND (i < 10)) THEN expon := 1; {make exponent space smaller}
 IF((i < 0)  AND (i > -10))THEN expon := 2; {make exponent space smaller}
 dec := width-expon-4;  {number of decimals}
 IF(dec < 0)THEN dec := 0;
 temp1 := NumStr((Num/Pwr(10.0,i)),(width-1-expon),dec);
 IF((i = 0) OR (i = 1))THEN
   BEGIN
     NumStrExp := NumStr(Num,width,(dec+1+expon)); {if e0, drop it}
    END
   ELSE
    BEGIN
     temp2 := Numstr(i,expon,0);  {make exponent string}
     ReplaceZero(temp2);  {get rid of blanks in exponent}
     ReplaceZero(temp2);
     NumStrExp := concat(temp1,'e',temp2); {make final string}
   END;
END;

{---------------------------------------------------------------------------}
{*-----               Procedures for Physics Calculations           -------*}
{---------------------------------------------------------------------------}

PROCEDURE NuclearPower(VAR xx,yy,zz,zn,d,t :Real); {Nuc Power Generation}

{ Computes p-p and CNO nuclear energy generation rate in stellar material. }
{ Formulas from KW; agree with BP except higher order screen factor}
{ Inputs: x,y,z,znitrogen,density,temperature }
{ Output: powerpp, powerCNO, power (erg/g/sec): global }
{ Output: dxdt (g/g/sec): global Real } {hydrogen depletion rate}

VAR
 t6            :Real; {t6}
 t6to13        :Real; {t6 to 1/3 power;used in NuclearPower}
 t6to23        :Real; {t6 to 2/3 power}
 screenpp      :Real; {screen factor for pp}
 screencno     :Real; {screen factor for cno}
 temporary     :Real;

BEGIN
 powerpp  := 1.0e-30;   {no energy generated at low T}
 powerCNO := 1.0e-30;
 power    := 1.0e-30;

 IF(t > 1.0e6)THEN
  BEGIN
   t6       := t/1.0e6;         {T6}
   t6to13   := Pwr(t6,0.3333);  {T6 to 1/3 power}
   t6to23   := t6to13*t6to13;   {T6 to 2/3 power}
   temporary:= (64.33-152.28/t6to13); {was 64.24, 152.313}
   IF(temporary > -30.0)THEN
     BEGIN
        screencno := 1+0.0027*t6to13-0.00778*t6to23-0.000149*t6;
        IF (screencno < 1.0) THEN screencno := 1.0;
        powerCNO := xx*zn*screencno*d*Exp(temporary)/t6to23;
     END;
   screenpp := 1+0.0123*t6to13+0.0109*t6to23+0.0009*t6;
   powerpp  := xx*xx*screenpp*d*Exp(14.68-33.80/t6to13)/t6to23; {was 14.54,33.81}
   power    := powerpp+powerCNO;
  END; {IF}

 dxdt := -1.667e-19*power; {rate of hydrogen depletion}
END;

{---------------------------------------------------------------------------}

PROCEDURE NuclearPowerAlpha(VAR xx,yy,zz,zn,d,t :Real); {3 alpha Power}

{ Computes 3 alpha nuclear energy generation rate in stellar material. }
{ Formulas from KW; agree with BP except higher order screen factor}
{ Inputs: x,y,z,znitrogen,density,temperature }
{ Output: power3alpha (erg/g/sec): global }
{ Output: dydt (g/g/sec): global Real } {helium depletion rate}

VAR
 t8            :Real;
BEGIN
 power3alpha := 1.0e-30;

 IF(t > 1.0e8)THEN
  BEGIN
   t8 := t/1.0e8;         
   power3alpha := 5.09e11*SQR(d)*PWR(yy/t8,3)*Exp(-44.027/t8);
  END; {IF}

 dydt := -1.7e-18*power3alpha; {rate of helium depletion}
END;

{---------------------------------------------------------------------------}

PROCEDURE Opacity(VAR xx,yy,zz,d,t :Real);              {Opacity computation}

{ Computes opacity of stellar material. }
{ Inputs: x,y,z,density,temperature }
{ Output: opacity (cm^2/g) and opacity components: global}

VAR
 zaverage         :Real;

CONST
 aa = 2; {from BP was 6.0 Kramers}
 bb = 1; {from BP was .001 H-}
 cc = 1; {from BP was .001 molecular}

BEGIN
 zaverage := xx+2*yy+8*zz; {was 4*yy ?}

 {compute electron scattering opacity with high T and density
  corrections - fairly accurate (leading term from KW;
  corrections by BP from AC)}
 kelectron := 0.2*(1+xx)/((1+2.7e11*d/t/t)
              *(1+Pwr((t/4.5e8),0.86)));

 {compute Kramers term: free-free, bound-free, bound-bound
  transitions crudely - the factor A is from BP}
{    kKramer := (13.7/aa)*(1+xx)*(0.0010+zz)*d
             *Pwr((1.0e7/t),3.5);     BP formula}
 kKramer := (4.56/aa)*(1+xx)*(0.0025+zz)*d
             *Pwr((1.0e7/t),3.5);     {Stein formula}

 {compute negative hydrogen ion opacity very crudely - factor b from BP}
 {compute molecular opacity very crudely - factor c from BP}
 IF(t > 4.0e4)THEN
   BEGIN
        kHminus := 65.0*SQRT(zz*d)*Pwr((4.0e4/3000.0),7.7);  {fixed value}
        kmolecular := 1.0e-5;
   END
 ELSE
   BEGIN
        kHminus := 65.0*SQRT(zz*d)*Pwr((t/3000.0),7.7); {BP multiplies this by bb}
        kmolecular := 0.1*zz; {BP multiplies this by cc}
   END;

 {compute total radiative opacity}
 kradiative := kmolecular + 1.0/(1.0/kHminus+1.0/(kelectron+kKramer));

 {compute electron conductivity opacity - fairly accurate}
 IF(d < 1.0e-5)THEN
   BEGIN
        kconductive := 1.0e10;
   END
 ELSE
   BEGIN
        kconductive := zaverage*(2.6e-7)*SQR(t/d)
                       *(1+PWR((d/2.0e6),0.66667));
   END;

 {compute total opacity}
 kopacity := 1/(1/kradiative+1/kconductive);

END;

{---------------------------------------------------------------------------}

PROCEDURE EquationOfState(VAR xx,yy,zz,d,t :Real);  {Calculate Equation of State}

{ Computes eq. of state and thermodynamic quantities in stellar material. }
{ Formulas from BP}
{ Inputs: x,y,z,density,temperature }
{ Output: pressure, gaspressure, radpresure (dynes/cm^2): global }
{ Output: dLogPdLogT, dLogPdLogD, dLnTdLnPad : global }
{ Full ionization is assumed, but electrons may be partly degenerate,}
{ partly relativistic}

CONST
 radconst = 2.523e-15; {rad const a/3 (erg/cc/K^4)}
 ionconst = 0.8263e8;  {Bolzman const/proton mass (erg/g/K)}
 const1   = 1.0036e13; {KW -for non-relativistic deg e gas}
 const2   = 1.2435e15; {KW -for relastivistic deg e gas}

VAR
 MeanIonWeight  :Real; {Mean molecular weight of ions; take z oxygen}
 MeanElWeight   :Real; {Mean mol wght per free electron, full ionization}
 relativistic   :Real; {how relativistic the electrons are}
 dLogPEdLogT    :Real; {d log(elect P)/d log(T) at constant density}
 dLogPEdLogD    :Real; {d log(elect P)/d log(density) at constant T}
 qt             :Real; {T*Cv, where Cv is specific heat const V per gram}
 qr             :Real; {qt*(d log(T)/d log(density)) at constant entropy}

BEGIN
 MeanIonWeight := 1.0/(xx+yy/4.0+zz/16.0); {mean moleclr weight of ions}
 MeanElWeight  := 2.0/(1.0+xx);          {mean mol weight per free elect}

 radpressure   := radconst*t*t*t*t; {radiation pressure}
 ionpressure   := d*t*ionconst/MeanIonWeight; {ion prss}
 DegRpressure  := const2*Pwr((d/MeanElWeight),(4.0/3.0));
 DegNRpressure := const1*Pwr((d/MeanElWeight),(5.0/3.0));
 Degpressure   := DegNRpressure/SQRT(1.0+SQR(DegNRpressure/DegRpressure));
 Nonpressure   := d*t*ionconst/MeanElWeight;
 electpressure := Nonpressure*SQRT(1.0+SQR(Degpressure/Nonpressure));

 gaspressure   := ionpressure+electpressure; {gas pressure}
 pressure      := gaspressure+radpressure;   {total pressure}

 relativistic  := (5.0/3.0)*SQR(Degpressure/DegNRpressure)
                  + (4.0/3.0)*SQR(Degpressure/DegRpressure);
 dLogPEdLogT   := SQR(Nonpressure/electpressure);
 dLogPEdLogD   := dLogPEdLogT+(1.0-dLogPEdLogT)*relativistic;

 dLogPdLogT    := (4.0*radpressure+ionpressure
                  +dLogPEdLogT*electpressure)/pressure;
 dLogPdLogD    := (ionpressure+dLogPEdLogD*electpressure)/pressure;
 qt            := (12.0*radpressure+1.5*ionpressure
                  +electpressure*dLogPEdLogT/(relativistic-1.0))/d;
 qr            := pressure*dLogPdLogT/d;
 dLnTdLnPad    := 1.0/(dLogPdLogD*qt/qr+dLogPdLogT); {DEL ADIABATIC}
 dLnTdLnPrad   := Constgrrad*kopacity*pressure/radpressure; {DEL RAD*M/L}

END;

{---------------------------------------------------------------------------}

PROCEDURE EvaluateEquations(VAR Params, Deriv :Vector5); {Eval Diff Eqs}

{ Evaluate differential equations of stellar structure}
{ Inputs: Array of initial parameters}
{ Output: Array of derivitives}
{ Params are: temperature, density, radius, Luminosity, mass}
{ Deriv: d Params[i]/d Params[1]}

VAR
 t           :Real; {temp}
 d           :Real; {density}
 r           :Real; {radius}
 l           :Real; {Luminosity}
 m           :Real; {mass}
 dLnTdLnP    :Real; {temperature gradiant in star dLnt/dLnp}
 dLnDdLnP    :Real; {density gradiant dLnDensity/dLnp}
 dPdM        :Real; {pressure gradient dLnp/d(M/Msun)}

BEGIN
 t := Params[1]; {temp}
 d := Params[2]; {density}
 r := Params[3]; {radius}
 l := Params[4]; {Luminosity}
 m := Params[5]; {mass}

 Opacity(x,y,z,d,t); {Evaluate opacity first}
 EquationOfState(x,y,z,d,t); {Evaluate the thermodynamic eq of state}
 NuclearPower(x,y,z,znitrogen,d,t); {Evaluate energy generation}

 dLnTdLnPrad := dLnTdLnPrad*l/m; {DEL RAD}

 dLnTdLnP := dLnTdLnPad;  {adiabatic T gradient}
 IF( dLnTdLnP > dLnTdLnPrad)THEN
   BEGIN
     dLnTdLnP := dLnTdLnPrad; {use smaller of radiative/adiabatic T grad}
   END; {IF}

 dLnDdLnP := (1.0-dLnTdLnP*dLogPdLogT)/dLogPdLogD; {density gradient}
 dPdM     := -ConstdPdM*m/(r*r*r*r);               {pressure gradient}

 Deriv[1] := dLnTdLnP*dPdM*t/pressure; {dT/dM}
 Deriv[2] := dLnDdLnP*dPdM*d/pressure; {dD/dM}
 Deriv[3] := Sundensity/3.0/d/SQR(r);    {dr/dM}
 Deriv[4] := Sunmass/SunLum*power;       {dL/dM}
 Deriv[5] := 1.0;                        {dM/dM}

END;

{---------------------------------------------------------------------------}

PROCEDURE IntegralStep(VAR Parameters : Vector5); {Second order RK integration}

{ Integrates one step using Runge Kutta method and parameters passed}
{ Params are adjusted one step by value found at middle of}
{ integration interval.}
{ Inputs: Array of initial parameters }
{ Output: Array of final parameter values at end of integration step}

VAR
 Derivs         :Vector5; {derivatives of parameters computed}
 halfparameters :Vector5; {parameters at mid point}
 stepsize       :Real;  {integration step size}
 halfstepsize   :Real;  {integration half step size}
 temporary      :Real;
 i              :Integer; {do loop index}

BEGIN
 EvaluateEquations(Parameters, Derivs); {Evaluate diff eqs with params}

 stepsize := abs(Derivs[5]/maxparamstep[5]/Parameters[5]); {integ step}

 FOR i := 1 TO 4 DO       {loop on each param to get integ step size}
    BEGIN
      temporary := abs(Derivs[i]/maxparamstep[i]/Parameters[i]);
      IF( stepsize < temporary) THEN
        BEGIN
          stepsize := temporary;
        END; {IF}
 END; {DO}
 stepsize := 1.0/stepsize;

 IF (Parameters[5] > FitMass) THEN
   BEGIN
     stepsize := -stepsize;  {negative step if mass > fitting mass}
   END; {IF}

 IF ((Parameters[5]-FitMass)*(Parameters[5]+stepsize-Fitmass) < 0.0) THEN
   BEGIN
     stepsize := Fitmass-Parameters[5];  {close to Fitmass}
   END; {IF}

 halfstepsize := stepsize/2.0;

 FOR i := 1 TO 5 DO          {make first half step of integration}
    BEGIN
      halfparameters[i] := Parameters[i]+halfstepsize*Derivs[i];
    END; {DO}

 EvaluateEquations(halfparameters, Derivs); {Evaluate diff eqs at midpnt}

 FOR i := 1 TO 5 DO {make full step of integration using midpoint derivs}
    BEGIN
      Parameters[i] := Parameters[i]+stepsize*Derivs[i];
    END; {DO}

END;

{-------------------------------------------------------------------------}

PROCEDURE Outward(Tcent, Dcent :Real;     {Integration from center outward}
                  VAR Core : Vector5;
                  VAR Error :Integer);

{ Integrates stellar structure equations from the center out to fit point}
{ Inputs: T core and core density }
{ Output: 5 values at fit point (T, density, radius, Luminosity, mass}

CONST
 maxiteration = 1000;    {maximum number of iterations}

VAR
 iteration       :Integer;{iteration counter}
 deviation       :Real; {deviation of mass at FitMass}
 coreradius      :Real; {core radius/sun radius}
 coremass        :Real; {core mass/sun mass}
 coreLuminosity  :Real; {core Luminosity/sun lum}
 xx              :Real;
 index           :Integer;

BEGIN
 NuclearPower(x,y,z,znitrogen,Dcent,Tcent); {compute power gener}
 coremass        := FitMass/10000.0;        {initial "mass" of core}
 coreLuminosity  := SunMass/SunLum*coremass*power;
 coreradius      := PWR((coremass/Dcent*Sundensity),(1.0/3.0));

 {initialize 5 params of core}
 Core[1] := Tcent;           {core temperature}
 Core[2] := Dcent;           {density}
 Core[3] := coreradius;      {radius/sun r}
 Core[4] := coreLuminosity; {Luminosity/sun lum}
 Core[5] := coremass;        {initila core mass/sun mass}

 iteration := 0;

 Message(concat('Out:',NumStr(ModIteration,2,0),'  Te:',NumStr(Teffective,4,0)
   ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
   ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2)
   ,'  <ESC> to abort'));

   IF(PlotControl = 1)THEN
    BEGIN
     SetColor(PlotColor);         {set the plot color}
     PlotColor := PlotColor+1;    {update the plot color}
     IF(PlotColor = 3)THEN PlotColor :=7;   {skip}
     IF(PlotColor = 8)THEN PlotColor :=10;   {skip}
     IF(PlotColor > 15)THEN PlotColor :=2;
     SetColor(PlotColor);         {set the plot color}
    END;

 REPEAT

   iteration := iteration +1; {loop counter}
   IntegralStep(Core);        {Integrate outward to FitMass}
   deviation := ABS(Core[5]/FitMass-1.0); {deviation from fit mass}
   IF(Core[1] < Teffective)THEN
              BEGIN CORE[1] := Teffective; END; {keep T>0}
   IF(Core[2] < 1.0e-12)THEN
              BEGIN CORE[2] := 1.0e-12; END; {keep rho>0}

   IF(PlotControl = 1)THEN
    BEGIN

    IF(PlotMassOrRadius)THEN BEGIN xx := Core[3]/radius; END
                             ELSE BEGIN xx := Core[5]/mass; END;  {x plot coordinate: mass fraction}
    IF((xx >0.01) AND (xx < 0.99))THEN
     BEGIN
       PlotDataModel(1,xx,Log(10.0,Core[1])); {plot T.vs.M}
       PlotDataModel(2,xx,Log(10.0,Core[2])); {plot density.vs.M}
       PlotDataModel(3,xx,Log(10.0,Core[3])); {plot radius.vs.M}
       PlotDataModel(4,xx,Log(10.0,Core[4])); {plot lum.vs.M}

       index := TRUNC(xx*Keepsize); {array index}
       T_array[index] := Core[1]; {save values}
       D_array[index] := Core[2];
       R_array[index] := Core[3];
       L_array[index] := Core[4];
       M_array[index] := Core[5];
       K_array[index] := kopacity;
       E_array[index] := power;
       P_array[index] := pressure;
     END;
    END;

 UNTIL ((deviation < precision) OR (iteration > maxiteration)); {Loop}

 IF( deviation > precision)THEN
  BEGIN
   Message(concat('OUTWARD:Integral deviation too large:',NumStr(deviation,9,4)));
  END;

END;

{---------------------------------------------------------------------------}

PROCEDURE Inward(Teff, Lum :Real;  {Integration from surface inward}
                 VAR Surf :Vector5;
                 VAR Error :Integer);

{ Integrates stellar structure equations from the surface in to fit point}
{ Inputs: Teffective and Luminosity }
{ Output: 5 values at fit point (T, density, radius, Luminosity, mass}

CONST
 maxiteration = 1000;    {maximum number of iterations}

VAR
 iteration     :Integer; {iteration counter}
 deviation     :Real;  {deviation of mass at FitMass}
 xx            :Real;
 index         :Integer;

BEGIN
 radius := SQRT(Lum)/SQR(Teff/SunTsurface); {photosphere radius/sun radius}

 {initialize 5 params of surface; the atmosphere is thin}
 Surf[1] := Teff/TwoTo14; {surface temp smaller than Teffect}
 Surf[2] := 1.0e-12; {density small}
 Surf[3] := radius;  {radius/sun r}
 Surf[4] := Lum;     {Luminosity/sun lum}
 Surf[5] := Mass;    {mass/sun mass}

 iteration := 0;

 Message(concat('In: ',NumStr(ModIteration,2,0),'  Te:',NumStr(Teffective,4,0)
    ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
    ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2)
    ,'  <ESC> to abort'));

   IF(PlotControl = 1)THEN
    BEGIN
     SetColor(PlotColor);   {set the plot color}
    END;
 REPEAT
   iteration := iteration +1;
   IntegralStep(Surf);   {Integrate inward to FitMass}
   deviation := ABS(Surf[5]/FitMass-1.0); {deviation from fit mass}
   IF(Surf[4] < 1.0e-10)THEN
              BEGIN Surf[4] := 1.0e-10; END; {keep L>0}

   IF(PlotControl = 1)THEN
    BEGIN
    IF(PlotMassOrRadius)THEN BEGIN xx := Surf[3]/radius; END
                             ELSE BEGIN xx := Surf[5]/mass; END;  {x plot coordinate: mass fraction}
    IF((xx >0.01) AND (xx < 0.99))THEN
     BEGIN
       PlotDataModel(1,xx,Log(10.0,Surf[1])); {plot T.vs.M}
       PlotDataModel(2,xx,Log(10.0,Surf[2])); {plot density.vs.M}
       PlotDataModel(3,xx,Log(10.0,Surf[3])); {plot radius.vs.M}
       PlotDataModel(4,xx,Log(10.0,Surf[4])); {plot lum.vs.M}

       index := TRUNC(xx*Keepsize); {array index}
       T_array[index] := Surf[1]; {save values}
       D_array[index] := Surf[2];
       R_array[index] := Surf[3];
       L_array[index] := Surf[4];
       M_array[index] := Surf[5];
       K_array[index] := kopacity;
       E_array[index] := power;
       P_array[index] := pressure;
     END;
    END;

 UNTIL ((deviation < precision) OR (iteration > maxiteration)); {Loop}

 IF( deviation > precision)THEN
  BEGIN
   Message(concat('INWARD:Integral deviation too large:',NumStr(deviation,9,4)));
  END;
END;

{---------------------------------------------------------------------------}

PROCEDURE CorrectBoundary(VAR deriv :Array5x5); {Correct boundary conds}

{Corrects the boundary conditions for the next iteration}
{Inputs: old boundary conds and array of derivitives:Del[x(i)] & C(i,j) }
{Output: corrected values of Teff, core density, T core, Luminosity}
{This routine uses the technique of BP to find corrections to the 4}
{boundary parameters based on the derivitives found}

CONST
 NumEqs = 4; {number of equations}

VAR
 i       :Integer; {Do index}
 j       :Integer; {Do index}
 k       :Integer; {Do index}
 i1      :Integer; {temporary}
 i2      :Integer; {temporary}
 d1      :Real;  {temporary}
 d2      :Real;  {temporary}
 f1      :Real;  {temporary}
 f2      :Real;  {temporary}
 factor  :Real;  {reduction factor for corrects,or max correct}

BEGIN
 {Solve 4 simultaneous equations: satisfy 4 eqns for T,rho,r,L: sumover j:}
 {       deriv[i,j]*corrections[j]+deriv[i,5]=0}

 FOR k := 1 TO NumEqs-1 DO
   BEGIN
     f1 := deriv[k,k];
     FOR i := k+1 TO NumEqs DO
       BEGIN
         f2 := deriv[i,k];
         FOR j := k+1 TO NumEqs+1 DO
           BEGIN
             deriv[i,j] :=deriv[i,j]*f1-deriv[k,j]*f2;
           END; {DO j}
       END; {DO i}
   END; {DO k}

 {deriv matrix is now triangular}
 corrections[NumEqs] := -deriv[NumEqs,NumEqs+1]/deriv[NumEqs,NumEqs];

 FOR i := 2 TO NumEqs DO
   BEGIN
     i1 := NumEqs-i+1;
     i2 := i1+1;
     corrections[i1] := -deriv[i1,NumEqs+1];
     FOR j := i2 TO NumEqs DO
       BEGIN
         corrections[i1] :=corrections[i1]-deriv[i1,j]*corrections[j];
       END; {DO j}
     corrections[i1] := corrections[i1]/deriv[i1,i1];
   END; {DO i}
 {The unknown corrections del[z(i)] have now been found}

 {Check the size of corrections}
 d1 := 0.0;
 FOR i := 1 TO NumEqs DO
   BEGIN
     d2 := abs(corrections[i]/maxcorrect[i]);
     IF( d1 < d2) THEN BEGIN d1 := d2; END; {IF}
   END; {DO i}

 factor := 1.0;
 IF( d1 > factor)THEN BEGIN factor := d1; END; {IF}

 FOR i := 1 TO NumEqs DO
   BEGIN
     corrections[i] := corrections[i]/factor;
   END; {DO i}
 factor := d1;


 {Now, use result del[z(i)] to correct the boundary parameters}
 Teffective  := Teffective*(1+corrections[1]);
 Luminosity  := Luminosity*(1+corrections[2]);
 Tcentral    := Tcentral*(1+corrections[3]);
 Dcentral    := Dcentral*(1+corrections[4]);

END;

{---------------------------------------------------------------------------}

PROCEDURE StellarModel(VAR Error :Integer; Learning :Boolean); {Compute a Stellar Model}

{Computes a chemically homogeneous stellar model in thermal equilibrium }
{That is, a zero age main sequence star. Integrates inward and outward}
{and fits together at fitting mass FitMass. Results from each direction}
{are compared, and boundary conditions are varied to improve fit on next}
{iteration. Routine quits when fit comparison is good, or on maximum}
{iterations. Boundary conditions are core density, core temperature,}
{effective surface temperature, and surface Luminosity.}
{Inputs: x,y,z,mass,effective T, core T, core density, Luminosity }
{Output: radius }

{This routine calls Inward and Outward to integrate in and out and}
{finds the difference; CorrectBoundary then adjusts the 4 boundary}
{conditions, and the iteration continues until a solution results.}
{Outward uses NuclearPower to compute E generation at center. Both}
{Inward and Outward use IntegralStep to make a step in the integration.}
{IntegralStep uses EvaluateEquations to get the physical conditions.}
{EvaluateEquations uses EqautionOfState, Opacity and NuclearPower to}
{determine the physical conditions and find the derivitives of the}
{parameters. }

{The Learning flag turns off the automatic boundary correction so the}
{student can learn the effect of each parameter}

CONST
 maxiteration = 1000; {maximum number of iterations}
 Tcdel = 0.0011;    {increment in central T for derivative}
 Tsdel = 0.0011;    {increment in surface T for derivative}
 Dcdel = 0.0011;    {increment in central density for derivative}
 Lsdel = 0.0011;    {increment in surface Luminosity for derivative}

VAR
 maxdelta :Real;  {Largest delta in present iteration}
 i        :Integer; {loop counter}
 delta    :Vector5; {diffs of core/surface vals}
 key      :Char;
 keylast  :Char;
 percentchange :Double; {percent change hot keys make}

BEGIN
 znitrogen := z*nitrogenfraction; {amount of nitrogen assumed}
 ModIteration := 0; {clear counter}
 percentchange := 0.1;

 comment (0.05,0.945,
  concat('Hot Keys Change One Parameter by',
          NumStr(percentchange*100,5,1),'% After Each Iteration; +/- Change %'));
 comment (0,0.05,
  'HotKeys: cC=Tcentral dD=Density eE=Teffective lL=Luminosity pP=Pause rR=Replot');

 IF(Learning)THEN
    comment(0.16,0.1,'Learning Mode: Automatic Boundary Adjustment Disabled');

 REPEAT  {Repeat until converged or out of iters}
   ModIteration := ModIteration+1;      {Increment iteration value}
   key :=' ';

   IF(PlotControl = 2)THEN BEGIN PlotControl := 1; END; {Turn Plotting back on}

   FOR i:=0 to keepsize DO    {Clear arrays - must do this to avoid variation}
    BEGIN
     M_array[i] :=0;
     T_array[i] :=0;
     D_array[i] :=0;
     R_array[i] :=0;
     L_array[i] :=0;
     K_array[i] :=0;
     E_array[i] :=0;
     P_array[i] :=0;
    END; {FOR}


   Outward(Tcentral,Dcentral,corevals,error); {integrate center to fit pnt}
   IF(Keypressed)THEN key := Readkey; {look for ESC}
   IF(key <> Chr(27))THEN Inward(Teffective,Luminosity,surfacevals,error); {integ from surfce in}
   IF(Keypressed)THEN key := Readkey; {look for ESC}

   IF(PlotControl = 1)THEN BEGIN PlotControl := 2 END; {Plotting off}

   maxdelta  := 0.0; {Max difference in core/surface values}
   FOR i:=1 TO 4 DO  {Find largest fractional diff in params}
     BEGIN
       delta[i] := surfacevals[i]/corevals[i]-1.0; {fractional diff}
       IF(ABS(delta[i]) > maxdelta)THEN
         BEGIN
             maxdelta := ABS(delta[i]);   {max fractional diff}
         END; {IF}
     END; {FOR}

 {Now compute derivitives for next iter step; vary each of 4 boundary conds}
   IF(key <> Chr(27))THEN Outward(Tcentral*(1+Tcdel),Dcentral,corevalsT,error); {dTc:integ out}
   IF(Keypressed)THEN key := Readkey; {look for ESC}
   IF(key <> Chr(27))THEN Inward(Teffective*(1+Tsdel),Luminosity,surfacevalsT,error); {dTs:surface}
   IF(Keypressed)THEN key := Readkey; {look for ESC}
   IF(key <> Chr(27))THEN Outward(Tcentral,Dcentral*(1+Dcdel),corevalsD,error); {dDc:integ out}
   IF(Keypressed)THEN key := Readkey; {look for ESC}
   IF(key <> Chr(27))THEN Inward(Teffective,Luminosity*(1+Lsdel),surfacevalsL,error); {dLs:surface}

   FOR i:=1 TO 4 DO  {Compute derivatives}
    BEGIN
     derivitive[i,1] := ((surfacevalsT[i]/corevals[i]-1.0)
                        -delta[i])/(Tsdel);
     derivitive[i,2] := ((surfacevalsL[i]/corevals[i]-1.0)
                        -delta[i])/(Lsdel);
     derivitive[i,3] := ((surfacevals[i]/corevalsT[i]-1.0)
                        -delta[i])/(Tcdel);
     derivitive[i,4] := ((surfacevals[i]/corevalsD[i]-1.0)
                        -delta[i])/(Dcdel);
     derivitive[i,5] := delta[i];
    END;

    comment (0.2,0.51,concat('Delta: T:',NumStrExp(delta[1],7)
          ,'  D:',NumStrExp(delta[2],7),'  R:',NumStrExp(delta[3],7)
          ,'  L:',NumStrExp(delta[4],7)));


   IF(SoundOnOff)THEN Beep;

   IF(NOT Learning)THEN CorrectBoundary(derivitive); {Now correct boundary conds using derivs}

   IF(maxdelta > ModelPrecision)THEN
         IF((key = 'p') OR (key = 'P'))THEN BEGIN Beep; StaticPause; END;

         IF(key = '+')THEN  {change percentchange by factor of 10}
           BEGIN
             Beep;
             percentchange := percentchange*2;
             key := 'r'; {force replot}
           END;

         IF(key = '-')THEN  {change percentchange by factor of 10}
           BEGIN
             Beep;
             percentchange := percentchange/2;
             key := 'r'; {force replot}
           END;

         IF((key = 'r') OR (key = 'R'))THEN   {replot}
           BEGIN
             ClearMUPPETPort;
             InitializeModelPlot(5);
             comment (0.05,0.945,
              concat('Hot Keys Change One Parameter by',
                NumStr(percentchange*100,5,1),'% After Each Iteration; +/- Change %'));
             comment (0,0.05,
              'HotKeys: cC=Tcentral dD=Density eE=Teffective lL=Luminosity pP=Pause rR=Replot');
             IF(Learning)THEN
               comment(0.16,0.1,'Learning Mode: Automatic Boundary Adjustment Disabled');
           END;

         IF(key = 'l')THEN BEGIN Beep;Luminosity := Luminosity/(1+percentchange);END;
         IF(key = 'L')THEN BEGIN Beep;Luminosity := Luminosity*(1+percentchange);END;
         IF(key = 'd')THEN BEGIN Beep;Dcentral := Dcentral/(1+percentchange);END;
         IF(key = 'D')THEN BEGIN Beep;Dcentral := Dcentral*(1+percentchange);END;
         IF(key = 'c')THEN BEGIN Beep;Tcentral := Tcentral/(1+percentchange);END;
         IF(key = 'C')THEN BEGIN Beep;Tcentral := Tcentral*(1+percentchange);END;
         IF(key = 'e')THEN BEGIN Beep;Teffective := Teffective/(1+percentchange);END;
         IF(key = 'E')THEN BEGIN Beep;Teffective := Teffective*(1+percentchange);END;


 UNTIL ((maxdelta < ModelPrecision) OR (ModIteration > maxiteration)
         OR (key = Chr(27))); {Loop}

 IF(maxdelta < ModelPrecision)THEN
    BEGIN
      Error     :=0; {returned error value if no conversion}
      Beep;
      Message(concat('Converged: Te:',NumStr(Teffective,4,0)
          ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
          ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2)
          ,'  <Enter> main menu '));
      comment (0.05,0.945,
          concat('Converged: Te:',NumStr(Teffective,4,0)
          ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
          ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2),'                '));
      ComputedModel :=1; {success flag}
      Beep;
      staticPause;
    END
 ELSE
    BEGIN
      Beep;
      Error     :=ModIteration; {returned error value if no conversion}
      Message(concat('Didnt Converge:Te:',NumStr(Teffective,4,0)
          ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
          ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2)
          ,'  <Enter> main menu '));
      comment (0.05,0.945,
          concat('Didnt Converge: Te:',NumStr(Teffective,4,0)
          ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
          ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2),'           '));
      Beep;
      StaticPause;
    END;

    comment (0,0.05,
    '                                                                              ');
END; {StellarModel}

{---------------------------------------------------------------------------}
(*-----             Input Screen Menu Procedures                      -----*)
{---------------------------------------------------------------------------}

PROCEDURE SetUpHotKeys;
 {main hot key menu}
BEGIN
   WITH HotKeys DO BEGIN
      init(3);
      key[1] := 'F1-Help';
      key[2] := 'F10-Menu';
      key[3] := 'ESC-Main Menu';
   END;
   HotKeys.Display;
END;

{---------------------------------------------------------------------------}

PROCEDURE SetUpMyMenu;
 {main menu}

BEGIN
   MyMenu.Init;
   WITH MyMenu DO
   BEGIN
      column(1, 'File');
       row(1,1, 'About STELLAR');
       row(1,2, 'About CUPS');
       row(1,3, 'Getting Started');
       row(1,4, 'Save File of Results');
       row(1,5, 'Configuration');
       row(1,6, 'Exit');
      column(2, 'Boundary');
       row(2,1, 'Modify Initial Parameters');
       row(2,2, 'Change Compositions');
       row(2,3, 'Explain Stellar Conditions');
       row(2,4, 'Explain Stellar Equations');
       row(2,5, 'Explain Convection and Degeneracy');
       row(2,6, 'Neutrino Production');
       row(2,7, 'Reinitialize Model');
      column(3, 'Compute');
       row(3,1, 'Run Model');
       row(3,2, 'Learn About Boundary Effects');
       row(3,3, 'Show Numerical Results');
       row(3,4, 'Plot Results');
       row(3,5, 'Plot More Results');
       row(3,6, 'Modify Compute Parameters');
      column(4, 'Power');
       row(4,1, 'Calculate Power');
       row(4,2, 'Plot Power vs Temperature');
      column(5, 'Eq. State');
       row(5,1, 'Calculate Equation of State');
       row(5,2, 'Plot Equation of State');
       row(5,3, 'Plot More Equation of State');
      column(6, 'Opacity');
       row(6,1, 'Calculate Opacity');
       row(6,2, 'Plot Opacity');
       row(6,3, 'Plot More Opacity');
   END;
   MyMenu.Display;
END;

{---------------------------------------------------------------------------}

PROCEDURE AboutStellar;
 {first display screen}
var
   MainScreen : HelpScrType;
BEGIN
   MainScreen[01] := ' ';
   MainScreen[02] := '           Stellar Structure Model';
   MainScreen[03] := ' ';
   MainScreen[04] := '             by Richard T. Kouzes';
   MainScreen[05] := '         Pacific Northwest Laboratory';
   MainScreen[06] := '        (c) 1995 John Wiley & Sons, Inc';
   MainScreen[07] := '             STELLAR Revision 2.0';
   MainScreen[08] := ' ';
   MainScreen[09] := ' This program models the interior of a star by';
   MainScreen[10] := ' assuming that it is in hydrostatic equilibrium.';
   MainScreen[11] := ' The differential equations governing the star are';
   MainScreen[12] := ' integrated from the center toward the surface and';
   MainScreen[13] := ' from the surface toward the center to a matching';
   MainScreen[14] := ' point. The menu choices are:';
   MainScreen[15] := '  File       - Exit and file handling';
   MainScreen[16] := '  Boundary   - the boundary conditions on the star';
   MainScreen[17] := '  Compute    - run a model calculation';
   MainScreen[18] := '  Power      - power generation in the star';
   MainScreen[19] := '  Eq. State  - equation of state in the star';
   MainScreen[20] := '  Opacity    - opacity calculations';
   MainScreen[21] := ' To perform a simulation, set the parameters in';
   MainScreen[22] := ' the Boundary menu and then Run under Compute.';
   MainScreen[23] := ' ';
   MainScreen[24] := ' See the Getting Started selection under File.';
   MainScreen[25] := ' ';
   help(mainScreen);
END;

{---------------------------------------------------------------------------}

PROCEDURE GettingStarted;
var
   MainScreen : HelpScrType;
BEGIN
   MainScreen[01] := ' ';
   MainScreen[02] := '           GETTING STARTED WITH STELLAR';
   MainScreen[03] := ' ';
   MainScreen[04] := ' * Start by selecting Run under the Compute menu.';
   MainScreen[05] := '   This will run a model of the default mass=1';
   MainScreen[06] := '   star.';
   MainScreen[07] := ' ';
   MainScreen[08] := ' * Next, use the Modify Initial Parameters item';
   MainScreen[09] := '   under the Boundary menu to change the mass of';
   MainScreen[10] := '   the star. Try mass=1.5. This time, when you Run';
   MainScreen[11] := '   the model, you will see the code slowly bring';
   MainScreen[12] := '   the model to convergence.';
   MainScreen[13] := ' ';
   MainScreen[14] := ' * Next, use the Modify Initial Parameters item';
   MainScreen[15] := '   under the Boundary menu to change the mass of';
   MainScreen[16] := '   the star to mass=2. This time, select the';
   MainScreen[17] := '   Learn About Boundary Effects option under the';
   MainScreen[18] := '   Compute menu. This runs the model, but requires';
   MainScreen[19] := '   the user to guide the model toward convergence';
   MainScreen[20] := '   using the hotkeys shown at the bottom of the';
   MainScreen[21] := '   screen. If you can not reach convergence, <ESC>';
   MainScreen[22] := '   out of the Learn mode and select Run - the';
   MainScreen[23] := '   program will then finish the convergence.';
   MainScreen[24] := ' ';
   MainScreen[25] := ' * Try different masses until you understand.';
   help(mainScreen);
END;

{---------------------------------------------------------------------------}

PROCEDURE StellarConditions;
var
   StellarScreen : HelpScrType;
   i : integer;
BEGIN
   StellarScreen[01] := ' ';
   StellarScreen[02] := '           STELLAR CONDITIONS ASSUMED';
   StellarScreen[03] := ' ';
   StellarScreen[04] := 'This stellar model makes several assumptions about';
   StellarScreen[05] := 'the star being modeled. These include:';
   StellarScreen[06] := ' ';
   StellarScreen[07] := '1) Hydrostatic equilibrium - the gravitational';
   StellarScreen[08] := '   attraction is balanced by the the thermodynamic';
   StellarScreen[09] := '   pressure.';
   StellarScreen[10] := '2) Static - no time dependence or rotation is';
   StellarScreen[11] := '   included.';
   StellarScreen[12] := '3) Spherically symetric - the star is spherical.';
   StellarScreen[13] := '4) Thermal equilibrium - the loss of energy by the';
   StellarScreen[14] := '   star is balanced by fusion energy generation.';
   StellarScreen[15] := '5) Chemically homogeneous - the mass fraction of';
   StellarScreen[16] := '   hydrogen (X), helium (Y) and heavy elements (Z)';
   StellarScreen[17] := '   is assumed to be uniform throughout the star.';
   for i := 18 to 25 do StellarScreen[i] := '';
   help(StellarScreen);
END;

{---------------------------------------------------------------------------}

PROCEDURE Equations;
var
   EquationScreen : HelpScrType;
   i : integer;
BEGIN
   EquationScreen[01] := ' ';
   EquationScreen[02] := '             STELLAR EQUATIONS';
   EquationScreen[03] := ' ';
   EquationScreen[04] := 'The stellar model is governed by four differential';
   EquationScreen[05] := 'equations which must be satisfied everywhere in';
   EquationScreen[06] := 'stellar interior. Since only hydrostatic';
   EquationScreen[07] := 'equilibrium is treated, there is no time';
   EquationScreen[08] := 'dependence in the equations. The four equations';
   EquationScreen[09] := 'are:';
   EquationScreen[10] := ' ';
   EquationScreen[11] := '1) mass - radius relation';
   EquationScreen[12] := '2) gravitation - thermal pressure relation';
   EquationScreen[13] := '3) heat transport - temperature relation';
   EquationScreen[14] := '4) energy generation - luminosity relation';
   for i := 15 to 25 do EquationScreen[i] := ' ';
   help(EquationScreen);
END;

{---------------------------------------------------------------------------}

PROCEDURE SetBoundaryConditions;
var
   BoundaryInput : TInputScreen;
BEGIN
   {New(BoundaryInput);}{needed on MAC}
   Tsurface := Teffective/TwoTo14;
   with BoundaryInput do
   begin
      init;
      DefineInputPort(0.15,0.85,0.10,0.90);
      LoadLine('                  BOUNDARY CONDITIONS');
      LoadLine('    ');
      LoadLine('   The stellar model is governed by four differential');
      LoadLine('   equations which require four boundary conditions');
      LoadLine('   be specified. Two of these conditions are at the');
      LoadLine('   center of the star (r=0) where the integral mass');
      LoadLine('   and the Luminosity are both zero. Two are at the');
      LoadLine('   surface of the star where the integral mass of the');
      LoadLine('   star equals the specified mass (where the density');
      LoadLine('   has dropped to a low value). You must specify the');
      LoadLine('   mass and make a guess at four initial conditions:');
      LoadLine('    ');
      LoadLine('    Mass (units of solar mass)      : {           }');
      LoadLine('    Central temperature (K)         : {           }');
      LoadLine('    Central density (g/cc)          : {           }');
      LoadLine('    Luminosity (units of solar Lum) : {           }');
      LoadLine('    Surface temperature (K)         : {           }');
      LoadLine('    ');
      LoadLine('                 [  Ok  ]     [Cancel]');

      SetNumber(1,Mass);
      SetNumber(2,TCentral);
      SetNumber(3,DCentral);
      SetNumber(4,Luminosity);
      SetNumber(5,TSurface);
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
   end;
   ClearMuppetPort;
   {Dispose(BoundaryInput);}{needed on MAC}
END;

{---------------------------------------------------------------------------}

{on Mac need $S stellar2}

PROCEDURE ComputeInputScreen;
var
   ComputeInput : TInputScreen;

BEGIN
   {New(ComputeInput);}{needed on MAC}
   with ComputeInput do
   begin
      init;
      DefineInputPort(0.15,0.85,0.25,0.72);
      LoadLine('          MODIFY COMPUTE PARAMETERS');
      LoadLine('    ');
      LoadLine(' The precision of the fit at the fitting point ');
      LoadLine(' can be varied as can the fitting point mass.  ');
      LoadLine('  ');
      LoadLine('   Precision                           : {         }');
      LoadLine('   Fit Mass Fraction                   : {         }');
      LoadLine('   Sound on                            :  #T');
      LoadLine('   Plot versus Radius Instead of Mass  :  #T');
      LoadLine('  ');
      LoadLine('            [  Ok  ]     [Cancel] ');
      SetNumber(1,precision);
      SetNumber(2,FitMassFraction);
      SetBoolean(3,SoundOnOff);
      SetBoolean(4,PlotMassOrRadius);
      SetNumberLimits(1,0.000001,1.0);
      SetNumberLimits(2,0.01,0.99);
      Accept;
      precision       := GetNumber(1);
      FitMassFraction := GetNumber(2);
      SoundOnOff      := GetBoolean(3);
      PlotMassOrRadius:= GetBoolean(4);
      ModelPrecision  := 10*precision;   {desired accuracy in StellarModel}
      done;
   end;
   ClearMuppetPort;
   {Dispose(ComputeInput);}{needed on MAC}
END;

{---------------------------------------------------------------------------}

PROCEDURE ShowResults;

VAR
 i  :Integer;
 j  :Integer;
BEGIN
   DefineViewPort(6, 0.15, 0.85, 0.10, 0.90);
   GraphBackColor := blue;
   OpenViewPort(6);
   GraphBackColor := DarkGray;
   SetColor(Whitte);
   Print(1,1,'   Mass  Temperature  Density      Radius  Luminosity');
   j := 1;
   FOR i := 3 TO 17 DO
   BEGIN {Print results of model values, only nonzero ones}
      print(1,i, concat('  ',NumStr(M_array[j],5,3),'  '
                       ,NumStrExp(T_array[j],10),' '
                       ,NumStrExp(D_array[j],10),' '
                       ,NumStr(R_array[j],10,3),' '
                       ,NumStrExp(L_array[j],10)));
      j := j+TRUNC(keepsize/15);
      IF (T_array[j] <= 0) THEN j :=j+1; {dont print zeros}
      IF (T_array[j] <= 0) THEN j :=j+1;
      IF (T_array[j] <= 0) THEN j :=j+1;
      IF (T_array[j] <= 0) THEN j :=j+1;
      IF (T_array[j] <= 0) THEN j :=j+1;
      IF (j >= keepsize) THEN j :=keepsize;
   END; {FOR}
END;

{---------------------------------------------------------------------------}

function ParametersChanged : boolean;
CONST
  complimit = 1e-10; {minimum value of X,Y,Z}
var
   ParametersInput : TInputScreen;
   InputOK : boolean;
   XVal,YVal : real;

BEGIN
  {New(ParametersInput);}{needed on MAC}
   with ParametersInput do
   begin
      init;
      DefineInputPort(0.0,0.39,0.5,0.9);
      LoadLine('      Modify Parameters');
      LoadLine('x (Hydrogen frac): {         }');
      LoadLine('y (Helium frac)  : {         }');
      LoadLine('density (g/cc)   : {         }');
      LoadLine('temperature (K)  : {         }');
      LoadLine('');
      LoadLine('     [  Ok  ]     [Cancel] ');
      SetNumber(1,x);
      SetNumber(2,y);
      SetNumber(3,density);
      SetNumber(4,temperature);
      SetNumberLimits(1, complimit, 1-2*complimit);
      SetNumberLimits(2, complimit, 1-2*complimit);
      SetNumberLimits(3, 1e-12, 1E12);
      SetNumberLimits(4,0,1E30);
      InputOK := false;
      REPEAT
         Accept;
         xVal := GetNumber(1);
         yVal := GetNumber(2);
        IF not canceled THEN  {unless ESC, compute}
         BEGIN
          IF (1-xVal-yVal < complimit) THEN
            announce('Error: x+y must be less than or equal to 1.0 <Enter>')
          ELSE
           BEGIN
              x := xVal;    {get values from screen}
              y := yVal;
              z := 1-x-y;
              density     := GetNumber(3);
              temperature := GetNumber(4);
              InputOK := true;
           END; {IF}
         END;
      UNTIL InputOK or Canceled;
      ParametersChanged := not Canceled;
 {     ClearInputPort;}
      done;
   end;
   {Dispose(ParametersInput);}{needed on MAC}
end;

{---------------------------------------------------------------------------}

procedure ShowOpacity;
begin
   DefineViewPort(6, 0.4, 1.0, 0.10, 0.90);
   GraphBackColor := blue;
   repeat
    SetColor(Whitte);
    OpenViewPort(6);
    Opacity(x,y,z,density,temperature); {compute opacity}
    Print(2,01,'               COMPUTE OPACITY');
    Print(2,02,'Opacity k is a measure of the hindrance of    ');
    Print(2,03,'energy transport in the star, consisting of   ');
    Print(2,04,'radiation transport and electron conductivity,');
    Print(2,05,'and has units of cross-section per unit mass. ');
    Print(4,07, concat('x (Hydrogen fraction): ',NumStr(x,12,3)));
    Print(4,08, concat('y (Helium fraction)  : ',NumStr(y,12,3)));
    Print(4,09, concat('density (g/cc)       : ',NumStr(density,12,4)));
    Print(4,10, concat('temperature (K)      : ',NumStr(temperature,12,0)));
    SetColor(Yellow);
    Print(2,11,'COMPUTED RESULTS FOR PARAMETERS ABOVE: ');
    Print(4,12, concat('k(electron)   = ',NumStrExp(kelectron,10),'   cm^2/g'));
    Print(4,13, concat('k(Kramers)    = ',NumStrExp(kKramer,10)));
    Print(4,14, concat('k(H minus)    = ',NumStrExp(kHminus,10)));
    Print(4,15, concat('k(molecular)  = ',NumStrExp(kmolecular,10)));
    Print(4,16, concat('k(conductive) = ',NumStrExp(kconductive,10)));
    Print(4,17, concat('k(radiative)  = ',NumStrExp(kradiative,10)));
    Print(4,18, concat('k(total)      = ',NumStrExp(kopacity,10)));
    Print(4,19, concat('Log[k(total)] = ',NumStr(Log(10.0,kopacity),10,4)));
   until not ParametersChanged;
   SetColor(Whitte);
   GraphBackColor := DarkGray;
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
   {New(ChemCompScreen);}{needed on MAC}
   WITH ChemCompScreen DO
   BEGIN
      init;
      DefineInputPort(0.15,0.85,0.10,0.90);
      LoadLine('            CHEMICAL COMPOSITION');
      LoadLine(' ');
      LoadLine(' The chemical composition of a star effects the    ');
      LoadLine(' opacity, the nuclear energy generation rate and   ');
      LoadLine(' the equation of state of a star. The mass fraction');
      LoadLine(' of hydrogen, helium and heavy elements are        ');
      LoadLine(' represented by the symbols x, y and z respectively.');
      LoadLine('  ');
      LoadLine('  x (Hydrogen fraction): {         }');
      LoadLine('  y (Helium fraction)  : {         }');
      LoadLine('  z (Heavy fraction)   :  1-x-y     ');
      LoadLine('  ');
      LoadLine('              [  Ok  ]      [Cancel]');
      SetNumber(1,x);
      SetNumber(2,y);
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
             announce('Error: x+y+z must be less than or equal to 1.0 <Enter>');
             GoodValues := false;
            END
           ELSE
            BEGIN
             x := xVal; {get values from screen}
             y := yVal;
             z := 1.0-x-y;
            END; {IF}
          END;
      UNTIL GoodValues or Canceled;
      done;
   END;
   DefineViewPort(7,0.15,0.85,0.10,0.90);
   SelectViewPort(7);
   SetFillStyle(SolidFill,blue);
   HideCursor;
   bar( round(0.01*GetMaxX), round(0.30*GetMaxY),
        round(0.60*GetMaxX), round(0.78*GetMaxY) );
   ShowCursor;
   Print(2,09,concat('  x (Hydrogen fraction): ', Num2Str(x,9) ));
   Print(2,10,concat('  y (Helium fraction)  : ', Num2Str(y,9) ));
   Print(2,11,concat('  z (Heavy fraction)   : ', Num2Str(z,9) ));
   {Dispose(ChemCompScreen);}{needed on MAC}
end;

{---------------------------------------------------------------------------}

PROCEDURE ShowEnergy;

BEGIN
   DefineViewPort(6, 0.4, 1.0, 0.10, 0.90);
   GraphBackColor := blue;
   repeat
    OpenViewPort(6);
    SetColor(Whitte);
    znitrogen := z*nitrogenfraction;
    NuclearPower(x,y,z,znitrogen,density,temperature);
    NuclearPowerAlpha(x,y,z,znitrogen,density,temperature);
    Power := Power + Power3alpha;
    Print(1,01,'            ENERGY GENERATION');
    Print(1,02,'Energy is generated in a star through nuclear   ');
    Print(1,03,'fusion in the pp and CNO chains. The energy     ');
    Print(1,04,'generation is a function of x,y,z, temperature  ');
    Print(1,05,'and density. The nitrogen content is assumed to ');
    Print(1,06,'be z/3. The computed energy production rate is: ');
    Print(4,08,concat('x:               ',NumStr(x,9,3)));
    Print(4,09,concat('y:               ',NumStr(y,9,3)));
    Print(4,10,concat('density (g/cc):  ',NumStr(density,9,4)));
    Print(4,11,concat('temperature (K): ',NumStr(temperature,9,0)));
    SetColor(Yellow);
    Print(1,13,'COMPUTED RESULTS:');
    Print(4,14,concat('pp     power  = ',NumStrExp(powerpp,10) ,' erg/g/sec'));
    Print(4,15,concat('CNO    power  = ',NumStrExp(powerCNO,10),' erg/g/sec'));
    Print(4,16,concat('3alpha power  = ',NumStrExp(power3alpha,10),' erg/g/sec'));
    Print(4,17,concat('Total  power  = ',NumStrExp(power,10)   ,' erg/g/sec'));
    Print(4,18,concat('dx/dt         = ',NumStrExp(dxdt,10)   ,' g/g/sec'));
   until not ParametersChanged;
   SetColor(Whitte);
   GraphBackColor := DarkGray;
END;

{---------------------------------------------------------------------------}

PROCEDURE ConvectionDegeneracyScreen;
var
   CDHelp : HelpScrType;
   i : integer;
BEGIN
   CDHelp[01] := '  ';
   CDHelp[02] := '          CONVECTION AND DEGENERACY';
   CDHelp[03] := '  ';
   CDHelp[04] := ' Convection is only treated in this model by using';
   CDHelp[05] := ' the smaller of the radiative or adiabatic';
   CDHelp[06] := ' temperature gradiants; no mixing-length theory is';
   CDHelp[07] := ' included.';
   CDHelp[08] := '  ';
   CDHelp[09] := ' When the density is high enough and the';
   CDHelp[10] := ' temperature low enough, the electrons exert a';
   CDHelp[11] := ' pressure due to the Pauli exclusion principle';
   CDHelp[12] := ' preventing them from occupying the same state.';
   CDHelp[13] := ' It is possible for the ions to produce a';
   CDHelp[14] := ' degeneracy pressure also.This model treats a';
   CDHelp[15] := ' partially degenerate gas.';
   for i := 16 to 25 do CDHelp[i]:='';
   Help(CDHelp);
END;

{---------------------------------------------------------------------------}

PROCEDURE ShowEqState;
BEGIN
   DefineViewPort(6, 0.4, 1.0, 0.10, 0.90);
   GraphBackColor := blue;
   repeat
    SetColor(Whitte);
    OpenViewPort(6);
    Opacity(x,y,z,density,temperature);
    EquationOfState(x,y,z,density,temperature);
    Print(2,01,'             EQUATION OF STATE');
    Print(2,02,'The thermodynamic properties of the star,    ');
    Print(2,03,'pressure (in dyne/cm^2) and its derivitives, ');
    Print(2,04,'are functions of x, y, z, temperature and    ');
    Print(2,05,'density. Since the radiative dLn(T)/dLn(P)   ');
    Print(2,06,'is a function of L and M at radius r, it is  ');
    Print(2,07,'given for L/M = 1.');
    Print(4,09,concat('x (Hydrogen fraction): ',NumStr(x,9,3)));
    Print(4,10,concat('y (Helium fraction)  : ',NumStr(y,9,3)));
    Print(4,11,concat('density (g/cc)       : ',NumStr(density,9,5)));
    Print(4,12,concat('temperature (K)      : ',NumStr(temperature,9,0)));
    SetColor(Yellow);
    Print(2,13,'COMPUTED RESULTS FOR PARAMETERS ABOVE: ');
    Print(4,14,concat('Radiation pressure = ',NumStrExp(radpressure,12)));
    Print(4,15,concat('Ion       pressure = ',NumStrExp(ionpressure,12)));
    Print(4,16,concat('Non-deg/deg e pres = ',NumStrExp(Nonpressure,10),' /'
                 ,NumStrExp(Degpressure,10)));
    Print(4,17,concat('Electron  pressure = ',NumStrExp(electpressure,12)));
    Print(4,18,concat('dLn(T)/dLn(P) rad* = ',NumStrExp(dLnTdLnPrad,12)));
    Print(4,19,concat('dLn(T)/dLn(P) ad   = ',NumStrExp(dLnTdLnPad ,12)));
   until not ParametersChanged;
   SetColor(Whitte);
   GraphBackColor := DarkGray;
END;

{---------------------------------------------------------------------------}

PROCEDURE ShowNeutrinos;

VAR
   neut  :Real;
   neutE :Real;
BEGIN
   DefineViewPort(6, 0.15, 0.85, 0.10, 0.90);
   GraphBackColor := blue;
   OpenViewPort(6);
   GraphBackColor := DarkGray;
   SetColor(whitte);
   neut  := Luminosity*SunLum/2.1e-5;     {neutrinos generated}
   neutE := neut/(4.0*pi*SQR(astrounit)); {neutrinos at Earth}
   Print(1, 2,'                 NEUTRINOS');
   Print(1, 4,' Neutrinos are produced in the fusion process that ');
   Print(1, 5,' provides the energy in a star. They carry away 2% ');
   Print(1, 6,' of the energy produced in the sun. For the present');
   Print(1, 7,' Luminosity, this star produces:                   ');
   SetColor(Yellow);
   Print(1,11,' COMPUTED RESULT: ');
   Print(8,12, concat('    ',NumStrExp(neut ,12),' neutrinos/s'));
   Print(8,13, concat('or  ',NumStrExp(neutE,12),' neutrinos/s/cm^2 at 1AU'));
   SetColor(whitte);
END;

{---------------------------------------------------------------------------}

PROCEDURE ShowCalcMenu;

BEGIN
 Message('Choose [  Ok  ] for calculation, [Cancel] for main menu');
END;

{---------------------------------------------------------------------------}

PROCEDURE ShowAcceptMenu;

BEGIN
 Message('<ENTER> or <DoubleClick> to accept density value; <ESC> for main menu');
END;

{---------------------------------------------------------------------------}

PROCEDURE Help1; {main help screen}

VAR
   helpScr    : HelpScrType;
BEGIN
   helpScr[1] :=   '                 HELP SCREEN               ';
   helpScr[2] :=   '                                           ';
   helpScr[3] :=   '   The menu choices are:                   ';
   helpScr[4] :=   '                                           ';
   helpScr[5] :=   '    File       - file save and Exit        ';
   helpScr[6] :=   '                                           ';
   helpScr[7] :=   '    Boundary   - the boundary conditions   ';
   helpScr[8] :=   '                 on the star and a         ';
   helpScr[9] :=   '                 description of the model  ';
   helpScr[10] :=   '                 assumptions and the       ';
   helpScr[11] :=   '                 equations used            ';
   helpScr[12] :=   '                                           ';
   helpScr[13] :=   '    Compute    - run a model calculation   ';
   helpScr[14] :=   '                                           ';
   helpScr[15] :=   '    Power      - compute energy generation ';
   helpScr[16] :=   '                                           ';
   helpScr[17] :=   '    Eq. State  - compute the thermodynamic ';
   helpScr[18] :=   '                 state of the star         ';
   helpScr[19] :=   '                                           ';
   helpScr[20] :=   '    Opacity    - calculate opacity factors ';
   helpScr[21] :=   '                                           ';
   helpScr[22] :=   '   To perform a simulation, set the        ';
   helpScr[23] :=   '   parameters of the star in the Boundary  ';
   helpScr[24] :=   '   Conditions menu and then go to Run      ';
   helpScr[25] :=   '   Model in the Compute menu.              ';

   help(helpScr);
END;

{---------------------------------------------------------------------------}
{*-----  Menu Response handling Procedures Including some Physics     -----*}
{---------------------------------------------------------------------------}


PROCEDURE BoundaryConditions;

BEGIN
 WITH MyMenu DO CASE rowChosen OF
  1: SetBoundaryConditions;

  { ------------------  Chemical Composition -------------------}

  2: ModifyChemicalComposition;

  { ------------------ Explain Conditions -------------------}

  3: StellarConditions;

  { ------------------ Explain Equations -------------------}

  4: Equations;

  { ------------------ Convection/Degeneracy -------------------}

  5: ConvectionDegeneracyScreen;

  { ------------------    Neutrinos          -------------------}

  6: ShowNeutrinos;

  { ------------------    Reinitialize       -------------------}

  7: InitializeModel;

 END; {CASE}
END;   {BoundaryConditions}

{---------------------------------------------------------------------------}

PROCEDURE Compute; {do the stellar calcs}

VAR
 Error :Integer; {error returned by StellarModel}
 fname :String;
 datafile :Text;
 i     :Integer;
 xx    :Real;
 yy    :Real;
 numplot :Integer;
 Learning :Boolean; {Flag used to determine if learning boundary effects}
BEGIN

  Learning := False;
  IF (MyMenu.rowChosen = 2) THEN Learning := True; {this is a learning exercise}

  PlotControl := 1;  {update plot if >0}

  radius := SQRT(Luminosity)/SQR(Teffective/SunTsurface); {initial r}

  WITH MyMenu DO CASE rowChosen OF

   1,2: BEGIN  {Run Model}
      FOR i:=0 to keepsize DO    {must clear arrays again}
        BEGIN
          M_array[i] :=0;
          T_array[i] :=0;
          D_array[i] :=0;
          R_array[i] :=0;
          L_array[i] :=0;
          K_array[i] :=0;
          E_array[i] :=0;
          P_array[i] :=0;
        END; {FOR}

       FitMass := FitMassFraction*mass;  {desired fit mass}

       ClearMUPPETPort;
       InitializeModelPlot(5);  {prepare plot screen}

       StellarModel(Error,Learning);  {go compute the model}

      END; {Run Model}

   3: ShowResults;

   4: BEGIN  {Plot Results}
       numplot := 5;
       REPEAT
        ClearMUPPETPort;
        comment (0.1,0.945, concat('  Te:',NumStr(Teffective,4,0)
            ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
            ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2)));
        Message('Click on plot zoom in/out, <ESC> for main menu');

        InitializeModelPlot(numplot);  {prepare plot screen}
        SetColor(Yellow);

        FOR i := 0 TO keepsize DO
           BEGIN
              IF(PlotMassOrRadius)THEN BEGIN xx := R_array[i]/radius; END
                             ELSE BEGIN xx := M_array[i]/mass; END;  {x plot coordinate: mass fraction}
              IF (T_array[i] > 0)THEN BEGIN
                yy := Log(10.0,T_array[i]);
                IF((numplot=5) OR (numplot=1))THEN PlotDataModel(1,xx,yy); {T vs. M}
              END; {IF}
              IF (D_array[i] > 0)THEN BEGIN
                yy := Log(10.0,D_array[i]);
                IF((numplot=5) OR (numplot=2))THEN PlotDataModel(2,xx,yy); {D vs. M}
              END; {IF}
              IF (R_array[i] > 0)THEN BEGIN
                yy := Log(10.0,R_array[i]);
                IF((numplot=5) OR (numplot=3))THEN PlotDataModel(3,xx,yy); {R vs. M}
              END; {IF}
              IF (L_array[i] > 0)THEN BEGIN
                yy := Log(10.0,L_array[i]);
                IF((numplot=5) OR (numplot=4))THEN PlotDataModel(4,xx,yy); {L vs. M}
              END; {IF}
           END; {FOR}

        REPEAT;
           CheckForEvents;
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

   5: BEGIN  {Plot More Results}
       numplot := 5;
       REPEAT
        ClearMUPPETPort;
        comment (0.1,0.945,
            concat('  Te:',NumStr(Teffective,4,0)
            ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
            ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2)));
        Message('Click on plot to zoom in/out, <ESC> for main menu');
        InitializeMoreModelPlot(numplot);  {prepare plot screen}

        FOR i := 0 TO keepsize DO
           BEGIN
              SetColor(Yellow);
              IF(PlotMassOrRadius)THEN BEGIN xx := R_array[i]/radius; END
                             ELSE BEGIN xx := M_array[i]/mass; END;  {x plot coordinate: mass fraction}
              IF (K_array[i] > 0)THEN BEGIN
                yy := Log(10.0,K_array[i]);
                IF((numplot=5) OR (numplot=1))THEN PlotDataModel(1,xx,yy); {K vs. M}
              END; {IF}
              IF (E_array[i] > 0)THEN BEGIN
                NuclearPower(x,y,z,znitrogen,D_array[i],T_array[i]);
                yy := Log(10.0,powerpp);
                IF((numplot=5) OR (numplot=2))THEN PlotDataModel(2,xx,yy); {E vs. M}
              END; {IF}
              IF (P_array[i] > 0)THEN BEGIN
                yy := Log(10.0,P_array[i]);
                IF((numplot=5) OR (numplot=3))THEN PlotDataModel(3,xx,yy); {P vs. M}
              END; {IF}
              IF ((D_array[i] > 0) AND (T_array[i] > 0))THEN BEGIN
                EquationOfState(x,y,z,D_array[i],T_array[i]);
                SetColor(LightGreen); {Adiabatic}
                yy := Log(10.0,dLnTdLnPad);
                IF((numplot=5) OR (numplot=4))THEN PlotDataModel(4,xx,yy); {derivitive vs. M}
              END; {IF}
        END; {FOR}

        FOR i := 0 TO keepsize DO  {must do again for second plot on same viewport}
           BEGIN
              IF(PlotMassOrRadius)THEN BEGIN xx := R_array[i]/radius; END
                             ELSE BEGIN xx := M_array[i]/mass; END;  {x plot coordinate: mass fraction}
              IF (E_array[i] > 0)THEN BEGIN
                NuclearPower(x,y,z,znitrogen,D_array[i],T_array[i]);
                SetColor(LightGreen); {total power}
                yy := Log(10.0,power); {total energy}
                IF((numplot=5) OR (numplot=2))THEN PlotDataModel(2,xx,yy); {E vs. M}
              END; {IF}
              IF ((D_array[i] > 0) AND (T_array[i] > 0))THEN BEGIN
                EquationOfState(x,y,z,D_array[i],T_array[i]);
                SetColor(Yellow); {Radiative}
                yy := Log(10.0,(dLnTdLnPrad*L_array[i]/M_array[i]));
                IF((numplot=5) OR (numplot=4))THEN PlotDataModel(4,xx,yy); {derivitive vs. M}
              END; {IF}
        END; {FOR}

        REPEAT;
           CheckForEvents;
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

   6: ComputeInputScreen;

  END; {CASE menu}


END;   {Compute}

{---------------------------------------------------------------------------}

PROCEDURE FileMenuItem; {FIle menu handling}

VAR
 fname    :String;
 datafile :Text;
 i        :Integer;
 dummy    :Real;
BEGIN

  WITH MyMenu DO CASE rowChosen OF

   1: AboutStellar;

   2: AboutCUPS;

   3: GettingStarted;

   4: BEGIN  {File Save}
       dummy :=0;
       fname := concat('ST',NumStr(Mass,7,3));
       WHILE POS(' ',fname) > 0 DO BEGIN fname[POS(' ',fname)] := '0'; END;
       ASSIGN(datafile,fname);  {on the MAC, this becomes OPEN}
       REWRITE(datafile);
       WRITELN(datafile,Mass,      '  :Mass      ');
       WRITELN(datafile,Tsurface,  '  :Tsurface  ');
       WRITELN(datafile,Teffective,'  :Teffective');
       WRITELN(datafile,Luminosity,'  :Luminosity');
       WRITELN(datafile,Tcentral,  '  :Tcentral  ');
       WRITELN(datafile,Dcentral,  '  :Dcentral  ');
       WRITELN(datafile,dummy,     '  :TimeFromZAMS');
       WRITELN(datafile,concat('Columns are: Mass, Temperature, Density, Radius,',
              ' Luminosity, Opacity, Power, Pressure'));
       FOR i := 0 to Keepsize DO
        BEGIN
          IF(T_array[i] > 0)THEN
            BEGIN
             WRITELN(datafile, M_array[i], T_array[i], D_array[i], R_array[i]
               ,L_array[i], K_array[i], E_array[i], P_array[i]);
            END;
        END;
        CLOSE(datafile);
      comment (0.1,0.9,
      concat('The computed model data results were written to file: ',fname));
      END; {File}

   5: Configuration;

   6: BEGIN CUPSdone; HALT; END;

  END; {CASE menu}


END;   {FileMenuItem}

{---------------------------------------------------------------------------}

PROCEDURE PowerGeneration;

VAR
   i  :Integer;
   j  :Integer;
   i1 :Integer;
   t  :Real;
   t1 :Real;
   d  :Real;
BEGIN

  WITH MyMenu DO CASE rowChosen OF

   { ------------------   Energy Generation   -------------------}

   1: BEGIN  { Compute Energy Generation}
        ClearMUPPETport;
        ShowCalcMenu;
        ShowEnergy;
      END;

   { ------------------    Plot Energy Generation   -------------}

   2: BEGIN  {Plot Energy Generation}
       ClearMUPPETPort;
       HideCursor;
       t :=1e10;
       NuclearPower(x,y,z,znitrogen,density,t);
       NuclearPowerAlpha(x,y,z,znitrogen,density,t);
       power := (power + power3alpha)*100;
       InitializeEnergyPlot;
       comment (0.1,0.945,
           '  pp, CNO, 3 alpha and Total Power versus T Computed by Model ');
       znitrogen := z*nitrogenfraction;
       i1 := Round(100.0*Log(10.0,t));

          SetColor(LightGreen);
       FOR i := 600 TO i1 DO      {do each plot seperately}
       BEGIN
          t  := i/100;
          t1 := Pwr(10.0,t);
          NuclearPower(x,y,z,znitrogen,density,t1);
          PlotDataModel(5,t,Log(10.0,Powerpp)); {E vs. T}
       END; {DO}

          SetColor(Yellow);
       FOR i := 600 TO i1 DO
       BEGIN
          t  := i/100;
          t1 := Pwr(10.0,t);
          NuclearPower(x,y,z,znitrogen,density,t1);
          PlotDataModel(5,t,Log(10.0,PowerCNO)); {E vs. T}
       END; {DO}

          SetColor(LightGray);
       FOR i := 600 TO i1 DO
       BEGIN
          t  := i/100;
          t1 := Pwr(10.0,t);
          NuclearPower(x,y,z,znitrogen,density,t1);
          NuclearPowerAlpha(x,y,z,znitrogen,density,t1);
          PlotDataModel(5,t,Log(10.0,Power3alpha)); {E vs. T}
       END; {DO}

          SetColor(Whitte);
       FOR i := 600 TO i1 DO
       BEGIN
          t  := i/100;
          t1 := Pwr(10.0,t);
          NuclearPower(x,y,z,znitrogen,density,t1);
          NuclearPowerAlpha(x,y,z,znitrogen,density,t1);
          PlotDataModel(5,t,Log(10.0,Power+Power3alpha)); {E vs. T}
       END; {DO}

       ShowCursor;
       HotKeys.Display;
       MyMenu.Display;
      END;

  END; {CASE menu}


END; {PowerGeneration}

{---------------------------------------------------------------------------}

{on Mac need $S stellar3}

PROCEDURE HandleEqofState;

CONST
   plotcompress = 20;
VAR
   i  :Integer;
   j  :Integer;
   i1 :Integer;
   t  :Real;
   t1 :Real;
   d  :Real;
   DensitySlider: Tsliders;
BEGIN
  {New(DensitySlider);}{needed on MAC}
  WITH MyMenu DO CASE rowChosen OF

   { ------------------    Equation of State  -------------------}

   1: BEGIN  { Eq. of State}
        ClearMUPPETport;
        ShowCalcMenu;
        ShowEqState;
      END;

   { ------------------    Plot Equation of State   -------------}

   2: BEGIN  {Plot Equation of State}
        DensitySlider.init;
        d := density;

        REPEAT
            ClearMUPPETPort;
            ShowAcceptMenu;
            t := 1e10;
            Opacity(x,y,z,density,t);
            EquationOfState(x,y,z,density,t);
            InitializeEqStatePlot(d);
            znitrogen := z*nitrogenfraction;
            i1 := Round(plotcompress*Log(10.0,t)); {plot density}

            DensitySlider.create(1,0.1,10000,d,0,0.05,1,1,'0','10000','Density',false);
            DensitySlider.drawall;
            SetColor(LightGreen);
            HideCursor;

            FOR j := 1 TO 2 DO
             BEGIN
              FOR i := 3*plotcompress TO i1 DO
               BEGIN
                t  := i/plotcompress;
                t1 := Pwr(10.0,t);
                Opacity(x,y,z,d,t1);
                EquationOfState(x,y,z,d,t1);
                PlotDataModel(1,t,Log(10.0,ionpressure)); {p vs. T}
                PlotDataModel(2,t,Log(10.0,Degpressure)); {p vs. T}
                PlotDataModel(3,t,Log(10.0,Nonpressure)); {p vs. T}
                PlotDataModel(4,t,Log(10.0,electpressure)); {p vs. T}
               END; {DO}
               d := d/10;
               SetColor(Yellow);
             END; {DO}
            ShowCursor;
            SetColor(Whitte);

             with event do
             REPEAT
              CheckForEvents;
              IF(DensitySlider.changed)THEN d:=DensitySlider.value(1);
              IF(d < 0.1)THEN d := 0.1;
             UNTIL (Readkey=chr(27)) OR (Readkey=chr(13)) OR DoubleClicked;

        UNTIL (Event.Readkey=chr(27));
        DensitySlider.Delete(1);
        DensitySlider.Done;
        ClearMUPPETport;
      END;

   { ------------------    Plot More Equation of State  ----------}

   3: BEGIN  {Plot More Equation of State}
        DensitySlider.init;
        d := density;

        REPEAT
            ClearMUPPETPort;
            ShowAcceptMenu;
            t := 1e10;
            Opacity(x,y,z,density,t);
            EquationOfState(x,y,z,density,t);
            InitializeMoreEqStatePlot(d);
            znitrogen := z*nitrogenfraction;
            i1 := Round(plotcompress*Log(10.0,t)); {plot density}

            DensitySlider.create(1,0.1,10000,d,0,0.05,1,1,'0','10000','Density',false);
            DensitySlider.drawall;
            SetColor(LightGreen);
            HideCursor;

            FOR j := 1 TO 2 DO
             BEGIN
              FOR i := 3*plotcompress TO i1 DO
               BEGIN
                t  := i/plotcompress;
                t1 := Pwr(10.0,t);
                Opacity(x,y,z,d,t1);
                EquationOfState(x,y,z,d,t1);
                PlotDataModel(1,t,Log(10.0,dLnTdLnPrad)); {p vs. T}
                PlotDataModel(2,t,Log(10.0,dLnTdLnPad)); {p vs. T}
                PlotDataModel(3,t,Log(10.0,pressure)); {p vs. T}
                PlotDataModel(4,t,Log(10.0,radpressure)); {p vs. T}
               END; {DO}
               d := d/10;
               SetColor(Yellow);
             END; {DO}
            ShowCursor;
            SetColor(Whitte);

             with event do
             REPEAT
              CheckForEvents;
              IF(DensitySlider.changed)THEN d:=DensitySlider.value(1);
              IF(d < 0.1)THEN d := 0.1;
             UNTIL ((Readkey=chr(27)) OR (Readkey=chr(13))) OR DoubleClicked;

        UNTIL (Event.Readkey=chr(27));
        DensitySlider.Delete(1);
        DensitySlider.Done;
        ClearMUPPETport;
      END;

  END; {CASE menu}
  {Dispose(DensitySlider);}{needed on MAC}

END; {HandleEqofState}


{---------------------------------------------------------------------------}

PROCEDURE Complications; {Opacity main menu item}

CONST
   plotcompress = 50;
VAR
   i  :Integer;
   j  :Integer;
   i1 :Integer;
   t  :Real;
   t1 :Real;
   d  :Real;
   DensitySlider: Tsliders;
BEGIN

  {New(DensitySlider);}{needed on MAC}
  WITH MyMenu DO CASE rowChosen OF


   { ------------------      Opacity          -------------------}

   1: BEGIN  { Eq. of State}
        ClearMUPPETport;
        ShowCalcMenu;
        ShowOpacity;
      END;

   { ------------------       Plot Opacity          -------------}

   2: BEGIN  {Plot Opacity}

        DensitySlider.init;
        d := density;

        REPEAT
            ClearMUPPETPort;
            ShowAcceptMenu;
            t := 1e10;
            Opacity(x,y,z,density,t);
            InitializeOpacityPlot(d);
            znitrogen := z*nitrogenfraction;
            i1 := Round(plotcompress*Log(10.0,t));

            DensitySlider.create(1,0.1,10000,d,0,0.05,1,1,'0','10000','Density',false);
            DensitySlider.drawall;

            SetColor(LightGreen);
            HideCursor;
            FOR j := 1 TO 2 DO
             BEGIN
              FOR i := 3*plotcompress TO i1 DO
               BEGIN
                t  := i/plotcompress;
                t1 := Pwr(10.0,t);
                Opacity(x,y,z,d,t1);
                PlotDataModel(1,t,Log(10.0,kelectron)); {k vs. T}
                PlotDataModel(2,t,Log(10.0,kKramer)); {k vs. T}
                PlotDataModel(3,t,Log(10.0,kHminus)); {k vs. T}
                PlotDataModel(4,t,Log(10.0,kmolecular)); {k vs. T}
               END; {DO}
               d := d/10;
               SetColor(Yellow);
             END; {DO}
             ShowCursor;
             SetColor(Whitte);

             with event do
             REPEAT
              CheckForEvents;
              IF(DensitySlider.changed)THEN d:=DensitySlider.value(1);
              IF(d < 0.1)THEN d := 0.1;
             UNTIL ((Readkey=chr(27)) OR (Readkey=chr(13))) OR DoubleClicked;

        UNTIL (Event.Readkey=chr(27));
        DensitySlider.Delete(1);
        DensitySlider.Done;
        ClearMUPPETport;
      END;

   { ------------------        Plot More Opacity    -------------}

   3: BEGIN  {Plot More Opacity}
        DensitySlider.init;
        d := density;

        REPEAT
            ClearMUPPETPort;
            ShowAcceptMenu;
            t := 1e10;
            Opacity(x,y,z,density,t);
            InitializeMoreOpacityPlot(d);
            znitrogen := z*nitrogenfraction;
            i1 := Round(plotcompress*Log(10.0,t));

            DensitySlider.create(1,0.1,10000,d,0,0.05,1,1,'0','10000','Density',false);
            DensitySlider.drawall;

            SetColor(LightGreen);
            HideCursor;
            FOR j := 1 TO 2 DO
             BEGIN
              FOR i := 3*plotcompress TO i1 DO
               BEGIN
                t  := i/plotcompress;
                t1 := Pwr(10.0,t);
                Opacity(x,y,z,d,t1);
                PlotDataModel(1,t,Log(10.0,kradiative));  {k vs. T}
                PlotDataModel(2,t,Log(10.0,kconductive)); {k vs. T}
                PlotDataModel(3,t,Log(10.0,kopacity));    {k vs. T}
               END; {DO}
              d := d/10;
              SetColor(Yellow);
             END; {DO}
             ShowCursor;
             SetColor(Whitte);

             with event do
             REPEAT
              CheckForEvents;
              IF(DensitySlider.changed)THEN d:=DensitySlider.value(1);
              IF(d < 0.1)THEN d := 0.1;
             UNTIL ((Readkey=chr(27)) OR (Readkey=chr(13))) OR DoubleClicked;

        UNTIL (Event.Readkey=chr(27));
        DensitySlider.Delete(1);
        DensitySlider.Done;
        ClearMUPPETport;
      END;

  END; {CASE menu}
  {Dispose(DensitySlider);}{needed on MAC}

END; {Complication}

{---------------------------------------------------------------------------}

PROCEDURE HandleMenu;

BEGIN
   ClearMUPPETPort;
   HotKeys.Display;
   WITH mymenu DO
    CASE colChosen OF
     1: FileMenuItem;
     2: BoundaryConditions;
     3: Compute;
     4: PowerGeneration;
     5: HandleEqofState;
     6: Complications;
    END; {CASE}
   MyMenu.Display; {make sure menu is up}
   HotKeys.Display;
END;  {HandleMenu}

{---------------------------------------------------------------------------}

{on Mac need $S stellar4}

PROCEDURE HotKeyHandle (key :byte);

BEGIN
   CASE key OF
    1 : Help1;
    2 : IF MyMenu.Chosen THEN HandleMenu;
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
   {New(HotKey);} {needed on MAC}
   {New(MyMenu);} {needed on MAC}
   InitializeModel;
   SetUpHotKeys;
   SetUpMyMenu;
   AboutStellar;
   REPEAT
      CheckForEvents;
      IF HotKeys.Pressed(key) THEN HotKeyHandle(key);
      IF MyMenu.Activated THEN HandleMenu;
   UNTIL false; {repeat forever}
END.
