unit evolvec;

{******************************************************************}
{*    Part of EVOLVE: Computes the Evolution of a Star            *}
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

Interface
USES crt,graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph;

CONST

 c = 2.998e10;            {speed of light cm/s}
 G = 6.67e-8;             {gravitational constant cm^3/g/s^2}
 sigma = 5.67e-5;         {Stefan-Boltzmannconstant erg/cm^2/s/K^4}
 astrounit = 1.4e13;      {Astronomical unit cm}
 secondsperyear = 3.15e7; {seconds per year}

 nitrogenfraction = 0.3;  {fraction of N in heavy ions}
 SunMass = 1.99e33;       {mass of Sun in grams}
 SunLum  = 3.83e33;       {luminosity of Sun erg/sec}
 SunRadius = 6.96e10;     {radius of Sun in cm}
 SunAge = 4.6e9;          {age of sun in years}
 SunTcentral = 15.5e6;    {central T of sun in K}
 SunTsurface = 5800;      {surface T of sun}
 SunDcentral = 147.2;     {central density of sun g/cc}
 SunX = 0.71;             {x of sun}
 SunY = 0.27;             {y of sun}
 SunZ = 0.02;             {z of sun}
 EnergyHtoHe = 7e18;      {E release by converting 1g H into He - 26 MeV/He}
 Energy3HetoC = 5.9e17;   {E release by converting 1g He into C - 7.275 MeV/C}
 Keepsize = 50;           {array size for keeping profiles}
 HRsize = 150;            {array size for HR diagram}
TYPE
 Vector5  = Array[1..5]      of Double;
 Array5x5 = Array[1..5,1..5] of Double;

VAR
 MyMenu        :TMenu;   {menu}
 precision     :Double; {desired fit accuracy at fitting point}
 ModelPrecision :Double;{desired accuracy in Stellar Model=10*precision}
 FractionDelX  :Double; {fractional change in composition allowed per time step}
 ModIteration  :Integer;{iteration counter in Stellar Model}
 PlotControl   :Double; {Plot control:0=none,1=yes}
 TwoTo14       :Double; {PWR(2,.25)}
 slastsavedlum :Double; {used in compute to clear if scale changed}
 slastsavedden :Double; {used in compute to clear if scale changed}
 ComputedModel :Integer; {0 unless calculated values are current}
 ProtoPressure :Boolean; {F if no pressure, T if pressure on in Protostar}
 ProtoTemp     :Boolean; {F if no temp change, T if temp on in Protostar}
 ProtoLum      :Boolean; {F if no lum change, T if lum change on in Protostar}
 DeltaTimeProto:Double;  {time step in protostar model}
 Hotkeys       :THotKeys; {hot keys}
 HRHotkeys     :THotKeys; {hot keys}
 ProtoHotkeys  :THotKeys; {hot keys}
 ScreenMap     :Timage;  {place to stick pieces of screen}
 PlotColor     :Integer; {color value used on data screen in compute}
 SoundOnOff    :Boolean; {flag for beep on/off}
 MessageUp     :Boolean; {flag for message on screen}
 EvLoop        :Integer; {counts number of time steps in compute}
 xLast         :Array[1..6] of Real; {last plotted x,y pair}
 yLast         :Array[1..6] of Real; {last plotted x,y pair}

 x             :Array[0..Keepsize] of Double; {Hydrogen by mass fraction }
 y             :Array[0..Keepsize] of Double; {Helium by mass fraction}
 z             :Array[0..Keepsize] of Double; {Heavy ions by mass fraction}
 znitrogen     :Array[0..Keepsize] of Double; {Nitrogen by mass fraction}

 TimefromZero  :Double; {time from when star begins in years}
 TimefromZAMS  :Double; {time from when star reached zams in Millions of yrs}
 density       :Double; {Density g/cc}
 temperature   :Double; {Temperature}
 Mass          :Double; {Mass of star}
 Radius        :Double; {Effective radius of star surface}
 Luminosity    :Double; {Luminosity of star surface}
 Tsurface      :Double; {Surface temperature}
 Teffective    :Double; {Effective temperature of surface}
 Tcentral      :Double; {Central temperature of star}
 Dcentral      :Double; {Central density of star}

 FitMass       :Double; {mass where match inward and outward integs}
 FitMassFraction :Double;{mass fraction for fitmass}
 Sundensity    :Double; {average sun density}
 Constgrrad    :Double; {constant sun lum/sun mass/(16*pi*c*G)}
 ConstdPdM     :Double; {constant G/4/pi*(Sun mass/Sun radius**2)**2}

 kopacity      :Double; {Opacity cm^2/g}
 kelectron     :Double; {electron scattering opacity}
 kKramer       :Double; {Kramer's opacity}
 kHminus       :Double; {H minus opacity}
 kmolecular    :Double; {molecular opacity}
 kradiative    :Double; {total radiative opacity}
 kconductive   :Double; {electron conductivity opacity}

 powerpp       :Double; {Power generated by p-p}
 powerCNO      :Double; {Power generated by CNO}
 power3alpha   :Double; {Power generated by 3 alpha burning}
 power         :Double; {Power generated by p-p + CNO}
 dxdt          :Double; {rate of Hydrogen depletion}
 dydt          :Double; {rate of Helium depletion}
 dzdt          :Double; {rate of Heavy depletion}

 pressure      :Double; {total p, radiation+ion+electron (dyne/cm^2)}
 dLogPdLogT    :Double; {d log(p)/d log(T) at constant density}
 dLogPdLogd    :Double; {d log(p)/d log(density) at constant T}
 radpressure   :Double; {radiation pressure}
 ionpressure   :Double; {ion pressure}
 electpressure :Double; {electron pressure}
 gaspressure   :Double; {gas pressure, ion+electron}
 DegRpressure  :Double; {pressure degenerate relativistic electrons}
 DegNRpressure :Double; {pressure non-relativistic degenerate electrons}
 Degpressure   :Double; {combined pressure of degenerate electrons}
 Nonpressure   :Double; {non-degen electron pressure, perfect gas law}
 dLnTdLnPrad   :Double; {radiative temperature gradiant}
 dLnTdLnPad    :Double; {d logT/d logP const entorpy; adiabatic T grad}
 InternalEnergy:Double; {internal energy per unit mass}

 corevals      :Vector5; {Core values of fit parameters}
 surfacevals   :Vector5; {Envelope values of fit parameters}
 corevalsT     :Vector5; {Core values with T varied}
 surfacevalsT  :Vector5; {Envelope values with T varied}
 corevalsD     :Vector5; {Core values with Density varied}
 surfacevalsL  :Vector5; {Envelope values with Lum varied}
 maxparamstep  :Vector5; {Maximum step size for each parameter}
 corrections   :Vector5; {corrections computed by CorrectBoundary}
 maxcorrect    :Vector5; {max correct appliedto parms in CorrectBoundary}
 derivitive    :Array5x5;{Derivitive of fit point vals}

 M_array       :Array[0..Keepsize] of Double; {mass profile}
 T_array       :Array[0..Keepsize] of Double; {temperature profile}
 D_array       :Array[0..Keepsize] of Double; {density profile}
 R_array       :Array[0..Keepsize] of Double; {radius profile}
 L_array       :Array[0..Keepsize] of Double; {luminosity profile}
 K_array       :Array[0..Keepsize] of Double; {opacity profile}
 E_array       :Array[0..Keepsize] of Double; {energy rate profile}
 A_array       :Array[0..Keepsize] of Double; {3 alpha energy rate profile}
 P_array       :Array[0..Keepsize] of Double; {pressure profile}
 V_array       :Array[0..Keepsize] of Double; {velocity profile}
 TS_array      :Array[0..HRsize] of Double; {Surface temp profile}
 LS_array      :Array[0..HRsize] of Double; {Luminosity temp profile}


FUNCTION  NumStrExp(Num: Double; width : Integer): string;
PROCEDURE InitializeModel;
PROCEDURE PrepareForModelCalc; {get ready to do the stellar calcs}
PROCEDURE StellarModel(VAR Error, Loop :Integer); {Compute a Stellar Model}
PROCEDURE InitializeModelPlot(numplot :Integer);
PROCEDURE PlotDataModel(screen :Integer; x :Double; y :Double);
PROCEDURE InitializeMoreModelPlot(numplot :Integer);
PROCEDURE NuclearPower(VAR xx,yy,zz,zn,d,t :Double); {Nuc Power Generation}
PROCEDURE InitializeCompositionPlot(numplot :Integer);
PROCEDURE InitializeProtostarPlot;
PROCEDURE InitializeProtostar; {set up protostar t=0 values}
PROCEDURE ProtostarEvolution;
PROCEDURE EquationOfState(VAR xx,yy,zz,d,t :Double);{Calculate Equation of State}
PROCEDURE InitializeThermodynamicsPlot;
PROCEDURE FreeFall;
PROCEDURE InitializeHRPlot(LowT: Double); {log(L) versus log(T)}

implementation

PROCEDURE InitializeModel;
{ This routine initializes: the global values for the stars parameters
to those of the Sun; some constants used in physics equations; arrays
of parameters used in the fitting proceedure; zeros out the arrays of the
stars interior condition as a function of mass (radius) which are filled
as the model computes}

VAR
 i :Integer;

BEGIN
 TwoTo14 := PWR(2.0,0.25);
 SoundOnOff := FALSE;
 MessageUp := FALSE;
 ComputedModel := 0;

 ProtoPressure := FALSE; {no pressure in protostar by default}
 ProtoTemp := FALSE; {no temp change in protostar by default}
 ProtoLum := FALSE; {no lum change in protostar by default}
 DeltaTimeProto := 1000;         {timestep in years}
 Mass := 1.0; {mass of star in solar units}
 Luminosity := 0.51;  {luminosity in solar units}
 Tsurface := 4250; {surface T}
 Teffective := Tsurface*TwoTo14; {effective T}
 Tcentral := 1.31e7; {central T}
 Dcentral := 66; {central density}

 FitMassFraction := 0.3;  {mass fraction for fit mass - was 1/3}
 FitMass := mass*FitMassFraction;  {mass where inward and outward matched}

 density := SunDcentral;  {density}
 temperature := SunTcentral; {temperature}
 TimeFromZAMS := 0; {millions of years from ZAMS}

 Constgrrad  := (SunLum/SunMass)/(16.0*pi*c*G); {used in EvaluateEq}
 ConstdPdM   := G/4.0/pi*SQR(Sunmass/SQR(Sunradius)); {used in EvaluateEq}
 Sundensity  := SunMass/(4*pi*Sunradius*Sunradius*Sunradius/3); {Ave Sun dnsty}

 maxparamstep[1] := 0.20; {Maximum step size for temperature - was .05}
 maxparamstep[2] := 0.20; {Maximum step size for density - was .15}
 maxparamstep[3] := 0.20; {Maximum step size for radius - was .05}
 maxparamstep[4] := 0.20; {Maximum step size for luminosity}
 maxparamstep[5] := 0.20; {Maximum step size for mass}

 precision       := 0.02; {desired fit accuracy at fitting point}
 ModelPrecision  := 10*precision;  {desired accuracy in Stellar Model}
 FractionDelX    := 0.01;  {fractional change in composition x and y per step}

 maxcorrect[1] := 0.02; {Teff}{max correction to be applied to each param}
 maxcorrect[2] := 0.10; {Lum }{in CorrectBoundary}
 maxcorrect[3] := 0.01; {Tcen - was .01}
 maxcorrect[4] := 0.10; {Dcen}
 maxcorrect[5] := 0.00; {Mass}

 FOR i:=0 TO keepsize DO    {Clear arrays}
  BEGIN
    M_array[i] :=0;
    T_array[i] :=0;
    D_array[i] :=0;
    R_array[i] :=0;
    L_array[i] :=0;
    K_array[i] :=0;
    E_array[i] :=0;
    A_array[i] :=0;
    P_array[i] :=0;
    V_array[i] :=0;
    x[i]       := SunX;  {Hydrogen mass fraction}
    y[i]       := SunY;  {Helium mass fraction}
    z[i]       := SunZ;  {Heavy mass fraction}
    znitrogen[i] := z[i]*nitrogenfraction;  {N mass fraction}
  END; {FOR}

 FOR i:=0 TO HRsize DO    {Clear arrays}
  BEGIN
    TS_array[i] :=0;
    LS_array[i] :=0;
  END; {FOR}

END; {InitializeModel}

{---------------------------------------------------------------------------}
{*-----               Graphics Proceedures                            -----*}
{---------------------------------------------------------------------------}

PROCEDURE InitializeOnePlot(xlabel,ylabel :string);
 {set up one plots for with both axis 0-9}

BEGIN
 GraphBackColor := Blue;
 SetLineStyle(0,0,1);
 SetColor(LightGreen);

 DefineViewport(1, 0.10, 0.90, 0.15, 0.90);  {whole screen}
 DefineScale(1,0,9,0,9); {T.vs.M scale}
 SelectScale(1);
 OpenViewPort(1);
 Axis(0,0,1,1);
 PutLabel(Left,ylabel);
 PutLabel(Bottom,xlabel);
 SetColor(Whitte);

 PlotColor :=2;              {Set up colors for cycling during calcultion}

END;

{---------------------------------------------------------------------------}

PROCEDURE InitializeModelPlot(numplot :Integer);
 {set up the plots for Compute, numplot=1..4 for seperate or 5 for all}

VAR
 i :Integer;
 s :Double;
 sb :Double;
 l :Double;
 lu :Double;
 tu :Double;
 t :Double;
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
   DefineViewport(1, 0.10, 0.45, 0.15, 0.47);  {bottom left}
   DefineViewport(2, 0.10, 0.45, 0.58, 0.90);  {top left}
   DefineViewport(3, 0.60, 0.95, 0.15, 0.47);  {bottom right}
   DefineViewport(4, 0.60, 0.95, 0.58, 0.90);  {top right}
  END
 ELSE
  BEGIN
   DefineViewport(numplot, 0.10, 0.90, 0.15, 0.90);  {whole screen}
 END;

 s := Log(10.0,Luminosity*10.0); {find upper scale}{just to test for clear}
 s := trunc(s+1);
 IF(s<>slastsavedlum)THEN
     BEGIN
       ClearMUPPETPort;
       comment (0,0.05,
       'Hot: cC=Tcentral dD=Density eE=Teff lL=Luminos pP=Pause rR=Replot sS=Stop +/-');
     END;
 slastsavedlum := s;
 s := Log(10.0,Dcentral*10.0); {find upper scale}{just to test for clear}
 s := trunc(s+1);
 IF(s<>slastsavedden)THEN 
     BEGIN
       ClearMUPPETPort;
       comment (0,0.05,
       'Hot: cC=Tcentral dD=Density eE=Teff lL=Luminos pP=Pause rR=Replot sS=Stop +/-');
     END;
 slastsavedden := s;

IF((numplot = 5) OR (numplot = 1))THEN BEGIN
 s := Log(10.0,Tcentral*10.0); {find upper scale}
 s := trunc(s+1);
 DefineScale(1,0.0,1.0,3.0,s); {T.vs.M scale}
 SelectScale(1);
 OpenViewPort(1);
 Axis(0.0,3.0,0.2,1.0);
 PutLabel(Left,'Log(Temperature)');
 PutLabel(Bottom,'M/Mstar');
END;

IF((numplot = 5) OR (numplot = 2))THEN BEGIN  
 s := Log(10.0,Dcentral*10.0); {find upper scale}
 s := trunc(s+1);
 DefineScale(2,0.0,1.0,-1.0,s); {density.vs.M scale}
 SelectScale(2);
 OpenViewPort(2);
 Axis(0.0,-1.0,0.2,1.0);
 PutLabel(Left,'Log(Density)');
END;

IF((numplot = 5) OR (numplot = 3))THEN BEGIN   {HR plot}
 DefineScale(3,5.0,3.0,-3.0,6.0); {luminosity.vs.temperature}
 SelectScale(3);
 OpenViewPort(3);
 Axis(3.0,-3.0,0.5,1.0);  {invertedaxis}
 PutLabel(Left,'Log(Luminosity)');
 PutLabel(Bottom,'Log(Effective Temperature)');
 Print(1,1,'HR Diagram');
 t := Log(10.0,2600/TwoTo14);  {plot ZAMS line}
 l := Log(10.0,0.001);
 tu := Log(10.0,73000/TwoTo14);
 lu := Log(10.0,16210676.0);
 SetColor(LightGreen);
 PlotLine(t,l,tu,lu);
 PlotSymbol(Log(10.0,6400),0,'*'); {mark sun's position on HR plot}
END;

IF((numplot = 5) OR (numplot = 4))THEN BEGIN
 s := Log(10.0,Luminosity*10.0); {find upper scale}
 s := trunc(s+1);
 sb :=s-3;
 DefineScale(4,0.0,1.0,sb,s); {Luminosity.vs.M scale}
 SelectScale(4);
 OpenViewPort(4);
 Axis(0.0,sb,0.2,1.0);
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
 s :Double;
 sb :Double;
BEGIN
 GraphBackColor := Blue;

 IF(numplot = 5)THEN
  BEGIN
   DefineViewport(1, 0.10, 0.45, 0.15, 0.47);  {bottom left}
   DefineViewport(2, 0.10, 0.45, 0.58, 0.90);  {top left}
   DefineViewport(3, 0.60, 0.95, 0.15, 0.47);  {bottom right}
   DefineViewport(4, 0.60, 0.95, 0.58, 0.90);  {top right}
  END
 ELSE
  BEGIN
   DefineViewport(numplot, 0.10, 0.90, 0.15, 0.90);  {whole screen}
 END;

 SetLineStyle(0,0,1);
 SetColor(LightGreen);

IF((numplot = 5) OR (numplot = 1))THEN BEGIN
 s :=4;
 IF(K_array[1]>0)THEN s := trunc(1+Log(10.0,K_array[1]*10.0)); {find upper scale}
 DefineScale(1,0.0,1.0,s-4,s); {K.vs.M scale}
 SelectScale(1);
 OpenViewPort(1);
 Axis(0.0,s-4,0.2,1.0);
 PutLabel(Left,'Log(Opacity)');
 PutLabel(Bottom,'M/Mstar');
END;

IF((numplot = 5) OR (numplot = 2))THEN BEGIN
 s := 1;
 FOR i := 1 TO Keepsize DO {find upper scale}
  BEGIN
   IF(E_array[i]>s)THEN s := E_array[i];
  END;
 s := Log(10.0,s*10.0);
 s := trunc(s+1);

 DefineScale(2,0.0,1.0,s-4,s); {energy.vs.M scale}
 SelectScale(2);
 OpenViewPort(2);
 Axis(0.0,s-4,0.2,1.0);
 PutLabel(Left,'Log(Power/gram)');
 PutLabel(Bottom,'M/Mstar');

 SetColor(LightGreen); {total power}
 Print(1,1,'pp+CNO');
 SetColor(Yellow); {pp power}
 Print(13,1,'pp');
 SetColor(LightGreen);
END;

IF((numplot = 5) OR (numplot = 3))THEN BEGIN
 IF(P_array[1]>0)THEN s := trunc(1+Log(10.0,P_array[1]*10.0)); {find upper scale}
 DefineScale(3,0.0,1.0,s-4,s); {pressure.vs.M scale}
 SelectScale(3);
 OpenViewPort(3);
 Axis(0.0,s-4,0.2,1.0);
 PutLabel(Left,'Log(Pressure)');
 PutLabel(Bottom,'M/Mstar');
END;

IF((numplot = 5) OR (numplot = 4))THEN BEGIN
 s := Log(10.0,radius*10.0); {find upper scale}
 DefineScale(4,0.0,1.0,-2.0,s); {radius.vs.M scale}
 SelectScale(4);
 OpenViewPort(4);
 Axis(0.0,-2.0,0.2,1.0);
 PutLabel(Left,'Log(Radius)');
 PutLabel(Bottom,'M/Mstar');
END;

 SetColor(Whitte);
END;

{---------------------------------------------------------------------------}

{needed on Mac $S evolve0}

PROCEDURE InitializeCompositionPlot(numplot :Integer);
 {plot setup for Composition}

VAR
 i :Integer;
 s :Double;
 sb :Double;
BEGIN
 GraphBackColor := Blue;

 IF(numplot = 5)THEN
  BEGIN
   DefineViewport(1, 0.10, 0.45, 0.15, 0.47);  {bottom left}
   DefineViewport(2, 0.10, 0.45, 0.58, 0.90);  {top left}
   DefineViewport(3, 0.60, 0.95, 0.15, 0.47);  {bottom right}
   DefineViewport(4, 0.60, 0.95, 0.58, 0.90);  {top right}
  END
 ELSE
  BEGIN
   DefineViewport(numplot, 0.10, 0.90, 0.15, 0.90);  {whole screen}
 END;

 SetLineStyle(0,0,1);
 SetColor(LightGreen);
 
IF((numplot = 5) OR (numplot = 1))THEN BEGIN
 DefineScale(1,0.0,1.0,-2.5,0.0); {x.vs.M scale}
 SelectScale(1);
 OpenViewPort(1);
 Axis(0.0,-2.5,0.2,0.5);
 PutLabel(Left,'Hydrogen mass fraction');
 PutLabel(Bottom,'M/Mstar');
END;

IF((numplot = 5) OR (numplot = 2))THEN BEGIN
 DefineScale(2,0.0,1.0,-2.5,0.0); {y.vs.M scale}
 SelectScale(2);
 OpenViewPort(2);
 Axis(0.0,-2.5,0.2,0.5);
 PutLabel(Left,'Helium mass fraction');
END;

IF((numplot = 5) OR (numplot = 3))THEN BEGIN
 DefineScale(3,0.0,1.0,-2.5,0.0); {y.vs.M scale}
 SelectScale(3);
 OpenViewPort(3);
 Axis(0.0,-2.5,0.2,0.5);
 PutLabel(Left,'Heavy mass fraction');
 PutLabel(Bottom,'M/Mstar');
END;

IF((numplot = 5) OR (numplot = 4))THEN BEGIN
 DefineScale(4,0.0,1.0,-2.5,0.0); {znitrogen.vs.M scale}
 SelectScale(4);
 OpenViewPort(4);
 Axis(0.0,-2.5,0.2,0.5);
 PutLabel(Left,'Nitrogen mass fractin');
 Print(1,1,concat('t=',NumStr(TimeFromZAMS,11,2),' Million years'));
 SetColor(whitte);
END;

END; {InitializeCompositionPlot}

{---------------------------------------------------------------------------}

PROCEDURE InitializeProtostarPlot;
 {Plots for Protostar}
VAR
 i :Integer;
 s :Double;
 sb :Double;
 sl :Double;
BEGIN

 FOR i:=1 to 6 DO    {Clear arrays}
  BEGIN
    xLast[i]:= 0;
    yLast[i]:= 0;
  END; {FOR}

 GraphBackColor := Blue;

 DefineViewport(1, 0.08, 0.30, 0.15, 0.47);  {bottom left}
 DefineViewport(2, 0.08, 0.30, 0.58, 0.90);  {top left}
 DefineViewport(3, 0.43, 0.65, 0.15, 0.47);  {bottom mid}
 DefineViewport(4, 0.43, 0.65, 0.58, 0.90);  {top mid}
 DefineViewport(5, 0.78, 0.99, 0.15, 0.47);  {bottom right}
 DefineViewport(6, 0.78, 0.99, 0.58, 0.90);  {top right}

 SetLineStyle(0,0,1);
 SetColor(LightGreen);

 DefineScale(1,0.0,1.0,0.0,8.0); {T.vs.M scale}
 SelectScale(1);
 OpenViewPort(1);
 Axis(0.0,0.0,0.25,1.0);
 PutLabel(Left,'Log(Temperature)');
 PutLabel(Bottom,'M/Mstar');

 DefineScale(2,0.0,1.0,-20.0,5.0); {density.vs.M scale}
 SelectScale(2);
 OpenViewPort(2);
 Axis(0.0,-20.0,0.25,5.0);
 PutLabel(Left,'Log(Density)');

 DefineScale(3,0.0,1.0,-1.0,8.0); {radius.vs.M scale}
 SelectScale(3);
 OpenViewPort(3);
 Axis(0.0,-1.0,0.25,1.0);
 PutLabel(Left,'Log(Radius/Rsun)');
 PutLabel(Bottom,'M/Mstar');

 DefineScale(4,0.0,1.0,-10.0,10.0); {luminosity.vs.M scale}
 SelectScale(4);
 OpenViewPort(4);
 Axis(0.0,-10.0,0.25,5.0);
 PutLabel(Left,'Log(Luminosity)');

 DefineScale(5,8.0,0,-10.0,10.0); {luminosity.vs.T surface - HR diagram}
 SelectScale(5);
 OpenViewPort(5);
 Axis(0,-10.0,1.0,5.0);
 PutLabel(Left,'Log(Luminosity)');
 PutLabel(Bottom,'Log(T Surface)');
 Print(1,2,'HR Diagram');

 DefineScale(6,0.0,1.0,-10.0,20.0); {pressure.vs.M scale}
 SelectScale(6);
 OpenViewPort(6);
 Axis(0.0,-10.0,0.25,5.0);
 PutLabel(Left,'Log(Pressure)');
 PutLabel(Bottom,'M/Mstar');

 SetColor(whitte);
 PlotColor :=2;              {Set up colors for cycling during calcultion}
END;

{---------------------------------------------------------------------------}

PROCEDURE InitializeThermodynamicsPlot;
BEGIN
 GraphBackColor := Blue;
 SetLineStyle(0,0,1);
 SetColor(LightGreen);
 DefineViewport(1, 0, 1, 0.1, 0.90);  {whole screen}
 DefineScale(1,0,9,0,9); {T.vs.M scale}
 SelectScale(1);
 OpenViewPort(1);
 Circle(290,225,140);
 SetColor(Whitte);
END;

{---------------------------------------------------------------------------}

PROCEDURE InitializeHRPlot(LowT: Double); {log(L) versus log(T)}
 {HR diagram plots}

VAR
 sxl  :Double;
 sxu  :Double;
 syl  :Double;
 syu  :Double;
BEGIN
 GraphBackColor := Blue;

 DefineViewport(5, 0.10, 0.90, 0.15, 0.90);  {whole screen}

 SetLineStyle(0,0,1);
 SetColor(LightGreen);

 syl  :=-4.0;
 syu  := 8.0;
 sxl  := LowT;
 sxu  := 5.0;

 DefineScale(5,sxu,sxl,syl,syu); {L.vs.T scale}
 SelectScale(5);
 OpenViewPort(5);
 Axis(sxl,syl,0.5,1.0);
 PutLabel(Left,'Log(Luminosity)');
 PutLabel(Bottom,'Log(Surface Temperature)');

 SetColor(whitte);
END;

{---------------------------------------------------------------------------}

PROCEDURE PlotDataModel(screen :Integer; x :Double; y :Double);
 {put up one point on plot for Compute/Inward/Outward}

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
 {replace one blanks with zero in a string for NumStrExp}

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

FUNCTION NumStrExp(Num: Double; width : Integer): string;
 {This produces an exponential format number for display - changed for mac}

VAR
 temp  :string;
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

PROCEDURE NuclearPower(VAR xx,yy,zz,zn,d,t :Double); {Nuc Power Generation}

{ Computes p-p and CNO nuclear energy generation rate in stellar material. }
{ Formulas from KW; agree with BP except higher order screen factor}
{ Inputs: x,y,z,znitrogen,density,temperature }
{ Output: powerpp, powerCNO, power (erg/g/sec): global }
{ Output: dxdt (g/g/sec): global Double } {hydrogen depletion rate}
{ Output: dydt (g/g/sec): global Double } {helium depletion rate}
{ Output: dzdt (g/g/sec): global Double } {heavy depletion rate}

VAR
 t6            :Double; {t6}
 t8            :Double;
 t6to13        :Double; {t6 to 1/3 power;used in NuclearPower}
 t6to23        :Double; {t6 to 2/3 power}
 screenpp      :Double; {screen factor for pp}
 screencno     :Double; {screen factor for cno}
 temporary     :Double;

BEGIN
 powerpp  :=    1.0e-30;   {no energy generated at low T}
 powerCNO :=    1.0e-30;
 power3alpha := 1.0e-30;
 power    :=    1.0e-30;

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
   t8 := t/1.0e8;         
   IF(t > 1.0e8)THEN
     BEGIN
        power3alpha := 5.09e11*SQR(d)*PWR((yy/t8),3)*Exp(-44.027/t8);
     END;
   power    := powerpp+powerCNO;
  END; {IF}

 dxdt  := -1.667e-19*power; {rate of hydrogen depletion, pp&CNO almost the same}
 dydt  := -1.7e-18*power3alpha - dxdt;  {rate of helium depletion}
 dzdt  :=  1.7e-18*power3alpha;         {rate of heavy depletion (increase)}
 power := power+power3alpha;
END;

{---------------------------------------------------------------------------}

PROCEDURE Opacity(VAR xx,yy,zz,d,t :Double);              {Opacity computation}

{ Computes opacity of stellar material. }
{ Inputs: x,y,z,density,temperature }
{ Output: opacity (cm^2/g) and opacity components: global}

VAR
 zaverage         :Double;

CONST
 aa = 2; {from BP was 6.0 Kramer}
 bb = 1; {from BP was .001 H-}
 cc = 1; {from BP was .001 molecular}

BEGIN
 zaverage := xx+2*yy+8*zz; {was 4*yy ?}

 {compute electron scattering opacity with high T and density
  corrections - fairly accurate (leading term from KW;
  corrections by BP from AC)}
 kelectron := 0.2*(1+xx)/((1+2.7e11*d/t/t)
              *(1+Pwr((t/4.5e8),0.86)));

{compute Kramer's term: free-free, bound-free, bound-bound transitions crudely - the factor A is from BP}
{kKramer := (13.7/aa)*(1+xx)*(0.0010+zz)*d*Pwr((1.0e7/t),3.5); BP formula}
 kKramer := (4.56/aa)*(1+xx)*(0.0025+zz)*d*Pwr((1.0e7/t),3.5); {Stein formula}

 {compute negative hydrogen ion opacity very crudely - factor b from BP}
 {compute molecular opacity very crudely - factor c from BP}

 IF(t > 4.0e4)THEN
   BEGIN
        kHminus := 1.0e10;
        kmolecular := 1.0e-5;
   END
 ELSE
   BEGIN
        kHminus := 65.0*SQRT(zz*d)*Pwr((t/3000.0),7.7); {BP multiplied this by bb}
        kmolecular := 0.1*zz; {BP multiplied this by cc}
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
        kconductive := zaverage*(2.6e-7)*SQR(t/d)*(1+PWR((d/2.0e6),0.66667));
   END;

 {compute total opacity}
 kopacity := 1.0/(1.0/kradiative+1.0/kconductive);

END;

{---------------------------------------------------------------------------}

PROCEDURE EquationOfState(VAR xx,yy,zz,d,t :Double);{Calculate Equation of State}

{ Computes eq. of state and thermodynamic quantities in stellar material. }
{ Formulas from BP}
{ Inputs: x,y,z,density,temperature }
{ Output: pressure, gaspressure, radpresure (dynes/cm^2): global }
{ Output: dLogPdLogT, dLogPdLogD, dLnTdLnPad : global }
{ Output: InternalEnergy : global }
{ Full ionization is assumed, but electrons may be partly degenerate,}
{ partly relativistic}

CONST
 radconst = 2.523e-15; {rad const a/3 (erg/cc/K^4)}
 ionconst = 0.8263e8;  {Bolzman const/proton mass (erg/g/K)}
 const1   = 1.0036e13; {KW -for non-relativistic deg e gas}
 const2   = 1.2435e15; {KW -for relastivistic deg e gas}

VAR
 MeanIonWeight  :Double; {Mean molecular weight of ions; take z oxygen}
 MeanElWeight   :Double; {Mean mol wght per free electron, full ionization}
 relativistic   :Double; {how relativistic the electrons are}
 dLogPEdLogT    :Double; {d log(elect P)/d log(T) at constant density}
 dLogPEdLogD    :Double; {d log(elect P)/d log(density) at constant T}
 qt             :Double; {T*Cv, where Cv is specific heat const V per gram}
 qr             :Double; {qt*(d log(T)/d log(density)) at constant entropy}

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

 InternalEnergy:= (3*ionconst*t/MeanElWeight)/2 + 3*radconst*t*t*t*t/d;

END;

{---------------------------------------------------------------------------}

PROCEDURE ModifyComposition; {Update composition profile}
        {The stars composition is modified as a function of mass}
        {according to pp+CNO and 3alpha rates. Constraint is x+y+z=1.}
        {X fraction decreases into y, y into z, and znitrogen is unchanged.}
        {Future improvements would be to keep track of various z components.}
CONST
 complimit = 1e-18; {minimum value of X}
VAR
 i          :Integer;
 ppmax      :Double;
 p3alphamax :Double;
 timescale  :Double;
 timescale3alpha  :Double;
 xcomplast  :Double;
 ycomplast  :Double;
BEGIN

  ppmax := 0;
  p3alphamax := 0;
  FOR i := 1 TO (Keepsize-1) DO
    BEGIN              {first, find max power rate in star for pp and 3alpha}
     IF((E_array[i]-A_array[i]) > ppmax) THEN ppmax := E_array[i]-A_array[i];
     IF(A_array[i] > p3alphamax) THEN p3alphamax := A_array[i];
  END; {DO}

  timescale := EnergyHtoHe/ppmax*FractionDelX; {step time to consume H}
  timescale3alpha := Energy3HetoC/p3alphamax*FractionDelX; {timeto consume He}
  IF(timescale > timescale3alpha)THEN
                                     timescale := timescale3alpha;
  TimeFromZAMS := TimeFromZAMS + timescale/secondsperyear/1e6; {millions yrs}

  FOR i := 0 TO Keepsize DO
    BEGIN                  {change the composition}
     IF((D_array[i] = 0.0) OR (T_array[i] = 0.0))THEN BEGIN
       x[i] :=0; y[i] := 0; z[i] := 0; END  {must flag missing points}
     ELSE BEGIN
       NuclearPower(x[i],y[i],z[i],znitrogen[i],D_array[i],T_array[i]);
       IF(x[i] + dxdt*timescale < complimit)THEN   {check for <0}
          BEGIN
           dydt := dydt + dxdt + (x[i] - complimit)/timescale; {fix He rate}
           x[i] :=complimit;           {don't allow to be negative}
          END
         ELSE
          BEGIN
           x[i] := x[i] + dxdt*timescale;
          END;
       y[i] := y[i] + dydt*timescale;
       IF(y[i] < complimit)THEN y[i] :=complimit;
       IF((x[i]+y[i]) >= (1.0-complimit))THEN
                    y[i] :=1.0-complimit-x[i]; {protect against overrun}
     END; {IF}
    END; {DO}

  FOR i := 0 TO Keepsize DO
    BEGIN                  {now, fill in x,y arrays}
       IF(x[i] > 0.0)THEN BEGIN
          xcomplast := x[i]; ycomplast := y[i]; END
       ELSE BEGIN
          x[i] := xcomplast; y[i] := ycomplast;
       END;
    END; {DO}

  FOR i := 1 TO (Keepsize-1) DO
    BEGIN                  {smooth the change, and update z}
       x[i] := (x[i-1]+x[i]+x[i+1])/3.0; {smooth neighbors}
       y[i] := (y[i-1]+y[i]+y[i+1])/3.0; {smooth neighbors}
       z[i] := 1.0 - x[i] - y[i];
  END; {DO}
  z[0] := 1.0 - x[0] - y[0];             {update z at edges}
  z[Keepsize] := 1.0 - x[Keepsize] - y[Keepsize];

END; {ModifyComposition}

{---------------------------------------------------------------------------}

PROCEDURE EvaluateEquations(VAR Params, Deriv :Vector5); {Eval Diff Eqs}

{ Evaluate differential equations of stellar structure}
{ Inputs: Array of initial parameters}
{ Output: Array of derivitives}
{ Params are: temperature, density, radius, Luminosity, mass}
{ Deriv: d Params[i]/d Params[1]}

VAR
 t           :Double; {temp}
 d           :Double; {density}
 r           :Double; {radius}
 l           :Double; {Luminosity}
 m           :Double; {mass}
 dLnTdLnP    :Double; {temperature gradiant in star dLnt/dLnp}
 dLnDdLnP    :Double; {density gradiant dLnDensity/dLnp}
 dPdM        :Double; {pressure gradient dLnp/d(M/Msun)}
 i           :Integer; {index}

BEGIN
 t := Params[1]; {temp}
 d := Params[2]; {density}
 r := Params[3]; {radius}
 l := Params[4]; {Luminosity}
 m := Params[5]; {mass}
 i := Trunc(Keepsize*m/Mass);

 Opacity(x[i],y[i],z[i],d,t); {Evaluate opacity first}
 EquationOfState(x[i],y[i],z[i],d,t); {Evaluate the thermodynamic eq of state}
 NuclearPower(x[i],y[i],z[i],znitrogen[i],d,t); {Evaluate energy generation}

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
 stepsize       :Double;  {integration step size}
 halfstepsize   :Double;  {integration half step size}
 temporary      :Double;
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

PROCEDURE Outward(Tcent, Dcent :Double;     {Integration from center outward}
                  VAR Core : Vector5;
                  VAR Error :Integer);

{ Integrates stellar structure equations from the center out to fit point}
{ Inputs: T core and core density }
{ Output: 5 values at fit point (T, density, radius, Luminosity, mass}

CONST
 maxiteration = 1000;    {maximum number of iterations}

VAR
 iteration       :Integer;{iteration counter}
 deviation       :Double; {deviation of mass at FitMass}
 coreradius      :Double; {core radius/sun radius}
 coremass        :Double; {core mass/sun mass}
 coreLuminosity  :Double; {core Luminosity/sun lum}
 xx              :Double;
 index           :Integer;


BEGIN
 NuclearPower(x[0],y[0],z[0],znitrogen[0],Dcent,Tcent); {compute power gener}
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
    xx := Core[5]/mass; {x plot coordinate: mass fraction}
    IF((xx >0.005) AND (xx < 0.99))THEN
     BEGIN
       PlotDataModel(1,xx,Log(10.0,Core[1])); {plot T.vs.M}
       PlotDataModel(2,xx,Log(10.0,Core[2])); {plot density.vs.M}
       PlotDataModel(4,xx,Log(10.0,Core[4])); {plot lum.vs.M}

       index := TRUNC(xx*Keepsize); {array index}
       T_array[index] := Core[1]; {save values}
       D_array[index] := Core[2];
       R_array[index] := Core[3];
       L_array[index] := Core[4];
       M_array[index] := Core[5];
       K_array[index] := kopacity;
       E_array[index] := power;
       A_array[index] := power3alpha;
       P_array[index] := pressure;
     END;
    END;

 UNTIL ((deviation <= precision) OR (iteration > maxiteration)); {Loop until reach fit mass}

 IF( deviation > precision)THEN
  BEGIN
   Message(concat('OUTWARD:Integral deviation too large:',NumStr(deviation,9,4)));
  END;

END; {outward}

{---------------------------------------------------------------------------}

PROCEDURE Inward(Teff, Lum :Double;  {Integration from surface inward}
                 VAR Surf :Vector5;
                 VAR Error :Integer);

{ Integrates stellar structure equations from the surface in to fit point}
{ Inputs: Teffective and Luminosity }
{ Output: 5 values at fit point (T, density, radius, Luminosity, mass}

CONST
 maxiteration = 1000;    {maximum number of iterations}

VAR
 iteration     :Integer; {iteration counter}
 deviation     :Double;  {deviation of mass at FitMass}
 xx            :Double;
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
    xx := Surf[5]/mass;   {x plot coordinate: mass fraction}
    IF((xx >0.005) AND (xx < 0.99))THEN
     BEGIN
       PlotDataModel(1,xx,Log(10.0,Surf[1])); {plot T.vs.M}
       PlotDataModel(2,xx,Log(10.0,Surf[2])); {plot density.vs.M}
       PlotDataModel(4,xx,Log(10.0,Surf[4])); {plot lum.vs.M}

       index := TRUNC(xx*Keepsize); {array index}
       T_array[index] := Surf[1]; {save values}
       D_array[index] := Surf[2];
       R_array[index] := Surf[3];
       L_array[index] := Surf[4];
       M_array[index] := Surf[5];
       K_array[index] := kopacity;
       E_array[index] := power;
       A_array[index] := power3alpha;
       P_array[index] := pressure;
     END;
    END;

 UNTIL ((deviation <= precision) OR (iteration > maxiteration)); {Loop until reach fit mass}

 IF( deviation > precision)THEN
  BEGIN
   Message(concat('INWARD:Integral deviation too large:',NumStr(deviation,9,4)));
  END;
END; {inward}

{---------------------------------------------------------------------------}

{needed on Mac $S evolve1}

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
 d1      :Double;  {temporary}
 d2      :Double;  {temporary}
 f1      :Double;  {temporary}
 f2      :Double;  {temporary}
 factor  :Double;  {reduction factor for corrects,or max correct}

BEGIN
 {Solve 4 simultaneous equations: satisfy 4 eqns for T,rho,r,L: sumover j:}
   {deriv[i,j]*corrections[j]+deriv[i,5]=0}

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

PROCEDURE StellarModel(VAR Error, Loop :Integer); {Compute a Stellar Model}

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

CONST
 maxiteration = 1000; {maximum number of iterations}
 Tcdel = 0.0011;    {increment in central T for derivative}
 Tsdel = 0.0011;    {increment in surface T for derivative}
 Dcdel = 0.0011;    {increment in central density for derivative}
 Lsdel = 0.0011;    {increment in surface Luminosity for derivative}

VAR
 maxdelta :Double;  {Largest delta in present iteration}
 i        :Integer; {loop counter}
 delta    :Vector5; {diffs of core/surface vals}
 key      :Char;
 percentchange :Double; {percent change hot keys make}
 xx       :Double;     
 yy       :Double;

BEGIN
 key :=' ';
 ModIteration := 0; {clear counter}
 percentchange := 0.1;


 FOR i:=0 TO Keepsize DO BEGIN znitrogen[i] := z[i]*nitrogenfraction; END;

 SelectViewPort(4); {put up radius result}
 Print(1,1,concat('r = ',NumStr(radius,10,2)));
 SelectViewPort(2); {put up time result}
 Print(1,1,concat('t=',NumStr(TimeFromZAMS,11,2),' Million years'));
 SelectViewPort(1); {put up time step}
 Print(1,1,concat('time step ',NumStr(EvLoop,4,0)));

 comment (0.05,0.96,concat('Hot Keys Change One Parameter by',
          NumStr(percentchange*100,5,1),'% After Each Iteration; +/- Change %'));

 REPEAT  {Repeat until converged or out of iters}
   ModIteration := ModIteration+1;      {Increment iteration value}

   IF(PlotControl = 2)THEN BEGIN PlotControl := 1 END; {Plotting back on}

   FOR i:=0 TO keepsize DO    {Clear arrays - must do this to avoid variation}
    BEGIN
     M_array[i] :=0;
     T_array[i] :=0;
     D_array[i] :=0;
     R_array[i] :=0;
     L_array[i] :=0;
     K_array[i] :=0;
     E_array[i] :=0;
     A_array[i] :=0;
     P_array[i] :=0;
     V_array[i] :=0;
    END; {FOR}

   Outward(Tcentral,Dcentral,corevals,error); {integrate center to fit pnt}
   Inward(Teffective,Luminosity,surfacevals,error); {integ from surfce in}

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
   Outward(Tcentral*(1+Tcdel),Dcentral,corevalsT,error); {dTc:integ out}
   Inward(Teffective*(1+Tsdel),Luminosity,surfacevalsT,error); {dTs:surface}
   Outward(Tcentral,Dcentral*(1+Dcdel),corevalsD,error); {dDc:integ out}
   Inward(Teffective,Luminosity*(1+Lsdel),surfacevalsL,error); {dLs:surface}

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

    comment (0.2,0.54,concat('Delta: T:',NumStrExp(delta[1],7)
          ,'  D:',NumStrExp(delta[2],7),'  R:',NumStrExp(delta[3],7)
          ,'  L:',NumStrExp(delta[4],7)));


   IF(SoundOnOff)THEN Beep;
   CorrectBoundary(derivitive); {Now correct boundary conds using derivs}

   IF(Keypressed)THEN      {look for a key press to escape or modify params}
     BEGIN
       key := Readkey; {look for ESC}
       IF((key = 'p') OR (key = 'P'))THEN BEGIN Beep; StaticPause; END;
       IF(maxdelta > ModelPrecision)THEN
        BEGIN              {if model didn't converge, change param}
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
             FOR i:=1 TO HRsize DO    {update HR plot}
              BEGIN
               IF (TS_array[i] > 0)THEN BEGIN
                xx := Log(10.0,TS_array[i]);
                yy := Log(10.0,LS_array[i]);               
                PlotDataModel(3,xx,yy); {L vs. T}
               END; {IF}
              END; {FOR}

             SelectViewPort(4); {put up radius result}
             Print(1,1,concat('r = ',NumStr(radius,10,2)));
             SelectViewPort(2); {put up radius result}
             Print(1,1,concat('t=',NumStr(TimeFromZAMS,11,2),' Million years'));
             SelectViewPort(1); {put up time step}
             Print(1,1,concat('time step ',NumStr(EvLoop,4,0)));
             comment (0,0.05,
              'Hot: cC=Tcentral dD=Density eE=Teff lL=Luminos pP=Pause rR=Replot sS=Stop +/-');
             comment (0.05,0.96,concat('Hot Keys Change One Parameter by',
                   NumStr(percentchange*100,5,1),
                   '% After Each Iteration; +/- Change %'));
           END;

         IF(key = 'l')THEN BEGIN Beep;Luminosity := Luminosity/(1+percentchange);END;
         IF(key = 'L')THEN BEGIN Beep;Luminosity := Luminosity*(1+percentchange);END;
         IF(key = 'd')THEN BEGIN Beep;Dcentral   := Dcentral/(1+percentchange);END;
         IF(key = 'D')THEN BEGIN Beep;Dcentral   := Dcentral*(1+percentchange);END;
         IF(key = 'c')THEN BEGIN Beep;Tcentral   := Tcentral/(1+percentchange);END;
         IF(key = 'C')THEN BEGIN Beep;Tcentral   := Tcentral*(1+percentchange);END;
         IF(key = 'e')THEN BEGIN Beep;Teffective := Teffective/(1+percentchange);END;
         IF(key = 'E')THEN BEGIN Beep;Teffective := Teffective*(1+percentchange);END;
        END;
     END; {look for param mod}


 UNTIL ((maxdelta < ModelPrecision) OR (ModIteration > maxiteration)
         OR (key = Chr(27))); {Loop}

 IF(maxdelta < ModelPrecision)THEN
    BEGIN                           {MODEL CONVERGED!}
      Error     :=0; {returned error value if no conversion}
      IF(key = Chr(27))THEN Error := -1;
      IF((key = 's') OR (key = 'S'))THEN Error := -2;
      IF(SoundOnOff)THEN Beep;
      Message(concat('Converged: Te:',NumStr(Teffective,4,0)
          ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
          ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2)
          ,'  <Enter> main menu '));
      ComputedModel :=1; {success flag}

      ModifyComposition; {update the composition profile}

      radius := SQRT(Luminosity)/SQR(Teffective/SunTsurface); {r/r sun}
      IF(SoundOnOff)THEN Beep;
      IF(Loop = 0)THEN StaticPause;
    END
 ELSE
    BEGIN
      IF(SoundOnOff)THEN Beep;
      Error     :=ModIteration; {returned error value if no conversion}
      Message(concat('Didnt Converge:Te:',NumStr(Teffective,4,0)
          ,'  L:',NumStr(Luminosity,4,3),'  Dc:',NumStr(Dcentral,5,1)
          ,'  Tc:',NumStr(Tcentral,8,0),'  M:',NumStr(Mass,4,2)
          ,'  <Enter> main menu '));
      IF(SoundOnOff)THEN Beep;
      StaticPause;
    END;

END; {StellarModel}

{---------------------------------------------------------------------------}

PROCEDURE PrepareForModelCalc; {get ready to do the stellar calcs}

VAR
 i     :Integer;

BEGIN
      FOR i:=0 TO keepsize DO    {must clear arrays again}
        BEGIN
          M_array[i] :=0;
          T_array[i] :=0;
          D_array[i] :=0;
          R_array[i] :=0;
          L_array[i] :=0;
          K_array[i] :=0;
          E_array[i] :=0;
          A_array[i] :=0;
          P_array[i] :=0;
          V_array[i] :=0;
        END; {FOR}
       FitMass := FitMassFraction*mass;  {desired fit mass}
       InitializeModelPlot(5);  {prepare plot screen}

END;   {PrepareForModelCalc}

{---------------------------------------------------------------------------}

PROCEDURE InitializeProtostar; {set up protostar t=0 values}

VAR
 i       :Integer;
 MassSum :Double;
 lastMassSum :Double;
 deltaR  :Double;
 rho0    :Double;

BEGIN
 {force inner core of high density -- prevente R from going to zero}
    M_array[0] := Mass/(keepsize+1);{small inner mass to force collapse}
    T_array[0] := 10;        {T same everywhere}
    D_array[0] := 1000;      {artificially high density}
    L_array[0] := 1e-19;     {artificial luminoscity}
    R_array[0] := PWR((3*M_array[0]*SunMass/(4*pi*D_array[0])),1.0/3.0)/SunRadius;
    V_array[0] := 0;
    MassSum    := M_array[0]*SunMass;
    rho0       := 1e-19;

 FOR i:=1 TO keepsize DO    {Initialize protostar arrays}
  BEGIN
    M_array[i] :=M_array[i-1]+(Mass-M_array[0])/(keepsize);{equal mass shells}
    lastMassSum :=MassSum;
    MassSum := M_array[i]*SunMass;      {mass inside}
    deltaR := (PWR(((3*MassSum)/(4*pi*rho0)),(1.0/3.0))
          - PWR(((3*lastMassSum)/(4*pi*rho0)),(1.0/3.0)))/SunRadius;{delta R}
    R_array[i] :=R_array[i-1]+deltaR;   {radius}
    T_array[i] :=2;                     {temperature 2 K}
    D_array[i] :=rho0;                  {density}
    L_array[i] :=1e-19;                 {Luminosity, prevent float fault}
    K_array[i] :=0;                     {N/A}
    E_array[i] :=0;                     {N/A}
    A_array[i] :=0;                     {N/A}
    EquationOfState(x[i],y[i],z[i],D_array[i],T_array[i]);
    P_array[i] :=gaspressure;                     {pressure}
    V_array[i] := 0;
  END; {FOR}
  P_array[0] := P_array[1]; {same pressure in center as first shell}
END;   {InitializeProtostar}

{---------------------------------------------------------------------------}

PROCEDURE FreeFall;
{This is an old protostar model which computes the energy change without the virial theorem}
{Evolve a protostar. Units are sun mass and sun radius, g/cc}
{Either free fall or freefall plus pressure effect is computed}
{Protostar is constrained not to grow bigger}

CONST
   gamma = 1.67;         {monotonic gas Cv/Cp}
   GasConstant = 8.31e7; {universal gas constant}

VAR
   secperyear :Double;
   ionconst   :Double;
   k          :Double;
   FourPi     :Double;
   radconst   :Double;
   Lconstant  :Double;

   pVtoGamma  :Double;
   TVtoGamma  :Double;
   i          :Integer;
   lifetime   :Double;
   deltatime  :Double;
   acceleration :Double;
   volume     :Double;
   lastMassSum :Double;
   MassSum    :Double;
   deltaU    :Double;
   lastR     :Double;
   deltaR    :Double;
   lastP     :Double;
   dTdM      :Double;
   deltaT    :Double;
   deltaL    :Double;
   deltaM    :Double;
   hkey      :Byte;

   accPress  :Double;
   r0        :Double;
   Vsound    :Double;
   MeanIonWeight :Double; {Mean mol wght, full ionization}
   NumberMoles :Double;
BEGIN
   secperyear := 3600*24*365.25;
   ionconst   := 0.8263e8; {boltzman const/proton mass (erg/g/K)}
   k          := 1.38e-16; {boltzman const}
   FourPi     := 4*pi;
   radconst   := 3*2.523e-15; {rad const a (erg/cc/K^4)}
   Lconstant  := 64*pi*pi*(radconst)*c/3; {Luminosity constant}

   MeanIonWeight := 1.0/(x[1]+y[1]/4.0+z[1]/16.0); {mean moleclr weight}
   hkey := 0;
   lifetime := 0;
   r0 := R_array[keepsize];       {outside radius}
   deltaT :=0;
   deltaM := M_array[1]*SunMass;  {all shells have same mass}
   NumberMoles := deltaM/MeanIonWeight; {moles}
   deltatime := DeltaTimeProto*secperyear; {time step in seconds}

   IF(ProtoPressure)THEN  comment (0.8,0.525,'Pressure On');
   IF(ProtoLum)THEN       comment (0.45,0.525,'Radiation On');
   IF(ProtoTemp)THEN      comment (0.1,0.525,'Temp Effect On');


   REPEAT   {repeat the calc until is very small in radius}

     lifetime := lifetime + DeltaTimeProto;  {time from start}
     MassSum    := M_array[0];
     lastP := P_array[1];

     FOR i:=1 TO keepsize DO    {update arrays for this time}
       BEGIN
         lastMassSum :=MassSum;
         MassSum := M_array[i];      {mass inside this radius}
         volume := fourpi*(PWR((SunRadius*R_array[i]),3)
                      -PWR((SunRadius*R_array[i-1]),3))/3; {initial volume}
         IF(volume<=0)THEN  volume := 1e25; {fake volume}

         TVtoGamma := LN(T_array[i])+(gamma-1)*LN(volume); {adiabatic TV^(gamma-1)=const}

     { 1) Find acceleration, and then update the velocity}

         acceleration:=-G*lastMassSum*SunMass/SQR(R_array[i]*SunRadius);

         IF(ProtoPressure)THEN BEGIN          {including pressure effects?}
              IF((i>1) AND (i<keepsize))THEN
               BEGIN
                accPress := fourpi*(lastP-P_array[i+1])
                            *R_array[i]*R_array[i]*SunRadius*SunRadius/deltaM;
                acceleration:= acceleration + accPress;
               END;
              lastP := P_array[i];        {save this pressure for next loop}
           END;

         V_array[i] := V_array[i] + acceleration*DeltaTime;     {velocity}

     { 2) Update the radius and then find the volume and density}

         lastR := R_array[i];           
         deltaR := V_array[i]*DeltaTime/Sunradius; {radius}
         R_array[i] :=R_array[i] +deltaR;          {update the radius}

      {this next part is just to make sure r remains well ordered; only go 95% if shells colide}

          IF(R_array[i] <= R_array[i-1])THEN BEGIN  
            R_array[i] := (0.95*R_array[i-1]+0.05*lastR); 
            END 
          ELSE  BEGIN
              IF(i = keepsize)THEN BEGIN
                IF(R_array[i] > r0)THEN R_array[i] := lastR;
                 END
              ELSE BEGIN
                IF(R_array[i] >= R_array[i+1])THEN
                   R_array[i] :=(0.95*R_array[i+1]+0.05*lastR);
              END;
          END;
          IF((i <> keepsize) AND (R_array[i] < SQRT(0.1/(keepsize-i))))THEN
                 R_array[i] := SQRT(0.1/(keepsize-i)); {arbitrary R minimum limit}


         volume := fourpi*(PWR((SunRadius*R_array[i]),3)
                     -PWR((SunRadius*R_array[i-1]),3))/3;
         IF(volume<=0)THEN
                     volume := 1e29; {fake volume if goes to 0}

         D_array[i] :=deltaM/volume;  {density}


     { 3) Compute physics conditions in star}

         Opacity(x[i],y[i],z[i],D_array[i],T_array[i]);
         K_array[i] := kopacity; {just save this for reference}

      {assume adiabatic process T(TV^(gamma-1)=const) and P(pV^gamma=const)}

         IF(ProtoTemp)THEN BEGIN              {including temp change effect?}
            T_array[i] := EXP(TVtoGamma-(gamma-1)*LN(volume));
            IF(T_array[i] < 2)THEN T_array[i] := 2;
            IF(T_array[i] > 1e6)THEN T_array[i] := 1e6;
         END;


         L_array[i] := SQR(SQR(T_array[i])*(R_array[i]*SunRadius))
                       *sigma*fourpi/SunLum;


     { 4) Now, Radiate away some energy}

         IF(ProtoLum)THEN BEGIN     {including temp change effect?}
            deltaT := L_array[i]*SunLum*
                   DeltaTime/(3*NumberMoles*GasConstant/2); {T loss ideal gas}
            IF(i=keepsize)THEN BEGIN
                dTdM := -T_array[i]/deltaM; END
             ELSE BEGIN
                dTdM := -T_array[i]/deltaM;
             END;
            deltaT :=0;
            IF(dTdM<0)THEN
                deltaT := Deltatime*dTdM*Lconstant*PWR(T_array[i],3)
                          *PWR(R_array[i]*SunRadius,4)
                          /kopacity/(3*NumberMoles*GasConstant/2);
            IF((i=10) AND (lifetime > 100000.0))THEN
                       deltaT:=deltaT;  {debugging stop point}
            T_array[i] := T_array[i] + deltaT; {radiate energy away}
            IF(T_array[i] < 2)THEN T_array[i] := 2;
            IF(T_array[i] > 1e6)THEN T_array[i] := 1e6;

            L_array[i] := SQR(SQR(T_array[i])*(R_array[i]*SunRadius))
                          *sigma*fourpi/SunLum;
         END;

     { 5) Update pressure}

         P_array[i] := NumberMoles*GasConstant*T_array[i]/volume;
         IF(P_array[i] < 1e-12)THEN P_array[i] := 1e-12;
     { 6) Update plots}

         PlotDataModel(1,MassSum/mass,Log(10.0,T_array[i])); {plot T.vs.M}
         PlotDataModel(2,MassSum/mass,Log(10.0,D_array[i])); {plot density.vs.M}
         PlotDataModel(3,MassSum/mass,Log(10.0,R_array[i])); {plot radius.vs.M}
         PlotDataModel(4,MassSum/mass,Log(10.0,L_array[i])); {plot lum.vs.M}
         PlotDataModel(6,MassSum/mass,Log(10.0,P_array[i])); {plot P.vs.M}
     END; {FOR}


     {smooth the pressure and other parameters}
            P_array[2] :=
                (P_array[1]+P_array[2]+P_array[3])/3;
            D_array[2] :=
                (D_array[1]+D_array[2]+D_array[3])/3;
            T_array[2] :=
                (T_array[1]+T_array[2]+T_array[3])/3;
            L_array[2] :=
                (L_array[1]+L_array[2]+L_array[3])/3;
            R_array[2] :=
                (R_array[1]+R_array[2]+R_array[3])/3;
            P_array[keepsize-1] :=
                (P_array[keepsize-2]+P_array[keepsize-1]+P_array[keepsize])/3;
            D_array[keepsize-1] :=
                (D_array[keepsize-2]+D_array[keepsize-1]+D_array[keepsize])/3;
            T_array[keepsize-1] :=
                (T_array[keepsize-2]+T_array[keepsize-1]+T_array[keepsize])/3;
            L_array[keepsize-1] :=
                (L_array[keepsize-2]+L_array[keepsize-1]+L_array[keepsize])/3;
            R_array[keepsize-1] :=
                (R_array[keepsize-2]+R_array[keepsize-1]+R_array[keepsize])/3;
     FOR i:=3 TO (keepsize-2) DO    {update arrays for this time}
          BEGIN
            P_array[i] :=
                (P_array[i-2]+P_array[i-1]+P_array[i]+P_array[i+1]+P_array[i+2])/5;
            D_array[i] :=
                (D_array[i-2]+D_array[i-1]+D_array[i]+D_array[i+1]+D_array[i+2])/5;
            T_array[i] :=
                (T_array[i-2]+T_array[i-1]+T_array[i]+T_array[i+1]+T_array[i+2])/5;
            L_array[i] :=
                (L_array[i-2]+L_array[i-1]+L_array[i]+L_array[i+1]+L_array[i+2])/5;
            R_array[i] :=
                (R_array[i-2]+R_array[i-1]+R_array[i]+R_array[i+1]+R_array[i+2])/5;
     END; {FOR}

     Setcolor(whitte);
     PlotDataModel(5,Log(10.0,T_array[keepsize]),
                      Log(10.0,L_array[keepsize])); {plot lum.vs.Te}

     RubOut(1,1,16,blue);
     Print(1,1,concat('t=',NumStr((lifetime/1000),7,0),' kyears'));
     SelectViewport(1);
     RubOut(1,1,16,blue);
     Print(1,1,concat(' Ts=',NumStrExp(T_array[keepsize],8),' K'));
     RubOut(1,2,16,blue);
     Print(1,2,concat(' Tc=',NumStrExp(T_array[1],8),' K'));
     SelectViewport(3);
     RubOut(1,1,16,blue);
     Print(1,1,concat('  r=',NumStr(R_array[keepsize],8,0),' '));
     SelectViewport(4);
     RubOut(1,1,16,blue);
     Print(1,1,concat('  L=',NumStrExp(L_array[keepsize],10)));

     SetColor(PlotColor);         {set the plot color}
     PlotColor := PlotColor+1;    {update the plot color}
     IF(PlotColor = 3)THEN PlotColor :=7;   {skip}
     IF(PlotColor = 8)THEN PlotColor :=10;   {skip}
     IF(PlotColor > 15)THEN PlotColor :=2;
     SetColor(PlotColor);         {set the plot color}

     CheckForEvents;
     IF(ProtoHotKeys.Pressed(hkey))THEN
      BEGIN
        CASE hkey OF     {1-pause, 2-replot 3-ESC}
         1: StaticPause;
         2: BEGIN
              InitializeProtostarPlot;
              SelectViewport(5);
              SelectScale(5);
              PlotSymbol(5-Log(10.0,6400),0,'*'); {mark sun's position on HR plot}
            END;
      END;
     END;
                            {stop on radius=1 or T>10^8}
   UNTIL ((hkey=3) OR (R_array[keepsize]<=1) OR (T_array[1] > 1e8));{repeat}

END;   {FreeFall}
{---------------------------------------------------------------------------}

{needed on Mac $S evolve2}

PROCEDURE ProtostarEvolution;
{Evolve a protostar. Units are sun mass and sun radius, g/cc}
{This is a simple model based on the virial theorem and ideal gas}
{Either free fall or freefall plus pressure effect is computed}
{Protostar is constrained not to grow bigger}

{First, the radius is adjusted due to acceleration of gravity}
{and pressure (if included). The gravitational potential energy change}
{determines the internal E change, causing a temperature rise.}
{Corrections are then made to the luminoscity and temperature due to }
{radiation, if it is turned on. Results are smoothed and plotted.}
{Model stops when the central temperature gets over 10^8 or radius =1.}

CONST
   gamma = 1.67;         {monotonic gas Cv/Cp}
   GasConstant = 8.31e7; {universal gas constant}
VAR
   secperyear :Double;
   ionconst   :Double;
   k          :Double;
   FourPi     :Double;
   radconst   :Double;
   Lconstant  :Double;
   GravityEnergy :Double;
   GravityEnergyLast :Double;

   i         :Integer;
   j         :Integer;
   lifetime  :Double;
   deltatime  :Double;
   acceleration :Double;
   volume    :Double;
   lastMassSum :Double;
   MassSum   :Double;
   answer    :string;
   deltaU    :Double;
   lastR     :Double;
   deltaR    :Double;
   lastP     :Double;
   deltaT    :Double;
   deltaL    :Double;
   deltaM    :Double;
   accPress  :Double;
   Paverage  :Double;
   r0        :Double;
   dVdM      :Double;
   dTdM      :Double;
   dLdM      :Double;
   dDdM      :Double;
   dUdM      :Double;
   dUdt      :Double;
   dDdt      :Double;
   dTdt      :Double;
   Vsound    :Double;
   MeanElWeight :Double; {Mean mol wght per free electron, full ionization}
   cee       :Double;
   value     :Double;
   R         :Double;
   hkey      :Byte;
   MeanIonWeight :Double; {Mean mol wght, full ionization}
   NumberMoles :Double;
   L_average :Double;
   T_average :Double;
BEGIN
   secperyear := 3600*24*365.25;
   ionconst   := 0.8263e8; {boltzman const/proton mass (erg/g/K)}
   k          := 1.38e-16; {boltzman const}
   FourPi     := 4*pi;
   radconst   := 3*2.523e-15; {rad const a (erg/cc/K^4)}
   Lconstant  := -64*pi*pi*(radconst)*c/3; {Luminosity constant}

   hkey := 0;
   lifetime := 0;
   r0 := R_array[keepsize];       {outside radius}
   lastP := P_array[1];

   deltaM := M_array[1]*SunMass;  {all shells have same mass}
   deltatime := DeltaTimeProto*secperyear; {time step in seconds}
   r0 := R_array[keepsize];       {outside radius}
   MeanIonWeight := 1.0/(x[1]+y[1]/4.0+z[1]/16.0); {mean moleclr weight}
   NumberMoles := deltaM/MeanIonWeight; {moles}

   IF(ProtoPressure)THEN  comment (0.8,0.525,'Pressure On');
   IF(ProtoLum)THEN       comment (0.45,0.525,'Radiation On');
   IF(ProtoTemp)THEN      comment (0.1,0.525,'Temp Effect On');

   REPEAT

         lifetime := lifetime + DeltaTimeProto;  {time from start}

         FOR i:=1 TO keepsize DO    {update arrays for this time}
           BEGIN

      {1. Find initial gravitational potential energy}
            GravityEnergyLast := G*M_array[i-1]*SunMass*deltaM/(R_array[i]*SunRadius);

      {2. update velocity}
            acceleration := -G*M_array[i-1]*SunMass/SQR(R_array[i]*SunRadius);

            IF(ProtoPressure)THEN BEGIN          {including pressure effects?}
              accPress := FourPi*(R_array[i-1]*R_array[i-1]*lastP
               -R_array[i]*R_array[i]*P_array[i+1])*SunRadius*SunRadius/deltaM;
              acceleration:= acceleration + accPress;
              lastP := P_array[i];
             END;

            V_array[i] := V_array[i] + acceleration*deltatime;  {v update-linear approximation}
             IF(ABS(V_array[i]) > 1e8)THEN
                V_array[i] := 1e8*V_array[i]/ABS(V_array[i]);{keep v finite (retain sign)}

      {3. update radius - linear approximation to integral}
            lastR := R_array[i];
            R_array[i] := R_array[i] + (V_array[i]*deltatime/SunRadius); {r}

   {this next part is just to make sure r remains well ordered; only go 95% if shells colide}
            IF(R_array[i] < (0.95*R_array[i-1]+0.05*lastR))THEN BEGIN
              R_array[i] := (0.95*R_array[i-1]+0.05*lastR);
             END
            ELSE  BEGIN
              IF(i = keepsize)THEN BEGIN
                IF(R_array[i] > r0)THEN R_array[i] := lastR;
                 END
              ELSE BEGIN
                IF(R_array[i] > (0.95*R_array[i+1]+0.05*lastR))THEN
                   R_array[i] :=(0.95*R_array[i+1]+0.05*lastR);
              END;
            END;
            IF((i <> keepsize) AND (R_array[i] < SQRT(1/(keepsize-i))))THEN
                                R_array[i] := SQRT(1/(keepsize-i));

      {4. update density and pressure}
            volume := FourPi*(PWR((SunRadius*R_array[i]),3)
                             -PWR((SunRadius*R_array[i-1]),3))/3;
            IF(volume > 0.0)THEN D_array[i] :=deltaM/volume;  {density}
            P_array[i] := NumberMoles*GasConstant*T_array[i]/volume; {ideal gas}

      {5. Virial theorem says 1/2 of gravity energy change goes into radiation
                 and 1/2 into internal energy}
          GravityEnergy := G*M_array[i-1]*SunMass*deltaM/(R_array[i]*SunRadius);
          deltaU := ABS(GravityEnergy-GravityEnergyLast)/2; {PE change/2}
          L_array[i] := L_array[i] + deltaU/(deltatime*SunLum); {update luminoscity}     
          deltaT := T_array[i]*deltaU*(gamma-1)/(P_array[i]*Volume);

          IF(ProtoTemp)THEN T_array[i] := T_array[i] + deltaT; {adjust T if effect turned on}

          IF(ProtoLum)THEN BEGIN
            deltaT := L_array[i]*SunLum*
                   DeltaTime/(3*NumberMoles*GasConstant/2); {T loss ideal gas}
            IF(ProtoTemp)THEN T_array[i] := T_array[i] - deltaT; {drop T from radiation}
            IF(T_array[i]<2)THEN T_array[i] := 2;
            L_array[i] := (sigma*(T_array[i]*T_array[i]*T_array[i]*T_array[i])
               *(FourPi/SunLum)*(R_array[i]*SunRadius*R_array[i]*SunRadius));
            IF(L_array[i]<0)THEN L_array[i] := 1e-19;
           END;

         END; {FOR}


      {6. have looped through star shells, now smooth out results}
         T_array[0] := T_array[1]; {force center to same as first shell}
         P_array[0] := P_array[1]; {force center to same as first shell}
         {smooth the pressure and other parameters}
         FOR i:=2 TO (keepsize-1) DO    {update arrays for this time}
          BEGIN
              P_array[i] := (P_array[i-1]+P_array[i]+P_array[i+1])/3;
              D_array[i] := (D_array[i-1]+D_array[i]+D_array[i+1])/3;
              T_array[i] := (T_array[i-1]+T_array[i]+T_array[i+1])/3;
              E_array[i] := (E_array[i-1]+E_array[i]+E_array[i+1])/3;
          END; {FOR}

         L_average := L_array[0];
         T_average := T_array[0]; 
         FOR i:=1 TO keepsize DO    {update plots for this time}
          BEGIN
            L_average := L_average+L_array[i]/keepsize;
            T_average := T_average+T_array[0]/keepsize;
            MassSum   := M_array[i];
            PlotDataModel(1,MassSum/mass,Log(10.0,T_array[i]));
            PlotDataModel(2,MassSum/mass,Log(10.0,D_array[i]));
            PlotDataModel(3,MassSum/mass,Log(10.0,R_array[i]));
            PlotDataModel(4,MassSum/mass,Log(10.0,L_array[i]));
            PlotDataModel(6,MassSum/mass,Log(10.0,P_array[i]));
          END; {FOR}

          PlotDataModel(5,Log(10.0,T_average),
                          Log(10.0,L_average)); {plot lum.vs.Te}

          Setcolor(whitte);
          RubOut(1,1,16,blue);
          Print(1,1,concat('t=',NumStr((lifetime/1000),7,0),' kyears'));
          SelectViewport(4);
          RubOut(1,1,16,blue);
          Print(1,1,concat('Ls=',NumStr(L_array[keepsize],9,5),'  '));
          SelectViewport(1);
          RubOut(1,1,16,blue);
          Print(1,1,concat('Ts=',NumStr(T_array[keepsize],10,1),' K'));
          SelectViewport(3);
          RubOut(1,1,16,blue);
          Print(1,1,concat('  r=',NumStr(R_array[keepsize],7,0),' '));

          SetColor(PlotColor);         {set the plot color}
          PlotColor := PlotColor+1;    {update the plot color}
          IF(PlotColor = 3)THEN PlotColor :=7;   {skip}
          IF(PlotColor = 8)THEN PlotColor :=10;   {skip}
          IF(PlotColor > 15)THEN PlotColor :=2;
          SetColor(PlotColor);         {set the plot color}

          CheckForEvents;
          IF(ProtoHotKeys.Pressed(hkey))THEN
           BEGIN
            CASE hkey OF     {1-pause, 2-replot, 3-ESC}
              1: StaticPause;
              2: BEGIN
                  InitializeProtostarPlot;
                  SelectViewport(5);
                  SelectScale(5);
                  PlotSymbol(5-Log(10.0,6400),0,'*'); {mark sun's position on HR plot}
                 END;
            END;
           END;

          lastP := P_array[1];

                            {stop on radius=1 or T>10^8}
      UNTIL ((hkey=3) OR (R_array[keepsize]<=1.1) OR (T_array[1] > 1e8));{repeat}

END;   {protostar evolution}

end.