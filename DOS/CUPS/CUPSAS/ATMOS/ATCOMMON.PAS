unit Atcommon;

{---------------------------------------------------------------------------}
{*-----        Declare all global variables, constants, and controls           -------*}
{---------------------------------------------------------------------------}
{Modified 6/14/94}

interface
  
USES  crt, dos, graph, printer, CupsMUPP,CUPS,cupsscrn, CUPSfunc,CupsProc,cupsgui, CUPSGrph;

CONST
 c = 2.998e10;            {speed of light cm/s}
 G = 6.67e-8;             {gravitational constant cm^3/g/s^2}
 sigma = 5.67e-5;         {Boltzmannconstant erg/cm^2/s/K^4}

 SunMass = 1.99e33;       {mass of Sun}
 SunLum  = 3.83e33;       {luminosity of Sun}
 SunRadius = 6.96e10;     {radius of Sun}

CONST
		maxStars = 10;
		maxshells = 20;
		maxNumfreqs = 36;
		{MaxWN = 54;}

		fourPi = 12.5663708;
		solarmass = 1.989e33; {gm, for computing dimensionless quantities for computation}
		solarLum = 3.827e33; {eg/sec}
		solarRad = 6.960e10; {cm}
		SolarDen = 1.409; {mean density: gm/cm3}
		Gas = 8.314e07;{Gas constant: erg/deg/mole}
		Grav = 6.67e-08;{G ravitation constant: dyn cm2/gm2}
		lightSpeed = 2.9979e10;
		mH = 1.66e-24;
		Thomson = 0.66524e-24;
{physical variables in atmosphere}
		pres = 1;
		temp = 2;
		dens = 3;
		Hion = 4;
		Heion = 5;
		adgrad = 6;
		radgrad = 7;
		kapp = 8;
		mmwt = 9;
		Tau = 10;
		HepIon = 11;
		Pe = 12;
		volu = 13;
		tGrad = 14;
		height = 15;
		dTau = 16;
		MachNo = 17; {convective speed divided by isothermal sound speed}
		ConFlux = 18;
{ radn moments}
		Iup = 1;
		Idn = 2;
		Jay = 3;
		Flux = 4;
		SFn = 5;
		Planck = 6;
{frequency}
		mean  = 1;
		monoL = 2;
		monoU = 3;


{model type}
		TConst = 1;
		RadEq  = 2;
		IncCon = 3;
{absorber}
		gray = 1;
      StepF = 2;
		Hyd  = 3;
		HydHe = 4;


		null = 0;
		firstmodel = 1;
		secmodel = 2;
      BBody=3;

{types of list in display window}
		structList = 1;
		radiationLIst = 2;

		LowBand = 1; {index for filter pass bands}
		HighBand = 2;

{Windows}
		Sky= 2;
		CM = 4;
		Spectrum = 11;
      opacPlot=16;
      setUp=1;
      Data=3;
      spectOpt=12;
      TAdjust=13;
      EscBut=14;
      AcceptBut=15;
      structdata=1;
      RadnData=2;
{===========================================}
TYPE
		unitsType = (solar, cgs);
		garray = array[1..3] of real; {used for RK integration in Build newlayer}
		TShell = object
			vari: dVector;
			Radn: dMatrix; {we carry the mean/integrated}
         opacity: dVector; {monochromatic opacity}
         procedure Init;
         procedure done;
{and a single mono value for each moment of the radiation field}
{KapMono: array[0..maxNumfreqs] of real;}
 {0 = mean, others mono}
{TauMono: array[0..maxNumfreqs] of real; }
{0 = mean, others mono}
			end;

		passbandtype = record {for determination of colors from spectrum}
				LowerWN, UpperWN, CenterWN: real;{limits in wavenumber}
				response: real; {relative sensitivity}
			end;

		str30 = string[30];
		str6 = string[6];
		str20 = string[20];


		framedata = record {data for plottting in each frame of a window}
		                xscale, yscale, logYscale: real;
				xright, xleft, ytop, ybottom: real;
		                end;

		windowRec = record
				firstPlot, lockscales: boolean;{if true, we look for scales}
				logarithmic, TauScale: boolean;
		        	end;

		modeltype = record
				name: str30;
				modtype: integer; {TConst = 1; RadEq = 2;IncCon = 3;}
				absorber: integer; {gray = 1; StepF = 2, Hyd = 3, H+He = 4;}
            wstep,stepFact:real; {location of simple step function in opacity;
            opacity increases by stepfact going to blue across edge}
				Q, L, K: real; {coeffs for picket fence solution}
				GrEps: real; {for gray atmosphere}
				numshells: integer;
				TauBot, TauTop: real;
				muZero: real; {cosine of ray angle from vertical}
				shell: array[0..maxshells] of TShell; {0 shell is starting point; not plotted}
				mass, rad, lum, Sflux, Teff, dynGrav, PRPrime: real;
				Habund, Heabund, metabund: real;
				Fluxlocked, incrad, inclradpress, incscatt: boolean;
				EmFLux: array[0..maxNumFreqs] of real;  {emitted flux from surface}
				gasbuilt, radFound: boolean;
				B_V, MVIS, MBOL, Ly_Distance, MV_App: real; {photometric parameters, evaluated in Procedure FINDRADIATON}
				plotcolor: Integer;
				{srect: rect;*}{for plotting in CM diagram}
			end;
      showshelltype = set of 0..maxnumfreqs;
		SpData = record
				lam, mag: real;
{lam = wave length in microns; mag = magnitude per unit frequency relative to value at lam = 5000A}
			end;
		starType = record
				starname: str20;
				designation: str20;
				RA, DEC: real;
				MVIS, mv_app, b_v, Ly_dist: real;
				spectralType: str6;
				spectrum: array[1..20] of SpData;
			end;


		constellationType = record
				name: str20;
				numstars: integer;
				star: array[1..maxStars] of startype;
			end;
		RECO = file of constellationtype;
      OpModeType = (menumode,selectstarsmode,specifyModMode,BuildMode,spectrumMode,AdjustTMode,PlotMode,OpacityMode);

{=======================================================}

var
		StarFileIsNamed: Boolean;
      StarFile   : string;
		StarFileVRefNum: integer;
		StarFIleName: string; {file used to store constellation data}
		starDataFile: RECO; {type defined to hold constellation data records}
		myConstellation: constellationType;
	       {	starrect: array[cmframe..skyframe, 1..maxstars] of rect;   *}
		ThisMod: modeltype;
                conSelected, StarSelected:integer; {constellation selected}
                numconstellations:integer;
                models: array[1..2] of modeltype;
		modSel: integer;
               	theLIst: integer;
		unitsselected: unitstype;
		DRSelected_Scatt: boolean;{absorber checkbox}
		EnterReady, CloseReady, DoSave: boolean;
		Isprinting: boolean;
		StructureRec, SpectrumRec, RadRec, SkyRec, temperatureRec: windowRec;
		maxval, minval: array[1..18] of real; {maximum values for finding  scales}
		Dchanged: boolean;{data entry  filter}
		ESUpperWN, ESLowerWN: real;
		ESBBTemp: real;
		solarFlux: real;{evaluated in init_atmos}
		EddUnstable, building: boolean; {unstable against radiation pressure}
		WNList: array[1..3,1..maxnumfreqs] of real; {wave numbers for monochromatic calculations}
		AFeut, BFeut, CFeut, DFeut, GFeut, UFeut, VFeut, ZFeut: array[1..maxshells] of real;
		numFreqs: integer; {number of frequencies=wave numbers used to cover specified interval}
		pictID: integer; {for identifying help pict}
		BuildIndex: integer;
      wN0: real; {standard WN for opaicty normalization; set in INIT}
		passBand: array[LowBand..HighBand] of passbandType;
		ReBuildmodel: boolean; {If TRUE, program rebuilds model using adjusted temperatures; }
{set TRUE by Accept button In Adjust_Temperature}
		TabGradT: array[0..maxshells] of real; {used for numerical derivative in FindGradT}
		B_Vconst, MBOLconst, MVISconst: real; {usedf for evaluating parameters; set in Init_Atmos}

    {plotting stuff}
                plotModel: array[1..2] of boolean;
                showSpectrum:array[1..3] of boolean;
                showshell:showshelltype;
                ListModel:integer;
                shellSel:integer; {for AdjustTMOde}
                gDoingMore:boolean; {for plotting in second window }
                gVarID:array[1..8] of integer; {identify data for frames}
                sb,st:array[1..2,1..8] of real;  {vertical scale limits}
                xr,xl:array[1..8] of real;  {horizontal scale limits}
                labelstr:array[1..2,1..8] of str30;
                myRight,myLower,myUpper:array[1..8] of real; {for axes of each frame}
                SameScale:boolean;{for models}
                lockScale:boolean; {for spectrum}
                ShowAllStars:Boolean; {CM diagram}
                AutoClear:boolean;
                firstPlot :boolean;

                MyMenu        :TMenu;
                Range         :real; {X and Y plot to same scale}
                GenHotkeys, MoveHotKeys       :THotKeys; {hot keys for general status}
                              {and for select stars mode}
                modelBuilt: array[1..2] of boolean; {is the model ready for plotting?}
                gMode:OpmodeType;
                Okay, Changed: boolean; {for returning from subroutines}
                quitNow: boolean; {exit program}

                SpecifyScrn, spectrumScrn,GetAxesScrn : TDialogScreen;
                TempSlider : TSliders; {for setting wave number range of emitted spectrum display}
                constBut:tbuttons;
                lockScaleBut,ClearBut:TButtons;
                ShowMod1But,showMod2But,ShowbbBut:tButtons;
                consName:array[1..10] of str20;
                TempArray:array[1..2,1..maxShells] of real; {for T adjust}
 {---------------------------------------------------------------------------}

implementation

procedure TShell.init;
begin
	vari.init(18);
   Radn.init(3,6);
   opacity.init(maxnumFreqs);
end;

procedure TShell.done;
begin
	vari.free;
   Radn.free;
   opacity.free;
end;

END.