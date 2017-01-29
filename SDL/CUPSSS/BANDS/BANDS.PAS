           (*************************************************)
           (*************************************************)
           (**                                             **)
           (**  Consortium of Upper-level Physics Software **)
           (**              ( CUPS Project )               **)
           (**      ( c) 1994 by John Wiley & Sons         **)
           (**  Compiled with Utilities Ver. 2.0 (95/10/10)**)
           (**                                             **)
           (*************************************************)
           (*************************************************)

{Translatable TURBO Pascal source code:}
{$M 65520,0,655360}
Program Bands;
{$N+}
{(**************************************************************)}
{(*MAC Compiling NOTE:                                         *)}
{(*      MUST compile using EXTENDED REALS, i.e.               *)}
{(*      set  EXTENDED_TO_REAL = TRUE                          *)}
{(**************************************************************)}
{Note TURBO Pascal ver 7 will ignore (However TURBO Pascal 6.0 will not)}
{these THINK Pascal compiler directives...so}
{DO NOT bracket them between} {MAC} {...} {CAM} {translation comments.}
(*{$S Zero Seg}*)

        {*******************************************************}
        {*            Program Electron Energy Bands            *}
        {*      Copyright (C)1992,93 by CUPS project and       *}
        {*          Roger W Rollins, Dept. of Physics,         *}
        {*          Ohio University, Athens, OH 45701          *}
        {*               First version 01/23/92                *}
        {*  ver 0.53  11/09/92                                 *}
        {*        Uses CUPS Utilities ver. 3.1 10/03/92        *}
        {*  ver 0.54  03/24/93                                 *}
        {*        Modified slightly to work correctly with the *}
        {*          CUPS Utilities 01/14/93                    *}
        {*        Corrected error in the printed value of E    *}
        {*          at the extreema of the Energy Eigenstate Fn*}
        {*  ver 0.55  03/25/93                                 *}
        {*        Modified information screens                 *}
        {*        Added Help Screens and better input checking *}
        {*  ver 0.56  05/22/93                                 *}
        {*        Added 'Configure' to 'File' item to conform  *}
        {*          with cups utilities version 3.4 (05/21/93) *}
        {*  ver 0.57  07/07/93                                 *}
        {*        Made max no. of Numerov integration pts=500  *}
        {*  ver 0.58  07/14/93                                 *}
        {*        Added the saving of history data for E vs k  *}
        {         so qualitative comparisons of E vs. k are    *}
        {*        much easier now.                             *}
        {*  ver 0.59  07/14/93                                 *}
        {*        Minor changes in history feature +           *}
        {*        Added History toggle.                        *}
        {*  ver 0.60  07/15/93                                 *}
        {*        Added slider for potential parameters        *}
        {*  ver 0.61  07/16/93                                 *}
        {*        Switched to the more transparent backwards   *}
        {*        numerov integration rather than making transf*}
        {*        Also fixed plotting scales and tic selection.*}
        {*  ver 0.62  07/19/93                                 *}
        {*        Code cleaned up a bit.                       *}
        {*        -Started development of Save data to file    *}
        {*         and Read data from previously saved files   *}
        {*        -Started development of                      *}
        {*         choose and plot wavefunction option         *}
        {*  ver 0.63  07/20/93                                 *}
        {*        Added Kronig-Penney analytic calc. of |t|    *}
        {*        and delta as a test of the numerical         *}
        {*        integration of Schrodinger eq.               *}
        {*        Found problem with wave fn calculation...    *}
        {*        After several days of checking the program   *}
        {*        verified the accuracy of t and delta etc.    *}
        {*        Everything checks out well but the wave fn.  *}
        {*        calculation is very sensitive at the zone    *}
        {*        boundaries.  Also strange problem that       *}
        {*        occurs only if Numorov_Pts = 100.  Ok if     *}
        {*        use 101 or 99 or anything but 100!  This     *}
        {*        numerical error was due to the infinitely    *}
        {*        steep square well potential.  The problem    *}
        {*        seems to have to do with round off error when*}
        {*        the integration grid falls right at the wall *}
        {*        of the square well.  Fixed the problem by    *}
        {*        making the well very steep but not infinitely*}
        {*        steep (width of changing part of pot.= 10^-6)*}
        {*  ver 0.64  07/23/93                                 *}
        {*        took out all the special checking procedures *}
        {*        used in debugging version 0.63.              *}
        {*  ver 0.65  07/25/93                                 *}
        {*        Added history feature to wave fn plots       *}
        {*  ver 0.65-0.73 07/27/93                             *}
        {*        Many improvements in use of CUPS utilities   *}
        {*        Added selection of wavefn. state by clicking *}
        {*        on the E(k) curve.                           *}
        {*  ver 0.74 and 0.74m  07/28/93                                *}
        {*        Translation to MAC ....works if              *}
        {*        EXTENDED REALS on the MAC.  If do not set    *}
        {*        Compile "option" Extended_TO_REAL=TRUE       *}
        {*        the program will APPEAR to work but will give*}
        {*        nonsense as output.                          *}
        {*  ver 0.75 07/31/93                                  *}
        {*        Essentially the same as version 0.74 but put *}
        {*        in translatable Pascal code form so can use  *}
        {*        my MacToDOS or DOStoMac programs to translate*}
        {*        back and forth between THINK and TURBO syntax*}
        {*  ver 0.76 08/1/93                                   *}
        {*        Added Point and click message or wavefn. plot*}
        {*        Fixed error in delete oldest hot key. (Still *}
        {*        does not work quite correctly with regard to *}
        {*        wavefns.)                                    *}
        {*        Moved InfV0 theory points touch right edge of*}
        {*        log(Eg) vs. log(V0) plot as suggested at CUPS*}
        {*        meeting.                                     *}
        {*        Read potential barrier into an array once to *}
        {*        speed up Numerov integration procedure.      *}
        {*        General clean-up of some of the code.        *}
        {*  ver 0.77 08/03/93                                  *}
        {*        -Can now swap positions (and size) of E vs. k*}
        {*        with the Wavefunction Plots.  This provides  *}
        {*        the option of showing a large Plot of the    *}
        {*        wavefunction while still showing E vs k.     *}
        {*        -Used more of the AutoScaling and TickSpace  *}
        {*        procedures for plotting (in CUPSmupp UNIT)   *}
        {*  ver 0.78 08/19/93                                  *}
        {*        Added choice of |Psi|^2, Re[Psi], or Im[Psi] *}
        {*        to plot-what menu column for wavefunction plt*}
        {*  ver 0.79  08/21/93                                 *}
        {*        -Modified history system so that WaveFn data *}
        {*         is also disposed of starting w oldest first.*}
        {*        -Added Multicell plot of Bloch Wavefns       *}
        {*  ver 0.80-0.81  08/22/93                            *}
        {*        -Corrected the calculation and display of    *}
        {*         reduced Bloch wave vectors for all display  *}
        {*         schemes when picking k with mouse for calc. *}
        {*         and display of the corresponding Bloch      *}
        {*         wavefunctions.                              *}
        {*        -Improved the axis labels on all zone-scheme *}
        {*         plots.                                      *}
        {*  ver 0.82  06/24/94                                 *}
        {*        -Minor cosmetic changes                      *}
        {*  ver 0.83  06/25/94                                 *}
        {*        -Fixed bug so slider is redrawn if change    *}
        {*         value from Lat.Pot. menu button             *}
        {*                                                     *}
        {*******************************************************}

  Uses
{MAC}
{    Graph, CUPSmupp, CUPS, CUPSgui, CUPSfunc, CUPSproc, CUPSgrph;}
{CAM}
{PC}
    DOS, Crt, Graph, CUPSmupp, CUPS, CUPSgui, CUPSfunc, CUPSproc, CUPSgrph;
{CP}
  Const
    VerNo = '0.83';
    VerDate = '06/25/94';
    Max_History_Level = 10;
    Max_WaveFnHistoryLevel = 6;
    MaxBands = 12;

    DefaultV0 = 3;
    DefaultNumEvskPts = 250;
    DefaultNumEgvsV0Pts = 20;
    DefaultNBands = 5;
    DefaultNPltBandMin = 1;
    DefaultNPltBandMax = 5;
    DefaultNumerovPts = 100;
    DefaultHistoryLevel = 6;
    DefaultWaveFnHistLevel = 6;

    DefaultVpMin = 0.05;
    DefaultVpMax = 200;

  Procedure Credits;
    Var
      helpScr: HelpScrType;
  Begin
    HelpScr[1] := '                                                 ';
    HelpScr[2] := '    Energy BANDS in an infinite 1-D Lattice ';
    HelpScr[3] := '                                                 ';
    HelpScr[4] := '                  Written by                                   ';
    HelpScr[5] := '                 Roger Rollins ';
    HelpScr[6] := '        Dept. of Physics and Astronomy, ';
    HelpScr[7] := '                Ohio University         ';
    HelpScr[8] := '                Athens, OH 45701         ';
    HelpScr[9] := '                                                 ';
    HelpScr[10] := '       (C) 1995 John Wiley and Sons, Inc.        ';
    HelpScr[11] := '                                                 ';
    HelpScr[12] := '                                                 ';
    HelpScr[13] := '    The energy levels and wavefn. are calc. for  ';
    HelpScr[14] := ' an electron moving in a 1-D symmetric V(x)=V(-x)';
    HelpScr[15] := ' periodic potential of arbitrary shape and of    ';
    HelpScr[16] := ' strength Vo.  The method used is based on an    ';
    HelpScr[17] := ' exact, non-perturbative approach so that the    ';
    HelpScr[18] := ' energy dispersion curves and band gaps can be   ';
    HelpScr[19] := ' obtained for arbitrarily large Vo.  The         ';
    HelpScr[20] := ' variation of the band gaps with Vo is calcu-    ';
    HelpScr[21] := ' lated and can be compared with the two opposite ';
    HelpScr[22] := ' limits of very weak Vo (perturbation method)    ';
    HelpScr[23] := ' and very strong Vo (Infinite Vo limit).         ';
    HelpScr[24] := '                                                 ';
    HelpScr[25] := ' Press any Key or Click Mouse to Continue...     ';
    help(helpScr);
  End;  {Credits}

  Procedure GenIntro;
    Var
      helpScr: HelpScrType;
  Begin
    HelpScr[1] := 'ENERGY BANDS, GAPS, AND WAVEFNS. IN A 1-D CRYSTAL';
    HelpScr[2] := '                                                 ';
    HelpScr[3] := '     The electron energy dispersion curves and   ';
    HelpScr[4] := '  wavefunctions are calculated for up to 12 bands';
    HelpScr[5] := '  in the 1-D crystal.  The crystal potential can ';
    HelpScr[6] := '  be very large.                                 ';
    HelpScr[7] := '     The method used is outlined in Ashcroft and ';
    HelpScr[8] := '  Mermin, "Solid State Physics", Holt, Rinehart  ';
    HelpScr[9] := '  and Winston, 1976, Chapter 8, Ex. 1, p 146-149.';
    HelpScr[10] := '  Some interesting suggestions:                  ';
    HelpScr[11] := '    1. Note the relationship between the three   ';
    HelpScr[12] := '       representations of E(k): Extended Zone,   ';
    HelpScr[13] := '       Reduced Zone, and Repeated Zone Schemes.  ';
    HelpScr[14] := '    2. Note how the Wavefunction shapes depend on';
    HelpScr[15] := '       reduced k vector and band Number.  Compare';
    HelpScr[16] := '       wavefunctions at the upper and lower edges';
    HelpScr[17] := '       of neighboring bands.                     ';
    HelpScr[18] := '    3. Note that the shape of the upper bands    ';
    HelpScr[19] := '       at high Vo is similar to lower bands at   ';
    HelpScr[20] := '       low Vo--a qualitative justification for   ';
    HelpScr[21] := '       using an effective potential.             ';
    HelpScr[22] := '    4. Note the band-gaps do NOT always vary     ';
    HelpScr[23] := '       uniformly with Vo! Notice results for the ';
    HelpScr[24] := '       square well potential ("Egap vs V0" opt.) ';
    HelpScr[25] := '       shows Egaps occasionally dip to near zero!';
    help(helpScr);
  End; {genIntro}

  Procedure HelpTopMenu;
    Var
      helpScr: HelpScrType;
  Begin
    HelpScr[1] := 'ENERGY BANDS, GAPS, AND WAVEFNS. IN A 1-D CRYSTAL';
    HelpScr[2] := '                                                 ';
    HelpScr[3] := '  Top Menu Help Screen:                          ';
    HelpScr[4] := '   File:       Exit the program or find general  ';
    HelpScr[5] := '               information about both the CUPS   ';
    HelpScr[6] := '               project and this program.         ';
    HelpScr[7] := '   Change V0:  Enter new value for the strength  ';
    HelpScr[8] := '               of the crystal potential.         ';
    HelpScr[9] := '   Lat. Pot.:  Select a lattice potential V(x).  ';
    HelpScr[10] := '   Plot-how:   Select Extended, Reduced, or Re-  ';
    HelpScr[11] := '               peated Zone Schemes for E(k) Plots';
    HelpScr[12] := '   Plot-What:  -Select the number of energy bands';
    HelpScr[13] := '               calc. and what bands are displayed';
    HelpScr[14] := '               -Toggle the display (in thick red)';
    HelpScr[15] := '               of the energy levels for an el. in';
    HelpScr[16] := '               a single infinite isolated well.  ';
    HelpScr[17] := '               -Set the number of previously calc.';
    HelpScr[18] := '               E(k) data to display.             ';
    HelpScr[19] := '               -Select Wave fn. for display.     ';
    HelpScr[20] := '   Numerics:   Enter parameters affecting the    ';
    HelpScr[21] := '               numerical precision.  Toggle the  ';
    HelpScr[22] := '               display of intermediate results.  ';
    HelpScr[23] := '   Egap vs V0: Calculate and display a log-log   ';
    HelpScr[24] := '               plot showing the variation of the ';
    HelpScr[25] := '               energy gaps with V0.              ';
    help(helpScr);
  End; {HelpTopMenu}

  Procedure HelpHotkey;
    Var
      helpScr: HelpScrType;
  Begin
    HelpScr[1] := 'ENERGY BANDS, GAPS, AND WAVEFNS. IN A 1-D CRYSTAL';
    HelpScr[2] := '                                                 ';
    HelpScr[3] := '  Display Information:                           ';
    HelpScr[4] := '      The large plot on the left of the screen   ';
    HelpScr[5] := '  shows the Electron Energy E vs. wave vector k  ';
    HelpScr[6] := '  for electrons in a lattice with the potential  ';
    HelpScr[7] := '  V(x) shown in the plot on the upper-right.     ';
    HelpScr[8] := '      The WaveFn is chosen by clicking the mouse ';
    HelpScr[9] := '  at the desired state on the E(k) plot or from  ';
    HelpScr[10] := '  the Plot-What at the top menu.                 ';
    HelpScr[11] := '      The chosen wavefunction is displayed in a  ';
    HelpScr[12] := '  plot on the lower right.                       ';
    HelpScr[13] := '      The recently calculated E(k) and Wavefns.  ';
    HelpScr[14] := '  are redisplayed in a different color for easy  ';
    HelpScr[15] := '  comparison.                                    ';
    HelpScr[16] := '  Hot-key Information:                           ';
    HelpScr[17] := '     F1- Help:       Display this help screen.   ';
    HelpScr[18] := '     F3- Halve V0:   Change value of V0 to V0/2  ';
    HelpScr[19] := '     F4- Double V0:  Change value of V0 to 2*V0  ';
    HelpScr[20] := '     F7- Clr Oldest: Clear oldest E(k) curve from';
    HelpScr[21] := '                     the display.                ';
    HelpScr[22] := '     F8- Clear All:  Clear all previously calc.  ';
    HelpScr[23] := '                     E(k) curves from the display';
    HelpScr[24] := '     F10- Menu:      Activate the Top Menu.      ';
    HelpScr[25] := '                     Select "File" for more help.';
    help(helpScr);
  End;  {HelpHotKey}

  Type
    ArrayofPotPrms = Array[1..5] Of REAL;
    ArrayofPotPrmLabels = Array[1..5] Of String;

    BandEdgeArray = Array[0..20] Of REAL;
    BandGapDVectors = Array[0..20] Of DVector;

    DataRecord = Record
        PID: INTEGER;
        V0s: REAL;
        VPotPrms: ArrayofPotPrms;
        PltColor: INTEGER;
        Ebots: BandEdgeArray;
        Etops: BandEdgeArray;
        WFnFlg: BOOLEAN;
        Es: DVector;
        ks: DVector;
      End;
    ArrayOfDataRecord = Array[0..Max_History_Level] Of DataRecord;

    WaveFnRecord = Record
        PID: INTEGER;
        color: INTEGER;
        E: REAL;
        kact: REAL;
        P: DVector;
        Re: DVector;
        Im: DVector;
      End;
    ArrayofWaveFns = Array[0..Max_WaveFnHistoryLevel] Of WaveFnRecord;

    TMySliders = object(TSliders)
      Procedure Init;
      Procedure Done;
      PROCEDURE Create(num : integer; ValMin, ValMax, VInitial,
                posX, posY, size : real; decimals: integer;
                Low,High,Name : string; vertical : boolean);
      PROCEDURE Delete(num : integer);
      PROCEDURE Erase(num, color : integer);
   private
      NumUsed : array[1..10] of boolean;
   END;

  Var
    waitslash: String[4];
    bandchar: String[15];
    MinNumerovPts: INTEGER;
    MaxNumerovPts: INTEGER;
    NumVInfPts: INTEGER;
    NumVPerturbPts: INTEGER;

    Qtol: REAL;
    ColorIndex: INTEGER;
    WFnkred: REAL;
    WFnkBand: INTEGER;

    waitcounter: INTEGER;
    waitslash_x: INTEGER;
    waitslash_y: INTEGER;

    GraphBoxColor: INTEGER;   {Axes and Legend color.}
    AxisLabelColor: INTEGER;
    EColor: INTEGER;
    VColor: INTEGER;
    FreeEColor: INTEGER;
    InfV0Color: INTEGER;{When XORed with Gray this becomes lightred}
    GKPFnColor: INTEGER;
    PsiSqColor: INTEGER;
    EColTable: Array[0..5] Of INTEGER;

    Enable_InfV0_Approx: BOOLEAN;
    WaveFlg: BOOLEAN;
    HistoryFLg: BOOLEAN;

    tmp1, tmp2, tmp3: REAL;
    V0, Qmax, delQ, Q, Vpmin, Vpmax, MaximumV0: REAL;
    EScaleMin, EScaleMax: REAL;

    Abs_t, Delta, Abs_r, Deltar: REAL;
    x, x0, x1, x2, Ai, yRi, yIi, h, hsq, delk: REAL;
    yR1, yR2, yI1, yI2: REAL;

    Numerov_Pts, Num_E_Pts, Nbands, Num_Pts_per_band: INTEGER;
    ZonePlot, PltBandMin, PltBandMax, PotIDNo: INTEGER;
    NumEgvsV0pts, ibpmin, ibpmax: INTEGER;
    itmp1, itmp2, itmp3: INTEGER;
    N_Saved_Data: INTEGER;
    N_Saved_Wavefns, ixlabel: INTEGER;
    History_Level: INTEGER;
    WaveFnHistLevel: INTEGER;
    i, iter: INTEGER;
    WFNvp,Evskvp: INTEGER;
    WFnPltType: INTEGER;

    GKPFnFlg, WrtFlg, QuitFlg, CalcFlg, InfV0Flg, MFlg, CSFlg: BOOLEAN;
    NeedNewEgvsV0, EgvsV0Flg, FirstTimeFlg: BOOLEAN;
    New_History_Record_Complete: BOOLEAN;
    sflag: boolean;

    ScreenEscFlag: BOOLEAN;

    Ebot, Etop, InfV0Energy, ReducedVcoeff: BandEdgeArray;
    logEgap, logInfV0Egap, logPerturbEgap: BandGapDVectors;
    logV0, logInfV0, logPerturbV0: DVector;
    k, E, Efree, Kfree: DVector;
    yR, yI, A, VB: DVector;
    Data: ArrayOfDataRecord;
    WaveFnData: ArrayOfWaveFns;
    WFx: DVector;
    Indx: Array[1..Max_WaveFnHistoryLevel] Of INTEGER;
    VpotLabel: String;
    VPotPrm, VPotPrmMin, VPotPrmMax: ArrayofPotPrms;
    VPotPrmLabel: ArrayofPotPrmLabels;
    NumVPotPrms: INTEGER;

    key: BYTE;
    BandMenu : TMenu;
    BandHotKeys1 : THotKeys;
    VPotSliders: TMySliders;

PROCEDURE TMySliders.init;
var i : integer;
begin
   TSliders.Init;
   for i := 1 to 10 do numUsed[i] := false;
end;

PROCEDURE TMySliders.done;
var i : integer;
begin
   TSliders.done;
   for i := 1 to 10 do numUsed[i] := false;
end;

PROCEDURE TMySliders.Erase(num, color : integer);
begin
   if numUsed[num] then TSliders.Erase(num,color);
end;

PROCEDURE TMySliders.Create(num : integer; ValMin, ValMax, VInitial,
          posX, posY, size : real; decimals: integer;
          Low,High,Name : string; vertical : boolean);
begin
   TSliders.Create(num,ValMin,ValMax,VInitial,
      posX, posY, size, decimals, Low,High,Name, vertical);
   numUsed[num] := true;
end;

PROCEDURE TMySliders.Delete(num : integer);
begin
   if numUsed[num] then TSliders.Delete(num);
   numUsed[num] := false;
end;


  Procedure SetVPotLabel (PotID: INTEGER);
  FORWARD;
  Procedure SetVPotSliders;
  FORWARD;
  Procedure DrawVPot (V0: REAL);
  FORWARD;

  Procedure InitParams;
  Begin
    MinNumerovPts := 25;
    MaxNumerovPts := 500;
    NumVInfPts := 3;
    NumVPerturbPts := 7;

    Evskvp := 1;
    WFNvp := 6;
    WFnPltType:= 0;    {0 => |Psi|^2; 1 => RePsi; 2 => ImPsi}

    Qtol := 10e-5;
    ColorIndex := 1;
    WFnkred := 0.0;
    WFnkBand := 2;

    waitcounter := 0;
    waitslash_x := 0;
    waitslash_y := 0;

    GraphBoxColor := LightGreen;   {Axes and Legend color.}
    AxisLabelColor := Whitte;
    EColor := Whitte;
    VColor := Whitte;
    FreeEColor := LightGray;
    InfV0Color := Red;{When XORed with Gray this becomes lightred}
    GKPFnColor := Whitte;
    PsiSqColor := Whitte;

    EColTable[0] := LightBlue;
    EColTable[1] := LightGreen;
    EColTable[2] := LightCyan;
    EColTable[3] := LightRed;
    EColTable[4] := LightMagenta;
    EColTable[5] := Yellow;

    Enable_InfV0_Approx := TRUE;
    WaveFlg := FALSE;
    HistoryFLg := TRUE;
  End;  {InitVars}


{**********************************************************************}
{********            Lattice Potential Functions              *********}
{**********************************************************************}
{--The following two procedures must be modified by the user to insert}
{   a User Defined Lattice Potential.                                 }
{------User Defined Potential Function goes here-----------------}
{---The Square Well Potential has been inserted as an example----}

{ VPotPrm[1] is the width of the well in units of a}
  Const
    NUserParam = 1; {Numbar of parameters actually used. 0<n<=5}
    UserParamLabel1 = 'well width';
    UserParam1Max = 0.98;  {Must specify a maximum allowed value for}
    UserParam1Min = 0.05;  {each parameter}
    UserMaxV0 = 5000;      {Maximum value of V0 for which the numerical calc.}
                           {will still work.}
  Var
    UserParam1: REAL;
    UserParam2: REAL;
    UserParam3: REAL;
    UserParam4: REAL;
    UserParam5: REAL;

  Procedure InitUserPotParams;
  Begin
    UserParam1 := 0.9; {the width of the well is initialized here}
    UserParam2 := 0.0;
    UserParam3 := 0.0;
    UserParam4 := 0.0;
    UserParam5 := 0.0;
  End;

  Function UserDefinedPotential (x, V0: real;
                  VPrm: ArrayofPotPrms): REAL;
    Var
      hw: REAL;
  Begin
    hw := VPrm[1] / 2.0;{half width of the well in units of lattice constant a}
    If (Abs(x) <= hw) Then
      UserDefinedPotential := 0.0
    Else
      UserDefinedPotential := V0;
  End;

{-----The User defined initialization procedure goes here----------}
  Procedure InitUserDefPot (Var V0, UserParam1: REAL);
    Var
      ParmScreen: TInputScreen;

  Begin
    ScreenEscFlag := FALSE;
    NumVPotPrms := NUserParam;
    MaximumV0:= UserMaxV0;
    VPotPrm[1] := UserParam1;
    VPotPrmLabel[1] := UserParamLabel1;
    VPotPrmMin[1] := UserParam1Min;
    VPotPrmMax[1] := UserParam1Max;
{MAC}
{    new(ParmScreen);}
{CAM}
    With ParmScreen Do
    Begin
      Init;
      DefineInputPort(0.05, 0.45, 0.60, 0.95);
      LoadLine('User Defined Sq. Well Pot.');
      LoadLine('');
      LoadLine(' Depth of Well: ');
      LoadLine('    V0 = {     } (h^2/8ma^2)');
      LoadLine('');
      LoadLine(' Width = {     } (fract. of a)');
      LoadLine('');
      LoadLine(' [  Ok  ] [ Help ] [Cancel]');
      SetNumber(1, V0);
      SetNumber(2, UserParam1);
      SetHelpFile('Bands.hlp', 'UserDefinedPotential Menu Help');
      AcceptScreen;
      If Canceled Then
      Begin
        ScreenEscFlag := TRUE;
        CalcFlg := FALSE;
      End
      Else
      Begin
        Enable_InfV0_Approx := FALSE;
        V0 := Abs(GetNumber(1));
        If V0 < 0.01 Then
          V0 := 0.01
        Else If V0 > MaximumV0 Then
          V0 := MaximumV0;
        UserParam1 := Abs(GetNumber(2));
        If UserParam1 < VPotPrmMin[1] Then
          UserParam1 := VPotPrmMin[1];
        If UserParam1 > VPotPrmMax[1] Then
          UserParam1 := VPotPrmMax[1];
        PotIDNo := 6;
         {All values of UserParamN should be updated by this point.}
         {Now the general variable array VPotPrm is updated         }

        VPotPrm[1] := UserParam1;
        VPotPrm[2] := UserParam2;
        VPotPrm[3] := UserParam3;
        VPotPrm[4] := UserParam4;
        VPotPrm[5] := UserParam5;

        SetVPotLabel(PotIDNo);
      End; {IF Canceled ... ELSE}
      done;
    End; {WITH}
{MAC}
{    dispose(ParmScreen);}
{CAM}
  End;
{----End of User Defined Potential Function Code --------------------}
{--------------------------------------------------------------------}

  Const
    NSqWellPrms = 1;
    SqWellMaxV0 = 5000;
    SqWellWidthLabel = 'well width';
    bsw = 1.0E-6;  {a steepness parameter for the square well}
                   {Note: a finite steepness is included to avoid}
                   {      strange numerical errors due to roundoff}
  Var
    SqWellWidth: REAL;

  Function SqWellPot (x, V0: REAL;
                  VPrm: ArrayofPotPrms): REAL;
    Var
      hw: REAL;
  Begin
    hw := VPrm[1] / 2.0;
    IF (Abs(x)<= (hw-bsw/2)) THEN
      SqWellPot:= 0.0
    ELSE
      BEGIN
        IF ((Abs(x)>(hw-bsw/2)) AND (Abs(x)<=(hw+bsw/2))) Then
          SqWellPot:= (Abs(x)-(hw-bsw/2))/bsw
        Else
          SqWellPot:= V0;
      END;
{Alternative form for "smooth" square well potential:                }
{    If Abs(x) > 0.5 Then                                            }
{      SqWellPot := 0                                                }
{    Else                                                            }
{      SqWellPot := V0 * (1 - 1 / (1 + exp((Abs(x) - hw) / 1.0E-4)));}
  End;

  Procedure InfV0SqWell (V0, SqWellWidth: REAL;
                  band: INTEGER;
                  Var InfV0Energy: BandEdgeArray);
  Begin
    InfV0Energy[band] := Band * Band / Sqr(SqWellWidth);
  End;

  Procedure InitSqWellPot (Var V0, SqWellWidth: REAL);
    Var
      ParmScreen: TInputScreen;
      i: integer;
  Begin
    NumVPotPrms := NSqWellPrms;
    MaximumV0 := SqWellMaxV0;
    VPotPrmLabel[1] := SqWellWidthLabel;
    VPotPrm[1] := SqWellWidth;
    VPotPrmMin[1] := 0.05;
    VPotPrmMax[1] := 0.98;
    ScreenEscFlag := FALSE;
{MAC}
{    new(ParmScreen);}
{CAM}
    With ParmScreen Do
    Begin
      Init;
      DefineInputPort(0.05, 0.47, 0.60, 0.95);
      LoadLine('Square Well Potential');
      LoadLine('');
      LoadLine(' Depth of Well: ');
      LoadLine('    V0 = {      } (h^2/8ma^2)');
      LoadLine('');
      LoadLine(' Width = {      } (fract. of a)');
      LoadLine('');
      LoadLine(' [  Ok  ] [ Help ] [Cancel]');
      SetNumber(1, V0);
      SetNumber(2, SqWellWidth);
      SetHelpFile('Bands.hlp', 'InitSqWellPot Menu Help');
      AcceptScreen;
      If Canceled Then
      Begin
        ScreenEscFlag := TRUE;
        CalcFlg := FALSE;
      End
      Else
      Begin
        Enable_InfV0_Approx := TRUE;
        V0 := Abs(GetNumber(1));
        If V0 < 0.01 Then
          V0 := 0.01
        Else If V0 > MaximumV0 Then
          V0 := MaximumV0;
        SqWellWidth := Abs(GetNumber(2));
        If SqWellWidth < VPotPrmMin[1] Then
          SqWellWidth := VPotPrmMin[1];
        If SqWellWidth > VPotPrmMax[1] Then
          SqWellWidth := VPotPrmMax[1];
        PotIDNo := 1;
        VPotPrm[1] := SqWellWidth;
        For i := 2 To 5 Do
          VPotPrm[i] := 0.0;  {These Var. are not used so just set to 0.0}
        SetVPotLabel(PotIDNo);
      End; {IF Canceled ... ELSE}
      done;
    End; {WITH}
{MAC}
{    dispose(ParmScreen);}
{CAM}
  End; {InitSqWellPot }


  Procedure InitKronigPenneyPot (Var V0, SqWellWidth: REAL);
    Var
      ParmScreen: TInputScreen;
      i: integer;
  Begin
    NumVPotPrms := NSqWellPrms;
    maximumV0 := SqWellMaxV0;
    VPotPrmLabel[1] := SqWellWidthLabel;
    VPotPrm[1] := SqWellWidth;
    VPotPrmMin[1] := 0.05;
    VPotPrmMax[1] := 0.98;
    ScreenEscFlag := FALSE;
{MAC}
{    new(ParmScreen);}
{CAM}
    With ParmScreen Do
    Begin
      Init;
      DefineInputPort(0.05, 0.48, 0.60, 0.95);
      LoadLine('Kronig-Penney (Sq.Well) Potential');
      LoadLine('(Partial analytic solution used)');
      LoadLine(' Depth of Well: ');
      LoadLine('    V0 = {     } (h^2/8ma^2)');
      LoadLine('');
      LoadLine(' Width = {     } (fract. of a)');
      LoadLine('');
      LoadLine(' [  Ok  ] [ Help ] [Cancel]');
      SetNumber(1, V0);
      SetNumber(2, SqWellWidth);
      SetHelpFile('Bands.hlp', 'InitSqWellPot Menu Help');
      AcceptScreen;
      If Canceled Then
      Begin
        ScreenEscFlag := TRUE;
        CalcFlg := FALSE;
      End
      Else
      Begin
        Enable_InfV0_Approx := TRUE;
        V0 := Abs(GetNumber(1));
        If V0 < 0.01 Then
          V0 := 0.01
        Else If V0 > MaximumV0 Then
          V0 := MaximumV0;
        SqWellWidth := Abs(GetNumber(2));
        If SqWellWidth < VPotPrmMin[1] Then
          SqWellWidth := VPotPrmMin[1];
        If SqWellWidth > VPotPrmMax[1] Then
          SqWellWidth := VPotPrmMax[1];
        PotIDNo := 0;
        VPotPrm[1] := SqWellWidth;
        For i := 2 To 5 Do
          VPotPrm[i] := 0.0;  {These Var. are not used so just set to 0.0}
        SetVPotLabel(PotIDNo);
      End; {IF Canceled ... ELSE}
      done;
    End; {WITH}
{MAC}
{    dispose(parmScreen);}
{CAM}
  End; {InitSqWellPot }

  Function CosinePot (x, V0: REAL;
                  VPrm: ArrayofPotPrms): REAL;
  Begin
    CosinePot := V0 * 0.5 * (1.0 - Cos(2 * pi * x));
  End;

  Procedure InfV0CPot (V0: REAL;
                  band: INTEGER;
                  Var InfV0Energy: BandEdgeArray);
  Begin
    InfV0Energy[band] := (Band - 1 + 0.5) * 2.0 * Sqrt(V0);
  End;

  Procedure InitCosinePot (Var V0: REAL);
    Var
      i: integer;

  Begin
    NumVPotPrms := 0;
    MaximumV0 := 5000;
    PotIDNo := 2;
    For i := 1 To 5 Do
      VPotPrm[i] := 0.0;  {These Var. are not used so just set to 0.0}
    SetVPotLabel(PotIDNo);
    Enable_InfV0_Approx := TRUE;
  End;

  Const
    BLabel = 'B';
    BMax = 1.0;
    BMin = 0.0;

  Var
    B: REAL;

  Function CosinePot2 (x, V0: REAL;
                  VPrm: ArrayofPotPrms): REAL;
  Begin
    CosinePot2 := V0 * 0.5 * ((1 - VPrm[1]) * (1.0 - Cos(4 * pi * x)) + VPrm[1] * (1.0 - Cos(2 * pi * x)));
  End;

  Procedure InfV0CPot2 (V0: REAL;
                  band: INTEGER;
                  Var InfV0Energy: BandEdgeArray);
  Begin
    InfV0Energy[band] := (Band - 1 + 0.5) * 4.0 * Sqrt(V0 * (1 - (3 * B / 4)));
  End;

  Procedure InitCosinePot2 (Var V0, B: REAL);
    Var
      ParmScreen: TInputScreen;
      i: integer;

  Begin
    ScreenEscFlag := FALSE;
    NumVPotPrms := 1;
    MaximumV0 := 1000;
    VPotPrm[1] := B;
    VPotPrmLabel[1] := BLabel;
    VPotPrmMin[1] := 0.0;
    VPotPrmMax[1] := 1.0;
{MAC
    new(ParmScreen);
{CAM}
    With ParmScreen Do
    Begin
      Init;
      DefineInputPort(0.05, 0.45, 0.60, 0.95);
      LoadLine(' Sum of two Cosines Terms:');
      LoadLine('     B V0(1-Cos(2pi x/a))/2');
      LoadLine('  (1-B)V0(1-Cos(4pi x/a))/2');
      LoadLine(' Depth of Well:');
      LoadLine('    V0= {     } (h^2/8ma^2)');
      LoadLine(' Relative weight: ');
      LoadLine('     B= {     } 0 <= B <= 1');
      LoadLine(' [  Ok  ] [ Help ] [Cancel]');
      SetNumber(1, V0);
      SetNumber(2, B);
      SetHelpFile('Bands.hlp', 'InitCosinePot2 Menu Help');
      AcceptScreen;
      If Canceled Then
      Begin
        ScreenEscFlag := TRUE;
        CalcFlg := FALSE;
      End
      Else
      Begin
        V0 := Abs(GetNumber(1));
        If V0 < 0.01 Then
          V0 := 0.01
        Else If V0 > MaximumV0 Then
          V0 := MaximumV0;
        B := Abs(GetNumber(2));
        If B < VPotPrmMin[1] Then
          B := VPotPrmMin[1];
        If B > VPotPrmMax[1] Then
          B := VPotPrmMax[1];
        PotIDNo := 3;
        VPotPrm[1] := B;
        For i := 2 To 5 Do
          VPotPrm[i] := 0.0;  {These Var. are not used so just set to 0.0}
        SetVPotLabel(PotIDNo);

        Enable_InfV0_Approx := TRUE;
      End;
      done;
    End; {WITH}
{MAC}
{    dispose(ParmScreen);}
{CAM}
  End;

  Function HarmonicOsc (x, V0: REAL;
                  VPrm: ArrayofPotPrms): REAL;
  Begin
    HarmonicOsc := V0 * 4.0 * x * x;
  End;

  Procedure InfV0HmOsc (V0: REAL;
                  band: INTEGER;
                  Var InfV0Energy: BandEdgeArray);
  Begin
    InfV0Energy[band] := (Band - 1 + 0.5) * 4.0 * Sqrt(V0) / pi;
  End;

  Procedure InitHarmOscPot (Var V0: REAL);
    Var
      i: integer;

  Begin
    NumVPotPrms := 0;
    MaximumV0 := 5000;
    PotIDNo := 4;
    For i := 1 To 5 Do
      VPotPrm[i] := 0.0;  {These Var. are not used so just set to 0.0}
    SetVPotLabel(PotIDNo);
    Enable_InfV0_Approx := TRUE;
  End;

  Const
    FBWMax = 0.9;
    FBWMin = 0.00;
    FBWLabel = 'Bottom Width';

  Var
    FBW: REAL;

  Function Linear (x, V0: REAL;
                  VPrm: ArrayofPotPrms): REAL;
    Var
      hFBW: REAL;
  Begin
    hFBW := VPrm[1] / 2.0;
    If Abs(x) <= hFBW Then
      Linear := 0.0
    Else
      Linear := V0 * 2.0 * (Abs(x) - hFBW) / (1.0 - VPrm[1]);
  End;

  Procedure InitLinearPot (Var V0, FBW: REAL);
    Var
      ParmScreen: TInputScreen;
      i: integer;

  Begin
    ScreenEscFlag := FALSE;
    NumVPotPrms := 1;
    MaximumV0 := 5000;
    VPotPrm[1] := FBW;
    VPotPrmLabel[1] := FBWLabel;
    VPotPrmMin[1] := FBWMin;
    VPotPrmMax[1] := FBWMax;
{MAC}
{    new(ParmScreen);}
{CAM}
    With ParmScreen Do
    Begin
      Init;
      DefineInputPort(0.05, 0.45, 0.60, 0.95);
      LoadLine(' Linear Potential Well');
      LoadLine('');
      LoadLine(' Depth of Well: ');
      LoadLine('    V0= {     } (h^2/8ma^2)');
      LoadLine(' Width of Flat Bottom:');
      LoadLine('   FBW= {     } (fract. of a)');
      LoadLine('');
      LoadLine(' [  Ok  ] [ Help ] [Cancel]');
      SetNumber(1, V0);
      SetNumber(2, FBW);
      SetHelpFile('Bands.hlp', 'InitLinearPot Menu Help');
      AcceptScreen;
      If Canceled Then
      Begin
        ScreenEscFlag := TRUE;
        CalcFlg := FALSE;
      End
      Else
      Begin
        V0 := Abs(GetNumber(1));
        If V0 < 0.01 Then
          V0 := 0.01
        Else If V0 > MaximumV0 Then
          V0 := MaximumV0;
        FBW := Abs(GetNumber(2));
        If FBW < VPotPrmMin[1] Then
          FBW := VPotPrmMin[1];
        If FBW > VPotPrmMax[1] Then
          FBW := VPotPrmMax[1];
        PotIDNo := 5;
        VPotPrm[1] := FBW;
        For i := 2 To 5 Do
          VPotPrm[i] := 0.0;  {These Var. are not used so just set to 0.0}
        SetVPotLabel(PotIDNo);
        Enable_InfV0_Approx := FALSE;
      End;
      done;
    End; {WITH}
{MAC}
{    dispose(ParmScreen);}
{CAM}
  End;

  Function VPot (x, V0: real;
                  VPrm: ArrayofPotPrms): REAL;
  Begin
    Case PotIDNo Of
      0:
        VPot := SqWellPot(x, V0, VPrm);
      1:
        VPot := SqWellPot(x, V0, VPrm);
      2:
        VPot := CosinePot(x, V0, VPrm);
      3:
        VPot := CosinePot2(x, V0, VPrm);
      4:
        VPot := HarmonicOsc(x, V0, VPrm);
      5:
        VPot := Linear(x, V0, VPrm);
      6:
        VPot := UserDefinedPotential(x, V0, VPrm);
    End {CASE}
  End; {SetVPot}

  Procedure SetVPotLabel;{(PotID: INTEGER); FORWARD}
  Begin
    Case PotID Of
      0:
        VpotLabel := Concat('Kronig-Penney (Sq.Well),width= ', NumStr(VPotPrm[1], 4, 2), 'a');
      1:
        VpotLabel := Concat('Square Well Potential, width = ', NumStr(VPotPrm[1], 4, 2), 'a');
      2:
        VpotLabel := 'V(x) = (V0/2)[1-Cos(2pi*x/a)]';
      3:
        VpotLabel := Concat('V(x)=(1-B)[1-Cos(4x)]+B[1-Cos(2x)]; B=', NumStr(VPotPrm[1], 4, 3));
      4:
        VpotLabel := 'V(x) = Harmonic Osc. Potential Well';
      5:
        VpotLabel := Concat('V(x) = Linear Well, Flat Bottom = ', NumStr(VPotPrm[1], 4, 2), 'a');
      6:
        VpotLabel := Concat('UserDefined Sqr Well Pot., width = ', NumStr(VPotPrm[1], 4, 2), 'a');
    End {CASE}
  End; {SetVPotLabel}


{**********************************************************************}
{********                 General Utilities                   *********}
{**********************************************************************}

  Function ClickedInside (vp: integer): boolean;
  Begin
    With event Do
      With views[vp] Do
        ClickedInside := mouseClicked And (x > vx1) And (x < vx2) And (y > vy1) And (y < vy2);
  End;

  Function NextPlotColor: INTEGER;
  Begin
    NextPlotColor := EColTable[(ColorIndex Mod 6)];
    ColorIndex := ColorIndex + 1;
  End;

  Function PlotColorN (j: INTEGER): INTEGER;
    Var
      pcolor: INTEGER;
  Begin
    pcolor := (j Mod 8) + 7;
    If (pcolor < GraphBackColor) Then
      PlotColorN := pcolor
    Else
      PlotColorN := (pcolor + 1);
  End;

  Procedure InitWaitSlash (x, y: INTEGER);
  Begin
    If ((x < 0) Or (y < 0)) Then  {use default positions}
    Begin
      waitslash_x := GetMaxX Div 2;
      waitslash_y := 7;
    End
    Else
    Begin
      waitslash_x := x;
      waitslash_y := y;
    End;
    waitcounter := 2;
    waitslash := '\|/-';
  End;

  Procedure RotateWaitSlash;
  Begin
    SelectMuppetPort;
    SetColor(Blue);
    OutTextXY(waitslash_x, waitslash_y, waitslash[(waitcounter Mod 4) + 1]);
    If waitcounter < 100 Then
      inc(waitcounter)
    Else
      waitcounter := 1;
    SetColor(Whitte);
    OutTextXY(waitslash_x, waitslash_y, waitslash[(waitcounter Mod 4) + 1]);
    SelectViewPort(ViewPortNumber);
  End;

  Procedure EraseWaitSlash;
  Begin
    SelectMuppetPort;
    SetColor(Blue);
    OutTextXY(waitslash_x, waitslash_y, waitslash[(waitcounter Mod 4) + 1]);
    SelectViewPort(ViewPortNumber);
  End;

  Procedure Shift_History_Data (Var nd: INTEGER);
    Var
      i, j: INTEGER;
  Begin
    nd := nd + 1;
    If nd > History_Level + 1 Then
      nd := History_Level + 1;
    i := nd - 1;
    If ((i > 0) And New_History_Record_Complete) Then
      Repeat
{         Data[i]:=Data[i-1];}
        Data[i].PID := Data[i - 1].PID;
        Data[i].V0s := Data[i - 1].V0s;
{         FOR j:=1 TO 5 DO}
        Data[i].VPotPrms := Data[i - 1].VPotPrms;
        Data[i].PltColor := Data[i - 1].PltColor;
        Data[i].WFnFlg := Data[i - 1].WFnFlg;
        Data[i].Ebots := Data[i - 1].Ebots;
        Data[i].Etops := Data[i - 1].Etops;
        Data[i].Es.Equate(1.0, Data[i - 1].Es);
        Data[i].ks.Equate(1.0, Data[i - 1].ks);
        i := i - 1;
      Until i = 0;
    Data[0].PID := PotIDNo;
    Data[0].V0s := V0;
    For j := 1 To 5 Do
      Data[0].VPotPrms[j] := VPotPrm[j];
    Data[0].PltColor := NextPlotColor;
    Data[0].WFnFlg := FALSE;
    New_History_Record_Complete := FALSE;
  End;

  Procedure Update_History_Data;
  Begin
    With Data[0] Do
    Begin
      PID := PotIDNo;
      V0s := V0;
      VPotPrms := VPotPrm;  {array of Pot. variables}
   {Note Data[0].PltColor is not updated because already set in}
{         Shift_History_Data procedure.  The variables PID, V0s}
{         and VPotPrms were also set but reset here to insure that}
{         they are always kept with the correct E vs K data.}
      Ebots := Ebot;
      Etops := Etop;
      Es.Equate(1.0, E);
      ks.Equate(1.0, k);
      New_History_Record_Complete := TRUE;
    End {WITH Data[0] }
  End;

{**********************************************************************}
{********      General Menu and Plotting Procedures           *********}
{**********************************************************************}

  PROCEDURE SetUpBandMenu;
  BEGIN
{MAC}
{    New(BandMenu);}
{CAM}
    BandMenu.Init;
    With BandMenu Do
    Begin
      column(1, 'File');
      row(1, 1, 'About CUPS');
      row(1, 2, 'About Program');
      row(1, 3, 'More About Program');
      row(1, 4, 'Help on Top Menu');
      row(1, 5, 'Configuration');
      row(1, 6, '------------------');
{Possible future additions:}
{    row(1, 7, 'Save E vs k Data');}
{    row(1, 8, 'Save Egap vs V0 Data');}
{    row (1,9 , 'Read E vs k Data' );}
{    row (1,10, 'Read Egap vs V0 Data' );}
      row(1, 7, 'Exit Program');
      rowactivate(1,6,false);
      column(2, 'Change V0');
{MAC}
{      row(2, 1, 'Do it');}
{CAM}
      column(3, 'Lat.Pot.');
      row(3, 1, 'Kronig-Penney Analytic Soln.');
      row(3, 2, 'Rectangular Well');
      row(3, 3, 'Cos 2pi x/a');
      row(3, 4, 'Cos 2pi x/a + Cos4pi x/a');
      row(3, 5, 'Harmonic Osc.');
      row(3, 6, 'Linear Pot.');
      row(3, 7, 'User Defined Potential');
      column(4, 'Plot-how');
      row(4, 1, 'Reduced  Zone Scheme ');
      row(4, 2, 'Extended Zone Scheme');
      row(4, 3, 'Repeated Zone Scheme');
      row(4, 4, '---------------------------');
      rowactivate(4,4,false);
      row(4, 5, 'Swap Evsk with Wavefn. Plot');
      column(5, 'Plot-what');
      row(5, 1, 'No. of Bands displayed');
{MAC}
{      row(5, 2, 'Show/Erase Infinite V) approx.');}
{CAM}
{PC}
      IF (Not InfV0Flg) THEN
        row(5, 2, 'Show Infinite V0 approx.')
      ELSE
        row(5, 2, 'Erase Infinite V0 approx.');
      IF (HistoryFlg) THEN
        row(5, 3, 'Stop displaying old E vs. k ')
      ELSE
        row(5, 3, 'Begin displaying old E vs. k');
{CP}
{MAC}
{      row(5, 3, 'Begin/Stop displaying old E vs k');}
{CAM}
      row(5, 4, '-------------------------------');
      rowactivate(5,4,false);
      row(5, 5, 'Show |Psi|^2 for chosen states');
      row(5, 6, 'Show Re{Psi} for chosen states');
      row(5, 7, 'Show Im{Psi} for chosen states');
      row(5, 8, '-------------------------------');
      rowactivate(5,8,false);
      row(5, 9, 'Show Multi-cell Plot of Psi');
      row(5, 10, 'Select State for Wavefn. plot');
      column(6, 'Numerics');
{MAC}
{      row(6, 1, 'Do it');}
{CAM}
      column(7, 'Egap vs V0');
{MAC}
{      row(7, 1, 'Do it');}
{CAM}
    END;
{   BandMenu.Display;}
  End;

  Procedure UpDateBandMenu;
  Begin
{PC}
    With BandMenu Do
    Begin
      If (Not InfV0Flg) Then
        row(5, 2, 'Show Infinite V0 approx.')
      Else
        row(5, 2, 'Erase Infinite V0 approx.');
      If (HistoryFlg) Then
        row(5, 3, 'Stop displaying old E vs. k ')
      Else
        row(5, 3, 'Begin displaying old E vs. k');
    End;
{CP}
    If Enable_InfV0_Approx Then
      BandMenu.RowActivate(5, 2, TRUE)
    Else
      BandMenu.RowActivate(5, 2, FALSE);
  End;


  Procedure SetUpHotKeys1;
  Begin
{MAC}
{    new(BandHotKeys1);}
{CAM}
    With BandHotKeys1 Do
    Begin
      Init(6);
      key[1] := 'F1-Help';
      key[2] := 'F3-Halve V0';
      key[3] := 'F4-Dbl V0';
      key[4] := 'F7-Clr Oldest';
      key[5] := 'F8-Clear All';
      key[6] := 'F10-Menu';
    End;
  End;

  Procedure SetVPotSliders;
    Var
      LoLabel, HiLabel: String;
  Begin
    With VPotSliders Do
    Begin
{      Done;}
{      Init;}
{      Create(1,-2.0,4.0,ln(V0)/ln(10),0.92,0.07,0.48,2,}
{             '0.01','10,000','log Vo',TRUE);}
{ }
      Delete(2);
      LoLabel := NumStr(VPotPrmMin[1], 4, 2);
      HiLabel := NumStr(VPotPrmMax[1], 4, 2);
      Create(2, VPotPrmMin[1], VPotPrmMax[1], VPotPrm[1], 0.63, 0.53, 0.98, 2, LoLabel, HiLabel, VPotPrmLabel[1], FALSE);

{      DrawAll;}
    End;
  End;


  Procedure DrawVPot (V0: REAL);
    Const
      npts = 201;
    Var
      viewPort: ViewPortType;
      ix1, iy1, ix2, iy2, ix, i, j, ilo: integer;
      image: TImage;
      xa, v: DVector;
      x0, x, h, V0scl: REAL;
      tPotIDNo: INTEGER;
      tvPotPrm: ArrayofPotPrms;
  Begin
{MAC}
{    New(xa);}
{CAM}
    xa.init(npts);
{MAC}
{    New(v);}
{CAM}
    v.init(npts);
    x0 := -0.5;
    h := 1 / (npts - 1);
    For ix := 1 To npts Do
    Begin
      x := x0 + (ix - 1) * h;
      xa.put(ix, x);
    End;
    V0scl := V0;
    If HistoryFlg Then
      For i := 1 To N_Saved_Data - 1 Do
        If Data[i].V0s > V0scl Then
          V0scl := Data[i].V0s;
    DefineScale(5, -0.5, 0.5, -0.1 * V0scl, 1.1 * V0scl);
    SelectScale(5);
    SelectViewPort(5);
    SetColor(GraphBoxColor);
    OpenViewport(5);
    Axis(0.0, 100, 0.5, TickSpace(1.1*V0scl));
    PutLabel(top, 'V(x) for one unit cell');
    SetColor(AxisLabelColor);
    PutLabel(bottom, ' x (in units of a)');
    PutLabel(left, 'V(x)');
    If HistoryFlg Then
    Begin
      tPotIDNo := PotIDNo;
      For j := 1 To 5 Do
        tvPotPrm[j] := VPotPrm[j];{Save the parameters used in potential def.}
      i := N_Saved_Data;
      If i > 0 Then
        ilo := 0
      Else
        ilo := 1;
      While i > ilo Do
      Begin
        i := i - 1;
        PotIDNo := Data[i].PID;
        For j := 1 To 5 Do
          VPotPrm[j] := Data[i].VPotPrms[j];
        For ix := 1 To npts Do
        Begin
          x := x0 + (ix - 1) * h;
          v.put(ix, VPot(x, Data[i].V0s, VPotPrm));
        End;
        SetColor(Data[i].PltColor);
        PlotDVectors(xa, v, 1, npts);
      End;
      PotIDNo := tPotIDNo;
      For j := 1 To 5 Do
        VPotPrm[j] := tVPotPrm[j];  {restore the vars used in potential def.}
    End; {IF HistoryFlg}
    For ix := 1 To npts Do
    Begin
      x := x0 + (ix - 1) * h;
      xa.put(ix, x);

      v.put(ix, Vpot(x, V0, VPotPrm));
    End;
    SetColor(VColor);
    PlotDVectors(xa, v, 1, npts);
    SelectViewport(Evskvp);
    SetColor(EColor);
    xa.free;
    v.free;
{MAC}
{    Dispose(xa);}
{    Dispose(v);}
{CAM}
    If NumVPotPrms > 0 Then
      VPotSliders.DrawAll;
  End; {DrawVPot }


  Procedure DispInfV0Energy (V0: REAL;
                  PotIDNo, ZonePlot, Bmin, Bmax, icolor: INTEGER);
    Var
      i, x1, x2, y, origcolor: INTEGER;

  Begin
    origcolor := GetColor;
    SetColor(Icolor);
    If Enable_InfV0_Approx Then
      For i := Bmin To Bmax Do
      Begin
        y := MapY(InfV0Energy[i]);
        Case ZonePlot Of
          1:
          Begin
            x1 := MapX(-1);
            x2 := MapX(+1);
          End;
          2:
          Begin
            x1 := MapX(i - 1);
            x2 := MapX(i);
          End;
          3:
          Begin
            x1 := MapX(-3);
            x2 := MapX(+3);
          End;
        End;{CASE}
        SetLineStyle(SolidLn, $FFFF, ThickWidth);
{MAC}
{        LineTP(x1, y, x2, y);}
{CAM}
{PC}
Line(x1, y, x2, y);
{CP}
        SetLineStyle(SolidLn, $FFFF, NormWidth);
      End;
    SetColor(origcolor);
  End;

  Procedure InitEkPort (vpNo,sclNo, pltbmin, pltbmax, Nbands: INTEGER;
                  EScaleMin, EScaleMax: REAL);

    Var
      Elabel,kastr: String;
      temin, temax, tdele, dnb: REAl;

    Procedure MaxMinHistoryEscale (Var ESmin, ESmax: REAL);
      Var
        ihd: INTEGER;
      Function PEmin (i: INTEGER): REAL;
      Begin
        If PltBMin = 1 Then
          PEmin := Data[i].Ebots[PltBandMin] / 2
        Else
          PEmin := (Data[i].Ebots[PltBMin] + Data[i].Etops[PltBMin - 1]) / 2;
      End; {FUNCTION PEmin}

      Function PEmax (i: INTEGER): REAL;
      Begin
        PEmax := (Data[i].Etops[PltBMax] + Data[i].Ebots[PltBMax + 1]) / 2;
      End; {FUNCTION PEmin}

    Begin  {MaxMinHistoryEscale}
      For ihd := 1 To N_Saved_Data - 1 Do
      Begin
        temin := PEmin(ihd);
        temax := PEmax(ihd);
        If temin < ESmin Then
          ESmin := temin;
        If temax > ESmax Then
          ESmax := temax;
      End;
    End; {MaxMinHistoryEscale}

  Begin {InitEkPort}
    CloseViewPort(10);
    UpDateBandMenu;
    DrawVPot(V0);
    SetColor(GraphBoxColor);
    OpenViewPort(vpNo);
    SelectViewport(vpNo);
    If HistoryFlg Then
      MaxMinHistoryEscale(EScaleMin, EScaleMax);
    If EScaleMin > 10 Then
      tEmin := round(EScaleMin - 0.5)
    Else
      tEmin := round(10 * EScaleMin - 0.5) / 10;
    If EScaleMax > 10 Then
      tEmax := round(EScaleMax + 0.5)
    Else
      tEmax := round(10 * EScaleMax + 0.5) / 10;

    DefineScale(1, -1.0, 1.0, tEmin, tEmax);
    DefineScale(2, pltbmin - 1, pltbmax, tEmin, tEmax);
    DefineScale(3, -3.0, 3.0, tEmin, tEmax);

    If (sclNo = 1) Then       {Reduced Zone Scheme}
    Begin
      SelectScale(1);
      kastr := 'ka';
      Axis(0.0, tEmin, 0.5, TickSpace(tEmax-tEmin));
    End;
    If (sclNo = 2) Then        {Extended }
    Begin
      If (pltbmin - 1) = 0 Then
        tEmin := 0.0;
      SelectScale(2);
      kastr := '|ka|';
      Axis(0.0, 0.0, 1, TickSpace(tEmax-tEmin));
    End;
    If (sclNo = 3) Then        {Repeated Zone Scheme}
    Begin
      SelectScale(3);
      kastr := 'ka';
      Axis(0.0, tEmin, 1, TickSpace(tEmax-tEmin));
    End;
    Elabel := VpotLabel;
    PutLabel(top, Elabel);
    RubOut(7, 2, 10, GraphBackColor);
    SetColor(AxisLabelColor);
    Elabel := Concat('V0 = ', NumStr(V0, 5, 1));
    Print(7, 2, Elabel);
    If Evskvp=6 Then
    Begin
      PutLabel(bottom, Concat(kastr,'/pi'));
      PutLabel(left, 'E/(h^2/8ma^2)');
    End
    Else
    Begin
      PutLabel(bottom, Concat(kastr,' (in units of pi)'));
      PutLabel(left, 'E (in units of h^2/8ma^2)');
    End;
    SetColor(EColor);
  End; {PROCEDURE InitEkPort}

  Procedure PlotEvsK (PlotColor: INTEGER;
                  PlottingOldFlg: BOOLEAN;
                  ZonePlot, BandIndxLo, BandIndxHi, Num_Pts_per_Band: INTEGER;
                  Var k, E: DVector);
    Var
      BandIndx, nppb, G, j, jk, jbot, jtop: INTEGER;
      Grlv, GrlvOffSet, Mult: INTEGER;
      Elabel: String;
      tmpk, tmpE: DVector;
  Begin
    SetColor(PlotColor);
    nppb := Num_Pts_per_Band;
    For BandIndx := BandIndxLo To BandIndxHi Do
    Begin
      jbot := (BandIndx - 1) * nppb + 1;
      jtop := BandIndx * nppb;

      If (ZonePlot = 3) Then
        GrlvOffSet := 1
      Else
        GrlvOffSet := 0;
      If ((ZonePlot = 1) Or (ZonePlot = 3)) Then   {Reduced Zone Scheme}
      Begin
{MAC}
{        New(tmpk);                           }
{CAM}
        tmpk.init(nppb); {or Repeated Zone Scheme}
{MAC}
{        New(tmpE);}
{CAM}
        tmpE.init(nppb);
        If (BandIndx Mod 2 = 0) Then
        Begin
          Grlv := BandIndx;
          mult := -1;
        End
        Else
        Begin
          Grlv := BandIndx - 1;
          Mult := 1;
        End;
        For G := -GrlvOffSet To GrlvOffset Do
        Begin
          For j := 1 To nppb Do
          Begin
            jk := jbot + (j - 1);
            tmpk.Put(j, Mult * (k.Value(jk) - (Grlv + 2 * G)));
            tmpE.Put(j, E.Value(jk));
          End;
          PlotDVectors(tmpk, tmpE, 1, nppb);
          For j := 1 To nppb Do
          Begin
            tmpk.Put(j, -tmpk.Value(j));
          End;
          PlotDVectors(tmpk, tmpE, 1, nppb);
        End;
        tmpk.Free;
        tmpE.Free;
{MAC}
{        Dispose(tmpk);}
{        Dispose(tmpE);}
{CAM}
      End;
      If (ZonePlot = 2) Then    {Extended Zone Scheme}
      Begin
        PlotDvectors(k, E, jbot, jtop);
        SetColor(FreeEColor);
        PlotDvectors(kfree, Efree, jbot, jtop);
        SetColor(PlotColor);
      End;
    End;
    If (InfV0Flg And (Not PlottingOldFlg)) Then
    Begin
      SetWriteMode(XORput);
      DispInfV0Energy(V0, PotIDNo, ZonePlot, BandIndxLo, BandIndxHi, infV0color);
      SetWriteMode(CopyPut);
    End;
    RubOut(7, 1, 10, GraphBackColor);
    Elabel := Concat('Bands ', NumStr(PltBandMin, 2, 0), ' - ', NumStr(PltBandMax, 2, 0));
    Print(7, 1, Elabel);
  End; {PlotEvsk}

  Procedure Plot_saved_E_Data (isaved, ZonePlot, BandIndxLo, BandIndxHi, Num_Pts_per_Band: INTEGER);
    Var
      id: INTEGER;
  Begin
    id := isaved;
    While id > 1 Do
    Begin
      id := id - 1;
      PlotEvsk(Data[id].PltColor, TRUE, ZonePlot, BandIndxLo, BandIndxHi, Num_Pts_per_Band, Data[id].ks, Data[id].Es);
    End;
  End; {Plot_saved_E_Data}


  Function VBarrier (x, V0: REAL;
                  VPotPrm: ArrayofPotPrms): REAL;
  Begin
    If Abs(x) > 0.5 Then
      VBarrier := 0.0
    Else If x <= 0.0 Then
      VBarrier := Vpot(x + 0.5, V0, VPotPrm)
    Else
      VBarrier := Vpot(x - 0.5, V0, VPotPrm);
  End;


  Procedure CalcInfV0Energy (V0: REAL;
                  PotIDNo, Band: INTEGER;
                  Var InfV0Energy: BandEdgeArray);
  Begin
    Case PotIDNo Of
      0:
        InfV0SqWell(V0, VPotPrm[1], Band, InfV0Energy);
      1:
        InfV0SqWell(V0, VPotPrm[1], Band, InfV0Energy);
      2:
        InfV0CPot(V0, Band, InfV0Energy);
      3:
        InfV0CPot2(V0, Band, InfV0Energy);
      4:
        InfV0HmOsc(V0, Band, InfV0Energy);
    End;
  End;


  Procedure Perturbation_Calc_Vcoeff (Var Vcoeff: BandEdgeArray);
    Const
      j = 1;
      tV0 = 1.0;
    Var
      iband: INTEGER;
      Ug: REAL;

    Function FTx (x: REAL): REAL;
    Begin
      FTx := 2.0 * Vpot(x, tV0, VPotPrm) * cos(2.0 * pi * iband * x);
    End;

    Procedure Integrate_by_Bodes_rule (xl, xu: REAL;
                    n: INTEGER;
                    Var Result: REAL);
      Var
        i, j: INTEGER;
        h: REAL;

    Begin
      h := (xu - xl) / (4 * n);
      Result := 0.0;
      i := 1;
      While (i <= (4 * n - 3)) Do
      Begin
        Result := Result + 32.0 * (FTx(xl + i * h) + FTx(xl + (i + 2) * h))
               + 12.0 * FTx(xl + (i + 1) * h) + 14.0 * FTx(xl + (i + 3) * h);
        i := i + 4;
      End;
      Result := Result + 7.0 * (FTx(xl) - FTx(xu));
      Result := ((2.0 * h) / 45.0) * Result;
    End;

  Begin
    For iband := 0 To MaxBands Do
    Begin
      Integrate_by_Bodes_rule(0, 0.5, 100, Ug);
      If Abs(Ug) < 1E-4 Then
        Ug := 1E-4;
      Vcoeff[iband] := Abs(Ug);
    End;
  End;

(*{$S One Seg }*)

  Procedure Check_Square_Barrier (Q, V0, width: REAL;
                  Var t_Abs, delta: REAL);
    Var
      vp, q1, alpha, beta: REAL;
      Z1, Z2, N, tN, tD, t: Complex;
  {Note the width is the width of the well (in units of lattice spacing a)}
{   so the width of the barrier is (1-width) in units of a.}
  Begin
    Vp := V0 * pi * pi;  { V0 is in units of h^2/(8ma^2)}
    Q1 := Q;
    If (sqr(Q1) > Vp) Then
    Begin
      alpha := sqrt(sqr(Q1) - Vp);
      tN.Re := 4.0 * Q1 * alpha * cos((alpha - Q1) * (1 - width));
      tN.Im := 4.0 * Q1 * Alpha * sin((alpha - Q1) * (1 - width));
            {tN = numerator of expression for t}
      tD.Re := sqr(Q1 + alpha) - sqr(Q1 - alpha) * cos(2 * alpha * (1 - width));
      tD.Im := -sqr(Q1 - alpha) * sin(2 * alpha * (1 - width));
            {tD = denomenator}
    End
    Else
    Begin
      If sqr(Q1) = Vp Then
      Begin
        tN.Re := cos(Q1 * (1 - width));
        tN.Im := sin(Q1 * (1 - width));
        tD.Re := 1.0;
        tD.Im := Q1 * (1 - width) / 2;
      End
      Else
      Begin
        beta := sqrt(Vp - Q1 * Q1);
        tN.Re := 4.0 * Q1 * beta * sin(Q1 * (1 - width));
        tN.Im := 4.0 * Q * beta * cos(Q1 * (1 - width));
        tD.Re := (sqr(Q) - sqr(beta)) * 2.0 * sinh(beta * (1 - width));
        tD.Im := 4.0 * beta * Q1 * cosh(beta * (1 - width));
      End;
    End;
    CxOp(tN, '/', tD, t);
    t_Abs := CAbs(t);
    delta := Arg(t);

{  WRITELN('tsq =', tsq:15:10, '  Delta =', delta:15:10,' ttheory=',tth:15:10);}
{  WRITELN('rsq =', rsq:15:10, '  Deltar =', deltar:15:10,' rtheory=',rth:15:10);}
{  Writeln('Error in transmission coef. sqr =', (tsq-tth):15:10);}
{  Writeln('Error in reflection coef. sqr =', (rsq-rth):15:10);}
{  Writeln(' |t|^2 + |r|^2 =',(tsq+rsq):15:10);}
{  Writeln('(DeltaR - Delta)*2/pi =', ((DeltaR - Delta)*2.0/pi):15:10);}
{ }
  End; {Check_Square_Barrier}


{**********************************************************************}
{********  Procedures below are for the numerical             *********}
{********  Calculation of 1-D energy bands, E(k).             *********}
{********   The Numerical Method is based on an exact         *********}
{********   relation between E(k) and the the transmission    *********}
{********   and phase shift of a plane wave scattering from   *********}
{********   the potential of a single unit cell.              *********}
{**********************************************************************}

  Procedure Numerov_Init_Once;
    Var
      ii: INTEGER;
      x: REAL;
  Begin
    h := 1 / (Numerov_Pts - 4);
    hsq := sqr(h);
    x0 := (1 / 2) + h;
    x1 := x0;
    x2 := x0 - h;
    For ii:=1 to Numerov_Pts Do
    Begin
      x := -1 / 2 + (ii - 3) * h;
      VB.Put(ii,VBarrier(x, V0 * pi * pi, VPotPrm));
    End;
  End; {Numerov_Init_Once}

  Var
    tsq, rsq, RePsi, DRePsi, ImPsi, DImPsi: REAL;
  Var
    DRePsibyQ, DImPsibyQ: REAL;

  Procedure Calc_t_and_delta (Q, V0: REAL;
                  VPotPrm: ArrayofPotPrms;
                  Var Abs_t, Delta: REAL;
                  Var yR, yI, A: DVector);
    Var
      xtmp, ytmp: REAL;
      i_at_minus_1half: INTEGER;

(*    Note:  Integrating backwards from x=1/2 to x=-1/2}
{       yR  (Real part of wave fn)}
{       yI  (Imaginary part of wave fn)}
{}
{     d^2y/dx^2 = (VBarrier - Q*Q) y, where x in units of a}
{                                           VBar. in units of hbar^2/2ma^2}
{                                           Q*Q = E in units of hbar^2/2ma^2}
{  Since VBarrier = 0 if x < -1/2 or > 1/2:}
{  (dividing both sides of equation by the transmission coef. t)}
{     yR(x)/t = cos(Qx),     x >= 1/2;}
{     yI(x)/t = sin(Qx),     x >= 1/2.}
{}
{  yR(x)/t = (1/|t|)cos(Qx - delta) + (|r|/|t|)cos(-Qx + deltar - delta), x<-1/2;}
{  yI(x)/t = (1/|t|)sin(Qx - delta) + (|r|/|t|)sin(-Qx + deltar - delta), x<-1/2;}
{and taking derivative w r t x:}
{}
{dyR/dx/t= -(Q/|t|)sin(Qx-delta) + (Q|r|/|t|)sin(-Qx + deltar - delta), x<-1/2;}
{dyI/dx/t= (Q/|t|)cos(Qx-delta) - (Q|r|/|t|)cos(-Qx + deltar - delta), x<-1/2;}
{}
{   Thus we start on the right side at x = 1/2 and integrate backwards to}
{   x=-1/2 and fit to the above four expressions evaluated at x = -1/2.}
{   This gives 4 equations for the 4 unknowns:}
{                       Transmssion coef. t = |t| exp(i delta)}
{                       reflection coef.  r = |r| exp(i deltar)}
{}
{   Solving the four equations we get:  (yR is yR/t (x=-1/2), etc.)}
{}
{            |t| =  2/sqrt[sqr(yR + DyI/Q) + sqr(yI - DyR/Q)]}
{}
{            and}
{               cos(Q/2 + delta) = (|t|/2)(yR + DyI/Q);}
{               sin(Q/2 + delta) = (|t|/2)(DyR/Q - yI),}
{             or}
{               delta = arctan[(DyR/Q - yI)/(yR + DyI/Q)] - Q/2;}
{               If (yR + DyI/Q) < 0  add pi to delta to get in correct quad.}
{}
{            The above is all we need for the particular problem at hand,}
{            however the reflection coefficient is also calc.}
{}
{            |r| = (sqrt[sqr(|t|) (sqr(DyI/Q - yR) + sqr(DyR/Q + yI)) ])/2}
{            and}
{               cos(Q/2 + deltar - delta) = (|t|/(2|r|))(yR - DyI/Q);}
{               sin(Q/2 + deltar - delta) = (|t|/(2|r|))(yI + DyR/Q),}
{             or}
{               deltar = arctan[(DyR/Q + yI)/(yR - DyI/Q)] - Q/2 + delta}
{               If (yR - DyI/Q) < 0 add pi to deltar.}
{}
{   The integration is done backwards from x=1/2 to -1/2 on an x grid}
{}
{        1     2     3     4     5              N-2   N-1     N}
{        .     .     .     .     .   ...         .     .      .}
{   -1/2-2h -1/2-h -1/2  -1/2+h               1/2-h   1/2   1/2+h}
{}
{   Must integrate two steps beyond -1/2 because want to evaluate the}
{   derivative of the solution at -1/2 and must be at least 2 steps from}
{   the end of the integration.}
{}
{      Thus x=-1/2 is at index 3, x=+1/2 is at index N-1, and h=1/(N-4).   *)
    Procedure Prepare_for_Numerov (Var Q: REAL);
      Var
        inum: INTEGER;
        C: REAL;
    Begin
      yR1 := Cos(Q * x1);
      yR2 := Cos(Q * x2);
      yI1 := Sin(Q * x1);
      yI2 := Sin(Q * x2);
      yR.Put(Numerov_Pts, yR1);
      yR.Put(Numerov_Pts - 1, yR2);
      yI.Put(Numerov_Pts, yI1);
      yI.Put(Numerov_Pts - 1, yI2);
      c := hsq / 12.0;
      For inum := 1 To Numerov_Pts Do
      Begin
{PC}
        A.VecPtr^[inum]:= (VB.VecPtr^[inum] - Q * Q) * c;
{CP}
{MAC}
{        A.VecHandle^^[inum]:= (VB.VecHandle^^[inum] - Q * Q) * c;}
{CAM}
      End;
    End; {Prepare_for_Numerov}

  Begin {Calc_t_and_delta}
    Prepare_for_Numerov(Q);
    Numerov(Numerov_Pts, 1, h, A, yR);
    Numerov(Numerov_Pts, 1, h, A, yI);
    i_at_minus_1half := 3;
    DRePsi := NumerovDeriv(i_at_minus_1half, h, A, yR);
    DImPsi := NumerovDeriv(i_at_minus_1half, h, A, yI);
    DRePsibyQ := DRePsi / Q;
    DImPsibyQ := DImPsi / Q;
    yR.Get(i_at_minus_1half, RePsi);
    yI.Get(i_at_minus_1half, ImPsi);
    ytmp := DRePsibyQ - ImPsi;
    xtmp := DImPsibyQ + RePsi;
    tsq := 4 / (sqr(xtmp) + sqr(ytmp));
    Abs_t := sqrt(tsq);
    delta := ArcTan2(xtmp, ytmp);
{     delta:= ArcTan(ytmp/xtmp) ;}
{     IF xtmp<0 THEN delta:= delta+pi;}
    delta := delta - (Q / 2);
    ytmp := ImPsi + DRePsibyQ;
    xtmp := RePsi - DImPsibyQ;
    rsq := tsq * (sqr(xtmp) + sqr(ytmp)) / 4.0;
    deltar := ArcTan2(xtmp, ytmp);
{     deltar:= ArcTan(ytmp/xtmp);}
{     IF (RePsi-DImPsi)<0 THEN deltar:= deltar+pi;}
    Abs_r := sqrt(rsq);
    deltar := deltar - Q / 2 + delta;
  End; {Calc_t_and_delta}

  Function GenKronig_PennyFn (Q: REAL): REAL;
  Begin
    If PotIDNo > 0 Then
      Calc_t_and_delta(Q, V0, VPotPrm, abs_t, delta, yR, yI, A)
    Else
      Check_Square_Barrier(Q, V0, VPotPrm[1], Abs_t, delta);
    If Abs_t < 1.0e-30 Then
      GenKronig_PennyFn := 1.0e30 * Cos(Q + delta)
    Else
      GenKronig_PennyFn := Cos(Q + delta) / Abs_t;
  End;

  Function Fka_minus1 (x: REAL): REAL;
  Begin
    Fka_minus1 := GenKronig_PennyFn(x) - 1.0;
  End;

  Function Fka_plus1 (x: REAL): REAL;
  Begin
    Fka_plus1 := GenKronig_PennyFn(x) + 1.0;
  End;

  Function DFka (Q: Real): REAL;
{PC}
  FAR;
{CP}
    Const
      h = 0.05;
  Begin
    DFka := (GenKronig_PennyFn(Q + h) - GenKronig_PennyFn(Q - h)) / (2 * h);
  End;
{PC}
PROCEDURE Find_Next_Zero_F(F: FUNCTIONX; Tol, Qmax: REAL;
                           VAR Qm: REAL; VAR iter: INTEGER);
{CP}
{MAC}
{  Procedure Find_Next_Zero_F (Function F (X: REAL): REAL;}
{                  Tol, Qmax: REAL;}
{                  Var Qm: REAL;}
{                  Var iter: INTEGER);}
{CAM}
    Var
      x1, F1, x2, F2, x3, dx: REAL;
  Begin
   {Start by using search method}
    iter := 0;
    dx := 0.3;
    x1 := Qm + Tol;

    If x1 < Qmax Then
      F1 := F(x1);
    Repeat
      F2 := F1;
      x2 := x1;
      x1 := x1 + dx;
      If x1 > Qmax Then
      Begin
        iter := -1;
        x1 := Qmax;
        F1 := F(x1);
      End
      Else
        F1 := F(x1);
    Until ((F2 * F1 < 0) Or (x1 >= Qmax));
     {After bracketing by search method}
{      use secant method to find zero to within Tol.}
    If x1 <= Qmax Then
    Begin
      Repeat
        iter := iter + 1;
        x3 := x2 - F2 * (x2 - x1) / (F2 - F1);
        x1 := x2;
        F1 := F2;
        x2 := x3;
        F2 := F(x2);
      Until (((Abs(x1 - x2) <= Tol)) Or (iter > 40));{(ABS(F2)<= 100*Tol) AND}
      Qm := x2;

    End;
  End; {Find_Next_Zero_F}

  Var
    FbeqFminus1: BOOLEAN;
  Function Fb (X: REAL): REAL;
{PC}
  FAR;
{CP}
  Begin
    If FbeqFminus1 Then
      Fb := Fka_minus1(X)
    Else
      Fb := Fka_plus1(x);
  End;

  Function Ft (X: REAL): REAL;
{PC}
  FAR;
{CP}
  Begin
    If FbeqFminus1 Then
      Ft := Fka_plus1(x)
    Else
      Ft := Fka_minus1(x);
  End;

  Procedure Calc_E_of_k (V0, Qmax, Qtol: REAL;
                  Numerov_Pts, Nb, NPpb: INTEGER;
                  Var Q: REAL;
                  Var Ebot, Etop: BandEdgeArray;
                  Var k, E: DVector);
    Var
      Qbot, Qtop, QExtrm, FkaExtrm: Array[1..20] Of REAL;
      Qa, Qb, ftmp1, ftmp2: REAL;




    Procedure EvalEvsK (BandIndx, nppb: INTEGER;
                    PFlg: BOOLEAN;
                    Var k, E: DVector);
      Var
        j, jdum, I1, I2: INTEGER;
        kb, kb0, Q, halfDelQ, Qb, Qt, x, dx, mult, Ftmp2: REAL;
    Begin
      If (BandIndx Mod 2 = 0) Then
        mult := -1.0
      Else
        mult := 1.0;
      Q := Qbot[BandIndx] / pi;
      j := (BandIndx - 1) * nppb + 1;  {value of j at bottom of band no.= BandIndx}
      kb0 := (BandIndx - 1);
      E.Put(j, Q * Q);
      k.Put(j, kb0);
      Q := Qtop[BandIndx] / pi;
      j := BandIndx * nppb;      {value of j at top of band no.= BandIndx}
      kb := BandIndx;
      E.Put(j, Q * Q);
      k.Put(j, kb);
{Want points closely spaced in energy both at the bottom of the band}
{and a the top of the band.  Thus split the band in half}
{ and equally space sqrt(Q-Qbot) on bottom half band }
{ and equally space sqrt(Qtop-Q) on top half band.}
{ I1 = no. of intervals in bottom half band and I2 no. intervals in top half}
{ Thus, I1 + I2 = nppb - 1 where nppb = no. points per band.}
      I1 := (nppb - 1) Div 2;
      I2 := nppb - 1 - I1;
      Qb := Qbot[BandIndx];
      Qt := Qtop[BandIndx];
      halfDelQ := (Qt - Qb) / 2;
      dx := Sqrt(halfDelQ) / I1;
      Q := Qb;
      X := 0;
      j := (BandIndx - 1) * nppb + 1;
      For jdum := 1 To I1 Do
      Begin
        x := x + dx;
        Q := Sqr(x) + Qb;
        ftmp2 := mult * GenKronig_PennyFn(Q);{mult=+-1 depending on whether odd or even band}
        If Abs(Ftmp2) <= 1.0 Then
          kb := kb0 + (ArcCos(Ftmp2)) / pi
        Else If Ftmp2 > 1 Then
          kb := kb0
        Else
          kb := kb0 + 1;
        Q := Q / pi;
        j := j + 1;
        E.Put(j, Q * Q);
        k.Put(j, kb);
        Q := Q * pi;
      End;
      x := 0;
      Q := Qt;
      j := BandIndx * nppb;
      For jdum := 1 To I2 - 1 Do
      Begin
        x := x - dx;
        Q := Qt - sqr(x);
        ftmp2 := mult * GenKronig_PennyFn(Q);{mult=+-1 depending on whether odd or even band}
        If Abs(Ftmp2) <= 1.0 Then
          kb := kb0 + (ArcCos(Ftmp2)) / pi
        Else If Ftmp2 > 1 Then
          kb := kb0
        Else
          kb := kb0 + 1;
        Q := Q / pi;
        j := j - 1;
        E.Put(j, Q * Q);
        k.Put(j, kb);
        Q := Q * pi;
      End;
      If (Pflg And (BandIndx >= PltBandMin) And (BandIndx <= PltBandMax)) Then
        PlotEvsK(EColor, FALSE, ZonePlot, BandIndx, BandIndx, nppb, k, E);
    End; {EvalEvsK}

    Procedure DrawGenKronigPenneyFn;
      Const
        Npts = 800;
      Var
        Qa, GKPfn: DVector;
        i,tGBColor: INTEGER;
        dQ, temp: REAL;
        GKPFnwait: TWaitMessage;
    Begin
      ClearMuppetPort;
      SetColor(GraphBoxColor);
      OpenViewPort(3);
      PutLabel(top, 'Gen. Kronig-Penny Fn:  [Cos(Qa + delta)]/|t|');
      SetColor(AxisLabelColor);
      PutLabel(bottom, 'Qa  (in units of Pi)');
      PutLabel(left, 'Cos(Qa + delta)/|t|');
{MAC}
{      New(Qa);}
{CAM}
      Qa.Init(Npts);
{MAC}
{      New(GKPfn);}
{CAM}
      GKPfn.Init(Npts);
      dQ := QExtrm[Nb + 1] / Npts;
      Q := 0.0;
      I := 0;
{MAC}
{      new(GKPFnwait);}
{CAM}
      GKPFnwait.Show;
      Repeat
        I := I + 1;
        Q := Q + dQ;
        Qa.Put(i, Q / pi);
        GKPfn.put(i, GenKronig_PennyFn(Q));
        If I Mod 20 = 0 Then
          GKPFnwait.Update;
      Until I >= Npts;
      GKPFnwait.Hide;
{MAC}
{      dispose(GKPFnwait);}
{CAM}
      SetColor(GraphBoxColor);
      AutoScaling(4, Qa, GKPfn);
      selectscale(4);
      SetColor(GKPFnColor);
      PlotDvectors(Qa, GKPfn, 1, Npts);
      DefineViewPort(11,0.1,0.9,0.02,0.08);
      tGBColor := GraphBackColor;
      GraphBackColor := BLUE;
      OpenViewPort(11);
      Print(5,1,' Press key or click mouse to zoom in on x axis of plot.');
      GraphBackColor := tGBColor;
      PressKeyMessage;
      ClearMuppetPort;
      DefineScale(4, (QExtrm[2] / pi - 1), (QExtrm[Nb + 1] / pi + 1),
                     -5.0, +5.0);
      SetColor(GraphBoxColor);
      OpenViewPort(3);
      SelectScale(4);
      Axis(0.0, 0.0, 1, 1.0);
      PutLabel(top, 'Gen. Kronig-Penney Fn:  [Cos(Qa + delta)]/|t|');
      SetColor(AxisLabelColor);
      PutLabel(bottom, 'Qa  (in units of Pi)');
      PutLabel(left, 'Cos(Qa + delta)/|t|');
      SetColor(GKPFnColor);
{The following FOR LOOP is a work-a-round for a bug in the CUPS utility}
{ PlotDVectors procedure that plots vertial lines if the y DVector is}
{ several orders of magnitude larger than the boarder of the plot.}
      For i := 1 To Npts Do
      Begin
        temp := GKPfn.Value(i);
        If temp > 10 Then
          GKPfn.put(i, 11)
        Else If temp < -10 Then
          GKPfn.put(i, -11);
      End;
      PlotDvectors(Qa, GKPfn, 1, Npts);
      For I := 1 To Nb Do
      Begin
        SetColor(Yellow);
{MAC}
{        LineTP(MapX(0.0), MapY(1.0), MapX(QExtrm[Nb + 1] / pi), MapY(1.0));}
{        LineTP(MapX(0.0), MapY(-1.0), MapX(QExtrm[Nb + 1] / pi), MapY(-1.0));}
{CAM}
{PC}
        Line(MapX(0.0), MapY(1.0), MapX(QExtrm[Nb + 1] / pi), MapY(1.0));
        Line(MapX(0.0), MapY(-1.0), MapX(QExtrm[Nb + 1] / pi), MapY(-1.0));
{CP}
        SetColor(Green);
{MAC}
{        LineTP(MapX(Qbot[i] / pi), MapY(-1.8), MapX(Qbot[i] / pi), MapY(1.8));}
{CAM}
{PC}
        Line(MapX(Qbot[i] / pi), MapY(-1.8), MapX(Qbot[i] / pi), MapY(1.8));
{CP}
        SetColor(Red);
{MAC}
{        LineTP(MapX(Qtop[i] / pi), MapY(-1.8), MapX(Qtop[i] / pi), MapY(1.8));}
{CAM}
{PC}
        Line(MapX(Qtop[i] / pi), MapY(-1.8), MapX(Qtop[i] / pi), MapY(1.8));
{CP}
      End;
      PressKeyMessage;

      BandMenu.Display;
      BandHotKeys1.Display;
      Qa.Free;
      GKPfn.Free;
{MAC}
{      Dispose(Qa);}
{      Dispose(GKPfn);}
{CAM}
    End; {DrawGenKronigPenneyFn}



    Procedure Calc_Band_Edges (Q: REAL;
                    NBds: INTEGER;
                    EgvsV0Flg, DrawRaw, WrtFlg: BOOLEAN;
                    Var EScaleMax: REAL;
                    Var Ebot, Etop: BandEdgeArray);
      Var
        I: INTEGER;
        Q1, tempF, tempF1, estEmin, estEmax: REAL;
        CutShortFlg: BOOLEAN;

    Begin
      If Not EgvsV0Flg Then
      Begin
        If ((Not WrtFlg) And (Not GKPFnFlg)) Then
        Begin
          CloseViewPort(10);
          DrawVPot(V0);
        End;
        Message('Wait...Calculating band edges...');
        InitWaitSlash(-1, -1);
        If WrtFlg Then
          SelectViewPort(0);
      End;
      QExtrm[1] := 0.0 + Qtol;
      Q := 0.0 + Qtol;
      I := 1;
      FkaExtrm[1] := GenKronig_PennyFn(QExtrm[1]);
      tempF := FkaExtrm[1];
      If tempF > 1.0E5 Then
        Repeat
          Q1 := Q;
          tempF1 := tempF;
          Q := Q + 1;
          tempF := GenKronig_PennyFn(Q);
        Until (tempF < tempF1);
      For I := 2 To Nbds + 1 Do
      Begin
        RotateWaitSlash;
        Find_Next_Zero_F(DFka, Qtol, Qmax, Q, iter);
        FkaExtrm[i] := GenKronig_PennyFn(Q);
        QExtrm[i] := Q;
      End;
      If ((Not EgvsV0Flg) And (WrtFlg)) Then
      Begin
        Print(2, 2, Concat(VPotLabel, ';  V0 = ', NumStr(V0, 4, 2)));
        For i := 1 To Nbds + 1 Do
        Begin
          Print(2, i + 3, Concat('E at extremum = ', Num2Str((Sqr(QExtrm[i] / pi)), 10)));
          Print(28, i + 3, Concat(' GKPFunction(E)=', Num2Str(FkaExtrm[i], 10), ' iter =', NumStr(iter, 3, 0)));
        End;
      End;
      Q := QExtrm[Nbds + 1] / pi;
      If PltBandMin = 1 Then
        estEmin := 0.0
      Else
        estEmin := Sqr(QExtrm[PltBandMin] / pi);
      estEmax := Sqr(Qextrm[PltBandMax + 1] / pi);
      EScaleMax := Sqr(Q);
      If ((Not EgvsV0Flg) And (Not WrtFlg) And (Not GKPFnFlg)) Then
      Begin
        If HistoryFlg Then
          Shift_History_Data(N_Saved_Data);
        InitEkPort(Evskvp, ZonePlot, PltBandMin, PltBandMax, Nbds, estEmin, estEmax);
        If ((N_Saved_Data > 1) And HistoryFlg) Then
          Plot_Saved_E_Data(N_Saved_Data, ZonePlot, PltBandMin, PltBandMax, nppb);
      End;
      Ftmp2 := (GenKronig_PennyFn(QExtrm[1]));
      For I := 1 To Nbds Do
      Begin
        Ftmp1 := Ftmp2;
        CutShortFlg := FALSE;
        If EgvsV0Flg Then
          RotateWaitSlash;
        Ftmp2 := (GenKronig_PennyFn(QExtrm[i + 1]));
        If I Mod 2 = 1 Then
        Begin
          If (Ftmp1 < 0) Then
          Begin
            Qbot[i] := QExtrm[i];
            Qtop[i] := Qbot[i];
            CutShortFlg := TRUE;
          End;
          If (Ftmp2 > 0) Then
          Begin
            Qbot[i] := QExtrm[i + 1];
            Qtop[i] := Qbot[i];
            CutShortFlg := TRUE;
          End;
          If Not CutShortFlg Then
          Begin
            Ftmp1 := Abs(Ftmp1);
            FbeqFminus1 := TRUE;
{               Fb:= Fka_minus1;}
{               Ft:= Fka_plus1;}
          End;
        End
        Else
        Begin
          If Ftmp1 > 0 Then
          Begin
            Qbot[i] := QExtrm[i];
            Qtop[i] := Qbot[i];
            CutShortFlg := TRUE;
          End;
          If Ftmp2 < 0 Then
          Begin
            Qbot[i] := QExtrm[i + 1];
            Qtop[i] := Qbot[i];
            CutShortFlg := TRUE;
          End;
          If Not CutShortFlg Then
          Begin
            Ftmp1 := Abs(Ftmp1);
            FbeqFminus1 := FALSE;
{               Fb:= Fka_plus1;}
{               Ft:= Fka_minus1;}
          End;
        End;
        If CutShortFlg Then
        Begin
          itmp1 := -1;
        End
        Else
        Begin
          If ((Not EgvsV0Flg) And (WrtFlg)) Then
            Print(1, 3, Concat('Finding Qbot for Band ', NumStr(i, 2, 0)));
          Q := QExtrm[i];
          Qa := QExtrm[i] + Qtol;
          Qb := QExtrm[i + 1] - Qtol;
          If (Ftmp1 >= 20) Then
          Begin
            Find_Next_Zero_F(Fb, Qtol, Qb, Q, iter);
            Qbot[i] := Q;
            itmp1 := iter;
          End
          Else If (Ftmp1 <= 1.0 + 10.0 * Qtol) Then
          Begin
            If Ftmp1 > 1.0 Then
              Qbot[i] := QExtrm[i] + Sqrt(2.0 * (Ftmp1 - 1.0) / Ftmp1)
            Else
              Qbot[i] := QExtrm[i];
            itmp1 := 0;
          End
          Else
          Begin
            Qa := QExtrm[i];
            Qb := QExtrm[i + 1];
            FindZero(Fb, Qa, Qb, Qtol, Q, iter);
            Qbot[i] := Q;
            itmp1 := iter;
          End;
          Ftmp1 := Abs(Ftmp2);
          If ((Not EgvsV0Flg) And (WrtFlg)) Then
          Begin
            RubOut(1, 3, 40, Blakk);
            Print(1, 3, Concat('Finding Qtop for Band ', NumStr(i, 2, 0)));
          End;
          If (Ftmp1 >= 20) Then
          Begin
            If PotIDNo > 0 Then
              Calc_t_and_delta(Qbot[i], V0, VPotPrm, abs_t, delta, yR, yI, A)
            Else
              Check_Square_Barrier(Q, V0, VpotPrm[1], Abs_t, delta);
{               RubOut(1,3,10,GraphBackColor);}
{               Print(1,3,NumStr(abs_t,10,7));}
            If (abs_t < Qtol) Then
              Q := Qbot[i] + 2 * Abs_t
            Else
            Begin
{                  IF ((NOT EgvsV0Flg) AND (WrtFlg)) THEN}
{                     Print(1,3,'Mark 1: Qbot= '+ NumStr(Qbot[i],10,5)}
{                               + 'GenK_PFn(Qbot)= ' +}
{                                Numstr(GenKronig_PennyFn(Qbot[i]),10,5));}
              If Ft(Qbot[i]) * Ft(QExtrm[i + 1]) > 0.0 Then
              Begin
                Q := Qbot[i];
                iter := 0;
              End
              Else
                FindZero(Ft, Qbot[i] - Qtol, QExtrm[i + 1], Qtol, Q, iter);
{                  IF ((NOT EgvsV0Flg) AND (WrtFlg)) THEN}
{                     RubOut(1,3,40,GraphBackColor);}
{                  Find_Zero_F_by_Secant(Ft,Qtol,Qbot[i],Qbot[i]+4*Abs_t,Q,iter);}
            End;
            Qtop[i] := Q;
          End
          Else If (Ftmp1 <= 1.0 + 10.0 * Qtol) Then
          Begin
            If Ftmp1 > 1.0 Then
              Qtop[i] := QExtrm[i + 1] - Sqrt(2.0 * (Ftmp1 - 1.0) / Ftmp1)
            Else
              Qtop[i] := QExtrm[i + 1];
            iter := 0;
          End
          Else
          Begin
{                  IF ((NOT EgvsV0Flg) AND (WrtFlg)) THEN}
{                     Print(1,3,'Mark 2');}
            FindZero(Ft, Qbot[i], QExtrm[i + 1], Qtol, Q, iter);
{                  IF ((NOT EgvsV0Flg) AND (WrtFlg)) THEN}
{                     RubOut(1,3,12,GraphBackColor);}
{                  Find_Zero_F_by_Secant(Ft,Qtol,Qbot[i],QExtrm[i+1]-Qtol,Q,iter);}
            Qtop[i] := Q;
          End;
          If ((Not EgvsV0Flg) And (WrtFlg)) Then
            RubOut(1, 3, 40, Blakk);
        End;
        CalcInfV0Energy(V0, PotIDNo, i, InfV0Energy);
        If (Not EgvsV0Flg) Then
        Begin
          If ((Not WrtFlg) And (Not GKPFnFlg)) Then
          Begin
            Message('Wait...Calculating E vs k...');
            EvalEvsK(i, nppb, TRUE, k, E);
          End
          Else
            EvalEvsK(i, nppb, FALSE, k, E);
        End;
        Ebot[i] := Sqr(Qbot[i] / pi);
        Etop[i] := Sqr(Qtop[i] / pi);
        If ((Not EgvsV0Flg) And (WrtFlg)) Then
        Begin
{            Print(2,2*i+2,'E at extremum = '+Num2Str((Sqr(QExtrm[i]/pi)),10));}
{            Print(28,2*i+2,' GKPFunction(E)='+Num2Str(FkaExtrm[i],10)+}
{                  ' iter ='+NumStr(iter,3,0));}
          Print(2, i + Nbds + 4, Concat('Band ', NumStr(i, 3, 0), ': Ebot =',
          Num2Str(Ebot[i], 10), ' iter=', NumStr(itmp1, 3, 0), ';',
          ' Etop =', Num2Str(Etop[i], 10), ' iter=', NumStr(iter, 3, 0)));
        End;
      End; {FOR I:=1 TO Nbds}
      Ebot[Nbds + 1] := EScaleMax;
      If (Not EgvsV0Flg) Then
      Begin
        If (WrtFlg) Then
          PressKeyMessage;
        If GKPFnFlg Then
          DrawGenKronigPenneyFn;
        If HistoryFlg Then
          If (Not New_History_Record_Complete) Then
            Update_History_Data;
        If ((WrtFlg) Or (GKPFnFlg)) Then
        Begin
          If HistoryFlg Then
            Shift_History_Data(N_Saved_Data);
          InitEkPort(Evskvp, ZonePlot, PltBandMin, PltBandMax, Nbds, estEmin, estEmax);
          If HistoryFlg Then
            Plot_Saved_E_Data(N_Saved_Data, ZonePlot, PltBandMin, PltBandMax, nppb);
          PlotEvsK(EColor, FALSE, ZonePlot, PltBandMin, PltBandMax, nppb, k, E);
          BandMenu.Display;
          BandHotKeys1.Display;
        End
        Else
        Begin
          UpDateBandMenu;
          BandMenu.Display;
          BandHotKeys1.Display;
        End;
      End; { IF NOT EgvsV0Flg}
      If Not EgvsV0FLG Then
        ClearMKBuffers;
    End; {Calc_Band_Edges}

  Begin  {Calc_E_of_k}
    Numerov_Init_Once;
    Q := Qtol;
    If ((WrtFlg) And (Not EgvsV0Flg)) Then
    Begin
      CloseViewPort(10);
    End;
    Calc_Band_Edges(Q, Nb, EgvsV0Flg, GKPFnFlg, WrtFlg, EScaleMax, Ebot, Etop);
  End;

  Procedure Calc_Efree (Nppb, Nbands: INTEGER;
                  Var Kfree, Efree: DVector);
  {Calculates free electron E(k).}
{        Input:}
{          Nppb     is the number of points per band.}
{          Nbands   is the number of bands to be included.}
{        Output:}
{          kfree    is the Dvector containing the free el. k vector.}
{          Efree    is the DVector containing the free el. Energy.   }
    Var
      i, j: INTEGER;
      kf, delkf: REAL;
  Begin
    delkf := 1 / (Nppb - 1);
    For i := 1 To Nbands Do
    Begin
      kf := (i - 1);
      j := ((i - 1) * Nppb + 1);
      Repeat
        kfree.Put(j, kf);
        Efree.Put(j, kf * kf);
        kf := kf + delkf;
        j := j + 1;
      Until (j = i * Nppb + 1);
    End;
  End; {Calc_Efree}

  Procedure PointAndClickMessage;
    Var
      tGBColor: INTEGER;
  Begin
    tGBColor := GraphBackColor;
    GraphBackColor := Blue;
    IF WFNvp=6 Then
       OpenViewPort(8);
    IF WFNvp=1 Then
       OpenViewPort(9);
    GraphBackColor := tGBColor;
    SetColor(Whitte);
    Print(2,2,'You may choose E(k) ');
    Print(2,3,'state with mouse and');
    Print(2,4,'wave function will ');
    Print(2,5,'be displayed here.');
    SelectViewPort(Evskvp);
  End;  {Procedure PointandClickMessage}

  Procedure DrawSavedWfns(ndx: Integer); FORWARD;

  Procedure ShowEvsk;
    Var
      Elabel: String;
      PltEmin, PltEmax: REAL;
  Begin
    If PltBandMin = 1 Then
      PltEmin := Ebot[PltBandMin] / 2
    Else
      PltEmin := (Ebot[PltBandMin] + Etop[PltBandMin - 1]) / 2;
    PltEmax := (Etop[PltBandMax] + Ebot[PltBandMax + 1]) / 2;
    InitEkPort(Evskvp, ZonePlot, PltBandMin, PltBandMax, Nbands, PltEmin, PltEmax);
    Plot_Saved_E_Data(N_Saved_Data, ZonePlot, PltBandMin, PltBandMax, Num_pts_per_band);
    PlotEvsK(EColor, FALSE, ZonePlot, PltBandMin, PltBandMax, Num_pts_per_band, k, E);
    If N_Saved_WaveFns>0 Then
      DrawSavedWfns(WFNvp)
    Else
      PointAndClickMessage;
  End; {PROCEDURE ShowEvsk}




  Procedure Egap_vs_V0 (Vmin, Vmax: REAL;
                  NlogVpts, ibmin, ibmax: INTEGER;
                  Var logV0: DVector;
                  Var logEgap: BandGapDVectors);
    Var
      i, j, ii, N, tgbxcolor: INTEGER;
      V0original, EScaleMaxOrig, log_V0, temp: REAL;
      logVscMin, logVscMax, logEg, logEgmin, logEgmax, DVcalc: REAL;
      DVtheory: REAL;
      PercentDone: String;
      LocalQuitFlg: BOOLEAN;
      Vscale, logEgscale: DVector;
      tempE, Ebotorig, Etoporig: BandEdgeArray;
      tempVInf: Array[0..20] Of REAL;
      tempVPerturb: Array[0..20] Of REAL;
      templogV0, templogEg: DVector;

    Procedure PltEgvsV0 (i: INTEGER;
                    logEgmin, logEgmax: REAL);
      Var
        ii, j, origcolor, ix1, ix2, iy1, iy2, idy: INTEGER;
        OldTextStyle: TextSettingsType;
        templogV0, templogEg: DVector;
    Begin
{      ClearMuppetPort;}
      CloseViewPort(10);
      origColor := GetColor;
      SetColor(GraphBoxColor);
      logVscMin := (log10(Vmin) - 3 * log10(DVtheory));
      If Enable_InfV0_Approx Then
        logVscMax := (log10(Vmax) + 3 * log10(DVtheory))
      Else
        logVscMax := log10(Vmax);
      OpenViewPort(2);
{MAC}
{      New(Vscale);}
{CAM}
      Vscale.init(2);
      Vscale.Put(1, logVscMin);
      Vscale.Put(2, logVscMax);
{MAC}
{      New(logEgScale);}
{CAM}
      logEgScale.init(2);
      If logEgmin < -2 Then
        logEgmin := -2;
      logEgScale.Put(1, logEgmin);
      logEgScale.Put(2, logEgmax);
      AutoScaling(11, Vscale, logEgScale);
      Vscale.Free;
      logEgScale.Free;
{MAC}
{      Dispose(Vscale);}
{      Dispose(logEgScale);}
{CAM}

      SelectScale(11);
      PutLabel(top, 'Log - Log Plot of Band Gaps vs. V0');
      SetColor(AxisLabelColor);
      PutLabel(inside, VPotLabel);
      PutLabel(bottom, 'log(V0)  (V0 in units of h^2/8ma^2)');
      PutLabel(left, 'log(Egap)  ');
      GetTextSettings(OldTextStyle);
      SetTextJustify(LeftText, CenterText);
      SetLineStyle(DottedLn, $FFFF, ThickWidth);
      idy := Abs(MapY(logEgMax) - MapY(logEgMin));
      ix1 := MapX(logVscMin);
      iy1 := Round(MapY(logEgMax) + 0.1 * idy);
      ix2 := MapX(logVscMin + 2 * log10(DVtheory));
      iy2 := iy1;
{MAC}
{      LineTP(ix1, iy1, ix2, iy2);}
{CAM}
{PC}
      Line(ix1, iy1, ix2, iy2);
{CP}
      OutTextXY((ix1 + ix2) Div 2, iy1, 'N');
      OutTextXY(ix2, iy2, '  Perturb. Approx.');
      SetLineStyle(SolidLn, $FFFF, NormWidth);
      iy1 := iy1 - Round(0.05 * idy);
      iy2 := iy1;
{MAC}
{      LineTP(ix1, iy1, ix2, iy2);}
{CAM}
{PC}
      Line(ix1, iy1, ix2, iy2);
{CP}
      OutTextXY((ix1 + ix2) Div 2, iy1, 'N');
      OutTextXY(ix2, iy2, '  Calc. Band Gap N');
      If Enable_InfV0_Approx Then
      Begin
        SetLineStyle(SolidLn, $FFFF, ThickWidth);
        iy1 := iy1 + 2 * Round(0.05 * idy);
        iy2 := iy1;
{MAC}
{        LineTP(ix1, iy1, ix2, iy2);}
{CAM}
{PC}
        Line(ix1, iy1, ix2, iy2);
{CP}
        OutTextXY((ix1 + ix2) Div 2, iy1, 'N');
        OutTextXY(ix2, iy2, '  Inf. V0 Approx.');
      End;
      SetLineStyle(SolidLn, $FFFF, NormWidth);
      With OldTextStyle Do
      Begin
        SetTextJustify(Horiz, Vert);
        SetTextStyle(Font, Direction, CharSize);
      End;
      For j := ibmin To ibmax Do
      Begin
        SetColor(PlotColorN(j));
{Note: The CUPS graphics utilities procedure PlotPoints should have}
{       the ibeg,iend feature that PlotDVectors has...perhaps it should}
{       also have an additional parameter, iskip, that indicates that the}
{       symbol is plotted at element ibeg, ibeg+iskip, ibeg+2*iskip, ...etc.}

        If Enable_InfV0_Approx Then
        Begin
          PlotPoints(logInfV0, logInfV0Egap[j], bandchar[j]);
          SetLineStyle(SolidLn, $FFFF, ThickWidth);
          PlotDVectors(logInfV0, logInfV0Egap[j], 1, NumVInfPts);
          SetLineStyle(SolidLn, $FFFF, NormWidth);
        End;
        PlotPoints(logPerturbV0, logPerturbEgap[j], bandchar[j]);
        SetLineStyle(DottedLn, $FFFF, ThickWidth);
        PlotDVectors(logPerturbV0, logPerturbEgap[j], 1, NumVPerturbPts);
        SetLineStyle(SolidLn, $FFFF, NormWidth);
        IF i>0 Then
        Begin
{**This is a work-a-round because PlotPoints does not have ibeg,iend ***}
{MAC}
{          New(templogEg);}
{          New(templogV0);}
{CAM}
          templogEg.init(i);
          templogV0.init(i);
          For ii := 1 To i Do
          Begin
            templogV0.Put(ii, logV0.Value(ii));
            templogEg.Put(ii, logEgap[j].Value(ii));
          End;
          PlotPoints(templogV0, templogEg, bandchar[j]);
          templogEg.free;
          templogV0.free;
{MAC}
{          dispose(templogEg);}
{          dispose(templogV0);}
{CAM}
{**End Work-a-round*****************************************************}
          PlotDvectors(logV0, logEgap[j], 1, i);
        End;
      End;
      SetColor(origColor);
    End;

  Begin {Egap_vs_V0}
    BandHotKeys1.Clear;
    EgvsV0Flg := TRUE;
    LocalQuitFlg := FALSE;
{Save all data when entered Egap_vs_V0}
    V0original := V0;
    EScaleMaxOrig := EScaleMax;
    Ebotorig := Ebot;
    Etoporig := Etop;
{Completed Save}
    V0 := Vmin;
    DVcalc := Exp((ln(Vmax) - ln(Vmin)) / (NlogVpts - 1));
    DVtheory := Exp((ln(Vmax) - ln(Vmin)) / 19);
    logEgmin := 1.0E10;
    logEgmax := 1.0E-10;
    {This scaling is premature but must get xmax from Autoscaling procedure}
    {So know where to put the InfV0 theoretical data on the plot.}
    logVscMin := (log10(Vmin) - 3 * log10(DVtheory));
    If Enable_InfV0_Approx Then
      logVscMax := (log10(Vmax) + 3 * log10(DVtheory))
    Else
      logVscMax := log10(Vmax);
{MAC}
{      New(Vscale);}
{CAM}
    Vscale.init(2);
    Vscale.Put(1, logVscMin);
    Vscale.Put(2, logVscMax);
    DefineViewPort(11,0,0.01,0.2,0.21);
    selectViewPort(11);
    {to keep AutoScaling from drawing much}
    AutoScaling(11, Vscale, Vscale);
    closeViewPort(10); {to clear garbage AutoScaling put on screen}
    Vscale.Free;
{MAC}
{      Dispose(Vscale);}
{CAM}

    If NeedNewEgvsV0 Then
    Begin
      For i := 1 To NBands Do
      Begin
        logEgap[i].Free;
        logEgap[i].init(NlogVpts);
      End;
      logV0.Free;
      logV0.init(NlogVpts);
      InitWaitSlash(-1, -1);
      With Graphscales[11] Do
        tempVInf[0] := exp((xmax -(xmax-xmin)/100) * ln(10.0)
                            - (NumVInfPts) * ln(DVtheory));
      tempVPerturb[0] := exp(ln(Vmin) - 4 * ln(DVtheory));
      Perturbation_Calc_Vcoeff(ReducedVcoeff);
      For i := 1 To NumVPerturbPts Do
      Begin
        tempVPerturb[i] := DVtheory * tempVPerturb[i - 1];
        logPerturbV0.Put(i, log10(tempVPerturb[i]));
        For j := 1 To MaxBands Do
          logPerturbEgap[j].Put(i, log10(2 * ReducedVcoeff[j] * tempVPerturb[i]));
      End;

      Message(Concat('Calculating Scales for Plotting...',
                     '             <Esc> to Abort Calculation'));
      If Enable_InfV0_Approx Then
      Begin
        For i := 1 To NumVInfPts Do
        Begin
          tempVInf[i] := DVtheory * tempVInf[i - 1];
          logInfV0.Put(i, log10(tempVInf[i]));
          For j := 1 To NBands + 1 Do
          Begin
            CalcInfV0Energy(tempVInf[i], PotIDNo, j, tempE);
            If j > 1 Then
            Begin
              logEg := log10(tempE[j] - tempE[j - 1]);
              If logEg > logEgMax Then
                logEgMax := logEg;
              logInfV0Egap[j - 1].put(i, logEg);
            End;
          End;
        End;
      End {IF Enable_InfV0_Approx}
      Else
      Begin
        temp := V0;
        V0 := vmax;
        Calc_E_of_k(V0, Qmax, Qtol, Numerov_pts, Nbands + 1,
                     Num_Pts_per_Band, Q, Ebot, Etop, k, E);
        V0 := temp;
        For ii := 1 To Nbands Do
        Begin
          If ((Ebot[ii + 1] - Etop[ii]) > 1.0E-10) Then
            logEg := log10(Ebot[ii + 1] - Etop[ii])
          Else
            logEg := -10;
          If logEg > logEgMax Then
            logEgMax := logEg;
        End;
      End; {IF Enable_InfV0_Approx ELSE}
      i := 0;
      PltEgvsV0(i, log10(Vmin), logEgmax);
      While ((i < NlogVpts) And (Not LocalQuitFlg)) Do
      Begin
        i := i + 1;
        PercentDone := NumStr(100.0 * (i - 1) / NlogVpts, 2, 0);
        Message(Concat('Calculating Band Edges...', PercentDone, '% done.               <Esc> to Abort Calculation'));
        If i > 1 Then
          V0 := DVcalc * V0;
        log_V0 := log10(V0);
        logV0.Put(i, log_V0);
        Calc_E_of_k(V0, Qmax, Qtol, Numerov_pts, Nbands + 1, Num_Pts_per_Band, Q, Ebot, Etop, k, E);
        For j := 1 To NBands Do
        Begin
          If ((Ebot[j + 1] - Etop[j]) > 1.0E-10) Then
            logEg := log10(Ebot[j + 1] - Etop[j])
          Else
            logEg := -10;
          If logEg < logEgmin Then
            logEgmin := logEg;
          If logEg > logEgmax Then
            logEgmax := logEg;
          logEgap[j].Put(i, logEg);
        End;
        If ((i Mod 5 = 0) Or (i = NlogVpts)) Then
        Begin
          If i = NlogVpts Then
            EraseWaitSlash;
          PltEgvsV0(i, logEgmin, logEgmax);
          If i < NlogVpts Then
            RotateWaitSlash;
        End;
        If KeyPressed Then
          If ReadKey = chr(27) Then
          Begin
            LocalQuitFlg := TRUE;
            EraseWaitSlash;
          End;
      End; {WHILE}
      If (Not LocalQuitFlg) Then
        NeedNewEgvsV0 := FALSE;
    End
    Else
    Begin
      For j := 1 To NBands Do
        For i := 1 To NlogVpts Do
        Begin
          If logEgap[j].Value(i) < logEgmin Then
            logEgmin := logEgap[j].Value(i);
          If logEgap[j].Value(i) > logEgmax Then
            logEgmax := logEgap[j].Value(i);
        End;
      PltEgvsV0(NlogVpts, logEgmin, logEgmax);
    End;
    PressKeyMessage;
    SetColor(Whitte);
    UpDateBandMenu;
    BandMenu.Display;
    BandHotKeys1.Display;
    V0 := V0original;
    EScaleMax := EScaleMaxOrig;
    Ebot := Ebotorig;
    Etop := Etoporig;
    EgvsV0Flg := FALSE;
    ClearMKBuffers;
  End; {Egap_vs_V0}


  Procedure ReadNumBands (Var NBands, PBMin, PBMax, HistLevel: INTEGER);
    Var
      ParmScreen: TInputScreen;
      tnbands, tpbmax: INTEGER;
  Begin
    ScreenEscFlag := FALSE;
{MAC}
{    new(ParmScreen);}
{CAM}
    With ParmScreen Do
    Begin
      Init;
      DefineInputPort(0.42, 0.80, 0.44, 0.95);
      LoadLine(' Highest Band Index Calc.:');
      LoadLine('      NBands = {   } ');
      LoadLine('');
      LoadLine('Bands displayed in E(k) Plot');
      LoadLine('   Range from Band Index  ');
      LoadLine('NBMin= {   } to  NBMax= {   }');
      LoadLine('');
      LoadLine('  Set the maximum number of');
      LoadLine('  Old E(k) plots displayed ');
      LoadLine('    History_Level= {   }');
      Loadline('');
      LoadLine('  [  Ok  ] [ Help ] [Cancel]');
      SetNumber(1, NBands);
      SetNumber(2, PBMin);
      SetNumber(3, PBMax);
      SetNumber(4, History_Level);
      SetHelpFile('Bands.hlp', 'ReadNumBands Menu Help');
      AcceptScreen;
      If Canceled Then
      Begin
        ScreenEscFlag := TRUE;
        CalcFlg := FALSE;
      End
      Else
      Begin
        tnbands := NBands;
        NBands := Round(Abs(GetNumber(1)));
        If NBands > MaxBands Then
          NBands := MaxBands;
        If NBands < 1 Then
          Nbands := 1;
        If NBands = tnbands Then
          CalcFlg := FALSE
        Else
        Begin
          Num_Pts_per_band := Num_E_Pts Div Nbands;
          CalcFlg := TRUE;
{            NeedNewEgvsV0:= TRUE;}
          N_Saved_Data := 0;
          New_History_Record_Complete := FALSE;
          ColorIndex := 1;
        End;
        tpbmax := PBMax;
        PBMin := Round(Abs(GetNumber(2)));
        PBMax := Round(Abs(GetNumber(3)));
        If ((NBands <> tnbands) And (PBMax = tpbmax)) Then
          PBMax := NBands;
        If PBMin < 1 Then
          PBMin := 1;
        If PBMax < 1 Then
          PBMax := 1;
        If PBMax > NBands Then
          PBMax := NBands;
        If PBMin > PBMax Then
          PBMin := PBMax;
        HistLevel := Round(Abs(GetNumber(4)));
        If HistLevel > Max_History_Level Then
          HistLevel := Max_History_Level;
        If N_Saved_Data > (HistLevel + 1) Then
          N_Saved_Data := (HistLevel + 1);

        ClearMKBuffers;
      End;
      done;
    End; {WITH}
{MAC}
{    dispose(ParmScreen);}
{CAM}
  End;


  Procedure ChangeV0 (Var V0: REAL);
    Var
      ParmScreen: TInputScreen;
      V0orig: REAL;
  Begin
    ScreenEscFlag := FALSE;
{MAC}
{    new(ParmScreen);}
{CAM}
    With ParmScreen Do
    Begin
      Init;
      DefineInputPort(0.05, 0.40, 0.68, 0.95);
      LoadLine('  Change Overall Strength');
      LoadLine(' of the Lattice Potential:');
      LoadLine('');
      LoadLine('    V0={     } (h^2/8ma^2)');
      LoadLine('');
      LoadLine(' [  Ok  ] [ Help ] [Cancel]');
      SetNumber(1, V0);
      SetHelpFile('Bands.hlp', 'ChangeV0 Menu Help');
      AcceptScreen;
      If Canceled Then
      Begin
        ScreenEscFlag := TRUE;
        CalcFlg := FALSE;
      End
      Else
      Begin
        V0orig := V0;
        V0 := Abs(GetNumber(1));
        If V0 < 0.01 Then
          V0 := 0.01
        Else If V0 > MaximumV0 Then
          V0 := MaximumV0;
        If Abs(V0orig - V0) > 1E-6 Then
          CalcFlg := TRUE;
      End;
      ClearMKBuffers;
      done;
    End; {WITH}
{MAC}
{    dispose(ParmScreen);}
{CAM}
  End;

  Procedure ReInitWaveFnData;
    Var
      i: integer;
  Begin
    For i := 0 To Max_WaveFnHistoryLevel Do
    Begin
      WaveFnData[i].P.free;
      WaveFnData[i].Re.free;
      WaveFnData[i].Im.free;
    End;
    WFx.free;
    WFx.Init(Numerov_Pts - 3);
    For i := 0 To Max_WaveFnHistoryLevel Do
    Begin
      WaveFnData[i].P.Init(Numerov_Pts - 3);
      WaveFnData[i].Re.Init(Numerov_Pts - 3);
      WaveFnData[i].Im.Init(Numerov_Pts - 3);
    End;
  End;

  Procedure Numerics (Var Qtol: REAL;
                  Var Numerov_Pts: INTEGER;
                  Var Wrtflg, GKPFnFlg: BOOLEAN);
    Var
      ParmScreen: TInputScreen;
      tQtol, tNumerov_Pts: REAL;
      twflg,tGKPFlg: BOOLEAN;
      n: byte;
  Begin
    ScreenEscFlag := FALSE;
{MAC}
{    new(ParmScreen);}
{CAM}
    With ParmScreen Do
    Begin
      Init;
      DefineInputPort(0.45, 0.95, 0.48, 0.95);
      LoadLine('  Parameters controlling');
      LoadLine('  the numerical precision.   ');
      LoadLine('');
      LoadLine('No. pts in Numerov integ. = {       }');
      LoadLine('Tolerance in Zero Searchs = {       }');
      LoadLine('');
      LoadLine('   Display intermediate results:');
      LoadLine('Write trace of band-edge search   #T');
      LoadLine('Plot Generalized Kronig-Penney Fn #T');
      LoadLine('');
      LoadLine(' [  Ok  ] [ Help ] [Cancel]');
      SetNumber(1, Numerov_Pts);
      SetNumber(2, Qtol);
      If WrtFlg Then
        SetBoolean(3, TRUE)
      Else
        SetBoolean(3, FALSE);
      If GKPFnFlg Then
        SetBoolean(4, TRUE)
      Else
        SetBoolean(4, FALSE);
      SetHelpFile('Bands.hlp', 'Numerics Menu Help');
      AcceptScreen;
      If Canceled Then
      Begin
        ScreenEscFlag := TRUE;
        CalcFlg := FALSE;
      End
      Else
      Begin
        tNumerov_Pts := Numerov_Pts;
        Numerov_Pts := Round(Abs(GetNumber(1)));
        If Numerov_Pts < MinNumerovPts Then
          Numerov_Pts := MinNumerovPts
        Else If Numerov_Pts > MaxNumerovPts Then
          Numerov_Pts := MaxNumerovPts;
        tQtol := Qtol;
        Qtol := (Abs(GetNumber(2)));
        If Qtol > 0.01 Then
          Qtol := 0.01;
        twflg := WrtFlg;
        WrtFlg := GetBoolean(3);
        tGKPFlg := GKPFnFlg;
        GKPFnFlg := GetBoolean(4);
        If (Numerov_Pts <> tNumerov_Pts) Then
        Begin
          VB.free;
          A.free;
          yR.free;
          yI.free;
          VB.Init(Numerov_Pts);
          A.Init(Numerov_Pts);
          yR.Init(Numerov_Pts);
          yI.Init(Numerov_Pts);
          ReInitWaveFnData;
          N_Saved_WaveFns := 0;
          ixlabel := Numerov_Pts Div 10;
          N_Saved_Data := 1;
          N_Saved_WaveFns := 0;
          ColorIndex := 1;
          Data[0].PltColor := NextPlotColor;
          CalcFlg := TRUE;
          NeedNewEgvsV0 := TRUE;
        End;
        If (Qtol <> tQtol) Then
        Begin
          CalcFlg := TRUE;
          NeedNewEgvsV0 := TRUE;
        End;
        If ((tWFlg<>WrtFlg) Or (tGKPFlg<>GKPFnFlg)) then
          CalcFlg := TRUE;
      End;
      done;
    End; {WITH}
{MAC}
{    dispose(ParmScreen);}
{CAM}
  End;


  Function EnterEgvsV0Param (Var Vmin, Vmax: REAL;
                  Var Npts, ibmin, ibmax: INTEGER): BOOLEAN;
    Var
      ParmScreen: TInputScreen;
      n: byte;
      Nold, ibmaxOld: INTEGER;
      vminold, vmaxold: REAL;
  Begin
    ScreenEscFlag := FALSE;
{MAC}
{    new(ParmScreen);}
{CAM}
    With ParmScreen Do
    Begin
      Init;
      DefineInputPort(0.55, 0.95, 0.48, 0.95);
      LoadLine('  Parameters related to');
      LoadLine('plot of Gap energies vs V0');
      LoadLine('');
      LoadLine(' Minimum V0 = {       }');
      LoadLine(' Maximum V0 = {       }');
      LoadLine(' No. of Pts = {       }');
      LoadLine(' Lowest Band index  = {    }');
      LoadLine(' Highest Band index = {    }');
      LoadLine('');
      LoadLine(' [  Ok  ] [ Help ] [Cancel]');
      SetNumber(1, Vmin);
      SetNumber(2, Vmax);
      SetNumber(3, Npts);
      SetNumber(4, ibmin);
      SetNumber(5, ibmax);
      Vminold := Vmin;
      Vmaxold := Vmax;
      Nold := Npts;
      ibmaxold := ibmax;
      EnterEgvsV0Param := FALSE;
      SetHelpFile('Bands.hlp', 'EnterEgvsV0Param Menu Help');
      AcceptScreen;
      If Canceled Then
      Begin
        ScreenEscFlag := TRUE;
        EnterEgvsV0Param := FALSE;
      End
      Else
      Begin
        EnterEgvsV0Param := TRUE;
        Vmin := Abs(GetNumber(1));
        If Vmin < 0.01 Then
          Vmin := 0.01;
        Vmax := Abs(GetNumber(2));
        If Vmax > MaximumV0 Then
          Vmax := MaximumV0;
        Npts := Round(Abs(GetNumber(3)));
        If ((VMin <> Vminold) Or (Vmax <> Vmaxold) Or (Npts <> Nold)) Then
          NeedNewEgvsV0 := TRUE;
        ibmin := Round(Abs(GetNumber(4)));
        If ibmin < 1 Then
          ibmin := 1;
        ibmax := Round(Abs(GetNumber(5)));
        If ibmax > Nbands Then
        Begin
          If ibmax > MaxBands Then
            ibmax := MaxBands;
          Nbands := ibmax;
          Num_Pts_per_band := Num_E_Pts Div Nbands;
          If ibmax <> ibmaxOld Then
            NeedNewEgvsV0 := TRUE;
{            CalcFlg:= TRUE;}
        End;
      End;
      done;
    End; {WITH}
{MAC}
{    dispose(ParmScreen);}
{CAM}
  End;

  Procedure Reducedk(kextended: REAL; Var kred: REAL; Var BandNo: INTEGER);
    Var
      ktmp: REAL;

    Begin
      ktmp:= ABS(kextended);
      BandNo:= Trunc(ktmp)+1;
      ktmp := ktmp - (BandNo Div 2) * 2;
      If kextended>0 Then kred := ktmp Else kred := -ktmp;
    End; {Procedure Reducedk}

  Procedure SetWFViewPort (Ndx: INTEGER);
  Begin
{    CloseViewPort(12);}
    If WFnPltType=0 Then
      SelectScale(6)
    Else
      SelectScale(7);
    SelectViewPort(ndx);
    SetColor(GraphBoxColor);
    OpenViewport(ndx);
    Axis(0.0, 0.0, 0.5, 1);
    Case WFnPltType OF
      0:
        Begin
          PutLabel(top, '|Psi|^2 for one unit cell');
          PutLabel(left, '|Psi|^2');
        End;
      1:
        Begin
          PutLabel(top, 'Re{Psi} for one unit cell');
          PutLabel(left, 'Re{Psi}');
        End;
      2:
        Begin
          PutLabel(top, 'Im{Psi} for one unit cell');
          PutLabel(left, 'Im{Psi}');
        End;
      End; {Case}
    SetColor(AxisLabelColor);
    PutLabel(bottom, ' x (in units of a)');
  End; {SetWFViewPort }

  Procedure DrawSavedWFns (ndx: INTEGER);
    Var
      k_actual,kred: REAL;
      i, npts, BandNo: INTEGER;
  Begin
    NPts := Numerov_Pts - 3;
    SetWFviewPort(ndx);
    If N_Saved_WaveFns > 0 Then
    Begin
      For I := 1 To N_Saved_WaveFns Do
      Begin
        SetColor(WaveFnData[indx[I]].Color);
        SelectviewPort(Evskvp);
        SelectScale(ZonePlot);
        k_actual:= WaveFnData[indx[i]].kact;

        If ZonePlot = 2 Then
          plotSymbol(ABS(k_actual),
                     WaveFnData[indx[i]].E,
                     bandchar[indx[i]])
        Else
        Begin
          Reducedk(k_actual,kred,BandNo);
          plotSymbol(kred, WaveFnData[indx[i]].E, bandchar[indx[i]])
        End;
        SelectviewPort(WFNvp);
        If WFnPltType=0 Then
          SelectScale(6)
        Else
          SelectScale(7);
        Case WFnPltType of
          0:
            Begin
              PlotDVectors(WFx, WaveFnData[indx[i]].P, 1, npts);
              PlotSymbol(WFx.Value(ixlabel),
                         WaveFnData[indx[i]].P.Value(ixlabel),
                         bandchar[indx[i]]);
            End;
          1:
            Begin
              PlotDVectors(WFx, WaveFnData[indx[i]].Re, 1, npts);
              PlotSymbol(WFx.Value(ixlabel),
                         WaveFnData[indx[i]].Re.Value(ixlabel),
                         bandchar[indx[i]]);
            End;
          2:
            Begin
              PlotDVectors(WFx, WaveFnData[indx[i]].Im, 1, npts);
              PlotSymbol(WFx.Value(ixlabel),
                         WaveFnData[indx[i]].Im.Value(ixlabel),
                         bandchar[indx[i]]);
            End;
          End;  {Case}
      End; {For i:=1 to N_Saved_WaveFns Do}
    End; {If N_Saved_WaveFns > 0 Then}
  End; {Procedure DrawSavedWFns}

(*{$S Two Seg}*)

  Procedure HandleBandMenu;
  FORWARD;
  Procedure HandleHotKeys1 (key: BYTE);
  FORWARD;
  Procedure HandleVPotSliders;
  FORWARD;
  Procedure WaveFnSubProg (kin, Ein: REAL;
                  Var MFlg, CSFlg: BOOLEAN;
                  Var WavFnD: ArrayofWaveFns);
    Const
      MainDBFlg = FALSE;
      DBLevel2Flg = FALSE;

    Function NxtFreeI: INTEGER;
      Var
        j, tnf: INTEGER;
        tResult: BOOLEAN;

      Function NotUsed (int: INTEGER): BOOLEAN;
        Var
          k: INTEGER;
          tResult: BOOLEAN;
      Begin
        tResult := TRUE;
        k := 0;
        If k < N_Saved_WaveFns Then
          Repeat
            k := k + 1;
            If Indx[k] = int Then
              tResult := FALSE;
          Until ((k = N_Saved_WaveFns) Or (Not tResult));
        NotUsed := tResult;
      End; {NotUsed}

    Begin  {FUNCTION NextFreeI}
      If N_Saved_WaveFns = WaveFnHistLevel Then
        NxtFreeI := Indx[WaveFnHistLevel]
      Else
      Begin
        j := 0;
        Repeat
          j := j + 1;
          NxtfreeI := j;
        Until NotUsed(j);
      End;
    End;  {FUNCTION NextFreeI}

    Procedure SaveAndUpdateWF (ind: INTEGER);
      Var
        id: INTEGER;
    Begin
      For id := 1 To WaveFnHistLevel - 1 Do
        Indx[WaveFnHistLevel - id + 1] := Indx[WaveFnHistLevel - id];
      Data[0].WFnFlg := TRUE;
      WaveFnData[ind].PID := PotIDNo;
      WaveFnData[ind].Color := WaveFnData[0].Color;
      WaveFnData[ind].E := WaveFnData[0].E;
      WaveFnData[ind].kact := WaveFnData[0].kact;
      WaveFnData[ind].P.Equate(1.0, WaveFnData[0].P);
      WaveFnData[ind].Re.Equate(1.0, WaveFnData[0].Re);
      WaveFnData[ind].Im.Equate(1.0, WaveFnData[0].Im);
      indx[1] := ind;
      N_Saved_WaveFns := N_Saved_Wavefns + 1;
      If N_Saved_WaveFns > WaveFnHistLevel Then
        N_Saved_WaveFns := WaveFnHistLevel;
    End; {SaveAndUpdateWF}

    Procedure Calc_Wave_Fn (kred: REAL;
                    BandNo: INTEGER);
      Var
        k_desired, tkred, ktmp, dk, ndk, Q, k_actual: REAL;
        xs, dx: REAL;
        t_a, d_t, terr, derr, err2, one, drmd: REAL;
        jbot, jtop, j_closest, im, npts, is, imid, idx, tband: INTEGER;
        y1real, y1imag: DVector;
        AbyB, A2byB: Complex;
        a11, a12, a21, a22, DetErr: Complex;
        Y1, Y2, eik, DY1, Err, Ztmp, Z1tmp, Z2tmp, Psi: Complex;

    Begin

      If ((BandNo Mod 2) = 0) Then
        k_desired := -Abs(kred) + (BandNo Div 2) * 2
      Else
        k_desired := +Abs(kred) + (BandNo Div 2) * 2;
                             {value of j at bottom of band no.= BandNo}
      jbot := (BandNo - 1) * Num_Pts_per_Band + 1;
      jtop := BandNo * Num_Pts_per_band;
      If abs(kred) < 0.5 Then
      Begin
        j_closest := jbot;
        ktmp := BandNo - 1;
      End
      Else
      Begin
        j_closest := jtop;
        ktmp := BandNo;
      End;
      dk := Abs(ktmp - k_desired);
      For im := jbot + 1 To jtop Do
      Begin
        k.Get(im, ktmp);
        ndk := Abs(ktmp - k_desired);
        If ndk < dk Then
        Begin
          dk := ndk;
          j_closest := im;
        End;
      End;
{The values of Q at the bottom and top of the band were extrapolated values to}
{give good accurate values for the band edges.  However, the calculation of the}
{wave fn. is very sensitive at the BZ boundary and hence found more reliable}
{results are obtained if use the first k away from the band edge:}
      If j_closest = jbot Then
        j_closest := j_closest + 1;
      If j_closest = jtop Then
        j_closest := J_closest - 1;
      E.Get(j_closest, Q);
      k.Get(j_closest, k_actual);
      Q := Sqrt(Q) * pi;
      Numerov_Init_once;
      Calc_t_and_delta(Q, V0, VPotPrm, abs_t, delta, yR, yI, A);
{Debug code section:}
      If MainDBFlg Then
      Begin
        Check_Square_Barrier(Q, V0, VPotPrm[1], t_a, d_t);
        terr := abs_t - t_a;   {should be zero}
        derr := delta - d_t;   {should be zero}
        err2 := cos(k_actual * pi) - (1 / abs_t) * cos(Q + delta);  {should be zero}
        one := sqr(abs_r) + sqr(abs_t);                   {should be 1.0}
        drmd := deltar - delta;        {should be +- pi/2 Modulo 2*pi}
      End;  {If MainDBFlg THEN}
{end Debugging lines}
      y1.Re := yR.value(3);
      y1.Im := yI.Value(3);
      DY1.Re := NumerovDeriv(3, h, A, yR);
      DY1.Im := NumerovDeriv(3, h, A, yI);
      Y2.Re := cos(Q / 2);
      Y2.Im := sin(Q / 2);
      eik.Re := cos(k_actual * pi);
      eik.Im := sin(k_actual * pi);
      CxOp(Y1, '*', eik, Ztmp);
      CxOp(Y2, '-', Ztmp, a11);
      CxOp(Y2, '*', eik, Ztmp);
      CxOp(Y1, '-', Ztmp, a12);
      CxOp(a12, '/', a11, Ztmp);
      RCprod(-1.0, Ztmp, AbyB);
{Note:}
{   The wave fn Psi(x) = A Psi1(x) + B Psi1(-x)   where Psi1(x) is found by}
{   integrating Shrodinger's equation for scattering from single barrier.}
{   A and B are found using the Bloch condition: Psi(x+a) = exp(ika) Psi(x)}
{   and the derivative of this condition.  Applying the Bloch conditions}
{   at the boundaries of the unit cell of a single barrier potential gives}
{   a set of equations for A and B:}
{                   a11 A + a12 B = 0}
{                   a21 A + a22 B = 0.}
{   The condition that we have a nontrivial solution is that the determinant}
{   a11 a22 - a12 a21 = 0.  It is this condition that gives connection between}
{   Q and k where Q*Q = E, i.e. the condition cos ka = (1/|t|)cos(Q*Q + delta).}
{   Thus, this is what gives the dispersion relations for the E(k).  To find}
{   the wave function we must use the Q and k that satisfy the condition that}
{   makes the determinant zero and then  find the resulting a11,a12,... to find}
{   A and B.  Actually, there is always an arbitrary phase factor so it is}
{   enough to determine A/B.  So can get this from either equation.}
{      A/B = - a12/a11   and   A/B = - a22/a21.    Both of these should give}
{   the same value for A/B  (of course everything is complex).}
{   This was used in debugging and checking the overal validity of the whole}
{   program.}

{Debugging and error checking:}
      If MainDBFlg Then
      Begin
        Z2tmp.Re := 0.0;
        Z2tmp.Im := Q;
        CxOp(Z2tmp, '*', Y2, Ztmp);
        CxOp(Ztmp, '*', eik, Z2tmp);
        CxOp(Z2tmp, '-', DY1, a22);
        CxOp(DY1, '*', eik, Z2tmp);
        CxOp(Ztmp, '-', Z2tmp, a21);
        CxOp(a22, '/', a21, Ztmp);
        RCprod(-1.0, Ztmp, A2byB);    {The complex coefficients AbyB and A2byB}
        CxOp(a11, '*', a22, Z1tmp);    {should be equal if all is well.}
        CxOp(a12, '*', a21, Z2tmp);
        CxOp(Z1tmp, '-', Z2tmp, DetErr);  { The complex no. DetErr should be zero}
      End;
{end Debugging lines}
      npts := Numerov_Pts - 3;
      dx := 1 / (npts - 1);
{For Debuging}
      If MainDBFlg Then
        If DBLevel2Flg Then
        Begin
{MAC}
{          New(y1real);}
{          New(y1imag);}
{CAM}
          y1real.init(npts);
          y1imag.init(npts);
        End;
{ end debugging lines}
      imid := (Npts Div 2);
      For im := 1 To npts Do
      Begin
        xs := -1 / 2 + (im - 1) * dx;
        Y1.Re := yR.Value(im + 2);
        Y1.Im := yI.Value(im + 2);
        Y2.Re := yR.Value(Numerov_Pts - im);
        Y2.Im := yI.Value(Numerov_Pts - im);
        CxOp(AbyB, '*', Y1, Ztmp);
        CxOp(Ztmp, '+', Y2, Z1tmp);
        Psi := Z1tmp;
{For Debugging:}
{Use the following set of lines if use unit cell where potential looks}
{   like a barrier; This is done if MainDBFlg is TRUE.}
        If MainDBFlg Then
        Begin
          WaveFnData[0].Re.Put(im, z1tmp.re);
          WaveFnData[0].Im.Put(im, z1tmp.im);
          WaveFnData[0].P.Put(im, Sqr(CAbs(Z1tmp)));
          If DBLevel2Flg Then
          Begin
            y1real.put(im, yR.Value(im + 2));
            y1imag.put(im, yi.value(im + 2));
          End;
          WFx.Put(im, xs);
        End;
{Normally, we want the wavefunction plotted in the usual unit cell}
{ where the potential looks like a well.}
        If (Not MainDBFlg) Then
          If im < imid + 1 Then
          Begin
            xs := xs + 1 / 2;
            is := npts - imid + im;
            WaveFnData[0].Re.Put(is, Psi.re);
            WaveFnData[0].Im.Put(is, Psi.im);
            WaveFnData[0].P.Put(is, Sqr(CAbs(Psi)));
            WFx.Put(is, xs);
          End
          Else
          Begin
            Ztmp.Re := eik.Re;
            Ztmp.Im := -eik.Im;
            CxOp(Psi, '*', Ztmp, Psi);
            xs := xs - 1 / 2;
            is := im - imid;
            WaveFnData[0].Re.Put(is, Psi.re);
            WaveFnData[0].Im.Put(is, Psi.im);
            WaveFnData[0].P.Put(is, Sqr(CAbs(Psi)));
            WFx.Put(is, xs);
          End;
      End;  {If .. ELSE  that shifts to usual unit cell}
      Ktmp := 0.0;
      For im := 1 To npts Do
        ktmp := ktmp + WaveFnData[0].P.Value(im);
      ktmp := ktmp / npts;
      WaveFnData[0].P.MultScalar(1 / ktmp);
      WaveFnData[0].Re.MultScalar(1 / sqrt(ktmp));
      WaveFnData[0].Im.MultScalar(1 / sqrt(ktmp));
      selectviewport(Evskvp);

      If HistoryFlg Then
      Begin
        idx := NxtFreeI;
        WaveFnData[0].color := Ecoltable[idx Mod 6];
        WaveFnData[0].E := Q * Q / (Pi * PI);
        If kred>=0 Then
        Begin
          If (BandNo MOD 2)=0 Then
            WaveFnData[0].kact := -k_actual
          Else
            WaveFnData[0].kact := k_actual;
        End
        Else
        Begin
          If (BandNo MOD 2)=0 Then
            WaveFnData[0].kact := k_actual
          Else
            WaveFnData[0].kact := -k_actual;
        End;
        DrawSavedWFns(WFNvp);
        SaveAndUpdateWF(idx);
      End
      Else
      Begin
        idx := 1;
        SetWFViewPort(WFNvp);
      End;

      SetColor(PsiSqColor);
      Case WFnPltType of
        0:
        Begin
          PlotDVectors(WFx, WaveFnData[0].P, 1, npts);
          PlotSymbol(WFx.Value(ixlabel),
                     WaveFnData[0].P.Value(ixlabel),
                     bandchar[indx[1]]);
        End;
        1:
        Begin
          PlotDVectors(WFx, WaveFnData[0].Re, 1, npts);
          PlotSymbol(WFx.Value(ixlabel),
                     WaveFnData[0].Re.Value(ixlabel),
                     bandchar[indx[1]]);
        End;
        2:
        Begin
          PlotDVectors(WFx, WaveFnData[0].Im, 1, npts);
          PlotSymbol(WFx.Value(ixlabel),
                     WaveFnData[0].Im.Value(ixlabel),
                     bandchar[indx[1]]);
        End;
      End;  {Case}
      Selectviewport(Evskvp);
      selectScale(ZonePlot);
      If ZonePlot = 2 Then
        plotSymbol(k_actual, Q * Q / (Pi * Pi), bandchar[idx])
      Else
      Begin
        Reducedk(WaveFnData[0].kact,ktmp,BandNo);
        plotSymbol(ktmp, Q * Q / (Pi * Pi), BandChar[idx]);
      End;
      If MainDBFlg Then
{Draw the shifted unit cell showing the potential as a barrier:}
      Begin
        OpenViewPort(5);
        DrawVPot(V0);
      End;
      If MainDBFlg Then
      Begin
        SetColor(Green);
        SelectScale(7);
        PlotDvectors(WFx, WaveFnData[0].Re, 1, npts);
        setColor(Red);
        PlotDvectors(WFx, WaveFnData[0].Im, 1, npts);
        If DBLevel2Flg Then
        Begin
          setcolor(lightblue);
          PlotDvectors(WFx, y1real, 1, npts);
          setcolor(yellow);
          PlotDvectors(WFx, y1imag, 1, npts);
        End;
        SelectViewport(Evskvp);
        SetColor(Whitte);
{PC}
      Rubout(1,3,25,GraphBackColor);
      Print(1,3,'k/pi='+NumStr(k_actual,10,5));
      Rubout(1,4,25,GraphBackColor);
      Print(1,4,'E(k)='+NumStr(Q*Q/(pi*pi),10,5));
      Rubout(1,5,25,GraphBackColor);
      Print(1,5,'|A/B|='+NumStr(CAbs(AbyB),10,5));
      Rubout(1,6,25,GraphBackColor);
      Print(1,6,'Arg(A/B)='+NumStr(Arg(AbyB)/pi,10,5)+'pi');
      Rubout(1,7,25,GraphBackColor);
      Print(1,7,'|a/b|2='+NumStr(CAbs(A2byB),10,5));
      Rubout(1,8,25,GraphBackColor);
      Print(1,8,'Arg(A/B)2='+NumStr((Arg(A2byB))/pi,10,5)+'pi');
      Rubout(1,9,25,GraphBackColor);
      Print(1,9,'|a/b|Err='+NumStr((CAbs(A2byB)-CAbs(AbyB)),10,5));
      Rubout(1,10,25,GraphBackColor);
      Print(1,10,'Arg(A/B)Err='+NumStr((Arg(A2byB)-Arg(AbyB))/pi,10,5)+'pi');
      Rubout(1,11,25,GraphBackColor);
      Print(1,11,'DetErr='+NumStr(CAbs(detErr),10,5));
      Rubout(1,12,25,GraphBackColor);
      Print(1,12,'tErr='+NumStr(terr,10,5));
      Rubout(1,13,25,GraphBackColor);
      Print(1,13,'deltaErr='+NumStr(derr/pi,10,5)+'pi');
      Rubout(1,14,25,GraphBackColor);
      Print(1,14,'coska-F(Q)='+NumStr(err2,10,5));
      Rubout(1,15,25,GraphBackColor);
      Print(1,15,'t^2 + r^2='+NumStr(one,10,5));
      Rubout(1,16,25,GraphBackColor);
      Print(1,16,'deltar-delta='+NumStr(drmd/pi,10,5)+'pi');
{CP}
      End;  {IF MainDBFlg THEN}
{   PressKeyMessage;     }
      SelectViewport(Evskvp);
      SetColor(EColor);
{Used for Debugging}
      If (MainDBFlg And DBLevel2Flg) Then
      Begin
        y1real.free;
        y1imag.free;
{MAC}
{        dispose(y1real);}
{        dispose(y1imag);}
{CAM}
      End;
    End;


    Procedure ChooseState (Var kred: REAL;
                    Var WFnkBand: INTEGER);
      Var
        ParmScreen: TInputScreen;
        tWFnkBand: INTEGER;
        tkred,k_actual,ktmp: REAL;
        tWaveFlg: BOOLEAN;
    Begin
      ScreenEscFlag := FALSE;
      tWaveFlg := WaveFlg;
{MAC}
{      new(ParmScreen);}
{CAM}
      With ParmScreen Do
      Begin
        Init;
        DefineInputPort(0.60, 0.99, 0.60, 0.95);
        LoadLine(' Display |Phi| for state  ');
        LoadLine('   labeled by:       ');
        LoadLine(' Reduced k vector:');
        LoadLine(' kred = {          }(-1<kred<1)');
        LoadLine(' Band Index = {    }');
        LoadLine('');
        LoadLine('  [  Ok  ] [ Help ] [Cancel]');
        If N_Saved_WaveFns>0 Then
          Reducedk(WaveFnData[0].kact,kred,WfnkBand);
        SetNumber(1, kred);
        SetNumber(2, WFnkBand);
        SetHelpFile('Bands.hlp', 'ChooseState Menu Help');
        AcceptScreen;
        If Canceled Then
        Begin
          ScreenEscFlag := TRUE;
          CSflg := FALSE;
{         CalcFlg:= FALSE;}
        End
        Else
        Begin
          tkred := kred;
          kred := GetNumber(1);
          tWFnkBand := WFnkBand;
          WFnkBand := Round(Abs(GetNumber(2)));
          If WFnkBand > NBands Then
            WFnkBand := NBands;
          If WFnkBand < 1 Then
            WFnkBand := 1;
          If ABS(kred) > 1.0 Then
          Begin
            If kred>0 Then kred := 1.0 Else kred := -1.0;
          End;
          ClearMKBuffers;
          WaveFlg := TRUE;
          CSFlg := TRUE;
        End;
        done;
      End; {WITH}
{MAC}
{      dispose(ParmScreen);}
{CAM}
    End; {ChooseState}

    Procedure GetEandk (k1, E1: REAL;
                    Var Kout: REAL;
                    Var Bandout: INTEGER);
      Var
        imx: INTEGER;
        tk: REAL;
    Begin
   {First find Bandnumber}
      imx := 1;
      While ((E1 > Etop[imx]) And (imx < PltBandMax)) Do
        imx := imx + 1;
      If imx = 1 Then
        BandOut := imx
      Else
      Begin
        If E1 > Ebot[imx] Then
          BANDOut := imx
        Else
        Begin
          If (Abs(E1 - Etop[imx - 1]) < Abs(E1 - Ebot[imx])) Then
            BandOut := imx - 1
          Else
            BandOut := imx;
        End;
      End;
   {Now have band number}
   {Note that in the units used here}
{                             ( k in units of pi/a)}
{                             the shortest Reciprocal lattice vector = 2}
      If ZonePlot = 2 Then
      Begin
        If (Abs(k1) < (BandOut - 1)) Then
          k1 := (BandOut - 1); {Take care of}
        If (Abs(k1) > BandOut) Then
          k1 := BandOut;        {absurd k values}
        kOut := {Abs}(Abs(k1) - (BandOut Div 2) * 2);    {calc. reduced k}
      End;

      If (ZonePlot = 1) Then
      Begin
        kOut := k1;
      End;

      If ZonePlot = 3 Then
      Begin
        tk := k1;
        kOut:= k1;
        While Abs(kOut) > 1 Do
          If tk>0 Then
            kOut := kOut -  2
          Else
            kOut := kOut +  2;
      End;

    End;


    Var
      InFlg: BOOLEAN;
      tk, tE: REAL;
  Begin {WaveFnSubProg}
    Repeat
      If MFlg Then
        GetEandk(kin, Ein, WFnkred, WfnkBand);
      If CSFlg Then
        ChooseState(WFnkred, WFnkBand);
      If MFlg Or CSFlg Then
      Begin
        Calc_Wave_Fn(WFnkred, WFnkBand);
        MFlg := false;
        CSFlg := false;
        ClearMKBuffers;
      End;
      CheckForEvents;
      If VPotSliders.Changed Then
        sFlag := true
      Else If sFlag Then
      Begin
        HandleVPotSliders;
        sFlag := false;
      End;
      If (BandHotKeys1.Pressed(key)) Then {AND (key<>0)}
      Begin
        HandleHotKeys1(key);
      End;
      If BandMenu.Activated Then
      Begin
        HandleBandMenu;
      End;
      If ClickedInside(Evskvp) Then
      Begin
        MousePosn(kin, Ein, Evskvp, ZonePlot, InFlg);
        MFlg := TRUE;
        WaveFlg := TRUE;
      End;
      If CalcFlg Then
        WaveFlg := FALSE;
      If N_Saved_WaveFns>0 Then
        BandMenu.RowActivate(5,9,TRUE)
      Else
        BandMenu.RowActivate(5,9,FALSE);
    Until (Not WaveFlg);
  End;

(*{$S Three Seg}*)

  Procedure MulticellWFnPlot;
    Var
      N,i,j,jj,Ncells,kk: INTEGER;
      dx,xr,twfn,tk: REAL;
      x,ReWFn,ImWFn,SqWFn: DVector;
  Begin
    N:= 555;
    Ncells:= 15;
    dx:= Ncells/(N-1);
{MAC}
{    New(x);}
{    New(ReWFn);}
{    New(ImWFn);}
{    New(SqWFn);}
{CAM}
    x.init(N);
    ReWFn.init(N);
    ImWFn.init(N);
    SqWFn.init(N);
    ClearMuppetPort;
    SetColor(AxisLabelColor);
    OpenViewPort(13);
    PutLabel(bottom, 'x (in units of lattice constant a)');
    PutLabel(left, 'Im{Psi}');
    OpenViewPort(14);
    PutLabel(left, 'Re{Psi}');
    OpenViewPort(15);
    PutLabel(top, 'Bloch Wavefunction |Psi|^2, Re{Psi}, and Im{Psi}');
    PutLabel(left, '|Psi|^2');

    For kk:= 1 to N_Saved_Wavefns do
    Begin
      tk:= Abs(WaveFnData[indx[kk]].kact);
      For i:=1 to N Do
      Begin
        xr:= -0.5 + dx*(i-1);
        x.Put(i,xr);
        j:= Trunc(xr+0.5);
        jj:= Round((xr + 0.5 - j)*(Numerov_Pts-4)) + 1;
        twfn:= cos(j*tk*pi)*WaveFnData[indx[kk]].Re.Value(jj)
                - sin(j*tk*pi)*WaveFnData[indx[kk]].Im.Value(jj);
        ReWFn.Put(i,twfn);
        twfn:= cos(j*tk*pi)*WaveFnData[indx[kk]].Im.Value(jj)
                + sin(j*tk*pi)*WaveFnData[indx[kk]].Re.Value(jj);
        ImWFn.Put(i,twfn);
        twfn:= WaveFnData[indx[kk]].P.Value(jj);
        SqWFn.Put(i,twfn);
      End;
      DefineScale(13,-0.5,NCells,-2.5,+2.5);
      SelectViewPort(13);
      SelectScale(13);
      Axis(0,0,1.0,1.0);
      SetColor(WaveFnData[indx[kk]].color);
      PlotDvectors(x, ImWFn, 1,N);
      SelectViewPort(14);
      Axis(0,0,1.0,1.0);
      SetColor(WaveFnData[indx[kk]].color);
      PlotDvectors(x,ReWFn,1,N);
      DefineScale(14,-0.5,NCells,0,3);
      SelectViewPort(15);
      Axis(0,0,1.0,1.0);
      SelectScale(14);
      SetColor(WaveFnData[indx[kk]].color);
      PlotDvectors(x,SqWFn,1,N);
    End;
    x.free;
    ReWFn.free;
    ImWFn.free;
    SqWFn.free;
{MAC}
{    Dispose(x);}
{    Dispose(ReWFn);}
{    Dispose(ImWFn);}
{    Dispose(SqWFn);}
{CAM}
    PressKeyMessage;
    ClearMuppetPort;
    BandMenu.Display;
    BandHotKeys1.Display;
  End;

  Procedure HandleBandMenu;
    Var
      i, tNBands, tNum_pts_per_band, tPotIDNo: INTEGER;
      tempEScalemax, tV0: REAL;
      tempEtop, tempEbot: BandEdgeArray;
      korig, Eorig: DVector;
      tvPotPrm: ArrayofPotPrms;
      HistoryFlgorig: BOOLEAN;

    Procedure WFnPltHndl;
    Begin
      If Not WaveFlg Then
      Begin
        WaveFlg := TRUE;
        CalcFlg := FALSE;
        MFlg := FALSE;
        CSFlg := TRUE;
        WaveFnSubProg(0.0, 0.0, MFlg, CSFlg, WaveFnData);
      End
      Else
        CalcFlg := FALSE;
      CSFlg := TRUE;
    End; {Procedure WFnPltHndl}
  Begin
    With BandMenu Do
      Case colChosen Of
        1:
        Begin
          CalcFlg := FALSE;
          Case rowChosen Of
            1:
              AboutCups;
            2:
              Credits;
            3:
              GenIntro;
            4:
              HelpTopMenu;
            5:
              Configuration;
            7:
            Begin
              WaveFlg := FALSE;
              QUITflg := TRUE;
              CalcFlg := FALSE;
            End;
          End; {CASE}
        End;
        2:
        Begin
          tV0 := V0;
          For i := 1 To NumVPotPrms Do
            tVPotPrm[i] := VPotPrm[i];
          Case PotIDNo Of
            0:
              InitKronigPenneyPot(V0, SqWellWidth);
            1:
              InitSqWellPot(V0, SqWellWidth);
            2:
              ChangeV0(V0);
            3:
              InitCosinePot2(V0, B);
            4:
              ChangeV0(V0);
            5:
              InitLinearPot(V0, FBW);
            6:
              InitUserDefPot(V0, UserParam1);
          End; {CASE}
          CalcFlg := FALSE;
          If tV0 <> V0 Then
            CalcFlg := TRUE;
          If NumVPotPrms > 0 Then
            For i := 1 To NumVPotPrms Do
            Begin
              If tVPotPrm[i] <> VPotPrm[i] Then
              Begin
                VPotSliders.ReSet((i + 1), VPotPrm[i]);
                CalcFlg := TRUE;
                NeedNewEgvsV0 := TRUE;
              End;
            End;
        End;
        3:
        Begin
          tV0 := V0;
          tPotIDNo := PotIDNo;
          For i := 1 To NumVPotPrms Do
            tVPotPrm[i] := VPotPrm[i];
          Case rowChosen Of
            1:
              InitKronigPenneyPot(V0, SqWellWidth);
            2:
              InitSqWellPot(V0, SqWellWidth);
            3:
              InitCosinePot(V0);
            4:
              InitCosinePot2(V0, B);
            5:
              InitHarmOscPot(V0);
            6:
              InitLinearPot(V0, FBW);
            7:
              InitUserDefPot(V0, UserParam1);
          End; {CASE}
          CalcFlg := FALSE;
          If tPotIDNo <> PotIDNo Then
          Begin
            CalcFlg := TRUE;
            NeedNewEgvsV0 := TRUE;
            If NumVPotPrms > 0 Then
            Begin
              SetVPotSliders;
              VPotSliders.Drawall;
            End;
          End;
          If tV0 <> V0 Then
            CalcFlg := TRUE;
          i := 1;
          While ((Not CalcFlg) And (i <= NumVPotPrms)) Do
          Begin
            If tVPotPrm[i] <> VPotPrm[i] Then
            Begin
{mk0}              VPotSliders.ReSet((i + 1), VPotPrm[i]);
              CalcFlg := TRUE;
              NeedNewEgvsV0 := TRUE;
            End;
            i := i + 1;
          End;
{                 IF VPotPrm[1]<>tVPotPrm[1] THEN VPotSliders.ReSet(2,VPotPrm[1]);}
          If (Not WrtFlg) Then
            DrawVPot(V0);
{                 ClearMKBuffers;}
        End;
        4:
        Begin
          CalcFlg := TRUE;
          Case rowChosen Of
            1:
            Begin
              ZonePlot := 1;
              CalcFlg := FALSE;
              RowChecked(4,1,TRUE);
              RowChecked(4,2,FALSE);
              RowChecked(4,3,FALSE);
            End;
            2:
            Begin
              ZonePlot := 2;
              CalcFlg := FALSE;
              RowChecked(4,1,FALSE);
              RowChecked(4,2,TRUE);
              RowChecked(4,3,FALSE);
            End;
            3:
            Begin
              ZonePlot := 3;
              CalcFlg := FALSE;
              RowChecked(4,1,FALSE);
              RowChecked(4,2,FALSE);
              RowChecked(4,3,TRUE);
            End;
            5:
            Begin
              i := Evskvp;
              Evskvp := WFNvp;
              WFNvp := i;
              CalcFlg := FALSE;
            End;
          End; {CASE}
          If (Not CalcFlg) Then
            ShowEvsk;
        End;
        5:
        Begin
          CalcFlg := TRUE;
          Case rowChosen Of
            1:
              ReadNumBands(Nbands, PltBandMin, PltBandMax, History_Level);
            2:
            Begin
              SetWriteMode(XORput);
              If InfV0Flg Then
                DispInfV0Energy(V0, PotIDNo, ZonePlot, 1, Nbands, InfV0color);
              InfV0Flg := Not InfV0Flg;
              If InfV0Flg Then
                For i := PltBandMin To PltBandMax Do
                Begin
                  CalcInfV0Energy(V0, PotIDNo, i, InfV0Energy);
                  DispInfV0Energy(V0, PotIDNo, ZonePlot, 1, Nbands, InfV0color);
                End;
              CalcFlg := FALSE;
              SetWriteMode(CopyPut);
            End;
            3:
            Begin
              HistoryFlg := Not HistoryFlg;
              N_Saved_Data := 0;
              N_Saved_WaveFns := 0;
              indx[1] := 1;
              New_History_Record_Complete := FALSE;
              ColorIndex := 1;
              If HistoryFlg Then
              Begin
                Shift_History_Data(N_Saved_Data);
                Update_History_Data;
              End;
              CalcFlg := FALSE;
            End;
            5:
            Begin
              WFnPltType:= 0;
              RowChecked(5,5,TRUE);
              RowChecked(5,6,FALSE);
              RowChecked(5,7,FALSE);
              CalcFlg:= FALSE;
            End;
            6:
            Begin
              WFnPltType:= 1;
              RowChecked(5,5,FALSE);
              RowChecked(5,6,TRUE);
              RowChecked(5,7,FALSE);
              CalcFlg:= FALSE;
            End;
            7:
            Begin
              WFnPltType:= 2;
              RowChecked(5,5,FALSE);
              RowChecked(5,6,FALSE);
              RowChecked(5,7,TRUE);
              CalcFlg:= FALSE;
            End;
            9:
            Begin
              MultiCellWFnPlot;
              CalcFlg := FALSE;
            End;
            10:
            WFnPltHndl;
          End; {CASE}
          If ((Not CalcFlg) And (Not CSFlg)) Then
            ShowEvsk;
        End;
        6:
          Numerics(Qtol, Numerov_Pts, WrtFlg, GKPFnFlg);
        7:
        Begin
          CalcFlg := FALSE;
          tNBands := NBands;
          tNum_Pts_per_band := Num_Pts_per_band;
          tempEbot := Ebot;
          tempEtop := Etop;
          tempEScaleMax := EScalemax;
{MAC}
{          New(korig);}
{          New(Eorig);}
{CAM}
          korig.Init(Num_E_Pts);
          korig.Equate(1.0, k);
          Eorig.Init(Num_E_Pts);
          Eorig.Equate(1.0, E);
          HistoryFlgOrig := HistoryFlg;
          HistoryFlg := FALSE;

          If EnterEgvsV0Param(Vpmin, Vpmax, NumEgvsV0pts, ibpmin, ibpmax) Then
          Begin
            Egap_vs_V0(Vpmin, Vpmax, NumEgvsV0pts, ibpmin, ibpmax, logV0,
                        logEgap);
          End
          Else
            CalcFlg := FALSE;

          NBands := tNBands;
          Num_Pts_per_band := tNum_Pts_per_band;
          Ebot := tempEbot;
          Etop := tempEtop;
          EScalemax := tempEScaleMax;
          HistoryFlg := HistoryFlgOrig;
          E.Equate(1.0, Eorig);
          k.Equate(1.0, korig);
          Eorig.free;
          korig.free;
{MAC}
{          Dispose(Eorig);}
{          Dispose(korig);}
{CAM}
          ShowEvsk;
        End;
      End;
    ClearMKBuffers;
{   IF CalcFlg THEN WaveFlg:= FALSE;}
  End; {HandleBandMenu}

  Procedure HandleHotKeys1 (key: BYTE);
    Var
      dummy: Boolean;
  Begin
    Case key Of
      1:
      Begin
        HelpHotKey;
        CalcFlg := FALSE;
      End;
      2:
      Begin
        V0 := V0 / 2.0;
        If V0 < 0.1 Then
          V0 := 0.1;
        CalcFlg := TRUE;
      End;
      3:
      Begin
        V0 := V0 * 2.0;
        If V0 > MaximumV0 Then
          V0 := MaximumV0;
        CalcFlg := TRUE;
      End;
      4:
      Begin
        If (N_Saved_data < 2) Then
        Else If Data[N_Saved_Data - 1].WFnFlg Then
          N_Saved_WaveFns := 0;
        N_Saved_Data := N_Saved_Data - 1;
        If N_Saved_Data < 1 Then
          N_Saved_Data := 1;
        If N_Saved_WaveFns>1 Then
          N_Saved_WaveFns:= N_Saved_WaveFns - 1;
        ShowEvsk;
        If N_Saved_WaveFns>0 Then
          BandMenu.RowActivate(5,9,TRUE)
        Else
          BandMenu.RowActivate(5,9,FALSE);
        CalcFlg := False;
      End;
      5:
      Begin
        N_Saved_Data := 1;
        N_Saved_WaveFns := 0;
        ColorIndex := 1;
        Data[0].PltColor := NextPlotColor;
        BandMenu.RowActivate(5,9,FALSE);
        ShowEvsk;
        CalcFlg := False;
      End;
      6:
      Begin
        CheckForEvents;
        If BandMenu.Chosen Then
          HandleBandMenu;
      End;
    End; {case}
  End;


  Procedure HandleVPotSliders;
    Var
      tVPotPrm, tV0: REAL;
      i, button: integer;
  Begin
{   MOuseGlobalPosn(i,i,button);}
{   if button=0 then}
    Begin
      CalcFlg := FALSE;
      tV0 := V0;
      tVPotPrm := VPotPrm[1];
      With VPotSliders Do
      Begin
        VPotPrm[1] := Value(2);
        SetVPotLabel(PotIDNo);
   {      V0:= exp(ln(10)*Value(1));}
      End;
      Case PotIDNo Of
        0:
          SqWellWidth := VPotPrm[1];
        1:
          SqWellWidth := VPotPrm[1];
        3:
          B := VPotPrm[1];
        5:
          FBW := VPotPrm[1];
        6:
          UserParam1 := VPotPrm[1];
      End;
      If (VPotPrm[1] <> tVPotPrm) Then
      Begin
        CalcFlg := TRUE;
        NeedNewEgvsV0 := TRUE;
      End;
      If V0 <> tV0 Then
        CalcFlg := TRUE;
    End;
  End;

  Procedure Calculate;
  Begin
    Calc_Efree(Num_Pts_per_band, Nbands, Kfree, Efree);
    Calc_E_of_k(V0, Qmax, Qtol, Numerov_Pts, Nbands, Num_Pts_per_band,
                 Q, Ebot, Etop, k, E);
    If N_Saved_WaveFns>0 Then
    Begin
      BandMenu.RowActivate(5, 9, TRUE);
      DrawSavedWfns(WFNvp);
    End
    Else
    Begin
      BandMenu.RowActivate(5, 9, FALSE);
      PointAndClickMessage;
    End;
    CalcFlg := FALSE;
  End;

  Procedure One_D_Case;
    Var
      tk, tE: REAL;
      dix, diy: INTEGER;
      InFlg: BOOLEAN;
  Begin
    sFlag := false;
    QuitFlg := FALSE;
    Calculate;
    If FirstTimeFlg Then
    Begin
      Credits;
      FirstTimeFlg := FALSE;
    End;
    Repeat
      CheckForEvents;
      InFlg := FALSE;
      If VPotSliders.Changed Then
        sFlag := true
      Else If sFlag Then
      Begin
        HandleVPotSliders;
        sFlag := false;
      End;
      If (BandHotKeys1.Pressed(key)) Then {AND (key<>0)}
      Begin
        HandleHotKeys1(key);
      End;
      If BandMenu.Activated Then
      Begin
        HandleBandMenu;
      End;
      If ClickedInside(Evskvp) Then
      Begin
        MousePosn(tk, tE, Evskvp, ZonePlot, InFlg);

        WaveFlg := TRUE;
        MFlg := TRUE;
        CSFlg := FALSE;
        WaveFnSubProg(tk, tE, MFlg, CSFlg, WaveFnData);
      End;
      If (CalcFlg) Then
        Calculate;
      If N_Saved_WaveFns>0 Then
        BandMenu.RowActivate(5,9,TRUE)
      Else
        BandMenu.RowActivate(5,9,FALSE);
    Until (QuitFlg);
  End;  {One_D_Case}

(*{$S Four Seg}*)

  Procedure ReadyAll;
    Var
      i: integer;
  Begin
   {Input and Output Energies are in units of h^2/(8ma^2)}
   {The program appears to work for V0<= 10000}

    DefineViewport(1, 0.1, 0.6, 0.12, 0.88);   {regular E vs k viewport}
    DefineViewport(2, 0.1, 0.9, 0.12, 0.87);   {log(Egap) vs log(V0) vp}
    DefineViewport(3, 0.1, 0.9, 0.15, 0.85);   {Gen. Kronig-Penney Fn. }
    DefineViewport(5, 0.7, 0.97, 0.65, 0.88);  {Lattice Pot. V(x) vs x }
    DefineViewport(6, 0.7, 0.97, 0.12, 0.42);  {regular Wavefunction vp}
    DefineViewPort(8, 0.7, 0.97, 0.15, 0.40);  {point and click message}
    DefineViewPort(9, 0.2, 0.47, 0.25, 0.50);  {point and click message}
    DefineViewPort(10, 0, 1, 0.06, 0.95);      {clear full plot area vp}
    DefineViewPort(12, 0.61, 1, 0.06, 0.5);    {clear viewport 6 area}
    DefineViewPort(13,0.1,0.95,0.05,0.30);     {multicell Im[Psi]}
    DefineViewPort(14,0.1,0.95,0.35,0.60);     {multicell Re[Psi]}
    DefineViewPort(15,0.1,0.95,0.65,0.85);     {multicell |Psi|^2}

    DefineScale(6, -0.5, 0.5, 0.0, 3.0);       {|Psi|^2 scale}
    DefineScale(7, -0.5, 0.5, -2.1, 2.1);      {Re Psi and Im Psi scale}

    InitParams;
    InitUserPotParams;
    FBW := 0.25;
    B := 0.25;
    SqWellWidth := 0.500;

    V0 := DefaultV0;
    Numerov_Pts := DefaultNumerovPts;
{  IF Numerov_Pts=100 THEN Numerov_Pts:= Numerov_Pts - 1;}
    Num_E_Pts := DefaultNumEvskPts;

    Nbands := DefaultNBands;
    PltBandMin := DefaultNPltBandMin;
    PltBandMax := DefaultNPltBandMax;
    History_Level := DefaultHistoryLevel;
    WaveFnHistLevel := DefaultWaveFnHistLevel;
    ixlabel := Numerov_Pts Div 10;
    Vpmin := DefaultVpMin;
    Vpmax := DefaultVpMax;
    NumEgvsV0pts := DefaultNumEgvsV0Pts;
    ibpmin := DefaultNPltBandMin;
    ibpmax := DefaultNPltBandMax;

    bandchar := '123456789ABCDEF';
{MAC}
{    New(VB);}
{    New(A);}
{    New(yR);}
{    New(yI);}
{CAM}
    VB.Init(Numerov_Pts);
    A.Init(Numerov_Pts);
    yR.Init(Numerov_Pts);
    yI.Init(Numerov_Pts);

    For i := 1 To MaxBands Do
    Begin
{MAC}
{      New(logEgap[i]);}
{      New(logInfV0Egap[i]);}
{      New(logPerturbEgap[i]);}
{CAM}
      logEgap[i].init(NumEgvsV0pts);
      logInfV0Egap[i].init(NumVInfPts);
      logPerturbEgap[i].init(NumVPerturbPts);
    End;
{MAC}
{    New(logV0);}
{    New(logInfV0);}
{    New(logPerturbV0);}
{CAM}
    logV0.init(NumEgvsV0pts);
    logInfV0.init(NumVInfPts);
    logPerturbV0.init(NumVPerturbPts);

{MAC}
{    NEW(k);}
{    New(Efree);}
{    New(Kfree);}
{    New(E);}
{CAM}
    k.Init(Num_E_Pts);
    Efree.Init(Num_E_Pts);
    Kfree.Init(Num_E_Pts);
    E.Init(Num_E_Pts);
    For i := 0 To Max_History_Level Do
    Begin
{MAC}
{      New(Data[i].Es);}
{      NEW(Data[i].ks);}
{CAM}
      Data[i].Es.Init(Num_E_Pts);
      Data[i].ks.Init(Num_E_Pts);
    End;
{MAC}
{    New(WFx);}
{CAM}
    WFx.Init(Numerov_Pts - 3);
    For i := 0 To Max_WaveFnHistoryLevel Do
    Begin
{MAC}
{      New(WaveFnData[i].P);}
{      New(WaveFnData[i].Re);}
{      New(WaveFnData[i].Im);}
{CAM}
      WaveFnData[i].P.Init(Numerov_Pts - 3);
      WaveFnData[i].Re.Init(Numerov_Pts - 3);
      WaveFnData[i].Im.Init(Numerov_Pts - 3);
    End;

    NeedNewEgvsV0 := TRUE;

{The startup lattice potential is the square well}
    PotIDNo := 1;
    NumVPotPrms := 1;
    VPotPrm[1] := SqWellWidth;
    VPotPrmMin[1] := 0.05;
    VPotPrmMax[1] := 0.98;
    VPotPrmLabel[1] := 'Well Width';
    For i := 2 To 5 Do
      VPotPrm[i] := 0.0;
    SetVPotLabel(PotIDNo);
    MaximumV0 := SqWellMaxV0;

    FirstTimeFlg := TRUE;
    ZonePlot := 2;
    InfV0Flg := FALSE;
    EgvsV0Flg := FALSE;

    GKPFnFlg := FALSE;
    WrtFlg := FALSE;


    Etop[0] := 0.0;
    Ebot[0] := 0.0;

    Num_Pts_per_band := Num_E_Pts Div Nbands;
    For i := 1 To WaveFnHistLevel Do
      Indx[i] := 0;
    N_Saved_Data := 0;
    N_Saved_WaveFns := 0;
    New_History_record_Complete := FALSE;
    HistoryFlg := TRUE;
    Qmax := 1500 * pi;
    delQ := 0.02;
    SetUpBandMenu;
    SetUpHotKeys1;
    BandMenu.AutoChecking(3, 2);
    BandMenu.RowChecked(4, 2, TRUE);
    BandMenu.RowChecked(5, 5, TRUE);
    BandMenu.RowActivate(5, 9, FALSE);
    BandMenu.Display;
    BandHotKeys1.Display;
{MAC}
{    New(VPotSliders);}
{CAM}
    VPotSliders.Init;
    SetVPotSliders;
  End; {ReadyAll}

Begin {Main}
  CUPSinit;

  ReadyAll;

  One_D_Case;
  CUPSdone;
End.{Bands.pas}
