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

{$A-}
{$N+}
{$M 65520,0,655360}
PROGRAM tight_binding_packet;
(*MAC COMPILING:  Can NOT use EXTENDED REALS*)

(**************************************************************
*                                                             *
*      CUPS -- Solid State Physics Simulation                 *
*  Program to display the forced motion of a wave packet      *
*  in a crystal.  -----Roger Rollins                          *
*-------------------------------------------------------------*
*  Copyright (C)1991,92,93 by the CUPS project and by         *
*  R.W.Rollins, Department of Physics and Astronomy,          *
*  Ohio University, Athens, OH 45701.   All rights reserved.  *
*-------------------------------------------------------------*
*  ver. 0   April 30, 1991                                    *
*  ver. 1   May 4, 1991                                       *
*     -changed normalization units                            *
*     -option to plot Re_C_n                                  *
*  ver. 2   May 5, 1991                                       *
*     -added CenterX procedure that shifts the array to       *
*      follow the center of the packet                        *
*  ver. 4   Sept. 23, 1991                                    *
*     -added Plot Data option including comparison with       *
*      semi-classical prediction                              *
*  ver. 4.2-4.3 Sept. 29, 1991                                *
*     -incorp. CUPSgrph TMenu                                 *
*     -added Lattice Potential choices                        *
*  ver. 4.4 Oct. 17, 1991                                     *
*     -changed my rk4 from unit to a procedure                *
*  ver. 4.5 - 4.7 to Dec. 31 1991                             *
*     - reorganize, clean up, and try to bring up to CUPS     *
*       standards                                             *
*  ver. 4.8 Jan. 1, 1992                                      *
*     - added Lattice Pot. parameters menu and Hot Keys--found*
*       bug in CUPSgraph THotKeys.Pressed function and used a *
*       workaround                                            *
*  ver. 4.9 Jan. 3, 1992                                      *
*     - added automatic adjust of the number of coupled sites *
*       to fit size of the wave packet, added second set of   *
*       Hot Keys.                                             *
*  ver. 5.0 Jan. 4, 1992                                      *
*     - Used DVectors for Data storage.                       *
*       Still not using DVetors for RK4 integration because   *
*       I believe speed is very important in this program.    *
*  ver. 5.1 Jan. 6-20, 1992                                   *
*     - Added Calculation of Packet Energy as Hot Key.        *
*     - Changed to recent versions of CUPSgrph gui math       *
*  ver. 5.2 Feb. 15, 1992                                     *
*     - cleaned up appearance of initial menu                 *
*     - added F10-Top Menu hotkey (still clumsy if no mouse)  *
*  ver. 5.3 Mar. 3-5, 1992                                    *
*     - fixed error in energy of packet calculation           *
*     - add MemAvail check to set MaxData                     *
*  ver. 5.4 March 14, 1992                                    *
*     - modified to run with the CUPS utilities ver. 1.4      *
*  ver. 5.5 May 13, 1992                                      *
*     - modified to run with CUPS util. version 2.10          *
*  ver. 0.56 Aug 12, 1992                                     *
*     - renormalized version numbers to indicate pre release  *
*     - modified top menu items to conform to CUPS standards  *
*  ver. 0.57 Aug 14, 1992                                     *
*     - changed plotting of prob. amplitudes to use XOR       *
*  ver. 0.58 Aug 28, 1992                                     *
*     - mainly cosmetic changes                               *
*  ver. 0.60 Sept. 3, 1992                                    *
*     - several modifications necessary                       *
*       to use CUPS util. ver 3.0                             *
*  ver. 0.61 Oct. 27, 1992                                    *
*     - minor changes to be compatible with                   *
*       CUPS Utilities of 10-03-92.                           *
*     - legend on wave packet plot and a few minor changes    *
*       to bring in line with CUPS standardization rules.     *
*  ver. 0.62 Nov. 8, 1992                                     *
*     - added check marks on menu items chosen                *
*     - added Random barrier potential and UserDefined pot.   *
*     - limited input values to prevent bombs                 *
*     - added warning if width of packet exceeds 200 sites    *
*  ver. 0.63 Dec. 6, 1992                                     *
*     - decreased maximum value of MaxData to fix problem     *
*       with DVector larger than 64K causing freez-up oc CPU! *
*     - added Pan/Zoom to main menu and removed from hit keys *
*     - added display of width of packet (Sigma)              *
*  ver. 0.64 Dec. 28, 1992                                    *
*     - changed all time units to (h/ev)                      *
*  ver. 0.65 January 1, 1993                                  *
*     - rearranged the menu and hot key items to make room    *
*       for 'Slower' and 'Faster' hot keys to make program    *
*       compatible with lightning fast machines of the future.*
*     - rather extensive cleaning up, renaming, etc.          *
*     - added display of warnings if packet too wide.         *
*     - made the program more bullet/idiot proof.             *
*  ver. 0.66 February 21, 1993                                *
*     - fixed an error that crept into ReadEmbPotParams       *
*  ver. 0.67 May 22, 1993                                     *
*     - update for cups utilities 05/21/93 (ver. 3.4)         *
*     - by changing order and adding 'Configuration' option   *
*     - under 'File' at  the main menu.                       *
*  ver. 0.68 July 28, 1993                                    *
*     - minor changes to make mouse avaliable as a pointing   *
*       device at all times ...useful during lecture          *
*       demonstrations, etc. (Would have been useful during   *
*       the production of the promotional tape...)            *
***************************************************************)

USES printer, crt, dos, graph,
     CUPSmupp, CUPS, CUPSfunc, CUPSproc, CUPSgui, CUPSgrph;

(*******   DEFINE GLOBAL CONSTANTS, TYPES AND VARIABLES *******)

(*
Note on the reduced (dimensionless) units used throughout the program code:

     time     -  in units of (h/eV)
     distance -  in units of a (a = the lattice constant)

     Energy   -  in units of (eV)

     Applied Electric field strength, E_a - in units of (Volts/a)

     wave vector, k - in units of (pi/a)

     hopping energy, A - in units of (eV)

  In these reduced units, the semi-classical model applied to an
  electron in a tight-binding band results in oscillations (called
  Bloch oscillations) with
           Amplitude = 2A/E_a   (in units of a)
           Period = 1/E_a      (in units of h/eV)
  where A is the "hopping energy" in eV and E_a is the applied electric
  field in units of Volts/a.
*)

(*******************************************************************)
(******* Global constants and variables defined ********************)
(*******************************************************************)

CONST
   VerNo = '0.69';    {Current Version Number}
   VerDate = '06/24/94';
   Nmax = 400;       {Maximum Num. of coupled eqns. that can be integrated.}
   NumCoupledSites : INTEGER = 200;
                           {Number of atom sites covered by any electron}
                           {wave packet.  The number of coupled eqns is }
                           {equal to 2*NumCoupledSites.}
                          {Note: Serious problems appear when the width of}
                          {the packet approaches NumCoupledSites because  }
                          {boundary conditions will effect the solution.  }
   Default_NCS : INTEGER = 190;
                         {The Default_NCS is the length of the chain
                         of coupled sites initially and on reset}
      {If AutoAdjustFlg is TRUE then the program automatically changes the
      length of the chain of coupled sites to just include to wave packet.
      Default is auto-adjust ON.  If the total probability in within WingSize sites
      of either end of the coupled chain is less than LowWingProbTol then
      the length of the coupled chain is shortened by WingSize div 2 sites
      at each end.  Likewise if the wing prob. is greater than
      HighWingProbTol then the length of the coupled chain is increased.}
   WingSize : INTEGER = 10;          {See above paragraph}
   AutoAdjustFlg : BOOLEAN = TRUE;   {See above paragraph}
   LowWingProbTol : REAL = 10e-5;    {See above paragraph}
   HighWingProbTol: REAL = 10e-4;    {See above paragraph}


   BGColor       : INTEGER = DarkGray;     {Background color for plots.}
   LegendColor   : INTEGER = LightGreen;   {Axes and Legend color.}
   AxisLabelColor: INTEGER = White;
   C_nSqrColor   : INTEGER = 2;           {Note colors are not what they}
   ReC_nColor    : INTEGER = 5;           {should be when XORed with DarkGray}
   ImC_nColor    : INTEGER = 3;
   TheoryColor   : INTEGER = Yellow;
   DataColor     : INTEGER = White;
   LatPotColor   : INTEGER = LightRed;
   TimeArrowColor: INTEGER = Yellow;       {For Backward time step warning}

   TimeArrowFlg  : BOOLEAN = TRUE;         {TRUE if integrating forward}

   PlotV_embFlg  : BOOLEAN = FALSE;        {Flags used to indicate which}
   EraseV_embFlg : BOOLEAN = FALSE;        {calculated results are plotted}
   ReC_nFlg      : BOOLEAN = FALSE;
   ImC_nFlg      : BOOLEAN = FALSE;
   WriteEFlg     : BOOLEAN = FALSE;
   ChangeMenuFlg : BOOLEAN = FALSE;
   ExitFlg       : BOOLEAN = FALSE;


TYPE
   LatticeFunction = FUNCTION(i: INTEGER): DOUBLE;
   DFproc = PROCEDURE (t: REAL; VAR y,yp: DataVector);
   PotentialType = (Constant, SqrBarrier, Superlat, RandBarrier, User);

VAR
   Potential : PotentialType; {indicates the type of lattice potential in use}
   C_n   : DataVector; {The (un-normalized) probability amplitude for finding
                       the electron at site i (where i is relative to the
                       start of the current coupled chain).
                       C_n is complex:  Real part at index 2*i-1
                                        Imaginary part at index 2*i.
                       Thus, this variable holds the set of unnormalized
                       probability amplitudes that describe the wave packet.
                       Note the coupled chain of atoms need only cover the
                       the packet and the program keeps the packet very
                       nearly centered in this coupled chain.  As the packet
                       moves, this coupled chain also moves along the real
                       chain and the program keeps track of where the
                       chain starts relative to the real chain.  This allows
                       us to consider a packet in a long chain of atoms but
                       keep the number of coupled ODEs to just the number
                       of sites covered by the packet.
                       If AutoAdjustFlg is TRUE then the program also
                       changes the length of the coupled chain so it is
                       just slightly longer than the packet itself.}
                   {NOTE: Most PROCEDUREs use the variable z for C_n}
   delta_i_accum : INTEGER; {accumulated shift of the coupled chain covering}

   prob  : DataVector; {normalized C_n^2 at site i on the coupled chain}
   lastp : DataVector; {Value of prob at last drawing}
   xx    : DataVector; {Vector holding the actual site index for coupled
                       chain site i, (actually xx[i]:= delta_x_accum + i)
                       xx is used to draw the packet in the correct
                       position on the full chain of atoms}
   lastxx: DataVector; {Value of xx at last drawing}
   lastNCS  : INTEGER; {The length of the coupled chain at last draw time}
   LV_emb: DataVector; {Vector holding the value of any additional potential
                        embedded in the Lattice.  The indices are sites of
                        the COUPLED CHAIN.  The embedded Potential function
                        is used to make a superlattice, for example.}
   Re_C_n, Im_C_n : DataVector; {The Real and Imaginary parts of the
                                 normalized probability amplitudes at
                                 each site of the coupled chain}

   Time    : DVector;  {This vector contains the value of the time (in units
                        of h/eV) at each instant the wave packet was drawn
                        to the screen}
   Exp_x   : DVector;  {This vector contains the value of the Expectation
                        value of the position of the wave packet on the full
                        chain at each instant the wave packet was drawn.}
   Theo_x  : DVector;  {This vector contains the value of the Theoretical
                         Position of the wave packet calculated using
                         the Semi-classical model...so it can be compared
                         to Exp_x mentioned above.}
   ndata    : INTEGER; {Number of data saved for current run.}
   MaxData  : INTEGER; {Maximum number of points in time series data
                         points that can be saved.  This is set by the
                         program at runtime by calling the MemAvail
                         function.  The maximum it can be is 8000 because
                         of the memory limitation (of Turbo Pascal)
                         of 64K on any single array variable.}

   EmbeddedPotential: LatticeFunction;  {A fucntion defined on the full
                                         lattice chain with domain running
                                         (at least) from index -100 to 300}

   Neqn     : INTEGER; {Number of ODEs to solve (= 2*NumCoupledSites)}
   xlo, xhi : INTEGER; {Site number range displayed is from xlo to xhi}
   icenter  : INTEGER; {Closest site number to Expectation value of position
                        for the current wave packet RELATIVE to the coupled
                        chain}
   t        : REAL; {Current time since initial release of packet}
                    { NOTE: all internal times used by the program are
                     in units of h/eV  because the period of
                     the Bloch oscillations then turn out to be near
                     integer values for simple values of parameters}
   dt    : REAL; {Integration time step (h/eV) (may be negative)}
   lastt : REAL; {Time of last drawing of packet}
   NumReDraw: INTEGER;  {Number of redraws of the packet per Block osc.}
   delta_t_draw : REAL; {time interval between redrawing drawing of packet}
                        {delta_t_draw = MAX(Period/NumReDraw,dt)}
   A     : REAL; {NN coupling used in tight binding approx.(in units of eV)}
   x0,sigma0,k : REAL; {position, width, and central wave number of
                        initial wave packet of the electron}
   Ef    : REAL; {Applied Electric field (in units of Volts per lat. spacing)
                  supplying a uniform force to the electron wave packet}
   lastEf: REAL; {Ef at last drawing of packet}
   Ex    : REAL; {Current expectation value of the position of packet}
   Sigma : REAL; {Current Sqrt(variance) (width) of the wave packet}
   pmax  : REAL; {Maximum probability amplitued of current packet}
   norm0  : REAL; {Normalization const. for initial packet}
   norm  : REAL; {Normalization const. for current packet}
   perr  : REAL; {Accumulated fractional error in total probability in
                  the current wave packet compared to the initial packet
                  i. e. perr= (norm - norm0)/norm0. }
   eoff,Erange : REAL; {Offset and scale used when displaying the "applied"
                       Potential that the electron wave packet "feels" }
   Period, Amplitude : REAL; {Theoretical period and amplitude for the
                              bloch oscillations expected applying the
                              semi-classical model and assuming the applied
                              force, Ef, is constant}

   DelayTime                                  : WORD;
   key                                        : BYTE;
   CanceledFlg                                : Boolean;
   Pktmenu                                    : TMenu;
   PktHotKeys1, PktHotKeys2                   : THotKeys;

{temporary values initialized to start-up values}
CONST
   tEf : REAL = 0.0;
   tx0 : REAL = 35;
   tsigma0 : REAL = 2.0;
   tk     : REAL = 0.0;
   tA     : REAL = 1.0;
   tdt    : REAL = 0.02;
   tNumReDraw: INTEGER = 200;
   xlo_Default: INTEGER = 0;
   xhi_Default: INTEGER = 125;


Procedure AboutProgram;
var
   helpScr : HelpScrType;
begin
    HelpScr[01] := '                                                 ';
    HelpScr[02] := '        Forced Wave Packet in a Crystal    ';
    HelpScr[03] := '                                                 ';
    HelpScr[04] := '           Written by Roger Rollins ';
    HelpScr[05] := '        Dept. of Physics and Astronomy, ';
    HelpScr[06] := '       Ohio University, Athens, OH 45701        ';
    HelpScr[07] := '       (C) 1995 John Wiley and Sons, Inc.        ';
    HelpScr[08] := '                                                 ';
    HelpScr[09] := '     This simulation shows how electron wave     ';
    HelpScr[10] := '  packets in a semi-conducting crystal move      ';
    HelpScr[11] := '  under the influence of external forces.        ';
    HelpScr[12] := '     The problem is solved in the tight binding  ';
    HelpScr[13] := '  approx. for a one-dim. chain of atoms.         ';
    HelpScr[14] := '     The "hopping" parameter, A, is the overlap  ';
    HelpScr[15] := '  integral coupling states on neighboring sites. ';
    HelpScr[16] := '     An electron wave packet of gaussian shape   ';
    HelpScr[17] := '  is inserted in the chain and the parameter k   ';
    HelpScr[18] := '  sets the initial energy of the packet.         ';
    HelpScr[19] := '     An electric field E can be applied to give  ';
    HelpScr[20] := '  an external force to the electron wave packet. ';
    HelpScr[21] := '  (Red line on plot represents this applied pot.)';
    HelpScr[22] := '     Coupled ODEs from the tight binding model   ';
    HelpScr[23] := '  are integrated to find the motion of packet.   ';
    HelpScr[24] := '                                                 ';
    HelpScr[25] := '  Press any key or click mouse to continue...    ';


   help(helpScr);
end;

Procedure Help1;
var
   helpScr : HelpScrType;
begin
    HelpScr[01] := ' Forced Motion of Electron Wave Packet in Crystal';
    HelpScr[02] := '                                                 ';
    HelpScr[03] := '    A 4th order Runge-Kutta method is used to    ';
    HelpScr[04] := ' solve 2*n coupled ODEs for the probability      ';
    HelpScr[05] := ' amplitude C_n of the electron at site n on a 1D ';
    HelpScr[06] := ' chain of atoms.  We are limited to n < 200.     ';
    HelpScr[07] := ' If the packet covers more than 200 sites you are';
    HelpScr[08] := ' warned that errors are accumulating.  Forces are';
    HelpScr[09] := ' exerted on the packet by adding an applied      ';
    HelpScr[10] := ' electric field or by adding a potential embedded';
    HelpScr[11] := ' in the lattice, or both.                        ';
    HelpScr[12] := 'About The Display:                               ';
    HelpScr[13] := '   C_n is the complex probability amplitude      ';
    HelpScr[14] := '    (|C_n|^2 = Probability electron is at site n)';
    HelpScr[15] := '   Time is measured in units of (h/eV)           ';
    HelpScr[16] := '   Distances are meas. in units of a= lat. const.';
    HelpScr[17] := '   <x> = expectation value of x for the packet.  ';
    HelpScr[18] := '   Sigma = width (variance) of the packet.       ';
    HelpScr[19] := '   Accu. prob. err. = [(total probability) - 1]  ';
    HelpScr[20] := '       gives an indication of numerical errors.  ';
    HelpScr[21] := '   E_a is the applied electric field (Volts/a)   ';
    HelpScr[22] := '   V_a is the electrostatic potential due to E_a.';
    HelpScr[23] := '   V_emb is any additional potential that is     ';
    HelpScr[24] := '       embedded in the lattice.                  ';
    HelpScr[25] := ' More help is found under File at the  Top Menu. ';
   help(helpScr);
end;

Procedure Help2;
var
   helpScr : HelpScrType;
begin
    HelpScr[01] := ' HELP ON BUTTONS AND HOT KEYS:                   ';
    HelpScr[02] := '   TOP BUTTONS: (Use F10 or mouse to activate)   ';
    HelpScr[03] := '    File     -- To EXIT program.                 ';
    HelpScr[04] := '    Params   -- Menu to change parameters.       ';
    HelpScr[05] := '    Run      -- Starts time evolution of packet. ';
    HelpScr[06] := '      Go     -- Begin numerical integration.     ';
    HelpScr[07] := '      Reset  -- Returns packet to initial cond.  ';
    HelpScr[08] := '    Options  -- Program or display options       ';
    HelpScr[09] := '      Reverse Time arrow -- Integrate forward or ';
    HelpScr[10] := '             backward in time.                   ';
    HelpScr[11] := '      Auto-Adjust Mode -- toggles the automatic  ';
    HelpScr[12] := '             adjustment of the no. coupled sites.';
    HelpScr[13] := '      Display Packet Energy -- toggles the disp. ';
    HelpScr[14] := '             of the expectation value of Energy  ';
    HelpScr[15] := '    Plot What-- Choose functions to be plotted:  ';
    HelpScr[16] := '      |C_n|^2.. Probability electron is at site n';
    HelpScr[17] := '      ReC_n  .. Real Part of Prob. Amplitude, C_n';
    HelpScr[18] := '      ImC_n  .. Imaginary part of C_n.           ';
    HelpScr[19] := '      Position vs. time for wave packet.         ';
    HelpScr[20] := '    Embedded V -- Choose embedded potential fn.  ';
    HelpScr[21] := '    Pan/Zoom -- Changes x-axis scale limits      ';
    HelpScr[22] := '   FUNCTION ''HOT'' KEYS:                        ';
    HelpScr[23] := '    -F2 Start/Stop integration.                  ';
    HelpScr[24] := '    -F3 Adds an idle time after redrawing packet.';
    HelpScr[25] := '    -F4 Removes idle time after redrawing packet.';
   help(helpScr);
end;

(*--------------------------------------------------------*)
(*-----            Physics Procedures                -----*)
(*--------------------------------------------------------*)

{Functions that give an added position dependent potential energy:
   The domain of i for these functions should be at least from
   i = -400 to i = 400  with the viewed region usually from 0 to about 120.}
     {---------------------}
(*CONST
   ubot: REAL = -2.0;
   utop: REAL = 2.0;
   uwidth: INTEGER = 80;

FUNCTION UserDefinedPotential(i:INTEGER): REAL; FAR;
{User definde potential function}
{The domain of the function should be from -400 to 400 or more}
   BEGIN

  { User defined potential goes here }
{      UserDefinedPotential:= 0.0}

      IF ( ( ((i DIV uWidth) + 1) MOD 2) = 0) THEN
         UserDefinedPotential:= ubot+ (i MOD uWidth)*(utop-ubot)/uwidth
      ELSE
         UserDefinedPotential:= utop - (i MOD uWidth)*(utop-ubot)/uwidth;


   END;  {FUNCTION UserDefinedPotential}
*)
FUNCTION UserDefinedPotential(i:INTEGER): REAL; FAR;
{User definde potential function}
{The domain of the function should be from -400 to 400 or more}
   BEGIN

   { User defined potential goes here }

      UserDefinedPotential:= 0.0

   END;  {FUNCTION UserDefinedPotential}

    {----------------------}
FUNCTION ConstantPotential(i:INTEGER): REAL; FAR;
{constant lattice Potential Energy}
   BEGIN
     ConstantPotential:= 0.0;
   END; {Procedure ConstantPotential}
     {---------------------}
CONST {Global constants defined for SquareBarrier}
      BarrierHeight : REAL = 3.0;
      BarrierWidth  : INTEGER = 10;
      BarrierX0     : INTEGER = 50;
FUNCTION SquareBarrier(i:Integer): REAL; FAR;
   {square barrier}
   BEGIN
      IF ((i<BarrierX0) OR (i>(BarrierX0+BarrierWidth))) THEN
         SquareBarrier:= 0.0
      ELSE
         SquareBarrier:= BarrierHeight;
   END; {FUNCTION SquareBarrier}
     {---------------------}
CONST {Global constants defined for SuperLattice}
     SLEtop : REAL = 0.375;
     SLEbot : REAL = -0.375;
     SLWidth: INTEGER = 1;
FUNCTION SuperLattice(i: INTEGER): REAL; FAR;
{superlattice crystal potential}
   BEGIN
      IF ( ( ((i DIV SLWidth) + 1) MOD 2) = 0) THEN SuperLattice:= SLEbot
      ELSE SuperLattice:= SLEtop;
   END; {SuperLattice}
     {---------------------}
var RandX: ARRAY [-400..400] of REAL;
CONST Iseed:INTEGER= -1;
      RandBarrierProb: REAL = 0.2;
      RandBarrierHeight: REAL = 0.5;
FUNCTION RandBarrierPotential(i:INTEGER): REAL; FAR;
   BEGIN
   IF RandX[i]<=(1-RandBarrierProb) THEN
      RandBarrierPotential:= 0.0
   ELSE
      RandBarrierPotential:= RandBarrierHeight;
   END;  {FUNCTION RandBarrierPotential}

    {----------------------}



   {---------------------------------------------}

PROCEDURE MakeWavePacket(VAR icenter: INTEGER; Xwidth: REAL;
                         VAR norm, pmax, Ex, Sigma: REAL; VAR z, p: DataVector);

  VAR
     xn,amp: REAL;
     i      : INTEGER;

    {--------Functions used in MakeWavePacket--------}

    FUNCTION f(x, sigmax: REAL): REAL;
      {f(n) is the probability the electron is found at site n}
    VAR a: REAL;
    BEGIN
       a:= 1/(sigmax*Sqrt(2*pi)) ;
       f:= a*exp(-(Sqr(x/sigmax)/2));
    END;
          {-----------}
    FUNCTION ReC(x, k, xw:REAL):REAL;
       {Calculates REAL part of c(k)}
    BEGIN
      ReC:= Sqrt(f(x,xw))*cos(k*pi*x);
    END; {ReC}


    FUNCTION ImC(x, k, xw:REAL):REAL;
       {Calculates imaginary part of c(k)}
    BEGIN
      ImC:= Sqrt(f(x,xw))*sin(k*pi*x);
    END; {ImC}


  BEGIN   {MakeWavePacket}
    norm:= 0.0;
    Ex:=0.0;
    FOR i:=1 TO NumCoupledSites  DO
    BEGIN      {Calculate wave function and probability}
       xn := i-icenter;
       z[2*i-1]:= ReC(xn,k,sigma0);
       z[2*i]:=ImC(xn,k,sigma0);
       p[i]:=(sqr(z[2*i-1])+sqr(z[2*i]));
       norm:= norm + p[i];
       Ex:= Ex + i*p[i];
    END;
    pmax:= 0.0; Sigma:= 0.0;
    Ex:= Ex/norm;
    FOR i:=1 TO NumCoupledSites DO
    BEGIN
       p[i]:= p[i]/norm;
       Sigma := Sigma + Sqr(i-Ex)*p[i];
       IF (p[i]>pmax) THEN  pmax:= p[i];
    END;
    Sigma:= Sqrt(Sigma);
    icenter := round(Ex);
  END; {MakeWavePacket}

     {----------------------------------------------}

PROCEDURE SemiClassicalModel(k,x0,A,Ef,Amplitude: REAL;
                             VAR Time, Theo_x: DVector);
    {Calculates the position of the wave packet for constant applied
     electric field Ef using the semi-classical model}
   VAR i: INTEGER;
   BEGIN
      FOR i:=1 TO ndata DO
      BEGIN
         IF Ef<>0 THEN
            Theo_x.put(i, x0 + Amplitude*(cos(pi*k)
                               - cos(pi*k+Ef*2*pi*Time.Value(i))) )
         ELSE
            Theo_x.put(i, x0 + 2*A*sin(pi*k)*Time.Value(i)*2*pi);
      END;
   END; {SemiClassicalModel}
{---------------------------------------}
FUNCTION PktE_free(VAR prob,z: DataVector;
                       norm: REAL; ncs: INTEGER): REAL;
   {Returns the expectation value of the energy of the packet
    (neglecting added potential energy)
    (i. e. energy of free packet) relative to the bottom of the tight-binding
     band.}
VAR i: INTEGER;
    enrgy: REAL;
BEGIN
   enrgy:= 0.0;
   FOR i:=2 TO ncs DO
      enrgy:= enrgy - 2*A*z[2*i-3]*z[2*i-1] - 2*A*z[2*i-2]*z[2*i];
   PktE_free:= 2.0*A + enrgy/norm;
END; {PktE_free}

{---------------------------------------}

FUNCTION PktV_emb(VAR LV_emb,prob: DataVector; ncs: INTEGER): REAL;
   {Returns the expectation value of the embedded potential energy}
VAR i: INTEGER;
    vemb: REAL;
BEGIN
   vemb:= 0.0;
   FOR i:= 1 TO ncs DO
      vemb:= vemb + LV_emb[i]*prob[i];
   PktV_emb:= vemb;
END; {PktV_emb}

{---------------------------------------}

FUNCTION PktV_app(VAR prob: DataVector; ncs: INTEGER): REAL;
VAR i: INTEGER;
    vapp: REAL;
BEGIN
   vapp:= 0.0;
   FOR i:= 1 TO ncs DO
      vapp:= vapp - Ef*(xx[i]-x0)*prob[i];
   PktV_app:= vapp;
END;
{---------------------------------------}

PROCEDURE CalcE(VAR Efree,Vemb,Vapp: REAL);
BEGIN
   Efree:= PktE_free(prob,C_n,norm,NumCoupledSites);
   Vemb:= PktV_emb(LV_emb,prob,NumCoupledSites);
   Vapp:= PktV_app(prob,NumCoupledSites);
END;

{---------------------------------------}

PROCEDURE tight_binding_dxdt(t: REAL; VAR z, dz: DataVector); FAR;

{The procedure tight_binding_dxdt gives the evaluates the first derivatives}
{for the set of differential equations.  It is called by the Runge-Kutta}
{procedure StepRK4 below.}

  VAR i,Twoi,ncs: INTEGER;
      SiteE,Twopi: REAL;

  BEGIN   {Use cyclic boundary conditions}
      ncs:= NumCoupledSites;
      Twopi:= 2*pi;
      SiteE:= LV_emb[1]-Ef*(1-icenter);
      dz[1]:= Twopi*(SiteE*z[2] - A*z[2*ncs] - A*z[4]);
      dz[2]:= Twopi*(-SiteE*z[1] + A*z[2*ncs-1] + A*z[3]);
      SiteE:= LV_emb[ncs]-Ef*(ncs-icenter);
      dz[2*ncs-1]:= Twopi*(SiteE*z[2*ncs]
                                - A*z[2*ncs-2] - A*z[2]);
      dz[2*ncs]:= Twopi*(-SiteE*z[2*ncs-1]
                                + A*z[2*ncs-3] + A*z[1]);
     FOR i:=2 TO (ncs-1) DO
     BEGIN
        Twoi:= 2*i;
        SiteE:= LV_emb[i]-Ef*(i-icenter);
        dz[Twoi-1]:= Twopi*(SiteE*z[Twoi] - A*z[Twoi-2] - A*z[Twoi+2]);
        dz[Twoi]:= Twopi*(-SiteE*z[Twoi-1] + A*z[Twoi-3] +A*z[Twoi+1]);
     END;
  END;



{--------------------------------------------------------------------}
{--------------- Menu and Input/Output Screen Procedures -------------------}
{--------------------------------------------------------------------}

PROCEDURE WriteE;
VAR
   i: INTEGER;
   E_free,V_emb,V_app,E_tot: REAL;
   tstr: String;
BEGIN
   SelectViewport(1);
   FOR i:= 2 TO 4 DO
      RubOut(57,i,7,GraphBackColor);
   SetColor(White);
   CalcE(E_free,V_emb,V_app);
   E_tot:= E_free+V_emb+V_app;
   tstr:= NumStr(E_tot,6,4);
   print(57,2,tstr);
   tstr:= NumStr(E_free+V_emb,6,4);
   print(57,3,tstr);
   tstr:= NumStr(E_free,6,4);
   print(57,4,tstr);
END;

PROCEDURE WriteELabels;
VAR tstr: String;
BEGIN
   SelectViewport(1);
   SetColor(White);
   tstr:= 'Accum. Prob. Err.=';
   print(39,1,tstr);
   tstr:= 'E_f+V_emb+V_a=';
   print(42,2,tstr);
   tstr:= 'E_f+V_emb=';
   print(46,3,tstr);
   tstr:= 'E_free=';
   print(49,4,tstr);
END;

PROCEDURE EraseELabels;
VAR i: INTEGER;
BEGIN
   SelectViewport(1);
   FOR i:=2 TO 4 DO
      RubOut(35,i,30,GraphBackColor);
END;

PROCEDURE GetScreenData(VAR tA,tEf,tx0,tsigma0,tk,tdt: REAL);
VAR ChangeScreen: TInputScreen;

BEGIN
  CanceledFlg:= FALSE;
  WITH ChangeScreen DO
     BEGIN
     Init;
     DefineInputPort(0.0,0.95,0.15,0.93);
     LoadLine('`Params` Menu: ');
     LoadLine('');
     LoadLine('  `Infinite` 1-D Lattice stretches from site -400 to 400.');
     LoadLine('   Lattice const.= a.');
     LoadLine('External Force:');
     LoadLine('   {   0.0} (Volts/a) = E_a = Applied Electric Field (Try 0.00 to 0.20).');
     LoadLine('Initial Packet:');
     LoadLine('   {  35.0} = Initial site of packet (index of center atom: 20 < n < 100).');
     LoadLine('   {   3.0} = Initial width (sigma) of wave packet: (0.7 < Delta n < 25). ');
     LoadLine('   {   0.0} (pi/a) = k_0 = Initial central wave vector of the packet.');
     LoadLine('  NOTE: For electron at bottom of band k_0 = 0 and k_0 = 1 at top of band.');
     LoadLine('Tight-Binding Model parameter:');
     LoadLine('   {   1.0} (eV) = A = Value of hopping energy. (Try 0.2 to 2)');
     LoadLine('Numerics:');
     LoadLine('   {  0.02} (units of h/eV) = Integration time step. (Keep dt<0.04)');
     LoadLine('   {   200} = no. of screen updates per Block Oscillation (20 to 400)');
     LoadLine('');
     LoadLine('                [  Ok  ]      [ Help ]      [Cancel]');
     SetNumber(1,Ef);        SetNumberLimits(1,-0.2,0.2);
     SetNumber(2,x0);
     SetNumber(3,sigma0);    SetNumberLimits(3,0.7,25);
     SetNumber(4,k);
     SetNumber(5,A);         SetNumberLimits(5,-2,2);
     SetNumber(6,dt);        SetNumberLimits(6,0,0.04);
     SetNumber(7,NumReDraw); SetNumberLimits(7,20,400);
     SetHelpFile('Packet.hlp','Params Menu Help');
     AcceptScreen;
     IF canceled THEN CanceledFlg:= TRUE;
     tEf := GetNumber(1);
     tx0 := Round(GetNumber(2));
     tsigma0 := GetNumber(3);
     tk := GetNumber(4);
     tA := GetNumber(5);
     IF ABS(tA)<0.2 THEN
      if tA=0 then tA := 0.2 else tA:= 0.2*tA/ABS(tA);
     tdt := GetNumber(6);
     IF (NOT TimeArrowFlg) THEN tdt:= -tdt;
     tNumReDraw:= Round(GetNumber(7));
     done;
     END; {WITH}
END;

PROCEDURE SetUpPktMenu;
BEGIN
   PktMenu.Init;
   WITH PktMenu DO
   BEGIN
      column(1,'File');
       row(1,1,'About CUPS');
       row(1,2,'About Program');
       row(1,3,'Configuration');
       row(1,4,'More Help ');
       row(1,5,'-------------');
       rowactivate(1,5,false);
       row(1,6,'Exit Program');
      column(2,'Params ');
      column(3,'Run/Reset');
       row(3,1,' Go');
       row(3,2,' Reset');
      column(4,'Options');
       row(4,1,'Reverse Time arrow ');
       row(4,2,'Auto-Adjust Mode');
       row(4,3,'Display Packet Energy');
      column(5,'Plot-What');
       row(5,1,' |C_n|^2');
       row(5,2,' Re{C_n}');
       row(5,3,' Im{C_n}');
       row(5,4,' Re and Im C_n');
       row(5,5,' Last data <x> vs t');
      column(6,'Embedded V');
       row(6,1,'Constant Potential');
       row(6,2,'Square Barrier');
       row(6,3,'Superlattice');
       row(6,4,'Random Barriers');
       row(6,5,'User Defined Pot.');
       row(6,6,'Change Parameters ');
      column(7,'Pan/Zoom');
       row(7,1,'Pan rt');
       row(7,2,'Pan lft');
       row(7,3,'Zoom in');
       row(7,4,'Zoom out');
       row(7,5,'Default');
   END;
END;

PROCEDURE ReadEmbPotParms(VAR BarrierHeight,SLEtop: REAL;
                          VAR BarrierWidth,BarrierX0,SLWidth: INTEGER);
VAR
   V_embParmScreen  : TInputScreen;
   n : byte;
   temp: REAL;
BEGIN
   CanceledFlg:= FALSE;
   WITH V_embParmScreen DO
      BEGIN
      Init;
      DefineInputPort (0.53, 0.95, 0.24, 0.92);
      LoadLine('Input Embedded Pot. Params');
      LoadLine('');
      LoadLine('   Square Barrier:');
      LoadLine('Height= {     } eV');
      LoadLine('Width = {     } Lat. Sites');
      LoadLine('Start at Site {   } ');
      LoadLine('');
      LoadLine('   Super Lattice: ');
      LoadLine('Amplitude   = {     } eV');
      LoadLine('Half-period = {     } Sites');
      LoadLine('');
      LoadLine('   Random barriers:');
      Loadline('Barrier Prob.  = {     } (0 to 1)');
      Loadline('Barrier Height = {     } eV');
      LoadLine('');
      LoadLine(' [  Ok  ] [ Help ] [Cancel]');
      SetNumber(1,BarrierHeight);
      SetNumber(2,BarrierWidth);
      SetNumber(3,BarrierX0);
      SetNumber(4,SLEtop);
      SetNumber(5,SLWidth);
      SetNumber(6,RandBarrierProb);
      SetNumber(7,RandBarrierHeight);
      SetHelpFile('Packet.hlp','V_embParmScreen Help');
      AcceptScreen;
      IF canceled THEN
         CanceledFlg:= TRUE
      ELSE
         BEGIN
         BarrierHeight:= GetNumber(1);          {|BarrierHeight|<=20}
         IF ABS(BarrierHeight)>20 THEN
            BarrierHeight:= 20*BarrierHeight/ABS(BarrierHeight);
         BarrierWidth:= ABS(Trunc(GetNumber(2)) );  {BarrierWidth<=200}
         IF BarrierWidth >200 THEN BarrierWidth:= 200;
         BarrierX0:= Trunc(GetNumber(3));        {|BarrierX0|< 400}
         IF ABS(BarrierX0)>400 THEN
            IF BarrierX0>0 THEN BarrierX0:=400 ELSE BarrierX0:=-400;
         SLEtop:= ABS(GetNumber(4));             {|SL_Amplitude|<=20}
         IF SLEtop>20 THEN SLEtop:= 20;
         SLEbot:= -SLEtop;
         SLWidth:= Trunc(ABS(GetNumber(5)));          {|SLWidth<=100}
         IF SLWidth>200 THEN SLWidth:=100;
         temp:= ABS(GetNumber(6));
         IF temp<=1.0 THEN RandBarrierProb:= temp;
         RandBarrierHeight:= GetNumber(7);
         END;
      done;
      END; {WITH}
   ClearMKBuffers;
   PktMenu.RowChecked(6,6,false);
   CASE Potential of
      Constant: PktMenu.RowChecked(6,1,true);
      SqrBarrier: PktMenu.RowChecked(6,2,true);
      Superlat: PktMenu.RowChecked(6,3,true);
      RandBarrier: PktMenu.RowChecked(6,4,true);
      User: PktMenu.RowChecked(6,5,true);
      END; {CASE}
END;

PROCEDURE AutoAdjustMessage(Flg:BOOLEAN);
BEGIN
   DefineViewport(3,0.79,0.99,0.05,0.10);
   OpenViewport(3);
   HideCursor;
   SetColor(Yellow);
   IF Flg THEN
      BEGIN
      PktMenu.RowChecked(4,2,true);
      Print(1,1,' AutoAdjust ON');
      END
   ELSE
      BEGIN
      PktMenu.RowChecked(4,2,false);
      Print(1,1,' AutoAdjust OFF');
      END;
   ShowCursor;
   SelectViewport(1);
END;
TYPE keystring = STRING[12];
PROCEDURE SetUpWriteE;
BEGIN
   IF WriteEFlg THEN
   BEGIN
      EraseELabels;
      WriteELabels;
      WriteE;
   END;
END;

PROCEDURE SetUpHotKeys1;
BEGIN
   WITH PktHotKeys1 DO BEGIN
      Init(5);
      key[1] := 'F1-Help';
      key[2] := 'F2-Go/Stop  ';
      key[3] := 'F3-Slower';
      key[4] := 'F4-Faster';
      key[5] := 'F10-Menu'
   END;
   PktHotKeys1.Display;
   SetUpWriteE;
   AutoAdjustMessage(AutoAdjustFlg);
END;

{-----------------------------------------------------------------------}
{------- Numerical integration procedure  ------------------------------}
{-----------------------------------------------------------------------}

PROCEDURE StepRK4(FYP: DFproc; VAR t,h: REAL; VAR Y: DataVector);
(*This is the StepRK4 procedure from CUPSmath but using MUPPET
  DataVectors rather than DVectors used in CUPSmath.  This makes the
  procedure about twice as fast. -- RWR*)
(* This procedure solves a system of up to 400 first order
differential equations  for one time step using the Runge-Kutta method
for fourth order. The main program must declare a DataVector that contains
the values of the dependent variables at time t which is passed as the
variable Y. The parameter FYP passes to RK4 the NAME of a procedure in
the main program that calculates the first derivative of the vector Y of
the dependent variables and stores the result in a DataVector YP, i.e.
dY/dt = YP.  (The actual names chosen for these DataVector's is not
important.) This procedure must be declared of type FAR as in the
following statement.

   PROCEDURE ANYNAME(t: REAL; VAR Y,YP:DataVector): FAR ;

 Input variables:
   t    - the inital value of the independent variable
   h   -  the desired increment, that is, the solution is to be calculated
          at  t + h. This variable may be negative
   Y    - the inital values of the dependent variables

 Output variables:
   t    - the value of t after the time step
   Y    - the vector of computed solutions at the new t              *)

VAR
   i,j: INTEGER;
   tt : REAL;
   Yt,YPt: DataVector;
   k: ARRAY[1..4] of DataVector;
   G: ARRAY[1..4] of REAL;
BEGIN
   G[1] := 1/2; G[2] := 1/2; G[3] := 1;
   FYP(t,Y,YPt);
   FOR i:=1 TO Neqn DO
   k[1,i] := h*YPt[i];
   FOR j := 1 TO 3 DO
   BEGIN
      FOR i:=1 TO Neqn DO
         Yt[i] := Y[i] + G[j]*k[j,i];
      tt := t + G[j]*h;
      FYP(tt,Yt,YPt);
      FOR i:=1 TO Neqn DO
         k[j+1,i] := h*YPt[i];
   END;
   {Add Y to a weighted average of increments in Y
     given by k[1]..k[4] to get Y at t + h}
   FOR i:=1 TO Neqn DO
      Y[i] := Y[i] + (k[1,i] + 2.0*k[2,i] +
               2.0*k[3,i] + k[4,i])/6.0;
   t := t + h;
END;

{-------------------------------------------------------------}
{--- Misc. utilities for dealing with wave function vector----}
{-------------------------------------------------------------}

PROCEDURE CenterX(V_emb: LatticeFunction;
                  VAR y,xx,LV_emb: DataVector; VAR delta_i_accum: INTEGER);
          {Centers the wave packet in an array of size 2*NumCoupledSites
                    with Real part at odd indices = 2*i - 1
                    and Imaginary part at even indices = 2*i
                    where i runs from 1 to NumCoupledSites}

   VAR delta_i,i,j, jadj: INTEGER;
       tempy: DataVector;
       LV_emb0: REAL;
BEGIN
    delta_i:=(icenter) - (NumCoupledSites DIV 2);
    delta_i_accum:= delta_i_accum + delta_i;
    jadj:= delta_i -1 + NumCoupledSites;{add NumCoupledSites to keep positive}
    FOR i:=1 TO NumCoupledSites DO
      BEGIN
        LV_emb0:= V_emb(Round(x0));
        j:= 2*(((i+jadj) MOD NumCoupledSites)+1);
        tempy[2*i-1]:= y[j-1];
        tempy[2*i]:= y[j];
        xx[i]:= (delta_i_accum + i);
        LV_emb[i]:= V_emb(delta_i_accum+i)-LV_emb0;
      END;
    y:=tempy;
END;

PROCEDURE Normalize(VAR z, p:DataVector;
                 VAR icenter: INTEGER; VAR norm,Ex,Sigma,initnorm,perr:REAL);
       {normalizes the wave function z and
        returns: normalized |C_n|^2 in array p, norm, expectation value of x,
                 variance Sigma, icenter, and perr}
    VAR
       i: INTEGER;
    BEGIN {Normalize}
      Ex := 0.0;
      Sigma:= 0.0;
      norm := 0.0;
      FOR i := 1 TO NumCoupledSites DO
         BEGIN
           p[i] := (sqr(z[2*i-1]) + sqr(z[2*i]));
           norm := p[i] + norm;
           Ex:= Ex + i*p[i];
         END;
      perr:= (norm -initnorm)/initnorm;
      Ex:= Ex/norm;
      FOR i:=1 TO NumCoupledSites DO
        BEGIN
           p[i]:= p[i]/norm;
           Sigma := Sigma + Sqr(i-Ex)*p[i];
        END;
      icenter := round(Ex);
      Ex := Ex + Delta_i_accum;
      Sigma:= Sqrt(Sigma);
    END; {Normalize}

{------------------------------------------------------------}
{------            Graphics Procedures                 ------}
{------------------------------------------------------------}

PROCEDURE PlotEmbeddedPotential(V_emb: LatticeFunction;
                                Ef,x0,eoff,Erange:REAL; V_embcolor: INTEGER);
   VAR
     tx,tpe: DataVector;
     i, imax, ioffset,j,jm,joff: INTEGER;
   BEGIN
      SetColor(V_embcolor);
      ioffset:= xlo;
      imax:= xhi-xlo;
      {The current version of CUPS utilities limits DataVector's
       to length of MaxNumData=401 and the viewed range may be more so...}
      jm:= imax div (MaxNumData-1);
      IF jm>0 THEN
         FOR j:=1 to jm DO
            BEGIN
            FOR i:= 1 TO (MaxNumData-1) DO
               BEGIN
               joff:= (j-1)*(MaxNumData-1);
               tx[i]:= joff + i + ioffset;
               tpe[i]:= eoff + (V_emb(joff + i +ioffset)
                          -Ef*(joff + i + ioffset - round(x0)))*Erange;
               END;
            PlotData(tx,tpe,MaxNumData-1);
            END;
      FOR i:= 1 TO imax MOD (MaxNumData-1) DO
         BEGIN
         joff:= jm*(MaxNumData-1);
         tx[i]:= joff+i + ioffset;
         tpe[i]:= eoff + (V_emb(joff+i +ioffset)
                    -Ef*(joff+i + ioffset - round(x0)))*Erange;
         END;
      PlotData(tx,tpe,imax MOD (MaxNumData-1));
      lastEF:= Ef;
   END; {PlotEmbeddedPotential}

PROCEDURE WriteToScreen(t, Ex, perr:REAL);
      {Prints new time, expectation value and accumulated error to screen }
VAR
   tstr:String;
BEGIN
   SetColor(White);
   tstr:= NumStr(t,5,2);
   print(5,1,tstr);
   tstr:= NumStr(Ex,5,1);
   print(7,2,tstr);
   tstr:= NumStr(Sigma,4,1);
   print(8,3,tstr);
   tstr:= NumStr(perr,6,4);
   print(57,1,tstr);
END;{WriteToScreen}

PROCEDURE UpDate_and_Plot(VAR z, xx, lastxx, prob, lastp,
                            Re_C_n, Im_C_n: DataVector;
                        VAR t, lastt, norm, Ex, Sigma, perr: REAL;
                            xlo, xhi: INTEGER);
          {Normalizes C_n^2 and finds new expectation value of x, Ex}
          {Finds fractional error accumulated in |C_n|^2, perr}
          {Plots the desired C_n function and writes t, Ex, Sigma, and perr}
          {Saves last values of everything so they can be erased}
   VAR
     i : INTEGER;
    {-----------------------------------}
  BEGIN {UpDate_and_Plot}
     IF ReC_nFlg THEN
        FOR i:=1 TO NumCoupledSites DO Re_C_n[i]:= z[2*i-1];
     IF ImC_nFlg THEN
        FOR i:=1 TO NumCoupledSites DO Im_C_n[i]:= z[2*i];
     Normalize(z, prob, icenter, norm, Ex, Sigma, norm0, perr);
     SetWriteMode(XorPut);
     IF NOT (ReC_nFlg OR ImC_nFlg) THEN
        BEGIN
        SetColor(C_nSqrColor);
        PlotData(xx,prob,NumCoupledSites-1);
        lastp:= prob;
        END
     ELSE
        BEGIN
        IF ReC_nFlg THEN
           BEGIN
           SetColor(ReC_nColor);
           PlotData(xx,Re_C_n,NumCoupledSites-1);
           END;
        IF ImC_nFlg THEN
           BEGIN
           SetColor(ImC_nColor);
           PlotData(xx,Im_C_n,NumCoupledSites-1);
           END;
        END;
     SetWriteMode(CopyPut);
     WriteToScreen(t,Ex,perr);
     IF WriteEFlg THEN
        WriteE;
     lastxx:= xx;
     lastt:= t;
     lastNCS:= NumCoupledSites;
     IF EraseV_embFlg then
        PlotEmbeddedPotential(EmbeddedPotential,lastEf,x0,eoff,Erange,GraphBackColor);
     IF PlotV_embFlg then
        PlotEmbeddedPotential(EmbeddedPotential,Ef,x0,eoff,Erange,LatPotColor);
  END; {UpDate_and_Plot}

PROCEDURE EraseC_n(VAR lastxx, lastp, Re_C_n, ImC_n: DataVector;
                   NCS: INTEGER);
      {Erases last plotted C_n so new one can be plotted}
   BEGIN {EraseC_n}
      SetWriteMode(XorPut);
      SetColor(GraphBackColor);
      IF (NOT (ImC_nFlg OR ReC_nFlg)) THEN
         BEGIN
         SetColor(C_nSqrColor);
         PlotData(lastxx,lastp,NCS-1);
         END
      ELSE
      BEGIN
         IF ReC_nFlg THEN
            BEGIN
            SetColor(ReC_nColor);
            Plotdata(lastxx,Re_C_n,NCS-1);
            END;
         IF ImC_nFlg THEN
            BEGIN
            SetColor(ImC_nColor);
            Plotdata(lastxx,Im_C_n,NCS-1);
            END;
      END;
      SetWriteMode(CopyPut);
      RubOut(5,1,7,GraphBackColor);
      RubOut(7,2,5,GraphBackColor);
      RubOut(8,3,4,GraphBackColor);
      RubOut(57,1,7,GraphBackColor);
      IF WriteEFlg THEN
      BEGIN
         RubOut(57,2,7,GraphBackColor);
         RubOut(57,3,7,GraphBackColor);
      END;
   END;  {EraseC_n}

PROCEDURE Plot_Exp_x_vs_t(VAR Exp_x, Time: DVector);
  {----------------------}
   PROCEDURE XTLegend;
   VAR Ave :REAL;
       i: INTEGER;
   BEGIN
      Ave:= 0.0;
      FOR i:=1 TO ndata-1 DO
         Ave:= Ave + Exp_x.Value(i)-x0;
      IF Ave<0 THEN DefineViewport(4,0.51,0.98,0.70,0.83)
      ELSE DefineViewport(4,0.51,0.98,0.12,0.25);
      SetColor(White);
      OpenViewport(4);
      SetColor(Yellow);
      Print(2,1, 'Yellow line -');
      SetColor(White);
      Print(2,3, 'White line  -');
      SetColor(LightGreen);
      Print(16,1, 'Semi-classical Model');
      Print(16,2, '   for uniform force');
      Print(16,3, 'Data from Wave Packet');
      SelectViewport(2);
   END; {XTLegend}
   {---------------------}
   VAR i : INTEGER;
       tmin,tmax,ttemp,tperiod,xtic: REAL;
   BEGIN
      ClearMUPPETport;
      DefineViewport(2, 0.1, 0.9, 0.1, 0.85);
      tmax:= -10e30;
      tmin:= 10e30;
      FOR i:=1 TO ndata DO
      BEGIN
         ttemp:= Time.Value(i);
         IF ttemp>tmax THEN tmax:= ttemp;
         IF ttemp<tmin THEN tmin:= ttemp;
      END;
      DefineScale(2,tmin, tmax, x0-2.5*Amplitude, x0+2.5*Amplitude);
      SelectScale(2);
      OpenViewPort(2);
      IF Ef<>0 THEN
      BEGIN
         tperiod:= 1/Ef;
         xtic:= tperiod/5;
         IF ((tmax-tmin)>3*tperiod) THEN xtic:= tperiod;
      END
      ELSE
         xtic:= (tmax-tmin)/5;
      ttemp:= (1000*xtic-frac(1000*xtic))/10;
      IF frac(ttemp)>=0.5 THEN ttemp:= ttemp + 1;
      xtic:= (ttemp-frac(ttemp))/100;
      SemiClassicalModel(k,x0,A,Ef,Amplitude,Time,Theo_x);

      SetColor(LegendColor);
      Axis(0.0,x0,xtic,amplitude/2);
      PutLabel(Top,'Wave Packet Propagation in a Crystal Lattice');
      SetColor(AxisLabelColor);
      PutLabel(Bottom,'Time (h/eV)');
      PutLabel(Left, '<x>');

      SetColor(DataColor);
      PlotDVectors(Time,Exp_x,1,ndata-1);
      SetColor(TheoryColor);
      PlotDVectors(Time,Theo_x,1,ndata-1);

      XTLegend;
   END;



{------------------------------------------------------------}
{-------------- Initialization procedures -------------------}
{------------------------------------------------------------}

PROCEDURE InitViewport1(xlo,xhi: INTEGER; pmax: REAL; VAR eoff, Erange: REAL);
VAR
   tempt,yrange,xstrt,xend: REAL;
   yline0,yline1,yline2   : REAL;
   xl,yl0,yl1,yl2         : INTEGER;
   tstr                   : String;
   {-------------------------------------------------}
   PROCEDURE SetRange(pmax: REAL; VAR yrng, eoff, Erange:REAL);
   BEGIN
     IF ReC_nFlg OR ImC_nFlg THEN
        BEGIN
          yrng:= Sqrt(1.0/(sigma0*sqrt(2*pi)));
          yrng:= (1000*1.1*yrng-frac(1000*1.1*yrng))/1000;
          eoff:= 0.0;
          Erange:= (1/20)*yrng;
        END
      ELSE
        BEGIN
          yrng:= (1000*1.1*pmax-frac(1000*1.1*pmax))/1000;
          eoff:= yrng/2;
          Erange:= yrng/40;
        END;
   END;
   {-----------------------------------------------}
BEGIN {InitViewport1}
   ClearMUPPETport;
   GraphBackColor := BGColor;
   SetRange(pmax, yrange, eoff, Erange);
{   xstrt:= xlo + 0.72*(xhi-xlo);
   xend:= xlo + 0.84*(xhi-xlo);
   xl:= 55; }
   xstrt:= xlo + 0.02*(xhi-xlo);
   xend:= xlo + 0.13*(xhi-xlo);
   xl:= 10;
   yline0:= -0.62*yrange; yl0:= 14;
   yline1:= -0.74*yrange; yl1:= 15;
   yline2:= -0.86*yrange; yl2:= 16;
   IF ((ReC_nFlg) OR (ImC_nFlg)) THEN
      DefineScale(1, xlo, xhi, -yrange, yRange)
   ELSE
      DefineScale(1, xlo, xhi, 0.0, yRange);
   DefineViewport(1, 0.1, 0.9, 0.15, 0.85);

   SelectScale(1);
   OpenViewPort(1);
   SetColor(LegendColor);
   Axis(0,0,(xhi-xlo)/5,yRange);
   PutLabel(Top,'Wave Packet Propagation in a Crystal Lattice');
   Setcolor(White);
   PutLabel(Bottom,'Lattice site number');
   IF (ReC_nFLg AND ImC_nFlg) THEN
      BEGIN
      PutLabel(Left,'Re{C_n} and Im{C_n}');
      SetWriteMode(XorPut);
      SetColor(ReC_nColor);
      PlotLine(xstrt, yline1, xend, yline1);
      SetColor(ImC_nColor);
      PlotLine(xstrt, yline2, xend, yline2);
      SetColor(White);
      SetWriteMode(CopyPut);
      Print(xl,yl1,'Re{C_n}');
      Print(xl,yl2,'Im{C_n}');
      END
   ELSE
      BEGIN
      IF ReC_nFlg THEN
         BEGIN
         PutLabel(Left, 'Re{C_n}');
         SetWriteMode(XorPut);
         SetColor(ReC_nColor);
         PlotLine(xstrt, yline1, xend, yline1);
         SetColor(White);
         SetWriteMode(CopyPut);
         Print(xl,yl1,'Re{C_n}');
         END
      ELSE
         IF ImC_nFlg THEN
            BEGIN
            PutLabel(Left,'Im{C_n}');
            SetWriteMode(XorPut);
            SetColor(ImC_nColor);
            PlotLine(xstrt, yline2, xend, yline2);
            SetWriteMode(CopyPut);
            SetColor(White);
            Print(xl,yl2,'Im{C_n}');
            END
         ELSE PutLabel(Left,'|C_n|^2');
      END;
   SetColor(LatPotColor);
   IF (ReC_nFLg OR ImC_nFlg) THEN PlotLine(xstrt, yline0, xend, yline0)
   ELSE
      BEGIN
      PlotLine(xstrt,0.19*yrange,xend,0.19*yrange);
      SetWriteMode(XorPut);
      SetColor(C_nSqrColor);
      PlotLine(xstrt,0.13*yrange,xend,0.13*yrange);
      SetWriteMode(CopyPut);
      SetColor(White);
      Print(xl,yl1,'|C_n|^2');
      END;
   SetColor(White);
   Print(xl,yl0,'V_emb + V_a');
   tstr:= 'T=';
   print(2,1,tstr);
   tstr:= '(h/eV)';
   print(12,1,tstr);
   tstr:= '<x> =';
   print(2,2,tstr);
   tstr:= 'Sigma=';
   print(2,3,tstr);
   tstr:= 'Accum. Prob. Err.=';
   print(39,1,tstr);
   tstr:= 'E_a=';
   Print(xl-8,yl0-1,tstr);
   tstr:= NumStr(Ef,7,4);
   Print(xl-4,yl0-1,tstr+' (V/a)');
   IF WriteEFlg THEN
   BEGIN
      EraseELabels;
      WriteELabels;
   END;
   WriteToScreen(t,Ex,perr);
   IF (NOT TimeArrowFlg) THEN
   BEGIN
      SetColor(TimeArrowColor);
      print(2,4,'Time Arrow Reversed');
   END;
   lastt:=t;
   SetUpHotKeys1;
END; {InitViewport1}

PROCEDURE RedrawPkt;
Begin
   InitViewport1(xlo, xhi, pmax, eoff, Erange);
   PlotEmbeddedPotential(EmbeddedPotential,Ef,x0,eoff,Erange,LatPotColor);
   UpDate_and_Plot(C_n, xx, lastxx, prob, lastp, Re_C_n, Im_C_n,
            t, lastt, norm, Ex, Sigma, perr, xlo, xhi);
END;

PROCEDURE ReadyAll;
BEGIN {ReadyAll}
   NumCoupledSites:= Default_NCS;
   Neqn:= 2*NumCoupledSites;
   icenter:=NumCoupledSites DIV 2;
   Delta_i_accum:= round(x0) - icenter;
   MakeWavePacket(icenter,sigma0,norm,pmax,Ex,Sigma,C_n,prob);
   norm0:= norm;
   Ex:= Ex + Delta_i_accum;
   perr:= 0.0;
   t:=0.0;
    InitViewport1(xlo, xhi, pmax, eoff, Erange);
   PlotEmbeddedPotential(EmbeddedPotential,Ef,x0,eoff,Erange,LatPotColor);
   CenterX(EmbeddedPotential,C_n,xx,LV_emb,delta_i_accum);
   HideCursor;
   UpDate_and_Plot(C_n, xx, lastxx, prob, lastp, Re_C_n, Im_C_n,
            t, lastt, norm, Ex, Sigma, perr, xlo, xhi);
   ShowCursor;
   {*****Semi-classical model for uniform force gives
         Bloch oscillation with Amplitude 2*A/Ef and Period 1/Ef *****}
   If Ef <> 0 THEN
     BEGIN
       Period:= 1/Ef;
       Amplitude:= 2*A/Ef;
     END
   ELSE
     BEGIN
       Period:= 100.0;
       Amplitude:= 30.0;
     END;
   ndata:= 1;
   Time.put(ndata, 0.0);
   Exp_x.put(ndata, x0);
   PktMenu.Display;
   ShowCursor;
END;

PROCEDURE Initialize_parameters;
   BEGIN
   Ef:= tEf;
   x0:= tx0;
   sigma0:= tsigma0;
   k:= tk;
   A:= tA;
   dt:= tdt;
   NumReDraw:= tNumReDraw;
   END;

PROCEDURE Init_Once;
   VAR I,mem3: LONGINT;
   BEGIN
   CUPSinit;
   I:= (MemAvail div 24);
   MaxData:= I div 2;
   IF MaxData>8000 THEN MaxData:= 8000;
   Time.init(MaxData);
   Exp_x.init(MaxData);
   Theo_x.init(MaxData);
{BEGIN --- debuging ---
   mem3:= MemAvail;
   PRINT(1,2,CONCAT('MaxData =',numstr(maxdata,10,0)));
   PRINT(1,3,CONCAT('MemAvail =',NumStr(mem3,10,0),' Bytes'));
   pause;
END  --- debuging ---}
   DelayTime:= 0;
   Initialize_parameters;
   SetUpPktMenu;
   EmbeddedPotential:= ConstantPotential;
   Potential:= Constant;
   Neqn:= 2*NumCoupledSites;
   ClearMKBuffers;
   ShowCursor;
   xlo:= xlo_Default;
   xhi:= xhi_Default;
   readyAll;
   AboutProgram;
   ChangeMenuFlg:= FALSE;
   PktMenu.AutoChecking(5,1);
   PktMenu.AutoChecking(6,1);
   END; {Init_Once}

PROCEDURE Read_Change_and_Reset;
  BEGIN
     GetScreenData(tA, tEf, tx0, tsigma0, tk, tdt);
     IF (NOT CanceledFlg) THEN
        BEGIN
        Initialize_parameters;
        ReC_nFlg:= FALSE;  PktMenu.RowChecked(5,2,false);
        ImC_nFlg:= FALSE;  PktMenu.RowChecked(5,3,false);
        PktMenu.RowChecked(5,4,false);
        PktMenu.RowChecked(5,1,true);
        HideCursor;
        ReadyAll;
        END;
     ChangeMenuFlg:= FALSE;
  END; {Read_Change_and_Reset}


{---------------------------------------------------------------------------}
{--- Main procedure that steps wave packet forward, saves, displays, etc ---}
{---------------------------------------------------------------------------}

PROCEDURE Interrupt1(key: byte; VAR dt: REAL; VAR QuitFlag: BOOLEAN); FORWARD;

PROCEDURE StepAndDisplay(VAR delta_t_draw, t, h: REAL;
                            VAR z: DataVector);
   VAR t0: REAL;
  {--------------------------------------------------------------------}
   PROCEDURE It_All_Happens_Here(t0: REAL; VAR h, t: REAL;
                                 VAR z: DataVector);
     {General procedure controlling the calc. of new wave packet
      and its display}
     VAR
       i: INTEGER;
       ErrorCount: LONGINT;
       wingp: REAL;
       qflag,ErrorFlag: BOOLEAN;
     {----------------------}
        PROCEDURE ChangeNumCoupledSites(di: INTEGER);
        VAR imin,i: INTEGER;
        BEGIN
           imin:= 2*NumCoupledSites + 1;
           IF di>0 THEN FOR i:= imin TO imin+2*di DO z[i]:= 0.0;
           NumCoupledSites:= NumCoupledSites + di;
           IF 2*NumCoupledSites>=(MaxNumData-2*WingSize) THEN
              Announce('Warning: Width of packet exceeding 200 sites, errors are likely.');
           neqn:=2*NumCoupledSites;
           CenterX(EmbeddedPotential,z,xx,LV_emb,delta_i_accum);
           Normalize(z, prob, icenter, norm, Ex, Sigma, norm0, perr);
        END;
     {----------------------}
     FUNCTION ProbabilityInWings(VAR z: DataVector; wngsz: INTEGER): REAL;
     VAR i: INTEGER;
         twings: REAL;
     BEGIN
        twings:= 0.0;
        For i:=1 TO wngsz DO
           twings:= twings + (sqr(z[2*i-1]) + sqr(z[2*i]));
        For i:=NumCoupledSites-wngsz TO NumCoupledSites DO
           twings:= twings + (sqr(z[2*i-1]) + sqr(z[2*i]));
        ProbabilityInWings:=twings/norm0;
     END;
      {---------------------}
     PROCEDURE SaveData(VAR ndata:INTEGER; VAR Exp_x, Time: DVector);
        { Saves Ex and time }
      BEGIN {SaveData}
         IF ndata < MaxData THEN
         BEGIN
            ndata := ndata + 1;
            Exp_x.put(ndata, Ex);
         END;
         Time.put(ndata, t);
      END;  { SaveData}
     {------------------------}
   BEGIN  {It_All_Happens_Here}
      t:= t0;
      qflag:=FALSE;
      ErrorCount:=0;
      REPEAT {UNTIL qflag}
      BEGIN
         {--------The following loop steps the wave packet
                        forward in time by delta_t_draw}
         ErrorFlag:= TRUE;
         REPEAT  {UNTIL (ABS(t-lastt) >= delta_t_draw - ABS(h/2))}
            StepRK4(tight_binding_dxdt, t, h, z);
            wingp:= ProbabilityInWings(z,WingSize);
            IF wingp<LowWingProbTol THEN
               IF AutoAdjustFlg THEN ChangeNumCoupledSites(-WingSize);
            IF (2*(NumCoupledSites+WingSize))<MaxNumDATA THEN
               BEGIN
               IF wingp>HighWingProbTol THEN
                  BEGIN
                  IF AutoAdjustFlg THEN
                     ChangeNumCoupledSites(WingSize)
                  ELSE
                     IF ErrorFlag THEN
                        BEGIN
                        IF ErrorCount MOD 20 = 0 THEN
                           Announce(Concat('Warning: Packet too wide -- ',
                           'errors accumulating; turn on auto-adjust mode.'));
                         ErrorFlag:= FALSE;
                         ErrorCount:= ErrorCount+1;
                         END;
                  END;
               END
            ELSE
               IF wingp > 10*HighWingProbTol THEN
                  IF ErrorFlag THEN
                     BEGIN
                     IF ErrorCount MOD 20 = 0 THEN
                        Announce(Concat('Warning: Packet much too wide -- ',
                                        'Stop integration by clicking twice.'));
                      ErrorFlag:= FALSE;
                      ErrorCount:= ErrorCount+1;
                      END;

         UNTIL (ABS(t-lastt) >= delta_t_draw - ABS(h/2));
         Delay(DelayTime);
         {--------Now recenter the packet if necessary and plot/save results}
         IF (ABS(icenter-(NumCoupledSites DIV 2))>3) THEN
             CenterX(EmbeddedPotential,z,xx,LV_emb,delta_i_accum);
         HideCursor;
         EraseC_n(lastxx, lastp, Re_C_n, Im_C_n, lastNCS);
         UpDate_and_Plot(C_n, xx, lastxx, prob, lastp, Re_C_n, Im_C_n,
                       t, lastt, norm, Ex, Sigma, perr, xlo, xhi);
         ShowCursor;
         SaveData(ndata, Exp_x, Time);
         CheckForEvents;
         IF PktHotKeys1.Pressed(key) THEN
            Interrupt1(key,h,qflag)
         ELSE
            IF (Event.KeyPressed Or Event.MouseClicked) THEN qflag := TRUE;
      END;
      UNTIL qflag;
      HideCursor;
      SetUpHotKeys1;
      ShowCursor;
   END; {It_All_Happens_Here}
   {-------------------------------------------------------------------}
BEGIN {StepAndDisplay}
   HideCursor;
   Message('Press any key or click mouse to stop integration');
   SetUpHotKeys1;
   ShowCursor;
   t0:= t;
   IF (ReC_nFlg OR ImC_nFlg) THEN
      BEGIN
      IF Abs(Ef) > 0.02 THEN
         delta_t_draw:= (1/Ef)/(2*NumReDraw)
      ELSE
         delta_t_draw:=1/25;
      END
   ELSE
      BEGIN
      IF Abs(Ef)>0.02 THEN
         delta_t_draw:= (1/Ef)/NumReDraw
      ELSE
         delta_t_draw:=1/10;
      END;
   IF delta_t_draw<ABS(dt) THEN delta_t_draw:= ABS(dt);
   It_All_Happens_Here(t0,h,t,z);
   HideCursor;
   PktMenu.Display;
   ShowCursor;
END; {StepAndDisplay}

{--------------------------------------------------------------------}
{--------- Hot Key Interrupt and Main menu handler procedures -------}
{--------------------------------------------------------------------}

PROCEDURE Interrupt1(key: byte; VAR dt: REAL; VAR QuitFlag: BOOLEAN);
var dummy: Boolean;
BEGIN
   CASE key OF
    1 : BEGIN  {Help}
           QuitFlag:= FALSE;
           help1;
        END;
    2 : BEGIN  {GO}
           QuitFlag:= NOT QuitFlag;
           IF NOT QuitFlag THEN StepAndDisplay(delta_t_draw, t, dt, C_n);
        END;
    3 : BEGIN  {Slower}
           QuitFlag:= FALSE;
           IF DelayTime<1000 THEN
              DelayTime:= DelayTime + 20
           ELSE
              Announce('Already as slow as my patience will allow.');
        END;
    4 : BEGIN  {Faster}
           QuitFlag:= FALSE;
           IF DelayTime>20 THEN
              DelayTime:= DelayTime - 20
           ELSE
              BEGIN
              Announce('I am working as fast as I can.');
              DelayTime:= 0;
              END;
        END;
    5 : QuitFlag:= TRUE;   {Top Menu}
   END; {case}
   ClearMKBuffers;
END;

PROCEDURE HandlePktMenu;
VAR I,xlo_tmp,xhi_tmp: INTEGER;
   BEGIN
        WITH PktMenu DO
          CASE colChosen OF
          1 : BEGIN
                 CASE rowChosen OF
                 1: AboutCUPS;
                 2: AboutProgram;
                 3: Configuration;
                 4: help2;
                 6: ExitFlg:= TRUE;
                 END;
              END;
          2 : ChangeMenuFlg:= TRUE;
          3 : BEGIN
                 CASE rowChosen OF
                 1: StepAndDisplay(delta_t_draw, t, dt, C_n);
                 2: BEGIN
                       HideCursor;
                       ClearMUPPETport;
                       ReadyAll;
                    END;
                 END; {CASE}
              END;
          4 : BEGIN
                 CASE rowChosen OF
                 1: BEGIN
                       dt:=-dt;
                       TimeArrowFlg:= NOT TimeArrowFlg;
                       SelectViewport(1);
                       RubOut(2,4,19,GraphBackColor);
                       SetColor(TimeArrowColor);
                       IF (NOT TimeArrowFlg) THEN
                          print(2,4,'Time Arrow Reversed');
                                              {SetUpHotKeys1;}
                    END;
                 2: BEGIN
                       AutoAdjustFlg:= NOT AutoAdjustFlg;
                       AutoAdjustMessage(AutoAdjustFlg);
                       {SetUpHotKeys1;}
                    END;
                 3: BEGIN
                       WriteEFlg:= NOT WriteEFlg;
                       IF (NOT WriteEFlg) THEN
                          BEGIN
                          EraseELabels;
                          PktMenu.RowChecked(4,3,false);
                          END
                       ELSE
                          BEGIN
                          EraseELabels;
                          WriteELabels;
                          WriteE;
                          PktMenu.RowChecked(4,3,true);
                          END;
                    END;
                 END; {CASE}
              END;
          5 : BEGIN
                 CASE rowChosen OF
                 1 : BEGIN ReC_nFlg:= FALSE; ImC_nFlg:= FALSE; END;
                 2 : BEGIN ReC_nFlg:= TRUE; ImC_nFlg:= FALSE; END;
                 3 : BEGIN ReC_nFlg:= FALSE; ImC_nFlg:= TRUE; END;
                 4 : BEGIN ReC_nFlg:= TRUE; ImC_nFlg:= TRUE; END;
                 5 : IF ndata>1 THEN
                           BEGIN
       {                       HideCursor;  }
                              Plot_Exp_x_vs_t(Exp_x,Time);
                              PressKeyMessage;
                              PktMenu.Rowchecked(5,5,false);
                              IF ((Not ReC_nFlg) AND (NOT ImC_nFlg)) THEN
                                 PktMenu.RowChecked(5,1,true)
                              ELSE IF ((ReC_nFlg) AND (NOT ImC_nFlg)) THEN
                                         PktMenu.RowChecked(5,2,true)
                                   ELSE  IF ((NOT ReC_nFlg) AND ImC_nFlg) THEN
                                             PktMenu.RowChecked(5,3,true)
                                         ELSE PktMenu.RowChecked(5,4,true);
      {                        ShowCursor;    }
                           END;
                 END; {CASE}
                 HideCursor;
                 RedrawPkt;
                 SetUpHotKeys1;
                 PktMenu.Display;
                 ShowCursor;
              END;

          6 : BEGIN
                 CanceledFlg:= FALSE;
                 CASE rowChosen OF
                 1 : BEGIN
                        EmbeddedPotential:= ConstantPotential;
                        Potential:= Constant;
                     END;
                 2 : BEGIN
                        EmbeddedPotential:= SquareBarrier;
                        Potential:= SqrBarrier;
                     END;
                 3 : BEGIN
                        EmbeddedPotential:= SuperLattice;
                        Potential:= Superlat;
                     END;
                 4 : BEGIN
                        EmbeddedPotential:= RandBarrierPotential;
                        FOR I:= -400 TO 400 DO RandX[i]:= Urand(Iseed);
                        Potential:= RandBarrier;
                     END;
                 5 : BEGIN
                        EmbeddedPotential:= UserDefinedPotential;
                        Potential:= User;
                     END;
                 6 : ReadEmbPotParms(BarrierHeight,SLEtop,
                                     BarrierWidth,BarrierX0,SLWidth);
                 END; {CASE}
                 IF NOT CanceledFlg THEN
                    BEGIN
                    HideCursor;
                    ClearMUPPETport;
                    ReadyAll;
                    END;
              END;
          7 : BEGIN
                 Case rowChosen OF
                 1 : BEGIN
                     IF ((xhi+25)<=400) THEN
                        BEGIN
                        xlo:= xlo + 25;
                        xhi:= xhi + 25;
                        RedrawPkt; PktMenu.Display;
                        END
                     ELSE
                        Announce(' Displayed lattice must remain within -400 to +400 range.');
                     END;
                 2 : BEGIN
                     IF ((xlo-25)>=-400) THEN
                        BEGIN
                        xlo:= xlo - 25;
                        xhi:= xhi - 25;
                        RedrawPkt; PktMenu.Display;
                        END
                     ELSE
                        Announce(' Displayed lattice must remain within -400 to +400 range.');
                     END;
                 3 : BEGIN
                     IF ((xhi-xlo)>25) THEN
                        BEGIN
                        xlo:= xlo + 25;
                        xhi:= xhi - 25;
                        IF ((xhi-xlo)<=0) THEN
                           BEGIN
                           xlo:= xlo-(25 div 2);
                           xhi:= xlo+25;
                           END;
                        RedrawPkt; PktMenu.Display;
                        END
                     ELSE
                        Announce(' Smallest length of lattice displayed is 25 sites.');
                     END;
                 4 : BEGIN
                        xlo_tmp:= xlo;
                        xhi_tmp:= xhi;
                        xlo_tmp:= xlo_tmp-25;
                        xhi_tmp:= xhi_tmp+25;
                        IF ((xlo_tmp>=-400) AND (xhi_tmp<=400)) THEN
                           BEGIN
                           xlo:= xlo_tmp;
                           xhi:= xhi_tmp;
                           RedrawPkt; PktMenu.Display;
                           END
                        ELSE
                           Announce(' Displayed lattice must remain within -400 to +400 range.');
                     END;
                 5 : BEGIN
                        xlo:= xlo_Default; xhi:= xhi_Default;
                        RedrawPkt; PktMenu.Display;
                     END;
                 END; {CASE}
              END;
        END;
   END; {HandlePktMenu}

{----------------------------------------------------------}
{*-----                Main Program                  -----*}
{----------------------------------------------------------}
VAR
   i: INTEGER;
   CtrlFlag: BOOLEAN;

BEGIN {Main}
Init_Once;
REPEAT
  IF ChangeMenuFlg THEN
     Read_Change_and_Reset;
  REPEAT
     CheckForEvents;
     IF PktHotKeys1.Pressed(key) THEN
        BEGIN
        CtrlFlag:= TRUE;
        Interrupt1(key,dt,CtrlFlag);
        END;
     If PktMenu.Activated THEN HandlePktMenu;
  UNTIL (ChangeMenuFlg OR ExitFlg);
UNTIL ExitFlg;
PktMenu.done;
CUPSdone;
END. {tight_binding_packet program}

