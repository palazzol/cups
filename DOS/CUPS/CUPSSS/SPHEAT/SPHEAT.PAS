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

          {****************************************************}
          {****************************************************}
          {**               Program SpHeat.Pas               **}
          {**        Lattice Specific Heat of Solids         **}
          {**             Written by Graham Keeler           **}
          {**                                                **}
          {**                   Version 1.5                  **}
          {**              Last change:  17/2/95             **}
          {****************************************************}
          {****************************************************}


{********************** GENERAL STRUCTURE OF PROGRAM: *********************}

{*********************** FOR COMPATIBILITY WITH MAC ***********************}
{**************************** INITIALIZATION ******************************}
{************************** SET UP MENU, HOT KEYS *************************}
{******************** GENERAL FUNCTIONS AND PROCEDURES ********************}
{***************** MATHEMATICAL FUNCTIONS AND PROCEDURES ******************}
{*************** SUBSIDIARY ROUTINES FOR EIGENVALUE HANDLING **************}
{************************ SET UP DYNAMICAL MATRICES ***********************}
{************************ SELECT DYNAMICAL MATRIX *************************}
{*************************** MAIN CALCULATIONS ****************************}
{********************* SET UP AND HANDLE INPUT SCREENS ********************}
{***************************** SET UP SLIDERS *****************************}
{*********************** GENERAL PRINTING PROCEDURES **********************}
{************************ DENSITY OF STATES DISPLAY ***********************}
{************************** SPECIFIC HEAT DISPLAY *************************}
{************************ DEBYE TEMPERATURE DISPLAY ***********************}
{************************* SELECT TYPE OF DRAWING *************************}
{******************************* HANDLE ZOOM ******************************}
{*********************** MENU AND HOT KEY HANDLING ************************}
{****************************** MAIN PROGRAM ******************************}

PROGRAM SpecificHeat;
USES dos, crt, Graph, cupsmupp, Cups, CupsFunc, CupsProc,
     CupsGrph, Cupsgui;
CONST
   AvNo = 6.022E23; hbar = 1.054E-34; kB = 1.380E-23;
   lhx = 0.09; rhx = 0.85; topy = 0.89; ctry = 0.26; btmy = 0.06;
   SHmax = 1.1; NoOfPoints = 101;
   BulkModData : array[1..7] of real = (2.7,75.5,103.6,169.8,12.7,24.5,542.0);
   {Data for Argon, Aluminium, Silver, Iron, CsCl, NaCl, diamond}
   DensityData : array[1..7] of real = (1688,2698,10500,7873,3988,2165,3300);
   {AtWtData : array[1..7] of real = (39.95,26.98,58.7,55.85,84.18,29.22,12);}
   MolWtData : array[1..7] of real = (39.95,26.98,107.87,55.85,168.36,58.44,12);

   ArgonPoints : integer = 24;
   AluminiumPoints : integer = 36;
   SilverPoints : integer = 27;
   IronPoints : integer = 3;
   CsClPoints : integer = 3;
   NaClPoints : integer = 3;
   DiamondPoints : integer = 3;
   ArgonData : array[2..24] of real = (0.0195,0.0673,0.175,0.368,0.691,1.770,
     3.294,5.100,8.01,12.16,15.30,17.49,19.13,20.25,20.95,21.54,21.94,22.19,
     22.34,22.60,22.94,23.34,24.02); {Ref: Peterson, Batchelder & Simmons, Phys Rev, 150, 703-711, 1966}
   ArgonTemp : array[2..24] of real = (2, 3, 4, 5, 6, 8, 10,12,15,20,25,30,
     35,40,45,50,55,60,65,70,75,80,83);
   AluminiumData : array[2..36] of real = (0.0030,0.0050,0.0076,0.014,0.023,
     0.044,0.068,0.118,0.170,0.247,0.339,0.610,1.501,2.190,3.082,4.052,5.509,
     6.920,8.681,10.40,12.07,13.06,14.41,15.70,16.74,17.68,18.50,19.28,20.00,
     20.58,21.11,21.62,22.32,23.00,23.50);
   AluminiumTemp : array[2..36] of real = (2,3,4,6,7.6,9.9,12.4,15.08,17.65,
     20,22.67,27.01,35.94,40.68,45.98,51.11,58.59,65.79,74.66,84,93.63,100,
     110,120,130,140,150,160,170,180,190,200,220,240,260);
   SilverData : array[2..27] of real = (0.00106,0.00262,0.00656,0.0126,0.0213,
     0.0372,0.0631,0.0987,0.198,0.347,0.558,0.845,1.671,4.296,7.087,10.80,
     13.33,15.36,16.89,18.09,20.07,21.27,22.48,22.85,23.33,23.45);
   SilverTemp : array[2..27] of real = (1.35,2,3,4,5,6,7,8,10,12,14,16,20,
     28.56,36.16,47.09,55.88,65.19,74.56,83.91,103.14,124.2,144.38,166.78,
     190.17,205.3);
   IronData : array[2..3] of real = (0.01, 5.6);
   IronTemp : array[2..3] of real = (5, 56);
   CsClData : array[2..3] of real = (0.01, 5.6);
   CsClTemp : array[2..3] of real = (5, 56);
   NaClData : array[2..3] of real = (0.01, 5.6);
   NaClTemp : array[2..3] of real = (5, 56);
   DiamondData : array[2..3] of real = (0.01, 5.6);
   DiamondTemp : array[2..3] of real = (5, 56);

TYPE
   FunctionX = function(x: real): real;

   MyDVector = OBJECT(DVector)
      procedure AddToElement(Index : integer; {Add in X at Index}
                             X     : real );
   end;

   ModelType = (None, Einstein, Debye, Combined, Schottky,RealCrystal,Experiment);
   StructureType = (nostructure, BCC, FCC,
                    DiamondType, CsClType, NaClType);
   SubstanceType = (USERDEFINED, Argon, Aluminium, Silver, Iron,
                    CsCl, NaCl, diamond);
   ResolutionType = (Fine, Medium, Coarse);
   DirnType = (IOO, IIO, III);
   EnergyLevels = array [0..100] of real;
   HelpContextType = (fileitem, model, structure, substance, plotoptns,
                      setdata, schottkydata, zoomitem, displayitem);
   SetDataType  =  RECORD
                      NoOfModes, StructNum : integer;
                      R, Rho, OldRho, NCFC : real;
                      BulkMod, density, MolWt{, AtWt} : real;
                      DisplaySpHt, DisplayTCubed, ShowDoS : boolean;
                      Superimpose, ShowPrevious, Autodraw, UseDebye : boolean;
                   end;
   SchottkyDataType  =  RECORD
                           Schottkyonly, IndividualLevels : boolean;
                           EqualSpacing, nfoldSpacing : boolean;
                           NoofLevels : integer;
                           E : EnergyLevels;
                        end;
   DataType = RECORD
                 Model, OldModel, PreviousModel : ModelType;
                 Structure, UDStructure, OldStructure : StructureType;
                 Substance : SubstanceType;
                 SetData : SetDataType;
                 SchottkyData : SchottkyDataType;
                 NoOfIntvls, DataPoints : integer;
                 wrange, FScale, UserTheta : real;
                 Tmax, Tzoom, SHzoom, Theta, OldTheta, dosmax : real;
                 VaryForce, DoSChange, Quit, SlidersFlag : Boolean;
                 FirstTime, DoSFirstTime : boolean;
                 SliderResolution : ResolutionType;
                 SpHtData : array[2..36] of real;
                 SpHtTemp : array[2..36] of real;
           end;

   freqVector = array [1..12] of MyDVector;
   RangeType = array [IOO..III] of real;
   TrigSet = array[1..3] of real;
   freqarray = array [1..6] of real;
   ComplexDMatrix = RECORD
                       Re, Im : DMatrix;
                    end;


procedure MyDVector.AddToElement(Index : integer; X : real);
var Y : real;
begin
   Y := Value(Index);
   Put(Index, X+Y);
end;

VAR WaitMessage : TWaitMessage;
    Sliders : TSliders;
    NoOfSliders : integer;
    ResButtons : TButtons;
    HelpContext : HelpContextType;
    Temperature, SpHt, DBT : MyDVector;
    {declare objects globally to avoid need to pass as parameters}
    xvector, dosvector, oldxvector, olddosvector : MyDVector;
    dbvector : MyDVector;
    {declared globally to preserve their values after call to CalcDofStates}

{********************** FOR COMPATIBILITY WITH MAC *********************}

procedure lineTP(x1, y1, x2, y2 : integer);
begin
   line(x1, y1, x2, y2);
end;

{**************************** INITIALIZATION ***************************}

procedure intro;
var IntroScreen : HelpScrType;
begin
   IntroScreen[01] := '                                                 ';
   IntroScreen[02] := '         LATTICE SPECIFIC HEAT OF SOLIDS         ';
   IntroScreen[03] := '                                                 ';
   IntroScreen[04] := '                        by                       ';
   IntroScreen[05] := '                                                 ';
   IntroScreen[06] := '                  Graham Keeler                  ';
   IntroScreen[07] := '            University of Salford, UK            ';
   IntroScreen[08] := '                                                 ';
   IntroScreen[09] := '                   Version 1.5                   ';
   IntroScreen[10] := '               Copyright (c) 1995                ';
   IntroScreen[11] := '                                                 ';
   IntroScreen[12] := '                                                 ';
   IntroScreen[13] := ' This program calculates and displays the temp-  ';
   IntroScreen[14] := ' erature variation of the lattice specific heat  ';
   IntroScreen[15] := ' for a number of different theoretical models,   ';
   IntroScreen[16] := ' and for a computer simulation of a realistic    ';
   IntroScreen[17] := ' density of states. It will also display the     ';
   IntroScreen[18] := ' results in the form of the equivalent Debye     ';
   IntroScreen[19] := ' Temperature.                                    ';
   IntroScreen[20] := '                                                 ';
   IntroScreen[21] := '                                                 ';
   IntroScreen[22] := '                                                 ';
   IntroScreen[23] := '                                                 ';
   IntroScreen[24] := '    Press <Enter> or Click Mouse to continue.    ';
   IntroScreen[25] := '                                                 ';
   help(IntroScreen);
end;

procedure InitializePortsAndData(var Menu : Tmenu; var Data : DataType);
begin
   with Data do begin
      VaryForce := true; Model := Einstein; OldModel := None;
      Structure := FCC; UDStructure := FCC;
      Substance := USERDEFINED; Quit := false;
      DoSChange := true;
      HelpContext := fileitem;
      with SetData do begin
         BulkMod := 2.7; density := 1688; MolWt := 40; {AtWt := 40;}
         NoofModes := 300; StructNum := 1; {select FCC initially}
         R := 0.5; Rho := 2; NCFC := 0.2;
         DisplaySpHt := true; DisplayTCubed := false;
         Superimpose := true; AutoDraw := true;
         ShowDoS := false; UseDebye := false;
      end;
      Menu.AutoChecking(2,1); {Select Einstein model initially}
      Menu.AutoChecking(3,1); {Select USERDEFINED initially}
      Menu.RowChecked(4,2,true);
      Menu.RowChecked(4,6,true);
      Menu.RowChecked(4,10,true);
      Menu.RowChecked(4,14,true);
      Menu.RowChecked(4,16,true);
      with SchottkyData do begin
         SchottkyOnly := true; IndividualLevels := false;
         EqualSpacing := true; nfoldSpacing := false;
         NoOfLevels := 2;
         E[0] := 0; E[1] := 20; E[2] := 30; E[3] := 40; E[4] := 50;
      end;
   end;
   Sliders.Create(1, 0.1, 10, 2, 0.91, 0.235,
                  0.565, 2, '.1', '10', 'force ratio', true);
   NoOfSliders := 1;
   ResButtons.Create(0, 0.88, 0.19, 'Fine');
   ResButtons.Create(1, 0.88, 0.14, 'Medium');
   ResButtons.Create(2, 0.88, 0.09, 'Coarse');
   DefineViewPort(1, lhx, rhx, ctry, topy);
   DefineViewPort(3, 0, rhx, btmy, ctry - 0.07); {label viewport}
   DefineScale(3, 0, rhx*GetMaxX, 0, (ctry - 0.07 - btmy)*GetMaxY);
   DefineViewPort(10, 0, 1, 0.05, 0.95); {whole screen - for use in clearing}
end;

{************************** SET UP MENU, HOT KEYS *************************}

procedure SetUpMenu(var Menu : TMenu);
begin
   Menu.Init;
   with Menu do
   begin
      column(1, 'File');
       row(1,1, 'About CUPS');
       row(1,2, 'About program');
       row(1,3, 'Program details');
       row(1,4, 'Configuration');
       row(1, 5, '------------------');
       rowactivate(1,5,false);
       row(1,6, 'Exit Program');
      column(2, 'Model');
       row(2,1, 'Einstein');
       row(2,2, 'Debye');
       row(2,3, 'Combined Einstein/Debye');
       row(2,4, 'Realistic Density of States');
       row(2,5, 'Schottky');
       row(2,6, 'Experimental Data');
      column(3, 'Substance');
       row(3,1, 'USER DEFINED');
       row(3,2, 'Solid Argon');
       row(3,3, 'Aluminium');
       row(3,4, 'Silver');
       row(3,5, 'Iron');
       row(3,6, 'CsCl');
       row(3,7, 'NaCl');
       row(3,8, 'Diamond');
      column(4, 'Plot Optns');
      column(5, 'Set Data');
      column(6, 'Schtky Dta');
      column(7, 'Sldr Resln');
       row(7,1, 'Fine');
       row(7,2, 'Medium');
       row(7,3, 'Coarse');

      Display;
      RowActivate(2,6,false);
      RowActivate(4,1,false);
      RowActivate(4,5,false);
      RowActivate(4,9,false);
      RowActivate(4,12,false);
      RowActivate(4,15,false);
   end;
end;


procedure SetUpHotKeys(var HotKeys : THotKeys);
begin
   with HotKeys do begin
      init(6);
      key[1] := 'F1-Help';
      key[2] := 'F5-Zoom';
      key[3] := 'F6-Unzoom';
      key[4] := 'F7-Clear';
      key[5] := 'F8-Plot';
      key[6] := 'F10-Menu';
   end;
   HotKeys.Display;
end;

{******************** GENERAL FUNCTIONS AND PROCEDURES *******************}

FUNCTION roundup (dis : REAL) : REAL;
VAR l,e,f,nf : REAL;
BEGIN
   l := log10(dis);
   IF l>=0 THEN e:=trunc(l) ELSE e:=trunc(l-1);
   f := dis/pwr(10,e);
   IF f<1.25 THEN nf:=1 ELSE
   IF f<1.75 THEN nf:=1.5 ELSE
   IF f<2.5 THEN nf:=2 ELSE
   IF f<4 THEN nf:=3 ELSE
   IF f<6.5 THEN nf:=5 ELSE
   IF f<9 THEN nf:=8 ELSE
   nf:=10;
   roundup := nf*pwr(10,e);
END;

FUNCTION rounddown (dis : REAL; var pow, nf : integer) : REAL;
VAR l,e,f : REAL;
BEGIN
   l := log10(dis);
   IF l>=0 THEN pow:=trunc(l) ELSE pow:=trunc(l-1);
   e := pow-1; f := dis/pwr(10,e);
   IF f<21 THEN nf:=2*trunc(f/2) ELSE
   IF f<51 THEN nf:=5*trunc(f/5) ELSE
   nf := 10*trunc(f/10);
   rounddown := nf*pwr(10,e);
END;

function cube(num : real) : real;
begin
   cube := Pwr(num, 3);
end;

function cuberoot(num : real) : real;
begin
   cuberoot := Pwr(num, 1/3);
end;

function Diatomic(Structure : StructureType) : Boolean;
begin
   if (Structure=CsClType) or (Structure=NaClType) then
      Diatomic := true
   else Diatomic := false;
end;

{***************** MATHEMATICAL FUNCTIONS AND PROCEDURES ******************}

function DebyeFunct(x : real) : real; far;
var ex, denom : real;
begin
   ex := exp(x);
   if x<0.01 then denom := 1 else denom := sqr(x/(ex - 1));
   DebyeFunct := ex*sqr(x)*denom;
end;

function EinsteinFunct(x : real) : real;
begin
   if x>0.001 then
      EinsteinFunct := sqr(x/(exp(x/2) - exp(-x/2)))
   else EinsteinFunct := 1;
end;

function SchottkyFunct(Data : DataType; T : real) : real;
var S1, S2, S3, ex : real;
    level : integer;
begin
   S1 := 0; S2 := 0; S3 := 0;
   with Data.SchottkyData do begin
      for level := 0 to NoofLevels do begin
         if E[level]/T < 75 then ex := exp(-E[level]/T) else ex := 0;
         S1 := S1 + ex;
         S2 := S2 + sqr(E[level])*ex;
         S3 := S3 + E[level]*ex;
      end;
   SchottkyFunct := (S1*S2 - sqr(S3))/(sqr(T*S1)*3);
   end;
end;

procedure Simpson(Fct : FunctionX; LowerLim, UpperLim, Acc : real;
                  var I : real);
var NoOfSteps, n : integer;
    h, y0, yn, ymid, OldSum, NewSum, OldInt : real;
begin
   {Integrates the function Func(X) between the limits}
   {X = LowerLim and X = UpperLim.}
   {Acc controls the absolute accuracy of the solution.}
   {(the result will probably be in error by less than Acc)}
   NoOfSteps := 4;
   h := (UpperLim - LowerLim)/NoOfSteps;
   y0 := Fct(LowerLim); yn := Fct(UpperLim);
   ymid := Fct((UpperLim - LowerLim)/2);
   OldSum := (y0 + yn)/2 + ymid; NewSum := 0;
   I := 0;
   repeat
      OldInt := I;
      for n := 1 to (NoOfSteps div 2) do begin
         ymid := Fct(LowerLim + h*(2*n-1));
         NewSum := NewSum + ymid;
      end;
      I := 2*h*(OldSum + 2*NewSum)/3;
        {Simpson's rule is I = 4/3 (y0 + 4y1 + 2y2 + 4y3 + ... + 4yn-1 + yn)}
        {OldSum contains the even terms (including half of y0 and yn).}
        {NewSum contains the odd terms.}
      NoOfSteps := NoOfSteps*2; h := h/2;
      OldSum := OldSum + NewSum;
        {These will be even terms for the next iteration}
      NewSum := 0;
   until (abs(I - OldInt) < Acc) and (NoOfSteps > 8);
    {The integral has changed by less than Acc}
end;

function DebyeIntegral(ThetaByT : real): real; far;
var I : real;
begin
   if ThetaByT < 25 then
      simpson(DebyeFunct, 0, ThetaByT, 0.00001, I)
   else I := 25.976;
   {if THETA/T > 25 then integral is effectively to}
   {infinity, with value 4 pi^4 /15}
   DebyeIntegral := I;
end;

procedure IntegrateRealXtal(Data : DataType; T : real; NoOfIntvls : integer;
                            xvector, yvector : MyDVector; var I : real);
var k : integer;
    w : real;
begin
   I := 0;
   for k := 0 to NoOfIntvls do begin
      w := xvector.value(k+1);
      I := I + EinsteinFunct(hbar*w/(kb*T))*yvector.value(k+1);
   end;
end;

Function EquivDBTemp(SpHt, THETA, T, Acc : real) : real;
var TBT, TBTNew, FOld, FNew, Slope : real;
begin
   TBTNew := THETA/T; FNew := DebyeIntegral(TBTNew)*3/cube(TBTNew);
   TBT := 0.9*TBTNew; FOld := DebyeIntegral(TBT)*3/cube(TBT);
   repeat
      Slope := (FNew - FOld)/(TBTNew - TBT);
      TBT := TBTNew; FOld := FNew;
      TBTNew := TBT + (SpHt - FOld)/Slope;
      Fnew := DebyeIntegral(TBTNew)*3/cube(TBTNew);
   until (abs(TBT - TBTNew) < (Acc/T)) or (TBTNew > 10*THETA/T);
   {loop until converged or EquivDBTemp way off-scale}
   EquivDBTemp := TBTNew*T;
end;

{*************** SUBSIDIARY ROUTINES FOR EIGENVALUE HANDLING **************}

PROCEDURE eigsrt(VAR d: DVector;
  	              n: integer);
{ This routine sorts the eigenvalues into descending order.}
{ The method is straight insertion.}

VAR
   k,j,i: integer;
       p: real;
BEGIN
   FOR i := 1 TO n-1 DO BEGIN
      k := i;
      p := d.value(i);
      FOR j := i+1 TO n DO BEGIN
         IF d.value(j) <= p THEN BEGIN
 	         k := j;
      	   p := d.value(j)
      	 END
      END;
      IF k <> i THEN BEGIN
         d.put(k, d.value(i));
      	d.put(i, p);
      END
   END
END;

PROCEDURE eigensolve(VAR  a: DMatrix;
                          N: integer;
                     VAR ev: DVector);
VAR
   e : Dvector; evecs : DMatrix;
BEGIN
   e.init(N); evecs.init(N,N);
   Sym2Trid(N, a, ev, e, false);
   Tridiag(N, ev, e, false, evecs);
   e.free; evecs.free;
   eigsrt(ev, n);
END;

PROCEDURE hermeigensolve(VAR a: complexDMatrix; n: integer; VAR ev: DVector);
VAR b: DMatrix;
    i, j, n2 : integer;
    ev2 : DVector;
BEGIN
   n2:=2*n; b.init(n2, n2); ev2.init(n2);
   for i := 1 to n do
      for j := 1 to n do begin
         b.put(i,j,     a.Re.value(i,j)  );
         b.put(i+n,j+n, b.value(i,j)  );
         b.put(i+n,j,   a.Im.value(i,j)  );
         b.put(i,j+n,   -b.value(i+n,j)  );
      end;
   eigensolve(b, n2, ev2);
   { after sorting eigenvalues should be in pairs. We need alternate values}
   for i := 1 to n do
      ev.put(i, ev2.value(2*i));
   b.free; ev2.free;
END;

{************************ SET UP DYNAMICAL MATRICES ***********************}

procedure SetupTrig(var c, c2, s : TrigSet; qx, qy, qz : real);
begin
   c[1] := cos(qx); c[2] := cos(qy); c[3] := cos(qz);
   c2[1] := cos(2*qx); c2[2] := cos(2*qy); c2[3] := cos(2*qz);
   s[1] := sin(qx); s[2] := sin(qy); s[3] := sin(qz);
end;

procedure setupFCC(qx, qy, qz : real; SetData : SetDataType;
                   var D : DMatrix);
var i, j, k : integer;
    r1, r2, diag : real;
    c, c2, s : TrigSet;
begin
   r1 := 1; r2 := SetData.R;
   SetupTrig(c, c2, s, qx, qy, qz);
   diag := 4*r1 + 2*r2;
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,i,  diag - 2*r1*(c[i]*c[j] + c[i]*c[k])- 2*r2*c2[i]  );
   end;
   for i := 1 to 3 do begin
      j := i mod 3 + 1;
      D.put(i,j,  2*r1*s[i]*s[j]  );
      D.put(j,i,  D.value(i,j)  );
   end;
end;

procedure setupBCC(qx, qy, qz : real; SetData : SetDataType;
                   var D : DMatrix);
var i, j, k : integer;
    r, r1, r2, diag : real;
    c, c2, s : TrigSet;
begin
   r1 := 1; r2 := SetData.R;
   SetupTrig(c, c2, s, qx, qy, qz);
   diag := 8*r1/3 + 2*r2 - (8*r1/3)*c[1]*c[2]*c[3];
   for i := 1 to 3 do begin
      D.put(i,i,  diag - 2*r2*c2[i]  );
   end;
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,j,  (8*r1/3)*s[i]*s[j]*c[k]  );
      D.put(j,i,  D.value(i,j)  );
   end;
end;

procedure setupCsCl(qx, qy, qz : real; SetData : SetDataType;
                    var D : DMatrix);
var i, j, k : integer;
    r, r1, r2, rootrho, diag1, diag2 : real;
    c, c2, s : TrigSet;
begin
   r1 := 1; r2 := SetData.R;
   {r1 relates to unlike atoms, r2 to like atoms}
   qx := qx/2; qy := qy/2; qz := qz/2;
   {Brillouin Zone is reduced in size for diatomic structures}
   SetupTrig(c, c2, s, qx, qy, qz);
   rootrho:=sqrt(SetData.Rho);
   diag1 := 8*r1/3 + 2*r2;
   diag2 := - (8*r1/3)*c[1]*c[2]*c[3];
   for i := 1 to 3 do begin
      D.put(i,i,      rootrho*(diag1 - 2*r2*c2[i])  );
      D.put(i+3,i+3,  D.value(i,i)/SetData.Rho  );
      D.put(i,i+3,    diag2  );
      D.put(i+3,i,    D.value(i,i+3)  );
   end;
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,j,     0  );
      D.put(j,i,     D.value(i,j)  );
      D.put(i+3,j,   (8*r1/3)*s[i]*s[j]*c[k]  );
      D.put(j,i+3,   D.value(i+3,j)  );
      D.put(i,j+3,   D.value(i+3,j)  );
      D.put(j+3,i,   D.value(i+3,j)  );
      D.put(i+3,j+3, D.value(i,j)/SetData.Rho  );
      D.put(j+3,i+3, D.value(i+3,j+3)  );
   end;

end;

procedure setupNaCl(qx, qy, qz : real; SetData : SetDataType;
                    var D : DMatrix);
var i, j, k : integer;
    r1, r2, rootrho, diag : real;
    c, c2, s : TrigSet;
begin
   r1 := 1; r2 := SetData.R;
   {r1, r3 and r5 relate to unlike atoms, r2, r4 and r6 to like atoms}
   SetupTrig(c, c2, s, qx, qy, qz);
   rootrho:=sqrt(SetData.Rho);
   diag :=  2*r1 + 4*r2;
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,i,  rootrho*(diag - 2*r2*(c[i]*c[j] + c[i]*c[k]))  );
      D.put(i+3,i+3,  D.value(i,i)/SetData.Rho  );
      D.put(i,i+3,    -2*r1*c[i]  );
      D.put(i+3,i,    D.value(i,i+3)  );
   end;
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,j,     rootrho*(2*r2*s[i]*s[j])  );
      D.put(i+3,j,   0  );
      D.put(j,i,     D.value(i,j)  );
      D.put(j,i+3,   D.value(i+3,j)  );
      D.put(i,j+3,   D.value(i+3,j)  );
      D.put(j+3,i,   D.value(i+3,j)  );
      D.put(i+3,j+3, D.value(i,j)/SetData.Rho  );
      D.put(j+3,i+3, D.value(i+3,j+3)  );
   end;
end;

procedure setupdiamond(qx, qy, qz : real; SetData : SetDataType;
                   var D : ComplexDMatrix);
var i, j, k : integer;
    r1, r2, r3 : real;
    c, c2, s, s2 : TrigSet;
begin
   r1 := 1; r2 := SetData.R;
   r3 := SetData.NCFC; {r3 is the non-central component of the n-n force}
   qx := qx/2; qy := qy/2; qz := qz/2;
   {Brillouin Zone is reduced in size for diatomic structures}
   SetupTrig(c, c2, s, qx, qy, qz);
   s2[1] := sin(2*qx); s2[2] := sin(2*qy); s2[3] := sin(2*qz);
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.Re.put(i,i,       4*r1 + 4*r2 - 2*r2*(c2[i]*c2[j] + c2[i]*c2[k])  );
      D.Im.put(i,i,       0  );
      D.Re.put(i+3,i+3,   D.Re.value(i,i)  );
      D.Im.put(i+3,i+3,   D.Im.value(i,i)  );
      D.Re.put(i,i+3,    -4*r1*c[i]*c[j]*c[k]  );
      D.Im.put(i,i+3,    -4*r1*s[i]*s[j]*s[k]  );
      D.Re.put(i+3,i,     D.Re.value(i,i+3)  );
      D.Im.put(i+3,i,    -D.Im.value(i,i+3)  );
   end;
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.Re.put(i,j,       2*r2*s2[i]*s2[j]  );
      D.Im.put(i,j,       0  );
      D.Re.put(j,i,       D.Re.value(i,j)  );
      D.Im.put(j,i,       D.Im.value(i,j)  );
      D.Re.put(i+3,j+3,   D.Re.value(i,j)  );
      D.Im.put(i+3,j+3,   D.Im.value(i,j)  );
      D.Re.put(j+3,i+3,   D.Re.value(i,j)  );
      D.Im.put(j+3,i+3,   D.Im.value(i,j)  );
      D.Re.put(i+3,j,     4*(r1-r3)*s[i]*s[j]*c[k]  );
      D.Im.put(i+3,j,    -4*(r1-r3)*c[i]*c[j]*s[k]  );
      D.Re.put(j+3,i,     D.Re.value(i+3,j)  );
      D.Im.put(j+3,i,     D.Im.value(i+3,j)  );
      D.Re.put(i,j+3,     D.Re.value(i+3,j)  );
      D.Im.put(i,j+3,    -D.Im.value(i+3,j)  );
      D.Re.put(j,1+3,     D.Re.value(i,j+3)  );
      D.Im.put(j,i+3,     D.Im.value(i,j+3)  );
   end;
end;

{************************ SELECT DYNAMICAL MATRIX ***********************}

procedure SolveDMatrix(Structure : StructureType; qx, qy, qz : real;
                       Data : DataType; var w : freqarray;
                       var fail : boolean);
var i, N : integer;
    w2, FScale : real;
    D : DMatrix;
    CD : ComplexDMatrix;
    ev : DVector;
begin
   if Structure > FCC then N:=6 else N:=3;
   ev.init(N);
   if Structure <> DiamondType then
      D.init(N,N)
   else begin
      CD.Re.init(N,N); CD.Im.init(N,N); end;
   case Structure of
      FCC :   setupFCC(qx, qy, qz, Data.SetData, D);
      BCC :   setupBCC(qx, qy, qz, Data.SetData, D);
      CsClType :  setupCsCl(qx, qy, qz, Data.SetData, D);
      NaClType :  setupNaCl(qx, qy, qz, Data.SetData, D);
      DiamondType :  setupdiamond(qx, qy, qz, Data.SetData, CD);
      nostructure : ;
   end;
   fail:=false;
   if Structure <> DiamondType then
      eigensolve(D, N, ev)
   else
      hermeigensolve(CD, N, ev);
   FScale := Data.FScale;
   if Data.SetData.UseDebye then {FScale scales as Theta^2}
      FScale := FScale*sqr(Data.UserTheta/Data.Theta);
   for i:=1 TO N do begin
      w2 := ev.value(i);
      if (ABS(w2) < 1.0E-6) then w2 := 0;
      {rounding errors may produce very small -ve w's when w should be zero}
      if (w2 < 0) then begin
         fail := true; w2 := 0;
      end;
      w[i] := SQRT(FScale*w2);
      {FScale is force constant multiplier for secular determinant.}
   end;
   ev.free;
   if Structure <> DiamondType then
      D.free
   else begin
      CD.Re.free; CD.Im.free; end;
end;

{*************************** MAIN CALCULATIONS **************************}

procedure CalcFScale(var Data : DataType);
var a, BulkMod, density, MolWt, R, NCFC, FScale : real;
begin
 if Data.Substance = USERDEFINED then begin
    BulkMod := Data.SetData.BulkMod;
    density := Data.SetData.density;
    MolWt := Data.SetData.MolWt; end
 else begin
    BulkMod := BulkModData[ord(Data.Substance)];
    density := densityData[ord(Data.Substance)];
    MolWt := MolWtData[ord(Data.Substance)];
 end;
 R := Data.SetData.R;
 NCFC := Data.SetData.NCFC;
 case Data.Structure of
   FCC : begin
            a := cuberoot(4*MolWt/(1000*AvNo*density))/2;
            {The unit cube is of side 2a, but contains 4 atoms (molecules)}
            FScale := (3/4)*(BulkMod*1E9/(density*a*a))/(1 + R);
         end;
   BCC : begin
            a := cuberoot(2*MolWt/(1000*AvNo*density))/2;
            {This time the unit cube is of side 2a, and contains 2 atoms}
            FScale := (3/4)*(BulkMod*1E9/(density*a*a))/(1 + R);
         end;
   NaClType :
         begin
            a := cuberoot(4*MolWt/(1000*AvNo*density))/2; {basically simple cubic}
            {The Dynamical matrix is calculated over a cell of}
            {size 2a, containing 4 molecules (8 atoms)}
            FScale := 3*((BulkMod*1E9)/(density*a*a))/(1 + 4*R);
            {units of Bulk Modulus are GN m-2}
            {Fscale is the quantity gamma/at. mass}
            Fscale := Fscale*sqrt(2+Data.SetData.Rho+1/Data.SetData.Rho)/2;
            {For NaCl, the bulk modulus relates to gamma/mean mass,}
            {ie 2*gamma/(m1+m2). What is needed is gamma/sqrt(m1m2)}
               end;
   CsClType :
         begin
            a := cuberoot(MolWt/(1000*AvNo*density))/2; {as BCC}
            {The unit cube is again of side 2a, and contains 1 molecule}
            FScale := (3/4)*(BulkMod*1E9/(density*a*a))/(1 + R);
            Fscale := Fscale*sqrt(2+Data.SetData.Rho+1/Data.SetData.Rho)/2;
            {For CsCl, the bulk modulus relates to gamma/mean mass,}
            {ie 2*gamma/(m1+m2). What is needed is gamma/sqrt(m1m2)}
         end;
   DiamondType :
         begin
            a := cuberoot(8*MolWt/(1000*AvNo*density))/2;
            {The unit cube is of side 2a, and contains 8 atoms}
            FScale := 3*(BulkMod*1E9/(density*a*a))/
                     (3/2 + 4*R - 2*NCFC);
         end;
   nostructure:
         begin
            a := cuberoot(MolWt/(1000*AvNo*density));
            {None of this really matters for nostructure}
            FScale := 3*((BulkMod*1E9)/(density*a*a))/(1 + 4*R);
         end;
 end {case};
 Data.FScale := FScale;
end;

procedure CalcTheta(var Data : DataType);
var C11, C12, C44, G, Gv, Gr, Vl, Vt, vol : real;
    R, NCFC, BulkMod, density, MolWt : real;
    NoOfAtoms : integer;
begin
   if Data.Substance = USERDEFINED then begin
      BulkMod := Data.SetData.BulkMod;
      density := Data.SetData.density;
      MolWt := Data.SetData.MolWt end
   else begin
      BulkMod := BulkModData[ord(Data.Substance)];
      density := densityData[ord(Data.Substance)];
      MolWt := MolWtData[ord(Data.Substance)];
   end;
   R := Data.SetData.R; NCFC := Data.SetData.NCFC;
   case Data.Structure of
      NaClType:   begin
                     C11 := 3*1E9*BulkMod*(1 + 2*R)/(1 + 4*R);
                     C44 := 3*1E9*BulkMod*R/(1 + 4*R); C12 := C44;
                  end;
      FCC :       begin
                     C11 := 3*1E9*BulkMod*(2 + 4*R)/(4 + 4*R);
                     C44 := 3*1E9*BulkMod/(4 + 4*R); C12 := C44;
                  end;
      BCC, CsClType :
                  begin
                     C11 := 3*1E9*BulkMod*(1 + 3*R)/(3 + 3*R);
                     C44 := 3*1E9*BulkMod/(3 + 3*R); C12 := C44;
                  end;
      DiamondType :
                  begin
                     C11 := 3*1E9*BulkMod*(1 + 4*R)/(3 - 4*NCFC + 8*R);
                     C12 := 3*1E9*BulkMod*(1 - 2*NCFC + 2*R)/
                                           (3 - 4*NCFC + 8*R);
                     C44 := 3*1E9*BulkMod*(2*NCFC - sqr(NCFC) + 2*R)/
                                           (3 - 4*NCFC + 8*R);
                  end;
   end {case};
   Gv := (C11 - C12 + 3*C44)/5;
   if (3*C11 - 3*C12 + 4*C44) < C11/10000 then
      Gr:=0 {denominator will vanish or is negative}
   else
      Gr := 5*C44*(C11 - C12)/(3*C11 - 3*C12 + 4*C44);
   G := (Gv + Gr)/2; {best value for G is mean of Gv and Gr}
   if G < C11/10000 then G:=C11/10000; {to avoid division by zero error}
   Vl := sqrt((BulkMod + 4*G/3)/density); Vt := sqrt(G/density);
   vol := MolWt/(1000*density); {vol is molar volume - mass/density}
   if Diatomic(Data.Structure) then NoOfAtoms := 2 else NoOfAtoms := 1;
   {For diatomic solids the number of atoms is 2N and thus the number of
    degrees of freedom is 6N, as used to calculate Debye frequency.}
   Data.Theta := (hbar/kB)*cuberoot((18*NoOfAtoms*AvNo*pi*pi/vol)/
                                    (1/cube(Vl) + 2/cube(Vt)));
end;

procedure CalcDofStates(var Data : DataType;
                        var xvector, dosvector : MyDVector);
var l, l3, j, k, kx, ky, kz, kxmax, kxmax2, kymax, kzmax : integer;
    BZsize, qx, qy, qz, wt, wfrac : real;
    NoOfIntvls, NoOfBranches, wint, ifail : integer;
    w : freqarray;
    fail : boolean;
    SumOfTerms, stripwidth : real;
    doscompvector : freqvector;
    s : string;
begin
   WaitMessage.Show;
   ifail := 0;
   if Data.Structure > FCC then NoOfBranches:=6 else NoOfBranches:=3;
   case Data.Structure of
      nostructure : ;
      CsClType : BZsize := 6;
      FCC, NaClType, DiamondType : BZsize := 12;
      BCC : BZsize := 24;
   end {case};
     {for a simple cubic BZ, the irreducible portion is 1/6 of an octant.}
     {For FCC types, the BZ is further reduced by 1/2, and for BCC 1/4.}
   kxmax := round(cuberoot(BZsize*Data.SetData.NoofModes));
   NoOfIntvls := 7*round(cuberoot(Data.SetData.NoofModes));
   Data.NoOfIntvls := NoOfIntvls;
      {This is a compromise factor for the number of intervals into which}
      {to divide the calculated points, to give reasonable resolution}
      {(for the chosen number of modes) without excessive noise}
   stripwidth := 1.5*Data.wrange/NoOfIntvls;
      {width of each histogram strip - in units of frequency}
   xvector.free; dosvector.free; {clear old values before setting up again}
   xvector.init(NoOfIntvls+1);
   for k := 0 to NoOfIntvls do xvector.put(k+1, k*stripwidth);
   dosvector.init(NoOfIntvls+1); dosvector.fill(0.0);
   for l := 1 to NoOfBranches do begin
      doscompvector[l].init(NoOfIntvls+1);
      doscompvector[l].fill(0.0);
   end;
   SumOfTerms := 0;
   kxmax2 := trunc(kxmax/5);
   for kx := 1 to 3*kxmax2 do begin
      qx := kx*pi/(3*kxmax); {do the first 1/3 of kx-space at closer mesh}
      for ky := 1 to kx do begin
         qy := ky*pi/(3*kxmax);
         for kz := 1 to ky do begin
            qz := (kz - 0.5)*pi/(3*kxmax);
            SolveDMatrix(Data.Structure, qx, qy, qz, Data, w, fail);
            WaitMessage.Update;
            if fail=true then ifail := kz;
            wt := 1/27;
            for l := 1 to NoOfBranches do begin
               SumOfTerms := SumOfTerms + wt;
               w[l] := w[l]/stripwidth; {rescale frequencies to strip width}
               wint := trunc(w[l]);
               wfrac := w[l] - wint;
               if wint<NoOfIntvls then begin
                  doscompvector[l].AddToElement(wint+1, (1 - wfrac)*wt);
                  doscompvector[l].AddToElement((wint+1)+1, wfrac*wt);
                     {+1 is to because dvectors must start from 1,}
                     {whereas wint starts from 0}
               end;
            end {l};
         end {kz};
      end {ky};
   end {kx};
   for kx := kxmax2+1 to kxmax do begin
      qx := kx*pi/kxmax;
      case Data.Structure of
         CsClType                : kymax := kx;
         BCC, nostructure        : if kx <= kxmax/2 then kymax := kx
                                      else kymax :=  kxmax - kx;
         FCC, NaClType, DiamondType: if kx <= 3*kxmax/4 then kymax := kx
                                        else kymax :=  trunc(3*kxmax/2) - kx;
      end {case};
      for ky := 1 to kymax do begin
         qy := ky*pi/kxmax;
         case Data.Structure of
            nostructure, BCC, CsClType : kzmax := ky;
            FCC, NaClType, DiamondType :
                                if ky <= kxmax/2 then kzmax := ky
                                   else kzmax := trunc(3*kxmax/2) - kx - ky;
         end {case};
         for kz := 1 to kzmax do begin
            qz := (kz - 0.5)*pi/kxmax;
            SolveDMatrix(Data.Structure, qx, qy, qz, Data, w, fail);
            WaitMessage.Update;
            if fail=true then ifail := kz;
            if (kx = kxmax) then wt := 0.5 else wt := 1;
            for l := 1 to NoOfBranches do begin
               SumOfTerms := SumOfTerms + wt;
               w[l] := w[l]/stripwidth;
               wint := trunc(w[l]);
               wfrac := w[l] - wint;
               if wint<NoOfIntvls then begin
                  doscompvector[l].AddToElement(wint+1, (1 - wfrac)*wt);
                  doscompvector[l].AddToElement((wint+1)+1, wfrac*wt);
               end;
            end {l};
         end {kz};
      end {ky};
   end {kx};
   if ifail>0 then begin
      s := NumStr(ifail, 3, 0);
      announce(concat('failure solving eigenvalues at k value ', s));
   end;
   Data.dosmax := 0;
   for k := 0 to NoOfIntvls do begin
      WaitMessage.Update;
      for l := 1 to 3 do begin
         for j := 1 to (NoOfBranches div 3) - 1 do begin
            doscompvector[l].AddToElement(k+1,
                                          doscompvector[l+3*j].Value(k+1));
         end;
         doscompvector[l].put(k+1, doscompvector[l].Value(k+1)/SumOfTerms);
         {scale down so area under histogram = 1}
         dosvector.AddToElement(k+1, doscompvector[l].Value(k+1));
      end {l};
      if Data.dosmax < abs(dosvector.Value(k+1)) then
          Data.dosmax := abs(dosvector.Value(k+1));
   end {k};
   WaitMessage.Hide;
   for l := 1 to NoOfBranches do
      doscompvector[l].free;
end;

procedure SetwRange(var Data : DataType);
var qx, qy, qz : real;
    dirn : DirnType;
    NoOfBranches, l, k, kmax, ifail : integer;
    w : freqarray;
    fail : boolean;
    s : string;
begin
 WaitMessage.Show;
 kmax := 25; {search a grid of 25 points on each symmetry direction}
             {to find an approximate value for wmax}
 if Data.Structure > FCC then NoOfBranches:=6 else NoOfBranches:=3;
 Data.wrange := 0.0; ifail := 0;
 for dirn := IOO to III do begin
   for k := 1 to kmax do begin
      qx := k*pi/kmax; {no need to bother with xrange[] here}
      if dirn > IOO then qy := qx else qy := 0.0;
      if dirn > IIO then qz := qx else qz := 0.0;
      SolveDMatrix(Data.Structure, qx, qy, qz, Data, w, fail);
      WaitMessage.Update;
      if fail=true then ifail := k;
      for l := 1 to NoOfBranches do
         if Data.wrange < w[l] then Data.wrange := w[l];
   end; {for k}
   if ifail>0 then begin
      s := NumStr(ifail, 3, 0);
      announce(concat('failure solving eigenvalues at k value ', s));
   end;
 end {for dirn};
 WAitMessage.Hide;
end;

procedure CalcSpHt(var Data : DataType);
   procedure SetTemperature(Data : DataType; k : integer; var T : real);
   begin
      T := Data.Tzoom*(k-1)/100;
      if Data.SetData.DisplayTCubed then
         temperature.put(k, cube(T) )
      else
         temperature.put(k, T);
   end;
var k, Points : integer;
    THETA, T, I, EinsteinFreq : real;
   {NoOfIntvls : integer; }
   {xvector, dosvector : MyDVector;}
   {THESE ARE GLOBAL VARIABLES USED TO RETAIN DofStates VALUES}
begin
   CalcFScale(Data); CalcTheta(Data);
   if Data.SetData.UseDebye then
      THETA := Data.UserTheta
      else THETA := Data.Theta;
   temperature.put(1, 0); SpHt.put(1, 0);
   case Data.Model of
      None        : ;
      Einstein    : for k := 2 to NoOfPoints do begin
                       SetTemperature(Data, k, T);
                       SpHt.put(k, EinsteinFunct(0.75*THETA/T));
                    end;
      Debye       : for k := 2 to NoOfPoints do begin
                       SetTemperature(Data, k, T);
                       I := DebyeIntegral(THETA/T);
                       SpHt.put(k, 3*cube(T/THETA)*I);
                    end;
      Combined    : for k := 2 to NoOfPoints do begin
                       SetTemperature(Data, k, T);
                       I := DebyeIntegral(THETA/T);
                       Einsteinfreq := (0.85+0.2*sqrt(Data.SetData.Rho))*THETA/T;
                       SpHt.put(k, 0.5*(EinsteinFunct(EinsteinFreq)
                                        + 3*cube(T/THETA)*I));
                    end;
      RealCrystal : begin
                       SetwRange(Data);
                       if Data.DoSChange then begin
                          CalcDofStates(Data, xvector, dosvector);
                          Data.DoSChange := false;
                       end;
                       for k := 2 to NoOfPoints do begin
                          SetTemperature(Data, k, T);
                          IntegrateRealXtal(Data, T, Data.NoOfIntvls,
                                            xvector, dosvector, I);
                          SpHt.put(k, I);
                       end;
                    end;
      Schottky    : for k := 2 to NoOfPoints do begin
                       SetTemperature(Data, k, T);
                       I := SchottkyFunct(Data, T);
                       if not Data.SchottkyData.Schottkyonly then
                          I := I + 3*cube(T/THETA)*DebyeIntegral(THETA/T);
                       SpHt.put(k, I);
                    end;
      Experiment  : for k := 2 to NoOfPoints do begin
                       if k<=Data.DataPoints then begin
                          if Data.SetData.DisplayTCubed then
                             temperature.put(k, cube(Data.SpHtTemp[k]))
                          else
                             temperature.put(k, Data.SpHtTemp[k]);
                          SpHt.put(k,Data.SpHtData[k]/(3*8.314));
                       end else begin
                          temperature.put(k, 0);
                          SpHt.put(k,0);
                       end;
                    end;
   end;
   if not Data.SetData.DisplaySpHt then begin
      WaitMessage.Show;
      DBT.put(1, THETA);
      if Data.Model = Experiment then
         begin
            Points := Data.Datapoints;
            DBT.put(1, 0);
            for k := Points+1 to NoOfPoints do
               DBT.put(k, 0);
         end
         else Points := NoOfPoints;
      for k := 2 to Points do begin
         WaitMessage.Update;
         DBT.put(k, EquivDBTemp(SpHt.value(k), THETA,
                                temperature.value(k), 0.0001*THETA));
      end;
      WaitMessage.Hide;
   end;
end;

{********************* SET UP AND HANDLE INPUT SCREENS ********************}

procedure ReadPlotScreen(var Data : DataType; var Menu : Tmenu;
                         var Redraw, cancel : boolean);
var
   DataScreen : TInputScreen;
   Num, fields : integer;
   OldDSH, OldDTC : boolean;
begin
   with DataScreen do begin
      init; fields:=8;
      DefineInputPort (0.23, 0.77, 0.10, 0.92);
      LoadLine('PLOT-WHAT');
      LoadLine(' #1 Plot Specific Heat');
      if Data.Model<>Schottky then begin
         LoadLine(' #1 Plot Specific Heat vs T^3');
         fields:=9;
         if Data.Model<>Debye then begin
            LoadLine(' #1 Plot Debye Temperature');
            fields:=10;
         end;
      end;
      LoadLine('PLOT-HOW');
      LoadLine(' #2 Show old curves');
      LoadLine(' #2 Show previous curve');
      LoadLine(' #2 Show new curve only');
      LoadLine('PLOT-WHEN');
      LoadLine(' #3 Automatic drawing');
      LoadLine(' #3 Press F8-Plot to draw new curve');
      LoadLine('');
      LoadLine(' #T Display density of states (for');
      Loadline('      Realistic Density of States model)');
      LoadLine('');
      LoadLine(' #F Plot Schottky specific heat');
      LoadLine('      with lattice background');
      LoadLine('');
      LoadLine('    [  Ok  ]    [ Help ]    [Cancel]');
      with Data.SetData do begin
         if Data.SetData.DisplayTCubed then Num:=2
         else if Data.SetData.DisplaySpHt then Num:=1 else Num:=3;
         SetRadioButton('1',Num);
         if Data.SetData.Superimpose then Num:=1
         else if Data.SetData.ShowPrevious then Num:=2 else Num:=3;
         SetRadioButton('2',Num);
         if Data.SetData.AutoDraw then Num:=1 else Num:=2;
         SetRadioButton('3',Num);
         SetBoolean(fields-1,Data.SetData.ShowDoS);
         SetBoolean(fields,not Data.SchottkyData.SchottkyOnly);
      end;
      SetHelpFile('Spheat.hlp','plotoptns help');
      AcceptScreen;
      if not Canceled then with Data.SetData do begin
         cancel := false;
         Num := GetRadioButton('1');
         OldDSH := Data.SetData.DisplaySpHt;
         OldDTC := Data.SetData.DisplayTCubed;
         Data.SetData.DisplaySpHt := (Num<>3);
         Data.SetData.DisplayTCubed := (Num=2);
         Menu.Rowactivate(2,5,(Num=1));
         Menu.Rowactivate(2,2,(Num<>3));
         if (OldDSH <> Data.SetData.DisplaySpHt) or
            (OldDTC <> Data.SetData.DisplayTCubed) then
               Redraw:=true else Redraw:=false;

         Num := GetRadioButton('2');
         Data.SetData.Superimpose := (Num=1);
         Data.SetData.ShowPrevious := (Num=2);

         Num := GetRadioButton('3');
         Data.SetData.AutoDraw := (Num=1);

         Data.SetData.ShowDoS := GetBoolean(fields-1);
         Data.SchottkyData.SchottkyOnly := not GetBoolean(fields);

         Data.DoSChange := true;
      end
      else begin
         Redraw:=false;
         cancel:=true;
      end;
      done;
   end;
end;

procedure ReadDataScreen(var Data : DataType; var cancel : boolean);
var
   DataScreen : TInputScreen;
   extras, Num, FieldNum : integer;
   OldMolWt : real;
   OldStructure : StructureType;
begin
   OldStructure := FCC;
   with DataScreen do begin
      init;
      if Data.Substance = USERDEFINED then
         DefineInputPort (0.20, 0.80, 0.06, 0.92)
      else DefineInputPort (0.20, 0.80, 0.21, 0.79);
      LoadLine('GENERAL');
      LoadLine('   No. of modes in D.o.States : {       }');
      LoadLine('      (50-10000)');
      if Data.Substance = USERDEFINED then begin
         LoadLine('');
         LoadLine('SUBSTANCE DETAILS');
         LoadLine('   Bulk Modulus (10^9 N/m^2)  : {       }');
         LoadLine('   Density (kg m^-3)          : {       }');
         LoadLine('   Molecular weight           : {       }');
      end;
      LoadLine('');
      LoadLine('   Ratio of force consts      : {       }');
      if Data.Structure = DiamondType then begin
         LoadLine('   Non-central force (ratio)  : {       }');
         LoadLine('   (-0.2 to +0.5)'); end
      else
      if Data.Structure > DiamondType then begin
         LoadLine('   Ratio of masses (>1)       : {       }');
         LoadLine('');
      end;
      if Data.Substance = USERDEFINED then begin
         LoadLine('');
         LoadLine('STRUCTURE');
         LoadLine('                   Monatomic');
         LoadLine('                FCC #1   BCC #1');
         LoadLine('                   Diatomic');
         LoadLine('   CsCl Type #1   NaCl Type #1   Diamond #1');
      end
      else begin
         LoadLine('');
         LoadLine('#1  Use physical parameters');
         LoadLine('#1  Use Debye temperature');
         if Data.SetData.UseDebye then
            LoadLine('   Debye Temperature          : {       }');
      end;
      LoadLine('');
      LoadLine('    [  Ok  ]      [ Help ]      [Cancel]');
      with Data.SetData do begin
         SetNumber(1,NoofModes);
         SetNumberLimits(1,50,10000);
         if Data.Substance = USERDEFINED then begin
            SetNumber(2,BulkMod);  {use old values as defaults}
            SetNumberLimits(2,1E-2,1E3);
            SetNumber(3,density);
            SetNumberLimits(3,1,1E6);
            SetNumber(4,MolWt);
            SetNumberLimits(4,1,1000);
            SetRadioButton('1',StructNum);
            extras := 3;
            OldStructure := Data.Structure;
            OldMolWt := MolWt;
         end
         else begin
            extras := 0;
            if UseDebye then FieldNum := 2 else FieldNum := 1;
            SetRadioButton('1',FieldNum);
            if Data.Structure >= DiamondType then
               FieldNum := 6 else FieldNum := 5;
            if UseDebye then begin
               SetNumber(FieldNum, Data.UserTheta);
               SetNumberLimits(FieldNum, 1,10000);
            end;
         end;
         SetNumber(2+extras,R);
         SetNumberLimits(2+extras,0,1E7);
         if Data.Structure = DiamondType then begin
            SetNumber(3+extras,NCFC);
            SetNumberLimits(3+extras,-0.2,+0.5);
         end
         else
         if Data.Structure > DiamondType then begin
            SetNumber(3+extras,Rho);
            SetNumberLimits(3+extras,1,1E7);
         end;
      end;
      SetHelpFile('Spheat.hlp','set data help');
      AcceptScreen;
      if not Canceled then with Data.SetData do begin
         cancel:=false;
         NoofModes := trunc(GetNumber(1));
         if Data.Substance = USERDEFINED then begin
            BulkMod := GetNumber(2);
            density := GetNumber(3);
            MolWt := GetNumber(4);
            Num := GetRadioButton('1');
         end
         else begin
            if UseDebye then begin
               if Data.Structure >= DiamondType then
                  FieldNum := 6 else FieldNum := 5;
               Data.UserTheta := GetNumber(FieldNum);
            end;
            Num := StructNum;
            if GetRadioButton('1')=2 then begin
               if not UseDebye then Data.UserTheta := Data.Theta;
               UseDebye := true;
            end else UseDebye := false;
         end;
         R := GetNumber(2+extras);
         if (R<0.1) and ((Data.Structure=DiamondType) or (Data.Structure=NaClType)) then R:=0.1;
            {small R with these lattices gives silly results for G}
         if Data.Structure = DiamondType then
            NCFC := GetNumber(3+extras)
         else
         if Data.Structure > DiamondType then
            Rho := GetNumber(3+extras);
         if Data.Substance = USERDEFINED then
            case Num of
               1 : Data.Structure := FCC;
               2 : Data.Structure := BCC;
               3 : Data.Structure := CsClType;
               4 : Data.Structure := NaClType;
               5 : Data.Structure := DiamondType;
            end;

         if Diatomic(Data.Structure) and not Diatomic(OldStructure)
            and (MolWt=OldMolWt) then MolWt := 2*MolWt;
         if not Diatomic(Data.Structure) and Diatomic(OldStructure)
            and (MolWt=OldMolWt) then MolWt := MolWt/2;
         if Data.Structure <= DiamondType then Data.VaryForce := true;
         if Num <> StructNum then Data.DoSChange := true;
         StructNum := Num;
      end else cancel:=true;
      done;
   end;
end;

procedure ReadSchottkyDataScreen(var SchottkyData : SchottkyDataType;
                                 var cancel : boolean);
var
   level : integer;
   factor : real;
   SchottkyDataScreen : TInputScreen;
begin
   with SchottkyDataScreen do begin
      init;
      DefineInputPort (0.16, 0.84, 0.1, 0.9);
      LoadLine('');
      LoadLine('SCHOTTKY DATA');
      LoadLine('');
      LoadLine(' Energy levels');
      LoadLine('');
      LoadLine('   1) Individual energy levels (1-4)      #1');
      LoadLine('   2) Equal spacing                       #1');
      LoadLine('   3) increasing spacing (n, 2n, 3n, ..)  #1');
      LoadLine('');
      LoadLine(' Energy level data');
      LoadLine('');
      LoadLine('   No. of excited energy levels (1-99) : {   }');
      LoadLine('   Spacing of first level (deg K)      : {      }');
      LoadLine('   Spacing of 2nd level (if needed)    : {      }');
      LoadLine('   Spacing of 3rd level (if needed)    : {      }');
      LoadLine('   Spacing of 4th level (if needed)    : {      }');
      LoadLine('');
      LoadLine('    [  Ok  ]      [ Help ]      [Cancel]');
      with SchottkyData do begin
         SetBoolean(1,IndividualLevels);
         SetBoolean(2,EqualSpacing);
         SetBoolean(3,nfoldSpacing);
         SetNumber(4,NoofLevels);
         SetNumberLimits(4,1,99);
         SetNumber(5,E[1]);
         SetNumberLimits(5,1E-3,1E7);
         SetNumber(6,E[2]);
         SetNumberLimits(6,1E-3,1E7);
         SetNumber(7,E[3]);
         SetNumberLimits(7,1E-3,1E7);
         SetNumber(8,E[4]);
         SetNumberLimits(8,1E-3,1E7);
      end;
      SetHelpFile('Spheat.hlp','schottky data help');
      AcceptScreen;
      if not Canceled then with SchottkyData do begin
         cancel:=false;
         IndividualLevels := GetBoolean(1);
         EqualSpacing := GetBoolean(2);
         nfoldSpacing := GetBoolean(3);
         NoofLevels := trunc(GetNumber(4));
         if IndividualLevels and (NoofLevels > 4) then NoofLevels := 4;
         E[1] := GetNumber(5); factor := 1;
         if IndividualLevels then begin
            E[2] := GetNumber(6);
            E[3] := GetNumber(7);
            E[4] := GetNumber(8);
         end
         else if EqualSpacing then
            for level := 2 to NoofLevels do
               E[level] := level*E[1]
         else
            for level := 2 to NoofLevels do begin
               factor := factor + level;
               E[level] := factor*E[1];
            end;
      end else cancel:=true;
      done;
   end;
end;

{***************************** SET UP SLIDERS ****************************}

procedure DrawSliders(Data : DataType);
var FInit, MInit, FMax, MMax, TMin, TMax : real;
    decimals : integer;
    Ftop, Mtop, Tbottom, Ttop : string;
begin
   if Data.SliderResolution = Fine then begin
      FMax := 2; MMax := 10; Ftop := '2'; Mtop := '10'; decimals := 2; end
   else if Data.SliderResolution = Medium then begin
      FMax := 20; MMax := 100; Ftop := '20'; Mtop := '100'; decimals := 1;
   end
   else begin
      FMax := 200; MMax := 500; Ftop := '200'; Mtop := '500';
      decimals := 0; end;
   FInit := Data.SetData.R;
   if FInit > FMax then FInit := FMax;
   MInit := Data.SetData.Rho;
   if MInit > MMax then MInit := MMax;
   if NoOfSliders = 2 then begin
      Sliders.Erase(2, blakk); Sliders.Delete(2); end;
   Sliders.Delete(1);
   Sliders.Create(1, 0.1, FMax, FInit, 0.91, 0.235,
                  0.565, decimals, '.1', Ftop, 'force ratio', true);
   if Data.SetData.UseDebye then begin
      NoOfSliders := 2;
      TMin := 10*round(Data.Theta/50); TMax := 10*round(Data.Theta/6);
      str(TMin:4:0, Tbottom); str(TMax:5:0, Ttop);
      Sliders.Create(2, TMin, TMax, Data.UserTheta, 0.91, 0.60,
                     0.93, 1, Tbottom, Ttop, 'Debye temp.', true);
   end
   else if Data.Structure > DiamondType then begin
      NoOfSliders := 2;
      Sliders.Create(2, 1, MMax, MInit, 0.91, 0.60,
                     0.93, decimals, '1', Mtop, 'mass ratio', true);
   end
   else if Data.Structure = DiamondType then begin
      NoOfSliders := 2;
      Sliders.Create(2, -0.2, 0.5, Data.SetData.NCFC, 0.91, 0.60,
                     0.93, decimals, '-0.2', '0.5', 'n/cen ratio', true);
   end
   else NoOfSliders := 1;
   Sliders.DrawAll;
   ResButtons.Delete(0); ResButtons.Create(0, 0.88, 0.19, 'Fine');
   ResButtons.Delete(1); ResButtons.Create(1, 0.88, 0.14, 'Medium');
   ResButtons.Delete(2); ResButtons.Create(2, 0.88, 0.09, 'Coarse');
   ResButtons.number := ord(Data.SliderResolution);
   ResButtons.DrawAll;
   SetTextJustify(LeftText, TopText);
   SetColor(Whitte);
end;

{*********************** GENERAL PRINTING PROCEDURES **********************}

procedure brokenvertline(x, y1, y2 : integer);
var stp, gap : integer;
begin
   gap := (y2 - y1) div 10;
   for stp := 0 to 9 do
      lineTP(x, y1 + stp*gap + gap div 4, x, y1 + stp*gap + 3 * (gap div 4));
end;

procedure printratio(ratio : real; ForceorMass : string; row : integer);
var s : string;
begin
   SelectViewPort(3);
   if ratio < 999 then s := NumStr(ratio, 6, 2) else s := NumStr(ratio, 6, 0);
   s := concat('ratio 1:', s);
   RubOut(25, row, 22, DarkGray);
   SetColor(Whitte);
   if ForceorMass <> '' then begin
      Print(25, row, ForceorMass); Print(31, row, s);
   end;
end;

procedure ShowColors(Model : ModelType; NewModel : boolean);
var s : string;
    modelno, row : integer;
begin
   case Model of
      None:        s := '';
      Einstein:    s := 'Einstein Model';
      Debye:       s := 'Debye Model';
      Combined:    s := 'Combined Model';
      RealCrystal: s := 'Real Crystal';
      Schottky:    s := 'Schottky Sp.Ht.';
      Experiment:  s := 'Experiment Data';
   end;
   modelno := ord(Model) - 1;
   if NewModel then row := 2 else row := 3;
   SelectViewPort(3); SelectScale(3);
   RubOut(1, row, 22, DarkGray);
   SetColor(LightBlue + modelno);
   if Model = Experiment then Print(1, row, 'o o o')
   else if Model <> None then
      PlotLine(0.5*ColWidth, (3.5-row)*RowHt, 6*ColWidth, (3.5-row)*RowHt);
   SetColor(Whitte);
   Print(8, row, s);
end;

procedure LabelSubstance(Substance : SubstanceType);
var s : string;
begin
   SelectViewPort(3);
   SetTextJustify(LeftText, TopText);
   SetColor(Whitte);
   case Substance of
      USERDEFINED :  s := 'UserDefined';
      Argon       :  s := 'SOLID ARGON';
      Aluminium   :  s := 'ALUMINIUM';
      Silver      :  s := 'SILVER';
      Iron        :  s := 'IRON';
      CsCl        :  s := 'CsCl';
      NaCl        :  s := 'NaCl';
      diamond     :  s := 'DIAMOND';
   end {case};
   RubOut(12, 1, 11, DarkGray);
   Print(1, 1, 'SUBSTANCE: ');
   Print(12, 1, s);
end;

procedure LabelStructure(Structure : StructureType);
var s : string;
begin
   SelectViewPort(3);
   SetTextJustify(LeftText, TopText);
   SetColor(Whitte);
   case Structure of
      BCC     :  s := 'BCC STRUCTURE';
      FCC     :  s := 'FCC STRUCTURE';
      CsClType:  s := 'CsCl STRUCTURE';
      NaClType:  s := 'NaCl STRUCTURE';
      DiamondType :  s := 'DIAMOND STRUCTURE';
   end {case};
   RubOut(27, 1, 18, DarkGray);
   Print(27, 1, s);
end;


procedure LabelData(Data : DataType);
var s : string;
    a, BulkMod, density, MolWt : real;
begin
   if Data.Substance = USERDEFINED then begin
      BulkMod := Data.SetData.BulkMod;
      density := Data.SetData.density;
      MolWt := Data.SetData.MolWt; end
   else begin
      BulkMod := BulkModData[ord(Data.Substance)];
      density := densityData[ord(Data.Substance)];
      MolWt := MolWtData[ord(Data.Substance)];
   end;
   SelectViewPort(3);
   SetTextJustify(LeftText, TopText);
   SetColor(Whitte);
   Print(47, 1, 'Bulk Modulus: ');
   s := NumStr(BulkMod, 7, 2);
   RubOut(61, 1, 7, DarkGray);
   Print(61, 1, s);
   Print(47, 2, 'Density     : ');
   s := NumStr(Density, 7, 2);
   RubOut(61, 2, 7, DarkGray);
   Print(61, 2, s);
   Print(47, 3, 'Molecular Wt: ');
   s := NumStr(MolWt, 7, 2);
   RubOut(61, 3, 7, DarkGray);
   Print(61, 3, s);
end;

procedure TCubedAxis(Data : DataType);
var k, pow, nf, upper : integer;
    T, Tmax, Tstp, x, ymax, TicSize : real;
    lbl : string;
begin
   TMax := rounddown(Data.Tzoom, pow, nf);
   if nf=10 then upper := 10 else if nf<21 then upper := nf div 2 else
      if nf<51 then upper := nf div 5 else upper := nf div 10;
   Tstp := TMax/upper;
   ymax := Data.SHzoom;
   TicSize := 0.02*ymax;
   for k := 1 to upper do begin
      T := k*Tstp; x := cube(T);
      PlotLine(x, 0, x, ticSize);
      PlotLine(x, ymax, x, ymax-ticSize);
      if ((upper=10) and (k=5)) or (k>upper-4) then begin
         if pow>0 then lbl := NumStr(T, pow+1, 0)
            else lbl := NumStr(T, pow+1, 1-pow);
         SetTextJustify(CenterText,TopText);
         PrintXY((x*(rhx-lhx)/cube(Data.Tzoom)+lhx), ctry-6/GetMaxY, lbl);
         SetTextJustify(LeftText,TopText);
      end;
   end;
end;

procedure SetUpSpHtAxes(Data : DataType);
var minystp : real;
begin
   minystp := roundup(Data.SHzoom/5);
   if minystp<0.001 then minystp := 0.001;
   SetColor(Whitte);
   if Data.SetData.DisplayTCubed then begin
      SelectScale(4); Axis(0, 0, cube(Data.Tzoom)*20, minystp);
      {choose step that is too large to suppress annotation}
      TCubedAxis(Data); {and then do our own annotation}
   end
   else begin
      SelectScale(1); Axis(0, 0, roundup(Data.Tzoom/5), minystp);
   end;
end;

procedure SetUpSpHtDisplay(Data : DataType);
begin
   DrawSliders(Data);
   if Data.SetData.Superimpose then begin
      SelectViewport(1);
      if Data.SetData.DisplayTCubed then
         SelectScale(4)
      else
         SelectScale(1);
   end
   else begin
      OpenViewport(1);
      SetUpSpHtAxes(Data);
   end;
end;

{************************ DENSITY OF STATES DISPLAY ***********************}

procedure DrawDofStates(Data : DataType; xvector, dosvector : MyDVector;
                        Theta, Rho : real; Structure : StructureType);
var k : integer;
    w, wD, wE : real;
    wEDone : boolean;
begin
   DefineScale(5, 0, 1.5*Data.wrange, 0, 8/Data.NoOfIntvls);
   SelectScale(5);
   SetWriteMode(XORput);
   SetColor(DarkGray);
   PlotDvectors(xvector, dosvector, 1, xvector.Length);
   wD := Theta*kB/hbar;
   wE := (0.85+0.2*sqrt(Rho))*wD; wEDone := false;
   dbvector.init(Data.NoOfIntvls+1);
   for k := 0 to Data.NoOfIntvls do begin
      w := xvector.value(k+1);
      if w<wD then
         dbvector.put(k+1,3*sqr(w)/cube(wD))
      else if (w<wE) or wEDone or (Structure<diamondtype) then
         dbvector.put(k+1,-1/wD) {force point negative to avoid drawing on axis}
      else begin
         dbvector.put(k+1,10/wD); {force point offscale to simulate delta fn.}
         wEDone := true;
      end;
   end;
   DefineScale(6, 0, 1.5*Data.wrange, 0, 8/(1.5*Data.wrange));
   SelectScale(6);
   SetLineStyle(DashedLn, 0, NormWidth);
   PlotDvectors(xvector, dbvector, 1, xvector.Length);
   SetLineStyle(SolidLn, 0, NormWidth);
   dbvector.free;
   SetWriteMode(copyput);
   SelectScale(1);
end;

{************************** SPECIFIC HEAT DISPLAY *************************}

procedure DrawSpHt(var Data : DataType);
begin
   SetColor(LightBlue + ord(Data.Model) - 1);
   if Data.Model = experiment then
      PlotPoints(temperature, SpHt, 'o')
   else if Data.Model <> None then
      PlotDVectors(temperature, SpHt, 1, NoOfPoints);
   if (Data.Model<>RealCrystal) or not Data.SetData.ShowDoS then {do nothing}
      else begin
         DrawDofStates(Data, xvector, dosvector, Data.Theta,
                       Data.SetData.Rho, Data.Structure);
         oldxvector := xvector; olddosvector := dosvector;
         Data.OldTheta := Data.Theta; Data.OldStructure := Data.Structure;
         Data.SetData.OldRho := Data.SetData.Rho;
      end;
end;

procedure DrawOldSpHt(var Data : DataType);
begin
   if (Data.Model<>RealCrystal) or not Data.SetData.ShowDoS then {do nothing}
      else if Data.SetData.Superimpose and not Data.DoSFirstTime then
         DrawDofStates(Data, oldxvector, olddosvector, Data.OldTheta,
                       Data.SetData.OldRho, Data.OldStructure)
      else Data.DoSFirstTime := false;
   if (Data.PreviousModel <> None) and
      (Data.SetData.Superimpose or Data.SetData.ShowPrevious) then begin
         SetColor(Blue + ord(Data.PreviousModel) - 1);
         if Data.PreviousModel = experiment then
            PlotPoints(temperature, SpHt, 'o')
         else if Data.PreviousModel <> None then
            PlotDVectors(temperature, SpHt, 1, NoOfPoints);
   end;
end;

{************************ DEBYE TEMPERATURE DISPLAY ***********************}

procedure SetUpDebyeAxes(Data : DataType);
begin
   SelectScale(2);
   SetColor(Whitte);
   Axis(0, 0, roundup(Data.Tzoom/5), roundup(Data.Theta/4));
end;


procedure SetUpDebyeDisplay(Data : DataType);
begin
   DrawSliders(Data);
   if Data.SetData.Superimpose then begin
      SelectViewport(1); SelectScale(2);
   end
   else begin
      OpenViewport(1);
      SetUpDebyeAxes(Data);
   end;
end;

procedure DrawDebye(var Data : DataType);
var k : integer;
begin
   SelectScale(2);
   SetColor(LightBlue + ord(Data.Model) - 1);
   k := 1;
   while temperature.value(k) < Data.Theta/100 do
      k := k + 1;
   {insufficient accuracy in specific heat for T < THETA/100}
   if Data.Model = experiment then
      PlotPoints(temperature, DBT, 'o')
   else if Data.Model <> None then
      PlotDVectors(temperature, DBT, k, NoOfPoints);
   SetWriteMode(XORput);
   SetColor(Whitte); {gives light gray on dark gray background}
   PlotLine(0, Data.Theta, Data.Tzoom, Data.Theta);
   SetWriteMode(copyput);
   Data.OldTheta := Data.Theta;
   if (Data.Model<>RealCrystal) or not Data.SetData.ShowDoS then {do nothing}
      else begin
         DrawDofStates(Data, xvector, dosvector, Data.Theta,
                       Data.SetData.Rho, Data.Structure);
         oldxvector := xvector; olddosvector := dosvector;
         Data.OldStructure := Data.Structure;
         Data.SetData.OldRho := Data.SetData.Rho;
      end;
end;

procedure DrawOldDebye(var Data : DataType);
var k : integer;
begin
   if (Data.Model<>RealCrystal) or not Data.SetData.ShowDoS then {do nothing}
      else if Data.SetData.Superimpose and not Data.DoSFirstTime then
         DrawDofStates(Data, oldxvector, olddosvector, Data.OldTheta,
                       Data.SetData.OldRho, Data.OldStructure)
                       {rub out old curve}
      else Data.DoSFirstTime := false;
   SelectScale(2);
   if Data.SetData.Superimpose and not Data.FirstTime then begin
      SetWriteMode(XORput);
      SetColor(Whitte);
      PlotLine(0, Data.OldTheta, Data.Tzoom, Data.OldTheta);
      SetWriteMode(copyput);
   end
   else Data.FirstTime := false;
   k := 1;
   while temperature.value(k) < Data.OldTheta/100 do
      k := k + 1;
   {insufficient accuracy in specific heat for T < THETA/100}
   if (Data.PreviousModel <> None) and
      (Data.SetData.Superimpose or Data.SetData.ShowPrevious) then begin
         SetColor(Blue + ord(Data.PreviousModel) - 1);
         if Data.PreviousModel = experiment then
            PlotPoints(temperature, DBT, 'o')
         else if Data.PreviousModel <> None then
         PlotDVectors(temperature, DBT, k, NoOfPoints);
   end;
end;

{************************* SELECT TYPE OF DRAWING *************************}

procedure Display(var Data : DataType);
var k : integer;
    s : string;
begin
   if Data.SetData.Superimpose or
      (Data.SetData.ShowPrevious and (Data.OldModel=Data.PreviousModel))
         then {do nothing}
   else Data.OldModel := None;
   if Data.SetData.DisplaySpHt then begin
      SetupSpHtDisplay(Data); DrawOldSpHt(Data); end
   else begin
      CalcTheta(Data);
      SetUpDebyeDisplay(Data); DrawOldDebye(Data);
   end;
   CalcSpHt(Data);
   if Data.SetData.DisplaySpHt then
      DrawSpHt(Data)
   else begin
      SelectViewPort(1); {waitmessage opens port 10}
      DrawDebye(Data);
   end;
   LabelStructure(Data.Structure);
   LabelSubstance(Data.Substance);
   LabelData(Data);
   ShowColors(Data.Model, true);
   if Data.OldModel <> Data.Model then ShowColors(Data.OldModel, false);
   printratio(Data.SetData.R, 'force', 2);
   SelectViewPort(3);
   if Data.SetData.UseDebye then
      s := NumStr(Data.UserTheta, 7, 1)
      else s := NumStr(Data.THETA, 7, 1);
   RubOut(25, 3, 22, DarkGray);
   SetColor(Whitte);
   Print(25, 3, 'Debye temp:'); Print(38, 3, s);
   Data.PreviousModel := Data.Model;
end;

procedure ReDraw(var Data : DataType; reset : boolean);
begin
   CloseViewport(10);
   CalcTheta(Data);
   Data.Tmax := roundup(1.5*Data.Theta);
   if reset then begin
      Data.Tzoom := Data.Tmax;
      Data.SHzoom := SHmax;
   end;
   Data.PreviousModel := none; Data.OldModel := none;
   DrawSliders(Data);
   SetColor(Whitte);
   OpenViewport(3); OpenViewport(1);
   if Data.SetData.DisplaySpHt then begin
      PutLabel(Left, 'Specific Heat (/3R)');
      if Data.SetData.DisplayTCubed then
         PutLabel(Top, 'LATTICE SPECIFIC HEAT VS. T^3')
      else
         PutLabel(Top, 'TEMPERATURE DEPENDENCE OF THE LATTICE SPECIFIC HEAT');
   end
   else begin
      PutLabel(Left, 'Debye Temperature (K)');
      PutLabel(Top,'LATTICE SPECIFIC HEAT IN TERMS OF THE DEBYE TEMPERATURE');
   end;
   PutLabel(Bottom, 'Temperature, T (K)');
   DefineScale(1, 0, Data.Tzoom, 0, Data.SHzoom);
   DefineScale(4, 0, cube(Data.Tzoom), 0, Data.SHzoom);
   DefineScale(2, 0, Data.Tzoom, 0, 5*Data.Theta/3);
   Data.FirstTime := true; Data.DoSFirstTime := true;
   if Data.SetData.DisplaySpHt then
      SetUpSpHtAxes(Data)
   else begin
      SetUpDebyeAxes(Data);
      Data.OldTheta := Data.Theta;
   end;
end;

{******************************* HANDLE ZOOM ******************************}

procedure Rcursor(a, b : PointType); far;
begin
   lineTP(0, b.y, b.x, b.y);
   lineTP(b.x, b.y, b.x, round((1-ctry)*GetMaxY));
end;

procedure Rbar(a, b : PointType); far;
begin
   lineTP(b.x, 0, b.x, round((1-ctry)*GetMaxY));
end;

procedure zoom(Menu : Tmenu; var Data : DataType);
var x, y, minystp : real;
    inside : boolean;
    SNum, xmax, ymax : integer;
    a, b : PointType;
begin
   SelectViewPort(1);
   if MousePresent then begin
      if Data.SetData.DisplaySpHt then
         message('Click on mouse and drag top right corner of zoom area')
      else
         message('Click on mouse and drag marker');
      a.x := 0; b.x := 0;
      if Data.SetData.DisplaySpHt and Data.SetData.DisplayTCubed then
         SNum := 4
      else SNum := 1;
      repeat
         SetColor(Yellow);
         MouseBorders(round(lhx*GetMaxX), round((1-topy)*GetMaxY),
                      round(rhx*GetMaxX), round((1-ctry)*GetMaxY) );
         if Data.SetData.DisplaySpHt then
            DrawRubberBand(Rcursor, a, b, DarkGray, true)
         else
            DrawRubberBand(Rbar, a, b, DarkGray, true);
         mousePosn(x, y, 1, SNum, inside);
      until inside
   end
   else begin
   if Data.SetData.DisplaySpHt then
      message('Use cursor keys to drag top right corner of box, Return to end')
   else
      message('Use cursor keys to drag marker, Return to end');
      SetColor(Yellow);
      SetWriteMode(XORput);
      xmax := round((rhx-lhx)*GetMaxX); ymax := round((topy-ctry)*GetMaxY);
      b.x := xmax div 2; b.y := ymax div 2;
      if Data.SetData.DisplaySpHt then
         Rcursor(a, b)
      else
         Rbar(a, b);
      repeat
         if Data.SetData.DisplaySpHt then
            Rcursor(a, b)
         else
            Rbar(a, b);
         CheckForEvents;
         if event.ExtendedKey then begin
            if (b.y>0)    and (ord(Event.ReadKey)=72{up   }) then b.y := b.y-3;
            if (b.y<ymax) and (ord(Event.ReadKey)=80{down }) then b.y := b.y+3;
            if (b.x>0)    and (ord(Event.ReadKey)=75{left }) then b.x := b.x-3;
            if (b.x<xmax) and (ord(Event.ReadKey)=77{right}) then b.x := b.x+3;
         end;
         if Data.SetData.DisplaySpHt then
            Rcursor(a, b)
         else
            Rbar(a, b);
      until Event.ReadKey=chr(13);
      SetWriteMode(copyput);
      if Data.SetData.DisplaySpHt and Data.SetData.DisplayTCubed then
         x := b.x*cube(Data.Tzoom)/xmax
      else
         x := b.x*Data.Tzoom/xmax;
      y := (ymax-b.y)*Data.SHzoom/ymax;
   end;
   MouseBorders(0, 0, GetMaxX, GetMaxY);
   SetColor(Whitte);
   if Data.SetData.DisplaySpHt and Data.SetData.DisplayTCubed then
      Data.Tzoom := cuberoot(x)
   else
      Data.Tzoom := x;
   Data.SHzoom := y;
   Menu.Display;
   ReDraw(Data, false);
end;

{*********************** MENU AND HOT KEY HANDLING ***********************}

procedure HandleMenu(var Menu : Tmenu; var Data : DataType;
                     var xrange : RangeType);
var DoRedraw,canceled : boolean;
    OldModel : ModelType;
    k : integer;
begin
   canceled:=false;
   with Menu do
   if colChosen in [1..7] then
    case colChosen of
    1 : begin
           canceled:=true;
           HelpContext := fileitem;
           case rowChosen of
              1 : AboutCups;
              2 : Intro;
              3 : begin
                     ShowHelp('Spheat.HLP','general help 1');
                     ShowHelp('Spheat.HLP','general help 2');
                     ShowHelp('Spheat.HLP','general help 3');
                     ShowHelp('Spheat.HLP','general help 4');
                  end;
              4 : Configuration;
              6 : Data.Quit := true;
           end;
        end;
    2 : begin
           HelpContext := model;
           OldModel := Data.Model;
           Rowactivate(4,3,not (rowChosen=5));
           Rowactivate(4,4,not ((rowChosen=5) or (rowChosen=2)));
           Rowactivate(4,13, (rowChosen=4));
           case rowChosen of
              1 : Data.Model := Einstein;
              2 : Data.Model := Debye;
              3 : Data.Model := Combined;
              4 : Data.Model := RealCrystal;
              5 : Data.Model := Schottky;
              6 : Data.Model := Experiment;
           end;
           if OldModel <> Data.Model then Data.OldModel := OldModel;
           if Data.Model = Experiment then
              Menu.RowActivate(3, 1, false) else
              Menu.RowActivate(3, 1, true);
        end;
    3 : begin
           HelpContext := substance;
           Data.DoSChange := true;
           if Data.Substance=USERDEFINED then
              Data.UDStructure := Data.Structure;
           case rowChosen of
              1 : begin
                     Data.Substance := USERDEFINED;
                     Data.Structure := Data.UDStructure;
                     ReadDataScreen(Data, canceled);
                  end;
              2 : begin
                     Data.Substance := Argon;
                     Data.Structure := FCC;
                     Data.DataPoints := ArgonPoints;
                     for k := 2 to Data.DataPoints do begin
                        Data.SpHtData[k] := ArgonData[k];
                        Data.SpHtTemp[k] := ArgonTemp[k];
                     end;
                  end;
              3 : begin
                     Data.Substance := Aluminium;
                     Data.Structure := FCC;
                     Data.DataPoints := AluminiumPoints;
                     for k := 2 to Data.DataPoints do begin
                        Data.SpHtData[k] := AluminiumData[k];
                        Data.SpHtTemp[k] := AluminiumTemp[k];
                     end;
                  end;
              4 : begin
                     Data.Substance := Silver;
                     Data.Structure := FCC;
                     Data.DataPoints := SilverPoints;
                     for k := 2 to Data.DataPoints do begin
                        Data.SpHtData[k] := SilverData[k];
                        Data.SpHtTemp[k] := SilverTemp[k];
                     end;
                  end;
              5 : begin
                     Data.Substance := Iron;
                     Data.Structure := BCC;
                     Data.DataPoints := IronPoints;
                     for k := 2 to Data.DataPoints do begin
                        Data.SpHtData[k] := IronData[k];
                        Data.SpHtTemp[k] := IronTemp[k];
                     end;
                  end;
              6 : begin
                     Data.Substance := CsCl;
                     Data.Structure := CsClType;
                     Data.DataPoints := CsClPoints;
                     for k := 2 to Data.DataPoints do begin
                        Data.SpHtData[k] := CsClData[k];
                        Data.SpHtTemp[k] := CsClTemp[k];
                     end;
                  end;
              7 : begin
                     Data.Substance := NaCl;
                     Data.Structure := NaClType;
                     Data.DataPoints := NaClPoints;
                     for k := 2 to Data.DataPoints do begin
                        Data.SpHtData[k] := NaClData[k];
                        Data.SpHtTemp[k] := NaClTemp[k];
                     end;
                  end;
              8 : begin
                     Data.Substance := Diamond;
                     Data.Structure := DiamondType;
                     Data.DataPoints := DiamondPoints;
                     for k := 2 to Data.DataPoints do begin
                        Data.SpHtData[k] := DiamondData[k];
                        Data.SpHtTemp[k] := DiamondTemp[k];
                     end;
                  end;
           end;
           if Data.Structure <= DiamondType then Data.VaryForce := true;
           Data.UserTheta := Data.Theta;
           if Data.Substance = USERDEFINED then
              Menu.RowActivate(2, 6, false) else
              Menu.RowActivate(2, 6, true);
        end;
    4 : begin
           HelpContext := plotoptns;
           ReadPlotScreen(Data, Menu, DoRedraw, canceled);
           if DoRedraw then Redraw(Data, true);
           Data.DoSChange := true;
        end;
    5 : begin
           HelpContext := setdata;
           Data.DoSChange := true;
           ReadDataScreen(Data, canceled);
        end;
    6 : begin
           HelpContext := schottkydata;
           ReadSchottkyDataScreen(Data.SchottkyData, canceled);
        end;
    7 : begin
           canceled:=true;
           Sliders.Erase(1, Blakk);
           if NoOfSliders = 2 then Sliders.Erase(2, Blakk);
           if RowChosen = 1 then Data.SliderResolution := Fine
           else if RowChosen = 2 then Data.SliderResolution := Medium
              else Data.SliderResolution := Coarse;
           DrawSliders(Data);
        end;
   end; {case}
   if not canceled and Data.SetData.Autodraw then Display(Data);
end;

procedure HandleHotKeys(key : byte; var Menu : TMenu; var Data : DataType;
                     var xrange : RangeType);
begin
   case key of
    1 : case HelpContext of
        fileitem    : ShowHelp('Spheat.HLP','file help');
        model       : ShowHelp('Spheat.HLP','model help');
        substance   : ShowHelp('Spheat.HLP','substance help');
        plotoptns   : ShowHelp('Spheat.HLP','plotoptns help');
        setdata     : begin
                         ShowHelp('Spheat.HLP','set data help');
                         ShowHelp('Spheat.HLP','structure help');
                      end;
        schottkydata: ShowHelp('Spheat.HLP','schottky data help');
        zoomitem    : ShowHelp('Spheat.HLP','zoom help');
        displayitem : ShowHelp('Spheat.HLP','display help');
        end{case};
    2 : begin
           HelpContext := zoomitem;
           zoom(Menu, Data);
           if Data.SetData.Autodraw then Display(Data);
        end;
    3 : begin
           HelpContext := displayitem;
           Redraw(Data, true);
           if Data.SetData.Autodraw then Display(Data);
        end;
    4 : begin
           HelpContext := displayitem;
           Redraw(Data, false);
        end;
    5 : begin
           HelpContext := displayitem;
           Display(Data);
        end;
    6 : begin
           CheckForEvents;
           if Menu.Chosen then
              HandleMenu(Menu, Data, xrange);
        end;
   end; {case}
end;

procedure HandleSliders(var Data : DataType);
begin
 Data.DoSChange := true;
 with Data.SetData do begin
   R := Sliders.Value(1);
   if (R<0.1) and ((Data.Structure=DiamondType) or (Data.Structure=NaClType))
      then R:=0.1;
      {small R with these lattices gives silly results for G}
   if Data.SetData.UseDebye then
      Data.UserTheta := Sliders.Value(2)
   else if Data.Structure >= DiamondType then begin
      Rho := Sliders.Value(2);
      if Rho < 0.001 then Rho := 0.001;
   end
   else if Data.Structure = DiamondType then
      NCFC := Sliders.Value(2);
   SetColor(Whitte);
   SetTextJustify(LeftText, TopText);
   Data.SlidersFlag := false;
   if Data.SetData.Autodraw then Display(Data);
 end;
end;

{****************************** MAIN PROGRAM *****************************}

var
   key        : byte;
   xrange     : RangeType;
   Menu       : TMenu;
   HotKeys    : THotKeys;
   Data       : DataType;

begin
   CupsInit;
   DelayTime := 100;
   Sliders.Init; ResButtons.init;
   xvector.init(1); dosvector.init(1); {dummy initialization}
   temperature.init(NoOfPoints); temperature.fill(0.0);
   SpHt.init(NoOfPoints); SpHt.fill(0.0);
   DBT.init(NoOfPoints); DBT.fill(0.0);
   oldxvector.init(NoOfPoints); oldxvector.fill(0.0);
   olddosvector.init(NoOfPoints); olddosvector.fill(0.0);
   SetUpMenu(Menu);
   SetUpHotKeys(HotKeys);
   InitializePortsAndData(Menu, Data);
   CalcTheta(Data); Data.UserTheta := Data.Theta;
   Data.Tmax := roundup(1.5*Data.Theta);
   Data.Tzoom := Data.Tmax; Data.SHzoom := SHmax;
   Redraw(Data, true);
   ShowCursor;
   Display(Data);
   intro;
   Data.SlidersFlag := false;
   repeat
      CheckForEvents;
      if HotKeys.Pressed(key) then HandleHotKeys(key, Menu, Data, xrange);
      if Menu.Activated then HandleMenu(Menu, Data, xrange);
      if ResButtons.Changed then begin
         Sliders.Erase(1, Blakk);
         if NoOfSliders = 2 then Sliders.Erase(2, Blakk);
         if ResButtons.number = 0 then Data.SliderResolution := Fine
         else if ResButtons.number = 1 then Data.SliderResolution := Medium
            else Data.SliderResolution := Coarse;
         DrawSliders(Data);
      end;
      if Sliders.Changed then Data.SlidersFlag:=true else
         if Data.SlidersFlag then HandleSliders(Data);
   until Data.Quit;
   xvector.free; dosvector.free;
   CupsDone; Halt;
   Menu.done;
end.
