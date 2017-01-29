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
          {**               Program Phonons.Pas              **}
          {** Phonon dispersion curves and density of states **}
          {**             Written by Graham Keeler           **}
          {**                                                **}
          {**                   Version 1.4                  **}
          {**              Last change: 19/6/94              **}
          {****************************************************}
          {****************************************************}


{********************** GENERAL STRUCTURE OF PROGRAM: *********************}

{*********************** FOR COMPATIBILITY WITH MAC ***********************}
{**************************** INITIALIZATION ******************************}
{************************** SET UP MENU, HOT KEYS *************************}
{******************** GENERAL FUNCTIONS AND PROCEDURES ********************}
{*************** SUBSIDIARY ROUTINES FOR EIGENVALUE HANDLING **************}
{************************ SET UP DYNAMICAL MATRICES ***********************}
{************************ SELECT DYNAMICAL MATRIX *************************}
{*************************** MAIN CALCULATIONS ****************************}
{********************* SET UP AND HANDLE INPUT SCREENS ********************}
{***************************** SET UP SLIDERS *****************************}
{*********************** GENERAL PRINTING PROCEDURES **********************}
{************************* DISPERSION CURVE DISPLAY ***********************}
{************************ DENSITY OF STATES DISPLAY ***********************}
{************************* SELECT TYPE OF DRAWING *************************}
{************************ SET UP RANGES AND SCALING ***********************}
{**************************** SET UP VIEWPORTS ****************************}
{*********************** MENU AND HOT KEY HANDLING ************************}
{**************************** CRYSTAL STRUCTURE ***************************}
{****************************** MAIN PROGRAM ******************************}

{$A-}
{$N+}

PROGRAM Phonons;
USES dos, crt, Graph, cupsmupp, Cups, CupsFunc, CupsProc,
     CupsGrph, Cupsgui, crystals;

CONST
   AvNo = 6.022E23;
   lhx = 0.08; rhx = 0.975; ctrx = 0.055;
   topy = 0.89; btmy = 0.11; midy = 0.5;
   lowy = 0.38; ctry = 0.08; lbly = 0.25;

TYPE
   StructureType = (none, cubic, BCC, FCC, diamond, CsClType, NaClType);
   ResolutionType = (Fine, Medium, Coarse);
   DirnType = (IOO, IIO, III);
   RangeType = array [IOO..III] of real;
   HelpContextType = (fileitem, structure, plotwhat, plothow,
                      setdata, displayitem, crystalitem, crystalstructure);

   SetDataType = RECORD NoofFConsts, NoofModes : integer;
                        FC                     : array [1..6] of real;
                        NCFC, BulkMod, density, Rho, MolWt, FScale : real;
                 end;
   DisplayDataType = RECORD
                        Superimpose, SepCurves,
                        TotalOnly, SingleDisplay : Boolean;
                     end;
   DataType = RECORD
                 SetData : SetDataType;
                 DisplayData : DisplayDataType;
                 Structure : StructureType;
                 DCurves, Changed, Reset, Quit : Boolean;
                 Swapped, ShowStructure, SlidersFlag : boolean;
                 SliderResolution : ResolutionType;
                 wrange, dosmax : real;
                 kmax : integer;
                 xrange : RangeType;
              end;
   ComplexDMatrix = RECORD
                       Re, Im : DMatrix;
                    end;

   MyDVector = OBJECT(DVector)
      procedure AddToElement(Index : integer; {Add in X at Index}
                             X     : real );
   end;

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


   kVectorType = array [IOO..III] of MyDVector;
   freqVector = array [1..12, IOO..III] of MyDVector;
   CompVector = array [1..3] of MyDVector;
   TrigSet = array [1..3] of real;
   freqarray = array [1..6] of real;

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

procedure MyDVector.AddToElement(Index : integer; X : real);
var Y : real;
begin
   Y := Value(Index);
   Put(Index, X+Y);
end;



var {declare objects globally to avoid need to pass as parameters}
    Sliders : TMySliders;
    ResButtons : TButtons;
    WaitMessage : TWaitMessage;
    HelpContext : HelpContextType;
    kvector   : kVectorType;
    wvector   : freqvector;
    xvector, dosvector : MyDVector;
    doscompvector      : compvector;
    {'CONST' arrays}
    NoOfBranches : array [1..6] of integer;
    Color : array [1..11] of integer;

{********************** FOR COMPATIBILITY WITH MAC *********************}

procedure lineTP(x1, y1, x2, y2 : integer);
begin
   line(x1, y1, x2, y2);
end;

procedure NewVector(var V : DVector);
begin
   {MAC only
    New(V);}
end;

procedure NewMyDVector(var V : MyDVector);
begin
   {MAC only
    New(V);}
end;

procedure NewMatrix(var M : DMatrix);
begin
   {MAC only
    New(M);}
end;

procedure DisposeVector(var V : DVector);
begin
   {MAC only
    Dispose(V);}
end;

procedure DisposeMyDVector(var V : MyDVector);
begin
   {MAC only
    Dispose(V);}
end;

procedure DisposeMatrix(var M : DMatrix);
begin
   {MAC only
    Dispose(M);}
end;

{**************************** INITIALIZATION ***************************}

procedure intro;
var IntroScreen : HelpScrType;
begin
   IntroScreen[01] := '                                                 ';
   IntroScreen[02] := ' PHONON DISPERSION CURVES AND DENSITY OF STATES  ';
   IntroScreen[03] := '                                                 ';
   IntroScreen[04] := '                        by                       ';
   IntroScreen[05] := '                                                 ';
   IntroScreen[06] := '                  Graham Keeler                  ';
   IntroScreen[07] := '            University of Salford, UK            ';
   IntroScreen[08] := '                                                 ';
   IntroScreen[09] := '        Version 1.4          June 19, 1994       ';
   IntroScreen[10] := '               Copyright (c) 1994                ';
   IntroScreen[11] := '                                                 ';
   IntroScreen[12] := '                                                 ';
   IntroScreen[13] := ' This program calculates and displays the phonon ';
   IntroScreen[14] := ' dispersion curves and the density of states for ';
   IntroScreen[15] := ' a number of different crystal structures. It    ';
   IntroScreen[16] := ' allows the user to study the effect of changing ';
   IntroScreen[17] := ' the interatomic forces between nearest and      ';
   IntroScreen[18] := ' further neighbour atoms, and for diatomic       ';
   IntroScreen[19] := ' crystal structures the ratio of the atomic      ';
   IntroScreen[20] := ' masses.                                         ';
   IntroScreen[21] := '    It also shows 3-dimensional views of the     ';
   IntroScreen[22] := ' different crystal structures.                   ';
   IntroScreen[23] := '                                                 ';
   IntroScreen[24] := '    Press <Enter> or Click Mouse to continue.    ';
   IntroScreen[25] := '                                                 ';
   help(IntroScreen);
end;

procedure InitializePortsAndData(var Menu : TMenu; var Data : DataType);
var l : integer;
    dirn : DirnType;
begin
   with Data do begin
      kmax := 50; {pseudo-constant: number of points calculated for curves}
      DCurves := true; Changed := true;
      Structure := FCC;
      NewMyDVector(xvector); NewMyDVector(dosvector);
      xvector.init(1); dosvector.init(1);
      {initialize DVectors to minimum length}
      for l := 1 to 3 do begin
         NewMyDVector(doscompvector[l]);
         doscompvector[l].init(1); end;
      for dirn := IOO to III do begin
         NewMyDVector(kvector[dirn]);
         kvector[dirn].init(kmax+1);
         for l := 1 to 12 do begin
            NewMyDVector(wvector[l,dirn]);
            wvector[l,dirn].init(kmax+1);
         end;
      end;
      SliderResolution := Fine; Reset := true; quit := false;
      HelpContext := fileitem;
      with SetData do begin
         BulkMod := 2.7; density := 1688; MolWt := 40;
         NoofFConsts := 3; NoofModes := 300;
         FC[1] := 1.0; FC[2] := 0.5; FC[3] := 0.1; NCFC := 0.6;
         FC[4] := 0.0; FC[5] := 0.0; FC[6] := 0.0;
         {Force constants are initially scaled to 1 for the first constant.}
         Rho := 2;
      end;
      with DisplayData do begin
         Superimpose := false;
         SepCurves := true;
         SingleDisplay := true;
         TotalOnly := false;
      end;
   end;
   Menu.AutoChecking(2,3); {Select FCC initially}
   Menu.AutoChecking(3,1); {Select Dispersion Curves initially}
   Menu.RowChecked(4,3,true);
   Menu.RowChecked(4,5,true);
   Menu.RowChecked(4,9,true);
   Menu.RowActivate(4,8,false);
   Menu.RowActivate(4,9,false);
   DefineViewPort(5, lhx, rhx, btmy + 0.11, topy); {Density of States - total only}
   DefineViewPort(6, lhx, rhx, btmy + 0.11, midy + 0.055); {D of S - components}
   DefineViewPort(7, lhx, rhx, midy + 0.055, topy); {D of S - total}
   DefineViewPort(9, lhx + 0.03, rhx, midy - lbly, midy + 0.05);
      {port for D of S colour labels}
   DefineScale(9, 0, (rhx - lhx - 0.03)*GetMaxX, 0, (lbly + 0.05)*GetMaxY);
   DefineViewPort(10, 0, 1, 0.05, 0.95); {whole screen - for use in clearing}
end;

procedure initializeShowStructure(Data : DataType; var crystal : TCrystal;
                                  var SmallBox : TCrystal; var st : string);
begin
   CloseViewPort(10); Sliders.done;
   DefineViewPort (1, 0.0,  0.65, 0.08, 0.92);
   DefineViewPort (2, 0.72, 1.0,  0.08, 0.43);
   OpenViewPort (1);
   crystal.init;
   crystal.boxdrawn := true;
   crystal.SetBoxRatios(1,1,1);
   crystal.alpha := 30;
   crystal.beta := 20;
   crystal.gamma := 0;
   crystal.size := 0.9;
   crystal.distance := 2.5;
   CASE Data.Structure OF
      none     : ;
      cubic    : Simple_Cubic_Structure(crystal);
      FCC      : FCC_Structure(crystal);
      BCC      : BCC_structure(crystal);
      CsClType : CsCl_Structure(crystal);
      NaClTYpe : NaCl_Structure(crystal);
      diamond  : Diamond_structure(crystal);
   end {case};
   OpenViewPort (2);
   SmallBox.Init;
   SmallBox.SetBoxRatios(1,1,1);
   smallBox.arrows := true;
   smallBox.boxColor := whitte;
   sliders.create(1,  0 ,360,30, 0.68,0.56,0.99,0, '0','360','alpha',false);
   sliders.create(3,  0 ,360, 0, 0.68,0.49,0.99,0, '0','360','gamma',false);
   sliders.create(2,-180,180,20, 0.68,0.08,0.43,0, '', '',   'beta' ,true );
   sliders.drawAll;
   case Data.Structure of
      none     : st := '';
      cubic    : st := 'SIMPLE CUBIC STRUCTURE';
      BCC      : st := 'BCC STRUCTURE';
      FCC      : st := 'FCC STRUCTURE';
      CsClType : st := 'CsCl STRUCTURE';
      NaClType : st := 'NaCl STRUCTURE';
      diamond  : st := 'DIAMOND STRUCTURE';
   end {case};
end;


procedure SetUpConstants;
begin
   {assign 'CONST' arrays}
   NoOfBranches[1] := 3;
   NoOfBranches[2] := 3;
   NoOfBranches[3] := 3;
   NoOfBranches[4] := 6;
   NoOfBranches[5] := 6;
   NoOfBranches[6] := 6;
   Color[1] := Blue;
   Color[2] := LightGreen;
   Color[3] := Yellow;
   Color[4] := LightMagenta;
   Color[5] := LightCyan;
   Color[6] := LightRed;
   Color[7] := LightGray;
   Color[8] := LightBlue;
   Color[9] := LightGreen;
   Color[10] := Yellow;
   Color[11] := Whitte;
end;

{************************** SET UP MENU, HOT KEYS *************************}

procedure SetUpMenu(var Menu : TMenu);
var j : integer;
begin
   Menu.Init;
   with Menu do
   begin
      column(1, 'File');
       row(1,1, 'About CUPS');
       row(1,2, 'About Program');
       row(1,3, 'Program Details');
       row(1,4, 'About Theory');
       row(1,5, 'Configuration');
       row(1,6, '---------------');
       rowactivate(1,6,false);
       row(1,7, 'Exit Program');
      column(2, 'Structure');
       row(2,1, '-- MONATOMIC LATTICES --');
       row(2,2, 'Simple cubic ');
       row(2,3, 'F.C.C.       ');
       row(2,4, 'B.C.C.       ');
       row(2,5, '-- DIATOMIC LATTICES --- ');
       row(2,6, 'CsCl type    ');
       row(2,7, 'NaCl type    ');
       row(2,8, 'diamond      ');
       row(2,9, '-- DISPLAY ONLY --------');
       row(2,10,'Tetrahedral');
       row(2,11,'Hexagonal');
       row(2,12,'H.C.P.');
       row(2,13,'Cu3Au');
       row(2,14,'CdI2');
       row(2,15,'NiAs');
       row(2,16,'ZnS');
       row(2,17,'CaF2');
       row(2,18,'FCC Brillouin Zone');
       row(2,19,'BCC Brillouin Zone');
       row(2,20,'User defined structure');
      column(3, 'Plot-what');
       row(3,1, 'Dispersion Curves');
       row(3,2, 'Density of States');
       row(3,3, 'Combined display');
       row(3,4, 'Crystal Structures');
      column(4, 'Plot-how');
       row(4,1, '-- GENERAL DISPLAY ----');
       row(4,2, 'Superimpose curves');
       row(4,3, 'Clear old curves');
       row(4,4, '-- DISPERSION CURVES --');
       row(4,5, 'Separate curves');
       row(4,6, 'Connected curves');
       row(4,7, '-- DENSITY OF STATES --');
       row(4,8, 'Total curve only');
       row(4,9, 'Total + components');
      column(5, 'Set Data');
      Display;
      RowActivate(2,1,false);
      RowActivate(2,5,false);
      RowActivate(4,1,false);
      RowActivate(4,4,false);
      RowActivate(4,7,false);
      for j := 9 to 20 do RowActivate(2,j,false);

   end;
end;

procedure SetUpHotKeys(var HotKeys : THotKeys);
begin
   with HotKeys do begin
      init(6);
      key[1] := 'F1-Help';
      key[2] := 'F5-Rescale';
      key[3] := 'F6-Clear Old';
      key[4] := 'F7-Fine Sldr';
      key[5] := 'F8-Coarse Sl';
      key[6] := 'F10-Menu';
   end;
   HotKeys.Display;
end;

PROCEDURE SetUpSTHotKeys(var HotKeys : THotKeys);
BEGIN
   WITH HotKeys DO BEGIN
      init(5);
      key[1] := 'F1-Help';
      key[2] := 'F6-Box on/off';
      key[3] := 'F7-Size';
      key[4] := 'F8-Distance';
      key[5] := 'F10-Menu';
   END;
   HotKeys.Display;
END;

{******************** GENERAL FUNCTIONS AND PROCEDURES *******************}

function ClickedInOnePort(vp : integer) : boolean;
begin
   with event do with views[vp] do
   ClickedInOnePort := MouseClicked and (x>vx1) and (x<vx2)
                    and (y>vy1) and (y<vy2);
end;

function ClickedInside(Data : DataType) : boolean;
begin
   ClickedInside := false;
   if not Data.DisplayData.SingleDisplay then begin
      if ClickedInOnePort(1) or ClickedInOnePort(2) or ClickedInOnePort(3)
         or ClickedInOnePort(8) then ClickedInside := true; end
   else if Data.DCurves then begin
      if ClickedInOnePort(1) or ClickedInOnePort(2) or ClickedInOnePort(3)
         then ClickedInside := true; end
   else if ClickedInOnePort(5) then ClickedInside := true;
end;

function cuberoot(num : real) : real;
begin
   cuberoot := Pwr(num, 1/3);
end;

{*************** SUBSIDIARY ROUTINES FOR EIGENVALUE HANDLING **************}

PROCEDURE eigsrt(VAR d: DVector;
  	              n: integer);
{ This routine sorts the eigenvalues into ascending order.}
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
   NewVector(e); NewMatrix(evecs);
   e.init(N); evecs.init(N,N);
   Sym2Trid(N, a, ev, e, false);
   Tridiag(N, ev, e, false, evecs);
   e.free; evecs.free;
   DisposeVector(e); DisposeMatrix(evecs);
   eigsrt(ev, n);
END;

PROCEDURE hermeigensolve(VAR a: complexDMatrix; n: integer; VAR ev: DVector);
VAR b: DMatrix;
    i, j, n2 : integer;
    ev2 : DVector;
BEGIN
   NewMatrix(b); NewVector(ev2);
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
   DisposeMatrix(b); DisposeVector(ev2);
END;

{************************ SET UP DYNAMICAL MATRICES ***********************}

procedure SetupTrig(var c, c2, c3, c4, s, s2, s3 : TrigSet; qx, qy, qz : real);
begin
   c[1] := cos(qx); c[2] := cos(qy); c[3] := cos(qz);
   c2[1] := cos(2*qx); c2[2] := cos(2*qy); c2[3] := cos(2*qz);
   c3[1] := cos(3*qx); c3[2] := cos(3*qy); c3[3] := cos(3*qz);
   c4[1] := cos(4*qx); c4[2] := cos(4*qy); c4[3] := cos(4*qz);
   s[1] := sin(qx); s[2] := sin(qy); s[3] := sin(qz);
   s2[1] := sin(2*qx); s2[2] := sin(2*qy); s2[3] := sin(2*qz);
   s3[1] := sin(3*qx); s3[2] := sin(3*qy); s3[3] := sin(3*qz);
end;

procedure SetUpCubic(qx, qy, qz : real; SetData : SetDataType;
                     var D : DMatrix);
var i, j, k : integer;
    r1, r2, r3, r4, r5, r6, diag : real;
    c, c2, c3, c4, s, s2, s3 : TrigSet;
begin
   r1 := SetData.FC[1]; r2 := SetData.FC[2];
   r3 := SetData.FC[3]; r4 := SetData.FC[4];
   r5 := SetData.FC[5]; r6 := SetData.FC[6];
   SetupTrig(c, c2, c3, c4, s, s2, s3, qx, qy, qz);
   diag := 2*r1 + 4*r2 + 8*r3/3 + 2*r4 + 8*r5 + 8*r6
           - (8*r3/3)*c[1]*c[2]*c[3];
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,i,  diag - 2*r1*c[i]- 2*r2*(c[i]*c[j] + c[i]*c[k]) - 2*r4*c2[i]
                       - (4*r5/5)*(4*c2[i]*c[j] + 4*c2[i]*c[k] + c[i]*c2[j]
                                   + c[i]*c2[k])
                       - (4*r6/3)*(4*c2[i]*c[j]*c[k] + c[i]*c2[j]*c[k]
                                   + c[i]*c[j]*c2[k])                       );
   end;
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,j,  2*r2*s[i]*s[j] + (8*r3/3)*s[i]*s[j]*c[k]
                  + (8*r5/5)*(s2[i]*s[j] + s[i]*s2[j])
                  + (4*r6/3)*(2*s2[i]*s[j]*c[k] + 2*s[i]*s2[j]*c[k]
                              + s[i]*s[j]*c2[k])                    );
      D.put(j,i,  D.value(i,j)  );
   end;
end;

procedure SetUpFCC(qx, qy, qz : real; SetData : SetDataType;
                   var D : DMatrix);
var i, j, k : integer;
    r1, r2, r3, r4, r5, r6, diag : real;
    c, c2, c3, c4, s, s2, s3 : TrigSet;
begin
   r1 := SetData.FC[1]; r2 := SetData.FC[2];
   r3 := SetData.FC[3]; r4 := SetData.FC[4];
   r5 := SetData.FC[5]; r6 := SetData.FC[6];
   SetupTrig(c, c2, c3, c4, s, s2, s3, qx, qy, qz);
   diag := 4*r1 + 2*r2 + 8*r3 + 4*r4 + 16*r5 + 8*r6/3
           - (8*r6/3)*c2[1]*c2[2]*c2[3];
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,i,  diag - 2*r1*(c[i]*c[j] + c[i]*c[k])
                       - 2*r2*c2[i]
                       - (4*r3/3)*(4*c2[i]*c[j]*c[k] + c[i]*c2[j]*c[k]
                                   + c[i]*c[j]*c2[k])
                       - 2*r4*(c2[i]*c2[j] + c2[i]*c2[k])
                       - (4*r5/5)*(9*c3[i]*c[j] + 9*c3[i]*c[k]
                                   + c[i]*c3[j] + c[i]*c3[k])          );
   end;
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,j,  2*r1*s[i]*s[j]
                  + (4*r3/3)*(2*s2[i]*s[j]*c[k]+2*s[i]*s2[j]*c[k]
                     +s[i]*s[j]*c2[k])
                  + (12*r5/5)*(s3[i]*s[j] + s[i]*s3[j])
                  + (8*r6/3)*s2[i]*s2[j]*c2[k]                    );
      D.put(j,i,  D.value(i,j)  );
   end;
end;

procedure SetUpBCC(qx, qy, qz : real; SetData : SetDataType;
                   var D : DMatrix);
var i, j, k : integer;
    r1, r2, r3, r4, r5, r6, diag : real;
    c, c2, c3, c4, s, s2, s3 : TrigSet;
begin
   r1 := SetData.FC[1]; r2 := SetData.FC[2];
   r3 := SetData.FC[3]; r4 := SetData.FC[4];
   r5 := SetData.FC[5]; r6 := SetData.FC[6];
   SetupTrig(c, c2, c3, c4, s, s2, s3, qx, qy, qz);
   diag := 8*r1/3 + 2*r2 + 4*r3 + 8*r4 + 8*r5/3 +2*r6
            - (8*r1/3)*c[1]*c[2]*c[3]
            - (8*r5/3)*c2[1]*c2[2]*c2[3];
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,i,  diag - 2*r2*c2[i]
                       - 2*r3*(c2[i]*c2[j] + c2[i]*c2[k])
                       - (8*r4/11)*(9*c3[i]*c[j]*c[k] + c[i]*c3[j]*c[k]
                                    + c[i]*c[j]*c3[k])
                       - 2*r6*c4[i]                                     );
   end;
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,j,  (8*r1/3)*s[i]*s[j]*c[k]
                  + 2*r3*s2[i]*s2[j]
                  + (8*r4/11)*(3*s3[i]*s[j]*c[k]+3*s[i]*s3[j]*c[k]
                               +s[i]*s[j]*c3[k])
                  + (8*r5/3)*s2[i]*s2[j]*c2[k]                     );
      D.put(j,i,  D.value(i,j)  );
   end;
end;

procedure SetUpCsCl(qx, qy, qz : real; SetData : SetDataType;
                    var D : DMatrix);
var i, j, k : integer;
    r1, r2, r3, r4, r5, r6, rootrho, diag1, diag2 : real;
    c, c2, c3, c4, s, s2, s3 : TrigSet;
begin
   r1 := SetData.FC[1]; r2 := SetData.FC[2];
   r3 := SetData.FC[3]; r4 := SetData.FC[4];
   r5 := SetData.FC[5]; r6 := SetData.FC[6];
   {r1 and r4 relate to unlike atoms, r2, r3, r5 and r6 to like atoms}
   qx := qx/2; qy := qy/2; qz := qz/2;
   {Brillouin Zone is reduced in size for diatomic structures}
   SetupTrig(c, c2, c3, c4, s, s2, s3, qx, qy, qz);
   rootrho:=sqrt(SetData.Rho);
   diag1 := 8*r1/3 + 2*r2 + 4*r3 + 8*r4 + 8*r5/3 +2*r6
            - (8*r5/3)*c2[1]*c2[2]*c2[3];
   diag2 := - (8*r1/3)*c[1]*c[2]*c[3];
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,i,      rootrho*(diag1 - 2*r2*c2[i]
                               - 2*r3*(c2[i]*c2[j] + c2[i]*c2[k])
                               - 2*r6*c4[i]) );
      D.put(i+3,i+3,  D.value(i,i)/SetData.Rho  );
      D.put(i,i+3,    diag2 - (8*r4/11)*(9*c3[i]*c[j]*c[k] + c[i]*c3[j]*c[k]
                                         + c[i]*c[j]*c3[k])                 );
      D.put(i+3,i,    D.value(i,i+3)  );
   end;
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,j,     rootrho*(2*r3*s2[i]*s2[j] + (8*r5/3)*s2[i]*s2[j]*c2[k]) );
      D.put(i+3,j,   (8*r1/3)*s[i]*s[j]*c[k]
                     + (8*r4/11)*(3*s3[i]*s[j]*c[k]+3*s[i]*s3[j]*c[k]
                                  +s[i]*s[j]*c3[k])                   );
      D.put(j,i,     D.value(i,j)  );
      D.put(j,i+3,   D.value(i+3,j)  ); D.put(i,j+3,  D.value(i+3,j)  );
      D.put(j+3,i,   D.value(i+3,j)  );
      D.put(i+3,j+3, D.value(i,j)/SetData.Rho  );
      D.put(j+3,i+3, D.value(i+3,j+3)  );
   end;
end;

procedure SetUpNaCl(qx, qy, qz : real; SetData : SetDataType;
                    var D : DMatrix);
var i, j, k : integer;
    r1, r2, r3, r4, r5, r6, rootrho, diag1, diag2 : real;
    c, c2, c3, c4, s, s2, s3 : TrigSet;
begin
   r1 := SetData.FC[1]; r2 := SetData.FC[2];
   r3 := SetData.FC[3]; r4 := SetData.FC[4];
   r5 := SetData.FC[5]; r6 := SetData.FC[6];
   {r1, r3 and r5 relate to unlike atoms, r2, r4 and r6 to like atoms}
   SetupTrig(c, c2, c3, c4, s, s2, s3, qx, qy, qz);
   rootrho:=sqrt(SetData.Rho);
   diag1 :=  2*r1 + 4*r2 + 8*r3/3 + 2*r4 + 8*r5 + 8*r6;
   diag2 :=  - (8*r3/3)*c[1]*c[2]*c[3];
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,i,     rootrho*(diag1 - 2*r2*(c[i]*c[j]+c[i]*c[k])-2*r4*c2[i]
                              - (4*r6/3)*(4*c2[i]*c[j]*c[k]+c[i]*c2[j]*c[k]
                              + c[i]*c[j]*c2[k]))                           );
      D.put(i+3,i+3, D.value(i,i)/SetData.Rho  );
      D.put(i,i+3,   diag2 - 2*r1*c[i]
                     - (4*r5/5)*(4*c2[i]*c[j] + 4*c2[i]*c[k]
                                + c[i]*c2[j] + c[i]*c2[k]) );
      D.put(i+3,i,   D.value(i,i+3)  );
   end;
   for i := 1 to 3 do begin
      j := i mod 3 + 1; k := j mod 3 + 1;
      D.put(i,j,     rootrho*(2*r2*s[i]*s[j]
                              + (4*r6/3)*(2*s2[i]*s[j]*c[k]+2*s[i]*s2[j]*c[k]
                                          +s[i]*s[j]*c2[k]))                );
      D.put(j,i,     D.value(i,j)  );
      D.put(i+3,j,   (8*r3/3)*s[i]*s[j]*c[k]
                      + (8*r5/5)*(s2[i]*s[j] + s[i]*s2[j])  );
      D.put(j,i+3,   D.value(i+3,j)  ); D.put(i,j+3,  D.value(i+3,j)  );
      D.put(j+3,i,   D.value(i+3,j)  );
      D.put(i+3,j+3, D.value(i,j)/SetData.Rho  );
      D.put(j+3,i+3, D.value(i+3,j+3)  );
   end;
end;

procedure SetUpDiamond(qx, qy, qz : real; SetData : SetDataType;
                   var D : ComplexDMatrix);
var i, j, k : integer;
    r1, r2, r3 : real;
    c, c2, c3, c4, s, s2, s3 : TrigSet;
begin
   r1 := SetData.FC[1]; r2 := SetData.FC[2];
   r3 := SetData.NCFC; {r3 is the non-central component of the n-n force}
   qx := qx/2; qy := qy/2; qz := qz/2;
   {Brillouin Zone is reduced in size for diatomic structures}
   SetupTrig(c, c2, c3, c4, s, s2, s3, qx, qy, qz);
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
                       SetData : SetDataType; var w : freqarray;
                       var fail : boolean);
var i, N : integer;
    w2 : real;
    D : DMatrix;
    CD : complexDMatrix;
    ev : DVector;
begin
   N:=NoOfBranches[ord(Structure)];
   NewVector(ev); ev.init(N);
   if Structure <> diamond then begin
      NewMatrix(D);
      D.init(N,N); D.fill(0.0); end
   else begin
      NewMatrix(CD.Re); NewMatrix(CD.Im);
      CD.Re.init(N,N); CD.Im.init(N,N);
      CD.Re.fill(0.0); CD.Im.fill(0.0); end;
   case Structure of
      cubic : SetUpCubic(qx, qy, qz, SetData, D);
      FCC :   SetUpFCC(qx, qy, qz, SetData, D);
      BCC :   SetUpBCC(qx, qy, qz, SetData, D);
      CsClType :  SetUpCsCl(qx, qy, qz, SetData, D);
      NaClType :  SetUpNaCl(qx, qy, qz, SetData, D);
      diamond  :  SetUpDiamond(qx, qy, qz, SetData, CD);
      none :  ;
   end;
   fail:=false;
   if Structure <> diamond then
      eigensolve(D, N, ev)
   else
      hermeigensolve(CD, N, ev);
   for i := 1 TO N do begin
      w2 := ev.value(i);
      if (ABS(w2) < 1.0E-6) then w2 := 0;
      {rounding errors may produce very small -ve w's when w should be zero}
      if (w2 < 0) then begin
         fail := true; w2 := 0;
      end;
      w[i] := SQRT(SetData.FScale*w2)/(2*pi*1E12);
      {scale to THz}
   end;
   ev.free;
   DisposeVector(ev);
   if Structure <> diamond then begin
      D.free;
      DisposeMatrix(D); end
   else begin
      CD.Re.free; CD.Im.free;
      DisposeMatrix(CD.Re); DisposeMatrix(CD.Im); end;
end;

{*************************** MAIN CALCULATIONS **************************}

procedure CalcFScale(var Data : DataType);
var a : real;
begin
 with Data.SetData do
 case Data.Structure of
   none, cubic:
         begin
            a := cuberoot(MolWt/(1000*AvNo*density));
            FScale := 3*((BulkMod*1E9)/(density*a*a))/
                  (FC[1] + 4*FC[2] + 4*FC[3] + 4*FC[4] + 20*FC[5] + 24*FC[6]);
            {units of Bulk Modulus are GN m-2}
            {Fscale is the quantity gamma/at. mass}
         end;
   FCC : begin
            a := cuberoot(4*MolWt/(1000*AvNo*density))/2;
            {The unit cube is of side 2a, but contains 4 atoms (molecules)}
            FScale := (3/4)*(BulkMod*1E9/(density*a*a))/
                     (FC[1] + FC[2] + 6*FC[3] + 4*FC[4] + 20*FC[5] + 4*FC[6]);
         end;
   BCC : begin
            a := cuberoot(2*MolWt/(1000*AvNo*density))/2;
            {This time the unit cube is of side 2a, and contains 2 atoms}
            FScale := (3/4)*(BulkMod*1E9/(density*a*a))/
                     (FC[1] + FC[2] + 4*FC[3] + 11*FC[4] + 4*FC[5] + 4*FC[6]);
         end;
   NaClType:
         begin
            a := cuberoot(4*MolWt/(1000*AvNo*density))/2; {basically simple cubic}
            {The Dynamical matrix is calculated over a cell of}
            {size 2a, containing 4 molecules (8 atoms)}
            FScale := 3*((BulkMod*1E9)/(density*a*a))/
                  (FC[1] + 4*FC[2] + 4*FC[3] + 4*FC[4] + 20*FC[5] + 24*FC[6]);
            {units of Bulk Modulus are GN m-2}
            {Fscale is the quantity gamma/at. mass}
            Fscale := Fscale*sqrt(2+Data.SetData.Rho+1/Data.SetData.Rho)/2;
            {For NaCl, the bulk modulus relates to gamma/mean mass,}
            {ie 2*gamma/(m1+m2). What is needed is gamma/sqrt(m1m2)}
         end;
   CsClType :
         begin
            a := cuberoot(MolWt/(1000*AvNo*density))/2;
            {The unit cube is again of side 2a, and contains 1 molecule}
            FScale := (3/4)*(BulkMod*1E9/(density*a*a))/
                     (FC[1] + FC[2] + 4*FC[3] + 11*FC[4] + 4*FC[5] + 4*FC[6]);
            Fscale := Fscale*sqrt(2+Data.SetData.Rho+1/Data.SetData.Rho)/2;
            {For CsCl, the bulk modulus relates to gamma/mean mass,}
            {ie 2*gamma/(m1+m2). What is needed is gamma/sqrt(m1m2)}
         end;
   diamond :
         begin
            a := cuberoot(8*MolWt/(1000*AvNo*density))/2;
            {The unit cube is of side 2a, and contains 8 atoms}
            FScale := 3*(BulkMod*1E9/(density*a*a))/
                     (3*FC[1]/2 + 4*FC[2] - 2*NCFC);
         end;
 end {case};
end;

procedure CalcDispnCurves(Data : DataType);
var qx, qy, qz, swop : real;
    dirn : DirnType;
    j, l, k, ifail : integer;
    w : freqarray;
    fail : boolean;
    s : string;
    slope, oldw, oldslope : array[1..6] of real;
begin
 WaitMessage.Show;
 for dirn := IOO to III do begin
   ifail := 0;
   for k := 0 TO Data.kmax do begin
      WaitMessage.Update;
      qx := k*pi*Data.xrange[dirn]/Data.kmax;
      if dirn > IOO then qy := qx else qy := 0.0;
      if dirn > IIO then qz := qx else qz := 0.0;
      SolveDMatrix(Data.Structure, qx, qy, qz, Data.SetData, w, fail);
      if fail then ifail := k;
      if ((not Data.DisplayData.SepCurves) or
          not Data.DisplayData.SingleDisplay) and
         (((Data.Structure <> BCC) and (dirn = IIO)) or
          ((Data.Structure = BCC) and (dirn = III))) then
         kvector[dirn].put(k+1, -k)
      else
         kvector[dirn].put(k+1, k);
      for l := 1 to NoOfBranches[ord(Data.Structure)] do begin
         if k=0 then begin
            oldw[l] := w[l]; oldslope[l] := w[l];
         end
         else begin
            slope[l] := w[l]-oldw[l];
            for j := l+1 to NoOfBranches[ord(Data.Structure)] do
               if abs(slope[l]-oldslope[l])>abs(w[j]-oldw[l]-oldslope[l])
                  then begin
                     swop := w[l]; w[l] := w[j]; w[j] := swop;
                     slope[l] := w[l]-oldw[l];
                  end; {avoid changing curve assignments when curves cross}
                       {by tracking slopes and avoiding abrupt changes}
            oldw[l] := w[l]; oldslope[l] := slope[l];
         end;
         wvector[l,dirn].put(k+1, w[l]);
      end;
      if ((dirn=IOO) or (dirn=III)) then begin
         if (abs(w[4]-w[5])>w[4]*0.00001) and
            (abs(w[5]-w[6])<w[5]*0.00001) then begin
               {sometimes the degenerate optic branches are not
                the lower frequency pair, so swop}
               swop := w[4]; w[4] := w[6]; w[6] := swop;
               wvector[4,dirn].put(k+1, w[4]);
               wvector[5,dirn].put(k+1, w[5]);
               wvector[6,dirn].put(k+1, w[6]);
         end;
      end;
   end; {for k}
   if ifail>0 then begin
      s := NumStr(ifail, 3, 0);
      s := Concat('dmatrix failure at k value ', s);
      announce(s);
   end;
 end; {for dirn}
 WaitMessage.Hide;
end;

procedure CalcDofStates(Data : DataType; var dosmax : real);
var l, l3, k, kx, ky, kz, kxmax, kymax, kzmax : integer;
    BZsize, qx, qy, qz, intvl, SumOfTerms, wt, wfrac : real;
    wint, ifail, NoOfIntvls : integer;
    w : freqarray;
    fail : boolean;
    s : string;
begin
   WaitMessage.Show;
   ifail := 0;
   case Data.Structure of
      none : ;
      cubic, CsClType : BZsize := 6;
      FCC, NaClType, diamond : BZsize := 12;
      BCC : BZsize := 24;
   end {case};
     {for a simple cubic BZ, the irreducible portion is 1/6 of an octant.}
     {For FCC types, the BZ is further reduced by 1/2, and for BCC 1/4.}
   kxmax := round(cuberoot(BZsize*Data.SetData.NoofModes));
   NoOfIntvls := 7*round(cuberoot(Data.SetData.NoofModes));
      {This is a compromise factor for the number of intervals into which}
      {to divide the calculated points, to give reasonable resolution}
      {(for the chosen number of modes) without excessive noise}
   intvl := 1.4*Data.wrange/NoOfIntvls;
   xvector.ReSize(NoOfIntvls+1); dosvector.ReSize(NoOfIntvls+1);
   for k := 0 to NoOfIntvls do xvector.put(k+1, k*intvl);
   dosvector.fill(0.0);
   for l := 1 to 3 do begin
      doscompvector[l].ReSize(NoOfIntvls+1);
      doscompvector[l].fill(0.0);
   end;
   SumOfTerms := 0;
   for kx := 1 to kxmax do begin
      qx := kx*pi/kxmax;
      case Data.Structure of
         cubic, CsClType           : kymax := kx;
         BCC, none                 : if kx <= kxmax/2 then kymax := kx
                                        else kymax :=  kxmax - kx;
         FCC, NaClType, diamond    : if kx <= 3*kxmax/4 then kymax := kx
                                        else kymax :=  trunc(3*kxmax/2) - kx;
      end {case};
      for ky := 1 to kymax do begin
         qy := ky*pi/kxmax;
         case Data.Structure of
            none, cubic, BCC, CsClType  : kzmax := ky;
            FCC, NaClType, diamond      :
                                if ky <= kxmax/2 then kzmax := ky
                                   else kzmax := trunc(3*kxmax/2) - kx - ky;
         end {case};
         for kz := 1 to kzmax do begin
            qz := (kz - 0.5)*pi/kxmax;
            SolveDMatrix(Data.Structure, qx, qy, qz, Data.SetData, w, fail);
            WaitMessage.Update;
            if fail then ifail := kz;
            if (ky = kymax) or (kx = kxmax) then wt := 0.5 else wt := 1;
            for l := 1 to NoOfBranches[ord(Data.Structure)] do begin
               SumOfTerms := SumOfTerms + wt;
               l3 := (l-1) mod 3 + 1;
               w[l] := w[l]/intvl;
               wint := trunc(w[l]);
               wfrac := w[l] - wint;
               if wint<NoOfIntvls then begin
                  doscompvector[l3].AddToElement(wint+1, (1 - wfrac)*wt);
                  doscompvector[l3].AddToElement((wint+1)+1, wfrac*wt);
                     {+1 is because dvectors must start from 1,}
                     {whereas wint starts from 0}
               end;
           end {l};
         end {kz};
      end {ky};
   end {kx};
   if ifail>0 then begin
      s := NumStr(ifail, 3, 0);
      s := Concat('dmatrix failure at k value ', s);
      announce(s);
   end;
   dosmax := 0;
   for k := 0 to NoOfIntvls do begin
      WaitMessage.Update;
      for l := 1 to 3 do begin
         doscompvector[l].put(k+1, doscompvector[l].Value(k+1)/
                                            (SumOfTerms*intvl));
         {scale down so area under curve = 1}
         if Data.DisplayData.SingleDisplay then
            dosvector.AddToElement(k+1, doscompvector[l].Value(k+1))
         else
            dosvector.AddToElement(k+1, -doscompvector[l].Value(k+1))
      end {l};
      if dosmax < abs(dosvector.Value(k+1)) then
          dosmax := abs(dosvector.Value(k+1));
   end {k};
   WaitMessage.Hide;
end;

{********************* SET UP AND HANDLE INPUT SCREENS ********************}

procedure ReadDataScreen(var Data : DataType; var EscPressed : boolean);
var
   DataScreen : TInputScreen;
   j, extras : integer;
begin
  {MAC only
   New(DataScreen);}
   with DataScreen do begin
      init;
      DefineInputPort (0.18, 0.82, 0.04, 0.96);
      LoadLine(' SET DATA');
      LoadLine('');
      LoadLine(' Bulk Modulus              = {      }GN m^-2');
      LoadLine(' Density                   = {      }kg m^-3');
      LoadLine(' Molecular weight          = {      }');
      LoadLine('');
      if Data.Structure <> diamond then
         LoadLine(' No. of force constants    = {  }(1-6)');
      LoadLine('');
      LoadLine(' Value of force consts (relative to first)');
      LoadLine('                       2nd = {      }');
      if Data.Structure = diamond then begin
         LoadLine('');
         LoadLine(' Non-central force         = {     }(-0.5 to 0.5)');
      end
      else begin
         LoadLine('                       3rd = {      }');
         LoadLine('                       4th = {      }');
         LoadLine('                       5th = {      }');
         LoadLine('                       6th = {      }');
         if Data.Structure > diamond then begin
            LoadLine('');
            LoadLine(' Ratio of masses           = {      }(>1) ');
         end;
      end;
      LoadLine('');
      LoadLine(' No.of modes in D.o.States = {      }(50-10^4)');
      LoadLine('');
      LoadLine('    [  Ok  ]      [ Help ]      [Cancel]');
      with Data.SetData do begin
         SetNumber(1,BulkMod);  {use old values as defaults}
         SetNumberLimits(1,1E-2,1E6);
         SetNumber(2,density);
         SetNumberLimits(2,1,1E6);
         SetNumber(3,MolWt);
         SetNumberLimits(3,1,1000);
         if Data.Structure = diamond then begin
            SetNumber(4,FC[2]);
            SetNumberLimits(4,-10,1E7);
            SetNumber(5,NCFC);
            SetNumberLimits(5,-0.5,+0.5);
            extras := 0;
         end
         else begin
            SetNumber(4,NoofFConsts);
            SetNumberLimits(4,1,6);
            for j := 2 to 6 do begin
               SetNumber(j+3,FC[j]);
               SetNumberLimits(j+3,-10,1E7);
            end;
            extras := 4;
            if Data.Structure > diamond then begin
               SetNumber(10,Rho);
               SetNumberLimits(10,1,1E7);
               extras := 5;
            end;
         end;
         SetNumber(6+extras,NoofModes);
         SetNumberLimits(6+extras,50,10000);
      end;
      SetHelpFile('Phonons.hlp','set data help');
      AcceptScreen;
      if Canceled then
         EscPressed := true
      else with Data.SetData do begin
         EscPressed := false;
         BulkMod := GetNumber(1);
         density := GetNumber(2);
         MolWt := GetNumber(3);
         if Data.Structure = diamond then begin
            FC[2] := GetNumber(4);
            NCFC := GetNumber(5); NoofFConsts := 2 end
         else begin
            NoofFConsts := trunc(GetNumber(4));
            for j := 2 to NoofFConsts do
               FC[j] := GetNumber(j+3);
            for j := NoofFConsts+1 to 6 do FC[j] := 0.0;
            if Data.Structure > diamond then
               Rho := GetNumber(10);
         end;
         NoofModes := trunc(GetNumber(6+extras));
      end;
      done;
   end;
   {MAC only
    Dispose(DataScreen);}

end;

{***************************** SET UP SLIDERS ****************************}

procedure DrawDCurveSliders(var Data : DataType);
var FInit, MInit, x0, y0, x1, y1, x2, y2 : real;
    Fmax, MMax, FMin, MMin, decimals : integer;
    Ftop, Mtop : string;
begin
   Fmin := 0; MMin := 1;
   if Data.SliderResolution = Fine then begin
      FMax := 2; MMax := 10; Ftop := '2'; Mtop := '10'; decimals := 2; end
   else if Data.SliderResolution = Medium then begin
      FMax := 20; MMax := 100; Ftop := '20'; Mtop := '100'; decimals := 1;
   end
   else begin
      FMax := 200; MMax := 500; FMin := -1; Ftop := '200'; Mtop := '500';
      decimals := 0; end;
   FInit := Data.SetData.FC[2]/Data.SetData.FC[1];
   if FInit > FMax then FInit := FMax;
   if FInit < FMin then FInit := FMin;
   MInit := Data.SetData.Rho;
   if MInit > MMax then MInit := MMax;
   if MInit < MMin then MInit := MMin;
   Sliders.Delete(1); Sliders.Delete(2);
   SetColor(Whitte);
   if Data.DisplayData.SepCurves and Data.DisplayData.SingleDisplay then
   begin
      if (Data.Structure = BCC) then begin
         Sliders.Create(1, FMin, FMax, FInit, 0.70, midy + ctry/2 + 0.01,
                        0.98, decimals, '0', Ftop, 'force ratio', false);
         x0 := 0.47; y0 := midy + ctry/2 + 0.06;
         x1 := 0.58; y1 := midy + ctry/2 + 0.06;
         x2 := 0.47; y2 := midy + ctry/2 - 0.01;
      end
      else begin
         Sliders.Create(1, FMin, FMax, FInit, 0.70, btmy + 0.05,
                        0.98, decimals, '0', Ftop, 'force ratio', false);
         if Data.Structure = cubic then begin
            x0 := 0.73; y0 := btmy - 0.005;
            x2 := 0.88; y2 := btmy - 0.005; end
         else begin
            x0 := 0.45; y0 := btmy + 0.065;
            x1 := 0.57; y1 := btmy + 0.065;
            x2 := 0.45; y2 := btmy - 0.005;
         end;
      end;
      if Data.Structure > diamond then
         Sliders.Create(2, MMin, MMax, MInit, 0.70, btmy - 0.02,
                        0.98, decimals, '1', Mtop, 'mass ratio', false);
   end
   else {Combined form display of some sort} begin
      if Data.Structure < CsCltype then begin
      Sliders.Create(1, FMin, FMax, FInit, 0.70, btmy + 0.14,
                     0.98, decimals, '0', Ftop, 'force ratio', false);
         x0 := 0.73; y0 := btmy + 0.07;
         x1 := 0.88; y1 := btmy + 0.07;
         x2 := 0.73; y2 := btmy + 0.00;
      end
      else begin
         Sliders.Create(1, FMin, FMax, FInit, 0.70, btmy + 0.17,
                        0.98, decimals, '0', Ftop, 'force ratio', false);
         Sliders.Create(2, MMin, MMax, MInit, 0.70, btmy + 0.10,
                        0.98, decimals, '1', Mtop, 'mass ratio', false);
         x0 := 0.73; y0 := btmy + 0.04;
         x1 := 0.88; y1 := btmy + 0.04;
         x2 := 0.73; y2 := btmy - 0.02;
      end;
   end;
   Sliders.DrawAll;
   if Data.Structure <> CsCltype then begin
      ResButtons.Done;
      ResButtons.Create(0, x0, y0, 'Fine');
      if Data.Structure <> cubic then
         ResButtons.Create(1, x1, y1, 'Medium');
      ResButtons.Create(2, x2, y2, 'Coarse');
      ResButtons.number := ord(Data.SliderResolution);
      ResButtons.DrawAll;
   end;
   SetTextJustify(LeftText, TopText);
end;

procedure DrawDosSliders(Data : DataType);
var FInit, MInit : real;
    FMax, MMax, FMin, MMin, decimals : integer;
    Ftop, Mtop : string;
begin
   Fmin := 0; MMin := 1;
   if Data.SliderResolution = Fine then begin
      FMax := 2; MMax := 10; Ftop := '2'; Mtop := '10'; decimals := 2; end
   else if Data.SliderResolution = Medium then begin
      FMax := 20; MMax := 100; Ftop := '20'; Mtop := '100'; decimals := 1; end
   else begin
      FMax := 200; MMax := 500; FMin := -1; Ftop := '200'; Mtop := '500';
      decimals := 0; end;
   FInit := Data.SetData.FC[2]/Data.SetData.FC[1];
   if FInit > FMax then FInit := FMax;
   if FInit < FMin then FInit := FMin;
   MInit := Data.SetData.Rho;
   if MInit > MMax then MInit := MMax;
   if MInit < MMin then MInit := MMin;
   Sliders.Delete(1); Sliders.Delete(2);
   SetColor(Whitte);
   if Data.Structure > diamond then begin
      Sliders.Create(2, 1, MMax, MInit, 0.70, btmy - 0.01,
                     0.98, decimals, '1', Mtop, 'mass ratio', false);
   end;
   Sliders.Create(1, 0, FMax, FInit, 0.36, btmy - 0.01,
                     0.64, decimals, '0', Ftop, 'force ratio',false);
   Sliders.DrawAll;
   ResButtons.Delete(0); ResButtons.Create(0, 0.09, btmy + 0.03, 'Fine');
   ResButtons.Delete(1); ResButtons.Create(1, 0.225, btmy + 0.03, 'Medium');
   ResButtons.Delete(2); ResButtons.Create(2, 0.09, btmy - 0.02, 'Coarse');
   ResButtons.number := ord(Data.SliderResolution);
   ResButtons.DrawAll;
   SetTextJustify(LeftText, TopText);
end;

{*********************** GENERAL PRINTING PROCEDURES **********************}

procedure LabelTitleAndStructure(Data : DataType);
var s1, s2 : string;
begin
  SetColor(Whitte);
  if not Data.DisplayData.SingleDisplay then
     s1 := 'DISPERSION CURVES AND DENSITY OF STATES - '
  else if Data.DCurves then
     s1 := 'PHONON DISPERSION CURVES - '
  else s1 := 'PHONON DENSITY OF STATES - ';
  case Data.Structure of
     none:  s2 := '';
     cubic: s2 := 'SIMPLE CUBIC STRUCTURE';
     BCC:   s2 := 'BCC STRUCTURE';
     FCC:   s2 := 'FCC STRUCTURE';
     CsClType:  s2 := 'CsCl STRUCTURE';
     NaClType:  s2 := 'NaCl STRUCTURE';
     diamond: s2 := 'DIAMOND STRUCTURE';
  end {case};
  SelectViewPort(5); {use DoS ViewPort to centre main heading}
  PutLabel(Top, concat(s1, s2));
end;

procedure OpenAndLabelPort(Data : DataType; dirn : DirnType);
var Vnum, xcoord, ycoord : integer;
    dstr : string;
begin
  with Data do begin
     Vnum := ord(dirn)+1;
     OpenViewport(Vnum);
     if (dirn = IOO) then begin
        PutLabel(Left, 'Frequency, w (THz)');
     end;
     if DisplayData.SepCurves and DisplayData.SingleDisplay then
        begin
           if ((dirn = III) and (Structure <> BCC)) then
              PutLabel(Left, 'Frequency, w (THz)');
           if (dirn = IIO) and (Structure = BCC) then
              begin
                 PutLabel(Left, 'Frequency, w (THz)');
                 PutLabel(Bottom, 'Reduced Wave Vector');
              end
           else
              PutLabel(Bottom, 'Reduced Wave Vector, ka/pi');
        end
     else
        if ((dirn = IIO) and (Structure <> BCC)) or
           ((dirn = III) and (Structure = BCC)) then
              PutLabel(Bottom, 'Reduced Wave Vector, ka/pi');
     DefineScale(Vnum, 0, xrange[dirn], 0, 1.4*wrange); SelectScale(Vnum);
     if (Structure <> BCC) and (dirn = IIO) then begin
        if DisplayData.SepCurves and
           DisplayData.SingleDisplay then
              xcoord := MapX(3*xrange[dirn]/4)
        else begin
           DefineScale(Vnum, -xrange[dirn], 0, 0, 1.4*wrange);
           SelectScale(Vnum);
           xcoord := MapX(-3*xrange[dirn]/4);
        end;
        if ((Structure = FCC) or (Structure = NaClType)
            or (Structure = diamond)) then begin
               SetLineStyle(DashedLn, 0, NormWidth);
               lineTP(xcoord, MapY(0), xcoord, MapY(1.4*wrange));
               xcoord:=(xcoord div 8) - 3;
               ycoord:=round(3/4*abs(MapY(1.4*wrange)-MapY(0))/16);
               print(xcoord, ycoord,'B.Z. boundary');
               SetLineStyle(SolidLn, 0, NormWidth);
            end;
     end;
     if (Structure = BCC) and(dirn = III) then begin
           if DisplayData.SepCurves and
              DisplayData.SingleDisplay then
                 xcoord := MapX(xrange[dirn]/2)
           else begin
              DefineScale(Vnum, -xrange[dirn], 0, 0, 1.4*wrange);
              SelectScale(Vnum);
              xcoord := MapX(-xrange[dirn]/2);
           end;
           SetLineStyle(DashedLn, 0, NormWidth);
           lineTP(xcoord, MapY(0), xcoord, MapY(1.4*wrange));
           xcoord:=(xcoord div 8) - 3;
           ycoord:=round(3/4*abs(MapY(1.4*wrange)-MapY(0))/16);
           print(xcoord, ycoord,'B.Z. boundary');
           SetLineStyle(SolidLn, 0, NormWidth);
     end;
     case dirn of
        IOO : dstr := '100 direction';
        IIO : if (Structure = BCC) and not DisplayData.SingleDisplay then
                 dstr := '110 dirn.'
              else
                 dstr := '110 direction';
        III : dstr := '111 direction';
     end;
     PutLabel(inside, dstr);
     if (DisplayData.SepCurves and DisplayData.SingleDisplay)
        or (dirn = IOO) then
           Axis(0.0, 0.0, 0.3, TickSpace(3*wrange))
     else
        Axis(0.0, -TickSpace(3*wrange), 0.3, TickSpace(30*wrange));
  end {with Data do};
end;

procedure SetUpDosPorts(Data : DataType; dosmax : real);
begin
   if Data.DisplayData.SingleDisplay then begin
      CloseViewPort(10);
      DefineScale(5, 0, 1.4*Data.wrange, 0, 1.1*dosmax);
      OpenViewPort(5); SelectScale(5);
      LabelTitleAndStructure(Data);
      PutLabel(Left, 'Density of States, D(w) (/THz)');
      PutLabel(Bottom, 'Phonon Frequency, w (THz)');
      if Data.DisplayData.TotalOnly then
         Axis(0, 0, TickSpace(1.4*Data.wrange), TickSpace(1.1*dosmax))
      else begin
         OpenViewPort(7); SelectScale(5);
         Axis(0, 0, 2*Data.wrange, TickSpace(2*dosmax));
         OpenViewPort(6); SelectScale(5);
         PutLabel(Bottom, 'Phonon Frequency, w (THz)');
         Axis(0, 0, TickSpace(1.4*Data.wrange), TickSpace(2*dosmax));
      end;
      DrawDosSliders(Data);
   end
   else begin
      DefineScale(5, -1.1*dosmax, 0, 0, 1.4*Data.wrange);
      OpenViewPort(8); SelectScale(5);
      PutLabel(Bottom, 'D(w) (/THz)');
      Axis(0, -1, TickSpace(2*dosmax), TickSpace(14*Data.wrange));
   end;
end;

procedure printforceratio(Data : DataType; ratio : real);
var s, s1, s2 : string;
    lgth : integer;
begin
   selectviewport(4);
   if not Data.DisplayData.SepCurves or not Data.DisplayData.SingleDisplay
      or not Data.DCurves or (Data.Structure = cubic)
      or (Data.Structure = CsClType) then begin
         s1 := 'force ratio 1:';
         if Data.DisplayData.SepCurves and Data.DisplayData.SingleDisplay
             and ((Data.Structure = cubic) or (Data.Structure = CsClType))
         then
              lgth := 20
         else lgth := 24;
      end
      else begin
         s1 := 'force const ratio 1:';
         lgth := 30;
      end;
   s2 := NumStr(ratio, 5, 2);
   s := Concat(s1, s2);
   RubOut(1, 1, lgth, DarkGray);
   SetColor(Whitte);
   Print(1, 1, s);
end;

procedure printmassratio(Data : DataType; ratio : real);
var s, s1, s2 : string;
begin
   selectviewport(4);
   if not Data.DisplayData.SepCurves or not Data.DisplayData.SingleDisplay
      or not Data.DCurves or (Data.Structure = cubic)
      or (Data.Structure = CsClType) then
         s1 := 'mass ratio  1:'
      else s1 := 'atomic mass ratio 1:';
   s2 := NumStr(ratio, 5, 2);
   s := Concat(s1, s2);
   RubOut(1, 2, 19, DarkGray);
   SetColor(Whitte);
   Print(1, 2, s);
end;

procedure ShowColors(SepCurves : boolean);
var mode : integer;
    s : string;
begin
   SelectViewport(4); SelectScale(4);
   for mode := 0 to 3 do begin
      case mode of
         0: s := 'Degenerate';
         1: s := 'Transverse 2';
         2: s := 'Transverse 1';
         3: s := 'Longitudinal';
      end;
      if mode=0 then SetColor(LightCyan) else SetColor(color[mode+7]);
      if SepCurves then
         PlotLine(1*ColWidth, (mode + 0.35)*RowHt,
                  5.5*ColWidth, (mode + 0.35)*RowHt)
      else
         PlotLine(26*ColWidth, (mode + 1.7)*RowHt,
                  30.5*ColWidth, (mode + 1.7)*RowHt);
      SetColor(Whitte);
      if SepCurves then
         Print(8, 6 - mode, s)
      else
         Print(33, 4 - mode, s);
   end;
end;

procedure ShowDoSColors;
var s : string;
    mode : integer;
begin
   SelectViewport(9); SelectScale(9);
   for mode := 1 to 4 do begin
      case mode of
         1: s := 'Transverse 2';
         2: s := 'Transverse 1';
         3: s := 'Longitudinal';
         4: s := 'Total       ';
      end;
      SetColor(color[mode+7]);
      PlotLine(1*ColWidth, (mode + 2.6)*RowHt,
               5.5*ColWidth, (mode + 2.6)*RowHt);
      SetColor(Whitte);
      Print(8, 5 - mode, s);
   end;
end;

{************************* DISPERSION CURVE DISPLAY ************************}

procedure PlotDCurves(Data : DataType; colornum : integer);
var mode, l : integer;
    dirn : DirnType;
begin
 for dirn := IOO to III do begin
   if Data.DisplayData.Superimpose then
      SelectViewport(ord(dirn)+1)
   else begin
      SetColor(whitte);
      OpenAndLabelPort(Data, dirn);
   end;
   if ((not Data.DisplayData.SepCurves) or
        not Data.DisplayData.SingleDisplay) and
     (((Data.Structure <> BCC) and (dirn = IIO)) or
     ((Data.Structure = BCC) and (dirn = III))) then
      DefineScale(ord(dirn)+1, -Data.kmax, 0, 0, 1.4*Data.wrange)
   else DefineScale(ord(dirn)+1, 0, Data.kmax, 0, 1.4*Data.wrange);
   SelectScale(ord(dirn)+1);
   for mode := 0 to (NoOfBranches[ord(Data.Structure)] div 3) - 1 do
      for l := 3 downto 1 do begin
         SetColor(color[colornum + l]);
         if (l=1) and (colornum=0) then
            SetWriteMode (XorPut)
         else
            SetWriteMode (CopyPut);
         PlotDvectors(kvector[dirn], wvector[l + 3*mode, dirn],
                      1, Data.kmax+1);
      end;
   SetWriteMode (CopyPut);
 end {dirn};
end;

procedure DrawDispnCurves(var Data : DataType);
begin
 if Data.DisplayData.Superimpose and not Data.Reset then
    PlotDCurves(Data, 3 {LightRed});
 DrawDCurveSliders(Data);
 with Data.SetData do begin
    printforceratio(Data, FC[2]/FC[1]);
    if Data.Structure > diamond then printmassratio(Data, Rho);
 end;
 if Data.Changed then begin
    CalcDispnCurves(Data);
    Data.Changed := false;
 end;
 PlotDCurves(Data, 0{LightBlue});
 ShowColors(Data.DisplayData.SepCurves and
           Data.DisplayData.SingleDisplay);
 if Data.DisplayData.SingleDisplay then Data.Reset := false;
end;

{************************ DENSITY OF STATES DISPLAY ************************}

procedure PlotDofStates(Data : DataType; colornum : integer);
var l : integer;
begin
   if not Data.DisplayData.TotalOnly and Data.DisplayData.SingleDisplay then
      begin
         SelectViewPort(6); SelectScale(5);
         for l := 1 to 3 do begin
            SetColor(color[colornum+l]);
            PlotDvectors(xvector, doscompvector[l], 1, xvector.Length);
         end;
      end;
   if not Data.DisplayData.SingleDisplay then
      SelectViewPort(8)
   else if Data.DisplayData.TotalOnly then
      SelectViewPort(5)
   else SelectViewPort(7);
   SelectScale(5);
   SetColor(color[colornum+4]);
   if Data.DisplayData.SingleDisplay then
      PlotDvectors(xvector, dosvector, 1, xvector.Length)
   else
      PlotDvectors(dosvector, xvector, 1, xvector.Length);
end;

procedure DrawDofStates(var Data : DataType);
begin
   if Data.DisplayData.Superimpose and not Data.Reset then
      PlotDofStates(Data, 3);
   if Data.Changed then begin
      CalcDofStates(Data, Data.dosmax);
      Data.Changed := false;
   end;
   if (Data.Reset or not Data.DisplayData.Superimpose) then
         SetUpDosPorts(Data, Data.dosmax);
   with Data.SetData do begin
      printforceratio(Data, FC[2]/FC[1]);
      if Data.Structure > diamond then printmassratio(Data, Rho);
   end;
   PlotDofStates(Data, 7);
   if Data.DisplayData.SingleDisplay and
      not Data.DisplayData.TotalOnly then ShowDoSColors;
   Data.Reset := false;
end;

{************************* SELECT TYPE OF DRAWING *************************}

procedure DrawCurves(var Data : DataType);
var Changed : boolean;
begin
  if Data.Structure = none then {do nothing}
   else
   if not Data.DisplayData.SingleDisplay then begin
      Changed := Data.Changed;
      DrawDispnCurves(Data);
      if Changed then Data.Changed := true;
      DrawDofStates(Data);
   end
   else if Data.DCurves then DrawDispnCurves(Data)
      else DrawDofStates(Data);
end;

{************************ SET UP RANGES AND SCALING ************************}

procedure SetwRange(var Data : DataType);
var qx, qy, qz : real;
    dirn : DirnType;
    l, k, ifail : integer;
    w : freqarray;
    fail : boolean;
    s : string;
begin
 WaitMessage.Show;
 Data.wrange := 0.0; ifail := 0;
 for dirn := IOO to III do begin
   for k := 0 to Data.kmax div 2 do begin
      qx := 2*k*pi/Data.kmax;
      {no need to bother with xrange[] here, and coarser grid is enough}
      if dirn > IOO then qy := qx else qy := 0.0;
      if dirn > IIO then qz := qx else qz := 0.0;
      SolveDMatrix(Data.Structure, qx, qy, qz, Data.SetData, w, fail);
      WaitMessage.Update;
      if fail then ifail := k;
      for l := 1 to NoOfBranches[ord(Data.Structure)] do
         if Data.wrange < w[l] then Data.wrange := w[l];
   end; {for k}
   if ifail>0 then begin
      s := NumStr(ifail, 3, 0);
      s := Concat('failure solving eigenvalues at k value ', s);
      announce(s);
   end;
 end {for dirn};
 WaitMessage.Hide;
end;

procedure SetxRangeAndScale(var Data : DataType; var xscale : real);
var actx : real;
begin
 with Data do begin
 actx := rhx - lhx - ctrx;
 case Structure of
  none, cubic, CsClType:
       begin
          xrange[IOO] := 1; xrange[IIO] := 1; xrange[III] := 1;
          xscale := actx/(xrange[IOO] + sqrt(2)*xrange[IIO]);
       end;
  FCC, NaClType, diamond:
       begin
          xrange[IOO] := 1; xrange[IIO] := 1; xrange[III] := 0.5;
          xscale := actx/(xrange[IOO] + sqrt(2)*xrange[IIO]);
       end;
  BCC: begin
          xrange[IOO] := 1; xrange[IIO] := 0.5; xrange[III] := 1;
          xscale := actx/(sqrt(2)*xrange[IIO] + sqrt(3)*xrange[III]);
       end;
  end {case};
  if (not DisplayData.SingleDisplay) or
    not DisplayData.SepCurves then
     xscale := (rhx - lhx)/
               (xrange[IOO] + sqrt(2)*xrange[IIO] + sqrt(3)*xrange[III]);
  end {with Data do};
end;

{**************************** SET UP VIEWPORTS ****************************}

procedure DefineDosLabelPort;
begin
   DefineViewPort(4, lhx + 0.03, rhx, topy - lbly, topy);
   DefineScale(4, 0, (rhx - lhx - 0.03)*GetMaxX, 0, lbly*GetMaxY);
end;

procedure DefineDCurvePorts(Data : DataType; xscale : real);
var ctrlx, ctrrx, thrdx : real;
begin
 with Data do
  if not DisplayData.SingleDisplay then begin
     ctrlx := lhx + 0.75*xscale*xrange[IOO];
     DefineViewPort(1, lhx, ctrlx, lowy, topy);
     if Structure = BCC then begin
        ctrrx := ctrlx + 0.75*xscale*sqrt(3)*xrange[III];
        thrdx := ctrrx + 0.75*xscale*sqrt(2)*xrange[IIO];
        DefineViewPort(2, ctrrx, thrdx, lowy, topy);
        DefineViewPort(3, ctrlx, ctrrx, lowy, topy); end
     else begin
        ctrrx := ctrlx + 0.75*xscale*sqrt(2)*xrange[IIO];
        thrdx := ctrrx + 0.75*xscale*sqrt(3)*xrange[III];
        DefineViewPort(2, ctrlx, ctrrx, lowy, topy);
        DefineViewPort(3, ctrrx, thrdx, lowy, topy);
     end;
     DefineViewPort(8, thrdx, rhx, lowy, topy); {D of S - combined display}
     DefineViewPort(4, lhx, 0.68*rhx, 0.08, 0.30); {labelling screen}
     DefineScale(4, 0, (0.68*rhx - lhx)*GetMaxX, 0, 0.22*GetMaxY);
  end
  else if not DisplayData.SepCurves then begin
     ctrlx := lhx + xscale*xrange[IOO];
     DefineViewPort(1, lhx, ctrlx, lowy, topy);
     if Structure = BCC then begin
        ctrrx := ctrlx + xscale*sqrt(3)*xrange[III];
        DefineViewPort(2, ctrrx, rhx, lowy, topy);
        DefineViewPort(3, ctrlx, ctrrx, lowy, topy); end
     else begin
        ctrrx := ctrlx + xscale*sqrt(2)*xrange[IIO];
        DefineViewPort(2, ctrlx, ctrrx, lowy, topy);
        DefineViewPort(3, ctrrx, rhx, lowy, topy);
     end;
     DefineViewPort(4, lhx, 0.68*rhx, 0.08, 0.30);
     DefineScale(4, 0, (0.68*rhx - lhx)*GetMaxX, 0, 0.22*GetMaxY);
  end
  else if Structure = BCC then begin
     ctrlx := lhx + xscale*xrange[IOO];
     ctrrx := ctrlx + ctrx/2; {don't need such a large gap for label port}
     DefineViewPort(1, lhx, ctrlx, midy + ctry/2, topy);
     DefineViewPort(4, ctrrx, rhx, midy + ctry/2 + 0.10, topy);
     DefineScale(4, 0, (rhx - ctrrx)*GetMaxX, 0,
                 (topy-midy-ctry/2-0.10)*GetMaxY);
     ctrlx := lhx + xscale*sqrt(2)*xrange[IIO];
     ctrrx := rhx - xscale*sqrt(3)*xrange[III];
     DefineViewPort(2, lhx, ctrlx, btmy, midy - ctry/2);
     DefineViewPort(3, ctrrx, rhx, btmy, midy - ctry/2);
  end
  else begin
     ctrlx := lhx + xscale*xrange[IOO];
     ctrrx := rhx - xscale*sqrt(2)*xrange[IIO];
     DefineViewPort(1, lhx, ctrlx, midy + ctry/2, topy);
     DefineViewPort(2, ctrrx, rhx, midy + ctry/2, topy);
     ctrlx := lhx + xscale*sqrt(3)*xrange[III];
     ctrrx := ctrlx + ctrx/2;
     DefineViewPort(3, lhx, ctrlx, btmy, midy - ctry/2);
     DefineViewPort(4, ctrrx, rhx, btmy + 0.10, midy - ctry/2);
     DefineScale(4, 0, (rhx - ctrrx)*GetMaxX, 0,
                 (midy-ctry/2-btmy-0.10)*GetMaxY);
  end;
end;

procedure SetUpDCurves(var Data : DataType; Rescale : boolean);
var xscale : real;
    dirn : DirnType;
begin
   CloseViewport(10); {clears the whole screen}
   SetxRangeAndScale(Data, xscale);
   if Rescale then SetwRange(Data);
   DefineDCurvePorts(Data, xscale);
   LabelTitleAndStructure(Data);
   for dirn := IOO to III do OpenAndLabelPort(Data, dirn);
   OpenViewport(4); SelectScale(4);
end;

{*********************** MENU AND HOT KEY HANDLING ***********************}

procedure HandleMenu(var Menu : TMenu; var Data : DataType);
var j, l : integer;
    EscPressed : boolean;
    dirn : DirnType;
begin
   with Menu do
   if colChosen in [1..5] then
   case colChosen of
    1 : begin
           HelpContext := fileitem;
           case rowChosen of
              1 : AboutCups;
              2 : Intro;
              3 : begin
                     ShowHelp('phonons.HLP','general help 1');
                     ShowHelp('phonons.HLP','general help 2');
                     ShowHelp('phonons.HLP','general help 3');
                     ShowHelp('phonons.HLP','general help 4');
                     ShowHelp('phonons.HLP','general help 5');
                  end;
              4 : begin
                     ShowHelp('phonons.HLP','theory help 1');
                     ShowHelp('phonons.HLP','theory help 2');
                     ShowHelp('phonons.HLP','theory help 3');
                  end;
              5 : Configuration;
              7 : Data.Quit := true;
           end;
        end;
    2 : begin
           HelpContext := structure;
           if (rowChosen=1) or (rowChosen=5) or (rowChosen=9) then
              rowChosen := rowChosen+1; {should not happen}
           case rowChosen of
              1 : Data.Structure := none;
              2 : Data.Structure := cubic;
              3 : Data.Structure := FCC;
              4 : Data.Structure := BCC;
              5 : Data.Structure := none;
              6 : Data.Structure := CsClType;
              7 : Data.Structure := NaClType;
              8 : Data.Structure := diamond;
              9..20 : Data.Structure := none;
           end;
           if Data.Structure = diamond then
              Data.kmax := 30
           else
              Data.kmax := 50;
           for dirn := IOO to III do begin
              kvector[dirn].ReSize(Data.kmax+1);
              for l := 1 to 12 do
                 wvector[l,dirn].ReSize(Data.kmax+1);
           end;
           CalcFScale(Data);
           if Data.DCurves or not Data.DisplayData.SingleDisplay then
              SetUpDCurves(Data, true)
           else
              SetwRange(Data);
           if Data.DCurves then Data.Reset := true;
           Data.Changed := true;
           DrawCurves(Data);
        end;
    3 : with Data.DisplayData do begin
           HelpContext := plotwhat;
           case rowChosen of
              1 : begin
                     Data.DCurves := true;
                     SingleDisplay := true;
                     SetUpDCurves(Data, true);
                  end;
              2 : begin
                     Data.DCurves := false;
                     SingleDisplay := true;
                     CloseViewport(10);
                     DefineDosLabelPort;
                     SetwRange(Data);
                  end;
              3 : begin
                     Data.DCurves := true;
                     SingleDisplay := false;
                     SetUpDCurves(Data, true);
                 end;
              4 : begin
                     Data.ShowStructure := true; Data.Swapped := true;
                     RowActivate(1,4,false);
                     for j := 1 to 9 do RowActivate(4,j,false);
                     for j := 1 to 20 do RowActivate(2,j,true);
                     RowActivate(2,1,false);
                     RowActivate(2,5,false);
                     RowActivate(2,9,false);
                  end;
              end {case};
              if not Data.Showstructure then begin
                 RowActivate(4,5,SingleDisplay and Data.DCurves);
                 RowActivate(4,6,SingleDisplay and Data.DCurves);
                 RowActivate(4,8,SingleDisplay and not Data.DCurves);
                 RowActivate(4,9,SingleDisplay and not Data.DCurves);
              end;

              Data.Reset := true; Data.Changed := true;
              if rowchosen < 4 then DrawCurves(Data);
           end;
      4 : with Data.DisplayData do begin
           HelpContext := plothow;
           case rowChosen of
              1    : ;
              2, 3 : begin
                        Superimpose := (rowChosen = 2);
                        RowChecked(4,2,Superimpose);
                        RowChecked(4,3,not Superimpose);
                     end;
              4    : ;
              5, 6 : begin
                        SepCurves := (rowChosen = 5);
                        RowChecked(4,5,SepCurves);
                        RowChecked(4,6,not SepCurves);
                        Data.Changed := true;
                     end;
              7    : ;
              8,9: begin
                        TotalOnly := (rowChosen = 8);
                        RowChecked(4,8,TotalOnly);
                        RowChecked(4,9,not TotalOnly);
                     end;
           end;
           if rowchosen > 4 then begin
              if Data.DCurves or not SingleDisplay then
                 SetUpDCurves(Data, true)
              else begin {Density of States only}
                 CloseViewport(10);
                 DefineDosLabelPort;
              end;
              Data.Reset := true;
              DrawCurves(Data);
           end;
        end;
    5 : begin
           HelpContext := setdata;
           ReadDataScreen(Data, EscPressed);
           if not EscPressed then begin
              CalcFScale(Data);
              Data.Changed := true;
              DrawCurves(Data);
           end;
        end;
    end {case};
end;

procedure HandleHotKeys(key : byte; var Menu : TMenu; var Data : DataType);
begin
   case key of
    1 : case HelpContext of
        fileitem         : ShowHelp('phonons.HLP','file help');
        structure        : ShowHelp('phonons.HLP','structure help');
        plotwhat         : ShowHelp('phonons.HLP','plotwhat help');
        plothow          : ShowHelp('phonons.HLP','plothow help');
        setdata          : ShowHelp('phonons.HLP','set data help');
        displayitem      : ShowHelp('phonons.HLP','display help');
        crystalitem      : ShowHelp('phonons.HLP','crystal help');
        crystalstructure : ShowHelp('phonons.HLP','crystal structure help');
        end {case};
    2 : begin
           HelpContext := displayitem;
           if  Data.DisplayData.SingleDisplay and not Data.DCurves then begin
              CloseViewport(10);
              DefineDosLabelPort;
              SetwRange(Data);
           end
           else
              SetUpDCurves(Data, true);
           Data.Reset := true;
           DrawCurves(Data);
        end;
    3 : begin
           HelpContext := displayitem;
           if  Data.DisplayData.SingleDisplay and not Data.DCurves then begin
              CloseViewport(10);
              DefineDosLabelPort;
           end
           else
              SetUpDCurves(Data, false);
           Data.Reset := true;
           DrawCurves(Data);
        end;
    4,5 : begin
           Sliders.Erase(1, black);
           Sliders.Erase(2, black);
           if key = 4 then Data.SliderResolution := Fine
              else Data.SliderResolution := Coarse;
           if Data.Structure = none then {do nothing}
           else if Data.DCurves or not Data.DisplayData.SingleDisplay then
              DrawDCurveSliders(Data)
           else DrawDoSSliders(Data);
        end;
    6 : begin
           CheckForEvents;
           if Menu.Chosen then HandleMenu(Menu, Data);
        end;
   end; {case}
end;

procedure HandleSliders(var Data : DataType);
var factor, oldvalue : real;
    j : integer;
begin
 with Data.SetData do begin
   factor := Sliders.Value(1);
   if factor < 1E-8 then factor := 1E-8;
   {keep value non-zero to preserve ratio of higher force constants}
   oldvalue := FC[2];
   if FC[1] > 0 then FC[2] := factor*FC[1];
   if oldvalue > 0 then factor := FC[2]/oldvalue;
   for j := 3 to NoofFConsts do
      FC[j] := factor*FC[j];
   if Data.Structure > diamond then
      Rho := Sliders.Value(2);
 end;
 CalcFScale(Data);
 SetColor(Whitte);
 SetTextJustify(LeftText, TopText);
 Data.Changed := true;
 Data.SlidersFlag := false;
 DrawCurves(Data);
end;

{**************************** CRYSTAL STRUCTURE ***************************}

procedure UserDefinedStructure(var crystal : TCrystal);
begin
   {*******************************************************}

   announce('reserved for you to set up an extra structure');

   {*******************************************************}
end;

procedure DrawCrystal(var crystal, SmallBox : TCrystal; title : string);
begin
   if title <> '' then RubOutXY(0.7, 0.9, 22, black);
   PrintXY(0.7, 0.9, title);
   OpenViewPort(2);
   smallBox.SetDistance(crystal.size, crystal.distance);
   SmallBox.SetViewAngles(crystal.alpha, crystal.beta, crystal.gamma);
   smallBox.DrawBox;
   OpenViewPort(1);
   crystal.SetDistance(crystal.size, crystal.distance);
   crystal.SetViewAngles(crystal.alpha, crystal.beta, crystal.gamma);
   crystal.draw;
end;

PROCEDURE HandleSTMenu(var Menu : TMenu; var crystal, SmallBox : TCrystal;
                       var Data : DataType; var HotKeys : THotKeys);
var dirn : dirntype;
    j, l : integer;
    s : string;
BEGIN
   WITH Menu DO
   CASE colChosen OF
    1 : begin
           HelpContext := fileitem;
           case rowChosen of
              1 : AboutCups;
              2 : Intro;
              3 : ShowHelp('phonons.HLP','crystal help');
              5 : Configuration;
              7 : Data.Quit := true;
           end;
        end;
    2 : begin
           HelpContext := crystalstructure;
           crystal.done;
           CASE RowChosen OF
            1, 5, 9 : ;
            2 : begin
                   Simple_Cubic_Structure(crystal);
                   Data.Structure := cubic;
                end;
            3 : begin
                   FCC_Structure(crystal);
                   Data.Structure := FCC;
                end;
            4 : begin
                   BCC_structure(crystal);
                   Data.Structure := BCC;
                end;
            6 : begin
                   CsCl_Structure(crystal);
                   Data.Structure := CsClType;
                end;
            7 : begin
                   NaCl_Structure(crystal);
                   Data.Structure := NaClType;
                end;
            8 : begin
                   Diamond_structure(crystal);
                   Data.Structure := diamond;
                end;
            10: tetrahedral(crystal);
            11: hexagonal(crystal);
            12: HCP_Structure(crystal);
            13: Cu3Au_Structure(crystal);
            14: CdI2_Structure(crystal);
            15: NiAs_Structure(crystal);
            16: ZnS_Structure(crystal);
            17: CaF2_Structure(crystal);
            18: FCCBZone(crystal);
            19: BCCBZone(crystal);
            20: UserDefinedStructure(crystal);
           end;
           case RowChosen of
              1, 5, 9 : s := '';
              2 : s := 'SIMPLE CUBIC STRUCTURE';
              3 : s := 'FCC STRUCTURE';
              4 : s := 'BCC STRUCTURE';
              6 : s := 'CsCl STRUCTURE';
              7 : s := 'NaCl STRUCTURE';
              8 : s := 'DIAMOND STRUCTURE';
              10: s := 'TETRAHEDRAL STUCTURE';
              11: s := 'HEXAGONAL STUCTURE';
              12: s := 'HCP STUCTURE';
              13: s := 'Cu3Au STUCTURE';
              14: s := 'CdI2 STUCTURE';
              15: s := 'NiAs STUCTURE';
              16: s := 'ZnS STUCTURE';
              17: s := 'CaF2 STUCTURE';
              18: s := 'FCC BRILLOUIN ZONE';
              19: s := 'BCC BRILLOUIN ZONE';
              20: s := 'User Defined Structure';
           end {case};
           HideCursor;
           DrawCrystal(crystal, SmallBox, s);
           ShowCursor;
           {set up for return to phonons proper}
           if Data.Structure = diamond then
              Data.kmax := 30
           else
              Data.kmax := 50;
           for dirn := IOO to III do begin
              kvector[dirn].ReSize(Data.kmax+1);
              for l := 1 to 12 do
                 wvector[l,dirn].ReSize(Data.kmax+1);
           end;
        end;
    3 : if RowChosen<4 then with Data.DisplayData do begin
           HelpContext := plotwhat;
           crystal.done; Sliders.done;
           SetUpHotKeys(HotKeys);
           NewMyDVector(xvector); NewMyDVector(dosvector);
           xvector.init(1); dosvector.init(1);
           {(re)initialize DVectors to minimum length}
           for l := 1 to 3 do begin
              NewMyDVector(doscompvector[l]);
              doscompvector[l].init(1);
           end;
           for dirn := IOO to III do begin
              NewMyDVector(kvector[dirn]);
              kvector[dirn].init(Data.kmax+1);
              for l := 1 to 12 do begin
                 NewMyDVector(wvector[l,dirn]);
                 wvector[l,dirn].init(Data.kmax+1);
              end;
           end;

           Data.Reset := true; Data.ShowStructure := false;
           case rowChosen of
              1 : begin
                     Data.DCurves := true;
                     SingleDisplay := true;
                     SetUpDCurves(Data, true);
                  end;
              2 : begin
                     Data.DCurves := false;
                     SingleDisplay := true;
                     CloseViewport(10);
                     DefineDosLabelPort;
                     SetwRange(Data);
                  end;
              3 : begin
                     SingleDisplay := false;
                     SetUpDCurves(Data, true);
                 end;
              4 : ;
           end {case};
           RowActivate(1,4,true);
           for j := 9 to 20 do RowActivate(2,j,false);
           RowActivate(4,2,true);
           RowActivate(4,3,true);
           RowActivate(4,5,SingleDisplay and Data.DCurves);
           RowActivate(4,6,SingleDisplay and Data.DCurves);
           RowActivate(4,8,SingleDisplay and not Data.DCurves);
           RowActivate(4,9,SingleDisplay and not Data.DCurves);
           DrawCurves(Data);
    end;
    4 : ;
    5 : ;
   end;
end;

PROCEDURE HandleSTHotKeys (key : byte; var Menu : TMenu; var Data : DataType;
                           var crystal, SmallBox : TCrystal;
                           var HotKeys : THotKeys);
BEGIN
   CASE key OF
    1 : case HelpContext of
        fileitem         : ShowHelp('phonons.HLP','file help');
        structure        : ShowHelp('phonons.HLP','structure help');
        plotwhat         : ShowHelp('phonons.HLP','plotwhat help');
        plothow          : ShowHelp('phonons.HLP','plothow help');
        setdata          : ShowHelp('phonons.HLP','set data help');
        displayitem      : ShowHelp('phonons.HLP','display help');
        crystalitem      : ShowHelp('phonons.HLP','crystal help');
        crystalstructure : ShowHelp('phonons.HLP','crystal structure help');
        end{case};
    2 : begin
           HelpContext := crystalitem;
           openViewPort(1);
           crystal.boxDrawn := NOT crystal.boxDrawn;
           DrawCrystal(crystal, SmallBox, '');
        end;
    3 : BEGIN
           HelpContext := crystalitem;
           IF crystal.size<2 THEN
              crystal.size := crystal.size+0.1
           ELSE crystal.size := 0.5;
           DrawCrystal(crystal, SmallBox, '');
        END;
    4 : BEGIN
           HelpContext := crystalitem;
           IF crystal.distance<1.30 THEN
              crystal.distance:=10
           ELSE crystal.distance:=crystal.distance/1.5;
           DrawCrystal(crystal, SmallBox, '');
        END;
    5 : if menu.chosen then
           HandleSTMenu(Menu, crystal, SmallBox, Data, HotKeys);
   END;
    {case}
END;

procedure HandleSTSliders(var crystal, SmallBox : TCrystal);
begin
   HelpContext := crystalitem;
   crystal.alpha:=sliders.value(1);
   crystal.beta:=sliders.value(2);
   crystal.gamma:=sliders.value(3);
   DrawCrystal(crystal, SmallBox, '');
end;

{****************************** MAIN PROGRAM *****************************}

var
   lglobal   : integer;
   closedirn : DirnType;
   key       : byte;
   a, b      : PointType;
   Data      : DataType;
   Menu      : TMenu;
   HotKeys   : THotKeys;
   SmallBox  : TCrystal;
   crystal   : TCrystal;
   st        : string;

   procedure HBar(a, b : PointType); far;
   begin
      lineTP(1, b.y, GetMaxX, b.y);
   end;

   procedure VBar(a, b : PointType); far;
   begin
      lineTP(b.x, 1, b.x, GetMaxY);
   end;

begin
  { MAC only
    New(Sliders);
    New(ResButtons);
    New(WaitMessage);
    New(Menu);
    New(HotKeys);
    New(SmallBox);
    New(crystal);}
   SetUpConstants;
   CupsInit;
   DelayTime := 100;
   Sliders.Init; ResButtons.init;
   SetUpMenu(Menu);
   SetUpHotKeys(HotKeys);
   InitializePortsAndData(Menu, Data);
   CalcFScale(Data);
   SetUpDCurves(Data, true);
   DrawDispnCurves(Data);
   Intro;
   Data.Swapped := true; Data.ShowStructure := false;
   Data.SlidersFlag := false;
   repeat
      if Data.ShowStructure then begin
         if Data.Swapped then begin
            Data.Swapped := false;
            xvector.free; dosvector.free;
            DisposeMyDVector(xvector); DisposeMyDVector(dosvector);
            for lglobal := 1 to 3 do begin
               doscompvector[lglobal].free;
               DisposeMyDVector(doscompvector[lglobal]); end;
            for closedirn := IOO to III do begin
               kvector[closedirn].free;
               DisposeMyDVector(kvector[closedirn]);
               for lglobal := 1 to 12 do begin
                  wvector[lglobal,closedirn].free;
                  DisposeMyDVector(wvector[lglobal,closedirn]);
               end;
            end;
            SetUpSTHotKeys(HotKeys);
            initializeShowStructure(Data, crystal, SmallBox, st);
            DrawCrystal(crystal, SmallBox, st);
         end;
         CheckForEvents;
         if HotKeys.Pressed(key) then
            HandleSTHotKeys(key, Menu, Data, crystal, SmallBox, HotKeys);
         if Menu.Activated then
            HandleSTMenu(Menu, crystal, SmallBox, Data, HotKeys);
         if Sliders.Changed then HandleSTSliders(crystal, SmallBox);
      end
      else begin
         CheckForEvents;
         if HotKeys.Pressed(key) then HandleHotKeys(key, Menu, Data);
         if Menu.Activated then HandleMenu(Menu, Data);
         if ResButtons.Changed then begin
            Sliders.Erase(1, black); Sliders.Erase(2, black);
            if ResButtons.number = 0 then Data.SliderResolution := Fine
            else if ResButtons.number = 1 then Data.SliderResolution := Medium
               else Data.SliderResolution := Coarse;
            if Data.Structure = none then {do nothing}
            else if Data.DCurves or not Data.DisplayData.SingleDisplay then
               DrawDCurveSliders(Data)
            else DrawDoSSliders(Data);
         end;
         if Sliders.Changed then Data.SlidersFlag:=true else
            if Data.SlidersFlag then HandleSliders(Data);
         if ClickedInside(Data) then begin
            SelectViewPort(10);
            SetColor(Blue);
            if Data.DisplayData.SingleDisplay and not Data.DCurves then
               DrawRubberBand(VBar, a, b, DarkGray, true)
            else
               DrawRubberBand(HBar, a, b, DarkGray, true);
            SetColor(Whitte);
         end;
      end;
   until Data.Quit;
   crystal.done;
   Sliders.Done;
   ResButtons.Done;
  { MAC only
    Dispose(Sliders);
    Dispose(ResButtons);
    Dispose(WaitMessage);
    Dispose(Menu);
    Dispose(HotKeys);
    Dispose(SmallBox);
    Dispose(crystal);}
   CUPSDone;
end.
