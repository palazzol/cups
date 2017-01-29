           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.0 (94/08/08) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

PROGRAM AtomicPolarization;
{
 **************************** Atomic Polarization **************************
 *                              Version 3.7                                *
 *                                                                         *
 *                    Concept and Design by Lyle Roelofs                   *
 *                Programming by Nat Johnson and Lyle Roelofs              *
 *                          Department of Physics                          *
 *                            Haverford College                            *
 *                           Haverford, PA  19041                          *
 *                                                                         *
 *                               June, 1994                                *
 *                                                                         *
 ***************************************************************************

 This program illustrates polarization of atoms by externally applied
 electric fields.}

{This program compiles to disk without difficulty using Turbo 7.0.
 If difficult is experienced in compiling with earlier versions,
 invoke turbo with the /s switch and under Options/Environment/Startup
 turn on Use Expanded Memory and restart.}

USES CUPS, CUPSmupp, crt, graph, CUPSgui, CUPSfunc, CUPSgrph, dos;
                                               {Version 3.7}

TYPE
   DipoleVal = Array[1..25] OF real; {25 = MaxAtoms}
   DipoleBool = Array[1..25] OF Boolean;
   TTimer = object
       lastTime : LONGINT;
       PROCEDURE ResetTimer;
       FUNCTION  GetDeltaTicks(ResetFlag : Boolean) : LONGINT;
   END;

CONST
   MaxAtoms = 25;
   DarkBlue = 1;
   DarkGreen = 2;
   WindowColor = 3;
   DipoleColor = 11;
   BackgroundColor = 7;                         {Light gray}
   SelectedDipoleColor = 13;
   FieldLineColor = 14;
   TextColor = 14;

VAR
{The following group of global variables are objects such as menus,
sliders, buttons and help screens}
   DipoleMenu : Tmenu;
   sliders    : Tsliders;
   HotKeys    : THotKeys;
   mouseTimer : TTimer;
   Credits    : HelpScrType;

   QuitFlag : Boolean;      {True when it's time to quit.}
   DipoleFieldMode : integer;
                    {1 = external, 2 = single, 3 = combined}
   ViewNucleus : Boolean;
                    {TRUE = view nucleus, FALSE = view entire atom}
   ViewArrows : Boolean;
                    {TRUE = show dipole arrows, FALSE = don't show arrows}

{The following values show the current slider values, they are set
initially in InitializeGlobals, and are changed in GetNewSliderValues}
   DipoleField, PolSliderValue : real;

{the following global variables monitor the status of the Atom in
Dipole Mode: their position, dipole moment, and whether they are
currently activated.  They are initialized in InitializeGlobals
and modified in DesignateAtom, PlaceDipole, and RemoveDipole}
   DipoleOn : DipoleBool;  {whether the atom is in use}
   DipoleSelected : DipoleBool; {whether the atom is selected}
   DipolePosX : DipoleVal; {x positions of atoms}
   DipolePosY : DipoleVal; {y positions of atoms}
   Polarizability : DipoleVal; {polarizabilities of atoms}

{This is the last selected Atom for Display single dipole field}
   SelectedDipole : integer;

{The following variables monitor the x and y components of the dipole
moments for all the atoms.  They are initialized in InitializeGlobals and
are changed only in the procedure GetNewDipoleMoments}
   DipoleMomentX, DipoleMomentY : DipoleVal;

PROCEDURE TTimer.ResetTimer;
VAR a : longint;
BEGIN
   a := GetDeltaTicks(TRUE);
END;

FUNCTION TTimer.GetDeltaTicks(ResetFlag : Boolean) : LONGINT;
{This is modified from SS's procedure in CUPS.PAS which checks the
 interval since the last call.  This one checks since the last ResetTimer.}
VAR
   Regs      : registers;
   TickCount : Longint;
BEGIN
   regs.AX := 0;
   Intr($1A,Regs);
   TickCount := regs.CX SHL 16 + regs.DX;
   GetDeltaTicks := TickCount - lastTime;
   IF ResetFlag THEN
      lastTime := TickCount;
END;

PROCEDURE BottomMessage(message : String);
{Prints a message at the bottom of the screen.  Note that ViewPort 1 is
automatically selected afterwards, in the case that another viewport
is selected, the viewport must be reselected manually.}
BEGIN
   GraphBackColor := DarkGray;;;
   OpenViewPort(4);
   SetColor(LightGreen);
   Print(1,1,message);
   SelectViewPort(1);
   SetColor(TextColor);
END;

PROCEDURE TopMessage(message : String);
{Prints a message at the top right of the screen.  Note that ViewPort 2 is
automatically selected afterwards, in the case that another viewport
is selected, the viewport must be reselected manually.}
BEGIN
   GraphBackColor := DarkGray;;;
   OpenViewPort(5);
   SetColor(textcolor);
   Print(1,1,message);
   SelectViewPort(2);
END;

PROCEDURE InitializeGlobals;
VAR
   inc : integer;  {incremental}
BEGIN
   QuitFlag := FALSE;
{default modes}
   ViewNucleus := FALSE;        {entire atom}
   ViewArrows := TRUE;          {show dipole arrows}
   DipoleFieldMode := 3;        {display combined field}
{default slider values for Dipole Mode}
   PolSliderValue := 34;        {value for potassium}
   DipoleField := 5;
   FOR inc := 1 to MaxAtoms DO
   BEGIN
      DipolePosX[inc] := 0;
      DipolePosY[inc] := 0;
      DipoleMomentX[inc] := 170;     {polarizability * DipoleField}
      DipoleMomentY[inc] := 0;
      DipoleOn[inc] := FALSE;
   END;
   SelectedDipole := 1;
   DipoleOn[1] := TRUE;  {start off with only one Atom at 0,0}
   DipoleSelected[1] := TRUE;
   Polarizability[1] := PolSliderValue;
END;

PROCEDURE DefineViewports;
BEGIN
   DefineViewPort(1, 0, 0.65, 0.1, 0.95);        {Field window}
   DefineViewPort(2, 0.66, 1, 0.46, 0.95);       {UpperRight window}
   DefineViewPort(3, 0.66, 1, 0.1, 0.45);        {Sliders window}
   DefineViewPort(4, 0, 1, 0.055, 0.09);         {bottom message window}
   DefineViewPort(5, 0.66, 1, 0.91, 0.95);       {small message window}
   DefineScale(1,-25,25,-25,25);
   SelectScale(1);
END;

PROCEDURE DefineColors;
BEGIN
   SetRGBPalette(1,$00,$14,$24);    {Dark Blue}
   SetRGBPalette(2,$00,$24,$14);    {Dark Green}
END;

PROCEDURE DefineHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      Init(6);
      key[1] := 'F1-Help';
      key[2] := 'F2-Solve';
      key[3] := 'F3-Slct all';
      key[4] := 'F4-Clear';
      key[5] := 'Del-Del.Slct';
      key[6] := 'F10-Menu';
   END;
END;
     
PROCEDURE DefineDipoleMenu;   {defines the menu, but does not display it.}
BEGIN
   WITH DipoleMenu DO
   BEGIN
      init;
      column(1, 'File');
      row(1, 1, 'About CUPS');
      row(1, 2, 'About Program');
      row(1, 3, 'System Configuration');
      row(1, 4, 'Exit Program');
      column(2, 'Display');
      row(2, 1, 'External Field');
      row(2, 2, 'Single Dipole Field');
      row(2, 3, 'Combined Field');
      row(2, 4, 'Label Atoms w/ Dipole Arrows');
      row(2, 5, 'Label Atoms w/ Letters');
      column(3, 'Tools');
      row(3, 1, 'View Typ. Atom (Nuc. Sc.)');
      row(3, 2, 'View Typ. Atom (Atm. Sc.)');
      row(3, 3, 'Atom Input Screen');
      row(3, 4, 'Field Probe');
      row(3, 5, 'Probe Atom');
      row(3, 6, 'Refresh Screen');
      column(4, 'Configs.');
      row(4, 1, 'Single Atom');
      row(4, 2, 'Horizontal line');
      row(4, 3, 'Vertical line');
      row(4, 4, 'Diagonal line');
      row(4, 5, 'Square lattice');
      row(4, 6, 'Rectangular lattice');
      row(4, 7, 'Close-Packed lattice');
      row(4, 8, 'Alloy');
      row(4, 9, 'Clear all');
      column(5, 'Help');
      row(5, 1, 'Display Menu');
      row(5, 2, 'View Options');
      row(5, 3, 'Atom Selection and Hot Keys');
      row(5, 4, 'Keyboard Atom Plcmnt');
      row(5, 5, 'Mouse Atom Plcmnt');
      row(5, 6, 'Field Probe');
      row(5, 7, 'Probe Atom');
      row(5, 8, 'Sliders');
      RowChecked(3,2,TRUE);
      AutoChecking(2,1);
   END;
END;

PROCEDURE DefineSliders;   {define sliders but do not display yet}
{the default values are global variables, so if the sliders are closed
down and then redisplayed, they will have the same value they last had}
VAR
   slider2label : string;
   slider1default, slider2default, slider2min, slider2max : real;
   slider2minstr, slider2maxstr : string;
   slider1Ypos, slider2Ypos : real;

BEGIN
   slider1Ypos := 0.36;
   slider2Ypos := 0.19;
   slider2label := 'Polarizability';
   slider2default := PolSliderValue;
   slider1default := DipoleField;
   slider2min := 0.2;
   slider2max := 34.;
   sliders.init;
   slider2minstr := NumStr(slider2min, 2, 0);
   slider2maxstr := NumStr(slider2max, 2, 0);
   sliders.create(1, 0, 10, slider1default, 0.67, slider1Ypos, 0.98, 1,
                             '0', '10', 'External Field', FALSE);
   sliders.create(2, slider2min, slider2max, slider2default, 0.67, slider2Ypos,
                 0.98, 1, slider2minstr, slider2maxstr, slider2label, FALSE);
END;

PROCEDURE DefineCredits;
BEGIN
   Credits[2] := '              Atomic Polarization                ';
   Credits[4] := '       Concept and design by Lyle Roelofs        ';
   Credits[5] := '   Programming by Nat Johnson and Lyle Roelofs   ';
   Credits[6] := '              Department of Physics              ';
   Credits[7] := '                Haverford College                ';
   Credits[8] := '               Haverford, PA  19041              ';
   Credits[9] := '                       USA                       ';
   Credits[11] :='      Copyright (c) John Wiley & Sons 1995       ';
   Credits[13] :='     For use with companion text -- CUPS E&M     ';
   Credits[15] :=' This program implements an investigation of the ';
   Credits[16] :=' origin of atomic polarization, the nature of the';
   Credits[17] :=' field of a polarized atom, and the interaction  ';
   Credits[18] :=' of up to 25 polarizable atoms in various geo-   ';
   Credits[19] :=' metries.  (The program solves for the inter-    ';
   Credits[20] :=' actions self-consistently.)                     ';
   Credits[22] :='    (Include file AtomPol.HLP in same directory  ';
   Credits[23] :='         /folder to enable Help screens.)        ';
   Credits[25] :='        Hit any key or click to continue.        ';
END;

PROCEDURE Initialize;
BEGIN
   CUPSInit;
   Randomize;
   InitializeGlobals;
   DefineViewPorts;
   DefineColors;
   DefineHotKeys;
   DefineDipoleMenu;
   DefineSliders;
   DefineCredits;
END;

PROCEDURE ClearAll; {clears screen, including sliders,}
          {but redefines sliders so they can be displayed again}
BEGIN
   ClearMuppetPort;
   DefineSliders;              {Sets up both sliders and buttons}
END;

PROCEDURE DisplayScreen;
BEGIN
   GraphBackColor := Black;;
   OpenViewPort(1);
   OpenViewPort(2);
   OpenViewPort(3);
   OpenViewPort(4);
   OpenViewPort(5);
   sliders.drawall;
   HotKeys.display;
END;

PROCEDURE WindowOutline;
BEGIN
   SetColor(WindowColor);
   PlotLine(-25,-25,-25,25);
   PlotLine(-25,25,25,25);
   PlotLine(25,25,25,-25);
   PlotLine(25,-25,-25,-25);
END;

PROCEDURE DrawEntireAtom; {this draws the theoretical view of the entire atom}
VAR
   angle, radius : real;
   ElectronX, ElectronY : real;
   inc, X, Y : Integer;

BEGIN
   TopMessage(' shift magnified 10,000 x');
   SetColor(14);
   FOR inc := 1 to 1500 DO
   BEGIN
      angle := Random;
      angle := 2 * PI * angle;   {pick an angle between 0 and 2 PI}
      radius := Random(75);      {pick a radius between 0 and 75}
      ElectronX := radius * cos(angle) + PolSliderValue * DipoleField / 30;
      ElectronY := radius * sin(angle);
      X := Round(111 - ElectronX);
      Y := Round(109 + ElectronY);
      PutPixel(X,Y,14);
   END;
   SetColor(LightCyan);
   X := Round(111 + PolSliderValue * DipoleField / 30);
   FillEllipse(X, 109, 5, 5);         {represents nucleus}
END;

PROCEDURE DrawNucleusOnly;
{this procedure draws protons, neutrons, and electron rings.}
VAR
   angle, radius : real;
   ElectronX, ElectronY : real;
   inc, X, Y : Integer;

BEGIN
   TopMessage(' magnified view of nucleus');
   X := Round(111 + PolSliderValue * DipoleField / 30);
   SetColor(LightCyan);  {first draw nucleus outline (in cyan) }
   Circle(X,109,60);     {draw nucleus}
   SetColor(LightRed);   {color for protons - Lt. Red}
   Circle(X,153,14);     {top proton}
   Circle(X,65,14);      {bottom proton}
   X := X + 44;
   Circle(X,109,14);     {right proton}
   X := X - 88;
   Circle(X,109,14);     {left proton}
   SetColor(DarkGreen);  {now draw neutrons (in dk.green) }
   X := X + 18;
   Circle(X,135,16);     {bottom left neutron}
   Circle(X,83,16);      {top left neutron}
   X := X + 52;
   Circle(X,135,16);     {bottom right neutron}
   Circle(X,83,16);      {top right neutron}
   X := X - 26;
   SetColor(LightRed);   {color for protons - Lt. Red}
   Circle(X,109,20);     {centered proton}
   SetColor(Yellow);     {now draw electron cloud (in yellow) }
   FOR inc := 1 to 1500 DO
   BEGIN
      angle := Random;
      angle := 2 * PI * angle;   {pick an angle between 0 and 2 PI}
      radius := Random(80);      {pick a radius between 90 and 170}
      radius := radius + 90;
      ElectronX := radius * cos(angle) + PolSliderValue * DipoleField / 30;
      ElectronY := radius * sin(angle);
      X := Round(111 - ElectronX);
      Y := Round(109 + ElectronY);
      PutPixel(X,Y,14);
   END;
END;

PROCEDURE DrawArrow(X1, Y1, X2, Y2 : real);
          {this procedure draws an arrow from (X1,Y1) to (X2,Y2)}
VAR
   deltaX, deltaY : real;
   newX, newY : real;
BEGIN
   PlotLine(X1,Y1,X2,Y2);                   {draw the shaft of the arrow}
   deltaX := X2 - X1;
   deltaY := Y2 - Y1;
   newX := X2 - (0.4 * deltaX - 0.2 * deltaY);
   newY := Y2 - (0.2 * deltaX + 0.4 * deltaY);
   PlotLine(newX,newY,X2,Y2);               {left half of arrowhead}
   newX := X2 - (0.4 * deltaX + 0.2 * deltaY);
   newY := Y2 - (-0.2 * deltaX + 0.4 * deltaY);
   PlotLine(newX,newY,X2,Y2);               {right half of arrowhead}
END;

PROCEDURE CheckWindow;
CONST
   Square: ARRAY[1..4] OF PointType = ((X: 0;Y:0),(X:222;Y:0),(X:222;Y:218),
   (X:0;Y:218));

BEGIN
   GraphBackColor := DarkGray;;;
   OpenViewPort(2);
   SetColor(2);
   SetFillStyle(0,2);
   FillPoly(4,Square);
END;

PROCEDURE DisplayAtom;
{this opens viewport 2 and displays the atom view corresponding
to the present View.}
BEGIN
   GraphBackColor := Black;;
   OpenViewPort(2);
   IF ViewNucleus THEN
      DrawNucleusOnly
   ELSE
      DrawEntireAtom;
   WindowOutline;                    {redraw outline of window}
END;

PROCEDURE DrawRightArrowHead(X, Y : Real);

VAR
   Xend, Yend  : Real;
BEGIN
   Xend := X - 0.8;
   Yend := Y + 0.6;
   PlotLine(X, Y, Xend, Yend);
   Yend := Y - 0.6;
   PlotLine(X, Y, Xend, Yend);
END;

PROCEDURE ExternalField;
{draws the constant external field only (straight lines)}
VAR
   X1, X2, Y1, Y2 : real;
   step : real;   {distance between field lines}
   inc : integer; {incremental}

BEGIN
   bottommessage('Externalfield');
   step := 25/DipoleField;
   X1 := -25;
   X2 := 25;
   PlotLine(X1,0,X2,0);
   DrawRightArrowHead(-20, 0);
   FOR inc := 1 to 25 DO
   BEGIN
      Y1 := inc*step;
      Y2 := Y1;
      PlotLine(X1,Y1,X2,Y2);  {draw top line}
      DrawRightArrowHead(-20, Y1);
      Y1 := 0 - Y1;
      Y2 := Y1;
      PlotLine(X1,Y1,X2,Y2);  {draw symmetrical bottom line}
      DrawRightArrowHead(-20, Y1);
      IF ((inc+1)*step) >= 25 THEN
         inc := 25;
   END;
END;

FUNCTION InDipole(X, Y, radius : real):integer;
{checks to see if the point (X,Y) is within a given radius of a Atom}
{if so, the number of the Atom is given, otherwise zero is returned}
VAR
   inc : integer; {incremental}
   delX, delY, checkR : real;
BEGIN
   InDipole := 0;
   FOR inc := 1 to MaxAtoms DO
   BEGIN
      IF DipoleOn[inc] THEN
      BEGIN
         delX := X - DipolePosX[inc];
         delY := Y - DipolePosY[inc];
         checkR := delX*delX + delY*delY;
         checkR := sqrt(checkR);
         IF checkR < radius THEN
         BEGIN
            InDipole := inc;
            inc := MaxAtoms;
         END;  {End If}
      END;  {END If Dipole ON}
   END;  {End For}
END;

FUNCTION InWindow(X, Y : real): BOOLEAN;
BEGIN
   IF (X > 25) OR (X < -25) OR (Y > 25) OR (Y < -25) THEN
      InWindow := FALSE
   ELSE
      InWindow := TRUE;
END;

PROCEDURE ETotal(VAR EtotalX, EtotalY : real; Xpos, Ypos : real);
VAR
   inc : integer;                {incremental}
   DiDisX, DiDisY : real;        {displacement from the point to each Atom}
   XMmnt, YMmnt : real;          {x and y components of the dipole moment}
   Xsqrd, Ysqrd, R, R5th : real; {used to calculate dipole field}
   deltaX, deltaY : real;        {change in field due to the dipole}
   scale : real;

BEGIN
   EtotalX := DipoleField;
   EtotalY := 0;
   scale := 1;
   FOR inc := 1 to MaxAtoms DO
   BEGIN
      IF DipoleOn[inc] THEN
      BEGIN
         DiDisX := Xpos - DipolePosX[inc];
         DiDisY := Ypos - DipolePosY[inc];
         Xsqrd := DiDisX*DiDisX;
         Ysqrd := DiDisY*DiDisY;
         XMmnt := DipoleMomentX[inc];
         YMmnt := DipoleMomentY[inc];
         R := sqrt(Xsqrd + Ysqrd);
         IF R > 2.49 THEN  {object cannot exert a force on itself}
         BEGIN
            R5th := R*R*R*R*R;
            deltaX := (XMmnt*(2*Xsqrd-Ysqrd) + YMmnt*(3*DiDisX*DiDisY))/R5th;
            deltaY := (XMmnt*(3*DiDisX*DiDisY) + YMmnt*(2*Ysqrd-Xsqrd))/R5th;
            EtotalX := EtotalX + scale*deltaX;
            EtotalY := EtotalY + scale*deltaY;
         END; {End IF}
      END; {End IF Dipole On}
   END; {End FOR}
END;

PROCEDURE ReflectThruDipoleHorizontal(DipoleNum : integer; X, Y : real; VAR Xrefl, Yrefl : real);
VAR
qX, qY, qdotp, DipoleMag2, JumpX, JumpY : real;

BEGIN
   qX := DipolePosX[DipoleNum] - X;
   qY := DipolePosY[DipoleNum] - Y;
   DiPoleMag2 := sqr(DipoleMomentX[DipoleNum]) + sqr(DipoleMomentY[DipoleNum]);
   qdotp := qX * DipoleMomentX[DipoleNum] + qY * DipoleMomentY[DipoleNum];
   JumpX := 2*qdotp*DipoleMomentX[DipoleNum]/DipoleMag2;
   JumpY := 2*qdotp*DipoleMomentY[DipoleNum]/DipoleMag2;
   Xrefl := X + JumpX;
   Yrefl := Y + JumpY;
END;

PROCEDURE ReflectThruDipoleCenter(DipoleNum : integer; X, Y : real ; VAR Xrefl, Yrefl : real);
BEGIN
   Xrefl := 2*DipolePosX[DipoleNum] - X;
   Yrefl := 2*DipolePosY[DipoleNum] - Y;
END;

PROCEDURE SingleDipoleFieldLines(angle : real);
{this procedure calculates 4 symmetric dipole field lines leaving the
selected Atom at a certain angle relative to the dipole moment,
which continue until they go off the screen or run into each other.}
VAR
   alpha : real; {angle of dipole moment arrow}
   Xdis, Ydis, xsqrd, ysqrd : real;  {used to calculate next point}
   Xold : ARRAY [1..4] OF real; {old x components of 4 reflected lines}
   Yold : ARRAY [1..4] OF real; {old y components of 4 reflected lines}
   Xnew : ARRAY [1..4] OF real; {new x components of 4 reflected lines}
   Ynew : ARRAY [1..4] OF real; {new y components of 4 reflected lines}
   endline : ARRAY [1..4] OF boolean;  {if end of field line is reached}
   scale, magnitude, magnitude2 : real;  {scale factor to assure smooth lines}
   deltaX, deltaY, deltaX2, deltaY2 : real; {speed at which line is traveled}
   inc : integer; {incremental}
BEGIN
   alpha := ArcTan(DipoleMomentY[SelectedDipole]/DipoleMomentX[SelectedDipole]);
   IF DipoleOn[SelectedDipole] THEN
   BEGIN
      scale := 0.2;
      Xdis := 2.5 * cos(alpha + angle);
      Ydis := 2.5 * sin(alpha + angle);
      Xold[1] := DipolePosX[SelectedDipole] + Xdis;
      Yold[1] := DipolePosY[SelectedDipole] + Ydis;
      ReflectThruDipoleHorizontal(SelectedDipole, Xold[1], Yold[1], Xold[2], Yold[2]);
      Xold[3] := DipolePosX[SelectedDipole] - Xdis;
      Yold[3] := DipolePosY[SelectedDipole] - Ydis;
      ReflectThruDipoleHorizontal(SelectedDipole, Xold[3], Yold[3], Xold[4], Yold[4]);
      FOR inc := 1 to 4 DO
         endline[inc] := FALSE;
      REPEAT
         Xdis := Xold[1] - DipolePosX[SelectedDipole];
         Ydis := Yold[1] - DipolePosY[SelectedDipole];
         xsqrd := Xdis*Xdis;
         ysqrd := Ydis*Ydis;
         deltaX := DipoleMomentX[SelectedDipole] * (2 * xsqrd - ysqrd) +
                   DipoleMomentY[SelectedDipole] * 3 * Xdis * Ydis;
         deltaY := DipoleMomentX[SelectedDipole] * 3 * Xdis * Ydis +
                   DipoleMomentY[SelectedDipole] * (2 * ysqrd - xsqrd);
         deltaX2 := deltaX*deltaX;
         deltaY2 := deltaY*deltaY;
         magnitude2 := deltaX2 + deltaY2;
         magnitude := sqrt(magnitude2);
         IF magnitude2 = 0 THEN
            magnitude2 := 0.1;
         Xnew[1] := Xold[1] + scale * deltaX / magnitude;
         Ynew[1] := Yold[1] + scale * deltaY / magnitude;
         IF endline[2] = FALSE THEN
            ReflectThruDipoleHorizontal(SelectedDipole, Xnew[1], Ynew[1], Xnew[2], Ynew[2]);
         IF (endline[3] = FALSE) OR (endline[4] = FALSE) THEN
            ReflectThruDipoleCenter(SelectedDipole, Xnew[1], Ynew[1], Xnew[3], Ynew[3]);
         IF endline[4] = FALSE THEN
            ReflectThruDipoleHorizontal(SelectedDipole, Xnew[3], Ynew[3], Xnew[4], Ynew[4]);
         FOR inc := 1 to 4 DO
         BEGIN
            IF (InWindow(Xnew[inc],Ynew[inc]) = FALSE) THEN
               endline[inc] := TRUE
            ELSE
               endline[inc] := FALSE;
            IF endline[inc] = FALSE THEN
               PlotLine(Xold[inc], Yold[inc], Xnew[inc], Ynew[inc]);
            Xold[inc] := Xnew[inc];
            Yold[inc] := Ynew[inc];
         END;
         IF Xold[2] > Xold[1] THEN
         BEGIN
            endline[1] := TRUE;
            endline[2] := TRUE;
         END;
         IF Xold[3] > Xold[4] THEN
         BEGIN
            endline[3] := TRUE;
            endline[4] := TRUE;
         END;
      UNTIL endline[1] AND endline[2] AND endline[3] AND endline[4];
   END; {end if}
END;

PROCEDURE DrawDipoleCircles;
VAR
   inc : integer; {incremental}
   CircleX,CircleY : integer;
BEGIN
   SetFillStyle(0,BackgroundColor);
   FOR inc := 1 to MaxAtoms DO
   BEGIN
      IF DipoleOn[inc] THEN
      BEGIN
         SetColor(DipoleColor);
         IF DipoleSelected[inc] THEN
            SetColor(SelectedDipoleColor);
         CircleX := Round(207 + 8.28 * DipolePosX[inc]);
         CircleY := Round(204 - 8.16 * DipolePosY[inc]);
         FillEllipse(CircleX,CircleY,20,20);
      END;  {End If}
   END;  {End For}
END;

PROCEDURE DrawDipoleArrows;
VAR
   inc : integer; {incremental}
   ArrowX1, ArrowY1, ArrowX2, ArrowY2: real;
BEGIN
   SetColor(15);
   FOR inc := 1 to MaxAtoms DO
   BEGIN
      IF DipoleOn[inc] THEN
      BEGIN
         ArrowX1 := DipolePosX[inc] - DipoleMomentX[inc]/200;
         ArrowX2 := DipolePosX[inc] + DipoleMomentX[inc]/200;
         ArrowY1 := DipolePosY[inc] - DipoleMomentY[inc]/200;
         ArrowY2 := DipolePosY[inc] + DipoleMomentY[inc]/200;
         DrawArrow(ArrowX1,ArrowY1,ArrowX2,ArrowY2);
      END; {End If}
   END;  {End For}
END;

FUNCTION inttochar(number : integer) : char;
BEGIN
   CASE number OF
      1:  inttochar := 'a';
      2:  inttochar := 'b';
      3:  inttochar := 'c';
      4:  inttochar := 'd';
      5:  inttochar := 'e';
      6:  inttochar := 'f';
      7:  inttochar := 'g';
      8:  inttochar := 'h';
      9:  inttochar := 'i';
      10: inttochar := 'j';
      11: inttochar := 'k';
      12: inttochar := 'l';
      13: inttochar := 'm';
      14: inttochar := 'n';
      15: inttochar := 'o';
      16: inttochar := 'p';
      17: inttochar := 'q';
      18: inttochar := 'r';
      19: inttochar := 's';
      20: inttochar := 't';
      21: inttochar := 'u';
      22: inttochar := 'v';
      23: inttochar := 'w';
      24: inttochar := 'x';
      25: inttochar := 'y';
   END; {End Case}
END;

PROCEDURE DrawDipoleLabels;
VAR
   inc : integer; {incremental}
   numchar : char;
BEGIN
   FOR inc := 1 to MaxAtoms DO
   BEGIN
      IF DipoleOn[inc] THEN
      BEGIN
         numchar := inttochar(inc);
         PlotSymbol(DipolePosX[inc],DipolePosY[inc],numchar);
      END;  {End If}
   END;  {End For}
END;

FUNCTION ArcSin(x : real): real;
VAR
   y : real;
BEGIN
   y := sqrt(1 - x*x);
   IF y <> 0 THEN
      ArcSin := ArcTan(x/y)
   ELSE IF y = 0 THEN
      ArcSin := 0;
END;

FUNCTION CountAtoms : integer;
VAR
   inc, counter : integer;
BEGIN
   counter := 0;
   FOR inc := 1 to MaxAtoms DO
      IF DipoleOn[inc] THEN
          counter := counter + 1;
   CountAtoms := counter;
END;

PROCEDURE GetNewSelectedDipole;
{This is the only procedure where SelectedDipole can be changed}
VAR
   inc : integer;
   inside, valid : boolean;
   mouseX, mouseY : real;
   OldNum : integer;

BEGIN
   OldNum := SelectedDipole;
   IF CountAtoms > 1 THEN
   BEGIN
      SelectedDipole := 0;
      valid := FALSE;
      HideCursor;
      DrawDipoleCircles;
      DrawDipoleLabels;
      Beep;
      REPEAT
         BottomMessage('Press the letter of the Atom to be probed or click on it to select.');
         ShowCursor;
         ClearMKBuffers;
         REPEAT
            CheckForEvents;
         UNTIL (Event.MouseClicked) OR (Event.KeyPressed);
         IF Event.MouseClicked THEN
         BEGIN
            MousePosn(mouseX, mouseY, 1, 1, inside);
            IF inside THEN
            BEGIN
              valid := TRUE;
              SelectedDipole := InDipole(mouseX,mouseY,2.5);
              IF SelectedDipole = 0 THEN
              BEGIN
               Announce('Please click inside an atom.');
               valid := FALSE;
              END;
            END
            ELSE Announce('Please click on an atom in the main viewport.');
         END
         ELSE IF Event.KeyPressed THEN
         BEGIN
           CASE Event.ReadKey OF
            'a': SelectedDipole := 1;
            'b': SelectedDipole := 2;
            'c': SelectedDipole := 3;
            'd': SelectedDipole := 4;
            'e': SelectedDipole := 5;
            'f': SelectedDipole := 6;
            'g': SelectedDipole := 7;
            'h': SelectedDipole := 8;
            'i': SelectedDipole := 9;
            'j': SelectedDipole :=10;
            'k': SelectedDipole :=11;
            'l': SelectedDipole :=12;
            'm': SelectedDipole :=13;
            'n': SelectedDipole :=14;
            'o': SelectedDipole :=15;
            'p': SelectedDipole :=16;
            'q': SelectedDipole :=17;
            'r': SelectedDipole :=18;
            's': SelectedDipole :=19;
            't': SelectedDipole :=20;
            'u': SelectedDipole :=21;
            'v': SelectedDipole :=22;
            'w': SelectedDipole :=23;
            'x': SelectedDipole :=24;
            'y': SelectedDipole :=25;
           END;                        {End CASE}
           If SelectedDipole = 0 Then       {Idiot user inputted an illegal character.}
            Announce('Please input a lower case character a - y.')
           ELSE IF (not DipoleOn[SelectedDipole]) Then
           BEGIN
            Announce('That letter does not represent an existing atom.  Please try again.');
            SelectedDipole := 0;
           END
           ELSE
            valid := TRUE;
         END;
      UNTIL valid;
      GraphBackColor := Black;;
      OpenViewPort(4);
      IF SelectedDipole = 0 THEN
         SelectedDipole := OldNum;
   END
   ELSE
   BEGIN
      FOR inc := 1 to MaxAtoms DO
         IF DipoleOn[inc] THEN
            SelectedDipole := inc;
   END;
END;

PROCEDURE CenterDipoleLine;
VAR
   X1, X2, Y1, Y2 : real;
   magnitude, scale : real;

BEGIN
   scale := 0.001;
   X1 := DipolePosX[SelectedDipole];
   Y1 := DipolePosY[SelectedDipole];
   X2 := X1;
   Y2 := Y1;
   WHILE InWindow(X1,Y1) DO
   BEGIN
      X1 := X1 + scale * DipoleMomentX[SelectedDipole];
      Y1 := Y1 + scale * DipoleMomentY[SelectedDipole];
   END;
   WHILE InWindow(X2,Y2) DO
   BEGIN
      X2 := X2 - scale * DipoleMomentX[SelectedDipole];
      Y2 := Y2 - scale * DipoleMomentY[SelectedDipole];
   END;
   PlotLine(X1,Y1,X2,Y2);
END;

PROCEDURE SingleDipoleField;
{calculates the density of field lines and their respective angles from
the selected Atom, then calls SingleDipoleFieldLine to plot each line.}
VAR
   scale, arg, theta, newtheta : real;
   Xinit, Yinit : real;

BEGIN
   CenterDipoleLine;
   scale := 50/(PolSliderValue * DipoleField + 50);
   arg := scale;
   WHILE (arg < 1) DO
   BEGIN
      newtheta := ArcSin(arg);
      theta := newtheta;
      arg := sin(theta) + scale;
      SingleDipoleFieldLines(theta);
   END; {End While}
END;

PROCEDURE DrawUnsArrow(X1, Y1, X2, Y2 : Real);
          {this procedure draws an arrow from (X1, Y1) to (X2,Y2)
           with an arrow head size that does not change.}
VAR
   deltaX, deltaY, scale : real;
   newX, newY : real;
BEGIN
   PlotLine(X1, Y1, X2, Y2);   {draw the shaft of the arrow}
   deltaX := X2 - X1;
   deltaY := Y2 - Y1;
   scale := 2/sqrt(deltaX*deltaX + deltaY*deltaY);
   newX := X2 - scale*(0.4 * deltaX - 0.2 * deltaY);
   newY := Y2 - scale*(0.2 * deltaX + 0.4 * deltaY);
   PlotLine(newX,newY,X2,Y2);              {left half of arrowhead}
   newX := X2 - scale*(0.4 * deltaX + 0.2 * deltaY);
   newY := Y2 - scale*(-0.2 * deltaX + 0.4 * deltaY);
   PlotLine(newX,newY,X2,Y2);             {right half of arrowhead}
END;

PROCEDURE CombinedFieldLine(Xinit, Yinit : real);

VAR
   newX, newY         : real;
   currentX, currentY : real;
   EfieldX, EfieldY   : real;  {used to calculate the next point}
   scale, Emagnitude  : real;
   inc                : integer; {incremental}
   DipoleNum          : integer;
   ArrowHeadDrawn     : Boolean;

BEGIN
   scale          := 0.1;
   inc            := 1;
   currentX       := Xinit;
   currentY       := Yinit;
   ArrowHeadDrawn := FALSE;
   WHILE (InWindow(currentX,currentY) = FALSE) AND (currentX < 25) DO
   BEGIN
      Etotal(EfieldX,EfieldY,currentX,currentY);
      Emagnitude := EfieldX*EfieldX + EfieldY*EfieldY;
      Emagnitude := sqrt(Emagnitude);
      currentX := currentX + scale * EfieldX/Emagnitude;
      currentY := currentY + scale * EfieldY/Emagnitude;
   END;
   WHILE currentX < 25 DO
   BEGIN
      inc := inc + 1;
      Etotal(EfieldX,EfieldY,currentX,currentY);
      Emagnitude := EfieldX*EfieldX + EfieldY*EfieldY;
      Emagnitude := sqrt(Emagnitude);
      newX := currentX + scale * EfieldX/Emagnitude;
      newY := currentY + scale * EfieldY/Emagnitude;
      PlotLine(currentX,currentY,newX,newY);
      IF (currentX > -21) AND NOT ArrowHeadDrawn THEN
      BEGIN
         DrawUnsArrow(currentX, currentY, newX, newY);
         ArrowHeadDrawn := TRUE;
      END;
      currentX := newX;
      currentY := newY;
      DipoleNum := InDipole(currentX,currentY,2.0);
      IF DipoleNum <> 0 THEN
      BEGIN
         ReflectThruDipoleHorizontal(DipoleNum,currentX,currentY,newX,newY);
         IF newX > currentX THEN
         BEGIN
            currentX := newX;
            currentY := newY
         END;
      END;
      IF inc = 400 THEN  {something is wrong - it shouldn't take this long}
      BEGIN               {the field line is probably stuck in an infinite}                                             
         currentX := currentX + 0.4;         {field loop - give it a nudge}
      END;
      IF inc = 700 THEN
      BEGIN               {something is still wrong - end the line}
         CurrentX := 25;
      END;
   END;                   
END;

PROCEDURE CombinedDipoleField;
{plots the combined field of the Atoms and the external field}
VAR
   step, Yinit : real; {distance between field lines, starting Y value}
   keyNum      : Byte;

BEGIN
   BottomMessage('Drawing field lines -- F4 to interrupt.');
   step := 25/DipoleField; {same distance between lines as for external}
   CombinedFieldLine(-27,0);        {field line starting at Y = 0}
   Yinit := 0;
   WHILE Yinit < 25 DO
   BEGIN
      Yinit := Yinit + step;
      CombinedFieldLine(-27,Yinit); {field line from pos y point}
      Yinit := 0 - Yinit;
      CombinedFieldLine(-27,Yinit); {field line from neg y point}
      Yinit := 0 - Yinit;
      CheckForEvents;
      IF HotKeys.pressed(keyNum) THEN   {Check for interrupt}
      BEGIN
         IF keyNum = 3 THEN
            Yinit := 26;
      END;
   END;
   BottomMessage(' ');
END;

PROCEDURE GetNewDipoleMoments;
VAR
   inciter, inc           : integer;
   Emax, EtotalX, EtotalY : real;
   change                 : real;  {to check for convergence}
   NewDiMmntX, NewDiMmntY : DipoleVal;  {array to store new Moments}
   keyNum                 : Byte;
   ConvTest               : Boolean;

BEGIN
   Emax := 100;
   HideCursor;
   FOR inc := 1 TO MaxAtoms DO
   BEGIN
      NewDiMmntX[inc] := 0;
      NewDiMmntY[inc] := 0;
   END;
   BottomMessage('Solving for polarizations self-consistently--F2 to stop');
   OpenViewport(1);
   ConvTest := FALSE;
   FOR inciter := 1 TO 25 DO                  {iteration loop}
   BEGIN
      FOR inc := 1 TO MaxAtoms DO
      BEGIN
         Change := 0;
         IF DipoleOn[inc] THEN
         BEGIN
            ETotal(EtotalX,EtotalY,DipolePosX[inc],DipolePosY[inc]);
            NewDiMmntX[inc] := Emax * Polarizability[inc] * Tanh(EtotalX/Emax);
            NewDiMmntY[inc] := Emax * Polarizability[inc] * Tanh(EtotalY/Emax);
            Change := Change + Abs(DipoleMomentX[inc] - NewDiMmntX[inc]);
            Change := Change + Abs(DipoleMomentY[inc] - NewDiMmntY[inc]);
         END;
      END;  {End FOR}
      CheckForEvents;
      IF HotKeys.pressed(keyNum) THEN      {Check for interrupt}
      BEGIN
         IF keyNum = 2 THEN
            inciter := 25;
      END;
      IF (Change < 0.01) AND (inciter > 2) THEN   {Convergence test}
      BEGIN
         Delay(500);
         BottomMessage('Converged');
         Delay(500);
         inciter := 25;
         ConvTest:= TRUE;
      END;
      DipoleMomentX := NewDiMmntX;
      DipoleMomentY := NewDiMmntY;
      DrawDipoleCircles;
      DrawDipoleArrows;
   END;  {End FOR}
   ShowCursor;
   IF (NOT ConvTest) THEN
   BEGIN
      Beep;
      BottomMessage('Iteration did not converge; F2 for another 25 steps');
      Delay (1000);
   END;
END;

PROCEDURE DrawNumbersAndLabels;
BEGIN
   PlotSymbol(24,-1,'X');
   PlotSymbol(-1,24,'Y');
   OutTextXY(0,395,'-25');
   OutTextXY(33,391,'-20');
   OutTextXY(74,391,'-15');
   OutTextXY(116,391,'-10');
   OutTextXY(160,391,'-5');
   OutTextXY(210,391,'0');
   OutTextXY(246,391,'5');
   OutTextXY(286,391,'10');
   OutTextXY(327,391,'15');
   OutTextXY(368,391,'20');
   OutTextXY(400,391,'25');
   OutTextXY(1,357,'-20');
   OutTextXY(1,317,'-15');
   OutTextXY(1,276,'-10');
   OutTextXY(2,235,'-5');
   OutTextXY(5,195,'0');
   OutTextXY(5,155,'5');
   OutTextXY(2,114,'10');
   OutTextXY(2,73,'15');
   OutTextXY(2,33,'20');
   OutTextXY(2,4,'25');
END;

PROCEDURE DisplayDipoleField;
BEGIN
   GraphBackColor := Black;;
   OpenViewport(1);
   IF CountAtoms = 0 Then
      DipoleFieldMode := 1;
   IF (DipoleFieldMode = 2) AND (SelectedDipole = 0) THEN
   BEGIN
      GetNewSelectedDipole;
      GraphBackColor := Black;;
      OpenViewport(1);
   END;
   HideCursor;
   SetColor(14);
   IF DipoleField > 0 THEN  {don't display field if field strength = 0}
      CASE DipoleFieldMode OF
         1: ExternalField;
         2: SingleDipoleField;
         3: CombinedDipoleField;
      END; {End Case}
   SetColor(15);
   DrawNumbersAndLabels;
   Axis(0,0,5,5);
   BottomMessage(' ');
   DrawDipoleCircles;
   IF ViewArrows THEN
      DrawDipoleArrows
   ELSE DrawDipoleLabels;
   WindowOutline;
   ShowCursor;
END;

PROCEDURE PlaceDipolesKeyboard;
{displays the input screen, letting the user change the positions of
the Atoms and add/remove them by clicking the appropriate fields.
The global variables for the dipoles are then adjusted accordingly.}
VAR
   inc, CheckOverlap : integer; {incremental, check for Atom overlap}
   Overlap : Boolean;
   NewX, NewY : DipoleVal;
   NewOn : DipoleBool;
   BoolChar : char;   {'T' or 'F' - used to assign new value to screen field}
   DipoleInputScreenA : TInputScreen;
   DipoleInputScreenB : TInputScreen;

BEGIN
 WITH DipoleInputScreenA DO
 BEGIN
   Init;
   DefineInputPort(0.2,0.8,0.05,0.9);
   Overlap := FALSE;
   LoadLine('                    X          Y      On/Off');
   LoadLine('     Atom 1   {  0.0}    {  0.0}      #T ');
   LoadLine('     Atom 2   {  0.0}    {  0.0}      #F ');
   LoadLine('     Atom 3   {  0.0}    {  0.0}      #F ');
   LoadLine('     Atom 4   {  0.0}    {  0.0}      #F ');
   LoadLine('     Atom 5   {  0.0}    {  0.0}      #F ');
   LoadLine('     Atom 6   {  0.0}    {  0.0}      #F ');
   LoadLine('     Atom 7   {  0.0}    {  0.0}      #F ');
   LoadLine('     Atom 8   {  0.0}    {  0.0}      #F ');
   LoadLine('     Atom 9   {  0.0}    {  0.0}      #F ');
   LoadLine('     Atom 10  {  0.0}    {  0.0}      #F ');
   LoadLine(' ');
   LoadLine('Screen ranges from -25 to +25 Ang. in x and y.');
   LoadLine('Atoms have a diameter of 5 A and may not    ');
   LoadLine('overlap.  To change a value, use the mouse,   ');
   LoadLine('tab or arrow keys to select the field and the ');
   LoadLine('keyboard to modify it.  The SPACE bar toggles ');
   LoadLine('the check boxes.  The next screen inputs      ');
   LoadLine('Atoms 11 - 25 if necessary.  ');
   LoadLine('           [  Ok  ]     [Cancel]           ');
   For inc:= 1 to 10 DO        {enter current values into fields}
   BEGIN
      IF DipoleOn[inc] = TRUE THEN
      BEGIN
         SetBoolean(3*inc,TRUE);
         SetNumber(3*inc-2,DipolePosX[inc]);
         SetNumber(3*inc-1,DipolePosY[inc]);
      END
      ELSE
      BEGIN
         SetBoolean(3*inc,FALSE);
         SetNumber(3*inc-1,0);
         SetNumber(3*inc-2,0);
      END;                             {End IF}
   END;                                {End FOR}   
   REPEAT
   AcceptScreen;                       {let user change values}
   IF Canceled = FALSE THEN
   BEGIN                          
      FOR inc := 1 to 10 DO    {read new values from fields}
      BEGIN
         NewX[inc] := GetNumber(3*inc-2);
         NewY[inc] := GetNumber(3*inc-1);
         NewOn[inc] := GetBoolean(3*inc);
      END;
      FOR inc := 11 to MaxAtoms DO
         NewOn[inc] := FALSE;
      Overlap := FALSE;  {now check to see if there are any overlaps}
      FOR inc := 1 to 9 DO
      BEGIN
         CheckOverlap := InDipole(NewX[inc],NewY[inc],5);
         IF (CheckOverlap <> 0) AND (NewOn[CheckOverlap] = TRUE) AND
               (NewOn[inc] = TRUE) AND (inc <> CheckOverlap) THEN
            Overlap := TRUE;
      END;                             {End For}
      IF Overlap THEN
      BEGIN
         beep;
         Announce(' The Atoms cannot overlap each other - please reinput');
      END;                             {End IF overlap}
   END;                                {End IF not Canceled}
   UNTIL Overlap = FALSE;
   Done;
 END;                                  {End DO WITH Screen A}
 IF NewOn[10] THEN
 BEGIN
 WITH DipoleInputScreenB DO
 BEGIN
     Init;
     DefineInputPort(0.2,0.8,0.05,0.9);
     Overlap := FALSE;
     LoadLine('                    X          Y      On/Off');
     LoadLine('    Atom 11   {  0.0}    {  0.0}      #T ');
     LoadLine('    Atom 12   {  0.0}    {  0.0}      #F ');
     LoadLine('    Atom 13   {  0.0}    {  0.0}      #F ');
     LoadLine('    Atom 14   {  0.0}    {  0.0}      #F ');
     LoadLine('    Atom 15   {  0.0}    {  0.0}      #F ');
     LoadLine('    Atom 16   {  0.0}    {  0.0}      #F ');
     LoadLine('    Atom 17   {  0.0}    {  0.0}      #F ');
     LoadLine('    Atom 18   {  0.0}    {  0.0}      #F ');
     LoadLine('    Atom 19   {  0.0}    {  0.0}      #F ');
     LoadLine('    Atom 20   {  0.0}    {  0.0}      #F ');
     LoadLine('    Atom 21   {  0.0}    {  0.0}      #T ');
     LoadLine('    Atom 22   {  0.0}    {  0.0}      #F ');
     LoadLine('    Atom 23   {  0.0}    {  0.0}      #F ');
     LoadLine('    Atom 24   {  0.0}    {  0.0}      #F ');
     LoadLine('    Atom 25   {  0.0}    {  0.0}      #F ');
     LoadLine(' ');
     LoadLine('           [  Ok  ]     [Cancel]           ');
     For inc:= 11 to MaxAtoms DO        {enter current values into fields}
      BEGIN
      IF DipoleOn[inc] = TRUE THEN
      BEGIN
         SetBoolean(3*(inc-10),TRUE);
         SetNumber(3*(inc-10)-2,DipolePosX[inc]);
         SetNumber(3*(inc-10)-1,DipolePosY[inc]);
      END
      ELSE
      BEGIN
         SetBoolean(3*(inc-10),FALSE);
         SetNumber(3*(inc-10)-1,0);
         SetNumber(3*(inc-10)-2,0);
      END;                             {End IF}
   END;                                {End FOR}   
   REPEAT
   AcceptScreen;                       {let user change values}
   IF Canceled = FALSE THEN
   BEGIN                          
      FOR inc := 11 to MaxAtoms DO    {read new values from fields}
      BEGIN
         NewX[inc] := GetNumber(3*(inc-10)-2);
         NewY[inc] := GetNumber(3*(inc-10)-1);
         NewOn[inc] := GetBoolean(3*(inc-10));
      END;
      Overlap := FALSE;  {now check to see if there are any overlaps}
      FOR inc := 11 to MaxAtoms DO
      BEGIN
         CheckOverlap := InDipole(NewX[inc],NewY[inc],5);
         IF (CheckOverlap <> 0) AND (NewOn[CheckOverlap] = TRUE) AND
               (NewOn[inc] = TRUE) AND (inc <> CheckOverlap) THEN
            Overlap := TRUE;
      END;                             {End For}
      IF Overlap THEN
      BEGIN
         beep;
         Announce(' The Atoms cannot overlap each other - please input again or cancel');
      END;                             {End IF overlap}
   END;                                {End IF not Canceled}
   UNTIL Overlap = FALSE;
   DefineInputPort(0,1,0,1);
   Done;
 END;                                  {End DO WITH Screen B}
 END;                                  {End IF NewOn[10]}
   FOR inc := 1 TO MaxAtoms DO
   BEGIN
      IF NewOn[inc] AND (DipoleOn[inc] = FALSE) THEN
      BEGIN
         Polarizability[inc] := PolSliderValue;
         DipoleMomentX[inc] := DipoleField*PolSliderValue;
         DipoleSelected[inc] := TRUE;
         SelectedDipole := inc;
      END;
   END;
   DipolePosX := NewX;
   DipolePosY := NewY;
   DipoleOn := NewOn;
   OpenViewPort(4);
   ShowCursor;
 SelectedDipole := 0;
END;

PROCEDURE FieldProbe;
VAR
   MouseX, MouseY : real;
   EfieldX, EfieldY : real;
   Emag, Edir : real;
   inside : boolean;
   EndFieldProbe : boolean;
   arrowX, arrowY : real;
   PrStr : String;
BEGIN
   EndFieldProbe := FALSE;
   BottomMessage(' Click on any point to get field info, hit any key to stop.');
   REPEAT
      ClearMKBuffers;
      REPEAT
         CheckForEvents;
      UNTIL Event.KeyPressed OR Event.MouseClicked;
      IF Event.MouseClicked THEN
      BEGIN
         MousePosn(mouseX,mouseY,1,1,inside);
         IF inside THEN
         BEGIN
            Etotal(EfieldX,EfieldY,mouseX,mouseY);
            Emag := EfieldX*EfieldX + EfieldY*EfieldY;
            Emag := sqrt(Emag);
            Edir := 180*ArcTan(EfieldY/EfieldX)/PI;
            IF EfieldX < 0 THEN
            BEGIN
               IF EfieldY > 0 THEN
                  Edir := Edir + 180
               ELSE
                  Edir := Edir - 180;
            END;
            arrowX := mouseX + EfieldX/5;
            arrowY := mouseY + EfieldY/5;
            SetColor(9);
            SelectViewPort(1);
            DrawArrow(mouseX,mouseY,arrowX,arrowY);
            TopMessage(' Field Probe Information: ');
            GraphBackColor := DarkGray;;;
            OpenViewPort(2);
            SetColor(TextColor);
            Print(2,1,'Field Probe:');
            Print(2,3,'Position:');
            Print(2,4,'X = ');
            PrStr := NumStr(MouseX,3,1);
            Print(7,4,PrStr);
            Print(13, 4,'Y = ');
            PrStr := NumStr(MouseY,3,1);
            Print(20, 4, PrStr);
            Print(2, 6,'Electric field: ');
            Print(2, 7,'Ex =');
            PrStr := NumStr(EfieldX,3,1);
            Print(7, 7, PrStr);
            Print(13, 7,'Ey =');
            PrStr := NumStr(EfieldY,3,1);
            Print(20, 7, PrStr);
            Print(2, 8, 'magnitude =');
            PrStr := NumStr(Emag,3,1);
            Print(14, 8,PrStr);
            Print(2, 9, 'direction =');
            PrStr := NumStr(Edir, 3, 0);
            Print(14, 9, PrStr);
            Print(19, 9,'degrees');
         END
         ELSE {click was not inside viewport}
            EndFieldProbe := TRUE;
      END
      ELSE {key was pressed instead of mouse clicked}
         EndFieldProbe := TRUE;
   UNTIL EndFieldProbe;
   GraphBackColor := DarkGray;;;
   OpenViewPort(4);
END;

PROCEDURE ClearAtoms;                     {Clears all atoms from screen}
                                          { and sets all positions to 0}
VAR
   inc    : Integer;   {incremental}

BEGIN
   FOR inc := 1 TO MaxAtoms DO
   BEGIN
      DipoleSelected[inc] := FALSE;
      DipoleOn[inc] := FALSE;
      DipoleMomentX[inc] := 0;
      DipoleMomentY[inc] := 0;
      DipolePosX[inc] := 0;
      DipolePosY[inc] := 0;
   END;   {End FOR}
END;

PROCEDURE HCP;                   {Fills a close-packed lattice}
VAR
   inc, incX        : integer; {incremental}
BEGIN
   inc := 0;
   FOR incX := 1 TO 5 DO
   BEGIN
      inc := inc + 1;
      DipoleSelected[inc] := TRUE;
      DipoleOn[inc] := TRUE;
      Polarizability[inc] := PolSliderValue;
      DipolePosX[inc] := (incX - 3)*5.1;
      DipoleMomentX[inc] := Polarizability[inc]*DipoleField;
      DipoleMomentY[inc] := 0;
   END;
   FOR incX := 1 TO 4 DO
   BEGIN
      inc := inc + 1;
      DipoleSelected[inc] := TRUE;
      DipoleOn[inc] := TRUE;
      Polarizability[inc] := PolSliderValue;
      DipolePosX[inc] := (incX - 2.5)*5.1;
      DipolePosY[inc] := 4.331;
      DipoleMomentX[inc] := Polarizability[inc]*DipoleField;
      DipoleMomentY[inc] := 0;
      inc := inc + 1;
      DipoleSelected[inc] := TRUE;
      DipoleOn[inc] := TRUE;
      Polarizability[inc] := PolSliderValue;
      DipolePosX[inc] := (incX - 2.5)*5.1;
      DipolePosY[inc] := -4.331;
      DipoleMomentX[inc] := Polarizability[inc]*DipoleField;
      DipoleMomentY[inc] := 0;
   END;
   FOR incX := 1 TO 3 DO
   BEGIN
      inc := inc + 1;
      DipoleSelected[inc] := TRUE;
      DipoleOn[inc] := TRUE;
      Polarizability[inc] := PolSliderValue;
      DipolePosX[inc] := (incX - 2)*5.1;
      DipolePosY[inc] := 8.661;
      DipoleMomentX[inc] := Polarizability[inc]*DipoleField;
      DipoleMomentY[inc] := 0;
      inc := inc + 1;
      DipoleSelected[inc] := TRUE;
      DipoleOn[inc] := TRUE;
      Polarizability[inc] := PolSliderValue;
      DipolePosX[inc] := (incX - 2)*5.1;
      DipolePosY[inc] := -8.661;
      DipoleMomentX[inc] := Polarizability[inc]*DipoleField;
      DipoleMomentY[inc] := 0;
   END;
END;

PROCEDURE AtomConfiguration(num : integer);
VAR
   inc        : integer; {incremental}
   incX, incY : integer; {incremental}

BEGIN
   ClearAtoms;
   CASE num OF
      1: BEGIN
            DipoleSelected[1] := TRUE;
            DipoleOn[1] := TRUE;
            Polarizability[1] := PolSliderValue;
            DipoleMomentX[1] := Polarizability[1]*DipoleField;
            DipoleMomentY[1] := 0;
         END;
      2: BEGIN                             {horizontal line}
            FOR inc := 1 TO 10 DO
            BEGIN
               DipoleSelected[inc] := TRUE;
               DipoleOn[inc] := TRUE;
               Polarizability[inc] := PolSliderValue;
               DipolePosX[inc] := (inc - 6)*5 + 2.5;
               DipoleMomentX[inc] := Polarizability[inc]*DipoleField;
               DipoleMomentY[inc] := 0;
            END;          {End FOR}
         END;
      3: BEGIN                             {vertical line}
            FOR inc := 1 TO 11 DO
            BEGIN
               DipoleSelected[inc] := TRUE;
               DipoleOn[inc] := TRUE;
               Polarizability[inc] := PolSliderValue;
               DipolePosY[inc] := (inc - 6)*5;
               DipoleMomentX[inc] := Polarizability[inc]*DipoleField;
               DipoleMomentY[inc] := 0;
            END;          {End FOR}
         END;
      4: BEGIN                             {diagonal line}
            FOR inc := 1 TO 11 DO
            BEGIN
               DipoleSelected[inc] := TRUE;
               DipoleOn[inc] := TRUE;
               Polarizability[inc] := PolSliderValue;
               DipolePosX[inc] := (inc - 6)*3.57;
               DipolePosY[inc] := (inc - 6)*3.57;
               DipoleMomentX[inc] := Polarizability[inc]*DipoleField;
               DipoleMomentY[inc] := 0;
            END;          {End FOR}
         END;
      5: BEGIN                            {square lattice}
            inc := 0;
            FOR incX := 1 TO 5 DO
               FOR incY := 1 TO 5 DO
               BEGIN
                  inc := inc + 1;
                  DipoleSelected[inc] := TRUE;
                  DipoleOn[inc] := TRUE;
                  Polarizability[inc] := PolSliderValue;
                  DipolePosX[inc] := (incX - 3)*5.1;
                  DipolePosY[inc] := (incY - 3)*5.1;
                  DipoleMomentX[inc] := Polarizability[inc]*DipoleField;
                  DipoleMomentY[inc] := 0;
               END;          {End FOR}
         END;
      6: BEGIN                            {rectangular lattice}
            inc := 0;
            FOR incX := 1 TO 6 DO
               FOR incY := 1 TO 4 DO
               BEGIN
                  inc := inc + 1;
                  DipoleSelected[inc] := TRUE;
                  DipoleOn[inc] := TRUE;
                  Polarizability[inc] := PolSliderValue;
                  DipolePosX[inc] := (incX - 3.5)*5.1;
                  DipolePosY[inc] := (incY - 2.5)*7.1;
                  DipoleMomentX[inc] := Polarizability[inc]*DipoleField;
                  DipoleMomentY[inc] := 0;
               END;          {End FOR}
         END;
      7: HCP;                            {Close-packed lattice}
      8: BEGIN                           {Close-packed alloy}
            HCP;
            DipoleSelected[1] := FALSE;
            DipoleSelected[4] := FALSE;
            DipoleSelected[8] := FALSE;
            DipoleSelected[9] := FALSE;
            DipoleSelected[18] := FALSE;
            DipoleSelected[19] := FALSE;
         END;
      9: ClearAtoms;
   END; {End Case}
END;

PROCEDURE UpdateScreen;

BEGIN
   ClearAll;
   DisplayScreen;
   DipoleMenu.display;
   DisplayAtom;
   DisplayDipoleField;
END;

PROCEDURE DipoleSliderHandler;
VAR
   inc      : Integer;

BEGIN
   WHILE sliders.changed DO
      CheckForEvents;                 {Wait until slider stops moving;}
   DipoleField := sliders.value(1);   {then get the new values}
   PolSliderValue := sliders.value(2);
   FOR inc := 1 TO MaxAtoms DO        {Update polarizabilities of selected}
   BEGIN                              { atoms.}
      IF DipoleSelected[inc] THEN
      BEGIN
         Polarizability[inc] := PolSliderValue;
         DipoleMomentX[inc] := DipoleField*PolSliderValue;
         DipoleMomentY[inc] := 0;
      END;
   END;
END;

PROCEDURE DipoleProbe;
VAR
   Pmag, Pdir, PX, PY  : Real;      {dipole moment}

BEGIN
  IF CountAtoms <> 0 THEN
  BEGIN
   IF SelectedDipole = 0 THEN
      GetNewSelectedDipole;
   PX := DipoleMomentX[SelectedDipole];
   PY := DipoleMomentY[SelectedDipole];
   Pmag := sqrt(PX*PX + PY*PY);
   Pdir := 180*ArcTan(PY/PX)/PI;
   IF PX < 0 THEN
   BEGIN
      IF PY > 0 THEN
         Pdir := Pdir + 180
      ELSE
         Pdir := Pdir - 180;
   END;
   GraphBackColor := DarkGray;
   OpenViewPort(2);
   SetColor(SelectedDipoleColor);
   PlotSymbol(16, 22.5, IntToChar(SelectedDipole));
   SetColor(TextColor);
   Print(2,  1, 'Probe of Atom ');
   Print(2,  3, 'Polarizability = ');
   Print(18, 3, NumStr(Polarizability[SelectedDipole], 5, 1));
   Print(2,  5, 'Position:');
   Print(2,  6, 'x = ');
   Print(7,  6, NumStr(DipolePosX[SelectedDipole], 5, 1));
   Print(13, 6, '; y = ');
   Print(20, 6, NumStr(DipolePosY[SelectedDipole], 5, 1));
   Print(2,  8, 'Dipole Moment:');
   Print(2,  9, 'Px= ');
   Print(7,  9, NumStr(DipoleMomentX[SelectedDipole], 5, 1));
   Print(13, 9, '; Py= ');
   Print(20, 9, NumStr(DipoleMomentY[SelectedDipole], 5, 1));
   Print(1,  10,' magnitude= ');
   Print(14, 10, NumStr(Pmag,3,1));
   Print(1,  11,' direction= ');
   Print(14, 11, NumStr(Pdir,3,0));
   Print(19, 11,'degrees');
  END
  ELSE
  BEGIN
   Beep;
   Announce('There are no atoms to look at.');
  END;
END;

PROCEDURE DipoleOptionsHandler(rowchosen : integer);
BEGIN
   CASE rowchosen OF    
      1: BEGIN
            ViewNucleus := TRUE;
            DipoleMenu.RowChecked(3,1,TRUE);
            DipoleMenu.RowChecked(3,2,FALSE);
            DisplayAtom;
         END;
      2: BEGIN
            ViewNucleus := FALSE;
            DipoleMenu.RowChecked(3,2,TRUE);
            DipoleMenu.RowChecked(3,1,FALSE);
            DisplayAtom;
         END;
      3: BEGIN
            PlaceDipolesKeyboard;
            DisplayDipoleField;
         END;
      4: FieldProbe;
      5: DipoleProbe;
      6: BEGIN
            {HideCursor;}
            UpdateScreen;
            {ShowCursor;}
         END;
   END; {End Case}
END;

PROCEDURE DipoleMenuHandler;
BEGIN
   CASE DipoleMenu.colchosen OF                        {The File Menu}
      1: CASE DipoleMenu.rowchosen OF
            1: AboutCUPS;
            2: Help(Credits);
            3: Configuration;
            4: QuitFlag := TRUE;
         END;                                          {End CASE}
      2: BEGIN
            IF DipoleMenu.rowchosen < 4 THEN
               DipoleFieldMode := DipoleMenu.rowchosen;
            IF DipoleMenu.rowchosen = 4 THEN
               ViewArrows := TRUE;
            IF DipoleMenu.rowchosen = 5 THEN
               ViewArrows := FALSE;
            DisplayDipoleField;
         END;                                           {End CASE}
      3: DipoleOptionsHandler(DipoleMenu.rowchosen);
      4: BEGIN                                    {Configuration choice}
            AtomConfiguration(DipoleMenu.rowchosen);
            DisplayDipoleField;
         END;
      5: BEGIN
            CASE DipoleMenu.rowchosen OF
               1 : ShowHelp('AtomPol.hlp','DisplayHelp');
               2 : ShowHelp('AtomPol.hlp','ViewHelp');
               3 : ShowHelp('AtomPol.hlp','AtomSelectionandHotKeyHelp');
               4 : ShowHelp('AtomPol.hlp','KeyPlacementHelp');
               5 : ShowHelp('AtomPol.hlp','MousePlacementHelp');
               6 : ShowHelp('AtomPol.hlp','FieldProbeHelp');
               7 : ShowHelp('AtomPol.hlp','ProbeAtomHelp');
               8 : BEGIN
                      ShowHelp('AtomPol.hlp','DipoleSlidersHelp');
                      ShowHelp('AtomPol.hlp','DipoleSlidersHelpContinued');
                   END;
            END;   {End Case}
         END;
   END; {End Case}
END;

PROCEDURE HotKeysHandler(keyNum : byte);
VAR
   inc    : Integer;   {incremental}

BEGIN
   CASE keyNum OF
      1: ShowHelp('AtomPol.hlp','DipoleHelp');
      2: BEGIN
            Beep;
            GetNewDipoleMoments;
         END;
      3: BEGIN                       {Select all 'on' dipoles}
            FOR inc := 1 TO MaxAtoms DO
            BEGIN
               IF DipoleOn[inc] THEN
                  DipoleSelected[inc] := TRUE;
            END;   {End FOR}
         END;
      4: FOR inc := 1 TO MaxAtoms DO         {Clear all selections}
             DipoleSelected[inc] := FALSE;
      5: BEGIN                               {Delete selected dipoles}
            FOR inc := 1 TO MaxAtoms DO
            BEGIN
               IF DipoleSelected[inc] THEN
               BEGIN
                  DipoleSelected[inc] := FALSE;
                  DipoleOn[inc] := FALSE;
                  DipoleMomentX[inc] := 0;
                  DipoleMomentY[inc] := 0;
                  IF SelectedDipole = inc Then
                     SelectedDipole := 0;
               END;         {End IF}
            END;   {End FOR}
         END;
      6: IF DipoleMenu.chosen THEN DipoleMenuHandler;
   END;   {End CASE}
   UpdateScreen;
END;

PROCEDURE PlaceAtom(mouseX, mouseY : Real) ;
VAR
   DipoleClicked  : Integer;
   inc: integer; {incremental}
   ChangeNum, DeleteNum: integer;

BEGIN
   SelectViewPort(1);
   DipoleClicked := InDipole(mouseX,mouseY,2.5);
   IF DipoleClicked <> 0 THEN          {remove the Atom}
   BEGIN
      DipoleOn[DipoleClicked] := FALSE;
      DipoleMomentX[DipoleClicked] := 0;
      DipoleMomentY[DipoleClicked] := 0;
      DipoleSelected[DipoleClicked] := FALSE;
      IF SelectedDipole = DipoleClicked THEN
         SelectedDipole := 0;
      {DisplayDipoleField;}
   END
   ELSE
   BEGIN                                {add a Atom here}
      ChangeNum := 0;
 {this next loop checks to see if there are any unused Atoms}
 {if so, ChangeNum gets the first number available.}
      For inc := 1 To MaxAtoms Do
      Begin
         If DipoleOn[inc] = FALSE Then
         Begin
            ChangeNum := inc;
            inc := MaxAtoms;
         End; {End IF}
      End; {End FOR}
      IF ChangeNum <> 0 THEN  {There must be an available Atom}
      BEGIN
         DipoleClicked := InDipole(mouseX,mouseY,5.0);
         IF DipoleClicked = 0 THEN    {New Atom cannot overlap}
         Begin
            DipolePosX[ChangeNum] := mouseX;
            DipolePosY[ChangeNum] := mouseY;
            DipoleOn[ChangeNum] := TRUE;
            DipoleSelected[ChangeNum] := TRUE;
            Polarizability[ChangeNum] := PolSliderValue;
            DipoleMomentX[ChangeNum] := DipoleField*PolSliderValue;
            SelectedDipole := ChangeNum;
            {DisplayDipoleField;}
         End
         Else {User has clicked too close to or on an existing atom}
         Begin
            Beep;
            Announce(' Atoms may not overlap.');
         END;
      END
      Else
      Begin {Error - max. Atoms reached.}
         Beep;
         Announce(' Maximum Number of Atoms reached')
      End;  {End Else}
   END;
END;

VAR
   inside, SingleClick : Boolean;
   DipoleNum, Dummy    : integer;
   keyNum              : byte;
   mouseX, mouseY      : Real;

BEGIN      {Main Program}
   Initialize;
   DefineSliders;
   Help(Credits);
   UpdateScreen;
   REPEAT
      CheckForEvents;
      IF sliders.changed THEN
      BEGIN
         DipoleSliderHandler;
         DisplayDipoleField;  {redisplay field using new values}
         DisplayAtom;
      END;
      If Event.MouseClicked Then
      Begin
         MousePosn(mouseX, mouseY, 1, 1, inside);
         If inside Then
         Begin
            mouseTimer.ResetTimer;
            Dummy := 0;
            Repeat
               Dummy := Dummy + 1;
            Until (mouseTimer.GetDeltaTicks(FALSE) > 0.75*DoubleClickTime);
            CheckForEvents;
            {SingleClick := Event.MouseClicked and (not Event.DoubleClicked);}
            If Event.DoubleClicked Then
                  PlaceAtom(MouseX,MouseY)      {double click - place/remove charge}
            Else
            Begin
               DipoleNum := InDipole(mouseX,mouseY, 2.5);  {check if click is on a Atom}
               If DipoleNum <> 0 Then                      {select/de-select it}
               Begin
                  DipoleSelected[DipoleNum] := NOT DipoleSelected[DipoleNum];
                  IF DipoleSelected[DipoleNum] THEN SelectedDipole := DipoleNum
                  ELSE IF SelectedDipole = DipoleNum THEN SelectedDipole := 0;
               End;
            End;
            DisplayDipoleField;
         End;
      END;
      IF HotKeys.pressed(keyNum) THEN HotKeysHandler(keyNum);
      IF DipoleMenu.activated THEN DipoleMenuHandler
   UNTIL (QuitFlag);  {QUIT}
   sliders.done;
   DipoleMenu.done;
   CUPSDone;
END.
