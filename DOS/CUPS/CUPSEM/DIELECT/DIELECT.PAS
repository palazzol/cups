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

PROGRAM Dielectric;
{
 ***************************** Dielectric Media ****************************
 *                               Version 3.7.1                             *
 *                    Concept and Design by Lyle Roelofs                   *
 *                Programming by Nat Johnson and Lyle Roelofs              *
 *                          Department of Physics                          *
 *                            Haverford College                            *
 *                           Haverford, PA  19041                          *
 *                                                                         *
 *                                July, 1994                               *
 *                                                                         *
 ***************************************************************************
 *                                                                         *
 * This program simulates the behavior of macroscopic dielectric materials.*
 *                                                                         *
 ***************************************************************************

 This program is big, but will compile normally to disk under Turbo 7.0.
  If it fails to compile under 6.0, invoke turbo with the /s switch
  and under Options/Environment/Startup turn on Use Expanded Memory
  and restart.
 Version 3.7.1 has the self field error corrected.}


USES CUPS, CUPSmupp, crt, graph, CUPSgui, CUPSfunc, CUPSgrph; {Version 3.7}

TYPE
   DielectricArray = Array[1..25] OF Array[1..25] OF real;
   DielecBoolArray = Array[1..25] OF Array[1..25] OF Boolean;

CONST
   MaxDielectricX = 25;
   MaxDielectricY = 25;
   DarkBlue = 1;
   DarkGreen = 2;
   WindowColor = 3;
   BackgroundColor = 7;                         {Light gray}
   EFieldColor = 14;                            {Yellow}
   PolarizationColor = 15;                      {White}
   DisplacementColor = 10;                      {Lightgreen}
   CellPolColor = 9;                            {Lightblue}
   TextColor = 14;                              {Yellow}

VAR
{The following group of global variables are objects such as menus,
sliders, buttons and help screens}
   DielectricMenu : Tmenu;
   sliders : Tsliders;
   buttons : Tbuttons;
   HotKeys : THotKeys;
   Credits : HelpScrType;

{These variables monitor the current program modes chosen by the user.
 The user can look at which menu selections are checked to see the
 current modes as well.}
   QuitFlag  : Boolean;   {Time to quit?}
   DielectricDisplayMode : integer;
                    {1 = Polarization, 2 = Electric field, 3 = Displacement}
   DielectricPolMode : integer;
                    {1 = Fixed polarization, 2 = Fixed susceptibility} 
{The following values show the current slider values, they are set
initially in InitializeGlobals, and are changed in GetNewSliderValues}
   DielectricExtField, susceptibility, polarization, DiScale : real;
   PolXorPolY  : integer;           {controls application of polarization
                                     slider 1 -> PolX, 0 -> PolY}
{The following arrays monitor the susceptibility, polarization,
and Electric Field of all grid points in Dielectric Mode.  The integer
variables are used to define the limits of sums on the array to only
those cells with Pol <> 0.}
   Susc             : DielectricArray;
   PolX, PolY       : DielectricArray;
   ElcX, ElcY       : DielectricArray;
   PolSumX, PolSumY : Array[0..25] OF Array[0..25] OF real;
   PolSumsRead      : Boolean;   {If TRUE polarization sums have been read in.}
   RecalcEfield     : Boolean;   {If TRUE the total electric field needs to
                                     be recalculated.}
   DielectSumMinX, DielectSumMaxX, DielectSumMinY, DielectSumMaxY : Integer;

{This array monitors which dielectric grid cells are currently
selected by the user.}
   CellSelected : DielecBoolArray;
   CloseupX, CloseupY : integer;  {cell for Closeup view}

PROCEDURE BottomMessage(message : String);
{Prints a message at the bottom of the screen.  Note that ViewPort 1 is
automatically selected afterwards, in the case that another viewport
is selected, the viewport must be reselected manually.}
BEGIN
   SetColor(WindowColor);
   OpenViewPort(4);
   SetColor(textcolor);
   Print(1,1,message);
   SelectViewPort(1);
END;

PROCEDURE TopMessage(message : String);
{Prints a message at the top right of the screen.  Note that ViewPort 2 is
automatically selected afterwards, in the case that another viewport
is selected, the viewport must be reselected manually.}
BEGIN
   SetColor(WindowColor);
   OpenViewPort(5);
   SetColor(textcolor);
   Print(1,1,message);
   SelectViewPort(2);
END;

PROCEDURE FillDielectricArray(VAR DieArr : DielectricArray;
                              X1, X2, Y1, Y2 : integer ; value : real);
VAR
   incX, incY : integer;

BEGIN
   FOR incX := X1 TO X2 DO
      FOR incY := Y1 TO Y2 DO
         DieArr[incX,incY] := value;
END;

PROCEDURE FillDielecBoolArray(VAR DieBoolArr : DielecBoolArray;
                              X1, X2, Y1, Y2 : integer ; value : boolean);
VAR
   incX, incY : integer;

BEGIN
   FOR incX := X1 TO X2 DO
      FOR incY := Y1 TO Y2 DO
         DieBoolArr[incX,incY] := value;
END;

PROCEDURE FillSelectedDielectricArray(VAR DieArr : DielectricArray; value : real);
                  {Fills array DieArr with value value in all selected cells}
                  {Other cells are not changed.}
VAR
   incX, incY : integer;

BEGIN
   FOR incX := 1 TO MaxDielectricX DO
      FOR incY := 1 TO MaxDielectricY DO
      BEGIN
         IF CellSelected[incX,incY] THEN
            DieArr[incX,incY] := value;
      END;     {End FOR Y}
END;

PROCEDURE ReadPolSums;

VAR
   wi, wj :integer;
   Lst    :text;
BEGIN
   Assign(Lst,'dielect.dat');
   Reset(Lst);
   FOR wi := 0 to MaxDielectricX DO
   BEGIN
      FOR wj := 0 to MaxDielectricY DO
         IF (wi<>0) OR (wj<>0) THEN readln(Lst,PolSumX[wi,wj],PolSumY[wi,wj]);
   END;   {End For}
   Close(Lst);
   Writeln(wi,'  ',wj);
   Writeln(PolSumx[1,1],'  ',PolSumY[1,1]);
   PolSumsRead := TRUE;
END;


PROCEDURE InitializeGlobals;
VAR
   inc : integer;  {incremental}
BEGIN
{default slider and button values for Dielectric Mode}
   QuitFlag := FALSE;
   DielectricExtField := 5;
   susceptibility := 4;
   polarization := 10;
   DiScale := 1.;
   PolXorPolY := 1;             {X polarization changed by slider}
   DielectricDisplayMode := 2;   {show electric field}
   PolSumsRead := FALSE;         {polarization sums not yet read}
   RecalcEfield := TRUE;         {Efield needs to be determined}
   DielectricPolMode := 1;       {fixed polarization mode}
   FillDielectricArray(Susc,1,MaxDielectricX,1,MaxDielectricY,0);
   FillDielectricArray(PolX,1,MaxDielectricX,1,MaxDielectricY,0);
   FillDielectricArray(PolY,1,MaxDielectricX,1,MaxDielectricY,0);
   FillDielectricArray(ElcX,1,MaxDielectricX,1,MaxDielectricY,0);
   FillDielectricArray(ElcY,1,MaxDielectricX,1,MaxDielectricY,0);
   FillDielecBoolArray(CellSelected,1,MaxDielectricX,1,MaxDielectricY,FALSE);
   CloseupX := 13;
   CloseupY := 13;
END;

PROCEDURE DefineViewports;
BEGIN
   DefineViewPort(1, 0, 0.65, 0.1, 0.95);        {Field window}
   DefineViewPort(2, 0.652, 1, 0.455, 0.91);     {UpperRight window}
   DefineViewPort(3, 0.652, 1, 0.1, 0.45);       {Sliders window}
   DefineViewPort(4, 0, 1, 0.055, 0.095);        {bottom message window}
   DefineViewPort(5, 0.652, 1, 0.91, 0.95);      {small message window}
   DefineScale(1,-25,25,-25,25);
   SelectScale(1);
END;

PROCEDURE DefineHotKeys;
BEGIN
   WITH HotKeys DO
   BEGIN
      Init(6);
      key[1] := 'F1-Help';
      key[2] := 'F2-Calculate';
      key[3] := 'F3-Break';
      key[4] := 'F4-Slct all';
      key[5] := 'F5-Clear';
      key[6] := 'F10-Menu';
   END;
END;
     
PROCEDURE DefineDielectricMenu;   {defines the dielectric menu.}
BEGIN
   WITH DielectricMenu DO
   BEGIN
      init;
      column(1, 'File');
      row(1, 1, 'About CUPS');
      row(1, 2, 'About Program');
      row(1, 3, 'System Configuration');
      row(1, 4, 'Exit Program');
      column(2, 'Display');
      row(2, 1, 'Polarization');
      row(2, 2, 'Electric Field');
      row(2, 3, 'Displacement');
      column(3, 'Options');
      row(3, 1, 'Refresh Screen');
      row(3, 2, 'Fixed Polarization');
      row(3, 3, 'Fixed Susc.(iter to convergence)');
      row(3, 4, 'Fixed Susc.(iterate 1 step)');
      column(4, 'Configs.');
      row(4, 1, 'perpendicular slab');
      row(4, 2, 'parallel slab');
      row(4, 3, 'cylinder');
      row(4, 4, 'thin perpendicular slab');
      row(4, 5, 'thin parallel slab');
      row(4, 6, 'double perpendicular slabs');
      row(4, 7, 'clear');
      column(5, 'Help');
      row(5, 1, 'General Help');
      row(5, 2, 'Cell Display');
      row(5, 3, 'Close-up Window');
      row(5, 4, 'Display Menu');
      row(5, 5, 'Sliders & Buttons');
      row(5, 6, 'Cell Selection');
      RowChecked(3,2,TRUE);
   END;
END;

PROCEDURE DefineSliders;   {define sliders and buttons but do not display yet}
{the default values are global variables, so if the sliders are closed
down and then redisplayed, they will have the same value they last had}
VAR
   slider2label : string;
   slider1default, slider2default, slider2min, slider2max : real;
   slider2minstr, slider2maxstr : string;
   slider1Ypos, slider2Ypos : real;

BEGIN
   slider1Ypos := 0.39;
   slider2Ypos := 0.30;
   slider1default := DielectricExtField;
   IF DielectricPolMode = 1 THEN
   BEGIN
      slider2label := 'Polarization';
      slider2default := polarization;
      slider2min := -50;
      slider2max := 50;
   END
   ELSE                {DielectricPolMode = 2 or 3}
   BEGIN
      slider2label := 'Susceptibility';
      slider2default := susceptibility;
      slider2min := 0;
      slider2max := 10.;
   END;
   sliders.init;
   slider2minstr := NumStr(slider2min, 2, 0);
   slider2maxstr := NumStr(slider2max, 2, 0);
   sliders.create(1, 0, 10, slider1default, 0.67, slider1Ypos, 0.98, 1,
                             '0', '10', 'External Field', FALSE);
   sliders.create(2, slider2min, slider2max, slider2default, 0.67, slider2Ypos,
                 0.98, 1, slider2minstr, slider2maxstr, slider2label, FALSE);
   sliders.create(3, 0, 2, DiScale, 0.67, 0.13, 0.98, 2, '0', '2',
                            'Plot Scale', FALSE);
   buttons.init;
   buttons.create(1, 0.7,  0.22, 'POLx');
   buttons.create(2, 0.85, 0.22, 'POLy');
   buttons.number := 2 - PolXorPolY;
END;

PROCEDURE EraseHelpScreen(VAR HelpScreen : HelpScrType);
VAR
   inc : integer;

BEGIN
   FOR inc := 1 to 25 DO
      HelpScreen[inc] := '                                                  ';
END;

PROCEDURE DefineCredits;
BEGIN
   EraseHelpScreen(Credits);
   Credits[2] := '                Dielectric Media                 ';
   Credits[4] := '       Concept and design by Lyle Roelofs        ';
   Credits[5] := '           Programming by Nat Johnson            ';
   Credits[6] := '              Department of Physics              ';
   Credits[7] := '                Haverford College                ';
   Credits[8] := '               Haverford, PA  19041              ';
   Credits[9] := '                       USA                       ';
   Credits[11] :='      Copyright (c) John Wiley & Sons 1995       ';
   Credits[13] :='     For use with companion text -- CUPS E&M     ';
   Credits[15] :='  This program implements an investigation of    ';
   Credits[16] :='  dielectric materials.  The user controls the   ';
   Credits[17] :='  geometry and dielectric parameters of the      ';
   Credits[18] :='  material and the program solves for the polar- ';
   Credits[19] :='  ization self-consistently.  The electric field ';
   Credits[20] :='  and electric displacement are also obtained.   ';
   Credits[22] :='  (Include files DIELECT.HLP and DIELECT.DAT in  ';
   Credits[23] :='   same directory for help and dielectric sums.) d';
   Credits[25] :='        Hit any key or click to continue.        ';
END;

PROCEDURE ClearAll; {clears screen, including sliders,}
          {but redefines sliders so they can be displayed again}
BEGIN
   ClearMuppetPort;
   DefineSliders;              {Sets up both sliders and buttons}
END;

PROCEDURE DisplayScreen;
BEGIN
   SetColor(WindowColor);
   OpenViewPort(1);
   OpenViewPort(2);
   OpenViewPort(3);
   OpenViewPort(4);
   OpenViewPort(5);
   sliders.drawall;
   IF DielectricPolMode = 1 THEN
      buttons.drawall;
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

PROCEDURE DrawArrow(X1, Y1, X2, Y2 : real);
          {This procedure draws an arrow from (X1,Y1) to (X2,Y2).
           The size of the arrowhead is limited to 1.}
VAR
   deltaX, deltaY, delta : real;
   newX, newY            : real;
BEGIN
   PlotLine(X1,Y1,X2,Y2);                   {draw the shaft of the arrow}
   deltaX := X2 - X1;
   deltaY := Y2 - Y1;
   delta := SQRT(deltaX*deltaX + deltaY*deltaY);
   IF delta > 2.5 THEN
   BEGIN
      deltaX := 2.5*deltaX/delta;
      deltaY := 2.5*deltaY/delta;
   END;
   newX := X2 - (0.4 * deltaX - 0.2 * deltaY);
   newY := Y2 - (0.2 * deltaX + 0.4 * deltaY);
   PlotLine(newX,newY,X2,Y2);               {left half of arrowhead}
   newX := X2 - (0.4 * deltaX + 0.2 * deltaY);
   newY := Y2 - (-0.2 * deltaX + 0.4 * deltaY);
   PlotLine(newX,newY,X2,Y2);               {right half of arrowhead}
END;

PROCEDURE DrawCentArrow(X1, Y1, X2, Y2 : real);
          {this procedure draws an arrow from (X1,Y1) to (X2,Y2)
            centered on (X1,Y1).}
VAR
   deltaX, deltaY, avX, avY : real;
   newX1, newY1, newX2, newY2 : real;
   ColorNum, Xpix, Ypix  : integer;

BEGIN
   ColorNum := GetColor;
   deltaX := (X2 - X1)/2;
   deltaY := (Y2 - Y1)/2;
   newX1 := X1 - deltaX;
   newX2 := X1 + deltaX;
   newY1 := Y1 - deltaY;
   newY2 := Y1 + deltaY;
   DrawArrow(newX1, newY1, newX2, newY2);
   SetColor(white);
   PlotLine(x1,y1,x1,y1);         {White point at center}
   Map(x1,y1,Xpix,Ypix);
   Line(Xpix-1,Ypix,Xpix,Ypix+1);
   Line(Xpix,Ypix-1,Xpix+1,Ypix);
   SetColor(ColorNum);
END;

PROCEDURE CheckWindow;
CONST
   Square: ARRAY[1..4] OF PointType = ((X: 0;Y:0),(X:222;Y:0),(X:222;Y:218),
   (X:0;Y:218));

BEGIN
   SetColor(WindowColor);
   OpenViewPort(2);
   SetColor(2);
   SetFillStyle(0,2);
   FillPoly(4,Square);
END;

PROCEDURE FindSumLimits;
VAR
   wi, wj       : Integer;
   Check        : Boolean;
BEGIN
   DielectSumMaxX := MaxDielectricX;
   DielectSumMaxY := MaxDielectricY;
   wi := 0;
   Check := TRUE;
   REPEAT
      wi := wi + 1;
      FOR wj := 1 TO MaxDielectricY DO
      BEGIN
         IF (Susc[wi,wj]<>0) THEN
            Check := False;
      END;
      IF wi = MaxDielectricX THEN
         Check := FALSE;   
   UNTIL Check = FALSE;
   DielectSumMinX := wi;
   IF (DielectSumMinX < MaxDielectricX) THEN
   BEGIN
      wi := MaxDielectricX;
      Check := TRUE;
      REPEAT
         wi := wi - 1;
         FOR wj := 1 TO MaxDielectricY DO
         BEGIN
             IF (Susc[wi,wj]<>0) THEN
               Check := False;
         END;
         IF wi = 1 THEN
            Check := FALSE;   
      UNTIL Check = FALSE;
      DielectSumMaxX := wi;
   END;
   wj := 0;
   Check := TRUE;
   REPEAT
      wj := wj + 1;
      FOR wi := 1 TO MaxDielectricX DO
      BEGIN
         IF (Susc[wi,wj]<>0) THEN
            Check := False;
      END;
      IF wj = MaxDielectricY THEN
         Check := FALSE;   
   UNTIL Check = FALSE;
   DielectSumMinY := wj;
   IF (DielectSumMinY < MaxDielectricY) THEN
   BEGIN
      wj := MaxDielectricY;
      Check := TRUE;
      REPEAT
         wj := wj - 1;
         FOR wi := 1 TO MaxDielectricX DO
         BEGIN
            IF (Susc[wi,wj]<>0) THEN
               Check := False;
         END;
         IF wj = 1 THEN
            Check := FALSE;   
      UNTIL Check = FALSE;
      DielectSumMaxY := wj;
   END;
END;

PROCEDURE InCell(Xpos,Ypos : real; VAR Xcell,Ycell : integer);
{given a point (x,y) in space, this procedure returns the x and y
coordinates of the cell it lies within.}

BEGIN
   Xpos := Int((Xpos + 27)/2);
   Xcell := Round(Xpos);
   Ypos := Int((Ypos + 27)/2);
   Ycell := Round(Ypos);
END;

PROCEDURE DrawBox(X,Y : integer);
VAR
   X1, X2, Y1, Y2 : real;

BEGIN
   IF DielectricPolMode = 1 THEN
      SetColor(LightBlue)
   ELSE
   BEGIN
    IF susc[X,Y] = 0 THEN
      SetColor(DarkGray)   {invisible}
    ELSE IF susc[X,Y] < 2 THEN
      SetColor(LightGray)
    ELSE IF susc[X,Y] < 4 THEN
      SetColor(Cyan)  
    ELSE IF susc[X,Y] < 6 THEN
      SetColor(Brown)  
    ELSE IF susc[X,Y] < 8 THEN
      SetColor(LightRed)  
    ELSE
      SetColor(LightMagenta);
   END;
   X1 := 2*X - 26.8;
   X2 := 2*X - 25.2;
   Y1 := 2*Y - 26.8;
   Y2 := 2*Y - 25.2;
   PlotLine(X1,Y1,X1,Y2);
   PlotLine(X1,Y2,X2,Y2);
   PlotLine(X2,Y2,X2,Y1);
   PlotLine(X2,Y1,X1,Y1);
END;

PROCEDURE DrawSelection(X,Y : integer; On : boolean);
VAR
   X1, X2, Y1, Y2 : real;

BEGIN
   IF On THEN
      SetColor(15)  {Draw Selection in white}
   ELSE
      SetColor(8);  {Erase selection - dark grey}
   X1 := 2*X - 26.3;
   X2 := 2*X - 25.7;
   Y1 := 2*Y - 26.3;
   Y2 := 2*Y - 25.7;
   PlotLine(X1,Y1,X1,Y2);
   PlotLine(X1,Y2,X2,Y2);
   PlotLine(X2,Y2,X2,Y1);
   PlotLine(X2,Y1,X1,Y1);
END;

PROCEDURE DrawCloseup(Select : Boolean);
VAR
   X1, X2, Y1, Y2 : real;
BEGIN
   IF Select = TRUE THEN
      SetColor(15)
   ELSE
      SetColor(8);
   X1 := 2*CloseupX - 26.5;
   X2 := 2*CloseupX - 25.5;
   Y1 := 2*CloseupY - 26.5;
   Y2 := 2*CloseupY - 25.5;
   {PlotLine(X1,Y1,X2,Y2);
   PlotLine(X2,Y1,X1,Y2);}
   PlotLine(X1,Y1,X2,Y1);
   PlotLine(X2,Y1,X2,Y2);
   PlotLine(X2,Y2,X1,Y2);
   PlotLine(X1,Y2,X1,Y1);
   X1 := 2*CloseupX - 26.4;
   X2 := 2*CloseupX - 25.6;
   Y1 := 2*CloseupY - 26.4;
   Y2 := 2*CloseupY - 25.6;
   PlotLine(X1,Y1,X2,Y1);
   PlotLine(X2,Y1,X2,Y2);
   PlotLine(X2,Y2,X1,Y2);
   PlotLine(X1,Y2,X1,Y1);
END;

PROCEDURE DrawDielectric(On : boolean);
VAR
   incX,incY : integer;

BEGIN
   FOR incX := 1 TO MaxDielectricX DO
      FOR incY := 1 TO MaxDielectricY DO
         IF (Susc[incX,incY] <> 0) THEN
            DrawBox(incX,incY);
   FOR incX := 1 TO MaxDielectricX DO
      FOR incY := 1 TO MaxDielectricY DO
         IF CellSelected[incX,incY] THEN
            DrawSelection(incX,incY,On);
   DrawCloseup(TRUE);
END;

PROCEDURE EfieldSum(wi, wj : Integer;  VAR NewEX, NewEY : Real);
              {Determines total electric field at cell (wi,wj)
               due to external field and other polarized cells.
               Self field is not included.}
VAR
   wip, wjp            : Integer;
   sumX, sumY          : Real;
   Argi, Argj, AbsArgi, AbsArgj, sign  : Integer;
BEGIN
   sumX := 0.;
   sumY := 0.;
   FOR wip := DielectSumMinX TO DielectSumMaxX DO
                                        {Loop over all other cells to}
   BEGIN                                {determine their contribution}
      Argi := wi - wip;                 {to the field at (wi,wj) cell.}
      AbsArgi := Abs(Argi);
      sign := -1;
      FOR wjp := DielectSumMinY TO DielectSumMaxY DO
      BEGIN
         IF ((wip <> wi) OR (wjp <> wj)) THEN
         BEGIN
            Argj := wj - wjp;
            AbsArgj := Abs(Argj);
            sign := Argi*Argj;
            IF (sign > 0) THEN
               sign := 1
            ELSE IF (sign < 0) THEN
               sign := -1;
            sumX := sumX + PolX[wip,wjp]*PolSumX[AbsArgi,AbsArgj];   
            sumX := sumX + PolY[wip,wjp]*PolSumY[AbsArgi,AbsArgj]*sign;
            sumY := sumY + PolY[wip,wjp]*PolSumX[AbsArgj,AbsArgi];
            sumY := sumY + PolX[wip,wjp]*PolSumY[AbsArgj,AbsArgi]*sign;
         END;    {End if}
      END;   {End FOR wjp}
   END;   {End FOR wip}
   NewEX := sumX/(4*Pi) + DielectricExtField;  {Note 4Pi from MKS units.}
   NewEY := sumY/(4*Pi);
END;

PROCEDURE SolvePolarization;

CONST
   ReimFactor = 0.528;           {beta + zeta(3)/(2*pi)}

VAR
   wi, wj, iterations, inc            : Integer;
   converged                          : Boolean;
   PolMagSqr, NewEX, NewEY            : Real;
   DelX, DelY, Del, IntDel, Tol       : Real;
   ArrowXi, ArrowYi, ArrowXf, ArrowYf : Real;
   keyNum                             : Byte;
   NewPolX, NewPolY                   : DielectricArray;

BEGIN
   IF DielectricPolMode <> 1 THEN
   BEGIN
      BottomMessage('Plotting P during self-consistent determination.');
      TopMessage('Use F3 to stop calculation.');
      FindSumLimits;
      Tol := 0.0001;                 {Convergence tolerance}
      iterations := 20;              {Max. # of iterations}
      IF DielectricPolMode = 3 THEN
         iterations := 1;
      OpenViewPort(1);
      SelectViewPort(1);
      DrawDielectric(FALSE);
      FOR inc := 1 TO iterations DO
      BEGIN
         FOR wi := DielectSumMinX TO DielectSumMaxX DO
         BEGIN
            FOR wj := DielectSumMinY TO DielectSumMaxY DO
            BEGIN
               EfieldSum(wi, wj, NewEX, NewEY);
               ElcX[wi,wj] := NewEX/(1+Susc[wi,wj]*ReimFactor);
               NewPolX[wi,wj] := Susc[wi,wj]*ElcX[wi,wj];
               ElcY[wi,wj] := NewEY/(1+Susc[wi,wj]*ReimFactor);
               NewPolY[wi,wj] := Susc[wi,wj]*ElcY[wi,wj];
            END;
         END;
         IntDel := 0.;                         {Reset conv. accumulator}
         FOR wi := DielectSumMinX TO DielectSumMaxX DO
         BEGIN
            FOR wj := DielectSumMinY TO DielectSumMaxY DO
            BEGIN
               ArrowXi := (2.0*wi - 26);
               ArrowYi := (2.0*wj - 26);
               ArrowXf := ArrowXi + DiScale*PolX[wi,wj];
               ArrowYf := ArrowYi + DiScale*PolY[wi,wj];
               SetColor(8);
               DrawCentArrow(ArrowXi, ArrowYi, ArrowXf, ArrowYf);
               DelX := NewPolX[wi,wj] - PolX[wi,wj];    {Convergence test}
               DelY := NewPolY[wi,wj] - PolY[wi,wj];
               Del := SQRT(DelX*DelX + DelY*DelY);
               IntDel := IntDel + Del;
               PolX[wi,wj] := NewPolX[wi,wj];
               PolY[wi,wj] := NewPolY[wi,wj];
               PolMagSqr := PolX[wi,wj]*PolX[wi,wj] + PolY[wi,wj]*PolY[wi,wj];
               IF (PolMagSqr > 1000) THEN                {Clip the polarization}
               BEGIN
                  PolMagSqr := SQRT(PolMagSqr);
                  PolX[wi,wj] := PolX[wi,wj]*1000/PolMagSqr;
                  PolY[wi,wj] := PolY[wi,wj]*1000/PolMagSqr;
               END; {End IF}
               ArrowXi := (2.0*wi - 26);
               ArrowYi := (2.0*wj - 26);
               ArrowXf := ArrowXi + DiScale*PolX[wi,wj];
               ArrowYf := ArrowYi + DiScale*PolY[wi,wj];
               SetColor(PolarizationColor);
               DrawCentArrow(ArrowXi, ArrowYi, ArrowXf, ArrowYf);
            END;                           {End For wj}
         END;                              {End For wi}
         IF IntDel < Tol THEN              {Check for convergence}
         BEGIN
            inc := iterations;
            BottomMessage('Calculation converged.  Press any key to continue');
            Pause;
         END;                               {End IF convergence}
         CheckForEvents;
         IF HotKeys.pressed(keyNum) THEN    {Check for interrupt}
         BEGIN
            IF keyNum = 3 THEN
               inc := iterations;
         END;
      END;                                  {End FOR iterations}
      BottomMessage(' ');
      TopMessage(' ');
   END
   ELSE
   BEGIN
      BottomMessage('Iterative calculation not active in fixed pol. mode.');
   END;                                     {End IF DielectricPolMode}
END;


PROCEDURE DielectricEfield;        {Determines Efield in whole region based}
                                   { on current polarization values.}
VAR
   wi, wj, wip, wjp    : Integer;
   sumX, sumY          : Real;
   Argi, Argj, AbsArgi, AbsArgj, sign  : Integer;
BEGIN
 IF RecalcEfield THEN              {Check flag}
 BEGIN
   BottomMessage('Calculating Efield everywhere -- this will take a moment.');
   FindSumLimits;
   FOR wi := 1 TO MaxDielectricX DO
   BEGIN
      FOR wj := 1 TO MaxDielectricY DO
      BEGIN
         sumX := 0;
         sumY := 0;
         FOR wip := DielectSumMinX TO DielectSumMaxX DO
                                        {Loop over all other cells to}
         BEGIN                          {determine their contribution}
            Argi := wi - wip;           {to the field at (wi,wj) cell.}
            AbsArgi := Abs(Argi);
            sign := -1;
            FOR wjp := DielectSumMinY TO DielectSumMaxY DO
            BEGIN
               IF ((wip <> wi) OR (wjp <> wj)) THEN
               BEGIN
                  Argj := wj - wjp;
                  AbsArgj := Abs(Argj);
                  sign := Argi*Argj;
                  IF (sign > 0) THEN
                     sign := 1
                  ELSE IF (sign < 0) THEN
                     sign := -1;
                  sumX := sumX + PolX[wip,wjp]*PolSumX[AbsArgi,AbsArgj];  
                  sumX := sumX + PolY[wip,wjp]*PolSumY[AbsArgi,AbsArgj]*sign;
                  sumY := sumY + PolY[wip,wjp]*PolSumX[AbsArgj,AbsArgi];
                  sumY := sumY + PolX[wip,wjp]*PolSumY[AbsArgj,AbsArgi]*sign;
               END
               ELSE                           {Do 'self' field arising from}
               BEGIN                          {other cells in [wi,wj] column}
                  sumX := sumX - 6.63584*PolX[wi,wj];   {6.63...=4*pi*beta +}
                  sumY := sumY - 6.63584*PolY[wi,wj];   {          2*zeta(3)}
               END;    {End if}
            END;   {End FOR wjp}
         END;   {End FOR wip}
         ElcX[wi,wj] := sumX/(4*Pi) + DielectricExtField;  {Note 4Pi from MKS units.}
         ElcY[wi,wj] := sumY/(4*Pi);
      END;   {End FOR wj}
   END;   {End FOR wi}
   RecalcEfield := FALSE;
   BottomMessage(' ');
 END;
END;

PROCEDURE DisplayDielectricField;

VAR
   wi, wj  : integer;
   ArrowXi, ArrowXf, ArrowYi, ArrowYf : real;

BEGIN
   SetColor(WindowColor);
   HideCursor;
   OpenViewPort(1);
   SelectViewPort(1);
   DrawDielectric(TRUE);
   DielectricEfield;
   CASE DielectricDisplayMode OF
      1: BEGIN       {Draw Polarization field}
            SetColor(PolarizationColor);
            FOR wi := 1 TO MaxDielectricX DO
            BEGIN
               FOR wj := 1 TO MaxDielectricY DO
               BEGIN
                  IF (Susc[wi,wj] <> 0) THEN
                  BEGIN
                     ArrowXi := (2.0*wi - 26);
                     ArrowYi := (2.0*wj - 26);
                     ArrowXf := ArrowXi + DiScale*PolX[wi,wj];
                     ArrowYf := ArrowYi + DiScale*PolY[wi,wj];
                     DrawCentArrow(ArrowXi, ArrowYi, ArrowXf, ArrowYf);
                  END;
               END; {End For wj}
            END; {End For wi}
         END;
      2: BEGIN       {Draw Electric field}
            SetColor(EFieldColor);
            FOR wi := 1 TO MaxDielectricX DO
            BEGIN
               FOR wj := 1 TO MaxDielectricY DO
               BEGIN
                  ArrowXi := (2.0*wi - 26);
                  ArrowYi := (2.0*wj - 26);
                  ArrowXf := ArrowXi + DiScale*ElcX[wi,wj];
                  ArrowYf := ArrowYi + DiScale*ElcY[wi,wj];
                  DrawCentArrow(ArrowXi, ArrowYi, ArrowXf, ArrowYf);
               END; {End For wj}
            END; {End For wi}
         END;
      3: BEGIN       {Draw Electric displacement}
            SetColor(DisplacementColor);
            FOR wi := 1 TO MaxDielectricX DO
            BEGIN
               FOR wj := 1 TO MaxDielectricY DO
               BEGIN
                  ArrowXi := (2.0*wi - 26);
                  ArrowYi := (2.0*wj - 26);
                  ArrowXf := ArrowXi + DiScale*(PolX[wi,wj] + ElcX[wi,wj]);
                  ArrowYf := ArrowYi + DiScale*(PolY[wi,wj] + ElcY[wi,wj]);
                  DrawCentArrow(ArrowXi, ArrowYi, ArrowXf, ArrowYf);
               END; {End For wj}
            END; {End For wi}
         END;
      END;  {End Case}
   ShowCursor;
END;

PROCEDURE DrawCellCloseup;
VAR
   Scale   : real;
   ArrowX, ArrowY : real;

BEGIN
   Scale := 2 * DiScale;
   TopMessage(' closeup of selected cell');
   SetColor(WindowColor);
   OpenViewPort(2);                 {get color for selected cell}
   IF susc[CloseupX,CloseupY] = 0 THEN
      SetColor(Black)
   ELSE IF susc[CloseupX,CloseupY] < 2 THEN
      SetColor(LightGray)   
   ELSE IF susc[CloseupX,CloseupY] < 4 THEN
      SetColor(Cyan)   
   ELSE IF susc[CloseupX,CloseupY] < 6 THEN
      SetColor(Brown)   
   ELSE IF susc[CloseupX,CloseupY] < 8 THEN
      SetColor(LightRed)  
   ELSE
      SetColor(LightMagenta);
   IF DielectricPolMode = 1 THEN
   BEGIN
      SetColor(CellPolColor);
      IF susc[CloseupX,CloseupY] = 0 THEN
         SetColor(Black);
   END;
   PlotLine(10,20,10,0);      {draw outline of cell in appropriate color}
   PlotLine(10,0,-10,0);
   PlotLine(-10,0,-10,20);
   PlotLine(-10,20,10,20);
   SetColor(DisplacementColor);            {draw arrow for displacement}
   ArrowX := Scale*(PolX[CloseupX,CloseupY] + ElcX[CloseupX,CloseupY]);
   ArrowY := Scale*(PolY[CloseupX,CloseupY] + ElcY[CloseupX,CloseupY]);
   DrawArrow(0, 10, ArrowX, 10+ArrowY);
   SetColor(EFieldColor);                  {draw arrow for electric field}
   ArrowX := Scale*ElcX[CloseupX,CloseupY];
   ArrowY := Scale*ElcY[CloseupX,CloseupY];
   DrawArrow(0, 10, ArrowX, 10+ArrowY);
   SetColor(PolarizationColor);            {draw arrow for polarization}
   ArrowX := Scale*PolX[CloseupX,CloseupY];
   ArrowY := Scale*PolY[CloseupX,CloseupY];
   DrawArrow(0, 10, ArrowX, 10+ArrowY);
   SetColor(7);
   IF DielectricPolMode <>1 THEN
   BEGIN
      Print(2,  7, 'Susc. = ');
      Print(10,  7, NumStr(susc[CloseupX,CloseupY], 5, 1));
   END;
   Print(2,  8, 'x = ');
   Print(7,  8, NumStr(CloseupX, 5, 1));
   Print(13, 8, '; y = ');
   Print(20, 8, NumStr(CloseupY, 5, 1));
   SetColor(PolarizationColor);
   Print(2,  9, 'Px= ');
   Print(7,  9, NumStr(PolX[CloseupX,CloseupY], 5, 2));
   Print(13, 9, '; Py= ');
   Print(20, 9, NumStr(PolY[CloseupX,CloseupY], 5, 2));
   SetColor(EFieldColor);
   Print(2, 10, 'Ex= ');
   Print(7, 10, NumStr(ElcX[CloseupX,CloseupY], 5, 2));
   Print(13,10, '; Ey= ');
   Print(20,10, NumStr(ElcY[CloseupX,CloseupY], 5, 2));
   SetColor(DisplacementColor);
   Print(2, 11, 'Dx= ');
   Print(7, 11, NumStr(PolX[CloseupX,CloseupY] + ElcX[CloseupX,CloseupY], 5, 2));
   Print(13,11, '; Dy= ');
   Print(20,11, NumStr(PolY[CloseupX,CloseupY] + ElcY[CloseupX,CloseupY], 5, 2));
END;

PROCEDURE UpdateScreen;                {calls the appropriate procedures}
                                       {to display the updated screen.}
BEGIN
   ClearAll;
   DisplayScreen;
   DielectricMenu.display;
   RecalcEField := TRUE;
   DisplayDielectricField;
   DrawCellCloseup;
END;

PROCEDURE switch(VAR value1, value2 : integer);
VAR
   dummy : integer;
BEGIN
   dummy := value1;
   value1 := value2;
   value2 := dummy;
END;

PROCEDURE DielectricSliderHandler(SliderChanged : Integer);
VAR
   incX, incY    : integer;  {incrementals}

BEGIN
   REPEAT
      CheckForEvents;                {wait until slider stops moving}
   UNTIL sliders.changed = FALSE;    {then get the new values}
   BottomMessage(numStr(SliderChanged,3,1));
   CASE SliderChanged OF
      1: BEGIN
            DielectricExtField := sliders.value(1);
            RecalcEField := TRUE;
         END;
      2: BEGIN
            IF DielectricPolMode = 1 THEN
            BEGIN
               polarization := sliders.value(2);
               polarization := 2 * round(polarization/2);   {Round off to nearest even int. value.}
               sliders.reset(2, polarization);              {Reset slider accordingly.}
               IF PolXorPolY = 1 THEN
                  FillSelectedDielectricArray(PolX,polarization)
               ELSE
                  FillSelectedDielectricArray(PolY,polarization);
               FillSelectedDielectricArray(susc,1);
            END
            ELSE
            BEGIN
               susceptibility := sliders.value(2);
               FillSelectedDielectricArray(susc,susceptibility);
               FillSelectedDielectricArray(PolX,susceptibility*DielectricExtField/2);
               FillSelectedDielectricArray(PolY,0);        {Reset polarizations}
            END;
            RecalcEfield := TRUE;
         END;
      3: DiScale := sliders.value(3);
   END;    {End CASE}
END;

PROCEDURE ButtonsHandler;

BEGIN
   PolXorPolY := 2 - buttons.number;
END;

PROCEDURE DielectricOptionsHandler(rowchosen : integer);
BEGIN
   CASE rowchosen of
      1: BEGIN
            HideCursor;
            UpdateScreen;
            ShowCursor;
         END;
      2: BEGIN            {Fixed polarization mode}
            DielectricPolMode := 1;
            DielectricMenu.RowChecked(3, 2, TRUE);
            DielectricMenu.RowChecked(3, 3, FALSE);
            DielectricMenu.RowChecked(3, 4, FALSE);
            IF PolXorPolY = 1 THEN
            BEGIN
               FillSelectedDielectricArray(PolX,polarization);
               FillSelectedDielectricArray(PolY,0);
            END
            ELSE
            BEGIN
               FillSelectedDielectricArray(PolY,polarization);
               FillSelectedDielectricArray(PolX,0);
            END;
         END;
      3: BEGIN            {Fixed susceptibility mode (to convergence)}
            DielectricPolMode := 2;
            DielectricMenu.RowChecked(3, 2, FALSE);
            DielectricMenu.RowChecked(3, 3, TRUE);
            DielectricMenu.RowChecked(3, 4, FALSE);
            FillSelectedDielectricArray(susc,susceptibility);
         END;
      4: BEGIN            {Fixed susceptibility mode (step by step)}
            DielectricPolMode := 3;
            DielectricMenu.RowChecked(3, 2, FALSE);
            DielectricMenu.RowChecked(3, 3, FALSE);
            DielectricMenu.RowChecked(3, 4, TRUE);
         END;
   END;                   {END CASE}
END;

PROCEDURE EraseArrays;
BEGIN
            FillDielectricArray(Susc,1,MaxDielectricX,1,MaxDielectricY,0);
            FillDielecBoolArray(CellSelected,1,MaxDielectricX,1,MaxDielectricY,FALSE);
            FillDielectricArray(PolX,1,MaxDielectricX,1,MaxDielectricY,0);
            FillDielectricArray(PolY,1,MaxDielectricX,1,MaxDielectricY,0);
END;

PROCEDURE SelectArray(X1,X2,Y1,Y2:integer);
BEGIN
   IF DielectricPolMode = 1 THEN
   BEGIN
     IF PolXorPolY = 1 THEN
        FillDielectricArray(PolX,X1,X2,Y1,Y2,polarization)
     ELSE
        FillDielectricArray(PolY,X1,X2,Y1,Y2,polarization);
     FillDielectricArray(Susc,X1,X2,Y1,Y2,1);      
         {Programming note: array susc serves as an indicator that a cell
          is polarized even in PolMode 1.}
   END
   ELSE
   BEGIN
     FillDielectricArray(PolX,X1,X2,Y1,Y2,DielectricExtField*susceptibility/2);
     FillDielectricArray(Susc,X1,X2,Y1,Y2,susceptibility);
   END;
   FillDielecBoolArray(CellSelected,X1,X2,Y1,Y2,TRUE);
   RecalcEfield := TRUE;
END;

PROCEDURE DielectricArrayHandler(rowchosen : integer);
VAR
   incX, incY       : integer;
   radius, distance : real;
BEGIN
   CASE rowchosen OF
      1: BEGIN                       {Perpendicular slab}
            EraseArrays;
            SelectArray(11,15,6,20);
         END;
      2: BEGIN                       {Parallel slab}
            EraseArrays;
            SelectArray(6,20,11,15);
         END;
      3: BEGIN                       {Cylinder}
            EraseArrays;
            radius := 6.1;
            FOR incX := 1 TO MaxDielectricX DO
                FOR incY := 1 TO MaxDielectricY DO
                BEGIN
                   distance := SQRT((incX-13)*(incX-13) + (incY-13)*(incY-13));
                   IF distance < radius THEN
                      SelectArray(incX,incX,incY,incY);
                END;     {End FOR Y}
         END;
      4: BEGIN                      {Thin perpendicular slab}
            EraseArrays;
            SelectArray(13,13,6,20);
         END;
      5: BEGIN                      {Thin parallel slab}
            EraseArrays;
            SelectArray(6,20,13,13);
         END;
      6: BEGIN                      {Double perpendicular slabs}
            EraseArrays;
            SelectArray(9,11,6,20);
            SelectArray(15,17,6,20);
         END;
      7: EraseArrays;
   END; {End Case}
   RecalcEfield := TRUE;
END;

PROCEDURE DielectricMenuHandler;
BEGIN
   CASE DielectricMenu.colchosen OF              
      1: CASE DielectricMenu.rowchosen OF
            1: AboutCUPS;
            2: Help(Credits);
            3: Configuration;
            4: QuitFlag := TRUE;
         END;
      2: BEGIN
            IF DielectricDisplayMode <> DielectricMenu.rowchosen THEN
               DielectricDisplayMode := DielectricMenu.rowchosen;
            DisplayDielectricField;
         END;
      3: BEGIN
            DielectricOptionsHandler(DielectricMenu.rowchosen);
            sliders.done;
            buttons.done;
            DefineSliders;
            UpdateScreen;
            DrawCellCloseup;
         END;
      4: BEGIN
            DielectricArrayHandler(DielectricMenu.rowchosen);
            DisplayDielectricField;
         END;
      5: CASE DielectricMenu.rowchosen OF
            1: BEGIN
                  ShowHelp('dielect.hlp','DielectricHelp');
                  ShowHelp('dielect.hlp','DielectricHelpContinued');
               END;
            2: ShowHelp('dielect.hlp','CellDisplayHelp');
            3: ShowHelp('dielect.hlp','CloseUpWindowHelp');
            4: ShowHelp('dielect.hlp','DielectricDisplayHelp');
            5: BEGIN
                  ShowHelp('dielect.hlp','DielecSlidersHelp');
                  ShowHelp('dielect.hlp','DielecSlidersHelpContinued');
               END;
            6: ShowHelp('dielect.hlp','CellSelectHelp');
         END; {End Case}
   END; {End Case}
END;

PROCEDURE Initialize;
BEGIN
   CUPSInit;
   Randomize;
   InitializeGlobals;
   DefineViewPorts;
   DefineHotKeys;
   DefineDielectricMenu;
   DefineSliders;
   DefineCredits;
   IF PolSumsRead = FALSE THEN
   BEGIN
      BottomMessage('Reading in polarization sums for dielectric calculations.');
      ReadPolSums;
      BottomMessage(' ');
   END;
   ClearAll;
   DielectricArrayHandler(1);
END;

VAR
   keyNum           : Byte;
   mouseX, mouseY   : real;
   cellX, cellY     : integer;
   inside           : boolean;
   incX, incY       : integer;          {incrementals}
   SliderChanged    : integer;

BEGIN  {main program}
   Initialize;
   DefineSliders;
   Help(Credits);
   UpdateScreen;
   REPEAT
      CheckForEvents;
      IF sliders.changed THEN
      BEGIN
         SliderChanged := sliders.LastAltered;
         DielectricSliderHandler(SliderChanged);
         HideCursor;
         UpdateScreen;
         ShowCursor;
      END;
      IF buttons.changed THEN ButtonsHandler;
      IF Event.MouseClicked THEN        {single or double click}
      BEGIN
         MousePosn(mouseX,mouseY,1,1,inside);
         IF inside THEN
         BEGIN
            SelectViewPort(1);
            InCell(mouseX,mouseY,cellX,cellY);
            IF Event.DoubleClicked THEN      {select or deselect a cell}
            BEGIN                            
               CellSelected[cellX,cellY] := not CellSelected[cellX,cellY];
               DrawSelection(cellX,cellY, CellSelected[cellX,cellY]);
               IF CellSelected[cellX,cellY] THEN
               BEGIN
                 IF DielectricPolMode = 1 THEN
                 BEGIN
                  IF PolXorPolY = 1 THEN
                     PolX[cellX,cellY] := polarization
                  ELSE
                     PolY[cellX,cellY] := polarization;
                     susc[cellX,cellY] := 1;
                  IF polarization = 0  THEN         {Reset susc.'s and selection}
                  BEGIN
                     susc[cellX,cellY] := 0;
                  END;
                 END
                 ELSE
                 BEGIN
                    susc[cellX,cellY] := susceptibility;
                    PolX[cellX,cellY] := susceptibility*DielectricExtField/2;
                    PolY[cellX,cellY] := 0;        {Reset polarizations}
                 END;
               END;
               RecalcEfield := TRUE;
               HideCursor;
               UpdateScreen;
               ShowCursor;
            END
            ELSE                             {must be a single click}
            BEGIN                            {change closeup pointer}
               DrawCloseup(FALSE);           {erase old selection}
               CloseupX := cellX;
               CloseupY := cellY;
               DrawCloseup(TRUE);            {put X on new selection}
               DrawCellCloseup;
            END;
         END;
      END;
      IF HotKeys.pressed(keyNum) THEN
         CASE keyNum OF
            1: BEGIN
                  ShowHelp('dielect.hlp','DielectricHelp');
                  ShowHelp('dielect.hlp','DielectricHelpContinued');
               END;
            2: BEGIN
                  HideCursor;
                  ClearAll;
                  SolvePolarization;
                  UpdateScreen;
                  ShowCursor;
               END;
            4: BEGIN                 {Select all cells with nonzero susc.}
                  FOR incX := 1 TO MaxDielectricX DO
                  BEGIN
                     FOR incY := 1 TO MaxDielectricY DO
                     BEGIN
                        IF susc[incX,incY] = 0 THEN
                           CellSelected[incX,incY] := FALSE
                        ELSE
                           CellSelected[incX,incY] := TRUE;
                     END;
                  END;           {End FOR incX}
                  DisplayDielectricField;
               END;
            5: BEGIN                 {Clear selection but do not change values}
                  FOR incX := 1 TO MaxDielectricX DO
                     FOR incY := 1 TO MaxDielectricY DO
                        CellSelected[incX,incY] := FALSE;
                  DisplayDielectricField;
               END;
            6: IF DielectricMenu.chosen THEN DielectricMenuHandler;
         END;   {End CASE}
      IF DielectricMenu.activated THEN DielectricMenuHandler;
   UNTIL QuitFlag;
   sliders.done;
   buttons.done;
   DielectricMenu.done;
   CUPSDone;
END.
