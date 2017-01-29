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

Program ImageChargesandMultipoleExpansions;

{******************Image Charge and Multipole Expansion*******************}
{*                                                                       *}
{*                            Version 3.7.1                              *}
{*                                                                       *}
{*               (3.7.1 has zero-field on conductor problem              *}
{*                      found by Derren fixed.)                          *}
{*                (Version 3.4.1 has changes suggested by                *}
{*                  CUPS directors in 6/92.)                             *}
{*                (Version 3.4.2 is sort of Mac compatible;              *}
{*                  See comments with labeled with &)                    *}
{*                                                                       *}
{*                  Concept and Design by Lyle Roelofs                   *}
{*              Programming by Nat Johnson and Lyle Roelofs              *}
{*                         Department of Physics                         *}
{*                           Haverford College                           *}
{*                          Haverford, PA  19041                         *}
{*                                                                       *}
{*                               July, 1994                              *}
{*                                                                       *}
{*************************************************************************}


   Uses
      CUPS, CUPSmupp, crt,           {& crt must be commented out for Mac}
      graph, CUPSgui, CUPSfunc, CUPSgrph;

   Type
      RealPoint = Record
            X: real;
            Y: real;
         End;
      RealPointArray = Array[1..6] Of RealPoint;  {MaxCharges = 6}
      PolyType = ARRAY[1..7] OF PointType;   {& Take out this line for Mac version}
      ChargeValue = Array[1..6] Of real;       {MaxCharges = 6}
      ChargeBoolean = Array[1..6] Of boolean;  {These two types also used for}
                                     {image change and multipole values.}
   Const
      MaxCharges = 6;
      MaxMoments = 6;
      WindowColor = Cyan;
      TextColor = Yellow;
      HiLiteTextColor = LightRed;
      ConductorColor = LightGray;
      FieldLineColor = whitte;
      CloseUpBoxColor = LightCyan;
      PosColor = LightRed;
      NegColor = LightBlue;
      SelectionColor = whitte;
      ButtonColor = whitte;
      DarkBlue = 1;
      DarkGreen = 2;

 {Global variables for menus, sliders, and help and input screens}
   Var
      MultipoleMenu, ImageChargeMenu: TMenu;
      sliders: Tsliders;
      contour: Tcontour;
      HotKeys: THotKeys;
      Credits: HelpScrType;
      SimulationMode: integer;  {1 = multipoles, 2 = image charges, 0 = Quit}
      Conductor: integer;  {1 - 5 depending on chosen conductor configuration}
      ICDisplayMode: integer;  {1 = charges only, 2 = Field shown only outside}
                               {the conductor, 3 = Field of all charges shown in all space.}
      MEDisplayMode: integer;  {1 = potential, 2 = E-field, 3 = superimposed}
      MESurface: integer; {1 = cylinder, 2 = split sphere I, 3 = sphere off center,}
                             {4 = split sphere II, 5 = cone, 6 = spool}
      AutoDisplay: boolean;  {true = always display, false = wait for hotkey}
      AutoDisplayOverride: boolean; {true = F2 pressed, override display suppression}
      multi, charge: real;{slider values in multipole and image charge modes}

      CloseUp: RealPoint;             {center point of the Close Up Box}

      ChargePts: RealPointArray;
      ChargeStrength, MomentStrength: ChargeValue;
      ChargeOn, MomentOn: ChargeBoolean;
      SelectedCharge, SelectedMoment: integer;



   Procedure InitializeGlobals;
      Var
         inc: integer;  {incremental}
   Begin
      SimulationMode := 1;         {Image Charge mode}
      Conductor := 1;              {vertical plane}
      MESurface := 1;              {cylinder}
      ICDisplayMode := 1;          {don't draw field lines}
      MEDisplayMode := 1;          {draw potential contour}
      AutoDisplay := TRUE;         {display field lines automatically}
      AutoDisplayOverride := FALSE;
      multi := 1;
      charge := 2.0;
      CloseUp.X := 0;
      CloseUp.Y := 0;
      For inc := 1 To MaxCharges Do
      Begin
         ChargePts[inc].X := 0;
         ChargePts[inc].Y := 0;
         ChargeStrength[inc] := 0;
         ChargeOn[inc] := FALSE;
         MomentStrength[inc] := 0;
         MomentOn[inc] := FALSE;
      End;
      ChargeOn[1] := TRUE;
      ChargePts[1].X := 10;          {configuration for conductor option 1}
      ChargeStrength[1] := 2.0;      {charge 1 is selected, with strength 2}
      SelectedCharge := 1;
      SelectedMoment := 2;
      MomentOn[2] := TRUE;
      MomentStrength[2] := 1;
   End;

   Procedure DefineViewports;
   Begin
      DefineViewPort(1, 0, 0.65, 0.1, 0.95);         {Field window}
      DefineViewPort(2, 0.652, 1, 0.455, 0.91);      {Upper right window}
      DefineViewPort(3, 0.652, 1, 0.1, 0.26);        {Sliders window}
      DefineViewPort(4, 0, 1, 0.055, 0.095);      {BottomMessage window}
      DefineViewPort(5, 0.652, 1, 0.91, 0.95);       {small message window}
      DefineViewPort(6, 0.652, 1, 0.26, 0.45);       {radio button window}
      DefineViewPort(7, 0.652, 0.710, 0.26, 0.39);   {radio button # 1}
      DefineViewPort(8, 0.710, 0.768, 0.26, 0.39);   {radio button # 2}
      DefineViewPort(9, 0.768, 0.826, 0.26, 0.39);   {radio button # 3}
      DefineViewPort(10, 0.826, 0.884, 0.26, 0.39);  {radio button # 4}
      DefineViewPort(11, 0.884, 0.942, 0.26, 0.39);  {radio button # 5}
      DefineViewPort(12, 0.942, 1.000, 0.26, 0.39);  {radio button # 6}
      DefineScale(1, -25, 25, -25, 25);
      DefineScale(3, 0.5, 6.5, 0.26, 0.45);
      SelectScale(1);
   End;

   Procedure DefineMultipoleMenu;
   Begin
      With MultipoleMenu Do
      Begin
         init;
         column(1, 'File');
         row(1, 1, 'About CUPS');
         row(1, 2, 'About Program');
         row(1, 3, 'Configuration');
         row(1, 4, 'Exit Program');
         column(2, 'Parts');
         row(2, 1, 'Part 1: Image charges');
         row(2, 2, 'Part 2: Multipole expansion');
         column(3, 'Display');
         row(3, 1, 'Potential');
         row(3, 2, 'Electric field');
         row(3, 3, 'Both superimposed');
         column(4, 'Tools');
         row(4, 1, 'Automatic Field Display');
         row(4, 2, 'Change moments - keyboard');
         row(4, 3, 'Field Probe');
         row(4, 4, 'Refresh Screen');
         column(5, 'Surfaces');
         row(5, 1, 'Cylinder');
         row(5, 2, 'Split sphere I');
         row(5, 3, 'Sphere off center');
         row(5, 4, 'Split sphere II');
         row(5, 5, 'Cone');
         row(5, 6, 'Spool');
         column(6, 'Help');
         row(6, 1, 'FILE help');
         row(6, 2, 'PARTS help');
         row(6, 3, 'DISPLAY Help');
         row(6, 4, 'TOOLS Help');
         row(6, 5, 'Eq. SURFACES Help');
         row(6, 6, 'General Help');
         RowChecked(2, 2, TRUE);
         RowChecked(4, 1, TRUE);
         AutoChecking(4, 1);
         AutoChecking(5, 1);
      End;
   End;

   Procedure DefineImageChargeMenu;
   Begin
      With ImageChargeMenu Do
      Begin
         init;
         column(1, 'File');
         row(1, 1, 'About CUPS');
         row(1, 2, 'About Program');
         row(1, 3, 'Configuration');
         row(1, 4, 'Exit Program');
         column(2, 'Parts');
         row(2, 1, 'Part 1: Image charges');
         row(2, 2, 'Part 2: Multipole expansion');
         column(3, 'Display');
         row(3, 1, 'Show Charges only');
         row(3, 2, 'Field Lines - in vacuo only');
         row(3, 3, 'Field Lines - all space');
         column(4, 'Tools');
         row(4, 1, 'Automatic Field Display');
         row(4, 2, 'Place Charges - Keyboard');
         row(4, 3, 'Move Close-up Window');
         row(4, 4, 'Field Probe');
         row(4, 5, 'Field Lines from arb. line');
         row(4, 6, 'Refresh Screen');
         column(5, 'Surfaces');
         row(5, 1, 'Vertical Plane');
         row(5, 2, '90 degree angle');
         row(5, 3, '60 degree angle');
         row(5, 4, 'Spherical');
         row(5, 5, '120 degree angle');
         column(6, 'Help');
         row(6, 1, 'FILE help');
         row(6, 2, 'PARTS help');
         row(6, 3, 'DISPLAY help');
         row(6, 4, 'TOOLS help');
         row(6, 5, 'SURFACES help');
         row(6, 6, 'General help');
         RowChecked(2, 1, TRUE);
         RowChecked(3, 1, TRUE);
         AutoChecking(3, 1);
         AutoChecking(5, 1);
      End;
   End;

   Procedure DefineHotKeys;
   Begin
      With HotKeys Do
      Begin
         Init(5);
         key[1] := 'F1-Help';
         key[2] := 'F2-Draw';
         key[3] := 'F3-Stop Draw';
         key[4] := 'F4-Clear';
         key[5] := 'F10-Menu';
      End;
   End;

   Procedure DefineSliders;
      Var
         name: String;
         default: real;
   Begin
      If SimulationMode = 2 Then
      Begin
         name := 'amplitude';
         If SelectedMoment <> 0 Then
            default := MomentStrength[SelectedMoment]
         Else
            default := multi;
      End
      Else
      Begin
         name := 'charge value';
         If SelectedCharge <> 0 Then
            default := ChargeStrength[SelectedCharge]
         Else
            default := charge;
      End;
      sliders.init;
      sliders.create(1, -5, 5, default, 0.67, 0.18, 0.98, 1, '-5', '5', name, FALSE);
   End;

   Procedure EraseHelpScreen (Var HelpScreen: HelpScrType);
      Var
         inc: integer;

   Begin
      For inc := 1 To 25 Do
         HelpScreen[inc] := '                                                  ';
   End;

   Procedure DefineCredits;
   Begin
      EraseHelpScreen(Credits);
      Credits[2] := '           The Image Charge Method and            ';
      Credits[3] := '              Multipole Expansions               ';
      Credits[5] := '       Concept and design by Lyle Roelofs        ';
      Credits[6] := '   Programming by Nat Johnson and Lyle Roelofs   ';
      Credits[7] := '             Department of Physics               ';
      Credits[8] := '               Haverford College                 ';
      Credits[9] := '             Haverford, PA  19041                ';
      Credits[10] := '                      USA                        ';
      Credits[12] := '      Copyright (c) John Wiley & Sons 1995       ';
      Credits[14] := '     For use with companion text -- CUPS E&M     ';
      Credits[16] := ' This program implements user controlled explor- ';
      Credits[17] := ' ations of the image charge method for solving   ';
      Credits[18] := ' electrostatic problems involving point charges  ';
      Credits[19] := ' and conducting surfaces; and of the expansion   ';
      Credits[20] := ' in multipole moment for dealing with the exter- ';
      Credits[21] := ' ior potential problem in the case of cylindri-  ';
      Credits[22] := ' cal symmetry.                                   ';
      Credits[24] := '        Hit any key or click to continue.        ';
   End;

   Procedure Initialize;
   Begin                    {& Turn on new() statements for Mac Version}
      {new(MultipoleMenu);
      new(ImageChargeMenu);
      new(sliders);
      news(contour);
      news(HotKeys);}
      CUPSInit;
      InitializeGlobals;
      DefineViewPorts;
      DefineMultipoleMenu;
      DefineImageChargeMenu;
      DefineCredits;
      DefineHotKeys;
      DefineSliders;
      contour.init;
   End;

   Procedure DisplayScreen;
   Begin
      SetColor(WindowColor);
      OpenViewPort(1);
      SetBkColor(8);
      OpenViewPort(2);
      OpenViewPort(3);
      OpenViewPort(4);
      OpenViewPort(5);
      OpenViewPort(6);
      sliders.drawall;
      HotKeys.display;
   End;

   Procedure ClearAll; {clears screen, including sliders,}
           {but redefines sliders so they can be displayed again}
   Begin
      ClearMuppetPort;
      sliders.done;
      DefineSliders;
   End;

   Function Distance (PtOne, PtTwo: RealPoint): real;
      Var
         Xdis, Ydis, Distance2: real;
   Begin
      Xdis := PtOne.X - PtTwo.X;
      Ydis := PtOne.Y - PtTwo.Y;
      Distance2 := Xdis * Xdis + Ydis * Ydis;
      Distance := sqrt(Distance2);
   End;

   Procedure WindowOutline;
   Begin
      SetColor(WindowColor);
      PlotLine(-25, -25, -25, 25);
      PlotLine(-25, 25, 25, 25);
      PlotLine(25, 25, 25, -25);
      PlotLine(25, -25, -25, -25);
   End;

   Function inttochar (number: integer): char;
   Begin
      Case number Of
         1: 
            inttochar := '1';
         2: 
            inttochar := '2';
         3: 
            inttochar := '3';
         4: 
            inttochar := '4';
         5: 
            inttochar := '5';
         6: 
            inttochar := '6';
         7: 
            inttochar := '7';
         8: 
            inttochar := '8';
         9: 
            inttochar := '9';
      End; {End Case}
   End;

   Procedure BottomMessage (message: String);
 {Prints a message at the bottom of the screen.  Note that ViewPort 1 is}
{
automatically selected afterwards, in the case that another viewport}
{
is selected, the viewport must be reselected manually.}
   Begin
      SetColor(WindowColor);
      OpenViewPort(4);
      SetColor(TextColor);
      Print(1, 1, message);
      SelectViewPort(1);
   End;

   Procedure DrawUnsArrow (PtOne, PtTwo: RealPoint);
           {this procedure draws an arrow from PtOne to PtTwo}
{
           with an arrow head size that does not change.}
      Var
         deltaX, deltaY, scale: real;
         newX, newY: real;
   Begin
      PlotLine(PtOne.X, PtOne.Y, PtTwo.X, PtTwo.Y);   {draw the shaft of the arrow}
      deltaX := PtTwo.X - PtOne.X;
      deltaY := PtTwo.Y - PtOne.Y;
      scale := 2 / sqrt(deltaX * deltaX + deltaY * deltaY);
      newX := PtTwo.X - scale * (0.4 * deltaX - 0.2 * deltaY);
      newY := PtTwo.Y - scale * (0.2 * deltaX + 0.4 * deltaY);
      PlotLine(newX, newY, PtTwo.X, PtTwo.Y);              {left half of arrowhead}
      newX := PtTwo.X - scale * (0.4 * deltaX + 0.2 * deltaY);
      newY := PtTwo.Y - scale * (-0.2 * deltaX + 0.4 * deltaY);
      PlotLine(newX, newY, PtTwo.X, PtTwo.Y);             {right half of arrowhead}
   End;

   Procedure DrawArrow (PtOne, PtTwo: RealPoint);
           {this procedure draws an arrow from PtOne to PtTwo}
      Var
         deltaX, deltaY: real;
         newX, newY: real;
   Begin
      PlotLine(PtOne.X, PtOne.Y, PtTwo.X, PtTwo.Y);   {draw the shaft of the arrow}
      deltaX := PtTwo.X - PtOne.X;
      deltaY := PtTwo.Y - PtOne.Y;
      newX := PtTwo.X - (0.4 * deltaX - 0.2 * deltaY);
      newY := PtTwo.Y - (0.2 * deltaX + 0.4 * deltaY);
      PlotLine(newX, newY, PtTwo.X, PtTwo.Y);              {left half of arrowhead}
      newX := PtTwo.X - (0.4 * deltaX + 0.2 * deltaY);
      newY := PtTwo.Y - (-0.2 * deltaX + 0.4 * deltaY);
      PlotLine(newX, newY, PtTwo.X, PtTwo.Y);             {right half of arrowhead}
   End;

 {These are the procedures for Multipole Expansion Mode: }

   Procedure DrawMERadioButtons;
      Var
         inc: integer; {incremental}
         PrChr: char;
   Begin
      HideMouse;
      SetColor(WindowColor);
      OpenViewPort(6);
      SelectScale(3);
      SetColor(whitte);
      Print(1, 1, '  click radio buttons to');
      Print(1, 2, '    select a multipole');
      PlotSymbol(0.95, 0.36, '1');
      PlotSymbol(1.95, 0.36, '2');
      PlotSymbol(2.95, 0.36, '4');
      PlotSymbol(3.95, 0.36, '8');
      PlotSymbol(4.85, 0.36, '1');
      PlotSymbol(5.1, 0.36, '6');
      PlotSymbol(5.85, 0.36, '3');
      PlotSymbol(6.1, 0.36, '2');
      For inc := 1 To MaxCharges Do
      Begin
         SelectViewPort(inc + 6);
         SetColor(ButtonColor);
         SetFillStyle(0, ButtonColor);
         FillEllipse(17, 32, 9, 9);
         If MomentOn[inc] Then
            Print(2, 3, 'ON')
         Else
            Print(1, 3, 'OFF');
         If inc = SelectedMoment Then
         Begin
            SetFillStyle(1, ButtonColor);
            FillEllipse(17, 32, 6, 6);
         End;
      End; {End FOR}
      SelectScale(1);
      ShowMouse;
      ;
   End;

   Function Legendre (Order: integer;
                           X: real): real;
      Var
         Xsquared: real;
   Begin
      Xsquared := X * X;
      Case Order Of
         0: 
            Legendre := 1;
         1: 
            Legendre := X;
         2: 
            Legendre := (3 * Xsquared - 1) / 2;
         3: 
            Legendre := (5 * Xsquared - 3) * X / 2;
         4: 
            Legendre := (((35 * Xsquared) - 30) * Xsquared + 3) / 8;
         5: 
            Legendre := (((63 * Xsquared) - 70) * Xsquared + 15) * X / 8;
         6: 
            Legendre := ((((231 * Xsquared) - 315) * Xsquared + 105) * Xsquared - 5) / 16;
      End;  {End case}
   End;

   Function Potential (X, Y: real): real; FAR;   {&Comment out FAR for Mac Version}
      Var
         order, inc: integer;
         R, CosTheta, Radius, PotAccum: real;
         RtotheL, RinverseL: real;
   Begin
      PotAccum := 0.0;
      Radius := 5.0;
      R := X * X + Y * Y;
      R := sqrt(R);
      CosTheta := Y / R;
      If R > Radius Then
      Begin
         RinverseL := Radius / R;
         For inc := 1 To MaxMoments Do
         Begin
            order := inc - 1;
            If MomentOn[inc] Then
               PotAccum := PotAccum + RinverseL * MomentStrength[inc] * Legendre(order, CosTheta);
            RinverseL := RinverseL * Radius / R;
         End;   {End For}
      End
      Else
      Begin
         RtotheL := 1.0;
         For inc := 1 To MaxMoments Do
         Begin
            order := inc - 1;
            If MomentOn[inc] Then
               PotAccum := PotAccum + RtotheL * MomentStrength[inc] * Legendre(order, CosTheta);
            RtotheL := RtotheL * R / Radius;
         End;  {End For}
      End;
      Potential := PotAccum
   End;

   Procedure EfieldME (Pt: RealPoint;
                           Var EfieldX, EfieldY: real);
      Var
         inc: integer;
         XplusdelX, YplusdelY, PotentialAtXY: real;
   Begin
      XplusdelX := Pt.X + 0.01;
      YplusdelY := Pt.Y + 0.01;
      PotentialAtXY := Potential(Pt.X, Pt.Y);
      EfieldX := PotentialAtXY - Potential(XplusdelX, Pt.Y);
      EfieldY := PotentialAtXY - Potential(Pt.X, YplusdelY);
   End;

   Function InRectangle (Pt: RealPoint;
                           xmin, xmax, ymin, ymax: real): Boolean;
                  {Returns TRUE if Pt is in the specified rectangle}
   Begin
      InRectangle := FALSE;
      If (Pt.X <= xmax) And (Pt.X >= xmin) And (Pt.Y >= ymin) And (Pt.Y <= ymax) Then
         InRectangle := TRUE;
   End;

   Function InEqSurface (Pt: RealPoint): Boolean;
 {Returns TRUE if Point is within the currently chosen equipotential surface.}
      Var
         Rsquared: real;
         InTop, InBot, InMid: Boolean;
   Begin
      InEqSurface := FALSE;
      If (MESurface = 2) Or (MESurface = 4) Then
      Begin
         Rsquared := Pt.X * Pt.X + Pt.Y * Pt.Y;
         If Rsquared <= 5 Then
            InEqSurface := TRUE;
      End  {End sphere case}
      Else
      Begin
         Case MESurface Of
            1: 
            Begin                                       {cylinder}
               If InRectangle(Pt, -5, 5, -10, 10) Then
                  InEqSurface := TRUE;
            End;
            3: 
            Begin                                       {sphere off center}
               If Pt.X * Pt.X + (Pt.Y - 5) * (Pt.Y - 5) <= 100 Then
                  InEqSurface := TRUE;
            End;
            5: 
            Begin                                       {cone}
               If (Pt.Y >= -5) And (Pt.Y <= 2.747 * Pt.X + 14.619) And (Pt.Y <= -2.747 * Pt.X + 14.619) Then
                  InEqSurface := TRUE;
            End;
            6: 
            Begin                                       {spool}
               InMid := InRectangle(Pt, -5, 5, -5, 5);
               InBot := InRectangle(Pt, -7.5, 7.5, -5, -2.5);
               InTop := InRectangle(Pt, -7.5, 7.5, 2.5, 5);
               If InMid Or InBot Or InTop Then
                  InEqSurface := TRUE;
            End;
         End;  {End Case}
      End;    {End IF}
   End;

   Function InViewPort (Pt: RealPoint): Boolean;
   Begin
      InViewPort := TRUE;
      If Pt.X > 26 Then
      Begin
         InViewPort := FALSE;
      End
      Else
      Begin
         If Pt.Y > 26 Then
         Begin
            InViewPort := FALSE;
         End
         Else
         Begin
            If Pt.X < -26 Then
            Begin
               InViewPort := FALSE;
            End
            Else
            Begin
               If Pt.Y < -26 Then
                  InViewPort := FALSE;
            End;
         End;
      End;
   End;

   Procedure DrawFieldLineME (initPt: RealPoint);
      Var
         EndLine, PositiveStep: Boolean;
         Emagnitude, EfieldX, EfieldY: real;
         step, Limit, newDis, oldDis, tolerance, error: real;
         oldPt, newPt, newPtPred, avePt, Origin, negOldPt, negNewPt: RealPoint;
         CheckOverlap, InConductorCounter: integer;

   Begin
      HideMouse;
      SetColor(FieldLineColor);
      Limit := 500;
      Origin.X := 0;
      Origin.Y := 0;
      EndLine := FALSE;
      PositiveStep := TRUE;
      step := 0.2;
      tolerance := 1.0e-4;
      oldPt := initPt;
      oldDis := Distance(Origin, initPt);
      Repeat
         EfieldME(oldPt, EfieldX, EfieldY);
         Emagnitude := EfieldX * EfieldX + EfieldY * EfieldY;
         Emagnitude := sqrt(Emagnitude);
         newPtPred.X := oldPt.X + step * EfieldX / Emagnitude;
         newPtPred.Y := oldPt.Y + step * EfieldY / Emagnitude;
         avePt.X := (newPtPred.X + oldPt.X) / 2;
         avePt.Y := (newPtPred.Y + oldPt.Y) / 2;
         EfieldME(avePt, EfieldX, EfieldY);
         Emagnitude := EfieldX * EfieldX + EfieldY * EfieldY;
         Emagnitude := sqrt(Emagnitude);
         newPt.X := oldPt.X + step * EfieldX / Emagnitude;
         newPt.Y := oldPt.Y + step * EfieldY / Emagnitude;
         newDis := Distance(Origin, newPt);
         If InViewPort(newPt) Or InViewPort(oldPt) Then
         Begin
            If (InEqSurface(newPt) = FALSE) Then
            Begin
               PlotLine(oldPt.X, oldPt.Y, newPt.X, newPt.Y);     {Plots field line.}
               PlotLine(-oldPt.X, oldPt.Y, -newPt.X, NewPt.Y);   {Plots symmetric field line}
               If ((newDis - 16) * (oldDis - 16) < 0) Then
               Begin                                          {Draw field line arrow}
                  DrawUnsArrow(oldPt, newPt);
                  negOldPt := oldPt;
                  negOldPt.X := -oldPt.X;
                  negNewPt := newPt;
                  negNewPt.X := -newPt.X;
                  DrawUnsArrow(negOldPt, negNewPt)
               End; {End IF}
            End;  {End IF not in surface}
         End;
         oldPt := newPt;
         oldDis := newDis;
         error := distance(newPtPred, newPt);
         If error <= tolerance Then                 {increase step size}
            step := step * 2;
         If error > 10 * tolerance Then               {decrease step size}
            step := step / 2;
         If InEqSurface(newPt) Then                 {do not adjust step size while in eq. surface}
            step := 0.2;
         If (newDis < 5.2) Then
            Endline := TRUE;
         If Emagnitude = 0 Then
            EndLine := TRUE;
         If (newPt.X < -Limit) Or (newPt.X > Limit) Or (newPt.Y < -Limit) Or (newPt.Y > Limit) Then
            EndLine := TRUE;
      Until EndLine;
      ShowMouse;
      ;
   End;

   Procedure DisplayEFieldME;
 {Determines starting points for outward headed field lines}
{
   at the appropriate spacings from a spherical surface of radius 5}
{
   and calls DrawFieldLineME to draw the lines.}

      Var
         EndAngles: Boolean;
         initPt: RealPoint;
         EfieldX, EfieldY, angle: real;
         Eflux, EfluxEnough, FluxInc, norm: real;
         keynum: byte;

   Begin
      EfluxEnough := 0.05;
      EndAngles := FALSE;
      initPt.X := 0.0;
      initPt.Y := 5.0;
      angle := 0.0;
      Eflux := EfluxEnough / 2;
      Repeat
         Repeat                      {Integrate flux}
            angle := angle + 0.01;
            initPt.X := 5.1 * sin(angle);
            initPt.Y := 5.1 * cos(angle);
            EfieldME(initPt, EfieldX, EfieldY);
            FluxInc := sin(angle) * (EfieldX * sin(angle) + EfieldY * cos(angle));
            If (FluxInc > 0) Then
               Eflux := Eflux + FluxInc;
            If angle > 3.14 Then
               EndAngles := TRUE;
         Until (Eflux > EfluxEnough) Or (EndAngles);
         If (EndAngles) Then
            Eflux := 0.0
         Else
         Begin
            DrawFieldLineME(initPt);
            Eflux := 0.0;
         End;   {End IF}
         CheckForEvents;
         If HotKeys.Pressed(keynum) Then
            Case keynum Of
               1: 
                  ShowHelp('Imag&Mul.HLP', 'FileHelp');  {this should be help for Multipole Efield}
               3: 
                  EndAngles := TRUE;
            End;  {End Case}
      Until EndAngles;
   End;

   Procedure DisplayMoment;
   Begin
      SelectViewPort(1);
      contour.init;
      Case MEDisplayMode Of
         1: 
            contour.DrawFunction(Potential);
         2: 
            DisplayEFieldME;
         3: 
         Begin
            contour.DrawFunction(Potential);
            DisplayEFieldME;
         End;
      End;  {End Case}
      ClearMKBuffers;
      WindowOutline;
   End;

   Procedure DrawMESurface;
      Var
         inc: integer;  {incremental for drawing dashed conducting boundaries}
         StartAngle, EndAngle: Word;
   Begin
      SelectViewPort(1);
      SetColor(ConductorColor);
      Case MESurface Of
         1: 
         Begin                              {Cylinder}
            PlotLine(-5, 10, 5, 10);
            PlotLine(-5, 10, -5, -10);
            PlotLine(-5, -10, 5, -10);
            PlotLine(5, -10, 5, 10);
         End;
         2: 
         Begin                              {Split sphere I}
            For inc := 1 To 12 Do
            Begin
               SetColor(ConductorColor);
               StartAngle := 15 * (inc - 1);
               EndAngle := 15 * inc - 7;
               Ellipse(207, 204, StartAngle, EndAngle, 42, 41);
               SetColor(PosColor);
               StartAngle := EndAngle;
               EndAngle := 15 * inc;
               Ellipse(207, 204, StartAngle, EndAngle, 42, 41);
            End;  {End FOR}
            For inc := 13 To 24 Do
            Begin
               SetColor(ConductorColor);
               StartAngle := 15 * (inc - 1);
               EndAngle := 15 * inc - 7;
               Ellipse(207, 204, StartAngle, EndAngle, 42, 41);
               SetColor(NegColor);
               StartAngle := EndAngle;
               EndAngle := 15 * inc;
               Ellipse(207, 204, StartAngle, EndAngle, 42, 41);
            End;  {End FOR}
            SetColor(ConductorColor);
            SetLineStyle(3, 0, 1);
            PlotLine(5, 0, -5, 0);
            SetLineStyle(0, 0, 1)
         End;
         3: 
         Begin                              {Sphere off center}
            Ellipse(207, 163, 0, 360, 82, 81);
         End;
         4: 
         Begin                              {Split sphere II}
            For inc := -2 To 2 Do
            Begin
               SetColor(ConductorColor);
               StartAngle := 15 * (inc - 1);
               EndAngle := 15 * inc - 7;
               Ellipse(207, 204, StartAngle, EndAngle, 42, 41);
               SetColor(PosColor);
               StartAngle := EndAngle;
               EndAngle := 15 * inc;
               Ellipse(207, 204, StartAngle, EndAngle, 42, 41);
            End;  {End FOR}
            For inc := 11 To 14 Do
            Begin
               SetColor(ConductorColor);
               StartAngle := 15 * (inc - 1);
               EndAngle := 15 * inc - 7;
               Ellipse(207, 204, StartAngle, EndAngle, 42, 41);
               SetColor(PosColor);
               StartAngle := EndAngle;
               EndAngle := 15 * inc;
               Ellipse(207, 204, StartAngle, EndAngle, 42, 41);
            End;  {End FOR}
            For inc := 3 To 10 Do
            Begin
               SetColor(ConductorColor);
               StartAngle := 15 * (inc - 1);
               EndAngle := 15 * inc - 7;
               Ellipse(207, 204, StartAngle, EndAngle, 42, 41);
               SetColor(NegColor);
               StartAngle := EndAngle;
               EndAngle := 15 * inc;
               Ellipse(207, 204, StartAngle, EndAngle, 42, 41);
            End;  {End FOR}
            For inc := 15 To 22 Do
            Begin
               SetColor(ConductorColor);
               StartAngle := 15 * (inc - 1);
               EndAngle := 15 * inc - 7;
               Ellipse(207, 204, StartAngle, EndAngle, 42, 41);
               SetColor(NegColor);
               StartAngle := EndAngle;
               EndAngle := 15 * inc;
               Ellipse(207, 204, StartAngle, EndAngle, 42, 41);
            End;  {End FOR}
            SetColor(ConductorColor);
            SetLineStyle(3, 0, 1);
            PlotLine(-4.3301, 2.5, 4.3301, 2.5);
            PlotLine(-4.3301, -2.5, 4.3301, -2.5);
            SetLineStyle(0, 0, 1)
         End;
         5: 
         Begin                            {Cone of apex angle 40 degrees}
            PlotLine(-7.1407, -5, 7.1407, -5);
            PlotLine(7.1407, -5, 0, 14.619);
            PlotLine(0, 14.619, -7.1407, -5);
         End;
         6: 
         Begin                            {Spool}
            PlotLine(-7.5, -5, 7.5, -5);
            PlotLine(7.5, -5, 7.5, -2.5);
            PlotLine(7.5, -2.5, 5, -2.5);
            PlotLine(5, -2.5, 5, 2.5);
            PlotLine(5, 2.5, 7.5, 2.5);
            PlotLine(7.5, 2.5, 7.5, 5);
            PlotLine(7.5, 5, -7.5, 5);
            PlotLine(-7.5, 5, -7.5, 2.5);
            PlotLine(-7.5, 2.5, -5, 2.5);
            PlotLine(-5, 2.5, -5, -2.5);
            PlotLine(-5, -2.5, -7.5, -2.5);
            PlotLine(-7.5, -2.5, -7.5, -5);
         End;
      End;
   End;

   Procedure UpdateMEDisplay;
   Begin
      HideMouse;
      OpenViewPort(1);
      SetBkColor(8);
      MultipoleMenu.display;
      ShowMouse;
      ;
      If AutoDisplay Or AutoDisplayOverride Then
      Begin
         AutoDisplayOverride := FALSE;
         DisplayMoment;
      End;
      DrawMESurface;
      DrawMERadioButtons;
      Sliders.done;
      DefineSliders;
      Sliders.drawall;
   End;

   Procedure SetMomentsKeyboard;
 {displays the input screen, letting the user change the multipole moments}
{
chosen and their amplitudes.}
 {The global variables are adjusted accordingly.}
      Var
         inc: integer; {incremental}
         MomentInputScreen: TInputScreen;
   Begin
      {new(MomentInputScreen);}   {&Turn on new() for Mac version}
      With MomentInputScreen Do
      Begin
         Init;
         DefineInputPort(0.2, 0.8, 0.1, 0.9);
       {ClearMKBuffers;}
         LoadLine('');
         LoadLine(' Multipole     Amplitude   On/Off ');
         LoadLine(' Monopole       {  0.0}     #F   ');
         LoadLine(' Dipole         { +1.0}     #T   ');
         LoadLine(' Quadrupole     {  0.0}     #F   ');
         LoadLine(' Octupole       {  0.0}     #F   ');
         LoadLine(' Hexadecapole   {  0.0}     #F   ');
         LoadLine(' 32-pole        {  0.0}     #F   ');
         LoadLine('');
         LoadLine('  Use Tab and Arrow Keys or Mouse to      ');
         LoadLine('  select fields, and keyboard to change   ');
         LoadLine('  their values.                           ');
         LoadLine('                                          ');
         LoadLine('  Any key or Mouse toggles check boxes.   ');
         LoadLine('                                          ');
         LoadLine('      [  Ok  ]        [Cancel]            ');
         LoadLine('');
         For inc := 1 To MaxMoments Do      {Enter current values in fields}
         Begin
            If MomentOn[inc] = TRUE Then
            Begin
               SetBoolean(2 * inc, TRUE);
               SetNumber(2 * inc - 1, MomentStrength[inc]);
            End
            Else
            Begin
               SetBoolean(2 * inc, FALSE);
               SetNumber(2 * inc - 1, 0);
            End; {End IF}
         End; {End FOR}
         AcceptScreen;   {let the user change values}
         If Canceled = FALSE Then
         Begin
            For inc := 1 To MaxMoments Do
            Begin
               MomentStrength[inc] := GetNumber(2 * inc - 1);
               MomentOn[inc] := GetBoolean(2 * inc);
            End;   {End For}
         End;  {End If}
         Done;
      End;  {End With}
      UpdateMEDisplay;
      {dispose(MomentInputScreen);}   {& Turn on dispose() for Mac version}
   End;

   Procedure ResetMoments;
      Var
         inc: integer;  {incremental}
   Begin
      For inc := 1 To MaxCharges Do
      Begin
         MomentStrength[inc] := 0;
         MomentOn[inc] := FALSE;
      End;
      SelectedMoment := 2;
      MomentOn[2] := TRUE;
      MomentStrength[2] := 1;
   End;

 {These are the procedures for Image Charge Mode: }

   Procedure DrawICRadioButtons;
      Var
         inc: integer; {incremental}
         PrChr: char;
   Begin
      HideMouse;
      SetColor(WindowColor);
      OpenViewPort(6);
      SelectScale(3);
      SetColor(whitte);
      Print(1, 1, 'click radio buttons or press');
      Print(1, 2, '1-6 to select a new charge');
      For inc := 1 To MaxCharges Do
      Begin
         SelectViewPort(6);
         SetColor(whitte);
         PrChr := inttochar(inc);
         PlotSymbol(inc, 0.36, PrChr);
         SelectViewPort(inc + 6);
         SetColor(ButtonColor);
         SetFillStyle(0, ButtonColor);
         FillEllipse(17, 32, 9, 9);
         If ChargeOn[inc] Then
            Print(2, 3, 'ON')
         Else
            Print(1, 3, 'OFF');
         If inc = SelectedCharge Then
         Begin
            SetFillStyle(1, ButtonColor);
            FillEllipse(17, 32, 6, 6);
         End;
      End; {End FOR}
      SelectScale(1);
      ShowMouse;
      ;
   End;

   Procedure DrawCharges;
      Var
         inc: integer; {incremental}
         CircleX, CircleY: integer;
         NumChar: char;

   Begin
      HideMouse;
      SelectViewport(1);
      For inc := 1 To MaxCharges Do
      Begin
         If ChargeOn[inc] Then
         Begin
            If ChargeStrength[inc] > 0 Then
            Begin
               SetColor(PosColor);
               SetFillStyle(1, PosColor);
            End
            Else
            Begin
               SetColor(NegColor);
               SetFillStyle(1, NegColor);
            End;
            If inc = SelectedCharge Then
               SetColor(SelectionColor);
            CircleX := Round(207 + 8.28 * ChargePts[inc].X);
            CircleY := Round(204 - 8.16 * ChargePts[inc].Y);
            FillEllipse(CircleX, CircleY, 8, 8);
            SetColor(blakk);
            NumChar := inttochar(inc);
            PlotSymbol(ChargePts[inc].X, ChargePts[inc].Y, NumChar);
         End;  {End If}
      End;  {End For}
      ShowMouse;
      ;
   End;

   Procedure Efield (Pt: RealPoint;
                           Var EfieldX, EfieldY: real);
      Var
         inc: integer;
         Xdis, Ydis, Distance2, Distance: real;
         PrintStr: String;

   Begin
      EfieldX := 0;
      EfieldY := 0;
      For inc := 1 To MaxCharges Do
      Begin
         If ChargeOn[inc] Then
         Begin
            Xdis := Pt.X - ChargePts[inc].X;
            Ydis := Pt.Y - ChargePts[inc].Y;
            Distance2 := Xdis * Xdis + Ydis * Ydis;
            Distance := sqrt(Distance2);
            EfieldX := EfieldX + ChargeStrength[inc] * Xdis / (Distance * Distance2);
            EfieldY := EfieldY + ChargeStrength[inc] * Ydis / (Distance * Distance2);
         End;
      End;
   End;

   Function NearCharge (Pt: RealPoint;
                           radius: real): integer;
 {checks to see if the point (X,Y) is within a given radius of a point charge}
 {if so, the number of the point charge is given, otherwise zero is returned}
      Var
         inc: integer; {incremental}
         Xdis, Ydis, checkR: real;
   Begin
      NearCharge := 0;
      For inc := 1 To MaxCharges Do
      Begin
         If ChargeOn[inc] Then
         Begin
            Xdis := Pt.X - ChargePts[inc].X;
            Ydis := Pt.Y - ChargePts[inc].Y;
            checkR := Xdis * Xdis + Ydis * Ydis;
            checkR := sqrt(checkR);
            If checkR < radius Then
            Begin
               NearCharge := inc;
               inc := MaxCharges;
            End;  {End If}
         End;  {END If Charge ON}
      End;  {End For}
   End;

 Function InConductor (Pt: RealPoint): Boolean;
 {Returns TRUE if Point is within the currently chosen conductor.}
      Var
         Rsquared: real;
 Begin
      InConductor := FALSE;
      If Conductor = 4 Then
      Begin
         Rsquared := (Pt.X + 10) * (Pt.X + 10) + Pt.Y * Pt.Y;
         If Rsquared <= 100 Then
            InConductor := TRUE;
      End  {End sphere case}
      Else If Pt.X <= 0 Then
         InConductor := TRUE
      Else
      Begin
         Case Conductor Of
            2: 
            Begin
               If Pt.Y <= 0 Then
                  InConductor := TRUE;
            End;
            3: 
            Begin
               If Pt.Y <= Pt.X / 1.7320508 Then
                  InConductor := TRUE;
            End;
            5: 
            Begin
               If Pt.Y <= Pt.X / (-1.7320508) Then
                  InConductor := TRUE;
            End;  {End Case}
         End;
      End;    {End IF}
   End;

Procedure DrawFieldLines (initPt: RealPoint);
Var
   EndLine, PositiveStep              : Boolean;
   Emagnitude, EfieldX, EfieldY, step : real;
   oldPt, newPt, avePt                : RealPoint;
   CheckOverlap, InConductorCounter   : integer;

Begin
   SetColor(FieldLineColor);
   EndLine := FALSE;
   PositiveStep := TRUE;
   step := 0.2;
   oldPt := initPt;
   Repeat
      Efield(oldPt, EfieldX, EfieldY);
      Emagnitude := EfieldX * EfieldX + EfieldY * EfieldY;
      Emagnitude := sqrt(Emagnitude);
      If Emagnitude > 1.0E-8 Then
      Begin
         newPt.X := oldPt.X + step * EfieldX / Emagnitude;
         newPt.Y := oldPt.Y + step * EfieldY / Emagnitude;
         avePt.X := (newPt.X + oldPt.X) / 2;
         avePt.Y := (newPt.Y + oldPt.Y) / 2;
         Efield(avePt, EfieldX, EfieldY);
         Emagnitude := EfieldX * EfieldX + EfieldY * EfieldY;
         Emagnitude := sqrt(Emagnitude);
         newPt.X := oldPt.X + step * EfieldX / Emagnitude;
         newPt.Y := oldPt.Y + step * EfieldY / Emagnitude;
         If (ICDisplayMode <> 2) Or (Inconductor(newPt) = FALSE) Then
            PlotLine(oldPt.X, oldPt.Y, newPt.X, newPt.Y);
         oldPt := newPt;
         If Emagnitude = 0 Then
            EndLine := TRUE;
         If (newPt.X < -50) Or (newPt.X > 50) Or (newPt.Y < -50) Or (newPt.Y > 50) Then
            EndLine := TRUE;
         CheckOverlap := NearCharge(newPt, 0.5);
         If CheckOverlap <> 0 Then
            EndLine := TRUE;
         If EndLine And PositiveStep Then
         Begin                              {when end of first line is reached}
            EndLine := FALSE;
            oldPt := initPt;                  {go back to the initial point}
            PositiveStep := FALSE;          {and now step the other direction}
            step := 0 - step;
         End;
      End
      Else
      Begin
         Endline := True;
         BottomMessage('Field vanishes on part of conducting surface.');
         Delay(1000);
         BottomMessage('');
      End;
   Until EndLine;
End;

Procedure DrawFieldFromLine (angle: real);
{Determines starting points for field lines in either direction}
{at the appropriate spacings from a ray emanating at}
{angle = angle from the center of the screen and calls}
{DrawFieldLines to draw the two lines.}
Var
   EndLine: Boolean;
   initPt: RealPoint;
   EfieldX, EfieldY: real;
   Eflux, EfluxEnough, norm: real;
   keynum: byte;
Begin
   EfluxEnough := 0.5;
   EndLine := FALSE;
   initPt.X := 0.0;
   initPt.Y := 0.0001;
   Repeat
      DrawFieldLines(initPt);
      Eflux := 0.0;
      Repeat                      {Integrate flux}
            initPt.X := initPt.X + 0.1 * cos(angle);
            initPt.Y := initPt.Y + 0.1 * sin(angle);
            Efield(initPt, EfieldX, EfieldY);
            Eflux := Eflux - EfieldX * sin(angle) + EfieldY * cos(angle);
            norm := (initPt.X * initPt.X) + (initPt.Y * initPt.Y);
            If norm > 2500 Then
               EndLine := TRUE;
         Until (sqrt(abs(Eflux)) > EfluxEnough) Or (Endline);
         CheckForEvents;
         If HotKeys.Pressed(keynum) Then
            Case keynum Of
               1: 
                  ShowHelp('Imag&Mul.HLP', 'DisplayHelp');
               3: 
                  Endline := TRUE;
            End;  {End Case}
   Until Endline
End;

Procedure DrawFieldFromSphere;
{Determines starting points for field lines in either direction}
{at the appropriate spacings from the spherical conductor surface}
{and calls DrawFieldLines to draw the two lines.}
Var
         EndLine: Boolean;
         initPt: RealPoint;
         EfieldX, EfieldY, angle: real;
         Eflux, EfluxEnough, norm: real;
         keynum: byte;

   Begin
      keynum := 0;
      EfluxEnough := 0.2;
      EndLine := FALSE;
      initPt.X := 0.0;
      initPt.Y := 0.0001;
      angle := 0.0001;
      Repeat
         DrawFieldLines(initPt);
         Eflux := 0.0;
         Repeat                      {Integrate flux}
            angle := angle + 0.05;
            initPt.X := -10 + 10 * cos(angle);
            initPt.Y := 10 * sin(angle);
            Efield(initPt, EfieldX, EfieldY);
            Eflux := Eflux + EfieldX * cos(angle) + EfieldY * sin(angle);
            norm := initPt.X * initPt.X + initPt.Y * initPt.Y;
            If angle > 3.09 Then
               EndLine := TRUE;
         Until (sqrt(abs(Eflux)) > EfluxEnough) Or (Endline);
         CheckForEvents;
         If HotKeys.Pressed(keynum) Then
            Case keynum Of
               1: 
                  ShowHelp('Imag&Mul.HLP', 'DisplayHelp');
               3: 
                  Endline := TRUE;
            End;  {End Case}
      Until Endline;
      initPt.X := 0.0;
      initPt.Y := 0.0001;
      angle := 0.0001;
      If keynum <> 3 Then
         Repeat
            DrawFieldLines(initPt);
            EndLine := FALSE;
            Eflux := 0.0;
            Repeat                      {Integrate flux}
               angle := angle - 0.05;
               initPt.X := -10 + 10 * cos(angle);
               initPt.Y := 10 * sin(angle);
               Efield(initPt, EfieldX, EfieldY);
               Eflux := Eflux + EfieldX * cos(angle) + EfieldY * sin(angle);
               norm := initPt.X * initPt.X + initPt.Y * initPt.Y;
               If angle < -3.09 Then
                  EndLine := TRUE;
            Until (sqrt(abs(Eflux)) > EfluxEnough) Or (Endline);
            CheckForEvents;
            If HotKeys.Pressed(keynum) Then
               Case keynum Of
                  1: 
                     ShowHelp('Imag&Mul.HLP', 'DisplayHelp');
                  3: 
                     Endline := TRUE;
               End;  {End Case}
         Until Endline
   End;

Procedure DrawConductorLines;
Var
   angle: real;
   keynum: byte;

Begin
      keynum := 0;
      If Conductor = 4 Then
         DrawFieldFromSphere
      Else
      Begin
         DrawFieldFromLine(PI / 2);
         If (HotKeys.Pressed(keynum) = FALSE) Or (keynum <> 3) Then
         Begin   {If Stop Draw hotkey not pressed then draw rest of lines}
            Case Conductor Of
               1: 
                  angle := 0 - PI / 2;
               2: 
                  angle := 0;
               3: 
                  angle := PI / 6;
               5: 
                  angle := 0 - PI / 6;
            End;  {End Case}
            DrawFieldFromLine(angle);
         End;
      End;
   End;

   Procedure FillPolyTP(NumOfPoints : Integer; ArrayOfPoints : PolyType);
   {& this is a shell procedure which can be removed in the Mac version.}
   BEGIN
      FillPoly(NumOfPoints, ArrayOfPoints);
   END;
 
   Procedure DrawConductor;
      Var
         ConductorPoly: PolyType;

   Begin
      ConductorPoly[1].X := 208;
      ConductorPoly[1].Y := 204;
      ConductorPoly[2].X := 208;
      ConductorPoly[2].Y := 0;
      ConductorPoly[3].X := 0;
      ConductorPoly[3].Y := 0;
      ConductorPoly[4].X := 0;
      ConductorPoly[4].Y := 407;
      ConductorPoly[5].X := 415;
      ConductorPoly[5].Y := 407;
      ConductorPoly[6].X := 415;
      ConductorPoly[6].Y := 0;
      SetColor(ConductorColor);
      SelectViewPort(1);
      SetFillStyle(InterleaveFill, ConductorColor);
      Case Conductor Of
         1: 
         Begin    {Vertical Plane}
            ConductorPoly[5].X := 208;
            FillPolyTP(5, ConductorPoly);
         End;
         2: 
         Begin    {90 degrees}
            ConductorPoly[6].Y := 204;
            FillPolyTP(6, ConductorPoly);
         End;
         3: 
         Begin    {60 degrees}
            ConductorPoly[6].Y := 86;
            FillPolyTP(6, ConductorPoly);
         End;
         4: 
         Begin    {sphere}
            FillEllipse(124, 204, 83, 82);
         End;
         5: 
         Begin    {120 degrees}
            ConductorPoly[6].Y := 290;
            FillPolyTP(6, ConductorPoly);
         End;
      End;  {End Case}
   End;

Procedure DisplayImageChargeScreen (Lines: integer);
Begin
   HideMouse;
   SetColor(WindowColor);
   OpenViewport(1);
   Case Lines Of
      1: DrawConductor;
      2: Begin
            DrawConductorLines;
            DrawConductor;
         End;
      3: Begin
            DrawConductor;
            DrawConductorLines;
         End;
      End;  {End Case}
      DrawCharges;
      WindowOutline;                    {redraw outline of window}
      ShowMouse;
   End;

   Procedure DrawCloseUpConductor;
      Var
         ConductorPoly: PolyType;
         realdx, realdy: real;
         deltaX, deltaY: integer;

   Begin
      realdX := 55.5 * (23 + CloseUp.X);  {55.5 pixels per X unit in small box}
      realdY := 54.5 * (23 - CloseUp.Y);  {54.5 pixels per Y unit in small box}
      deltaX := Round(realdx);
      deltaY := Round(realdy);
      ConductorPoly[1].X := 1388 - deltaX;
      ConductorPoly[1].Y := 1363 - deltaY;
      ConductorPoly[2].X := 1388 - deltaX;
      ConductorPoly[2].Y := 0 - deltaY;
      ConductorPoly[3].X := 0 - deltaX;
      ConductorPoly[3].Y := 0 - deltaY;
      ConductorPoly[4].X := 0 - deltaX;
      ConductorPoly[4].Y := 2725 - deltaY;
      ConductorPoly[5].X := 2775 - deltaX;
      ConductorPoly[5].Y := 2725 - deltaY;
      ConductorPoly[6].X := 2775 - deltaX;
      ConductorPoly[6].Y := 0 - deltaY;
      SetColor(ConductorColor);
      SelectViewPort(2);
      SetFillStyle(InterleaveFill, ConductorColor);
      Case Conductor Of
         1: 
         Begin    {Vertical Plane}
            ConductorPoly[5].X := 1388 - deltaX;
            FillPolyTP(5, ConductorPoly);
         End;
         2: 
         Begin    {90 degrees}
            ConductorPoly[6].Y := 1363 - deltaY;
            FillPolyTP(6, ConductorPoly);
         End;
         3: 
         Begin    {60 degrees}
            ConductorPoly[6].Y := 577 - deltaY;
            FillPolyTP(6, ConductorPoly);
         End;
         4: 
         Begin    {sphere}
            FillEllipse(833 - deltaX, 1363 - deltaY, 555, 545);
         End;
         5: 
         Begin    {120 degrees}
            ConductorPoly[6].Y := 2148 - deltaY;
            FillPolyTP(6, ConductorPoly);
         End;
      End;  {End Case}
   End;

   Function GetConductAngle (IntersectPt: RealPoint): real;
   Begin
      GetConductAngle := 90;
      If IntersectPt.X > 0.04 Then
         Case Conductor Of
            2: 
               GetConductAngle := 0;
            3: 
               GetConductAngle := 30;
            5: 
               GetConductAngle := -30;
         End;  {end case}
      If (Conductor = 4) Then
         If (IntersectPt.X <> -10) Then
            GetConductAngle := 90 + ArcTan(IntersectPt.Y / (IntersectPt.X + 10)) * 180 / PI
         Else
            GetConductAngle := 0;
   End;

   Function GetIntersectAngle (Var ConductAngle: real;
                           LineAngle: real): real;
      Var
         TempAngle: real;

   Begin
      TempAngle := ConductAngle - LineAngle;
      If TempAngle > 180 Then
      Begin
         ConductAngle := ConductAngle - 180;
         TempAngle := TempAngle - 180;
      End;
      If TempAngle > 90 Then
      Begin
         ConductAngle := ConductAngle - 180;
         TempAngle := TempAngle - 180;
      End
      Else If TempAngle < -90 Then
      Begin
         ConductAngle := ConductAngle + 180;
         TempAngle := TempAngle + 180;
      End;
      If TempAngle < 0 Then
         TempAngle := 0 - TempAngle;
      GetIntersectAngle := TempAngle;
   End;

Procedure DrawAngle (IntersectPt: RealPoint);
Var
         LinePt, ConductPt, AnglePt: RealPoint;
         LineAngle, ConductAngle, IntersectAngle: real;
         EfieldX, EfieldY, Emagnitude, CLdis, IAdis: real;
         PrStr: String;

Begin
   If IntersectPt.X = 100 Then    {field line does not intersect conductor}
   Begin
      SetColor(WindowColor);
      OpenViewPort(5);
      SetColor(TextColor);
      Print(1, 1, 'no intersection');
   End
   Else
   Begin
      Efield(IntersectPt, EfieldX, EfieldY);
      Emagnitude := EfieldX * EfieldX + EfieldY * EfieldY;
      Emagnitude := sqrt(Emagnitude);
      If Emagnitude <> 0 Then
      Begin
         If EfieldX <> 0 Then
         Begin
            LineAngle := ArcTan(EfieldY / EfieldX);
            LineAngle := LineAngle * 180 / PI;
         End
         Else If EfieldY < 0 Then
            LineAngle := 270
         Else
            LineAngle := 90;
         SetColor(CloseUpBoxColor);
         LinePt.X := IntersectPt.X + 0.25 * EfieldX / Emagnitude;
         LinePt.Y := IntersectPt.Y + 0.25 * EfieldY / Emagnitude;
         If InConductor(LinePt) Then
         Begin
            LinePt.X := IntersectPt.X - 0.25 * EfieldX / Emagnitude;
            LinePt.Y := IntersectPt.Y - 0.25 * EfieldY / Emagnitude;
            If LineAngle < 0 Then
               LineAngle := LineAngle + 180
            Else
               LineAngle := LineAngle - 180;
         End;
         ConductAngle := GetConductAngle(IntersectPt);
         IntersectAngle := GetIntersectAngle(ConductAngle, LineAngle);
         ConductPt.X := IntersectPt.X + 0.25 * Cos(PI * ConductAngle / 180);
         ConductPt.Y := IntersectPt.Y + 0.25 * Sin(PI * ConductAngle / 180);
         AnglePt.X := ConductPt.X + LinePt.X - IntersectPt.X;
         AnglePt.Y := ConductPt.Y + LinePt.Y - IntersectPt.Y;
         CLdis := distance(ConductPt, LinePt);
         IAdis := distance(IntersectPt, AnglePt);
         If CLdis > IAdis Then
         Begin
            ConductPt.X := IntersectPt.X - 0.25 * Cos(PI * ConductAngle / 180);
            ConductPt.Y := IntersectPt.Y - 0.25 * Sin(PI * ConductAngle / 180);
            AnglePt.X := ConductPt.X + LinePt.X - IntersectPt.X;
            AnglePt.Y := ConductPt.Y + LinePt.Y - IntersectPt.Y;
         End;
         PlotLine(IntersectPt.X, IntersectPt.Y, ConductPt.X, ConductPt.Y);
         PlotLine(IntersectPt.X, IntersectPt.Y, LinePt.X, LinePt.Y);
         PlotLine(AnglePt.X, AnglePt.Y, ConductPt.X, ConductPt.Y);
         PlotLine(AnglePt.X, AnglePt.Y, LinePt.X, LinePt.Y);
         SetColor(WindowColor);
         OpenViewPort(5);
         SetColor(TextColor);
         Print(1, 1, ' intersect angle:');
         PrStr := NumStr(IntersectAngle, 3, 0);
         Print(19, 1, PrStr);
         Print(23, 1, 'deg.');
      End
      Else
      Begin
         SetColor(WindowColor);
         OpenViewPort(5);
         SetColor(TextColor);
         Print(1, 1, 'Field vanishes in closeup box.');
      End;
   End;  {End Else}
End;

Procedure CloseUpFieldLine (Var IntersectPt: RealPoint);
Var
         PositiveStep, EndLine, InConduct: boolean;
         Emagnitude, EfieldX, EfieldY, step: real;
         OldPt, NewPt, AvePt: RealPoint;
         CheckOverlap: integer;

Begin
   IntersectPt.X := 100;         {check to see if this changes later}
   SetColor(FieldLineColor);
   EndLine := FALSE;
   PositiveStep := TRUE;
   InConduct := InConductor(CloseUp);
   step := 0.03;                {about 1 pixel in Close up window}
   OldPt := CloseUp;            {start at center point in window}
   OldPt.Y := OldPt.Y + 0.0001; {shift a bit to avoid zeros}
   NewPt := CloseUp;
   Repeat
      Efield(oldPt, EfieldX, EfieldY);
      Emagnitude := EfieldX * EfieldX + EfieldY * EfieldY;
      Emagnitude := sqrt(Emagnitude);
      If Emagnitude > 1.0E-8 Then
      Begin
         If EfieldX <> 0 Then
         Begin
            newPt.X := oldPt.X + step * EfieldX / Emagnitude;
            newPt.Y := oldPt.Y + step * EfieldY / Emagnitude;
            avePt.X := (newPt.X + oldPt.X) / 2;
            avePt.Y := (newPt.Y + oldPt.Y) / 2;
            Efield(avePt, EfieldX, EfieldY);
            Emagnitude := EfieldX * EfieldX + EfieldY * EfieldY;
            Emagnitude := sqrt(Emagnitude);
            newPt.X := oldPt.X + step * EfieldX / Emagnitude;
            newPt.Y := oldPt.Y + step * EfieldY / Emagnitude;
            If (InConductor(newPt) <> InConduct) Then
            Begin      {the field line crosses the boundary of the conductor}
               If InConduct Then       {line is passing out of the conductor}
               Begin
                  IntersectPt := newPt;    {get point just outside conductor}
                  InConduct := FALSE                {flip value of InConduct}
               End
               Else                      {line is passing into the conductor}
               Begin
                  IntersectPt := oldPt;        {point just outside conductor}
                  InConduct := TRUE;                {flip value of InConduct}
               End;
            End;
            If (ICDisplayMode <> 2) Or (InConduct = FALSE) Then
            Begin       {Display option 2 draws lines only outside conductor}
               PlotLine(oldPt.X, oldPt.Y, newPt.X, newPt.Y);
            End;
            oldPt := newPt;
         End
         Else
            EndLine := TRUE;
      End
      Else
         EndLine := TRUE;
      If (newPt.X < (CloseUp.X - 2)) Or (newPt.X > (CloseUp.X + 2)) Or (newPt.Y < (CloseUp.Y - 2))
                   Or (newPt.Y > (CloseUp.Y + 2)) Then
         EndLine := TRUE;   {If outside of box}
      CheckOverlap := NearCharge(newPt, 0.5);
      If CheckOverlap <> 0 Then
         EndLine := TRUE;
      If EndLine And PositiveStep Then   {half the line is now finished}
      Begin                          {when end of first half is reached}
         EndLine := FALSE;              {get ready to do the other half}
         oldPt := CloseUp;                {go back to the initial point}
         PositiveStep := FALSE;          {and now step the other direction}
         step := 0 - step;
         InConduct := InConductor(CloseUp);   {reset InConduct as well}
      End;
   Until EndLine;
End;

Procedure DrawCloseUpBox (a, b: PointType); FAR; {& Comment out FAR for Mac version}
      Var
         ScalePix: Integer;
   Begin
      ScalePix := round(GetMaxX * 16.32 / 640);
      Rectangle(b.x - ScalePix, b.y - ScalePix, b.x + ScalePix, b.y + ScalePix);
End;

Procedure DisplayCloseUp;
Var
         Left, Right, Top, Bottom: Real;
         IntersectPt: RealPoint;
         CloseUpPix: PointType;         {CloseUp position in pixel units}

Begin
      Left := CloseUp.X - 2;
      Right := CloseUp.X + 2;
      Top := CloseUp.Y + 2;
      Bottom := CloseUp.Y - 2;
      HideMouse;
      SelectViewPort(1);               {Draw CloseUp window in main viewport}
      SelectScale(1);
      SetColor(CloseUpBoxColor);
      Map(CloseUp.X, CloseUp.Y, CloseUpPix.X, CloseUpPix.Y);
      DrawCloseUpBox(CloseUpPix, CloseUpPix);
      SetColor(WindowColor);
      DefineScale(2, Left, Right, Bottom, Top);
      OpenViewPort(2);
      SelectScale(2);
      If ICDisplayMode = 2 Then
      Begin
         CloseUpFieldLine(IntersectPt);
         DrawCloseUpConductor;
      End
      Else
      Begin
         DrawCloseUpConductor;
         CloseUpFieldLine(IntersectPt);
      End;
      DrawAngle(IntersectPt);
      SelectViewPort(2);
      SelectScale(1);
      WindowOutline;
      ShowMouse;
      ;
   End;

{&dollarsign&S Stuff1}    {replace &dollarsign& with the actual symbol}

Procedure UpdateICDisplay;  {redraws the IC screen}
Var
         Lines: Integer;  {determines whether to draw field}
Begin
      ImageChargeMenu.display;
      Lines := 1;
      If AutoDisplay Or AutoDisplayOverride Then
         Lines := ICDisplayMode;
      AutoDisplayOverride := FALSE;   {Must reset override}
      DisplayImageChargeScreen(Lines);
      DisplayCloseUp;
      DrawICRadioButtons;
      Sliders.done;
      DefineSliders;
      Sliders.drawall;
      ShowMouse;
End;

Procedure SpecifyArbitraryLine;
      Var
         MousePt, initialPt, finalPt: RealPoint;
         angle, COSAngle, SINAngle: real;
         inside: boolean;
         inc: integer;
   Begin
      SelectViewPort(1);
      SelectScale(1);
      ClearMKBuffers;
      ShowMouse;
      ;
      BottomMessage(' Click mouse to designate a line from the center.');
      Repeat
         CheckForEvents;
      Until Event.MouseClicked Or Event.KeyPressed;
      If Event.MouseClicked Then
      Begin
         MousePosn(MousePt.X, MousePt.Y, 1, 1, inside);
         If inside Then
         Begin
            If (Abs(MousePt.X) = 0.001) Then
            Begin
               angle := -0.5 * PI;
               If (MousePt.Y > 0) Then
                  angle := 0.5 * PI;
            End
            Else If (MousePt.X < 0) Then
            Begin
               If (MousePt.Y < 0) Then
                  angle := ArcTan(MousePt.Y / MousePt.X) - PI
               Else
                  angle := ArcTan(MousePt.Y / MousePt.X) + PI
            End
            Else
               angle := ArcTan(MousePt.Y / MousePt.X);
         End;
      End;
      HideMouse;
      COSAngle := cos(angle);
      SINAngle := sin(angle);
      SetColor(ConductorColor);      {Draw dashed line from which field}
      For inc := 1 To 37 Do          {  lines are to be drawn.         }
      Begin
         finalPt.X := inc * COSAngle;
         finalPt.Y := inc * SINAngle;
         initialPt.X := (inc - 0.5) * COSAngle;
         initialPt.Y := (inc - 0.5) * SINAngle;
         PlotLine(initialPt.X, initialPt.Y, finalPt.X, finalPt.Y);
      End;        {End FOR}
      DrawFieldFromLine(angle);
      DrawCharges;
      BottomMessage(' ');    {clears message window}
      ClearMKBuffers;
      WindowOutline;                    {redraw outline of window}
      ShowMouse;
      ;
   End;

FUNCTION min(Var R1, R2 :real): real;   {compares reals R1 and R2 and returns the smaller}
BEGIN
   min := R1;
   If R2 < R1 Then
      min := R2;
END;

Function DistanceToLine(Var ChargePos : RealPoint; m, b : Real): real;
{Determines distance from point ChargePos to line with slope m and int. b.}
Begin
   DistanceToLine := sqrt(sqr(ChargePos.Y - m*ChargePos.X -b)/(1+m*m));
End;

FUNCTION CheckOnConductor(Var ChargePos : RealPoint): Boolean;
VAR
   tolerance : real;   {closest allowed distance from conducting surfaces}
   distance, distance2, m  : real;
   ChargePosShift : realpoint;
Begin
   CheckOnConductor := FALSE;
   tolerance := 0.02;
   distance := 2*tolerance;
   Case Conductor Of
      1: distance := abs(ChargePos.X);
      2: Begin
           ChargePosShift.X := ChargePos.X + tolerance;
           ChargePosShift.Y := ChargePos.Y + tolerance;
           If InConductor(ChargePosShift) = FALSE THEN
           Begin
              distance := abs(ChargePos.X);
              distance2 := abs(ChargePos.Y);
              distance := min(distance, distance2);
           End;
         End;
      3: Begin
           ChargePosShift.X := ChargePos.X + tolerance;
           ChargePosShift.Y := ChargePos.Y + sqrt(3)*tolerance/2;
           If InConductor(ChargePosShift) = FALSE THEN
           Begin
              distance := abs(ChargePos.X);
              m := 1/sqrt(3);
              distance2 := DistanceToLine(ChargePos,m,0);
              distance := min(distance, distance2);
           End;
         End;
      4: distance := abs(sqrt(sqr(ChargePos.X + 10) + sqr(ChargePos.Y)) - 10);
      5: Begin
           ChargePosShift.X := ChargePos.X + tolerance;
           ChargePosShift.Y := ChargePos.Y + sqrt(3)*tolerance/2;
           If InConductor(ChargePosShift) = FALSE THEN
           Begin
              distance := abs(ChargePos.X);
              m := -1/sqrt(3);
              distance2 := DistanceToLine(ChargePos,m,0);
              distance := min(distance, distance2);
           End;
         End;
      End;   {End CASE}
   IF distance < tolerance THEN
      CheckOnConductor := TRUE;
End;

Procedure PlaceChargesKeyboard;
{displays the input screen, letting the user change the positions/strengths}
{of the charges and add/remove them by modifying the appropriate fields.}
{The global variables for the charges are then adjusted accordingly.}
Var
         inc, CheckOverlap: integer; {incremental, check for charge overlap}
         Overlap, OnConductor, ThisCharge, Redisplay: Boolean;
         ChargeInputScreen: TInputScreen;
Begin
      {new(ChargeInputScreen);}    {& Turn on new() for Mac version}
   With ChargeInputScreen Do
   Begin
         Init;
         DefineInputPort(0.2, 0.8, 0.1, 0.9);
         Overlap := FALSE;
         Redisplay := FALSE;
         Repeat
            LoadLine('');
            LoadLine('              X       Y    charge   On/Off');
            LoadLine(' Charge 1 { 10.0} {  0.0} {  2.0}     #T   ');
            LoadLine(' Charge 2 {  0.0} {  0.0} {  2.0}     #F   ');
            LoadLine(' Charge 3 {  0.0} {  0.0} {  2.0}     #F   ');
            LoadLine(' Charge 4 {  0.0} {  0.0} {  2.0}     #F   ');
            LoadLine(' Charge 5 {  0.0} {  0.0} {  2.0}     #F   ');
            LoadLine(' Charge 6 {  0.0} {  0.0} {  2.0}     #F   ');
            LoadLine('');
            LoadLine('  Use Tab and Arrow Keys or Mouse to      ');
            LoadLine('  select fields, and keyboard to change   ');
            LoadLine('  their values.                           ');
            LoadLine('                                          ');
            LoadLine('  Any key or Mouse toggles check boxes.   ');
            LoadLine('                                          ');
            LoadLine('      [  Ok  ]        [Cancel]            ');
            LoadLine('');
            For inc := 1 To MaxCharges Do      {Enter current values in fields}
            Begin
               If ChargeOn[inc] = TRUE Then
               Begin
                  SetBoolean(4 * inc, TRUE);
                  SetNumber(4 * inc - 3, ChargePts[inc].X);
                  SetNumber(4 * inc - 2, ChargePts[inc].Y);
                  SetNumber(4 * inc - 1, ChargeStrength[inc]);
               End
               Else
               Begin
                  SetBoolean(4 * inc, FALSE);
               End; {End IF}
            End; {End FOR}
            AcceptScreen;   {let the user change values}
            If Canceled = FALSE Then
            Begin
               Redisplay := TRUE;
               For inc := 1 To MaxCharges Do
               Begin
                  ChargePts[inc].X := GetNumber(4 * inc - 3);
                  ChargePts[inc].Y := GetNumber(4 * inc - 2);
                  ChargeStrength[inc] := GetNumber(4 * inc - 1);
                  ChargeOn[inc] := GetBoolean(4 * inc);
               End;
               Overlap := FALSE;  {now check to see if there are any overlaps}
               For inc := 1 To MaxCharges Do
               Begin
                  ThisCharge := False;
                  CheckOverlap := NearCharge(ChargePts[inc], 2);
                  If (CheckOverlap <> 0) And (ChargeOn[CheckOverlap] = TRUE) And (ChargeOn[inc] = TRUE)
                          And (inc <> CheckOverlap) Then
                     ThisCharge := TRUE;
                     If ThisCharge Then
                     Begin
                        Overlap := TRUE;
                        ChargeOn[inc] := FALSE;
                     End;
               End; {End For}
               If Overlap = TRUE Then
               Begin
                  beep;
                  Announce(' The charges cannot overlap each other - please reinput');
               End;
               OnConductor := FALSE; {now check to see if any charges are
                                      too close to the conducting surfaces.}
               For inc := 1 To MaxCharges Do
               Begin
                  If ChargeOn[inc] = TRUE  Then
                     ThisCharge := CheckOnConductor(ChargePts[inc]);
                     If ThisCharge Then
                     Begin
                        ChargeOn[inc] := FALSE;
                        OnConductor := TRUE;
                     End;
               End; {End For}
               If OnConductor = TRUE Then
               Begin
                  beep;
                  Announce(' The charges may not be placed on the conducting surface - please reinput');
               End;
            End;
         Until ((Overlap = FALSE) AND (OnConductor = FALSE)) Or Canceled;
         DefineInputPort(0, 1, 0, 1);
         BottomMessage(' ');    {clears bottom message window}
         ShowMouse;
         Done;
   End;
      {dispose(ChargeInputScreen);}     {& turn on dispose() for Mac version}
End;


Procedure PlaceCharge (mousePt: RealPoint);
Var
   inc                     : integer; {incremental}
   ChangeNum, DeleteNum    : integer;
   OnConductor, ThisCharge : boolean;
Begin
   ChangeNum := 0;
    {this next loop checks to see if there are any unused charges}
    {if so, ChangeNum gets the number of the first available charge}
   For inc := 1 To MaxCharges Do
   Begin
      If ChargeOn[inc] = FALSE Then
      Begin
         ChangeNum := inc;
         inc := MaxCharges;
      End; {End IF}
   End; {End FOR}
   OnConductor :=  CheckOnConductor(mousePt); {now check to see if the
                    charge would be too close to the conducting surfaces.}
   If (NearCharge(mousePt, 2) = 0)   {Also check to see if the new charge}
      And (ChangeNum <> 0) And (NOT OnConductor) Then   {would overlap.}
   Begin                                         {add the charge}
      ChargePts[ChangeNum] := mousePt;
      ChargeOn[ChangeNum] := TRUE;
      ChargeStrength[ChangeNum] := charge;
      SelectedCharge := ChangeNum;
      UpdateICDisplay;
   End
   Else {User has clicked close to or on an existing charge}
   Begin
      DeleteNum := NearCharge(mousePt, 1);
      If DeleteNum <> 0 Then         {User has clicked in a charge}
      Begin                           {remove the selected charge}
         ChargeOn[DeleteNum] := FALSE;
         ChargePts[DeleteNum].X := 0;
         ChargePts[DeleteNum].Y := 0;
         ChargeStrength[DeleteNum] := 0;
         UpdateICDisplay;
      End
      Else
      Begin {Error - Charge overlap or max. charges reached.}
         Beep;
         If ChangeNum = 0 Then
            Announce(' Maximum Number of charges reached')
         Else If OnConductor Then
            Announce(' The charges may not be placed on the conducting surface - please reinput')
         Else
            Announce(' Charges may not overlap');
         End;  {End Else (Error)}
      End;  {End Else (close to or on a charge)}
   End;

   Procedure DrawSelectedCharge (a, b: PointType); FAR;  {& comment out FAR for Mac version}
   Begin
      HideMouse;
      If ChargeStrength[SelectedCharge] > 0 Then
         SetFillStyle(1, PosColor)
      Else
         SetFillStyle(1, NegColor);
      SetColor(SelectionColor);
      FillEllipse(b.X, b.Y, 8, 8);
          {SetColor(blakk);}
{
         NumChar := inttochar(SelectedCharge);}
{
         PlotSymbol(ChargePts[inc].X,ChargePts[inc].Y,NumChar);}
      ShowMouse;
      ;
   End;

   Procedure MoveCharge (MousePt: RealPoint);
                                {click and drag selected charge}
      Var
         x, y, button, CheckOverlap: integer;
         ChargeLoc, b: PointType;      {selected charge location in pixel units}
         SelChargePos: Realpoint;
         inside: Boolean;
   Begin
      Message('Drag selected charge to new position and release mouse.');
      DisplayImageChargeScreen(1);
      SelChargePos := ChargePts[SelectedCharge];
      SelectViewport(1);
      SelectScale(1);
      Map(SelChargePos.X, SelChargePos.Y, ChargeLoc.X, ChargeLoc.Y);
      DrawSelectedCharge(ChargeLoc, ChargeLoc);
      mouseGlobalPosn(x, y, button);
      If button <> 0 Then
      Begin
         DrawRubberBand(DrawSelectedCharge, ChargeLoc, b, DarkGray, false);
         MousePosn(mousePt.X, mousePt.Y, 1, 1, inside);
         ChargeOn[SelectedCharge] := false;
         CheckOverlap := NearCharge(mousePt, 2);
         ChargeOn[SelectedCharge] := true;
         If CheckOverlap = 0 Then
         Begin
            If inside Then
               ChargePts[SelectedCharge] := mousePt
            Else
            Begin
               beep;
               BottomMessage('Charges may not be dragged beyond window bounds. Press any key to continue.');
               Repeat
                  CheckForEvents;
               Until Event.MouseClicked Or Event.KeyPressed;
            End;
         End
         Else
         Begin
            beep;
            BottomMessage(' Charges may not overlap; resetting.  Press any key to continue.');
            Repeat
               CheckForEvents;
            Until Event.MouseClicked Or Event.KeyPressed;
         End;
      End;
      Message(' ');
      BottomMessage(' ');
      UpdateICDisplay;
   End;

   Procedure SelectCharge (MousePt: RealPoint);
      Var
         ChargeNum: integer;
   Begin
      ChargeNum := NearCharge(mousePt, 1);
      If ChargeNum <> 0 Then
      Begin
         SelectedCharge := ChargeNum;
         sliders.done;
         DefineSliders;
         sliders.drawall;
         DrawCharges;
      End;
   End;

   Procedure MoveCloseUpBox;
      Var
         EndMovement, inside: Boolean;
         step: real;
         mousePt: RealPoint;
         x, y, button: Integer;
         CloseUpPix, b: PointType;

   Begin
      EndMovement := FALSE;
      step := 0.5;
      BottomMessage(' Use arrow keys or click and drag with mouse to relocate close-up window.');
      Repeat
         DisplayImageChargeScreen(1);
         DisplayCloseUp;
         ClearMKBuffers;
         Repeat
            CheckForEvents;
         Until Event.KeyPressed Or Event.MouseClicked;
         If Event.MouseClicked Then
         Begin
            MousePosn(mousePt.X, mousePt.Y, 1, 1, inside);
            If inside And (abs(mousePt.X - CloseUp.X) <= 2) And (abs(mousePt.Y - CloseUp.Y) <= 2) Then
            Begin
               Message('Press any (non-arrow) key or click outside main screen when done.');
               SelectViewport(1);
               SelectScale(1);
               SetColor(CloseUpBoxColor);
               Map(CloseUp.X, CloseUp.Y, CloseUpPix.X, CloseUpPix.Y);
               mouseGlobalPosn(x, y, button);
               If button <> 0 Then
               Begin
                  DrawRubberBand(DrawCloseUpBox, CloseUpPix, b, DarkGray, true);
                  MousePosn(mousePt.X, mousePt.Y, 1, 1, inside);
               End;
               CloseUp := mousePt;
            End
            Else If inside Then
            Begin
               Message('Click on close-up window and drag it to desired location or use arrow keys.');
               Beep;
            End
            Else {click was not inside viewport}
               EndMovement := TRUE;
         End
         Else { a key was pressed}
         Begin
         CASE Event.ReadKey OF
            #72: CloseUp.Y := CloseUp.Y + step;   {up arrow pressed}
            #80: CloseUp.Y := CloseUp.Y - step;   {down arrow pressed}
            #75: CloseUp.X := CloseUp.X - step;   {left arrow pressed}
            #77: CloseUp.X := CloseUp.X + step;   {right arrow pressed}
         ELSE
            EndMovement := TRUE;
         END;                                   {End Case}
{&&the following 12 lines replace the preceeding 8 lines for the Mac version}
            {Case ord(Event.ReadKey) Of
               $7E: 
                  CloseUp.Y := CloseUp.Y + step;   {up arrow pressed
               $7D: 
                  CloseUp.Y := CloseUp.Y - step;   {down arrow pressed
               $7B: 
                  CloseUp.X := CloseUp.X - step;   {left arrow pressed
               $7C: 
                  CloseUp.X := CloseUp.X + step;   {right arrow pressed
               Otherwise
                  EndMovement := TRUE;
            End;                                   {End Case}
{&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&}
         End;
         If CloseUp.X > 23 Then
            CloseUp.X := 23;
         If CloseUp.X < -23 Then
            CloseUp.X := -23;
         If CloseUp.Y > 23 Then
            CloseUp.Y := 23;
         If CloseUp.Y < -23 Then
            CloseUp.Y := -23;
      Until EndMovement;
      BottomMessage(' ');  {clears the bottom message window}
      Message(' ');        {clears top message window}
      UpdateICDisplay;
   End;

   Procedure SetChargePosition;
      Var
         inc: integer;
   Begin
      For inc := 1 To MaxCharges Do  {Clear all currently defined charges.}
      Begin
         ChargePts[inc].X := 0;
         ChargePts[inc].Y := 0;
         ChargeStrength[inc] := 0;
         ChargeOn[inc] := FALSE;
      End;      {End FOR}
      ChargeOn[1] := TRUE;       {Turn on just charge #1}
      ChargeStrength[1] := 2.0;  {charge 1 is selected, with strength 2}
      SelectedCharge := 1;
      sliders.done;
      DefineSliders;
      sliders.drawall;
      Case Conductor Of
         1: 
         Begin
            ChargePts[1].X := 10;        {Location for conductor option 1}
            ChargePts[1].Y := 0;
         End;
         2: 
         Begin
            ChargePts[1].X := 7.1;      {Location for conductor option 2}
            ChargePts[1].Y := 7.1;
         End;
         3: 
         Begin
            ChargePts[1].X := 5.0;      {Location for conductor option 3}
            ChargePts[1].Y := 8.7;
         End;
         4: 
         Begin
            ChargePts[1].X := 10.0;      {Location for conductor option 4}
         End;
         5: 
         Begin
            ChargePts[1].X := 8.7;      {Location for conductor option 5}
            ChargePts[1].Y := 5.0;
         End;
      End;   {End CASE}
   End;

 {These are procedures for both modes: }

   Procedure RedisplayScreen;
   Begin
      HideMouse;
      ClearAll;
      DisplayScreen;
      Case SimulationMode Of
         1: UpdateICDisplay;
         2: UpdateMEDisplay;
      End;
      ShowMouse;
      ;
   End;

   Procedure FieldProbe;
      Var
         MousePt, PrevMousePt, ArrowPt: RealPoint;
         EfieldX, EfieldY, Emag, Edir, Poten, scale: real;
         inside, EndFieldProbe, MouseMoved: boolean;
         PrStr: String;
         keynum: byte;
         long: longint;
   Begin
      EndFieldProbe := FALSE;
      Message(' Field Probe  (Click outside window or press F10 to return to main menu.) ');
      OpenViewPort(5);
      SetColor(TextColor);
      Print(1, 1, ' E-field and Potential Data');
      BottomMessage(' Window gives field info at mouse position; click to get E-field arrow.');
      Repeat
         ClearMKBuffers;
         Repeat
            MouseMoved := FALSE;
            Delay(500);    {& replace with Delay(10, long); for Mac version}
            CheckForEvents;
            MousePosn(mousePt.X, mousePt.Y, 1, 1, inside);
            If inside And ((mousePt.X <> PrevMousePt.X) Or (mousePt.Y <> PrevMousePt.Y)) Then
            Begin
               PrevMousePt := mousePt;
               MouseMoved := TRUE;
            End;
         Until Event.KeyPressed Or Event.MouseClicked Or MouseMoved;
         If Event.KeyPressed Then   {key was pressed instead of mouse clicked}
         Begin
            If HotKeys.pressed(keynum) Then
            Begin
               Case keynum Of
                  1: 
                     ShowHelp('Imag&Mul.HLP', 'FieldProbeHelp');
                  2: 
                  Begin
                     AutoDisplayOverride := TRUE;
                     RedisplayScreen;
                  End;
                  5: 
                     EndFieldProbe := TRUE;
               End;  {End Case}
            End
            Else
               EndFieldProbe := True;
         End
         Else                            {Mouse clicked or no event}
         Begin
            If inside Then
            Begin
               If SimulationMode = 2 Then
               Begin
                  scale := 1000;
                  EfieldME(mousePt, EfieldX, EfieldY)
               End
               Else
               Begin
                  scale := 10;
                  EField(mousePt, EfieldX, EfieldY);
               End;
               EfieldX := EfieldX * scale;
               EfieldY := EfieldY * scale;
               Emag := EfieldX * EfieldX + EfieldY * EfieldY;
               Emag := sqrt(Emag);
               If EfieldX <> 0 Then
                  Edir := 180 * ArcTan(EfieldY / EfieldX) / PI
               Else If EfieldY > 0 Then
                  Edir := 90
               Else
                  Edir := -90;
               If EfieldX < 0 Then
                  If EfieldY > 0 Then
                     Edir := Edir + 180
                  Else
                     Edir := Edir - 180;
               SetColor(WindowColor);
               OpenViewPort(2);
               SetColor(TextColor);
               Print(1, 1, ' Position:');       {Print info to window}
               Print(1, 2, '  X : ');
               PrStr := NumStr(MousePt.X, 3, 1);
               Print(7, 2, PrStr);
               Print(1, 3, '  Y : ');
               PrStr := NumStr(MousePt.Y, 3, 1);
               Print(7, 3, PrStr);
               Print(1, 4, ' Electric field components : ');
               Print(1, 5, '  X : ');
               PrStr := NumStr(EfieldX, 3, 1);
               Print(7, 5, PrStr);
               Print(1, 6, '  Y : ');
               PrStr := NumStr(EfieldY, 3, 1);
               Print(7, 6, PrStr);
               Print(1, 7, ' magnitude : ');
               PrStr := NumStr(Emag, 5, 2);
               Print(14, 7, PrStr);
               Print(1, 8, ' direction : ');
               PrStr := NumStr(Edir, 4, 0);
               Print(14, 8, PrStr);
               Print(20, 8, 'degrees');
               If SimulationMode = 2 Then
               Begin
                  Poten := Potential(MousePt.X, MousePt.Y);
                  Print(1, 9, ' Potential : ');
                  PrStr := NumStr(Poten, 3, 2);
                  Print(14, 9, PrStr);
                  Print(19, 9, 'Volts');
               End;
               If Event.MouseClicked Then
               Begin
                  arrowPt.X := mousePt.X + EfieldX;
                  arrowPt.Y := mousePt.Y + EfieldY;
                  SetColor(9);
                  SelectViewPort(1);
                  DrawArrow(MousePt, arrowPt);
               End;
            End
            Else        {click was not inside viewport}
               EndFieldProbe := TRUE;
         End;
      Until EndFieldProbe;
      BottomMessage(' ');          {clears Bottom Message Window}
   End;

   Procedure SliderHandler;
   Begin
      Repeat
         CheckForEvents;                {wait until slider stops moving}
      Until sliders.changed = FALSE;
      If SimulationMode = 2 Then
      Begin
         multi := sliders.value(1);
         MomentStrength[SelectedMoment] := multi;
         If abs(multi) < 0.09 Then
         Begin
            MomentOn[SelectedMoment] := FALSE;
            MomentStrength[SelectedMoment] := 0;
         End
         Else
         Begin
            MomentOn[SelectedMoment] := TRUE;
         End;
         UpdateMEDisplay;
      End
      Else
      Begin
         charge := sliders.value(1);
         ChargeStrength[SelectedCharge] := charge;
         UpdateICDisplay;
      End;
   End;

   Procedure MultipoleMenuHandler;
   Begin
      Case MultipoleMenu.colchosen Of
         1: 
            Case MultipoleMenu.rowchosen Of
               1: AboutCUPS;
               2: Help(Credits);
               3: Configuration;
               4: SimulationMode := 0;
            End;
         2: Case MultipoleMenu.rowchosen Of
               1: SimulationMode := 1;          {Image charge mode}
               2: SimulationMode := 2;          {Multipole mode}
            End;
         3: Begin
            MEDisplayMode := MultipoleMenu.rowchosen;
            UpdateMEDisplay;
            End;
         4: 
            Case MultipoleMenu.rowchosen Of
               1: 
               Begin
                  If AutoDisplay Then
                  Begin
                     AutoDisplay := FALSE;
                     MultipoleMenu.rowchecked(3, 1, FALSE);
                     ImageChargeMenu.rowchecked(3, 1, FALSE);
                  End
                  Else
                  Begin
                     AutoDisplay := TRUE;
                     MultipoleMenu.rowchecked(3, 1, TRUE);
                     ImageChargeMenu.rowchecked(3, 1, TRUE);
                     DisplayMoment;
                  End;
               End;
               2: 
                  SetMomentsKeyboard;    {Change moments - keyboard chosen}
               3: 
               Begin
                  FieldProbe;
                  UpdateMEDisplay;
               End;
               4: 
                  RedisplayScreen;
            End;                         {End Case}
         5: 
         Begin
            MESurface := MultipoleMenu.rowchosen;
            UpdateMEDisplay;
         End;
         6: 
            Case MultipoleMenu.rowchosen Of
               1: 
                  ShowHelp('Imag&Mul.HLP', 'FileHelp');
               2: 
                  ShowHelp('Imag&Mul.HLP', 'PartsHelp');
               3: 
                  ShowHelp('Imag&Mul.HLP', 'MEDisplayHelp');
               4: 
                  ShowHelp('Imag&Mul.HLP', 'METoolsHelp');
               5: 
               Begin
                  ShowHelp('Imag&Mul.HLP', 'MESurfacesHelp');
                  ShowHelp('Imag&Mul.HLP', 'MESurfacesHelpContinued1');
               End;
               6: 
               Begin
                  ShowHelp('Imag&Mul.HLP', 'MultipoleExpansionHelp');
                  ShowHelp('Imag&Mul.HLP', 'MultipoleExpansionHelpContinued1');
               End;
            End;
      End; {End Case}
   End;

Procedure Multipole;
   Var
         mousePt: RealPoint;
         inside: boolean;
         KeyNum: byte;
   Begin
      UpdateMEDisplay;
      ShowMouse;

      Repeat
         CheckForEvents;
         If sliders.changed Then
            SliderHandler;
         If Event.MouseClicked Then
         Begin
            MousePosn(mousePt.X, mousePt.Y, 6, 3, inside);
            If inside And (mousePt.Y < 0.36) Then
            Begin
               SelectedMoment := Round(MousePt.X);
               DrawMERadioButtons;
               sliders.done;
               DefineSliders;
               sliders.drawall;
            End;
         End;
         If HotKeys.Pressed(keynum) Then
            Case keynum Of
               1: 
               Begin
                  ShowHelp('Imag&Mul.HLP', 'MultipoleExpansionHelp');
                  ShowHelp('Imag&Mul.HLP', 'MultipoleExpansionHelpContinued1');
               End;
               2: 
               Begin
                  AutoDisplayOverride := TRUE;
                  RedisplayScreen;
               End;
               4: 
               Begin
                  ResetMoments;
                  RedisplayScreen;
               End;
               5: 
                  If MultipoleMenu.chosen Then
                     MultipoleMenuHandler;
            End;    {End Case}
         If MultipoleMenu.activated Then
            MultipoleMenuHandler;
      Until (SimulationMode <> 2);  {until change mode or QUIT}
End;

Procedure ImageChargeToolsHandler (rowchosen: integer);
Begin
      Case rowchosen Of
         1: 
         Begin
            If AutoDisplay Then
            Begin
               AutoDisplay := FALSE;
               ImageChargeMenu.RowChecked(3, 1, FALSE);
               MultipoleMenu.rowchecked(3, 1, FALSE);
            End
            Else
            Begin
               ImageChargeMenu.RowChecked(3, 1, TRUE);
               MultipoleMenu.rowchecked(3, 1, TRUE);
               AutoDisplay := TRUE;
               UpdateICDisplay;
            End;
         End;
         2: 
         Begin
            PlaceChargesKeyboard;
            UpdateICDisplay;
         End;
         3: 
            MoveCloseUpBox;
         4: 
         Begin
            FieldProbe;
            UpdateICDisplay;
         End;
         5: 
            SpecifyArbitraryLine;
         6: 
            RedisplayScreen;
      End; {End Case}
   End;

   Procedure ImageChargeMenuHandler;
   Begin
      Case ImageChargeMenu.colchosen Of
         1: 
            Case ImageChargeMenu.rowchosen Of
               1: AboutCUPS;
               2: Help(Credits);
               3: Configuration;
               4: SimulationMode := 0;
            End;
         2: Case ImageChargeMenu.rowchosen Of
               1: SimulationMode := 1;             {Image charge}
               2: SimulationMode := 2;             {Multipole}
            End;
         3: Begin
               ICDisplayMode := ImageChargeMenu.rowchosen;
               UpdateICDisplay;
            End;
         4: ImageChargeToolsHandler(ImageChargeMenu.rowchosen);
         5: Begin
               Conductor := ImageChargeMenu.rowchosen;
               SetChargePosition;
               UpdateICDisplay;
            End;
         6: Case ImageChargeMenu.rowchosen Of
               1: ShowHelp('Imag&Mul.HLP', 'FileHelp');
               2: ShowHelp('Imag&Mul.HLP', 'PartsHelp');
               3: ShowHelp('Imag&Mul.HLP', 'DisplayHelp');
               4: Begin
                     ShowHelp('Imag&Mul.HLP', 'ToolsHelp');
                     ShowHelp('Imag&Mul.HLP', 'ToolsHelpContinued1');
                  End;
               5: ShowHelp('Imag&Mul.HLP', 'SurfacesHelp');
               6: Begin
                     ShowHelp('Imag&Mul.HLP', 'ImageChargeHelp');
                     ShowHelp('Imag&Mul.HLP', 'ImageChargeHelpContinued1');
                     ShowHelp('Imag&Mul.HLP', 'ImageChargeHelpContinued2');
                  End;
            End;  {End Case}
      End; {End Case}
   End;

Procedure ImageCharge;
Var
         mousePt: RealPoint;
         inside: boolean;
         keynum: byte;
         x, y, button, chargeNum: integer;
Begin
      UpdateICDisplay;
      ShowMouse;
      Repeat
         CheckForEvents;
         If sliders.changed Then
            SliderHandler;
         If Event.MouseClicked Then
         Begin
            MousePosn(mousePt.X, mousePt.Y, 1, 1, inside);
            If inside Then
            Begin
               If Event.DoubleClicked Then
                  PlaceCharge(MousePt)                {double click - place/remove charge}
               Else
               Begin
                  ChargeNum := NearCharge(MousePt, 1);  {check if click is on a charge}
                  If ChargeNum <> 0 Then
                  Begin
                     SelectedCharge := ChargeNum;
                     MouseGlobalPosn(x, y, button);
                     If button <> 0 Then
                        MoveCharge(MousePt);
                  End;
               End;
            End
            Else
            Begin
               MousePosn(mousePt.X, mousePt.Y, 6, 3, inside);
               If inside Then
               Begin
                  If mousePt.Y < 0.36 Then
                  Begin
                     SelectedCharge := Round(MousePt.X);
                     DrawCharges;
                     DrawICRadioButtons;
                     sliders.done;
                     DefineSliders;
                     sliders.drawall;
                  End;
               End;
            End;
         End;
         If HotKeys.Pressed(keynum) Then
            Case keynum Of
               1: 
               Begin
                  ShowHelp('Imag&Mul.HLP', 'ImageChargeHelp');
                  ShowHelp('Imag&Mul.HLP', 'ImageChargeHelpContinued1');
               End;
               2: 
               Begin
                  AutoDisplayOverride := TRUE;
                  RedisplayScreen;
               End;
               4: 
               Begin
                  SetChargePosition;
                  RedisplayScreen;
               End;
            End;    {End Case}
         If Event.KeyPressed And (event.ReadKey In ['1'..'6']) Then
         Begin
            SelectedCharge := ord(event.ReadKey) - 48;
            sliders.done;
            DefineSliders;
            sliders.drawall;
            DrawCharges;
            DrawICRadioButtons;
         End;
         If ImageChargeMenu.activated Then
            ImageChargeMenuHandler;
      Until (SimulationMode <> 1);  {until change mode or QUIT}
   End;

Begin
   Initialize;
   HideMouse;
   Help(Credits);
   Repeat
      DisplayScreen;
      ShowMouse;
      Case SimulationMode Of
         1: 
            ImageCharge;
         2: 
            Multipole;
      End;
      ClearAll;
   Until (SimulationMode = 0);
   MultipoleMenu.done;
   ImageChargeMenu.done;   {Turn on dispose()'s for Mac version}
   {dispose(MultipoleMenu);
   dispose(ImageChargeMenu);
   dispose(sliders);
   dispose(contour);
   dispose(HotKeys);}
   CUPSdone;
End.