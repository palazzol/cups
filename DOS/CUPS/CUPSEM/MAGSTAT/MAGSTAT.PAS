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

(*  Program Title  : MAGSTAT
Author         : Ron Stoner
Version        : 7
   Program Summary: Computes magnetic fields in and near magnetized
   solids in the shapes of surfaces of revolution.   The user can choose
   from among four different shapes (ellipsoid, toroid cylindrical
   and double-coned), various sizes and aspect ratios.  Two distinct
   cases are treated:
   1) A uniformly magnetized permanent magnet with no external field, and
   2) a homogeneous, linear, isotropic medium in a uniform magnetic field.
   In the former case, the magnetization is parallel to the axis of
   symmetry; in the latter case the externally applied field must
   be parallel to that axis of symmetry, which is the vertical direction
   on the viewport. Algorithm:  In all cases, the magnetic field,
   H is computed via by an approximation that replaces the continuous
   distribution of magnetic charge (i.e. the negative divergence of
   magnetization) at the surface by circular rings of charge.
   The field H is then the superposition of contributions from such
   individual rings.  The contribution from each ring at each point
   in space is an integral over the ring, whose value is
   computed from by Chebyschev polynomial approximants. Once the H field
   is known the other two fields (B and M) can also be computed.
      In the case of a magnetizable medium in an external field, the charges
   on individual rings are computed through a process of "relaxation."
   The equations solved by the relaxation process represent the requirement
   that the magnetic charge on each ring must be self-consistent with the
   induced magnetization produced by both the externally applied field and
   the field of the other rings of magnetic charge.  The relaxation process
   consists of iteratively finding the magnetic charge on each ring due to
   previously computed magnetic charges on other rings until all charges are
   self-consistent. *)

  (******************************************************)
  (**      CUPS Simulation by Ron Stoner               **)
  (**    Physics and Astronomy Department              **)
  (**    Bowling Green State University                **)
  (**    Displays fields of either a permanent         **)
  (**    magnet or a linear magnetizable solid in      **)
  (**    an externally applied magnetic field.         **)
  (**    Last Change 6/29/94                           **)
  (******************************************************)


Program MAGSTAT;

   Uses
      CRT, Graph, CUPSmupp, CUPS, CUPSgrph, CUPSgui, CUPSfunc, CUPSproc;

   Const
      np = 100;
      whitte = white;
      blakk = black;
   Type
      Array32 = Array[1..32] Of REAL;
      RealArrayNP = Array[1..np] Of REAL;
   Var
      FieldColor     : Array[1..3] Of INTEGER;
      Fname          : String[3];
      SimulateName   : Array[1..2] Of String;
      ShapeName      : Array[1..5] Of String;
      CgsUnits       : Array[1..3] Of String;
      MksUnits       : Array[1..3] Of String;
      CgsToMks       : Array[1..3] OF REAL;
      shades         : Array[1..11] Of INTEGER;
      cheb0Lo, cheb0Hi, cheb1Lo, cheb1Hi: Array32;
      cgs            : BOOLEAN;
      CreditsOn      : BOOLEAN;
      GraphingOver   : Boolean;
      Plot3D         : Boolean;
      MyMenu         : TMenu;   (* Pull-down Menu *)
      QuitFlag       : boolean;
      key            : byte;   (* Hotkey variable *)
      HotKeys        : THotKeys;
      MCheb          : INTEGER;
      MaxIter        : INTEGER;   (*Relaxation Iterations *)
      ShapeType      : INTEGER;
      FieldToShow    : INTEGER; (*1->H ; 2->B; 3 ->M *)
      SimulateType   : WORD;
      integral       : REAL;
      Coeff          : DMatrix; (*Used in relaxation calculation *)
      x, y           : DataVector;
      MatHx          : DMatrix;
      MatHy          : DMatrix;
      DisplayMat     : DMatrix;
      DisplayType    : WORD;  (* Type of Field display *)
      MatSize        : Integer; (*Dimension of the above DMatrices *)
      Contours       : TContour;
      ThreeD         : TPlot3D;
      H, D           : REAL; (*Characteristic half-height and radius of solid *)
      scale          : REAL; (*Plotting scale *)
      axerr          : WORD; (* Error flag *)
      Ho, Hfar       : REAL;
      maxr           : REAL;
      Mu, Chi        : REAL;  (*Relative permeability and susceptibility *)
      ds             : REAL;
      sx, sy         : DataVector; (*Points on the surface *)
      nx, ny         : DataVector; (*Unit normal components *)
      sigm           : RealArrayNP; (*Magnetic Charge *)

   Procedure SetArray32b (Var a: Array32;
                           c01, c02, c03, c04, c05, c06, c07, c08,
                           c09, c10, c11, c12, c13, c14, c15, c16: real);
   Begin
      a[1] := c01;
      a[2] := c02;
      a[3] := c03;
      a[4] := c04;
      a[5] := c05;
      a[6] := c06;
      a[7] := c07;
      a[8] := c08;
      a[9] := c09;
      a[10] := c10;
      a[11] := c11;
      a[12] := c12;
      a[13] := c13;
      a[14] := c14;
      a[15] := c15;
      a[16] := c16;
   End;

   Procedure SetArray32e (Var a: Array32;
                           c17, c18, c19, c20, c21, c22, c23, c24, c25,
                           c26, c27, c28, c29, c30, c31, c32: real);
   Begin
      a[17] := c17;
      a[18] := c18;
      a[19] := c19;
      a[20] := c20;
      a[21] := c21;
      a[22] := c22;
      a[23] := c23;
      a[24] := c24;
      a[25] := c25;
      a[26] := c26;
      a[27] := c27;
      a[28] := c28;
      a[29] := c29;
      a[30] := c30;
      a[31] := c31;
      a[32] := c32;
   End;

   Procedure SetConstants;
   Begin
      FieldColor[1] := Yellow;
      FieldColor[2] := LightCyan;
      FieldColor[3] := whitte;
      Fname := 'HBM';
      SimulateName[1] := ' UNIFORMLY MAGNETIZED';
      SimulateName[2] := ' IN AN EXTERNAL FIELD';
      ShapeName[1] := ' CYLINDER';
      ShapeName[2] := 'N ELLIPSOID';
      ShapeName[3] := ' DOUBLE CONE';
      ShapeName[4] := ' TORUS';
      CgsUnits[1] := ' Oersted';
      CgsUnits[2] := ' Gauss';
      CgsUnits[3] := ' Oersted';
      MksUnits[1] := ' Amp/m';
      MksUnits[2] := ' Tesla';
      MksUnits[3] := ' Amp/m';
      CgsToMks[1] := 1000/(4*Pi);
      CgsToMks[2] := 1.0E-4;
      CgsToMks[3] := 1000;
      shades[1] := Blue;
      shades[2] := LightBlue;
      shades[3] := LightCyan;
      shades[4] := Cyan;
      shades[5] := LightGreen;
      shades[6] := Green;
      shades[7] := Yellow;
      shades[8] := Brown;
      shades[9] := LightMagenta;
      shades[10] := Magenta;
      shades[11] := LightRed;
      DisplayType := 1 ; (* Initial display mode is vector array *);
      SetArray32b(Cheb0Lo, 4.18286705, -0.78918748, 0.19350654, -0.04976659,
                           0.01302235, -0.00343569, 0.00091057, -0.00024200,
                           0.00006445, -0.00001721, 0.00000461, -0.00000123,
                           0.00000031, -0.00000006, 0.00000002, -0.00000002);
      SetArray32e(Cheb0Lo, 0.00000003, -0.00000001, -0.00000001, 0.00000002,
                          -0.00000001, -0.00000001, 0.00000002, -0.00000001,
                          -0.00000001, 0.00000001, -0.00000001, -0.00000001,
                          0.00000001, -0.00000001, -0.00000001, 0.00000001);
      SetArray32b(Cheb0Hi, 13.07841457, -8.25809524, 5.17515226, -3.26461696,
                           2.06557672, -1.30854182, 0.82943840, -0.52591437,
                           0.33352348, -0.21153890, 0.13418083, -0.08511704,
                           0.05399604, -0.03425487, 0.02173169, -0.01378709);
      SetArray32e(Cheb0Hi, 0.00874703, -0.00554952, 0.00352090, -0.00223392,
                           0.00141738, -0.00089926, 0.00057055, -0.00036198,
                           0.00022962, -0.00014570, 0.00009247, -0.00005869,
                           0.00003727, -0.00002366, 0.00001498, -0.00000947);
      SetArray32b(Cheb1Lo, 0.78918744, 0.25106046, -0.09994639, 0.03106287,
                          -0.00901373, 0.00253953, -0.00070449, 0.00019366,
                          -0.00005290, 0.00001435, -0.00000385, 0.00000104,
                          -0.00000032, 0.00000013, -0.00000005, -0.00000002);
      SetArray32e(Cheb1Lo, 0.00000005, -0.00000004, -0.00000001, 0.00000005,
                          -0.00000004, -0.00000000, 0.00000005, -0.00000005,
                           0.00000001, 0.00000004, -0.00000005, 0.00000002,
                           0.00000003, -0.00000005, 0.00000003, 0.00000002);
      SetArray32b(Cheb1Hi, 9.88364071, -7.31986064, 4.85897525, -3.13578046,
                           2.00580824, -1.27860190, 0.81372078, -0.51740308,
                           0.32881166, -0.20888686, 0.13266870, -0.08424583,
                           0.05348982, -0.03395863, 0.02155721, -0.01368385);
      SetArray32e(Cheb1Hi, 0.00868565, -0.00551282, 0.00349898, -0.00222075,
                           0.00140939, -0.00089450, 0.00056765, -0.00036017,
                           0.00022858, -0.00014504, 0.00009204, -0.00005849,
                           0.00003713, -0.00002355, 0.00001495, -0.00000942);

   End;

   Procedure Credits;
      Var
         screen: HelpScrType;
   Begin
      screen[1] := '                                                  ';
      screen[2] := '               MAGNETOSTATICS                     ';
      screen[3] := '                                                  ';
      screen[4] := '                    by                            ';
      screen[5] := '                 Ron Stoner                       ';
      screen[6] := '   Department of Physics and Astronomy            ';
      screen[7] := '       Bowling Green State University             ';
      screen[8] := '        Bowling Green, OH 43403                   ';
      screen[9] := '                                                  ';
      screen[10] := '   This program computes and plots magnetostatic  ';
      screen[11] := ' fields in and around axially symmetric magnetized';
      screen[12] := ' solids.  The user can choose from among several  ';
      screen[13] := ' basic solid shapes and aspect ratios.  The solid ';
      screen[14] := ' can be either a uniformly magnetized permanent   ';
      screen[15] := ' magnet or a linear magnetizable material in a    ';
      screen[16] := ' uniform external magnetic field.                 ';
      screen[17] := '   In each of these cases, the user can choose to ';
      screen[18] := ' to examine three different fields -- namely the  ';
      screen[19] := ' induction B, the magnetization M or the magnetic ';
      screen[20] := ' field H.  Each of these fields may be examined in';
      screen[21] := ' several different ways, including color-coded    ';
      screen[22] := ' field patterns, and plots of field magnitudes or ';
      screen[23] := ' directions along mouse-selected lines.           ';
      screen[24] := '                                                  ';
      screen[25] := '   Copyright (c) John Wiley & Sons, Inc. 1995     ';
      help(screen);
   End;

   Procedure ShapeHelp (Var screen: HelpScrType);
   Begin
      screen[1] := '                                                  ';
      screen[2] := '* Parameters set by this screen determine         ';
      screen[3] := '  the shape and size of the magnetic object.      ';
      screen[4] := '                                                  ';
      screen[5] := '* All shapes are solids of revolution about a     ';
      screen[6] := '  vertical axis through the center of the         ';
      screen[7] := '  viewing port.                                   ';
      screen[8] := '                                                  ';
      screen[9] := '* The scale parameter is the half-width of        ';
      screen[10] := ' the viewport.                                    ';
      screen[11] := '                                                  ';
      screen[12] := '* Height is the vertical height of the magnetic   ';
      screen[13] := '  object.                                         ';
      screen[14] := '                                                  ';
      screen[15] := '* Diameter is the central half width of the       ';
      screen[16] := '  magnetic object.                                ';
      screen[17] := '                                                  ';
      screen[18] := '* To get a spherical shape, choose an ellipsoid   ';
      screen[19] := '  with aspect ratio = 1.00.                       ';
      screen[20] := '                                                  ';
      screen[21] := '                                                  ';
      screen[22] := '* Ok accepts screen values;  Cancel resets them.  ';
      screen[23] := '                                                  ';
      screen[24] := '                                                  ';
      screen[25] := '                                                  ';
   End;

   Procedure DisplayHelp (Var screen: HelpScrType);
   Begin
      screen[1] := '                                                  ';
      screen[2] := '* Parameters set by this screen determine         ';
      screen[3] := '  the way the results of the field computation    ';
      screen[4] := '  will be displayed.                             ';
      screen[5] := '                                                  ';
      screen[6] := '* The scale parameter is the linear dimension     ';
      screen[7] := '  of the square viewing port.  Large values       ';
      screen[8] := '  allow more space to be displayed, but what is   ';
      screen[9] := '  displayed will have less spatial resolution.    ';
      screen[10] := '                                                  ';
      screen[11] := '* The Grid Dimension parameter increases or       ';
      screen[12] := '  decreases the dimension of the matrix used      ';
      screen[13] := '  to store values of the magnetic field H.        ';
      screen[14] := '  small values allow faster computation of        ';
      screen[15] := '  over a coarse grid of points.  This parameter   ';
      screen[16] := '  should not be too small if you want to get      ';
      screen[17] := '  an accurate picture of the field using          ';
      screen[18] := '  contours or 3-D surface displays.               ';
      screen[19] := '                                                  ';
      screen[20] := '* OK accepts screen values;  Cancel resets them.  ';
      screen[21] := '                                                  ';
      screen[22] := '* You should use values for the parameters that   ';
      screen[23] := '  are within the reasonable ranges suggested on   ';
      screen[24] := '  the screen.                                     ';
      screen[25] := '                                                  ';
   End;

   Procedure WhatToDo;
      Var
         screen: HelpScrType;
   Begin
      screen[1] := '                                                  ';
      screen[2] := '  The viewport at right displays a magnetic field ';
      screen[3] := '(H, B or M) in and near a symmetric solid whose   ';
      screen[4] := 'cross section is outlined in green.  At lower     ';
      screen[5] := 'left is a plot of either magnetic charge density  ';
      screen[6] := 'or magnetic current density around the part of the';
      screen[7] := 'perimeter that is shaded when the plot is made.   ';
      screen[8] := '                                                  ';
      screen[9] := '   You can choose solids of various shapes made of';
      screen[10] := 'materials of various magnetic permeabilities by   ';
      screen[11] := 'selecting from the menu.  You can change the fine-';
      screen[12] := 'ness of the grid used to display the field and    ';
      screen[13] := 'the manner in which the field is displayed by     ';
      screen[14] := 'other menu choices.                               ';
      screen[15] := '                                                  ';
      screen[16] := '   The TOOLS menu provides several methods for    ';
      screen[17] := 'measuring or displaying the various fields.       ';
      screen[18] := '                                                  ';
      screen[19] := '   Use the DATA menu to change the simulation or  ';
      screen[20] := 'the way it is displayed.  The SHAPES menu allows  ';
      screen[21] := 'you to quickly to change the shape of the solid   ';
      screen[22] := 'without changing anything else (such as size,     ';
      screen[23] := 'permeability, etc.).                              ';
      screen[24] := '                                                  ';
      screen[25] := '                                                  ';
      help(screen);
   End;  (*WhatToDo *)


   Procedure MainHelp;
      Var
         screen: HelpScrType;
   Begin
      screen[1] := '                                                  ';
      screen[2] := '* This simulation computes and displays the       ';
      screen[3] := '  magnetic field patterns in and near             ';
      screen[4] := '  magnetized materials.                           ';
      screen[5] := '                                                  ';
      screen[6] := '* The materials are uniform and have 3-dimensional';
      screen[7] := '  shapes that are solids of revolution about      ';
      screen[8] := '  a vertical axis.   The shape of the material can';
      screen[9] := '  be modified or chosen from a data input screen. ';
      screen[10] := '                                                  ';
      screen[11] := '* You have the option of generating the fields    ';
      screen[12] := '  produced by a permanently and uniformly         ';
      screen[13] := '  magnetized object or of generating the fields   ';
      screen[14] := '  of a magnetizable object placed in an otherwise ';
      screen[15] := '  uniform external field.                         ';
      screen[16] := '                                                  ';
      screen[17] := '* The H field is displayed first after any change ';
      screen[18] := '  in parameters.  Thereafter the field pattern is ';
      screen[19] := '  regenerated for the field you have chosen (H, B ';
      screen[20] := '  or M). You can then use the pull down menu to   ';
      screen[21] := '  examine and display the field pattern in several';
      screen[22] := '  different ways.                                 ';
      screen[23] := '                                                  ';
      screen[24] := '                                                  ';
      screen[25] := '                                                  ';
      help(screen);
   End;

   Procedure TypeHelp (Var screen: HelpScrType);
   Begin
      screen[1] := '                                                  ';
      screen[2] := '* Parameters set by this screen are needed        ';
      screen[3] := '  to generate a field-pattern.                    ';
      screen[4] := '                                                  ';
      screen[5] := '* Any one of three fields can be generated:       ';
      screen[6] := '     H - Magnetic Field Strength;                 ';
      screen[7] := '     B - Magnetic Induction Field;                ';
      screen[8] := '     M - Magnetization Field.                     ';
      screen[9] := '                                                  ';
      screen[10] := '* You can choose to find the fields when the solid';
      screen[11] := '  is uniformly magnetized (permanent magnet), or  ';
      screen[12] := '  to apply an external uniform field Ho to a      ';
      screen[13] := '  uniform, linear, magnetizable solid.            ';
      screen[14] := '                                                  ';
      screen[15] := '* Other parameters include:                       ';
      screen[16] := '  Scale =  half-width of square plotting area.    ';
      screen[17] := '  Ho = the magnitude of the applied field.        ';
      screen[18] := '  Mu = relative magnetic susceptibility.          ';
      screen[19] := '                                                  ';
      screen[20] := '  Ok accepts screen values;  Cancel resets them.  ';
      screen[21] := '                                                  ';
      screen[22] := '* You should use values for the parameters that   ';
      screen[23] := '  are within the reasonable ranges suggested on   ';
      screen[24] := '  the screen.                                     ';
      screen[25] := '                                                  ';
   End;

   Procedure UnitsHelp;
      Var
         screen: HelpScrType;
   Begin
      screen[1] := '  You can choose either cgs or SI units.          ';
      screen[2] := '* The Pascal code uses cgs (Gaussian) units,      ';
      screen[3] := '  in which the field units have these names:      ';
      screen[4] := '     H - Magnetic Field Strength - Oersteds       ';
      screen[5] := '     B - Magnetic Induction Field - Gauss         ';
      screen[6] := '     M - Magnetization Field -- Oersteds          ';
      screen[7] := '                                                  ';
      screen[8] := '* Most undergraduate textbooks use SI (mks) units,';
      screen[9] := '  in which the field units have these names:      ';
      screen[10] := '     H - Amperes per meter                        ';
      screen[11] := '     B - Tesla (or Webers per square meter)       ';
      screen[12] := '     M - Amperes per meter                        ';
      screen[13] := '                                                  ';
      screen[14] := '* In empty space, a magnetic induction field B    ';
      screen[15] := '  of one Gauss corresponds to a magnetic field    ';
      screen[16] := '  strength H of one Oersted (i.e. permittivity    ';
      screen[17] := '  of free space = 1), so these units make Pascal  ';
      screen[18] := '  codes and theoretical equations easier. To      ';
      screen[19] := '  convert units, use the following conversion     ';
      screen[20] := '  factors:                                        ';
      screen[21] := '    B: Tesla = 10,000 Gauss                       ';
      screen[22] := '    H: 1 amp/meter = 0.001257 Oersted             ';
      screen[23] := '    M: 1 amp/meter = 0.000100 Oersted             ';
      screen[24] := '                                                  ';
      screen[25] := '  (Note different Oersted conversion for H & M)   ';
      help(screen);
   End;


 (******************  Menuing and HotKeys *********************** *)

   Procedure SetUpMenu;  (*Pulldown *)
   Begin
      With MyMenu Do
         Begin
            init;
            column(1, 'FILE');
            row(1, 1, 'About CUPS');
            row(1, 2, 'Title Screen');       (*Credits *)
            row(1, 3, 'About Program'); (*MainHelp *)
            row(1, 4, 'About Units');   (*UnitsHelp *)
            row(1, 5, 'Configuration');
            row(1, 6, 'EXIT program');
            column(2, 'TOOLS');
            row(2, 1, 'Plot |H|');
            row(2, 2, 'Plot |B|');
            row(2, 3, 'Plot |M|');
            row(2, 4, 'Plot H Direction');
            row(2, 5, 'Plot B Direction');
            row(2, 6, 'Field Probe');
            row(2, 7, 'Plot Magnetic Charge');
            row(2, 8, 'Plot Bound Current');
            column(3, 'CHOOSE');
            row(3, 1, 'Show H');
            row(3, 2, 'Show B');
            row(3, 3, 'Show M');
            column(4, 'DISPLAY');
            row(4, 1, 'Vector Grid');
            row(4, 2, 'Contours');
            row(4, 3, '3-D Surface');
            column(5, 'SHAPE');
            row(5, 1, 'Cylinder');
            row(5, 2, 'Ellipsoid');
            row(5, 3, 'Double Cone');
            row(5, 4, 'Torus');
            column(6, 'DATA ');
            row(6, 1, 'Shape & Size');
            row(6, 2, 'Simulation');
            row(6, 3, 'Display');
            row(6, 4, 'Switch Units');
         End;
      MyMenu.Display;
   End;


   Procedure SetUpHotKeys;
   Begin
      With HotKeys Do
         Begin
            init(3);
            key[1] := 'F1-Help';
            key[2] := 'F2-Refresh';
            key[3] := 'F10-Menu';
         End;
      HotKeys.Display;
   End;

 (* ************ Utility Procedures ************************** *)

   Procedure annotate(note : STRING);
   Begin
      GraphBackColor := Blue;
      OpenViewPort(6);
      SetColor(LightRed);
      print(2,1,note);
   End;

   Function EscapeFromThis : BOOLEAN;
        begin
           CheckForEvents;
	   IF (ord(Event.ReadKey) = 27) THEN EscapeFromThis := TRUE
              ELSE EscapeFromThis := FALSE;
        end;


   Procedure WaitForMouseInside;
        var
            inside:  BOOLEAN;
            x, y : REAL;
        {Loops until mouse is inside Viewport1}
        begin
            inside := FALSE;
            REPEAT  MousePosn (x, y,1,1,inside);
            UNTIL inside;
        end;


   Function Mag (x, y: REAL): REAL;
   Begin
      Mag := SQRT(SQR(x) + SQR(y));
   End;

   Function Dot (x1, y1, x2, y2: REAL): REAL;
   Begin
      Dot := x1 * x2 + y1 * y2;
   End;

   Procedure Unitize (Var x, y: real);
      Var
         size: REAL;
   Begin
      size := Mag(x, y);
      If size > MachEps Then
         Begin
            x := x / size;
            y := y / size
         End;
   End;


   Procedure Arrow(xo, yo, xcomp, ycomp, long: real;
                           Barbs: BOOLEAN);
 (*Draws arrows of specified length from (xo,yo)  *)
 (*to terminus or to intersection with viewport boundary. *)
      Var
         xorg, yorg, xend, yend: REAL;
         x1, y1, dx, dy, size, temp: real;
   Begin
      size := Mag(xcomp, ycomp);
      If (size < Macheps) Then
         size := Macheps;
      xend := xo + long * xcomp / size;
      yend := yo + long * ycomp / size;
      x1 := xo - long * xcomp / size;
      y1 := yo - long * ycomp / size;
      If abs(xend) > scale Then
         Begin          (*Replace by intersection with x-boundary. *)
            temp := scale * xend / abs(xend);
            yend := yo + (yend - yo) * (temp - xo) / (xend - xo);
            xend := temp;
         End;
      If abs(yend) > scale Then
         Begin          (*Replace by intersection with y-boundary. *)
            temp := scale * yend / abs(yend);
            xend := xo + (xend - xo) * (temp - yo) / (yend - yo);
            yend := temp
         End;
      PlotLine(x1, y1, xend, yend);
      If Barbs Then
         Begin
            dx := 0.33*(xend - x1);
            dy := 0.33*(yend - y1);
            xorg := xend;
            yorg := yend;
            xend := xend - dx + 0.5 * dy;
            yend := yend - dy - 0.5 * dx;
            PlotLine(xorg, yorg, xend, yend);
            xend := xend - dy;
            yend := yend + dx;
            PlotLine(xorg, yorg, xend, yend)
         End; (*Barbs *)
   End; (*Arrow *)


   Procedure ShowScaleColors;
   VAR
      Hcolor : WORD;
      fmin, fmax : REAL;
      line : INTEGER;
   BEGIN
      GraphBackColor := Blakk;
      SelectViewPort(6);
      ClearViewPort;
      DefineScale(6, -0.75*Ho,2.4*Ho,0,1);
      SelectScale(6);
      SetColor(shades[1]);
      FOR line := 1 TO 11 DO
       begin
         fmin := 0.20*(line - 1.5)*Ho;
         fmax := 0.20*(line - 0.5)*Ho;
         IF line = 1 THEN fmin := 0;
         IF line = 11 THEN fmax := 2.3*Ho;
         SetColor(shades[line]);
         PlotLine(fmin,0.2,fmax,0.2);
         PlotLine(fmin,0.22,fmax,0.22);
         PlotLine(fmin,0.18,fmax,0.18);
         PlotLine(fmin,0.16,fmax,0.16);
         PlotLine(fmin,0.24,fmax,0.24);
       end;
      SetColor(shades[1]);
      print(12,1,'0');
      Setcolor(shades[6]);
      print(12,1,'               '+Fname[FieldtoShow]+'o');
      SetColor(shades[11]);
      print(12,1,'                              2'+Fname[FieldtoShow]+'o');
      SetColor(Whitte);
      PlotLine(0.0, 0.1, 0.0, 0.3);
      PlotLine(1.0, 0.1, 1.0, 0.3);
      PlotLine(2.0, 0.1, 2.0, 0.3);
      print(1,1, 'MAGNITUDE');
      SelectViewPort(9);
      print(1,1,'COLOR CODE');
   END;

PROCEDURE ScaleAxes( sNum : integer;
    VAR pow10:REAL; x, y: datavector; numdata:INTEGER);
 (* Modified version of Autoscale from CUPSMUPP unit *)
  VAR
   min,max,Tick,CInt : ARRAY[1..2] OF real;
   n : integer;
  BEGIN
   min[1] := x[1];  max[1] := x[1];           {find max and min values of...}
   min[2] := y[1];  max[2] := y[1];           {... the data vectors x and y.}
   FOR n := 2 TO NumData DO
   BEGIN
      IF x[n] < min[1] THEN min[1] := x[n];
      IF x[n] > max[1] THEN max[1] := x[n];
      IF y[n] < min[2] THEN min[2] := y[n];
      IF y[n] > max[2] THEN max[2] := y[n];
   END;
   IF NOT (min[2] < max[2]) THEN axerr := 2 ELSE
      IF NOT (min[1] < max[1]) THEN axerr := 1 ELSE axerr := 0;
   IF axerr = 0 THEN
   BEGIN
      FOR n := 1 TO 2 DO
      BEGIN
         max[n] := max[n] + (max[n] - min[n])/20.0;
         min[n] := min[n] - (max[n] - min[n])/10.0;
         Tick[n] := TickSpace ((max[n]-min[n]));
      END;
      pow10 := pwr(10,Round(log(10,Tick[2])));
      DefineScale(snum,min[1],max[1], min[2]/pow10,max[2]/pow10);
      Axis(0,0,Tick[1],Tick[2]/pow10);
      DefineScale(snum,min[1],max[1],min[2],max[2]);
   END;
END;

   Procedure DoContour;
   Begin
      Contours.init;
      Contours.SetPaletteNum(1);
      Contours.DrawMatrix(DisplayMat);
      ShowScaleColors;
   End;

   Procedure DrawThreeD;
   Begin
      Plot3D := true;
      annotate('3D magnitude plot. Sliders change orientation.');
      SelectViewPort(1);
      GraphBackColor := Blakk;
      ClearviewPort;
      ThreeD.PlotMatrix(DisplayMat, '');
   End;



   FUNCTION PhiIntegral (a, b: real;
                           IntegralNo: INTEGER;
                           m: integer;
                           x: real): REAL;
    (*Evaluates Chebyschev polynomial approximants to 4 Integrals *)
     (*The following are computed assuming a breakpoint alpha=20 *)

   VAR
         sum, temp, sv, y, y2, term: real;
         j: integer;
   BEGIN
      If (x - a) * (x - b) > 0.0 Then
         Begin
            writeln('pause in CHEBEV - x not in range.');
            readln
         End;
      sum := 0.0;
      temp := 0.0;
      y := (2.0 * x - a - b) / (b - a);
      y2 := 2.0 * y;
      For j := m Downto 2 Do
         Begin
            sv := sum;
            Case IntegralNo Of
               1: term := Cheb0Lo[j];
               2: term := Cheb0Hi[j];
               3: term := Cheb1Lo[j];
               4: term := Cheb1Hi[j];
            End; (*CASE *)
            sum := y2 * sum - temp + term;
            temp := sv
         End; (*j loop *)
      Case IntegralNo Of
         1: term := Cheb0Lo[1];
         2: term := Cheb0Hi[1];
         3: term := Cheb1Lo[1];
         4: term := Cheb1Hi[1];
      End; (*CASE *)
      PhiIntegral := y * sum - temp + 0.5 * term
   END;

   Procedure Defaults;
   VAR
      scale4 :REAL;
   Begin
      MCheb := 12;
      MatSize := 17;
      ShapeType := 4;
      FieldToShow := 1;
      SimulateType := 1;
      D := 3.1;
      H := 1.5;
      scale := 5.0;
      MaxIter := 200;
      Ho := 1.0;
      Mu := 5.0;
      Chi := (Mu - 1) / (4 * pi);
      scale4 := scale*(matsize-1)/matsize;
      DefineScale(4,-scale4, scale4, -scale4, scale4);
   End;

   Procedure ShapeData;
      Var
         AspectRatio: REAL;
         ShapeHelpScr: HelpScrType;
         ShapeScreen: TInputScreen;
 (*Uses data screen to get shape information *)
   Begin
      AspectRatio := H / D;
      With ShapeScreen Do
         Begin
            init;
            DefineInputPort(0.15, 0.85, 0.13, 0.87);
            LoadLine('');
            LoadLine('  MAGNETIZATION OF AN AXIALLY SYMMETRIC OBJECT');
            LoadLine('');
            LoadLine('  Input screen for shape and size:');
            LoadLIne('');
            LoadLine('  Choose one of these shapes:');
            LoadLine('      #1-Cylinder            #1-Ellipsoid');
            LoadLine('      #1-Double Cone         #1-Torus ');
            LoadLine('');
            LoadLine('  Set size and shape: ');
            LoadLine('           Radius (at Center)     =  {    } cm.');
            LoadLine('   Aspect Ratio = Height/Diameter =  {    } cm.');
            LoadLine('');
            LoadLine('          [  Ok  ]       [Cancel]       [ Help ]');
            SetNumber(5, D);
            SetNumberLimits(5,scale/20,scale);
            SetNumber(6, AspectRatio);
            SetNumberLimits(6,0.1,10);
            ShapeHelp(ShapeHelpScr);
            SetHelpScreen(ShapeHelpScr);
            SetRadioButton('1', ShapeType);
            Accept
         End;
      With ShapeScreen Do
         Begin
            ShapeType := GetRadioButton('1');
            D := GetNumber(5);
            AspectRatio := GetNumber(6);
            H := D * AspectRatio;
         End;
   End;

   Procedure DisplayData;
      VAR
         DisplayHelpScreen: HelpScrType;
         DisplayScreen: TInputScreen;
         scale4     : REAL;
         button1    : INTEGER;
      BEGIN
      With DisplayScreen Do
         Begin
            init;
            DefineInputPort(0.15, 0.85, 0.10, 0.90);
            LoadLine('');
            LoadLine('  Set display parameters:');
            LoadLine('');
            LoadLine('           Plotting Scale  =  {     } 1 to 100 cm.');
            LoadLine('');
            LoadLine('                 Grid Size =  {     } 8 to 35');
            LoadLine('');
            LoadLine('  Choose a preferred system of units:');
            LoadLine('      #1 --  S.I. units (mks)');
            LoadLine('      #1 --  Gaussian units (cgs)');
            LoadLine('');
            LoadLine('          [  Ok  ]       [Cancel]       [ Help ]');
            LoadLine('');
            SetNumberLimits(1, 1.0, 100);
            SetNumber(1, scale);
            SetNumberLimits(2, 8, 35);
            SetNumber(2, MatSize);
            DisplayHelp(DisplayHelpScreen);
            SetHelpScreen(DisplayHelpScreen);
            IF cgs THEN SetRadioButton('1', 2)
                   ELSE SetRadioButton('1', 1);
            ACCEPT;
         End;
      With DisplayScreen Do
         Begin
            Scale := GetNumber(1);
            MatSize := Round(GetNumber(2));
            Scale4 := scale*(matsize-1)/matsize;
            DefineScale(4,-scale4, scale4,-scale4, scale4);
            Button1 := GetRadioButton('1');
            IF Button1 = 1 THEN cgs := FALSE
               ELSE cgs := TRUE;
         End;
      END;

   Procedure TypeData;
      Var
         TypeHelpScr: HelpScrType;
         TypeScreen : TInputScreen;
         Units      : String;
         Convert    : REAL;
         Scale4     : REAL;
   Begin
      IF cgs THEN Convert := 1 ELSE convert := cgstomks[1];
      With TypeScreen Do
         Begin
            init;
            DefineInputPort(0.15, 0.85, 0.10, 0.90);
            LoadLine('');
            LoadLine('  MAGNETIZATION OF AN AXIALLY SYMMETRIC OBJECT');
            LoadLine('');
            LoadLine('  Choose between the following cases:');
            LoadLine(' #3 Ferromagnet, uniform M, no applied field.');
            LoadLine(' #3 Uniform material in an applied field Ho.');
            LoadLine('');
            LoadLine('  Choose what field to display: ');
            LoadLine('        #2-H         #2-B         #2-M ');
            LoadLine('');
            IF cgs THEN Units := cgsunits[1]
                   ELSE Units := mksunits[1];
            LoadLine(Concat('            Applied field  Ho =  {     }',Units));
            LoadLine('   Relative permeability (Mu) =  {     } (0 - 10)');
            LoadLine('');
            LoadLine('          [  Ok  ]       [Cancel]       [ Help ]');
            SetNumberLimits(6, 0.01*convert, 100*convert);
            SetNumber(6, convert*Ho);
            SetNumberLimits(7, 0.0, 10.0);
            SetNumber(7, Mu);
            TypeHelp(TypeHelpScr);
            SetHelpScreen(TypeHelpScr);
            SetRadioButton('2', FieldToShow);
            SetRadioButton('3', SimulateType);
            Accept;
         End;
      If SimulateType = 2 Then
         Hfar := Ho
      Else
         Hfar := 0;
      With TypeScreen Do
         Begin
            Ho := GetNumber(6)/convert;
            Mu := GetNumber(7);
            Scale4 := scale*(matsize-1)/matsize;
            DefineScale(4,-scale4, scale4,-scale4, scale4);
            Chi := (Mu - 1) / (4 * Pi);
            FieldToShow := GetRadioButton('2');
            SimulateType := GetRadioButton('3')
         End;
   End;


   Function InsideSolid (x, y: REAL): BOOLEAN;
 (*Determines whether given point is inside or outside the solid *)
      Var
         r: REAL;
   Begin
      r := Mag(x, y);
      InsideSolid := FALSE;
      If r < maxr Then
         Case ShapeType Of

            1:
               If (ABS(x) < D) And (ABS(y) < H) Then
                  InsideSolid := TRUE;   (*Cylinder *)
            2:
               If (SQR(x / D) + SQR(y / H)) < 1 Then
                  InsideSolid := TRUE;  (*Ellipse *)
            3:
               If ABS(x) < D * (1 - (ABS(Y) / H)) Then
                  InsideSolid := TRUE;  (*DoubleCone *)
            4: IF sqr(ABS(x)-D+H)+sqr(y) < sqr(H) Then
                  InsideSolid := TRUE;  (*Toris *)
         End; (*Case of Shapetype *)
   End;

   Procedure Symmetrize (n: INTEGER);
 (*Creates a Symmetric Surface of revolution *)
      Var
         i: integer;
   Begin
      For i := 1 To n Do
         Begin
            sx[51 - i] := sx[i];
            sy[51 - i] := -sy[i];
            nx[51 - i] := nx[i];
            ny[51 - i] := -ny[i];
            sx[50 + i] := -sx[i];
            sy[50 + i] := -sy[i];
            nx[50 + i] := -nx[i];
            ny[50 + i] := -ny[i];
            sx[101 - i] := -sx[i];
            sy[101 - i] := sy[i];
            nx[101 - i] := -nx[i];
            ny[101 - i] := ny[i]
         End;
      sx[101] := sx[1];
      sy[101] := sy[1];
      nx[101] := nx[1];
      ny[101] := ny[1];
   End; (*Symmetrize *)

   Procedure CylinderSurface (H, D: Real);
 (* Procedure defines the surface of revolution and unit normals to it, *)
 (* and intializes magnetic charges for a cylinder *)
      Var
         Charge: REAL;
         Nr, Nh, i: Integer;
   Begin
      maxr := Mag(D, H);
      ds := (D + H) / 25;
      Charge := 3 * Hfar * (Chi / (Chi + 3)) / (4 * PI); (*Est. Max. Surface charge density *)
      Nr := Round(D / ds);
      Nh := 50 - 2 * Nr;
      For i := 1 To nr Do
         Begin        (*Define surface of rotation *)
            sx[i] := ds * (i - 0.5);    (*Top-right *)
            sy[i] := H;
            nx[i] := 0;
            ny[i] := 1;
         End;
      ds := 2 * H / Nh;
      For i := 1 To Nh Do
         Begin
            sx[nr + i] := D;       (*Right side *)
            sy[nr + i] := H - ds * (i - 0.5);
            nx[nr + i] := 1;
            ny[nr + i] := 0;
         End;
      Symmetrize(25);
      For i := 1 To 50 Do
         Begin
            Unitize(nx[i], ny[i]);
            sigm[i] := Dot(nx[i], ny[i], 0, Charge)
         End;
   End;

   Procedure Perimeter (N: Integer;
                           a, b: REAL;
                           Var S: REAL);
 (*Numerical integration to find the semiperimeter of an ellipse *)
 (* of semimajor axis a and semiminor axis b.  Uses Simpson rule. *)
      Var
         theta: REAL;
         alpha: REAL;
         i: INTEGER;
         dtheta, sum: REAL;
   Begin
      dtheta := PI / (4 * N);
      sum := a + b;
      For i := 1 To N - 1 Do
         Begin
            theta := (2 * i - 1) * dtheta;
            sum := sum + 4 * SQRT(sqr(a * sin(theta)) + sqr(b * cos(theta)));
            theta := theta + dtheta;
            sum := sum + 2 * SQRT(sqr(a * sin(theta)) + sqr(b * cos(theta)));
         End;
      S := (dtheta / 3) * sum
   End; (*Perimeter *)

   Procedure EllipSurface (a, b: REAL);
 (*Sets up an ellipsoid of revolution *)
      Var
         x2, y2, b2, a2: REAL; (*Intermediate variables *)
         Charge: REAL;
         Semiperim: REAL; (*1/4 of ellipse perimeter *)
         z1, z2, rho1, rho2: REAL;
         drdz, dzdr: REAL;
         i: integer;
   Begin
      Perimeter(100, a, b, Semiperim);
      Charge := 3 * Hfar * (Chi / (Chi + 3)) / (4 * PI); (*Est. Max. Surface charge density *)
      dS := Semiperim / 25;
      maxr := a;
      If b > maxr Then
         maxr := b;
      z1 := a;
      rho1 := 0;
      rho2 := 0.5 * dS;
      z2 := a * sqrt(1 - sqr(rho2 / b));
      drdz := (rho2 - rho1) / (z2 - z1);
      z1 := a - 0.5 * dS / sqrt(1 + sqr(drdz));
      sy[1] := z1;
      rho1 := b * sqrt(1 - sqr(z1 / a));
      ;
      sx[1] := rho1;
      For i := 2 To 25 Do
         Begin
            drdz := b * z1 / (sqr(a) * sqrt(1 - sqr(z1 / a)));
            If drdz > 1 Then
               Begin
                  z2 := z1 - dS / sqrt(1 + sqr(drdz));
                  drdz := 0.5 * b * (z1 + z2) / (sqr(a) * sqrt(1 - 0.25 * sqr((z1 + z2) / a)));
                  z1 := z1 - dS / sqrt(1 + sqr(drdz));
                  sy[i] := z1;
                  sx[i] := b * sqrt(1 - sqr(z1 / a));
                  rho1 := sx[i]
               End
            Else
               Begin
                  rho2 := rho1 + dS / sqrt(1 + sqr(1 / drdz));
                  dzdr := 0.5 * a * (rho1 + rho2) / (sqr(b) * sqrt(1 - 0.25 * sqr((rho1 + rho2) / b)));
                  rho1 := rho1 + dS / sqrt(1 + sqr(dzdr));
                  sx[i] := rho1;
                  sy[i] := a * sqrt(1 - sqr(rho1 / b));
                  z1 := sy[i]
               End;
         End;
      a2 := sqr(a);
      b2 := sqr(b);
      For i := 1 To 25 Do
         Begin
            x2 := sqr(sx[i]);
            y2 := sqr(sy[i]);
            nx[i] := SQRT(a2 * (a2 - y2) / (a2 * (a2 - y2) + b2 * y2));
            ny[i] := SQRT(b2 * (b2 - x2) / (b2 * (b2 - x2) + a2 * x2));
         End;
      Symmetrize(25);
      For i := 1 To 101 Do
         sigm[i] := Dot(nx[i], ny[i], 0, Charge);
   End;


   Procedure TwoConeSurface (High, Wide: Real);
 (* Procedure defines the surface of revolution and unit normals to it, *)
 (* and initializes magnetic charges *)
      Var
         Charge: REAL;
         Nr, Nh, i: Integer;
         side: Real;
   Begin
      maxr := wide;
      If High > maxr Then
         maxr := High;
      side := Mag(High, Wide);
      ds := side / 25;
      Charge := 3 * Hfar * (Chi / (Chi + 3)) / (4 * PI); (*Est. Max. Surface charge density *)
      For i := 1 To 25 Do
         Begin
            sy[i] := High * (1 - 0.04 * (i - 0.5));
            sx[i] := 0.04 * Wide * (i - 0.5);
            nx[i] := High / side;
            ny[i] := Wide / side;
         End;
      Symmetrize(25);
      For i := 1 To 101 Do
         sigm[i] := Dot(nx[i], ny[i], 0, Charge)
   End;  (*DoubleCone *)

Procedure TorusSurface (Height, Width: Real);
 (* Computes position and unit normals for a spherical *)
 (* surface of given radius *)
      Var
         Radius       : REAL;
         Charge, Angle: REAL;
         i            : INTEGER;

      Const
         Nr = 100;

   Begin
      maxr := Width;
      Radius := Height;
      ds := PI * Radius / 50;
      Charge := 3 * Hfar * (Chi / (Chi + 3)); (*Est. Max. Surface charge density *)
      For i := 1 To 50 Do
         Begin        (*Define surface of rotation *)
            Angle := PI - 0.04 * PI * (i - 0.5);
            nx[i] := COS(ANGLE);
            ny[i] := SIN(ANGLE);
            sx[i] := Width - Radius + Radius * nx[i];
            sy[i] := Radius * ny[i];
         End;
      For i := 1 To 50 Do
         Begin        (*Define surface of rotation *)
            Angle := - 0.04 * PI * (i - 0.5);
            nx[i+50] := COS(ANGLE);
            ny[i+50] := SIN(ANGLE);
            sx[i+50] := - Width + Radius + Radius * nx[i];
            sy[i+50] := Radius * ny[i];
         End;
           For i := 1 To 101 Do
         sigm[i] := Dot(nx[i], ny[i], 0, Charge);
   End;

   Procedure RingField (rho,  (*Distance from axis *)
                           z, rad,  (*Radius of ring *)
                           zeta: REAL; (*Shortest distance to ring *)
                           Var Hr, Hz: REAL);
 (*Finds H field due to a ring of unit-density magnetic charge *)
      Var
         coeff: REAL;  (*Intermediate variable *)
         alpha: REAL;  (*2*R*rho/SQR(zeta), where zeta = dist to ring *)
         dummy: REAL;
         I0, I1: REAL;  (*Integrals over angle *)

   Begin
      dummy := zeta / rad;
      If dummy > 0.001 Then
         Begin
            coeff := 2 / (dummy * SQR(zeta));
            alpha := 2 * rho / (dummy * zeta);
         End
      Else
         Begin
            coeff := 0;
            alpha := 1000
         End;
      If alpha > 20 Then
         Begin
            I0 := SQRT(2 / alpha);
            I1 := I0;
         End
      Else If alpha > 1 Then
         Begin
            I0 := PhiIntegral(0.05, 1, 2, 32, 1 / alpha) / (alpha * SQRT(alpha));
            I1 := PhiIntegral(0.05, 1, 4, 32, 1 / alpha) / (alpha * SQRT(alpha));
         End
      Else
         Begin
            I0 := PhiIntegral(0, 1, 1, Mcheb, alpha);
            I1 := PhiIntegral(0, 1, 3, Mcheb, alpha)
         End;
      Hz := coeff * z * I0;
      Hr := coeff * (rho * I0 - rad * I1);
   End;

   Procedure FindCoeff;
      Var
         zeta: REAL;
         zcoord, RingRadius: REAL;
         AbsRho: REAL;
         Hr, Hz: REAL;
         Working: TWaitMessage;
         i, j: integer;
   Begin
      (*new(working);*)
      working.Show;
      For I := 1 To 50 Do
       BEGIN
         For J := 1 To 50 Do
            Begin
               RingRadius := ABS(sx[J]);
               zcoord := sy[I] - sy[J];
               AbsRho := ABS(sx[I]);
               zeta := Mag(zcoord, (AbsRho - RingRadius));
               If zeta > ds / 2 Then
                  RingField(Absrho, zcoord, RingRadius, zeta, Hr, Hz)
               Else
                  Begin
                     Hr := 0;
                     Hz := 0
                  End;
               Coeff.Put(I, J, ds * (Hr * nx[I] + Hz * ny[I]));
            End;
            working.update;
         End;
      working.Hide;
      (*dispose(working);*)
   End;

   Procedure ComputeField (rho, z: REAL;
                           Var Hr, Hz: REAL);
 (*Computes components of H field *)
      Var
         Nearby, i: INTEGER;
         sumr, sumz, zeta: REAL;
         zcoord, RingRadius: REAL;
         AbsRho: REAL;
   Begin
      sumr := 0;
      sumz := 0;
      absrho := ABS(rho);
      Nearby := -1;
      For i := 1 To 50 Do  (*Sum over rings of magnetic charge. *)
         Begin
            RingRadius := abs(sx[i]);
            zcoord := z - sy[i];
            zeta := Mag(zcoord, (AbsRho - RingRadius)); (*Distance to ring *)
            If zeta > ds / 2 Then
               RingField(Absrho, zcoord, RingRadius, zeta, Hr, Hz)
            Else
               Begin
                  Nearby := i;
                  Hr := 0;
                  Hz := 0
               End;
            If rho < 0 Then
               Hr := -Hr;
            sumr := sumr + Hr * sigm[i] * ds;
            sumz := sumz + Hz * sigm[i] * ds
         End;
      Hr := sumr;
      Hz := sumz + Hfar;
      If Nearby > 0 Then
         If InsideSolid(rho, z) Then
            Begin
               Hr := Hr - 2 * pi * nx[Nearby] * sigm[Nearby];
               Hz := Hz - 2 * pi * ny[Nearby] * sigm[Nearby]
            End
         Else
            Begin
               Hr := Hr + 2 * pi * nx[Nearby] * sigm[Nearby];
               Hz := Hz + 2 * pi * ny[Nearby] * sigm[Nearby]
            End;

   End;

   Procedure FindHMatrices;
   VAR
      I, J, Hcolor  : INTEGER;
      HalfSize : INTEGER;
      MagH : REAL;
      rho, z  : REAL;
      Hr, Hz: REAL;  (*components of field *)
   BEGIN
      Hidecursor;
      ShowScaleColors;
      SelectViewPort(1);
      SelectScale(1);
      HalfSize := Round(MatSize/1.99);
      FOR I := 1 TO MatSize DO
         BEGIN
            FOR J := 1 TO MatSize DO
               BEGIN
                  rho := scale * (2*i-matsize-1) /MatSize;
                  z := scale * (2*j-matsize-1) / Matsize; (*Grid Points in z-dir. (vertical) *)
                  ComputeField(rho, z, Hr, Hz);
                  MagH := Mag(Hr, Hz);
                  DisplayMat.put(j, i, MagH);
                  {SetColor(White);}
                  If MagH < 2 * Ho
                    Then Hcolor := 1 + round(5 * MagH / Ho)
                    Else Hcolor := 11;
                  SetColor(shades[Hcolor]);
                  PlotSymbol(rho, z, '.');
                  Arrow(rho, z, Hr, Hz, scale/MatSize, TRUE);
                  MatHx.Put(j,i,Hr);
                  MatHy.Put(j,i,Hz);
               END;
         END;
      GraphBackColor := DarkGray;
      OpenViewPort(8);
      SelectViewPort(8);
      SetColor(FieldColor[FieldtoShow]);
      Print(5, 1, concat(FName[1], ' NEAR A',
            ShapeName[Shapetype], SimulateName[SimulateType]));
      SelectViewPort(1);
      IF CreditsOn THEN
         BEGIN
         Credits;
         CreditsOn := FALSE
         END;
      GraphBackColor := Blakk;
      OpenViewPort(1);
      Showcursor;
   END;

   Procedure RelaxField;
 (* Iterates to find magnetic charges along surface. *)
      Var
         rho, z: REAL;
         Hr, Hz: REAL;  (* components of field *)
         Iter, i, j: Integer;
         ring: Integer;
         sumr, sumz: REAL;
         GlobalErr: REAL;
         OldValue: REAL;
         Working: TWaitMessage;
   Begin
      FindCoeff;
      Working.Show;  (*Indicate long computation is starting *)
      Iter := 0;
      While Iter < MaxIter Do
         Begin
            GlobalErr := 0;
            For I := 1 To 25 Do
               Begin
                  OldValue := sigm[I];
                  sigm[I] := Hfar * ny[I];   (*Contribution from afar *)
                  For J := 1 To 50 Do      (*Add contributions from rings *)
                     sigm[I] := Sigm[I] + Coeff.Value(I, J) * sigm[J];
                  sigm[I] := Chi * sigm[I] / (1 + 2 * pi * chi);
                  GlobalErr := GlobalErr + Sqr(sigm[I] - OldValue);
                  sigm[i] := sigm[i] + 0.1*(sigm[i] - OldValue);
                  sigm[51 - I] := -sigm[I]  (*Use symmetry to aid convergence *)
               End;
            working.update;
            If SQRT(GlobalErr) < 1E-8 * SQR(Ho) Then
               Iter := MaxIter;
            Iter := Iter + 1
         End;
      If SQRT(GlobalErr) > 1E-8 * SQR(Ho) Then
         Begin
            BEEP;
            ANNOUNCE('>>> CONVERGENCE FAILURE -- RESULTS MAY BE IN ERROR! <<<');
            StaticPause
         End;
      Working.Hide;
   End;

   Procedure MakePermanent;
 (*For permanent magnet, assume uniform magnetization in z-direction *)
      Var
         i: integer;
   Begin
      HFar := 0;
      For i := 1 To 50 Do
         sigm[i] := DOT(nx[i], ny[i], 0.0, Ho);
   End;

   Procedure  DrawShape(NiceColor : Integer);

   VAR  I: Integer;
   BEGIN
   SetColor(NiceColor);
   SelectViewPort(1);
     IF ShapeType = 4 THEN
         BEGIN
            PlotData(sx,sy,50);
            FOR I := 1 TO 50 DO sx[i] := -sx[i];
            PlotData(sx, sy, 50);
            FOR I := 1 TO 50 DO sx[i] := -sx[i];
            PlotLine(sx[50], sy[50], sx[1], sy[1]);
            PlotLine(-sx[50], sy[50],-sx[1], sy[1]);
         END
      ELSE  PlotData(sx, sy, 101);
   END;

   Procedure ShowShape;
   VAR i : INTEGER;
   Begin
      HideCursor;
      GraphBackColor := Blakk;
      OpenViewport(1);
      SelectViewport(1);
      SelectScale(1);
      SetColor(Magenta);
      PlotSymbol(0, 0, '+');
      DrawShape(LightGreen);
      ShowCursor;
   End;

   Procedure GetComponents (rho, z: REAL;
                           Var Hr, Hz, Br, Bz, Mr, Mz: REAL);
 (*Computes components of the fields at location (rho,z) *)
   CONST
      fourpi = 4*3.141593;
   Begin
      ComputeField(rho, z, Hr, Hz);
      If InsideSolid(rho, z) Then
         If SimulateType = 2 Then
            Begin
               Br := Mu * Hr;
               Bz := Mu * Hz;
               Mr := (Br - Hr) / fourpi;
               Mz := (Bz - Hz) / fourpi
            End
         Else
            Begin
               Mz := Ho;
               {Mz := Ho/fourpi};
               Mr := 0;
               Br := Hr;
               Bz := Hz + fourpi*Mz
            End
      Else
         Begin
            Mr := 0;
            Mz := 0;
            Br := Hr;
            Bz := Hz
         End;
   End;

   Procedure GetTheField (FieldNumber: INTEGER;
                           x, y: REAL;
                           Var Fieldx, Fieldy: REAL);
      Var
         Hx, Hy, Bx, By, Mx, My: REAL;
   Begin
      GetComponents(x, y, Hx, Hy, Bx, By, Mx, My);
      If FieldNumber = 1 Then
         Begin
            Fieldx := Hx;
            Fieldy := Hy
         End
      Else If FieldNumber = 2 Then
         Begin
            Fieldx := Bx;
            Fieldy := By;
         End
      Else
         Begin
            Fieldx := Mx;
            Fieldy := My
         End;
   End;

   Procedure PlotSources(SourceType: Integer; Length :REAL);
   CONST
      eps = 1e-2;
   VAR
      I      : INTEGER;
      Mx, My : REAL;
      rho, z : REAL;
      multiplier : REAL;
   BEGIN
      For i := 0 To 51 Do
         Begin
            x[i] := i * 0.02 * Length;
            IF SourceType = 1 THEN y[i] := Sigm[i]
            ELSE
               BEGIN
                  rho := sx[i] - eps*scale*nx[i]; (* Just inside surface *)
                  z := sy[i] - eps*scale*ny[i];
                  GetTheField(3, rho, z, Mx, My);
                  y[i] := -My*nx[i]+Mx*ny[i]  (* Bound current *)
               END;
         End;
      OpenViewPort(2);
      IF SourceType = 1 THEN SetColor(LightRed)
                        ELSE SetColor(LightCyan);
      SelectViewPort(1);
      SelectScale(1);
      IF DisplayType < 3 THEN PlotData(sx, sy, 50);
      CloseViewPort(4);
      SelectViewPort(2);
      scaleaxes(2, multiplier, x, y, 50);
      IF axerr > 0 THEN
      begin
         BEEP;
         IF axerr = 1 THEN annotate('Horizontal scaling error. Press F2.')
         ELSE annotate('Vertical scaling error. Press F2.');
         dynamicpause;
      end
      ELSE
      begin
         PlotData(x, y, 50);
         GraphBackColor := DarkGray;
         OpenViewPort(7);
         IF SourceType = 1 THEN print(1,1,' MAGNETIC CHARGE PLOT ')
         ELSE IF SourceType = 2 THEN print(1,1,'  BOUND CURRENT PLOT  ');
      end;
   END;

   Procedure ShowField;
      CONST
        fourpi = 4*3.141593;
      Var
         z             : REAL;
         Hx, Hy        : REAL;
         rh            : REAL;
         Bx, By, Mx, My: REAL;
         Fieldx, Fieldy: REAL;
         Length        : REAL;
         MagH          : REAL;
         irho, iz, i   : INTEGER;
         Hcolor        : INTEGER;
   Begin
      HideCursor;
      ShowScaleColors;
      SelectViewport(1);
      SelectScale(1);
      For irho := 1 To MatSize Do
         For iz := 1 To MatSize Do
            BEGIN
               rh := scale * (2*irho-matsize-1) / MatSize;
               z :=  scale * (2*iz-matsize-1) / MatSize; (*Grid Points in z-dir. (vertical) *)
               SetColor(whitte);
               MatHx.Get(iz,irho,Hx);
               MatHy.Get(iz,irho,Hy);
            If InsideSolid(rh, z) Then
               If SimulateType = 2 Then
                  Begin
                     Bx := Mu * Hx;
                     By := Mu * Hy;
                     Mx := (Bx - Hx) / fourpi;
                     My := (By - Hy) / fourpi
                  End
               Else
                  Begin
                     My := Ho;
                     Mx := 0;
                     Bx := Hx;
                     By := Hy + fourpi* My
                  End
            Else
               Begin
                  Mx := 0;
                  My := 0;
                  Bx := Hx;
                  By := Hy
               End;
          CASE FieldToShow OF
          1: BEGIN
                Fieldx := Hx;
                Fieldy := Hy;
             END;
          2: BEGIN
                Fieldx := Bx;
                Fieldy := By;
             END;
          3: BEGIN
                Fieldx := Mx;
                Fieldy := My;
             END;
          END;
          MagH := Mag(Fieldx, Fieldy);
          DisplayMat.put(iz,irho,MagH);
               If MagH < 2 * Ho Then
                  Hcolor := 1 + round(5 * MagH / Ho)
               Else
                  Hcolor := 11;
               SetColor(shades[Hcolor]);
               PlotSymbol(rh, z, '.');
               Arrow(rh, z, Fieldx, Fieldy, scale/MatSize, TRUE);
               DisplayMat.put(iz, irho, MagH);
            End;
      Case ShapeType Of
         1: Length := 2 * (D + H);
         2: Perimeter(50, D, H, Length);
         3: Length := 2 * Mag(D, H);
         4: Length := 2*PI*H;
      End;
      GraphBackColor := DarkGray;
      OpenViewPort(8);
      SelectViewPort(8);
      SetColor(FieldColor[FieldToShow]);
      Print(5, 1, concat(FName[FieldToShow],
           ' NEAR A', ShapeName[Shapetype], SimulateName[SimulateType]));
      SelectViewPort(4);
      ClearViewPort;
      SelectViewPort(1);
      SelectScale(1);
      PlotSources(1,length);
      SetColor(LightGreen);
      SelectViewport(1);
      SelectScale(1);
      ShowCursor;
      MyMenu.Display;
      StaticPause;
      DrawShape(LightGreen);
      ShowCursor
   End;

   Procedure DefineSurface (Shape: word;
                           Height, Width: REAL);
   Begin
      Case Shape Of
         1:
            CylinderSurface(Height, Width);
         2:
            EllipSurface(Height, Width);
         3:
            TwoConeSurface(Height, Width);
         4:
            TorusSurface(Height,Width);
      End; (*CASE *)
   End; (*Definesurface *)

   Procedure PrintParameters;
   (*Prints relevant information on viewport #3 *)
   VAR
      Units  : String;
      Hout   : REAL;
   Begin
      GraphBackColor := Blue;
      OpenViewport(3);
      GraphBackColor := blakk;
      SetColor(Yellow);
      IF cgs THEN Hout := Ho
             ELSE Hout := Ho*cgstomks[1];
      Print(2,1,'Simulation of the');
      SetColor(FieldColor[FieldtoShow]);
      Print(2,2, Concat(FName[FieldtoShow],' field of'));
      IF SimulateType = 2 THEN
         BEGIN
           IF cgs then Units := cgsunits[1]
                  ELSE Units := mksunits[1];
           Print(1,3,Concat(' s',ShapeName[ShapeType],'; external field'));
           Print(1,4,Concat(' Ho = ',ScNumStr(Hout, 3),Units));
           SetColor(LightCyan);
           Print(1,5,Concat(' Permeability = ',NumStr(Mu, 6,2)));
         END
      ELSE
         BEGIN
           IF cgs then Units := cgsunits[3]
                  ELSE Units := mksunits[3];
           Print(1,3,Concat(' A',ShapeName[ShapeType], ' MAGNET'));
           Print(1,4,Concat(' WITH M = ',ScNumStr(Hout, 3),units));
         END;
      SetColor(LightGray);
      Print(1,6,Concat('     Radius: ',Numstr(D, 6,2),' cm'));
      Print(1,7,Concat('     Height: ',NumStr(2*H,6,2),' cm'));
      Print(1,8,Concat(' Plot Width: ',NumStr(2*scale,4,1),' cm'));
      SelectViewPort(1);
   End;

   Procedure PrintFields (x, y, Hx, Hy, Bx, By, Mx, My: REAL);
   (*Prints results of Field Probe menu option *)
   VAR
      Units : Array[1..3] Of String;
      Convert : Array [1..3] Of REAL;
      I  : INTEGER;
   Begin
      GraphBackColor := DarkGray;
      OpenViewport(3);
      GraphBackColor := blakk;
      SetColor(whitte);
      Print(2, 1, 'Field Point');
      Print(3, 2, concat('X= ', NumStr(x,6, 3)));
      Print(3, 3, concat('Y= ', NumStr(y,6, 3)));
      Print(1, 5, 'at        degrees.');
      Print(1, 7, 'at        degrees.');
      Print(1, 9, 'at        degrees.');
      FOR i := 1 TO 3 DO
         IF cgs THEN Convert[i] := 1
                ELSE Convert[i] := CgsToMks[i];
      FOR i := 1 TO 3 DO
         IF cgs THEN Units[i] := CgsUnits[i]
                ELSE Units[i] := MksUnits[i];
      SetColor(FieldColor[1]);
      Print(2, 4, concat('H = ',
         ScNumStr(Convert[1]*Mag(Hx, Hy), 4), Units[1]));
      Print(4, 5, Num2Str((180 / pi) * ArcTan2(Hx, Hy), 6));
      SetColor(FieldColor[2]);
      Print(2, 6, concat('B = ',
         ScNumStr(Convert[2]*Mag(Bx, By), 4), Units[2]));
      Print(4, 7, Num2Str((180 / pi) * ArcTan2(Bx, By), 6));
      SetColor(FieldColor[3]);
      Print(2, 8, concat('M = ',
         ScNumStr(Convert[3]*Mag(Mx, My), 4), Units[3]));
      Print(4, 9, Num2Str((180 / pi) * ArcTan2(Mx, My), 6));
   End;

   Procedure MouseReporter;
   VAR
     xpos, ypos   :  REAL;
     inside       :  BOOLEAN;
     ix, iy       : INTEGER;
   Begin
      SelectViewPort(3);
      SetColor(LightGray);
      REPEAT
        MousePosn(xpos, ypos, 1, 1, inside);
        IF inside THEN begin
           RubOut(16, 1, 8, DarkGray);
           RubOut(17, 2, 6, DarkGray);
           RubOut(17, 3, 6, DarkGray);
           Print(16, 1, 'Mouse at');
           Print(17, 2, NumStr(xpos, 5,3));
           Print(17, 3, NumStr(ypos, 5,3));
           Delay(100);
           end
        ELSE begin
           RubOut(16, 1, 8, DarkGray);
           RubOut(17, 2, 6, DarkGray);
           RubOut(17, 3, 6, DarkGray);
           end;
      UNTIL MouseClicked(ix, iy);
   End;

   Procedure Explore;
 (*Use mouse as a probe to measure and print fields *)
      Var
         Hx, Hy, Bx, By: Real;     (*Field components *)
         Mx, My: REAL;
         xmouse, ymouse: Real;     (*Mouse positions *)
         xpos, ypos: Real;
         inside: Boolean;
         Sscale, long: Real;   (*Used to determine length of vectors *)
         FScale: REAL;   (*Typical field magnitude *)
         MinValue: REAL;   (*Doesn't show Field if less than this *)
   Begin
      Annotate('Click inside for fields, outside to quit.');
      MinValue := 1.0E-4 * Ho;
      SelectViewPort(1);
      SelectScale(1);
      WaitforMouseInside;
      inside := FALSE;
      GraphBackColor := DarkGray;
      OpenViewport(3);
      SetColor(Whitte);
      Repeat
         MouseReporter;
         MousePosn(xmouse, ymouse, 1, 1, inside);
         If inside Then
            Begin
               xpos := xmouse;
               ypos := ymouse;
               GetComponents(xpos, ypos, Hx, Hy, Bx, By, Mx, My);
               PrintFields(xpos, ypos, Hx, Hy, Bx, By, Mx, My);
            End;
            SelectViewPort(1);
            SetColor(Whitte);
            PlotSymbol(xpos,ypos,'x');
      Until Not inside;
      ShowScaleColors;
   End;

   Procedure RVector (a, b: pointType); far;
 (*Used with CUPS rubber-band utility *)
   Begin
      vector(a.x, a.y, b.x, b.y);
   End;

   Procedure PrintMouseXY(xplace, yplace: Real; Eraseit: BOOLEAN);
   BEGIN
   IF EraseIt THEN SetColor(Blakk)
              ELSE SetColor(Whitte);
   Print(2,1, Concat('X = ', NumStr(xplace,7,2)));
   Print(2,2, Concat('Y = ', NumStr(yplace,7,2)));
   END;

   Procedure GetGraphLine (Var xstart, ystart, xend, yend: REAL;
                           inside: BOOLEAN);
 (*Obtains and plots cut for GRAPH option. *)
      Var
         xtic, ytic, dx, dy: REAL;  (*Position and size of tic marks. *)
         ntic: integer;        (*Tic mark number. *)
         x, y, button: INTEGER;
         a, b: pointType;
         vp: ViewPortType;
   Begin
      Annotate('Use click and drag inside.  <ESC> to quit.');
      WaitForMouseInside;
      SelectViewport(1);
      GetViewSettings(vp);
      {ShowCursor;}
      SetLineStyle(SolidLn, 0, normWidth);
      xstart := 0;
      ystart := 0;
      Repeat         (*Choose start point *)
         Begin
            MouseGlobalPosn(x, y, button);
            MousePosn(xstart, ystart, 1, 1, inside);
            IF NOT inside THEN GraphingOver := TRUE;
         End
      Until (button <> 0) OR GraphingOver;
      IF NOT GraphingOver THEN
         begin
            a.x := x - vp.x1;
            a.y := y - vp.y1;
            SetColor(LightGray);
            DrawRubberBand(Rvector, a, b, blakk, true);
            MousePosn(xend, yend, 1, 1, inside);
            IF NOT inside THEN GraphingOver := TRUE;
            SelectScale(1);
            SetColor(whitte);
            SelectScale(1);
            HideCursor;
            Comment(0.36,0.95,'                                             ');
            PlotSymbol(xstart, ystart, 'x');       (*mark it. *)
            PLotSymbol(xend, yend, 'x');
            SetColor(LightGray);
            PlotLine(xstart, ystart, xend, yend);
            dx := (yend - ystart) / 40;          (* Draw graph line *)
            dy := -(xend - xstart) / 40;
            For ntic := 1 To 9 Do             (* Draw tic marks. *)
               Begin
                  xtic := xstart + ntic * (xend - xstart) / 10;
                  ytic := ystart + ntic * (yend - ystart) / 10;
                  PlotLine(xtic + dx, ytic + dy, xtic - dx, ytic - dy);
               End;
         end;
      ShowCursor;
   End;

   Procedure GraphField (N: integer);
 (*Find and graph field values along a rubber-band selected line *)
      CONST
         radtodeg = 180/pi;
      Var
         x, y     : Datavector;
         Working        : TWaitMessage;
         xstart, ystart : REAL;
         xend, yend     : REAL;
         Fx, Fy         : REAL;
         xp, yp         : REAL;
         multiplier     : REAL;
         point, long    : INTEGER;
         inside         : BOOLEAN;
         units          : STRING;
   Begin
   GraphingOver := False;
   GetGraphLine(xstart, ystart, xend, yend, inside);
   REPEAT IF ((NOT EscapeFromThis) AND (NOT GraphingOver)) THEN
    begin
      SelectViewPort(4);
      ClearViewPort;
      GraphBackColor := blakk;
      OpenViewport(2);
      GraphBackColor := DarkGray;
      OpenViewport(3);
      GraphBackColor := blakk;
      ClearViewPort;
      SetColor(whitte);
      Print(1, 1, 'Plot along a line');
      Print(4, 2, 'from ');
      Print(4, 5, 'to ');
      IF N < 4 THEN SetColor(FieldColor[N])
               ELSE SetColor(FieldColor[N-3]);
      SetColor(LightGreen);
      Print(2, 3, 'X1=');
      Print(2, 4, 'Y1=');
      Print(6, 3, ScNumStr(xstart, 5));
      Print(6, 4, ScNumStr(ystart, 5));
      Print(2, 6, 'X2=');
      Print(2, 7, 'Y2=');
      Print(6, 6, ScNumStr(xend, 5));
      Print(6, 7, ScNumStr(yend, 5));
      For point := 0 To 61 Do (* Construct vector of data for plotting *)
         Begin
            x[point] := point / 60;
            xp := xstart + (xend - xstart) * x[point];
            yp := ystart + (yend - ystart) * x[point];
            IF N < 3 THEN
               BEGIN
                   GetTheField(N, Abs(xp), yp, Fx, Fy);
                   IF NOT cgs THEN
                     begin
                       Fx := Fx * cgstomks[N];
                       Fy := Fy * cgstomks[N]
                     end;
                   y[point] := Mag(Fx, Fy)
               END
            ELSE
              BEGIN
                 GetTheField(N-3, Abs(xp), yp, Fx, Fy);
                 IF xp < 0 THEN Fx := -Fx;
                 y[point] := Radtodeg*ArcTan2(Fx,Fy)
              END;
         End;(* loop *)
      CloseViewPort(4);
      SetColor(LightGreen);   (* Scale and plot *)
      x[62] := 0;
      y[62] := 0;  (* Force origin to be in plot *)
      SelectViewPort(2);
      scaleaxes(2, multiplier, x, y, 62);
      IF (cgs AND (N < 4)) THEN units := cgsunits[N];
      IF ((NOT cgs) AND (N<4)) THEN units := mksunits[N];
      IF N > 3 THEN units := ' degs.';
      IF N < 4 THEN SetColor(FieldColor[N])
               ELSE SetColor(FieldColor[N-3]);
      IF axerr > 0 THEN
      begin
         BEEP;
         IF axerr = 1 THEN annotate('Horizontal scaling error. Press F2.')
         ELSE annotate('Vertical scaling error. Press F2.');
         dynamicpause;
      end
      ELSE
      begin
         PlotData(x, y, 61);
         GraphBackColor := DarkGray;
         OpenViewPort(7);
         Case N Of
            1: Print(1,1,' MAGNETIC FIELD PLOT  ');
            2: print(1,1,' MAGNETIC INDUCTION B ');
            3: print(1,1,' MAGNETIZATION FIELD  ');
            4: print(1,1,'    DIRECTION OF H    ');
            5: print(1,1,'    DIRECTION OF B    ');
         End; (*CASE *)
         SelectViewPort(3);
         print(1, 8, 'Units: ');
         long :=  LENGTH(ScNumStr(multiplier,1));
         print(8, 8,ScNumStr(multiplier,1) );
         print(9+long, 8, units);
         IF EscapeFromThis Then GraphingOver := TRUE;
        end;
      end;
    GetGraphLine(xstart, ystart, xend, yend, inside);
    UNTIL (EscapeFromThis OR GraphingOver);
    ShowScaleColors;
   End; (*GraphField Procedure *)

   Procedure StartUp;
 (*Gets starting data, initializes variable, etc. *)
   Begin
(*    ClearMuppetPort; *)
      If SimulateType = 2 Then
         Hfar := Ho
      Else
      Hfar := 0;
      Coeff.init(50,50);
      DisplayMat.init(MatSize, MatSize);
      MatHx.init(MatSize, MatSize);
      MatHy.init(MatSize, MatSize);
      DefineScale(1, -scale, scale, -scale, scale);
      CloseViewPort(5);
      DefineSurface(ShapeType, H, D);
      ShowShape;
      PrintParameters;
      SetColor(whitte);
      If SimulateType = 2 Then
         RelaxField
      Else
         MakePermanent;
      FindHmatrices;
      ShowShape;
      ShowField;
   End;

   Procedure SetUp;
 (*Sets up viewports, menus and HotKeys *)
   Begin
      GraphBackColor := blakk;
      DefineViewport(1, 0.37, 0.97, 0.105, 0.89);  (* field plot screen *)
      DefineViewport(2, 0.075, 0.35, 0.16, 0.53);   (* lower left *)
      DefineViewPort(3, 0.01, 0.35, 0.55, 0.94);   (* upper left *)
      DefineViewPort(4, 0.00, 0.36, 0.05, 0.54);  (* clear lower left *)
      DefineViewPort(5, 0, 1, 0.05, 0.95);         (* clear all *)
      DefineViewPort(6, 0.375, 0.99, 0.90, 0.955); (* upper rhs *)
      DefineViewPort(7, 0.04, 0.31, 0.05, 0.10);   (* label lhs *)
      DefineViewPort(8, 0.38, 0.96, 0.05, 0.10);   (* label rhs *)
      DefineViewPort(9, 0.375, 0.8, 0.875, 0.93);
      DefineViewPort(10, 0.361, 1.0, 0.105,0.95); (* clear rhs *)
      ThreeD.init(1);
      DeFaults;
      SetUpMenu;
      SetUpHotKeys;
      SetConstants;
      QuitFlag := false;
   End;

   Procedure ShutDown;
   Begin
      MyMenu.done;
      ThreeD.done;
      DisplayMat.free;
      MatHx.free;
      MatHy.free;
      CupsDone;
   End;

   Procedure HandleMenu;
   Begin
      With MyMenu Do
         BEGIN
           Case ColChosen Of
            1:  Case RowChosen Of
                  1:  AboutCUPS;
                  2:  Credits;
                  3:  MainHelp;
                  4:  UnitsHelp;
                  5:  Configuration;
                  6:  QuitFlag := true;
               End;

            2: If (RowChosen < 6) AND (DisplayType < 3) THEN
                  GraphField(rowChosen)
               ELSE If (RowChosen = 6) AND (DisplayType < 3) Then Explore
                    ELSE PlotSources(RowChosen-6,1);

            3: Begin
                  FieldToShow := RowChosen;
                  Showshape;
                  SetColor(Magenta);
                  PlotSymbol(0, 0, '+');
                  PrintParameters;
                  ShowField
               End;

            4: Begin
                  SelectViewPort(10);
                  ClearViewPort;
                  SelectViewPort(1);
                  DisplayType := rowChosen;
                  Plot3D := false;
                  Case rowChosen Of
                     1: Begin
                           ShowShape;
                           PrintParameters;
                           Showfield
                        End;
                     2: Begin
                           HideCursor;
                           DoContour;
                           SelectScale(4);
                           DrawShape(Whitte);
                           ShowCursor;
                        End;
                     3: DrawThreeD;
                  End (*Case *)
               End;

            5: Begin  (*New Shape *)
                  ShapeType := rowChosen;
                  DefineSurface(ShapeType, H, D);
                  ShowShape;
                  PrintParameters;
                  If SimulateType = 2 Then RelaxField
                     Else MakePermanent;
                  FindHMatrices;
                  ShowShape;
                  ShowField;
               End;

            6: Begin
                  Case rowchosen Of
                     1: ShapeData;
                     2: TypeData;
                     3: DisplayData;
                     4: BEGIN
                           cgs := NOT cgs;
                           PrintParameters;
                        END;
                  End;
                  IF rowchosen < 4 THEN StartUp
               End;

         End; (*Case *)
      END;
   End;

   Procedure HandleHotKey (key: BYTE);
   Begin
      Case key Of
         1:
            WhatToDo;
         2: 
            Begin
               Showshape;
               SetColor(Magenta);
               PlotSymbol(0, 0, '+');
               ShowField;
               Plot3D := FALSE;
            End;
         3: 
            if MyMenu.Chosen then HandleMenu;

      End; (*CASE *)
   End;

 (*-------------------------  Main Program  -------------------------------- *)


Begin
   CUPSinit;
   CreditsOn := TRUE; (* Flag to show Credits at first interesting screen *)
   Setup;
   StartUp;
   Repeat
      CheckForEvents;
      If HotKeys.Pressed(key) Then
         HandleHotKey(key);
      If MyMenu.Activated Then
         HandleMenu;
      if plot3D then ThreeD.check;
   Until QuitFlag;
   ShutDown;
End.