           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.6 (95/05/17) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

   {*************************************************}
   {*************************************************}
   {**         Program TwoD_RandomWalk             **}
   {**                                             **}
   {** This program models a Random Walk on either **}
   {** a two dimensional grid or two dimensional   **}
   {** plane.                                      **}
   {**                                             **}
   {** A random path of numj steps is plotted nums **}
   {** times.                                      **}
   {**                                             **}
   {** In the lower left corner is a plot of the   **}
   {** final displacement along the x axis and the **}
   {** corresponding Binominal Distribution.  In   **}
   {** the lower right corner is a plot of the     **}
   {** final displacement along the y axis and the **}
   {** corresponding Binominal Distribution.  In   **}
   {** the upper right corner is a plot of the     **}
   {** final radial displacement.  At the          **}
   {** conclusion of a run, the average x          **}
   {** displacement, y displacement and radial     **}
   {** displacement is given.                      **}
   {**                                             **}
   {**       Written by L. B. Spornick             **}
   {**       Applied Physics Laboratory            **}
   {**       The Johns Hopkins University          **}
   {**                                             **}
   {**        July 12, 1992                        **}
   {**            ver 1.0, July, 1993              **}
   {**            ver 1.1, Nov, 1993               **}
   {**            ver 1.2, Jan, 1994               **}
   {**            ver 1.3, May, 1994               **}
   {**            ver 1.4, Apr, 1995               **}
   {**                                             **}
   {*************************************************}
   {*************************************************}


PROGRAM TwoD_RandomWalk;

USES
   Graph, CUPSMUPP, CUPS, CUPSgui, CUPSfunc, Crt;

TYPE
   gridarray = ARRAY[1..41] OF INTEGER;
   yplotarray = ARRAY[1..280] OF INTEGER;
   countarray = ARRAY[1..41, 1..41] OF INTEGER;
   binarray = ARRAY[1..41] OF REAL;
   rayarray = ARRAY[1..20] OF REAL;

VAR
   deltat: INTEGER;
   STEP: BOOLEAN;
   quitf: BOOLEAN;
   restart: BOOLEAN;
   cancel : boolean;
   TwoD_Output: STRING;
   first: BOOLEAN;

   numj, nums, i, error, ymiddle, max : integer;
   xleft, x1, ytop, screenkey: INTEGER;
   p: REAL;
   x, xx, xy, xr, y, countx, county, countr: gridarray;
   yy, yr: yplotarray;
   count: countarray;
   Rayleigh: rayarray;
   binomial: binarray;
   F: TEXT;
   HotKeys: THotKeys;
   key: BYTE;
   MyMenuOut: TMenu;


PROCEDURE HELP1;
   VAR
      helpScr: HelpScrType;
BEGIN
   helpScr[1] := '                                                 ';
   helpScr[2] := '          Random Walk in Two Dimensions          ';
   helpScr[3] := '                                                 ';
   helpScr[4] := '   The grid of dots in the upper left corner     ';
   helpScr[5] := '   represents a two dimensional coordinate       ';
   helpScr[6] := '   system in which nums random walks of length   ';
   helpScr[7] := '   numj are made.  numj and nums are inputs.     ';
   helpScr[8] := '                                                 ';
   helpScr[9] := '   The probability that a random walk will end   ';
   helpScr[10] := '   at coordinates (m,n) starting at coordinates  ';
   helpScr[11] := '   (numj+1,numj+1) is p(2 numj,m) p(2 numj,n)    ';
   helpScr[12] := '   where p(2 numj,m) is the Binomial Dist.       ';
   helpScr[13] := '                                                 ';
   helpScr[14] := '   The bar chart in the lower left is a plot of  ';
   helpScr[15] := '   the distribution of end points along the x    ';
   helpScr[16] := '   axis.  The bar chart in the lower right is a  ';
   helpScr[17] := '   plot of the distribution of end points along  ';
   helpScr[18] := '   the y axis.  The blue bars are the            ';
   helpScr[19] := '   corresponding Binomial Distribution. The      ';
   helpScr[20] := '   plot in the upper right corner is the radial  ';
   helpScr[21] := '   distribution.                                 ';
   helpScr[22] := '                                                 ';
   helpScr[23] := '                                                 ';
   helpScr[24] := '                                                 ';
   helpScr[25] := '                                                 ';
   HELP(helpScr);
END;   {  PROCEDURE HELP1  }

PROCEDURE HELP2;
   VAR
      helpScr: HelpScrType;
BEGIN
   helpScr[1] := '                                                 ';
   helpScr[2] := '          Random Walk in Two Dimensions          ';
   helpScr[3] := '                                                 ';
   helpScr[4] := '   The screen in the upper left corner           ';
   helpScr[5] := '   represents a two dimensional coordinate       ';
   helpScr[6] := '   system in which nums random walks of numj     ';
   helpScr[7] := '   steps are made.  numj and nums are inputs.    ';
   helpScr[8] := '                                                 ';
   helpScr[9] := '   The steps are of equal length but random      ';
   helpScr[10] := '   direction.                                    ';
   helpScr[11] := '                                                 ';
   helpScr[12] := '   The bar chart in the lower left is a plot of  ';
   helpScr[13] := '   the final x displacement and the              ';
   helpScr[14] := '   corresponding Binomial Displacement.  The     ';
   helpScr[15] := '   bar chart in the lower right is a plot of the ';
   helpScr[16] := '   final y displacement and the corresponding    ';
   helpScr[17] := '   Binominal Displacement.  The bar chart in the ';
   helpScr[18] := '   upper right is a plot of the final radial     ';
   helpScr[19] := '   displacements and the corresponding Rayleigh  ';
   helpScr[20] := '   Distribution Displacement.                    ';
   helpScr[21] := '                                                 ';
   helpScr[22] := '                                                 ';
   helpScr[23] := '                                                 ';
   helpScr[24] := '                                                 ';
   helpScr[25] := '                                                 ';
   HELP(helpScr);
END;    {  PROCEDURE HELP2  }

PROCEDURE ABOUTPROGRAM;
   VAR
      helpScr: HelpScrType;
BEGIN
   helpScr[1] := '                                                 ';
   helpScr[2] := '                                                 ';
   helpScr[3] := '         A Random Walk in Two Dimensions         ';
   helpScr[4] := '                                                 ';
   helpScr[5] := '                       by                        ';
   helpScr[6] := '                                                 ';
   helpScr[7] := '                  L.B. Spornick                  ';
   helpScr[8] := '           Applied Physics Laboratory            ';
   helpScr[9] := '          The Johns Hopkins University           ';
   helpScr[10] := '                                                 ';
   helpScr[11] := '                 April 1995                      ';
   helpScr[12] := '              Copyright (c) 1995                 ';
   helpScr[13] := '                                                 ';
   helpScr[14] := '  This program models a random walk in which     ';
   helpScr[15] := '  the drunk can either take equally spaced steps ';
   helpScr[16] := '  on a two dimensional grid or can take equally  ';
   helpScr[17] := '  spaced steps in any direction.  This program   ';
   helpScr[18] := '  demonstrates the Binominal Distribution and    ';
   helpScr[19] := '  the Rayleigh Distribution.                     ';
   helpScr[20] := '                                                 ';
   helpScr[21] := '                                                 ';
   helpScr[22] := '                                                 ';
   helpScr[23] := '                                                 ';
   helpScr[24] := '                                                 ';
   helpScr[25] := '                                                 ';
   HELP(helpScr);
END;  {  PROCEDURE ABOUTPROGRAM  }

FUNCTION Power (x: REAL; y: INTEGER): REAL;
   VAR
      i: LONGINT;
      s: REAL;
BEGIN
   s := 1.0;
   IF (y > 0) THEN
      FOR i := 1 TO y DO
         s := s * x;
   IF (y < 0) AND (x <> 0.0) THEN
      FOR i := 1 TO Abs(y) DO
         s := s / x;
   Power := s;
END;  { FUNCTION Power }

FUNCTION Factor (i: INTEGER): REAL;
   VAR
      s: REAL;
      i1: LONGINT;
BEGIN
   s := 1;
   IF i > 0 THEN
      FOR i1 := 1 TO i DO
         s := s * i1;
   Factor := s;
END;   { FUNCTION Factor }

PROCEDURE SetUpMyMenu;
BEGIN
   MyMenuOut.Init;
   WITH MyMenuOut DO
   BEGIN
      column(1, 'File');
      row(1, 1, 'About Program');
      row(1, 2, 'About CUPS');
      row(1, 3, 'Configuration');
      row(1, 4, '---------------');
      row(1, 5, 'Restart Program');
      row(1, 6, '---------------');
      row(1, 7, 'Exit Program');
      rowactivate(1,4,false);
      rowactivate(1,6,false);
   END;
   MyMenuOut.Display;
END;  { PROCEDURE SetUpMyMenu }

PROCEDURE HandleMenu (VAR restart, quitf: BOOLEAN);
BEGIN
   WITH MyMenuOut DO
   BEGIN
      CASE rowChosen OF
         1: ABOUTPROGRAM;
         2: ABOUTCUPS;
         3: configuration;
         5: restart := true;
         7: quitf := NOT quitf;
      END; { CASE }
   END;  { WITH MyMenuOut DO }
END; { PROCEDURE HandleMenu }

PROCEDURE SetUpHotKeys (VAR HotKeys: THotKeys);
BEGIN
   WITH HotKeys DO
   BEGIN
      Init(3);
      key[1] := 'F1-Help';
      key[2] := 'F3-Delay';
      key[3] := 'F10-Menu';
   END;
END;  { PROCEDURE SetUpHotKeys }

PROCEDURE HandleHotKeys (key: BYTE; screenkey: INTEGER;
                         VAR STEP: BOOLEAN; VAR restart, quitf: boolean);
BEGIN
   CASE key OF
      1: IF (screenkey = 1) THEN HELP1 ELSE HELP2;
      2: STEP := NOT STEP;  { F3 - Step }
      3:
      BEGIN
         MouseBorders(0, 0, GetMaxX, GetMaxY);
         IF MyMenuOut.chosen THEN
            HandleMenu(restart, quitf);
      END;
   END;  { CASE }
END;  { PROCEDURE HandleHotKeys }

PROCEDURE ReadData;
   VAR
      screen: TInputScreen;
      nr, error: INTEGER;
BEGIN
   WITH screen DO
   BEGIN
      init;
      DefineInputPort(0.1, 0.87, 0.15, 0.85);
      LoadLine('            Random Walk In Two Dimensions      ');
      LoadLine('                                                    ');
      LoadLine('  Type of Walk:                                 ');
      LoadLine('   #1 Random Walk on a Grid                         ');
      LoadLine('   #1 Random Walk on a Plane                        ');
      LoadLine('                                                    ');
      LoadLine('  Number of Steps: {     } ( 5 - 10 for Grid)      ');
      LoadLine('                           (10 - 15 for Plane)      ');
      LoadLine('                                                   ');
      LoadLine('  Number of Walks: {     } (200 - 400)            ');
      LoadLine('                                                   ');
      LoadLine('  Output File Name:                                ');
      LoadLine('   "                                    "          ');
      LoadLine('                                                   ');
      LoadLine('               [   Ok   ]   [Cancel]               ');
      SetRadioButton('1', screenkey);
      SetNumber(3, numj);
      SetNumber(4, nums);
      SetNumberLimits(4, 200, 400);
      REPEAT
         error := 0;
         SetString(5, TwoD_Output);
         Accept;
         screenkey := GetRadioButton('1');
         numj := Trunc(GetNumber(3));
         nums := Trunc(GetNumber(4));
         TwoD_Output := GetString(5);
         IF (((screenkey = 1) AND ((numj < 5) OR (numj > 10)))
          OR ((screenkey = 2) AND (numj < 10) OR (numj > 15))) THEN
         BEGIN
            Announce('Error in Number of Steps');
            error := 1;
         END;
      UNTIL (error = 0);
      IF (canceled) THEN
         quitf := true;
      DONE;
   END;
END;  { PROCEDURE ReadData }

PROCEDURE ReadData1;
   VAR
      screen: TInputScreen;
      nr, error: INTEGER;
BEGIN
   WITH screen DO
   BEGIN
      init;
      DefineInputPort(0.1, 0.87, 0.15, 0.85);
      LoadLine('            Random Walk In Two Dimensions      ');
      LoadLine('                                                    ');
      LoadLine('  Type of Walk:                                 ');
      LoadLine('  #1 Random Walk on a Grid                          ');
      LoadLine('  #1 Random Walk on a Plane                         ');
      LoadLine('                                                    ');
      LoadLine('  Number of Steps: {    }  ( 5 - 10 for Grid)      ');
      LoadLine('                           (10 - 15 for Plane)      ');
      LoadLine('                                                    ');
      LoadLine('  Number of Walks: {    }  (200 - 400)              ');
      LoadLine('                                                   ');
      LoadLine('               [   Ok   ]   [Cancel]               ');
      SetRadioButton('1', screenkey);
      SetNumber(3, numj);
      SetNumber(4, nums);
      SetNumberLimits(4, 200, 400);
      REPEAT
         error := 0;
         Accept;
         screenkey := GetRadioButton('1');
         numj := Trunc(GetNumber(3));
         nums := Trunc(GetNumber(4));
         IF (((screenkey = 1) AND ((numj < 5) OR (numj > 10)))
          OR ((screenkey = 2) AND (numj < 10) OR (numj > 15))) THEN
         BEGIN
            Announce('Error in Number of Steps');
            error := 1;
         END;
      UNTIL (error = 0);
      cancel := canceled;
      DONE;
   END;
END;  { PROCEDURE ReadData1 }

PROCEDURE InitializeGrid (screenkey: INTEGER; numj, nums: INTEGER;
          p: REAL; VAR max, ymiddle, xleft, x1, ytop: INTEGER;
          VAR x, y, xx, xy, xr: gridarray; VAR binomial: binarray;
          VAR Rayleigh: rayarray; VAR yy, yr: yplotarray);
   VAR
      dx, dx1, xright, xwidth, xborder, xborder1,dy, dy1 : integer;
      dr, ybottom, ylength, yborder, i, j, maxr, n, n1, n2: INTEGER;
      r, raytotal, sigma2, average: REAL;
      textinfo: TextSettingsType;
      Font, CharSize, Horiz: WORD;
      OutputText: STRING;
BEGIN
   DefineViewPort(1, 0.1, 0.9, 0.1, 0.9);
   DefineScale(1, 0.1, 0.9, 0.1, 0.9);
   OpenViewPort(1);
   SelectScale(1);
   SelectViewPort(1);
   SetColor(white);
   OutputText := 'A Random Walk in Two Dimensions - '+ NumStr(numj, 3, 0)
               + ' Steps, ' + NumStr(nums, 3, 0) + ' Trials';
   PutLabel(top, OutputText);
   map(0.1, 0.1, xleft, ybottom);
   map(0.9, 0.9, xright, ytop);
   x1 := Round((xright + xleft) / 2.0);
   ymiddle := Round((ytop + ybottom) / 2.0);
   xwidth := x1 - xleft;
   ylength := ymiddle - ytop;

   SetColor(white);

   Line(xleft, ymiddle, xright, ymiddle);
   Line(x1, ytop, x1, ybottom);

   { determine the border and location of points in the grid }

   dx := xwidth DIV (2 * numj + 2);
   xborder := (xwidth - (2 * numj + 1) * dx);
   IF (xborder >= 2 * dx) THEN
      xborder := xborder DIV 2;
   dx1 := xwidth DIV (numj + 2);
   xborder1 := (xwidth - (numj + 1) * dx);
   IF (xborder1 >= 2 * dx) THEN
      xborder1 := xborder1 DIV 2;

   dy := ylength DIV (2 * numj + 2);
   yborder := (ylength - (2 * numj + 1) * dy);
   IF (yborder >= 2 * dy) THEN
      yborder := yborder DIV 2;

   FOR i := 1 TO (2 * numj + 1) DO
   BEGIN
      x[i] := xleft + xborder + (i - 1) * dx;
      y[i] := ytop + yborder + (i - 1) * dy;
   END;

   IF (screenkey = 1) THEN
   BEGIN
      SetFillStyle(solidfill, white);
      FOR i := 1 TO (2 * numj + 1) DO
         FOR j := 1 TO (2 * numj + 1) DO
            FillEllipse(x[i], y[j], 1, 1);
   END;
   SetColor(Blue);
   Line(x[numj + 1], y[1], x[numj + 1], y[2 * numj + 1]);
   Line(x[1], y[numj + 1], x[2 * numj + 1], y[numj + 1]);
   SetColor(white);

   { determine the binomial distribution }

   max := 0;
   FOR i := 1 TO (2 * numj + 1) DO
   BEGIN
      n := i - 1;
      binomial[i] := nums * factor(2 * numj) * Pwr(p, n) *
                     Pwr(1.0 - p, 2 * numj - n) /
                     (factor(n) * factor(2 * numj - n));
      IF (max < Round(binomial[i])) THEN
         max := Round(1.6 * binomial[i]);
   END;
   max := Round(1.3 * max);

   { determine plot coordindates }

   FOR i := 1 TO (2 * numj + 1) DO
   BEGIN
      xx[i] := x[i];
      xy[i] := x1 + x[i];
   END;

   FOR i := 1 TO (numj + 1) DO
      xr[i] := x1 + xborder + (i - 1) * dx1;

   dy := ylength DIV max;

   FOR i := 1 TO max DO
      yy[i] := ybottom - (i - 1) * dy;

   { determine the Rayleigh Distribution }
   maxr := 0;
   raytotal := 0.0;
   sigma2 := numj / 2.0;
   FOR i := 0 TO numj - 1 DO
   BEGIN
      Rayleigh[i + 1] := exp(-SQR(i) / (2.0 * sigma2)) -
                         exp(-SQR(i + 1) / (2.0 * sigma2));
      raytotal := raytotal + Rayleigh[i + 1];
   END;
   FOR i := 1 TO (numj - 1) DO
   BEGIN
      Rayleigh[i] := nums * Rayleigh[i] / raytotal;
      IF (maxr < Round(Rayleigh[i] + 1)) THEN
         maxr := Round(Rayleigh[i] + 1);
   END;
   maxr := Round(1.6 * maxr);
   dy1 := ylength DIV maxr;
   FOR i := 1 TO maxr DO
      yr[i] := ymiddle - (i - 1) * dy1;

     { annotate the screen }

   SetTextJustify(1, 1);
   GetTextSettings(textinfo);
   Font := textinfo.Font;
   Horiz := textinfo.Horiz;
   CharSize := textinfo.CharSize;
   SetTextStyle(2, HorizDir, CharSize DIV 2);
   OutTextXY(xx[numj + 1] - 5, ybottom + 10, '0');
   OutTextXY(xy[numj + 1] - 5, ybottom + 10, '0');
   OutTextXY(xx[1], ybottom + 10, NumStr(-numj, 3, 0));
   OutTextXY(xy[1], ybottom + 10, NumStr(-numj, 3, 0));
   OutTextXY(xx[2 * numj + 1], ybottom + 10, NumStr(numj, 2, 0));
   OutTextXY(xy[2 * numj + 1], ybottom + 10, NumStr(numj, 2, 0));
   IF NOT Odd(numj) THEN
   BEGIN
     OutTextXY(xx[(numj DIV 2) +1], ybottom +10, NumStr(-(numj DIV 2),1,0));
     OutTextXY(xy[(numj DIV 2) +1], ybottom +10, NumStr(-(numj DIV 2),1,0));
     OutTextXY(xx[3*numj DIV 2 +1], ybottom +10, NumStr( (numj DIV 2),1,0));
     OutTextXY(xy[3*numj DIV 2 +1], ybottom +10, NumStr( (numj DIV 2),1,0));
   END
   ELSE
   BEGIN
     OutTextXY(xx[(numj DIV 2) +2], ybottom +10, NumStr(-(numj DIV 2),1,0));
     OutTextXY(xy[(numj DIV 2) +2], ybottom +10, NumStr(-(numj DIV 2),1,0));
     OutTextXY(xx[3*numj DIV 2 +1], ybottom +10, NumStr( (numj DIV 2),1,0));
     OutTextXY(xy[3*numj DIV 2 +1], ybottom +10, NumStr( (numj DIV 2),1,0));
   END;

   OutTextXY(xleft - 10, yy[max], NumStr(max, 3, 0));
   OutTextXY(xright + 10, yy[max], NumStr(max, 3, 0));
   OutTextXY(xright + 10, yr[maxr], NumStr(maxr, 3, 0));
   FOR i := 1 TO numj + 1 DO
      OutTextXY(xr[i], ymiddle + 7, NumStr(i - 1, 2, 0));
   Line(xleft, yy[max], xleft + 8, yy[max]);
   Line(x1 - 8, yy[max], x1 + 8, yy[max]);
   Line(xright - 8, yy[max], xright, yy[max]);
   Line(xright - 8, yr[maxr], xright, yr[maxr]);

   OutTextXY(xleft - 10, yy[max DIV 2], NumStr(max DIV 2, 3, 0));
   OutTextXY(xright + 10, yy[max DIV 2], NumStr(max DIV 2, 3, 0));
   OutTextXY(xright + 10, yr[maxr DIV 2], NumStr(maxr DIV 2, 3, 0));
   Line(xleft, yy[max DIV 2], xleft + 8, yy[max DIV 2]);
   Line(x1 - 8, yy[max DIV 2], x1 + 8, yy[max DIV 2]);
   Line(xright - 8, yy[max DIV 2], xright, yy[max DIV 2]);
   Line(xright - 8, yr[maxr DIV 2], xright, yr[maxr DIV 2]);

   SetTextStyle(Font, HorizDir, CharSize);

   OutTextXY(xleft + 130, ymiddle + 20, 'x Axis Distribution');
   OutTextXY(x1 + 130, ymiddle + 20, 'y Axis Distribution');
   OutTextXY(x1 + 130, ytop + 20, ' Radial Distribution');

     { draw the Binomial Distribution on the x,y axis plots }

   SetColor(Blue);
   FOR i := 1 TO (2 * numj + 1) DO
   BEGIN
      n := Round(binomial[i]) + 1;
      Line(xx[i] - 5, yy[n], xx[i] + 2, yy[n]);
      Line(xx[i] - 5, yy[n] - 1, xx[i] + 2, yy[n] - 1);
      Line(xx[i] - 5, yy[n], xx[i] - 5, ybottom);
      Line(xx[i] + 2, yy[n], xx[i] + 2, ybottom);
   END;
   FOR i := 1 TO (2 * numj + 1) DO
   BEGIN
      n := Round(binomial[i]) + 1;
      Line(xy[i] - 5, yy[n], xy[i] + 2, yy[n]);
      Line(xy[i] - 5, yy[n] - 1, xy[i] + 2, yy[n] - 1);
      Line(xy[i] - 5, yy[n], xy[i] - 5, ybottom);
      Line(xy[i] + 2, yy[n], xy[i] + 2, ybottom);
   END;

     { if screenkey = 2 draw the Rayleigh Distribution on the r axis plot }

   IF (screenkey = 2) THEN
   BEGIN
      FOR i := 1 TO numj DO
      BEGIN
         n := Round(Rayleigh[i]) + 1;
         Line(xr[i] - 3, yr[n], xr[i] + 7, yr[n]);
         Line(xr[i] - 3, yr[n] - 1, xr[i] + 7, yr[n] - 1);
         Line(xr[i] - 3, yr[n], xr[i] - 3, ymiddle);
         Line(xr[i] + 7, yr[n], xr[i] + 7, ymiddle);
      END;
   END;
   SetColor(white);
   SetTextJustify(0, 2);

END;  { PROCEDURE Initializegrid }

PROCEDURE Paths (screenkey: INTEGER; max, deltat, numj, nums, ymiddle,
          xleft, x1, ytop: INTEGER; binomial: binarray;
          Rayleigh: rayarray; x, y, xx, xy, xr: gridarray;
          yy, yr: yplotarray; STEP: BOOLEAN;
          VAR countx, county, countr: gridarray; VAR count: countarray;
          VAR quitf, restart: BOOLEAN);
   VAR
      i, i1, ir, m, n, nmax, n1, dgx, dgy, gstepl, maxr: INTEGER;
      r, lengthr, dr, d2r, rx, ry, dx, dy, theta, sigmax : real;
      sigmay, sigmar, averagex, averagey, averager: REAL;
      xe, ye: ARRAY[1..41] OF INTEGER;
BEGIN
   maxr := Round(1.3 * max);
   averagex := 0.0;
   averagey := 0.0;
   averager := 0.0;
   sigmax := 0.0;
   sigmay := 0.0;
   sigmar := 0.0;
   dr := 1.0;
   nmax := 2 * numj + 1;
   FOR i1 := 1 TO nmax DO
   BEGIN
      countx[i1] := 0;
      county[i1] := 0;
      countr[i1] := 0;
      FOR i := 1 TO nmax DO
         count[i1, i] := 0;
   END;  { FOR i1 := 1 TO nmax DO }

   FOR i1 := 1 TO nums DO
   BEGIN
      IF ((quitf = false) AND (restart = false)) THEN
      BEGIN
         IF (screenkey = 1) THEN
         BEGIN
      { randomly select and draw turn }
            m := numj + 1;
            n := numj + 1;
            xe[1] := x[m];
            ye[1] := y[n];
            FOR i := 1 TO numj DO
            BEGIN
               r := Random;
               IF (r < 0.25) THEN
               BEGIN   { positive x turn }
                  INC(m);
                  Line(x[m - 1], y[n], x[m], y[n]);
               END
               ELSE IF (r < 0.5) THEN
               BEGIN   { positive y turn }
                  Inc(n);
                  Line(x[m], y[n - 1], x[m], y[n]);
               END
               ELSE IF (r < 0.75) THEN
               BEGIN   { negative x turn }
                  Dec(m);
                  Line(x[m + 1], y[n], x[m], y[n]);
               END
               ELSE
               BEGIN   { negative y turn }
                  Dec(n);
                  Line(x[m], y[n + 1], x[m], y[n]);
               END;
               IF STEP THEN Delay(deltat * 3);
               xe[i + 1] := x[m];
               ye[i + 1] := y[n];
            END; { FOR i := 1 TO numj DO }
            Circle(x[m], y[n], 2);
            IF STEP THEN Delay(deltat * 3);
            lengthr := (SQR(m - numj - 1) + SQR(n - numj - 1));
            lengthr := SQRT(lengthr);
            averagex := averagex + m - numj - 1;
            averagey := averagey + n - numj - 1;
            averager := averager + lengthr;
            sigmax := sigmax + SQR(m - numj - 1);
            sigmay := sigmay + SQR(n - numj - 1);
            sigmar := sigmar + SQR(lengthr);
         END  {  IF (screenkey = 1) THEN }
         ELSE
         BEGIN   { screenkey = 2 }
            rx := 0.0;
            ry := 0.0;
            xe[1] := x[numj + 1];
            ye[1] := y[numj + 1];
            r := 1.0;
            gstepl := 10;
            d2r := Pi / 180.0;
            FOR i := 1 TO numj DO
            BEGIN
               theta := random(360) * d2r;
               dx := r * cos(theta);
               dy := r * sin(theta);
               rx := rx + dx;
               ry := ry + dy;
               dgx := Round(dx * gstepl);
               dgy := Round(dy * gstepl);
               xe[i + 1] := xe[i] + dgx;
               ye[i + 1] := ye[i] + dgy;
               line(xe[i], ye[i], xe[i + 1], ye[i + 1]);
            END;  { FOR i := 1 TO numj DO }
            m := Round(rx) + numj + 1;
            n := Round(ry) + numj + 1;
            Circle(xe[numj + 1], ye[numj + 1], 2);
            lengthr := SQRT(SQR(rx) + SQR(ry));
            averagex := averagex + rx;
            averagey := averagey + ry;
            averager := averager + lengthr;
            sigmax := sigmax + SQR(rx);
            sigmay := sigmay + SQR(ry);
            sigmar := sigmar + SQR(lengthr);
         END;  { ELSE - screenkey = 2 }
      { increment the appropriate bins }
         Inc(count[m, n]);
         Inc(countx[m]);
         Inc(county[n]);
         FOR i := 1 TO numj + 1 DO
         BEGIN
            IF (lengthr >= (i - 1) * dr) AND (lengthr < i * dr) THEN
            BEGIN
               Inc(countr[i]);
               ir := i;
            END;
         END;

      { add the new points to the plots }

      { x axis plot }
         IF (countx[m] > 0) AND (countx[m] <= max) THEN
         BEGIN
            Line(xx[m] - 3, yy[1], xx[m] - 3, yy[countx[m] + 1]);
            Line(xx[m], yy[1], xx[m], yy[countx[m] + 1]);
            Line(xx[m] - 3, yy[countx[m] + 1], xx[m], yy[countx[m] + 1]);
            Line(xx[m] - 3, yy[1], xx[m], yy[1]);
         END;  { IF (countx[m] > 0) THEN }

      { y axis plot }
         IF (county[n] > 0) AND (county[n] <= max) THEN
         BEGIN
            Line(xy[n] - 3, yy[county[n]], xy[n] - 3, yy[county[n] + 1]);
            Line(xy[n], yy[county[n]], xy[n], yy[county[n] + 1]);
            Line(xy[n] - 3, yy[county[n] + 1], xy[n], yy[county[n] + 1]);
            Line(xy[n] - 3, yy[county[n]], xy[n], yy[county[n]]);
         END;  { IF (county[n] > 0) THEN }

      { radial plot }
         IF (countr[ir] > 0) AND (countr[ir] <= maxr) THEN
         BEGIN
            Line(xr[ir] -1, yr[countr[ir]], xr[ir] -1, yr[countr[ir] +1]);
            Line(xr[ir] +5, yr[countr[ir]], xr[ir] +5, yr[countr[ir] +1]);
            Line(xr[ir] -1, yr[countr[ir] +1], xr[ir] +5, yr[countr[ir] +1]);
            Line(xr[ir] -1, yr[countr[ir]], xr[ir] +5, yr[countr[ir]]);
         END;  { IF (count[ir] > 0) THEN }

         CheckForEvents;
         IF HotKeys.Pressed(key) THEN
            HandleHotKeys(key, screenkey, STEP, restart, quitf);
         IF (key = 3) THEN
         BEGIN
            MouseBorders(0, 0, GetMaxX, GetMaxY);
         END;
         IF (MyMenuOut.Activated) THEN
            HandleMenu(restart, quitf);
         MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
         Delay(deltat);
         IF STEP THEN Delay(deltat * 3);

       { erase previous path }

         SetColor(DarkGray);
         Circle(xe[numj + 1], ye[numj + 1], 2);
         FOR i := 1 TO numj DO
         BEGIN
            SetColor(DarkGray);
            Line(xe[i], ye[i], xe[i + 1], ye[i + 1]);
            SetColor(white);
            IF (screenkey = 1) THEN
            BEGIN
               SetFillStyle(solidFill, white);
               FillEllipse(xe[i], ye[i], 1, 1);
               FillEllipse(xe[i + 1], ye[i + 1], 1, 1);
            END;  { IF (screenkey = 1) THEN }
         END;  { FOR i := 1 TO numj DO }

         SetColor(Blue);
         Line(x[numj + 1], y[1], x[numj + 1], y[2 * numj + 1]);
         Line(x[1], y[numj + 1], x[2 * numj + 1], y[numj + 1]);
         SetColor(white);

     { draw the Binomial Distribution on the plots }

         SetColor(Blue);
         FOR i := 1 TO (2 * numj + 1) DO
         BEGIN
            n1 := Round(binomial[i]) + 1;
            Line(xx[i] - 5, yy[n1], xx[i] + 2, yy[n1]);
            Line(xx[i] - 5, yy[n1] - 1, xx[i] + 2, yy[n1] - 1);
            Line(xx[i] - 5, yy[n1], xx[i] - 5, yy[1]);
            Line(xx[i] + 2, yy[n1], xx[i] + 2, yy[1]);
         END;
         FOR i := 1 TO (2 * numj + 1) DO
         BEGIN
            n1 := Round(binomial[i]) + 1;
            Line(xy[i] - 5, yy[n1], xy[i] + 2, yy[n1]);
            Line(xy[i] - 5, yy[n1] - 1, xy[i] + 2, yy[n1] - 1);
            Line(xy[i] - 5, yy[n1], xy[i] - 5, yy[1]);
            Line(xy[i] + 2, yy[n1], xy[i] + 2, yy[1]);
         END;

     { if screenkey = 2 draw the Rayleigh Distribution on the r axis plot }

         IF (screenkey = 2) THEN
         BEGIN
            FOR i := 1 TO numj DO
            BEGIN
               n := Round(Rayleigh[i]) + 1;
               Line(xr[i] - 3, yr[n], xr[i] + 7, yr[n]);
               Line(xr[i] - 3, yr[n] - 1, xr[i] + 7, yr[n] - 1);
               Line(xr[i] - 3, yr[n], xr[i] - 3, ymiddle);
               Line(xr[i] + 7, yr[n], xr[i] + 7, ymiddle);
            END;
         END;
         SetColor(white);

      END;    { IF (quitf = false) AND (restart = false) ... }
   END;  { FOR i1 := 1 TO nums DO }
   IF ((quitf = false) AND (restart = false)) THEN
   BEGIN

      averagex := averagex / nums;
      averagey := averagey / nums;
      averager := averager / nums;
      sigmax := SQRT(sigmax / nums - SQR(averagex));
      sigmay := SQRT(sigmay / nums - SQR(averagey));
      sigmar := SQRT(sigmar / nums);

      OutTextXY(xleft +60, ymiddle +30, 'average x ' +NumStr(averagex,5,3));
      OutTextXY(x1 +60, ymiddle +30, 'average y ' +NumStr(averagey,5,3));
      OutTextXY(x1 +30, ytop +30, 'rms radial disp. ' +NumStr(sigmar,5,3));
      OutTextXY(x1 +30, ytop +42, 'exp. rms radial disp. '
                +NumStr(SQRT(numj), 5, 3));

      writeln(F, ' Summary of Walks Statistics');
      writeln(F);
      writeln(F, '  Final x axis       Actual Number     Theorical Number');
      writeln(F, '  displacement       of walks          of walks using a');
      writeln(F, '                                       Binominal Dist.');
      FOR i := 1 TO (2 * numj + 1) DO
         writeln(F, '     ', -numj + i - 1 : 3, '                 ',
                 countx[i] : 3, '              ', binomial[i] : 7 : 3);
      writeln(F, ' expected average final x displacement: 0.0');
      writeln(F, ' actual average final x displacement: ', averagex :10:7);
      writeln(F, ' actual one sigma in the x direction: ', sigmax : 10:7);
      writeln(F);
      writeln(F, '  Final y axis       Actual Number     Theorical Number');
      writeln(F, '  displacement       of walks          of walks using a');
      writeln(F, '                                       Binominal Dist.');
      FOR i := 1 TO (2 * numj + 1) DO
         writeln(F, '     ', -numj + i - 1 : 3, '                 ',
                 county[i] : 3, '              ', binomial[i] : 7 : 3);
      writeln(F, ' expected average final y displacement: 0.0');
      writeln(F, ' actual average final y displacement: ', averagey : 10:7);
      writeln(F, ' actual one sigma in the y direction: ', sigmay : 10:7);
      writeln(F);
      IF (screenkey = 1) THEN
      BEGIN
         writeln(F, '  Final r axis       Actual Number');
         writeln(F, '  displacement       of walks');
         FOR i := 1 TO (2 * numj + 1) DO
            writeln(F, '     ', -numj + i - 1 : 3, '                 ',
                    countr[i] : 3);
         writeln(F, ' rms radial displacement: ', sigmar : 5 : 3);
      END
      ELSE
      BEGIN
         writeln(F, '  Final r axis       Actual Number     Theorical Number');
         writeln(F, '  displacement       of walks          of walks using a');
         writeln(F, '                                       Rayleigh Dist.');
         FOR i := 1 TO (numj + 1) DO
            writeln(F, '     ', i - 1 : 3, '                 ',
                    countr[i] : 3, '            ', Rayleigh[i] : 5 : 3);
         writeln(F, ' rms radial displacement: ', sigmar : 5 : 3);
         writeln(F, ' expected rms radial displacement: ', SQRT(numj) :5:3);
      END;
      writeln(F);
      writeln(F, ' Distribution of End Points');
      writeln(F);
      write(F, 'x/y ');
      FOR i := 1 TO (2 * numj + 1) DO
         write(F, -numj + i - 1 : 3, ' ');
      writeln(F);
      FOR i := 1 TO (2 * numj + 1) DO
      BEGIN
         write(F, -numj + i - 1 : 3, ' ');
         FOR i1 := 1 TO (2 * numj + 1) DO
            write(F, count[i, i1] : 3, ' ');
         writeln(F);
      END;
      writeln(F);

   END;  {  IF ((quitf = false) AND (restart = false)) ...}
END;  { PROCEDURE Paths }


BEGIN { PROGRAM TwoD_RandomWalk }

CUPSinit;
ABOUTPROGRAM;
CUPSdone;
CUPSinit;
TwoD_Output := 'TwoD.dat';
deltat := 200;
STEP := false;
quitf := false;
restart := false;
first := true;
cancel := false;

Randomize;
screenkey := 1;
numj := 10;
nums := 200;

REPEAT  { UNTIL quitf = true}
   IF (quitf = false) THEN
   BEGIN
      restart := false;
      ClearMuppetPort;
      ShowCursor;
      IF (first = true) THEN
      BEGIN
         ReadData;
         first := false;
         ASSIGN(F, TwoD_Output);
         REWRITE(F);
      END
      ELSE
         repeat ReadData1 until (not cancel);
      ClearMuppetPort;
      HideCursor;
      SetUpHotKeys(HotKeys);
      HotKeys.Display;
      SetUpMyMenu;
      p := 0.5;
      IF (screenkey = 1) THEN
         writeln(F, ' Random Walk Confined to a Two Dimensional Grid')
      ELSE
         writeln(F, ' Random Walk Confined to a Two Dimensional Plane');
      writeln(F, ' Number of Steps in each Walk ', numj);
      writeln(F, ' Number of Walks ', nums);
      writeln(F);
      InitializeGrid(screenkey, numj, nums, p, max, ymiddle, xleft, x1,
                     ytop, x, y, xx, xy, xr, binomial, Rayleigh, yy, yr);
      MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
      ShowCursor;
      Paths(screenkey, max, deltat, numj, nums, ymiddle, xleft, x1,
            ytop, binomial, Rayleigh, x, y, xx, xy, xr, yy, yr, STEP,
            countx, county, countr, count, quitf, restart);
      MyMenuOut.Display;
      HideCursor;
      MouseBorders(0, 0, GetMaxX, GetMaxY);
      ShowCursor;
      IF ((restart = false) AND (quitf = false)) THEN
      BEGIN
         REPEAT
            CheckForEvents;
            IF (MyMenuOut.Activated) THEN
               HandleMenu(restart, quitf);
            IF HotKeys.Pressed(key) THEN
               IF (key <> 2) THEN
                  HandleHotKeys(key, screenkey, STEP, restart, quitf);
            IF (key = 3) THEN
            BEGIN
               MouseBorders(0, 0, GetMaxX, GetMaxY);
            END;
         UNTIL ((restart = true) OR (quitf = true));
      END;   { IF ((restart ... }
   END;  { IF (quitf = false) }
UNTIL (quitf = true);

{ Close(TwoD_Output);}
Close(F);
CUPSDone;

END. { PROGRAM TwoD_RandomWalk }