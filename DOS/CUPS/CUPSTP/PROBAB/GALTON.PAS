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
   {**         Program GALTON_BOARD    **}
   {**                  **}
   {** This programs models either a traditional   **}
   {** Galton Board or a customized Galton Board.  **}
   {**                  **}
   {** The traditional Galton board consists of **}
   {** a number of levels of pins spaced down a **}
   {** board.  Balls move down the board. At each **}
   {** level, the ball can move either one unit **}
   {** to the right or left.  The choice is  **}
   {** determined by the probablity p.  If p = 50, **}
   {** then there is equal probability that the **}
   {** ball will move to the right or left.  If **}
   {** p < 50, the ball is more likely to move to  **}
   {** the left, and if p > 50 the ball is more **}
   {** likely to move to the right.    **}
   {**                  **}
   {** The customized Galton board allows the   **}
   {** user to select the probability for each pin **}
   {** separately, add traps, reflecting walls  **}
   {** and/or absorbing walls.                   **}
   {**                  **}
   {** For both boards, the balls are collected **}
   {** in bins and the average and one sigma **}
   {** numbers are calculated.         **}
   {**                  **}
   {** For a small traditional Galton board  **}
   {** (nlevels <= 6 and nballs <= 100), the **}
   {** entire board is shown.  For the other cases **}
   {** only the bins are shown.        **}
   {**                  **}
   {** This program demonstrates a Random Walk, **}
   {** the Binomial Distribution, the Normal **}
   {** Distribution, a customized distribution and,**}
         {** the Central Limit Theorem.                   **}
   {**                  **}
   {**       Written by L. B. Spornick    **}
   {**       Applied Physics Laboratory      **}
   {**       The Johns Hopkins University **}
   {**                  **}
   {**      February 22, 1992               **}
         {**             Version 1.0 February, 1993      **}
   {**      Version 1.1 October, 1993       **}
   {**      Version 1.2 January, 1994       **}
   {**      Version 1.3 May, 1994           **}
         {**             Version 1.4 June, 1994          **}
         {**             Version 1.5 Apr, 1995           **}
   {**                  **}
   {*************************************************}
   {*************************************************}

PROGRAM GALTON_BOARD;

USES
   Graph, CUPSMUPP, CUPSgui, Crt, CUPS;

TYPE
   arrays = ARRAY[1..20, 1..20] OF LONGINT;
   vector = ARRAY[1..20] OF LONGINT;
   binarray = ARRAY[1..21] OF LONGINT;
   probarray = ARRAY[1..20, 1..20] OF REAL;
   typearray = ARRAY[1..20, 1..20] OF INTEGER;
   realarray = ARRAY[1..21] OF REAL;
   cenarray = ARRAY[1..80] OF INTEGER;
   normarray = ARRAY[1..80] OF REAL;
   chiarray = ARRAY[1..250] OF REAL;

CONST
   shape:   FillPatternType = ( $FF , $FF , $FF , $FF , $FF , $FF , $FF , $FF );
   deltat:  INTEGER = 750;
   STEP:    BOOLEAN = false;
   beepon:  BOOLEAN = false;
   quitf:   BOOLEAN = false;
   menuf:   BOOLEAN = false;
   restart: BOOLEAN = false;
   first:   BOOLEAN = true;
   cntlmthrm: BOOLEAN = false;
   Galton_Output: STRING   = 'Galton.dat';

VAR
   ballsize, htype, btype, center, error, nlevels, nballs: LONGINT;
   i, i1, i2, j, ilevel, ileveli, ilevelf, right, ybintop: LongInt;
   ybinbottom, small, ytop: LONGINT;
   xwidth, ybottom, numdiv, deltay, nlevelsold: LONGINT;
   nball, ninbin, numchi: INTEGER;
   x, y: arrays;
   xbin, ybinfill, icount: binarray;
   averagecount, xcen, ycen: cenarray;
   prob: probarray;
   binomialcount, customcount, ynormal: realarray;
   chiavg: chiarray;
   binw, p, rnumchi: REAL;
   actavg, average, avgchi, chi, chisigma2, onesigma, ri1 : extended;
   baverage, ei, binstart: EXTENDED;
   F: TEXT;
   StatisticOutput: STRING;
   HotKeys: THotKeys;
   HotKeys2: THotKeys;
   key: BYTE;
   MyMenuOut: TMenu;

PROCEDURE Help1;
   VAR
      helpScr: HelpScrType;
BEGIN
   helpScr[1] := '                                                 ';
   helpScr[2] := '                                                 ';
   helpScr[3] := '          The Traditional Galton Board           ';
   helpScr[4] := '                                                 ';
   helpScr[5] := '                                                 ';
   helpScr[6] := '   The bins collect the balls as they fall       ';
   helpScr[7] := '   through the grid of pins above.               ';
   helpScr[8] := '                                                 ';
   helpScr[9] := '   For a small Galton Board (nlevels <=6 and     ';
   helpScr[10] := '   nballs <= 100), the grid of pins is also      ';
   helpScr[11] := '   shown.                                        ';
   helpScr[12] := '                                                 ';
   helpScr[13] := '   Light Red markers indicate the Binomial       ';
   helpScr[14] := '   Distribution and a yellow line connects       ';
   helpScr[15] := '   points associated with the Normal             ';
   helpScr[16] := '   Distribution.                                 ';
   helpScr[17] := '                                                 ';
   helpScr[18] := '   When all of the balls have fallen, a summary  ';
   helpScr[19] := '   comparing the Actual results to the Binomial  ';
   helpScr[20] := '   and Normal Distributions is displayed.        ';
   helpScr[21] := '                                                 ';
   helpScr[22] := '   The process can be repeated to demonstrate    ';
   helpScr[23] := '   the Central Limit Theorem.                    ';
   helpScr[24] := '                                                 ';
   helpScr[25] := '                                                 ';
   HELP(helpScr);
END;   { PROCEDURE Help1 }

PROCEDURE Help2;
   VAR
      helpScr: HelpScrType;
BEGIN
   helpScr[1] := '                                               ';
   helpScr[2] := '           A Customized Galton Board           ';
   helpScr[3] := '                                               ';
   helpScr[4] := '  The user first selects the layout of the     ';
   helpScr[5] := '  board (the placement of traps, reflecting    ';
   helpScr[6] := '  walls, and/or absorbing walls) and the       ';
   helpScr[7] := '  probability characteristics of each pin.     ';
   helpScr[8] := '                                               ';
   helpScr[9] := '  The balls are dropped down the board and     ';
   helpScr[10] := '  the user watches them collect in the bins.   ';
   helpScr[11] := '                                               ';
   helpScr[12] := '  The processes can be repeated to demonstrate ';
   helpScr[13] := '  the Central Limit Theorem.                   ';
   helpScr[14] := '                                               ';
   helpScr[15] := '                                               ';
   helpScr[16] := '                                               ';
   helpScr[17] := '                                               ';
   helpScr[18] := '                                               ';
   helpScr[19] := '                                               ';
   helpScr[20] := '                                               ';
   helpScr[21] := '                                               ';
   helpScr[22] := '                                               ';
   helpScr[23] := '                                               ';
   helpScr[24] := '                                               ';
   helpScr[25] := '                                               ';
   HELP(helpScr);
END;    {  PROCEDURE Help2  }

PROCEDURE HELP3;
   VAR
      helpScr: HelpScrType;
BEGIN
   helpScr[1] := '                                                   ';
   helpScr[2] := '              The Central Limit Theorem            ';
   helpScr[3] := '                                                   ';
   helpScr[4] := '     The Central Limit Theorem states that the     ';
   helpScr[5] := '     sum of a large number of independent random   ';
   helpScr[6] := '     variables will be approximately normally      ';
   helpScr[7] := '     distributed almost regardless of their        ';
   helpScr[8] := '     individual distributions.                     ';
   helpScr[9] := '                                                   ';
   helpScr[10] := '     In this case, the independent random          ';
   helpScr[11] := '     variables are the averages for 250 similar    ';
   helpScr[12] := '     Galton Board runs and the individual          ';
   helpScr[13] := '     distributions are the 250 different final     ';
   helpScr[14] := '     distributions of the balls in the bins.       ';
   helpScr[15] := '                                                   ';
   helpScr[16] := '     A chi square test of Goodness of Fit is       ';
   helpScr[17] := '     used to test the Central Limit Theorem.       ';
   helpScr[18] := '                                                   ';
   helpScr[19] := '     At the completion of the runs, a normal       ';
   helpScr[20] := '     distributions is superimposed over the        ';
   helpScr[21] := '     distribution of averages.                     ';
   helpScr[22] := '                                                   ';
   helpScr[23] := '                                                   ';
   helpScr[24] := '                                                   ';
   helpScr[25] := '                                                   ';
   HELP(helpScr);
END;  {  PROCEDURE HELP3  }

PROCEDURE ABOUTPROGRAM;
   VAR
      helpScr: HelpScrType;
BEGIN
   helpScr[1] := '                                                  ';
   helpScr[2] := '   The Galton Board and the Central Limit Theorem ';
   helpScr[3] := '                                                  ';
   helpScr[4] := '                        by                        ';
   helpScr[5] := '                                                  ';
   helpScr[6] := '                   L.B. Spornick                  ';
   helpScr[7] := '            Applied Physics Laboratory            ';
   helpScr[8] := '           The Johns Hopkins University           ';
   helpScr[9] := '                                                  ';
   helpScr[10] := '                    April 1995               ';
   helpScr[11] := '                Copyright (c) 1995                ';
   helpScr[12] := '                                                  ';
   helpScr[13] := '   This program models either a traditional or a ';
   helpScr[14] := '   custom Galton Board.  The program demonstrates';
   helpScr[15] := '   a Random Walk, the Binomial Distribution, the ';
   helpScr[16] := '   Normal Distribution, a Customized Probability ';
   helpScr[17] := '   Distribution, and the Central Limit Theorem.  ';
   helpScr[18] := '                                                  ';
   helpScr[19] := '                                                  ';
   helpScr[20] := '                                                  ';
   helpScr[21] := '                                                  ';
   helpScr[22] := '                                                  ';
   helpScr[23] := '                                                  ';
   helpScr[24] := '                                                  ';
   helpScr[25] := '                                                  ';
   HELP(helpScr);
END;   {  PROCEDURE ABOUTPROGRAM  }

FUNCTION Power (x: REAL;
                        y: LONGINT): REAL;
   VAR
      i: LONGINT;
      s: REAL;
BEGIN
   s := 1.0;
   IF (y <> 0) THEN
      FOR i := 1 TO y DO
         s := s * x;
   Power := s;
END;  { FUNCTION Power }

FUNCTION Factor (i: LONGINT): REAL;
   VAR
      s: REAL;
      i1: LONGINT;
BEGIN
   s := 1;
   IF i <> 0 THEN
      FOR i1 := 1 TO i DO
         s := s * i1;
   Factor := s;
END;   { FUNCTION Factor }

PROCEDURE SetUpMyMenu;
BEGIN
   MyMenuOut.Init;
   WITH MyMenuOut DO BEGIN
      column(1, 'File');
      row(1, 1, 'About Program');
      row(1, 2, 'About CUPS');
      row(1, 3, 'Configuration');
      row(1, 4, '---------------------');
      row(1, 5, 'Central Limit Theorem');
      row(1, 6, 'Restart Program');
      row(1, 7, '---------------------');
      row(1, 8, 'Exit Program');
      rowactivate(1,4,false);
      rowactivate(1,7,false);
   END;
   MyMenuOut.Display;
END;  { PROCEDURE SetUpMyMenu }

PROCEDURE HandleMenu;
BEGIN
   CASE MyMenuOut.colChosen OF
      1:
         CASE MyMenuOut.rowChosen OF
            1: ABOUTPROGRAM;
            2: AboutCups;
            3: configuration;
            5: cntlmthrm := NOT cntlmthrm;
            6: restart := NOT restart;
            8: quitf := true;
         END; { CASE }
   END;   { CASE }
END; { PROCEDURE HandleMenu }

PROCEDURE SetUpHotKeys (VAR HotKeys: THotKeys);
BEGIN
   WITH HotKeys DO BEGIN
      init(3);
      key[1] := 'F1-Help';
      key[2] := 'F3-Step';
      key[3] := 'F10-Menu';
   END;
END;  { PROCEDURE SetUpHotKeys }

PROCEDURE HandleHotKeys (htype: INTEGER;
                        key: BYTE;
                        VAR STEP: BOOLEAN);
BEGIN
   CASE key OF
      1:
         IF (htype = 1) THEN
            Help1
         ELSE IF (htype = 2) THEN
            Help2
         ELSE
            Help3;
      2:
         STEP := NOT STEP;  { F3 - Step }
      3:  BEGIN
         MouseBorders(0, 0, GetMaxX, GetMaxY);
         IF (MyMenuOut.chosen) THEN
            HandleMenu;
      END;
   END;  { CASE }
END;  { PROCEDURE HandleHotKeys }

PROCEDURE ReadData;
   VAR
      screen: TInputScreen;
BEGIN
   WITH screen DO BEGIN
      init;
      DefineInputPort(0.1, 0.87, 0.1, 0.95);
      LoadLine('                 The Galton Board                   ');
      LoadLine('                                                    ');
      LoadLine('  The Board Type:                                   ');
      LoadLine('   #1 Traditional Galton Board                       ');
      LoadLine('   #1 Customized Galton Board                        ');
      LoadLine('                                                    ');
      LoadLine('  The Number of Balls:  {    } (10 - 250)           ');
      LoadLine('                                                    ');
      LoadLine('  The Number of Levels: {    } (3 - 8)             ');
      LoadLine('                                                    ');
      LoadLine('  For the Traditional Galton Board -                ');
      LoadLine('   The Deflection Probability to the Right:         ');
      LoadLine('                        {    } (10 - 90 )           ');
      LoadLine('   (Ignored for a Customized Galton Board)          ');
      LoadLine('                                                    ');
      LoadLine('  Output File Name: "                          "    ');
      LoadLine('                                                    ');
      LoadLine('               [   Ok   ]   [Cancel]               ');
      SetRadioButton('1', btype);
      SetNumber(3, nballs);
      SetNumberLimits(3, 10, 250);
      SetNumber(4, nlevels);
      SetNumberLimits(4, 3, 8);
      SetNumber(5, p * 100.0);
      SetNumberLimits(5, 10.0, 90.0);
      SetString(6, Galton_Output);
      Accept;
      nballs := Trunc(GetNumber(3));
      nlevels := Trunc(GetNumber(4));
      p := GetNumber(5) / 100.0;
      btype := GetRadioButton('1');
      Galton_Output := GetString(6);
      IF (canceled) THEN
         quitf := true;
      DONE;
   END;
END;  { PROCEDURE ReadData }

PROCEDURE ReadData1;
   VAR
      screen: TInputScreen;
BEGIN
   WITH screen DO BEGIN
      init;
      DefineInputPort(0.1, 0.87, 0.1, 0.95);
      LoadLine('                 The Galton Board                   ');
      LoadLine('                                                    ');
      LoadLine('  The Board Type:                                   ');
      LoadLine('   #1 Traditional Galton Board                       ');
      LoadLine('   #1 Customized Galton Board                        ');
      LoadLine('                                                    ');
      LoadLine('  The Number of Balls:  {    } (10 - 250)            ');
      LoadLine('                                                    ');
      LoadLine('  The Number of Levels: {    } (3 - 8)                ');
      LoadLine('                                                    ');
      LoadLine('  For the Traditional Galton Board -                ');
      LoadLine('   The Deflection Probability to the Right:         ');
      LoadLine('                        {    } (10 - 90 )               ');
      LoadLine('   (Ignored for a Customized Galton Board)          ');
      LoadLine('                                                    ');
      LoadLine('               [   Ok   ]   [Cancel]               ');
      SetRadioButton('1', btype);
      SetNumber(3, nballs);
      SetNumberLimits(3, 10, 250);
      SetNumber(4, nlevels);
      SetNumberLimits(4, 3, 8);
      SetNumber(5, p * 100.0);
      SetNumberLimits(5, 10.0, 90.0);
      Accept;
      nballs := Trunc(GetNumber(3));
      nlevels := Trunc(GetNumber(4));
      p := GetNumber(5) / 100.0;
      btype := GetRadioButton('1');
      IF (canceled) THEN
         quitf := true;
      DONE;
   END;
END;  { PROCEDURE ReadData1 }

PROCEDURE CreateCustomBoard (nlevels: LONGINT);
   VAR
      screen: TInputScreen;
      i, i1, j: INTEGER;
      Reflect, Absorb: ARRAY[1..2] OF INTEGER;
BEGIN
   WITH screen DO BEGIN
      init;
      DefineInputPort(0.1, 0.87, 0.01, 0.99);
      LoadLine('            Customized Galton Board                    ');
      LoadLine('  Use the Galton Board below to set the Deflection     ');
      LoadLine('   Probability to the Right (10-90) for each pin.      ');
      LoadLine('  To place a Trap, set the individual pin              ');
      LoadLine('  Probability to -1.                                   ');
      LoadLine('                                                       ');
      LoadLine('                           {   }                      ');
      LoadLine('                        {   } {   }                  ');
      LoadLine('                     {   } {   } {   }              ');
      IF (nlevels = 3) THEN BEGIN
         LoadLine('         Columns    1   2     3   4                  ');
         LoadLine('   Place the Reflecting Wall(s) in Column(s) {  } {  } ');
         LoadLine('   Place the Absorbing Wall(s) in Column(s)  {  } {  } ');
      END;  { IF (nlevels = 3) }
      IF (nlevels >= 4) THEN
         LoadLine('                 {   } {   } {   } {   }               ');
      IF (nlevels = 4) THEN BEGIN
         LoadLine('        Columns   1  2  3     4  5  6               ');
         LoadLine('  Place the Reflecting Wall(s) in Column(s) {  } {  }    ');
         LoadLine('  Place the Absorbing Wall(s) in Column(s)  {  } {  }    ');
      END;  { IF (nlevels = 4) }
      IF (nlevels >= 5) THEN
         LoadLine('               {   } {   } {   } {   } {   }        ');
      IF (nlevels = 5) THEN BEGIN
         LoadLine('      Columns   1  2  3  4     5  6  7  8            ');
         LoadLine('  Place the Reflecting Wall(s) in Column(s) {  } {  }');
         LoadLine('  Place the Absorbing Wall(s) in Column(s)  {  } {  }  ');
      END;  { IF (nlevels = 5) }
      IF (nlevels >= 6) THEN
         LoadLine('           {   } {   } {   } {   } {   } {   }         ');
      IF (nlevels = 6) THEN BEGIN
         LoadLine('                                           1         ');
         LoadLine(' Columns     1  2  3  4  5     6  7  8  9  0         ');
         LoadLine(' Place the Reflecting Wall(s) in Column(s) {   } {   }  ');
         LoadLine(' Place the Absorbing Wall(s) in Column(s)  {   } {   }   ');
      END;  { IF (nlevels = 6) }
      IF (nlevels >= 7) THEN
         LoadLine('         {   } {   } {   } {   } {   } {   } {   }  ');
      IF (nlevels = 7) THEN BEGIN
         LoadLine('                                         1  1  1      ');
         LoadLine('Columns    1  2  3  4  5  6     7  8  9  0  1  2      ');
         LoadLine('Place the Reflecting Wall(s) in Column(s) {   } {   }');
         LoadLine('Place the Absorbing Wall(s) in Column(s)  {   } {   }');
      END;  { IF (nlevels = 7) }
      IF (nlevels >= 8) THEN
         LoadLine('     {   } {   } {   } {   } {   } {   } {   } {   }   ');
      IF (nlevels = 8) THEN BEGIN
         LoadLine('                                     1  1  1  1  1   ');
         LoadLine('Cols   1  2  3  4  5  6  7     8  9  0  1  2  3  4   ');
         LoadLine('Place the Reflecting Wall(s) in Column(s)  {   } {   }');
         LoadLine('Place the Absorbing Wall(s) in Column(s)   {   } {   } ');
      END;  { IF (nlevels = 8) }
      LoadLine('                                                   ');
      LoadLine('               [   Ok   ]   [Cancel]               ');
      i1 := 0;
      FOR i := 1 TO nlevels DO BEGIN
         FOR j := 1 TO i DO BEGIN
            INC(i1);
            IF (ABS(prob[i, j]) <> 1.0) THEN
               SetNumber(i1, prob[i, j] * 100.0)
            ELSE
               SetNumber(i1, prob[i, j]);
         END;  { FOR j := 1 TO i }
      END;  { FOR i := 1 TO nlevels }
      REPEAT
         error := 0;
         Accept;
         i1 := 0;
         FOR i := 1 TO nlevels DO BEGIN
            FOR j := 1 TO i DO BEGIN
               INC(i1);
               IF (ABS(GetNumber(i1)) <> 1.0) THEN
                  prob[i, j] := GetNumber(i1) / 100.0
               ELSE IF (GetNumber(i1) = -1.0) THEN
                  prob[i, j] := -1.0
               ELSE
                  prob[i, j] := 1.0;
               IF ((prob[i, j] < 0.09) OR (prob[i, j] > 0.91)) THEN
                  IF (ABS(prob[i, j]) <> 1.0) AND (prob[i, j] <> 0.0) THEN BEGIN
                     error := 1;
                     Announce('Error in pin Deflection Probability');
                  END;
            END; { FOR j := 1 TO i DO }
         END;  { FOR i := 1 TO nlevels DO }
         i1 := (nlevels * (nlevels + 1)) DIV 2;
         Reflect[1] := Trunc(GetNumber(i1 + 1));
         Reflect[2] := Trunc(GetNumber(i1 + 2));
         Absorb[1] := Trunc(GetNumber(i1 + 3));
         Absorb[2] := Trunc(GetNumber(i1 + 4));
         FOR i := 1 TO 2 DO BEGIN
            IF (Reflect[i] <> 0) THEN BEGIN  { Place Reflecting Wall }
 { determine the direction the balls are reflected }
               IF (Reflect[i] > 0) AND (Reflect[i] < nlevels) THEN BEGIN
                  IF Odd(Reflect[i]) THEN BEGIN
                     i1 := 0;
                     j := (nlevels - Reflect[i] + 1);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := 1.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 2);
                  END  { IF Odd(Reflect[i] THEN }
                  ELSE  { Reflect[i] is even }
                     BEGIN
                     i1 := 0;
                     j := (nlevels - Reflect[i] + 1);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := 1.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 1);
                  END;  { Reflect[i] is even }
               END  { IF Reflect[i] > 0 ... }
               ELSE IF (Reflect[i] >= nlevels) AND (Reflect[i] <= 2 * (nlevels - 1)) THEN BEGIN
                  IF Odd(Reflect[i]) THEN BEGIN
                     i1 := Reflect[i] - nlevels + 1;
                     j := (Reflect[i] - nlevels + 2);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := 0.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 1);
                  END  { IF Odd(Reflect[i]) THEN }
                  ELSE  { Reflect[i] is even }
                     BEGIN
                     i1 := Reflect[i] - nlevels + 1;
                     j := (Reflect[i] - nlevels + 2);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := 0.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 2);
                  END; { Reflect[i] is even }
               END  { IF Relect[i] >= nlevels ... }
               ELSE BEGIN  { Error }
                  Announce('Error in Reflection Wall');
                  error := 1;
               END;  { Error }
            END;  { IF (Reflect[i] <> 0) THEN }
         END; { FOR i := 1 TO 2 DO }
         FOR i := 1 TO 2 DO BEGIN
            IF (Absorb[i] <> 0) THEN BEGIN  { Place Absorbing Wall }
               IF (Absorb[i] > 0) AND (Absorb[i] < nlevels) THEN BEGIN
                  IF Odd(Absorb[i]) THEN BEGIN
                     i1 := 0;
                     j := (nlevels - Absorb[i] + 1);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := -1.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 2);
                  END  { IF Odd(Absorb(i) THEN }
                  ELSE  { Absorb(i) is even }
                     BEGIN
                     i1 := 0;
                     j := (nlevels - Absorb[i] + 1);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := -1.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 1);
                  END;  { Absorb[i] is even }
               END  { IF Absorb[i] > 0 ... }
               ELSE IF (Absorb[i] >= nlevels) AND (Absorb[i] <= 2 * (nlevels - 1)) THEN BEGIN
                  IF Odd(Absorb[i]) THEN BEGIN
                     i1 := Absorb[i] - nlevels + 1;
                     j := (Absorb[i] - nlevels + 2);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := -1.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 1);
                  END  { IF Odd(Absorb[i] THEN }
                  ELSE  { Absorb[i] is even }
                     BEGIN
                     i1 := Absorb[i] - nlevels + 1;
                     j := (Absorb[i] - nlevels + 2);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := -1.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 2);
                  END; { Absorb(i) is even }
               END  { IF Absorb[i] >= nlevels ... }
               ELSE BEGIN  { Error }
                  Announce('Error in Absorbing Wall');
                  error := 1;
               END;  { Error }
            END;  { IF (Absorb[i] <> 0) THEN }
         END; { FOR i := 1 TO 2 DO }
         FOR i := 1 TO 2 DO BEGIN
            IF (Reflect[i] <> 0) THEN BEGIN  { Place Reflecting Wall }
{ determine the direction the balls are reflected }
               IF (Reflect[i] > 0) AND (Reflect[i] < nlevels) THEN BEGIN
                  IF Odd(Reflect[i]) THEN BEGIN
                     i1 := 0;
                     j := (nlevels - Reflect[i] + 1);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := 1.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 2);
                  END  { IF Odd(Reflect[i] THEN }
                  ELSE  { Reflect[i] is even }
                     BEGIN
                     i1 := 0;
                     j := (nlevels - Reflect[i] + 1);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := 1.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 1);
                  END;  { Reflect[i] is even }
               END  { IF Reflect[i] > 0 ... }
               ELSE IF (Reflect[i] >= nlevels) AND (Reflect[i] <= 2 * (nlevels - 1)) THEN BEGIN
                  IF Odd(Reflect[i]) THEN BEGIN
                     i1 := Reflect[i] - nlevels + 1;
                     j := (Reflect[i] - nlevels + 2);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := 0.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 1);
                  END  { IF Odd(Reflect[i]) THEN }
                  ELSE  { Reflect[i] is even }
                     BEGIN
                     i1 := Reflect[i] - nlevels + 1;
                     j := (Reflect[i] - nlevels + 2);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := 0.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 2);
                  END; { Reflect[i] is even }
               END  { IF Relect[i] >= nlevels ... }
               ELSE BEGIN  { Error }
                  Announce('Error in Reflecting Wall');
                  error := 1;
               END;  { Error }
            END;  { IF (Reflect[i] <> 0) THEN }
         END; { FOR i := 1 TO 2 DO }
         FOR i := 1 TO 2 DO BEGIN
            IF (Absorb[i] <> 0) THEN BEGIN  { Place Absorbing Wall }
               IF (Absorb[i] > 0) AND (Absorb[i] < nlevels) THEN BEGIN
                  IF Odd(Absorb[i]) THEN BEGIN
                     i1 := 0;
                     j := (nlevels - Absorb[i] + 1);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := -1.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 2);
                  END  { IF Odd(Absorb(i) THEN }
                  ELSE  { Absorb(i) is even }
                     BEGIN
                     i1 := 0;
                     j := (nlevels - Absorb[i] + 1);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := -1.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 1);
                  END;  { Absorb[i] is even }
               END  { IF Absorb[i] > 0 ... }
               ELSE IF (Absorb[i] >= nlevels) AND (Absorb[i] <= 2 * (nlevels - 1)) THEN BEGIN
                  IF Odd(Absorb[i]) THEN BEGIN
                     i1 := Absorb[i] - nlevels + 1;
                     j := (Absorb[i] - nlevels + 2);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := -1.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 1);
                  END  { IF Odd(Absorb[i] THEN }
                  ELSE  { Absorb[i] is even }
                     BEGIN
                     i1 := Absorb[i] - nlevels + 1;
                     j := (Absorb[i] - nlevels + 2);
                     REPEAT
                        Inc(i1);
                        prob[j, i1] := -1.0;
                        j := j + 2;
                     UNTIL (j = nlevels + 2);
                  END; { Absorb(i) is even }
               END  { IF Absorb[i] >= nlevels ... }
               ELSE BEGIN  { Error }
                  Announce('Error in Absorbing Wall');
                  error := 1;
               END;  { Error }
            END;  { IF (Absorb[i] <> 0) THEN }
         END; { FOR i := 1 TO 2 DO }
      UNTIL (error = 0);
      IF (canceled) THEN
         quitf := true;
      DONE;
   END;
END;  { PROCEDURE CreateCustonBoard }


PROCEDURE DrawMarkers (btype: LONGINT;
                        xbin: binarray;
                        ballsize, ybinbottom: LONGINT;
                        binominalcount, customcount, ynormal: realarray);
   VAR
      i, i1: INTEGER;
BEGIN
   IF (btype = 1) THEN BEGIN
     { place binomial distribution markers }
      SetColor(lightred);
      FOR i := 1 TO (nlevels + 1) DO BEGIN
         line(xbin[i] - ballsize, ybinbottom -
              Round(binomialcount[i] * 2 * ballsize) +
              (ballsize DIV 2), xbin[i] + ballsize,
              ybinbottom - Round(binomialcount[i] * 2 * ballsize) +
              (ballsize DIV 2));
         line(xbin[i] - ballsize, ybinbottom -
              Round(binomialcount[i] * 2 * ballsize) + 1 +
              (ballsize DIV 2), xbin[i] + ballsize,
              ybinbottom - Round(binomialcount[i] * 2 * ballsize) + 1 +
              (ballsize DIV 2));
      END; { FOR i := 1 TO (nlevels+1) DO  }
      SetColor(yellow);
     { draw normal distribution }
      i1 := xbin[1] - (xbin[2] - xbin[1]) DIV 2;
      line(i1, ybinbottom, xbin[1], ybinbottom - Round(ynormal[1]));
      FOR i := 1 TO (2 * nlevels) DO BEGIN
         i1 := (i DIV 2) + 1;
         IF Odd(i) THEN
            line(xbin[i1], ybinbottom - Round(ynormal[i]),
                 ((xbin[i1] + xbin[i1 + 1]) DIV 2), ybinbottom -
                 Round(ynormal[i + 1]))
         ELSE
            line(((xbin[i1 - 1] + xbin[i1]) DIV 2), ybinbottom -
            Round(ynormal[i]), xbin[i1], ybinbottom - Round(ynormal[i + 1]));
      END; { FOR i := 1 TO (2*nlevels - 1) DO }
      i1 := xbin[nlevels + 1] + (xbin[2] - xbin[1]) DIV 2;
      line(xbin[nlevels + 1], ybinbottom - Round(ynormal[2 * nlevels + 1]),
          i1, ybinbottom);
      SetColor(white);
   END  { IF (btype = 1) THEN }
   ELSE
    { IF (btype = 2) THEN }
      BEGIN
     { place binomial distribution markers }
      SetColor(lightred);
      FOR i := 1 TO (nlevels + 1) DO BEGIN
         line(xbin[i] - ballsize, ybinbottom -
              Round(customcount[i] * 2 * ballsize) + (ballsize DIV 2),
              xbin[i] + ballsize, ybinbottom - Round(customcount[i] * 2 *
              ballsize) + (ballsize DIV 2));
         line(xbin[i] - ballsize, ybinbottom - Round(customcount[i] * 2 *
              ballsize) + 1 + (ballsize DIV 2), xbin[i] + ballsize,
              ybinbottom - Round(customcount[i] * 2 * ballsize) + 1 +
              (ballsize DIV 2));
      END; { FOR i := 1 TO (nlevels+1) DO }
   END;  { ELSE - IF (btype = 2) THEN }
   SetColor(white);
END;  { PROCEDURE DrawMarker}

PROCEDURE Predictions (ballsize, btype, nballs, nlevels, right, ytop: LONGINT;
                        p: REAL;
                        baverage: EXTENDED;
                        binominalcount, customcount, ynormal: realarray);

   VAR
      i: INTEGER;
      StatisticOutput: STRING;
      expfill: REAL;

BEGIN
    { print predicted statistics on screen }
   IF (btype = 1) THEN
      Print((right DIV 8) + 5, (ytop DIV 8) + 3, 'Bin Actual Binomial Normal')
   ELSE
      Print((right DIV 8) + 5, (ytop DIV 8) + 3, 'Bin Actual Predicted');

   FOR i := 1 TO (nlevels + 1) DO BEGIN
      IF (btype = 1) THEN
         StatisticOutput := NumStr((i - 1), 2, 0) + '           ' +
         NumStr(binomialcount[i], 3, 0) + '     ' +
         NumStr(ynormal[2 * i - 1] / (2 * ballsize), 3, 0)
      ELSE  { btype = 2 }
         StatisticOutput := NumStr((i - 1), 2, 0) + '           ' +
         NumStr(customcount[i], 3, 0);
      Print((right DIV 8) + 5, (ytop DIV 8) + 3 + i, StatisticOutput);
   END;  { FOR i := 1 TO (nlevels + 1) DO }
   IF (btype = 1) THEN BEGIN
      StatisticOutput := 'Theory Average : ' + NumStr(baverage, 6, 3);
      Print((right DIV 8) + 5, (ytop DIV 8) + 6 + nlevels, StatisticOutput);
      StatisticOutput := 'Theory One Sigma : ' + NumStr(SQRT((nlevels) * p * (1.0 - p)), 6, 3);
      Print((right DIV 8) + 5, (ytop DIV 8) + 9 + nlevels, StatisticOutput);
   END  { IF (btype = 1) THEN }
   ELSE  { IF (btype = 2) THEN }
      BEGIN
      StatisticOutput := 'Theory Average : ' + NumStr(baverage, 6, 3);
      Print((right DIV 8) + 5, (ytop DIV 8) + 6 + nlevels, StatisticOutput);
      StatisticOutput := 'Number of Balls Dropped : ' + NumStr(nballs, 3, 0);
      Print((right DIV 8) + 5, (ytop DIV 8) + 8 + nlevels, StatisticOutput);
      expfill := 0;
      FOR i := 1 TO (nlevels + 1) DO
         expfill := expfill + customcount[i];
      StatisticOutput := 'Expected Number of Balls  ';
      Print((right DIV 8) + 5, (ytop DIV 8) + 10 + nlevels, StatisticOutput);
      StatisticOutput := 'in Bins                 : ' + NumStr(expfill, 3, 0);
      Print((right DIV 8) + 5, (ytop DIV 8) + 11 + nlevels, StatisticOutput);
   END;  { ELSE - IF btype = 2 }
END;  { PROCEDURE Predictions  }

PROCEDURE GALTON_BOARD_Initialize (nlevels, nballs: LONGINT;
                        shape: FillPatternType;
                        p: REAL;
                        prob: probarray;
                        btype: LONGINT;
                        VAR ballsize, center, small, ytop: LONGINT;
                        VAR binomialcount, customcount, ynormal: realarray;
                        VAR x, y: arrays;
                        VAR xbin, ybinfill: binarray;
                        VAR right, ybinbottom, ybintop: LONGINT;
                        VAR average: EXTENDED);

   VAR
      binheight, binwidth, boardwidth, boardlength, boardtype: LONGINT;
      height, i, i1, left, nwidth, screenwidth, spacing: LONGINT;
      totalheight, xleft, xright, xleftbin, xrightbin, ybottom: LONGINT;
      xright1, xleft1, ybottom1, ytop1: INTEGER;
      nfact, sigma2: REAL;
      intermediate: EXTENDED;
      typelabel: STRING;

   PROCEDURE ComputeExpectedProbability (btype: LONGINT;
                           nlevels, nballs: LONGINT;
                           p: REAL;
                           prob: probarray;
                           VAR binominalcount, customcount: realarray;
                           VAR average: EXTENDED;
                           VAR height: LONGINT);

      VAR
         probrow: probarray;
         i, i1: INTEGER;
         nfact, nball: REAL;

   BEGIN
      average := 0.0;
      height := 0;
      IF (btype = 1) THEN BEGIN
   { compute binomial distribution }
         nfact := Factor(nlevels);
         FOR i := 1 TO (nlevels + 1) DO BEGIN
            i1 := i - 1;
            binomialcount[i] := nfact * power(p, i1) * power((1.0 - p),
            (nlevels - i1)) * nballs / (Factor(i1) * Factor(nlevels - i1));
            IF (height < (Round(binomialcount[i]) + 15)) THEN
               height := Round(binomialcount[i]) + 15;
            average := average + binomialcount[i] * i1;
         END;  { FOR i := 1 TO (nlevels + 1) DO }
      END  { IF (btype = 1 ) THEN DO }
      ELSE
   { IF (btype = 2) THEN DO }
         BEGIN
         FOR i := 1 TO (nlevels + 1) DO
            FOR i1 := 1 TO (nlevels + 1) DO
               probrow[i, i1] := 0;
         probrow[1, 1] := 1.0;
         probrow[2, 1] := 1.0 - prob[1, 1];
         probrow[2, 2] := prob[1, 1];
         IF (prob[2, 1] = -1.0) THEN
            probrow[2, 1] := 0.0;
         IF (prob[2, 2] = -1.0) THEN
            probrow[2, 2] := 0.0;
         FOR i := 3 TO (nlevels + 1) DO BEGIN
            probrow[i, 1] := probrow[i - 1, 1] * (1.0 - prob[i - 1, 1]);
            FOR i1 := 2 TO i - 1 DO
               probrow[i, i1] := probrow[i - 1, i1 - 1] * prob[i - 1,
                  i1 - 1] + probrow[i - 1, i1] * (1.0 - prob[i - 1, i1]);
            probrow[i, i] := probrow[i - 1, i - 1] * prob[i - 1, i - 1];
            FOR i1 := 1 TO i DO
               IF (prob[i, i1] = -1) THEN
                  probrow[i, i1] := 0.0;
         END;  { FOR i := 3 TO (nlevels + 1) DO }
         FOR i := 1 TO (nlevels + 1) DO BEGIN
            i1 := i - 1;
            IF (probrow[(nlevels + 1), i] <= 0.0) THEN
               probrow[(nlevels + 1), i] := 0.0;
            customcount[i] := probrow[(nlevels + 1), i] * nballs;
            IF (height < Round(customcount[i] + 15)) THEN
               height := Round(customcount[i] + 15);
            average := average + customcount[i] * i1;
         END;  { FOR i := 1 TO (nlevels + 1) DO }
      END;  { ELSE - IF (btype = 2) THEN DO }
      IF (btype = 1) THEN
         average := average / nballs
      ELSE BEGIN
         nball := 0;
         FOR i := 1 TO (nlevels + 1) DO
            nball := nball + customcount[i];
         average := average / nball;
      END;  { ELSE }
   END;  { PROCEDURE ComputeExpectedProbability }

BEGIN  { PROCEDURE Galton_Board_Initialize }
   OpenViewPort(1);
   Map(0.1, 0.1, xleft1, ybottom1);
   Map(0.9, 0.9, xright1, ytop1);
   IF (btype = 1) THEN
      typelabel := 'A Traditional Galton Board - p = ' +
                   NumStr(p * 100.0, 3, 0)
   ELSE
      typelabel := 'A Customized Galton Board';
   PutLabel(top, typelabel);
   xleft := xleft1;
   xright := xright1;
   ytop := ytop1;
   ybottom := ybottom1;
   IF (nlevels <= 6) AND (nballs <= 100) THEN
      small := 1
   ELSE
      small := 0;
   totalheight := ybottom - ytop;
   ComputeExpectedProbability(btype, nlevels, nballs, p, prob,
                              binomialcount, customcount, average, height);
   IF (small = 1) THEN
      ballsize := Round(totalheight / (2 * height + (nlevels + 2) * 4))
   ELSE
      ballsize := Round(totalheight / (2 * height));
   IF (ballsize > 3) THEN
      ballsize := 3;
   screenwidth := xright - xleft;
   IF (small = 1) THEN
      boardwidth := (nlevels * 8 + 1) * ballsize
   ELSE BEGIN { IF (small = 0) }
      nwidth := 10;
      boardwidth := 2 * (nlevels + 1) * ballsize + nwidth * nlevels;
      IF (boardwidth > screenwidth) THEN BEGIN
         REPEAT
            Dec(nwidth);
            boardwidth := 2 * (nlevels + 1) * ballsize + nwidth * nlevels;
         UNTIL (boardwidth <= screenwidth) OR (nwidth = 0);
         IF (nwidth = 0) THEN
            Halt;
      END;  { IF (boardwidth > screenwidth) }
   END;  { ELSE - IF (small = 0) }
   center := xleft + (boardwidth DIV 2) + 50;
   right := center + (boardwidth DIV 2);
   left := center - (boardwidth DIV 2);

  {  draw the outline of the Galton Board }
   SetColor(white);
   line(left - 1, ytop + 2, right + 2, ytop + 2);
   line(left - 1, ybottom, right + 2, ybottom);
   line(left - 1, ytop + 2, left - 1, ybottom);
   line(right + 2, ytop + 2, right + 2, ybottom);
   SetFillPattern(shape, Blue);
   FloodFill(left - 2, ytop + 1, white);
   FloodFill(right + 3, ytop + 1, white);

  {  place the bins  }
   IF (small = 1) THEN
      boardlength := (nlevels + 1) * 4 * ballsize
   ELSE
      boardlength := 0;
   binheight := ybottom - (totalheight - boardlength);
   IF (small = 1) THEN
      binwidth := (boardwidth - (nlevels + 1) * 2 * ballsize) DIV nlevels
   ELSE
      binwidth := nwidth;
   FOR i := 1 TO nlevels DO BEGIN
      xleftbin := left + i * 2 * ballsize + (i - 1) * binwidth + 1;
      xrightbin := xleftbin + binwidth - 2;
      SetColor(white);
      Rectangle(xleftbin, binheight, xrightbin, ybottom);
      SetFillPattern(shape, 9);
      FloodFill(xleftbin + 1, ybottom - 1, white)
   END;  { FOR i := 1 TO nlevels DO }
   ybintop := binheight;
   ybinbottom := ybottom;
   ybinfill[1] := ybottom - 2 * ballsize;
   xbin[1] := left + ballsize;
   FOR i := 2 TO (nlevels + 1) DO BEGIN
      xbin[i] := xbin[i - 1] + binwidth + 2 * ballsize;
      ybinfill[i] := ybottom - 2 * ballsize;
   END;  { FOR i := 2 TO (nlevels + 1) DO }

   IF (btype = 1) THEN BEGIN
    { compute normal distribution }
      sigma2 := (nlevels + 1) * p * (1.0 - p);
      FOR i := 1 TO (2 * nlevels + 1) DO BEGIN
         ri1 := (i - 1.0) / 2.0;
         intermediate := exp(-SQR(ri1 - average) / (2 * sigma2)) / SQRT(2.0 * Pi * sigma2);
         intermediate := 2 * ballsize * nballs * intermediate;
         ynormal[i] := intermediate;
      END;  { FOR i := 1 TO (2*nlevels+1) DO }
   END;  { IF (btype = 1) THEN DO }

   DrawMarkers(btype, xbin, ballsize, ybinbottom, binomialcount, customcount, ynormal);

  {  place pins  }
   IF (small = 1) THEN BEGIN
      FOR i := 1 TO nlevels DO BEGIN
         spacing := (boardwidth - 8 * (i - 1) * ballsize) DIV 2;
         FOR i1 := 1 TO i DO BEGIN
            y[i, i1] := i * 4 * ballsize + ytop;
            x[i, i1] := left + spacing + (i1 - 1) * 8 * ballsize;
            Circle(x[i, i1], y[i, i1], 1);
   { place reflecting walls }
            IF (prob[i, i1] = 0.0) OR (prob[i, i1] = 1.0) THEN
               Line(x[i, i1], ytop, x[i, i1], binheight);
         END;  { FOR i1 := 1 TO i DO }
      END;  { FOR i := 1 TO nlevels DO }
   END;  { IF (small = 1) THEN }
END;  { PROCEDURE Galton_Board_Initialize }

PROCEDURE EquationOfMotion (ilevel, ileveli: LONGINT;
                        prob: probarray;
                        VAR ilevelf: LONGINT);

   VAR
      r: REAL;

BEGIN
   r := Random;
   IF r > prob[ilevel, ileveli] THEN
      ilevelf := ileveli
   ELSE
      ilevelf := ileveli + 1;
END;  { PROCEDURE EquationOfMotion }

PROCEDURE MoveBall (beepon: BOOLEAN;
                        ballsize, center, nlevels, ytop: LONGINT;
                        small, ybintop: LONGINT;
                        x, y: arrays;
                        xbin: binarray;
                        prob: probarray;
                        shape: FillPatternType;
                        binomialcount, customcount, ynormal: realarray;
                        deltat: INTEGER;
                        btype: LONGINT;
                        STEP: BOOLEAN;
                        VAR ybinfill, icount: binarray);

   VAR
      dt, ilevel, i, i1, ileveli, ilevelf, x1, x2, y1, y2: LONGINT;
      slopex, slopey: REAL;

BEGIN

   dt := deltat DIV 10;
   x1 := 0;
   y1 := ballsize;
   IF (small = 1) THEN BEGIN
      ilevel := 0;
      ileveli := 1;
      ilevelf := 1;
      REPEAT
         IF (ilevel = 0) THEN BEGIN
            slopex := (x[1, 1] - center) / deltat;
            slopey := (y[1, 1] - ytop) / deltat;
         END  { IF (ilevel = 0) THEN }
         ELSE IF (ilevel < nlevels) THEN BEGIN
            EquationOfMotion(ilevel, ileveli, prob, ilevelf);
            slopex := (x[ilevel + 1, ilevelf] - x[ilevel, ileveli]) / deltat;
            slopey := (y[ilevel + 1, ilevelf] - y[ilevel, ileveli]) / deltat;
            IF STEP THEN BEGIN
               Delay(500);
            END;
         END  { IF (ilevel < n) THEN }
         ELSE BEGIN
            EquationOfMotion(ilevel, ileveli, prob, ilevelf);
            slopex := (xbin[ilevelf] - x[ilevel, ileveli]) / deltat;
            slopey := (ybintop - y[ilevel, ileveli]) / deltat;
            IF STEP THEN BEGIN
               Delay(500);
            END;
         END;  { ELSE - IF (ilevel = nlevels) }
         FOR i := 1 TO 10 DO BEGIN
            IF (x1 <> 0) AND (y1 <> 0) THEN BEGIN
               SetColor(DarkGray);
               SetFillPattern(shape, DarkGray);
               FillEllipse(x1, y1, ballsize, ballsize);
               SetColor(white);
               IF (ilevel < (nlevels + 1)) AND (ilevel <> 0) THEN
                  Circle(x[ilevel, ileveli], y[ilevel, ileveli], 1);
               IF (ilevel < nlevels) THEN
                  Circle(x[ilevel + 1, ilevelf], y[ilevel + 1, ilevelf], 1);
            END;  { IF (x1 <> 0) AND (y1 <> 0) THEN }
            SetColor(white);
            IF (ilevel = 0) THEN BEGIN
               x2 := Round(slopex * i * dt) + center;
               y2 := Round(slopey * i * dt) + ytop;
            END  { IF (ilevel = 0) THEN }
            ELSE BEGIN
               x2 := Round(slopex * i * dt) + x[ilevel, ileveli];
               y2 := Round(slopey * i * dt) + y[ilevel, ileveli];
            END;  { ELSE - ilevel <> 0 }
            SetFillPattern(shape, blue);
            FillEllipse(x2, y2, ballsize, ballsize);
            x1 := x2;
            y1 := y2;
         END; { FOR i := 1 TO 10 DO }
         Inc(ilevel);
         ileveli := ilevelf;
      UNTIL (ilevel = nlevels + 1) OR (prob[ilevel, ileveli] = -1.0);
   END   { IF small := 1 THEN }
   ELSE BEGIN  { IF (small = 0) THEN }
      ilevel := 1;
      ileveli := 1;
      ilevelf := 1;
      REPEAT
         EquationOfMotion(ilevel, ileveli, prob, ilevelf);
         Inc(ilevel);
         ileveli := ilevelf;
      UNTIL (ilevel = nlevels + 1) OR (prob[ilevel, ileveli] = -1.0);
   END;  { ELSE - IF (small = 0) THEN }

  {  put the balls in the bins }
   IF (prob[ilevel, ileveli] <> -1.0) THEN BEGIN
      x2 := xbin[ilevelf];
      slopey := (ybinfill[ilevelf] - ybintop) / deltat;
      FOR i := 1 TO 10 DO BEGIN
         SetColor(DarkGray);
         SetFillPattern(shape, DarkGray);
         FillEllipse(x2, y1, ballsize, ballsize);
         SetColor(white);
         line(xbin[1], ybintop, xbin[nlevels + 1], ybintop);
         y2 := Round(slopey * i * dt) + ybintop;
         SetFillPattern(shape, Blue);
         FillEllipse(x2, y2, ballsize, ballsize);
         x1 := x2;
         y1 := y2;
      END;  { FOR i := 1 TO 10 DO }

      DrawMarkers(btype, xbin, ballsize, ybinbottom, binomialcount, customcount, ynormal);

      IF STEP THEN BEGIN
         Delay(500);
      END;
      SetColor(white);
  { bin updates }
      ybinfill[ilevelf] := ybinfill[ilevelf] - 2 * ballsize;
      Inc(icount[ilevelf]);
   END;  { IF prob[ilevel,ileveli] <> -1.0D }
   IF (prob[ilevel, ileveli] = -1.0) THEN
      beep;

END;  { PROCEDURE MoveBall }

PROCEDURE Statistics (nlevels: LONGINT;
                        icount: binarray;
                        VAR average, onesigma: EXTENDED;
                        VAR nball: INTEGER);

   VAR
      i, i1: INTEGER;

BEGIN
   nball := 0;
   average := 0.0;
   FOR i := 1 TO (nlevels + 1) DO BEGIN
      nball := nball + icount[i];
      i1 := i - 1;
      average := average + i1 * icount[i];
   END;
   average := average / (nball);
   onesigma := 0.0;
   FOR i := 1 TO (nlevels + 1) DO BEGIN
      i1 := i - 1;
      onesigma := onesigma + (SQR(i1) - SQR(average)) * icount[i];
   END;
   onesigma := SQRT(onesigma / nball);
END;  { PROCEDURE Statistics }

PROCEDURE StatisticsOut (ballsize, btype, nballs, nlevels, right, ytop: LONGINT;
                        nball: INTEGER;
                        p: REAL;
                        average, baverage, onesigma: EXTENDED;
                        icount: binarray;
                        binominalcount, customcount, ynormal: realarray;
                        VAR actavg: EXTENDED);

   VAR
      i, j: INTEGER;
      StatisticOutput: STRING;
      expfill: REAL;

BEGIN
   actavg := average;
   writeln(Output);
   IF (btype = 1) THEN
      writeln(Output, ' Type of Galton Board: Traditional')
   ELSE
      writeln(Output, ' Type of Galton Board: Customized');
   writeln(Output, ' Number of Balls: ', nballs);
   writeln(Output, ' Number of Levels: ', nlevels);
   IF (btype = 1) THEN
      writeln(Output, ' Deflection Probability to the Right: ', p * 100.0 : 2 : 0)
   ELSE
      FOR i := 1 TO nlevels DO
         FOR j := 1 TO i DO
            writeln(Output, ' prob[', i, ',', j, '] ', prob[i, j] * 100.0 : 2 : 0);
    { print final statistics on screen }
   IF (btype = 1) THEN BEGIN
      Print((right DIV 8) + 5, (ytop DIV 8) + 3, 'Bin Actual Binomial Normal');
      writeln(Output, ' Bin Actual Binomial Normal');
   END
   ELSE BEGIN
      Print((right DIV 8) + 5, (ytop DIV 8) + 3, 'Bin Actual Predicted');
      writeln(Output, ' Bin Actual Predicted');
   END;
   FOR i := 1 TO (nlevels + 1) DO BEGIN
      IF (btype = 1) THEN BEGIN
         StatisticOutput := NumStr((i - 1), 2, 0) + '   ' +
                  NumStr(icount[i], 3, 0) + '     ' +
                  NumStr(binomialcount[i], 3, 0) + '     ' +
                  NumStr(ynormal[2 * i - 1] / (2 * ballsize), 3, 0);
         writeln(Output, StatisticOutput);
      END
      ELSE  { btype = 2 }
         BEGIN
         StatisticOutput := NumStr((i - 1), 2, 0) + '   ' +
         NumStr(icount[i], 3, 0) + '     ' + NumStr(customcount[i], 3, 0);
         writeln(Output, StatisticOutput);
      END;
      Print((right DIV 8) + 5, (ytop DIV 8) + 3 + i, StatisticOutput);
   END;  { FOR i := 1 TO (nlevels + 1) DO }
   IF (btype = 1) THEN BEGIN
      StatisticOutput := 'Actual Average : ' + NumStr(average, 6, 3);
      Print((right DIV 8) + 5, (ytop DIV 8) + 5 + nlevels, StatisticOutput);
      writeln(Output, ' ', StatisticOutput);
      StatisticOutput := 'Theory Average : ' + NumStr(baverage, 6, 3);
      Print((right DIV 8) + 5, (ytop DIV 8) + 6 + nlevels, StatisticOutput);
      writeln(Output, ' ', StatisticOutput);
      StatisticOutput := 'Actual One Sigma : ' + NumStr(onesigma, 6, 3);
      Print((right DIV 8) + 5, (ytop DIV 8) + 8 + nlevels, StatisticOutput);
      writeln(Output, ' ', StatisticOutput);
      StatisticOutput := 'Theory One Sigma : ' + NumStr(SQRT((nlevels) * p * (1.0 - p)), 6, 3);
      Print((right DIV 8) + 5, (ytop DIV 8) + 9 + nlevels, StatisticOutput);
      writeln(Output, ' ', StatisticOutput);
   END  { IF (btype = 1) THEN }
   ELSE  { IF (btype = 2) THEN }
      BEGIN
      StatisticOutput := 'Actual Average : ' + NumStr(average, 6, 3);
      Print((right DIV 8) + 5, (ytop DIV 8) + 5 + nlevels, StatisticOutput);
      writeln(Output, ' ', StatisticOutput);
      StatisticOutput := 'Theory Average : ' + NumStr(baverage, 6, 3);
      Print((right DIV 8) + 5, (ytop DIV 8) + 6 + nlevels, StatisticOutput);
      writeln(Output, ' ', StatisticOutput);
      StatisticOutput := 'Number of Balls Dropped : ' + NumStr(nballs, 3, 0);
      Print((right DIV 8) + 5, (ytop DIV 8) + 8 + nlevels, StatisticOutput);
      writeln(Output, ' ', StatisticOutput);
      StatisticOutput := 'Number of Balls in Bins : ' + NumStr(nball, 3, 0);
      Print((right DIV 8) + 5, (ytop DIV 8) + 9 + nlevels, StatisticOutput);
      writeln(Output, ' ', StatisticOutput);
      expfill := 0.0;
      FOR i := 1 TO (nlevels + 1) DO
         expfill := expfill + customcount[i];
      StatisticOutput := 'Expected Number of Balls  ';
      Print((right DIV 8) + 5, (ytop DIV 8) + 10 + nlevels, StatisticOutput);
      writeln(Output, ' ', StatisticOutput);
      StatisticOutput := 'in Bins                 : ' + NumStr(expfill, 3, 0);
      Print((right DIV 8) + 5, (ytop DIV 8) + 11 + nlevels, StatisticOutput);
      writeln(Output, ' ', StatisticOutput);
   END;  { ELSE - IF btype = 2 }
END;  { PROCEDURE StatisticOut  }

PROCEDURE CntrlLmtThrmInitialize (binw: REAL;
                        nlevels, nballs, numdiv: LONGINT;
                        btype: LONGINT;
                        shape: FillPatternType;
                        VAR xwidth, ybottom, center, deltay: LONGINT;
                        VAR xcen, ycen: cenarray);

   VAR
      i, xright, xleft, ytop, ybottom1: INTEGER;
      typelabel: STRING;

BEGIN

   hidecursor;
   ClearMUPPETport;
   OpenViewPort(1);
   SetUpHotKeys(HotKeys);
   HotKeys.Display;
   SetUpMyMenu;
   Map(0.1, 0.1, xleft, ybottom1);
   Map(0.9, 0.9, xright, ytop);
   ybottom := ybottom1;
   IF (btype = 1) THEN BEGIN
      typelabel := 'The Central Limit Theorem and a Traditional Galton Board';
      PutLabel(top, typelabel);
      typelabel := ' nlevels = ' + NumStr(nlevels, 2, 0) + ' nballs = ' +
      NumStr(nballs, 3, 0) + ' p = ' + NumStr(100.0 * p, 4, 0);
      PutLabel(bottom, typelabel);
   END
   ELSE BEGIN
      typelabel := 'The Central Limit Theorem and a Customized Galton Board';
      PutLabel(top, typelabel);
      typelabel := ' nlevels = ' + NumStr(nlevels, 2, 0) + ' nballs = ' + NumStr(nballs, 3, 0);
      PutLabel(bottom, typelabel);
   END;
  { xwidth = bin width }
   center := (xleft + xright) DIV 2;
   xwidth := center DIV numdiv;
   center := xleft + xwidth * numdiv;
  { determine bin locations }
   xcen[1] := xleft;
   FOR i := 2 TO numdiv DO
      xcen[i] := xcen[i - 1] + xwidth;
   FOR i := 1 TO numdiv DO
      ycen[i] := ybottom;
  { draw bar chart outline }
   line(xleft, ybottom, center, ybottom);
   line(xleft, ytop, center, ytop);
   line(xleft, ybottom, xleft, ytop);
   line(center, ybottom, center, ytop);
   SetFillPattern(shape, 9);
   FloodFill(center + 4, ybottom - 4, white);
   StatisticOutput := NumStr(baverage - binw, 4, 2);
   OutTextXY(xcen[1], ybottom + 8, StatisticOutput);
   StatisticOutput := NumStr(baverage, 4, 2);
   OutTextXY(xcen[numdiv DIV 2], ybottom + 8, StatisticOutput);
   StatisticOutput := NumStr(baverage + binw, 4, 2);
   OutTextXY(xcen[numdiv - 1], ybottom + 8, StatisticOutput);
   IF (numdiv > 15) THEN
      deltay := xwidth DIV 2
   ELSE
      deltay := xwidth DIV 5;
   OutTextXY(xcen[1] - 10, ybottom - deltay, '0');
   FOR i := 1 TO 4 DO
      OutTextXY(xcen[1] - 20, ybottom - 20 * i * deltay, NumStr(i * 20, 2, 0));
   IF (numdiv <= 15) THEN BEGIN
      OutTextXY(xcen[1] - 30, ybottom - 100 * deltay, '100');
      OutTextXY(xcen[1] - 30, ybottom - 120 * deltay, '120');
   END;  { IF (numdiv <= 15) THEN }
END;  { PROCEDURE CntrlLmtThrmInitialization }

PROCEDURE CntrlLmtThrmDef (numchi: INTEGER;
                        baverage: EXTENDED;
                        xcen: cenarray;
                        ybottom, center, ytop: LONGINT);

   VAR
      StatisticOutput: STRING;

BEGIN
   StatisticOutput := 'The Central Limit Theorem';
   Print((center DIV 8) + 2, (ytop DIV 8) + 1, StatisticOutput);
   StatisticOutput := 'applied to the Galton Board - ';
   Print((center DIV 8) + 2, (ytop DIV 8) + 2, StatisticOutput);
   StatisticOutput := 'Regardless of the shape of';
   Print((center DIV 8) + 2, (ytop DIV 8) + 3, StatisticOutput);
   StatisticOutput := 'the distribution of balls in';
   Print((center DIV 8) + 2, (ytop DIV 8) + 4, StatisticOutput);
   StatisticOutput := 'bins, the distribution of the';
   Print((center DIV 8) + 2, (ytop DIV 8) + 5, StatisticOutput);
   StatisticOutput := 'averages for n identical boards';
   Print((center DIV 8) + 2, (ytop DIV 8) + 6, StatisticOutput);
   StatisticOutput := 'approaches a normal';
   Print((center DIV 8) + 2, (ytop DIV 8) + 7, StatisticOutput);
   StatisticOutput := 'distribution as n approaches';
   Print((center DIV 8) + 2, (ytop DIV 8) + 8, StatisticOutput);
   StatisticOutput := 'infinity.';
   Print((center DIV 8) + 2, (ytop DIV 8) + 9, StatisticOutput);
   StatisticOutput := 'On the left is a plot of the';
   Print((center DIV 8) + 2, (ytop DIV 8) + 11, StatisticOutput);
   StatisticOutput := 'averages for ' + NumStr(numchi, 3, 0) + ' identical';
   Print((center DIV 8) + 2, (ytop DIV 8) + 12, StatisticOutput);
   StatisticOutput := 'Galton Boards.';
   Print((center DIV 8) + 2, (ytop DIV 8) + 13, StatisticOutput);
END;  { PROCEDURE CntrlLmtThrmDef }

PROCEDURE PlotAverages (binw: REAL;
                        nlevels, numdiv: LONGINT;
                        baverage, average: EXTENDED;
                        xwidth, ybottom, deltay: LONGINT;
                        xcen: cenarray;
                        shape: FillPatternType;
                        VAR averagecount, ycen: cenarray);

   VAR
      binstart, binend, rnumdiv: REAL;
      i: INTEGER;

BEGIN
   binstart := baverage - binw;
   rnumdiv := numdiv;
   binend := binstart + (2.0 * binw) / rnumdiv;
   FOR i := 1 TO numdiv DO BEGIN
      IF (average >= binstart) AND (average < binend) THEN BEGIN
         Inc(averagecount[i]);
         SetColor(white);
         ycen[i] := ycen[i] - deltay;
         line(xcen[i], ycen[i], xcen[i] + xwidth, ycen[i]);
         line(xcen[i], ybottom, xcen[i] + xwidth, ybottom);
         line(xcen[i], ybottom, xcen[i], ycen[i]);
         line(xcen[i] + xwidth, ybottom, xcen[i] + xwidth, ycen[i]);
         SetFillPattern(shape, 9);
         FloodFill(xcen[i] + 1, ycen[i] + 1, white);
      END; { IF (average >= binstart ... }
      binstart := binend;
      binend := binend + (2.0 * binw) / rnumdiv;
   END;  { FOR i := 1 TO numdiv DO }
END;  { PROCEDURE PlotAverages }

PROCEDURE DrawNormal (e0: REAL;
                        numdiv, xwidth, ybottom, deltay: LONGINT;
                        ei: normarray;
                        xcen: cenarray);

   VAR
      di, y0, y1, y2: INTEGER;

BEGIN
   y0 := ybottom - Round(e0 * deltay);
   y1 := ybottom - Round(ei[1]) * deltay;
   SetColor(lightred);
   line(xcen[1], y0, xcen[1] + xwidth, y1);
   line(xcen[1], y0 - 1, xcen[1] + xwidth, y1 - 1);
   FOR i := 2 TO numdiv DO BEGIN
      y2 := ybottom - Round(ei[i]) * deltay;
      line(xcen[i], y1, xcen[i] + xwidth, y2);
      line(xcen[i], y1 - 1, xcen[i] + xwidth, y2 - 1);
      y1 := y2;
   END;  { FOR i := 2 TO numdiv }
   SetColor(white);
END;  { PROCEDURE DrawNormal }

PROCEDURE CountDown (i, numdiv, deltay, ybottom: LONGINT;
                        xcen: cenarray);

BEGIN
   IF (numdiv > 15) THEN BEGIN
      line(xcen[22], ybottom - 90 * deltay, xcen[27], ybottom - 90 * deltay);
      line(xcen[22], ybottom - 85 * deltay, xcen[27], ybottom - 85 * deltay);
      line(xcen[22], ybottom - 90 * deltay, xcen[22], ybottom - 85 * deltay);
      line(xcen[27], ybottom - 90 * deltay, xcen[27], ybottom - 85 * deltay);
      SetFillPattern(shape, 9);
      FloodFill(xcen[23] - 1, ybottom - 88 * deltay - 1, white);
      IF (i > 2) THEN BEGIN
         SetColor(9);
         OutTextXY(xcen[23], ybottom - 88 * deltay, NumStr(i - 2, 3, 0));
         SetColor(white);
      END;  { IF (i > 2) THEN }
      OutTextXY(xcen[23], ybottom - 88 * deltay, NumStr(i, 3, 0));
   END  { IF (numdiv > 15) THEN }
   ELSE BEGIN
      line(xcen[11], ybottom - 108 * deltay, xcen[13], ybottom - 108 * deltay);
      line(xcen[11], ybottom - 100 * deltay, xcen[13], ybottom - 100 * deltay);
      line(xcen[11], ybottom - 100 * deltay, xcen[11], ybottom - 108 * deltay);
      line(xcen[13], ybottom - 100 * deltay, xcen[13], ybottom - 108 * deltay);
      SetFillPattern(shape, 9);
      FloodFill(xcen[12] - 1, ybottom - 105 * deltay - 1, white);
      IF (i > 10) THEN BEGIN
         SetColor(9);
         OutTextXY(xcen[11] + 5, ybottom - 105 * deltay, NumStr(i - 10, 3, 0));
         SetColor(white);
      END;  { IF (i > 10) THEN }
      OutTextXY(xcen[11] + 5, ybottom - 105 * deltay, NumStr(i, 3, 0));
   END;  { ELSE }
END;  { PROCEDURE CountDown }

PROCEDURE CntrlLmtThrmCal (numchi: INTEGER;
                        binw: REAL;
                        nballs, nlevels, numdiv, xwidth, ybottom, deltay: LONGINT;
                        shape: FillPatternType;
                        xcen: cenarray;
                        VAR ycen: cenarray;
                        prob: probarray;
                        baverage: EXTENDED;
                        VAR restart, cntlmthrm, quitf: BOOLEAN);

   VAR
      i, i1, i2, ilevel, ileveli, ilevelf: LONGINT;
      ninbin: INTEGER;
      averagecount: cenarray;
      chiavg: chiarray;
      icount: binarray;
      average, avgchi, binstart, chi, chisigma2, onesigma: EXTENDED;
      ei: normarray;
      e0, rnumdiv, rnumchi: REAL;
BEGIN
   rnumdiv := numdiv;
   rnumchi := numchi;
   chi := 0.0;
   chisigma2 := 0;
   avgchi := 0.0;
   FOR i := 1 TO numchi DO
      chiavg[i] := 0.0;
   FOR i := 1 TO 8 * (nlevels + 1) DO
      averagecount[i] := 0;
   FOR i := 1 TO numchi DO BEGIN
      IF ((restart = false) AND (quitf = false) AND (cntlmthrm = false)) THEN BEGIN
         CheckForEvents;
         IF HotKeys.Pressed(key) THEN
            HandleHotKeys(htype, key, STEP);
         MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
         FOR i2 := 1 TO (nlevels + 1) DO
            icount[i2] := 0;
         FOR i1 := 1 TO nballs DO BEGIN
            ilevel := 1;
            ileveli := 1;
            ilevelf := 1;
            REPEAT
               EquationOfMotion(ilevel, ileveli, prob, ilevelf);
               Inc(ilevel);
               ileveli := ilevelf;
            UNTIL (ilevel = nlevels + 1);
            Inc(icount[ilevelf]);
         END; { FOR i1 := 1 TO nballs DO }
         IF ((restart = false) AND (quitf = false) AND (cntlmthrm = false)) THEN BEGIN
            Statistics(nlevels, icount, average, onesigma, nball);
            chiavg[i] := average;
            avgchi := avgchi + average;
            PlotAverages(binw, nlevels, numdiv, baverage, average, xwidth, ybottom, deltay, xcen, shape, averagecount, ycen);
            IF (Odd(i - 1)) AND (numdiv > 15) THEN
               CountDown(i, numdiv, deltay, ybottom, xcen)
            ELSE IF ((i - (i DIV 10) * 10) = 0) THEN
               CountDown(i, numdiv, deltay, ybottom, xcen);
         END;  { IF ((restart = false)... }
      END;  { IF ((restart = false ... }
   END; { FOR i := 1 TO numchi DO }
   IF ((restart = false) AND (quitf = false) AND (cntlmthrm = false)) THEN BEGIN
      avgchi := avgchi / rnumchi;
      FOR i := 1 TO numchi DO
         chisigma2 := chisigma2 + SQR(chiavg[i] - avgchi);
      chisigma2 := chisigma2 / (rnumchi - 1.0);
      binstart := baverage - binw + (2.0 * binw) / (2.0 * rnumdiv);
      ninbin := 0;
      FOR i := 1 TO numdiv DO
         ninbin := ninbin + averagecount[i];
      writeln(Output);
      writeln(Output, ' Chi Square Goodness of Fit Test');
      writeln(Output, ' number of divisions: ', numdiv);
      IF (chisigma2 > 0.0) THEN BEGIN
         e0 := ninbin * (2.0 * binw) / rnumdiv * exp(-SQR(binstart -
           (2.0 * binw) / rnumdiv - avgchi) / (2.0 * chisigma2)) /
           SQRT(2.0 * Pi * chisigma2);
         FOR i := 1 TO numdiv DO BEGIN
            ei[i] := ninbin * (2.0 * binw) / rnumdiv * exp(-SQR(binstart -
              avgchi) / (2.0 * chisigma2)) / SQRT(2.0 * Pi * chisigma2);
            binstart := binstart + (2.0 * binw) / rnumdiv;
            chi := chi + SQR(averagecount[i] - ei[i]) / ei[i];
            writeln(Output, ' averagecount[', i : 2, '] ', averagecount[i]
              : 3, '    ei[', i : 2, '] ', ei[i] : 5 : 3);
         END;   { FOR i := 1 TO numdiv DO }
      END  { IF (chisigma2 > 0.0) }
      ELSE
         chi := 0.0;
      DrawNormal(e0, numdiv, xwidth, ybottom, deltay, ei, xcen);
      writeln(Output, ' average of averages ', avgchi : 8 : 5);
      writeln(Output, ' theoretical average of averages ', baverage : 8 : 5);
      writeln(Output, ' Chi Square ', chi : 10 : 7, ' with ',
              NumStr(numdiv - 3, 2, 0) + ' Degrees of Freedom');
      StatisticOutput := 'The average of the averages is';
      Print((center DIV 8) + 2, (ytop DIV 8) + 15, StatisticOutput);
      StatisticOutput := NumStr(avgchi, 6, 3) + '. The theoretical';
      Print((center DIV 8) + 2, (ytop DIV 8) + 16, StatisticOutput);
      StatisticOutput := 'average of averages is ' + NumStr(baverage, 6, 3) + '.';
      Print((center DIV 8) + 2, (ytop DIV 8) + 17, StatisticOutput);
      StatisticOutput := 'The Chi-Square is ' + NumStr(chi, 6, 2);
      Print((center DIV 8) + 2, (ytop DIV 8) + 18, StatisticOutput);
      StatisticOutput := 'with ' + NumStr(numdiv - 3, 2, 0) + ' Degrees of Freedom';
      Print((center DIV 8) + 2, (ytop DIV 8) + 19, StatisticOutput);
   END;  { IF ((restart = false ... }
   IF ((restart = true) OR (quitf = true) OR (cntlmthrm = true)) THEN BEGIN
      ClearViewPort;
      CloseViewPort(1);
      CUPSDone;
   END;
END;  { PROCEDURE CntrlLmtThrmCal }

BEGIN  { PROGRAM GALTON_BOARD }
   CUPSinit;
   ABOUTPROGRAM;
   CUPSdone;
   Randomize;  { Randomize the initial random number seed }
   btype := 1;
   nballs := 100;
   nlevels := 6;
   nlevelsold := 0;
   p := 0.5;
   FOR i := 1 TO 8 DO
      FOR j := 1 TO 8 DO
         prob[i, j] := p;
   REPEAT  { UNTIL quitf = false }
      CUPSinit;
      DefineViewPort(1, 0.1, 0.9, 0.1, 0.9);
      DefineScale(1, 0.1, 0.9, 0.1, 0.9);
      IF (quitf = false) THEN BEGIN
         IF (cntlmthrm = false) THEN BEGIN
            ClearMuppetPort;
            restart := false;
            IF (first = true) THEN BEGIN
               ReadData;
               first := false;
               ASSIGN(Output, Galton_Output);
               REWRITE(Output);
            END
            ELSE
               ReadData1;
            IF (btype = 1) THEN BEGIN
               FOR i := 1 TO nlevels DO
                  FOR i1 := 1 TO nlevels DO
                     prob[i, i1] := p;
            END;
            htype := btype;
       { customized Galton Board }
            IF (btype = 2) THEN BEGIN
               IF (nlevels <> nlevelsold) THEN BEGIN
                  FOR i := 1 TO nlevelsold DO
                     FOR i1 := 1 TO nlevelsold DO
                        prob[i, i1] := p;
                  nlevelsold := nlevels;
               END;
               CreateCustomBoard(nlevels);
            END;
            MouseBorders(0, GetMaxy - 15, GetMaxx - 5, GetMaxy - 10);
            ClearMuppetPort;
            hidecursor;
            SetUpHotKeys(HotKeys);
            HotKeys.Display;
            SetUpMyMenu;
            FOR i := 1 TO 11 DO
               icount[i] := 0;
            GALTON_BOARD_Initialize(nlevels, nballs, shape, p, prob,
                  btype, ballsize, center, small, ytop, binomialcount,
                  customcount, ynormal, x, y, xbin, ybinfill, right,
                  ybinbottom, ybintop, baverage);
            Predictions(ballsize, btype, nballs, nlevels, right, ytop,
                  p, baverage, binomialcount, customcount, ynormal);
            ShowCursor;
            FOR i := 1 TO nballs DO BEGIN
               IF ((quitf = false) AND (restart = false) AND
                  (cntlmthrm = false)) THEN BEGIN
                  MoveBall(beepon, ballsize, center, nlevels, ytop, small,
                     ybintop, x, y, xbin, prob, shape, binomialcount,
                     customcount, ynormal, deltat, btype, STEP, ybinfill,
                     icount);
                  CheckForEvents;
                  IF HotKeys.Pressed(key) THEN
                     HandleHotKeys(htype, key, STEP);
                  MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
               END; { IF ((quitf = false ... }
            END;  { FOR i := 1 TO nballs DO }
            IF ((quitf = false) AND (restart = false)
                AND (cntlmthrm = false)) THEN BEGIN
               Statistics(nlevels, icount, average, onesigma, nball);
               StatisticsOut(ballsize, btype, nballs, nlevels, right,
                  ytop, nball, p, average, baverage, onesigma, icount,
                  binomialcount, customcount, ynormal, actavg);
            END;  { IF (quitf ... }
            HideCursor;
            MouseBorders(0, 0, GetMaxX, GetMaxY);
            ShowCursor;
            REPEAT
               CheckForEvents;
               IF HotKeys.Pressed(key) THEN
                  IF (key = 1) AND (htype = 1) THEN
                     Help1
                  ELSE IF (key = 1) AND (htype = 2) THEN
                     Help2;
               IF (MyMenuOut.Activated) THEN
                  HandleMenu;
            UNTIL ((restart = true) OR (cntlmthrm = true) OR (quitf = true));
         END;   { IF (cntlmthrm = false) }
         ClearMuppetPort;
         menuf := false;
         CloseViewPort(1);
         IF (cntlmthrm = true) THEN BEGIN  { Central Limit Theorem }
            htype := 3;
            cntlmthrm := false;
            numchi := 250;
            rnumchi := numchi;
            IF (nballs < 20) THEN BEGIN
               numdiv := 15;
               binw := 1.5;
            END  { IF (nballs < 20) THEN }
            ELSE BEGIN
               numdiv := 30;
               binw := 1.00;
            END;  { ELSE }
            HideCursor;
            MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
            ShowCursor;
            CntrlLmtThrmInitialize(binw, nlevels, nballs, numdiv, btype,
               shape, xwidth, ybottom, center, deltay, xcen, ycen);
            CntrlLmtThrmDef(numchi, baverage, xcen, ybottom, center, ytop);
            MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
            ShowCursor;
            CntrlLmtThrmCal(numchi, binw, nballs, nlevels, numdiv, xwidth,
               ybottom, deltay, shape, xcen, ycen, prob, baverage,
               restart, cntlmthrm, quitf);
         END;  { Central Limit Theorem }
         HideCursor;
         MouseBorders(0, 0, GetMaxX, GetMaxY);
         ShowCursor;
         REPEAT
            CheckForEvents;
            IF (MyMenuOut.Activated) THEN
               HandleMenu;
            IF HotKeys.Pressed(key) THEN
               IF (key = 1) THEN
                  Help3;
         UNTIL ((restart = true) OR (cntlmthrm = true) OR (quitf = true));
         CUPSDone;
      END;   { IF (quitf = false) }
   UNTIL quitf = true;
   Close(Output);
   CupsDone;
END.  {  PROGRAM GALTON_BOARD  }


