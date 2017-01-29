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
         {**             Program KAC_RING                **}
         {**                                             **}
         {** This programs models either a large (2001   **}
         {** balls) or a small (9 balls) KAC RING and    **}
         {** their appropriate statistical models.       **}
         {**                                             **}
         {** A KAC RING consists of a ring with a        **}
         {** number (n) of points around the perimeter.  **}
         {**                                             **}
         {** Between each set of points is a ball which  **}
         {** can be either dark or white.                **}
         {**                                             **}
         {** At m of the points are markers.             **}
         {**                                             **}
         {** As the balls move around the ring, they     **}
         {** change color when they pass a point with a  **}
         {** marker.                                     **}
         {**                                             **}
         {** The KAC RING and the ensemble (statistical  **}
         {** average) can be used to describe how a real **}
         {** system (the KAC RING) and the statistical   **}
         {** average differ and demonstrates that,       **}
         {** during a significant portion of time,       **}
         {** the system is close to equilibrium (i.e.,   **}
         {** when the number of dark balls = the number  **}
         {** of white balls).                            **}
         {**                                             **}
         {** This helps to demonstrate the validity of   **}
         {** the Boltzmann's H Theorem, Kinetic Theory   **}
         {** and Statistical Mechanics                   **}
         {**                                             **}
         {**         Written by L. B. Spornick           **}
         {**         Applied Physics Laboratory          **}
         {**         The Johns Hopkins University        **}
         {**                                             **}
         {**            September 23, 1991               **}
         {**           ver 1.0, July, 1993               **}
         {**           ver 1.2, Jan, 1994                **}
         {**           ver 1.3, May, 1994                **}
         {**           ver 1.4, Apr, 1995                **}
         {**                                             **}
         {*************************************************}
         {*************************************************}




PROGRAM KAC_RING;

   USES
      Graph, CUPSMUPP, CUPS, CUPSgui, CUPSfunc, Crt;

   {$M 32768,0,655360}

   CONST
      deltat:  INTEGER = 750;
      STEP:    BOOLEAN = false;
      beepon:  BOOLEAN = false;
      menuf:   BOOLEAN = false;
      restart: BOOLEAN = false;
      quitf:   BOOLEAN = false;
      cancel:  BOOLEAN = false;
      first:   BOOLEAN = true;
      KAC_Output: STRING = 'KAC.dat';

   TYPE
      arrays = ARRAY[1..130, 1..9] OF INTEGER;
      markers = ARRAY[1..2001] OF INTEGER;
      small_markers = ARRAY[1..9] OF INTEGER;
      small_marker_array = ARRAY[1..5, 1..9] OF INTEGER;

   VAR
      above, above2, ax, ax1, ax2, ay, ay1, ay2, error, i, i1, i2, icount, ir, ixc, iyc, n, nb, n0, m, maxi, t, tmax: INTEGER;
      im, ib: markers;
      xm, ym: small_markers;
      xb, yb, xbr, ybr: small_marker_array;
      ibe, ime: arrays;
      GraphDriver, GraphMode: INTEGER;
      ch: CHAR;
      averagediff, averagesig, ensemble_average, actual, actual_average, actual_sigma, ensemble_averagep, actualp: REAL;
      am, an, mu: EXTENDED;
      HotKeys, HotKeys2: THotKeys;
      key: BYTE;
      MyMenuOut: TMenu;

   PROCEDURE HELP1;
      VAR
         helpScr: HelpScrType;
   BEGIN
      helpScr[1] := '                                                  ';
      helpScr[2] := '                                                  ';
      helpScr[3] := '                The Small KAC RING                ';
      helpScr[4] := '                                                  ';
      helpScr[5] := '                                                  ';
      helpScr[6] := '    The screen is split:                          ';
      helpScr[7] := '                                                  ';
      helpScr[8] := '                                                  ';
      helpScr[9] := '    The left side displays a nine ball KAC RING   ';
      helpScr[10] := '    turning first 18 steps counterclockwise       ';
      helpScr[11] := '    (one cycle) then reversing itself (i.e.,      ';
      helpScr[12] := '    turning clockwise) 18 more steps.             ';
      helpScr[13] := '                                                  ';
      helpScr[14] := '                                                  ';
      helpScr[15] := '    The right side displays a plot of the         ';
      helpScr[16] := '    number of dark balls (N(D)) minus the         ';
      helpScr[17] := '    number of white balls (N(L)) for the          ';
      helpScr[18] := '    KAC RING on the left (the red curve) and      ';
      helpScr[19] := '    for the ensemble average (the blue curve)     ';
      helpScr[20] := '    as a function of the number of steps.         ';
      helpScr[21] := '                                                  ';
      helpScr[22] := '                                                  ';
      helpScr[23] := '                                                  ';
      helpScr[24] := '                                                  ';
      helpScr[25] := '                                                  ';
      HELP(helpScr);
   END;  {  PROCEDURE HELP1  }

   PROCEDURE HELP2;
      VAR
         helpScr: HelpScrType;
   BEGIN
      helpScr[1] := '                                                  ';
      helpScr[2] := '               The Large KAC RING                 ';
      helpScr[3] := '                                                  ';
      helpScr[4] := '    There are two screens:                        ';
      helpScr[5] := '                                                  ';
      helpScr[6] := '    The first screen is a plot of the number      ';
      helpScr[7] := '    of dark balls (N(D)) minus the number of      ';
      helpScr[8] := '    white balls (N(L)) for a 2001 ball KAC        ';
      helpScr[9] := '    RING.  These numbers are plotted as they      ';
      helpScr[10] := '    are calculated.                               ';
      helpScr[11] := '                                                  ';
      helpScr[12] := '    The average and one sigma values for          ';
      helpScr[13] := '    N(D)-N(L) are calculated from this data.      ';
      helpScr[14] := '                                                  ';
      helpScr[15] := '    The second screen displays N(D)-N(L)          ';
      helpScr[16] := '    for the KAC RING (the red curve) and          ';
      helpScr[17] := '    <N(D)-N(L)> for a statistical model (the      ';
      helpScr[18] := '    yellow curve).                                ';
      helpScr[19] := '                                                  ';
      helpScr[20] := '    The number of times |N(D)-N(L)| exceeds one   ';
      helpScr[21] := '    sigma (the blue lines) or two sigma (the      ';
      helpScr[22] := '    green lines) is calculated along with a       ';
      helpScr[23] := '    comparison of N(D)-N(L) to <N(D)-N(L)>.       ';
      helpScr[24] := '                                                  ';
      helpScr[25] := '                                                  ';
      HELP(helpScr);
   END;  {  PROCEDURE HELP2  }

   PROCEDURE ABOUTPROGRAM;
      VAR
         helpScr: HelpScrType;
   BEGIN
      helpScr[1] := '                                                  ';
      helpScr[2] := '                 The Kac Ring                     ';
      helpScr[3] := '                                                  ';
      helpScr[4] := '                      by                          ';
      helpScr[5] := '                                                  ';
      helpScr[6] := '                 L.B. Spornick                    ';
      helpScr[7] := '           Applied Physics Laboratory             ';
      helpScr[8] := '          The Johns Hopkins University            ';
      helpScr[9] := '                                                  ';
      helpScr[10] := '                   April 1995                     ';
      helpScr[11] := '               Copyright (c) 1995                 ';
      helpScr[12] := '                                                  ';
      helpScr[13] := '   This program models either a small (9 balls)   ';
      helpScr[14] := '   or a large (2001 balls) Kac Ring. The Kac Ring ';
      helpScr[15] := '   demonstrates the relationship between a real   ';
      helpScr[16] := '   system and its ensemble average.  It shows     ';
      helpScr[17] := '   that as the size of the system approaches      ';
      helpScr[18] := '   infinity, there is agreement between the       ';
      helpScr[19] := '   system average and the ensemble average.       ';
      helpScr[20] := '                                                  ';
      helpScr[21] := '                                                  ';
      helpScr[22] := '                                                  ';
      helpScr[23] := '                                                  ';
      helpScr[24] := '                                                  ';
      helpScr[25] := '                                                  ';
      HELP(helpScr);
   END;  {  PROCEDURE ABOUTPROGRAM  }

   FUNCTION Fac (n: INTEGER): EXTENDED;
      VAR
         i: LONGINT;
         Fac1: EXTENDED;
   BEGIN
      Fac1 := 1;
      IF (n > 0) THEN
      BEGIN
         FOR i := 1 TO n DO
            Fac1 := Fac1 * i;
      END;
      Fac := Fac1;
   END; { FUNCTION Fac }

   FUNCTION Power (s: INTEGER; x: EXTENDED): EXTENDED;
      VAR
         i: INTEGER;
         Powerm: EXTENDED;
   BEGIN
       { Power := Exp(s*Ln(x)); }
      Powerm := 1.0E0;
      IF (s > 0) THEN
      BEGIN
         FOR i := 1 TO s DO
            Powerm := Powerm * x;
      END;
      Power := Powerm;
   END;  { FUNCTION Power }

   PROCEDURE ReadData;
      VAR
         screen: TInputScreen;
         nr, error: INTEGER;
   BEGIN
      WITH screen DO
      BEGIN
         init;
         DefineInputPort(0.15, 0.87, 0.1, 0.9);
         LoadLine('                 The KAC Ring                       ');
         LoadLine('                                                    ');
         LoadLine('  Ring Size:                                        ');
         LoadLine('   #1 Small Ring (9 points)                          ');
         LoadLine('   #1 Large Ring (2001 points)                       ');
         LoadLine('                                                    ');
         LoadLine('  Number of Markers:              {      }                      ');
         LoadLine('   For a Small Ring, ( 1 to 8 )    ');
         LoadLine('   For a Large Ring, ( 500 to 1500 )   ');
         LoadLine('                                                    ');
         LoadLine('  Initial Number of Dark Balls:   {      }            ');
         LoadLine('   For a Small Ring, ( 0 to 9 )        ');
         LoadLine('   For a Large Ring, ( 0 to 2001 )     ');
         LoadLine('                                                    ');
         LoadLine('  Number of Moves for Large Ring: {      } (10 to 200)          ');
         LoadLine('                                                    ');
         LoadLine('  Output File Name: "                    "         ');
         LoadLine('                                                    ');
         LoadLine('               [   Ok   ]   [Cancel]               ');
         IF (n = 9) THEN
            nr := 1
         ELSE
            nr := 2;
         SetRadioButton('1', nr);
         SetNumber(3, m);
         SetNumber(4, nb);
         SetNumber(5, tmax);
         SetString(6, KAC_Output);
         REPEAT
            error := 0;
            Accept;
            nr := GetRadioButton('1');
            IF (nr = 1) THEN
               n := 9
            ELSE
               n := 2001;
            m := Trunc(GetNumber(3));
            nb := Trunc(GetNumber(4));
            tmax := Trunc(GetNumber(5));
            KAC_Output := GetString(6);
            IF (nr = 1) THEN
            BEGIN
               IF ((m < 1) OR (m > 8)) THEN
               BEGIN
                  Announce('Error in Number of Markers');
                  error := 1;
               END
               ELSE IF ((error = 1) OR (error = 11)) THEN
                  error := error - 1;
               IF ((nb < 0) OR (nb > 9)) THEN
               BEGIN
                  Announce('Error in the Initial Number of Dark Balls');
                  error := error + 10;
               END
               ELSE IF ((error = 10) OR (error = 11)) THEN
                  error := error - 10;
               tmax := 18;
            END
            ELSE
            BEGIN
               IF ((m < 500) OR (m > 1500)) THEN
               BEGIN
                  Announce('Error in Number of Markers');
                  error := 1;
               END
               ELSE IF ((error = 1) OR (error = 11) OR (error = 101) OR (error = 111)) THEN
                  error := error - 1;
               IF ((nb < 0) OR (nb > 2001)) THEN
               BEGIN
                  Announce('Error in the Initial Number of Dark Balls');
                  error := error + 10;
               END
               ELSE IF ((error = 10) OR (error = 11) OR (error = 110) OR (error = 111)) THEN
                  error := error - 10;
               IF ((tmax < 10) OR (tmax > 200)) THEN
               BEGIN
                  Announce('Error in the Number of Moves');
                  error := error + 100;
               END
               ELSE IF ((error = 100) OR (error = 101) OR (error = 110) OR (error = 111)) THEN
                  error := error - 100;
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
         DefineInputPort(0.15, 0.87, 0.13, 0.9);
         LoadLine('                 The KAC Ring                       ');
         LoadLine('                                                    ');
         LoadLine('  Ring Size:                                        ');
         LoadLine('   #1 Small Ring (9 points)                          ');
         LoadLine('   #1 Large Ring (2001 points)                       ');
         LoadLine('                                                    ');
         LoadLine('  Number of Markers:              {      }                      ');
         LoadLine('   For a Small Ring, ( 1 to 8 )    ');
         LoadLine('   For a Large Ring, ( 500 to 1500 )   ');
         LoadLine('                                                    ');
         LoadLine('  Initial Number of Dark Balls:   {      }            ');
         LoadLine('   For a Small Ring, ( 0 to 9 )        ');
         LoadLine('   For a Large Ring, ( 0 to 2001 )     ');
         LoadLine('                                                    ');
         LoadLine('  Number of Moves for Large Ring: {      } (10 to 200)          ');
         LoadLine('                                                    ');
         LoadLine('               [   Ok   ]   [Cancel]               ');
          IF (n = 9) THEN
            nr := 1
         ELSE
            nr := 2;
         SetRadioButton('1', nr);
         SetNumber(3, m);
         SetNumber(4, nb);
         SetNumber(5, tmax);
         REPEAT
            error := 0;
            Accept;
            nr := GetRadioButton('1');
            IF (nr = 1) THEN
               n := 9
            ELSE
               n := 2001;
            m := Trunc(GetNumber(3));
            nb := Trunc(GetNumber(4));
            tmax := Trunc(GetNumber(5));
            IF (nr = 1) THEN
            BEGIN
               IF ((m < 1) OR (m > 8)) THEN
               BEGIN
                  Announce('Error in Number of Markers');
                  error := 1;
               END
               ELSE IF ((error = 1) OR (error = 11)) THEN
                  error := error - 1;
               IF ((nb < 0) OR (nb > 9)) THEN
               BEGIN
                  Announce('Error in the Initial Number of Dark Balls');
                  error := error + 10;
               END
               ELSE IF ((error = 10) OR (error = 11)) THEN
                  error := error - 10;
            END
            ELSE
            BEGIN
               IF ((m < 500) OR (m > 1500)) THEN
               BEGIN
                  Announce('Error in Number of Markers');
                  error := 1;
               END
               ELSE IF ((error = 1) OR (error = 11) OR (error = 101) OR (error = 111)) THEN
                  error := error - 1;
               IF ((nb < 0) OR (nb > 2001)) THEN
               BEGIN
                  Announce('Error in the Initial Number of Dark Balls');
                  error := error + 10;
               END
               ELSE IF ((error = 10) OR (error = 11) OR (error = 110) OR (error = 111)) THEN
                  error := error - 10;
               IF ((tmax < 10) OR (tmax > 200)) THEN
               BEGIN
                  Announce('Error in the Number of Moves');
                  error := error + 100;
               END
               ELSE IF ((error = 100) OR (error = 101) OR (error = 110) OR (error = 111)) THEN
                  error := error - 100;
            END;
         UNTIL (error = 0);
         cancel := canceled;
         DONE;
      END;
   END;  { PROCEDURE ReadData1 }

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
       CASE colChosen OF
        1: CASE rowChosen OF
            1: ABOUTPROGRAM;
            2: AboutCups;
            3: Configuration;
            5: restart := true;
            7: quitf := true;
           END; { CASE }
       END;
   END; { PROCEDURE HandleMenu }

   PROCEDURE SetUpHotKeys (VAR HotKeys: THotKeys);
   BEGIN
      WITH HotKeys DO
      BEGIN
         init(5);
         key[1] := 'F1-Help';
         key[2] := 'F3-Step';
         key[3] := 'F5-Slower';
         key[4] := 'F6-Faster';
         key[5] := 'F10-Menu';
      END;
   END;  { PROCEDURE SetUpHotKeys }

   PROCEDURE HandleHotKeys (key: BYTE; VAR deltat: INTEGER; VAR STEP: BOOLEAN);
   BEGIN
      CASE key OF
         1: 
            HELP1;
         2: 
            STEP := NOT STEP;  { F3 - Step }
         3: 
         BEGIN      { F5 - Slower }
            deltat := round(1.3*deltat);
            IF (deltat > 2000) THEN begin
               deltat := 2000;
               beep;
            end;
         END;
         4:
         BEGIN      { F6 - Faster }
            deltat := round(deltat/1.3);
            IF (deltat < 50) THEN begin
               deltat := 50;
               beep;
            end;
         END;
         5:
         BEGIN   { F10 - Menu }
            MouseBorders(0, 0, GetMaxX, GetMaxY);
            IF MyMenuOut.chosen THEN
               HandleMenu(restart, quitf);
         END;
      END;  { CASE }
   END;  { PROCEDURE HandleHotKeys }

   PROCEDURE SetUpHotKeys2 (VAR HotKeys: THotKeys);
   BEGIN
      WITH HotKeys DO
      BEGIN
         init(2);
         key[1] := 'F1-Help';
         key[2] := 'F10-Menu';
      END;
   END;  { PROCEDURE SetUpHotKeys }

   PROCEDURE HandleHotKeys2 (key: BYTE);
   BEGIN
      CASE key OF
         1: 
            if n=9 then help1 else HELP2;
         2: 
         BEGIN
            MouseBorders(0, 0, GetMaxX, GetMaxY);
            IF MyMenuOut.chosen THEN
               HandleMenu(restart, quitf);
         END;
      END;  { CASE }
   END;  { PROCEDURE HandleHotKeys }

   PROCEDURE Move_Ring (im: markers; n: INTEGER; VAR ib: markers);

     { ******************************************************** }
     {                            }
     { PROCEDURE Move_Ring moves the Kac Ring counterclockwise  }
     {                            }
     { ******************************************************** }

      VAR
         inb: markers;
         i, i1: INTEGER;
   BEGIN
      FOR i1 := 1 TO (n - 1) DO
         inb[i1 + 1] := ib[i1] * im[i1 + 1];
      inb[1] := ib[n] * im[1];
      FOR i1 := 1 TO n DO
         ib[i1] := inb[i1];
   END;  { PROCEDURE Move_Ring }

   PROCEDURE Move_Ring_Back (im: markers; n: INTEGER; VAR ib: markers);

     { ***************************************************** }
     {                         }
     { PROCEDURE Move_Ring moves the Kac Ring clockwise      }
     {                         }
     { ***************************************************** }

      VAR
         inb: markers;
         i, i1: INTEGER;
   BEGIN
      FOR i1 := 2 TO n DO
         inb[i1 - 1] := ib[i1] * im[i1];
      inb[n] := ib[1] * im[1];
      FOR i1 := 1 TO n DO
         ib[i1] := inb[i1];
   END;  { PROCEDURE Move_Ring_Back }

   PROCEDURE Move_Ensemble (n, maxi: INTEGER; ime: arrays; VAR ibe: arrays);

     { ******************************************************* }
     {                                                         }
     { PROCEDURE Move_Ensemble moves the member rings of the   }
     { ensemble counterclockwise                               }
     {                                                         }
     { ******************************************************* }

      VAR
         i, i1: INTEGER;
         inb: markers;

   BEGIN
      FOR i := 1 TO maxi DO
      BEGIN
         FOR i1 := 1 TO (n - 1) DO
            inb[i1 + 1] := ibe[i, i1] * ime[i, i1 + 1];
         inb[1] := ibe[i, n] * ime[i, 1];
         FOR i1 := 1 TO n DO
            ibe[i, i1] := inb[i1];
      END;
   END;  { PROCEDURE Move_Ensemble }

   PROCEDURE Move_Ensemble_Back (n, maxi: INTEGER; ime: arrays; VAR ibe: arrays);

     { ****************************************************** }
     {                                                        }
     { PROCEDURE Move_Ensemble_Back moves the ensemble rings  }
     { clockwise                                              }
     {                                                        }
     { ****************************************************** }

      VAR
         i, i1: INTEGER;
         inb: markers;

   BEGIN
      FOR i := 1 TO maxi DO
      BEGIN
         FOR i1 := 2 TO n DO
            inb[i1 - 1] := ibe[i, i1] * ime[i, i1];
         inb[n] := ibe[i, 1] * ime[i, 1];
         FOR i1 := 1 TO n DO
            ibe[i, i1] := inb[i1];
      END;
   END;  { PROCEDURE Move_Ensemble_Back }

   PROCEDURE Initialize_System (n, m, nb: INTEGER; VAR im, ib: markers);

    { ************************************************************ }
    {                        }
    { PROCEDURE Initialize_System initialize the system by    }
    { determining im and ib.                }
    {                        }
    { ************************************************************ }

      VAR
         i, i1: INTEGER;

   BEGIN
      FOR i := 1 TO n DO
         im[i] := 1;

      { randomly select the m markers }

      i := 0;
      REPEAT
         IF n = 2001 THEN
            i1 := Round(Random * 2000.0E0 + 0.5) + 1
         ELSE
            i1 := Round(Random * 8.0 + 0.5) + 1;
         IF (im[i1] = 1) THEN
         BEGIN
            INC(i);
            im[i1] := -1;
         END;
      UNTIL (i = m);

      { randomly select the nb white balls }

      FOR i := 1 TO n DO
         ib[i] := 1;
      i := 0;
      IF (nb = 0) THEN
      BEGIN
         FOR i1 := 1 TO n DO
            ib[i1] := -1;
      END
      ELSE IF (n <> nb) THEN
      BEGIN
         REPEAT
            IF n = 2001 THEN
               i1 := Round(Random * 2000 + 0.5) + 1
            ELSE
               i1 := Round(Random * 8 + 0.5) + 1;
            IF (ib[i1] = 1) THEN
            BEGIN
               INC(i);
               ib[i1] := -1;
            END;
         UNTIL (i = n - nb);
      END;

   END;  { PROCEDURE Initial_System }

   PROCEDURE Compute_Average (n: INTEGER; ib, im: markers;
             VAR actual_average, actual_sigma: REAL; VAR quitf, restart: BOOLEAN);

     { *************************************************************** }
     {                              }
     { PROCEDURE Compute Average computes the system average (actual_  }
     { average) and the system sigma (actual_sigma) and plots them     }
     {                              }
     { *************************************************************** }

      VAR
         i, i1: INTEGER;
         iba: markers;
         intermediate, intermediatep, an: REAL;
         tmess: STRING;
   BEGIN
       { prepare plot }
      an := n;
      SetUpHotKeys2(HotKeys);
      HotKeys.Display;
      SetUpMyMenu;
{      SetBkColor(Blue);}
      DefineViewPort(2, 0, 1, 0.055, 0.95);
      graphBackColor := blue;
      OpenViewPort(2);
      graphBackColor := darkGray;
      DefineViewPort(1, 0.1, 0.9, 0.11, 0.9);
      DefineScale(1, 0.0, an, -an, an);
      OpenViewPort(1);
      SelectScale(1);
      SetColor(White);
      Axis(0.0, 0.0, 500.0, 500.0);
      PutLabel(inside, 'Computing the System Average of N(D) - N(L)');
      PutLabel(bottom, 'number of moves');
      PutLabel(left, 'N(D) - N(L)');
       { initialize iba, actual_average, actual_sigma }
      FOR i := 1 TO n DO
         iba[i] := ib[i];
      intermediate := 0;
      FOR i1 := 1 TO n DO
      BEGIN
         intermediate := iba[i1] + intermediate;
      END;
      actual_average := intermediate;
      actual_sigma := SQR(intermediate);
   { move ring }
      FOR i := 1 TO n DO
      BEGIN
         IF ((quitf = false) AND (restart = false)) THEN
         BEGIN
            intermediatep := intermediate;
            Move_Ring(im, n, iba);
            intermediate := 0;
           {  compute average and sigma }
            FOR i1 := 1 TO n DO
               intermediate := iba[i1] + intermediate;
            actual_average := actual_average + intermediate;
            actual_sigma := actual_sigma + SQR(intermediate);
     { plot line }
            SetColor(red);
            PlotLine((i - 1), intermediatep, i, intermediate);
            CheckForEvents;
            IF HotKeys.Pressed(key) THEN
               HandleHotKeys2(key);
            MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
         END;  { IF (quitf = false) ... }
      END;
      IF ((quitf = false) AND (restart = false)) THEN
      BEGIN
         actual_average := actual_average / (an + 1.0);
         actual_sigma := actual_sigma / (an + 1.0) - SQR(actual_average);
         actual_sigma := SQRT(actual_sigma);
         writeln(Output, ' actual_average of N(D)-N(L): ', actual_average : 10 : 7);
         writeln(Output, ' actual_sigma of N(D)-N(L): ', actual_sigma : 10 : 7);
         SetColor(lightred);
         PlotLine(0, actual_average, 2 * an, actual_average);
         tmess := 'system average: ' + NumStr(actual_average, 6, 3);
         Print(35, 14, tmess);
         SetColor(lightblue);
         PlotLine(0, actual_sigma, 2 * an, actual_sigma);
         PlotLine(0, -actual_sigma, 2 * n, -actual_sigma);
         tmess := 'system sigma:    ' + NumStr(actual_sigma, 6, 3);
         Print(35, 15, tmess);
         SetColor(lightgreen);
         PlotLine(0, 2 * actual_sigma, 2 * an, 2 * actual_sigma);
         PlotLine(0, -2 * actual_sigma, 2 * an, -2 * actual_sigma);
         SetColor(white);
         beep;
         PressKeyMessage;
         ClearMUPPETport;
      END;  { IF (quitf = false) ... }
   END;  { PROCEDURE Compute_Average }

   PROCEDURE PreparePlot (actual_average, actual_sigma, an, tmax: REAL);

   BEGIN
{      SetBkColor(Blue);}
      DefineViewPort(2, 0, 1, 0.055, 0.95);
      graphBackColor := blue;
      OpenViewPort(2);
      graphBackColor := darkGray;
      DefineViewPort(1, 0.1, 0.9, 0.11, 0.9);
      DefineScale(1, 0.0, tmax, -an, an);
      OpenViewPort(1);
      SelectScale(1);
      SetColor(White);
      if (tMax>=40) then Axis(0.0, 0.0, 20.0, 500.0)
      else Axis(0,0,4,500);
      SetColor(lightred);
      PutLabel(inside, 'N(D)-N(L)                ');
      SetColor(yellow);
      PutLabel(inside, '              <N(D)-N(L)>');
      SetColor(white);
      PutLabel(inside, '          and            ');
      PutLabel(bottom, 'number of moves');
      PutLabel(left, 'N(D) - N(L)');
      SetColor(lightred);
      PlotLine(0, actual_average, tmax, actual_average);
      SetColor(lightblue);
      PlotLine(0, actual_sigma, tmax, actual_sigma);
      PlotLine(0, -actual_sigma, tmax, -actual_sigma);
      SetColor(lightgreen);
      PlotLine(0, 2 * actual_sigma, tmax, 2 * actual_sigma);
      PlotLine(0, -2 * actual_sigma, tmax, -2 * actual_sigma);
   END;   { PROCEDURE PreparePlot }

   PROCEDURE Statistics (ib: markers; n, n0, t: INTEGER; mu: EXTENDED; VAR ensemble_average, actual: REAL);

     { ******************************************************* }
     {                           }
     { PROCEDURE Statistics computes the actual value of       }
     { N(R) - N(L) (actual) and the ensemble average of N(R) - }
     { N(L) (ensemble_average)                  }
     {                           }
     { ******************************************************* }

      VAR
         average: REAL;
         i, i1: INTEGER;
         mus, one_minus_mut, onei, Facs, Fact, Facst: EXTENDED;

   BEGIN
      ensemble_average := 0.0E0;
      actual := 0.0E0;
      { compute actual value }
      FOR i1 := 1 TO n DO
         actual := actual + ib[i1];
      { compute ensemble average }
      FOR i := 0 TO t DO
      BEGIN
         Facs := Fac(i);
         Fact := Fac(t);
         Facst := Fac(t - i);
         IF Odd(i) THEN
            onei := -1.0E0
         ELSE
            onei := 1.0E0;
         mus := Power(i, mu);
         one_minus_mut := Power(t - i, (1.0E0 - mu));
         ensemble_average := ensemble_average + onei * mus * one_minus_mut * n0 * Fact / (Facs * Facst);
      END;
   END;  {PROCEDURE Statistics }

   PROCEDURE StatisticsOutput (above, above2: INTEGER; averagediff, averagesig, actual_sigma: REAL);
      VAR
         tmess: STRING;
         aabove: REAL;
   BEGIN
      SetColor(white);
      tmess := 'Number of points above or below:';
      Print(20, 13, tmess);
      aabove := above;
      SetColor(lightblue);
      tmess := 'one sigma(' + NumStr(actual_sigma, 6, 2) + '):   ' + NumStr(aabove, 3, 0);
      writeln(Output, 'The number of points above or below one sigma(', actual_sigma : 6 : 2, ') is ', above : 3);
      Print(25, 14, tmess);
      aabove := above2;
      SetColor(lightgreen);
      tmess := 'two sigma(' + NumStr(2.0 * actual_sigma, 6, 1) + '):   ' + NumStr(aabove, 3, 0);
      Print(25, 15, tmess);
      SetColor(white);
      writeln(Output, 'The number of points above or below two sigma(', 2 * actual_sigma : 6 : 2, ') is ', above2 : 3);
      tmess := 'average difference between N(D)-N(L) and <N(D)-N(L)>:';
      Print(5, 3, tmess);
      tmess := NumStr(averagediff, 6, 3);
      Print(15, 4, tmess);
      writeln(Output, 'The average difference between N(D)-N(L) and <N(D)-N(L)> is ', averagediff : 6 : 3);
      tmess := 'sigma of difference between N(D)-N(L) and <N(D)-N(L)>:';
      Print(5, 5, tmess);
      tmess := NumStr(averagesig, 6, 3);
      Print(15, 6, tmess);
      writeln(Output, 'The one sigma of the difference between ' + 'N(D)-N(L) and <N(D)-N(L)> is ', averagesig : 6 : 3);
   END;  { PROCEDURE StatisticsOutput }

   PROCEDURE Initialize_Ensemble (n, m: INTEGER; ib: markers; VAR ibe, ime: arrays; VAR maxi: INTEGER);

     { ***************************************************** }
     {                         }
     { PROCEDURE Initialize_Ensemble creates and initializes }
     { the ensemble                  }
     {                         }
     { input -                    }
     { n : the number of balls (or points)          }
     { m : the number of markers              }
     { ib : the initial descriptions of the balls      }
     {       -1 = white ball, 1 = red ball          }
     {                         }
     { output -                      }
     { ibe : a description of the balls for each member of   }
     {       the ensemble               }
     { ime : a description of the markers for each member of }
     {       the ensemble               }
     { maxi : the number of members in the ensemble       }
     {                         }
     { ***************************************************** }

      VAR
         i, i1, i2, i3, i4, i5, i6, i7, i8, i9: INTEGER;
   BEGIN
      maxi := Round(Fac(n) / (Fac(m) * Fac(n - m)));
      writeln(Output, ' number of members in the ensemble: ', maxi);
      { initialize ime }
      FOR i := 1 TO maxi DO
      BEGIN
         FOR i1 := 1 TO 9 DO
         BEGIN
            ime[i, i1] := 1;
         END;
      END;
      i := 0;
      FOR i1 := 1 TO (n - m + 1) DO
      BEGIN
         IF (m = 1) THEN
         BEGIN
            INC(i);
            ime[i, i1] := -1;
         END
         ELSE  { IF m >= 2 }
         BEGIN
            FOR i2 := (i1 + 1) TO (n - m + 2) DO
            BEGIN
               IF (m = 2) THEN
               BEGIN
                  INC(i);
                  ime[i, i1] := -1;
                  ime[i, i2] := -1;
               END
               ELSE  { Ii m >=i3 }
               BEGIN
                  FOR i3 := (i2 + 1) TO (n - m + 3) DO
                  BEGIN
                     IF (m = 3) THEN
                     BEGIN
                        INC(i);
                        ime[i, i1] := -1;
                        ime[i, i2] := -1;
                        ime[i, i3] := -1;
                     END
                     ELSE  { IF m >= 4 }
                     BEGIN
                        FOR i4 := (i3 + 1) TO (n - m + 4) DO
                        BEGIN
                           IF (m = 4) THEN
                           BEGIN
                              INC(i);
                              ime[i, i1] := -1;
                              ime[i, i2] := -1;
                              ime[i, i3] := -1;
                              ime[i, i4] := -1;
                           END
                           ELSE  { IF m >= 5 }
                           BEGIN
                              FOR i5 := (i4 + 1) TO (n - m + 5) DO
                              BEGIN
                                 IF (m = 5) THEN
                                 BEGIN
                                    INC(i);
                                    ime[i, i1] := -1;
                                    ime[i, i2] := -1;
                                    ime[i, i3] := -1;
                                    ime[i, i4] := -1;
                                    ime[i, i5] := -1;
                                 END
                                 ELSE  { IF m >= 6 }
                                 BEGIN
                                    FOR i6 := (i5 + 1) TO (n - m + 6) DO
                                    BEGIN
                                       IF (m = 6) THEN
                                       BEGIN
                                          INC(i);
                                          ime[i, i1] := -1;
                                          ime[i, i2] := -1;
                                          ime[i, i3] := -1;
                                          ime[i, i4] := -1;
                                          ime[i, i5] := -1;
                                          ime[1, i6] := -1;
                                       END
                                       ELSE  { IF m >=7 }
                                       BEGIN
                                          FOR i7 := (i6 + 1) TO (n - m + 7) DO
                                          BEGIN
                                             IF (m = 7) THEN
                                             BEGIN
                                                INC(i);
                                                ime[i, i1] := -1;
                                                ime[i, i2] := -1;
                                                ime[i, i3] := -1;
                                                ime[i, i4] := -1;
                                                ime[i, i5] := -1;
                                                ime[i, i6] := -1;
                                                ime[i, i7] := -1;
                                             END
                                             ELSE  { IF m = 8 }
                                             BEGIN
                                                FOR i8 := (i7 + 1) TO (n - m + 8) DO
                                                BEGIN
                                                   INC(i);
                                                   ime[i, i1] := -1;
                                                   ime[i, i2] := -1;
                                                   ime[i, i3] := -1;
                                                   ime[i, i4] := -1;
                                                   ime[i, i5] := -1;
                                                   ime[i, i6] := -1;
                                                   ime[i, i7] := -1;
                                                   ime[i, i8] := -1;
                                                END;  { FOR i8 := ... }
                                             END;  { IF m = 7 ... }
                                          END;  { FOR i7 := ... }
                                       END;  { IF m = 6 ... }
                                    END;  { FOR i6 := ... }
                                 END;  { IF m = 5 ... }
                              END; { FOR i5 ... }
                           END;  { IF (m>=5) ... }
                        END;  { FOR i4 ... }
                     END;  { IF (m>=4) ... }
                  END;  { FOR i3 ... }
               END;  { IF (m >=3) ... }
            END;  { FOR i2 ... }
         END;   { IF (m>=2) ... }
      END;  { FOR i1 ... }
      { initialize ibe }
      FOR i := 1 TO maxi DO
      BEGIN
         FOR i1 := 1 TO n DO
            ibe[i, i1] := ib[i1];
      END;
   END;  { PROCEDURE Initial_Ensemble }

   PROCEDURE Prepare_Plot_Small (tmax: INTEGER; VAR xb, yb, xbr, ybr: small_marker_array;
             VAR xm, ym: small_markers; VAR ixc, iyc, ir: INTEGER);

    { ************************************************************ }
    {                        }
    { Prepare two screens for the small Kac Ring:       }
    {                        }
    { Screen 1 - Kac Ring                }
    {                        }
    { Screen 2 - Plot of N(D) - N(L) and <N(D) - N(L)>as a function}
    {    of the number of moves             }
    {                        }
    { ************************************************************ }

      VAR
         ArcCoords: ArcCoordsType;
         ixl, iyl, stp, stb, deltan, delta2, i, i1: INTEGER;
         tmess: STRING;

   BEGIN
{      SetBkColor(Blue);}
      DefineViewPort(2, 0, 1, 0.055, 0.95);
      graphBackColor := blue;
      OpenViewPort(2);
      graphBackColor := darkGray;
      DefineViewPort(1, 0.1, 0.5, 0.3, 0.8);
      DefineScale(1, 0.0, 1.0, -1.0, 1.0);
      OpenViewPort(1);
      SelectViewPort(1);
      SetColor(white);
      PutLabel(top, 'KAC Ring');
     { determine center of view port in screen coordinates }
      map(0.5, 0.0, ixc, iyc);
     { determine radius of circle }
      map(0.1, 0.0, ixl, iyl);
      ir := Trunc(SQRT(SQR(ixc - ixl) + SQR(iyc - iyl)));
     { determine location of points and balls }
      stp := 90;
      stb := 110;
      deltan := 40;
      delta2 := 10;
      SetColor(white);
      Arc(ixc, iyc, 0, stp, ir);
      GetArcCoords(ArcCoords);
      WITH ArcCoords DO
      BEGIN
         xm[1] := Xend;
         ym[1] := Yend;
      END;
      FOR i := 1 TO 4 DO
      BEGIN
         Arc(ixc, iyc, 0, stb + (i - 2) * delta2, ir);
         GetArcCoords(ArcCoords);
         WITH ArcCoords DO
         BEGIN
            xb[i, 1] := Xend;
            yb[i, 1] := Yend;
         END;
      END;
      FOR i := 2 TO 9 DO
      BEGIN
         Arc(ixc, iyc, stp, stp + (i - 1) * deltan, ir);
         GetArcCoords(ArcCoords);
         WITH ArcCoords DO
         BEGIN
            xm[i] := Xend;
            ym[i] := Yend;
         END;
         FOR i1 := 1 TO 4 DO
         BEGIN
            Arc(ixc, iyc, stb, stb + (i - 1) * deltan + (i1 - 2) * delta2, ir);
            GetArcCoords(ArcCoords);
            WITH ArcCoords DO
            BEGIN
               xb[i1, i] := Xend;
               yb[i1, i] := Yend;
            END;
         END;  { FOR i1 := 1 TO 4 ... }
      END;  { FOR i := 2 TO 9 ... }
      xb[5, 1] := xb[4, 9];
      yb[5, 1] := yb[4, 9];
      FOR i := 2 TO 9 DO
      BEGIN
         xb[5, i] := xb[4, i - 1];
         yb[5, i] := yb[4, i - 1];
      END;
      FOR i := 1 TO 9 DO
      BEGIN
         xbr[1, i] := xb[3, i];
         xbr[2, i] := xb[2, i];
         xbr[3, i] := xb[1, i];
         xbr[4, i] := xb[5, i];
         xbr[5, i] := xb[4, i];
         ybr[1, i] := yb[3, i];
         ybr[2, i] := yb[2, i];
         ybr[3, i] := yb[1, i];
         ybr[4, i] := yb[5, i];
         ybr[5, i] := yb[4, i];
      END;
     { define plot view port }
      DefineViewPort(3, 0.5, 0.95, 0.3, 0.8);
      OpenViewPort(3);
      SetColor(LightRed);
      PutLabel(inside, 'N(D)-N(L)                 >');
      SetColor(yellow);
      PutLabel(inside, '              < N(D)-N(L) >');
      SetColor(white);
      PutLabel(inside, '          and              ');
      DefineViewPort(4, 0.6, 0.9, 0.4, 0.7);
      DefineScale(2, 0.0, 2 * tmax, -9.0, 9.0);
      OpenViewPort(4);
      SelectScale(2);
      SetColor(white);
      Axis(0.0, 0.0, 4.0, 3.0);
      PutLabel(bottom, 'number of moves');
      PutLabel(left, 'N(D)-N(L)');
      SetLineStyle(DashedLn, 0, 1);
      PlotLine(tmax, 9, tmax, -9);
      SetLineStyle(SolidLn, 0, 1);
      tmess := 'Forward';
      Print(3, -1, tmess);
      tmess := 'Reverse';
      Print(15, -1, tmess);
   END;  { PROCEDURE Prepare_Plot_Small }

   PROCEDURE Draw_Circle (icount, ixc, iyc, ir: INTEGER; im, ib: markers; xm, ym: small_markers; xb, yb: small_marker_array);

   { *********************************************************** }
   {                                                             }
   { PROCEDURE Draw_Circle draws the KAC Ring for the small ring }
   {                                                             }
   { *********************************************************** }

      CONST
         shape: 
         FillPatternType = ( $FF , $FF , $FF , $FF , $FF , $FF , $FF , $FF );
      VAR
         i, i1: INTEGER;
   BEGIN
      SelectViewPort(1);
      SetColor(darkGray);
      SetFillPattern(shape, darkGray);
      FOR i := 1 TO 9 DO
      BEGIN
         IF (icount <> 1) THEN
            FillEllipse(xb[icount - 1, i], yb[icount - 1, i], 10, 10)
         ELSE
            FillEllipse(xb[5, i], yb[5, i], 10, 10);
      END;
      SetColor(white);
      Circle(ixc, iyc, ir);
      FOR i := 1 TO 9 DO
      BEGIN
         IF ib[i] = -1 THEN
            SetFillPattern(shape, white)
         ELSE
            SetFillPattern(shape, blue);
         FillEllipse(xb[icount, i], yb[icount, i], 10, 10);
      END;  { FOR i := 1 TO 9 ... }
      FOR i := 1 TO 9 DO
      BEGIN
         IF im[i] = -1 THEN
            SetFillPattern(shape, red)
         ELSE
            SetFillPattern(shape, white);
         FillEllipse(xm[i], ym[i], 4, 4);
      END;  {FOR i := 1 TO 9 DO ... }
      SetColor(white);
   END;  { PROCEDURE Draw_Circle }

   PROCEDURE Statistics_Small (n, maxi: INTEGER; ib: markers; ibe: arrays; VAR actual, ensemble: REAL);

   { *************************************************************** }
   {                           }
   { PROCEDURE Statistics_Small computes N(D)-N(L) for the system and}
   {    <N(D)-N(L)> for the ensemble                }
   {                           }
   { *************************************************************** }

      VAR
         i, i1: INTEGER;
         amaxi, intermediate: REAL;
   BEGIN
      amaxi := maxi;
      actual := 0.0;
      FOR i := 1 TO n DO
         actual := actual + ib[i];
      ensemble := 0.0;
      FOR i := 1 TO maxi DO
      BEGIN
         intermediate := 0.0;
         FOR i1 := 1 TO n DO
            intermediate := intermediate + ibe[i, i1];
         ensemble := ensemble + intermediate;
      END;
      ensemble := ensemble / amaxi;
   END;  { PROCEDURE Statistics_Small }

   PROCEDURE Plot_Small (t: INTEGER; actualp, actual, ensemblep, ensemble: REAL);

     { ***************************************************** }
     {                                                       }
     { PROCEDURE Plot_Small plots the actual N(D)-N(L) - red }
     { line and the ensemble <N(D)-N(L)> - blue line         }
     {                                                       }
     { ***************************************************** }

   BEGIN
      SelectViewPort(4);
      SetColor(lightred);
      PlotLine(t - 1, actualp, t, actual);
      SetColor(yellow);
      PlotLine(t - 1, ensemblep, t, ensemble);
      SetColor(white);
   END;  { PROCEDURE Plot_Small }

BEGIN  { KAC_RING }
   CUPSinit;
   ABOUTPROGRAM;
   CUPSdone;
   n := 9;
   m := 2;
   nb := 2;
   tmax := 18;

   REPEAT   { UNTIL (quitf = true) }
      CUPSinit;
      ShowCursor;
      IF (first = true) THEN
      BEGIN
         ReadData;
         first := false;
         ASSIGN(Output, Kac_Output);
         REWRITE(Output);
      END
      ELSE
         ReadData1;
      IF (cancel = false) THEN
      BEGIN
         restart := false;
         ClearMuppetPort;
         MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
         am := m;
         an := n;
         mu := am / an;
         n0 := 2 * nb - n;
         Initialize_System(n, m, nb, im, ib);
         IF (n = 9) THEN
         BEGIN  { small ring }
            writeln(Output, ' A Small Ring ');
            writeln(Output, ' number of balls: ', n);
            writeln(Output, ' number of markers: ', m);
            writeln(Output, ' initial number of dark balls: ', nb);
            Initialize_Ensemble(n, m, ib, ibe, ime, maxi);
            Prepare_Plot_Small(tmax, xb, yb, xbr, ybr, xm, ym, ixc, iyc, ir);
            SetUpHotKeys(HotKeys);
            SetUpMyMenu;
            HotKeys.Display;
            MyMenuOut.Display;
            Statistics_Small(n, maxi, ib, ibe, actual, ensemble_average);
            writeln(Output, ' No. of Moves  N(D)-N(L)  <N(D)-N(L)>');
            writeln(Output, '      ', ' 0', '          ', actual : 2 : 0, '          ', ensemble_average : 2 : 0);
            actualp := actual;
            ensemble_averagep := ensemble_average;

            FOR t := 1 TO 2 * tmax + 1 DO
            BEGIN
               IF ((quitf = false) AND (restart = false)) THEN
               BEGIN
                  FOR icount := 1 TO 5 DO
                  BEGIN
                     IF icount <> 5 THEN
                     BEGIN
                        IF (t = tmax + 1) AND (icount = 1) AND (NOT STEP) THEN
                        BEGIN
                           PressKeyMessage;
                           MyMenuOut.Display;
                        END;
                        IF (t <= tmax) THEN
                        BEGIN
                           Draw_Circle(icount, ixc, iyc, ir, im, ib, xm, ym, xb, yb);
                           IF (t = 1) AND (icount = 1) AND (NOT STEP) THEN
                           BEGIN
                              PressKeyMessage;
                              MyMenuOut.Display;
                           END;
                        END
                        ELSE
                           Draw_Circle(icount, ixc, iyc, ir, im, ib, xm, ym, xbr, ybr);
                        Delay(deltat);
                     END;
                     IF (icount = 5) AND (t <> 2 * tmax + 1) THEN
                     BEGIN
                        IF (t <= tmax) THEN
                        BEGIN
                           Move_Ensemble(n, maxi, ime, ibe);
                           Move_Ring(im, n, ib);
                           IF STEP THEN
                           BEGIN
                              PressKeyMessage;
                              MyMenuOut.Display;
                           END
                        END
                        ELSE
                        BEGIN
                           Move_Ensemble_Back(n, maxi, ime, ibe);
                           Move_Ring_Back(im, n, ib);
                           IF STEP THEN
                           BEGIN
                              PressKeyMessage;
                              MyMenuOut.Display;
                           END
                        END;
                        IF (t <= tmax) THEN
                           Draw_Circle(icount, ixc, iyc, ir, im, ib, xm, ym, xb, yb)
                        ELSE
                           Draw_Circle(icount, ixc, iyc, ir, im, ib, xm, ym, xbr, ybr);
                        IF beepon THEN
                           Beep;
                        Statistics_Small(n, maxi, ib, ibe, actual, ensemble_average);
                        writeln(Output, '      ', t : 2, '          ', actual : 2 : 0, '          ', ensemble_average : 2 : 0);
                        Plot_Small(t, actualp, actual, ensemble_averagep, ensemble_average);
                        actualp := actual;
                        ensemble_averagep := ensemble_average;
                     END;   { IF icount = 5 }
                     CheckForEvents;
                     IF HotKeys.Pressed(key) THEN
                        HandleHotKeys(key, deltat, STEP);
                     MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
                  END;  { FOR icount := 1 TO 5 ... }
               END;  { IF (quitf = false) AND (restart = false)) ... }
            END;  {  FOR t := 1 TO 2*tmax ... }
            MyMenuOut.Display;
         END  { IF (n = 9) ... small ring }
         ELSE    { large ring }
         BEGIN
            writeln(Output);
            writeln(Output, ' A Large KAC Ring');
            writeln(Output, ' number of balls: 2001');
            writeln(Output, ' number of markers: ', m);
            writeln(Output, ' initial number of dark balls: ', nb);
            writeln(Output, ' number of moves: ', tmax);
            Compute_Average(n, ib, im, actual_average, actual_sigma, quitf, restart);
            IF ((quitf = false) AND (restart = false)) THEN
            BEGIN
               SetUpHotKeys2(HotKeys);
               HotKeys.Display;
               SetUpMyMenu;
               PreparePlot(actual_average, actual_sigma, an, tmax);
               t := 0;
               above := 0;
               above2 := 0;
               averagediff := 0.0;
               averagesig := 0.0;
               Statistics(ib, n, n0, t, mu, ensemble_averagep, actualp);
               averagediff := averagediff + (ensemble_averagep - actualp);
               averagesig := averagesig + SQR(ensemble_averagep - actualp);
               IF (ABS(actualp) > actual_sigma) THEN
                  Inc(above);
               IF (ABS(actualp) > 2.0 * actual_sigma) THEN
                  Inc(above2);
               writeln(Output, ' No. of Moves  N(D)-N(L)  <N(D)-N(L)>');
               FOR t := 1 TO tmax DO
               BEGIN
                  IF ((quitf = false) AND (restart = false)) THEN
                  BEGIN
                     Move_Ring(im, n, ib);
                     Statistics(ib, n, n0, t, mu, ensemble_average, actual);
                     writeln(Output, '    ', t : 4, '        ', actual : 5 : 0, '        ', ensemble_average : 5 : 0);
                     averagediff := averagediff + (ensemble_average - actual);
                     averagesig := averagesig + SQR(ensemble_average - actual);
                     SetColor(yellow);
                     PlotLine(t - 1.0, ensemble_averagep, t, ensemble_average);
                     SetColor(red);
                     PlotLine(t - 1.0, actualp, t, actual);
                     ensemble_averagep := ensemble_average;
                     actualp := actual;
                     IF (ABS(actual) > actual_sigma) THEN
                        Inc(above);
                     IF (ABS(actual) > 2.0 * actual_sigma) THEN
                        Inc(above2);
                     CheckForEvents;
                     IF HotKeys.Pressed(key) THEN
                        HandleHotKeys2(key);
                     MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
                  END;  {  IF (quitf = false) ... }
               END;  { FOR t := 1 TO tmax ... }
               averagediff := averagediff / (tmax + 1.0);
               averagesig := averagesig / (tmax + 1.0) - SQR(averagediff);
               averagesig := SQRT(averagesig);
               beep;
               StatisticsOutput(above, above2, averagediff, averagesig, actual_sigma);
            END; { IF ((quitf = false) ... }
         END;  { ELSE ( large ring ) }
         IF ((restart = false) AND (quitf = false)) THEN
         BEGIN
            MouseBorders(0, 0, GetMaxX - 15, GetMaxY);
            SetUpHotKeys2(HotKeys);
            HotKeys.Display;
            REPEAT
               CheckForEvents;
               IF HotKeys.Pressed(key) THEN HandleHotKeys2(key);
               IF (MyMenuOut.Activated) THEN
                  HandleMenu(restart, quitf);
            UNTIL ((restart = true) OR (quitf = true));
         END;   { IF ((restart ... }
         MouseBorders(0, 0, GetMaxX, GetMaxY);
      END;  { IF (quitf = false) }
      CUPSdone;
   UNTIL quitf = true;
   Close(Output);
END.  { PROGRAM KAC_RING }