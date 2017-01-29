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
   {**         Program PoisExp                     **}
   {**                                             **}
   {** This program uses statistical models to     **}
   {** describe the decay rate of a radioactive    **}
   {** substance.  The decay rate equals the       **}
   {** number of decays in a fixed time interval   **}
   {** (the observation time = fixed time interval)**}
   {**                                             **}
   {** The Poisson Distribution (P(i) = exp(-mu)*  **}
   {** (mu)^i/i!) describes the distribution of    **}
   {** the decay rates for an ensemble of samples  **}
   {** of radioactive material.                    **}
   {** (sample = a group of radioactive nuclei)    **}
   {** i = the number of decays in a time interval **}
   {** (i.e. the decay rate), mu = the mean number **}
   {** of events that occur in the time interval   **}
   {** = the probability of a decay (p) times the  **}
   {** no. of radioactive nuclei (n) in a sample.  **}
   {**                                             **}
   {** The Exponential Distribution (f(t) =        **}
   {** p*exp(-p*t)) describes the number of        **}
   {** parent (original) nuclei as a function of   **}
   {** time.                                       **}
   {** p = ln(2)/half-life.                        **}
   {**                                             **}
   {**       Written by L. B. Spornick             **}
   {**       Applied Physics Laboratory            **}
   {**       The Johns Hopkins University          **}
   {**                                             **}
   {**         June 13, 1992                       **}
   {**         ver 1.0, July, 1993                 **}
   {**         ver 1.1, Nov, 1993                  **}
   {**         ver 1.2, Jan, 1994                  **}
   {**         ver 1.3, May, 1994                  **}
   {**         ver 1.4, Apr, 1995                  **}
   {**                                             **}
   {*************************************************}
   {*************************************************}

PROGRAM POisExp;

USES
   Graph, CUPSMUPP, CUPS, CUPSgui, CUPSfunc, Crt;

TYPE
   statearray = ARRAY[1..1000] OF INTEGER;
   colorarray = ARRAY[1..2] OF WORD;
   plotyarray = ARRAY[1..300] OF LONGINT;
   countarray = ARRAY[1..31] OF INTEGER;
   poissonarray = ARRAY[1..31] OF INTEGER;
   decayarray = ARRAY[1..750] OF INTEGER;

CONST
   shape:
   FillPatternType = ( $FF , $FF , $FF , $FF , $FF , $FF , $FF , $FF );
   deltat:  INTEGER = 150;
   STEP:    BOOLEAN = false;
   beepon:  BOOLEAN = false;
   quitf:   BOOLEAN = false;
   restart: INTEGER = 0;
   first:   BOOLEAN = true;
   Poisson_Output: STRING = 'PoisExp.dat';

VAR
   dtype, error, i, j, maxy, nparticles, ntrials, ntrls : integer;
   numdecays, numground, ybottom: INTEGER;
   halflife, p, tmax, avg, mu, deltatime, deltatimem : real;
   dy, obst, obstime: REAL;
   count: countarray;
   xp, ypd, ybd, yed: countarray;
   yp: plotyarray;
   poisson, binomial, exponential: poissonarray;
   state, x, y: statearray;
   MyMenuOut: TMenu;
   HotKeys: THotKeys;
   key: BYTE;
   halfl: STRING;
   out: STRING[80];

PROCEDURE HELP1;
   VAR
      helpScr: HelpScrType;
BEGIN
   helpScr[1] := '                                                  ';
   helpScr[2] := '               The Poisson Distribution           ';
   helpScr[3] := '                                                  ';
   helpScr[4] := '    The distribution of the number of radioactive ';
   helpScr[5] := '    nuclei which decay in a fixed time interval   ';
   helpScr[6] := '    (the decay rate) is described by a            ';
   helpScr[7] := '    Poisson Distribution:                         ';
   helpScr[8] := '    P(i) = exp(-mu)*(mu^i)/i! where i is the      ';
   helpScr[9] := '    decay rate, mu = the probability of a decay   ';
   helpScr[10] := '    (p) times the number of nuclei in the sample, ';
   helpScr[11] := '    and p = ln(2)/half-life.                       ';
   helpScr[12] := '                                                  ';
   helpScr[13] := '    The matrix of circles in the upper screen     ';
   helpScr[14] := '    represents a sample of radioactive nuclei.    ';
   helpScr[15] := '    Their positions are irrelevant.  Their        ';
   helpScr[16] := '    colors indicate their states: Parent (Not     ';
   helpScr[17] := '    Decayed) - Dark Gray, Daughter (Decayed)      ';
   helpScr[18] := '    - White.                                      ';
   helpScr[19] := '                                                  ';
   helpScr[20] := '    The graph in the bottom screen, plots the     ';
   helpScr[21] := '    number of samples (= a group of radioactive   ';
   helpScr[22] := '    nuclei) with a particular decay rate as a     ';
   helpScr[23] := '    function of the decay rate (white bars). The  ';
   helpScr[24] := '    corresponding Poisson Distribution is also    ';
   helpScr[25] := '    plotted (light red outline bars).             ';
   Help(helpScr);
END;   {  PROCEDURE HELP1)  }


PROCEDURE HELP2;
   VAR
      helpScr: HelpScrType;
BEGIN
   helpScr[1] := '                                                  ';
   helpScr[2] := '            The Exponential Distribution          ';
   helpScr[3] := '                                                  ';
   helpScr[4] := '    The Exponential Distribution describes the    ';
   helpScr[5] := '    distribution of the number of radioactive     ';
   helpScr[6] := '    nuclei which have not decayed as a function   ';
   helpScr[7] := '    of time:                                      ';
   helpScr[8] := '              f(t) = p*exp(-p*t)                  ';
   helpScr[9] := '    where p = ln(2)/half-life.                     ';
   helpScr[10] := '                                                  ';
   helpScr[11] := '    The matrix of circles in the upper screen     ';
   helpScr[12] := '    represents a group of radioactive nuclei.     ';
   helpScr[13] := '    Their positions  are irrelevant.  Their       ';
   helpScr[14] := '    colors indicate their states: Parent (Not     ';
   helpScr[15] := '    Decayed) - Dark Gray, Decaying - White,       ';
   helpScr[16] := '    Daughter (Decayed) - Light Red.               ';
   helpScr[17] := '                                                  ';
   helpScr[18] := '    The graph in the lower screen plots the       ';
   helpScr[19] := '    distribution of the number of Parent nuclei   ';
   helpScr[20] := '    as a function of time (white) and the         ';
   helpScr[21] := '    corresponding Exponential Distribution        ';
   helpScr[22] := '    (light red).                                  ';
   helpScr[23] := '                                                  ';
   helpScr[24] := '                                                  ';
   helpScr[25] := '                                                  ';
   HELP(helpScr);
END;


PROCEDURE ABOUTPROGRAM;
   VAR
      helpScr: HelpScrType;
BEGIN
   helpScr[1] := '                                                  ';
   helpScr[2] := '    The Poisson and Exponential Distributions     ';
   helpScr[3] := '                                                  ';
   helpScr[4] := '                      by                          ';
   helpScr[5] := '                                                  ';
   helpScr[6] := '                 L.B. Spornick                    ';
   helpScr[7] := '           Applied Physics Laboratory             ';
   helpScr[8] := '          The Johns Hopkins University            ';
   helpScr[9] := '                                                  ';
   helpScr[10] := '                   April 1995                    ';
   helpScr[11] := '               Copyright (c) 1995                 ';
   helpScr[12] := '                                                  ';
   helpScr[13] := '   This program models an ensemble of samples of  ';
   helpScr[14] := '   radioactive material.  The program             ';
   helpScr[15] := '   demonstrates the Poisson Distribution and the  ';
   helpScr[16] := '   Exponential Distribution.                      ';
   helpScr[17] := '                                                  ';
   helpScr[18] := '                                                  ';
   helpScr[19] := '                                                  ';
   helpScr[20] := '                                                  ';
   helpScr[21] := '                                                  ';
   helpScr[22] := '                                                  ';
   helpScr[23] := '                                                  ';
   helpScr[24] := '                                                  ';
   helpScr[25] := '                                                  ';
   HELP(helpScr);
END;


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


FUNCTION PFactor (n, i: INTEGER): REAL;
   VAR
      s: REAL;
      i1: LONGINT;
BEGIN
   s := 1;
   IF (i = 1) THEN
      s := n
   ELSE IF i > 1 THEN
   BEGIN
      s := n;
      FOR i1 := 1 TO i - 1 DO
         s := s * (n - i1);
   END;
   PFactor := s;
END;   { FUNCTION PFactor }


PROCEDURE SetUpMyMenu;
BEGIN
   MyMenuOut.Init;
   WITH MyMenuOut DO
   BEGIN
      column(1, 'File');
      row(1, 1, 'About Program');
      row(1, 2, 'About CUPS');
      row(1, 3, 'Configuration');
      row(1, 4, '-------------');
      row(1, 5, 'Exit Program');
      column(2, 'Restart');
      row(2, 1, 'Exponential');
      row(2, 2, 'Poisson');
      rowactivate(1,4,false);
      autochecking(2,3-dtype);
   END;
   MyMenuOut.Display;
END;  { PROCEDURE SetUpMyMenu }


PROCEDURE HandleMenu (VAR quitf: BOOLEAN; VAR restart: INTEGER);
BEGIN
   WITH MyMenuOut DO
    CASE colChosen OF
     1: CASE rowChosen OF
         1: ABOUTPROGRAM;
         2: AboutCUPS;
         3: Configuration;
         5: quitf := true;
        END; { CASE }
     2: CASE rowChosen OF
         1: restart := 1;
         2: restart := 2;
        END;  { CASE }
    END;  { CASE }
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


PROCEDURE HandleHotKeys (key: BYTE; dtype: LONGINT;
          VAR deltat: INTEGER; VAR STEP: BOOLEAN);
BEGIN
   CASE key OF
      1:
         IF (dtype = 1) THEN
            Help1
         ELSE
            Help2;
      2:
         STEP := NOT STEP;  { F3 - Step }
      3:
      BEGIN      { F5 - Slower }
         deltat := round(1.3*deltat);
         If (deltat = 0) then deltat := 15;
         IF (deltat > 1500) THEN begin
            deltat := 1500;
            beep;
         end;
      END;
      4:
      BEGIN      { F6 - Faster }
         deltat := round(deltat/1.3);
         IF (deltat < 15) THEN begin
            deltat := 0;
            beep;
         end;
      END;
      5:
      BEGIN    { F10 - Menu }
         step := false;
         MouseBorders(0, 0, GetMaxX, GetMaxY);
         IF MyMenuOut.chosen THEN
            HandleMenu(quitf, restart);
      END;
   END;  { CASE }
END;  { PROCEDURE HandleHotKeys }


PROCEDURE ReadData;
   VAR
      InitScreen: TInputScreen;
      error: INTEGER;
BEGIN
   WITH InitScreen DO
   BEGIN
      init;
      DefineInputPort(0.15, 0.85, 0.05, 0.95);
      LoadLine('   The Poisson and Exponential Distribution        ');
      LoadLine('                                                    ');
      LoadLine('  Select Distribution Type:');
      LoadLine('   #1 Poisson Distribution');
      LoadLine('   #1 Exponential Distribution');
      LoadLine('                                                    ');
      LoadLine('  Sample half-life:    {     } (.01 - 999 years)       ');
      LoadLine('                                                    ');
      LoadLine('  Poisson Distribution Initial Conditions:          ');
      LoadLine('    Observation Time : {     } (1 - 60 minutes)      ');
      LoadLine('    Number of Samples: {     } (200 - 1000)     ');
      LoadLine('                                                    ');
      LoadLine('  Exponential Distribution Initial Conditions:      ');
      LoadLine('    Observation Time:  {     } (.001 - 5 half-lives)  ');
      LoadLine('    Number of Evenly Spaced Observations:           ');
      LoadLine('                       {     } (10 or 30)           ');
      LoadLine('                                                    ');
      LoadLine('  Output File Name: "                              "  ');
      LoadLine('                                                    ');
      LoadLine('             [  Ok  ]    [Cancel]                    ');
      SetRadioButton('1', dtype);
      SetNumber(3, halflife);
      SetNumberLimits(3, 0.01, 999);
      SetNumber(4, deltatimem);
      SetNumberLimits(4, 1.0, 60.0);
      SetNumber(5, ntrials);
      SetNumberLimits(5, 200, 1000);
      SetNumber(6, obstime);
      SetNumberLimits(6, 0.001, 5.0);
      SetNumber(7, ntrls);
      SetString(8, Poisson_Output);
      REPEAT
         Accept;
         error := 0;
         dtype := GetRadioButton('1');
         halflife := GetNumber(3);
         IF (dtype = 1) THEN
         BEGIN
            deltatimem := GetNumber(4);
            deltatime := deltatimem / (512640.0);  { convert to years }
            ntrials := Trunc(GetNumber(5));
         END
         ELSE
         BEGIN
            obstime := GetNumber(6);
            ntrls := Trunc(GetNumber(7));
            obst := obstime * halflife / ntrls;
            IF ((ntrls <> 10) AND (ntrls <> 30)) THEN
            BEGIN
               Announce('Error in Number of Observations');
               error := 1;
            END
            ELSE
               error := 0;
         END;
      UNTIL (error = 0);
      Poisson_Output := GetString(8);
      IF (canceled) THEN
         quitf := true;
      DONE;
   END;  { WITH InitScreen DO  }
END;  { PROCEDURE ReadData }


PROCEDURE ReadData1;
   VAR
      InitScreen: TInputScreen;
      error: INTEGER;
BEGIN
   WITH InitScreen DO
   BEGIN
      init;
      DefineInputPort(0.15, 0.85, 0.25, 0.75);
      LoadLine('            The Exponential Distribution            ');
      LoadLine('                                                    ');
      LoadLine('    Sample half-life: (.01 - 999 years)     {     }        ');
      LoadLine('                                                    ');
      LoadLine('    Observation Time: (.001 - 5 half-lives) {     }  ');
      LoadLine('                                                    ');
      LoadLine('    Number of Evenly Spaced Observations:           ');
      LoadLine('    (10 or 30)  {    }                                ');
      LoadLine('                                                    ');
      LoadLine('             [  Ok  ]    [Cancel]                    ');
      SetNumber(1, halflife);
      SetNumberLimits(1, 0.01, 999);
      SetNumber(2, obstime);
      SetNumberLimits(2, 0.001, 5.0);
      SetNumber(3, ntrls);
      REPEAT
         Accept;
         error := 0;
         halflife := GetNumber(1);
         obstime := GetNumber(2);
         ntrls := Trunc(GetNumber(3));
         obst := obstime * halflife / ntrls;
         IF ((ntrls <> 10) AND (ntrls <> 30)) THEN
         BEGIN
            Announce('Error in Number of Observations');
            error := 1;
         END
         ELSE
            error := 0;
      UNTIL (error = 0);
      IF (not canceled) THEN dType:=2;
      DONE;
   END;  { WITH InitScreen DO  }
END;  { PROCEDURE ReadData1 }


PROCEDURE ReadData2;
   VAR
      InitScreen: TInputScreen;
      error: INTEGER;
BEGIN
   WITH InitScreen DO
   BEGIN
      init;
      DefineInputPort(0.15, 0.85, 0.25, 0.75);
      LoadLine('             The Poisson Distribution               ');
      LoadLine('                                                    ');
      LoadLine('  Sample half-life (.01 - 999 years)  {     }        ');
      LoadLine('                                                    ');
      LoadLine('  Observation Time: (1 - 60 minutes)  {     }       ');
      LoadLine('                                                    ');
      LoadLine('  Number of Samples: (200 - 1000)     {     }         ');
      LoadLine('                                                    ');
      LoadLine('             [  Ok  ]    [Cancel]                    ');
      SetNumber(1, halflife);
      SetNumberLimits(1, 0.01, 999);
      SetNumber(2, deltatimem);
      SetNumberLimits(2, 1.0, 60.0);
      SetNumber(3, ntrials);
      SetNumberLimits(3, 200, 1000);
      Accept;
      error := 0;
      halflife := GetNumber(1);
      deltatimem := GetNumber(2);
      deltatime := deltatimem / (512640.0);  { convert to years }
      ntrials := Trunc(GetNumber(3));
      IF (not canceled) THEN dType:=1;
      DONE;
   END;  { WITH InitScreen DO  }
END;  { PROCEDURE ReadData2 }


PROCEDURE InitializeRadioactive (dtype, ntrials: INTEGER; p, deltatime,
          obst: REAL; VAR state, x, y: statearray;
          VAR nparticles, maxy: INTEGER; VAR mu: REAL;
          VAR poisson, binomial, exponential: poissonarray;
          shape: FillPatternType);

   VAR
      dx, dy, xborder, xleft, xright, xwidth, x1, x2, nx, ny: integer;
      yborder, ybottom, ymiddle, ylength, ytop: INTEGER;
      dylength, dnparticles: REAL;
      i, i1, j: LONGINT;
      s : string;
BEGIN
   DefineViewPort(1, 0.0, 0.95, 0.51, 0.89);
   DefineScale(1, 0.0, 0.95, 0.0, 0.4);
   OpenViewPort(1);
   SelectScale(1);
   SelectViewPort(1);
   SetColor(white);
   map(0.0, 0.0, xleft, ybottom);
   map(0.95, 0.3, xright, ytop);
   SetFillPattern(shape, blue);
   xwidth := xright - xleft;
   ylength := ybottom - ytop;
   nx := xwidth DIV 12;
   ny := ylength DIV 9;
   xborder := 9;
   yborder := 12;
   nparticles := nx * ny;
   s :=' initial number of Parent (Not Decayed) nuclei in a sample: ';
   writeln(Output, s, nparticles);
   SetColor(white);
   SetFillPattern(shape, DarkGray);
   { determine the (x,y) coordinates of the atoms }
   { initialize thier states to 0                 }
   FOR i := 1 TO ny DO
   BEGIN
      FOR j := 1 TO nx DO
      BEGIN
         i1 := (i - 1) * nx + j;
         x[i1] := xleft + xborder + (j - 1) * 12;
         y[i1] := ybottom - yborder - (i - 1) * 11;
         FillEllipse(x[i1], y[i1], 3, 3);
         state[i1] := 0;
      END;  { FOR j := 1 TO nx DO }
   END;  { FOR i := 1 TO ny DO }
   mu := p * nparticles;
   maxy := 0;
   IF (dtype = 1) THEN
   BEGIN
      s :=' the probability of a decay * the number of nuclei in a sample (mu): ';
      writeln(Output, s, mu : 10 : 7);
      writeln(Output);
      FOR i := 1 TO 30 DO
      BEGIN
         poisson[i] := Round(ntrials * exp(-mu) * pwr(mu, i - 1) /
                       factor(i - 1));
         IF (maxy < poisson[i]) THEN
            maxy := poisson[i];
         binomial[i] := Round(ntrials * pfactor(nparticles, i - 1) *
                        pwr(p, i - 1) * pwr((1.0 - p),
                        (nparticles - i - 1)) / factor(i - 1));
      END;  { FOR i := 1 TO 30 DO }
      maxy := Round(1.3 * maxy);
   END  { IF (dtype = 1) THEN }
   ELSE
      FOR i := 1 TO ntrials + 1 DO
         exponential[i] := Round(nparticles * exp(-p * (i - 1.0) * obst));
END;  { PROCEDURE InitializeRadioactive }


PROCEDURE InitializePlot (poisson, binomial: poissonarray; maxy: INTEGER;
 VAR ybottom: INTEGER; VAR dy: REAL; VAR xp, ypd, ybd: countarray;
 VAR yp: plotyarray);

   VAR
      xleft, xright, xwidth, nx, ny, ymiddle, ylength, ytop: INTEGER;
      i, i1, j: LONGINT;
      TextInfo: TextSettingsType;
      s : string;
BEGIN
   DefineViewPort(2, 0.0, 0.95, 0.1, 0.5);
   DefineScale(2, 0.0, 0.95, 0.0, 0.4);
   OpenViewPort(2);
   SelectScale(2);
   SelectViewPort(2);
   SetColor(white);
   map(0.08, 0.08, xleft, ybottom);
   map(0.95, 0.5, xright, ytop);
   xwidth := xright - xleft;
   ylength := ybottom - ytop;
   dy := (ylength - 4) / maxy;
   Line(xleft, ybottom, xleft, ytop);
   Line(xleft, ytop, xright, ytop);
   Line(xleft, ybottom, xright, ybottom);
   Line(xright, ybottom, xright, ytop);
   SetFillPattern(shape, blue);
   FloodFill(xleft + 2, ybottom - 3, white);
   nx := xwidth DIV 30;
   ny := ylength DIV (ylength - 4);
   SetColor(white);
   { determine the (x,y) coordinates of the bars }
   FOR i := 1 TO 30 DO
      xp[i] := xleft + nx DIV 2 + (i - 1) * nx;
   FOR i := 1 TO (ylength - 2) DO
      yp[i] := ybottom - (i - 1) * ny;
   FOR i := 1 TO 30 DO
   BEGIN
     { determine the y coordinates for the Poisson Distribution }
      i1 := Round(dy * poisson[i]);
      IF (i1 > 0) THEN
         ypd[i] := yp[i1]
      ELSE
         ypd[i] := 0;
     { draw Poisson Distribution }
      SetColor(LightRed);
      IF (ypd[i] > 0) THEN
      BEGIN
         Line(xp[i] - 4, ypd[i], xp[i] + 4, ypd[i]);
         Line(xp[i] - 4, ypd[i], xp[i] - 4, ybottom);
         Line(xp[i] + 4, ypd[i], xp[i] + 4, ybottom);
      END;
   END;
   SetColor(white);
   { annotate the scales }
   FOR i := 1 TO 10 DO
      OutTextXY(xp[i] - 2, ybottom + 10, NumStr(i - 1, 1, 0));
   FOR i := 11 TO 30 DO if odd(i) then
      OutTextXY(xp[i] - 2, ybottom + 10, NumStr(i - 1, 2, 0));
   i1 := Round(dy * maxy / 4);
   OutTextXY(xleft - 30, yp[i1], NumStr(maxy / 4, 3, 0)+'-');
   i1 := Round(dy * maxy / 2);
   OutTextXY(xleft - 30, yp[i1], NumStr(maxy / 2, 3, 0)+'-');
   i1 := Round(dy * 3 *maxy / 4);
   OutTextXY(xleft - 30, yp[i1], NumStr(maxy*3/4, 3, 0)+'-');
   i1 := Round(dy * maxy);
   OutTextXY(xleft - 30, yp[i1], NumStr(maxy, 3, 0)+'-');
   OutTextXY(xp[8], yp[ylength - 5], 'The Distribution of Decay Rates');
   GetTextSettings(TextInfo);
   SetTextStyle(TextInfo.Font, 1, 1);
   OutTextXY(xleft - 38, ytop + 80, 'Number of Samples');
   SetTextStyle(TextInfo.Font, 0, 1);
   s :='Number of Decays per Observation Time (Decay Rate)';
   OutTextXY(xp[4], ybottom + 25, s);
END;  { PROCEDURE InitializePlot }


PROCEDURE InitializePlot2 (nparticles, ntrials: INTEGER; obst:
 REAL; exponential: poissonarray; VAR ybottom: INTEGER; VAR dy: REAL;
 VAR xp, yed: countarray; VAR yp: plotyarray);

   VAR
      xleft, xright, xwidth, nx, ny, ymiddle, ylength, ytop: INTEGER;
      i, i1, j: LONGINT;
      TextInfo: TextSettingsType;
      s : string;
BEGIN
   DefineViewPort(2, 0.0, 0.95, 0.1, 0.5);
   DefineScale(2, 0.0, 0.95, 0.0, 0.5);
   OpenViewPort(2);
   SelectScale(2);
   SelectViewPort(2);
   SetColor(white);
   map(0.08, 0.08, xleft, ybottom);
   map(0.95, 0.5, xright, ytop);
   xwidth := xright - xleft;
   ylength := ybottom - ytop;
   dy := (ylength - 12.0) / nparticles;
   Line(xleft, ybottom, xleft, ytop);
   Line(xleft, ytop, xright, ytop);
   Line(xleft, ybottom, xright, ybottom);
   Line(xright, ybottom, xright, ytop);
   SetFillPattern(shape, blue);
   FloodFill(xleft + 2, ybottom - 3, white);
   nx := xwidth DIV (ntrials + 1);
   SetColor(white);
   { determine the (x,y) coordinates of the bars }
   FOR i := 1 TO (ntrials + 1) DO
      xp[i] := xleft + nx DIV 2 + (i - 1) * nx;
   FOR i := 1 TO (ylength) DO
      yp[ylength - i + 1] := ytop + (i - 1);
   FOR i := 1 TO (ntrials + 1) DO
   BEGIN
     { determine the y coordinates for the Exponential Distribution }
      i1 := Round(dy * exponential[i]);
      IF (i1 > 0) THEN
         yed[i] := yp[i1]
      ELSE
         yed[i] := 0;
     { draw Exponential Distribution }
      SetColor(LightRed);
      IF (yed[i] > 0) THEN
      BEGIN
         Line(xp[i] - 4, yed[i], xp[i] + 4, yed[i]);
         Line(xp[i] - 4, yed[i], xp[i] - 4, ybottom);
         Line(xp[i] + 4, yed[i], xp[i] + 4, ybottom);
      END;
   END;
   SetColor(white);
   { annotate the scales }
   IF ntrials = 10 THEN
   BEGIN
      IF (10.0 * obst < 1.0) THEN
      BEGIN
         OutTextXY(xp[1] - 20, ybottom + 10, '0.000');
         OutTextXY(xp[6] - 20, ybottom + 10, NumStr(5 * obst, 4, 3));
         OutTextXY(xp[11] - 20, ybottom + 10, NumStr(10 * obst, 4, 3));
      END
      ELSE IF (10.0 * obst < 1.0) THEN
       FOR i := 1 TO 11 DO
        OutTextXY(xp[i] - 10, ybottom + 10, NumStr((i - 1) * obst, 3, 2))
      ELSE
       FOR i := 1 TO 11 DO
        OutTextXY(xp[i] - 2, ybottom + 10, NumStr((i - 1) * obst, 3, 1))
   END
   ELSE
   BEGIN
      OutTextXY(xp[1] - 2, ybottom + 10, '0');
      IF (obst * 15.0 > 1) THEN
      BEGIN
         OutTextXY(xp[16] - 2, ybottom + 10, NumStr(15.0 * obst, 3, 2));
         OutTextXY(xp[31] - 2, ybottom + 10, NumStr(30.0 * obst, 3, 2));
      END
      ELSE
      BEGIN
         OutTextXY(xp[16] - 20, ybottom + 10, NumStr(15.0 * obst, 6, 5));
         OutTextXY(xp[31] - 20, ybottom + 10, NumStr(30.0 * obst, 6, 5));
      END;
   END;
   i1 := Round(dy * nparticles / 2);
   OutTextXY(xleft - 30, yp[i1], NumStr(nparticles / 2, 3, 0));
   i1 := Round(dy * nparticles);
   OutTextXY(xleft - 30, yp[i1], NumStr(nparticles, 3, 0));
   s := 'No. of Parent (Not Decayed) Nuclei as a Function of Time';
   OutTextXY(xleft + 60, yp[ylength - 5], s);
   GetTextSettings(TextInfo);
   SetTextStyle(TextInfo.Font, 1, 1);
   OutTextXY(xleft - 38, yp[i1], 'No. of Parent Nuclei');
   SetTextStyle(TextInfo.Font, 0, 1);
   OutTextXY(xleft + xwidth DIV 2 - 25, ybottom + 20, 'Years');
END;  { PROCEDURE InitializePlot2 }


PROCEDURE TimeEvolution (nparticles: INTEGER; p, mu: REAL;
 shape: FillPatternType; x, y: statearray; VAR quitf: BOOLEAN;
 VAR restart, deltat: INTEGER; VAR numdecays: INTEGER;
 VAR count: countarray);
   VAR
      i, i1, j: INTEGER;
      r: REAL;
      xd, yd: countarray;
      tex: CHAR;
BEGIN
   SelectViewPort(1);
   numdecays := 0;
   CheckForEvents;
   IF HotKeys.Pressed(key) THEN
      HandleHotKeys(key, 1, deltat, STEP);
   MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
   FOR i := 1 TO nparticles DO
   BEGIN
      IF ((quitf = false) AND (restart = 0)) THEN
      BEGIN
     { test each atom for decay }
         r := Random;
         IF (r < p) THEN
         BEGIN
            Inc(numdecays);
            SetColor(white);
            SetFillPattern(shape, white);
            FillEllipse(x[i], y[i], 3, 3);
            Delay(deltat);
            xd[numdecays] := x[i];
            yd[numdecays] := y[i];
            IF (beepon) THEN
               Beep;
         END;  { IF (r < p) THEN }
      END;  { IF ((quitf ... }
   END; { FOR i := 1 TO nparticles DO }
   IF ((quitf = false) AND (restart = 0)) THEN
   BEGIN
      Inc(count[numdecays + 1]);
      IF (STEP = TRUE) THEN
      BEGIN
         PressKeyMessage;
         SetUpMyMenu;
      END;
      IF (numdecays > 0) AND (numdecays < 30) THEN
      BEGIN
         FOR i := 1 TO numdecays DO
         BEGIN
            SetFillPattern(shape, darkgray);
            FillEllipse(xd[i], yd[i], 3, 3);
         END;  { For i := 1 TO numdecays DO }
      END;  { IF (numdecays > 0 ... }
   END;  { IF ((quitf = false) ... }
END;  { PROCEDURE TimeEvolution }


PROCEDURE TimeEvolution2 (nparticles: INTEGER; p, obst: REAL;
 shape: FillPatternType; VAR state: statearray; x, y: statearray;
 VAR quitf: BOOLEAN; VAR restart, deltat: INTEGER; VAR numground: INTEGER);
   VAR
      i, i1, j, numdecays: INTEGER;
      r: REAL;
      xd, yd: decayarray;
      menuf: BOOLEAN;
BEGIN
   SelectViewPort(1);
   numdecays := 0;
   menuf := false;
   IF (numground > 0) THEN
   BEGIN
      FOR i := 1 TO nparticles DO
      BEGIN
         IF ((quitf = false) AND (restart = 0)) THEN
         BEGIN
            CheckForEvents;
            IF HotKeys.Pressed(key) THEN
               HandleHotKeys(key, 2, deltat, STEP);
            MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
            IF (state[i] = 0) THEN
            BEGIN
         { test each atom for decay }
               r := Random;
               IF (r < p * obst) THEN
               BEGIN
                  Inc(numdecays);
                  SetColor(white);
                  SetFillPattern(shape, white);
                  FillEllipse(x[i], y[i], 3, 3);
                  xd[numdecays] := x[i];
                  yd[numdecays] := y[i];
                  state[i] := 1;
                  IF (beepon) THEN
                     Beep;
               END;  { IF (r < p) THEN }
            END; { IF (state[i] = 0) THEN }
         END;  {IF ((quitf ... }
      END; { FOR i := 1 TO nparticles DO }
      numground := numground - numdecays;
      FOR i := 1 TO numdecays DO
      BEGIN
         SetColor(lightRed);
         SetFillPattern(shape, LightRed);
         FillEllipse(xd[i], yd[i], 3, 3);
      END;  { FOR i := 1 TO numdecays DO }
   END;  { IF (numground > 0) THEN }
   SetColor(white);
   Delay(3 * deltat);
   IF (STEP = TRUE) THEN
   BEGIN
      PressKeyMessage;
      SetUpMyMenu;
   END;
END;  { PROCEDURE TimeEvolution2 }


PROCEDURE PlotDistribution (xp, ypd: countarray; yp: plotyarray;
 dy: REAL; numdecays, ybottom: INTEGER; count: countarray);
   VAR
      i: INTEGER;
      x , y : integer;
BEGIN
   SelectViewPort(2);
   { add new point to the graph }
   i := ROUND(dy * count[numdecays + 1] + 1);
   x := xp[numdecays + 1];
   y := ypd[numdecays + 1];
   Line( x - 3, yp[i], x + 3, yp[i]);
   Line( x - 3, yp[i], x - 3, ybottom);
   Line( x + 3, yp[i], x + 3, ybottom);
   { redraw Poisson Distribution }
   SetColor(LightRed);
   IF (y > 0) THEN
   BEGIN
      Line(x-4, y, x+4, y);
      Line(x-4, y, x-4, ybottom);
      Line(x+4, y, x+4, ybottom);
   END;
   SetColor(white);
END;  { PROCEDURE PlotDistribution }


PROCEDURE CountDown (i, maxy: INTEGER; dy: REAL; xp: countarray;
          yp: plotyarray);
BEGIN
   SelectViewPort(2);
   SetColor(white);
   line(xp[20], yp[Round(dy *maxy) -70], xp[27], yp[Round(dy *maxy) -70]);
   line(xp[20], yp[Round(dy *maxy)-100], xp[27], yp[Round(dy *maxy) -100]);
   line(xp[20], yp[Round(dy *maxy) -70], xp[20], yp[Round(dy *maxy) -100]);
   line(xp[27], yp[Round(dy *maxy) -70], xp[27], yp[Round(dy *maxy) -100]);
   SetFillPattern(shape, 9);
   IF (i = 1) THEN
      FloodFill(xp[20] + 1, yp[Round(dy * maxy) - 70] + 1, white);
   SetColor(9);
   IF (i > 1) THEN
      OutTextXY(xp[20] + 5, yp[Round(dy * maxy) - 80],
                '           ' + NumStr(i - 1, 3, 0));
   SetColor(white);
   OutTextXY(xp[20] + 5, yp[Round(dy * maxy) - 80],
             'Sample No. ' + NumStr(i, 3, 0));
END; { PROCEDURE CountDown }


PROCEDURE PlotDistribution2 (i1: INTEGER; xp, yed: countarray;
          yp: plotyarray; dy: REAL; numground, ybottom: INTEGER);
   VAR
      i: INTEGER;
BEGIN
   SelectViewPort(2);
   { add new point to the graph }
   i := Round(dy * numground);
   IF (i > 0) THEN
   BEGIN
      Line(xp[i1] - 3, yp[i], xp[i1] + 3, yp[i]);
      Line(xp[i1] - 3, yp[i], xp[i1] - 3, ybottom);
      Line(xp[i1] + 3, yp[i], xp[i1] + 3, ybottom);
     { redraw Poisson Distribution }
      SetColor(LightRed);
      IF (yed[i1] > 0) THEN
      BEGIN
         Line(xp[i1] - 4, yed[i1], xp[i1] + 4, yed[i1]);
         Line(xp[i1] - 4, yed[i1], xp[i1] - 4, ybottom);
         Line(xp[i1] + 4, yed[i1], xp[i1] + 4, ybottom);
      END;
   END;
   SetColor(white);
END;  { PROCEDURE PlotDistribution }


BEGIN  { PROGRAM POISSON_DIST }
   CUPSinit;
   ABOUTPROGRAM;
   CUPSdone;
   CUPSinit;
   Randomize;
   dtype := 1;
   halflife := 0.1;
   deltatimem := 30.0;
   ntrials := 200;
   obstime := 1.0;
   ntrls := 10;

   REPEAT  { UNTIL quitf = true }
      ClearMuppetPort;
      IF (first = true) THEN
      BEGIN
         first := false;
         readdata;
         ASSIGN(Output, Poisson_Output);
         REWRITE(Output);
      END
      ELSE IF (restart = 1) THEN
      BEGIN
         restart := 0;
         readdata1;
      END
      ELSE
      BEGIN
         restart := 0;
         readdata2;
      END;
      MouseBorders(0, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
      IF (quitf = false) THEN
      BEGIN
         ClearMuppetPort;
       { Distribution of Radioactive Atoms }
         SetUpHotKeys(HotKeys);
         HotKeys.Display;
         SetUpMyMenu;
         IF (dtype = 1) THEN
         BEGIN
            writeln(Output, ' The Poisson Distribution ');
            p := deltatime * ln(2.0) / halflife;
            writeln(Output, ' halflife: ', halflife : 6 : 2, ' years');
            writeln(Output, ' the probability that a decay occurs: ',p:10:7);
            writeln(Output, ' the observation time: ', deltatime * 512640.0, ' minutes');
            writeln(Output, ' the observation time: ', deltatime, ' years');
            writeln(Output, ' number of samples in the ensemble: ', ntrials);
         END
         ELSE
         BEGIN
            writeln(Output, ' The Exponential Distribution ');
            writeln(Output, ' the number of observations: ', ntrials);
            p := ln(2) / halflife;
            writeln(Output, ' halflife: ', halflife : 10 : 5, ' years ');
            writeln(Output, ' the decay constant (lamda): ',p:10:5,'/years');
            tmax := halflife * obstime;
            writeln(Output, ' the amount of time that the sample is observed: ',
                    tmax : 10 : 5, ' years');
            deltatime := tmax / ntrials;
            writeln(Output, ' the amount of time between observations: ',
                    deltatime : 8 : 3, ' years');
         END;
         IF (dtype = 1) THEN
            InitializeRadioactive(dtype, ntrials, p, deltatime, obst,
             state, x, y, nparticles, maxy, mu, poisson, binomial,
             exponential, shape)
         ELSE
            InitializeRadioactive(dtype, ntrls, p, deltatime, obst,
             state, x, y, nparticles, maxy, mu, poisson, binomial,
             exponential, shape);
         IF (halflife < 0.1) THEN
            halfl := NumStr(halflife, 3, 2)
         ELSE IF (halflife < 1.0) THEN
            halfl := NumStr(halflife, 3, 1)
         ELSE IF (halflife < 10) THEN
            halfl := NumStr(halflife, 1, 0)
         ELSE IF (halflife < 100) THEN
            halfl := NumStr(halflife, 2, 0)
         ELSE
            halfl := NumStr(halflife, 3, 0);
         IF (dtype = 1) THEN
            out := 'Poisson Distribution: half-life=' + halfl +
            'yr,obs time=' + NumStr(deltatime * 512640.0, 2, 0) +
            'min, # of samples = ' + NumStr(ntrials, 4, 0)
         ELSE
         BEGIN
            IF (obst > 1) THEN
               out := 'Exponential Distribution: half-life=' + halfl +
                      'yr,observation interval=' + Num2Str(obst, 8) + 'yr'
            ELSE
               out := 'Exponential Distribution: half-life=' + halfl +
                      'yr,observation interval=' + Num2Str(obst, 8) + 'yr'
         END;
         Comment(0,0.95,out);
{        PutLabel(top, out);}
         avg := 0.0;
         FOR i := 1 TO 30 DO
            count[i] := 0;
         IF (dtype = 1) THEN
         BEGIN
            InitializePlot(poisson,binomial,maxy,ybottom,dy,xp,ypd,ybd,yp);
            FOR i := 1 TO ntrials DO
            BEGIN
               IF ((quitf = false) AND (restart = 0)) THEN
               BEGIN
                  IF (STEP = TRUE) THEN
                  BEGIN
                     PressKeyMessage;
                     SetUpMyMenu;
                  END;
                  CountDown(i, maxy, dy, xp, yp);
                  TimeEvolution(nparticles, p, mu, shape, x, y, quitf,
                                restart, deltat, numdecays, count);
                  IF (numdecays < 31) THEN
                   PlotDistribution(xp, ypd, yp, dy, numdecays, ybottom, count);
                  avg := avg + numdecays;
                  delay(deltat + 100);
               END;
            END;
            writeln(Output, '  No. of     No. of       Poisson     Binomial');
            writeln(Output, '  Decays    Occurances Distribution Distribution');
            writeln(Output, '  per Obser.');
            writeln(Output, '  Interval');
            FOR i := 1 TO 30 DO
               writeln(Output, '   ', i - 1 : 2, '        ',
               count[i] : 4, '          ', poisson[i] : 4, '         ',
               binomial[i] : 4);
            avg := avg / ntrials;
            writeln(Output);
            writeln(Output, ' Theoretical Average: ', mu : 10 : 7);
            writeln(Output, ' Actual Average: ', avg : 10 : 7);
            writeln(Output);
         END  { IF (dtype = 1) THEN }
         ELSE
         BEGIN
            numground := nparticles;
            InitializePlot2(nparticles, ntrls, obst, exponential, ybottom,
                            dy, xp, yed, yp);
            writeln(Output);
            writeln(Output, '      time    no. of Parent    expected no.');
            writeln(Output, '     (years)     nuclei        of undecayed atoms assuming');
            writeln(Output, '                               an Exponential Distribution');
            FOR i := 1 TO (ntrls + 1) DO
            BEGIN
               IF ((quitf = false) AND (restart = 0)) THEN
               BEGIN
                  writeln(Output, ' ', (i - 1) * deltatime : 10 : 5,
                  '        ', numground : 3, '              ',
                  exponential[i] : 3);
                  PlotDistribution2(i, xp, yed, yp, dy, numground, ybottom);
                  TimeEvolution2(nparticles, p, obst, shape, state, x, y,
                                 quitf, restart, deltat, numground);
               END;  {  IF ((quitf = false ... }
            END;   { FOR i := 1 TO (ntrls+1) DO }
         END;
         MyMenuOut.Display;
         MouseBorders(0, 0, GetMaxX - 5, GetMaxY);
         REPEAT
            ShowCursor;
            CheckForEvents;
            IF HotKeys.Pressed(key) THEN
               HandleHotKeys(key, dtype, deltat, STEP);
            IF (MyMenuOut.Activated) THEN
               HandleMenu(quitf, restart);
         UNTIL ((restart <> 0) OR (quitf = true));
      END; { IF (quitf = false) }
   UNTIL (quitf = true);
   Close(Output);
   CUPSDone;
END.  {  PROGRAM PoisExp  }


