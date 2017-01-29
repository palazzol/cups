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
   {**         Program Otto                        **}
   {**                                             **}
   {** This program demonstrates an Ideal Gas      **}
   {** Four Stroke Internal Combustion Engine -i.e.**}
   {** an Otto Cycle.                              **}
   {**                                             **}
   {** In the upper left corner of the screen is   **}
   {** a T-S plot, in the upper right corner is    **}
   {** a P-V plot and in the lower portion is an   **}
   {** animation of the engine along with some     **}
   {** information about the cycle.                **}
   {**                                             **}
   {**       Written by L. B. Spornick             **}
   {**       Applied Physics Laboratory            **}
   {**       The Johns Hopkins University          **}
   {**                                             **}
   {**        May 16, 1992                         **}
   {**        ver 1.0, July, 1993                  **}
   {**        ver 1.1, October, 1993               **}
   {**        ver 1.2, January, 1994               **}
   {**        ver 1.3, May, 1994                   **}
   {**        ver 1.4, Feb, 1995                   **}
   {**        ver 1.5, Apr, 1995                   **}
   {**                                             **}
   {*************************************************}
   {*************************************************}

PROGRAM Otto;

   USES
      Graph, CUPSmupp, CUPSgui, Crt, cups;

   TYPE
      movement = ARRAY[1..16] OF INTEGER;
      xarray = ARRAY[1..21] OF INTEGER;
      yarray = ARRAY[1..20] OF INTEGER;
      sparray = ARRAY[1..4] OF INTEGER;
      ccarray = ARRAY[1..10] OF INTEGER;
      b2array = ARRAY[1..32] OF INTEGER;
      parray = ARRAY[1..5, 1..32] OF INTEGER;
      varray = ARRAY[1..2, 1..32] OF INTEGER;
      funarray = ARRAY[1..46] OF REAL;
      plotarray = ARRAY[1..46] OF INTEGER;
      adex = ARRAY[1..21] OF REAL;
      padex = ARRAY[1..21] OF INTEGER;
      warray = ARRAY[1..6] OF INTEGER;

   CONST
      slash: 
      FillPatternType = ( $AA , $55 , $AA , $55 , $AA , $55 , $AA , $55 );
      deltatime: 
      b2array = ( 50 , 50 , 50 , 50 , 75 , 75 , 75 , 75 , 75 , 75 , 75 ,
                  75 , 600 , - 40 , - 40 , - 40 , - 40 , - 40 , - 40 ,
                  - 40 , - 40 , 75 , 75 , 75 , 75 , 75 , 75 , 75 , 400 ,
                  50 , 50 , 50 );
      quitf:   BOOLEAN = false;
      restart: BOOLEAN = false;
      STEP:    BOOLEAN = false;
      DELTAT:  INTEGER = 600;

   VAR
      MyMenuOut: TMenu;
      HotKeys: THotKeys;
      key: BYTE;
      vmax, vmin, pmax, pmin, tmax, tmin, smax, smin: REAL;
      vpmax, vpmin, spmax, spmin, tpmax, tpmin, ppmax, ppmin: INTEGER;
      xleft, xright, x1, ytop, ymiddle, ybottom, error: INTEGER;
      x: xarray;
      y: yarray;
      xsp, ysp, xpc, ypc: sparray;
      xcc, ycc: ccarray;
      xb, yb, bradius: INTEGER;
      xivlp, yivlp, rivlp, xcvlp, ycvlp, rcvlp: INTEGER;
      xb2, yb2: b2array;
      b2radius: INTEGER;
      xp, yp: parray;
      xivr, yivr, xiv, yiv, xevr, yevr, xev, yev: varray;
      vol, temp, pres, entrpy: funarray;
      voladex, tempadex, presadex, entadex: adex;
      pvol, ptemp, ppres, pentrpy: plotarray;
      pvoladex, ptempadex, ppresadex, pentadex: padex;
      xw, yw: warray;
      cratio, efficiency: REAL;
      F: text;

   PROCEDURE HELP1;
      VAR
         helpScr: HelpScrType;
   BEGIN
      helpScr[1] := '                                                 ';
      helpScr[2] := '                The Otto Engine                  ';
      helpScr[3] := '  The Otto Engine represents an idealized        ';
      helpScr[4] := '  gasoline engine.  There are six processes:     ';
      helpScr[5] := '  1. The Intake Stroke: A mixture of gasoline    ';
      helpScr[6] := '  vapor and air are drawn into the combustion    ';
      helpScr[7] := '  chamber brought about by the downward move-    ';
      helpScr[8] := '  ment of the piston  (an isobaric and           ';
      helpScr[9] := '  isothermal process)                            ';
      helpScr[10] := '  2. The Compression Stroke: The piston moves    ';
      helpScr[11] := '  up, compressing the gas mixture  (an           ';
      helpScr[12] := '  adiabatic process)                             ';
      helpScr[13] := '  3. Explosion: An electric spark ignites the    ';
      helpScr[14] := '  mixture.  The piston does not move  (an        ';
      helpScr[15] := '  isochoric process)                             ';
      helpScr[16] := '  4. The Power Stroke: The hot gases cause the   ';
      helpScr[17] := '  piston to move down  (an adiabatic process)    ';
      helpScr[18] := '  5. Valve Exhaust: A drop in pressure and       ';
      helpScr[19] := '  temperature caused by the quasistatic          ';
      helpScr[20] := '  ejection of heat due to the opening of the     ';
      helpScr[21] := '  exhaust valve  (an isochoric process)          ';
      helpScr[22] := '  6. Exhaust Stroke: The piston moves up,pushing ';
      helpScr[23] := '  out the remaining gases  (an isobaric and      ';
      helpScr[24] := '  isothermal process)                            ';
      helpScr[25] := '                                                 ';
      HELP(helpScr);
   END;  {  PROCEDURE HELP1  }

   PROCEDURE ABOUTPROGRAM;
      VAR
         helpScr: HelpScrType;
   BEGIN
      helpScr[1] := '                                                 ';
      helpScr[2] := '                                                 ';
      helpScr[3] := '               The Otto Engine                   ';
      helpScr[4] := '                                                 ';
      helpScr[5] := '                       by                        ';
      helpScr[6] := '                                                 ';
      helpScr[7] := '                L.B. Spornick                    ';
      helpScr[8] := '            Applied Physics Laboratory           ';
      helpScr[9] := '           The Johns Hopkins University          ';
      helpScr[10] := '                                                 ';
      helpScr[11] := '             Version 1.5 Apr, 1995               ';
      helpScr[12] := '              Copyright (c) 1995                 ';
      helpScr[13] := '                                                 ';
      helpScr[14] := '  This program models an Otto engine which       ';
      helpScr[15] := '  represents an ideal gasoline engine.  As the   ';
      helpScr[16] := '  engine moves through its cycle, the            ';
      helpScr[17] := '  corresponding points on the Temperature vs     ';
      helpScr[18] := '  Entropy (T vs S) plot and the Pressures vs     ';
      helpScr[19] := '  Volume plot (P vs V) are indicated.            ';
      helpScr[20] := '                                                 ';
      helpScr[21] := '                                                 ';
      helpScr[22] := '                                                 ';
      helpScr[23] := '                                                 ';
      helpScr[24] := '                                                 ';
      helpScr[25] := '                                                 ';
      HELP(helpScr);
   END;  {  PROCEDURE ABOUTPROGRAM  }

   FUNCTION Power (x, y: REAL; VAR error: INTEGER): REAL;

    {****************************************************}
    {**              FUNCTION Power                    **}
    {**   computes the quantity x raised to the y.     **}
    {****************************************************}

   BEGIN
      error := 0;
      IF (x = 0.0) THEN
         Power := 1
      ELSE IF (x > 0.0) THEN
         Power := exp(y * ln(x))
      ELSE IF (x < 0.0) THEN
         error := 1;
   END;  { END FUNCTION Power }

   PROCEDURE ReadData;
      VAR
         screen: TInputScreen;
   BEGIN
      WITH screen DO
      BEGIN
         init;
         DefineInputPort(0.1, 0.87, 0.1, 0.95);
         LoadLine('                 The Otto Engine                  ');
         LoadLine('  The following program demonstrates a four stroke, ');
         LoadLine('  Ideal Gas Otto Engine.                          ');
         LoadLine('  At the top of the screen are the Temperature      ');
         LoadLine('  versus Entropy and the Pressure versus Volume     ');
         LoadLine('  curves.  At the bottom of the screen is an        ');
         LoadLine('  animated engine going through its cycle.          ');
         LoadLine('  The efficiency of the engine depends upon the     ');
         LoadLine('  maximum operating temperature of the engine and   ');
         LoadLine('  the compression ratio (i.e., the ratio of the     ');
         LoadLine('  volumes before and after the adiabatic compression).');
         LoadLine('                                                    ');
         LoadLine('   Input the maximum operating temperature of the engine');
         LoadLine('                {      }  (1000 - 2000 K)          ');
         LoadLine('                                                   ');
         LoadLine('   Input the compression ratio                     ');
         LoadLine('                {      }  (5.0 - 12.0)            ');
         LoadLine('                                                   ');
         LoadLine('               [   Ok   ]   [Cancel]               ');
         SetNumber(1, tMax);
         SetNumberLimits(1, 1000, 2000);
         SetNumber(2, cratio);
         SetNumberLimits(2, 5, 12);
         Accept;
         tMax := GetNumber(1);
         cratio := GetNumber(2);
         IF (canceled) THEN
            quitf := true;
         done;
      END;
   END;  { PROCEDURE ReadData }

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
      CASE MyMenuOut.colChosen OF
         1: 
         BEGIN
            CASE MyMenuOut.rowChosen OF
               1: ABOUTPROGRAM;
               2: AboutCups;
               3: configuration;
               5: restart := NOT restart;
               7: quitf := true;
            END; { CASE }
         END;
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

   PROCEDURE INTERRUPT (key: BYTE; VAR deltat: INTEGER; VAR STEP: BOOLEAN);
   BEGIN
      CASE key OF
         1: 
            Help1;
         2: 
            STEP := NOT STEP;  { F3 - Step }
         3: 
         BEGIN      { F5 - Slower }
            deltat := round(deltat*1/3);
            IF (deltat > 1500) THEN begin
               deltat := 1500;
               beep;
            end;
         END;
         4:
         BEGIN      { F6 - Faster }
            deltat := round(deltat/1.3) ;
            IF (deltat < 200) THEN begin
               deltat := 200;
               beep;
            end;
         END;
         5:
         BEGIN
            MouseBorders(0, 0, GetMaxX, GetMaxY);
            IF (MyMenuOut.chosen) THEN
               HandleMenu(restart, quitf);
         END;
      END;  { CASE }
   END;  { PROCEDURE INTERRUPT }

   PROCEDURE GetCycle (vmax, vmin, pmax, pmin, tmax, tmin, smax, smin,
             cratio: REAL; VAR vol, temp, pres, entrpy: funarray;
             VAR voladex, tempadex, presadex, entadex: adex;
             VAR efficiency: REAL);
    {***************************************************************}
    {**                   PROCEDURE GetCycle                      **}
    {** calculates the various points (T,S) and (P,V) along the   **}
    {** cycle and the engine's efficiency.                        **}
    {***************************************************************}
      CONST
         R: 
         REAL = 8.314510;
      VAR
         i, error: INTEGER;
         deltatemp, deltavol, atm, cv, cp, gamma: REAL;
   BEGIN
      gamma := 1.4;
      cv := 2.5 * R;
      cp := cv * gamma;
      atm := 1.01325 * Power(10.0, 5, error);
       { intake and exhaust strokes data }
      vol[1] := 0.0125;
      pres[1] := 1.0;
      temp[1] := 300.0;
      entrpy[1] := 37.5;
      vol[2] := 0.0154;
      pres[2] := 1.0;
      temp[2] := 300.0;
      entrpy[2] := 49.0;
      vol[3] := 0.0185;
      pres[3] := 1.0;
      temp[3] := 300.0;
      entrpy[3] := 56.3;
      vol[4] := 0.0216;
      pres[4] := 1.0;
      temp[4] := 300.0;
      entrpy[4] := 65.7;
      vol[5] := 0.0246;
      pres[5] := 1.0;
      temp[5] := 300.0;
      entrpy[5] := 75.0;
      vol[36] := 0.0216;
      pres[36] := 1.0;
      temp[36] := 300.0;
      entrpy[36] := 65.7;
      vol[37] := 0.0185;
      pres[37] := 1.0;
      temp[37] := 300.0;
      entrpy[37] := 56.3;
      vol[38] := 0.0154;
      pres[38] := 1.0;
      temp[38] := 300.0;
      entrpy[38] := 46.9;
      vol[39] := 0.0125;
      pres[39] := 1.0;
      temp[39] := 300.0;
      entrpy[39] := 37.5;
      vol[40] := 0.0096;
      pres[40] := 1.0;
      temp[40] := 300.0;
      entrpy[40] := 28.2;
      vol[41] := 0.0067;
      pres[41] := 1.0;
      temp[41] := 300.0;
      entrpy[41] := 18.9;
      vol[42] := 0.0038;
      pres[42] := 1.0;
      temp[42] := 300.0;
      entrpy[42] := 09.3;
      vol[43] := 0.0000;
      pres[43] := 1.0;
      temp[43] := 300.0;
      entrpy[43] := 00.0;
      vol[44] := 0.0038;
      pres[44] := 1.0;
      temp[44] := 300.0;
      entrpy[44] := 09.3;
      vol[45] := 0.0067;
      pres[45] := 1.0;
      temp[45] := 300.0;
      entrpy[45] := 18.9;
      vol[46] := 0.0096;
      pres[46] := 1.0;
      temp[46] := 300.0;
      entrpy[46] := 28.2;
       { adiabatic compression - compression stroke data }
      deltatemp := (Power(cratio, (gamma - 1.0), error) - 1.0) * temp[5]/8.0;
      FOR i := 6 TO 13 DO
      BEGIN
         temp[i] := temp[5] + (i - 5) * deltatemp;
         vol[i] := vol[5]*Power((temp[5]/temp[i]), 1.0/(gamma-1.0),error);
         pres[i] := R * temp[i] / (vol[i] * atm);
         entrpy[i] := entrpy[5];
      END;
       { isochoric expansion - explosion data }
      deltatemp := (tmax - temp[13]) / 7.0;
      FOR i := 14 TO 20 DO
      BEGIN
         vol[i] := vol[13];
         temp[i] := temp[13] + (i - 13) * deltatemp;
         pres[i] := pres[13] * temp[i] / temp[13];
         entrpy[i] := entrpy[13] + cv * ln(temp[i] / temp[13]);
      END;
       { adiabatic expansion - power stroke }
      deltavol := (vol[5] - vol[20]) / 7.0;
      FOR i := 21 TO 27 DO
      BEGIN
         vol[i] := vol[20] + (i - 20) * deltavol;
         temp[i] := temp[20]*Power((vol[20]/vol[i]), (gamma-1.0),error);
         pres[i] := R * temp[i] / (atm * vol[i]);
         entrpy[i] := entrpy[20];
      END;
      deltavol := (vol[5] - vol[20]) / 20;
      FOR i := 1 TO 21 DO
      BEGIN
         voladex[i] := vol[20] + (i - 1) * deltavol;
         tempadex[i] := temp[20] * Power((vol[20] / voladex[i]),
                        (gamma - 1.0), error);
         presadex[i] := R * tempadex[i] / (atm * voladex[i]);
         entadex[i] := entrpy[20];
      END;
       { isochoric process - valve exhaust }
      deltatemp := (temp[5] - temp[27]) / 8.0;
      FOR i := 28 TO 35 DO
      BEGIN
         temp[i] := temp[27] + (i - 27) * deltatemp;
         pres[i] := pres[27] * temp[i] / temp[27];
         vol[i] := vol[5];
         entrpy[i] := entrpy[27] + cv * ln(temp[i] / temp[27]);
      END;
      efficiency := 1.0 - (temp[27] - temp[35]) / (temp[20] - temp[13]);
   END;  { PROCEDURE GetCycle }

   PROCEDURE InitializeGrid (deltat: INTEGER; deltatime: b2array;
      vol, temp, pres, entrpy: funarray; voladex, tempadex, presadex,
      entadex: adex; vmax, vmin, pmax, pmin, tmax, tmin, smax, smin,
      efficiency, cratio: REAL; VAR vpmax, vpmin, ppmax, ppmin, tpmax,
      tpmin, spmax, spmin, xleft, x1, xright, ytop, ymiddle,
      ybottom: INTEGER; VAR x: xarray; VAR y: yarray;
      VAR xsp, ysp: sparray; VAR xcc, ycc: ccarray;
      VAR xb, yb, bradius: INTEGER; VAR xpc, ypc: sparray;
      VAR xivlp, yivlp, rivlp: INTEGER; VAR xcvlp, ycvlp, rcvlp: INTEGER;
      VAR xb2, yb2: b2array; VAR b2radius: INTEGER; VAR xp, yp: parray;
      VAR xivr, yivr, xiv, yiv, xevr, yevr, xev, yev: varray;
      VAR pvol, ptemp, ppres, pentrpy: plotarray;
      VAR pvoladex, ptempadex, ppresadex, pentadex: padex;
      VAR xw, yw: warray);

    {****************************************************}
    {**             PROCEDURE InitializeGrid           **}
    {** sets up the screen and determines the plot     **}
    {** coordinate systems.                            **}
    {****************************************************}

      VAR
         xwidth, ylength, i, i1, i2, ip: INTEGER;
         val: REAL;
         tex: STRING;
         ss, sp, sv, st: REAL;
         s1, t1, p1, v1: INTEGER;
         deltay, deltax: INTEGER;
         ArcCoords: ArcCoordsType;
         TextInfo: TextSettingsType;
   BEGIN
      DefineViewPort(1, 0.1, 0.95, 0.13, 0.9);
      DefineScale(1, 0.1, 1.0, 0.05, 1.0);
      OpenViewPort(1);
      SelectScale(1);
      SelectViewPort(1);
      SetColor(white);
      PutLabel(top, 'Internal Combustion Engine - Otto Cycle');
      tex := 'Max Operating Temp-' + NumStr(tmax - 100, 5, 1) +
      ',      Compression Ratio-' + NumStr(cratio, 4, 2) + ', Efficiency-' +
      NumStr(efficiency, 6, 4);
      PutLabel(bottom, tex);
      { draw the lines which separates the different sections }

      map(0.1, 0.05, xleft, ybottom);
      map(1.0, 1.0, xright, ytop);
      x1 := Round((xright + xleft) / 2.0);
      ymiddle := Round((ytop + ybottom) / 2.0);
      xwidth := x1 - xleft;
      ylength := ymiddle - ytop;

      SetColor(white);

      Line(xleft, ymiddle, xright, ymiddle);
      Line(x1, ytop, x1, ymiddle);

      {*********************************************}
      { annote the different sections of the screen }
      {*********************************************}

           { T-S Diagram - upper left section }

      OutTextXY(xleft + 60, ytop + 10, 'T(K) versus S(J/K)');
      tpmin := ymiddle - 30;
      tpmax := ytop + 40;
      spmin := xleft + 40;
      spmax := x1 - 30;
      Line(spmin, tpmin, spmin, tpmax);
      Line(spmax, tpmin, spmax, tpmax);
      Line(spmin, tpmin, spmax, tpmin);
      Line(spmin, tpmax, spmax, tpmax);
      tex := NumStr(smin, 4, 0);
      OutTextXY(spmin - 20, tpmin + 10, tex);
      tex := NumStr(smax, 4, 0);
      OutTextXY(spmax - 20, tpmin + 10, tex);
      tex := NumStr(tmin, 4, 0);
      OutTextXY(spmin - 40, tpmin - 8, tex);
      tex := NumStr(tmax, 4, 0);
      OutTextXY(spmin - 40, tpmax, tex);
      FOR i := 1 TO 3 DO
      BEGIN
         i1 := spmin + (i * (spmax - spmin)) DIV 4;
         Line(i1, tpmin, i1, tpmin - 5);
         Line(i1, tpmax, i1, tpmax + 5);
         i1 := tpmin + (i * (tpmax - tpmin)) DIV 4;
         Line(spmin, i1, spmin + 5, i1);
         Line(spmax, i1, spmax - 5, i1);
      END;  { FOR i := 1 TO 3 DO }
      tex := 'Entropy (J/K)';
      OutTextXY(spmin + 40, tpmin + 10, tex);
      tex := 'Temp (K)';
      GetTextSettings(TextInfo);
      SetTextStyle(TextInfo.Font, 1, 1);
      OutTextXY(spmin - 10, tpmax + 25, tex);
      SetTextStyle(TextInfo.Font, 0, 1);
      ss := (spmax - spmin) / (smax - smin);
      st := (tpmax - tpmin) / (tmax - tmin);
      FOR i := 1 TO 46 DO
      BEGIN
         ptemp[i] := Round(st * (temp[i] - tmin) + tpmin);
         pentrpy[i] := Round(ss * (entrpy[i] - smin) + spmin);
      END;
      FOR i := 1 TO 21 DO
      BEGIN
         ptempadex[i] := Round(st * (tempadex[i] - tmin) + tpmin);
         pentadex[i] := Round(ss * (entadex[i] - smin) + spmin);
      END;
      FOR i := 1 TO 19 DO
         Line(pentrpy[i], ptemp[i], pentrpy[i + 1], ptemp[i + 1]);
      FOR i := 1 TO 20 DO
         Line(pentadex[i], ptempadex[i], pentadex[i + 1], ptempadex[i + 1]);
      FOR i := 27 TO 45 DO
         Line(pentrpy[i], ptemp[i], pentrpy[i + 1], ptemp[i + 1]);
      Line(pentrpy[1], ptemp[1], pentrpy[46], ptemp[46]);

      {*********************************************}
           { P-V Diagram - upper right section }

      OutTextXY(x1 + 60, ytop + 10, 'P(atm) versus V(m3)');
      ppmin := ymiddle - 30;
      ppmax := ytop + 40;
      vpmin := x1 + 40;
      vpmax := xright - 30;
      Line(vpmin, ppmin, vpmin, ppmax);
      Line(vpmax, ppmin, vpmax, ppmax);
      Line(vpmin, ppmin, vpmax, ppmin);
      Line(vpmin, ppmax, vpmax, ppmax);
      tex := NumStr(vmin, 4, 3);
      OutTextXY(vpmin - 20, ppmin + 10, tex);
      tex := NumStr(vmax, 4, 3);
      OutTextXY(vpmax - 20, ppmin + 10, tex);
      tex := NumStr(pmin, 4, 1);
      OutTextXY(vpmin - 40, ppmin - 8, tex);
      tex := NumStr(pmax, 4, 1);
      OutTextXY(vpmin - 40, ppmax, tex);
      FOR i := 1 TO 3 DO
      BEGIN
         i1 := vpmin + (i * (vpmax - vpmin)) DIV 4;
         Line(i1, ppmin, i1, ppmin - 5);
         Line(i1, ppmax, i1, ppmax + 5);
         i1 := ppmin + (i * (ppmax - ppmin)) DIV 4;
         Line(vpmin, i1, vpmin + 5, i1);
         Line(vpmax, i1, vpmax - 5, i1);
      END;  { FOR i := 1 TO 3 DO }
      tex := 'Volume (m3)';
      OutTextXY(vpmin + 50, ppmin + 10, tex);
      tex := 'Pres (atm)';
      SetTextStyle(TextInfo.Font, 1, 1);
      OutTextXY(vpmin - 10, ppmax + 18, tex);
      SetTextStyle(TextInfo.Font, 0, 1);
      sp := (ppmax - ppmin) / (pmax - pmin);
      sv := (vpmax - vpmin) / (vmax - vmin);
      FOR i := 1 TO 46 DO
      BEGIN
         pvol[i] := Round(sv * (vol[i] - vmin) + vpmin);
         ppres[i] := Round(sp * (pres[i] - pmin) + ppmin);
      END;
      FOR i := 1 TO 21 DO
      BEGIN
         pvoladex[i] := Round(sv * (voladex[i] - vmin) + vpmin);
         ppresadex[i] := Round(sp * (presadex[i] - pmin) + ppmin);
      END;
      FOR i := 1 TO 19 DO
         Line(pvol[i], ppres[i], pvol[i + 1], ppres[i + 1]);
      FOR i := 1 TO 20 DO
         Line(pvoladex[i], ppresadex[i], pvoladex[i + 1], ppresadex[i + 1]);
      FOR i := 27 TO 45 DO
         Line(pvol[i], ppres[i], pvol[i + 1], ppres[i + 1]);
      Line(pvol[1], ppres[1], pvol[46], ppres[46]);

      {*********************************************}
             { draw engine - lower screen }

      deltay := (ybottom - ymiddle) DIV 20;
      FOR i := 1 TO 20 DO
         y[i] := ymiddle + i * deltay;
      deltax := deltay;
      x[1] := x1 - 10 * deltax;
      FOR i := 2 TO 21 DO
         x[i] := x[1] + (i - 1) * deltax;
      { spark plug }
      xsp[1] := (x[11] + x[12]) DIV 2;
      xsp[2] := (x[10] + x[11]) DIV 2;
      xsp[3] := xsp[2];
      xsp[4] := xsp[1];
      ysp[1] := y[1];
      ysp[2] := y[1];
      ysp[3] := (y[2] + y[3]) DIV 2;
      ysp[4] := (y[2] + y[3]) DIV 2;
      Line(xsp[1], ysp[1], xsp[2], ysp[2]);
      Line(xsp[2], ysp[2], xsp[3], ysp[3]);
      Line(xsp[3], ysp[3], xsp[4], ysp[4]);
      Line(xsp[1], ysp[1], xsp[4], ysp[4]);
      SetColor(white);
      SetFillStyle(1, white);
      FloodFill(xsp[1] - 2, ysp[1] + 2, white);
      SetColor(white);
      { combustion chamber }
      xcc[1] := x[15] + 3;
      xcc[2] := x[7] - 3;
      xcc[3] := x[7] - 3;
      xcc[4] := x[7];
      xcc[5] := x[8];
      xcc[6] := x[9];
      xcc[7] := x[13];
      xcc[8] := x[14];
      xcc[9] := x[15];
      xcc[10] := x[15] + 3;
      ycc[1] := y[2];
      ycc[2] := y[2];
      ycc[3] := y[3];
      ycc[4] := y[3];
      ycc[5] := y[3];
      ycc[6] := y[3];
      ycc[7] := y[3];
      ycc[8] := y[3];
      ycc[9] := y[3];
      ycc[10] := y[3];
      Line(xcc[1], ycc[1], xcc[2], ycc[2]);
      Line(xcc[2], ycc[2], xcc[3], ycc[3]);
      Line(xcc[3], ycc[3], xcc[4], ycc[4]);
      Line(xcc[5], ycc[5], xcc[6], ycc[6]);
      Line(xcc[7], ycc[7], xcc[8], ycc[8]);
      Line(xcc[9], ycc[9], xcc[10], ycc[10]);
      Line(xcc[1], ycc[1], xcc[10], ycc[10]);
      { bearing and bearing chamber }
      xb := x[11];
      yb := (y[13] + y[14]) DIV 2;
      bradius := deltax DIV 3;
      SetColor(white);
      SetFillStyle(1, white);
      FillEllipse(xb, yb, bradius, bradius);
      Arc(xb, yb, 0, 67, 5 * deltay);
      Arc(xb, yb, 113, 360, 5 * deltay);
      SetColor(DarkGray);
      FOR i := 1 TO 15 DO
      BEGIN
         Arc(xb, yb, 0, Round(i * 22.5), Round(0.9 * deltay));
         GetArcCoords(ArcCoords);
         WITH ArcCoords DO
         BEGIN
            xb2[17 - i] := Xend;
            yb2[17 - i] := Yend;
         END;
      END;
      xb2[1] := x[11] + Round(0.9 * deltay);
      yb2[1] := yb;
      FOR i := 1 TO 16 DO
      BEGIN
         xb2[i + 16] := xb2[i];
         yb2[i + 16] := yb2[i];
      END;
      b2radius := 4;
      SetColor(white);
      { piston chamber }
      xpc[1] := x[13];
      xpc[2] := x[9];
      xpc[3] := x[9];
      xpc[4] := x[13];
      ypc[1] := y[3];
      ypc[2] := y[3];
      ypc[3] := y[9];
      ypc[4] := y[9];
      Line(xpc[1], ypc[1], xpc[4], ypc[4]);
      Line(xpc[2], ypc[2], xpc[3], ypc[3]);
      { intake valve lope point }
      xivlp := (x[7] + x[8]) DIV 2;
      yivlp := y[8];
      rivlp := 2;
      SetColor(white);
      SetFillStyle(1, white);
      FillEllipse(xivlp, yivlp, rivlp, rivlp);
      FOR i := 1 TO 32 DO
      BEGIN
         xivr[1, i] := (x[7] + x[8]) DIV 2;
         xivr[2, i] := (x[7] + x[8]) DIV 2;
         xiv[1, i] := x[7];
         xiv[2, i] := x[8];
         yivr[1, i] := y[8];
         yivr[2, i] := y[3];
         yiv[1, i] := y[3];
         yiv[2, i] := y[3];
      END;
      FOR i := 1 TO 4 DO
      BEGIN
         yivr[1, i] := y[8] - 4;
         yivr[2, i] := y[3] - 4;
         yiv[1, i] := y[3] - 4;
         yiv[2, i] := y[3] - 4;
      END;
      FOR i := 30 TO 32 DO
      BEGIN
         yivr[1, i] := y[8] - 4;
         yivr[2, i] := y[3] - 4;
         yiv[1, i] := y[3] - 4;
         yiv[2, i] := y[3] - 4;
      END;
      { piston }
      FOR i := 1 TO 32 DO
      BEGIN
         xp[1, i] := x[13] - 1;
         xp[2, i] := x[9] + 1;
         xp[3, i] := x[9] + 1;
         xp[4, i] := x[13] - 1;
         xp[5, i] := x[11];
      END;
      yp[1, 1] := y[5];
      yp[1, 2] := (y[5] + y[6]) DIV 2;
      yp[1, 3] := y[6];
      yp[1, 4] := (y[6] + y[7]) DIV 2;
      yp[1, 5] := y[7];
      yp[1, 6] := yp[1, 4];
      yp[1, 7] := yp[1, 3];
      yp[1, 8] := yp[1, 2];
      yp[1, 9] := yp[1, 1];
      yp[1, 10] := (y[4] + y[5]) DIV 2;
      yp[1, 11] := y[4];
      yp[1, 12] := (y[3] + y[4]) DIV 2;
      yp[1, 13] := y[3];
      yp[1, 14] := yp[1, 12];
      yp[1, 15] := yp[1, 11];
      yp[1, 16] := yp[1, 10];
      FOR i := 1 TO 16 DO
      BEGIN
         yp[2, i] := yp[1, i];
         yp[3, i] := yp[1, i] + 2 * deltay;
         yp[4, i] := yp[3, i];
         yp[5, i] := yp[3, i];
         yp[1, i + 16] := yp[1, i];
         yp[2, i + 16] := yp[2, i];
         yp[3, i + 16] := yp[3, i];
         yp[4, i + 16] := yp[4, i];
         yp[5, i + 16] := yp[5, i];
      END;
      { camshaft valve lifter point }
      xcvlp := (x[14] + x[15]) DIV 2;
      ycvlp := y[8];
      rcvlp := 2;
      SetColor(white);
      SetFillStyle(1, white);
      FillEllipse(xcvlp, ycvlp, rcvlp, rcvlp);
      FOR i := 1 TO 32 DO
      BEGIN
         xevr[1, i] := (x[14] + x[15]) DIV 2;
         xevr[2, i] := (x[14] + x[15]) DIV 2;
         xev[1, i] := x[14];
         xev[2, i] := x[15];
         yevr[1, i] := y[8];
         yevr[2, i] := y[3];
         yev[1, i] := y[3];
         yev[2, i] := y[3];
      END;
      FOR i := 21 TO 28 DO
      BEGIN
         yevr[1, i] := y[8] - 4;
         yevr[2, i] := y[3] - 4;
         yev[1, i] := y[3] - 4;
         yev[2, i] := y[3] - 4;
      END;
      xw[1] := xleft + 30;
      xw[2] := xleft + 30;
      xw[3] := xleft + 30;
      xw[4] := x[18];
      xw[5] := x[18];
      xw[6] := x[18];
      yw[1] := y[4];
      yw[2] := y[10];
      yw[3] := y[15];
      yw[4] := y[4];
      yw[5] := y[10];
      yw[6] := y[15];
      SetColor(white);
      OutTextXY(xw[1], yw[1], 'Intake Stroke');
      OutTextXY(xw[1], yw[1] + 10, '(Isobaric & Isothermal)');
      OutTextXY(xw[1], yw[1] + 20, '(Intake Air and Fuel)');
      OutTextXY(xw[2], yw[2], 'Compression Stroke');
      OutTextXY(xw[2], yw[2] + 10, '(Adiabatic)');
      OutTextXY(xw[2], yw[2] + 20, '(Compress Mixture)');
      OutTextXY(xw[3], yw[3], 'Explosion');
      OutTextXY(xw[3], yw[3] + 10, '(Isochoric)');
      OutTextXY(xw[4], yw[4], 'Power Stroke');
      OutTextXY(xw[4], yw[4] + 10, '(Adiabatic)');
      OutTextXY(xw[4], yw[4] + 20, '(Expand Combustion');
      OutTextXY(xw[4], yw[4] + 30, ' By-Products)');
      OutTextXY(xw[5], yw[5], 'Valve Exhaust');
      OutTextXY(xw[5], yw[5] + 10, '(Isochoric Heat Exchange');
      OutTextXY(xw[5], yw[5] + 20, ' bet Comb By-Prod and Atm)');
      OutTextXY(xw[6], yw[6], 'Exhaust Stroke');
      OutTextXY(xw[6], yw[6] + 10, '(Isobaric & Isothermal)');
      OutTextXY(xw[6], yw[6] + 20, '(Exhaust Combustion');
      OutTextXY(xw[6], yw[6] + 30, ' By-Products)');
   END;  { PROCEDURE InitializeGrid }

   PROCEDURE Animation (VAR deltat: INTEGER; deltatime: b2array;
      xsp, ysp: sparray; xcc, ycc: ccarray; xb, yb, bradius: INTEGER;
      xpc, ypc: sparray; xivlp, yivlp, rivlp: INTEGER;
      xcvlp, ycvlp, rcvlp: INTEGER; xb2, yb2: b2array;
      VAR b2radius: INTEGER; xp, yp: parray; xivr, yivr, xiv, yiv, xevr,
      yevr, xev, yev: varray; pvol, ptemp, ppres, pentrpy: plotarray;
      pvoladex, ptempadex, ppresadex, pentadex: padex; xw, yw: warray;
      vpmax, vpmin, ppmax, ppmin, tpmax, tpmin, spmax, spmin: INTEGER;
      VAR quitf, restart: BOOLEAN);

    {****************************************************}
    {**             PROCEDURE Animation                **}
    {** animates the engine and the P-V and T-S        **}
    {** diagrams.                                      **}
    {****************************************************}

      VAR
         i, i1, i2, ip: INTEGER;
         STEP: BOOLEAN;
   BEGIN
      STEP := false;
      restart := false;
      REPEAT
         FOR i := 1 TO 32 DO
         BEGIN
            IF ((quitf = false) AND (restart = false)) THEN
            BEGIN
               IF (i = 1) OR (i = 30) THEN
               BEGIN
                  SetColor(green);
                  OutTextXY(xw[1], yw[1], 'Intake Stroke');
                  OutTextXY(xw[1], yw[1] + 10, '(Isobaric & Isothermal)');
                  OutTextXY(xw[1], yw[1] + 20, '(Intake Air and Fuel)');
                  SetColor(white);
                  OutTextXY(xw[6], yw[6], 'Exhaust Stroke');
                  OutTextXY(xw[6], yw[6] + 10, '(Isobaric & Isothermal)');
                  OutTextXY(xw[6], yw[6] + 20, '(Exhaust Combustion');
                  OutTextXY(xw[6], yw[6] + 30, ' By-Products)');
               END;
               IF (i = 6) THEN
               BEGIN
                  SetColor(green);
                  OutTextXY(xw[2], yw[2], 'Compression Stroke');
                  OutTextXY(xw[2], yw[2] + 10, '(Adiabatic)');
                  OutTextXY(xw[2], yw[2] + 20, '(Compress Mixture)');
                  SetColor(white);
                  OutTextXY(xw[1], yw[1], 'Intake Stroke');
                  OutTextXY(xw[1], yw[1] + 20, '(Intake Air and Fuel)');
                  OutTextXY(xw[1], yw[1] + 10, '(Isobaric & Isothermal)');
               END;
               IF (i = 13) THEN
               BEGIN
                  SetColor(lightred);
                  OutTextXY(xw[3], yw[3], 'Explosion');
                  OutTextXY(xw[3], yw[3] + 10, '(Isochoric)');
                  SetColor(white);
                  OutTextXY(xw[2], yw[2], 'Compression Stroke');
                  OutTextXY(xw[2], yw[2] + 10, '(Adiabatic)');
                  OutTextXY(xw[2], yw[2] + 20, '(Compress Mixture)');
               END;
               IF (i = 14) THEN
               BEGIN
                  SetColor(lightred);
                  OutTextXY(xw[4], yw[4], 'Power Stroke');
                  OutTextXY(xw[4], yw[4] + 10, '(Adiabatic)');
                  OutTextXY(xw[4], yw[4] + 20, '(Expand Combustion');
                  OutTextXY(xw[4], yw[4] + 30, ' By-Products)');
                  SetColor(white);
                  OutTextXY(xw[3], yw[3], 'Explosion');
                  OutTextXY(xw[3], yw[3] + 10, '(Isochoric)');
               END;
               IF (i = 21) THEN
               BEGIN
                  SetColor(lightred);
                  OutTextXY(xw[5], yw[5], 'Valve Exhaust');
                  OutTextXY(xw[5], yw[5] + 10, '(Isochoric Heat Exchange');
                  OutTextXY(xw[5], yw[5] + 20, ' bet Comb By-Prod and Atm)');
                  SetColor(white);
                  OutTextXY(xw[4], yw[4], 'Power Stroke');
                  OutTextXY(xw[4], yw[4] + 10, '(Adiabatic)');
                  OutTextXY(xw[4], yw[4] + 20, '(Expand Combustion');
                  OutTextXY(xw[4], yw[4] + 30, ' By-Products)');
               END;
               IF (i = 22) THEN
               BEGIN
                  SetColor(lightred);
                  OutTextXY(xw[6], yw[6], 'Exhaust Stroke');
                  OutTextXY(xw[6], yw[6] + 10, '(Isobaric & Isothermal)');
                  OutTextXY(xw[6], yw[6] + 20, '(Exhaust Combustion');
                  OutTextXY(xw[6], yw[6] + 30, ' By-Products)');
                  SetColor(white);
                  OutTextXY(xw[5], yw[5], 'Valve Exhaust');
                  OutTextXY(xw[5], yw[5] + 10, '(Isochoric Heat Exchange');
                  OutTextXY(xw[5], yw[5] + 20, ' bet Comb By-Prod and Atm)');
               END;
               IF (i <= 13) THEN
               BEGIN
                  SetColor(white);
                  SetFillStyle(solidfill, green);
                  FillEllipse(pvol[i], ppres[i], 2, 2);
                  FillEllipse(pentrpy[i], ptemp[i], 2, 2);
               END;
               IF (i > 13) AND (i < 21) THEN
               BEGIN
                  SetColor(white);
                  SetFillStyle(solidfill, lightred);
                  FillEllipse(pvol[i + 7], ppres[i + 7], 2, 2);
                  FillEllipse(pentrpy[i + 7], ptemp[i + 7], 2, 2);
               END;
               IF (i > 21) THEN
               BEGIN
                  IF (i < 29) THEN
                     SetFillStyle(solidfill, lightred)
                  ELSE
                     SetFillStyle(solidfill, green);
                  SetColor(white);
                  FillEllipse(pvol[i + 14], ppres[i + 14], 2, 2);
                  FillEllipse(pentrpy[i + 14], ptemp[i + 14], 2, 2);
               END;
               IF ((i >= 1) AND (i <= 4)) OR ((i >= 30) AND (i <= 32)) THEN
               BEGIN
                  SetBkColor(DarkGray);
                  SetColor(green);
                  SetFillStyle(WideDotFill, green);
                  BAR(xcc[1] - 1, ycc[1] + 1, xcc[3] + 1, ycc[3] - 1);
                  BAR(xcc[7] - 1, ycc[7] + 1, xp[2, i] + 1, yp[2, i] - 1);
               END;
               IF ((i >= 5) AND (i <= 12)) THEN
               BEGIN
                  SetBkColor(DarkGray);
                  SetFillStyle(CloseDotFill, green);
                  BAR(xcc[1] - 1, ycc[1] + 1, xcc[3] + 1, ycc[3] - 1);
                  BAR(xcc[7] - 1, ycc[7] + 1, xp[2, i] + 1, yp[2, i] - 1);
               END;
               IF ((i >= 14) AND (i <= 21)) THEN
               BEGIN
                  SetBkColor(DarkGray);
                  SetFillStyle(WideDotFill, lightred);
                  BAR(xcc[1] - 1, ycc[1] + 1, xcc[3] + 1, ycc[3] - 1);
                  BAR(xcc[7] - 1, ycc[7] + 1, xp[2, i] + 1, yp[2, i] - 1);
               END;
               IF ((i >= 22) AND (i <= 28)) THEN
               BEGIN
                  SetBkColor(DarkGray);
                  SetFillStyle(CloseDotFill, lightred);
                  BAR(xcc[1] - 1, ycc[1] + 1, xcc[3] + 1, ycc[3] - 1);
                  BAR(xcc[7] - 1, ycc[7] + 1, xp[2, i] + 1, yp[2, i] - 1);
               END;
               IF (i = 29) THEN
               BEGIN
                  SetBkColor(DarkGray);
                  SetFillStyle(EmptyFill, DarkGray);
                  BAR(xcc[1] - 1, ycc[1] + 1, xcc[3] + 1, ycc[3] - 1);
                  BAR(xcc[7] - 1, ycc[7] + 1, xp[2, i] + 1, yp[2, i] - 1);
               END;
               IF (i = 13) THEN
               BEGIN
                  SetFillStyle(CloseDotFill, lightred);
                  BAR(xcc[1] - 1, ycc[1] + 1, xcc[3] + 1, ycc[3] - 1);
                  BAR(xcc[7] - 1, ycc[7] + 1, xp[2, i] + 1, yp[2, i] - 1);
                  SetColor(red);
                  SetFillPattern(slash, red);
                  BAR(xsp[1], (ysp[1] + ysp[4]) DIV 2, xsp[3], ysp[3]);
                  SetBkColor(DarkGray);
               END;
               SetColor(white);
               SetFillStyle(1, white);
               FillEllipse(xb2[i], yb2[i], b2radius, b2radius);
               FillEllipse(xb, yb, bradius, bradius);
               Line(xp[1, i], yp[1, i], xp[2, i], yp[2, i]);
               Line(xp[3, i], yp[3, i], xp[4, i], yp[4, i]);
               Line(xp[1, i], yp[1, i], xp[4, i], yp[4, i]);
               Line(xp[2, i], yp[2, i], xp[3, i], yp[3, i]);
               SetFillStyle(0, white);
               FloodFill(xp[1, i] - 1, yp[1, i] + 1, white);
               SetLineStyle(solidln, 0, thickwidth);
               Line(xb, yb, xb2[i], yb2[i]);
               Line(xb2[i], yb2[i], xp[5, i], yp[5, i]);
               SetLineStyle(solidln, 0, normwidth);
               Line(xivr[1, i], yivr[1, i], xivr[2, i], yivr[2, i]);
               Line(xiv[1, i], yiv[1, i], xiv[2, i], yiv[2, i]);
               Line(xevr[1, i], yevr[1, i], xevr[2, i], yevr[2, i]);
               Line(xev[1, i], yev[1, i], xev[2, i], yev[2, i]);
               IF (i = 13) THEN
               BEGIN
                  FOR i1 := 13 TO 20 DO
                  BEGIN
                     SetColor(white);
                     SetFillStyle(solidfill, lightred);
                     FillEllipse(pvol[i1], ppres[i1], 2, 2);
                     FillEllipse(pentrpy[i1], ptemp[i1], 2, 2);
                     delay(100);
                     SetColor(DarkGray);
                     SetFillStyle(solidfill, DarkGray);
                     FillEllipse(pvol[i1], ppres[i1], 2, 2);
                     FillEllipse(pentrpy[i1], ptemp[i1], 2, 2);
                     SetColor(white);
                     Line(pvol[13], ppres[13], pvol[20], ppres[20]);
                     FOR ip := 13 TO 21 DO
                        Line(pentrpy[ip - 1], ptemp[ip - 1], pentrpy[ip],
                             ptemp[ip]);
                  END;
               END;
               IF (i = 21) THEN
               BEGIN
                  FOR i1 := 28 TO 35 DO
                  BEGIN
                     SetColor(white);
                     SetFillStyle(solidfill, lightred);
                     FillEllipse(pvol[i1], ppres[i1], 2, 2);
                     FillEllipse(pentrpy[i1], ptemp[i1], 2, 2);
                     delay(100);
                     SetColor(DarkGray);
                     SetFillStyle(solidfill, DarkGray);
                     FillEllipse(pvol[i1], ppres[i1], 2, 2);
                     FillEllipse(pentrpy[i1], ptemp[i1], 2, 2);
                     SetColor(white);
                     Line(pvol[13], ppres[13], pvol[20], ppres[20]);
                     FOR ip := 28 TO 36 DO
                        Line(pentrpy[ip - 1], ptemp[ip - 1],
                             pentrpy[ip], ptemp[ip]);
                  END;
               END;
               IF (STEP) THEN
               BEGIN
                  PressKeyMessage;
                  SetUpMyMenu;
               END;
               IF (i <> 13) AND (i <> 21) THEN{ AND (Not STEP)}
               BEGIN
                  delay(deltatime[i] + deltat);
                  CheckForEvents;
                  IF HotKeys.Pressed(key) THEN
                     INTERRUPT(key, deltat, STEP);
                  MouseBorders(5, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
               END;
               SetColor(DarkGray);
               SetFillStyle(solidfill, darkgray);
               IF (i <= 13) THEN
               BEGIN
                  FillEllipse(pvol[i], ppres[i], 2, 2);
                  FillEllipse(pentrpy[i], ptemp[i], 2, 2);
               END;
               IF (i >= 13) AND (i < 21) THEN
               BEGIN
                  FillEllipse(pvol[i + 7], ppres[i + 7], 2, 2);
                  FillEllipse(pentrpy[i + 7], ptemp[i + 7], 2, 2);
               END;
               IF (i > 21) THEN
               BEGIN
                  FillEllipse(pvol[i + 14], ppres[i + 14], 2, 2);
                  FillEllipse(pentrpy[i + 14], ptemp[i + 14], 2, 2);
               END;
               SetColor(white);
               FOR i1 := 1 TO 19 DO
                  Line(pvol[i1], ppres[i1], pvol[i1 + 1], ppres[i1 + 1]);
               FOR i1 := 1 TO 20 DO
                  Line(pvoladex[i1], ppresadex[i1],
                       pvoladex[i1 + 1], ppresadex[i1 + 1]);
               FOR i1 := 27 TO 45 DO
                  Line(pvol[i1], ppres[i1], pvol[i1 + 1], ppres[i1 + 1]);
               Line(pvol[1], ppres[1], pvol[46], ppres[46]);
               FOR i1 := 1 TO 19 DO
                  Line(pentrpy[i1], ptemp[i1], pentrpy[i1 + 1], ptemp[i1 + 1]);
               FOR i1 := 1 TO 20 DO
                  Line(pentadex[i1], ptempadex[i1],
                       pentadex[i1 + 1], ptempadex[i1 + 1]);
               FOR i1 := 27 TO 45 DO
                  Line(pentrpy[i1], ptemp[i1], pentrpy[i1 + 1], ptemp[i1 + 1]);
               Line(pentrpy[1], ptemp[1], pentrpy[46], ptemp[46]);

               SetColor(DarkGray);
               SetFillStyle(1, DarkGray);
               FillEllipse(xb2[i], yb2[i], b2radius, b2radius);
               Line(xp[1, i], yp[1, i], xp[2, i], yp[2, i]);
               Line(xp[3, i], yp[3, i], xp[4, i], yp[4, i]);
               Line(xp[1, i], yp[1, i], xp[4, i], yp[4, i]);
               Line(xp[2, i], yp[2, i], xp[3, i], yp[3, i]);
               SetLineStyle(solidln, 0, thickwidth);
               Line(xb, yb, xb2[i], yb2[i]);
               Line(xb2[i], yb2[i], xp[5, i], yp[5, i]);
               SetLineStyle(solidln, 0, normwidth);
               IF (i = 4) OR (i = 29) THEN
               BEGIN
                  Line(xivr[1, i], yivr[1, i], xivr[2, i], yivr[2, i]);
                  Line(xiv[1, i], yiv[1, i], xiv[2, i], yiv[2, i]);
               END;
               IF (i = 20) OR (i = 28) THEN
               BEGIN
                  Line(xevr[1, i], yevr[1, i], xevr[2, i], yevr[2, i]);
                  Line(xev[1, i], yev[1, i], xev[2, i], yev[2, i]);
               END;
               SetColor(white);
               Line(spmin, tpmin, spmin, tpmax);
               Line(spmax, tpmin, spmax, tpmax);
               Line(spmin, tpmin, spmax, tpmin);
               Line(spmin, tpmax, spmax, tpmax);
               Line(vpmin, ppmin, vpmin, ppmax);
               Line(vpmax, ppmin, vpmax, ppmax);
               Line(vpmin, ppmin, vpmax, ppmin);
               Line(vpmin, ppmax, vpmax, ppmax);
            END;  { IF ((quitf=false) AND (restart=false)) THEN DO }
         END;
      UNTIL (quitf OR restart);
   END;  { PROCEDURE Animation }

BEGIN  { PROGRAM Otto }

   CUPSinit;
   ABOUTPROGRAM;
   CUPSdone;
   tmax := 1280;
   cratio := 9.5;
   REPEAT  { until quitf = true }
      CUPSinit;
      restart := false;
      error := 0;
      ClearMuppetPort;
      ReadData;
      MouseBorders(0, GetMaxY - 15, GetMaxX, GetMaxY - 10);
      ClearMuppetPort;
      SetUpHotKeys(HotKeys);
      HotKeys.Display;
      SetUpMyMenu;
      vmax := 0.025;
      vmin := 0.0;
      pmax := 75.0;
      pmin := 0.0;
      tmin := 250;
      smax := 125.0;
      smin := 0.0;
      GetCycle(vmax, vmin, pmax, pmin, tmax, tmin, smax, smin, cratio,
         vol, temp, pres, entrpy, voladex, tempadex, presadex, entadex,
         efficiency);
      InitializeGrid ( deltat , deltatime , vol , temp , pres , entrpy ,
         voladex , tempadex , presadex , entadex , vmax , vmin , pmax ,
         pmin , tmax + 100 , tmin , smax , smin , efficiency , cratio ,
         vpmax , vpmin , ppmax , ppmin , tpmax , tpmin , spmax , spmin ,
         xleft , x1 , xright , ytop , ymiddle , ybottom , x , y , xsp ,
         ysp , xcc , ycc , xb , yb , bradius , xpc , ypc , xivlp , yivlp ,
         rivlp , xcvlp , ycvlp , rcvlp , xb2 , yb2 , b2radius , xp , yp ,
         xivr , yivr , xiv , yiv , xevr , yevr , xev , yev , pvol , ptemp ,
         ppres , pentrpy , pvoladex , ptempadex , ppresadex , pentadex ,
         xw , yw );
      Animation(deltat, deltatime, xsp, ysp, xcc, ycc, xb, yb, bradius,
         xpc, ypc, xivlp, yivlp, rivlp, xcvlp, ycvlp, rcvlp, xb2, yb2,
         b2radius, xp, yp, xivr, yivr, xiv, yiv, xevr, yevr, xev, yev,
         pvol, ptemp, ppres, pentrpy, pvoladex, ptempadex, ppresadex,
         pentadex, xw, yw, vpmax, vpmin, ppmax, ppmin, tpmax, tpmin, spmax,
         spmin, quitf, restart);
      CUPSDone;
   UNTIL (quitf);
END.  { PROGRAM Otto }