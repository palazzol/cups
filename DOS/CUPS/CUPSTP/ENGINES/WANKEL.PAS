   {*************************************************}
   {*************************************************}
   {**         Program Wankel    **}
   {**                  **}
   {** This program demonstrates an Ideal Gas   **}
   {** Wankel or Rotary Engine.        **}
   {**                  **}
   {** In the upper left corner of the screen is   **}
   {** a T-S plot, in the upper right corner is **}
   {** a P-V plot and in the lower portion is an   **}
   {** animation of the engine along with some     **}
   {** information about the cycle.         **}
   {**                  **}
   {**       Written by L. B. Spornick    **}
   {**       Applied Physics Laboratory      **}
   {**       The Johns Hopkins University **}
   {**                  **}
   {**          May 16, 1992     **}
   {**          ver 1.0, July, 1993    **}
   {**          ver 1.1, October,1993       **}
   {**          ver 1.2, January,1994       **}
   {**               ver 1.3  May, 1994          **}
   {**               ver 1.4  Feb, 1995          **}
   {**               ver 1.5  Apr, 1995          **}
   {**                  **}
   {*************************************************}
   {*************************************************}

PROGRAM Wankel;

   USES
      Graph, CUPSmupp, CUPSgui, Crt, cups;

   TYPE
      movement = ARRAY[1..16] OF INTEGER;
      xarray = ARRAY[1..21] OF INTEGER;
      yarray = ARRAY[1..20] OF INTEGER;
      sparray = ARRAY[1..4] OF INTEGER;
      castarray = ARRAY[1..24] OF REAL;
      castparray = ARRAY[1..24] OF INTEGER;
      abcarray = ARRAY[1..28] OF INTEGER;
      rabcarray = ARRAY[1..28] OF REAL;
      funarray = ARRAY[1..24] OF REAL;
      plotarray = ARRAY[1..24] OF INTEGER;
      expl = ARRAY[1..8] OF REAL;
      pexpl = ARRAY[1..8] OF INTEGER;
      exhaustv = ARRAY[1..5] OF REAL;
      pexhaustv = ARRAY[1..5] OF INTEGER;
      adex = ARRAY[1..21] OF REAL;
      padex = ARRAY[1..21] OF INTEGER;
      warray = ARRAY[1..6] OF INTEGER;
      b2array = ARRAY[1..32] OF INTEGER;
      pointtype6 = ARRAY[1..6] OF PointType;
      pointtype7 = ARRAY[1..7] OF PointType;
      pointtype8 = ARRAY[1..8] OF PointType;
      pointtype9 = ARRAY[1..9] OF PointType;
      pointtype10 = ARRAY[1..10] OF PointType;
      pointtype12 = ARRAY[1..12] OF PointType;
      pointtype13 = ARRAY[1..13] OF PointType;
      pointtype14 = ARRAY[1..14] OF PointType;
      pointtype15 = ARRAY[1..15] OF PointType;

   CONST
      slash:
      FillPatternType = ( $AA , $55 , $AA , $55 , $AA , $55 , $AA , $55 );
      deltatime: b2array = ( 50 , 50 , 50 , 50 , 75 , 75 , 75 , 75 , 75 ,
         75 , 75 , 75 , 600 , - 40 , - 40 , - 40 , - 40 , - 40 , - 40 ,
         - 40 , - 40 , 75 , 75 , 75 , 75 , 75 , 75 , 75 , 400 , 50 , 50 ,
         50 );
      a: abcarray = ( 3 , 2 , 1 , 24 , 23 , 22 , 21 , 20 , 19 , 18 , 17 ,
      16 , 15 , 14 , 14 , 13 , 13 , 12 , 11 , 10 , 9 , 8 , 8 , 7 , 7 , 6 ,
      5 , 4 );
      b: abcarray = ( 19 , 18 , 17 , 16 , 15 , 14 , 13 , 12 , 11 , 10 , 9 ,
      8 , 7 , 6 , 6 , 5 , 5 , 4 , 3 , 2 , 1 , 24 , 24 , 23 , 23 , 22 , 21 ,
      20 );
      c: abcarray = ( 11 , 10 , 9 , 8 , 7 , 6 , 5 , 4 , 3 , 2 , 1 , 24 , 23 ,
      22 , 22 , 21 , 21 , 20 , 19 , 18 , 17 , 16 , 16 , 15 , 15 , 14 , 13 ,
      12 );
      quitf:   BOOLEAN = false;
      restart: BOOLEAN = false;
      STEP:    BOOLEAN = false;
      DELTAT:  INTEGER = 200;

   VAR
      MyMenuOut: TMenu;
      HotKeys: THotKeys;
      key: BYTE;
      vol, temp, pres, entrpy: funarray;
      presexpl, tempexpl, entexpl: expl;
      presexhaustv, tempexhaustv, entexhaustv: exhaustv;
      presadex, voladex, tempadex, entadex: adex;
      vmax, vmin, smax, smin, tmax, tmin, pmax, pmin: REAL;
      vpmax, vpmin, spmax, spmin, tpmax, tpmin, ppmax, ppmin: INTEGER;
      cratio, efficiency: REAL;
      xleft, xright, x1, ytop, ymiddle, ybottom, i: INTEGER;
      xsg, ysg, rsg, rcm: INTEGER;
      deltax, error: INTEGER;
      x: xarray;
      y: yarray;
      xsp, ysp, xpc, ypc: sparray;
      xpcast, ypcast: castparray;
      xa, ya, xb, yb, xc, yc, xcm, ycm: abcarray;
      saa, sab, sac, eaa, eab, eac: abcarray;
      pvol, ptemp, ppres, pentrpy: plotarray;
      ppresexpl, ptempexpl, pentexpl: pexpl;
      ppresexhaustv, ptempexhaustv, pentexhaustv: pexhaustv;
      ppresadex, pvoladex, ptempadex, pentadex: padex;
      xw, yw: warray;
      compression7: pointtype6;
      explosion: pointtype7;
      compression6: pointtype8;
      intake1, exhaust4: pointtype9;
      power1: pointtype10;
      compression3, compression4, compression5, power2, power3: pointtype12;
      intake2, compression1, compression2, power4, power5, exhaust3: pointtype13;
      intake3, exhaust2: pointtype14;
      intake4, intake5, exhaustvent, exhaust1: pointtype15;
      F: text;

   PROCEDURE HELP1;
      VAR
         helpScr: HelpScrType;
   BEGIN
      helpScr[1] := '                                                 ';
      helpScr[2] := '          The Wankel or Rotary Engine            ';
      helpScr[3] := '                                                 ';
      helpScr[4] := '  The Wankel Engine represents an idealized      ';
      helpScr[5] := '  gasoline engine.  There are six processes:     ';
      helpScr[6] := '  1. Intake: A mixture of gasoline vapor and     ';
      helpScr[7] := '  air is drawn into the combustion chamber       ';
      helpScr[8] := '  by the movement of the rotor. (an isobaric     ';
      helpScr[9] := '  and isothermal process)                        ';
      helpScr[10] := '  2. Compression: The rotor moves, compressing   ';
      helpScr[11] := '  the gas mixture  (an adiabatic process)        ';
      helpScr[12] := '  3. Explosion: An electric spark ignites the    ';
      helpScr[13] := '  mixture.  The rotor does not move.   (an       ';
      helpScr[14] := '  isochoric process)                             ';
      helpScr[15] := '  4. Power: The hot gases cause the rotor to     ';
      helpScr[16] := '  move.  (an adiabatic process)                  ';
      helpScr[17] := '  5. Vent Exhaust: A drop in pressure and        ';
      helpScr[18] := '  temperature caused by the quasistatic          ';
      helpScr[19] := '  ejection of heat due to the contact of the     ';
      helpScr[20] := '  combustion gases with the surrounding. (an     ';
      helpScr[21] := '  isochoric process)                             ';
      helpScr[22] := '  6. Exhaust: The rotor moves, pushing the       ';
      helpScr[23] := '  remaining gases out of the chamber. (an        ';
      helpScr[24] := '  isobaric and isothermal process)               ';
      helpScr[25] := '                                                 ';
      HELP(helpScr);
   END;  {  PROCEDURE HELP1  }

   PROCEDURE ABOUTPROGRAM;
      VAR
         helpScr: HelpScrType;
   BEGIN
      helpScr[1] := '                                                 ';
      helpScr[2] := '                                                 ';
      helpScr[3] := '               The Wankel Engine                 ';
      helpScr[4] := '                                                 ';
      helpScr[5] := '                       by                        ';
      helpScr[6] := '                                                 ';
      helpScr[7] := '                  L.B. Spornick                  ';
      helpScr[8] := '            Applied Physic Laboratory            ';
      helpScr[9] := '           The Johns Hopkins University          ';
      helpScr[10] := '                                                 ';
      helpScr[11] := '             Version 1.5, Apr, 1995              ';
      helpScr[12] := '               Copyright (c) 1995                ';
      helpScr[13] := '                                                 ';
      helpScr[14] := '  This program models a Wankel or Rotary engine. ';
      helpScr[15] := '  As the engine moves through its cycle, the     ';
      helpScr[16] := '  corresponding points on the Temperature vs     ';
      helpScr[17] := '  Entropy (T vs S) plot and the Pressures vs     ';
      helpScr[18] := '  Volume plot (P vs V) are indicated.            ';
      helpScr[19] := '                                                 ';
      helpScr[20] := '                                                 ';
      helpScr[21] := '                                                 ';
      helpScr[22] := '                                                 ';
      helpScr[23] := '                                                 ';
      helpScr[24] := '                                                 ';
      helpScr[25] := '                                                 ';
      HELP(helpScr);
   END;   {  PROCEDURE ABOUTPROGRAM  }

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
         LoadLine('                 The Wankel Engine                  ');
         LoadLine('  The following program demonstrates a four stroke, ');
         LoadLine('  Ideal Gas Wankel Engine.                          ');
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

   PROCEDURE HandleHotKeys (key: BYTE; VAR deltat: INTEGER;
             VAR STEP, restart: BOOLEAN);
   BEGIN
      CASE key OF
         1: 
            Help1;
         2: 
            STEP := NOT STEP;  { F3 - Step }
         3:
         BEGIN      { F5 - Slower }
            deltat := round(deltat*1.3);
            IF (deltat > 1500) THEN  begin
               deltat := 1500;
               beep;
            end;
         END;
         4:
         BEGIN      { F6 - Faster }
            deltat := round(deltat/1.3);
            IF (deltat < 20) THEN begin
               deltat := 20;
               beep;
            end;
         END;
         5: 
         BEGIN  { F10 - Menu }
            MouseBorders(0, 0, GetMaxX, GetMaxY);
            IF (MyMenuOut.chosen) THEN
               HandleMenu(restart, quitf);
         END;
      END;  { CASE }
   END;  { PROCEDURE HandleHotKeys }

   PROCEDURE GetCycle (vmax, vmin, pmax, pmin, tmax, tmin, smax, smin,
             cratio: REAL; VAR vol, temp, pres, entrpy: funarray;
             VAR tempexpl, presexpl, entexpl: expl;
             VAR tempexhaustv, presexhaustv, entexhaustv: exhaustv;
             VAR tempadex, presadex, entadex, voladex: adex;
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
         i, i1, error: INTEGER;
         deltatemp, deltavol, atm, cv, cp, gamma: REAL;
   BEGIN
      gamma := 1.4;
      cv := 2.5 * R;
      cp := cv * gamma;
      atm := 1.01325 * Power(10.0, 5, error);
       { intake and exhaust strokes data }
      vol[1] := 0.0098;
      pres[1] := 1.0;
      temp[1] := 300.0;
      entrpy[1] := 30.0;
      vol[2] := 0.0049;
      pres[2] := 1.0;
      temp[2] := 300.0;
      entrpy[2] := 15.0;
      vol[3] := 0.0000;
      pres[3] := 1.0;
      temp[3] := 300.0;
      entrpy[3] := 00.0;
      vol[4] := 0.0049;
      pres[4] := 1.0;
      temp[4] := 300.0;
      entrpy[4] := 15.0;
      vol[5] := 0.0098;
      pres[5] := 1.0;
      temp[5] := 300.0;
      entrpy[5] := 30.0;
      vol[6] := 0.0148;
      pres[6] := 1.0;
      temp[6] := 300.0;
      entrpy[6] := 45.0;
      vol[7] := 0.0197;
      pres[7] := 1.0;
      temp[7] := 300.0;
      entrpy[7] := 60.0;
      vol[8] := 0.0246;
      pres[8] := 1.0;
      temp[8] := 300.0;
      entrpy[8] := 75.0;
      vol[22] := 0.0246;
      pres[22] := 1.0;
      temp[22] := 300.0;
      entrpy[22] := 75.0;
      vol[23] := 0.0197;
      pres[23] := 1.0;
      temp[23] := 300.0;
      entrpy[23] := 60.0;
      vol[24] := 0.0148;
      pres[24] := 1.0;
      temp[24] := 300.0;
      entrpy[24] := 45.0;
       { adiabatic compression - compression stroke data }
      deltatemp := (Power(cratio, (gamma - 1.0), error) - 1.0) * temp[22] / 6.0;
      FOR i := 1 TO 7 DO
      BEGIN
         i1 := 22 - i;
         temp[i1] := temp[22] + (i - 1) * deltatemp;
         vol[i1] := vol[22] * Power((temp[22] / temp[i1]),
                    1.0 / (gamma - 1.0), error);
         pres[i1] := R * temp[i1] / (vol[i1] * atm);
         entrpy[i1] := entrpy[22];
      END;
       { isochoric expansion - explosion data }
      deltatemp := (tmax - temp[15]) / 7.0;
      FOR i := 1 TO 8 DO
      BEGIN
         i1 := 9 - i;
         tempexpl[i1] := temp[15] + (i - 1) * deltatemp;
         presexpl[i1] := pres[15] * tempexpl[i1] / temp[15];
         entexpl[i1] := entrpy[15] + cv * ln(tempexpl[i1] / temp[15]);
      END;
      vol[14] := vol[15];
      pres[14] := presexpl[1];
      temp[14] := tempexpl[1];
      entrpy[14] := entexpl[1];
       { adiabatic expansion - power stroke }
      deltavol := (vol[22] - vol[14]) / 5.0;
      FOR i := 1 TO 5 DO
      BEGIN
         i1 := 14 - i;
         vol[i1] := vol[14] + (i) * deltavol;
         temp[i1] := temp[14] * Power((vol[14] / vol[i1]),
                     (gamma - 1.0), error);
         pres[i1] := R * temp[i1] / (atm * vol[i1]);
         entrpy[i1] := entrpy[14];
      END;
      deltavol := (vol[22] - vol[14]) / 20.0;
      FOR i := 1 TO 21 DO
      BEGIN
         voladex[i] := vol[14] + (i - 1) * deltavol;
         tempadex[i] := temp[14] * Power((vol[14] / voladex[i]),
                        (gamma - 1.0), error);
         presadex[i] := R * tempadex[i] / (atm * voladex[i]);
         entadex[i] := entrpy[14];
      END;
       { isochoric process - valve exhaust }
      deltatemp := (temp[22] - temp[9]) / 4.0;
      FOR i := 1 TO 5 DO
      BEGIN
         i1 := 6 - i;
         tempexhaustv[i1] := temp[9] + (i - 1) * deltatemp;
         presexhaustv[i1] := pres[9] * tempexhaustv[i1] / temp[9];
         entexhaustv[i1] := entrpy[9] + cv * ln(tempexhaustv[i1] / temp[9]);
      END;
      efficiency := 1.0 - (temp[9] - temp[22]) / (temp[14] - temp[15]);
   END;  { PROCEDURE GetCycle }

   PROCEDURE InitialGasDisplay (xpcast, ypcast: castparray;
      VAR compression7: pointtype6; VAR explosion: pointtype7;
      VAR compression6: pointtype8; VAR intake1, exhaust4: pointtype9;
      VAR power1: pointtype10;
      VAR compression3, compression4, compression5, power2,
      power3: pointtype12; VAR intake2, compression1, compression2,
      power4, power5, exhaust3: pointtype13; VAR intake3, exhaust2:
      pointtype14; VAR intake4, intake5, exhaustvent, exhaust1: pointtype15);

    {*******************************************************}
    {  PROCEDURE InitialGasDisplay defines the part of the  }
    {  screen where the gas is displayed as a function of   }
    {  of the cycle                 }
    {*******************************************************}

   BEGIN
     { intake1 }
      intake1[1].x := xpcast[5] - 2;
      intake1[1].y := ypcast[5] - 14;
      intake1[2].x := xpcast[5] - 2;
      intake1[2].y := ypcast[5] + 1;
      intake1[3].x := xpcast[4] - 2;
      intake1[3].y := ypcast[4] + 1;
      intake1[4].x := xpcast[3] - 2;
      intake1[4].y := ypcast[3] + 1;
      intake1[5].x := xpcast[2] - 5;
      intake1[5].y := ypcast[2] - 3;
      intake1[6].x := (xpcast[2] + xpcast[6]) DIV 2 + 7;
      intake1[6].y := (ypcast[2] + ypcast[6]) DIV 2 - 1;
      intake1[7].x := xpcast[6] + 5;
      intake1[7].y := ypcast[6] + 2;
      intake1[8].x := xpcast[6] + 5;
      intake1[8].y := ypcast[6] - 14;
      intake1[9].x := xpcast[5] - 2;
      intake1[9].y := ypcast[5] - 14;
     { intake2 }
      intake2[1].x := xpcast[5] - 1;
      intake2[1].y := ypcast[5] - 14;
      intake2[2].x := xpcast[5] - 1;
      intake2[2].y := ypcast[5] + 1;
      intake2[3].x := xpcast[4] - 1;
      intake2[3].y := ypcast[4] + 1;
      intake2[4].x := xpcast[3] - 1;
      intake2[4].y := ypcast[3] + 1;
      intake2[5].x := xpcast[2] - 1;
      intake2[5].y := ypcast[2] + 1;
      intake2[6].x := xpcast[1] - 1;
      intake2[6].y := ypcast[1] - 1;
      intake2[7].x := (3 * xpcast[1] + xpcast[7]) DIV 4 + 8;
      intake2[7].y := (3 * ypcast[1] + ypcast[7]) DIV 4 - 1;
      intake2[8].x := (xpcast[2] + xpcast[7]) DIV 2 + 5;
      intake2[8].y := (ypcast[2] + ypcast[7]) DIV 2;
      intake2[9].x := (xpcast[2] + 3 * xpcast[7]) DIV 4 + 1;
      intake2[9].y := (ypcast[2] + 3 * ypcast[7]) DIV 4 - 1;
      intake2[10].x := xpcast[7] + 5;
      intake2[10].y := ypcast[7] + 1;
      intake2[11].x := xpcast[6] + 1;
      intake2[11].y := ypcast[6] + 1;
      intake2[12].x := xpcast[6] + 1;
      intake2[12].y := ypcast[6] - 14;
      intake2[13].x := xpcast[5] - 1;
      intake2[13].y := ypcast[5] - 14;
     { intake3 }
      intake3[1].x := xpcast[5] - 1;
      intake3[1].y := ypcast[5] - 14;
      intake3[2].x := xpcast[5] - 1;
      intake3[2].y := ypcast[5] + 1;
      intake3[3].x := xpcast[4] - 1;
      intake3[3].y := ypcast[4] + 1;
      intake3[4].x := xpcast[3] - 1;
      intake3[4].y := ypcast[3] + 1;
      intake3[5].x := xpcast[2] - 1;
      intake3[5].y := ypcast[2] + 1;
      intake3[6].x := xpcast[1] - 1;
      intake3[6].y := ypcast[1] + 1;
      intake3[7].x := xpcast[24] - 1;
      intake3[7].y := ypcast[24] - 1;
      intake3[8].x := (3 * xpcast[24] + xpcast[7]) DIV 4 + 10;
      intake3[8].y := (3 * ypcast[24] + ypcast[7]) DIV 4 - 1;
      intake3[9].x := (xpcast[24] + xpcast[7]) DIV 2 + 17;
      intake3[9].y := (ypcast[24] + ypcast[7]) DIV 2 - 1;
      intake3[10].x := (xpcast[24] + 3 * xpcast[7]) DIV 4 + 7;
      intake3[10].y := (ypcast[24] + 3 * ypcast[7]) DIV 4 - 1;
      intake3[11].x := xpcast[7] + 1;
      intake3[11].y := ypcast[7] + 1;
      intake3[12].x := xpcast[6] + 1;
      intake3[12].y := ypcast[6] + 1;
      intake3[13].x := xpcast[6] + 1;
      intake3[13].y := ypcast[6] - 14;
      intake3[14].x := xpcast[5] - 1;
      intake3[14].y := ypcast[5] - 14;
     { intake4 }
      intake4[1].x := xpcast[5] - 1;
      intake4[1].y := ypcast[5] - 14;
      intake4[2].x := xpcast[5] - 1;
      intake4[2].y := ypcast[5] + 1;
      intake4[3].x := xpcast[4] - 1;
      intake4[3].y := ypcast[4] + 1;
      intake4[4].x := xpcast[3] - 1;
      intake4[4].y := ypcast[3] + 1;
      intake4[5].x := xpcast[2] - 1;
      intake4[5].y := ypcast[2] + 1;
      intake4[6].x := xpcast[1] - 1;
      intake4[6].y := ypcast[1] + 1;
      intake4[7].x := xpcast[24] - 1;
      intake4[7].y := ypcast[24] - 1;
      intake4[8].x := xpcast[23] + 1;
      intake4[8].y := ypcast[23] - 1;
      intake4[9].x := (3 * xpcast[23] + xpcast[7]) DIV 4 + 14;
      intake4[9].y := (3 * ypcast[23] + ypcast[7]) DIV 4;
      intake4[10].x := (xpcast[23] + xpcast[7]) DIV 2 + 22;
      intake4[10].y := (ypcast[23] + ypcast[7]) DIV 2;
      intake4[11].x := (xpcast[23] + 3 * xpcast[7]) DIV 4 + 13;
      intake4[11].y := (ypcast[23] + 3 * ypcast[7]) DIV 4;
      intake4[12].x := xpcast[7] + 1;
      intake4[12].y := ypcast[7] + 1;
      intake4[13].x := xpcast[6] + 1;
      intake4[13].y := ypcast[6] + 1;
      intake4[14].x := xpcast[6] + 1;
      intake4[14].y := ypcast[6] - 14;
      intake4[15].x := xpcast[5] - 1;
      intake4[15].y := ypcast[5] - 14;
     { intake5 }
      intake5[1].x := xpcast[5] - 1;
      intake5[1].y := ypcast[5] - 14;
      intake5[2].x := xpcast[5] - 1;
      intake5[2].y := ypcast[5] + 1;
      intake5[3].x := xpcast[4] - 1;
      intake5[3].y := ypcast[4] + 1;
      intake5[4].x := xpcast[3] - 1;
      intake5[4].y := ypcast[3] + 1;
      intake5[5].x := xpcast[2] - 1;
      intake5[5].y := ypcast[2] + 1;
      intake5[6].x := xpcast[1] - 1;
      intake5[6].y := ypcast[1] + 1;
      intake5[7].x := xpcast[24] - 1;
      intake5[7].y := ypcast[24] - 1;
      intake5[8].x := xpcast[23] - 1;
      intake5[8].y := ypcast[23] - 1;
      intake5[9].x := xpcast[22] - 1;
      intake5[9].y := ypcast[22] - 1;
      intake5[10].x := (3 * xpcast[22] + xpcast[6]) DIV 4 + 17;
      intake5[10].y := (3 * ypcast[22] + ypcast[6]) DIV 4 - 1;
      intake5[11].x := (xpcast[22] + xpcast[6]) DIV 2 + 20;
      intake5[11].y := (ypcast[22] + ypcast[6]) DIV 2 - 1;
      intake5[12].x := (xpcast[22] + 3 * xpcast[6]) DIV 4 + 15;
      intake5[12].y := (ypcast[22] + 3 * ypcast[6]) DIV 4 - 1;
      intake5[13].x := xpcast[6] + 1;
      intake5[13].y := ypcast[6] + 1;
      intake5[14].x := xpcast[6] + 1;
      intake5[14].y := ypcast[6] - 14;
      intake5[15].x := xpcast[5] - 1;
      intake5[15].y := ypcast[5] - 14;
     { compression1 }
      compression1[1].x := xpcast[5] + 1;
      compression1[1].y := ypcast[5] + 1;
      compression1[2].x := xpcast[4] - 1;
      compression1[2].y := ypcast[4] + 1;
      compression1[3].x := xpcast[3] - 1;
      compression1[3].y := ypcast[3] + 1;
      compression1[4].x := xpcast[2] - 1;
      compression1[4].y := ypcast[2] + 1;
      compression1[5].x := xpcast[1] - 1;
      compression1[5].y := ypcast[1];
      compression1[6].x := xpcast[24] - 1;
      compression1[6].y := ypcast[24] - 1;
      compression1[7].x := xpcast[23] - 1;
      compression1[7].y := ypcast[23] - 1;
      compression1[8].x := xpcast[22] - 1;
      compression1[8].y := ypcast[22] - 1;
      compression1[9].x := xpcast[21] - 1;
      compression1[9].y := ypcast[21] - 1;
      compression1[10].x := (3 * xpcast[21] + xpcast[5]) DIV 4 + 10;
      compression1[10].y := (3 * ypcast[21] + ypcast[5]) DIV 4;
      compression1[11].x := (xpcast[21] + xpcast[5]) DIV 2 + 20;
      compression1[11].y := (ypcast[21] + ypcast[5]) DIV 2;
      compression1[12].x := (xpcast[21] + 3 * xpcast[5]) DIV 4 + 10;
      compression1[12].y := (ypcast[21] + 3 * ypcast[5]) DIV 4;
      compression1[13].x := xpcast[5] + 1;
      compression1[13].y := ypcast[5] + 1;
     { compression2 }
      compression2[1].x := xpcast[4] - 1;
      compression2[1].y := ypcast[4] + 1;
      compression2[2].x := xpcast[3] - 1;
      compression2[2].y := ypcast[3] + 1;
      compression2[3].x := xpcast[2] - 1;
      compression2[3].y := ypcast[2] + 1;
      compression2[4].x := xpcast[1] - 1;
      compression2[4].y := ypcast[1];
      compression2[5].x := xpcast[24] - 1;
      compression2[5].y := ypcast[24] - 1;
      compression2[6].x := xpcast[23] - 1;
      compression2[6].y := ypcast[23] - 1;
      compression2[7].x := xpcast[22] - 1;
      compression2[7].y := ypcast[22] - 1;
      compression2[8].x := xpcast[21] - 1;
      compression2[8].y := ypcast[21] - 1;
      compression2[9].x := xpcast[20] - 1;
      compression2[9].y := ypcast[20] - 1;
      compression2[10].x := (3 * xpcast[20] + xpcast[4]) DIV 4 + 10;
      compression2[10].y := (3 * ypcast[20] + ypcast[4]) DIV 4;
      compression2[11].x := (xpcast[20] + xpcast[4]) DIV 2 + 20;
      compression2[11].y := (ypcast[20] + ypcast[4]) DIV 2;
      compression2[12].x := (xpcast[20] + 3 * xpcast[4]) DIV 4 + 10;
      compression2[12].y := (ypcast[20] + 3 * ypcast[4]) DIV 4;
      compression2[13].x := xpcast[4] - 1;
      compression2[13].y := ypcast[4] + 1;
     { compression3 }
      compression3[1].x := xpcast[3] - 1;
      compression3[1].y := ypcast[3] + 1;
      compression3[2].x := xpcast[2] - 1;
      compression3[2].y := ypcast[2] + 1;
      compression3[3].x := xpcast[1] - 1;
      compression3[3].y := ypcast[1];
      compression3[4].x := xpcast[24] - 1;
      compression3[4].y := ypcast[24] - 1;
      compression3[5].x := xpcast[23] - 1;
      compression3[5].y := ypcast[23] - 1;
      compression3[6].x := xpcast[22] - 1;
      compression3[6].y := ypcast[22] - 1;
      compression3[7].x := xpcast[21] - 1;
      compression3[7].y := ypcast[21] - 1;
      compression3[8].x := xpcast[20] - 10;
      compression3[8].y := ypcast[20] - 1;
      compression3[9].x := (3 * xpcast[20] + xpcast[3]) DIV 4 + 5;
      compression3[9].y := (3 * ypcast[20] + ypcast[3]) DIV 4;
      compression3[10].x := (xpcast[20] + xpcast[3]) DIV 2 + 20;
      compression3[10].y := (ypcast[20] + ypcast[3]) DIV 2;
      compression3[11].x := (xpcast[20] + 3 * xpcast[3]) DIV 4 + 10;
      compression3[11].y := (ypcast[20] + 3 * ypcast[3]) DIV 4;
      compression3[12].x := xpcast[3] - 1;
      compression3[12].y := ypcast[3] + 1;
     { compression4 }
      compression4[1].x := xpcast[2] - 1;
      compression4[1].y := ypcast[2] + 1;
      compression4[2].x := xpcast[1] - 1;
      compression4[2].y := ypcast[1] + 1;
      compression4[3].x := xpcast[24] - 1;
      compression4[3].y := ypcast[24];
      compression4[4].x := xpcast[23] - 1;
      compression4[4].y := ypcast[23] - 1;
      compression4[5].x := xpcast[22] - 1;
      compression4[5].y := ypcast[22] - 1;
      compression4[6].x := xpcast[21] - 1;
      compression4[6].y := ypcast[21] - 1;
      compression4[7].x := xpcast[20] - 1;
      compression4[7].y := ypcast[20] - 1;
      compression4[8].x := xpcast[7] - 2;
      compression4[8].y := ypcast[21] - 1;
      compression4[9].x := (3 * xpcast[7] + xpcast[2]) DIV 4 + 10;
      compression4[9].y := (3 * ypcast[21] + ypcast[2]) DIV 4;
      compression4[10].x := (xpcast[7] + xpcast[2]) DIV 2 + 15;
      compression4[10].y := (ypcast[21] + ypcast[2]) DIV 2;
      compression4[11].x := (xpcast[7] + 3 * xpcast[2]) DIV 4 + 10;
      compression4[11].y := (ypcast[21] + 3 * ypcast[2]) DIV 4;
      compression4[12].x := xpcast[2] - 1;
      compression4[12].y := ypcast[2] + 1;
     { compression5 }
      compression5[1].x := xpcast[1] - 1;
      compression5[1].y := ypcast[1] + 1;
      compression5[2].x := xpcast[24] - 1;
      compression5[2].y := ypcast[24] + 1;
      compression5[3].x := xpcast[23] - 1;
      compression5[3].y := ypcast[23];
      compression5[4].x := xpcast[22] - 1;
      compression5[4].y := ypcast[22] - 1;
      compression5[5].x := xpcast[21] - 1;
      compression5[5].y := ypcast[21] - 1;
      compression5[6].x := xpcast[20] - 1;
      compression5[6].y := ypcast[21] - 1;
      compression5[7].x := xpcast[7];
      compression5[7].y := ypcast[21] - 1;
      compression5[8].x := xpcast[7] - 2;
      compression5[8].y := ypcast[21] - 1;
      compression5[9].x := (3 * xpcast[7] + xpcast[1]) DIV 4 + 10;
      compression5[9].y := (3 * ypcast[21] + ypcast[1]) DIV 4;
      compression5[10].x := (xpcast[7] + xpcast[1]) DIV 2 + 15;
      compression5[10].y := (ypcast[21] + ypcast[1]) DIV 2;
      compression5[11].x := (xpcast[7] + 3 * xpcast[1]) DIV 4 + 10;
      compression5[11].y := (ypcast[21] + 3 * ypcast[1]) DIV 4;
      compression5[12].x := xpcast[1] - 1;
      compression5[12].y := ypcast[1] + 1;
     { compression6 }
      compression6[1].x := xpcast[24] - 1;
      compression6[1].y := ypcast[24];
      compression6[2].x := xpcast[23] - 1;
      compression6[2].y := ypcast[23] - 1;
      compression6[3].x := xpcast[22] - 1;
      compression6[3].y := ypcast[22] - 1;
      compression6[4].x := xpcast[21] - 1;
      compression6[4].y := ypcast[21] - 1;
      compression6[5].x := xpcast[20];
      compression6[5].y := ypcast[21] - 1;
      compression6[6].x := xpcast[7] - 7;
      compression6[6].y := ypcast[21] - 1;
      compression6[7].x := (xpcast[7] + xpcast[24]) DIV 2 + 20;
      compression6[7].y := (ypcast[21] + ypcast[24]) DIV 2;
      compression6[8].x := xpcast[24] - 1;
      compression6[8].y := ypcast[24] + 1;
     { compression7 }
      compression7[1].x := xpcast[23] - 1;
      compression7[1].y := ypcast[23];
      compression7[2].x := xpcast[22] - 1;
      compression7[2].y := ypcast[22] - 1;
      compression7[3].x := xpcast[21] - 1;
      compression7[3].y := ypcast[21] - 1;
      compression7[4].x := xpcast[20] - 1;
      compression7[4].y := ypcast[21] - 1;
      compression7[5].x := (xpcast[20] + xpcast[23]) DIV 2;
      compression7[5].y := (ypcast[21] + ypcast[23]) DIV 2;
      compression7[6].x := xpcast[23] - 1;
      compression7[6].y := ypcast[23] + 1;
     { explosion }
      explosion[1].x := xpcast[18] - 2;
      explosion[1].y := ypcast[16] - 2;
      explosion[2].x := xpcast[17] - 1;
      explosion[2].y := ypcast[16] - 2;
      explosion[3].x := xpcast[16] + 1;
      explosion[3].y := ypcast[16] - 2;
      explosion[4].x := xpcast[15] + 1;
      explosion[4].y := ypcast[15] - 1;
      explosion[5].x := xpcast[14] + 1;
      explosion[5].y := ypcast[14] + 1;
      explosion[6].x := (xpcast[14] + xpcast[18]) DIV 2;
      explosion[6].y := (ypcast[14] + ypcast[16]) DIV 2 + 10;
      explosion[7].x := xpcast[18] - 2;
      explosion[7].y := ypcast[16] - 2;
     { power1 }
      power1[1].x := xpcast[20] - 5;
      power1[1].y := ypcast[16] - 1;
      power1[2].x := xpcast[17];
      power1[2].y := ypcast[16] - 1;
      power1[3].x := xpcast[16] + 1;
      power1[3].y := ypcast[16] - 1;
      power1[4].x := xpcast[15] + 1;
      power1[4].y := ypcast[15] - 1;
      power1[5].x := xpcast[14] + 1;
      power1[5].y := ypcast[14] - 1;
      power1[6].x := xpcast[13] + 1;
      power1[6].y := ypcast[13] - 1;
      power1[7].x := (3 * xpcast[13] + xpcast[20]) DIV 4 - 10;
      power1[7].y := (3 * ypcast[13] + ypcast[16]) DIV 4;
      power1[8].x := (xpcast[13] + xpcast[20]) DIV 2 - 25;
      power1[8].y := (ypcast[13] + ypcast[16]) DIV 2;
      power1[9].x := (xpcast[13] + 3 * xpcast[20]) DIV 4 - 30;
      power1[9].y := (ypcast[13] + 3 * ypcast[16]) DIV 4;
      power1[10].x := xpcast[20] - 5;
      power1[10].y := ypcast[16] - 1;
     { power2 }
      power2[1].x := xpcast[19];
      power2[1].y := ypcast[16] - 1;
      power2[2].x := xpcast[18];
      power2[2].y := ypcast[16] - 1;
      power2[3].x := xpcast[17] + 1;
      power2[3].y := ypcast[16] - 1;
      power2[4].x := xpcast[16] + 1;
      power2[4].y := ypcast[16] - 1;
      power2[5].x := xpcast[15] + 1;
      power2[5].y := ypcast[15] - 1;
      power2[6].x := xpcast[14] + 1;
      power2[6].y := ypcast[14] - 1;
      power2[7].x := xpcast[13] + 1;
      power2[7].y := ypcast[13] - 1;
      power2[8].x := xpcast[12] + 1;
      power2[8].y := ypcast[12] + 1;
      power2[9].x := (3 * xpcast[12] + xpcast[19]) DIV 4;
      power2[9].y := (3 * ypcast[12] + ypcast[16]) DIV 4 + 18;
      power2[10].x := (xpcast[12] + xpcast[19]) DIV 2;
      power2[10].y := (ypcast[12] + ypcast[16]) DIV 2 + 20;
      power2[11].x := (xpcast[12] + 3 * xpcast[19]) DIV 4;
      power2[11].y := (ypcast[12] + 3 * ypcast[16]) DIV 4 + 10;
      power2[12].x := xpcast[19];
      power2[12].y := ypcast[16] - 1;
     { power3 }
      power3[1].x := xpcast[18];
      power3[1].y := ypcast[16] - 1;
      power3[2].x := xpcast[17];
      power3[2].y := ypcast[16] - 1;
      power3[3].x := xpcast[16] + 1;
      power3[3].y := ypcast[16] - 1;
      power3[4].x := xpcast[15] + 1;
      power3[4].y := ypcast[15] - 1;
      power3[5].x := xpcast[14] + 1;
      power3[5].y := ypcast[14] - 1;
      power3[6].x := xpcast[13] + 1;
      power3[6].y := ypcast[13] - 1;
      power3[7].x := xpcast[12] + 1;
      power3[7].y := ypcast[12] - 1;
      power3[8].x := xpcast[11] + 1;
      power3[8].y := ypcast[11] + 1;
      power3[9].x := (3 * xpcast[11] + xpcast[18]) DIV 4;
      power3[9].y := (3 * ypcast[11] + ypcast[16]) DIV 4 + 25;
      power3[10].x := (xpcast[11] + xpcast[18]) DIV 2;
      power3[10].y := (ypcast[11] + ypcast[16]) DIV 2 + 28;
      power3[11].x := (xpcast[11] + 3 * xpcast[18]) DIV 4;
      power3[11].y := (ypcast[11] + 3 * ypcast[16]) DIV 4 + 8;
      power3[12].x := xpcast[18];
      power3[12].y := ypcast[16] - 1;
     { power4 }
      power4[1].x := xpcast[18];
      power4[1].y := ypcast[16] - 1;
      power4[2].x := xpcast[17];
      power4[2].y := ypcast[16] - 1;
      power4[3].x := xpcast[16] + 1;
      power4[3].y := ypcast[16] - 1;
      power4[4].x := xpcast[15] + 1;
      power4[4].y := ypcast[15] - 1;
      power4[5].x := xpcast[14] + 1;
      power4[5].y := ypcast[14] - 1;
      power4[6].x := xpcast[13] + 1;
      power4[6].y := ypcast[13] - 1;
      power4[7].x := xpcast[12] + 1;
      power4[7].y := ypcast[12] + 1;
      power4[8].x := xpcast[11] + 1;
      power4[8].y := ypcast[11] + 1;
      power4[9].x := xpcast[10] - 1;
      power4[9].y := ypcast[10] + 1;
      power4[10].x := (3 * xpcast[10] + xpcast[18]) DIV 4 - 8;
      power4[10].y := (3 * ypcast[10] + ypcast[16]) DIV 4 + 25;
      power4[11].x := (xpcast[10] + xpcast[18]) DIV 2 - 13;
      power4[11].y := (ypcast[10] + ypcast[16]) DIV 2 + 28;
      power4[12].x := (xpcast[10] + 3 * xpcast[18]) DIV 4 - 8;
      power4[12].y := (ypcast[10] + 3 * ypcast[16]) DIV 4 + 8;
      power4[13].x := xpcast[18];
      power4[13].y := ypcast[16] - 1;
     { power5 }
      power5[1].x := xpcast[17];
      power5[1].y := ypcast[16] - 1;
      power5[2].x := xpcast[16];
      power5[2].y := ypcast[16] - 1;
      power5[3].x := xpcast[15] + 1;
      power5[3].y := ypcast[15] - 1;
      power5[4].x := xpcast[14] + 1;
      power5[4].y := ypcast[14] - 1;
      power5[5].x := xpcast[13] + 1;
      power5[5].y := ypcast[13] - 1;
      power5[6].x := xpcast[12] + 1;
      power5[6].y := ypcast[12] - 1;
      power5[7].x := xpcast[11] + 1;
      power5[7].y := ypcast[11] - 1;
      power5[8].x := xpcast[10] + 1;
      power5[8].y := ypcast[10] + 1;
      power5[9].x := xpcast[9] - 1;
      power5[9].y := ypcast[9] + 1;
      power5[10].x := (3 * xpcast[9] + xpcast[18]) DIV 4 - 18;
      power5[10].y := (3 * ypcast[9] + ypcast[16]) DIV 4;
      power5[11].x := (xpcast[9] + xpcast[18]) DIV 2 - 30;
      power5[11].y := (ypcast[9] + ypcast[16]) DIV 2;
      power5[12].x := (xpcast[9] + 3 * xpcast[18]) DIV 4 - 26;
      power5[12].y := (ypcast[9] + 3 * ypcast[16]) DIV 4;
      power5[13].x := xpcast[17];
      power5[13].y := ypcast[16] - 1;
     { exhaustvent }
      exhaustvent[1].x := xpcast[16];
      exhaustvent[1].y := ypcast[16] - 1;
      exhaustvent[2].x := xpcast[15] + 1;
      exhaustvent[2].y := ypcast[15] - 1;
      exhaustvent[3].x := xpcast[14] + 1;
      exhaustvent[3].y := ypcast[14] - 1;
      exhaustvent[4].x := xpcast[13] + 1;
      exhaustvent[4].y := ypcast[13] - 1;
      exhaustvent[5].x := xpcast[12] + 1;
      exhaustvent[5].y := ypcast[12] + 1;
      exhaustvent[6].x := xpcast[11] + 1;
      exhaustvent[6].y := ypcast[11] + 1;
      exhaustvent[7].x := xpcast[10] + 1;
      exhaustvent[7].y := ypcast[10] + 1;
      exhaustvent[8].x := xpcast[9] + 1;
      exhaustvent[8].y := ypcast[9] + 1;
      exhaustvent[9].x := xpcast[9] + 1;
      exhaustvent[9].y := ypcast[9] - 14;
      exhaustvent[10].x := xpcast[8] - 1;
      exhaustvent[10].y := ypcast[8] - 14;
      exhaustvent[11].x := xpcast[8] - 1;
      exhaustvent[11].y := ypcast[8] - 1;
      exhaustvent[12].x := (3 * xpcast[8] + xpcast[16]) DIV 4 - 10;
      exhaustvent[12].y := (3 * ypcast[8] + ypcast[16]) DIV 4;
      exhaustvent[13].x := (xpcast[8] + xpcast[16]) DIV 2 - 20;
      exhaustvent[13].y := (ypcast[8] + ypcast[16]) DIV 2;
      exhaustvent[14].x := (xpcast[8] + 3 * xpcast[16]) DIV 4 - 10;
      exhaustvent[14].y := (ypcast[8] + 3 * ypcast[16]) DIV 4;
      exhaustvent[15].x := xpcast[16];
      exhaustvent[15].y := ypcast[16] - 1;
     { exhaust1 }
      exhaust1[1].x := xpcast[15] + 5;
      exhaust1[1].y := ypcast[15] - 1;
      exhaust1[2].x := xpcast[14] + 1;
      exhaust1[2].y := ypcast[14] - 1;
      exhaust1[3].x := xpcast[13] + 1;
      exhaust1[3].y := ypcast[13] - 1;
      exhaust1[4].x := xpcast[12] + 1;
      exhaust1[4].y := ypcast[12] - 1;
      exhaust1[5].x := xpcast[11] + 1;
      exhaust1[5].y := ypcast[11] + 1;
      exhaust1[6].x := xpcast[10] + 1;
      exhaust1[6].y := ypcast[10] + 1;
      exhaust1[7].x := xpcast[9] + 1;
      exhaust1[7].y := ypcast[9] + 1;
      exhaust1[8].x := xpcast[9] + 1;
      exhaust1[8].y := ypcast[9] - 14;
      exhaust1[9].x := xpcast[8] - 1;
      exhaust1[9].y := ypcast[8] - 14;
      exhaust1[10].x := xpcast[8] - 1;
      exhaust1[10].y := ypcast[8] + 1;
      exhaust1[11].x := xpcast[7] - 1;
      exhaust1[11].y := ypcast[7] + 1;
      exhaust1[12].x := (3 * xpcast[7] + xpcast[15]) DIV 4 - 12;
      exhaust1[12].y := (3 * ypcast[7] + ypcast[15]) DIV 4;
      exhaust1[13].x := (xpcast[7] + xpcast[15]) DIV 2 - 25;
      exhaust1[13].y := (ypcast[7] + ypcast[15]) DIV 2;
      exhaust1[14].x := (xpcast[7] + 3 * xpcast[15]) DIV 4 - 12;
      exhaust1[14].y := (ypcast[7] + 3 * ypcast[15]) DIV 4;
      exhaust1[15].x := xpcast[15] + 5;
      exhaust1[15].y := ypcast[15] - 1;
     { exhaust2 }
      exhaust2[1].x := xpcast[14] + 5;
      exhaust2[1].y := ypcast[14] - 1;
      exhaust2[2].x := xpcast[13] + 1;
      exhaust2[2].y := ypcast[13] - 1;
      exhaust2[3].x := xpcast[12] + 1;
      exhaust2[3].y := ypcast[12] - 1;
      exhaust2[4].x := xpcast[11] + 1;
      exhaust2[4].y := ypcast[11] - 1;
      exhaust2[5].x := xpcast[10] + 1;
      exhaust2[5].y := ypcast[10] + 1;
      exhaust2[6].x := xpcast[9] + 1;
      exhaust2[6].y := ypcast[9] + 1;
      exhaust2[7].x := xpcast[9] + 1;
      exhaust2[7].y := ypcast[9] - 14;
      exhaust2[8].x := xpcast[8] - 1;
      exhaust2[8].y := ypcast[8] - 14;
      exhaust2[9].x := xpcast[8] - 1;
      exhaust2[9].y := ypcast[8] + 1;
      exhaust2[10].x := xpcast[7] - 1;
      exhaust2[10].y := ypcast[7] + 1;
      exhaust2[11].x := (3 * xpcast[7] + xpcast[14]) DIV 4 - 11;
      exhaust2[11].y := (3 * ypcast[7] + ypcast[14]) DIV 4;
      exhaust2[12].x := (xpcast[7] + xpcast[14]) DIV 2 - 17;
      exhaust2[12].y := (ypcast[7] + ypcast[14]) DIV 2;
      exhaust2[13].x := (xpcast[7] + 3 * xpcast[14]) DIV 4 - 11;
      exhaust2[13].y := (ypcast[7] + 3 * ypcast[14]) DIV 4;
      exhaust2[14].x := xpcast[14] + 5;
      exhaust2[14].y := ypcast[14] - 1;
     { exhaust3 }
      exhaust3[1].x := xpcast[13] + 3;
      exhaust3[1].y := ypcast[13] + 2;
      exhaust3[2].x := xpcast[12] + 3;
      exhaust3[2].y := ypcast[12] + 2;
      exhaust3[3].x := xpcast[11] + 3;
      exhaust3[3].y := ypcast[11] + 2;
      exhaust3[4].x := xpcast[10] + 3;
      exhaust3[4].y := ypcast[10] + 2;
      exhaust3[5].x := xpcast[9] + 1;
      exhaust3[5].y := ypcast[9] + 1;
      exhaust3[6].x := xpcast[9] + 1;
      exhaust3[6].y := ypcast[9] - 14;
      exhaust3[7].x := xpcast[8] - 1;
      exhaust3[7].y := ypcast[8] - 14;
      exhaust3[8].x := xpcast[8] - 1;
      exhaust3[8].y := ypcast[8] + 1;
      exhaust3[9].x := xpcast[7] - 1;
      exhaust3[9].y := ypcast[7] + 1;
      exhaust3[10].x := (3 * xpcast[7] + xpcast[13]) DIV 4 - 5;
      exhaust3[10].y := (3 * ypcast[7] + ypcast[13]) DIV 4;
      exhaust3[11].x := (xpcast[7] + xpcast[13]) DIV 2 - 15;
      exhaust3[11].y := (ypcast[7] + ypcast[13]) DIV 2;
      exhaust3[12].x := (xpcast[7] + 3 * xpcast[13]) DIV 4 - 5;
      exhaust3[12].y := (ypcast[7] + 3 * ypcast[13]) DIV 4;
      exhaust3[13].x := xpcast[13] + 3;
      exhaust3[13].y := ypcast[13] + 2;
     { exhaust4 }
      exhaust4[1].x := xpcast[12] + 1;
      exhaust4[1].y := ypcast[12] - 1;
      exhaust4[2].x := xpcast[11] + 1;
      exhaust4[2].y := ypcast[11] - 1;
      exhaust4[3].x := xpcast[10] + 1;
      exhaust4[3].y := ypcast[10] + 1;
      exhaust4[4].x := xpcast[9] + 1;
      exhaust4[4].y := ypcast[9] + 1;
      exhaust4[5].x := xpcast[9] + 1;
      exhaust4[5].y := ypcast[9] - 14;
      exhaust4[6].x := xpcast[8] - 1;
      exhaust4[6].y := ypcast[8] - 14;
      exhaust4[7].x := xpcast[8] - 1;
      exhaust4[7].y := ypcast[8] + 1;
      exhaust4[8].x := (xpcast[8] + xpcast[12]) DIV 2 - 5;
      exhaust4[8].y := (ypcast[8] + ypcast[12]) DIV 2;
      exhaust4[9].x := xpcast[12] + 1;
      exhaust4[9].y := ypcast[12] - 1;
   END;  { PROCEDURE InitialGasDisplay }

   PROCEDURE InitializeGrid(a,b,c : abcarray;
                            vmax,vmin,pmax,pmin,tmax,tmin,smax,smin,
                            cratio,efficiency : REAL;
                            VAR deltax : INTEGER;
			    vol,temp,pres,entrpy : funarray;
                            presexpl,tempexpl,entexpl : expl;
                            presexhaustv,tempexhaustv,entexhaustv :
                             exhaustv;
                            presadex,tempadex,entadex,voladex : adex;
			    VAR vpmax,vpmin,ppmax,ppmin,tpmax,tpmin,
				spmax,spmin,
				xleft,x1,xright,
				ytop,ymiddle,ybottom : INTEGER;
			    VAR x : xarray;
			    VAR y : yarray;
			    VAR xsg,ysg,rsg : INTEGER;
			    VAR xsp,ysp : sparray;
			    VAR pvol,ptemp,ppres,pentrpy : plotarray;
                            VAR ppresexpl,ptempexpl,pentexpl : pexpl;
                            VAR ppresexhaustv,ptempexhaustv,pentexhaustv
                                : pexhaustv;
                            VAR ppresadex,ptempadex,pentadex,pvoladex 
                                : padex;
			    VAR xpcast,ypcast : castparray;
			    VAR xa,ya,xb,yb,xc,yc,xcm,ycm : abcarray;
			    VAR saa,sab,sac,eaa,eab,eac : abcarray;
			    VAR rcm : INTEGER;
			    VAR xw,yw : warray;
			    VAR compression7 : pointtype6;
			    VAR explosion : pointtype7;
			    VAR compression6 : pointtype8;
			    VAR intake1,exhaust4 : pointtype9;
			    VAR power1 : pointtype10;
			    VAR compression3,compression4,compression5,
				  power2,power3 : pointtype12;
			    VAR intake2,compression1,compression2,power4,
				  power5,exhaust3 :  pointtype13;
			    VAR intake3,exhaust2 : pointtype14;
			    VAR intake4,intake5,exhaustvent,exhaust1
                                  : pointtype15);

    {****************************************************}
    {**        PROCEDURE InitializeGrid         **}
    {** sets up the screen and determines the plot     **}
    {** coordinate systems.               **}
    {****************************************************}

      CONST
         xcast: castarray = ( 3.50000 , 3.25133 , 2.59808 , 1.76777 ,
            1.00000 , 0.42290 , 0.00000 , - 0.42290 , - 1.00000 , - 1.76777
            , - 2.59808 , - 3.25133 , - 3.50000 , - 3.25133 , - 2.59808 ,
            - 1.76777 , - 1.00000 , - 0.42290 , 0.00000 , 0.42290 ,
            1.00000 , 1.76777 , 2.59808 , 3.25133 );
         ycast: castarray = ( 0.00000 , 1.13001 , 2.00000 , 2.47487 ,
            2.59808 , 2.54422 , 2.50000 , 2.54422 , 2.59808 , 2.47487 ,
            2.00000 , 1.13001 , 0.00000 , - 1.13001 , - 2.00000 , - 2.47487
            , - 2.59808 , - 2.54422 , - 2.50000 , - 2.54422 , - 2.59808 ,
            - 2.47487 , - 2.00000 , - 1.13001 );
         cmx:   castarray = ( 2.00000 , 1.41421 , 0.00000 , - 1.41421 ,
            - 2.00000 , - 1.41421 , 0.00000 , 1.41421 , 2.00000 , 1.41421 ,
            0.00000 , - 1.41421 , - 2.00000 , - 1.41421 , 0.00000 , 1.41421
            , 2.00000 , 1.41421 , 0.00000 , - 1.41421 , - 2.00000 ,
            - 1.41421 , 0.00000 , 1.41421 );
         cmy:   castarray = ( 0.00000 , 1.41421 , 2.00000 , 1.41421 ,
            0.00000 , - 1.41421 , - 2.00000 , - 1.41421 , 0.00000 , 1.41421
            , 2.00000 , 1.41421 , 0.00000 , - 1.41421 , - 2.00000 ,
            - 1.41421 , 0.00000 , 1.41421 , 2.00000 , 1.41421 , 0.00000 ,
            - 1.41421 , - 2.00000 , - 1.41421 );
         sa:    castparray = ( 150 , 165 , 180 , 195 , 210 , 225 , 240 ,
            255 , 270 , 285 , 300 , 315 , 330 , 345 , 000 , 15 , 30 , 45 ,
            60 , 75 , 90 , 105 , 120 , 135 );
         ea:    castparray = ( 210 , 225 , 240 , 255 , 270 , 285 , 300 ,
            315 , 330 , 345 , 000 , 15 , 30 , 45 , 60 , 75 , 90 , 105 ,
            120 , 135 , 150 , 165 , 180 , 195 );
      VAR
         xwidth, ylength, i, i1, i2, ip: INTEGER;
         val: REAL;
         tex: STRING;
         ss, sp, sv, st, sx, sy: REAL;
         s1, t1, p1, v1: INTEGER;
         deltay: INTEGER;
         ArcCoords: ArcCoordsType;
         TextInfo: TextSettingsType;
   BEGIN
      DefineViewPort(1, 0.1, 0.95, 0.13, 0.9);
      DefineScale(1, 0.1, 1.0, 0.05, 1.0);
      OpenViewPort(1);
      SelectScale(1);
      SelectViewPort(1);
      SetColor(white);
      PutLabel(top, 'Internal Combustion Engine - Wankel (Rotary) Engine');
      tex := 'Max Operating Temp-' + NumStr(tmax - 100, 5, 1) +
      ',      Compression Ratio-' + NumStr(cratio, 4, 2) + ', Efficiency-'
      + NumStr(efficiency, 6, 4);
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
      FOR i := 1 TO 24 DO
      BEGIN
         ptemp[i] := Round(st * (temp[i] - tmin) + tpmin);
         pentrpy[i] := Round(ss * (entrpy[i] - smin) + spmin);
      END;
      FOR i := 1 TO 8 DO
      BEGIN
         ptempexpl[i] := Round(st * (tempexpl[i] - tmin) + tpmin);
         pentexpl[i] := Round(ss * (entexpl[i] - smin) + spmin);
      END;
      FOR i := 1 TO 5 DO
      BEGIN
         ptempexhaustv[i] := Round(st * (tempexhaustv[i] - tmin) + tpmin);
         pentexhaustv[i] := Round(ss * (entexhaustv[i] - smin) + spmin);
      END;
      FOR i := 1 TO 21 DO
      BEGIN
         ptempadex[i] := Round(st * (tempadex[i] - tmin) + tpmin);
         pentadex[i] := Round(ss * (entadex[i] - smin) + spmin);
      END;
      FOR i := 1 TO 3 DO
         Line(pentrpy[i], ptemp[i], pentrpy[i + 1], ptemp[i + 1]);
      FOR i := 4 TO 7 DO
         Line(pentrpy[i], ptemp[i], pentrpy[i + 1], ptemp[i + 1]);
      FOR i := 1 TO 4 DO
         Line(pentexhaustv[i], ptempexhaustv[i], pentexhaustv[i + 1],
            ptempexhaustv[i + 1]);
      Line(pentexhaustv[5], ptempexhaustv[5], pentadex[1], ptempadex[1]);
      FOR i := 1 TO 20 DO
         Line(pentadex[i], ptempadex[i], pentadex[i + 1], ptempadex[i + 1]);
      FOR i := 1 TO 7 DO
         Line(pentexpl[i], ptempexpl[i], pentexpl[i + 1], ptempexpl[i + 1]);
      Line(pentexpl[8], ptempexpl[8], pentrpy[15], ptemp[15]);
      FOR i := 15 TO 23 DO
         Line(pentrpy[i], ptemp[i], pentrpy[i + 1], ptemp[i + 1]);
      Line(pentrpy[1], ptemp[1], pentrpy[24], ptemp[24]);

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
      FOR i := 1 TO 24 DO
      BEGIN
         pvol[i] := Round(sv * (vol[i] - vmin) + vpmin);
         ppres[i] := Round(sp * (pres[i] - pmin) + ppmin);
      END;
      FOR i := 1 TO 8 DO
         ppresexpl[i] := Round(sp * (presexpl[i] - pmin) + ppmin);
      FOR i := 1 TO 5 DO
         ppresexhaustv[i] := Round(sp * (presexhaustv[i] - pmin) + ppmin);
      FOR i := 1 TO 21 DO
      BEGIN
         ppresadex[i] := Round(sp * (presadex[i] - pmin) + ppmin);
         pvoladex[i] := Round(sv * (voladex[i] - vmin) + vpmin);
      END;
      FOR i := 1 TO 3 DO
         Line(pvol[i], ppres[i], pvol[i + 1], ppres[i + 1]);
      FOR i := 4 TO 7 DO
         Line(pvol[i], ppres[i], pvol[i + 1], ppres[i + 1]);
      FOR i := 1 TO 4 DO
         Line(pvol[8], ppresexhaustv[i], pvol[8], ppresexhaustv[i + 1]);
      Line(pvol[8], ppresexhaustv[5], pvol[9], ppres[9]);
      FOR i := 1 TO 20 DO
         Line(pvoladex[i], ppresadex[i], pvoladex[i + 1], ppresadex[i + 1]);
      FOR i := 1 TO 7 DO
         Line(pvol[15], ppresexpl[i], pvol[15], ppresexpl[i + 1]);
      Line(pvol[15], ppresexpl[8], pvol[15], ppres[15]);
      FOR i := 15 TO 23 DO
         Line(pvol[i], ppres[i], pvol[i + 1], ppres[i + 1]);
      Line(pvol[1], ppres[1], pvol[24], ppres[24]);

      {*********************************************}
       { draw engine - lower screen }

      deltay := -(ybottom - ymiddle) DIV 20;
      FOR i := 1 TO 20 DO
         y[i] := ybottom + i * deltay;
      deltax := -deltay;
      x[1] := x1 - 10 * deltax;
      FOR i := 2 TO 21 DO
         x[i] := x[1] + (i - 1) * deltax;
      sx := (x[21] - x[1]) / 20.0;
      sy := (y[20] - y[1]) / 19.0;
      { casting outline }
      FOR i := 1 TO 24 DO
      BEGIN
         xpcast[i] := ROUND(sx * 3.0 * xcast[i]) + x[10];
         ypcast[i] := ROUND(sy * 3.0 * ycast[i]) + (y[10] + y[9]) DIV 2;
      END;
      SetLineStyle(SolidLn, 0, ThickWidth);
      FOR i := 1 TO 4 DO
         Line(xpcast[i], ypcast[i], xpcast[i + 1], ypcast[i + 1]);
      Line(xpcast[6], ypcast[6], xpcast[7], ypcast[7]);
      Line(xpcast[7], ypcast[7], xpcast[8], ypcast[8]);
      FOR i := 9 TO 15 DO
         Line(xpcast[i], ypcast[i], xpcast[i + 1], ypcast[i + 1]);
      Line(xpcast[17], ypcast[17], xpcast[21], ypcast[21]);
      FOR i := 21 TO 23 DO
         Line(xpcast[i], ypcast[i], xpcast[i + 1], ypcast[i + 1]);
      Line(xpcast[1], ypcast[1], xpcast[24], ypcast[24]);
      { intake port }
      Line(xpcast[5], ypcast[5], xpcast[5], ypcast[5] - 15);
      Line(xpcast[6], ypcast[6], xpcast[6], ypcast[6] - 16);
      { exhaust port }
      Line(xpcast[9], ypcast[5], xpcast[9], ypcast[9] - 15);
      Line(xpcast[8], ypcast[6], xpcast[8], ypcast[8] - 16);
       { spark plug }
      xsp[1] := (3 * xpcast[16] + xpcast[17]) DIV 4;
      xsp[2] := (xpcast[16] + 3 * xpcast[17]) DIV 4;
      xsp[3] := xsp[2];
      xsp[4] := xsp[1];
      ysp[1] := ypcast[16] + 2;
      ysp[2] := ysp[1];
      ysp[3] := ypcast[16] + 10;
      ysp[4] := ysp[3];
      Line(xsp[1], ysp[1], xsp[2], ysp[2]);
      Line(xsp[2], ysp[2], xsp[3], ysp[3]);
      Line(xsp[3], ysp[3], xsp[4], ysp[4]);
      Line(xsp[1], ysp[1], xsp[4], ysp[4]);
      SetColor(white);
      SetFillStyle(1, white);
     { FloodFill(xsp[1]-2,ysp[1]-2,white);}
      { rotor's gear }
      FOR i := 1 TO 28 DO
      BEGIN
         xcm[i] := Round(sx * 3.0 * cmx[a[i]] / 4.0) + x[10];
         ycm[i] := Round(sy * 3.0 * cmy[a[i]] / 4.0) + (y[10] + y[9]) DIV 2;
      END;
      rcm := 3 * deltax;
      { Stationary Gear }
      SetFillStyle(SolidFill, White);
      xsg := (xpcast[13] + xpcast[1]) DIV 2;
      ysg := (ypcast[7] + ypcast[19]) DIV 2;
      rsg := 3 * deltax DIV 2;
      FillEllipse(xsg, ysg, rsg, rsg);
      SetLineStyle(SolidLn, 0, NormWidth);
      { assign xa .. yc - movement of the rotor }
      FOR i := 1 TO 28 DO
      BEGIN
         xa[i] := xpcast[a[i]];
         ya[i] := ypcast[a[i]];
         xb[i] := xpcast[b[i]];
         yb[i] := ypcast[b[i]];
         xc[i] := xpcast[c[i]];
         yc[i] := ypcast[c[i]];
      END;
      { assign ssa .. eac - starting and ending angles of rotor sides }
      FOR i := 1 TO 28 DO
      BEGIN
         saa[i] := sa[a[i]];
         eaa[i] := ea[a[i]];
         sab[i] := sa[b[i]];
         eab[i] := ea[b[i]];
         sac[i] := sa[c[i]];
         eac[i] := ea[c[i]];
      END;
      InitialGasDisplay(xpcast, ypcast, compression7, explosion,
         compression6, intake1, exhaust4, power1, compression3,
         compression4, compression5, power2, power3, intake2, compression1,
          compression2, power4, power5, exhaust3, intake3, exhaust2,
          intake4, intake5, exhaustvent, exhaust1);
      SetColor(white);
      xw[1] := xleft + 5;
      xw[2] := xleft + 5;
      xw[3] := xleft + 5;
      xw[4] := x[21] + 5;
      xw[5] := x[21] + 5;
      xw[6] := x[21] + 5;
      yw[1] := y[17];
      yw[2] := y[11];
      yw[3] := y[5];
      yw[4] := y[17];
      yw[5] := y[11];
      yw[6] := y[5];
      SetColor(white);
      OutTextXY(xw[1], yw[1], 'Intake');
      OutTextXY(xw[1], yw[1] + 10, '(Isobaric & Isothermal)');
      OutTextXY(xw[1], yw[1] + 20, '(Intake Air and Fuel)');
      OutTextXY(xw[2], yw[2], 'Compression');
      OutTextXY(xw[2], yw[2] + 10, '(Adiabatic)');
      OutTextXY(xw[2], yw[2] + 20, '(Compress Mixture)');
      OutTextXY(xw[3], yw[3], 'Explosion');
      OutTextXY(xw[3], yw[3] + 10, '(Isochoric)');
      OutTextXY(xw[4], yw[4], 'Power');
      OutTextXY(xw[4], yw[4] + 10, '(Adiabatic)');
      OutTextXY(xw[4], yw[4] + 20, '(Expand Combustion');
      OutTextXY(xw[4], yw[4] + 30, ' By-Products)');
      OutTextXY(xw[5], yw[5], 'Vent Exhaust');
      OutTextXY(xw[5], yw[5] + 10, '(Isochoric)');
      OutTextXY(xw[5], yw[5] + 20, '(Heat Exchange bet');
      OutTextXY(xw[5], yw[5] + 30, ' Comb By-Prod and Atm)');
      OutTextXY(xw[6], yw[6], 'Exhaust');
      OutTextXY(xw[6], yw[6] + 10, '(Isobaric&Isothermal)');
      OutTextXY(xw[6], yw[6] + 20, '(Exhaust Combustion');
      OutTextXY(xw[6], yw[6] + 30, ' By-Products)');
   END;  { PROCEDURE InitializeGrid }

   PROCEDURE GasAnimate (a, i: INTEGER; xsp, ysp: sparray;
      compression7: pointtype6; explosion: pointtype7;
      compression6: pointtype8; intake1, exhaust4: pointtype9;
      power1: pointtype10; compression3, compression4, compression5,
      power2, power3: pointtype12; intake2, compression1, compression2,
      power4, power5, exhaust3: pointtype13; intake3,
      exhaust2: pointtype14; intake4, intake5, exhaustvent,
      exhaust1: pointtype15);

     {*********************************************************}
     { PROCEDURE GasAnimate is responsible for putting the gas }
     { in the appropriate part of the engine.            }
     {*********************************************************}

   BEGIN { GasAnimate }
      SetBkColor(DarkGray);
      SetColor(DarkGray);
      IF (i = 2) THEN
         SetFillStyle(SolidFill, DarkGray)
      ELSE
      BEGIN
         IF ((a >= 1) AND (a <= 2)) OR ((a >= 22) AND (a <= 24)) THEN
            SetFillStyle(WideDotFill, green)
         ELSE IF (a = 14) THEN
            SetFillStyle(CloseDotFill, lightred)
         ELSE IF (a >= 15) AND (a <= 21) THEN
            SetFillStyle(CloseDotFill, green)
         ELSE IF (a >= 4) AND (a <= 8) THEN
            SetFillStyle(WideDotFill, lightred)
         ELSE
            SetFillStyle(WideDotFill, lightred);
      END;
      IF (a <> 3) THEN
      BEGIN
         CASE a OF
            1:
               FillPoly(13, intake2);
            2:
               FillPoly(9, intake1);
            4:
               FillPoly(9, exhaust4);
            5:
               FillPoly(13, exhaust3);
            6:
               FillPoly(14, exhaust2);
            7:
               FillPoly(15, exhaust1);
            8:
               FillPoly(15, exhaustvent);
            9:
               FillPoly(13, power5);
            10:
               FillPoly(13, power4);
            11:
               FillPoly(12, power3);
            12:
               FillPoly(12, power2);
            13:
               FillPoly(10, power1);
            14:
            BEGIN
               FillPoly(7, explosion);
               IF (i = 1) THEN
                  SetFillStyle(solidfill, lightred);
               FloodFill(xsp[1] + 4, ysp[1] + 4, white);
               SetColor(DarkGray);
            END;
            15:
               FillPoly(6, compression7);
            16:
               FillPoly(8, compression6);
            17:
               FillPoly(12, compression5);
            18:
               FillPoly(12, compression4);
            19:
               FillPoly(12, compression3);
            20:
               FillPoly(13, compression2);
            21:
               FillPoly(13, compression1);
            22:
               FillPoly(15, intake5);
            23:
               FillPoly(15, intake4);
            24:
               FillPoly(14, intake3);
         END;  { CASE }
      END;
      SetColor(white);
   END;  { GasAnimate }

   PROCEDURE Animate (a, xa, xb, xc: abcarray; deltax, deltat: INTEGER;
      xw, yw: warray; xsp, ysp: sparray; compression7: pointtype6;
      explosion: pointtype7; compression6: pointtype8;
      intake1, exhaust4: pointtype9; power1: pointtype10;
      compression3, compression4, compression5, power2,
      power3: pointtype12; intake2, compression1, compression2, power4,
      power5, exhaust3: pointtype13; intake3, exhaust2: pointtype14;
      intake4, intake5, exhaustvent, exhaust1: pointtype15;
      pvol, ptemp, ppres, pentrpy: plotarray; ppresexpl, ptempexpl,
      pentexpl: pexpl; ppresexhaustv, ptempexhaustv,
      pentexhaustv: pexhaustv; ppresadex, ptempadex, pentadex,
      pvoladex: padex; vpmax, vpmin, ppmax, ppmin, tpmax, tpmin: INTEGER;
      VAR quitf: BOOLEAN);

    {**********************************************************}
    {*                  PROCEDURE Animate                     *}
    {* animates the engine, moves the dots around the T-S and *}
    {* P-V curve and changes the words describing the current *}
    {* process.                                               *}
    {**********************************************************}

      VAR
         i, i1, i2: INTEGER;
         STEP, restart: BOOLEAN;
   BEGIN
      STEP := false;
      restart := false;
      quitf := false;
      REPEAT
         FOR i := 1 TO 28 DO
         BEGIN
            CheckForEvents;
            IF HotKeys.Pressed(key) THEN
               HandleHotKeys(key, deltat, STEP, restart);
            MouseBorders(5, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
            IF ((quitf = false) AND (restart = false)) THEN
            BEGIN
             { change process color }
               IF (a[i] = 2) THEN
               BEGIN
                  SetColor(green);
                  OutTextXY(xw[1], yw[1], 'Intake');
                  OutTextXY(xw[1], yw[1] + 10, '(Isobaric & Isothermal)');
                  OutTextXY(xw[1], yw[1] + 20, '(Intake Air and Fuel)');
                  SetColor(white);
                  OutTextXY(xw[6], yw[6], 'Exhaust');
                  OutTextXY(xw[6], yw[6] + 10, '(Isobaric&Isothermal)');
                  OutTextXY(xw[6], yw[6] + 20, '(Exhaust Combustion');
                  OutTextXY(xw[6], yw[6] + 30, ' By-Products)');
               END;
               IF (a[i] = 21) THEN
               BEGIN
                  SetColor(green);
                  OutTextXY(xw[2], yw[2], 'Compression');
                  OutTextXY(xw[2], yw[2] + 10, '(Adiabatic)');
                  OutTextXY(xw[2], yw[2] + 20, '(Compress Mixture)');
                  SetColor(white);
                  OutTextXY(xw[1], yw[1], 'Intake');
                  OutTextXY(xw[1], yw[1] + 10, '(Isobaric & Isothermal)');
                  OutTextXY(xw[1], yw[1] + 20, '(Intake Air and Fuel)');
               END;
               IF (a[i] = 14) THEN
               BEGIN
                  SetColor(lightred);
                  OutTextXY(xw[3], yw[3], 'Explosion');
                  OutTextXY(xw[3], yw[3] + 10, '(Isochoric)');
                  SetColor(white);
                  OutTextXY(xw[2], yw[2], 'Compression');
                  OutTextXY(xw[2], yw[2] + 10, '(Adiabatic)');
                  OutTextXY(xw[2], yw[2] + 20, '(Compress Mixture)');
               END;
               IF (a[i] = 13) THEN
               BEGIN
                  SetColor(lightred);
                  OutTextXY(xw[4], yw[4], 'Power');
                  OutTextXY(xw[4], yw[4] + 10, '(Adiabatic)');
                  OutTextXY(xw[4], yw[4] + 20, '(Expand Combustion');
                  OutTextXY(xw[4], yw[4] + 30, ' By-Products)');
                  SetColor(white);
                  OutTextXY(xw[3], yw[3], 'Explosion');
                  OutTextXY(xw[3], yw[3] + 10, '(Isochoric)');
               END;
               IF (a[i] = 8) THEN
               BEGIN
                  SetColor(lightred);
                  OutTextXY(xw[5], yw[5], 'Vent Exhaust');
                  OutTextXY(xw[5], yw[5] + 10, '(Isochoric)');
                  OutTextXY(xw[5], yw[5] + 20, '(Heat Exchange bet');
                  OutTextXY(xw[5], yw[5] + 30, ' Comb By-Prod and Atm)');
                  SetColor(white);
                  OutTextXY(xw[4], yw[4], 'Power');
                  OutTextXY(xw[4], yw[4] + 10, '(Adiabatic)');
                  OutTextXY(xw[4], yw[4] + 20, '(Expand Combustion');
                  OutTextXY(xw[4], yw[4] + 30, ' By-Products)');
               END;
               IF (a[i] = 7) THEN
               BEGIN
                  SetColor(lightred);
                  OutTextXY(xw[6], yw[6], 'Exhaust');
                  OutTextXY(xw[6], yw[6] + 10, '(Isobaric&Isothermal)');
                  OutTextXY(xw[6], yw[6] + 20, '(Exhaust Combustion');
                  OutTextXY(xw[6], yw[6] + 30, ' By-Products)');
                  SetColor(white);
                  OutTextXY(xw[5], yw[5], 'Vent Exhaust');
                  OutTextXY(xw[5], yw[5] + 10, '(Isochoric)');
                  OutTextXY(xw[5], yw[5] + 20, '(Heat Exchange bet');
                  OutTextXY(xw[5], yw[5] + 30, ' Comb By-Prod and Atm)');
               END;
             { draw gas }
               IF (a[i] <> 3) THEN
                  GasAnimate(a[i], 1, xsp, ysp, compression7, explosion,
                     compression6, intake1, exhaust4, power1, compression3,
                     compression4, compression5, power2, power3, intake2,
                     compression1, compression2, power4, power5, exhaust3,
                     intake3, exhaust2, intake4, intake5, exhaustvent,
                     exhaust1);
               SetColor(white);
             { draw rotor }
               Arc(xa[i], ya[i], saa[i], eaa[i], Round(15.3 * deltax));
               Arc(xb[i], yb[i], sab[i], eab[i], Round(15.3 * deltax));
               Arc(xc[i], yc[i], sac[i], eac[i], Round(15.3 * deltax));
               Circle(xcm[i], ycm[i], rcm);
             { plot points on T-S and P-V curves }
               IF ((a[i] >= 1) AND (a[i] <= 3)) OR ((a[i] >= 15)
                  AND (a[i] <= 24)) THEN
                  SetFillStyle(solidfill, green)
               ELSE
                  SetFillStyle(solidfill, lightred);
               SetColor(white);
               FillEllipse(pvol[a[i]], ppres[a[i]], 2, 2);
               FillEllipse(pentrpy[a[i]], ptemp[a[i]], 2, 2);
               IF (STEP) THEN
               BEGIN
                  PressKeyMessage;
                  SetUpMyMenu;
               END;
               Delay(deltat);
               CheckForEvents;
               IF HotKeys.Pressed(key) THEN
                  HandleHotKeys(key, deltat, STEP, restart);
               MouseBorders(5, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
             { erase gas }
               IF (a[i] <> 3) THEN
                  GasAnimate(a[i], 2, xsp, ysp, compression7, explosion,
                     compression6, intake1, exhaust4, power1, compression3,
                     compression4, compression5, power2, power3, intake2,
                     compression1, compression2, power4, power5, exhaust3,
                     intake3, exhaust2, intake4, intake5, exhaustvent,
                     exhaust1);
             { erase rotor }
               SetColor(DarkGray);
               Arc(xa[i], ya[i], saa[i], eaa[i], Round(15.3 * deltax));
               Arc(xb[i], yb[i], sab[i], eab[i], Round(15.3 * deltax));
               Arc(xc[i], yc[i], sac[i], eac[i], Round(15.3 * deltax));
               Circle(xcm[i], ycm[i], rcm);
             { redraw casting }
               SetColor(white);
               SetLineStyle(SolidLn, 0, ThickWidth);
               FOR i2 := 1 TO 4 DO
                  Line(xpcast[i2], ypcast[i2], xpcast[i2 + 1], ypcast[i2 + 1]);
               Line(xpcast[6], ypcast[6], xpcast[7], ypcast[7]);
               Line(xpcast[7], ypcast[7], xpcast[8], ypcast[8]);
               FOR i2 := 9 TO 15 DO
                  Line(xpcast[i2], ypcast[i2], xpcast[i2 + 1], ypcast[i2 + 1]);
               Line(xpcast[17], ypcast[17], xpcast[21], ypcast[21]);
               FOR i2 := 21 TO 23 DO
                  Line(xpcast[i2], ypcast[i2], xpcast[i2 + 1], ypcast[i2 + 1]);
               Line(xpcast[1], ypcast[1], xpcast[24], ypcast[24]);
               SetLineStyle(SolidLn, 0, NormWidth);
             { erase points on T-S and P-V plots }
               SetFillStyle(solidfill, darkgray);
               SetColor(darkgray);
               FillEllipse(pvol[a[i]], ppres[a[i]], 2, 2);
               FillEllipse(pentrpy[a[i]], ptemp[a[i]], 2, 2);
               SetColor(white);
             { redraw T-S and P-V curves and outlines}
               FOR i2 := 1 TO 3 DO
               BEGIN
                  Line(pentrpy[i2], ptemp[i2], pentrpy[i2 + 1], ptemp[i2 + 1]);
                  Line(pvol[i2], ppres[i2], pvol[i2 + 1], ppres[i2 + 1]);
               END;
               FOR i2 := 4 TO 7 DO
               BEGIN
                  Line(pentrpy[i2], ptemp[i2], pentrpy[i2 + 1], ptemp[i2 + 1]);
                  Line(pvol[i2], ppres[i2], pvol[i2 + 1], ppres[i2 + 1]);
               END;
               FOR i2 := 1 TO 4 DO
               BEGIN
                  Line(pentexhaustv[i2], ptempexhaustv[i2], pentexhaustv[i2 + 1], ptempexhaustv[i2 + 1]);
                  Line(pvol[8], ppresexhaustv[i2], pvol[8], ppresexhaustv[i2 + 1]);
               END;
               Line(pentexhaustv[5], ptempexhaustv[5], pentrpy[9], ptemp[9]);
               Line(pvol[8], ppresexhaustv[5], pvol[9], ppres[9]);
               FOR i2 := 1 TO 20 DO
               BEGIN
                  Line(pentadex[i2], ptempadex[i2], pentadex[i2 + 1], ptempadex[i2 + 1]);
                  Line(pvoladex[i2], ppresadex[i2], pvoladex[i2 + 1], ppresadex[i2 + 1]);
               END;
               FOR i2 := 1 TO 7 DO
               BEGIN
                  Line(pentexpl[i2], ptempexpl[i2], pentexpl[i2 + 1], ptempexpl[i2 + 1]);
                  Line(pvol[15], ppresexpl[i2], pvol[15], ppresexpl[i2 + 1]);
               END;
               Line(pentexpl[8], ptempexpl[8], pentrpy[15], ptemp[15]);
               Line(pvol[15], ppresexpl[8], pvol[15], ppres[15]);
               FOR i2 := 15 TO 23 DO
               BEGIN
                  Line(pentrpy[i2], ptemp[i2], pentrpy[i2 + 1], ptemp[i2 + 1]);
                  Line(pvol[i2], ppres[i2], pvol[i2 + 1], ppres[i2 + 1]);
               END;
               Line(pentrpy[1], ptemp[1], pentrpy[24], ptemp[24]);
               Line(pvol[1], ppres[1], pvol[24], ppres[24]);
               Line(spmin, tpmin, spmin, tpmax);
               Line(spmax, tpmin, spmax, tpmax);
               Line(spmin, tpmin, spmax, tpmin);
               Line(spmin, tpmax, spmax, tpmax);
               Line(vpmin, ppmin, vpmin, ppmax);
               Line(vpmax, ppmin, vpmax, ppmax);
               Line(vpmin, ppmin, vpmax, ppmin);
               Line(vpmin, ppmax, vpmax, ppmax);
            END;  { IF ((quitf = false) AND (restart = false) THEN }
         END;
      UNTIL ((quitf = true) OR (restart = true));
   END; { PROCEDURE Animate }

BEGIN  { PROGRAM Wankel }
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
      IF (quitf = false) THEN
      BEGIN
         MouseBorders(5, GetMaxY - 15, GetMaxX - 5, GetMaxY - 10);
         ClearMuppetPort;
         SetUpHotKeys(HotKeys);
         HotKeys.Display;
         SetUpMyMenu;
         HideCursor;
         vmax := 0.025;
         vmin := 0.0;
         pmax := 75.0;
         pmin := 0.0;
         tmin := 250;
         smax := 125.0;
         smin := 0.0;
         GetCycle(vmax, vmin, pmax, pmin, tmax, tmin, smax, smin, cratio,
            vol, temp, pres, entrpy, tempexpl, presexpl, entexpl,
            tempexhaustv, presexhaustv, entexhaustv, tempadex, presadex,
            entadex, voladex, efficiency);
         InitializeGrid ( a , b , c , vmax , vmin , pmax , pmin ,
            tmax + 100 , tmin , smax , smin , cratio , efficiency ,
            deltax , vol , temp , pres , entrpy , presexpl , tempexpl ,
            entexpl , presexhaustv , tempexhaustv , entexhaustv , presadex ,
            tempadex , entadex , voladex , vpmax , vpmin , ppmax , ppmin ,
            tpmax , tpmin , spmax , spmin , xleft , x1 , xright , ytop ,
            ymiddle , ybottom , x , y , xsg , ysg , rsg , xsp , ysp , pvol ,
            ptemp , ppres , pentrpy , ppresexpl , ptempexpl , pentexpl ,
            ppresexhaustv , ptempexhaustv , pentexhaustv , ppresadex ,
            ptempadex , pentadex , pvoladex , xpcast , ypcast , xa , ya ,
            xb , yb , xc , yc , xcm , ycm , saa , sab , sac , eaa , eab ,
            eac , rcm , xw , yw , compression7 , explosion , compression6 ,
            intake1 , exhaust4 , power1 , compression3 , compression4 ,
            compression5 , power2 , power3 , intake2 , compression1 ,
            compression2 , power4 , power5 , exhaust3 , intake3 , exhaust2 ,
            intake4 , intake5 , exhaustvent , exhaust1 );
         ShowCursor;
         Animate ( a , xa , xb , xc , deltax , deltat , xw , yw , xsp ,
            ysp , compression7 , explosion , compression6 , intake1 ,
            exhaust4 , power1 , compression3 , compression4 , compression5 ,
            power2 , power3 , intake2 , compression1 , compression2 ,
            power4 , power5 , exhaust3 , intake3 , exhaust2 , intake4 ,
            intake5 , exhaustvent , exhaust1 , pvol , ptemp , ppres ,
            pentrpy , ppresexpl , ptempexpl , pentexpl , ppresexhaustv ,
            ptempexhaustv , pentexhaustv , ppresadex , ptempadex , pentadex ,
            pvoladex , vpmax , vpmin , ppmax , ppmin , tpmax , tpmin , quitf );
      END;  { IF (quitf = false) }
      CUPSDone;
   UNTIL (quitf);
END.  { PROGRAM Wankel }