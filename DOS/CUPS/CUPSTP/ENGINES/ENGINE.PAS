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
   {*              PROGRAM Engine                   *}
   {*                                               *}
   {* With this program, the user designs an engine *}
   {* which contains one mole of gas.  The user     *}
   {* specifies the engine type (reversible or      *}
   {* irreversible), the gas (Helium, Argon,        *}
   {* Nitrogen, Steam), the initial temperature and *}
   {* pressure (usually T=300K & P=1.0 atm), the    *}
   {* processes (adiabatic, isobaric, isochoric, or *}
   {* isothermal) and the appropriate final         *}
   {* condition (i.e., final temperature, volume or *}
   {* pressure).                                    *}
   {*                                               *}
   {* The irreversible engine is a simple endo-     *}
   {* reversible heat engine where irreversible     *}
   {* processes can take place only in the heat     *}
   {* transport and not in the conversion of heat   *}
   {* to work.  Thus the heat exchanged in a process*}
   {* equals Q*(1.0+alpha) where Q is the heat that *}
   {* is involved in the reversible part of the     *}
   {* process and alpha is related to the heat loss.*}
   {* This is true only for heat being added to the *}
   {* engine.                                       *}
   {*                                               *}
   {* The steps are plotted on a T-S diagram and a  *}
   {* P-V diagram at the top of the screen.  There  *}
   {* is a summary of the steps in the lower left of*}
   {* the screen and a blank block in the lower     *}
   {* right of the screen.  After the cycle is      *}
   {* completed, a summary of the engine performance*}
   {* is given in the blank block.  All the data is *}
   {* also sent to a disk file entitled engine.dat  *}
   {*                                               *}
   {*        Written by L. B. Spornick              *}
   {*        Applied Physics Laboratory             *}
   {*        The Johns Hopkins University           *}
   {*                                               *}
   {*              April 10 ,1992                   *}
   {*              ver 1.0 July, 1993               *}
   {*              ver 1.1 Nov, 1993                *}
   {*              ver 1.2 May,1994                 *}
   {*              ver 1.3 Sept,1994                *}
   {*              ver 1.3 Apr,1995                 *}
   {*                                               *}
   {*************************************************}
   {*************************************************}

PROGRAM ENGINE;

   USES
      Graph, CUPSMUPP, CUPSgui, Crt, CUPS;

   TYPE
      varray = ARRAY[1..51] OF REAL;
      plotarray = ARRAY[1..20, 1..21] OF REAL;
      sarray = ARRAY[1..9] OF INTEGER;
      parray = ARRAY[0..20] OF INTEGER;
      carray = ARRAY[1..11] OF CHAR;
      larray = ARRAY[1..18] OF CHAR;
      tarray = ARRAY[0..20] OF REAL;
      testarray = ARRAY[1..4] OF CHAR;
      sixarray = ARRAY[1..6] OF CHAR;
      gasarray = ARRAY[1..4] OF REAL;

   CONST
      slash: 
      FillPatternType = ( $AA , $55 , $AA , $55 , $AA , $55 , $AA , $55 );
      cvgas:
      gasarray = ( 1.5 , 1.5 , 2.5 , 4.55 );
      gammagas: 
      gasarray = ( 1.66666667 , 1.66666667 , 1.4 , 1.22222222 );
      massgas: 
      gasarray = ( 39.948 , 4.002602 , 28.01348 , 18.01528 );
      beepon: 
      BOOLEAN = true;
      quitf: 
      BOOLEAN = false;
      first: 
      BOOLEAN = true;
      Engine_Output: STRING
      = 'Engine.dat';
      restart: 
      BOOLEAN = false;

   VAR
      cp, cv, gamma, R, hP, amu, lam, amass, mass, atm, kB, Navg, pmax,
      pmin, vmax, vmin, tmax, tmin, smax, smin, heatin, heatout, alpha,
      totalwork, totalheat, totalenergy, efficiency: REAL;
      ppmax, ppmin, vpmax, vpmin, tpmax, tpmin, spmax, spmin, error,
      enginetype, gastype, gas, sy, xleft, x1, x2, xright, ytop, ymiddle1,
      ymiddle2, ybottom, step, sxrow, i, j, skey: INTEGER;
      process, func, processold, funcold, endcycle: INTEGER;
      slideropen, stepold: INTEGER;
      syrow: sarray;
      readk: CHAR;
      proc: parray;
      pf, vf, tf, sf, work, heat, deltau: tarray;
      pp, vp, tp, sp: plotarray;
      ppold, vpold, tpold, spold: REAL;
      test: testarray;
      MyMenuout: TMenu;
      HotKeys: THotKeys;
      buttons: TButtons;
      key: BYTE;
      sliders: TSliders;
      lname: STRING;
      itype, islider, isliderc: INTEGER;
      tex: CHAR;
      told, vold, pold, sold: REAL;

   PROCEDURE HELP1;
      VAR
         helpScr: HelpScrType;
   BEGIN
      helpScr[1] := '                                                  ';
      helpScr[2] := '                  Processes                       ';
      helpScr[3] := '                                                  ';
      helpScr[4] := '   To design an engine, the user selects a series ';
      helpScr[5] := '   of processes along with the final value for T, ';
      helpScr[6] := '   V or P for each process. The engine is defined ';
      helpScr[7] := '   when the cycle is completed (i.e., the system  ';
      helpScr[8] := '   is back to its initial condition).             ';
      helpScr[9] := '                                                  ';
      helpScr[10] := '   In an adiabatic process, the engine is         ';
      helpScr[11] := '   thermally isolated from its surroundings,      ';
      helpScr[12] := '   there is no exchange of heat and the entropy   ';
      helpScr[13] := '   remains constant.                              ';
      helpScr[14] := '                                                  ';
      helpScr[15] := '   In an isobaric process, the pressure remains   ';
      helpScr[16] := '   constant.                                      ';
      helpScr[17] := '                                                  ';
      helpScr[18] := '   In an isochoric process, the volume remains    ';
      helpScr[19] := '   constant.  No work is done.                    ';
      helpScr[20] := '                                                  ';
      helpScr[21] := '   In an isothermal process, the temperature and, ';
      helpScr[22] := '   for an Ideal Gas, the internal energy remain   ';
      helpScr[23] := '   constant.                                      ';
      helpScr[24] := '                                                  ';
      helpScr[25] := '                                                  ';
      HELP(helpScr);
   END;  {  PROCEDURE HELP1  }

   PROCEDURE HELP_Adiabatic;
      VAR
         helpScr: HelpScrType;
   BEGIN
      helpScr[1] := '             An Adiabatic Process                 ';
      helpScr[2] := '                                                  ';
      helpScr[3] := '   An Adiabatic Process is a thermodynamic        ';
      helpScr[4] := '   process in which no heat is exchanged between  ';
      helpScr[5] := '   the engine and its surroundings.  The engine   ';
      helpScr[6] := '   is thermally isolated from its surroundings.   ';
      helpScr[7] := '   In an Adiabatic Process, the entropy remains   ';
      helpScr[8] := '   constant.                                      ';
      helpScr[9] := '                                                  ';
      helpScr[10] := '   For an Ideal Gas, the relationships between    ';
      helpScr[11] := '   the initial and final temperatures, volumes,   ';
      helpScr[12] := '   and pressures are:                             ';
      helpScr[13] := '                                                  ';
      helpScr[14] := '   T(i)*V(i)**(gamma-1) = T(f)*V(f)**(gamma-1)    ';
      helpScr[15] := '   P(i)*V(i)**(gamma) = P(f)*V(f)**(gamma)        ';
      helpScr[16] := '   T(i)*P(i)**(-(gamma-1)/gamma) =                ';
      helpScr[17] := '        T(f)*P(f)**(-(gamma-1)/gamma)             ';
      helpScr[18] := '                                                  ';
      helpScr[19] := '   where x**y is x to the yth power, and gamma is ';
      helpScr[20] := '   (the Specific Heat at Constant Pressure = CP)/ ';
      helpScr[21] := '   (the Specific Heat at Constant Volume = CV).   ';
      helpScr[22] := '                                                  ';
      helpScr[23] := '   The work done is (P(f)V(f)-P(i)V(i))/(gamma-1) ';
      helpScr[24] := '   and the change in the internal energy is       ';
      helpScr[25] := '   CV*(T(f)-T(i)).                                ';
      HELP(helpScr);
   END;  {  PROCEDURE HELP_Adiabatic }

   PROCEDURE HELP_Isobaric;
      VAR
         helpScr: HelpScrType;
   BEGIN
      helpScr[1] := '                                                  ';
      helpScr[2] := '                An Isobaric Process               ';
      helpScr[3] := '                                                  ';
      helpScr[4] := '   An Isobaric Process is a thermodynamic         ';
      helpScr[5] := '   process in which the pressure is kept          ';
      helpScr[6] := '   constant.                                      ';
      helpScr[7] := '                                                  ';
      helpScr[8] := '   For an Ideal Gas, the relationships between    ';
      helpScr[9] := '   the initial and final temperatures, volumes    ';
      helpScr[10] := '   and entropies are:                             ';
      helpScr[11] := '                                                  ';
      helpScr[12] := '   T(f)/V(f) = T(i)/V(i)                          ';
      helpScr[13] := '   S(f) = S(i) + CP*ln(T(f)/T(i))                 ';
      helpScr[14] := '                                                  ';
      helpScr[15] := '   The work done by the engine is P(f)(V(f)-V(i)),';
      helpScr[16] := '   the heat that is absorbed by the engine is     ';
      helpScr[17] := '   CP*(T(f)-T(i) and the change in the internal   ';
      helpScr[18] := '   energy is CV*(T(f)-T(i)).  CP is the Specific  ';
      helpScr[19] := '   Heat at Constant Pressure and CV is the        ';
      helpScr[20] := '   Specific Heat at Constant Volume.              ';
      helpScr[21] := '                                                  ';
      helpScr[22] := '                                                  ';
      helpScr[23] := '                                                  ';
      helpScr[24] := '                                                  ';
      helpScr[25] := '                                                  ';
      HELP(helpScr);
   END;  {  PROCEDURE HELP_Isobaric }

   PROCEDURE HELP_Isochoric;
      VAR
         helpScr: HelpScrType;
   BEGIN
      helpScr[1] := '                                                  ';
      helpScr[2] := '                An Isochoric Process              ';
      helpScr[3] := '                                                  ';
      helpScr[4] := '   An Isochoric Process is a thermodynamic        ';
      helpScr[5] := '   process in which the volume is kept            ';
      helpScr[6] := '   constant.                                      ';
      helpScr[7] := '                                                  ';
      helpScr[8] := '   For an Ideal Gas, the relationships between    ';
      helpScr[9] := '   the initial and final temperatures, pressures, ';
      helpScr[10] := '   and entropies are:                             ';
      helpScr[11] := '                                                  ';
      helpScr[12] := '   T(f)/P(f) = T(i)/P(i)                          ';
      helpScr[13] := '   S(f) = S(i) + CV*ln(T(f)/T(i))                 ';
      helpScr[14] := '                                                  ';
      helpScr[15] := '   No work is done by the engine, the heat that   ';
      helpScr[16] := '   is absorbed by the engine is CV*(T(f)-T(i))    ';
      helpScr[17] := '   and the change in the internal energy is       ';
      helpScr[18] := '   CV*(T(f)-T(i)).  CV is the Specific Heat at    ';
      helpScr[19] := '   Constant Volume.                               ';
      helpScr[20] := '                                                  ';
      helpScr[21] := '                                                  ';
      helpScr[22] := '                                                  ';
      helpScr[23] := '                                                  ';
      helpScr[24] := '                                                  ';
      helpScr[25] := '                                                  ';
      HELP(helpScr);
   END;  {  PROCEDURE HELP_Isochoric  }

   PROCEDURE HELP_Isothermal;
      VAR
         helpScr: HelpScrType;
   BEGIN
      helpScr[1] := '                                                  ';
      helpScr[2] := '                An Isothermal Process             ';
      helpScr[3] := '                                                  ';
      helpScr[4] := '   An Isothermal Process is a thermodynamic       ';
      helpScr[5] := '   process in which the temperature is kept       ';
      helpScr[6] := '   constant.                                      ';
      helpScr[7] := '                                                  ';
      helpScr[8] := '   For an Ideal Gas, the relationships between    ';
      helpScr[9] := '   the initial and final volumes, pressures, and  ';
      helpScr[10] := '   entropies are:                                 ';
      helpScr[11] := '                                                  ';
      helpScr[12] := '   V(f)*P(f) = V(i)*P(i)                          ';
      helpScr[13] := '   S(f) = S(i) + R*ln(V(f)/V(i))                  ';
      helpScr[14] := '                                                  ';
      helpScr[15] := '   The work done by the engine is                 ';
      helpScr[16] := '   R*T(f)*ln(V(f)/V(i)) and the heat that is      ';
      helpScr[17] := '   absorbed by the engine is T(f)*(S(f)-S(i)).    ';
      helpScr[18] := '   The internal energy does not change.  R is the ';
      helpScr[19] := '   Gas Constant.                                  ';
      helpScr[20] := '                                                  ';
      helpScr[21] := '                                                  ';
      helpScr[22] := '                                                  ';
      helpScr[23] := '                                                  ';
      helpScr[24] := '                                                  ';
      helpScr[25] := '                                                  ';
      HELP(helpScr);
   END;  {  PROCEDURE HELP_Isothermal  }

   PROCEDURE ABOUTPROGRAM;
      VAR
         helpScr: HelpScrType;
   BEGIN
      helpScr[1] := '              Design Your Own Engine              ';
      helpScr[2] := '                                                  ';
      helpScr[3] := '   L. B. Spornick, APL, The Johns Hopkins Univ.   ';
      helpScr[4] := '                Copyright (c) 1995                ';
      helpScr[5] := '                                                  ';
      helpScr[6] := '  This program allows the user to design his/her  ';
      helpScr[7] := '  own ideal gas engine.  The ideal gases are      ';
      helpScr[8] := '  either Helium, Argon, Nitrogen, or Steam.  The  ';
      helpScr[9] := '  properties of Nitrogen are most like those of   ';
      helpScr[10] := '  air.                                            ';
      helpScr[11] := '                                                  ';
      helpScr[12] := '  An engine consists of a series of thermodynamic ';
      helpScr[13] := '  processes.  These can be adiabatic (no heat is  ';
      helpScr[14] := '  absorbed by or expelled from the engine,        ';
      helpScr[15] := '  constant entropy), isobaric (constant pressure),';
      helpScr[16] := '  isochoric (constant volume, also known as       ';
      helpScr[17] := '  isovolumic), or isothermal (constant            ';
      helpScr[18] := '  temperature).                                   ';
      helpScr[19] := '                                                  ';
      helpScr[20] := '  The engine is either reversible or irreversible.';
      helpScr[21] := '  If it is irreversible, part of the heat that is ';
      helpScr[22] := '  extracted from the heat reservior(s) to be      ';
      helpScr[23] := '  absorbed by the engine is lost to irreversible  ';
      helpScr[24] := '  processes (friction).                           ';
      helpScr[25] := '                                                  ';
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

   PROCEDURE SetUpMyMenu;
   BEGIN
      MyMenuOut.Init;
      WITH MyMenuOut DO
      BEGIN
         column(1, 'File');
         row(1, 1, 'About CUPS');
         row(1, 2, 'About Program');
         row(1, 3, 'Configuration');
         row(1, 4, 'Restart Program');
         row(1, 5, 'Exit Program');
         column(2, 'Tutorial');
         row(2, 1, 'Adiabatic Proc');
         row(2, 2, 'Isobaric Proc');
         row(2, 3, 'Isochoric Proc');
         row(2, 4, 'Isothermal Proc');
      END;
      MyMenuOut.Display;
   END;  { PROCEDURE SetUpMyMenu }

   PROCEDURE HandleMenu;
   BEGIN
      CASE MyMenuOut.colChosen OF
         1: 
            CASE MyMenuOut.rowChosen OF
               1: 
                  ABOUTCUPS;
               2: 
                  ABOUTPROGRAM;
               3: 
                  configuration;
               4: 
                  restart := true;
               5: 
                  quitf := true;
            END; { CASE }
         2: 
            CASE MyMenuOut.rowChosen OF
               1: 
                  Help_Adiabatic;
               2: 
                  Help_Isobaric;
               3: 
                  Help_Isochoric;
               4: 
                  Help_Isothermal;
            END; { CASE }
      END;   { CASE }
   END; { PROCEDURE HandleMenu }

   PROCEDURE SetUpHotKeys (VAR HotKeys: THotKeys);
   BEGIN
      WITH HotKeys DO
      BEGIN
         init(2);
         key[1] := 'F1-Help';
         key[2] := 'F10-Menu';
         display;
      END;
   END;  { PROCEDURE SetUpHotKeys }

   PROCEDURE HandleHotKeys (key: byte);
   BEGIN
      CASE key OF
         1: 
            Help1;  { Processes }
         2: 
            IF MyMenuOut.chosen THEN
               HandleMenu;
      END;  { CASE }
   END;  { PROCEDURE HandleHotKeys }

   PROCEDURE SetUpSliders;
   BEGIN
      sliders.init;
      IF (gas <> 4) THEN
         sliders.create(1, 300, 2100, tf[0],
                        0.66, 0.3, 1, 1, '300', '2100', 'Temperature', false)
      ELSE
         sliders.create(1, 500, 2100, tf[0], 0.66, 0.3, 1, 1,
                        '500', '2100', 'Temperature', false);
      sliders.create(2, 0.0001, vmax, vf[0], 0.66, 0.2, 1, 6, '0.0001',
                     NumStr(vmax, 6, 4), 'Volume', false);
      sliders.create(3, 1.0, 75.0, pf[0], 0.66, 0.1, 1, 3, '1', '75',
                     'Pressure', false);
   END;   {* PROCECURE SetUpSliders *}

   PROCEDURE HandleSliders (process, step: INTEGER;
      VAR islider, func: INTEGER; VAR tf, pf, vf: tarray);
   BEGIN
      CASE sliders.LastAltered OF
         1: 
            IF process = 4 THEN
            BEGIN
               sliders.erase(1, black);
               islider := 0;
            END
            ELSE
            BEGIN
               tf[step] := sliders.value(1);
               func := 1;
               islider := 1;
            END;
         2: 
            IF process = 3 THEN
            BEGIN
               sliders.erase(2, black);
               islider := 0;
            END
            ELSE
            BEGIN
               vf[step] := sliders.value(2);
               func := 3;
               islider := 1;
            END;
         3: 
            IF process = 2 THEN
            BEGIN
               sliders.erase(3, black);
               islider := 0;
            END
            ELSE
            BEGIN
               pf[step] := sliders.value(3);
               func := 2;
               islider := 1;
            END;
      END;  { CASE }
   END;  {  PROCEDURE HandleSliders }

   PROCEDURE SetUpButtons;
   BEGIN
      buttons.init;
      buttons.create(1, 0.67, 0.45, 'Adiabatic');
      buttons.create(2, 0.85, 0.45, 'Isobaric');
      buttons.create(3, 0.67, 0.40, 'Isochoric');
      buttons.create(4, 0.85, 0.40, 'Isothermal');
      buttons.number := 0;
      buttons.DrawAll;
   END;   {* PROCEDURE SetUpButttons *}

   PROCEDURE ReadData;
      VAR
         InitScreen: TInputScreen;
         Ok: boolean;
         tf_: real;
         gas_: integer;
   BEGIN
      WITH InitScreen DO
      BEGIN
         init;
         DefineInputPort(0.15, 0.85, 0.05, 0.95);
         LoadLine('                 Design An Engine                  ');
         LoadLine('                                                    ');
         LoadLine('  Select Engine Type - #1 for Reversible             ');
         LoadLine('                       #1 for Irreversible           ');
         LoadLine('                                                    ');
         LoadLine('   Select Gas - #2 for Helium                        ');
         LoadLine('                #2 for Argon                         ');
         LoadLine('                #2 for Nitrogen                      ');
         LoadLine('                #2 for Steam                         ');
         LoadLine('                                                    ');
         LoadLine('   Initial Conditions :                             ');
         LoadLine('    Temperature = {        } K                      ');
         LoadLine('    (300-2000 for He,Ar,N)  (500-2000 for Steam)   ');
         LoadLine('                                                    ');
         LoadLine('    Pressure    = {        } atm (1 - 75)           ');
         LoadLine('                                                    ');
         LoadLine('    % of Heat Loss = {   } (Irreversible Engine only)');
         LoadLine('                                                    ');
         LoadLine('    Output File Name: "                   "         ');
         LoadLine('                                                    ');
         LoadLine('             [  Ok  ]    [Cancel]                    ');
         SetRadioButton('1', EngineType);
         SetRadioButton('2', Gas);
         SetNumber(7, tf[0]);
         SetNumberLimits(7, 300, 2000);
         SetNumber(8, pf[0]);
         SetNumberLimits(8, 1.0, 75.0);
         SetNumber(9, 0.0);
         SetNumberLimits(9, 0, 99);
         SetString(10, Engine_Output);
         OK := false;
         REPEAT
            Accept;
            Gas_ := GetRadioButton('2');
            tf_ := GetNumber(7);
            IF (Gas_ = 4) AND (tf_ < 500) THEN
               announce('Temperature out of range')
            ELSE
               OK := true;
         UNTIL OK;
         IF (NOT canceled) THEN
         BEGIN
            Gas := Gas_;
            tf[0] := tf_;
         END;
         EngineType := GetRadioButton('1');
         pf[0] := GetNumber(8);
         IF (EngineType = 2) THEN
            alpha := GetNumber(9) / 100.0
         ELSE
            alpha := 0.0;
         Engine_Output := GetString(10);
         IF (canceled) THEN
            quitf := true;
         DONE;
      END;  { WITH InitScreen DO  }
   END;  { PROCEDURE ReadData }

   PROCEDURE ReadData1;
      VAR
         InitScreen: TInputScreen;
         Ok: boolean;
         tf_: real;
         gas_: integer;
   BEGIN
      WITH InitScreen DO
      BEGIN
         init;
         DefineInputPort(0.15, 0.85, 0.05, 0.95);
         LoadLine('                 Design An Engine                  ');
         LoadLine('                                                    ');
         LoadLine('  Select Engine Type - #1 for Reversible             ');
         LoadLine('                       #1 for Irreversible           ');
         LoadLine('                                                    ');
         LoadLine('   Select Gas - #2 for Helium                        ');
         LoadLine('                #2 for Argon                         ');
         LoadLine('                #2 for Nitrogen                      ');
         LoadLine('                #2 for Steam                         ');
         LoadLine('                                                    ');
         LoadLine('   Initial Conditions :                             ');
         LoadLine('                                                    ');
         LoadLine('    Temperature = {        } K                      ');
         LoadLine('    (300-2000 for He,Ar,N)  (500-2000 for Steam)   ');
         LoadLine('                                                    ');
         LoadLine('    Pressure    = {        } atm (1.0 - 75.0)       ');
         LoadLine('                                                    ');
         LoadLine('    % of Heat Loss = {   } (Irreversible Engine only)');
         LoadLine('                                                    ');
         LoadLine('             [  Ok  ]    [Cancel]                    ');
         SetRadioButton('1', EngineType);
         SetRadioButton('2', Gas);
         SetNumber(7, tf[0]);
         SetNumberLimits(7, 300, 2000);
         SetNumber(8, pf[0]);
         SetNumberLimits(8, 1.0, 75.0);
         SetNumber(9, alpha);
         SetNumberLimits(9, 0, 99);
         OK := false;
         REPEAT
            Accept;
            Gas_ := GetRadioButton('2');
            tf_ := GetNumber(7);
            IF (Gas_ = 4) AND (tf_ < 500) THEN
               announce('Temperature out of range')
            ELSE
               OK := true;
         UNTIL OK;
         IF (NOT canceled) THEN
         BEGIN
            Gas := Gas_;
            tf[0] := tf_;
         END;
         EngineType := GetRadioButton('1');
         pf[0] := GetNumber(8);
         IF (EngineType = 2) THEN
            alpha := GetNumber(9) / 100.0
         ELSE
            alpha := 0.0;
         IF (canceled) THEN
            quitf := true;
         DONE;
      END;  { WITH InitScreen DO  }
   END;  { PROCEDURE ReadData1 }

   PROCEDURE ClearStepOut (i, sxrow: INTEGER; syrow: sarray);

    {*****************************************************}
    {**            PROCEDURE ClearStepOut               **}
    {** clears the step output screen (ie, the lower left*}
    {*****************************************************}

      TYPE
         barray = ARRAY[1..6] OF CHAR;
      CONST
         blank:  barray = ( chr ( 219 ) , chr ( 219 ) , chr ( 219 ) ,
                            chr ( 219 ) , chr ( 219 ) , chr ( 219 ) );
         sblank: testarray = ( chr ( 219 ) , chr ( 219 ) , chr ( 219 ) ,
                               chr ( 219 ) );
      VAR
         i1: INTEGER;
   BEGIN
      SetColor(darkgray);
      IF (i > 0) THEN
         i1 := 1
      ELSE
         i1 := 0;
      OutTextXY(sxrow + (i + 1) * 62 + 16 + 18 * i1, syrow[1] + 4 * i1, chr(219) + chr(219));
      OutTextXY(sxrow + (i + 1) * 62 + 8 + 18 * i1, syrow[2] + 4 * i1, sblank);
      OutTextXY(sxrow + (i + 1) * 62 + 18 * i1, syrow[3] + 4 * i1, blank);
      OutTextXY(sxrow + (i + 1) * 62 + 18 * i1, syrow[4] + 4 * i1, blank);
      OutTextXY(sxrow + (i + 1) * 62 + 18 * i1, syrow[5] + 4 * i1, blank);
      OutTextXY(sxrow + (i + 1) * 62 + 18 * i1, syrow[6] + 4 * i1, blank);
      OutTextXY(sxrow + (i + 1) * 62 + 18 * i1, syrow[7] + 4 * i1, blank);
      OutTextXY(sxrow + (i + 1) * 62 + 18 * i1, syrow[8] + 4 * i1, blank);
      OutTextXY(sxrow + (i + 1) * 62 + 18 * i1, syrow[9] + 4 * i1, blank);
      SetColor(white);
   END;  { PROCEDURE ClearStepOut }

   PROCEDURE StepOutput (step, process: INTEGER; pf, vf, tf, sf, work, heat,
             deltau: tarray; sxrow: INTEGER; syrow: sarray);

    {****************************************************}
    {**             PROCEDURE StepOutput               **}
    {** outputs the summary data for the current step. **}
    {****************************************************}

      CONST
         proc: ARRAY[0..4] OF STRING[4]
         = ( '----' , 'adia' , 'isob' , 'isoc' , 'isot' );
      VAR
         tex: STRING;
         i, i1, i2, i3: INTEGER;
   BEGIN
      i1 := 1;
      IF (step > 4) THEN
         i3 := 4
      ELSE
         i3 := step;
      ClearStepOut(i3, sxrow, syrow);
      tex := NumStr(step, 2, 0);
      OutTextXY(sxrow + (i3 + 1) * 62 + 16 + 18 * i1, syrow[1] + 4 * i1, tex);
      OutTextXY(sxrow + (i3 + 1) * 62 + 8 + 18 * i1, syrow[2] + 4 * i1, proc[process]);
      tex := NumStr(pf[step], 6, 1);
      OutTextXY(sxrow + (i3 + 1) * 62 + 18 * i1, syrow[3] + 4 * i1, tex);
      tex := NumStr(vf[step], 6, 5);
      OutTextXY(sxrow + (i3 + 1) * 62 + 18 * i1, syrow[4] + 4 * i1, tex);
      tex := NumStr(tf[step], 6, 1);
      OutTextXY(sxrow + (i3 + 1) * 62 + 18 * i1, syrow[5] + 4 * i1, tex);
      tex := NumStr(sf[step], 6, 1);
      OutTextXY(sxrow + (i3 + 1) * 62 + 18 * i1, syrow[6] + 4 * i1, tex);
      IF (work[step] < 0.0) THEN
         SetColor(yellow)
      ELSE IF (work[step] > 0.0) THEN
         SetColor(green)
      ELSE
         SetColor(white);
      tex := NumStr(work[step], 6, 0);
      OutTextXY(sxrow + (i3 + 1) * 62 + 18 * i1, syrow[7] + 4 * i1, tex);
      IF (heat[step] < 0.0) THEN
         SetColor(cyan)
      ELSE IF (heat[step] > 0.0) THEN
         SetColor(lightred)
      ELSE
         SetColor(white);
      tex := NumStr(heat[step], 6, 0);
      OutTextXY(sxrow + (i3 + 1) * 62 + 18 * i1, syrow[8] + 4 * i1, tex);
      SetColor(white);
      tex := NumStr(deltau[step], 6, 0);
      OutTextXY(sxrow + (i3 + 1) * 62 + 18 * i1, syrow[9] + 4 * i1, tex);
   END;  { PROCEDURE StepOutput }

   PROCEDURE MoveStep (step: INTEGER; proc: parray; pf, vf, tf, sf, work,
                heat, deltau: tarray; sxrow: INTEGER; syrow: sarray);

    {****************************************************}
    {**             PROCEDURE MoveStep                 **}
    {** moves the summary data.                        **}
    {****************************************************}

      CONST
         process: ARRAY[0..4] OF STRING[4]
         = ( '----' , 'adia' , 'isob' , 'isoc' , 'isot' );
      VAR
         tex: STRING;
         i, i1, i2, i3: INTEGER;
   BEGIN
      i := step - 3;
      FOR i3 := 1 TO 3 DO
      BEGIN
         ClearStepOut(i3, sxrow, syrow);
         tex := NumStr(i + (i3 - 1), 2, 0);
         OutTextXY(sxrow + (i3 + 1) * 62 + 16 + 18, syrow[1] + 4, tex);
         OutTextXY(sxrow + (i3 + 1) * 62 + 8 + 18, syrow[2] + 4, process[proc[i + (i3 - 1)]]);
         tex := NumStr(pf[i + (i3 - 1)], 6, 1);
         OutTextXY(sxrow + (i3 + 1) * 62 + 18, syrow[3] + 4, tex);
         tex := NumStr(vf[i + (i3 - 1)], 6, 4);
         OutTextXY(sxrow + (i3 + 1) * 62 + 18, syrow[4] + 4, tex);
         tex := NumStr(tf[i + (i3 - 1)], 6, 1);
         OutTextXY(sxrow + (i3 + 1) * 62 + 18, syrow[5] + 4, tex);
         tex := NumStr(sf[i + (i3 - 1)], 6, 1);
         OutTextXY(sxrow + (i3 + 1) * 62 + 18, syrow[6] + 4, tex);
         IF (work[i + (i3 - 1)] < 0.0) THEN
            SetColor(yellow)
         ELSE IF (work[i + (i3 - 1)] > 0.0) THEN
            SetColor(green)
         ELSE
            SetColor(white);
         tex := NumStr(work[i + (i3 - 1)], 6, 0);
         OutTextXY(sxrow + (i3 + 1) * 62 + 18, syrow[7] + 4, tex);
         IF (heat[i + (i3 - 1)] < 0.0) THEN
            SetColor(cyan)
         ELSE IF (heat[i + (i3 - 1)] > 0.0) THEN
            SetColor(lightred)
         ELSE
            SetColor(white);
         tex := NumStr(heat[i + (i3 - 1)], 6, 0);
         OutTextXY(sxrow + (i3 + 1) * 62 + 18, syrow[8] + 4, tex);
         SetColor(white);
         tex := NumStr(deltau[i + (i3 - 1)], 6, 0);
         OutTextXY(sxrow + (i3 + 1) * 62 + 18, syrow[9] + 4, tex);
      END;   { FOR i3 := 1 TO 3 DO  }
   END;  { PROCEDURE MoveStep }

   PROCEDURE InitializeGrid (enginetype, gas: INTEGER; alpha, vmax, vmin,
   pmax, pmin, tmax, tmin, smax, smin: REAL; vf, pf, tf, sf, work, heat,
   deltau: tarray; proc: parray; VAR vpmax, vpmin, ppmax, ppmin, tpmax,
   tpmin, spmax, spmin, sy, xleft, x1, x2, xright, ytop, ymiddle1,
   ymiddle2, ybottom, sxrow: INTEGER; VAR syrow: sarray);

    {****************************************************}
    {**             PROCEDURE InitializeGrid           **}
    {** sets up the screen and determines the plot     **}
    {** coordinate systems.                            **}
    {****************************************************}

      CONST
         sixb: 
         sixarray = ( chr ( 219 ) , chr ( 219 ) , chr ( 219 ) , chr ( 219 ) ,
                      chr ( 219 ) , chr ( 219 ) );
      VAR
         xwidth, ylength, i, i1, y: INTEGER;
         val: REAL;
         tex: STRING;
         ss, sp, sv, st: REAL;
         s1, t1, p1, v1: INTEGER;
         process: carray;
         TextInfo: TextSettingsType;
   BEGIN
      HideCursor;
      DefineViewPort(1, 0.05, 0.95, 0.5, 0.95);
      DefineScale(1, 0.05, 0.95, 0.05, 0.95);
      DefineViewPort(2, 0.05, 0.65, 0.05, 0.48);
      OpenViewPort(1);
      SelectScale(1);
      SelectViewPort(1);
      SetColor(white);

      { draw the lines which separates the different sections }

      map(0.05, 0.05, xleft, ybottom);
      map(0.95, 0.95, xright, ytop);

      x1 := Round((xright + xleft) / 2.0);
      x2 := Round(2 * (xright + xleft) / 3.0) + 32;
      ymiddle1 := Round((ytop + ybottom) / 2.0);
      xwidth := x1 - xleft;
      ylength := ybottom - ytop;
      Line(x1 - 5, ytop, x1 - 5, ybottom);
      SetColor(white);

      {*********************************************}
      { annote the different sections of the screen }
      {*********************************************}

           { T-S Diagram - upper left section }

      OutTextXY(xleft + 100, ytop + 10, 'T(K) versus S(J/K)');
      tpmin := ybottom - 35;
      tpmax := ytop + 25;
      spmin := xleft + 60;
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
         tex := NumStr(smin + i * (smax - smin) / 4.0, 4, 0);
         OutTextXY(i1 - 20, tpmin + 10, tex);
         i1 := tpmin + (i * (tpmax - tpmin)) DIV 4;
         Line(spmin, i1, spmin + 5, i1);
         Line(spmax, i1, spmax - 5, i1);
         tex := NumStr(tmin + i * (tmax - tmin) / 4.0, 4, 0);
         OutTextXY(spmin - 40, i1 - 4, tex);
      END;  { FOR i := 1 TO 3 DO }
      ss := (spmax - spmin) / (smax - smin);
      st := (tpmax - tpmin) / (tmax - tmin);
      GetTextSettings(TextInfo);
      tex := 'Entropy (J/K)';
      OutTextXY(xleft + 120, tpmin + 22, tex);
      tex := 'Temperature (K)';
      SetTextStyle(TextInfo.Font, 1, 1);
      OutTextXY(xleft + 15, tpmin - 140, tex);
      SetTextStyle(TextInfo.Font, 0, 1);
      {*********************************************}
           { P-V Diagram - upper right section }

      OutTextXY(x1 + 85, ytop + 10, 'P(atm) versus V(m3)');
      ppmin := ybottom - 35;
      ppmax := ytop + 25;
      vpmin := x1 + 60;
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
         tex := NumStr(vmin + i * (vmax - vmin) / 4.0, 4, 3);
         OutTextXY(i1 - 20, ppmin + 10, tex);
         i1 := ppmin + (i * (ppmax - ppmin)) DIV 4;
         Line(vpmin, i1, vpmin + 5, i1);
         Line(vpmax, i1, vpmax - 5, i1);
         tex := NumStr(pmin + i * (pmax - pmin) / 4.0, 4, 1);
         OutTextXY(vpmin - 40, i1 - 4, tex);
      END;  { FOR i := 1 TO 3 DO }
      sp := (ppmax - ppmin) / (pmax - pmin);
      sv := (vpmax - vpmin) / (vmax - vmin);
      tex := 'Volume (m3)';
      OutTextXY(x1 + 130, ppmin + 22, tex);
      tex := 'Pressure (atm)';
      SetTextStyle(TextInfo.Font, 1, 1);
      OutTextXY(x1 + 15, ppmin - 140, tex);
      SetTextStyle(TextInfo.Font, 0, 1);
      {*********************************************}
             { Summary - lower left section }
      OpenViewPort(2);
      SelectScale(1);
      SelectViewPort(2);

      map(0.05, 0.05, xleft, ybottom);
      map(0.95, 0.95, xright, ytop);
      tex := 'Summary - ';
      CASE gas OF
         1: 
            tex := tex + ' (Helium)';
         2: 
            tex := tex + ' (Argon)';
         3: 
            tex := tex + ' (Nitrogen)';
         4: 
            tex := tex + ' (Steam)';
      END;  { CASE gas of }
      IF (enginetype = 1) THEN
         tex := tex + ' Reversible Engine'
      ELSE
         tex := tex + ' Irr. Eng (' + NumStr(alpha * 100.0, 2, 0) + '% heat loss)';
      OutTextXY(xleft + 10, ytop + 10, tex);
      { compute the row locations }
      syrow[1] := ytop + 36;
      syrow[2] := ytop + 62;
      FOR i := 3 TO 6 DO
         syrow[i] := ytop + 88 + (i - 3) * 13;
      FOR i := 7 TO 9 DO
         syrow[i] := ytop + 154 + (i - 7) * 13;
      sxrow := xleft + 2;
      OutTextXY(sxrow, syrow[1], 'Step');
      OutTextXY(sxrow, syrow[2], 'Proc');
      OutTextXY(sxrow, syrow[3], 'Pf');
      OutTextXY(sxrow, syrow[4], 'Vf');
      OutTextXY(sxrow, syrow[5], 'Tf');
      OutTextXY(sxrow, syrow[6], 'Sf');
      OutTextXY(sxrow, syrow[7], 'Work');
      OutTextXY(sxrow, syrow[8], 'Heat');
      OutTextXY(sxrow, syrow[9], 'DeltaU');

      {*********************************************}
      { output the initial conditions }
      proc[0] := 0;
      StepOutput(0, proc[0], pf, vf, tf, sf, work, heat, deltau, sxrow, syrow);
      SelectViewPort(1);
      SelectScale(1);
      s1 := trunc(ss * (sf[0] - smin) + spmin);
      t1 := trunc(st * (tf[0] - tmin) + tpmin);
      SetFillStyle(SolidFill, lightred);
      FillEllipse(s1, t1, 2, 2);
      p1 := trunc(sp * (pf[0] - pmin) + ppmin);
      v1 := trunc(sv * (vf[0] - vmin) + vpmin);
      SetFillStyle(SolidFill, lightred);
      FillEllipse(v1, p1, 2, 2);
      ShowCursor;
   END;  { PROCEDURE Initializegrid }

   PROCEDURE PlotCurves (step: INTEGER; vmax, vmin, pmax, pmin, tmax,
   tmin, smax, smin: REAL; vpmax, vpmin, ppmax, ppmin, tpmax, tpmin,
   spmax, spmin, x2, ymiddle, size: INTEGER; p, s, t, v: plotarray);


    {*****************************************************}
    {**               PROCEDURE PlotCurves              **}
    {** inputs the (v,p) and (s,t) curves and draws them**}
    {** in the coordinate systems in the upper half of  **}
    {** the screen.                                     **}
    {*****************************************************}

      VAR
         ss, sp, st, sv: REAL;
         i, s1, t1, v1, p1, s2, t2, v2, p2: INTEGER;
         t1p, t2p, s1p, s2p, v1p, v2p, p1p, p2p: INTEGER;
   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      ss := (spmax - spmin) / (smax - smin);
      sp := (ppmax - ppmin) / (pmax - pmin);
      sv := (vpmax - vpmin) / (vmax - vmin);
      st := (tpmax - tpmin) / (tmax - tmin);
      FOR i := 2 TO size DO
      BEGIN
         s1 := trunc(ss * (s[step, i - 1] - smin) + spmin);
         s2 := trunc(ss * (s[step, i] - smin) + spmin);
         t1 := trunc(st * (t[step, i - 1] - tmin) + tpmin);
         t2 := trunc(st * (t[step, i] - tmin) + tpmin);
         p1 := trunc(sp * (p[step, i - 1] - pmin) + ppmin);
         p2 := trunc(sp * (p[step, i] - pmin) + ppmin);
         v1 := trunc(sv * (v[step, i - 1] - vmin) + vpmin);
         v2 := trunc(sv * (v[step, i] - vmin) + vpmin);
         SetColor(white);
         Line(s1, t1, s2, t2);
         Line(v1, p1, v2, p2);
      END;
   END;  { PROCEDURE PlotCurves }

   PROCEDURE EndPoint (itype, step: INTEGER; vmax, vmin, pmax, pmin, tmax,
   tmin, smax, smin: REAL; vpmax, vpmin, ppmax, ppmin, tpmax, tpmin,
   spmax, spmin, p, s, t, v: REAL);
     {******************************************************************}
     {**                     PROCEDURE EndPoint                       **}
     {** points a red circle at the end point of a process.           **}
     {******************************************************************}
      VAR
         ss, sp, st, sv: REAL;
         s1, t1, v1, p1, cir: INTEGER;
   BEGIN
      SelectViewPort(1);
      SelectScale(1);
      ss := (spmax - spmin) / (smax - smin);
      sp := (ppmax - ppmin) / (pmax - pmin);
      sv := (vpmax - vpmin) / (vmax - vmin);
      st := (tpmax - tpmin) / (tmax - tmin);
      s1 := trunc(ss * (s - smin) + spmin);
      t1 := trunc(st * (t - tmin) + tpmin);
      p1 := trunc(sp * (p - pmin) + ppmin);
      v1 := trunc(sv * (v - vmin) + vpmin);
      IF (itype <> 3) THEN
         SetColor(white)
      ELSE
         SetColor(Red);
      IF (itype <> 2) THEN
         SetFillStyle(SolidFill, lightred)
      ELSE
         SetFillStyle(SolidFill, white);
      IF (itype = 1) THEN
         cir := 2
      ELSE
         cir := 0;
      FillEllipse(s1, t1, cir, cir);
      FillEllipse(v1, p1, cir, cir);
   END;  { PROCEDURE EndPoint }

   PROCEDURE Adiabatic (func, step: INTEGER; atm, cv, gamma, R: REAL;
   VAR tf, pf, vf, sf: tarray; VAR tp, pp, vp, sp: plotarray;
   VAR heat, work, delatu: tarray);
      VAR
         i: INTEGER;
         dv: REAL;
   BEGIN
       { compute final tf,pf,vf and sf }
      IF (func = 1) THEN    { given final temperature }
      BEGIN
         vf[step] := vf[step - 1] * Power((tf[step - 1] / tf[step]), 1.0 / (gamma - 1.0), error);
         pf[step] := R * tf[step] / (atm * vf[step]);
         sf[step] := sf[step - 1];
      END
      ELSE IF (func = 2) THEN    { given final pressure }
      BEGIN
         vf[step] := vf[step - 1] * Power((pf[step - 1] / pf[step]), 1.0 / gamma, error);
         tf[step] := atm * vf[step] * pf[step] / R;
         sf[step] := sf[step - 1];
      END
      ELSE    { given final volume }
      BEGIN
         tf[step] := tf[step - 1] * Power((vf[step - 1] / vf[step]), (gamma - 1.0), error);
         pf[step] := R * tf[step] / (atm * vf[step]);
         sf[step] := sf[step - 1];
      END;
       { compute heat exchanged, work done and change in interal energy }
      heat[step] := 0.0;
      work[step] := (pf[step - 1] * vf[step - 1] - pf[step] * vf[step]) / (gamma - 1.0);
       { convert work from atm-m3 to ergs }
      work[step] := work[step] * atm;
      deltau[step] := cv * (tf[step] - tf[step - 1]);
       { compute points along the P-V and T-S curves }
      dv := (vf[step] - vf[step - 1]) / 20.0;
      FOR i := 1 TO 21 DO
      BEGIN
         vp[step, i] := vf[step - 1] + (i - 1) * dv;
         pp[step, i] := pf[step - 1] * Power((vf[step - 1] / vp[step, i]), gamma, error);
         tp[step, i] := tf[step - 1] * Power((vf[step - 1] / vp[step, i]),
                        (gamma - 1.0), error);
         sp[step, i] := sf[step - 1];
      END;
   END;  { PROCEDURE Adiabatic }

   PROCEDURE Isobaric (func, step: INTEGER; alpha, atm, cv, cp: REAL;
   VAR tf, pf, vf, sf: tarray; VAR tp, pp, vp, sp: plotarray;
   VAR heat, work, delatu: tarray);
      VAR
         i: INTEGER;
         dv: REAL;
   BEGIN
       { compute final tf,pf,vf and sf }
      IF (func = 1) THEN    { given final temperature }
      BEGIN
         vf[step] := vf[step - 1] * tf[step] / tf[step - 1];
         pf[step] := pf[step - 1];
         sf[step] := sf[step - 1] + cp * ln(tf[step] / tf[step - 1]);
      END
      ELSE    { given final volume }
      BEGIN
         tf[step] := tf[step - 1] * vf[step] / vf[step - 1];
         pf[step] := pf[step - 1];
         sf[step] := sf[step - 1] + cp * ln(tf[step] / tf[step - 1]);
      END;
       { compute heat exchanged, work done and change in interal energy }
      heat[step] := cp * (tf[step] - tf[step - 1]);
      IF (heat[step] > 0.0) THEN
         heat[step] := heat[step] * (1.0 + alpha);
      work[step] := pf[step - 1] * (vf[step] - vf[step - 1]);
       { convert work from atm-m3 to ergs }
      work[step] := work[step] * atm;
      deltau[step] := cv * (tf[step] - tf[step - 1]);
       { compute points along the P-V and T-S curves }
      dv := (vf[step] - vf[step - 1]) / 20.0;
      FOR i := 1 TO 21 DO
      BEGIN
         vp[step, i] := vf[step - 1] + (i - 1) * dv;
         pp[step, i] := pf[step - 1];
         tp[step, i] := tf[step - 1] * vp[step, i] / vf[step - 1];
         sp[step, i] := sf[step - 1] + cp * ln(tp[step, i] / tf[step - 1]);
      END;
   END;  { PROCEDURE Isobaric }

   PROCEDURE Isochoric (func, step: INTEGER; alpha, atm, cv: REAL;
   VAR tf, pf, vf, sf: tarray; VAR tp, pp, vp, sp: plotarray;
   VAR heat, work, delatu: tarray);
      VAR
         i: INTEGER;
         dt: REAL;
   BEGIN
       { compute final tf,pf,vf and sf }
      IF (func = 1) THEN    { given final temperature }
      BEGIN
         vf[step] := vf[step - 1];
         pf[step] := tf[step] * pf[step - 1] / tf[step - 1];
         sf[step] := sf[step - 1] + cv * ln(tf[step] / tf[step - 1]);
      END
      ELSE IF (func = 2) THEN    { given final pressure }
      BEGIN
         vf[step] := vf[step - 1];
         tf[step] := pf[step] * tf[step - 1] / pf[step - 1];
         sf[step] := sf[step - 1] + cv * ln(tf[step] / tf[step - 1]);
      END;
       { compute heat exchanged, work done and change in interal energy }
      heat[step] := cv * (tf[step] - tf[step - 1]);
      IF (heat[step] > 0.0) THEN
         heat[step] := heat[step] * (1.0 + alpha);
      work[step] := 0.0;
      deltau[step] := cv * (tf[step] - tf[step - 1]);
       { compute points along the P-V and T-S curves }
      dt := (tf[step] - tf[step - 1]) / 20.0;
      FOR i := 1 TO 21 DO
      BEGIN
         vp[step, i] := vf[step - 1];
         tp[step, i] := tf[step - 1] + (i - 1) * dt;
         pp[step, i] := pf[step - 1] * tp[step, i] / tf[step - 1];
         sp[step, i] := sf[step - 1] + cv * ln(tp[step, i] / tf[step - 1]);
      END;
   END;  { PROCEDURE Isochoric }

   PROCEDURE Isothermal (func, step: INTEGER; alpha, cv, R: REAL;
   VAR tf, pf, vf, sf: tarray;
   VAR tp, pp, vp, sp: plotarray; VAR heat, work, delatu: tarray);
      VAR
         i: INTEGER;
         dv: REAL;
   BEGIN
       { compute final tf,pf,vf and sf }
      IF (func = 3) THEN    { given final volume }
      BEGIN
         tf[step] := tf[step - 1];
         pf[step] := vf[step - 1] * pf[step - 1] / vf[step];
         sf[step] := sf[step - 1] + R * ln(vf[step] / vf[step - 1]);
      END
      ELSE IF (func = 2) THEN    { given final pressure }
      BEGIN
         tf[step] := tf[step - 1];
         vf[step] := vf[step - 1] * pf[step - 1] / pf[step];
         sf[step] := sf[step - 1] + R * ln(vf[step] / vf[step - 1]);
      END;
       { compute heat exchanged, work done and change in interal energy }
      heat[step] := tf[step] * (sf[step] - sf[step - 1]);
      IF (heat[step] > 0.0) THEN
         heat[step] := heat[step] * (1.0 + alpha);
      work[step] := R * tf[step] * ln(vf[step] / vf[step - 1]);
      deltau[step] := 0.0;
       { compute points along the P-V and T-S curves }
      dv := (vf[step] - vf[step - 1]) / 20.0;
      FOR i := 1 TO 21 DO
      BEGIN
         vp[step, i] := vf[step - 1] + (i - 1) * dv;
         tp[step, i] := tf[step];
         pp[step, i] := pf[step - 1] * vf[step - 1] / vp[step, i];
         sp[step, i] := sf[step - 1] + R * ln(vp[step, i] / vf[step - 1]);
      END;
   END;  { PROCEDURE Isothermal }

   PROCEDURE PrintSummary (alpha, amu, cv, cp, mass, totalwork, totalheat,
    heatin, heatout, efficiency: REAL; enginetype, gas, step: INTEGER;
    tf, pf, sf, vf, heat, work, deltau: tarray; proc: parray);

    {*****************************************************}
    {**             PROCEDURE SummaryOutput             **}
    {** outputs the engine's or refrigerator's performance}
    {*****************************************************}

      VAR
         tex: STRING;

      CONST
         process: ARRAY[0..4] OF STRING[4]
         = ( '----' , 'adia' , 'isob' , 'isoc' , 'isot' );

   BEGIN
      writeln(Output);
      writeln(Output, ' Summary ');
      writeln(Output);
      IF (enginetype = 1) THEN
         tex := 'Reversible'
      ELSE
         tex := 'Irreversible';
      writeln(Output, ' Engine Type: ', tex);
      CASE gas OF
         1: 
            tex := 'Helium';
         2: 
            tex := 'Argon';
         3: 
            tex := 'Nitrogen';
         4: 
            tex := 'Steam';
      END;  { CASE gas OF }
      writeln(Output, ' Gas: ', tex);
      IF (enginetype = 2) THEN
         writeln(Output, ' % of heat loss: ', 100 * alpha : 2 : 0);
      writeln(Output, ' cv: (in J/K) ', cv : 17);
      writeln(Output, ' gamma: ', cp / cv : 17);
      writeln(Output, ' mass: (in amus) ', mass / amu : 17);
      writeln(Output);
      writeln(Output, '  proc  volume (m3) /     pressure (atm)/  ' + 'temperature (K)  / entropy (J/K)');
      writeln(Output, '        work (J)          heat (J)         ' + 'delta u (J)  ');
      writeln(Output);

      FOR i := 0 TO step DO
      BEGIN
         IF (i = 0) THEN
            writeln(Output, i, ' ', '----', ' ', vf[i] : 17, ' ', pf[i] : 17, ' ', tf[i] : 17, ' ', sf[i] : 14)
         ELSE
            writeln(Output, i, ' ', process[proc[i]], ' ', vf[i] : 17, ' ', pf[i] : 17, ' ', tf[i] : 17, ' ', sf[i] : 14);
         writeln(Output, '       ', work[i] : 17, ' ', heat[i] : 17, ' ', deltau[i] : 17);
         writeln(Output);
      END;  { FOR i := 1 TO step DO }

      IF (totalwork > 0.0) THEN
      BEGIN      { engine }
         writeln(Output, ' engine ');
         writeln(Output, ' total work: ', totalwork, ' j ');
         writeln(Output, ' total heat: ', totalheat, ' j ');
         writeln(Output, ' heat in:    ', heatin, ' j ');
         writeln(Output, ' efficiency: ', efficiency);
      END  { IF (totalwork > 0.0) THEN }
      ELSE        { refrigerator }
      BEGIN
         writeln(Output, ' refrigerator ');
         writeln(Output, ' work done: ', -totalwork, ' j');
         writeln(Output, ' heat extracted: ', heatin, ' j');
         writeln(Output, ' coefficient of performance: ',
                          heatin / (-totalwork * (1.0 + alpha)));
      END;  { refrigerator }
   END;  { PROCEDURE PrintSummary }

   PROCEDURE SummaryOutput (alpha, totalwork, totalheat, heatin, heatout: REAL);

    {*****************************************************}
    {**             PROCEDURE SummaryOutput             **}
    {** outputs the engine's or refrigerator's performance}
    {*****************************************************}

      VAR
         x, y, ytop, xleft: INTEGER;
         tex: STRING;
   BEGIN
      DefineViewPort(3, 0.65, 1.00, 0.05, 0.48);
      DefineScale(3, 0.05, 0.95, 0.05, 0.48);
      OpenViewPort(3);
      SelectScale(3);
      SelectViewPort(3);
      map(0.05, 0.48, xleft, ytop);
      IF (totalwork > 0.0) THEN      {  ENGINE }
      BEGIN
         OutTextXY(xleft + 10, ytop + 13, 'Engine Performance');
         OutTextXY(xleft + 10, ytop + 26, '    Summary       ');
         OutTextXY(xleft + 10, ytop + 52, 'Total Work :');
         tex := NumStr(totalwork, 7, 0);
         OutTextXY(xleft + 10, ytop + 65, tex);
         OutTextXY(xleft + 10, ytop + 91, 'Total Heat');
         OutTextXY(xleft + 10, ytop + 104, '  Absorbed :');
         tex := NumStr(heatin, 7, 0);
         OutTextXY(xleft + 10, ytop + 117, tex);
         OutTextXY(xleft + 10, ytop + 142, 'Engine Efficiency');
         IF (abs(heatin) > 0.0) THEN
            tex := NumStr(totalwork / heatin, 5, 4)
         ELSE
            tex := 'undefined';
         OutTextXY(xleft + 10, ytop + 155, tex);
      END  { IF (totalwork > 0.0) THEN }
      ELSE      { refrigerator }
      BEGIN
         OutTextXY(xleft + 10, ytop + 13, 'Refrigerator');
         OutTextXY(xleft + 10, ytop + 26, 'Performance  ');
         OutTextXY(xleft + 10, ytop + 52, 'Work Done:');
         tex := NumStr(-totalwork, 7, 0);
         OutTextXY(xleft + 10, ytop + 65, tex);
         OutTextXY(xleft + 10, ytop + 91, 'Heat');
         OutTextXY(xleft + 10, ytop + 104, 'Extracted :');
         tex := NumStr(heatin, 7, 0);
         OutTextXY(xleft + 10, ytop + 117, tex);
         OutTextXY(xleft + 10, ytop + 142, 'Coefficient');
         OutTextXY(xleft + 10, ytop + 155, 'of Performance');
         IF (abs(totalwork) > 0.0) THEN
            tex := NumStr(heatin / (-totalwork * (1.0 + alpha)), 5, 3)
         ELSE
            tex := 'undefined';
         OutTextXY(xleft + 10, ytop + 168, tex);
      END;  { ELSE - refrigerator }
   END;  { PROCEDURE SummaryOutput }

   PROCEDURE OutOfBounds (step: INTEGER; tf, pf, vf, sf: tarray; tmin,
   tmax, pmin, pmax, vmin, vmax, smin, smax: REAL; VAR itype: INTEGER);
   BEGIN
      itype := 0;
      IF ((sf[step] < smin) OR (sf[step] > smax)) THEN
      BEGIN
         announce('entropy out of range');
         itype := 1;
      END;
      IF ((tf[step] < tmin) OR (tf[step] > tmax)) THEN
      BEGIN
         announce('temperature out of range');
         itype := itype + 2;
      END;
      IF ((vf[step] < vmin) OR (vf[step] > vmax)) THEN
      BEGIN
         announce('volume out of range');
         itype := itype + 10;
      END;
      IF ((pf[step] < pmin) OR (pf[step] > pmax)) THEN
      BEGIN
         announce('pressure out of range');
         itype := itype + 20;
      END;
   END;   {* Procedure OutOfBound *}

   PROCEDURE SlidersMoved;
   BEGIN
      itype := 0;
      IF (step = 0) THEN
         step := 1;
      HandleSliders(process, step, isliderc, func, tf, pf, vf);
      IF (isliderc = 1) THEN
      BEGIN
         islider := 1;
         proc[step] := process;
         CASE process OF
            1: 
            BEGIN
               Adiabatic(func, step, atm, cv, gamma, R, tf, pf, vf, sf, tp,
                         pp, vp, sp, heat, work, deltau);
               OutOfBounds(step, tf, pf, vf, sf, tmin, tmax, pmin, pmax,
                         vmin, vmax, smin, smax, itype);
               IF (itype = 0) THEN
               BEGIN
                  told := tf[step];
                  vold := vf[step];
                  pold := pf[step];
                  sold := sf[step];
               END
               ELSE
               BEGIN
                  tf[step] := told;
                  vf[step] := vold;
                  pf[step] := pold;
                  sf[step] := sold;
               END;
               sliders.reset(1, tf[step]);
               sliders.reset(2, vf[step]);
               sliders.reset(3, pf[step]);
            END;
            2: 
            BEGIN
               Isobaric(func, step, alpha, atm, cv, cp, tf, pf, vf, sf, tp,
                        pp, vp, sp, heat, work, deltau);
               OutOfBounds(step, tf, pf, vf, sf, tmin, tmax, pmin, pmax,
                        vmin, vmax, smin, smax, itype);
               IF (itype = 0) THEN
               BEGIN
                  vold := vf[step];
                  told := tf[step];
                  sold := sf[step];
               END
               ELSE
               BEGIN
                  tf[step] := told;
                  vf[step] := vold;
                  pf[step] := pold;
                  sf[step] := sold;
               END;
               sliders.reset(2, vf[step]);
               sliders.reset(1, tf[step]);
            END;
            3: 
            BEGIN
               Isochoric(func, step, alpha, atm, cv, tf, pf, vf, sf, tp, pp,
                         vp, sp, heat, work, deltau);
               OutOfBounds(step, tf, pf, vf, sf, tmin, tmax, pmin, pmax,
                         vmin, vmax, smin, smax, itype);
               IF (itype = 0) THEN
               BEGIN
                  told := tf[step];
                  pold := pf[step];
                  sold := sf[step];
               END
               ELSE
               BEGIN
                  tf[step] := told;
                  vf[step] := vold;
                  pf[step] := pold;
                  sf[step] := sold;
               END;
               sliders.reset(3, pf[step]);
               sliders.reset(1, tf[step]);
            END;
            4: 
            BEGIN
               Isothermal(func, step, alpha, cv, R, tf, pf, vf, sf, tp, pp,
                          vp, sp, heat, work, deltau);
               OutOfBounds(step, tf, pf, vf, sf, tmin, tmax, pmin, pmax,
                          vmin, vmax, smin, smax, itype);
               IF (itype = 0) THEN
               BEGIN
                  pold := pf[step];
                  vold := vf[step];
                  sold := sf[step];
               END
               ELSE
               BEGIN
                  tf[step] := told;
                  vf[step] := vold;
                  pf[step] := pold;
                  sf[step] := sold;
               END;
               sliders.reset(2, vf[step]);
               sliders.reset(3, pf[step]);
            END;
         END;   { CASE process OF }
         IF (itype = 0) THEN
         BEGIN
            SelectScale(1);
            SelectViewPort(1);
            IF (tpold > 0.0) THEN
               EndPoint(2, step, vmax, vmin, pmax, pmin, tmax, tmin,
               smax, smin, vpmax, vpmin, ppmax, ppmin, tpmax, tpmin,
               spmax, spmin, ppold, spold, tpold, vpold);
            PlotCurves(step, vmax, vmin, pmax, pmin, tmax, tmin, smax,
            smin, vpmax, vpmin, ppmax, ppmin, tpmax, tpmin, spmax, spmin,
            x2, ymiddle1, 21, pp, sp, tp, vp);
            ppold := pp[step, 21];
            vpold := vp[step, 21];
            tpold := tp[step, 21];
            spold := sp[step, 21];
            EndPoint(3, step, vmax, vmin, pmax, pmin, tmax, tmin, smax,
            smin, vpmax, vpmin, ppmax, ppmin, tpmax, tpmin, spmax, spmin,
            pp[step, 21], sp[step, 21], tp[step, 21], vp[step, 21]);
         END;
      END;  {* IF (isliderc = 1) *}
   END; {* PROCEDURE SlidersMoved *}

   PROCEDURE ButtonPressed;
   BEGIN
      itype := 0;
      sliders.DrawAll;
      processold := process;
      Process := buttons.number;
      IF (tf[step] > 0.0) THEN
      BEGIN
         sliders.reset(1, tf[step]);
         sliders.reset(2, vf[step]);
         sliders.reset(3, pf[step]);
      END
      ELSE
      BEGIN
         sliders.reset(1, tf[step - 1]);
         sliders.reset(2, vf[step - 1]);
         sliders.reset(3, pf[step - 1]);
      END;
      CASE process OF
         2: 
            sliders.erase(3, black);
         3: 
            sliders.erase(2, black);
         4: 
            sliders.erase(1, black);
      END;  { CASE process OF }
      IF (islider = 1) THEN
      BEGIN
         IF ((processold <> process) AND (step > 0)) THEN
         BEGIN
            EndPoint(1, step, vmax, vmin, pmax, pmin, tmax, tmin, smax,
            smin, vpmax, vpmin, ppmax, ppmin, tpmax, tpmin, spmax, spmin,
            pp[step, 21], sp[step, 21], tp[step, 21], vp[step, 21]);
            SelectScale(2);
            SelectViewPort(2);
            IF (step > 4) THEN
               MoveStep(step, proc, pf, vf, tf, sf, work, heat, deltau,
                        sxrow, syrow);
            StepOutput(step, proc[step], pf, vf, tf, sf, work, heat,
                       deltau, sxrow, syrow);
         END; { IF ((processold <> process) ... }
         step := step + 1;
         islider := 0;
      END;  { IF (islider = 1) THEN }
   END;  { PROCEDURE ButtonPressed }

   PROCEDURE Initialize;
   BEGIN
      tf[0] := 300.0;
      pf[0] := 1.0;
      EngineType := 1;
      gas := 3;
      alpha := 0;
    { value for constants was taken from the 72nd Edition of the }
    { CRC Handbook of Chemistry and Physics                      }
      Navg := 6.0221367;{*Power(10.0,23,error)}
  { Avogadro's Number }
      R := 8.314510;                             { Gas Constant }
      kB := 1.3800658;{*Power(10.0,-23,error)}
   { Boltzmann's Constant }
      hP := 6.6260755;{*Power(10.0,-34,error)}
   { Planck's Constant }
      amu := 1.6605402;{*Power(10.0,-27,error)}
  { Conversion from amu to kgms }
      atm := 1.01325 * Power(10.0, 5, error);   { Conversion from j-m3 to atm}
   END;  {  PROCEDURE Initialize  }

   PROCEDURE ResetVariables;
      VAR
         i: INTEGER;
   BEGIN
      amass := massgas[gas];
      mass := amass * amu;
      cv := cvgas[gas] * R;
      gamma := gammagas[gas];
      cp := cv * gamma;
      vmin := 0.00;
      vmax := R * tf[0] / atm;
      IF (gas = 4) THEN
         tmin := 500
      ELSE
         tmin := 300.0;
      tmax := 2100.0;
      pmin := 1.0;
      pmax := 75.0;
      lam := hP / SQRT(2.0 * 3.1415926 * mass * kB);
      smin := 3.0 * R / 2.0;
      smax := smin + R * ln(exp(1) * vmax *
              Power(tmax / lam, 1.5, error) * 10000.0 / Navg);
      vf[0] := R * tf[0] / (pf[0] * atm);
      sf[0] := smin + R * ln(exp(1) * vf[0] * Power(tf[0] / lam, 1.5, error));
      FOR i := 1 TO 20 DO
      BEGIN
         tf[i] := 0.0;
         pf[i] := 0.0;
      END;
      work[0] := 0.0;
      heat[0] := 0.0;
      deltau[0] := 0.0;
      totalwork := 0.0;
      totalheat := 0.0;
      totalenergy := 0.0;
      heatin := 0.0;
      heatout := 0.0;
      step := 0;
      endcycle := 0;
      slideropen := 0;
      restart := false;
      told := tf[0];
      pold := pf[0];
      vold := vf[0];
      sold := sf[0];
      tpold := 0;
      ppold := 0;
      vpold := 0;
      spold := 0;
   END;  { PROCEDURE ResetVariables }

BEGIN  { PROGRAM ENGINE }
   CUPSinit;
   ABOUTPROGRAM;
   initialize;
   REPEAT
      ClearMuppetPort;
      error := 0;
      IF (first = true) THEN
      BEGIN
         ReadData;
         first := false;
         ASSIGN(Output, Engine_Output);
         REWRITE(Output);
      END
      ELSE
         ReadData1;
      IF (quitf = false) THEN
      BEGIN
         ClearMuppetPort;
         ResetVariables;
         SetUpMyMenu;
         SetUpHotKeys(HotKeys);
         SetUpSliders;
         SetUpButtons;
         InitializeGrid(enginetype, gas, alpha, vmax, vmin, pmax, pmin,
         tmax, tmin, smax, smin, vf, pf, tf, sf, work, heat, deltau, proc,
         vpmax, vpmin, ppmax, ppmin, tpmax, tpmin, spmax, spmin, sy,
         xleft, x1, x2, xright, ytop, ymiddle1, ymiddle2, ybottom, sxrow,
         syrow);
         processold := 0;
         process := 0;
         funcold := -1;
         func := 0;
         stepold := 0;
         step := 0;
         itype := 0;
         islider := 0;
         isliderc := 0;
         REPEAT
            CheckForEvents;
            IF MyMenuOut.Activated THEN
               HandleMenu;
            IF HotKeys.Pressed(key) THEN
               HandleHotKeys(key);
            IF Buttons.Changed THEN
               ButtonPressed;
            IF (process <> 0) AND Sliders.Changed THEN
               SlidersMoved;
            IF ((ABS(tf[step] - tf[0]) < 2.0) AND
                (ABS(pf[step] - pf[0]) < 0.1) AND (step > 1)) THEN
               endcycle := 1;
         UNTIL ((quitf) OR (endcycle = 1) OR (restart));
         IF (endcycle = 1) THEN
         BEGIN
            EndPoint(1, step, vmax, vmin, pmax, pmin, tmax, tmin, smax,
            smin, vpmax, vpmin, ppmax, ppmin, tpmax, tpmin, spmax, spmin,
            pp[step, 21], sp[step, 21], tp[step, 21], vp[step, 21]);
            SelectScale(2);
            SelectViewPort(2);
            IF (step > 4) THEN
               MoveStep(step, proc, pf, vf, tf, sf, work, heat, deltau,
               sxrow, syrow);
            StepOutput(step, proc[step], pf, vf, tf, sf, work, heat,
            deltau, sxrow, syrow);
            FOR i := 1 TO step DO
            BEGIN
               totalwork := totalwork + work[i];
               totalheat := totalheat + heat[i];
               IF (heat[i] > 0.0) THEN
                  heatin := heatin + heat[i];
               IF (heat[i] < 0.0) THEN
                  heatout := heatout - heat[i];
               totalenergy := totalenergy + deltau[step];
            END;  {  FOR i := 1 TO step DO }
            IF (abs(heatin) > 0.0) THEN
               efficiency := totalwork / heatin;
            SummaryOutput(alpha, totalwork, totalheat, heatin, heatout);
            PrintSummary(alpha, amu, cv, cp, mass, totalwork, totalheat,
            heatin, heatout, efficiency, enginetype, gas, step, tf, pf,
            sf, vf, heat, work, deltau, proc);
         END;  { IF (endcycle = 1) }
         IF ((NOT quitf) AND (NOT restart)) THEN
            REPEAT
               CheckForEvents;
               IF MyMenuOut.Activated THEN
                  HandleMenu;
            UNTIL ((quitf) OR (restart));
         buttons.done;
         sliders.done;
         alpha := alpha * 100.0;
      END;  {* IF (quitf = false) *}
   UNTIL (quitf);
   Close(Output);
   CUPSDone;
END.  { PROGRAM ENGINE }