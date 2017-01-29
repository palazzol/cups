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

Program qmgas2;

(*    CUPS Project}
{}
{      Thermal Physics Programs}
{      Interactive program featuring:}
{      }
{          Calculation of properties of Quantum ideal particles}
{          }
{          }
{      }
{      October 10 1994}
{}
{      by:  Jan Tobochnik}
{           Kalamazoo College}
{      }
{      programming by:  Jan Tobochnik}
{                        }
{                        }
{ *)

    Uses

       crt, graph, CUPS, CUPSmupp, CUPSgui, CUPSgrph, CUPSproc, CUPSfunc;


(*------------------ DECLARATIONS ---------------------*)
    Const
      {  pi = 3.141592654;}
        maxNp = 1000;
        maxstates = 5000; {2000}
        whitte = white;
        blakk  = black;
    Type
        ParticleType = (BE, FD, MB);
        MassType = (Massive, Massless);
        staterec = Record
                E: real;
                kx, ky, kz, occupied: integer;
            End;
        stateptr = ^staterec;
        statetype = Array[1..maxstates] Of stateptr;
        dtype = Array[1..maxstates] Of longint;
        stype = Array[0..maxNp] Of longint;
        resultrec = Record
                ecum, ecum2, accept: real;
                ndat,ndat0: longint;
            End;

    Var
        Particle: ParticleType;
        Distribution: dtype;
        StateOf: stype;
        Statelist: statetype;
        beta, T, n, Energy, k2max, Emax: real;
        results: resultrec;
        active, Np: longint;
        d: integer;
        ch: char;
        RandomSeed, RandomSeed0: integer;
        kxm, kym, kzm, nstate: integer;
        hotkeys: thotkeys;
        changetemp: tslider;
        Menu: TMenu;
        started, quit,running: boolean;
        Escale: real;
        delaytime:real;


(*--------------------- Information Screens ---------------------------*)

    Procedure AboutProgram;
        Var
            i: integer;
            C: HelpScrType;

    Begin {AboutProgram}
        For i := 1 To 25 Do
            C[i] := '';
        C[2] := '     MONTE CARLO SIMULATION OF QUANTUM';
        C[3] := '             IDEAL PARTICLES';
        C[5] := '                    by  ';
        C[7] := '              Jan Tobochnik  ';
        C[8] := '            Kalamazoo College  ';
        C[12] := '       (c) 1995 John Wiley & Sons, Inc.';
        C[15] := '  This program simulates particles with no';
        C[16] := '  interparticle potential, but which obey ';
        C[17] := '  Bose-Einstein(BE), Fermi-Dirac(FD) or';
        C[18] := '  Maxwell-Boltzmann(MB) statistics. The ';
        C[19] := '  occupancy in momentum space is displayed';
        C[20] := '  as well as thermodynamic data. Initially,';
        C[21] := '  you will be asked to specify the system you';
        C[22] := '  wish to see simulated.';
        C[24] := '     Press any Key or Click Mouse to continue';
        HELP(C);
    End; {AboutProgram}

    Procedure AboutProgram2;
        Var
            i: integer;
            C: HelpScrType;
    Begin {ExplainSim}
        For i := 1 To 25 Do
            C[i] := '';
        C[3] := ' We simulate a small number of particles';
        C[4] := ' in the canonical(constant temperature and ';
        C[5] := ' particle number) ensemble. First you  ';
        C[6] := ' choose a particle type, FD, BE, or MB, the ';
        C[7] := ' dispersion relation, the dimensions of phase ';
        C[8] := ' space, the number of particles and the initial ';
        C[9] := ' temperature. The program then orders the states';
        C[10] := ' in phase space from lowest to highest energy. ';
        C[11] := ' Then using a Monte Carlo algorithm the  ';
        C[12] := ' particles move around in phase space. The ';
        C[13] := ' occupancy in phase space is shown as the';
        C[14] := ' particles move. You can then graph the  ';
        C[15] := ' distribution function and see the mean energy';
        C[16] := ' and specific heat by choosing the UPDATE hotkey.';
        C[17] := ' The number of Monte Carlo steps per particle(MCS)';
        C[18] := ' and the fraction of attempted moves accepted(Acc)';
        C[19] := ' are also shown. These calculations are reset';
        C[20] := ' whenever the temperature is changed or you choose';
        C[21] := ' the RESET hotkey. A separate window shows the';
        C[22] := ' instantaneous and average energy per particle.';
        C[24] := '   Press any Key or Click Mouse for more help';
        HELP(C);
    End; {AboutProgram}

    Procedure Helpscreen;
        Var
            i: integer;
            C: HelpScrType;

    Begin {Helpscreen}
        For i := 1 To 25 Do
            C[i] := '';
        C[2] := '  <E>/N = mean energy per particle';
        C[3] := '   E/N = instantaneous energy per particle';
        C[4] := '   T = absolute temperature';
        C[5] := '   C = specific heat   ';
        C[6] := '   MCS = number of MC steps per particle';
        C[7] := '   ACC = fraction of attempted moves accepted';
        C[8] := '   f(0) = mean occupation of ground state';
        C[9] := '   f = distribution function';
        C[11] := '  Horizontal axis of lower right window is';
        C[12] := '  number of MC steps mod 1000';
        C[14] := '  To increase temperature beyond slider ';
        C[15] := '  maximum, choose Specify System from SYSTEM';
        C[16] := '  menu, and increase initial temperature in ';
        C[17] := '  input screen.';
        c[19] := '  Reset zeros the accumulation of data and ';
        c[20] := '  should be used to start collecting data ';
        c[21] := '  after allowing the system to equilibrate.';
        C[24] := '     Press any Key or Click Mouse to continue';
        HELP(C);
    End; {Helpscreen}



{*****-------------------Small useful functions-------------*****}

    Function imin (x, y: integer): integer;
    Begin
        If x < y Then
            imin := x
        Else
            imin := y;
    End;

    Function imax (x, y: integer): integer;
    Begin
        If x > y Then
            imax := x
        Else
            imax := y;
    End;

    Function iran (Var iseed: integer;
                                    low, high: integer): integer;
    Begin
        iran := trunc(low + (high - low + 1) * urand(iseed));
    End;



{--------------------- Initialization Procedures-----------------------------}

    Procedure initialparameters;
    Begin
        Particle := BE;
        n := 2;
        kxm := 9;
        kym := 9;
        kzm := 0;
        d := 2;
        Np := 50;
        T := 1.50;
        RandomSeed0 := 12345;
        delaytime:= 1;  {pause between MC moves}
    End;

    Procedure disposestates (Var Statelist: statetype);
        Var
            i: integer;
    Begin
        For i := 1 To nstate Do
            dispose(statelist[i]);
    End;

    Procedure ZeroVariables (Var Distribution: dtype;
                                    Var results: resultrec);
        Var
            i: integer;
    Begin {zeroVariables}
        With results Do
            Begin
                ndat := 0;
                ecum := 0.0;
                ecum2 := 0.0;
                accept := 0.0;
            End;
        For i := 1 To nstate Do
            Distribution[i] := 0;
    End; {zeroVariables}

    Procedure SortEnergyLevels (Var Statelist: statetype);
    { sort lattice sites in k space according to energy }
        Var
            kx2, ky2, k2, k2max, kx, ky, kz, i: integer;

        Procedure QuickSort (Var table: statetype;
                                        first, last: integer);
            Var
                middle: integer;

            Procedure Split (Var table: statetype;
                                            first, last: integer;
                                            Var middle: integer);
                Var
                    up, down: integer;
                    pivot: real;

                Procedure Exchange (Var x, y: stateptr);
                    Var
                        temp: stateptr;
                Begin
                    temp := x;
                    x := y;
                    y := temp;
                End;

            Begin {Split}
                pivot := table[first]^.E;
                up := first;
                down := last;
                Repeat
                    While (up < last) And (table[up]^.E <= pivot) Do
                        up := up + 1;
                    While (table[down]^.E > pivot) Do
                        down := down - 1;
                    If up < down Then
                        exchange(table[up], table[down]);
                Until up >= down;
                exchange(table[first], table[down]);
                middle := down;
            End; {Split}


        Begin {QuickSort}
            If first < last Then
                Begin
                    split(table, first, last, middle);
                    quicksort(table, first, middle - 1);
                    quicksort(table, middle + 1, last);
                End
        End; {QuickSort}


    Begin {SortEnergyLevels}
        Message('Creating k-space and sorting energy levels...');
        For i := 1 To maxstates Do
            statelist[i] := Nil;
        k2max := sqr(kxm);
        If sqr(kym) > k2max Then
            k2max := sqr(kym);
        If sqr(kzm) > k2max Then
            k2max := sqr(kzm);
        nstate := 0;
        For kx := -kxm To kxm Do
            Begin
                kx2 := sqr(kx);
                For ky := -kym To kym Do
                    Begin
                        ky2 := sqr(ky);
                        For kz := -kzm To kzm Do
                            Begin
                                k2 := kx2 + ky2 + sqr(kz);
                                If k2 <= k2max Then
                                    Begin
                                        nstate := nstate + 1;
                                        new(statelist[nstate]);
                                        Statelist[nstate]^.kx := kx;
                                        Statelist[nstate]^.ky := ky;
                                        Statelist[nstate]^.kz := kz;
                                        Statelist[nstate]^.occupied := 0;
                                        Statelist[nstate]^.E := pwr(k2, n / 2)
                                    End; {endif}
                            End;
                    End;
            End;
        Quicksort(Statelist, 1, nstate);
        Message('Done sorting');

    End; {SortEnergyLevels}

    Procedure Initialize (Var Statelist: statetype;
                                    Var Distribution: dtype;
                                    Var StateOf: stype;
                                    Var results: resultrec);
        Var
            i: integer;

        Procedure GetParameters;
            Var
                ParamScreen: TInputScreen;
                i: integer;
                particlename: String;
        Begin
{            new(paramscreen);}
            With paramscreen Do
                Begin
                    Init;
                    DefineInputPort(0.06, 0.94, 0.06, 0.94);
                    loadline('');
                    loadline(' Choose type of statistics:');
                    loadline(' #1Bose-Einstein   #1Fermi-Dirac   #1Maxwell-Boltzmann');
                    loadline('');
                    loadline(' Energy  =  c k^p,   where p = {   } ');
                    loadline(' usually p = 1 for massless and p = 2 for massive particles  ');
                    loadline('');
                    loadline(' Maximum k in x direction = {   }');
                    loadline(' Maximum k in y direction = { } (enter 0 for 1-D)');
                    loadline(' Maximum k in z direction = { }  (enter 0 for 1-D and 2-D)');
                    loadline('');
                    loadline(' Number of particles = {   }');
                    loadline(' Initial temperature = {        }');
                    loadline(' Random number seed = {         }');
                    LoadLine('');
                    LoadLine('                     [  Ok  ]   [Cancel]   ');
                    Case particle Of
                        BE: 
                            setRadioButton('1', 1);
                        FD: 
                            setRadioButton('1', 2);
                        MB: 
                            setRadioButton('1', 3)
                    End;{case}
                    setnumber(4, n);
                    setnumber(5, kxm);
                    setnumber(6, kym);
                    setnumber(7, kzm);
                    setnumber(8, np);
                    setnumber(9, T);
                    setnumber(10, RandomSeed0);
                    setnumberlimits(4, 1, 9);
                    setnumberlimits(5, 1, 100);
                    setnumberlimits(6, 0, 20);
                    setnumberlimits(7, 0, 20);
                    setnumberlimits(8, 1, MaxNp);
                    setnumberlimits(9, 1E-10, 1000);
                    AcceptScreen;
                    Case getRadiobutton('1') Of
                        1: 
                            Particle := BE;
                        2: 
                            Particle := FD;
                        3: 
                            Particle := MB;
                    End;{case}
                    n := getnumber(4);
                    kxm := trunc(getnumber(5));
                    kym := trunc(getnumber(6));
                    kzm := trunc(getnumber(7));
                    d := 3;
                    If kzm = 0 Then
                        Begin
                            d := 2;
                            If kym = 0 Then
                                d := 1;
                        End;
                    Np := trunc(getnumber(8));
                    T := getnumber(9);
                    RandomSeed0 := trunc(getnumber(10));
                    RandomSeed := -abs(RandomSeed0);
                    k2max := sqr(kxm);
                    If sqr(kym) > k2max Then
                        k2max := sqr(kym);
                    If sqr(kzm) > k2max Then
                        k2max := sqr(kzm);
                    Emax := 10;
                    done;
                End;{with}
        End;{GetParameters}

    Begin{initialize}
        Repeat
            GetParameters
        Until (N > 0) And (T > 0.0);
        SortEnergyLevels(Statelist);
        zerovariables(distribution, results);
     {put particles in initial states}
        beta := 1.0 / T;
        If Particle = FD Then
            Begin
                Energy := 0;
                For i := 1 To Np Do
                    Begin
                        StateOf[i] := i;
                        Energy := Energy + Statelist[i]^.E;
                        StateList[i]^.occupied := 1;
                    End;
                StateOf[0] := 0;
            End
        Else {Particle = BE or MB}
            Begin
                For i := 1 To Np Do
                    StateOf[i] := 1;
                Energy := 0;
                StateOf[0] := 1;
                StateList[1]^.occupied := Np;
            End;
        StateOf[Np + 1] := StateOf[Np] + 10;
        If StateOf[Np + 1] > nstate Then
            StateOf[Np + 1] := nstate;
    End; {Initialize}

 {-----------Procedures used during simulation-----------------}

        Procedure plotstate(i:integer);
        {Plot's state with color according to occupancy}
            Var
                xp, yp: real;
        Begin
            xp := stateList[i]^.kx;
            yp := stateList[i]^.ky;
            Case statelist[i]^.occupied Of  {change states}
                1:
                    SetFillStyle(solidfill,lightmagenta);
                2: 
                    SetFillStyle(solidfill,green);
                3:
                    SetFillStyle(solidfill,yellow);
                4..maxint:
                    SetFillStyle(solidfill,whitte);
            End;
              if statelist[i]^.occupied > 0 then
                   bar(mapx(xp),mapy(yp),mapx(xp)+4,mapy(yp)+4)
              else
                   begin
                    setfillstyle(solidfill,graphbackcolor);
                    bar(mapx(xp),mapy(yp),mapx(xp)+4,mapy(yp)+4);
                    setcolor(blakk);
                    circle(mapx(xp)+2,mapy(yp)+2,1);
                   end;
   End;{plotstate}

   Procedure MonteCarloWalk (Var Statelist: statetype;
                                    Var StateOf: stype;
                                    Var results: resultrec);
        Const
            maxjump = 40;
        Var
            move, i, dsmax, stry, s: integer;
            dE, xp, yp: real;


        Procedure acceptmove;
        Begin
            statelist[stateOf[i]]^.occupied := statelist[stateOf[i]]^.occupied - 1;  {reduce occupancy of initial state}
            If stateList[stateof[i]]^.kz = 0 Then
                plotstate(stateof[i]);
            StateOf[i] := stry;
            statelist[stry]^.occupied := statelist[stry]^.occupied + 1; {increase occupancy of final state}
            If stateList[stry]^.kz = 0 Then
                plotstate(stry);
            Energy := Energy + dE;
            Results.accept := Results.accept + 1.0;
            {Test for new maximum energy}
            IF statelist[stry]^.E > Emax then Emax := statelist[stry]^.E;
            If i = Np Then
                StateOf[Np + 1] := imin(StateOf[Np] + maxjump, nstate);
        End;{acceptmove}

    Begin {MonteCarloWalk}
        selectviewport(7);
        selectscale(7);
        For move := 1 To Np Do
            Begin
                delay(round(delaytime));
                i := iran(RandomSeed, 1, Np);
                s := StateOf[i];
                If particle = FD Then
                    stry := iran(RandomSeed, StateOf[i - 1] + 1, StateOf[i + 1] - 1)  { Fermion }
                Else If particle = BE Then
                    stry := iran(RandomSeed, StateOf[i - 1], StateOf[i + 1]) {Boson}
                Else
                    stry := iran(Randomseed, imax(stateOF[i] - 10, 1), imin(StateOf[i] + 10, nstate));
                If (stry <> s) Then
                    Begin
                        dE := Statelist[stry]^.E - statelist[s]^.E;
                        If dE <= 0.0 Then
                            acceptmove
                        Else If exp(-beta * dE) > urand(RandomSeed) Then  {accept change of state according to Metropolis rule}
                            acceptmove;
                    End;{if}
            End;   {do-for loop}
            setcolor(graphbackcolor);
            print(2,3,concat('MCS = ',numstr(results.ndat0,6,0)));
            results.ndat0:= results.ndat + 1;
            setcolor(whitte);
            print(2,3,concat('MCS = ',numstr(results.ndat+1,6,0)));
    End;   {MonteCarloWalk}

    Procedure CollectData (Var Statelist: statetype;
                                    Var Distribution: dtype;
                                    Var StateOf: stype;
                                    Var results: resultrec);
        Const
            MCSmax = 1000;
        Var
            i: integer;
    Begin {CollectData}
        For i := 1 To Np Do
            Distribution[StateOf[i]] := Distribution[StateOf[i]] + 1;
        With results Do
            Begin
                ecum := ecum + Energy;
                ecum2 := ecum2 + Energy * Energy;
                ndat := ndat + 1;
                If ((ndat Mod MCSmax) = 1)or (Energy > np*Escale) Then
                    Begin
                        SetColor(blakk);
                        DefineViewPort(3, 0.65, 0.95, 0.15, 0.38);
                        closeviewport(3);
                        openviewport(2);
                        Escale := 1;
                        While Escale < Energy / Np Do
                            Escale := 10 * Escale;
                        Definescale(2, 0, MCSmax, 0, Escale);
                        SelectScale(2);
                        axis(0, 0, MCSmax / 4, Escale / 2);
                        setcolor(yellow);
                        print(3, 1, 'E/N');
                        setcolor(green);
                        print(10, 1, '<E>/N');
                        setcolor(whitte);
                        putlabel(bottom,'MCS');
                    End;
                selectviewport(2);
                selectscale(2);
                setcolor(yellow);
                setfillstyle(solidfill,yellow);
                fillellipse(mapx(ndat Mod MCSmax), mapy(Energy / Np), 1,1);
                setcolor(green);
                setfillstyle(solidfill,green);
                fillellipse(mapx(ndat Mod MCSmax), mapy(Ecum / (Np * ndat)),1,1)
            End;{with}
    End; {CollectData}


    Procedure Showdata (Var Statelist: statetype;
                                    Var Distribution: dtype;
                                    var results: resultrec);
     {Plots distribution function and prints energy and specific heat}
       Var
            xmax, ymax,xp,yp: real;
            m, mmax, j, ndata, nplot,xsc,ysc: integer;
            x, y: dvector;
    Begin {Showdata}
        SetColor(blakk);
        DefineViewPort(3, 0.60, 0.99, 0.38, 0.78);
        closeviewport(3);
        openviewport(5);
        SetColor(whitte);
        x.init(nState-1);
        y.init(nState-1);
        ymax := 1;
        For m := 2 To nstate Do
            Begin
                y.put(m - 1, Distribution[m] / results.ndat);
                If y.value(m - 1) > ymax Then
                    ymax := y.value(m - 1);
                x.Put(m - 1, Statelist[m]^.E);
            End;
        Definescale(5, 0, Emax, 0, 1.2 * ymax);
        SelectScale(5);
        PutLabel(bottom, chr(238));
        SetColor(whitte);
        print(2, 1, 'Distribution Function');
        axis(0, 0, TickSpace(Emax), TickSpace(ymax) );
        SetColor(yellow);
        SetFillStyle(solidfill,yellow);
        For m := 2 To nstate Do
            Begin
                xp := x.value(m-1);
                yp := y.value(m-1);
                map(xp,yp,xsc,ysc);
                if yp > 0 then fillellipse(xsc,ysc,2,2);
            End;
        OpenViewPort(9);
        SetColor(yellow);
        With results Do
            Begin
                PRINT(1, 1, concat('T = ', NumStr(T, 5, 2)));
                PRINT(17,1, concat('MCS = ', NumStr(ndat, 4, 0)));
                setcolor(whitte);
                PRINT(1,3, concat('f(0) = ', NumStr(Distribution[1] / ndat, 6, 2)));
                PRINT(1, 2, concat('<E>/N = ', NumStr((ecum / Np) / ndat, 6, 3)));
                PRINT(17, 2, concat('C = ', NumStr(beta * beta * ((ecum2 / ndat) - sqr(ecum / ndat)) / Np, 6, 3)));
                PRINT(17, 3, concat('Acc = ', NumStr(accept / (ndat * Np), 5, 3)));
            End;
            x.free;
            y.free;
    End; {Showdata}

    {-----------------Display  Procedures----------------------}

    Procedure particledata;
    Begin
        OpenViewPort(8);
        SetColor(whitte);
        If Particle = BE Then
            Print(1, 1, 'Bose-Einstein Statistics')
        Else If Particle = FD Then
            Print(1, 1, 'Fermi-Dirac Statistics')
        Else
            Print(1, 1, 'Maxwell-Boltzmann Statistics');
        Print(1, 2, concat('Dimension of space = ', NumStr(d, 2, 1)));
        Print(1, 3, concat('Single particle energy = k^', NumStr(n, 2, 1)));
        Print(1, 4, concat('Number of particles = ', NumStr(Np, 5, 0)));
    End;

    Procedure DrawMyscreen (Var Statelist: statetype;
                                    Var hotkeys: thotkeys);
        Var
            xp, yp: real;
             m, i: integer;
    Begin
        clearmuppetport;
        hotkeys.Display;
        changetemp.draw;
        SetColor(whitte);
        OpenViewPort(7);
        SelectScale(7);
        PutLabel(left, 'ky');
        PutLabel(bottom, 'kx');
        SetColor(blakk);
         For i := 1 To Nstate Do
          plotstate(i);
        line(mapx(-12), mapy(15), mapx(12), mapy(15));
        setcolor(whitte);
        print(2, 1, 'Code for State Occupancy');
        SetColor(lightmagenta);
        setfillstyle(solidfill,lightmagenta);
        bar(mapx(-6.5),mapy(16.5),mapx(-6.5)+4,mapy(16.5)+4);
        PRINT(8, 2, '1');
        setfillstyle(solidfill,green);
        bar(mapx(-2.5),mapy(16.5),mapx(-2.5)+4,mapy(16.5)+4);
        SetColor(green);
        Print(13, 2, '2');
        SetColor(yellow);
        setfillstyle(solidfill,yellow);
        bar(mapx(2.0),mapy(16.5),mapx(2.0)+4,mapy(16.5)+4);
        Print(18, 2, '3');
        SetColor(whitte);
        setfillstyle(solidfill,whitte);
        bar(mapx(6.5),mapy(16.5),mapx(6.5)+4,mapy(16.5)+4);
        Print(23, 2, '>=4');
        particledata;
        Message('');
        menu.display;
    End; {DrawMyScreen}

    Procedure DrawHotkeys;
    begin
  {        new(hotkeys);}
        With hotkeys Do
            Begin
                init(6);
                key[1] := 'F1-HELP';
                key[2] := 'F2-RESET';
                key[3] := 'F3-UPDATE';
                key[4] := 'F4-FASTER';
                key[5] := 'F5-SLOWER';
                if running then
                   key[6] := 'F6-PAUSE'
                else
                   key[6] := 'F6-RUN';
                display;
            End;
     end;
 
    Procedure SetUpDisplay;

    Begin {SetUpDisplay}
        DrawHotkeys;
       DefineViewPort(7, 0.10, 0.45, 0.15, 0.70);
        Definescale(7, -12, 12, -12, 20);
        DefineViewPort(8, 0.10, 0.50, 0.75, 0.95);
        DefineViewPort(2, 0.70, 0.95, 0.15, 0.35);
        DefineViewPort(9, 0.60, 0.95, 0.8, 0.95);
        DefineViewPort(5, 0.65, 0.95, 0.45, 0.75);
        SetColor(whitte);
    End;{SetUpDisplay}

    Procedure drawdisplay (Var Statelist: statetype;
                                    Var hotkeys: thotkeys);
        Var
            tmax: real;
    Begin
        If T < 10 Then
            tmax := 10
        Else
            tmax := 2 * T;
        setcolor(whitte);
{        new(changetemp);}
        if started then changetemp.done;
        changetemp.create(0.009, Tmax, T, 0.55, 0.6, 0.2, 2, ' 0.00', NumStr(Tmax, 4, 2), 'T/To', true);
        DrawMyScreen(Statelist, hotkeys);
    End;

    Procedure closeup;
    Begin
        closeViewPort(5);
        closeViewPort(7);
        closeViewPort(8);
        closeviewport(6);
{        dispose(hotkeys); }
        disposestates(Statelist);
        clearmuppetport;
        changetemp.done;
        menu.done;
    End;


    Procedure InitializeMenu (Var Menu: TMenu);
    Begin {InitializeMenu}
{        new(menu);}
        menu.init;
        With Menu Do
            Begin
                column(1, 'File');
                row(1, 1, 'About CUPS');
                row(1, 2, 'About Program');
                row(1, 3, 'Configuration');
                row(1, 4, 'Exit Program');
                column(2, 'System');
                row(2, 1, 'Specify System');
               display;
            End;
    End; {InitializeMenu}

    Procedure handlemenu;
    Begin
        Case menu.Colchosen Of
            1:
                Begin
                    running := false;
                    drawhotkeys;
                    Case menu.RowChosen Of
                        1: 
                            AboutCups;
                        2: 
                            AboutProgram;
                        3:
                            Configuration;
                        4:
                            quit := true;
                    End; {case}
               End;
            2:
              Case menu.RowChosen Of
                1:
                  Begin
                    running := false;
                    drawhotkeys;
                    initialize(statelist, distribution, stateof, results);
                    drawdisplay(statelist, hotkeys);
                   End;
              end;{case}
        End;
    End;

    Procedure HandleHotKeys(kNum : byte);
    begin
      Case knum Of
          1:  begin
                 running := false;
                 drawhotkeys;
                 AboutProgram2;
                 Helpscreen;
              end;
          2:
              Begin
                  zerovariables(distribution, results);
                  setcolor(blakk);
              End;
          3:  if results.ndat > 0 then
              showdata(Statelist, distribution, results)
              else
              message('No data to show, choose F6-RUN');
          4: delaytime:= (delaytime/1.5);
          5: delaytime:=(delaytime*1.5);
          6: begin
              running := not running;
              drawhotkeys;
              menu.display;
             end;
         { 6: If Menu.chosen then HandleMenu;}
      End; {case}
    end;

{S  calculate}

{---------------------------------------------------------------------------}
{-------------- M A I N   P R O G R A M ------------------------------------}
{---------------------------------------------------------------------------}
var
  TNew : real;
  kNum : byte;
Begin    {Main program}
    Cupsinit;
    InitializeMenu(Menu);
    AboutProgram;
    initialparameters;
    SetUpDisplay;
    quit := false;
    running := false;
    initialize(statelist, distribution, stateof, results);
    drawdisplay(statelist, hotkeys);
     started := true;
        Repeat
            checkforevents;
            if running then
            begin
              MonteCarloWalk(Statelist, stateof, results);
              Collectdata(Statelist, distribution, stateof, results);
            end;
               Tnew := changetemp.value;
               If T <> Tnew Then  {Temperature has changed}
               Begin
                   IF Tnew < T then Emax := 10;
                   T := Tnew;
                   beta := 1 / Tnew;
                   zerovariables(distribution, results);
                   setcolor(blakk);
               End;
            If hotkeys.pressed(knum) Then HandleHotKeys(kNum);
            If Menu.Activated Then handlemenu;
        Until quit;
    closeup;
    CupsDone;
End.    {program}