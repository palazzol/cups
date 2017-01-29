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

Program qmgas1;
{   CUPS - Thermal Physics }
{   October 10 1994        }
{   Jan Tobochnik          }

Uses
    graph, CUPS, CUPSmupp, CUPSgui, CUPSgrph, CUPSproc,cupsfunc;
Type
    ParticleType = (BE, FD, MB);
    particlerec = Record
            p, d: real;
            s, ndata: integer;
            statistic: ParticleType;
            debye, massive,newdata: boolean;
            Tc: real;
        End;
    dataptr = ^datarec;
    datarec = Record
            T, mu, E, c: real;
            next: dataptr;
        End;
        scaletype = Record
                smax, smin: Array[1..8] Of real;
                name: Array[1..8] Of String;
            End;

Var
    Menu: tmenu;
    particle: particlerec;
    datalist: dataptr;
    hotkeys:thotkeys;
    knum:byte;
    quit,plotted,cancel:boolean;
    scalerec:scaletype;
    iseed:integer;




{----------Information Screens -----------------------}

Procedure AboutProgram;
    Var
        i: integer;
        C: HelpScrType;

    Begin {AboutProgram}
        For i := 1 To 25 Do
            C[i] := '';
        C[2] := '     THERMODYNAMIC PROPERTIES OF QUANTUM';
        C[3] := '               IDEAL PARTICLES';
        C[5] := '                      by  ';
        C[7] := '                Jan Tobochnik  ';
        C[8] := '              Kalamazoo College  ';
        C[12] := '       (c) 1995 John Wiley & Sons, Inc.';
        C[14] := '  This program calculates the energy, specific';
        C[15] := '  heat, and the chemical potential as a function';
        C[16] := '  of temperature for particles with no';
        C[17] := '  interparticle potential, but which obey ';
        C[18] := '  Bose-Einstein(BE), Fermi-Dirac(FD) or';
        C[19] := '  Maxwell-Boltzmann(MB) statistics. The  ';
        C[20] := '  density of states and various distribution';
        C[21] := '  functions can also be displayed. Initially,';
        C[22] := '  you will be asked to specify the system you';
        C[23] := '  wish to work with.';
        C[25] := '     Press any Key or Click Mouse to continue';
        HELP(C);
    End; {AboutProgram}

Procedure AboutProgram2;
    Var
        i: integer;
        C: HelpScrType;

    Begin {AboutProgram2}
        For i := 1 To 25 Do
            C[i] := '';
        C[2] := 'This program uses the grand canonical ensemble';
        C[3] := 'to calculate thermodynamic properties. Thus,';
        C[4] := 'for massive particles one must first calculate';
        C[5] := 'the relevant integral, which the program does';
        C[6] := 'numerically using Simpsons rule or various';
        C[7] := 'series approximations when appropriate. Then the';
        C[8] := 'energy and specific heat are calculated from';
        C[9] := 'other integrals, and the results are listed';
        C[10] := 'in order of increasing temperature. One can';
        C[11] := 'then plot these thermodynamic data(T Plots),';
        C[12] := 'or plot (E Plots) the following functions for';
        C[13] := 'three temperatures at once.';
        C[15] := 'D(E) = number of states per energy interval';
        C[16] := 'f(E) = number of particles per state';
        C[17] := 'N(E) = f(E)D(E)';
        C[18] := 'I(E) = Ef(E)D(E)';
        C[25] := '   Press any Key or Click Mouse to quit help';
        HELP(C);
    End;{AboutProgram}

Procedure HelpListdata;
    Var
        i: integer;
        C: HelpScrType;

    Begin
        For i := 1 To 25 Do
            C[i] := '';
       C[2] := 'Data listed in order of increasing Temperature';
       C[4] := 'y = chemical potential/(kB T)';
       c[5] := 'z = exp(y)';
       c[7] := 'E/N = energy per particle, C = specific heat';
       Help(C);
   end;

Procedure initialparameters;
    Begin

        With particle Do
            Begin
                statistic := BE;
                p := 2;
                d := 3;
                Debye := false;
                Massive := true;
            End;
    End;

{-----Calculates integrals needed to compute T,  energy and specific heat ---}

Function simpson (mu, pow, xmax: real;
                                s, choice: integer): real;
    Var
        iteration, interior: integer;
        oldintegral, integral, integral2, oldintegral2, z, z_inverse, tolerance: real;


    Procedure midpoint (choice:integer;
                                    Var integral: real;
                                    Var interior: integer;
                                    iteration: integer;
                                    a, b: real);
        Var
            dx, dx2, x, sum: real;
            i: integer;

        Function fz (x: real): real;
        Begin
            fz := pwr(x, pow) / (z_inverse * exp(x) + s);
        End;

    Function fzp (x: real): real;
        {dfz/dz}
        Begin
            fzp := exp(x) * pwr(x, pow) / sqr(exp(x) + z * s);
        End;

        Begin
            If iteration = 1 Then
                Begin
                    integral := (b - a) * pwr((b - a) * 0.5, pow) / (z_inverse * exp((b - a) * 0.5) + s);
                    interior := 1;
                End
            Else
                Begin
                    dx := (b - a) / (3.0 * interior);
                    dx2 := dx + dx;
                    x := a + 0.5 * dx;
                    sum := 0;
                    if choice = 1 then
                    For i := 1 To interior Do
                        Begin
                            sum := sum + fz(x);
                            x := x + dx2;
                            sum := sum + fz(x);
                            x := x + dx;
                        End
                        else
                        For i := 1 To interior Do
                        Begin
                            sum := sum + fzp(x);
                            x := x + dx2;
                            sum := sum + fzp(x);
                            x := x + dx;
                        End;
                 integral := (integral + (b - a) * sum / interior) / 3.0;
                    interior := 3 * interior;
                End;
        End;

    Begin{simpson}
        z := exp(mu);
        z_inverse := 1 / z;  {inverse of fugacity}
        tolerance := 1.0e-4;
        oldintegral := -1.0e10;
        oldintegral2 := -1.0e10;
        iteration := 1;
        midpoint(choice, integral2, interior, iteration, 0, xmax);
        integral := (9.0 * integral2 - oldintegral2) / 8;
        repeat
            Begin
                iteration := iteration + 1;
                oldintegral := integral;
                oldintegral2 := integral2;
                midpoint(choice, integral2, interior, iteration, 0, xmax);
                integral := (9.0 * integral2 - oldintegral2) / 8;
            End;
         until  (abs(integral - oldintegral) < tolerance * abs(oldintegral));
        simpson := integral;
    End;

Function sommerfeld (y, pow: real;
                                choice: integer): real;
{Computes Fermi integrals using Sommerfeld expansion}
    Var
        c, yp, sum: real;
        i, j: integer;

    Function an (n: integer): real;
        Var
            n2: real;
        Begin
            If n > 5 Then
                Begin
                    n2 := -2 * n;
                    an := 2 * (1 - pwr(2, n2) + pwr(3, n2) - pwr(4, n2) + pwr(5, n2) - pwr(6, n2));
                End
            Else
                Case n Of
                    1: 
                        an := 1.644934067;
                    2: 
                        an := 1.894065616;
                    3: 
                        an := 1.971101653;
                    4: 
                        an := 1.992458674;
                    5: 
                        an := 1.998074741;
                End;{case}
        End; {an}

    Begin
        If choice = 1 Then
            Begin
                yp := pwr(y, pow + 1);
                sum := yp / (pow + 1);   {first term}
                yp := yp / sqr(y);
                c := pow;
                sum := sum + yp * c * an(1);   {add second term}
                i := 1;
                j := 1;
                While yp > 0.00001 Do
                    Begin
                        c := c * (pow - i) * (pow - i - 1);
                        i := i + 2;
                        j := j + 1;
                        yp := yp / sqr(y);
                        sum := sum + an(j) * yp * c;
                    End;
                sommerfeld := sum
            End
        Else
            Begin
                yp := pwr(y, pow);
                sum := yp;
                yp := yp / sqr(y);
                c := pow * (pow - 1);
                sum := sum + yp * c * an(1);   {add second term}
                i := 2;
                j := 1;
                While yp > 0.00001 Do
                    Begin
                        c := c * (pow - i) * (pow - i - 1);
                        i := i + 2;
                        j := j + 1;
                        yp := yp / sqr(y);
                        sum := sum + an(j) * yp * c;
                    End;
                sommerfeld := sum * exp(-y)
            End
    End;
{S seg1}

Function gamma (n: real): real;
    Var
        g, x, tmp, sum: real;
        i: integer;
    Begin
        If (abs(n - trunc(n)) < 0.00001) Then
            Begin {integer n}
                g := 1;
                For i := 1 To trunc(n - 1) Do
                    g := g * i;
            End
        Else If (abs(n - 0.5 - trunc(n)) < 0.00001) Then
            Begin  {n is an integer plus a half}
                g := sqrt(pi);
                i := 1;
                While n > i Do
                    Begin
                        g := g * (i - 0.5);
                        i := i + 1;
                    End
            End
        Else
            Begin  {From Numerical Recipes}
                x := n - 1.0;
                tmp := x + 5.5;
                tmp := (x + 0.5) * ln(tmp) - tmp;
                sum := 1 + 76.18009173 / (x + 1);
                sum := sum - 86.50532033 / (x + 2);
                sum := sum + 24.01409822 / (x + 3);
                sum := sum - 1.231739516 / (x + 4);
                sum := sum + 1.20858003e-3 / (x + 5);
                sum := sum - 5.36382e-6 / (x + 6);
                sum := tmp + ln(sum * 2.50662827465);
                g := exp(sum);
            End;
        gamma := g;
    End;{gamma}

Function series (mu, pow: real;
                                s, choice: integer): real;
{ computes series approximation to Boson and Fermion integrals when z < 1}
    Var
        sum, z, zp: real;
        sign, n, nterms: integer;
    Begin
        z := exp(mu);
        nterms := 1 - trunc(15.0 / mu);  {rough estimate of number of terms needed for 10^-6 accuracy}
        If nterms > 200 Then
            nterms := 200;
        sign := -s;
        If choice = 1 Then  {do integral}
            Begin
                sum := z;
                zp := z;
                For n := 2 To nterms Do
                    Begin
                        zp := z * zp;
                        sum := sum + sign * zp * pwr(n, -(pow + 1));
                        sign := sign * (-s);
                    End;
            End
        Else   {do derivative of integral}
            Begin
                sum := 1;
                zp := 1;
                For n := 2 To nterms Do
                    Begin
                        zp := z * zp;
                        sum := sum + sign * zp * pwr(n, -pow);
                        sign := sign * (-s);
                    End;
            End;
        series := gamma(pow + 1) * sum;
    End;{series}

Function exactcase (mu, xmax: real;
                                s, choice: integer): real;
     {Exact integral when pow = 0}
    Var
        z: real;
    Begin
        z := exp(mu);
        If choice = 1 Then
            If s = 0 Then
                exactcase := z
            Else If (z * s = -1) Then
                exactcase := 1.0e30
            Else If (s = 1) And (mu > 7) Then
                exactcase := mu
            Else
                exactcase := (ln((1 + z * s) / (1 + z * s * exp(-xmax)))) / s
        Else If choice = 2 Then
            If s = 0 Then
                exactcase := 1
            Else If (z * s = -1) Then
                exactcase := 1.0e30
            Else
                exactcase := 1 / (1 + z * s);
    End;


Function I_of_z (y, pow, xmax: real;
                                Var particle: particlerec): real;
         {I(z) is the integral of f(E,z)E^pow dE, where f = distribution function and z = fugacity = exp(mu)}
         {y = chemical potential over kT}
         {p = -1 Bose-Einstein, p = 1 Fermi-Dirac, p = 0 Maxwell-Boltzmann}
    Begin
        With particle Do
            If (abs(pow) < 0.001) Then
                I_of_z := exactcase(y, xmax, s, 1)
            Else If (abs(y) < 0.01) And (s = -1) And (Not Debye) Then
       {Some previously computed cases that converge slowly}
                If (abs(pow - 0.5) < 0.005) Then
                    I_of_z := 2.315
                Else If (abs(pow - 1.5) < 0.005) Then
                    I_of_z := 1.7813
                Else If (abs(pow - 4.0) < 0.005) Then     {Integrals for photons have pow = 1,2,3,4}
                    I_of_z := 24.886
                Else If (abs(pow - 3.0) < 0.005) Then
                    I_of_z := 6.4939
                Else If (abs(pow - 2.0) < 0.005) Then
                    I_of_z := 2.4041
                Else If (abs(pow - 1.0) < 0.005) Then
                    I_of_z := 1.6449
                Else If (pow <= 0) Then
                    I_of_z := 1.0e30
                Else
                    I_of_z := simpson(y, pow, xmax, s, 1)
            Else If (s = -1) And (y < 0) Then
                I_of_z := series(y, pow, s, 1)
            Else If (s = 1) And (y > 5) Then
                I_of_z := sommerfeld(y, pow, 1)
            Else If (s = 1) And (y < 0) Then
                I_of_z := series(y, pow, s, 1)
            Else If (s = 0) And massive Then
                I_of_z := exp(y) * gamma(pow + 1)
            Else
                I_of_z := simpson(y, pow, xmax, s, 1);
    End;{I_of_z}

Function I_of_zp (y, pow, xmax: real;
                                Var particle: particlerec): real;
          { Compute derivative of I(z) w.r.t  z}
          {p = -1 Bose-Einstein, p = 1 Fermi-Dirac, p = 0 Maxwell-Boltzmann}
    Begin
        With particle Do
            If (abs(pow) < 0.001) Then
                I_of_zp := exactcase(y, xmax, s, 2)
            Else If (abs(y) < 0.01) And (s = -1) And (Not Debye) Then
                If (abs(pow - 1.5) < 0.005) Then
                    I_of_zp := 3.4722
                Else If (pow <= 1) Then
                    I_of_zp := 1.0e30
                Else
                    I_of_zp := simpson(y, pow, xmax, s, 2)
            Else If (s = 1) And (y > 5) Then
                I_of_zp := sommerfeld(y, pow, 2)
            Else If (s = 1) And (y < 0) Then
                I_of_zp := series(y, pow, s, 2)
            Else If (s = -1) And (y < 0) Then
                I_of_zp := series(y, pow, s, 2)
            Else If (s = 0) And (massive) Then
                I_of_zp := gamma(pow + 1)
            Else
                I_of_zp := simpson(y, pow, xmax, s, 2);
    End;{I_of_z}

Procedure computedata (a: String;
                                x: real;
                                Var T1, mu1, E1, C1: real;
                                Var particle: particlerec);
{Calculates temperature from y or z and then computes}
{chemical potential, energy per particle, and specific heat}
    Var
        iof1, limit, qd, x2, T2, E2: real;
    Begin
        limit := 500;
        With particle Do
            Begin
                If (Not massive) Then
                    Begin
                        T1 := x;
                        mu1 := 0;
                        x := 0;
                    End
                Else If ((statistic = BE) And (Tc > 0) And (a[1] In ['t', 'T'])) Then
                    Begin
                        T1 := x * Tc;
                        mu1 := 0;
                        x := 0;
                    End
                Else If statistic = BE Then
                    Begin
                        x := ln(x);
                        T1 := pwr(p / (d * I_of_z(x, (d / p) - 1, limit, particle)), p / d);
                        mu1 := x * T1;
                    End
                Else
                    Begin
                        T1 := pwr(p / (d * I_of_z(x, (d / p) - 1, limit, particle)), p / d);
                        mu1 := x * T1;
                    End;
                If (Tc > 0) Or ((Not debye) And (Not massive)) Then
                    Iof1 := (d / p) * I_of_z(0, d / p, limit, particle);
                c1 := 0;
                If debye Then
                    Begin
                        limit := 1 / T1;
                        e1 := (d / p) * pwr(T1, 1.0 + d / p) * I_of_z(x, d / p, limit, particle);
                        c1 := -(d / p) * pwr(1, 1.0 + d / p) / (T1 * (exp(limit) + s));
                    End
                Else If (Not massive) Or (T1 < Tc) Then
                    e1 := pwr(T1, 1.0 + d / p) * Iof1
                Else
                    e1 := (d / p) * pwr(T1, 1.0 + d / p) * I_of_z(x, d / p, limit, particle);
                If (s = 1) And (x > 5.0) Then
                    Begin
                        qd := d / p;
                        c1 := c1 + (pi * pi * d / (3 * p * x)); {need another term here}
                        c1 := c1 + (2.705808 *qd *(-4*qd*qd +6*qd -2)
                              +1.894065616*(12*qd*qd -36*qd +24))*qd/(x*x*x);

                    End
                Else If (s = 1) And (x > 0) Then
                    Begin
                        x2 := x / 1.1;
                        T2 := pwr(p / (d * I_of_z(x2, (d / p) - 1, limit, particle)), p / d);
                        e2 := (d / p) * pwr(T2, 1.0 + d / p) * I_of_z(x2, d / p, limit, particle);
                        c1 := (e2 - e1) / (T2 - T1);
                    End
                Else
                    Begin
                        c1 := c1 + ((d / p) + 1) * e1 / T1;
                        If massive And (T1 >= Tc) Then
                         c1 := c1 - (d / p) *
                         I_of_zp(x, d / p, limit, particle)
                         / I_of_zp(x, (d / p) - 1.0, limit, particle);
                    End;
            End;{with}
    End;

Procedure listdata;
    Var
        i: integer;
        item: dataptr;
    Begin
        DefineViewPort(1, 0.05, 0.95, 0.1, 0.84);
        openviewport(1);
        setcolor(white );
        With particle Do
            Begin
                Case statistic Of
                    BE: 
                        Print(1, 1, 'Bose-Einstein Statistics');
                    FD: 
                        Print(1, 1, 'Fermi-Dirac Statistics');
                    MB: 
                        Print(1, 1, 'Maxwell-Boltzmann Statistics');
                End;{case}
                If Not massive Then
                    Begin
                        Print(1, 2, 'Chemical potential = 0');
                        If Debye Then
                            Print(35, 2, 'Debye cutoff in effect')
                        Else
                            Print(35, 2, 'Debye cutoff not in effect');
                    End;
                Print(30, 1, concat('d = ', NumStr(d, 2, 1)));
                Print(44, 1, chr(238));
                Print(45, 1, concat(' = k^', NumStr(p, 2, 1)));
                print(2, 4, 'data #');
                if statistic = BE then
                  print(15,4,'z')
                else
                  print(15,4,'y');
                print(26, 4, 'T');
                print(34, 4, 'Chem. Pot.');
                print(50, 4, 'E / N');
                print(66, 4, 'c');
                item := datalist;
                For i := 1 To ndata Do
                    With item^ Do
                        Begin
                            print(2, 4 + i, NumStr(i * 1.0, 4, 0));
                            if statistic = BE then
                               if(exp(mu/T) > 1000) or (exp(mu/T) < 0.001) then
                                 print(11,4+i,scnumstr(exp(mu/T),2))
                               else
                                 print(8,4+i,numstr(exp(mu/T),10,4))
                            else
                               if(abs(mu/T) > 1000) or (abs(mu/T) < 0.001) then
                                 print(11,4+i,scnumstr(mu/T,2))
                               else
                                 print(8,4+i,numstr(mu/T,10,4));
                            if (T > 1000) or (T < 0.001) then
                              print(23,4+i,Scnumstr(T,2))
                            else
                              print(21, 4 + i, NumStr(T, 8, 4));
                           if (abs(mu) > 1000) or (abs(mu) < 0.001) then
                              print(34,4+i,Scnumstr(mu,2))
                            else
                              print(27, 4 + i, NumStr(mu, 15, 5));
                            if (E > 1000) or (E < 0.001) then
                              print(51,4+i,Scnumstr(E,2))
                            else
                              print(42, 4 + i, NumStr(E, 15, 5));
                            if (c > 1000) or (c < 0.001) then
                              print(64,4+i,Scnumstr(c,2))
                            else
                                print(55, 4 + i, NumStr(c, 15, 4));
                            item := item^.next;
                        End;
            End;{with}
    End;{listdata}

Procedure insertdata(var enough:boolean;var saveparameter:integer);
    Var
        ascreen: TInputscreen;
        i: integer;
        item, newitem, previtem: dataptr;
        alldone: boolean;
        Torz, T1, c1, E1, mu1, val: real;
    Begin
{        new(ascreen);   }
        With ascreen Do
            Begin
                init;
                DefineInputPort(0.05, 0.95, 0.85, 0.95);
             With particle Do
                    Begin
                        If massive Then
                            If statistic = FD Then
                                Begin
                                    loadline(' Enter y = chem. pot. / T  {+100.00} ');
                                    setnumber(1, 50.0*saveparameter);
                                    saveparameter := saveparameter + 1;
                                End
                            Else If statistic = MB Then
                                Begin
                                    loadline(' Enter y = chem. pot. / T  {-10.000} ');
                                    setnumber(1, -1.0*saveparameter);
                                    saveparameter := saveparameter + 1;
                                End
                            Else If Tc = 0 Then
                                Begin
                                    loadline('Enter z = exp(chem. pot. / T){   +0.5}  (0 < z < 1)');
                                    setnumber(1, 1.0*urand(iseed));
                                    setnumberlimits(1,0.0001,0.99999);
                                End
                            Else
                                Begin
                                    loadline('Enter  #1t = T/Tc (0<t<1) or #1fugacity z (0<z<1) {   +0.5}');
                                    setRadioButton('1',saveparameter);
                                    setnumber(3, 1.0*urand(iseed));
                                    setnumberlimits(3,0.0001,0.99999);
                                 End
                        Else
                            Begin
                                loadline(' Enter Temperature   {  1.0000}');
                                setnumber(1, saveparameter*1.0);
                                setnumberlimits(1,0.001,1000);
                                saveparameter := saveparameter + 1;
                            End;
                        LoadLine('[OK]         [ Done ]       [Cancel]');
                        defineviewport(9,0,1,0.9,1);
                        closeviewport(9);
                        listdata;
                        Accept;
                        ClearInputPort;
                        message('computing data ...');
                        If Not (canceled) Then
                            Begin
                                If (statistic = BE) And (Tc > 0) Then
                                    Begin
                                        val := getnumber(3);
                                        saveparameter := getRadioButton('1');
                                        if getRadioButton('1') = 1 then
                                          computedata('t', Val, T1, mu1, E1, C1, particle)
                                        else
                                          computedata('z', Val, T1, mu1, E1, C1, particle);
                                        enough := not getboolean(4);
                                    End
                                Else
                                    Begin
                                        val := getnumber(1);
                                        computedata(' ', Val, T1, mu1, E1, C1, particle);
                                        enough := not getboolean(2);
                                    End;
                                    done;
                               ndata := ndata + 1;
                                newdata := true;
                               new(newitem);
                                newitem^.T := T1;
                                newitem^.E := E1;
                                newitem^.mu := mu1;
                                newitem^.C := C1;
                                If datalist = Nil Then
                                    Begin
                                        datalist := newitem;
                                        datalist^.next := Nil
                                    End
                                Else If datalist^.T > T1 Then
                                    Begin
                                        newitem^.next := datalist;
                                        datalist := newitem
                                    End
                                Else
                                    Begin
                                        previtem := datalist;
                                        item := datalist^.next;
                                        alldone := false;
                                        Repeat
                                            If item = Nil Then
                                                Begin
                                                    previtem^.next := newitem;
                                                    newitem^.next := Nil;
                                                    alldone := true;
                                                End
                                            Else If T1 < item^.T Then
                                                Begin
                                                    previtem^.next := newitem;
                                                    newitem^.next := item;
                                                    alldone := true;
                                                End;
                                            If Not alldone Then
                                                Begin
                                                    previtem := item;
                                                    item := item^.next;
                                                End;
                                        Until alldone;
                                    End;{else}
                          End {if}
                                      else
                                      enough := true;
                  End; {with}
               { done;}
            End; {with ascreen}
    End;{insertdata}

Procedure deletedata;
    Var
        ascreen: TInputscreen;
        i, n: integer;
        item, tempitem: dataptr;
    Begin
{        new(ascreen);  }
        With ascreen Do
            Begin
                init;
                DefineInputPort(0.45, 0.9, 0.85, 0.95);
                loadline('Enter point No. to delete {   1}' );
               LoadLine(' [  Ok  ]   [Cancel]   ');
                setnumber(1, 1);
                listdata;
                AcceptScreen;
                n := trunc(getnumber(1));
                If (n > 0) and (n <= particle.ndata) And (Not (canceled)) Then
                    Begin
                        particle.ndata := particle.ndata - 1;
                         particle.newdata := true;

                     If n = 1 Then
                            Begin
                                item := datalist;
                                datalist := datalist^.next;
                                dispose(item);
                            End
                        Else
                            Begin
                                item := datalist;
                                For i := 1 To n - 2 Do
                                    item := item^.next;
                                tempitem := item^.next;
                                item^.next := tempitem^.next;
                                dispose(tempitem);
                            End;
                    End;
                done;
            End;{with}
    End;{deletedata}


 Procedure DefineSystem(var cancel:boolean);
    Var
        ParamScreen: TInputScreen;
        i: integer;
        particlename: String;
    Begin
{        new(paramscreen);}
        With paramscreen Do
            Begin
                Init;
                DefineInputPort(0.06, 0.94, 0.16, 0.94);
                loadline('');
                loadline('If you wish to use a previously created file');
                loadline('press cancel and choose file/open from menu');
                loadline('');
                loadline(' Choose type of statistics:');
                loadline('   #1Bose-Einstein');
                loadline('   #1Fermi-Dirac  ');
                loadline('   #1Maxwell-Boltzmann');
                loadline('');
                loadline(' Energy = B k^p, where p = {    } ');
                loadline('  usually p=1 for massless and p=2 for massive particles  ');
                loadline(' Dimension of space      = {    }');
                loadline(' Set chemical potential = 0 (Only for BE Statistics) #T ');
                loadline(' Debye cutoff?   (Only if chemical potential = 0)    #F');
                loadline('');
                LoadLine('');
                LoadLine('                     [  Ok  ]   [Cancel]   ');
                With particle Do
                    Begin
                        Case statistic Of
                            BE: 
                                setRadioButton('1', 1);
                            FD:
                                setRadioButton('1', 2);
                            MB: 
                                setRadioButton('1', 3)
                        End;{case}
                        setnumber(4, p);
                        setnumber(5, d);
                        setboolean(6, Not (massive));
                        setboolean(7, debye);
                        setnumberlimits(4, 0.5, 5);
                        setnumberlimits(5, 0.5, 5);
                        Accept;
                        ClearInputPort;
                        cancel := canceled ;
            if not canceled then
              begin
                        Case getRadiobutton('1') Of
                            1: 
                                Begin
                                    statistic := BE;
                                    s := -1;
                                End;
                            2: 
                                Begin
                                    statistic := FD;
                                    s := +1;
                                End;
                            3: 
                                Begin
                                    statistic := MB;
                                    s := 0;
                                End;
                        End;{case}
                        p := getnumber(4);
                        d := getnumber(5);
                        massive := true;
                        if (statistic = BE) then
                            massive := Not getboolean(6);
                        debye := false;
                        If Not massive Then
                            debye := getboolean(7);
                        If (statistic = BE) And (d / p - 1 > 0) And (massive) Then
                            Tc := pwr(p / (d * I_of_z(0, d / p - 1, 20, particle)), p / d)
                        Else
                            Tc := 0;
                        ndata := 0;
                        newdata := false;
                end
                else
                  cancel := canceled;
                    End; {withparticle}
                done;
            End;{with}
    End;
 Procedure printdata;
 {Print data to a file}
    Var
        i,npos: integer;
        item: dataptr;
        f: text;
        ascreen:TInputScreen;
        fname:string;
        cancel:boolean;
    Begin
         With ascreen Do
            Begin
                init;
                DefineInputPort(0.45, 0.9, 0.6, 0.95);
                loadline('Enter filename "        "');
                loadline('');
                 LoadLine(' [  Ok  ]   [Cancel]   ');
                 AcceptScreen;
                 if canceled then fname := ''
                  else fname := getstring(1);
                cancel := canceled;
                done;
            end; {with ascreen}
       if not cancel then
       With particle Do
            Begin
                npos := Pos('.',fname) + 1;
                if npos > 1 then
                 begin
                  Delete(fname,npos,3);
                  Insert('qm1',fname,npos);
                 end
                else
                 fname := concat(fname,'.qm1');
                 if fname <> '' then
                  begin
                   assign(f,fname);
                   rewrite(f);
                   writeln(f,'File created by program qmgas1.exe.');
                   Case statistic Of
                    BE:
                        writeln(f,'Bose-Einstein Statistics');
                    FD: 
                        writeln(f,'Fermi-Dirac Statistics');
                    MB: 
                        writeln(f,'Maxwell-Boltzmann Statistics');
                End;{case}
                If Not massive Then
                    Begin
                        write(f, 'n');
                        If Debye Then
                             writeln(f, 'D')
                        Else
                            writeln(f,'N');
                    End
                else
                  writeln(f,'m','m');
                writeln(f,d,p);
                writeln(f,ndata);
                write(f, 'T','   ');
                write(f,'  Chem. Pot.','   ');
                write(f,'  E / N','   ');
                write(f,'  c');
                writeln(f);
                item := datalist;
                For i := 1 To ndata Do
                    With item^ Do
                        Begin
                            write(f,i * 1.0,' ');
                            if statistic = BE then
                               write(f,exp(mu/T),' ')
                            else
                               write(f,mu/T,' ');
                            writeln(f,T,' ',mu,' ',E,' ',c);
                          item := item^.next;
                        End;
             close(f);
             end {if};
            End;{with}
   End;{printdata}

    Procedure readdata;
    {Read in data from a file}
    Var
        i: integer;
        line:string;
        item,previtem: dataptr;
        f: text;
        ascreen:TInputScreen;
        fname:string;
        cancel:boolean;
        c:char;
        xdum,ydum:real;
    Begin
       fname := openFile('*.qm1');
       if fname<>'' then
       With particle Do
            Begin
                assign(f,fname);
                reset(f);
                readln(f,line);
                if (line = 'File created by program qmgas1.exe.')then
                begin
                readln(f,c);
                Case c Of
                    'B':begin
                        statistic := BE;
                        s := -1;
                        end;
                    'F':begin
                        statistic := FD;
                        s := 1;
                        end;
                    'M':begin
                        statistic := MB;
                        s := 0;
                        end;
                End;{case}
                read(f,c);
                IF c = 'm' then
                   massive := true
                else
                   massive := false;
                Debye:=false;
                readln(f,c);
                If (Not massive) and (c = 'D') Then
                  Debye:= true;
                readln(f,d,p);
                readln(f,ndata);
                newdata:= true;
                readln(f);
               For i := 1 To ndata Do
                  begin
                    new(item);
                    if i = 1 then datalist := item
                    else previtem^.next := item;
                    With item^ Do
                        Begin
                            read(f,xdum);
                            read(f,xdum);
                           readln(f,T,mu,E,c);
                           next := nil;
                           previtem := item;
                        End;
                  end; {for}
             close(f);
             If (statistic = BE) And (d / p - 1 > 0) And (massive) Then
                 Tc := pwr(p / (d * I_of_z(0, d / p - 1, 20, particle)), p / d)
             Else
                Tc := 0;
                end
           else
             Announce(' The file you selected was not created by this program.');
            End;{with}
   End;{readdata}

{-----------------Calculate and plot results ---------------------}
{S seg2}
Procedure Dcalc (Var x, y: dVector;
                                Var numdata: integer;
                                Emax: real;
                                Var particle: particlerec);
     {Density of states}
    Var
        Ecut, dE, E, dq, dq1: real;

    Begin {Dcalc}
        With particle Do
            Begin
                dq := d / p;
                dq1 := dq - 1;
                If Debye Then
                    Ecut := 1
                Else
                    Ecut := Emax;
                dE := Ecut / 200;
                E := 0;
                numdata := 0;
                While E < Ecut Do
                    Begin
                        numdata := numdata + 1;
                        x.put(numdata, E);
                        y.put(numdata, dq * pwr(E, dq1));
                        E := E + dE;
                    End;
            End;{with}
    End;{Dcalc}

Procedure Fcalc (T, mu: real;
                                Var x, y, z, w: dVector;
                                Var numdata: integer;
                                Emax: real;
                                Var ymax, zmax, wmax: real;
                                Var particle: particlerec);
    {y = Number of particles per energy level}
    {z = Number of particles per energy interval}
    {w = Energy of particles per energy interval}
    Var
        Ecut, E, dE, dq, dq1: real;
        i: integer;

    Begin {Fcalc}
        With particle Do
            Begin
                dq := d / p;
                dq1 := dq - 1;
                If Debye Then
                    Ecut := 1
                Else
                    Ecut := Emax;
                dE := Ecut / 100;
                numdata := 100;
                ymax := 0;
                zmax := 0;
                wmax := 0;
                For i := 1 To numdata Do
                    Begin
                        E := i * dE;
                        x.put(i, E);
                        y.put(i, 1 / (exp((E / T) - mu) + s));
                        z.put(i, dq * pwr(E, dq1) * y.value(i));
                        w.put(i, z.value(i) * E);
                        If y.value(i) > ymax Then
                            ymax := y.value(i);
                        If z.value(i) > zmax Then
                            zmax := z.value(i);
                        If w.value(i) > wmax Then
                            wmax := w.value(i);
                    End;
            End;{with}
    End;{Fcalc}


Procedure Eplots;
    Var
        x1, x2, x3, y1, y2, y3, z1, z2, z3, w1, w2, w3: dVector;
        numdata, numdata1, numdata2, numdata3, i, j: integer;
        Emax, ymax, zmax, wmax: real;
        ymax1, zmax1, wmax1: real;
        ymax2, zmax2, wmax2: real;
        ymax3, zmax3, wmax3: real;
        ascreen: TInputscreen;
        Tp, yp: Array[1..3] Of real;
        n: Array[1..3] Of integer;
        item: dataptr;
        cancel : boolean;

    Function axismax (xinp: real): real;
        Var
            xmax: real;
        Begin
            xmax := 0.05;
            Repeat
                xmax := 2 * xmax;
            Until xinp <= xmax;
            axismax := xmax;
        End;
    Function max3 (x, y, z: real): real;
        Var
            max0: real;
        Begin
            max0 := x;
            If y > max0 Then
                max0 := y;
            If z > max0 Then
                max0 := z;
            max3 := max0;
        End;

Begin {Eplots}
{        new(ascreen);   }
    With particle Do
    Begin
        With ascreen Do
        Begin
            init;
            DefineInputPort(0.05, 0.95, 0.85, 0.95);
            loadline(' Enter three points to plot : Data Nos.  {  1} {  2} {  3}');
            LoadLine('     [  Ok  ]   [Cancel]  ');
            setnumber(1, 1);
            setnumber(2, 2);
            setnumber(3, 3);
            listdata;
            Acceptscreen;
            cancel := canceled;
            If Not (cancel) Then
            Begin
                For i := 1 To 3 Do
                Begin
                    n[i] := trunc(getnumber(i));
                    If n[i] > particle.ndata Then
                        n[i] := particle.ndata;
                    item := datalist;
                    For j := 1 To n[i] - 1 Do
                        item := item^.next;
                    Tp[i] := item^.T;
                    yp[i] := item^.mu / Tp[i];
                End;
             end;
             done;
          end; {with}
          if not cancel then
          begin
             If Debye Then
                 Emax := axismax(pwr(d, p / d))
             Else If Not massive Then
                 Emax := axismax(6.0 * max3(Tp[1], Tp[2], Tp[3]))
             Else If statistic = FD Then
                 Emax := 2
             Else
                 Emax := 4;
                      defineviewport(8,0,1,0.05,0.95);
             closeviewport(8);
 
             x1.init(300);
             y1.init(300);
             Dcalc(x1, y1, numdata, Emax, particle);
             ymax := axismax(1 + trunc((d / p) * pwr(Emax, (d / p) - 1)));
             DefineViewPort(5, 0.10, 0.45, 0.60, 0.95);
             SetColor(white );
             OpenViewPort(5);
             Definescale(5, 0, Emax, 0, ymax);
             SelectScale(5);
             axis(0, 0, TickSpace(Emax) , TickSpace(ymax));
             SetColor(white );
             Print(1, 1, ' Density of States');
             PutLabel(bottom, 'Energy');
             PlotdVectors(x1, y1, 1, numdata);
             x1.free;
             y1.free;

             x1.init(100); y1.init(100); z1.init(100); w1.init(100);
             x2.init(100); y2.init(100); z2.init(100); w2.init(100);
             x3.init(100); y3.init(100); z3.init(100); w3.init(100);

             Fcalc(Tp[1], yp[1], x1, y1, z1, w1, numdata1,
                   Emax, ymax1, zmax1, wmax1, particle);
             Fcalc(Tp[2], yp[2], x2, y2, z2, w2, numdata2,
                   Emax, ymax2, zmax2, wmax2, particle);
             Fcalc(Tp[3], yp[3], x3, y3, z3, w3, numdata3,
                   Emax, ymax3, zmax3, wmax3, particle);
             ymax := axismax(max3(ymax1, ymax2, ymax3));
             zmax := axismax(max3(zmax1, zmax2, zmax3));
             wmax := axismax(max3(wmax1, wmax2, wmax3));

             DefineViewPort(2, 0.60, 0.95, 0.60, 0.95);
             SetColor(white );
             OpenViewPort(2);
             Definescale(2, 0, Emax, 0, ymax);
             SelectScale(2);
             axis(0, 0, TickSpace(Emax), TickSpace(ymax));
             PutLabel(bottom, 'Energy');
             SetColor(white );
             print(1, 1, ' # of particles per state');
             SetColor(lightmagenta);
             if n[1] > 0 then begin
              print(18, 3, concat('T= ', Numstr(Tp[1], 5, 5)));
              PlotdVectors(x1, y1, 1, numdata1);
             end;
             SetColor(yellow);
             if (n[2]  > 0) and (n[2] <> n[1]) then begin
              print(20, 4, concat(' ', Numstr(Tp[2], 5, 5)));
              PlotdVectors(x2, y2, 1, numdata2);
             end;
             SetColor(green);
             if (n[3]  > 0) and (n[3] <> n[1]) and (n[3] <> n[2]) then begin
              print(20, 5, concat(' ', Numstr(Tp[3], 5, 5)));
              PlotdVectors(x3, y3, 1, numdata3);
             end;

             DefineViewPort(3, 0.10, 0.45, 0.15, 0.50);
             SetColor(white );
             OpenViewPort(3);
             Definescale(3, 0, Emax, 0, zmax);
             SelectScale(3);
             axis(0, 0, TickSpace(Emax), TickSpace(zmax) );
             PutLabel(bottom, 'Energy');
             SetColor(white );
             print(1, 1, ' # of particles per d');
             print(22,1,chr(238));
             SetColor(lightmagenta);
             if n[1] > 0 then
              PlotdVectors(x1, z1, 1, numdata1);
             SetColor(yellow);
             if (n[2]  > 0) and (n[2] <> n[1]) then
              PlotdVectors(x2, z2, 1, numdata2);
             SetColor(green);
             if (n[3]  > 0) and (n[3] <> n[1]) and (n[3] <> n[2]) then
              PlotdVectors(x3, z3, 1, numdata3);

             DefineViewPort(6, 0.60, 0.95, 0.15, 0.50);
             SetColor(white );
             OpenViewPort(6);
             Definescale(6, 0, Emax, 0, wmax);
             SelectScale(6);
             axis(0, 0, TickSpace(Emax), TickSpace(wmax));
             PutLabel(bottom, 'Energy');
             SetColor(white );
             print(1, 1, ' Amount of energy per d');
             print(24,1,chr(238));
             SetColor(lightmagenta);
             if n[1] > 0 then
              PlotdVectors(x1, w1, 1, numdata1);
             SetColor(yellow);
             if (n[2]  > 0) and (n[2] <> n[1]) then
              PlotDVectors(x2, w2, 1, numdata2);
             SetColor(green);
             if (n[3]  > 0) and (n[3] <> n[1]) and (n[3] <> n[2]) then
              PlotdVectors(x3, w3, 1, numdata3);
             x1.free; y1.free;  z1.free; w1.free;
             x2.free; y2.free;  z2.free; w2.free;
             x3.free; y3.free;  z3.free; w3.free;
             hotkeys.display;
             waitonmouseclick;
           End; {if}
    End; {with}
End; {Eplots}

Procedure Tplots (var plotted:boolean);
    Var
        x: Array[1..4] Of dVector;
        xt,smax0,smin0: Array[1..4] Of real;
        i, j, numdata, numdatat: integer;
        xdum, ydum: real;
       rangescreen: TInputscreen;
        Tlow, Thigh, zz: real;
        item: dataptr;


    Procedure BottomLabel;
        Begin
            With Particle Do
                If Tc > 0 Then
                    PutLabel(bottom, 'T/Tc')
                Else
                    PutLabel(bottom, 'T');
        End;


{S seg3}
  Procedure fixscale;
  var
    i:integer;
    x:real;
   begin
     with scalerec do begin
       For i := 1 To 4 Do
          Begin
           x:= 0.5;
           repeat
            if smax[i] > x then x:=2*x
           until smax[i] < x;
           smax[i] := x;
          end;{for}
           x:= -0.5;
           repeat
            if smin[2] < x then x:=2*x
           until smin[2] > x;
           smin[2] := x;
           smin[1] := 0;
           smin[3] := 0;
           smin[4] := 0;
      end; {with}
   end; {fixscale}

    Procedure setscales;
        Var
            i, i0, nlines: integer;
            rangescreen: TInputscreen;
            alldone: boolean;
        Begin
{            new(rangescreen); }
            With rangescreen Do
             Begin
               init;
               DefineInputPort(0.05, 0.95, 0.05, 0.95);
               loadline('');
               With scalerec Do
                Begin
                 For i := 1 To 4 Do
                  Begin
                   Case i Of
                      1:
                        loadline('Temperature');
                      2:
                        loadline('Chemical Potential');
                      3:
                        loadline('Energy per Particle');
                      4:
                        loadline('Specific Heat');
                    End; {case}
                    loadline(' Min={0.00000}    Max={0.00000} ');
                    loadline('');
                   setnumber(2 * i - 1, smin[i]);
                   setnumber(2 * i , smax[i]);
                   End; {for loop}
                   LoadLine('            [  Ok  ]   [Cancel]   ');
                   acceptscreen;
                   If Not (canceled) Then
                     For i := 1 To 4 Do
                         Begin
                          smin[i] := getnumber(2 * i - 1);
                          smax[i] := getnumber(2 * i );
                         End; {for}
                 End; {with scalerec}
                 done;
                End; {with}
        End;{setscales}

    Begin {Tplots}

       With scalerec Do
            Begin
                name[1] := 'Temperature';
                name[2] := 'Chemical Potential';
                name[3] := 'Energy per Particle';
                name[4] := 'Specific Heat';
                For i := 1 To 4 Do
                    Begin
                        smax0[i] := -1.0e30;
                        smin0[i] := 1.0e30;
{                        new(x[i]); }
                        x[i].init(particle.ndata);
                    End;
                item := datalist;
                For j := 1 To particle.ndata Do
                    Begin
                        xt[1] := item^.T;
                        If particle.Tc > 0 Then
                            xt[1] := xt[1] / particle.Tc;
                        xt[2] := item^.mu;
                        xt[3] := item^.E;
                        xt[4] := item^.c;
                        item := item^.next;
                        For i := 1 To 4 Do
                            Begin
                                x[i].put(j, xt[i]);
                                If xt[i] > smax0[i] Then
                                    smax0[i] := xt[i];
                                If xt[i] < smin0[i] Then
                                    smin0[i] := xt[i];
                            End;
                    End;

            End;{with}
     if particle.newdata then begin
      for i := 1 to 4 do begin
         scalerec.smin[i] := smin0[i];
         scalerec.smax[i] := smax0[i];
         end;
         fixscale;
      end;
       if plotted then
        setscales;
        DefineViewPort(2, 0.10, 0.45, 0.60, 0.95);
        DefineViewPort(3, 0.10, 0.45, 0.15, 0.50);
        DefineViewPort(6, 0.60, 0.95, 0.15, 0.50);
        DefineViewPort(4, 0.60, 0.95, 0.60, 0.95);
            defineviewport(8,0,1,0.05,0.95);
            closeviewport(8);
            particle.newdata:= false;
           For i := 1 To 3 Do
                With scalerec Do
                    Begin
                        SetColor(white );
                        OpenViewPort(i + 1);
                        Definescale(i + 1, smin[1], smax[1], smin[i + 1], smax[i + 1]);
                        SelectScale(i + 1);
                        axis(smin[1],smin[i+1],tickspace(smax[1]-smin[1]),tickspace(smax[i+1]-smin[i+1]));
                        BottomLabel;
                        SetColor(white );
                        Print(2, 1, name[i + 1]);
                        SetColor(yellow);
                        setFillStyle(solidfill,yellow);
                       for j := 1 to particle.ndata do
                         fillellipse(mapx(x[1].value(j)),mapy(x[i+1].value(j)),2,2);
                    End;
                      hotkeys.key[3] := 'F3-rescale';
                        plotted:=true;
                        hotkeys.display;

       For i := 1 To 4 Do
            x[i].free;
    End; {Tplots}

{---------------------Display Procedures---------------------}

  Procedure SetUpHotkeys;
  begin
      {        new(hotkeys);}
        With hotkeys Do
            Begin
                init(7);
                key[1] := 'F1-Help';
                key[2] := 'F2-E Plots';
                key[3] := 'F3-T Plots';
                key[4] := 'F4-See Table';
                key[5] := 'F5-Add Data';
                key[6] := 'F6-Delete';
                key[7] := 'F10-MENU';
                display;
            End;
end;{setuphotkeys}

Procedure resethotkeys;
  begin
           hotkeys.key[3] := 'F3-T Plots';
        plotted:=false;
         hotkeys.display;
  end;

Procedure disposeall;
var
        item: dataptr;
begin
  if particle.ndata > 0 then
     repeat
       item := datalist;
       datalist := datalist^.next;
       dispose(item);
     until datalist = nil;
  datalist := nil;
  particle.ndata := 0;
end;

Procedure SetUpMenu;
    Begin {MainMenu}
   {     new(Menu); }
        Menu.init;
        With Menu Do
            Begin
                column(1, 'File');
                row(1, 1, 'About Cups');
                row(1, 2, 'About Program');
                row(1, 3, 'Configuration');
                row(1, 4, '-------------');
                row(1, 5, 'Open');
                row(1, 6, 'Save');
                row(1, 7, '-------------');
                row(1, 8, 'Exit Program');
                column(2, 'System');
                row(2, 1, 'Specify System');
                rowactivate(1,4 ,false);
                rowactivate(1,7 ,false);
                display;
            End;
    End; {MainMenu}

    Procedure HandleMenu;
    var
    enough,cancel :boolean;
    saveparameter :integer;
    begin
    with Menu do
            Case Colchosen Of
                1: 
                    Case rowchosen Of
                        1: 
                            AboutCups;
                        2: 
                            AboutProgram;
                        3:
                            Configuration;
                       5:
                            begin
                              resethotkeys;
                              disposeall;
                              readdata;
                              listdata;
                             end;
                        6:
                            Printdata;
                        8:
                            quit := true;
                    End;{case}
                2:
                   case rowchosen of
                   1:
                    Begin
                      resethotkeys;
                      definesystem(cancel);
                      if not cancel then begin
                        disposeall;
                        enough := false;
                        defineviewport(8,0,1,0.05,0.95);
                        closeviewport(8);
                        hotkeys.display;
                        saveparameter := 1;
                     repeat
                         insertdata(enough,saveparameter);
                        until enough = true;
             defineviewport(9,0,1,0.9,1);
     closeviewport(9);
                         listdata;
                     end;{if}
                    End;
                 end; {case}
           End;  {case}
  end; {handlemenu}

  Procedure handleHotkeys;
  var
     enough:boolean;
     saveparameter:integer;
  begin
     defineviewport(8,0,1,0.0,1);
     closeviewport(8);
     if knum <> 3 then resethotkeys;
     case knum of
     1: Aboutprogram2;
    2: if particle.ndata > 0 then eplots else
     message('You need to add data before plotting');
     3: if particle.ndata > 1 then tplots(plotted) else
      begin
       message('You need to add more data before plotting');
       hotkeys.display;
      end;
     4: listdata;
     5: begin
         enough := false;
         saveparameter := 1;
         repeat
            insertdata(enough,saveparameter);
         until enough = true;
         defineviewport(9,0,1,0.9,1);
         closeviewport(9);
         listdata;
        end;{help}
 
     6: begin
             deletedata;
             listdata;
        end;
     7: if menu.chosen then handlemenu;
     end; {case}
 end; {handlehotkeys}

Begin {main program}
    Cupsinit;
    SetUpMenu;
    initialparameters;
    AboutProgram;
    SetUpHotkeys;
    quit:=false;
    plotted:=false;
    definesystem(cancel);
    knum := 5;
    iseed := -12345;
    if not cancel then HandleHotkeys;
    repeat
      checkforevents;
      If hotkeys.pressed(knum) Then HandleHotKeys;
      If Menu.Activated Then handlemenu;
    Until quit;
    CUPSDone;
End.