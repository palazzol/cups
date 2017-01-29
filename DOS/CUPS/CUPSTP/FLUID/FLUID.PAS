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

Program fluidprogram;
{September 7, 1994}
    Uses
        graph, CUPS, CUPSmupp, CUPSgui, CUPSgrph, CUPSproc, CUPSfunc;
    Type
        fluidtype = (water, VdW);
        coexistencerectype = Record
                T, P, vl, vg, ul, ug, sl, sg: real;
            End; {coexrectype}
        coextype = Array[1..76, water..VdW] Of coexistencerectype;
        parameterrec = Record
                Vmn, Vmx, Pmn, Pmx, Tmn, Tmx, Smn, Smx, Umn, Umx: real;
                Vtic, Ptic, Ttic, Stic, Utic: real;
                Pc, Tc, vc: real; {Critical constants}
                Ncc: integer; {number of points on coexistence curve}
            End;
        parametertype = Array[water..VdW] Of parameterrec;
        pdtype = Array[1..4] Of integer;
        loctype = Array[1..4, 1..4] Of real;
        constrec = Record
                c: Array[1..8] Of real;
                taj, raj: Array[1..7] Of real;
                aij: Array[1..10, 1..7] Of real;
                E, Tk, R: real;
            End;
        dataarray = Array[1..40] Of real;
        datatype = Record
                T, P, s, u, v, fe, cv, cpcvi, kappaTi, CTEi: real;
            End;

    const
      whitte = white;
      blakk = black;
    Var
        CT: coextype;
        loc: loctype;
        pa: parametertype;
        consts: constrec;
        Menu: Tmenu;
        hotkeys: thotkeys;
        quit: boolean;
        pd: pdtype;
        fluid: fluidtype;
        i: integer;



PROCEDURE RLine(a,b : pointType); far;
BEGIN
   line(a.x,a.y,b.x,b.y)
END;


{------------------- Information Screens ----------------------------------}

    Procedure AboutProgram;
        Var
            i: integer;
            C: HelpScrType;

    Begin {AboutProgram}
        For i := 1 To 25 Do
            C[i] := '';
{C[1] := '12345678901234567890123456789012345678901234567890';}
        C[3] := '           PHASE DIAGRAMS OF FLUIDS';
        C[5] := '                      by  ';
        C[7] := '                 Jan Tobochnik  ';
        C[8] := '               Kalamazoo College  ';
        C[12] := '       (c) 1995 John Wiley & Sons, Inc.';
        C[15] := '    This program simulates thermodynamic';
        C[16] := '    paths for water and the van der Waals';
        C[17] := '    fluid. You draw a path in one phase ';
        C[18] := '    diagram and the program calculates the';
        C[19] := '    path in three other diagrams. Data for the';
        C[20] := '    last point on the path is saved. Initially';
        C[21] := '    you will be asked to specify the fluid and';
        C[22] := '    the phase diagrams you wish to see.';
        C[24] := '    Press any key or click mouse to continue';
        HELP(C);
    End; {AboutProgram}

    Procedure HowtoUseProgram;
        Var
            i: integer;
            C: HelpScrType;

    Begin {HowtoUseProgram}
        For i := 1 To 25 Do
            C[i] := '';
        C[04] := '  This program lets you explore how the thermo-';
        C[05] := '  dynamic quantities P, T, v, u, and s vary in';
        C[06] := '  different kinds of thermodynamic processes.';
        C[07] := '  Simulations of liquid and vapor water as well';
        C[08] := '  as the van der Waals model are available.';
        C[10] := '  The first step is to choose four phase diagrams';
        C[11] := '  from the following possibilities:';
        C[12] := '  PT, Pv, vT, sv, sT, uv, and uT. ';
        C[13] := '  The PT diagram shows the coexistence curve and';
        C[14] := '  the other diagrams show the boundaries of the  ';
        C[15] := '  coexistence region. Then you can draw a straight';
        C[16] := '  line path in one diagram and see what this path';
        C[17] := '  corresponds to in the others. Quantities are ';
        C[18] := '  computed from the Helmholtz free energy, f(T,v).';
        C[19] := '  Because f(T,v) is very complicated for water,';
        C[20] := '  we must be given v and T, and thus you may only ';
        C[21] := '  draw in the vT plane for water, and see the';
        C[22] := '  paths in the other diagrams.';
        C[24] := '     Press any key or click mouse to continue';
        HELP(C);
    End; {HowtoUseProgram}

    Procedure AboutUnits;
        Var
            i: integer;
            C: HelpScrType;
    Begin {AboutUnits}
        For i := 1 To 25 Do
            C[i] := '';
        C[3] := ' Units for Water';
        C[5] := '  T = temperature in degrees celsius';
        C[6] := '  v = specific volume in cm cubed per gram';
        C[7] := '  P = pressure in megapascals';
        C[8] := '    (1 MPa = 9.869 atmos. = 10^7 dynes/cm sq.)';
        C[9] := '    (1 MPa = 10 bar = 7500 mm Hg)';
        C[10] := ' specific energy(u or f) in Joules per gram';
        C[11] := ' s = specific entropy in Joules/gm-degree Kelvin ';
        C[12] := ' c = specific heat in Joules/gm-degree Kelvin';
        C[15] := ' Units for van der Waals Fluid';
        C[17] := '   P, v, and T are all in reduced units of the';
        C[18] := '   critical pressure, volume, and absolute ';
        C[19] := '   temperature, respectively. In these units';
        C[20] := '   Pc, vc, and Tc are all equal to 1.';
        C[24] := '     Press any key or click mouse to continue';
        HELP(C);
    End; {AboutUnits}

    Procedure AboutFE;
        Var
            i: integer;
            C: HelpScrType;
    Begin {AboutFE}
        For i := 1 To 25 Do
            C[i] := '';
   {C[1] := '12345678901234567890123456789012345678901234567890';}
        C[3] := '  The van der Waals free energy in reduced units';
        C[4] := '  is given by:';
        C[5] := '  f = -3/v - (8T/3)[1 + ln(v - 1/3) + (3/2)ln(T)]';
        C[6] := '  This is constructed from the two equations of';
        C[7] := '  state P(v,T) and u(v,T):';
        C[8] := '  P = -df/dv = -3/v^2 + (8T/3)/(v - 1/3)';
        C[9] := '  u = -d(bf)/db = -3/v + 4T, where b = 1/T';
        C[10] := '  The coexistence curve is found by the equal area';
        C[11] := '  construction. Inside the coexistence region';
        C[12] := '  f(T,v) = x f(T,vg) + (1-x) f(T,vl), ';
        C[13] := '  where x = (v-vl)/(vg-vl), vg = gas specific';
        C[14] := '  volume, and vl = liquid specific volume.';
        C[16] := '  The free energy for water is an empirical';
        C[17] := '  equation from Steam Tables by J.H. Keenan, ';
        C[18] := '  F.G. Keyes, P.G. Hill, and J.G. Moore, ';
        C[19] := '  Wiley-Interscience, NY, 1978.';
        C[24] := '     Press any key or click mouse to continue';
        HELP(C);
    End; {AboutFE}

    Procedure HelpScreen;
        Var
            i: integer;
            C: HelpScrType;
    Begin {HelpScreen}
        For i := 1 To 25 Do
            C[i] := '';
   {C[1] := '12345678901234567890123456789012345678901234567890';}
        C[2] := '(1) Be sure to click in a window and hold the';
        C[3] := '    button down as you draw a line segment.';
        C[5] := '(2) For water you may only draw in the v-T diagram.';
        C[6] := '    For van der Waals you may draw in any plot';
        C[7] := '    EXCEPT for the s-v and u-v diagrams.';
        C[9] := '(3) Press the SEE DATA hotkey to see thermodynamic';
        C[10] := '    data for the last point on the last path.';
        C[12] := '(4) Press the CLEAR hotkey to remove all paths';
        C[14] := '(5) Press the NEW hotkey to change fluids or';
        C[15] := '    phase diagrams.';
        C[17] := '(6) Press the MENU hotkey to use the menu.';
        C[19] := 'NB: If after drawing a path in a window you do';
        C[20] := 'not see a path in the other windows, then either';
        C[21] := 'the path is off the screen or unphysical values';
        C[22] := 'were obtained from the van der Waals equations.';
        C[24] := '      Press any key or click mouse to continue';
        HELP(C);
    End; {Helpscreen}


    Procedure InitializeMenu (Var Menu: TMenu);
    Begin {InitializeMenu}
     {   new(menu);}
        menu.init;
        With Menu Do Begin
                column(1, 'File');
                row(1, 1, 'About CUPS');
                row(1, 2, 'About Program');
                row(1, 3, 'Configuration');
                row(1, 4, 'Exit Program');
                column(2, 'Diagrams');
                row(2, 1, 'Specify Phase Diagrams');
                column(3, 'Help');
                row(3, 1, 'How to use Program');
                row(3, 2, 'About Units');
                row(3, 3, 'About Free Energy');
            End;
    End; {InitializeMenu}
    Procedure setuphotkeys (Var hotkeys: thotkeys);
    Begin
{        new(hotkeys);}
        With hotkeys Do Begin
                init(5);
                key[1] := 'F1-Help';
                key[2] := 'F2-Clear';
                key[3] := 'F3-New';
                key[4] := 'F4-See Data';
                key[5] := 'F10-Menu';
            End;
    End;{setuphotkeys}



{--------------Read in necessary data -----}

    Procedure Createcoexistence (Var CT: coextype);
        Var
            f: text;
            i, j: integer;

        Procedure Constants;
            Var
                i, j: integer;
        Begin
            With consts Do Begin
                    c[1] := 1857.065;
                    c[2] := 3229.12;
                    c[3] := -419.465;
                    c[4] := 36.6649;
                    c[5] := -20.5516;
                    c[6] := 4.85233;
                    c[7] := 46.0;
                    c[8] := -1011.249;
                    taj[1] := 1.544912;
                    raj[1] := 0.634;
                    For j := 2 To 7 Do Begin
                            taj[j] := 2.5;
                            raj[j] := 1.0;
                        End;
                    E := 4.8;
                    R := 0.46151;
                    Tk := 273.15;
                    For i := 1 To 10 Do
                        For j := 1 To 7 Do
                            Aij[i, j] := 0;
                    Aij[1, 1] := 29.492937;
                    Aij[2, 1] := -132.13917;
                    Aij[3, 1] := 274.64632;
                    Aij[4, 1] := -360.93828;
                    Aij[5, 1] := 342.18431;
                    Aij[6, 1] := -244.50042;
                    Aij[7, 1] := 155.18535;
                    Aij[8, 1] := 5.9728487;
                    Aij[9, 1] := -410.30848;
                    Aij[10, 1] := -416.05860;
                    Aij[1, 2] := -5.1985860;
                    Aij[2, 2] := 7.7779182;
                    Aij[3, 2] := -33.301902;
                    Aij[4, 2] := -16.254622;
                    Aij[5, 2] := -177.31074;
                    Aij[6, 2] := 127.48742;
                    Aij[7, 2] := 137.46153;
                    Aij[8, 2] := 155.97836;
                    Aij[9, 2] := 337.31180;
                    Aij[10, 2] := -209.88866;
                    Aij[1, 3] := 6.8335354;
                    Aij[2, 3] := -26.149751;
                    Aij[3, 3] := 65.326396;
                    Aij[4, 3] := -26.181978;
                    Aij[9, 3] := -137.46618;
                    Aij[10, 3] := -733.96848;
                    Aij[1, 4] := -0.1564104;
                    Aij[2, 4] := -0.72546108;
                    Aij[3, 4] := -9.2734289;
                    Aij[4, 4] := 4.3125840;
                    Aij[9, 4] := 6.7874983;
                    Aij[10, 4] := 10.401717;
                    Aij[1, 5] := -6.3972405;
                    Aij[2, 5] := 26.409282;
                    Aij[3, 5] := -47.740374;
                    Aij[4, 5] := 56.323130;
                    Aij[9, 5] := 136.87317;
                    Aij[10, 5] := 645.81880;
                    Aij[1, 6] := -3.9661401;
                    Aij[2, 6] := 15.453061;
                    Aij[3, 6] := -29.142470;
                    Aij[4, 6] := 29.568796;
                    Aij[9, 6] := 79.847970;
                    Aij[10, 6] := 399.17570;
                    Aij[1, 7] := -0.69048554;
                    Aij[2, 7] := 2.7407416;
                    Aij[3, 7] := -5.1028070;
                    Aij[4, 7] := 3.9636085;
                    Aij[9, 7] := 13.041253;
                    Aij[10, 7] := 71.531353;
                End; {with}
        End;

    Begin{CreateCoex}
        With pa[VdW] Do Begin
                Vmn := 0.34; {VanDerWaal undefined for v < 1/3}
                Vmx := 4.0;
                Tmn := 0.25;
                Tmx := 1.5;
                Pmn := 0.0;
                Pmx := 1.5;
                Smn := 0;
                Smx := 10;
                Umn := -5;
                Umx := 10;
                Vtic := 1;
                Ttic := 0.5;
                Stic := 5;
                Utic := 5;
                Ptic := 0.5;
                Tc := 1;
                vc := 1;
                Pc := 1;
                Ncc := 76;
            End;
        With pa[water] Do Begin
                Vmn := 1;
                Vmx := 16.0;
                Tmn := 0.0;
                Tmx := 500;
                Pmn := 0.0;
                Pmx := 30;
                Umn := 0;
                Umx := 3000;
                Smn := 0;
                Smx := 10;
                Vtic := 4;
                Ttic := 250;
                Ptic := 10;
                Utic := 1000;
                Stic := 5;
                Tc := 374.136;
                vc := 3.155;
                Pc := 22.09;
                Ncc := 71;
            End;
        constants;
        assign(f, 'VdW.dat');
        {$I-}
        Reset(F);
        {$I+}
        if IOResult <> 0 then
          Error('File VdW.dat was not found');
        For i := 1 To pa[VdW].Ncc Do
            With CT[i, VdW] Do Begin
                    readln(f, T, P, vl, vg);
                    ul := (-3.0 / vl) + 4.0 * T;
                    ug := (-3.0 / vg) + 4.0 * T;
                    sl := 4.0 + 2.6666667 * (1 + ln(vl - 0.333333) + 1.5 * ln(T));
                    sg := 4.0 + 2.6666667 * (1 + ln(vg - 0.333333) + 1.5 * ln(T));
                End;
        close(f);
        Assign(f, 'water.dat');
        {$I-}
        Reset(F);
        {$I+}
        if IOResult <> 0 then
          Error('File water.dat was not found');
        For i := 1 To pa[water].Ncc Do
            With CT[i, water] Do Begin
                    readln(f, T, P, vl, vg, ul, ug, sl, sg);
                End;
        close(f);
    End; {Createcoexistence}

{----Procedures for locating points within coexistence region----}

    Procedure PVfromT (T: real;
                                    fluid: fluidtype;
                                    Var vl, vg, P, dPdT: real);
        Var
            i: integer;
            rr: real;
    Begin
        i := 0;
        Repeat
            i := i + 1;
        Until (T >= ct[i, fluid].T) And (T <= ct[i + 1, fluid].T);
        rr := (T - ct[i, fluid].T) / (ct[i + 1, fluid].T - ct[i, fluid].T);
        vg := ct[i, fluid].vg * (1 - rr) + rr * ct[i + 1, fluid].vg;
        vl := ct[i, fluid].vl * (1 - rr) + rr * ct[i + 1, fluid].vl;
        P := (1 - rr) * ct[i, fluid].P + rr * ct[i + 1, fluid].P;
        dpdT := (ct[i + 1, fluid].P - ct[i, fluid].P) / (ct[i + 1, fluid].T - ct[i, fluid].T);
    End;{pVfromT}


    Procedure VTfromP (p: real;
                                    fluid: fluidtype;
                                    Var vl, vg, T, dPdT: real);
        Var
            i: integer;
            rr: real;
    Begin
        i := 0;
        Repeat
            i := i + 1;
        Until (p >= ct[i, fluid].p) And (p <= ct[i + 1, fluid].p);
        rr := (p - ct[i, fluid].p) / (ct[i + 1, fluid].p - ct[i, fluid].p);
        vg := ct[i, fluid].vg * (1 - rr) + rr * ct[i + 1, fluid].vg;
        vl := ct[i, fluid].vl * (1 - rr) + rr * ct[i + 1, fluid].vl;
        T := (1 - rr) * ct[i, fluid].T + rr * ct[i + 1, fluid].T;
        dpdT := (ct[i + 1, fluid].P - ct[i, fluid].P) / (ct[i + 1, fluid].T - ct[i, fluid].T);
    End;{vtfromp}

{----Functions giving VdW quantity as a function of two others ----}

    Function VfromUT (u, T: real): real;
        Var
            i: integer;
            rr, ug, ul, vl, vg, x,v: real;
            fluid: fluidtype;
    Begin
        fluid := VdW;
        If T < pa[fluid].Tc Then Begin
                i := 0;
                Repeat
                    i := i + 1;
                Until (T >= ct[i, fluid].T) And (T <= ct[i + 1, fluid].T);
                rr := (T - ct[i, fluid].T) / (ct[i + 1, fluid].T - ct[i, fluid].T);
                ug := ct[i, fluid].ug * (1 - rr) + rr * ct[i + 1, fluid].ug;
                ul := ct[i, fluid].ul * (1 - rr) + rr * ct[i + 1, fluid].ul;
                vg := ct[i, fluid].vg * (1 - rr) + rr * ct[i + 1, fluid].vg;
                vl := ct[i, fluid].vl * (1 - rr) + rr * ct[i + 1, fluid].vl;
                If (u > ul) And (u < ug) Then Begin
                        x := (u - ul) / (ug - ul);
                        v := (1 - x) * vl + x * vg;
                    End
                Else
                    v := 3.0 / (4 * T - U)
            End
        Else
            v := 3.0 / (4 * T - U);
            if v < 0.34 then VfromUT := 0.34
              else VfromUT := v;
    End; {VfromUT}

    Function VfromST (S, T: real): real;
        Var
            i: integer;
            rr, sg, sl, vl, vg, x: real;
            fluid: fluidtype;
    Begin
        fluid := VdW;
        If T < pa[fluid].Tc Then Begin
                i := 0;
                Repeat
                    i := i + 1;
                Until (T >= ct[i, fluid].T) And (T <= ct[i + 1, fluid].T);
                rr := (T - ct[i, fluid].T) / (ct[i + 1, fluid].T - ct[i, fluid].T);
                sg := ct[i, fluid].sg * (1 - rr) + rr * ct[i + 1, fluid].sg;
                sl := ct[i, fluid].sl * (1 - rr) + rr * ct[i + 1, fluid].sl;
                vg := ct[i, fluid].vg * (1 - rr) + rr * ct[i + 1, fluid].vg;
                vl := ct[i, fluid].vl * (1 - rr) + rr * ct[i + 1, fluid].vl;
                If (s > sl) And (s < sg) Then Begin
                        x := (s - sl) / (sg - sl);
                        VfromST := (1 - x) * vl + x * vg;
                    End
                Else
                    VfromST := (1.0 / 3.0) + exp(0.375 * (S - 4.0) - (1 + 1.5 * ln(T)))
            End
        Else
            VfromST := (1.0 / 3.0) + exp(0.375 * (S - 4.0) - (1 + 1.5 * ln(T)))
    End; {VfromST}

    Function VfromTP (T, P: Real): Real;
     { Find volume by solving cubic equation }
        Const
            pi = 3.141592654;
        Var
            D, Q, R, a1, a2, a3, sign1, sign2, rr, Pt, v1, v2, v3, vs: real;
            i: integer;
    Begin
        a1 := -((1.0 / 3.0) + 8 * T / (3 * p));
        a2 := 3 / P;
        a3 := -1 / P;
        Q := (3 * a2 - a1 * a1) / 9;
        R := (9 * a1 * a2 - 27 * a3 - 2 * a1 * a1 * a1) / 54;
        D := (Q * Q * Q + R * R);
        If D > 0 Then Begin
                D := sqrt(D);
                sign1 := (R + D) / abs(R + D);
                sign2 := (R - D) / abs(R - D);
                VfromTP := sign1 * pwr(abs(R + D), 1 / 3) + sign2 * pwr(abs(R - D), 1.0 / 3.0) - a1 / 3;
            End
        Else Begin
                i := 0;
                Repeat
                    i := i + 1;
                Until (T >= ct[i, VdW].T) And (T <= ct[i + 1, VdW].T);
                rr := (T - ct[i, VdW].T) / (ct[i + 1, VdW].T - ct[i, VdW].T);
                Pt := ct[i, VdW].P * (1 - rr) + rr * ct[i + 1, VdW].P;
                a2 := arccos(R / sqrt(-Q * Q * Q));
                v1 := 2 * sqrt(-Q) * cos((a2 + 4 * pi) / 3) - a1 / 3;
                v2 := 2 * sqrt(-Q) * cos((a2 + 2 * pi) / 3) - a1 / 3;
                v3 := 2 * sqrt(-Q) * cos((a2) / 3) - a1 / 3;
                If P > Pt Then {pick smallest v}
                    Begin
                        If v1 < v2 Then
                            vs := v1
                        Else
                            vs := v2;
                        If v3 < vs Then
                            vs := v3
                    End
                Else Begin
                        If v1 > v2 Then
                            vs := v1
                        Else
                            vs := v2;
                        If v3 > vs Then
                            vs := v3
                    End;
                VfromTP := vs;
            End;
    End;{VfromTP}

    Function TfromPV (P, V: Real): Real;
        Var
            dpdT, T, vg, vl: real;
    Begin
        If (P < 1) Then Begin
                VTfromP(P, VdW, vl, vg, T, dPdT);
                If (v > vl) And (v < vg) Then
                    TfromPV := T
                Else
                    TfromPV := (P * V * V + 3) * (3 * V - 1) / (8 * V * V);
            End
        Else
            TfromPV := (P * V * V + 3) * (3 * V - 1) / (8 * V * V);
    End; {TfromPV}

{S  part1}


{---------- Functions giving free energy and its derivatives ---------}

    Function F (T, v: real;
                                    recur: boolean;
                                    fluid: fluidtype): real;
        Var
            vl, vg, p, dpdT, x, Fg, Ff: real;

        Function FH2O (T, v: real): real;
            Var
                rho, tau, psi, q, q1, q2, cc: real;
                i, j: integer;
        Begin
            With consts Do Begin
                    psi := 0;
                    rho := 1 / v;
                    T := T + Tk;
                    tau := 1000 / T;
                    cc := tau;
                    For i := 1 To 6 Do Begin
                            cc := cc / tau;
                            psi := psi + cc * c[i];
                        End;
                    psi := psi + c[7] * ln(T) + c[8] * ln(T) / tau;
                    q := 0;
                    For j := 1 To 7 Do Begin
                            q1 := 0;
                            For i := 1 To 8 Do
                                q1 := q1 + Aij[i, j] * pwr(rho - raj[j], i - 1);
                            q2 := 0;
                            For i := 9 To 10 Do
                                q2 := q2 + Aij[i, j] * pwr(rho, i - 9);
                            q2 := q2 * exp(-E * rho);
                            q := q + (q1 + q2) * pwr(tau - taj[j], j - 2);
                        End;
                    q := q * (tau - taj[1]);
                    FH2O := psi + R * T * (ln(rho) + rho * q);
                End;
        End;{FH2O}

    Begin {F}
        If (T > pa[fluid].Tc) Or recur Then
            If fluid = water Then
                F := FH2O(T, v)
            Else
                F := (-3 / v) - (8 / 3) * T * (1 + ln(v - 0.333333) + 1.5 * ln(T))
        Else Begin
                PVfromT(T, fluid, vl, vg, p, dpdT);
                If (v > vl) And (v < vg) Then Begin
                        x := (v - vl) / (vg - vl);
                        F := x * F(T, vg, true, fluid) + (1 - x) * F(T, vl, true, fluid);
                    End
                Else
                    F := F(T, v, true, fluid)
            End;
    End;

    Function dFdT (T, v: real;
                                    recur: boolean;
                                    fluid: fluidtype): real;
        Var
            vl, vg, p, dpdT, x, dff, dfg: real;

        Function dFdTH2O (T, v: real): real;
            Var
                rho, tau, psi, q, qp, q1, q2, cc: real;
                i, j: integer;
        Begin
            With consts Do Begin
                    psi := 0;
                    rho := 1 / v;
                    T := T + Tk;
                    tau := 1000 / T;
                    cc := tau;
                    For i := 2 To 6 Do Begin
                            cc := cc / tau;
                            psi := psi + (i - 1) * cc * c[i];
                        End;
                    psi := (psi / 1000) + (c[7] / T) + (c[8] / 1000) * (1 + ln(T));
                    q := 0;
                    qp := 0;
                    For j := 1 To 7 Do Begin
                            q1 := 0;
                            For i := 1 To 8 Do
                                q1 := q1 + Aij[i, j] * pwr(rho - raj[j], i - 1);
                            q2 := 0;
                            For i := 9 To 10 Do
                                q2 := q2 + Aij[i, j] * pwr(rho, i - 9);
                            q2 := q2 * exp(-E * rho);
                            cc := (q1 + q2) * pwr(tau - taj[j], j - 2);
                            q := q + cc;
                            qp := qp + cc * (j - 2) / (tau - taj[j]);
                        End;
                    dFdTH2O := psi + R * (ln(rho) + rho * ((tau - taj[1]) * (q - tau * qp) - tau * q));
                End;
        End;{dFdTH2O}

    Begin
        If (T > pa[fluid].Tc) Or recur Then
            If fluid = water Then
                dFdT := dFdTH2O(T, v)
            Else
                dFdT := -(8 / 3) * (2.5 + ln(v - 0.333333) + 1.5 * ln(T))
        Else Begin
                PVfromT(T, fluid, vl, vg, p, dpdT);
                If (v > vl) And (v < vg) Then Begin
                        x := (v - vl) / (vg - vl);
                        dFdT := x * dFdT(T, 1.001 * vg, true, fluid) + (1 - x) * dFdT(T, 0.999 * vl, true, fluid);
                    End
                Else
                    dFdT := dFdT(T, v, true, fluid)
            End;
    End;

    Function dFdv (T, v: real;
                                    recur: boolean;
                                    fluid: fluidtype): real;
        Var
            vl, vg, p, dpdT: real;

        Function dFdvH2O (T, v: real): real;
            Var
                rho, tau, q, q1, q2, qp, q1p, q2p, cc, cc2: real;
                i, j: integer;
        Begin
            With consts Do Begin
                    rho := 1 / v;
                    T := T + Tk;
                    tau := 1000 / T;
                    q := 0;
                    qp := 0;
                    For j := 1 To 7 Do Begin
                            q1 := 0;
                            q1p := 0;
                            cc := 1;
                            For i := 1 To 8 Do Begin
                                    q1 := q1 + Aij[i, j] * cc;
                                    If i > 1 Then
                                        q1p := q1p + Aij[i, j] * (i - 1) * cc2;
                                    cc2 := cc;
                                    cc := cc * (rho - raj[j]);
                                End;
                            cc := exp(-E * rho);
                            q2 := cc * (Aij[9, j] + rho * Aij[10, j]);
                            q2p := (-E * Aij[9, j] + (1 - E * rho) * Aij[10, j]) * cc;
                            cc := pwr(tau - taj[j], j - 2);
                            q := q + (q1 + q2) * cc;
                            qp := qp + (q1p + q2p) * cc;
                        End;
                    dFdvH2O := -R * T * rho * (1 + rho * (tau - taj[1]) * (q + rho * qp));
                End;
        End;{dFdvH2O}

    Begin
        If (T > pa[fluid].Tc) Or recur Then
            If fluid = water Then
                dFdv := dFdvH2O(T, v)
            Else
                dFdv := (3 / (v * v)) - (8 / 3) * T / (v - 0.333333)
        Else Begin
                PVfromT(T, fluid, vl, vg, p, dpdT);
                If (v > vl) And (v < vg) Then
                    dFdv := -p
                Else
                    dFdv := dFdv(T, v, true, fluid)
            End;
    End;{dFdv}

    Function d2Fdv2 (T, v: real;
                                    recur: boolean;
                                    fluid: fluidtype): real;
        Var
            vl, vg, p, dpdT: real;

        Function d2Fdv2H2O (T, v: real): real;
            Var
                rho, tau, qpp, q1pp, q2pp, qp, q1p, q2p, cc, cc2: real;
                i, j: integer;
        Begin
            With consts Do Begin
                    rho := 1 / v;
                    T := T + Tk;
                    tau := 1000 / T;
                    qp := 0;
                    qpp := 0;
                    For j := 1 To 7 Do Begin
                            q1p := 0;
                            q1pp := 0;
                            cc := 1;
                            For i := 2 To 8 Do Begin
                                    q1p := q1p + (i - 1) * Aij[i, j] * cc;
                                    If i > 2 Then
                                        q1pp := q1pp + Aij[i, j] * (i - 1) * (i - 2) * cc2;
                                    cc2 := cc;
                                    cc := cc * (rho - raj[j]);
                                End;
                            cc := exp(-E * rho);
                            q2pp := cc * (-E * E * Aij[9, j] - E * (2 - E * rho) * Aij[10, j]);
                            q2p := (-E * Aij[9, j] + (1 - E * rho) * Aij[10, j]) * cc;
                            cc := pwr(tau - taj[j], j - 2);
                            qp := qp + (q1p + q2p) * cc;
                            qpp := qpp + (q1pp + q2pp) * cc;
                        End;
                    d2Fdv2H2O := R * T * rho * rho * (1 + rho * rho * (tau - taj[1]) * (4 * qp + rho * qpp));
                End;
        End;{d2Fdv2H2O}

    Begin
        If (T > pa[fluid].Tc) Or recur Then
            If fluid = water Then
                d2Fdv2 := d2Fdv2H2O(T, v)
            Else
                d2Fdv2 := (-6 / (v * v * v)) + (8 / 3) * T / sqr(v - 0.333333)
        Else Begin
                PVfromT(T, fluid, vl, vg, p, dpdT);
                If (v > vl) And (v < vg) Then
                    d2Fdv2 := 0
                Else
                    d2Fdv2 := d2Fdv2(T, v, true, fluid)
            End;
    End;

    Function d2FdvdT (T, v: real;
                                    recur: boolean;
                                    fluid: fluidtype): real;
        Var
            vl, vg, p, dpdT: real;

        Function d2FdvdTH2O (T, v: real): real;
            Var
                rho, tau, q, q1, q2, qp, q1p, q2p, cc, cc2: real;
                i, j: integer;
        Begin
            With consts Do Begin
                    rho := 1 / v;
                    T := T + Tk;
                    tau := 1000 / T;
                    q := 0;
                    qp := 0;
                    For j := 1 To 7 Do Begin
                            q1 := 0;
                            q1p := 0;
                            cc := 1;
                            For i := 1 To 8 Do Begin
                                    q1 := q1 + Aij[i, j] * cc;
                                    If i > 1 Then
                                        q1p := q1p + Aij[i, j] * (i - 1) * cc2;
                                    cc2 := cc;
                                    cc := cc * (rho - raj[j]);
                                End;
                            cc := exp(-E * rho);
                            q2 := cc * (Aij[9, j] + rho * Aij[10, j]);
                            q2p := (-E * Aij[9, j] + (1 - E * rho) * Aij[10, j]) * cc;
                            cc := pwr(tau - taj[j], j - 2);
                            cc := (q1 + q2 + rho * (q1p + q2p)) * cc;
                            q := q + cc;
                            qp := qp + cc * (j - 2) / (tau - taj[j]);
                        End;
                    d2FdvdTH2O := -R * rho * (1 + rho * ((tau - taj[1]) * (q - tau * qp) - tau * q));
                End;
        End;{d2FdvdTH2O}

    Begin
        If (T > pa[fluid].Tc) Or recur Then
            If fluid = water Then
                d2FdvdT := d2FdvdTH2O(T, v)
            Else
                d2FdvdT := -(8 / 3) / (v - 0.333333)
        Else Begin
                PVfromT(T, fluid, vl, vg, p, dpdT);
                If (v > vl) And (v < vg) Then
                    d2FdvdT := -dpdT
                Else
                    d2FdvdT := d2FdvdT(T, v, true, fluid)
            End;
    End;

    Function d2FdT2 (T, v: real;
                                    recur: boolean;
                                    fluid: fluidtype): real;
        Var
            vl, vg, p, dpdT, x: real;

        Function d2FdT2H2O (T, v: real): real;
            Var
                rho, tau, psi, qp, qpp, q1, q2, cc: real;
                i, j: integer;
        Begin
            With consts Do Begin
                    psi := 0;
                    rho := 1 / v;
                    T := T + Tk;
                    tau := 1000 / T;
                    cc := tau;
                    For i := 3 To 6 Do Begin
                            cc := cc / tau;
                            psi := psi + (i - 1) * (i - 2) * cc * c[i];
                        End;
                    psi := (psi / sqr(1000)) - (c[7] / (T * T)) + c[8] / (T * 1000);
                    qp := 0;
                    qpp := 0;
                    For j := 1 To 7 Do Begin
                            q1 := 0;
                            For i := 1 To 8 Do
                                q1 := q1 + Aij[i, j] * pwr(rho - raj[j], i - 1);
                            q2 := 0;
                            For i := 9 To 10 Do
                                q2 := q2 + Aij[i, j] * pwr(rho, i - 9);
                            q2 := q2 * exp(-E * rho);
                            cc := (j - 2) * (q1 + q2) * pwr(tau - taj[j], j - 3);
                            qp := qp + cc;
                            qpp := qpp + cc * (j - 3) / (tau - taj[j]);
                        End;
                    d2FdT2H2O := psi + (R * tau * tau * rho / T) * (2 * qp + qpp * (tau - taj[1]));
                End;
        End;{d2FdT2H2O}

    Begin
        If fluid = VdW Then
            d2FdT2 := -4 / T
        Else If (T > pa[fluid].Tc) Or recur Then
            d2FdT2 := d2FdT2H2O(T, v)
        Else Begin
                PVfromT(T, fluid, vl, vg, p, dpdT);
                If (v > vl) And (v < vg) Then Begin
                        x := (v - vl) / (vg - vl);
                        d2FdT2 := x * d2FdT2(T, 1.001 * vg, true, fluid) + (1 - x) * d2FdT2(T, 0.999 * vl, true, fluid);
                    End
                Else
                    d2FdT2 := d2FdT2(T, v, true, fluid)
            End;
    End;

{S  part2}

{---------------Choose and Draw phase diagrams-------------}
    Procedure plotlocations (Var loc: loctype);
    Begin
        loc[1, 1] := 0.10;
        loc[1, 2] := 0.45;
        loc[1, 3] := 0.58;
        loc[1, 4] := 0.93;
        loc[2, 1] := 0.60;
        loc[2, 2] := 0.95;
        loc[2, 3] := 0.58;
        loc[2, 4] := 0.93;
        loc[3, 1] := 0.10;
        loc[3, 2] := 0.45;
        loc[3, 3] := 0.13;
        loc[3, 4] := 0.48;
        loc[4, 1] := 0.60;
        loc[4, 2] := 0.95;
        loc[4, 3] := 0.13;
        loc[4, 4] := 0.48;
    End;

    Procedure chooseplots (Var pd: pdtype;
                                    Var fluid: fluidtype);
        Var
            i: integer;
            cscreen: TInputscreen;
    Begin
{        new(cscreen);}
        With cscreen Do Begin
                init;
                DefineInputPort(0.17, 0.83, 0.15, 0.85);
                loadline('');
                loadline(' Choose fluid:   #1Water   #1Van der Waals model ');
                loadline('');
                loadline(' Choose four phase diagrams to plot');
                loadline('');
                loadline('         vT    PT    Pv    sv    sT    uv    uT');
                loadline(' Plot 1  #2    #2    #2    #2    #2    #2    #2');
                loadline(' Plot 2  #3    #3    #3    #3    #3    #3    #3');
                loadline(' Plot 3  #4    #4    #4    #4    #4    #4    #4');
                loadline(' Plot 4  #5    #5    #5    #5    #5    #5    #5');
               loadline('      P - pressure         s - molar entropy');
               loadline('      T - temperature      u - molar energy');
               loadline('      v - molar volume ');
               loadline('');
                loadline('               [  OK  ]   [Cancel]  ');
                If fluid = VdW Then
                    setRadioButton('1', 2)
                Else
                    setRadioButton('1', 1);
                setRadioButton('2', pd[1]);
                setRadioButton('3', pd[2]);
                setRadioButton('4', pd[3]);
                setRadioButton('5', pd[4]);
                Accept;
                ClearInputPort;
                If Not (canceled) Then Begin
                        If (getradiobutton('1') = 1) Then
                            fluid := water
                        Else
                            fluid := VdW;
                  pd[1]:= getRadioButton('2');
                  pd[2]:= getRadioButton('3');
                  pd[3]:= getRadioButton('4');
                  pd[4]:= getRadioButton('5');
               End;
                done;
            End;{with}
    End;{chooseplots}

    Procedure phasediagrams (Var CT: coextype;
                                    Var loc: loctype);
        Var
            i, where: integer;
            xA, yA, xB, yB: real;
            knum: byte;
            plotcolor: integer;
            datapoint: datatype;
            menuchosen: boolean;



        Procedure DefineAndOpen (i: integer;
                                        xsmin, xsmax, ysmin, ysmax, ticx, ticy: real;
                                        lab1, lab2: String);
        Begin
            DefineScale(i, xsmin, xsmax, ysmin, ysmax);
            DefineViewPort(i, loc[i, 1], loc[i, 2], loc[i, 3], loc[i, 4]);
            SetColor(yellow);
            OpenViewPort(i);
            SelectScale(i);
            If ticx > 0 Then
                Axis(xsmin, ysmin, ticx, ticy);
            SetColor(white);
            PutLabel(left, lab1);
            PutLabel(bottom, lab2);
            SetColor(yellow);
        End;

        Procedure labelphases (xgv, ygv, xlv, ylv, xgw, ygw, xlw, ylw: real;
                                        fluid: fluidtype);
        Begin
            SetColor(whitte);
            If fluid = vdw Then Begin
                    outtextxy(mapx(xgv), mapy(ygv), 'G');
                    outtextxy(mapx(xlv), mapy(ylv), 'L');
                End
            Else Begin
                    outtextxy(mapx(xgw), mapy(ygw), 'G');
                    outtextxy(mapx(xlw), mapy(ylw), 'L');
                End;
            SetColor(blue);
        End;


        Procedure DrawCoex (var CT: coextype;
                                        fluid: fluidtype;
                                        plottype, plotloc: integer);
            Var
                i, j, ncc, ncc2, ndata: integer;
                x, y: DataVector;

        Begin {DrawCoex}
            ncc := pa[fluid].ncc;
            ncc2 := 2 * ncc + 1;
           Case plottype Of
                1: 
                    For j := 1 To ncc Do
                        With ct[j, fluid] Do Begin
                                x[j] := T;
                                y[j] := vl;
                                y[ncc2 - j] := vg;
                                x[ncc2 - j] := T;
                            End;
                2: 
                    For j := 1 To ncc Do
                        With ct[j, fluid] Do Begin
                                x[j] := T;
                                y[j] := p;
                            End;

                3: 
                    For j := 1 To ncc Do
                        With ct[j, fluid] Do Begin
                                x[j] := vl;
                                y[j] := p;
                                y[ncc2 - j] := p;
                                x[ncc2 - j] := vg;
                            End;
                4:
                    For j := 1 To ncc Do
                        With ct[j, fluid] Do Begin
                                x[j] := vl;
                                y[j] := sl;
                                y[ncc2 - j] := sg;
                                x[ncc2 - j] := vg;
                            End;
                5: 
                    For j := 1 To ncc Do
                        With ct[j, fluid] Do Begin
                                x[j] := T;
                                y[j] := sl;
                                y[ncc2 - j] := sg;
                                x[ncc2 - j] := T;
                            End;
                6: 
                    For j := 1 To ncc Do
                        With ct[j, fluid] Do Begin
                                x[j] := vl;
                                y[j] := ul;
                                y[ncc2 - j] := ug;
                                x[ncc2 - j] := vg;
                            End;
                7: 
                    For j := 1 To ncc Do
                        With ct[j, fluid] Do Begin
                                x[j] := T;
                                y[j] := ul;
                                y[ncc2 - j] := ug;
                                x[ncc2 - j] := T;
                            End;
            End;{case}
            With pa[fluid] Do
                Case plottype Of
                    1:  Begin
                            DefineAndOpen(plotloc, Tmn, Tmx, Vmn, Vmx, Ttic, Vtic, 'Volume', 'Temperature');
                            labelphases(0.9, 3.0, 0.93, 0.57, 340.0, 13.0, 370.0, 2.0, fluid);
                        End;
                    2:  Begin
                            DefineAndOpen(plotloc, Tmn, Tmx, Pmn, Pmx, Ttic, Ptic, 'Pressure', 'Temperature');
                            labelphases(0.9, 0.3, 0.5, 0.5, 350.0, 10.0, 150.0, 10.0, fluid);
                        End;
                    3:  Begin
                            DefineAndOpen(plotloc, Vmn, Vmx, Pmn, Pmx, Vtic, Ptic, 'Pressure', 'Volume');
                            labelphases(3.0, 0.8, 0.5, 0.8, 13.0, 17.0, 1.3, 20.0, fluid);
                        End;
                    4:  Begin
                            DefineAndOpen(plotloc, Vmn, Vmx, Smn, Smx, Vtic, Stic, 'Entropy', 'Volume');
                            labelphases(2.5, 9.5, 0.4, 4.0, 13.0, 6.5, 1.3, 4.0, fluid);
                        End;
                    5:  Begin
                            DefineAndOpen(plotloc, Tmn, Tmx, Smn, Smx, Ttic, Stic, 'Entropy', 'Temperature');
                            labelphases(0.85, 9.5, 0.93, 2.0, 250.0, 8.0, 250.0, 2.0, fluid);
                        End;
                    6:  Begin
                            DefineAndOpen(plotloc, Vmn, Vmx, Umn, Umx, Vtic, Utic, 'Energy', 'Volume');
                            labelphases(3.0, 4.0, 0.5, 0, 13.0, 2800.0, 1.3, 1800.0, fluid);
                        End;
                    7:  Begin
                            DefineAndOpen(plotloc, Tmn, Tmx, Umn, Umx, Ttic, Utic, 'Energy', 'Temperature');
                            labelphases(0.5, 3.4, 0.9, -3.0, 50.0, 2800.0, 200.0, 800.0, fluid);
                        End;
                End;{CASE}
            ndata := 2 * ncc;
            setcolor(yellow);
            If (fluid = VdW) And ((plottype = 1) Or (plottype = 6)) Then
                ndata := ndata - 15;
            If (fluid = water) And ((plottype = 1) Or (plottype = 6) Or (plottype = 4)) Then
                ndata := ndata - 18;
            If plottype = 2 Then
                PlotData(x, y, ncc)
            Else
                PlotData(x, y, ndata);
            If fluid = water Then
                Message('Click and drag between two places in vT window for path.')
            Else
                Message('Click and drag between two places in a window (not s-v or u-v) for path');

        End; {DRAWCOEX}

{--------Choose path in one diagram and create paths in other diagrams ------}
        Procedure newcolor (Var Plotcolor: integer);
        Begin
            Case plotcolor Of
                green:
                    plotcolor := lightmagenta;
                lightmagenta:
                    plotcolor := lightred;
               lightred:
                    plotcolor := yellow;
                yellow:
                    plotcolor := green;
            End;{case}
        End;

        Function Ta (T: real;
                                        fluid: fluidtype): real;
        Begin
            If fluid = water Then
                Ta := T + consts.Tk
            Else
                Ta := T;
        End;

        Procedure ShowGraph (vnum: integer;
                                        Xvect, Yvect: Dataarray;
                                        N: integer);
            Var
                i: integer;
        Begin {ShowGraph}
            SelectViewPort(vnum);
            SelectScale(vnum);
            SetColor(plotcolor);
            PlotSymbol(Xvect[1], Yvect[1], 'A');
            if not ((Xvect[1]=Xvect[N]) and (Yvect[1]=Yvect[N])) then
            begin
               For i := 2 To N - 1 Do
                   circle(mapx(Xvect[i]), mapy(Yvect[i]), 1);
               PlotSymbol(Xvect[N], Yvect[N], 'B');
            end;
        End; {ShowGraph}

        Procedure createpaths (where: integer;
                                        Var pd: pdtype;
                                        xA, yA, xB, yB: real;
                                        fluid: fluidtype;
                                        Var datapoint: datatype);
            Const
                npts = 20;
            Var
                i: integer;
                dx, dy, fe: real;
                p, v, T, s, u: dataarray;
        Begin
            Message('Calculating data for paths....');
            dx := (xB - xA) / (npts - 1);
            For i := 1 To npts Do
                T[i] := xA + (i - 1) * dx;
            dy := (yB - yA) / (npts - 1);
            For i := 1 To npts Do
                v[i] := yA + (i - 1) * dy;
            Case pd[where] Of
                1: 
                    For i := 1 To npts Do Begin
                            p[i] := -dfdv(T[i], v[i], false, fluid);
                            s[i] := -dfdT(T[i], v[i], false, fluid);
                            fe := f(T[i], v[i], false, fluid);
                            u[i] := fe + Ta(T[i], fluid) * s[i];
                        End;
                2: 
                    For i := 1 To npts Do Begin
                            p[i] := v[i];
                            v[i] := vfromTp(T[i], p[i]);
                            s[i] := -dfdT(T[i], v[i], false, fluid);
                            fe := f(T[i], v[i], false, fluid);
                            u[i] := fe + Ta(T[i], fluid) * s[i];
                        End;
                3: 
                    For i := 1 To npts Do Begin
                            p[i] := v[i];
                            v[i] := T[i];
                            T[i] := Tfrompv(p[i], v[i]);
                            s[i] := -dfdT(T[i], v[i], false, fluid);
                            fe := f(T[i], v[i], false, fluid);
                            u[i] := fe + Ta(T[i], fluid) * s[i];
                        End;
                5: 
                    For i := 1 To npts Do Begin
                            s[i] := v[i];
                            v[i] := vfromST(s[i], T[i]);
                            p[i] := -dfdv(T[i], v[i], false, fluid);
                            fe := f(T[i], v[i], false, fluid);
                            u[i] := fe + Ta(T[i], fluid) * s[i];
                        End;
                7: 
                    For i := 1 To npts Do Begin
                            u[i] := v[i];
                            v[i] := vfromUT(u[i], T[i]);
                            p[i] := -dfdv(T[i], v[i], false, fluid);
                            s[i] := -dfdT(T[i], v[i], false, fluid);
                            fe := u[i] - Ta(T[i], fluid) * s[i];
                        End;
            End;{case}
            For i := 1 To 4 Do
                Case pd[i] Of
                    1: 
                        showgraph(i, T, v, npts);
                    2: 
                        showgraph(i, T, p, npts);
                    3: 
                        showgraph(i, v, p, npts);
                    4: 
                        showgraph(i, v, s, npts);
                    5: 
                        showgraph(i, T, s, npts);
                    6: 
                        showgraph(i, v, u, npts);
                    7: 
                        showgraph(i, T, u, npts);
                End;{case}
            datapoint.p := p[npts];
            datapoint.s := s[npts];
            datapoint.T := T[npts];
            datapoint.u := u[npts];
            datapoint.v := v[npts];
            datapoint.fe := fe;
            If fluid = water Then
                Message('Click and drag between two places in vT window for path.')
            Else
                Message('Click and drag between two places in a window (not s-v or u-v) for path');
       End;{createpaths}

        Procedure choosepath (fluid: fluidtype;
                                        Var pd: pdtype;
                                        Var datapoint: datatype);
            Var
                inside: boolean;
                xA, yA, xB, yB: real;
                where: integer;
                a,b : pointType;
        Begin
            where := 1;
            inside := false;
            Repeat
                MousePosn(xA, yA, where, where, inside);
                If Not inside Then
                    where := where + 1;
            Until inside Or (where > 4);
            If where < 5 Then
                If ((fluid = vdw) And (pd[where] In [1, 2, 3, 5, 7])) Or ((fluid = water) And (pd[where] = 1)) Then Begin
                        SelectViewPort(where);
                        with views[where] do MouseBorders(vx1,vy1,vx2,vy2);
                        SelectScale(where);
                        SetColor(Blakk);
                        PlotSymbol(xA, yA, 'A');
                        SetColor(whitte);
                        a.x := event.x-views[where].vx1;
                        a.y := event.y-views[where].vy1;
                        DrawRubberBand(RLine,a,b,DarkGray,true);
{                        inside := false;
                        Repeat
                            waitonmouseclick;
                            MousePosn(xB, yB, where, where, inside);
                        Until inside;    }
                        MousePosn(xB, yB, where, where, inside);
                        if not(xA=xB) and (yA=yB) then
                        PlotSymbol(xB, yB, 'B');
                        MouseBorders(0,0,GetMaxX,GetMaxY);
                        newcolor(plotcolor);
                        createpaths(where, pd, xA, yA, xB, yB, fluid, datapoint);
                    End{if}
                    else
                       Announce('You may not draw in this window');
        End;{choosepath}


{S  part3}


        Procedure showdata (Var datapoint: datatype;
                                        fluid: fluidtype);
            Var
                d2fdv2a, d2fdvdTa: real;
                i: integer;
                C: HelpScrType;
        Begin
            For i := 1 To 25 Do
                C[i] := '';
            If datapoint.T < 0 Then Begin
                    C[5] := 'Data calculated from last point of a path';
                    C[7] := 'You must draw at least one path';
                End
            Else
                With datapoint Do Begin
                  c[1] := 'Thermodynamic data for last point plotted';
                        d2fdv2a := d2Fdv2(T, v, false, fluid);
                        d2fdvdTa := d2FdvdT(T, v, false, fluid);
                        cv := -Ta(T, fluid) * d2FdT2(T, v, false, fluid);
                        KappaTi := v * d2fdv2a;
                        cpcvi := d2fdv2a / (Ta(T, fluid) * sqr(d2fdvdTa));
                        CTEi := -v * d2fdv2a * d2fdvdTa;
                        If fluid = water Then
                            C[2] := 'water'
                        Else
                            C[2] := 'van der Waals model';
                        C[4] := concat('Pressure =                          ', scnumstr(P, 2));
                        C[5] := concat('Temperature =                       ', scnumstr(T, 2));
                        C[6] := concat('Specific Volume =                   ', scnumstr(v, 2));
                        C[7] := concat('Specific Energy =                   ', scnumstr(u, 2));
                        C[8] := concat('Specific Entropy =                  ', scnumstr(s, 2));
                        C[9] := concat('Specific Free Energy =              ', scnumstr(fe, 2));
                        C[10] := concat('Specific Heat at constant volume =  ', scnumstr(cv, 2));
                        If (KappaTi > 0.0001) Then
                        C[11] := concat('Isothermal Compressibility =        ', scnumstr(1 / KappaTi, 2))

                        Else
                            C[11] := 'Isothermal Compressibility diverges';
                        If (cpcvi > 0.0001) Then
                        C[12] := concat('cp-cv =                             ', scnumstr(1 / cpcvi, 2))

                        Else
                            C[12] := 'cp-cv diverges';
                        If (CTEi > 0.0001) Then
                        C[13] := concat('Coef. of Thermal Expansion =        ', scnumstr(1 / CTEi, 2))
                        Else
                            C[13] := 'Coefficient of Thermal Expansion diverges';
                    End;{with}
            C[24] := '     Press any key or click mouse to continue';
            HELP(C);
        End;

   procedure HandleHotKeys(kNum : byte);
   var i : integer;
   begin
                   Case knum Of
                       1:
                           HelpScreen;
                       2:  Begin
                               clearmuppetport;
                                For i := 1 To 4 Do
                                   DrawCoex(CT, fluid, pd[i], i);
                               hotkeys.display;
                           End;
                       3:  Begin
                               chooseplots(pd, fluid);
                               clearmuppetport;
                               For i := 1 To 4 Do
                                   DrawCoex(CT, fluid, pd[i], i);
                               hotkeys.display;
                           End;
                       4:
                           showdata(datapoint, fluid);
                       5:
                           menuchosen := true;
                   End{case}
   end;

    Begin {phasediagrams}
        plotcolor := green;
        showcursor;{ showmouse}
        knum := 3;
        datapoint.T := -1;
        menuchosen := false;
        For i := 1 To 4 Do
            DrawCoex(CT, fluid, pd[i], i);
        hotkeys.display;
        Repeat
            checkforevents;
            If hotkeys.pressed(knum) Then HandleHotKeys(kNum)
            Else If event.mouseclicked Then
                choosepath(fluid, pd, datapoint);
        Until menuchosen;
    End;{phasediagrams}


procedure HandleMenu;
begin
   with menu do
   Case colchosen Of
       1:  Begin
               Case rowchosen Of
                   1:
                       AboutCups;
                   2:
                       AboutProgram;
                   3:
                       Configuration;
                   4:
                       Quit := true;
               End; {case}
               If Not quit Then
                   PhaseDiagrams(CT, loc);
           End;
       2:  Begin
               clearmuppetport;
               chooseplots(pd, fluid);
               PhaseDiagrams(CT, loc);
           End;
       3:begin  case rowchosen of
            1:
               HowtoUseProgram;
            2:
               AboutUnits;
            3:
               AboutFE;
        end; {case}
                  PhaseDiagrams(CT, loc);
                  end;

  

   End; {case}
end;

 begin {main}

    cupsinit;
    Createcoexistence(CT);
    plotlocations(loc);
    AboutProgram;
    InitializeMenu(Menu);
    setuphotkeys(hotkeys);
    quit := false;
    For i := 1 To 4 Do
        pd[i] := i;
    fluid := VdW;
    chooseplots(pd, fluid);
    PhaseDiagrams(CT, loc);

    Repeat
       If menu.chosen Then HandleMenu;
    Until quit;
    cupsDone;
End.