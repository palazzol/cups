Unit atPhys;

{------------Modified June 14, 1994------------------}
interface

USES  crt, dos, graph, printer, CUPS, cupsMUPP, CUPSfunc,CupsProc,cupsgui, CUPSGrph, ATCommon;
 {----------------------------------------------}

{------------------------------------------}
{In all of the following, P is gas pressure}
	procedure DoState1 (V, T, Hab, Heab, Metab: real; var P, Pel, xH, xHe, yHe, mu: real); {solves ionization eq}
	procedure FindV (P, T, Hab, HeAb, MetAb: real; var V, mu, xH, xHe, yHe, Pe: real); {iterative solultion of}
{ionization equation given pressure and temperature: finds specific volume and degrees of ionization}
       {	procedure FindT (P, V, Hab, HeAb, MetAb: real; var T, mu, xH, xHe, yHe, Pe: real);} {iterative solultion of}
{ionization equation given pressure and specific volume: finds  temperature and degrees of ionization}
	function OpChristy (V, T, Hab, HeAb, MetAb: real): real; {opacity per unit electron pressure}
	function OHMinusOP (T, xH, Hab, mu: real): real;{ H- opacity per unit electron pressure}
	procedure FindKapp (V, T, xH, mu, Hab, HeAb, MetAb, Pel: real; var kappa: real); {calculates total opacity}
	procedure findTau (N: integer; TauTop, Taubot: real; modsel: integer);
	function fdLnPdtau (t: real; y: garray): real;{dlnP/dtau= g/kappa}
	procedure FAdGrad (T, beta, xH, XHe, yHe, Hab, HeAb: real; var AdGradT, Cp: real); {find adiabatic gradient, dLnT/dLnP and Cp}
{see Cox and Giuli, page 225; adGrad is later stored as vari[gamm]}
	function FRadGrad (tauTemp, P, T, mu: real): real; {radiative dlnT/dlnP}
	procedure fMixingLength (grav, teff, mu, H, V, TTemp, kapp, agradT, rgradT, Cp: real; var gradT, MachNo: real);

	procedure BuildNewLayer (s: integer; var P, T, H: real);
	procedure LoadLayer (P, T, H: real; s: integer); {compute and put variables into array}
	procedure BuildModel; {If global RebuildModel is FALSE  this is a new model using theoretical temp}
   Procedure findOpacity(m,S:integer;w:real;absorber:integer;var kappa:real);

{for radiation field}
	function BB (T, WN: real): real;{Enter with temperature and wavenumber recip microns}
	function dBdT (T, WN: real): real; {find temperature derivative of Planck}
	procedure FindRadiation; {driver}
	procedure ClearRadnArrays; {initialize}
	procedure GetWNs (abs: integer; maxNum: integer; WNMin, WNMax: real; var Num: integer); {set  up wave number table}
	procedure DoTriDiag (WN: real; WNindex: integer; abs: integer; numshells: integer; muZero: real);
{find matrix of coeffs for finite-difference equation of Feutrier method}
{and cast them into the Rybicki-Hummer form; then solve and find other functions}
	procedure UpDateTabulation (m, w: integer); {Find and store surface flux for wave number}
{ Add the monochromatic radiation to accumulating integrals for I+, I-, S, B, J}

{=======================================================}

implementation
{==============PROCEDURES FOR SOLVING HYDROSTATIC MODEL===============}


	procedure DoState1;{ (V, T: real; var P, Pe, xH, xHe, yHe, mu)}
		const
			DegpEv = 11604.9;
			ChiH = 13.595;
			ChiHe = 24.581;
			ChiHep = 54.403;
		var
			a, b, c: real;

		procedure goCold;
		begin
			xHe := 0;
			yHe := 0;

			xH := 0;
		end;
		procedure goLow;
		begin
			xHe := 0;
			yHe := 0;
			a := (1 / 3) * V * T * sqrt(T) * exp(-chiH * degpev / T) / gas;
			xH := 0.5 * a * (sqrt(1 + 4 * HAb / a) - 1) / HAb;
		end;

		procedure GoMedium;
		begin
			xH := 1;
			yHe := 0;
			b := (4 / 3) * V * T * sqrt(T) * exp(-chiHe * degpev / T) / gas;
			xHe := 2 * (HAb + b) * (sqrt(1 + b * HeAb / (HAb + b) / (HAb + b)) - 1) / HeAb;
		end;

		procedure GoHIgh;
		begin
			xH := 1;
			c := (1 / 3) * V * T * sqrt(T) * exp(-chiHep * degpev / T) / gas;
			a := HAb + HeAb / 4;
			b := HeAb / 4;
			yHe := 0.5 * (A + c) * (sqrt(1 + 4 * b * c / (a + c) / (a + c)) - 1) / b;
			xHe := 1 - yHe;
		end;

	begin {main DoState}
{FInd temp range}
		if t < 4000 then
			gocold
		else if t < 15000 then
			GoLow
		else if t > 30000 then
			goHigh
		else
			goMedium;

		mu := 1 / ((1 + xH) * HAb + (1 + xHe) * HeAb / 4 + yHe * HeAb / 2);
		P := gas * T / mu / V; {gas pressure}

{we do not add radiation pressure to find total pressure for optically thin atmosphere; }
{the effects are included as accleration counterbalancing gravity in hydrostatic eqn.}
		Pel := gas * T / V * (metab / 200 + xH * HAb + (xHe + 2 * YHe) * HeAb / 4);
	end;{DOState1}
{=======================================================}


	procedure FindV;{(P, T, Hab,HeAb, MetAb: real; var V,  mu, xH, xHe, yHe, Pe: real)}
		var
			loop: integer;
			pt, pg, mun: real;
	begin

		mu := 1.0;
		loop := 0;
		repeat {iterate on mu}
			loop := loop + 1;
			v := gas * t / p / mu;
			DOState1(v, t, HAb, HeAb, MetAb, pt, pe, xH, xHe, yHe, mun);
			mu := 0.25 * mu + 0.75 * mun;
		until ((abs(Pt / p - 1) < 0.1) or (loop > 100));
		if loop > 100 then
			begin
				building := false;
				beep;
			end;
	end;{findV}
{=======================================================}


{=======================================================}

	function OpChristy;
		var
			t6, t4, t5, op: real;
		function xey (x, y: real): real;
		begin
			xey := exp(ln(x) * y);
		end;

	begin
		t := T / 10000;
		t6 := xey(t, 6);
		t4 := xey(t, 4);
		t5 := xey(T, 5);
		op := 5.4e-13 * V / T + Hab * sqrt(T) / (2.0e06 / t4 + 2.1 * t6);
		op := op + HAb / (4.5 * t6 + (1 / t) * (1 / (4.0e-03 / t4 + 2.0e-04 * xey(v, 0.25))));
		op := op + Heab * ((1 / (1.4e03 * T + t6)) + 1.5 / (1.0e06 + 0.1 * t6));
		opchristy := op + MetAb * sqrt(T) / (20.0 * T + 5.0 * t4 + t5);
	end; {OpChristy}
{=======================================================}

	function OHMinusOP;

		function xey (x, y: real): real;
		begin
			xey := exp(ln(x) * y);
		end;
	begin
		if T < 10000 then
			OHMinusOP := HAb * (0.602e-06) * (1.0 - xH) / mu * 4.571e18 / xey(T, 3.7)
		else
			OHMInusOP := 0;
	end;{OHMinusOP}


{=======================================================}
	procedure FindKapp;{    ( V , T , xH , mu , Hab , HeAb , MetAb , Pel : real;var kappa : real );    }

		var
			c, hm: real;
	begin
		c := opChristy(V, T, Hab, HeAb, MetAb);
		hm := OHMinusOP(T, xH, Hab, mu);
		kappa := (c + hm) * Pel;
		if kapp < 0.01 then
			kappa := 0.01;
	end;{FindKapp}


{=======================================================}

{findTau (N: integer; Taubot: real;modSel:integer );}
{Set up geometric series between tautop and taubot and then inserts another level}
{as starting point, which is not plotted}
	procedure findTau;
		var
			i: integer;
			f: real;

	begin {findTau, main}
		with models[modsel] do
			begin
				f := exp(1 / (N - 1) * ln(taubot / tautop));
				shell[0].vari.put(tau, tautop / f); {this layer will be used to step to tautop}
				i := 0;
				repeat
					i := i + 1;
					shell[i].vari.put(tau, shell[i - 1].vari.value(tau) * f);
				until i = numshells;
			end;
	end; {findq}
{-------------------}


{Find pressure gradient: function dlnP/dtau = fdLnPdtau (t: real;y:garray): real;}
	function fdLnPdtau;
		var
			mu, xH, xHe, yHe, Pe, Kap, V, gradPr: real;
			f, P, TTemp: double;
	begin
		with models[modsel] do
			begin

				P := exp(y[1]);
				TTemp := exp(y[2]);
				findV(P, TTemp, Habund, HeAbund, MetAbund, V, mu, xH, xHe, yHe, Pe);
				Kap := Pe * (OPChristy(V, TTemp, Habund, HeAbund, MetAbund) + OHMinusOP(TTemp, xH, Habund, mu));
				if incscatt then
					kap := kap + Pe * Thomson / mu / mH / (P - Pe);
				if inclradpress then
					begin
						gradPr := PRPrime;
						f := (dyngrav / kap - gradPr) / p;
					end
				else
					f := (dyngrav / kap) / p;
				if f > 5000 then {avoid bad values}
					f := 5000;
				if f < 0 then
					begin {exit building}
						eddunstable := true;
						building := false;

					end;
				fdLnPdtau := f;
			end;
	end;
{=======================================================}

{procedure FAdGrad (T, beta, xH, XHe, yHe, Hab, HeAb: real;var AdGradT,Cp:real); returns }
{(gam2-1)/gam2 = dlnT/dlnP }
{this is stored as vari[gamm]; see Cox and Giuli, page 225}
	procedure FAdGrad;
		const
			degpev = 11604.9;
		var
			xsi, ybar, num, sum, denom, mu, Cv: real;
			ion: integer;
			nu, y, chi: array[1..3] of real;
	begin
		chi[1] := 13.95;
		chi[2] := 24.581;
		chi[3] := 54.403;
		nu[1] := 4 * Hab / (4 * Hab + Heab);
		nu[2] := Heab / (4 * Hab + Heab);
		nu[3] := nu[2];
		y[1] := xH;
		y[2] := xHe;
		y[3] := yHe;
		ion := 0;
		if t > 3000 then
			ion := 1;
		if T > 15000 then
			ion := 2;
		if t > 30000 then
			ion := 3;
		mu := 1 / ((1 + xH) * HAb + (1 + xHe) * HeAb / 4 + yHe * HeAb / 2);
		if ion > 0 then
			begin
{equation 9.165 inverted}
				ybar := y[1] * Hab + (y[2] + y[3] * 2) * HeAb / 4;
				Xsi := 2 * ybar * y[ion] * (1 - y[ion]) * nu[ion];
			     	Xsi := Xsi / (ybar * (1 + ybar) + y[ion] * (1 - y[ion]) * nu[ion]);
			      	denom := (4 - 3 * beta) * (8 - 3 * beta) + 12 * beta * (1 - beta);
                                denom:=denom+ beta * beta * xsi * sqr((4 * (1 - beta) / beta + (2.5 + chi[ion] * degpev / t)));
				sum := 4 * (1 - beta) / beta + 2.5 + chi[ion] * degpev / t;
				num := 2 * (4 - 3 * beta) + beta * xsi * sum;
				AdGradT := num / denom;
{now eq. 9.161}
				num := (8 - 7 * beta) / beta + (2 / 3 * Xsi) / (2 + Xsi) * sqr(3 / 2 + chi[ion] * degpev / t);
				Cv := (3 / 2) * gas / mu * (1 + ybar) * num;
				Cp := Cv + gas / mu;
			end
		else {gas is not ionized}
			begin
				AdGradT := 0.4;
				Cv := (3 / 2) * gas / mu;
				Cp := (5 / 2) * gas / mu;
			end;
	end;{FAdGrad}

{=======================================================}


	function FRadGrad (tauTemp, P, T, mu: real): real; {returns dlnT/dlnP from analytical solution, unless}
{model is being rebuilt from adjusted temperature}
		var
			num, den: real;
			y: garray;
			sht: integer;
{---------------------------}
		function findshell (mytau: real; m: integer): integer;
			var
				sh: integer;
		begin
			sh := 0;
			repeat
				sh := sh + 1
			until models[m].shell[sh].vari.value(tau) >= mytau;
			Findshell := sh;{return shell in which mytau occurs}
		end;
{----------------------------}
	begin {main: dlnT/dlnP= dlnTdtau /DlnPdtau}
		if ((not rebuildmodel) and (models[modsel].modtype = tconst)) then
			fradgrad := 0
		else
			begin
				y[1] := ln(P);
				y[2] := ln(T);
				den := fdLnPdtau(tauTemp, y); {dlnP/dtau}
				if not rebuildmodel then {take it from analytical expression}
					num := 1 / (4 * (tauTemp + mu)) {dlnT/dtau for gray RE case}
				else {use tabulated values loaded at start of building process}
					begin

						num := TabGradT[findShell(tauTemp, modsel)];
					end;
{now finiosh the calculation}
				fradgrad := num / den;
			end;
	end;{FRadGrad}
{===============================}
	procedure fMixingLength (grav, teff, mu, H, V, TTemp, kapp, agradT, rgradT, Cp: real; var gradT, MachNo: real);
		const
			alpha = 2; {ratio of mixing length to scale height, so alpha*H = mixing length}
		var
			tauE, ATemp, A, B, Q, X: real;
			R, Qp, RQRad, a1, a2, a3: real;
	begin
{find coefficients of the cubic equation M:6-294}
		tauE := kapp * alpha * H / V; {optical thickness of convecting element. Note alpha is declared }
{as a constant at the head of this procedure.}
		Q := 2.0;{approx for  1 - (dln mu/dlnT)}

{Cp := 5 / 2 * gas / mu; perfect non-ionizing gas}
		Atemp := alpha * Cp * TTemp / V * sqrt(Grav * H * Q);
		B := 16 * sqrt(2) * sigma * exp(4 * ln(TTemp)) * tauE / (ATemp * (1 + tauE * tauE / 2)); {M:6-287}
		A := alpha * ATemp * rgradT / (4 * sqrt(2) * sigma * exp(4 * ln(teff))); {M:6-292}
{Solve cubic according to Francois Viete 's equation for case when there is only a single real root }
{(see Numerical Recipes, Chapter 5, p 146)}
		a1 := 1 / A;
		a2 := B * a1;
		a3 := -a1 * (rgradT - agradT);
		R := (2 * a1 * a1 * a1 - 9 * a1 * a2 + 27 * a3) / 54;
		Qp := (a1 * a1 - 3 * a2) / 9;
		if (R * R - Qp * Qp * Qp) > 0 then {single real root}
			begin
				RQRad := exp(ln(sqrt(R * R - Qp * Qp * Qp) + abs(R)) / 3);
				x := -R / abs(R) * (RQRad + Qp / RQRad) - (a1) / 3;{NumRec: eq. 5.5.10}
				gradT := agradT + B * x + x * x; {find actual gradient M: 6-295}
				MachNo := alpha / (2 * sqrt(2)) * sqrt(Q) * x; {vbar/Sqrt(gH) = isothermal Mach no; M:6-280}
			end
		else
			begin
				gradT := rgradt;
				machno := 0;
			end;
	end;
{===============================}
{procedure BuildNewLayer (s, P, T,H); Driver for RK4 integration of hydrostatic eqn.}
{Based on Numerical recipes p 777-8}

	procedure BuildNewLayer; { (s: integer; var P, T, H: real)}

		var
			y, dydx, yout: garray;

{------------------------------------------}

		procedure findGradT (t: real; y: garray; var gradT: real);
{Evaluates gradT = dlnT / dlnP from mixing length theory described by Mihalas in }
{Stellar Atmospheres 1970, chapter 6. Equation references are given }
			var
				agradT, Cp, rgradT, TTemp, H, MachNo: real; {dlnT/dlnP for isentropic and radiative zone}
				V, mu, xH, xHe, YHe, Pe, kapp: real;
		begin
			with models[modsel] do
				begin

					if (modtype = TConst) then
						gradT := 0
					else
{Assume radiative for the moment.}
						gradT := FRadgrad(t, exp(y[1]), exp(y[2]), models[modsel].muzero);
{This value is retained if the atmosphere is radiative or }
{if the region is stable against convection.}

{If model includes convection check for convective instability }
					if ((modtype = incCon) and (not rebuildmodel)) then
						begin
							findV(exp(y[1]), exp(y[2]), Habund, Heabund, Metabund, V, mu, xH, xHe, YHe, Pe);
							FAdGrad(exp(y[2]), 1, xH, xHe, yHe, HAbund, HeAbund, AgradT, Cp); {adiabatic gradient}
{compare with radiative}
							if agradT < gradT then
								begin{mixing length calculation}

									rgradT := gradT;{store the radiative gradient}
									TTemp := exp(y[2]); {local temperature}
									findKapp(V, TTemp, xH, mu, HAbund, HeAbund, MetAbund, pe, kapp); {find opacity}
									H := Gas * TTemp / mu / dyngrav; {scale height}

									fMixingLength(dyngrav, teff, mu, H, V, TTemp, kapp, agradT, rgradT, Cp, gradT, MachNo);

								end;
						end;

				end; {with models[modsel]}
		end; {FindGradT}


{------------------------------------------}
{find dH/dtau = -1/kappa*rho}

		function findGradH (y: garray): real;
			var
				T, V, mu, xH, xHe, YHe, Pe, kapp, dens: real;
		begin
			with models[modsel] do
				begin
					T := exp(y[2]);
					findV(exp(y[1]), T, Habund, Heabund, Metabund, V, mu, xH, xHe, YHe, Pe);
					findKapp(v, T, xH, mu, Habund, Heabund, Metabund, pe, kapp);
					findGradH := -v / kapp;
				end;{with}
		end;

{------------------------------------------}

{dydx[1]=g/kap*P; dydx[2]=dlnT/dtau=dydx[1]*gradT; dydx[3]=-1/kappa*rho}
{where gradT=dlnT/dlnP, and is computed from smaller of radiative and }
{adiabatic gradients}
		procedure derivs (t: real; y: garray; var dydx: garray);
			var
				gradT, gradH: real;
		begin
			if not building then
				exit;{*(derivs);}
			dydx[1] := fdLnPdtau(t, y);{dlnP/dtau}
         If not rebuildModel then
         begin
			    findGradT(t, y, gradT); {dlnT/dlnP}
             dydx[2] := dydx[1] * gradT;{dlnT/dtau = dlnT/dlnP * dLnP/dtau}
         end
         else
             dydx[2]:=models[modsel].shell[s].vari.value(tgrad);   {take numerical value}
			dydx[3] := FindgradH(y);{dH/dtau}
		end;

{-----------------------------}

{Integrate hydrostatic equilibrium between tau points; returns lnP = y[1] and LnT = y[2]}
{tau is the independent variable, x}
		procedure RK4FindY (y, dydx: garray; ti, tf: real; var yout: garray);
			var
				h, xh, hh, h6: real;
				dym, dyt, yt: garray;
				i: integer;

		begin
			h := tf - ti; {step of independent var}
			hh := h * 0.5;
			h6 := h / 6;
			xh := ti + hh;

			for i := 1 to 3 do
				yt[i] := y[i] + hh * dydx[i];

			derivs(xh, yt, dyt);
			for i := 1 to 3 do
				yt[i] := y[i] + hh * dyt[i];

			derivs(xh, yt, dym);
			for i := 1 to 3 do
				begin
					yt[i] := y[i] + h * dym[i];
					dym[i] := dyt[i] + dym[i];
				end;

			derivs(h + ti, yt, dyt);
			for i := 1 to 3 do
				yout[i] := y[i] + h6 * (dydx[i] + dyt[i] + 2 * dym[i]);
		end;

{-----------------------------}


	begin {main Build new layer}
		with models[modsel] do
			begin
				y[1] := ln(shell[s].vari.value(pres));
				y[2] := ln(shell[s].vari.value(temp));
				y[3] := shell[s].vari.value(height);

				if not building then
					exit;{*(buildnewlayer);}{get out}

				derivs(shell[s].vari.value(tau), y, dydx);{starting value}
				if not building then
					exit;{*(buildnewlayer);}{get out}
				RK4FindY(y, dydx, shell[s].vari.value(tau), shell[s + 1].vari.value(tau), yout);

				P := exp(yout[1]); {gas pressure}
            T := exp(yout[2]); {temperature}
            {if s=12 then t :=t*1.3;}{example of temperature discontinuity}
				h := yout[3]; {geometric height}
				if ((not rebuildmodel) and (modtype = tconst)) then
					T := teff;
			end;
	end; {build new layer}

{============================================}
{get first point in atmosphere by approximation to hydrostatic equation}
	procedure GetStart (Tau, Te, G, muZ, Hab, HeAb, metab: real; var P0, T0: real);
		var
			kzero, mmwt, tefourth, foverH, GradPr, tot, int, dint, wn: real;
	begin
		with models[modsel] do
			begin

{find surface temperature for radiative model}
				if not (modtype = TConst) then
					begin {compute gray surface temp if not an isothermal model }
						tefourth := Te * Te * Te * Te;
						T0 := Te * sqrt(sqrt(tau + muZ)) / sqrt(2 * muZ); { surface temp for gray radiative  atmosphere}
					end;
{treatment of isothermal model is different}
				if modtype = tConst then
					if (not rebuildmodel) then
						T0 := Te
					else { it is being rebuilt}
						T0 := models[modsel].shell[0].vari.value(temp);

{now find pressure}
				if t0 > 15000 then
					begin
						mmwt := (1 + 4 * HeAb) / (2 + HeAb);{assumes Np=Ne; He neutral}
						if inclradpress then
							begin
								gradPr := PRPrime; {computed for stellar flux}
								P0 := tau * (g * mmwt * mh / thomson - gradPr);
								if P0 < 0 then
									begin
										eddunstable := true;
										building := false;
									end
							end
						else
							P0 := tau * (g * mmwt * mh / thomson);
					end

				else if T0 < 5000 then  {cold gas, H- opacity}
					begin
						mmwt := (1 + 4 * Heab) / (1 + Heab); {assumes H, He neutral}
						KZero := OHMinusOP(T0, 0, Hab, mmwt);
						P0 := sqrt(2 * 1e04 * g * tau / kzero);
					end
				else{intermediate, constant opacity}
					begin
						mmwt := (1 + 4 * Heab) / (1 + Heab); {assumes H, He neutral}
						KZero := 0.1;
						P0 := g * tau / kzero;
					end;
			end;{with models}
	end;


{=======================================================}
{ LoadLayer (P,T,H:real;s: integer); put variables into data array for layer}
	procedure LoadLayer;{(P, T, H: real; s: integer)}
		var
			Cp: real;
                        a,b,c,d,e,f,g,hh,i,j,k,m,n,pp:real;
	begin
	
		        a:= models[modsel].Habund ;
              b:= models[modsel].HeAbund ;
              c:= models[modsel].MetAbund ;

              with models[modsel].shell[s] do
              begin
                        vari.put(pres, P);
			               vari.put(temp, T);
			               vari.put(height, H);
                        j:=vari.value(temp);
                        pp:= vari.value(pres);
                        n:= vari.value(tau);

                       findV(p,j,a,b,c,d,e,f,g,hh,i);
		                 vari.put(volu,d)  ;
                       vari.put(mmwt,e) ;
                       vari.put(Hion,f) ;
                       vari.put(Heion,g) ;
                       vari.put(HepIon,hh) ;
                       vari.put(Pe,i);
                       vari.put(dens, 1 / d);

                        models[modsel].shell[s].vari.put(radgrad,fradgrad(n,pp,j,models[modsel].muzero));
            
                        FAdGrad(j,1,f,g,hh,a,b,k,Cp);
	                     vari.put(adgrad,k) ;

                        findKapp(d,j,f,e,a,b,c,i,m);
		                  vari.put(kapp,m);
              end; {with shell}

	end;

{===========================================}
	procedure LoadTabGradT (m: integer); {constant slope of lnT between points}
{assigned to the deeper surface of the shell}
		var
			s: integer;
			LT1, LT2, Tau1, Tau2: real;
	begin
		with models[m] do
			for s := 1 to numshells do
				begin
					LT1 := ln(shell[s - 1].vari.value(temp));
					LT2 := ln(shell[s].vari.value(temp));

					tau1 := shell[s - 1].vari.value(tau);
					tau2 := shell[s].vari.value(tau);
					TabGradT[s] := (LT2 - LT1) / (Tau2 - Tau1);
               shell[s-1].vari.put(tgrad,tabGradT[s]);    {note shift}
				end;
	end;


  {=======================================================}

	procedure CallBuilder;
		var
			P, T, HT: real;
	begin
             with models[modsel] do  begin
                repeat
		          BuildNewLayer(BuildIndex, P, T, Ht);
		          if building then
			          begin
				       LoadLayer(P, T, Ht, BuildIndex+1);
				       BuildIndex := BuildIndex + 1;
			          if BuildIndex >= Numshells then
						    gasBuilt := true;
			          end;
                until  (gasbuilt or (buildindex>numshells))
              end; {with models}
	end;

{=======================================================}

Procedure ClearOldModel(m:integer);
var i,j:integer;
begin
	with models[m] do
			begin {clear out the arrays}
				for i := 1 to numshells do
					for j := 1 to 18 do
						shell[i].vari.put(j,0);
            gasbuilt:=false;
            radfound:=false;
			end;
end; {clearOldModel]

{===========DRIVER/BUILD==================}
{Here is the driver for this unit; it is called by the menu}
	procedure buildmodel;
		var
			i, s: integer;
                        a,b,c,d,e,f,g,hh:real;
			P0, T0, P, T, Ht, H: real;
			V, mu, xH, xHe, YHe, Pe, AgradT, Cp: real;
			theItem: integer;
			MyType: integer;
			tempstr3: string;
	begin
		with models[modsel] do
			begin
				building := true; {initialize; program sets building:=false when integration fails or stop button hit}
				eddunstable := false;
				if not ReBuildModel then
					begin
					   clearOldModel(modsel); {clear arrays in the oldmodel}
						Teff := Lum / Rad / Rad;
						teff := 5770 * exp(ln(teff) / 4);
						FindTau(numshells, Tautop, TauBot, modsel); {set up  tau scale}
					end   {if not rebuildModel}
				else {rebuilding, so use surface flux to find new Teff еее fix this}
					begin
{Teff := models[modsel].shell[1].radn.value(mean, flux) / solarFlux;*}
						teff := (esUpperwn - eslowerwn) * fourpi * models[modsel].shell[1].radn.value(mean, flux) / sigma; {ее}
						teff := exp(ln(teff) / 4);
{ratio of surface fluxes}
{Teff := Lum / Rad / Rad;}
{teff := 5770 * exp(ln(teff) / 4); }
{Stefan's law}
						LoadTabGradT(modsel); {load numerical gradient from adjusted temperatures}
                  gasbuilt:=false;
					end; { if rebuilding}

				dyngrav := 27401 * Mass / Rad / Rad;
				GetStart(shell[0].vari.value(tau), Teff, dynGrav, muZero, HAbund, HeAbund, metabund, P0, T0);
{find P0 and T0 of top layer by analytical approximation}
				PRPrime := sigma / lightspeed * Teff * Teff * Teff * Teff; {radiation pressure gradient, assumed}
{constant; used in function fdLnPdtau}

				if building then
					begin
						LoadLayer(P0, T0, 0, 0);
 					   BuildNewLayer(0, P, T, Ht);
						LoadLayer(P, T, 0, 1);
						BuildIndex := 1;
					end;

				if not building then
					begin
						beep;
						beep;
                  models[modsel].gasbuilt:=false;
						exit;{*(buildmodel);  }
					end;

            CallBuilder;   {step through the other layers}

				if (gasBuilt and (not ReBuildmodel)) then
					for s := 1 to models[modsel].numshells do {find temp gradient}
						with models[modsel].shell[s] do
							begin
								vari.put(tgrad, vari.value(radgrad));
								vari.put(conflux, 0);
								if ((vari.value(radgrad) >= vari.value(adgrad)) and (models[modsel].modtype = incCon)) then
                                 {evaluate mixing length gradient where unstable}
									begin
										findV(vari.value(pres), vari.value(temp), Habund, Heabund, Metabund, V, mu, xH, xHe, YHe, Pe);
{need Cp and H to re compute mixing length gradient for tabulation}
					FAdGrad(vari.value(temp), 1, xH, xHe, yHe, HAbund, HeAbund, AgradT, Cp);
                                        H := gas * vari.value(temp) / vari.value(mmwt) / models[modsel].dyngrav; {scale height}
                                        a:=vari.value(mmwt);
                                        b:=vari.value(volu);
                                        c:=vari.value(temp);
                                        d:=vari.value(kapp);
                                        e:=vari.value(adgrad);
                                        f:=vari.value(radgrad);
                                        fMixingLength(models[modsel].dyngrav, models[modsel].teff,a,H,b,c,d,e,f,Cp,g,hh);
                                        vari.put(tgrad,g);
                                        vari.put(MachNo,hh);
                                     
                                        vari.put(conflux, 1 - vari.value(tgrad) / vari.value(radgrad));
                                         {Convective flux/total flux Mihalas: eq. 6-290}
                                      end;
                                 tempArray[modsel,s]:=vari.value(temp); {for tAdjust}
		              end;

				if gasBuilt then {update status indicators and enable TAdjust item}
					case modsel of
						1: 
							begin
								{*enableItem(Menu_model_1, MItem_Adjust_Temperat);}
							end;
						2: 
							begin
							      {*	enableItem(Menu_model_2, MItem_Adjust_Temperat2); }
							end;
						else
					end;
			end;
{updateRadios(models[1].gasBuilt, models[2].gasBuilt);}
		eddunstable := false;
	end; {buildmodel}

{==========================================================}
{============PROCEDURES FOR RADIATION FIELD ===================}


	function BB;{ (T, WN: real) Evaluate Black Body function}
		const
			c1 = 1.191e11;
			c2 = 14388; {micron deg}
		var
			bbt: real;
	begin
		if wn = 0 then
			bbt := 0
		else
			bbt := c1 * wn * wn * wn / (exp(c2 * wn / t) - 1);{erg cm-2 sec-1 micron}
		bb := bbt;
	end;
{=========================}
	function dBdT;{ (T, WN: real) temperature derivative of Black Body function}
		const

			c2 = 14388; {micron deg}
		var
			bbt: real;
	begin
		if wn = 0 then
			bbt := 0
		else
			bbt := BB(T, WN) * c2 * wn / T / T / (1 - exp(-c2 * wn / t));
		dBdT := bbt;
	end;
{=========================}

	procedure ClearRadnArrays;
		var
			i, s: integer;
	begin
		for s := 1 to maxshells do
			for i := Iup to Planck do
				models[modsel].shell[s].radn.put(mean, i, 0);{Set all sums to zero}
	end;
{=========================}

	procedure GetWNs; {(abs:integer;maxNum: integer; WNMin, WNMax: real; var Num: integer)}
{Between wavenumbers corresponding to upper and lower wavelength on }
{Spectrum window, find a uniform set of wave numbers for integration. Their number }
{will not affect storage requirements, as we work one at a time and accumulate results in integral}
{Sequenced numerically. Add WNs at steps in opacity to plot sharply}
var
			n, NumPts, m,q: integer;
			dWNC, dWNL, f: real;
	begin
     numPts:=maxNum;
		if ((WNmax - WNMin) > 0.01) then
			num := NumPts
		else
			num := 1;

		if Num > 1 then
			dWNC := (WNmax - WNMin) / (Num - 1){waveNumber interval}
		else
			dWNC := 0;
      {set up standard list, for both models}
      for m := 1 to 3 do     {m = 3 is for BB curve}
          for n := 1 to num do {find wavenumbers and load into global list}
			      WNList[m,n] := WNMin + (n - 1) * dWNC;
      {now modify standard list if necessary}
      for m:=1 to 2 do
      begin
            case models[m].absorber of
            StepF:  begin  {replace two interior values with WStep}
                    with models[modsel] do
                    for n:=1 to num-1 do
                      begin  {find interval containing wStep}
                      if ((WNList[m,n]<=wstep) and (WNList[m,n+1]>wstep)) then
                         begin
                          wNList[m,n]:=Wstep-0.01;
                          WNList[m,n+1]:=Wstep+0.01;
                         end;
                      end; {for n}
                    end;
            Hyd: Begin

                   for q:=2 to 5 do {Now put some points at Hydrogen absorption edges}
                       begin
                         for n:=1 to num-1 do   {step through spectrum points}
                         if ((WNList[m,n]<=10.97/q/q) and (WNList[m,n+1]>10.97/q/q)) then
                         begin
                          wNList[m,n]:=10.97/q/q-0.01;  {location of absorption edge}
                          WNList[m,n+1]:=10.97/q/q+0.01;
                         end;

                       end;{for n}
                   end;
       end;{case}
       end;{for m}
	end;
{=========================}
 function FindChi(m:integer;wIndex:integer;s:integer):real;    {normalize to wn0}
 var a,c:real;
 begin
 a:=models[m].shell[s].opacity.value(wIndex);
 findOpacity(m,s,WN0,models[m].absorber,c); {standard}
 findChi:=a/c;
 end;

 {========================================}
  function FindEps(m:integer;wIndex:integer;s:integer):real;    {normalize to wn0}
 var a,c:real;
 begin
 a:=models[m].shell[s].opacity.value(wIndex);  {total}
                                                      {scattering}
 findEps:=c/a;
 end;

 {========================================}
 Procedure findOpacity(m,S:integer;w:real;absorber:integer;var kappa:real);
 var opaciH,opaciHminus:real;
 {-----------------------------}
 function findnstar(f:real):integer;  {this is the lower limit of shell sum}
          var n:integer;
          begin
          n:=trunc(sqrt(10.97/f))+1;
          findNStar:=n;
          end; {find NStar}
 {-------------------------------}
 procedure FindaH(T,w:real;var aH:real);   {finds Hyd absorption coeff}
   {fraction neutral H, gms per gm Hyd, freq, temperature, aH eqn. 4-227, Mihalas}
           const
                ntop=10;
           var sum,factor,freq,u1,term:real;
               n,nlower:integer;
           begin
                 freq:= 2.99e10*w*1e4;  {?}
                 sum:=0;
                 u1:=157872/T;    { un = u1/n/n}
                 if w>0 then
                 begin
                  nlower:=findnstar(w);
                  if nlower>ntop then nlower:=ntop-1;
                  for n :=nlower to ntop do
                  begin
                       term:=exp(u1/n/n)/n/n/n ;
                       sum:=sum+term;
                  end;{for  n}
                  {add ff}
                  sum:=sum+exp(u1/(ntop*ntop))/(2*u1);

                  factor:=2.815e29/freq/freq/freq*exp(-u1);
                  aH:=sum*factor;
                end;
    end; {findaH}
 {-------------------------------}
 procedure findaHminus(t,w:real; var ahm:real);
           var
              ahmfb,theta,thetaSq,lam,clam,k:real;
           begin
           theta:=5040/t;
           thetaSq:=theta*theta;
           lam:=1/w*1e4;{angstroms; w is reciprocal microns: w=1 is 10,000A}
           {w*1e4 is reciprocal cm.}
           {freq/(w*1e4)=c, so freq= c*w*1e4}
            {Gingerich, Proc. First Smithsonian Conf. on Stellar Atmos.}
           {free-free}
           ahm:=0.0053666-0.011493*theta+0.027039*thetaSq;
           ahm:=ahm+(lam/1e6)*(-3.2062+11.924*theta-5.939*thetaSq);
           ahm:=ahm+(lam*lam/1e9)*(-0.40192+7.0355*theta-0.34592*thetaSq);
           {free-bound, only exists for lam<16419A}
           ahmfb:=0; {initialize}
           If lam<16419 then
              begin    {two different expressions}
                       if lam>14200 then
                          begin
                          clam:=(16419-lam)/1000;
                          k:=0.269818+clam*clam*(0.22019+clam*(-0.0411288+0.00273236*clam));
                          end
                       else   {lam<14200}
                          begin
                          clam:=lam/1000;
                          k:=0.00680133+clam*(0.178708+clam*(0.164790+clam*(-0.0204842+clam*(5.95244e-04))));
                          end;
                ahmfb:=k*0.4158*theta*theta*sqrt(theta)*exp(theta*1.726)*(1-exp(-theta*2.846*w));
              {hc/kB=9.5210e-15*2.99e10= 2.846e-4; note factor 1e4 of w is absorbed }
              end; {if lam<16419}

              ahm:=(ahm+ahmfb)*1e-26;
           end;{findaHminus}
 {------------------------------------}
 begin  {main, findOpacity}
      case absorber of
      Gray: kappa:=models[m].shell[s].vari.value(kapp);
      StepF: begin   {Artificial case; This is the place to specify the step ratio}
                  if W>models[modsel].Wstep then
                   kappa:=models[m].stepFact*models[m].shell[s].vari.value(kapp)
                   else
                   kappa:=models[m].shell[s].vari.value(kapp);
             end;
      Hyd:   begin
      {defining relationship:
        kappa = (1/(1+4B)/mH)(nH a(H) + nH Pe a(H-))}  {nH is number of neutral hydrogen atoms per g}
             with models[modsel].shell[s] do
                 begin
                 {This is the place to play around with contributions to the opacity}
                      findaH(vari.value(temp),W,opaciH);
                      {opaciH:=0; {insert this to remove H opacity}
                      findaHminus(vari.value(temp),W,opaciHminus);
                      {opaciHminus:=0; {insert this to remove H- opacity}
                      kappa:=(1-vari.value(Hion))/vari.value(mmwt)/1.66e-24*(opaciH  + vari.value(pe)*opaciHminus);
                      {kappa:=0;} {...insert this to get effect of electron scattering only}
                      {Electron scattering: sigma :=0.66524e-24 cm2 per electron. }
                           {per gram, k:=sigma Pe/mu/mH/(Pg-Pe); mihalas p. 125-126}
                      kappa:=kappa+ 0.66524e-24/1.66e-24*vari.value(pe)/(vari.value(pres)-vari.value(pe))/vari.value(mmwt);

                 end;
             end;
      end; {case}

 end;{FindOpacity}
{==============================}

	procedure DoTriDiag; {(WN: real; WNindex:integer;abs: integer; numshells: integer; muZero: real);}
{calculate Chi, Eps, Bnu at our frequency for each tau adn combine to A,B, C; }
{Recast them into R-H form}
		var
			chi, eps, capDelta, delta: array[1..maxShells] of real;
			dtau: array[0..maxShells] of real; {use 0 for outermost}
			i,n: integer;
			tfact, tgrad: real;
	                a:real;     {dummy}

     begin
{first compute Planck function at the frequency}
		for i := 1 to numshells do
			models[modsel].shell[i].radn.put(monoU, Planck, bb(models[modsel].shell[i].vari.value(temp), WN));

		for i := 1 to numshells do
			begin
				chi[i] := FindChi(modsel,WNindex,i);
				if (models[modsel].absorber = gray) then
               eps[i] := models[modsel].GrEps
               else if  (models[modsel].absorber = stepF) then
                    begin
                    if wnlist[modsel,wnIndex]>models[modsel].WStep
                       then eps[i]:= models[modsel].GrEps/models[modsel].stepfact;
                    end
               else if (models[modsel].absorber = hyd) then
               eps[i]:=FindEps(modsel,WNindex,i);
			end;


{compute deltas}
		for i := 1 to numshells - 1 do
			begin
				dtau[i] := models[modsel].shell[i + 1].vari.value(tau) - models[modsel].shell[i].vari.value(tau);
				capDelta[i] := (chi[i] + chi[i + 1]) * dtau[i] / (2 * models[modsel].muzero);
			end;
		dtau[0] := models[modsel].shell[1].vari.value(tau);
		for i := 2 to numshells - 1 do
			Delta[i] := (capdelta[i] + capdelta[i - 1]) / 2;

{compute the surface coefficients}
		tfact := eps[1] * chi[1] * chi[1] * dtau[1] * dtau[1] / (2 * muzero * muzero);
		AFeut[1] := 0;
{CFeut[1] := 1 / capdelta[1];}
		CFeut[1] := 1;
		BFeut[1] := 1 + chi[1] * dtau[1] / muzero + tfact;
		DFeut[1] := CFEut[1] / BFeut[1];
		GFeut[1] := tfact * models[modsel].shell[1].radn.value(monoU, Planck);
		VFeut[1] := eps[1];
		UFeut[1] := VFeut[1] / CFeut[1];
		ZFeut[1] := GFeut[1] / BFeut[1];
{now use recursion relations to find the remaining coefficients}
		for i := 2 to Numshells - 1 do
			begin

				CFeut[i] := 1 / delta[i] / capdelta[i];
				AFeut[i] := 1 / delta[i] / capdelta[i - 1];
				BFeut[i] := eps[i] + AFeut[i] + CFeut[i];
				DFeut[i] := cFEut[i] / (BFeut[i] - AFeut[i] * Dfeut[i - 1]);
				GFeut[i] := eps[i] * models[modsel].shell[i].radn.value(monoU, Planck); {+ incident radiation}
				VFeut[i] := eps[i];
				UFeut[i] := (VFeut[i] + AFeut[i] * UFeut[i - 1] / (1 + UFeut[i - 1])) / CFeut[i];
{ZFeut[i] := (GFeut[i] + AFeut[i] * ZFeut[i - 1]) / (CFeut[i] * (1 + UFeut[i]));}
				ZFeut[i] := (GFeut[i] + AFeut[i] * ZFeut[i - 1]) / (BFeut[i] - AFeut[i] * Dfeut[i - 1]);
			end;
{at bottom}
		i := numshells;
		AFeut[i] := 1 / capdelta[i - 1];
		BFeut[i] := 1 + AFeut[i];
		Tgrad := models[modsel].shell[i].vari.value(temp) - models[modsel].shell[i - 1].vari.value(temp);
		TGrad := TGrad / capdelta[i - 1]; {this includes the necessary chi}
		GFeut[i] := models[modsel].shell[i].radn.value(monoU, Planck)
                + 2 * dBdT(models[modsel].shell[i].vari.value(temp), WN) * TGrad;
		ZFeut[i] := (GFeut[i] + AFeut[i] * ZFeut[i - 1]) / (BFeut[i] - AFeut[i] * Dfeut[i - 1]);

{Solve; assume mean intensity = bb intensity at bottom of atmosphere}
		models[modsel].shell[numshells].radn.put(monoU, Jay, ZFeut[numshells]);

		with models[modsel] do
			begin
				for i := numshells - 1 downto 1 do
					shell[i].radn.put(monoU, Jay, shell[i + 1].radn.value(monoU, Jay) * Dfeut[i] + ZFeut[i]);
{shell[i].radn.value(monoU, Jay) := shell[i + 1].radn.value(monoU, Jay) / (1 + UFeut[i]) + ZFeut[i];}

{Evaluate remaining functions:}
{at  surface}
				shell[1].radn.put(monoU, flux, shell[1].radn.value(monoU, jay)* muzero);
				Shell[1].radn.put(monoU, Iup, shell[1].radn.value(monoU, jay) * 2);
				Shell[1].radn.put(monoU, IDn, 0);
				Shell[1].radn.put(monoU, SFn, eps[1] * shell[1].radn.value(monoU, Planck)
                  + (1 - eps[1]) * shell[1].radn.value(monoU, Jay));

{interior}
				for i := 2 to numshells - 1 do
					begin
                a:=(shell[i + 1].radn.value(monoU, Jay) - shell[i].radn.value(monoU, Jay)) / capDelta[i];
                a:= a + (shell[i].radn.value(monoU, Jay) - shell[i - 1].radn.value(monoU, Jay)) / capDelta[i - 1];
                a:= a*muzero/2;
                shell[i].radn.put(monoU, flux, a);
					 shell[i].radn.put(monoU, Iup,shell[i].radn.value(monoU, Jay) + shell[i].radn.value(monoU, Flux) / muzero);
					 shell[i].radn.put(monoU, IDn, shell[i].radn.value(monoU, Jay) - shell[i].radn.value(monoU, Flux) / muzero);
   	          a := eps[i] * shell[i].radn.value(monoU, Planck);
                a:= a + (1 - eps[i]) * shell[i].radn.value(monoU, Jay); {е + incident}
                shell[i].radn.put(monoU, SFn,a);

					end;
{bottom}      n:=numshells; {dummy}
               shell[n].radn.put(monoU, flux, shell[n - 1].radn.value(monoU, flux)); {е temporary approximation}
				   shell[numshells].radn.put(monoU, Iup,shell[numshells].radn.value(monoU, Jay)
                  + shell[numshells].radn.value(monoU, Flux) / muzero);
				   shell[numshells].radn.put(monoU, IDn, shell[numshells].radn.value(monoU, Jay)
                  - shell[numshells].radn.value(monoU, Flux) / muzero);
               a := eps[n] * shell[n].radn.value(monoU, Planck); {a=dummy}
               a := a+(1 - eps[n]) * shell[n].radn.value(monoU, Jay);{е + incident}
               shell[n].radn.put(monoU, SFn, a);

			        end; {with models[modsel]}

	end;


{=========================}
	procedure UpDateTabulation;{m,w:integer; w is  the wavenumber index}
		var
			i, kk: integer;
			dWNC, dWNL: real;
                        a:real;
 {---------------------------------------------}
		procedure ShiftAll;
			var
				st, kt: integer;
		begin
			with models[modsel] do
				for st := 1 to numshells do
					for kt := IUp to Planck do
						shell[st].radn.put(monoL, kt, shell[st].radn.value(monoU, kt));
		end;
{-----------------------------------------------}
	begin {main UpDateTabulation}
		with models[m] do
			begin {store current monochromatic beam flux}
				emflux[w] := shell[1].radn.value(monoU, flux);


				for i := 1 to models[modsel].numshells do
					if (w = 1) and (numfreqs = 1) then {if there is only one spectrum point, load mean for plotting, after}
{multiplying by WN interval}
						for kk := IUp to Planck do
							shell[i].radn.put(mean, kk, shell[i].radn.value(monoU, kk) * (ESUpperWN - ESLowerWN) )
					else if w > 1 then
						begin
							for kk := IUp to Planck do {multiply avg mono by wavenumber interval and add to integral (mean)}
                             BEGIN
a := (shell[i].radn.value(monoU, kk) + shell[i].radn.value(monoL, kk)) / 2 * (WNList[modsel,w] - WNLIst[modsel,w - 1]);
  shell[i].radn.put(mean,kk, a + shell[i].radn.value(mean, kk));
                             END;{FOR}
						end;   {ELSE}

				shiftall;{replace old monoL}
			end; {with models}
	end; {upDateTabulation}

{==========MODAL DIALOG HANDLER==============}
	procedure CallRadiationFInder;
   var
      shellIndex:integer;
      op:real;
	begin

		with models[modsel] do
			begin        {find all monochromatic opacities}
             for shellindex:=1 to numshells do     {run through shells}
                 for  BuildIndex := 1 to numfreqs do    {run through frequencies}
                    begin
                      findOpacity(modsel,Shellindex,WnList[modsel,buildindex],absorber,op);

{ Procedure findOpacity(m,S:integer;w:real;absorber:integer;var opaci:real)}
                      shell[shellIndex].opacity.put(buildindex,op);
                    end;
            buildIndex:=1;
                repeat   {Solve transfer for each frequency}
				DoTriDiag(WNList[modsel,BuildIndex], Buildindex, absorber, numshells, muzero);
				UpDateTabulation(modsel, BuildIndex);
				buildIndex:=buildIndex+1;
                until buildindex>numfreqs;
            models[modsel].radfound:=true;
			end;
	    
	end;

{===========================}

	function FIndUpperWNIndex (TWN2: real): integer;
		var
			w: integer;
	begin
		w := 0;
		repeat
			w := w + 1;
		until WNList[modsel,w] > TWN2;
		FindUpperWNIndex := w;
	end;

{===================================}
	function FindFluxAtBand (TWN: real; model: integer): real;
{returns emitted flux for specified WN and model}
		var
			WTU, WTL: real;
			Tindex,dummy: integer;
	begin

		Tindex := FindUpperWNIndex(TWN);
		WTU := abs(1 / (WNList[model,Tindex] - TWN));{interpolation weights, inversely proportional to distance from end of band}
		WTL := abs(1 / (WNList[model,Tindex - 1] - TWN));
		FindFluxAtBand := models[model].emflux[TIndex] * WTU + models[model].emflux[TIndex - 1] * WTL;
                 dummy:=tindex;
	end;




{=========DRIVER FOR RADIATION========}
	procedure findradiation;  {call from menu}
		var

		      {	DialogP: DialogPtr; }
			theItem: integer;
		      {	myRect: rect;        }
		       {	MyHandle: handle; }
			MyType: integer;
			i, kk: integer;
			delWN: real;
                        a,b:real; {dummy}

	begin
		if models[modsel].gasbuilt = true then
			begin
				ClearRadnArrays;
				GetWNs(models[modsel].absorber, maxNumfreqs, ESLowerWN, ESUpperWN, numfreqs);
            models[modsel].radfound := false;
            callRadiationFinder;
{Now find  the photometeric parameters}
						with models[modsel] do
							if ((ESUpperWN > passband[highband].UpperWN) and (ESLowerWN < passband[lowband].lowerWN)) then
								begin
                            a:= Passband[highband].centerWN ;
                            b:= Passband[lowband].centerWN;
{find fluxes and convert to per unit Wave length to correspond to convention; lns  are  converted to base 10}
B_V := -2.5 * 0.4343 * ln(FindFluxAtBand(a, modsel) / FIndFluxAtBand(b, modsel)) - 5 * 0.4343 * ln((b) / a) + B_Vconst;
									MVIS := -2.5 * 0.4343 * ln(rad * rad * FindFluxAtBand(Passband[lowband].centerWN, modsel)) + MVISconst;
								   MBOL := -2.5 * 0.4343 * ln(rad * rad * shell[1].radn.value(mean, flux)) + MBOLconst;
{use surface value of flux integrated between spectrum limits}
									MV_App := MVIS + 5 * 0.4343 * ln(Ly_Distance) - 7.56;
								end
							else {spectrum did not bracket the photometric region, so show null values}
								begin
									B_V := 99;
									MBOL := 99;
									MVIS := 99;
								end;


			end;{if dialogP<>nil}



{renormalize means}
		delWN := ESUpperWN - ESLowerWN;
		with models[modsel] do
			for i := 1 to numshells do
				for kk := IUp to Planck do
					shell[i].radn.put(mean, kk, shell[i].radn.value(mean, kk) / delWN);

	end;

end. {Unit physics_atmos}

 END.