unit PBuild;

 {-------builds static model------}

 {---STELLAR PULSATION: C. A. WHITNEY------}
 {----------Modified Aug 31, 1994-------}

interface

	uses
		crt, dos, graph, printer, CUPS, cupsMUPP, CUPSfunc, CupsProc, cupsgui, CUPSGrph, PCommon;

 {------calls----}

	function frand (min, max: real): real; {returns randomly selected real number in range min-max}
	procedure FindRadius (Mymass: extended; Mydens, Router: real; var rinner: real; var nmass: extended);
 {find inner radius of shell given outer radius, mass and density;}
           {adjust mass of shell if necessary to find solution; return Nmass=0 if failed}
 {In all of the following, P is total pressure,   i.e. gas pressure plus radiation pressure}
	procedure DoState1 (V, T, Hab, Heab, Metab: real; var P, Pel, xH, xHe, yHe, mu: real); {solves ionization eq}
	procedure FindV (P, T, Hab, HeAb, MetAb: real; var V, mu, xH, xHe, yHe, Pe: real); {iterative solultion of}
           {ionization equation given pressure and temperature: finds specific volume and degrees of ionization}
	procedure FindT (P, V, Hab, HeAb, MetAb: real; var T, mu, xH, xHe, yHe, Pe: real); {iterative solultion of}
           {ionization equation given pressure and specific volume: finds  temperature and degrees of ionization}
	function OpStelling (V, T, Hab, HeAb, MetAb: real): real; {opacity per unit electron pressure}
	function OHMinusOP (T, xH, Hab, mu: real): real;{ H- opacity per unit electron pressure}
	function fBeta (T, P: real): real; {enter with T and P, find ratio of gas pressure to total pressure}
	function adGrad (T, beta, xH, XHe, yHe, Hab, HeAb: real): real; {find adiabatic gradient, dLnT/dLnP}
          {see Cox and Giuli, page 225; this is stored as vari[gamm]}
	procedure FindKapp (V, T, xH, mu, Hab, HeAb, MetAb, Pel: real; var kappa: real); {calculates total opacity}
	procedure SFindFluxes; {find radiative fluxes in all shells from current temperatures and opacities;}
           {See Sellingwerf, Ap J., 195, 462, 1975}
	procedure EndModel (i: integer); {called by FindNextRShell and FindNextrCShell when integration must be ended}
	procedure GetHomMasses (N: integer; Cmass: real); {Get shell masses for constant density model}
	procedure GetInHomMasses (N: integer; Cmass: real); {Get shell masses for other models}
	procedure HHydStat (N: integer); {construct constant-density model}
	procedure findOuterShell;{finds outer shell for radiative atmosphere}
           {consistent with R,L,M and radiative transfer}
	procedure FindNextRShell (s: integer);{does one shell of radiative model}
	procedure RHydStat (N: integer); {constructs model in radiative equilibrium}
	procedure FindTemp (s: integer);{finds next temperature from known pressure by iterates with}
           { adiabatic temperature gradient; assumes average adgrad, rather than acutally computing the entropy;}
           { so this is not exact, but it is very close}
	procedure FindNextCShell (i: integer); {djoes one shell of isentropic model}
	procedure CHydStat (N: integer);{Finds radiative outer shell, readjusts masses, then}
           {works inward using isentropic relation dLnT/dLnP = adgrad. The model is called convective, }
           {and it assumes convection carries the entire heat flux from the interior.}
	procedure EitherStat (N: integer);{Finds radiative outer shell, same procedure as for radiatvie model, then}
           {works inward using either isentropic relation dLnT/dLnP = adgrad or radiative gradient, whichever leads}
           {to smaller temperature increase.}
           {This algorithm  convection must be subsonic, and it retains radiative gradient if the subsonic}
           {convection cannot carry total energy.}
	procedure FindTempGrad;{find actual logarithmic temp gradient vs pressure in model}
	procedure BuildModel (var okay: boolean); {driver, called by menu commands}

 {=======================================================}

implementation

	function frand; {returns randomly selected real number in range min-max}
	begin
		frand := abs(random / 32767) * (max - min) + min;
	end;

	procedure FindRadius;
		var
			rtemp: extended;
			i: integer;
	begin
		i := 0;

		repeat
			rtemp := Router * Router * Router - Mymass / Mydens * 3 / fourpi;
			if rtemp < 0 then
				Mymass := 0.917 * Mymass; {decrease mass if solution is  imaginary}
			i := i + 1;
		until (rtemp > 0) or (i > 100);

		if rtemp > 0 then
			begin
				Rinner := exp(ln(rtemp) / 3);
				Nmass := Mymass;
			end
		else
			Nmass := 0; {failed to find solution; time to exit}
	end;{Function radius}

	function fbeta; {beta=Pg/Ptot= 1-pr/ptot}
		var
			b: real;
	begin
		b := 1 - (T * T * T * T * 2.521e-15) / p;
		fBeta := b;
	end;

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
		if t < 1000 then
			gocold
		else if t < 15000 then
			GoLow
		else if t > 30000 then
			goHigh
		else
			goMedium;

		mu := 1 / ((1 + xH) * HAb + (1 + xHe) * HeAb / 4 + yHe * HeAb / 2);
		P := gas * T / mu / V; {gas pressure}
		P := P + T * T * T * T * 2.521e-15; {add radiation pressure to find total pressure}
		Pel := gas * T / V * (metab / 200 + xH * HAb + (xHe + 2 * YHe) * HeAb / 4);

	end;{DOState1}
 {=======================================================}


	procedure FindV;{(P, T, HeAb, MetAb: real; var V,  mu, xH, xHe, yHe, Pe: real)}
		var
			loop: integer;
			pt, pg, mun: real;
	begin
		if T > 2e6 then {assume complete ionization}
			begin
				mu := Habund / 2 + 3 * HeAbund / 4;
				v := gas * T / (mu * (p - 2.2521e-15 * T * T * T * T));
				xH := 1;
				xHe := 0;
				yHe := 1;
				Pe := gas * T / V * (metab / 200 + xH * HAbund + (xHe + 2 * YHe) * HeAbund / 4);

			end
		else
			begin
				mu := 1.0;
				loop := 0;
				repeat {iterate on mu}
					loop := loop + 1;
					pg := p - T * T * T * T * 2.521e-15;
  {¥find gas pressure}
					if pg < 0 then
						pg := p;{ignore radiation pressure when pressure is too small}
					v := gas * t / pg / mu;
					DOState1(v, t, HAb, HeAb, MetAb, pt, pe, xH, xHe, yHe, mun);
					mu := 0.25 * mu + 0.75 * mun;
				until ((abs(mu - mun) < 0.001) or (loop > 100));

				{if loop > 100 then
					beep;}
			end;
	end;{findV}
 {=======================================================}

	procedure FindT;{(P, V, HeAb, MetAb: real; var T,  mu, xH, xHe, yHe, Pe: real)}
		var
			loop: integer;
			pt, pg, mun, rat, TN: real;
	begin

		mu := 1.0;{first guess}
		loop := 0;
		t := p * v / gas * mu;

		if t > 2e6 then {assume complete ionization}
			begin
				loop := 0;
				TN := T;
				mu := HAbund / 2 + 3 * HeAbund / 4;
				repeat
					loop := loop + 1;
					T := P / (gas / mu / V + 2.2521e-15 * TN * TN * TN);
					RAT := T / TN;
					TN := T;
				until ((abs(Rat - 1) < 0.01) or (loop > 100));
				{if loop > 100 then
					beep;}
			end
		else
			begin
				repeat {iterate on mu}
					loop := loop + 1;
					pg := p - T * T * T * T * 2.521e-15; {find gas pressure}
					T := Pg * mu * v / gas;
					DOState1(v, t, HAb, HeAb, MetAb, pt, pe, xH, xHe, yHe, mun);
					mu := 0.25 * mu + 0.75 * mun;
				until ((abs(Pt / p - 1) < 0.01) or (loop > 100));
				{if loop > 100 then
					beep;}
			end;

	end;{findV}

 {=======================================================}
	function OpStelling;
 { function OpStelling (V, T, Hab, HeAb, MetAb: real): real; opacity per unit electron pressure}
    { general fit to king opacity tables; ApJ 195,465, 1975}
		var
			zvar1, zvar2, yvar1, yvar2, u, u4, u6, squ, um5, um8, um10, u25, u35, um45, v1, v2, t1, t2: real;
		function xey (x, y: real): real;
		begin
			xey := exp(ln(x) * y);
		end;

	begin
 {transform input variables}
		zvar1 := 21.0 * metab + 0.9790;
		zvar2 := 105.0 * metab + 0.8950;
		yvar1 := -6.0e-5 * heab + 6.294e-5;
		yvar2 := 3.53e6 * heab - 3.0447e5;
		u := t / 1.0e4;
		v1 := xey(v, 0.350);
		v2 := sqrt(v1);
 {compute auxiliary quantities}
		u4 := t * t * t * t / 1.0e16;
		u6 := u4 * sqr(u);
		squ := sqrt(t) * 1.0e-2;
		um8 := 1.0 / sqr(u4);
		um10 := 1.0 / (u4 * u6);
		u25 := squ * sqr(u);
		u35 := u * u25;
		um45 := 1.0 / (u * u35);
		um5 := u / u6;
 {get to work}
		t1 := 760.0 / um5 + 316.0 / v2;
		t1 := yvar1 * v1 * u35 + 1.0 / t1;
		t1 := 1.0 / (10.0 * u6 + 1.0 / t1);
		t2 := zvar1 * yvar2 * um10 + 2.13e-3 * v2 * zvar2 * um45;
		t2 := 513.40 * u4 / (zvar1 * u) + 1.0 / t2; {this is what came from Warsaw}
 {t2 := 1780 * u25 / zvar1 + t2; this is the expression in Stellingwerf's original paper}
 {It does not give good results at high temp}
		t2 := 47.30 * um8 + 1.0 / t2;
		t2 := 1.0 / (4.0e3 + 1.0 / t2);
		opstelling := (4.819e-13 * v / u + t1 + t2);{kappa/pe}
	end; {OpStelling}
 {=======================================================}
	function OHMinusOP;

		function xey (x, y: real): real;
		begin
			xey := exp(ln(x) * y);
		end;
	begin
		if T < 15000 then
			OHMinusOP := HAb * (0.602e-06) * (1.0 - xH) / mu * 4.571e18 / xey(T, 3.7)
		else
			OHMInusOP := 0;
	end;{OHMinusOP}
 {=======================================================}

 {function adGrad (T, beta, xH, XHe, yHe, Hab, HeAb: real): real; returns gam2/(gam2-1); }
 {this is stored as vari[gamm]; see Cox and Giuli, page 225}
	function adGrad;
		const
			degpev = 11604.9;
		var
			xsi, ybar, num, sum, denom: real;
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

		if ion > 0 then
			begin
				ybar := y[1] * Hab + (y[2] + y[3] * 2) * HeAb / 4;
				Xsi := 2 * ybar * y[ion] * (1 - y[ion]) * nu[ion];
				Xsi := Xsi / (ybar * (1 + ybar) + y[ion] * (1 - y[ion]) * nu[ion]);
				denom := (4 - 3 * beta) * (8 - 3 * beta) + 12 * beta * (1 - beta) + beta * beta * xsi * sqr((4 * (1 - beta) / beta
                                + (2.5 + chi[ion] * degpev / t)));
				sum := 4 * (1 - beta) / beta + 2.5 + chi[ion] * degpev / t;
				num := 2 * (4 - 3 * beta) + beta * xsi * sum;
				adgrad := num / denom;
			end
		else
			adgrad := 0.4;
	end;{AdGrad}

 {=======================================================}
	procedure FindKapp;{    ( V , T , xH , mu , Hab , HeAb , MetAb , Pel : real;var kappa : real );    }

		var
			c, hm: real;
			ke, kk: real;
	begin
		if t < 5e04 then
			begin
				c := opStelling(V, T, Hab, HeAb, MetAb);

				hm := OHMinusOP(T, xH, Hab, mu);
				if pel = 0 then
					pel := 1;
				kappa := (c + hm) * Pel;
				if kappa < 0.01 then
					kappa := 0.01;
			end
		else  {KOuzes' Kramers' plus electrton scattering}
			begin
				ke := 0.2 * (1 + HAbund) / (1 + 2.7e11 / v / t / t) / (1 + pwr(t / 4.5e8, 0.86));
				kk := 2 * (1 + HAbund) * (0.02 + 1 - hAbund - HeAbund) / v / pwr(t / 1e+07, 3.5);
				kappa := ke + kk;
			end;
	end;{FindKapp}


 {=======================================================}
	procedure SFindFluxes; {find radiative flux from temperatures and opacities.}
 {Uses Stellingwerf algorithm. }

		var
			s: integer;
			a, b, b1, c, rn, wi, wip1, ne: extended;

		function fW (T: real): real;
			var
				ttemp: real;
		begin
			ttemp := t / star.teff;
			fW := Ttemp * Ttemp * Ttemp * Ttemp;
		end;

	begin

		c := 4 * fourpi * starlum * solarlum / 3;
		rn := starrad * solarrad;
 {surface shell}
		with star.eq[numshells] do
			begin
				vari[flux] := starlum * solarlum;

			end;
 {now, interior }
		for s := numshells - 1 downto 1 do
			begin
				with star.eq[s] do
					begin
						wi := fw(vari[temp]);
						wip1 := fw(star.eq[s + 1].vari[temp]);
						a := sqr(vari[radi]) * sqr(vari[radi]) / sqr(rn);
						b1 := (wi - wip1) / dm2 * (wi / vari[kapp] + wip1 / star.eq[s + 1].vari[kapp]) / (wi + wip1);
						ne := ln(star.eq[s + 1].vari[kapp] / vari[kapp]) / ln(wip1 / wi);
						b := -(wip1 / star.eq[s + 1].vari[kapp] - wi / vari[kapp]) / (1 - ne) / dm2;
						vari[flux] := a * b * c;
					end;{with}
			end;{for }
 {at surface}
	end; {SFindfluxes}
 {=======================================================}

 {find RadGrad := logarithmic  gradient, dLnT/dLnP}
	procedure FindTempGrad;
		var
			s: integer;
	begin

		for s := numshells - 1 downto 1 do
			with star.eq[s] do
				vari[radgrad] :=
                                 ln(star.eq[s + 1].vari[temp] / star.eq[s].vari[temp]) / ln(star.eq[s + 1].vari[pres]
                                 / star.eq[s].vari[pres]);

		star.eq[numshells].vari[radGrad] := star.eq[numshells - 1].vari[radGrad]; {for graphical tidiness, not actually defined}

	end;
 {=======================================================}

	procedure EndModel;{called by findNextRshell and findNextCShell when integration has failed}
		var
			j, v: integer;
	begin
		with star do
			begin
				numshells := numshells - i;
				for j := 0 to numshells do
					begin
						for v := radi to Pe do
							star.eq[j].vari[v] := star.eq[j + i].vari[v];

						star.eq[j].intmass := star.eq[j + i].intmass;
						star.eq[j].dm1 := star.eq[j + i].dm1;
						star.eq[j].dm2 := star.eq[j + i].dm2;

					end;
				coremass := star.eq[i].intmass / starmass / solarmass;
				envelmass := 1 - coremass;
				building := false;
				modelname := concat(modelname, '/'); {add mark to end of name to indicate that the }
 {model was truncated}
			end;{with}

		{beep;   }
	end;{EndModel}
 {=======================================================}

	procedure GetHomMasses;
 {find geometric sequence of shell masses; returns values in gm}

		var
			i, j: integer;
			dm, xi, xim1, summasses, normfact: real;
			ok: boolean;

		function RCube (r: real): real;
		begin
			RCube := exp(3 * ln(r));
		end; {function}


	begin {main  for GetHomMasses}
		StarMeanDen := SolarDen * StarMass / StarRad / StarRad / StarRad; {get mean density of star}
		with star do
			begin

				for i := N downto 0 do
					eq[i].vari[radi] := starRad * solarRad * i / N;  {equally spaced radii }

				for i := N downto 2 do
					begin
						eq[i].vari[dens] := starmeanden;
						Eq[i].dm1 := starmeanden * 4 / 3 * pi * (rCube(eq[i].vari[radi]) - rCube(eq[i - 1].vari[radi]));
					end;
				Eq[1].dm1 := starmeanden * 4 / 3 * pi * rCube(eq[1].vari[radi]);
				eq[1].vari[dens] := starmeanden;


 {now sum to find interior masses, starting from core mass}
				Eq[0].intmass := starmass * solarmass * coremass;
				i := 0;
				repeat
					i := i + 1;
					Eq[i].intmass := Eq[i].dm1 + Eq[i - 1].intmass;
					Eq[i - 1].dm2 := 0.5 * (Eq[i].dm1 + Eq[i - 1].dm1);
				until i >= N;
				Eq[N].dm2 := Eq[N].dm1 / 2;{ / 2¥}
				eq[0].dm2 := 0;
				eq[0].dm1 := coremass;
			end;{with ThisMod}

	end;{GetHomMasses}
 {=======================================================}

	procedure GetInHomMasses;
 {find geometric sequence of shell masses; returns values in gm;}
 {outer shell is mass of tau  = 2/3 layer}
		var
			i: integer;
			dm, q, xi, xim1, TopMass, a: real;
			ok: boolean;
 {-----------------}

		function RCube (r: real): real;
		begin
			RCube := exp(3 * ln(r));
		end; {function}
 {-----------------}


		procedure findq (N: integer; CMass: real; var q: real; var converged: boolean);
			const
				dqLim = 0.0001; {fractional error in q permitted}
			var
				i: integer;
				f, fo, fracErr: real;
				eps: real;
 {-----------------}
			function sgn (f: real): integer;
			begin
				if (f < 0) then
					sgn := -1
				else
					sgn := 1;
			end;
 {-----------------}

		begin {findq, main}
			a := star.eq[numshells].dm1 / envelmass / starmass / solarmass; {outermost shell has been found in findoutershell}
			eps := 0.05;
			q := exp(ln(a) / (N - 1)); {first approximation}
			i := 0;
			converged := false;
 {dq := -(a * (1 - exp(N * ln(q))) * exp((1 - N) * ln(q)) - 1 + q) / (N * (1 - a) - (N - 1) / q);}
			fo := a * (1 - exp(N * ln(q))) - exp((N - 1) * ln(q)) * (1 - q);
			repeat {hunt for q ; the more sophisticated Newton-Raphson method did not work, so we}
 {simply step until discrepancy changes sign, then halve the step and reverse direction}
				q := q + eps;
				f := a * (1 - exp(N * ln(q))) - exp((N - 1) * ln(q)) * (1 - q);
				if (sgn(f) = -sgn(fo)) then {overshot, halve the step and turn around }
					eps := -eps * 0.5;
 {now some bookkeeping}
				fo := f;
				fracErr := abs(eps);
				i := i + 1;
				if q > 1 then
					q := q - eps / 3;
			until ((fracErr < dqLim) or (i > 1000));

			if i < 1000 then
				converged := true;
		end; {findq}
 {-----------------}

	begin {main  for GetInHOmMasses}
		with star do
			begin

				findq(N, CMass, q, ok);

				{if not ok then
					beep; }{this will be replaced by a warning dialog}

				eq[0].intmass := solarmass * starmass * coremass; {mass of core}
				if N > 1 then
					eq[N].dm1 := a * starmass * solarmass * envelmass  {mass of outermost shell = a*envelope mass}
				else
					eq[N].dm1 := starmass * solarmass * envelmass;

 {Now find mass of each shell}
				for i := N - 1 downto 1 do
					eq[i].dm1 := eq[i + 1].dm1 / q;

 {now sum to find interior masses, starting from core mass}
				Eq[0].intmass := starmass * solarmass * coremass;
				i := 0;
				repeat
					i := i + 1;
					Eq[i].intmass := Eq[i].dm1 + Eq[i - 1].intmass;
					Eq[i - 1].dm2 := 0.5 * (Eq[i].dm1 + Eq[i - 1].dm1);
				until i >= N;
				Eq[N].dm2 := Eq[N].dm1 / 2;{ / 2¥}
				eq[0].dm2 := 0;
				eq[0].dm1 := coremass;
			end;{with ThisMod}

	end;{GetInHomMasses}

 {============================================}


 {============================================}
	procedure HHydStat;
 {Uses densities to find the shell radii, pressures and temperatures for the constant-density model}

		var
			i: integer;
			mu, TBeta: real;


 {-------------------------------------------}
	begin {main}
		getHomMasses(numshells, coremass);
		with star do
			begin  {all calculations are expressed in cgs}
				with eq[numshells] do
					begin
 {Hydrostatic pressure equilibrium }
						vari[pres] := intmass * gravCon * dm2 / fourpi / sqr(vari[radi]) / sqr(vari[radi]);
						vari[volu] := 1 / vari[dens];
 {Now find corresponding molecular wt  and temperature}
						mu := 1 / (2 * HAbund + HeAbund / 4 + HeAbund / 2);
						vari[temp] := vari[pres] * mu * vari[volu] / gas;
						vari[gamm] := 0.4; {ionization is complete}
 {No radiation calculations for the homogeneous model, so we use null values}
						vari[kapp] := 0;
						vari[flux] := 0;
						vari[hion] := 1;
						vari[heion] := 0;
						vari[hepion] := 1;
 {now replace with calculated values}
						findT(vari[pres], vari[volu], HAbund, HeAbund, MetAbund, vari[temp], vari[lcmu], vari[hion], vari[heion], vari[hepion],
                                                vari[pe]);
						findKapp(vari[volu], vari[temp], vari[hion], vari[lcmu], HAbund, HeAbund, Metabund, vari[pe], vari[kapp]);
						TBeta := fBeta(vari[temp], vari[pres]);
						vari[gamm] := adgrad(vari[temp], TBeta, vari[hion], vari[heion], vari[hepion], HAbund, HeAbund);


					end;{with}

 {now, do interior}
				for i := N - 1 downto 1 do
					begin
						with eq[i] do
							begin
								vari[pres] := eq[i + 1].vari[pres] + gravcon * intmass * dm2 / fourpi / sqr(vari[radi]) / sqr(vari[radi]);
								vari[volu] := 1 / vari[dens]; {density was already loaded for this model}
								vari[temp] := vari[pres] * mu * vari[volu] / gas;
								vari[gamm] := 0.4;
								vari[kapp] := 0;
								vari[flux] := 0;
								vari[hion] := 1;
								vari[heion] := 0;
								vari[hepion] := 1;
								vari[lcmu] := mu;
 {replace with computed values}
								findT(vari[pres], vari[volu], HAbund, HeAbund, MetAbund, vari[temp], vari[lcmu], vari[hion], vari[heion],
                                                                vari[hepion], vari[pe]);
								findKapp(vari[volu], vari[temp], vari[hion], vari[lcmu], HAbund, HeAbund, Metabund, vari[pe], vari[kapp]);
								TBeta := fBeta(vari[temp], vari[pres]);
								vari[gamm] := adgrad(vari[temp], TBeta, vari[hion], vari[heion], vari[hepion], HAbund, HeAbund);
							end;{with}

					end;{for i}

 {now do core}

				with eq[0] do {core; not really  used}
					begin

						vari[hion] := 1;
						vari[heion] := 0;
						vari[hepion] := 1;
						vari[pres] := star.eq[1].vari[pres];
						vari[lcmu] := mu;
						vari[gamm] := 0.4;
						vari[dens] := star.eq[1].vari[dens];
						vari[temp] := star.eq[1].vari[temp];
					end;{with eq[0]}

			end; {with Star}
		FindTempGrad;

	end;{HHydStat}
 {============================================}

	procedure FindOutershell; {integrates isothermal  atmosphere  to find pressure at}
 {tau = 2/3; used in all but the homogeneous model}
		var
			te, beta, rinner, g: real;
			newmass: extended;
 {---------------}
		function findPressure (teff, geff: real): real; {integrates isothermal atmosphere from}
 {TauTop to TauM}
			const
				m = 10;{number of layers in the upper, isothermal atmosphere used to find pressure}
				tauM = 0.667;
				tauTop = 0.0667; {these were chosen by trial and error to give a good result}
			var
				lnP, lnP0, taurat, lnPgrad, t, dt, kappa: real;
				V, mu, xH, xHe, yHe, Pe: real;
				i: integer;

		begin
			lnP0 := ln(tauTop * geff); {crude calculation for starting value; }
 {assumes kappa = 1 all the way out; result is not sensitive to this assumption}
			taurat := exp(ln(tauM / tauTop) / M); {ratio in geometric series}
			lnP := lnP0;
			i := 0;
			t := tauTop;
			repeat
 {find opacity}
				FindV(exp(lnP), teff, Habund, HeAbund, MetAbund, V, mu, xH, xHe, yHe, Pe);
				findKapp(V, Teff, xH, mu, HAbund, heabund, metabund, pe, kappa);
 {now compute derivative of hydrostatic equation}
				lnPgrad := geff / kappa / exp(lnP);
				if lnPgrad > 200 then
					lnPgrad := 200;

				dt := (taurat - 1) * t;
				t := taurat * t;
 {integrate it another step}
				lnP := lnP + lnPgrad * dt;
				i := i + 1;
			until ((t > tauM) or (i > M));
			findpressure := exp(lnP);
		end;{function findPressure }
 {---------------}

	begin {main Findoutershell}
		with star.eq[Numshells] do
			begin
				vari[radi] := starrad * solarrad;
				star.teff := 5700 * exp(ln(starLum / starRad / starRad) / 4);
				vari[temp] := star.teff;
				g := 27401 * starMass / starRad / starRad; {surface gravity}
				star.grav := g;
				vari[pres] := findpressure(star.teff, g); {found from model atmosphere}
 {now finish the calculations for this shell}
				FindV(vari[pres], vari[temp], Habund, HeAbund, MetAbund, vari[Volu], vari[lcmu], vari[Hion], vari[Heion],
                                 vari[Hepion], vari[Pe]);
				findKapp(vari[volu], vari[temp], vari[Hion], vari[lcmu], HAbund, heabund, metabund, vari[Pe], vari[kapp]);
				dm2 := fourpi * vari[radi] * vari[radi] * vari[pres] / g;
				dm1 := 2 * dm2;
				beta := fbeta(vari[temp], vari[pres]);
				vari[gamm] := adgrad(vari[temp], beta, vari[Hion], vari[heion], vari[hepion], HAbund, Heabund);
				vari[dens] := 1 / vari[volu];
				vari[w] := 1;
				FindRadius(dm1, vari[dens], vari[radi], rinner, newmass); {find inner surface of shell}
				dm1 := newmass;
				star.eq[Numshells - 1].vari[radi] := rinner;
			end;{with}
	end; {outershell}
 {=======================================================}

	procedure FindNextRShell;
		var
			loop: integer;
			rinner, diffmass, beta: real;
			newmass: extended;
 {---------------------------------}
 {------------------}

		procedure FindW (s: integer); {finds temperature of inner shell by Newton-Raphson iteration to give}
 {constant flux , g=0; differential dg/dW is computed numerically}

			var
				WTN: extended; {effectively double precision}
				oldG, newG, step: extended;
				i: Integer;
				err, errorlim: real;
				diffg: extended;
 {------------------}

			procedure findg (dm2, rad: real; wout, win: extended; kapout, kapin: real; var g: extended);
 {The vanishing of this function is the condition of flux =   desired flux}
				var
					gtemp: extended;
			begin
				gtemp := 3 * starrad * solarrad * starrad * solarrad * dm2 / 4 / fourpi / rad / rad / rad / rad;
 {g := gtemp - (win - wout) / (win + wout) * (win / kapin + wout / kapout); Christy}
				g := gtemp - (win / kapin - wout / kapout) / (1 - ln(kapin / kapout) / ln(win / wout)); {Stellingwerf;}
 {this is the only place where g appears}
			end;{findg}
 {------------------}


 {------------------}


			function sign (x: real): integer;
			begin
				if x = 0 then
					sign := 1
				else if x / abs(x) > 0 then
					sign := 1
				else
					sign := -1;
			end;

 {------------------}
		begin{main findW}
 {W  = T^4/Te^4 is the dimensionless temperature variable}
			with star.eq[s] do
				begin
					WTn := star.eq[s + 1].vari[W] * 1.4; {FIrst guess at new  temperature function W=T^4/Te^4}
					loop := 0;

					step := 1;
					vari[temp] := star.teff * sqrt(sqrt(WTN));
					findV(vari[pres], vari[temp], HAbund, HeAbund, MetAbund, vari[volu], vari[lcmu], vari[Hion], vari[Heion],
                                        vari[Hepion], vari[Pe]);
					findKapp(vari[volu], vari[temp], vari[Hion], vari[lcmu], HAbund, heabund, metabund, vari[Pe], vari[kapp]);
					findg(dm2, vari[radi], star.eq[s + 1].vari[w], WTN, star.eq[s + 1].vari[kapp], vari[kapp], NewG);

					repeat
						step := step / 2;
						loop := 0;
						repeat
							loop := loop + 1;

							oldG := newG;
							wtn := wtn * (1 + sign(oldg) * step); {increment WTN}
							vari[temp] := star.teff * sqrt(sqrt(WTN));
							findV(vari[pres], vari[temp], HAbund, HeAbund, MetAbund, vari[volu], vari[lcmu], vari[Hion], vari[Heion],
                                                        vari[Hepion], vari[Pe]);
							findKapp(vari[volu], vari[temp], vari[Hion], vari[lcmu], HAbund, heabund, metabund, vari[Pe], vari[kapp]);
							findg(dm2, vari[radi], star.eq[s + 1].vari[w], WTN, star.eq[s + 1].vari[kapp], vari[kapp], NewG);

						until ((sign(NewG) <> Sign(OldG)) or (loop > 20));

					until ((step < 0.01) or (loop > 20));

 {store the new value of W}
					vari[W] := WTN;
					vari[dens] := 1 / vari[volu];
				end;{with}
			{if loop > 20 then

				beep; }
		end; {findW}

 {-----------------------------------------}
	begin {main FindNextRShell}
		with star.Eq[s] do
			begin
				vari[pres] := star.eq[s + 1].vari[pres] + gravcon * intmass * dm2 / fourpi / sqr(vari[radi]) / sqr(vari[radi]);
				FindW(s); {find temperature corresponding to radiative equilibrium}
				beta := fbeta(vari[temp], vari[pres]);
				vari[gamm] := adgrad(vari[temp], beta, vari[Hion], vari[heion], vari[hepion], HAbund, Heabund);

 {now find the inner radius of the shell}
				FindRadius(dm1, vari[dens], vari[radi], rinner, Newmass);

				if newmass = 0 then {step failed, time to quit}
					Endmodel(s)
				else if NewMass < dm1 then
					begin
						diffmass := dm1 - newmass; {this positive quantity has been subtracted from shell and}
 {must be added to core, leaving all others unchanged}
						dm1 := newmass;
						dm2 := 0.5 * (star.Eq[s + 1].dm1 + dm1);
						Star.eq[0].intmass := Star.eq[0].intmass + diffmass;
						envelmass := 1 - Star.eq[0].intmass / starmass / solarmass;
						coremass := 1 - envelmass;
					end;
				star.eq[s - 1].vari[radi] := rinner;
			end;{with}
	end; {findNextRshell}


 {============================================}

	procedure RHydStat; {model in radiative equilbirum}
		var
			i: integer;

	begin {main of RHydStat}

		with star do
			begin
				FindOuterShell;
				getInHomMasses(numshells, coremass);

				for i := numshells - 1 downto 1 do
					if building then
						begin
							FindNextRShell(i); {if the model has been truncated, building = false and this loop is exitted}

						end;
				SFindFluxes; {evaluate radiative fluxes as a check on the model}
				FindTempGrad;
			end; {with}
	end; {RHydStat}
 {===========================}


	procedure findTemp; {finds next temperature from known pressure by iterates wiht adiabatic temperature gradient}
 {assumes average adgrad, rather than acutally computing the entropy; so this is not exact, but it is very close}
		var

			TT, DT, LNPrat, LNT1, TN, g1, g2, beta: real;
			loop: integer;


	begin{main findTemp}

		loop := 0;
		with star.eq[s + 1] do
			begin
				LNT1 := ln(vari[temp]);
				beta := fbeta(vari[temp], vari[pres]);
				g1 := adgrad(vari[temp], beta, vari[hion], vari[heion], vari[hepion], Habund, Heabund); {outer shell}
				LNPRat := ln(star.eq[s].vari[pres] / star.eq[s + 1].vari[pres]);
				tt := star.eq[s + 1].vari[temp] * exp(g1 * LNPrat); {guess at next shell}
				TN := vari[temp]; {store for later comparison}
			end;
		with star do
			begin
				repeat
					loop := loop + 1;
					findV(eq[s].vari[pres], tt, Habund, heabund, metabund, eq[s].vari[volu], eq[s].vari[lcmu], eq[s].vari[hion],
                                        eq[s].vari[heion], eq[s].vari[hepion], eq[s].vari[pe]);
					beta := fbeta(tt, eq[s].vari[pres]);
					g2 := adgrad(tt, beta, eq[s].vari[hion], eq[s].vari[heion], eq[s].vari[hepion], habund, heabund);
					TT := exp(LNT1 + LNPrat * (g1 + g2) / 2);
					DT := TT - TN;{get this for check on convergence}
					tN := tt; {temp storage for later  comparison}


				until ((abs(DT) / TT < 0.0001) or (loop > 100));
				eq[s].vari[temp] := TT;
				eq[s].vari[gamm] := g2;
			end; {with star}
	end; {findTemp}

 {================================}
	procedure findNextCShell;
		var
			rinner, diffmass: real;
			newmass: extended;
	begin
		with star.eq[i] do
			begin
				vari[pres] := star.eq[i + 1].vari[pres] + gravcon * intmass / sqr(vari[radi]) * dm2 / fourpi / sqr(vari[radi]);
				findTemp(i);{also loads volu and ionization}
				vari[dens] := 1 / vari[volu];
				findKapp(vari[volu], vari[temp], vari[Hion], vari[lcmu], HAbund, heabund, metabund, vari[Pe], vari[kapp]);

				FindRadius(dm1, vari[dens], vari[radi], rinner, newmass);

				if newmass = 0 then {step failed, time to exit}
					EndModel(i)
				else if newmass < dm1 then {masses adjusted}
					begin
						diffmass := dm1 - newmass; {this positive quantity has been subtracted from shell and}
 {must be added to core, leaving all others unchanged}
						intmass := intmass - diffmass;
						dm1 := newmass;
						dm2 := 0.5 * (star.Eq[i + 1].dm1 + dm1);
						Star.eq[0].intmass := Star.eq[0].intmass + diffmass;
						envelmass := 1 - Star.eq[0].intmass / starmass / solarmass;
						coremass := 1 - envelmass;
					end;
				star.eq[i - 1].vari[radi] := rinner;

			end;
	end;{findNextCShell}
 {============================================}
	procedure CHydStat; {Finds radiative outer shell, readjusts masses, then}
 {works inward using isentropic relation dLnT/dLnP = adgrad. The model is called convective, }
 {and it assumes convection carries the entire heat flux from the interior.}
		var
			i: integer;
			rinner: real;

	begin {main CHydstat}

		with star do
			begin
				FindOuterShell;
				getInHomMasses(numshells, coremass);

				for i := numshells - 1 downto 1 do
					if building then
						begin
							FindNextCShell(i);

						end;
				SFindFluxes; {evaluate radiative fluxes}
				FindTempGrad;
			end; {with}
	end; {CHydStat}
 {=======================================================}
	procedure EitherStat; {Finds radiative outer shell, same procedure as for radiatvie model, then}
 {works inward using either isentropic relation dLnT/dLnP = adgrad or radiative gradient, whichever leads}
 {to smaller temperature increase.}
 {This algorithm  convection must be subsonic, and it retains radiative gradient if the subsonic}
 {convection cannot carry total energy.}
		var
			i: integer;
			rad, myreset: boolean;
			ctemp, rtemp, TotFlux, MaxCflux: real;
	begin {main Eitherstat}
		with star do
			begin
				totflux := starlum * solarlum / sqr(solarrad * starrad) / fourpi;
				FindOuterShell;
				getInHomMasses(numshells, coremass);
				rad := true;

				for i := numshells - 1 downto 1 do
					if building then
						begin {Find both gradients; order depends on previous shell}
							if rad then
								begin
									FindNextCShell(i);
									CTemp := eq[i].vari[temp];
									findnextRshell(i);
									RTemp := eq[i].vari[temp];
									maxCflux := eq[i].vari[dens] / 10 * exp(1.5 * ln(gas * eq[i].vari[temp])); {maximum}
 {convective flux is estimated at rho*a^3/2 divided by arbitrary fudge factor; if this is smaller than total flux, }
 { the star's temperature gradient in this layer is assumed to be the radiative gradient.}
								end
							else
								begin
									findnextRshell(i);
									RTemp := eq[i].vari[temp];
									FindNextCShell(i);
									CTemp := eq[i].vari[temp];
								end;

 {now select the lower gradient}
							if rad then
								begin
									if ((CTemp < Rtemp) and (maxcflux > totflux)) then {it is conv}
										begin {redo shell because we guessed wrong}
											FindNextCShell(i);
											Myreset := true;
										end
									else
										Myreset := false; {We guessed right; it is radiative; no need to recompute shell}
								end

							else {It  was conv}

								begin
									if RTemp < CTemp then {it is radiative}
										begin
											findnextRshell(i);{redo shell because we guessed wrong}
											Myreset := true;
										end
									else
										Myreset := false;{We guessed right; no need to recompute shell}
								end;

 {get ready for next shell}
							if Myreset then
								if rad then
									rad := false
								else
									rad := true;

						end;
				SFindFluxes; {evaluate radiative fluxes}
				FindTempGrad;
			end; {with}
	end; {CHydStat}
 {=======================================================}
 {Here is the driver for this unit; it is called by the menu}
	procedure buildmodel (var okay: boolean);
		var
			i: integer;
	begin
		building := true;

 {now solve the equation of hydrostatic equilibrium for pressure etc.}

		case star.densityLaw of
			Dhom: 
				HHydStat(Numshells);
			DRadCon: 
				EitherStat(Numshells);
			DRad: 
				RHydStat(Numshells);
			DCon: 
				CHydStat(Numshells);

		end; {case}

 {compute mean radii of shells for plotting}
		for i := numshells downto 1 do
			star.eq[i].vari[mrad] := (star.eq[i].vari[radi] + star.eq[i - 1].vari[radi]) / 2;
		if building then
			okay := true;
		building := false;


	end; {buildmodel}

 {=======================================================}



end.