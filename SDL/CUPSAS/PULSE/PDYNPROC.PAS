unit PDynProcs;

  {Unit for Stellar Atmosphere; C. A. Whitney}
 {Solve for Eigen modess and}
 {carry linear superposition forward in time, calculating physical variables}

 {---STELLAR PULSATION: C. A. WHITNEY------}
 {----------Modified June 21, 1994-------}

interface

	uses
		crt, dos, graph, printer, CUPS, cupsMUPP, CUPSfunc, CupsProc, cupsgui, CUPSGrph, PCommon, PoutPut;
 {--------------MATRIX PROCEDURES-------}
	function BuildTriDiagMat: boolean; {builds tri-diagonal matrix for linear oscillations}
	procedure FindMatA; {find Aij from G1i,j-1}
	procedure ClearMat (var myMat: Sqmatrixtype);
	procedure clearVect (var avect: vectortype);
	procedure MatVectMult (myMat: Sqmatrixtype; myVect: vectortype; var outVect: Vectortype);
	procedure TQLI (n: integer; var d, e: vectortype; var z: SqMatrixtype);
	procedure FindLinearModes; {Driver: Finds <=4 eigen modes by TQLI}


 {------------PULSATION SYNTHESIS AND SCALING-------------}
	procedure FindADisplacement (t: real);
	procedure FindAVelocity (t: real);
	procedure FindADensity;
	procedure SynthesizeAModel (t: real);{find all variables at time t}
	procedure StepAhead (eqn, HT: integer);
 	{PROCEDURE GetOtherScales (var ThisWRec: windowrec);}
{called by getallscales for each window}
	procedure GetMaxRelVals;{called by getallscales for all variables}
	procedure GetAllScales; {called by startcalc}
	procedure StartCalc;   {Initialize and find scales for plots}
	procedure DoOneStep;  {carry pulsation ahead one step}
   procedure FindFAmplitudes;

implementation
 {===================MATRIX PROCEDURES==========}

	procedure MatVectMult;
		var
			i, j: integer;
	begin
		outvect.size := myvect.size;
		clearVect(outVect);
		for i := 1 to myMat.size do
			for j := 1 to myMat.size do
				outVect.e[i] := outVect.e[i] + myMat.e[i, j] * myVect.e[j];
	end;

	procedure TriDMVmult (inmat: SqMatrixtype; invect: vectortype; var outvect: vectortype);
		var
			i, j, k: integer;

	begin
		outvect.size := invect.size;
		clearVect(outvect);
		for i := 1 to invect.size do
			for j := 1 to 3 do
				begin
					k := i - 1 + j - 1;
					if ((k > 0) and (k <= invect.size)) then
						outvect.e[i] := outvect.e[i] + G1[i, j] * invect.e[k];
				end;
	end;

 {----------------------------}
	procedure ClearMat;
		var
			i, j: integer;
	begin
		for i := 1 to myMat.size do
			for j := 1 to myMat.size do
				myMat.e[i, j] := 0;
	end;
 {-------------------------}

	procedure clearVect;
		var
			i: integer;
	begin
		for i := 1 to avect.size do
			avect.e[i] := 0;
	end;
 {-------------------------}

	function BuildTriDiagMat; {returns true if successful}
		var
			i, j: integer;
			gtemp, f1temp: extended;

		function F1 (k: integer): real;
			var
				f, gam1: real;
		begin
			with star.eq[k] do
				begin
					gam1 := 1 / (1 - vari[gamm]);
  {vari[gamm] is actually (g-1)/g}
 {gam1 := 5 / 3;}
 {force for comparison with theory. This is the only appeartance of gamma}
					if k <= numshells then
						F := gam1 * vari[pres] * vari[dens] * 4 * pi * vari[radi] * vari[radi] / dm1;
				end;
			F1 := f;
		end;

		function F2 (k: integer): real;
			var
				f: real;
		begin
			with star.eq[k] do
				F := 4 * pi * vari[radi] * vari[radi] / sqrt(dm2);
			f2 := f;
		end;

	begin
		buildTriDiagMat := false; {initialize}


		for i := 1 to numshells do {clear G1}
			for j := 1 to 3 do
				G1[i, j] := 0;


		with star do
			begin
				for i := 1 to numshells - 1 do {off-diagonal elements}
					begin
						G1[i, 3] := F2(i) * F1(i + 1) / sqrt(eq[i + 1].dm2);
						G1[i + 1, 1] := G1[i, 3]; {symmetry}
					end;{for i}
				for i := 1 to numshells do {diagonal  elements}
					begin
						gtemp := 4 * GravCon * eq[i].intmass / eq[i].vari[radi] / eq[i].vari[radi] / eq[i].vari[radi];
						if i < numshells then
							f1temp := F1(i + 1) * sqr(eq[i].vari[radi] / eq[i + 1].vari[radi])
						else
							f1temp := 0;
						G1[i, 2] := gtemp - F2(i) / sqrt(eq[i].dm2) * (f1temp + F1(i));

					end;{for i}


			end;{;with star}

		BuildTriDiagMat := true;
	end;

 {--------------------------}

	procedure FindMatA; {called by FIndLinearMOdes to find}
 {coefficients for numerical integration of Finite Difference Equation }
		var
			i, j, k: integer;
	begin
		for i := 2 to numshells - 1 do
			begin
				for j := 1 to 3 do {A and G1 have the same array indexing, but A has the masses removed}
					begin
						Aij[i, j] := G1[i, j] * sqrt(star.eq[i + j - 2].dm2 / star.eq[i].dm2);
					end;
			end;
 {now the end cases}
		Aij[1, 2] := G1[1, 2] * sqrt(star.eq[1].dm2 / star.eq[1].dm2);
		Aij[1, 3] := G1[1, 3] * sqrt(star.eq[2].dm2 / star.eq[1].dm2);
		Aij[numshells, 1] := G1[numshells, 1] * sqrt(star.eq[numshells - 1].dm2 / star.eq[numshells].dm2);
		Aij[numshells, 2] := G1[numshells, 2] * sqrt(star.eq[numshells].dm2 / star.eq[numshells].dm2);
	end;

 {===============================================}
	procedure TQLI;{ (n, nv: integer; var d, e: vectortype; var z: SqMatrixtype);}
 {Finds eigen values and vectors of real symmetric, tri-diagonal matrix; }
 {input: d, and e are the diagonal and sub-diagonal elements of the matrix}
 {Numerical Recipes, Chapter 11; p. 751}

		label
			1, 2;
		var
			m, L, iter, i, k, length, Twidth, TTop: integer;
			s, r, p, g, f, dd, c, b: real;
 {-------------------}
		function sign (a, b: real): real;
		begin
			if (b < 0) then
				sign := -abs(a)
			else
				sign := abs(a);
		end;
 {----------------------------}
	begin {Main TQLI}
 {renumbering has been done in driver}

		for L := 1 to n do
			begin
				iter := 0;
1:
				for m := L to n - 1 do
					begin
						dd := abs(d.E[m]) + abs(d.E[m + 1]);
						if (abs(e.E[m]) + dd = dd) then
							goto 2
					end;
				m := n;
2:
				if (m <> L) then
					begin
						if (iter = 50) then
							beep;
						iter := iter + 1;
						g := (d.E[L + 1] - d.E[L]) / (2.0 * e.E[L]);
						r := sqrt(sqr(g) + 1.0);
						g := d.E[m] - d.E[L] + e.E[L] / (g + sign(r, g));
						s := 1.0;
						c := 1.0;
						p := 0.0;
						for i := m - 1 downto L do
							begin
								f := s * e.E[i];
								b := c * e.E[i];
								if (abs(f) >= abs(g)) then
									begin
										c := g / f;
										r := sqrt(sqr(c) + 1.0);
										e.E[i + 1] := f * r;
										s := 1.0 / r;
										c := c * s;
									end
								else
									begin
										s := f / g;
										r := sqrt(sqr(s) + 1.0);
										e.E[i + 1] := g * r;
										c := 1 / r;
										s := s * c;
									end;
								g := d.E[i + 1] - p;
								r := (d.E[i] - g) * s + 2.0 * c * b;
								p := s * r;
								d.E[i + 1] := g + p;
								g := c * r - b;

 {compute nv eigen vectors}
								for k := 1 to n do
									begin
										f := z.E[k, i + 1];
										z.E[k, i + 1] := s * z.E[k, i] + c * f;
										z.E[k, i] := c * z.E[k, i] - s * f;
									end;
							end;
						d.E[L] := d.E[L] - p;
						e.E[L] := g;
						e.E[m] := 0.0;
						goto 1;
					end;
			end;
	end;
 {======================================}

	procedure EigSrt (var d: vectorType; var v: Sqmatrixtype; n: integer);
 {NR p 749, sorts eigenvalues in order of size}
		var
			k, j, i: Integer;
			p: real;

	begin
		for i := 1 to n - 1 do
			begin
				k := i;
				p := d.E[i];
				for j := i + 1 to n do
					begin
						if (d.e[j] >= p) then
							begin
								k := j;
								p := d.e[j];
							end;
					end;
				if (k <> i) then
					begin
						d.e[k] := d.e[i];
						d.e[i] := p;
						for j := 1 to n do
							begin
								p := v.e[j, i];
								v.e[j, i] := v.e[j, k];
								v.e[j, k] := p;
							end;
					end;
			end;
	end;{Eigsort}

 {============================================}

	procedure FindLinearModes;{driver for TQLI}
		label
			1;
		const
			ShowIt = false; {If true, the eigenvectors will be displayed}
			tridiag = false;
		var
			i, j, n, nv, top: integer;
			Doit: boolean;
			Q, fundfreq: real;
			normfact: real;{for normalizing wave function}
	begin
 {first, set up the input for TQLI from the original tri-diagonal matrix}
 {Go from Castor's to NumRecip. notation}

		for i := 1 to numshells do
			dVect.E[i] := G1[i, 2]; {diagonal elements}
		for i := 2 to numshells do {sub-diagonal elemenst; this performs the renumbering in TQLI}
			eVect.E[i - 1] := G1[i, 1];
		eVect.E[numshells] := 0;

 {now set z:= unitary matrix}
		zMat.size := numshells;
		clearMat(zMat);
		for i := 1 to zMat.size do
			zMat.E[i, i] := 1;
		n := numshells;
		top := 175;

 {AT THIS POINT THE VECTORS ARE AVAILABLE FOR DISPLAY}

		TQLI(n, dVect, eVect, zMat); {carry out the solutions}

 {sort by eigen frequency}
		EigSrt(dVect, zMat, n);

		for i := 1 to numshells do {step through shells}
			begin
 {compute coeffs that will be used for finding variation of thermodynamic variables}
 {in dynprocs.p unit}
				star.ER1[i] := star.eq[i].vari[dens] / star.eq[i].dm1 * fourpi * star.eq[i - 1].vari[radi] * star.eq[i - 1].vari[radi];
				star.ER2[i] := star.eq[i].vari[dens] / star.eq[i].dm1 * fourpi * star.eq[i].vari[radi] * star.eq[i].vari[radi];

 {renormalize dr  = X/Sqrt(DM2) and put into mode. shape}
 {renumber output so fundamental =mode[0]}

				for j := 1 to numshells do {construct modes}
					begin
						normfact := zMat.E[numshells, j] / sqrt(star.eq[numshells].dm2); {surface displacement}
						mode[j - 1].RShape[i] := zMat.E[i, j] / sqrt(star.eq[i].dm2) / normfact; {Displacements}
					end;
			end;

 {find frequencies}
 {First, clear out the old ones in case star has fewer shells}
		for j := 1 to maxnumshells do
			begin
				mode[j - 1].freq := 0;
				mode[j - 1].I_freq := 0;
                                			end;
 {renumber output so fundamental =mode[0]}

		for j := 1 to numshells do
			begin
				mode[j - 1].freq := sqrt(abs(dvect.E[j]));
				mode[j - 1].I_freq := 0; {undamped if Heat Trans= adiabatic only}
			end;
 {compute pulsation constant from lowest frequency}
		Q := 2 * pi / mode[0].freq / 86400 / 1.187 * sqrt(StarMass / (starRad * starRad * starRad));
 {dt := 0.1 * pi / mode[0].freq;}

		FindMatA; {solve for dynamical coefficients from G1 matrix;
                       for use in Linear_FDE integration}

	end;


 {================PULSATION SYNTHESIS AND SCALING=====================}
	procedure FindADisplacement;  {(t)}
 {computes linear displacements }
		var
			h, s: integer;
			Targ, MContr, RelAmp: real;
			MadeIt: boolean;

 {---------------}
		procedure ReduceAmp (f: real);
			var
				m: integer;
		begin
			for m := 0 to numshells-1 do  {rescale all modes}
				mode[m].amp := f * mode[m].Amp;
         for m := 1 to 20 do    {rescale variables}
                maxrelval[m]:=f*maxrelval[m];
		end;
 {-------------------------}
 	begin {main FindADisplacement}
		relAmp := 1 / 15;
		madeIt := true;
		with star do
			repeat
				if ((not madeIt) and (runstate = stopped)) then
					begin
						reduceAmp(0.8);
						madeit := true;
					end;
				for s := 1 to numshells do
					begin
						dyn[s].vari[disp] := 0;
						for h := 0 to numshells-1 do
							with mode[h] do
								begin
									if active then
										begin
											Targ := t * freq + phase;
											Mcontr := Rshape[s] * amp * cos(Targ) * starrad * solarrad * RelAmp; {max at t=0}
											dyn[s].vari[disp] := dyn[s].vari[disp] + MContr;
										end;{for h}
									dyn[s].vari[radi] := eq[s].vari[radi] + dyn[s].vari[disp];
 {add displacement to find current radius}
									dyn[s].vari[Mrad] := eq[s].vari[Mrad];
                                                  { do not use this + (dyn[s].vari[disp] + dyn[s - 1].vari[disp]) / 2;}
 {defined inside the shells}
								end;{with mode}

						if ((runstate = stopped) and (dyn[s].vari[radi] <= dyn[s - 1].vari[radi])) then {shells overlap}
							madeIt := false;
					end;{for s}

			until (madeIt or (RelAmp < 0.01));{for star,s}
	end;{GetLProfiles}

 {=========================}
	procedure FindADensity; {uses current displacements with coefficients computed}
		var
			s: integer;
	begin
		with star do
			for s := 1 to numshells do
				dyn[s].vari[dens] := ER1[s] * dyn[s - 1].vari[disp] - ER2[s] * dyn[s].vari[disp];
 { note, for comparison with Castor: ER1[s]   = DR1[s-1]*sqrt(dm2[s]), and ER2[s]=DR2[s-1]*sqrt(dm2[s+1])}
	end;

 {=========================}

	procedure FindAVelocity;  {(t)}
 {computes linear velocities}
		var
			h, s: integer;
			Targ, MContr: real;
	begin
		with star do
			for s := 1 to numshells do
				begin
					dyn[s].vari[velo] := 0;
					for h := 0 to numshells do
						with mode[h] do
							begin
								if active then
									begin
										Targ := t * freq + phase;
										MContr := Rshape[s] * amp * cos(Targ + pi / 2) * mode[h].freq * starrad * solarrad / 15;
										dyn[s].vari[velo] := dyn[s].vari[velo] + MContr;
									end;{if active}
							end;{for h}
				end; {for star,s}
	end;{FindAVelocity}
 {=============================}

	procedure FindRequiredVariables;{Linear theory: uses density and entropy to find other variables}
		var
			s: integer;
	begin
		with star do
			begin
				for s := 1 to numshells do
					begin
						dyn[s].vari[gamm] := eq[s].vari[gamm]; {¥should this be recomputed using routines in Build.p?}
 {Variations are probably not needed for linear pulsations.}
						dyn[s].vari[temp] := dyn[s].vari[gamm] * dyn[s].vari[dens];
						dyn[s].vari[pres] := dyn[s].vari[dens] / (1 - dyn[s].vari[gamm]);
						dyn[s].vari[volu] := -dyn[s].vari[dens];
						if heattrans = adiabatic then
							begin
							 dyn[s].vari[flux] := 0;
     					    dyn[s].vari[bolmag] :=  0;
                     end;
					 end;
            if time>0 then
				   dyn[numshells].vari[bolmag] := 5 * 0.4343 * ln(dyn[numshells].vari[radi] / solarrad) + 10 * 0.43243 *
                                ln((1 + dyn[numshells].vari[temp]) * eq[numshells].vari[temp] / 5770);
			end; {with star}
	end;

 {=============================}
	procedure GetMaxRelVals;    {get ranges for all variables }
		var
			i, v: integer;
	begin
		with star do
			begin
				for v := 1 to 20 do
					begin
						if (v in [radi, pres, temp, flux, velo, disp, bolmag, dens]) then
							begin
								for i := 1 to Numshells do
									begin
										if abs(star.dyn[i].vari[v]) > MaxRelVal[v] then
											MaxRelVal[v] := abs(star.dyn[i].vari[v]);
									end; {for}
							end; {ifv }
					end; {for v=1 to ..}
 {Re-scale two variables}
				maxrelval[bolmag] := 1.01 * MaxRelVal[bolmag];{expand slightly}
 {MaxRelVal[velo]:=MaxRelVal[velo]/1e5;}
 {MaxRelVal[disp]:=MaxRelVal[disp]/starRad/SolarRad;}
			end;{with Star}
	end;{getMaxrelVals}

 {======================}
	procedure SynthesizeAModel;
	begin
		findADisplacement(t);
 {FindAnEntropy(t);}
		findaVelocity(t);
		FindADensity;
		FindRequiredVariables;

	end;
 {=================================}
	procedure StepAhead (eqn, HT: integer);
 {so-called Improved Euler numerical integration, similar to 2nd order RK}

		var
			i: integer;
			f, g: array[1..maxnumshells] of real;
			acc, at, h: vectortype;
 {--------------------}
		procedure FindAcc (y: vectortype; var acc: vectortype);
                {uses coeff of linearized equations}
			var
				i, j: integer;
		begin
			for i := 2 to numshells - 1 do {interior masses}
				begin
					acc.e[i] := 0;{       initialize}
					for j := 1 to 3 do
						acc.e[i] := acc.e[i] + Aij[i, j] * star.dyn[i + j - 2].vari[disp]; {sign?}
				end;
 {now the end points}
			acc.e[1] := Aij[1, 2] * star.dyn[1].vari[disp] + Aij[1, 3] * star.dyn[2].vari[disp];
			acc.e[numshells] := Aij[numshells, 1] * star.dyn[numshells - 1].vari[disp] +
                         Aij[numshells, 2] * star.dyn[numshells].vari[disp];

		end;
 {--------------------}
	begin {main stepAhead}
		with star do
			begin

				for i := 1 to numshells do
					h.e[i] := dyn[i].vari[disp]; {load  disp in dummy variable}

				FindAcc(h, acc);

				for i := 1 to numshells do {First guess at end time}
					begin
						f[i] := dyn[i].vari[disp] + dt[equation] * dyn[i].vari[velo];
						g[i] := dyn[i].vari[velo] + dt[equation] * acc.e[i];
					end;

				for i := 1 to numshells do
					h.e[i] := f[i]; {load new  disp in dummy variable}

				findAcc(h, at); {acceleration at end time}

				for i := 1 to numshells do {revised end-time data}
					begin
						dyn[i].vari[disp] := dyn[i].vari[disp] + dt[equation] * (dyn[i].vari[velo] + g[i]) / 2;
						dyn[i].vari[velo] := dyn[i].vari[velo] + dt[equation] * (acc.e[i] + at.e[i]) / 2;

                                        {dyn[i].vari[disp] := dyn[i].vari[disp] + dt[equation] * (dyn[i].vari[velo] );
                                        {these are euler method
						dyn[i].vari[velo] := dyn[i].vari[velo] + dt[equation] * (acc.e[i]) ;
			                                    }

					end;
 {now evaluate other thermodynamic variables}
				findADensity;
				findRequiredVariables;
			end;{with star}
	end;

 {=================================}
	procedure GetAllScales; {scan through one cycle of the linear variation to find amplitude of }
 {synthesized variation of each variable. These are stored for each window and are available for }
 {each frame, on the assumption that all frames in a given window have the same size.}
		const
			numSynsteps = 15; {number of steps used in scanning  cycle for scaling}
			NumFDESteps = 80;
		var
			i, j, myHeight, DHeight, myWidth, f, fLower, fUpper: integer;
			a: real;
 {-----------------}
		procedure FindDt; {find an appropriate time step to cover the cycle in 5 steps}
			var
				m: integer;
		begin
			dt[linear_Syn] := 0;
			m := 4;
			repeat
				m := m - 1;
				if mode[m].active then
					dt[linear_Syn] := pi / mode[m].freq / (numSynsteps + 1) * 6;
			until ((dt[linear_Syn] > 0) or (m = 0));
			dt[Linear_FDE] := pi / mode[numshells - 1].freq / NumFDESteps; {must use time step derived from highest mode}
		end; {find DT}
 {--------------}
	begin {main getAllScales}
 {Get ready to scan a cycle to find yscale}
		for f := 1 to 4 do
			for i := 1 to maxnumpoints do  {clear arrays}
				for j := 1 to 5 do
					begin
						stripChart[f].olddata[j, i] := 0;
						stripChart[f].newdata[j, i] := 0;
					end;
		FindDt;
		if dt[linear_Syn] = 0 then  {probably no mode was active at  the time, set one and then reset}
			begin
				mode[0].active := true;
				finddt;
				mode[0].active := false;
			end;
      for i:=1 to 20 do maxrelval[i]:=0; {initialize}
		for i := 0 to numSynsteps do {Step through cycle for each window frame.}
			begin
				synthesizeAModel(i * dt[linear_Syn]);
				GetMaxRelVals;
			end;

		for f := 1 to 4 do
			with GraphScales[f] do
				begin
					selectviewport(f);
					case frame[f].kind of
						stripK: 
							begin
								xmin := 0;
                        xmax:=maxnumpoints;
								{xmax := maxnumpoints*dt[equation]*skipframe/20/8.64e4;}
								ymax := MaxRelVal[frame[f].yvariable];
								if ymax = 0 then
									ymax := 1;
              				ymin := -ymax;

								defineScale(f, xmin, xmax, ymin, ymax);
								Axis(0.0, 0.0, TickSpace(xmax - xmin), TickSpace(ymax - ymin));
							end;
						linePlotK: 
							begin
								xmin := 0;
								if MassPointScale then
									xmax := numshells
								else
									xmax := star.eq[numshells].vari[radi];
								ymax := MaxRelVal[frame[f].yvariable];
								if ymax = 0 then
									ymax := 1;
								ymin := -ymax;
								defineScale(f, xmin, xmax, ymin, ymax);
								Axis(0.0, 0.0, TickSpace(xmax - xmin), TickSpace(ymax - ymin));
							end;
						PhaseDiagK: 
							begin
								xmax := maxRelVal[frame[f].xvariable];
								xmin := -xmax;
								ymax := maxRelVal[frame[f].yvariable];
								if ymax = 0 then
									ymax := 1;
								ymin := -ymax;
								defineScale(f, xmin, xmax, ymin, ymax);
								Axis(0.0, 0.0, TickSpace(xmax - xmin), TickSpace(ymax - ymin));

							end;
						AnimDiagK: 
							begin
								xmax := maxRelVal[radi];
								xmin := -xmax;
								ymax := xmax;
								ymin := -ymin;
								defineScale(f, xmin, xmax, ymin, ymax);
							end;
					end;
				end;{FOR f with graphscales}
	end;{GetAllScales}
 {=================================}

  procedure FindFAmplitudes; {Fourier analysis to find amplitudes
  from initDisp as set manually}
  var
     m,s:integer;
     Am,Cm:array[0..maxnumshells] of real;
     Bms:array[0..maxnumshells,0..maxNumshells] of real;

     {Definition: Bms = mode[m].rShape[s]*scaling factors}
     {Definition: Am = mode[m].amp}
  begin
     for m:=0 to numshells-1 do
      begin
           Cm[m]:=0;
           Am[m]:=0;
           for s:=1 to numshells do    {Find normalization}
               begin
               Bms[m,s] := mode[m].RShape[s]*sqrt(star.eq[s].dm2);
               Cm[m]:=Cm[m]+ Bms[m,s]*Bms[m,s];
               Am[m]:=Am[m]+Bms[m,s]*star.dyn[s].vari[disp]*sqrt(star.eq[s].dm2);
               end;{for s}
           mode[m].amp:=Am[m]/Cm[m]*10;     {amp corrrection}
           if (abs(mode[m].amp)>0) then mode[m].active:=true
              else mode[m].active:=false;
     end;{for m}
  end; {FindFAmplitudes}
 {====================================}

	procedure StartCalc; {called by hotkey Run}
		var
			i,s: integer;

	begin
		if buildtridiagmat then
			begin
				FindLinearModes;
			end;
		counter := 0;
		framecount := 0;
		time := 0;
		runstate := stopped;
      for s:=1 to numshells do
          star.dyn[s].vari[disp]:=initDisp[s]; {load initial values}

       {Find fourier amplitudes for synthesis. These are either set
       in window for modes or they are derived from the set displacement
       window. Each of these windows resets a boolean, manualDisp,
        which tells how the amplitudes are to be determined.}
      If manualDisp then FindFAmplitudes; {use initDisp to find amplitudes}
        {and load into Mode[m].amp}
		synthesizeAModel(0); {set  up the initial conditions}
      GetAllScales;
      dt[linear_syn]:=dt[linear_syn]/20;
      {dt[Linear_FDE]:= dt[linear_syn]/40;
      dt[nonlinear]:=dt[linear_FDE]; }
      with mymenu do rowactivate(4,5,true);
	end;{StartCalc}

 {=========================}

	procedure DoOneStep; {called after StartCalc in run hotkey or by continue}
		var
			harm, theframe: integer;
			t: real;
			nowtick: longint;

	begin  { main loop}

		counter := counter + 1;{no. of steps since started}
		time := dt[equation] + time;
		if (equation = linear_Syn) then
			skipFrame := 1
		else {equation is linear_FDE or nonlinear}
			skipFrame := 20;

 {move ahead one time  step; will depend on method}
		if (equation = linear_Syn) then
			SynthesizeAModel(time)
		else
			StepAhead(Linear_FDE, HeatTrans);

		if ((counter mod skipframe) = 0) then
			begin
				framecount := framecount + 1; {number of points plotted}
      {draw appropriate frames}
				plotFrames;
			end; {If}
		justCleared := false;
	end;{DoOneStep}
end.