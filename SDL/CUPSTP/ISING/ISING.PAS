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

program Ising;
{Simulation of the two-dimensional Ising Model}
{using the Metropolis, demon, Wolff, and Kawasaki algorithms.}
{June 28, 1994, Harvey Gould with assistance from Jan Tobochnik}
   uses
      dos,crt,graph, CUPSmupp, CUPS, CUPSfunc, CUPSproc, CUPSgui, CUPSgrph;
   const
      Lmax = 48;   {maximum linear dimension}
   type
      list = array[0..9] of real;
      probability = array[-200..200] of longint;
      correlation = array[1..200] of real;
      unitvector = array[1..4] of integer;
      perimeter = array[1..1024] of integer;
      RunType = (Metro, Demon, Wolff, Kawasaki, MetroDemo, DemonDemo, WolffDemo, KawasakiDemo);
      ProbabilityParameter = record
            E, M: probability;
            xmax, xmin: integer;
            nshow: array[Metro..Kawasaki] of integer;  {nshow # MC steps between showing P(E)}
            max: real;
         end;
      plots = record
            tmax, tmin: longint;
            tmin0, tmax0, emax, emin, Mmax: integer;
         end;
      flags = record    {display flags}
            E, P, ce, renorm, edit, change, changeL, show, demo, exit, rg: boolean;
            run: RunType;
         end;
      correlfn = record
            e, m: correlation;
            esave, msave: correlation;
            nsave, nshow, nmax: array[Metro..Kawasaki] of integer;   {nsave size of time series saved}
         end;
      nodeptr = ^node;
      node = record
            dir: integer;
            next: nodeptr
         end;
      spinarray = array[0..Lmax, 0..Lmax] of node;
      averages = record
            Ecum, E2cum, Edcum, Ed2cum, Mcum, M2cum, Mabscum: real
         end;
      SpinDisplay = record
            x, y: integer
         end;
      SpinColors = record
            up, down: word
         end;
      Length = record
            x, y: integer;
         end;
   var
      Menu: Tmenu;
      HotKeys: ThotKeys;
      Sliders: Tsliders;
      TemperatureSlider, Hslider: Tslider;
      spin: spinarray;
      w: list;
      N, b: integer;
      nmcs, nflip: longint;
      T, E, Ed, M, ratio, J, H, pb: real;
      P: ProbabilityParameter;
      average: averages;
      flag: flags;
      plot: plots;
      C: correlfn;
      Color: SpinColors;
      L: Length;
      pixels: SpinDisplay;
      nx, ny: unitvector;
      nper, ndir, noccup, mass, line, levelmax: integer;
      perx, pery: perimeter;
      seed, seed0, rsc1,rsc2: integer;  {rsc radius of plot circle}
      start, quit, hideGrid: boolean;

   procedure NewAxis;
   var x,y : real;
       vp  : integer;
   begin
      vp := ViewPortNumber;
      SelectMuppetPort;
      SetFillStyle(SolidFill,black);
      HideMouse;
      with views[vp] do
      begin
         Bar(vx1-50,vy1-7,vx1-1,vy2+13);
         Bar(vx1-50,vy2+1,vx2+10,vy2+13);
      end;
      ShowMouse;
      with graphScales[ScaleNumber] do
      begin
         if (xmin=0) or (xmax=0) then x := 0
         else x := sgn(xmin) *Pwr(10, trunc(Log10( abs(xmin))+1 ));
         if (ymin=0) or (xmax=0) then y := 0
         else y := sgn(ymin) *Pwr(10, trunc(Log10( abs(ymin))+1 ));
      end;
      SelectViewPort(vp);
      Axis(x,y,0,0);
   end;

   procedure closeup;
   begin
      closeViewPort(2);
      closeViewPort(3);
      closeViewPort(4);
      closeViewPort(5);    {color codes}
      closeviewport(6);    {E/M window}
      closeViewPort(7);    {P(E)}
      closeViewPort(8);    {correl fns}
      closeViewPort(9);
      closeViewPort(10);
      Sliders.done;
      Temperatureslider.Done;
      Hslider.Done;
   end;

   procedure ShowTitle;
      var
         i: integer;
         C: HelpScrType;
   begin {ShowTitle}
      for i := 1 to 25 do
         C[i] := '';
      C[4] := '        SIMULATION OF THE ISING MODEL';
      C[5] := '                      by  ';
      C[6] := '         Harvey Gould, Clark University  ';
      C[9] := '                 May 13, 1995';
      C[10] := '           (c) 1995 John Wiley & Sons';
      C[16] := '   A Monte Carlo program for simulating the';
      C[17] := '        Ising model in two dimensions.';
      C[18] := '        Choose Demo for demonstration.';
      C[24] := '  Press any key or click mouse to continue.';
      HELP(C);
   end; {ShowTitle}

   procedure GeneralInfo;
      var
         i: integer;
         C: HelpScrType;
   begin
      for i := 1 to 25 do
         C[i] := '';
      C[2]  := ' The Ising model is defined on a lattice with a';
      C[3]  := ' binary variable associated with each site.';
      C[4]  := ' In the context of magnetism, this variable is a';
      C[5]  := ' magnetic moment or spin and has the values';
      C[6]  := ' +1 (up) or - 1 (down).';
      C[7]  := ' Each spin interacts with its nearest-neighbors';
      C[8]  := ' and with an external magnetic field.';
      C[9]  := ' The interaction between two nearest-neighbor';
      C[10] := ' spins is -J if the spins are parallel and +J';
      C[11] := ' if the spins are antiparallel.';
      C[13] := ' The exchange constant J > 0 so that the state';
      C[14] := ' of lowest energy is ferromagnetic.';
      C[15] := ' The temperature is measured in terms of J/k.';
      C[16] := ' The default parameters are for the square';
      C[17] := ' lattice of linear dimension L in zero magnetic';
      C[18] := ' field at the critical temperature of the';
      C[19] := ' infinite lattice.' ;
      C[25] := '     Press any key or click mouse to continue.';
      HELP(C);
   end; {GeneralInfo}

   procedure Description;
      var
         i: integer;
         C: HelpScrType;
   begin
      for i := 1 to 25 do C[i] := '';
      C[01] := ' Subject  :  Thermal and Statistical Physics';
      C[02] := ' Program  :  Ising';
      C[03] := ' Title    :  Ising Model In One and Two Dimensions';
      C[04] := ' Author   :  Harvey Gould, Clark University';
      C[05] := ' Abstract :';
      C[06] := '';
      C[07] := '  ISING allows the user to explore the static and';
      C[08] := '  dynamic properties of the one- and';
      C[09] := '  two-dimensional Ising model using four different';
      C[10] := '  Monte Carlo algorithms and three different';
      C[11] := '  ensembles.  The choice of the Metropolis';
      C[12] := '  algorithm allows the user to study the Ising';
      C[13] := '  model at constant temperature and external';
      C[14] := '  magnetic field.  The orientation of the spins';
      C[15] := '  is shown on the screen as well as the evolution';
      C[16] := '  of the total energy or magnetization.  The mean';
      C[17] := '  energy, magnetization, heat capacity, and';
      C[18] := '  susceptibility are monitored as a function of';
      C[19] := '  the number of configurations that are sampled.';
      C[20] := '  Other computed quantities include the';
      C[21] := '  equilibrium-averaged energy and magnetization';
      C[22] := '  autocorrelation functions and the energy';
      C[23] := '  histogram.  Important physical concepts that';
      C[25] := '                                           over->';
      Help(c);
      for i := 1 to 25 do C[i] := '';
      C[01] := '  can be studied with the aid of the program';
      C[02] := '  include the Boltzmann probability, the';
      C[03] := '  qualitative behavior of systems near critical';
      C[04] := '  points, critical exponents, the renormalization';
      C[05] := '  group, and critical slowing down. Other';
      C[06] := '  algorithms that can be chosen by the user';
      C[07] := '  correspond to spin exchange dynamics (constant';
      C[08] := '  magnetization), constant energy (the Demon';
      C[09] := '  algorithm), and single cluster Wolff dynamics.';
      C[10] := '  The latter is particularly useful for';
      C[11] := '  generating equilibrium configurations at the';
      C[12] := '  critical point.';
      HELP(C);
   end; {GeneralInfo}

   procedure HotKeysHelp;
      var
         i: integer;
         C: HelpScrType;
   begin {HelpHotKeys}
      for i := 1 to 25 do C[i] := '';
      C[1]  := '              Hot Keys Summary                    ';
      C[3]  := ' Choose F1 to call help files.';
      C[5]  := ' Choose F2 to begin the simulation or to pause';
      C[6]  := ' if the simulation is running.';
      C[8]  := ' Choose F3 to hide spins and reduce the speed';
      C[9]  := ' penalty of screen drawing.';
      C[11] := ' Choose F4 to reset the time and reinitialize ';
      C[12] := ' the computation of the correlation functions';
      C[13] := ' and the energy histogram.';
      C[15] := ' Choose F5 to edit the individual spins.';
      C[17] := ' Choose F10 to return to menu.';
      C[25] := '    Press any key or click mouse to continue.';
      HELP(C);
   end; {HelpHotKeys}

   procedure HelpHotkeys;
      var
         i: integer;
         C: HelpScrType;
   begin {HelpHotKeys}
      for i := 1 to 25 do C[i] := '';
      case flag.run of
       Metro:
          begin
             C[2]  := '           Metropolis Algorithm';
             C[4]  := ' The algorithm generates configurations with';
             C[5]  := ' the correct Boltzmann probability at fixed';
             C[6]  := ' temperature and magnetic field.';
             C[8]  := ' It is convenient to measure the time in Monte';
             C[9]  := ' Carlo steps per spin (mcs). On the average';
             C[10] := ' all N spins have one chance to change their';
             C[11] := ' orientation in one mcs.';
             C[13] := ' Output:';
             C[14] := ' Upper-left window: total energy as a function';
             C[15] := ' of time.';
             C[17] := ' Upper-right window: The energy autocorrelation';
             C[18] := ' function as a function of the time difference.';
             C[20] := ' Middle-right window: the probability P(E) that';
             C[21] := ' the system has energy E.';
             C[22] := ' Click on output windows to see the corresponding';
             C[23] := ' physical quantities for the magnetization.';
          end;
       Demon:
          begin
             C[2]  := '           Demon Algorithm';
             C[4]  := ' The algorithm generates configurations at';
             C[5]  := ' fixed energy and magnetic field.';
             C[7]  := ' It is convenient to measure the time in Monte';
             C[8]  := ' Carlo steps per spin (mcs). On the average';
             C[9]  := ' all N spins have one chance to change their';
             C[10] := ' orientation in one mcs.';
             C[13] := ' Output:';
             C[14] := ' Upper-left window: energy of the demon as a';
             C[15] := ' function of time.';
             C[17] := ' Upper-right window: The demon energy auto-';
             C[18] := ' correlation function.';
             C[20] := ' Middle-right window: the probability P(Ed) that';
             C[21] := ' the demon has energy Ed.';
             C[22] := ' Click on output windows to see the corresponding';
             C[23] := ' physical quantities for the magnetization.';
          end;
       Wolff:
          begin
             C[2]  := '     Wolff (single cluster) Algorithm';
             C[4]  := ' The algorithm generates configurations at';
             C[5]  := ' fixed temperature. The magnetic field is set';
             C[6]  := ' equal to zero because the algorithm is not';
             C[7]  := ' efficient except at the critical point.';
             C[9]  := ' It is convenient to measure the time in terms';
             C[10] := ' of cluster flips.';
             C[12] := ' Output:';
             C[14] := ' Upper-left window: energy of the system as a';
             C[15] := ' function of time.';
             C[16] := ' Upper-right window: The energy autocorrelation';
             C[17] := ' function.';
             C[19] := ' Middle-right window: the probability P(E) that';
             C[20] := ' the system has energy E.';
             C[22] := ' Click on output windows to see the corresponding';
             C[23] := ' physical quantities for the magnetization.';
          end;
       Kawasaki:
          begin
             C[2]  := '    Kawasaki (spin exchange) Algorithm';
             C[4]  := ' The algorithm generates configurations at';
             C[5]  := ' fixed temperature and fixed magnetization.';
             C[7]  := ' It is convenient to measure the time in terms';
             C[8]  := ' of Monte Carlo steps.';
             C[10] := ' Output:';
             C[11] := ' Upper-left window: energy of the system as a';
             C[12] := ' function of time.';
             C[14] := ' Upper-right window: The energy autocorrelation';
             C[15] := ' function.';
             C[17] := ' Middle-right window: the probability P(E) that';
             C[18] := ' the system has energy E.';
             C[20] := ' Click on output windows to see the corresponding';
             C[21] := ' physical quantities for the magnetization.';
          end;
       MetroDemo:
          begin
             C[1]  := '    Summary of the Metropolis algorithm:';
             C[3]  := ' Choose a single spin at random and make a';
             C[4]  := ' trial flip.';
             C[6]  := ' Compute delta E, the change in energy of the';
             C[7]  := ' system, due to the trial flip.';
             C[9]  := ' If delta E is less than or equal to zero, accept';
             C[10] := ' the flip.';
             C[11] := ' Otherwise compute the Boltzmann probability';
             C[12] := ' w = exp(-delta E/kT) and generate a number 0<r<1';
             C[13] := ' from a random sequence.';
             C[15] := ' If r <= w, accept the flip; otherwise retain the';
             C[16] := ' previous configuration.';
             C[18] := ' Click on a spin to generate a trial flip.';
             C[20] := ' The change in the energy is found from the sum';
             C[21] := ' of the four nearest-neighbor spins.';
          end;
       DemonDemo:
          begin
             C[1]  := '    Summary of the Demon algorithm:';
             C[3]  := ' Choose a single spin at random and make a';
             C[4]  := ' trial flip.';
             C[6]  := ' Compute delta E, the change in energy of the';
             C[7]  := ' system, due to the trial flip.';
             C[9]  := ' If the trial flip decreases the energy of the';
             C[10] := ' system, the system gives the extra energy to the';
             C[11] := ' demon and the trial flip is accepted.';
             C[13] := ' If the trial flip increases the energy of the';
             C[14] := ' system, the change is accepted if the demon has';
             C[15] := ' sufficient energy to give to the system.';
             C[16] := ' Otherwise the trial change is rejected and the ';
             C[17] := ' previous spin configuration is retained.';
             C[19] := ' If the trial flip does not change the energy of';
             C[20] := ' the system, the trial change is accepted.';
             C[22] := ' The only restriction is that the demon cannot';
             C[23] := ' have negative energy.';
             C[24] := ' Click on a spin to generate a trial flip.';
          end;
       WolffDemo:
          begin
             C[1]  := '    Summary of the Wolff algorithm:';
             C[3]  := ' Choose a seed spin and grow a cluster by adding';
             C[4]  := ' bonds between parallel spins with probability';
             C[5]  := ' p = 1 - exp(-2J/kT).';
             C[6]  := ' The cluster is flipped with probability 1.';
             C[8]  := ' Click on a spin to choose a seed site.';
             C[9]  := ' The nearest-neighbor parallel spins are shown in';
             C[10] := ' blue and are known as perimeter spins.';
             C[11] := ' Choose a perimeter spin and generate a number ';
             C[12] := ' 0<r<1 from a random sequence.';
             C[13] := ' If r <= p, add the spin to the cluster and find';
             C[14] := ' the new perimeter sites.';
             C[16] := ' Continue adding sites to the cluster until there';
             C[17] := ' are no more perimeter sites.';
             C[19] := ' The Wolff algorithm is an example of a non-local';
             C[20] := ' cluster algorithm. It generates statistically';
             C[21] := ' independent configurations more efficiently than';
             C[22] := ' the single spin-flip Metropolis algorithm.';
          end;
       KawasakiDemo:
          begin
             C[1]  := '    Summary of the Spin Exchange algorithm:' ;
             C[3]  := ' Choose a spin and an antiparallel nearest-' ;
             C[4]  := ' neighbor spin at random.';
             C[6]  := ' Compute delta E, the change in energy of the';
             C[7]  := ' system, due to the trial flip.';
             C[9]  := ' If delta E is less than or equal to zero,';
             C[10] := ' exchange the two spins (flip each spin).';
             C[12] := ' Otherwise compute the Boltzmann probability';
             C[13] := ' p = exp(-delta E/kT) and generate a number ';
             C[14] := ' 0<r<1 from a random sequence.';
             C[16] := ' If r <= p, flip each spin; otherwise retain the';
             C[17] := ' previous configuration.';
             C[19] := ' The algorithm is due to Kawasaki and is known as';
             C[20] := ' the Kawasaki or spin exchange algorithm.';
             C[22] := ' Note that the magnetization is constant.';
          end;
      end;  {case}
      if flag.renorm then C[1] :='xxx';
      C[25] := '      Press any key or click mouse to continue.';
      HELP(C);
   end; {HelpHotKeys}

   function imax (x, y: integer): integer;
   begin
      if x >= y then
         imax := x
      else
         imax := y;
   end;   {imax}

   function imin (x, y: integer): integer;
   begin
      if x <= y then
         imin := x
      else
         imin := y;
   end;   {imin}

   procedure SetupSliders;
   begin
      with Sliders do
         begin
            Init;
            Create(1, 0.1, 4, T, 0.52, 0.18, 0.75, 3, '0', '4', 'T', false);
            Create(2, -2, 2, H, 0.52, 0.08, 0.75, 2, '-2', '2', 'H', false);
         end;
   end;

   procedure SetupOtherSliders;
   begin
      Hslider.Create(-2, 2, H, 0.52, 0.12, 0.75, 2, '-2', '2', 'H', false);
      TemperatureSlider.Create(0.1, 4, T, 0.52, 0.18, 0.75, 3, '0', '4', 'T', false);
   end;   {SetupOtherSliders}

   procedure InitializeMenu;  {set up main menu for major branches of program}
   begin
      Menu.Init;
      with Menu do
         begin
            column(1, 'File');
            row(1, 1, 'About Program');
            row(1, 2, 'About CUPS');
            row(1, 3, 'Configuration');
            row(1, 4, '-------------');
            row(1, 5, 'New Lattice');
            row(1, 6, 'Open');
            row(1, 7, 'Save');
            row(1, 8, '-------------');
            row(1, 9, 'Exit Program');
            column(2, 'Dynamics');
            row(2, 1, 'Metropolis');
            row(2, 2, 'Demon');
            row(2, 3, 'Wolff');
            row(2, 4, 'Kawasaki');
            column(3, 'Demo');
            row(3, 1, 'Metropolis');
            row(3, 2, 'Demon');
            row(3, 3, 'Wolff');
            row(3, 4, 'Kawasaki');
            column(4, 'RG');
            row(4, 1, 'on');
            row(4, 2, 'off');
            column(5, 'Settings');
            row(5, 1, 'Parameters');
            row(5, 2, 'Display');
            column(6, 'Help');
            row(6, 1, 'General Description');
            row(6, 2, 'Ising Model');
            row(6, 3, 'Metropolis Algorithm');
            row(6, 4, 'Metropolis Help');
            row(6, 5, 'Demon Algorithm');
            row(6, 6, 'Demon Help');
            row(6, 7, 'Wolff Algorithm');
            row(6, 8, 'Wolff Help');
            row(6, 9, 'Kawasaki Algorithm');
            row(6,10, 'Kawasaki Help');
            row(6,11, 'Hot Keys Summary');
            display;
            RowActivate(1, 4, False);
            RowActivate(1, 8, False);
            RowActivate(1, 7, False);
            RowActivate(4, 2, False);
            AutoChecking(2, 1);
            AutoChecking(3, 1);
            RowChecked(3, 1, false);
         end;
   end; {InitializeMenu}

   procedure SaveActivate;
   begin
      Menu.RowActivate(1, 7, true);
   end; {SaveActivate}


   procedure ClearDemoMenuCheck;
   begin
      flag.demo := false;
      with Menu do
         begin
            RowChecked(3, 1, false);
            RowChecked(3, 2, false);
            RowChecked(3, 3, false);
            RowChecked(3, 4, false);
            RowActivate(1, 5, true);
            RowActivate(1, 6, true);
            RowActivate(5, 1, true);
            RowActivate(5, 2, true);
         end;
   end;  {ClearDemoMenuCheck}

   procedure EraseDynamicsMenuCheck;
   begin
      with Menu do
         begin
            RowChecked(2, 1, false);
            RowChecked(2, 2, false);
            RowChecked(2, 3, false);
            RowChecked(2, 4, false);
         end
   end;  {EraseDynamicsMenuCheck}

   procedure Boltzmann;
      var
         index, spin, sum: integer;
         beta, arg: real;
   begin
    {compute Boltzmann probabilites; redo calculation after changing H.}
      spin := -1;  {spin is +- 1}
      beta := 1 / (T+0.0000001);
      while (spin <= 1) do
         begin
            sum := -4;
            while (sum <= 4) do
               begin
                  index := 1 - spin;   {index ranges from 0 to 8}
                  index := 5 * index + 4 + sum;
                  index := index div 2;
                  arg := sum * J + h;
                  arg := 2 * spin * beta * arg;
                  w[index] := exp(-arg);
                  sum := sum + 2;
               end;
            spin := spin + 2;
         end;
   end;   {Boltzmann}

   procedure TransitionProbabilities;
      var
         index: integer;
         beta: real;
   begin
    {compute probability ratios for Kawasaki}
      beta := 1 / T;
      for index := 1 to 3 do
         begin
            w[1] := exp(-4 * beta);
            w[2] := exp(-8 * beta);
            w[3] := exp(-12 * beta);
         end;
   end;   {TransitionProbabilities}

   procedure ComputeMinMaxEnergy;
   begin
      if (L.x > 1) and (L.y > 1) then
         plot.emax := trunc(N * (2 * J - abs(h)))
      else plot.emax := trunc(N * (J - abs(h)));
      if flag.run <> demon then plot.emin := trunc(N * (-2 * J - abs(h)))
      else plot.emin := 0;
   end;  {ComputeMinMaxEnergy}

   procedure Bonds;
   begin
      pb := 1.0 - exp(-2.0 / T);    {bond probability}
      nx[1] := 1;
      ny[1] := 0;
      nx[2] := -1;
      ny[2] := 0;
      nx[3] := 0;
      ny[3] := 1;
      nx[4] := 0;
      ny[4] := -1
   end;    {Bonds}

   procedure HandleSliders (Tnew, Hnew: real);
   begin
      case flag.run of
         Metro:
            begin
               Tnew := Sliders.Value(1);
               Hnew := Sliders.Value(2);
               if Tnew <> T then
                  begin
                     flag.exit := true;
                     Sliders.Reset(1, Tnew);
                     T := Tnew;
                     Boltzmann;
                     Sliders.DrawAll;
                  end;
               if Hnew <> H then
                  begin
                     flag.exit := true;
                     Sliders.Reset(2, Hnew);
                     H := Hnew;
                     ComputeMinMaxEnergy;
                     Boltzmann;
                     Sliders.DrawAll;
                  end;
            end;
         demon:
            begin
               if Hnew <> H then
                  begin
                     flag.exit := true;
                     Hslider.Reset(Hnew);
                     H := Hnew;
                  end;
            end;
         Wolff:
            begin
               if Tnew <> T then
                  begin
                     flag.exit := true;
                     Temperatureslider.Reset(Tnew);
                     T := Tnew;
                     bonds;
                  end;
            end;
         Kawasaki:
            begin
               if Tnew <> T then
                  begin
                     flag.exit := true;
                     Temperatureslider.Reset(Tnew);
                     T := Tnew;
                     TransitionProbabilities;
                  end;
            end;
      end;
   end;  {HandleSliders}

   procedure HandleMouse;
      var
         xdum, ydum: real;
         inside: boolean;
   begin
      MousePosn(xdum, ydum, 2, 2, inside);
      if inside then
         begin
            start := true;
            flag.change := false;
         end;
   end;   {HandleMouse}

   procedure HandleHotKeys( knum : byte);
   forward;

   procedure HandleMenu;
   forward;

   function ran (var iseed: integer): real;
   begin
      ran := urand(iseed);
   end;

   procedure RandomInitialConfiguration;
      var
         i, j: integer;
   begin    {random initial configuration}
      for j := 1 to L.y do
         begin
            for i := 1 to L.x do
               begin
                  spin[i, j].next := nil;
                  if (ran(seed) < 0.5) then
                     spin[i, j].dir := 1
                  else
                     spin[i, j].dir := -1;
               end;  {choice of direction of spin}
         end;
   end;     {RandomInitialConfiguration}

   procedure XOpenFile;
      var
         i, j: integer;
         FileName,s: string;
         f: text;
   begin
      FileName := openFile('*.ISI');
      if FileName <> '' then
      begin
         assign(f, Filename);
         reset(f);
         readln(f, s);
         if (s='File created by Ising.EXE') then
         begin
            readln(f, L.x, L.y);
            readln(f, T);
            readln(f, H);
            readln(f);
            for j := 1 to L.y do
               begin
                  for i := 1 to L.x do
                     begin
                        read(f, spin[i, j].dir);
                        if spin[i, j].dir = 0 then
                           spin[i, j].dir := -1;  {spins saved as 0 and 1}
                        spin[i, j].next := nil;
                     end;
                  readln(f);
               end;
         end
         else Announce('This file was not created by Ising.exe');
         close(f);
      end;
      flag.change := true;
   end;   {XOpenFile}

   procedure InitialEOneD;
      var
         i, j, right: integer;
   begin
      {compute initial energy E}
      j := L.y;
      for i := 1 to L.x do
         begin
            if (i = L.x) then
               right := 1
            else
               right := i + 1;
            E := E - spin[i, j].dir * spin[right, j].dir;        {total energy}
            M := M + spin[i, j].dir;
         end
   end; {InitialEOneD}

   procedure InitialE;
      var
         i, j, up, right, sum: integer;
   begin
      {compute initial energy E}
      E := 0;
      M := 0;
      if (L.y = 1) or (L.x = 1) then
         InitialEOneD
      else
         begin
            for j := 1 to L.y do
               begin
                  if (j = L.y) then
                     up := 1
                  else
                     up := j + 1;
                  for i := 1 to L.x do
                     begin
                        if (i = L.x) then
                           right := 1
                        else
                           right := i + 1;
                        sum := spin[i, up].dir + spin[right, j].dir;
                        E := E - spin[i, j].dir * sum;        {total energy}
                        M := M + spin[i, j].dir;
                     end
               end;
         end;
   end;   {InitialE}

   procedure CheckB;
      var
         i: integer;
   begin
      flag.rg := true;
      if L.x <> L.y then
         flag.rg := false;
      if L.x < 8 then
         flag.rg := false;
      if flag.rg then
         begin
            if (L.x mod 2) = 0 then   {L even}
               begin  {now test if lattice can be renormalized at least 3 times}
                  for i := 1 to 2 do
                     if (L.x div trunc(pwr(2, i))) <> (L.x / trunc(pwr(2, i))) then
                        flag.rg := false;
               end
            else {L odd}
               begin
                  for i := 1 to 2 do
                     if (L.x div trunc(pwr(3, i))) <> (L.x / trunc(pwr(3, i))) then
                        flag.rg := false;
               end;
         end;   {rg true}
      if flag.rg then
         Menu.RowActivate(4, 1, true)
      else
         begin
            Menu.RowActivate(4, 1, false);
            if flag.renorm then
               begin
                  flag.renorm := false;
                  Menu.RowActivate(4, 2, false);
               end;
         end;
   end;  {CheckB}

   procedure OneD;
      var
         i, j: integer;
   begin
      if (L.y = 1) then
         begin
            j := 2;
            for i := 1 to L.x do
               spin[i, j].dir := 1;
            j := 0;
            for i := 1 to L.x do
               spin[i, j].dir := -1
         end
      else if (L.x = 1) then
         begin
            i := 2;
            for j := 1 to L.y do
               spin[i, j].dir := 1;
            i := 0;
            for j := 1 to L.y do
               spin[i, j].dir := -1
         end;
         menu.rowactivate(2,3,false);
   end;   {OneD}

   procedure plotparameters;
   begin
      C.nsave[Metro] := imin(10 * N, 200);
      P.nshow[Metro] := 200;   {how often show P(E)}
      C.nshow[Metro] := 200;   {how often show Ce}
      C.nmax[Metro] := imin(5 * N, 200);    {maximum time difference plotted}
      C.nsave[Demon] := imin(10 * N, 200);   {size of array saved}
      P.nshow[Demon] := 200;
      C.nshow[Demon] := 200;
      C.nmax[Demon] := imin(2 * N, 200);
      C.nsave[Wolff] := imin(5 * N, 200);
      P.nshow[Wolff] := 200;
      C.nshow[Wolff] := 200;
      C.nmax[Wolff] := imin(5 * N, 200);
      C.nsave[Kawasaki] := imin(5 * N, 200);
      P.nshow[Kawasaki] := 200;
      C.nshow[Kawasaki] := 200;
      C.nmax[Kawasaki] := imin(5 * N, 200);
   end;   {plotparameters}

   procedure newlattice;
      var
         LatticeScreen: TInputscreen;
         i, temp: integer;
         OK: boolean;
         temp2: real;
   begin
      with LatticeScreen do
         begin
            init;
            DefineInputPort(0.2, 0.85, 0.24, 0.70);
            loadline('          Define New Lattice');
            Loadline('');
            loadline(' number of columns  = {    } (1-48)');
            loadline(' number of rows     = {    } (1-48)');
            loadline(' Set rows = 1 to obtain one-dimensional lattice.');
            Loadline('');
            Loadline(' random number seed = {      } (prime number)');
            Loadline(' show grid lines #T');
            Loadline('');
            Loadline('           [  Ok  ]   [Cancel] ');
            setnumber(1, L.x);
            setnumber(2, L.y);
            setnumber(3, seed0);
            setBoolean(4, not HideGrid);
            setnumberlimits(1, 1, 48);
            setnumberlimits(2, 1, 48);
            setnumberlimits(3, 1, maxint);
            repeat
               AcceptScreen;
               temp := Trunc(getnumber(1));
                  if temp <> L.x then flag.changeL := true;
                  L.x := temp;
                  temp := Trunc(getnumber(2));
                  if temp <> L.y then flag.changeL := true;
                  L.y := temp;
                  OK := not ((flag.run=wolff) and ((L.x=1) or (L.y=1)));
                  if not OK then
                   announce('Wolff algorithm not appropriate for one dimension.');
            until OK;
            if not canceled then
               begin
                  start := false;
                  flag.change := true;
                  flag.changeL := false;
                  N := L.x * L.y;
                  if (L.y = 1) or (L.x = 1) then
                     OneD    {define neighbors for one-dimensional lattice}
                  else menu.rowactivate(2,3,true);
                  seed0 := Trunc(getnumber(3));
                  seed := -seed0;
                  HideGrid := not getBoolean(4);
                  plotparameters;
                  ComputeMinMaxEnergy;
                  CheckB;    {determine value of b}
                  RandomInitialConfiguration;
               end;   {not canceled}
            done;
         end;  {with}
   end;   {newlattice}

   procedure SetUpHotKeys;
   begin
      with HotKeys do
         begin
            init(6); {for PC}
            key[1] := 'F1-Help';
            key[2] := 'F2-Run/Pause';
            key[3] := 'F3-Hide/Show';
            key[4] := 'F4-Reset';
            key[5] := 'F5-Edit';
            key[6] := 'F10-Menu';  {not for Mac}
            Display;
         end;
   end;    {SetUpHotKeys}

   procedure SetUpEditHotKeys;
   begin
      with HotKeys do
         begin
            init(2);
            key[1] := 'F1-Help';
            key[2] := 'F8-Return';
            Display;
         end;
   end;    {SetUpEditHotKeys}

   procedure SetUpDemoHotKey;
   begin
      with HotKeys do
         begin
            {init(1)};  {Mac}
            init(2); {for PC}
            key[1] := 'F1-Help';
            key[2] := 'F10-Menu'; {for PC}
            Display;
         end;
   end;    {SetUpDemoHotKey}

   procedure SetUpRenormHotKeys;
   begin
      with HotKeys do
         begin
            init(3); {for PC}
            key[1] := 'F1-Help';
            key[2] := 'F2-Run/Pause';
            key[3] := 'F10-Menu'; {for PC}
            Display;
         end;
   end;    {SetUpRenormHotKeys}

   procedure HideSpins;
   begin
      SelectViewPort(2);
      CloseViewPort(2);
   end;

   procedure ShowSpin (direction, xt, yt, px, py: integer);
   var color1, oldColor : integer;
   begin
      oldColor := getColor;
      if direction > 0 then color1 := color.up
      else color1 := color.down;
      SetFillStyle(solidfill, color1);
      HideMouse;
      bar(xt, yt, xt + px, yt + py);
      if hideGrid then SetColor(color1);
      rectangle(xt, yt, xt + px, yt + py);
      ShowMouse;
      SetColor(oldColor);
   end;   {ShowSpin}

   procedure ShowSpins;
 {bar(x1,y1,x2,y2) x1,y1 = upper left corner x2,y2 = lower right corner.}
      var
         i, j, xsc, ysc, px, py: integer;
   begin
      SelectViewPort(2);
      SelectScale(2);
      setcolor(white);
      px := pixels.x;
      py := pixels.y;
      for j := 1 to L.y do
         begin
            for i := 1 to L.x do
               begin
                  xsc := (i - 1) * pixels.x;
                  ysc := (j - 1) * pixels.y;
                  ndir := spin[i, j].dir;
                  ShowSpin(ndir, xsc, ysc, px, py);
               end;   {loop i}
         end;   {lattice}
   end;  {ShowSpins}

   procedure ConvertColor (var colour: integer);
   begin
      if colour = 4 then  {red}
         colour := 1
      else if colour = 12 then   {lightred}
         colour := 2
      else if colour = 1 then   {blue}
         colour := 3
      else if colour = 9 then   {lightblue}
         colour := 4
      else if colour = 2 then   {green}
         colour := 5;
   end;    {ConvertColor}

   procedure ReConvertColor (var colour: integer);
   begin
      if colour = 1 then
         colour := 4    {red}
      else if colour = 2 then
         colour := 12  {lightred}
      else if colour = 3 then
         colour := 1 {blue}
      else if colour = 4 then
         colour := 9 {lightblue}
      else if colour = 5 then
         colour := 2; {green}
   end;   {ReConvertColor}

   procedure ShowKawasakiData;
   forward;

   procedure ChangeMagnetization (Mnew: real);
      var
         idiff, i, j, xt, yt: integer;
         xchange: boolean;
   begin
      SelectViewPort(2);
      SelectScale(2);
      setcolor(white);
      idiff := trunc(Mnew - M);
      idiff := idiff div 2;
      idiff := idiff * 2;
      while abs(idiff) <> 0 do
         begin
            xchange := false;
            i := trunc(L.x * ran(seed)) + 1;
            j := trunc(L.y * ran(seed)) + 1;
            if (idiff > 0) and (spin[i, j].dir < 0) then
               begin
                  spin[i, j].dir := 1;
                  xchange := true;
               end;
            if (idiff < 0) and (spin[i, j].dir > 0) then
               begin
                  spin[i, j].dir := -1;
                  xchange := true;
               end;
            if xchange then
               begin
                  M := M + 2 * spin[i, j].dir;
                  idiff := idiff - 2 * spin[i, j].dir;
                  xt := (i - 1) * pixels.x;
                  yt := (j - 1) * pixels.y;
                  ShowSpin(spin[i, j].dir, xt, yt, pixels.x, pixels.y);
               end;   {change}
         end; {while}
      ShowKawasakiData;
   end;  {ChangeMagnetization}

   procedure ChangeParameters;
      var
         DisplayScreen: TInputScreen;
         C: HelpScrType;
         i: integer;
         Mnew: real;
         ns : string[10];
   begin

      for i := 1 to 25 do
         C[i] := '';
      if not flag.renorm then
         begin
            if (flag.run <> Wolff) and (flag.run <> Kawasaki) then
               begin
                  C[4] := ' The magnetic field can be changed from the menu';
                  C[5] := ' or by using the slider bar.';
               end;
            if (flag.run = Wolff) then
               begin
                  C[4] := ' The Wolff (single cluster) algorithm is efficient';
                  C[5] := ' only near the critical temperature and for zero';
                  C[6] := ' magnetic field.';
               end;
            if (flag.run = Kawasaki) then
               begin
                  C[4] := ' The value of the magnetic field is irrelevant for';
                  C[5] := ' Kawasaki (spin exchange) dynamics, because the';
                  C[6] := ' total magnetization is conserved.';
               end
         end
      else   {flag.renorm true}
         begin
            C[1] := ' The visualization of the effects of blocking spins';
            C[2] := ' is most effective at the critical point.';
         end;
      C[25] := '    Press any key or click mouse to continue.';
      with DisplayScreen do
         begin
            Init;
            DefineInputPort(0.01, 0.65, 0.54, 0.89);
            case flag.run of
             demon:    loadline(' Demon Algorithm');
             Wolff:    loadline(' Parameters of Wolff Algorithm');
             Kawasaki: loadline(' Parameters of Kawasaki Algorithm');
             Metro:    loadline(' Parameters of Metropolis Algorithm');
            end;
            loadline('');
            ns := Num2Str(n,4);
            if (flag.run <> demon) then
               begin
                  loadline('temperature           T = {      } > 0');
                  setnumber(1, T);
                  setnumberlimits(1, 0, 1000);
               end
            else
               begin
                  loadline('current demon energy Ed = {      } (0 to '+ns+')');
                  setnumber(1, Ed);
                  setnumberlimits(1, 0, 2 * N);
               end;
            if (flag.run <> Wolff) and (flag.run <> Kawasaki) then
               begin
                  loadline('magnetic field        H = {      } (-2 to 2)');
                  setnumber(2, H);
                  setnumberlimits(2, -2, 2);
               end;
            if (flag.run = Kawasaki) then
               begin
                  ns := Num2Str(n,4);
                  loadline('magnetization         M = {      } (-'+ns+' to '+ns+')');
                  setnumber(2, M);
                  setnumberlimits(2, -N, N);
               end;
            loadline('');
            loadline(' [  Ok  ]   [Cancel]   [ Help ]');
            SetHelpScreen(C);
            AcceptScreen;
            if not canceled then
               begin
                  if (flag.run <> demon) then
                     begin
                        T := getnumber(1);
                        if (flag.run = Metro) then
                           begin
                              Sliders.Reset(1, T);
                              Sliders.DrawAll;
                              Boltzmann;
                           end
                        else
                           begin
                              TemperatureSlider.Reset(T);
                              TemperatureSlider.Draw;
                              if (flag.run = Wolff) then
                                 bonds
                              else
                                 Boltzmann;
                           end;
                     end
                  else
                     Ed := getnumber(1);
                  if (flag.run <> Wolff) and (flag.run <> Kawasaki) then
                     begin
                        H := getnumber(2);
                        if (flag.run = Metro) then
                           begin
                              Sliders.Reset(2, H);
                              Sliders.DrawAll;
                              Boltzmann
                           end
                        else
                           begin
                              HSlider.Reset(H);
                              HSlider.Draw;
                           end;
                     end;
                  if flag.run = Kawasaki then
                     begin
                        Mnew := getnumber(2);
                        if Mnew <> M then
                           ChangeMagnetization(Mnew);
                     end;
               end; {not canceled}
            done;
         end;   {with}
   end;  {ChangeParameters}

   procedure ShowCorrelationFunction (ntemp: longint);
      var
         xsc, ysc: integer;
         idiff: integer;
         Xbar, X2bar, cnorm, correl{, TicX, TicY}: real;
         reset: boolean;
   begin
      reset := false;
      if ntemp <= 0 then
         begin
            ntemp := -ntemp;
            reset := true;
         end;
      OpenViewPort(8);
      SelectScale(8);
      SetColor(white);
      if reset then
         begin
            DefineScale(8, 0, C.nmax[flag.run], 0, 1.1);
            NewAxis;
         end;
         setcolor(graphbackcolor);
         PutLabel(inside, ' '+chr(219));
         SetColor(white);
      if flag.ce then
         begin
            PutLabel(inside, 'Ce');
            if ntemp > 0 then
               begin
                  Xbar := average.Ecum / ntemp;
                  X2bar := average.E2cum / ntemp;
               end;
         end  {end flag.ce}
      else
         begin
            PutLabel(inside, 'Cm');
            if ntemp > 0 then
               begin
                  Xbar := average.Mabscum / ntemp;
                  X2bar := average.M2cum / ntemp;
               end;
         end;   {end of normalization}
      if ntemp >= P.nshow[flag.run] then
         begin
            cnorm := X2bar - Xbar * Xbar;
            if cnorm > 0 then
               begin
                  setcolor(graphbackcolor);
                  PutLabel(inside, ' '+chr(219));
                  SetColor(white);
                  if flag.ce then PutLabel(inside, 'Ce')
                  else PutLabel(inside, 'Cm');
                  cnorm := 1.0 / cnorm;
                  SetColor(yellow);
                  map(0, 1, xsc, ysc);   {value of correlation function at idiff = 0}
                  setfillstyle(solidfill,yellow);
                  fillellipse(xsc, ysc, rsc1,rsc1);
                  for idiff := 1 to C.nmax[flag.run] do
                     begin
                        if flag.ce then
                           correl := C.e[idiff]
                        else
                           correl := C.m[idiff];
                        if correl > 0 then
                           begin
                              correl := correl / (ntemp - C.nsave[flag.run]);
                              correl := correl - Xbar * Xbar;
                              correl := correl * cnorm;
                              map(idiff, correl, xsc, ysc);
                              fillellipse(xsc, ysc, rsc1,rsc1);
                           end
                     end
               end
            else
               begin
                  if flag.run<>kawasaki then
                  print(1, 1, 'check initial condition');
                  print(1, 2, 'no fluctuations');
               end;
         end
      else
         if ntemp > 0 then
         begin
          if flag.run <> demon then
           begin
           Xbar := average.Ecum / ntemp;
           X2bar := average.E2cum / ntemp;
           end
          else
           begin
           Xbar := average.Edcum / ntemp;
           X2bar := average.Ed2cum / ntemp;
           end;
         end;
(*        if (ntemp > 0) then
         begin
            setcolor(lightgreen);
            if C.nshow[flag.run] > 99 then
               idiff := 4  {using iff as a dummy variable}
            else
               idiff := 3;
            if reset then
               begin
                  print(1, 3, concat('need to wait for', NumStr(C.nshow[flag.run], idiff, 0), ' updates'));
                  print(1, 4, 'before results can be shown.');
               end;
         end;*)

   end;      {ShowCorrelationFunction}

   procedure setflags;
   begin
      with flag do
         begin
            demo := false;
            show := true;
            change := true;
            renorm := false;   {default is show original lattice}
            rg := true;
            exit := false;
            changeL := false;
            E := true;
            P := true;
            Ce := true;
            run := Metro;
         end;
      start := false;
      quit := false;
   end;    {setflags}

   procedure SetDefaults;
      var
         ibin, diff: integer;
   begin
      P.max := 0.0;
      T := 2 / (ln(1 + sqrt(2)));
      J := 1;
      H := 0;
      seed0 := 113;
      hideGrid := false;
      L.x := 16;
      L.y := 16;
      b := 2;
      N := L.x * L.y;
      ComputeMinMaxEnergy;
      plotparameters;
      with plot do
         begin
            tmin0 := 0;
            tmax0 := 50;
            tmin := tmin0;
            tmax := tmax0;         {total # times data plotted on screen}
            Mmax := N;
         end;
      rsc1 := 1;
      rsc2 :=2;
      ratio := 0;
      nmcs := 0;
      with average do
         begin
            Ecum := 0;
            E2cum := 0;
            Edcum := 0;
            Ed2cum := 0;
            Mcum := 0;
            M2cum := 0;
            Mabscum := 0;
         end;
      for ibin := -200 to 200 do
         begin
            P.E[ibin] := 0;
            P.m[ibin] := 0;
         end;
      for diff := 1 to C.nsave[flag.run] do
         begin
            C.e[diff] := 0;
            C.m[diff] := 0;
         end;
      Boltzmann;
      color.up := red;
      color.down := green;
   end;    {SetDefaults}

   procedure savedata;

      var
         SaveScreen: TInputScreen;
         FileName: string;
         f: text;
         i, j, temp, m: integer;
         tempbar: real;
         ntemp: longint;
         HS: HelpScrType;
   begin
      for i := 1 to 25 do
         HS[i] := '';
      HS[1] := ' Defaults:';
      HS[3] := ' The linear dimensions of the lattice are saved';
      HS[4] := ' on the first line.';
      HS[6] := ' The temperature or the mean demon energy is saved';
      HS[7] := ' on the second line.';
      HS[9] := ' The magnetic field is saved on the third line.';
      HS[11] := ' The spin configuration is saved as 1 (up) and';
      HS[12] := ' 0 (down).';
      HS[14] := ' The energy histogram and the energy auto-';
      HS[15] := ' correlation function also are saved.';
      HS[24] := '  Press any key or click mouse to continue.';
      filename := 'Ising.isi';
      with SaveScreen do
         begin
            Init;
            DefineInputPort(0.00, 0.39, 0.71, 0.90);
            loadline('filename "           "');
            loadline('');
            Loadline('[  Ok  ]   [Cancel]   [ Help ]');
            setstring(1, Filename);
            setHelpScreen(HS);
            AcceptScreen;
            Filename := getstring(1);
            if (not canceled) and (filename<>'') then
               begin
                 m := Pos('.',FileName)+1;
                 if copy(FileName,m,3)<>'ISI' then
                 begin
                    Delete(FileName,m,3);
                    Insert('ISI',FileName,m);
                 end;
                  assign(f, Filename);
                  rewrite(f);
                  writeln(f, 'File created by Ising.EXE');
                  writeln(f, L.x : 3,' ', L.y : 3);
                  if flag.run <> Demon then
                     writeln(f, T : 6 : 3)
                  else
                     writeln(f, Ed : 6 : 3);
                  writeln(f, H : 6 : 3);
                  writeln(f);
                  for j := 1 to L.y do
                     begin
                        for i := 1 to L.x do
                           begin
                              if spin[i, j].dir = -1 then
                                 temp := 0
                              else
                                 temp := 1;
                              write(f, temp : 2);
                           end;
                        writeln(f, '');
                     end;
                  writeln(f, '');
                  if (flag.run <> Wolff) then
                     begin
                        write(f, 'nmcs =');
                        writeln(f, nmcs : 4)
                     end
                  else
                     begin
                        write(f, 'nflip =');
                        writeln(f, nflip : 4)
                     end;
                  writeln(f, 'mean values');
                  if flag.run <> wolff then
                     ntemp := nmcs
                  else
                     ntemp := nflip;
                  tempbar := average.Mcum / ntemp;
                  tempbar := tempbar / N;
                  write(f, tempbar : 6 : 6);
                  writeln(f, '     mean magnetization');
                  tempbar := average.Ecum / ntemp;
                  tempbar := tempbar / N;
                  write(f, tempbar : 6 : 6);
                  writeln(f, '     mean energy per particle');
                  writeln(f, '');
                  tempbar := average.E2cum / ntemp;
                  writeln(f, 'energy histogram');
                  for i := -200 to 200 do
                     writeln(f, i, P.E[i]);
                  writeln(f, '');
                  writeln(f, 'correlation function');
                  for i := 1 to C.nsave[flag.run] do
                     writeln(f, i, C.e[i]);
                  close(f);
               end;
            done;
         end;  {with}
   end;   {savedata}

   procedure InitializeVariables;
      var
         ibin, diff: integer;
   begin
      ratio := 0;
      nmcs := 0;
      nflip := 0;
      with average do
         begin
            Ecum := 0;
            E2cum := 0;
            Mcum := 0;
            M2cum := 0;
            Mabscum := 0;
            Edcum := 0;
            Ed2cum := 0;
         end;
      for ibin := -200 to 200 do
         begin
            P.E[ibin] := 0;
            P.M[ibin] := 0;
         end;
      for diff := 1 to C.nsave[flag.run] do
         begin
            C.e[diff] := 0;
            C.m[diff] := 0;
         end;
      P.xmax := 0;
      P.xmin := 0;
      P.max := 0;
   end;        {InitializeVariables}

   procedure PowerofTwo (var varpass: integer);
      var
         roundoff, dum: real;
         lower, upper: integer;
   begin
      roundoff := varpass;
      if roundoff > 0 then
         begin
            roundoff := ln(abs(roundoff)) / ln(2);
            roundoff := sgn(varpass) * trunc(roundoff);
            dum := pwr(2, roundoff);
            lower := trunc(dum);
            roundoff := pwr(2, roundoff + 1);
            upper := trunc(roundoff);
            if abs(varpass - lower) < abs(upper - varpass) then
               varpass := lower
            else
               varpass := upper;
         end
      else
         varpass := 0;
   end;  {PowerofTwo}

   procedure SetupMagnetizationDisplay;
{      var
         TicX, TicY: real;}
   begin
      DefineViewPort(6, 0.07, 0.5, 0.68, 0.88);
      OpenViewPort(6);
      setcolor(white);
      DefineScale(6, plot.tmin, plot.tmax, -plot.Mmax, plot.Mmax);
      SelectScale(6);
      NewAxis;
      print(1, 1, 'M');
      if flag.run = Wolff then
         PutLabel(bottom, 'nflip')
      else
         PutLabel(bottom, 'mcs');
   end;   {SetupMagnetizationDisplay}

   procedure SetupEnergyDisplay;
      var
         {TicX, TicY,} LowerLimit, UpperLimit: real;
         cycle, ntemp : integer;
   begin
      DefineViewPort(6, 0.07, 0.5, 0.68, 0.88);
      OpenViewPort(6);
      setcolor(white);
      if flag.run = Wolff then ntemp := abs(nflip)
      else ntemp := abs(nmcs);
      cycle := ntemp div (plot.tmax0 - plot.tmin0);
      plot.tmax := (1 + cycle) * plot.tmax0;
      plot.tmin := cycle * plot.tmax0;
      if flag.run <> demon then
         begin
            DefineScale(6, plot.tmin, plot.tmax, plot.emin, plot.emax);
            SelectScale(6);
            NewAxis;
            print(3, 1, 'E')
         end
      else
         begin
            if plot.emax = 0 then
               upperlimit := abs(plot.emin)
            else
               UpperLimit := plot.emax;
            if UpperLimit=0 then begin
               Announce('Error in Demon algorithm: wrong initial conditions.');
               halt(1);
            end;
            DefineScale(6, plot.tmin, plot.tmax, 0, UpperLimit);
            SelectScale(6);
            NewAxis;
            print(3,1,'Ed');
         end;
      if flag.run = Wolff then PutLabel(bottom, 'nflip')
      else PutLabel(bottom, 'mcs');
   end;   {SetupEnergyDisplay}

   procedure ShowEnergyDistribution (ntemp: longint);
      var
         ibin, xsc, ysc: integer;
         pbar: real;
         reset: boolean;
   begin
      reset := false;
      if ntemp <= 0 then
         begin
            reset := true;
            ntemp := -ntemp
         end;
      if reset then   {call when switch windows}
         begin
            DefineViewPort(7, 0.56, 1.0, 0.28, 0.67);
         end
      else
         DefineViewPort(7, 0.56, 1.0, 0.34, 0.67);
      OpenViewPort(7);           {open window to cover axis}
      setcolor(black);
      CloseViewPort(7);
      DefineViewPort(7, 0.62, 0.97, 0.34, 0.65);
      OpenViewPort(7);
      if ntemp <> 0 then
         begin
            P.max := 0;
        {normalize energy distribution}
            for ibin := P.xmin to P.xmax do
               begin
                  if P.E[ibin] > 0 then
                     begin
                        pbar := P.E[ibin] / ntemp;
                        if pbar > P.max then
                           P.max := pbar;
                     end;   {if statement}
               end;   {loop}
         end
      else
         P.max := 1;
      OpenViewPort(7);
      setcolor(white);
      if (flag.run <> demon) then
         begin
            DefineScale(7, plot.emin, plot.emax, 0, P.max);
            SelectScale(7);
            NewAxis;
            PutLabel(inside, 'P(E)');
            PutLabel(bottom, '    E');
         end
      else
         begin
            DefineScale(7, 0, plot.emax, 0, P.max);
            SelectScale(7);
            NewAxis;
            PutLabel(inside,'P(Ed)');
            Putlabel(bottom,'    Ed');
         end;
      SetColor(yellow);
      setfillstyle(solidfill,yellow);
      for ibin := P.xmin to P.xmax do   {plot energy distribution}
         begin
            if P.E[ibin] > 0 then
               begin
                  pbar := P.E[ibin] / ntemp;
                  Map(4 * ibin, pbar, xsc, ysc);
                  fillellipse(xsc, ysc, rsc1,rsc1);
               end;
         end;   {loop}
   end;   {ShowEnergyDistribution}

   procedure ShowMagnetizationDistribution (ntemp: longint);
      var
         ibin, xsc, ysc, plotN: integer;
         pbar{, TicX, TicY}: real;
         reset: boolean;
   begin
      reset := false;
      plotN := N;
      if plotN > 200 then
         plotN := 200;
      if ntemp <= 0 then
         begin
            reset := true;
            ntemp := -ntemp
         end;
      if ntemp <> 0 then
         begin
            P.max := 0;
       {normalize magnetization distribution}
            for ibin := -plotN to plotN do
               begin
                  if P.m[ibin] > 0 then
                     begin
                        pbar := P.m[ibin] / ntemp;
                        if pbar > P.max then
                           P.max := pbar;
                     end;   {if statement}
               end;   {loop}
         end
      else
         P.max := 1;
      if reset then
         begin
            DefineViewPort(7, 0.56, 1.0, 0.28, 0.67);
            OpenViewPort(7);           {open window to cover axis}
            setcolor(black);
            CloseViewPort(7);
            DefineViewPort(7, 0.62, 0.97, 0.34, 0.65);
         end;
      OpenViewPort(7);
      SetColor(white);
      DefineScale(7, -plot.Mmax, plot.Mmax, 0, 1.1*P.max);
      SelectScale(7);
      NewAxis;
      PutLabel(inside, 'P(M)');
      PutLabel(bottom, '    M');
      SetColor(yellow);
      setfillstyle(solidfill,yellow);
      if ntemp > 0 then
         begin
            for ibin := -plotN to plotN do   {normalize energy distribution}
               begin
                  pbar := P.m[ibin] / ntemp;
                  map(2 * ibin, pbar, xsc, ysc);
                  fillellipse(xsc, ysc, rsc1,rsc1);
               end;   {plot}
         end;
   end;   {ShowMagnetizationDistribution}

   procedure ShowColorTable;
      var
         xpix, ypix: integer;
   begin
      DefineViewPort(5, 0.51, 0.63, 0.15, 0.24);
      OpenViewPort(5);           {animation window}
      with Views[5] do
         begin
            xpix := vx2 - vx1;
            ypix := vy2 - vy1;
         end;
      Setcolor(white);
      SetFillStyle(solidfill, color.up);
      HideMouse;
      bar(2, 2, 12, 12);
      OutTextXY(16, 2, 'spin up');
      SetFillStyle(solidfill, color.down);
      bar(2, 15, 12, 25);
      OutTextXY(16, 15, 'spin down');
      ShowMouse;
   end;       {ShowColorTable}

   procedure ShowEnergy (ntemp: longint);
      var
         xsc, ysc, delta, cycle: integer;
         reset: boolean;
         Ebar: real;
   begin
      reset := false;
      if ntemp < 0 then
         begin
            ntemp := -ntemp;
            if nmcs < 0 then
               nmcs := -nmcs;
            if nflip < 0 then
               nflip := -nflip;
            reset := true;
         end;
      cycle := ntemp div (plot.tmax0 - plot.tmin0);
      delta := ntemp mod (plot.tmax0 - plot.tmin0);
      if (delta = 0) or (ntemp = 0) or reset then
         begin
            DefineViewPort(6, 0.0, 0.53, 0.625, 0.90);
            OpenViewPort(6);           {open window to cover axis}
            setcolor(black);
            CloseViewPort(6);
            plot.tmax := (1 + cycle) * plot.tmax0;
            plot.tmin := cycle * plot.tmax0;
            SetUpEnergyDisplay;
         end;
      SelectViewPort(6);
      SelectScale(6);
      SetColor(lightgreen);
      SetFillStyle(solidfill,lightgreen);
      Map(ntemp, E, xsc, ysc);
      fillellipse(xsc, ysc, rsc2,rsc2);
      if ntemp > 0 then
         begin
            Ebar := average.Ecum / ntemp;
            SetFillStyle(solidfill,yellow);
            SetColor(yellow);
            Map(ntemp, Ebar, xsc, ysc);
            setfillstyle(solidfill,yellow);
            fillellipse(xsc, ysc, rsc2,rsc2);
         end;
   end;    {ShowEnergy}

   procedure ShowDemonEnergy;
      var
         xsc, ysc, delta, cycle: integer;
         reset: boolean;
         Edbar: real;
   begin
      reset := false;
      if nmcs < 0 then
         begin
            nmcs := -nmcs;
            reset := true;
         end;
      cycle := nmcs div (plot.tmax0 - plot.tmin0);
      delta := nmcs mod (plot.tmax0 - plot.tmin0);
      if (delta = 0) or (nmcs = 0) or reset then
         begin
            DefineViewPort(6, 0.0, 0.53, 0.625, 0.90);
            OpenViewPort(6);           {open window to cover axis}
            setcolor(black);
            CloseViewPort(6);
            plot.tmax := (1 + cycle) * plot.tmax0;
            plot.tmin := cycle * plot.tmax0;
            SetUpEnergyDisplay;
         end;
      SelectViewPort(6);
      SelectScale(6);
      SetColor(lightGreen);
      setfillstyle(solidfill,lightgreen);
      Map(nmcs, Ed, xsc, ysc);
      fillellipse(xsc, ysc, rsc2,rsc2);
      if nmcs > 0 then
         begin
            Edbar := average.Edcum / nmcs;
            SetColor(yellow);
            Map(nmcs, Edbar, xsc, ysc);
            SetFillstyle(solidfill,yellow);
            fillellipse(xsc, ysc, rsc2,rsc2);
         end;
   end;    {ShowDemonEnergy}

   procedure ShowMagnetization (ntemp: longint);
      var
         xsc, ysc, delta, cycle: integer;
         reset: boolean;
         Mbar: real;
   begin
      reset := false;
      if ntemp < 0 then
         begin
            ntemp := -ntemp;
            reset := true;
         end;
      cycle := ntemp div (plot.tmax0 - plot.tmin0);
      delta := ntemp mod (plot.tmax0 - plot.tmin0);
      if (delta = 0) or (ntemp = 0) or reset then
         begin
            DefineViewPort(6, 0.0, 0.53, 0.625, 0.90);
            OpenViewPort(6);           {open window to cover axis}
            setcolor(black);
            CloseViewPort(6);
            plot.tmax := (1 + cycle) * plot.tmax0;
            plot.tmin := cycle * plot.tmax0;
            SetUpMagnetizationDisplay;
         end;
      SelectViewPort(6);
      SelectScale(6);
      SetColor(lightgreen);
      setfillstyle(solidfill,lightgreen);
      Map(ntemp, M, xsc, ysc);
      fillellipse(xsc, ysc, rsc2,rsc2);
      if ntemp > 0 then
         begin
            Mbar := average.Mcum / ntemp;
            SetColor(yellow);
            setfillstyle(solidfill,yellow);
            Map(ntemp, Mbar, xsc, ysc);
            fillellipse(xsc, ysc, rsc2,rsc2);
         end;
   end;    {ShowMagnetization}

   procedure ChangeDisplayOptions;
      var
         DisplayScreen: TInputScreen;
         H: HelpScrType;
         up, down, i: integer;
         min, max : integer;
         xchange, xchangee, energyOK: boolean;
   begin
      xchange := false;
      xchangee := false;
      energyOK := true;
      with DisplayScreen do
      begin
         Init;
         if not flag.renorm and not flag.demo then
         begin
            if flag.run <> demon then
               DefineInputPort(0.01, 0.76, 0.33, 0.89)
            else
               DefineInputPort(0.01, 0.76, 0.38, 0.89);
         end
         else DefineInputPort(0.01, 0.74, 0.58, 0.89);
         up := color.up;
         ConvertColor(up);
         down := color.down;
         ConvertColor(down);
         Loadline('color of up spins:');
         loadline('#1 red   #1 lightred  #1 blue   #1 lightblue  #1 green');
         Loadline('color of down spins:');
         loadline('#2 red   #2 lightred  #2 blue   #2 lightblue  #2 green');
         setRadioButton('1', up);
         setRadioButton('2', down);
         LoadLine('');
         if not flag.renorm and not flag.demo then
         begin
            loadline('time interval for E(t) and M(t) plots   = {      }');
            setnumber(11, (plot.tmax - plot.tmin));
            setnumberlimits(11, 10, 1000);
            loadline('max energy for E(t) and P(E) plots      = {      }');
            setnumber(12, plot.emax);
            if flag.run <> demon then
            begin
               loadline('min energy for E(t) and P(E) plots      = {      }');
               setnumber(13, plot.emin);
               i := 14;
            end else begin
               setnumberlimits(12, 1e-10, 1e10);
               i := 13;
            end;
            loadline('max value of M for M(t) and P(M) plots  = {      }');
            setnumber(i, N);
            loadline('max difference for correlation function = {      }(200)');
            setnumber(i + 1, C.nmax[flag.run]);
         end;
         loadline('');
         loadline('     [  Ok  ]   [Cancel] ');
         repeat
            AcceptScreen;
            if not canceled then
            begin
               if up <> getradiobutton('1') then
               begin
                  up := getradiobutton('1');
                  ReConvertColor(up);
                  color.up := up;
                  xchange := true;
               end;
               if down <> getradiobutton('2') then
               begin
                  down := getradiobutton('2');
                  ReConvertColor(down);
                  color.down := down;
                  xchange := true;
               end;
               if xchange then
               begin
                  ShowSpins;
                  flag.show := true;
                  xchange := false;
               end;
               if not flag.renorm and not flag.demo then
               begin
                  if (plot.tmax0 <> trunc(getnumber(11))) then
                  begin
                     plot.tmax0 := trunc(getnumber(11));
                     xchange := true;
                  end;
                  max := trunc(getnumber(12));
                  if (flag.run <> demon) then min := trunc(getnumber(13))
                  else min := 0;
                  if plot.emax <> max then
                  begin
                     PowerOfTwo(max);
                     xchangee := true;
                  end;
                  if (flag.run <> demon) and (plot.emin <> min) then
                  begin
                     PowerOfTwo(min);
                     xchangee := true;
                  end;
                  if (max<=min) then begin
                     energyOk := false;
                     setnumber(12, plot.emax);
                     if flag.run<>demon then setnumber(13, plot.emin);
                  end else begin
                     energyOk := true;
                     plot.emin := min;
                     plot.emax := max;
                     if (xchange or xchangee) and flag.E then
                     begin
                        if (flag.run = Metro) or (flag.run = Kawasaki) then
                           ShowEnergy(-nmcs)
                        else if (flag.run = wolff) then
                           ShowEnergy(-nflip)
                        else if (flag.run = demon) then
                           begin
                              nmcs := -nmcs;
                              ShowDemonEnergy;
                           end;
                     end;
                     if xchangee and flag.P then
                     begin
                        if flag.run <> Wolff then
                           ShowEnergyDistribution(-nmcs)
                        else
                           ShowEnergyDistribution(-nflip);
                     end;
                     xchangee := false;
                     if plot.Mmax <> getnumber(i) then
                     begin
                        plot.Mmax := trunc(getnumber(i));
                        PowerOfTwo(plot.Mmax);
                        xchangee := true;
                     end;
                     if (xchange or xchangee) and not flag.E then
                     begin
                        if (flag.run <> wolff) then
                           ShowMagnetization(-nmcs)
                        else
                           ShowMagnetization(-nflip);
                     end;
                     if xchangee and not flag.P then
                     begin
                        if flag.run <> Wolff then
                           ShowMagnetizationDistribution(-nmcs)
                        else
                           ShowMagnetizationDistribution(-nflip);
                     end;
                     if C.nmax[flag.run] <> getnumber(i + 1) then
                     begin
            {                  RedrawCorrelationFunctionAxis;}
                        C.nmax[flag.run] := trunc(getnumber(i + 1));
                        if flag.run <> Wolff then
                           ShowCorrelationFunction(-nmcs)
                        else
                           ShowCorrelationFunction(-nflip);
                     end;
                  end; { if max>min }
               end; {if not flag.remorm}
            end; {not canceled}
            if (not energyOK) then
            announce('Maximum energy has to be bigger then minimum energy');
         until (energyOK);
         done;
      end;
   end;     {ChangeDisplayOptions}

   procedure CheckWindows (ntemp: longint);
      var
         ixdum, iydum, button: integer;
         xdum, ydum: real;
         inside6, inside7, inside8: boolean;
   begin
      inside6 := false;  {E/M windows}
      inside7 := false;   {P(E), P(M) distribution functions}
      inside8 := false;  {correlation function}
      if Event.MouseClicked then
         begin
            SelectViewport(6);
            MousePosn(xdum, ydum, 6, 6, inside6);    {E/M window}
            SelectViewport(7);
            MousePosn(xdum, ydum, 7, 7, inside7);    {Prob window}
            Selectviewport(8);
            MousePosn(xdum, ydum, 8, 8, inside8);    {Correlfn}
            if inside6 then
               begin
                  if flag.E then
                     begin
                        flag.E := false;
                        ShowMagnetization(-ntemp)
                     end
                  else
                     begin
                        flag.E := true;
                        ShowEnergy(-ntemp);
                     end;
               end;   {flag.E}
         end;  {if inside6}
      if inside8 then   {correlation function}
         begin
            if flag.ce then
               flag.ce := false
            else
               flag.ce := true;
            ShowCorrelationFunction(-ntemp)
         end;
      if inside7 then
         begin
            if flag.P then
               begin
                  flag.P := false;
                  ShowMagnetizationDistribution(-ntemp);
               end
            else
               begin
                  flag.P := true;
                  ShowEnergyDistribution(-ntemp);
               end;
         end;
   end;   {CheckWindows}

   procedure ShowData (ntemp: longint);  {called by Metro and Wolff}
      var
         Ebar, E2bar, C, Mbar, M2Bar, chi, Ndiv, Ndiv2: real;
         BkColor, line: integer;
   begin
      SelectViewPort(3);
      SetColor(lightgreen);
      Ndiv := 1 / N;
      BkColor := GraphBackColor;
      rubout(9, 1, 7, BkColor);
      if ntemp=0 then rubout(1, 1, 15, BkColor);
      if flag.run = Wolff then
         print(1, 1, concat('nflip = ', NumStr(ntemp, 3, 0)))
      else
         print(1, 1, concat(' mcs  = ', NumStr(ntemp, 3, 0)));
      if ntemp = 1 then
         begin
            rubout(1, 1, 8, BkColor);
            rubout(1, 2, 8, BkColor);
            rubout(1, 3, 8, BkColor);
         end;
      if ntemp > 0 then
         begin
            SetColor(yellow);
            Ebar := average.Ecum / ntemp;
            Ebar := Ebar * Ndiv;
            rubout(9, 2, 7, BkColor);
            print(1, 2, concat('<E/N> = ', NumStr(Ebar, 3, 2)));
            Mbar := average.Mcum / ntemp;
            Mbar := Mbar * Ndiv;
            rubout(9, 3, 7, BkColor);
            print(1, 3, concat('<M/N> = ', NumStr(Mbar, 3, 3)));
            E2bar := average.E2cum / ntemp;
            line := 1;
            if T > 0 then
               begin
                  C := (E2bar - Ebar * Ebar) / (T * T);
                  Ndiv2 := Ndiv * Ndiv;
                  C := C * Ndiv2;
                  rubout(9, 4, 7, BkColor);
                  print(1, 4, concat(' C    = ', NumStr(C, 3, 2)));
                  M2bar := average.M2cum / ntemp;
                  chi := (M2bar - Mbar * Mbar) / T;
                  chi := chi * Ndiv2;
                  rubout(9, 5, 7, BkColor);
                  print(1, 5, concat(' chi  = ', NumStr(chi, 3, 2)));
               end;
         end
      else  {ntemp = 0}
         begin
            SetColor(lightgreen);
            print(1, 2, concat(' E/N  = ', NumStr(E * Ndiv, 3, 2)));
            print(1, 3, concat(' M/N  = ', NumStr(M * Ndiv, 3, 2)));
         end;
   end;     {ShowData}

   procedure SetupSpinDisplay;
      var
         Lmax: integer;
   begin
      DefineViewPort(2, 0.01, 0.5, 0.05, 0.62);
      SelectViewPort(2);           {animation window}
      Lmax := imax(L.x, L.y);
      DefineScale(2, 0, Lmax, 0, Lmax);
      SelectScale(2);
      with Views[2] do
         begin
            pixels.x := vx2 - vx1;
            pixels.y := vy2 - vy1;
         end;
   {draw spins such that spin[16,16] at extreme right and next to hot keys.}
      pixels.x := pixels.x div Lmax;     {width of spin in pixels}
      pixels.y := pixels.y div Lmax;
   end;    {SetupSpinDisplay}

   procedure SetupDataDisplay;
   begin
      DefineViewPort(3, 0.78, 0.98, 0.05, 0.27);    {data window}
      OpenViewPort(3);
   end;   {SetupDataDisplay}

   procedure SetupProbabilityDisplay;
   begin
      DefineViewPort(7, 0.62, 0.97, 0.34, 0.65);
      OpenViewPort(7);
      SetColor(white);
      if (flag.run <> demon) then
         begin
            DefineScale(7, plot.emin, plot.emax, 0, 1.1);
            SelectScale(7);
            NewAxis;
            PutLabel(inside, 'P(E)');
            PutLabel(bottom, '    E');
         end
      else
         begin
            DefineScale(7, 0, plot.emax, 0, 1.1);
            SelectScale(7);
            NewAxis;
            PutLabel(inside, 'P(Ed)');
            PutLabel(bottom,'    Ed');
         end
   end;   {SetupProbabilityDisplay}

   procedure SetupCorrelationDisplay;
{      var
         TicX, TicY: real;}
   begin
      DefineViewPort(8, 0.62, 0.97, 0.73, 0.94);
      OpenViewPort(8);
      SetColor(white);
      DefineScale(8, 0, C.nmax[flag.run], 0, 1.1);
      SelectScale(8);
      NewAxis;
      setcolor(graphbackcolor);
      PutLabel(inside, ' '+chr(219));
      SetColor(white);
      if flag.Ce then PutLabel(inside, 'Ce')
      else PutLabel(inside,'Cm');
      PutLabel(bottom, '    time difference');
   end;   {SetupCorrelationDisplay}


   procedure ShowStatus;
      var
         i: integer;
   begin
      if not flag.renorm then
         begin
            SetColor(white);
            for i := 0 to 1 do
               begin
                  SelectViewport(7 + i);
                  print(1, 1, 'computing ...');
               end;
         end;
   end;  {ShowStatus}

   procedure CalculateCorrelationFunction (Elocal: real; ntemp: longint);
      var
         idiff: integer;
         X: real;
   begin
      X := abs(M);
      if (ntemp > C.nsave[flag.run]) then
         begin
            for idiff := 1 to C.nsave[flag.run] do
               begin
                  C.e[idiff] := C.e[idiff] + C.esave[C.nsave[flag.run] + 1 - idiff] * Elocal;
                  C.m[idiff] := C.m[idiff] + C.msave[C.nsave[flag.run] + 1 - idiff] * X;
               end;
            for idiff := 1 to C.nsave[flag.run] - 1 do
               begin
                  C.esave[idiff] := C.esave[idiff + 1];
                  C.msave[idiff] := C.msave[idiff + 1];
               end;
            C.esave[C.nsave[flag.run]] := Elocal;
            C.msave[C.nsave[flag.run]] := X;
         end
      else
         begin
            C.esave[ntemp] := Elocal;  {subrange error after finish renormalization}
            C.msave[ntemp] := M
         end;
   end; {CalculateCorrelationFunction}

   procedure CalculateProbability;
      var
         ibin: integer;
   begin   {compute probability every Monte Carlo step}
      ibin := trunc(E) div 4;
      if ibin > 200 then
         ibin := 200;
      if ibin < -200 then
         ibin := -200;
      P.E[ibin] := P.E[ibin] + 1;
      if ibin > P.xmax then   {rescale probability scale}
         P.xmax := ibin;
      if ibin < P.xmin then
         P.xmin := ibin;
      ibin := trunc(M) div 2;
      if ibin > 200 then
         ibin := 200;
      if ibin < -200 then
         ibin := -200;
      P.m[ibin] := P.m[ibin] + 1;
   end;     {CalculateProbability}

   procedure pbc (var xpass, ypass: integer; var Lt: length; var sumtemp: integer);
 {determine sum of neighbor values using periodic boundary conditions}
      var
         left, right, up, down: integer;
   begin
      if (xpass = 1) then
         begin
            left := spin[Lt.x, ypass].dir;
            if (Lt.x = 1) then
               left := 0;
         end
      else
         left := spin[xpass - 1, ypass].dir;
      if (xpass = Lt.x) then
         begin
            right := spin[1, ypass].dir;
            if (Lt.x = 1) then
               right := 0;
         end
      else
         right := spin[xpass + 1, ypass].dir;
      if (ypass = 1) then
         begin
            down := spin[xpass, Lt.y].dir;
            if (Lt.y = 1) then
               down := 0;
         end
      else
         down := spin[xpass, ypass - 1].dir;
      if (ypass = Lt.y) then
         begin
            up := spin[xpass, 1].dir;
            if (Lt.y = 1) then
               up := 0;
         end
      else
         up := spin[xpass, ypass + 1].dir;
      sumtemp := left + right + up + down;
   end;

   procedure Accumulate;
  {accumulate data after every Monte Carlo step per spin or cluster flip}
   begin
      with average do
         begin
            Ecum := Ecum + E;
            E2cum := E2cum + E * E;
            Mcum := Mcum + M;
            M2cum := M2cum + M * M;
            Mabscum := Mabscum + abs(M);
         end;
   end;    {Accumulate}

   procedure ShowChange (i, j: integer);
   forward;

   procedure Edit;
      var
         xpix, ypix, xsc, ysc, sum, i, j, ndir, Lmax, colr: integer;
         xdum, ydum, de: real;
         inside: boolean;
         knum : byte;
   begin
      ClearMuppetport;
      SetUpEditHotKeys;
      Lmax := imax(L.x, L.y);
      DefineViewPort(9, 0, 1.0, 0.05, 0.95);
      OpenViewPort(9);
      DefineScale(9, 0.5, Lmax + 0.5, 0.5 + Lmax, 0.5);
      SelectScale(9);
      Comment(0, 0.95, 'Click on spin to flip it. Press F10 to return to simulation.');
      Comment(0.7, 1, concat('E = ', NumStr(E, 3, 0)));
      with Views[9] do
         begin
            xpix := vx2 - vx1;
            ypix := vy2 - vy1;
         end;
      SetFillStyle(solidfill, black);
      HideMouse;
      bar(0, 0, Lmax * xpix, Lmax * ypix);
      rectangle(0, 0, Lmax * xpix, Lmax * ypix);
      ShowMouse;
      setcolor(white);
      xpix := xpix div Lmax;
      ypix := ypix div Lmax;
      HideMouse;
      for j := 1 to L.y do  {draw original configuration}
         for i := 1 to L.x do
            begin
               if spin[i, j].dir > 0 then colr := color.up
               else colr := color.down;
               SetFillStyle(solidfill, colr);
               if hideGrid then SetColor(colr);
               xsc := (i - 1) * xpix;
               ysc := (j - 1) * ypix;
               bar(xsc, ysc, xsc + xpix, ysc + ypix);
               rectangle(xsc, ysc, xsc + xpix, ysc + ypix);
            end;   {draw lattice}
      ShowMouse;
      with Event do
         repeat
            inside := false;
            MouseClicked := false;
            repeat
               CheckForEvents;
               MousePosn(xdum, ydum, 9, 9, inside);
            until (inside and MouseClicked) or HotKeys.pressed(knum);
            if (knum = 1) then
               HelpHotKeys;
            if MouseClicked and inside then
               begin
                  xsc := (x-Views[9].vx1) div xpix + 1;
                  ysc := (y-Views[9].vy1) div ypix + 1;
                  pbc(xsc, ysc, L, sum);
                  de := 2 * spin[xsc, ysc].dir * (sum + h);
                  E := E + de;
                  spin[xsc, ysc].dir := -spin[xsc, ysc].dir;    {flip spin}
                  if spin[xsc, ysc].dir > 0 then colr := color.up
                  else colr := color.down;
                  SetFillStyle(solidfill, colr);
                  if hideGrid then SetColor(colr);
                  xsc := (xsc - 1) * xpix;
                  ysc := (ysc - 1) * ypix;
                  hideMouse;
                  bar(xsc, ysc, xsc + xpix, ysc + ypix);
                  rectangle(xsc, ysc, xsc + xpix, ysc + ypix);
                  ShowMouse;
                  comment(0.7, 1, '     ');
                  comment(0.7, 1, concat('E = ', NumStr(E, 3, 0)));
               end;
         until (knum = 2);
{      start := true;}
      flag.change := true;
      flag.edit := true;
      CloseViewPort(9);
      ClearMuppetPort;
   end;    {Edit}

   procedure ShowBlockSpins (x, y: integer);
   forward;

   procedure RescaleSpinsAfterdemo;
      var
         x, y: integer;
   begin
      for y := 1 to L.y do
         begin
            for x := 1 to L.x do
               begin
                  if (spin[x, y].dir > 1) then
                     spin[x, y].dir := 1
                  else if spin[x, y].dir < -1 then
                     spin[x, y].dir := -1;
               end    {loop over x}
         end; {loop over y}
   end;   {RescaleSpinsAfterDemo}

   procedure CheckEnergy;
      var
         i, j, up, right, sum: integer;
         Etemp: real;
   begin
     {compute initial energy E}
      Etemp := 0;
      for j := 1 to L.y do
         begin
            if (j = L.y) then
               up := 1
            else
               up := j + 1;
            for i := 1 to L.x do
               begin
                  if (i = L.x) then
                     right := 1
                  else
                     right := i + 1;
                  sum := spin[i, up].dir + spin[right, j].dir;
                  Etemp := Etemp - spin[i, j].dir * sum;        {total energy}
               end
         end;
      if (Etemp <> E) then
         begin
            OpenViewPort(7);
            SelectScale(7);
            SetColor(lightgreen);
            print(1, 1, 'inconsistency in energy calculation');
            print(1, 2, concat('Etemp = ', NumStr(Etemp, 3, 3)));
            print(1, 3, concat('E = ', NumStr(E, 3, 3)))
         end
   end;   {checkenergy}

   procedure new_perimeter (var x, y: integer);
      var
         i, nnx, nny, perim: integer;
         xsc, ysc: integer;
   begin
      for i := 1 to 4 do       {find perimeter sites of site x,y}
         begin
            nnx := x + nx[i];
            nny := y + ny[i];
            if nnx > L.x then
               nnx := 1;
            if nnx < 1 then
               nnx := L.x;
            if nny > L.y then
               nny := 1;
            if nny < 1 then
               nny := L.y;
            perim := spin[nnx, nny].dir;
            if (perim * ndir > 0) then
               begin
                  nper := nper + 1;
                  perx[nper] := nnx;
                  pery[nper] := nny;
               end;
         end;   {end loop}
   end;     {new_perimeter}

   procedure ShowGrowCluster (var x, y: integer);
      var
         xsc, ysc, px, py: integer;
   begin
      SelectViewPort(2);
      SelectScale(2);
      SetColor(white);
      xsc := (x - 1) * pixels.x;
      ysc := (y - 1) * pixels.y;
      px := pixels.x;
      py := pixels.y;
      ShowSpin(-ndir, xsc, ysc, px, py);
      if flag.renorm then
         ShowBlockSpins(x, y);
   end;   {ShowGrowCluster}

   procedure grow;
      var
         occup, dm, ntest: integer;
         r: real;
         xsc, ysc: integer;
         sum, DeltaE: integer;
         x, y: integer;
   begin   {add percolation bonds with probability pb}
      x := trunc(L.x * ran(seed)) + 1;   {choose initial spin at random}
      y := trunc(L.y * ran(seed)) + 1;
      ndir := spin[x, y].dir;   {ndir direction of seed}
      occup := -ndir;
      pbc(x, y, L, sum);
      DeltaE := 2 * ndir * sum;    {change in energy}
      dm := 2 * occup;  {change in magnetization}
      spin[x, y].dir := -ndir;
      if flag.show then
         ShowGrowCluster(x, y);
      M := M + dm;
      E := E + deltaE;
      nper := 0;     {number of perimeter sites}
      new_perimeter(x, y);
      repeat      {begin growing cluster}
         if nper > 0 then
            begin    {try to add bond to "first" perimeter site}
               x := perx[1];             {location of perimeter site}
               y := pery[1];
               perx[1] := perx[nper];
               pery[1] := pery[nper];
               nper := nper - 1;
               if spin[x, y].dir <> occup then       {spin not already flipped}
                  begin
                     r := ran(seed);
                     if r < pb then
                        begin   {site added to cluster}
                           pbc(x, y, L, sum);
                           DeltaE := 2 * ndir * sum;
                           spin[x, y].dir := occup;    {flip spin}
                           E := E + deltaE;
                           M := M + dm;    {change in magnetization}
                           if flag.show then
                              ShowGrowCluster(x, y);
                           new_perimeter(x, y);
                        end
                  end;
            end;   {if nper > 0}
      until (nper < 1);
   end;    {grow}

   procedure SetUpWindowsRunWolff;
   begin
      ClearMuppetPort;
      menu.display;{Added}
      Comment(0, 0.95, 'Wolff single cluster algorithm');
      if flag.rg then
         Menu.RowActivate(4, 1, true);
      if flag.demo then
         begin
            ClearDemoMenuCheck;
         end;
      InitializeVariables;
      SetupSpinDisplay;
      Bonds;
      SetupHotKeys;
      TemperatureSlider.Reset(T);
      TemperatureSlider.Draw;
      ShowSpins;
      ComputeMinMaxEnergy;
      SetupEnergyDisplay;
      SetupCorrelationDisplay;
      SetupDataDisplay;
      ShowData(nflip);
      SetupProbabilityDisplay;
   end;  {SetUpWindowsRunWolff}

   procedure InitialRunWolff;
   begin
      flag.show := true;
      if flag.demo then
         RescaleSpinsAfterDemo;
      InitialE;
      nper := 0;
   end;  {InitialRunWolff}

   procedure RunWolff;
      var
         Tnew: real;
         kNum : byte;
   begin
      if flag.renorm then
         flag.exit := true;
      if flag.change or not start then
         repeat
            flag.change := false;
            if flag.edit then
               begin
                  flag.edit := false;
                  start := false;
               end;
            CheckForEvents;
            if Event.MouseClicked then
               begin
                  HandleMouse;
                  if not flag.renorm then
                     begin
                        CheckWindows(nflip);
                        Tnew := TemperatureSlider.value;
                        if (Tnew <> T) then
                           HandleSliders(Tnew, 0);
                     end;
               end;
            if hotkeys.pressed(knum) then begin
               HandleHotKeys(knum);
               if knum=5 then SetUpWindowsRunWolff;
            end;
            if Menu.activated then
               begin
                  flag.exit := true;
                  handlemenu;
               end;
         until (Event.KeyPressed or Event.MouseClicked or flag.exit);
      if start and not quit then
         begin
            SaveActivate;
            repeat
               flag.exit := false;
               if nflip = 0 then
                  ShowStatus;
               grow;
               nflip := nflip + 1;
               Accumulate;
               CalculateProbability;
               CalculateCorrelationFunction(E, nflip);
               CheckForEvents;
               if not flag.renorm then
                  begin
                     ShowData(nflip);
                     CheckWindows(nflip);
                     Tnew := TemperatureSlider.value;
                     if (Tnew <> T) then
                        HandleSliders(Tnew, 0);
                     if flag.E then
                        ShowEnergy(nflip)
                     else
                        ShowMagnetization(nflip);
                     if (nflip mod P.nshow[flag.run] = 0) then
                        begin
                           if (nflip > C.nshow[flag.run]) then
                              ShowCorrelationFunction(nflip);
                           if flag.P then
                              ShowEnergyDistribution(nflip)
                           else
                              ShowMagnetizationDistribution(nflip);
                        end;
                  end;   {flag.renorm}
            until (Event.KeyPressed or Event.MouseClicked) and not flag.exit;
         end;  {if start}
   end;   {RunWolff}

{{$S part3}

   procedure CalculateDemonProbability;
      var
         ibin: integer;
   begin   {compute probability every Monte Carlo step}
      ibin := trunc(Ed) div 4;
      if ibin > 200 then
         ibin := 200;
      P.E[ibin] := P.E[ibin] + 1;
      if ibin > P.xmax then   {rescale probability scale}
         P.xmax := ibin;
      ibin := trunc(M) div 2;
      if ibin > 200 then
         ibin := 200;
      if ibin < -200 then
         ibin := -200;
      P.m[ibin] := P.m[ibin] + 1;
   end;     {CalculateDemonProbability}

   procedure AccumulateDemonEnergy;
   begin
      average.Edcum := Ed + average.Edcum;
      average.Ed2cum := Ed * Ed + average.Ed2cum;
   end;     {AccumulateDemonEnergy}

   procedure ShowDemonData;
      var
         Edbar, Ed2bar, Mbar, M2Bar, Ndiv: real;
         BkColor: integer;
   begin
      SelectViewPort(3);
      SetColor(white);
      Ndiv := 1 / N;
      BkColor := GraphBackColor;
      RowHt := 15;
      SetColor(lightgreen);
      rubout(10,1 , 6, BkColor);
      Print (1 ,1, concat('  mcs  = ', NumStr(nmcs, 3, 0)));
      if nmcs = 1 then
         begin
            rubout(1, 2, 10, BkColor);
            rubout(1, 3, 10, BkColor);   {erase M}
         end;
      if nmcs > 0 then
         begin
            SetColor(yellow);
            Edbar := average.Edcum / nmcs;
            rubout(10, 2, 6, BkColor);
            Print (1 , 2, concat(' <Ed>  = ', NumStr(Edbar, 4, 2)));
            Mbar := average.Mcum / nmcs;
            Mbar := Mbar * Ndiv;
            rubout(10, 3, 6, Bkcolor);
            Print (1 , 3, concat(' <M/N> = ', NumStr(M * Ndiv, 4, 2)));
            M2bar := average.M2cum / nmcs;
            Ed2bar := average.Ed2cum / nmcs;
            rubout(10, 4, 6, BkColor);
            Print (1 , 4, concat('<Ed^2> = ', NumStr(Ed2bar, 4, 1)));
            rubout(10, 5, 6, BkColor);
            print (1 , 5, concat(' <M^2> = ', NumStr(M2bar, 4, 1)));
         end
      else  {time = 0}
         begin
            SetColor(lightgreen);
            print(1,2, concat(' <Ed>  = ', NumStr(Ed,4,2)));
            Print(1,3, concat('  M/N  = ', NumStr(M * Ndiv, 4, 2)));
         end;
      RowHt := 20;
   end;       {ShowDemonData}

   procedure SweepLattice;
      var
         ispin, i, j, sum: integer;
         de: real;
         xsc, ysc: integer;
   begin
      ispin := 0;   {# of spins chosed at random}
      repeat     {randomly select location of spin}
         ispin := ispin + 1;
         i := trunc(L.x * ran(seed)) + 1;
         j := trunc(L.y * ran(seed)) + 1;
         pbc(i, j, L, sum);
         de := 2 * spin[i, j].dir * sum;  {change in energy of system}
         if (de <= Ed) then
            begin   {accept change}
               ndir := spin[i, j].dir;
               spin[i, j].dir := -spin[i, j].dir;
               if flag.show then
                  ShowChange(i, j);
               ratio := ratio + 1;
               Ed := Ed - de;
               E := E + de;
               M := M + 2 * spin[i, j].dir;
            end;
      until (ispin = N);
   end;   {SweepLattice}

   procedure SetUpWindowsRunDemon;
   begin
      ClearMuppetPort;
      menu.display;{Added}
      Comment(0, 0.95, 'Demon algorithm');
      if flag.rg then
         Menu.RowActivate(4, 1, true);
      if flag.demo then
         ClearDemoMenuCheck;
      SetupSpinDisplay;
      ShowSpins;
      SetupHotKeys;
      Hslider.Reset(H);
      Hslider.Draw;
      ComputeMinMaxEnergy;
      SetupEnergyDIsplay;
      SetupCorrelationDisplay;
      SetupDataDisplay;
      ShowDemonData;
      SetupProbabilityDisplay;
   end;   {SetUpWindowsRunDemon}

   procedure InitialRunDemon;
   begin
      flag.show := true;
      InitializeVariables;
      InitialE;
   end;   {InitialRunDemon}

   procedure RunDemon;
      var
         Hnew: real;
         knum : byte;
   begin
      if flag.renorm then
         flag.exit := true;
      if flag.change or not start then
         repeat
            flag.change := false;
            if flag.edit then
               begin
                  flag.edit := false;
                  start := false;
               end;
            CheckForEvents;
            if Event.MouseClicked then
               begin
                  handlemouse;
                  if not flag.renorm then
                     begin
                        CheckWindows(nmcs);
                        Hnew := Hslider.value;
                        if (Hnew <> H) then
                           HandleSliders(0, Hnew);
                     end;
               end;
            if hotkeys.pressed(knum) then begin
               HandleHotKeys(knum);
               if knum=5 then SetUpWindowsRunDemon;
            end;
            if Menu.activated then
               begin
                  flag.exit := true;
                  handlemenu;
               end;
         until (Event.KeyPressed or Event.MouseClicked or flag.exit);
      if start and not quit then
         begin
            SaveActivate;
            repeat
               flag.exit := false;
               if nmcs = 0 then
                  ShowStatus;
               SweepLattice;
               nmcs := nmcs + 1;
               Accumulate;    {accumulate system averages}
               AccumulateDemonEnergy;
               CalculateDemonProbability;
               CalculateCorrelationFunction(Ed, nmcs);
               CheckForEvents;
               if not flag.renorm then
                  begin
                     ShowDemonData;
                     if flag.E then
                        ShowDemonEnergy
                     else
                        ShowMagnetization(nmcs);
                     CheckWindows(nmcs);
                     Hnew := Hslider.value;
                     if (Hnew <> H) then
                        HandleSliders(0, Hnew);
                     if (nmcs mod P.nshow[flag.run] = 0) then
                        begin
                           ShowCorrelationFunction(nmcs);
                           if flag.P then
                              ShowEnergyDistribution(nmcs)
                           else
                              ShowMagnetizationDistribution(nmcs);
                        end;
                  end;  {not flag.renorm}
            until (Event.KeyPressed or Event.MouseClicked) and not flag.exit;
         end;
   end;  {RunDemon}

   procedure ShowChange (i, j: integer);
      var
         xsc, ysc, colr: integer;
   begin   {change sign of spin before show change}
      SelectViewPort(2);
      SelectScale(2);
      xsc := (i - 1) * pixels.x;
      ysc := (j - 1) * pixels.y;
      if ndir > 0 then colr := color.up
      else colr := color.down;
      SetFillStyle(solidfill, colr);
      if hideGrid then SetColor(colr) else SetColor(white);
      HideMouse;
      bar(xsc, ysc, xsc + pixels.x, ysc + pixels.y);
      rectangle(xsc, ysc, xsc + pixels.x, ysc + pixels.y);
      ShowMouse;
      if flag.renorm then ShowBlockSpins(i, j);
   end;    {ShowChange}

   procedure AcceptChange (i, j, sum: integer);
      var
         xsc, ysc: integer;
   begin
      spin[i, j].dir := -spin[i, j].dir;
      ratio := ratio + 1;
      M := M + 2 * spin[i, j].dir;
      E := E - 2 * spin[i, j].dir * sum;
      if flag.show or flag.renorm then
         begin
            ndir := spin[i, j].dir;
            ShowChange(i, j);
         end;
   end;    {AcceptChange}

   procedure Metropolis;
      var
         ispin, i, j, sum, index: integer;
   begin
      ispin := 0;   {# of spins chosed at random}
      repeat     {randomly select location of spin}
         ispin := ispin + 1;
         i := trunc(L.x * ran(seed)) + 1;
         j := trunc(L.y * ran(seed)) + 1;
         pbc(i, j, L, sum);   {periodic boundary conditions}
         if (spin[i, j].dir * (sum + h) <= 0) then
            AcceptChange(i, j, sum)
         else
            begin
               index := 5 * (1 - spin[i, j].dir) + 4 + sum;
               index := index div 2;
               if (ran(seed) < w[index]) then
                  AcceptChange(i, j, sum);
            end;
      until (ispin = N);
   end;    {Metropolis}

   procedure SetUpWindowsRunMetropolis;
    begin
      ClearMuppetPort;
      menu.display;{Added}
      Comment(0, 0.95, 'Metropolis algorithm');
      if flag.rg then
         Menu.RowActivate(4, 1, true);
      if flag.demo then
         ClearDemoMenuCheck;
      SetupSpinDisplay;
      ShowSpins;
      SetupHotKeys;
      Sliders.Reset(1, T);
      Sliders.Reset(2, H);
      Sliders.DrawAll;
      ComputeMinMaxEnergy;
      SetupEnergyDisplay;
      SetupCorrelationDisplay;
      SetupDataDisplay;
      ShowData(nmcs);
      SetupProbabilityDisplay;
   end;   {SetUpWindowsRunMetropolis}

   procedure InitialRunMetropolis;
   begin
      flag.show := true;
      InitializeVariables;
      InitialE;
      Boltzmann;
   end;   {InitialRunMetropolis}

   procedure RunMetropolis;
      var
         Tnew, Hnew: real;
         kNum : byte;
   begin
      if flag.renorm then
         flag.exit := true;
      if flag.change or not start then
         repeat
            flag.change := false;
            if flag.edit then
               begin
                  flag.edit := false;
                  start := false;
               end;
            CheckForEvents;
            if Event.MouseClicked then
               begin
                  HandleMouse;
                  if not flag.renorm then
                     CheckWindows(nmcs);
               end;
            if hotkeys.pressed(knum) then begin
               HandleHotKeys(knum);
               if knum=5 then SetUpWindowsRunMetropolis;
            end;
            if Sliders.Changed then
               HandleSliders(Tnew, Hnew);
            if Menu.activated then
               begin
                  flag.exit := true;
                  handlemenu;
               end;
         until (Event.KeyPressed or Event.MouseClicked or flag.exit);
      if start and not quit then
         begin
            SaveActivate;
            repeat
               flag.exit := false;
               if nmcs = 0 then
                  ShowStatus;
               Metropolis;
               nmcs := nmcs + 1;
               Accumulate;
               CalculateProbability;
               CalculateCorrelationFunction(E, nmcs);
               if not flag.renorm then
                  ShowData(nmcs);
               CheckForEvents;
               if not flag.renorm then
                  begin
                     CheckWindows(nmcs);
                     if flag.E then
                        ShowEnergy(nmcs)
                     else
                        ShowMagnetization(nmcs);
                     if Sliders.Changed then
                        HandleSliders(Tnew, Hnew);
                     if (nmcs mod P.nshow[flag.run] = 0) then
                        begin
                           ShowCorrelationFunction(nmcs);
                           if flag.P then
                              ShowEnergyDistribution(nmcs)
                           else
                              ShowMagnetizationDistribution(nmcs);
                        end;
                  end;   {not flag.renorm}
            until (Event.KeyPressed or Event.MouseClicked) and not flag.exit;
         end;
   end;  {RunMetropolis}

   procedure ShowKawasakiData;
      var
         Ebar, E2bar, C, Ndiv, Ndiv2: real;
         BkColor, line: integer;
   begin
      SelectViewPort(3);
      SetColor(lightgreen);
      Ndiv := 1 / N;
      BkColor := GraphBackColor;
      rubout(8, 1, 6, BkColor);
      print(1, 1, concat(' mcs = ', NumStr(nmcs, 3, 0)));
      if nmcs = 1 then
         begin
            rubout(1, 2, 10, BkColor);
            rubout(1, 3, 10, BkColor);
         end;
      if nmcs > 0 then
         begin
            Ebar := average.Ecum / nmcs;
            Ebar := Ebar * Ndiv;
            setcolor(yellow);
            rubout(8, 2, 7, BkColor);
            print(1, 2, concat('<E/N>= ', NumStr(Ebar, 3, 2)));
            rubout(8, 3, 7, BkColor);
            print(1, 3, concat('  M  = ', NumStr(M, 3, 0)));
            E2bar := average.E2cum / nmcs;
            line := 0;
            if T > 0 then
               begin
                  C := (E2bar - Ebar * Ebar) / (T * T);
                  Ndiv2 := Ndiv * Ndiv;
                  C := C * Ndiv2;
                  rubout(8, 4, 6, BkColor);
                  print(1, 4,  concat('  C  = ', NumStr(C, 3, 2)));
               end;
         end
      else  {nmcs = 0}
         begin
            print(1, 2, concat(' E/N = ', NumStr(E * Ndiv, 3, 2)));
            print(1, 3, concat('  M  = ',  NumStr(M, 3, 0)));
         end;
   end;     {ShowKawasakiData}

   procedure AcceptSpinExchange (x, y, nnx, nny, deltaE: integer);
   begin
      spin[x, y].dir := -spin[x, y].dir;
      spin[nnx, nny].dir := -spin[nnx, nny].dir;
      ratio := ratio + 1.0;
      E := E + deltaE;
      if flag.show then
         begin
            ndir := spin[x, y].dir;
            ShowChange(x, y);
            ndir := spin[nnx, nny].dir;
            ShowChange(nnx, nny);
         end;
   end;    {AcceptSpinExchange}

   procedure spinexchange;
      var
         ispin, x, y, nn, nnx, nny, sum, deltaE, index: integer;
   begin
      ispin := 0;   {# of spins chosed at random}
      repeat     {randomly select location of spin}
         ispin := ispin + 1;
         x := trunc(L.x * ran(seed)) + 1;
         y := trunc(L.y * ran(seed)) + 1;
  {find nearest-neighbor at random}
         nn := trunc(4 * ran(seed)) + 1;
         nnx := x + nx[nn];
         nny := y + ny[nn];
         if nnx > L.x then
            nnx := 1;
         if nnx < 1 then
            nnx := L.x;
         if nny > L.y then
            nny := 1;
         if nny < 1 then
            nny := L.y;
         sum := spin[x, y].dir * spin[nnx, nny].dir;
         if sum < 0 then
            begin   {spins are antiparallel}
               pbc(x, y, L, sum);
               deltaE := spin[x, y].dir * (sum - spin[nnx, nny].dir);
               pbc(nnx, nny, L, sum);
               deltaE := deltaE + spin[nnx, nny].dir * (sum - spin[x, y].dir);
         {real change in energy is twice deltaE}
         {error compensated by dividing deltaE by 2 }
               if deltaE <= 0 then
                  AcceptSpinExchange(x, y, nnx, nny, DeltaE)     {accept flip}
               else
                  begin   {accept flip with probability}
                     index := DeltaE div 2;
                     if ran(seed) < w[index] then
                        AcceptSpinExchange(x, y, nnx, nny, DeltaE);
                  end;
            end;
      until (ispin = N);
   end;    {spinexchange}

   procedure SetUpWindowsRunKawasaki;
   begin
      ClearMuppetPort;
      menu.display;{Added}
      Comment(0, 0.95, 'Kawasaki (spin exchange) algorithm');
      SetupSpinDisplay;
      if flag.rg then
         Menu.RowActivate(4, 1, true);
      if flag.demo then
         ClearDemoMenuCheck;
      ShowSpins;
      SetupHotKeys;
      TemperatureSlider.Reset(T);
      TemperatureSlider.Draw;
      ComputeMinMaxEnergy;
      SetupEnergyDIsplay;
      SetupCorrelationDisplay;
      SetupDataDisplay;
      ShowKawasakiData;
      SetupProbabilityDisplay;
   end;   {SetUpWindowsRunKawasaki}

   procedure InitialRunKawasaki;
   begin
      flag.show := true;
      InitializeVariables;
      Bonds;  {define directional bonds}
      InitialE;
      TransitionProbabilities;
   end;   {InitialRunKawasaki}

   procedure RunKawasaki;
      var
         Tnew: real;
         knum : byte;
   begin
      if flag.renorm then
         flag.exit := true;
      if flag.edit then
         begin
            flag.edit := false;
            start := false;
         end;
      if flag.change or not start then
         repeat
            flag.change := false;
            CheckForEvents;
            if Event.MouseClicked then
               begin
                  HandleMouse;
                  if not flag.renorm then
                     begin
                        CheckWindows(nmcs);
                        Tnew := Temperatureslider.value;
                        if (Tnew <> T) then
                           HandleSliders(Tnew, 0);
                     end;
               end;
            if hotkeys.pressed(knum) then begin
               HandleHotKeys(knum);
               if knum=5 then SetUpWindowsRunKawasaki;
            end;
            if Menu.activated then
               begin
                  flag.exit := true;
                  handlemenu;
               end;
         until (Event.KeyPressed or Event.MouseClicked or flag.exit);
      if start and not quit then
         begin
            SaveActivate;
            repeat
               flag.exit := false;
               if nmcs = 0 then
                  ShowStatus;
               SpinExchange;
               nmcs := nmcs + 1;
               Accumulate;
               CalculateProbability;
               CalculateCorrelationFunction(E, nmcs);
               if not flag.renorm then
                  ShowKawasakiData;
               CheckForEvents;
               if not flag.renorm then
                  begin
                     CheckWindows(nmcs);
                     if flag.E then
                        ShowEnergy(nmcs)
                     else
                        ShowMagnetization(nmcs);
                     Tnew := Temperatureslider.value;
                     if (Tnew <> T) then
                        HandleSliders(Tnew, 0);
                     if (nmcs mod P.nshow[flag.run] = 0) then
                        begin
                           ShowCorrelationFunction(nmcs);
                           if flag.P then
                              ShowEnergyDistribution(nmcs)
                           else
                              ShowMagnetizationDistribution(nmcs);
                        end;
                  end;   {not flag.renorm}
            until (Event.KeyPressed or Event.MouseClicked) and not flag.exit;
         end;
   end;  {RunKawasaki}

   procedure RescaleLengthInfo;
      var
         RescaleScreen: TInputscreen;
         i: integer;
         C: HelpScrType;
   begin
      for i := 1 to 25 do
         C[i] := '';
      C[1] := ' The lattice is renormalized by blocking spins of';
      C[2] := ' linear dimension b. If the majority of spins in';
      C[3] := ' a block are up, the block spin is up.';
      C[4] := ' The sign of a block is chosen at random if there';
      C[5] := ' are equal numbers of up and down spins in the';
      C[6] := ' block.';
      C[8] := ' If L, the lattice dimension, equals a power';
      C[9] := ' of two, b = 2.';
      C[10] := ' If L equals a power of three, b = 3.';
      C[12] := ' Other values of L and b are not convenient for';
      C[13] := ' small lattices.';
      C[24] := '     Press any key or click mouse to continue.';
      with RescaleScreen do
         begin
            init;
            DefineInputPort(0, 0.45, 0.75, 0.96);
            loadline(concat('The rescaling length b =', NumStr(b, 2, 0), '.'));
            Loadline('');
            Loadline(' [  OK  ]    [Cancel]    [ Help ]');
            SetHelpScreen(C);
            AcceptScreen;
            if canceled then
               flag.renorm := false;
            flag.changeL := false;
            done;
         end;  {with}
   end;   {RescaleLengthInfo}

   procedure RenormalizeBlock (var block, sum: integer);
   begin
      if sum > 0 then
         block := 1
      else if sum < 0 then
         block := -1
      else if ran(seed) > 0.5 then
         block := 1
      else
         block := -1;
   end;    {RenormalizeBlock}

   procedure InitializeBlockspins;
      var
         i, j, ib, jb, level, bl, sum, br, block, xsc, ysc, px, py: integer;
         item, temp: nodeptr;
   begin
      if flag.changeL then
         RescaleLengthInfo;
      if flag.renorm then
         begin
            ClearMuppetPort;
            menu.display;{added}
            CloseViewPort(2);
            CloseViewPort(3);
            CloseViewPort(4);
            CloseViewPort(5);
            SetUpRenormHotKeys;
            Comment(0.31, 0.95, 'original');
            case flag.run of
               Metro:
                  Comment(0, 0.95, 'Metropolis');
               demon: Comment(0, 0.95, 'Demon');
               Wolff: Comment(0, 0.95, 'single cluster');
               Kawasaki: Comment(0, 0.95, 'spin exchange');
            end;
            DefineViewPort(2, 0.02, 0.42, 0.55, 0.88);
            DefineScale(2, 0, L.x, 0, L.y);
            SelectScale(2);
            SelectViewport(2);
            with Views[2] do
               begin
                  pixels.x := vx2 - vx1;
                  pixels.y := vy2 - vy1;
               end;
            pixels.x := pixels.x div L.x;     {width of spin in pixels}
            pixels.y := pixels.y div L.y;
            ShowSpins;    {show original lattice}
            br := 1;
            bl := b;
            levelmax := 2;
            for level := 1 to 3 do
               begin
                  i := 1;
                  j := 1;
                  while j < L.y do
                     begin
                        while i < L.x do
                           begin
                           new(item);
                              case level of
                                 1:                   spin[i, j].next := item;
                                 2:                   spin[i, j].next^.next := item;
                                 3:                   spin[i, j].next^.next^.next := item;
                              end;{case}
                              i := i + bl;
                           end;{loop over i}
                        i := 1;
                        j := j + bl;
                     end;
                  br := br * b;
                  bl := bl * b;
               end;{for level loop}
            px := b * pixels.x;
            py := b * pixels.y;
            DefineViewPort(3, 0.52, 0.92, 0.55, 0.88);    {first renormalization}
            SelectViewport(3);
            DefineScale(3, 0, L.x, 0, L.y);
            SelectScale(3);
            Comment(0.84, 0.95, 'first');
            ysc := 0;
            br := 1;
            bl := b;
            i := 1;
            j := 1;
            ib := 0;
            jb := 0;
            while j < L.y do         {show first renormalized lattice}
               begin
                  xsc := 0;
                  while i < L.x do
                     begin
                        sum := 0;
                        while jb < bl do
                           begin
                              while ib < bl do
                                 begin
                                    sum := sum + spin[i + ib, j + jb].dir;
                                    ib := ib + br;
                                 end;
                              ib := 0;
                              jb := jb + br;
                           end;
                        RenormalizeBlock(block, sum);
                        spin[i, j].next^.dir := block;
                        ShowSpin(block, xsc, ysc, px, py);
                        xsc := xsc + px;
                        i := i + bl;
                        jb := 0;
                     end;   {loop over x}
                  ysc := ysc + py;
                  i := 1;
                  ib := 0;
                  j := j + bl;
                  jb := 0;
               end; {while}
            DefineViewPort(4, 0.02, 0.42, 0.05, 0.45);
            DefineScale(4, 0, L.x, 0, L.y);
            SelectScale(4);
            SelectViewport(4);
            px := b * px;
            py := b * py;
            ysc := 0;
            br := br * b;
            bl := bl * b;
            Comment(0.33, 0.5, 'second');
            i := 1;
            j := 1;
            ib := 0;
            jb := 0;
            while j < L.y do           {second renormalized lattice}
               begin
                  xsc := 0;
                  while i < L.x do
                     begin
                        sum := 0;
                        while jb < bl do
                           begin
                              while ib < bl do
                                 begin
                                    sum := sum + spin[i + ib, j + jb].next^.dir;
                                    ib := ib + br;
                                 end;
                              ib := 0;
                              jb := jb + br;
                           end;
                        RenormalizeBlock(block, sum);
                        spin[i, j].next^.next^.dir := block;
                        ShowSpin(block, xsc, ysc, px, py);
                        xsc := xsc + px;
                        i := i + bl;
                        jb := 0;
                     end;  {loop over x}
                  ysc := ysc + py;
                  i := 1;
                  ib := 0;
                  j := j + bl;
                  jb := 0;
               end;   {loop over y}
            px := b * px;
            py := b * py;
            ysc := 0;
            br := br * b;
            bl := bl * b;
            if ((L.x mod bl) = 0) and ((L.x div bl) >= b) then
               begin
                  levelmax := 3;
                  DefineViewPort(5, 0.52, 0.92, 0.05, 0.45);
                  DefineScale(5, 0, L.x, 0, L.y);
                  SelectScale(5);
                  SelectViewport(5);
                  Comment(0.84, 0.5, 'third');
                  i := 1;
                  j := 1;
                  ib := 0;
                  jb := 0;
                  while j < L.y do           {third renormalized lattice}
                     begin
                        xsc := 0;
                        while i < L.x do
                           begin
                              sum := 0;
                              while jb < bl do
                                 begin
                                    while ib < bl do
                                       begin
                                          sum := sum + spin[i + ib, j + jb].next^.next^.dir;
                                          ib := ib + br;
                                       end;
                                    ib := 0;
                                    jb := jb + br;
                                 end;
                              RenormalizeBlock(block, sum);
                              spin[i, j].next^.next^.next^.dir := block;
                              ShowSpin(block, xsc, ysc, px, py);
                              xsc := xsc + px;
                              i := i + bl;
                              jb := 0;
                           end;  {loop over x}
                        ysc := ysc + py;
                        i := 1;
                        ib := 0;
                        j := j + bl;
                        jb := 0;
                     end;  {loop over y}
               end;   {br > 2}
         end;
   end;    {InitializeBlockspins}

   procedure ShowBlockSpins (x, y: integer);
      var
         i, j, ib, jb: integer;
         sum, block, br, bl, xsc, ysc, px, py, level: integer;
         change: boolean;
   begin   {assume x,y changed}
      change := true;
      bl := b;
      level := 1;
      br := L.x + 1 - bl;
      px := b * pixels.x;
      py := b * pixels.y;
      repeat
         repeat
            sum := x div br;
            br := br - bl;
         until sum >= 1;
         i := br + bl;
         br := L.x + 1 - bl;
         repeat
            sum := y div br;
            br := br - bl;
         until sum >= 1;
         j := br + bl;
         br := 1;
         sum := 0;
         case level of
            1: begin
                  br := 1;
                  sum := 0;
                  ib := 0;
                  jb := 0;
                  while jb < bl do
                     begin
                        while ib < bl do
                           begin
                              sum := sum + spin[i + ib, j + jb].dir;
                              ib := ib + br;
                           end;
                        ib := 0;
                        jb := jb + br;
                     end;
                  RenormalizeBlock(block, sum);
                  if block <> spin[i, j].next^.dir then
                     spin[i, j].next^.dir := block
                  else
                     change := false;
               end;
            2: begin
                  sum := 0;
                  br := b;
                  ib := 0;
                  jb := 0;
                  while jb < bl do
                     begin
                        while ib < bl do
                           begin
                              sum := sum + spin[i + ib, j + jb].next^.dir;
                              ib := ib + br;
                           end;
                        ib := 0;
                        jb := jb + br;
                     end;
                  RenormalizeBlock(block, sum);
                  if block <> spin[i, j].next^.next^.dir then
                     spin[i, j].next^.next^.dir := block
                  else
                     change := false;
               end;
            3: begin
                  sum := 0;
                  br := b * b;
                  ib := 0;
                  jb := 0;
                  while jb < bl do
                     begin
                        while ib < bl do
                           begin
                              sum := sum + spin[i + ib, j + jb].next^.next^.dir;
                              ib := ib + br;
                           end;
                        ib := 0;
                        jb := jb + br;
                     end;
                  RenormalizeBlock(block, sum);
                  if block <> spin[i, j].next^.next^.next^.dir then
                     spin[i, j].next^.next^.next^.dir := block
                  else
                     change := false;
               end;
         end;  {case}
         if change then
            begin
               SelectViewport(2 + level);
               SelectScale(2 + level);
               ib := i div bl;
               jb := j div bl;
               xsc := ib * px;
               ysc := jb * py;
               ShowSpin(block, xsc, ysc, px, py);
               level := level + 1;
               bl := bl * b;
               br := L.x + 1 - bl;
               px := b * px;
               py := b * py;
            end;
      until not change or (level > levelmax);
   end;   {ShowBlockSpins}

   procedure freememory;
      var
         i, j: integer;
         temp, item: nodeptr;
         br, bl, level: integer;
   begin
      bl := 1;
      for level := 1 to levelmax do
         bl := bl * b;
      br := bl div b;
      for level := levelmax downto 1 do
         begin
            i := 1;
            j := 1;
            while j < L.y do
               begin
                  while i < L.x do
                     begin
                        case level of
                           1:             dispose(spin[i, j].next);
                           2:             dispose(spin[i, j].next^.next);
                           3:             dispose(spin[i, j].next^.next^.next);
                        end;{case}
                        i := i + bl;
                     end;{loop over i}
                  i := 1;
                  j := j + bl;
               end;
            br := br div b;
            bl := bl div b;
         end;   {for level loop}
   end;   {freememory}

   procedure RenormalizeOn;
   begin
      flag.renorm := true;
      RescaleLengthInfo;
      InitializeBlockspins;
      if flag.renorm = true then
         begin
            Menu.RowActivate(4, 1, false);
            Menu.RowActivate(4, 2, true);
            Menu.RowActivate(3, 1, false);
            Menu.RowActivate(3, 2, false);
            Menu.RowActivate(3, 3, false);
            Menu.RowActivate(3, 4, false);
         end;
   end;   {RenormalizeOn}

   procedure RenormalizeOff;
   begin
      flag.renorm := false;
      freememory;
      Menu.RowActivate(4, 1, true);
      Menu.RowActivate(4, 2, false);
      Menu.Rowactivate(3, 1, true);
      Menu.RowActivate(3, 2, true);
      Menu.RowActivate(3, 3, true);
      Menu.RowActivate(3, 4, true);
      ClearMuppetPort;
      CloseViewPort(2);
      CloseViewPort(3);
      CloseViewPort(4);
      CloseViewPort(5);
      case flag.run of
         Metro: SetUpWindowsRunMetropolis;
         demon: SetUpWindowsRunDemon;
         Wolff: SetUpWindowsRunWolff;
         Kawasaki: SetUpWindowsRunKawasaki;
      end;
   end;   {RenormalizeOff}

   procedure SetupLog;
   begin
      Comment(0.7, 0.95, 'Summary of Results');
      DefineViewPort(3, 0.61, 0.97, 0.05, 0.95);
      SetlineStyle(solidLn, 0, NormWidth);
   end;   {SetupLog}

   procedure savecolors (var up, down: integer);
   begin
      if color.up <> 4 then   {red}
         begin
            up := color.up;
            color.up := red;
         end;
      if color.down <> 2 then   {green}
         begin
            down := color.down;
            color.down := green;
         end;
   end;  {savecolors}

   procedure SetupDemo;
      var
         i, j, up, right, down, sum: integer;
   begin
      ClearMuppetPort;
      menu.display;{Added}
      EraseDynamicsMenuCheck;
      Menu.RowActivate(5, 1, false);
      with Menu do
         begin
            RowActivate(1, 5, False);
            RowActivate(1, 6, False);
            RowActivate(1, 7, False);
            RowActivate(4, 1, false);
         end;
      flag.demo := true;
      InitialE;
      case flag.run of
         MetroDemo:
            Comment(0, 0.95, 'Demonstration of Metropolis algorithm');
         DemonDemo:
            Comment(0, 0.95, 'Demonstration of Demon algorithm');
         WolffDemo:              Comment(0, 0.95, 'Demonstration of cluster algorithm');
         KawasakiDemo:
            Comment(0, 0.95, 'Demonstration of spin exchange algorithm');
      end;  {case}
      SetupSpinDisplay;
      up := 4;
      down := 2;
      if (flag.run = Wolffdemo) and ((color.up <> 4) or (color.down <> 2)) then
         savecolors(up, down);
      ShowSpins;
      if (flag.run = Wolffdemo) then
         begin
            color.up := up;
            color.down := down;
         end;
      SetUpDemoHotKey;
      SetupLog;
   end; {SetupDemo}

   procedure ShowSpinUpDown;
      var
         xpix, ypix: integer;
   begin
      DefineViewPort(5, 0.15, 0.3, 0.78, 0.88);
      OpenViewPort(5);           {animation window}
      with Views[5] do
         begin
            xpix := vx2 - vx1;
            ypix := vy2 - vy1;
         end;
      Setcolor(white);
      SetFillStyle(solidfill, color.up);
      HideMouse;
      bar(2, 2, 12, 12);
      OutTextXY(16, 2, 'spin up');
      SetFillStyle(solidfill, color.down);
      bar(2, 15, 12, 25);
      OutTextXY(16, 15, 'spin down');
                ShowMouse;
   end;   {ShowSpinUpDown}

   procedure Checkline;
   begin
      if line >= 20 then
         begin
            OpenViewPort(3);
            line := 0;
         end;
   end;   {Checkline}

   procedure InitialRunDemonDemo;
   begin
      SetUpDemo;
      ShowSpinUpDown;
      Ed := 0.0;
      line := 1;
      Comment(0.05, 0.75, 'Click on spin for trial flip.');
      OpenViewPort(3);
      print(1, line, concat('M = ', NumStr(M, 2, 0)));
      print(10, line, concat('E = ', NumStr(E, 2, 0)));
      print(21, line, concat('Ed = ', NumStr(Ed, 2, 0)));
      Menu.RowActivate(4, 1, false)
   end;   {InitialRunDemonDemo}

   procedure RunDemonDemo;
      var
         xdum, ydum: real;
         xsc, ysc, sum: integer;
         inside: boolean;
         deltaE: real;
         knum : byte;
   begin
      flag.exit := false;
      if flag.change or flag.demo then
         repeat
            flag.change := false;
            CheckForEvents;
            if Event.MouseClicked then
               MousePosn(xdum, ydum, 2, 2, inside);
            if hotkeys.pressed(knum) then
               HandleHotKeys(knum);
            if Menu.activated then
               begin
                  flag.exit := true;
                  handlemenu;
               end;
         until (Event.KeyPressed or Event.MouseClicked or flag.exit);
      if inside then
         begin
            xsc:= (Event.x - views[2].vx1) div pixels.x + 1;
            ysc:= (Event.y - views[2].vy1) div pixels.y + 1;
            SelectViewPort(3);
            SetColor(white);
            line := line + 1;
            checkline;
            pbc(xsc, ysc, L, sum);
            deltaE := 2 * spin[xsc, ysc].dir * sum;
            line := line + 1;
            print(1, line, concat('change in energy = ', NumStr(deltaE, 2, 0)));
            if (deltaE <= Ed) then
               begin    {accept flip}
                  line := line + 1;
                  print(1, line, 'flip spin');
                  spin[xsc, ysc].dir := -spin[xsc, ysc].dir;
                  M := M + 2 * spin[xsc, ysc].dir;
                  E := E + deltaE;
                  Ed := Ed - deltaE;
                  SelectViewPort(2);
                  SelectScale(2);
                  SetColor(white);
                  ndir := spin[xsc, ysc].dir;
                  xsc := (xsc - 1) * pixels.x;
                  ysc := (ysc - 1) * pixels.y;
                  ShowSpin(ndir, xsc, ysc, pixels.x, pixels.y);
               end  {accept flip}
            else
               begin
                  SelectViewPort(3);
                  SetColor(white);
                  line := line + 1;
                  print(1, line, 'no change');
               end;
            SelectViewPort(3);
            SetColor(white);
            line := line + 1;
            print(1, line, concat('M = ', NumStr(M, 2, 0)));
            print(10, line, concat('E = ', NumStr(E, 2, 0)));
            print(21, line, concat('Ed = ', NumStr(Ed, 2, 0)));
         end;  {if inside}
      if inside then
         flag.demo := true;
   end;   {RunDemonDemo}

   procedure ShowColorCode;
      var
         xpix, ypix: integer;
   begin
      DefineViewPort(5, 0.05, 0.51, 0.78, 0.88);
      OpenViewPort(5);
      with Views[5] do
         begin
            xpix := vx2 - vx1;
            ypix := vy2 - vy1;
         end;
      Setcolor(white);
      SetFillStyle(solidfill, red);
      HideMouse;
      bar(2, 2, 12, 12);
      rectangle(2, 2, 12, 12);
      OutTextXY(16, 2, 'spin up');
      SetFillStyle(solidfill, green);
      bar(2, 17, 12, 27);
      rectangle(2, 17, 12, 27);
      OutTextXY(16, 17, 'spin down');
      SetFillStyle(solidfill, lightblue);
      bar(100, 2, 110, 12);
      rectangle(100, 2, 110, 12);
      OutTextXY(120, 2, 'perimeter site');
      SetFillStyle(solidfill, black);
      bar(100, 17, 110, 27);
      rectangle(100, 17, 110, 27);
      OutTextXY(120, 17, 'site added to cluster');
      ShowMouse;
   end;   {ShowColorCode}

   procedure RescaleSpins (var pixels: SpinDisplay);
      var
         x, y: integer;
         xt, yt: integer;
   begin
      SelectViewPort(2);
      SelectScale(2);
      for y := 1 to L.y do
         begin
            for x := 1 to L.x do
               begin
                  xt := (x - 1) * pixels.x;
                  yt := (y - 1) * pixels.y;
                  if (spin[x, y].dir > 1) then
                     begin
                        spin[x, y].dir := 1;
                        SetFillStyle(solidfill, red);
                        if hideGrid then SetColor(red);
                        HideMouse;
                        bar(xt, yt, xt + pixels.x, yt + pixels.y);
                        rectangle(xt, yt, xt + pixels.x, yt + pixels.y);
                        ShowMouse;
                     end
                  else if spin[x, y].dir < -1 then
                     begin
                        spin[x, y].dir := -1;
                        SetFillStyle(solidfill, green);
                        if hideGrid then SetColor(green);
                        HideMouse;
                        bar(xt, yt, xt + pixels.x, yt + pixels.y);
                        rectangle(xt, yt, xt + pixels.x, yt + pixels.y);
                        ShowMouse;
                     end;  {if statement}
               end    {loop over x}
         end; {loop over y}
   end;   {RescaleSpins}

   procedure new_perimeter_demo (var x, y: integer);
      var
         i, nnx, nny, perim: integer;
         xt, yt: integer;
   begin
      for i := 1 to 4 do       {find perimeter sites of site x,y}
         begin
            nnx := x + nx[i];
            nny := y + ny[i];
            if nnx > L.x then
               nnx := 1;
            if nnx < 1 then
               nnx := L.x;
            if nny > L.y then
               nny := 1;
            if nny < 1 then
               nny := L.y;
            perim := spin[nnx, nny].dir;  {possible perimeter site}
            if (perim * ndir = 1) then
               begin
                  SelectViewPort(2);
                  SelectScale(2);
                  SetFillStyle(solidfill, LightBlue);
                  xt := (nnx - 1) * pixels.x;
                  yt := (nny - 1) * pixels.y;
                  HideMouse;
                  bar(xt, yt, xt + pixels.x, yt + pixels.y);
                  rectangle(xt, yt, xt + pixels.x, yt + pixels.y);
                  ShowMouse;
                  nper := nper + 1;
                  spin[nnx, nny].dir := spin[nnx, nny].dir + ndir;
                  perx[nper] := nnx;
                  pery[nper] := nny;
               end;
         end;   {end loop}
   end;     {new_perimeter_demo}

   procedure CheckPerimeterChoice (xsc, ysc: integer; var select: integer; var correct: boolean);
      var
         iper, xper, yper: integer;
   begin
      iper := 1;
      correct := false;
      repeat
         xper := perx[iper];   {check if selected spin is perimeter site}
         yper := pery[iper];
         if (xsc = xper) and (ysc = yper) then
            begin
               correct := true;
               select := iper;
            end;
         iper := iper + 1;
      until correct or (iper > nper);
   end; {CheckPerimeterChoice}

   procedure ShowSpinColorFixed (direction, xt, yt, px, py: integer);
   var colr : integer;
   begin
      HideMouse;
      if direction > 0 then colr := red
      else colr := green;
      SetFillStyle(solidfill, colr);
      if hideGrid then SetColor(colr);
      bar(xt, yt, xt + px, yt + py);
      rectangle(xt, yt, xt + px, yt + py);
      ShowMouse;
   end;   {ShowSpinColorFixed}

   procedure ShowSpinChosen (xsc, ysc: integer);
      var
         xt, yt: integer;
   begin
      SelectViewPort(2);
      SelectScale(2);
      xt := (xsc - 1) * pixels.x;
      yt := (ysc - 1) * pixels.y;
      SetFillStyle(solidfill, black);
      HideMouse;
      bar(xt, yt, xt + pixels.x, yt + pixels.y);
      rectangle(xt, yt, xt + pixels.x, yt + pixels.y);
      ShowMouse;
   end;   {ShowSpinChosen}

   procedure ConvertScreenCoordinates (var xdum, ydum: real; var xsc, ysc: integer);
   begin
      {xdum := xdum + 1.0;   {choose seed site}
      {ydum := ydum + 1.0;
      xsc := Trunc(xdum);
      ysc := Trunc(ydum);}
      {ysc := L.y - ysc + 1;}
       xsc:= (Event.x - views[2].vx1) div pixels.x + 1;
       ysc:= (Event.y - views[2].vy1) div pixels.y + 1;
   end;   {ConvertScreenCoordinates}

   procedure ChooseSeed (var xdum, ydum: real; var xsc, ysc: integer);
   begin
      ConvertScreenCoordinates(xdum, ydum, xsc, ysc);
      ndir := spin[xsc, ysc].dir;
      noccup := -2 * ndir;
      spin[xsc, ysc].dir := noccup;
      ShowSpinChosen(xsc, ysc);
      new_perimeter_demo(xsc, ysc);   {find perimeter sites of seed}
   end;  {ChooseSeed}

   procedure InitialRunWolffDemo;
   var st : string;
   begin
      SetUpDemo;
      ShowColorCode;
      nper := 0;
      line := 1;
      st := num2Str(T,5);
      Comment(0.03, 0.70, 'Temperature = '+st+'   H field = 0');
      OpenViewPort(3);
      print(1, line, concat('M = ', NumStr(M, 2, 0)));
      print(13, line, concat('E = ', NumStr(E, 2, 0)));
      Menu.RowActivate(5, 2, false);
   end;    {InitialRunWolffDemo}

{{$S part4}

   procedure RunWolffDemo;
      var
         xdum, ydum: real;
         xsc, ysc: integer;
         x, y: integer;
         inside, correct: boolean;
         select: integer;
         r: real;   {bond probability}
         xt, yt: integer;
         knum : byte;
   begin
      flag.exit := false;
      if nper = 0 then
         Comment(0.02, 0.75, 'Click on seed site to grow cluster.');
      if flag.change or flag.demo then
         repeat
            flag.change := false;
            CheckForEvents;
            if Event.MouseClicked then
               MousePosn(xdum, ydum, 2, 2, inside);
            if hotkeys.pressed(knum) then
               HandleHotKeys(knum);
            if Menu.activated then
               begin
                  flag.exit := true;
                  handlemenu;
               end;
         until (Event.KeyPressed or Event.MouseClicked or flag.exit);
      {SaveActivate};
      if inside then
         begin
            if nper = 0 then
               begin
                  ChooseSeed(xdum, ydum, xsc, ysc);
                  mass := 1;
                  ShowColorCode;
                  Comment(0.02, 0.75, 'Click on perimeter site.           ');
               end
            else
               begin  {choose perimeter site}
                  ConvertScreenCoordinates(xdum, ydum, xsc, ysc);
                  CheckPerimeterChoice(xsc, ysc, select, correct);
                  if correct then
                     begin
                        perx[select] := perx[nper];
                        pery[select] := pery[nper];
                        nper := nper - 1;
                        if spin[xsc, ysc].dir <> noccup then       {spin not already flipped}
                           begin
                              r := ran(seed);
                              SelectViewPort(3);
                              Checkline;
                              line := line + 1;
                              print(1, line, concat('r = ', NumStr(r, 3, 3)));
                              if r < pb then
                                 begin   {site added to cluster}
                                    spin[xsc, ysc].dir := noccup;    {flip spin}
                                    mass := mass + 1;
                                    ShowSpinChosen(xsc, ysc);
                                    SelectViewPort(3);
                                    print(13, line, 'site added');
                                    new_perimeter_demo(xsc, ysc);
                                 end   {added site to cluster and found new perimeter sites}
                              else
                                 begin    {site visited but not added to cluster}
                                    SelectViewPort(3);
                                    print(13, line, 'site not added');  {return site to orginal color}
                                    SelectViewPort(2);
                                    selectscale(2);
                                    xt := (xsc - 1) * pixels.x;
                                    yt := (ysc - 1) * pixels.y;
                                    spin[xsc, ysc].dir := spin[xsc, ysc].dir - ndir;
                                    ShowSpinColorFixed(spin[xsc, ysc].dir, xt, yt, pixels.x, pixels.y);
                                 end;    {visited site not added}
                           end;   {r < pb}
                     end;   {if spin not already added}
               end; {if correct}
         end;  {if inside}
      if inside and (nper = 0) then
         begin
            RescaleSpins(pixels);
            line := line + 1;
            SelectViewPort(3);
            print(1, line, concat('mass = ', NumStr(mass, 2, 0)));
            print(13, line, 'cluster flipped');
            InitialE;
            line := line + 1;
            print(1, line, concat('M = ', NumStr(M, 2, 0)));
            print(13, line, concat('E = ', NumStr(E, 2, 0)));
            line := line + 1;
         end;
      if inside then
         flag.demo := true;
   end;  {RunWolffDemo}

   procedure InitialRunMetropolisDemo;
   var st : string;
   begin
      SetUpDemo;
      ShowSpinUpDown;
      Boltzmann;
      Comment(0.05, 0.75, 'Click on spin for trial flip.');
      st := num2Str(T,5);
      Comment(0.03, 0.70, 'Temperature = '+st+'   H field = 0');
      OpenViewPort(3);
      line := 1;
      print(1, line, concat('M = ', NumStr(M, 3, 0)));
      print(10, line, concat('E = ', NumStr(E, 3, 0)));
   end;   {InitialRunMetropolisDemo}

   procedure RunMetropolisDemo;
      var
         xdum, ydum: real;
         xsc, ysc, sum, xt, yt, index: integer;
         inside, change: boolean;
         deltaE, r: real;
         knum : byte;
   begin
      flag.exit := false;
      if flag.change or flag.demo then
         repeat
            flag.change := false;
            CheckForEvents;
            if Event.MouseClicked then
               MousePosn(xdum, ydum, 2, 2, inside);
            if hotkeys.pressed(knum) then
               HandleHotKeys(knum);
            if Menu.activated then
               begin
                  flag.exit := true;
                  handlemenu;
               end;
         until (Event.KeyPressed or Event.MouseClicked or flag.exit);
      with Event do
         if inside then
            begin
               change := false;
               xsc:=(Event.x - views[2].vx1) div pixels.x + 1;
               ysc:=(Event.y - views[2].vy1) div pixels.y + 1;
               line := line + 1;
               checkline;
               pbc(xsc, ysc, L, sum);
               deltaE := 2 * spin[xsc, ysc].dir * sum;
               line := line + 1;
               print(1, line, concat('change in energy = ', NumStr(deltaE, 2, 0)));
               if deltaE <= 0 then
                  change := true  {accept flip}
               else
                  begin   {accept flip with probability}
                     index := 1 - spin[xsc, ysc].dir;   {index ranges from 0 to 8}
                     index := 5 * index + 4 + sum;
                     index := index div 2;
                     r := ran(seed);
                     line := line + 1;
                     print(1, line, concat('r = ', NumStr(r, 3, 3)));
                     print(14, line, concat('w = ', NumStr(w[index], 3, 3)));
                     if (r < w[index]) then
                        begin
                           change := true;
                        end;
                  end;
               if change then
                  begin
                     line := line + 1;
                     print(1, line, 'flip spin');
                     spin[xsc, ysc].dir := -spin[xsc, ysc].dir;
                     M := M + 2 * spin[xsc, ysc].dir;
                     E := E - 2 * spin[xsc, ysc].dir * sum;
                     SelectViewPort(2);
                     SelectScale(2);
                     SetColor(white);
                     xt := (xsc - 1) * pixels.x;
                     yt := (ysc - 1) * pixels.y;
                     ShowSpin(spin[xsc, ysc].dir, xt, yt, pixels.x, pixels.y);
                     change := false;
                  end
               else
                  begin
                     SelectViewPort(3);
                     SetColor(white);
                     line := line + 1;
                     print(1, line, 'no change');
                  end;
               SelectViewPort(3);
               SetColor(white);
               line := line + 1;
               print(1, line, concat('M = ', NumStr(M, 3, 0)));
               print(10, line, concat('E = ', NumStr(E, 3, 0)));
            end;   {if inside}
      if inside then
         flag.demo := true;
   end;  {RunMetropolisDemo}

   procedure InitialRunKawasakiDemo;
   var st : string;
   begin
      SetUpDemo;
      ShowSpinUpDown;
      Boltzmann;
      bonds;   {define directions for nearest-neighbors}
      Comment(0.01, 0.75, 'Click on two neighboring antiparallel spins.');
      st := num2Str(T,5);
      Comment(0.03, 0.70, 'Temperature = '+st+'   H field = 0');
      OpenViewPort(3);
      line := 1;
      print(1, line, concat('M = ', NumStr(M, 3, 0)));
      print(10, line, concat('E = ', NumStr(E, 3, 0)));
      Menu.RowActivate(4, 1, false);
   end;   {InitialRunKawasakiDemo}

   procedure RunKawasakiDemo;
      var
         xdum, ydum: real;
         xsc, ysc, xt, yt, i, nnx, nny, index, sum: integer;
         inside, change: boolean;
         deltaE, beta, prob, r: real;
         anylongint: longint;
         knum : byte;
   begin
      flag.exit := false;
      if flag.change or flag.demo then
         repeat
            flag.change := false;
            CheckForEvents;
            if Event.MouseClicked then
               MousePosn(xdum, ydum, 2, 2, inside);
            if hotkeys.pressed(knum) then
               HandleHotKeys(knum);
            if Menu.activated then
               begin
                  flag.exit := true;
                  handlemenu;
               end;
         until (Event.KeyPressed or Event.MouseClicked or flag.exit);
      {SaveActivate};
      with Event do
         if inside then
            begin
               change := false;
               if noccup > 0 then
                  begin
                     line := line + 1;
                     checkline;
                  end;
               ConvertScreenCoordinates(xdum, ydum, xsc, ysc);
               ndir := spin[xsc, ysc].dir;
               ShowSpinChosen(xsc, ysc);
          {noccup = 1 means that one site has been selected already}
               if noccup = 0 then
                  begin
                     noccup := 1;
                     perx[1] := xsc; {use perimeter array to store the first spin}
                     pery[1] := ysc;
                  end
               else   {now have checked two spins}
                  begin    {check that two spins are antiparallel}
                     xt := perx[1];
                     yt := pery[1];
                     sum := spin[xsc, ysc].dir * spin[xt, yt].dir;
                     if sum > 0 then
                        begin
                           noccup := 0;
                           beep;
                           SelectViewPort(3);
                           SetColor(white);
                           line := line + 1;
                           if (xsc = xt) and (ysc = yt) then
                              print(1, line, 'choose different spins')
                           else
                              print(1, line, 'spins must be antiparallel');
                        end
                     else   {sum < 0}
                        begin
                           i := 0;
                           repeat  {check if spins really are nearest-neighbors}
                              i := i + 1;
                              nnx := xsc + nx[i];
                              nny := ysc + ny[i];
                              if nnx > L.x then
                                 nnx := 1;
                              if nnx < 1 then
                                 nnx := L.x;
                              if nny > L.y then
                                 nny := 1;
                              if nny < 1 then
                                 nny := L.y;
                              if (nnx = xt) and (nny = yt) then
                                 change := true;
                           until (i = 4) or change;
                           if not change then
                              begin
                                 beep;
                                 SelectViewPort(3);
                                 SetColor(white);
                                 line := line + 1;
                                 print(1, line, 'choose nearest-neighbors');
                                 noccup := 0;
                              end;
                        end;
                     if change and (sum < 0) then
                        begin   {spins are antiparallel and nearest-neighbor}
                           change := false;
                           noccup := 0;
                           pbc(xsc, ysc, L, sum);
                           deltaE := spin[xsc, ysc].dir * (sum - spin[xt, yt].dir);
                           pbc(xt, yt, L, sum);
                           deltaE := deltaE + spin[xt, yt].dir * (sum - spin[xsc, ysc].dir);
                           deltaE := 2 * deltaE;
                           SelectViewPort(3);
                           SetColor(white);
                           line := line + 1;
                           print(1, line, concat('change in energy = ', NumStr(deltaE, 2, 0)));
                           if deltaE <= 0 then
                              change := true     {accept flip}
                           else
                              begin   {accept flip with probability}
                                 prob := exp(-deltaE / T);
                                 r := ran(seed);
                                 line := line + 1;
                                 print(1, line, concat('r = ', NumStr(r, 3, 3)));
                                 print(14, line, concat('w = ', NumStr(prob, 3, 3)));
                                 if (r < prob) then
                                    change := true;
                              end;
                           if change then
                              begin
                                 line := line + 1;
                                 print(1, line, 'exchange pair');
                                 spin[xt, yt].dir := -spin[xt, yt].dir;
                                 spin[xsc, ysc].dir := -spin[xsc, ysc].dir;
                                 E := E + deltaE;
                                 SelectViewPort(2);
                                 SelectScale(2);
                                 SetColor(white);
                                 ndir := spin[xsc, ysc].dir;
                                 xsc := (xsc - 1) * pixels.x;
                                 ysc := (ysc - 1) * pixels.y;
                                 ShowSpin(ndir, xsc, ysc, pixels.x, pixels.y);
                                 ndir := spin[xt, yt].dir;
                                 xsc := (xt - 1) * pixels.x;
                                 ysc := (yt - 1) * pixels.y;
                                 ShowSpin(ndir, xsc, ysc, pixels.x, pixels.y);
                              end
                           else
                              begin
                                 SelectViewPort(3);
                                 SetColor(white);
                                 line := line + 1;
                                 print(1, line, 'no change');  {change colors back}
                                 noccup := 0;
                              end;
                        end;
                     if not change then
                        begin
                           ndir := spin[xsc, ysc].dir;  {set the spins back to their original color}
                           xsc := (xsc - 1) * pixels.x;
                           ysc := (ysc - 1) * pixels.y;
                           SelectViewPort(2);
                           SelectScale(2);
                           SetColor(white);
                           ShowSpin(ndir, xsc, ysc, pixels.x, pixels.y);
                           ndir := spin[xt, yt].dir;
                           xsc := (xt - 1) * pixels.x;
                           ysc := (yt - 1) * pixels.y;
                           ShowSpin(ndir, xsc, ysc, pixels.x, pixels.y);
                        end;
                     line := line + 1;
                     SelectViewPort(3);
                     SetColor(white);
                     print(1, line, concat('M = ', NumStr(M, 3, 0)));
                     print(10, line, concat('E =', NumStr(E, 3, 0)));
                     delay(31);
                  end;   {if inside}
            end;
      if inside then
         flag.demo := true;
   end;  {RunKawasakiDemo}

   procedure HandleHotKeys( knum : byte);
   begin
      if flag.demo then
         begin
              case knum of
              1: HelpHotKeys;
              2: If Menu.Chosen then HandleMenu;
              end  {case}
         end else
      if flag.renorm then
         begin
              case knum of
              1: HelpHotKeys;
              2: start := not start;
              3: If Menu.Chosen then HandleMenu;
              end  {case}
         end
      else   {standard hotkeys}
         begin
            case knum of
               1: HelpHotKeys;
               2: begin
                     if start then
                        start := false
                     else
                        start := true;
                  end;
               3: begin
                     if flag.show then
                        begin
                           flag.show := false;
                           HideSpins;
                           knum := 0;
                           ClearMKBuffers;
                        end
                     else
                        begin
                           flag.show := true;
                           ShowSpins
                        end;
                  end;
               4: {reset variables}
                  begin
                     InitializeVariables;
                     if flag.E then
                        ShowEnergy(nmcs)
                     else
                        ShowMagnetization(nmcs);
                     OpenViewPort(7);
                     setcolor(white);
                     if flag.P then
                        begin
                           if flag.run <> demon then
                              PutLabel(inside, 'P(E)')
                           else
                              begin
                                 PutLabel(inside, 'P(Ed)');
                              end;
                        end
                     else
                        PutLabel(inside, 'P(M)');
                     OpenViewport(8);   {correlation function}
                     setcolor(graphbackcolor);
                     PutLabel(inside, ' '+chr(219));
                     SetColor(white);
                     if flag.ce then PutLabel(inside,'Ce')
                     else PutLabel(inside, 'Cm');
                  end;
               5: {edit spins}
                  edit;
               6: if Menu.Chosen then HandleMenu;
            end; {case}
         end;   {flag}
         if not start then CheckForEvents;    { Clears Event record }
   end;  {HandleHotKeys}

{{$S part5}

procedure RunSimulation;
begin
  case flag.run of
     Metro: begin
           if flag.change then
              begin
                 InitialRunMetropolis;
                 if not flag.renorm then
                    begin
                       SetUpWindowsRunMetropolis;
                    end
                 else
                    begin
                       InitializeBlockspins;
                       Menu.RowActivate(4, 1, false);
                    end
              end;
           RunMetropolis;
        end;
     Demon: begin
           if flag.change then
              begin
                 InitialRunDemon;
                 if not flag.renorm then
                    begin
                       SetUpWindowsRunDemon;
                    end
                 else
                    begin
                       InitializeBlockspins;
                       Menu.RowActivate(4, 1, false);
                    end;
              end;
           RunDemon;
        end;
     Wolff: begin
           if flag.change then
              begin
                 InitialRunWolff;
                 if not flag.renorm then
                    begin
                       SetUpWindowsRunWolff;
                    end
                 else
                    begin
                       InitializeBlockspins;
                       Menu.RowActivate(4, 1, false);
                    end;
              end;
           RunWolff;
        end;
     Kawasaki: begin
           if flag.change then
              begin
                 InitialRunKawasaki;
                 if not flag.renorm then
                    begin
                       SetUpWindowsRunKawasaki;

                    end
                 else
                    begin
                       InitializeBlockspins;
                       Menu.RowActivate(4, 1, false);
                    end;
              end;
           RunKawasaki;
        end;
     Metrodemo: begin
           if flag.change then
              InitialRunMetropolisDemo;
           RunMetropolisDemo;
        end;
     DemonDemo: begin
           if flag.change then
              InitialRunDemonDemo;
           RunDemonDemo;
        end;
     WolffDemo: begin
           if flag.change then
              InitialRunWolffDemo;
           RunWolffDemo;
        end;
     KawasakiDemo: begin
           if flag.change then
              InitialRunKawasakiDemo;
           RunKawasakiDemo;
        end;
  end;
end; {RunSimulation}

procedure HandleMenu;
var
   runflag : runtype;
begin
  case Menu.Colchosen of
     1: begin
           case Menu.RowChosen of
              1: Description;
              2: AboutCups;
              3: Configuration;
              5: newlattice;
              6: XOpenFile;
              7: Savedata;
              9: Quit := true;
           end;   {case Rowchosen}
        end;   {case 1}
     2: {algorithm}
        begin
           flag.change := true;
           start := false;
           case Menu.RowChosen of
              1: flag.run := Metro;
              2: flag.run := demon;
              3: flag.run := Wolff;
              4: flag.run := Kawasaki;
           end;
        end;   {case 2}
     3: {demos}
        begin
           flag.change := true;
           start := false;
           case Menu.RowChosen of
              1: flag.run := MetroDemo;
              2: flag.run := DemonDemo;
              3: flag.run := WolffDemo;
              4: flag.run := KawasakiDemo;
           end;  {case}
        end;   {case 3}
     4: {renormalize}
        begin
           case Menu.RowChosen of
              1: RenormalizeOn;
              2: RenormalizeOff;
           end;  {case}
        end;   {case 4}
     5: begin
           case Menu.RowChosen of
              1: ChangeParameters;
              2: ChangeDisplayOptions;
           end;  {case}
        end;   {case 5}
     6: begin
           runflag := flag.run;
           case Menu.RowChosen of
              1: Description;
              2: GeneralInfo;
              3: flag.run := MetroDemo;
              4: flag.run := Metro;
              5: flag.run := DemonDemo;
              6: flag.run := demon;
              7: flag.run := WolffDemo;
              8: flag.run := Wolff;
              9: flag.run := KawasakiDemo;
             10: flag.run := Kawasaki;
             11: HotKeysHelp;
           end;  {case}
           if Menu.RowChosen in [3..10] then HelpHotKeys;
           flag.run := runflag;
        end;
  end;   {case}
end;   {HandleMenu}

var
   knum : byte;
begin            {main program}
   GraphBackcolor :=DarkGray;
   CUPSinit;
   GraphDefaults;
   ShowTitle;
   InitializeMenu;
   SetDefaults;
   bonds;
   SetupSliders;
   SetupOtherSliders;
   Setflags;
   RandomInitialConfiguration;
   repeat  {main loop}
      RunSimulation;
      if hotkeys.pressed(knum) then
         HandleHotKeys(knum);
      if Menu.activated then
         handlemenu;
   until quit;
   closeup;
   CupsDone;
end.   {main program}

