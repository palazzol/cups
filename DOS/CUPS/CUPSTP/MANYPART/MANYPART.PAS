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

program ManyPart;
{CUPS Project on Thermal Physics}
{Molecular dynamics and Monte Carlo simulation of gases, liquids, and solids}
{July 3, 1994}
{written by Harvey Gould with assistance from Jan Tobochnik}

   uses
      crt,dos,graph, CUPSmupp, CUPS, CUPSfunc, CUPSproc, CUPSgui, CUPSgrph;
   const
      Nmax = 200;   {maximum number of particles}
      {pi = 3.141592654}      {need for Mac}
      binmax = 100;
      TimeBig = 100000;
   type
      component = array[1..Nmax] of real;
      list = array[1..Nmax] of integer;
      count = array[1..Nmax] of real;
      probability = array[-binmax..binmax] of LongInt;
      RunType = (MDLJ, MDHD, MCLJ, MCHD);
      flags = record    {flags for displaying temperature, etc.}
            T, D, v, g, change, show, disk, exit: boolean;
            run: RunType;
         end;
      Distribution = record
            s, v: probability;
            vmax, max, delta: real;
            bin: integer
         end;
      timestep = record
            dt: real;
            dt2: real
         end;
      length = record
            x, y: real;
            area, density: real
         end;
      position = record
            x, y: component;
            x0, y0: component;
         end;
      accel = record
            x, y: component;
         end;
      velocity = record
            x, y: component
         end;
      energy = record
            ke, kecum, ke2cum, pe, pecum, pe2cum, total: real;
         end;
      Temperature = record
            now, bar: real;
            temperature, beta: real;
            lambda, scaleup, scaledown: real;   {scale factors}
            desire: real;
            rescale: boolean;
         end;
      Pressure = record
            now: real;
            virial: real;
            vcum, vbar: real;
         end;
      correlation = record   {g(r)}
            cum: probability;
            nbin: integer;
            rmax, dr, max: real;
         end;
      plots = record
            hd, rsc: integer;   {hd hard disk radius, rsc plot size}
            MDLJtmax: real;     {maximum time difference shown on T(t) plot}
            tmax, tmin: real;   {updated time for T(t) plot}
            nmax, nmin: integer;  {corresponding # of iterationsvfor T(t) plot}
            MDLJtdiff: real;    {time interval for diffusion plot}
            ndiff: array[MDLJ..MCHD] of integer;
            ndmax, ndmin: integer;   {updated # of collisions}
            MCLJnmax: integer;   {maximum # of iterations on PE plot}
            R2max: integer;  {maximum displacment of R^2 during interval}
         end;
   var
      Menu: Tmenu;
      HotKeys: ThotKeys;
      Slider: Tsliders;
      N: integer;         {number of particles}
      r: position;
      v: velocity;
      a: accel;
      step: timestep;
      L: length;
      T: temperature;
      P: pressure;
      E: energy;
      R2: real;
      prob: distribution;
      time: real;
      nmcs, ncalc, ndiff: longint;
      flag: flags;
      g: correlation;
      plot: plots;
      knum: byte;
      seed, seed0: integer;
      start, overlap, quit: boolean;
      nbar: integer;
      naccept: real;  {use naccept for mean free time in MDHD}
      dmax, tij: real;
      CollisionTime: component;
      Partner: list;
      distx, disty : component;
      too_far : boolean;
      warning : boolean;

   procedure ShowTitle; {First screen of program}
      var
         i: integer;
         C: HelpScrType;
   begin
      for i := 1 to 25 do
         C[i] := '';
      C[1] := '      SIMULATION OF LIQUIDS, SOLIDS AND GASES  ';
      C[2] := '                       by           ';
      C[3] := '          Harvey Gould, Clark University       ';
      C[6] := '                 May 13, 1995         ';
      C[8] := '          (c) 1995, John Wiley & Sons           ';
      C[10] := ' A molecular dynamics and constant temperature';
      C[11] := ' Monte Carlo program for simulating a gas,    ';
      C[12] := ' liquid, and solid in two dimensions.         ';
      C[14] := ' Default potential is Lennard-Jones.          ';
      C[16] := ' Default algorithm is constant energy         ';
      C[17] := ' molecular dynamics.                          ';
      C[19] := ' Press any key or click mouse to continue.    ';
      C[21] := ' Press F2 or click mouse in trajectory window ';
      C[22] := ' to update configurations.                    ';
      HELP(C);
   end;     {ShowTitle}

   procedure AboutUnits;
      var
         i: integer;
         C: HelpScrType;
   begin
      for i := 1 to 25 do
         C[i] := '';
      if (flag.run = MDLJ) or (flag.run = MCLJ) then
         begin
            C[1] := ' The Lennard-Jones potential is characterized';
            C[2] := ' by a length, sigma, and an energy, epsilon. ';
            C[3] := ' Values for liquid argon are epsilon/k = 119.8 K';
            C[4] := ' and sigma = 3.405 angstroms. The constant k';
            C[5] := ' is Boltzmann''s constant. The mass m of an';
            C[6] := ' argon atom is 6.69 x 10^{-26} kg.';
            C[8] := ' It is convenient to express all quantities in';
            C[9] := ' terms of sigma, epsilon, and the mass.';
            C[11] := ' Velocity is measured in units of ';
            C[12] := ' (epsilon/m)^{1/2}.';
            C[13] := ' Time is measured in units of tau, where';
            C[14] := ' tau = (m sigma^2/epsilon)^{1/2}; tau is';
            C[15] := ' 1.82 x 10^{-12} sec for liquid argon';
         end;
      if (flag.run = MDHD) or (flag.run = MCHD) then
         begin
            C[1] := ' The interparticle interaction V(r) is infinity';
            C[2] := ' if r < sigma, and zero if r is greater than or ';
            C[3] := ' equal to sigma. The length sigma is the diameter';
            C[4] := ' of the hard disk.';
            C[6] := ' It is convenient to express all quantities in';
            C[7] := ' terms of sigma. For example, the density rho is';
            C[8] := ' measured in terms of sigma^{-d}, where d is the';
            C[9] := ' spatial dimension.';
         end;
      C[24] := ' Press any key or click mouse to continue.    ';
      HELP(C);
   end;

   procedure GeneralInfo;
      var
         i: integer;
         C: HelpScrType;
   begin
      for i := 1 to 25 do
         C[i] := '';
      C[01] := ' Subject : Thermal and Statistical Physics';
      C[02] := ' Program : ManyPart';
      C[03] := ' Title   : Simulation of liquids, solids and gases';
      C[04] := ' Author  : Harvey Gould, Clark University';
      C[05] := ' Abstract :';
      C[06] := '';
      C[07] := '  MANYPART allows the user to simulate a dense gas,';
      C[08] := '  liquid, or solid in two dimensions using either';
      C[09] := '  molecular dynamics (constant energy, constant';
      C[10] := '  volume) or Monte Carlo (constant temperature,';
      C[11] := '  constant volume) methods.  Both hard disks and';
      C[12] := '  the Lennard-Jones interaction can be chosen.';
      C[13] := '  The trajectories for the particles are shown';
      C[14] := '  as the system evolves.  Physical quantities of';
      C[15] := '  interest that are monitored include the';
      C[16] := '  pressure, temperature, heat capacity, mean';
      C[17] := '  square displacement, the distribution of the';
      C[18] := '  speeds and velocities, and the pair correlation';
      C[19] := '  function.  Important physical concepts that can';
      C[20] := '  be studied with the aid of the program include';
      C[21] := '  the Maxwell-Boltzmann probability distribution,';
      C[22] := '  fluctuations, equation of state, correlations,';
      C[23] := '  and the importance of chaotic mixing.';
      C[25] := '  Press any key or click mouse to continue.';
      HELP(C);
   end; {GeneralInfo}

   procedure HelpHotkeys;
      var
         i: integer;
         C: HelpScrType;
   begin {HelpHotKeys}
      for i := 1 to 25 do
         C[i] := '';
      C[1] := ' Choose F2 to begin the simulation.              ';
      C[2] := ' Choose F2 to pause if the simulation is running.';
      C[3] := ' Choose F3 to hide the trajectory and allow the  ';
      C[4] := ' program to generate new configurations faster.  ';
      C[5] := ' Choose F4 to reset the computations of averages.';
      C[25] := '      Press any key or click mouse to continue. ';
      case flag.run of
         MDLJ:
            begin
               C[6] := ' Choose F5 to refresh the path of each particle.';
               C[7] := ' Click on the temperature window to show the pair';
               C[8] := ' correlation function g(r). Click on the mean';
               C[9] := ' squared displacement window to hide/show of R^2.';
               C[11] := ' In Molecular Dynamics the classical equations of';
               C[12] := ' motion for each particle is integrated, and  ';
               C[13] := ' appropriate averages over the trajectories are';
               C[14] := ' computed. The second-order velocity form of the';
               C[15] := ' Verlet algorithm is used for the numerical';
               C[16] := ' integration. The interaction is the Lennard-Jones';
               C[17] := ' potential:';
               C[19] := ' U(r) = 4 epsilon [(sigma/r)^{12} - (sigma/r)^6]';
               C[21] := ' Lengths are measured in terms of sigma; energy';
               C[22] := ' is measured in terms of epsilon and temperature.';
               C[23] := ' is measured in terms of epsilon/k.';
            end;
         MDHD:
            begin
               C[6] := ' Choose F5 to refresh the path of each particle.';
               C[8] := ' Click on g(r) and the mean squared displacement';
               C[9] := ' windows to hide/show these windows.';
               C[11] := ' A molecular dynamics simulation of a system of';
               C[12] := ' hard disks. This portion of the program is based';
               C[13] := ' on M. P. Allen and D. J. Tildesley, Computer';
               C[14] := ' Simulation of Liquids, Clarendon Press (1987),';
               C[15] := ' pp. 101-105.';
               C[17] := ' The algorithm locates the next collision, moves';
               C[18] := ' all the particles in their direction of motion';
               C[19] := ' until the collision occurs, implements the';
               C[20] := ' changes in velocities of the colliding pair,';
               C[21] := ' and computes the quantities of interest.';
               C[22] := ' Algorithm not applicable if no collisions.';
            end;
         MCLJ:
            begin
               C[6] := ' Click on g(r) and the mean squared displacement';
               C[7] := ' windows to hide/show these windows.';
               C[9] := ' A constant volume Monte Carlo simulation of a ';
               C[10] := ' two-dimensional system of particles in        ';
               C[11] := ' equilibrium with a heat bath at temperature T. ';
               C[12] := ' The two-body interaction is the Lennard-Jones';
               C[13] := ' potential:   ';
               C[15] := ' U(r) = 4 epsilon [(sigma/r)^{12} - (sigma/r)^6]';
               C[17] := ' A particle is chosen at random and its position';
               C[18] := ' changed by a small, random amount. The change in';
               C[19] := ' potential energy is computed. If the change is';
               C[20] := ' negative, the trial move is accepted. If the ';
               C[21] := ' change is positive, the trial positions are';
               C[22] := ' accepted with the Boltzmann probability.';
            end;
         MCHD:
            begin
               C[6] := ' Click on g(r) and the mean squared displacement';
               C[7] := ' windows to hide/show these windows.';
               C[9] := ' A Monte Carlo simulation of hard disks.';
               C[10] := ' A particle is chosen at random and its position';
               C[11] := ' is changed by a small, random amount.';
               C[12] := ' If the new position does not overlap with any';
               C[13] := ' other hard disks, the trial move is accepted.';
               C[14] := ' Note that all physical quantities of interest';
               C[15] := ' are geometrical in nature.';
            end;
      end;  {case}
      Help(C);
   end;   {HelpHotkeys}

   procedure NewAxis( horiz, vert : string);
   var x,y : real;
       vp  : integer;
   begin
      vp := ViewPortNumber;
      SelectMuppetPort;
      SetFillStyle(SolidFill,black);
      with views[vp] do
      begin
         Bar(vx1-60,vy1-7,vx1-1,vy2+25); {vertical}
         Bar(vx1-60,vy2+1,vx2+12,vy2+25);
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
      SetColor(white);
      PutLabel(bottom, horiz);
      PutLabel(left, vert);
   end;

   procedure HandleHotKeys;
   forward;

   procedure HandleMenu;
   forward;

   function dmin (x, y: real): real;
   begin
      if x < y then
         dmin := x
      else
         dmin := y;
   end;

   function imin (x, y: integer): integer;
   begin
      if x < y then
         imin := x
      else
         imin := y;
   end;

   function ran (var iseed: integer): real;
   begin
      ran := urand(iseed);
   end;

   procedure InitializeMenu;
   begin
      Menu.init;
      with Menu do
         begin
            init;
            ColSize := 14;
            column(1, 'File');
            row(1, 1, 'About Program');
            row(1, 2, 'About CUPS');
            row(1, 3, 'About Units');
            row(1, 4, 'Configuration');
            row(1, 5, '-------------');
            row(1, 6, 'New System');
            row(1, 7, 'Open');
            row(1, 8, 'Save');
            row(1, 9, '-------------');
            row(1,10, 'Exit Program');   {Exit for PC}
            column(2, 'Molecular Dyn');
            row(2, 1, 'Lennard-Jones');
            row(2, 2, 'Hard Disks');
            column(3, 'Monte Carlo');
            row(3, 1, 'Lennard-Jones');
            row(3, 2, 'Hard Disks');
            column(4, 'Settings');
            row(4, 1, 'temperature');
            row(4, 2, 'time step');
            row(4, 3, 'box length');
            row(4, 4, 'Parameters');
            AutoChecking(2, 1);
            AutoChecking(3, 1);
            RowActivate(1, 8, false);
            RowActivate(1, 9, false);
            RowActivate(1, 5, false);
            display;
         end;
   end; {InitializeMenu}

   procedure SaveActivate;
   begin
      Menu.RowActivate(1, 8, true);
   end;

   procedure ClearMCcheck;
   begin
      with Menu do
         begin
            RowChecked(3, 1, false);
            RowChecked(3, 2, false);
         end;
   end;   {ClearMCcheck}

   procedure ClearMDcheck;
   begin
      with Menu do
         begin
            RowChecked(2, 1, false);
            RowChecked(2, 2, false);
         end;
   end;  {ClearMDcheck}

   procedure SetUpHotKeys;
   begin
      with HotKeys do
         begin
         init(6);
            key[1] := 'F1-Help';
            key[2] := 'F2-Run/Pause';
            key[3] := 'F3-Show/Hide';
            key[4] := 'F4-Reset';
            key[5] := 'F5-Refresh';
            key[6] := 'F10-Menu';
            Display;
         end;
   end;    {SetUpHotKeys}

   procedure SetUpShortHotKeys;
   begin
      with HotKeys do
         begin
            init(5);
            key[1] := 'F1-Help';
            key[2] := 'F2-Run/Pause';
            key[3] := 'F3-Show/Hide';
            key[4] := 'F4-Reset';
            key[5] := 'F10-Menu';
            Display;
         end;
   end;    {SetUpShortHotKeys}

   procedure SetupSlider;
      var
         PosX, PosY, Size, Valmin: real;
         Valmax: integer;
   begin
      Slider.init;
      PosX := 0.57;
      PosY := 0.25;
      Size := 0.9;
      Valmin := 0;
      Valmax := 10;
      Slider.Create(1, Valmin, Valmax, T.temperature, PosX, PosY, Size, 2, '0', '10', 'T', false);
   end;   {SetupSlider}

   procedure ChangeDiffusionDisplay;
      var
         r2 : real;
         i, xsc, ysc: integer;
         tStr : string;
   begin
      OpenViewPort(6);
      if flag.run = MDLJ then
         begin
         {used to reinitialize diffusion display}
            DefineScale(6, 0, plot.MDLJtdiff, 0, plot.R2max);
            plot.ndmax := Trunc(plot.MDLJtdiff / step.dt) + 1;
         end
      else
         begin
            DefineScale(6, 0, plot.ndiff[flag.run], 0, plot.R2max);
            plot.ndmax := plot.ndiff[flag.run];
         end;
      SelectScale(6);
      if flag.run <> MDHD then tStr := 'time difference'
      else tStr := 'number of collisions';
      NewAxis( tStr,'');
      PrintXY(0.61, 0.88, '<R ');
      PrintXY(0.633, 0.885, '2');
      PrintXY(0.643, 0.88, '>');
   end;    {ChangeDiffusionDisplay}

   procedure SetupDiffusionDisplay;
      var
         i: integer;
         xsc, ysc: integer;
         tStr : string;
   begin
      ndiff := 0;
      DefineViewPort(6, 0.60, 0.98, 0.70, 0.90);
      SetColor(white);
      if flag.D then
         OpenViewPort(6);
      plot.R2max := trunc(g.rmax * g.rmax);
      if flag.run = MDLJ then
         begin
         {used to reinitialize diffusion display}
            DefineScale(6, 0, plot.MDLJtdiff, 0, plot.R2max);
            plot.ndmax := Trunc(plot.MDLJtdiff / step.dt) + 1;
         end
      else
         begin
            DefineScale(6, 0, plot.ndiff[flag.run], 0, plot.R2max);
            plot.ndmax := plot.ndiff[flag.run];
         end;
      if flag.D then
         begin
            selectviewport(6);
            SelectScale(6);
            if flag.run <> MDHD then tStr := 'time difference'
            else tStr := 'number of collisions';
            NewAxis(tStr,'');
            PrintXY(0.61, 0.88, '<R ');
            PrintXY(0.637, 0.89, '2');
            PrintXY(0.645, 0.88, '>');
            Map(0, 0, xsc, ysc);
            SetColor(lightgreen);
            SetFillStyle(solidfill,lightgreen);
            FillEllipse(xsc, ysc, plot.rsc,plot.rsc);
         end;
        R2 := 0;
      for i := 1 to N do
         begin
            r.x0[i] := r.x[i];
            r.y0[i] := r.y[i]
         end;
   end;    {SetupDiffusionDisplay}

   procedure SetupTemperatureDisplay;
      var
         i: integer;
         Tmax: real;
         {TicX,} TicY: real;
         factor, dum: integer;
   begin
      DefineViewPort(9, 0.60, 0.98, 0.70, 0.90);
      Tmax := E.ke / N;
      SetColor(white);
      OpenViewPort(9);
   {   factor := 1;
      repeat
         dum := round(2 * factor * Tmax);
         TicY := dum / (4 * factor);
         factor := 10 * factor;
      until TicY > 0;   }
      If Tmax>0 then TMax:=2*TMax
      else tMax:=2;
      DefineScale(9, plot.tmin, plot.tmax, 0, tMax);
      SelectScale(9);
      NewAxis('time','');
      print(1, 1, 'Temperature');
   end;    {SetupTemperatureDisplay}

   procedure SetHeatBath;
   begin
      T.temperature := E.ke / N;
      T.beta := 1 / T.temperature;
      T.desire := T.temperature;
   end;   {SetHeatBath}

   procedure SetupTrajectoryDisplay;

   begin
      DefineScale(2, -0.01 * L.x, 1.01 * L.x, -0.01 * L.y, 1.01 * L.y);
      SelectScale(2);
      OpenViewPort(2);
   end;    {SetupTrajectoryDisplay}

   procedure SetupHDTrajectoryDisplay;
      var
         xpix, ypix: integer;
   begin
      DefineScale(2, -0.01 * L.x, 1.01 * L.x, -0.01 * L.y, 1.01 * L.y);
      SelectScale(2);
      OpenViewPort(2);
      with Views[2] do
         begin
            xpix := vx2 - vx1;
            ypix := vy2 - vy1;
         end;
      xpix := xpix div trunc(L.x);
      ypix := ypix div trunc(L.y);
      plot.hd := imin(xpix, ypix);
      plot.hd := plot.hd div 2;
   end;    {SetupHDTrajectoryDisplay}

   procedure SetupPotentialEnergyDisplay;   {called by MCLJ only}
      var
         Pemin: real;
         {TicX,} TicY: real;
   begin
      Pemin := E.pe;
      DefineViewPort(5, 0.60, 0.98, 0.40, 0.60);
      SetColor(white);
      OpenViewPort(5);
      TicY := int(1.2 * Pemin);
      TicY := abs(TicY) / 4;
      TicY := int(TicY + 1);
      DefineScale(5, plot.nmin, plot.nmax, -4 * TicY, 0);
      SelectScale(5);
      NewAxis('time','');
      print(1, 1, 'PE');
   end;    {SetupPotentialEnergyDisplay}

   procedure SetupDataDisplay;
   begin
      OpenViewPort(8);
   end;   {SetupDataDisplay}

   procedure SetupHDDataDisplay;
   begin
      OpenViewPort(8);
      setcolor(lightgreen);
      print(1, 1, concat('  mcs   = ', NumStr(nmcs, 3, 0)));
      print(1, 3, concat('density = ', NumStr(L.density, 5, 2)));
   end;   {SetupHDDataDisplay}

   procedure SetupDistributionDisplay;
      var
         TicX {TicY}: real;
   begin
      DefineViewPort(7, 0.6, 0.98, 0.33, 0.55);
      SetColor(white);
      OpenViewPort(7);
      TicX := int(prob.vmax * 100);
      TicX := TicX / 100;
      if flag.v then
         begin
            TicX := TicX / 5;
            DefineScale(7, 0, 5 * TicX, 0, prob.max);  {speed}
         end
      else
         begin
            TicX := TicX / 2;
            DefineScale(7, -2 * TicX, 2 * TicX, 0, 0.5 * prob.max);
         end;
      SelectScale(7);
      NewAxis('','');
      if flag.v then
         begin
            print(1, 1, 'P(v)');
            PutLabel(bottom, 'v');
         end
      else
         begin
            PrintXY(0.61, 0.54, 'P(v');
            PrintXY(0.655, 0.54, ')');
            PrintXY(0.81, 0.30, 'v');
            PrintXY(0.645, 0.525, 'x');  {subscript for P()}
            PrintXY(0.82, 0.29, 'x');  {subscript for horizonal axis}
         end;
   end;   {SetupDistributionDisplay}

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

   procedure closeup;
   begin
      closeViewPort(2);   {trajectory}
      closeViewPort(4);   {g(r)}
      closeViewPort(5);   {pressure}
      closeviewport(6);   {mean squared displacement}
      closeViewPort(7);   {speed distribution}
      closeViewPort(8);   {data}
      closeViewPort(9);   {temperature}
      closeViewPort(10);
      closeViewPort(11);
      ClearMuppetPort;
   end;  {closeup}

   procedure U (r2: real; var potential: real);
      var
         one_over_r2, one_over_r6: real;
   begin
      one_over_r2 := 1.0 / r2;
      one_over_r6 := one_over_r2 * one_over_r2 * one_over_r2;
      potential := 4.0 * one_over_r6 * (one_over_r6 - 1);
   end;   {procedure U}

   procedure f (r2: real; var force_over_r: real);
      var
         one_over_r2, one_over_r6: real;
   begin
      one_over_r2 := 1.0 / r2;
      one_over_r6 := one_over_r2 * one_over_r2 * one_over_r2;
      force_over_r := 24.0 * one_over_r2 * one_over_r6 * (2.0 * one_over_r6 - 1.0);
   {1/r factor compensated by factor of dx and dy for Fx and Fy}
   end;   {procedure f}

   procedure ShowTrajectories;
      var
         i: integer;
         xsc, ysc: integer;
   begin
      SelectViewPort(2);
      SelectScale(2);
      SetColor(lightgreen);
      HideMouse;
      for i := 1 to N do
         begin
            Map(r.x[i], r.y[i], xsc, ysc);
            Circle(xsc, ysc, plot.rsc);
         end;
      ShowMouse;
   end;     {ShowTrajectories}

   procedure HideTrajectories;
   begin
      CloseViewPort(2);
   end;    {HideTrajectories}

   procedure ShowR2 (ntemp: integer);    {called by all four algorithms}
      var
         xsc, ysc, delta: integer;
         R2mean, time_diff: real;
   begin
      delta := ntemp mod plot.ndmax;
      if (delta = 0) or (ntemp = 0) then
         SetupDiffusionDisplay;
      SelectViewPort(6);
      SelectScale(6);
      SetColor(lightgreen);
      R2mean := R2 / N;   {mean squared displacement}
      if flag.run = MDLJ then
         time_diff := delta * step.dt
      else
         time_diff := delta;
      Map(time_diff, R2mean, xsc, ysc);
      setfillStyle(solidfill,lightgreen);
      hidemouse;
      FillEllipse(xsc, ysc, plot.rsc,plot.rsc);
      showmouse;
   end;      {ShowR2}

   procedure ShowVelocityDistribution (ntemp: longint);
      var
         ibin, xsc, ysc: integer;
         {TicX, TicY,} normal_prob, vx: real;
   begin
      OpenViewPort(7);
      SelectScale(7);
      SetColor(white);
      NewAxis('','');
      PrintXY(0.61, 0.54, 'P(v');
      PrintXY(0.655, 0.54, ')');
      PrintXY(0.81, 0.30, 'v');
      PrintXY(0.645, 0.527, 'x');  {subscript for P()}
      PrintXY(0.82, 0.29, 'x');  {subscript for horizonal axis}
      SetColor(yellow);
      SetFillstyle(solidfill,yellow);
      Hidemouse;
      for ibin := -prob.bin to prob.bin do   {normalize velocity distribution}
         begin
           {factor of 2 since average over vx and vy to obtain better statistics}
            normal_prob := prob.v[ibin] / (2 * N * ntemp);
        {    if ibin = 0 then normal_prob := normal_prob/2;}
            vx := ibin * prob.delta;
            Map(vx, normal_prob, xsc, ysc);
            circle(xsc, ysc, plot.rsc)
         end;
         ShowMouse;
   end;      {ShowVelocityDistribution}

   procedure ShowSpeedDistribution (ntemp: longint);
      var
         ibin, xsc, ysc: integer;
         normal_prob, speed: real;
   begin
      OpenViewPort(7);
      SelectScale(7);
      SetColor(white);
      NewAxis('v','');
      print(1, 1, 'P(v)');
      SetColor(yellow);
      SetFillStyle(solidfill, yellow);
      HideMouse;
      for ibin := 1 to prob.bin do   {normalize speed distribution}
         begin
            normal_prob := prob.s[ibin] / (N * ntemp);
            speed := ibin * prob.delta;
            Map(speed, normal_prob, xsc, ysc);
            FillEllipse(xsc, ysc, plot.rsc,plot.rsc)
         end;
      ShowMouse;
   end;      {ShowSpeedDistribution}


   procedure ShowData;   {called by MDLJ}
      var
         Bkcolor: integer;
         Cv, denom, deltaK2, kebar, ke2bar: real;
   begin
      SelectViewPort(8);
      SetColor(lightgreen);
      BkColor := GraphBackColor;
      rubout(8, 1, 10, BkColor);
      Print (8, 1, NumStr(time, 5, 2));
      rubout(27, 1, 8, BkColor);
      print (27, 1, NumStr(E.total, 6, 3));
      SetColor(yellow);
      if (ncalc = 1) then begin
         rubout(1, 2, 33, BkColor);   {erase entire line}
         print(1,  2, '<T>  =');
         print(18, 2, '    <P> =');
         Print(1,  3, ' C   = ');
         PrintXY(0.58, 0.093, 'v');
      end;
      if not T.rescale then
         begin
            kebar := E.kecum / ncalc;
            rubout(8, 2, 7, BkColor);
            print(8, 2, NumStr(T.now, 5, 2));
            rubout(27, 2, 7, BkColor);
            print(27, 2, NumStr(P.now, 5, 2)); {!!!!}
            ke2bar := E.ke2cum / ncalc;  {mean squared kinetic energy}
            deltaK2 := ke2bar - kebar * kebar;
            denom := 1 - deltaK2 / (N * T.bar * T.bar);
            Cv := 1 / denom;  {specific heat per particle}
            rubout(8, 3, 7, BkColor);
            Print(8, 3,  NumStr(Cv, 5, 2));
         end
      else
         begin
            SetColor(lightgreen);
            RubOutXY(0.58, 0.093, 3, BkColor);
            rubout(1, 2, 15, BkColor);
            print(1, 2, concat(' T   =', NumStr(T.now, 5, 2)));
            rubout(18, 2, 15, BkColor);
            P.now := N * T.now + 0.5 * P.virial;
            P.now := P.now / L.area;     {pressure at time t}
            print(18, 2, concat('     P  =', NumStr(P.now, 5, 2)));
            rubout(1, 3, 16, BkColor);
         end;
   end;       {ShowData}

   procedure SetupPressureDisplay;
      var
         i: integer;
         Pmax: real;
         TicY: real;
         factor, dum: real;
   begin
      DefineViewPort(5, 0.05, 0.49, 0.58, 0.86);
      SetColor(white);
      OpenViewPort(5);
      Pmax := abs(P.now);
      factor := 1;
      repeat
         dum := round(2 * factor * Pmax);
         TicY := dum / (4 * factor);
         factor := 10 * factor;
      until TicY > 0;
      DefineScale(5, plot.tmin, plot.tmax, -TicY, 4 * TicY);
      SelectScale(5);
      NewAxis('time','');
      print(1, 1, 'Pressure');
   end;    {SetupPressureDisplay}

   procedure ShowPressure;
      var
         xsc, ysc, delta_steps: integer;
         cycle: integer;
         reset: boolean;
   begin
      reset := false;
      if ncalc < 0 then
         begin
            ncalc := -ncalc;
            reset := true;
         end;
      cycle := ncalc div plot.nmax;
      delta_steps := ncalc mod plot.nmax; {plot.nmax is integer # steps corresponding to plot.mdmax}
      if (delta_steps = 0) or (ncalc = 0) or reset then
         begin
            if not flag.T then
               begin
                  plot.tmax := (1 + cycle) * plot.MDLJtmax;
                  plot.tmin := cycle * plot.MDLJtmax;
               end;
            SetupPressureDisplay;
         end;
      SelectViewPort(5);
      SelectScale(5);
      SetColor(lightgreen);
      SetFillStyle(solidfill, lightgreen);
      Map(time, P.now, xsc, ysc);
      FillEllipse(xsc, ysc, plot.rsc, plot.rsc);
      if ncalc > 0 then
         begin
            if not T.rescale then
               begin
                  SetColor(yellow);
                  SetFillStyle(solidfill, yellow);
                  Map(time, P.vbar, xsc, ysc);
                  HideMouse;
                  FillEllipse(xsc, ysc, plot.rsc, plot.rsc);
                  ShowMouse;
               end;
         end;
   end;    {ShowPressure}

   procedure ShowTemperature;      {used by MDLJ only}
      var
         xsc, ysc, delta_steps: integer;
         cycle: integer;
         reset: boolean;
   begin
      reset := false;
      if ncalc < 0 then
         begin
            ncalc := -ncalc;
            reset := true;
         end;
      cycle := ncalc div plot.nmax;
      delta_steps := ncalc mod plot.nmax; {plot.nmax is integer # steps corresponding to plot.mdmax}
      if (delta_steps = 0) or (ncalc = 0) or reset then
         begin
            plot.tmax := (1 + cycle) * plot.MDLJtmax;
            plot.tmin := cycle * plot.MDLJtmax;
            SetupTemperatureDisplay;
         end;
      SelectViewPort(9);
      SelectScale(9);
      SetColor(lightgreen);
      SetFillStyle(solidfill, lightgreen);
      Map(time, T.now, xsc, ysc);
      FillEllipse(xsc, ysc, plot.rsc, plot.rsc);
      if not T.rescale then
         begin
            if ncalc > 0 then
               begin
                  SetColor(yellow);
                  SetFillStyle(solidfill, yellow);
                  Map(time, T.bar, xsc, ysc);
                  HideMouse;
                  FillEllipse(xsc, ysc, plot.rsc, plot.rsc);
                  ShowMouse;
               end;
         end;
   end;      {ShowTemperature}

   procedure ShowPotentialEnergy;      {called by MCLJ}
      var
         xsc, ysc: integer;
         delta, delta_steps: integer;
         pebar: real;
   begin
      delta := nmcs mod (plot.nmax - plot.nmin);
      if (delta = 0) or (nmcs = 0) then
         begin
            if nmcs = 0 then
               begin
                  plot.nmax := plot.MCLJnmax;
                  plot.nmin := 0;
               end
            else
               begin
                  delta_steps := plot.nmax - plot.nmin;
                  plot.nmax := plot.nmax + delta_steps;
                  plot.nmin := plot.nmin + delta_steps;
               end;
            SetupPotentialEnergyDisplay;
         end;
      SelectViewPort(5);
      SelectScale(5);
      SetColor(lightgreen);
      Map(nmcs, E.pe, xsc, ysc);
      circle(xsc, ysc, plot.rsc);
      pebar := E.pecum / (nmcs+0.0000001);
      SetColor(yellow);
      Map(nmcs, pebar, xsc, ysc);
      circle(xsc, ysc, plot.rsc);
   end;      {ShowPotentialEnergy}

   procedure ShowInitialData;   {called by MDLJ}
   begin
      T.now := E.ke / N;
      P.now := (N * T.now) + 0.5 * P.virial;
      P.now := P.now / L.area;
      SelectViewPort(8);
      SetColor(lightgreen);
      Print(1,  1, concat('time = ', NumStr(time, 1, 0)));
      print(18, 1, concat('     E  = ', NumStr(E.total, 6, 3)));
      print(1,  2, concat(' T   =', NumStr(T.now, 5, 2)));
      print(18, 2, concat('     P  =', NumStr(P.now, 5, 2)));
      print(18, 3, concat('density =', NumStr(L.density, 5, 2)));
   end;   {ShowInitialData}

   procedure ShowMCData;  {called by MCLJ}
      var
         Bkcolor: integer;
         pebar, pe2bar, Cv, norm, ratio: real;
   begin
      SelectViewPort(8);
      SetColor(lightgreen);
      BkColor := GraphBackColor;
      rubout(8, 1, 5, BkColor);
      print (1, 1, concat(' mcs = ', NumStr(nmcs, 4, 0)));
      if nmcs = 1 then
         begin
            SetColor(yellow);
            rubout(1, 2, 33, BkColor);
            print(1, 2, '<PE> = ');
            print(18, 2, '    <P> = ');
            print (18, 1, '  ratio =');
            rubout(1, 3, 15, BkColor);
            Print (1, 3, ' C   = ' );
            PrintXY(0.58, 0.093, 'v');
         end;
      if nmcs > 0 then
         begin
            norm  := 1 / (N * nmcs);
            ratio := naccept * norm;
            SetColor(yellow);
            rubout(27, 1, 8, BkColor);
            print (27, 1, NumStr(ratio, 5, 2));
            pebar := E.pecum / nmcs;
            rubout(8, 2, 13, BkColor);
            print (8,  2, Num2Str(pebar, 6));
            rubout(27, 2, 8, BkColor);
            print (27, 2, Num2Str(P.vbar, 5));
            pe2bar := E.pe2cum / nmcs;
            if T.temperature>0 then begin
               Cv := pe2bar - pebar * pebar;  {potential energy fluctuations}
               Cv := Cv * T.beta * T.beta;
               Cv := 1.5 * N + Cv;
               rubout(8, 3, 9, BkColor);
               Print (8, 3, Num2Str(Cv, 6));
            end;
         end;
      if nmcs = 0 then
         begin
            OpenViewPort(8);
            SetColor(lightgreen);
            print(1, 2, concat(' PE  = ', Num2Str(E.pe, 6)));
            P.now := (N * T.temperature) + 0.5 * P.virial;
            P.now := P.now / L.area;
            print(18, 2, concat('     P  = ', Num2Str(P.now, 5)));
            print(18, 3, concat('density =', NumStr(L.density, 5, 2)));
         end;
   end;   {ShowMCData}

   procedure ShowHDdata;  {called by MCHD}
      var
         Bkcolor: integer;
         dum: real;
   begin
      SelectViewPort(8);
      BkColor := GraphBackColor;
      rubout(10, 1, 12, BkColor);
      SetColor(lightgreen);
      print(11, 1, NumStr(nmcs, 3, 0));
      dum := 1.0 / nmcs;
      dum := dum / N;
      dum := naccept * dum;
      SetColor(yellow);
      print(1, 2, ' ratio  =');
      rubout(10, 2, 12, BkColor);
      print(10, 2, NumStr(dum, 5, 2));
   end;       {ShowHDData}

   procedure ShowInitialDisks;
      var
         i: integer;
         xsc, ysc: integer;
   begin
      OpenViewPort(2);
      SelectScale(2);
      HideMouse;
      SetColor(lightgreen);
      for i := 1 to N do
         begin
            Map(r.x[i], r.y[i], xsc, ysc);
            SetFillStyle(SolidFill, lightgreen);
            FillEllipse(xsc, ysc, plot.hd, plot.hd);
         end;
         ShowMouse;
   end;    {ShowInitialDisks;}

   procedure ShowInitialPositions;
      var
         i: integer;
         xsc, ysc: integer;
   begin
      OpenViewPort(2);
      SelectScale(2);
      SetColor(lightblue);
      SetFillStyle(Solidfill, lightblue);
      HideMouse;
      for i := 1 to N do
         begin
            Map(r.x[i], r.y[i], xsc, ysc);
            FillEllipse(xsc, ysc, 2 * plot.rsc, 2 * plot.rsc);
         end;
      ShowMouse;
   end;   {ShowInitialPositions;}

   procedure CalculateMaximumStepLength;
   begin
      dmax := sqrt(L.density);
      dmax := 1 / dmax;
      dmax := dmax / 5;
   end;   {CalculateMaximumStepLength}

   procedure InitializeMCVariables;
   begin
      nmcs := 0;
      ndiff := 0;
      naccept := 0;
      E.pecum := 0;
      E.pe2cum := 0;
   end;

   procedure InitializeMDvariables;
   begin
      time := 0.0;
      ncalc := 0;
      ndiff := 0;
   end;

   procedure InitializeProbabilityDistribution;
      var
         i: integer;
   begin
      for i := -prob.bin to prob.bin do
         begin
            prob.s[i] := 0;
            prob.v[i] := 0;
         end;
   end;   {InitializeProbabilityDistribution}

   procedure InitializeTemperature;
   begin
      E.kecum := 0.0;
      E.ke2cum := 0;
   end;  {InitializeTemperature}

   procedure InitializeVirial;
   begin  {called by InitialMCLJ and InitialMDHD}
      P.vcum := 0.0;
   end;   {InitializeVirial}

   procedure Initializeg_r;
      var
         i: integer;
   begin
      for i := 1 to g.nbin do
         g.cum[i] := 0;
   end;        {Initializeg_r}

   procedure DisplayDefaults;
   begin
      with plot do
         begin
            MDLJtmax := 1;      {max time shown on T(t) plot}
            if step.dt<>0 then
               plot.nmax := trunc(plot.MDLJtmax / step.dt) + 1
            else plot.nMax := 1;
            plot.nmin := 0;
            MDLJtdiff := 4;
            plot.ndiff[MDLJ] := 100;
            plot.ndiff[MDHD] := 100;
            plot.ndiff[MCLJ] := 100;
            plot.ndiff[MCHD] := 100;
            MCLJnmax := 100;    {maximum # of iterations for PE plot}
            plot.ndmax := Trunc(plot.MDLJtdiff / abs(step.dt)) + 1;
            rsc := 1;
         end;
      with prob do
         begin
            bin := 25;
            delta := 0.1;
            vmax := bin * delta;
            max := 0.16;
         end;
      nbar := 40;
      with flag do
         begin
            T := true;   {display temperature}
            v := true;   {display speed distribution}
         end;
   end;    {DisplayDefaults}

   procedure separation (var dx, dy: real);
   begin
      if dx > 0.5 * L.x then
         dx := dx - L.x
      else if dx < -0.5 * L.x then
         dx := dx + L.x;
      if dy > 0.5 * L.y then
         dy := dy - L.y
      else if dy < -0.5 * L.y then
         dy := dy + L.y
   end;   {separation}

   procedure KineticEnergy;
      var
         i: integer;
         v2: real;
   begin
      E.ke := 0;
      for i := 1 to N do
         begin
            v2 := v.x[i] * v.x[i] + v.y[i] * v.y[i];
            E.ke := E.ke + v2
         end;
      E.ke := 0.5 * E.ke;
   end;   {KineticEnergy}

   procedure Newparameters;
   forward;

   procedure CheckMinimumDistance (var dxmin, dymin: real);
      var
         dx, dy, r2, r2min: real;
         i, j: integer;
   begin
      dxmin := L.x;
      dymin := L.y;
      r2min := 10000;
      for i := 1 to N - 1 do
         begin
            for j := (i + 1) to N do
               begin
                  dx := r.x[i] - r.x[j];
                  dy := r.y[i] - r.y[j];
                  separation(dx, dy);
                  r2 := dx * dx + dy * dy;
                  if r2 < r2min then
                     begin
                        r2min := r2;
                        dx := abs(dx);
                        dy := abs(dy);
                        dxmin := dx;
                        dymin := dy;
                     end;
               end;
         end;
   end;   {CheckMinimumDistance}
 procedure InitialConditions;
begin
   r.x[1] := 0.16463;
   r.y[1] := 1.32606;
   v.x[1] := 0.45348;
   v.y[1] := 0.70945;
   r.x[2] := 3.30071;
   r.y[2] := 3.12369;
   v.x[2] := -2.24010;
   v.y[2] := 0.50159;
   r.x[3] := 3.81055;
   r.y[3] := 1.39697;
   v.x[3] := 1.37008;
   v.y[3] := 2.03635;
   r.x[4] := 2.53477;
   r.y[4] := 1.76324;
   v.x[4] := 0.70486;
   v.y[4] := 0.16871;
   r.x[5] := 1.09382;
   r.y[5] := 3.71761;
   v.x[5] := -0.13337;
   v.y[5] := -3.59282;
   r.x[6] := 1.31151;
   r.y[6] := 0.01920;
   v.x[6] := -2.77195;
   v.y[6] := 0.37992;
   r.x[7] := 0.33636;
   r.y[7] := 4.48875;
   v.x[7] := -0.70136;
   v.y[7] := -0.37179;
   r.x[8] := 1.56664;
   r.y[8] := 1.13613;
   v.x[8] := 0.76675;
   v.y[8] := -1.42628;
   r.x[9] := 4.12306;
   r.y[9] := 2.52172;
   v.x[9] := -1.16583;
   v.y[9] := -0.56468;
   r.x[10] := 3.61837;
   r.y[10] := 0.22779;
   v.x[10] := 0.35389;
   v.y[10] := 0.97841;
   r.x[11] := 2.50310;
   r.y[11] := 4.92282;
   v.x[11] := 0.25550;
   v.y[11] := -0.21263;
   r.x[12] := 4.36799;
   r.y[12] := 3.54247;
   v.x[12] := 1.72348;
   v.y[12] := 1.08366;
   r.x[13] := 2.56769;
   r.y[13] := 3.86761;
   v.x[13] := -1.66463;
   v.y[13] := -1.68093;
   r.x[14] := 2.28553;
   r.y[14] := 2.79469;
   v.x[14] := 2.82721;
   v.y[14] := 1.90850;
   r.x[15] := 0.14960;
   r.y[15] := 2.81467;
   v.x[15] := -0.88822;
   v.y[15] := -1.30965;
   r.x[16] := 1.26565;
   r.y[16] := 2.33660;
   v.x[16] := 1.11020;
   v.y[16] := 1.39221;
end;   {InitialConditions}



   procedure CheckTotalMomentum;
      var
         vxcum, vycum: real;
         i: integer;
   begin
      vxcum := 0;
      vycum := 0;
      for i := 1 to N do
         begin
            vxcum := vxcum + v.x[i];
            vycum := vycum + v.y[i];
         end;
      vxcum := vxcum / N;   {check total momentum}
      vycum := vycum / N;
      for i := 1 to N do
         begin
            v.x[i] := v.x[i] - vxcum;
            v.y[i] := v.y[i] - vycum
         end
   end;    {CheckTotalMomentum}

   procedure XOpenFile;
      var
         i: integer;
         temp: real;
         FileName,s: string;
         f: text;
   begin
      FileName := OpenFile('*.mnp');
      if FileName <> '' then
      begin
         assign(f, Filename);
         reset(f);
         readln(f, s);
         if (s='File created by MANYPART.EXE') then
         begin
            readln(f, N);
            readln(f, L.x, L.y);
            if (flag.run <> MCLJ) then
               readln(f)
            else
               readln(f, T.temperature);
            readln(f);  {corresponds to coordinates}
            for i := 1 to N do
               readln(f, r.x[i], r.y[i]);
            if (flag.run <> MCLJ) or (flag.run <> MCHD) then
               begin
                  readln(f);
                  for i := 1 to N do
                     readln(f, v.x[i], v.y[i]);
               end;
         end
         else Announce('This file was not created by ManyPart.exe');
         close(f)
      end;
      flag.change := true;
   end;   {XOpenFile}


   procedure SaveData (ntemp: longint);
      var
         SaveScreen: TInputScreen;
         C: HelpScrType;
         FileName: string;
         f: text;
         i, m: integer;
         gr, norm, dist, area, vx, speed: real;
   begin
      for i := 1 to 25 do
         C[i] := '';
      C[2] := 'The number of particles, the linear dimensions of';
      C[3] := 'the central cell, the temperature (if relevant),';
      C[4] := 'the x and y coordinates, and the x and y';
      C[5] := 'velocities (if relevant) of each particle are';
      C[6] := 'saved in a file.';
      C[8] := 'Also saved is the pair distribution function g(r).';
      C[10] := 'File names have the form *.mnp.';
      C[24] := '      Press any key or click mouse to continue. ';
      if (flag.run = MDLJ) or (flag.run = MDHD) then
         filename := 'md.mnp'
      else
         filename := 'mc.mnp';
      with SaveScreen do
         begin
            Init;
            DefineInputPort(0.00, 0.43, 0.70, 0.90);
            loadline('Enter filename "             "');
            loadline('');
            Loadline('[  Ok  ]   [Cancel]   [ Help ]');
            setstring(1, Filename);
            setHelpScreen(C);
            AcceptScreen;
            Filename := getstring(1);
            if (not canceled) and (filename<>'') then
            begin
                  m := Pos('.',FileName)+1;
                  if copy(FileName,m,3)<>'MNP' then
                  begin
                     Delete(FileName,m,3);
                     Insert('MNP',FileName,m);
                  end;
                  assign(f, Filename);
                  rewrite(f);
                  writeln(f, 'File created by MANYPART.EXE');
                  writeln(f, N : 3);
                  writeln(f, L.x : 6 : 3,' ', L.y : 6 : 3);
                  if (flag.run = MDLJ)  then
                     writeln(f, T.bar : 6 : 3)
                  else if (flag.run = MCLJ) or (flag.run = MDHD)then
                     writeln(f, T.temperature : 6 : 3)
                  else
                     writeln(f);
                  writeln(f, 'coordinates');
                  for i := 1 to N do
                     writeln(f, r.x[i] : 10 : 6, r.y[i] : 10 : 6);
                  if (flag.run = MDLJ) or (flag.run = MDHD) then
                     begin
                        writeln(f, 'velocities');
                          for i := 1 to N do
                           writeln(f, v.x[i] : 10 : 6,' ',v.y[i] : 10 : 6);
                     end;
                  writeln(f, 'averages');
                  case flag.run of
                     MDLJ:
                        begin
                           writeln('Molecular dynamics: Lennard-Jones');
                        end;
                  end;
                  writeln(f, 'g(r)');
                  norm := 2.0 / (L.density * ntemp * N);
                  for i := 1 to g.nbin do
                     begin
                        if g.cum[i] > 0 then
                           begin
                              dist := i * g.dr;
                              area := 2 * pi * dist * g.dr;
                              gr := g.cum[i] * norm / area;
                              writeln(f, dist : 4 : 2, gr : 6 : 3);
                           end;
                     end;
                  if (flag.run = MDLJ) or (flag.run = MDHD) then
                     begin
                        writeln(f, 'velocity distribution');
                        for i := -prob.bin to prob.bin do   {normalize velocity distribution}
                           begin
                              norm := prob.v[i] / (2 * N * ntemp);
                              vx := i * prob.delta;
                              writeln(f, vx : 4 : 2, norm : 8 : 4);
                           end;
                        writeln(f, 'speed distribution');
                        for i := 1 to prob.bin do   {normalize speed distribution}
                           begin
                              norm := prob.s[i] / (N * ntemp);
                              speed := i * prob.delta;
                              writeln(f, speed : 4 : 2, norm : 8 : 4);
                           end;
                     end;
                  close(f);
               end;
            done;
         end;  {with}
   end;   {savedata}

   procedure DisplayOptions;
      var
         DisplayScreen: TInputScreen;
         C: HelpScrType;
         i, dum: integer;
         temp: real;
   begin
      for i := 1 to 25 do
         C[i] := '';
      case flag.run of
         MDLJ:
            begin
               C[1] := 'The temperature and pressure at time t are';
               C[2] := 'plotted in green. The time averages of the';
               C[3] := 'temperature and pressure are shown in yellow.';
               C[5] := 'The time axis is updated periodically. For';
               C[6] := 'example, if the time interval is equal to 4,';
               C[7] := 'the time axis will read 0--4, 4--8, etc.';
               C[9] := 'Choose the time interval for the mean square';
               C[10] := 'displacement so that <R^2> increases by approx-';
               C[11] := 'imately one-half of its maximum allowed value.';
               C[13] := 'The time is measured in terms of the quantity';
               C[14] := 'tau.';
               C[25] := '      Press any key or click mouse to continue. ';
            end;
         MDHD:
            begin
               C[2] := 'Choose the time interval for the mean square';
               C[3] := 'displacement so that <R^2> increases by';
               C[4] := 'approximately one-half of its maximum allowed';
               C[5] := 'value.';
               C[7] := 'The x-axis is the number of collisions. It can';
               C[8] := 'be converted to the equivalent time by';
               C[9] := 'multiplying the number of collisions by the';
               C[10] := 'mean time between collisions.';
            end;
         MCLJ:
            begin
               C[2] := 'The potential energy is plotted as a function';
               C[3] := 'of the number of Monte Carlo steps per particle.';
               C[4] := 'If the maximum interval is equal to 100 MC steps,';
               C[5] := 'the x-axis will read 0--100, 100--200, etc.';
               C[7] := 'Choose the interval for the mean square';
               C[8] := 'displacement so that <R^2> increases by ';
               C[9] := 'approximately one-half of its maximum allowed';
               C[10] := 'value.';
            end;
         MCHD:
            begin
               C[2] := 'Choose the interval for the mean square';
               C[3] := 'displacement so that <R^2> increases by ';
               C[4] := 'approximately one-half of its maximum allowed';
               C[5] := 'value.';
            end;
      end; {case}
      with DisplayScreen do
         begin
            Init;
            with plot do
               begin
                  case flag.run of
                     MDLJ:
                        begin
                           DefineInputPort(0.25, 0.85, 0.35, 0.65);
                           loadline('time interval for T(t) and P(t) plot = {    }');
                           loadline('time interval for <R^2>              = {    }');
                           setnumber(1, MDLJtmax);
                           setnumberLimits(1, 0.01, 100);
                           setnumber(2, MDLJtdiff);
                           setnumberLimits(2, 0.01, 100);
                        end;
                     MDHD:
                        begin
                           DefineInputPort(0.25, 0.85, 0.35, 0.65);
                           loadline('number of collisions for <R^2> = {    }');
                           setnumber(1, ndiff[flag.run]);
                           setnumberLimits(1, 1, 100);
                     end;
                     MCLJ:
                        begin
                           DefineInputPort(0.25, 0.85, 0.3, 0.7);
                           loadline('number of MC steps for PE plot = {    }');
                           loadline('number of MC steps for <R^2>   = {    }');
                           loadline('Show path of particle or disk?');
                           loadline(' #3 path   #3 disk ');
                           setnumber(1, MCLJnmax);
                           setnumberLimits(1, 1, 100);
                           setnumber(2, ndiff[flag.run]);
                           setnumberLimits(2, 1, 100);
                           if flag.disk then
                              setRadioButton('3', 2)
                           else
                              setRadioButton('3', 1);
                        end;
                     MCHD:
                        begin
                           DefineInputPort(0.25, 0.85, 0.35, 0.65);
                           loadline('number of MC steps for <R^2> = {    }');
                           loadline('Show path of particle or disk?');
                           loadline('#2 path   #2 disk ');
                           setnumber(1, ndiff[flag.run]);
                           setnumberLimits(1, 1, 100);
                           if flag.disk then
                              setRadioButton('2', 2)
                           else
                              setRadioButton('2', 1);
                        end;
                  end; {case}
                  loadline('');
                  Loadline('[  Ok  ]   [Cancel]   [ Help ]');
                  SetHelpScreen(C);
                  AcceptScreen;
                  if not (canceled) then
                     begin
                        case flag.run of
                           MDLJ:
                              begin
                                 temp := getnumber(1);
                                 if temp <> MDLJtmax then
                                    begin
                                       MDLJtmax := temp;
                                       plot.tmax := MDLJtmax;
                                       plot.tmin := 0;
                                       if step.dt<>0 then
                                       plot.nmax := trunc(MDLJtmax / abs(step.dt)) + 1
                                       else plot.nmax := 1;
                                       if flag.T then
                                          begin
                                             SetupTemperatureDisplay;
                                          end;
                                    end;
                                 temp := getnumber(2);
                                 if temp <> MDLJtdiff then
                                    begin
                                       MDLJtdiff := temp;
                                       if flag.D then
                                          ChangeDiffusionDisplay;
                                    end;
                              end;
                           MDHD:
                              begin
                                 dum := trunc(getnumber(1));
                                 if ndiff[flag.run] <> dum then
                                    begin
                                       ndiff[flag.run] := dum;
                                       ChangeDiffusionDisplay;
                                       flag.D := true;
                                    end;
                              end;
                           MCLJ:
                              begin
                                 dum := trunc(getnumber(1));
                                 if MCLJnmax <> dum then
                                    begin
                                       MCLJnmax := dum;
                                       nmax := MCLJnmax;
                                       nmin := 0;
                                       SetupPotentialEnergyDisplay;
                                    end;
                                 dum := trunc(getnumber(2));
                                 if ndiff[flag.run] <> dum then
                                    begin
                                       ndiff[flag.run] := dum;
                                       ChangeDiffusionDisplay;
                                       flag.D := true;
                                    end;
                                 dum := getradiobutton('3');
                              end;
                           MCHD:
                              begin
                                 dum := trunc(getnumber(1));
                                 if ndiff[flag.run] <> dum then
                                    begin
                                       ndiff[flag.run] := dum;
                                       ChangeDiffusionDisplay;
                                       flag.D := true;
                                    end;
                                 dum := getradiobutton('2');
                              end;
                        end; {case}
                        if (flag.run = MCLJ) or (flag.run = MCHD) then
                           begin
                              if dum = 1 then
                                 begin
                                    if flag.disk = true then
                                       begin
                                          ShowInitialPositions;
                                          HotKeys.Clear;
                                          SetUpHotKeys;
                                       end;
                                    flag.disk := false;
                                 end
                              else
                                 begin
                                    if flag.disk = false then
                                       begin
                                          ShowInitialDisks;
                                          HotKeys.Clear;
                                          SetUpShortHotKeys;
                                       end;
                                    flag.disk := true;
                                 end;
                           end;
                     end; {not canceled}
               end;   {with plot}
            done;
         end;   {with}
   end;     {DisplayOptions}

   procedure GaussianDistribution (temp: real);
      var
         i, isum, icomp: integer;
         v2: real;
         sum: real;
   begin
      for i := 1 to N do
         begin
            for icomp := 1 to 2 do
               begin
                  sum := 0;
                  for isum := 1 to 12 do
                     sum := sum + ran(seed);   {sum has mean value 6 and variance 1}
                  sum := sum - 6.0;   {sum has mean value zero}
                  if icomp = 1 then
                     v.x[i] := sqrt(temp)*sum
                  else
                     v.y[i] := sqrt(temp)*sum;
               end;   {sum over x and y components}
         end;  {sum over particles}
      CheckTotalMomentum;
      KineticEnergy;
   end;   {GaussianDistribution}

   procedure RandomStart;
      var
         i: integer;
   begin
      for i := 1 to N do
         begin
            r.x[i] := ran(seed) * L.x;
            r.y[i] := ran(seed) * L.y;
         end;  {sum over particles}
   end;   {RandomStart}

   procedure SquareLatticeStart;
      var
         i, icol, irow, ncol, nrow: integer;
         ny, nx, ratio, latRatio: real;
   begin
 {place particles on rectangular grid}
      ratio := L.x/L.y;
      nrow  := Trunc(sqrt(N/Ratio));
      ncol  := Trunc(ratio*nrow);
      while nrow * ncol < N do
      begin
         latRatio := ncol/nrow;
         if (latRatio>ratio) then nrow := nrow + 1
         else ncol := ncol + 1;
      end;
      nx := L.x / ncol;   {lattice spacing}
      ny := L.y / nrow;
      i := 0;
      for icol := 1 to ncol do
         begin
            for irow := 1 to nrow do
               if i<nMax then
               begin
                  i := i + 1;
                  r.y[i] := ny * (irow - 0.5);
                  r.x[i] := nx * (icol - 0.5);
               end;     {irow}
         end;   {icol}
   end;   {SquareLatticeStart}

   procedure TriangularLatticeStart;
   begin
   end;   {TriangularLatticeStart}

   procedure ChangeBoxLength;
      var
         ParamScreen: TInputScreen;
         C: HelpScrType;
         i: integer;
         Lxold, Lyold, Lxlower, Lylower: real;
         r2, r1, dxmin, dymin: real;
         alert: string;
   begin
      Lxold := L.x;
      Lyold := L.y;
      for i := 1 to 25 do
         C[i] := '';
      if flag.run <> MDHD then
         begin
            C[1] := 'Change the size of the box by changing either the';
            C[2] := 'horizontal and/or the vertical dimensions.';
            C[3] := 'Remember that the system cannot be compressed too';
            C[4] := 'quickly.';
            C[6] := 'The size of the box is reduced by changing the';
            C[7] := 'positions of the particles to account for periodic';
            C[8] := 'boundary conditions.';
         end;
      case flag.run of
         MDLJ:
            begin
               C[9] := 'It is important that the product of the force on';
               C[10] := 'any particle and the square of the time step';
               C[11] := 'be much less than one. For this reason a five';
               C[12] := 'percent limit has been imposed on the decrease';
               C[13] := 'of the linear dimension.';
               C[15] := 'In spite of this restriction, it is still possible';
               C[15] := 'that the numerical integration method will fail';
               C[16] := 'and the total energy will not be conserved.';
            end;
         MDHD:
            begin
               C[1] := 'It is impossible to compress the system of hard';
               C[2] := 'disk during a molecular dynamics run because the';
               C[3] := 'program exits the updates of positions and';
               C[4] := 'velocities immediately after a collision occurs.';
            end;
         MCLJ:
            begin
               C[9] := 'No restriction on the change in linear dimensions';
               C[10] := 'of the system is necessary.';
            end;
         MCHD:
            begin
               C[9] := 'It is important that the hard disks do not overlap';
            end;
      end;  {case}
      C[25] := '      Press any key or click mouse to continue. ';
      if (flag.run = MCHD) then
         CheckMinimumDistance(dxmin, dymin);     {check for minimum distance between hard disks}
      with ParamScreen do
         begin
            init;
            DefineInputPort(0.00, 0.55, 0.65, 0.90);
            case flag.run of
               MDLJ:
                  begin
                     Lxlower := 0.95 * L.x;
                     Lylower := 0.95 * L.y;
                  end;
               MDHD:
                  begin
                     Lxlower := L.x;
                     Lylower := L.y;
                     loadline('Box cannot be compressed ');
                     loadline('(except at beginning of run).');
                  end;
               MCLJ:
                  begin
                     Lxlower := 0.95 * L.x;
                     Lylower := 0.95 * L.y;
                  end;
               MCHD:
                  begin
                     if (dxmin < 1) and (dymin < 1) then
                        begin
                           r2 := dxmin * dxmin + dymin * dymin;
                           r1 := sqrt(r2);
                           Lxlower := L.x / r1;
                           Lylower := L.y / r1;
                        end
                     else if dxmin < 1 then
                        begin
                           Lxlower := L.x;
                           Lylower := L.y / dymin;
                        end
                     else if dymin < 1 then
                        begin
                           Lxlower := L.x / dxmin;
                           Lylower := L.y;
                        end;
                  end;
            end;   {case}
            if flag.run <> MDHD then
               begin
                  alert := concat('Smallest x and y dimensions are ', NumStr(Lxlower, 3, 1));
                  alert := concat(alert, ', ');
                  alert := concat(alert, NumStr(Lylower, 3, 1));
                  alert := concat(alert, '.');
                  loadline(alert);
                  loadline('No upper limit.');
               end;
            loadline('x and y dimensions of box = {    } {    }');
            setnumber(1, L.x);
            setnumber(2, L.y);
            setnumberlimits(1, Lxlower, 10 * L.x);
            setnumberlimits(2, Lylower, 10 * L.y);
            loadline('');
            loadline('[  Ok  ]   [Cancel]   [ Help ]');
            SetHelpScreen(C);
            AcceptScreen;
            if not canceled then
               begin
                  start := false;
                  flag.change := true;
                  L.x := getnumber(1);
                  L.y := getnumber(2);
                  L.area := L.x * L.y;
                  L.density := N / L.area;
                  CalculateMaximumStepLength;
                  if (L.x < Lxold) or (L.y < Lyold) then
                     begin  {positions are changed to reflect periodic boundary conditions}
                        for i := 1 to N do
                           repeat
                              if r.x[i] > L.x then
                                 r.x[i] := r.x[i] - L.x;
                              if r.y[i] > L.y then
                                 r.y[i] := r.y[i] - L.y;
                           until (r.x[i] <= L.x) and (r.y[i] <= L.y);
                     end;  {rescale lengths}
               end; {not canceled}
            done;
         end;   {with}
   end;   {ChangeBoxLength}

   procedure CheckConfiguration;
 {check configuration for overlap of hard disks}
      var
         dx, dy, r2: real;
         i, j: integer;
   begin
      overlap := false;
      for i := 1 to N - 1 do
         begin
            for j := (i + 1) to N do
               begin
                  dx := r.x[i] - r.x[j];
                  dy := r.y[i] - r.y[j];
                  separation(dx, dy);
                  r2 := dx * dx + dy * dy;
                  if r2 < 0.999999999 then
                     begin
                        overlap := true;
                     end;
               end;
         end;
   end;   {CheckConfiguration}

   procedure overlapmessage (ntemp: integer);
   begin
      Announce('Configuration not acceptable, because at least two disks overlap.')
   end;   {overlapmessage}

procedure Newparameters;
var
   ParamScreen: TinputScreen;
   C: HelpScrType;
   i, pos, vel, nOld: integer;
   temp: real;
   ratio : real;
   ratioOK : boolean;
begin
   for i := 1 to 25 do
       C[i] := '';
   C[2] := 'Change the area of the box by retaining the';
   C[3] := 'present coordinates in memory and changing either';
   C[4] := 'the horizontal and/or vertical dimensions.';
   C[6] := 'Remember that the system cannot be compressed ';
   C[7] := 'too quickly.';
   C[10] := 'Initial conditions can be generated by ';
   C[11] := 'starting the particles on a lattice and choosing';
   C[12] := 'the velocities at random from a Gaussian';
   C[13] := 'distribution. The temperature can be changed';
   C[14] := 'gradually until the desired temperature is ';
   C[15] := 'reached.';
   C[17] := 'If you increase the number of particles, make sure';
   C[18] := 'that you increase the size of the box.';
   C[19] := 'It is essential that hard disks do not overlap.';
   C[25] := '  Press any key or click mouse to continue. ';
   with ParamScreen do
   begin
      Init;
      DefineInputPort(0, 0.57, 0.25, 0.85);
      loadline('Number density must be less then one.');
      loadline('');
      loadline('no. of particles = {     } (10-200)');
      setnumber(1, N);    setNumberLimits(1, 10, 200);
      loadline('width  of box    = {     } (2-100)');
      loadline('height of box    = {     } (2-100)');
      setnumber(2, L.x);  setNumberLimits(2, 2, 100);
      setnumber(3, L.y);  setNumberLimits(3, 2, 100);
      loadline('random no. seed  = {     } (prime number)');
      setnumber(4, seed0);  setNumberLimits(4, 1, maxint);
      if (flag.run <> MCHD) then
      begin
         loadline('temperature      = {     }');
         if flag.run = MCLJ then setnumber(5, T.temperature)
         else setnumber(5, T.now);
         setnumberlimits(5, 1e-20,1000);
      end;
      loadline('initial positions:');
      if (flag.run = MDLJ) then loadline(' #5 square lattice  #5 current')
      else
      if (flag.run = MCLJ) then
       loadline(' #5 square lattice  #5 current  #5 random ')
      else   {hard disks}
      begin
         if overlap then
         begin
            loadline('Two hard disks overlap. Start from');
            loadline(' #5 square lattice.');
         end else begin
            CheckConfiguration;
            loadline(' #5 square lattice  #5 current ');
         end;
      end;
      setRadioButton('5', 1);
      if (flag.run = MDLJ) or (flag.run = MDHD) then
      begin
         loadline('initial velocities:');
         loadline(' #7 random          #7 current');
         setRadioButton('7', 1);
      end;
      loadline('');
      loadline('[  Ok  ]   [Cancel]   [ Help ]');
      SetHelpScreen(C);
      NOld := N;
      repeat
         AcceptScreen;
         N := Trunc(getnumber(1));
         L.x := getNumber(2);
         L.y := getNumber(3);
         seed0 := trunc(getNumber(4));
         seed  := -seed0;
         pos := getradiobutton('5');
         if (flag.run=MDLJ) or (flag.run=MDHD) then
          vel :=getradiobutton('7');
         ratio := L.x/L.y;
         RatioOK := (ratio<=3.1) and (ratio>=1/3.1);
         if not RatioOK then
          announce('Box aspect ratio must be between 1/3 and 3');
         L.area := L.x * L.y;
         L.density := N / L.area;
         if RatioOK and (L.density>1) then
          announce('Number density must be less then one.');
      until RatioOK and (L.density<=1);
      if not canceled then
      begin
         start := false;
         flag.change := true;
         CalculateMaximumStepLength;
         if (flag.run <> MCHD) then
         begin
            temp := getnumber(5);
            if flag.run = MCLJ then
            begin
               T.temperature := temp;
               if T.temperature>0 then
                  T.beta := 1 / T.temperature;
            end else T.desire := temp;   {for MD}
         end;  {if flag.run <> MCHD}
         case pos of
          1: SquareLatticeStart;       {latice}
          2: if (N>NOld) then SquareLatticeStart;       {current}
          3: RandomStart;  {random positions}
         end;
         if overlap then
         begin
            SquareLatticeStart;
            overlap := false;
            ClearMuppetPort;
            menu.display;
            if (flag.run = MDHD) then
              Comment(0, 0.95, 'Molecular Dynamics: Hard Disks')
            else Comment(0, 0.95, 'Monte Carlo: Hard Disks');
         end;
         if (N>NOld) then begin
            if (vel=2) then
              announce('Velocities chosen at random because no current information for new particles');
            if (pos=2) then
              announce('Particles put on lattice because no current information for new particles');
            if (vel=1) then GaussianDistribution(temp); {choose velocities from random distribution}
         end;
      end;   {not canceled}
      done;
   end;   {with}
   warning := false;
end;   {Newparameters}

   procedure GetDefaults;
      var
         ibin, diff: integer;
   begin
      N := 16;
      L.x := 5;
      L.y := 5;
      L.area := L.x * L.y;
      L.density := N / L.area;
      CalculateMaximumStepLength;
      step.dt := 0.01;
      step.dt2 := step.dt * step.dt;
      seed0 := 101;
      seed := -seed0;
      g.dr := 0.1;
      g.max := 3;
      g.rmax := 0.5 * dmin(L.x, L.y);  {maximum separation between particles}
      g.nbin := trunc(g.rmax / g.dr) + 1;
      if g.nbin > binmax then
         begin
            g.nbin := binmax;
            g.dr := g.rmax / g.nbin;
         end;
      plot.R2max := trunc(g.rmax * g.rmax);
      T.scaledown := 0.997;
      T.scaleup := 1.002;
   end;    {GetDefaults}

   procedure Setupg_of_rDisplay;
      var
          TicY: real;
   begin
      DefineViewPort(4, 0.05, 0.49, 0.58, 0.86);
      SetColor(white);
      OpenViewPort(4);
      g.rmax := 0.5 * dmin(L.x, L.y);  {maximum separation between particles}
      g.nbin := trunc(g.rmax / g.dr) + 1;
      if g.nbin > binmax then
         begin
            g.nbin := binmax;
            g.dr := g.rmax / g.nbin;
         end;
      TicY := g.max / 3;
      g.rmax := round(10 * g.rmax);
      g.rmax := g.rmax / 10;
      DefineScale(4, 0, g.rmax, 0, 4 * TicY);
      SelectScale(4);
      NewAxis('r','');
      print(1, 1, 'g(r)');
   end;    {Setupg_of_rDisplay}


   procedure Showg_of_r (naverage: integer);  {compute normalized g(r) and plot results}
      var
         ibin, xsc, ysc: integer;
         norm, area, gplot, gold, dist: real;
         nticks, factor: integer;
   begin
      OpenViewPort(4);
      SelectScale(4);
      NewAxis('r','');
      setcolor(white);
      print(1, 1, 'g(r)');
      SetColor(yellow);
      norm := 2.0 / (L.density * naverage * N);
      Map(0.5 * g.dr, 0, xsc, ysc);
      SetFillStyle(SolidFill, blue);
      Circle(xsc, ysc, 1);
      gold := 0;
      for ibin := 1 to (g.nbin - 1) do
         begin
            dist := (ibin + 0.5) * g.dr;
            area := 2 * pi * dist * g.dr;
            gplot := g.cum[ibin] * norm / area;
            Map(dist, gplot, xsc, ysc);
            Circle(xsc, ysc, 1);
            PlotLine((ibin - 0.5) * g.dr, gold, (ibin + 0.5) * g.dr, gplot);
            gold := gplot;
         end;
   end;  {Showg_of_r}

   procedure CorrelationFunction;
      var
         i, j, ibin: integer;
         r1, r2, dx, dy, rbin: real;
   begin
      for i := 1 to N - 1 do
         begin
            for j := i + 1 to N do
               begin
                  dx := r.x[i] - r.x[j];
                  dy := r.y[i] - r.y[j];
                  separation(dx, dy);
                  r2 := dx * dx + dy * dy;
                  r1 := sqrt(r2);
                  r1 := r1 / g.dr;
                  rbin := (r1 + g.dr);
                  if rbin > g.nbin then
                     ibin := g.nbin
                  else ibin := trunc(rbin);
                  g.cum[ibin] := g.cum[ibin] + 1;
               end;
         end;
   end;  {CorrelationFunction}

   procedure TotalEnergy;
   begin
      E.total := E.pe + E.ke;
   end;  {TotalEnergy}

   procedure PotentialEnergy;
      var
         i, j: integer;
         dx, dy, r2, potential: real;
   begin   {compute potential energy}
      E.pe := 0.0;
      for i := 1 to (N - 1) do
         for j := (i + 1) to N do
            begin
               dx := r.x[i] - r.x[j];
               dy := r.y[i] - r.y[j];
               separation(dx, dy);
               r2 := dx * dx + dy * dy;
               U(r2, potential);
               E.pe := E.pe + potential;
            end;
   end;  {PotentialEnergy}

   procedure TimeAverages;
   begin
      T.now := E.ke / N;    {ke computed in procedure Verlet at every time step}
      P.now := N * T.now + 0.5 * P.virial;
      P.now := P.now / L.area;     {pressure at time t}
      if not T.rescale then
         begin
            E.kecum := E.kecum + E.ke;   {used to compute running average of temperature}
            T.bar := E.kecum / (N * ncalc);
            E.ke2cum := E.ke2cum + E.ke * E.ke;
            P.vcum := P.vcum + P.now;
            P.vbar := P.vcum / ncalc;
         end;
   end;  {TimeAverages}

   procedure MCAverages;
   begin
      E.pecum := E.pecum + E.pe;
      E.pe2cum := E.pe2cum + E.pe * E.pe;
      P.now := (N * T.temperature) + 0.5 * P.virial;
      P.now := P.now / L.area;     {pressure at time t}
      P.vcum := P.vcum + P.now;
      P.vbar := P.vcum / nmcs;
   end;  {MCAverages}

   procedure pbc (var xtemp, ytemp: real);
   begin
      if xtemp < 0.0 then
         begin
            xtemp := xtemp + L.x;
         end;
      if xtemp > L.x then
         begin
            xtemp := xtemp - L.x;
         end;
      if ytemp < 0.0 then
         begin
            ytemp := ytemp + L.y;
         end;
      if ytemp > L.y then
         begin
            ytemp := ytemp - L.y;
         end;
   end;     {pbc}

   procedure cell (var xtemp, ytemp: real);
   begin
      if xtemp < 0.0 then
         xtemp := xtemp + L.x;
      if xtemp > L.x then
         xtemp := xtemp - L.x;
      if ytemp < 0.0 then
         ytemp := ytemp + L.y;
      if ytemp > L.y then
         ytemp := ytemp - L.y;
   end;     {cell}

   procedure acceleration;
      var
         i, j: integer;
         dx, dy, r2, force_over_r: real;
   begin
      for i := 1 to N do
         begin
            a.x[i] := 0.0;
            a.y[i] := 0.0
         end;
      P.virial := 0;
      for i := 1 to (N - 1) do
         begin
            for j := (i + 1) to N do
               begin
                  dx := r.x[i] - r.x[j];
                  dy := r.y[i] - r.y[j];
                  separation(dx, dy);
                  r2 := dx * dx + dy * dy;
                  f(r2, force_over_r);
                  a.x[i] := a.x[i] + force_over_r * dx;
                  a.y[i] := a.y[i] + force_over_r * dy;
                  a.x[j] := a.x[j] - force_over_r * dx;
                  a.y[j] := a.y[j] - force_over_r * dy;
                  P.virial := P.virial + dx * a.x[i] + dy * a.y[i];
               end;
         end;
   end;   {acceleration}

   procedure R2sum;
      var
         i: integer;
         dx, dy: real;
   begin
      R2 := 0;
      for i := 1 to N do
         begin
            dx := r.x[i] - r.x0[i];
            dy := r.y[i] - r.y0[i];
            separation(dx, dy);
            R2 := R2 + dx * dx + dy * dy;   {squared displacement}
         end;
   end;   {R2sum}

   procedure NewMCTemperature;
      var
         ChangeScreen: TinputScreen;
         C: HelpScrType;
         i: integer;
   begin
      for i := 1 to 25 do
         C[i] := '';
      C[2] := ' The temperature of the heat bath can be ';
      C[3] := ' changed by either using the slider or ';
      C[3] := ' the Settings menu.';
      C[24] := '      Press any key or click mouse to continue. ';
      with ChangeScreen do
         begin
            Init;
            DefineInputPort(0.00, 0.50, 0.63, 0.90);
            loadline('');
            loadline('desired temperature = {    } 0<T<100');
            setnumber(1, T.temperature);
            setnumberlimits(1, 0, 100);
            loadline('');
            loadline('[  Ok  ]   [Cancel]   [ Help ]');
            SetHelpScreen(C);
            AcceptScreen;
            if not canceled then
               begin
                  T.temperature := getnumber(1);
                  if (T.temperature > 0) then
                   T.beta := 1 / T.temperature; {adddd}
                  T.desire := T.temperature;
                  Slider.Reset(1, T.temperature);
                  Slider.DrawAll;
               end;
            done;
         end;  {with}
   end;    {NewMCTemperature}

   procedure CheckWindows2 (ntemp: integer);
      var
         ixdum, iydum, button: integer;
         xdum, ydum: real;
         inside4, inside5, inside6, inside7, inside9: boolean;
   begin
      inside4 := false;    {g(r)}
      inside5 := false;    {pressure}
      inside6 := false;    {R2}
      inside7 := false;    {velocity/speed distribution}
      inside9 := false;    {temperature window}
      if Event.MouseClicked then
         begin
            MousePosn(xdum, ydum, 9, 9, inside9);    {temperature window}
            Selectviewport(9);
            MousePosn(xdum, ydum, 6, 6, inside6);    {R2 window}
            Selectviewport(6);
            MousePosn(xdum, ydum, 7, 7, inside7);    {velocity/speed distribution window}
            Selectviewport(7);
            MousePosn(xdum, ydum, 4, 4, inside4);     {g(r) window}
            selectviewport(4);
            if flag.run = MDLJ then
               begin
                  MousePosn(xdum, ydum, 5, 5, inside5);     {Pressure window}
                  selectviewport(5);
               end;
            if inside9 or inside6 and (flag.run = MDLJ) then
               begin
                  if flag.T = false then
                     begin
                        flag.T := true;
                        flag.D := false;
                        ncalc := -ncalc;
                        ShowTemperature;
                     end
                  else
                     begin  {turn off temperature}
                        flag.T := false;
                        flag.D := true;  {turn on diffusion}
                         ShowR2(0);
                     end;
               end;
         end;
      if inside4 or inside5 and (flag.run = MDLJ) then
         begin
            if flag.g = false then
               begin
                  flag.g := true;
                  Setupg_of_rDisplay;
               end
            else   {flag.g = true}
               begin
                  flag.g := false;
                  ncalc := -ncalc;
                  ShowPressure;
               end;
         end;
      if inside7 and ((flag.run = MDLJ) or (flag.run = MDHD)) then
         begin
            flag.v := not flag.v;
            SetupDistributionDisplay;
            if ntemp > 0 then
               begin
                  if flag.v then
                     ShowSpeedDistribution(ntemp)
                  else
                     ShowVelocityDistribution(ntemp);
               end;
         end;
   end;   {CheckWindows2}

   procedure CheckWindows (ntemp: integer);
      var
         ixdum, iydum, button: integer;
         xdum, ydum: real;
         inside4, inside6, inside7, inside9: boolean;
   begin
      inside4 := false;
      inside6 := false;
      inside7 := false;
      inside9 := false;
      if Event.MouseClicked then
         begin
            MousePosn(xdum, ydum, 9, 9, inside9);    {temperature window}
            Selectviewport(9);
            MousePosn(xdum, ydum, 6, 6, inside6);    {R2 window}
            Selectviewport(6);
            MousePosn(xdum, ydum, 7, 7, inside7);    {velocity/speed distribution window}
            Selectviewport(7);
            if (flag.run <> MDLJ) then
               begin
                  MousePosn(xdum, ydum, 4, 4, inside4);     {g(r) window}
                  selectviewport(4);
               end;
            if inside9 and (flag.run = MDLJ) then
               begin
                  if flag.T = false then
                     begin
                        flag.T := true;
                        if ntemp > 0 then
                           begin
                              ncalc := -ncalc;
                              ShowTemperature;
                           end
                     end
                  else   {flag.T = true}
                     begin
                        flag.T := false;
                        DefineViewPort(9, 0.0, 0.51, 0.69, 0.955);
                        OpenViewPort(9);
                        setcolor(black);
                        ClearViewPort;
                     end;  {else}
               end;
            if inside6 then
               begin
                  if flag.D = false then
                     begin
                        flag.D := true;
                        ShowR2(0);
                     end
                  else   {flag.T = true}
                     begin
                        flag.D := false;
                        DefineViewPort(6, 0.52, 1.0, 0.65, 0.92);
                        CloseViewPort(6);
                     end; {else}
               end;   {inside}
         end;
      if inside4 and (flag.run <> MDLJ) then
         begin
            if flag.g = false then
               begin
                  flag.g := true;
                  Setupg_of_rDisplay;
                  if (flag.run <> MDHD) and (ntemp <> 0) then
                     Showg_of_r(nmcs);  {g(r) not updated after every collision for hard disks}
               end
            else   {flag.g = true}
               begin
                  flag.g := false;
                  DefineViewPort(4, 0.0, 0.51, 0.51, 0.87);
                  CloseViewPort(4);
               end;
         end;
      if inside7 and ((flag.run = MDLJ) or (flag.run = MDHD)) then
         begin
            flag.v := not flag.v;
            SetupDistributionDisplay;
         end;
   end;   {CheckWindows}

   procedure ChangeTimeStep;
      var
         ChangeScreen: TinputScreen;
         C: HelpScrType;
         i: integer;
   begin
      for i := 1 to 25 do
         C[i] := '';
      C[2] := ' The time step has no physical significance. ';
      C[3] := ' Choose the time step so that the total energy';
      C[4] := ' is conserved to the desired accuracy.';
      C[6] := ' The optimum choice of the time step depends on';
      C[7] := ' the temperature, density, the nature';
      C[8] := ' of the potential, and the numerical algorithm.';
      c[9] := ' If the time step is too big, the numerical';
      c[10]:= ' algorithm will become unstable.';
      C[24] := '      Press any key or click mouse to continue. ';
      with ChangeScreen do
         begin
            Init;
            DefineInputPort(0.00, 0.46, 0.70, 0.90);
            loadline('time step = {       } (about 0.01)');
            setnumber(1, step.dt);
            setnumberlimits(1, -0.02, 0.02);
            loadline('');
            loadline('[  Ok  ]   [Cancel]   [ Help ]');
            SetHelpScreen(C);
            repeat
               AcceptScreen;
               step.dt := getnumber(1);
               if (abs(step.dt)<=1e-8) then
               announce('Time step cannot be zero');
            until (abs(step.dt)>1e-8);
            start := true;
            if not canceled then
               begin
                  step.dt2 := step.dt * step.dt;
                  if step.dt<>0 then
                     plot.nmax := trunc(plot.MDLJtmax / abs(step.dt)) + 1;
                  start := false;
                  flag.change := true;
               end;
            done;
         end;  {with}
   end;   {ChangeTimeStep}

   procedure ChangeMaximumStepLength;
      var
         ChangeScreen: TinputScreen;
         C: HelpScrType;
         i: integer;
   begin
      for i := 1 to 25 do
         C[i] := '';
      C[1] := 'The maximum step length has no physical meaning.';
      C[2] := 'There is no rule for the optimum choice of';
      C[3] := 'the step length.';
      C[5] := 'If the step length is too small, the acceptance';
      C[6] := 'ratio will be near unity, and the sampling of';
      C[7] := 'statistically independent configurations will';
      C[8] := 'be inefficient.';
      C[10] := 'If the step length is too large, the acceptance';
      C[11] := 'ratio will be near zero, and the sampling of the';
      C[12] := 'configurations will be inefficient.';
      C[14] := 'A rule of thumb is to keep the acceptance';
      C[15] := 'ratio near 50%.';
      C[17] := 'A more sophisticated criterion is to maximize the';
      C[18] := 'self-diffusion coefficient.';
      C[20] := 'The program default sets the maximum step length';
      C[21] := 'to be 20% of the mean distance between particles.';
      C[24] := '      Press any key or click mouse to continue. ';
      with ChangeScreen do
         begin
            Init;
            DefineInputPort(0.00, 0.42, 0.35, 0.65);
            loadline('Suggest step length 20% of mean');
            loadline('distance between particles.');
            loadline('maximum step length = {      }');
            setnumber(1, dmax);
            setnumberlimits(1, 0.001, dmin(L.x, L.y));
            loadline('');
            loadline('[  Ok  ]   [Cancel]   [ Help ]');
            SetHelpScreen(C);
            AcceptScreen;
            start := true;
            if not canceled then
               dmax := getnumber(1);
            done;
         end;  {with}
   end;   {ChangeMaximumStepLength}

   procedure Rescalevelocities;
      var
         i: integer;
   begin
      for i := 1 to N do
         begin
            v.x[i] := T.lambda * v.x[i];
            v.x[i] := T.lambda * v.x[i];
         end;
      E.ke := T.lambda * T.lambda * E.ke;   {rescaled kinetic energy}
      T.now := E.ke / N;   {temperature}
      if T.lambda <= 1 then
         begin
            if T.now <= T.desire then
               T.rescale := false
         end
      else
         begin
            if T.now >= T.desire then
               T.rescale := false;
         end;
      if not T.rescale then
         begin
            Initializeg_r;
            InitializeVirial;
            if flag.D then
               SetupDiffusionDisplay;
            if flag.run = MDLJ then
               begin
                  InitializeMDvariables;
                  InitializeProbabilityDistribution;
                  InitializeTemperature;
                  if flag.T then
                     ShowTemperature;
                  if flag.g then
                     Setupg_of_rDisplay
                  else
                     ShowPressure;
                  SetupDistributionDisplay;
               end
            else if flag.run = MDHD then
               begin
                  InitializeMDvariables;
                  InitializeProbabilityDistribution;
                  Setupg_of_rDisplay;
                  SetupDistributionDisplay;
               end;
         end;  {not T.rescale}
   end;     {RescaleVelocities}

   procedure SetLambda (temp: real);
      var
         ChangeScreen: TinputScreen;
         C: HelpscrType;
         i: integer;
   begin
      for i := 1 to 25 do
         C[i] := '';
      C[2] := 'The magnitude of the velocity rescaling parameter';
      C[3] := 'should be close to unity so that the system is as';
      C[4] := 'to equilibrium as possible.';
      C[24] := '      Press any key or click mouse to continue. ';
      with ChangeScreen do
         begin
            Init;
            DefineInputPort(0.00, 0.48, 0.70, 0.90);
            loadline('velocity rescaling factor = {     }');
            if T.desire <= temp then
               setnumber(1, T.scaledown)
            else
               setnumber(1, T.scaleup);
            setnumberLimits(1, 0.5, 2);
            loadline('');
            loadline('[  Ok  ]   [Cancel]   [ Help ]');
            SetHelpScreen(C);
            AcceptScreen;
            if not canceled then
               begin
                  T.lambda := getnumber(1);
                  T.rescale := true;
               end;
            done;
         end;   {with}
   end;   {SetLambda}

   procedure Verlet;
      var
         xnew, ynew, dx, dy: real;
         i, ibin: integer;
         speed, v2, rbin : real;
   begin
      for i := 1 to N do
         begin
            dx := v.x[i] * step.dt + 0.5 * a.x[i] * step.dt2;
            dy := v.y[i] * step.dt + 0.5 * a.y[i] * step.dt2;
            if (dx*dx+dy*dy>0.1) then begin
               warning := true;
               step.dt := 0;
               step.dt2:= sqr(step.dt);
               GaussianDistribution(1);
            end;
            xnew := r.x[i] + dx;
            ynew := r.y[i] + dy;
            cell(xnew, ynew);  {periodic boundary condition}
       {partially update velocity using old acceleration}
            v.x[i] := v.x[i] + 0.5 * a.x[i] * step.dt;
            v.y[i] := v.y[i] + 0.5 * a.y[i] * step.dt;
            r.x[i] := xnew;
            r.y[i] := ynew;
         end;
      acceleration;     {compute new acceleration}
      E.ke := 0;   {compute kinetic energy at each time step since little overhead}
      for i := 1 to N do
         begin
   {complete update of velocity using new acceleration}
            v.x[i] := v.x[i] + 0.5 * a.x[i] * step.dt;
            v.y[i] := v.y[i] + 0.5 * a.y[i] * step.dt;
            v2 := v.x[i] * v.x[i] + v.y[i] * v.y[i];
            E.ke := E.ke + v2;
            speed := sqrt(v2);
            rbin := speed / prob.delta;
            if rbin > prob.bin then ibin := prob.bin
            else ibin := trunc(rbin);
            prob.s[ibin] := prob.s[ibin] + 1;  {speed distribution}
            rbin := v.x[i] / prob.delta;
            if rbin > prob.bin then ibin := prob.bin else
            if rbin <-prob.bin then ibin :=-prob.bin
            else ibin := round(rbin);
            prob.v[ibin] := prob.v[ibin] + 1;   {velocity distribution}
            rbin := v.y[i] / prob.delta;
            if rbin > prob.bin then ibin := prob.bin else
            if rbin <-prob.bin then ibin :=-prob.bin
            else ibin := round(rbin);
            prob.v[ibin] := prob.v[ibin] + 1;
         end;    {sum over all N particles}
      E.ke := 0.5 * E.ke;
   end;   {Verlet}

   procedure InitialMDLJ;
   begin
      ClearMuppetPort;
      menu.display;{added}
      Comment(0, 0.95, 'molecular dynamics: Lennard-Jones');
      Menu.RowActivate(4, 1, true);
      Menu.RowActivate(4, 2, true);
      Menu.Row(4, 2, 'time step');
      ClearMCcheck;
      flag.show := true;
      flag.T := true;
      SetupTrajectoryDisplay;
      ShowInitialPositions;
      Initializeg_r;
      InitializeMDvariables;
      InitializeTemperature;
      InitializeVirial;
      InitializeProbabilityDistribution;
      SetupHotKeys;
      SetupDataDisplay; {open window for data display}
      SetupDistributionDisplay;
      KineticEnergy;
      PotentialEnergy;
      TotalEnergy;
      acceleration;          {compute initial acceleration and pressure}
      ShowInitialData;
      ShowTemperature;
      if not flag.g then
         ShowPressure
      else
         Setupg_of_rDisplay;
   end;     {InitialMDLJ}

   procedure RunMDLJ;
   begin
      if flag.change or not start then
         repeat
            flag.change := false;
            CheckForEvents;
            if Event.MouseClicked then
               begin
                  handlemouse;
                  CheckWindows2(ncalc);
               end;
            if hotkeys.pressed(knum) then
               HandleHotKeys;
            if Menu.activated then
               begin
                  flag.exit := true;
                  handlemenu;
               end;
         until (Event.KeyPressed or Event.MouseClicked or flag.exit);
      if start then
         begin
            flag.exit := false;
            Menu.RowActivate(1, 8, true);   {save}
            repeat
               Verlet;    {one time step}
               if T.rescale then
                  RescaleVelocities;
               time := time + step.dt;
               ncalc := ncalc + 1;
               ndiff := ndiff + 1;
               PotentialEnergy;
               TotalEnergy;
               TimeAverages;
               R2sum;
               CorrelationFunction;
               CheckForEvents;
               CheckWindows2(ncalc);
               if flag.T then
                  ShowTemperature  {plot instantaneous value of T and running average}
               else
                  ShowR2(ndiff);
               if not T.rescale then
                  begin
                     if (ncalc mod nbar = 0) then
                        begin
                           CheckTotalMomentum;
                           if flag.v then
                              ShowSpeedDistribution(ncalc)
                           else
                              ShowVelocityDistribution(ncalc);
                        end;
                  end;
               if flag.g then
                  begin
                     if (ncalc mod nbar = 0) then Showg_of_r(ncalc);
                  end
               else ShowPressure;
               ShowData;
               if flag.show then
                  ShowTrajectories;  {draw positions of particles}
            until (Event.KeyPressed or Event.MouseClicked or warning);
            if warning then begin
               announce('Time step too large, set to 0 to prevent overflow error.');
               announce('Reset time step and try again.');
               warning := false;
            end;
         end;  {start}
   end;     {RunMDLJ}

   procedure showmove (xold, yold, xtrial, ytrial: real);
      var
         xsc, ysc, BkColor: integer;
   begin
      SelectViewPort(2);
      SelectScale(2);
      HideMouse;
      if flag.disk then
         begin
            BkColor := GraphBackColor;
            SetColor(BkColor);
            SetFillStyle(SolidFill, BkColor);
            Map(xold, yold, xsc, ysc);
            FillEllipse(xsc, ysc, plot.hd, plot.hd);
            SetColor(lightgreen);
            SetFillStyle(SolidFill, lightgreen);
            Map(xtrial, ytrial, xsc, ysc);
            FillEllipse(xsc, ysc, plot.hd, plot.hd);
         end
      else
         begin
            SetColor(lightgreen);
            SetFillStyle(SolidFill, lightgreen);
            Map(xtrial, ytrial, xsc, ysc);
            Circle(xsc, ysc, plot.rsc);
         end;
         ShowMouse;
   end;   {showmove}

   procedure Checkoverlap (itrial: integer; xtrial, ytrial: real);
      var
         i: integer;
         r2, dx, dy: real;
         xold, yold: real;
         accept: boolean;
   begin
      i := 1;
      accept := true;
      repeat   {check overlap with all other particles}
         if i <> itrial then
            begin
               dx := r.x[i] - xtrial;
               dy := r.y[i] - ytrial;
               separation(dx, dy);
               r2 := dx * dx + dy * dy;
               if r2 < 1 then
                  accept := false;
            end;   {if}
         i := i + 1;   {consider next particle}
      until (i > N) or not accept;
      if accept then
         begin
            naccept := naccept + 1;
            xold := r.x[itrial];
            yold := r.y[itrial];
            r.x[itrial] := xtrial;
            r.y[itrial] := ytrial;
            if flag.show then
               showmove(xold, yold, xtrial, ytrial);
         end;
   end;   {overlap}

   procedure move;
      var
         i, itrial: integer;
         xtrial, ytrial: real;
   begin
      for i := 1 to N do
         begin
            itrial := trunc(N * ran(seed)) + 1;
            xtrial := r.x[itrial] + (2 * ran(seed) - 1) * dmax;
            ytrial := r.y[itrial] + (2 * ran(seed) - 1) * dmax;
            cell(xtrial, ytrial);
            Checkoverlap(itrial, xtrial, ytrial);
         end;  {loop}
      SetColor(white);
      HideMouse;
      if (viewPortNumber=2) and flag.show then with views[2] do
      rectangle(0,0,vx2-vx1,vy2-vy1);
      ShowMouse;
   end;   {move}

   procedure InitialMCHD;
   begin
      Slider.erase(1, black);
      ClearMuppetPort;
      menu.display;{added}
      Comment(0, 0.95, 'Monte Carlo: Hard Disks');
      Menu.Row(4, 2, 'step length');
      Menu.RowActivate(4, 1, False);
      Menu.RowActivate(4, 2, true);
      ClearMDcheck;
      flag.show := true;
      flag.g := true;
      flag.D := true;
      CheckConfiguration;
      if overlap then
         begin
            overlapmessage(nmcs);
            Newparameters;
         end;
      Initializeg_r;
      InitializeMCVariables;
      SetUpShortHotKeys;
      Setupg_of_rDisplay;
      SetupDiffusionDisplay;
      SetupHDTrajectoryDisplay;
      if flag.disk then
         begin
            ShowInitialDisks;
            SetUpShortHotKeys;
         end
      else
         begin
            ShowInitialPositions;
            SetUpHotKeys
         end;
      SetupHDDataDisplay; {open window for data display}
   end;    {InitialMCHD}

   procedure RunMCHD;
   begin
      if flag.change or not start then
         repeat
            flag.change := false;
            CheckForEvents;
            if Event.MouseClicked then
               begin
                  handlemouse;
                  CheckWindows(nmcs);
               end;
            if hotkeys.pressed(knum) then
               HandleHotKeys;
            if Menu.activated then
               begin
                  flag.exit := true;
                  handlemenu;
               end;
         until (Event.KeyPressed or Event.MouseClicked or flag.exit);
      if start then
         begin
            flag.exit := false;
            Menu.RowActivate(1, 8, true);
            repeat
               move;
               nmcs := nmcs + 1;
               ndiff := ndiff + 1;
               CorrelationFunction;
               R2sum;
               ShowHDdata;
               CheckForEvents;
               CheckWindows(nmcs);
               if flag.D then
                  ShowR2(ndiff);
               if flag.g then
                  if (nmcs mod nbar = 0) then
                     Showg_of_r(nmcs);
            until (Event.KeyPressed or Event.MouseClicked);
         end;
   end;     {RunMCHD}

   procedure test (itrial: integer; xtrial, ytrial: real);
      var
         i: integer;
         xold, yold, petrial, peold, potential, de, r2, dx, dy: real;
         tprob, rn: real;
         accept: boolean;
   begin
      petrial := 0; {trial potential energy of particle itrial}
      peold := 0;   {original potential energy of particle itrial}
      accept := true;
      for i := 1 to N do
         begin
            if i <> itrial then
               begin
                  dx := r.x[i] - xtrial;
                  dy := r.y[i] - ytrial;
                  separation(dx, dy);
                  r2 := dx * dx + dy * dy;
                  U(r2, potential);
                  petrial := petrial + potential;
                  dx := r.x[i] - r.x[itrial];
                  dy := r.y[i] - r.y[itrial];
                  separation(dx, dy);
                  r2 := dx * dx + dy * dy;
                  U(r2, potential);
                  peold := peold + potential;
               end;   {if}
         end;
      de := petrial - peold;
      if (de > 0) then
      begin
         if (T.temperature > 0) then
         begin
            if exp(-T.beta * de) < ran(seed) then
             accept := false;
         end else accept := false;
      end;
      if accept then
         begin
            naccept := naccept + 1;
            xold := r.x[itrial];
            yold := r.y[itrial];
            r.x[itrial] := xtrial;
            r.y[itrial] := ytrial;
            E.pe := E.pe + de;
            if flag.show then
               showmove(xold, yold, xtrial, ytrial);
         end;
   end;   {test}

   procedure Metropolis;
      var
         i, itrial: integer;
         xtrial, ytrial: real;
   begin
      for i := 1 to N do
         begin
            itrial := trunc(N * ran(seed)) + 1;
            xtrial := r.x[itrial] + (2 * ran(seed) - 1) * dmax;
            ytrial := r.y[itrial] + (2 * ran(seed) - 1) * dmax;
            cell(xtrial, ytrial);
            test(itrial, xtrial, ytrial);
         end;  {loop}
      HideMouse;                 {redraw box}
      SetColor(white);
      if flag.show then with views[2] do
      rectangle(0,0,vx2-vx1,vy2-vy1);
      ShowMouse;
   end;   {Metropolis}

   procedure CheckSlider;
      var
         Tnew: real;
   begin
      flag.exit := true;
      Tnew := Slider.Value(1);
      T.temperature := Tnew;
      Slider.Reset(1, T.temperature);
      if T.temperature>0 then
        T.beta := 1 / T.temperature;
      T.desire := T.temperature;
   end;  {CheckSlider}

   procedure InitialMCLJ;
   begin
      ClearMuppetPort;
      menu.display;{added}
      Comment(0, 0.95, 'Monte Carlo: Lennard-Jones');
      Menu.Row(4, 2, 'step length');
      menu.rowactivate(4,1,true);
      menu.rowactivate(4,2,true);
      ClearMDcheck;
      flag.show := true;
      flag.g := true;
      flag.D := true;
      Initializeg_r;
      InitializeMCVariables;
      InitializeVirial;
      Slider.Reset(1, T.temperature);
      Slider.DrawAll;
      Setupg_of_rDisplay;
      SetupDiffusionDisplay;
      SetupHDTrajectoryDisplay;
      if flag.disk then
         begin
            ShowInitialDisks;
            SetUpShortHotKeys;
         end
      else
         begin
            ShowInitialPositions;
            SetUpHotKeys
         end;
      PotentialEnergy;
      CloseViewPort(7);
      ShowPotentialEnergy;
      SetupDataDisplay; {open window for data display}
      acceleration;  {compute virial}
      ShowMCData;
   end;   {InitialMCLJ}

   procedure RunMCLJ;
   begin
      if flag.change or not start then
         repeat
            flag.change := false;
            CheckForEvents;
            if Event.MouseClicked then
               begin
                  handlemouse;
                  CheckWindows(nmcs);
               end;
            if hotkeys.pressed(knum) then
               HandleHotKeys;
            if Slider.Changed then
               CheckSlider;
            if Menu.activated then
               begin
                  flag.exit := true;
                  handlemenu;
               end;
         until (Event.KeyPressed or Event.MouseClicked or flag.exit);
      if start then
         begin
            Menu.RowActivate(1, 8, true);
            repeat
               flag.exit := false;
               Metropolis;
               nmcs := nmcs + 1;
               ndiff := ndiff + 1;
               MCAverages;
               CorrelationFunction;
               R2sum;
               acceleration;  {compute virial}
               ShowPotentialEnergy;
               ShowMCData;
               CheckForEvents;
               CheckWindows(nmcs);
               if flag.D then
                  ShowR2(ndiff);
               if Slider.Changed then
                  CheckSlider;
               if flag.g then
                  if (nmcs mod nbar = 0) then
                     Showg_of_r(nmcs);
            until (Event.KeyPressed or Event.MouseClicked) and not flag.exit;
         end;
   end;     {RunMCLJ}

   procedure check_collision(icol,jcol: integer);
   var
      dx,dy,dvx,dvy,bij: real;
      r2,v2,discr: real;
      xcell,ycell: integer;
    begin
    for xcell := -1 to 1 do
        begin
        for ycell := -1 to 1 do
            begin
               dx := r.x[icol] - r.x[jcol] + xcell*L.x;
               dy := r.y[icol] - r.y[jcol] + ycell*L.y;
               dvx := v.x[icol] - v.x[jcol];
               dvy := v.y[icol] - v.y[jcol];
               bij := dx * dvx + dy * dvy;    {impact parameter}
               if bij < 0.0 then
                  begin
                     r2 := dx * dx + dy * dy;
                     v2 := dvx * dvx + dvy * dvy;
                     discr := bij * bij - v2 * (r2 - 1.0);  {1.0 is sigma^2}
                     if (discr > 0.0) then
                        begin
                           tij := -(bij + sqrt(discr)) / v2;
                           if tij < CollisionTime[icol] then
                              begin
                                 CollisionTime[icol] := tij;
                                 partner[icol] := jcol;
                              end;  {tij < CollisionTime[icol]}
                           end;  {discr > 0.0}
               end;   {bij < 0.0}
            end; {ycell}
        end; {xcell}
    end;    {check_collisin}

   procedure downlist (icol: integer);
      var
         k: integer;
   begin {look for collisions with particles i > j}
      for k := 1 to icol - 1 do
          check_collision(k,icol);

   end;   {downlist}

   procedure uplist (icol: integer);
      var
         k: integer;
   begin {look for collisions with particles k > icol}
      CollisionTime[icol] := TimeBig;
      for k := icol + 1 to N do
          check_collision(icol,k);
   end;   {uplist}

   procedure RescaleHDvelocities;
      var
         i, Bkcolor: integer;
         lambda: real;
   begin
      lambda := T.desire / (E.ke / N);
      lambda := sqrt(lambda);
      for i := 1 to N do
         begin
            v.x[i] := lambda * v.x[i];
            v.y[i] := lambda * v.y[i];
         end;
      for i := 1 to N - 1 do
         uplist(i);
      E.ke := lambda * lambda * E.ke;
      SelectViewPort(8);
      BkColor := GraphBackColor;
      rubout(24, 1, 6, BkColor);
      print(20, 1, concat('T =', NumStr(E.ke / N, 5, 2)));
   end;  {RescaleHDVelocities}

   procedure NewMeanTemperature;
      var
         ChangeScreen: TinputScreen;
         C: HelpScrType;
         i: integer;
         temp, temp2: real;
   begin
      for i := 1 to 25 do
         C[i] := '';
      temp := E.ke / N;
      if flag.run = MDLJ then
         begin
            C[1] := 'The kinetic energy per particle at the time the';
            C[2] := 'simulation was stopped is shown.';
            C[4] := 'If a different temperature is desired, the';
            C[5] := 'particle velocities will be rescaled at each';
            C[6] := 'time step until the desired kinetic energy is';
            C[7] := 'reached.';
            C[9] := 'It is important that the temperature not be raised';
            C[10] := 'too quickly and that the system remain close to';
            C[11] := 'equilibrium.';
            C[13] := 'The velocity rescaling factor is chosen in the';
            C[14] := 'next input screen.';
            temp2 := T.bar;
         end
      else
         begin
            C[1] := 'The temperature is meaningless for hard disks';
            C[2] := 'because the only effect is to rescale';
            C[3] := 'the time between collisions.';
            temp2 := temp;
         end;
      C[24] := '      Press any key or click mouse to continue. ';
      with ChangeScreen do
         begin
            Init;
            DefineInputPort(0.00, 0.62, 0.70, 0.90);
            loadline(concat('mean temperature    = ', NumStr(temp2, 3, 3)));
            loadline('desired temperature = {     } (0.01-'+NumStr(temp*2,3,2)+')');
            setnumber(1, temp);
            setnumberlimits(1, 0.01, 2 * temp);
            loadline('');
            loadline('[  Ok  ]   [Cancel]   [ Help ]');
            SetHelpScreen(C);
            AcceptScreen;
            if not canceled then
               begin
                  T.desire := getnumber(1);
                  if flag.run = MDLJ then
                     begin
                        setlambda(temp);   {call procedure to set velocity rescaling factor}
                        DefineViewPort(7, 0.53, 1.0, 0.22, 0.56);
                        CloseViewport(7);
                        Comment(0.6, 0.4, 'Rescaling velocities ...');
                     end
                  else
                     RescaleHDvelocities;
               end;
            done;
         end;  {with}
   end;    {NewMeanTemperature}

   procedure SetupMDHDDataDisplay;
   begin
      OpenViewPort(8);
      setcolor(lightgreen);
      print(2, 1,  concat('collisions =', NumStr(ncalc, 2, 0)));
      print(2, 2,  concat('density    =', NumStr(L.density, 5, 2)));
      print(21, 1, concat(' T  =', NumStr(E.ke / N, 5, 2)));
      SetColor(yellow);
      print(21, 2,        ' P  ='+NumStr(P.now / N, 5, 2));
   end;   {SetupMDHDDataDisplay}

   procedure InitializeMDHDVariables;
      var
         i: integer;
   begin
      for i := 1 to N do     {initialize collision times}
         begin
            CollisionTime[i] := 1000000;
            partner[i] := N;
         end;
      time := 0.0;
      ncalc := 0;
      naccept := 0;   {use to determine mean free time}
      too_far := false;
   end;   {InitializeMDHDVariables}

   procedure InitialMDHD;
      var
         i: integer;
   begin
      ClearMuppetPort;
      menu.display;{added}
      Comment(0, 0.95, 'Molecular Dynamics: Hard Disks');
      {Comment(0, 0.91 , 'Click on Viewports to Change Plot Style')};
      Menu.Row(4, 2, 'time step');
      Menu.RowActivate(4, 1, True);
      Menu.RowActivate(4, 2, False);
      ClearMCcheck;
      flag.show := true;
      flag.D := true;
      flag.g := true;
      warning := false;
      CheckConfiguration;
      if overlap then
         begin
            overlapmessage(ncalc);
            newparameters;
         end;
      KineticEnergy;
      Initializeg_r;
      SetUpHotKeys;
      InitializeMDHDVariables;
      Setupg_of_rDisplay;
      SetupDiffusionDisplay;
      SetupHDTrajectoryDisplay;
      ShowInitialPositions;
      SetupMDHDDataDisplay;
      SetupDistributionDisplay;
      InitializeProbabilityDistribution;
      InitializeVirial;
      for i := 1 to N - 1 do
         uplist(i);   {look for collisions with particles j > i}
   end;   {InitialMDHD}

   procedure RunMDHD;
      var
         i, j: integer;    {collision partners}

    procedure ReInitializeCollisionTimes;
       var
        i: integer;
      begin
       for i := 1 to N do     {initialize collision times}
        begin
         CollisionTime[i] := TimeBig;
         partner[i] := N;
         distx[i] := 0;
         disty[i] := 0;
        end;
      end;   {ReInitializeCollisionTimes}

      procedure TrackDistances;   {nadddd}
         var
          k: integer;
          dx, dy, vmax: real;
          xflag: boolean;
          tijm: real;
        begin
         too_far := false;
         k := 1;
         while (not too_far) and (k <= N) do
          begin
           dx := v.x[k] * tij;
           dy := v.y[k] * tij;
           distx[k] := dx + distx[k];
           if abs(distx[k]) > 0.5 * L.x then
            too_far := true;
           disty[k] := dy + disty[k];
           if abs(disty[k]) > 0.5 * L.y then
            too_far := true;
           k := k + 1;
          end;
         if (not too_far) then
          begin  {particles i and j really did collide}
           distx[i] := 0;
           distx[j] := 0;
           disty[i] := 0;
           disty[j] := 0;
          end
         else
          begin
           vmax := 0;
           for k := 1 to N do
            begin   {find maximum velocity component}
             if vmax < abs(v.x[k]) then
             begin
             vmax := abs(v.x[k]);
             xflag := true;
             end;
             if vmax < abs(v.y[k]) then
             begin
             vmax := abs(v.y[k]);
             xflag := false;
             end;
            end;   {of for loop}
           if xflag then
            tijm := 0.05 * L.x / vmax
           else
            tijm := 0.05 * L.y / vmax;
           if tijm > tij then
            tij := 0.05 * tij
           else
            tij := tijm;
          end;
        end;   {TrackDistances}

      procedure ShowMDHDdata;
         var
            meanfreetime, wbar: real;
      begin
         SelectViewPort(8);
         SetColor(lightgreen);
         rubout(14, 1, 7, GraphBackColor);
         if ncalc < 1000 then print(14, 1, NumStr(ncalc, 4, 0))
         else print(14, 1, NumStr(ncalc, 5, 0));
         P.vcum := P.vcum + P.virial;
         wbar := P.vcum / (2 * time);    {variable time is time of observation}
         P.vbar := E.ke + wbar;
         P.vbar := P.vbar / L.area;
         setcolor(yellow);
         print(21, 2, '<P> =');
         rubout(27, 2, 8, GraphBackColor);
         print(27, 2, Num2Str(P.vbar, 5));
         print(2, 3, 'mean free time =');
         meanfreetime := time * N / (2 * ncalc);
         rubout(18, 3, 8, GraphBackColor);
         print(18, 3, NumStr(meanfreetime, 6, 2));
      end;       {ShowMDHDdata}


      procedure reset;       {find new partners}
         var
            k: integer;
      begin
         for k := 1 to N - 1 do
            begin
               if ((k = i) or (partner[k] = i) or (k = j) or (partner[k] = j)) then
                  uplist(k);
            end;
      end; {reset}

      procedure SmallestCollisionTime (var i, j: integer);
         var
            k: integer;
      begin    {find the earliest collision and the colliding pair}
         tij := TimeBig;
         i := 0;
         for k := 1 to N do
            begin
               if (CollisionTime[k] < tij) then
                  begin
                     tij := CollisionTime[k];
                     i := k;
                  end;   {CollisionTime[k] < tij}
            end;  {k loop}
         if (i=0) then j:=0 else j := partner[i];
      end;   {SmallestCollisionTime}

      procedure MoveForward;
         var
            k: integer;
            xnew, ynew: real;
            vx, vy: real;
            dx, dy, dvx, dvy: real;
            bij, deltavx, deltavy: real;
      begin
         for k := 1 to N do
            begin  {move particles for time tij and adjust future collision times}
               CollisionTime[k] := CollisionTime[k] - tij;
               xnew := r.x[k] + v.x[k] * tij;
               ynew := r.y[k] + v.y[k] * tij;
               cell(xnew, ynew);  {apply periodic boundary conditions}
               r.x[k] := xnew;
               r.y[k] := ynew;
            end;
      end;   {MoveForward}

   PROCEDURE special;
      VAR
         k: integer;
         xnew, ynew: double;
   BEGIN
      tij := 1;
      FOR k := 1 TO N DO
      BEGIN  {move particles for time tij and adjust future collision times}
         CollisionTime[k] := CollisionTime[k] - tij;
         xnew := r.x[k] + v.x[k] * tij;
         ynew := r.y[k] + v.y[k] * tij;
         IF xnew < 0 THEN
            WHILE (xnew < 0) DO
               xnew := xnew + L.x
         ELSE
            WHILE (xnew > 0) DO
               xnew := xnew - L.x;
         r.x[k] := xnew;
         r.y[k] := ynew;
      END;
   END;   {special}

      procedure bump;
         var
            dx, dy, dvx, dvy: real;
            factor, deltavx, deltavy: real;
      begin
  {compute collision dynamics of colliding pair i,j}
         dx := r.x[i] - r.x[j];
         dy := r.y[i] - r.y[j];
         separation(dx, dy);
         dvx := v.x[i] - v.x[j];
         dvy := v.y[i] - v.y[j];
         factor := dx * dvx + dy * dvy;
         deltavx := -factor * dx;
         deltavy := -factor * dy;
         v.x[i] := v.x[i] + deltavx;
         v.y[i] := v.y[i] + deltavy;
         v.x[j] := v.x[j] - deltavx;
         v.y[j] := v.y[j] - deltavy;
         P.virial := deltavx * dx + deltavy * dy;
      end;  {bump}

      procedure ComputeSpeedDistribution;
         var
            i, ibin: integer;
            v2, speed: real;
      begin
         for i := 1 to N do
            begin
               v2 := v.x[i] * v.x[i] + v.y[i] * v.y[i];
               speed := sqrt(v2);
               ibin := Trunc(speed / prob.delta) + 1;
               if ibin > prob.bin then ibin := prob.bin;
               prob.s[ibin] := prob.s[ibin] + 1;  {speed distribution}
               ibin := round(v.x[i] / prob.delta) + 1;
               if ibin >  prob.bin then ibin :=  prob.bin else
               if ibin < -prob.bin then ibin := -prob.bin;
               prob.v[ibin] := prob.v[ibin] + 1;
               ibin := round(v.y[i] / prob.delta) + 1;
               if ibin >  prob.bin then ibin :=  prob.bin else
               if ibin < -prob.bin then ibin := -prob.bin;
               prob.v[ibin] := prob.v[ibin] + 1;
            end;  {sum over particles}
      end;   {ComputeSpeedDistribution}

      procedure MaximumCollisionTime (var tijmax: real);
         var
            vxmax, vymax: real;
            k: integer;
      begin
         vxmax := 0;
         vymax := 0;
         tijmax := 0;
         for k := 1 to N do
            begin
               tijmax := abs(v.x[k]);
               if tijmax > vxmax then
                  vxmax := tijmax;
               tijmax := abs(v.y[k]);
               if tijmax > vymax then
                  vymax := tijmax;
            end;   {loop}
         tijmax := 0.2 * dmin(L.x / vxmax, L.y / vymax);
      end;   {MaximumCollisionTime}

      procedure FindPartnersAgain;
       var
        i: integer;
      begin
       tij := TimeBig;
       for i := 1 to N - 1 do
        uplist(i);
      end;    {FindPartnersAgain}

      var
         ntemp: integer;
         tijmax: real;
         ev : boolean;
         too_dense, noCollisions : boolean;
   begin   {RunMDHD}
      if flag.change or not start then
         repeat
            flag.change := false;
            CheckForEvents;
            if Event.MouseClicked then
               begin
                  handlemouse;
                  CheckWindows(ntemp);
               end;
            if hotkeys.pressed(knum) then
               HandleHotKeys;
            if Menu.activated then
               begin
                  flag.exit := true;
                  handlemenu;
               end;
         until (Event.KeyPressed or Event.MouseClicked or flag.exit);
         if start then
         begin
             flag.exit := false;
             Menu.RowActivate(1, 8, true);   {save}
             repeat
              if too_far then
               FindPartnersAgain;
              SmallestCollisionTime(i, j);
              too_dense := (tij<0);
              noCollisions := (i=0);
              if L.density < 0.5 then
              begin
                TrackDistances;
                if too_far then
                ReInitializeCollisionTimes;
              end;
              if (tij > 0) AND (tij <> TimeBIg) then
              begin
                MoveForward;
                R2sum;
                time := time + tij;
                CheckConfiguration;
                if overlap then too_dense := true;
              end;
              IF (tij = TimeBig) THEN
              BEGIN
                 too_far := true;
                 special;
                 IF flag.show THEN
                    ShowTrajectories;
                 CheckForEvents;
              END;
              if not (too_far or noCollisions) then
              begin
                ncalc := ncalc + 1;    {number of collisions}
                ndiff := ndiff + 1;
                  {move particles forward by time tij and reduce collision times}
                if overlap then overlapmessage(ncalc);
                bump;
                reset;
                if (tij<>TimeBig) then
                begin
                   if j <> 1 then downlist(j);
                   if i <> 1 then downlist(i);
                end;
                if (ncalc mod 10 = 0) then
                begin
                   ComputeSpeedDistribution;
                   CorrelationFunction;
                end;
                if not T.rescale then
                if (ncalc mod nbar = 0) then
                begin
                   ntemp := ncalc div 10;
                   if flag.v then ShowSpeedDistribution(ntemp)
                   else ShowVelocityDistribution(ntemp);
                   if flag.g then Showg_of_r(ntemp);
                end;
                CheckForEvents;
                CheckWindows(ntemp);
                if flag.D then ShowR2(ndiff);
                if flag.show then ShowTrajectories;
                ShowMDHDdata;
               end;
              ev := Event.KeyPressed or Event.MouseClicked
           until (ev and not flag.exit) or too_dense or noCollisions;
           if (too_dense and (not warning)) then begin
              announce('System too dense. Choose new system.');
              warning := true;
           end;
           if noCollisions then begin
              Announce('No collisions - algorithm broke down. Change initial conditions.');
              start := false;
              CheckForEvents; {hack}
           end;
        end;
   end;     {RunMDHD}

   procedure setflags;
   begin
      flag.show := true;
      flag.g := false;
      flag.change := true;
      flag.disk := true;
      T.rescale := false;
      overlap := false;
      start := false;
      quit := false;
   end;   {setflags}

   procedure HandleMenu;
      var
         ntemp: longint;
   begin
      case Menu.Colchosen of
         1:
            begin
               case Menu.RowChosen of
                  1: GeneralInfo;
                  2: AboutCups;
                  3: AboutUnits;
                  4: Configuration;
                  6: Newparameters;
                  7: XOpenFile;
                  8: begin
                        if (flag.run = MDLJ) or (flag.run = MDHD) then
                           ntemp := ncalc
                        else
                           ntemp := nmcs;
                        SaveData(ntemp);
                     end;
                  10:
                     begin
                        quit := true;
                        start := false;
                     end;
               end;   {case Rowchosen}
            end;
         2:
            begin
               flag.change := true;
               case Menu.RowChosen of
                  1:
                     flag.run := MDLJ;
                  2:
                     flag.run := MDHD;
               end;
            end;
         3:
            begin
               flag.change := true;
               case Menu.RowChosen of
                  1:
                     flag.run := MCLJ;
                  2:
                     flag.run := MCHD;
               end;
            end;
         4:
            begin
               case Menu.RowChosen of
                  1:
                     if (flag.run = MDLJ) or (flag.run = MDHD) then
                        NewMeanTemperature
                     else if (flag.run = MCLJ) then
                        NewMCTemperature;
                  2:
                     begin
                        if (flag.run = MDLJ) then
                           ChangeTimeStep
                        else if (flag.run <> MDHD) then
                           ChangeMaximumStepLength;
                     end;
                  3:
                     ChangeBoxLength;
                  4:
                     DisplayOptions;
               end;
            end;
      end; {case}
   end;   {HandleMenu}

   procedure run_pause;
   begin
       ClearMkBuffers;
       start := not start;
   end;   {run_pause}

   procedure hide_show;
   begin
      if flag.show then
         begin
            flag.show := false;
            HideTrajectories
         end
      else
         begin
            flag.show := true;
            OpenViewPort(2);
            if (flag.run = MCHD) or (flag.run = MCLJ) then
               ShowInitialDisks
            else
               ShowInitialPositions;
         end;
   end;   {hide_show}

   procedure refresh;
   begin
      if flag.disk and ((flag.run = MCLJ) or (flag.run = MCHD)) then
         ShowInitialDisks
      else
         ShowInitialPositions;
   end;   {refresh}

   procedure reset_variables;
   begin
      Initializeg_r;
      SetupDiffusionDisplay;
      InitializeVirial;
      InitializeProbabilityDistribution;
      case flag.run of
         MDLJ:
            begin
               InitializeMDvariables;
               InitializeTemperature;
               if flag.T then
                  begin
                     ncalc := -ncalc;
                     ShowTemperature;
                  end;
               if not flag.g then
                  begin
                     ncalc := -ncalc;
                     ShowPressure
                  end
               else
                  Setupg_of_rDisplay;
               if not T.rescale then
                  SetupDistributionDisplay;
            end;
         MDHD:
            begin
               InitializeMDvariables;
               flag.g := true;
               flag.D := true;
               Setupg_of_rDisplay;
               SetupDistributionDisplay;
            end;
         MCLJ:
            begin
               InitializeMCVariables;
               Setupg_of_rDisplay;
               flag.g := true;
               flag.D := true;
               ShowPotentialEnergy;
            end;
         MCHD:
            begin
               InitializeMCVariables;
               Setupg_of_rDisplay;
               flag.g := true;
               flag.D := true;
            end;
      end;  {case}
   end;   {reset_variables}

   procedure HandleHotKeys;
   begin
      case knum of
         1: begin
               HelpHotkeys;
               Event.KeyPressed := false;   {hack}
               Event.MouseClicked := false;   {hack}
            end;
         2: run_pause;
         3: hide_show;
         4: reset_variables;
         5: refresh;
         6: If Menu.Chosen then HandleMenu;
      end; {case}
      if not start then CheckForEvents;
   end;  {HandleHotKeys}


   procedure RunSimulation;
   begin
      case flag.run of
         MDLJ: {molecular dynamics, Lennard-Jones}
            begin
               if flag.change then
                  InitialMDLJ;
               RunMDLJ;
            end;
         MDHD:
            begin
               if flag.change then
                  InitialMDHD;
               RunMDHD;
            end;
         MCLJ:
            begin
               if flag.change then
                  InitialMCLJ;
               RunMCLJ;
            end;
         MCHD:
            begin
               if flag.change then
                  InitialMCHD;
               RunMCHD;
            end;
      end;
   end; {RunSimulation}

begin            {main program}
   CUPSinit;
   warning := false;
{   GraphDefaults;}
   ShowTitle;
   InitializeMenu;
   Setflags;
   GetDefaults;
   DisplayDefaults;
   InitialConditions;
   CheckTotalMomentum;
   KineticEnergy;
   SetHeatBath;  {set temperature of heat bath to mean kinetic energy}
   SetupSlider;
   DefineViewPort(2, 0.07, 0.49, 0.07, 0.50);
   DefineViewPort(8, 0.55, 0.99, 0.07, 0.2);
   DefineViewPort(4, 0.05, 0.5, 0.74, 0.94);
   DefineViewPort(6, 0.57, 1.0, 0.70, 0.90);
   DefineScale(6,0,1,0,1);
   repeat  {main loop}
      RunSimulation;
      if hotkeys.pressed(knum) then
         HandleHotKeys;
      if Menu.activated then
         handlemenu;
   until quit;
   closeup;
   CupsDone;
end.   {main program}
