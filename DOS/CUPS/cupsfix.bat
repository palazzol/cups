@echo off
cls
echo **********************************************************
echo * This file will fix problem with a runtime error 200   *
echo * that CUPS programs (and all other programs compiled    *
echo * using Borland Pascal compiler) encounter on Pentium    *
echo * Pro 200MHz or a faster computer.                       *
echo *                                                        * 
echo * How to use it?                                         *
echo * 1) Copy cupsfix and tppatch programs to the directory  *
echo *    where all cups subdirectories (like cupsem) are.    *
echo *    By default it should be c:\cups directory.          *
echo * 2) If you have cupsem, than rename directoty imag&mul  *
echo *    to imagmul and imag&mul.exe (inside) to imagmul.exe *
echo *    and rename them back after running cupsfix.         *
echo * 3) Click cupsfix icon OR type either in DOS shell or   *
echo *    in Windows Run window: c:\cups\cupsfix. If cupsfix  *
echo *    is not in c:\cups than give the proper path.        *
echo **********************************************************
echo tppatch written by Andreas Bauer (andi.tio@hit.handshake.de)
echo cupsfix written by Jarek Tuszynski (jarek@chopin.gmu.edu)
echo Report problems to Jarek Tuszynski.
echo:
echo If you want to quit now press Ctrl-C; otherwise,
pause
call tppatch cupsas\cupsas.exe
call tppatch cupsas\Binaries\Binaries.exe
call tppatch cupsas\NBodies\NBodies.exe
call tppatch cupsas\Galaxies\Galaxies.exe
call tppatch cupsas\Stellar\Stellar.exe
call tppatch cupsas\Evolve\Evolve.exe
call tppatch cupsas\Pulse\Pulse.exe
call tppatch cupsas\Atmos\Atmos.exe
call tppatch cupscm\cupscm.exe
call tppatch cupscm\GenMot\gmGener.exe
call tppatch cupscm\GenMot\gm1DGrav.exe
call tppatch cupscm\GenMot\gmPend.exe
call tppatch cupscm\GenMot\gmRace.exe
call tppatch cupscm\Orbiter\Orbiter.exe
call tppatch cupscm\Anharm\Anharm.exe
call tppatch cupscm\Colision\Colision.exe
call tppatch cupscm\CoupOsc\CoupOsc.exe
call tppatch cupscm\Rotate\Rotate.exe
call tppatch cupsem\cupsem.exe
call tppatch cupsem\Fields\Fields.exe
call tppatch cupsem\Gauss\Gauss.exe
call tppatch cupsem\Poisson\Poisson.exe
call tppatch cupsem\ImagMul\ImagMul.exe  
call tppatch cupsem\AtomPol\AtomPol.exe    
call tppatch cupsem\Dielect\Dielect.exe
call tppatch cupsem\MagStat\MagStat.exe
call tppatch cupsem\QAnimate\QAnimate.exe
call tppatch cupsem\AccelQ\AccelQ.exe      
call tppatch cupsem\EMwave\EMwave.exe
call tppatch cupsmp\cupsmp.exe
call tppatch cupsmp\Scatter\Scatter.exe
call tppatch cupsmp\Germer\Germer.exe
call tppatch cupsmp\SpecRel\SpecRel.exe
call tppatch cupsmp\Laser\Laser.exe
call tppatch cupsmp\Nuclear\Nuclear.exe
call tppatch cupsmp\Nuclear\Decay.exe
call tppatch cupsmp\Quantum\Quantum.exe
call tppatch cupsmp\HAtom\HAtom.exe
call tppatch cupsnp\cupsnp.exe
call tppatch cupsnp\Nuclear\Nuclear.exe
call tppatch cupsnp\Nuclear\Decay.exe
call tppatch cupsnp\RelKin\RelKin.exe
call tppatch cupsnp\DetSim\DetSim.exe
call tppatch cupsnp\DetSim\Examples\trfitmen.exe
call tppatch cupsnp\NucRad\NucRad.exe
call tppatch cupsnp\ShellMod\ShellMod.exe
call tppatch cupsnp\TwoBody\TwoBody.exe
call tppatch cupsnp\ElScatt\ElScatt.exe
call tppatch cupsqm\cupsqm.exe
call tppatch cupsqm\Bound1D\Bound1D.exe
call tppatch cupsqm\Scattr1D\Scattr1D.exe
call tppatch cupsqm\QMtime\QMtime.exe
call tppatch cupsqm\Latce1D\Latce1D.exe
call tppatch cupsqm\Bound3D\Bound3D.exe
call tppatch cupsqm\Ident\Ident.exe
call tppatch cupsqm\Scattr3D\Scattr3D.exe
call tppatch cupsqm\CylSym\CylSym.exe
call tppatch cupsss\cupsss.exe
call tppatch cupsss\Phonons\Phonons.exe
call tppatch cupsss\spheat\spheat.exe
call tppatch cupsss\lattice\lattice.exe
call tppatch cupsss\bands\bands.exe
call tppatch cupsss\packet\packet.exe
call tppatch cupsss\solidlab\solidlab.exe
call tppatch cupsss\lcao\lcao.exe
call tppatch cupstp\cupstp.exe
call tppatch cupstp\Fluid\Fluid.exe
call tppatch cupstp\Engines\Engine.exe
call tppatch cupstp\Engines\Diesel.exe
call tppatch cupstp\Engines\Otto.exe
call tppatch cupstp\Engines\Wankel.exe
call tppatch cupstp\Probab\Galton.exe
call tppatch cupstp\Probab\PoisExp.exe
call tppatch cupstp\Probab\TwoD.exe
call tppatch cupstp\Probab\Kac.exe
call tppatch cupstp\Probab\Stadium.exe
call tppatch cupstp\ManyPart\ManyPart.exe
call tppatch cupstp\QMGas\QmGas1.exe
call tppatch cupstp\QMGas\QmGas2.exe
call tppatch cupstp\Ising\Ising.exe
call tppatch cupswo\cupswo.exe
call tppatch cupswo\Fourier\Fourier.exe
call tppatch cupswo\Wave\Wave.exe
call tppatch cupswo\Diffract\Diffract.exe
call tppatch cupswo\RayTrace\RayTrace.exe
call tppatch cupswo\TwoLens\TwoLens.exe
call tppatch cupswo\Spectrum\Spectrum.exe
call tppatch cupswo\EMWave\EMWave.exe
call tppatch cupswo\Chain\Chain.exe
echo all done