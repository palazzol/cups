           (*************************************************)
           (*************************************************)
           (**                                             **)
           (**  Consortium of Upper-level Physics Software **)
           (**              ( CUPS Project )               **)
           (**      ( c) 1994 by John Wiley & Sons         **)
           (**  Compiled with Utilities Ver. 1.9 (95/09/10)**)
           (**                                             **)
           (*************************************************)
           (*************************************************)

{ Start of file QM_MAIN.PAS }


{ ----------------------------------------------------------------------
   Program Title:    One Dimensional Wave Mechanics
   Author:           Douglas E. Brandt
   Version:          0.11
   Program Summary:  This is a program consisting of several sections that
                     can be used to gain underst
                     anding of several aspects
                     of wave mechanics.  This version consists of four
                     different sections.

                        Free Particles - Time Independent

                     This is the first section of the CUPS Modern Physics
                     One Dimensional Wave Mechanics Program.  Its code is
                     found in the file QM_FPTI.PAS, which should be
                     compiled to QM_FPTI.TPU.  It is written to give
                     insight into the complementary nature of position
                     and momentum in wave mechanics in relation to the
                     uncertainty principle.  It is able to investigate
                     Gaussian, rectangular, and triangular shaped wave
                     packets.  It can be used at an elementary level by
                     not displaying real and imaginary components of the
                     functions.

                        Free Particles - Time Dependent

                     This is the second section of the CUPS Modern Physics
                     One Dimensional Wave Mechanics Program.  Its code
                     is found in the file QM_FPTD.PAS, which should be
                     compiled to QM_FPTD.TPU.  It displays free Gaussian
                     wave packets and animates their time evolution.
                     It can display the exact evolution of a true Gaussian
                     wave packet according to the Schrodinger equation.
                     In order to give insight into phase and group velocity,
                     the time propagation of three momentum eigenfunction
                     components of the wave packet are also displayed.
                     To give insight into dispersion relations, the
                     gaussian wave packet can be modeled as a superposition
                     of momentum eigenwaves.  This superposition of
                     momentum eigenwaves can be propagated through time
                     subject to a choice of dispersion relations.

                        Barriers

                     This is the third section of the CUPS Modern Physics
                     One Dimensional Wave Mechanics Program.  Its code is
                     found in the file QM_BARR.PAS, which should be compiled
                     to QM_BARR.TPU.  This section shows Shrodinger waves
                     incident on potential barriers.  It displays the
                     potential as a function of position, the reflection and
                     transmission coefficients of energy eigenstates
                     incident on the barrier, and can display the time
                     evolution of the the energy eigenstates or the time
                     evolution of a wave packet incident on the barrier.The
                     current version only allows a single barrier, but the
                     final version will allow a general piecewise constant
                     barrier.

                        Particles in Wells

                     This is one section of the CUPS Modern Physics
                     One Dimensional Wave Mechanics Program.  Its code is
                     found in the file QM_SWELL.PAS.  It should be compiled
                     to QM_SWELL.TPU.  This program solves for the energy
                     eigenvalues and eigenstates for a piecewise constant
                     potential well.  The current version solves only a
                     single well, but the final version will solve for a
                     more general piecewise constant potential.  The program
                     displays the energy eigenstates of the well, energy
                     of the corresponding width infinite square well energy
                     eigenvalues, and a wavepacket made of a superposition
                     of up to three energy eigenstates.  The propagation of
                     the wavepacket through time can be shown in animation.

   Library Files:    CRT,DOS,GRAPH,MUPPET,CUPSGRAPH,CUPSMATH
   Utilities:
   Algorithms:
   NOTE:             Each of the section files are set up so that they can
                     be compiled either as a unit or as a stand alone
                     program.  To compile any of the sections as a stand
                     alone program, the compiler define directive must be used
                     to define MAIN.  To compile to a unit, MAIN must not be
                     defined by the compiler define directive.
------------------------------------------------------------------------}
{F+}

{$M 64000, 120000, 655360}

PROGRAM One_D_Wave_Mechanics;

USES Crt, Dos, Graph, CUPSmupp, CUPS, CUPSfunc, CUPSproc, CUPSgrph,
     CUPSgui, Objects, QM_FPTI, QM_FPTD, QM_BARR, QM_SWEL, QM_Ifac;

CONST
   BorderColor = 1;
   AxesColor = 1;
   BackColor = 1;
   PsiColor = 1;
   PsiSquaredColor = 13;
   RealPsiColor = 11;
   ImagPsiColor = 10;
   PhiColor = 1;
   RealPhiColor = 1;
   ImagPhiColor =1 ;


VAR
   kNum: byte;
   done, next: integer;
   Main_Menu: TMenu;
   hotKeys: THotKeys;
   Message: MessageRec;
   QMInterface: TInterface;

PROCEDURE Initialize;

   BEGIN
      CUPSinit;
      WITH QMInterface DO
      BEGIN
         Menu.init;
         Menu.Column(1,'File');
            Menu.Row(1,1,'About CUPS');
            Menu.Row(1,2,'About Program');
            Menu.Row(1,3,'About Section');
            Menu.Row(1,4,'Configuration');
            Menu.Row(1,5,'-------------');
            Menu.Row(1,6,'Exit Program');
            Menu.RowActivate(1,5,false);
         Menu.Column(2,'Section');
            Menu.Row(2,1,'Uncertainty Principle');
            Menu.Row(2,2,'Time Dependent Free Particles');
            Menu.Row(2,3,'Barriers');
            Menu.Row(2,4,'Bound Particles in a Well');
         Menu.Column(3,'Parameters');
            Menu.Row(3,1, 'Wave Function Parameters');
            Menu.Row(3,2, 'Barrier Parameters');
            Menu.Row(3,3, 'Well Parameters');
         Menu.Autochecking(2,1);
         MEnu.ColSize := 11;
         Menu.Display;
         hotKeys.init(6);
         hotKeys.key[1] := 'F1-Help';
         hotKeys.key[2] := 'F2-Run';
         hotKeys.key[3] := 'F3-Reset';
         hotKeys.key[4] := 'F6-Show Real';
         hotKeys.key[5] := 'F7-Integral';
         hotKeys.key[6] := 'F10-Menu';
         hotKeys.Display;
      END;
   END;

PROCEDURE StartupScreen;
   VAR
      AboutScr : HelpScrType;
   BEGIN              {                                                  }
      AboutScr[01] := '                                                  ';
      AboutScr[02] := '                CUPS MODERN PHYSICS               ';
      AboutScr[03] := '                 QUANTUM MECHANICS                ';
      AboutScr[04] := '                                                  ';
      AboutScr[05] := '                        by                        ';
      AboutScr[06] := '                 Douglas E. Brandt                ';
      AboutScr[07] := '           Eastern Illinois University            ';
      AboutScr[08] := '                                                  ';
      AboutScr[09] := '          Version 0.12     May 24, 1993           ';
      AboutScr[10] := '                Copyright (c) 1993                ';
      AboutScr[11] := '                                                  ';
      AboutScr[12] := '                                                  ';
      AboutScr[13] := '                                                  ';
      AboutScr[14] := '   This program consists of four different        ';
      AboutScr[15] := '   sections of simulations of various aspects     ';
      AboutScr[16] := '   of the behavior of waves encountered in an     ';
      AboutScr[17] := '   introduction to quantum mechanics in a course  ';
      AboutScr[18] := '   in modern physics: wave packets, the time      ';
      AboutScr[19] := '   development of free wave packets, solutions    ';
      AboutScr[20] := '   to barrier problems, and the solution to       ';
      AboutScr[21] := '   bound particle problems all in one dimension.  ';
      AboutScr[22] := '                                                  ';
      AboutScr[23] := '                                                  ';
      AboutScr[24] := '    Press any Key or Click Mouse to continue.     ';
      AboutScr[25] := '                                                  ';
      Help(AboutScr);
   END;


BEGIN
   Initialize;
   GraphBackColor := MyBackColor;
   {InitHelpScr1};
   {CurrentHelp := HelpScr1};
   {Define_Input_Screens};
   WITH Message DO
   WITH QMInterface DO
   BEGIN
      Paused := True;
      Done := False;
      next := 1;
      StartUpScreen;
      Free_Particle(QMInterface, Message);
      WHILE Done = False DO
         CASE next OF
            1: Free_Particle(QMInterface,Message);
            2: Free_Particle_Time(QMInterface,Message);
            3: Potential_Barriers(QMInterface,Message);
            4: Simple_Well(QMInterface,Message);
         END;
   END;
   CUPSdone;

END.  { End of file QM_MAIN.PAS }