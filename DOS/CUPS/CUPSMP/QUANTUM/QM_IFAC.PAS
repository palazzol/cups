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

UNIT qm_Ifac;

INTERFACE

USES Crt, Graph, CUPS, CUPSmupp, CUPSgui, CUPSfunc, CUPSproc, CUPSpars,
     Objects, XSliders;

CONST
   MyBackColor = 0;

TYPE
   TInterface = Object
      Menu: TMenu;
      CurrentHelp: HelpScrType;
      HotKeys: THotKeys;
      Sliders: TXSliders;
      SlidersExist: Boolean;
      FUNCTION ProcessEvent(VAR Message: MessageRec): Boolean;
      FUNCTION QueryEvent(VAR Message: MessageRec): Boolean;
   END;



IMPLEMENTATION


PROCEDURE AboutProgram;
   VAR
      AboutScr : HelpScrType;
   BEGIN              {                                                  }
      AboutScr[01] := '                                                  ';
      AboutScr[02] := '                  ABOUT QUANTUM                   ';
      AboutScr[03] := 'Quantum is a program to aimed at helping the user ';
      AboutScr[04] := 'gain an understanding of the basic wave mechanics ';
      AboutScr[05] := 'encountered when first studying quantum mechanics ';
      AboutScr[06] := 'during a modern physics course.  It is divided    ';
      AboutScr[07] := 'into four sections.  The first section,           ';
      AboutScr[08] := 'UNCERTAINTY PRINCIPLE, is for looking at the      ';
      AboutScr[09] := 'relationship between the spatial distribution and ';
      AboutScr[10] := 'momentum distribution of a wave.  The second part,';
      AboutScr[11] := 'TIME DEPENDENT FREE PARTICLES, looks at the time  ';
      AboutScr[12] := 'development of wave packets for various dispersion';
      AboutScr[13] := 'functions to help the user with concepts of phase ';
      AboutScr[14] := 'and group velocity and dispersion.  The third     ';
      AboutScr[15] := 'section, BARRIERS, is for studying the interaction';
      AboutScr[16] := 'of asymptotically free waves with a barrier.  Both';
      AboutScr[17] := 'stationary wave and wavepacket solutions to the   ';
      AboutScr[18] := 'Schrodinger equation can be displayed along with  ';
      AboutScr[19] := 'the reflection and transmission coefficients of   ';
      AboutScr[20] := 'the barrier.  The fourth section, SIMPLE WELLS, is';
      AboutScr[21] := 'for studying the Schrodinger equation solutions of';
      AboutScr[22] := 'single and double finite square wells             ';
      AboutScr[23] := '                                                  ';
      AboutScr[24] := '    Press any Key or Click Mouse to continue.     ';
      AboutScr[25] := '                                                  ';
      Help(AboutScr);
   END;

PROCEDURE AboutSection(next: Integer);

   VAR
      AboutScr : HelpScrType;
   BEGIN
   CASE next OF
   1: BEGIN              {                                                  }
         AboutScr[01] := '                                                  ';
         AboutScr[02] := '          ABOUT UNCERTAINTY PRINCIPLE            ';
         AboutScr[03] := ' ';
         AboutScr[04] := 'The uncertainty principle section of this program';
         AboutScr[05] := 'allows the user to investigate the relationship';
         AboutScr[06] := 'between the spatial wavefunction and the momentum';
         AboutScr[07] := 'distribution of a packet.  ';
         AboutScr[08] := '  ';
         AboutScr[09] := 'The initial screen that the user sees displays the';
         AboutScr[10] := 'the probabilty density of a gaussian wavepacket';
         AboutScr[11] := 'and its corresponding momentum distribution.  The';
         AboutScr[12] := 'width and mean values of both distributions can be';
         AboutScr[13] := 'adjusted using sliders.  The shape of one of the ';
         AboutScr[14] := 'distributions can be specified using the Particle';
         AboutScr[15] := 'Particle Parameter screen under the Parameters';
         AboutScr[16] := ' menu item.  The program then calculates the other';
         AboutScr[17] := 'distribution.';
         AboutScr[18] := '';
         AboutScr[19] := 'Users can investigate the real and imaginary';
         AboutScr[20] := 'components of the wavefunctions by using the F6';
         AboutScr[21] := '"Show Re" hot key.';
         AboutScr[22] := '';
         AboutScr[23] := ' ';
         AboutScr[24] := '    Press any Key or Click Mouse to continue.     ';
         AboutScr[25] := '                                                  ';
         Help(AboutScr);
      END;
   2: BEGIN              {                                                  }
         AboutScr[01] := '                                                  ';
         AboutScr[02] := '        ABOUT TIME DEPENDENT FREE PARTICLES       ';
         AboutScr[03] := ' ';
         AboutScr[04] := ' This section shows the time development of ';
         AboutScr[05] := ' Gaussian wavepackets for various dispersion';
         AboutScr[06] := ' relations.  It shows the exact time evolution of';
         AboutScr[07] := ' the wavepackets for Schrodinger''s equation and ';
         AboutScr[08] := ' for the electromagnetic wave equation.  For any';
         AboutScr[09] := ' other dispersion relation selected by the user,';
         AboutScr[10] := ' the program shows the time propagation of an ';
         AboutScr[11] := ' approximate Gaussian wavefunction made up of ';
         AboutScr[12] := ' a linear combination of momentum eigenfunctions.';
         AboutScr[13] := ' ';
         AboutScr[14] := ' The user can adjust the mean momentum of the ';
         AboutScr[15] := ' wavepacket and the initial width of the  ';
         AboutScr[16] := ' probability density in position space using ';
         AboutScr[17] := ' sliders on the screen. ';
         AboutScr[18] := ' ';
         AboutScr[19] := '  ';
         AboutScr[20] := '';
         AboutScr[21] := '';
         AboutScr[22] := ' ';
         AboutScr[23] := '                                                  ';
         AboutScr[24] := '    Press any Key or Click Mouse to continue.     ';
         AboutScr[25] := '                                                  ';
         Help(AboutScr);
      END;
   3: BEGIN              {                                                  }
         AboutScr[01] := '                                                  ';
         AboutScr[02] := '                  ABOUT BARRIERS                  ';
         AboutScr[03] := ' ';
         AboutScr[04] := ' This section allows users to define a potential';
         AboutScr[05] := ' barrier and investigate the solutions to ';
         AboutScr[06] := ' Schordinger''s equation for that potential ';
         AboutScr[07] := ' barrier.  The reflection and transmission';
         AboutScr[08] := ' coefficients are calculated for the barrier ';
         AboutScr[09] := ' specified by the user, either eigenfunctions or';
         AboutScr[10] := ' wavepacketscan be displayed and their time';
         AboutScr[11] := ' evolution can be followed in animation.';
         AboutScr[12] := '';
         AboutScr[13] := ' An integration tool is available for making';
         AboutScr[14] := ' measurements on the wavefunctions displayed.';
         AboutScr[15] := '';
         AboutScr[16] := '';
         AboutScr[17] := '  ';
         AboutScr[18] := ' ';
         AboutScr[19] := '  ';
         AboutScr[20] := '';
         AboutScr[21] := '';
         AboutScr[22] := ' ';
         AboutScr[23] := '                                                  ';
         AboutScr[24] := '    Press any Key or Click Mouse to continue.     ';
         AboutScr[25] := '                                                  ';
         Help(AboutScr);
      END;
   4: BEGIN              {                                                  }
         AboutScr[01] := '                                                  ';
         AboutScr[02] := '          ABOUT BOUND SOLUTIONS IN WELLS          ';
         AboutScr[03] := ' ';
         AboutScr[04] := ' The Bound Solutions in Wells section of this     ';
         AboutScr[05] := ' program allows users to investigate the exact    ';
         AboutScr[06] := ' bound solutions to Schrodinger''s equation in a  ';
         AboutScr[07] := ' square well and investigate approximate solutions';
         AboutScr[08] := ' for arbitrary potentials input by the user.      ';
         AboutScr[09] := ' ';
         AboutScr[10] := ' The program solves for and displays the energies ';
         AboutScr[11] := ' and wavefunctions of the 10 lowest energy        ';
         AboutScr[12] := ' eigenvalues for the chosen potential.  The user  ';
         AboutScr[13] := ' can construct a wavepacket made of up to three   ';
         AboutScr[14] := ' component eigenfunctions and amplitudes.  The    ';
         AboutScr[15] := ' time evolution of the wavepacket can be viewed in';
         AboutScr[16] := ' animation.';
         AboutScr[17] := '  ';
         AboutScr[18] := ' There is a tool for finding the integral of the  ';
         AboutScr[19] := ' probability density, first and second moments of ';
         AboutScr[20] := ' position, and potential over a selected region.  ';
         AboutScr[21] := '';
         AboutScr[22] := '';
         AboutScr[23] := ' ';
         AboutScr[24] := '    Press any Key or Click Mouse to continue.     ';
         AboutScr[25] := '                                                  ';
         Help(AboutScr);
      END;
      END;
   END;


FUNCTION TInterface.ProcessEvent(VAR Message: MessageRec): Boolean;

   VAR
      knum: Byte;

   PROCEDURE ProcessMenu;

      BEGIN
        WITH Message DO
        BEGIN
         CASE Menu.ColChosen OF
            1: CASE Menu.RowChosen of
               1: AboutCups;
               2: AboutProgram;
               3: AboutSection(next);
               4: Configuration;
               6: BEGIN
                   Paused := False;
                   Quit := TRUE;
                   Done := True;
                   ProcessEvent := True;
                  END;
               END;
            2: BEGIN
                 Next := menu.RowChosen;
                 Quit := True;
                 Paused := False;
                 ProcessEvent := True;
               END;
            3: BEGIN
                 Change := menu.RowChosen;
                 ProcessEvent := True;
               END;
         END;
         END;
     END;

   BEGIN
      WITH Message DO
      BEGIN
         ProcessEvent := False;
         IF Menu.Activated THEN ProcessMenu;
         IF hotKeys.Pressed(kNum) THEN
            BEGIN
            ProcessEvent := True;
            CASE kNum of
               1: Change := 10;
               2: BEGIN
                     IF Paused THEN
                       BEGIN
                         Paused := False;
                         hotKeys.key[2] := 'F2-Stop';
                         hotKeys.Display;
                       END
                     ELSE
                       BEGIN
                         Paused := True;
                         hotKeys.key[2] := 'F2-Run';
                         hotKeys.Display;
                       END;
                     Quit := False;
                  END;
               3: Change := 13;
               4: Change := 14;
               5: Change := 15;
               6: IF Menu.Chosen THEN ProcessMenu;
            END;
            END;
            IF SlidersExist THEN IF Sliders.Changed THEN
               BEGIN
                  ProcessEvent := True;
                  Change := 20;
               END;
      END;
   END;

{=========================================================================

     Check for events and loop during pauses.

==========================================================================}
FUNCTION TInterface.QueryEvent(VAR Message: MessageRec): Boolean;
   BEGIN
      WITH Message DO
      BEGIN
         QueryEvent := False;
         CheckForEvents;
         IF Event.KeyPressed OR Event.MouseClicked THEN
             QueryEvent := ProcessEvent(Message);

      END;
   END;

END.