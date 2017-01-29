           (*************************************************)
           (*************************************************)
           (**                                             **)
           (** Consortium of Upper-level Physics Software  **)
           (**             ( CUPS Project )                **)
           (**     (c) 1994 by John Wiley & Sons           **)
           (** Compiled with Utilities Ver. 1.7 (95/06/27) **)
           (**                                             **)
           (*************************************************)
           (*************************************************)

PROGRAM TWOBODY;
{$M 40000,0,655360}
{$N+}

                    {*********************************}
                    {*    Two-Nucleon Simulation     *}
                    {*          TWOBODY.PAS          *}
                    {*   Written by John Philpott    *}
                    {*        July XX, 1993.         *}
                    {*   Copyright (c) 1992, 1993    *}
                    {*********************************}
 
USES  crt, graph, CUPSmupp,
      CUPS, CUPSfunc, CUPSproc, CUPSgui, CUPSgrph, Consts,
      Master, S_Wave, DeutCalc, PhasCalc, All_Wave, Spin_Lab;
 
{ ------------------------- Initial Credit Screen ------------------------- }

PROCEDURE InitialCreditScreen;
VAR
   helpScr : HelpScrType;
BEGIN
   HelpScr[ 1] := '                                                  ';
   HelpScr[ 2] := '             THE TWO-NUCLEON PROBLEM              ';
   HelpScr[ 3] := '                                                  ';
   HelpScr[ 4] := '                        by                        ';
   HelpScr[ 5] := '                                                  ';
   HelpScr[ 6] := '                  John Philpott                   ';
   HelpScr[ 7] := '           The Florida State University           ';
   HelpScr[ 8] := '                                                  ';
   HelpScr[ 9] := '               Copyright (c) 1994                 ';
   HelpScr[10] := '                                                  ';
   HelpScr[11] := '                                                  ';
   HelpScr[12] := '  This program illustrates many features of the   ';
   HelpScr[13] := '  two-nucleon problem.  Bound state wavefunctions ';
   HelpScr[14] := '  can be calculated for a variety of interactions ';
   HelpScr[15] := '  that may include non-central parts.  Phase      ';
   HelpScr[16] := '  shifts and cross sections for pp, pn and nn     ';
   HelpScr[17] := '  scattering can be calculated and compared with  ';
   HelpScr[18] := '  those obtained experimentally.  Spin-polariza-  ';
   HelpScr[19] := '  tion features of the cross sections can be      ';
   HelpScr[20] := '  extensively investigated.  This program demon-  ';
   HelpScr[21] := '  strates the richness of the two-nucleon data    ';
   HelpScr[22] := '  and its relation to the underlying nucleon-     ';
   HelpScr[23] := '  nucleon interaction.                            ';
   HelpScr[24] := '                                                  ';
   HelpScr[25] := '                                                  ';
   Help(HelpScr);
END;  { InitialCreditScreen) }

{ -------------------------- Handle Master Menu --------------------------- }

PROCEDURE HandleMenu;
BEGIN { HandleMenu }
   WITH MasterMenu DO
      BEGIN { WITH }
         CASE ColChosen OF
            1: CASE RowChosen OF
              1: AboutCUPS;
              2: InitialCreditScreen;
              3: Configuration;
              5: quitprog := TRUE;
               END; {CASE}
            2: SWave;
            3: Deuteron;
            4: Phases;
            5: AllWaves;
            6: SpinLab;
         END; { CASE }
{        IF RowChosen <> 0 THEN
            BEGIN
               AutoChecking(ColChosen, RowChosen);}
         MasterMenu.Display;
         HotKeys.Display;
      END;  { WITH }
END;  { HandleMenu }
 
{ ------------------------ Handle Master Hot Keys ------------------------- }
 
PROCEDURE HandleHotKeys (key :byte);
BEGIN
   CASE key OF
    1 : ShowHelp('twobody.hlp','GeneralHelpScreen');
    2 : BEGIN
           CheckForEvents;
           IF MasterMenu.Chosen Then HandleMenu;
        END;
   END; {case}
END;
 
{ ----------------------------- MAIN PROGRAM ------------------------------ }
 
VAR
 key   :Byte;

BEGIN { TWOBODY.PAS }


   CUPSinit;
   SetUpMasterMenu;
   MasterMenu.Display;
   SetUpHotKeys;
   HotKeys.Display;
   InitialCreditScreen;
   GraphBackColor := Blakk;
   GeneralInit;
   SwaveInit;
   PhasesInit;
   InitializeSpinlabConstants;

   quitprog := FALSE;
   REPEAT       { main program loop }
      CheckForEvents;
      IF MasterMenu.Activated THEN HandleMenu;
      IF HotKeys.Pressed(key) THEN HandleHotKeys(key);
   UNTIL quitprog; { main program loop }
 
   CUPSdone;


END.  { TWOBODY.PAS }
