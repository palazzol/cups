{Program title: Hydrogenic atoms and the H2+ molecule                }
{Author: John R. Hiller                                              }
{Version: 1.0               9/12/95                                  }
{Copyright (c) 1995  John Wiley & Sons, Inc.                         }
{                                                                    }
{Program summary:                                                    }
{   Compute eigenfunctions and eigenenergies for hydrogen, hydrogenic}
{atoms, and single-electron diatomic ions.  Hydrogenic atoms may be  }
{exposed to external fields of the following types: uniform electric }
{fields, uniform magnetic fields, and the Coulomb field of another   }
{charge.  The magnetic interaction is the quadratic Zeeman term;     }
{in the absence of spin-orbit coupling, the linear term adds only a  }
{trivial energy shift.                                               }
{   The unperturbed hydrogenic eigenfunctions are computed directly  }
{from the known solutions.  When external fields are included the    }
{eigenfunctions are obtained from approximate diagonalization in a   }
{basis of unperturbed functions or from solution of a finite-        }
{difference approximation via the Lanczos diagonalization algorithm. }
{When the molecular state is considered, only the Lanczos algorithm  }
{is used.                                                            }
{   In the molecular case, an effective nuclear potential is recorded}
{for use in calculation of the nuclear binding energy.               }
{                                                                    }
{Library files: none                                                 }
{Utilities: Graph, CUPS, CUPSmupp, CUPSfunc, CUPSproc, CUPSgui,      }
{           CUPSgrph                                                 }
{Other units: HatmMach, HatmPlot, HatmCalc, HatmFile, HatmEign       }
{Algorithms: Lanczos, basis-function                                 }
{                                                                    }
PROGRAM Hatom(input,output);

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph,
     HatmMach,HatmPlot,HatmCalc,HatmFile,HatmEign;

VAR
   WishToQuit,
   WishToSave    :boolean;
   MainMenu      :TMenu;
   MainKeys      :THotKeys;
   Mkey          :byte;
   FileDesc      :FileDescType;
   cPi           :real;          {global constant set to Turbo Pi fn}

{--------Routines to prepare and handle main menu and hot keys---------}

PROCEDURE Initialize(VAR FileDesc :FileDescType;
                     VAR WF :WFtype; VAR Potl :PotlType);
VAR
   nn :integer;
   EnoughMemory :boolean;
BEGIN
   DefineAllViewPorts;
   WITH WF DO                             
      BEGIN
         {New(Rnl); New(Psi); New(PsiSlice); {Mac only}
         {New(Veff); New(Dipole); {Mac only}
         Rnl.Init(1); Psi.Init(1,1); PsiSlice.Init(1,1);
         Veff.Init(1); Dipole.Init(1);
      END;
   WITH Potl DO
      BEGIN
         {New(Vmat); {Mac only}
         Vmat.Init(1,1);
      END;
   NewFile(FileDesc,WF,Potl);
   MainMenu.Display; MainKeys.Display;
   WITH WF,Potl DO  {Display hydrogen ground state.}
      BEGIN
         Computed:=Undistorted;
         En:=-sqr(Z1*charge)/sqr(n);
         PrepRadialWF(Rnl,n,l,nSteps,-Z1*charge*Potl.dr);
         PrepStorage(WF,Potl,EnoughMemory);
         PrepPsiSlice(WF,Potl);
         Plotted:=False;
         PrepUWFPlotScr(WF,Potl);
         PlotWF(WF,Potl,True);
      END;
END;

PROCEDURE AboutProgram;
VAR
   AboutProg :HelpScrType;
   nn        :integer;
BEGIN
   {New(AboutProg); {Mac only}
   AboutProg[01]:='';
   AboutProg[02]:='      HYDROGENIC ATOMS AND THE H2+ MOLECULE';
   AboutProg[03]:='';
   AboutProg[04]:='                 John R. Hiller';
   AboutProg[05]:='         University of Minnesota, Duluth';
   AboutProg[06]:='';
   AboutProg[07]:=ConCat('                  Version ',NumStr(Version,3,1));
   AboutProg[08]:='         (c) 1995 John Wiley & Sons, Inc.';
   AboutProg[09]:='';
   AboutProg[10]:='     This program computes eigenfunctions and';
   AboutProg[11]:='  eigenenergies for hydrogen, hydrogenic atoms,';
   AboutProg[12]:='  and single-electron diatomic ions.  Hydrogenic';
   AboutProg[13]:='  atoms may be exposed to uniform electric and';
   AboutProg[14]:='  magnetic fields.  Spin interactions are not';
   AboutProg[15]:='  included.  The magnetic interaction used is';
   AboutProg[16]:='  the quadratic Zeeman term; in the absence of';
   AboutProg[17]:='  spin-orbit coupling, a linear term would add';
   AboutProg[18]:='  only a trivial energy shift.  In the diatomic';
   AboutProg[19]:='  case, an effective nuclear potential can be';
   AboutProg[20]:='  recorded for use in calculation of the nuclear';
   AboutProg[21]:='  binding energy.';
   AboutProg[25]:='    Press <Enter> or click mouse to continue.';
   FOR nn:=22 TO 24 DO AboutProg[nn]:='';
   Help(AboutProg);
   {Dispose(AboutProg); {Mac only}
END;

PROCEDURE AboutMenus;
VAR
   AboutMenu :HelpScrType;
   nn        :integer;
BEGIN
   {New(AboutMenu); {Mac only}
   AboutMenu[01]:='';
   AboutMenu[02]:=' Menu choices:';
   AboutMenu[03]:='';
   AboutMenu[04]:='    File - use files to obtain or save data;';
   AboutMenu[05]:='       information about CUPS and the program;';
   AboutMenu[06]:='       exit from program.';
   AboutMenu[07]:='';
   AboutMenu[08]:='    Particle - set light particle type & mass,';
   AboutMenu[09]:='       and energy & length units.';
   AboutMenu[10]:='';
   AboutMenu[11]:='    Compute - compute energies and wave functions';
   AboutMenu[12]:='       with and without external forces present.';
   AboutMenu[13]:='';
   AboutMenu[14]:='    Help - display various help screens.';
   FOR nn:=15 TO 25 DO AboutMenu[nn]:='';
   Help(AboutMenu);
   {Dispose(AboutMenu); {Mac only}
END;

PROCEDURE AboutFiles;
VAR
   AboutFile :HelpScrType;
BEGIN
   {New(AboutFile); {Mac only}
   AboutFile[01]:=' File: use files to obtain or save information.';
   AboutFile[02]:='';
   AboutFile[03]:='    * About CUPS - show description';
   AboutFile[04]:='                   of software consortium.';
   AboutFile[05]:='';
   AboutFile[06]:='    * About Program - show credits and';
   AboutFile[07]:='                      a brief description.';
   AboutFile[08]:='';
   AboutFile[09]:='    * Configuration - verify and/or change';
   AboutFile[10]:='                      program configuration.';
   AboutFile[11]:='';
   AboutFile[12]:=
           ConCat('    * New - set file name to default ',FileDesc.FileName);
   AboutFile[13]:='            and start new calculation.';
   AboutFile[14]:='';
   AboutFile[15]:='    * Open - open file and read contents.';
   AboutFile[16]:='';
   AboutFile[17]:='    * Save - save current state to file.';
   AboutFile[18]:='';
   AboutFile[19]:='    * Save As - save current state';
   AboutFile[20]:='                to file with chosen name and';
   AboutFile[21]:='                set file name to this choice.';
   AboutFile[22]:='';
   AboutFile[23]:='    * Play Back - play recorded sequence.';
   AboutFile[24]:='';
   AboutFile[25]:='    * Exit Program.';
   Help(AboutFile);
   {Dispose(AboutFile); {Mac only}
END;

PROCEDURE AboutParticles;
VAR
   AboutPart :HelpScrType;
   nn        :integer;
BEGIN
   {New(AboutPart); {Mac only}
   AboutPart[01]:='';
   AboutPart[02]:=' Particle: Select light particle type and units.';
   AboutPart[03]:='         A dialog box is presented for selection';
   AboutPart[04]:='         of the particle type or a specified';
   AboutPart[05]:='         mass and charge, and a choice of units';
   AboutPart[06]:='         for the calculation.';
   FOR nn:=7 TO 25 DO AboutPart[nn]:='';
   Help(AboutPart);
   {Dispose(AboutPart); {Mac only}
END;

PROCEDURE AboutCompute;
VAR
   AboutComp :HelpScrType;
   nn        :integer;
BEGIN
   {New(AboutComp); {Mac only}
   AboutComp[01]:='';
   AboutComp[02]:=' Compute: plot previous result, if any, and';
   AboutComp[03]:='          compute new results, if desired,';
   AboutComp[04]:='          in the chosen category.';
   AboutComp[05]:='';
   AboutComp[06]:='    * Undistorted Eigenfunctions.';
   AboutComp[07]:='';
   AboutComp[08]:='    * Distorted Eigenfunctions, in the';
   AboutComp[09]:='         presence of external fields.';
   AboutComp[10]:='';
   AboutComp[11]:='    * Shared Eigenfunction of particle';
   AboutComp[12]:='         that provides molecular binding.';
   AboutComp[13]:='         An effective nuclear potential is';
   AboutComp[14]:='         computed for a series of separations.';
   AboutComp[15]:='';
   AboutComp[16]:='    * Binding Of Molecule.  Compute the binding';
   AboutComp[17]:='         energy and molecular wave function.';
   AboutComp[18]:='         The effective potential must be computed';
   AboutComp[19]:='         first.   This is done by computing the';
   AboutComp[20]:='         ''Shared Eigenfunction'' in auto mode,';
   AboutComp[21]:='         with recording of the effective';
   AboutComp[22]:='         potential selected.';
   FOR nn:=23 TO 25 DO AboutComp[nn]:='';
   Help(AboutComp);
   {Dispose(AboutComp); {Mac only}
END;

PROCEDURE AboutAlgorithms;
VAR
   AboutAlgs :HelpScrType;
   nn        :integer;
BEGIN
   {New(AboutAlgs); {Mac only}
   AboutAlgs[01]:='';
   AboutAlgs[02]:=' Algorithms:';
   AboutAlgs[03]:='';
   AboutAlgs[04]:='    Two algorithms are included for the solution';
   AboutAlgs[05]:=' of the cylindrical wave equation that determines';
   AboutAlgs[06]:=' the wave function for a hydrogenic atom in a';
   AboutAlgs[07]:=' cylindrically-symmetric external field.  One';
   AboutAlgs[08]:=' algorithm is a basis-function approach, in';
   AboutAlgs[09]:=' which the wave function is approximated by a';
   AboutAlgs[10]:=' finite expansion in unperturbed hydrogenic';
   AboutAlgs[11]:=' eigenfunctions; the expansion coefficients are';
   AboutAlgs[12]:=' obtained by diagonalizing the Hamiltonian in';
   AboutAlgs[13]:=' this finite basis.  The other algorithm is the';
   AboutAlgs[14]:=' Lanczos diagonalization method applied to a';
   AboutAlgs[15]:=' finite-difference representation for the wave';
   AboutAlgs[16]:=' equation; it is an iterative method that will';
   AboutAlgs[17]:=' continue until a specified tolerance is reached';
   AboutAlgs[18]:=' or the number of iterations reaches some preset';
   AboutAlgs[19]:=' limit.';
   FOR nn:=20 TO 25 DO AboutAlgs[nn]:='';
   Help(AboutAlgs);
   {Dispose(AboutAlgs); {Mac only}
END;


PROCEDURE PrepMain(VAR MainMenu :TMenu; VAR MainKeys :THotKeys);
{Prepare main menu and hot keys.}
BEGIN
   WITH MainMenu DO               {Prepare main menu.}
      BEGIN
         Init;
         Column(1,'File');
            Row(1,1,'About CUPS');
            Row(1,2,'About Program');
            Row(1,3,'Configuration');
            Row(1,4,'-------------');
            Row(1,5,'New');
            Row(1,6,'Open...');
            Row(1,7,'Save');
            Row(1,8,'Save As...');
            Row(1,9,'Play Back...');
            Row(1,10,'-------------');
            Row(1,11,'Exit Program');
            RowActivate(1,4, False);
            RowActivate(1,10,False);
         Column(2,'Particle');
         Column(3,'Compute');
            Row(3,1,'Undistorted Eigenfunctions');
            Row(3,2,'Distorted Eigenfunctions');
            Row(3,3,'Shared Eigenfunction');
            Row(3,4,'Binding of Molecule');
            RowActivate(3,4,False);
         Column(4,'Help');
            Row(4,1,'Summary');
            Row(4,2,'`File''');
            Row(4,3,'`Particle''');
            Row(4,4,'`Compute''');
            Row(4,5,'Algorithms');
      END;
   WITH MainKeys DO                {Prepare main hot keys.}
      BEGIN
         Init(2);
         key[1]:='F1-Help';
         key[2]:='F10-Menu';
         Display;
      END;
END; {PrepMain}

PROCEDURE HandleMenu(VAR MainMenu :TMenu; VAR MainKeys :THotKeys;
                     VAR FileDesc :FileDescType; VAR WF :WFtype;
                     VAR Potl :PotlType; VAR WishToQuit :boolean);
{Process selection from main menu.}
CONST
   Ffile=1; Pparticle=2; Compute=3; Hhelp=4;
VAR
   Verified,Modified,MenuClicked,WishToAbort :boolean;
BEGIN
   MainKeys.Clear;
   WITH MainMenu DO
      REPEAT
         MenuClicked:=False; Modified:=False; 
         CASE colChosen OF
            Ffile :case rowChosen OF
                      1 :AboutCUPS;
                      2 :AboutProgram;
                      3 :Configuration;
                      5 :BEGIN
                            VerifyNew(WF.Saved,Verified);
                            IF Verified THEN
                               BEGIN
                                  NewFile(FileDesc,WF,Potl);
                                  RowActivate(Compute,4,False);
                                  MainMenu.Display;
                               END;
                         END;
                      6 :BEGIN
                            VerifyNew(WF.Saved,Verified);
                            IF Verified THEN
                               BEGIN
                                  OpenDataFile(FileDesc,WF,Potl,WishToAbort);
                                  IF NOT WishToAbort THEN
                                     BEGIN
                                        RowActivate(Compute,4,WF.RecordVeff);
                                        IF WF.Computed<>NotDone THEN
                                           PlotWF(WF,Potl,True);
                                     END;
                               END;
                         END;
                      7 :SaveDataFile(FileDesc,WF,Potl);
                      8 :SaveAs(FileDesc,WF,Potl);
                      9 :PlayBack(FileDesc,WF,Potl,MainMenu,MenuClicked);
                      11:WishToQuit:=True
                      END; {case rowChosen}
            Pparticle :SetMassAndUnits(Potl,Modified);
            Compute :case rowChosen OF
                        1 :ComputeUWF(FileDesc,WF,Potl,MainMenu,MenuClicked);
                        2 :ComputeDWF(FileDesc,WF,Potl,MainMenu,MenuClicked);
                        3 :ComputeSWF(FileDesc,WF,Potl,MainMenu,MenuClicked);
                        4 :ComputeBWF(FileDesc,WF,Potl,MainMenu,MenuClicked);
                        END; {case rowChosen}
            Hhelp :case rowChosen OF
                      1 :AboutMenus;
                      2 :AboutFiles;
                      3 :AboutParticles;
                      4 :AboutCompute;
                      5 :AboutAlgorithms;
                      END; {case rowChosen}
            END; {case of colChosen}
         IF Modified THEN WF.Computed:=NotDone;
      UNTIL NOT MenuClicked;
   MainKeys.Display;
END; {HandleMenu}

PROCEDURE HandleHotKeys(VAR MainMenu :TMenu;
                        VAR MainKeys :THotKeys; Mkey :byte;
                        VAR FileDesc :FileDescType; VAR WF :WFtype;
                        VAR Potl :PotlType; VAR WishToQuit :boolean);
{Process selection from main hot keys.}
BEGIN
   MainKeys.Clear;
   CASE Mkey OF
      1 :AboutMenus;
      2 :BEGIN
            IF MainMenu.Chosen THEN
               HandleMenu(MainMenu,MainKeys,FileDesc,WF,Potl,WishToQuit);
         END;
      END; {case}
   MainKeys.Display;
END; {HandleHotKeys}

{--------Main program--------------------------------------------------}

BEGIN {Hatom}
   CUPSinit;
   ClearMUPPETport;
   ProgName:='Hatom'; VersionDate:='September 12, 1995'; Version:=1.0;
   cPi:=Pi;
   {New(MainMenu); New(MainKeys); {Mac only}
   PrepMain(MainMenu,MainKeys);
   Initialize(FileDesc,WF,Potl);
   AboutProgram;
   WishToQuit:=False;
   WHILE NOT WishToQuit DO
      BEGIN
         CheckForEvents;
         IF Event.ReadKey=Escape THEN WishToQuit:=True;
         IF MainKeys.Pressed(Mkey) THEN
            HandleHotKeys(MainMenu,MainKeys,Mkey,
                          FileDesc,WF,Potl,WishToQuit);
         IF MainMenu.Activated THEN
            HandleMenu(MainMenu,MainKeys,FileDesc,WF,Potl,WishToQuit);
         IF WishToQuit AND (NOT WF.Saved) THEN
            BEGIN
               VerifyQuit(FileDesc,WishToSave,WishToQuit);
               IF WishToSave THEN SaveDataFile(FileDesc,WF,Potl);
            END;
      END;
   MainMenu.Done;
   {Dispose(MainMenu); Dispose(MainKeys); {Mac only}
   {Dispose(WF.Rnl); Dispose(WF.Psi); Dispose(WF.PsiSlice); {Mac only}
   {Dispose(WF.Veff); Dispose(WF.Dipole); {Mac only}
   {Dispose(Potl.Vmat); {Mac only}
   ClearMUPPETport;
   CUPSdone;
END. {Hatom}
