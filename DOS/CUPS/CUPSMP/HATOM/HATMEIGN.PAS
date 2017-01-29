{Unit of routines for calculation of H-atom eigenfunctions.          }
{Used by Hatom.pas.                                                  }
{                                                                    }
{Author: John R. Hiller                                              }
{Version: 1.0              7/24/95                                   }
{Copyright (c) 1995  John Wiley & Sons, Inc.                         }
{                                                                    }
UNIT HatmEign;

INTERFACE

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph,
     HatmMach,HatmCalc,HatmPlot,HatmFile;

VAR
   WF            :WFtype;
   Potl          :PotlType;

{--------Routines for undistorted eigenfunctions----------------------}

PROCEDURE ComputeUWF(VAR FileDesc :FileDescType;
                     VAR WF :WFtype; VAR Potl :PotlType;
                     VAR MainMenu :TMenu; VAR MenuClicked :boolean);

{--------Routines for distorted eigenfunctions-------------------------}

PROCEDURE ComputeDWF(VAR FileDesc :FileDescType;
                     VAR WF :WFtype; VAR Potl :PotlType;
                     VAR MainMenu :TMenu; VAR MenuClicked :boolean);

{--------Routines for shared eigenfunctions----------------------------}

PROCEDURE ComputeSWF(VAR FileDesc :FileDescType;
                     VAR WF :WFtype; VAR Potl :PotlType;
                     VAR MainMenu :TMenu; VAR MenuClicked :boolean);

{--------Routines for the binding eigenfunction------------------------}

PROCEDURE ComputeBWF(VAR FileDesc :FileDescType;
                     VAR WF :WFtype; VAR Potl :PotlType;
                     VAR MainMenu :TMenu; VAR MenuClicked :boolean);

IMPLEMENTATION

{--------Routines for undistorted eigenfunctions----------------------}

PROCEDURE ComputeUndistorted(VAR WF :WFtype; VAR Potl :PotlType);
VAR 
   ComputeScr :TInputScreen;
BEGIN
   {New(ComputeScr); {Mac only}
   WITH ComputeScr DO
      BEGIN
         Init;
         DefineInputPort(0.2,0.8,0.2,0.8);
         LoadLine('');
         LoadLine( '          Undistorted eigenfunctions');
         LoadLine('');
         LoadLine( '     Quantum numbers:');
         LoadLine( '             n: {   }  l: {   }  m: {   }');
         LoadLine('');
         LoadLine( '     Nuclear mass: {      }GeV/c^2');
         LoadLine( '          and charge: {   }e');
         LoadLine(
            ConCat('     Particle mass: ',Num2Str(Potl.Mass,6),
                                               ' ',Potl.Munit,'eV/c^2'));
         LoadLine('');
         LoadLine( '     Number of calculation steps: {    }');
         LoadLine(
            ConCat('     Range of calculation (r_max): {      }',
                                                            Potl.Lunit,'m'));
         LoadLine('');
         LoadLine('       [  OK  ]      [Cancel]');
         SetNumber(1,WF.n);
         SetNumber(2,WF.l);
         SetNumber(3,WF.m);
         SetNumber(4,Potl.m1);
         SetNumber(5,Potl.Z1);
         SetNumber(6,Potl.nSteps);
         SetNumber(7,Potl.L0*Potl.rMax);
         AcceptScreen;
         IF NOT Canceled THEN WITH WF,Potl DO
            BEGIN
               Computed:=Undistorted;
               RecordVeff:=False;
               n:=round(GetNumber(1));
               IF n<1 THEN IntInputWarn('n',n,1,2);
               l:=round(GetNumber(2));
               IF l<0 THEN IntInputWarn('l',l,0,2)
               ELSE IF l>=n THEN IntInputWarn('l',l,n-1,2);
               nr:=n-l-1;
               m:=round(GetNumber(3));
               IF (-l>m) OR (l<m) THEN IntInputWarn('m',m,0,2);
               m1:=GetNumber(4);
               IF m1<=0.0 THEN RealInputWarn('m_1',m1,0.938,6);
               Z1:=GetNumber(5);
               IF Z1<=0.0 THEN RealInputWarn('charge',Z1,1.0,6);
               mu:=ReducedMass(Mass,m1,Munit);
               InitializeScales(Potl);
               ComputeZeta(Potl);
               En:=-sqr(Z1*charge)/sqr(n);
               nSteps:=round(GetNumber(6));  
               IF nSteps<1 THEN
                  IntInputWarn('no. of calculation steps',nSteps,1,4)
               ELSE IF nSteps>MaxRange THEN
                  IntInputWarn('no. of calculation steps',nSteps,MaxRange,4);
               rMax:=GetNumber(7);
               IF rMax<=0.0 THEN RealInputWarn('r_max',rMax,3*L0,6);
               rMax:=rMax/L0; dr:=rMax/nSteps;
               rhoMax:=rMax; zMax:=rMax;
               PrhoMax:=rhoMax; PzMax:=zMax;
               drho:=rhoMax/(NrhoSteps-0.5); dz:=zMax/NzSteps;
               PrepRadialWF(Rnl,n,l,nSteps,-Z1*charge*Potl.dr);
               PrepPsiSlice(WF,Potl);
               Plotted:=False; Saved:=False;
               PrepUWFPlotScr(WF,Potl);
               PlotWF(WF,Potl,True);
            END;
         Done;
      END;
   {Dispose(ComputeScr); {Mac only}
END; {ComputeUndistorted}

PROCEDURE ComputeUWF(VAR FileDesc :FileDescType;
                     VAR WF :WFtype; VAR Potl :PotlType;
                     VAR MainMenu :TMenu; VAR MenuClicked :boolean);
{Compute and plot eigenfunction.}
VAR
   WFHelpScr    :HelpScrType;
   WFkeys       :THotKeys;
   Wkey         :byte;
   nn           :integer;
   WishToReturn :boolean;
BEGIN
   WFHelpScr[01]:='';
   WFHelpScr[02]:=' <F1>-show this help screen.';
   WFHelpScr[03]:='';
   WFHelpScr[04]:=' <F3>-toggle size of main plot.';
   WFHelpScr[05]:='    When small, a legend, text information,';
   WFHelpScr[06]:='    and a second plot are shown.';
   WFHelpScr[07]:='';
   WFHelpScr[08]:=' <F4>-change plot type, select different plane,';
   WFHelpScr[09]:='    or modify view of surface via rotation';
   WFHelpScr[10]:='    or change in size, perspective, or range.';
   WFHelpScr[11]:='';
   WFHelpScr[12]:=' <F5>-input new parameters.';
   WFHelpScr[13]:='';
   WFHelpScr[14]:=' <F10>-activate menu.';
   FOR nn:=15 TO 25 DO WFHelpScr[nn]:='';

   WITH WF DO
      BEGIN
         IF Computed=Undistorted THEN PlotWF(WF,Potl,True)
         ELSE
            BEGIN
               Zoom:=False; Plotted:=False;
               PrhoMax:=Potl.rhoMax; PzMax:=Potl.zMax;
               RecordToFile:=False;
            END;
         ComputeUndistorted(WF,Potl);
         IF Computed=Undistorted THEN
            BEGIN
               MainMenu.RowActivate(3,4,False);
               {New(WFkeys); {Mac only}
               WITH WFkeys DO
                  BEGIN
                     Init(5);
                     key[1]:='F1-Help';
                     IF Zoom THEN key[2]:='F3-Contract'
                     ELSE key[2]:='F3-Expand';
                     key[3]:='F4-ModView';
                     key[4]:='F5-Input';
                     key[5]:='F10-Menu';
                     Display;
                  END;
               WishToReturn:=False;
               WHILE (NOT WishToReturn) AND (NOT MenuClicked) DO
                  BEGIN
                     CheckForEvents;
                     IF Event.ReadKey=Escape THEN WishToReturn:=True
                     ELSE IF WFkeys.Pressed(Wkey) THEN
                        BEGIN
                           WFkeys.Clear;
                           case Wkey of
                              1 :Help(WFHelpScr);
                              2 :BEGIN
                                   Zoom:=not Zoom; Plotted:=False;
                                   PlotWF(WF,Potl,True);
                                   IF Zoom THEN
                                      WFkeys.key[2]:='F3-Contract'
                                   ELSE WFkeys.key[2]:='F3-Expand'
                                 END;
                              3 :ModViewWF(FileDesc,WF,Potl);
                              4 :ComputeUndistorted(WF,Potl);
                              5 :BEGIN
                                    WishToReturn:=True;
                                    IF MainMenu.Chosen THEN
                                       MenuClicked:=True;
                                 END;
                              END; {case}
                           WFkeys.Display;
                        END
                     ELSE IF MainMenu.Activated THEN MenuClicked:=True;
                  END;
               WFkeys.Clear;
               {Dispose(WFkeys); {Mac only}
               Plotted:=False;
            END;
      END;
END; {ComputeUWF}

{--------Routines for distorted eigenfunctions-------------------------}

PROCEDURE ComputeNextDistorted(VAR FileDesc :FileDescType;
                               VAR WF :WFtype; VAR Potl :PotlType);
VAR
   NDkeys      :THotKeys;
   Nkey        :byte;
   NDHelpScr   :HelpScrType;
   nn          :integer;
   rNumIter,
   rCwarn      :real;
   FileExists  :boolean;
BEGIN
   NDHelpScr[01]:='';
   NDHelpScr[02]:=' <F1>-show this help screen.   ';
   NDHelpScr[03]:='';
   NDHelpScr[04]:=' <F2>-pause in calculation.';
   NDHelpScr[05]:='    Can later resume.';
   FOR nn:=6 TO 25 DO NDHelpScr[nn]:='';
   {New(NDkeys); {Mac only}
   WITH NDkeys DO
      BEGIN
         Init(2);
         key[1]:='F1-Help';
         key[2]:='F2-Pause';
      END;
   WITH WF,Potl DO
      BEGIN
         ResumeStep:=False;
         CASE Mode OF
            Manual:BEGIN
                      ComputeViaChosenAlg(WF,Potl,NDkeys);
                      DipoleMoment(WF,Potl);
                      PrepDWFPlotScr(FileDesc,WF,Potl);
                      PlotWF(WF,Potl,True);
                   END;
            Auto :BEGIN
                     REPEAT
                        ComputeViaChosenAlg(WF,Potl,NDkeys);
                        DipoleMoment(WF,Potl);
                        IF NOT ResumeLanczos THEN
                           BEGIN
                              NautoRemain:=NautoRemain-1;
                              NstepsRemain:=NautoRemain;
                              Dipole.Put(NstepsRemain+1,DipoleVal);
                           END;
                        PrepDWFPlotScr(FileDesc,WF,Potl);
                        PlotWF(WF,Potl,True);
                        IF NOT ResumeLanczos THEN
                           BEGIN
                              IF RecordToFile THEN
                                 BEGIN
                                    rNumIter:=NumIter;
                                    IF Cwarn THEN rCwarn:=1.0
                                    ELSE rCwarn:=0.0;
                                    write(FileDesc.RecFile,
                                                En,Efield,Bfield,
                                                ExtCharge,Separation,
                                                rNumIter,rCwarn);
                                    Psi.WriteF(FileDesc.RecFile);
                                 END;
                              IF NautoRemain>0 THEN
                                 BEGIN
                                    Efield:=Efield+dEfield;
                                    Bfield:=Bfield+dBfield;
                                    NumIter:=0;
                                 END;
                              CheckForEvents;
                              IF Event.ReadKey=Escape THEN ResumeStep:=True
                              ELSE IF NDkeys.Pressed(Nkey) THEN
                                 CASE Nkey OF
                                    1 :Help(NDHelpScr);
                                    2 :ResumeStep:=True;
                                    END; {case}
                              IF NautoRemain<=0 THEN
                                 BEGIN
                                    IF RecordToFile THEN WITH FileDesc DO
                                       BEGIN
                                          Close(RecFile);
                                          ConnectTextFile(DataFile,
                                                         FileName,FileExists);
                                          IF FileExists THEN
                                             BEGIN
                                                Rewrite(DataFile);
                                                WriteFile(DataFile,WF,Potl);
                                             END
                                          ELSE Close(DataFile);
                                       END;
                                    ResumeStep:=False;
                                 END;
                              NDkeys.Clear;
                           END;
                     UNTIL ResumeLanczos OR ResumeStep OR (NautoRemain<=0);
                  END;
            END; {case}
         NewPotl:=False;
      END;
   {Dispose(NDkeys); {Mac only}
END; {ComputeNextDistorted}

PROCEDURE ComputeDistorted(VAR FileDesc :FileDescType;
                           VAR WF :WFtype; VAR Potl :PotlType;
                           VAR Eslider,Bslider :boolean);
VAR
   NzStepsOld,NrhoStepsOld  :integer; 
   WishToAbort,
   EnoughMemory,Enough      :boolean;
   ComputeScr               :TInputScreen;
BEGIN
   {New(ComputeScr); {Mac only}
   WITH ComputeScr DO
      REPEAT
         EnoughMemory:=True;
         Init;
         DefineInputPort(0.1,0.9,0.0,0.9);
         LoadLine( '              Distorted eigenfunctions');
         LoadLine('');
         LoadLine(
            ConCat(' Mode: #1 manual',
                   '  #1 auto ({   }steps; #F Record to file?)'));
         LoadLine('');
         LoadLine(
            ' E-field (TV/m): {      } (step from {      } to {      })');
         LoadLine(
            ' B-field (MT):   {      } (step from {      } to {      })');
         LoadLine('');
         LoadLine( ' Magnetic quantum number (m): {   }');
         LoadLine( ' Nuclear mass: {      }GeV/c^2  and charge: {   }e');
         LoadLine('');
         LoadLine( ' Algorithm:    #2 basis functions, with q numbers:');
         LoadLine( '                    n: |m|+1 to {   }, l: |m| to {   }');
         LoadLine( '                    Energy level to compute: {   }');
         LoadLine( '               #2 Lanczos (lowest energy only)');
         LoadLine( '                    Limit on iterations: {    }');
         LoadLine(
            ConCat('                    Convergence tolerance: {      }',
                                                         Potl.Eunit,'eV'));
         LoadLine('');
         LoadLine(
            ConCat(' Grid size:  rho_max: {      }',Potl.Lunit,'m  ',
                                'z_max: {      }',Potl.Lunit,'m'));
         LoadLine( '      spacings:  rho: {   }    z: {   }');
         LoadLine('');
         LoadLine( '      [  OK  ]       [Cancel]');
         CASE WF.Mode OF
            Manual :SetRadioButton('1',1);
            Auto   :SetRadioButton('1',2);
            END; {case}
         SetNumber(3,Potl.NautoSteps);
         SetNumber(5,Potl.Efield);
         SetNumber(6,Potl.MinEfield);
         SetNumber(7,Potl.MaxEfield);
         SetNumber(8,Potl.Bfield);
         SetNumber(9,Potl.MinBfield);
         SetNumber(10,Potl.MaxBfield);
         SetNumber(11,WF.m);
         SetNumber(12,Potl.m1);
         SetNumber(13,Potl.Z1);
         CASE WF.Algorithm OF
            Basis   :SetRadioButton('2',1);
            Lanczos :SetRadioButton('2',2);
            END; {case}
         SetNumber(15,WF.nMax);
         SetNumber(16,WF.lMax);
         SetNumber(17,WF.Level);
         SetNumber(19,WF.MaxNumIter);
         SetNumber(20,Potl.V0*WF.tolerance);
         SetNumber(21,Potl.L0*Potl.rhoMax);
         SetNumber(22,Potl.L0*Potl.zMax);
         SetNumber(23,Potl.NrhoSteps);
         SetNumber(24,Potl.NzSteps);
         AcceptScreen;
         IF NOT Canceled THEN WITH WF,Potl DO
            BEGIN
               Computed:=Distorted;
               RecordVeff:=False;
               WishToAbort:=False;
               Efield:=GetNumber(5);
               MinEfield:=GetNumber(6);
               MaxEfield:=GetNumber(7);
               Bfield:=GetNumber(8);
               MinBfield:=GetNumber(9);
               MaxBfield:=GetNumber(10);
               m:=round(GetNumber(11));
               m1:=GetNumber(12);
               IF m1<=0.0 THEN RealInputWarn('m_1',m1,0.938,6);
               Z1:=GetNumber(13);
               IF Z1<=0.0 THEN RealInputWarn('charge',Z1,1.0,6);
               mu:=ReducedMass(Mass,m1,Munit);
               InitializeScales(Potl);
               ComputeZeta(Potl);
               rhoMax:=GetNumber(21);
               IF (rhoMax<=0.0) THEN
                  RealInputWarn('rho_max',rhoMax,10.0,6);
               zMax:=GetNumber(22);
               IF (zMax<=0.0) THEN
                  RealInputWarn('z_max',zMax,10.0,6);
               rhoMax:=rhoMax/L0; zMax:=zMax/L0;
               PrhoMax:=rhoMax; PzMax:=zMax;
               NrhoStepsOld:=NrhoSteps;
               NrhoSteps:=round(GetNumber(23));
               IF NrhoSteps<1 THEN
                  IntInputWarn('no. of rho grid steps',
                               NrhoSteps,NrhoStepsOld,3);
               NzStepsOld:=NzSteps;
               NzSteps:=round(GetNumber(24));
               IF NzSteps<1 THEN
                  IntInputWarn('no. of z grid steps',NzSteps,NzStepsOld,3);
               drho:=rhoMax/(NrhoSteps-0.5); dz:=zMax/NzSteps;
               CASE GetRadioButton('2') OF
                  1 :Algorithm:=Basis;
                  2 :Algorithm:=Lanczos;
                  END; {case}
               PrepStorage(WF,Potl,EnoughMemory);
               IF EnoughMemory THEN
                  BEGIN
                     CASE GetRadioButton('1') OF
                        1 :BEGIN
                              Mode:=Manual;
                              IF Efield<MinEfield THEN MinEfield:=Efield
                              ELSE IF Efield>MaxEfield THEN MaxEfield:=Efield;
                              IF MinEfield>=MaxEfield then Eslider:=False
                              ELSE Eslider:=True;
                              IF Bfield<MinBfield THEN MinBfield:=Bfield
                              ELSE IF Bfield>MaxBfield THEN MaxBfield:=Bfield;
                              IF MinBfield>=MaxBfield then Bslider:=False
                              ELSE Bslider:=True;
                           END;
                        2 :BEGIN
                              Mode:=Auto;
                              Eslider:=False; Bslider:=False;
                              RecordToFile:=GetBoolean(4);
                              IF RecordToFile THEN
                                 BEGIN
                                    Computed:=NotDone;
                                    SaveAs(FileDesc,WF,Potl);
                                    Computed:=Distorted;
                                    RewriteRecFile(FileDesc,WishToAbort);
                                    IF WishToAbort THEN RecordToFile:=False;
                                 END;
                              NautoSteps:=round(GetNumber(3));
                              NautoRemain:=NautoSteps+1;
                              NstepsRemain:=NautoRemain;
                              dEfield:=(MaxEfield-MinEfield)/NautoSteps;
                              dBfield:=(MaxBfield-MinBfield)/NautoSteps;
                              Efield:=MinEfield; Bfield:=MinBfield;
                              ResumeStep:=True;
                              Dipole.Free; Dipole.Init(NautoSteps+1);
                              DipoleMin:=-10*pz0; DipoleMax:=10*pz0;
                              IF abs(dEfield)<=MachEps THEN
                                 BEGIN
                                    dField:=dBField; FieldLabel:='B (MT)';
                                    FieldMin:=MinBField; FieldMax:=MaxBField;
                                 END
                              ELSE
                                 BEGIN
                                    dField:=dEField; FieldLabel:='E (TV/m)';
                                    FieldMin:=MinEField; FieldMax:=MaxEField;
                                 END;
                           END;
                        END; {case '1'}
                     CASE Algorithm OF
                        Basis :BEGIN
                                  lMax:=round(GetNumber(16));
                                  IF lMax<abs(m) THEN
                                     IntInputWarn('l',l,abs(m)+1,2);
                                  nMax:=round(GetNumber(15));
                                  IF nMax<1 THEN
                                     IntInputWarn('n_max',nMax,1,2);
                                  Level:=round(GetNumber(17));
                                  IF Level<1 THEN
                                     IntInputWarn('level number',Level,1,4);
                               END;
                        Lanczos :BEGIN
                                    MaxNumIter:=round(GetNumber(19));
                                    IF MaxNumIter<1 THEN
                                       IntInputWarn('limit on iterations',
                                                    MaxNumIter,1,4)
                                    ELSE IF sqr(MaxNumIter)>MaxRange THEN
                                       IntInputWarn('limit on iterations',
                                          MaxNumIter,trunc(sqrt(MaxRange)),4);
                                    tolerance:=GetNumber(20)/V0;
                                    InitialGuess(WF,Potl);
                                 END;
                        END;  {case Algorithm}
                     IF NOT WishToAbort THEN
                        BEGIN
                           IF (Mode=Auto) AND (NOT WF.Zoom) THEN
                              BEGIN
                                 SelectViewPort(4);
                                 ClearViewPort;
                              END;
                           ComputeNextDistorted(FileDesc,WF,Potl);
                        END;
                  END
               ELSE
                  BEGIN
                     Announce(ConCat('Memory capacity insufficient for ',
                                     'requested array size.'));
                     Computed:=NotDone;
                     NrhoSteps:=NrhoStepsOld; NzSteps:=NzStepsOld;
                     drho:=rhoMax/(NrhoSteps-0.5); dz:=zMax/NzSteps;
                     PrepStorage(WF,Potl,Enough);
                  END;
            END;
         Done;
      UNTIL EnoughMemory;
   {Dispose(ComputeScr); {Mac only}
END; {ComputeDistorted}

PROCEDURE ShowDWFSliders(VAR WF :WFtype; VAR Potl :PotlType;
                         VAR Sliders :TSliders; Eslider,Bslider :boolean);
VAR
   sldr :integer;
BEGIN
   SelectViewPort(4);
   Sliders.Init;
   ClearViewPort;
   sldr:=1;
   WITH Sliders,Potl DO
      BEGIN
         IF Eslider THEN
            BEGIN
               Create(sldr,MinEfield,MaxEfield,Efield,0.33,0.3  ,0.63,3,
                      Num2Str(MinEfield,6),Num2Str(MaxEfield,6),
                      'E (TV/m)',False);
               sldr:=sldr+1;
            END;
         IF Bslider THEN
            BEGIN
               Create(sldr,MinBfield,MaxBfield,Bfield,0.33,0.225,0.63,3,
                      Num2Str(MinBfield,6),Num2Str(MaxBfield,6),
                      'B (MT)',False);
               sldr:=sldr+1;
            END;
         DrawAll;
      END;
END; {ShowDWFSliders}

PROCEDURE ComputeDWF(VAR FileDesc :FileDescType;
                     VAR WF :WFtype; VAR Potl :PotlType;
                     VAR MainMenu :TMenu; VAR MenuClicked :boolean);
{Compute and plot eigenfunction distorted by fields.}
VAR
   WFHelpScr       :HelpScrType;
   WFkeys          :THotKeys;
   Wkey            :byte;
   WishToReturn,
   Eslider,Bslider :boolean;
   Sliders         :TSliders;
BEGIN
   WFHelpScr[01]:='';
   WFHelpScr[02]:=' <F1>-show this help screen.';
   WFHelpScr[03]:='';
   WFHelpScr[04]:=' <F2>-run with new slider position,';
   WFHelpScr[05]:='    or resume iterations or auto step,';
   WFHelpScr[06]:='    or select new level from basis-function';
   WFHelpScr[07]:='    diagonalization.';
   WFHelpScr[08]:='';
   WFHelpScr[09]:=' <F3>-toggle size of main plot.';
   WFHelpScr[10]:='    When small, a legend, text information,';
   WFHelpScr[11]:='    and sliders or a second plot are shown.';
   WFHelpScr[12]:='';
   WFHelpScr[13]:=' <F4>-change plot type, select different plane,';
   WFHelpScr[14]:='    or modify view of surface via rotation';
   WFHelpScr[15]:='    or change in size, perspective, or range.';
   WFHelpScr[16]:='';
   WFHelpScr[17]:=' <F5>-input new parameters.';
   WFHelpScr[18]:='    If select manual mode and a positive range,';
   WFHelpScr[19]:='    sliders will be made available to adjust';
   WFHelpScr[20]:='    field strengths.  If select auto mode,';
   WFHelpScr[21]:='    field strengths will be automatically varied';
   WFHelpScr[22]:='    within the selected range.';
   WFHelpScr[23]:='';
   WFHelpScr[24]:=' <F10>-activate menu.';
   WFHelpScr[25]:='';

   WITH WF DO
      BEGIN
         IF Computed=Distorted THEN PlotWF(WF,Potl,True)
         ELSE
            BEGIN
               Zoom:=False; Plotted:=False;
               PrhoMax:=Potl.rhoMax; PzMax:=Potl.zMax;
            END;
         ComputeDistorted(FileDesc,WF,Potl,Eslider,Bslider);
         IF Computed=Distorted THEN
            BEGIN
               MainMenu.RowActivate(3,4,False);
               {New(WFkeys); {Mac only}
               WITH WFkeys DO
                  BEGIN
                     Init(6);
                     key[1]:='F1-Help';
                     key[2]:='F2-Run';
                     IF Zoom THEN key[3]:='F3-Contract'
                     ELSE key[3]:='F3-Expand';
                     key[4]:='F4-ModView';
                     key[5]:='F5-Input';
                     key[6]:='F10-Menu';
                     Display;
                  END;
               {New(Sliders); {Mac only}
               IF (Mode=Manual) AND (NOT Zoom) THEN
                  ShowDWFSliders(WF,Potl,Sliders,Eslider,Bslider);
               WishToReturn:=False;
               WHILE (NOT WishToReturn) AND (NOT MenuClicked) DO
                  BEGIN
                     CheckForEvents;
                     IF Event.ReadKey=Escape THEN WishToReturn:=True
                     ELSE IF (Mode=Manual) AND Sliders.Changed THEN
                        BEGIN
                           IF Eslider THEN
                              BEGIN
                                 Potl.Efield:=Sliders.Value(1);
                                 IF Bslider THEN
                                    Potl.Bfield:=Sliders.Value(2);
                              END
                           ELSE Potl.Bfield:=Sliders.Value(1);
                           Potl.NewPotl:=True;
                           WF.NumIter:=0;
                        END
                     ELSE IF WFkeys.Pressed(Wkey) THEN
                        BEGIN
                           WFkeys.Clear;
                           IF (Mode=Manual) AND (NOT Zoom) THEN Sliders.Done;
                           case Wkey of
                              1 :Help(WFHelpScr);
                              2 :if Potl.NewPotl OR WF.ResumeLanczos
                                    OR WF.ResumeStep THEN
                                       ComputeNextDistorted(FileDesc,WF,Potl)
                                 ELSE
                                    ComputeDistorted(FileDesc,WF,Potl,
                                                     Eslider,Bslider);
                              3 :BEGIN
                                   Zoom:=not Zoom; Plotted:=False;
                                   PlotWF(WF,Potl,True);
                                   IF Zoom THEN WFkeys.key[3]:='F3-Contract'
                                   ELSE WFkeys.key[3]:='F3-Expand'
                                 END;
                              4 :ModViewWF(FileDesc,WF,Potl);
                              5 :ComputeDistorted(FileDesc,WF,Potl,
                                                  Eslider,Bslider);
                              6 :BEGIN
                                    WishToReturn:=True;
                                    IF MainMenu.Chosen THEN
                                       MenuClicked:=True;
                                 END;
                              END; {case}
                           WFkeys.Display;
                           IF (Mode=Manual) AND (NOT Zoom) THEN
                              ShowDWFSliders(WF,Potl,Sliders,Eslider,Bslider);
                        END
                     ELSE IF MainMenu.Activated THEN MenuClicked:=True;
                  END;
               WFkeys.Clear;
               CloseViewPort(4);
               IF Mode=Manual THEN Sliders.Done
               ELSE IF RecordToFile AND ResumeStep THEN
                  Close(FileDesc.RecFile);
               {Dispose(Sliders); Dispose(WFkeys); {Mac only}
               Plotted:=False;
            END;
      END;
END; {ComputeDWF}

{--------Routines for shared eigenfunctions----------------------------}

PROCEDURE ComputeNextShared(VAR FileDesc :FileDescType;
                            VAR WF :WFtype; VAR Potl :PotlType);
VAR
   NSkeys      :THotKeys;
   Nkey        :byte;
   NSHelpScr   :HelpScrType;
   nn          :integer;
   rNumIter,
   rCwarn      :real;
   FileExists  :boolean;
BEGIN
   NSHelpScr[01]:='';
   NSHelpScr[02]:=' <F1>-show this help screen.   ';
   NSHelpScr[03]:='';
   NSHelpScr[04]:=' <F2>-pause in calculation.';
   NSHelpScr[05]:='    Can later resume.';
   FOR nn:=6 TO 25 DO NSHelpScr[nn]:='';
   {New(NSkeys); {Mac only}
   WITH NSkeys DO
      BEGIN
         Init(2);
         key[1]:='F1-Help';
         key[2]:='F2-Pause';
      END;
   WITH WF,Potl DO
      BEGIN
         ResumeStep:=False;
         CASE Mode OF
            Manual :BEGIN
                       ComputeViaChosenAlg(WF,Potl,NSkeys);
                       PrepSWFPlotScr(FileDesc,WF,Potl);
                       PlotWF(WF,Potl,True);
                    END;
            Auto   :BEGIN
                       REPEAT
                          ComputeViaChosenAlg(WF,Potl,NSkeys);
                          IF NOT ResumeLanczos THEN
                             BEGIN
                                NautoRemain:=NautoRemain-1;
                                IF RecordVeff THEN
                                   BEGIN
                                      NstepsRemain:=NautoRemain;
                                      Veff.Put(NstepsRemain+1,
                                           En+2*ExtCharge/Separation-EnAsymp);
                                   end;
                             END;
                          PrepSWFPlotScr(FileDesc,WF,Potl);
                          PlotWF(WF,Potl,True);
                          IF NOT ResumeLanczos THEN
                             BEGIN
                                IF RecordToFile THEN
                                   BEGIN
                                      rNumIter:=NumIter;
                                      IF Cwarn THEN rCwarn:=1.0
                                      ELSE rCwarn:=0.0;
                                      write(FileDesc.RecFile,En,Efield,Bfield,
                                            ExtCharge,Separation,
                                            rNumIter,rCwarn);
                                      Psi.WriteF(FileDesc.RecFile);
                                   end;
                                IF NautoRemain>0 THEN
                                   BEGIN
                                      ExtCharge:=ExtCharge+dExtCh;
                                      Separation:=Separation+dSep;
                                      NumIter:=0;
                                   end;
                                CheckForEvents;
                                IF Event.ReadKey=Escape THEN ResumeStep:=True
                                ELSE IF NSkeys.Pressed(Nkey) THEN
                                CASE Nkey OF
                                   1 :Help(NSHelpScr);
                                   2 :ResumeStep:=True;
                                   END; {case}
                                IF NautoRemain<=0 THEN
                                   BEGIN
                                      IF RecordToFile THEN WITH FileDesc DO
                                         BEGIN
                                            Close(RecFile);
                                            ConnectTextFile(DataFile,
                                                         FileName,FileExists);
                                            IF FileExists THEN
                                               BEGIN
                                                  Rewrite(DataFile);
                                                  WriteFile(DataFile,WF,Potl);
                                               END
                                            ELSE Close(DataFile);
                                         END;
                                      ResumeStep:=False;
                                   end;
                                NSkeys.Clear;
                             END;
                       UNTIL ResumeLanczos OR ResumeStep OR (NautoRemain<=0);
                    END;
            END; {case}
         NewPotl:=False;
      END;
   {Dispose(NSkeys); {Mac only}
END; {ComputeNextShared}

PROCEDURE ComputeShared(VAR FileDesc :FileDescType;
                        VAR WF :WFtype; VAR Potl :PotlType;
                        VAR Cslider,Sslider :boolean);
VAR 
   NzStepsOld,NrhoStepsOld  :integer;
   WishToAbort,
   EnoughMemory,Enough      :boolean;
   ComputeScr               :TInputScreen;
BEGIN
   {New(ComputeScr); {Mac only}
   WITH ComputeScr DO
      REPEAT
         EnoughMemory:=True;
         Init;
         DefineInputPort(0.1,0.9,0.05,0.9);
         LoadLine( '              Shared eigenfunctions');
         LoadLine('');
         LoadLine(
            ConCat(' Mode: #1 manual',
                   '  #1 auto ({   }steps; #F Record to file?'));
         LoadLine(
            ConCat('                ',
                   '            #F Record effective potential?)'));
         LoadLine('');
         LoadLine(' First charge (e):  {      }');
         LoadLine(
            ' Second charge (e): {      } (step from {      } to {      })');
         LoadLine(
            ConCat(' Distance (',Potl.Lunit,
                   'm):     {      } (step from {      } to {      })'));
         LoadLine('');
         LoadLine( ' Magnetic quantum number (m): {   }');
         LoadLine('');
         LoadLine( ' Lanczos algorithm:');
         LoadLine( '     Limit on iterations: {    }');
         LoadLine(
            ConCat('     Convergence tolerance: {      }',Potl.Eunit,'eV'));
         LoadLine('');
         LoadLine(
            ConCat(' Grid size:  rho_max: {      }',Potl.Lunit,'m  ',
                                'z_max: {      }',Potl.Lunit,'m'));
         LoadLine( '      spacings:  rho: {   }    z: {   }');
         LoadLine('');
         LoadLine( '      [  OK  ]       [Cancel]');
         CASE WF.Mode OF
            Manual :SetRadioButton('1',1);
            Auto   :SetRadioButton('1',2);
            END; {case}
         SetNumber(3,Potl.NautoSteps);
         SetBoolean(5,WF.RecordVeff);
         SetNumber(6,Potl.Z1);
         SetNumber(7,Potl.ExtCharge);
         SetNumber(8,Potl.MinExtCh);
         SetNumber(9,Potl.MaxExtCh);
         SetNumber(10,Potl.L0*Potl.Separation);
         SetNumber(11,Potl.L0*Potl.MinSep);
         SetNumber(12,Potl.L0*Potl.MaxSep);
         SetNumber(13,WF.m);
         SetNumber(14,WF.MaxNumIter);
         SetNumber(15,Potl.V0*WF.tolerance);
         SetNumber(16,Potl.L0*Potl.rhoMax);
         SetNumber(17,Potl.L0*Potl.zMax);
         SetNumber(18,Potl.NrhoSteps);
         SetNumber(19,Potl.NzSteps);
         AcceptScreen;
         IF NOT Canceled THEN WITH WF,Potl DO
            BEGIN
               Computed:=Shared;
               WishToAbort:=False;
               Z1:=GetNumber(6);
               IF Z1<=0.0 THEN RealInputWarn('first charge',Z1,1.0,6);
               mu:=Mass;
               InitializeScales(Potl);
               ComputeZeta(Potl);
               ExtCharge:=GetNumber(7);
               MinExtCh:=GetNumber(8);
               MaxExtCh:=GetNumber(9);
               Separation:=GetNumber(10);
               IF Separation<0.0 THEN
                  RealInputWarn('distance',Separation,0.0,6);
               Separation:=Separation/L0;
               MinSep:=GetNumber(11);
               IF MinSep<0.0 THEN
                  RealInputWarn('min distance',MinSep,0.0,6);
               MinSep:=MinSep/L0;
               MaxSep:=GetNumber(12);
               IF MaxSep<=0.0 THEN
                  RealInputWarn('max distance',MaxSep,L0*zMax,6);
               MaxSep:=MaxSep/L0;
               m:=round(GetNumber(13));
               rhoMax:=GetNumber(16);
               IF (rhoMax<=0.0) THEN
                  RealInputWarn('rho_max',rhoMax,10.0,6);
               zMax:=GetNumber(17);
               IF (zMax<=0.0) THEN
                  RealInputWarn('z_max',zMax,10.0,6);
               rhoMax:=rhoMax/L0; zMax:=zMax/L0;
               PrhoMax:=rhoMax; PzMax:=zMax;
               NrhoStepsOld:=NrhoSteps;
               NrhoSteps:=round(GetNumber(18));
               IF NrhoSteps<1 THEN
                  IntInputWarn('no. of rho grid steps',
                               NrhoSteps,NrhoStepsOld,3);
               NzStepsOld:=NzSteps;
               NzSteps:=round(GetNumber(19));
               IF NzSteps<1 THEN
                  IntInputWarn('no. of z grid steps',NzSteps,NzStepsOld,3);
               drho:=rhoMax/(NrhoSteps-0.5); dz:=zMax/NzSteps;
               Algorithm:=Lanczos;
               PrepStorage(WF,Potl,EnoughMemory);
               IF EnoughMemory THEN
                  BEGIN
                     CASE GetRadioButton('1') OF
                        1 :BEGIN
                              Mode:=Manual;
                              IF ExtCharge<MinExtCh THEN MinExtCh:=ExtCharge
                              ELSE IF ExtCharge>MaxExtCh THEN
                                 MaxExtCh:=ExtCharge;
                              IF MinExtCh>=MaxExtCh THEN Cslider:=False
                              ELSE Cslider:=True;
                              IF Separation<MinSep THEN MinSep:=Separation
                              ELSE IF Separation>MaxSep THEN
                                 MaxSep:=Separation;
                              IF MinSep>=MaxSep THEN Sslider:=False
                              ELSE Sslider:=True;
                           END;
                        2 :BEGIN
                              Mode:=Auto;
                              Cslider:=False; Sslider:=False;
                              NautoSteps:=round(GetNumber(3));
                              IF NautoSteps>MaxRange THEN
                                 IntInputWarn('number of auto steps',
                                              NautoSteps,MaxRange,4);
                              RecordToFile:=GetBoolean(4);
                              IF RecordToFile THEN
                                 BEGIN
                                    Computed:=NotDone;
                                    SaveAs(FileDesc,WF,Potl);
                                    Computed:=Shared;
                                    RewriteRecFile(FileDesc,WishToAbort);
                                    IF WishToAbort THEN RecordToFile:=False;
                                 END;
                              RecordVeff:=GetBoolean(5);
                              IF RecordVeff THEN
                                 BEGIN
                                    Veff.Free; Veff.Init(NautoSteps);
                                    m1:=round(abs(Z1))*0.938;
                                    m2:=round(abs(ExtCharge/Z1))*m1;
                                    dExtCh:=0.0;
                                    dSep:=-MaxSep/NautoSteps;
                                    Separation:=MaxSep;
                                    IF ExtCharge>Z1 THEN
                                       EnAsymp:=-sqr(ExtCharge*charge)
                                    ELSE EnAsymp:=-sqr(Z1*charge);
                                    NautoRemain:=NautoSteps;
                                    IF Z1*ExtCharge>=0.0 THEN
                                       BEGIN
                                          VeffMin:=V0*EnAsymp;
                                          VeffMax:=V0*2*Z1*ExtCharge/(-dSep);
                                       END
                                    ELSE
                                       BEGIN
                                          VeffMin:=V0*2*Z1*ExtCharge/(-dSep);
                                          VeffMax:=0.0;
                                       END;
                                 END
                              ELSE
                                 BEGIN
                                    dExtCh:=(MaxExtCh-MinExtCh)/NautoSteps;
                                    dSep:=(MaxSep-MinSep)/NautoSteps;
                                    ExtCharge:=MinExtCh; Separation:=MinSep;
                                    NautoRemain:=NautoSteps+1;
                                 END;
                              ResumeStep:=True;
                              NstepsRemain:=NautoRemain;
                           END;
                        END; {case '1'}
                     MaxNumIter:=round(GetNumber(14));
                     IF MaxNumIter<1 THEN
                        IntInputWarn('limit on iterations',MaxNumIter,1,4)
                     ELSE IF sqr(MaxNumIter)>MaxRange THEN
                        IntInputWarn('limit on iterations',MaxNumIter,
                                     trunc(sqrt(MaxRange)),4);
                     tolerance:=GetNumber(15)/V0;
                     InitialGuess(WF,Potl);
                     IF NOT WishToAbort THEN
                        BEGIN
                           IF (Mode=Auto) AND (NOT WF.Zoom) THEN
                              BEGIN
                                 SelectViewPort(4);
                                 ClearViewPort;
                              END;
                           ComputeNextShared(FileDesc,WF,Potl);
                        END;
                  END
               ELSE
                  BEGIN
                     Announce(ConCat('Memory capacity insufficient for ',
                                     'requested array size.'));
                     Computed:=NotDone;
                     NrhoSteps:=NrhoStepsOld; NzSteps:=NzStepsOld;
                     drho:=rhoMax/(NrhoSteps-0.5); dz:=zMax/NzSteps;
                     PrepStorage(WF,Potl,Enough);
                  END;
            END;
         Done;
      UNTIL EnoughMemory;
   {Dispose(ComputeScr); {Mac only}
END; {ComputeShared}
 
PROCEDURE ShowSWFSliders(VAR WF :WFtype; VAR Potl :PotlType;
                         VAR Sliders :TSliders; Cslider,Sslider :boolean);
VAR
   sldr :integer;
BEGIN
   SelectViewPort(4);
   Sliders.Init;
   ClearViewPort;
   sldr:=1;
   WITH Sliders,Potl DO
      BEGIN
         IF Cslider THEN
            BEGIN
               Create(sldr,MinExtCh,MaxExtCh,ExtCharge,0.33,0.3  ,0.63,3,
                      Num2Str(MinExtCh,6),Num2Str(MaxExtCh,6),
                      '2nd charge',False);
               sldr:=sldr+1;
            END;
         IF Sslider THEN
            BEGIN
               Create(sldr,L0*MinSep,L0*MaxSep,L0*Separation,
                      0.33,0.225,0.63,3,
                      Num2Str(L0*MinSep,6),Num2Str(L0*MaxSep,6),
                      ConCat('dist. (',Lunit,'m)'),False);
               sldr:=sldr+1;
            END;
         DrawAll;
      END;
END; {ShowSWFSliders}

PROCEDURE ComputeSWF(VAR FileDesc :FileDescType;
                     VAR WF :WFtype; VAR Potl :PotlType;
                     VAR MainMenu :TMenu; VAR MenuClicked :boolean);
{Compute and plot eigenfunction distorted by a second charge.}
VAR
   WFHelpScr       :HelpScrType;
   WFkeys          :THotKeys;
   Wkey            :byte;
   WishToReturn,
   Cslider,Sslider :boolean;
   Sliders         :TSliders;
BEGIN
   WFHelpScr[01]:='';
   WFHelpScr[02]:=' <F1>-show this help screen.';
   WFHelpScr[03]:='';
   WFHelpScr[04]:=' <F2>-run with new slider position,';
   WFHelpScr[05]:='    or resume iterations or auto step.';
   WFHelpScr[06]:='';
   WFHelpScr[07]:=' <F3>-toggle size of main plot.';
   WFHelpScr[08]:='    When small, a legend, text information,';
   WFHelpScr[09]:='    and sliders or a second plot are shown.';
   WFHelpScr[10]:='';
   WFHelpScr[11]:=' <F4>-change plot type, select different plane,';
   WFHelpScr[12]:='    or modify view of surface via rotation';
   WFHelpScr[13]:='    or change in size, perspective, or range.';
   WFHelpScr[14]:='';
   WFHelpScr[15]:=' <F5>-input new parameters.';
   WFHelpScr[16]:='    If select manual mode and a positive range,';
   WFHelpScr[17]:='    sliders will be made available to adjust';
   WFHelpScr[18]:='    the second charge and the distance between';
   WFHelpScr[19]:='    charges.  If select auto mode, charge and';
   WFHelpScr[20]:='    distance will be automatically varied within';
   WFHelpScr[21]:='    the selected range.';
   WFHelpScr[22]:='';
   WFHelpScr[23]:=' <F10>-activate menu.';
   WFHelpScr[24]:='';
   WFHelpScr[25]:='';

   WITH WF DO
      BEGIN
         IF Computed=Shared THEN PlotWF(WF,Potl,True)
         ELSE
            BEGIN
               Zoom:=False; Plotted:=False;
               PrhoMax:=Potl.rhoMax; PzMax:=Potl.zMax;
            END;
         ComputeShared(FileDesc,WF,Potl,Cslider,Sslider);
         IF Computed=Shared THEN
            BEGIN
               MainMenu.RowActivate(3,4,RecordVeff);
               {New(WFkeys); {Mac only}
               WITH WFkeys DO
                  BEGIN
                     Init(6);
                     key[1]:='F1-Help';
                     key[2]:='F2-Run';
                     IF Zoom THEN key[3]:='F3-Contract'
                     ELSE key[3]:='F3-Expand';
                     key[4]:='F4-ModView';
                     key[5]:='F5-Input';
                     key[6]:='F10-Menu';
                     Display;
                  END;
               {New(Sliders); {Mac only}
               IF (Mode=Manual) AND (NOT Zoom) THEN
                  ShowSWFSliders(WF,Potl,Sliders,Cslider,Sslider);
               WishToReturn:=False;
               WHILE (NOT WishToReturn) AND (NOT MenuClicked) DO
                  BEGIN
                     CheckForEvents;
                     IF Event.ReadKey=Escape THEN WishToReturn:=True
                     ELSE IF (Mode=Manual) AND Sliders.Changed THEN
                        BEGIN
                           IF Cslider THEN
                              BEGIN
                                 Potl.ExtCharge:=Sliders.Value(1);
                                 IF Sslider THEN
                                    Potl.Separation:=Sliders.Value(2)/Potl.L0;
                              END
                           ELSE Potl.Separation:=Sliders.Value(1)/Potl.L0;
                           Potl.NewPotl:=True;
                           WF.NumIter:=0;
                        END
                     ELSE IF WFkeys.Pressed(Wkey) THEN
                        BEGIN
                           WFkeys.Clear;
                           IF (Mode=Manual) AND (NOT Zoom) THEN Sliders.Done;
                           case Wkey of
                              1 :Help(WFHelpScr);
                              2 :if Potl.NewPotl OR WF.ResumeLanczos
                                    OR WF.ResumeStep THEN
                                       ComputeNextShared(FileDesc,WF,Potl)
                                 ELSE
                                    ComputeShared(FileDesc,WF,Potl,
                                                  Cslider,Sslider);
                              3 :BEGIN
                                   Zoom:=not Zoom; Plotted:=False;
                                   PlotWF(WF,Potl,True);
                                   IF Zoom THEN WFkeys.key[3]:='F3-Contract'
                                   ELSE WFkeys.key[3]:='F3-Expand'
                                 END;
                              4 :ModViewWF(FileDesc,WF,Potl);
                              5 :ComputeShared(FileDesc,WF,Potl,
                                                  Cslider,Sslider);
                              6 :BEGIN
                                    WishToReturn:=True;
                                    IF MainMenu.Chosen THEN
                                       MenuClicked:=True;
                                 END;
                              END; {case}
                           WFkeys.Display;
                           IF (Mode=Manual) AND (NOT Zoom) THEN
                              ShowSWFSliders(WF,Potl,Sliders,Cslider,Sslider);
                           MainMenu.RowActivate(3,4,RecordVeff);
                        END
                     ELSE IF MainMenu.Activated THEN MenuClicked:=True;
                  END;
               WFkeys.Clear;
               CloseViewPort(4);
               IF Mode=Manual THEN Sliders.Done
               ELSE IF RecordToFile AND ResumeStep THEN
                  Close(FileDesc.RecFile);
               {Dispose(Sliders); Dispose(WFkeys); {Mac only}
               Plotted:=False;
            END;
      END;
END; {ComputeSWF}

{--------Routines for the binding eigenfunction------------------------}

PROCEDURE ComputeBinding(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   ComputeScr       :TInputScreen;
   z                :DMatrix;
   Diag,OffDiag     :DVector;
   SpaceAvail       :longint;
   nLoc,i,iMin,iMax :integer;
   RnlMin,RnlMax,
   rMaxMax,rMaxDef,
   zetaNucl         :real;
BEGIN
   {New(ComputeScr); {Mac only}
   WITH ComputeScr DO
      BEGIN
         Init;
         DefineInputPort(0.2,0.8,0.225,0.775);
         LoadLine('');
         LoadLine( '        Molecular binding eigenfunctions');
         LoadLine('');
         LoadLine( '     Quantum numbers:');
         LoadLine('             n_r: {   }  l: {   }  m: {   }');
         LoadLine('');
         LoadLine( '     Nuclear masses (GeV/c^2): ');
         LoadLine( '          m_1: {      }       m_2: {      }');
         LoadLine('');
         LoadLine(
            ConCat('     Range of calculation (r_max): {      }',
                                                            Potl.Lunit,'m'));
         LoadLine('');
         LoadLine('       [  OK  ]      [Cancel]');
         SetNumber(1,WF.nr);
         SetNumber(2,WF.l);
         SetNumber(3,WF.m);
         SetNumber(4,Potl.m1);
         SetNumber(5,Potl.m2);
         SetNumber(6,Potl.L0*Potl.rMax);
         AcceptScreen;
         IF NOT Canceled THEN WITH WF,Potl DO
            BEGIN
               Computed:=Binding;
               nr:=round(GetNumber(1));
               IF nr<0 THEN IntInputWarn('n_r',nr,0,2)
               ELSE IF nr>NautoSteps THEN IntInputWarn('n_r',nr,NautoSteps,3);
               l:=round(GetNumber(2));
               IF l<0 THEN IntInputWarn('l',l,0,2);
               n:=nr+l+1;
               m:=round(GetNumber(3));
               IF (-l>m) OR (l<m) THEN IntInputWarn('m',m,0,2);
               m1:=GetNumber(4);
               IF m1<=0.0 THEN RealInputWarn('m_1',m1,0.938,6);
               m2:=GetNumber(5);
               IF m2<=0.0 THEN RealInputWarn('m_2',m2,0.938*abs(Extcharge),6);
               rMax:=GetNumber(6);
               SpaceAvail:=MaxAvail;
               IF SpaceAvail>65521 THEN SpaceAvail:=65521;
               rMaxMax:=-(sqrt(SpaceAvail/SizeOf(real))-1)*L0*dSep;
               rMaxDef:=2*L0*MaxSep;
               IF rMaxDef>rMaxMax THEN rMaxDef:=rMaxMax;
               IF rMax<=0.0 THEN RealInputWarn('r_max',rMax,rMaxDef,6)
               ELSE IF rMax>rMaxMax THEN
                  RealInputWarn('r_max',rMax,rMaxMax,6);
               rMax:=rMax/L0;
               nSteps:=trunc(-rMax/dSep);
               IF nSteps<NautoSteps THEN nSteps:=NautoSteps;
               rMax:=-nSteps*dSep; dr:=-dSep;
               rhoMax:=rMax; zMax:=rMax;
               PrhoMax:=rhoMax; PzMax:=zMax;
               drho:=rhoMax/(NrhoSteps-0.5); dz:=zMax/NzSteps;
               zetaNucl:=2*m1*m2/(m1+m2)
                              *sqr(L0)*V0*Efac*1.0e9*sqr(Lfac)/sqr(hbarc);
               {New(z); New(Diag); New(OffDiag); {Mac only}
               z.Init(nSteps,nSteps);
               z.Fill(0.0);
               FOR i:=1 TO nSteps DO z.Put(i,i,1.0);
               Diag.Init(nSteps); OffDiag.Init(nSteps);
               FOR i:=1 TO NautoSteps DO
                  Diag.Put(i,2+sqr(dSep)*(zetaNucl*Veff.Value(i)
                                                +l*(l+1)/sqr(i*dSep)));
               FOR i:=NautoSteps+1 TO nSteps DO
                  Diag.Put(i,2+sqr(dSep)*l*(l+1)/sqr(i*dSep));
               OffDiag.Fill(-1.0);
               OffDiag.Put(nSteps,0.0);
               Tridiag(nSteps,Diag,OffDiag,True,z);
               FindnthLevel(nr+1,nSteps,Diag,nLoc);
               En:=Diag.Value(nLoc)/zetaNucl/sqr(dSep);
               Rnl.Free; Rnl.Init(nSteps+1);
               Rnl.Put(1,0.0);
               FOR i:=1 TO nSteps DO
                  Rnl.Put(i+1,z.Value(i,nLoc)/(-i*dSep));
               Rnl.MinMax(iMin,RnlMin,iMax,RnlMax);
               IF abs(RnlMin)>RnlMax THEN Rnl.MultScalar(1/RnlMin)
               ELSE Rnl.MultScalar(1/RnlMax);
               OffDiag.Free; Diag.Free; z.Free;
               {Dispose(OffDiag); Dispose(Diag); Dispose(z); {Mac only}
               PrepPsiSlice(WF,Potl);
               Plotted:=False; Saved:=False;
               PrepBWFPlotScr(WF,Potl);
               PlotWF(WF,Potl,True);
            END;
         Done;
      END;
   {Dispose(ComputeScr); {Mac only}
END; {ComputeBinding}

PROCEDURE ComputeBWF(VAR FileDesc :FileDescType;
                     VAR WF :WFtype; VAR Potl :PotlType;
                     VAR MainMenu :TMenu; VAR MenuClicked :boolean);
{Compute and plot eigenfunction.}
VAR
   WFHelpScr    :HelpScrType;
   WFkeys       :THotKeys;
   Wkey         :byte;
   nn           :integer;
   WishToReturn :boolean;
BEGIN
   WFHelpScr[01]:='';
   WFHelpScr[02]:=' <F1>-show this help screen.';
   WFHelpScr[03]:='';
   WFHelpScr[04]:=' <F3>-toggle size of main plot.';
   WFHelpScr[05]:='    When small, a legend, text information,';
   WFHelpScr[06]:='    and a second plot are shown.';
   WFHelpScr[07]:='';
   WFHelpScr[08]:=' <F4>-change plot type, select different plane,';
   WFHelpScr[09]:='    or modify view of surface via rotation';
   WFHelpScr[10]:='    or change in size, perspective, or range.';
   WFHelpScr[11]:='';
   WFHelpScr[12]:=' <F5>-input new parameters.';
   WFHelpScr[13]:='';
   WFHelpScr[14]:=' <F10>-activate menu.';
   FOR nn:=15 TO 25 DO WFHelpScr[nn]:='';

   WITH WF DO
      BEGIN
         IF Computed=Binding THEN PlotWF(WF,Potl,True)
         ELSE WITH Potl DO
            BEGIN
               Zoom:=False; Plotted:=False;
               rMax:=MaxSep; rhoMax:=rMax; zMax:=rMax;
               dr:=-dSep; drho:=rhoMax/(NrhoSteps-0.5); dz:=zMax/NzSteps;
               PrhoMax:=rhoMax; PzMax:=zMax;
               RecordToFile:=False;
            END;
         ComputeBinding(WF,Potl);
         IF Computed=Binding THEN
            BEGIN
               {New(WFkeys); {Mac only}
               WITH WFkeys DO
                  BEGIN
                     Init(5);
                     key[1]:='F1-Help';
                     IF Zoom THEN key[2]:='F3-Contract'
                     ELSE key[2]:='F3-Expand';
                     key[3]:='F4-ModView';
                     key[4]:='F5-Input';
                     key[5]:='F10-Menu';
                     Display;
                  END;
               WishToReturn:=False;
               WHILE (NOT WishToReturn) AND (NOT MenuClicked) DO
                  BEGIN
                     CheckForEvents;
                     IF Event.ReadKey=Escape THEN WishToReturn:=True
                     ELSE IF WFkeys.Pressed(Wkey) THEN
                        BEGIN
                           WFkeys.Clear;
                           case Wkey of
                              1 :Help(WFHelpScr);
                              2 :BEGIN
                                   Zoom:=not Zoom; Plotted:=False;
                                   PlotWF(WF,Potl,True);
                                   IF Zoom THEN
                                      WFkeys.key[2]:='F3-Contract'
                                   ELSE WFkeys.key[2]:='F3-Expand'
                                 END;
                              3 :ModViewWF(FileDesc,WF,Potl);
                              4 :ComputeBinding(WF,Potl);
                              5 :BEGIN
                                    WishToReturn:=True;
                                    IF MainMenu.Chosen THEN
                                       MenuClicked:=True;
                                 END;
                              END; {case}
                           WFkeys.Display;
                        END
                     ELSE IF MainMenu.Activated THEN MenuClicked:=True;
                  END;
               WFkeys.Clear;
               {Dispose(WFkeys); {Mac only}
               Plotted:=False;
            END;
      END;
END; {ComputeBWF}

END. {HatmEign}
