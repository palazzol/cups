{Program title: Bound states in cylindrically symmetric potentials   }
{Author: John R. Hiller                                              }
{Version: 1.0              11/15/94                                  }
{Copyright (c) 1995  John Wiley & Sons, Inc.                         }
{                                                                    }
{Program summary:                                                    }
{     Solve the time-independent Schrodinger equation in the case of }
{a cylindrically symmetric potential for the lowest state of a chosen}
{parity and magnetic quantum number.  The method of solution is based}
{on evolution in imaginary time, which converges to the state of the }
{lowest energy with the symmetry of the initial guess.  [See S.E.    }
{Koonin, Computational Physics (Addison-Wesley, Reading, Mass., 1986)}
{p. 171-6.] Convergence is sensitive to the length of the time step; }
{the ADI algorithm can be unstable.                                  }
{                                                                    }
{Library files: none                                                 }
{Utilities: Graph, CUPS, CUPSmupp, CUPSfunc, CUPSproc, CUPSgui,      }
{           CUPSgrph                                                 }
{Other units: ClSmMach, ClSmCmn, ClSmPotl, ClSmWF                    }
{Algorithms: alternating direction implicit, for parabolic PDEs      }
{                                                                    }
PROGRAM CylSym(input,output);

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph,
     ClSmMach,ClSmCmn,ClSmPotl,ClSmWF;

VAR
   CreditScr,MainHelpScr,
   AboutProg,AboutPram,
   AboutPotl,AboutInit,
   AboutHowTo    :HelpScrType;
   WishToQuit,
   WishToSave    :boolean;
   MainMenu      :TMenu;
   MainKeys      :THotKeys;
   Mkey          :byte;
   DataFile      :text;
   ProgName      :string[8];
   VersionDate,
   FileName      :string;
   Version       :real;

{--------Routines to change parameters----------------------------------}

PROCEDURE SetMassAndUnits(VAR WF :WFtype;
                          VAR Potl :PotlType; VAR Grid :GridType);
VAR
   Modified :boolean;
BEGIN
   WITH Potl DO
      BEGIN
         SelectMassAndUnits(Particle,Mass,Lfac,Munit,Eunit,Lunit,Modified);
         IF Modified THEN
            BEGIN
               InitializeScales(Potl);
               ComputeZeta(V0,L0,Mass,Eunit,Lunit,Munit,zeta);
               SetWFUnits(WF.IDcode,Eunit,Lunit,Munit,WF.ParamUnits);
               PrepPotl(Potl,Grid); PrepWF(WF,Potl,Grid);
               IF NOT Grid.Zoom THEN Info(WF.PlotInfoScr,25,5);
            END;
      END;
END; {SetMassAndUnits}

PROCEDURE ChangeTimeStep(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   TimeScr :TInputScreen;
BEGIN
   {New(TimeScr); {Mac only}
   WITH TimeScr DO
      BEGIN
         Init;
         DefineInputPort(0.2,0.7,0.3,0.75);
         LoadLine('');
         LoadLine(       '        Time step parameters');
         LoadLine('');
         LoadLine(       '  No. of steps: {    }');
         LoadLine(ConCat('     Step size: {      }hbar/(',Potl.Eunit,'eV)'));
         LoadLine(ConCat('         Shift: {      }',Potl.Eunit,'eV'));
         LoadLine(       '     Tolerance: {      }(fractional)');
         LoadLine('');
         LoadLine('  [  OK  ]    [Cancel]');
         SetNumber(1,WF.NSteps);
         SetNumber(2,WF.StepSize);
         SetNumber(3,Potl.V0*Potl.Shift);
         SetNumber(4,WF.Tolerance);
         AcceptScreen;
         IF NOT Canceled THEN WITH WF DO
            BEGIN
               NSteps:=round(GetNumber(1));
               IF NSteps<1 THEN IntInputWarn('no. of steps',NSteps,MaxNumE,3);
               StepSize:=GetNumber(2);
               Potl.Shift:=GetNumber(3)/Potl.V0;
               Tolerance:=GetNumber(4);
            END;
         Done;
      END;
   {Dispose(TimeScr); {Mac only}
END; {ChangeTimeStep}

{--------Routines for file management----------------------------------}

PROCEDURE ReadFile(VAR DataFile :text; VAR WF :WFtype;
                   VAR Potl :PotlType; VAR Grid :GridType;
                   VAR Valid :boolean);
VAR
   NrhoExtra,NrhoRow,i,j,jRow,n      :integer;
   uTemp,TimeTemp,EHtemp,Vers        :real;
   ValidPotl,ValidWF,ValidFile       :boolean;
   pName                             :string[8];
   PotlWarnScr,WFWarnScr,FileWarnScr,
   PotlID,WFID                       :string;
BEGIN
   PotlWarnScr:='Unknown potential in file.';
   WFWarnScr:='Unknown wave function in file.';
   FileWarnScr:='Data file not appropriate for program.';
   Valid:=False;
   readln(DataFile,pName);
   readln(DataFile);
   readln(DataFile,Vers);
   ValidFile:=True;
   FOR n:=1 TO Length(pName) DO
      IF pName[n]<>ProgName[n] THEN ValidFile:=False;
   IF round(10*Vers)<>round(10*Version) THEN ValidFile:=False;
   IF ValidFile THEN
      BEGIN
         readln(DataFile,PotlID);
         ValidatePotl(PotlID,ValidPotl);
         readln(DataFile,WFID);
         ValidateWF(WFID,ValidWF);
         IF ValidPotl AND ValidWF THEN
            BEGIN
               Valid:=True;
               WITH Potl DO
                  BEGIN
                     IDcode:=PotlID;
                     readln(DataFile,NumLines);
                     FOR n:=1 TO NumLines DO readln(DataFile,InfoScr[n]);
                     readln(DataFile,NumParams);
                     FOR n:=1 TO NumParams DO
                        BEGIN
                           readln(DataFile,ParamNames[n]);
                           readln(DataFile,Params[n]);
                           readln(DataFile,DefParams[n]);
                           readln(DataFile,ParamUnits[n]);
                        END;
                     ReadParticle(DataFile,Particle);
                     readln(DataFile,V0); readln(DataFile,Eunit);
                     readln(DataFile,L0); readln(DataFile,Lunit);
                     readln(DataFile,Shift);
                     Shift:=Shift/V0;
                     readln(DataFile,Mass); readln(DataFile,Munit);
                     readln(DataFile,DefrhoMax,DefzMax,DefVertMin,DefVertMax);
                     DefrhoMax:=DefrhoMax/L0; DefzMax:=DefzMax/L0;
                     Plotted:=False;
                  END;
               WITH Grid DO
                  BEGIN
                     readln(DataFile,NrhoSteps,NzSteps,
                                     DefNrhoSteps,DefNzSteps);
                     readln(DataFile,rhoMax,zMax,drho,dz);
                     rhoMax:=rhoMax/Potl.L0; zMax:=zMax/Potl.L0;
                     drho:=drho/Potl.L0; dz:=dz/Potl.L0;
                     readln(DataFile,VertMin,VertMax);
                     readln(DataFile,Size,Distance,alpha,beta,gamma);
                     PlotType:=1; Zoom:=False;
                  END;
               InitializeScales(Potl);
               WITH Potl DO ComputeZeta(V0,L0,Mass,Eunit,Lunit,Munit,zeta);
               PrepPotl(Potl,Grid);
               WITH WF DO
                  BEGIN
                     IDcode:=WFID;
                     readln(DataFile,NumLines);
                     FOR n:=1 TO NumLines DO readln(DataFile,InfoScr[n]);
                     readln(DataFile,NumParams);
                     FOR n:=1 TO NumParams DO
                        BEGIN
                           readln(DataFile,ParamNames[n]);
                           readln(DataFile,Params[n]);
                           readln(DataFile,DefParams[n]);
                           readln(DataFile,ParamUnits[n]);
                        END;
                     readln(DataFile,Parity,zParity,m,DefParity,Defm);
                     readln(DataFile,NSteps,NStepsPrev,StepSize,Tolerance);
                     readln(DataFile,PreviousEH,ExpectH);
                     PreviousEH:=PreviousEH/Potl.V0; ExpectH:=ExpectH/Potl.V0;
                     NzSteps:=Grid.NzSteps; NrhoSteps:=Grid.NrhoSteps;
                     zMax:=Grid.zMax; rhoMax:=Grid.rhoMax;
                     dz:=Grid.dz; drho:=Grid.drho;
                     uSave.Free; u.Free; u.Init(NrhoSteps,NzSteps+1);
                     NrhoExtra:=NrhoSteps MOD 10; NrhoRow:=NrhoSteps DIV 10;
                     FOR i:=0 TO NzSteps DO
                        BEGIN
                           FOR jRow:=1 TO NrhoRow DO
                              BEGIN
                                 FOR j:=1 TO 10 DO
                                    BEGIN
                                       read(DataFile,uTemp);
                                       u.Put(j+10*(jRow-1),i+1,uTemp)
                                    END;
                                 readln(DataFile)
                              END;
                           FOR j:=1 TO NrhoExtra DO
                              BEGIN
                                 read(DataFile,uTemp);
                                 u.Put(j+10*NrhoRow,i+1,uTemp)
                              END;
                           IF NrhoExtra>0 THEN readln(DataFile)
                        END;
                     uSave.Init(NrhoSteps,NzSteps+1); uSave.Equate(u);
                     SaveExpectH:=ExpectH; SavePreviousEH:=PreviousEH;
                     Time.Rewrite; ExpectHn.Rewrite;
                     WHILE NOT eof(DataFile) DO
                        BEGIN
                           readln(DataFile,TimeTemp,EHtemp);
                           Time.PutNext(TimeTemp);
                           ExpectHn.PutNext(EHtemp/Potl.V0)
                        END;
                     SetWFUnits(IDcode,Potl.Eunit,Potl.Lunit,Potl.Munit,
                                ParamUnits);
                     PrepWFParamList(WF,Potl);
                     PrepWFPlotScr(WF,Potl);
                     Plotted:=False; Undone:=True;
                  END;
            END
         ELSE IF NOT ValidWF THEN Announce(WFWarnScr)
         ELSE Announce(PotlWarnScr);
      END
   ELSE Announce(FileWarnScr);
   Close(DataFile);
END; {ReadFile}

PROCEDURE WriteFile(VAR DataFile :text; VAR WF :WFtype;
                    VAR Potl :PotlType; VAR Grid :GridType);
VAR
   NrhoExtra,NrhoRow,i,j,jRow,n :integer;
BEGIN
   writeln(DataFile,ProgName);
   writeln(DataFile,ConCat('Version date: ',VersionDate));
   writeln(DataFile,Version :3:1);
   writeln(DataFile,Potl.IDcode);
   writeln(DataFile,WF.IDcode);
   WITH Potl DO
      BEGIN
         writeln(DataFile,NumLines :2);
         FOR n:=1 TO NumLines DO writeln(DataFile,InfoScr[n]);
         writeln(DataFile,NumParams :2);
         FOR n:=1 TO NumParams DO
            BEGIN
               writeln(DataFile,ParamNames[n]);
               writeln(DataFile,Params[n] :14);
               writeln(DataFile,DefParams[n] :14);
               writeln(DataFile,ParamUnits[n]);
            END;
         WriteParticle(DataFile,Particle);
         writeln(DataFile,V0 :9:6); writeln(DataFile,Eunit,'eV');
         writeln(DataFile,L0 :9:6); writeln(DataFile,Lunit,'m');
         writeln(DataFile,V0*Shift :9:6);
         writeln(DataFile,Mass :9:6); writeln(DataFile,Munit,'eV/c^2');
         writeln(DataFile,L0*DefrhoMax :9:6,'  ',L0*DefzMax :9:6,
                          '  ',DefVertMin :9:6,'  ',DefVertMax :9:6);
      END;
   WITH Grid DO
      BEGIN
         writeln(DataFile,NrhoSteps :3,'  ',NzSteps :3,'  ',
                          DefNrhoSteps :3,'  ',DefNzSteps :3);
         writeln(DataFile,Potl.L0*rhoMax :9:6,'  ',Potl.L0*zMax :9:6,
                          '  ',Potl.L0*drho :9:6,'  ',Potl.L0*dz :9:6);
         writeln(DataFile,VertMin :9:6,'  ',VertMax :9:6);
         writeln(DataFile,Size :9:6,'  ',Distance :9:6,
                          '  ',alpha :9:6,'  ',beta :9:6,
                          '  ',gamma :9:6)
      END;
   WITH WF DO
      BEGIN
         writeln(DataFile,NumLines :2);
         FOR n:=1 TO NumLines DO writeln(DataFile,InfoScr[n]);
         writeln(DataFile,NumParams :2);
         FOR n:=1 TO NumParams DO
            BEGIN
               writeln(DataFile,ParamNames[n]);
               writeln(DataFile,Params[n] :14);
               writeln(DataFile,DefParams[n] :14);
               writeln(DataFile,ParamUnits[n]);
            END;
         writeln(DataFile,Parity :2,'  ',zParity :2,'  ',m :2,
                          '  ',DefParity :2,'  ',Defm :2);
         writeln(DataFile,NSteps :3,'  ',NStepsPrev :3,'  ',StepSize :9:6,
                          '  ',Tolerance :10);
         writeln(DataFile,Potl.V0*PreviousEH :9:6,'  ',Potl.V0*ExpectH :9:6);
         NrhoExtra:=NrhoSteps MOD 10; NrhoRow:=NrhoSteps DIV 10;
         FOR i:=0 TO NzSteps DO
            BEGIN
               FOR jRow:=1 TO NrhoRow DO
                  BEGIN
                     FOR j:=1 TO 10 DO
                        write(DataFile,
                              u.Value(j+10*(jRow-1),i+1) :6:3,'  ');
                     writeln(DataFile);
                  END;
               FOR j:=1 TO NrhoExtra DO
                  write(DataFile,u.Value(j+10*NrhoRow,i+1) :6:3,'  ');
               IF NrhoExtra>0 THEN writeln(DataFile);
            END;
         Time.Reset; ExpectHn.Reset;
         WHILE Time.Left DO
            writeln(DataFile,Time.GetNext:6:3,
                    '  ',Potl.V0*ExpectHn.GetNext :6:3);
      END;
   Close(DataFile);
END; {WriteFile}

PROCEDURE OpenDataFile(VAR DataFile :text; VAR FileName :string;
                       VAR WF :WFtype; VAR Potl :PotlType;
                       VAR Grid :GridType);
VAR
   WishToAbort,FileExists,Valid :boolean;
BEGIN
   GetFileToOpen(FileName,WishToAbort);
   IF NOT WishToAbort THEN
      BEGIN
         ConnectTextFile(DataFile,FileName,FileExists);
         ReadFile(DataFile,WF,Potl,Grid,Valid);
         IF Valid THEN
            BEGIN
               Potl.Saved:=True;
               PlotWF(WF,Potl,Grid);
            END;
      END;
END; {OpenDataFile}

Procedure NewFile(var FileName :string;
                  VAR WF :WFtype; VAR Potl :PotlType;
                  VAR Grid :GridType);
BEGIN
   FileName:='CylSym.dat';             {set default file name}
   WITH Potl DO
      BEGIN
         Eunit:=' '; Lunit:='n';       {set units to atomic sizes}
         Particle:=electron;           {set particle to electron}
         Mass:=0.511; Munit:='M';
         V0:=1.0; L0:=1.0; Shift:=0.0;
      END;
   WITH Grid DO                        {set grid parameters}
      BEGIN
         Zoom:=False;
         NzSteps:=DefNzSteps; NrhoSteps:=DefNrhoSteps;
         SetDefaultSurfaceView(alpha,beta,gamma,Size,Distance);
      END;
   WITH WF DO                          {set default numerical parameters}
      BEGIN
         StepSize:=0.01; NSteps:=MaxNumE; Tolerance:=1.0e-6;
      END;
   SetPotlToZeeman(Potl,Grid);          {set default potential and grid}
   SetInitWFToCoulombic(WF,Potl,Grid);  {set default initial guess}
   PlotSurface(WF.u,Grid,WF.PlotInfoScr,Potl.InfoScr,
               Potl.NumLines,WF.zParity,
               True,False,WF.Plotted,'Wave function',LightMagenta);
   Potl.Saved:=True;
END; {NewFile}

PROCEDURE SaveDataFile(VAR DataFile :text; VAR FileName :string;
                       VAR WF :WFtype; VAR Potl :PotlType;
                       VAR Grid :GridType);
VAR
   WishToAbort,FileExists  :boolean;
BEGIN
   ConnectTextFile(DataFile,FileName,FileExists);
   IF FileExists THEN AskOverWrite(FileName,WishToAbort)
   ELSE WishToAbort:=False;
   IF NOT WishToAbort THEN
      BEGIN
         Rewrite(DataFile);
         WriteFile(DataFile,WF,Potl,Grid);
         Potl.Saved:=True;
      END
   ELSE Close(DataFile);
END; {SaveDataFile}

PROCEDURE SaveAs(VAR DataFile :text; VAR FileName :string;
                 VAR WF :WFtype; VAR Potl :PotlType;
                 VAR Grid :GridType);
VAR
   WishToAbort :boolean;
BEGIN
   GetFileName(FileName,WishToAbort);
   IF NOT WishToAbort THEN SaveDataFile(DataFile,FileName,WF,Potl,Grid)
END; {SaveAs}

{--------Routines to prepare and handle main menu and hot keys---------}

PROCEDURE Initialize(VAR FileName :string; VAR WF :WFtype;
                     VAR Potl :PotlType; VAR Grid :GridType);
VAR
   n :integer;
BEGIN
   DefineViewPort(1,0.0,0.25,0.08,0.38);    {viewport for Euler angles}
   DefineScale(1,0.0,1.0,0.0,1.0);        
   DefineViewPort(2,0.08,0.62,0.47,0.87);   {viewport for 2D plots}
   DefineViewPort(3,0.08,0.96,0.12,0.87);   {viewport for large plots}
   DefineViewPort(4,0.255,0.65,0.08,0.38);  {viewport for form of potential}
   DefineViewPort(5,0.7,1.0,0.08,0.92);     {viewport for parameter list}
   {New(Potl.Vmat); {Mac only}
   Potl.Vmat.Init(1,1);     {initialize matrices and circular buffers}
   WITH WF DO                        
      BEGIN
         {New(u); New(uSave); {Mac only}
         u.Init(1,1); uSave.Init(1,1);
         {New(Time); New(ExpectHn); {Mac only}
         Time.Init(MaxNumE); ExpectHn.Init(MaxNumE)
      END;
   WITH Grid Do             {initialize default grid size}
      BEGIN
         DefNzSteps:=20; DefNrhoSteps:=20;
      END;


   AboutProg[01]:='';
   AboutProg[02]:='                 BOUND STATES';
   AboutProg[03]:='       IN CYLINDRICALLY SYMMETRIC POTENTIALS';
   AboutProg[04]:='';
   AboutProg[05]:='                 John R. Hiller';
   AboutProg[06]:='         University of Minnesota, Duluth';
   AboutProg[07]:='';
   AboutProg[08]:=ConCat('                  Version ',NumStr(Version,3,1));
   AboutProg[09]:='         (c) 1995 John Wiley & Sons, Inc.';
   AboutProg[10]:='';
   AboutProg[11]:='    This program solves the time-independent';
   AboutProg[12]:='  Schrodinger equation Hu=Eu in the case of a';
   AboutProg[13]:='  cylindrically symmetric potential for the';
   AboutProg[14]:='  lowest state of a chosen parity and magnetic';
   AboutProg[15]:='  quantum number.  The method of solution is';
   AboutProg[16]:='  based on evolution in imaginary time, which';
   AboutProg[17]:='  converges to the state of the lowest energy';
   AboutProg[18]:='  that has the symmetry of the initial guess.';
   AboutProg[19]:='  The evolution is a diffusion process controlled';
   AboutProg[20]:='  by the equation HU=-hbar @U/@t, where @ denotes';
   AboutProg[21]:='  partial differentiation.  This equation is';
   AboutProg[22]:='  solved by the alternating direction implicit';
   AboutProg[23]:='  method.';
   AboutProg[24]:='';
   AboutProg[25]:='';

   FOR n:=1 TO 24 DO CreditScr[n]:=AboutProg[n];
   CreditScr[25]:='  Press <Enter> or click mouse to continue.';

   MainHelpScr[01]:='';
   MainHelpScr[02]:=' Menu choices:';
   MainHelpScr[03]:='';
   MainHelpScr[04]:='  File - use files to obtain or save';
   MainHelpScr[05]:='       starting information; exit from program.';
   MainHelpScr[06]:='';
   MainHelpScr[07]:='  Parameters - set parameters, including';
   MainHelpScr[08]:='     particle type & mass, energy & length units,';
   MainHelpScr[09]:='     grid size, viewing angles and perspective.';
   MainHelpScr[10]:='';
   MainHelpScr[11]:='  Potential - choose or modify potential.';
   MainHelpScr[12]:='';
   MainHelpScr[13]:='  Initial Wf - initialize wave function.';
   MainHelpScr[14]:='';
   MainHelpScr[15]:='  Help - information about menu choices.';
   MainHelpScr[16]:='';
   MainHelpScr[17]:=' Function keys:';
   MainHelpScr[18]:='';
   MainHelpScr[19]:='    <F1> - show this help screen.';
   MainHelpScr[20]:='    <F2> - begin time steps.';
   MainHelpScr[21]:='    <F3> - take single time step.';
   MainHelpScr[22]:='    <F4> - undo last set of time steps.';
   MainHelpScr[23]:='    <F5> - change plot type.';
   MainHelpScr[24]:='    <F10> - activate menu.';
   MainHelpScr[25]:='';

   AboutPram[01]:='';
   AboutPram[02]:=' Parameters: set various parameters.';
   AboutPram[03]:='';
   AboutPram[04]:='    * Particle Type & Mass, energy & length Units.';
   AboutPram[05]:='        A dialog box is presented for selection';
   AboutPram[06]:='        of the particle type or a specified mass';
   AboutPram[07]:='        and a choice of units for the calculation.';
   AboutPram[08]:='';
   AboutPram[09]:='    * Grid Size and Spacings.';
   AboutPram[10]:='        An input screen is used to change the';
   AboutPram[11]:='        size of the grid and the number of points.';
   AboutPram[12]:='';
   AboutPram[13]:='    * Time Step and Limits.';
   AboutPram[14]:='        An input screen is used to set the size';
   AboutPram[15]:='        of the time step and the limit on the';
   AboutPram[16]:='        number of steps and the tolerance.  Also,';
   AboutPram[17]:='        the shift in the potential may be changed.';
   AboutPram[18]:='';
   AboutPram[19]:='    * Viewpoint.';
   AboutPram[20]:='        A special input screen is used to change';
   AboutPram[21]:='        the view of surface plots.';
   FOR n:=22 TO 25 DO AboutPram[n]:='';

   AboutPotl[01]:='';
   AboutPotl[02]:=' Potential: choose or modify potential.  ';
   AboutPotl[03]:='';
   AboutPotl[04]:='    * Display & Modify Current Choice,';
   AboutPotl[05]:='      and select range, mesh spacing, and';
   AboutPotl[06]:='      vertical scale.';
   AboutPotl[07]:='';
   AboutPotl[08]:='    * Choose & Modify one of the following:';
   AboutPotl[09]:='        > Zeeman - hydrogen in magnetic field';
   AboutPotl[10]:='        > H2+ ion';
   AboutPotl[11]:='        > Woods-Saxon';
   AboutPotl[12]:='        > Oscillator';
   AboutPotl[13]:='        > Cylinder - cylindrical well';
   AboutPotl[14]:='        > User-Defined';
   AboutPotl[15]:='';
   AboutPotl[16]:='      The user-defined potential should';
   AboutPotl[17]:='      be changed in the code to the';
   AboutPotl[18]:='      desired form before use.';
   FOR n:=19 TO 25 DO AboutPotl[n]:='';

   AboutInit[01]:='';
   AboutInit[02]:=' Initial Wf: choose parity and magnetic';
   AboutInit[03]:='      quantum number, and initialize wave';
   AboutInit[04]:='      function. The current one is overwritten.';
   AboutInit[05]:='';
   AboutInit[06]:='    * Display & Modify Current Choice for';
   AboutInit[07]:='      initial guess.';
   AboutInit[08]:='';
   AboutInit[09]:='    * Choose & Modify new initial guess from:';
   AboutInit[10]:='         > Coulombic';
   AboutInit[11]:='         > Two-Ctr Coulombic';
	AboutInit[12]:='            - two-centered Coulombic for ion';
   AboutInit[13]:='         > Gaussian';
   AboutInit[14]:='         > Bessel-Cosine';
   AboutInit[15]:='            - product of J_m and cosine';
   AboutInit[16]:='         > User-Defined';
   AboutInit[17]:='';
   AboutInit[18]:='      The user-defined guess should';
   AboutInit[19]:='      be changed in the code to the';
   AboutInit[20]:='      desired form before use.';
   AboutInit[21]:='';
   AboutInit[22]:='      All are multiplied by a Legendre function';
   AboutInit[23]:='      P_lm, where l is equal to m or m+1 so that';
   AboutInit[24]:='      the chosen parity is equal to (-1)^l.';
   FOR n:=25 TO 25 DO AboutInit[n]:='';

   AboutHowTo[01]:='';
   AboutHowTo[02]:=' How To Use:';
   AboutHowTo[03]:='';
   AboutHowTo[04]:='     Modify the initial guess for the wave';
   AboutHowTo[05]:=' function to keep it well within the potential;';
   AboutHowTo[06]:=' use <H> as a guide.  Iterate with a small time';
   AboutHowTo[07]:=' step for 20 or more steps.  Check that the wave';
   AboutHowTo[08]:=' function is very small near the edges of the';
   AboutHowTo[09]:=' grid; if not, increase the grid size.  Select';
   AboutHowTo[10]:=' the <H>_n plot type to check convergence.  Once';
   AboutHowTo[11]:=' nearly converged, decrease the grid spacings';
   AboutHowTo[12]:=' for better accuracy and iterate for several ';
   AboutHowTo[13]:=' more steps.  For stability, the time step may';
   AboutHowTo[14]:=' need to be reduced.  If the range of the <H>_n';
   AboutHowTo[15]:=' plot is exceeded, deselect it, then reselect to';
   AboutHowTo[16]:=' induce use of a new range.  If the eigenvalue';
   AboutHowTo[17]:=' sought is negative, a positive value of "Shift"';
   AboutHowTo[18]:=' that makes <H>+Shift positive might be useful.';
   AboutHowTo[19]:='';
   AboutHowTo[20]:='     To create an initial wave function that is';
   AboutHowTo[21]:=' strictly zero outside some range, initialize it';
   AboutHowTo[22]:=' with a grid set at this range, and later (out of';
   AboutHowTo[23]:=' "Initial Wf") expand the grid to the full size.';
   AboutHowTo[24]:=' Zeros will be inserted outside the original grid.';
   AboutHowTo[25]:='';

   NewFile(FileName,WF,Potl,Grid);
   CommInit(FileName);
END; {Initialize}

PROCEDURE DisplayPotential(VAR WF :WFtype;
                           VAR Potl :PotlType; VAR Grid :GridType;
                           VAR MainMenu :TMenu; VAR MenuClicked :boolean);
{Display potential and allow modifications.}
VAR
   DPHelpScr    :HelpScrType;
   DPkeys       :THotKeys;
   Dkey         :byte;
   WishToReturn :boolean;
   PotlPlotType :integer;
BEGIN
   DPHelpScr[01]:='';
   DPHelpScr[02]:=' <F1>-show this help screen.';
   DPHelpScr[03]:='';
   DPHelpScr[04]:=' <F2>-modify potential. An input screen will';
   DPHelpScr[05]:='    appear to allow changes to parameters';
   DPHelpScr[06]:='    and the vertical range of the plot';
   DPHelpScr[07]:='    (set by vert_min and vert_max).';
   DPHelpScr[08]:='';
   DPHelpScr[09]:=' <F3>-change grid size and spacings.';
   DPHelpScr[10]:='';
   DPHelpScr[11]:=' <F4>-modify view of surface via rotation';
   DPHelpScr[12]:='    or via change in size or perspective.';
   DPHelpScr[13]:='';
   DPHelpScr[14]:=' <F5>-toggle between surface and contour plots.';
   DPHelpScr[15]:='';
   DPHelpScr[16]:=' <Enter>-accept potential.';
   DPHelpScr[17]:='     The parameterization of the potential';
   DPHelpScr[18]:='     is then set.';
   DPHelpScr[19]:='';
   DPHelpScr[20]:=' The default view of the potential is such';
   DPHelpScr[21]:=' that the z axis (the axis of cylindrical';
   DPHelpScr[22]:=' symmetry) points to the right.  The other';
   DPHelpScr[23]:=' axis can be considered any transverse';
   DPHelpScr[24]:=' direction.';
   DPHelpScr[25]:='';

   WITH Potl,Grid DO
      BEGIN
         {New(DPkeys); {Mac only}
         WITH DPkeys DO
            BEGIN
               Init(6);
               key[1]:='F1-Help';
               key[2]:='F2-Input';
               key[3]:='F3-Grid';
               key[4]:='F4-ModView';
               IF PlotType=1 THEN key[5]:='F5-Contour'
               ELSE key[5]:='F5-Surface';
               key[6]:='RETURN-Accept';
            END;
         IF PlotType=2 THEN PotlPlotType:=2
         ELSE PotlPlotType:=1;
         PlotPotential(Potl,Grid,PotlPlotType);
         ModifyPotential(Potl,Grid);
         UpdateWF(WF,Potl,Grid);
         PlotPotential(Potl,Grid,PotlPlotType);
         IF NOT Grid.Zoom THEN Info(PlotInfoScr,25,5);
         DPkeys.Display;
         WishToReturn:=False;
         WHILE (NOT WishToReturn) AND (NOT MenuClicked) DO
            BEGIN
               CheckForEvents;
               IF Event.ReadKey=Escape THEN WishToReturn:=True
               ELSE IF DPkeys.Pressed(Dkey) THEN
                  BEGIN
                     DPkeys.Clear;
                     CASE Dkey OF
                        1 :Help(DPHelpScr);
                        2 :BEGIN
                              ModifyPotential(Potl,Grid);
                              UpdateWF(WF,Potl,Grid);
                              PlotPotential(Potl,Grid,PotlPlotType);
                           END;
                        3 :BEGIN
                              ChangeGrid(WF,Potl,Grid,True);
                              PlotPotential(Potl,Grid,PotlPlotType);
                           END;
                        4 :BEGIN
                              IF PotlPlotType<>1 then Plotted:=False;
                              PotlPlotType:=1;
                              DPkeys.key[5]:='F5-Contour';
                              RotateSurface(Vmat,Grid,PlotInfoScr,InfoScr,
                                            NumLines,1,True,Plotted,
                                            'Potential',LightCyan);
                           END;
                        5 :BEGIN
                              PotlPlotType:=(PotlPlotType MOD 2)+1;
                              Plotted:=False;
                              PlotPotential(Potl,Grid,PotlPlotType);
                              IF PotlPlotType=1 THEN
                                 DPkeys.key[5]:='F5-Contour'
                              ELSE DPkeys.key[5]:='F5-Surface';
                           END;
                        6 :WishToReturn:=True
                        END; {case}
                     DPkeys.Display;
                  END
               ELSE IF MainMenu.Activated THEN MenuClicked:=True;
            END;
         Plotted:=False; {force new plot on next return}
         WF.Plotted:=False;
         DPkeys.Clear;
         {Dispose(DPkeys); {Mac only}
      END;
END; {DisplayPotential}

Procedure DisplayInitWF(var WF :WFtype;
                        VAR Potl :PotlType; VAR Grid :GridType);
{Display initial wave function and allow modifications.}
BEGIN
   WITH WF DO
      BEGIN
         PlotSurface(u,Grid,PlotInfoScr,InfoScr,NumLines,zParity,
                     True,False,Plotted,'Wave function',LightMagenta);
         ModifyInitWF(WF,Potl,Grid);
         IF NOT Grid.Zoom THEN Info(Potl.InfoScr,Potl.NumLines,4);
      END
END; {DisplayInitWF}

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
            Row(1,4,'New');
            Row(1,5,'Open...');
            Row(1,6,'Save');
            Row(1,7,'Save As...');
            Row(1,8,'Exit Program');
         Column(2,'Parameters');
            Row(2,1,'Particle Type & Mass, Units');
            Row(2,2,'Grid Size and Spacings');
            Row(2,3,'Time Step and Limits');
            Row(2,4,'Viewpoint');
         Column(3,'Potential');
            Row(3,1,'Display & Modify Current Choice');
            Row(3,2,'Choose & Modify: Zeeman');
            Row(3,3,'                 H2+ ion');
            Row(3,4,'                 Woods-Saxon');
            Row(3,5,'                 Oscillator');
            Row(3,6,'                 Cylinder');
            Row(3,7,'                 User-Defined');
         Column(4,'Initial Wf');
            Row(4,1,'Display & Modify Initial Guess');
            Row(4,2,'Choose & Modify: Coulombic');
            Row(4,3,'                 Two-Ctr Coulombic');
            Row(4,4,'                 Gaussian');
            Row(4,5,'                 Bessel-Cosine');
            Row(4,6,'                 User-Defined');
         Column(5,'Help');
            Row(5,1,'Summary');
            Row(5,2,'`File''');
            Row(5,3,'`Parameters''');
            Row(5,4,'`Potential''');
            Row(5,5,'`Initial Wf''');
            Row(5,6,'How To Use');
         Display;
      END;
   WITH MainKeys DO                {Prepare main hot keys.}
      BEGIN
         Init(6);
         key[1]:='F1-Help';
         key[2]:='F2-Run';
         key[3]:='F3-Step';
         key[4]:='F4-Undo';
         key[5]:='F5-PlotType';
         key[6]:='F10-Menu';
         Display;
      END;
END; {PrepMain}

PROCEDURE HandleMenu(VAR MainMenu :TMenu; VAR MainKeys :THotKeys;
                     VAR DataFile :text; VAR FileName :string;
                     VAR WF :WFtype; VAR Potl :PotlType;
                     VAR Grid :GridType; VAR WishToQuit :boolean);
{Process selection from main menu.}
CONST 
   Ffile=1; Parameters=2; Potential=3; InitWF=4; Hhelp=5;
VAR
   Verified,MenuClicked :boolean;
BEGIN
   MainKeys.Clear;
   WITH MainMenu DO
      REPEAT
         MenuClicked:=False;
         CASE colChosen OF 
            Ffile :case rowChosen OF
                      1 :AboutCUPS;
                      2 :Help(AboutProg);
                      3 :Configuration;
                      4 :BEGIN
                            VerifyNew(Potl.Saved,Verified);
                            IF Verified THEN
                               NewFile(FileName,WF,Potl,Grid)
                         END;
                      5 :BEGIN
                            VerifyNew(Potl.Saved,Verified);
                            IF Verified THEN
                               OpenDataFile(DataFile,FileName,WF,Potl,Grid)
                         END;
                      6 :SaveDataFile(DataFile,FileName,WF,Potl,Grid);
                      7 :SaveAs(DataFile,FileName,WF,Potl,Grid);
                      8 :WishToQuit:=True;
                      END; {case rowChosen}
            Parameters :case rowChosen OF
                           1 :SetMassAndUnits(WF,Potl,Grid);
                           2 :BEGIN
                                 ChangeGrid(WF,Potl,Grid,False);
                                 PlotWF(WF,Potl,Grid);
                              END;
                           3 :ChangeTimeStep(WF,Potl);
                           4 :BEGIN
                                 Grid.PlotType:=1;
                                 RotateSurface(WF.u,Grid,WF.PlotInfoScr,
                                               Potl.InfoScr,
                                               Potl.NumLines,WF.zParity,
                                               False,WF.Plotted,
                                               'Wave function',LightMagenta);
                              END;
                           END; {case rowChosen}
            Potential :BEGIN
                          IF rowChosen>1 THEN
                             BEGIN
                                CASE rowChosen OF
                                   2 :SetPotlToZeeman(Potl,Grid);
                                   3 :SetPotlToH2Ion(Potl,Grid);
                                   4 :SetPotlToWoodsSaxon(Potl,Grid);
                                   5 :SetPotlToOscillator(Potl,Grid);
                                   6 :SetPotlToCylinder(Potl,Grid);
                                   7 :SetPotlToUserDefined(Potl,Grid);
                                   END; {case rowChosen}
                                InterpolateWF(WF,Potl,Grid)
                             END;
                          DisplayPotential(WF,Potl,Grid,MainMenu,MenuClicked);
                          SetWFUnits(WF.IDcode,
                                     Potl.Eunit,Potl.Lunit,Potl.Munit,
                                     WF.ParamUnits);
                          PrepWFParamList(WF,Potl);
                          PrepWFPlotScr(WF,Potl);
                       END;
            InitWF :BEGIN
                       IF rowChosen=1 THEN
                          BEGIN
                             InitializeWF(WF,Potl,Grid);
                             PrepWFPlotScr(WF,Potl);
                          END
                       ELSE
                          CASE rowChosen OF
                             2 :SetInitWFToCoulombic(WF,Potl,Grid);
                             3 :SetInitWFToTwoCtrCoulombic(WF,Potl,Grid);
                             4 :SetInitWFToGaussian(WF,Potl,Grid);
                             5 :SetInitWFToBesselCosine(WF,Potl,Grid);
                             6 :SetInitWFToUserDefined(WF,Potl,Grid);
                             END; {case rowChosen}
                       DisplayInitWF(WF,Potl,Grid);
                    END;
            Hhelp :case rowChosen OF
                      1 :Help(MainHelpScr);
                      2 :Help(AboutFile);
                      3 :Help(AboutPram);
                      4 :Help(AboutPotl);
                      5 :Help(AboutInit);
                      6 :Help(AboutHowTo)
                      END; {case rowChosen}
            END; {case of colChosen}
      UNTIL NOT MenuClicked;
   PlotWF(WF,Potl,Grid);
   MainKeys.Display;
END; {HandleMain}

PROCEDURE HandleHotKeys(VAR MainMenu :TMenu;
                        VAR MainKeys :THotKeys; Mkey :byte;
                        VAR DataFile :text; VAR FileName :string;
                        VAR WF :WFtype; VAR Potl :PotlType;
                        VAR Grid :GridType; VAR WishToQuit :boolean);
{Process selection from main hot keys.}
BEGIN
   MainKeys.Clear;
   CASE Mkey OF
      1 :Help(MainHelpScr);
      2 :IterateWF(WF,Potl,Grid,False);
      3 :IterateWF(WF,Potl,Grid,True);   {Single step}
      4 :UndoWF(WF,Potl,Grid);
      5 :ChangePlotType(WF,Potl,Grid);
      6 :BEGIN
            CheckForEvents;
            IF MainMenu.Chosen THEN 
               HandleMenu(MainMenu,MainKeys,
                          DataFile,FileName,WF,Potl,Grid,WishToQuit);
         END;
      END; {case}
   MainKeys.Display;
END; {HandleHotKeys}

{--------Main program--------------------------------------------------}

BEGIN {CylSym}
   CUPSinit;
   ClearMUPPETport;
   ProgName:='CylSym'; VersionDate:='November 15, 1994'; Version:=1.0;
   cPi:=Pi;
   {New(MainMenu); New(MainKeys); {Mac only}
   PrepMain(MainMenu,MainKeys);
   Initialize(FileName,WF,Potl,Grid);
   Help(CreditScr);
   WishToQuit:=False;
   WHILE NOT WishToQuit DO
      BEGIN
         CheckForEvents;
         IF Event.ReadKey=Escape THEN WishToQuit:=True
         ELSE IF MainKeys.Pressed(Mkey) THEN
            HandleHotKeys(MainMenu,MainKeys,Mkey,
                          DataFile,FileName,WF,Potl,Grid,WishToQuit)
         ELSE IF MainMenu.Activated THEN
            HandleMenu(MainMenu,MainKeys,
                       DataFile,FileName,WF,Potl,Grid,WishToQuit);
         IF WishToQuit AND (NOT Potl.Saved) THEN
            BEGIN
               VerifyQuit(DataFile,FileName,WishToSave,WishToQuit);
               IF WishToSave THEN
                  SaveDataFile(DataFile,FileName,WF,Potl,Grid)
            END
      END;
   MainMenu.Done;
   {Dispose(MainMenu); Dispose(MainKeys); {Mac only}
   {Dispose(Potl.Vmat); {Mac only}
   {Dispose(WF.u); Dispose(WF.uSave); {Mac only}
   {Dispose(WF.Time); Dispose(WF.ExpectHn); {Mac only}
   ClearMUPPETport;
   CUPSdone;
END. {CylSym}
