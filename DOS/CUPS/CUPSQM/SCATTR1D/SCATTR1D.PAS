{Program title: Stationary scattering states in one dimension         }
{Author: John R. Hiller                                               }
{Version: 1.0               11/20/94                                  }
{Copyright (c) 1995 John Wiley & Sons, Inc.                           }
{                                                                     }
{Program summary:                                                     }
{     Compute wave functions and transmission/reflection probabilities}
{for a variety of one-dimensional potentials.  The potentials are     }
{assumed to be zero to the left of x_min and constant to the right    }
{of x_max, with particles incident from the left.  The wave functions }
{are computed from right to left, up to an arbitrary complex coeffi-  }
{cient which is determined by fixing the incident amplitude and phase.}
{The transmission and reflection probabilities are extracted from     }
{ratios of coefficients, and can be plotted as functions of energy.   }
{     The program assumes that computations are to be done for        }
{electrons incident on potentials measured in eV.  Lengths are        }
{measured in nm.                                                      }
{                                                                     }
{Library files: none                                                  }
{Utilities: Graph, CUPS, CUPSmupp, CUPSfunc, CUPSproc, CUPSgui        }
{Other units: Sc1DMach, Sc1DCmn, Sc1DPotl, Sc1DWF, Sc1DTR             }
{Algorithms: 4th order Runge-Kutta (StepRK4 in CUPSmath)              }
{                                                                     }
PROGRAM Scattr1D(input,output);

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,
     Sc1DMach,Sc1DCmn,Sc1DPotl,Sc1DWF,Sc1DTR;

VAR
   CreditScr,AboutMenu,
   AboutProg,AboutPram,AboutComp,
   AboutPotl,AboutAlgs :HelpScrType;
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

{--------Routines for changes to numerical parameters----------------------}

PROCEDURE ChangeNumericalParams(VAR Potl :PotlType;
                                VAR WFcomputed,TRcomputed :boolean);
VAR
   InputScr                :TInputScreen;
   offset,nStepsOld,
   nRStepsOld,nLStepsOld   :integer;
   HorizMaxOld,HorizMinOld :real;
   Failed,TempBool         :boolean;
BEGIN
   IF Potl.DefAlg=0 THEN offset:=1
   ELSE
      BEGIN
         offset:=0;
         IF Potl.Alg=0 THEN Potl.Alg:=Potl.DefAlg;
      END;
   {New(InputScr); {Mac only}
   WITH InputScr DO
      REPEAT
         Failed:=False;
         Init;
         DefineInputPort(0.2,0.75,0.25,0.8);
         LoadLine('');
         LoadLine('      Numerical parameters');
         LoadLine('');
         LoadLine('  Choice of integration algorithm:');
         IF Potl.DefAlg=0 THEN
            LoadLine('     piecewise constant #1');
         LoadLine(   '        transfer matrix #1');
         LoadLine(   '     direct integration #1');
         LoadLine(   '     integration of log #1');
         LoadLine('');
         LoadLine('  Number of integration steps: {    }');
         LoadLine('');
         LoadLine('  [  OK  ]    [Cancel]');
         SetRadioButton('1',Potl.Alg+offset);
         SetNumber(offset+NumAlgs+1,Potl.nSteps);
         AcceptScreen;
         IF NOT Canceled THEN
            BEGIN
               nStepsOld:=Potl.nSteps;
               WFcomputed:=False; TRcomputed:=False;
               Potl.Alg:=GetRadioButton('1')-offset;
               Potl.nSteps:=round(GetNumber(offset+NumAlgs+1));
               IF Potl.nSteps<1 THEN
                  IntInputWarn('number of steps',Potl.nSteps,nStepsOld,4)
               ELSE IF Potl.nSteps>MaxRange THEN
                  IntInputWarn('number of steps',Potl.nSteps,MaxRange,4);
               WITH Potl DO
                  BEGIN
                     nRStepsOld:=nRSteps; nLStepsOld:=nLSteps;
                     HorizMaxOld:=HorizMax; HorizMinOld:=HorizMin;
                     dx:=(xMax-xMin)/nSteps;
                     nRSteps:=round((HorizMax/L0-xMax)/dx);
                     HorizMax:=(xMax+dx*nRSteps)*L0;
                     nLSteps:=round((xMin-HorizMin/L0)/dx);
                     HorizMin:=(xMin-dx*nLSteps)*L0;
                  END;
               PrepPotl(Potl,Failed);
               IF Failed THEN WITH Potl DO
                  BEGIN
                     nSteps:=nStepsOld;
                     dx:=(xMax-xMin)/nSteps;
                     nRSteps:=nRStepsOld; nLSteps:=nLStepsOld;
                     HorizMax:=HorizMaxOld; HorizMin:=HorizMinOld;
                     PrepPotl(Potl,TempBool);
                     Announce(ConCat('Insufficient dynamic range for chosen',
                                     ' number of steps.'));
                  END;
            END;
         Done;
      UNTIL NOT Failed;
   {Dispose(InputScr); {Mac only}
END; {ChangeNumericalParams}

{--------Routines for file management------------------------------------}

PROCEDURE ReadFile(VAR DataFile :text; VAR WF :WFtype;
                   VAR TR :TRtype; VAR Potl :PotlType; VAR Valid :boolean);
CONST
   PotlWarnScr='Unknown potential in file.';
   FileWarnScr='Data file not appropriate for program.';
VAR
   iSplit,n    :integer;
   TempBool,
   ValidPotl,ValidFile   :boolean;
   pName       :string[8];
   ID          :string;
   Vers        :real;
BEGIN
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
         readln(DataFile,ID);
         ValidatePotl(ID,ValidPotl);
         IF ValidPotl THEN
            BEGIN
               WITH Potl DO
                  BEGIN
                     Valid:=True;
                     IDcode:=ID;
                     readln(DataFile,NumLines);
                     FOR n:=1 TO NumLines DO readln(DataFile,InfoScr[n]);
                     readln(DataFile,Invert);
                     readln(DataFile,NumParams);
                     FOR n:=1 TO NumParams DO
                        BEGIN
                           readln(DataFile,ParamNames[n]);
                           readln(DataFile,Params[n]);
                           readln(DataFile,DefParams[n]);
                           readln(DataFile,ParamUnits[n]);
                        END;
                     readln(DataFile,Vinfty); 
                     readln(DataFile,V0,DefV0); readln(DataFile,Eunit);
                     readln(DataFile,L0); readln(DataFile,Lunit);
                     readln(DataFile,Mass); readln(DataFile,Munit);
                     readln(DataFile,xMin,xMax,DefxMin,DefxMax);
                     readln(DataFile,DefHorizMin,DefHorizMax);
                     Vinfty:=Vinfty/V0;
                     xMin:=xMin/L0; xMax:=xMax/L0;
                     DefxMin:=DefxMin/L0; DefxMax:=DefxMax/L0;
                     DefHorizMin:=DefHorizMin/L0;
                     DefHorizMax:=DefHorizMax/L0;
                     readln(DataFile,HorizMin,HorizMax,VertMin,VertMax);
                     readln(DataFile,Alg,DefAlg);
                     readln(DataFile,nSteps,nLSteps,nRSteps,DefnSteps);
                     dx:=(xMax-xMin)/nSteps;
                     Zoom:=False
                  END;
               PrepPotl(Potl,TempBool);
               readln(DataFile,WF.PlotType,iSplit);
               IF WF.PlotType<0 THEN SetWFDefaults(WF,Potl)
               ELSE WITH WF DO
                  BEGIN
                     IF iSplit=1 THEN SplitIncRefl:=True
                     ELSE SplitIncRefl:=False;
                     readln(DataFile,Energy,MagMax);
                     Energy:=Energy/Potl.V0;
                     readln(DataFile,ReflAmp,ReflPhase);
                     readln(DataFile,TransAmp,TransPhase,LogT);
                     readln(DataFile,HorizMin,HorizMax,VertMin,VertMax);
                     readln(DataFile,nLSteps,nRSteps);
                     Phase.Free; Mag.Free;
                     Mag.Init(Potl.nSteps+1); Phase.Init(Potl.nSteps+1);
                     ReadDVector(DataFile,Potl.nSteps+1,Mag,1.0);
                     ReadDVector(DataFile,Potl.nSteps+1,Phase,1.0);
                     Computed:=True; Zoom:=False;
                     PrepWFPlotScr(WF,Potl);
                  END;
               readln(DataFile,TR.PlotType);
               IF TR.PlotType<0 THEN SetTRDefaults(TR,Potl)
               ELSE WITH TR DO
                  BEGIN
                     readln(DataFile,Emin,Emax,VertMin,VertMax);
                     Emin:=Emin/Potl.V0; Emax:=Emax/Potl.V0;
                     readln(DataFile,deltaE,LogTRmin);
                     deltaE:=deltaE/Potl.V0;
                     readln(DataFile,NumE);
                     ReadDVector(DataFile,NumE,T,1.0);
                     ReadDVector(DataFile,NumE,LogT,1.0);
                     ReadDVector(DataFile,NumE,Energy,Potl.V0);
                     Computed:=True; Zoom:=False;
                     PrepTRPlotScr(TR,Potl);
                  END
            END
         ELSE Announce(PotlWarnScr);
      END
   ELSE Announce(FileWarnScr);
   Close(DataFile)
END; {ReadFile}

PROCEDURE WriteFile(VAR DataFile :text; VAR WF :WFtype;
                    VAR TR :TRtype; VAR Potl :PotlType);
VAR
   n :integer;
BEGIN
   writeln(DataFile,ProgName);
   writeln(DataFile,ConCat('Version date: ',VersionDate));
   writeln(DataFile,Version :3:1);
   WITH Potl DO
      BEGIN
         writeln(DataFile,IDcode);
         writeln(DataFile,NumLines :2);
         FOR n:=1 TO NumLines DO writeln(DataFile,InfoScr[n]);
         writeln(DataFile,Invert :2);
         writeln(DataFile,NumParams :2);
         FOR n:=1 TO NumParams DO
            BEGIN
               writeln(DataFile,ParamNames[n]);
               writeln(DataFile,Params[n] :14);
               writeln(DataFile,DefParams[n] :14);
               writeln(DataFile,ParamUnits[n]);
            END;
         writeln(DataFile,V0*Vinfty :9:6);
         writeln(DataFile,V0 :9:6,DefV0 :9:6); writeln(DataFile,Eunit,'eV');
         writeln(DataFile,L0 :9:6); writeln(DataFile,Lunit,'m');
         writeln(DataFile,Mass :9:6); writeln(DataFile,Munit,'eV/c^2');
         writeln(DataFile,L0*xMin :9:6,'  ',L0*xMax :9:6,
                          '  ',L0*DefxMin :9:6,'  ',L0*DefxMax :9:6);
         writeln(DataFile,L0*DefHorizMin :9:6,'  ',L0*DefHorizMax :9:6);
         writeln(DataFile,HorizMin :9:6,'  ',HorizMax :9:6,
                          '  ',VertMin :9:6,'  ',VertMax :9:6);
         writeln(DataFile,Alg :2,DefAlg :2);
         writeln(DataFile,nSteps :4,'  ',nLSteps :4,'  ',nRSteps :4,
                          '  ',DefnSteps :4)
      END;
   IF NOT WF.Computed THEN writeln(DataFile,'-1  -1')
   ELSE WITH WF DO
      BEGIN
         write(DataFile,PlotType :2);
         IF SplitIncRefl THEN writeln(DataFile,'  1')
         ELSE writeln(DataFile,'  0');
         writeln(DataFile,Potl.V0*Energy :9:6,'  ',MagMax :9:6);
         writeln(DataFile,ReflAmp :9:6,'  ',ReflPhase :9:6);
         writeln(DataFile,TransAmp :9:6,'  ',TransPhase :9:6,
                          '  ',LogT :9:6);
         writeln(DataFile,HorizMin :9:6,'  ',HorizMax :9:6,
                          '  ',VertMin :9:6,'  ',VertMax :9:6);
         writeln(DataFile,nLSteps :4,'  ',nRSteps :4);
         WriteDVector(DataFile,Potl.nSteps+1,Mag,1.0);
         WriteDVector(DataFile,Potl.nSteps+1,Phase,1.0)
      END;
   IF NOT TR.Computed THEN writeln(DataFile,'-1')
   ELSE WITH TR DO
      BEGIN
         writeln(DataFile,PlotType :2);
         writeln(DataFile,Potl.V0*Emin :9:6,'  ',Potl.V0*Emax :9:6,
                          '  ',VertMin :9:6,'  ',VertMax :9:6);
         writeln(DataFile,Potl.V0*deltaE :9:6,'  ',LogTRmin :9:6);
         writeln(DataFile,NumE :4);
         WriteDVector(DataFile,NumE,T,1.0);
         WriteDVector(DataFile,NumE,LogT,1.0);
         WriteDVector(DataFile,NumE,Energy,Potl.V0)
      END;
   Close(DataFile)
END; {WriteFile}

PROCEDURE OpenDataFile(VAR DataFile :text; VAR FileName :string;
                       VAR WF :WFtype; VAR TR :TRtype; VAR Potl :PotlType);
VAR
   WishToAbort,FileExists,Valid :boolean;
BEGIN
   GetFileToOpen(FileName,WishToAbort);
   IF NOT WishToAbort THEN
      BEGIN
         ConnectTextFile(DataFile,FileName,FileExists);
         ReadFile(DataFile,WF,TR,Potl,Valid);
         IF Valid THEN
            BEGIN
               Potl.Saved:=True;
               IF WF.Computed THEN PlotWaveFunction(WF,Potl)
               ELSE IF TR.Computed THEN PlotTransRefl(TR,Potl)
               ELSE PlotPotential(Potl);
            END;
      END;
END; {OpenDataFile}

Procedure NewFile(var FileName :string;
                  VAR WF :WFtype; VAR TR :TRtype; VAR Potl :PotlType);
BEGIN
   FileName:='Scattr1D.dat';        {set default file name}
   WITH Potl DO
      BEGIN
         Eunit:=' '; Lunit:='n';    {set units to atomic sizes}
         Alg:=0;               
         nSteps:=100;
      END;
   SetPotlToSquare(Potl);           {set default potential}
   PlotPotential(Potl);
   SetWFDefaults(WF,Potl);
   SetTRDefaults(TR,Potl)
END; {NewFile}

PROCEDURE SaveDataFile(VAR DataFile :text; VAR FileName :string;
                       VAR WF :WFtype; VAR TR :TRtype; VAR Potl :PotlType);
VAR
   WishToAbort,FileExists  :boolean;
BEGIN
   ConnectTextFile(DataFile,FileName,FileExists);
   IF FileExists THEN AskOverWrite(FileName,WishToAbort)
   ELSE WishToAbort:=False;
   IF NOT WishToAbort THEN
      BEGIN
         Rewrite(DataFile);
         WriteFile(DataFile,WF,TR,Potl);
         Potl.Saved:=True;
      END
   ELSE Close(DataFile);
END; {SaveDataFile}

PROCEDURE SaveAs(VAR DataFile :text; VAR FileName :string;
                 VAR WF :WFtype; VAR TR :TRtype; VAR Potl :PotlType);
VAR
   WishToAbort :boolean;
BEGIN
   GetFileName(FileName,WishToAbort);
   IF NOT WishToAbort THEN SaveDataFile(DataFile,FileName,WF,TR,Potl)
END; {SaveAs}

{--------Routines to prepare and handle main menu and hot keys---------}

PROCEDURE Initialize(VAR FileName :string;
                     VAR WF :WFtype; VAR TR :TRtype; VAR Potl :PotlType);
VAR
   n :integer;
BEGIN
   DefineViewPort(1,0.0,0.25,0.06,0.43);   {viewport for legends}
   DefineScale(1,0.0,1.0,0.0,1.0);         {scale for legends}
   DefineViewPort(2,0.1,0.65,0.47,0.87);   {viewport for plots}
   DefineViewPort(3,0.08,0.96,0.12,0.87);  {viewport for large plots}
   DefineViewPort(4,0.255,0.65,0.08,0.38); {viewport for form of potential}
   DefineViewPort(5,0.7,1.0,0.08,0.92);    {viewport for parameter list}
   {New(Potl.V); {Mac only}
   Potl.V.Init(1);                        {initialize DVectors}
   WITH WF DO                             
      BEGIN
         {New(Mag); New(Phase); {Mac only}
         Mag.Init(1); Phase.Init(1);
      END;
   WITH TR DO
      BEGIN
         {New(T); New(LogT); New(Energy); {Mac only}
         T.Init(MaxNumE); LogT.Init(MaxNumE); Energy.Init(MaxNumE);
      END;

   AboutProg[01]:='';
   AboutProg[02]:='  STATIONARY SCATTERING STATES IN ONE DIMENSION';
   AboutProg[03]:='';
   AboutProg[04]:='                 John R. Hiller';
   AboutProg[05]:='         University of Minnesota, Duluth';
   AboutProg[06]:='';
   AboutProg[07]:=ConCat('                  Version ',NumStr(Version,3,1));
   AboutProg[08]:='         (c) 1995 John Wiley & Sons, Inc.';
   AboutProg[09]:='';
   AboutProg[10]:='     This program solves the time-independent';
   AboutProg[11]:='  Schrodinger equation for stationary scattering';
   AboutProg[12]:='  states in one-dimensional potentials.  The';
   AboutProg[13]:='  wave function is displayed in a variety of';
   AboutProg[14]:='  ways, and the transmission and reflection';
   AboutProg[15]:='  probabilities are computed.  The probabilities';
   AboutProg[16]:='  may be displayed as functions of energy.';
   AboutProg[17]:='';
   AboutProg[18]:='     The computations are done by numerically';
   AboutProg[19]:='  integrating the Schrodinger equation from the';
   AboutProg[20]:='  region of the transmitted wave to the region of';
   AboutProg[21]:='  the incident and reflected waves.  The potential';
   AboutProg[22]:='  is assumed to be zero in the incident region and';
   AboutProg[23]:='  constant in the region of transmission.';
   FOR n:=24 TO 25 DO AboutProg[n]:='';

   FOR n:=1 TO 24 DO CreditScr[n]:=AboutProg[n];
   CreditScr[25]:='  Press <Enter> or click mouse to continue.';

   AboutMenu[01]:='';
   AboutMenu[02]:=' Menu choices:';
   AboutMenu[03]:='';
   AboutMenu[04]:='    File - use files to obtain or save data;';
   AboutMenu[05]:='       information about CUPS and the program;';
   AboutMenu[06]:='       exit from program.';
   AboutMenu[07]:='';
   AboutMenu[08]:='    Parameters - change numerical parameters.';
   AboutMenu[09]:='';
   AboutMenu[10]:='    Potential - choose or modify potential.';
   AboutMenu[11]:='';
   AboutMenu[12]:='    Compute - compute wave functions';
   AboutMenu[13]:='       and transmission & reflection';
   AboutMenu[14]:='       probabilities.';
   AboutMenu[15]:='';
   AboutMenu[16]:='    Help - display various help screens.';
   FOR n:=17 TO 25 DO AboutMenu[n]:='';

   AboutPram[01]:='';
   AboutPram[02]:=' Parameters: change numerical parameters.';
   AboutPram[03]:='';
   AboutPram[04]:='  The parameters include the number of';
   AboutPram[05]:='  integration steps and the choice of';
   AboutPram[06]:='  algorithm.';
   FOR n:=7 TO 25 DO AboutPram[n]:='';

   AboutPotl[01]:='';
   AboutPotl[02]:=' Potential: choose or modify potential.  ';
   AboutPotl[03]:='';
   AboutPotl[04]:='    * Display & Modify Current Choice.';
   AboutPotl[05]:='';
   AboutPotl[06]:='    * Choose & Modify one of the following:';
   AboutPotl[07]:='        > Square Barrier';
   AboutPotl[08]:='        > Double Square barrier';
   AboutPotl[09]:='        > Step';
   AboutPotl[10]:='        > Ramp';
   AboutPotl[11]:='        > Smooth Step';
   AboutPotl[12]:='        > Gaussian';
   AboutPotl[13]:='        > Woods-Saxon';
   AboutPotl[14]:='        > User-Defined';
   AboutPotl[15]:='';
   AboutPotl[16]:='      The user-defined potential should';
   AboutPotl[17]:='      be changed in the code to the';
   AboutPotl[18]:='      desired form before use.';
   FOR n:=19 TO 25 DO AboutPotl[n]:='';

   AboutComp[01]:='';
   AboutComp[02]:=' Compute: compute wave functions and';
   AboutComp[03]:='          transmission/reflection probabilities.';
   AboutComp[04]:='';
   AboutComp[05]:='    * Wave Function - plot previous wave';
   AboutComp[06]:='      function, if any, and compute new';
   AboutComp[07]:='      results, if desired.  The transmission';
   AboutComp[08]:='      and reflection probabilities are also';
   AboutComp[09]:='      given.';
   AboutComp[10]:='';
   AboutComp[11]:='    * Transmission & Reflection Probabilities';
   AboutComp[12]:='      - plot previous results for probabilities';
   AboutComp[13]:='      versus energy, if any, and compute new';
   AboutComp[14]:='      results, if desired.';
   FOR n:=15 TO 25 DO AboutComp[n]:='';

   AboutAlgs[01]:='';
   AboutAlgs[02]:=' Three different algorithms are included.';
   AboutAlgs[03]:=' They are:';
   AboutAlgs[04]:='';
   AboutAlgs[05]:='    1. transfer matrix approach.  See T.M Kalotas';
   AboutAlgs[06]:='    and A.R. Lee, Am. J. Phys. 59, 48 (1991).';
   AboutAlgs[07]:='';
   AboutAlgs[08]:='    2. direct integration of the Schrodinger';
   AboutAlgs[09]:='    equation.';
   AboutAlgs[10]:='';
   AboutAlgs[11]:='    3. integration of equations derived for';
   AboutAlgs[12]:='    the log of the wave function.';
   AboutAlgs[13]:='';
   AboutAlgs[14]:=' The first is also used in a special form for';
   AboutAlgs[15]:=' piecewise constant potentials, where it gives';
   AboutAlgs[16]:=' essentially exact results.  This special form';
   AboutAlgs[17]:=' is much faster than the others.  The exponential';
   AboutAlgs[18]:=' decay of the wave function inside a barrier';
   AboutAlgs[19]:=' receives special treatment in the case of the';
   AboutAlgs[20]:=' first and third algorithms.  The third algorithm';
   AboutAlgs[21]:=' can be unstable for energies near the asymptotic';
   AboutAlgs[22]:=' value of the potential on the right.';
   FOR n:=23 TO 25 DO AboutAlgs[n]:='';

   Wheel[0]:=LightRed;    Wheel[1]:=Yellow;     Wheel[2]:=LightGreen;
   Wheel[3]:=LightCyan;   Wheel[4]:=LightBlue;  Wheel[5]:=Blue;
   Wheel[6]:=Magenta;     Wheel[7]:=LightMagenta;

   NewFile(FileName,WF,TR,Potl);
   CommInit(FileName);
END; {Initialize}

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
         Column(3,'Potential');
            Row(3,1,'Display & Modify Current Choice');
            Row(3,2,'Choose & Modify: Square Barrier');
            Row(3,3,'                 Double Square');
            Row(3,4,'                 Step');
            Row(3,5,'                 Ramp');
            Row(3,6,'                 Smooth Step');
            Row(3,7,'                 Gaussian');
            Row(3,8,'                 Woods-Saxon');
            Row(3,9,'                 User-Defined');
         Column(4,'Compute');
            Row(4,1,'Wave Function');
            Row(4,2,'Trans & Refl Probabilities');
         Column(5,'Help');
            Row(5,1,'Summary');
            Row(5,2,'`File''');
            Row(5,3,'`Parameters''');
            Row(5,4,'`Potential''');
            Row(5,5,'`Compute''');
            Row(5,6,'Algorithms');
         Display;
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
                     VAR DataFile :text; VAR FileName :string;
                     VAR WF :WFtype; VAR TR :TRtype; VAR Potl :PotlType;
                     VAR WishToQuit :boolean);
{Process selection from main menu.}
CONST
   Ffile=1; Parameters=2; Potential=3; Compute=4; Hhelp=5;
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
                               NewFile(FileName,WF,TR,Potl)
                         END;
                      5 :BEGIN
                            VerifyNew(Potl.Saved,Verified);
                            IF Verified THEN
                               OpenDataFile(DataFile,FileName,WF,TR,Potl)
                         END;
                      6 :SaveDataFile(DataFile,FileName,WF,TR,Potl);
                      7 :SaveAs(DataFile,FileName,WF,TR,Potl);
                      8 :WishToQuit:=True
                      END; {case rowChosen}
            Parameters: ChangeNumericalParams(Potl,WF.Computed,TR.Computed);
            Potential:
               BEGIN
                  IF rowChosen>1 THEN
                     BEGIN
                        CASE rowChosen OF
                           2 :SetPotlToSquare(Potl);
                           3 :SetPotlToDbleSquare(Potl);
                           4 :SetPotlToStep(Potl);
                           5 :SetPotlToRamp(Potl);
                           6 :SetPotlToSmoothStep(Potl);
                           7 :SetPotlToGaussian(Potl);
                           8 :SetPotlToWoodsSaxon(Potl);
                           9 :SetPotlToUserDefined(Potl);
                           END; {case rowChosen}
                        WF.Computed:=False; TR.Computed:=False
                     END;
                  DisplayPotential(Potl,MainMenu,MenuClicked,
                                   WF.Computed,TR.Computed)
               END;
            Compute :case rowChosen OF
                        1 :ComputeWF(WF,Potl,MainMenu,MenuClicked);
                        2 :ComputeTR(TR,Potl,MainMenu,MenuClicked);
                        END; {case rowChosen}
            Hhelp :case rowChosen OF
                      1 :Help(AboutMenu);
                      2 :Help(AboutFile);
                      3 :Help(AboutPram);
                      4 :Help(AboutPotl);
                      5 :Help(AboutComp);
                      6 :Help(AboutAlgs)
                      END; {case rowChosen}
            END; {case of colChosen}
      UNTIL NOT MenuClicked;
   MainKeys.Display;
END; {HandleMenu}

PROCEDURE HandleHotKeys(VAR MainMenu :TMenu;
                        VAR MainKeys :THotKeys; Mkey :byte;
                        VAR DataFile :text; VAR FileName :string;
                        VAR WF :WFtype; VAR TR :TRtype;
                        VAR Potl :PotlType; VAR WishToQuit :boolean);
{Process selection from main hot keys.}
BEGIN
   MainKeys.Clear;
   CASE Mkey OF
      1 :Help(AboutMenu);
      2 :BEGIN
            CheckForEvents;
            IF MainMenu.Chosen THEN 
               HandleMenu(MainMenu,MainKeys,
                          DataFile,FileName,WF,TR,Potl,WishToQuit);
         END;
      END; {case}
   MainKeys.Display;
END; {HandleHotKeys}

{--------Main program------------------------------------------------------}

BEGIN {Scattr1D}
   CUPSinit;
   ClearMUPPETport;
   ProgName:='Scattr1D'; VersionDate:='November 20, 1994'; Version:=1.0;
   Log10e:=Log10(exp(1.0)); cPi:=Pi;
   {New(MainMenu); New(MainKeys); {Mac only}
   PrepMain(MainMenu,MainKeys);
   Initialize(FileName,WF,TR,Potl);
   Help(CreditScr);
   WishToQuit:=False;
   WHILE NOT WishToQuit DO 
      BEGIN
         CheckForEvents;
         IF Event.ReadKey=Escape THEN WishToQuit:=True
         ELSE IF MainKeys.Pressed(Mkey) THEN
            HandleHotKeys(MainMenu,MainKeys,Mkey,
                          DataFile,FileName,WF,TR,Potl,WishToQuit)
         ELSE IF MainMenu.Activated THEN
            HandleMenu(MainMenu,MainKeys,
                       DataFile,FileName,WF,TR,Potl,WishToQuit);
         IF WishToQuit AND (NOT Potl.Saved) THEN
            BEGIN
               VerifyQuit(DataFile,FileName,WishToSave,WishToQuit);
               IF WishToSave THEN SaveDataFile(DataFile,FileName,WF,TR,Potl)
            END
      END;
   MainMenu.Done;
   {Dispose(MainMenu); Dispose(MainKeys); {Mac only}
   {Dispose(Potl.V); {Mac only}
   {Dispose(WF.Mag); Dispose(WF.Phase); {Mac only}
   {Dispose(TR.T); Dispose(TR.LogT); Dispose(TR.Energy); {Mac only}
   ClearMUPPETport;
   CUPSdone;
END. {Scattr1D}
