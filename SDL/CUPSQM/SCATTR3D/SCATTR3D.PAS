{Program title: Stationary scattering states in three dimensions     }
{Author: John R. Hiller                                              }
{Version: 1.0              11/14/94                                  }
{Copyright (c) 1995 John Wiley & Sons, Inc.                          }
{                                                                    }
{Program summary:                                                    }
{     Perform partial-wave analysis of scattering from a spherically }
{symmetric potential.  The potential is assumed to be zero beyond a  }
{selectable maximum radius.  The modified radial wave function u=rR  }
{and the associated phase shift are computed.                        }
{                                                                    }
{Library files: none                                                 }
{Utilities: Graph, CUPS, CUPSmupp, CUPSfunc, CUPSproc, CUPSgui,      }
{           CUPSgrph                                                 }
{Other units: Sc3DMach, Sc3DCmn, Sc3DPotl, Sc3DWF, Sc3DPDT           }
{                                                                    }
{Algorithms: Numerov                                                 }
{                                                                    }
PROGRAM Scattr3D(input,output);

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph,
     Sc3DMach,Sc3DCmn,Sc3DPotl,Sc3DWF,Sc3DPDT;

VAR
   CreditScr,AboutMenu,
   AboutProg,AboutPram,
   AboutPotl,
   AboutComp,AboutAlgs   :HelpScrType;
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

{--------Routines for changes to parameters----------------------}

PROCEDURE SetMassAndUnits(VAR Potl :PotlType; VAR Modified :boolean);
BEGIN
   WITH Potl DO
      BEGIN
         SelectMassandUnits(Particle,Mass,Lfac,Munit,Eunit,Lunit,Modified);
         InitializeScales(Potl);
         ComputeZeta(V0,L0,Mass,Eunit,Lunit,Munit,zeta);
         PrepPotl(Potl);
      END;
END; {SetMassandUnits}

PROCEDURE ChangeNumericalParams(VAR Potl :PotlType; VAR Modified :boolean);
VAR
   InputScr :TInputScreen;
   offset   :integer;
   DefrStop :real;
BEGIN
   IF Potl.DefAlg=0 THEN offset:=1
   ELSE
      BEGIN
         offset:=0;
         IF Potl.Alg=0 THEN Potl.Alg:=Potl.DefAlg;   
      END;
   {New(InputScr); {Mac only}                                            
   WITH InputScr DO
      BEGIN
         Modified:=False;
         Init;
         DefineInputPort(0.2,0.825,0.2,0.825);
         LoadLine('');
         LoadLine('      Numerical parameters');
         LoadLine('');
         LoadLine(   '  Choice of integration algorithm:');
         LoadLine(   '     direct integration #1');
         LoadLine(   '        transfer matrix #1  (not installed)');
         IF Potl.DefAlg=0 THEN
            LoadLine('     piecewise constant #1  (not installed)');
         LoadLine('');
         LoadLine('          Number of integration steps: {    }');
         LoadLine('  Range of integration (r_stop/r_max): {      }');
         LoadLine('             Match points (r_1/r_max): {      }');
         LoadLine('                          (r_2/r_max): {      }');
         LoadLine('');
         LoadLine('  [  OK  ]    [Cancel]');
         CASE Potl.Alg OF
            0 :SetRadioButton('1',3);
            1 :SetRadioButton('1',2);
            2 :SetRadioButton('1',1);
            END; {case}
         SetNumber(offset+NumAlgs+1,Potl.nSteps);
         SetNumber(offset+NumAlgs+2,Potl.rStop/Potl.rMax);
         SetNumber(offset+NumAlgs+3,Potl.r1);
         SetNumber(offset+NumAlgs+4,Potl.r2);
         AcceptScreen;
         IF NOT Canceled THEN WITH Potl DO
            BEGIN
               Modified:=True;
               CASE GetRadioButton('1') OF
                  1 :Alg:=2;
                  2 :Alg:=1;
                  3 :Alg:=0;
                  END; {case}
               Alg:=2;   {remove when other algorithms installed}
               nSteps:=round(GetNumber(offset+NumAlgs+1));  
               IF nSteps<1 THEN
                  IntInputWarn('number of steps',nSteps,100,4)
               ELSE IF nSteps>MaxRange div 4 THEN
                  IntInputWarn('number of steps',nSteps,MaxRange div 4,4);
               rStop:=GetNumber(offset+NumAlgs+2);
               dr:=(rMax-rMin)/nSteps; u1:=-uInit*sqr(dr)/6.0;
               IF rMax<(MaxRange-nSteps)*dr THEN DefrStop:=2.0
               ELSE DefrStop:=1.0+(MaxRange-nSteps)*dr/rMax;
               IF rStop<=1.0 THEN
                  RealInputWarn('r_stop/r_max',rStop,DefrStop,6);
               nRSteps:=round(rMax*(rStop-1.0)/dr);
               IF (nSteps+nRSteps>MaxRange div 2) OR (nRSteps<=0) THEN
                  BEGIN
                     RealInputWarn('r_stop/r_max',rStop,
                                   1.0+(MaxRange div 2-nSteps)*dr/rMax,6);
                     nRSteps:=round(rMax*(rStop-1.0)/dr);
                  END;
               A.Free; uFree.Free; u.Free; V.Free;  {resize storage}
               V.Init(nSteps+nRSteps+1);
               u.Init(nSteps+nRSteps+1);
               uFree.Init(nSteps+nRSteps+1);
               A.Init(nSteps+nRSteps+1);
               rStop:=rMax+dr*nRSteps;
               IF HorizMax>L0*rStop THEN HorizMax:=L0*rStop;
               r1:=GetNumber(offset+NumAlgs+3);
               IF (r1<1.0) OR (r1>rStop/rMax) THEN
                  RealInputWarn('r_1/r_max',r1,0.2*rStop/rMax+0.8,6);
               r2:=GetNumber(offset+NumAlgs+4);
               IF (r2<1.0) OR (r2>rStop/rMax) THEN
                  RealInputWarn('r_2/r_max',r2,0.4*rStop/rMax+0.6,6);           
               Saved:=False;
               PrepPotl(Potl);
            END;
         Done;
      END;
   {Dispose(InputScr); {Mac only}
END; {ChangeNumericalParams}

{--------Routines for file management----------------------------------}

PROCEDURE ReadFile(VAR DataFile :text;
                   VAR WF :WFtype; VAR WF3D :WF3Dtype;
                   VAR PS :PStype; VAR DXS :DXStype; VAR TXS :TXStype;
                   VAR Potl :PotlType; VAR Valid :boolean);
VAR
   PotlWarnScr,FileWarnScr,ID   :string;
   n,NrhoRow,NrhoExtra,i,j,jRow :integer;
   ValidPotl,ValidFile   :boolean;
   pName                 :string[8];
   Vers,psiTemp,L02      :real;
BEGIN
   PotlWarnScr:='Unknown potential in file.';
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
                           readln(DataFile,DefParams[n])
                        END;
                     readln(DataFile,rStop);
                     readln(DataFile,rMin,rMax,DefrMin,DefrMax);
                     readln(DataFile,r1,r2);
                     ReadParticle(DataFile,Particle);
                     readln(DataFile,V0); readln(DataFile,Eunit);
                     readln(DataFile,L0); readln(DataFile,Lunit);
                     readln(DataFile,Mass); readln(DataFile,Munit);
                     readln(DataFile,Lfac);
                     readln(DataFile,DefV0); readln(DataFile,DefL0); 
                     readln(DataFile,HorizMin,HorizMax,VertMin,VertMax);
                     readln(DataFile,DXSscale,TXSscale);
                     readln(DataFile,Alg,DefAlg);
                     readln(DataFile,nSteps,nRSteps,DefnSteps);
                     A.Free; uFree.Free; u.Free; V.Free;  {resize storage}
                     V.Init(nSteps+nRSteps+1);
                     u.Init(nSteps+nRSteps+1);
                     uFree.Init(nSteps+nRSteps+1);
                     A.Init(nSteps+nRSteps+1);
                     rStop:=rStop/L0;
                     rMin:=rMin/L0; rMax:=rMax/L0;
                     DefrMin:=DefrMin/L0; DefrMax:=DefrMax/L0;
                     SetPotlScales(IDcode,V0,L0,Invert,rMin,rMax,
                                   Mass,Efac,Lfac,Mfac,
                                   VertMin,VertMax,
                                   Params,DefParams,ParamNames);
                     InitializeScales(Potl);
                     ComputeZeta(V0,L0,Mass,Eunit,Lunit,Munit,zeta);
                     dr:=(rMax-rMin)/nSteps;
                     u1:=-uInit*sqr(dr)/6.0;
                     L02:=sqr(L0);
                     Zoom:=False;
                  END;
               PrepPotl(Potl);
               readln(DataFile,WF.AngMom);
               IF WF.AngMom<0 THEN SetWFDefaults(WF,Potl)
               ELSE WITH WF DO
                  BEGIN
                     readln(DataFile,Energy,PhaseShift);
                     Energy:=Energy/Potl.V0;
                     readln(DataFile,HorizMin,HorizMax,VertMin,VertMax);
                     readln(DataFile,uMax);
                     uFree.Free; u.Free;
                     u.Init(Potl.nSteps+Potl.nRSteps+1);
                     uFree.Init(Potl.nSteps+Potl.nRSteps+1);
                     ReadDVector(DataFile,Potl.nSteps+Potl.nRSteps+1,u,1.0);
                     ReadDVector(DataFile,Potl.nSteps+Potl.nRSteps+1,
                                 uFree,1.0);
                     Computed:=True; Zoom:=False; Plotted:=False;
                     PrepWFPlotScr(WF,Potl);
                  END;
               readln(DataFile,WF3D.AngMomMin,WF3D.AngMomMax,WF3D.PlotType);
               IF WF3D.AngMomMin<0 THEN SetWF3DDefaults(WF3D,Potl)
               ELSE WITH WF3D DO
                  BEGIN
                     readln(DataFile,NzSteps,NrhoSteps,zMax,rhoMax);
                     zMax:=zMax/Potl.L0; rhoMax:=rhoMax/Potl.L0;
                     drho:=rhoMax/NrhoSteps; dz:=zMax/NzSteps;
                     readln(DataFile,Energy);
                     Energy:=Energy/Potl.V0;
                     readln(DataFile,Size,Distance);
                     readln(DataFile,alpha,beta,gamma);
                     Prob.Free; PsiIm.Free; PsiRe.Free;
                     PsiRe.Init(NrhoSteps+1,2*NzSteps);
                     PsiIm.Init(NrhoSteps+1,2*NzSteps);
                     Prob.Init(NrhoSteps+1,2*NzSteps);
                     NrhoExtra:=NrhoSteps MOD 10; NrhoRow:=NrhoSteps DIV 10;
                     FOR i:=-NzSteps TO NzSteps-1 DO
                        BEGIN
                           FOR jRow:=1 TO NrhoRow DO
                              BEGIN
                                 FOR j:=1 TO 10 DO
                                    BEGIN
                                       read(DataFile,psiTemp);
                                       PsiRe.Put(j+10*(jRow-1),
                                                 i+NzSteps+1,psiTemp)
                                    END;
                                 readln(DataFile)
                              END;
                           FOR j:=1 TO NrhoExtra DO
                              BEGIN
                                 read(DataFile,psiTemp);
                                 PsiRe.Put(j+10*NrhoRow,i+NzSteps+1,psiTemp);
                              END;
                           IF NrhoExtra>0 THEN readln(DataFile)
                        END;
                     FOR i:=-NzSteps TO NzSteps-1 DO
                        BEGIN
                           FOR jRow:=1 TO NrhoRow DO
                              BEGIN
                                 FOR j:=1 TO 10 DO
                                    BEGIN
                                       read(DataFile,psiTemp);
                                       PsiIm.Put(j+10*(jRow-1),
                                                 i+NzSteps+1,psiTemp)
                                    END;
                                 readln(DataFile)
                              END;
                           FOR j:=1 TO NrhoExtra DO
                              BEGIN
                                 read(DataFile,psiTemp);
                                 PsiIm.Put(j+10*NrhoRow,i+NzSteps+1,psiTemp);
                              END;
                           IF NrhoExtra>0 THEN readln(DataFile)
                        END;
                     MagSqr(PsiRe,PsiIm,Prob);
                     Computed:=True; Zoom:=False; Plotted:=False; Surf:=True;
                     PrepWF3DPlotScr(WF3D,Potl);
                  END;
               readln(DataFile,PS.AngMom,PS.PlotType);
               IF PS.AngMom<0 THEN SetPSDefaults(PS,Potl)
               ELSE WITH PS DO
                  BEGIN
                     readln(DataFile,Emin,Emax,PSmin,PSmax);
                     readln(DataFile,deltaE);
                     Emin:=Emin/Potl.V0; Emax:=Emax/Potl.V0;
                     deltaE:=deltaE/Potl.V0;
                     readln(DataFile,NumE);
                     ReadDVector(DataFile,NumE,PShift,1.0);
                     ReadDVector(DataFile,NumE,Energy,Potl.V0);
                     Computed:=True; Zoom:=False; Plotted:=False;
                     PrepPSPlotScr(PS,Potl);
                  END;
               readln(DataFile,DXS.AngMomMin,DXS.AngMomMax,DXS.PlotType);
               IF DXS.AngMomMin<0 THEN SetDXSDefaults(DXS,Potl)
               ELSE WITH DXS DO
                  BEGIN
                     readln(DataFile,Energy,dTheta);
                     Energy:=Energy/Potl.V0;
                     readln(DataFile,ThetaMin,ThetaMax,VertMin,VertMax);
                     readln(DataFile,nAngles,Min,Max);
                     Angle.Free; DiffXS.Free;
                     DiffXS.Init(nAngles); Angle.Init(nAngles);
                     ReadDVector(DataFile,nAngles,DiffXS,L02);
                     ReadDVector(DataFile,nAngles,Angle,1.0);
                     Computed:=True; Zoom:=False; Plotted:=False;
                     PrepDXSPlotScr(DXS,Potl)
                  END;
               readln(DataFile,TXS.AngMomMin,TXS.AngMomMax,TXS.PlotType);
               IF TXS.AngMomMin<0 THEN SetTXSDefaults(TXS,Potl)
               ELSE WITH TXS DO
                  BEGIN
                     readln(DataFile,Emin,Emax,VertMin,VertMax);
                     readln(DataFile,deltaE);
                     Emin:=Emin/Potl.V0; Emax:=Emax/Potl.V0;
                     deltaE:=deltaE/Potl.V0;
                     readln(DataFile,NumE,Min,Max);
                     ReadDVector(DataFile,NumE,TotalXS,L02);
                     ReadDVector(DataFile,NumE,Energy,Potl.V0);
                     Computed:=True; Zoom:=False; Plotted:=False;
                     PrepTXSPlotScr(TXS,Potl);
                  END
            END
         ELSE Announce(PotlWarnScr);
      END
   ELSE Announce(FileWarnScr);
   Close(DataFile);
END; {ReadFile}

Procedure WriteFile(var DataFile :text;
                    VAR WF :WFtype; VAR WF3D :WF3Dtype;
                    VAR PS :PStype; VAR DXS :DXStype; VAR TXS :TXStype;
                    VAR Potl :PotlType);
VAR
   n,NrhoRow,NrhoExtra,i,j,jRow :integer;
   L02 :real;
BEGIN
   writeln(DataFile,ProgName);
   writeln(DataFile,ConCat('Version date: ',VersionDate));
   writeln(DataFile,Version :3:1);
   WITH Potl DO
      BEGIN
         L02:=sqr(L0);
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
            END;
         writeln(DataFile,L0*rStop :9:6);
         writeln(DataFile,L0*rMin :9:6,'  ',L0*rMax :9:6,
                          '  ',L0*DefrMin :9:6,'  ',L0*DefrMax :9:6);
         writeln(DataFile,r1 :9:6,'  ',r2 :9:6);
         WriteParticle(DataFile,Particle);
         writeln(DataFile,V0 :9:6); writeln(DataFile,Eunit,'eV');
         writeln(DataFile,L0 :9:6); writeln(DataFile,Lunit,'m');
         writeln(DataFile,Mass :9:6); writeln(DataFile,Munit,'eV/c^2');
         writeln(DataFile,Lfac :9:6);
         writeln(DataFile,DefV0 :9:6); writeln(DataFile,DefL0 :9:6); 
         writeln(DataFile,HorizMin :9:6,'  ',HorizMax :9:6,
                          '  ',VertMin :9:6,'  ',VertMax :9:6);
         writeln(DataFile,DXSscale :9:6,'  ',TXSscale :9:6);
         writeln(DataFile,Alg :2,'  ',DefAlg :2);
         writeln(DataFile,nSteps :4,'  ',nRSteps :4,'  ',DefnSteps :4)
      END;
   IF NOT WF.Computed THEN writeln(DataFile,'-1')
   ELSE WITH WF DO
      BEGIN
         writeln(DataFile,AngMom :2);
         writeln(DataFile,Potl.V0*Energy :9:6,'  ',PhaseShift :9:6);
         writeln(DataFile,HorizMin :9:6,'  ',HorizMax :9:6,
                          '  ',VertMin :9:6,'  ',VertMax :9:6);
         writeln(DataFile,uMax :9:6);
         WriteDVector(DataFile,Potl.nSteps+Potl.nRSteps+1,u,1.0);
         WriteDVector(DataFile,Potl.nSteps+Potl.nRSteps+1,uFree,1.0)
      END;
   IF NOT WF3D.Computed THEN writeln(DataFile,'-1  -1  -1')
   ELSE WITH WF3D DO
      BEGIN
         writeln(DataFile,AngMomMin :2,'  ',AngMomMax :2,'  ',PlotType);
         writeln(DataFile,NzSteps :3,'  ',NrhoSteps :3,
                          '  ',Potl.L0*zMax :9:6,'  ',Potl.L0*rhoMax :9:6);
         writeln(DataFile,Potl.V0*Energy :9:6);
         writeln(DataFile,Size :9:6,'  ',Distance :9:6);
         writeln(DataFile,alpha :9:6,'  ',beta :9:6,'  ',gamma :9:6);
         NrhoExtra:=NrhoSteps MOD 10; NrhoRow:=NrhoSteps DIV 10;
         FOR i:=-NzSteps TO NzSteps-1 DO
            BEGIN
               FOR jRow:=1 TO NrhoRow DO
                  BEGIN
                     FOR j:=1 TO 10 DO
                        write(DataFile,PsiRe.Value(j+10*(jRow-1),
                                                i+NzSteps+1) :6:3,'  ');
                     writeln(DataFile)
                  END;
               for j:=1 to NrhoExtra do
                  write(DataFile,PsiRe.Value(j+10*NrhoRow,
                                             i+NzSteps+1) :6:3,'  ');
               IF NrhoExtra>0 THEN writeln(DataFile);
            END;
         FOR i:=-NzSteps TO NzSteps-1 DO
            BEGIN
               FOR jRow:=1 TO NrhoRow DO
                  BEGIN
                     FOR j:=1 TO 10 DO
                        write(DataFile,PsiIm.Value(j+10*(jRow-1),
                                               i+NzSteps+1) :6:3,'  ');
                     writeln(DataFile)
                  END;
               for j:=1 to NrhoExtra do
                  write(DataFile,PsiIm.Value(j+10*NrhoRow,
                                             i+NzSteps+1) :6:3,'  ');
               IF NrhoExtra>0 THEN writeln(DataFile)
            END
      END;
   IF NOT PS.Computed THEN writeln(DataFile,'-1  -1')
   ELSE WITH PS DO
      BEGIN
         writeln(DataFile,AngMom :2,'  ',PlotType :2);
         writeln(DataFile,Potl.V0*Emin :9:6,'  ',Potl.V0*Emax :9:6,
                          '  ',PSmin :9:6,'  ',PSmax :9:6);
         writeln(DataFile,Potl.V0*deltaE :9:6);
         writeln(DataFile,NumE :4);
         WriteDVector(DataFile,NumE,PShift,1.0);
         WriteDVector(DataFile,NumE,Energy,Potl.V0)
      END;
   IF NOT DXS.Computed THEN writeln(DataFile,'-1  -1  -1')
   ELSE WITH DXS DO
      BEGIN
         writeln(DataFile,AngMomMin :2,'  ',AngMomMax :2,
                          '  ',PlotType :2);
         writeln(DataFile,Potl.V0*Energy :9:6,'  ',dTheta :9:6);
         writeln(DataFile,ThetaMin :9:6,'  ',ThetaMax :9:6,
                          '  ',VertMin :9:6,'  ',VertMax :9:6);
         writeln(DataFile,nAngles :3,'  ',Min :9:6,'  ',Max :9:6);
         WriteDVector(DataFile,nAngles,DiffXS,L02);
         WriteDVector(DataFile,nAngles,Angle,1.0)
      END;
   IF NOT TXS.Computed THEN writeln(DataFile,'-1  -1  -1')
   ELSE WITH TXS DO
      BEGIN
         writeln(DataFile,AngMomMin :2,'  ',AngMomMax :2,
                          '  ',PlotType :2);
         writeln(DataFile,Potl.V0*Emin :9:6,'  ',Potl.V0*Emax :9:6,
                          '  ',VertMin :9:6,'  ',VertMax :9:6);
         writeln(DataFile,Potl.V0*deltaE :9:6);
         writeln(DataFile,NumE :4,'  ',Min :9:6,'  ',Max :9:6);
         WriteDVector(DataFile,NumE,TotalXS,L02);
         WriteDVector(DataFile,NumE,Energy,Potl.V0);
      END;
   Close(DataFile);
END; {WriteFile}

PROCEDURE OpenDataFile(VAR DataFile :text; VAR FileName :string;
                       VAR WF :WFtype; VAR WF3D :WF3Dtype;
                       VAR PS :PStype; VAR DXS :DXStype; VAR TXS :TXStype;
                       VAR Potl :PotlType);
VAR
   WishToAbort,FileExists,Valid :boolean;
BEGIN
   GetFileToOpen(FileName,WishToAbort);
   IF NOT WishToAbort THEN
      BEGIN
         ConnectTextFile(DataFile,FileName,FileExists);
         ReadFile(DataFile,WF,WF3D,PS,DXS,TXS,Potl,Valid);
         IF Valid THEN
            BEGIN
               Potl.Saved:=True;
               IF WF.Computed THEN PlotWaveFunction(WF,Potl)
               ELSE IF WF3D.Computed THEN PlotWF3D(WF3D,Potl,True)
               ELSE IF PS.Computed THEN PlotPhaseShift(PS,Potl)
               ELSE IF DXS.Computed THEN PlotDiffXSect(DXS,Potl)
               ELSE IF TXS.Computed THEN PlotTotalXSect(TXS,Potl)
               ELSE PlotPotential(Potl);
            END;
      END;
END; {OpenDataFile}

Procedure NewFile(VAR FileName :string;
                  VAR WF :WFtype; VAR WF3D :WF3Dtype;
                  VAR PS :PStype; VAR DXS :DXStype; VAR TXS :TXStype;
                  VAR Potl :PotlType);
BEGIN
   FileName:='Scattr3D.dat';        {set default file name}
   WITH Potl DO                     
      BEGIN
         Eunit:=' '; Lunit:='n';    {set units to atomic sizes}
         Particle:=electron;        {set particle to electron}
         Mass:=0.511; Munit:='M';
         nSteps:=DefnSteps; nRSteps:=5*nSteps;
         A.Free; uFree.Free; u.Free; V.Free;  {resize storage}
         V.Init(nSteps+nRSteps+1);
         u.Init(nSteps+nRSteps+1);
         uFree.Init(nSteps+nRSteps+1);
         A.Init(nSteps+nRSteps+1);
         r1:=1.2; r2:=1.4;
      END;
   SetPotlToSphere(Potl);           {set default potential}
   WITH Potl DO                     
      BEGIN
         Alg:=DefAlg;
         Alg:=2;    {remove when other algorithms installed}
      END;
   PlotPotential(Potl);
   SetWFDefaults(WF,Potl);
   SetWF3DDefaults(WF3D,Potl);
   SetPSDefaults(PS,Potl);
   SetDXSDefaults(DXS,Potl);
   SetTXSDefaults(TXS,Potl)
END; {NewFile}

PROCEDURE SaveDataFile(VAR DataFile :text; VAR FileName :string;
                       VAR WF :WFtype; VAR WF3D :WF3Dtype;
                       VAR PS :PStype; VAR DXS :DXStype; VAR TXS :TXStype;
                       VAR Potl :PotlType);
VAR
   WishToAbort,FileExists  :boolean;
BEGIN
   ConnectTextFile(DataFile,FileName,FileExists);
   IF FileExists THEN AskOverWrite(FileName,WishToAbort)
   ELSE WishToAbort:=False;
   IF NOT WishToAbort THEN
      BEGIN
         Rewrite(DataFile);
         WriteFile(DataFile,WF,WF3D,PS,DXS,TXS,Potl);
         Potl.Saved:=True;
      END
   ELSE Close(DataFile);
END; {SaveDataFile}

PROCEDURE SaveAs(VAR DataFile :text; VAR FileName :string;
                 VAR WF :WFtype; VAR WF3D :WF3Dtype;
                 VAR PS :PStype; VAR DXS :DXStype; VAR TXS :TXStype;
                 VAR Potl :PotlType);
VAR
   WishToAbort :boolean;
BEGIN
   GetFileName(FileName,WishToAbort);
   IF NOT WishToAbort THEN
      SaveDataFile(DataFile,FileName,WF,WF3D,PS,DXS,TXS,Potl)
END; {SaveAs}

{--------Routines to prepare and handle main menu and hot keys---------}

PROCEDURE Initialize(VAR FileName :string; VAR WF :WFtype;
                     VAR WF3D :WF3Dtype; VAR PS :PStype;
                     VAR DXS :DXStype; VAR TXS :TXStype;
                     VAR Potl :PotlType);
VAR
   n :integer;
BEGIN
   DefineViewPort(1,0.0,0.25,0.06,0.43);   {viewport for legends}
                                           {and Euler angles}
   DefineScale(1,0.0,1.0,0.0,1.0);         {scale for legends}
   DefineViewPort(2,0.1,0.65,0.47,0.87);   {viewport for plots}
   DefineViewPort(3,0.08,0.96,0.12,0.87);  {viewport for large plots}
   DefineViewPort(4,0.255,0.65,0.08,0.38); {viewport for form of potential}
   DefineViewPort(5,0.7,1.0,0.08,0.92);    {viewport for parameter list}

   WITH Potl DO                            {initialize DVectors}
      BEGIN
         DefnSteps:=100;
         {New(V); New(u); New(uFree); New(A); {Mac only}                
         V.Init(1); u.Init(1); uFree.Init(1); A.Init(1);       
      END;
   WITH WF DO                             
      BEGIN
         {New(u); New(uFree); {Mac only}
         u.Init(1); uFree.Init(1);        
      END;
   WITH WF3D DO
      BEGIN
         {New(PsiRe); New(PsiIm); New(Prob); {Mac only}
         PsiRe.Init(1,1); PsiIm.Init(1,1); Prob.Init(1,1);
      END;
   WITH PS DO
      BEGIN
         {New(PShift); New(Energy); {Mac only}
         PShift.Init(MaxNumE); Energy.Init(MaxNumE);
      END;
   WITH DXS DO
      BEGIN
         {New(DiffXS); New(Angle); {Mac only}
         DiffXS.Init(1); Angle.Init(1);
      END;
   WITH TXS DO
      BEGIN
         {New(TotalXS); New(Energy); {Mac only}
         TotalXS.Init(MaxNumE); Energy.Init(MaxNumE);
      END;

   AboutProg[01]:='';
   AboutProg[02]:='STATIONARY SCATTERING STATES IN THREE DIMENSIONS:';
   AboutProg[03]:='             PARTIAL WAVE ANALYSIS';
   AboutProg[04]:='';
   AboutProg[05]:='                 John R. Hiller';
   AboutProg[06]:='         University of Minnesota, Duluth';
   AboutProg[07]:='';
   AboutProg[08]:=ConCat('                  Version ',NumStr(Version,3,1));
   AboutProg[09]:='         (c) 1995 John Wiley & Sons, Inc.';
   AboutProg[10]:='';
   AboutProg[11]:='     This program performs a partial-wave';
   AboutProg[12]:='  analysis of scattering from a spherically';
   AboutProg[13]:='  symmetric potential.  Radial and three-';
   AboutProg[14]:='  dimensional wave functions can be displayed,';
   AboutProg[15]:='  as can phase shifts, and differential and';
   AboutProg[16]:='  total cross sections.  The analysis employs an';
   AboutProg[17]:='  expansion in the natural angular momentum basis.';
   AboutProg[18]:='  Radial wave functions are computed numerically;';
   AboutProg[19]:='  far from the scattering center they differ from';
   AboutProg[20]:='  free radial wave functions by shifts in phase';
   AboutProg[21]:='  only.  Knowledge of these phase shifts for the';
   AboutProg[22]:='  dominant values of angular momentum is used to';
   AboutProg[23]:='  approximate the cross sections.';
   FOR n:=24 TO 25 DO AboutProg[n]:='';

   FOR n:=1 TO 24 DO CreditScr[n]:=AboutProg[n];
   CreditScr[25]:='  Press <Enter> or click mouse to continue.';

   AboutMenu[01]:='';
   AboutMenu[02]:=' Menu choices:';
   AboutMenu[03]:='';
   AboutMenu[04]:='    File - use files to obtain or save data;';
   AboutMenu[05]:='      information about CUPS and the program;';
   AboutMenu[06]:='      exit from program.';
   AboutMenu[07]:='';
   AboutMenu[08]:='    Parameters - set parameters, including';
   AboutMenu[09]:='      particle type & mass, energy & length units,';
   AboutMenu[10]:='      numerical parameters, and algorithm choice.';
   AboutMenu[11]:='';
   AboutMenu[12]:='    Potential - choose or modify potential.';
   AboutMenu[13]:='';
   AboutMenu[14]:='    Compute - compute wave functions,';
   AboutMenu[15]:='      phase shifts, and differential';
   AboutMenu[16]:='      and total cross sections.';
   AboutMenu[17]:='';
   AboutMenu[18]:='    Help - display various help screens.';
   FOR n:=19 TO 25 DO AboutMenu[n]:='';

   AboutPram[01]:='';
   AboutPram[02]:=' Parameters: set various parameters.';
   AboutPram[03]:='';
   AboutPram[04]:='    * Particle Type & Mass, energy & length Units.';
   AboutPram[05]:='         A dialog box is presented for selection';
   AboutPram[06]:='         of the particle type or a specified';
   AboutPram[07]:='         mass and a choice of units for the';
   AboutPram[08]:='         calculation.';
   AboutPram[09]:='';
   AboutPram[10]:='    * Numerical Parameters, Algorithm.';
   AboutPram[11]:='         An input screen is used to set the';
   AboutPram[12]:='         limits and step size of the integrations,';
   AboutPram[13]:='         and select the match points for the';
   AboutPram[14]:='         phase shift calculation.  A choice of';
   AboutPram[15]:='         algorithm may be made available, if';
   AboutPram[16]:='         installed.';
   FOR n:=17 TO 25 DO AboutPram[n]:='';

   AboutPotl[01]:='';
   AboutPotl[02]:=' Potential: choose or modify potential.  ';
   AboutPotl[03]:='';
   AboutPotl[04]:='    * Display & Modify Current Choice,';
   AboutPotl[05]:='      and select range and step size.';
   AboutPotl[06]:='';
   AboutPotl[07]:='    * Choose & Modify one of the following:';
   AboutPotl[08]:='        > Power-Law';
   AboutPotl[09]:='        > Yukawa';
   AboutPotl[10]:='        > Gaussian';
   AboutPotl[11]:='        > Lennard-Jones';
   AboutPotl[12]:='        > Woods-Saxon';
   AboutPotl[13]:='        > Spherical Shells';
   AboutPotl[14]:='        > Hard Sphere';
   AboutPotl[15]:='        > User-Defined';
   AboutPotl[16]:='';
   AboutPotl[17]:='      The user-defined potential should';
   AboutPotl[18]:='      be changed in the code to the';
   AboutPotl[19]:='      desired form before use.';
   FOR n:=20 TO 25 DO AboutPotl[n]:='';

   AboutComp[01]:='';
   AboutComp[02]:=' Compute: plot previous result, if any, and';
   AboutComp[03]:='          compute new results, if desired,';
   AboutComp[04]:='          in the chosen category.  Except for';
   AboutComp[05]:='          the 3D wave function, previous results';
   AboutComp[06]:='          are retained until a plot option is';
   AboutComp[07]:='          changed.';
   AboutComp[08]:='';
   AboutComp[09]:='    * Radial Wave Functions';
   AboutComp[10]:='';
   AboutComp[11]:='    * 3D Wave Function - provides a surface or';
   AboutComp[12]:='          contour plot of partial wave sum.';
   AboutComp[13]:='';
   AboutComp[14]:='    * Phase Shifts - computed for a range of';
   AboutComp[15]:='          energies.';
   AboutComp[16]:='';
   AboutComp[17]:='    * Differential Cross Sections - partial wave';
   AboutComp[18]:='          sum vs angle.';
   AboutComp[19]:='';
   AboutComp[20]:='    * Total Cross Sections - partial wave sum';
   AboutComp[21]:='          vs energy.';
   FOR n:=22 TO 25 DO AboutComp[n]:='';

   AboutAlgs[01]:='';
   AboutAlgs[02]:=' One algorithm is included.  It is based on';
   AboutAlgs[03]:=' Numerov integration of the radial Schrodinger';
   AboutAlgs[04]:=' equation.  A stub for a second algorithm is';
   AboutAlgs[05]:=' provided.  It is assumed to be the transfer';
   AboutAlgs[06]:=' matrix approach.  [See T.M Kalotas, A.R. Lee,';
   AboutAlgs[07]:=' and V.E. Howard, Am. J. Phys. 59, 225 (1991).]';
   AboutAlgs[08]:=' It can also be used in a special form for';
   AboutAlgs[09]:=' piecewise constant potentials, where it gives';
   AboutAlgs[10]:=' essentially exact results.  This special';
   AboutAlgs[11]:=' form will be much faster than the others.';
   AboutAlgs[12]:=' Lines in the code that set "Alg:=2" must be';
   AboutAlgs[13]:=' removed to make any new algorithms accessible.';
   AboutAlgs[14]:='';
   AboutAlgs[15]:=' The free wave function is matched at points r_1';
   AboutAlgs[16]:=' and r_2 to determine the phase shift.';
   FOR n:=17 TO 25 DO AboutAlgs[n]:='';

   NewFile(FileName,WF,WF3D,PS,DXS,TXS,Potl);
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
            Row(2,1,'Particle Type & Mass, Units');
            Row(2,2,'Numerical Parameters, Algorithm');
         Column(3,'Potential');
            Row(3,1,'Display & Modify Current Choice');
            Row(3,2,'Choose & Modify: Power-Law');
            Row(3,3,'                 Yukawa');
            Row(3,4,'                 Gaussian');
            Row(3,5,'                 Lennard-Jones');
            Row(3,6,'                 Woods-Saxon');
            Row(3,7,'                 Spherical Shells');
            Row(3,8,'                 Hard Sphere');
            Row(3,9,'                 User-Defined');
         Column(4,'Compute');
            Row(4,1,'Radial Wave Functions');
            Row(4,2,'3D Wave Function');
            Row(4,3,'Phase Shifts');
            Row(4,4,'Differential Cross Sections');
            Row(4,5,'Total Cross Sections');
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

PROCEDURE HandleMain(VAR MainMenu :TMenu; VAR MainKeys :THotKeys;
                     VAR DataFile :text; VAR FileName :string;
                     VAR WF :WFtype; VAR WF3D :WF3Dtype;
                     VAR PS :PStype; VAR DXS :DXStype; VAR TXS :TXStype;
                     VAR Potl :PotlType; VAR WishToQuit :boolean);
{Process selection from main menu.}
CONST
   Ffile=1; Parameters=2; Potential=3; Compute=4; Hhelp=5;
VAR
   Verified,Modified,MenuClicked :boolean;
BEGIN
   MainKeys.Clear;
   WITH MainMenu DO
      REPEAT
         MenuClicked:=False; Modified:=False;
         CASE colChosen OF
            Ffile :case rowChosen OF
                      1 :AboutCUPS;
                      2 :Help(AboutProg);
                      3 :Configuration;
                      4 :BEGIN
                            VerifyNew(Potl.Saved,Verified);
                            IF Verified THEN
                               NewFile(FileName,WF,WF3D,PS,DXS,TXS,Potl)
                         END;
                      5 :BEGIN
                            VerifyNew(Potl.Saved,Verified);
                            IF Verified THEN
                               OpenDataFile(DataFile,FileName,
                                            WF,WF3D,PS,DXS,TXS,Potl)
                         END;
                      6 :SaveDataFile(DataFile,FileName,
                                      WF,WF3D,PS,DXS,TXS,Potl);
                      7 :SaveAs(DataFile,FileName,
                                WF,WF3D,PS,DXS,TXS,Potl);
                      8 :WishToQuit:=True
                      END; {case rowChosen}
            Parameters :case rowChosen OF
                           1 :SetMassAndUnits(Potl,Modified);
                           2 :ChangeNumericalParams(Potl,Modified);
                           END; {case rowChosen}
            Potential :BEGIN
                          IF rowChosen>1 THEN
                             BEGIN
                                CASE rowChosen OF
                                   2 :SetPotlToPowerLaw(Potl);
                                   3 :SetPotlToYukawa(Potl);
                                   4 :SetPotlToGaussian(Potl);
                                   5 :SetPotlToLennardJones(Potl);
                                   6 :SetPotlToWoodsSaxon(Potl);
                                   7 :SetPotlToSphere(Potl);
                                   8 :SetPotlToHardSphere(Potl);
                                   9 :SetPotlToUserDefined(Potl);
                                   END; {case rowChosen}
                                Modified:=True;
                             END;
                          DisplayPotential(Potl,MainMenu,
                                           MenuClicked,Modified);
                       END;
            Compute :case rowChosen OF
                        1 :ComputeWF(WF,Potl,MainMenu,MenuClicked);
                        2 :ComputeWF3D(WF3D,Potl,MainMenu,MenuClicked);
                        3 :ComputePS(PS,Potl,MainMenu,MenuClicked);
                        4 :ComputeDXS(DXS,Potl,MainMenu,MenuClicked);
                        5 :ComputeTXS(TXS,Potl,MainMenu,MenuClicked);
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
         IF Modified THEN
            BEGIN
               WF.Computed:=False;
               WF3D.Computed:=False;
               PS.Computed:=False;
               DXS.Computed:=False;
               TXS.Computed:=False
            END;
      UNTIL NOT MenuClicked;
   MainKeys.Display;
END; {HandleMain}

PROCEDURE HandleHotKeys(VAR MainMenu :TMenu;
                        VAR MainKeys :THotKeys; Mkey :byte;
                        VAR DataFile :text; VAR FileName :string;
                        VAR WF :WFtype; VAR WF3D :WF3Dtype;
                        VAR PS :PStype; VAR DXS :DXStype; VAR TXS :TXStype;
                        VAR Potl :PotlType; VAR WishToQuit :boolean);
{Process selection from main hot keys.}
BEGIN
   MainKeys.Clear;
   CASE Mkey OF
      1 :Help(AboutMenu);
      2 :BEGIN
            CheckForEvents;
            IF MainMenu.Chosen THEN 
               HandleMain(MainMenu,MainKeys,DataFile,FileName,
                          WF,WF3D,PS,DXS,TXS,Potl,WishToQuit);
         END;
      END; {case}
   MainKeys.Display;
END; {HandleHotKeys}

{--------Main program--------------------------------------------------}

BEGIN {Scattr3D}
   CUPSinit;
   ClearMUPPETport;
   ProgName:='Scattr3D'; VersionDate:='November 14, 1994'; Version:=1.0;
   cPi:=Pi; MachEpsY12:=Pwr(MachEps,1/12);
   {New(MainMenu); New(MainKeys); {Mac only}
   PrepMain(MainMenu,MainKeys);
   Initialize(FileName,WF,WF3D,PS,DXS,TXS,Potl);
   Help(CreditScr);
   WishToQuit:=False;
   WHILE NOT WishToQuit DO
      BEGIN
         CheckForEvents;
         IF Event.ReadKey=Escape THEN WishToQuit:=True
         ELSE IF MainKeys.Pressed(Mkey) THEN
            HandleHotKeys(MainMenu,MainKeys,Mkey,DataFile,FileName,
                          WF,WF3D,PS,DXS,TXS,Potl,WishToQuit)
         ELSE IF MainMenu.Activated THEN
            HandleMain(MainMenu,MainKeys,DataFile,FileName,
                       WF,WF3D,PS,DXS,TXS,Potl,WishToQuit);
         IF WishToQuit AND (NOT Potl.Saved) THEN
            BEGIN
               VerifyQuit(DataFile,FileName,WishToSave,WishToQuit);
               IF WishToSave THEN
                  SaveDataFile(DataFile,FileName,WF,WF3D,PS,DXS,TXS,Potl);
            END;
      END;
   MainMenu.Done;
   {Dispose(MainMenu); Dispose(MainKeys); {Mac only}
   {Dispose(Potl.V); {Mac only}
   {Dispose(Potl.u); Dispose(Potl.uFree); Dispose(Potl.A); {Mac only}
   {Dispose(WF.u); Dispose(WF.uFree); {Mac only}
   {Dispose(WF3D.PsiRe); Dispose(WF3D.PsiIm); Dispose(WF3D.Prob); {Mac only}
   {Dispose(PS.PShift); Dispose(PS.Energy); {Mac only}
   {Dispose(DXS.DiffXS); Dispose(DXS.Angle); {Mac only}
   {Dispose(TXS.TotalXS); Dispose(TXS.Energy); {Mac only}
   ClearMUPPETport;
   CUPSdone;
END. {Scattr3D}
