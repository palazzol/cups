{Unit of routines for file management and playback.                  }
{Used by Hatom.pas.                                                  }
{                                                                    }
{Author: John R. Hiller                                              }
{Version: 1.0       7/24/95                                          }
{Copyright (c) 1995  John Wiley & Sons, Inc.                         }
{                                                                    }
UNIT HatmFile;

INTERFACE

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph,
     HatmMach,HatmCalc,HatmPlot;

CONST
   NumPerRow=7;

VAR
   ProgName      :string[8];
   VersionDate   :string;
   Version       :real;

PROCEDURE VerifyNew(Saved :boolean; VAR Verified :boolean);
PROCEDURE VerifyQuit(VAR FileDesc :FileDescType;
                     VAR WishToSave,WishToQuit :boolean);
PROCEDURE WriteFile(var DataFile :text;
                    VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE OpenDataFile(VAR FileDesc :FileDescType;
                       VAR WF :WFtype; VAR Potl :PotlType;
                       VAR WishToAbort :boolean);
PROCEDURE RewriteRecFile(VAR FileDesc :FileDescType;
                         VAR WishToAbort :boolean);
PROCEDURE ResetRecFile(VAR FileDesc :FileDescType; VAR WF :WFtype;
                       VAR Potl :PotlType; VAR WishToAbort :boolean);
PROCEDURE NewFile(VAR FileDesc :FileDescType;
                  VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE SaveDataFile(VAR FileDesc :FileDescType;
                       VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE SaveAs(VAR FileDesc :FileDescType;
                 VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE PlayBack(VAR FileDesc :FileDescType;
                   VAR WF :WFtype; VAR Potl :PotlType;
                   VAR MainMenu :TMenu; VAR MenuClicked :boolean);

IMPLEMENTATION

PROCEDURE GetFileToOpen(extension :Str3; VAR FileName :string;
                        VAR WishToAbort :boolean);
VAR
   tempFN :string;
BEGIN
   tempFN:=OpenFile(ConCat('*.',extension));
   SetColor(Whitte);
   IF length(tempFN)=0 THEN WishToAbort:=True
   ELSE
      BEGIN
         WishToAbort:=False;
         FileName:=tempFN
      END
END; {GetFileToOpen}

PROCEDURE PrepRecFileName(VAR FileDesc :FileDescType);
VAR
   i :integer;
BEGIN
   WITH FileDesc DO
      BEGIN
         i:=0;
         REPEAT i:=i+1 UNTIL (i>Length(FileName)) OR (FileName[i]='.');
         RecFileName:=ConCat(Copy(FileName,1,i-1),'.rec');
      END;
END; {PrepRecFileName}

PROCEDURE ReadDVector(VAR DataFile :text;
                      Num :integer; VAR Vec :DVector; divisor :real);
VAR
   nExtra,nRow,i,iRow :integer;
   temp :real;
BEGIN
   nExtra:=Num MOD NumPerRow; nRow:=Num DIV NumPerRow;
   FOR iRow:=1 TO nRow DO
      BEGIN
         FOR i:=1 TO NumPerRow DO
            BEGIN
               read(DataFile,temp);
               Vec.Put(i+NumPerRow*(iRow-1),temp/divisor)
            END;
         readln(DataFile)
      END;
   FOR i:=1 TO nExtra DO
      BEGIN
         read(DataFile,temp);
         Vec.Put(i+NumPerRow*nRow,temp/divisor)
      END;
   IF nExtra>0 THEN readln(DataFile)
END; {ReadDVector}

PROCEDURE WriteDVector(VAR DataFile :text;
                       Num :integer; VAR Vec :DVector; multiplier :real);
VAR
   nExtra,nRow,i,iRow :integer;
BEGIN
   nExtra:=Num MOD NumPerRow; nRow:=Num DIV NumPerRow;
   FOR iRow:=1 TO nRow DO
      BEGIN
         FOR i:=1 TO NumPerRow DO
            write(DataFile,
                  multiplier*Vec.Value(i+NumPerRow*(iRow-1)) :9:6,'  ');
         writeln(DataFile);
      END;
   FOR i:=1 TO nExtra DO
      write(DataFile,multiplier*Vec.Value(i+NumPerRow*nRow) :9:6,'  ');
   IF nExtra>0 THEN writeln(DataFile);
END; {WriteDVector}

PROCEDURE ReadParticle(VAR DataFile :text; VAR Particle :ParticleType);
VAR
   name :string;
BEGIN
   readln(DataFile,name);
   IF name='electron' THEN Particle:=electron
   ELSE IF name='muon' THEN Particle:=muon
   ELSE IF name='tau' THEN Particle:=tau
   ELSE Particle:=custom;
END; {ReadParticle}

PROCEDURE WriteParticle(VAR DataFile :text; Particle :ParticleType);
BEGIN
   CASE Particle OF
      electron :writeln(DataFile,'electron');
      muon     :writeln(DataFile,'muon');
      tau      :writeln(DataFile,'tau');
      custom   :writeln(DataFile,'custom');
      END; {case}
END; {WriteParticle}

PROCEDURE VerifyNew(Saved :boolean; VAR Verified :boolean);
{Verify decision to start new calculation.}
VAR
   NewScr :TInputScreen;
BEGIN
   {New(NewScr); {Mac only}
   IF Saved THEN Verified:=True
   ELSE WITH NewScr DO
      BEGIN
         Init;
         DefineInputPort(0.12,0.82,0.35,0.65);
         LoadLine('');
         LoadLine('  Current results not saved.');
         LoadLine('  Should they be overwritten and lost?');
         LoadLine('');
         LoadLine('    [  OK  ]    [Cancel]');
         AcceptScreen;
         IF Canceled THEN Verified:=False
         ELSE Verified:=True;
         Done;
      END;
   {Dispose(NewScr); {Mac only}
END; {VerifyNew}

PROCEDURE VerifyQuit(VAR FileDesc :FileDescType;
                     VAR WishToSave,WishToQuit :boolean);
VAR
   QuitScr :TInputScreen;
BEGIN
   {New(QuitScr);  {Mac only}
   WITH QuitScr DO
      BEGIN
         Init;
         DefineInputPort(0.12,0.82,0.35,0.6);
         LoadLine('');
         LoadLine('  Current results not saved.');
         LoadLine('');
         LoadLine('    [  OK  ]  [ Save ]  [Cancel]');
         AcceptScreen;
         WishToSave:=False;
         IF Canceled THEN
            WishToQuit:=False
         ELSE IF GetBoolean(2) THEN
            BEGIN
               Done;
               Init;
               DefineInputPort(0.12,0.82,0.35,0.6);
               LoadLine('');
               LoadLine(
                  ConCat('  Save results in file ',FileDesc.FileName,'?'));
               LoadLine('');
               LoadLine('     [  OK  ]    [Cancel]');
               AcceptScreen;
               IF NOT Canceled THEN WishToSave:=True
               ELSE WishToQuit:=False;
            END;
         Done;
      END;
   {Dispose(QuitScr); {Mac only}
END; {VerifyQuit}

PROCEDURE ReadFile(VAR FileDesc :FileDescType;
                   VAR WF :WFtype; VAR Potl :PotlType;
                   VAR ValidFile :boolean);
VAR
   nn,NrhoRow,NrhoExtra,i,j,jRow,
   iComputed,iRecordToFile,
   iRecordVeff,iAlgorithm,iMode :integer;
   pName         :string[8];
   Vers,psiTemp  :real;
   Enough        :boolean;
BEGIN
   readln(FileDesc.DataFile,pName);
   readln(FileDesc.DataFile);
   readln(FileDesc.DataFile,Vers);
   ValidFile:=True;
   FOR nn:=1 TO Length(pName) DO
      IF pName[nn]<>ProgName[nn] THEN ValidFile:=False;
   IF round(10*Vers)<>round(10*Version) THEN ValidFile:=False;
   IF ValidFile THEN WITH FileDesc DO
      BEGIN
         WITH Potl DO
            BEGIN
               ReadParticle(DataFile,Particle);
               readln(DataFile,V0); readln(DataFile,Eunit);
               readln(DataFile,L0); readln(DataFile,Lunit);
               SetpzScale(L0,Lunit,pz0,pzUnit);
               readln(DataFile,Mass); readln(DataFile,Munit);
               readln(DataFile,charge);
               readln(DataFile,Z1);
               readln(DataFile,m1,m2);
               readln(DataFile,Efac,Mfac,Lfac);
               readln(DataFile,rMax);
               readln(DataFile,rhoMax,zMax);
               readln(DataFile,nSteps,NrhoSteps,NzSteps);
               rMax:=rMax/L0;
               rhoMax:=rhoMax/L0; zMax:=zMax/L0;
               dr:=rMax/nSteps;
               drho:=rhoMax/(NrhoSteps-0.5); dz:=zMax/NzSteps;
               readln(DataFile,Efield,dEfield,MinEfield,MaxEfield);
               readln(DataFile,Bfield,dBfield,MinBfield,MaxBfield);
               readln(DataFile,ExtCharge,dExtCh,MinExtCh,MaxExtCh);
               readln(DataFile,Separation,dSep,MinSep,MaxSep);
               Separation:=Separation/L0; dSep:=dSep/L0;
               MinSep:=MinSep/L0; MaxSep:=MaxSep/L0;
               readln(DataFile,NautoSteps,NautoRemain);
            END;
         readln(DataFile,WF.sPlane);
         IF WF.sPlane<0 THEN SetDefaults(WF,Potl)
         ELSE WITH WF,Potl DO
            BEGIN
               readln(DataFile,iComputed);
               CASE iComputed OF
                  0 :Computed:=NotDone;
                  1 :Computed:=Undistorted;
                  2 :Computed:=Distorted;
                  3 :Computed:=Shared;
                  4 :Computed:=Binding;
                  END; {case}
               IF Computed=Shared THEN mu:=Mass
               ELSE mu:=ReducedMass(Mass,m1,Munit);
               ComputeZeta(Potl);
               readln(DataFile,iRecordToFile,iRecordVeff);
               IF iRecordToFile=1 THEN RecordToFile:=True
               ELSE RecordToFile:=False;
               IF RecordToFile THEN PrepRecFilename(FileDesc);
               IF iRecordVeff=1 THEN RecordVeff:=True
               ELSE RecordVeff:=False;
               ResumeLanczos:=False; ResumeStep:=False;
               readln(DataFile,iAlgorithm,iMode);
               CASE iAlgorithm OF
                  1 :Algorithm:=Basis;
                  2 :Algorithm:=Lanczos;
                  END; {case}
               CASE iMode OF
                  1 :Mode:=Manual;
                  2 :Mode:=Auto;
                  END; {case}
               readln(DataFile,n,l,m,nr);
               readln(DataFile,nMax,lMax);
               readln(DataFile,Level,MaxNumIter,NumIter);
               readln(DataFile,s1Max,s2Max);
               s1Max:=s1Max/L0; s2Max:=s2Max/L0;
               readln(DataFile,Size,Distance);
               readln(DataFile,alfa,beta,gamma);
               readln(DataFile,VertMin,VertMax);
               readln(DataFile,PrhoMax,PzMax);
               PrhoMax:=PrhoMax/L0; PzMax:=PzMax/L0;
               readln(DataFile,En,tolerance);
               En:=En/V0; tolerance:=tolerance/V0;
               readln(DataFile,s1Label);
               readln(DataFile,s2Label);
               IF Computed IN [Undistorted,Binding] THEN
                  BEGIN
                     Rnl.Free; Rnl.Init(Potl.nSteps+1);
                     ReadDVector(DataFile,Potl.nSteps+1,Rnl,1.0);
                  END;
               IF RecordVeff THEN
                  BEGIN
                     readln(DataFile,EnAsymp);
                     EnAsymp:=EnAsymp/V0;
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
                     Veff.Free; Veff.Init(NautoSteps);
                     ReadDVector(DataFile,NautoSteps,Veff,V0);
                     NstepsRemain:=0;
                  END
               ELSE IF (Computed=Distorted) AND (Mode=Auto) THEN
                  BEGIN
                     readln(DataFile,DipoleVal,DipoleMin,DipoleMax);
                     DipoleVal:=DipoleVal/pz0;
                     Dipole.Free; Dipole.Init(NautoSteps+1);
                     ReadDVector(DataFile,NautoSteps+1,Dipole,pz0);
                     NstepsRemain:=0;
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
               PrepStorage(WF,Potl,Enough);
               IF Computed<>Undistorted THEN
                  BEGIN
                     NrhoExtra:=NrhoSteps MOD 10; NrhoRow:=NrhoSteps DIV 10;
                     FOR i:=-NzSteps TO NzSteps DO
                        BEGIN
                           FOR jRow:=1 TO NrhoRow DO
                              BEGIN
                                 FOR j:=1 TO 10 DO
                                    BEGIN
                                       read(DataFile,psiTemp);
                                       Psi.Put(j+10*(jRow-1),
                                               i+NzSteps+1,psiTemp);
                                    END;
                                 readln(DataFile);
                              END;
                           FOR j:=1 TO NrhoExtra DO
                              BEGIN
                                 read(DataFile,psiTemp);
                                 Psi.Put(j+10*NrhoRow,i+NzSteps+1,psiTemp);
                              END;
                           IF NrhoExtra>0 THEN readln(DataFile);
                        END;
                  END;
               Zoom:=False; Plotted:=False; Surf:=False; Cwarn:=False;
               CASE Computed OF
                  NotDone     :{no-op};
                  Undistorted :PrepUWFPlotScr(WF,Potl);
                  Distorted   :PrepDWFPlotScr(FileDesc,WF,Potl);
                  Shared      :PrepSWFPlotScr(FileDesc,WF,Potl);
                  Binding     :PrepBWFPlotScr(WF,Potl);
                  END; {case}
               PrepPsiSlice(WF,Potl);
            END;
      END
   ELSE
      BEGIN
         Announce('Data file not appropriate for program.');
         FileDesc.FileName:=FileDesc.DefFileName;
      END;
   Close(FileDesc.DataFile);
END; {ReadFile}

Procedure WriteFile(var DataFile :text;
                    VAR WF :WFtype; VAR Potl :PotlType);
VAR
   n,NrhoRow,NrhoExtra,i,j,jRow,
   iComputed,iRecordToFile,
   iRecordVeff,iAlgorithm,iMode    :integer;
BEGIN
   writeln(DataFile,ProgName);
   writeln(DataFile,ConCat('Version date: ',VersionDate));
   writeln(DataFile,Version :3:1);
   WITH Potl DO
      BEGIN
         WriteParticle(DataFile,Particle);
         writeln(DataFile,V0 :9:6); writeln(DataFile,Eunit,'eV');
         writeln(DataFile,L0 :9:6); writeln(DataFile,Lunit,'m');
         writeln(DataFile,Mass :9:6); writeln(DataFile,Munit,'eV/c^2');
         writeln(DataFile,charge :9:6);
         writeln(DataFile,Z1 :9:6);
         writeln(DataFile,m1 :9:6,'  ',m2 :9:6);
         writeln(DataFile,Efac :9:6,'  ',Mfac :9:6,'  ',Lfac :9:6);
         writeln(DataFile,L0*rMax :9:6);
         writeln(DataFile,L0*rhoMax :9:6,'  ',L0*zMax :9:6);
         writeln(DataFile,nSteps :3,'  ',NrhoSteps :3,'  ',NzSteps :3);
         writeln(DataFile,Efield :9:6,'  ',dEfield :9:6,
                          '  ',MinEfield :9:6,'  ',MaxEfield :9:6);
         writeln(DataFile,Bfield :9:6,'  ',dBfield :9:6,
                          '  ',MinBfield :9:6,'  ',MaxBfield :9:6);
         writeln(DataFile,ExtCharge :9:6,'  ',dExtCh :9:6,
                          '  ',MinExtCh :9:6,'  ',MaxExtCh :9:6);
         writeln(DataFile,L0*Separation :9:6,'  ',L0*dSep :9:6,
                          '  ',L0*MinSep :9:6,'  ',L0*MaxSep :9:6);
         writeln(DataFile,NautoSteps :3,'  ',NautoRemain :3);
      END;
   IF WF.Computed=NotDone THEN writeln(DataFile,'-1')
   ELSE WITH WF,Potl DO
      BEGIN
         writeln(DataFile,sPlane :2);
         CASE Computed OF
            NotDone     :writeln(DataFile,'0');
            Undistorted :writeln(DataFile,'1');
            Distorted   :writeln(DataFile,'2');
            Shared      :writeln(DataFile,'3');
            Binding     :writeln(DataFile,'4');
            END; {case}
         IF RecordToFile THEN iRecordToFile:=1 ELSE iRecordToFile:=0;
         IF RecordVeff THEN iRecordVeff:=1 ELSE iRecordVeff:=0;
         writeln(DataFile,iRecordToFile :2,'  ',iRecordVeff :2);
         CASE Algorithm OF
            Basis   :iAlgorithm:=1;
            Lanczos :iAlgorithm:=2;
            END; {case}
         CASE Mode OF
            Manual :iMode:=1;
            Auto   :iMode:=2;
            END; {case}
         writeln(DataFile,iAlgorithm :2,'  ',iMode :2);
         writeln(DataFile,n :3,'  ',l :3,'  ',m :3,'  ',nr :3);
         writeln(DataFile,nMax :3,'  ',lMax :3);
         writeln(DataFile,Level :2,'  ',MaxNumIter :3,'  ',NumIter :2);
         writeln(DataFile,L0*s1Max :9:6,'  ',L0*s2Max :9:6);
         writeln(DataFile,Size :9:6,'  ',Distance :9:6);
         writeln(DataFile,alfa :9:6,'  ',beta :9:6,'  ',gamma :9:6);
         writeln(DataFile,VertMin :9:6,'  ',VertMax :9:6);
         writeln(DataFile,L0*PrhoMax :9:6,'  ',L0*PzMax :9:6);
         writeln(DataFile,V0*En :9:6,'  ',V0*tolerance :9:6);
         writeln(DataFile,s1Label);
         writeln(DataFile,s2Label);
         IF Computed IN [Undistorted,Binding] THEN
            WriteDVector(DataFile,Potl.nSteps+1,Rnl,1.0);
         IF RecordVeff THEN
            BEGIN
               writeln(DataFile,V0*EnAsymp :9:6);
               WriteDVector(DataFile,NautoSteps,Veff,Potl.V0);
            END
         ELSE IF (Computed=Distorted) AND (Mode=Auto) THEN
            BEGIN
               writeln(DataFile,pz0*DipoleVal :9:6,'  ',
                       DipoleMin :9:6,'  ',DipoleMax :9:6);
               WriteDVector(DataFile,NautoSteps+1,Dipole,pz0);
            END;
         IF Computed<>Undistorted THEN
            BEGIN
               NrhoExtra:=NrhoSteps MOD 10; NrhoRow:=NrhoSteps DIV 10;
               FOR i:=-NzSteps TO NzSteps DO
                  BEGIN
                     FOR jRow:=1 TO NrhoRow DO
                        BEGIN
                           FOR j:=1 TO 10 DO
                              write(DataFile,Psi.Value(j+10*(jRow-1),
                                                  i+NzSteps+1) :6:3,'  ');
                           writeln(DataFile);
                        END;
                     FOR j:=1 TO NrhoExtra DO
                        write(DataFile,Psi.Value(j+10*NrhoRow,
                                              i+NzSteps+1) :6:3,'  ');
                     IF NrhoExtra>0 THEN writeln(DataFile);
                  END;
            END;
      END;
   Close(DataFile);
END; {WriteFile}

PROCEDURE OpenDataFile(VAR FileDesc :FileDescType;
                       VAR WF :WFtype; VAR Potl :PotlType;
                       VAR WishToAbort :boolean);
VAR
   FileExists,ValidFile :boolean;
BEGIN
   WITH FileDesc DO
      BEGIN
         GetFileToOpen('dat',FileName,WishToAbort);
         IF NOT WishToAbort THEN
            BEGIN
               ConnectTextFile(DataFile,FileName,FileExists);
               ReadFile(FileDesc,WF,Potl,ValidFile);
               IF ValidFile THEN WF.Saved:=True
               ELSE WishToAbort:=True;
            END;
      END;
END; {OpenDataFile}

PROCEDURE RewriteRecFile(VAR FileDesc :FileDescType;
                         VAR WishToAbort :boolean);
VAR
   FileExists   :boolean;
BEGIN
   IF NOT WishToAbort THEN WITH FileDesc DO
      BEGIN
         PrepRecFileName(FileDesc);
         ConnectRealFile(RecFile,RecFileName,FileExists);
         rewrite(RecFile);
      END;
END; {RewriteRecFile}

PROCEDURE ResetRecFile(VAR FileDesc :FileDescType; VAR WF :WFtype;
                       VAR Potl :PotlType; VAR WishToAbort :boolean);
VAR
   FileExists :boolean;
BEGIN
   IF NOT WishToAbort THEN WITH FileDesc DO
      BEGIN
         ConnectRealFile(RecFile,RecFileName,FileExists);
         IF NOT FileExists THEN 
            BEGIN
               Announce(
                  ConCat('File ',RecFileName,' of recordings not found.'));
               WishToAbort:=True;
               Close(RecFile);
            END
         ELSE WITH WF,Potl DO
            BEGIN
               NautoRemain:=NautoSteps;
               IF RecordVeff THEN NautoRemain:=NautoRemain-1;
            END;
      END;
END; {ResetRecFile}

PROCEDURE NewFile(VAR FileDesc :FileDescType;
                  VAR WF :WFtype; VAR Potl :PotlType);
BEGIN
   FileDesc.DefFileName:='Hatom.dat';  {set default file names}
   FileDesc.FileName:=FileDesc.DefFileName;
   PrepRecFileName(FileDesc);
   SetDefaults(WF,Potl);
   WF.NumIter:=0;
   ClearMUPPETport;
END; {NewFile}

PROCEDURE SaveDataFile(VAR FileDesc :FileDescType;
                       VAR WF :WFtype; VAR Potl :PotlType);
VAR
   WishToAbort,FileExists  :boolean;
   OverWriteScr :TInputScreen;
BEGIN
   {New(OverWriteScr); {Mac only}
   WITH FileDesc DO
      BEGIN
         ConnectTextFile(DataFile,FileName,FileExists);
         IF FileExists THEN WITH OverWriteScr DO
            BEGIN
               Init;
               DefineInputPort(0.12,0.82,0.4,0.65);
               LoadLine('');
               LoadLine(
                  ConCat('   File ',FileName,' already exists.'));
               LoadLine( '   Overwrite?');
               LoadLine('');
               LoadLine( '        [  OK  ]      [Cancel]');
               LoadLine('');
               AcceptScreen;
               IF Canceled THEN WishToAbort:=True
               else WishToAbort:=False;
               Done;
            END
         ELSE WishToAbort:=False;
         IF NOT WishToAbort THEN
            BEGIN
               Rewrite(DataFile);
               WriteFile(DataFile,WF,Potl);
               IF NOT WF.RecordToFile THEN
                  BEGIN
                     PrepRecFileName(FileDesc);
                     ConnectRealFile(RecFile,RecFileName,FileExists);
                     rewrite(RecFile); Erase(RecFile);
                  END;
               WF.Saved:=True;
            END
         ELSE Close(DataFile);
      END;
   {Dispose(OverWriteScr); {Mac only}
END; {SaveDataFile}

PROCEDURE SaveAs(VAR FileDesc :FileDescType;
                 VAR WF :WFtype; VAR Potl :PotlType);
VAR
   WishToAbort :boolean;
BEGIN
   GetFileName(FileDesc.FileName,WishToAbort);
   IF NOT WishToAbort THEN SaveDataFile(FileDesc,WF,Potl)
END; {SaveAs}

PROCEDURE AskAboutPlayBack(VAR FileDesc :FileDescType;
                           VAR WF :WFtype; VAR Potl :PotlType;
                           VAR AutoLoop,Animate,HideCurs,
                           WishToCancel,WishToReturn :boolean);
VAR                       
   AskScr  :TInputScreen;
   Current,Verified :boolean;
BEGIN
   {New(AskScr);  {Mac only}
   WITH AskScr DO
      BEGIN
         Init;
         DefineInputPort(0.27,0.67,0.25,0.8);
         LoadLine('');
         LoadLine('    Play-back options');
         LoadLine('');
         LoadLine('  #1 animate one plot');
         LoadLine('  #1 show four plots');
         LoadLine('');
         LoadLine('  #F Loop continuously.');
         LoadLine('  #F Hide cursor if animate.');
         IF WF.RecordToFile AND (WF.Mode=Auto) THEN
            BEGIN
               LoadLine('');
               LoadLine('  #T Use current results.');
            END;
         LoadLine('');
         LoadLine('    [  OK  ]  [Cancel]');
         IF Animate THEN SetRadioButton('1',1)
         ELSE SetRadioButton('1',2);
         IF AutoLoop THEN SetBoolean(3,True);
         AcceptScreen;
         IF Canceled THEN WishToCancel:=True
         ELSE WITH WF DO
            BEGIN
               WishToCancel:=False;
               WishToReturn:=False;
               CASE GetRadioButton('1') OF
                  1 :Animate:=True;
                  2 :Animate:=False;
                  END; {case}
               AutoLoop:=GetBoolean(3);
               HideCurs:=GetBoolean(4);
               IF RecordToFile AND (Mode=Auto) THEN
                  Current:=GetBoolean(5)
               ELSE Current:=False;
               IF NOT Current THEN
                  BEGIN
                     VerifyNew(Saved,Verified);
                     IF Verified THEN
                        BEGIN
                           OpenDataFile(FileDesc,WF,Potl,WishToReturn);
                           IF (Computed<>NotDone) AND RecordToFile THEN
                              BEGIN
                                 PrepRecFileName(FileDesc);
                                 ResetRecFile(FileDesc,WF,Potl,WishToReturn);
                              END
                           ELSE
                              BEGIN
                                 IF Computed<>NotDone THEN
                                    PlotWF(WF,Potl,True);
                                 Announce(ConCat('Chosen file does not',
                                          ' contain a recorded sequence.'));
                                 WishToReturn:=True;
                              END;
                        END
                     ELSE WishToReturn:=True;
                  END
               ELSE ResetRecFile(FileDesc,WF,Potl,WishToReturn);
            END;
         Done;
      END;
   {Dispose(AskScr); {Mac only}
END; {AskAboutPlayback}

PROCEDURE CloseFrame(VAR WF :WFtype; VAR Potl :PotlType; VAR Frame :TFrames);
BEGIN
   Frame.Close;
   SetVGAHi;           {no-op for Mac}
   DefineAllViewPorts;
   ClearMUPPETport;
   WF.Plotted:=False;
   PlotWF(WF,Potl,True);
   ShowMouse;
END; {CloseFrame}

PROCEDURE PauseInPlayback(VAR FileDesc :FileDescType;
                          VAR WF :WFtype; VAR Potl :PotlType;
                          VAR MainMenu :TMenu; VAR Frame :TFrames;
                          VAR MenuClicked,AutoLoop,
                              Animate,HideCurs,WishToReturn :boolean);
VAR
   WFHelpScr     :HelpScrType;
   WFkeys        :THotKeys;
   Wkey          :byte;
   nn            :integer;
   WishToCancel,
   FileExists,
   WishToResume  :boolean;
BEGIN
   WFHelpScr[01]:='';
   WFHelpScr[02]:=' <F1>-show this help screen.';
   WFHelpScr[03]:='';
   WFHelpScr[04]:=' <F2>-resume play back.';
   WFHelpScr[05]:='';
   WFHelpScr[06]:=' <F3>-toggle size of main plot.';
   WFHelpScr[07]:='    When small, a legend and';
   WFHelpScr[08]:='    text information are shown.';
   WFHelpScr[09]:='';
   WFHelpScr[10]:=' <F4>-change plot type, select different plane,';
   WFHelpScr[11]:='    or modify view of surface via rotation';
   WFHelpScr[12]:='    or change in size, perspective, or range.';
   WFHelpScr[13]:='';
   WFHelpScr[14]:=' <F5>-change file or looping option.';
   WFHelpScr[15]:='';
   WFHelpScr[16]:=' <F10>-activate menu.';
   FOR nn:=17 TO 25 DO WFHelpScr[nn]:='';

   IF Animate THEN CloseFrame(WF,Potl,Frame);
   MainMenu.Display;
   {New(WFkeys); {Mac only}
   WITH WFkeys DO
      BEGIN
         Init(6);
         key[1]:='F1-Help';
         key[2]:='F2-Resume';
         IF WF.Zoom THEN key[3]:='F3-Contract'
         ELSE key[3]:='F3-Expand';
         key[4]:='F4-ModView';
         key[5]:='F5-Input';
         key[6]:='F10-Menu';
         Display;
      END;
   WITH WF DO
      BEGIN
         WishToResume:=False;
         WHILE (NOT WishToResume) AND (NOT WishToReturn)
            AND (NOT MenuClicked) DO
               BEGIN
                  CheckForEvents;
                  IF Event.ReadKey=Escape THEN WishToReturn:=True
                  ELSE IF WFkeys.Pressed(Wkey) THEN
                     BEGIN
                        CASE Wkey OF
                           1 :Help(WFHelpScr);
                           2 :WishToResume:=True;
                           3 :BEGIN
                                 Zoom:=not Zoom; Plotted:=False;
                                 PlotWF(WF,Potl,True);
                                 IF Zoom THEN WFkeys.key[3]:='F3-Contract'
                                 ELSE WFkeys.key[3]:='F3-Expand';
                                 WFkeys.Display;
                              END;
                           4 :BEGIN
                                 ModViewWF(FileDesc,WF,Potl);
                                 WFkeys.Display;
                              END;
                           5 :BEGIN
                                 Close(FileDesc.RecFile);
                                 AskAboutPlayBack(FileDesc,WF,Potl,
                                                  AutoLoop,Animate,HideCurs,
                                                  WishToCancel,WishToReturn);
                                 IF WishToCancel THEN WITH FileDesc DO
                                    ConnectRealFile(RecFile,RecFileName,
                                                    FileExists)
                                 ELSE IF NOT WishToReturn THEN
                                    BEGIN
                                       Potl.NautoRemain:=Potl.NautoRemain+1;
                                       WishToResume:=True;
                                       MainMenu.RowActivate(3,4,RecordVeff);
                                    END;
                              END;
                           6 :BEGIN
                                 WishToReturn:=True;
                                 IF MainMenu.Chosen THEN
                                    MenuClicked:=True;
                              END;
                           END; {case}
                     END
                  ELSE IF MainMenu.Activated THEN MenuClicked:=True;
               END;
         WFkeys.Clear;
      END;
   {Dispose(WFkeys); {Mac only}
END; {PauseInPlayBack}

PROCEDURE StartAnimation(VAR Frame :TFrames; VAR PBkeys :THotKeys;
                         VAR WF :WFtype; VAR Potl :PotlType;
                         HideCurs :boolean);
VAR
   vp :integer;
BEGIN
   ClearMUPPETport;
   IF HideCurs THEN HideMouse;
   Frame.Init;   
   SetVGAMed;        {no-op on Mac}
   DefineAllViewPorts;
   PrepPlot(WF,Potl,True,vp);
   PutLabel(Top,ConCat('Probability density (',
                       WF.s2Label,'-',WF.s1Label,' plane)'));
   PBkeys.Display;
   SelectViewPort(vp);
   IF WF.Surf THEN PlotSurface(WF,HideCurs)
   ELSE PlotContour(WF,True,HideCurs);
   SelectViewPort(vp);
   Frame.Open(vp);
   IF WF.Surf THEN PlotSurface(WF,HideCurs)
   ELSE PlotContour(WF,True,HideCurs);
   SelectViewPort(vp);
   IF NOT HideCurs THEN ShowMouse;
   Frame.SwitchAndClean;
END; {StartAnimation}

PROCEDURE StartFourPanel(VAR vpCurrent :integer; VAR PBkeys :THotKeys;
                         VAR WF :WFtype; VAR Potl :PotlType);
VAR
   vp :integer;
BEGIN
   ClearMUPPETport;
   DefineAllViewPorts;
   PBkeys.Display;
   vpCurrent:=11;
END; {StartFourPanel}

PROCEDURE StepAnimation(VAR FileDesc :FileDescType; VAR Frame :TFrames;
                        VAR WF :WFtype; VAR Potl :PotlType;
                        HideCurs :boolean);
VAR
   vp :integer;
BEGIN
   WITH WF,Potl DO
      BEGIN
         IF Zoom THEN vp:=3
         ELSE
            BEGIN
               CASE Computed OF
                  Distorted :BEGIN
                                Dipole.Get(NautoRemain+1,DipoleVal);
                                PrepDWFPlotScr(FileDesc,WF,Potl);
                             END;
                  Shared    :PrepSWFPlotScr(FileDesc,WF,Potl);
                  END; {case}
               {Frame.Pause;  {Mac only}
               Info(PlotInfoScr,25,5);
               {Frame.Continue; {Mac only}
               vp:=2;
            END;
         IF Surf THEN
            BEGIN
               SelectViewPort(vp);
               ClearViewPort;
               PlotSurface(WF,HideCurs);
            END
         ELSE
            BEGIN
               SelectViewPort(vp);
               PlotContour(WF,True,HideCurs);
            END;
         SelectViewPort(vp);
         IF NOT HideCurs THEN ShowMouse;
         Frame.SwitchAndClean;
      END;
END; {StepAnimation}

PROCEDURE StepFourPanel(VAR vpCurrent :integer;
                        VAR WF :WFtype; VAR Potl :PotlType);
VAR
   PlotLabel :string;
BEGIN
   WITH WF,Potl DO
      BEGIN
         CASE Computed OF
            Distorted :PlotLabel:=ConCat('E: ',Num2Str(Efield,8),'TV/m',
                                         '   B: ',Num2Str(Bfield,8),'MT');
            Shared :PlotLabel:=ConCat('Z_2: ',Num2Str(ExtCharge,8),' e',
                                      '   Dist: ',Num2Str(Separation,8),
                                      ' ',Lunit,'m');
            END; {case}
         IF Surf THEN
            BEGIN
               OpenViewPort(vpCurrent);
               PutLabel(Inside,PlotLabel);
               PlotSurface(WF,False);
            END
         ELSE
            BEGIN
               SelectViewPort(vpCurrent);
               PrepContour(WF.s1Max,WF.s2Max);
               OpenViewPort(vpCurrent);
               PlotContour(WF,False,False);
               SelectViewPort(vpCurrent);
               PutLabel(Inside,PlotLabel);
               DefineAllViewPorts;
            END;
         vpCurrent:=vpCurrent+1;
         IF vpCurrent>14 THEN vpCurrent:=11;
      END;
END; {StepFourPanel}

PROCEDURE TerminatePlayBack(VAR MainMenu :TMenu;
                            VAR WF :WFtype; VAR Potl :PotlType;
                            VAR Frame :TFrames; Animate :boolean);
BEGIN
   IF Animate THEN CloseFrame(WF,Potl,Frame)
   ELSE
      BEGIN
         DefineAllViewPorts;
         ClearMuppetPort;
         WF.Plotted:=False;
         PlotWF(WF,Potl,True);
      END;
   MainMenu.Display;
END; {TerminatePlayBack}

PROCEDURE PlayBack(VAR FileDesc :FileDescType;
                   VAR WF :WFtype; VAR Potl :PotlType;
                   VAR MainMenu :TMenu; VAR MenuClicked :boolean);
VAR
   PBHelpScr       :HelpScrType;
   PBkeys          :THotKeys;
   Pkey            :byte;
   nn,vpCurrent    :integer;
   rNumIter,rCwarn :real;
   AutoLoop,
   Step,TakeStep,
   Animate,HideCurs,
   WishToCancel,
   WishToReturn    :boolean;
   Frame           :TFrames;
BEGIN
   AutoLoop:=False; Step:=False; TakeStep:=True; Animate:=True;
   AskAboutPlayback(FileDesc,WF,Potl,
                    AutoLoop,Animate,HideCurs,WishToCancel,WishToReturn);
   MainMenu.RowActivate(3,4,WF.RecordVeff);
   IF (NOT WishToReturn) AND (NOT WishToCancel) THEN
      BEGIN
         PBHelpScr[01]:='';
         PBHelpScr[02]:=' <F1>-show this help screen.   ';
         PBHelpScr[03]:='';
         PBHelpScr[04]:=' <F2>-pause in playback, if in run mode,';
         PBHelpScr[05]:='    or resume run if in step mode.';
         PBHelpScr[06]:='';
         PBHelpScr[07]:=' <F3>-enter step mode or take step,';
         PBHelpScr[08]:='    if already in that mode.';
         PBHelpScr[09]:='';
         PBHelpScr[10]:=' <F4>-stop playback.';
         FOR nn:=11 TO 25 DO PBHelpScr[nn]:='';

         {New(PBkeys); {Mac only}
         WITH PBkeys DO
            BEGIN
               Init(4);
               key[1]:='F1-Help';
               key[2]:='F2-Pause';
               key[3]:='F3-Step';
               key[4]:='F4-Stop';
            END;
         WITH WF,Potl DO
            BEGIN
               read(FileDesc.RecFile,En,EField,Bfield,
                    ExtCharge,Separation,rNumIter,rCwarn);
               NumIter:=round(rNumIter);
               IF round(rCwarn)=1 THEN Cwarn:=True ELSE Cwarn:=False;
               Psi.ReadF(FileDesc.RecFile);
               PrepPsiSlice(WF,Potl);
            END;
         {New(Frame); {Mac only}
         IF Animate THEN StartAnimation(Frame,PBkeys,WF,Potl,HideCurs)
         ELSE StartFourPanel(vpCurrent,PBkeys,WF,Potl);
         WITH WF,Potl DO
            BEGIN
               REPEAT
                  reset(FileDesc.RecFile);
                  NautoRemain:=NautoSteps+1;
                  IF RecordVeff THEN NautoRemain:=NautoRemain-1;
                  WHILE (NOT WishToReturn) AND (NOT MenuClicked)
                    AND (NOT eof(FileDesc.RecFile)) DO
                     BEGIN
                        IF (NOT Step) OR TakeStep THEN
                           BEGIN
                              NautoRemain:=NautoRemain-1;
                              read(FileDesc.RecFile,En,EField,Bfield,
                                   ExtCharge,Separation,rNumIter,rCwarn);
                              NumIter:=round(rNumIter);
                              IF round(rCwarn)=1 THEN Cwarn:=True
                              ELSE Cwarn:=False;
                              Psi.ReadF(FileDesc.RecFile); Plotted:=False;
                              PrepPsiSlice(WF,Potl);
                              PBkeys.Display;
                              IF Animate THEN
                                 StepAnimation(FileDesc,Frame,
                                               WF,Potl,HideCurs)
                              ELSE StepFourPanel(vpCurrent,WF,Potl);
                              TakeStep:=False;
                           END;
                        IF Animate THEN Frame.Pause;
                        CheckForEvents;
                        IF Event.ReadKey=Escape THEN
                           BEGIN
                              WishToReturn:=True;
                              TerminatePlayBack(MainMenu,WF,Potl,
                                                Frame,Animate);
                           END
                        ELSE IF PBkeys.Pressed(Pkey) THEN
                           BEGIN
                              CASE Pkey OF
                                 1 :Help(PBHelpScr);
                                 2 :BEGIN
                                       PBkeys.Clear;
                                       IF Step THEN
                                          BEGIN
                                             Step:=False;
                                             PBkeys.key[2]:='F2-Pause';
                                             PBkeys.Display;
                                          END
                                       ELSE
                                          BEGIN
                                             PauseInPlayBack(FileDesc,
                                                       WF,Potl,MainMenu,
                                                       Frame,MenuClicked,
                                                       AutoLoop,Animate,
                                                       HideCurs,
                                                       WishToReturn);
                                             IF (NOT WishToReturn)
                                              AND (NOT MenuClicked) THEN
                                                BEGIN
                                                   IF Animate THEN
                                                      StartAnimation(Frame,
                                                              PBkeys,WF,Potl,
                                                              HideCurs)
                                                   ELSE
                                                     StartFourPanel(vpCurrent,
                                                              PBkeys,WF,Potl);
                                                END;
                                          END;
                                    END;
                                 3 :BEGIN
                                       TakeStep:=True;
                                       IF NOT Step THEN
                                          BEGIN
                                             Step:=True;
                                             PBkeys.Clear;
                                             PBkeys.key[2]:='F2-Run';
                                             PBkeys.Display;
                                          END;
                                    END;
                                 4 :BEGIN
                                       WishToReturn:=True;
                                       TerminatePlayBack(MainMenu,WF,Potl,
                                                         Frame,Animate);
                                    END;
                                 END; {case}
                           END;
                        IF Animate AND (NOT WishToReturn)
                           AND (NOT MenuClicked) THEN Frame.Continue;
                     END;
               UNTIL (NOT AutoLoop) OR WishToReturn OR MenuClicked;
               PBkeys.Clear;
               IF (NOT WishToReturn) AND (NOT MenuClicked) THEN
                  TerminatePlayBack(MainMenu,WF,Potl,Frame,Animate);
            END;
         Close(FileDesc.RecFile);
         {Dispose(Frame); {Mac only}
         {Dispose(PBkeys); {Mac only}
      END;
END; {PlayBack}

END. {HatmFile}
