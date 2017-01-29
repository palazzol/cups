{Unit of routines for calculation of H-atom wave functions.          }
{Used by Hatom.pas.                                                  }
{                                                                    }
{Author: John R. Hiller                                              }
{Version: 1.0              7/24/95                                   }
{Copyright (c) 1995  John Wiley & Sons, Inc.                         }
{                                                                    }
UNIT HatmCalc;

INTERFACE

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph,
     HatmMach;

CONST
   alpha=0.729735e-2;
   hbarc=197.327; {MeV-fm}
   MTeslaConv=1.112;
   MaxRange=8000;

TYPE
   CompType=(NotDone,Undistorted,Distorted,Shared,Binding);
   ParticleType=(electron,muon,tau,custom);
   AlgType=(Basis,Lanczos);
   ModeType=(Manual,Auto);
   InfoScrType=array [1..25] OF string[30];
   PotlType=record
      Vmat          :DMatrixExt;
      V0,                        {energy scale}
      L0,                        {length scale}
      pz0,                       {dipole moment scale}
      Mass,                      {mass*c^2 for particle}
      charge,                    {particle charge}
      Z1,                        {(first) nuclear charge}
      m1,m2,                     {nuclear masses}
      mu,                        {reduced mass}
      zeta,                      {dimensionless scale parameter}
      Efac,Lfac,Mfac,
      Efield,dEfield,
      MinEfield,MaxEfield,
      Bfield,dBfield,
      MinBfield,MaxBfield,       {strengths of applied fields, TV/m & MTesla}
      FieldMin,FieldMax,
      dField,
      ExtCharge,dExtCh,
      MaxExtCh,MinExtCh,         {external charge (second nuclear charge)}
      Separation,dSep,                
      MaxSep,MinSep :real;       {nuclear separation}
      Eunit,                     {energy unit prefix}
      Lunit,                     {length unit prefix}
      pzUnit,                    {dipole moment unit prefix}
      Munit         :char;       {prefix for unit of mass}
      Particle      :ParticleType;
      FieldLabel    :string;
      nSteps,
      NrhoSteps,NzSteps,
      NautoSteps,
      NautoRemain   :integer;
      NewPotl       :boolean;
      rMax,dr,
      rhoMax,zMax,
      drho,dz       :real;
      END; {PotlType}
   WFtype=record
      Rnl             :DVector;
      Psi             :DMatrixExt;
      PsiSlice        :DMatrix;
      Dipole,                           {dipole moment values}
      Veff            :DVector;
      NstepsRemain,
      sPlane,
      n,l,m,nr,
      nMax,lMax,
      Level,
      MaxNumIter,NumIter,
      PNrhoSteps,PNzSteps :integer;
      s1Max,s2Max,
      Size,Distance,
      alfa,beta,gamma,
      VertMin,VertMax,
      PrhoMax,PzMax,
      DipoleVal,
      DipoleMin,DipoleMax,            {dipole moment & plot range}
      VeffMin,VeffMax,
      EnAsymp,
      En,tolerance    :real;
      s1Label,s2Label :string;
      Cwarn,
      Zoom,Surf,
      Plotted,
      Saved,
      RecordToFile,
      ResumeLanczos,
      ResumeStep,
      RecordVeff      :boolean;
      Computed        :CompType;
      Algorithm       :AlgType;
      Mode            :ModeType;
      PlotInfoScr     :InfoScrType;
      END; {WFtype}

{--------Routines for text input and output----------------------------}

PROCEDURE IntInputWarn(Name :string; VAR InpValue :integer;
                       DefValue :integer; field :integer);
PROCEDURE RealInputWarn(Name :string; VAR InpValue :real;
                        DefValue :real; field :integer);
PROCEDURE Info(VAR InfoScr :InfoScrType; NumLines,Vnum :integer);

{--------Routines for changes to parameters----------------------}

PROCEDURE SetpzScale(L0 :real; Lunit :char; VAR pz0 :real; VAR pzUnit :char);
PROCEDURE InitializeScales(VAR Potl :PotlType);
PROCEDURE ComputeZeta(VAR Potl :PotlType);
FUNCTION ReducedMass(Mass,m1 :real; Munit :char) :real;
PROCEDURE SetSurfaceView(VAR alfa,beta,gamma,Size,Distance :real);
PROCEDURE SetDefaults(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE SetMassAndUnits(VAR Potl :PotlType; VAR Modified :boolean);

{--------Preparation for plot---------------------------------------------}

PROCEDURE PrepPsiSlice(VAR WF :WFtype; VAR Potl :PotlType);

{--------Numerical routines-----------------------------------------------}

FUNCTION RadialWF(n,l :integer; r :real) :real;
FUNCTION AssocLegendre(l,m :integer; x :real) :real;
PROCEDURE PrepRadialWF(VAR Rnl :DVector; n,l,nSteps :integer; dr :real);
PROCEDURE DipoleMoment(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE OrderLevels(Nlevels :integer; VAR Levels :DVector;
                      VAR Ptr :IDVector);
PROCEDURE FindnthLevel(n,Nlevels :integer; VAR Levels :DVector;
                       VAR nLoc :integer);
PROCEDURE PrepStorage(VAR WF :WFtype; VAR Potl :PotlType;
                      VAR Enough :boolean);
PROCEDURE InitialGuess(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE ComputeViaChosenAlg(VAR WF :WFtype; VAR Potl :PotlType;
                              VAR Nkeys :THotKeys);

IMPLEMENTATION

{--------Routines for text input and output----------------------------}

PROCEDURE IntInputWarn(Name :string; VAR InpValue :integer;
                       DefValue :integer; field :integer);
{Warn of problem with integer input value.}
BEGIN
   Announce(ConCat('Value of ',NumStr(InpValue,field,0),
                   ' unacceptable for ',Name,'.  Will use ',
                   NumStr(DefValue,field,0),' instead.'));
   InpValue:=DefValue
END; {IntInputWarn}

PROCEDURE RealInputWarn(Name :string; VAR InpValue :real;
                        DefValue :real; field :integer);
{Warn of problem with real input value.}
BEGIN
   Announce(ConCat('Value of ',Num2Str(InpValue,field),
                   ' unacceptable for ',Name,'.  Will use ',
                   Num2Str(DefValue,field),' instead.'));
   InpValue:=DefValue
END; {RealInputWarn}

PROCEDURE Info(VAR InfoScr :InfoScrType; NumLines,Vnum :integer);
{Display text window for information.}
VAR
   i           :integer;
   Environment :TEnvironment;
BEGIN
   {New(Environment); {Mac only}
   Environment.Save;
   Environment.Standardize;
   OpenViewPort(Vnum);
   HideMouse;
   FOR i:=1 TO NumLines DO Print(1,i,InfoScr[i]);
   Environment.Reset;
   {Dispose(Environment); {Mac only}
   ShowMouse;
END; {Info}

{--------Routines for changes to parameters----------------------}

PROCEDURE SetpzScale(L0 :real; Lunit :char; VAR pz0 :real; VAR pzUnit :char);
BEGIN
  CASE Lunit OF
     'n' :pzUnit:='p';
     'p' :pzUnit:='f';
     'f' :pzUnit:='a';
     END; {case}
  pz0:=L0*1.0e3;
END;

PROCEDURE InitializeScales(VAR Potl :PotlType);
BEGIN
   WITH Potl DO
      BEGIN
         CASE Eunit OF
            ' ' :Efac:=1.0;
            'k' :Efac:=1.0e3;
            'M' :Efac:=1.0e6;
            'G' :Efac:=1.0e9;
            END; {case}
         CASE Munit OF
            'M' :Mfac:=1.0e6;
            'G' :Mfac:=1.0e9;
            END; {case}
         CASE Lunit OF
            'n' :Lfac:=1.0;
            'p' :Lfac:=1.0e-3;
            'f' :Lfac:=1.0e-6;
            END; {case}
         L0:=hbarc/(alpha*mu*Mfac*Lfac);
         V0:=0.5*mu*sqr(alpha)*Mfac/Efac;
         SetpzScale(L0,Lunit,pz0,pzUnit);
      END;
END; {InitializeScales}

PROCEDURE ComputeZeta(VAR Potl :PotlType);
{Compute dimensionless parameter of Schrodinger equation.}
VAR
   fac :real;
BEGIN
   WITH Potl DO
      BEGIN
         fac:=Efac*Mfac*sqr(Lfac);
         zeta:=2*mu*sqr(L0)*V0*fac/sqr(hbarc);
      END;
END; {ComputeZeta}

FUNCTION ReducedMass(Mass,m1 :real; Munit :char) :real;
BEGIN
   CASE Munit OF
      'M' : m1:=1.0e3*m1;
      'G' : m1:=m1;
      END; {case}
   ReducedMass:=Mass*m1/(Mass+m1);
END; {ReducedMass}

PROCEDURE SetSurfaceView(VAR alfa,beta,gamma,Size,Distance :real);
BEGIN
   alfa:=30; beta:=20; gamma:=0;
   Size:=1.9; Distance:=3
END; {SetSurfaceView}

PROCEDURE SetDefaults(VAR WF :WFtype; VAR Potl :PotlType);
BEGIN
   WITH WF,Potl DO
      BEGIN
         Particle:=electron;
         Mass:=0.511; Munit:='M'; charge:=-1;
         m1:=0.938; m2:=0.938;
         Z1:=1.0;
         mu:=ReducedMass(Mass,m1,Munit);
         Eunit:=' '; Lunit:='n';
         InitializeScales(Potl);
         ComputeZeta(Potl);
         NautoSteps:=10; NautoRemain:=0;
         rMax:=0.2/L0;
         rhoMax:=rMax; zMax:=rMax;
         nSteps:=100;
         dr:=rMax/nSteps;
         NrhoSteps:=20; NzSteps:=20;
         drho:=rhoMax/(NrhoSteps-0.5); dz:=zMax/NzSteps;
         Efield:=0.0; MinEfield:=0.0; MaxEfield:=0.2;
         dEfield:=(MaxEfield-MinEfield)/NautoSteps;
         Bfield:=0.0; MinBfield:=0.0; MaxBfield:=2.0;
         dBfield:=(MaxBfield-MinBfield)/NautoSteps;
         ExtCharge:=1.0; MinExtCh:=1.0; MaxExtCh:=1.0;
         dExtCh:=(MaxExtCh-MinExtCh)/NautoSteps;
         Separation:=zMax/2; MinSep:=0.0; MaxSep:=zMax;
         dSep:=(MaxSep-MinSep)/NautoSteps;
         VertMin:=-1.0; VertMax:=1.0;
         PrhoMax:=rhoMax; PzMax:=zMax;
         SetSurfaceView(alfa,beta,gamma,Size,Distance);
         Level:=1;
         MaxNumIter:=20; tolerance:=0.001;
         n:=1; l:=0; m:=0; nr:=n-l-1; nMax:=5; lMax:=4;
         Computed:=NotDone;
         sPlane:=2; Plotted:=False; Saved:=True;
         Zoom:=False; Surf:=False;
         ResumeLanczos:=False; ResumeStep:=False;
         Algorithm:=Lanczos; Mode:=Manual;
         RecordToFile:=False; RecordVeff:=False;
         NewPotl:=True;
      END
END; {SetDefaults}

PROCEDURE SetMassAndUnits(VAR Potl :PotlType; VAR Modified :boolean);
VAR
   MassScr :TInputScreen;
BEGIN
   Modified:=False;
   {New(MassScr);  {Mac only}
   WITH MassScr,Potl DO
      BEGIN
         Init;
         DefineInputPort(0.15,0.85,0.15,0.85);
         LoadLine('');
         LoadLine('       Choice of Light Particle and Units');
         LoadLine('');
         LoadLine('   #1  electron,  mass: 0.511 MeV/c^2  charge: -e');
         LoadLine('          energy unit: eV     length unit: nm');
         LoadLine('   #1  muon,  mass: 0.106 GeV/c^2  charge: -e');
         LoadLine('          energy unit: keV    length unit: pm');
         LoadLine('   #1  tau,  mass: 1.784 GeV/c^2  charge: -e');
         LoadLine('          energy unit: MeV    length unit: fm');
         LoadLine('   #1  custom, mass: {      } #2 MeV/c^2   #2 Gev/c^2');
         LoadLine('          charge: {    } multiples of e');
         LoadLine('          energy unit: #3 eV  #3 keV  #3 MeV  #3 GeV');
         LoadLine('          length unit: #4 nm  #4 pm   #4 fm');
         LoadLine('');
         LoadLine('   [  OK  ]    [Cancel]');
         LoadLine('');
         CASE Particle OF
            electron :SetRadioButton('1',1);
            muon     :SetRadioButton('1',2);
            tau      :SetRadioButton('1',3);
            custom   :SetRadioButton('1',4);
            END; {case}
         SetNumber(5,Mass);
         SetNumber(8,charge);
         CASE Munit OF
            'M' :SetRadioButton('2',1);
            'G' :SetRadioButton('2',2);
            END; {case}
         CASE Eunit OF
            ' ' :SetRadioButton('3',1);
            'k' :SetRadioButton('3',2);
            'M' :SetRadioButton('3',3);
            'G' :SetRadioButton('3',4);
            END; {case}
         CASE Lunit OF
            'n' :SetRadioButton('4',1);
            'p' :SetRadioButton('4',2);
            'f' :SetRadioButton('4',3);
            END; {case}
         AcceptScreen;
         IF NOT Canceled THEN
            BEGIN
               Modified:=True;
               CASE GetRadioButton('1') OF
                  1 :BEGIN
                        Particle:=electron;
                        Mass:=0.511; Munit:='M'; charge:=-1;
                        Eunit:=' '; Lunit:='n';
                     END;
                  2 :BEGIN
                        Particle:=muon;
                        Mass:=0.106; Munit:='G'; charge:=-1;
                        Eunit:='k'; Lunit:='p';
                     END;
                  3 :BEGIN
                        Particle:=tau;
                        Mass:=1.784; Munit:='G'; charge:=-1;
                        Eunit:='M'; Lunit:='f';
                     END;
                  4 :BEGIN
                        Particle:=custom;
                        Mass:=GetNumber(5);
                        IF Mass<=0.0 THEN
                           RealInputWarn('mass',Mass,0.511,6);
                        CASE GetRadioButton('2') OF
                           1 :Munit:='M';
                           2 :Munit:='G';
                           END; {case}
                        charge:=GetNumber(8);
                        CASE GetRadioButton('3') OF
                           1 :Eunit:=' ';
                           2 :Eunit:='k';
                           3 :Eunit:='M';
                           4 :Eunit:='G';
                           END; {case}
                        CASE GetRadioButton('4') OF
                           1 :Lunit:='n';
                           2 :Lunit:='p';
                           3 :Lunit:='f';
                           END; {case}
                     END;
                  END; {case}
            END;
         Done;
         InitializeScales(Potl);
         ComputeZeta(Potl);
      END;
   {Dispose(MassScr);  {Mac only}
END; {SetMassandUnits}

{--------Preparation for plot---------------------------------------------}

PROCEDURE PrepPsiSlice(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   MinRow,MinCol,MaxRow,MaxCol,
   ixStep,iyStep,izStep,irho,ir,
   PNrhoSteps,PNzSteps                :integer;
   PsiVal,x,y,z,r,rho,ctheta,Min,Max  :real;
BEGIN
   WITH WF,Potl DO
      BEGIN
         CASE sPlane OF
            1 :BEGIN
                  s1Max:=PrhoMax; s2Max:=PrhoMax;
                  s1Label:='y'; s2Label:='x';
                  FOR ixStep:=1 TO PNrhoSteps DO
                     FOR iyStep:=1 TO PNrhoSteps DO
                        BEGIN
                           x:=(ixStep-0.5)*drho; y:=(iyStep-0.5)*drho;
                           rho:=sqrt(sqr(x)+sqr(y));
                           CASE Computed OF
                              Undistorted :PsiVal:=
                                              RadialWF(n,l,-Z1*charge*rho)
                                                   *AssocLegendre(l,m,0.0);
                              Distorted,
                              Shared      :BEGIN
                                              irho:=trunc(rho/drho+0.5);
                                              IF irho>=NrhoSteps THEN
                                                 PsiVal:=0.0
                                              ELSE
                                                 PsiVal:=(irho+0.5-rho/drho)
                                                   *Psi.Value(irho,NzSteps+1)
                                                  +(rho/drho+0.5-irho)
                                                 *Psi.Value(irho+1,NzSteps+1);
                                           END;
                              Binding     :BEGIN
                                              irho:=trunc(rho/dr);
                                              IF irho>=nSteps THEN
                                                 PsiVal:=0.0
                                              ELSE
                                                 PsiVal:=((irho+1-rho/dr)
                                                          *Rnl.Value(irho+1)
                                                        +(rho/dr-irho)
                                                          *Rnl.Value(irho+2))
                                                      *AssocLegendre(l,m,0.0);
                                           END;
                              END; {case}
                           PsiVal:=sqr(PsiVal);
                           PsiSlice.Put(ixStep+PNrhoSteps,
                                        iyStep+PNrhoSteps,PsiVal);
                           PsiSlice.Put(ixStep+PNrhoSteps,
                                        PNrhoSteps-iyStep+1,PsiVal);
                           PsiSlice.Put(PNrhoSteps-ixStep+1,
                                        iyStep+PNrhoSteps,PsiVal);
                           PsiSlice.Put(PNrhoSteps-ixStep+1,
                                        PNrhoSteps-iyStep+1,PsiVal);
                        END;
               END;
            2 :BEGIN
                  s1Max:=PzMax; s2Max:=PrhoMax;
                  s1Label:='z'; s2Label:='y';
                  FOR iyStep:=1 TO PNrhoSteps DO
                     FOR izStep:=-PNzSteps TO PNzSteps DO
                        BEGIN
                           CASE Computed OF
                              Undistorted :BEGIN
                                              y:=(iyStep-0.5)*drho;
                                              z:=izStep*dz;
                                              r:=sqrt(sqr(y)+sqr(z));
                                              IF r=0.0 THEN ctheta:=1.0
                                              ELSE ctheta:=sqrt(1-sqr(y/r));
                                              PsiVal:=
                                                 RadialWF(n,l,-Z1*charge*r)
                                                 *AssocLegendre(l,m,ctheta);
                                           END;
                              Distorted,
                              Shared      :if (iyStep<=NrhoSteps)
                                              AND (-NzSteps<izStep)
                                              AND (izStep<NzSteps) THEN
                                                 PsiVal:=Psi.Value(iyStep,
                                                          izStep+NzSteps+1)
                                           ELSE PsiVal:=0.0;
                              Binding     :BEGIN
                                              y:=(iyStep-0.5)*drho;
                                              z:=izStep*dz;
                                              r:=sqrt(sqr(y)+sqr(z));
                                              IF r=0.0 THEN ctheta:=1.0
                                              ELSE ctheta:=sqrt(1-sqr(y/r));
                                              ir:=trunc(r/dr);
                                              IF ir>=nSteps THEN
                                                 PsiVal:=0.0
                                              ELSE
                                                 PsiVal:=((ir+1-r/dr)
                                                          *Rnl.Value(ir+1)
                                                        +(r/dr-ir)
                                                          *Rnl.Value(ir+2))
                                                   *AssocLegendre(l,m,ctheta);
                                           END;
                              END; {case}
                           PsiVal:=sqr(PsiVal);
                           PsiSlice.Put(iyStep+PNrhoSteps,izStep+PNzSteps+1,
                                        PsiVal);
                           PsiSlice.Put(PNrhoSteps-iyStep+1,izStep+PNzSteps+1,
                                        PsiVal);   
                        END;
               END;
            3 :BEGIN
                  s1Max:=PzMax; s2Max:=PrhoMax;
                  s1Label:='z'; s2Label:='x';
                  FOR ixStep:=1 TO PNrhoSteps DO
                     FOR izStep:=-PNzSteps TO PNzSteps DO
                        BEGIN
                           CASE Computed OF
                              Undistorted :BEGIN
                                              x:=(ixStep-0.5)*drho;
                                              z:=izStep*dz;
                                              r:=sqrt(sqr(x)+sqr(z));
                                              IF r=0.0 THEN ctheta:=1.0
                                              ELSE ctheta:=sqrt(1-sqr(x/r));
                                              PsiVal:=
                                                 RadialWF(n,l,-Z1*charge*r)
                                                 *AssocLegendre(l,m,ctheta);
                                           END;
                              Distorted,
                              Shared      :if (ixStep<=NrhoSteps)
                                              AND (-NzSteps<izStep)
                                              AND (izStep<NzSteps) THEN
                                                 PsiVal:=Psi.Value(ixStep,
                                                          izStep+NzSteps+1)
                                           ELSE PsiVal:=0.0;
                              Binding     :BEGIN
                                              x:=(ixStep-0.5)*drho;
                                              z:=izStep*dz;
                                              r:=sqrt(sqr(x)+sqr(z));
                                              IF r=0.0 THEN ctheta:=1.0
                                              ELSE ctheta:=sqrt(1-sqr(x/r));
                                              ir:=trunc(r/dr);
                                              IF ir>=nSteps THEN
                                                 PsiVal:=0.0
                                              ELSE
                                                 PsiVal:=((ir+1-r/dr)
                                                          *Rnl.Value(ir+1)
                                                        +(r/dr-ir)
                                                          *Rnl.Value(ir+2))
                                                   *AssocLegendre(l,m,ctheta);
                                           END;
                              END; {case}
                           PsiVal:=sqr(PsiVal);
                           PsiSlice.Put(ixStep+PNrhoSteps,izStep+PNzSteps+1,
                                        PsiVal);
                           PsiSlice.Put(PNrhoSteps-ixStep+1,izStep+PNzSteps+1,
                                        PsiVal);
                        END;
               END;
            END; {case}
         PsiSlice.MinMax(MinRow,MinCol,Min,MaxRow,MaxCol,Max);
         IF abs(Min)>Max THEN Max:=abs(Min);
         IF Max=0.0 THEN Max:=1.0;
         PsiSlice.MultScalar(1/Max);
      END;
END; {PrepPsiSlice}

{--------Numerical routines-----------------------------------------------}

FUNCTION RadialWF(n,l :integer; r :real) :real;
VAR
   nlm1,k :integer;
   Lk,Lkm1,Lkp1,t :real;
BEGIN
   IF r/n>80.0 THEN RadialWF:=0.0
   ELSE
      BEGIN
         nlm1:=n-l-1; t:=2*r/n;           {compute L_(n-l-1)^(2l+1)(2r/n)}
         Lkm1:=1.0; Lk:=2*(l+1)-t;
         IF nlm1=0 THEN Lkp1:=Lkm1
         ELSE IF nlm1=1 THEN Lkp1:=Lk
         ELSE
            BEGIN
               k:=1;
               WHILE k<nlm1 DO
                  BEGIN
                     k:=k+1;
                     Lkp1:=((2*(k+l+1)-t)*Lk-(k+2*l+1)*Lkm1)/(k+1);
                     Lkm1:=Lk; Lk:=LkP1;
                  END;
            END;
         RadialWF:=exp(-r/n)*Pwr(r,1.0*l)*Lkp1;
      END;
END; {RadialWF}

FUNCTION AssocLegendre(l,m :integer; x :real) :real;
VAR
   k    :integer;
   Plp1,Pl,Plm1 :real;
BEGIN
   Plm1:=0.0; Pl:=Pwr(sqrt(1-sqr(x)),1.0*m);
   IF l=m THEN Plp1:=Pl
   ELSE
      BEGIN
         k:=m;
         WHILE k<l DO
            BEGIN
               k:=k+1;
               Plp1:=((2*k+1)*x*Pl-(k+m)*Plm1)/(k-m+1);
               Plm1:=Pl; Pl:=Plp1;
            END;
      END;
   AssocLegendre:=Plp1;
END; {AssocLegendre}

PROCEDURE PrepRadialWF(VAR Rnl :DVector; n,l,nSteps :integer; dr :real);
VAR
   iStep,iMin,iMax :integer;
   RnlMax,RnlMin   :real;
BEGIN
   Rnl.Free; Rnl.Init(nSteps+1);
   FOR iStep:=0 TO nSteps DO Rnl.Put(iStep+1,RadialWF(n,l,iStep*dr));
   Rnl.MinMax(iMin,RnlMin,iMax,RnlMax);
   IF abs(RnlMin)>RnlMax THEN RnlMax:=abs(RnlMin);
   IF RnlMax=0.0 THEN RnlMax:=1.0;
   Rnl.MultScalar(1/RnlMax);
END; {PrepRadialWF}

PROCEDURE DipoleMoment(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   i,j   :integer;
   z,rho :real;
BEGIN
   WITH WF,Potl DO
      BEGIN
         DipoleVal:=0.0;
         FOR i:=-NzSteps TO NzSteps DO
            BEGIN
               z:=i*dz;
               for j:=1 to NrhoSteps do
                  BEGIN
                     rho:=(j-0.5)*drho;
                     DipoleVal:=DipoleVal+rho*z*sqr(Psi.Value(j,i+NzSteps+1));
                  END;
            END;
         DipoleVal:=charge*DipoleVal
      END;
END; {DipoleMoment}

PROCEDURE OrderLevels(Nlevels :integer; VAR Levels :DVector;
                      VAR Ptr :IDVector);
VAR
   i,temp :integer;
   Done   :boolean;
BEGIN
   for i:=1 to Nlevels do Ptr.Put(i,i);
   REPEAT
      Done:=True;
      FOR i:=1 TO NLevels-1 DO
         IF Levels.Value(Ptr.Value(i))>Levels.Value(Ptr.Value(i+1)) THEN
            BEGIN
               Done:=False;
               temp:=Ptr.Value(i);
               Ptr.Put(i,Ptr.Value(i+1));
               Ptr.Put(i+1,temp);
            END;
   UNTIL Done;
END; {OrderLevels}

PROCEDURE FindnthLevel(n,Nlevels :integer; VAR Levels :DVector;
                       VAR nLoc :integer);
VAR
   Ptr    :IDVector;
BEGIN
   {New(Ptr); {Mac only}
   Ptr.Init(NLevels);
   OrderLevels(Nlevels,Levels,Ptr);
   nLoc:=Ptr.Value(n);
   Ptr.Free;
   {Dispose(Ptr); {Mac only}
END; {FindnthLevel}

PROCEDURE PrepStorage(VAR WF :WFtype; VAR Potl :PotlType;
                      VAR Enough :boolean);
VAR
   NumRows,NumCols,
   MemBlock :longint;
   um1      :DMatrixExt;
BEGIN
   {New(um1); {Mac only}
   WITH WF,Potl DO
      BEGIN
         PsiSlice.Free; Psi.Free; Vmat.Free;
         Enough:=True;
         NumRows:=NrhoSteps; NumCols:=2*NzSteps+1;
         MemBlock:=NumRows*NumCols*Longint(SizeOf(real));
         IF (Computed=Distorted) OR (Computed=Shared) THEN
            BEGIN
               CASE Algorithm OF
                  Basis :Vmat.Init(1,1);
                  Lanczos :BEGIN
                              IF (MemBlock>=65521)
                               OR (MemBlock>MaxAvail) THEN
                                 BEGIN
                                    Vmat.Init(1,1); um1.Init(1,1);
                                    Enough:=False;
                                 END
                              ELSE
                                 BEGIN
                                    Vmat.Init(NrhoSteps,2*NzSteps+1);
                                    IF MemBlock>MaxAvail THEN
                                       BEGIN
                                          um1.Init(1,1); Enough:=False;
                                       END
                                    ELSE um1.Init(NrhoSteps,2*NzSteps+1);
                                 END;
                           END;
                  END; {case}
               IF NOT Enough OR (MemBlock>=65521) OR (MemBlock>MaxAvail) THEN
                  BEGIN
                     Psi.Init(1,1); Enough:=False;
                  END
               ELSE Psi.Init(NrhoSteps,2*NzSteps+1);
               IF Algorithm=Lanczos THEN um1.Free;
            END
         ELSE 
            BEGIN
               Vmat.Init(1,1);
               Psi.Init(1,1);
            END;
         PNrhoSteps:=trunc(PrhoMax/drho);
         IF PNrhoSteps<1 THEN PNrhoSteps:=1;
         PNzSteps:=trunc(PzMax/dz);
         IF PNzSteps<1 THEN PNzSteps:=1;
         NumRows:=2*PNrhoSteps;
         CASE sPlane OF
            1 :NumCols:=2*PNrhoSteps;
            2 :NumCols:=2*PNzSteps+1;
            3 :NumCols:=2*PNzSteps+1;
            END; {case}
         MemBlock:=NumRows*NumCols*Longint(SizeOf(real));
         IF NOT Enough OR (MemBlock>=65521) OR (MemBlock>MaxAvail) THEN
            BEGIN
               PsiSlice.Init(1,1); Enough:=False;
            END
         ELSE PsiSlice.Init(NumRows,NumCols);
      END;
   {Dispose(um1); {Mac only}
END; {PrepStorage}

{--------Routines for basis-function calculations---------------------}

PROCEDURE ConstructIndex(VAR Index :IDMatrix; VAR Ncap :integer;
                         nMax,lMax,m :integer);
VAR
   n,l :integer;
BEGIN
   Index.Fill(-1);
   Ncap:=0;
   FOR l:=abs(m) TO lMax DO
      FOR n:=l+1 TO nMax DO
         BEGIN
            Ncap:=Ncap+1;
            Index.Put(n,l+1,Ncap);
         END;
END; {ConstructIndex}

FUNCTION AngularMatrixElt(s,lp,l,m :integer) :real;
VAR
   AME :real;
BEGIN
   AME:=0.0;
   IF s=1 THEN
      BEGIN
         IF lp=l+1 THEN
            AME:=sqrt((sqr(l+1)-sqr(m))/((2*l+1)*(2*l+3)))
         ELSE IF lp=l-1 THEN
            AME:=sqrt((sqr(l)-sqr(m))/((2*l-1)*(2*l+1)));
      END
   ELSE IF s=2 THEN
      BEGIN
         IF lp=l+2 THEN
            AME:=-sqrt((sqr(l+2)-sqr(m))*(sqr(l+1)-sqr(m))/((2*l+5)*(2*l+1)))
                 /(2*l+3)
         ELSE IF lp=l THEN
            BEGIN
               AME:=(sqr(l+1)-sqr(m))/((2*l+1)*(2*l+3));
               IF l>0 THEN AME:=AME+(sqr(l)-sqr(m))/((2*l+1)*(2*l-1));
            END
         ELSE IF lp=l-2 THEN
            AME:=-sqrt((sqr(l-1)-sqr(m))*(sqr(l)-sqr(m))/((2*l-3)*(2*l+1)))
                 /(2*l-1);
      END;
   AngularMatrixElt:=AME;
END; {AngularMatrixElt}

Function Fac(n :integer) :real;    {n!}
VAR
   F :real;
BEGIN
   IF (n=0) OR (n=1) THEN Fac:=1.0
   ELSE
      BEGIN
         F:=Fac(n-1);   {avoid filling 8087 stack}
         Fac:=n*F;
      END;
END; {Fac}

FUNCTION RadialMatrixElt(s,np,lp,n,l :integer; Z :real) :real;
VAR
   j,jp              :integer;
   Norm,NormP,
   sum,sumjp0,sumjpj :real;
BEGIN
   Norm:=sqrt(Fac(n-l-1)/(2*n*Fac(n+l)));
   NormP:=sqrt(Fac(np-lp-1)/(2*np*Fac(np+lp)));
   sum:=0.0;
   sumjp0:=Pwr(2*np/(np+n),lp+1.5)*Pwr(2*n/(np+n),l+1.5)*Fac(lp+l+s+2)
           /(Fac(np-lp-1)*Fac(2*lp+1)*Fac(n-l-1)*Fac(2*l+1));
   FOR jp:=0 TO np-lp-1 DO
      BEGIN
         IF jp>0 THEN
            BEGIN
               sumjp0:=-sumjp0*(2*np/(np+n))*(lp+l+jp+s+2)/(jp*(2*lp+jp+1));
               IF jp<np-lp-2 THEN sumjp0:=sumjp0*(np-lp-jp-1);
            END;
         sumjpj:=sumjp0;
         FOR j:=0 TO n-l-1 DO
            BEGIN
               IF j>0 THEN
                  BEGIN
                     sumjpj:=-sumjpj*(2*n/(np+n))*(lp+l+jp+j+s+2)
                                     /(j*(2*l+j+1));
                     IF j<n-l-2 THEN sumjpj:=sumjpj*(n-l-j-1);
                  END;
               sum:=sum+sumjpj;
            END;
      END;
   RadialMatrixElt:=Pwr(n*np/(np+n)/Z,1.0*s)*Norm*NormP*sum;
END; {RadialMatrixElt}

PROCEDURE ConstructMatrix(VAR A :DMatrix; VAR Index :IDMatrix;
                          VAR WF :WFtype; VAR Potl :PotlType;
                          VAR Propeller :TWaitMessage);
VAR
    l,lp,n,np,
    j,jp      :integer;
    Aval,Z    :real;
    s         :array [0..2] of integer;
    ffac      :array [0..2] OF real;
BEGIN
   A.Fill(0.0);
   s[0]:=2; s[1]:=1; s[2]:=2;
   WITH Potl DO
      BEGIN
         Z:=abs(Z1*charge);
         ffac[0]:=sqr(charge*Bfield*MTeslaConv/sqr(mu*Mfac/1.0e6))/4;
         ffac[1]:=-charge*Efield*L0*Lfac*1.0e3/(V0*Efac);
         ffac[2]:=ffac[0];
      END;
   FOR l:=abs(WF.m) TO WF.lMax DO
      FOR n:=l+1 TO WF.nMax DO
         BEGIN
            Propeller.Update;
            j:=Index.Value(n,l+1);
            A.Put(j,j,-sqr(Z/n));
            FOR lp:=l TO l+2 DO
               IF lp<=WF.lMax THEN
                  FOR np:=lp+1 TO WF.nMax DO
                     BEGIN
                        Propeller.Update;
                        jp:=Index.Value(np,lp+1);
                        IF jp>=j THEN
                           BEGIN
                              Aval:=A.Value(j,jp)
                                    +ffac[lp-l]
                                       *AngularMatrixElt(s[lp-l],lp,l,WF.m)
                                       *RadialMatrixElt(s[lp-l],np,lp,n,l,Z);
                              A.Put(j,jp,Aval);
                              IF jp>j THEN A.Put(jp,j,Aval);
                           END;
                     END;
         END;
END; {ConstructMatrix}

PROCEDURE ConstructPsi(VAR A :DMatrix; VAR Index :IDMatrix; VAR Ptr :IDVector;
                       Ncap :integer; VAR WF :WFtype; VAR Potl :PotlType;
                       VAR Propeller :TWaitMessage);
VAR
   n,l,i,j     :integer;
   coeff,ZZ,
   z,rho,r,
   ctheta,temp :real;
BEGIN
   WITH WF,Potl DO
      BEGIN
         ZZ:=abs(Z1*charge);
         Psi.Fill(0.0);
      END;
   FOR l:=abs(WF.m) TO WF.lMax DO
      FOR n:=l+1 TO WF.nMax DO
         BEGIN
            coeff:=A.Value(Index.Value(n,l+1),Ptr.Value(WF.Level));
            FOR i:=-Potl.NzSteps TO Potl.NzSteps DO
               BEGIN
                  z:=i*Potl.dz;
                  FOR j:=1 TO Potl.NrhoSteps DO
                     BEGIN
                        Propeller.Update;
                        rho:=(j-0.5)*Potl.drho;
                        r:=sqrt(sqr(rho)+sqr(z));
                        IF r=0.0 THEN ctheta:=1.0
                        ELSE ctheta:=z/r;
                        temp:=WF.Psi.Value(j,i+Potl.NzSteps+1)
                              +coeff*RadialWF(n,l,ZZ*r)
                                    *AssocLegendre(l,WF.m,ctheta);
                        WF.Psi.Put(j,i+Potl.NzSteps+1,temp);
                     END;
               END;
         END;
END; {ConstrucPsi}

PROCEDURE ApplyBasisFn(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   Index :IDMatrix;
   A     :DMatrix;
   e,d   :DVector;
   Ptr   :IDVector;
   Ncap,
   nn    :integer;
   Propeller :TWaitMessage;
BEGIN
   {New(Index); New(A); New(d); New(e); New(Ptr); {Mac only}
   {New(Propeller); {Mac only}
   Index.Init(WF.nMax,WF.lMax+1);
   ConstructIndex(Index,Ncap,WF.nMax,WF.lMax,WF.m);
   Propeller.Show;
   A.Init(Ncap,Ncap);
   ConstructMatrix(A,Index,WF,Potl,Propeller);
   d.Init(Ncap); e.Init(Ncap);
   Sym2Trid(Ncap,A,d,e,True);
   Tridiag(Ncap,d,e,True,A);
   Ptr.Init(Ncap);
   OrderLevels(Ncap,d,Ptr);
   IF WF.Level>Ncap THEN IntInputWarn('level number',WF.Level,Ncap,4);
   WF.En:=d.Value(Ptr.Value(WF.Level));
   ConstructPsi(A,Index,Ptr,Ncap,WF,Potl,Propeller);
   Propeller.Hide;
   Ptr.Free; e.Free; d.Free; A.Free; Index.Free;
   {Dispose(Propeller); {Mac only}
   {Dispose(Ptr); Dispose(e); Dispose(d); {Mac only}
   {Dispose(A); Dispose(Index); {Mac only}
END; {ApplyBasisFn}

{--------Routines for Lanczos calculations----------------------------}

PROCEDURE FindMin(N :integer; VAR Vec :DVector;
                  VAR Min :real; VAR iMin :integer);
VAR
   temp :real;
   i    :integer;
BEGIN
   iMin:=1;
   Min:=Vec.Value(1);
   FOR  i:=2 TO N DO
      BEGIN
         temp:=Vec.Value(i);
         IF temp<Min THEN
            BEGIN
               iMin:=i; Min:=temp;
            END;
      END;
END; {FindMin}

PROCEDURE Multiply(VAR umOld,umNew :DMatrixExt; b :real;
                   VAR WF :WFtype; VAR Potl :PotlType;
                   VAR Propeller :TWaitMessage);
{umNew=H*umOld-b*umNew}
VAR
   i,j :integer;
   umTemp,rho :real;
BEGIN
   WITH WF,Potl DO
      BEGIN
         umNew.MultScalar(-b);
         FOR i:=-NzSteps+1 TO NzSteps-1 DO
            FOR j:=1 TO NrhoSteps-1 DO
               BEGIN
                  Propeller.Update;
                  rho:=(j-0.5)*drho;
                  umTemp:=umNew.Value(j,NzSteps+i+1)
                          +(2/sqr(drho)+2/sqr(dz)+sqr(m/rho)
                              +zeta*Vmat.Value(j,NzSteps+i+1))
                                   *umOld.Value(j,NzSteps+i+1);
                  IF j>1 THEN
                     umTemp:=umTemp-umOld.Value(j-1,NzSteps+i+1)
                                     *(1/sqr(drho)-0.5/drho/rho)
                  ELSE
                     umTemp:=umTemp-umOld.Value(1,NzSteps+i+1)
                                     *(1/sqr(drho)-0.5/drho/rho);
                  umTemp:=umTemp-umOld.Value(j+1,NzSteps+i+1)
                                  *(1/sqr(drho)+0.5/drho/rho);
                  umTemp:=umTemp-(umOld.Value(j,NzSteps+i)
                                  +umOld.Value(j,NzSteps+i+2))/sqr(dz);
                  umNew.Put(j,NzSteps+i+1,umTemp);
               END;
      END;
END; {Multiply}

PROCEDURE ApplyLanczos(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   neigen,ne,temp,
   NumRows,NumCols,
   old,cur,
   nn,imin       :integer;
   alf,bta,
   eigen,ee      :DVector;
   a,b,OldE,E,
   sqrtMachEps,tol,
   res,min       :real;
   z             :DMatrix;
   um            :array [1..2] OF DMatrixExt;
   VecFile       :RfileType;
   WFkeys        :THotKeys;
   Wkey          :byte;
   Done,
   WishToReturn,
   WishToResume,
   FileExists    :boolean;
   Progress      :string;
   WFHelpScr     :HelpScrType;
   Propeller     :TWaitMessage;
BEGIN
   WFHelpScr[01]:='';
   WFHelpScr[02]:=' <F1>-show this help screen.   ';
   WFHelpScr[03]:='';
   WFHelpScr[04]:=' <F2>-pause in calculation.';
   WFHelpScr[05]:='    Can later resume from estimate of';
   WFHelpScr[06]:='    eigenfunction obtained so far.';
   FOR nn:=7 TO 25 DO WFHelpScr[nn]:='';

   {New(alf); New(bta); New(eigen); New(ee); {Mac only}
   {New(z); New(um[1]); New(um[2]); {Mac only}
   {New(Propeller); {Mac only}
   {New(WFkeys); {Mac only}
   alf.Init(WF.MaxNumIter); bta.Init(WF.MaxNumIter);
   eigen.Init(WF.MaxNumIter); ee.Init(WF.MaxNumIter);
   z.Init(WF.MaxNumIter,WF.MaxNumIter);
   WF.Psi.GetSize(NumRows,NumCols);
   um[1].Init(NumRows,NumCols); um[1].Equate(WF.Psi);
   WF.Psi.Free;
   um[2].Init(NumRows,NumCols); um[2].Fill(0.0);
   tol:=WF.tolerance*Potl.zeta;
   WF.Cwarn:=False; WF.ResumeLanczos:=False;
   sqrtMachEps:=sqrt(MachEps);
   old:=1; cur:=2; Done:=False;
   WITH WFkeys DO
      BEGIN
         Init(2);
         key[1]:='F1-Help';
         key[2]:='F2-Pause';
         Display;
      END;
   WishToReturn:=False;
   Propeller.Show;
   neigen:=0; b:=um[old].Norm(Potl.drho);
   ConnectRealFile(VecFile,'H-atom.tmp',FileExists); rewrite(VecFile);
   WHILE (NOT Done) AND (WF.NumIter<WF.MaxNumIter) AND (NOT WishToReturn)
    AND (b<>0.0) DO
      BEGIN
         Propeller.Update;
         neigen:=neigen+1; WF.NumIter:=WF.NumIter+1;
         um[old].MultScalar(1/b); um[old].WriteF(VecFile);
         Multiply(um[old],um[cur],b,WF,Potl,Propeller);
         a:=um[old].Projection(um[cur],Potl.drho); alf.Put(neigen,a);
         um[cur].AddMatrix(-a,um[old]);
         b:=um[cur].Norm(Potl.drho); bta.Put(neigen,b);
         temp:=old; old:=cur; cur:=temp; {switch storage for next pass}
         IF neigen=1 THEN
            BEGIN
               OldE:=a; E:=OldE;
            END
         ELSE
            BEGIN
               eigen.Equate(1.0,alf); ee.Equate(1.0,bta);
               Tridiag(neigen,eigen,ee,False,z);
               FindMin(neigen,eigen,E,imin);
               IF (abs(OldE-E)<tol) OR (b<sqrtMachEps) THEN Done:=True
               ELSE OldE:=E;
            END;
         WF.En:=E/Potl.zeta;
         Progress:=ConCat('Iteration: ',NumStr(WF.NumIter,3,0),
                          '   Energy:',Num2Str(Potl.V0*WF.En,6),
                          ' ',Potl.Eunit,'eV');
         WHILE Length(Progress)<36 DO Progress:=ConCat(Progress,' ');
         Comment(0.05,0.45,Progress);
         CheckForEvents;
         IF Event.ReadKey=Escape THEN WishToReturn:=True
         ELSE IF WFkeys.Pressed(Wkey) THEN
            CASE Wkey OF
               1 :Help(WFHelpScr);
               2 :BEGIN
                     WishToReturn:=True;
                     WF.ResumeLanczos:=True;
                  END;
               END; {case}
      END;
   Propeller.Update;
   IF (WishToReturn OR (WF.NumIter>=WF.MaxNumIter)) AND (NOT Done) THEN
         WF.Cwarn:=True; 
   IF WF.NumIter>=WF.MaxNumIter THEN WF.ResumeLanczos:=False;
   um[old].Free; WF.Psi.Init(NumRows,NumCols);
   IF neigen=1 THEN WF.Psi.Equate(um[cur]) 
   ELSE
      BEGIN
         eigen.Equate(1.0,alf); ee.Equate(1.0,bta);
         z.Fill(0.0);
         FOR ne:=1 TO neigen DO z.Put(ne,ne,1.0);
         Tridiag(neigen,eigen,ee,True,z);
         FindMin(neigen,eigen,min,imin);
         {construct Ritz vector - the eigenvector of original Hamiltonian}
         WF.Psi.Fill(0.0);
         reset(VecFile);
         FOR ne:=1 TO neigen DO
            BEGIN
               Propeller.Update;
               um[cur].ReadF(VecFile);
               WF.Psi.AddMatrix(z.Value(ne,imin),um[cur]);
            END;
      END;
   Close(VecFile); Erase(VecFile);
   um[cur].Free;
   z.Free; ee.Free; eigen.Free;
   bta.Free; alf.Free;
   Propeller.Hide;
   WFkeys.Clear;
   {Dispose(WFkeys); {Mac only}
   {Dispose(Propeller); {Mac only}
   {Dispose(um[2]); Dispose(um[1]); Dispose(z); {Mac only}
   {Dispose(ee); Dispose(eigen); Dispose(bta); Dispose(alf); {Mac only}
END; {ApplyLanczos}

PROCEDURE PrepPotl(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   i,j :integer;
   rho,z,r,r1,r2 :real;
BEGIN
   WITH Potl DO
      BEGIN
         Vmat.Fill(0.0);
         FOR i:=-NzSteps+1 TO NzSteps-1 DO
            BEGIN
               z:=i*dz;
               FOR j:=1 TO NrhoSteps-1 DO
                  BEGIN
                     rho:=(j-0.5)*drho;
                     CASE WF.Computed OF
                        Distorted :BEGIN
                                      r:=sqrt(rho*rho+z*z);
                                      Vmat.Put(j,NzSteps+i+1,
                                               charge*(2*Z1/r
                                                 -Efield*z
                                                  *(L0*Lfac*1.0e3)/(V0*Efac))
                                        +sqr(charge*Bfield*MTeslaConv*rho
                                              /sqr(mu*Mfac/1.0e6))/4);
                                   END;
                        Shared    :BEGIN
                                      r1:=sqrt(sqr(rho)
                                               +sqr(z+0.5*Separation));
                                      r2:=sqrt(sqr(rho)
                                              +sqr(z-0.5*Separation));
                                      Vmat.Put(j,NzSteps+i+1,
                                             charge*(2*Z1/r1+2*ExtCharge/r2));
                                   END;

                        END; {case}
                  END;
            END;
      END;
END; {PrepPotl}

PROCEDURE InitialGuess(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   i,j,absm       :integer;
   rho,z,r,ctheta,
   r1,r2,ct1,ct2  :real;
   Propeller      :TWaitMessage;
BEGIN
   {New(Propeller); {Mac only}
   WITH Potl,WF DO
      BEGIN
         Propeller.Show;
         NumIter:=0;
         Psi.Fill(0.0);
         absm:=abs(m);
         FOR i:=-NzSteps+1 TO NzSteps-1 DO
            BEGIN
               Propeller.Update;
               z:=i*dz;
               FOR j:=1 TO NrhoSteps-1 DO
                  BEGIN
                     rho:=(j-0.5)*drho;
                     CASE Computed OF
                        Distorted :BEGIN
                                      r:=sqrt(rho*rho+z*z);
                                      IF r=0.0 THEN ctheta:=1.0
                                      ELSE ctheta:=z/r;
                                      Psi.Put(j,NzSteps+i+1,
                                        RadialWF(absm+1,absm,-Z1*charge*r)
                                        *AssocLegendre(absm,absm,ctheta));
                                   end;
                        Shared    :BEGIN
                                      r1:=sqrt(sqr(rho)
                                               +sqr(z+0.5*Separation));
                                      r2:=sqrt(sqr(rho)
                                              +sqr(z-0.5*Separation));
                                      IF r1=0.0 THEN ct1:=1.0
                                      ELSE ct1:=(z+0.5*Separation)/r1;
                                      IF r2=0.0 THEN ct2:=1.0
                                      ELSE ct2:=(z-0.5*Separation)/r2;
                                      Psi.Put(j,NzSteps+i+1,
                                             (RadialWF(absm+1,absm,
                                                    abs(Z1*charge)*r1)
                                              *AssocLegendre(absm,absm,ct1)
                                          +RadialWF(absm+1,absm,
                                                    abs(ExtCharge*charge)*r2)
                                              *AssocLegendre(absm,absm,ct2)));
                                   END;
                        END; {case}
                  END;
            END;
         Propeller.Hide;
      END;
   {Dispose(Propeller); {Mac only}
END; {InitialGuess}

{--------Use chosen algorithm---------------------------------------}

PROCEDURE ComputeViaChosenAlg(VAR WF :WFtype; VAR Potl :PotlType;
                              VAR Nkeys :THotKeys);
BEGIN
   CASE WF.Algorithm OF
      Basis :BEGIN
                IF WF.Mode=Auto THEN Nkeys.Display;
                ApplyBasisFn(WF,Potl);
                WF.ResumeLanczos:=False;
             END;
      Lanczos :BEGIN
                  PrepPotl(WF,Potl);
                  ApplyLanczos(WF,Potl);
                  IF (NOT WF.ResumeLanczos) AND (WF.Mode=Auto) THEN
                     Nkeys.Display;
               END;
      END; {case Algorithm}
   PrepPsiSlice(WF,Potl);
   WF.Plotted:=False;
   IF NOT WF.RecordToFile THEN WF.Saved:=False;
END; {ComputeViaChosenAlg}

END. {HatmCalc}
