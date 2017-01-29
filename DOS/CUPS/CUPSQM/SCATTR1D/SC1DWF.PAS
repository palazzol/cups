{Unit of routines for 1-D wave functions.                             }
{Used by Scattr1D.pas.                                                }
{                                                                     }
{Author: John R. Hiller                                               }
{Version: 1.0                11/20/94                                 }
{Copyright (c) 1995 John Wiley & Sons, Inc.                           }
{                                                                     }
UNIT Sc1DWF;

INTERFACE

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,
     Sc1DMach,Sc1DCmn,Sc1DPotl;

CONST
   NumAlgs=3;
   fInit=-30.0;    {initial value for ln of magnitude of wave function}
   WheelRange=7;   {range of color wheel; one less than number of colors}

TYPE
   FormType=(RealInc,ImagInc,RealRefl,ImagRefl,RealIncRefl,ImagIncRefl,
             RealCntr,ImagCntr,RealTrans,ImagTrans,
             ProbIncRefl,ProbCntr,ProbTrans);
   WFtype=record
      Mag,
      Phase             :DVector;
      Energy,
      HorizMin,HorizMax,         {horizontal range for plot}
      VertMin,VertMax,           {vertical range for plot}
      MagMax,                    {normalization factor for plot}
      ReflAmp,
      ReflPhase,
      TransAmp,
      TransPhase,
      LogT          :real;
      nRSteps,                   {number of steps beyond x_max}
      nLSteps,                   {number of steps before x_min}
      PlotType      :integer;
      Zoom,
      SplitIncRefl,
      Computed      :boolean;
      PlotInfoScr   :InfoScrType       {text output screen for wave fn plot}
      END; {WFtype}

VAR
   WF            :WFtype;
   Log10e,                      {global value of log_10(e^1)}
   Evalue        :real;         {global value of energy parameter}
   FailedGlobal  :boolean;
   Wheel         :array [0..WheelRange] OF word;     {color wheel}

{--------Routines for numerical calculations--------------------------------}

PROCEDURE SolveSchrodinger(VAR Mag,Phase :DVector;
                           E :real; VAR Potl :PotlType; VAR Failed :boolean;
                           VAR RAmp,RPhase,TAmp,TPhase,LogTval :real);

{--------Routines for wave function computations and plots-------------------}

PROCEDURE SetWFDefaults(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE PrepWFPlotScr(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE PlotWaveFunction(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE ComputeWF(VAR WF :WFtype; VAR Potl :PotlType;
                    VAR MainMenu :TMenu; VAR MenuClicked :boolean);

IMPLEMENTATION

{--------Routines for numerical calculations--------------------------------}

PROCEDURE SolveSchrodinger1(NumSteps :integer;
                            VAR Varr,Xarr,Mag,Phase :DVector;
                            E :real; VAR Potl :PotlType;
                            VAR RAmp,RPhase,TAmp,TPhase,LogTval :real);
{Solve Schrodinger equation by the transfer matrix algorithm.}
VAR
   M,Minv :CMatrix;
   iPos,iStep :integer;
   ab,tempVec :CVector;
   k,c,s,ex,ex2,Vn,xn,xnp1,x,fac,a,b,phia,phib :real;
   ik,ikpsi,expC,expCinv,tempC,temp1C,temp2C,psi,psiPrime :complex;
BEGIN
   WITH Potl DO
      BEGIN
         {New(ab); New(tempVec); New(M); New(Minv); {Mac only}
         ab.Init(2); tempVec.Init(2); M.Init(2,2); Minv.Init(2,2);
         Vn:=Varr.Value(NumSteps+1); xn:=Xarr.Value(NumSteps+1);
         IF E>Vn THEN
            BEGIN
               k:=sqrt(zeta*(E-Vn));
               tempVec.Put(1,exp(fInit),0.0);
               tempVec.Put(2,0.0,k*exp(fInit))
            END
         ELSE IF E<Vn THEN
            BEGIN
               k:=sqrt(zeta*(Vn-E));
               tempVec.Put(1,exp(fInit),0.0);
               tempVec.Put(2,-k*exp(fInit),0.0)
            END
         ELSE {E=Vn}
            BEGIN
               tempVec.Put(1,exp(fInit),0.0);
               tempVec.Put(2,0.0,0.0)
            END;
         Mag.Put(nSteps+1,exp(fInit)); Phase.Put(nSteps+1,0.0);
         iStep:=nSteps; x:=xMax-dx; fac:=0.0;
         for iPos:=NumSteps downto 1 do
            BEGIN
               Vn:=Varr.Value(iPos); xnp1:=xn; xn:=Xarr.Value(iPos);
               IF E>Vn THEN
                  BEGIN
                     k:=sqrt(zeta*(E-Vn));
                     c:=cos(k*xnp1); s:=sin(k*xnp1);
                     Minv.Put(1,1,0.5*c,-0.5*s);
                     Minv.Put(1,2,-0.5*s/k,-0.5*c/k);
                     Minv.Put(2,1,0.5*c,0.5*s);
                     Minv.Put(2,2,-0.5*s/k,0.5*c/k);
                     Minv.MultVec(tempVec,ab);
                     WHILE (x>=xn) AND (iStep>0) DO
                        BEGIN
                           expC.Re:=cos(k*x); expC.Im:=sin(k*x);
                           ab.Get(1,temp1C);
                           CxOp(temp1C,'*',expC,temp1C);
                           ab.Get(2,temp2C);
                           CxOp(temp2C,'/',expC,temp2C);
                           CxOp(temp1C,'+',temp2C,tempC);
                           Mag.Put(iStep,CABS(tempC)*exp(fac));
                           Phase.Put(iStep,Arg(tempC));
                           iStep:=iStep-1; x:=(iStep-1)*dx+xMin
                        END;
                     c:=cos(k*xn); s:=sin(k*xn);
                     M.Put(1,1,c,s);
                     M.Put(1,2,c,-s);
                     M.Put(2,1,-k*s,k*c);
                     M.Put(2,2,-k*s,-k*c);
                     M.MultVec(ab,tempVec)
                  END
               ELSE IF E<Vn THEN
                  BEGIN
                     k:=sqrt(zeta*(Vn-E));
                     ex:=exp(-k*(xnp1-xn));
                     Minv.Put(1,1,0.5/ex,0.0);
                     Minv.Put(1,2,-0.5/k/ex,0.0);
                     Minv.Put(2,1,0.5*ex,0.0);
                     Minv.Put(2,2,0.5*ex/k,0.0);
                     Minv.MultVec(tempVec,ab);
                     WHILE (x>=xn) AND (iStep>0) DO
                        BEGIN
                           expC.Re:=exp(-k*(x-xn)); expC.Im:=0.0;
                           ab.Get(1,temp1C);
                           CxOp(temp1C,'*',expC,temp1C);
                           expCinv.Re:=1/expC.Re; expCinv.Im:=0.0;
                           ab.Get(2,temp2C);
                           CxOp(temp2C,'*',expCinv,temp2C);
                           CxOp(temp1C,'+',temp2C,tempC);
                           Mag.Put(iStep,CABS(tempC)*exp(fac)/ex);
                           Phase.Put(iStep,Arg(tempC));
                           iStep:=iStep-1; x:=(iStep-1)*dx+xMin
                        END;
                     M.Put(1,1,1.0,0.0);
                     M.Put(1,2,1.0,0.0);
                     M.Put(2,1,-k,0.0);
                     M.Put(2,2,k,0.0);
                     M.MultVec(ab,tempVec);
                     fac:=fac+k*(xnp1-xn)       {accumulate exponent}
                  END
               ELSE {E=Vn}
                  BEGIN
                     Minv.Put(1,1,1.0,0.0);
                     Minv.Put(1,2,-xnp1,0.0);
                     Minv.Put(2,1,0.0,0.0);
                     Minv.Put(2,2,1.0,0.0);
                     Minv.MultVec(tempVec,ab);
                     WHILE (x>=xn) AND (iStep>0) DO
                        BEGIN
                           ab.Get(2,temp2C);
                           RCprod(x,temp2C,temp2C);
                           ab.Get(1,temp1C);
                           CxOp(temp1C,'+',temp2C,tempC);
                           Mag.Put(iStep,CABS(tempC)*exp(fac));
                           Phase.Put(iStep,Arg(tempC));
                           iStep:=iStep-1; x:=(iStep-1)*dx+xMin
                        END;
                     M.Put(1,1,1.0,0.0);
                     M.Put(1,2,xn,0.0);
                     M.Put(2,1,0.0,0.0);
                     M.Put(2,2,1.0,0.0);
                     M.MultVec(ab,tempVec);
                  END
            END; {for iPos}
         tempVec.Get(1,psi); tempVec.Get(2,psiPrime);
         k:=sqrt(zeta*E); ik.Re:=0.0; ik.Im:=k;
         CxOp(ik,'*',psi,ikpsi);
         CxOp(ikpsi,'+',psiPrime,tempC);
         a:=CABS(tempC)/(2*k); phia:=Arg(tempC)-cPi/2-k*xMin;
         CxOp(ikpsi,'-',psiPrime,tempC);
         b:=CABS(tempC)/(2*k); phib:=Arg(tempC)-cPi/2+k*xMin;
         RAmp:=b/a; RPhase:=phib-phia;
         {Reset amplitudes and phases to have standard incident wave.}
         Mag.MultScalar(exp(-fac)/a); Phase.AddScalar(-phia);
         IF E>Vinfty THEN
            BEGIN
               TAmp:=exp(fInit-fac)/a;
               TPhase:=-phia-sqrt(zeta*(E-Vinfty))*xMax
            END
         ELSE IF E=Vinfty THEN
            BEGIN
               TAmp:=exp(fInit-fac)/a; TPhase:=-phia
            END
         ELSE
            BEGIN
               TAmp:=exp(sqrt(zeta*(Vinfty-E))*xMax+fInit-fac)/a;
               TPhase:=-phia
            END;
         IF E>Vinfty THEN
            LogTval:=0.5*Log10((E-Vinfty)/E)
                     -2*Log10(a)+2*(fInit-fac)*Log10e
         ELSE LogTval:=1.0;   {represents -infinity}
         Minv.Free; M.Free; tempVec.Free; ab.Free;
         {Dispose(Minv); Dispose(M); {Mac only}
         {Dispose(tempVec); Dispose(ab); {Mac only}
      END
END; {SolveSchrodinger1}

PROCEDURE Schrod2(x :real; VAR psi,psiP :Dvector);
FAR; {PC only}
{Procedure used by StepRK4 when called from SolveSchrodinger2.}
{Uses global variable Evalue and global record Potl.}
BEGIN
   psiP.Put(1,psi.Value(2));
   psiP.Put(2,Potl.zeta*(Vfunc(x,Potl)-Evalue)*psi.Value(1))
END; {Schrod2}

PROCEDURE SolveSchrodinger2(VAR Mag,Phase :DVector;
                            E :real; VAR Potl :PotlType;
                            VAR RAmp,RPhase,TAmp,TPhase,LogTval :real);
{Solve Schrodinger equation by direct integration.}
VAR
   ik,ikpsi,psiC,psiPrimeC,tempC :complex;
   iStep :integer;
   k,x,negdx,a,b,phia,phib :real;
   psi :DVector;
BEGIN
   WITH Potl DO
      BEGIN
         {New(psi); {Mac only}
         psi.Init(2);
         Evalue:=E;           {set global variables for use by Schrod2}
         psi.Put(1,exp(fInit)); {integrate real part}
         IF E>Vinfty THEN psi.Put(2,0.0)
         ELSE psi.Put(2,-exp(fInit)*sqrt(zeta*(Vinfty-E)));
         Mag.Put(nSteps+1,exp(fInit)); Phase.Put(nSteps+1,0.0);
         x:=xMax; negdx:=-dx;
         FOR iStep:=nSteps DOWNTO 1 DO   {use Mag for temporary storage}
            BEGIN
               StepRK4(Schrod2,x,negdx,psi);
               Mag.Put(iStep,psi.Value(1));  Phase.Put(iStep,0.0);
            END;
         psiPrimeC.Re:=psi.Value(2);     {save real part of last psi'}
         IF E>Vinfty THEN                {integrate imaginary part}
            BEGIN
               psi.Put(1,0.0);
               psi.Put(2,exp(finit)*sqrt(zeta*(E-Vinfty)));
               x:=xMax;
               FOR iStep:=nSteps DOWNTO 1 DO
                  BEGIN
                     StepRK4(Schrod2,x,negdx,psi);
                     psiC.Re:=Mag.Value(iStep);
                     psiC.Im:=psi.Value(1);
                     Mag.Put(iStep,CABS(psiC));
                     Phase.Put(iStep,Arg(psiC))
                  END;
               psiPrimeC.Im:=psi.Value(2)
            END
         ELSE {E<=Vinfty}
            BEGIN
               psiC.Re:=Mag.Value(1); psiC.Im:=0.0;
               psiPrimeC.Im:=0.0;
               FOR iStep:=1 TO nSteps DO
                  IF Mag.Value(iStep)<0.0 THEN
                     BEGIN
                        Mag.Put(iStep,-Mag.Value(iStep));
                        Phase.Put(iStep,-cPi);
                     END
            END;
         psi.Free;
         {Dispose(psi); {Mac only}
         k:=sqrt(zeta*E); ik.Re:=0.0; ik.Im:=k;
         CxOp(ik,'*',psiC,ikpsi);
         CxOp(ikpsi,'+',psiPrimeC,tempC);
         a:=CABS(tempC)/(2*k); phia:=Arg(tempC)-cPi/2-k*xMin;
         CxOp(ikpsi,'-',psiPrimeC,tempC);
         b:=CABS(tempC)/(2*k); phib:=Arg(tempC)-cPi/2+k*xMin;
         RAmp:=b/a; RPhase:=phib-phia;
         {Reset amplitudes and phases to have standard incident wave.}
         Mag.MultScalar(1/a); Phase.AddScalar(-phia);
         IF E>Vinfty THEN
            BEGIN
               TAmp:=exp(fInit)/a;
               TPhase:=-phia-sqrt(zeta*(E-Vinfty))*xMax
            END
         ELSE IF E=Vinfty THEN
            BEGIN
               TAmp:=exp(fInit)/a; TPhase:=-phia
            END
         ELSE
            BEGIN
               TAmp:=exp(sqrt(zeta*(Vinfty-E))*xMax+fInit)/a;
               TPhase:=-phia
            END;
         IF E>Vinfty THEN
            BEGIN
               LogTval:=0.5*Log10((E-Vinfty)/E)-2*Log10(a)+2*fInit*Log10e;
               IF LogTval>0.0 THEN
                  IF sqr(RAmp)<1.0 THEN LogTval:=Log10(1-sqr(RAmp))
                  ELSE LogTVal:=1.0;
            END
         ELSE LogTVal:=1.0; {represents -infinity}
      END;
END; {SolveSchrodinger2}

PROCEDURE Schrod3(x :real; VAR fg,fgPrime :DVector);
FAR; {PC only}
{Procedure used by StepRK4 when called from SolveSchrodinger3.}
{Uses global variables Evalue and FailedGlobal, global record Potl,}
{and global constant fInit.}
VAR
   f,fcap,g,gcap,VminusE :real; {wave fn=exp(f+ig), fcap=f', gcap=g'}
BEGIN
   f:=fg.Value(1); fcap:=fg.Value(2); g:=fg.Value(3);
   IF (fcap>exp(-fInit)) OR (f>80.0) OR (f-fInit<-40.0) THEN
      BEGIN
         FailedGlobal:=True;
         f:=1.0; fcap:=-1.0; gcap:=0.0;
      END
   ELSE IF Evalue>Potl.Vinfty THEN
      gcap:=sqrt(Potl.zeta*(Evalue-Potl.Vinfty))*exp(-2*(f-fInit))
   ELSE gcap:=0.0;
   VminusE:=Potl.zeta*(Vfunc(x,Potl)-Evalue);
   fgPrime.Put(1,fcap);
   fgPrime.Put(2,VminusE-sqr(fcap)+sqr(gcap));
   fgPrime.Put(3,gcap);
END; {Schrod3}

PROCEDURE SolveSchrodinger3(VAR Mag,Phase :DVector;
                           E :real; VAR Potl :PotlType; VAR Failed :boolean;
                           VAR RAmp,RPhase,TAmp,TPhase,LogTval :real);
{Solve Schrodinger equation by integration of equations}
{for the log of psi.}
VAR
   iStep :integer;
   fg    :DVector;
   k,x,negdx,f,fcap,g,gcap,a,phia,phib :real;
BEGIN
   Evalue:=E;           {set global variables for use by Schrod3}
   FailedGlobal:=False;
   WITH Potl DO
      BEGIN
         {New(fg); {Mac only}
         fg.Init(3);
         {Initial values matched to transmitted wave on right.}
         fg.Put(1,fInit); fg.Put(3,0.0);
         IF Vinfty<=E THEN fg.Put(2,0.0)
         ELSE fg.Put(2,-sqrt(zeta*(Vinfty-E)));
         Mag.Put(nSteps+1,exp(fg.Value(1)));
         Phase.Put(nSteps+1,fg.Value(3));
         x:=xMax; negdx:=-dx;
         iStep:=nSteps;
         StepRK4(Schrod3,x,negdx,fg);
         WHILE (iStep>1) AND (NOT FailedGlobal) DO
            BEGIN
               f:=fg.Value(1);
               IF f>80.0 THEN FailedGlobal:=True
               ELSE
                  BEGIN
                     IF f<-80.0 THEN Mag.Put(iStep,0.0)
                     ELSE Mag.Put(iStep,exp(fg.Value(1)));
                     Phase.Put(iStep,fg.Value(3));
                     StepRK4(Schrod3,x,negdx,fg);
                     iStep:=iStep-1;
                  END;
            END;
         IF FailedGlobal THEN
            BEGIN
               Failed:=True;
               Announce('Integration-of-log algorithm has failed.');
            END
         ELSE
            BEGIN
               Mag.Put(1,exp(fg.Value(1)));
               Phase.Put(1,fg.Value(3));
               {Extract incident and reflected components on left.}
               f:=fg.Value(1); fcap:=fg.Value(2); g:=fg.Value(3);
               IF E>Vinfty THEN
                  gcap:=sqrt(zeta*(E-Vinfty))*exp(-2*(f-fInit))
               ELSE gcap:=0.0;
               k:=sqrt(zeta*E);
               a:=Mag.Value(1)*sqrt(sqr(fcap)+sqr(gcap+k))/(2*k);
               IF a=0.0 THEN
                  BEGIN
                     Failed:=True;
                     Announce('Integration-of-log algorithm has failed.');
                  END
               ELSE
                  BEGIN
                     RAmp:=sqrt((sqr(fcap)+sqr(gcap-k))
                            /(sqr(fcap)+sqr(gcap+k)));
                     phia:=g+ArcTan2(fcap,gcap+k)-cPi/2-k*xMin;
                     IF RAmp=0.0 THEN phib:=0.0
                     ELSE phib:=g+ArcTan2(-fcap,-gcap+k)-cPi/2+k*xMin;
                     RPhase:=phib-phia;
                     {Reset amplitudes and phases}
                     {to have standard incident wave.}
                     Mag.MultScalar(1/a);
                     Phase.AddScalar(-phia);
                     IF E>Vinfty THEN
                        BEGIN
                           TAmp:=exp(fInit)/a;
                           TPhase:=-phia-sqrt(zeta*(E-Vinfty))*xMax
                        END
                     ELSE IF E=Vinfty THEN
                        BEGIN
                           TAmp:=exp(fInit)/a; TPhase:=-phia
                        END
                     ELSE
                        BEGIN
                           TAmp:=exp(sqrt(zeta*(Vinfty-E))*xMax+fInit)/a;
                           TPhase:=-phia
                        END;
                     {Compute log_10 of transmission probability.}
                     IF E>Vinfty THEN
                        BEGIN
                           LogTval:=0.5*Log10((E-Vinfty)/E)+2*fInit*Log10e
                                    -2*(f*Log10e-Log10(2*k)
                                    +0.5*Log10(sqr(fcap)+sqr(gcap+k)));
                        END
                     ELSE LogTval:=1;  {Use value > 0 to indicate -infinity.}
                  END;
            END;
         fg.Free;
         {Dispose(fg); {Mac only}
      END
END; {SolveSchrodinger3}

PROCEDURE SolveSchrodinger(VAR Mag,Phase :DVector;
                           E :real; VAR Potl :PotlType; VAR Failed :boolean;
                           VAR RAmp,RPhase,TAmp,TPhase,LogTval :real);
{Solve Schrodinger equation by selected algorithm.}
VAR
   NumSteps,n :integer;
   Vn,Vnm1 :real;
   Varr,Xarr :DVector;
BEGIN
   Failed:=False;
   Phase.Free; Mag.Free;
   Mag.Init(Potl.nSteps+1); Phase.Init(Potl.nSteps+1);
   CASE Potl.Alg OF
      0 :with Potl DO
            BEGIN
               {New(Varr); New(Xarr); {Mac only}
               NumSteps:=1+NumParams DIV 2;
               Varr.Init(NumSteps+1); Xarr.Init(NumSteps+1);
               IF IDcode='Step' THEN Varr.Put(1,Invert*Params[1])
               ELSE Varr.Put(1,Invert*1.0);
               Xarr.Put(1,xMin);
               FOR n:=1 TO NumSteps-1 DO
                  if IDcode='Step' then
                     BEGIN
                        Varr.Put(n+1,Invert*Params[2*n-1]);
                        Xarr.Put(n+1,Params[2*n]);
                     END
                  ELSE
                     BEGIN
                        Varr.Put(n+1,Invert*Params[2*n]);
                        Xarr.Put(n+1,Params[2*n-1]);
                     END;
               Varr.Put(NumSteps+1,Vinfty); Xarr.Put(NumSteps+1,xMax);
               SolveSchrodinger1(NumSteps,Varr,Xarr,Mag,Phase,E,Potl,
                                 RAmp,RPhase,TAmp,TPhase,LogTval);
               Xarr.Free; Varr.Free;
               {Dispose(Xarr); Dispose(Varr); {Mac only}
            END;
      1 :with Potl DO
            BEGIN
               {New(Varr); New(Xarr); {Mac only}
               Varr.Init(nSteps+1); Xarr.Init(nSteps+1); Vn:=0.0;
               FOR n:=1 TO nSteps DO
                  BEGIN
                     Vnm1:=Vn; Vn:=V.Value(n+1);
                     Varr.Put(n,0.5*(Vnm1+Vn));
                     Xarr.Put(n,(n-1)*dx+xMin)
                  END;
               Varr.Put(nSteps+1,Vinfty); Xarr.Put(nSteps+1,xMax);
               SolveSchrodinger1(nSteps,Varr,Xarr,Mag,Phase,E,Potl,
                                 RAmp,RPhase,TAmp,TPhase,LogTval);
               Xarr.Free; Varr.Free;
               {Dispose(Xarr); Dispose(Varr); {Mac only}
            END;
      2 :SolveSchrodinger2(Mag,Phase,E,Potl,
                           RAmp,RPhase,TAmp,TPhase,LogTval);
      3 :SolveSchrodinger3(Mag,Phase,E,Potl,Failed,
                           RAmp,RPhase,TAmp,TPhase,LogTval)
   END; {case}
END; {SolveSchrodinger}

{--------Routines for wave function computations and plots-------------------}

PROCEDURE SetWFDefaults(VAR WF :WFtype; VAR Potl :PotlType);
BEGIN
   WITH WF DO                  {prep wave function record}
      BEGIN
         Energy:=1.25;
         PlotType:=1;          {show magnitude and phase}
         SplitIncRefl:=False;  {do not split incident and reflected parts}
         Computed:=False;
         Zoom:=False
      END
END; {SetWFDefaults}

PROCEDURE PrepWFPlotScr(VAR WF :WFtype; VAR Potl :PotlType);
{Prepare Info screen with results for trans/refl probabilities.}
VAR
   n   :integer;
   R,T :real;
BEGIN
   WITH WF,Potl DO
      BEGIN
         FOR n:=1 TO NumParams+7 DO WF.PlotInfoScr[n]:=ParamList[n];
         WF.PlotInfoScr[NumParams+8]:=' Wave fn parameters';
         WF.PlotInfoScr[NumParams+9]:=ConCat('  Energy: ',
                                      Num2Str(V0*Energy,6),Eunit,'eV');
         IF Energy>Vinfty THEN
            BEGIN
               R:=sqr(ReflAmp); T:=1-R
            END
         ELSE BEGIN R:=1; T:=0 END;
         WF.PlotInfoScr[NumParams+10]:=ConCat('       R: ',Num2Str(R,6));
         WF.PlotInfoScr[NumParams+11]:=ConCat('       T: ',Num2Str(T,6));
         IF Energy>Vinfty THEN
            WF.PlotInfoScr[NumParams+12]:=ConCat('   Log T: ',
                                          Num2Str(LogT,6))
         ELSE WF.PlotInfoScr[NumParams+12]:='';
         WF.PlotInfoScr[NumParams+13]:='';
         WF.PlotInfoScr[NumParams+14]:=(' Algorithm: ');
         CASE Alg OF
            0 :WF.PlotInfoScr[NumParams+15]:='  piecewise constant';
            1 :WF.PlotInfoScr[NumParams+15]:='  transfer matrix';
            2 :WF.PlotInfoScr[NumParams+15]:='  direct integration';
            3 :WF.PlotInfoScr[NumParams+15]:='  integration of log'
            END; {case}
         FOR n:=NumParams+16 TO 25 DO WF.PlotInfoScr[n]:=''
      END
END; {PrepWFPlotScr}

FUNCTION Mod2Pi(val :real) :real;
{Compute remainder when divided by 2*pi.}
{Uses the global variable cPi, which is equal to pi.}
VAR
   temp :real;
BEGIN
   temp:=val/(2*cPi);
   IF temp<0.0 THEN REPEAT temp:=temp+1.0 UNTIL temp>0.0
   else while temp>=1.0 do temp:=temp-1.0;
   Mod2Pi:=temp;
END; {Mod2Pi}

FUNCTION ColorWheel(val :real; VAR NumColors :integer) :word;
{Convert val to color on color wheel.}
{Uses the global array Wheel, which contains color values,}
{and the global constant WheelRange that sets the length of Wheel.}
VAR
   index :integer;
BEGIN
   NumColors:=WheelRange+1;
   index:=trunc(NumColors*Mod2Pi(val)+0.5);
   IF index=NumColors THEN index:=0;
   ColorWheel:=Wheel[index]
END; {ColorWheel}

PROCEDURE WFLegend(VAR WF :WFtype);
{Display legend for plot of wave function.}
VAR
   NumColors,n,sx,sy,temp,LegLine :integer;
   Color,height :word;
   delta :real;
BEGIN
   WITH WF DO
      BEGIN
         OpenViewPort(1);
         ClearViewPort;
         SelectScale(1);
         height:=HeightFactor*TextHeight('O');
         CASE PlotType OF
            1 :BEGIN  {magnitude and phase as color}
                  HideMouse;
                  Color:=ColorWheel(0.0,NumColors);
                  delta:=360.0/NumColors;
                  Map(WheelX,WheelY,sx,sy);
                  Color:=ColorWheel(0.0,temp);
                  SetFillStyle(SolidFill,Color); SetColor(Color);
                  PieSlice(sx,sy,round(360-0.5*delta),360,WheelRadius);
                  PieSlice(sx,sy,0,round(0.5*delta),WheelRadius);
                  FOR n:=1 TO NumColors-1 DO
                     BEGIN
                        Color:=ColorWheel(2*cPi*n*delta/360,temp);
                        SetFillStyle(SolidFill,Color); SetColor(Color);
                        PieSlice(sx,sy,round((n-0.5)*delta),
                                 round((n+0.5)*delta),WheelRadius)
                     END;
                  SetColor(Whitte);
                  OutTextXY(sx-WheelRadius-height,
                            sy-WheelRadius-height,'phase:');
                  OutTextXY(sx+WheelRadius+1,
                            sy-(height DIV (2*HeightFactor))+(HeightFactor-1),
                            '-0');
                  LegLine:=8;
                  ShowMouse;
               END;
            2 :BEGIN  {magnitude and phase in line plot}
                  IF SplitIncRefl THEN
                     BEGIN
                        SetColor(Whitte);
                        OutTextXY(1,height,'solid  - magnitude');
                        OutTextXY(1,2*height,'dotted - phase');
                        LegendEntry(3,LightRed,'incident');
                        LegendEntry(4,LightGreen,'reflected');
                        LegendEntry(5,LightMagenta,'interior');
                        OutTextXY(30,6*height,'& transmitted');
                        LegLine:=7;
                     END
                  ELSE
                     BEGIN
                        SetColor(Whitte);
                        OutTextXY(1,height,'solid  - magnitude');
                        OutTextXY(1,2*height,'dotted - phase');
                        LegendEntry(3,LightMagenta,'wave fn');
                        LegLine:=4;
                     END
               END;
            3 :BEGIN  {real and imaginary parts}
                  IF SplitIncRefl THEN
                     BEGIN
                        SetColor(Whitte);
                        OutTextXY(1,height,'solid  - real part');
                        OutTextXY(1,2*height,'dotted - imag. part');
                        LegendEntry(3,LightRed,'incident');
                        LegendEntry(4,LightGreen,'reflected');
                        LegendEntry(5,LightMagenta,'interior');
                        OutTextXY(30,6*height,'& transmitted');
                        LegLine:=7;
                     END
                  ELSE
                     BEGIN
                        SetColor(Whitte);
                        OutTextXY(1,height,'solid  - real part');
                        OutTextXY(1,2*height,'dotted - imag. part');
                        LegendEntry(3,LightMagenta,'wave fn');
                        LegLine:=4;
                     END
               END;
            4 :BEGIN  {probability density}
                  IF SplitIncRefl THEN
                     BEGIN
                        LegendEntry(1,LightRed,'incident');
                        LegendEntry(2,LightGreen,'reflected');
                        LegendEntry(3,LightMagenta,'interior');
                        OutTextXY(30,4*height,'& transmitted');
                        LegLine:=5;
                     END
                  ELSE
                     BEGIN
                        LegendEntry(1,LightMagenta,'wave fn');
                        LegLine:=2;
                     END;
               END;
            END; {case}
         LegendEntry(LegLine,LightBlue,'x_min, x_max'); LegLine:=LegLine+1;
         LegendEntry(LegLine,LightCyan,'potential'); LegLine:=LegLine+1;
         LegendEntry(LegLine,Yellow,'energy');
         SetColor(Whitte);
      END;
END; {WFLegend}

PROCEDURE PlotDots(Color :word; x1,y1,x2,y2 :real);
VAR
   sx,sy,sx2,sy2,nPixel,nP :integer;
   dx,dy,dist :real;
   Skip       :boolean;
BEGIN
   Skip:=False;
   Map(x1,y1,sx,sy); Map(x2,y2,sx2,sy2);
   dist:=sqrt(sqr(1.0*(sx2-sx))+2*sqr(1.0*(sy2-sy))/3);
   IF sx2<>sx THEN nPixel:=trunc(dist/(sx2-sx))
   ELSE IF sy2<>sy THEN nPixel:=trunc(dist/(sy2-sy))
   ELSE Skip:=True;
   IF nPixel=0 THEN Skip:=True;
   IF NOT Skip THEN
      BEGIN
         dx:=(x2-x1)/nPixel; dy:=(y2-y1)/nPixel;
         FOR nP:=1 TO nPixel DO
           BEGIN
              Map(x1+nP*dx,y1+nP*dy,sx,sy);
              PutPixel(sx,sy,Color);
           END;   
         PutPixel(sx2,sy2,Color);
      END;
END; {PlotDots}

FUNCTION Func(x :real; step :integer; k :real; Form :FormType) :real;
{Needs global records Potl and WF.}
BEGIN
   CASE Form OF
      RealInc :Func:=cos(k*x)/WF.MagMax;
      ImagInc :Func:=sin(k*x)/WF.MagMax;
      RealRefl :Func:=WF.ReflAmp*cos(WF.ReflPhase-k*x)/WF.MagMax;
      ImagRefl :Func:=WF.ReflAmp*sin(WF.ReflPhase-k*x)/WF.MagMax;
      RealIncRefl :Func:=(cos(k*x)+WF.ReflAmp*cos(WF.ReflPhase-k*x))
                                          /WF.MagMax;
      ImagIncRefl :Func:=(sin(k*x)+WF.ReflAmp*sin(WF.ReflPhase-k*x))
                                          /WF.MagMax;
      RealCntr :Func:=WF.Mag.Value(step)*cos(WF.Phase.Value(step))/WF.MagMax;
      ImagCntr :Func:=WF.Mag.Value(step)*sin(WF.Phase.Value(step))/WF.MagMax;
      RealTrans :IF Potl.Vinfty<=WF.Energy THEN
                    Func:=WF.TransAmp*cos(WF.TransPhase+k*x)/WF.MagMax
                  ELSE
                     Func:=WF.TransAmp*cos(WF.TransPhase)*exp(-k*x)/WF.MagMax;
      ImagTrans :IF Potl.Vinfty<=WF.Energy THEN
                    Func:=WF.TransAmp*sin(WF.TransPhase+k*x)/WF.MagMax
                 ELSE
                    Func:=WF.TransAmp*sin(WF.TransPhase)*exp(-k*x)/WF.MagMax;
      ProbIncRefl :Func:=(1+sqr(WF.ReflAmp)
                        +2*WF.ReflAmp*cos(WF.ReflPhase-2*k*x))/sqr(WF.MagMax);
      ProbCntr :Func:=sqr(WF.Mag.Value(step)/WF.MagMax);
      ProbTrans :Func:=sqr(WF.TransAmp*exp(-k*x)/WF.MagMax);
   END; {case}
END; {Func}

PROCEDURE PlotFuncDots(Color :word; xBegin,xStep,L0 :real;
                       NumSteps :integer; Param :real;
                       Form :FormType);
VAR
   n,sx,sy :integer;
   x1,x2,y1,y2 :real;
BEGIN
   x2:=xBegin; y2:=Func(x2,1,Param,Form);
   Map(L0*x2,y2,sx,sy); PutPixel(sx,sy,Color);
   FOR n:=1 TO NumSteps DO
      BEGIN
         x1:=x2; y1:=y2;
         x2:=x2+xStep; y2:=Func(x2,n+1,Param,Form);
         PlotDots(Color,L0*x1,y1,L0*x2,y2);
      END;
END; {PlotFuncDots}

PROCEDURE PlotFunc(Color :word; xBegin,xStep,L0 :real; NumSteps :integer;
                   Param :real; Form :FormType);
VAR
   n :integer;
   x1,x2,y1,y2 :real;
BEGIN
   SetColor(Color);
   x2:=xBegin; y2:=Func(x2,1,Param,Form);
   FOR n:=1 TO NumSteps DO
      BEGIN
         x1:=x2; y1:=y2;
         x2:=x2+xStep; y2:=Func(x2,n+1,Param,Form);
         PlotLine(L0*x1,y1,L0*x2,y2)
      END
END; {PlotFunc}

PROCEDURE PlotMagPhaseColor(x,dx,amp1,amp2,phi,ref :real);
VAR
   num,sx0,sy0,sx1,sx2,sy1,sy2 :integer;
   Color :word;
BEGIN
   Color:=ColorWheel(phi,num);
   SetFillStyle(SolidFill,Color);
   SetLineStyle(SolidLn,0,NormWidth); SetColor(Color);
   Map(0.0,ref,sx0,sy0);
   Map(x,amp1+ref,sx1,sy1);
   Map(x+dx,amp2+ref,sx2,sy2);
   PlotTrapezoid(sx1,sx2,sy0,sy1,sy2);
END; {PlotMagPhaseColor}

PROCEDURE PlotMagPhaseLine(Color :word; x,dx,amp1,phi1,amp2,phi2 :real);
VAR
   sx,sy :integer;
BEGIN
   SetColor(Color);
   PlotLine(x,amp1,x+dx,amp2);
   Map(x,Mod2Pi(phi1),sx,sy); PutPixel(sx,sy,Color);
   PlotDots(Color,x,Mod2Pi(phi1),x+dx,Mod2Pi(phi2));
   SetColor(Whitte);
END; {PlotMagPhaseLine}

PROCEDURE MagPhase(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   n,sx,sy :integer;
   x,k,TransMom,Amp1,Amp2,Phi,Phi2 :real;
BEGIN
   HideMouse;
   WITH WF,Potl DO
      BEGIN
         k:=sqrt(zeta*Energy);
         TransMom:=sqrt(zeta*abs(Energy-Potl.Vinfty));
         IF SplitIncRefl THEN
            BEGIN
               FOR n:=1 TO WF.nLSteps DO
                  BEGIN
                     x:=(n-1)*dx+WF.HorizMin/L0;
                     Amp1:=1.0/MagMax; Amp2:=Amp1;
                     if PlotType=1 then
                        PlotMagPhaseColor(L0*x,L0*dx,Amp1,Amp2,k*x,0.0)
                     ELSE
                        PlotMagPhaseLine(LightRed,L0*x,L0*dx,
                                         Amp1,k*x,Amp2,k*(x+dx));
                  END;
               FOR n:=WF.nLSteps DOWNTO 1 DO
                  BEGIN
                     x:=(n-1)*dx+WF.HorizMin/L0;
                     Amp1:=ReflAmp/MagMax; Amp2:=Amp1;
                     if PlotType=1 then
                        PlotMagPhaseColor(L0*x,L0*dx,Amp1,Amp2,
                                          ReflPhase-k*x,-1.0)
                     ELSE
                        PlotMagPhaseLine(LightGreen,L0*x,L0*dx,
                                         Amp1,ReflPhase-k*x,
                                         Amp2,ReflPhase-k*(x+dx));
                  END;
               IF PlotType=1 THEN
                  BEGIN
                     Map(L0*xMin,-1.0,sx,sy);
                     SetColor(Whitte);
                     OutTextXY(sx+3,sy,'reflected');
                  END;
            END
         ELSE
            BEGIN
               FOR n:=1 TO WF.nLSteps DO
                  BEGIN
                     x:=(n-1)*dx+WF.HorizMin/L0;
                     Amp1:=sqrt(1+sqr(ReflAmp)
                           +2*ReflAmp*cos(ReflPhase-2*k*x))/MagMax;
                     Amp2:=sqrt(1+sqr(ReflAmp)
                           +2*ReflAmp*cos(ReflPhase-2*k*(x+dx)))/MagMax;
                     Phi:=ArcTan2(cos(k*x)+ReflAmp*cos(ReflPhase-k*x),
                                  sin(k*x)+ReflAmp*sin(ReflPhase-k*x));
                     if PlotType=1 then
                        PlotMagPhaseColor(L0*x,L0*dx,Amp1,Amp2,Phi,0.0)
                     ELSE
                        BEGIN
                           Phi2:=ArcTan2(cos(k*(x+dx))
                                         +ReflAmp*cos(ReflPhase-k*(x+dx)),
                                         sin(k*(x+dx))
                                         +ReflAmp*sin(ReflPhase-k*(x+dx)));
                           PlotMagPhaseLine(LightMagenta,L0*x,L0*dx,
                                            Amp1,Phi,Amp2,Phi2);
                        END;
                  END
            END;
         FOR n:=1 TO nSteps DO
            BEGIN
               x:=(n-1)*dx+xMin;
               Amp1:=Mag.Value(n)/MagMax; Amp2:=Mag.Value(n+1)/MagMax;
               Phi:=Phase.Value(n);
               IF PlotType=1 THEN
                  PlotMagPhaseColor(L0*x,L0*dx,Amp1,Amp2,Phi,0.0)
               ELSE
                  PlotMagPhaseLine(LightMagenta,L0*x,L0*dx,
                                   Amp1,Phi,Amp2,Phase.Value(n+1));
            END;
         FOR n:=1 TO WF.nRSteps DO
            BEGIN
               x:=(n-1)*dx+xMax;
               IF Vinfty>Energy THEN
                  BEGIN
                     Amp1:=TransAmp*exp(-TransMom*x)/MagMax;
                     Amp2:=TransAmp*exp(-TransMom*(x+dx))/MagMax;
                     Phi:=TransPhase; Phi2:=Phi;
                  END
               ELSE
                  BEGIN
                     Amp1:=TransAmp/MagMax; Amp2:=Amp1;
                     Phi:=TransPhase+TransMom*x;
                     Phi2:=Phi+TransMom*dx;
                  END;
               IF PlotType=1 THEN
                  PlotMagPhaseColor(L0*x,L0*dx,Amp1,Amp2,Phi,0.0)
               ELSE
                  PlotMagPhaseLine(LightMagenta,L0*x,L0*dx,
                                   Amp1,Phi,Amp2,Phi2);
            END;
      END;
   ShowMouse;
END; {MagPhase}

PROCEDURE RealImag(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   n :integer;
   x,k,TransMom :real;
BEGIN
   WITH WF,Potl DO
      BEGIN
         k:=sqrt(zeta*Energy); TransMom:=sqrt(zeta*abs(Energy-Vinfty));
         IF WF.HorizMin/L0<xMin THEN
            BEGIN
               IF SplitIncRefl THEN
                  BEGIN
                     PlotFunc(LightRed,WF.HorizMin/L0,dx,L0,
                              WF.nLSteps,k,RealInc);
                     PlotFunc(LightGreen,WF.HorizMin/L0,dx,L0,
                              WF.nLSteps,k,RealRefl);
                     PlotFuncDots(LightRed,WF.HorizMin/L0,dx,L0,
                                  WF.nLSteps,k,ImagInc);
                     PlotFuncDots(LightGreen,WF.HorizMin/L0,dx,L0,
                                 WF.nLSteps,k,ImagRefl)
                  END
               ELSE
                  BEGIN
                     PlotFunc(LightMagenta,WF.HorizMin/L0,dx,L0,
                              WF.nLSteps,k,RealIncRefl);
                     PlotFuncDots(LightMagenta,WF.HorizMin/L0,dx,L0,
                                  WF.nLSteps,k,ImagIncRefl)
                  END
            END;
         PlotFunc(LightMagenta,xMin,dx,L0,nSteps,k,RealCntr);
         PlotFuncDots(LightMagenta,xMin,dx,L0,nSteps,k,ImagCntr);
         IF WF.HorizMax/L0>xMax THEN
            BEGIN
               PlotFunc(LightMagenta,xMax,dx,L0,
                        WF.nRSteps,TransMom,RealTrans);
               PlotFuncDots(LightMagenta,xMax,dx,L0,
                            WF.nRSteps,TransMom,ImagTrans)
            END
   END
END; {RealImag}

PROCEDURE Probability(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   Prob,k,TransMom,Skip :real;
BEGIN
   WITH WF,Potl DO
      BEGIN
         k:=sqrt(zeta*Energy); TransMom:=sqrt(zeta*abs(Energy-Vinfty));
         IF WF.HorizMin/L0<xMin THEN
            BEGIN
               IF SplitIncRefl THEN
                  BEGIN
                     IF ReflAmp<0.99 THEN
                        BEGIN
                           SetColor(LightRed);
                           Prob:=1.0/sqr(MagMax);
                           PlotLine(WF.HorizMin,Prob,L0*xMin,Prob);
                           SetColor(LightGreen);
                           Prob:=sqr(ReflAmp/MagMax);
                           PlotLine(WF.HorizMin,Prob,L0*xMin,Prob)
                        END
                     ELSE
                        BEGIN
                           Prob:=1.0/sqr(MagMax);
                           Skip:=(L0*xMin-WF.HorizMin)/4;
                           SetColor(LightRed);
                           PlotLine(WF.HorizMin,Prob,
                                    WF.HorizMin+Skip,Prob);
                           PlotLine(WF.HorizMin+2*Skip,Prob,
                                    WF.HorizMin+3*Skip,Prob);
                           SetColor(LightGreen);
                           PlotLine(WF.HorizMin+Skip,Prob,
                                    WF.HorizMin+2*Skip,Prob);
                           PlotLine(WF.HorizMin+3*Skip,Prob,
                                    L0*xMin,Prob)
                        END
                  END
               ELSE PlotFunc(LightMagenta,WF.HorizMin/L0,dx,L0,
                             WF.nLSteps,k,ProbIncRefl)
            END;
         PlotFunc(LightMagenta,xMin,dx,L0,nSteps,k,ProbCntr);
         IF WF.HorizMax/L0>xMax THEN
            BEGIN
               IF Vinfty>Energy THEN
                  PlotFunc(LightMagenta,xMax,dx,L0,
                          WF.nRSteps,TransMom,ProbTrans)
               ELSE
                  BEGIN
                     SetColor(LightMagenta);
                     Prob:=sqr(TransAmp/MagMax);
                     PlotLine(L0*xMax,Prob,WF.HorizMax,Prob)
                  END
            END
      END
END; {Probability}

PROCEDURE PlotWaveFunction(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   n,vp :integer;
BEGIN
   WITH WF DO
      BEGIN
         IF Zoom THEN vp:=3
         ELSE
            BEGIN
               vp:=2; ClearOversizedViewPort(3);
               Info(Potl.InfoScr,Potl.NumLines,4);
               Info(PlotInfoScr,25,5);
            END;
         PrepViewPort(vp);
         SpecialAxis(3,HorizMin,HorizMax,5,VertMin,VertMax,10);
         CASE PlotType OF
            1,2 :PutLabel(Top,'Wave function - magnitude & phase');
            3   :PutLabel(Top,'Wave function - real & imaginary parts');
            4   :PutLabel(Top,'Probability density')
            END; {case}
         PutLabel(Left,'arbitrary scale');
         PutLabel(Bottom,ConCat('x (',Potl.Lunit,'m)'));
         SetColor(LightBlue);
         PlotLine(Potl.L0*Potl.xMin,VertMin,Potl.L0*Potl.xMin,VertMax);
         PlotLine(Potl.L0*Potl.xMax,VertMin,Potl.L0*Potl.xMax,VertMax);
         SelectScale(3);
         CASE PlotType OF
            1,2 :MagPhase(WF,Potl);
            3   :RealImag(WF,Potl);
            4   :Probability(WF,Potl)
            END; {case}
         SetColor(Yellow);
         PlotLine(HorizMin,1.05*Energy,HorizMax,1.05*Energy);
         DrawPotential(Potl,HorizMin,HorizMax,VertMin,VertMax,1.05);
         if not Zoom then WFLegend(WF);
         SetColor(Whitte);
      END;
END; {PlotWaveFunction}

PROCEDURE ComputeWaveFunction(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   n,iMin,iMax :integer;
   Failed      :boolean;
   ComputeScr  :TInputScreen;
   MagMin,
   DefHorizMin,DefHorizMax,
   Tsmall,Tlarge            :real;
BEGIN
   {New(ComputeScr); {Mac only}
   WITH WF,ComputeScr DO
      BEGIN
         Tsmall:=Potl.Esmall; Tlarge:=Potl.Elarge;
         Init;
         DefineInputPort(0.63,0.93,0.00,0.9);
         FOR n:=1 TO Potl.NumParams+6 DO LoadLine(Potl.ParamList[n]);
         LoadLine(       'Wave fn parameters');
         LoadLine(ConCat(' Energy: {      }',Potl.Eunit,'eV'));
         LoadLine(ConCat('  in (',Num2Str(Tsmall,6),', ',
                          Num2Str(Tlarge,6),')'));
         LoadLine('');
         LoadLine(ConCat(' Horizontal scale (',Potl.Lunit,'m)'));
         LoadLine(       '  {      } to {      }');
         LoadLine('');
         LoadLine(       ' Vertical scale (arb.)');
         LoadLine(       '  {      } to {      }');
         LoadLine('');
         LoadLine(' [  OK  ]    [Cancel]');
         IF (Potl.V0*Energy<=Potl.Esmall)
          OR (Potl.V0*Energy>=Potl.Elarge) THEN
            SetNumber(1,(Potl.Elarge+Potl.Esmall)/2)
         ELSE
            SetNumber(1,Potl.V0*Energy);
         SetNumber(2,HorizMin);
         SetNumber(3,HorizMax);
         SetNumber(4,VertMin);
         SetNumber(5,VertMax);
         AcceptScreen;
         IF NOT Canceled THEN
            BEGIN
               Energy:=GetNumber(1);
               IF Energy<Potl.Esmall THEN
                  RealInputWarn('energy',Energy,Potl.Esmall,6)
               ELSE IF Energy>Potl.Elarge THEN
                  RealInputWarn('energy',Energy,Potl.Elarge,6);
               Energy:=Energy/Potl.V0;
               HorizMin:=GetNumber(2);
               DefHorizMin:=Potl.L0*(Potl.xMin-1.0e3*Potl.dx);
               DefHorizMax:=Potl.L0*(Potl.xMax+1.0e3*Potl.dx);
               IF HorizMin<DefHorizMin THEN
                  RealInputWarn('horiz_min',HorizMin,DefHorizMin,6)
               ELSE IF HorizMin>=DefHorizMax THEN
                  RealInputWarn('horiz_min',HorizMin,
                                DefHorizMax-Potl.L0*(Potl.xMax-Potl.xMin),6);
               HorizMax:=GetNumber(3);
               IF HorizMax<=HorizMin THEN
                  RealInputWarn('horiz_max',HorizMax,
                                HorizMin+Potl.L0*(Potl.xMax-Potl.xMin),6)
               ELSE IF HorizMax>DefHorizMax THEN
                  RealInputWarn('horiz_max',HorizMax,DefHorizMax,6);
               VertMin:=GetNumber(4);
               VertMax:=GetNumber(5);
               if VertMax<=VertMin then
                  RealInputWarn('vert_max',VertMax,VertMin+2.0,6);
               IF HorizMax/Potl.L0>Potl.xMax THEN
                  BEGIN
                     nRSteps:=round((HorizMax/Potl.L0-Potl.xMax)/Potl.dx);
                     HorizMax:=Potl.L0*(Potl.xMax+Potl.dx*nRSteps);
                  END
               ELSE nRSteps:=0;
               IF HorizMin/Potl.L0<Potl.xMin THEN
                  BEGIN
                     nLSteps:=round((Potl.xMin-HorizMin/Potl.L0)/Potl.dx);
                     HorizMin:=Potl.L0*(Potl.xMin-Potl.dx*nLSteps);
                  END 
               ELSE nLSteps:=0;
               SolveSchrodinger(Mag,Phase,Energy,Potl,Failed,
                                ReflAmp,ReflPhase,TransAmp,TransPhase,LogT);
               IF NOT Failed THEN
                  BEGIN
                     Mag.MinMax(iMin,MagMin,iMax,MagMax);
                     IF MagMax<1+ReflAmp THEN MagMax:=1+ReflAmp;
                     PrepWFPlotScr(WF,Potl);
                     Computed:=True; 
                     PlotWaveFunction(WF,Potl);
                  END
               ELSE Computed:=False;
               Potl.Saved:=False;
            END;
         Done;
      END;
   {Dispose(ComputeScr); {Mac only}
END; {ComputeWaveFunction}

PROCEDURE ChangePlotType(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   num      :integer;
   InputScr :TInputScreen;
BEGIN
   {New(InputScr); {Mac only}
   WITH InputScr DO
      BEGIN
         Init;
         DefineInputPort(0.2,0.75,0.35,0.8);
         LoadLine('');
         LoadLine('            Plot type');
         LoadLine('');
         LoadLine('    #1 Magnitude & phase as color');
         LoadLine('    #1 Magnitude & phase in line plot');
         LoadLine('    #1 Real & imaginary parts');
         LoadLine('    #1 Probability density');
         LoadLine('');
         LoadLine('    [  OK  ]    [Cancel]');
         SetRadioButton('1',WF.PlotType);
         AcceptScreen;
         IF NOT Canceled THEN WITH WF DO
            BEGIN
               PlotType:=GetRadioButton('1');
               CASE PlotType OF
                  1,3 :BEGIN
                          VertMin:=-1.5; VertMax:=1.5;
                       END;
                  2,4 :BEGIN
                          VertMin:=0.0; VertMax:=1.5;
                       END;
                  END; {case}
               PlotWaveFunction(WF,Potl);
            END;
         Done;
      END;
   {Dispose(InputScr); {Mac only}
END; {ChangePlotType}

PROCEDURE ComputeWF(VAR WF :WFtype; VAR Potl :PotlType;
                    VAR MainMenu :TMenu; VAR MenuClicked :boolean);
{Compute and plot wave function.}
VAR
   WFHelpScr :HelpScrType;
   WFkeys    :THotKeys;
   Wkey      :byte;
   WishToReturn,SaveZoom :boolean;
BEGIN
   WFHelpScr[01]:='';
   WFHelpScr[02]:=' <F1>-show this help screen.';
   WFHelpScr[03]:='';
   WFHelpScr[04]:=' <F2>-compute wave function for new energy';
   WFHelpScr[05]:='    or new plot range.';
   WFHelpScr[06]:='';
   WFHelpScr[07]:=' <F3>-toggle size of plot.  When small, text';
   WFHelpScr[08]:='    information and a legend are shown.';
   WFHelpScr[09]:='';
   WFHelpScr[10]:=' <F4>-change plot type.';
   WFHelpScr[11]:='    The types currently installed are:';
   WFHelpScr[12]:='      > magnitude & phase as color';
   WFHelpScr[13]:='      > magnitude & phase in line plot';
   WFHelpScr[14]:='      > real & imaginary parts';
   WFHelpScr[15]:='      > probability density';
   WFHelpScr[16]:='';
   WFHelpScr[17]:=' <F5>-toggle on/off splitting of incident';
   WFHelpScr[18]:='    & reflected parts of wave function.';
   WFHelpScr[19]:='    When the plot type is "magnitude & phase as';
   WFHelpScr[20]:='    color", the reflected wave is plotted relative';
   WFHelpScr[21]:='    to -1 so that the incident wave is visible.';
   WFHelpScr[22]:='';
   WFHelpScr[23]:=' <F10>-activate menu.';
   WFHelpScr[24]:='';
   WFHelpScr[25]:='';

   WITH WF DO
      BEGIN
         IF Computed THEN PlotWaveFunction(WF,Potl)
         ELSE
            BEGIN
               Zoom:=False;
               HorizMin:=Potl.HorizMin; HorizMax:=Potl.HorizMax;
               CASE PlotType OF
                  1,3 :BEGIN
                          VertMin:=-1.5; VertMax:=1.5;
                       END;
                  2,4 :BEGIN
                          VertMin:=0.0; VertMax:=1.5;
                       END;
                  END; {case}
               SaveZoom:=Potl.Zoom; Potl.Zoom:=False;
               PlotPotential(Potl); Potl.Zoom:=SaveZoom;
            END;
         ComputeWaveFunction(WF,Potl);
         IF Computed THEN
            BEGIN
               {New(WFkeys); {Mac only}
               WITH WFkeys DO
                  BEGIN
                     Init(6);
                     key[1]:='F1-Help';
                     key[2]:='F2-Input';
                     IF Zoom THEN key[3]:='F3-Contract'
                     ELSE key[3]:='F3-Expand';
                     key[4]:='F4-PlotType';
                     IF SplitIncRefl THEN key[5]:='F5-Unite'
                     ELSE key[5]:='F5-Split';
                     key[6]:='F10-Menu';
                     Display
                  END;
               WishToReturn:=False;
               WHILE (NOT WishToReturn) AND (NOT MenuClicked)
                AND Computed DO
                  BEGIN
                     CheckForEvents;
                     IF Event.ReadKey=Escape THEN WishToReturn:=True
                     ELSE IF WFkeys.Pressed(Wkey) THEN
                        BEGIN
                           WFkeys.Clear;
                           case Wkey of
                              1 :Help(WFHelpScr);
                              2 :ComputeWaveFunction(WF,Potl);
                              3 :BEGIN
                                    Zoom:=not Zoom;
                                    PlotWaveFunction(WF,Potl);
                                    IF Zoom THEN
                                       WFkeys.key[3]:='F3-Contract'
                                    ELSE WFkeys.key[3]:='F3-Expand'
                                 END;
                              4 :ChangePlotType(WF,Potl);
                              5 :BEGIN
                                    SplitIncRefl:=not SplitIncRefl;
                                    PlotWaveFunction(WF,Potl);
                                    IF SplitIncRefl THEN
                                       WFkeys.key[5]:='F5-Unite'
                                    ELSE WFkeys.key[5]:='F5-Split';
                                 END;
                              6 :BEGIN
                                    WishToReturn:=True;
                                    IF MainMenu.Chosen THEN
                                       MenuClicked:=True;
                                 END;
                              END; {case}
                           WFkeys.Display
                        END
                     ELSE IF MainMenu.Activated THEN MenuClicked:=True;
                  END;
               WFkeys.Clear;
               {Dispose(WFkeys); {Mac only}
            END {if Computed}
      END {with WF}
END; {ComputeWF}

END. {Sc1DWF}
