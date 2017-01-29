{Unit of routines for the radial and 3D wave functions.              }
{Used by Scattr3D.pas.                                               }
{                                                                    }
{Author: John R. Hiller                                              }
{Version: 1.0                11/14/94                                }
{Copyright (c) 1995 John Wiley & Sons, Inc.                          }
{                                                                    }
UNIT Sc3DWF;

INTERFACE

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph,
     Sc3DMach,Sc3DCmn,Sc3DPotl;

CONST
   NumAlgs=2;

TYPE
   WFtype=record
      u,                        {modified radial wave function}
      uFree         :DVector;   {free wave function}
      uMax          :real;
      AngMom        :integer;
      HorizMin,HorizMax,        {horizontal range for plot}
      VertMin,VertMax,          {vertical range for plot}
      Energy,
      PhaseShift    :real;
      Plotted,
      Zoom,
      Computed      :boolean;
      PlotInfoScr   :InfoScrType
      END; {WFtype}
   WF3Dtype=record
      PsiRe,PsiIm,
      Prob          :DMatrix;
      AngMomMin,
      AngMomMax,
      NzSteps,
      NrhoSteps,
      PlotType      :integer;
      rhoMax,zMax,
      drho,dz,
      Size,Distance,
      alpha,beta,gamma,
      Energy        :real;
      Plotted,
      Zoom,
      Surf,
      Computed      :boolean;
      PlotInfoScr   :InfoScrType
      END; {WF3Dtype}

VAR
   WF            :WFtype;
   WF3D          :WF3Dtype;
   cPi           :real;  {store value of pi given by Turbo function Pi}

{--------Routines for numerical calculations---------------------------}

FUNCTION SphBessReg(l :integer; x :real) :real;
FUNCTION SphBessIrreg(l :integer; x :real) :real;
PROCEDURE Legendre(VAR P :DVector; lMax :integer; x :real);
PROCEDURE ComputeFreeWF(VAR uFree :DVector; E,zeta,rMin,rMax :real;
                        AngMom,nSteps,nRSteps :integer);
PROCEDURE SolveSchrodinger(E :real; AngMom :integer;
                           VAR Potl :PotlType; VAR uMax,PSval :real);

{--------Routines for radial wave functions----------------------------}

PROCEDURE SetWFDefaults(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE PrepWFPlotScr(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE PrepWFPlot(VAR WF :WFtype; Potl :PotlType);
PROCEDURE PlotWaveFunction(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE ComputeWF(VAR WF :WFtype; VAR Potl :PotlType;
                    VAR MainMenu :TMenu; VAR MenuClicked :boolean);

{--------Routines for 3D wave functions---------------------------------}

PROCEDURE SetWF3DDefaults(VAR WF3D :WF3Dtype; VAR Potl :PotlType);
PROCEDURE PrepWF3DPlotScr(VAR WF3D :WF3Dtype; VAR Potl :PotlType);
PROCEDURE PlotWF3D(VAR WF3D :WF3Dtype; Potl :PotlType; Post :boolean);
PROCEDURE ComputeWF3D(VAR WF3D :WF3Dtype; VAR Potl :PotlType;
                      VAR MainMenu :TMenu; VAR MenuClicked :boolean);

IMPLEMENTATION

{--------Routines for numerical calculations---------------------------}

FUNCTION SphBessReg(l :integer; x :real) :real;
{Regular spherical Bessel function of order l}
CONST
   jStart=1.0e-20; jTooLarge=1.0e20;
VAR
   nuStart,nu :integer;
   j0,jl,jlp1,jlp2,jlSave :real;
BEGIN
   IF x=0.0 THEN
      BEGIN
         IF l=0 THEN SphBessReg:=1.0 ELSE SphBessReg:=0.0;
      END
   ELSE IF l=0 THEN SphBessReg:=sin(x)/x
   ELSE IF l=1 THEN SphBessReg:=sin(x)/sqr(x)-cos(x)/x
   ELSE
      BEGIN
         j0:=sin(x)/x;
         nuStart:=l+10;
         IF nuStart<x+10 THEN nuStart:=round(x+10);
         jlp2:=0.0; jlp1:=jStart;
         FOR nu:=nuStart-1 DOWNTO 1 DO
            BEGIN
               jl:=(2*nu+1)*jlp1/x-jlp2;
               IF Abs(jl)>jTooLarge THEN
                  BEGIN
                     IF nu-1<=l THEN jlSave:=0.0;
                     jlp2:=0.0; jlp1:=jStart;
                  END
               ELSE
                  BEGIN
                     IF nu-1=l THEN jlSave:=jl;
                     jlp2:=jlp1; jlp1:=jl;
                  END;
            END;
         SphBessReg:=j0*jlSave/jl;
      END;
END; {SphBessReg}

FUNCTION SphBessIrreg(l :integer; x :real) :real;
{Irregular spherical Bessel function of order l}
CONST
   nTooLarge=1.0e20;
VAR
   nu :integer;
   n0,n1,nl :real;
BEGIN
   n0:=-cos(x)/x; n1:=n0/x-sin(x)/x;
   IF l=0 THEN SphBessIrreg:=n0
   ELSE IF l=1 THEN SphBessIrreg:=n1
   ELSE
      BEGIN
         nu:=1;
         WHILE (nu<l) and (Abs(nl)<nTooLarge) do
            BEGIN
               nu:=nu+1;
               nl:=(2*nu-1)*n1/x-n0; n0:=n1; n1:=nl
            END;
         IF Abs(nl)<nTooLarge THEN SphBessIrreg:=nl
         ELSE SphBessIrreg:=nTooLarge*(nl/Abs(nl));
      END;
END; {SphBessIrreg}

PROCEDURE Legendre(VAR P :DVector; lMax :integer; x :real);
{Legendre polynomials P_l(x) for l=0 to lMax.}
VAR
   l :integer;
   Plp1,Pl,Plm1 :real;
BEGIN
   IF lMax<0 THEN lMax:=0;
   Plm1:=1.0; P.Put(1,Plm1);
   IF lMax>0 THEN BEGIN Pl:=x; P.Put(2,Pl) END;
   FOR l:=1 TO lMax-1 DO
      BEGIN
         Plp1:=2*x*Pl-Plm1-(x*Pl-Plm1)/(l+1);
         P.Put(l+2,Plp1); Plm1:=Pl; Pl:=Plp1
      END
END; {Legendre}

PROCEDURE ComputeFreeWF(VAR uFree :DVector; E,zeta,rMin,rMax :real;
                        AngMom,nSteps,nRSteps :integer);
VAR
   n   :integer;
   k,r :real;
BEGIN
   k:=sqrt(zeta*E);
   FOR n:=1 TO Potl.nSteps+Potl.nRSteps+1 DO
      BEGIN
         r:=(n-1)*Potl.dr+Potl.rMin;
         uFree.Put(n,r*SphBessReg(AngMom,k*r))
      END
END; {ComputeFreeWF}

PROCEDURE SolveSchrodinger1(NumSteps :integer; Varr,Xarr :DVector;
                            E :real; AngMom :integer;
                            VAR Potl :PotlType; VAR uMax,PSval :real);
{Solve Schrodinger equation by the transfer matrix algorithm.}
BEGIN
END; {SolveSchrodinger1}

PROCEDURE SolveSchrodinger2(E :real; AngMom :integer;
                            VAR Potl :PotlType; VAR uMax,PSval :real);
{Solve Schrodinger equation by direct integration.}
VAR
   iStart,iStep,
   iMin,iMax,ir1,ir2,
   uNode,uFreeNode,NetNode :integer;
   k,gamma,r,
   r1T,r2T,uRat,uMin,
   uV1,uV2,uF1,uF2,r0,rF0  :real;
   FoundLastZero           :boolean;
   junk :text;
BEGIN
   WITH Potl DO
      BEGIN
         IF rMin>0.0 THEN BEGIN u.Put(1,0.0); u.Put(2,uInit) END
         ELSE
            BEGIN
               IF AngMom=1 THEN u.Put(1,u1) ELSE u.Put(1,0.0);
               u.Put(2,uInit*Pwr(dr,AngMom+1))
            END;
         A.Put(1,0.0);
         FOR iStep:=2 TO nSteps+nRSteps+1 DO
            BEGIN
               r:=(iStep-1)*dr+rMin;
               A.Put(iStep,sqr(dr)*(zeta*(V.Value(iStep)-E)
                           +AngMom*(AngMom+1)/sqr(r))/12)
            END;
         Numerov(1,nSteps+nRSteps+1,dr,A,u);
         u.Put(1,0.0);
         ir1:=trunc((r1*rMax-rMin)/dr); ir2:=1+trunc((r2*rMax-rMin)/dr);
         r1T:=ir1*dr+rMin; r2T:=ir2*dr+rMin;
         k:=sqrt(zeta*E);
         u.MinMax(iMin,uMin,iMax,uMax);
         IF uMax<abs(uMin) THEN uMax:=abs(uMin);
         IF uMax=0.0 THEN uMax:=1.0;
         u.MultScalar(1/uMax);
         ComputeFreeWF(uFree,E,zeta,rMin,rMax,AngMom,nSteps,nRSteps);
         uNode:=0; uFreeNode:=0;
         r:=rMin+dr;
         uF2:=uFree.Value(2);
         WHILE r-dr>0.0 DO
            BEGIN
               r:=r-dr;
               uF1:=SphBessReg(AngMom,k*r);
               IF (uF1=0.0) OR (uF1*uF2<0.0) THEN uFreeNode:=uFreeNode+1;
               uF2:=uF1;
            END;
         iStart:=1;
         uV1:=u.Value(iStart+1); uF1:=uFree.Value(iStart+1);
         WHILE (abs(uV1)<uInit) AND (iStart<ir1+1) DO
            BEGIN
               iStart:=iStart+1;
               uV1:=u.Value(iStart+1);
               uF2:=uFree.Value(iStart+1);
               IF (uF1=0.0) OR (uF1*uF2<0.0) THEN uFreeNode:=uFreeNode+1;
               uF1:=uF2;
            END;
         FOR iStep:=iStart+1 TO ir1 DO
            BEGIN
               uV2:=u.Value(iStep+1); uF2:=uFree.Value(iStep+1);
               IF (uV1=0.0) OR (uV1*uV2<0.0) THEN uNode:=uNode+1;
               IF (uF1=0.0) OR (uF1*uF2<0.0) THEN uFreeNode:=uFreeNode+1;
               uV1:=uV2; uF1:=uF2;
            END;
         IF uF1<>0.0 THEN  {look for nodes in u before next node in uFree}
            BEGIN
               iStep:=ir1;
               FoundLastZero:=False;
               REPEAT
                  iStep:=iStep+1;
                  uV2:=u.Value(iStep+1); uF2:=uFree.Value(iStep+1);
                  IF uF1*uF2<0.0 THEN
                     BEGIN
                        FoundLastZero:=True;
                        IF uV1=0.0 THEN uNode:=uNode+1
                        ELSE IF uV1*uV2<0.0 THEN
                           BEGIN
                              r0:=(iStep+uV1/(uV1-uV2))*dr;
                              rF0:=(iStep+uF1/(uF1-uF2))*dr;
                              IF r0<rF0 THEN uNode:=uNode+1;
                           END;
                     END
                  ELSE IF (uV1=0.0) OR (uV1*uV2<0.0) THEN uNode:=uNode+1;
                  IF uF2=0.0 THEN FoundLastZero:=True
                  ELSE
                     BEGIN
                        uV1:=uV2; uF1:=uF2
                     END;
               UNTIL FoundLastZero OR (iStep=nSteps+nRSteps);
            END;
         NetNode:=uNode-uFreeNode;
         IF u.Value(ir1+1)=0.0 THEN
            PSval:=ArcTan(SphBessReg(AngMom,k*r1T)
                          /SphBessIrreg(AngMom,k*r1T))
         ELSE
            BEGIN
               gamma:=r1T*u.Value(ir2+1)/(r2T*u.Value(ir1+1));
               PSval:=ArcTan((gamma*SphBessReg(AngMom,k*r1T)
                               -SphBessReg(AngMom,k*r2T))
                             /(gamma*SphBessIrreg(AngMom,k*r1T)
                               -SphBessIrreg(AngMom,k*r2T)));
            END;
         IF PSval>0.0 THEN PSval:=PSval-cPi;
         PSval:=NetNode*cPi+PSval;
         IF u.Value(ir2+1)=0.0 THEN     
            uRat:=r2T*(cos(PSval)*SphBessReg(AngMom,k*r2T)
                       -sin(PSval)*SphBessIrreg(AngMom,k*r2T))
         ELSE
            uRat:=r2T*(cos(PSval)*SphBessReg(AngMom,k*r2T)
                     -sin(PSval)*SphBessIrreg(AngMom,k*r2T))/u.Value(ir2+1);
         u.MultScalar(uRat);
         u.MinMax(iMin,uMin,iMax,uMax);
         IF uMax<abs(uMin) THEN uMax:=abs(uMin);
         IF uMax=0.0 THEN uMax:=1.0;
         uFree.MultScalar(1/uMax);
      END;
END; {SolveSchrodinger2}

PROCEDURE SolveSchrodinger(E :real; AngMom :integer;
                           VAR Potl :PotlType; VAR uMax,PSval :real);
{Solve Schrodinger equation by selected algorithm.}
VAR
   NumSteps,n,nMid :integer;
   Vn,Vnm1,rMid    :real;
   Varr,Xarr       :DVector;
BEGIN
   CASE Potl.Alg OF
      0 :with Potl DO
            BEGIN
               NumSteps:=1+NumParams DIV 2;
               IF NumSteps=1 THEN rMid:=(rMax+rMin)/2
               ELSE rMid:=(Params[1]+rMin)/2;
               nMid:=round((rMid-rMin)/dr);
               {New(Varr); New(Xarr); {Mac only}
               Varr.Init(NumSteps+1); Xarr.Init(NumSteps+1);
               Varr.Put(1,V.Value(nMid)); Xarr.Put(1,rMin);
               FOR n:=1 TO NumSteps-1 DO
                  BEGIN
                     Varr.Put(n+1,Invert*Params[2*n]);
                     Xarr.Put(n+1,Params[2*n-1])
                  END;
               Varr.Put(NumSteps+1,0.0); Xarr.Put(NumSteps+1,rMax);
               SolveSchrodinger1(NumSteps,Varr,Xarr,
                                 E,AngMom,Potl,uMax,PSval);
               Xarr.Free; Varr.Free;
               {Dispose(Xarr); Dispose(Varr); {Mac only}
            END;
      1 :with Potl DO
            BEGIN
               {New(Varr); New(Xarr); {Mac only}
               Varr.Init(nSteps+1); Xarr.Init(nSteps+1);
               Vn:=V.Value(1);
               FOR n:=1 TO nSteps DO
                  BEGIN
                     Vnm1:=Vn; Vn:=V.Value(n+1);
                     Varr.Put(n,0.5*(Vnm1+Vn));
                     Xarr.Put(n,(n-1)*dr+rMin);
                  END;
               Varr.Put(nSteps+1,0.0); Xarr.Put(nSteps+1,rMax);
               SolveSchrodinger1(nSteps,Varr,Xarr,
                                 E,AngMom,Potl,uMax,PSval);
               Xarr.Free; Varr.Free;
               {Dispose(Xarr); Dispose(Varr); {Mac only}
            END;
      2 :SolveSchrodinger2(E,AngMom,Potl,uMax,PSval);
      END; {case}
END; {SolveSchrodinger}

{--------Routines for radial wave functions----------------------------}

PROCEDURE SetWFDefaults(VAR WF :WFtype; VAR Potl :PotlType);
BEGIN
   WITH WF DO
      BEGIN
         Energy:=0.5;
         AngMom:=0;
         Computed:=False;
         Plotted:=False;
         Zoom:=False
      END
END; {SetWFDefaults}

PROCEDURE PrepWFPlotScr(VAR WF :WFtype; VAR Potl :PotlType);
{Prepare Info screen for plot of wave function, with result for phase shift.}
VAR
   n,i :integer;
BEGIN
   WITH WF,Potl DO
      BEGIN
         FOR n:=1 TO NumParams+5 DO WF.PlotInfoScr[n]:=ParamList[n];
         WF.PlotInfoScr[NumParams+6]:=' Wave fn parameters';
         WF.PlotInfoScr[NumParams+7]:=
                               ConCat('  Ang. momentum: ',NumStr(AngMom,2,0));
         WF.PlotInfoScr[NumParams+8]:=
                               ConCat('  Energy: ',Num2Str(V0*Energy,6),
                                      ' ',Eunit,'eV');
         WF.PlotInfoScr[NumParams+9]:=
                               ConCat('  Mass: ',Num2Str(Mass,6),
                                       ' ',Munit,'eV/c^2');
         WF.PlotInfoScr[NumParams+10]:=
                               ConCat('  Phase shift: ',
                                      Num2Str(PhaseShift,6));
         WF.PlotInfoScr[NumParams+11]:='';
         WF.PlotInfoScr[NumParams+12]:=ConCat(' Algorithm: ');
         CASE Alg OF
            0 :WF.PlotInfoScr[NumParams+13]:='  piecewise constant';
            1 :WF.PlotInfoScr[NumParams+13]:='  transfer matrix';
            2 :WF.PlotInfoScr[NumParams+13]:='  direct integration';
            END; {case}
         FOR n:=NumParams+14 TO 25 DO WF.PlotInfoScr[n]:=''
      END
END; {PrepWFPlotScr}

PROCEDURE WFLegend;
{Display legend for plot of wave function.}
BEGIN
   OpenViewPort(1);
   ClearViewPort;
   SelectScale(1);
   LegendEntry(3,LightMagenta,'new wave fn');
   LegendEntry(4,LightRed,'old wave fn');
   LegendEntry(5,LightBlue,'free wave fn');
   LegendEntry(6,LightCyan,'potential');
   LegendEntry(7,Yellow,'energy');
   LegendEntry(8,Whitte,'phase shift');
   SetColor(Whitte)
END; {WFPlotLegend}

PROCEDURE PrepWFPlot(VAR WF :WFtype; Potl :PotlType);
{Prepare viewport for wave function plot.}
VAR
   vp :integer;
BEGIN
   WITH WF DO
      BEGIN
         IF Zoom THEN vp:=3
         ELSE
            BEGIN
               vp:=2;
               ClearOversizedViewPort(3);
               Info(Potl.InfoScr,Potl.NumLines,4);
               IF Computed THEN Info(PlotInfoScr,25,5)
               ELSE Info(Potl.PlotInfoScr,25,5);
               WFLegend
            END;
         PrepViewPort(vp);
         SpecialAxis(3,HorizMin,HorizMax,5,VertMin,VertMax,10);
         PutLabel(Top,'Radial wave function');
         PutLabel(Left,'arbitrary scale');
         PutLabel(Bottom,ConCat('r (',Potl.Lunit,'m)'));
         SetColor(Yellow);
         PlotLine(HorizMin,Energy,HorizMax,Energy);
         DrawPotential(Potl,HorizMin,HorizMax,VertMin,VertMax,1.0);
         SetColor(Whitte);
      END;
END; {PrepWFPlot}

PROCEDURE AddFreeWFToPlot(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   n,ir2 :integer;
   k,r :real;
BEGIN
   WITH WF DO
      BEGIN
         IF Zoom THEN SelectViewPort(3) ELSE SelectViewPort(2);
         SelectScale(3);
         k:=sqrt(Potl.zeta*Energy);
         ir2:=trunc((Potl.r2*Potl.rMax-Potl.rMin)/Potl.dr);
         SetColor(LightBlue);
         FOR n:=2 TO Potl.nSteps+Potl.nRSteps+1 DO
            PlotLine(Potl.L0*((n-2)*Potl.dr+Potl.rMin),uFree.Value(n-1),
                     Potl.L0*((n-1)*Potl.dr+Potl.rMin),uFree.Value(n));
         SetColor(Whitte);
         PlotLine(Potl.L0*(ir2*Potl.dr+Potl.rMin),uFree.Value(ir2+1),
                  Potl.L0*(ir2*Potl.dr+Potl.rMin-PhaseShift/k),
                  uFree.Value(ir2+1));
      END;
END; {AddFreeWFToPlot}

PROCEDURE AddWFToPlot(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   n :integer;
BEGIN
   WITH WF DO
      BEGIN
         if Zoom then SelectViewPort(3)
         ELSE
            BEGIN
               SelectViewPort(2);
               Info(PlotInfoScr,25,5)
            END;
         SelectScale(3);
         SetColor(LightMagenta);
         FOR n:=2 TO Potl.nSteps+Potl.nRSteps+1 DO 
            PlotLine(Potl.L0*((n-2)*Potl.dr+Potl.rMin),u.Value(n-1),
                     Potl.L0*((n-1)*Potl.dr+Potl.rMin),u.Value(n));
         SetColor(Whitte);
      END;
END; {AddWFToPlot}

PROCEDURE ReplotOldWF(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   n :integer;
BEGIN
   WITH WF DO
      BEGIN
         IF Zoom THEN SelectViewPort(3) ELSE SelectViewPort(2);
         SelectScale(3);
         SetColor(LightRed);
         FOR n:=2 TO Potl.nSteps+Potl.nRSteps+1 DO 
            PlotLine(Potl.L0*((n-2)*Potl.dr+Potl.rMin),u.Value(n-1),
                     Potl.L0*((n-1)*Potl.dr+Potl.rMin),u.Value(n));
         SetColor(Whitte);
      END;
END; {ReplotOldWF}

PROCEDURE PlotWaveFunction(VAR WF :WFtype; VAR Potl :PotlType);
BEGIN
   PrepWFPlot(WF,Potl);
   AddWFToPlot(WF,Potl);
   WF.Plotted:=True;
END; {PlotWaveFunction}

PROCEDURE ComputeWaveFunction(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   n,iMin,iMax :integer;
   MagMin      :real;
   ComputeScr  :TInputScreen;
BEGIN
   {New(ComputeScr); {Mac only}
   WITH Potl,ComputeScr DO
      BEGIN
         Init;
         DefineInputPort(0.63,0.93,0.0,0.92);
         FOR n:=1 TO NumParams+5 DO LoadLine(ParamList[n]);
         LoadLine(' Wave fn parameters');
         LoadLine('  Ang. momentum: {   }');
         LoadLine(ConCat('  Energy: {      }',Eunit,'eV'));
         LoadLine(ConCat('  Mass: ',Num2Str(Mass,6),' ',Munit,'eV/c^2'));
         LoadLine('');
         IF WF.Plotted THEN LoadLine('   Clear plot? #F');
         LoadLine('');
         LoadLine(' [  OK  ]    [Cancel]');
         SetNumber(1,WF.AngMom);
         SetNumber(2,V0*WF.Energy);
         AcceptScreen;
         IF NOT Canceled THEN WITH WF DO
            BEGIN
               IF Plotted AND NOT GetBoolean(3) THEN ReplotOldWF(WF,Potl);
               AngMom:=round(GetNumber(1));
               IF (AngMom<0) OR (AngMom>MaxRange) THEN
                  IntInputWarn('angular momentum',AngMom,0,2);
               Energy:=GetNumber(2);
               IF Energy<=0.0 THEN
                  RealInputWarn('energy',Energy,1.0e-7,6);
               IF 2*cPi/(sqrt(zeta*Energy/V0)*dr)<3 THEN
                  BEGIN
                     Announce(ConCat('Wave number too large for numerical ',
                                     'calculation to proceed.'));
                     RealInputWarn('energy',Energy,
                                   V0*sqr(2*cPi/(3*dr))/zeta,6);
                  END;
               Energy:=Energy/Potl.V0;
               IF Plotted AND GetBoolean(3) THEN PrepWFPlot(WF,Potl);
               SolveSchrodinger(Energy,AngMom,Potl,uMax,PhaseShift);
               uFree.Free; u.Free;
               u.Init(Potl.nSteps+Potl.nRSteps+1);
               uFree.Init(Potl.nSteps+Potl.nRSteps+1);
               u.Equate(1.0/uMax,Potl.u); uFree.Equate(1.0,Potl.uFree);
               PrepWFPlotScr(WF,Potl);
               Computed:=True; Potl.Saved:=False;
               IF Plotted THEN AddWFToPlot(WF,Potl)
               ELSE PlotWaveFunction(WF,Potl);
            END;
         Done;
      END;
   {Dispose(ComputeScr); {Mac only}
END; {ComputeWaveFunction}

PROCEDURE ChangeWFPlotScales(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   Changed,AutoSet :boolean;
BEGIN
   WITH WF DO
      BEGIN
         AutoSet:=False;
         ChangePlotScales(HorizMin,HorizMax,ConCat('(',Potl.Lunit,'m)'),
                          0.0,Potl.rStop,
                          VertMin,VertMax,'   ',-1.0,2.4,
                          Changed,AutoSet);
         IF Changed THEN PlotWaveFunction(WF,Potl);
      END
END; {ChangeWFPlotScales}

PROCEDURE ComputeWF(VAR WF :WFtype; VAR Potl :PotlType;
                    VAR MainMenu :TMenu; VAR MenuClicked :boolean);
{Compute and plot wave function.}
VAR
   WFHelpScr :HelpScrType;
   WFkeys    :THotKeys;
   Wkey      :byte;
   n         :integer;
   WishToReturn,SaveZoom :boolean;
BEGIN
   WFHelpScr[01]:='';
   WFHelpScr[02]:=' <F1>-show this help screen.';
   WFHelpScr[03]:='';
   WFHelpScr[04]:=' <F2>-compute wave function with new choice';
   WFHelpScr[05]:='    of angular momentum or energy.';
   WFHelpScr[06]:='    The input screen will include an option';
   WFHelpScr[07]:='    to erase previous results, if any.';
   WFHelpScr[08]:='';
   WFHelpScr[09]:=' <F3>-toggle size of plot.  When small, text';
   WFHelpScr[10]:='    information and a legend are shown.';
   WFHelpScr[11]:='';
   WFHelpScr[12]:=' <F4>-change horizontal and vertical scales';
   WFHelpScr[13]:='    of plot.';
   WFHelpScr[14]:='';
   WFHelpScr[15]:=' <F5>-draw free wave function and phase shift.';
   WFHelpScr[16]:='';
   WFHelpScr[17]:=' <F10>-activate menu.';
   for n:=18 to 25 do WFHelpScr[n]:='';

   WITH WF DO
      BEGIN
         IF Computed THEN PlotWaveFunction(WF,Potl)
         ELSE
            BEGIN
               Zoom:=False; Plotted:=False;
               HorizMin:=Potl.HorizMin; HorizMax:=Potl.HorizMax;
               VertMin:=-1.5; VertMax:=1.5;
               SaveZoom:=Potl.Zoom; Potl.Zoom:=False;
               PlotPotential(Potl);
               Potl.Zoom:=SaveZoom;
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
                     key[4]:='F4-Scale';
                     key[5]:='F5-Free wf';
                     key[6]:='F10-Menu';
                     Display
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
                              2 :ComputeWaveFunction(WF,Potl);
                              3 :BEGIN
                                    Zoom:=not Zoom;
                                    PlotWaveFunction(WF,Potl);
                                    IF Zoom THEN
                                       WFkeys.key[3]:='F3-Contract'
                                    ELSE WFkeys.key[3]:='F3-Expand';
                                 END;
                              4 :ChangeWFPlotScales(WF,Potl);
                              5 :AddFreeWFToPlot(WF,Potl);
                              6 :BEGIN
                                    WishToReturn:=True;
                                    IF MainMenu.Chosen THEN
                                       MenuClicked:=True;
                                 END;
                              END; {case}
                           WFkeys.Display;
                        END
                     ELSE IF MainMenu.Activated THEN MenuClicked:=True
                  END;
               WFkeys.Clear;
               {Dispose(WFkeys); {Mac only}
            END;
      END;
END; {ComputeWF}

{--------Routines for 3D wave functions---------------------------------}

PROCEDURE SetSurfaceView(VAR alpha,beta,gamma,Size,Distance :real);
BEGIN
   alpha:=150; beta:=35; gamma:=0;
   Size:=1.9; Distance:=3
END; {SetSurfaceView}

PROCEDURE SetWF3DDefaults(VAR WF3D :WF3Dtype; VAR Potl :PotlType);
BEGIN
   WITH WF3D DO
      BEGIN
         Energy:=0.5;
         AngMomMin:=0; AngMomMax:=20;
         NzSteps:=20; NrhoSteps:=20;
         PlotType:=1;         {probability density}
         SetSurfaceView(alpha,beta,gamma,Size,Distance);
         Computed:=False;
         Plotted:=False;
         Zoom:=False;
         Surf:=True;
      END
END; {SetWF3DDefaults}

PROCEDURE PrepWF3DPlotScr(VAR WF3D :WF3Dtype; VAR Potl :PotlType);
{Prepare Info screen for plot of 3D wave function.}
VAR
   n,i :integer;
BEGIN
   WITH WF3D,Potl DO
      BEGIN
         FOR n:=1 TO NumParams+5 DO WF3D.PlotInfoScr[n]:=ParamList[n];
         WF3D.PlotInfoScr[NumParams+6]:=' 3D wave fn parameters';
         WF3D.PlotInfoScr[NumParams+7]:='  Angular momentum';
         WF3D.PlotInfoScr[NumParams+8]:=
                                 ConCat('   ',NumStr(AngMomMin,2,0),
                                        ' to ',NumStr(AngMomMax,2,0));
         WF3D.PlotInfoScr[NumParams+9]:=
            ConCat('  Energy: ',Num2Str(V0*Energy,6),' ',Eunit,'eV');
         WF3D.PlotInfoScr[NumParams+10]:=
            ConCat('  Mass: ',Num2Str(Mass,6),' ',Munit,'eV/c^2');
         WF3D.PlotInfoScr[NumParams+11]:=
            ConCat('  z_max: ',Num2Str(L0*zMax,6),' ',Lunit,'m');
         WF3D.PlotInfoScr[NumParams+12]:=
            ConCat('  rho_max: ',Num2Str(L0*rhoMax,6),' ',Lunit,'m');
         WF3D.PlotInfoScr[NumParams+13]:='';
         WF3D.PlotInfoScr[NumParams+14]:=' Algorithm: ';
         CASE Alg OF
            0 :WF3D.PlotInfoScr[NumParams+15]:='  piecewise constant';
            1 :WF3D.PlotInfoScr[NumParams+15]:='  transfer matrix';
            2 :WF3D.PlotInfoScr[NumParams+15]:='  direct integration';
            END; {case}
         FOR n:=NumParams+16 TO 25 DO WF3D.PlotInfoScr[n]:=''
      END
END; {PrepWF3DPlotScr}

PROCEDURE PlotSurface(VAR WF3D :WF3Dtype; Post :boolean);
VAR
   s       :PointType;
   Surface :TGraph3D;
BEGIN
   HideMouse;
   WITH WF3D DO
      BEGIN
         {New(Surface); {Mac only}
         Surface.Init;
         Surface.Set3DScales(-zMax,zMax,-rhoMax,0.0,-1.0,1.0);
         Surface.SetDistance(Size,Distance);
         Surface.SetViewAngles(alpha,beta,gamma);
         IF rhoMax>2*zMax THEN
            Surface.SetBoxRatios(2*zMax/rhoMax,1.0,0.6)
         ELSE Surface.SetBoxRatios(1.0,rhoMax/(2*zMax),0.6);
         CASE PlotType OF
            1 :BEGIN
                  Surface.SurfaceColor:=LightMagenta;
                  Surface.DrawSurface(Prob);
               END;
            2 :BEGIN
                  Surface.SurfaceColor:=LightRed;
                  Surface.DrawSurface(PsiRe);
               END;
            3 :BEGIN
                  Surface.SurfaceColor:=LightGreen;
                  Surface.DrawSurface(PsiIm);
               END;
            END; {case}
         SetColor(Whitte);
         Surface.Vector3D(-1.5*zMax,0.0,0.0,-1.1*zMax,0.0,0.0);
         Surface.GetPoint(-1.5*zMax,0.0,0.0,s);
         OutTextXY(s.x-3,s.y+3,'beam');
         Surface.Vector3D(1.1*zMax,0.0,0.0,1.5*zMax,0.0,0.0);
         Surface.GetPoint(1.5*zMax,0.0,0.0,s);
         OutTextXY(s.x+3,s.y,'z');
         Surface.Vector3D(0.0,-1.1*rhoMax,0.0,0.0,-1.5*rhoMax,0.0);
         Surface.GetPoint(0.0,-1.5*rhoMax,0.0,s);
         OutTextXY(s.x+3,s.y,'rho');
         Surface.GetPoint(0.0,0.0,0.0,s);
         SetColor(LightCyan);
         OutTextXY(s.x,s.y,'o');
         Plotted:=True;
         SetColor(Whitte);
         IF Post AND (NOT Zoom) THEN PostEulerAngles(alpha,beta,gamma);
         {Dispose(Surface); {Mac only}
      END;
   ShowMouse;
END; {PlotSurface}

PROCEDURE PlotContour(VAR WF3D :WF3Dtype; Post :boolean);
VAR
   Contour :TContour;
BEGIN
   HideMouse;
   WITH WF3D DO
      BEGIN
         {New(Contour); {Mac only}
         Contour.Init;
         CASE PlotType OF
            1 :Contour.DrawMatrix(Prob);
            2 :Contour.DrawMatrix(PsiRe);
            3 :Contour.DrawMatrix(PsiIm);
            END; {case}
         IF Zoom THEN Contour.PrintLegend(4)
         ELSE IF Post THEN
            BEGIN
               SelectViewPort(1); Contour.PrintLegend(4)
            END;
         Plotted:=True;
         {Dispose(Contour); {Mac only}
      END;
   ShowMouse;
END; {PlotContour}

PROCEDURE PlotWF3D(VAR WF3D :WF3Dtype; Potl :PotlType; Post :boolean);
{Plot 3D wave function.}
VAR
   vp :integer;
BEGIN
   IF NOT WF3D.Plotted THEN WITH WF3D DO
      BEGIN
         IF Zoom THEN vp:=3
         ELSE
            BEGIN
               vp:=2;
               IF Post THEN
                  BEGIN
                     ClearOversizedViewPort(3);
                     Info(Potl.InfoScr,Potl.NumLines,4);
                  END;
               Info(PlotInfoScr,25,5);
            END;
         PrepViewPort(vp);
         SetColor(Whitte);
         CASE PlotType OF
            1 :PutLabel(Top,'Probability density');
            2 :PutLabel(Top,'Wave function - real part');
            3 :PutLabel(Top,'Wave function - imaginary part')
            END; {case}
         IF Surf THEN PlotSurface(WF3D,Post)
         ELSE PlotContour(WF3D,Post);
      END;
END; {PlotWF3D}

PROCEDURE ModViewWF3D(VAR WF3D :WF3Dtype; VAR Potl :PotlType);
{Rotate view of surface and replot.}
VAR
   RVHelpScr :HelpScrType;
   RVkeys    :THotKeys;
   Rkey      :byte;
   View      :TView3D;
   SaveSize,SaveDistance,SaveAlpha,SaveBeta,SaveGamma :real;
   WishToAccept,SaveZoom :boolean;
BEGIN
   RVHelpScr[01]:='';
   RVHelpScr[02]:=' <F1>-show this help screen.   ';
   RVHelpScr[03]:='';
   RVHelpScr[04]:=' <F2>-plot surface. This regenerates the';
   RVHelpScr[05]:='    surface plot to illustrate the effects';
   RVHelpScr[06]:='    of changes made.';
   RVHelpScr[07]:='';
   RVHelpScr[08]:=' <F3>-change size of plot. Each press of the key';
   RVHelpScr[09]:='    changes the size by one fixed step in a cycle.';
   RVHelpScr[10]:='';
   RVHelpScr[11]:=' <F4>-change perspective of plot. Each press';
   RVHelpScr[12]:='    of the key changes the distance of the plot';
   RVHelpScr[13]:='    by one fixed step in a cycle.';
   RVHelpScr[14]:='';
   RVHelpScr[15]:=' <F5>-reset to default view.';
   RVHelpScr[16]:='';
   RVHelpScr[17]:=' Sliders - change Euler angles. Use mouse to drag';
   RVHelpScr[18]:='    a block, or click on arrows.  Use arrow keys';
   RVHelpScr[19]:='    to select and move block.';
   RVHelpScr[20]:='';
   RVHelpScr[21]:=' The changes made are immediately visible in the';
   RVHelpScr[22]:=' appearance of the small box in the lower left.';
   RVHelpScr[23]:='';
   RVHelpScr[24]:=' <Enter>-accept view and return';
   RVHelpScr[25]:='    to previous screen.';

   WITH WF3D DO
      BEGIN
         {New(RVkeys); {Mac only}
         WITH RVkeys DO
            BEGIN
               Init(6);
               key[1]:='F1-Help';
               key[2]:='F2-Plot';
               key[3]:='F3-Size';
               key[4]:='F4-Distance';
               key[5]:='F5-Reset';
               key[6]:='Return-Accept';
               Display
            END;
         SaveSize:=Size; SaveDistance:=Distance;
         SaveAlpha:=alpha; SaveBeta:=beta; SaveGamma:=gamma;
         SaveZoom:=Zoom; Zoom:=False;
         Surf:=True;
         Plotted:=False;
         ClearOversizedViewPort(3);
         PlotWF3D(WF3D,Potl,False);
         {New(View); {Mac only}
         View.Init; 
         View.Draw(alpha,beta,gamma,Size,Distance);
         WishToAccept:=False;
         WHILE NOT WishToAccept DO
            BEGIN
               CheckForEvents;
               IF Event.ReadKey=Escape THEN
                  BEGIN
                     WishToAccept:=True; Plotted:=False;
                     Size:=SaveSize; Distance:=SaveDistance;
                     alpha:=SaveAlpha; beta:=SaveBeta; gamma:=SaveGamma;
                  END
               ELSE IF RVkeys.Pressed(Rkey) THEN
                  CASE Rkey OF
                     1 :Help(RVHelpScr);
                     2 :BEGIN
                           Plotted:=False;
                           PlotWF3D(WF3D,Potl,False)
                        END;
                     3 :View.Resize(Size,Distance);
                     4 :View.Persp(Size,Distance);
                     5 :BEGIN
                           View.Done;
                           SetSurfaceView(alpha,beta,gamma,
                                          Size,Distance);
                           View.Init;
                           View.Draw(alpha,beta,gamma,Size,Distance);
                           Plotted:=False;
                        END;
                     6 :WishToAccept:=True
                     END {case}
               ELSE IF View.Changed THEN
                  BEGIN
                     View.Rotate(alpha,beta,gamma);
                     Plotted:=False
                  END
            END;
         RVkeys.Clear;
         View.Done;
         {Dispose(View); Dispose(RVkeys); {Mac only}
         Zoom:=SaveZoom;
         IF Zoom THEN Plotted:=False;
         IF NOT Plotted THEN PlotWF3D(WF3D,Potl,True)
         ELSE IF NOT Zoom THEN
            BEGIN
               Info(Potl.InfoScr,Potl.NumLines,4);
               PostEulerAngles(alpha,beta,gamma)
            END;
      END;
END; {ModViewWF3D}

PROCEDURE Compute3DWaveFn(VAR WF3D :WF3Dtype; VAR Potl :PotlType);
VAR
   n,AngMom,ir,i,j,
   NzStepsOld,NrhoStepsOld,
   MinRow,MinCol,MaxRow,MaxCol :integer;
   k,r,theta,z,rho,Rkl,PSval,
   ratio,PsiTemp,Min,Max,uMax  :real;
   WishToAbort,Failed          :boolean;
   MemBlock    :longint;
   f           :complex;
   Pl          :DVector;
   WF3DHelpScr :HelpScrType;
   WF3Dkeys    :THotKeys;
   Wkey        :byte;
   ComputeScr  :TInputScreen;
   Propeller   :TWaitMessage;
BEGIN
   WF3DHelpScr[01]:='';
   WF3DHelpScr[02]:='<F1>-show this help screen.';
   WF3DHelpScr[03]:='';
   WF3DHelpScr[04]:='<F2>-stop the calculation.';
   WF3DHelpScr[05]:='';
   for n:=6 to 25 do WF3DHelpScr[n]:='';
   {New(ComputeScr); {Mac only}
   WITH Potl,ComputeScr DO
      BEGIN
         Init;
         DefineInputPort(0.63,0.93,0.0,0.92);
         FOR n:=1 TO NumParams+5 DO LoadLine(ParamList[n]);
         LoadLine(       '3D wave fn parameters');
         LoadLine(       ' Angular momentum');
         LoadLine(       '  min {   }  max {   }');
         LoadLine(ConCat(' Energy: {      }',Eunit,'eV'));
         LoadLine(ConCat(' Mass: ',Num2Str(Mass,6),' ',Munit,'eV/c^2'));
         LoadLine(ConCat(' z_max: {      }',Lunit,'m'));
         LoadLine(       '  no. of steps: {    }');
         LoadLine(ConCat(' rho_max: {      }',Lunit,'m'));
         LoadLine(       '  no. of steps: {    }');
         LoadLine('');
         LoadLine(' [  OK  ]    [Cancel]');
         SetNumber(1,WF3D.AngMomMin);
         SetNumber(2,WF3D.AngMomMax);
         SetNumber(3,V0*WF3D.Energy);
         SetNumber(4,L0*WF3D.zMax);
         SetNumber(5,WF3D.NzSteps);
         SetNumber(6,L0*WF3D.rhoMax);
         SetNumber(7,WF3D.NrhoSteps);
         AcceptScreen;
         IF NOT Canceled THEN WITH WF3D DO
            BEGIN
               AngMomMin:=round(GetNumber(1));
               IF (AngMomMin<0) OR (AngMomMin>MaxRange) THEN
                  IntInputWarn('minimum angular momentum',AngMomMin,0,2);
               AngMomMax:=round(GetNumber(2));
               IF (AngMomMax<AngMomMin) OR (AngMomMax>MaxRange) THEN
                  IntInputWarn('maximum angular momentum',
                               AngMomMax,AngMomMin,2);
               Energy:=GetNumber(3);
               IF Energy<=0.0 THEN
                  RealInputWarn('energy',Energy,1.0e-7,6);
               IF 2*cPi/(sqrt(zeta*Energy/V0)*dr)<3 THEN
                  BEGIN
                     Announce(ConCat('Wave number too large for numerical ',
                                     'calculation to proceed.'));
                     RealInputWarn('energy',Energy,
                                   V0*sqr(2*cPi/(3*dr))/zeta,6);
                  END;
               Energy:=Energy/Potl.V0;
               zMax:=GetNumber(4);
               IF zMax<0.0 THEN
                  RealInputWarn('z_max',zMax,Potl.rMax,6);
               zMax:=zMax/Potl.L0;
               rhoMax:=GetNumber(6);
               IF rhoMax<0.0 THEN
                  RealInputWarn('rho_max',rhoMax,Potl.rMax,6);
               rhoMax:=rhoMax/Potl.L0;
               NzStepsOld:=NzSteps; NzSteps:=round(GetNumber(5));
               IF NzSteps<1 THEN
                  IntInputWarn('no. of z steps',NzSteps,NzStepsOld,3);
               NrhoStepsOld:=NrhoSteps; NrhoSteps:=round(GetNumber(7));
               IF NrhoSteps<1 THEN
                  IntInputWarn('no. of rho steps',NrhoSteps,NrhoStepsOld,3);
               Prob.Free; PsiIm.Free; PsiRe.Free;
               MemBlock:=Longint(2*NzSteps*(NrhoSteps+1))*SizeOf(real);
               IF MemBlock>=65521 THEN Failed:=True
               ELSE IF MemBlock>MaxAvail THEN Failed:=True
               ELSE
                  BEGIN
                     PsiRe.Init(NrhoSteps+1,2*NzSteps);
                     IF MemBlock>MaxAvail THEN
                        BEGIN
                           Failed:=True;
                           PsiRe.Free;
                        END
                     ELSE 
                        BEGIN
                           PsiIm.Init(NrhoSteps+1,2*NzSteps);
                           IF MemBlock>MaxAvail THEN
                              BEGIN
                                 Failed:=True;
                                 PsiIm.Free; PsiRe.Free;
                              END
                           ELSE
                              BEGIN
                                 Failed:=False;
                                 Prob.Init(NrhoSteps+1,2*NzSteps);
                              END;
                        END;
                  END;
               IF Failed THEN
                  BEGIN
                     Announce(ConCat('Memory capacity insufficient for ',
                              'requested array size.'));
                     NzSteps:=NzStepsOld; NrhoSteps:=NrhoStepsOld;
                     PsiRe.Init(NrhoSteps+1,2*NzSteps);
                     PsiIm.Init(NrhoSteps+1,2*NzSteps);
                     Prob.Init(NrhoSteps+1,2*NzSteps);
                  END;
               dz:=zMax/(NzSteps-0.5); drho:=rhoMax/NrhoSteps;
               {New(Pl); {Mac only}
               Pl.Init(AngMomMax+1);
               PsiRe.Fill(0.0); PsiIm.Fill(0.0);
               k:=sqrt(Potl.zeta*Energy);
               {New(WF3Dkeys); {Mac only}
               WITH WF3Dkeys DO
                  BEGIN
                     Init(2);
                     key[1]:='F1-Help';
                     key[2]:='F2-Stop';
                     Display;
                  END;
               WishToAbort:=False;
               AngMom:=AngMomMin-1;
               {New(Propeller); {Mac only}
               Propeller.Show;
               WHILE (AngMom<AngMomMax) AND (NOT WishToAbort) DO
                  BEGIN
                     AngMom:=AngMom+1;
                     SolveSchrodinger(Energy,AngMom,Potl,uMax,PSval);
                     FOR i:=-NzSteps TO NzSteps-1 DO FOR j:=0 TO NrhoSteps DO
                        BEGIN
                           Propeller.Update;
                           z:=(i+0.5)*dz; rho:=j*drho;
                           r:=sqrt(sqr(rho)+sqr(z));
                           theta:=ArcTan2(z,rho);
                           IF (r<=Potl.rMin) OR (r>Potl.rStop) THEN Rkl:=0.0
                           ELSE WITH Potl DO
                              BEGIN
                                 ratio:=(r-rMin)/dr;
                                 ir:=trunc(ratio);
                                 IF ir>=nSteps+nRSteps THEN Rkl:=0.0
                                 ELSE Rkl:=((ratio-ir)*u.Value(ir+2)
                                           +(ir+1-ratio)*u.Value(ir+1))/(k*r)
                              END;
                           Legendre(Pl,AngMom,cos(theta));
                           PsiTemp:=(2*AngMom+1)*cos(PSval+AngMom*cPi/2)
                                    *Rkl*Pl.Value(AngMom+1);
                           PsiRe.Put(j+1,i+NzSteps+1,
                                     PsiRe.Value(j+1,i+NzSteps+1)+PsiTemp);
                           PsiTemp:=(2*AngMom+1)*sin(PSval+AngMom*cPi/2)
                                    *Rkl*Pl.Value(AngMom+1);
                           PsiIm.Put(j+1,i+NzSteps+1,
                                     PsiIm.Value(j+1,i+NzSteps+1)+PsiTemp);
                        END;
                     CheckForEvents;
                     IF Event.ReadKey=Escape THEN WishToAbort:=True
                     ELSE IF WF3Dkeys.Pressed(Wkey) THEN
                        CASE Wkey OF
                           1 :Help(WF3DHelpScr);
                           2 :WishToAbort:=True;
                           END; {case Wkey}
                  END; {while AngMom<AngMomMax}
               Propeller.Hide;
               {Dispose(Propeller); {Mac only}
               MagSqr(PsiRe,PsiIm,Prob);
               Prob.MinMax(MinRow,MinCol,Min,MaxRow,MaxCol,Max);
               if Max=0.0 then Max:=1.0;
               Prob.MultScalar(1/Max);
               PsiRe.MultScalar(1/sqrt(Max)); PsiIm.MultScalar(1/sqrt(Max));
               Pl.Free;
               {Dispose(Pl); {Mac only}
               Computed:=True; Plotted:=False; Potl.Saved:=False;
               IF WishToAbort AND (AngMom<AngMomMax) THEN
                  BEGIN
                     Announce(
                        ConCat('Completed partial-wave sum from l=0 to ',
                               NumStr(AngMom,2,0),' only.'));
                     AngMomMax:=AngMom;
                  END;
               PrepWF3DPlotScr(WF3D,Potl);
               PlotWF3D(WF3D,Potl,True);
            END;
         Done;
      END;
   {Dispose(ComputeScr); {Mac only}
END; {Compute3DWaveFn}

PROCEDURE ChangePlotType(VAR WF3D :WF3Dtype; VAR Potl :PotlType);
VAR
   InputScr :TInputScreen;
BEGIN
   {New(InputScr); {Mac only}
   WITH InputScr DO
      BEGIN
         Init;
         DefineInputPort(0.2,0.8,0.325,0.8);
         LoadLine('');
         LoadLine('                Plot type');
         LoadLine('');
         LoadLine('    #1  Probability density');
         LoadLine('    #1  Real part of psi');
         LoadLine('    #1  Imaginary part of psi');
         LoadLine('');
         LoadLine('    #2  surface plot');
         LoadLine('    #2  contour plot');
         LoadLine('');
         LoadLine('    [  OK  ]    [Cancel]');
         SetRadioButton('1',WF3D.PlotType);
         IF WF3D.Surf THEN SetRadioButton('2',1)
         ELSE SetRadioButton('2',2);
         AcceptScreen;
         IF NOT Canceled THEN WITH WF3D DO
            BEGIN
               PlotType:=GetRadioButton('1');
               IF GetRadioButton('2')=1 THEN Surf:=True ELSE Surf:=False;
               Plotted:=False;
               PlotWF3D(WF3D,Potl,True);
            END;
         Done;
      END;
   {Dispose(InputScr); {Mac only}
END; {ChangePlotType}

PROCEDURE ComputeWF3D(VAR WF3D :WF3Dtype; VAR Potl :PotlType;
                      VAR MainMenu :TMenu; VAR MenuClicked :boolean);
{Compute and plot 3D wave function.}
VAR
   WF3DHelpScr :HelpScrType;
   WF3Dkeys    :THotKeys;
   Wkey        :byte;
   n           :integer;
   WishToReturn,SaveZoom :boolean;
BEGIN
   WF3DHelpScr[01]:='';
   WF3DHelpScr[02]:=' <F1>-show this help screen.';
   WF3DHelpScr[03]:='';
   WF3DHelpScr[04]:=' <F2>-compute 3D wave function with new';
   WF3DHelpScr[05]:='    choice of partial waves or energy,';
   WF3DHelpScr[06]:='    or with new plot range.';
   WF3DHelpScr[07]:='';
   WF3DHelpScr[08]:=' <F3>-toggle size of plot.  When small, text';
   WF3DHelpScr[09]:='    information and a legend are shown.';
   WF3DHelpScr[10]:='';
   WF3DHelpScr[11]:=' <F4>-modify view of surface via rotation';
   WF3DHelpScr[12]:='    or via change in size or perspective.';
   WF3DHelpScr[13]:='';
   WF3DHelpScr[14]:=' <F5>-change plot type.';
   WF3DHelpScr[15]:='    Those currently installed, as a surface';
   WF3DHelpScr[16]:='    or contour plot, are:';
   WF3DHelpScr[17]:='       > probability density';
   WF3DHelpScr[18]:='       > real part';
   WF3DHelpScr[19]:='       > imaginary part';
   WF3DHelpScr[20]:='';
   WF3DHelpScr[21]:=' <F10>-activate menu.';
   FOR n:=22 TO 25 DO WF3DHelpScr[n]:='';

   WITH WF3D DO
      BEGIN
         IF Computed THEN PlotWF3D(WF3D,Potl,True)
         ELSE
            BEGIN
               Zoom:=False; Plotted:=False;
               zMax:=0.5*Potl.rStop; rhoMax:=0.5*Potl.rStop;
               SaveZoom:=Potl.Zoom; Potl.Zoom:=False;
               PlotPotential(Potl); Potl.Zoom:=SaveZoom;
            END;
         Compute3DWaveFn(WF3D,Potl);
         IF Computed THEN
            BEGIN
               {New(WF3Dkeys); {Mac only}
               WITH WF3Dkeys DO
                  BEGIN
                     Init(6);
                     key[1]:='F1-Help';
                     key[2]:='F2-Input';
                     IF Zoom THEN key[3]:='F3-Contract'
                     ELSE key[3]:='F3-Expand';
                     key[4]:='F4-ModView';
                     key[5]:='F5-PlotType';
                     key[6]:='F10-Menu';
                     Display
                  END;
               WishToReturn:=False;
               WHILE (NOT WishToReturn) AND (NOT MenuClicked) DO
                  BEGIN
                     CheckForEvents;
                     IF Event.ReadKey=Escape THEN WishToReturn:=True
                     ELSE IF WF3Dkeys.Pressed(Wkey) THEN
                        BEGIN
                           WF3Dkeys.Clear;
                           case Wkey of
                              1 :Help(WF3DHelpScr);
                              2 :Compute3DWaveFn(WF3D,Potl);
                              3 :BEGIN
                                   Zoom:=not Zoom; Plotted:=False;
                                   PlotWF3D(WF3D,Potl,True);
                                   IF Zoom THEN
                                      WF3Dkeys.key[3]:='F3-Contract'
                                   ELSE WF3Dkeys.key[3]:='F3-Expand'
                                 END;
                              4 :ModViewWF3D(WF3D,Potl);
                              5 :ChangePlotType(WF3D,Potl);
                              6 :BEGIN
                                    WishToReturn:=True;
                                    IF MainMenu.Chosen THEN
                                       MenuClicked:=True;
                                 END;
                              END; {case}
                           WF3Dkeys.Display;
                        END
                     ELSE IF MainMenu.Activated THEN MenuClicked:=True;
                  END;
               WF3Dkeys.Clear;
               {Dispose(WF3Dkeys); {Mac only}
               Plotted:=False
            END
      END
END; {ComputeWF3D}

END. {Sc3DWF}

