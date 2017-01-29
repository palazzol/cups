{Unit of routines for calculation and plots of wave functions.       }
{Used by CylSym.pas.                                                 }
{                                                                    }
{Author: John R. Hiller                                              }
{Version: 1.0              11/15/94                                  }
{Copyright (c) 1995  John Wiley & Sons, Inc.                         }
{                                                                    }
{How to add a guess for the wave function:                           }
{     The addition of a new form for the initial guess can be done   }
{in a way similar to the way addition of a potential is handled.     }
{There is the procedure for text screens and default parameter values}
{and additions to two procedures, SetWFValues and SetWFUnits.        }
{                                                                    }
UNIT ClSmWF;

INTERFACE

USES Graph,CUPS,CUPSmupp,CUPSfunc,CUPSproc,CUPSgui,CUPSgrph,
     ClSmMach,ClSmCmn,ClSmPotl;

TYPE
   WFtype=record
      IDcode        :string;
      u,uSave       :DMatrixExt; {current and previous wave functions}
      Time,ExpectHn :DCircBuf;
      DefParity,
      Parity,
      zParity,
      Defm,
      m,
      NzSteps,
      NrhoSteps     :integer;
      zMax,rhoMax,
      dz,drho       :real;       
      NSteps,
      NStepsPrev    :integer;
      StepSize,
      Tolerance,
      SavePreviousEH,
      SaveExpectH,
      PreviousEH,
      ExpectH       :real;
      NumParams     :integer;
      DefParams,                 {default parameters}
      Params        :ParamsType;
      ParamNames,
      ParamUnits    :ParamNameType;
      ParamList     :array [1..25] OF string[25];
      Undone,                    {False if last set of steps not undone.}
      Plotted       :boolean;
      NumLines      :integer;    {number of lines in InfoScr}
      InfoScr,                   {description of initial wave function}
      PlotInfoScr   :InfoScrType;{parameter values}
      END; {WFtype}

VAR
   WF            :WFtype;

{--------Routines for defining initial guess for wave function---------}

PROCEDURE SetWFValues(VAR WF :WFtype; VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE SetWFUnits(IDcode :string; Eunit,Lunit,Munit :char;
                     VAR ParamUnits :ParamNameType);
PROCEDURE ValidateWF(VAR IDcode :string; VAR ValidWF :boolean);
PROCEDURE InitializeWF(VAR WF :WFtype; VAR Potl :PotlType;
                       VAR Grid :GridType);
PROCEDURE PrepWFParamList(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE PrepWFPlotScr(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE PrepWF(VAR WF :WFtype; VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE SetInitWFToDefaults(VAR WF :WFtype; VAR Potl :PotlType);
PROCEDURE SetInitWFToGaussian(VAR WF :WFtype;
                              VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE SetInitWFToCoulombic(VAR WF :WFtype;
                               VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE SetInitWFToBesselCosine(VAR WF :WFtype;
                                  VAR Potl :PotlType;
                                  VAR Grid :GridType);
PROCEDURE SetInitWFToTwoCtrCoulombic(VAR WF :WFtype;
                                     VAR Potl :PotlType;
                                     VAR Grid :GridType);
PROCEDURE SetInitWFToUserDefined(VAR WF :WFtype;
                                 VAR Potl :PotlType; VAR Grid :GridType);

{--------Routines for wave function parameters and plots-------------}

PROCEDURE PlotWF(VAR WF :WFtype; VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE ModifyInitWF(VAR WF :WFtype;
                       VAR Potl :PotlType; VAR Grid :GridType);

{--------Routines for wave function computations----------------------------}

PROCEDURE UpdateWF(VAR WF :WFtype; VAR Potl :PotlType;
                   var Grid :GridType);
PROCEDURE InterpolateWF(VAR WF :WFtype; VAR Potl :PotlType;
                        VAR Grid :GridType);
PROCEDURE IterateWF(VAR WF :WFtype; VAR Potl :PotlType;
                    VAR Grid :GridType; SingleStep :boolean);
PROCEDURE UndoWF(VAR WF :WFtype; VAR Potl :PotlType; VAR Grid :GridType);
PROCEDURE ChangeGrid(VAR WF :WFtype; VAR Potl :PotlType;
                     VAR Grid :GridType; PotlSelected :boolean);
PROCEDURE ChangePlotType(VAR WF :WFtype;
                         VAR Potl :PotlType; VAR Grid :GridType);

IMPLEMENTATION

{--------Routines for numerical calculations---------------------------}

FUNCTION OrdBessReg(m :integer; x :real) :real;
{Estimate value for the ordinary regular Bessel function J_m(x).}
CONST
   Jstart=1.0e-20; JtooLarge=1.0e20;
VAR
   nuStart,nu,Sign :integer;
   Sum,Jnp2,Jnp1,Jn,Jm :real;
BEGIN
   IF x=0.0 THEN
      IF m=0 THEN OrdBessReg:=1.0 ELSE OrdBessReg:=0.0
   ELSE
      BEGIN
         Sign:=1;
         IF m<0 THEN
            BEGIN
               m:=-m; FOR nu:=1 TO m DO Sign:=-Sign
            END;
         IF x>1000.0 THEN
            OrdBessReg:=Sign*sqrt(2/(cPi*x))*cos(x-0.5*m*cPi-0.25*cPi)
         ELSE
            BEGIN
               nuStart:=m+10;
               IF nuStart<x+10 THEN nuStart:=round(x+10);
               Jnp2:=0.0; Jnp1:=Jstart;
               Sum:=0.0;
               FOR nu:=nuStart-2 DOWNTO 0 DO
                  BEGIN
                     Sum:=Sum+2*sqr(Jnp1);
                     Jn:=2*(nu+1)*Jnp1/x-Jnp2;
                     IF Abs(Jn)>JtooLarge THEN
                        BEGIN
                           IF nu<=m THEN Jm:=0.0;
                           Jnp2:=0.0; Jnp1:=Jstart;
                           Sum:=0.0;
                        END
                     ELSE
                        BEGIN
                           IF nu=m THEN Jm:=Jn;
                           Jnp2:=Jnp1; Jnp1:=Jn;
                        END;
                  END;
               Sum:=sqrt(Sum+sqr(Jn));
               IF Sum=0.0 THEN OrdBessReg:=0.0
               ELSE OrdBessReg:=Sign*Jm/Sum;
            END;
      END;
END; {OrdBessReg}

PROCEDURE TridiagFactorize(VAR l,u,c :DVector; N :integer);
{Input main diagonal in l, upper codiagonal in u, lower codiagonal}
{in c, and size of system in N.  Entries in l, u, and c indexed by]
{row number.   Output Crout factorized form in l, u, and c.}
VAR
   i :integer;
   lTemp,uTemp :real;
BEGIN
   lTemp:=l.Value(1);
   FOR i:=1 TO N-1 DO
      BEGIN
         uTemp:=u.Value(i)/lTemp; u.Put(i,uTemp);
         lTemp:=l.Value(i+1)-c.Value(i+1)*uTemp; l.Put(i+1,lTemp)
      END
END; {TridiagFactorize}

PROCEDURE TridiagSolve(VAR l,u,c :DVector; VAR d :DVectorExt; N :integer);
{Input factorized representation of tridiagonal matrix in l,u,c,}
{the r.h.s. in d, and size of system in N.  Output solution in d.}
{Note that the r.h.s is overwritten.}
VAR
   i :integer;
   Temp :real;
BEGIN
   Temp:=d.Value(1)/l.Value(1);
   FOR i:=1 TO N-1 DO
      BEGIN
         d.Put(i,Temp);
         Temp:=(d.Value(i+1)-c.Value(i+1)*Temp)/l.Value(i+1)
      END;
   d.Put(N,Temp);
   FOR i:=N-1 DOWNTO 1 DO
      BEGIN
         Temp:=d.Value(i)-u.Value(i)*Temp;
         d.Put(i,Temp)
      END
END; {TridiagSolve}

PROCEDURE StepADI(VAR u,V :DMatrixExt;
                  m,zParity,NrhoSteps,NzSteps :integer;
                  drho,dz,zeta,StepSize,Shift :real;
                  VAR Propeller :TWaitMessage);
{Take one step in the ADI algorithm for the solution of a two-}
{dimensional parabolic PDE for the wave function.}
VAR
   i,j :integer;
   deltaT,dtr,dtr2,dtz2,rho: real;
   w :DMatrixExt;
   a,b,c,uTemp,Vtemp,rhoVec :DVector;
   d :DVectorExt;
BEGIN
   {New(a); New(b); New(c); New(uTemp); New(Vtemp); New(rhoVec);  {Mac only}
   {New(w); New(d); {Mac only}
   Propeller.Update;
   w.Init(NrhoSteps,NzSteps+1); w.Fill(0.0);
   uTemp.Init(NzSteps); Vtemp.Init(NzSteps); d.Init(NzSteps);
   a.Init(NzSteps); b.Init(NzSteps); c.Init(NzSteps);
   deltaT:=StepSize/2;
   dtr:=deltaT/drho; dtr2:=deltaT/sqr(drho); dtz2:=deltaT/sqr(dz);
   {Construct and factorize matrix for sweep of rows.}
   Propeller.Update;
   c.Put(1,0.0); a.Put(1,1+2*dtz2);
   IF zParity<0 THEN b.Put(1,0.0) ELSE b.Put(1,-2*dtz2);
   FOR i:=2 TO NzSteps-1 DO
      BEGIN
         c.Put(i,-dtz2); a.Put(i,1+2*dtz2); b.Put(i,-dtz2)
      END;
   c.Put(NzSteps,-dtz2); a.Put(NzSteps,1+2*dtz2); b.Put(NzSteps,0.0);
   Propeller.Update;
   TridiagFactorize(a,b,c,NzSteps);
   {Sweep rows}
   FOR j:=1 TO NrhoSteps-1 DO
      BEGIN
         {Construct r.h.s.}
         Propeller.Update;
         rho:=(j-0.5)*drho;
         u.ExtractRow(j,NzSteps,uTemp);
         V.ExtractRow(j,NzSteps,Vtemp);
         d.Equate(1.0-2*dtr2-deltaT*(sqr(m/rho)+zeta*Shift),uTemp);
         d.AddMultVec(NzSteps,-zeta*deltaT,Vtemp,uTemp);
         IF j>1 THEN u.ExtractRow(j-1,NzSteps,uTemp);
         d.AddVector(dtr2-0.5*dtr/rho,uTemp);
         u.ExtractRow(j+1,NzSteps,uTemp);
         d.AddVector(dtr2+0.5*dtr/rho,uTemp);
         {Solve system and insert new row in w}
         Propeller.Update;
         TridiagSolve(a,b,c,d,NzSteps);
         w.EquateRow(j,NzSteps,d)
      END;
   c.Free; b.Free; a.Free; d.Free; Vtemp.Free; uTemp.Free;
   Propeller.Update;
   rhoVec.Init(NrhoSteps);
   FOR j:=1 TO NrhoSteps DO rhoVec.Put(j,(j-0.5)*drho);
   uTemp.Init(NrhoSteps-1); Vtemp.Init(NrhoSteps-1);
   d.Init(NrhoSteps-1);
   a.Init(NrhoSteps-1); b.Init(NrhoSteps-1); c.Init(NrhoSteps-1);
   {Construct and factorize matrix for sweep of columns.}
   Propeller.Update;
   c.Put(1,0.0); a.Put(1,1+dtr2+dtr/drho); b.Put(1,-dtr2-dtr/drho);
   FOR j:=2 TO NrhoSteps-2 DO
      BEGIN
         rho:=(j-0.5)*drho;
         c.Put(j,-dtr2+0.5*dtr/rho); a.Put(j,1+2*dtr2);
         b.Put(j,-dtr2-0.5*dtr/rho)
      END;
   rho:=(NrhoSteps-1.5)*drho;
   c.Put(NrhoSteps-1,-dtr2+0.5*dtr/rho); a.Put(NrhoSteps-1,1+2*dtr2);
   b.Put(NrhoSteps-1,0.0);
   TridiagFactorize(a,b,c,NrhoSteps-1);
   {Sweep columns}
   FOR i:=0 TO NzSteps-1 DO
      BEGIN
         {Construct r.h.s.}
         Propeller.Update;
         w.ExtractColumn(i+1,NrhoSteps-1,uTemp);
         V.ExtractColumn(i+1,NrhoSteps-1,Vtemp);
         d.Equate(1.0-2*dtz2-deltaT*zeta*Shift,uTemp);
         d.DivVec2(NrhoSteps-1,-deltaT*sqr(m),rhoVec,uTemp);
         d.AddMultVec(NrhoSteps-1,-zeta*deltaT,Vtemp,uTemp);
         w.ExtractColumn(i+2,NrhoSteps-1,uTemp);
         d.AddVector(dtz2,uTemp);
         IF i>0 THEN w.ExtractColumn(i,NrhoSteps-1,uTemp)
         ELSE IF zParity<0 THEN uTemp.MultScalar(-1.0);
         d.AddVector(dtz2,uTemp);
         {Solve system and insert new column in u}
         Propeller.Update;
         TridiagSolve(a,b,c,d,NrhoSteps-1);
         u.EquateColumn(i+1,NrhoSteps-1,d)
      END;
   u.Normalize;
   c.Free; b.Free; a.Free; d.Free;
   Vtemp.Free; uTemp.Free; rhoVec.Free; w.Free;
   {Dispose(d); Dispose(w); {Mac only}
   {Dispose(rhoVec); Dispose(Vtemp);  {Mac only}
   {Dispose(uTemp); Dispose(c); Dispose(b); Dispose(a); {Mac only}
END; {StepADI}

PROCEDURE ComputeExpectH(VAR u,V :DMatrixExt; m,NrhoSteps,NzSteps :integer;
                         drho,dz,zeta :real; VAR PreviousEH,ExpectH :real);
VAR
   i,j :integer;
   psiHpsi,norm,temp,uij,rho :real;
BEGIN
   norm:=0.0;
   FOR j:=1 TO NrhoSteps-1 DO
      BEGIN
         rho:=(j-0.5)*drho;
         temp:=0.0;
         FOR i:=1 TO NzSteps-1 DO
            BEGIN
               temp:=temp+sqr(u.Value(j,i+1));
            END;
         norm:=norm+2*rho*temp+rho*sqr(u.Value(j,1));
      END;
   if norm=0.0 then norm:=1.0;
   psiHpsi:=0.0;
   FOR j:=1 TO NrhoSteps-1 DO
      BEGIN
         rho:=(j-0.5)*drho;
         temp:=0.0;
         FOR i:=1 TO NzSteps-1 DO
            BEGIN
               u.Get(j,i+1,uij);
               temp:=temp+j*sqr(u.Value(j+1,i+1)-uij)/drho
                          +rho*sqr((u.Value(j,i+2)-uij)/dz);
               temp:=temp+rho*(sqr(m/rho)+zeta*V.Value(j,i+1))*sqr(uij);
            END;
         u.Get(j,1,uij);
         psiHpsi:=psiHpsi+2*temp+j*sqr(u.Value(j+1,1)-uij)/drho
                          +rho*(2*sqr((u.Value(j,2)-uij)/dz));
         psiHpsi:=psiHpsi+rho*(sqr(m/rho)+zeta*V.Value(j,1))*sqr(uij);
      END;
   PreviousEH:=ExpectH;
   ExpectH:=psiHpsi/(norm*zeta);
END; {ComputeExpectH}

{--------Routines for defining initial guess for wave function---------}

FUNCTION AngFactor(m,zParity :integer; rho,z :real) :real;
{Compute P_lm(theta), with l=m or m+1 as needed to have correct parity.}
VAR
   theta :real;
BEGIN
   theta:=ArcTan2(z,rho);
   IF zParity>0 THEN AngFactor:=1.0{Pwr(sin(theta),abs(m))}
   ELSE {zParity<0} AngFactor:=cos(theta){*Pwr(sin(theta),abs(m))}
END; {AngFactor}

PROCEDURE SetWFValues(VAR WF :WFtype; VAR Potl :PotlType; VAR Grid :GridType);
VAR
   i,j           :integer;
   rplus,rminus,
   rho,z,uInit   :real;
BEGIN
   WITH WF DO
      BEGIN
         u.Free; u.Init(NrhoSteps,NzSteps+1); u.Fill(0.0);
         IF IDcode='Gaussian' THEN
            FOR i:=0 TO NzSteps-1 DO FOR j:=1 TO NrhoSteps-1 DO
               BEGIN
                  rho:=(j-0.5)*drho; z:=i*dz;
                  uInit:=AngFactor(WF.m,WF.zParity,rho,z)
                         *ExpSafe(-WF.Params[1]*sqr(Potl.L0*rho)
                                 -WF.Params[2]*sqr(Potl.L0*z));
                  u.Put(j,i+1,uInit);
               END
         ELSE IF IDcode='Coulombic' THEN
            FOR i:=0 TO NzSteps-1 DO FOR j:=1 TO NrhoSteps-1 DO
               BEGIN
                  rho:=(j-0.5)*drho; z:=i*dz;
                  uInit:=AngFactor(WF.m,WF.zParity,rho,z)
                       *ExpSafe(-WF.Params[1]*Potl.L0*sqrt(sqr(rho)+sqr(z)));
                  u.Put(j,i+1,uInit);
               END
         ELSE IF IDcode='BesselCosine' THEN
            FOR i:=0 TO NzSteps-1 DO FOR j:=1 TO NrhoSteps-1 DO
               BEGIN
                  rho:=(j-0.5)*drho; z:=i*dz;
                  uInit:=AngFactor(WF.m,WF.zParity,rho,z)
                          *OrdBessReg(WF.m,WF.Params[1]*Potl.L0*rho)
                          *cos(WF.Params[2]*Potl.L0*z);
                  u.Put(j,i+1,uInit);
               END
         ELSE IF IDcode='TwoCtrCoulombic' THEN
            FOR i:=0 TO NzSteps-1 DO FOR j:=1 TO NrhoSteps-1 DO
               BEGIN
                  rho:=(j-0.5)*drho; z:=i*dz;
                  rplus:=Potl.L0*sqrt(sqr(rho)+sqr(z+WF.Params[3]/Potl.L0));
                  rminus:=Potl.L0*sqrt(sqr(rho)+sqr(z-WF.Params[3]/Potl.L0));
                  uInit:=AngFactor(WF.m,WF.zParity,rho,z)
                           *(ExpSafe(-WF.Params[1]*rplus)
                               +ExpSafe(-WF.Params[2]*rminus));
                  u.Put(j,i+1,uInit);
               END
         ELSE IF WF.IDcode='UserDefined' THEN
            FOR i:=0 TO NzSteps-1 DO FOR j:=1 TO NrhoSteps-1 DO
               BEGIN
                  rho:=(j-0.5)*drho; z:=i*dz;
                  uInit:=AngFactor(WF.m,WF.zParity,rho,z);  {*  expression}
                  u.Put(j,i+1,uInit);
               END;
      END;
END; {SetWFValues}

PROCEDURE SetWFUnits(IDcode :string; Eunit,Lunit,Munit :char;
                     VAR ParamUnits :ParamNameType);
BEGIN
   IF IDcode='Gaussian' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Lunit,'m^-2');
         ParamUnits[2]:=ConCat(Lunit,'m^-2');
      END
   ELSE IF IDcode='Coulombic' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Lunit,'m^-1');
      END
   ELSE IF IDcode='BesselCosine' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Lunit,'m^-1');
         ParamUnits[2]:=ConCat(Lunit,'m^-1');
      END
   ELSE IF IDcode='TwoCtrCoulombic' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Lunit,'m^-1');
         ParamUnits[2]:=ConCat(Lunit,'m^-1');
         ParamUnits[3]:=ConCat(Lunit,'m');
      END
   ELSE IF IDcode='UserDefined' THEN
      BEGIN
         ParamUnits[1]:=ConCat(Lunit,'m^-1');
         ParamUnits[2]:=ConCat(Lunit,'m');
      END;
END; {SetWFUnits}

PROCEDURE ValidateWF(VAR IDcode :string; VAR ValidWF :boolean);
BEGIN
   IF IDcode='Gaussian' THEN ValidWF:=True
   ELSE IF IDcode='Coulombic' THEN ValidWF:=True
   ELSE IF IDcode='BesselCosine' THEN ValidWF:=True
   ELSE IF IDcode='TwoCtrCoulombic' THEN ValidWF:=True
   ELSE IF WF.IDcode='UserDefined' THEN ValidWF:=True
   ELSE ValidWF:=False;
END; {ValidateWF}

PROCEDURE InitializeWF(VAR WF :WFtype; VAR Potl :PotlType;
                       VAR Grid :GridType);
BEGIN
   WITH WF DO
      BEGIN
         NzSteps:=Grid.NzSteps; NrhoSteps:=Grid.NrhoSteps;
         zMax:=Grid.zMax; rhoMax:=Grid.rhoMax;
         dz:=Grid.dz; drho:=Grid.drho;
         Plotted:=False; Undone:=True; Potl.Saved:=False;
         uSave.Free;
         SetWFValues(WF,Potl,Grid);
         Time.Rewrite; ExpectHn.Rewrite;
         ExpectH:=0.0;
         ComputeExpectH(u,Potl.Vmat,m,NrhoSteps,NzSteps,
                        drho,dz,Potl.zeta,PreviousEH,ExpectH);
         Time.PutNext(0.0); ExpectHn.PutNext(ExpectH);
         u.Normalize;
         uSave.Init(NrhoSteps,NzSteps+1); uSave.Equate(u);
         SaveExpectH:=ExpectH; SavePreviousEH:=PreviousEH;
      END;
END; {InitializeWF}

PROCEDURE PrepWFParamList(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   n :integer;
BEGIN
   WITH WF DO
      BEGIN
         ParamList[1]:=' Wave fn parameters';
         FOR n:=1 TO NumParams DO
            ParamList[n+1]:=ConCat('  ',ParamNames[n],
                            ' ',Num2Str(Params[n],6),
                            ' ',ParamUnits[n]);
         IF Parity>0 THEN ParamList[NumParams+2]:='  Parity: even'
         ELSE ParamList[NumParams+2]:=  '  Parity: odd';
         ParamList[NumParams+3]:=ConCat('  m: ',NumStr(m,2,0));
         ParamList[NumParams+4]:=ConCat('  Mass: ',
                                        Num2Str(Potl.Mass,6),' ',
                                        Potl.Munit,'eV/c^2');
         FOR n:=NumParams+5 TO 25 DO ParamList[n]:=''
      END
END; {PrepWFParamList}

PROCEDURE PrepWFPlotScr(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   n,index :integer;
BEGIN
   WITH WF DO
      BEGIN
         FOR n:=1 TO Potl.NumParams+5 DO PlotInfoScr[n]:=Potl.ParamList[n];
         PlotInfoScr[Potl.NumParams+6]:='';
         FOR n:=1 TO NumParams+5 DO
            BEGIN
               index:=n+Potl.NumParams+6;
               PlotInfoScr[index]:=ParamList[n];
            END;
         PlotInfoScr[index+1]:=ConCat('      <H>: ',
                                      Num2Str(Potl.V0*ExpectH,6),
                                      ' ',Potl.Eunit,'eV');
         PlotInfoScr[index+2]:=ConCat(' delta<H>: ',
                             Num2Str(Potl.V0*(ExpectH-PreviousEH),6),
                                      ' ',Potl.Eunit,'eV');
         FOR n:=index+3 TO 25 DO PlotInfoScr[n]:=''
      END
END; {PrepWFPlotScr}

PROCEDURE PrepWF(VAR WF :WFtype; VAR Potl :PotlType; VAR Grid :GridType);
BEGIN
   InitializeWF(WF,Potl,Grid);
   SetWFUnits(WF.IDcode,Potl.Eunit,Potl.Lunit,Potl.Munit,WF.ParamUnits);
   PrepWFParamList(WF,Potl);
   PrepWFPlotScr(WF,Potl)
END; {PrepWF}

PROCEDURE SetInitWFToDefaults(VAR WF :WFtype; VAR Potl :PotlType);
{Set wave function parameters to default values.}
VAR
   n :integer;
BEGIN
   WITH WF DO
      BEGIN
         FOR n:=1 TO NumParams DO Params[n]:=DefParams[n];
         Parity:=DefParity; m:=Defm;
         zParity:=Parity; FOR n:=1 TO abs(m) DO zParity:=-zParity;
         SetWFUnits(IDcode,Potl.Eunit,Potl.Lunit,Potl.Munit,ParamUnits);
      END
END; {SetInitWFToDefaults}

PROCEDURE SetInitWFToGaussian(VAR WF :WFtype;
                              VAR Potl :PotlType; VAR Grid :GridType);
{Set parameters to use Gaussian as initial wave function.}
VAR
   n :integer;
BEGIN
   WITH WF DO
      BEGIN
         IDcode:='Gaussian';
         NumParams:=2;
         ParamNames[1]:='eta_1:';
         ParamNames[2]:='eta_2:';
         NumLines:=5;
         InfoScr[1]:='    Gaussian wave function    ';
         InfoScr[2]:='';
         InfoScr[3]:='u(rho,z)=P_lm                 ';
         InfoScr[4]:='         * exp(-eta_1*rho^2   ';
         InfoScr[5]:='                 -eta_2*z^2)  ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefParams[1]:=5.0; DefParams[2]:=5.0;
         DefParity:=1; Defm:=0
      END;
   SetInitWFToDefaults(WF,Potl);
   PrepWF(WF,Potl,Grid)
END; {SetInitWFToGaussian}

PROCEDURE SetInitWFToCoulombic(VAR WF :WFtype;
                               VAR Potl :PotlType; VAR Grid :GridType);
{Set parameters to use Coulombic form for initial wave function.}
VAR
   n :integer;
BEGIN
   WITH WF DO
      BEGIN
         IDcode:='Coulombic';
         NumParams:=1;
         ParamNames[1]:='eta:';
         NumLines:=5;
         InfoScr[1]:='   Coulombic wave function    ';
         InfoScr[2]:='';
         InfoScr[3]:='u(rho,z)=P_lm * exp(-eta*r)   ';
         InfoScr[4]:='                              ';
         InfoScr[5]:='  r=sqrt(rho^2+z^2)           ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefParams[1]:=50.0;
         DefParity:=1; Defm:=0
      END;
   SetInitWFToDefaults(WF,Potl);
   PrepWF(WF,Potl,Grid)
END; {SetInitWFToCoulombic}

PROCEDURE SetInitWFToBesselCosine(VAR WF :WFtype;
                                  VAR Potl :PotlType;
                                  VAR Grid :GridType);
{Set parameters to use Bessel-cosine form for initial wave function.}
VAR
   n :integer;
BEGIN
   WITH WF DO
      BEGIN
         IDcode:='BesselCosine';
         NumParams:=2;
         ParamNames[1]:='eta_1:';
         ParamNames[2]:='eta_2:';
         NumLines:=4;
         InfoScr[1]:=' Bessel-cosine wave function  ';
         InfoScr[2]:='';
         InfoScr[3]:='u(rho,z)=P_lm * J_m(eta_1*rho)';
         InfoScr[4]:='                * cos(eta_2*z)';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefParams[1]:=2.0; DefParams[2]:=1.0;
         DefParity:=1; Defm:=0
      END;
   SetInitWFToDefaults(WF,Potl);
   PrepWF(WF,Potl,Grid)
END; {SetInitWFToBesselCosine}

PROCEDURE SetInitWFToTwoCtrCoulombic(VAR WF :WFtype;
                                     VAR Potl :PotlType;
                                     VAR Grid :GridType);
{Set parameters to use two-center Coulombic form for initial wave}
{function.}
VAR
   n :integer;
BEGIN
   WITH WF DO
      BEGIN
         IDcode:='TwoCtrCoulombic';
         NumParams:=3;
         ParamNames[1]:='eta_1:';
         ParamNames[2]:='eta_2:';
         ParamNames[3]:='    a:';
         NumLines:=7;
         InfoScr[1]:=' Two-center Coulombic wave fn ';
         InfoScr[2]:='';
         InfoScr[3]:='u(rho,z)=                     ';
         InfoScr[4]:='    P_lm*(exp(-eta_1*rplus)   ';
         InfoScr[5]:='          +exp(-eta_2*rminus))';
         InfoScr[6]:='  rplus=sqrt(rho^2+(z+a)^2)   ';
         InfoScr[7]:='  rminus=sqrt(rho^2+(z-a)^2)  ';
         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';
         DefParams[1]:=10.0; DefParams[2]:=10.0; DefParams[3]:=0.1;
         DefParity:=1; Defm:=0
      END;
   SetInitWFToDefaults(WF,Potl);
   PrepWF(WF,Potl,Grid)
END; {SetInitWFToTwoCtrCoulombic}

PROCEDURE SetInitWFToUserDefined(VAR WF :WFtype;
                                 VAR Potl :PotlType; VAR Grid :GridType);
{Set parameters to use a user-defined initial wave function.}
VAR
   n :integer;
BEGIN
   Announce('Modify code to insert your initial guess.');
{   with WF do                                                 }
{      BEGIN                                                   }
{         IDcode:='UserDefined';                               }
{         uInit:=UserDefinedWF;                                }
{         SetUnits:=SetUserDefinedWFParamUnits;                }
{         NumParams:=2;                                        }
{         ParamNames[1]:='eta_1:';                             }
{         ParamNames[2]:='eta_2:';                             }
{         NumLines:=3;                                         }
{         InfoScr[1]:='  user-defined wave function  ';        }
{         InfoScr[2]:='';                                      }
{         InfoScr[3]:='u(rho,z)=P_lm * expression    ';        }
{         FOR n:=NumLines+1 TO 25 DO InfoScr[n]:='';           }
{         DefParams[1]:=3.0; DefParams[2]:=3.0;                }
{         DefParity:=1; Defm:=0                                }
{      END;                                                    }
{   SetInitWFToDefaults(WF,Potl);                              }
{   PrepWF(WF,Potl,Grid)                                       }
END; {SetInitWFToUserDefined}

{--------Routines for wave function parameters and plots-------------}

PROCEDURE PrepPlotHvsT(VAR WF :WFtype; VAR Potl :PotlType);
VAR
   Tmin,Tmax,Emin,Emax,TicX,TicY :real;
   i,iMin,iMax,vp :integer;
BEGIN
   WITH WF DO
      BEGIN
         Plotted:=False;
         Time.MinMax(iMin,Tmin,iMax,Tmax);
         Tmax:=Tmax+StepSize*NSteps;
         ExpectHn.MinMax(iMin,Emin,iMax,Emax);
         IF (Emin=0.0) AND (Emax=0.0) THEN Emax:=10.0
         ELSE Emin:=Emin-10*abs(ExpectH-PreviousEH);
         IF Emax-Emin<0.01 THEN
            BEGIN
               Emax:=Emax+0.005;
               Emin:=Emin-0.005
            END;
         Emin:=Potl.V0*Emin; Emax:=Potl.V0*Emax;
         PrepAxisInput(Tmin,Tmax,TicX,5,2);
         PrepAxisInput(Emin,Emax,TicY,10,2);
         DefineScale(2,Tmin,Tmax,Emin,Emax);
         IF Grid.Zoom THEN vp:=3
         ELSE
            BEGIN
               vp:=2; ClearOversizedViewPort(3);
               Info(Potl.InfoScr,Potl.NumLines,4);
               Info(PlotInfoScr,25,5);
            END;
         PrepViewPort(vp);
         SelectScale(2);
         SetColor(Whitte);
         Axis(Tmin,Emin,TicX,TicY);
         PutLabel(Left,ConCat('<H>_n (',Potl.Eunit,'eV)'));
         PutLabel(Bottom,ConCat('Imaginary time (hbar/(',Potl.Eunit,'eV))'));
         Time.Reset; ExpectHn.Reset;
         SetColor(Yellow);
         WHILE Time.Left DO
            PlotSymbol(Time.GetNext,Potl.V0*ExpectHn.GetNext,'o');
         SetColor(Whitte);
      END
END; {PrepPlotHvsT}

PROCEDURE PlotWF(VAR WF :WFtype; VAR Potl :PotlType; VAR Grid :GridType);
BEGIN
   CASE Grid.PlotType OF
      1 :PlotSurface(WF.u,Grid,WF.PlotInfoScr,Potl.InfoScr,
                     Potl.NumLines,WF.zParity,True,False,
                     WF.Plotted,'Wave function',LightMagenta);
      2 :PlotContour(WF.u,Grid,WF.PlotInfoScr,Potl.InfoScr,
                     Potl.NumLines,WF.zParity,True,
                     WF.Plotted,'Wave function');
      3 :BEGIN
            IF NOT WF.Plotted THEN PrepPlotHvsT(WF,Potl)
            ELSE
               BEGIN
                  IF Grid.Zoom THEN SelectViewPort(3)
                  ELSE
                     BEGIN
                        Info(WF.PlotInfoScr,25,5);
                        SelectViewPort(2);
                     END;
                  SelectScale(2);
                  SetColor(Yellow);
                  PlotSymbol(WF.Time.GetNext,Potl.V0*WF.ExpectHn.GetNext,'o');
                  SetColor(Whitte);
               END;
         END;
      END; {case}
   Potl.Plotted:=False;
END; {PlotWF}

PROCEDURE ModifyInitWF(VAR WF :WFtype;
                       VAR Potl :PotlType; VAR Grid :GridType);
VAR
   n        :integer;
   InputScr :TInputScreen;
BEGIN
   {New(InputScr); {Mac only}
   WITH WF,InputScr DO
      BEGIN
         IF NOT Grid.Zoom THEN Info(WF.InfoScr,WF.NumLines,4);
         SetWFUnits(IDcode,Potl.Eunit,Potl.Lunit,Potl.Munit,ParamUnits);
         Init;
         DefineInputPort(0.64,0.94,0.01,0.9);
         FOR n:=1 TO Potl.NumParams+5 DO LoadLine(Potl.ParamList[n]);
         LoadLine('');
         LoadLine(          ' Wave fn parameters');
         FOR n:=1 TO NumParams DO
            LoadLine(ConCat('  ',ParamNames[n],' {      }',ParamUnits[n]));
         LoadLine(          '  Odd parity? #F');
         LoadLine(          '  m: {   }');
         LoadLine(ConCat(   '  Mass: ',Num2Str(Potl.Mass,6),
                                            ' ',Potl.Munit,'eV/c^2'));
         LoadLine('');
         LoadLine(          '  Restore defaults? #F');
         LoadLine('');
         LoadLine('  [  OK  ]    [Cancel]');
         FOR n:=1 TO NumParams DO SetNumber(n,Params[n]);
         IF Parity>0 THEN SetBoolean(NumParams+1,False)
         ELSE SetBoolean(NumParams+1,True);
         SetNumber(NumParams+2,m);
         AcceptScreen;
         IF NOT Canceled THEN WITH WF DO
            BEGIN
               IF GetBoolean(NumParams+3) THEN SetInitWFToDefaults(WF,Potl)
               ELSE
                  BEGIN
                     FOR n:=1 TO NumParams DO Params[n]:=GetNumber(n);
                     IF GetBoolean(NumParams+1) THEN Parity:=-1
                     ELSE Parity:=1;
                     m:=round(GetNumber(NumParams+2));
                     IF m>MaxRange THEN IntInputWarn('m',m,0,2);
                     zParity:=Parity; FOR n:=1 TO abs(m) DO zParity:=-zParity;
                  END;
               InitializeWF(WF,Potl,Grid);
               PrepWFParamList(WF,Potl);
               PrepWFPlotScr(WF,Potl);
               IF Grid.PlotType=3 THEN Grid.PlotType:=1;
               PlotWF(WF,Potl,Grid);
            END;
         Done;
      END;
   {Dispose(InputScr); {Mac only}
END; {ModifyInitWF}

{--------Routines for wave function computations----------------------------}

PROCEDURE UpdateWF(VAR WF :WFtype; VAR Potl :PotlType;
                   var Grid :GridType);
BEGIN
   WITH WF DO
      BEGIN
         ComputeExpectH(u,Potl.Vmat,m,NrhoSteps,NzSteps,
                        drho,dz,Potl.zeta,PreviousEH,ExpectH);
         Time.PutNext(0.0); ExpectHn.PutNext(ExpectH);
         SetWFUnits(IDcode,Potl.Eunit,Potl.Lunit,Potl.Munit,ParamUnits);
         PrepWFPlotScr(WF,Potl);
      END;
END; {UpdateWF}

PROCEDURE InterpolateWF(VAR WF :WFtype; VAR Potl :PotlType;
                        VAR Grid :GridType);
VAR
   i,j,ii,jj :integer;
   rho,z,rr,zz :real;
BEGIN
   IF (WF.rhoMax<Grid.drho) OR (WF.zMax<Grid.dz) THEN
      InitializeWF(WF,Potl,Grid)
   ELSE WITH WF DO
      BEGIN
         uSave.Free; uSave.Init(Grid.NrhoSteps,Grid.NzSteps+1);
         uSave.Fill(0.0);
         FOR i:=1 TO Grid.NzSteps DO FOR j:=1 TO Grid.NrhoSteps DO
            BEGIN
               rho:=(j-0.5)*Grid.drho; z:=i*Grid.dz;
               IF (rho<rhoMax) AND (z<zMax) THEN
                  BEGIN
                     rr:=rho/drho; zz:=z/dz;
                     jj:=trunc(rr+0.5); ii:=trunc(zz);
                     IF jj>0 THEN
                        uSave.Put(j,i+1,
                               (ii+1-zz)*(jj+0.5-rr)*u.Value(jj,ii+1)
                              +(zz-ii)  *(jj+0.5-rr)*u.Value(jj,ii+2)
                              +(ii+1-zz)*(rr+0.5-jj)*u.Value(jj+1,ii+1)
                              +(zz-ii)  *(rr+0.5-jj)*u.Value(jj+1,ii+2))
                     ELSE {jj=0}
                        uSave.Put(j,i+1,(ii+1-zz)*u.Value(1,ii+1)
                                       +(zz-ii)  *u.Value(1,ii+2));
                  END
            END;
         FOR j:=1 TO Grid.NrhoSteps DO  {handle z=0}
            BEGIN
               rho:=(j-0.5)*Grid.drho;
               IF rho<rhoMax THEN
                  BEGIN
                     rr:=rho/drho; jj:=trunc(rr+0.5);
                     IF jj>0 THEN
                        uSave.Put(j,1,(jj+0.5-rr)*u.Value(jj,1)
                                     +(rr+0.5-jj)*u.Value(jj+1,1))
                     ELSE {jj=0}
                        uSave.Put(j,1,u.Value(1,1))
                  END
            END;
         NrhoSteps:=Grid.NrhoSteps; NzSteps:=Grid.NzSteps;
         rhoMax:=Grid.rhoMax; zMax:=Grid.zMax;
         drho:=Grid.drho; dz:=Grid.dz;
         Plotted:=False; Undone:=True; Potl.Saved:=False;
         u.Free; u.Init(NrhoSteps,NzSteps+1);
         u.Equate(uSave); u.Normalize; uSave.Equate(u);
         UpdateWF(WF,Potl,Grid);
      END
END; {InterpolateWF}

PROCEDURE IterateWF(VAR WF :WFtype; VAR Potl :PotlType;
                    VAR Grid :GridType; SingleStep :boolean);
VAR
   HelpScr      :HelpScrType;
   WFkeys       :THotKeys;
   Wkey         :byte;
   NumIter,n    :integer;
   WishToReturn :boolean;
   Propeller    :TWaitMessage;
BEGIN
   HelpScr[01]:='';
   HelpScr[02]:=' <F1>-show this help screen.   ';
   HelpScr[03]:='';
   HelpScr[04]:=' <F2>-stop calculation, but retain last results.';
   FOR n:=5 TO 25 DO HelpScr[n]:='';

   WITH WF DO
      BEGIN
         uSave.Equate(u); SaveExpectH:=ExpectH; SavePreviousEH:=PreviousEH;
         {New(WFkeys); {Mac only}
         WITH WFkeys DO
            BEGIN
               Init(2);
               key[1]:='F1-Help';
               key[2]:='F2-Stop';
               Display
            END;
         WishToReturn:=False; NumIter:=0;
         {New(Propeller);  {Mac only}
         Propeller.Show;
         WHILE NOT WishToReturn DO
            BEGIN
               StepADI(u,Potl.Vmat,m,zParity,NrhoSteps,NzSteps,
                       drho,dz,Potl.zeta,StepSize,Potl.Shift,Propeller);
               Plotted:=False; Undone:=False; Potl.Saved:=False;
               ComputeExpectH(u,Potl.Vmat,m,NrhoSteps,NzSteps,
                              drho,dz,Potl.zeta,PreviousEH,ExpectH);
               Time.AddNext(StepSize); ExpectHn.PutNext(ExpectH);
               NumIter:=NumIter+1;
               IF SingleStep OR (NumIter>=NSteps) THEN WishToReturn:=True;
               IF (abs(PreviousEH-ExpectH)<Tolerance*abs(ExpectH))
                  AND (NumIter>1) THEN WishToReturn:=True;
               IF Grid.PlotType=3 THEN
                  BEGIN
                     IF Grid.Zoom THEN SelectViewPort(3)
                     ELSE SelectViewPort(2);
                     SelectScale(2);
                     SetColor(Yellow);
                     PlotSymbol(Time.GetNext,
                                Potl.V0*ExpectHn.GetNext,'o');
                     SetColor(Whitte);
                  END;
               CheckForEvents;
               IF Event.ReadKey=Escape THEN WishToReturn:=True
               ELSE IF WFkeys.Pressed(Wkey) THEN
                  CASE Wkey OF
                     1 :Help(HelpScr);
                     2 :WishToReturn:=True;
                     END {case}
            END;
         Propeller.Hide;
         {Dispose(Propeller);  {Mac only}
         NStepsPrev:=NumIter;
         PrepWFPlotScr(WF,Potl);
         PlotWF(WF,Potl,Grid);
         WFkeys.Clear;
         {Dispose(WFkeys); {Mac only}
      END
END; {IterateWF}

PROCEDURE UndoWF(VAR WF :WFtype; VAR Potl :PotlType; VAR Grid :GridType);
{Undo previous set of time steps.}
BEGIN
   IF NOT WF.Undone THEN WITH WF DO
      BEGIN
         u.Equate(uSave); Plotted:=False; Undone:=True;
         Time.Strip(NStepsPrev); ExpectHn.Strip(NStepsPrev);
         ExpectH:=SaveExpectH; PreviousEH:=SavePreviousEH;
         IF NStepsPrev=MaxNumE THEN
            BEGIN
               Time.PutNext(0.0); ExpectHn.PutNext(ExpectH);
            END;
         PrepWFPlotScr(WF,Potl);
         PlotWF(WF,Potl,Grid);
      END;
END; {UndoWF}

PROCEDURE ChangeGrid(VAR WF :WFtype; VAR Potl :PotlType;
                     VAR Grid :GridType; PotlSelected :boolean);
{Change grid parameters.}
VAR
   NzStepsOld,NrhoStepsOld :integer;
   MemBlock  :longint;
   Failed    :boolean;
   InputScr  :TInputScreen;
BEGIN
   {New(InputScr); {Mac only}
   WITH Grid,InputScr DO
      BEGIN
         Init;
         IF PotlSelected THEN DefineInputPort(0.28,0.72,0.25,0.8)
         ELSE DefineInputPort(0.28,0.72,0.2,0.8);
         LoadLine('');
         LoadLine('     Grid parameters');
         LoadLine('');
         IF PotlSelected THEN
            BEGIN
               LoadLine(ConCat('        z_max: {      }',Potl.Lunit,'m'));
               LoadLine(' No. of steps: {    }');
               LoadLine(ConCat('      rho_max: {      }',Potl.Lunit,'m'));
               LoadLine(' No. of steps: {    }');
            END
         ELSE
            BEGIN
               LoadLine(ConCat('        z_max: {      }',Potl.Lunit,'m *'));
               LoadLine(' No. of steps: {    }');
               LoadLine(ConCat('      rho_max: {      }',Potl.Lunit,'m *'));
               LoadLine(' No. of steps: {    }');
               LoadLine('');
               LoadLine(' * set after potential selected.');
            END;
         LoadLine('');
         LoadLine(' Use full screen for plot? #F');
         LoadLine('');
         LoadLine('  [  OK  ]    [Cancel]');
         SetNumber(1,Potl.L0*zMax);
         SetNumber(2,NzSteps);
         SetNumber(3,Potl.L0*rhoMax);
         SetNumber(4,NrhoSteps);
         SetBoolean(5,Zoom);
         AcceptScreen;
         IF NOT Canceled THEN
            BEGIN
               zMax:=GetNumber(1);
               IF zMax<=0.0 THEN
                  RealInputWarn('z_max',zMax,Potl.DefzMax,6);
               zMax:=zMax/Potl.L0;
               rhoMax:=GetNumber(3);
               IF rhoMax<=0.0 THEN
                  RealInputWarn('rho_max',rhoMax,Potl.DefrhoMax,6);
               rhoMax:=rhoMax/Potl.L0;
               NzStepsOld:=NzSteps; NzSteps:=round(GetNumber(2));
               IF NzSteps<1 THEN
                  IntInputWarn('no. of z steps',NzSteps,NzStepsOld,3);
               NrhoStepsOld:=NrhoSteps; NrhoSteps:=round(GetNumber(4));
               IF NrhoSteps<2 THEN 
                  IntInputWarn('no. of rho steps',NrhoSteps,NrhoStepsOld,3);
               WF.uSave.Free; Potl.Vmat.Free;
               MemBlock:=Longint(NrhoSteps*(NzSteps+1))*SizeOf(real);
               IF MemBlock>=65521 THEN Failed:=True
               ELSE IF MemBlock>MaxAvail THEN Failed:=True
               ELSE
                  BEGIN
                     Potl.Vmat.Init(NrhoSteps,NzSteps+1);
                     IF MemBlock>MaxAvail THEN
                        BEGIN
                           Failed:=True;
                           Potl.Vmat.Free;
                        END
                     ELSE 
                        BEGIN
                           WF.uSave.Init(NrhoSteps,NzSteps+1);
                           IF MemBlock>MaxAvail THEN
                              BEGIN
                                 Failed:=True;
                                 WF.uSave.Free; Potl.Vmat.Free;
                              END
                           ELSE Failed:=False;
                        END;
                  END;
               IF Failed THEN
                  BEGIN
                     Announce(ConCat('Memory capacity insufficient for ',
                              'requested array size.'));
                     NzSteps:=NzStepsOld; NrhoSteps:=NrhoStepsOld;
                     Potl.Vmat.Init(NrhoSteps,NzSteps+1);
                     WF.uSave.Init(NrhoSteps,NzSteps+1);
                  END;
               dz:=zMax/NzSteps;
               drho:=rhoMax/(NrhoSteps-0.5);
               Zoom:=GetBoolean(5);
               PrepPotl(Potl,Grid);
               InterpolateWF(WF,Potl,Grid);
            END;
         Done;
      END;
   {Dispose(InputScr); {Mac only}
END; {ChangeGrid}

PROCEDURE ChangePlotType(VAR WF :WFtype;
                         VAR Potl :PotlType; VAR Grid :GridType);
VAR
   InputScr :TInputScreen;
BEGIN
   {New(InputScr); {Mac only}
   WITH InputScr DO
      BEGIN
         Init;
         DefineInputPort(0.2,0.75,0.3,0.7);
         LoadLine('');
         LoadLine('       Plot type');
         LoadLine('');
         LoadLine('    #1 Surface plot      #2 With text');
         LoadLine('    #1 Contour plot      #2 Full screen');
         LoadLine('    #1 <H> vs time');
         LoadLine('');
         LoadLine('  [  OK  ]    [Cancel]');
         SetRadioButton('1',Grid.PlotType);
         IF Grid.Zoom THEN SetRadioButton('2',2)
         ELSE SetRadioButton('2',1);
         AcceptScreen;
         IF NOT Canceled THEN
            BEGIN
               Grid.PlotType:=GetRadioButton('1');
               IF GetRadioButton('2')=2 THEN Grid.Zoom:=True
               ELSE Grid.Zoom:=False;
               WF.Plotted:=False;
               PlotWF(WF,Potl,Grid);
            END;
         Done;
      END;
   {Dispose(InputScr); {Mac only}
END; {ChangePlotType}

END. {ClSmWF}

